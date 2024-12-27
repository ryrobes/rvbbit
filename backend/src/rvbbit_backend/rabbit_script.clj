(ns rvbbit-backend.rabbit-script
  (:require [clojure.string :as str]
            [clojure.walk :as walk]
            [clojure.set :as cset]
            [rvbbit-backend.util :as ut]
            [clj-time.core :as t]
            [clj-time.format :as f]
            [clj-time.coerce :as c])
  (:import [java.time LocalDate]
           [java.time.format DateTimeFormatter]
           [java.time.temporal ChronoUnit IsoFields]))

;; custom excel formula parser and transformer for vectors of maps (rabbit rowsets)
;; dec 2024 - Ry

(defn median [nums]
  (let [sorted (sort nums)
        cnt (count nums)
        halfway (quot cnt 2)]
    (if (odd? cnt)
      (nth sorted halfway)
      (let [bottom (dec halfway)
            bottom-val (nth sorted bottom)
            top-val (nth sorted halfway)]
        (/ (+ bottom-val top-val) 2.0)))))

(defn mode [nums]
  (->> nums
       frequencies
       (sort-by val >)
       first
       key))

(def date-formatter (DateTimeFormatter/ofPattern "yyyy-MM-dd"))
(def yearmonth-formatter (DateTimeFormatter/ofPattern "yyyy-MM"))

(defn to-date [val]
  (cond
    (nil? val) nil
    (instance? LocalDate val) val
    (string? val) (try
                    (LocalDate/parse val date-formatter)
                    (catch Exception _ nil))
    :else nil))

(defn vectorize-date-fn [f val & more]
  ;; Generic helper to handle scalar or vector inputs for date functions.
  ;; f should be a function that takes coerced date arguments and returns a scalar.
  (let [args (cons val more)
        vectors? (some #(coll? %) args)
        coerce-arg (fn [a]
                     (if (coll? a)
                       (mapv to-date a)
                       (let [d (to-date a)]
                         (if (some #(coll? %) args)
                           ;; If other args are vectors, unify to a vector
                           [d]
                           d))))]
    (let [coerced (map coerce-arg args)]
      (if (some #(vector? %) coerced)
        (let [max-len (apply max (map #(if (vector? %) (count %) 1) coerced))
              expanded (map (fn [c]
                              (if (vector? c)
                                c
                                ;; scalar to vector
                                (vec (repeat max-len c)))) coerced)]
          (apply mapv (fn [& vs] (apply f vs)) expanded))
        (apply f coerced)))))

(defn yearmonth-fn [date]
  (if date
    (.format date yearmonth-formatter)
    nil))

(defn months-between-fn [d1 d2]
  (if (and d1 d2)
    (int (.between ChronoUnit/MONTHS d1 d2))
    nil))

(defn days-between-fn [d1 d2]
  (if (and d1 d2)
    (int (.between ChronoUnit/DAYS d1 d2))
    nil))

(defn days-since-fn [d]
  (if d
    (int (.between ChronoUnit/DAYS d (LocalDate/now)))
    nil))

;; Date formatting helpers
(def date-format-patterns
  {"mm/dd/yyyy" "MM/dd/yyyy"
   "m/d/yyyy" "M/d/yyyy"
   "mmm d, yyyy" "MMM d, yyyy"
   "mmmm d, yyyy" "MMMM d, yyyy"
   "d-mmm" "d-MMM"
   "d-mmmm" "d-MMMM"
   "ddd" "EEE"
   "dddd" "EEEE"
   "mm/dd/yy" "MM/dd/yy"
   "yyyy-mm-dd" "yyyy-MM-dd"
   "hh:mm:ss" "HH:mm:ss"
   "h:mm am/pm" "h:mm a"})

(defn parse-date [value]
  (try
    (cond
      (instance? org.joda.time.DateTime value) value
      (string? value) (f/parse (f/formatter "yyyy-MM-dd HH:mm:ss") value)
      :else nil)
    (catch Exception _
      (try
        (f/parse (f/formatter "yyyy-MM-dd") value)
        (catch Exception _
          nil)))))

(defn format-date [value format-str]
  (let [date (parse-date value)
        pattern (get date-format-patterns format-str format-str)]
    (when date
      (try
        (f/unparse (f/formatter pattern) date)
        (catch Exception _
          "Invalid Format")))))

;; Number formatting helpers
(defn format-number [value pattern]
  (try
    (let [num (if (string? value)
                (Double/parseDouble value)
                (double value))
          ;; Simple pattern handling
          formatted (cond
                      (= pattern "0%") (format "%.0f%%" (* num 100))
                      (= pattern "#,##0.00") (format "%,.2f" num)
                      (= pattern "$#,##0.00") (format "$%,.2f" num)
                      (re-matches #"0{1,}" pattern) (format (str "%0" (count pattern) "d") (int num))
                      :else (format "%.2f" num))]
      formatted)
    (catch Exception _
      "Invalid Number")))

(defn text-format [value format-pattern]
  (cond
    ;; Try date formatting first
    (get date-format-patterns format-pattern)
    (format-date value format-pattern)

    ;; Then try number formatting
    :else
    (format-number value format-pattern)))

(defrecord ColumnRef [name])
(defrecord FieldRef [column row])
(defrecord RangeRef [columns start-row end-row])

(def token-regex
  #"(?:>=|<=|<>|\+|\*|\/|=|<|>|\(|\)|\[|\]|,|\"[^\"]*\"|(?i)true|(?i)false|-?\d+(?:\.\d+)?|[A-Za-z0-9_]+(?:\.[0-9]+)?(?:\[\d+:\d+\])?|\-)")



(defn tokenize-nested-formula [formula]
  (let [f (if (and (not (empty? formula)) (= (first formula) \=))
            (subs formula 1)
            formula)
        tokens (vec (re-seq token-regex f))]
    tokens))


(defn parse-reference [token]
  (cond
    (and (string? token) (str/includes? token "."))
    (let [[col row] (str/split token #"\.")]
      (->FieldRef (keyword col) (parse-long row)))

    ;; Handle boolean literals (case-insensitive)
    (and (string? token) (re-matches #"(?i)^true$" token)) true
    (and (string? token) (re-matches #"(?i)^false$" token)) false

    ;; Handle string literals (enclosed in quotes)
    (and (string? token)
         (str/starts-with? token "\"")
         (str/ends-with? token "\""))
    (subs token 1 (dec (count token)))

    ;; Handle numbers
    (and (string? token) (re-matches #"-?\d+\.?\d*" token))
    (Double/parseDouble token)

    ;; Range reference: column[start:end]
    (and (string? token)
         (str/includes? token "[")
         (str/includes? token "]"))
    (let [[col range] (str/split token #"\[")
          [start end] (-> range
                          (str/replace "]" "")
                          (str/split #":"))
          parsed (->RangeRef [(keyword (str/trim col))]
                             (parse-long start)
                             (parse-long end))]
      parsed)

    ;; Simple column reference
    (string? token)
    (->ColumnRef (keyword token))

    :else
    token))


(def recognized-functions
  #{"SUM" "AVERAGE" "MEDIAN" "MODE" "COUNT" "COUNTD"
    "OFFSET" "FILTER" "CURRENT"
    "MIN" "MAX" "LARGE" "SMALL" "AND" "OR" "NOT" "UPPER" "LOWER"
    "SUMIF" "COUNTIF" "AVERAGEIF"
    "SUMIFS" "COUNTIFS" "AVERAGEIFS" "TEXT"
    "CURRENT_DATE" "YEARMONTH" "MONTHS_BETWEEN" "DAYS_BETWEEN" "DAYS_SINCE"
    "TODAY" "NOW" "SPLIT" "REPLACE" "SUBSTR"
    "ROUND" "FLOOR" "CEIL"
    "LEFT" "RIGHT" "MID" "TRIM" "PROPER"
    "WEEKDAY" "SQRT"
    "POWER" "LOG" "LOG10" "TRUE" "FALSE"
    "SIN" "COS" "TAN" "CONCAT"
    "ASIN" "ACOS" "ATAN"
    "ABS" "EXP" "VLOOKUP" "CASE"
    "ISNUMBER" "ISTEXT" "ISNULL" "TO_NUMBER" "TO_TEXT"
    "FIND" "ADD_MONTHS" "ADD_DAYS" "VARIANCE" "STDDEV"
    "QUARTER" "YEAR" "MONTH" "DAY" "WEEK" "LAG" "LEAD" "RUNNING_SUM"
    "RANK" "SORT" "DISTINCT" "ARRAY_LENGTH" "COALESCE" "SWITCH"
;;;"REDUCE"
    "UNIQUE"})

(defn make-parser [tokens]
  (let [pos (atom 0)]
    (letfn [(current-token [] (get tokens @pos))
            (consume-token []
              (let [t (current-token)]
                (swap! pos inc)
                t))
            (peek-token []
              (current-token))
            (match-token [expected]
              (when (= (current-token) expected)
                (consume-token) true))
            (expect-token [expected]
              (let [t (consume-token)]
                (when-not (= t expected)
                  (throw (ex-info "Unexpected token" {:expected expected :got t})))))

            (parse-expression [] (parse-comparison))

            (parse-comparison []
              (let [left (parse-sum)]
                (loop [l left]
                  (let [op (peek-token)]
                    (if (some #{op} [">" "<" ">=" "<=" "=" "<>"])
                      (do
                        (consume-token)
                        (let [right (parse-sum)]
                          (recur [({">"  :gt "<"  :lt ">=" :gte "<=" :lte "=" :eq "<>" :neq} op) l right])))
                      l)))))

            (parse-sum []
              (let [left (parse-term)]
                (loop [l left]
                  (let [op (peek-token)]
                    (if (some #{op} ["+" "-"])
                      (do
                        (consume-token)
                        (let [right (parse-term)]
                          (recur [(if (= op "+") :add :subtract) l right])))
                      l)))))

            (parse-term []
              (let [left (parse-factor)]
                (loop [l left]
                  (let [op (peek-token)]
                    (if (some #{op} ["*" "/"])
                      (do
                        (consume-token)
                        (let [right (parse-factor)]
                          (recur [(if (= op "*") :multiply :divide) l right])))
                      l)))))

            ;; New function to parse arrays like [expr, expr, ...]
            (parse-array []
              (expect-token "[")
              (if (= (peek-token) "]")
                (do (consume-token) []) ;; empty array
                (let [arg (parse-expression)]
                  (loop [acc [arg]]
                    (if (= (peek-token) ",")
                      (do
                        (consume-token)
                        (recur (conj acc (parse-expression))))
                      (do
                        (expect-token "]")
                        acc))))))

            (parse-factor []
              (let [t (peek-token)]
                (cond
                  (nil? t) (throw (ex-info "Unexpected end of input" {}))

                  ;; handle double-quoted strings if needed
                  (and (string? t)
                       (clojure.string/starts-with? t "\"")
                       (clojure.string/ends-with? t "\""))
                  (do
                    (consume-token)
                    (subs t 1 (dec (count t))))  ;; strip quotes

                  (= t "IF")
                  (do
                    (consume-token) ; consume IF
                    (expect-token "(")
                    (let [cond-expr (parse-expression)]
                      (expect-token ",")
                      (let [then-expr (parse-expression)]
                        (expect-token ",")
                        (let [else-expr (parse-expression)]
                          (expect-token ")")
                          [:if cond-expr then-expr else-expr]))))

                        ;; IFS branch - variable number of condition/result pairs
                  ;; IFS branch
                  (= t "IFS")
                  (do
                    (consume-token)  ; consume IFS
                    (expect-token "(")
                    (let [pairs
                          (loop [acc []]
                            (let [condition (parse-expression)]
                              (if (= (peek-token) ")")
                                acc
                                (do
                                  (when (not= (peek-token) ",")
                                    (throw (ex-info "Expected comma after condition" {:got (peek-token)})))
                                  (consume-token)  ; consume comma
                                  (let [result (parse-expression)]
                                    (if (= (peek-token) ")")
                                      (conj acc condition result)  ; last pair
                                      (do
                                        (when (not= (peek-token) ",")
                                          (throw (ex-info "Expected comma after result" {:got (peek-token)})))
                                        (consume-token)  ; consume comma
                                        (recur (conj acc condition result)))))))))]
                      (consume-token)  ; consume closing paren
                      [:ifs pairs]))

      ;; SWITCH branch - expression followed by value/result pairs
                  (= t "SWITCH")
                  (do
                    (consume-token)  ; consume SWITCH
                    (expect-token "(")
                    (let [switch-expr (parse-expression)
                          _ (expect-token ",")
                          pairs-and-default
                          (loop [acc []]
                            (let [case-expr (parse-expression)]
                              (if (= (peek-token) ")")
                                ;; Single expression at end = default case
                                (conj acc case-expr)
                                (do
                                  (expect-token ",")
                                  (let [result-expr (parse-expression)]
                                    (if (= (peek-token) ")")
                                      ;; End with a pair
                                      (conj acc case-expr result-expr)
                                      (do
                                        (expect-token ",")
                                        (recur (conj acc case-expr result-expr)))))))))]
                      (expect-token ")")
                      [:switch (into [switch-expr] pairs-and-default)]))

      ;; CHOOSE branch - index followed by choices
                  (= t "CHOOSE")
                  (do
                    (consume-token)
                    (expect-token "(")
                    (let [index-expr (parse-expression)]
                      (loop [choices [index-expr]]
                        (if (= (peek-token) ",")
                          (do
                            (consume-token)
                            (recur (conj choices (parse-expression))))
                          (do
                            (expect-token ")")
                            [:choose choices])))))

      ;; LET branch - pairs of name/expression followed by final expression
                 (= t "LET")
                 (do
                   (consume-token)  ; consume LET
                   (expect-token "(")
                   (let [[bindings body]
                         (loop [acc []]
                           (let [var-name (parse-expression)]
                             (if (= (peek-token) ")")
                               [acc var-name]  ; var-name is actually the body in this case
                               (do
                                 (when (not= (peek-token) ",")
                                   (throw (ex-info "Expected comma" {:got (peek-token)})))
                                 (consume-token)  ; consume comma
                                 (let [value-expr (parse-expression)]
                                   (if (= (peek-token) ")")
                                     [acc value-expr]  ; value-expr is the body
                                     (do
                                       (when (not= (peek-token) ",")
                                         (throw (ex-info "Expected comma" {:got (peek-token)})))
                                       (consume-token)  ; consume comma
                                       (recur (conj acc var-name value-expr)))))))))]
                     (consume-token)  ; consume closing paren
                     [:let bindings body]))

                  (= t "REDUCE")
                  (do
                    (consume-token)  ; consume REDUCE
                    (expect-token "(")
                    (let [array-expr (parse-expression)
                          _ (expect-token ",")
                          init-expr (parse-expression)
                          _ (expect-token ",")
                          fn-expr (parse-expression)]
                      (expect-token ")")
                      [:reduce array-expr init-expr fn-expr]))

      ;; MAP branch - array and function expression
                  (= t "MAP")
                  (do
                    (consume-token)
                    (expect-token "(")
                    (let [array-expr (parse-expression)
                          _ (expect-token ",")
                          fn-expr (parse-expression)]
                      (expect-token ")")
                      [:map array-expr fn-expr]))

      ;; LAMBDA branch - parameter list and body
                  (= t "LAMBDA")
                  (do
                    (consume-token)
                    (expect-token "(")
                    (let [param (parse-expression)
                          _ (expect-token ",")
                          body-expr (parse-expression)
                          _ (expect-token ")")]
                      [:lambda param body-expr]))

                  ;; recognized functions
                  (recognized-functions t)
                  (do
                    (consume-token) ; consume function name
                    (expect-token "(")
                    (let [args (parse-args)]
                      (expect-token ")")
                      [:function (keyword (clojure.string/lower-case t)) args]))

                  ;; handle arrays
                  (= t "[")
                  (parse-array)

                  (= t "(")
                  (do
                    (consume-token) ; "("
                    (let [expr (parse-expression)]
                      (expect-token ")")
                      expr))

                  (re-matches #"-?\d+(\.\d+)?$" t)
                  (do (consume-token) (Double/parseDouble t))

                  (re-matches #"[A-Za-z0-9_]+.*" t)
                  (do
                    (let [ident t]
                      (consume-token)
                      (if (= (peek-token) "(")
                        (parse-function-call ident)
                        (parse-reference ident))))

                  :else
                  (throw (ex-info "Unexpected token in factor" {:token t})))))

            (parse-function-call [name]
              (expect-token "(")
              (let [args (parse-args)]
                (expect-token ")")
                [:function (keyword (str/lower-case name)) args]))

            (parse-args []
              (if (= (peek-token) ")")
                ;; No arguments
                []
                ;; At least one argument
                (let [arg
                      (case (peek-token)
                        "[" (parse-array)
                        (parse-expression))]
                  (loop [acc [arg]]
                    (if (= (peek-token) ",")
                      (do
                        (consume-token)
                        (let [next-arg (case (peek-token)
                                         "[" (parse-array)
                                         (parse-expression))]
                          (recur (conj acc next-arg))))
                      acc)))))]
      {:parse-expression parse-expression
       :done? (fn [] (= @pos (count tokens)))})))

(defn bool-filter [vals condition]
  (map second (filter (fn [[c v]] c) (map vector condition vals))))

(defn sumif-like [vals condition]
  (let [filtered (bool-filter vals condition)]
    (reduce + 0 (remove nil? filtered))))

(defn countif-like [vals condition]
  (let [filtered (bool-filter vals condition)]
    (count (remove nil? filtered))))

(defn averageif-like [vals condition]
  (let [filtered (bool-filter vals condition)]
    (if (empty? (remove nil? filtered))
      nil
      (/ (reduce + 0 (remove nil? filtered)) (count (remove nil? filtered))))))

(defn combine-conditions [conds]
  ;; conds is a list of boolean vectors
  (apply (partial mapv #(and %1 %2)) conds))

(declare eval-expr)

(defn sumifs-like [arg-exprs agg-context]
  ;; SUMIFS: first arg is sum-range, then pairs of criteria_range, criteria
  ;; e.g. SUMIFS(sum_range, crit_range1, crit1, crit_range2, crit2, ...)
  (let [[sum-range & rest] arg-exprs
        sum-vals (eval-expr sum-range agg-context)]
    (loop [rs rest
           conditions []]
      (if (empty? rs)
        (let [combined (if (empty? conditions)
                         (mapv (constantly true) sum-vals)
                         (combine-conditions conditions))
              filtered (bool-filter sum-vals combined)]
          (reduce + 0 (remove nil? filtered)))
        (let [[crit-range crit-expr & more] rs
              crit-vals (eval-expr crit-range agg-context)
              cond-result (eval-expr crit-expr agg-context)]
          ;; Ensure same length, vector of booleans
          (when-not (and (vector? cond-result) (vector? crit-vals))
            (throw (ex-info "SUMIFS conditions must be vectors" {})))
          (when (not= (count crit-vals) (count sum-vals))
            (throw (ex-info "SUMIFS range and criteria must match length" {})))

          (recur more (conj conditions (bool-filter crit-vals cond-result))))))))

(defn countifs-like [arg-exprs agg-context]
  (let [[count-range & rest] arg-exprs
        count-vals (eval-expr count-range agg-context)]
    (loop [rs rest
           conditions []]
      (if (empty? rs)
        (let [combined (if (empty? conditions)
                         (mapv (constantly true) count-vals)
                         (combine-conditions conditions))
              filtered (bool-filter count-vals combined)]
          (count (remove nil? filtered)))
        (let [[crit-range crit-expr & more] rs
              crit-vals (eval-expr crit-range agg-context)
              cond-result (eval-expr crit-expr agg-context)]
          (recur more (conj conditions (bool-filter crit-vals cond-result))))))))

(defn averageifs-like [arg-exprs agg-context]
  (let [[avg-range & rest] arg-exprs
        avg-vals (eval-expr avg-range agg-context)]
    (loop [rs rest
           conditions []]
      (if (empty? rs)
        (let [combined (if (empty? conditions)
                         (mapv (constantly true) avg-vals)
                         (combine-conditions conditions))
              filtered (bool-filter avg-vals combined)
              filtered-non-nil (remove nil? filtered)]
          (if (empty? filtered-non-nil)
            nil
            (/ (reduce + 0 filtered-non-nil) (count filtered-non-nil))))
        (let [[crit-range crit-expr & more] rs
              crit-vals (eval-expr crit-range agg-context)
              cond-result (eval-expr crit-expr agg-context)]
          (recur more (conj conditions (bool-filter crit-vals cond-result))))))))



(defn string-len [val]
  (count (str val)))

(defn string-concat [vals]
  (apply str vals))


(defrecord EvalContext [row-index
                        current-row
                        data
                        aggregations
                        window-fns
                        base-row])

(defn rationalize-result [result]
  (if (ratio? result)
    (double result)
    result))

(defn vectorize-op [op-fn left right]
  (try
    (cond
      (and (coll? left) (coll? right))
      (mapv (fn [l r]
              (if (or (nil? l) (nil? r))
                nil
                (rationalize-result (op-fn l r))))
            left right)

      (coll? left)
      (mapv #(if (nil? %)
               nil
               (rationalize-result (op-fn % right)))
            left)

      (coll? right)
      (mapv #(if (nil? %)
               nil
               (rationalize-result (op-fn left %)))
            right)

      :else
      (rationalize-result (op-fn left right)))
    (catch Exception _ nil)))

(defn vlookup-logic [lookup-value keys-array values-array approximate]
  ;; keys-array and values-array should be same length
  ;; Sort keys and values together if approximate = TRUE
  (let [pairs (map vector keys-array values-array)
        sorted-pairs (sort-by first pairs)]
    (if approximate
      ;; find largest key <= lookup-value
      (let [candidates (take-while #(<= (first %) lookup-value) sorted-pairs)]
        (if (empty? candidates) nil (second (last candidates))))
      ;; exact match
      (some (fn [[k v]] (when (= k lookup-value) v)) sorted-pairs))))

(defn resolve-resource [resource-name context]
  ;; Example: from context or a global registry:
  ;; {:TierMapping [[0 1000 2000] ["Tier 1" "Tier 2" "Tier 3"]]}
  (let [resources (get-in context [:external-resources])]
    (or (get resources resource-name)
        (throw (ex-info "Resource not found" {:name resource-name})))))

(def formula-cache (atom {}))

(defn with-cache [cache-key f]
  (if-let [cached (get @formula-cache cache-key)]
    cached
    (let [result (f)]
      (swap! formula-cache assoc cache-key result)
      result)))

(def aggregator-fns #{:sum :average :median :mode :count :countd :variance :stddev :min :max
                      :large
                      :small
                      :running_sum
                      :unique
                      :rank
                      :sort
                      :distinct
                      :array_length})

(def non-agg-functions
  (cset/difference
   (set (map (comp keyword str/lower-case) recognized-functions))
   aggregator-fns))


(defmacro time-it [label & body]
  `(let [start# (System/nanoTime)
         result# (do ~@body)
         end# (System/nanoTime)]
     (println ~label "took:" (/ (- end# start#) 1000000.0) "ms")
     result#))

(def function-cache (atom {}))
(def times (atom 0))
(def htimes (atom 0))

(defn eval-expr [expr context]
  ;;(println "expr "  expr)
  (cond
    (nil? expr) nil
    (number? expr) expr
    (string? expr) expr
    (boolean? expr) expr

    (instance? ColumnRef expr)
    (do
      (let [col-name (:name expr)
            ;;_ (println "Resolving ColumnRef:" col-name)
            ;;_ (println "Context keys:" (keys context))
            result (cond
                     (= col-name :accumulator)
                     (do ;(println "Found accumulator:" (:accumulator context))
                         (:accumulator context))

                     (= col-name :current_value)  ; Changed from :current-value
                     (do ;(println "Found current_value:" (:current_value context))
                         (:current_value context))

                     (get-in context [:in-aggregation])
                     (mapv #(get % col-name) (:data context))

                     :else
                     (get-in context [:current-row col-name]))]
        ;(println "ColumnRef result:" result)
        result))

    (instance? FieldRef expr)
    (get-in (:data context) [(:row expr) (:column expr)])

    ;; (instance? FieldRef expr)
    ;; (let [_ (println ["field-ref exp" expr " kk " (:data context)])
    ;;       row-idx (dec (:row expr))  ; Convert 1-based to 0-based indexing
    ;;       col-key (:column expr)]
    ;;   (get-in (:data context) [row-idx col-key]))  ; Access data directly by index

    (instance? RangeRef expr)
    ;; Ranges always return vectors
    (let [start-idx (:start-row expr)
          end-idx (inc (:end-row expr))
          col (first (:columns expr))]
      (mapv #(get % col) (subvec (:data context) start-idx end-idx)))

    (vector? expr)
    (let [[op & args] expr]
      (if (keyword? op) ;; a literal value in an array vec and NOT a function or a resolvable resource
        (case op
          :ifs (let [pairs (partition 2 args)]
                 (loop [[[condition result] & rest] pairs]
                   (let [cond-res (eval-expr condition context)]
                     (cond
                       (nil? condition) nil  ; No conditions left
                       cond-res (eval-expr result context)  ; Found true condition
                       :else (recur rest)))))  ; Try next pair
          :switch (let [[expr & cases] args
                        value (eval-expr expr context)
                        pairs (partition 2 2 nil cases)]  ; Allow for optional default
                    (loop [[[case result] & rest] pairs]
                      (cond
                        (nil? case) nil
                        (= value (eval-expr case context)) (eval-expr result context)
                        (and (empty? rest) result) (eval-expr result context)  ; Default case
                        :else (recur rest))))
          :choose (let [[index & choices] args
                        idx (eval-expr index context)]
                    (when (and (number? idx)
                               (<= 1 idx (count choices)))
                      (eval-expr (nth choices (dec idx)) context)))
          ;; In the eval-expr function, update the :map case:
          :map
          (let [[array-expr fn-expr] args
                array-val (eval-expr array-expr context)
                array-vec (if (vector? array-val) array-val [array-val])]
            (mapv (fn [idx]
                    (let [current-val (nth array-vec idx)
                          base-row (nth (:data context) idx)
                          new-context (assoc context
                                             :in-aggregation false
                                             :row-index idx
                                             :current-row (assoc base-row
                                                                 (:name array-expr)
                                                                 current-val))
                          result (eval-expr fn-expr new-context)]
                      ;; Handle different result types
                      (cond
                        ;; If it's a function (lambda), apply it
                        (fn? result)
                        (let [fn-result (result current-val)]
                          (if (vector? fn-result)
                            (first fn-result)
                            fn-result))

                        ;; If it's a vector, unwrap it
                        (vector? result)
                        (first result)

                        ;; Otherwise return as is
                        :else
                        result)))
                  (range (count array-vec))))
          :reduce
          (let [[array-expr init-expr fn-expr] args
                array-val (let [val (eval-expr array-expr (assoc context :in-aggregation true))]
                            (if (vector? val)
                              val
                              [val]))
                init-val (eval-expr init-expr context)
               ;; Get the original data rows to access per-row values
                data-rows (:data context)
                result (reduce (fn [acc [current row-data]]
                                 (let [new-context (assoc context
                                                          :accumulator acc
                                                          :current_value current
                                                          :current-row row-data)
                                       result (eval-expr fn-expr new-context)]
                                   result))
                               init-val
                             ;; Zip the values with their corresponding row data
                               (map vector array-val data-rows))]
            result)
          :lambda (let [[params & body] args]
                    (fn [& args]
                      (eval-expr (last body)
                                 (assoc context :params (zipmap params args)))))
          :let (let [bindings (partition 2 (butlast args))
                     body (last args)
                     new-context (reduce (fn [ctx [name expr]]
                                           (assoc-in ctx [:bindings name]
                                                     (eval-expr expr ctx)))
                                         context
                                         bindings)]
                 (eval-expr body new-context))
          :if (let [[c t e] args
                    agg? (get-in context [:in-aggregation])
                    cond-res (eval-expr c context)
                    ;;_ (println "IF condition result:" cond-res)  ; Debug log
                    ;;_ (println "Then expr:" t)  ; Debug log
                    ;;_ (println "Else expr:" e)  ; Debug log
                    ]
                (if (not agg?)
                ;; Scalar IF
                  (if cond-res
                    (eval-expr t context)
                    (eval-expr e context))
                ;; Vectorized IF in aggregation mode
                  (let [then-res (eval-expr t context)
                        else-res (eval-expr e context)]
                    (when (not (vector? cond-res))
                      (throw (ex-info "Condition in IF must return vector of booleans in aggregator mode"
                                      {:condition cond-res})))
                    (when (some #(not (or (true? %) (false? %))) cond-res)
                      (throw (ex-info "Condition vector contains non-boolean values"
                                      {:condition cond-res})))

                    (when (or (not (vector? then-res)) (not (vector? else-res)))
                      (throw (ex-info "Then and else must be vectors in aggregator mode"
                                      {:then then-res :else else-res})))

                    (when (or (not= (count cond-res) (count then-res))
                              (not= (count cond-res) (count else-res)))
                      (throw (ex-info "Vectors in IF must have matching lengths"
                                      {:condition-count (count cond-res)
                                       :then-count (count then-res)
                                       :else-count (count else-res)})))

                    (mapv (fn [cval tval eval] (if cval tval eval))
                          cond-res then-res else-res))))

          :gt (let [[l r] args
                    lv (eval-expr l context)
                    rv (eval-expr r context)]
                (vectorize-op > lv rv))

          :lt (let [[l r] args
                    lv (eval-expr l context)
                    rv (eval-expr r context)]
                (vectorize-op < lv rv))

          :gte (let [[l r] args
                     lv (eval-expr l context)
                     rv (eval-expr r context)]
                 (vectorize-op >= lv rv))

          :lte (let [[l r] args
                     lv (eval-expr l context)
                     rv (eval-expr r context)]
                 (vectorize-op <= lv rv))

          :eq (let [[l r] args
                    lv (eval-expr l context)
                    rv (eval-expr r context)]
                (vectorize-op = lv rv))

          :neq (let [[l r] args
                     lv (eval-expr l context)
                     rv (eval-expr r context)]
              ;; vectorize-op = returns booleans, negate them for !=
                 (mapv not (vectorize-op = lv rv)))

          :add (let [[l r] args
                     lv (eval-expr l context)
                     rv (eval-expr r context)]
                 (vectorize-op + lv rv))

          :subtract (let [[l r] args
                          lv (eval-expr l context)
                          rv (eval-expr r context)]
                      (vectorize-op - lv rv))

          :multiply (let [[l r] args
                          lv (eval-expr l context)
                          rv (eval-expr r context)]
                      (vectorize-op * lv rv))

          :divide (let [[l r] args
                        lv (eval-expr l context)
                        rv (eval-expr r context)]
                    (vectorize-op / lv rv))

          :function

          (let [[func-name arg-exprs] args
                force-scalar? (contains? non-agg-functions func-name)
                is-agg? (contains? aggregator-fns func-name)
                agg-context (if force-scalar?
                              (assoc context :in-aggregation false)
                              (assoc context :in-aggregation true))

                        ;; First, fully resolve all argument values
                values (if force-scalar?
                         (mapv #(eval-expr % agg-context) arg-exprs)
                         (mapcat
                          (fn [ex]
                            (let [val (eval-expr ex agg-context)]
                              (if (coll? val) val [val])))
                          arg-exprs))

                        ;; Only cache if this is a "leaf" function (all args are primitive values)
                is-leaf-fn? (every? (fn [v] (or (number? v)
                                                (string? v)
                                                (and (vector? v)
                                                     (every? #(or (number? %) (string? %)) v))))
                                    values)

                cache-key (when is-leaf-fn?
                            (cond
                                      ;; For aggregations, use hash of values instead of full vector
                              (and is-agg? (not (:in-aggregation context)))
                              (let [values-vec (vec values)]
                                [func-name
                                 (count values-vec)  ; Include count for safety
                                 (hash values-vec)]) ; Use hash instead of full vector

                                      ;; For scalar functions, keep existing strategy
                              force-scalar?
                              [func-name (vec values)]

                              :else nil))
                _ (swap! times inc)

                  ;; Debug logging
                _ (when false ;(= func-name :countd)
                    (println "\n-------------------------------------------")
                    (println func-name " Debug:")
                    (println "Row index:" (:row-index context))
                    (println "Is leaf function?" is-leaf-fn?)
                    (println "Values:" (vec (take 5 values)) "...")
                    (println "Cache key:" cache-key)
                    (println "Cache hit?" (boolean (get @function-cache cache-key))))]
                    ;;(time-it (str "Cache lookup " func-name " cache-key: " cache-key " hit? "
                    ;;              (boolean (get @function-cache cache-key)) " #" @times " / " @htimes  "* cache-keys: " (count (keys @function-cache)))
            (if-let [cached (when cache-key (@function-cache cache-key))]
              cached
              (let [result (case func-name

                             :true
                             true

                             :false
                             false

                             :vlookup
                             (let [[lookup-expr source-expr & rest-args] arg-exprs
                                   lookup-val (eval-expr lookup-expr context)
                                   source-val (eval-expr source-expr context)]
                               (cond
               ;; keys array form: VLOOKUP(value, keys_array, values_array, approx)
                                 (and (coll? source-val) (>= (count rest-args) 1))
                                 (let [values-expr (first rest-args)   ;; The first of rest-args is values_array
                                       values-val (eval-expr values-expr context)
                                       approx (if (> (count rest-args) 1)
                                                (boolean (eval-expr (second rest-args) context))
                                                false)]
                                   (vlookup-logic lookup-val source-val values-val approx))

                ;; Named resource form: VLOOKUP(value, "ResourceName", approx)
                                 (string? source-val)
                                 (let [approx (if (not-empty rest-args)
                                                (boolean (eval-expr (first rest-args) context))
                                                false)
                                       [keys-array values-array] (resolve-resource source-val context)]
                                   (vlookup-logic lookup-val keys-array values-array approx))

                                 :else
                                 (throw (ex-info "Invalid VLOOKUP arguments"
                                                 {:lookup lookup-val :source source-val :args rest-args}))))

              ;; notes: 12/14/2024
              ;;   (evaluate-formula "=VLOOKUP(sales, [0, 1000, 2000],
              ;;          ['Tier 1', 'Tier 2', 'Tier 3'], TRUE())" test-data)
              ;;(value, keys_vector, values_vector, approximate?)
              ;;(value, resource_name, approximate?) <--- resource_name is a string, will sub with rabbit table name

                             :case
                             (let [[expr & rest-args] arg-exprs
                                   val (eval-expr expr context)
                                   pairs (partition 2 (butlast rest-args))
                                   default (last rest-args)
                                   compare-vals (fn [v1 v2]
                                                  (cond
                                                    (and (number? v1) (number? v2))
                                                    (= (double v1) (double v2))

                                                    (and (string? v1) (string? v2))
                                                    (= v1 v2)

                                                    :else false))]
                               (if (true? val)
                    ;; For TRUE() case, evaluate the condition for current row only
                                 (let [curr-row (:row-context context)]
                                   (or (some (fn [[condition result-expr]]
                                               (when (eval-expr condition context)  ; context already has current row
                                                 (eval-expr result-expr context)))
                                             pairs)
                                       (eval-expr default context)))
                    ;; Normal CASE behavior for value matching
                                 (if (coll? val)
                                   (mapv (fn [v]
                                           (or (some (fn [[case-val result-expr]]
                                                       (when (compare-vals (eval-expr case-val context) v)
                                                         (eval-expr result-expr context)))
                                                     pairs)
                                               (eval-expr default context)))
                                         val)
                                   (or (some (fn [[case-val result-expr]]
                                               (when (compare-vals (eval-expr case-val context) val)
                                                 (eval-expr result-expr context)))
                                             pairs)
                                       (eval-expr default context)))))


                             :switch
                             (let [expr (eval-expr (first arg-exprs) context)
                                   pairs (partition 2 (rest arg-exprs))
                                   default (when (odd? (count (rest arg-exprs)))
                                             (last arg-exprs))]
                               (or (some (fn [[case result]]
                                           (when (= (eval-expr case context) expr)
                                             (eval-expr result context)))
                                         pairs)
                                   (when default
                                     (eval-expr default context))))



                             :array_length
                             (let [arr (eval-expr (first arg-exprs) context)]
                               (if (coll? arr)
                                 (count arr)
                                 1))

                             :sort
                             (let [arr (eval-expr (first arg-exprs) context)
                                   desc? (when (second arg-exprs)
                                           (eval-expr (second arg-exprs) context))]
                               (if desc?
                                 (vec (sort > arr))
                                 (vec (sort arr))))

                             :running_sum
                             (let [[val-expr] arg-exprs
                                   vals (eval-expr val-expr agg-context)
                      ;; If we're in aggregation context, calculate running sum for all values
                                   running-sums (if (coll? vals)
                                                  (vec (reductions + vals))
                                                  [vals])]
                               (if (get-in context [:in-aggregation])
                                 running-sums
                    ;; If not in aggregation context, return just the current row's running sum
                                 (nth running-sums (:row-index context))))

                             :lag
                             (let [[val-expr offset-expr default-expr] arg-exprs
                                   offset (int (if offset-expr
                                                 (eval-expr offset-expr context)
                                                 1))  ; ensure offset is an integer
                                   default (if default-expr
                                             (eval-expr default-expr context)
                                             nil)
                                   curr-idx (:row-index context)
                                   prev-idx (- curr-idx offset)  ; should now be integer math
                                   column-key (if (instance? ColumnRef val-expr)
                                                (:name val-expr)
                                                (keyword (name (:name val-expr))))
                                   result (if (and (>= prev-idx 0)
                                                   (< prev-idx (count (:data context))))
                                            (get-in (:data context) [prev-idx column-key])
                                            default)]
                               result)

                             :lead
                             (let [[val-expr offset-expr default-expr] arg-exprs
                                   offset (int (if offset-expr
                                                 (eval-expr offset-expr context)
                                                 1))  ; ensure offset is an integer
                                   default (if default-expr
                                             (eval-expr default-expr context)
                                             nil)
                                   curr-idx (:row-index context)
                                   next-idx (+ curr-idx offset)  ; integer math
                                   column-key (if (instance? ColumnRef val-expr)
                                                (:name val-expr)
                                                (keyword (name (:name val-expr))))
                                   result (if (and (>= next-idx 0)
                                                   (< next-idx (count (:data context))))
                                            (get-in (:data context) [next-idx column-key])
                                            default)]
                               result)

                             :rank
                             (let [val-expr (first arg-exprs)
                                   vals (eval-expr val-expr context)
                                   desc? (when (second arg-exprs)
                                           (eval-expr (second arg-exprs) context))
                                   rank-map (into {}
                                                  (map-indexed
                                                   (fn [idx v] [v (inc idx)])
                                                   (sort (if desc? > <) (distinct vals))))]
                               (if (coll? vals)
                                 (mapv rank-map vals)
                                 (rank-map vals)))

                             :power
                             (let [[base-expr exp-expr] arg-exprs
                                   base (eval-expr base-expr context)
                                   exp (eval-expr exp-expr context)]
                               (if (or (coll? base) (coll? exp))
                                 (let [bases (if (coll? base) base (repeat (count (or (coll? exp) [exp])) base))
                                       exps (if (coll? exp) exp (repeat (count bases) exp))]
                                   (mapv #(Math/pow %1 %2) bases exps))
                                 (Math/pow base exp)))

                             :log
                             (let [[num-expr] arg-exprs
                                   val (eval-expr num-expr context)]
                               (if (coll? val)
                                 (mapv #(if (and % (> % 0))
                                          (Math/log %)
                                          nil)
                                       val)
                                 (if (and val (> val 0))
                                   (Math/log val)
                                   nil)))

                             :log10
                             (let [[num-expr] arg-exprs
                                   val (eval-expr num-expr context)]
                               (if (coll? val)
                                 (mapv #(if (and % (> % 0))
                                          (Math/log10 %)
                                          nil)
                                       val)
                                 (if (and val (> val 0))
                                   (Math/log10 val)
                                   nil)))

                             :sin
                             (let [[angle-expr] arg-exprs
                                   val (eval-expr angle-expr context)]
                               (if (coll? val)
                                 (mapv #(if % (Math/sin %) nil) val)
                                 (if val (Math/sin val) nil)))

                             :cos
                             (let [[angle-expr] arg-exprs
                                   val (eval-expr angle-expr context)]
                               (if (coll? val)
                                 (mapv #(if % (Math/cos %) nil) val)
                                 (if val (Math/cos val) nil)))

                             :tan
                             (let [[angle-expr] arg-exprs
                                   val (eval-expr angle-expr context)]
                               (if (coll? val)
                                 (mapv #(if % (Math/tan %) nil) val)
                                 (if val (Math/tan val) nil)))

                             :asin
                             (let [[num-expr] arg-exprs
                                   val (eval-expr num-expr context)]
                               (if (coll? val)
                                 (mapv #(if (and % (>= % -1) (<= % 1))
                                          (Math/asin %)
                                          nil)
                                       val)
                                 (if (and val (>= val -1) (<= val 1))
                                   (Math/asin val)
                                   nil)))

                             :acos
                             (let [[num-expr] arg-exprs
                                   val (eval-expr num-expr context)]
                               (if (coll? val)
                                 (mapv #(if (and % (>= % -1) (<= % 1))
                                          (Math/acos %)
                                          nil)
                                       val)
                                 (if (and val (>= val -1) (<= val 1))
                                   (Math/acos val)
                                   nil)))

                             :atan
                             (let [[num-expr] arg-exprs
                                   val (eval-expr num-expr context)]
                               (if (coll? val)
                                 (mapv #(if % (Math/atan %) nil) val)
                                 (if val (Math/atan val) nil)))

                             :abs
                             (let [[num-expr] arg-exprs
                                   val (eval-expr num-expr context)]
                               (if (coll? val)
                                 (mapv #(if % (Math/abs %) nil) val)
                                 (if val (Math/abs val) nil)))

                             :exp
                             (let [[num-expr] arg-exprs
                                   val (eval-expr num-expr context)]
                               (if (coll? val)
                                 (mapv #(if % (Math/exp %) nil) val)
                                 (if val (Math/exp val) nil)))

                             :sqrt
                             (let [[num-expr] arg-exprs
                                   val (eval-expr num-expr context)]
                               (if (coll? val)
                                 (mapv #(if (and % (>= % 0))
                                          (Math/sqrt %)
                                          nil)
                                       val)
                                 (if (and val (>= val 0))
                                   (Math/sqrt val)
                                   nil)))

                             :find
                             (let [[find-expr within-expr start-expr] arg-exprs
                                   find-text (str (eval-expr find-expr context))
                                   within-text (str (eval-expr within-expr context))
                                   start-pos (if start-expr
                                               (dec (eval-expr start-expr context)) ; Convert 1-based to 0-based
                                               0)]
                               (try
                                 (inc (.indexOf within-text find-text start-pos)) ; Convert back to 1-based
                                 (catch Exception _
                                   0))) ; Return 0 if not found (like Excel)

                             :add_months
                             (let [[date-expr months-expr] arg-exprs
                                   val (eval-expr date-expr context)
                                   months (eval-expr months-expr context)
                                   add-months-fn (fn [date months]
                                                   (when date
                                                     (.format
                                                      (.plusMonths (to-date date) months)
                                                      date-formatter)))]
                               (if (coll? val)
                                 (mapv #(add-months-fn % months) val)
                                 (add-months-fn val months)))

                             :add_days
                             (let [[date-expr days-expr] arg-exprs
                                   val (eval-expr date-expr context)
                                   days (eval-expr days-expr context)
                                   add-days-fn (fn [date days]
                                                 (when date
                                                   (.format
                                                    (.plusDays (to-date date) days)
                                                    date-formatter)))]
                               (if (coll? val)
                                 (mapv #(add-days-fn % days) val)
                                 (add-days-fn val days)))

                             :variance
                             (let [vals (mapcat #(let [v (eval-expr % agg-context)]
                                                   (if (coll? v)
                                                     [v]  ; Keep vectors as nested vectors
                                                     (when-not (nil? v) [[v]]))) ; Single values become nested vectors
                                                arg-exprs)
                                   calc-variance (fn [nums]
                                                   (when (seq nums)
                                                     (let [n (count nums)
                                                           mean (/ (reduce + nums) n)
                                                           squared-diff-sum (reduce + (map #(let [diff (- % mean)]
                                                                                              (* diff diff))
                                                                                           nums))]
                                                       (/ squared-diff-sum n))))]
                               (if (and (= 1 (count vals)) (vector? (first vals)))
                                 (let [groups (first vals)]
                                   (if (every? vector? groups)
            ;; Handle nested vectors (grouped data)
                                     (mapv calc-variance groups)
            ;; Handle single vector
                                     (calc-variance groups)))
            ;; Handle multiple arguments
                                 (calc-variance (flatten vals))))

                             :stddev
                             (let [vals (mapcat #(let [v (eval-expr % agg-context)]
                                                   (if (coll? v)
                                                     [v]  ; Keep vectors as nested vectors
                                                     (when-not (nil? v) [[v]]))) ; Single values become nested vectors
                                                arg-exprs)
                                   calc-stddev (fn [nums]
                                                 (when (seq nums)
                                                   (let [n (count nums)
                                                         mean (/ (reduce + nums) n)
                                                         squared-diff-sum (reduce + (map #(let [diff (- % mean)]
                                                                                            (* diff diff))
                                                                                         nums))]
                                                     (Math/sqrt (/ squared-diff-sum n)))))]
                               (if (and (= 1 (count vals)) (vector? (first vals)))
                                 (let [groups (first vals)]
                                   (if (every? vector? groups)
            ;; Handle nested vectors (grouped data)
                                     (mapv calc-stddev groups)
            ;; Handle single vector
                                     (calc-stddev groups)))
            ;; Handle multiple arguments
                                 (calc-stddev (flatten vals))))

                             :left
                             (let [[text-expr length-expr] arg-exprs
                                   text (str (eval-expr text-expr context))
                                   length (eval-expr length-expr context)]
                               (if (< length (count text))
                                 (subs text 0 length)
                                 text))

                             :right
                             (let [[text-expr length-expr] arg-exprs
                                   text (str (eval-expr text-expr context))
                                   length (eval-expr length-expr context)]
                               (if (< length (count text))
                                 (subs text (- (count text) length))
                                 text))

                             :proper
                             (let [[text-expr] arg-exprs
                                   text (str (eval-expr text-expr context))]
                               (str/join " "
                                         (map str/capitalize
                                              (str/split text #"\s+"))))

                             :quarter
                             (let [[date-expr] arg-exprs
                                   val (eval-expr date-expr context)
                                   date (if (coll? val)
                                          (mapv to-date val)
                                          (to-date val))]
                               (if (coll? date)
                                 (mapv #(if % (inc (quot (.getMonthValue %) 3)) nil) date)
                                 (if date (inc (quot (.getMonthValue date) 3)) nil)))

                             :year
                             (let [[date-expr] arg-exprs
                                   val (eval-expr date-expr context)
                                   date (if (coll? val)
                                          (mapv to-date val)
                                          (to-date val))]
                               (if (coll? date)
                                 (mapv #(if % (.getYear %) nil) date)
                                 (if date (.getYear date) nil)))

                             :month
                             (let [[date-expr] arg-exprs
                                   val (eval-expr date-expr context)
                                   date (if (coll? val)
                                          (mapv to-date val)
                                          (to-date val))]
                               (if (coll? date)
                                 (mapv #(if % (.getMonthValue %) nil) date)
                                 (if date (.getMonthValue date) nil)))

                             :day
                             (let [[date-expr] arg-exprs
                                   val (eval-expr date-expr context)
                                   date (if (coll? val)
                                          (mapv to-date val)
                                          (to-date val))]
                               (if (coll? date)
                                 (mapv #(if % (.getDayOfMonth %) nil) date)
                                 (if date (.getDayOfMonth date) nil)))

                             :week
                             (let [[date-expr] arg-exprs
                                   val (eval-expr date-expr context)
                                   date (if (coll? val)
                                          (mapv to-date val)
                                          (to-date val))
                                   get-week (fn [d]
                                              (if d
                                                (.get d java.time.temporal.IsoFields/WEEK_OF_WEEK_BASED_YEAR)
                                                nil))]
                               (if (coll? date)
                                 (mapv get-week date)
                                 (get-week date)))

                             :weekday
                             (let [[date-expr] arg-exprs
                                   date (to-date (eval-expr date-expr context))]
                               (.getValue (.getDayOfWeek date)))

                             :isnumber
                             (let [[val-expr] arg-exprs
                                   val (eval-expr val-expr context)]
                               (if (coll? val)
                                 (mapv #(boolean (or (number? %)
                                                     (and (string? %)
                                                          (try (Double/parseDouble %)
                                                               true
                                                               (catch Exception _ false)))))
                                       val)
                                 (boolean (or (number? val)
                                              (and (string? val)
                                                   (try (Double/parseDouble val)
                                                        true
                                                        (catch Exception _ false)))))))

                             :istext
                             (let [[val-expr] arg-exprs
                                   val (eval-expr val-expr context)]
                               (if (coll? val)
                                 (mapv #(boolean (and (string? %)
                                                      (try (Double/parseDouble %)
                                                           false
                                                           (catch Exception _ true))))
                                       val)
                                 (boolean (and (string? val)
                                               (try (Double/parseDouble val)
                                                    false
                                                    (catch Exception _ true))))))

                             :isnull
                             (let [[val-expr] arg-exprs
                                   val (eval-expr val-expr context)]
                               (if (coll? val)
                                 (mapv nil? val)
                                 (nil? val)))

                             :to_number
                             (let [[val-expr] arg-exprs
                                   val (eval-expr val-expr context)]
                               (if (coll? val)
                                 (mapv #(cond
                                          (number? %) %
                                          (string? %) (try (Double/parseDouble %)
                                                           (catch Exception _ nil))
                                          :else nil)
                                       val)
                                 (cond
                                   (number? val) val
                                   (string? val) (try (Double/parseDouble val)
                                                      (catch Exception _ nil))
                                   :else nil)))

                             :to_text
                             (let [[val-expr] arg-exprs
                                   val (eval-expr val-expr context)]
                               (if (coll? val)
                                 (mapv #(when-not (nil? %) (str %)) val)
                                 (when-not (nil? val) (str val))))

                             :coalesce
                             (let [vals (map #(eval-expr % context) arg-exprs)]
                               (first (remove nil? vals)))

                             :unique
                             (let [[expr] arg-exprs
                                   vals (eval-expr expr context)]
                               (vec (distinct vals)))

                             :round
                             (let [[num-expr decimals-expr] arg-exprs
                                   num (eval-expr num-expr context)
                                   decimals (if decimals-expr
                                              (eval-expr decimals-expr context)
                                              0) ;; default to 0 decimals if not specified
                                   scale (Math/pow 10 decimals)]
                               (if (coll? num)
                                 (mapv #(/ (Math/round (* % scale)) scale) num)
                                 (/ (Math/round (* num scale)) scale)))

                             :floor
                             (let [[num-expr decimals-expr] arg-exprs
                                   num (eval-expr num-expr context)
                                   decimals (if decimals-expr
                                              (eval-expr decimals-expr context)
                                              0)
                                   scale (Math/pow 10 decimals)]
                               (if (coll? num)
                                 (mapv #(/ (Math/floor (* % scale)) scale) num)
                                 (/ (Math/floor (* num scale)) scale)))

                             :ceil
                             (let [[num-expr decimals-expr] arg-exprs
                                   num (eval-expr num-expr context)
                                   decimals (if decimals-expr
                                              (eval-expr decimals-expr context)
                                              0)
                                   scale (Math/pow 10 decimals)]
                               (if (coll? num)
                                 (mapv #(/ (Math/ceil (* % scale)) scale) num)
                                 (/ (Math/ceil (* num scale)) scale)))

                             (:current_date :today)
                             (.format (LocalDate/now) date-formatter) ;; Returns "yyyy-MM-dd" formatted string

                             :now
                             (.format (java.time.LocalDateTime/now)
                                      (DateTimeFormatter/ofPattern "yyyy-MM-dd HH:mm:ss"))

                             :yearmonth
                             (let [[date-expr] arg-exprs
                                   val (eval-expr date-expr context)]
                               (vectorize-date-fn yearmonth-fn val))

                             :months_between
                             (let [[d1-expr d2-expr] arg-exprs
                                   d1 (eval-expr d1-expr context)
                                   d2 (eval-expr d2-expr context)]
                               (vectorize-date-fn months-between-fn d1 d2))

                             :days_between
                             (let [[d1-expr d2-expr] arg-exprs
                                   d1 (eval-expr d1-expr context)
                                   d2 (eval-expr d2-expr context)]
                               (vectorize-date-fn days-between-fn d1 d2))

                             :days_since
                             (let [[date-expr] arg-exprs
                                   val (eval-expr date-expr context)]
                               (vectorize-date-fn days-since-fn val))

                             :split
                             (let [[text-expr delim-expr] arg-exprs
                                   text (eval-expr text-expr context)
                                   delim (eval-expr delim-expr context)]
                               (if (coll? text)
                                 (mapv #(if (string? %) (vec (str/split % (re-pattern (java.util.regex.Pattern/quote delim)))) nil) text)
                                 (if (string? text)
                                   (vec (str/split text (re-pattern (java.util.regex.Pattern/quote delim))))
                                   nil)))

                             :replace
                             (let [[text-expr search-expr repl-expr] arg-exprs
                                   text (eval-expr text-expr context)
                                   search (eval-expr search-expr context)
                                   repl (eval-expr repl-expr context)]
                               (if (coll? text)
                                 (mapv #(if (string? %) (str/replace % search repl) %) text)
                                 (if (string? text)
                                   (str/replace text search repl)
                                   nil)))

                             :substr
                             (let [[text-expr start-expr len-expr] arg-exprs
                                   text (eval-expr text-expr context)
                                   start (int (eval-expr start-expr context))
                                   length (int (eval-expr len-expr context))]
                               (if (coll? text)
                                 (mapv #(if (and (string? %) (>= (count %) start))
                                          (subs % start (min (count %) (+ start length)))
                                          nil) text)
                                 (if (and (string? text) (>= (count text) start))
                                   (subs text start (min (count text) (+ start length)))
                                   nil)))

                             :upper
                             (let [val (first values)]
                               (if (string? val)
                                 (str/upper-case val)
                                 val))

                             :lower
                             (let [val (first values)]
                               (if (string? val)
                                 (str/lower-case val)
                                 val))

                             :text
                             (let [[value-expr format-expr] arg-exprs
                                   value (eval-expr value-expr context)
                                   format-pattern (eval-expr format-expr context)]
                               (if (get-in context [:in-aggregation])
                  ;; Handle vector input
                                 (if (coll? value)
                                   (mapv #(text-format % format-pattern) value)
                                   (text-format value format-pattern))
                  ;; Handle scalar input
                                 (text-format value format-pattern)))


                             :and (let [vals (map #(eval-expr % context) arg-exprs)]
                                    (if (some coll? vals)
                                      (let [max-len (apply max (map count (filter coll? vals)))
                                            expanded (map (fn [v] (if (coll? v) v (repeat max-len v))) vals)]
                                        (mapv (fn [& vs] (every? true? vs)) (apply map vector expanded)))
                                      (every? true? vals)))

                             :or (let [vals (map #(eval-expr % context) arg-exprs)]
                                   (if (some coll? vals)
                                     (let [max-len (apply max (map count (filter coll? vals)))
                                           expanded (map (fn [v] (if (coll? v) v (repeat max-len v))) vals)]
                                       (mapv (fn [& vs] (some true? vs)) (apply map vector expanded)))
                                     (some true? vals)))

                             :not (let [val (eval-expr (first arg-exprs) context)]
                                    (if (coll? val)
                                      (mapv not val)
                                      (not val)))


                             :len (let [vals (map #(eval-expr % context) arg-exprs)]
                                    (if (some coll? vals)
                         ;; Element-wise if vector found
                                      (let [flattened (mapcat (fn [v] (if (coll? v) v [v])) vals)]
                                        (mapv string-len flattened))
                         ;; Scalar
                                      (string-len (first vals))))

                             :concat (let [vals (map #(eval-expr % context) arg-exprs)]
                                       (if (some coll? vals)
                            ;; If any arg is vector, align them element-wise:
                                         (let [vectors (map #(if (coll? %) % [%]) vals)
                                               max-len (apply max (map count vectors))]
                                           (mapv (fn [i]
                                                   (string-concat (map #(nth % i "") vectors)))
                                                 (range max-len)))
                            ;; All scalar
                                         (string-concat vals)))

                             :min (let [vals (mapcat #(let [v (eval-expr % agg-context)]
                                                        (if (coll? v) v [v]))
                                                     arg-exprs)]
                                    (if (empty? vals) nil (reduce min vals)))

                             :max (let [vals (mapcat #(let [v (eval-expr % agg-context)]
                                                        (if (coll? v) v [v]))
                                                     arg-exprs)]
                                    (if (empty? vals) nil (reduce max vals)))

                             :large (let [[range-expr n-expr] arg-exprs
                                          vals (mapcat #(if (coll? %) % [%])
                                                       (eval-expr range-expr agg-context))
                                          n (int (eval-expr n-expr context))]
                                      (if (or (empty? vals) (> n (count vals)))
                                        nil
                                        (nth (reverse (sort vals)) (dec n)))) ; nth largest: sort descending and pick (n-1) index

                             :small (let [[range-expr n-expr] arg-exprs
                                          vals (mapcat #(if (coll? %) % [%])
                                                       (eval-expr range-expr agg-context))
                                          n (int (eval-expr n-expr context))]
                                      (if (or (empty? vals) (> n (count vals)))
                                        nil
                                        (nth (sort vals) (dec n)))) ; nth smallest

                ;; SUMIF, COUNTIF, AVERAGEIF:
                             :sumif (let [[range-expr cond-expr] arg-exprs]
                                      (let [range-vals (eval-expr range-expr agg-context)
                                            condition-result (eval-expr cond-expr agg-context)]
                                        (sumif-like range-vals condition-result)))

                             :countif (let [[range-expr cond-expr] arg-exprs]
                                        (let [range-vals (eval-expr range-expr agg-context)
                                              condition-result (eval-expr cond-expr agg-context)]
                                          (countif-like range-vals condition-result)))

                             :averageif (let [[range-expr cond-expr] arg-exprs]
                                          (let [range-vals (eval-expr range-expr agg-context)
                                                condition-result (eval-expr cond-expr agg-context)]
                                            (averageif-like range-vals condition-result)))

                ;; SUMIFS, COUNTIFS, AVERAGEIFS:
                             :sumifs (sumifs-like arg-exprs agg-context)
                             :countifs (countifs-like arg-exprs agg-context)
                             :averageifs (averageifs-like arg-exprs agg-context)

                             :current
                             (let [col-name-arg (first arg-exprs)
                                   col-name (cond
                                              (string? col-name-arg)
                                              col-name-arg

                                              (instance? ColumnRef col-name-arg)
                             ;; Extract the column name from the ColumnRef
                                              (name (:name col-name-arg))

                                              :else
                             ;; If it's something else, try evaluating it. But ideally, CURRENT should
                             ;; always get a simple column reference or string.
                                              (let [val (eval-expr col-name-arg context)]
                                                (if (string? val)
                                                  val
                                                  (throw (ex-info "CURRENT requires a column name" {:val val})))))]
              ;; Now col-name is a string representing the column name
                               (get (:base-row context) (keyword col-name)))


                             :offset
                             (let [[col-expr offset-expr] arg-exprs
                                   offset (int (eval-expr offset-expr context)) ; ensure offset is int
                                   col-val (eval-expr col-expr context)]
                               (if (get-in context [:in-aggregation])
                    ;; Aggregation mode: col-val is a vector of the entire column
                                 (let [col-vec (if (coll? col-val) col-val [col-val]) ; ensure vector
                                       length (count col-vec)]
                                   (mapv (fn [i]
                                           (let [new-idx (+ i offset)]
                                             (if (and (>= new-idx 0) (< new-idx length))
                                               (col-vec new-idx)
                                               nil)))
                                         (range length)))
                    ;; scalar mode:
                    ;; col-val in scalar mode is just current row value
                                 (let [row-idx (+ (:row-index context) offset)]
                                   (if (and (>= row-idx 0) (< row-idx (count (:data context))))
                                     (get-in (:data context) [row-idx (:name col-expr)])
                                     nil))))

                             :filter
                             (let [[col-expr cond-expr] arg-exprs
                                   agg-context (assoc context :in-aggregation true)
                      ;; Evaluate cond-expr in agg mode to get a boolean vector
                                   condition-values (eval-expr cond-expr agg-context)
                                   col-values (eval-expr col-expr agg-context)]
                               (when-not (and (coll? condition-values) (coll? col-values))
                                 (throw (ex-info "FILTER expects vector conditions and column values" {})))
                               (when (not= (count condition-values) (count col-values))
                                 (throw (ex-info "FILTER condition and column must match length" {})))
                  ;; Filter col-values where condition-values is true
                               (mapv second (filter (fn [[c v]] c) (map vector condition-values col-values))))

                             :sum (reduce + 0 values)
                             :average (/ (reduce + 0 values) (count values))
                             :median (median values)
                             :mode (mode values)
                             :count (count (remove nil? values))
                             :countd (count (distinct (remove nil? values)))
                             (throw (ex-info "Unknown function" {:function func-name})))]
                                ;;  (println func-name " return value "
                                ;;           (if (> (count (str result)) 50)
                                ;;             (subs (str result) 0 50)
                                ;;             result)
                                ;;           " type " (type result))
                (swap! htimes inc)
                (swap! function-cache assoc cache-key result)
                result))) ;)

          (throw (ex-info "Unknown operator" {:op op})))
        (mapv #(eval-expr % context) expr)))

    :else
    (throw (ex-info "Unknown expression type" {:expr expr}))))




(defn rewrite-average-if [ast]
  (cond
    ;; If it's a function call with an aggregator
    (and (vector? ast)
         (= :function (first ast))
         (aggregator-fns (second ast)))
    (let [[_ func-name args] ast
          ;; args is a vector of expressions
          rewritten-args (mapv rewrite-average-if args)]
      ;; Check if the aggregator has a single argument and that argument is an IF
      (if (and (= (count rewritten-args) 1)
               (vector? (first rewritten-args))
               (= :if (first (first rewritten-args))))
        ;; We have something like AVERAGE(IF(...))
        (let [[_ condition then-expr else-expr] (first rewritten-args)]
          ;; Rewrite to IF(condition, AVERAGE(then-expr), AVERAGE(else-expr)) - cheating
          [:if condition
           [:function func-name [then-expr]]
           [:function func-name [else-expr]]])
        ;; Otherwise, just return the function call with rewritten args
        [:function func-name rewritten-args]))

    ;;  an IF expression, rewrite its children too
    (and (vector? ast) (= :if (first ast)))
    (let [[_ cond t e] ast]
      [:if (rewrite-average-if cond)
       (rewrite-average-if t)
       (rewrite-average-if e)])

    ;;  if it's a vector or something else, just walk it
    (vector? ast)
    (mapv rewrite-average-if ast)

    :else
    ast))

(defn estimate-chunk-size [data-size formula-ast]
  (let [available-cores (.availableProcessors (Runtime/getRuntime))

        ;; Target chunks per core (aim for good parallelization)
        chunks-per-core 6
        target-chunks (* available-cores chunks-per-core)

        ;; Simple chunk size calculation
        chunk-size (-> (quot data-size target-chunks)
                       (max 50)    ;; Minimum size
                       (min 200)   ;; Maximum size
                       int)]

    (println "Calculated chunk size:" chunk-size
             "\n  Data size:" data-size
             "\n  Available cores:" available-cores
             "\n  Target chunks:" target-chunks)

    chunk-size))

(def formula-parser-cache (atom {}))

(defn get-cached-ast [formula]
  (if-let [cached (@formula-parser-cache formula)]
    (do
      ;;(println "🎯 Parser cache hit!")
      cached)
    (let [formula (-> formula
                      (str/replace  #"'" "\"")
                      (str/replace  #"TRUE" "true")
                      (str/replace  #"FALSE" "false")
                      (str/replace  #"True" "true")
                      (str/replace  #"False" "false"))
          tokens (tokenize-nested-formula formula)
          parser (make-parser tokens)
          ast ((:parse-expression parser))
          rewritten-ast (rewrite-average-if ast)]
      (swap! formula-parser-cache assoc formula rewritten-ast)
      rewritten-ast)))

(def expression-cache (atom {}))

(defn has-current? [ast]
  (str/includes? (str ast) ":function :current"))

(defn check-formula-syntax [formula]
  (try
    (let [ast (get-cached-ast formula)
          ;; Convert recognized-functions to lowercase keywords for comparison
          valid-functions (cset/union
                          ;; From recognized-functions (strings to keywords)
                           (set (map (comp keyword str/lower-case) recognized-functions))
                          ;; From aggregator-fns (already keywords)
                           aggregator-fns)
          errors (atom [])]

      ;; Validate function calls in AST
      (letfn [(validate-node [node]
                (when (vector? node)
                  (let [[type func-name args] node]
                    (when (= :function type)
                      (when-not (contains? valid-functions func-name)
                        (swap! errors conj
                               {:error :unknown-function
                                :message (format "Unknown function: %s"
                                                 (str/upper-case (name func-name)))})))
                    ;; Recurse through arguments
                    (when (sequential? args)
                      (doseq [arg args]
                        (validate-node arg))))))]

        (validate-node ast)

        (if (seq @errors)
          {:valid false :errors @errors}
          {:valid true})))

    (catch Exception e
      {:valid false
       :errors [{:error :syntax-error
                 :message (str "Invalid formula syntax: " (.getMessage e))}]})))


(defn evaluate-formula [formula data & [result-field]]
  (let [data (vec (doall data))
        ;_ (println (str "Check Excel Syntax: " (check-formula-syntax formula)))
        rewritten-ast (get-cached-ast formula)
       ;;; _ (println (str "rewritten-ast: " rewritten-ast))
        result-field (or result-field :result)
        result-field (if (keyword? result-field)
                       result-field
                       (keyword (str/trim (str result-field))))

        ;; needed columns from AST
        needed-cols (->> rewritten-ast
                         (tree-seq coll? seq)
                         (keep (fn [node]
                                 (cond
                                   (and (record? node)
                                        (= (type node) rvbbit_backend.rabbit_script.ColumnRef))
                                   (:name node)

                                   (and (record? node)
                                        (= (type node) rvbbit_backend.rabbit_script.FieldRef))
                                   (:column node)

                                   (and (record? node)
                                        (= (type node) rvbbit_backend.rabbit_script.RangeRef))
                                   (first (:columns node))

                                   :else nil)))
                         set)
        _ (ut/pp {"🤑💱🦠" formula :min-cols needed-cols :ast rewritten-ast})

        ;; pass minimal data set (y not?)
        minimal-data (mapv #(select-keys % needed-cols) data)

        ;; is pure agg??
        is-pure-agg? (and (vector? rewritten-ast)
                          (= :function (first rewritten-ast))
                          (contains? #{:countd :sum :average} (second rewritten-ast))
                          (not (has-current? rewritten-ast)))

        ;; if pure single agg we can cheat
        agg-value (when is-pure-agg?
                    (let [minimal-first (first minimal-data)
                          context (->EvalContext 0 minimal-first minimal-data {} {} minimal-first)]
                      (eval-expr rewritten-ast context)))

        ;; else we do the dance
        results (mapv (fn [idx row minimal-row]
                        (let [new-value (if is-pure-agg?
                                          agg-value
                                          (let [context (->EvalContext
                                                         idx
                                                         minimal-row
                                                         minimal-data
                                                         {}
                                                         {}
                                                         minimal-row)]
                                            (eval-expr rewritten-ast context)))]
                          (assoc row result-field new-value)))
                      (range (count data))
                      data
                      minimal-data)]

    (reset! times 0)
    (reset! htimes 0)
    (reset! function-cache {})
    (reset! expression-cache {})
    (mapv #(into (sorted-map) %) results)))



(comment
  (def test-data
    [{:id 1 :sales 100 :cost 80 :category "A" :customer "C1" :tax 0.2 :discount 0.1}
     {:id 2 :sales 120 :cost 85 :category "B" :customer "C2" :tax 0.2 :discount 0.15}
     {:id 3 :sales 90  :cost 70 :category "A" :customer "C2" :tax 0.2 :discount 0.05}
     {:id 4 :sales 200 :cost 150 :category "B" :customer "C3" :tax 0.15 :discount 0.2}
     {:id 5 :sales 180 :cost 140 :category "B" :customer "C3" :tax 0.15 :discount 0.1}])

  (evaluate-formula "=sales" test-data)
  (evaluate-formula "=LOWER(category)" test-data)
  (evaluate-formula "=SUM(sales)" test-data)

  (evaluate-formula "=SUMIFS(sales[1:3], category[1:3], ['A'], customer[1:3], '[C2'])" test-data)

  (evaluate-formula "=sales" test-data)

  ;; Test 1: Simple column reference  -  PASSES!
  (evaluate-formula "=sales" test-data)
  ;; no agg, so will return that value for the row

  ;; Test 2: Basic arithmetic - PASSES!
  (evaluate-formula "=sales * 2" test-data)
  (evaluate-formula "=CONCAT(\" $\", cost, \" for \", category )" test-data)
  (evaluate-formula "=CONCAT('farts_', category, cost )" test-data)

  ;; Test 4: Field references - ALL PASS!
  (evaluate-formula "=sales.2" test-data)
  (evaluate-formula "=sales.2 * sales.4" test-data)
  (evaluate-formula "=sales / OFFSET(sales, -1)" test-data)

  ;; Test Entire column aggregation  -  PASSES!
  (evaluate-formula "=SUM(sales)" test-data)
  (evaluate-formula "=MIN(sales)" test-data)

  (evaluate-formula "=UPPER(category)" test-data)
  (evaluate-formula "=UPPER(CONCAT('farts_', LOWER(category)))" test-data)
  ;; has agg so entire column gets processed

  ;; Test Range aggregation  -  PASSES!
  (evaluate-formula "=SUM(sales[1:3])" test-data)
  ;; has agg so range gets looked up and processed

  ;; Test 3: Simple IF  - PASSES!
  (evaluate-formula "=IF(sales >= 150, cost * 1.1, cost * 2.9)" test-data)

  ;; Test Complex formula with column aggregation   -   PASSES!
  (evaluate-formula "=IF(sales <= AVERAGE(sales), cost * 1.1, cost * 0.9)" test-data)

  ;; Test Nested functions -  PASSES!
  (evaluate-formula "=IF(AVERAGE(sales) > SUM(cost)/5, sales * 1.1, sales * 0.9)" test-data)

  ;; Test Multiple conditions  -   PASSES!
  (evaluate-formula "=IF(sales > AVERAGE(sales), IF(cost < AVERAGE(cost), sales * 1.2, sales * 1.1), sales * 0.9)" test-data)

  ;; Test Range references with functions -  PASSES!
  (evaluate-formula "=IF(sales > AVERAGE(sales[1:3]), cost * 1.1, cost * 0.9)" test-data)

  ;; Test Multiple aggregations in one expression    -  PASSES!
  (evaluate-formula "=(SUM(sales) + AVERAGE(cost)) / 2" test-data)
  (evaluate-formula "=(AVERAGE(sales) - MEDIAN(cost)) * (SUM(sales)/COUNT(sales))" test-data)
  (evaluate-formula "=( (sales + cost) * (1 + tax) ) - ( (sales - cost) * (1 - discount) )" test-data)

  ;; Test Deeply nested expressions:
  (evaluate-formula "=((SUM(sales) * 2) / (AVERAGE(cost) + 10))" test-data)


  ;; Test Field references with aggregations  -  PASSES!
  (evaluate-formula "=IF(sales.1 > AVERAGE(sales), cost * 1.1, cost * 0.9)" test-data)

  ;; Test Multiple ranges  -  PASSES!
  (evaluate-formula "=IF(SUM(sales[1:2]) > SUM(sales[3:4]), cost * 3.1, cost * 1.9)" test-data)

  (evaluate-formula "=AVERAGE(IF(sales > 100, cost, cost))" test-data)

  ;; Test Nested ranges  - FAILS!!!
  (evaluate-formula "=SUM(IF(sales > 100, cost[1:2], cost[3:4]))" test-data)
  (evaluate-formula "=AVERAGE(IF(sales > 100, cost[1:2], cost[3:4]))" test-data)
  (evaluate-formula "=IF(sales > 100, AVERAGE(cost[1:2]), AVERAGE(cost[3:4]))" test-data)
; Execution error (ClassCastException) at carrots.nested-parser3/eval-expr (form-init5938019385332533484.clj:206).
; class clojure.lang.PersistentVector cannot be cast to class java.lang.Number (clojure.lang.PersistentVector is in unnamed module of loader 'app'; java.lang.Number is in module java.base of loader 'bootstrap')

  ;; Test complex formula with column aggregation  -   PASSES!
  (evaluate-formula "=IF(sales > AVERAGE(sales), cost * 1.1, cost * 0.9)" test-data)

  ;; Test Complex arithmetic  -  PASSES!
  (evaluate-formula "=(sales * (1 + tax)) - (cost * (1 - discount))" test-data)

  ;; Test Multiple column references  -  PASSES
  (evaluate-formula "=IF(sales > cost * 1.5, AVERAGE(sales), MEDIAN(cost))" test-data)

  (evaluate-formula "=IF(AND((discount <= 0.2), (cost > 100)), 5, 6)" test-data)
  (evaluate-formula "=IF(OR((sales < cost), (AVERAGE(sales) > 100)), 5, MEDIAN(cost))" test-data)

  ;; Test Mixed aggregations and field references  -  PASSES
  (evaluate-formula "=IF(sales.3 < AVERAGE(sales), SUM(cost[1:3]), MEDIAN(sales[2:4]))" test-data)


  (evaluate-formula "=sales / OFFSET(sales, 1)" test-data)
  (evaluate-formula "=sales * 2 + OFFSET(sales, 1)" test-data)
  (evaluate-formula "=SUM(FILTER(sales, category = CURRENT(category)))" test-data)
  (evaluate-formula "=IF(category = CURRENT(category), sales / SUM(FILTER(sales, category = CURRENT(category))), 0)" test-data)


  (evaluate-formula "=YEARMONTH (\"2021-05-15\")" test-data)
  (evaluate-formula "=MONTHS_BETWEEN (\"2021-01-01\", \"2021-04-01\")" test-data)
  (evaluate-formula "=DAYS_BETWEEN (\"2021-01-01\", \"2021-01-10\")" test-data)
  (evaluate-formula "=DAYS_SINCE (\"2021-05-15\")" test-data)
  (evaluate-formula "=CURRENT_DATE()" test-data)
  (evaluate-formula "=TODAY()" test-data)
  (evaluate-formula "=NOW()" test-data)
  (evaluate-formula "=sales[1:3]" test-data)

  (evaluate-formula "=SPLIT(\"a,b,c\", \",\")" test-data)
  (evaluate-formula "=REPLACE(\"hello world\", \"world\", \"carrots\")" test-data)
  (evaluate-formula "=SUBSTR(\"hello world\", 7, 5)" test-data)

  ;; Test usage:
  (evaluate-formula "=ROUND(123.456)" test-data)
;; Returns: 123

  (evaluate-formula "=ROUND(123.456, 2)" test-data)
;; Returns: 123.46

  (evaluate-formula "=FLOOR(123.456)" test-data)
;; Returns: 123

  (evaluate-formula "=FLOOR(123.456, 2)" test-data)
;; Returns: 123.45

  (evaluate-formula "=CEIL(123.456)" test-data)
;; Returns: 124

  (evaluate-formula "=CEIL(123.456, 2)" test-data)
;; Returns: 123.46

;; They also work with column references
  (evaluate-formula "=ROUND(sales, 1)" test-data)
;; Returns rounded values for each row

;; And in combinations
  (evaluate-formula "=ROUND(sales * 1.234, 2)" test-data)
;; Returns calculations rounded to 2 decimal places

  (evaluate-formula "=LEFT(category, 1)" test-data)
  (evaluate-formula "=PROPER(CONCAT(category, ' - ', customer))" test-data)
  (evaluate-formula "=QUARTER(date)" test-data)
  (evaluate-formula "=COALESCE(null_column, sales, 'N/A')" test-data)
  (evaluate-formula "=UNIQUE(category[1:4])" test-data)

  (def test-dates
    [{:id 1 :date "2024-02-02"}
     {:id 2 :date "2024-05-15"}
     {:id 3 :date "2024-08-30"}
     {:id 4 :date "2024-11-20"}])

  ;; Test each function
  (evaluate-formula "=QUARTER(date)" test-dates)
  ;; Returns: [1 2 3 4]

  (evaluate-formula "=YEAR(date)" test-dates)
  ;; Returns: [2024 2024 2024 2024]

  (evaluate-formula "=MONTH(date)" test-dates)
  ;; Returns: [2 5 8 11]

  (evaluate-formula "=DAY(date)" test-dates)
  ;; Returns: [2 15 30 20]

  (evaluate-formula "=WEEK(date)" test-dates)
  ;; Returns: [5 20 35 47] (approximate week numbers)

  ;; You can also combine them
  (evaluate-formula "=CONCAT('Q', QUARTER(date), ' ', YEAR(date))" test-dates)
  ;; Returns: ["Q1 2024" "Q2 2024" "Q3 2024" "Q4 2024"]
  )


(comment
  (def test-data
    [{:id 1 :text "Hello World" :date "2024-02-02" :value 10}
     {:id 2 :text "Hello There" :date "2024-05-15" :value 20}
     {:id 3 :text "Goodbye World" :date "2024-08-30" :value 30}
     {:id 4 :text "Hello Again" :date "2024-11-20" :value 40}])

  ;; Test FIND
  (evaluate-formula "=FIND('World', text)" test-data)
  ;; Returns: [7 0 8 0]

  (evaluate-formula "=FIND('Hello', text, 1)" test-data)
  ;; Returns: [1 1 0 1]

  ;; Test ADD_MONTHS
  (evaluate-formula "=ADD_MONTHS(date, 3)" test-data)
  ;; Returns: ["2024-05-02" "2024-08-15" "2024-11-30" "2025-02-20"]

  ;; Test ADD_DAYS
  (evaluate-formula "=ADD_DAYS(date, 7)" test-data)
  ;; Returns: ["2024-02-09" "2024-05-22" "2024-09-06" "2024-11-27"]

  ;; Test VARIANCE and STDDEV
  (evaluate-formula "=VARIANCE(value[1:3])" test-data)
  ;; Returns the variance of the values

  (evaluate-formula "=STDDEV(value[1:3])" test-data)
  ;; Returns the standard deviation of the values

  ;; You can also combine them
  (evaluate-formula "=IF(FIND('Hello', text) > 0, ADD_DAYS(date, 1), date)" test-data)
  ;; Adds one day to dates where text contains "Hello"

  ;; Find dates with specific text and add days
  (evaluate-formula "=IF(FIND('Hello', text) > 0, ADD_DAYS(date, 7), date)" test-data)

;; Statistical analysis within groups
  (evaluate-formula "=STDDEV(FILTER(value, FIND('Hello', text) > 0))" test-data)

;; Date ranges
  (evaluate-formula "=DAYS_BETWEEN(date, ADD_MONTHS(date, 3))" test-data))



(comment
  ;; Test data with dates and numbers
  (def test-data-with-dates
    [{:id 1 :date "2024-01-15" :value 1234.567}
     {:id 2 :date "2024-02-20" :value 2345.678}
     {:id 3 :date "2024-03-25" :value 3456.789}])

  ;; Test date formatting
  (evaluate-formula "=TEXT(date, 'mm/dd/yyyy')" test-data-with-dates)
  ;; => [{:id 1, :date "2024-01-15", :value 1234.567, :result "01/15/2024"} ...]

  (evaluate-formula "=TEXT(date, 'mmmm d, yyyy')" test-data-with-dates)
  ;; => [{:id 1, :date "2024-01-15", :value 1234.567, :result "January 15, 2024"} ...]

  (evaluate-formula "=TEXT(date, 'ddd')" test-data-with-dates)
  ;; => [{:id 1, :date "2024-01-15", :value 1234.567, :result "Mon"} ...]

  ;; Test number formatting
  (evaluate-formula "=TEXT(value, '#,##0.00')" test-data-with-dates)
  ;; => [{:id 1, :date "2024-01-15", :value 1234.567, :result "1,234.57"} ...]

  (evaluate-formula "=TEXT(value, '$#,##0.00')" test-data-with-dates)
  ;; => [{:id 1, :date "2024-01-15", :value 1234.567, :result "$1,234.57"} ...]

  (evaluate-formula "=TEXT(value * 0.01, '0%')" test-data-with-dates)
  ;; => [{:id 1, :date "2024-01-15", :value 1234.567, :result "12%"} ...]

  (evaluate-formula "=TEXT(1234, '000000')" test-data-with-dates)
  ;; => [{:id 1, :date "2024-01-15", :value 1234.567, :result "001234"} ...]
  )

(comment
  (def test-data
    [{:id 1 :category "A" :value 105}
     {:id 2 :category "A" :value 20}
     {:id 3 :category "B" :value 340}
     {:id 4 :category "B" :value 40}])

(evaluate-formula "=value " test-data)
  ;; Test each part separately
  (evaluate-formula "=CURRENT(category)" test-data)
;; Should return the category for each row

  (evaluate-formula "=FILTER(value, category = CURRENT(category))" test-data)
;; Should return filtered values for each category

  (evaluate-formula "=VARIANCE(FILTER(value, category = CURRENT(category)))" test-data)
;; Should return different variances for each category


  ;; Calculate overall standard deviation
  (evaluate-formula "=STDDEV(value)" test-data)
  ;; Returns a single value for the entire column

  ;; Calculate variance per category using FILTER
  (evaluate-formula "=IF(category = CURRENT(category),
                        VARIANCE(FILTER(value, category = CURRENT(category))),
                        0)"
                    test-data)
  ;; Returns variance for each category

  ;; Combine with other aggregations
  (evaluate-formula "=STDDEV(value) / AVERAGE(value)" test-data)
  ;; Returns coefficient of variation

  (evaluate-formula "=VARIANCE(FILTER(value, category = CURRENT(category)))" test-data))


(comment
  (def test-data
    [{:id 1 :value 16}
     {:id 2 :value 25}
     {:id 3 :value 100}
     {:id 4 :value -4}])  ; negative number to test handling

  ;; Basic square root
  (evaluate-formula "=SQRT(value)" test-data)
  ;; Returns: [4.0 5.0 10.0 nil]

  ;; Combined with other functions
  (evaluate-formula "=SQRT(POWER(value, 2))" test-data)

  ;; In calculations
  (evaluate-formula "=SQRT(SUM(value))" test-data))



(comment
  (def test-data
    [{:id 1 :value 16 :angle 0}
     {:id 2 :value 25 :angle 1.5708}  ; π/2
     {:id 3 :value 100 :angle 3.14159}  ; π
     {:id 4 :value -4 :angle 4.71239}]) ; 3π/2

  ;; Power function
  (evaluate-formula "=POWER(value, 2)" test-data)
  ;; Returns: [256 625 10000 16]

  ;; Logarithms
  (evaluate-formula "=LOG(value)" test-data)
  (evaluate-formula "=LOG10(value)" test-data)

  ;; Trigonometric functions
  (evaluate-formula "=SIN(angle)" test-data)
  (evaluate-formula "=COS(angle)" test-data)

  ;; Inverse trig functions
  (evaluate-formula "=ASIN(0.5)" test-data)

  ;; Absolute value
  (evaluate-formula "=ABS(value)" test-data)

  ;; Exponential
  (evaluate-formula "=EXP(1)" test-data)  ; e^1

  ;; Combinations
  (evaluate-formula "=SQRT(POWER(value, 2) + POWER(2, 3))" test-data))


(comment
  (def test-data
    [{:id 1 :value 10 :category "A"}
     {:id 2 :value 20 :category "A"}
     {:id 3 :value 30 :category "B"}
     {:id 4 :value 40 :category "B"}])

  ;; Running sum
  (evaluate-formula "=RUNNING_SUM(value)" test-data)
  ;; Returns: [10 30 60 100]

  ;; LAG examples
  (evaluate-formula "=LAG(value)" test-data)
  ;; Returns: [nil 10 20 30]

  (evaluate-formula "=LAG(value, 1, 0)" test-data)
  ;; Returns: [0 10 20 30]

  (evaluate-formula "=LAG(value, 2)" test-data)
  ;; Returns: [nil nil 10 20]

  ;; LEAD examples
  (evaluate-formula "=LEAD(value)" test-data)
  ;; Returns: [20 30 40 nil]

  ;; Combining with other functions
  (evaluate-formula "=value - LAG(value, 1, 0)" test-data)
  ;; Returns: [10 10 10 10] (difference from previous value)

  ;; Running total within category
  (evaluate-formula "=RUNNING_SUM(FILTER(value, category = CURRENT(category)))" test-data)
  ;; Returns: [10 30 30 70]

  (evaluate-formula "=LEAD(value)" test-data)
  ;; Should return: [20 30 40 nil]

  (evaluate-formula "=LEAD(value, 1, 0)" test-data)
  ;; Should return: [20 30 40 0]

  (evaluate-formula "=LEAD(value, 2)" test-data)
  ;; Should return: [30 40 nil nil]

  (evaluate-formula "=LEAD(value, 2, 0)" test-data)
  ;; Should return: [30 40 0 0]
  )



(comment
  (def test-data
    [{:id 1 :value 10 :text "ABC" :mixed "123" :empty nil}
     {:id 2 :value 20 :text "DEF" :mixed "XYZ" :empty nil}
     {:id 3 :value 30 :text "GHI" :mixed "456" :empty nil}])

  ;; ISNUMBER tests
  (evaluate-formula "=ISNUMBER(value)" test-data)
  ;; Returns: [true true true]

  (evaluate-formula "=ISNUMBER(mixed)" test-data)
  ;; Returns: [true false true]

  ;; ISTEXT tests
  (evaluate-formula "=ISTEXT(text)" test-data)
  ;; Returns: [true true true]

  (evaluate-formula "=ISTEXT(mixed)" test-data)
  ;; Returns: [false true false]

  ;; ISNULL tests
  (evaluate-formula "=ISNULL(empty)" test-data)
  ;; Returns: [true true true]

  ;; TO_NUMBER tests
  (evaluate-formula "=TO_NUMBER(mixed)" test-data)
  ;; Returns: [123 nil 456]

  ;; TO_TEXT tests
  (evaluate-formula "=TO_TEXT(value)" test-data)
  ;; Returns: ["10" "20" "30"]

  ;; Combined usage
  (evaluate-formula "=IF(ISNUMBER(mixed), TO_NUMBER(mixed), 0)" test-data)
  ;; Returns: [123 0 456]

  (evaluate-formula "=IF(ISTEXT(mixed), CONCAT('Text: ', mixed), 'Not text')" test-data)
  ;; Returns: ["Not text" "Text: XYZ" "Not text"]
  )


(comment

  (def test-data
    [{:id 1 :sales 100 :category "A"}
     {:id 2 :sales 500 :category "B"}
     {:id 3 :sales 1500 :category "A"}
     {:id 4 :sales 2500 :category "B"}])

  (evaluate-formula "=CASE(id,
                            1, 'Low Risk',
                            2, 'Medium Risk',
                            3, 'High Risk',
                            'Unknown')"
                    test-data)

  ;; The lookup table could be defined inline in the formula
  (evaluate-formula "=VLOOKUP(sales,
                             {0, 'Tier 1';
                              1000, 'Tier 2';
                              2000, 'Tier 3'},
                             TRUE)"
                    test-data)
  ;; Should return: ["Tier 1" "Tier 1" "Tier 2" "Tier 3"]

  ;; Or using column ranges for dynamic tiers
  (evaluate-formula "=VLOOKUP(current_value,
                             tier_ranges[1:3],
                             tier_names[1:3],
                             TRUE)"
                    test-data)




  (def test-data
    [{:id 1 :sales 100 :category "A"}
     {:id 2 :sales 500 :category "B"}
     {:id 3 :sales 1000 :category "A"}
     {:id 4 :sales 2000 :category "B"}])

  (evaluate-formula "=VLOOKUP(sales, [0, 1000, 2000],
                     ['Tier 1', 'Tier 2', 'Tier 3'], TRUE())" test-data)

;; Test with exact matches on id
  (evaluate-formula "=CASE(id,
                        1, 'Low Risk',
                        2, 'Medium Risk',
                        3, 'High Risk',
                        'Unknown')"
                    test-data)


;; Test with category
  (evaluate-formula "=CASE(category,
                        'A', 'Premium',
                        'B', 'Standard',
                        'Basic')"
                    test-data)
;; Should return:
;; ["Premium" "Standard" "Premium" "Standard"]

  (evaluate-formula "=TRUE()" test-data)

;; Test with numeric ranges (using boolean expressions)
  (evaluate-formula "=CASE(TRUE(),
                        sales < 200, 'Tier 1',
                        sales < 1000, 'Tier 2',
                        sales < 2000, 'Tier 3',
                        'Tier 4')"
                    test-data)

  (evaluate-formula "=CASE(TRUE(), sales < 200, 'Small', 'Large')" test-data)

  (evaluate-formula "=CASE(FALSE(),
                        sales < 200, 'Tier 1',
                        sales < 1000, 'Tier 2',
                        sales < 2000, 'Tier 3',
                        'Tier 4')"
                    test-data)

  )
