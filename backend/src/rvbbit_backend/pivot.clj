(ns rvbbit-backend.pivot
  (:require [clojure.string :as cstr]
            [clojure.walk :as walk]))

(defn- keywordify
  [s]
  (let [formatted-value (cond (nil? s) "is_null"  ;; handle nil
                              (number? s) (str s) ;; handle numbers by
                                                  ;; converting them to strings
                              :else s)]  ;; handle all other cases
    (-> formatted-value
        (cstr/lower-case)
        (cstr/replace " " "_")
        (cstr/replace "-" "_")
        keyword)))

 ;;  [[:sum [:case [:= :season "Winter"] :rainfall_int :else 0]] :winter]

;;  [:sum
;;   [:as
;;    [:case [[:= :season "Winter"] :rainfall_int] [:else 0]]
;;    :winter]]

(defn- pivot-case
  [field value agg-fn agg-field]
  (if (= agg-fn :count)
    [:case
     [:= field value] 1
     :else 0]
    [:case
     [:= field value] agg-field
     :else 0]))

(defn- pivot-select
  [base-select pivot-config]
  (let [field (first (keys pivot-config))
        config-values (first (vals pivot-config))
        agg-info (first config-values)
        values (last config-values)
        just-count? (= agg-info :count)
        agg-params? (true? (vector? agg-info))
        agg-fn (if agg-params? (first agg-info) agg-info)
        agg-field (if agg-params? (second agg-info) 1)]
    (vec (concat base-select  ;; wrap with vec to convert to vector
                 (for [v values]
                   (let [field-name (keywordify v)]
                     [[(if just-count? :sum agg-fn)
                       (pivot-case field v agg-fn agg-field)]
                      field-name]))))))

(defn pivot-query
  [query]
  (let [{:keys [pivot-by select]} query
        pivot-select-fields (pivot-select select pivot-by)]
    (-> query
        (assoc :select pivot-select-fields)
        (dissoc :pivot-by))))

(defn deep-pivot
  [query]
  (letfn [(walk-query [q]
            (cond
              ;; If it's a map with :pivot-by, pivot it
              (and (map? q) (contains? q :pivot-by)) (pivot-query q)
              ;; If it's a map, recurse on all values
              (map? q) (into {} (map (fn [[k v]] [k (walk-query v)]) q))
              ;; If it's a vector, recurse on all elements
              (vector? q) (mapv walk-query q)
              ;; Otherwise, return it as is
              :else q))]
    (walk-query query)))

(defn find-select-maps
  [structure]
  (let [path (atom [])
        maps (atom [])]
    (walk/prewalk (fn [x]
                    (when (map? x)
                      (if (and (contains? x :select) (contains? x :pivot-by))
                        (do (swap! maps conj x) (swap! path conj @maps))
                        (when-let [s (seq x)] (reset! path (conj @path s)))))
                    x)
                  structure)
    ;; Now we sort the maps based on their depth, deepest first
    (vec (map first (sort-by (comp count second) > (zipmap @maps @path))))))

(defn pivot-each1
  [orig]
  (let [finders (find-select-maps orig)
        qa (atom {})]
    (reset! qa orig)
    (doseq [q finders]
      (reset! qa (walk/postwalk-replace {q (deep-pivot q)} @qa)))
    ;[(first finders) @qa]
    (if (empty? (find-select-maps @qa))
      @qa
      (pivot-each1 @qa) ;; go again
      )))

(defn pivot-each
  [orig iteration-limit]
  (let [finders (find-select-maps orig)
        qa (atom orig)
        ;prev-state (atom nil)
        ]
    (reset! qa orig)
    (doseq [q finders]
      (reset! qa (walk/postwalk-replace {q (deep-pivot q)} @qa)))
    (if (or (= iteration-limit 1) ;; check if the iteration limit is reached
            (empty? (find-select-maps @qa))
            ;(and @prev-state (= @prev-state @qa))
) ;; check if the state is unchanged
      @qa
      (do
        ;(reset! prev-state @qa) ;; update the previous state
        (pivot-each @qa (dec iteration-limit)))))) ;; decrement the counter and continue

;; Usage with a limit of 10 iterations
;;(pivot-each your-original-query 10)


;; Test Usage
(def query1
  {:select [:field1 :field2 :field3 [[:count 1] :full_rows]]
   :pivot-by {:season [[:sum :rainfall_int]
                       ["Winter" "Fall" "Summer" "Spring" nil 23]]}
   :group-by [:field1 :field2 :field3]
   :where [:= :field1 "Bigfoot"]
   :from [[:table1 :table-alias]]})

;; Another query example
(def query2
  {:select [:field1 :field2 :field3]
   :pivot-by {:season [[:count 1] ["Winter" "Fall" "Summer"
                                   "Spring"]]}
   :group-by [:field1 :field2 :field3]
   :where [:= :field1 "Bigfoot"]
   :from [[:table1 :table-alias]]})

(def nested-query1
  {:select [:summer [[:count 1] :cnt] :field1]
   :group-by [:field1]
   :from [[{:select [:field1 :field2 :field3]
            :pivot-by {:season [[:sum :rainfall_int] ["Winter" "Fall" "Summer"
                                                      "Spring" nil 23]]}
            :group-by [:field1 :field2 :field3]
            :where [:= :field1 "Bigfoot"]
            :from [[:table1 :table-alias]]}] :aliasedpivot]})

(def nested-query2
  {:select [:summer [[:count 1] :cnt] :field1]
   :group-by [:field1]
   :from [[{:select [:field1 :field2 :field3]
            :pivot-by {:season [[:sum :rainfall_int] ["Winter" "Fall" "Summer"
                                                      "Spring" nil 23]]}
            :group-by [:field1 :field2 :field3]
            :where [:= :field1 "Bigfoot"]
            :from [[{:select [:summer [[:count 1] :cnt] :field1]
                     :group-by [:field1]
                     :from [[{:select [:field1 :field2 :field3]
                              :pivot-by {:season [[:sum :rainfall_int] ["Winter"
                                                                        "Fall" "Summer"
                                                                        "Spring" nil 23]]}
                              :group-by [:field1 :field2 :field3]
                              :where [:= :field1 "Bigfoot"]
                              :from [[:table1 :table-alias]]}] :aliasedpivot]}
                    :table-alias]]}] :aliasedpivot]})

(def nested-query3
  {:select [:summer [[:count 1] :cnt] :field1]
   :group-by [:field1]
   :from [[{:select [:field1 :field2 :field3]
            :pivot-by {:season [[:sum :rainfall_int] ["Winter" "Fall" "Summer"
                                                      "Spring" nil 23]]}
            :group-by [:field1 :field2 :field3]
            :where [:= :field1 "Bigfoot"]
            :from
            [[{:select [:summer [[:count 1] :cnt] :field1]
               :group-by [:field1]
               :from
               [[{:select [:field1 :field2 :field3]
                  :pivot-by {:season [[:sum :rainfall_int] ["Winter"
                                                            "Fall" "Summer"
                                                            "Spring" nil 23]]}
                  :group-by [:field1 :field2 :field3]
                  :where [:= :field1 "Bigfoot"]
                  :from
                  [[{:select [:summer [[:count 1] :cnt] :field1]
                     :group-by [:field1]
                     :from
                     [[{:select [:field1 :field2 :field3]
                        :pivot-by {:season [[:sum :rainfall_int]
                                            ["Winter" "Fall" "Summer"
                                             "Spring" nil 23]]}
                        :group-by [:field1 :field2 :field3]
                        :where [:= :field1 "Bigfoot"]
                        :from
                        [[{:select [:summer [[:count 1] :cnt] :field1]
                           :group-by [:field1]
                           :from [[{:select [:field1 :field2 :field3]
                                    :pivot-by {:season
                                               [[:sum :rainfall_int]
                                                ["Winter"
                                                 "Fall" "Summer"
                                                 "Spring" nil 23]]}
                                    :group-by [:field1 :field2
                                               :field3]
                                    :where [:= :field1 "Bigfoot"]
                                    :from [[:table1 :table-alias]]}]
                                  :aliasedpivot]}
                          :table-alias]]}] :aliasedpivot]}
                    :table-alias]]}] :aliasedpivot]}
              :table-alias]]}] :aliasedpivot]})

;; {:q11a (pivot-each query1 10)
;;  :q2 (pivot-each query2 10)
;;  ;:q3n (pivot-each nested-query1 10)
;;  ;:q4n (pivot-each nested-query2 10)
;;  ;:q5n (pivot-each nested-query3 10)
;;  ;:qq (map :pivot-by (find-select-maps nested-query2))
;;  ;:q4n (deep-pivot (first (find-select-maps nested-query2)))
;;  }