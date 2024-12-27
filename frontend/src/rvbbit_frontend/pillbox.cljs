(ns rvbbit-frontend.pillbox
  (:require [clojure.string          :as cstr]
            [clojure.edn             :as edn]
            [rvbbit-frontend.utility :as ut]
            [rvbbit-frontend.db :as db]
            [re-com.core             :as re-com :refer [at]]
            [re-com.util             :refer [px]]
            [re-frame.core           :as re-frame]
            [day8.re-frame.undo      :as undo :refer [undoable]]
            [rvbbit-frontend.connections :as conn]
            [clojure.walk            :as walk]
            ["react-drag-and-drop"   :as rdnd]
            [reagent.core            :as reagent]))

;; _  _ ____ _  _ ____ _   _    ____ ____ _       ___ ____    ____ _  _ ___  ____ ____    _  _ ____ ___     _  _ ____ _  _ ____ _   _    ____ ____ _       ____ _  _ ___     ___  ____ ____ _  _
;; |__| |  | |\ | |___  \_/  __ [__  |  | |        |  |  |    [__  |  | |__] |___ |__/ __ |\/| |__| |__] __ |__| |  | |\ | |___  \_/  __ [__  |  | |       |__| |\ | |  \    |__] |__| |    |_/
;; |  | |__| | \| |___   |      ___] |_\| |___     |  |__|    ___] |__| |    |___ |  \    |  | |  | |       |  | |__| | \| |___   |      ___] |_\| |___    |  | | \| |__/    |__] |  | |___ | \_


(defn honey-sql->honeycomb [hsql-map & [metadata]]
  (try
    (let [group-bys (set (:group-by hsql-map))
          select-items (get hsql-map :select)
          categorize-field (fn [item]
                             (if (or (contains? group-bys item)
                                     (and (vector? item) (contains? group-bys (first item))))
                               {:type :dimension
                                :field (if (vector? item) (first item) item)
                                :alias (when (vector? item) (second item))
                                :metadata (get-in metadata [:fields (if (vector? item) (first item) item)])}
                               {:type :measure
                                :field (if (vector? item)
                                         (if (vector? (first item))
                                           (first item)  ; For cases like [[:count 1] :alias]
                                           item)         ; For cases like [:sum :AMOUNT]
                                         item)
                                :alias (when (vector? item) (last item))
                                :metadata (get-in metadata [:fields (if (vector? item) (first item) item)])}))
          categorized-fields (mapv categorize-field select-items)
          dimensions (filterv #(= (:type %) :dimension) categorized-fields)
          measures (filterv #(= (:type %) :measure) categorized-fields)
          field-order (mapv (fn [item]
                              (if (vector? item)
                                (if (= (count item) 2)
                                  (second item)  ; Use alias if present
                                  (first item))  ; Use field if no alias
                                item))
                            select-items)
          filters (letfn [(parse-filter [f]
                            (if (vector? f)
                              (let [[op & args] f]
                                {:op op :args (mapv parse-filter args)})
                              f))]
                    (parse-filter (get hsql-map :where)))
          order-by-vec (vec (for [e (get hsql-map :order-by)]
                              (if (keyword? e) [e :asc] e)))
          order-bys (mapv (fn [[field direction]]
                            {:field field :direction direction
                             :metadata (get-in metadata [:fields field])})
                          order-by-vec)
          table-names (let [from-clause (:from hsql-map)]
                        (cond
                          (and (vector? from-clause) (= (count from-clause) 1)) ; Single-wrapped case
                          (let [table (first from-clause)]
                            (if (vector? table)
                              (mapv (fn [[name alias]]
                                      {:name name :alias alias
                                       :metadata (get metadata name)})
                                    [table])
                              [{:name table :alias nil
                                :metadata (get metadata table)}]))

                          (vector? from-clause) ; Double-wrapped case or with alias
                          (mapv (fn [table]
                                  (if (vector? table)
                                    {:name (first table)
                                     :alias (second table)
                                     :metadata (get metadata (first table))}
                                    {:name table
                                     :alias nil
                                     :metadata (get metadata table)}))
                                from-clause)

                          :else ; Fallback for any other case
                          [{:name from-clause :alias nil
                            :metadata (get metadata from-clause)}]))
          joins (mapv (fn [[join-type table alias & on]]
                        {:type join-type :table table :alias alias :on on
                         :metadata (get metadata table)})
                      (get hsql-map :join))]
      {:dimensions dimensions
       :measures measures
       :field-order field-order
       :filters filters
       :order-by order-bys
       :table-names table-names
       :joins joins})
    (catch :default e (do (ut/tapp>> [:honey-sql->honeycomb-error! (str e)]) {}))))


(defn honeycomb->honey-sql [{:keys [dimensions measures field-order filters order-by table-names joins]}]
  (let [select (mapv (fn [field-id]
                       (let [field (first (filter #(or (= (:alias %) field-id)
                                                       (= (:field %) field-id))
                                                  (concat dimensions measures)))]
                         (if field
                           (let [{:keys [field alias]} field]
                             (cond
                               (= field :*) :*
                               (and (vector? field) (= field [:count :*])) [[:count :*] alias]
                               (and (vector? field) alias) [field alias]
                               (vector? field) field
                               alias [field alias]
                               :else field))
                           field-id)))
                     field-order)
        group-by (mapv :field dimensions)
        from (if (= (count table-names) 1)
               (let [{:keys [name alias]} (first table-names)]
                 (if alias
                   [[name alias]]
                   [name]))
               (mapv (fn [{:keys [name alias]}]
                       (if alias
                         [name alias]
                         name))
                     table-names))
        join (mapv (fn [{:keys [type table alias on]}]
                     (vec (concat [type table alias] on)))
                   joins)
        where (letfn [(build-filter [f]
                        (if (map? f)
                          (cons (:op f) (mapv build-filter (:args f)))
                          f))]
                (build-filter filters))
        hsql-map (cond-> {:from from}
                   (seq join) (assoc :join join)
                   (seq group-by) (assoc :group-by group-by)
                   (seq select) (assoc :select select)
                   (seq filters) (assoc :where where)
                   (seq order-by) (assoc :order-by (mapv (fn [{:keys [field direction]}]
                                                           [field direction])
                                                         order-by)))]
    (ut/lists-to-vectors hsql-map)))

(defn get-main-table [hsql-map]
  (let [from (get hsql-map :from)]
    (when (seq from)
      ;(-> from first first)
      (first (flatten from))
      )))

(defn filter-fields [f]
  (when (vector? f)
    (let [[_ & args] f]
      (mapv #(if (keyword? %) % (filter-fields %)) args))))

(defn get-field-names [hsql-map]
  (let [select-fields (get hsql-map :select)
        group-bys (get hsql-map :group-by)
        filters (get hsql-map :where)
        order-bys (get hsql-map :order-by)
        extract-fields (fn [fields]
                         (mapv (fn [item]
                                 (if (vector? item)
                                   (if (keyword? (first item))
                                     (second item)
                                     (first item))
                                   item))
                               fields))]
    (vec (distinct (concat (extract-fields select-fields)
                           (extract-fields group-bys)
                           (flatten (filter-fields filters))
                           (if (vector? (first order-bys)) ;; if properly used
                             (extract-fields (mapv first order-bys))
                             (extract-fields (vec order-bys))) ;; other wise if used w/o direction
                           )))))

;; _  _ ____ _    ___  ____ ____    _  _ ___ _ _       ____ _  _ ____          _  _ ____ _  _ ____ _   _ ____ ____ _  _ ___     _  _ _  _ ___ ____ ___ ____ ____
;; |__| |___ |    |__] |___ |__/    |  |  |  | |       |___ |\ | [__     __    |__| |  | |\ | |___  \_/  |    |  | |\/| |__]    |\/| |  |  |  |__|  |  |___ [__
;; |  | |___ |___ |    |___ |  \    |__|  |  | |___    |    | \| ___]          |  | |__| | \| |___   |   |___ |__| |  | |__]    |  | |__|  |  |  |  |  |___ ___]

(defn reorder-honey-sql-map [hsql-map]
  (let [ordered-keys [:with :select :from :join :where :group-by :having :order-by :limit :offset]
        ordered-map (reduce (fn [acc key]
                              (if (contains? hsql-map key)
                                (assoc acc key (get hsql-map key))
                                acc))
                            {}
                            ordered-keys)]
    ordered-map))

(defn add-dimension
  "Adds a dimension to the query map if it doesn't already exist.
   Updates both dimensions and field-order to ensure it appears in group-by and select."
  [query-map dimension-name & {:keys [alias metadata]}]
  (if (some #(= (:field %) dimension-name) (:dimensions query-map))
    query-map  ; Return unchanged if dimension already exists
    (-> query-map
        (update :dimensions conj
                {:type :dimension
                 :field dimension-name
                 :alias alias
                 :metadata metadata})
        (update :field-order #(vec (distinct (conj % (or alias dimension-name))))))))

(defn remove-dimension
  "Removes a dimension from the query map.
   Removes it from dimensions, field-order, and order-by to ensure complete removal."
  [query-map dimension-name]
  (let [dimension (first (filter #(= (:field %) dimension-name) (:dimensions query-map)))
        alias (:alias dimension)]
    (-> query-map
        (update :dimensions #(filterv (fn [dim] (not= (:field dim) dimension-name)) %))
        (update :field-order #(filterv (fn [field]
                                         (and (not= field dimension-name)
                                              (not= field alias)))
                                       %))
        (update :order-by #(filterv (fn [order]
                                      (not (or (= (:field order) dimension-name)
                                               (= (:field order) alias))))
                                    %)))))

(defn remove-measure
  "Removes a measure from the query map.
   identifier can be either:
   - a string or keyword representing the alias of the measure
   - a vector representing the measure logic (e.g., [:sum :amount])
   - a keyword representing a simple measure field
   Removes it from measures, field-order, and order-by to ensure complete removal."
  [query-map identifier]
  (let [measure (first (filter #(or (= (:alias %) identifier)
                                    (= (:field %) identifier))
                               (:measures query-map)))
        alias (:alias measure)
        field (:field measure)]
    (-> query-map
        (update :measures #(remove (fn [m]
                                     (or (= (:alias m) identifier)
                                         (= (:field m) identifier)))
                                   %))
        (update :field-order #(filterv (fn [f]
                                         (and (not= f identifier)
                                              (not= f alias)
                                              (not= f field)))
                                       %))
        (update :order-by #(filterv (fn [order]
                                      (not (or (= (:field order) identifier)
                                               (= (:field order) alias)
                                               (= (:field order) field))))
                                    %)))))

;; used as (add-measure :AMOUNT :agg :sum :alias :sum_amounts) - didn't work, or at least didn't make it through to the honey-sql output
(defn add-measure
  "Adds a measure to the query map.
   If agg is provided, it creates an aggregate measure.
   agg should be one of :sum, :avg, :count, :min, :max"
  [query-map field & {:keys [agg alias metadata]}]
  (let [measure-field (if agg [agg field] field)
        field-identifier (or alias field)]
    (-> query-map
        (update :measures conj
                {:type :measure
                 :field measure-field
                 :alias alias
                 :metadata metadata})
        (update :field-order conj field-identifier))))

(defn add-filter
  "Adds a filter to the query map."
  [query-map filter-expr]
  (update query-map :filters
          (fn [filters]
            (if filters
              {:op :and :args [filters filter-expr]}
              filter-expr))))

(defn remove-filter
  "Removes a filter from the query map.
   filter-to-remove can be:
   - a keyword representing the field name (e.g., :DISTRICT)
   - a vector representing the entire filter clause (e.g., [:= :DISTRICT \"B2\"])
   - a vector representing the field and value (e.g., [:DISTRICT \"B2\"])
   Returns the updated query map with the filter removed."
  [query-map filter-to-remove]
  (letfn [(match-filter? [filter-expr]
            (cond
              (keyword? filter-to-remove)
              (= (first (:args filter-expr)) filter-to-remove)

              (and (vector? filter-to-remove) (= (count filter-to-remove) 3))
              (and (= (:op filter-expr) (first filter-to-remove))
                   (= (first (:args filter-expr)) (second filter-to-remove))
                   (= (second (:args filter-expr)) (last filter-to-remove)))

              (and (vector? filter-to-remove) (= (count filter-to-remove) 2))
              (and (= (first (:args filter-expr)) (first filter-to-remove))
                   (= (second (:args filter-expr)) (second filter-to-remove)))

              :else false))

          (remove-matching-filter [filter-expr]
            (if (and (map? filter-expr) (= (:op filter-expr) :and))
              (let [new-args (remove match-filter? (:args filter-expr))]
                (cond
                  (empty? new-args) nil
                  (= (count new-args) 1) (first new-args)
                  :else (assoc filter-expr :args new-args)))
              (when-not (match-filter? filter-expr) filter-expr)))]

    (update query-map :filters remove-matching-filter)))

(defn add-order-by
  "Adds an order-by clause to the query map if it doesn't already exist. Defaults to ascending order."
  [query-map field & {:keys [direction metadata] :or {direction :asc}}]
  (if (some #(= (:field %) field) (:order-by query-map))
    query-map  ; Return unchanged if order-by already exists
    (update query-map :order-by conj
            {:field field
             :direction direction
             :metadata metadata})))

;; New toggle-order-by-direction function
(defn toggle-order-by-direction
  "Toggles the direction of an order-by clause in the query map."
  [query-map field]
  (update query-map :order-by
          (fn [order-bys]
            (map (fn [order-by]
                   (if (= (:field order-by) field)
                     (update order-by :direction #(if (= % :asc) :desc :asc))
                     order-by))
                 order-bys))))

;; New change-agg-type function
(defn change-agg-type
  "Changes the aggregate type of a measure in the query map.
   identifier can be either:
   - a string or keyword representing the alias of the measure
   - a vector representing the current aggregate (e.g., [:count 1])
   new-agg-type should be one of :sum, :count, :avg, :min, :max"
  [query-map identifier new-agg-type]
  (update query-map :measures
          (fn [measures]
            (map (fn [measure]
                   (if (or (= (:alias measure) identifier)
                           (= (:field measure) identifier))
                     (update measure :field
                             (fn [field]
                               (if (vector? field)
                                 (assoc field 0 new-agg-type)
                                 [new-agg-type field])))
                     measure))
                 measures))))

(def agg-cycle [:sum :avg :min :max :count])

(defn cycle-agg-type
  "Cycles the aggregate type of a measure in the query map to the next type in the cycle.
   identifier can be either:
   - a string or keyword representing the alias of the measure
   - a vector representing the current aggregate (e.g., [:count 1])
   The cycle order is: sum -> avg -> min -> max -> count -> sum"
  [query-map identifier]
  (update query-map :measures
          (fn [measures]
            (map (fn [measure]
                   (if (or (= (:alias measure) identifier)
                           (= (:field measure) identifier))
                     (update measure :field
                             (fn [field]
                               (let [current-agg (if (vector? field) (first field) :count)
                                     current-index (.indexOf agg-cycle current-agg)
                                     next-index (mod (inc current-index) (count agg-cycle))
                                     next-agg (nth agg-cycle next-index)]
                                 (if (vector? field)
                                   (assoc field 0 next-agg)
                                   [next-agg field]))))
                     measure))
                 measures))))

(defn toggle-star-count
  "Toggles between :* and [:count :*] in the query map.
   When changing to [:count :*], it adds an alias.
   When changing to :*, it removes the alias and ensures it's the only field.
   query-map: The query map to modify
   Returns the updated query map."
  [query-map]
  (let [measures (:measures query-map)
        current-field (:field (first measures))]
    (if (= current-field :*)
      ;; Change from :* to [:count :*]
      (-> query-map
          (assoc :measures [{:type :measure, :field [:count :*], :alias :count_star, :metadata nil}])
          (assoc :field-order [:count_star]))
      ;; Change from [:count :*] (or anything else) to :*
      (-> query-map
          (assoc :measures [{:type :measure, :field :*, :alias nil, :metadata nil}])
          (assoc :field-order [:*])))))

(defn reorder-field
  "Reorders a field in the query map's field-order.
   existing-index: The current index of the field to move.
   put-in-front-of-this-index: The index of the field to put it in front of.
   If put-in-front-of-this-index is greater than the length of field-order,
   the field will be moved to the end."
  [query-map existing-index put-in-front-of-this-index]
  (let [field-order (:field-order query-map)
        max-index (count field-order)
        existing-index (mod existing-index max-index)
        put-in-front-of-this-index (min put-in-front-of-this-index max-index)
        field-to-move (nth field-order existing-index)
        new-field-order (vec
                         (concat
                          (subvec field-order 0 existing-index)
                          (subvec field-order (inc existing-index))))
        insert-index (if (< existing-index put-in-front-of-this-index)
                       (dec put-in-front-of-this-index)
                       put-in-front-of-this-index)
        final-field-order (vec
                           (concat
                            (subvec new-field-order 0 insert-index)
                            [field-to-move]
                            (subvec new-field-order insert-index)))]
    (assoc query-map :field-order final-field-order)))

;; Example usage:
;; (reorder-field query-map 1 3)  ; Move the second field (index 1) to be in front of the fourth field (index 3)

(defn remove-order-by
  "Removes an order-by clause from the query map."
  [query-map field]
  (update query-map :order-by
          (fn [order-bys] (filterv #(not= (:field %) field) order-bys))))

;; (def sample-query
;;   {:select   [:DISTRICT [[:count 1] :rowcnt]
;;               :UCR_PART :OFFENSE_CODE]
;;    ;:from     [[:offenses :oo349]]
;;    ;:from     [:offenses]
;;    :from     [[:offenses]]
;;    :group-by [:DISTRICT :UCR_PART :OFFENSE_CODE]
;;    ;:where [:and [:= :DISTRICT "A2"] [:= 1 1]]
;;    :order-by [[:rowcnt :desc]]})

;; (def sample-query
;;   {:select   [:DISTRICT :UCR_PART :OFFENSE_CODE]
;;    :from     [[:offenses :oo349]]
;;    })

;; (def sample-query
;;   {:from [[:table1 :t1]],
;;    :group-by [:column-a],
;;    :select [[[:sum :column-a] :sum_a] [[:count :column-b] :count_b] :column-a],
;;    :where [:and [:= :column-a 10] [:< :column-b 20]],
;;    :order-by [[:column-a :asc]]})

;; (def sample-metadata
;;   {:fields
;;    {:column-a {:min 10 :max 100 :data-type "integer"}
;;     :column-b {:min 5 :max 50 :data-type "integer"}}})

;; (def parsed-query (honey-sql->honeycomb sample-query {}))
;; (def back-honeycomb->honey-sql (honeycomb->honey-sql parsed-query))
;; (ut/tapp>> [:sample-query (str (reorder-honey-sql-map sample-query))])
;; (ut/tapp>> [:parsed-query (str parsed-query)])
;; (ut/tapp>> [:re-parsed-query (str (reorder-honey-sql-map back-honeycomb->honey-sql))])
;; (ut/tapp>> [:add-grp-parsed-out-agg
;;             (str (add-measure parsed-query :AMOUNT :agg :sum :alias :sum_amounts))
;;             (str (reorder-honey-sql-map (honeycomb->honey-sql (add-measure parsed-query :AMOUNT :agg :sum :alias :sum_amounts))))])

;; (ut/tapp>> [:is-equal? (= sample-query back-honeycomb->honey-sql)])

;; (ut/tapp>> [:add-grp-parsed  (str (add-dimension parsed-query :bamboo))])
;; (ut/tapp>> [:add-grp-parsed-out2  (str (honeycomb->honey-sql (add-dimension parsed-query :bamboo)))])
;; (ut/tapp>> [:add-grp-parsed-out  (str (reorder-honey-sql-map (honeycomb->honey-sql (add-dimension parsed-query :bamboo))))])

;; (ut/tapp>> [:add-grp-parsed-out222  (str (honeycomb->honey-sql (remove-dimension parsed-query :DISTRICT)))])

;; (ut/tapp>> [:add-grp-parsed-out-agg  (str (honeycomb->honey-sql (add-measure parsed-query :AMOUNT :agg :sum :alias :sum_amounts)))])

;; (ut/tapp>> [:add-grp-parsed-out-agg2  (str (reorder-honey-sql-map
;;                                             (honeycomb->honey-sql (-> parsed-query
;;                                                                       (add-measure :AMOUNT :agg :sum :alias :sum_amounts)
;;                                                                       ;(remove-measure :sum_amounts)
;;                                                                       (add-dimension :bamboo2)
;;                                                                       (remove-order-by :rowcnt)
;;                                                                       (remove-filter :DISTRICT)
;;                                                                       (remove-dimension :DISTRICT)
;;                                                                       (add-order-by :bamboo2 :direction :desc)
;;                                                                       (add-order-by :bamboo2 :asc)
;;                                                                       (add-dimension :bamboo2)
;;                                                                       (change-agg-type :sum_a :avg)
;;                                                                       (toggle-order-by-direction :column-a)
;;                                                                       (add-filter [:= "farts"  "farts"])
;;                                                                       (add-filter [:= "2farts"  "2farts"])
;;                                                                       ))))])


;; (ut/tapp>> [:hcomb-test (str (honey-sql->honeycomb {:select   [:DISTRICT [[:count 1] :rowcnt]
;;                                                   :UCR_PART :OFFENSE_CODE]
;;                                        :from     [[:offenses :oo349]]
;;                                        :group-by [:DISTRICT :UCR_PART :OFFENSE_CODE]
;;                                        :order-by [[:rowcnt :desc]]} {}))])


;; _  _ ____ _  _ ____ _   _ ____ ____ _  _ ___     ____ _  _ ___  ____    ____ _  _ ___     ____ _  _ ____ _  _ ___ ____
;; |__| |  | |\ | |___  \_/  |    |  | |\/| |__]    [__  |  | |__] [__     |__| |\ | |  \    |___ |  | |___ |\ |  |  [__
;; |  | |__| | \| |___   |   |___ |__| |  | |__]    ___] |__| |__] ___]    |  | | \| |__/    |___  \/  |___ | \|  |  ___]

;;; subs and events - redundancy, but they are trivial, so whatever

(re-frame/reg-sub
 ::get-metadata
 (fn [db {:keys [table-key]}]
   (let [meta (get-in db [:meta table-key] {})
         ;post-meta (get-in db [:meta table-key])
         ]
     ;; (if post-meta (for [[k v] meta] {k (merge v (get meta k))})) ;; lets do this later
     meta)))

(re-frame/reg-sub
 ::get-query
 (fn [db {:keys [panel-key runner table-key]}]
   (get-in db [:panels panel-key runner table-key])))

(re-frame/reg-sub
 ::all-sql-calls
 (fn [db]
   (into {}
         (for [[_ v] (get db :panels)]
           (into {} (for [[kk vv] (get v :queries)]
                      (when (nil? (find vv :vselect))
                        {kk vv})))))))

(re-frame/reg-sub
 ::get-query-name-from-query
 (fn [_ {:keys [query]}]
   (let [query (dissoc query :where)
         all-sql-calls @(ut/tracked-sub ::all-sql-calls {})] ;; safe sub
     (first (for [[qk q] all-sql-calls
                  :when (= query (dissoc q :where))]
              qk)))))

;; (ut/pp [:get-key @(ut/tracked-sub ::get-query-name-from-query {:query {:select [:DAY_OF_WEEK :DISTRICT :HOUR
;;                                                                                 :INCIDENT_NUMBER :Lat :Location
;;                                                                                 :Long :MONTH :OCCURRED_ON_DATE
;;                                                                                 :OFFENSE_CODE :OFFENSE_CODE_GROUP
;;                                                                                 :OFFENSE_DESCRIPTION
;;                                                                                 :REPORTING_AREA :SHOOTING :STREET
;;                                                                                 :UCR_PART :YEAR]
;;                                                                        :from   [[:offenses :zz961]]}})])

(re-frame/reg-event-db
 ::drag-in
 (undoable)
 (fn [db [_ {:keys [panel-key runner view-key parsed dropped-to drag-body query]}]]
   (let [[field-name meta-map] drag-body
         data-type (get meta-map :data-type)
         extra-query-keys (dissoc query :select :group-by :order-by :where)
         common-val (ffirst (get meta-map :commons))
         pre-change-sql (reorder-honey-sql-map (honeycomb->honey-sql parsed))
         aggable? (or (= data-type "integer") (= data-type "decimal") (= data-type "float"))
         new-honey (case
                    dropped-to
                     :measures (reorder-honey-sql-map (honeycomb->honey-sql
                                                       (if aggable?
                                                         (add-measure parsed field-name :agg :sum
                                                                      :alias (keyword (cstr/replace (str "sum_" field-name) ":" "")))
                                                         (add-measure parsed field-name :agg :count
                                                                      :alias (keyword (cstr/replace (str "cnt_" field-name) ":" ""))))))
                     :dimensions (reorder-honey-sql-map (honeycomb->honey-sql (add-dimension parsed field-name)))
                     :filters (reorder-honey-sql-map (honeycomb->honey-sql (add-filter parsed [:= field-name common-val])))
                     :order-by (reorder-honey-sql-map (honeycomb->honey-sql (add-order-by parsed field-name))))
         new-honey (merge new-honey extra-query-keys)]
     (ut/tapp>> [:drop-in panel-key runner view-key parsed dropped-to data-type aggable? (str drag-body)])
     (ut/tapp>> [:drop-in-new-honey dropped-to (str new-honey) (str pre-change-sql)])
     (assoc-in db [:panels panel-key runner view-key] new-honey))))

(re-frame/reg-event-db
 ::drag-out
 (undoable)
 (fn [db [_ {:keys [panel-key runner view-key parsed dragged-from drag-body query]}]]
   (let [[field-name meta-map] drag-body
         data-type (get meta-map :data-type)
         extra-query-keys (dissoc query :select :group-by :order-by :where)
         pre-change-sql (reorder-honey-sql-map (honeycomb->honey-sql parsed))
         new-honey (case
                    dragged-from
                     :measures (reorder-honey-sql-map (honeycomb->honey-sql (remove-measure parsed field-name)))
                     :dimensions (reorder-honey-sql-map (honeycomb->honey-sql (remove-dimension parsed field-name)))
                     :filters (reorder-honey-sql-map (honeycomb->honey-sql (remove-filter parsed field-name)))
                     :order-by (reorder-honey-sql-map (honeycomb->honey-sql (remove-order-by parsed field-name))))
         new-honey (merge new-honey extra-query-keys)]
     (ut/tapp>> [:drop-out panel-key runner view-key parsed dragged-from data-type field-name (str drag-body)])
     (ut/tapp>> [:drop-out-new-honey dragged-from (str new-honey) (str pre-change-sql)])
     (assoc-in db [:panels panel-key runner view-key] new-honey))))

(re-frame/reg-event-db
 ::inline-change
 (undoable)
 (fn [db [_ {:keys [panel-key runner view-key parsed field-name query]}]]
   (let [;[field-name meta-map] drag-body
         ;data-type (get meta-map :data-type)
         extra-query-keys (dissoc query :select :group-by :order-by :where)
        ;;  pre-change-sql (reorder-honey-sql-map (honeycomb->honey-sql parsed))
         new-honey (if (or (= field-name :*) (cstr/includes? (str field-name) ":*"))
                     (reorder-honey-sql-map (honeycomb->honey-sql (toggle-star-count parsed  )))
                     (reorder-honey-sql-map (honeycomb->honey-sql (cycle-agg-type parsed field-name))))
         new-honey (merge new-honey extra-query-keys)]
     (ut/tapp>> [:change-inline panel-key runner view-key (str parsed) field-name])
     (assoc-in db [:panels panel-key runner view-key] new-honey))))

;; _  _ _    ____ _  _ ___     ____ ____    ____ ____ _  _    ____ ____ _  _ ___  ____ _  _ ____ _  _ ___ ____
;; |  | |    |__| |\ | |  \    |__/ |___ __ |    |  | |\/|    |    |  | |\/| |__] |  | |\ | |___ |\ |  |  [__
;; |__| |    |  | | \| |__/    |  \ |___    |___ |__| |  |    |___ |__| |  | |    |__| | \| |___ | \|  |  ___]

;;; atom party

(def dragging?         (reagent/atom false))
(def dragging-out?     (reagent/atom false))
(def dyn-dropper-hover (reagent/atom false))
(def dragging-body     (reagent/atom false))

;;; UI elements and supporting functions

(defn draggable
  [data type element & [out?]]
  [(reagent/adapt-react-class rdnd/Draggable)
   {:type          type
    :on-drag-end   #(do (reset! (if out? dragging-out? dragging?) false)
                        (reset! dyn-dropper-hover false)
                        (reset! dragging-body false))
    :on-drag-start #(do (reset! (if out? dragging-out? dragging?) true)
                        (reset! dragging-body data))
    :data          (pr-str data)}
   [re-com/box
    :child element
    :style {:cursor "grab"}]])

(defn droppable
  [types-vec drag-type panel-key runner view-key parsed query element]
  (if true ;; allow drops at all? breaker
    [(reagent/adapt-react-class rdnd/Droppable)
     {:types types-vec
      :on-drop
      #(let [incoming    (js->clj %)
             _ (ut/tapp>> [:tabro-pill-incoming incoming @dragging-body])
             data        (edn/read-string (last (last incoming)))
             dragged-from (get (last data) :dragged-from)
             type        (first (first incoming))]
         (ut/tapp>> [:dropped!])
         (if dragged-from
           (do (ut/tracked-dispatch [::drag-out {:panel-key panel-key :runner runner :view-key view-key :query query
                                                 :parsed parsed :dragged-from dragged-from :drag-body data}])
               (ut/tapp>> [:tabro-pill-dropped-out :from dragged-from (str type) :drag-meta-type drag-type data]))
           (do (ut/tracked-dispatch [::drag-in {:panel-key panel-key :runner runner :view-key view-key :query query
                                                :parsed parsed :dropped-to drag-type :drag-body data}])
               (ut/tapp>> [:tabro-pill-dropped-in (str type) :drag-meta-type drag-type data])))
         (reset! dragging? false)
         (reset! dragging-out? false))}
     ;[re-com/box :child element]
     element]
    element))

(defn droppable-stub
  [_ _ _ _ _ _ _ element]
  element)

(defn pill [field-data width & [used-fields]]
  (let [[f {:keys [min max data-type type distinct]}] field-data
        ;dcolor (get @(ut/tracked-sub ::conn/data-colors {}) data-type)
        dcolor (or (get @(ut/tracked-sub ::conn/data-colors {}) data-type)  "#ffffff")
        used? (when used-fields (some #(= % f) used-fields))]
    [re-com/v-box
     :padding "4px"
     :size "none"
     :width (px width)
     :height "40px"
     ;:size "auto"
     :style {:color dcolor
             :font-weight 700
             :margin-bottom "20px"
             :overflow "hidden"
             :opacity (when used? 0.4)
             :background-color (str dcolor "10")
            ;;  :border (str "2px solid " dcolor 99)
             :border (if data-type
                       (str "2px solid " dcolor 99)
                       (str "2px dashed " dcolor 99))}
     :children [[re-com/box
                 ;:height "40px"
                 :child (str f)
                 :style {:font-size "12px"
                         :padding-left "2px"}]
                [re-com/h-box
                 ;:justify :between
                 ;:height "10px"
                 :gap  "4px"
                 :children (if (= type :special)
                             [[re-com/label :label "special"]]
                             [[re-com/label :label (str data-type " - ")]
                              [re-com/label :label (ut/nf distinct)]])
                 :style {:padding-left "2px"
                         :padding-right "4px"
                         :font-size "9px"}]]]))

(defn pill-in [field-data metadata width panel-key runner view-key parsed query]
  (let [field-data (cond (keyword? field-data) ;; group-by
                         {:metadata (get-in metadata [:fields field-data])
                          :field field-data :alias nil}

                         (empty? field-data)
                         {:field :_plus}

                         :else field-data)
        data-type (get-in field-data [:metadata :data-type])
        field-name (get field-data :field)
        ttype (get field-data :type)
        alias (get field-data :alias)
        dir (get field-data :direction)
        dcolor (or (get @(ut/tracked-sub ::conn/data-colors {}) data-type)  "#ffffff")]
    (if (= field-name :_plus)
      [re-com/box
       :child [re-com/md-icon-button
               :md-icon-name "ri-function-add-fill"
                :style {:font-size "28px"
                        :color (str (conn/theme-pull :theme/editor-outer-rim-color nil)  33)
                        :padding "0px"
                        :margin-top "-6px"}]
       :justify :center :align :center
       :style {:border "2px dashed #ffffff12"}
       :padding "4px"
       :size "none"
       :width (px width)
       :height "40px"]
      [re-com/v-box
       :padding "4px"
       :size "none"
       :width (px width)
       :height "40px"
       :attr (when (= ttype :measure)
               {:on-context-menu
                #(ut/tracked-dispatch [::inline-change {:panel-key panel-key :runner runner
                                                        :view-key view-key :parsed parsed
                                                        :field-name field-name :query query}])})
      ;:size "auto"
       :style {:color dcolor
               :font-weight 700
               :overflow "hidden"
               :background-color (str dcolor "10")
               :border (if data-type
                         (str "2px solid " dcolor 99)
                         (str "2px dashed " dcolor 99))}
       :children [[re-com/h-box
                   :justify :between :align :center
                   :children [[re-com/box :child (str field-name)]
                              (when alias [re-com/box
                                           :style {:opacity 0.4}
                                           :child (str "as " alias)])
                              (when dir [re-com/box
                                           :style {:opacity 0.4}
                                           :child (str dir)])
                              ]
                   :style {:font-size "12px"
                           :padding-left "2px"}]
                  [re-com/h-box
                   :justify :between
                   :gap  "4px"
                   :children [[re-com/box :child (str data-type)]
                              [re-com/box :child (str ttype)]]
                   :style {:padding-left "2px"
                           :padding-right "4px"
                           :font-size "9px"}]]])))

(defn drop-target-rows [w w-per-group items]
  (let [items-per-row (Math/floor (/ w w-per-group))
        ;;single? (= items-per-row 1)
        rows (partition-all items-per-row items)]
    [re-com/v-box
     :children
     (for [row rows]
       [re-com/h-box
        :width (str w "px")
        :children
        (for [item row]
          [re-com/box
           :width (str w-per-group "px")
           :child item])])]))



(defn honeycomb-builder [panel-key runner view-key h w]
  (let [query @(ut/tracked-sub ::get-query {:panel-key panel-key :runner runner :table-key view-key})
        cache-key [query (hash @dragging-body)]]
    (if-let [cached nil] ;(get @db/honey-comb-cache cache-key)]
      cached
      (let [hc-body (let [main-table (get-main-table query)
                          main-table (cond
                                       (cstr/starts-with? (str main-table) ":query/")
                                       (keyword (cstr/replace (str main-table) ":query/" ""))

                                       (map? main-table)
                                       @(ut/tracked-sub ::get-query-name-from-query {:query main-table})

                                       :else main-table)
                          used-fields (get-field-names query)
                          metadata  @(ut/tracked-sub ::get-metadata {:table-key main-table})
                          ;;meta2     @(ut/tracked-subscribe [::conn/sql-metadata [main-table]])
                          _ (ut/tapp>> [:main-table main-table metadata])
                          parsed    (honey-sql->honeycomb query metadata)
                          react! [@dyn-dropper-hover @dragging? @dragging-out?]
                            ;w (- w 20)
                          pill-menu-w 175]
                      [re-com/h-box
                       :width (px w)
                       :height (px h)
                       :children
                       [[re-com/box
                         :height (px (- h 30))
                         :width (px (+ 20 pill-menu-w))
                         :size "none" ;;(if @dragging-out? "auto" "none")
                         :style {:overflow "auto"
                                 :padding-left "1px"}
                         :child
                         [(if @dragging-out? droppable droppable-stub)
                          [:tabro-pill] :_remove panel-key runner view-key parsed query
                          [re-com/v-box
                           :padding "2px"
                           :gap "2px"
                           :size "auto"
                           :width (px (+ 20 pill-menu-w))
                           :style {:background-color (when @dragging-out? (str (conn/theme-pull :theme/editor-outer-rim-color nil) "99"))}
                            ;; :attr (when @dragging-out? {:on-drag-enter #(reset! dyn-dropper-hover :_out)
                            ;;                             :on-drag-over  #(when (not @dyn-dropper-hover)
                            ;;                                               (reset! dyn-dropper-hover :_out))
                            ;;                             :on-drag-leave #(reset! dyn-dropper-hover false)})
                           :children (if @dragging-out?
                                       [[re-com/box
                                         :size "none"
                                         :height (px (- h 30))
                                         :width (px (+ 20 pill-menu-w))
                                         :align :center :justify :center
                                         :child (str "remove")
                                         :style {:color "#ffffff"
                                                 :border-radius "14px"
                                                 :font-size "22px"
                                                 :font-weight 700}]]

                                       (for [ff (merge (get metadata :fields)
                                                       {:* {:field :*  :type :special}
                                                          ;1  {:field 1   :type :special}
                                                        })
                                             :let [ww pill-menu-w]]
                                         [re-com/box
                                          :height "40px"
                                          :child [draggable ff :tabro-pill [pill ff ww used-fields]]]))]]]
                        [re-com/v-box
                         :gap "5px"
                         :size "none"
                         :width (px (- w (+ pill-menu-w 5) 10))
                         :height (px (- h 30))
                         :style {;:border "1px solid lime"
                                 :overflow "auto"}
                         :children (for [[k v] (dissoc parsed :field-order :joins :table-names)
                                         :let [v (if (= k :filters)
                                                   (if (or (= (get v :op) :and)
                                                           (= (get v :op) :or))
                                                     (vec (for [fff (get v :args)] {:field (into [(get fff :op)] (get fff :args))}))
                                                     (if (get v :op)
                                                       [{:field (into [(get v :op)] (get v :args))}] []))
                                                   v)
                                               v (if (nil? v) [] v)
                                                ;; _ (ut/tapp>> [k v])
                                               clause-label (if (= @dyn-dropper-hover k)
                                                              (str "add " (key @dragging-body)  " to " (cstr/replace (str k) ":" ""))
                                                              (cstr/replace (str k) ":" ""))]]
                                     [re-com/v-box
                                      :children [;;  [re-com/h-box
                                                  ;;   :style {:border "1px dashed #ffffff33"}
                                                  ;;   :min-height "40px"
                                                  ;;   ;:align :center
                                                  ;;   :width (px (- w 153))
                                                  ;;   :children (for [ff v]
                                                  ;;               [re-com/v-box
                                                  ;;                :children [[pill-in ff metadata 145]
                                                  ;;                           [re-com/box :child (str ff) :width "145px"]
                                                  ;;                           ]])]

                                                 [droppable [:tabro-pill] k panel-key runner view-key parsed query
                                                  [re-com/box
                                                   :attr (when @dragging? {:on-drag-enter #(reset! dyn-dropper-hover k)
                                                                           :on-drag-over  #(when (not @dyn-dropper-hover)
                                                                                             (reset! dyn-dropper-hover k))
                                                                           :on-drag-leave #(reset! dyn-dropper-hover false)})
                                                   :style {:border "3px dashed #ffffff23"
                                                           :background-color (str (conn/theme-pull :theme/editor-outer-rim-color nil)
                                                                                  (if (= @dyn-dropper-hover k) "99" "08"))}
                                                   :padding "7px"
                                                   :min-height "40px"
                                                   :width (px (- w (+ pill-menu-w 8) 20))
                                                   :child [drop-target-rows
                                                           (- w (+ pill-menu-w 8))
                                                           (+ 20 pill-menu-w) ;; padding per block
                                                           (vec (for [ff v
                                                                      :let [ff (if (vector? ff) {(first ff) (last ff)} ff)
                                                                              ;;_ (ut/tapp>> [:parsed ff (str parsed)])
]] ;; where clause issues?
                                                                  [re-com/box
                                                                   :height "40px"
                                                                   :child [draggable
                                                                           [(get ff :field) (assoc ff :dragged-from k)]
                                                                           :tabro-pill
                                                                           [pill-in (assoc ff :dragged-from k)
                                                                            metadata pill-menu-w
                                                                            panel-key runner view-key parsed query] true]]))]]]
                                                 [re-com/box
                                                  :padding "3px"
                                                  :align :end
                                                  :style {:margin-right "12px"
                                                          :font-size "16px"
                                                          :font-weight 700
                                                          :color "white"}
                                                  :child (if (= k :measures)
                                                           [re-com/v-box
                                                            :children [[re-com/box
                                                                        :align :end
                                                                        :child clause-label]
                                                                       [re-com/box
                                                                        :style {:font-size "12px" :font-weight 400 :opacity 0.7}
                                                                        :child "(+ non group-by fields)"]]]
                                                           clause-label)]
                                                 [re-com/gap :size "10px"]]])]]])]
        (swap! db/honey-comb-cache assoc cache-key hc-body)
        hc-body))))