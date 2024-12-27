(ns rvbbit-backend.nested-pivot
  (:require [clojure.pprint :refer [pprint]]
            [clojure.edn :as edn]
            [clojure.string :as cstr]
            [rvbbit-backend.db :as db]
            [rvbbit-backend.util :as ut]
            [clojure.walk :as walk]))

(def agg-fns
  {:sum (fn [acc x] (+ (or acc 0) x))
   :min (fn [acc x] (if acc (min acc x) x))
   :max (fn [acc x] (if acc (max acc x) x))
   :avg (fn [acc x]
          (if (vector? acc)
            [(+ (first acc) (or x 0)) (inc (second acc))]
            [(or x 0) 1]))
   :count (fn [acc _] (inc (or acc 0)))})

(defn safe-div [a b]
  (if (and (number? a) (number? b) (not (zero? b)))
    (/ a b)
    a))  ; Return a if division is not possible

(defn finalize-agg [agg-key result]
  (if (= agg-key :avg)
    (if (vector? result)
      (safe-div (first result) (second result))
      result)  ; If result is not a vector, it's already a single value
    result))


(defn nest-data-multi-agg [data row-keys column-keys agg-specs]
  (reduce (fn [acc record]
            (let [row-path (mapv #(get record %) row-keys)
                  col-path (mapv #(get record %) column-keys)]
              (reduce (fn [inner-acc [agg-key agg-fn init-val value-key]]
                        (update-in inner-acc [:rows row-path :cols col-path agg-key]
                                   #(agg-fn (or % init-val) (get record value-key))))
                      acc
                      agg-specs)))
          {:rows {} :cols {}}
          data))

(defn create-nested-report [data row-keys column-keys agg-specs]
  (let [nested-report (nest-data-multi-agg data row-keys column-keys agg-specs)]
    (walk/postwalk
     (fn [x]
       (if (and (map? x) (every? #(contains? x %) (map first agg-specs)))
         (into {} (map (fn [[k v]] [k (finalize-agg k v)]) x))
         x))
     nested-report)))

(defn nested-update [m [k & ks]]
                (if ks
                  (update m k (fnil nested-update {}) ks)
                  (update m k (fnil identity []))))

(defn create-tree [paths]
  (reduce (fn [tree path]
            (assoc-in tree path {}))
          {}
          paths))

(defn tree->nested-vector [tree]
  (if (map? tree)
    (let [sorted-children (sort-by first tree)]
      (if (= 1 (count sorted-children))
        (let [[k v] (first sorted-children)]
          (if (empty? v)
            k
            [k (tree->nested-vector v)]))
        (vec (for [[k v] sorted-children]
               (if (empty? v)
                 k
                 [k (tree->nested-vector v)])))))
    []))

(defn pivot-to-nested-grid [pivot-data row-keys column-keys rowset-keypath]
  (let [row-paths (keys (:rows pivot-data))
        rowset-keypath (if (vector? rowset-keypath) rowset-keypath ["raw"])
        col-paths (distinct (mapcat #(keys (:cols %)) (vals (:rows pivot-data))))
        row-tree (tree->nested-vector (create-tree row-paths))
        row-tree (if (vector? row-tree) row-tree [row-tree])
        column-tree (tree->nested-vector (create-tree col-paths))
        column-tree (if (vector? column-tree) column-tree [column-tree])]
    {:column-tree column-tree
     :reaction-clover (keyword (str "solver-status/metadata>" (cstr/replace (first rowset-keypath) ":" "")))
     :row-tree row-tree}))

(defn get-grid-data [{:keys [input client-name connection-id ui-keypath solver-name ]}]
  (try
    (let [{:keys [rowset-keypath row-keys column-keys agg-specs raw? query-data]} input
          rowset (if (and query-data
                          (map? (first query-data))
                          (ut/ne? (first query-data)))
                   query-data
                   (if (map? (first rowset-keypath))
                     rowset-keypath  ;; raw data injected
                     (get-in (deref db/last-solvers-data-atom) rowset-keypath)))
          agg-specs (vec
                     (for [[alias agg default field] agg-specs]
                       [alias (agg agg-fns) default field]))
          reaction-keypath (keyword (str "solver-status/metadata>" (cstr/replace (first rowset-keypath) ":" "")))
          nested-report (create-nested-report rowset row-keys column-keys agg-specs)
          nested-grid-data (when (not raw?)
                             (pivot-to-nested-grid nested-report row-keys column-keys rowset-keypath))]
    (ut/pp ["🏋" :pivot! ui-keypath rowset-keypath reaction-keypath input] {:width 60})
    ;; (wss/push-to-client (vec (rest ui-keypath)) [] client-name 1 :push-assocs {block-keypath input})
    (if raw?
      nested-report
      {:data nested-report
       :grid (merge
              {:row-keys row-keys
               :column-keys column-keys
               :agg-specs (pr-str agg-specs)}
              nested-grid-data)}))
              (catch Exception e
                (do (ut/pp ["🏋" :get-grid-data-error (str e)])
                    {:data []
                     :grid [:v-box :children [[:box :child (str "clj/get-grid-data error: ")]
                                              [:box :child (str  (str e))]]]}))))

