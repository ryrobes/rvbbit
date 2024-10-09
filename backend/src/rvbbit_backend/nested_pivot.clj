(ns rvbbit-backend.nested-pivot
  (:require [clojure.pprint :refer [pprint]]
            [clojure.edn :as edn]
            [clojure.string :as cstr]
            [rvbbit-backend.db :as db]
            [rvbbit-backend.util :as ut]
            ;[rvbbit-backend.websockets :as wss]
            [clojure.walk :as walk]))

;; Sample data
(def sample-data
  [{:product "Laptop" :brand "TechX" :region "North" :country "USA" :year 2021 :quarter "Q1" :month "Jan" :amount 1000 :units 10 :rating 4.5}
   {:product "Laptop" :brand "TechX" :region "North" :country "USA" :year 2021 :quarter "Q1" :month "Feb" :amount 1100 :units 11 :rating 4.7}
   {:product "Laptop" :brand "TechX" :region "Northeast" :country "USA" :year 2021 :quarter "Q1" :month "Feb" :amount 1100 :units 11 :rating 4.7}
   {:product "Laptop" :brand "TechX" :region "Northwest" :country "USA" :year 2021 :quarter "Q1" :month "Feb" :amount 1100 :units 11 :rating 4.7}
   {:product "Laptop" :brand "TechY" :region "South" :country "Mexico" :year 2021 :quarter "Q2" :month "Apr" :amount 900 :units 8 :rating 4.2}
   {:product "Laptop" :brand "TechY" :region "South" :country "Mexico" :year 2021 :quarter "Q2" :month "May" :amount 950 :units 9 :rating 4.3}
   {:product "Phone"  :brand "TechX" :region "North" :country "Canada" :year 2022 :quarter "Q1" :month "Jan" :amount 500 :units 20 :rating 4.6}
   {:product "Phone"  :brand "TechX" :region "North" :country "Canada" :year 2022 :quarter "Q1" :month "Feb" :amount 550 :units 22 :rating 4.8}
   {:product "Phone"  :brand "TechY" :region "South" :country "Brazil" :year 2022 :quarter "Q2" :month "Apr" :amount 400 :units 15 :rating 4.1}
   {:product "Phone"  :brand "TechY" :region "South" :country "Brazil" :year 2022 :quarter "Q2" :month "May" :amount 450 :units 17 :rating 4.4}])

;; Aggregation functions
(defn safe-div [x y] (if (zero? y) 0 (/ x y)))

(def agg-fns
  {:sum (fn [acc x] (+ (or acc 0) x))
   :min (fn [acc x] (if acc (min acc x) x))
   :max (fn [acc x] (if acc (max acc x) x))
   :avg (fn [[sum count] x] [(+ (or sum 0) x) (inc (or count 0))])
   :count (fn [acc _] (inc (or acc 0)))})

(defn finalize-agg [agg-key result]
  (if (= agg-key :avg)
    (safe-div (first result) (second result))
    result))

;; Pivot table creation functions
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
        column-tree (if (vector? column-tree) column-tree [column-tree])

        cell-fn '(fn [{:keys [column-path row-path]}]
                   (let [value (get-in pivot-data [:rows row-path :cols column-path])]
                     [:div
                      {:style {:color "grey"
                               :font-size 10}}
                      (str (or value ""))]))]

     {:column-tree column-tree
      :reaction-clover (keyword (str "solver-status/metadata>" (cstr/replace (first rowset-keypath) ":" "")))
      :row-tree row-tree
      ;:cell cell-fn
      }))



;; (defn run-example []
;;   (let [row-keys [:product :brand :region :country]
;;         column-keys [:year :quarter :month]
;;         agg-specs [[:total    (:sum agg-fns)   0 :amount]
;;                    [:min-amt  (:min agg-fns)   nil :amount]
;;                    [:max-amt  (:max agg-fns)   nil :amount]
;;                    [:avg-amt  (:avg agg-fns)   nil :amount]
;;                    [:units    (:sum agg-fns)   0 :units]
;;                    [:avg-rating (:avg agg-fns) nil :rating]]
;;         nested-report (create-nested-report sample-data row-keys column-keys agg-specs)
;;         nested-grid-data (pivot-to-nested-grid nested-report row-keys column-keys)]

;;     (println "Nested Report Structure (first two rows):")
;;     (pprint (update nested-report :rows #(into {} (take 2 %))))

;;     (println "\nNested Grid Component:")
;;     (pprint nested-grid-data)

;;     (println "\nColumn Tree:")
;;     (pprint (:column-tree (apply hash-map (rest nested-grid-data))))

;;     (println "\nRow Tree:")
;;     (pprint (:row-tree (apply hash-map (rest nested-grid-data))))

;;     (println "\nSample Cell Data:")
;;     (let [sample-row-path (first (keys (:rows nested-report)))
;;           sample-col-path (first (keys (:cols (first (vals (:rows nested-report))))))]
;;       (pprint (get-in nested-report [:rows sample-row-path :cols sample-col-path])))))

;; Run the example
;; (run-example)

(defn get-grid-data [{:keys [input client-name connection-id ui-keypath solver-name]}]
  (let [{:keys [rowset-keypath row-keys column-keys agg-specs raw?]} input
        rowset (if (map? (first rowset-keypath))
                 rowset-keypath  ;; raw data injected 
                 (get-in (deref db/last-solvers-data-atom) rowset-keypath))
        ;; row-keys [:product :brand :region :country]
        ;; column-keys [:year :quarter :month]
        ;; agg-specs [[:total    (:sum agg-fns)   0 :amount]
        ;;            [:min-amt  (:min agg-fns)   nil :amount]
        ;;            [:max-amt  (:max agg-fns)   nil :amount]
        ;;            [:avg-amt  (:avg agg-fns)   nil :amount]
        ;;            [:units    (:sum agg-fns)   0 :units]
        ;;            [:low-stock (:count agg-fns) 0 #(< (:units %) 10)] ;; w custom field predicate? useful?
        ;;            [:avg-rating (:avg agg-fns) nil :rating]]
        agg-specs (vec
                   (for [[alias agg default field] agg-specs]
                     [alias (agg agg-fns) default field]))
        reaction-keypath (keyword (str "solver-status/metadata>" (cstr/replace (first rowset-keypath) ":" "")))
        nested-report (create-nested-report rowset row-keys column-keys agg-specs)
        nested-grid-data (when (not raw?)
                           (pivot-to-nested-grid nested-report row-keys column-keys rowset-keypath))]
    ;;(ut/pp [:pivot! ui-keypath rowset-keypath reaction-keypath input] {:width 60})
    ;; (wss/push-to-client (vec (rest ui-keypath)) [] client-name 1 :push-assocs {block-keypath input})
    (if raw?
      nested-report
      {:data nested-report
       :grid (merge 
              {:row-keys row-keys 
               :column-keys column-keys 
               :agg-specs (pr-str agg-specs)}
              nested-grid-data)})))

;; (ut/pp (get-grid-data))


(ut/pp (get-grid-data {:input {:rowset-keypath [:DISTRICT-drag-36]
                               :raw?  true
                               :row-keys [:DISTRICT]
                               :column-keys [:OFFENSE_CODE_GROUP :HOUR]
                               :agg-specs [[:incidents :sum 0 :INCIDENT_NUMBER_countd_166]
                                           [:incidents-max :max 0 :INCIDENT_NUMBER_countd_166]]}}))

(ut/pp (get-in @db/last-solvers-data-atom [:DISTRICT-drag-36]))
(ut/pp (keys @db/last-solvers-data-atom))
 

