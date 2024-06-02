(ns rvbbit-frontend.select-transform
  (:require [clojure.string :as cstr]
            [clojure.set :as cset]
            [rvbbit-frontend.utility :as ut]
            [clojure.edn :as edn]))

(defn process-dim-spec
  [dim-spec]
  (if (vector? dim-spec) [(first dim-spec) (second dim-spec)] [dim-spec dim-spec]))

(defn calculate-aggregations
  [agg-specs vals]
  (reduce (fn [agg-result [agg-fn agg-key]]
            (let [[agg-op field] agg-fn
                  agg-value (case agg-op
                              :sum (reduce + (map #(get % field) vals))
                              :avg (/ (reduce + (map #(get % field) vals))
                                      (count vals))
                              :count (count vals))]
              (assoc agg-result (keyword agg-key) agg-value)))
          {}
          agg-specs))

(defn vals-by-pivot-key
  [vals pivot-key]
  (let [pivots (distinct (map #(get % pivot-key) vals))]
    (map (fn [pivot] (filter #(= pivot (get % pivot-key)) vals)) pivots)))

(defn merge-pivot-and-dims
  [dim-maps pivot-map all-pivot-keys]
  (let [pivot-key-vals (apply merge (vals pivot-map))]
    (merge dim-maps (zipmap (map name all-pivot-keys) (repeat {})) pivot-key-vals)))

(defn sanitize-key
  [key]
  (let [key (if (keyword? key) (name key) key)]  ; Convert to string if it's a keyword
    (-> key
        (clojure.string/replace " " "_")   ; Replace spaces with dashes
        (clojure.string/replace #"[^\w-]" "") ; Remove any characters that are not alphanumeric, underscore or dash
        )))

(def transform-cache (reagent.core/atom {}))

(defn transform-pre-deep-nest
  [dsl data]
  (if (get @transform-cache [(hash dsl) (hash data)])
    (do ;(ut/tapp>> [:transform-cache-for dsl (take 2 data)])
        (get @transform-cache [(hash dsl) (hash data)]))

    (let [{:keys [transform-select from fill-gaps? pivot-by order-nest-by nest-by]} dsl
          agg-specs (filter #(vector? (try (first %) (catch :default _ nil)))
                            transform-select)
          dim-specs (filter #(not (vector? (try (first %) (catch :default _ nil))))
                            transform-select)
          sf-name (fn [x] (try (name x) (catch :default _ (str x))))
          pivot-key (first pivot-by)
          nest-key (first nest-by)
          single-nest-string? (string? nest-key)
          all-pivot-keys (distinct (map #(get % pivot-key) data))
        ;single-agg? (= 1 (count agg-specs))
          group-fn (fn [row]
                     (mapv (fn [dim-spec]
                             (let [[dim-key _] (process-dim-spec dim-spec)]
                               (get row dim-key)))
                           dim-specs))
          output (if nest-key
                ;; Nesting Case
                   (let [grouped (group-by #(get % nest-key) data)]
                     (vec
                      (map (fn [[group vals]]
                             {:id group
                              :data (vec
                                     (map
                                      (fn [val]
                                        (let [dim-maps
                                              (apply merge
                                                     (map-indexed
                                                      (fn [idx dim-spec]
                                                        (let [[dim-key dim-alias] (process-dim-spec dim-spec)]
                                                          {(keyword dim-alias) (get val dim-key)}))
                                                      dim-specs))
                                              agg-maps (calculate-aggregations agg-specs [val])]
                                          (merge dim-maps agg-maps)))
                                      vals))})
                           grouped)))
                     ;; Pivoting Case
                   (let [grouped (group-by group-fn data)
                         single-agg? (= 1 (count agg-specs))]
                     (vec
                      (map (fn [[category vals]]
                             (let [pivot-vals (vals-by-pivot-key vals pivot-key)
                                   pivot-map
                                   (apply merge
                                          (map
                                           (fn [pivot-val-group]
                                             (let [color (get (first pivot-val-group) pivot-key)
                                                   color (if (nil? color) "IS_NULL" color)
                                                   agg-maps (calculate-aggregations agg-specs
                                                                                    pivot-val-group)]
                                               (into {}
                                                     (map (fn [[agg-key agg-val]]
                                                            {(keyword
                                                              (sanitize-key
                                                               (if single-agg?
                                                                 (sf-name color)
                                                                 (str (sf-name color) "_" agg-key))))
                                                             agg-val})
                                                          agg-maps))))
                                           pivot-vals))
                                   dim-maps (apply merge
                                                   (map-indexed
                                                    (fn [idx dim-spec]
                                                      (let [[_ dim-alias] (process-dim-spec dim-spec)]
                                                        {(keyword dim-alias) (nth category idx)}))
                                                    dim-specs))]
                               (merge
                                dim-maps
                                (zipmap
                                 (mapcat (fn [pivot]
                                           (let [pivot (if (nil? pivot) "IS_NULL" pivot)]
                                             (map (fn [agg]
                                                    (keyword
                                                     (sanitize-key
                                                      (if single-agg?
                                                        (sf-name pivot)
                                                        (str (sf-name pivot) "_" (second agg))))))
                                                  agg-specs)))
                                         all-pivot-keys)
                                 (repeat 0))
                                pivot-map)))
                           grouped))))
          output (cond single-nest-string? (assoc-in output [0 :id] nest-key)
                       nest-key (let [x-vals (vec (sort (distinct (apply concat (for [{:keys [data]} output] (distinct (map :x data)))))))]
                                  ;(ut/tapp>> [:xvals x-vals])
                                  (vec (for [{:keys [id data]} output]
                                         {:id id
                                          :data (let [this-x (vec (sort (distinct (map :x data))))
                                                      whole? (= this-x x-vals)
                                                      missing (vec (cset/difference (set x-vals) (set this-x)))
                                                      empties (vec (for [m missing] {:x m :y 0}))]
                                                  ;(when (not whole?) (ut/tapp>> [id :missing-keys this-x x-vals missing]))
                                                  (vec (sort-by (or order-nest-by :x)
                                                                (if (and fill-gaps? (not whole?))
                                                                  (into data empties) data))))})))
                       :else output)]
      (ut/tapp>> [:transform-NOT-cached-for dsl (take 2 data) output])
      (swap! transform-cache assoc [(hash dsl) (hash data)] output)
    ;(when nest-key (ut/tapp>> [:ts-output output]))
      output)))

(defn deep-nest
  [fields data agg-specs dim-specs]
  (if (empty? fields)
    (vec
     (map (fn [val]
            (let [dim-maps (apply merge
                                  (map-indexed
                                   (fn [idx dim-spec]
                                     (let [[dim-key dim-alias] (process-dim-spec dim-spec)]
                                       {(keyword dim-alias) (get val dim-key)}))
                                   dim-specs))
                  agg-maps (calculate-aggregations agg-specs [val])]
              (merge dim-maps agg-maps)))
          data))
    (let [field (first fields)
          grouped (group-by #(get % field) data)]
      (map (fn [[group vals]]
             {:name group
              :children (vec (deep-nest (rest fields) vals agg-specs dim-specs))})
           grouped))))

(defn transform
  [dsl data]
  (if (get @transform-cache [(hash dsl) (hash data)])
    (do ;(ut/tapp>> [:transform-cache-for dsl (take 2 data)])
        (get @transform-cache [(hash dsl) (hash data)]))

    (let [{:keys [transform-select from fill-gaps? pivot-by order-nest-by nest-by]} dsl
          agg-specs (filter #(vector? (try (first %) (catch :default _ nil)))
                            transform-select)
          dim-specs (filter #(not (vector? (try (first %) (catch :default _ nil))))
                            transform-select)
          sf-name (fn [x] (try (name x) (catch :default _ (str x))))
          pivot-key (first pivot-by)
          nest-key (first nest-by)
          single-nest-string? (string? nest-key)
          all-pivot-keys (distinct (map #(get % pivot-key) data))
          group-fn (fn [row]
                     (mapv (fn [dim-spec]
                             (let [[dim-key _] (process-dim-spec dim-spec)]
                               (get row dim-key)))
                           dim-specs))
          output (cond
                   (and nest-key (> (count nest-by) 1))
                   (let [top-level-nest-key (try (if (string? (first nest-key)) (first nest-key) (str "(all " (name (first nest-key)) ")"))
                                                 (catch :default _ "(all)"))
                         nest-keys (vec (remove string? nest-by))]
                     {:name top-level-nest-key
                      :children (vec (deep-nest nest-keys data agg-specs dim-specs))})
                   nest-key
                  ;; Nesting Case
                   (let [grouped (group-by #(get % nest-key) data)]
                     (vec
                      (map (fn [[group vals]]
                             {:id group
                              :data (vec
                                     (map
                                      (fn [val]
                                        (let [dim-maps
                                              (apply merge
                                                     (map-indexed
                                                      (fn [idx dim-spec]
                                                        (let [[dim-key dim-alias] (process-dim-spec dim-spec)]
                                                          {(keyword dim-alias) (get val dim-key)}))
                                                      dim-specs))
                                              agg-maps (calculate-aggregations agg-specs [val])]
                                          (merge dim-maps agg-maps)))
                                      vals))})
                           grouped)))
                   :else
                  ;; Pivoting Case
                   (let [grouped (group-by group-fn data)
                         single-agg? (= 1 (count agg-specs))]
                     (vec
                      (map (fn [[category vals]]
                             (let [pivot-vals (vals-by-pivot-key vals pivot-key)
                                   pivot-map
                                   (apply merge
                                          (map
                                           (fn [pivot-val-group]
                                             (let [color (get (first pivot-val-group) pivot-key)
                                                   color (if (nil? color) "IS_NULL" color)
                                                   agg-maps (calculate-aggregations agg-specs
                                                                                    pivot-val-group)]
                                               (into {}
                                                     (map (fn [[agg-key agg-val]]
                                                            {(keyword
                                                              (sanitize-key
                                                               (if single-agg?
                                                                 (sf-name color)
                                                                 (str (sf-name color) "_" agg-key))))
                                                             agg-val})
                                                          agg-maps))))
                                           pivot-vals))
                                   dim-maps (apply merge
                                                   (map-indexed
                                                    (fn [idx dim-spec]
                                                      (let [[_ dim-alias] (process-dim-spec dim-spec)]
                                                        {(keyword dim-alias) (nth category idx)}))
                                                    dim-specs))]
                               (merge
                                dim-maps
                                (zipmap
                                 (mapcat (fn [pivot]
                                           (let [pivot (if (nil? pivot) "IS_NULL" pivot)]
                                             (map (fn [agg]
                                                    (keyword
                                                     (sanitize-key
                                                      (if single-agg?
                                                        (sf-name pivot)
                                                        (str (sf-name pivot) "_" (second agg))))))
                                                  agg-specs)))
                                         all-pivot-keys)
                                 (repeat 0))
                                pivot-map)))
                           grouped))))]
      (ut/tapp>> [:transform-NOT-cached-for dsl (take 2 data) output])
      (swap! transform-cache assoc [(hash dsl) (hash data)] output)
      output)))

(def data2 [{:city "san marcos",
             :comments "This event took place in early fall around 1949-50. It occurred after a Boy Scout meeting in the Baptist Church. The Baptist Church sit",
             :country "us",
             :date_posted "2004-04-27",
             :datetime "1949-10-10 20:30:00.000000",
             :duration_hours "45 minutes",
             :id 1,
             :duration_seconds "2700",
             :duration_seconds_int 2700,
             :latitude "29.8830556",
             :longitude -97.9411111,
             :shape "cylinder",
             :state "tx"}
            {:city "lackland afb",
             :comments "1949 Lackland AFB&#44 TX.  Lights racing across the sky &amp; making 90 degree turns on a dime.",
             :country nil,
             :date_posted "2005-12-16",
             :datetime "1949-10-10 21:00:00.000000",
             :duration_hours "1-2 hrs",
             :id 2,
             :duration_seconds "7200",
             :duration_seconds_int 7200,
             :latitude "29.38421",
             :longitude -98.581082,
             :shape "light",
             :state "tx"}
            {:city "chester (uk/england)",
             :comments "Green/Orange circular disc over Chester&#44 England",
             :country "gb",
             :date_posted "2008-01-21",
             :datetime "1955-10-10 17:00:00.000000",
             :duration_hours "20 seconds",
             :id 3,
             :duration_seconds "20",
             :duration_seconds_int 20,
             :latitude "53.2",
             :longitude -2.916667,
             :shape "circle",
             :state nil}
            {:city "edna",
             :comments "My older brother and twin sister were leaving the only Edna theater at about 9 PM&#44...we had our bikes and I took a different route home",
             :country "us",
             :date_posted "2004-01-17",
             :datetime "1956-10-10 21:00:00.000000",
             :duration_hours "1/2 hour",
             :id 4,
             :duration_seconds "20",
             :duration_seconds_int 20,
             :latitude "28.9783333",
             :longitude -96.6458333,
             :shape "circle",
             :state "tx"}])

;; Sample data and DSL for testing
(def data
  [{:product "Widget"
    :category "Gadget"
    :region "North"
    :rep "Alice"
    :items-sold 10
    :sales 200}
   {:product "Widget"
    :category "Gadget"
    :region "South"
    :rep "Bob"
    :items-sold 15
    :sales 300}
   {:product "Gizmo"
    :category "Gadget"
    :region "North"
    :rep "Alice"
    :items-sold 20
    :sales 400}
   {:product "Gizmo"
    :category "Gadget"
    :region "South"
    :rep "Bob"
    :items-sold 25
    :sales 500}
   {:product "Thingamabob"
    :category "Doohickey"
    :region "North"
    :rep "Alice"
    :items-sold 30
    :sales 600}
   {:product "Thingamabob"
    :category "Doohickey"
    :region "South"
    :rep "Bob"
    :items-sold 35
    :sales 700}
   {:product "Thingamabob"
    :category "Doohickey"
    :region "Northeast"
    :rep "Joe"
    :items-sold 335
    :sales 400}
   {:product "Whatsit"
    :category "Doohickey"
    :region "North"
    :rep "Alice"
    :items-sold 40
    :sales 800}
   {:product "Whatsit"
    :category "Doohickey"
    :region "South"
    :rep "Bob"
    :items-sold 45
    :sales 900}])

;; tests

(def dsl-pivot-region
  {:transform-select [[[:sum :sales] :total-sales]
                      ;[[:sum :items-sold] :total-items]
                      :product :category :rep]
   :from [:data]
   :pivot-by [:category]})

(def dsl-nested-category
  {:transform-select [[[:sum :sales] :total-sales]
                      [[:sum :items-sold] :total-items]
                      :product [:region :r] :rep]
   :from [:data]
   :nest-by [:category]})

(def dsl-pivot-rep
  {:transform-select [[[:sum :sales] :total-sales]
                      [[:sum :items-sold] :total-items]
                      [[:avg :sales] :avg-sales2]
                      ; [[:count :region] :cnt-sales2]
                      ;[:product :product-alias] :category
                      :product :category
                      ;[:region :reggie]
                      :region]
   :from [:data]
   :pivot-by [:rep]})

(def dsl-pivot-rep2
  {:transform-select [[[:sum :sales] :total-sales]
                      :product
                      ; [:region :r]
                      ]
   :from [:data]
   :pivot-by [:rep]})

(def dsl-deep-nest
  {:transform-select [[[:sum :sales] :total-sales]
                      :product]
   :from [:data]
   :nest-by [:region :category :rep]})

 ;(ut/tapp>> [:dsl-pivot-region (transform dsl-pivot-region data)])
 ;(ut/tapp>> [:dsl-nested-category (transform dsl-nested-category data)])
 ;(ut/tapp>> [:dsl-deep-nest (transform dsl-deep-nest data)])
 ;(ut/tapp>> [:dsl-pivot-rep (transform dsl-pivot-rep data)])
 ;(ut/tapp>> [:dsl-pivot-rep2 (transform dsl-pivot-rep2 data)])

