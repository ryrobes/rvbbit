(ns rvbbit-backend.stacks
  (:require [clojure.string         :as str]
            [clojure.set            :as cset]
            [clojure.edn            :as edn]
            [rvbbit-backend.rabbit-script :as p]
            [rvbbit-backend.util :as ut]
            [rvbbit-backend.db :as db]
            [puget.printer          :as puget]
            [clj-time.core          :as t]
            [clj-time.format        :as tf])
  (:import [jline TerminalFactory])
  (:gen-class))


(def sample-data
  [{:date "2023-03-01" :category "A" :sales 100 :cost 80}
   {:date "2023-03-02" :category "A" :sales 120 :cost 85}
   {:date "2023-03-03" :category "A" :sales 90  :cost 70}
   {:date "2023-03-04" :category "B" :sales 200 :cost 150}
   {:date "2023-03-05" :category "B" :sales 180 :cost 140}
   {:date "2023-03-06" :category "B" :sales 220 :cost 160}])

(def terminal (TerminalFactory/get))
(defn get-terminal-width [] (try (.getWidth terminal) (catch Throwable _ 85)))
(def console-lock (Object.))
(defn safe-cprint [x & [opts-map]]
  (locking console-lock
    (puget/with-options (merge
                         {:width (get-terminal-width)}
                         opts-map) (puget/cprint x))))

(defn ppl [x & [opts-map]] (safe-cprint x opts-map))

;; (ppl [:yo "yo"])

;; ============================================================================
;; Core Formula Implementations
;; ============================================================================

(defn calc-moving-window
  [{:keys [data value-field window-size operation]
    :or {window-size 3
         operation :avg}}]
  (let [op-fn (case operation
                :avg #(double (/ (reduce + %) (count %)))  ;; Added double conversion
                :sum #(reduce + %)
                :min #(apply min %)
                :max #(apply max %))]
    (->> data
         (map-indexed
          (fn [idx row]
            (let [window (->> (range (- idx (dec window-size)) (inc idx))
                              (filter #(and (>= % 0) (< % (count data))))
                              (map #(get (nth data %) value-field)))]
              (assoc row :window_value
                     (when (seq window)
                       (op-fn window))))))
         vec)))

(defn calc-relative-change
  [{:keys [data value-field partition-fields]
    :or {partition-fields []}}]
  (let [partitioned (if (empty? partition-fields)
                      [data]
                      (partition-by #(select-keys % partition-fields) data))]
    (->> partitioned
         (mapcat (fn [group]
                   (map-indexed
                    (fn [idx row]
                      (let [prev-value (when (pos? idx)
                                         (get (nth group (dec idx)) value-field))]
                        (cond-> row
                          true (assoc :prev_value prev-value)
                          prev-value (assoc
                                      :change (- (get row value-field) prev-value)
                                      :pct_change (/ (- (get row value-field) prev-value)
                                                     prev-value)))))
                    group)))
         vec)))

(defn calc-contribution
  [{:keys [data value-field group-fields]}]
  (let [groups (group-by #(select-keys % group-fields) data)]
    (->> data
         (map (fn [row]
                (let [group-vals (map value-field (get groups (select-keys row group-fields)))
                      total (reduce + group-vals)
                      sorted-vals (sort > group-vals)]
                  (assoc row
                         :total_in_group total
                         :pct_of_group (/ (value-field row) total)
                         :rank_in_group (inc (count (filter #(> % (value-field row))
                                                            sorted-vals)))))))
         vec)))

(defn calc-percent-of-total
  "Calculate percent of total with optional dimension grouping
   If no dimension fields provided, calculates against grand total"
  [{:keys [data value-field dimension-fields]
    :or {dimension-fields []}}]
  (let [grand-total (reduce + (map value-field data))
        groups (if (empty? dimension-fields)
                 {nil data}  ; Single group for grand total
                 (group-by #(select-keys % dimension-fields) data))]
    (->> data
         (map (fn [row]
                (let [group-key (if (empty? dimension-fields)
                                  nil
                                  (select-keys row dimension-fields))
                      group-total (reduce + (map value-field (get groups group-key)))]
                  (cond-> row
                    true (assoc :pct_of_total (double (/ (value-field row) grand-total)))
                    (not-empty dimension-fields)
                    (assoc :pct_of_group (double (/ (value-field row) group-total)))))))
         vec)))

(defn calc-zscore
  [{:keys [data value-field group-fields]
    :or {group-fields []}}]
  (println "Calculating z-scores for field:" value-field)
  (println "First few values:" (take 5 (map value-field data)))
  (let [groups (if (empty? group-fields)
                 [data]
                 (vals (group-by #(select-keys % group-fields) data)))]
    (->> groups
         (mapcat (fn [group]
                   (let [values (keep value-field group)  ;; Use keep instead of map to filter out nils
                         mean (when (seq values)  ;; Only calculate if we have values
                                (/ (reduce + values) (count values)))
                         std (when (seq values)   ;; Only calculate if we have values
                               (Math/sqrt (/ (reduce + (map #(Math/pow (- % mean) 2) values))
                                             (count values))))]
                     (map #(if-let [val (value-field %)]  ;; Only calculate z-score for non-nil values
                             (assoc %
                                    :zscore (when (and mean std (not (zero? std)))
                                              (/ (- val mean) std))
                                    :is_significant (when (and mean std (not (zero? std)))
                                                      (> (Math/abs (/ (- val mean) std)) 2)))
                             %)  ;; Return original row if value is nil
                          group))))
         vec)))

(defn calc-rank-analysis
  [{:keys [data value-field partition-fields rank-type direction]
    :or {rank-type :rank
         direction :desc
         partition-fields []}}]
  (let [partitioned (if (empty? partition-fields)
                      [data]
                      (partition-by #(select-keys % partition-fields) data))
        rank-fn (if (= direction :desc) > <)]
    (->> partitioned
         (mapcat
          (fn [group]
            (let [sorted-vals (sort-by value-field rank-fn group)
                  total-count (count group)]
              (map-indexed
               (fn [idx row]
                 (let [curr-val (value-field row)
                       same-vals (filter #(= (value-field %) curr-val) sorted-vals)
                       min-rank (inc (.indexOf sorted-vals (first same-vals)))
                       dense-rank (inc (count (filter #(rank-fn (value-field %) curr-val) sorted-vals)))]
                   (assoc row
                          :rank_value (case rank-type
                                        :rank min-rank
                                        :dense_rank dense-rank
                                        :percent_rank (double (/ min-rank total-count))
                                        :cumulative (inc idx))
                          :rank_dense dense-rank
                          :percentile (double (/ min-rank total-count)))))
               sorted-vals))))
         vec)))

(defn calc-window-aggregation
  [{:keys [data value-field partition-fields order-field window-type window-start window-end aggregation]
    :or {window-type :rows
         window-start -1
         window-end 0
         aggregation :sum}}]
  (let [partitioned (if (empty? partition-fields)
                      [data]
                      (partition-by #(select-keys % partition-fields) data))
        agg-fn (case aggregation
                 :sum #(reduce + 0 %)
                 :avg #(/ (reduce + 0 %) (count %))
                 :min #(apply min %)
                 :max #(apply max %)
                 :median #(let [sorted (sort %)]
                            (nth sorted (quot (count sorted) 2)))
                 :count count
                 :stddev #(let [avg (/ (reduce + %) (count %))
                                squared-diff (map (fn [x] (Math/pow (- x avg) 2)) %)]
                            (Math/sqrt (/ (reduce + squared-diff) (count %)))))]
    (->> partitioned
         (mapcat
          (fn [group]
            (let [sorted-group (sort-by order-field group)]
              (map-indexed
               (fn [idx row]
                 (let [window-vals (case window-type
                                     :rows (->> (range (+ idx window-start) (+ idx window-end 1))
                                                (filter #(and (>= % 0) (< % (count sorted-group))))
                                                (map #(value-field (nth sorted-group %))))
                                     :range (let [current-val (order-field row)]
                                              (->> sorted-group
                                                   (filter #(and (>= (order-field %) (- current-val window-start))
                                                                 (<= (order-field %) (+ current-val window-end))))
                                                   (map value-field))))]
                   (assoc row :window_result (when (seq window-vals)
                                               (agg-fn window-vals)))))
               sorted-group))))
         vec)))

(defn calc-distribution
  [{:keys [data value-field partition-fields]}]
  (let [partitioned (if (empty? partition-fields)
                      [data]
                      (partition-by #(select-keys % partition-fields) data))
        calc-quartiles (fn [values]
                         (let [sorted (sort values)
                               len (count sorted)
                               q1-idx (quot len 4)
                               q2-idx (quot len 2)
                               q3-idx (* 3 (quot len 4))
                               q1 (nth sorted q1-idx)
                               q2 (nth sorted q2-idx)
                               q3 (nth sorted q3-idx)
                               iqr (- q3 q1)]
                           {:q1 q1 :q2 q2 :q3 q3 :iqr iqr}))]
    (->> partitioned
         (mapcat
          (fn [group]
            (let [values (map value-field group)
                  stats (calc-quartiles values)
                  {:keys [q1 q2 q3 iqr]} stats
                  outlier-low (- q1 (* 1.5 iqr))
                  outlier-high (+ q3 (* 1.5 iqr))]
              (map (fn [row]
                     (let [val (value-field row)]
                       (assoc row
                              :quartile (cond
                                          (<= val q1) 1
                                          (<= val q2) 2
                                          (<= val q3) 3
                                          :else 4)
                              :decile (inc (int (* 10 (/ (.indexOf (sort values) val)
                                                         (count values)))))
                              :iqr iqr
                              :is_outlier (or (< val outlier-low)
                                              (> val outlier-high)))))
                   group))))
         vec)))

(defn calc-running-total
  [{:keys [data value-field partition-fields order-field]
    :or {partition-fields []}}]
  (let [partitioned (if (empty? partition-fields)
                      [data]
                      (partition-by #(select-keys % partition-fields) data))]
    (->> partitioned
         (mapcat
          (fn [group]
            (let [sorted-group (sort-by order-field group)]
              (reductions
               (fn [acc row]
                 (let [curr-total (+ (get acc :running_total 0)
                                     (value-field row))]
                   (assoc row
                          :running_total curr-total
                          :running_average (/ curr-total
                                              (inc (.indexOf sorted-group row)))
                          :pct_of_total (/ curr-total
                                           (reduce + (map value-field group))))))
               {:running_total 0}
               sorted-group))))
         vec)))

(defn calc-running-totals
  [{:keys [data value-field partition-fields order-field]}]
  (let [partitioned (if (empty? partition-fields)
                      [data]
                      (partition-by #(select-keys % partition-fields) data))]
    (->> partitioned
         (mapcat
          (fn [group]
            (let [sorted-group (sort-by order-field group)
                  group-total (reduce + (map value-field sorted-group))]
              (reductions
               (fn [acc row]
                 (let [curr-total (+ (get acc :running_total 0)
                                     (value-field row))]
                   (assoc row
                          :running_total curr-total
                          :running_average (/ curr-total
                                              (inc (.indexOf sorted-group row)))
                          :pct_of_total (/ curr-total group-total))))
               {:running_total 0}
               sorted-group))))
         vec)))


(defn calc-time-bucket
  "Bucket timestamps into regular intervals"
  [{:keys [data date-field bucket-size]
    :or {bucket-size :month}}]
  (let [formatter (tf/formatter "yyyy-MM-dd")
        bucket-fn (case bucket-size
                    :hour #(t/date-time (t/year %) (t/month %) (t/day %) (t/hour %))
                    :day #(t/date-midnight (t/year %) (t/month %) (t/day %))
                    :week #(t/minus % (t/days (dec (t/day-of-week %))))
                    :month #(t/date-midnight (t/year %) (t/month %) 1)
                    :quarter #(let [month (t/month %)
                                    quarter-month (- month (mod (dec month) 3))]
                                (t/date-midnight (t/year %) quarter-month 1))
                    :year #(t/date-midnight (t/year %) 1 1))]
    (->> data
         (map #(let [dt (tf/parse formatter (date-field %))
                     bucket-start (bucket-fn dt)
                     bucket-end (t/minus
                                 (case bucket-size
                                   :hour (t/plus (bucket-fn dt) (t/hours 1))
                                   :day (t/plus (bucket-fn dt) (t/days 1))
                                   :week (t/plus (bucket-fn dt) (t/weeks 1))
                                   :month (t/plus (bucket-fn dt) (t/months 1))
                                   :quarter (t/plus (bucket-fn dt) (t/months 3))
                                   :year (t/plus (bucket-fn dt) (t/years 1)))
                                 (t/seconds 1))]
                 (assoc %
                        :bucket_start (tf/unparse formatter bucket-start)
                        :bucket_end (tf/unparse formatter bucket-end))))
         vec)))

(defn calc-decay-score
  "Calculate time-weighted score with exponential decay"
  [{:keys [data value-field date-field half-life]
    :or {half-life 30}}]
  (let [now (t/today)  ; Use clj-time's today function
        formatter (tf/formatter "yyyy-MM-dd")  ; Add formatter for date strings
        decay-constant (/ (Math/log 2) half-life)]
    (->> data
         (map #(let [date (tf/parse formatter (date-field %))  ; Parse the date string
                     days-ago (t/in-days (t/interval date now))  ; Calculate days between dates
                     decay-factor (Math/exp (* -1 decay-constant days-ago))]
                 (assoc % :decay_score (* (value-field %) decay-factor))))
         vec)))

(defn calc-dimension-bucket
  "Bucket dimension values based on various strategies"
  [{:keys [data dimension-field strategy bucket-count threshold]
    :or {strategy :frequency
         bucket-count 5
         threshold 0.01}}]
  (let [total-count (count data)
        frequency-map (frequencies (map dimension-field data))

        buckets
        (case strategy
          :frequency
          (let [sorted-dims (sort-by #(get frequency-map %) > (keys frequency-map))
                min-count (* threshold total-count)]
            (->> sorted-dims
                 (reduce (fn [{:keys [current-bucket buckets remaining-count] :as acc} dim]
                           (let [dim-count (get frequency-map dim)]
                             (if (or (< remaining-count min-count)
                                     (>= (count buckets) (dec bucket-count)))
                               (update acc :buckets assoc dim "Other")
                               (if (< (count current-bucket) (/ total-count bucket-count))
                                 (-> acc
                                     (update :current-bucket conj dim)
                                     (update :remaining-count - dim-count))
                                 (-> acc
                                     (update :buckets merge (zipmap current-bucket
                                                                    (repeat (str "Bucket " (count buckets)))))
                                     (assoc :current-bucket [dim])
                                     (update :remaining-count - dim-count))))))
                         {:current-bucket []
                          :buckets {}
                          :remaining-count total-count})
                 :buckets))

          :alphabetical
          (let [sorted-dims (sort (keys frequency-map))
                bucket-size (Math/ceil (/ (count sorted-dims) bucket-count))]
            (->> sorted-dims
                 (partition-all bucket-size)
                 (map-indexed (fn [idx dims]
                                [dims (str "Bucket " (inc idx))]))
                 (reduce (fn [acc [dims bucket-name]]
                           (merge acc (zipmap dims (repeat bucket-name))))
                         {})))

          :custom-ranges
          (throw (ex-info "Custom ranges not yet implemented"
                          {:strategy strategy})))]

    (->> data
         (map #(assoc %
                      :dimension_bucket (get buckets (dimension-field %))
                      :is_other (= "Other" (get buckets (dimension-field %)))))
         vec)))

(defn calc-period-comparison
  "Compare values across different time periods"
  [{:keys [data value-field date-field period-type custom-offset]
    :or {period-type :prior}}]
  (let [formatter (tf/formatter "yyyy-MM-dd")  ; Add formatter for parsing dates
        period-offset (case period-type
                        :prior 1
                        :yoy 12
                        :mom 1
                        :qoq 3
                        custom-offset)
        get-period-key (case period-type
                         :yoy #(vector (t/year %) (t/month %))
                         :mom #(t/month %)
                         :qoq #(let [month (t/month %)]  ; Convert to clj-time quarter calculation
                                 (quot (dec month) 3))
                         :prior identity)
        parsed-data (map #(update % date-field (fn [d] (tf/parse formatter d))) data)  ; Parse dates
        by-period (group-by #(get-period-key (date-field %)) parsed-data)]
    (->> parsed-data
         (map (fn [row]
                (let [current-period (get-period-key (date-field row))
                      comparison-data (get by-period
                                           (case period-type
                                             :yoy (update current-period 0 dec)  ; Decrement year
                                             :mom (if (= current-period 1)
                                                    12
                                                    (dec current-period))
                                             :qoq (if (= current-period 0)
                                                    3
                                                    (dec current-period))
                                             :prior (dec current-period)))
                      comparison-value (when comparison-data
                                         (reduce + (map value-field comparison-data)))]
                  (cond-> row
                    comparison-value
                    (assoc :comparison_value comparison-value
                           :absolute_change (- (value-field row) comparison-value)
                           :percent_change (/ (- (value-field row) comparison-value)
                                              comparison-value))))))
         vec)))

(defn calc-pivot
  [{:keys [data value-field pivot-field group-by-fields aggregate]
    :or {group-by-fields []
         aggregate :sum}}]
  (let [value-field (if (string? value-field)
                      (keyword value-field)
                      value-field)
        pivot-field (if (string? pivot-field)
                      (keyword pivot-field)
                      pivot-field)
        group-by-fields (if (string? group-by-fields)
                          [(keyword group-by-fields)]
                          (mapv keyword group-by-fields))
        pivot-values (distinct (map pivot-field data))
        agg-fn (case aggregate
                 :sum #(reduce + 0 %)
                 :avg #(/ (reduce + 0 %) (count %))
                 :min #(apply min %)
                 :max #(apply max %)
                 :count count)]
    (if (empty? group-by-fields)
      ;; If no grouping fields, just create one row with pivot totals
      [(let [by-pivot (group-by pivot-field data)]
         (reduce (fn [acc pivot-val]
                   (assoc acc
                          (keyword (str (name pivot-field) "_" pivot-val))
                          (if-let [pivot-items (get by-pivot pivot-val)]
                            (agg-fn (map value-field pivot-items))
                            0)))
                 {}
                 pivot-values))]
      ;; Otherwise do the grouped pivot
      (->> data
           (group-by #(select-keys % group-by-fields))
           (map (fn [[base-row items]]
                  (let [by-pivot (group-by pivot-field items)]
                    (merge
                     base-row
                     (reduce (fn [acc pivot-val]
                               (assoc acc
                                      (keyword (str (name pivot-field) "_" pivot-val))
                                      (if-let [pivot-items (get by-pivot pivot-val)]
                                        (agg-fn (map value-field pivot-items))
                                        0)))
                             {}
                             pivot-values)))))
           vec))))

(defn calc-lag-lead
  "Calculate lag/lead values with conditions"
  [{:keys [data value-field partition-fields order-field offset direction]
    :or {offset 1
         direction :lag}}]
  (let [partitioned (if (empty? partition-fields)
                      [data]
                      (partition-by #(select-keys % partition-fields) data))]
    (->> partitioned
         (mapcat
          (fn [group]
            (let [sorted-group (sort-by order-field group)]
              (map-indexed
               (fn [idx row]
                 (let [offset-idx (case direction
                                    :lag (- idx offset)
                                    :lead (+ idx offset))
                       offset-value (when (and (>= offset-idx 0)
                                               (< offset-idx (count sorted-group)))
                                      (value-field (nth sorted-group offset-idx)))]
                   (assoc row
                          :offset_value offset-value
                          :is_changed (and offset-value
                                           (not= offset-value (value-field row))))))
               sorted-group))))
         vec)))


(def formula-registry
  {"moving-window"
   {:id "moving-window"
    :name "Moving Window"
    :description "Calculate moving window statistics"
    :input-fields [{:name "value-field"
                    :type :numeric
                    :description "Field to analyze"}
                   {:name "window-size"
                    :type :integer
                    :default 3
                    :description "Size of moving window"}
                   {:name "operation"
                    :type :enum
                    :values [:avg :sum :min :max]
                    :default :avg
                    :description "Window operation to perform"}]
    :output-fields [{:name "window_value"
                     :type :numeric
                     :description "Result of window calculation"}]
    :fn calc-moving-window}

   "dimension-bucket"
   {:id "dimension-bucket"
    :name "Dimension Bucketing"
    :description "Group dimension values into buckets using various strategies"
    :input-fields [{:name "dimension-field"
                    :type :dimension
                    :description "Field to bucket"}
                   {:name "strategy"
                    :type :enum
                    :values [:frequency :alphabetical :custom-ranges]
                    :default :frequency
                    :description "Bucketing strategy"}
                   {:name "bucket-count"
                    :type :integer
                    :default 5
                    :description "Number of buckets to create"}
                   {:name "threshold"
                    :type :numeric
                    :default 0.01
                    :description "Minimum frequency threshold for frequency strategy"}]
    :output-fields [{:name "dimension_bucket"
                     :type :string
                     :description "Assigned bucket name"}
                    {:name "is_other"
                     :type :boolean
                     :description "Flag for values in 'Other' bucket"}]
    :fn calc-dimension-bucket}

   "running-total"
   {:id "running-total"
    :name "Running Total"
    :description "Calculate running totals and related metrics"
    :input-fields [{:name "value-field"
                    :type :numeric
                    :description "Value to accumulate"}
                   {:name "partition-fields"
                    :type [:dimension]
                    :optional true
                    :description "Optional fields to partition by"}
                   {:name "order-field"
                    :type [:date :numeric]
                    :description "Field to order by"}]
    :output-fields [{:name "running_total"
                     :type :numeric
                     :description "Cumulative sum"}
                    {:name "running_average"
                     :type :numeric
                     :description "Running average"}
                    {:name "pct_of_total"
                     :type :numeric
                     :description "Percent of final total"}]
    :fn calc-running-total}

   "percent-of-total"
   {:id "percent-of-total"
    :name "Percent of Total"
    :description "Calculate percentage of total, optionally within dimension groups"
    :input-fields [{:name "value-field"
                    :type :numeric
                    :description "Numeric field to calculate percentages from"}
                   {:name "dimension-fields"
                    :type [:dimension]
                    :optional true
                    :description "Optional fields to group by"}]
    :output-fields [{:name "pct_of_total"
                     :type :numeric
                     :description "Percentage of grand total"}
                    {:name "pct_of_group"
                     :type :numeric
                     :description "Percentage within dimension group (if dimensions specified)"}]
    :fn calc-percent-of-total}

   "time-bucket"
   {:id "time-bucket"
    :name "Time Bucket"
    :description "Group timestamps into regular intervals"
    :input-fields [{:name "date-field"
                    :type :date
                    :description "Date/time to bucket"}
                   {:name "bucket-size"
                    :type :enum
                    :values [:hour :day :week :month :quarter :year]
                    :default :month
                    :description "Bucket size"}]
    :output-fields [{:name "bucket_start"
                     :type :date
                     :description "Start of bucket"}
                    {:name "bucket_end"
                     :type :date
                     :description "End of bucket"}]
    :fn calc-time-bucket}

   "decay-score"
   {:id "decay-score"
    :name "Time Decay Score"
    :description "Calculate score with time decay"
    :input-fields [{:name "value-field"
                    :type :numeric
                    :description "Value to decay"}
                   {:name "date-field"
                    :type :date
                    :description "Date for decay calculation"}
                   {:name "half-life"
                    :type :integer
                    :default 30
                    :description "Days for 50% decay"}]
    :output-fields [{:name "decay_score"
                     :type :numeric
                     :description "Time-adjusted score"}]
    :fn calc-decay-score}

   "relative-change"
   {:id "relative-change"
    :name "Relative Change"
    :description "Calculate changes relative to previous values"
    :input-fields [{:name "value-field"
                    :type :numeric
                    :description "Field to analyze"}
                   {:name "partition-fields"
                    :type [:dimension]
                    :optional true
                    :description "Fields to partition by"}]
    :output-fields [{:name "prev_value"
                     :type :numeric
                     :description "Previous value in sequence"}
                    {:name "change"
                     :type :numeric
                     :description "Absolute change from previous"}
                    {:name "pct_change"
                     :type :numeric
                     :description "Percentage change from previous"}]
    :fn calc-relative-change}

   "contribution-analysis"
   {:id "contribution-analysis"
    :name "Contribution Analysis"
    :description "Analyze relative contributions and rankings"
    :input-fields [{:name "value-field"
                    :type :numeric
                    :description "Value to analyze"}
                   {:name "group-fields"
                    :type [:dimension]
                    :description "Fields to group by"}]
    :output-fields [{:name "total_in_group"
                     :type :numeric
                     :description "Total value within group"}
                    {:name "pct_of_group"
                     :type :numeric
                     :description "Percentage of group total"}
                    {:name "rank_in_group"
                     :type :numeric
                     :description "Rank within group"}]
    :fn calc-contribution}

   "zscore-analysis"
   {:id "zscore-analysis"
    :name "Z-Score Analysis"
    :description "Calculate statistical significance"
    :input-fields [{:name "value-field"
                    :type :numeric
                    :description "Value to analyze"}
                   {:name "group-fields"
                    :type [:dimension]
                    :optional true
                    :description "Optional grouping"}]
    :output-fields [{:name "zscore"
                     :type :numeric
                     :description "Standard deviations from mean"}
                    {:name "is_significant"
                     :type :boolean
                     :description "Beyond 2 standard deviations"}]
    :fn calc-zscore}

   "excel-formula"
   {:id "excel-formula"
    :name "Excel Formula"
    :description "Execute Excel-style formula on data"
    :input-fields [{:name "formula"
                    :type :string
                    :description "Formula string in Excel-like syntax"}
                   {:name "result-field"
                    :type :string
                    :description "Name for the result column"
                    :default "result"}]
    :output-fields [{:name :dynamic
                     :type :numeric
                     :description "Result of formula calculation"}]
    :fn (fn [{:keys [data formula result-field]
              :or {result-field "result"}}]
          (ppl ["🦝 Processing Excel formula:" formula :--> result-field])
          (p/evaluate-formula formula data result-field))}

"clojure"
{:id "clojure"
 :name "Clojure Expression"
 :description "Execute Clojure expression on data"
 :input-fields [{:name "formula"
                 :type :string
                 :description "Complete Clojure function taking rows parameter"}
                {:name "result-field"
                 :type :string
                 :description "Name for the result column"
                 :default "result"}]
 :output-fields [{:name :dynamic
                  :type :any
                  :description "Result of Clojure evaluation"}]
 :fn (fn [{:keys [data formula result-field]
           :or {result-field "result"}}]
       (ppl ["🦝 Processing Clojure:" formula :--> result-field])
       (let [expr-fn (try
                       (eval (read-string formula))
                       (catch Throwable e
                         (throw (ex-info "Invalid Clojure function"
                                         {:formula formula
                                          :error (.getMessage e)}))))]
         (try
           (if (fn? expr-fn)
             ;; Process in chunks to avoid memory pressure
             (vec
              (sequence
               cat  ;; flatten the chunks
               (map expr-fn  ;; apply the function to each chunk
                    (partition-all 1000 data))))
             (throw (ex-info "Expression must evaluate to a function"
                             {:formula formula})))
           (catch Throwable e
             (throw (ex-info "Error evaluating Clojure function"
                             {:formula formula
                              :error (.getMessage e)}))))))}



   "rank-analysis"
   {:id "rank-analysis"
    :name "Rank Analysis"
    :description "Flexible ranking with ties and dense ranking"
    :input-fields [{:name "value-field"
                    :type :numeric
                    :description "Value to rank"}
                   {:name "partition-fields"
                    :type [:dimension]
                    :optional true
                    :description "Partition fields"}
                   {:name "rank-type"
                    :type :enum
                    :values [:rank :dense_rank :percent_rank :cumulative]
                    :default :rank
                    :description "Type of ranking"}
                   {:name "direction"
                    :type :enum
                    :values [:asc :desc]
                    :default :desc
                    :description "Ranking direction"}]
    :output-fields [{:name "rank_value"
                     :type :numeric
                     :description "Rank value"}
                    {:name "rank_dense"
                     :type :numeric
                     :description "Dense rank"}
                    {:name "percentile"
                     :type :numeric
                     :description "Percentile rank"}]
    :fn calc-rank-analysis}

   "period-comparison"
   {:id "period-comparison"
    :name "Period Comparison"
    :description "Compare values across different time periods"
    :input-fields [{:name "value-field"
                    :type :numeric
                    :description "Value to compare"}
                   {:name "date-field"
                    :type :date
                    :description "Date field"}
                   {:name "period-type"
                    :type :enum
                    :values [:prior :yoy :mom :qoq :custom]
                    :default :prior
                    :description "Comparison type"}
                   {:name "custom-offset"
                    :type :integer
                    :optional true
                    :description "Custom periods offset"}]
    :output-fields [{:name "comparison_value"
                     :type :numeric
                     :description "Value from comparison period"}
                    {:name "absolute_change"
                     :type :numeric
                     :description "Absolute change"}
                    {:name "percent_change"
                     :type :numeric
                     :description "Percentage change"}]
    :fn calc-period-comparison}

   "window-aggregation"
   {:id "window-aggregation"
    :name "Window Aggregation"
    :description "Flexible window-based calculations"
    :input-fields [{:name "value-field"
                    :type :numeric
                    :description "Value to aggregate"}
                   {:name "partition-fields"
                    :type [:dimension]
                    :optional true
                    :description "Partition fields"}
                   {:name "order-field"
                    :type [:date :numeric]
                    :description "Field to order by"}
                   {:name "window-type"
                    :type :enum
                    :values [:rows :range]
                    :default :rows
                    :description "Window type"}
                   {:name "window-start"
                    :type :integer
                    :default -1
                    :description "Window start offset"}
                   {:name "window-end"
                    :type :integer
                    :default 0
                    :description "Window end offset"}
                   {:name "aggregation"
                    :type :enum
                    :values [:sum :avg :min :max :median :count :stddev]
                    :default :sum
                    :description "Aggregation function"}]
    :output-fields [{:name "window_result"
                     :type :numeric
                     :description "Window calculation result"}]
    :fn calc-window-aggregation}

   "distribution-analysis"
   {:id "distribution-analysis"
    :name "Distribution Analysis"
    :description "Analyze value distributions and patterns"
    :input-fields [{:name "value-field"
                    :type :numeric
                    :description "Value to analyze"}
                   {:name "partition-fields"
                    :type [:dimension]
                    :optional true
                    :description "Partition fields"}]
    :output-fields [{:name "quartile"
                     :type :numeric
                     :description "Quartile (1-4)"}
                    {:name "decile"
                     :type :numeric
                     :description "Decile (1-10)"}
                    {:name "iqr"
                     :type :numeric
                     :description "Inter-quartile range"}
                    {:name "is_outlier"
                     :type :boolean
                     :description "IQR-based outlier detection"}]
    :fn calc-distribution}

   "running-totals"
   {:id "running-totals"
    :name "Running Totals"
    :description "Cumulative calculations with flexible resets"
    :input-fields [{:name "value-field"
                    :type :numeric
                    :description "Value to accumulate"}
                   {:name "partition-fields"
                    :type [:dimension]
                    :optional true
                    :description "Reset boundaries"}
                   {:name "order-field"
                    :type [:date :numeric]
                    :description "Sort field"}]
    :output-fields [{:name "running_total"
                     :type :numeric
                     :description "Cumulative sum"}
                    {:name "running_average"
                     :type :numeric
                     :description "Cumulative average"}
                    {:name "pct_of_total"
                     :type :numeric
                     :description "Running percent of total"}]
    :fn calc-running-totals}

   "pivot-transform"
   {:id "pivot-transform"
    :name "Pivot Transform"
    :description "Dynamic pivot/unpivot operations with optional grouping dimensions"
    :input-fields [{:name "value-field"
                    :type :numeric
                    :description "Value to pivot"}
                   {:name "pivot-field"
                    :type :dimension
                    :description "Field to pivot on"}
                   {:name "group-by-fields"
                    :type [:dimension]
                    :optional true
                    :default []
                    :description "Fields to group by before pivoting"}
                   {:name "aggregate"
                    :type :enum
                    :values [:sum :avg :min :max :count]
                    :default :sum
                    :description "Aggregation for pivot"}]
    :output-fields [{:name :dynamic
                     :type :numeric
                     :description "Pivoted values as new columns"}]
    :fn calc-pivot}

   "lag-lead-analysis"
   {:id "lag-lead-analysis"
    :name "Lag/Lead Analysis"
    :description "Access previous/next values with conditions"
    :input-fields [{:name "value-field"
                    :type [:numeric :dimension]
                    :description "Field to analyze"}
                   {:name "partition-fields"
                    :type [:dimension]
                    :optional true
                    :description "Partition fields"}
                   {:name "order-field"
                    :type [:date :numeric]
                    :description "Sort field"}
                   {:name "offset"
                    :type :integer
                    :default 1
                    :description "Number of positions to look"}
                   {:name "direction"
                    :type :enum
                    :values [:lag :lead]
                    :default :lag
                    :description "Look backward or forward"}]
    :output-fields [{:name "offset_value"
                     :type :dynamic
                     :description "Value at offset"}
                    {:name "is_changed"
                     :type :boolean
                     :description "Value changed from offset"}]
    :fn calc-lag-lead}})





;; A Bunch is just a map with a :pipeline key containing more carrots/bunches
(def bunches
  {"sales-seasonality"
   {:name "Sales Seasonality Analysis"
    :description "Comprehensive analysis of seasonal patterns and anomalies"
    :required-fields #{:date :sales :category}
    :pipeline  ; This can now be used directly in a pipeline
    [{:formula "time-bucket"
      :params {:date-field :date
               :bucket-size :month}}
     {:formula "window-aggregation"
      :params {:value-field :sales
               :partition-fields [:category]
               :order-field :bucket_start
               :window-size 12
               :aggregation :avg}}]}

   "customer-value"
   {:name "Customer Value Analysis"
    :description "Customer spending patterns and segments"
    :required-fields #{:customer_id :purchase_date :amount}
    :pipeline
    [{:formula "running-totals"
      :params {:value-field :amount
               :partition-fields [:customer_id]
               :order-field :purchase_date}}
     {:formula "distribution-analysis"
      :params {:value-field :running_total}}]}})

;; ============================================================================
;; Pipeline Execution
;; ============================================================================

(defn execute-pipeline [data pipeline & [depth notify-fn]]
  (let [pipeline-start-time (System/nanoTime)
        depth (or depth 0)
        step-vec (atom [])
        indent (apply str (repeat depth "  "))
        prefix (str indent (if (pos? depth) "↳ " ""))]

    (when (zero? depth)
      (ppl [(str prefix "🐇🥕 Executing pipeline with:") (vec (keys (first data))) [:rows (count data)]]))

    (let [fields (atom (set (keys (first data))))
          result (reduce (fn [curr-data step]
                           (let [start-time (System/nanoTime)]
                            ;; Show step execution with proper indentation
                             (when notify-fn (notify-fn (str "running stack layer: " (inc (.indexOf pipeline step)) " " prefix " " (:result-field step))))
                             (ppl [(str prefix "🥕 " (cond
                                                       (string? step) (str "Bunch '" step "'")
                                                       (:pipeline step) (str "Bunch '" (:name step) "'")
                                                       :else (str "Formula '" (:formula step) "'")))])

                             (let [error-shoe (atom nil)
                                   result (cond
                                         ;; It's a bunch reference
                                            (string? step)
                                            (if-let [bunch (get bunches step)]
                                              (do
                                                (ppl [(str prefix "   ⚡ Required fields:") (:required-fields bunch)])
                                                (if (every? (set (keys (first curr-data))) (:required-fields bunch))
                                                  (get (execute-pipeline curr-data (:pipeline bunch) (inc depth) notify-fn) :result)
                                                  (throw (ex-info "Data missing required fields for bunch"
                                                                  {:bunch step
                                                                   :required (:required-fields bunch)}))))
                                              (throw (ex-info "Unknown bunch" {:bunch step})))

                                         ;; It's a bunch definition inline
                                            (:pipeline step)
                                            (do
                                              (ppl [(str prefix "   ⚡ Contains") (count (:pipeline step)) "steps"])
                                              (get (execute-pipeline curr-data (:pipeline step) (inc depth) notify-fn) :result))

                                         ;; It's a regular carrot
                                            :else
                                            (try
                                              (let [formula (get formula-registry (:formula step))
                                                  formula-fn (:fn formula)]
                                              (ppl [(str prefix "   ⚡ Params:") (:params step)])
                                              (formula-fn (assoc (:params step) :data curr-data)))
                                              (catch Exception e
                                                (do ;(when notify-fn (notify-fn (str " Step Error: " (get formula-registry (:formula step)) (str e))))
                                                    (reset! error-shoe {:error (str e)})
                                                    curr-data))))
                                   end-time (System/nanoTime)
                                   duration-ms (/ (- end-time start-time) 1000000.0)]
                               (ppl [(str prefix "   ✓ Completed in") (format "%.2f" duration-ms) "ms"
                                     ;(vec (keys (first result)))
                                     [:fields-added (cset/difference (set (keys (first result))) @fields) (:params step)]])
                               (swap! step-vec conj (let [fields-added (vec (cset/difference (set (keys (first result))) @fields))
                                                          related-fields (if (str/starts-with? (str (get-in step [:params :formula])) "=")
                                                                           (try
                                                                             (->> (p/get-cached-ast (get-in step [:params :formula]))
                                                                                  (tree-seq coll? seq)
                                                                                  (keep (fn [node]
                                                                                          (when (and (record? node)
                                                                                                     (= (type node) rvbbit_backend.rabbit_script.ColumnRef))
                                                                                            (:name node))))
                                                                                  vec)
                                                                             (catch Exception e
                                                                               (ppl [:ast-extraction-error (str e)])
                                                                               []))
                                                                           (try (ut/deep-flatten
                                                                                 (edn/read-string
                                                                                  (->
                                                                                   (str (get-in step [:params :formula]))
                                                                                   (str/replace "#" "")
                                                                                   (str/replace "@" "")
                                                                                   (str/replace "%" ""))))
                                                                                (catch Exception _ [])))]
                                                      (-> (:params step)
                                                          (assoc :error @error-shoe)
                                                          (assoc :ast-raw (try
                                                                            (when (str/starts-with? (str (get-in step [:params :formula])) "=")
                                                                            (pr-str (p/get-cached-ast (get-in step [:params :formula]))))
                                                                            (catch Exception _ [:ast-error!])))
                                                        ;; (assoc :ast-raw (get-in step [:params :formula]))
                                                          (assoc :related-fields (vec (cset/difference (set related-fields) (set fields-added))))
                                                          (assoc :duration-ms duration-ms)
                                                          (assoc :fields-added fields-added))))
                               (reset! fields (set (keys (first result))))
                               result)))
                         data
                         pipeline)
          pipeline-end-time (System/nanoTime)
          total-duration-ms (/ (- pipeline-end-time pipeline-start-time) 1000000.0)]

      (when (zero? depth)
        (ppl [(str prefix "🎯 Pipeline completed in") (format "%.2f" total-duration-ms) "ms"]))

      {:result result
       :steps @step-vec})))


(comment


  ;; Example pipeline using both pre-built and formula operations
  (def analysis-pipeline
    [{:formula "moving-window"
      :params {:value-field :sales
               :window-size 3}}

     {:formula "excel-formula"
      :params {:formula "=IF(sales > AVERAGE(sales), window_value * 1.1, window_value * 0.9)"
               :result-field "adjusted_value"}}

     {:formula "relative-change"
      :params {:value-field :adjusted_value
               :partition-fields [:category]}}

     {:formula "zscore-analysis"
      :params {:value-field :pct_change
               :group-fields [:category]}}])

  ;; Execute the pipeline
  (execute-pipeline sample-data analysis-pipeline))


;; ============================================================================
;; Example Usage with Pipeline Pattern
;; ============================================================================

(comment

  ;; Sample data


  ;; Single formula as pipeline
  (def moving-avg-pipeline
    [{:formula "moving-window"
      :params {:value-field :sales
               :window-size 3
               :operation :avg}}

     {:formula "excel-formula"
      :params {:formula "=sales / OFFSET(sales, -1)"
               :result-field "growth_rate"}}

     {:formula "excel-formula"
      :params {:formula "=sales * 2 / OFFSET(sales, -1)"
               :result-field "growth_rate22"}}])

  (execute-pipeline sample-data moving-avg-pipeline)





  ;; Calculate percent of total sales (no dimensions)
  (execute-pipeline sample-data
                    [{:formula "percent-of-total"
                      :params {:value-field :sales}}])

;; Calculate percent of total sales within each category
  (execute-pipeline sample-data
                    [{:formula "percent-of-total"
                      :params {:value-field :sales
                               :dimension-fields [:category]}}])





  ;; Single Excel formula as pipeline
  (def growth-rate-pipeline
    [{:formula "excel-formula"
      :params {:formula "=sales / OFFSET(sales, -1)"
               :result-field "growth_rate"}}])

  (execute-pipeline sample-data growth-rate-pipeline)


  ;; Simple calculation
[{:formula "clojure"
  :params {:formula "(fn [rows]
                      (mapv #(assoc % :doubled_sales (* (:sales %) 2))
                            rows))"
           :result-field "doubled_sales"}}]

;; More complex logic
[{:formula "clojure"
  :params {:formula "(fn [rows]
                      (mapv #(assoc % :adjusted_sales
                                    (if (> (:sales %) 100)
                                      (* (:sales %) 1.1)
                                      (* (:sales %) 0.9)))
                            rows))"
           :result-field "adjusted_sales"}}]

;; Using aggregations
[{:formula "clojure"
  :params {:formula "(fn [rows]
                      (let [avg-sales (/ (reduce + (map :sales rows))
                                       (count rows))]
                        (mapv #(assoc % :sales_vs_avg
                                      (- (:sales %) avg-sales))
                              rows)))"
           :result-field "sales_vs_avg"}}]



  (def ttt  [{:category "A", :cost 80, :date "2023-03-01", :year "2023", :month "03", :sales 100}
             {:category "B", :cost 85, :date "2023-03-02", :year "2023", :month "03", :sales 120}
             {:category "A", :cost 70, :date "2023-03-03", :year "2023", :month "03", :sales 90}
             {:category "B", :cost 150, :date "2024-03-04", :year "2024", :month "03", :sales 200}
             {:category "A", :cost 140, :date "2024-03-05", :year "2024", :month "03", :sales 180}
             {:category "B", :cost 160, :date "2024-03-06", :year "2024", :month "03", :sales 220}])

          ;;   (reduce (fn [acc stack-map]
          ;;   (try
          ;;     (do
          ;;       (ppl [:formula (get stack-map :formula) (get stack-map :name)])
          ;;       (p/evaluate-formula
          ;;        (get stack-map :formula) ; Get the formula from the stack map
          ;;        acc
          ;;        (get stack-map :name))) ; Pass in the accumulated result from previous evaluation
          ;;     (catch Throwable e
          ;;       (ppl [:formula-evaluation-error (str e)])
          ;;       acc))) ; On error, return unchanged accumulator
          ;; (vec ttt) ; Initial accumulator value is the original result
          ;; [{:formula "=sales / OFFSET(sales, -1)"
          ;;   :name :growtddh_rate1}
          ;;  {:formula "=sales + OFFSET(sales, -1)"
          ;;   :name "growth_rate2"}
          ;;  {:formula "=sales + OFFSET(sales, -1)"
          ;;   :name "growth_rate2s"}
          ;;  {:formula "=sales - OFFSET(sales, -1)"
          ;;   :name "growth_rate3"}])

    (execute-pipeline ttt [{:formula "excel-formula"
                            :params {:formula "=sales / OFFSET(sales, -1)"
                                     :result-field :growtddh_rate1}}
                           {:formula "excel-formula"
                            :params {:formula "=sales + OFFSET(sales, -1)"
                                     :result-field "growth_rate2"}}
                           {:formula "excel-formula"
                            :params {:formula "=sales + OFFSET(sales, -1)"
                                     :result-field "growth_rate2s"}}
                           {:formula "excel-formula"
                            :params {:formula "=sales - OFFSET(sales, -1)"
                                     :result-field "growth_rate3"}}
                           {:formula "clojure"
                            :params {:formula "(fn [rows]
                                                 (mapv #(assoc % :adjusted_sales
                                                               (if (> (:sales %) 100)
                                                                 (* (:sales %) 1.1)
                                                                 (* (:sales %) 0.9)))
                                                       rows))"
                                     :result-field "adjusted_sales"}}
                           ])



  (def pivot-pipeline
     [{:formula "pivot-transform"
       :params {:value-field "sales"
                :aggregate :sum
                :group-by-fields ["date"]
                :pivot-field "category"}}])

  (execute-pipeline ttt [{:formula "pivot-transform"
                          :params {:value-field "sales"
                                   :aggregate :sum
                                   :group-by-fields ["year"]
                                   :pivot-field "category"}}])





  ; Pivot with grouping by date
  (def pivot-pipeline
    [{:formula "pivot-transform"
      :params {:value-field "sales"
               :aggregate :sum
               :pivot-field "category"
               :group-by-fields ["date"]}}])

;; Or group by multiple dimensions
  (def pivot-pipeline-multi
    [{:formula "pivot-transform"
      :params {:value-field "sales"
               :aggregate :sum
               :pivot-field "category"
               :group-by-fields ["date" "region"]}}])





  ;; Category contribution as pipeline
  (def category-share-pipeline
    [{:formula "excel-formula"
      :params {:formula "=sales / SUM(FILTER(sales, category = CURRENT(category)))"
               :result-field "category_share"}}])

  ;; Complex multi-step analysis
  (def sales-analysis-pipeline
    [{:formula "moving-window"
      :params {:value-field :sales
               :window-size 3}}

     {:formula "relative-change"
      :params {:value-field :window_value
               :partition-fields [:category]}}

     {:formula "excel-formula"
      :params {:formula "=IF(pct_change > 0.1, 'growing', 'stable')"
               :result-field "trend"}}])

  (execute-pipeline sample-data sales-analysis-pipeline)





  ;; Complex margin analysis
  (def margin-analysis-pipeline
    [{:formula "excel-formula"
      :params {:formula "=(sales - cost) / sales"
               :result-field "margin"}}

     {:formula "moving-window"
      :params {:value-field :margin
               :window-size 3}}

     {:formula "zscore-analysis"
      :params {:value-field :window_value
               :group-fields [:category]}}])

  ;; Usage is now consistent:



;; User drags sales column, picks "Moving Average"
;; [{:formula "moving-window"
;;   :params {:value-field :sales}}]

;; ;; User adds trend analysis
;; [{:formula "moving-window"
;;   :params {:value-field :sales}}
;;  {:formula "excel-formula"
;;   :params {:formula "=window_value / OFFSET(window_value, -1)"}}]
  )

(comment

  ;; Price Elasticity Analysis
  (def price-elasticity-pipeline
    [{:formula "relative-change"
      :params {:value-field :price
               :partition-fields [:product]}}
     {:formula "relative-change"
      :params {:value-field :quantity
               :partition-fields [:product]}}
     {:formula "excel-formula"
      :params {:formula "=IF(pct_change_price != 0,
                           pct_change_quantity / pct_change_price,
                           0)"
               :result-field "elasticity"}}
     {:formula "zscore-analysis"
      :params {:value-field :elasticity
               :group-fields [:category]}}])

  ;; Cohort Retention Analysis
  (def cohort-retention-pipeline
    [{:formula "excel-formula"
      :params {:formula "=YEARMONTH(first_purchase_date)"
               :result-field "cohort"}}
     {:formula "excel-formula"
      :params {:formula "=MONTHS_BETWEEN(purchase_date, first_purchase_date)"
               :result-field "months_since_first"}}
     {:formula "contribution-analysis"
      :params {:value-field :customer_id
               :group-fields [:cohort :months_since_first]}}])

  ;; Inventory Turnover Analysis
  (def inventory-analysis-pipeline
    [{:formula "moving-window"
      :params {:value-field :quantity_sold
               :window-size 30  ;; 30 day window
               :operation :sum}}
     {:formula "excel-formula"
      :params {:formula "=window_value / inventory_on_hand"
               :result-field "turnover_rate"}}
     {:formula "zscore-analysis"
      :params {:value-field :turnover_rate
               :group-fields [:category]}}])

  ;; Market Basket Affinity
  (def basket-affinity-pipeline
    [{:formula "excel-formula"
      :params {:formula "=COUNT(FILTER(order_id, product_id = CURRENT(product_id)))"
               :result-field "product_frequency"}}
     {:formula "excel-formula"
      :params {:formula "=COUNT(FILTER(order_id,
                                     product_id = CURRENT(product_id) AND
                                     related_product_id IN SAME_ORDER))"
               :result-field "joint_frequency"}}
     {:formula "excel-formula"
      :params {:formula "=joint_frequency / (product_frequency *
                        COUNT(FILTER(order_id, product_id = related_product_id)) /
                        COUNT(DISTINCT order_id))"
               :result-field "lift"}}])

  ;; Seasonal Decomposition
  (def seasonal-analysis-pipeline
    [{:formula "moving-window"
      :params {:value-field :sales
               :window-size 12  ;; 12 month window for yearly pattern
               :operation :avg}}
     {:formula "excel-formula"
      :params {:formula "=sales / window_value"
               :result-field "seasonal_factor"}}
     {:formula "excel-formula"
      :params {:formula "=AVERAGE(FILTER(seasonal_factor,
                                       MONTH(date) = MONTH(CURRENT(date))))"
               :result-field "seasonal_index"}}])

  ;; Customer Lifetime Value
  (def clv-pipeline
    [{:formula "excel-formula"
      :params {:formula "=DAYS_BETWEEN(first_purchase, last_purchase) / 365"
               :result-field "customer_age"}}
     {:formula "excel-formula"
      :params {:formula "=total_revenue / customer_age"
               :result-field "annual_value"}}
     {:formula "moving-window"
      :params {:value-field :annual_value
               :window-size 90  ;; 90 day trending
               :operation :avg}}
     {:formula "zscore-analysis"
      :params {:value-field :window_value
               :group-fields [:customer_segment]}}])

  ;; ABC Analysis (Pareto)
  (def abc-analysis-pipeline
    [{:formula "contribution-analysis"
      :params {:value-field :revenue
               :group-fields [:product_id]}}
     {:formula "excel-formula"
      :params {:formula "=IF(running_pct <= 0.8, 'A',
                          IF(running_pct <= 0.95, 'B', 'C'))"
               :result-field "abc_class"}}])

  ;; RFM Scoring
  (def rfm-pipeline
    [{:formula "excel-formula"
      :params {:formula "=DAYS_BETWEEN(CURRENT_DATE, last_purchase_date)"
               :result-field "recency_days"}}
     {:formula "zscore-analysis"
      :params {:value-field :recency_days}}
     {:formula "zscore-analysis"
      :params {:value-field :purchase_frequency}}
     {:formula "zscore-analysis"
      :params {:value-field :monetary_value}}
     {:formula "excel-formula"
      :params {:formula "=CONCAT(
                          IF(recency_zscore < -0.5, '3',
                             IF(recency_zscore < 0.5, '2', '1')),
                          IF(frequency_zscore > 0.5, '3',
                             IF(frequency_zscore > -0.5, '2', '1')),
                          IF(monetary_zscore > 0.5, '3',
                             IF(monetary_zscore > -0.5, '2', '1')))"
               :result-field "rfm_score"}}]))

  ;; Predictive Stock Reorder Points
  (def reorder-point-pipeline
    [{:formula "time-bucket"
      :params {:date-field :order_date
               :bucket-size :week}}
     {:formula "moving-window"
      :params {:value-field :quantity
               :window-size 12  ;; 12 weeks
               :operation :avg}}
     {:formula "zscore-analysis"
      :params {:value-field :quantity}}
     {:formula "excel-formula"
      :params {:formula "=window_value +
                        (zscore * SQRT(window_value) * safety_factor) +
                        (window_value * lead_time_days / 7)"
               :result-field "reorder_point"}}])

  ;; Customer Health Score
  (def customer-health-pipeline
    [{:formula "decay-score"
      :params {:value-field :interaction_score
               :date-field :interaction_date
               :half-life 14}}
     {:formula "moving-window"
      :params {:value-field :decay_score
               :window-size 90
               :operation :avg}}
     {:formula "relative-change"
      :params {:value-field :window_value
               :partition-fields [:customer_id]}}
     {:formula "excel-formula"
      :params {:formula "=IF(pct_change < -0.1, 'declining',
                          IF(pct_change > 0.1, 'growing', 'stable'))"
               :result-field "health_trend"}}])

  ;; Product Cannibalization Analysis
  (def cannibalization-pipeline
    [{:formula "time-bucket"
      :params {:date-field :launch_date
               :bucket-size :month}}
     {:formula "moving-window"
      :params {:value-field :sales
               :window-size 3
               :operation :avg}}
     {:formula "excel-formula"
      :params {:formula "=SUM(FILTER(sales,
                                   category = CURRENT(category) AND
                                   product_id != CURRENT(product_id) AND
                                   bucket_start = CURRENT(bucket_start)))"
               :result-field "category_others_sales"}}
     {:formula "relative-change"
      :params {:value-field :category_others_sales
               :partition-fields [:category]}}
     {:formula "excel-formula"
      :params {:formula "=IF(pct_change < -0.1 AND window_value > 1000,
                          'potential_cannibalization', 'normal')"
               :result-field "impact"}}])

  ;; Churn Risk Scoring
  (def churn-risk-pipeline
    [{:formula "decay-score"
      :params {:value-field :engagement_score
               :date-field :activity_date
               :half-life 7}}
     {:formula "moving-window"
      :params {:value-field :decay_score
               :window-size 30
               :operation :avg}}
     {:formula "excel-formula"
      :params {:formula "=DAYS_SINCE(last_activity)"
               :result-field "days_inactive"}}
     {:formula "zscore-analysis"
      :params {:value-field :days_inactive
               :group-fields [:customer_segment]}}
     {:formula "excel-formula"
      :params {:formula "=0.4 * (1 - window_value/100) +
                        0.4 * (days_inactive_zscore) +
                        0.2 * (1 - support_satisfaction/10)"
               :result-field "churn_risk"}}])

  ;; Marketing Campaign Attribution
  (def attribution-pipeline
    [{:formula "time-bucket"
      :params {:date-field :interaction_date
               :bucket-size :day}}
     {:formula "decay-score"
      :params {:value-field 1  ;; touchpoint weight
               :date-field :interaction_date
               :half-life 7}}
     {:formula "excel-formula"
      :params {:formula "=decay_score /
                        SUM(FILTER(decay_score,
                                 conversion_id = CURRENT(conversion_id)))"
               :result-field "attribution_weight"}}
     {:formula "contribution-analysis"
      :params {:value-field :attribution_weight
               :group-fields [:channel :campaign]}}])

  ;; Operational Efficiency Score
  (def efficiency-pipeline
    [{:formula "time-bucket"
      :params {:date-field :completion_date
               :bucket-size :day}}
     {:formula "excel-formula"
      :params {:formula "=completion_time / expected_time"
               :result-field "time_efficiency"}}
     {:formula "excel-formula"
      :params {:formula "=actual_cost / budgeted_cost"
               :result-field "cost_efficiency"}}
     {:formula "moving-window"
      :params {:value-field :time_efficiency
               :window-size 30
               :operation :avg}}
     {:formula "zscore-analysis"
      :params {:value-field :window_value
               :group-fields [:department]}}
     {:formula "excel-formula"
      :params {:formula "=(1/time_efficiency + 1/cost_efficiency) / 2 * 100"
               :result-field "efficiency_score"}}])



;; (ppl sample-data)

(comment

;; Example usage - mixing and matching carrots and bunches
  (def analysis-pipeline
    [;; Start with a single carrot
     {:formula "time-bucket"
      :params {:date-field :date
               :bucket-size :month}}

   ;; Use a predefined bunch
     "sales-seasonality"

   ;; Add another single carrot
  ;;  {:formula "distribution-analysis"
  ;;   :params {:value-field :window_result}}

   ;; Use another bunch
  ;;  "customer-value"

   ;; Even define a custom inline bunch
  ;;  {:name "Custom Analysis"
  ;;   :pipeline
  ;;   [{:formula "rank-analysis"
  ;;     :params {:value-field :running_total}}
  ;;    {:formula "zscore-analysis"
  ;;     :params {:value-field :rank_value}}]}
     ])

;; Execute mixed pipeline
  (execute-pipeline sample-data analysis-pipeline)

  (def test-data
    [{:id 1 :sales 100 :cost 80 :category "A" :tax 0.2 :discount 0.1}
     {:id 2 :sales 120 :cost 85 :category "A" :tax 0.2 :discount 0.15}
     {:id 3 :sales 90  :cost 70 :category "A" :tax 0.2 :discount 0.05}
     {:id 4 :sales 200 :cost 150 :category "B" :tax 0.15 :discount 0.2}
     {:id 5 :sales 180 :cost 140 :category "B" :tax 0.15 :discount 0.1}])

  (ppl (p/evaluate-formula "=(SUM(sales) + AVERAGE(cost)) / 2" test-data  :farts)))


