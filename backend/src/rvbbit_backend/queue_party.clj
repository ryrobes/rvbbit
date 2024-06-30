(ns rvbbit-backend.queue-party
  (:require [rvbbit-backend.util :as ut]
            [zprint.core       :as zp]
            [clojure.string :as cstr])
  (:import (java.util.concurrent LinkedBlockingQueue TimeUnit)))

;; ;; queue party v0 usage 

;; ;; start
;; ;; (queue-party/create-slot-queue-system)

;; ;; give it shit to do
;; ;; (queue-party/slot-queue :client-tasks :client1 #(println "Processing task for client1"))

;; ;; modify resources
;; ;; (queue-party/configure-slot-queue-system {:min-workers 2
;; ;;                                           :max-workers 10
;; ;;                                           :adjust-interval 30000})

;; ;; stats
;; ;; (queue-party/get-queue-stats)

;; ;; shut it down
;; ;; (queue-party/stop-slot-queue-system)


(def ^:private queue-systems (atom {}))

(defn create-queue-system []
  {:task-queues (atom {})
   :running (atom true)
   :stopping (atom false) 
   :workers (atom {})
   :active-tasks (atom {})
   :last-scaled (atom {})
   :config (atom {:min-workers 1
                  :max-workers 5
                  :adjust-interval 10000
                  :scale-up-factor 1.6
                  :scale-down-factor 0.3
                  :cooldown-period 45000
                  :serial-queues {}})}) ; New key for tracking serial queues

;; Helper functions for safe queue access
(defn- get-queue [queue-pair]
  (when (vector? queue-pair)
    (first queue-pair)))

(defn- safe-queue-size [queue-pair]
  (if-let [queue (get-queue queue-pair)]
    (if (instance? LinkedBlockingQueue queue)
      (.size queue)
      0)
    0))

(defn- worker-loop [system queue-type id-keyword]
  (let [{:keys [task-queues running stopping active-tasks]} system]
    (loop []
      (when (and @running (not @stopping))
        (when-let [[queue _] (get-in @task-queues [queue-type id-keyword])]
          (when-let [task (.poll queue 100 TimeUnit/MILLISECONDS)]
            (swap! task-queues assoc-in [queue-type id-keyword 1] (System/currentTimeMillis))
            (swap! active-tasks update-in [queue-type id-keyword] (fnil inc 0))
            (try
              (task)
              (catch Exception e
                (println "Error executing task:" (.getMessage e)))
              (finally
                (swap! active-tasks update-in [queue-type id-keyword] dec))))))
      (when (and @running (not @stopping))
        (recur)))))


(defn- ensure-queue [system queue-type id-keyword]
  (swap! (:task-queues system) update-in [queue-type id-keyword]
         #(or % [(LinkedBlockingQueue.) (System/currentTimeMillis)])))

(defn- start-workers [system queue-type id-keyword num-workers]
  (swap! (:workers system) update-in [queue-type id-keyword]
         (fn [current-workers]
           (into (or current-workers [])
                 (repeatedly num-workers #(future (worker-loop system queue-type id-keyword)))))))

(defn- stop-workers [system queue-type id-keyword & [num-workers-to-remove]]
  (let [current-workers (get-in @(:workers system) [queue-type id-keyword])
        num-to-remove (or num-workers-to-remove (count current-workers))
        workers-to-keep (- (count current-workers) num-to-remove)
        [workers-to-stop workers-to-keep] (split-at num-to-remove current-workers)]
    (doseq [worker workers-to-stop]
      (future-cancel worker))
    (swap! (:workers system) assoc-in [queue-type id-keyword] (vec workers-to-keep))))

(defn- is-serial-queue? [system queue-type id-keyword]
  (get-in @(:config system) [:serial-queues queue-type id-keyword] false))

(defn- adjust-workers [system queue-type id-keyword]
  (when-not @(:stopping system)
   (let [{:keys [task-queues workers config active-tasks]} system
        {:keys [min-workers max-workers scale-up-factor scale-down-factor cooldown-period]} @config
        queue-pair (get-in @task-queues [queue-type id-keyword])
        queued-tasks (safe-queue-size queue-pair)
        active-tasks-count (get-in @active-tasks [queue-type id-keyword] 0)
        total-tasks (+ queued-tasks active-tasks-count)
        current-workers (count (get-in @workers [queue-type id-keyword] []))
        last-scaled-time (get-in @(:last-scaled system) [queue-type id-keyword] 0)
        current-time (System/currentTimeMillis)]
    (if (is-serial-queue? system queue-type id-keyword)
      ;; For serial queues, ensure exactly one worker
      (when (not= current-workers 1)
        (stop-workers system queue-type id-keyword)
        (start-workers system queue-type id-keyword 1))
      ;; For non-serial queues, apply scaling logic with cooldown
      (when (and queue-pair
                 (pos? current-workers)
                 (> (- current-time last-scaled-time) cooldown-period))
        (cond
          (and (> total-tasks (* scale-up-factor current-workers)) (< current-workers max-workers))
          (let [new-workers (min (inc current-workers) max-workers)]
            (ut/pp [:*scaling-workers-up queue-type id-keyword current-workers :> new-workers])
            (start-workers system queue-type id-keyword (- new-workers current-workers))
            (swap! (:last-scaled system) assoc-in [queue-type id-keyword] current-time))

          (and (< total-tasks (* scale-down-factor current-workers)) (> current-workers min-workers))
          (let [new-workers (max (dec current-workers) min-workers)]
            (ut/pp [:*scaling-workers-down queue-type id-keyword current-workers :> new-workers])
            (let [workers-to-remove (- current-workers new-workers)]
              (stop-workers system queue-type id-keyword workers-to-remove))
            (swap! (:last-scaled system) assoc-in [queue-type id-keyword] current-time))))))))

(defn slot-queue
  ([queue-type id-keyword task]
   (slot-queue :default queue-type id-keyword task))
  ([system-id queue-type id-keyword task]
   (let [system (or (get @queue-systems system-id)
                    (let [new-system (create-queue-system)]
                      (swap! queue-systems assoc system-id new-system)
                      new-system))]
     (ensure-queue system queue-type id-keyword)
     (let [[queue _] (get-in @(:task-queues system) [queue-type id-keyword])]
       (.offer queue task)
       (swap! (:task-queues system) assoc-in [queue-type id-keyword 1] (System/currentTimeMillis)))
     (when-not (get-in @(:workers system) [queue-type id-keyword])
       (start-workers system queue-type id-keyword (get-in @(:config system) [:min-workers])))
     system)))

(defn- ensure-serial-queue [system queue-type id-keyword]
  (ensure-queue system queue-type id-keyword)
  (let [current-workers (get-in @(:workers system) [queue-type id-keyword])]
    (when (not= (count current-workers) 1)
      (stop-workers system queue-type id-keyword)
      (swap! (:workers system) assoc-in [queue-type id-keyword]
             [(future (worker-loop system queue-type id-keyword))]))))

(defn serial-slot-queue
  ([queue-type id-keyword task]
   (serial-slot-queue :default queue-type id-keyword task))
  ([system-id queue-type id-keyword task]
   (let [system (or (get @queue-systems system-id)
                    (let [new-system (create-queue-system)]
                      (swap! queue-systems assoc system-id new-system)
                      new-system))]
     (ensure-serial-queue system queue-type id-keyword)
     (swap! (:config system) assoc-in [:serial-queues queue-type id-keyword] true)
     (let [[queue _] (get-in @(:task-queues system) [queue-type id-keyword])]
       (.offer queue task)
       (swap! (:task-queues system) assoc-in [queue-type id-keyword 1] (System/currentTimeMillis)))
     system)))

(defn create-slot-queue-system
  ([]
   (create-slot-queue-system :default))
  ([system-id]
   (let [system (create-queue-system)]
     (swap! queue-systems assoc system-id system)

     ;; Start a background thread for worker adjustment
     (future
       (while @(:running system)
         (doseq [[queue-type queues] @(:task-queues system)
                 [id-keyword _] queues]
           (adjust-workers system queue-type id-keyword))
         (Thread/sleep (get-in @(:config system) [:adjust-interval]))))

     system)))

(defn stop-slot-queue-system
  ([]
   (stop-slot-queue-system :default))
  ([system-id]
   (when-let [system (get @queue-systems system-id)]
     (reset! (:stopping system) true)
     (ut/pp ["Queue system" system-id "is stopping. No new tasks will be processed."]))))

(defn cleanup-unused-queues
  ([]
   (cleanup-unused-queues :default))
  ([system-id]
   (when-let [system (get @queue-systems system-id)]
     (let [{:keys [task-queues workers]} system
           current-time (System/currentTimeMillis)
           cleanup-threshold (* 60 60 1000) ; older than 1 hour
           queues-to-remove (atom {})]
       (doseq [[queue-type queues] @task-queues]
         (doseq [[id-keyword [queue last-access-time]] queues]
           (when (and (.isEmpty queue)
                      (> (- current-time last-access-time) cleanup-threshold))
             (swap! queues-to-remove update queue-type (fnil conj #{}) id-keyword))))
       (doseq [[queue-type ids] @queues-to-remove]
         (doseq [id ids]
           (swap! task-queues update queue-type dissoc id)
           (swap! workers update queue-type dissoc id)
           (when-let [worker (first (get-in @workers [queue-type id]))]
             (future-cancel worker))))
       (reduce + (map count (vals @queues-to-remove)))))))

(defn configure-slot-queue-system
  ([config]
   (configure-slot-queue-system :default config))
  ([system-id config]
   (when-let [system (get @queue-systems system-id)]
     (swap! (:config system) merge config))))

(defn zp-stats [s]
  (let [s (pr-str s)
        o (zp/zprint-str s
                         (ut/get-terminal-width)
                         {:parse-string-all? true ;; was :parse-string-all?
                          ;:color?        true
                          :style         [:justified-original] ;;type ;:community ;[:binding-nl :extend-nl]
                          :pair          {:force-nl? false}
                          :map {:hang? true :comma? false :sort? false}
                          :pair-fn {:hang? true}
                          :binding       {:force-nl? true}
                          :vector        {:respect-nl? true}
                          :parse         {:interpose "\n\n"}})]
    o))

(defn get-queue-stats+
  ([]
   (get-queue-stats+ :default))
  ([system-id]
   (when-let [system (get @queue-systems system-id)]
     (let [{:keys [task-queues workers active-tasks]} system]
       {:overall {:total-queue-types (count @task-queues)
                  :total-queues (reduce + (map (comp count second) @task-queues))
                  :total-workers (reduce + (map (fn [[_ queues]]
                                                  (reduce + (map count (vals queues))))
                                                @workers))
                  :total-tasks (+ (reduce + (map (fn [[_ queues]]
                                                   (reduce + (map safe-queue-size (vals queues))))
                                                 @task-queues))
                                  (reduce + (map (fn [[_ queues]]
                                                   (reduce + (map (fn [[_ count]] count) queues)))
                                                 @active-tasks)))
                  :diff (format "%+d" (- (reduce + (map (fn [[_ queues]]
                                                        (reduce + (map count (vals queues))))
                                                      @workers))
                                         (reduce + (map (comp count second) @task-queues))))}
        :by-type (into {}
                       (for [[queue-type queues] @task-queues
                             :let [active-count (reduce + (map second (get @active-tasks queue-type {})))
                                   worker-count (reduce + (map count (vals (get @workers queue-type {}))))]]
                         [queue-type
                          {:queues (count queues)
                           :total-tasks (+ (reduce + (map safe-queue-size (vals queues)))
                                           active-count)
                           :queued-tasks (reduce + (map safe-queue-size (vals queues)))
                           :active-tasks active-count
                           :workers worker-count
                           :diff (format "%+d" (- worker-count (count queues)))}]))}))))

(defn cleanup-unused-queues
  ([]
   (cleanup-unused-queues :default))
  ([system-id]
   (when-let [system (get @queue-systems system-id)]
     (let [{:keys [task-queues workers]} system
           current-time (System/currentTimeMillis)
           cleanup-threshold (* 60 60 1000) ; 1 hour in milliseconds
           queues-to-remove (atom {})]
       (doseq [[queue-type queues] @task-queues]
         (doseq [[id-keyword queue-pair] queues]
           (when (and (zero? (safe-queue-size queue-pair))
                      (> (- current-time (second queue-pair)) cleanup-threshold))
             (swap! queues-to-remove update queue-type (fnil conj #{}) id-keyword))))
       (doseq [[queue-type ids] @queues-to-remove]
         (doseq [id ids]
           (swap! task-queues update queue-type dissoc id)
           (swap! workers update queue-type dissoc id)
           (when-let [worker (first (get-in @workers [queue-type id]))]
             (future-cancel worker))))
       (reduce + (map count (vals @queues-to-remove)))))))


