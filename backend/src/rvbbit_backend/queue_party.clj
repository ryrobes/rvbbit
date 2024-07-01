(ns rvbbit-backend.queue-party
  (:require [rvbbit-backend.util :as ut]
            [clojure.string      :as cstr])
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

(def queue-stats-history (atom {:timestamp []
                                :total-queues []
                                :total-queued []
                                :total-workers []
                                :total-tasks []
                                :total-resizes []}))

(def ^:private queue-systems (atom {}))

(defn create-queue-system []
  {:task-queues (atom {})
   :running (atom true)
   :stopping (atom false) 
   :workers (atom {})
   :active-tasks (atom {})
   :last-scaled (atom {})
   :resize-counts (atom {})
   :config (atom {:min-workers 1
                  :max-workers 20
                  :adjust-interval 5000
                  :scale-up-factor 2.0
                  :scale-down-factor 0.5
                  :cooldown-period 20000 ;; 45000
                  :serial-queues {}})})

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
    (let [{:keys [task-queues workers config active-tasks resize-counts]} system
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
          (start-workers system queue-type id-keyword 1)
          (swap! resize-counts update-in [queue-type id-keyword] (fnil inc 0)))
      ;; For non-serial queues, apply scaling logic with cooldown
        (when (and queue-pair
                   (pos? current-workers)
                   (> (- current-time last-scaled-time) cooldown-period))
          (cond
            (and (> total-tasks (* scale-up-factor current-workers)) (< current-workers max-workers))
            (let [boost 0
                  new-workers (min (+ boost (inc current-workers)) max-workers)]
              ;(ut/pp [:*scaling-workers-up queue-type id-keyword current-workers :> new-workers])
              (start-workers system queue-type id-keyword (- new-workers current-workers))
              (swap! (:last-scaled system) assoc-in [queue-type id-keyword] current-time)
              (swap! resize-counts update-in [queue-type id-keyword] (fnil inc 0)))

            (and (< total-tasks (* scale-down-factor current-workers)) (> current-workers min-workers))
            (let [new-workers (max (dec current-workers) min-workers)]
              ;(ut/pp [:*scaling-workers-down queue-type id-keyword current-workers :> new-workers])
              (let [workers-to-remove (- current-workers new-workers)]
                (stop-workers system queue-type id-keyword workers-to-remove))
              (swap! (:last-scaled system) assoc-in [queue-type id-keyword] current-time)
              (swap! resize-counts update-in [queue-type id-keyword] (fnil inc 0)))))))))

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


(defn configure-slot-queue-system
  ([config]
   (configure-slot-queue-system :default config))
  ([system-id config]
   (when-let [system (get @queue-systems system-id)]
     (swap! (:config system) merge config))))

(defn update-queue-stats-history
  ([]
   (update-queue-stats-history :default))
  ([system-id]
   (when-let [system (get @queue-systems system-id)]
     (let [{:keys [task-queues workers active-tasks resize-counts]} system
           current-time (System/currentTimeMillis)
           total-queues (reduce + (map (comp count second) @task-queues))
           total-workers (reduce + (map (fn [[_ queues]] (reduce + (map count (vals queues)))) @workers))
           total-queued (reduce + (map (fn [[_ queues]] (reduce + (map safe-queue-size (vals queues)))) @task-queues))
           total-tasks (+ total-queued
                          (reduce + (map (fn [[_ queues]] (reduce + (map second queues))) @active-tasks)))
           total-resizes (reduce + (map (fn [[_ queues]] (reduce + (vals queues))) @resize-counts))]

       (swap! queue-stats-history
              (fn [history]
                (-> history
                    (update :timestamp conj current-time)
                    (update :total-queues conj total-queues)
                    (update :total-queued conj total-queued)
                    (update :total-workers conj total-workers)
                    (update :total-tasks conj total-tasks)
                    (update :total-resizes conj total-resizes))))))))

(defn get-queue-stats+
  ([]
   (get-queue-stats+ :default))
  ([system-id]
   (when-let [system (get @queue-systems system-id)]
     (let [{:keys [task-queues workers active-tasks resize-counts]} system
           total-queue-types (count @task-queues)
           total-queues (reduce + (map (comp count second) @task-queues))
           worker-counts (map (fn [[_ queues]] (reduce + (map count (vals queues)))) @workers)
           total-workers (reduce + worker-counts)
           queued-tasks (reduce + (map (fn [[_ queues]] (reduce + (map safe-queue-size (vals queues)))) @task-queues))
           active-task-counts (map (fn [[_ queues]] (reduce + (map second queues))) @active-tasks)
           total-active-tasks (reduce + active-task-counts)
           total-tasks (+ queued-tasks total-active-tasks)
           total-resizes (reduce + (map (fn [[_ queues]] (reduce + (vals queues))) @resize-counts))
           by-type (into {}
                         (for [[queue-type queues] @task-queues
                               :let [active-count (reduce + (map second (get @active-tasks queue-type {})))
                                     worker-count (reduce + (map count (vals (get @workers queue-type {}))))
                                     resize-count (reduce + (vals (get @resize-counts queue-type {})))
                                     queued-count (reduce + (map safe-queue-size (vals queues)))]]
                           [queue-type
                            {:queues (count queues)
                             :total-tasks (+ queued-count active-count)
                             :queued-tasks queued-count
                             :active-tasks active-count
                             :workers worker-count
                             :diff (format "%+d" (- worker-count (count queues)))
                             :resizes resize-count}]))]
       {:overall {:total-queue-types total-queue-types
                  :total-queues total-queues
                  :total-workers total-workers
                  :total-tasks total-tasks
                  :diff (format "%+d" (- total-workers total-queues))
                  :total-resizes total-resizes}
        :by-type by-type}))))

;; (defn cleanup-inactive-queues
;;   ([minutes]
;;    (cleanup-inactive-queues :default minutes))
;;   ([system-id minutes]
;;    (when-let [system (get @queue-systems system-id)]
;;      (let [{:keys [task-queues workers active-tasks last-scaled]} system
;;            current-time (System/currentTimeMillis)
;;            inactive-threshold (* minutes 60 1000) ; Convert minutes to milliseconds
;;            queues-to-remove (atom {})]

;;        ;; Identify inactive queues
;;        (doseq [[queue-type queues] @task-queues]
;;          (doseq [[id-keyword [queue last-access-time]] queues]
;;            (when (> (- current-time last-access-time) inactive-threshold)
;;              (swap! queues-to-remove update queue-type (fnil conj #{}) id-keyword))))

;;        ;; Remove inactive queues and associated resources
;;        (doseq [[queue-type ids] @queues-to-remove]
;;          (doseq [id ids]
;;            (try
;;              ;; Remove from task-queues
;;              (swap! task-queues update queue-type dissoc id)

;;              ;; Stop and remove workers
;;              (when-let [queue-workers (get-in @workers [queue-type id])]
;;                (doseq [worker queue-workers]
;;                  (when (future? worker)
;;                    (future-cancel worker)))
;;                (swap! workers update queue-type dissoc id))

;;              ;; Remove from active-tasks
;;              (swap! active-tasks update queue-type dissoc id)

;;              ;; Remove from last-scaled
;;              (swap! last-scaled update queue-type dissoc id)

;;              (catch Exception e
;;                (println "Error cleaning up queue:" queue-type id)
;;                (.printStackTrace e)))))

;;        ;; Return the number of removed queues
;;        (let [removed-count (reduce + (map count (vals @queues-to-remove)))]
;;          (ut/pp [:queue-party-housekeeping :cleaned-up removed-count :inactive-queues])
;;          removed-count)))))

(defn cleanup-inactive-queues
  ([minutes]
   (cleanup-inactive-queues :default minutes))
  ([system-id minutes]
   (when-let [system (get @queue-systems system-id)]
     (let [{:keys [task-queues workers active-tasks last-scaled]} system
           current-time (System/currentTimeMillis)
           inactive-threshold (* minutes 60 1000) ; Convert minutes to milliseconds
           queues-to-remove (atom {})]

       ;; Identify inactive queues
       (doseq [[queue-type queues] @task-queues]
         (doseq [[id-keyword [queue last-access-time]] queues]
           (when (> (- current-time last-access-time) inactive-threshold)
             (swap! queues-to-remove update queue-type (fnil conj #{}) id-keyword))))

       ;; Remove inactive queues and associated resources
       (doseq [[queue-type ids] @queues-to-remove]
         (doseq [id ids]
           (try
             ;; Remove from task-queues
             (swap! task-queues update queue-type dissoc id)

             ;; Stop and remove workers
             (when-let [queue-workers (get-in @workers [queue-type id])]
               (doseq [worker queue-workers]
                 (when (future? worker)
                   (future-cancel worker)))
               (swap! workers update queue-type dissoc id))

             ;; Remove from active-tasks
             (swap! active-tasks update queue-type dissoc id)

             ;; Remove from last-scaled
             (swap! last-scaled update queue-type dissoc id)

             (catch Exception e
               (println "Error cleaning up queue:" queue-type id)
               (.printStackTrace e)))))

       ;; Return the number of removed queues
       (let [removed-count (reduce + (map count (vals @queues-to-remove)))]
         (ut/pp [:queue-party-housekeeping :cleaned-up removed-count :inactive-queues])
         removed-count)))))


