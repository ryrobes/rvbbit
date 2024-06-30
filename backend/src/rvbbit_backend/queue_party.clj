(ns rvbbit-backend.queue-party
  (:require [rvbbit-backend.util :as ut]
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
   :workers (atom {})
   :config (atom {:min-workers 1
                  :max-workers 5
                  :adjust-interval 60000})}) ; 60 seconds

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
  (let [{:keys [task-queues running]} system]
    (loop []
      (when @running
        (when-let [[queue _] (get-in @task-queues [queue-type id-keyword])]
          (when-let [task (.poll queue 100 TimeUnit/MILLISECONDS)]
            (swap! task-queues assoc-in [queue-type id-keyword 1] (System/currentTimeMillis))
            (task))))
      (recur))))

(defn- ensure-queue [system queue-type id-keyword]
  (swap! (:task-queues system) update-in [queue-type id-keyword]
         #(or % [(LinkedBlockingQueue.) (System/currentTimeMillis)])))

(defn- start-workers [system queue-type id-keyword num-workers]
  (swap! (:workers system) update-in [queue-type id-keyword]
         (fnil into [])
         (repeatedly num-workers #(future (worker-loop system queue-type id-keyword)))))

(defn- stop-workers [system queue-type id-keyword]
  (doseq [worker (get-in @(:workers system) [queue-type id-keyword])]
    (future-cancel worker))
  (swap! (:workers system) update queue-type dissoc id-keyword))

;; Update other functions to use safe-queue-size
(defn- adjust-workers [system queue-type id-keyword]
  (let [{:keys [task-queues workers config]} system
        {:keys [min-workers max-workers]} @config
        queue-pair (get-in @task-queues [queue-type id-keyword])
        queue-size (safe-queue-size queue-pair)
        current-workers (count (get-in @workers [queue-type id-keyword]))]
    (cond
      (and (> queue-size (* 2 current-workers)) (< current-workers max-workers))
      (start-workers system queue-type id-keyword (- (min (inc current-workers) max-workers) current-workers))

      (and (< queue-size (/ current-workers 2)) (> current-workers min-workers))
      (do
        (stop-workers system queue-type id-keyword)
        (start-workers system queue-type id-keyword (max (dec current-workers) min-workers))))))

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
  (when-not (get-in @(:workers system) [queue-type id-keyword])
    (swap! (:workers system) assoc-in [queue-type id-keyword]
           [(future (worker-loop system queue-type id-keyword))])))

(defn serial-slot-queue
  ([queue-type id-keyword task]
   (serial-slot-queue :default queue-type id-keyword task))
  ([system-id queue-type id-keyword task]
   (let [system (or (get @queue-systems system-id)
                    (let [new-system (create-queue-system)]
                      (swap! queue-systems assoc system-id new-system)
                      new-system))]
     (ensure-serial-queue system queue-type id-keyword)
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
     (reset! (:running system) false)
     (doseq [[queue-type queues] @(:workers system)
             [id-keyword workers] queues
             worker workers]
       (future-cancel worker))
     (swap! queue-systems dissoc system-id))))

(defn get-queue-stats+
  ([]
   (get-queue-stats+ :default))
  ([system-id]
   (try
     (when-let [system (get @queue-systems system-id)]
       (let [{:keys [task-queues workers]} system]
         {:overall {:total-queue-types (count @task-queues)
                    :total-queues (reduce + (map (comp count second) @task-queues))
                    :total-workers (reduce + (map (comp count second) @workers))
                    :total-tasks (reduce + (map (fn [[_ queues]]
                                                  (reduce + (map (fn [[queue _]] (.size queue)) queues)))
                                                @task-queues))}
          :by-type (into {}
                         (for [[queue-type queues] @task-queues]
                           [queue-type
                            {:queues (count queues)
                             :total-tasks (reduce + (map (fn [[queue _]] (.size queue)) queues))
                             :workers (count (get @workers queue-type))}]))}))
     (catch Exception e (println "get-queue-stats-error" e)))))

;; (defn get-queue-stats+
;;   ([]
;;    (get-queue-stats+ :default))
;;   ([system-id]
;;    (when-let [system (get @queue-systems system-id)]
;;      (let [{:keys [task-queues workers]} system
;;            by-type-map (into {}
;;                              (for [[queue-type queues] @task-queues]
;;                                [queue-type
;;                                 {:queues (count queues)
;;                                  :total-tasks (reduce + (map #(.size %) (vals queues)))
;;                                  :workers (count (get @workers queue-type))}]))]
;;        {:overall {:total-queue-types (count @task-queues)
;;                   :total-queues (reduce + (map (comp count second) @task-queues))
;;                   :total-tasks (apply + (for [[_ v] by-type-map] (get v :total-tasks)))
;;                   :total-workers (reduce + (map (comp count second) @workers))}
;;         :by-type by-type-map}))))

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





(defn get-queue-stats+
  ([]
   (get-queue-stats+ :default))
  ([system-id]
   (when-let [system (get @queue-systems system-id)]
     (let [{:keys [task-queues workers]} system]
       {:overall {:total-queue-types (count @task-queues)
                  :total-queues (reduce + (map (comp count second) @task-queues))
                  :total-workers (reduce + (map (comp count second) @workers))
                  :total-tasks (reduce + (map (fn [[_ queues]]
                                                (reduce + (map safe-queue-size queues)))
                                              @task-queues))}
        :by-type (into {}
                       (for [[queue-type queues] @task-queues]
                         [queue-type
                          {:queues (count queues)
                           :total-tasks (reduce + (map safe-queue-size queues))
                           :workers (count (get @workers queue-type))}]))}))))



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


