(ns rvbbit-backend.pool-party
  (:require [clojure.string         :as cstr]
            [clojure.data           :as data]
            [clojure.core.memoize   :as memoize]
            [rvbbit-backend.queue-party :as qp]
            [rvbbit-backend.util    :as ut])
  (:import
   [java.util.concurrent                  Executors ThreadPoolExecutor SynchronousQueue TimeUnit TimeoutException ArrayBlockingQueue ThreadPoolExecutor$CallerRunsPolicy]
   [java.lang                              ProcessBuilder]
   [java.io                                BufferedReader InputStreamReader]))



(defonce general-scheduler-thread-pool
  (Executors/newScheduledThreadPool 8))

;; (defonce websocket-thread-pool
;;   (ThreadPoolExecutor. 100 300 60 TimeUnit/SECONDS (SynchronousQueue.) (ThreadPoolExecutor$CallerRunsPolicy.)))

(defonce websocket-thread-pool-atom
  (atom (ThreadPoolExecutor. 100 300 60 TimeUnit/SECONDS (SynchronousQueue.) (ThreadPoolExecutor$CallerRunsPolicy.))))

(defn reboot-websocket-thread-pool! []
  (let [old-pool @websocket-thread-pool-atom
        new-pool (ThreadPoolExecutor. 100 300 60 TimeUnit/SECONDS (SynchronousQueue.) (ThreadPoolExecutor$CallerRunsPolicy.))]
    (.shutdownNow old-pool)  ; Attempt to stop all actively executing tasks
    (reset! websocket-thread-pool-atom new-pool)
    (println "Websocket thread pool rebooted")))

;; (defonce general-scheduler-thread-pool
;;   (ThreadPoolExecutor. 4 50 60 TimeUnit/SECONDS (SynchronousQueue.) (ThreadPoolExecutor$CallerRunsPolicy.)))

  ;; (defonce custom-watcher-thread-master-pool 
  ;;   (Executors/newFixedThreadPool 32))
;; (defonce custom-watcher-thread-master-pool ;; master watchers
;;   (ThreadPoolExecutor. 10 50 60 TimeUnit/SECONDS (SynchronousQueue.) (ThreadPoolExecutor$CallerRunsPolicy.)))
  ;; (defonce custom-watcher-thread-master-pool
  ;;   (ThreadPoolExecutor. 4 50 60 TimeUnit/SECONDS (LinkedBlockingQueue.)))

;; (defonce custom-watcher-thread-pool
;;   (ThreadPoolExecutor. 10 1000 60 TimeUnit/SECONDS (SynchronousQueue.)))

;; (defonce custom-watcher-thread-pool ;; if pool is 100% full, will fallback to parent callers thread pool instead of rejection
;;   (ThreadPoolExecutor. 10 2000 60 TimeUnit/SECONDS (SynchronousQueue.) (ThreadPoolExecutor$CallerRunsPolicy.)))  



;; simple cached thread pool worx

(defonce dyn-pools (atom {}))
(defonce pool-exec-times (atom {}))
(defonce thread-pool-timing? true)

;; (defn create-cached-thread-pool [name]
;;   (cond
;;     (cstr/includes? (str name) "serial")
;;     (ThreadPoolExecutor. 1 1 60 TimeUnit/SECONDS (java.util.concurrent.LinkedBlockingQueue.))

;;     (= name :watchers/signal)
;;     ;;(ThreadPoolExecutor. 10 1000 15 TimeUnit/SECONDS  (ArrayBlockingQueue. 200) (ThreadPoolExecutor$CallerRunsPolicy.))
;;     (ThreadPoolExecutor. 3 1000 600 TimeUnit/SECONDS (SynchronousQueue.) (ThreadPoolExecutor$CallerRunsPolicy.))

;;     (or (cstr/includes? (str name) "flow-runner/")
;;         (cstr/includes? (str name) "nrepl-eval/")
;;         (cstr/includes? (str name) "query-runstream/")
;;         (cstr/includes? (str name) "watchers/")
;;         ;(cstr/includes? (str name) "subscriptions/")
;;         )
;;     (ThreadPoolExecutor. 3 1000 600 TimeUnit/SECONDS (SynchronousQueue.) (ThreadPoolExecutor$CallerRunsPolicy.))
;;     ;(ThreadPoolExecutor. 3 1000 120 TimeUnit/SECONDS  (ArrayBlockingQueue. 20) (ThreadPoolExecutor$CallerRunsPolicy.)) ;;; was 10 300 60

;;     :else (ThreadPoolExecutor. 1 1000 600 TimeUnit/SECONDS (SynchronousQueue.) (ThreadPoolExecutor$CallerRunsPolicy.))))


(defn create-cached-thread-pool [name]
  (let [base-pool (cond
                    (cstr/includes? (str name) "serial")
                    (ThreadPoolExecutor. 1 1 60 TimeUnit/SECONDS (java.util.concurrent.LinkedBlockingQueue.))

                    (= name :watchers/signal)
                    (ThreadPoolExecutor. 3 1000 10 TimeUnit/SECONDS (SynchronousQueue.) (ThreadPoolExecutor$CallerRunsPolicy.))

                    (= name :query-runstreams)
                    (ThreadPoolExecutor. 3 1000 120 TimeUnit/SECONDS  (ArrayBlockingQueue. 20) (ThreadPoolExecutor$CallerRunsPolicy.))

                    (or (cstr/includes? (str name) "flow-runner/")
                        (cstr/includes? (str name) "nrepl-eval/")
                        (cstr/includes? (str name) "query-runstream/")
                        (cstr/includes? (str name) "watchers/"))
                    (ThreadPoolExecutor. 3 1000 10 TimeUnit/SECONDS (SynchronousQueue.) (ThreadPoolExecutor$CallerRunsPolicy.))

                    :else (ThreadPoolExecutor. 1 1000 10 TimeUnit/SECONDS (SynchronousQueue.) (ThreadPoolExecutor$CallerRunsPolicy.)))
        thread-factory (proxy [java.util.concurrent.ThreadFactory] []
                         (newThread [r]
                           (let [thread (java.lang.Thread. r)]
                             (.setName thread (str name "-" (.getName thread)))
                             thread)))]
    (.setThreadFactory base-pool thread-factory)
    base-pool))





;; for later to test with:
;; (defn create-adaptive-pool [] 
;;   (Executors/newWorkStealingPool))

;; (defn create-bounded-pool []
;;   (ThreadPoolExecutor. 1 100 60 TimeUnit/SECONDS  (ArrayBlockingQueue. 1000) (ThreadPoolExecutor$CallerRunsPolicy.)))


;; (defn get-or-create-cached-thread-pool [name]
;;   (if-let [queue (@dyn-pools name)]
;;     queue
;;     (let [new-queue (create-cached-thread-pool name)]
;;       (ut/pp [:*creating-thread-pool-for name])
;;       (swap! dyn-pools assoc name new-queue)
;;       new-queue)))

(defn get-or-create-cached-thread-pool [name]
  (if-let [queue (@dyn-pools name)]
    queue
    (locking dyn-pools
      ;; busy busy. bobs burgers.
      (if-let [queue (@dyn-pools name)]
        queue
        (let [new-queue (create-cached-thread-pool name)]
          (ut/pp [:*creating-thread-pool-for name])
          (swap! dyn-pools assoc name new-queue)
          new-queue)))))

;; (def get-or-create-cached-thread-pool-memoized
;;   (memoize/memo get-or-create-cached-thread-pool))

(defn close-cached-thread-pool [name]
  (if-let [queue (@dyn-pools name)]
    (do
      (.shutdownNow queue) ;; or (.shutdown queue) ;; but tasks waiting for a dead client are pointless to wait for generally
      (swap! dyn-pools dissoc name)
      (ut/pp [:*closed-thread-pool-for name]))
    (ut/pp [:*no-thread-pool-found-for name])))

(defn reset-cached-thread-pool [name]
  ;; First, close the existing thread pool if it exists
  (close-cached-thread-pool name)
  ;; Then, create and return a new thread pool with the same name
  (get-or-create-cached-thread-pool name))

(defn reset-cached-thread-pools-wildcard [wildcard]
  (doseq [[name _] (filter #(cstr/includes? (str (key %) " ") wildcard) @dyn-pools)]
    (reset-cached-thread-pool name)))

;; (defn execute-in-thread-pools [base-type f]
;;   (try
;;     (.execute (get-or-create-cached-thread-pool base-type) f)
;;     (catch Throwable e (ut/pp [:pool-execute-on base-type :failed (str e)]))))

;; (defn execute-in-thread-pools [base-type f]
;;   (.execute (get-or-create-cached-thread-pool base-type) f))

(def pool-tasks-run (atom 0))

(defn add-exec-time [pool-name exec-time]
  (swap! pool-exec-times
         (fn [times]
           (update times pool-name (fn [existing]
                                     (if existing
                                       (conj existing exec-time)
                                       [exec-time]))))))

(defn execute-in-thread-pools [base-type f]
  (swap! pool-tasks-run inc)
  (if thread-pool-timing?
    (let [start-time (System/nanoTime)]
      (try
        (.execute (get-or-create-cached-thread-pool base-type)
                  (fn []
                    (try
                      (f)
                      (finally
                        (let [end-time (System/nanoTime)
                              exec-time (/ (- end-time start-time) 1e6)] ;; convert nano to milli
                          (add-exec-time base-type exec-time))))))
        (catch Throwable e (ut/pp [:timed-pool-execute-on base-type :failed (str e)]))))
    (try
      (.execute (get-or-create-cached-thread-pool base-type) f)
      (catch Throwable e (ut/pp [:pool-execute-on base-type :failed (str e)])))))

(defn execute-in-thread-pools-but-deliver [base-type f]
  (swap! pool-tasks-run inc)
  (let [p (promise)]
    (if thread-pool-timing?
      (let [start-time (System/nanoTime)]
        (try
          (.execute (get-or-create-cached-thread-pool base-type)
                    (fn []
                      (try
                        (deliver p (f))
                        (finally
                          (let [end-time (System/nanoTime)
                                exec-time (/ (- end-time start-time) 1e6)]
                            (add-exec-time base-type exec-time))))))
          (catch Throwable e
            (ut/pp [:timed-pool-execute-on base-type :failed (str e)])
            (deliver p (throw e)))))  ; Re-throw the exception if thread pool execution fails
      (try
        (.execute (get-or-create-cached-thread-pool base-type)
                  #(deliver p (f)))
        (catch Throwable e
          (ut/pp [:pool-execute-on base-type :failed (str e)])
          (deliver p (throw e)))))  ; Re-throw the exception if thread pool execution fails
    @p))  ; Dereference the promise to return the result or propagate the exception


;;(declare break-up-flow-key-ext)

(defn wrap-custom-watcher-pool [watcher-fn base-type client-name flow-key]
  (fn [key ref old-state new-state]
;; test, dont bother taking the next step if nothing changed...? less thrash, does shit still work?
    (when (not= old-state new-state)
      (when (let [;[added removed _] (clojure.data/diff old-state new-state)
                  ;akp (ut/keypaths added)
                  ;rkp (ut/keypaths removed)
                  ;changed-kp (vec (distinct (into akp rkp)))
                  changed-kp (ut/diff-keypaths old-state new-state)]
              (ut/ne? changed-kp))
        (execute-in-thread-pools
         (keyword (str (if client-name
                         "subscriptions/"
                         "watchers/")
                       (cstr/replace (str base-type) ":" "")
                      ;(when client-name (str "." (cstr/replace (str client-name) ":" "")))
                       ;(when client-name ".*")
                       ))
         (fn []
           (try
             (watcher-fn key ref old-state new-state)
             (catch Exception e
               (ut/pp [:error-in-wrap-custom-watcher-pool-exec key (.getMessage e)])
               ;(throw e)
               ))))))))

;; (defn wrap-custom-watcher-pool [watcher-fn base-type client-name flow-key]
;;   (fn [key ref old-state new-state]
;;     (when (not= old-state new-state)
;;       (let [diff (clojure.data/diff old-state new-state)
;;             changed-kp (into #{} (comp (take 2)
;;                                        (remove nil?)
;;                                        (mapcat ut/keypaths)
;;                                        (distinct))
;;                              diff)]
;;         (when (seq changed-kp)
;;           (execute-in-thread-pools
;;            (keyword (str (if client-name
;;                            "subscriptions/"
;;                            "watchers/") 
;;                          (cstr/replace (str base-type) ":" "")
;;                           ;(when client-name (str "." (cstr/replace (str client-name) ":" "")))
;;                          (when client-name ".*")))
;;            (fn []
;;              (try
;;                (watcher-fn key ref old-state new-state)
;;                (catch Exception e
;;                  (ut/pp [:error-in-wrap-custom-watcher-pool-exec key e])
;;                  ;;(throw e)
;;                  )))))))))


(defn add-watch+ [atom key watcher-fn base-type & [client-name flow-key]]
  (let [;base-type (if false ;(= base-type :solver-status)
        ;            (keyword (str (cstr/replace (str base-type) ":" "") "/"
        ;                          (cstr/replace (str client-name) ":" "")))
        ;            ;;(keyword (str "client/" (cstr/replace (str client-name) ":" ""))) ;client-name
        ;            base-type)
        ;; all-clients-subbed (for [c     (keys @atoms-and-watchers)
        ;;                                         :when (some #(= % flow-key) (keys (get @atoms-and-watchers c)))]
        ;;                                     c)
        ;; _(ut/pp [:rvbbit-watching flow-key :for (count all-clients-subbed) :client-subs ])
        ;; base-type (if (and (cstr/includes? (str flow-key) ">*") (= base-type :flow)) :flow-status base-type)
        wrapped-watcher (wrap-custom-watcher-pool watcher-fn base-type client-name flow-key)]
    (add-watch atom key wrapped-watcher)))
