(ns rvbbit-backend.websockets
  (:require [clojure.core.async :as async :refer [<! >! <!! >!! go chan]]
            [websocket-layer.core :as wl]
            ;[ring.server.standalone :refer [create-server]]
            [ring.adapter.jetty9 :as jetty]
            [clojure.pprint :as pprint]
            [websocket-layer.network :as net]
            [nextjournal.beholder :as beholder]
            [io.pedestal.http :as http]
            [rvbbit-backend.surveyor :as surveyor]
            [clojure.java.shell :as shell]
            [rvbbit-backend.assistants :as assistants]
            [flowmaps.db :as flow-db]
            [clojure.data :as data]
            [rvbbit-backend.external :as ext]
            [rvbbit-backend.evaluator :as evl]
            ;; [rvbbit-backend.search :as search]
            [io.pedestal.http.body-params :as body-params]
            [chime.core :as chime]
            ;[rvbbit-backend.flowmaps :as flow]
            [flowmaps.core :as flow]
            ;[flowmaps.db :as flow-db]
            ;[durable-atom.core :refer [durable-atom]]
            [rvbbit-backend.embeddings :as em]
            [clj-ssh.ssh :as ssh]
            ;[duratom.core :as da]
            [cheshire.core :as json]
            [ring.util.response :as ring-resp]
            [io.pedestal.http :as server]
            [taskpool.taskpool :as tp]
            [clojure.edn :as edn]
            [rvbbit-backend.util :as ut :refer [ne?]]
            [io.pedestal.http.route :as route]
            ;[clojure.core.ex-info :refer [Throwable->map]]
            [hikari-cp.core :as hik]
            [clojure.string :as cstr]
            [rvbbit-backend.transform :as ts]
            [rvbbit-backend.pivot :as pivot]
            [rvbbit-backend.sql :as sql :refer [sql-exec sql-query sql-query-one system-db ghost-db flows-db insert-error-row! to-sql pool-create]]
            [clojure.data.csv :as csv]
            [csv-map.core :as ccsv]
            [clojure.core.cache :as cache]
            [clojure.java.io :as io]
            [rvbbit-backend.clickhouse-ddl :as clickhouse-ddl]
            [rvbbit-backend.ddl :as ddl]
            [rvbbit-backend.ddl :as sqlite-ddl] ;; needed for hardcoded rowset filter fn
            [rvbbit-backend.cruiser :as cruiser]
            [tea-time.core :as tt]
            [com.climate.claypoole :as cp]
            ;[rvbbit-backend.instrument :as instrument]
            ;[metrics.ring.instrument :as rinst]
            ;;;[cheshire.core :as json]
            [clj-http.client :as client]
            [clojure.data.json :as json2]
            [clojure.set :as cset]
            ;[metrics.counters :refer [counter inc! dec!]]
            ;[metrics.timers :refer [timer time!]]
            [clojure.walk :as walk]
            [clj-time.coerce :as coerce]
            [rvbbit-backend.config :as config]
            [honey.sql :as honey])
  (:import ;(clojure.lang MultiFn)
   [com.github.vertical_blank.sqlformatter SqlFormatter]
   [java.util.concurrent Executors TimeUnit TimeoutException]

   ;[org.eclipse.jetty.server Server]

  ;;  [org.eclipse.jetty.server Server]
  ;;  [org.eclipse.jetty.servlet ServletContextHandler ServletHolder]




   [java.lang ProcessBuilder]
   [java.io BufferedReader InputStreamReader]


   ;[java.util.concurrent CountDownLatch]
   ;(import (java.util.concurrent Executors))
   ;[java.util.concurrent Executors]
   ;[java.util.concurrent Executors Callable Future ExecutorService ExecutionException CountDownLatch]
   ;[java.lang.management ManagementFactory ThreadMXBean]
   ;[java.time Instant]
   ;[com.github.vertical_blank.sqlformatter.core FormatConfig]
   ;[java.util Arrays]
   ))

(defonce flow-status (atom {}))

(def stats-cnt (atom 0))
(def restart-map (atom {}))
(def orig-caller (atom {}))
(def sub-flow-blocks (atom {}))
(def custom-flow-blocks (ut/thaw-atom {} "./data/atoms/custom-flow-blocks-atom.edn"))
;(def screens-atom (atom {}))
(def screens-atom (ut/thaw-atom {} "./data/atoms/screens-atom.edn"))
(def server-atom (ut/thaw-atom {} "./data/atoms/server-atom.edn"))

(def last-signals-atom (ut/thaw-atom {} "./data/atoms/last-signals-atom.edn"))
(defonce signal-parts-atom (atom []))
(def last-signals-history-atom (ut/thaw-atom {} "./data/atoms/last-signals-history-atom.edn"))
(def last-signal-value-atom (ut/thaw-atom {} "./data/atoms/last-signal-value-atom.edn"))
(def last-signals-atom-stamp (ut/thaw-atom {} "./data/atoms/last-signals-atom-stamp.edn"))
;;(def panels-atom (atom {}))
(def panels-atom (ut/thaw-atom {} "./data/atoms/panels-atom.edn"))
(def watchdog-atom (atom {}))
(def last-block-written (atom {})) ;; kind of wasteful, but it's a small atom and is clean. 
(def latest-run-id (ut/thaw-atom {} "./data/atoms/latest-run-id-atom.edn"))
(def shutting-down? (atom false))
(defonce tracker-client-only (atom {}))
(def acc-trackers (atom {}))
(def temp-error-blocks (atom {}))

(def ack-scoreboard (atom {}))
(def ping-ts (atom {}))
(def client-latency (atom {}))

(def signals-atom (ut/thaw-atom {} "./defs/signals.edn"))
(def rules-atom (ut/thaw-atom {} "./defs/rules.edn"))

(def solvers-atom (ut/thaw-atom {} "./defs/solvers.edn"))
(def solvers-cache-atom (ut/thaw-atom {} "./data/atoms/solvers-cache.edn"))
(def last-solvers-atom (ut/thaw-atom {} "./data/atoms/last-solvers-atom.edn"))
(def last-solvers-atom-meta (ut/thaw-atom {} "./data/atoms/last-solvers-atom-meta.edn"))
(def last-solvers-history-atom (ut/thaw-atom {} "./data/atoms/last-solvers-history-atom.edn"))


(def time-atom (ut/thaw-atom {} "./data/atoms/time-atom.edn"))

(def clover-sql-training-atom (ut/thaw-atom {} "./data/training/clover-sql-training-atom.edn"))
(def clover-sql-enriched-training-atom (ut/thaw-atom {} "./data/training/clover-sql-enriched-training-atom.edn"))

(def param-var-mapping (atom {}))
(def param-var-crosswalk (atom {}))
(def param-var-key-mapping (atom {}))

(def jvm-stats-every 30)


(def task-queue (java.util.concurrent.LinkedBlockingQueue.))
(def running (atom true))
(def worker (atom nil)) ; Holds the future of the worker thread

(defn enqueue-task [task]
  (.put task-queue task))

(defn worker-loop []
  (loop []
    (when @running
      (let [task (.take task-queue)]
        (task))))
  (recur))

(defn start-worker []
  ;(Thread/sleep 1000)
  (ut/pp [:starting-sync-worker-thread 1])
  (reset! running true)
  (reset! worker (future (worker-loop))))

(defn stop-worker []
  (reset! running false)
  (when-let [w @worker]
    (future-cancel w)
    (while (not (.isDone w)) ; Ensure the future is cancelled before proceeding
      (Thread/sleep 60))))

(defn recycle-worker []
  ;(ut/pp [:REBOOTING-WORKER-THREAD!!])
  (reset! stats-cnt 0)
  ;(System/gc) ;;; hehe, fuck it
  (stop-worker)
  (start-worker))

(def task-queue2 (java.util.concurrent.LinkedBlockingQueue.))
(def running2 (atom true))
(def worker2 (atom nil)) ; Holds the future of the worker thread

(defn enqueue-task2 [task]
  (.put task-queue2 task))

(defn worker-loop2 []
  (loop []
    (when @running2
      (let [task (.take task-queue2)]
        (task))))
  (recur))

(defn start-worker2 []
  ;(Thread/sleep 2000)
  (ut/pp [:starting-sync-worker-thread 2])
  (reset! running2 true)
  (reset! worker2 (future (worker-loop2))))

(defn stop-worker2 []
  (reset! running2 false)
  (when-let [w @worker2]
    (future-cancel w)
    (while (not (.isDone w)) ; Ensure the future is cancelled before proceeding
      (Thread/sleep 60))))

(defn recycle-worker2 []
  ;(ut/pp [:REBOOTING-WORKER-THREAD2!!])
  ;(System/gc) ;;; hehe, fuck it
  (stop-worker2)
  (start-worker2))

(def task-queue3 (java.util.concurrent.LinkedBlockingQueue.))
(def running3 (atom true))
(def worker3 (atom nil)) ; Holds the future of the worker thread

(defn enqueue-task3 [task]
  (.put task-queue3 task))

(defn worker-loop3 []
  (loop []
    (when @running3
      (let [task (.take task-queue3)]
        (task))))
  (recur))

(defn start-worker3 []
  ;(Thread/sleep 3000)
  (ut/pp [:starting-sync-worker-thread 3])
  (reset! running3 true)
  (reset! worker3 (future (worker-loop3))))

(defn stop-worker3 []
  (reset! running3 false)
  (when-let [w @worker3]
    (future-cancel w)
    (while (not (.isDone w)) ; Ensure the future is cancelled before proceeding
      (Thread/sleep 60))))

(defn recycle-worker3 []
  ;(ut/pp [:REBOOTING-WORKER-THREAD3!!])
  ;(System/gc) ;;; hehe, fuck it
  (stop-worker3)
  (start-worker3))





;; thread pool version... testing out with run-solvers instead of async free for all...
(def task-queue4 (java.util.concurrent.LinkedBlockingQueue.))
(def running4 (atom true))
(def workers4 (atom nil)) ; Holds the futures of the worker threads

(defn enqueue-task4 [task]
  (.put task-queue4 task))

(defn worker-loop4 []
  (loop []
    (when @running4
      (let [task (.take task-queue4)]
        (task))))
  (recur))

(defn start-workers4 [num-workers]
  (ut/pp [:starting-sync-worker-thread-*pool 4])
  (reset! running4 true)
  (reset! workers4 (doall (map (fn [_] (future (worker-loop4))) (range num-workers)))))

;; (defn stop-workers4 []
;;   (reset! running4 false)
;;   (doseq [w @workers4]
;;     (future-cancel w)
;;     (while (not (.isDone w)) ; Ensure the future is cancelled before proceeding
;;       (Thread/sleep 60))))

(defn stop-workers4 []
  (reset! running4 false)
  (doseq [w @workers4]
    (future-cancel w)
    (while (not (.isDone w)) ; Ensure the future is cancelled before proceeding
      (Thread/sleep 60)))
  (while (not (.isEmpty task-queue4)) ; Wait until the task queue is empty
    (Thread/sleep 60)))

(defn recycle-workers4 [num-workers]
  (stop-workers4)
  (start-workers4 num-workers))







;; thread pool version... testing out with run-solvers instead of async free for all...
(def task-queue5 (java.util.concurrent.LinkedBlockingQueue.))
(def running5 (atom true))
(def workers5 (atom nil)) ; Holds the futures of the worker threads

(defn enqueue-task5 [task]
  (.put task-queue5 task))

(defn worker-loop5 []
  (loop []
    (when @running5
      (let [task (.take task-queue5)]
        (task))))
  (recur))

;; (defn worker-loop5 [] ;;; with 10 min time out... TBD...
;;   (loop []
;;     (when @running5
;;       (let [task (.take task-queue5)
;;             future-task (future (task))]
;;         (if (= ::timeout (deref future-task 600000 ::timeout)) ; 5000 is the timeout in milliseconds
;;           (do
;;             (println "Task timed out, cancelling")
;;             (future-cancel future-task))
;;           (println "Task completed")))))
;;   (recur))

(defn start-workers5 [num-workers]
  (ut/pp [:starting-sync-worker-thread-*pool 5])
  (reset! running5 true)
  (reset! workers5 (doall (map (fn [_] (future (worker-loop5))) (range num-workers)))))

;; (defn stop-workers5 []
;;   (reset! running5 false)
;;   (doseq [w @workers5]
;;     (future-cancel w)
;;     (while (not (.isDone w)) ; Ensure the future is cancelled before proceeding
;;       (Thread/sleep 60))))

(defn stop-workers5 []
  (reset! running5 false)
  (doseq [w @workers5]
    (future-cancel w)
    (while (not (.isDone w)) ; Ensure the future is cancelled before proceeding
      (Thread/sleep 60)))
  (while (not (.isEmpty task-queue5)) ; Wait until the task queue is empty
    (Thread/sleep 60)))

(defn recycle-workers5 [num-workers]
  (stop-workers5)
  (start-workers5 num-workers))








(def task-queue6 (java.util.concurrent.LinkedBlockingQueue.))
(def workers6 (atom {})) ; Holds a map of task IDs to futures, start times, and task strings

(defn enqueue-task6 [task]
  (let [id (str (java.util.UUID/randomUUID))
        task-str (pr-str (take 10 task))] ; Stringify the task, taking only the first 10 elements to limit the length
    (.put task-queue6 {:task task :id id :task-str task-str})))

(defn worker-loop6 []
  (loop []
    (when-let [{:keys [task id task-str]} (.poll task-queue6)] ; Use poll instead of take to avoid blocking when the queue is empty
      (let [future-task (async/future-timeout (task) (* 10 60 1000))] ; Add a 10 minute timeout to the task
        (swap! workers6 assoc id {:future future-task :start-time (System/currentTimeMillis) :task-str task-str}) ; Store the future, start time, and task string in workers6
        (task)
        (swap! workers6 dissoc id))))
  (recur))

(defn start-workers6 [num-workers]
  (ut/pp [:starting-sync-worker-thread-*pool 6])
  (reset! workers6 {})
  (doall (map (fn [_] (future (worker-loop6))) (range num-workers))))

(defn stop-workers6 []
  (doseq [[id {:keys [future]}] @workers6]
    (future-cancel future)
    (while (not (.isDone future)) ; Ensure the future is cancelled before proceeding
      (Thread/sleep 60)))
  (while (not (.isEmpty task-queue6)) ; Wait until the task queue is empty
    (Thread/sleep 60)))

(defn cancel-task [id] ; Function to cancel a task by its ID
  (let [{:keys [future]} (get @workers6 id)]
    (when future
      (future-cancel future)
      (swap! workers6 dissoc id))))

(defn recycle-workers6 [num-workers]
  (stop-workers6)
  (start-workers6 num-workers))





;;; same with timeout and diff setup and tracker / cancel
(def task-queue7 (java.util.concurrent.LinkedBlockingQueue.))
(def workers7 (atom {})) ; Holds a map of task IDs to futures, start times, and task strings
(def executor (java.util.concurrent.Executors/newScheduledThreadPool 1)) ; Executor for cancelling tasks after the timeout

(defn enqueue-task7 [task]
  (let [id (str (java.util.UUID/randomUUID))
        task-str (pr-str (take 10 task))] ; Stringify the task, taking only the first 10 elements to limit the length
    (.put task-queue7 {:task task :id id :task-str task-str})))

(defn worker-loop7 []
  (loop []
    (when-let [{:keys [task id task-str]} (.poll task-queue7)] ; Use poll instead of take to avoid blocking when the queue is empty
      (let [future-task (future (task))
            cancel-task (reify Runnable (run [_] (future-cancel future-task)))] ; Task to cancel the future after the timeout
        (.schedule executor cancel-task 10 java.util.concurrent.TimeUnit/MINUTES) ; Schedule the cancellation task to run after 10 minutes
        (swap! workers7 assoc id {:future future-task :start-time (System/currentTimeMillis) :task-str task-str}) ; Store the future, start time, and task string in workers7
        (task)
        (swap! workers7 dissoc id))))
  (recur))

(defn start-workers7 [num-workers]
  (ut/pp [:starting-sync-worker-thread-*pool 7])
  (reset! workers7 {})
  (doall (map (fn [_] (future (worker-loop7))) (range num-workers))))

(defn stop-workers7 []
  (doseq [[id {:keys [future]}] @workers7]
    (future-cancel future)
    (while (not (.isDone future)) ; Ensure the future is cancelled before proceeding
      (Thread/sleep 60)))
  (while (not (.isEmpty task-queue7)) ; Wait until the task queue is empty
    (Thread/sleep 60))
  (reset! workers7 {})) ; Clear the workers7 atom

(defn cancel-task [id] ; Function to cancel a task by its ID
  (let [{:keys [future]} (get @workers7 id)]
    (when future
      (future-cancel future)
      (swap! workers7 dissoc id))))

(defn recycle-workers7 [num-workers]
  (stop-workers7)
  (start-workers7 num-workers))





;; (def flow-executor-service (Executors/newFixedThreadPool 5))

;; (defonce flow-running-tasks (atom {}))

;; (defn submit-named-task [task-name task]
;;   (let [task-name (str task-name)
;;         future (.submit flow-executor-service task)
;;         thread-id (.getId (Thread/currentThread))
;;         start-time (Instant/now)]
;;     (swap! flow-running-tasks assoc task-name {:future future :start-time start-time :thread-id thread-id})
;;     future))

;; ;; huge waste of time. thanks for nothing.
;; (defn submit-named-task-and-wait [task-name task-fn]
;;   (let [task-name (str task-name)
;;         done-flag (atom false) ; An atom to act as a flag
;;         callable-task (reify Callable
;;                         (call [_]
;;                           (let [thread-id (.getId (Thread/currentThread))
;;                                 start-time (Instant/now)]
;;                             (println (str "starting task " task-name " " thread-id))
;;                             (swap! flow-running-tasks assoc task-name {:start-time start-time :thread-id thread-id})
;;                             (task-fn #(reset! done-flag true)) ; Set the flag to true upon completion
;;                             (loop [] ; Wait for the flag to be true
;;                               (when (not @done-flag)
;;                                 (Thread/sleep 100) ; Avoid busy waiting
;;                                 (recur)))
;;                             (println (str "Task " task-name " completed"))
;;                             "Task completed successfully")))]
;;     (let [future (.submit flow-executor-service callable-task)]
;;       (try
;;         (let [result (.get future)]
;;           (println (str "Task " task-name " fully completed, result retrieved"))
;;           (swap! flow-running-tasks dissoc task-name)
;;           result)
;;         (catch InterruptedException e
;;           (println (str "Task " task-name " was interrupted."))
;;           (swap! flow-running-tasks dissoc task-name)
;;           nil)
;;         ;; (catch ExecutionException e
;;         ;;   (println (str "Execution of task " task-name " failed: " (.getMessage e)))
;;         ;;   (swap! flow-running-tasks dissoc task-name)
;;         ;;   nil)
;;         ))))

;; (defn get-task-cpu-time [task-name]
;;   (let [task-name (str task-name)
;;         thread-id (get-in @flow-running-tasks [task-name :thread-id])
;;         thread-mx-bean (ManagementFactory/getThreadMXBean)]
;;     (when thread-id
;;       ;; Ensure the MXBean supports thread CPU time measurement
;;       (when (.isThreadCpuTimeSupported thread-mx-bean)
;;         ;; Enable CPU time measurement if it's not already enabled
;;         (.setThreadCpuTimeEnabled thread-mx-bean true)
;;         ;; Get the CPU time for the thread
;;         (let [cpu-time (.getThreadCpuTime thread-mx-bean thread-id)]
;;           (if (neg? cpu-time)
;;             (println "CPU time measurement is not available for the thread or the thread does not exist.")
;;             (println (str "CPU time for task '" task-name "' is " cpu-time " nanoseconds.")))
;;             cpu-time)))))

;; (defn cancel-named-task [task-name]
;;   (let [task-name (str task-name)]
;;     (when-let [task-meta (@flow-running-tasks task-name)]
;;       (.cancel (:future task-meta) true) ;; Attempt to interrupt if running
;;       (swap! flow-running-tasks dissoc task-name)
;;       (println (str "Task cancelled: " task-name)))))




;; (defn start-process [process-id command & [wait?]]
;;   (let [process-builder (ProcessBuilder. command)
;;         process (.start process-builder)]
;;     (swap! processes assoc process-id {:process process
;;                                        :output {:stdout "" :stderr ""}
;;                                        :*running? true
;;                                        :start (System/currentTimeMillis)})
;;     (let [stdout-future (future (read-stream (.getInputStream process) process-id :stdout))
;;           stderr-future (future (read-stream (.getErrorStream process) process-id :stderr))
;;           exit-code-future (future (let [exit-code (.waitFor process)]
;;                                      (swap! processes assoc-in [process-id :exit-code] exit-code)
;;                                      (swap! processes assoc-in [process-id :end] (System/currentTimeMillis))
;;                                      (swap! processes assoc-in [process-id :*running?] false)))]
;;       (when wait?
;;         @exit-code-future
;;         @stdout-future
;;         @stderr-future))))



;; (defn start-process [process-id command & [wait?]]
;;   (let [process-builder (ProcessBuilder. command)
;;         process (.start process-builder)
;;         exit-code-promise (promise)]
;;     (swap! processes assoc process-id {:process process
;;                                        :output {:stdout "" :stderr ""}
;;                                        :command (cstr/join " " command)
;;                                        :*running? true
;;                                        :start (System/currentTimeMillis)})
;;     (future (read-stream (.getInputStream process) process-id :stdout))
;;     (future (read-stream (.getErrorStream process) process-id :stderr))
;;     (future (let [exit-code (.waitFor process)]
;;               (deliver exit-code-promise exit-code)
;;               (swap! processes assoc-in [process-id :exit-code] exit-code)
;;               (swap! processes assoc-in [process-id :end] (System/currentTimeMillis))
;;               (swap! processes assoc-in [process-id :*running?] false)))
;;     (when wait?
;;       (let [exit-code @exit-code-promise]
;;         (get-output process-id)))))

(def processes (atom {}))

(defn- read-stream [input-stream process-id output-key]
  (let [reader (BufferedReader. (InputStreamReader. input-stream))]
    (loop [line (.readLine reader)]
      (when line
        (swap! processes update-in [process-id :output output-key] #(str % line "\n"))
        (recur (.readLine reader))))))

(defn get-output [process-id]
  (get-in @processes [process-id :output]))

;; (defn start-process [process-id command & [wait?]]
;;   (let [process-builder (ProcessBuilder. command)
;;         process (.start process-builder)]
;;     (swap! processes assoc process-id {:process process
;;                                        :output {:stdout "" :stderr ""}
;;                                        :command (cstr/join " " command)
;;                                        :*running? true
;;                                        :start (System/currentTimeMillis)})
;;     (future (read-stream (.getInputStream process) process-id :stdout))
;;     (future (read-stream (.getErrorStream process) process-id :stderr))
;;     (if wait?
;;       (let [exit-code (.waitFor process)]
;;         (swap! processes assoc-in [process-id :exit-code] exit-code)
;;         (swap! processes assoc-in [process-id :end] (System/currentTimeMillis))
;;         (swap! processes assoc-in [process-id :*running?] false)
;;         (get-output process-id))
;;       process)))

(defn start-process [process-id command & [wait? ssh-host ssh-user ssh-pass]]
  (if (not (empty? ssh-host))
    (let [session (ssh/session ssh-host ssh-user ssh-pass)]
      (ssh/with-connection session
        (let [result (ssh/ssh session {:cmd (cstr/join " " command)})
              end-time (System/currentTimeMillis)]
          (swap! processes assoc process-id {:output {:stdout (:out result) :stderr (:err result)}
                                             :command (cstr/join " " command)
                                             :*running? false
                                             :start (System/currentTimeMillis)
                                             :end end-time
                                             :exit-code (:exit result)}))))
    (let [process-builder (ProcessBuilder. command)
          process (.start process-builder)]
      (swap! processes assoc process-id {:process process
                                         :output {:stdout "" :stderr ""}
                                         :command (cstr/join " " command)
                                         :*running? true
                                         :start (System/currentTimeMillis)})
      (future (read-stream (.getInputStream process) process-id :stdout))
      (future (read-stream (.getErrorStream process) process-id :stderr))
      (if wait?
        (let [exit-code (.waitFor process)
              end-time (System/currentTimeMillis)]
          (swap! processes assoc-in [process-id :exit-code] exit-code)
          (swap! processes assoc-in [process-id :end] end-time)
          (swap! processes assoc-in [process-id :*running?] false)
          (get-output process-id))
        process))))

(defn stop-process [process-id]
  (when-let [process-info (@processes process-id)]
    (.destroy (:process process-info))
    (swap! processes assoc-in [process-id :*running?] false)))

(defn process-running? [process-id]
  (get-in @processes [process-id :*running?]))

(defn process-exit-code [process-id]
  (get-in @processes [process-id :exit-code]))



(defn sql-formatter [sql-str]
  (try (let [res (pr-str (SqlFormatter/format sql-str))]
         (if (nil? res)
           sql-str res))
       (catch Exception _ sql-str)))

(defn flatten-map
  ([m] (flatten-map m '()))
  ([m prefix]
   (if (map? m)
     (reduce-kv (fn [acc k v]
                  (let [new-prefix (conj prefix k)]
                    (merge acc (flatten-map v new-prefix))))
                {}
                m)
     (let [key-str (clojure.string/join "-" (map name prefix))
           final-key (if (re-matches #"\d.*" key-str)
                       (str "_" key-str)
                       key-str)]
       {(keyword final-key) m}))))

(defn filter-not-done [m]
  (into {}
        (group-by first (vec (for [kp (filter #(= (count %) 3) (ut/kvpaths m))
                                   :let [v (get-in m kp)]
                                   :when (not (= :done v))]
                               [v kp])))))

;; (defn http-call [req]
;;   (ut/pp [:HTTP-CALL req])
;;   (try
;;     (let [{:keys [url headers query-params method body file] :or {method :get}} req
;;           http-method (case method
;;                         :get client/get
;;                         :post client/post
;;                         :put client/put
;;                         :delete client/delete
;;                         :head client/head
;;                         :options client/options
;;                         :patch client/patch
;;                         :GET client/get
;;                         :POST client/post
;;                         :PUT client/put
;;                         :DELETE client/delete
;;                         :HEAD client/head
;;                         :OPTIONS client/options
;;                         :PATCH client/patch
;;                         (throw (IllegalArgumentException. {:error (str "Unknown http method: " method)})))
;;           body2 (if (nil? body)
;;                   {:query-params query-params}
;;                   (if (nil? file)
;;                     {:body (json/generate-string body)}
;;                     {:multipart [{:name "file" :content (slurp file) :filename (last (clojure.string/split file #"/"))}
;;                                  {:name "purpose" :content "assistants"}]}))
;;           response (try
;;                      (http-method url (merge {:as :json :headers headers :debug false}
;;                                              body2))
;;                      (catch Exception e
;;                        {:error true
;;                         :message (str (.getMessage e)) :msg (str e)
;;                         :class (str (type e))}))]
;;       (ut/pp [:HTTP-CALL method req response])
;;       (if (:error response)
;;         (do (ut/pp [:http-call-error response :body-sent body2])
;;             response)
;;         (get response :body)))
;;     (catch Exception e {:error true
;;                         :message (str (.getMessage e)) :msg (str e)
;;                         :class (str (type e))})))

(defn clean-key [k]
  (if (keyword? k)
    (keyword (clojure.string/replace (name k) #"/" ""))
    k))

(defn recursive-clean [data]
  (cond
    (map? data) (into {} (map (fn [[k v]] [(clean-key k) (recursive-clean v)]) data))
    (coll? data) (mapv recursive-clean data)
    :else data))

;; Usage
;;(clean-data [{:success {:/groups/200/action/hue 20000}} {:success {:/groups/200/action/sat 254}}])


(defn http-call [req]
  ;(ut/pp [:HTTP-CALL! req])
  (try
    (let [{:keys [url headers query-params method body file save-to] :or {method :get}} req
          http-method (case method
                        :get client/get
                        :post client/post
                        :put client/put
                        :delete client/delete
                        :head client/head
                        :options client/options
                        :patch client/patch
                        :GET client/get
                        :POST client/post
                        :PUT client/put
                        :DELETE client/delete
                        :HEAD client/head
                        :OPTIONS client/options
                        :PATCH client/patch
                        (throw (IllegalArgumentException. {:error (str "Unknown http method: " method)})))
          body2 (if (nil? body)
                  {:query-params query-params}
                  (if (nil? file)
                    {:body (json/generate-string body)}
                    {:multipart [{:name "file" :content (slurp file) :filename (last (clojure.string/split file #"/"))}
                                 {:name "purpose" :content "assistants"}]}))
          response (try
                     (http-method url (merge {:as (if save-to :byte-array :json)
                                              :headers headers
                                              :debug false}
                                             body2))
                     (catch Exception e
                       {:error (str e)
                        :message (str (.getMessage e)) :msg (str e)
                        :class (str (type e))}))]
     ; (ut/pp [:HTTP-CALL method req response])
      (cond (:error response)
            (do (ut/pp [:http-call-error response :body-sent body2])
                {:error response
                 :message (:error response)})
            (cstr/includes? (str response) "status 400")
            (do (ut/pp [:http-call-error-400 response :body-sent body2])
                {:error response
                 :message "400 code"})
            :else (if save-to
                    (do (spit save-to (:body response)) {:success true :path save-to})
                    (let [resp (recursive-clean (get response :body {:success true :status (get response :status)}))
                          kp (ut/kvpaths resp)
                ;_ (ut/pp [:http-call resp])
                          dumped (str "/home/ryanr/rvbbit-out/b64-decoded-" (rand-int 10000000) ".webp")
                ;; ks (filter #(cstr/includes? (str %) ":b64_json") kp)
                ;; files (vec (for [k ks]
                ;;              (let [v (get-in resp k)
                ;;                    dumped (str "/home/ryanr/rvbbit-out/b64-decoded-" (rand-int 100000) ".webp")]
                ;;                (do (ut/save-base64-to-webp v dumped)
                ;;                    dumped))))
                          kps (first (filter #(cstr/includes? (str %) ":b64_json") kp))
                          b64? (not (empty? kps))
                          _ (when b64? (ut/save-base64-to-webp (get-in resp kps) dumped))
                          resp (ut/deep-remove-keys resp [:b64_json])]
           ; (ut/pp [:http-call-last (recursive-clean resp) resp :fuck-me])
            ;(ut/deep-remove-keys resp [:b64_json])
                      (if b64? ;(not (empty? kp))
                        (assoc-in resp kps dumped)
                        (let [rr (recursive-clean resp)
                              rrs (pr-str rr) ;; i hate everyone, why does client-http keywording fucked ""keys""?
                              tts (cstr/replace rrs #":/" ":")]
                          (edn/read-string tts)))))))
    (catch Exception e {:error (str e)
                        :message (str (.getMessage e)) :msg (str e)
                        :class (str (type e))})))


;; (defn throwable->map [t]
;;   {:message (.getMessage t)
;;    :class (str (type t))
;;    :stack-trace (clojure.stacktrace/print-str t)})

;; (catch Throwable t (throwable->map t))

;; (defn throwable->serializable-map [t]
;;   {:message (.getMessage t)
;;    :class (str (.getName (.getClass t)))
;;    :cause (when-let [cause (.getCause t)]
;;             {:message (.getMessage cause)
;;              :class (str (.getName (.getClass cause)))})})


(defn throwable->serializable-map [t]
  {:message (.getMessage t)
   :class (str (.getName (.getClass t)))
   :cause (when-let [cause (.getCause t)]
            {:message (.getMessage cause)
             :class (str (.getName (.getClass cause)))})
   :stack-trace (map (fn [ste]
                       {:class (.getClassName ste)
                        :method (.getMethodName ste)
                        :file (.getFileName ste)
                        :line (.getLineNumber ste)})
                     (.getStackTrace t))})

(defn parse-error-body [e]
  (try
    (let [response-body (-> e ex-data :body json2/read-str)]
      {:error (get-in response-body ["error" "message"])
       :class (str (type e))})
    (catch Exception _ {:error (str e)
                        :class "ERROR IN PARSE-ERROR-BODY. lol"})))

(defn make-http-call [req]
  ;; (ut/pp [:MAKE-HTTP-CALL! (hash req) req])
  (try
    (let [{:keys [url headers query-params method body file save-to] :or {method :get}} req
          http-method (case method
                        :get client/get
                        :post client/post
                        :put client/put
                        :delete client/delete
                        :head client/head
                        :options client/options
                        :patch client/patch
                        :GET client/get
                        :POST client/post
                        :PUT client/put
                        :DELETE client/delete
                        :HEAD client/head
                        :OPTIONS client/options
                        :PATCH client/patch
                        (throw (IllegalArgumentException.
                                {:error (str "Unknown http method: " method)})))
          body2 (if (nil? body)
                  {:query-params query-params}
                  (if (nil? file)
                    {:body (json/generate-string body)}
                    {:multipart [{:name "file"
                                  :content (slurp file)
                                  :filename (last (clojure.string/split file #"/"))}
                                 {:name "purpose"
                                  :content "assistants"}]}))
          response (try
                     (http-method url (merge {:as (if save-to :byte-array :json)
                                              :headers headers
                                              :debug false}
                                             body2))
                     (catch Exception e
                       {;:error (str e)
                        :error (parse-error-body e)
                       ; :error (throwable->serializable-map e) ;(str (.getMessage e))
                        ;:error (Throwable->map e)
                        ;:msg (str e)
                        :class (str (type e))}))]
      ;(ut/pp [:HTTP-CALL-RESPONSE method req (ut/replace-large-base64 response)])
      (cond (:error response)
            (do ;(ut/pp [:http-call2-error response :body-sent body2])
              {:error response
               :message (:error response)})
            (cstr/includes? (str response) "status 400")
            (do ;(ut/pp [:http-call2-error-400 response :body-sent body2])
              {:error response
               :message "400 code"})
            :else (if save-to
                    (do (spit save-to (:body response)) {:success true :path save-to})
                    (let [resp (recursive-clean (get response :body {:success true :status (get response :status)}))
                         ; resp (ut/deep-remove-keys resp [:b64_json])
                          rr (recursive-clean resp)
                          rrs (pr-str rr) ;; i hate everyone, why does client-http keywording fucked ""keys""?
                          tts (cstr/replace rrs #":/" ":")
                          ttsr (edn/read-string tts)]
                     ;(ut/pp [:MAKE-HTTP-CALL-RESPONSE-SUCCESS ttsr])
                      ttsr))))
    (catch Exception e ;{:error (str e)
                       ; :message (str (.getMessage e))
                       ; :msg (str e)
                       ; :class (str (type e))}
      {;:error (str e)
       :error (parse-error-body e)
                        ;:message (str (.getMessage e))
       ;:error (Throwable->map e)
       ; :error (throwable->serializable-map e)
       :class (str (type e))})))




;; (defn sql-formatter [sql-str]
;;   (let [config (-> (.builder FormatConfig)
;;                    (.tabulateAlias true)
;;                    (.indentStyle "tabularRight")
;;                    ;(.indent "    ")
;;                    ;(.uppercase true)
;;                    ;(.linesBetweenQueries 2)
;;                    (.maxColumnLength 100)
;;                    ;(.params (Arrays/asList (into-array ["a" "b" "c"])))
;;                    (.build))]
;;     (SqlFormatter/format sql-str config)))


(defn- run-task [executor task timeout-ms]
  (let [future (.submit executor task)]
    (try
      (.get future timeout-ms TimeUnit/MILLISECONDS)
      (catch TimeoutException e
        (println "Task timed out, cancelling!")
        (.cancel future true)) ; mayInterruptIfRunning = true
      (catch Exception e
        (println "Task failed with an uncaught exception:" (.getMessage e)))
      (finally
        ;; If you want to shutdown the executor after this task, do it here
        ;; Otherwise, manage the executor lifecycle outside of this function
        ))))


;; (defmacro with-timeout [timeout-ms & body]
;;   `(let [executor# (Executors/newFixedThreadPool 10) ; Adjust thread pool size as needed
;;          task# (fn [] ~@body)]
;;      (run-task executor# task# ~timeout-ms)
;;      ;; Shutdown logic can go here if you are not planning to reuse the executor
;;      ;; (.shutdownNow executor#)
;;      ))


(defmacro timed-expr [expr]
  `(let [start# (System/currentTimeMillis)
         result# ~expr
         end# (System/currentTimeMillis)]
     {:result result#
      :elapsed-ms (- end# start#)}))

;(def dev? false)

;; (def cache-db
;;   ;cruiser/mem-db-pgsql
;; ;  cruiser/mem-db-sqlite
;;   cruiser/mem-db-sqlite2
;;  ; cruiser/mem-db-mysql
;;   ;cruiser/mem-db-vertica
;;   ;cruiser/mem-db-clickhouse
;;   )

;; (defonce cache-db0 (delay (hik/make-datasource
;;                             {:jdbc-url "jdbc:sqlite:db/cache.db"
;;                              :minimum-idle       10
;;                              :pool-name "cache-db-pool"
;;                              :cache "shared"
;;                              :metric-registry instrument/metric-registry
;;                              :maximum-pool-size  15})))
;; (defonce cache-db {:datasource @cache-db0})

;; (defonce import-db0 (delay (hik/make-datasource
;;                            {:jdbc-url "jdbc:sqlite:db/csv-imports.db"
;;                             :minimum-idle       10
;;                             :pool-name "imports-db-pool"
;;                             :cache "shared"
;;                             :metric-registry instrument/metric-registry
;;                             :maximum-pool-size  15})))
;; (defonce import-db {:datasource @import-db0})

(def import-db {:datasource @(pool-create {:jdbc-url "jdbc:sqlite:db/csv-imports.db"
                                           ;:idle-timeout        600000
                                           ;:max-lifetime       1800000
                                           :cache "shared"} "imports-db-pool")})

;; (def import-db {:datasource @(pool-create {:jdbc-url "jdbc:sqlite:file:importdb?mode=memory&cache=shared" ; "jdbc:sqlite:db/cache.db"
;;                                           :idle-timeout       600000
;;                                           :max-lifetime       1800000
;;                                           :cache "shared"} "imports-db-pool")})

(def cache-db {:datasource @(pool-create {;;:jdbc-url "jdbc:sqlite:file:cachedb?mode=memory&cache=shared&transaction_mode=IMMEDIATE&auto_vacuum=FULL" ; "jdbc:sqlite:db/cache.db"
                                          :jdbc-url "jdbc:sqlite:file:cachedb?mode=memory&cache=shared&auto_vacuum=FULL"
                                          ;:jdbc-url "jdbc:sqlite:file:./db/cache.db?cache=shared&transaction_mode=IMMEDIATE&auto_vacuum=FULL" ; "jdbc:sqlite:db/cache.db"
                                          ;:jdbc-url "jdbc:sqlite::memory:?mode=memory&cache=shared&transaction_mode=IMMEDIATE&journal_mode=WAL" ; "jdbc:sqlite:db/cache.db"
                                          ;:idle-timeout        600000 ;;; 10/25/23 LAST KNOWN GOOD SQLITE CONFIG
                                          ;:max-lifetime       1800000
                                          ;:maximum-pool-size 60
                                          ;:transaction_mode "IMMEDIATE"
                                          ;:journal_mode "WAL"
                                          :cache "shared"} "cache-db-pool")})

;; (def cache-db {:datasource @(pool-create {:jdbc-url "jdbc:clickhouse://10.174.1.150:8123/rabbit_cache"
;;                                           :username "rabbit_cache"
;;                                           :password "notofox"
;;                                           :driver-class-name "ru.yandex.clickhouse.ClickHouseDriver"
;;                                           :connection-timeout 5000
;;                                           ;:maximum-pool-size 20
;;                                           :max-lifetime 300000} "cache-db-pool")})

;; (def cache-db {:datasource @(pool-create {:jdbc-url "jdbc:sqlite:file:cachedb?mode=memory&cache-shared&transaction_mode=DEFERRED&journal_mode=WAL" ; "jdbc:sqlite:db/cache.db"
;;                                           ;:idle-timeout        600000
;;                                           ;:max-lifetime       1800000
;;                                           :transaction_mode "DEFERRED"
;;                                           :cache "shared"} "cache-db-pool")})

;; (def cache-db {:datasource @(pool-create {:jdbc-url "jdbc:h2:mem:cachedb;DATABASE_TO_UPPER=FALSE" ; "jdbc:hsqldb:mem:cachedb" ;"jdbc:duckdb:db/cache.duck" IGNORECASE=TRUE;
;;                                           :idle-timeout       600000
;;                                           :max-lifetime       1800000
;;                                           ;:cache "shared"
;;                                           } "cache-db-pool")})

(def running-system-queries -1) ;;(counter instrument/metric-registry ["queries" "counters" "running-system-queries"]))
(def running-user-queries -1)  ;;(counter instrument/metric-registry ["queries" "counters" "running-user-queries"]))

;; (def import-db22 {:classname   "org.sqlite.JDBC"
;;                   :subprotocol "sqlite"
;;                   :subname    (str "./data/csv-imports.db?cache=shared")}) ;; busy_timeout=20000&

;; (def import-db "jdbc:sqlite:data/csv-imports.db?cache=shared")

;(def row-counter (atom {}))

(defonce deep-run-list (atom []))
(defonce q-calls (atom 0))
(defonce q-calls2 (atom 0))
(defonce literal-data-map (atom {}))
(defonce literal-data-output (atom {}))

(defn insert-rowset-csv ;; [rowset query & columns-vec]
  "takes a 'rowset' (vector of uniform maps) or a vector with a vector of column names
   - inserts it into an in memory SQL db, executes a SQL query on it
   (via a honey-sql map) and returns it"
  ;; yes, I know this is inefficent as all hell compared to map,filter,reduce, etc - but I want the honeySQL DSL for consistency / end-user usability sake
  [rowset table-name client-name op-name & columns-vec]
  (ut/pp [:importing-csv-to-sql table-name])
  (let [rowset-type (cond (and (map? (first rowset)) (vector? rowset)) :rowset
                          (and (not (map? (first rowset))) (vector? rowset)) :vectors)
        columns-vec-arg (first columns-vec)
        batch-size 100
        ;columns-vec-arg-underscores (edn/read-string (cstr/replace (str columns-vec-arg) #"-" "_")) ;; ugly, but I need to keep the order
       ; db-conn {:classname   "org.sqlite.JDBC"
       ;          :subprotocol "sqlite"
       ;          :subname    (str "./data/" table-name ".db?busy_timeout=20000&cache=shared")}
        rowset-fixed (if (= rowset-type :vectors)
                       (vec (for [r rowset]
                              (zipmap columns-vec-arg r)))
                       rowset)
        columns (keys (first rowset-fixed))
        values (vec (for [r rowset-fixed] (vals r)))
        ;table-name-str  "csvfile" ;(ut/unkeyword table-name)
        ddl-str (ddl/create-attribute-sample table-name rowset-fixed)
       ; insert-sql (to-sql {:insert-into [:csvfile]
       ;                     :columns columns
       ;                     :values values})
       ; status-update (fn [x] (sql-exec system-db
       ;                                 (to-sql {:insert-into [(keyword table-name)]
       ;                                          :columns [:client_name :op_name :status]
       ;                                          :values [[client-name op-name (str "inserting rows... " x "/" (count rowset-fixed))]]})))
        ;query-sql (to-sql query)
        ;pool (cp/threadpool 15) ;; https://github.com/clj-commons/claypoole
        extra [ddl-str columns-vec-arg table-name table-name]]
    ;(ut/pp extra)
    ;(swap! row-counter assoc [client-name table-name] 0)
    (sql-exec system-db (to-sql {:insert-into [:status]
                                 :columns [:client_name :op_name :status]
                                 :values [[client-name op-name (str "inserting " (ut/nf (count rowset-fixed)) " rows... ")]]}))
    (sql-exec import-db (str "drop table if exists " table-name " ; ") extra)
    (sql-exec import-db ddl-str extra)
    ;(sql-exec db-conn insert-sql extra)
    (doseq [batch (partition-all batch-size values)] ; (cp/pdoseq pool
      (dorun
       (sql-exec import-db (to-sql {:insert-into [(keyword table-name)]
                                    :columns columns
                                    :values batch}) extra)
         ; (let [rrows (get-in @row-counter [client-name table-name])
         ;       newcnt (+ rrows batch-size)]
         ;   (swap! row-counter assoc-in [client-name table-name] newcnt)
         ; (sql-exec system-db
         ;           (to-sql {:insert-into [:status]
         ;                    :columns [:client_name :op_name :status]
         ;                    :values [[client-name op-name (str "inserting rows... " newcnt "/" (count rowset-fixed))]]})))
       ))
    (ut/pp {:sql-csv-table table-name :rows (count rowset)})))

(defn insert-rowset-old ;; [rowset query & columns-vec]
  "takes a 'rowset' (vector of uniform maps) or a vector with a vector of column names
   - inserts it into an in memory SQL db, executes a SQL query on it
   (via a honey-sql map) and returns it"
  ;; yes, I know this is inefficent as all hell compared to map,filter,reduce, etc - but I want the honeySQL DSL for consistency / end-user usability sake
  [rowset table-name & columns-vec]
  (try
    (let [rowset-type (cond (and (map? (first rowset)) (vector? rowset)) :rowset
                            (and (not (map? (first rowset))) (vector? rowset)) :vectors)
          columns-vec-arg (first columns-vec)
        ;columns-vec-arg-underscores (edn/read-string (cstr/replace (str columns-vec-arg) #"-" "_")) ;; ugly, but I need to keep the order
          db-conn cache-db ;cruiser/mem-db2
        ;{:classname   "org.sqlite.JDBC"
        ;          :subprotocol "sqlite"
        ;          :subname     "db/query-cache.db"}
          rowset-fixed (if (= rowset-type :vectors)
                         (vec (for [r rowset]
                                (zipmap columns-vec-arg r)
                              ;(map vector columns-vec-arg r)
                                ))
                         rowset)
        ;map-order (fn [amap order] (conj {} (select-keys amap order)))
       ; asort (fn [m order]
       ;         (let [order-map (apply hash-map (interleave order (range)))]
       ;           (conj
       ;            (sorted-map-by #(compare (order-map %1) (order-map %2))) ; empty map with the desired ordering
       ;            (select-keys m order))))
          columns (keys (first rowset-fixed))
          values (vec (for [r rowset-fixed] (vals r)))
          table-name-str (ut/unkeyword table-name)
        ;ddl-str (clickhouse-ddl/create-attribute-sample table-name-str rowset-fixed)
          ddl-str (sqlite-ddl/create-attribute-sample table-name-str rowset-fixed)
          insert-sql (to-sql {:insert-into [table-name]
                              :columns columns
                              :values values})
        ;query-sql (to-sql query)
          extra [ddl-str columns-vec-arg table-name table-name-str]]
    ;(ut/pp extra)
      (sql-exec db-conn (str "drop table if exists " table-name-str " ; ") extra)
      (sql-exec db-conn ddl-str extra)

      (sql-exec db-conn insert-sql extra)

   ; (let [results (sql-query db-conn query-sql)]
   ;   (sql-exec db-conn "drop table if exists rowset;")
   ;   (if (= rowset-type :vectors) ;; give it back in the format it was given
   ;     (vec
   ;      (for [r results]
   ;        (vec ;; need to return the vec in the same field order as we got it, not just random key order
   ;         (vals (asort r columns-vec-arg-underscores)))))
   ;     results))

      (ut/pp [:INSERTED-SUCCESS! (count rowset) :into table-name-str])

      {:sql-cache-table table-name :rows (count rowset)})
    (catch Exception e (ut/pp [:INSERT-ERROR! (str e) table-name]))))

(defn insert-rowset [rowset table-name & columns-vec]
  (if (not (empty? rowset))
    (try
      (let [rowset-type (cond (and (map? (first rowset)) (vector? rowset)) :rowset
                              (and (not (map? (first rowset))) (vector? rowset)) :vectors)
            columns-vec-arg (first columns-vec)
            db-conn cache-db
            rowset-fixed (if (= rowset-type :vectors)
                           ;(vec
                           (for [r rowset]
                             (zipmap columns-vec-arg r))
                           ; )
                           rowset)
            columns (keys (first rowset-fixed))

            table-name-str (ut/unkeyword table-name)
            ddl-str (sqlite-ddl/create-attribute-sample table-name-str rowset-fixed)

            extra [ddl-str columns-vec-arg table-name table-name-str]]
        (sql-exec db-conn (str "drop table if exists " table-name-str " ; ") extra)
        (sql-exec db-conn ddl-str extra)

        (doseq [batch (partition-all 100 rowset-fixed)
                :let [values (vec (for [r batch] (vals r)))
                      insert-sql (to-sql {:insert-into [table-name]
                                          :columns columns
                                          :values values})]]
          (sql-exec db-conn insert-sql extra))

        (ut/pp [:INSERTED-SUCCESS! (count rowset) :into table-name-str])
        {:sql-cache-table table-name :rows (count rowset)})
      (catch Exception e (ut/pp [:INSERT-ERROR! (str e) table-name])))
    (ut/pp [:cowardly-wont-insert-empty-rowset table-name :puttem-up-puttem-up!])))

(defn insert-rowset-plus-one  ;; [rowset query & columns-vec]
  "takes a 'rowset' (vector of uniform maps) or a vector with a vector of column names
   - inserts it into an in memory SQL db, executes a SQL query on it
   (via a honey-sql map) and returns it"
  ;; yes, I know this is inefficent as all hell compared to map,filter,reduce, etc - but I want the honeySQL DSL for consistency / end-user usability sake
  [rowset table-name & columns-vec]
  (let [rowset-type (cond (and (map? (first rowset)) (vector? rowset)) :rowset
                          (and (not (map? (first rowset))) (vector? rowset)) :vectors)
        columns-vec-arg (first columns-vec)
        db-conn cache-db ;cruiser/mem-db2
        rowset-fixed (if (= rowset-type :vectors)
                       (vec (for [r rowset]
                              (zipmap columns-vec-arg r)))
                       rowset)
        columns (keys (first rowset-fixed))
        values (vec (for [r rowset-fixed] (vals r)))
        table-name-str (ut/unkeyword table-name)
        ddl-str (ddl/create-attribute-sample table-name-str rowset-fixed)
        insert-sql (to-sql {:insert-into [table-name]
                            :columns columns
                            :values values})
        extra [ddl-str columns-vec-arg table-name table-name-str]]

    (sql-exec db-conn (str "drop table if exists " table-name-str " ; ") extra)
    (sql-exec db-conn ddl-str extra)
    (sql-exec db-conn insert-sql extra)
    {:sql-cache-table table-name :rows (count rowset)}))

(def external-changes (async/chan)) ;; anything we dump on here goes to the client (single client)

;; simple jetty websockets on seperate port with async queues for re-frame bi-directional comms
;; (defmethod wl/handle-subscription :dispatch-key [{:keys [kind ui-keypath extras]}]
;;   (let [results (async/chan)]
;;     (async/go-loop []
;;       (async/<! (async/timeout 7000))

;;      ; (when (async/>! results external-changes) (recur))

;;      ; (when
;;      ;  (async/>! results {:fuck-ducks? true :timestamp (System/currentTimeMillis)}
;;      ;                 )
;;      ;   (recur))
;;       )
;;     results))

;; (def work-queue (clojure.lang.PersistentQueue/EMPTY))

  ;; (defmethod wl/handle-subscription :server-push2 [{:keys [kind stuff]}]
  ;;   (let [results (async/chan)]
  ;;     (async/go
  ;;       ;(async/<! (async/timeout 5000))
  ;;       (while true ;(async/>! results {:subbed!? true})
  ;;            ;(async/<! (async/timeout 5000))
  ;;         (async/>! results (async/<!! external-changes))
  ;;            ;(async/<! (async/timeout 50))
  ;;         ;(recur)
  ;;         ))
  ;;     results))

;;  (defmethod wl/handle-subscription :server-push2 [{:keys [kind ui-keypath extras]}]
;;    (let [cc (async/chan)]
;;      (async/go-loop []
;;      ;(when-some [v (async/<! external-changes)]
;;         ;(async/<! (async/timeout 800))
;;         (async/>! cc (async/<! external-changes))

;;        ;external-changes
;;        (recur)
;;        )
;;      cc
;;      )
;;      )

;; (defmethod wl/handle-subscription :server-push2 [data]
  ;;   (let [results (async/chan)]
  ;;     (async/go-loop []
  ;;       ;(async/<! (async/timeout 5000))
  ;;       (when (async/<! (async/timeout 200))
  ;;                     (async/>! results (async/<!! external-changes));)
  ;;         (recur)
  ;;              ))
  ;;     results))

;; ;;; works as well as can be expected....
;; (defmethod wl/handle-subscription :server-push23333333-old [{:keys [kind ui-keypath client-name]}] ;; last working before queues
;;   (let [results (async/chan)]
;;     (ut/ppln [:subbed kind client-name])
;;     (async/pipeline 1 results (filter #(= (get % :client-name) client-name)) external-changes)
;;        ;(async/<!! (async/timeout 2500))
;;     (async/thread
;;       (async/go-loop []
;;         (async/<! (async/timeout 2500))
;;         (when
;;              ;(async/take 1 results)
;;          (async/<! results)
;;           (async/<! (async/timeout 2500))
;;           (recur))))
;;     results))


;;; good with single
(defonce push-queue (atom clojure.lang.PersistentQueue/EMPTY)) ;; needs to be on a PER CLIENT BASIS !!!!! TODO

(defonce tracker-history (atom {}))

(defonce queue-status (atom {}))
(defonce queue-data (atom {}))

;; (defmethod wl/handle-subscription :server-push2 [{:keys [kind ui-keypath client-name]}] ;; default subscription server->client push "queue"...
;;   (let [results (async/chan 100)]
;;     (async/go-loop []
;;       (async/<! (async/timeout 300)) ;; 600-800 seems ideal
;;       (if-let [item (ut/dequeue! push-queue)]
;;         (when (async/>! results item)
;;           (recur))
;;         (recur)))
;;     results))

;; (defn push-to-client [ui-keypath data client-name & [reco-count elapsed-ms]]
;;   (doall (doseq [_ (range 1)] ;; :/
;;    (swap! push-queue conj {:ui-keypath ui-keypath
;;                            :elapsed-ms elapsed-ms
;;                            :data [data (get (first reco-count) :cnt)]
;;                            :client-name client-name }))))
;;; good with single

;; (defmethod wl/handle-subscription :server-push2 [{:keys [kind ui-keypath client-name] :as data}]
;;   (try
;;     (let [results (async/chan 100)]
;;       (async/go-loop []
;;         (async/<! (async/timeout 600)) ;; was 300 ?
;;         (if-let [queue-atom (get @client-queues client-name (atom clojure.lang.PersistentQueue/EMPTY))]
;;           (let [item (ut/dequeue! queue-atom)]
;;             (if item
;;               (when (async/>! results item)
;;                 (recur))
;;               (recur)))
;;           (recur)))
;;       results)
;;     (catch Throwable e (ut/pp [:server-push2-subscription-err!! (str e) data]))))

;; (defmethod wl/handle-subscription :server-push2 [{:keys [kind ui-keypath client-name] :as data}]
;;   (try
;;     (let [results (async/chan (async/sliding-buffer 100))]
;;       (async/go-loop []
;;         (async/<! (async/timeout 600)) ;; was 300 ?
;;         (if-let [queue-atom (get @client-queues client-name (atom clojure.lang.PersistentQueue/EMPTY))]
;;           (let [item (ut/dequeue! queue-atom)]
;;             (if item
;;               (when (async/>! results item)
;;                 (recur))
;;               (recur)))
;;           (recur)))
;;       results)
;;     (catch Throwable e (ut/pp [:server-push2-subscription-err!! (str e) data]))))

(def all-pushes (atom 0))

(defn inc-score! [client-name key & [ts?]]
  (swap! all-pushes inc)
  (swap! ack-scoreboard assoc-in [client-name key]
         (if ts?
           [(System/currentTimeMillis) (ut/get-current-timestamp)]
           (inc (get-in @ack-scoreboard [client-name key] 0)))))

(declare client-statuses)

(defmethod wl/handle-request :ack [{:keys [client-name memory flow-subs]}]
  ;(ut/pp [:thank-you-from client-name])
  (inc-score! client-name :ack)
  (swap! client-latency assoc client-name
         (vec (conj (get @client-latency client-name [])
                    (try (- (System/currentTimeMillis) (get @ping-ts client-name)) (catch Exception _ -2)))))
  (let [cstats (client-statuses)
        ins-sql {:insert-into [:client-memory] :values [(-> memory
                                                            (assoc :latency (get-in cstats [client-name :client-latency]))
                                                            (assoc :server_subs (get-in cstats [client-name :server-subs]))
                                                            (assoc :client_subs (get-in cstats [client-name :client-subs]))
                                                            (assoc :recent-messages-per-second (get-in @ack-scoreboard [client-name :recent-messages-per-second]))
                                                            (assoc :messages-per-second (get-in @ack-scoreboard [client-name :messages-per-second]))
                                                            (assoc :mem_used_mb (ut/bytes-to-mb (get memory :mem_used))))]}]
    (sql-exec system-db (to-sql ins-sql)))
  (swap! ack-scoreboard assoc-in [client-name :memory] (ut/bytes-to-mb (get memory :mem_used)))
  (swap! ack-scoreboard assoc-in [client-name :client-sub-list] flow-subs)
  (swap! ack-scoreboard assoc-in [client-name :client-subs] (count flow-subs))
  (inc-score! client-name :last-ack true)
  ;(ut/pp [" - - - - - - - - - - - - - - - - - - - - - - - - - - - - - "])
  {})





;;; ORIGINAL HUMAN VERSION


(defonce client-queues (atom {}))
(defonce client-queues-2 (atom {}))
(defonce client-queues-3 (atom {}))
(defonce client-queue-atoms [client-queues]) ;; [client-queues client-queues-2 client-queues-3 ])

(defonce queue-distributions (atom {}))

(defn new-client [client-name]
  (ut/pp [:new-client-is-alive! client-name :opening (count client-queue-atoms) :queues])
  (swap! ack-scoreboard assoc-in [client-name :booted-ts] (System/currentTimeMillis))
  (doseq [cq client-queue-atoms]
    (let [new-queue-atom (atom clojure.lang.PersistentQueue/EMPTY)]
      (swap! cq assoc client-name new-queue-atom))))

;; (defn sub-push-loop [client-name data cq sub-name] ;; works, old human version
;;   (when (not (get @cq client-name)) (new-client client-name)) ;; new? add to atom, create queue
;;   (inc-score! client-name :booted true)
;;   (let [results (async/chan (async/sliding-buffer 100))] ;; was (async/sliding-buffer 450)
;;     (try
;;       (async/go-loop []
;;         (async/<! (async/timeout 70)) ;; was 70 ?
;;         (if-let [queue-atom (get @cq client-name (atom clojure.lang.PersistentQueue/EMPTY))]
;;           (let [item (ut/dequeue! queue-atom)]
;;             (if item
;;               (when (async/>! results item)
;;                 (recur))
;;               (recur)))
;;           (recur)))
;;       (catch Throwable e
;;         (ut/pp [:subscription-err!! sub-name (str e) data])
;;         (async/go-loop [] (recur))))
;;     results))


;; (defn sub-push-loop [client-name data cq sub-name] ;;; version 1 of batching, does NOT remove dupe task-ids
;;   (when (not (get @cq client-name)) (new-client client-name)) ;; new? add to atom, create queue
;;   (inc-score! client-name :booted true)
;;   (let [results (async/chan (async/sliding-buffer 100))] ;; was (async/sliding-buffer 450)
;;     (try
;;       (async/go-loop []
;;         (async/<! (async/timeout 500)) ;; was 70 ?
;;         (if-let [queue-atom (get @cq client-name (atom clojure.lang.PersistentQueue/EMPTY))]
;;           (let [items (loop [res []]
;;                         (if-let [item (ut/dequeue! queue-atom)]
;;                           (recur (conj res item))
;;                           res))]
;;             (if (not-empty items)
;;               (let [message (if (= 1 (count items)) (first items) items)]
;;                 (when (async/>! results message)
;;                   (recur)))
;;               (recur)))
;;           (recur)))
;;       (catch Throwable e
;;         (ut/pp [:subscription-err!! sub-name (str e) data])
;;         (async/go-loop [] (recur))))
;;     results))

(def valid-groups #{:flow-runner :tracker-blocks :acc-tracker :tracker :alert1})

(defn sub-push-loop [client-name data cq sub-name] ;; version 2, tries to remove dupe task ids
  (when (not (get @cq client-name)) (new-client client-name)) ;; new? add to atom, create queue
  (inc-score! client-name :booted true)
  (let [results (async/chan (async/sliding-buffer 100))] ;; was (async/sliding-buffer 450)
    (try
      (async/go-loop []
        (async/<! (async/timeout 600)) ;; was 70 ?
        (if-let [queue-atom (get @cq client-name (atom clojure.lang.PersistentQueue/EMPTY))]
          (let [items (loop [res []]
                        (if-let [item (ut/dequeue! queue-atom)]
                          (recur (conj res item))
                          res))
                items-by-task-id (group-by :task-id items)
                latest-items (mapv (fn [group]
                                     (if
                                      (contains? valid-groups (first group))
                                      ;; (or
                                      ;;  (= :flow-runner (first group))
                                      ;;  (= :tracker-blocks (first group))
                                      ;;  (= :acc-tracker (first group))
                                      ;;  (= :tracker (first group))
                                      ;;  (= :alert1 (first group)))
                                       group
                                       (last group)))
                                   (vals items-by-task-id))]
            (if (not-empty latest-items)
              (let [message (if (= 1 (count latest-items)) (first latest-items) latest-items)]
                (when (async/>! results message)
                  (recur)))
              (recur)))
          (recur)))
      (catch Throwable e
        (ut/pp [:subscription-err!! sub-name (str e) data])
        (async/go-loop [] (recur))))
    results))

(defmethod wl/handle-subscription :server-push2 [{:keys [kind client-name] :as data}]
  (sub-push-loop client-name data client-queues kind))

;; (defmethod wl/handle-subscription :server-push3 [{:keys [kind client-name] :as data}]
;;   (sub-push-loop client-name data client-queues-2 kind))

;; (defmethod wl/handle-subscription :server-push4 [{:keys [kind client-name] :as data}]
;;   (sub-push-loop client-name data client-queues-3 kind))

(defn push-to-client [ui-keypath data client-name queue-id task-id status & [reco-count elapsed-ms]]
  ;(doall
  (enqueue-task5
   (fn []
     (try
       (let [rr 0 ;(rand-int 3)
             cq (get client-queue-atoms rr)
             _ (swap! queue-distributions assoc client-name
                      (vec (conj (get @queue-distributions client-name []) rr)))
             client-queue-atom (get @cq client-name)]
         (swap! queue-status assoc-in [client-name task-id ui-keypath] status)
         (swap! queue-data assoc-in [client-name task-id ui-keypath] {:data data :reco-count reco-count :elapsed-ms elapsed-ms})
         (if client-queue-atom
           (do
          ;;  (ut/pp [:PUSH-to-client! client-name ui-keypath task-id status data])
             (inc-score! client-name :push)
             (inc-score! client-name :last-push true)
             (swap! client-queue-atom conj
                    {:ui-keypath ui-keypath
                     :status status
                     :elapsed-ms elapsed-ms
                     :reco-count reco-count
                     :queue-id queue-id
                     :task-id task-id
                     :data [data  ;; data is likely needed for :payload and :payload-kp that kits need? but we can revisit later... 
                                      ;; not sure if kits will ship in current form. doubtful actually
                                      ;; in the meantime this takes a chunk out of client memory reqs  || 5/10/25 12:35am
                            (try (get (first reco-count) :cnt) (catch Exception _ reco-count))]
                     :client-name client-name}))
         ;(doall
           (let [] ;[new-queue-atom (atom clojure.lang.PersistentQueue/EMPTY)]
             (new-client client-name)
             (push-to-client ui-keypath data client-name queue-id task-id status reco-count elapsed-ms))));)
       (catch Throwable e (ut/pp [:push-to-client-err!! (str e) data]))))));)
;;; ORIGINAL HUMAN VERSION


;;; saturday ai version
;; (defonce client-queues (atom {}))

;; (defn get-or-create-queue [client-name]
;;   (or (get @client-queues client-name)
;;       (let [new-queue-atom (atom clojure.lang.PersistentQueue/EMPTY)]
;;         (swap! client-queues assoc client-name new-queue-atom)
;;         new-queue-atom)))

;; (defmethod wl/handle-subscription :server-push2 [{:keys [client-name] :as data}]
;;   (let [results (async/chan (async/sliding-buffer 10))]
;;     (try
;;       (async/go-loop []
;;         (let [queue-atom (get-or-create-queue client-name)
;;               ;; Wait for a timeout or an item, whichever comes first
;;               item-or-timeout (async/alts! [(async/timeout 1000) (ut/dequeue! queue-atom)])]
;;           (when (and (second item-or-timeout) (async/>! results (first item-or-timeout)))
;;             (recur)))) ;; recur is now in tail position
;;       (catch Throwable e
;;         (ut/pp [:server-push2-subscription-err!! (str e) data])
;;         (async/go-loop [] (recur)))) ;; recur is also in tail position here
;;     results))

;; (defn push-to-client [ui-keypath data client-name queue-id task-id status & [reco-count elapsed-ms]]
;;   (try
;;     (let [client-queue-atom (get-or-create-queue client-name)]
;;       (swap! queue-status assoc-in [client-name task-id ui-keypath] status)
;;       (swap! queue-data assoc-in [client-name task-id ui-keypath] {:data data :reco-count reco-count :elapsed-ms elapsed-ms})
;;       (ut/pp [:push-to-client client-name ui-keypath status reco-count elapsed-ms])
;;       (inc-score! client-name :push)
;;       (swap! client-queue-atom conj
;;              {:ui-keypath ui-keypath
;;               :status status
;;               :elapsed-ms elapsed-ms
;;               :reco-count reco-count
;;               :queue-id queue-id
;;               :task-id task-id
;;               :data [data (try (get (first reco-count) :cnt) (catch Exception _ reco-count))]
;;               :client-name client-name}))
;;     (catch Throwable e (ut/pp [:push-to-client-err!! (str e) data]))))
;;; saturday ai version


;;; ai version with multiple channels?
;; (def client-queues (atom {}))

;; (defn add-client [client-name]
;;   (let [client-queue (atom clojure.lang.PersistentQueue/EMPTY)]
;;     (swap! client-queues assoc client-name client-queue)
;;     client-queue))

;; (defn remove-client [client-name]
;;   (swap! client-queues dissoc client-name))

;; (def incoming-messages (async/chan))

;; (defn push-to-client [ui-keypath data client-name queue-id task-id status & [reco-count elapsed-ms]]
;;   (println "push-to-client called with" client-name)
;;   (when-not (get @client-queues client-name)
;;     (add-client client-name))
;;   (let [message-added (async/offer! incoming-messages {:client-name client-name :message {:ui-keypath ui-keypath
;;                                                                                           :status status
;;                                                                                           :elapsed-ms elapsed-ms
;;                                                                                           :reco-count reco-count
;;                                                                                           :queue-id queue-id
;;                                                                                           :task-id task-id
;;                                                                                           :data [data (try (get (first reco-count) :cnt) (catch Exception _ reco-count))]
;;                                                                                           :client-name client-name}})]
;;     (println "Message added to incoming-messages:" message-added)))


;; (defmethod wl/handle-subscription :server-push2 [{:keys [kind ui-keypath client-name] :as data}]
;;   (println "handle-subscription called with" client-name)
;;   (let [results (async/chan (async/sliding-buffer 100))]
;;     (async/go-loop []
;;       (println "In go-loop")
;;       (let [[value channel] (async/alts! [incoming-messages])]
;;         (println "Received value from" channel)
;;         (cond
;;           (= channel incoming-messages)
;;           (let [{:keys [client-name message]} value
;;                 client-queue (get @client-queues client-name)]
;;             (when client-queue
;;               (swap! client-queue conj message)
;;               (async/put! results message (fn [_] (println "Message sent to client")))))
;;           :else
;;           (if-let [client-queue (get @client-queues client-name)]
;;             (let [item (ut/dequeue! client-queue)]
;;               (when item
;;                 (async/put! results item (fn [_] (println "Item sent to client")))))
;;             (do
;;               (println "Creating new client queue for" client-name)
;;               (swap! client-queues assoc client-name (async/chan (async/sliding-buffer 100))))))  ;; Move recur here
;;       (recur)))
;;     results))
;;; ai version with multiple channels?


;; second ai version.... WORKS?!?!
;; (def client-queues (atom {}))

;; (defn add-client [client-name]
;;   ;(println (str "add-client " client-name))
;;   (swap! client-queues
;;          #(if-not (get % client-name)
;;             (assoc % client-name (atom clojure.lang.PersistentQueue/EMPTY))
;;             %)))

;; (defn remove-client [client-name]
;;   (swap! client-queues dissoc client-name))

;; (defn push-to-client [ui-keypath data client-name queue-id task-id status & [reco-count elapsed-ms]]
;;   ;(add-client client-name) ; Ensure client is added if not present
;;   (let [client-queue (get @client-queues client-name)
;;         message {:ui-keypath ui-keypath
;;                  :status status
;;                  :elapsed-ms elapsed-ms
;;                  :reco-count reco-count
;;                  :queue-id queue-id
;;                  :task-id task-id
;;                  :data [data (try (get (first reco-count) :cnt) (catch Exception _ reco-count))]
;;                  :client-name client-name}]
;;     (swap! client-queue conj message)))

;; ;; works well, some hiccups
;; ;; (defmethod wl/handle-subscription :server-push2 [{:keys [kind ui-keypath client-name] :as data}]
;; ;;   (let [results (async/chan )]
;; ;;     (async/go-loop []
;; ;;       (if-let [client-queue (get @client-queues client-name)]
;; ;;         (when-let [item (first @client-queue)]
;; ;;           (async/put! results item
;; ;;                       (fn [_]
;; ;;                         (swap! client-queue rest))))
;; ;;         ;(println "No queue found for client" client-name "; unable to send messages.")
;; ;;         (add-client client-name))
;; ;;       (recur))
;; ;;     results))

;; ;; less hiccups? more
;; (defmethod wl/handle-subscription :server-push2 [{:keys [kind ui-keypath client-name] :as data}]
;;   (let [results (async/chan)]
;;     (async/go-loop []
;;       (try
;;         (if-let [client-queue (get @client-queues client-name)]
;;           (when-let [item (first @client-queue)]
;;             (async/put! results item
;;                         (fn [_]
;;                           (swap! client-queue rest))))
;;           ; Optionally, handle the case when no queue is found
;;           ;(println "No queue found for client" client-name "; unable to send messages.")
;;           (add-client client-name))
;;         (catch Exception e
;;           ;; Handle exception, e.g., log it
;;           (println "Exception in go-loop:" (.getMessage e))))
;;       (recur)) ; This ensures the loop continues even after an exception
;;     results))
;; second ai version....

;; third ai version
;; (def client-queues (atom {}))

;; (defn add-client [client-name]
;;   (swap! client-queues
;;          #(if-not (get % client-name)
;;             (assoc % client-name (async/chan 100))
;;             %)))

;; (defn remove-client [client-name]
;;   (swap! client-queues dissoc client-name))

;; (defn push-to-client [ui-keypath data client-name queue-id task-id status & [reco-count elapsed-ms]]
;;   (add-client client-name) ; Ensure client is added if not present
;;   (let [client-chan (get @client-queues client-name)
;;         message {:ui-keypath ui-keypath
;;                  :status status
;;                  :elapsed-ms elapsed-ms
;;                  :reco-count reco-count
;;                  :queue-id queue-id
;;                  :task-id task-id
;;                  :data [data (try (get (first reco-count) :cnt) (catch Exception _ reco-count))]
;;                  :client-name client-name}]
;;     (async/put! client-chan message)))


;; (defmethod wl/handle-subscription :server-push2 [{:keys [client-name] :as data}]
;;   (let [results (async/chan)]
;;     (async/go-loop []
;;       (try
;;         (if-let [client-chan (get @client-queues client-name)]
;;           (let [[value channel] (async/alts! [client-chan (async/timeout 5000)])] ; 5000 ms timeout
;;             (cond
;;               (= channel client-chan)
;;               (when value
;;                 (async/put! results value)
;;                 (println (str "value put for client" client-name))
;;                 ;(swap! client-chan rest)
;;                 ) ; Remove the processed message from the queue

;;               (= channel (async/timeout 5000))
;;               (do
;;                 (println "Timeout for client" client-name "; resetting channel.")
;;                 (swap! client-queues dissoc client-name)))) ; Remove the unresponsive client's channel

;;           ;; If no channel exists for this client, create it
;;           (add-client client-name))

;;         (catch Exception e
;;           (println "Exception in go-loop for client" client-name ":" (.getMessage e))))
;;       (recur))
;;     results))
;; third ai version


(defn react-to-file-changes [{:keys [path type]}]
  ;p(let [path (get x :path)])
  (doall (let [;file-data (try (edn/read-string (slurp (str path))) (catch Exception e [:invalid-edn! (str e)]))
               splt (cstr/split (str path) #"/")
        ;panel-key (keyword (cstr/replace (str (last splt)) #".edn" ""))
               client-name (keyword (cstr/replace (str (get splt 2)) #".edn" ""))
               root-file (str "./live/" (ext/fixstr client-name) ".edn")
               tab-name (str (get splt 3))
               block-name (str (get splt 4))
               source (try (read-string (slurp root-file)) (catch Exception e {:invalid-edn! (str e)}))

               name-mapping-raw (into {} (for [[k v] source] {k (get v :name)}))
               name-mapping (ut/generate-unique-names name-mapping-raw)
               rev-name-mapping (ut/reverse-map name-mapping)

               panel-key (get rev-name-mapping block-name)
              ; panel-key (if (nil? panel-key)
              ;             (let [] ; [block-names (ext/get-subdirs (str "./live/" client-name "/" tab-name "/" block-name))]
              ;               ;(ut/pp [:need-panel-key block-names splt (count splt)])
              ;               (keyword (str "ren-block-" (rand-int 123))))
              ;             panel-key)

        ;client-dir (str "./live/" client-name "/")

               valid-edn? true
               repacked (ut/remove-empty-sub-keys (try (ext/repack-block client-name tab-name block-name panel-key)
                                                       (catch Exception e {:repack-err! (str e)})))
               oldpacked (ut/remove-empty-sub-keys (get source panel-key))
               new-from-server? (and (nil? oldpacked) (= type :create))
               delete-from-server? (and (nil? repacked) (= type :delete))
        ;new-from-client? (and (nil? repacked) (= type :create))
        ;new-incoming? (and (not (empty? oldpacked)) (nil? repacked))
               we-did-it? (= repacked oldpacked) ;; either we have authority over this change of the overwrite is a side-effect of a client update
               ignore? (or (cstr/ends-with? path ".swp")
                    ;new-incoming?
                           (= type :create)
                    ;new-from-client?
                    ;(and (nil? repacked) (= type :create))
                           (= type :delete)
                           (= (str path) root-file))]

    ;(when delete-from-server? (ut/pp [:delete-from-server panel-key]))

           (when (and new-from-server? false) ;; disable for now... WAAAAAY too fucky.
             (let [;panel-key (if (nil? panel-key) (keyword (str "ren-block-" (rand-int 1234))) panel-key)
                   subd (str "./live/" (ext/fixstr client-name) "/" tab-name "/")
                   curr-names (ext/get-subdirs (cstr/replace subd "//" ""))
                   cntsplt (count splt)
                   type-of (cond (= cntsplt 3) :nothing
                                 (= cntsplt 5) :block-rename
                                 :else :?)
                   missing (cset/difference (set (keys rev-name-mapping)) (set curr-names))
                   panel-key (if (and (= 1 (count missing)) (nil? panel-key))
                               (keyword (first missing))
                               panel-key)
                   block-data (assoc (merge (get repacked panel-key) (dissoc repacked panel-key)) :tab tab-name)]
               (when (= cntsplt 5) ;; block-rename or move
                 (do
                   (ut/pp [:create-on-server new-from-server? :pushing-new curr-names (vec missing) subd cntsplt  type-of ;;panel-key ;we-did-it?
                           {:path (str path) :type type
                            :panel-key panel-key :block-data block-data}
              ;(nil? oldpacked) (nil? repacked) (empty? oldpacked) (empty? repacked) (data/diff repacked oldpacked) repacked oldpacked
                           ])
                   (push-to-client [:file-changed]
                                   {:path (str path) :type type
                                    :panel-key panel-key :block-data block-data} client-name 0 :file-change :done)))))

    ;; (when new-from-server? ;; delete from client
    ;;   (ut/pp [:create-on-server new-from-server?
    ;;           ;(nil? oldpacked) (nil? repacked) (empty? oldpacked) (empty? repacked) (data/diff repacked oldpacked) repacked oldpacked
    ;;           ]))

           (when (and valid-edn? (not ignore?))
             (do (ut/pp [:changed-file! (str path) type client-name panel-key :splt-cnt (count splt)
                         (when (not we-did-it?) {;:repacked repacked
                                          ;:oldpacked oldpacked
                                                 :diff (data/diff repacked oldpacked)})])
                 (when (not we-did-it?)
                   (push-to-client [:file-changed]
                                   {:path (str path) :type type
                                    :panel-key panel-key :block-data repacked} client-name 0 :file-change :done)))))))

(defn subscribe-to-session-changes []
  (let [file-path0 (str "./live/")]
    (do
      ;(shell/sh "/bin/bash" "-c" (str "mkdir -p " file-path0))
      (ut/pp [:beholder-watching file-path0])
      (beholder/watch
      ;;  #(when (and (not (cstr/ends-with? (str (get % :path)) ".rabbit"))
      ;;                             ;(not (cstr/includes? (str (get % :path)) ))
      ;;                             (not (cstr/includes? (str (get % :path)) "/.")))
      ;;                    ;; we dont care about the main file, that changes constantly
      ;;                    (let [client-id (sf-nth (cstr/split (str (get % :path)) #"/") 2)
      ;;                          update-map (merge (get-update-details (str (get % :path))) {:file-op (get % :type)})
      ;;                          stream-blocks (vec (apply concat (for [v (vals @who)]
      ;;                                                             (for [s (get v :stream-subs)]
      ;;                                                               [(get s :flow-name) (get s :block-id)]))))
      ;;                          vk [(get-in update-map [:real-vals :flow-name])
      ;;                              (get-in update-map [:real-vals :block-id])]]
      ;;                      (when (not
      ;;                             (some (fn [x] (= vk x))
      ;;                                   stream-blocks)) ;; ignore stream block changes, can cause queue thrash...
      ;;                        (do
      ;;                          (println (tc/bold (tc/yellow  "  !!!!  file " (get % :type) " " (str (get % :path)) " !!!! " client-id)))
      ;;                          (println (str vk "\n " stream-blocks "\n " @who))
      ;;                      ;(async/put! external-changes (merge (get-update-details (str (get % :path))) {:file-op (get % :type)}))
      ;;                          (swap! queue2 conj update-map)
      ;;                    ;    (println [:EDITOR-PUSH (merge (get-update-details (str (get % :path))) {:file-op (get % :type)})])
      ;;                        ;(swap! queue2 assoc client-id (merge (get-update-details (str (get % :path))) {:file-op (get % :type)}))
      ;;                          ))))
       #(send-off ext/file-work-agent (react-to-file-changes %))
       file-path0))))

(defonce last-panels (atom {}))

(defn hash-objects [panels]
  (into {} (for [[k v] panels]
             {k {:base (hash v) ; (hash (-> v (dissoc :views) (dissoc :queries)))
                 :views (into {} (for [[k v] (get v :views {})] {k (hash v)}))
                 :queries (into {} (for [[k v] (get v :queries {})] {k (hash v)}))}})))

(defn data-objects [panels]
  (into {} (for [[k v] panels]
             {k {:base v ;(-> v (dissoc :views) (dissoc :queries))
                 :views (into {} (for [[k v] (get v :views {})] {k v}))
                 :queries (into {} (for [[k v] (get v :queries {})] {k v}))}})))

(def panel-history (agent nil))
(set-error-mode! panel-history :continue)

(defmethod wl/handle-request :current-panels [{:keys [panels client-name resolved-panels]}] ;; TODO add these
  (ext/write-panels client-name panels) ;; pushn to file system for beholder cascades
  (swap! panels-atom assoc client-name resolved-panels) ;; save to master atom for reactions, etc 
  ;(ut/pp [:panels (get panels :block-7416)])
  (let [prev-hashes (hash-objects (get @last-panels client-name))
        this-hashes (hash-objects panels)
        diffy (data/diff this-hashes prev-hashes)
        diffy-kps (vec (filter #(and (not (= (count %) 1))
                                     (not (= (last %) :views))
                                     (not (= (last %) :queries)))
                               (ut/kvpaths (first diffy))))
        dd (data-objects panels)
        pdd (data-objects (get @last-panels client-name))]

    (send panel-history ;; shouldnt the whole thing be in an agent sync block? rapid updates seem to clash
          (fn [_]
            ;(ut/pp [:panels-diff-objects? (first diffy) diffy-kps])
            (let [;dd (data-objects panels)
                  ;pdd (data-objects (get @last-panels client-name))
                  rows (vec (for [kp diffy-kps
                                  :let [data (get-in dd kp)
                                        pdata (get-in pdd kp)
                                        pdiff (first (data/diff data pdata))]]
                              {:kp (str kp) :client_name (str client-name) :data (pr-str data) :pre_data (pr-str pdata)
                               :diff (pr-str pdiff) :diff_kp (pr-str (ut/kvpaths pdiff))
                               :panel_key (str (get kp 0)) :key (str (get kp 2)) :type (str (get kp 1))}))
                  ins-sql {:insert-into [:panel-history] :values rows}
                  board-ins-sql {:insert-into [:board_history] :values [{:client_name (str client-name) :data (pr-str panels)}]}]
              (sql-exec system-db (to-sql board-ins-sql))
              (when (not (empty? rows))
                (sql-exec system-db (to-sql ins-sql))))))

    (swap! last-panels assoc client-name panels) ;; lastly update last
  ;(ut/pp [:received-panels-data-from client-name])
    ))

(defn run-shell-command
  "execute a generic shell command and return output as a map of timing and seq of str lines"
  [command]
  (let [output (shell/sh "/bin/bash" "-c" (str "mkdir -p shell-root ; cd shell-root ; " command))
        ;output (shell/sh "/bin/bash" "-c" command)
        split-lines (vec (remove empty? (cstr/split-lines (get output :out))))
        exit-code (get output :exit)
        error (vec (remove empty? (cstr/split-lines (get output :err))))
        has-timing? (if (not (empty? error)) (cstr/starts-with? (get error 0) "real") false)
        error-data (if has-timing? [] error)
        timing-values-to-seconds #(let [split-timing (cstr/split (get (cstr/split % #"\t") 1) #"m")
                                        minutes (edn/read-string (get split-timing 0))
                                        seconds (edn/read-string (cstr/join "" (drop-last (get split-timing 1))))]
                                    (+ (* 60 minutes) seconds))

        timing-data (if has-timing? (into [] (for [x error] (timing-values-to-seconds x))) [])]
    {;;  :output [{:output split-lines
    ;;            :exit-code exit-code
    ;;            :error error-data}]
     :output split-lines
     :exception error-data
     ;:exit exit-code
     ;:error error-data
     :seconds timing-data
     :command (str command)}))

(defn read-local-file
  [full-path]
  (let [fqd? (or (cstr/starts-with? full-path "/")
                 (cstr/starts-with? full-path "~"))
        output (run-shell-command "pwd")
        pwd (first (get-in output [:output :output] []))
        full-path (if fqd? full-path (str pwd "/" full-path))]
    (ut/pp [:reading-file full-path])
    (try
      {:file-data (str (slurp full-path))
       :error nil}
      (catch Exception e
        {:file-data (str  "\n" (str (.getMessage e)) "\n")
         :error nil ;(str "read-local-file, caught exception: " (.getMessage e))
         }))))

(defn write-local-file
  [full-path file-data]
  (let [fqd? (or (cstr/starts-with? full-path "/")
                 (cstr/starts-with? full-path "~"))
        output (run-shell-command "pwd")
        pwd (first (get-in output [:output :output] []))
        full-path (if fqd? full-path (str pwd "/" full-path))]
    ;(println (str fqd? pwd))
    (ut/pp [:writing-file full-path])
    (do (try
          (spit full-path file-data)
      ;"hello"
          (catch Exception e
            (do (println "err")
                {;:file-data file-data
                 :status :error
                 :file-path full-path
                 :error (str "caught exception: " (.getMessage e))})))
        {:status :ok
         :file-path full-path})))

;; (defn get-audio [request] ;;; for streaming local audio file to browser - see prompt-flow/server
;;   (let [jss (get request :json-params)
;;         path (get jss :path)
;;         file (java.io.File. (str path))]
;;     (if (.exists file)
;;       (-> (ring-resp/response (java.io.FileInputStream. file))
;;           (ring-resp/content-type "audio/mpeg"))
;;       (ring-resp/not-found "File not found"))))

(defn get-audio [request]
  (let [jss (get request :json-params)
        path (get jss :path)
        file (if (clojure.string/starts-with? path "http")
               (let [url (java.net.URL. path)
                     connection (.openConnection url)
                     input-stream (.getInputStream connection)
                     file-path (str "/tmp/" (last (clojure.string/split path #"/")))
                     file (java.io.File. file-path)]
                 (with-open [output-stream (java.io.FileOutputStream. file)]
                   (clojure.java.io/copy input-stream output-stream))
                 file)
               (java.io.File. (str path)))]
    (if (.exists file)
      (-> (ring-resp/response (java.io.FileInputStream. file))
          (ring-resp/content-type "audio/mpeg"))
      (ring-resp/not-found "File not found"))))

(defmethod wl/handle-request :get-settings [{:keys [client-name]}]
  (ut/pp [:client client-name :just-booted])
  (merge config/settings {:kits config/kit-fns
                          :screens (vec (map :screen_name
                                             (sql-query system-db
                                                        (to-sql {:select   [:screen_name]
                                                                 :from     [[:screens :jj24a7a]]
                                                                 :group-by [:screen_name]
                                                                 :order-by [[1 :asc]]}))))}))

;(def last-run (atom nil))
;(defonce params-atom (durable-atom "./data/params-atom.edn")) ;(atom {}))
;(def params-atom (atom {}))
(def times-atom (ut/thaw-atom {} "./data/atoms/times-atom.edn"))
(def params-atom (ut/thaw-atom {} "./data/atoms/params-atom.edn"))
(def atoms-and-watchers (atom {}))

;;(def last-values (atom {}))
;(defonce last-values (durable-atom "./data/flow-sub-last-values.edn")) ;; so we can persist between server sessions
;(def last-values (atom {}))
(def last-values (ut/thaw-atom {} "./data/atoms/last-values.edn"))
(def last-values-per (ut/thaw-atom {} "./data/atoms/last-values-per.edn"))

(defmethod wl/handle-request :client-ui [{:keys [client-name atom-name value]}]
  (swap! params-atom assoc-in [client-name (keyword atom-name)] value)
  (ut/pp [:client-ui-atom client-name atom-name value])
  {}) ;; send nothing

(defn client-statuses []
  (into {}
        ;(remove (fn [[_ v]] (= -1 (:last-seen-seconds v))) ;; just take out any dead ones for now
        (for [[k v] @ack-scoreboard]
          {k (let [seconds-ago (int (/ (- (System/currentTimeMillis)
                                          (get-in v [:last-ack 0] 0)) 1000))
                   never? (nil? (get-in v [:last-ack 0]))]
               (-> v
                   (assoc :queue-distro (frequencies (get @queue-distributions k)))
                   (assoc :queue-size (count @(get @client-queues k)))
                   (assoc :client-latency (ut/avg (take-last 20 (get @client-latency k []))))
                   (assoc :server-subs (count (keys (get @atoms-and-watchers k))))
                   (assoc :last-seen-seconds (if never? -1 seconds-ago))
                   (dissoc :client-sub-list)
                   (assoc :last-seen (cond (< seconds-ago 0) "not since boot"
                                           (= k :rvbbit-scheduler) "n/a"
                                           :else (ut/format-duration-seconds seconds-ago)))))})));)

(declare flow-kill!)
(declare alert!)

(defn flow-statuses []
  (into {} (for [[k {:keys [*time-running *running? *started-by *finished overrides started waiting-blocks running-blocks]}] @flow-status
                 :let [chans (count (get @flow-db/channels-atom k))
                       chans-open (count
                                   (doall
                                    (map (fn [[_ ch]]
                                           (let [vv (try (not (ut/channel-open? ch))
                                                         (catch Throwable e (str e)))]
                                             (if (cstr/includes? (str vv) "put nil on channel") :open vv)))
                                         (get @flow-db/channels-atom k))))
                       channels-open? (true? (> chans-open 0))
                                                ;_ (swap! flow-status assoc-in [k :*channels-open?] channels-open?)
                                                ;_ (swap! flow-status assoc-in [k :*channels-open] chans-open)
                                                ;; ^^ TODO, important should be elsewhere - but is an expensive check, so passively here makes some sense
                                                ;;_ (swap! channel-counts assoc k {:channels chans :channels-open chans-open}) ;; wtf is this? unused
                       last-update-seconds (int (/ (- (System/currentTimeMillis) (get @watchdog-atom k)) 1000))
                       human-elapsed (if *running?
                                       (ut/format-duration started (System/currentTimeMillis))
                                       *time-running)
                       last-update (get @last-block-written k)
                       _ (when (and (> last-update-seconds 240) (empty? running-blocks) (not (nil? last-update)) *running?)
                           (alert! *started-by [:box
                                                :style {:color "red"}
                                                :child (str "ATTN: flow " k " killed by rabbit watchdog for going idle - TODO")] 15 1 60)
                           (flow-kill! k :rvbbit-watchdog))]]

             {k {:time-running *time-running
                 :*running? *running?
                 ;:tracker-events tracker-events
                 :retries-left (get @restart-map k -1)
                 :*started-by *started-by
                 :last-update-seconds (when *running? last-update-seconds)
                 :last-updated (when *running? (ut/format-duration-seconds last-update-seconds))
                 :last-update last-update
                 :running-blocks running-blocks
                 :block-overrides (vec (keys overrides))
                 :since-start (str human-elapsed)
                 :waiting-blocks waiting-blocks
                 ;:timeouts (count (filter #(= % :timeout) (ut/deep-flatten tracker-events)))
                 ;:skips (count (filter #(= % :skip) (ut/deep-flatten tracker-events)))
                 :channels-open? channels-open?
                 :channels chans
                 :channels-open chans-open
                 :blocks_finished *finished}})))

;; (defn close-channels! [uid]
;;   (ut/pp [:closing-channels uid (get @flow-db/channels-atom uid)]))

(defn flow-waiter [in-flow uid & [opts]]
  (let [a (atom nil)
        post-id (if (get opts :increment-id? false) ;; if auto increment idx, lets use that for lookups...
                  (str uid "-" (count (filter #(cstr/starts-with? % uid) (keys @flow-db/channel-history))))
                  uid)
        debug? false ;true ; false
        overrides (get opts :overrides nil)
        flow-opts (merge {:debug? debug?
                          :close-on-done? true ; false ;true
                          :flow-id uid} opts)
        uid post-id] ;; swap to post-run ID ;; temp
    ;(swap! flow-status assoc-in [uid :*running?] true)
    ;(Thread/sleep 1800) ;; testing
    ;(ut/pp [:flow-running in-flow])
    ;(ut/pp [:flow-diff (data/diff in-flow @last-run)])
    ;(reset! last-run in-flow)
     ;(ut/pp (vec (remove nil? [:testing-flow-atom-output])))
    (doall (flow/flow in-flow flow-opts a overrides))
    (while (and (nil? @a) (not (some #(get % :error) (map :value (get @flow-db/channel-history uid)))))
      (let [;chist (get @flow-db/channel-history uid)
            ;errors? (some #(get % :error) (map :value (get @flow-db/channel-history uid)))
            ]
        ;(ut/pp {:running errors? :chist chist})
        (Thread/sleep 100))) ;; check the atom every 100 ms
    (let [err? (some #(get % :error) (map :value (get @flow-db/channel-history uid)))]
      (when err? (flow/close-channels! uid))
      (or @a (some #(get % :error) (map :value (get @flow-db/channel-history uid)))))))

(defn gn [x] (try (name x) (catch Exception _ x)))
(defn gns [x] (try (namespace x) (catch Exception _ x)))

(defmacro create-fn [k v]
  (let [str? (string? v)
        inputs (get v :inputs)
        syms (vec (map (fn [_] (gensym)) inputs))
        bindings (into {} (map vector inputs syms))]
    `(def ~k
       (fn ~syms
         (let [walkmap ~bindings]
           (if ~str?
             (ut/template-replace walkmap ~v)
             (walk/postwalk-replace walkmap ~v)))))))




(declare materialize-flowmap)

;;; materialize-flowmap  [client-name flowmap flow-id opts & [no-eval?]]

;; (defn process-flow-paths [m]
;;   (cond
;;     (map? m)
;;     (if-let [flow-path (:flow-path m)]
;;       (let [sub-flow-id (:sub-flow-id m)
;;             new-map (materialize-flowmap :client-name flow-path sub-flow-id  {} true)]
;;         (process-flow-paths new-map))
;;       (into {} (map (fn [[k v]] [k (process-flow-paths v)]) m)))
;;     (coll? m)
;;     (mapv process-flow-paths m)
;;     :else m))

;; (defn process-nested-map [m]
;;   (let [processed-map (process-flow-paths m)]
;;     (if (= processed-map m)
;;       m
;;       (recur processed-map))))

(defn process-flowmaps [flowmap sub-map]
  ;;(ut/pp [:processing-nested-flowmap (process-nested-map flowmap)])
  (doall (let [flowmap-comps (into {} (for [[k v] (get flowmap :components)
                                            ;:let [_ (ut/pp [:*processing-comp k v])]
                                            :let [;vv v ;; for debugging
                                                  v (if (get v :flow-path) (materialize-flowmap (get sub-map :client-name) (get v :flow-path) (get v :sub-flow-id) {} true) v)
                                                  ;_ (ut/pp [:process-flowmaps k vv v ])
                                                  ]]
                                        (walk/postwalk-replace
                                         {:block-id k
                                          :block-id-str (str k)
                                          :bid k
                                          :bids (str k)}
                                         (cond (not (empty? (get v :raw-fn))) ;; user provided anon fn
                                              ;; (try
                                               (let [fn-raw (get v :raw-fn)]
                                                 {k (-> v (assoc :fn fn-raw) (dissoc :raw-fn))})
                                                ;; (catch Throwable e (do (ut/pp [:thrown v])
                                                ;;                        {k {:error "cant process fn" :e (str e)}}))
                                                ;; )

                                              ;;  (not (empty? (get v :file-path))) ;; pre-packed subflow from file
                                              ;;  (let [file-path (get v :file-path)
                                              ;;        sflow-id (get v :flow-id nil)
                                              ;;        vv (materialize-flowmap (get sub-map :client-name) file-path sflow-id {} true)] ;; <-- true flag is to not pre-eval fn forms
                                              ;;    {k (assoc (process-flowmaps vv sub-map) :description [file-path sflow-id])})

                                               (not (empty? (get v :components))) ;; unpacked subflow! (legacy saved flows and post unpacking)
                                               ;; or :file-path not empty?
                                               {k (assoc (process-flowmaps v sub-map) :description [(get v :file-path nil) (get v :flow-id nil)])} ;; sneaky sneaky
                                       ;;; ^^^ need to load file from disk instead of using the saved flowmap in the embedded flow... TODO
                                       ;;; we just need to copy the server flowmap version, or post-process it here using FE's fn
                                              ;; (and (or (not (map? v)) (and (map? v) (nil? (get v :fn-key)))) ;; ""static"" value w/o a fn BUT with inputs!
                                              ;;      (nil? (get v :raw-fn)) ;(and (nil? (get v :raw-fn)) (nil? (get v :fn)))
                                              ;;      (not (empty? (get v :inputs))))
                                               (and (not (empty? (get v :data)))
                                                    (not (empty? (get v :inputs)))) ;; templated input "static"
                                               (let [vv (get v :data)
                                                     str? (string? vv)
                                                     inputs (get v :inputs)
                                                    ;syms (vec (map (fn [_] (gensym)) inputs))
                                                    ;bindings (into {} (map vector inputs syms))
                                                     replace-fn (if str? 'rvbbit-backend.util/template-replace 'clojure.walk/postwalk-replace)
                                                     fn-form (walk/postwalk-replace
                                                              {:inputs inputs
                                                               :replace-fn replace-fn
                                                               :data vv}
                                                              '(fn [& args]
                                                                 (let [m (zipmap :inputs args)]
                                                                   (:replace-fn m :data))))]
                                                ;(ut/pp [:spooky-fn! inputs fn-form])
                                                 {k {:fn fn-form :inputs inputs}})
                                              ;; (let [str? (string? v)
                                              ;;       inputs (get v :inputs)
                                              ;;       syms (vec (map (fn [_] (gensym)) inputs))
                                              ;;       bindings (into {} (map vector inputs syms))
                                              ;;       bindings-pairs (apply concat bindings)
                                              ;;       fn-form `(fn ~syms
                                              ;;                  (let [~bindings-pairs]
                                              ;;                    (if ~str?
                                              ;;                      (ut/template-replace walkmap ~v)
                                              ;;                      (walk/postwalk-replace walkmap ~v))))]
                                              ;;   (ut/pp [:dyn-fn fn-form syms bindings])
                                              ;;   {k {:fn fn-form :inputs inputs}})

                                               (and (or (not (map? v)) (and (map? v) (nil? (get v :fn-key)))) ;; static value w/o a fn at all
                                                    (empty? (get v :inputs))) ;; no inputs dbl check
                                               {k v}
                                               :else (let [fn-key (try (vec (conj (vec (get v :fn-key)) :fn)) (catch Exception _ [:issue :oops])) ;; lookup fn from library
                                        ; _ (ut/pp [:fn-key fn-key (get v :fn-key)])
                                                           fn (get-in cruiser/default-flow-functions fn-key)]
                                                       {k (-> v (assoc :fn fn) (dissoc :fn-key))})))))
               conns (get flowmap :connections)
               gen-comps (atom {})
               ;;_ (ut/pp [:conns1 conns])
               conns (ut/flatten-one-level (for [[c1 c2] conns ;; remap mapped outputs with generated GET blocks
                                                 :let [mapped? (try (cstr/includes? (str c1) "/") (catch Exception _ false))]]
                                             (if mapped? (let [spt (-> (str c1) (cstr/replace #":" "") (cstr/split #"/"))
                                                               base (keyword (first spt))
                                                               kkey (keyword (last spt))
                                                               kkey (if (cstr/starts-with? (str kkey) ":idx") ;; vector get index "key"
                                                                      (try (edn/read-string (cstr/replace (str kkey) ":idx" "")) (catch Exception _ -1))
                                                                      kkey)
                                                               gen-get (-> (str c1) (cstr/replace #":" "") (cstr/replace #"/" "-"))
                                                               gen-get1 (keyword (str gen-get "/in"))
                                                               gen-get2 (keyword gen-get)]
                                                           (if (= kkey :*)
                                                             [[base c2]] ;; if user wants the whole map instead with :* then just pass it through like a reg :out
                                                             (do (swap! gen-comps assoc gen-get2 ;; swap in the literal, but keep "unread"
                                                                        {:fn (walk/postwalk-replace {:kkey kkey} '(fn [m] (get m :kkey 0)))
                                                                         :inputs [:in]})
                                                                 [[base gen-get1]
                                                                  [gen-get2 c2]])))
                                                 [c1 c2])))

               flowmap-comps (merge flowmap-comps @gen-comps) ;; if we have to realize one-get hops, merge them in
               gen-multi-arities (atom [])
               conns (into [] (for [[c1 c2] conns] ;; need to detect multi-aritiy ports and generate the inputs for them
                                (if (cstr/ends-with? (str c2) "+")
                                  (let [spl (cstr/split (cstr/replace (str c2) ":" "") #"/")
                                                    ;_ (swap! cntr inc)
                                        bid (keyword (first spl))
                                        cntr (count (get (group-by first @gen-multi-arities) bid []))
                                        new-port (str (last spl) (str cntr))
                                        new-c2 (keyword (str (first spl) "/" new-port))
                                        _ (swap! gen-multi-arities conj [bid (keyword new-port) new-c2])]
                                    [c1 new-c2])
                                  [c1 c2])))
               multi-arity-flowmap-comps (into {} (for [[k v] (group-by first @gen-multi-arities)
                                                        :let [other-inputs (vec (remove #(cstr/ends-with? (str %) "+") ;; for static arities, if exist
                                                                                        (get-in flowmap-comps [k :inputs] [])))
                                                              inputs (into other-inputs (vec (map second v)))]]
                                                    {k (merge (get flowmap-comps k) {:inputs inputs})}))

               flowmap-comps (merge flowmap-comps multi-arity-flowmap-comps)

               ;_ (ut/pp [:**multi-arity-conns flowmap-comps])
               ;_ (ut/pp [:**multi-arity-flowmap-comps  conns])

               ;_ (ut/pp [:gen-gets conns2 @gen-comps (vec (flatten conns2))])
               all-inputs (vec (distinct (ut/deep-flatten (for [[k v] flowmap-comps] (for [kk (get v :inputs)] (keyword (str (gn k) "/" (gn kk))))))))
               input-refs (distinct (ut/deep-flatten conns))
               uk (fn [x] (cstr/replace (str x) #":" ""))
               uk2 (fn [x] (-> (str x) uk (cstr/replace #"/" "")))
               defaults-map (apply merge (for [[k v] (get flowmap :components) ;; get the default values for each component type, preface them with the BLOCK NAME
                                               :let [fn-key (get v :fn-key)
                                                     defs (get-in cruiser/default-flow-functions (vec (conj (vec fn-key) :defaults)))
                                                     override-defs (get-in v [:default-overrides] {}) ;; new defaults packaged with the flow, not canned components
                                                     defs (merge defs override-defs)]]
                                           (into {} (for [[kk vv] defs] {(keyword (str (uk k) "/" (uk kk))) vv}))))
               defaults-map (walk/postwalk-replace sub-map defaults-map) ;; sub in any run-specific values to defaults - possibly to more? TODO
               missing-inputs (vec (cset/difference (set all-inputs) (set input-refs))) ;; general missing inputs from all blocks
               ;_ (ut/pp [:defalts-map defaults-map :missing-inputs-pre missing-inputs])
               defaults-missing (or (select-keys defaults-map missing-inputs) {}) ;; get the default statics from missing keys list
               missing-inputs (or (cset/difference (set missing-inputs) (set (keys defaults-missing))) []) ;; remove missing inputs w defaults from nilly map
               defaults-comps (or (into {} (for [[k v] defaults-missing] {(keyword (uk2 (str k))) v})) {}) ;; create the static default inputs as components
               defaults-conns (or (vec (for [[k _] defaults-missing] [(keyword (uk2 (str k))) k])) []) ;; create the static default inputs as connections to above comps
               ;_ (ut/pp [:missing-inputs missing-inputs defaults-missing defaults-comps defaults-conns])
               nil-inputs-conns (vec (for [i missing-inputs] [:nilly i])) ;; what is left over gets nilly input (or 0 as we do here, you can't pass NIL on a channel)
               finished-flowmap (-> flowmap ;; package that shit together for christmas dinner
                                    (assoc :components (merge flowmap-comps {:nilly :skip} defaults-comps))
                                    (assoc :connections (vec (remove empty? (into (into conns nil-inputs-conns) defaults-conns)))))
              ;;  finished-flowmap (-> flowmap ;; package that shit together for christmas dinner
              ;;                       (assoc :components (merge flowmap-comps   defaults-comps))
              ;;                       (assoc :connections (vec (remove empty? (into (into conns []) defaults-conns)))))
                ;;  _ (ut/pp [:all-inputs all-inputs
                ;;            :missing-inputs missing-inputs
                ;;            :flowmap (dissoc finished-flowmap :canvas)])
               ;;_ (ut/pp [:finished-flowmap finished-flowmap])
               ]
           finished-flowmap))) ;; tonight we feast on the values!

;; [:v-box
;;  :size
;;  "auto"
;;  :width
;;  "490px"
;;  :style
;;  {:font-size "13px", :opacity 0.33}
;;  :children
;;  [[:h-box :size "auto" :justify :between :children [[:box :child ":mean"] [:box :child "3853.75" :style {:font-weight 400}]]]
;;   [:h-box :size "auto" :justify :between :children [[:box :child ":standard-deviation"] [:box :child "1099.883942741233" :style {:font-weight 400}]]]
;;   [:h-box :size "auto" :justify :between :children [[:box :child ":sample-size"] [:box :child "4" :style {:font-weight 400}]]]
;;   [:h-box :size "auto" :justify :between :children [[:box :child ":fields"] [:box :child "[:YEAR]" :style {:font-weight 400}]]]
;;   [:h-box :size "auto" :justify :between :children [[:box :child ":calc-used"] [:box :child "[:percentiles [0.01 0.99]]" :style {:font-weight 400}]]]]]
;[:v-box :size "auto" :width "490px" :style {:font-size "16px"} :children [[:h-box :size "auto" :justify :between :children [[:box :child ":YEAR"] [:box :child "2016" :style {:font-weight 400}]]]]]

(defn wrap-payload [payload thread-id thread-desc message-name]
  (let [thread-id (ut/keywordize-string thread-id)]
    {thread-id {:data [{;:ask-mutates
                          ;; {"Highlight these in your source query?" {[:panels :block-984 :queries :OFFENSE-CODE-GROUP-drag-40 :style-rules [:* :higlight-1584150552]] {:logic [:and [:= :DISTRICT "B2"]],
                          ;;                                                                                                                                                             :style {:background-color "#008b8b66", :border "1px solid #00000000"}}}},
                        :content (vec (remove #(or (nil? %) (= 0 %)) (first payload)))
                        :name (str message-name)
                        :order (ut/unix-timestamp)
                        :parameters {}
                        :forced-mutates {} ;; TODO, not implemented, but should work exactly like flow-sub pushes do
                        :step-mutates {}}]
                :description (str thread-desc)
                :mutates {}
                :options {:actions? false, :pages? false, :search? false}
                :parameters {}}}))

(declare insert-kit-data)

(defn replace-keywords [sub-task subbed-subs]
  (let [replacements (into {} (map (fn [[k v]] [v k]) subbed-subs))]
    (map (fn [k] (get replacements k k)) sub-task)))

(defn kick [client-name task-id sub-task thread-id thread-desc message-name & args]
  ;(ut/pp [:kick-called-with! {:client-name client-name :task-id task-id :sub-task sub-task :thread-id thread-id :thread-desc thread-desc :message-name message-name}])
  (let [ui-keypath [:kick] ;;; ^ sub-task is the UI item-key in push ops
        payload (vec args)
        payload? (not (empty? payload))
        heartbeat? (= sub-task :heartbeat)
        payload (when (and payload? (not heartbeat?))
                  (wrap-payload payload thread-id thread-desc message-name))

        ;_ (insert-kit-data client-name task-id sub-task payload)
        ;; insert-kit-data [output query-hash ui-keypath ttype kit-name elapsed-ms & [client-name flow-id]]
        _ (when (and payload? (not heartbeat?))
            (insert-kit-data payload (hash payload) sub-task task-id ui-keypath 0 client-name "flow-id-here!"))
        data (merge {:sent! task-id :to client-name :at (str (ut/get-current-timestamp)) :payload payload}
                    (when payload? {:payload-kp [sub-task task-id]}))
        queue-id -1
       ; client-name (try (if (string? client-name) (edn/read-string client-name) client-name) (catch :default _ client-name))
        ;status "what?"

        destinations (cond (= client-name :all) (vec (keys @client-queues))
                           (keyword? client-name) [client-name]
                           :else client-name)]
    (doseq [cid destinations]
      (let [hb? (= sub-task :heartbeat)
            sub-task (if hb?
                       (let [ssubs (vec (keys (get @atoms-and-watchers cid {})))
                             subbed-subs (vec (distinct (get @param-var-key-mapping cid [])))
                             sss-map (into {} (for [[orig subbb] subbed-subs] {subbb orig}))
                             replaced-subs (walk/postwalk-replace sss-map ssubs)]
                        ;;  (ut/pp [:kick-subs-boomerang! cid ssubs subbed-subs replaced-subs])
                        ;;  ssubs
                         replaced-subs)
                       sub-task)]
        ;; (when true ; false ;(not (= task-id :heartbeat))
        ;;   (ut/pp [:kick! ui-keypath data cid queue-id task-id sub-task]))
        (when hb? (swap! ping-ts assoc cid (System/currentTimeMillis)))
        (push-to-client ui-keypath data cid queue-id task-id sub-task)))
    ;data
    :sent!))

(defn process-flow-map [fmap]
  (into {} (for [[k v] fmap
                 :let [ttype (get-in v [:data :drag-meta :type])]]
             (cond


              ;;  (= ttype :sub-flow222) ;; kek
              ;;  (let [subs (get v :sub-flow)
              ;;        post-subs (process-flow-map (get subs :map))]
              ;;   ; (tap> [:sub-flow-preporocess subs post-subs])
              ;;    {k (-> v
              ;;           (assoc :sub-flow post-subs))})

               (= ttype :query)
               (let [pfull (first (vals (get-in v [:data :queries])))
                      ;; _ (tap> [:param-fmaps ttype k pfull])
                    ;ddata @(re-frame/subscribe [::conn/clicked-parameter-key [pfull]])
                     ddata pfull ;(first @(re-frame/subscribe [::resolver/logic-and-params [pfull]]))
                     dtype (ut/data-typer ddata)
                     ports {:out {:out (keyword dtype)}}
                     ports (cond (= dtype "map")
                                 {:out (assoc (into {} (for [[k v] ddata] {k (keyword (ut/data-typer v))})) :* :map)}
                                 (or (= dtype "vector") (= dtype "rowset"))
                                 {:out (assoc (into {} (for [k (range (count ddata)) :let [v (get ddata k)]] {(keyword (str "idx" k)) (keyword (ut/data-typer v))})) :* :vector)}
                                 :else ports)
                     full (-> v
                              (assoc-in [:data :user-input] ddata)
                              (assoc-in [:ports] ports))]
                 {k full})

               (= ttype :param)
               (let [pfull (get-in v [:data :drag-meta :param-full])
                       ;; _ (tap> [:param-fmaps ttype k pfull])
                     ddata pfull ; (first pfull) ;;@(re-frame/subscribe [::conn/clicked-parameter-key [pfull]])
                     dtype (ut/data-typer ddata)
                     ports {:out {:out (keyword dtype)}}
                     ports (cond (= dtype "map")
                                 {:out (assoc (into {} (for [[k v] ddata] {k (keyword (ut/data-typer v))})) :* ddata)}
                                 (= dtype "vector")
                                 {:out (assoc (into {} (for [k (range (count ddata)) :let [v (get ddata k)]] {(keyword (str "idx" k)) (keyword (ut/data-typer v))})) :* ddata)}
                                 :else ports)
                     full (-> v
                              (assoc-in [:data :user-input] ddata)
                              (assoc-in [:ports] ports))]
                 {k full})
                      ;; (first (resolver/logic-and-params [cell-ref] nil))

               (= ttype :cell)
               (let [pfull (get-in v [:data :drag-meta :param-full])
                       ;; _ (tap> [:param-fmaps ttype k pfull])
                            ;ddata @(re-frame/subscribe [::conn/clicked-parameter-key [pfull]])
                     ddata pfull ;(first pfull) ;(first @(re-frame/subscribe [::resolver/logic-and-params [pfull]]))
                     dtype (ut/data-typer ddata)
                     ports {:out {:out (keyword dtype)}}
                     ports (cond (= dtype "map")
                                 {:out (assoc (into {} (for [[k v] ddata] {k (keyword (ut/data-typer v))})) :* ddata)}
                                 (= dtype "vector")
                                 {:out (assoc (into {} (for [k (range (count ddata)) :let [v (get ddata k)]] {(keyword (str "idx" k)) (keyword (ut/data-typer v))})) :* ddata)}
                                 :else ports)
                     full (-> v
                              (assoc-in [:data :user-input] ddata)
                              (assoc-in [:ports] ports))]
                 {k full})
               (= ttype :open-block)
               (let [pfull (if (= (get-in v [:data :syntax] "clojure") "clojure")
                             (get-in v [:data :user-input])
                             (cstr/join "\n" (get-in v [:data :user-input])))
                        ;;     _ (tap> [:user-input-fmaps ttype k pfull])
                            ;ddata (resolver/logic-and-params pfull nil) ;@(re-frame/subscribe [::conn/clicked-parameter-key [pfull]])
                     ddata pfull ; (first pfull) ;(first @(re-frame/subscribe [::resolver/logic-and-params [pfull]]))
                     dtype (ut/data-typer ddata)
                     ports {;:in (get-in v [:ports :in])
                            :out {:out (keyword dtype)}}
                     ports (cond (= dtype "map")
                                 {;:in (get-in v [:ports :in])
                                  :out (assoc (into {} (for [[k v] ddata] {k (keyword (ut/data-typer v))})) :* ddata)}
                                 (= dtype "vector")
                                 {;:in (get-in v [:ports :in])
                                  :out (assoc (into {} (for [k (range (count ddata)) :let [v (get ddata k)]] {(keyword (str "idx" k)) (keyword (ut/data-typer v))})) :* ddata)}
                                 :else ports)
                     ports (merge (get v :ports) ports)
                     full (-> v
                              (assoc-in [:data :user-input] ddata)
                              (assoc-in [:ports] ports))]
              ; (tap> [:param-step {k full}])
                 {k full})
               :else {k v}))))

(declare save!)
(declare ttap>)

(defn process-flowmap2 [flowmap flowmaps-connections fid]
  ;;; test
  (let [canvas-key (into {} (for [[k {:keys [w h x y]}] flowmap]
                              {k {:w w :h h :x x :y y :view-mode "text"}}))
        flowmaps-connections (vec (for [[c1 c2] flowmaps-connections]
                                    (if (cstr/ends-with? (str c1) "/*")
                                      [(keyword (-> (ut/unkeyword (str c1)) (cstr/replace "/*" "") (cstr/replace ":" ""))) c2] [c1 c2])))
        components-key (into {} (for [[k {:keys [data ports view file-path flow-path raw-fn sub-flow-id flow-id sub-flow]}] flowmap ;; <-- flow-id refers to the subflow embed, not the parent
                                      :let [ttype (or (get-in data [:flow-item :type])
                                                      (get-in data [:drag-meta :type]))
                                            try-read  (fn [x] (try (edn/read-string x) (catch Exception _ x)))
                                            view-swap (fn [obody flow-id bid push-key]
                                                        (let [pkey       (keyword (str (cstr/replace (str push-key) ":" "") ">"))
                                                              kps        (ut/extract-patterns obody pkey 2)
                                                              logic-kps  (into {} (for [v kps]
                                                                                    (let [[_ that] v]
                                                                                      {v [:push> [flow-id (str bid) that]]})))]
                                                          (walk/postwalk-replace logic-kps obody)))
                                            view-swap2 (fn [obody flow-id bid push-key]
                                                         (let [pkey       (keyword (str (cstr/replace (str push-key) ":" "") ">"))
                                                               kps        (ut/extract-patterns obody pkey 1)
                                                               logic-kps  (into {} (for [v kps]
                                                                                     (let [[_] v]
                                                                                       {v [:push>> [flow-id (str bid)]]})))]
                                                           (walk/postwalk-replace logic-kps obody)))
                                            view-swaps (fn [obody flow-id push-key-bid-pairs]
                                                         (reduce (fn [body [push-key bid]]
                                                                   (-> body
                                                                       (view-swap flow-id bid push-key)
                                                                       (view-swap2 flow-id bid push-key)))
                                                                 obody
                                                                 push-key-bid-pairs))
                                            view (when view (let [conns (vec (filter #(cstr/includes? (str (first %)) "/push-path") flowmaps-connections))
                                                                  push-key-bid-pairs (vec (for [[c1 c2] conns] [(keyword (last (cstr/split (str c1) #"/"))) c2]))
                                                                  view (view-swaps view fid push-key-bid-pairs)]
                                                              ;(tap> [:view-builder k push-key-bid-pairs view fid])
                                                              view))
                                            nname (get-in data [:flow-item :name] ":unknown!")
                                            fn-key (if flow-path nname (try-read nname))
                                            fn-category (try-read (get-in data [:flow-item :category] ":unknown!"))]]
                                  (cond

                                    (and (= ttype :open-block) (ut/ne? (get ports :in))) ;; open block with inputs
                                    {k {:data (get data :user-input)
                                        :default-overrides (get-in data [:flow-item :defaults] {})
                                        :inputs (vec (keys (get ports :in)))}}

                                    (or (= ttype :open-block) (= ttype :cell) (= ttype :param))
                                    {k (if (= (get data :syntax "clojure") "clojure")
                                         (get data :user-input)
                                         ;(cstr/join "\n" (get data :user-input))
                                         (get data :user-input))}

                                    (= ttype :open-fn)
                                    {k (merge
                                        (when view {:view view})
                                        {:fn raw-fn :raw-fn raw-fn
                                         :default-overrides (get-in data [:flow-item :defaults] {})
                                         :inputs (vec (keys (get ports :in)))})}

                                    (= ttype :query)
                                    {k {:fn (get data :user-input) ;'(fn [x] x) ;:raw-fn '(fn [x] x)
                                        :inputs (vec (keys (get ports :in)))}}

                                    flow-path
                                    {k {:flow-path flow-path :sub-flow-id sub-flow-id
                                        :default-overrides (get-in data [:flow-item :defaults] {})
                                        :inputs (vec (keys (get ports :in)))}}

                                    (= ttype :sub-flow)
                                    {k (-> (process-flowmap2 (get sub-flow :map) (get sub-flow :connections) fid)
                                           (assoc :file-path file-path)
                                           (assoc :flow-id flow-id))} ;; flow-id of the embdeeded flow, NOT the parent
                                              ;; {k {:components (get sub-flow :map)
                                               ;;     :connections (get sub-flow :connections)}}
                                            ;;(= ttype :param) {k (get data :user-input)}

                                    :else {k {:fn-key [fn-category fn-key]
                                              :default-overrides (get-in data [:flow-item :defaults] {})
                                              :inputs (if false ;expandable-in?
                                                        [(vec (keys (get ports :in)))] ;; send as single value to apply%
                                                        (vec (keys (get ports :in))))}})))
        components-key (into {} ;; deconstruct conditional paths to single condi key
                             (for [[k v] flowmap]
                               (if (not (empty? (get v :cond)))
                                 (let [ccond (into {}
                                                   (for [[c1 c2] flowmaps-connections
                                                         :let [link (keyword (str (cstr/replace (str k) ":" "") "/cond-path"))
                                                               ff (cstr/split (str c1) #"/")
                                                               cname (keyword (last ff))
                                                               c2wo (keyword (first (cstr/split (cstr/replace (str c2) ":" "") #"/")))] ;; w/o port id.... TEST
                                                         :when (cstr/starts-with? (str c1) (str link))]
                                                     {c2wo (get-in v [:cond cname :fn])}))]
                                   {k (assoc (get components-key k) :cond ccond)})
                                 {k (get components-key k)})))
        flowmaps-connections (vec (filter #(and
                                            (not (cstr/includes? (str (first %)) "/cond-path"))
                                            (not (cstr/includes? (str (first %)) "/push-path"))) flowmaps-connections))
        server-flowmap {:canvas canvas-key :components components-key :connections flowmaps-connections}]
    server-flowmap))

(defn materialize-flowmap  [client-name flowmap flow-id opts & [no-eval?]]
  (let [flowmap (if (string? flowmap) ;; load from disk and run client sync post processing (make sure fn is in sync w client code)
                  (let [ppath (if (cstr/ends-with? (cstr/lower-case flowmap) ".edn")
                                flowmap ;; already a file path
                                (str "./flows/" flowmap ".edn"))
                        raw (try (edn/read-string (slurp ppath))
                                 (catch Exception _ (do (ut/pp [:error-reading-flow-from-disk flow-id client-name])
                                                        {})))
                        flowmaps (process-flow-map (get raw :flowmaps))
                        connections (get raw :flowmaps-connections)
                        fmap (process-flowmap2 flowmaps connections flow-id)]
                    (ut/pp [:materialized-flowmap fmap])
                    fmap)
                  flowmap)
        uid (str flow-id) ;(ut/generate-name)
        post-id (if (get opts :increment-id? false) ;; if auto increment idx, lets use that for lookups...
                  (str uid "-" (count (filter #(cstr/starts-with? % uid) (keys @flow-db/channel-history))))
                  uid)
        ;_ (ut/pp [:incoming-flowmap flowmap])
        ;; eval the flowmap to get the compiled functions in there rather than just un-read symbols
               ;; might be a problem with repl shipping though... might have to selectively eval the fns pre run step?
       ; _ (ut/pp [:flowmap finished-flowmap])
        sub-map {:flow-id uid :client-name client-name}
        finished-flowmap (if no-eval?
                           flowmap
                           (process-flowmaps flowmap sub-map))]
    (if no-eval?
      finished-flowmap
      (eval finished-flowmap))))

(declare get-flow-open-ports)

(defn create-flowblock  [file-path]
  (let [data (get-flow-open-ports file-path nil nil)
        fid0 (cstr/split file-path #"/")
        fid (cstr/replace (last fid0) ".edn" "")
        out (first (for [[_ v] (get data :blocks)
                         :when (true? (get v :last?))] (get-in v [:type :out] (get-in v [:type :*]))))
        flow-id (get data :flow-id)
        block {:flow-id flow-id
               :flow-path file-path
               :fn '()
               :inputs (vec (keys (get data :open-inputs [])))
               :required (vec (keys (get data :open-inputs [])))
               :defaults (into {} (for [[k v] (get data :open-inputs [])] {k (get v :user-input)}))
               :description (get data :description)
               :icon "zmdi-puzzle-piece"
               :types (assoc
                       (into {} (for [[k v] (get data :open-inputs [])] {k (get-in v [:type :out] (get v :type))})) ;; work around for legacy issue, TODO
                       :out out)}
        ;data (assoc data :flow-block block)
        ]

    ;; (ut/pp [:create-flowblock-for file-path])

    (async/thread ;; really expensive logging below. temp
      (let [fp (str "./flow-blocks/" fid ".edn")]
        (ext/create-dirs "./flow-blocks/")
        (ut/pretty-spit fp block 225)))

    (swap! sub-flow-blocks assoc flow-id block)
    ;; update the flow-blocks sql db with the new block
    {flow-id block}))


(defn alert! [client-name content w h duration & [type]]
  (push-to-client [:alerts] [content w h duration] client-name -1 :alert1 :alert2)
  :alerted!)

(defn ttap> [client-name x & [w h d]]
  (alert! client-name
          (if (number? x) (str x) x)
          (or w 10) (or h 1.35) (or d 12)))

;; (add-tap my-tap)

;; (tap> "Hello, world!")

(defn save! [kp v & [client-name]]
  (let [kp (if (keyword? kp) [kp] kp)] ;; just incase 
    ;;(ut/pp [:saving-server-param! {:to kp :val v :from client-name}])
    (swap! server-atom assoc-in kp v)))

(def last-times (atom {})) ;; stopgap for error times TODO, april 2024

(defn flow! [client-name flowmap file-image flow-id opts & [no-return?]]
  (try
    (let [orig-flowmap flowmap
          from-string? (true? (string? flowmap))
          ;;_ (ut/pp [:run-flow-called! [client-name flowmap file-image flow-id opts no-return?]])
          _ (swap! orig-caller assoc flow-id client-name) ;; caller for the flow in case of flowmaps starter overlap
          orig-opts opts ;; contains overrides
        ;opts (merge {:client-name (str (or (get opts :client-name) client-name :rvbbit))} (or opts {}))) ;; stuff into opts for flow-waiter
          ;user-opts (get opts :opts)
          ;opts (dissoc opts :opts)
          ;instance-id (get opts :instance-id)
          ;opts (if (get opts :client-name) opts (merge {:client-name client-name} opts)) ;; stuff into opts for flow-waiter
          ;opts (merge {:client-name client-name} opts)
          ;; _ (ut/pp [:flow!-passed-opts flow-id opts])
          oo-flowmap (if (string? flowmap) ;; load from disk and run client sync post processing (make sure fn is in sync w client code)
                       (let [fss (str "./flows/" flowmap ".edn")
                             raw (try (edn/read-string (slurp fss))
                                      (catch Exception _ (do (ut/pp [:error-reading-flow-from-disk-oo-flowmap-flow! fss flow-id client-name])
                                                             {})))
                             flowmaps (process-flow-map (get raw :flowmaps))
                             connections (get raw :flowmaps-connections)
                             fmap (process-flowmap2 flowmaps connections flow-id)
                             fmap (merge fmap {:opts (get raw :opts)})]
                      ;(ut/pp fmap)
                         fmap)
                       flowmap)
          file-image (or file-image
                         (try (edn/read-string (slurp (str "./flows/" flowmap ".edn")))
                              (catch Exception _ (do (when (not= flow-id "client-keepalive") ;; since no file exists and its not from a client, it has not real "file-image" map
                                                       (ut/pp [:error-reading-flow-from-disk-file-image-flow! flow-id client-name]))
                                                     {}))))

          ;; _ (async/thread (do (ext/create-dirs "./flow-history/src-maps-pre0") ;; TEMP FOR DEBUGGING PRE PROCESSED MAPS
          ;;           (ut/pretty-spit (str "./flow-history/src-maps-pre0/" flow-id "." from-string? ".edn") ;; expensive...
          ;;                           flowmap 185)))            


          user-opts (get oo-flowmap :opts (get opts :opts))
          opts (assoc opts :opts user-opts) ;; kinda weird, but we need to account for saved opts or live interactive opts
          ;;_ (ut/pp [:flow!-opts opts user-opts])
          walk-map (into {} (for [[k v] (get @params-atom client-name)] {(keyword (str "param/" (cstr/replace (str k) ":" ""))) v}))
        ;; _ (ut/pp [:applying-walk-map walk-map])
        ;; pre-run subbing in params and other contextual values for offline running... (:server / :calliope will have own, etc)
          flowmap (walk/postwalk-replace walk-map oo-flowmap)

          ;; _ (async/thread (do (ext/create-dirs "./flow-history/src-maps-pre") ;; TEMP FOR DEBUGGING PRE PROCESSED MAPS
          ;;           (ut/pretty-spit (str "./flow-history/src-maps-pre/" flow-id "." from-string? ".edn") ;; expensive...
          ;;                           flowmap 185)))          

          uid (get opts :instance-id (str flow-id))  ;(ut/generate-name)
          post-id (if (get opts :increment-id? false) ;; if auto increment idx, lets use that for lookups...
                    (str uid "-" (count (filter #(cstr/starts-with? % uid) (keys @flow-db/channel-history))))
                    uid)
;;          run-id (get-in @flow-db/results-atom [flow-id :run-id] "no-run-id")
          uuid (str (java.util.UUID/randomUUID))
        ;;_ (ut/pp [:incoming-flowmap flowmap])
        ;; eval the flowmap to get the compiled functions in there rather than just un-read symbols
               ;; might be a problem with repl shipping though... might have to selectively eval the fns pre run step?
       ; _ (ut/pp [:flowmap finished-flowmap])

          sub-map {:flow-id uid :client-name client-name}
          finished-flowmap (process-flowmaps flowmap sub-map)
          finished-flowmap (walk/postwalk-replace (merge ;; need to do recursively eventually? this map keeps getting bigger... will need to share with REPL execution as well, including a server-side resolver...
                                                   {:client-name client-name
                                                    :cid client-name
                                                    :flow-id flow-id
                                                    :fid flow-id
                                                    :client-id client-name
                                                    ;;`save! '(fn [kp v] (rvbbit-backend.websockets/save! kp v client-name))
                                                    'save! 'rvbbit-backend.websockets/save!
                                                    'tap> 'rvbbit-backend.websockets/ttap>}
                                                   (ut/deselect-keys (get opts :opts) ;; TODO, why did I nest/unnest this? so confusing
                                                                     [:retries :retry-on-error? :close-on-done? :debug? :timeout]))
                                                  ;; ^^ any user-space flow opts that are not system level
                                                  finished-flowmap)
          opts (merge opts (select-keys (get opts :opts {}) [:close-on-done? :debug? :timeout]))
          opts (merge {:client-name client-name} opts)
          finished-flowmap (assoc finished-flowmap :opts opts)

          ;; _ (async/thread (do (ext/create-dirs "./flow-history/src-maps") ;; TEMP FOR DEBUGGING POST PROCESSED MAPS
          ;;                     (ut/pretty-spit (str "./flow-history/src-maps/" flow-id "." from-string? ".edn") ;; expensive...
          ;;                                     finished-flowmap 185)))

          ;;_ (ut/pp [:***flow!-finished-flowmap finished-flowmap [:opts opts]])
          _ (swap! watchdog-atom assoc flow-id 0) ;; clear watchdog counter          
          _ (swap! tracker-history assoc flow-id []) ;; clear loop step history
          _ (swap! last-times assoc-in [flow-id :start] (System/currentTimeMillis))
          _ (swap! tracker-client-only assoc flow-id {})
          _ (swap! acc-trackers assoc flow-id [])
          _ (swap! temp-error-blocks assoc flow-id [])
          ;;_ (swap! last-times assoc flow-id []) 
          ;_ (alert! client-name  10 1.35)
          base-flow-id (if (cstr/includes? (str flow-id) "-SHD-") (first (cstr/split flow-id #"-SHD-")) flow-id) ;; if history run, only for estimates for now
          prev-times (get @times-atom base-flow-id [-1])

          ;; _ (push-to-client [:estimate] (ut/avg prev-times) client-name -1 :est1 :est2)

          ship-est (fn [client-name]
                     (try
                       (let [times (ut/avg   ;; avg based on last 10 runs, but only if > 1
                                    (vec (take-last 10 (vec (remove #(< % 1) prev-times)))))]
                         (when (not (nil? times))
                           (kick client-name [:estimate] {flow-id {:times times
                                                                   :run-id uuid}} nil nil nil)))
                       (catch Exception e (ut/pp [:error-shipping-estimates
                                                  (Throwable->map e)
                                                  (str e)]) 0)))

          ;; _ (when (not= flow-id "client-keepalive")
          ;;     (doseq [client-name (keys (client-statuses))] (ship-est client-name))) ;; send to all clients just in case they care... (TODO make this more relevant)

          _ (ship-est client-name)


         ;; _ (ut/pp [:opts!! opts flow-id])
          image-map (assoc file-image :opts (merge (get file-image :opts) (select-keys orig-opts [:overrides])))
          ;; _ (swap! latest-image-map assoc flow-id {:image image-map ;; a partial flow-history map so if someone wants to "jump into" a running flow we have it. ephemeral, obvs
          ;;                                          :source-map finished-flowmap
          ;;                                          :client-name client-name
          ;;                                          :flow-id flow-id})
          ;; needed for history echoes to work
          _ (do (ext/create-dirs "./flow-history") ;; just in case
                (spit ;; a partial flow-history map so if someone wants to "jump into" a running flow we can use flow-history pipeline. ephemeral, obvs
                 (str "./flow-history/" flow-id ".edn")
                 {:image image-map ;; a partial flow-history map so if someone wants to "jump into" a running flow we have it. ephemeral, obvs
                  :source-map finished-flowmap
                  :client-name client-name
                  :flow-id flow-id}))
          return-val (flow-waiter (eval finished-flowmap) uid opts) ;; eval to realize fn symbols in the maps
          ;return-val (submit-named-task-and-wait uid (flow-waiter (eval finished-flowmap) uid opts))

          retry? (get-in opts [:opts :retry-on-error?] false)
          restarts (if retry?
                     (- (get-in opts [:opts :retries] 0)
                        (get @restart-map flow-id 0)) 0)
          restarts-left? (> restarts 0)
       ; return-map (get @flow-db/results-atom uid)
          relevant-keys (vec (filter #(cstr/starts-with? (str %) (str post-id)) (keys @flow-db/results-atom)))
          working-data-ref (into {} (for [[k v] (select-keys @flow-db/working-data relevant-keys)] {k (get v :description)}))
          run-id (get-in @flow-db/results-atom [flow-id :run-id] "no-run-id")
          output {:client-name client-name
                  :flow-id flow-id
                  :run-id run-id
                ;:server-map finished-flowmap
                ;:all-results @flow-db/results-atom
                ;:working-data @flow-db/working-data
                  :fn-history {} ;(select-keys @flow-db/fn-history relevant-keys)
                  :run-refs working-data-ref
                  :tracker (select-keys @flow-db/tracker relevant-keys) ;; generally the last values. deprecated since loops
                  :tracker-history (ut/accumulate-unique-runs (get @tracker-history flow-id []))
                  :source-map finished-flowmap ;(if no-return? {} finished-flowmap)
                  :return-maps (if no-return? {} (select-keys @flow-db/results-atom relevant-keys))
                  :return-map {} ;(get @flow-db/results-atom post-id)
                  :return-val (if no-return? nil return-val)}
        ;_ (swap! assoc-in flow-db/results-atom [])
        ;output (walkl/postwalk-replace {:} output)
          ;output (ut/deep-remove-keys output [:b64_json222])
          ;; _ (ut/pp [:returned*** flow-id (cstr/includes? (str return-val) ":error") restarts restarts-left? opts user-opts])
          ]
      (do
        (swap! restart-map assoc flow-id (inc (get @restart-map flow-id 0)))
        (when (not (cstr/includes? flow-id "keepalive"))
          (ut/pp [:flowmap-returned-val (ut/limited (ut/replace-large-base64 return-val)) :flowmap-returned-val]))
      ;(push-to-client [:alerts] (str "flow " flow-id " has finished") client-name -1 :alert1 :alert2)
        (let [cnt (count (str return-val))
              limit 160
              over? (> cnt limit)
              error? (or (cstr/includes? (str return-val) ":error")
                         (cstr/includes? (str return-val) ":timeout"))
              sample (str ; (str (get-in @flow-status [flow-id :*time-running]) " ")
                      (if over? (str (subs (str return-val) 0 limit) "...")
                          (str return-val)))
              result-code (cond (cstr/includes? (str return-val) ":error") "error"
                                (cstr/includes? (str return-val) ":timeout") "timeout"
                                :else "success")
              sample (if error? (get-in return-val [:error :error] sample) sample)]
          (alert! client-name [:v-box
                           ;:size "auto" :height "60px"
                               :justify :center
                               :style {:margin-top "-6px" :color (if error? "red" "inherit")}
                               :children [[:box :child (str "flow " flow-id " has finished"
                                                            (when error? (str " in error")))
                                         ;:style {:font-size "12px"}
                                           ]
                                          (when (and error? restarts-left?)
                                            [:box
                                             :style {:font-size "9px"}
                                             :child (str "  " restarts " attempts left, restarting in 10 seconds...")])
                                        ;; [:box ;:align :end
                                        ;;  :style {:font-weight 400 :font-size "10px"
                                        ;;          ;:margin-left "20px"
                                        ;;          :opacity 0.6}
                                        ;;  :child (str "in " (get-in @flow-status [flow-id :*time-running]))]
                                          (when (not error?)
                                            [:box
                                             :style {:font-weight 700 :font-size "10px" :opacity 0.7}
                                             :child (str (get-in @flow-status [flow-id :*time-running]) " ")])
                                          [:box
                                           ;:size "auto"
                                           ;:width "455px"
                                           :style {:font-weight 700
                                                   :font-size (if error? "13px" "10px")
                                                   :opacity (if error? 1.0 0.7)}
                                           :child (if error?
                                                    (str sample)
                                                    (str "returns: " sample))]]] 10
                  (if (and error? restarts-left?) 1.5 1.35)
                  (if error?  25 9))

          (when (and error? restarts-left?)
            (async/thread
              (do (Thread/sleep 10000)
                  ;(swap! flow-status assoc-in [flow-id :*running?] true) ;; just to keep the UI from thinking it's done
                  (flow! client-name orig-flowmap file-image flow-id opts))))

          (when (not error?) (swap! restart-map dissoc flow-id)) ;; reset the counter on success

          (do
            (when (not (= flow-id "client-keepalive")) ;; no need to track heartbeats..
              (swap! latest-run-id assoc flow-id run-id)
              (ut/pp [:saving-flow-exec-for flow-id result-code run-id])
              ;(async/thread 
              (do (ext/create-dirs "./flow-history") ;; just in case
                  (spit ;; ut/pretty-spit
                                 ;;(str "./flow-history/" flow-id "-" run-id "-" result-code ".edn") ;; ut/pretty-spit is SO expensive...
                   (str "./flow-history/" run-id ".edn")
                   (ut/replace-large-base64
                    (merge
                     output
                     {;;:image file-image ;; as unprocessed as we can w/o being a file path
                                    ;;:orig-opts orig-opts ;; needed for overrides
                      :image image-map
                                    ;;:first-stage-flowmap flowmap
                                    ;;:tracker (get @tracker-history flow-id [])
                                    ;;:return-map (get @flow-db/results-atom post-id)
                      :return-maps (select-keys @flow-db/results-atom relevant-keys) ;; in case no-return? is true
                      :return-val return-val} ;; in case no-return? is true
                     ))
                                 ;125
                   ))
               ; )
) ;; just for the record
            output)))
    ;; run it!
   ; (ut/pp [:flowmap-returned-val return-val])
   ;     (ut/pp [:flowmap-returned-map return-map])
   ;     ;(ut/pp [:channel-history @flow-db/channel-history])
   ;     ;; set flow finished from client-name/flow-id
      )
    (catch Throwable e ;; mostly for mal-formed raw-fns, etc TODO make more informative
      (let [error-map (Throwable->map e)]
        (do (ut/pp [:error-in-flow-processing error-map])
            (alert! client-name [:v-box :children [[:box :child (str flow-id ":error in flow processing!")]
                                                   [:box :child (str (:phase error-map))]
                                                   [:box
                                                    :style {:font-size "11px"}
                                                    :child (str (:cause error-map))]]] 10 2.1 8)
            {:error error-map})))))

;; forked from flowmaps source so we can call flow! instead of flowmaps/flow
(defn schedule! [time-seq1 flowmap & [opts]]
  (let [;[opts chan-out override] args
        opts (if (nil? opts) {} opts)
        client-name :rvbbit-scheduler
        raw (if (string? flowmap)
              (try (edn/read-string (slurp (str "./flows/" flowmap ".edn")))
                   (catch Exception _ (do (ut/pp [:error-reading-flow-from-disk-raw-schedule flowmap client-name])
                                          {}))) {})
        user-opts (get raw :opts)
        opts (merge {:client-name client-name} opts user-opts)
        ;opts (if override (merge opts {:override override}) opts)
        flow-id (get opts :flow-id (str "unnamed-flow-sched" (hash flowmap)))
        times (if (and (vector? time-seq1) (keyword? (first time-seq1)))
                (doall (take 1000 (ut/time-seq time-seq1)))
                time-seq1)
        times-unlimited (if (and (vector? time-seq1) (keyword? (first time-seq1)))
                          (ut/time-seq time-seq1)
                          time-seq1)
        ch (chime/chime-at
            times-unlimited ;; [] chime time seq, https://github.com/jarohen/chime#recurring-schedules
            (fn [time]
              (when (not @shutting-down?)
                (let [opts (merge opts {:schedule-started (str time)})]
                ;(flow flowmap opts chan-out override)
                  (ut/pp [:*scheduled-run! flow-id :at (str time)])
                  (flow! client-name flowmap nil flow-id opts))))
            {:on-finished (fn [] (ut/pp [:schedule-finished! opts time-seq1]))
             :error-handler (fn [e] (ut/pp [:scheduler-error e]))})] ;; if custom time list, just send it.
      ;; save channel ref for closing later w unschedule!
      ;; update the live schedules atom with the new schedule entry
    (swap! flow-db/live-schedules conj {:flow-id flow-id ;(get opts :flow-id "unnamed-flow-sched")
                                        :override (get opts :overrides)
                                        :next-times (try (vec (map str times)) (catch Exception _ times))
                                        :schedule (if (vector? time-seq1) time-seq1 [:custom-time-fn])
                                        :channel ch})))

;; forked from flowmaps source so we can call flow! instead of flowmaps/flow
(defn unschedule! [flow-id]
  (let [schedule-to-remove (some #(when (= (:flow-id %) flow-id) %) @flow-db/live-schedules)]
    (when schedule-to-remove
      ;; Close the channel associated with the schedule
      (async/close! (:channel schedule-to-remove))
      ;; Remove the schedule from the live schedules atom
      (swap! flow-db/live-schedules #(remove (fn [x] (= (:flow-id x) flow-id)) %)))))

(defn get-flow-open-ports [flowmap flow-id client-name] ;; generally a local path, not a map, but hey w/e
  (let [raw (try (edn/read-string
                  (slurp (if (cstr/ends-with? (cstr/lower-case flowmap) ".edn")
                           flowmap ;; file path, use as is
                           (str "./flows/" flowmap ".edn"))))
                 (catch Exception _ (do (ut/pp [:error-reading-flow-from-disk-raw-open-ports flow-id client-name])
                                        {})))
        [flowmap raw-conns] [(if (string? flowmap) ;; load from disk and run client sync post processing (make sure fn is in sync w client code)
                               (let [flowmaps (process-flow-map (get raw :flowmaps))
                                     connections (get raw :flowmaps-connections)
                                     fmap (process-flowmap2 flowmaps connections flow-id)]
                                 fmap)
                               flowmap) (get raw :flowmaps-connections)]
        ;;_ (ut/pp [:get-flow-open-ports flowmap raw-conns (get flowmap :connections)])
        vblocks (vec (for [[k v] (get flowmap :components)
                           :when (get v :view)]
                       (keyword (str (cstr/replace (str k) ":" "") "-vw"))))
        blocks     (set (into vblocks (vec (keys (get flowmap :components)))))
        flow-id (or flow-id (get raw :flow-id)) ;; if not passed in, get from file (mostly for flowblock creation)
        blocks-types (into {}
                           (for [b blocks]
                             (let [actual-data (get-in @flow-db/results-atom [flow-id b])
                                   trunc-num 60
                                   view? (some #(= % b) vblocks)
                                   llen (count (str actual-data))
                                   declared-out-type (get-in raw [:flowmaps b :ports :out])]
                               {b {:type (if actual-data
                                           (keyword (ut/data-typer actual-data))
                                           declared-out-type)
                                   :ttype (or (get-in raw [:flowmaps b :data :drag-meta :type])
                                              (get-in raw [:flowmaps b :data :flow-item :type]))
                                   :meta (get-in raw [:flowmaps b :data :flow-item :meta])
                                   :last? (true? (some true? (for [[f1 f2] (get flowmap :connections)
                                                                   :when (and (= f1 b) (= f2 :done))] true)))
                                   :defaults (get-in raw [:flowmaps b :data :defaults] {})
                                   :sample (if view?
                                             "(renderable view)"
                                             (if actual-data
                                               (if (> llen trunc-num)
                                                 (str (subs (str actual-data) 0 trunc-num)
                                                      (when (> llen trunc-num) "..."))
                                                 (str actual-data))
                                               "no sample data"))}})))
        base-conns (set (for [[_ v2] raw-conns ;(get flowmap :connections)
                              ;; ^^ we need condis also which have diff connections that get removed by process-flowmaps2
                              ](keyword (gns v2))))
        desc (get-in raw [:opts :description])
        no-inputs (vec (remove #(cstr/ends-with? (str %) "-vw") (cset/difference blocks base-conns)))
        flow-inputs (into {} (for [i no-inputs
                                   :let [outs (keys (get-in raw [:flowmaps i :ports :out]))
                                         ;_ (ut/pp [:outs i outs (get-in raw [:flowmaps i :ports])])
                                         ]]
                               {i {:user-input (get-in raw [:flowmaps i :data :user-input])
                                   :defaults (get-in raw [:flowmaps i :data :flow-item :defaults] {})
                                   :type (get-in raw [:flowmaps i :ports :out
                                                      (if (some #(= % :*) outs) :* (first outs))])}}))]
    {:flow-id flow-id :description desc :open-inputs flow-inputs :blocks blocks-types :found? (true? (not (empty? raw)))}))

(defmethod wl/handle-request :get-flow-open-ports [{:keys [client-name flowmap flow-id]}]
  ;;(ut/pp [:get-flow-open-ports client-name flow-id])
  ;; set running flow from client-name/flow-id
  (get-flow-open-ports flowmap flow-id client-name))

(defmethod wl/handle-request :run-flow [{:keys [client-name flowmap file-image flow-id opts no-return?]}]
  (ut/pp [:running-flow-map-from client-name])
  ;; set running flow from client-name/flow-id
  (when (not (get-in (flow-statuses) [flow-id :*running?] false))
  ;; ^^  dont let the web kick off a double run, in case their client state gets fucked up and accidentally allows it (it shouldnt, but still)
    (flow! client-name flowmap file-image flow-id opts true)))

(defn flow-kill! [flow-id client-name]
  (future
    ;;(Thread/sleep 1000) ; wait for 5 seconds for the rest to die so we dont get overwritten by a dying thread
    (swap! flow-status assoc flow-id {:*done? true
                                      :*result (str "killed-by-user-" client-name)
                                      :*finished -1
                                      :*running []
                                      :*done []
                                      :*ms-elapsed -1
                                      :*time-running "killed"
                                      :*not-started []
                                      :*open-channels []
                                      :*running? false}))
  (swap! flow-db/status assoc-in [flow-id :end] (System/currentTimeMillis))
  (swap! flow-db/results-atom assoc-in [flow-id :done] :you-killed-it!) ;; to force the idea that we've mutated the output...
  (ttap> client-name [:v-box :children
                      [[:box
                        :style {:font-size "10px"}
                        :child (str "force stop " flow-id)]
                       [:box :child (str "killing " (count (get @flow-db/channels-atom flow-id)) " channels")]]] 14 1.1 10)
  (doseq [[k c] (get @flow-db/channels-atom flow-id)]
    (try #_{:clj-kondo/ignore [:redundant-do]}
     (do ;(ut/ppln [:closing-channel k c])
       (try
         (when (not (nil? c))
           (do (swap! flow-db/channels-atom ut/dissoc-in [flow-id k])
               (async/close! c)))
         (catch Throwable e (do (swap! flow-db/channels-atom assoc-in [flow-id k] c)
                                (ut/ppln [:error-closing-channelS-inner e k c])))) ;; close channel
) ;; remove from "live" channel atom
         (catch Throwable e
           (do (swap! flow-db/channels-atom assoc-in [flow-id k] c) ;; if failed, keep key val for now
               (ut/ppln [:error-closing-channelS-outer e k c]))))))

(defmethod wl/handle-request :kill-flow [{:keys [client-name flow-id process?]}]
  (ut/pp [:kill-flow flow-id :from client-name :!])
  (if process?
    (stop-process flow-id)
    (flow-kill! flow-id client-name))
  [:assassin-sent!])



(defn boomerang-client-subs [cid]
  (let [sub-task (vec (keys (get @atoms-and-watchers cid {})))
        ;;subbed-subs (get @param-var-key-mapping cid []) ;; will be [original-key replacement-key] 
        ;;replaced-sub-task (vec (replace-keywords sub-task subbed-subs))
        ]

    ;; (ut/pp [:boomerang-client-subs cid sub-task subbed-subs  ])

    (push-to-client [:kick] {:at "", :payload nil, :payload-kp [:heartbeat :heartbeat], :sent! :heartbeat, :to :all}
                    cid -1 :heartbeat sub-task)))

(declare remove-watchers-for-flow)

(defmethod wl/handle-request :remove-flow-watcher [{:keys [client-name flow-id]}]
  (ut/pp [::remove-flow-watcher flow-id :from client-name :!])
  (remove-watchers-for-flow flow-id client-name)
  ;; (ut/delay-execution 2000 (fn [] (boomerang-client-subs client-name)))
  (boomerang-client-subs client-name)
  [:watcers-removed!])

;; (defmethod wl/handle-request :search-groups [{:keys [client-name search-str max-hits]}]
;;   (ut/pp [:search-for search-str :from client-name])
;;   (let [results (search/search-index search/idx-path search-str)]
;;     ;(group-by :type (for [[k v] results] (merge {:docid k} v)))
;;     (frequencies (map :type (for [[k v] results] (merge {:docid k} v))))))

;; (defmethod wl/handle-request :search [{:keys [client-name search-str max-hits hit-type]}]
;;   (ut/pp [:search-for search-str :from client-name])
;;   (let [results (search/search-index search/idx-path search-str (or max-hits 100))]
;;     results))

(defmethod wl/handle-request :session-snaps [{:keys [client-name]}]
  ;(ut/pp [:get-session-snaps :from client-name :!])
  (take 9 (sort-by second (ut/get-file-vectors-with-time-diff "./snaps/" "edn"))))

;; (defmethod wl/handle-request :save-snap [{:keys [client-name image]}] ;; too big for websockets, fucks everything all up
;;   (ut/pp [:saving-image-snap :from client-name])
;;   (ut/save-base64-to-png image (str "./snaps/" client-name ".png"))
;;   [:saved!])

(defmethod wl/handle-request :push-value [{:keys [client-name flow-id bid value alert?]}]
  (let [start (System/currentTimeMillis)]
    (ut/pp [:pushing-value value :to flow-id bid :from client-name :!])
  ;; get all output channels from given bid
    (doseq [[channel-name c] (get @flow-db/channels-atom flow-id)
            :let [from (first channel-name)
                  bid (if (string? bid) (try (edn/read-string bid) (catch Exception _ bid)) bid) ;; we need to stringify it to avoid pre-mature resolving sometimes
                  cbid (try (keyword (first (cstr/split (cstr/replace (str (first channel-name)) ":" "") #"/")))
                            (catch Exception _ :none))
                  cbid2 (try (keyword (first (cstr/split (cstr/replace (str (second channel-name)) ":" "") #"/")))
                             (catch Exception _ :none))
                  _ (ut/pp [:push-channel-logic cbid channel-name bid])]
            :when (and
                   (or (= (first channel-name) bid)
                       (= bid cbid))
                   (not
                    (or (= (second channel-name) bid)
                        (= bid cbid2))))]
      ;; (swap! flow-db/channel-history update flow-id conj {:path [:pushed :from :web]
      ;;                                                     :type :channel
      ;;                                                     :channel channel-name
      ;;                                                     :dest (last channel-name)
      ;;                                                     :start start
      ;;                                                     :end (System/currentTimeMillis)
      ;;                                                     :value (ut/limited v flow-id)
      ;;                                                     :data-type (ut/data-typer v)})
      (ut/pp [:pushing-to cbid channel-name bid])
      (when alert?
        (ttap> client-name [:v-box :children
                            [[:box
                              :style {:font-size "10px"}
                              :child (str [:pushing-to cbid cbid2 channel-name bid])]
                             [:box :child (str value)]]] 14 1.1 10))
      (swap! flow-db/tracker assoc-in [flow-id bid :end] (System/currentTimeMillis))
      (swap! flow-db/results-atom assoc-in [flow-id bid] value) ;; to force the idea that we've mutated the output...
      (swap! flow-db/fn-history assoc flow-id (conj (get @flow-db/fn-history flow-id [])
                                                    {:block from :from :static
                                                     :path [:from :static from]
                                                     :value (ut/limited value flow-id)
                                                     :type :function
                                                     :dest from
                                                     :channel [from]
                                                     :data-type (ut/data-typer (ut/limited value flow-id))
                                                     :start start
                                                     :end (System/currentTimeMillis)
                                                     :elapsed-ms (- (System/currentTimeMillis) start)}))
      (async/put! c ;(get-in @flow-db/channels-atom [flow-id channel-name])
                  {:sender (last channel-name) :value value})
      [:value-pushed!])))

(defmethod wl/handle-request :flow-status [{:keys [client-name flow-id]}]
  (ut/pp [:run-status-req-from client-name :for flow-id])
  ;; set running flow from client-name/flow-id
  {:results-atom (get @flow-db/results-atom flow-id)
   :status (get @flow-status flow-id)
   :tracker (get @flow-db/results-atom flow-id)
   ;:channels (get @flow-db/channels-atom flow-id)
   :flow-id flow-id})

(defmethod wl/handle-request :signals-map [{:keys [client-name]}]
  (ut/pp [:get-signals-map client-name])
  (let [bm {}]
    @signals-atom))

(defmethod wl/handle-request :rules-map [{:keys [client-name]}]
  (ut/pp [:get-rules-map client-name])
  (let [bm {}]
    @rules-atom))

(defmethod wl/handle-request :solvers-map [{:keys [client-name]}]
  (ut/pp [:get-solvers-map client-name])
  (let [bm {}]
    @solvers-atom))

(defonce client-helper-atom (atom {}))

(defmethod wl/handle-request :signals-history [{:keys [client-name signal-name]}]
  ;;(ut/pp [:get-signals-history client-name :for signal-name])
  (inc-score! client-name :push)
  ;; (ut/pp [:PUSH-signals-history client-name signal-name])
  (let [cc (get-in @signals-atom [signal-name :signal])
        ccw (vec (ut/where-dissect cc))
        history (select-keys (get @last-signals-history-atom signal-name {}) ccw)
        ;last-payload (get-in @client-helper-atom [client-name signal-name])
        ;is-new? (not= last-payload history)
        ]
    ;(ut/pp [:signals-history signal-name ccw])
    ;(ut/pp [:hist-keys (keys @last-signals-history-atom)])
    ;; (into {} (for [[k hist] (select-keys @last-signals-history-atom ccw)]
    ;;   {k (vec (map first (take-last 10 (sort-by last hist))))}
    ;;   ))
    ;; (if is-new? 
    ;;   (do 
    ;;     (swap! client-helper-atom assoc-in [client-name signal-name] history)
    ;;     history) 
    ;;   :no-updates)
    history))

(defmethod wl/handle-request :save-signals-map [{:keys [client-name data-map]}]
  (ut/pp [:saving-signals-map client-name])
  (let [bm {}]
    (reset! signals-atom data-map)))

(defmethod wl/handle-request :save-rules-map [{:keys [client-name data-map]}]
  (ut/pp [:saving-rules-map client-name])
  (let [bm {}]
    (reset! rules-atom data-map)))

(defmethod wl/handle-request :save-solvers-map [{:keys [client-name data-map]}]
  (ut/pp [:saving-solvers-map client-name])
  (let [bm {}]
    (reset! solvers-atom data-map)))

(defmethod wl/handle-request :save-custom-flow-block [{:keys [client-name name block-map]}]
  (ut/pp [:saving-custom-flow-block client-name :for name])
  (let [bm {name block-map}]
    (ut/pp [:saving-new-custom-block bm])))

;;(def client-param-lucene-history (atom {}))
;; (def client-param-lucene-history (ut/thaw-atom {} "./data/atoms/client-param-lucene-history-atom.edn"))

(defmethod wl/handle-request :sync-client-params [{:keys [client-name params-map]}]
  ;; (ut/pp [:sync-client-params-from client-name (keys (or (dissoc params-map nil) {}))])
  ;; set running flow from client-name/flow-id
  (let [params (or (dissoc params-map nil) {})
        kps (ut/kvpaths params)
        make-key (fn [x] (str :client-param "-" (cstr/join ">" x)))
        these-keys (map make-key kps)]
    (swap! params-atom assoc client-name params) ;; push the actual params
    ;; now we update the lucene index...
    ;; (doseq [kk (cset/difference ;; trying to only do the ones that are gone to avoid more work than needed, since existing will get overwritten anways
    ;;             (set these-keys)
    ;;             (set (get @client-param-lucene-history client-name)))] ;; clean up ones from last time in case they were deleted
    ;;   (search/delete-document search/index-writer kk))
    ;; (doseq [kp kps]
    ;;   (let [doc-key (make-key kp)]
    ;;     (search/add-or-update-document search/index-writer doc-key
    ;;                                    {:content (str kp " - " (get params kp)) :type :client-param
    ;;                                     :row {:client-name client-name :keypath kp}})))
    ;(swap! client-param-lucene-history assoc client-name these-keys)
    [:got-it!]))

(defmethod wl/handle-request :run-flow2 [{:keys [client-name flowmap flow-id]}]
  (ut/pp [:running-flow-map-from client-name])
  ;; set running flow from client-name/flow-id
  (let []
    ;; run it!
    (do ;(ut/pp [:flowmap-returned-val return-val])
        ;(ut/pp [:flowmap-returned-map return-map])
        ;(ut/pp [:channel-history @flow-db/channel-history])
        ;; set flow finished from client-name/flow-id
      {:client-name client-name
       :flow-id flow-id
       :server-map "finished-flowmap"
       :return-map "return-map"
       :return-val "return-val"})))

(defonce trigger-words-atom (atom {}))

(defmethod wl/handle-request :schedule-flow [{:keys [schedule client-name flow-id trigger-words trigger-word-insert]}]
  (if (not (nil? trigger-words))
    (do (ut/pp [:flow-start-trigger-words flow-id :when trigger-words :send-to trigger-word-insert :created-by client-name])
        (swap! trigger-words-atom assoc (cstr/lower-case trigger-words) {:flow-id flow-id :created-by client-name :trigger-word-insert trigger-word-insert})
        (sql-exec flows-db (to-sql {:insert-into [:live_schedules]
                                    :values [{:flow-id flow-id
                                              :override (str trigger-word-insert)
                                              :schedule (str trigger-words)}]}))
        :scheduled!)
    (when schedule
      (do (ut/pp [:schedule-flow-start flow-id :for schedule :by client-name])
          (let [sched-vec [(keyword (get schedule :period))  ;; needs to be keyword
                           (edn/read-string (get schedule :every))]] ;; [:seconds 45]
            (ut/pp [:schedule-flow flow-id :for sched-vec :by client-name])
            (schedule! sched-vec (materialize-flowmap :server flow-id flow-id {})
                       {:flow-id flow-id
                        :increment-id? false
                        :close-on-done? true
                        :debug? false}) ;; add overrides to opts-map
            :scheduled!)))))

(defmethod wl/handle-request :voice-trigger [{:keys [client-name voice-text]}]
  (ut/pp [:voice-text-trigger-analyzing voice-text :from client-name])
  (doseq [[k {:keys [flow-id trigger-words trigger-word-insert]}] @trigger-words-atom
          :let [match? (cstr/starts-with? (cstr/replace (cstr/lower-case voice-text) "," "") k)]
          :when match?]
    ;(materialize-flowmap :server flow-id flow-id {})
    (flow! client-name flow-id flow-id nil {:increment-id? false
                                            :close-on-done? true ; false ;true
                                            :debug? false
                                            :overrides {trigger-word-insert voice-text}})))

(defmethod wl/handle-request :get-status [{:keys [client-name]}]
  (ut/pp [:client-status-check-from client-name])
  (inc-score! client-name :push)
  (ut/pp [:PUSH-get-status! client-name])
  {:statuses (get @queue-status client-name)
   :data (get @queue-data client-name)})

(defmethod wl/handle-request :get-flow-statuses [{:keys [client-name]}]
  (inc-score! client-name :push)
  (ut/pp [:PUSH-get-flow-status! client-name])
  (let [fss (flow-statuses)
        ;fssk (vec (filter #(not (cstr/includes? (str %) "-solver-flow-")) (vec (keys fss))))
        fssk (vec (keys fss))
        payload   (merge (select-keys fss fssk)
                         (into {} (for [[k v] @processes]
                                    {k (-> v
                                           (assoc :process? true)
                                           (dissoc :output)
                                           (dissoc :process))})))]
    ;(ut/pp [:get-flow-statuses-from client-name [payload]])
    payload))

(defmethod wl/handle-request :delete-kit-rows [{:keys [client-name where]}]
  (ut/pp [:delete-kit-rows-from client-name])
  (let [sql-stmt {:delete-from [:kits]
                  :where where}
        sql-str (to-sql sql-stmt)]
    ;(ut/pp [:deleting sql-str])
    (sql-exec system-db sql-str)
    [:done]))


;; (defn make-watcher [keypath client-name handler-fn]
;;   (fn [key atom old-state new-state]
;;     (let [old-value (get-in old-state keypath)
;;           new-value (get-in new-state keypath)]
;;       (when (and new-value (not= old-value new-value))
;;         (handler-fn keypath client-name new-value)))))

;; (defn add-watcher [keypath client-name fn]
;;   (let [watcher (make-watcher keypath client-name fn)]
;;     (add-watch flow-db/results-atom (str client-name "-" (str keypath)) watcher)
;;     (swap! atoms-and-watchers assoc keypath {client-name watcher})))






;; (defn add-watcher [keypath client-name fn flow-key sub-type] ;; old version
;;   (let [watcher (make-watcher keypath client-name fn)
;;         ;status? (or (some #(= % :*running?) keypath) (some #(= % :*done?) keypath))
;;         status? (cstr/includes? (str keypath) ":*" )]
;;     (add-watch (if status?
;;                  flow-status
;;                  flow-db/results-atom) (str client-name "-" (str keypath) "-" sub-type) watcher)
;;     (swap! atoms-and-watchers assoc-in [client-name flow-key]
;;            {:created (ut/get-current-timestamp)
;;             :keypath keypath})))



;; (defn make-watcher [keypath handler-fn] ;; new
;;   (fn [key atom old-state new-state]
;;     (let [old-value (get-in old-state keypath)
;;           new-value (get-in new-state keypath)]
;;       (when (and (not (nil? new-value))
;;                  (not= old-value new-value))
;;         (doseq [client-name (get @client-subscriptions keypath)]
;;           (handler-fn keypath client-name new-value)
;;           (swap! last-values assoc-in [keypath client-name] new-value))))))

;; (defn add-watcher [keypath client-name fn flow-key sub-type & [flow-id]] ;; new
;;   (let [watcher (make-watcher keypath fn)
;;         status? (cstr/includes? (str keypath) ":*")
;;         watch-key (str keypath "-" sub-type)
;;         atom-to-watch (if status? flow-status flow-db/results-atom)]
;;     (remove-watch atom-to-watch watch-key)
;;     (add-watch atom-to-watch watch-key watcher)
;;     (swap! client-subscriptions assoc keypath (conj (get @client-subscriptions keypath []) client-name))
;;     (swap! atoms-and-watchers assoc-in [client-name flow-key]
;;            {:created (ut/get-current-timestamp)
;;             :sub-type sub-type
;;             :watch-key watch-key
;;             :flow-id flow-id
;;             :keypath keypath})))



;; (defn remove-watcher [keypath client-name sub-type] ;; old pre client-subscriptions
;;   (let [atom-to-watch (get-in @flow-db/results-atom keypath)
;;         watcher (get-in @atoms-and-watchers [keypath client-name])]
;;     (remove-watch atom-to-watch (str client-name "-" (str keypath) "-" sub-type))
;;     (swap! atoms-and-watchers ut/dissoc-in [client-name keypath])))

;; (defn remove-watcher [keypath client-name sub-type]
;;   (let [watch-key (str keypath "-" sub-type)
;;         atom-to-watch (if (cstr/includes? (str keypath) ":*") flow-status flow-db/results-atom)]
;;     (remove-watch atom-to-watch watch-key)
;;     (swap! client-subscriptions update keypath (partial remove #(= % client-name)))
;;     (when (empty? (get @client-subscriptions keypath))
;;       (swap! client-subscriptions dissoc keypath))
;;     (swap! atoms-and-watchers ut/dissoc-in [client-name keypath])))


(defonce screen-child-atoms (atom {}))
(defonce param-child-atoms (atom {}))
(defonce panel-child-atoms (atom {}))
(defonce flow-child-atoms (atom {})) ;; flows

(defn get-atom-splitter [key ttype child-atom parent-atom]
  (if-let [child-atom (get @child-atom key)]
    child-atom
    (let [;base-dir (str "./data/atoms/" (cstr/replace (str ttype) ":" ""))
          ;_ (ext/create-dirs base-dir) ;; just in case...
          ;new-child-atom  (ut/thaw-atom {} (str base-dir "/" key "-atom.edn"))
          new-child-atom  (atom {})]
      (swap! child-atom assoc key new-child-atom)
      (swap! new-child-atom assoc key (get @parent-atom key))
      (get @child-atom key))))

;; (defn get-or-create-child-atom [key]
;;   (if-let [child-atom (get @flow-child-atoms key)]
;;     child-atom
;;     (let [new-child-atom (atom {})]
;;       (swap! flow-child-atoms assoc key new-child-atom)
;;       (swap! new-child-atom assoc key (get @flow-db/results-atom key))
;;       (get @flow-child-atoms key))))

(add-watch flow-db/results-atom :master-watcher ;; flow watcher split of results-atom 
           (fn [_ _ old-state new-state]
             (doseq [key (keys new-state)]
               (if-let [child-atom (get @flow-child-atoms key)]
                 (swap! child-atom assoc key (get new-state key))
                 (let [new-child-atom (atom {})]
                   (swap! flow-child-atoms assoc key new-child-atom)
                   (swap! new-child-atom assoc key (get new-state key)))))))
;;; ^^ master atom watcher. dissects keys into child atoms to watch easier...

;; (defn break-up-flow-key-ext [key]
;;   (let [ff (cstr/split (-> (str key) (cstr/replace #":" "")) #"/")
;;         ff2 (cstr/split (last ff) #">")]
;;     ;(ut/pp [:splitter ff ff2])
;;     ;(vec (map keyword ff2))
;;     (vec (into [(keyword (first ff)) (first ff2)] (rest (for [e ff2] (keyword e)))))))

(defn break-up-flow-key-ext [key]
  (let [ff (cstr/split (-> (str key) (cstr/replace #":" "")) #"/")
        ff2 (cstr/split (last ff) #">")
        keyword-or-int (fn [s] (if (re-matches #"\d+" s) (Integer/parseInt s) (keyword s)))] ;; dont want to keywordize integer vector indexes/"keys"
    (vec (into [(keyword (first ff)) (first ff2)] (rest (for [e ff2] (keyword-or-int e)))))))

(defn make-watcher [keypath flow-key client-name handler-fn & [no-save]]
  (fn [kkey atom old-state new-state]
    (let [client-name :all ;; test, no need for individual cache for clients. kinda pointless ATM
          old-value (get-in old-state keypath)
          new-value (get-in new-state keypath)
          sub-path (break-up-flow-key-ext flow-key)
          base-type (first sub-path)
          ;param-key (-> (str watch-key) (cstr/replace ":flow/" "") (cstr/replace ":" "") keyword)
          ;param-value (get-in @params-atom [client-name :flow param-key])
          last-value (get-in @last-values-per [client-name keypath])
          all-clients-subbed (for [c (keys @atoms-and-watchers)
                                   :when (some #(= % flow-key) (keys (get @atoms-and-watchers c)))] c)


          ;; keypath-wildcards-map {"*client-name*" client-name  ;; ALREADY IN SUB TO VALUE, NOT NEEDED HERE.
          ;;                        :*client-name* client-name}
          ;; keypath (walk/postwalk-replace keypath-wildcards-map keypath) ;; keypath wildcards... but sub still ref'd flow-key as wildcard for client in question
          ]
      ;;(ut/pp [:mkk-watcher flow-key client-name keypath (not= old-value new-value)])
      (when ;(or
       (and (not (nil? new-value))
                  ;(or ;(not= last-value new-value) 
            (not= old-value new-value)
                      ;(not= last-value new-value) ;; the actual last seen value on client...
                   ;(boolean? new-value)
                   ;   )
                ; (or (not= old-value new-value) 
                ;     ;(some #(cstr/starts-with? (str %) ":*") keypath)
                ;     )
            )
                 ;(not= last-value new-value) ;; temp remove. some weird issue
               ;  ) (and (not (nil? new-value))
                ;        (some #(cstr/starts-with? (str %) ":*") keypath)))
      ;;  (and (not (nil? new-value))
      ;;       (or (not= old-value new-value)
      ;;       ;)
      ;;       (not= last-value new-value)))

        (doseq [client-name all-clients-subbed]
          (handler-fn base-type keypath client-name new-value))

        ;(ut/pp [:make-watcher key keypath client-name new-value])
        ;(when (not (cstr/starts-with? (str keypath) ":tracker"))
        (when (and (not no-save) (ut/serializable? new-value)) ;; dont want to cache tracker updates and other ephemeral shit
            ;(swap! last-values assoc-in [keypath client-name] new-value)
          (swap! last-values assoc keypath new-value)
          (swap! last-values-per assoc-in [client-name keypath] new-value)) ;; if a new client loads a subbed screen, we want to have a cache avail
        ;  )
        ))))



;;; [:add-watcher! :fair-salmon-hawk-hailing-from-malpais :screen/error-monitor-vanessa3>:panels>:block-494>:name :param-sub [:screen "error-monitor-vanessa3" :panels :block-494 :name]]
;;; [:add-watcher! :screen :fair-salmon-hawk-hailing-from-malpais :screen/error-monitor-vanessa3>:panels>:block-899>:name :param-sub [:screen "error-monitor-vanessa3" :panels :block-899 :name] ("error-monitor-vanessa3" :panels :block-899 :name)]

(defn get-atom-from-keys [base-type sub-type sub-path keypath]
  (let [tracker? (= sub-type :tracker)
        flow?    (= base-type :flow) ;; (cstr/starts-with? (str flow-key) ":flow/")
        status?  (and (cstr/includes? (str keypath) ":*") flow?)
        client?  (= base-type :client)
        panel?   (= base-type :panel)
                ;param?  (= base-type :ext-param)
        screen?  (= base-type :screen) ;; (cstr/starts-with? (str flow-key) ":screen/")
        time?    (= base-type :time)
        signal?  (= base-type :signal)
        solver?  (= base-type :solver)
        solver-meta?  (= base-type :solver-meta)
        server?  (= base-type :server)]
    (cond status? flow-status
          tracker? flow-db/tracker
                              ;:else flow-db/results-atom
                              ;:else (get-or-create-child-atom (first keypath))
          signal? last-signals-atom ;; no need to split for now, will keep an eye on it
          solver? last-solvers-atom ;; no need to split for now, will keep an eye on it - besides, there is not "natural" keypath split ATM..
          solver-meta? last-solvers-atom-meta
          server? server-atom ;; no need to split for now, will keep an eye on it - don't expect LOTS of writes... but who knows
          time?   time-atom ;; zero need to split ever. lol, its like 6 keys
          panel?  (get-atom-splitter (keyword (second sub-path)) :panel panel-child-atoms panels-atom)
          client? (get-atom-splitter (keyword (second sub-path)) :client param-child-atoms params-atom)
          flow?   (get-atom-splitter (first keypath) :flow flow-child-atoms flow-db/results-atom)
          screen? (get-atom-splitter (second sub-path) :screen screen-child-atoms screens-atom)
          :else   (get-atom-splitter (first keypath) :flow flow-child-atoms flow-db/results-atom))))

(defn add-watcher [keypath client-name fn flow-key sub-type & [flow-id]] ;; flow id optional is for unsub stuff later (NOT reduntant)
  (let [;;client-name :all ;; test
        ;watch-key (str client-name "-" (str keypath) "-" sub-type)
        ;watcher (make-watcher keypath flow-key client-name fn (= sub-type :tracker))
        sub-path (break-up-flow-key-ext flow-key)

        ;; keypath-wildcards-map {"*client-name*" client-name  ;; ALREADY IN SUB TO VALUE, NOT NEEDED HERE.
        ;;                        :*client-name* client-name}
        ;; keypath (walk/postwalk-replace keypath-wildcards-map keypath) ;; keypath wildcards... but sub still ref'd flow-key as wildcard for client in question
        ;; sub-path (walk/postwalk-replace keypath-wildcards-map sub-path)

        watch-key (str :all "-" (str keypath) "-" sub-type "-" flow-key)

        base-type (first sub-path)
        tracker? (= sub-type :tracker)
        flow?    (= base-type :flow) ;; (cstr/starts-with? (str flow-key) ":flow/")
        status?  (and (cstr/includes? (str keypath) ":*") flow?)
        client?  (= base-type :client)
        panel?   (= base-type :panel)
        ;param?  (= base-type :ext-param)
        screen?  (= base-type :screen) ;; (cstr/starts-with? (str flow-key) ":screen/")
        time?    (= base-type :time)
        signal?  (= base-type :signal)
        solver?  (= base-type :solver)
        solver-meta?  (= base-type :solver-meta)
        server?  (= base-type :server)
        keypath  (cond ;flow? keypath 
                   signal? (vec (rest keypath))
                   solver? (vec (into [(keyword (second sub-path))] (vec (rest (rest sub-path))))) ;;(vec (rest keypath))
                   solver-meta? (vec (into [(keyword (second sub-path))] (vec (rest (rest sub-path))))) ;;(vec (rest keypath))
                   time?   (vec (rest keypath))
                   server? (vec (rest keypath))
                   screen? (vec (rest sub-path))
                   panel?  (vec (into [(keyword (second sub-path))] (vec (rest (rest sub-path)))))
                   client? (vec (into [(keyword (second sub-path))] (vec (rest (rest sub-path)))))
                   :else keypath)



        watcher (make-watcher keypath flow-key :all fn (= sub-type :tracker))
        ;;_ (ut/pp [:is-flow-id? (first keypath)])
        atom-to-watch (cond status? flow-status
                            tracker? flow-db/tracker
                            ;:else flow-db/results-atom
                            ;:else (get-or-create-child-atom (first keypath))
                            signal? last-signals-atom ;; no need to split for now, will keep an eye on it - besides there is not natural parent key "work" distribution like others
                            solver? last-solvers-atom ;; no need to split for now, will keep an eye on it - besides there is not natural parent key "work" distribution like others
                            solver-meta? last-solvers-atom-meta
                            server? server-atom ;; no need to split for now, will keep an eye on it - I don't expect LOTS of writes... but who knows
                            time?   time-atom ;; zero need to split ever. lol, its like 6 keys
                            panel?  (get-atom-splitter (keyword (second sub-path)) :panel panel-child-atoms panels-atom)
                            client? (get-atom-splitter (keyword (second sub-path)) :client param-child-atoms params-atom)
                            flow? (get-atom-splitter (first keypath) :flow flow-child-atoms flow-db/results-atom)
                            screen? (get-atom-splitter (second sub-path) :screen screen-child-atoms screens-atom)
                            :else (get-atom-splitter (first keypath) :flow flow-child-atoms flow-db/results-atom))]
    (remove-watch atom-to-watch watch-key)
    ;; (ut/pp [:add-watcher! base-type (keyword (second sub-path)) client-name flow-key watch-key sub-type sub-path keypath client? 
    ;;         ;(get-in atom-to-watch (vec (rest keypath)))
    ;;         ])
    (add-watch atom-to-watch watch-key watcher)
    (swap! atoms-and-watchers assoc-in [client-name flow-key]
           {:created (ut/get-current-timestamp)
            :sub-type sub-type
            :sub-path sub-path
            :base-type base-type
            ;:atom atom-to-watch ;; ?
            :flow-key flow-key
            :watch-key watch-key
            :flow-id flow-id
            :keypath keypath})))

;;(def client-subscriptions (atom {}))

(defn remove-watcher [keypath client-name sub-type flow-id flow-key]
  (try (let [sub-path (break-up-flow-key-ext flow-key)

             keypath-wildcards-map {"*client-name*" client-name  ;; ALREADY IN SUB TO VALUE, NOT NEEDED HERE.
                                    :*client-name* client-name}
             keypath (walk/postwalk-replace keypath-wildcards-map keypath) ;; keypath wildcards... but sub still ref'd flow-key as wildcard for client in question
             sub-path (walk/postwalk-replace keypath-wildcards-map sub-path)

             watch-key (str :all "-" (str keypath) "-" sub-type "-" flow-key)

             base-type (first sub-path)
             tracker? (= sub-type :tracker)
             flow?    (= base-type :flow) ;; (cstr/starts-with? (str flow-key) ":flow/")
             status?  (and (cstr/includes? (str keypath) ":*") flow?)
             client?  (= base-type :client)
             panel?   (= base-type :panel)
                     ;param?  (= base-type :ext-param)
             screen?  (= base-type :screen) ;; (cstr/starts-with? (str flow-key) ":screen/")
             time?    (= base-type :time)
             signal?  (= base-type :signal)
             solver?  (= base-type :solver)
             solver-meta?  (= base-type :solver-meta)
             server?  (= base-type :server)
             keypath  (cond ;flow? keypath 
                        signal? (vec (rest keypath))
                        solver? (vec (into [(keyword (second sub-path))] (vec (rest (rest sub-path))))) ;; (vec (rest keypath))
                        solver-meta? (vec (into [(keyword (second sub-path))] (vec (rest (rest sub-path))))) ;;(vec (rest keypath))
                        time?   (vec (rest keypath))
                        screen? (vec (rest sub-path))
                        server? (vec (rest sub-path))
                        panel?  (vec (into [(keyword (second sub-path))] (vec (rest (rest sub-path)))))
                        client? (vec (into [(keyword (second sub-path))] (vec (rest (rest sub-path)))))
                        :else keypath)

            ;;  keypath-wildcards-map {"*client-name*" client-name ;; ALREADY IN SUB TO VALUE, NOT NEEDED HERE.
            ;;                         :*client-name* client-name}
            ;;  keypath (walk/postwalk-replace keypath-wildcards-map keypath) ;; keypath wildcards... but sub still ref'd flow-key as wildcard for client in question
            ;;  sub-path (walk/postwalk-replace keypath-wildcards-map sub-path)

             atom-to-watch (cond status? flow-status
                                 tracker? flow-db/tracker
                                         ;:else flow-db/results-atom
                                         ;:else (get-or-create-child-atom (first keypath))
                                 signal? last-signals-atom ;; no need to split for now, will keep an eye on it
                                 solver? last-solvers-atom ;; no need to split for now, will keep an eye on it - besides there is not natural parent key "work" distribution like others
                                 solver-meta? last-solvers-atom-meta
                                 server? server-atom ;; no need to split for now, will keep an eye on it - don't expect LOTS of writes... but who knows
                                 time?   time-atom ;; zero need to split ever. lol, its like 6 keys
                                 panel?  (get-atom-splitter (keyword (second sub-path)) :panel panel-child-atoms panels-atom)
                                 client? (get-atom-splitter (keyword (second sub-path)) :client param-child-atoms params-atom)
                                 flow? (get-atom-splitter (first keypath) :flow flow-child-atoms flow-db/results-atom)
                                 screen? (get-atom-splitter (second sub-path) :screen screen-child-atoms screens-atom)
                                 :else (get-atom-splitter (first keypath) :flow flow-child-atoms flow-db/results-atom))]
         (ut/pp [:removing-watcher keypath client-name sub-type flow-id watch-key])
         (remove-watch atom-to-watch watch-key)
    ;; (swap! client-subscriptions update keypath (partial remove #(= % client-name)))
    ;; (when (empty? (get @client-subscriptions keypath))
    ;;   (swap! client-subscriptions dissoc keypath))
         (swap! atoms-and-watchers ut/dissoc-in [client-name flow-key])) (catch Throwable e (ut/pp [:remove-watcher e]))))

;; (defn remove-watcher-old [keypath client-name sub-type flow-id flow-key]
;;   (try (let [watch-key (str client-name "-" (str keypath) "-" sub-type "-" flow-key)
;;             ;;  _ (ut/pp [:removing-watcher-try!!! keypath client-name sub-type flow-id flow-key])
;;              atom-to-watch (if (cstr/includes? (str keypath) ":*")
;;                              flow-status
;;                              ;flow-db/results-atom
;;                              ;(get-or-create-child-atom (first keypath))
;;                              (get-atom-splitter (first keypath) :flow flow-child-atoms flow-db/results-atom))]
;;     (ut/pp [:removing-watcher keypath client-name sub-type flow-id watch-key])
;;     (remove-watch atom-to-watch watch-key)
;;     ;; (swap! client-subscriptions update keypath (partial remove #(= % client-name)))
;;     ;; (when (empty? (get @client-subscriptions keypath))
;;     ;;   (swap! client-subscriptions dissoc keypath))
;;     (swap! atoms-and-watchers ut/dissoc-in [client-name flow-key])) (catch Throwable e (ut/pp [:remove-watcher e]))))

(defn replace-flow-key-vars [flow-key client-name]
  (let [client-name-str (cstr/replace (str client-name) ":" "")]
    (edn/read-string ;; should always be a keyword coming in
     (ut/replace-multiple (str flow-key)
                          {"*client-name*" client-name-str}))))

(defmethod wl/handle-request :unsub-to-flow-value [{:keys [client-name flow-key]}]
  (let [;;orig-flow-key flow-key
        ;;flow-key (replace-flow-key-vars flow-key client-name)
        ;;sub (get-in @atoms-and-watchers [client-name flow-key] {})
        subbed-sub (get-in @param-var-crosswalk [client-name flow-key])
        flow-id nil]
    (if (ut/ne? subbed-sub) ;; clean up ugly crosswalk keys; what in gods name have we done?

      (let [[srv-flow-key mapping-key] subbed-sub
            sub (get-in @atoms-and-watchers [client-name srv-flow-key] {})]
        (ut/pp [:unsubbing*w.var! client-name flow-key sub])
        (remove-watcher (:keypath sub) client-name (:sub-type sub) flow-id (:flow-key sub))
        (swap! param-var-mapping dissoc [client-name mapping-key]) ;; compound (single) key
        (swap! param-var-crosswalk ut/dissoc-in [client-name flow-key])
        (swap! param-var-key-mapping assoc client-name (vec (filter #(not (= (first %) flow-key)) (get @param-var-key-mapping client-name)))))

      (let [sub (get-in @atoms-and-watchers [client-name flow-key] {})]
        (ut/pp [:unsubbing! client-name flow-key sub])
        (remove-watcher (:keypath sub) client-name (:sub-type sub) flow-id (:flow-key sub))))))

(defn remove-watchers-for-flow [flow-id & [client-name]]
  (doseq [[c-name subs] @atoms-and-watchers
          :let [matching-subs (filter #(= (:flow-id %) flow-id) (vals subs))]
          :when (ut/ne? matching-subs)
                ;(and
                ; (not (empty? matching-subs))
                ; (if client-name (= c-name client-name) true))
          ]
    (ut/pp [:matching-subs c-name matching-subs])
    (doseq [sub matching-subs]
      (do
        (ut/pp [:removing (count matching-subs) :watchers :for flow-id c-name [[(:keypath sub) c-name (:sub-type sub) flow-id (:flow-key sub)]]])
        (remove-watcher (:keypath sub) c-name (:sub-type sub) flow-id (:flow-key sub))))))

;; remove-watcher [keypath client-name sub-type flow-id flow-key]

(defn break-up-flow-key [key]
  (let [ff (cstr/split (-> (str key) (cstr/replace #":" "")) #"/")
        ff2 (cstr/split (last ff) #">")]
    ;(ut/pp [:splitter ff ff2])
    ;(vec (map keyword ff2))
    [(first ff2) (keyword (last ff2))]))

(declare client-kp)

(defn clover-lookup [client-name flow-key & [signal?]]
  ;; flow-key expects the clover user-space version of the compound keyword kp bundle, i.e. :flow/flow-forever>final-math etc
 ;; ^^ still includes complex/messy side effecting *var logic. keeping for now since it will never be used in this instance, I DONT THINK
 ;; ^^ ....also this keeps it congrunt with the orginal sub-to-value fn - TODO
  (let [flow-key-orig flow-key
        flow-key-sub (replace-flow-key-vars flow-key client-name)

        flow-key-split (break-up-flow-key flow-key)
        flow-key-split-sub (break-up-flow-key flow-key-sub)
        vars? (not (= flow-key flow-key-sub))

        flow-key (if vars? flow-key-sub flow-key)
        flow-key-split (if vars? flow-key-split-sub flow-key-split)

        [flow-id step-id] flow-key-split ;(break-up-flow-key flow-key)
        ;signal? (true? signal?)
        keypath [flow-id step-id]
        sub-path (break-up-flow-key-ext flow-key)
        base-type (first sub-path)
       ;screen? (cstr/starts-with? (str flow-key) ":screen/")
        flow-client-param-path  (keyword (cstr/replace (str (first keypath) (last keypath)) #":" ">"))
        other-client-param-path (keyword (cstr/replace (cstr/join ">" (vec (rest sub-path))) ":" ""))
        client-param-path (if (= base-type :flow) flow-client-param-path other-client-param-path)
        client-keypath (client-kp flow-key keypath base-type sub-path client-param-path)
        ssp (break-up-flow-key-ext flow-key-orig)
        req-client-kp (client-kp flow-key-orig (vec (break-up-flow-key flow-key-orig)) base-type ssp
                                 (keyword (cstr/replace (cstr/join ">" (vec (rest ssp))) ":" "")))

        _ (ut/pp [:flow-key flow-key vars? flow-key-sub flow-key-split flow-key-split-sub]) ;; this is all a clusterfuck, total rewrite of flow-key vars in future version. will be more extendable, sensical
        _ (when vars? (swap! param-var-mapping assoc [client-name client-keypath] req-client-kp))
        _ (when vars? (swap! param-var-crosswalk assoc-in [client-name flow-key-orig] [flow-key [client-name client-keypath]])) ;; all the precomputed values to delete the rest. ugh. mistake.
        ;;         ^^ flow-key as the CLIENT understands it.... client-keypath as the server understands it.... ^^
        _ (when vars? (swap! param-var-key-mapping assoc client-name (vec (distinct (conj (get @param-var-key-mapping client-name []) [flow-key-orig flow-key])))))

        lv (get @last-values keypath)]

    (ut/pp [:solver-lookup! flow-key base-type client-param-path keypath {:sub-path sub-path}])

    (cond (cstr/includes? (str flow-key) "*running?") false
          (= base-type :time)   (get @time-atom client-param-path)
          (= base-type :signal) (get @last-signals-atom client-param-path)
          (= base-type :solver) (get-in @last-solvers-atom (vec (into [(keyword (second sub-path))] (vec (rest (rest sub-path))))) lv) ;; (get @last-solvers-atom client-param-path)
          (= base-type :solver-meta) (get-in @last-solvers-atom-meta (vec (into [(keyword (second sub-path))] (vec (rest (rest sub-path))))) lv)
          (= base-type :server) (get @server-atom client-param-path lv)
          (= base-type :screen) (get-in @screens-atom (vec (rest sub-path)) lv)
          (= base-type :client) (get-in @params-atom (vec (into [(keyword (second sub-path))] (vec (rest (rest sub-path))))) lv)
          (= base-type :panel) (get-in @panels-atom (vec (into [(keyword (second sub-path))] (vec (rest (rest sub-path))))) lv)
          :else (get-in @flow-db/results-atom keypath lv) ;; assume flow
                        ;; ^^ SHOULD be the last val (persistently cached), on cold boot @results-atom will be empty anyways
          )))

;;;  :client-reaction-push! ["error-monitor-vanessa3" :panels :block-899 :name] :fair-salmon-hawk-hailing-from-malpais "drag-from-select-all-jvm_stddat!s!!" :error-monitor-vanessa3>name]
;;; [:client-reaction-push! ["error-monitor-vanessa3" :panels :block-899 :name] :fair-salmon-hawk-hailing-from-malpais "drag-from-seldect-all-jvm_sstddat!s!!" :error-monitor-vanessa3>name]

;; (defn millis-to-date [millis]
;;   (.toString (java.time.ZonedDateTime/ofInstant
;;               (java.time.Instant/ofEpochMilli millis)
;;               (java.time.ZoneId/systemDefault))))

(defn millis-to-date [millis]
  (let [zdt (java.time.ZonedDateTime/ofInstant
             (java.time.Instant/ofEpochMilli millis)
             (java.time.ZoneId/systemDefault))
        formatter (java.time.format.DateTimeFormatter/ofPattern "yyyy-MM-dd HH:mm:ss SSS")]
    (.format zdt formatter)))

(defn run-solver [solver-name]
  (let [solver-map (get @solvers-atom solver-name)
        use-cache? (get solver-map :cache? false)
        ;;solver-map (walk/postwalk-replace {} solver-map) ;; we need a universal solver for compund keys - finds needed, resolves them, and replaces them. similar to make-watcher / click-param resolver?
        vdata (get solver-map :data -1)
        vdata-clover-kps (vec (filter
                               #(and (keyword? %)
                                     (cstr/includes? (str %) "/")
                                     (not= % (keyword (str "solver/" (cstr/replace (str solver-name) ":" "")))) ;; dont recur resolve self if someone was foolish enough to attempt
                                     ;(not (cstr/includes? (str %) "solver/"))
                                     ;(not (cstr/includes? (str %) "solver-meta/")
                                     )
                               (ut/deep-flatten vdata)))
        vdata-clover-walk-map (into {}
                                    (for [kp vdata-clover-kps]
                                      {kp (try
                                            (clover-lookup :rvbbit-solver kp)
                                            (catch Exception e (ut/pp [:clover-lookup-error kp e])
                                                   (str "clover-param-lookup-error " kp)))}))
        vdata (if (ut/ne? vdata-clover-walk-map)
                (walk/postwalk-replace vdata-clover-walk-map vdata) vdata) ;; sub out all clover kp refs...
        _ (ut/pp [:vdata-clover-walk-map vdata-clover-walk-map])
        runner-name (get solver-map :type :clojure)
        runner-map (get-in config/settings [:runners runner-name] {})
        runner-type (get runner-map :type runner-name)
        timestamp (System/currentTimeMillis)
        _ (ut/pp [:trying solver-name runner-type timestamp {:runner-map runner-map :vdata vdata}])
        cache-key (when use-cache? (hash [vdata runner-name])) ;; since we could have 2 connections with the same data... I suppose.
        cache-val (when use-cache? (get @solvers-cache-atom cache-key))
        cache-hit? (true? (and use-cache? (vector? cache-val)))
        err? (fn [s] (let [s (str s)] (or (cstr/includes? s "Exception") (cstr/includes? s ":err") (cstr/includes? s ":error"))))
        timestamp-str (cstr/trim (str (when use-cache? "^")
                                      (when cache-hit? "*") " " (millis-to-date timestamp)))]
    ;; (ut/pp [:solverCACHE?? use-cache? cache-key cache-val cache-hit?])

    (cond

      cache-hit? (let [[output output-full] cache-val
                       meta-extra {:extra {:last-processed timestamp-str :cache-hit? cache-hit? :elapsed-ms 0}}
                       timestamp-str (str timestamp-str " (cache hit)")
                       new-history (vec (conj (get @last-solvers-history-atom solver-name []) timestamp-str))] ;; regardless of what it is, if its cached and enabled, we send it. IFYKYK
                   (ut/pp [:solver-cache-hit solver-name {:output output :output-full (assoc output-full :cache-hit? true)}])
                   (swap! last-solvers-atom assoc solver-name output)
                   (swap! last-solvers-atom-meta assoc solver-name (merge meta-extra output-full {:history (vec (reverse (take-last 20 new-history)))}))
                   (swap! last-solvers-history-atom assoc solver-name new-history))

      (= runner-type :nrepl)
      ;;(enqueue-task3
      ;; (fn []
      (try
        (let [repl-host (get-in runner-map [:runner :host])
              repl-port (get-in runner-map [:runner :port])
                 ;;output-full (evl/repl-eval vdata repl-host repl-port)
              {:keys [result elapsed-ms]} (ut/timed-exec (evl/repl-eval vdata repl-host repl-port))
              output-full result
              output (last (get-in output-full [:evald-result :value]))
              output-full (-> output-full
                              (assoc-in [:evald-result :output-lines]
                                        (try (count (remove empty? (get-in output-full [:evald-result :out])))
                                             (catch Exception _ 0)))
                              (assoc-in [:evald-result :values]
                                        (try (count (last (get-in output-full [:evald-result :value])))
                                             (catch Exception _ 0))))
              error? (err? output-full)
              runs (get @last-solvers-history-atom solver-name [])
              meta-extra {:extra {:last-processed timestamp-str :cache-hit? cache-hit? :elapsed-ms elapsed-ms :runs (count runs) :error? error?}}
              timestamp-str (str timestamp-str " (" elapsed-ms "ms)")
              new-history (vec (conj runs timestamp-str))]
          (ut/pp [:running-solver solver-name {:output output :output-full output-full}])
          (swap! last-solvers-atom assoc solver-name output)
          (swap! last-solvers-atom-meta assoc solver-name (merge meta-extra output-full {:history (vec (reverse (take-last 20 new-history)))}))
          (swap! last-solvers-history-atom assoc solver-name new-history)
          (when use-cache? (swap! solvers-cache-atom assoc cache-key [output output-full])))
        (catch Throwable e
          (do (ut/pp [:SOLVER-REPL-ERROR!!! (str e) :tried vdata :for solver-name :runner-type runner-type])
              (swap! last-solvers-atom-meta assoc solver-name {:error (str e)}))))
        ;; ))

      (= runner-type :flow) ;; no runner def needed for anon flow pulls
      (try
        (let [client-name :rvbbit-solver
              flowmap (get vdata :flowmap)
            ;; flowmap (if (string? flowmap)
            ;;           (if (cstr/includes? flowmap "/") flowmap (str "./flows/" flowmap))
            ;;           flowmap) ;; if supplied some dirs, we leave as is.. if its just a file name, we append a relative path
              opts (merge {:close-on-done? true
                           :increment-id? false
                           :timeout 10000} (get vdata :opts {}))
              return (get vdata :return) ;; alternate block-id to fetch when done
            ;;  flow-id (str (cstr/replace (str solver-name) ":" "") "-solver-flow-" timestamp)
              flow-id (str (cstr/replace (str solver-name) ":" "") "-solver-flow-")
              ;output (flow! client-name flowmap nil flow-id opts)
              {:keys [result elapsed-ms]} (ut/timed-exec (flow! client-name flowmap nil flow-id opts))
              output result
              output (ut/remove-namespaced-keys
                      (ut/replace-large-base64 output))
              output-val (get-in output [:return-val])
              output-val (if return ;;(keyword? return)
                           (get-in output [:return-maps flow-id return] output-val)
                           output-val)
              output-full {:req vdata
                           :value (-> (assoc output :return-maps (select-keys (get output :return-maps) [flow-id]))
                                      (dissoc :tracker) (dissoc :tracker-history))
                          ;:rval (get-in output [:return-val])
                           :output-val output-val
                           :flow-id flow-id
                           :return return}
              error? (err? output-full)
              runs (get @last-solvers-history-atom solver-name [])
              meta-extra {:extra {:last-processed timestamp-str :cache-hit? cache-hit? :elapsed-ms elapsed-ms :runs (count runs) :error? error?}}
              timestamp-str (str timestamp-str " (" elapsed-ms "ms)")
              new-history (conj runs timestamp-str)]
          (ut/pp [:running-solver-flow solver-name output-full])
          (swap! last-solvers-atom assoc solver-name output-val)
          (swap! last-solvers-atom-meta assoc solver-name (merge meta-extra output-full {:history (vec (reverse (take-last 20 new-history)))}))
          (swap! last-solvers-history-atom assoc solver-name new-history)
          (when use-cache? (swap! solvers-cache-atom assoc cache-key [output output-full])))
        (catch Throwable e
          (do (ut/pp [:SOLVER-FLOW-ERROR!!! (str e) :tried vdata :for solver-name :runner-type runner-type])
              (swap! last-solvers-atom-meta assoc solver-name {:error (str e)}))))

      :else ;; else we assume it's just data and keep it as is
      (let [output-full {:static-data vdata}
            runs (get @last-solvers-history-atom solver-name [])
            meta-extra {:extra {:last-processed timestamp-str :cache-hit? cache-hit? :elapsed-ms -1 runs (count runs)}}
            timestamp-str (str timestamp-str " (static)")
            new-history (conj runs timestamp-str)]
        (ut/pp [:solver-static solver-name vdata])
        (swap! last-solvers-atom assoc solver-name vdata)
        (swap! last-solvers-atom-meta assoc solver-name (merge meta-extra output-full {:history (vec (reverse (take-last 20 new-history)))}))
        (swap! last-solvers-history-atom assoc solver-name new-history)
        vdata))))

(defn process-signal [signal-name & [solver-dep?]]
  (doall
   (let [signals-map (select-keys @signals-atom [signal-name])
         signals-parts-map (into {} (for [[k {:keys [signal]}] signals-map] {k (vec (distinct (ut/where-dissect signal)))}))
         resolve-changed-fn (fn [obody sigk] (let [kps       (ut/extract-patterns obody :changed? 2)
                                                   logic-kps (into {} (for [v kps]
                                                                        (let [[_ & ss] v
                                                                               ;[[flow-id bid value alert?]] this
                                                                              ss (first ss)
                                                                               ;kp (get-in @atoms-and-watchers [:rvbbit-signals ss :keypath])
                                                                              v2 (get-in @last-signal-value-atom [sigk ss]) ;; should be the last time we evaluated this in THIS context.. ;; (get @last-values kp)
                                                                              new [:not [:= v2 ss]] ;; has it changed from the last time WE (context is important) looked at it?
                                                                               ;;_ (ut/pp [:resolve-changed-fn ss kp kps obody new])
                                                                              ]
                                                                          {v new}))) ;; compare against the last LITERAL value, not it's truthiness
                                                   ]
                                               (walk/postwalk-replace logic-kps obody)))
         signals-resolve-map (into {} (for [[k {:keys [signal]}] signals-map]
                                        {k (let [cmpkeys (vec (filter #(and (keyword? %) (cstr/includes? (str %) "/")) (ut/deep-flatten signal)))
                                                 vvals (select-keys (get-in @atoms-and-watchers [:rvbbit-signals]) cmpkeys)]
                                             (into {} (for [[k {:keys [base-type sub-type sub-path keypath]}] vvals
                                                            :let [;;_ (when (not= base-type :time) (ut/pp [:resolver-map!!? k base-type sub-type sub-path keypath]))
                                                                 ;;v3 (get @last-values keypath) ;; we dont want last-values we want the value NOW!
                                                                  v3 (get-in @(get-atom-from-keys base-type sub-type sub-path keypath) keypath (get @last-values keypath))
                                                                        ;_ (swap! last-signal-value-atom assoc k v3)
]] ;; extra cache for last value due to diff update cadence on last-values... TODO
                                                        {k v3})) ;; keypath is used as the actual key here, btw
                                             )}))
         nowah (System/currentTimeMillis) ;; want to have a consistent timestamp for this reaction event
         signals-resolved (into {} (for [[k v] signals-map]
                                     {k (walk/postwalk-replace (get signals-resolve-map k) (resolve-changed-fn (get v :signal) k))}))
         parts-work (into {} ;; important side effects / will transition to a doseq after debugging phase
                          (for [[kk vv] signals-parts-map]
                            {kk (into {} (for [vvv vv
                                               :let [rvv (walk/postwalk-replace (get signals-resolve-map kk) (resolve-changed-fn vvv kk))
                                                     honey-sql-str (to-sql {:select [[1 :vv]] :where rvv})
                                                           ;; need to calculate last-value from atom compared to new/curr-value to resolve [:changed? :val]
                                                     result (true? (ut/ne? (sql-query ghost-db honey-sql-str [:ghost-signal-resolve kk])))
                                                     part-key (keyword (str "part-" (cstr/replace (str kk) ":" "") "-" (ut/index-of vv vvv)))
                                                    ;;_ (ut/pp [:part-key kk part-key vvv])

                                                     _ (when true ;(not= result (get @last-signals-atom part-key))  ;; no need for dupe histories
                                                         (swap! last-signals-history-atom assoc-in [kk part-key]
                                                                (into (vec (take-last 12 (sort-by second (get-in @last-signals-history-atom [kk part-key])))) [[result nowah]])))
                                                     _ (when true ;(not= result (get @last-signals-atom vvv)) ;; no need for dupe histories
                                                         (swap! last-signals-history-atom assoc-in [kk vvv]
                                                                (into (vec (take-last 12 (sort-by second (get-in @last-signals-history-atom [kk vvv])))) [[result nowah]])))
                                                     _ (swap! last-signals-atom-stamp assoc vvv nowah)
                                                     _ (swap! last-signals-atom assoc vvv result)
                                                     _ (swap! last-signals-atom assoc part-key result)]] ;; last time we resolved this signal, will be used to stop re-runs on the same signal

                                           {[vvv rvv] result}))}))
         full-parts-work (into {} ;; all side effects / will transition to a doseq after debugging phase
                               (for [[kk vv] signals-resolved
                                     :let [honey-sql-str (to-sql {:select [[1 :vv]] :where vv})
                                           result (true? (ut/ne? (sql-query ghost-db honey-sql-str [:ghost-signal-resolve kk])))

                                           _ (when true ;(not= result (get @last-signals-atom kk))  ;; no need for dupe histories
                                               (swap! last-signals-history-atom assoc-in [kk kk]
                                                      (into (vec (take-last 19 (sort-by second (get-in @last-signals-history-atom [kk kk])))) [[result nowah]])))
                                           _ (swap! last-signals-atom assoc kk result)
                                           _ (when (and solver-dep? (true? result)) ;; singal resolved as true and is a solver trigger..
                                               ;(ut/pp [:...solver-dep-TRUE! :GO kk result])
                                               (doseq [[sk sv] @solvers-atom
                                                       :when (or (= (get sv :signal) kk)
                                                                 (= (get sv :signal) (keyword (str "signal/" (cstr/replace (str kk) ":" "")))))]
                                                 ;(ut/pp [:running-solver! sk :dep-met kk])
                                                 (enqueue-task4 (fn [] (run-solver sk)))))
                                           _ (swap! last-signals-atom-stamp assoc kk nowah)]]
                                 {[kk vv] result}))]
     (doseq [[k v] signals-resolve-map]
       (doseq [[kk vv] v]
         (swap! last-signal-value-atom assoc-in [k kk] vv))) ;; when all done, set "last-value" so future :changed statements work - has to be PER signal, else we miss some                                
     )))

(defn process-signals-reaction [base-type keypath new-value client-param-path]
  (when (not (= base-type :time)) (ut/pp [:process-signals-reaction! base-type keypath new-value client-param-path]))
  (let [re-con-key (keyword (str (cstr/replace (str base-type) ":" "") "/" (cstr/replace (str client-param-path) ":" "")))
        valid-signals (map first (vec (filter #(some (fn [x] (= x re-con-key)) (last %)) @signal-parts-atom)))
        ;; _ (ut/pp [:signal-processing! (vec valid-signals)]) ;; useful but annoying

        ;; signals-map (select-keys @signals-atom valid-signals)
        ;; signals-parts-map (into {} (for [[k {:keys [signal]}] signals-map] {k (vec (distinct (ut/where-dissect signal)))}))
        ;; resolve-changed-fn (fn [obody sigk] (let [kps       (ut/extract-patterns obody :changed? 2)
        ;;                                      logic-kps (into {} (for [v kps]
        ;;                                                           (let [[_ & ss] v
        ;;                                                                 ;[[flow-id bid value alert?]] this
        ;;                                                                 ss (first ss)
        ;;                                                                 ;kp (get-in @atoms-and-watchers [:rvbbit-signals ss :keypath])
        ;;                                                                 v2 (get-in @last-signal-value-atom [sigk ss]) ;; should be the last time we evaluated this in THIS context.. ;; (get @last-values kp)
        ;;                                                                 new [:not [:= v2 ss]] ;; has it changed from the last time WE (context is important) looked at it?
        ;;                                                                 ;;_ (ut/pp [:resolve-changed-fn ss kp kps obody new])
        ;;                                                                 ]
        ;;                                                             {v new}))) ;; compare against the last LITERAL value, not it's truthiness
        ;; ]
        ;;                                  (walk/postwalk-replace logic-kps obody)))
        ;; signals-resolve-map (into {} (for [[k {:keys [signal]}] signals-map]
        ;;                                {k (let [cmpkeys (vec (filter #(and (keyword? %) (cstr/includes? (str %) "/")) (ut/deep-flatten signal)))
        ;;                                         vvals (select-keys (get-in @atoms-and-watchers [:rvbbit-signals]) cmpkeys)]
        ;;                                     (into {} (for [[k {:keys [keypath]}] vvals
        ;;                                                    :let [v3 (get @last-values keypath)
        ;;                                                          ;_ (swap! last-signal-value-atom assoc k v3)
        ;;                                                          ]] ;; extra cache for last value due to diff update cadence on last-values... TODO
        ;;                                                {k v3})) ;; keypath is used as the actual key here, btw
        ;;                                     )}))
        ;; signals-resolved (into {} (for [[k v] signals-map] 
        ;;                             {k (walk/postwalk-replace (get signals-resolve-map k) (resolve-changed-fn (get v :signal) k))}))
        ;; parts-work (into {} ;; important side effects / will transition to a doseq after debugging phase
        ;;                  (for [[kk vv] signals-parts-map]
        ;;                    {kk (into {} (for [vvv vv
        ;;                                       :let [rvv (walk/postwalk-replace (get signals-resolve-map kk) (resolve-changed-fn vvv kk))
        ;;                                             honey-sql-str (to-sql {:select [[1 :vv]] :where rvv})
        ;;                                             ;; need to calculate last-value from atom compared to new/curr-value to resolve [:changed? :val]
        ;;                                             result (true? (ut/ne? (sql-query ghost-db honey-sql-str [:ghost-signal-resolve kk])))
        ;;                                             part-key (keyword (str "part-" (cstr/replace (str kk) ":" "") "-" (ut/index-of vv vvv)))
        ;;                                             _ (ut/pp [:part-key kk part-key vvv])
        ;;                                             _ (swap! last-signals-atom assoc vvv result)
        ;;                                             _ (swap! last-signals-atom assoc part-key result)
        ;;                                             _ (swap! last-signals-atom-stamp assoc vvv (System/currentTimeMillis))]] ;; last time we resolved this signal, will be used to stop re-runs on the same signal

        ;;                                   {[vvv rvv] result}))}))
        ;; full-parts-work (into {} ;; important side effects / will transition to a doseq after debugging phase
        ;;                       (for [[kk vv] signals-resolved
        ;;                             :let [honey-sql-str (to-sql {:select [[1 :vv]] :where vv})
        ;;                                   result (true? (ut/ne? (sql-query ghost-db honey-sql-str [:ghost-signal-resolve kk])))
        ;;                                   _ (swap! last-signals-atom assoc kk result)
        ;;                                   _ (swap! last-signals-atom-stamp assoc kk (System/currentTimeMillis))]]
        ;;                         {[kk vv] result}))
        ]

    (doseq [signal valid-signals
            :let [solver-deps (vec (distinct (for [[_ v] @solvers-atom]
                                               (keyword (-> (get v :signal) str (cstr/replace ":signal/" "") (cstr/replace ":" "")))
                                               ;; ^^^ we already KNOW its a signal, so we just need the name, unless omitted
                                               )))
                  solver-to-run? (true? (some #(= signal %) solver-deps))]] ;; run in parallel later perhaps
      ;; (ut/pp [:process-signal! signal solver-to-run? (when solver-to-run? 
      ;;                                                  (str (get @last-signals-atom signal))
      ;;                                                  )])
      (process-signal signal solver-to-run?))
    ;; use signals-resolve-map to materialized values and the SQL run it FROM DUAL

    ;; (ut/pp [:process-signal-sub-data base-type keypath new-value client-param-path valid-signals signals-map
    ;;         {:where-parts parts-work}
    ;;         {:runs? full-parts-work}])

            ;; (doseq [[k v] signals-resolve-map] 
            ;;   (doseq [[kk vv] v]
            ;;     (swap! last-signal-value-atom assoc-in [k kk] vv))) ;; when all done, set "last-value" so future :changed statements work - has to be PER signal, else we miss some
    ))

(declare process-solver)

(defn process-solvers-reaction [base-type keypath new-value client-param-path]
  (when (not (= base-type :time)) (ut/pp [:process-signals-reaction! base-type keypath new-value client-param-path]))
  (let [re-con-key (keyword (str (cstr/replace (str base-type) ":" "") "/" (cstr/replace (str client-param-path) ":" "")))
        valid-signals (map first (vec (filter #(some (fn [x] (= x re-con-key)) (last %)) @signal-parts-atom)))]

    (doseq [signal valid-signals] ;; run in parallel later perhaps
      (process-solver signal))))

;;         flow-key-split (break-up-flow-key flow-key)
;; flow-key-split-sub (break-up-flow-key flow-key-sub)
;; vars? (not (= flow-key flow-key-sub))
;; _ (ut/pp [:flow-key flow-key vars? flow-key-sub flow-key-split flow-key-split-sub])
;; _ (when vars? (swap! param-var-mapping assoc [client-name flow-key-split-sub] flow-key-sub))

(defn send-reaction [base-type keypath client-name new-value]
  (let [;;_ (ut/pp [:send-reaction-keypath keypath client-name])
        keypath (get @param-var-mapping [client-name keypath] keypath) ;; get the REQUESTED keypath from the client, even if we replaced it...

        flow-client-param-path (keyword (cstr/replace (str (first keypath) (last keypath)) #":" ">"))
        other-client-param-path (keyword (cstr/replace (cstr/join ">" keypath) ":" "")) ;;(keyword (cstr/join ">" keypath))
        client-param-path (if (= base-type :flow)
                            flow-client-param-path
                            other-client-param-path)
        signal? (= client-name :rvbbit-signals)]

  ;;  (when (= base-type :server) (ut/pp [:SERVER=PARAM! base-type keypath client-name new-value]))

  ;;  (when (and (not= base-type :time) (not signal?))
  ;;    (ut/pp [:send-reaction! base-type keypath client-name new-value client-param-path @param-var-mapping]))

  ;;  (when (and (not signal?) (not (= base-type :time))) ;; temp to eliminate console spam
  ;;    (ut/pp [(if signal?
  ;;              :signal-reaction-process!
  ;;              :client-reaction-push!) base-type keypath client-name new-value client-param-path]))

    (when (and (not= base-type :time)
               (not= base-type :signal))
      (async/thread ;; really expensive logging below. temp
        (let [summary (-> (str keypath) (cstr/replace " " "_") (cstr/replace "/" ":"))
              b? (boolean? new-value)
              fp (str "./reaction-logs/" (str (System/currentTimeMillis)) "@@" client-name "=" summary (when b? (str "*" new-value)) ".edn")]
          (ext/create-dirs "./reaction-logs/")
          (ut/pretty-spit fp {:client-name client-name
                              :keypath keypath
                              :value (ut/replace-large-base64 new-value)
                              :last-vals (ut/replace-large-base64 (get @last-values-per client-name))
                              :subs-at-time (keys (get @atoms-and-watchers client-name))
                              :flow-child-atoms (keys @flow-child-atoms)} 125))))

    (if (not signal?)
      (kick client-name [(or base-type :flow) client-param-path] new-value nil nil nil)
      (do ;;(ut/pp [:signal-or-solver base-type keypath client-name])
        (process-signals-reaction base-type keypath new-value client-param-path)))))



;; (defn send-signal [base-type keypath client-name new-value]
;;    (let [flow-client-param-path (keyword (cstr/replace (str (first keypath) (last keypath)) #":" ">"))
;;          other-client-param-path (keyword (cstr/replace (cstr/join ">" keypath) ":" "")) ;;(keyword (cstr/join ">" keypath))
;;          client-param-path (if (= base-type :flow)
;;                              flow-client-param-path
;;                              other-client-param-path)]

;;      (ut/pp [:signal-push! base-type keypath client-name new-value client-param-path])

;;     ;; (async/thread ;; really expensive logging below. temp
;;     ;;   (let [summary (-> (str keypath) (cstr/replace " " "_") (cstr/replace "/" ":"))
;;     ;;         b? (boolean? new-value)
;;     ;;         fp (str "./reaction-logs/" (str (System/currentTimeMillis)) "@@" client-name "=" summary (when b? (str "*" new-value)) ".edn")]
;;     ;;     (ext/create-dirs "./reaction-logs/")
;;     ;;     (ut/pretty-spit fp {:client-name client-name
;;     ;;                         :keypath keypath
;;     ;;                         :value (ut/replace-large-base64 new-value)
;;     ;;                         :last-vals (ut/replace-large-base64 (get @last-values-per client-name))
;;     ;;                         :subs-at-time (keys (get @atoms-and-watchers client-name))
;;     ;;                         :flow-child-atoms (keys @flow-child-atoms)} 125)))

;;     ;; (kick client-name [(or base-type :flow) client-param-path] new-value nil nil nil)\
;;      ))




(defn send-reaction-runner [base-type keypath client-name new-value]
  (let [flow-id (first keypath)]
    (ut/pp [:reaction-runner flow-id keypath client-name]) ;; (ut/replace-large-base64 new-value)

    (kick client-name (vec (cons :flow-runner keypath)) new-value nil nil nil)
    ;(kick client-name (vec (cons :tracker keypath)) (get @flow-db/tracker flow-id) nil nil nil)
    ))

(defn purge-dead-client-watchers []
  (let [cc (dissoc (client-statuses) :rvbbit-scheduler)]
    (doseq [[k {:keys [last-seen-seconds]}] cc
            :let [subs (get @atoms-and-watchers k)]
            :when (> last-seen-seconds 6000)]

      (ut/pp [:dead-client :cleaning-up k])
      ;(ut/pp [:***DEAD-CLIENT-SUBS***! subs])

      ;; (doseq [[kk {:keys [keypath sub-type flow-id]}] subs] ;; temp remove since we are dealing with master watchers now with :* as client-name...
      ;;   (remove-watcher keypath k sub-type flow-id kk))

      (swap! atoms-and-watchers dissoc k) ;; remove client from watchers atom
      (swap! ack-scoreboard dissoc k)     ;; remove client client board
      )))

(defn accumulate-unique-runs [data]
  (let [merge-runs (fn [runs run]
                     (let [run-start (:start run)
                           run-end (:end run)]
                       (cond
                         ; If run with same start and end exists, return runs as is.
                         (some #(and (= (:start %) run-start)
                                     (= (:end %) run-end)) runs) runs
                         ; If run with same start exists without an end, replace if current has an end.
                         (some #(and (= (:start %) run-start) (nil? (:end %)) run-end) runs)
                         (conj (vec (remove #(and (= (:start %) run-start) (nil? (:end %))) runs)) run)
                         ; Otherwise, add run.
                         :else (conj runs run))))]
    (reduce (fn [acc entry]
              (reduce (fn [inner-acc [block-id run]]
                        (update inner-acc block-id (fn [runs]
                                                     (merge-runs (or runs []) run))))
                      acc (into [] entry)))
            {} data)))

(defn send-tracker-runner [base-type keypath client-name new-value]
  (let [flow-id (first keypath)]
    ;(ut/pp [:reaction-runner-tracker flow-id keypath client-name ]) ;; (ut/replace-large-base64 new-value)
    ;(kick client-name (vec (cons :flow-runner keypath)) new-value nil nil nil)
    (let [orig-tracker (get @flow-db/tracker flow-id)
          tracker (into {} (for [[k v] orig-tracker ;; remove condi non-starts. dont send to client. data noise. neccessary noise for reactions, but noise
                                 :when (not (get v :in-chan?))]
                             {k v}))
          ;tracker (select-keys tracker (last keypath))
          ;new? (not (some #(= % tracker) (get @tracker-history flow-id [])))
          ;new? (not= tracker (get @tracker-client-only flow-id []))
          condis (get-in @flow-db/status [flow-id :condis])
          running? (get @flow-status flow-id :*running?)
          first-tracker? (empty? (get-in @tracker-client-only [flow-id client-name]))]

      ;;(println (str keypath " is new? " new? tracker))

      (when true ;running? ;; true ;(and new? running?) ;; (not= tracker last-tracker)
        (swap! tracker-client-only assoc-in [flow-id client-name] tracker)
        ;; doing this in core? ;; (swap! tracker-history assoc flow-id (vec (conj (get @tracker-history flow-id []) tracker))) ;; for loop step history saving..

        (when false ;; DEPRECATED ;(not running?) ;; test to stop thrash... worth it?
          (kick client-name (vec (cons :tracker keypath))
                (if first-tracker?
                  (assoc tracker :*start! [{}])
                  tracker)
                nil nil nil))

        (when (not (empty? condis))  ;; still need this little guy tho
          (kick client-name (vec (cons :condis keypath)) condis nil nil nil))))))



(defn send-acc-tracker-runner [base-type keypath client-name new-value]
  (let [flow-id (first keypath)
        orig-tracker (get @flow-db/tracker flow-id)
        tracker (into {} (for [[k v] orig-tracker ;; remove condi non-starts. dont send to client. data noise. neccessary noise for reactions, but noise
                               :when (not (get v :in-chan?))]
                           {k v}))
        all-tracks (vec (conj (get @acc-trackers flow-id []) tracker))
        acc-tracker (accumulate-unique-runs all-tracks)]
    (swap! acc-trackers assoc flow-id all-tracks) ;; for next acc 
    (kick client-name (vec (cons :acc-tracker keypath))
          acc-tracker
          nil nil nil)))

(defn send-tracker-block-runner [base-type keypath client-name new-value]
  (let [flow-id (first keypath)]
    (let []
      (kick client-name (vec (cons :tracker-blocks keypath))
            (select-keys (get @flow-status flow-id) [:running-blocks :done-blocks :error-blocks :waiting-blocks])
            nil nil nil))))

(defn client-kp [flow-key keypath base-type sub-path client-param-path]
  (cond (cstr/includes? (str flow-key) "*running?") false
        (= base-type :time)   client-param-path
        (= base-type :signal) client-param-path
        (= base-type :solver) client-param-path
        (= base-type :solver-meta) (vec (into [(keyword (second sub-path))] (vec (rest (rest sub-path))))) ;;(vec sub-path) ;; client-param-path
        (= base-type :server) client-param-path
        (= base-type :screen) (vec (rest sub-path))
        (= base-type :client) (vec (into [(keyword (second sub-path))] (vec (rest (rest sub-path)))))
        (= base-type :panel) (vec (into [(keyword (second sub-path))] (vec (rest (rest sub-path)))))
        :else keypath ;; assume flow
        ))

(defn sub-to-value [client-name flow-key & [signal?]] ;;; IF CHANGED, REMEMBER TO ALSO UPDATE "CLOVER-LOOKUP" - TODO, combine the logic?
  (let [flow-key-orig flow-key
        flow-key-sub (replace-flow-key-vars flow-key client-name)

        flow-key-split (break-up-flow-key flow-key)
        flow-key-split-sub (break-up-flow-key flow-key-sub)
        vars? (not (= flow-key flow-key-sub))


        flow-key (if vars? flow-key-sub flow-key)
        flow-key-split (if vars? flow-key-split-sub flow-key-split)

        [flow-id step-id] flow-key-split ;(break-up-flow-key flow-key)
        signal? (true? signal?)
        keypath [flow-id step-id]
        sub-path (break-up-flow-key-ext flow-key)
        base-type (first sub-path)
       ;screen? (cstr/starts-with? (str flow-key) ":screen/")
        flow-client-param-path  (keyword (cstr/replace (str (first keypath) (last keypath)) #":" ">"))
        other-client-param-path (keyword (cstr/replace (cstr/join ">" (vec (rest sub-path))) ":" ""))
        client-param-path (if (= base-type :flow) flow-client-param-path other-client-param-path)
        client-keypath (client-kp flow-key keypath base-type sub-path client-param-path)
        ssp (break-up-flow-key-ext flow-key-orig)
        req-client-kp (client-kp flow-key-orig (vec (break-up-flow-key flow-key-orig)) base-type ssp
                                 (keyword (cstr/replace (cstr/join ">" (vec (rest ssp))) ":" "")))

          ;; keypath (cond ;flow? keypath 
          ;;           screen? (vec (rest sub-path))
          ;;           :else keypath)
          ;;clis (first (keys (get-in @last-values [keypath]))) ;; in case we are a new ""client"" (as is usual)

        _ (ut/pp [:flow-key flow-key vars? flow-key-sub flow-key-split flow-key-split-sub]) ;; this is all a clusterfuck, total rewrite of flow-key vars in future version. will be more extendable, sensical
        _ (when vars? (swap! param-var-mapping assoc [client-name client-keypath] req-client-kp))
        _ (when vars? (swap! param-var-crosswalk assoc-in [client-name flow-key-orig] [flow-key [client-name client-keypath]])) ;; all the precomputed values to delete the rest. ugh. mistake.
        ;;         ^^ flow-key as the CLIENT understands it.... client-keypath as the server understands it.... ^^
        _ (when vars? (swap! param-var-key-mapping assoc client-name (vec (distinct (conj (get @param-var-key-mapping client-name []) [flow-key-orig flow-key])))))


        ;; keypath-wildcards-map {"*client-name*" client-name
        ;;                        :*client-name* client-name}
        ;; keypath (walk/postwalk-replace keypath-wildcards-map keypath) ;; keypath wildcards... but sub still ref'd flow-key as wildcard for client in question
        ;; sub-path (walk/postwalk-replace keypath-wildcards-map sub-path)

        lv (get @last-values keypath)]

      ;(ut/pp [:client-sub! base-type flow-id :step-id step-id :client-name client-name :last-val lv :flow-key flow-key :keypath keypath :client-param-path client-param-path])
      ;(ut/pp [:client-sub! base-type flow-id :keypath keypath :client-param-path client-param-path])

    (ut/pp [:client-sub! (if signal? :signal! :regular!) client-name :wants base-type client-param-path keypath {:sub-path sub-path} flow-key])

    (when (get-in @flow-db/results-atom keypath)
      (ut/pp [:react (get-in @flow-db/results-atom keypath)]))

    ;; (if signal?
    ;;   (add-watcher keypath client-name send-signal flow-key :param-sub)
    ;;   (add-watcher keypath client-name send-reaction flow-key :param-sub))

    (add-watcher keypath client-name send-reaction flow-key :param-sub)

        ;; (add-watcher keypath client-name send-reaction flow-key :param-sub)

    (when (not signal?)
      (kick client-name [base-type client-param-path]
            (cond (cstr/includes? (str flow-key) "*running?") false
                  (= base-type :time)   (get @time-atom client-param-path)
                  (= base-type :signal) (get @last-signals-atom client-param-path)
                  (= base-type :solver) (get-in @last-solvers-atom (vec (into [(keyword (second sub-path))] (vec (rest (rest sub-path))))) lv) ;; (get @last-solvers-atom client-param-path)
                  (= base-type :solver-meta) (get-in @last-solvers-atom-meta (vec (into [(keyword (second sub-path))] (vec (rest (rest sub-path))))) lv)
                  (= base-type :server) (get @server-atom client-param-path lv)
                  (= base-type :screen) (get-in @screens-atom (vec (rest sub-path)) lv)
                  (= base-type :client) (get-in @params-atom (vec (into [(keyword (second sub-path))] (vec (rest (rest sub-path))))) lv)
                  (= base-type :panel) (get-in @panels-atom (vec (into [(keyword (second sub-path))] (vec (rest (rest sub-path))))) lv)
                  :else (get-in @flow-db/results-atom keypath lv) ;; assume flow
                    ;; ^^ SHOULD be the last val (persistently cached), on cold boot @results-atom will be empty anyways
                  )nil nil nil))

      ;; push init val, else try cache, else nil!

    [:client-sub-request flow-id :step-id step-id :client-name client-name]))

;; :client/fair-salmon-hawk-hailing-from-malpais>click-param>param>jessica

(defmethod wl/handle-request :sub-to-flow-value [{:keys [client-name flow-key]}]
  ;;  (ut/pp [:lookup-flow-value client-name :> flow-key]) ;; set callback to kick updates? "subscribe" to this value?
  ;; monitor changes to value and kick updates to client-name so it can add to click-param
  ;; should be pushed by a scheudled flow?
  ;; need to revisit this. might not work as scale. use sep atoms per flow perhaps, or something like javelin's cells? except on CLJ
  ;; or if we wanted to skip real-time reactions, we could just run it every X seconds and push the result to the client
  ;; OR a watcher that updates OTHER atoms based on results-atom changes, and then we can just watch those atoms instead... [big brain]
  ;; (ut/pp [:sub-to-value flow-key client-name :********])
  (doall (sub-to-value client-name flow-key)))




;; (defmethod wl/handle-request :sub-to-running-values [{:keys [client-name flow-keys]}]
;;   ;; get a list of all block to listen to for the running session. remove all these watchers on session end
;;   (vec (for [keypath flow-keys]
;;          (let [[flow-id step-id] keypath]
;;            (ut/pp [:client-sub-flow-runner flow-id :step-id step-id :client-name client-name])
;;            ;(ut/pp [:react-flow-runner (get-in @flow-db/results-atom keypath)])
;;            (add-watcher keypath client-name send-reaction-runner (keyword (str "runner||" flow-id "||" (hash keypath))) :flow-runner flow-id)
;;            (add-watcher keypath client-name send-tracker-runner (keyword (str "tracker||" flow-id "||" (hash keypath))) :tracker flow-id)
;;            (kick client-name (vec (cons :flow-runner keypath)) :started nil nil nil) ;; push initial value, if we have one
;;            ;(kick client-name (vec (cons :tracker keypath)) (get @flow-db/tracker flow-id) nil nil nil) ;; push init tracker state, if exists
;;            [:client-sub-request flow-id :step-id step-id :client-name client-name]))))

(defn remove-watchers-for-flow22 [flow-id & [client-name]]
  (doseq [[c-name subs] @atoms-and-watchers
          :let [matching-subs (filter #(= (:flow-id %) flow-id) (vals subs))]
          :when (not (empty? matching-subs))
                ;(and
                ; (not (empty? matching-subs))
                ; (if client-name (= c-name client-name) true))
          ]
    (ut/pp [:matching-subs c-name matching-subs])
    (doseq [sub matching-subs]
      (do
        (ut/pp [:removing (count matching-subs) :watchers :for flow-id c-name [[(:keypath sub) c-name (:sub-type sub) flow-id (:flow-key sub)]]])
        (remove-watcher (:keypath sub) c-name (:sub-type sub) flow-id (:flow-key sub))))))





(defn reload-signals-subs []
  (let [parts (vec (for [[signal-name {:keys [signal]}] @signals-atom]
                     [signal-name (vec (filter #(and (keyword? %) (cstr/includes? (str %) "/")) ;; get all valid reolvable compound keywords in the signal :where def
                                               (ut/deep-flatten signal)))]))
        curr-keyparts (vec (keys (get @atoms-and-watchers :rvbbit-signals)))
        all-keyparts (vec (distinct (ut/deep-flatten (map last parts))))
        to-remove (vec (cset/difference (set curr-keyparts) (set all-keyparts)))]

    (reset! signal-parts-atom parts) ;; faster than running it every time - since we need it for reaction lookups in send-reaction, + is ONLY reset when rules are modified

    (ut/pp [:reload-signals-subs! parts curr-keyparts all-keyparts {:remove! to-remove}])

    (doseq [rm to-remove] ;; clean up ones we dont need to watch anymore
      (let [{:keys [sub-type keypath flow-key]} (get-in @atoms-and-watchers [:rvbbit-signals rm])]
        (remove-watcher keypath :rvbbit-signals sub-type nil flow-key)))

    (doseq [kk all-keyparts] ;; re-add them all - TODO, only add new ones (however, they get removed before they get added, so it's a nil regardless)
      (ut/pp [:signal-sub-to kk])
      (sub-to-value :rvbbit-signals kk true))

    (reset! last-signals-atom (select-keys @last-signals-atom (vec (filter #(not (cstr/includes? (str %) "/part-")) (keys @last-signals-atom))))) ;; clear cache of parts, since their indexes might change

    (doseq [signal (keys @signals-atom)] ;; (re)process everythiung since we just got updated
      (ut/pp [:re-processing-signal signal])
      (process-signal signal))))


(defn process-solver [signal-name]
  (let [signals-map (select-keys @signals-atom [signal-name])
        signals-parts-map (into {} (for [[k {:keys [signal]}] signals-map] {k (vec (distinct (ut/where-dissect signal)))}))
        resolve-changed-fn (fn [obody sigk] (let [kps       (ut/extract-patterns obody :changed? 2)
                                                  logic-kps (into {} (for [v kps]
                                                                       (let [[_ & ss] v
                                                                               ;[[flow-id bid value alert?]] this
                                                                             ss (first ss)
                                                                               ;kp (get-in @atoms-and-watchers [:rvbbit-signals ss :keypath])
                                                                             v2 (get-in @last-signal-value-atom [sigk ss]) ;; should be the last time we evaluated this in THIS context.. ;; (get @last-values kp)
                                                                             new [:not [:= v2 ss]] ;; has it changed from the last time WE (context is important) looked at it?
                                                                               ;;_ (ut/pp [:resolve-changed-fn ss kp kps obody new])
                                                                             ]
                                                                         {v new}))) ;; compare against the last LITERAL value, not it's truthiness
                                                  ]
                                              (walk/postwalk-replace logic-kps obody)))
        signals-resolve-map (into {} (for [[k {:keys [signal]}] signals-map]
                                       {k (let [cmpkeys (vec (filter #(and (keyword? %) (cstr/includes? (str %) "/")) (ut/deep-flatten signal)))
                                                vvals (select-keys (get-in @atoms-and-watchers [:rvbbit-signals]) cmpkeys)]
                                            (into {} (for [[k {:keys [base-type sub-type sub-path keypath]}] vvals
                                                           :let [;;_ (when (not= base-type :time) (ut/pp [:resolver-map!!? k base-type sub-type sub-path keypath]))
                                                                 ;;v3 (get @last-values keypath) ;; we dont want last-values we want the value NOW!
                                                                 v3 (get-in @(get-atom-from-keys base-type sub-type sub-path keypath) keypath (get @last-values keypath))
                                                                        ;_ (swap! last-signal-value-atom assoc k v3)
]] ;; extra cache for last value due to diff update cadence on last-values... TODO
                                                       {k v3})) ;; keypath is used as the actual key here, btw
                                            )}))
        nowah (System/currentTimeMillis) ;; want to have a consistent timestamp for this reaction event
        signals-resolved (into {} (for [[k v] signals-map]
                                    {k (walk/postwalk-replace (get signals-resolve-map k) (resolve-changed-fn (get v :signal) k))}))
        parts-work (into {} ;; important side effects / will transition to a doseq after debugging phase
                         (for [[kk vv] signals-parts-map]
                           {kk (into {} (for [vvv vv
                                              :let [rvv (walk/postwalk-replace (get signals-resolve-map kk) (resolve-changed-fn vvv kk))
                                                    honey-sql-str (to-sql {:select [[1 :vv]] :where rvv})
                                                           ;; need to calculate last-value from atom compared to new/curr-value to resolve [:changed? :val]
                                                    result (true? (ut/ne? (sql-query ghost-db honey-sql-str [:ghost-signal-resolve kk])))
                                                    part-key (keyword (str "part-" (cstr/replace (str kk) ":" "") "-" (ut/index-of vv vvv)))
                                                    ;;_ (ut/pp [:part-key kk part-key vvv])

                                                    _ (when true ;(not= result (get @last-signals-atom part-key))  ;; no need for dupe histories
                                                        (swap! last-signals-history-atom assoc-in [kk part-key]
                                                               (into (vec (take-last 12 (sort-by second (get-in @last-signals-history-atom [kk part-key])))) [[result nowah]])))
                                                    _ (when true ;(not= result (get @last-signals-atom vvv)) ;; no need for dupe histories
                                                        (swap! last-signals-history-atom assoc-in [kk vvv]
                                                               (into (vec (take-last 12 (sort-by second (get-in @last-signals-history-atom [kk vvv])))) [[result nowah]])))
                                                    _ (swap! last-signals-atom-stamp assoc vvv nowah)
                                                    _ (swap! last-signals-atom assoc vvv result)
                                                    _ (swap! last-signals-atom assoc part-key result)]] ;; last time we resolved this signal, will be used to stop re-runs on the same signal

                                          {[vvv rvv] result}))}))
        full-parts-work (into {} ;; important side effects / will transition to a doseq after debugging phase
                              (for [[kk vv] signals-resolved
                                    :let [honey-sql-str (to-sql {:select [[1 :vv]] :where vv})
                                          result (true? (ut/ne? (sql-query ghost-db honey-sql-str [:ghost-signal-resolve kk])))

                                          _ (when true ;(not= result (get @last-signals-atom kk))  ;; no need for dupe histories
                                              (swap! last-signals-history-atom assoc-in [kk kk]
                                                     (into (vec (take-last 19 (sort-by second (get-in @last-signals-history-atom [kk kk])))) [[result nowah]])))
                                          _ (swap! last-signals-atom assoc kk result)
                                          _ (swap! last-signals-atom-stamp assoc kk nowah)]]
                                {[kk vv] result}))]
    (doseq [[k v] signals-resolve-map]
      (doseq [[kk vv] v]
        (swap! last-signal-value-atom assoc-in [k kk] vv))) ;; when all done, set "last-value" so future :changed statements work - has to be PER signal, else we miss some                                
    ))


(defn reload-solver-subs []
  (let [parts (vec (for [[solver-name {:keys [signal]}] @solvers-atom]
                     [solver-name (vec (filter #(and (keyword? %) (cstr/includes? (str %) "/")) ;; get all valid reolvable compound keywords in the signal :where def
                                               (ut/deep-flatten [signal])))])) ;; enclose in case its just a single keyword, which it should be...?
        curr-keyparts (vec (keys (get @atoms-and-watchers :rvbbit-signals)))
        all-keyparts (vec (distinct (ut/deep-flatten (map last parts))))
        to-remove (vec (filter #(cstr/starts-with? (str %) ":solver/") (cset/difference (set curr-keyparts) (set all-keyparts))))]

    (ut/pp [:reload-solver-subs! {:parts parts :curr-keypaths curr-keyparts :all-keyparts all-keyparts :to-remove to-remove}])

    ;; (doseq [rm to-remove] ;; clean up ones we dont need to watch anymore - dont do this here since we are dealing with solvers only ?
    ;;   (let [{:keys [sub-type keypath flow-key]} (get-in @atoms-and-watchers [:rvbbit-signals rm])]
    ;;     (remove-watcher keypath :rvbbit-signals sub-type nil flow-key)))

    (doseq [kk all-keyparts]
      (ut/pp [:solver-sub-to kk])
      (sub-to-value :rvbbit-signals kk true))

    (doseq [signal (keys @solvers-atom)] ;; (re)process everythiung since we just got updated
      (ut/pp [:re-processing-solver signal]) ;; as if it was brand new... like an initial sub push
      ;(process-signal signal)
      )))



(defn gen-flow-keys [flow-id client-name]
  (let [ppath (if (cstr/ends-with? (cstr/lower-case flow-id) ".edn")
                flow-id ;; already a file path
                (str "./flows/" flow-id ".edn"))
        raw (try (edn/read-string (slurp ppath))
                 (catch Exception _ (do (ut/pp [:error-reading-flow-from-disk-gen-flow-keys flow-id client-name])
                                        {})))
        ;;raw (get-in @) ; latest-flow-image-map atom pull to get NON saved flow data.. ???
        flowmaps (process-flow-map (get raw :flowmaps))
        connections (get raw :flowmaps-connections)
        server-flowmap (process-flowmap2 flowmaps connections flow-id)
        ;flowmap @(re-frame/subscribe [::flowmap])
        ;flowmaps-connections @(re-frame/subscribe [::flowmap-connections])
        ;client-name @(re-frame/subscribe [::conn/client-name])
        ;server-flowmap (process-flowmap2 flowmap flowmaps-connections fid)
        comps (get server-flowmap :components) ;; (assoc (get server-flowmap :components) :*running? {})
        running-view-subs (vec (for [[k v] comps :when (get v :view)]
                                 [flow-id (keyword (str (cstr/replace (str k) ":" "") "-vw"))]))
        running-subs (vec (for [k (keys comps)] [flow-id k]))
        running-subs (vec (into running-subs running-view-subs))]
    running-subs))

(defmethod wl/handle-request :sub-to-running-values [{:keys [client-name flow-keys flow-id]}]
  ;; get a list of all block to listen to for the running session. remove all these watchers on session end
  (ut/pp [:sub-to-running-values client-name flow-keys])
  (let [flow-keys (if (empty? flow-keys) (gen-flow-keys flow-id client-name) flow-keys)] ;; jumping in for flows were we have no component context yet...
    (doseq [keypath flow-keys]
      (let [[flow-id step-id] keypath]
        (ut/pp [:client-sub-flow-runner flow-id :step-id step-id :client-name client-name])
           ;(ut/pp [:react-flow-runner (get-in @flow-db/results-atom keypath)])
        (add-watcher keypath client-name send-reaction-runner       (keyword (str "runner||" flow-id "||" (hash keypath))) :flow-runner flow-id)
        (add-watcher keypath client-name send-tracker-runner        (keyword (str "tracker||" flow-id "||" (hash keypath))) :tracker flow-id)
        (add-watcher keypath client-name send-tracker-block-runner  (keyword (str "blocks||" flow-id "||" (hash keypath))) :tracker flow-id)
        (add-watcher keypath client-name send-acc-tracker-runner    (keyword (str "acc-tracker||" flow-id "||" (hash keypath))) :tracker flow-id)
           ;(kick client-name (vec (cons :flow-runner keypath)) :started nil nil nil) ;; push initial value, if we have one
           ;(kick client-name (vec (cons :tracker keypath)) (get @flow-db/tracker flow-id) nil nil nil) ;; push init tracker state, if exists
           ;[:client-sub-request flow-id :step-id step-id :client-name client-name]
        ;;(boomerang-client-subs client-name)
        ))
    (boomerang-client-subs client-name))
  [:copy-that client-name])




(defmethod wl/handle-request :open-ai-push [{:keys [kind convo panels client-name]}]
  ;(ext/write-panels client-name panels)
  (doall
   (let [resp (assistants/chat convo)
        ;;  recos-csv (sql-query system-db (to-sql {:select [:combo_edn
        ;;                                                   :combo_hash
        ;;                                                   :connection_id
        ;;                                                   :context_hash
        ;;                                                   :key_hashes
        ;;                                                   :key_hashes_hash
        ;;                                                   :query_map
        ;;                                                   :shape_name
        ;;                                                   :table_name
        ;;                                                   :viz_map]
        ;;                                          :from   [[:combos
        ;;                                                    :gg313]]}))
        ;;  meta-csv (sql-query system-db (to-sql {:select [:connection_id
        ;;                                                  :context_hash
        ;;                                                  :data_type
        ;;                                                  :db_catalog
        ;;                                                  :db_schema
        ;;                                                  :db_type
        ;;                                                  :derived_calc
        ;;                                                  :derived_name
        ;;                                                  :field_name
        ;;                                                  :field_type
        ;;                                                  :is_group_by
        ;;                                                  :key_hash
        ;;                                                  :table_name
        ;;                                                  :table_type]
        ;;                                         :from   [[:fields
        ;;                                                   :aa846]]}))
         ]
     (do ;(ut/write-csv recos-csv "./recos.csv")
         ;(ut/write-csv meta-csv "./meta.csv")
         ;(ut/write-json panels "./canvas.json")
       (ut/pp [:chat-resp resp])
       {:convo resp :client-name client-name})))
  ;(ut/pp [:received-panels-data-from client-name])
  )




;;    ;;; mostly works well
;;    (defmethod wl/handle-subscription :server-push2 [{:keys [kind ui-keypath client-name]}]
;;      (let [results (async/chan 4)]
;;        (ut/ppln [:subbed kind client-name])
;;        (async/pipeline 4 results (filter #(= (get % :client-name) client-name)) external-changes)
;;        (async/thread
;;          (loop []
;;            (async/<!! (async/timeout 100))
;;            (when
;;             ;(async/>!! results (async/<!! external-changes))
;;             (async/<!! results)
;; ;             (async/pipeline 2 results (filter #(= (get % :client-name) client-name)) external-changes)
;;              (recur))))
;;        results))

;(defonce sql-cache (atom {}))


(def sql-cache (atom (cache/lru-cache-factory {} :threshold 1000)))

(defn lookup-cache-exists? [key]
  (cache/has? @sql-cache key))

(defn get-from-cache [key]
  (cache/lookup @sql-cache key nil))

(defn insert-into-cache [key value]
  (swap! sql-cache assoc key value))

(defonce conn-map (atom {}))

(defn get-connection-string [connection-id]
  (let [;conn (sql-query-one system-db (to-sql {:select [:original_connection_str] :from [:connections] :where [:= :connection_id connection-id]}))
        ;[;f-path (str prefix f)
         ;conn-name (str (cstr/replace (cstr/lower-case (last (cstr/split f-path #"/"))) ".edn" ""))
        ;;  conn (edn/read-string conn)
        ;;  conn (if (try (and (map? conn)
        ;;                     (find conn :jdbc-url))
        ;;                (catch Exception _ false))
        ;;         {:datasource @(pool-create conn (str connection-id "-pool"))}
        ;;         conn)
       ; conn (edn/read-string conn)
        conn (get @conn-map connection-id)]
    ;(ut/pp [:incoming-conn conn])

    ;; (if (try (and (map? conn)
    ;;               (find conn :jdbc-url))
    ;;          (catch Exception _ false)) ;; create hikari pool based connection?
    ;;   ;(cstr/includes? conn ":jdbc-url")
    ;;   (if (nil? (get @conn-map connection-id))
    ;;     (do (swap! conn-map assoc connection-id {:datasource @(pool-create conn (str connection-id "-pool"))})
    ;;       {:datasource @(pool-create conn (str connection-id "-pool"))})
    ;;     (get @conn-map connection-id)
    ;;     )
    ;;   (if (cstr/starts-with? (str conn) "jdbc:") ;; or no connection pool, old-school conn
    ;;     (str conn)
    ;;     ;(read-string conn)
    ;;     conn
    ;;     ))
    conn))

(defn data-type-value [v]
  (cond (or (cstr/includes? (str (type v)) "DateTime")
            (cstr/includes? (str (type v)) "TimeStamp")) "datetime"
        (cstr/includes? (str (type v)) "Date") "date"
        (or (and (cstr/starts-with? (str v) "@@") (string? v)) (vector? v) (map? v)) "rabbit-code"
        (string? v) "string"
        (integer? v) "integer"
        (float? v) "float"
        (boolean? v) "boolean"
        :else "unknown"))

(defn get-query-metadata [rowset query]
  ;(ut/pp [:get-meta rowset])
  (let [sample rowset ;(repeatedly 100 (fn [] (rand-nth rowset)))
        sample-size (count sample)
       ;actual? (not (= sample-size 100))
        selects (get query :select)
        group-bys (get query :group-by)
        no-group-by? (and (empty? group-bys)
                          (or (= selects [:*])
                              (and (not (= 1 (count selects)))
                                   (or (not (some #(= % :count) (flatten selects)))
                                       (not (some #(= % :sum) (flatten selects)))
                                       (not (some #(= % :min) (flatten selects)))
                                       (not (some #(= % :max) (flatten selects)))))))
        selects-non-aliased (into {} (for [s selects] (if (vector? s) {(first s) (last s)} {s s})))
        selects-only-aliased (vec (for [s selects] (if (vector? s) (last s) s)))
        materialize-group-bys (vec (for [g group-bys] (cond (integer? g) (get selects-only-aliased (- g 1))
                                                            (vector? g) (get selects-non-aliased g)
                                                            :else g)))
        fields (keys (first sample))
        field-data (into {} (for [f fields] {f (let [fsamples (map f sample)
                                                     distinct-samples (count (distinct fsamples))
                                                     commons (into {} (take 3 (reverse (sort-by last (frequencies fsamples)))))
                                                     data-type (get-in (vec (reverse (sort-by last (frequencies (map data-type-value fsamples))))) [0 0])]
                                                 {:data-type data-type
                                                  :distinct distinct-samples
                                                  :group-by? (if (not no-group-by?)
                                                               (true? (try (some #(= f %) materialize-group-bys) (catch Exception _ false))) true)
                                                  :commons (if (cstr/includes? data-type "date")
                                                             (into {} (for [[k v] commons] {(str k) v}))
                                                             commons)
                                                  :cardinality (int (* 100 (float (/ distinct-samples sample-size))))})}))]
    {:fields field-data
     :rowcount (count sample)}))

;; (defmethod wl/handle-subscription :server-push [{:keys [kind ui-keypath extras]}]
;;   ;(ut/ppln {:subscription-in [kind ui-keypathextras]})
;;   (async/go-loop []
;;     (async/<!!
;;      (async/timeout 50000)
;;      ;external-changes
;; ) ;(recur)
;;     )
;;   external-changes)

;;  (defmethod wl/handle-subscription :server-push2 [{:keys [kind ui-keypath extras]}]
;;    ;(ut/ppln {:subscription-in [kind ui-keypathextras]})
;;    (async/go-loop []
;;      (async/<!!
;;       ;(async/timeout 50000)
;;       external-changes
;;  ) ;(recur)
;;      )
;;    external-changes)

;(defmethod wl/handle-subscription :server-push2 [x]
;  (ut/ppln x)
;  external-changes)

;; (defmethod wl/handle-subscription :server-push0 [data]
;;       ;(async/>!! external-changes {:result 1})
;;       ;(async/>!! external-changes {:result 2})
;;       ;(future (Thread/sleep 500) (async/<! (async/timeout 99999999)))

;;   (async/go-loop [seconds 1]
;;     (async/<! (async/timeout 1000))
;;         ;(print "waited" seconds "seconds")
;;     external-changes
;;     (recur (inc seconds)))

;;   external-changes)

    ;; (defmethod wl/handle-subscription :server-push2 [x]
    ;;   (async/go-loop []
    ;;     (async/<!! external-changes ;(async/timeout 100)
    ;;                )
    ;;             ; external-changes
    ;;                  )
    ;;   external-changes)

;(defonce sql-cache (atom {}))

(def per-page-limit 200) ;; for sending to the UI, never allow more than this in each chunk sent (unless overridden by front end)

(defmethod wl/handle-request :honey-call [{:keys [kind ui-keypath honey-sql client-name]}]
  (swap! q-calls2 inc)
  (inc-score! client-name :push)
  ;; (ut/pp [:PUSH-honey-call! (str honey-sql)])
  ;; (ut/pp [kind {:kind kind :ui-keypath ui-keypath :honey-sql honey-sql}])
  ;; (ut/pp [kind ;(not (empty? (ut/extract-patterns honey-sql :pivot-by 2)))
  ;;         @q-calls2 kind ui-keypath :system-db client-name ;; honey-sql ; (get honey-sql :transform-select) :full-honey honey-sql ; :select (get honey-sql :select)
  ;;           ;(when (get honey-sql :transform-select) [:TSELECT!! honey-sql])
  ;;         ])
  ;(time
  ;(inc! running-system-queries)
  ;(time! (timer instrument/metric-registry ["queries" "timings" "system-db"]) ;; test

  (doall
   (let [;req-hash (hash [kind ui-keypath honey-sql client-name])
         cache? false ;(not (nil? (get @sql-cache req-hash)))
         per-page-limit 600 ;; override temp TODO (internal data only)
         ]
          ;  (if cache? (do (ut/ppln {:cached? cache? :kind kind :ui-keypath ui-keypath :honey-sql honey-sql})
          ;                 (get @sql-cache req-hash))

     (try
       (let [;honey-sql (dissoc honey-sql :last-known-fields)
             page-num (get honey-sql :page)
             honey-sql (if page-num (assoc (dissoc honey-sql :page) :offset (* page-num per-page-limit)) honey-sql)
             honey-sql-str (to-sql honey-sql)
             honey-result (sql-query system-db honey-sql-str ui-keypath)
             honey-meta (get-query-metadata honey-result honey-sql)
             fields (get honey-meta :fields)
             dates (remove nil? (for [[k v] fields] (when (cstr/includes? (get v :data-type) "date") k)))
             result (if (empty? dates) (vec (take per-page-limit honey-result))
                        (vec (for [r (take per-page-limit honey-result)]
                               (into {} (for [[k v] r]
                                          (if (some #(= % k) dates)
                                            {k (str v)}
                                            {k v}))))))
          ;result (vec (take 500 honey-result))
             output {:kind kind :ui-keypath ui-keypath :result result :result-meta honey-meta
                     :client-name client-name :map-order (get @sql/map-orders honey-sql-str)}]
            ;(dec! running-system-queries)
         (ut/ppln {:cached? cache? :kind kind :rows (count result) :ui-keypath ui-keypath :honey-sql honey-sql :client-name client-name})
    ;(ut/ppln {kind {:kind kind :ui-keypath ui-keypath :extras extras :honey-sql honey-sql :honey-result honey-result}})
    ;{:kind kind :stuff :you-return :recieved? true :rand-int rr :extras extras :ui-keypath ui-keypath :honey-result honey-result}
      ;; dump into in-memory db table with query name... NON-BLOCKING thread/taskpool
         (do ;; (swap! sql-cache assoc req-hash output)
           output))
       (catch Exception e (ut/ppln [:error! e]))))))

;(def task-pool (tp/create-pool "field-processing-pool" 1))

;; (defn sniff-meta [kind ui-keypath honey-sql fields req-hash target-db client-name] ;; server base field meta deep sniff. switched to client calls.
;;   (async/thread
;;     (let [honey-sql (-> honey-sql
;;                         (dissoc :offset) (dissoc :page) (dissoc :limit) (dissoc :cache?))
;;           fields (if (nil? fields) ;; if nil assume is a parent-cached run loop
;;                    (get-in @sql-cache [req-hash :result-meta :fields])
;;                    fields)]
;;       ;(ut/ppln [:ff fields kind ui-keypath honey-sql req-hash])
;;       (dorun
;;        (for [[[name f] hsql] (merge {[:rowcount :*] {:select [[[:count 1] :rowcnt]] :from [[honey-sql :subq]]}
;;                                      ;[:rowcount :*] {:select [[[:count 1] :rowcnt2]] :from [[honey-sql :subq]]} ;; other "rules"
;;                                      }
;;                                     (into {} (for [field (keys fields)]
;;                                                {[:distinct field]
;;                                                 {:select [[[:count [:distinct field]] :distinct-values]] :from [[honey-sql :subq]]}})))]

;;          (let [preq-hash (hash [kind ui-keypath hsql name])
;;                pcache? (not (nil? (get @sql-cache preq-hash)))]
;;            (if pcache? (async/go
;;                          ;(async/<! (async/timeout 2000))
;;                          (async/>! external-changes (let [payload (get @sql-cache preq-hash)]
;;                                                       ;(async/<! (async/timeout 1800))
;;                                                       (do (ut/ppln [:post-meta-cache req-hash preq-hash payload])
;;                                                           (merge payload
;;                                                                  {:client-name client-name})))))

;;                (async/go
;;                  ;(async/<! (async/timeout 2000))
;;                  (async/>! external-changes
;;                            (let [str-sql (to-sql hsql)
;;                                  sql-result (first (vals (first (sql-query target-db str-sql [ui-keypath :post-meta]))))
;;                                  payload {:ui-keypath ui-keypath
;;                                           :name name
;;                                           ;:client-name client-name
;;                                           :field f
;;                                           :data sql-result}]
;;                              (ut/ppln [:post-meta req-hash preq-hash payload]) ;; debug
;;                              ;(async/<! (async/timeout 1800))
;;                              (do (swap! sql-cache assoc preq-hash payload)
;;                                  (merge payload
;;                                         {:client-name client-name}))))))))))))


(defmethod wl/handle-request :selected-reco [{:keys [kind context_hash dm-type drag-meta combo_hash client-name]}]
  (cond (= dm-type :viz-reco)
        (let [combo-row-sql {:select [:*] :from [:combos] :where [:= :combo_hash combo_hash]}
              combo-row (sql-query system-db (to-sql combo-row-sql))
              shape-name (get (first combo-row) :shape_name)
              key-hashes (edn/read-string (get (first combo-row) :key_hashes))
              get-combo-field-sql (fn [k v] {:select [:yy501/connection_id ;:yy501/context_hash
                                                      :yy501/data_type :yy501/db_catalog
                                                      :yy501/db_schema :yy501/db_type
                                                      :yy501/derived_calc :yy501/derived_name
                                                      :yy501/field_name :yy501/field_type
                                                ;:yy501/key_hash :yy501/run_id
                                                ;:yy501/table_name :yy501/table_type
                                                ;:yy501/updated
                                                ;:bb977/shape_name
                                                      :bb977/logic_map :bb977/axes_key]
                                             :from [[:fields :yy501]]
                                             :join
                                             [[{:select [:axes_key :connection_id ;:context_hash
                                                         :db_catalog :db_schema :db_type
                                                         :derived_calc :derived_name :field_name
                                                         :key_hash
                                                         :logic_map ;:run_id
                                                   ;:shape_name :table_name :table_type
                                                   ;:updated
                                                         ]
                                                :from [[:found_fields :xx420]]} :bb977]
                                              [:and
                                               [:= :bb977/key_hash :yy501/key_hash]
                                               [:= v :yy501/key_hash]
                                               [:= :bb977/axes_key k]]]})
              combo-fields-sql {:union-all (vec (for [[k v] key-hashes] (get-combo-field-sql k v)))}]
          (doseq [row (sql-query system-db (to-sql combo-fields-sql))]
            (async/thread ;;; TODO replace with a serial agent send
              (let [lm (edn/read-string (get row :logic_map))
                    fixed-row (into (sorted-map)
                                    (-> (merge lm row)
                                        (dissoc :logic_map)
                                        (dissoc :database_version)
                                        (dissoc :key_hash)
                                        (assoc  :table_name (cstr/join "_" (drop-last (cstr/split (get lm :table_name) #"_"))))
                                        (dissoc :context_hash)
                                        (dissoc :user_name)
                                        (dissoc :table_type)
                                        (dissoc :total_rows)
                                        (assoc :shape_name shape-name)))]
                (swap! em/selected-recos conj fixed-row))))

          (ut/pp [:received-selected-reco kind dm-type context_hash combo_hash client-name])
          {})  ;; return is useless, but wl/handle-push does not work.. :/
        (= dm-type :meta-fields)
        (let [shape-name "basic-group-by"
              combo-fields-sql (walk/postwalk-replace {:target :field_name
                                                       :data-type :data_type
                                                       :source_table :table_name
                                                       :connection-id :connection_id}
                                                      (merge drag-meta {:shape_name shape-name
                                                                        :axes_key "group-by"}))]
          (doseq [row (sql-query system-db (to-sql combo-fields-sql))]
            (async/thread
              (let [lm (edn/read-string (get row :logic_map))
                    fixed-row (into (sorted-map)
                                    (-> (merge lm row)
                                        (dissoc :logic_map)
                                        (dissoc :database_version)
                                        (dissoc :key_hash)
                                        (assoc  :table_name (cstr/join "_" (drop-last (cstr/split (cstr/replace (str (get lm :table_name)) ":" "") #"_"))))
                                        (dissoc :context_hash)
                                        (dissoc :user_name)
                                        (dissoc :table_type)
                                        (dissoc :total_rows)
                                        (assoc :shape_name shape-name)))]
                ;(swap! em/selected-recos conj fixed-row)
                ;(ut/pp fixed-row)
                )))

          (ut/pp [:received-selected-reco kind dm-type context_hash combo_hash client-name])
          {}))) ;; return is useless, but wl/handle-push does not work.. :/

(def pivot-cache (atom {}))
;;(def rql-holder (atom {}))

(defn extract-rql
  [ui-keypath obody rql-holder]
  (let [;kps (into (ut/extract-patterns obody :*code* 1)
        ;          (ut/extract-patterns obody :*render* 1))
        kps (vals (filter #(let [[_ v] %] (or (cstr/starts-with? (str v) "[:*code")
                                              (cstr/starts-with? (str v) "[:*read-edn")
                                              (cstr/starts-with? (str v) "[:*render")))
                          (into {} (for [kp (ut/kvpaths obody)] {kp (get-in obody kp)}))))
        logic-kps
        (into {}
              (for [v kps]
                (let [;[_ l this] v
                      rql-key (str "@@r" (rand-int 123455))]
                  (swap! rql-holder assoc-in
                         (conj ui-keypath rql-key)
                         v)
                  {v rql-key})))]
    (walk/postwalk-replace logic-kps obody)))


(defonce pre-sql-cache (ut/thaw-atom [] "./data/atoms/pre-sql-cache.edn")) ;; no need to persist for now...

(defn replace-pre-sql [honey-sql]
  (let [if-walk-map2           (fn [query] (let [kps       (ut/extract-patterns query :*if 4) ;(kv-map-fn obody) ;(into {} (for [p (ut/kvpaths obody)] {p (get-in obody p)}))
                                                 logic-kps (into {} (for [v kps]
                                                                      (let [[_ l this that] v]
                                                                        {v (if (not (or (empty? l) (nil? l))) this that)})))]
                                             (walk/postwalk-replace logic-kps query)))
        =-walk-map2            (fn [query] (let [kps       (ut/extract-patterns query :*= 3)
                                                 logic-kps (into {} (for [v kps]
                                                                      (let [[_ that this] v]
                                                                        {v (= (str that) (str this))})))]
                                             (walk/postwalk-replace logic-kps query)))
        when-walk-map2            (fn [query] (let [kps       (ut/extract-patterns query :*when 3)
                                                    logic-kps (into {} (for [v kps]
                                                                         (let [[_ that this] v]
                                                                           {v (when (or (true? that) (ut/ne? that)) this)})))]
                                                (walk/postwalk-replace logic-kps query)))
        fix-nil-ins            (fn [query] (let [kps       (ut/extract-patterns query :in 3)
                                                 logic-kps (into {} (for [v kps
                                                                          :when (nil? (last v))]
                                                                      (let [[_ that this] v]
                                                                        {v nil})))]
                                             (walk/postwalk-replace logic-kps query)))
        all=-map2-inc           (fn [query] (let [kps       (ut/extract-patterns query :*all= 3)
                                                  logic-kps (into {} (for [v kps]
                                                                       (let [[_ fmap incvec] v]
                                                                         {v (vec (conj (for [[k v] (select-keys fmap incvec)
                                                                                             :let [in? (vector? v)]]
                                                                                         (if in? [:in k v] [:= k v])) :and))})))]
                                              (walk/postwalk-replace logic-kps query)))
        all=-map2                (fn [query] (let [kps       (ut/extract-patterns query :*all= 2)
                                                   logic-kps (into {} (for [v kps]
                                                                        (let [[_ fmap] v]
                                                                          {v (vec (conj (for [[k v] fmap ;(select-keys fmap (get fmap :last-known-fields))
                                                                                              :let [in? (vector? v)]]
                                                                                          (if in? [:in k v] [:= k v])) :and))})))]
                                               (walk/postwalk-replace logic-kps query)))
        pre-sql-out (-> honey-sql
                        =-walk-map2
                        when-walk-map2
                        if-walk-map2
                        all=-map2
                        all=-map2-inc
                        fix-nil-ins
                        ut/deep-remove-nil-values)
        pre-sql-out (ut/lists-to-vectors pre-sql-out)]
    (reset! pre-sql-cache (vec (distinct (conj @pre-sql-cache pre-sql-out))))
    pre-sql-out))

;(def fj-pool (Executors/newWorkStealingPool 10))
;(set-agent-send-off-executor! fj-pool)
;(set-agent-send-executor! fj-pool)

;; (def task-queue (java.util.concurrent.LinkedBlockingQueue.))
;; (def running (atom true))

;; (defn start-thread-worker []
;;   (let [worker-thread (Thread. #(loop []
;;                                   (when @running
;;                                     (try
;;                                       (let [task (.take task-queue)]  ; task is a zero-argument function
;;                                         (task))  ; call the task function
;;                                       (catch Exception e
;;                                         ;; Log the exception and continue processing
;;                                         (println "Error executing task:" e)))
;;                                     (recur))))]
;;     (.start worker-thread)
;;     worker-thread))

;; (defn enqueue-task [task]
;;   (.put task-queue task))

;; ;; (defn stop-thread-worker [worker-thread]
;; ;;   (reset! running false)
;; ;;   ;; Join the thread to ensure it has finished before continuing
;; ;;   (.join worker-thread))

;; (defn stop-thread-worker [worker-thread]
;;   (reset! running false)
;;   ;; Wait for the current task to finish
;;   (while (not (.isInterrupted worker-thread))
;;     (Thread/sleep 100))
;;   (.join worker-thread)
;;   ;; Clear the interrupted status if needed
;;   (.interrupted worker-thread))

;; (defn recycle-thread-worker [worker-thread]
;;   (ut/pp [:RECYCLING-SYNC-WORKER-THREAD!!])
;;   ;; Stop the current worker thread
;;   (stop-thread-worker worker-thread)
;;   ;; Reset the running flag
;;   (reset! running true)
;;   ;; Start a new worker thread
;;   (start-thread-worker))





;; (def task-queue (java.util.concurrent.LinkedBlockingQueue.))
;; (def running (atom true))

;; (defn enqueue-task [task]
;;   (.put task-queue task))

;; (defn worker-loop []
;;   (loop []
;;     (when @running
;;       (try
;;         (let [task (.take task-queue)]  ; task is a zero-argument function
;;           (task))  ; call the task function
;;         (catch Exception e
;;           ;; Log the exception and continue processing
;;           (println "Error executing task:" e)))
;;       (recur))))

;; (defn start-thread-worker []
;;   (let [worker-thread (Thread. worker-loop)]
;;     (.start worker-thread)
;;     worker-thread))

;; ;; Initialize the worker thread
;; (def worker-thread (atom (start-thread-worker)))

;; (defn stop-thread-worker [worker-thread]
;;   (reset! running false)
;;   ;; Wait for the current task to complete and the thread to stop.
;;   (.join worker-thread)
;;   ;; After the old thread is fully stopped, clear any interrupted status.
;;   (.interrupted worker-thread))

;; (defn recycle-thread-worker []
;;   ;; Stop the current worker thread and ensure it's fully stopped before starting a new one.
;;   (let [old-thread @worker-thread]
;;     (stop-thread-worker old-thread)
;;     ;; After ensuring the old thread is stopped, start a new worker thread.
;;     (let [new-thread (start-thread-worker)]
;;       (reset! running true) ;; Set running to true only after the old thread has stopped.
;;       new-thread)))

;; (defn recycle-worker []
;;   (let [new-thread (recycle-thread-worker)]
;;     (ut/pp [:thread-being-rebooted...])
;;     (reset! worker-thread new-thread))) ;; Update the atom with the new thread.









;; (def executor (Executors/newSingleThreadExecutor)) ; Creates an ExecutorService that uses a single worker thread
;; (def futures (atom []))

;; (defn enqueue-task [task-fn]
;;   (let [future (.submit executor ^Runnable task-fn)]
;;     (swap! futures conj future)
;;     future))

;; (defn cleanup-futures []
;;   (ut/pp [:futures futures])
;;   (swap! futures #(remove (fn [x] (.isDone x)) %)))

;; (defn shutdown-executor []
;;   (.shutdown executor)
;;   ;; Optional: Wait for a certain time for all tasks to complete
;;   (.awaitTermination executor 60 java.util.concurrent.TimeUnit/SECONDS))

;; (defn recycle-executor-service []
;;   ;; Shutdown the old executor service
;;   (shutdown-executor)
;;   ;; Replace the old executor service with a new one
;;   (reset! executor (Executors/newFixedThreadPool 10))) ;; ?

;; (defn cancel-all-tasks []
;;   (ut/pp ["FUTURES-ATOM" @futures])
;;   (doseq [future @futures]
;;     ;; Attempt to cancel the task; mayInterruptIfRunning is set to true
;;     (.cancel future true))
;;   ;; Clear the futures atom after cancellation
;;   (reset! futures []))


(defn ded-thread [task-fn]
  ;; Define the thread, passing in the task function to be run.
  (let [thread (Thread. (fn []
                          (try
                            ;; Run the provided function.
                            (task-fn)
                            (catch Exception e
                              ;; Handle any exceptions thrown by the task.
                              (println "Exception in thread:" (.getMessage e)))
                            (finally
                              ;; This will run after the task is complete or if an exception is thrown.
                              ;; If there are any resources to clean up, do it here.
                              ))))]
    ;; Start the thread.
    (.start thread)
    ;; Optionally join the thread if you want to wait for it to finish.
    ;; (.join thread)
    ))



;; (def task-queue (java.util.concurrent.LinkedBlockingQueue.))
;; (def running (atom true))
;; (def worker (atom nil)) ; Holds the future of the worker thread

;; (defn enqueue-task [task]
;;   (.put task-queue task))

;; (defn worker-loop []
;;   (loop []
;;     (when @running
;;       (let [task (.take task-queue)]
;;         (ded-thread2 task))))
;;   (recur))

;; (defn start-worker []
;;   (ut/pp [:STARTING-WORKER-THREAD!!])
;;   (reset! running true)
;;   (reset! worker (future (worker-loop))))

;; (defn stop-worker []
;;   (reset! running false)
;;   (when-let [w @worker]
;;     (future-cancel w)
;;     (while (not (.isDone w)) ; Ensure the future is cancelled before proceeding
;;       (Thread/sleep 60))))

;; (defn recycle-worker []
;;   (ut/pp [:REBOOTING-WORKER-THREAD!!])
;;   (System/gc) ;;; hehe, fuck it
;;   (stop-worker)
;;   (start-worker))


;; (def task-queue (atom []))
;; ;(def semaphore (java.util.concurrent.Semaphore. 1))

;; (defn execute-next-task []
;;   ;; Acquire a permit to run a task
;;   (.acquire cruiser/semaphore)
;;   (when-let [task-fn (first @task-queue)]
;;     ;; Remove the task from the queue
;;     (swap! task-queue rest)
;;     ;; Run the task in a separate thread
;;     (ded-thread
;;      (fn []
;;        (try
;;          (task-fn)
;;          (catch Exception e
;;            (println "Exception during task:" (.getMessage e)))
;;          (finally
;;             ;; Release the permit and execute the next task in the queue
;;            (.release cruiser/semaphore)
;;             ;; Call this function again to check for more tasks
;;            (future (execute-next-task))))))))

;; (defn enqueue-task [task-fn]
;;   ;; Add the task to the queue
;;   (swap! task-queue conj task-fn)
;;   ;; Try to execute the next task
;;   (execute-next-task))
















;; Start the recycling process based on your timer mechanism
;; (lunchbreak ...) as you've defined it


;; (enqueue-task (fn [] ... ) )
;;(def completion-channel-client (async/chan)) ;; used for data literals table insertion chicken/egg issue
;(async/>! completion-channel true)

;(def completion-channel (async/chan)) ;; used for data literals table insertion chicken/egg issue


(def execution-channel (chan 100))

(go (loop []
      (when-let [{:keys [run-fn result-chan]} (<! execution-channel)]
        (try
          (let [result (run-fn)]
           ; (println "runstream result:" result) ; Debugging
            (>! result-chan result))
          (catch Exception e
            (println "Error in runstream:" e) ; Error handling
            (>! result-chan {:error e})))
        (recur))))

(defn queue-runstream [runstream-fn]
  (let [result-chan (chan)]
    (println "Queueing runstream") ; Debugging
    (>!! execution-channel {:run-fn runstream-fn :result-chan result-chan})
    result-chan))

;; (if data-call?
;;   (let [async-result-chan (go (let [result-chan (queue-runstream runstream)]
;;                                 (println "Waiting for result...") ; Debugging
;;                                 (<! result-chan)))] ; This returns a channel
;;       ;; To get the result, you need to take from async-result-chan
;;     (println "Getting result from async operation...")
;;     (async/<!! async-result-chan)) ; Blocking take from the channel
;;   (runstream))


;(def sniff-agent1 (agent nil))
;(set-error-mode! sniff-agent1 :continue)

;(def sniff-agent2 (agent nil))
;(set-error-mode! sniff-agent2 :continue)

(defn get-all-from [m]
  (if (map? m)
    (into [] (concat
              (when-let [where (:from m)] [where])
              (mapcat get-all-from (vals m))))
    []))

(defn group-by-field-name [data]
  (into {}
        (for [[table-key records] data]
          [table-key (into {}
                           (for [[field-key grouped-records] (group-by :field_name records)]
                             [(keyword field-key) (first (map #(dissoc % :connection_id :table_name :db_type :field_name) grouped-records))]))])))



(defn get-clover-sql-training [clover-sql honey-sql-str2]
  (try
    (let [clover-sql (ut/deep-remove-keys2 clover-sql [:_last-run :connection-id])
          connection-id (get clover-sql :connection-id)
          data-dict-honey-sql {:select [:db_type :table_name :field_name :connection_id
                                        :field_type :data_type]
                               :where [:and [:= :connection_id connection-id] [:<> :field_name "*"]]
                               :from   [[:fields :ee473as]]}
          sql-str (to-sql data-dict-honey-sql)
          res (sql-query system-db sql-str [:data-dict-for-training-clover-sql])
          group-res (group-by-field-name (group-by (comp keyword :table_name) res))
          tables (filter keyword? (ut/deep-flatten (get-all-from clover-sql)))
          metadata (select-keys group-res tables)
          data-map (merge (get @clover-sql-training-atom clover-sql) ;; in case we have already enriched this query, don't want to lose the data
                          {:sql-string honey-sql-str2
                           :db-type (get-in res [0 :db_type])
                           :clover-sql clover-sql
                           :table-metadata metadata})]
      ;;(ut/pp [:TRAINING-DATA! data-map])
      (swap! clover-sql-training-atom assoc clover-sql data-map))
    (catch Throwable e (ut/pp [:error-in-clover-sql-training-harvest! (str e)]))))


(defn query-runstream [kind ui-keypath honey-sql client-cache? sniff? connection-id client-name page panel-key clover-sql]
  ;; (ut/pp [:honey-sql honey-sql])
  (doall
   (let [;;_ (ut/pp [:honey-sql honey-sql])
         post-process-fn (get honey-sql :post-process-fn) ;; allowed at the top level only - will be discarded elsewhere FOR NOW...
         ;;_ (when post-process-fn (ut/pp [:post-process1! post-process-fn]))
         honey-sql (if (get honey-sql :limit) ;; this is a crap solution since we can't introspect the top level query to get dims and measures, etc... TODO, selective limit pruning
                     {:select [:*] :from [honey-sql]}
                     honey-sql)
         honey-sql (ut/deep-remove-keys honey-sql [:post-process-fn]) ;; disregard if we have nested ones, we cant use them currently since its a SQL subquery...
         has-rql? (try (true? (some #(or (= % :*render*) (= % :*read-edn*) (= % :*code*)) (ut/deep-flatten honey-sql))) (catch Exception _ false))
         data-call? (true? (and (not has-rql?)
                                (ut/ne? (first (filter #(= (last %) :data) (ut/kvpaths honey-sql))))))
         ;; used for simple blocking in code-execution and data literals table insertion chicken/egg issue
         runstream (fn []
                     (try
                       (doall ;;;; ? fixed the socket laziness? (no pun intended)
                        (let [;last-known-fields (get honey-sql :last-known-fields [])
           ;honey-sql (dissoc honey-sql :last-known-fields)
           ;client-cache? (get honey-sql :cache? true)
           ;honey-sql (dissoc honey-sql :cache?)

                              repl-host (get-in honey-sql (or (first (filter #(= (last %) :repl-host) (ut/kvpaths honey-sql))) [:nope]))
                              repl-port (get-in honey-sql (or (first (filter #(= (last %) :repl-port) (ut/kvpaths honey-sql))) [:nope]))
          ;;  _ (ut/pp [:!!!! (first (filter #(= (last %) :repl-host) (ut/kvpaths honey-sql)))
          ;;            (get-in honey-sql [])
          ;;            (first (filter #(= (last %) :repl-port) (ut/kvpaths honey-sql)))])
           ;_ (ut/pp [:PPPP (filter #(= (last %) :repl-host) (ut/kvpaths honey-sql)) (filter #(= (last %) :repl-port) (ut/kvpaths honey-sql))])
                              honey-sql (ut/deep-remove-keys honey-sql [:repl-host :repl-port])
                              _ (when (and repl-host repl-port) (ut/pp [:external-repl! repl-host repl-port]))
                              honey-sql (walk/postwalk-replace {[:*all= {}] nil  ;; take care of empty wherealls
                                                                :*client-name-str (pr-str client-name)
                                                                :*client-name (str client-name)} honey-sql)
                              orig-honey-sql honey-sql ;; for transform later
                              query-meta-subq? (true? (some #(or (= % :query_meta_subq) (= % :query-meta-subq)) (ut/deep-flatten honey-sql)))
                              literal-data? (and (get orig-honey-sql :data) ;false
                                                 (vector? (get orig-honey-sql :data)))
          ; literal-data-output (atom {})
        ;tt (get honey-sql :transform-select)
        ;has-rql? #_{:clj-kondo/ignore [:not-empty?]}
        ;(or (not (empty? (ut/extract-patterns honey-sql :*render* 1)))
        ;    (not (empty? (ut/extract-patterns honey-sql :*code* 1))))
                              rql-holder (atom {})
                             ;has-rql? (try (true? (some #(or (= % :*render*) (= % :*read-edn*) (= % :*code*)) (ut/deep-flatten honey-sql))) (catch Exception _ false))
                              post-sniffed-literal-data? (and (not literal-data?) ;false
                                                              (ut/ne? (filter #(= (last %) :data) (ut/kvpaths honey-sql)))
                                                              (not has-rql?)) ;; <-- look into, clashing with rql
          ; _ (when has-rql? (ut/pp [:has-rql!]))
          ; _ (when literal-data? (ut/pp [:literal-data!]))
          ; _ (when post-sniffed-literal-data? (ut/pp [:nested-literal-data!]))
                              data-literals (or (first (filter #(= (last %) :data) (ut/kvpaths honey-sql))) [:nope])
                              data-literal-code? (false? (when (or literal-data? post-sniffed-literal-data?)
                                                           (let [dl (get-in honey-sql data-literals)]
                                                             (and (vector? dl) (map? (first dl))))))
                              data-literals-data (get-in honey-sql data-literals)
          ; _ (when (not (empty? data-literals)) (ut/pp [:data-literal-code? data-literal-code?]))
          ; _ (when (not (empty? data-literals)) (ut/pp [:data-literal data-literals-data]))
          ; _ (when (not (empty? data-literals)) (ut/pp [:literal-data-map @literal-data-map]))
                              honey-sql (cond post-sniffed-literal-data?
                              ;;  (walk/postwalk-replace {(let [literals-kp (first (filter #(= (last %) :data) (ut/kvpaths honey-sql)))]
                              ;;                            ;(get-in honey-sql [:from 0 0])
                              ;;                            ;(get-in honey-sql literals-kp)
                              ;;                            (get-in honey-sql (vec (drop-last literals-kp)))
                              ;;                            )
                              ;;                          (ut/keypath-munger [(first ui-keypath)]) ;; munged cache table name
                              ;;                         } honey-sql) ;; (ut/keypath-munger ui-keypath)
                                              (walk/postwalk-replace @literal-data-map orig-honey-sql)
                                              literal-data? (get orig-honey-sql :data)
                                              (ut/ne? (get orig-honey-sql :transform-select))
                                              (-> (first (get orig-honey-sql :from))
                               ;(assoc :limit 4)
                                                  (dissoc :limit)
                                                  (assoc :page -1))
                                              :else orig-honey-sql)
         ;  _ (ut/pp [:post-cond honey-sql])
         ;  _ (ut/pp [:post-cond2 (walk/postwalk-replace @literal-data-map honey-sql)])
          ; honey-sql (replace-pre-sql honey-sql)
                              honey-sql (if has-rql? (extract-rql ui-keypath honey-sql rql-holder) honey-sql)

                              honey-sql (replace-pre-sql honey-sql) ;; runs a subset of clover replacements that might have ended up in SQL / based on clover params being materialized pre-sql materialization
          ; _ (ut/pp [:pre-post (str honey-sql)])
                              target-db (cond query-meta-subq? system-db ;; override for sidecar meta queries
                                              (= connection-id "system-db") system-db
                                              (= connection-id "flows-db") flows-db
                                              (= connection-id "system") system-db
                                              (or (= connection-id :cache) (= connection-id "cache.db") (nil? connection-id)) cache-db ;mem-db2
                                              :else (get-connection-string connection-id))
                              has-pivot? #_{:clj-kondo/ignore [:not-empty?]}
                              (not (empty? (ut/extract-patterns honey-sql :pivot-by 2)))
                              honey-sql (if has-pivot?
                                          (let [dim-lookups (distinct (map :pivot-by (pivot/find-select-maps honey-sql)))
                                                hold (atom honey-sql)] ;; temp to replace all instances of sql with literals before CASE creation
                                            (doseq [d dim-lookups
                                                    :let [field (first (first d))
                                    ;agg (first (last d))
                                                          vls0 (last (last (last d))) ;; either static vals or a select to get them
                                                          sql? (and (map? vls0) (or (contains? vls0 :select) (contains? vls0 :select-distinct)))
                                                          cached? (true? (ut/ne? (get @pivot-cache vls0)))
                                                          vls (if sql?
                                                                (if cached? ;; if got exact cache, send it. else query
                                                                  (get @pivot-cache vls0)
                                                                  (try (let [sql-str (to-sql (assoc vls0 :limit 50))
                                                                             sres (sql-query target-db sql-str [:get-pivot-vals-for field :kp ui-keypath])
                                                                             just-vals (vec (map (first (keys (first sres))) sres))]
                                                                         (swap! pivot-cache assoc vls0 just-vals)
                                                                         just-vals) (catch Exception _ ["error" "in" "pivot" "get-vals"])))
                                                                vls0)]]
                        ;(ut/pp [:pivot-dim-lookups :cached? cached? vls sql? :field field])
                                              (when sql? (reset! hold (walk/postwalk-replace {vls0 vls} @hold))))
                                            (pivot/pivot-each @hold 25))

                                          honey-sql)
        ;_ (when has-pivot?
        ;    (ut/pp [:*post-pivot honey-sql :*pre-pivot orig-honey-sql :lookups (map :pivot-by (pivot/find-select-maps honey-sql))]))
                              req-hash (hash [kind ui-keypath honey-sql client-name])
               ;req-sub-hash (hash [kind ui-keypath (sort (get-in honey-sql [:from 0])) client-name])
                              filtered-req-hash (hash [kind ui-keypath
                                 ;honey-sql
                                 ;(-> honey-sql (dissoc :from) (dissoc :order-by))
                                                       (vec (filter keyword? (ut/deep-flatten honey-sql))) ;; eyes emoji ! todo(?)
                                 ;(walk/prewalk-replace {:where nil} honey-sql)
                                                       client-name])
            ;; {cache? false} disables the cache. shockingly.
                              cache? (and ;;(not (nil? (get @sql-cache req-hash))) ;; disabled for testing, 10/9/23
                                      (lookup-cache-exists? req-hash)
                                      (not post-sniffed-literal-data?) ;; questionable, we SHOULD be able to invalidate this and all chilren on change, but for now
                                      (not literal-data?) ;; questionable, we SHOULD be able to invalidate this on change, but for now
               ;;?;;     (not (some #(= % :query-preview) ui-keypath))
                                      (not (= connection-id "flows-db"))
                                      (not (= connection-id "system-log"))
                                      (not (= target-db system-db))
                    ;false
                                      )
                              cache-table-name ;(if (or post-sniffed-literal-data? literal-data?) ;literal-data?
                                ;  (str (ut/keypath-munger ui-keypath) (rand-int 1234545))
                              (ut/keypath-munger ui-keypath)
                                ;  )
                              completion-channel (async/chan) ;; moved to outer
                              data-literal-insert-error (atom nil)
                              honey-sql (if (or post-sniffed-literal-data? literal-data?)
                                          (let [;_ (ut/pp [:last-let? honey-sql])
                             ;lkp (get-in honey-sql (first (filter #(= (last %) :data) (ut/kvpaths honey-sql))))
                                                cache-table cache-table-name ;(str (ut/keypath-munger ui-keypath) (rand-int 1234545))
                            ; cache-table (keyword (str (ut/keypath-munger ui-keypath) (rand-int 1234545)))
                                                honey-sql (if data-literal-code? honey-sql (ut/lists-to-vectors honey-sql))
                            ; literals  data-literals-data
                                                literals (if data-literal-code? data-literals-data
                                                             (get-in honey-sql (or (first (filter #(= (last %) :data) (ut/kvpaths honey-sql))) [:nope])))
                            ; _ (ut/pp [:last-let2? honey-sql])
                            ; literals (vec (if (not post-sniffed-literal-data?) ;(and (not post-sniffed-literal-data?) (seq honey-sql))
                            ;            honey-sql
                            ;            (get-in honey-sql (first (filter #(= (last %) :data) (ut/kvpaths honey-sql)))))) ;; get nested data literal mass
                            ; _ (ut/pp [:last-let3? honey-sql literals])
                                                cached? (if (not client-cache?) false
                                                            (try (not (empty? (get @literal-data-map {:data literals}))) (catch Exception _ false)))
                            ; _ (ut/pp [:last-let4? honey-sql   cached?])
                                                ]

                         ;(ut/pp [:last-hsql-let? ui-keypath cache-table-name honey-sql cache-table cached? post-sniffed-literal-data? data-literal-code? literals ])

                                            (if (and (not cached?)
                                                     (or (list? literals) (vector? literals))
                                  ;(some #(= % :data) (ut/deep-flatten honey-sql))
) ;(and (not cached?) (not post-sniffed-literal-data?))

                                              (do (if data-literal-code?

                                                    (enqueue-task
                                                     (fn []
                                                       (let [;output (evl/run literals) ;; (and repl-host repl-port)
                                                             literals (walk/postwalk-replace {'cie-to-hex 'rvbbit-backend.util/cie-to-hex
                                                                                              'hue-to-hex 'rvbbit-backend.util/hue-to-hex
                                                                                              'hex-to-cie 'rvbbit-backend.util/hex-to-cie
                                                                                              'hex-to-hue-sat 'rvbbit-backend.util/hex-to-hue-sat
                                                                                              'http-call 'rvbbit-backend.websockets/http-call
                                                                                              'flatten-map 'rvbbit-backend.websockets/flatten-map} literals)
                                                             output-full (evl/repl-eval literals repl-host repl-port)
                                                             output (last (get-in output-full [:evald-result :value]))
                                                             output-full (-> output-full
                                                                             (assoc-in [:evald-result :output-lines]
                                                                                       (try (count (remove empty? (get-in output-full [:evald-result :out])))
                                                                                            (catch Exception _ 0)))
                                                                             (assoc-in [:evald-result :values]
                                                                                       (try (count (last (get-in output-full [:evald-result :value])))
                                                                                            (catch Exception _ 0))))
                                                 ;output-meta (dissoc (get output-full :evald-result) :value)
                                                 ;console-output (get output-full :out)
                                                 ;console-meta (get output :meta) ;;nrepl host, etc
                                                             ]
                                             ;(ut/pp [:eval-outputs output output-full])
                                                         (swap! literal-data-output assoc ui-keypath output-full)
                                                         (try (insert-rowset output cache-table (keys (first output)))
                                                              (catch Exception e (do (reset! data-literal-insert-error
                                                                                             ["Data struct not a proper 'rowset', see console log ^" e])
                                                                                     nil)))
                                                         (async/>!! completion-channel true) ;; unblock
                                                         )))

                                                    (enqueue-task
                                                     (fn []
                                                       (insert-rowset literals cache-table (keys (first literals)))
                                                       (async/>!! completion-channel true) ;; unblock
                                                       )))

                                                  (async/<!! completion-channel) ;; BLOCK until our threaded job is done so we don't get out of sync w client
                                                  (swap! literal-data-map assoc {:data literals} cache-table)
                                      ;(walk/postwalk-replace {{:data literals} cache-table} honey-sql)
                                                  (walk/postwalk-replace @literal-data-map honey-sql))

                                              (walk/postwalk-replace @literal-data-map honey-sql)))
                                          honey-sql)]

;(ut/pp [:post-first-let ui-keypath (str honey-sql)])

          ;;  (when (or post-sniffed-literal-data? literal-data?)
          ;;    (let [literals (get (get-in honey-sql (first (filter #(= (last %) :data) (ut/kvpaths honey-sql)))) :data)]
          ;;      (ut/pp [:literals-insert ui-keypath cache-table-name literals])
          ;;      (do (insert-rowset
          ;;           literals ;result
          ;;           cache-table-name (keys (first literals)))
          ;;          (swap! literal-data-map assoc
          ;;                 {:data (get orig-honey-sql :data)}
          ;;             ;(ut/keypath-munger [(first ui-keypath)])
          ;;                 cache-table-name))))

           ;(ut/pp [:post-honey ui-keypath honey-sql])

           ;(ut/pp [:honey-run tt (get orig-honey-sql :transform-select) cache? honey-sql orig-honey-sql])

           ;(when (get honey-sql :transform-select) (ut/pp [:sub-cache? req-sub-hash (not (nil? (get @sql-cache req-sub-hash))) (get-in honey-sql [:from 0])]))

           ;(when (= (count ui-keypath) 0) (ut/pp [:REQ! kind ui-keypath honey-sql connection-id client-name]))

                          (if (and (not (= :styles (last ui-keypath)))
                                   (not sniff?)
                                   cache? client-cache?)

                            (do (ut/pp [:*cache-hit @q-calls kind ui-keypath connection-id client-name] ;:honey-sql honey-sql :client-name client-name
                                       )

                  ; (sniff-meta kind ui-keypath honey-sql nil req-hash target-db client-name) ;; cached meta assumedly...
                  ; (async/thread ;; extra delayed cache run TODO
                  ;   (async/<!! (async/timeout 3500))
                  ;   (sniff-meta kind ui-keypath honey-sql nil req-hash target-db client-name))
                                (-> ;(get @sql-cache req-hash)
                                 (get-from-cache req-hash)
                                 (assoc :cached? true) (assoc :query-ms nil))) ;; return output ;; :query-ms query-ms :cached? false

                            (let [page-num page ;(get honey-sql :page)
                                  per-page-limit (cond (= page-num -1) 50000
                                                       (= page-num -2) 1000000 ;; yikes. revisit TODO
                                                       :else per-page-limit)
                                  honey-sql (if literal-data? honey-sql ;; dont mutate literal data rowsets
                                                (cond (and page-num (and (not (= page-num -2)) (not (= page-num -1))))
                                                      (assoc (dissoc honey-sql :page) :offset (* (- page-num 1) per-page-limit))

                                                      (or (= page-num -1) (= page-num -2))
                                                      (dissoc honey-sql :page)

                                                      :else honey-sql))
              ;honey-sql (walk/postwalk-replace {:date_dim :public.date_dimension} honey-sql) ;; test
;                  target-db (get-connection-string connection-id)
               ;_ (ut/pp [:pre-run-honey (str honey-sql)])
                                  honey-sql-str (when (not literal-data?) ;; no query!
                                                  (if (or (= page-num -1) (= page-num -2)) ;; or limit exists? 10/1/23
                                                    (to-sql honey-sql)
                                                    (to-sql (assoc honey-sql :limit 500))))
                   ;error? (cstr/includes? honey-sql-str)
               ;;_ (ut/pp [:formatter-test (sql-formatter (first honey-sql-str))])
                                  honey-sql-str2 (first honey-sql-str)
                                  _ (async/thread (get-clover-sql-training clover-sql honey-sql-str2)) ;; ONLY WHEN BUILDING TRAINING DATA. test 5/26/24
               ;honey-sql-str2 (sql-formatter (first honey-sql-str))
                                  honey-result (timed-expr (if literal-data? honey-sql ;; which in this case IS the result data
                                                               (sql-query target-db honey-sql-str ui-keypath)))
                                  query-ms (get honey-result :elapsed-ms)
                                  honey-result (get honey-result :result)
                                  query-error? (get (first honey-result) :query_error)

                                  honey-meta (get-query-metadata honey-result honey-sql)
                                  fields (get honey-meta :fields)
                                  dates (remove nil? (for [[k v] fields] (when (cstr/includes? (get v :data-type) "date") k)))
                                  is-meta? (= 3 (count ui-keypath))
                                  is-condi? (= (keys fields) '(:v)) ;; condi eval call
                   ;; i.e. is a post-meta query, not the "real" query - or if 1, it's a condi eval...
                                  result (if (empty? dates) (vec (take per-page-limit honey-result))
                                             (vec (for [r (take per-page-limit honey-result)]
                                                    (into {} (for [[k v] r]
                                                               (if (some #(= % k) dates)
                                                                 {k (str v)}
                                                                 {k v}))))))
                  ; result honey-result
                                  result (if (and query-error? @data-literal-insert-error)
                                           (vec (into result [{:query_error (str (first @data-literal-insert-error))}
                                                              {:query_error (str (last @data-literal-insert-error))}]))
                                           result) ;; add extra error data from code evaL
                                  result (if (get orig-honey-sql :transform-select)
                                           (do (ut/pp [:transform (assoc orig-honey-sql :from [:data])])
                                               (let [res (ts/transform (assoc orig-honey-sql :from [:data]) result)]
                               ;(ut/pp [:transformed res])
                                                 res))
                                           result)
            ;;;_ (ut/pp [:result-pre-replace result @rql-holder])
                                  result (if has-rql? (let [walk-map (get-in @rql-holder ui-keypath)
                                                            replaced (vec (for [row-map result]
                                                                            (let [;with-code (walk/postwalk-replace walk-map row-map)
                                                               ;with-vals (walk/postwalk-replace row-map)
                                                                                ; _ (println (str ">> " walk-map))
                                                                                  safe-keys (into {} (apply (fn [x] {x (keyword (cstr/replace (str x) #":_" ""))})
                                                                                                            (filter #(cstr/starts-with? (str %) ":_")
                                                                                                                    (distinct (ut/deep-flatten orig-honey-sql)))))
                                                                                  safe-keys-rev (into {} (for [[k v] safe-keys] [v k])) ; {:key :_key}
                                                                                  from-kp [1 :queries :gen-viz-609 :from] ;(first (filter #(= (last %) :from) (ut/kvpaths new-field)))
                                                                                ; select-kp (first (filter #(= (last %) :select) (ut/kvpaths new-field)))
                                                                                ; order-kp (first (filter #(= (last %) :order-by) (ut/kvpaths new-field)))
                                                                                ; group-kp (first (filter #(= (last %) :group-by) (ut/kvpaths new-field)))
                                                                                  walk-map (into {} (for [[k v] walk-map
                                                                                                          :let [protect-where (get-in v from-kp)]]
                                                                                                      {k (assoc-in v from-kp (walk/postwalk-replace safe-keys-rev protect-where))}))

                                                                                ; _ (println protect-where)
                                                                                ; protect-select (get-in new-field select-kp)
                                                                                ; protect-group (get-in new-field group-kp)
                                                                                ; protect-order (get-in new-field order-kp)
                                                                                ; _ (println from-kp)
                                                                                 ;walk-map (assoc-in walk-map from-kp (walk/postwalk-replace safe-keys-rev protect-where)) ;; protect where clause keys
                                                                                ; new-field (assoc-in new-field select-kp (walk/postwalk-replace safe-keys-rev protect-select)) ;; protect where clause keys
                                                                                ; new-field (assoc-in new-field group-kp (walk/postwalk-replace safe-keys-rev protect-group)) ;; protect where clause keys
                                                                                ; new-field (assoc-in new-field order-kp (walk/postwalk-replace safe-keys-rev protect-order)) ;; protect where clause keys

                                                                                  new-field (walk/postwalk-replace row-map walk-map)
                                                                                  new-field (into {}
                                                                                                  (for [[k v] new-field]
                                                                                                    (if (= (first v) :*read-edn*)
                                                                                                      (let [bd (get v 1) ;; edn body
                                                                                                            kp (get v 2) ;; keypath
                                                                                                            cst (get v 3) ;; cast if asked?
                                                                                     ; rd (try (edn/read-string bd) (catch Exception e (str e)))
                                                                                                            rr (try (let [vv (get-in (edn/read-string bd) kp)]
                                                                                                                      (if cst (ut/cast-to-type vv cst) vv))
                                                                                                                    (catch Exception e
                                                                                                                      (str ":*read-edn-error*:" e bd)))]
                                                                                  ;(ut/pp [:ROW-DEBUG rd rr k v])
                                                                                                        {k rr})
                                                                                                      {k v})))

                                                                                ; _ (println safe-keys) ; {:_key :key}

                                                                                  new-row (walk/postwalk-replace new-field row-map)
                                                                                  new-row (walk/postwalk-replace safe-keys new-row)] ;; replace :_ with : to avoid keyword collision, skip replace
                                                        ;(ut/pp [:ROW! new-row new-field row-map])
                                                                              new-row)))]
                                  ;;(swap! rql-holder ut/dissoc-in ui-keypath) ;; pointless, atom is ephemeral
                                                        (println (first replaced))
                                                        replaced)
                                             result)
                                  ;; _ (when (not (nil? post-process-fn)) (ut/pp [:post-process2! post-process-fn]))
                                  result (if (not (nil? post-process-fn))
                                           (try ((eval post-process-fn) result)
                                                (catch Throwable e (let [res [{:error "post-process-fn error" :vval (str e)}]]
                                                                     (ut/pp [:post-process-fn-error res])
                                                                     res)))
                                           result)
                                  ;; _ (when (not (nil? post-process-fn)) (ut/pp result))

                                  honey-meta (if (or (get orig-honey-sql :transform-select)
                                                     has-rql?      ;query-error?
                                                     post-process-fn
                                                     (get orig-honey-sql :data)) ;; TODO< this is ugly rebinding shit
                                               (get-query-metadata result honey-sql) ;; get new meta on transformed data
                                               honey-meta)
                                  ;; _ (when (not (nil? post-process-fn)) (ut/pp [:post-process-meta! honey-meta]))
                                  fields (get honey-meta :fields) ;; lol, refactor, this is cheesy (will overwrite if transform)
                                  sniff-worthy? (and (not is-meta?)
                                                     (not has-rql?) ;;; temp since insert will fail dur to not being stringified, TODO
                                                     (not (= (ut/dissoc-recursive honey-sql)
                                                             (ut/dissoc-recursive (get-in @sql/query-history
                                                                                          [cache-table-name :honey-sql])))) ;; <-- seen this exact shape before?
                                                     (ut/ne? (flatten result))
                                                     (not (cstr/includes? (str ui-keypath) "-hist-")) ;; undo history preview queries, do NOT sniff
                                                     (not query-error?)
                                                     (not is-condi?)
                                                     (not (= (get honey-sql :limit) 111))
                                                     (not (some #(cstr/starts-with? (str %) ":query-preview") ui-keypath)))
                                  sniffable? (or sniff? ;; <-- req from client only?
                                                 (and (not is-meta?) false ;; otherwise never? only on manual for now
                                                      (not query-error?)
                                                      (not post-process-fn)
                             ;(not (get orig-honey-sql :transform-select))
                                                      (not is-condi?)
                             ;(not (= (last ui-keypath) :styles))  ;; style call. deprecated
                                                      (not (= (get honey-sql :limit) 111)) ;; a "base table sniff", generally unnecessary...
                             ; (not (some #(= % filtered-req-hash) @deep-run-list)) ;; we seen it!
                                                      (not (some #(cstr/starts-with? (str %) ":query-preview") ui-keypath)) ;; browsing previously generated recos...
                                                      ))


                               ;  kit-output? (or sniff? (and (not is-meta?) (not query-error?) (not is-condi?) false))
                                  repl-output (ut/limited (get-in @literal-data-output [ui-keypath :evald-result] {}))
                                  output {:kind kind :ui-keypath ui-keypath :result result :result-meta honey-meta
                                          :sql-str honey-sql-str2 :query-ms query-ms :cached? false :connection-id connection-id
                                          :repl-output repl-output :original-honey orig-honey-sql :panel-key panel-key
                                          :client-name client-name :map-order (if (or (get orig-honey-sql :transform-select)
                                                                                      query-error?
                                                                                      post-process-fn
                                                                                      (get orig-honey-sql :data))
                                                                                (keys fields)
                                                                                (get @sql/map-orders honey-sql-str))}
                                  result-hash (hash result)]
        ;;(ut/pp (dissoc output :result))
               ;(tap> [:kkkk (keys (first honey-result))])
         ; (dec! running-user-queries)

          ;;                    (when kit-output? ;; honestly we should kick off a new specil version of query so it can use all the fancy logic, but it gets no limit and saved...
          ;;  ;; i.e. we spawn a new honeyx-call with no limit, and save it to the cache via an enqueue-task job - re-call this method
          ;;  ;; but for now lets just fake it
          ;;  ;(let [full-result])
          ;;                      (ut/pp [:kit-call]))

        ;; (when sniffable? ;;; commented out 10/21/23
        ;;   (ut/pp {:cached? cache? :kind kind :rows (count result) :ui-keypath ui-keypath
        ;;           :honey-sql honey-sql :client-name client-name :fields (keys fields)}))

              ;;  (when ;(and (not sniffable?)
              ;;             (get orig-honey-sql :transform-select)
              ;;        ;     )
              ;;    (async/thread
              ;;      (insert-rowset
              ;;       result
              ;;       (ut/keypath-munger ui-keypath) (keys (first result)))))

         ;;  (ut/pp [:sniffable? sniffable? ui-keypath :via client-name ui-keypath])
        ; (ut/pp [:result! ui-keypath (take 2 result)])
                              ;(ut/pp [:sniffable? sniffable? sniff?])

                              (if sniffable?
             ;; full reco sniff or just quick meta cache...
                                (doall (do (swap! deep-run-list conj filtered-req-hash) ;; mark this as run BEFORE we start the thread...
                 ;(send-off sniff-agent1 ;async/thread ;;with-timeout 300000 ;async/thread
                                           (enqueue-task
                   ;(ut/pp [:deep-running filtered-req-hash cache-table-name ui-keypath])
                                            (fn []
                    ;    (async/thread
                                              (ut/pp :kick-sniff)
                                              (push-to-client ui-keypath [:reco-status (first ui-keypath)] client-name  1 :reco :started)
                                              (swap! sql/query-history assoc cache-table-name {:honey-sql honey-sql
                                                                                               :connection-id connection-id})

                                              (let [result (vec (for [r result] (assoc r :rows 1)))] ;;; hack to generate rowcount recos
                                                (insert-rowset
                                                 result
                                                 cache-table-name (keys (first result))))

                                              (doall
                                               (do

                                                 (do (ut/pp [[:recos-started-for (first ui-keypath)] :via client-name ui-keypath filtered-req-hash])
                                                     (push-to-client ui-keypath [:reco-status (first ui-keypath)] client-name  1 :reco :started))
                  ;(time

                  ;;  (cruiser/lets-give-it-a-whirl
                  ;;   "cache.db"
                  ;;   cache-db system-db
                  ;;   cruiser/default-sniff-tests
                  ;;   cruiser/default-field-attributes
                  ;;   cruiser/default-derived-fields
                  ;;   cruiser/default-viz-shapes [:= :table-name cache-table-name])

                  ;;   (cruiser/captured-sniff "cache.db" connection-id cache-db result-hash [:= :table-name cache-table-name])
                  ;(cruiser/captured-sniff connection-id target-db result-hash [:= :table-name cache-table-name])
                  ;; src-conn-id base-conn-id src-conn result-hash & [sql-filter]]

; )

                                                 (doall
                                                  (let [run-it (timed-expr (cruiser/captured-sniff "cache.db" connection-id target-db cache-db result-hash [:= :table-name cache-table-name]))
                                                        reco-count-sql {:select [[[:count 1] :cnt]] :from [:combos] :where [:= :table-name cache-table-name]}
                                                        reco-count (get-in (sql-query system-db (to-sql reco-count-sql) [:reco-count :client-status]) [0 :cnt])]
                                                    (ut/pp [[:recos-finished-for (first ui-keypath)] :via client-name ui-keypath filtered-req-hash])
                                                    (push-to-client ui-keypath [:reco-status (first ui-keypath)] client-name 1 :reco :done reco-count (get run-it :elapsed-ms)))))) ;; vertica [:= :db-schema "online_sales"]
            ;(ut/pp [:completed? ])
            ;    (async/go (async/>! external-changes {:client-name client-name :do-it (rand-int 123456)})) ;; tell the client to refresh its recos
                                              ))))

                                (when sniff-worthy? ;; want details, but not yet the full expensive meta reco sniff
               ;(send-off sniff-agent1 ;;async/thread ;;with-timeout 300000 ;async/thread ;; quick no viz sniff
                                  (enqueue-task
                                   (fn []
              ; (async/thread
                                     (swap! sql/query-history assoc cache-table-name {:honey-sql honey-sql
                                                                                      :connection-id connection-id})
                                     (doall
                                      (let [result (vec (for [r result] (assoc r :rows 1)))] ;;; hack to generate rowcount recos
                     ; (when true ; (not (= honey-sql (get @sql/query-history cache-table-name)))
                     ;   (insert-rowset
                     ;    result
                     ;    cache-table-name (keys (first result)))
                     ;   )                                                                      ;;quick? bool, resultset
                                        (cruiser/captured-sniff "cache.db" connection-id target-db cache-db result-hash [:= :table-name cache-table-name] true result)
                                        ;; (kick client-name "kick-test!" (first ui-keypath)
                                        ;;       "query-log"
                                        ;;       (str "query-log-" (first ui-keypath))
                                        ;;       (str "query-log-" (first ui-keypath))
                                        ;;       [(str (ut/get-current-timestamp) " - quick meta sniff ran.")])
                                        (ut/pp [[:quick-sniff-for (first ui-keypath)] :via client-name ui-keypath filtered-req-hash]))))))

;(ut/pp [ui-keypath (ut/keypath-munger ui-keypath) (keys (first result))])
                                )
          ;(insert-rowset result (first ui-keypath) (keys result))

        ;  (sniff-meta kind ui-keypath honey-sql fields req-hash target-db client-name)
        ;  (async/thread ;; extra delayed run (which hits cache) to make sure there wasnt any async hiccups TODO
        ;    (async/<!! (async/timeout 3500))
        ;    (sniff-meta kind ui-keypath honey-sql fields req-hash target-db client-name))

          ;; (async/thread
          ;;   (let [honey-sql (dissoc honey-sql :offset)]
          ;;     (dorun
          ;;      (for [[[name f] hsql] (merge {[:rowcount :*] {:select [[[:count 1] :rowcnt]] :from [[honey-sql :subq]]}
          ;;                                   ;[:rowcount2 :*] {:select [[[:count 1] :rowcnt2]] :from [[honey-sql :subq]]} ;; other "rules"
          ;;                                    }
          ;;                                   (into {} (for [field (keys fields)]
          ;;                                              {[:distinct field]
          ;;                                               {:select [[[:count [:distinct field]] :distinct-values]] :from [[honey-sql :subq]]}})))]

          ;;        (let [preq-hash (hash [kind ui-keypath hsql name])
          ;;              pcache? (not (nil? (get @sql-cache preq-hash)))]
          ;;          (if pcache? (let [payload (get @sql-cache preq-hash)]
          ;;                        (do (ut/ppln [:post-meta-cache payload])
          ;;                            payload))

          ;;              (async/go
          ;;                (async/>! external-changes
          ;;                          (let [str-sql (to-sql hsql)
          ;;                                sql-result (first (vals (first (sql-query target-db str-sql [ui-keypath :post-meta]))))
          ;;                                payload {:ui-keypath ui-keypath
          ;;                                         :name name
          ;;                                         :field f
          ;;                                         :data sql-result}]
          ;;                            (ut/ppln [:post-meta payload]) ;; debug
          ;;                            (do (swap! sql-cache assoc preq-hash payload)
          ;;                                payload))))
          ;;              ))
          ;;        ))))

                              (do ;(swap! sql-cache assoc req-hash output)

                                ;; query kick back. kinda neat, but also kinda unnecessary
                                ;; (when (and (not (cstr/starts-with? (str (first ui-keypath)) ":kick"))
                                ;;            (not (cstr/includes? (str (first ui-keypath)) "-sys")))
                                ;;   (kick client-name "kick-test!" (first ui-keypath)
                                ;;         "query-log"
                                ;;         (str "query-log-" (first ui-keypath))
                                ;;         (str "query-log-" (first ui-keypath))
                                ;;         [(str (ut/get-current-timestamp) " - query ran in " query-ms " ms.")]))

                                (when client-cache? (insert-into-cache req-hash output)) ;; no point to cache things that are :cache?false
                                output)))))
                       (catch Exception e
                         (do
                           (ut/pp [:honeyx-OUTER-LOOP-EXCEPTION (str e) :connection-id connection-id ui-keypath (str honey-sql)])

                           {:kind kind :ui-keypath ui-keypath
                            :result [{:query_error "execution error"}
                                     {:query_error (str e)}] :result-meta {}
                            :sql-str "" :query-ms nil :cached? false
                            :repl-output (assoc (ut/limited (get-in @literal-data-output [ui-keypath :evald-result] {})) :error (str e))
                            :client-name client-name
                            :map-order []}))))]

                        ;;  (if data-call?
                        ;;    (do
                        ;;      (async/<!! completion-channel-client)
                        ;;      (runstream)
                        ;;      (async/>!! completion-channel-client true))
                        ;;    (runstream))

     ;(doall
     (doall (if data-call?
              (doall (let [async-result-chan (go (let [result-chan (queue-runstream runstream)]
                                                   (println "Waiting for result...") ; Debugging
                                                   (<! result-chan)))] ; This returns a channel
      ;; To get the result, you need to take from async-result-chan
                       (do (println "Getting result from async operation...")
                           (async/<!! async-result-chan)))) ; Blocking take from the channel
              (runstream))))));)

(defn kit-to-kit [payload])

(defn insert-kit-data [output query-hash ui-keypath ttype kit-name elapsed-ms & [client-name flow-id]]
  (enqueue-task3 ;; blocking for inserts and deletes (while we are one sqlite, will be conditional for diff system-db choices)
   (fn []
     (let [output-is-valid? (map? output) ;; basic spec checking
           kit-keys (keys output)
           kkit-name (if (vector? kit-name) (str (first kit-name)) (str kit-name))
           output-rows (vec  (apply (if (= kit-keys 1) flatten into)
                                    (for [[k v] output
                                          :let [;descr (pr-str (get v :description))
                                                                        ;mutates (pr-str (get v :mutates))
                                                                        ;options (pr-str (get v :options))
                                                                        ;parameters (pr-str (get v :parameters))
                                                rows (get v :data)
                                                rowset (vec (for [idx (range (count rows))
                                                                  :let [i (get rows idx)
                                                                        row-map {:item_hash query-hash
                                                                                 :item_name (if (vector? ui-keypath) (str (first ui-keypath)) (str ui-keypath))
                                                                                 :item_type (str ttype)
                                                                                 :item_key (str k)
                                                                                 :kit_name kkit-name
                                                                                 :item_idx idx
                                                                                 :client_name (str client-name)
                                                                                 :flow_id (str flow-id)
                                                                                 :item_options (pr-str (select-keys v [:options :parameters
                                                                                                                       :mutates :description]))
                                                                                 :item_data (pr-str i)}]]
                                                              row-map))]]
                                      rowset)))
           ;kit-rows (count output-rows)
           ]

       ;(ut/pp [:kit-returned (count (keys output)) :keys kit-rows :items elapsed-ms :ms])
       (when (not (= kkit-name ":kick")) ;; let kick "messages" pile up, dont swap out like a normal kit fn response
         (sql-exec system-db (to-sql {:delete-from [:kits] :where [:and
                                                                   [:= :kit_name kkit-name]
                                                                   [:= :item_name (if (vector? ui-keypath) (str (first ui-keypath)) (str ui-keypath))]
                                                              ;[:= :item_hash query-hash]
                                                                   ]})))
       (sql-exec system-db (to-sql {:insert-into [:kits] :values output-rows}))))))

(defmethod wl/handle-request :honey-xcall [{:keys [kind ui-keypath honey-sql client-cache? sniff? connection-id panel-key client-name page kit-name clover-sql]}]
  (swap! q-calls inc)
  ;;(inc! running-user-queries)
  (inc-score! client-name :push)
  ;; (ut/pp [:PUSH-honeyx-call! (str honey-sql)])

  ;; (ut/pp [kind ;(not (empty? (ut/extract-patterns honey-sql :pivot-by 2)))
  ;;         @q-calls kind ui-keypath connection-id client-name panel-key page
  ;;         {:client-cache? client-cache? :sniff? sniff? ;:raw-honey (str honey-sql) ; (get honey-sql :transform-select) :full-honey honey-sql ; :select (get honey-sql :select)
  ;;         ;(when (get honey-sql :transform-select) [:TSELECT!! honey-sql])
  ;;          }])

  (when (keyword? kit-name) ;;; all kit stuff. deprecated?
    (enqueue-task2 (fn [] (try ;; save off the full thing... or try
                            (let [;kit-name :outliers
                                  _ (push-to-client ui-keypath [:kit-status (first ui-keypath)] client-name 2 kit-name :started)
                                  output (query-runstream kind ui-keypath honey-sql false false connection-id client-name -2 panel-key clover-sql) ;; -2 has 1m row limit. yikes.
                                  fullrows (get output :result)
                                  fullrows (vec (for [r fullrows] (assoc r :rows 1)))
                                  kp (str (ut/unkeyword (get-in output [:ui-keypath 0] "unknown")))
                                  rows (count fullrows)
                                  meta (-> (get output :result-meta)
                                           (assoc :original-honey (get output :original-honey))
                                           (assoc :connection-id (get output :connection-id)))
                                  mpath (str "./kit-rowsets/" kp ".meta.edn")
                                  path (str "./kit-rowsets/" kp ".edn")
                                  query-hash (hash honey-sql)
                                  ttype :queries]

                              (spit path (pr-str fullrows) :append false)
                              (spit mpath (pr-str meta) :append false)
                              (ut/pp [:saved-full-to path rows :rows])
                              (ut/pp [:repl-exec-for kit-name])

                                       ;; run kit repl
                              (let [repl-fn (get-in config/kit-fns [kit-name :fn]) ;'(+ (rand-int 12345) (rand-int 12345))
                                    repl-command (walk/postwalk-replace
                                                  {:query honey-sql
                                                  ; :meta (pr-str meta)
                                                  ; :data (pr-str fullrows) ;; full rowset EDN
                                                   :query-name (first ui-keypath)
                                                   :kit-name kit-name
                                                   :panel-name panel-key}
                                                  repl-fn)
                                    _ (when kit-name (ut/pp [:running-kit kit-name]))
                                    repl-host (get-in config/kit-fns [kit-name :repl :host])
                                    repl-port (get-in config/kit-fns [kit-name :repl :port])
                                    output-full (timed-expr (evl/repl-eval repl-command repl-host repl-port))
                                    elapsed-ms (get output-full :elapsed-ms)
                                    output-full (get output-full :result) ;; from timed map
                                    output (last (get-in output-full [:evald-result :value]))

                                            ;;  output-full (-> output-full ;; with console output
                                            ;;                  (assoc-in [:evald-result :output-lines]
                                            ;;                            (try (count (remove empty? (get-in output-full [:evald-result :out])))
                                            ;;                                 (catch Exception _ 0)))
                                            ;;                  (assoc-in [:evald-result :values]
                                            ;;                            (try (count (last (get-in output-full [:evald-result :value])))
                                            ;;                                 (catch Exception _ 0))))
                                               ;kit-output {kit-name {(first ui-keypath) output}}
                                    _ (ut/pp [:**************************** :repl-output :****************************])
                                    _ (ut/pp (ut/limited (get-in output-full [:evald-result :out]) 5))
                                    ;_ (ut/pp (ut/limited output-full 5))
                                    _ (ut/pp [:**************************** :repl-output :****************************])
                                               ; output ddl/sample-kit-output ;; temp testing
                                    ;; output-is-valid? (map? output) ;; basic spec checking
                                    ;; kit-keys (keys output)
                                    ;; output-rows (vec  (apply (if (= kit-keys 1) flatten into)
                                    ;;                          (for [[k v] output
                                    ;;                                :let [;descr (pr-str (get v :description))
                                    ;;                                     ;mutates (pr-str (get v :mutates))
                                    ;;                                     ;options (pr-str (get v :options))
                                    ;;                                     ;parameters (pr-str (get v :parameters))
                                    ;;                                      rows (get v :data)
                                    ;;                                      rowset (vec (for [idx (range (count rows))
                                    ;;                                                        :let [i (get rows idx)]]
                                    ;;                                                    {:item_hash query-hash
                                    ;;                                                     :item_name (str (first ui-keypath))
                                    ;;                                                     :item_type (str ttype)
                                    ;;                                                     :item_key (str k)
                                    ;;                                                     :kit_name (str kit-name)
                                    ;;                                                     :item_idx idx
                                    ;;                                                     :item_options (pr-str (select-keys v [:options :parameters
                                    ;;                                                                                           :mutates :description]))
                                    ;;                                                     :item_data (pr-str i)}))]]
                                    ;;                            rowset)))
                                    ;; kit-rows (count output-rows)
                                    kit-keys (count (keys output))]
                                (insert-kit-data output query-hash ui-keypath ttype kit-name elapsed-ms)
                                ;;             ;(ut/pp output-rows)
                                ;; (ut/pp [:kit-returned (count (keys output)) :keys kit-rows :items elapsed-ms :ms])
                                ;; (sql-exec system-db (to-sql {:delete-from [:kits] :where [:and
                                ;;                                                           [:= :kit_name (str kit-name)]
                                ;;                                                           [:= :item_name (str (first ui-keypath))]
                                ;;                                                            ;[:= :item_hash query-hash]
                                ;;                                                           ]}))
                                ;; (sql-exec system-db (to-sql {:insert-into [:kits] :values output-rows}))

                               ; (do
                                (push-to-client ui-keypath [:kit-status (first ui-keypath)] client-name 2 kit-name :done kit-keys elapsed-ms)
                                              ;(ut/pp output)
));)
                            (catch Exception e (ut/pp [:save-full-error (str e) :err-end]))))))

   ;; if keypath includeds query-preview-block or query-preview-inviz, we know its a weave trellis, so maybe throttle them so they dont overwhelm everything
   ;; with any luck the cache will even things out eventually - ah, nevermind we'd need a blocking queue

  (doall (if (or (cstr/includes? (str ui-keypath) "query-preview-block") ;; queue up the inviz queries
                 (cstr/includes? (str ui-keypath) "query-preview-inviz"))
           (doall (let [async-result-chan (go (let [result-chan (queue-runstream (fn []
                                                                                   (query-runstream kind ui-keypath honey-sql client-cache? sniff? connection-id client-name page panel-key clover-sql)))]
                                                (println "inviz: Waiting for result...") ; Debugging
                                                (<! result-chan)))] ; This returns a channel; This returns a channel
      ;; To get the result, you need to take from async-result-chan
                    (do (println "inviz: Getting result from async operation...")
                        (async/<!! async-result-chan)))) ; Blocking take from the channel

           (query-runstream kind ui-keypath honey-sql client-cache? sniff? connection-id client-name page panel-key clover-sql)))







  ;; (when sniff? (enqueue-task2 (fn [] (try ;; save off the full thing... or try
  ;;                                      (let [output (query-runstream kind ui-keypath honey-sql false false connection-id client-name -2 panel-key) ;; -2 has 1m row limit. yikes.
  ;;                                            fullrows (get output :result)
  ;;                                            fullrows (vec (for [r fullrows] (assoc r :rows 1)))
  ;;                                            kp (str (ut/unkeyword (get-in output [:ui-keypath 0] "unknown")))
  ;;                                            rows (count fullrows)
  ;;                                            meta (-> (get output :result-meta)
  ;;                                                     (assoc :original-honey (get output :original-honey))
  ;;                                                     (assoc :connection-id (get output :connection-id)))
  ;;                                            mpath (str "./kit-rowsets/" kp ".meta.edn")
  ;;                                            path (str "./kit-rowsets/" kp ".edn")
  ;;                                            kit-name :outliers
  ;;                                            ]
  ;;                                        (spit path (pr-str fullrows))
  ;;                                        (spit mpath (pr-str meta))
  ;;                                        (ut/pp [:saved-full-to path rows :rows])

  ;;                                      ;; run kit repl
  ;;                                        (let [repl-fn (get-in config/kit-fns [kit-name :fn])
  ;;                                              repl-command (walk/postwalk-replace
  ;;                                                            {:query honey-sql
  ;;                                                             :query-name (first ui-keypath)
  ;;                                                             :panel-name panel-key}
  ;;                                                            repl-fn)
  ;;                                              _ (when kit-name (ut/pp [:running-kit kit-name repl-fn repl-command]))
  ;;                                              repl-host (get-in config/kit-fns [kit-name :repl :host])
  ;;                                              repl-port (get-in config/kit-fns [kit-name :repl :port])
  ;;                                              output-full (evl/repl-eval repl-command repl-host repl-port)
  ;;                                              output (last (get-in output-full [:evald-result :value]))
  ;;                                             ;;  output-full (-> output-full ;; with console output
  ;;                                             ;;                  (assoc-in [:evald-result :output-lines]
  ;;                                             ;;                            (try (count (remove empty? (get-in output-full [:evald-result :out])))
  ;;                                             ;;                                 (catch Exception _ 0)))
  ;;                                             ;;                  (assoc-in [:evald-result :values]
  ;;                                             ;;                            (try (count (last (get-in output-full [:evald-result :value])))
  ;;                                             ;;                                 (catch Exception _ 0))))
  ;;                                              kit-output {kit-name {(first ui-keypath) output}}]
  ;;                                          (ut/pp kit-output)))
  ;;                                      (catch Exception e (ut/pp [:save-full-error (str e) :err-end]))))))


  ;; (enqueue-task (fn [] (let [tt (wl/handle-request :honey-xcall {:kind kind
  ;;                                                                      :ui-keypath ui-keypath
  ;;                                                                      :honey-sql honey-sql
  ;;                                                                      :client-cache? false ;client-cache?
  ;;                                                                      :sniff? false
  ;;                                                                      :connection-id connection-id
  ;;                                                                      :client-name client-name
  ;;                                                                      :page -1})]
  ;;                        (ut/pp [:top-call (take 10 tt)]))))

  ;; (ut/pp (take 10 (call-honey-xcall {:kind kind
  ;;                                    :ui-keypath ui-keypath
  ;;                                    :honey-sql honey-sql
  ;;                                    :client-cache? false ;client-cache?
  ;;                                    :sniff? false
  ;;                                    :connection-id connection-id
  ;;                                    :client-name client-name
  ;;                                    :page -1})))

  ;; (when (>= (count @sql-cache) 500)
  ;;   (do (ut/pp [:clearing-out-sql-cache! :at 500 :keys])
  ;;       (reset! sql-cache {})))

  ;(time! (timer instrument/metric-registry ["queries" "timings" connection-id]) ;; test
  )




;(async/put! external-changes
;            (merge (get-update-details (str (get % :path))) {:file-op (get % :type)}))


(defn send-file-success [content filename]
  (assoc (http/edn-response content) :headers {"Content-Type" "application/octet-stream" ;;"application/edn"
                                               "Content-Disposition" "attachment"
                                               "filename" filename}))  ;"text/plain"

(defn send-edn-success [content]
  (assoc (http/edn-response content) :headers {"Content-Type" "application/edn"}))  ;"text/plain"

(def pool (cp/threadpool 5))

(defn process-csv [request & [local-file]]
  (ut/pp [:incoming-file (if local-file local-file
                             (get-in request [:edn-params :fname]))])
  (time (let [seed-id (rand-int 123124) ;; for status table

              file-name (if local-file local-file
                            (get-in request [:edn-params :fname]))
              client-name (if local-file "system"
                              (str (get-in request [:edn-params :client-name])))
              fdata (if local-file (slurp local-file)
                        (get-in request [:edn-params :image]))

        ;file-base-name (last (cstr/split (first (cstr/split file-name #"\.")) #"/")) ;(ut/sanitize-name (str screen-name))
              file-base-name (first (cstr/split (last (cstr/split file-name #"/")) #"\."))
              file-path (str "./data/" file-name)

              fdata-map (doall (ut/csv-data->maps (csv/read-csv fdata)))
              ftypes (into {} (for [f (keys (first fdata-map))]
                                {f (flatten (take 5 (map (juxt f) fdata-map)))}))
              ftypes-coerce (into {}
                                  (for [[k v] ftypes]
                                    {k (cond
                                         (try (integer? (Long/parseLong (first v))) (catch Exception _ false)) :integer
                                         (try (float? (Float/parseFloat (first v))) (catch Exception _ false)) :float
                                         :else :string)}))
              fdata-map-mod (vec (cp/pfor pool [row fdata-map] ;; cp/pfor pool
                                          (into {} (for [[k tt] ftypes-coerce]
                                                     {k ;[tt (get row k)]
                                                      (try (let [ff (get row k nil)
                                                                 fff (if (or (empty? ff) (= ff " ") (= ff "")) nil ff)]
                                                             (cond
                                                               (= tt :float) (Float/parseFloat fff)
                                                               (= tt :integer) (Long/parseLong fff)
                                                               :else (str fff)))
                                                           (catch Exception e
                                                        ;(str e)
                                                             nil))}))))
              op-name (str "csv: " file-path)
               ; csv-db {:classname   "org.sqlite.JDBC"
               ;         :subprotocol "sqlite"
               ;         :subname    (str "./data/" file-base-name ".db?busy_timeout=20000&cache=shared")}
                ;fdata-map (ccsv/parse-csv fdata)
              ]
          (ut/pp [:processing op-name])
          (sql-exec system-db (to-sql {:insert-into [:status]
                                       :columns [:client_name :op_name :status]
                                       :values [[client-name op-name "processing csv..."]]}))
          (insert-rowset-csv fdata-map-mod file-base-name client-name op-name)
           ; (sql-exec system-db (to-sql {:insert-into [:status]
           ;                              :columns [:client_name :op_name :status]
           ;                              :values [[client-name op-name "examining table..."]]}))
          (cruiser/lets-give-it-a-whirl-no-viz
           file-path
           import-db system-db
           cruiser/default-sniff-tests
           cruiser/default-field-attributes
     ;{} ;cruiser/default-derived-fields
           cruiser/default-derived-fields
           cruiser/default-viz-shapes
             ;[:= :table-name file-base-name]
           )
           ; (sql-exec system-db (to-sql {:insert-into [:status]
           ;                              :columns [:client_name :op_name :status]
           ;                              :values [[client-name op-name "finished"]]}))
          (do (ut/pp [:saved-csv file-path file-base-name (first fdata-map) ftypes ftypes-coerce  (first fdata-map-mod)])
                ;(spit file-path fdata)
              ))))

(defn save-csv [request]
  (time (do
          (process-csv request)))
  (send-edn-success {:status "saved-csv" :screen (first (get request :edn-params))}))

(defn save-alert-notification [client-name name fpath flow?]
  (alert! client-name [:v-box
                       :justify :center
                       :style {;:margin-top "-6px"
                               :opacity 0.7} ;:color (if error? "red" "inherit")}
                       :children [[:box
                                   :style {:font-weight 700 :font-size "11px" :opacity 0.6}
                                   :child (str (str "successfully saved " (if flow? "flow" "screen")))]
                                  [:box
                                   :style {:font-weight 700 :font-size "18px"}
                                   :child (str name)]
                                  [:box
                                   :style {:font-weight 700 :font-size "11px" :opacity 0.6}
                                   :child (str "@ " fpath)]]] 10 1.6 8))

(defn save-alert-notification-pre [client-name name fpath flow?]
  (alert! client-name [:v-box
                       :justify :center
                       :style {;:margin-top "-6px"
                               :opacity 0.6} ;:color (if error? "red" "inherit")}
                       :children [[:box
                                   :style {:font-weight 700 :font-size "13px"}
                                   :child (str "saving " name "...")]]] 10 0.6 8))

(defn save [request]
  ;(time 
  (do
    (let [screen-name (get-in request [:edn-params :screen-name])
          client-name (get-in request [:edn-params :client-name] "unknown")
          file-base-name (ut/sanitize-name (str screen-name))
          file-path (str "./screens/" file-base-name ".edn")
          fpath (ut/abs-file-path file-path)]
      (save-alert-notification-pre client-name screen-name nil false)

      (do (ut/pp [:saved-file file-path])
               ;(ut/pretty-spit file-path (get-in request [:edn-params :image])) ;; file looks good, but SO fuggin slow
          (ut/pretty-spit file-path (get-in request [:edn-params :image]))
                ;(spit file-path (get-in request [:edn-params :image]))

         ; (async/go
         ;   (async/<! (ut/pretty-spit file-path (get-in request [:edn-params :image])))
         ;   ;(ut/ppln [:finished :prety-spit-fn])
         ;   )
          (save-alert-notification client-name screen-name fpath false))))
   ;)


   ; (let [flow_name (get-in request [:edn-params :flowset-name])
   ;       file-name (clojure.string/replace (str flow_name ".rabbit") " " "_")]
   ;   (send-file-success {:status "saved" :flowset (first (get request :edn-params))} file-name))


  (send-edn-success {:status "saved" :screen (first (get request :edn-params))}))

;; (ut/save-base64-to-png image (str "./snaps/" client-name ".png"))

(defn save-snap [request]
  (try
    ;(time
    (let [image (get-in request [:edn-params :image])
          session (get-in request [:edn-params :session])
          client-name (get-in request [:edn-params :client-name] "unknown")
          client-name (cstr/replace (str client-name) ":" "")
          file-path (str "./snaps/" client-name ".jpg")
          ifile-path (str "/home/ryanr/rvbbit/frontend/resources/public/snaps/" client-name ".jpg")
          sess-path (str "./snaps/" client-name ".edn")
              ;fpath (ut/abs-file-path file-path)
          ]
       ;(ut/pp [:saved-snap file-path])
      (spit sess-path session)
          ;(ut/save-base64-to-png image file-path)
      (ut/save-base64-to-jpeg image file-path)
      (ut/save-base64-to-jpeg image ifile-path)
       ;(ut/copy-file file-path ifile-path)
          ;;(save-alert-notification client-name file-path fpath true)
);)
    (catch Exception e (ut/pp [:save-snap-error e])))
  (send-edn-success {:status "snap-saved" :flow (first (get request :edn-params))}))

(defn save-screen-snap [request]
  (try
    ;(time
    (let [image (get-in request [:edn-params :image])
          ;session (get-in request [:edn-params :session])
          screen-name (get-in request [:edn-params :screen-name] "unknown")
          ;;screen-name (cstr/replace (str screen-name) ":" "")
          file-path (str "./screen-snaps/" screen-name ".jpg")
          ifile-path (str "/home/ryanr/rvbbit/frontend/resources/public/screen-snaps/" screen-name ".jpg")]
      (ut/save-base64-to-jpeg image file-path)
      (ut/save-base64-to-jpeg image ifile-path));)
    (catch Exception e (ut/pp [:save-snap-error e])))
  (send-edn-success {:status "screen-snap-saved" :flow (first (get request :edn-params))}))

(defn save-flow [request]
  (time (do
          (let [screen-name (get-in request [:edn-params :flow-id]) ;(ut/generate-name) ; (get-in request [:edn-params :screen-name])
                client-name (get-in request [:edn-params :client-name] "unknown")
                file-base-name (ut/sanitize-name (str screen-name))
                file-path (str "./flows/" file-base-name ".edn")
                fpath (ut/abs-file-path file-path)]
            (save-alert-notification-pre client-name screen-name nil false)
            (do (ut/pp [:saved-flow file-path])
               ;(ut/pretty-spit file-path (get-in request [:edn-params :image])) ;; file looks good, but SO fuggin slow
                (ut/pretty-spit file-path (get-in request [:edn-params :image]) 125)
                ;(spit file-path (get-in request [:edn-params :image]))

         ; (async/go
         ;   (async/<! (ut/pretty-spit file-path (get-in request [:edn-params :image])))
         ;   ;(ut/ppln [:finished :prety-spit-fn])
         ;   )
                (save-alert-notification client-name screen-name fpath true)))))

   ; (let [flow_name (get-in request [:edn-params :flowset-name])
   ;       file-name (clojure.string/replace (str flow_name ".rabbit") " " "_")]
   ;   (send-file-success {:status "saved" :flowset (first (get request :edn-params))} file-name))


  (send-edn-success {:status "saved" :flow (first (get request :edn-params))}))


;(async/go
;  (async/<! (ut/pretty-spit file-path (get-in request [:edn-params :image])))
;  (ut/ppln [:finished :prety-spit-fn]))


(defn load-screen [request]
  ;(ut/pp [:load? request])
  (let [file-path (or (get-in request [:edn-params :file-path])
                      (get-in request [:query-params :file-path]))
        flow-data-file (edn/read-string (slurp file-path))]
    (ut/ppln [:loading-screen-from-file file-path])
    (send-edn-success {:image flow-data-file})))

(defn load-flow [request]
  ;(ut/pp [:load-flow? request])
  (let [file-path (or (get-in request [:edn-params :file-path])
                      (get-in request [:query-params :file-path]))
        flow-data-file (edn/read-string (slurp file-path))]
    (ut/ppln [:loading-flow-from-file file-path])
    (send-edn-success {:image flow-data-file})))

;; (str "./flow-history/" run-id ".edn")

(defn load-flow-history [request]
  (let [run-id (str (or (get-in request [:edn-params :run-id])
                        (get-in request [:query-params :run-id])))
        start-ts (str (or (get-in request [:edn-params :start-ts])
                          (get-in request [:query-params :start-ts])))
        runner? (true? (or (get-in request [:edn-params :runner?])
                           (get-in request [:query-params :runner?])))
        runner? (or (= start-ts "null") runner?)
        running? (true? (when runner? (get-in (flow-statuses) [run-id :*running?])))
        last-run-id (when runner? (get @latest-run-id run-id))
        start-ts (-> (str start-ts) (cstr/replace ":" "") (cstr/replace " " "-"))
        run-id (if (and runner? (not running?)) last-run-id run-id)
        file-path (str "./flow-history/" run-id ".edn")
        _ (ut/pp [:load-flow-history file-path (or (empty? start-ts) runner?) start-ts run-id])
        flow-data-file (edn/read-string (slurp file-path))
        flow-id (get flow-data-file :flow-id)
        hist-flow-id (if runner? flow-id (str flow-id "-SHD-" start-ts))
        flow-data-file (if runner? flow-data-file ;; literal null from JSON conversion. odd.
                           (walk/postwalk-replace {flow-id hist-flow-id} flow-data-file))
        flow-data-file (if (and runner? running?) ;; add some history if we have it, since its still running
                         (merge flow-data-file {:return-maps (select-keys @flow-db/results-atom [flow-id])
                                                :tracker-history (ut/accumulate-unique-runs (get @tracker-history flow-id []))})
                         flow-data-file)]
    (ut/ppln [:loading-flow-history-from-file (when runner? :as-runner!) file-path flow-id hist-flow-id])
    (send-edn-success flow-data-file)))


;(rinst/instrument websocket-server)


(defn jvm-memory-used []
  (let [mm (int (Math/floor (/ (float (/ (- (-> (java.lang.Runtime/getRuntime) (.totalMemory))
                                            (-> (java.lang.Runtime/getRuntime) (.freeMemory))) 1024)) 1024)))]
    (ut/pp ["     " :jvm-stats :*cached-queries (ut/nf (count @sql-cache)) :*jvm-memory-used (ut/nf mm) :mb])))

;; (defn stringify-except [m exclude-keys]
;;   (reduce (fn [acc k]
;;             (if (not (contains? (set exclude-keys) k))
;;               (let [serializer (if (or (map? acc) (vector? acc)) pr-str str)]
;;                 (update acc k serializer))
;;               acc))
;;           m
;;           (keys m)))

(defn stringify-except [m exclude-keys]
  (reduce (fn [acc k]
            (if (not (contains? (set exclude-keys) k))
              (let [v (get m k) ;; double quoted strings in SQL is not ideal
                    serializer (if (or (map? v) (vector? v)) pr-str str)]
                (update acc k (fn [_] (serializer v))))
              acc))
          m
          (keys m)))

(defn strunc [s]
  (let [s (str s)
        limit 80]
    (if (and s (> (count s) limit))
      (subs s 0 limit)
      s)))

(defn param-sql-sync [] ;; placeholder function, will not scale - output is correct however
  (let [display-name (fn [x & [num]] (cstr/join " " (vec (drop (or num 2) x))))
        flow-rows (vec (filter #(and (= (count %) 2) (not= (second %) :opts-map)
                                     (not (cstr/includes? (str (get % 1 "")) "/")) ;; debatable
                                     )
                               (ut/keypaths @flow-db/results-atom)))
        flow-rows (for [e flow-rows]
                    [(str (first e))
                     "flow-values"
                     (cstr/replace (str (first e)) ":" "")
                     (str ":flow/" (cstr/replace (cstr/join ">" e) ":" ""))
                     nil
                     (strunc (ut/replace-large-base64 (get-in @flow-db/results-atom e)))
                     ;(str (vec (drop 1 e)))
                     (display-name e 1)]
                    ;(conj e (str ":flow/" (cstr/replace (cstr/join ">" e) ":" "")))
                    )
        param-rows (filter #(and (not (cstr/includes? (str %) ">"))
                                 (not (cstr/includes? (str %) "-sys"))
                                 (not (cstr/includes? (str %) ":sys"))
                                 (not (cstr/includes? (str %) ":theme")))
                           (ut/keypaths2 @params-atom))
        param-rows (vec (distinct (map (fn [x] (vec (take 3 x))) param-rows)))
        live-clients (keys (client-statuses))
        param-rows (for [e param-rows]
                     [(cstr/replace (str (first e)) ":" "")
                      "client-params"
                      (cstr/replace (str (second e)) ":" "")
                      (str ":client/" (cstr/replace (cstr/join ">" e) ":" ""))
                      (true? (some #(= (first e) %) live-clients))
                      (strunc (ut/replace-large-base64 (get-in @params-atom e)))
                      ;(str (vec (drop 2 e)))
                      (display-name e)
                      nil]
                     ;(conj (conj e (str ":client/" (cstr/replace (cstr/join ">" e) ":" ""))) (some #(= (first e) %) live-clients))
                     )
        panel-rows (vec (filter #(and
                                  (or (= (get % 2) :views)
                                      (= (get % 2) :queries))
                                  ;(= (count %) 4)
                                  )(ut/kvpaths @panels-atom)))
        panel-rows (vec (distinct (map (fn [x] (vec (take 4 x))) panel-rows)))
        panel-rows (vec (filter #(= (count %) 4) panel-rows)) ;; hacking - merge all this once logic gets solidified
        panel-rows (for [e panel-rows
                        ;;  :let [block? (or (= (last e) :views)
                        ;;                   (= (last e) :queries))]
                         :let [param? (= :param (str (get e 2)))]]
                     [(cstr/replace (str (first e)) ":" "")
                      ;;(str "client-" (if block? "panels" (cstr/replace (str (get e 3)) ":" "")))
                      (str "client-" (cstr/replace (str (get e 2)) ":" ""))
                      (cstr/replace (str (second e)) ":" "")
                      (str ":panel/" (cstr/replace (cstr/join ">" e) ":" ""))
                      (true? (some #(= (first e) %) live-clients))
                      (strunc (ut/replace-large-base64 (get-in @panels-atom e)))
                      (display-name e 3)
                      (when (not param?) (try (str (ut/replace-large-base64 (-> (get-in @panels-atom (vec (drop-last (drop-last e)))) (dissoc :views) (dissoc :root)
                                                                                (assoc :natural-key (get e 3))
                                                                                (dissoc :tab) (dissoc :queries)))) (catch Exception ee (str ee))))
                      ;(when (not param?) (str (vec (drop-last e))))
                      ])
        block-rows  (vec (filter #(and (or (= (count %) 5)
                                           (or (= (last %) :views)
                                               (= (last %) :queries)))
                                       (or (= (get % 3) :views)
                                           (= (get % 3) :queries))
                                       (= (second %) :panels))
                                 (ut/kvpaths @screens-atom)))
        block-rows (for [e block-rows
                         :let [block? (or (= (last e) :views)
                                          (= (last e) :queries))
                               e (if block? (drop-last e) e)
                               sample (if (not block?)
                                        (try (ut/replace-large-base64 (-> (get-in @screens-atom (vec (drop-last (drop-last e)))) (dissoc :views) (dissoc :root) (dissoc :tab) (dissoc :queries)
                                                                          (assoc :natural-key (if block? (get e 3) (get e 4)))))
                                             (catch Exception ee (str ee)))
                                        (try (ut/replace-large-base64 (-> (get-in @screens-atom (vec e)) (dissoc :views) (dissoc :root)
                                                                          (assoc :natural-key (last e))
                                                                          (dissoc :tab) (dissoc :queries)))
                                             (catch Exception ee (str ee))))]]
                     [(cstr/replace (str (first e)) ":" "")
                      ;;;(str "::" (last e)) ;; 
                      (str "saved-" (if block? "block" (cstr/replace (str (get e 3)) ":" "")))
                      (cstr/replace (if block?
                                      (str (get sample :name (str (get e 3)))) ;; (str (get e 3))
                                      (str (get e 4))) ":" "")
                      (str ":screen/" (cstr/replace (cstr/join ">" e) ":" ""))
                      ;(true? (some #(= (first e) %) live-clients))
                      nil
                      (strunc (ut/replace-large-base64 (get-in @screens-atom e)))
                      ;(str (vec (drop 2 e)))
                      (display-name e)
                      (str sample)
                      ;(when (not block?) (str (vec (drop-last e))))
                      ])
        ;prows (vec (filter #(cstr/includes? (str (get % 6) "") "-preview-") (distinct (into (into (into param-rows flow-rows) block-rows) panel-rows))))
        prows (vec (distinct (into (into (into param-rows flow-rows) block-rows) panel-rows)))
        ;;keys (vec (distinct (map first prows)))
        rows (vec (for [r prows] (zipmap [:item_key :item_type :item_sub_type :value :is_live :sample :display_name :block_meta] r)))
        delete-sql {:delete-from [:client_items] :where [:= 1 1]} ;; (cons :or (vec (for [k keys] [:= :item_key k])))}
        ;insert-sql {:insert-into [:client_items] :values rows}
        ]
    ;; (ut/pp [:panel-rows (take 75 panel-rows)])
    ;;(ut/pp [:block-rows (vec (take 75 block-rows))])
    ;;(ut/pp [:flow-results-rows rows])
    ;; (when (not (empty? rows))
    ;;   (do (sql-exec flows-db (to-sql {:delete-from [:flow_results]}))
    ;;       (sql-exec flows-db (to-sql insert-sql))))
    ;; (ut/pp [flow-rows
    ;;         (take 30 param-rows)
    ;;         keys])
    ;; (ut/pp [:delete delete-sql])
    (enqueue-task3 (fn []
                     (sql-exec system-db (to-sql delete-sql))
                     (doseq [rr (partition-all 50 rows)]
                       (sql-exec system-db (to-sql {:insert-into [:client_items] :values rr})))))))


(defn update-flow-results>sql []
  (let [rows (try (vec (apply concat
                              (for [[flow-id v] (ut/replace-large-base64 (dissoc @flow-db/results-atom "client-keepalive"))]
                                (vec (for [[block_key block_value] v]
                                       (stringify-except
                                        {:flow_id flow-id
                                         :block_key block_key
                                         :block_value (try (subs (str block_value) 0 1000) (catch Exception _ (str block_value)))
                                         :data_type (ut/data-typer block_value)}
                                        [:start :end]))))))
                  (catch Exception e (do (ut/pp [:update-flow-results>sql-error (str e)]) [])))
        ;; rows (vec (for [r rows
        ;;                 :let [v (str (if (> (count (get r :block_value)) 1000) (subs (get r :block_value) 0 1000) (get r :block_value)))]]
        ;;             (assoc r :block_value v)))
        ;insert-sql {:insert-into [:flow_results] :values (vec rows)}
        ]
    ;(ut/pp [:flow-results-rows rows])
    (when (ut/ne? rows)
      (sql-exec flows-db (to-sql {:delete-from [:flow_results]}))

      ;(sql-exec flows-db (to-sql insert-sql)) ;;; this is dumb. TODO - worked earlier, but won't at scale unless we are careful wiht maintaining "active" flow roster
      (doseq [chunk (partition-all 50 rows)]
        (sql-exec flows-db (to-sql {:insert-into [:flow_results] :values (vec chunk)}))))))

(defn update-channel-history>sql []
  (let [rows (try (apply concat
                         (for [[flow-id v] (ut/replace-large-base64 (dissoc @flow-db/channel-history "client-keepalive"))]
                           (for [vv v]
                             (stringify-except
                              (walk/postwalk-replace {:data-type :data_type}
                                                     (merge {:flow_id flow-id
                                                             :start_ts (ut/millis-to-date-string (get vv :start))
                                                             :end_ts (ut/millis-to-date-string (get vv :end))} vv))
                              [:start :end]))))
                  (catch Exception e (do (ut/pp [:update-channel-history>sql-error (str e)]) [])))
        le (sql-query flows-db (to-sql {:select [[[:max :end] :last_end]] :from [:channel_history]}))
        lee (get-in le [0 :last_end] 0)
        lee (or lee 0) ;; weird, but sometimes less was nil - TODO
        rows-filtered (vec (filter #(> (get % :end) lee) rows))
        rows-filtered (vec (for [r rows-filtered
                                 :let [v (str (if (> (count (get r :value)) 3000) (subs (get r :value) 0 3000) (get r :value)))]]
                             (assoc r :value v)))
        insert-sql {:insert-into [:channel_history] :values rows-filtered}]
    (when (not (empty? rows-filtered))
      (do ;(ut/pp [:channel-history-added>sql :last-end lee :full (count rows) :filtered (count rows-filtered)])
        (sql-exec flows-db (to-sql insert-sql) :update-channel-history>sql)))))

(defn update-fn-history>sql []
  (let [rows (try (apply concat
                         (for [[flow-id v] (ut/replace-large-base64 (dissoc @flow-db/fn-history "client-keepalive"))]
                           (for [vv v]
                             (stringify-except
                              (walk/postwalk-replace {:from :from_block
                                                      :data-type :data_type}
                                                     (merge {:flow_id flow-id
                                                             :start_ts (ut/millis-to-date-string (get vv :start))
                                                             :end_ts (ut/millis-to-date-string (get vv :end))} vv))
                              [:start :end :elapsed-ms]))))
                  (catch Exception e (do (ut/pp [:update-fn-history>sql-error (str e)]) [])))
        le (sql-query flows-db (to-sql {:select [[[:max :end] :last_end]] :from [:fn_history]}))
        lee (get-in le [0 :last_end] 0)
        lee (or lee 0) ;; weird, but sometimes less was nil - TODO
        rows-filtered (vec (filter #(> (get % :end) lee) rows))
        rows-filtered (vec (for [r rows-filtered
                                 :let [v (if (> (count (get r :value)) 3000) (subs (get r :value) 0 3000) (get r :value))]]
                             (assoc r :value v)))
        insert-sql {:insert-into [:fn_history] :values rows-filtered}]
    (when (not (empty? rows-filtered))
      (do ;(ut/pp [:fn-history-added>sql :last-end lee :full (count rows) :filtered (count rows-filtered)])
        (sql-exec flows-db (to-sql insert-sql) :update-fn-history>sql)))))

(defn update-live-schedules>sql []
  (let [rows (try (vec (for [v @flow-db/live-schedules]
                         (stringify-except v [:start :end])))
                  (catch Exception e (do (ut/pp [:update-flow-results>sql-error (str e)]) [])))
        insert-sql {:insert-into [:live_schedules] :values rows}]
    ;(ut/pp [:live-schedules-rows rows])
    (when (not (empty? rows))
      (do (sql-exec flows-db (to-sql {:delete-from [:live_schedules]}))
          (sql-exec flows-db (to-sql insert-sql))))))

(def checkpoint-atom (atom {}))

(defn execute-if-changed [input-atom f name]
  (let [current-hash (hash @input-atom)]
    (when (not= (get @checkpoint-atom name) current-hash)
      (swap! checkpoint-atom assoc name current-hash)
      (do ;(ut/pp [:changed-flow-atom! :running name])
        (f)))))

(defn flow-atoms>sql []
  ;; (reset! flow-db/channel-history (ut/deep-remove-keys @flow-db/channel-history [:b64_json]))
  ;; (reset! flow-db/results-atom (ut/deep-remove-keys @flow-db/results-atom [:b64_json]))
  ;; (reset! flow-db/fn-history (ut/deep-remove-keys @flow-db/fn-history [:b64_json]))
  (execute-if-changed flow-db/live-schedules update-live-schedules>sql :live-schedules)
  (execute-if-changed flow-db/results-atom update-flow-results>sql :flow-results)
  (execute-if-changed flow-db/channel-history update-channel-history>sql :channel-history)
  (execute-if-changed flow-db/fn-history update-fn-history>sql :fn-history))

;; {:flow-id (get opts :flow-id "unnamed-flow-sched")
;;  :override override
;;  :schedule (if (vector? time-seq1) time-seq1 [:custom-time-fn])
;;  :channel ch}

(defn count-watchers [atom]
  (try
    (let [field (.getDeclaredField clojure.lang.Atom "watches")]
      (.setAccessible field true)
      (-> atom
          (.get field)
          (count)))
    (catch Exception e
      (println "Error counting watchers:" (.getMessage e))
      nil)))

(def channel-counts (atom {}))
;;(def stats-shadow (atom []))
(def stats-shadow (ut/thaw-atom [] "./data/atoms/stats-shadow-atom.edn"))

(defn last-x-items [v x]
  (let [start (max 0 (- (count v) x))]
    (subvec v start)))

(defn average-chunks [data]
  (vec (map (fn [chunk]
              {:mem (float (/ (reduce + (map :mem chunk)) (count chunk)))
               :threads (float (/ (reduce + (map :threads chunk)) (count chunk)))
               :load (float (/ (reduce + (map :load chunk)) (count chunk)))
               :subs (float (/ (reduce + (map :subs chunk)) (count chunk)))})
            (partition 50 50 [] data))))

(defn find-max-key [data]
  (let [max-threads (apply max (map :threads data))
        max-subs (apply max (map :subs data))]
    (if (> max-threads max-subs) :threads :subs)))

(def mps-helper (atom {}))
(def last-stats-row (atom {}))
(def booted (atom nil))

(defn jvm-stats []
  (when (not @shutting-down?)
    (try
      (let [runtime (java.lang.Runtime/getRuntime)
            total-memory (.totalMemory runtime)
            free-memory (.freeMemory runtime)
            used-memory (/ (- total-memory free-memory) (* 1024 1024))
            mm (int (Math/floor used-memory))
            sys-load (ut/get-system-load-average)
            thread-mx-bean (java.lang.management.ManagementFactory/getThreadMXBean)
            thread-count (.getThreadCount thread-mx-bean)
            booted? (= @stats-cnt 0)

        ;; chart-view [:>
        ;;             :ResponsiveContainer
        ;;             {:width "100%" :height :panel-height+50}
        ;;             [:>
        ;;              :BarChart
        ;;              {:data (last-x-items @stats-shadow 30)
        ;;               :margin
        ;;               {:top 5 :bottom 5 :right 30 :left 20}}
        ;;              [:>
        ;;               :CartesianGrid
        ;;               {:strokeDasharray "1 4" :opacity 0.33}]
        ;;              [:> :Tooltip]
        ;;              [:> :XAxis {:dataKey :tick
        ;;                          :hide true}]
        ;;              [:> :YAxis {:yAxisId "left"
        ;;                          ;:hide true
        ;;                          :dataKey :mem}]
        ;;              [:> :YAxis {:yAxisId "right"
        ;;                          ;:hide true
        ;;                          :dataKey :threads
        ;;                          :orientation "right"}]
        ;;              [:>
        ;;               :Bar
        ;;               {:yAxisId "left"
        ;;                ;:hide true
        ;;                :dataKey :mem
        ;;                :stroke :theme/editor-outer-rim-color
        ;;                :fill :theme/editor-outer-rim-color}]
        ;;              [:>
        ;;               :Line
        ;;               {:yAxisId "right"
        ;;                ;:hide true
        ;;                :dataKey :threads
        ;;                ;:stroke :theme/editor-outer-rim-color
        ;;                ;:fill :theme/editor-outer-rim-color
        ;;                }]]]
            ttl (try (apply + (for [[_ v] @atoms-and-watchers] (count (keys v)))) (catch Exception _ -1))
            _ (swap! stats-shadow conj {:mem mm :tick (count @stats-shadow) :threads thread-count :subs ttl :load sys-load})
            chart-view (fn [data]
                         (let [max-key (find-max-key data)]
                           [:>
                            :ResponsiveContainer
                            {:width "100%" :height :panel-height+50}
                            [:>
                             :ComposedChart
                             {:data data ;(last-x-items @stats-shadow 50)
                              :margin
                              {:top 5 :bottom 5 :right 30 :left 20}}
                             [:>
                              :CartesianGrid
                              {:strokeDasharray "1 4" :opacity 0.33}]
                             [:> :Tooltip {:contentStyle {:backgroundColor "#00000099"}}]
                             [:> :XAxis {:dataKey :tick
                                         :hide true}]
                             [:> :YAxis {:yAxisId "left"
                                         :hide true
                                         :dataKey :mem}]
                             [:> :YAxis {:yAxisId "right"
                                         :hide true
                                         :dataKey max-key ;:threads
                                         :orientation "right"}]
                             [:>
                              :Bar
                              {:yAxisId "left" :dataKey :mem
                               :isAnimationActive false
                               :stroke :theme/editor-outer-rim-color
                               :fill [:string :theme/editor-outer-rim-color "33"]}]
                             [:>
                              :Line
                              {:yAxisId "right"
                               :strokeWidth 4
                               :type "monotone"
                               :dot false
                               :dataKey :threads
                               :isAnimationActive false
                               :stroke :theme/editor-grid-selected-background-color
                               :fill :theme/editor-grid-selected-background-color}]
                             [:>
                              :Line
                              {:yAxisId "right" :strokeWidth 3 :type "monotone" :dot false :dataKey :subs
                               :isAnimationActive false
                               :stroke :theme/block-tab-selected-font-color
                               :fill :theme/block-tab-selected-font-color
                               :strokeDasharray "5 5"}]]]))
            ;; load-view (fn [data]
            ;;             [:>
            ;;              :ResponsiveContainer
            ;;              {:width "100%" :height :panel-height+50}
            ;;              [:>
            ;;               :BarChart
            ;;               {:data data ;(laerror
            ;;                {:strokeDasharray "1 4" :opacity 0.33}]
            ;;               [:> :Tooltip {:contentStyle {:backgroundColor "#00000099"}}]
            ;;               [:> :XAxis {:dataKey :tick
            ;;                           :hide true}]
            ;;               [:> :YAxis {:hide true
            ;;                           :dataKey :load}]
            ;;               [:>
            ;;                :Bar
            ;;                {:dataKey :load
            ;;                 :isAnimationActive false
            ;;                 :stroke :theme/editor-outer-rim-color
            ;;                 :fill [:string :theme/editor-outer-rim-color "33"]}]]])
        ;; long-chart-view [:>
        ;;                  :ResponsiveContainer
        ;;                  {:width "100%" error:height :panel-height+50}
        ;;                  [:>
        ;;                   :BarChart
        ;;                   {:data (last-x-items @stats-shadow 30)
        ;;                    :margin
        ;;                    {:top 5 :bottom 5 :right 30 :left 20}}
        ;;                   [:>
        ;;                    :CartesianGrid
        ;;                    {:strokeDasharray "1 4" :opacity 0.33}]
        ;;                   [:> :Tooltip]
        ;;                   [:> :XAxis {:dataKey :tick :hide true}]
        ;;                   [:>
        ;;                    :Bar
        ;;                    {:dataKey :mem
        ;;                     :stroke :theerror(for [[_ v] @flow-db/channels-atom] (count v))) 0)
            ;; _ (ut/pp [:client-statuses (client-statuses)])
            ack-scoreboardv (into {} (for [[k v] (client-statuses)
                                           :when (not= (get v :last-seen-seconds) -1)]
                                       {k (ut/deselect-keys v [:booted :last-ack :last-push])}))
            ;; _ (ut/pp [:ack-scoreboardv ack-scoreboardv])
            cli-rows (vec (for [[k v] ack-scoreboardv
                                :let [booted (get v :booted-ts)
                                      now (System/currentTimeMillis)
                                      pushed (get v :push)
                                      [last-now last-pushed] (get @mps-helper k)
                                      _ (swap! mps-helper assoc k [now pushed])
                                      recent-messages-per-second (try (Double/parseDouble (format "%.2f" (/ (- pushed last-pushed) (/ (- now last-now) 1000.0)))) (catch Exception _ -1))
                                      uptime-seconds (try (/ (- now booted) 1000.0) (catch Exception _ -1))
                                      msg-per-second (try (Double/parseDouble (format "%.2f" (/ pushed uptime-seconds))) (catch Exception _ -1))
                                      uptime-str (ut/format-duration-seconds uptime-seconds)
                                      _ (swap! ack-scoreboard assoc-in [k :uptime] uptime-str) ;; bad behavior all around, will refactor all this later. i just need to get to release 0 or this will never come out
                                      _ (swap! ack-scoreboard assoc-in [k :messages-per-second] msg-per-second) ;; ^^ this.
                                      _ (swap! ack-scoreboard assoc-in [k :recent-messages-per-second] recent-messages-per-second) ;; ^^ this.
                                      queue-distro (get v :queue-distro)]]
                            (merge {:client-name (str k)
                                    :uptime-seconds uptime-seconds
                                    :messages-per-second msg-per-second
                                    :uptime uptime-str}
                                   (assoc v :queue-distro (pr-str queue-distro)))))
            ;; _ (ut/pp [:cli-rows cli-rows])
            _ (doseq [cli-row cli-rows]
                (swap! params-atom assoc-in [(edn/read-string (get cli-row :client-name)) :stats] cli-row))

            insert-cli {:insert-into [:client_stats] :values cli-rows}
            seconds-since-last (try (/ (- (System/currentTimeMillis) (get @last-stats-row :unix_ms)) 1000) (catch Exception _ -1))
            seconds-since-boot (try (/ (- (System/currentTimeMillis) @booted) 1000) (catch Exception _ -1))
            last-messages (- @all-pushes (get @last-stats-row :messages 0))
            queries-since-last (- (+ @q-calls @q-calls2)
                                  (+ (get @last-stats-row :queries_run 0)
                                     (get @last-stats-row :internal_queries_run 0)))
                                   ;; (- (System/currentTimeMillis) (get @last-stats-row :unix_ms 0))
            as-double (fn [x] (Double/parseDouble (clojure.pprint/cl-format nil "~,2f" x)))
            jvm-stats-vals {:used_memory_mb mm
                            :messages @all-pushes
                            :unix_ms (System/currentTimeMillis)
                            ;;:messages_per_second (try (Double/parseDouble (format "%.2f" (/ @all-pushes seconds-since-boot))) (catch Exception _ -1))
                            :uptime_seconds seconds-since-boot
                            :seconds_since_last_update seconds-since-last
                            :messages_per_second (as-double (try (/ @all-pushes seconds-since-boot) (catch Exception _ -1)))
                            :recent_messages_per_second (as-double (try (/ last-messages seconds-since-last) (catch Exception _ -1)))
                            :recent_queries_run queries-since-last
                            :recent_messages (- @all-pushes last-messages)
                            :recent_queries_per_second (as-double (try (/ queries-since-last seconds-since-last) (catch Exception _ -1)))
                            :queries_per_second (try (/ (+ @q-calls @q-calls2) seconds-since-boot) (catch Exception _ -1))
                            :thread_count thread-count
                            :sql_cache_size (count @sql-cache)
                            :ws_peers (count @wl/sockets)
                            :subscriptions ttl
                            :open_flow_channels (apply + (for [[_ v] (flow-statuses)] (get v :channels-open)))
                            :queries_run @q-calls
                            :internal_queries_run @q-calls2
                            :sniffs_run @cruiser/sniffs
                            :sys_load (as-double sys-load) ;;sys-load to 2 decimal places
                            }
            _ (reset! last-stats-row jvm-stats-vals)
            insert-sql {:insert-into [:jvm_stats]
                        ;:columns [:used_memory_mb :thread_count :sql_cache_size :ws_peers :open_flow_channels :queries_run :internal_queries_run :sniffs_run :sys_load]
                        ;:values [[mm thread-count (count @sql-cache) (count @wl/sockets) -1 @q-calls @q-calls2 @cruiser/sniffs sys-load]]
                        :values [jvm-stats-vals]}
            _ (swap! server-atom assoc :uptime (ut/format-duration-seconds seconds-since-boot))
        ;; _ (ut/pp [:insert-sql insert-sql])
            ]
        (swap! stats-cnt inc)
        (sql-exec system-db (to-sql {:delete-from [:client_stats]}))
        (sql-exec system-db (to-sql insert-cli))
        (sql-exec system-db (to-sql insert-sql))
        ;; (ut/pp [:we-insert?])
    ;(ut/pp @queue-status)
      ;; (ut/pp {:client-sub-queues (into {} (for [[k v] @client-queues]
      ;;                                       ;{k (seq @v)}
      ;;                                       {k (count v)}
      ;;                                       ))}) ;; queue to seq so it is viewable

    ;(ut/pp [:ack-scoreboard @ack-scoreboard])
    ;(ut/pp {:flow-results-atom @flow-db/results-atom})
    ;; (ut/pp {;:flow-chains-completed-atom @flow-db/chains-completed
    ;;         :fn-history @flow-db/fn-history
    ;;         ;:resolved-paths @flow-db/resolved-paths
    ;;         :open-flow-channels (into {} (for [[k v] @flow-db/channels-atom] {k (count v)}))
    ;;         :channel-history @flow-db/channel-history
    ;;         ;:waffle-data @flow-db/waffle-data
    ;;        ; :block-dump @flow-db/block-dump
    ;;         ;:flow-results-sql (update-flow-results>sql)
    ;;         ;:channel-history-sql (update-channel-history>sql)
    ;;         ;:fn-history-sql (update-fn-history>sql)
    ;;         })

        (when booted?
      ;(ut/pp [:booted!])
          (println " ")
          (reset! booted (System/currentTimeMillis))
      ;(ut/print-ansi-art "rrvbbit.ans")
          (ut/print-ansi-art "nname.ans")
          (ut/pp [:version 0 :june 2024 "Hi."])
      ;(ut/pp "Hi.")
          (println " "))

        ;; (ut/pp [:client-latency (into {} (for [[k v] @client-latency] {k (vec (take-last 10 v))}))])

        (let [fss (flow-statuses)
              ;fssk (vec (filter #(not (cstr/includes? (str %) "-solver-flow-")) (keys fss)))
              fssk (vec (keys fss))]
          (ut/pp [:flow-status (select-keys fss fssk)]))

      ;; (ut/pp [:processes (into {} (for [[k {:keys [start *running? end]}] @processes]
      ;;                               {k {:time-running (- (or end (System/currentTimeMillis)) start)
      ;;                                   :*running? *running?}}))])

        ;; (ut/pp [:ack-scoreboard ack-scoreboardv])

        (ut/pp [:date-map @time-atom])

        (ut/pp [:sql-errors! {:ttl (count @sql/errors)
                              ;;:freq (frequencies (mapv #(cstr/join (drop 1 (cstr/split (str (first %)) " ")) " ") @sql/errors))
                              :freq (frequencies (mapv first @sql/errors))
                              :freq-db (frequencies (mapv second @sql/errors))}])

    ;; (ut/pp [:atoms-and-watchers (for [[k v] @atoms-and-watchers] {k (count (keys v))})])
      ;; (ut/pp [:atoms-and-watchers (for [[k v] @atoms-and-watchers] {k (vec (keys v))})])
      ;; (ut/pp [:signals-watcher (get @atoms-and-watchers :rvbbit-signals)])
      ;; (ut/pp [:signals-watcher-values (get @last-values-per :rvbbit-signals)])

    ;(ut/pp [:times-atom (into {} (for [[k v] @times-atom] {k [(count v) :samples (int (ut/avg v)) :avg-seconds]}))])
    ;; (ut/pp [:times-atom @times-atom])

    ;(ut/pp [:flow-tracker @flow-db/tracker])

    ;; (ut/pp [:watchers {:screen [(count (keys @screen-child-atoms)) (count (keys @screens-atom))]
    ;;                    :params [(count (keys @param-child-atoms)) (count (keys @params-atom))]
    ;;                    :panels [(count (keys @panel-child-atoms)) (count (keys @panels-atom))]
    ;;                    :flow [(count (keys @flow-child-atoms))]}])

    ;; (ut/pp [:lucene 
    ;;         (search/count-documents-by-type search/idx-path) 
    ;;         (search/count-documents search/idx-path)])

    ;;(ut/pp [:live-schedules (map #(dissoc % :next-times) @flow-db/live-schedules)])
    ;; (ut/pp {:tables {:fn-history (keys (first @flow-db/fn-history))
    ;;                  :channel-history (keys (first @flow-db/channel-history))
    ;;                  :results-atom  (keys (first @flow-db/results-atom))}})

    ;(ut/pp (ut/kvpaths @queue-status))
    ;(ut/pp (filter #(= (count %) 3) (ut/kvpaths @queue-status)))
    ;(ut/pp {:async-tasks-running (filter-not-done @queue-status)})


        (try
          (let [peers (count @wl/sockets)
                uptime-str (ut/format-duration-seconds (ut/uptime-seconds))
                server-subs ttl]
            ;; (swap! server-atom assoc :uptime uptime-str)
            ;; (swap! server-atom assoc :clients peers)
            ;; (swap! server-atom assoc :threads thread-count)
            ;; (swap! server-atom assoc :memory mm)
            (swap! server-atom assoc
                   :uptime uptime-str
                   :clients peers
                   :threads thread-count
                   :memory mm)
            (ut/pp ["     "
                    :jvm-stats
                    {:*cached-queries (count @sql-cache)
                     :clover-sql-training-queries (count (keys @clover-sql-training-atom))
                     :clover-sql-training-enriched (count (keys @clover-sql-enriched-training-atom))
                     :ws-peers peers
                     :sys-load sys-load
                     :cwidth (ut/get-terminal-width)
             ;:uptime-seconds (ut/uptime-seconds)
                     :uptime uptime-str
                     :server-subs server-subs
            ;:live-channels (into {} (for [[k v] @flow-db/channels-atom] {k (count v)}))
                     :*jvm-memory-used [(ut/nf mm) :mb]
                     :*current-threads thread-count}]))
          (catch Throwable e (ut/pp [:printing-shit-error? (str e)])))


      ;; (when
      ;;  (or booted? (zero? (mod @stats-cnt 100)))
      ;;   (doseq [[client-name v] @atoms-and-watchers]
      ;; ;(ut/pp [:watchers (for [[k f] v] [k (first f)])])
      ;;     (let [clients (count @wl/sockets)]
      ;;       (alert! client-name [:v-box
      ;;                            :justify :center
      ;;                            :style {;:margin-top "-6px"
      ;;                                    :opacity 0.7} ;:color (if error? "red" "inherit")}
      ;;                            :children [[:box
      ;;                                        :style {:font-weight 700 :font-size "18px"}
      ;;                                        :child (str "[sys-stats] " client-name)]
      ;;                                       [:box
      ;;                                        :style {:font-weight 700 :font-size "11px" :opacity 0.6}
      ;;                                        :child (str thread-count " threads,  "
      ;;                                                    (ut/nf mm) " MB used on server, "
      ;;                                                    (ut/nf ttl) " active client subs, "
      ;;                                                    clients " client" (when (> clients 1) "s")  " connected")]
      ;;                                       [:box
      ;;                                        :style {:font-weight 700 :font-size "11px" :opacity 0.6}
      ;;                                        :child (str "uptime: " (ut/format-duration-seconds (ut/uptime-seconds)))]
      ;;                                       [:box
      ;;                                        :style {:font-weight 700 :font-size "11px" :opacity 0.6}
      ;;                                        :child (str "you have " (count (keys v)) " (server) watcher subs")]

      ;;                                       (when booted?
      ;;                                         [:box
      ;;                                          :style {:color :theme/editor-outer-rim-color :font-weight 700}
      ;;                                          :child
      ;;                                      ;[:speak-always (str "Hello. R-V-B-B-I-T system is online." )]
      ;;                                          [:box :child (str "Hello. R-V-B-B-I-T system is online.")]])

      ;;                                 ;; [:box
      ;;                                 ;;  :style {:font-weight 700 :font-size "11px" :opacity 0.6}
      ;;                                 ;;  :child (str "internal-watchers-count " (count-watchers flow-db/results-atom) )]
      ;;                                       ]]10 (if booted? 2.2 1.7) 6)
      ;;       (alert! client-name (load-view  (last-x-items (average-chunks @stats-shadow) 10)) 10 4 5)
      ;;       (alert! client-name (load-view  (last-x-items @stats-shadow 50)) 10 4 5)
      ;;       (alert! client-name (chart-view (last-x-items (average-chunks @stats-shadow) 10)) 10 4 5)
      ;;       (alert! client-name (chart-view (last-x-items @stats-shadow 50)) 10 4 5))))


    ;(ut/pp [:watchers (for [[k f] @atoms-and-watchers] [k (first f)])])
  ;(ut/pp [:watchers @atoms-and-watchers])
        )
      (catch Exception e (ut/pp [:jvm-stats (str e)])))))

;; (def ring-options
;;   {:port                 websocket-port
;;    :join?                false
;;    :async?               false ; change this to false
;;    :max-threads          100 ; reduce this number
;;    :websockets           ws-endpoints
;;    :allow-null-path-info true
;;    :max-idle-time        30000 ; add this line to set the maximum idle time for WebSocket connections
;;    :max-text-message-size 8192}) ; add this line to set the maximum message size








;; (defn websocket-server []
;;   (jetty/run-jetty #'web-handler ring-options))


;; (defonce websocket-server (atom nil))

;; (defn create-websocket-server! []
;;   ;(ut/ppa [:CALLED!])
;;   (when (nil? @websocket-server)
;;     (reset! websocket-server (jetty/run-jetty #'web-handler ring-options)))
;;   (.start @websocket-server))

;; (defn websocket-server []
;;   (jetty/run-jetty web-handler ring-options))

;; (defn create-websocket-server! []
;;   (ut/ppa [:starting-websocket-server :port websocket-port])
;;   (.start (websocket-server)))


;;; ##############################

;; (defonce websocket-server (delay (do (Thread/sleep 30000) (jetty/run-jetty #'web-handler ring-options))))





;; (defonce websocket-server (atom nil))

;; (defn create-websocket-server-instance []
;;   (ut/ppa [:creating-websocket-server-instance :port websocket-port])
;;   (jetty/run-jetty #'web-handler (assoc ring-options :port websocket-port :join? false :async? true)))

;; (defn create-websocket-server! []
;;   (ut/ppa [:starting-websocket-server :port websocket-port])
;;   (let [server-instance (create-websocket-server-instance)]
;;     (reset! websocket-server server-instance)
;;     (.start @websocket-server) ;; Manually start the server
;;     (ut/ppa [:websocket-server-started :port websocket-port])))

;; (defn destroy-websocket-server! []
;;   (ut/ppa [:shutting-down-websocket-server :port websocket-port])
;;   (when @websocket-server
;;     (.stop @websocket-server)
;;     (reset! websocket-server nil)
;;     (ut/ppa [:websocket-server-stopped :port websocket-port])))


;; (def websocket-port 3030)

;; (def ring-options
;;   {:port                 websocket-port
;;    :join?                false
;;    :async?               true
;;    :max-threads          450
;;    ;; :max-idle-time        10000
;;    :websockets           ws-endpoints
;;    :allow-null-path-info true})

;; (def service-map
;;   (merge ring-options
;;          {:env :prod
;;           ::http/type :jetty
;;           ::http/host "0.0.0.0"
;;           ::http/port  websocket-port
;;           ::http/routes []
;;           ::http/container-options ring-options}))

;; (defonce runnable-wsservice (server/create-server service-map))

;; (def wss-server (atom nil))

;; (defn create-websocket-server! []
;;   (ut/ppa [:starting-websocket-server :port websocket-port])
;;   (reset! wss-server
;;           (server/start runnable-wsservice)))

;; (defn destroy-websocket-server! []
;;   (ut/ppa [:shutting-down-websocket-server :port websocket-port])
;;   (when @wss-server
;;     (server/stop @wss-server)
;;     (reset! wss-server nil)))

;; (defn create-websocket-server! []
;;   (ut/ppa [:starting-websocket-server :port websocket-port])
;;   (reset! websocket-server
;;           (http/start (http/create-server service-map))))

;; (defn destroy-websocket-server! []
;;   (ut/ppa [:shutting-down-websocket-server :port websocket-port])
;;   (when @websocket-server
;;     (http/stop @websocket-server)
;;     (reset! websocket-server nil)))






(defn web-handler [request]
  {:status  200
   :headers {"Content-Type" "text/html"}
   :body    "<html><head></head><body>youre never going to see this, bro</body></html>"})

(def websocket-port 3030)

;; (def ws-endpoints
;;   {"/ws" (try 
;;            (net/websocket-handler {:encoding :edn})
;;            (catch Throwable _ (ut/pp [:websocket-layer-interupppted])))})

(def ws-endpoints
  {"/ws" (net/websocket-handler {:encoding :edn})})

(def ring-options-old
  {:port                 websocket-port
   :join?                false
   :async?               true
   ;; :max-threads          450
   ;; :max-idle-time        10000
   :max-idle-time        5000
   :websockets           ws-endpoints
   :allow-null-path-info true})

;; (def ring-options
;;   {:port                 websocket-port
;;    :join?                false
;;    :async?               true
;;    :min-threads          100 ;; 50
;;    :max-threads          450
;;    :idle-timeout         5000
;;    :queue-size           1000
;;    :max-idle-time        30000
;;    ;:input-buffer-size    8192
;;    ;:output-buffer-size   8192
;;    ;:max-message-size     65536
;;    :input-buffer-size    16384  ;; doubled from 8192 to 16384
;;    :output-buffer-size   16384
;;    :max-message-size     1048576  ;; 1MB - only like 1% of messages
;;    :websockets           ws-endpoints
;;    :allow-null-path-info true})

(def ring-options
  {:port                 websocket-port
   :join?                false
   :async?               true
   :min-threads          300
   :max-threads          1000  ;; Increased max threads
   :idle-timeout         10000  ;; Increased idle timeout
   :queue-size           5000  ;; Increased queue size
   :max-idle-time        60000  ;; Increased max idle time
   :input-buffer-size    32768  ;; Increased buffer sizes
   :output-buffer-size   32768
   :max-message-size     2097152  ;; Increased max message size
   :websockets           ws-endpoints
   :allow-null-path-info true})


;; (defonce websocket-server (jetty/run-jetty #'web-handler ring-options))






;; (defn create-websocket-server! []
;;   (ut/ppa [:starting-websocket-server :port websocket-port])
;;   (.start websocket-server))



;; (defn create-websocket-server! []
;;   (try 
;;     (letfn [(handler [request respond raise] (respond {:status 404}))]
;;     (jetty/run-jetty (fn [& args] (apply handler args)) ring-options))
;;     (catch Throwable e (ut/pp [:wss (str e)]))))



(defonce websocket-server (atom nil))

;; (defn start-server []
;;   (jetty/run-jetty #'web-handler ring-options))

;; (defn create-websocket-server! []
;;   (ut/ppa [:starting-websocket-server :port websocket-port])
;;   (reset! websocket-server (start-server)))

;; (defn destroy-websocket-server! [] @websocket-server)

;; (defn destroy-websocket-server! []
;;   (ut/ppa [:shutting-down-websocket-server :port websocket-port])
;;   (.stop @websocket-server)
;;   (let [timeout-ms 10000
;;         start-time (System/currentTimeMillis)]
;;     (while (and (.isRunning @websocket-server)
;;                 (< (- (System/currentTimeMillis) start-time) timeout-ms))
;;       (Thread/sleep 300))
;;     (when (.isRunning @websocket-server)
;;       (ut/ppa [:killing-websocket-server! (format "Forcefully stopping websocket server @ %d after timeout" websocket-port)])
;;       (.destroy @websocket-server))))
;; #######

(defn destroy-websocket-server! []
  (ut/ppa [:shutting-down-websocket-server :port websocket-port])
  (try
    (do
      (when @websocket-server
        (.stop @websocket-server)
        @websocket-server
    ;; (.destroy @websocket-server)
        (reset! websocket-server nil))
      @websocket-server)
    (catch Throwable _ nil)))


;; (defn create-jetty-server []
;;   (let [server (Server. websocket-port)
;;         context (ServletContextHandler. ServletContextHandler/NO_SESSIONS)]
;;     (.setContextPath context "/")
;;     (.addServlet context (ServletHolder. (jetty/proxy-handler #'web-handler)) "/")
;;     (.setHandler server context)
;;     server))

;; (defonce websocket-server (atom nil))

;; (defn create-websocket-server! []
;;   (ut/ppa [:starting-websocket-server :port websocket-port])(def ws-endpoints
  ;; {"/ws" (net/websocket-handler {:encoding :edn})})

;; (defn destroy-websocket-server! []
;;   (ut/ppa [:shutting-down-websocket-server :port websocket-port])
;;   (when @websocket-server
;;     (.stop @websocket-server)
;;     (reset! websocket-server nil)
;;     (ut/ppa [:websocket-server-stopped :port websocket-port])))





;; (defonce websocket-server (atom nil))

;; (defn create-websocket-server! []
;;   (ut/ppa [:starting-websocket-server :port websocket-port])
;;   (reset! websocket-server (jetty/run-jetty #'web-handler ring-options)))

;; (defn create-websocket-server! []
;;   (ut/ppa [:starting-websocket-server :port websocket-port])
;;   (reset! websocket-server (http/create-server (assoc ring-options :io.pedestal.http/routes #'web-handler)))
;;   (http/start @websocket-server))



;; (defn create-websocket-server! []
;;   (ut/ppa [:starting-websocket-server :port websocket-port])
;;   (reset! websocket-server (server/create-server (assoc ring-options :io.pedestal.http/routes #'web-handler)))
;;   (http/start @websocket-server))


;; (defn destroy-websocket-server! []
;;   (ut/ppa [:shutting-down-websocket-server :port websocket-port])
;;   (.stop websocket-server)
;;   (let [timeout-ms 10000
;;         start-time (System/currentTimeMillis)]
;;     (while (and (.isRunning websocket-server)
;;                 (< (- (System/currentTimeMillis) start-time) timeout-ms))
;;       (Thread/sleep 300))
;;     (when (.isRunning websocket-server)
;;       (ut/ppa [:killing-websocket-server! (format "Forcefully stopping websocket server @ %d after timeout" websocket-port)])
;;       (.destroy websocket-server))))


;;; ##############################



;; (defn restart-websocket-server! []
;;   (let [cache-size (count @sql-cache)]
;;     (jvm-memory-used)
;;     (ut/ppa [:resstarting-websocket-server :port websocket-port])
;;     (.stop websocket-server)
;;     (.start websocket-server)))

;;  (rvbbit-backend.websockets/create-websocket-server!)
;;  (rvbbit-backend.websockets/create-web-server!)

;;; simple pedestal cfg for serving static SPA re-frame root...
(defn home-page [request] (ring-resp/response "Hello World! Home!"))
(defn static-root [request] (ring-resp/content-type (ring-resp/resource-response "index.html" {:root "public"}) "text/html"))
(def common-interceptors [(body-params/body-params) http/html-body])

(def routes #{["/" :get (conj common-interceptors `static-root)]
              ["/save" :post (conj common-interceptors `save)]
              ["/save-flow" :post (conj common-interceptors `save-flow)]
              ["/save-snap" :post (conj common-interceptors `save-snap)]
              ["/save-screen-snap" :post (conj common-interceptors `save-screen-snap)]
              ["/save-csv" :post (conj common-interceptors `save-csv)]
              ["/load" :get (conj common-interceptors `load-screen)]
              ;["/load-session" :get (conj common-interceptors `load-screen)]
              ["/audio" :post (conj common-interceptors `get-audio)]
              ["/load-flow" :get (conj common-interceptors `load-flow)]
              ["/load-flow-history" :get (conj common-interceptors `load-flow-history)]
              ;["/home" :get (conj common-interceptors `home-page)]
              ;;["/ws" :get (conj common-interceptors (ws/connection-fn ws-endpoints))]
              })

(def web-server-port 8888) ;; 8888

(def service {:env :prod
              ::http/routes routes
              ::http/allowed-origins {:creds false :allowed-origins (constantly true)}
              ::http/secure-headers {:content-security-policy-settings {:object-src "none"}}
              ::http/resource-path "/public"
              :max-threads 150
              ::http/type :jetty
              ::http/host "0.0.0.0"
              ::http/port web-server-port
              ;;::http/websockets ws-endpoints ;;{"/ws" (net/websocket-handler {:encoding :edn})}
              ::http/container-options {:h2c? true
                                        :h2? false
                                        :ssl? false}})

(defonce runnable-service (server/create-server service))

(def web-server (atom nil))

(defn create-web-server! []
  (ut/ppa [:starting-web-server :port web-server-port])
  (reset! web-server
          (server/start runnable-service)))

(defn stop-web-server! []
  (ut/ppa [:shutting-down-web-server :port web-server-port])
  (when @web-server
    (server/stop @web-server)
    (reset! web-server nil)))

