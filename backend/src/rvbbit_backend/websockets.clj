(ns rvbbit-backend.websockets
  (:require
    [clojure.core.async        :as    async
                               :refer [<! >! <!! >!! go chan]]
    [websocket-layer.core      :as wl]
    [ring.adapter.jetty9       :as jetty]
    [clojure.pprint            :as pprint]
    [websocket-layer.network   :as net]
    [cognitect.transit         :as transit]
    [nextjournal.beholder      :as beholder]
    [io.pedestal.http          :as http]
    [rvbbit-backend.surveyor   :as surveyor]
    [clojure.java.shell        :as shell]
    [rvbbit-backend.assistants :as assistants]
    [flowmaps.db               :as flow-db]
    [clojure.data              :as data]
    [rvbbit-backend.external   :as ext]
    [rvbbit-backend.evaluator  :as evl]
    [io.pedestal.http.body-params :as body-params]
    [chime.core                :as chime]
    [flowmaps.core             :as flow]
    [rvbbit-backend.embeddings :as em]
    [clj-ssh.ssh               :as ssh]
    [cheshire.core             :as json]
    [ring.util.response        :as ring-resp]
    [io.pedestal.http          :as server]
    [taskpool.taskpool         :as tp]
    [clojure.edn               :as edn]
    [rvbbit-backend.util       :as    ut
                               :refer [ne?]]
    [io.pedestal.http.route    :as route]
    [hikari-cp.core            :as hik]
    [clojure.string            :as cstr]
    [rvbbit-backend.transform  :as ts]
    [rvbbit-backend.pivot      :as pivot]
    [rvbbit-backend.sql        :as    sql
                               :refer [sql-exec sql-query sql-query-one system-db ghost-db flows-db insert-error-row! to-sql
                                       pool-create]]
    [clojure.data.csv          :as csv]
    [csv-map.core              :as ccsv]
    [clojure.core.cache        :as cache]
    [clojure.java.io           :as io]
    [rvbbit-backend.clickhouse-ddl :as clickhouse-ddl]
    [rvbbit-backend.ddl        :as ddl]
    [rvbbit-backend.ddl        :as sqlite-ddl] ;; needed for hardcoded rowset filter fn
    [rvbbit-backend.cruiser    :as cruiser]
    [tea-time.core             :as tt]
    [com.climate.claypoole     :as cp]
    [clj-http.client           :as client]
    [clojure.data.json         :as json2]
    [clojure.set               :as cset]
    [clojure.walk              :as walk]
    [clj-time.coerce           :as coerce]
    [rvbbit-backend.config     :as config]
    [honey.sql                 :as honey])
  (:import ;(clojure.lang MultiFn)
    [com.github.vertical_blank.sqlformatter SqlFormatter]
    [java.util.concurrent                   Executors TimeUnit TimeoutException]
    [java.lang                              ProcessBuilder]
    [java.io                                BufferedReader InputStreamReader]))

(defonce flow-status (atom {}))

(def stats-cnt (atom 0))
(def restart-map (atom {}))
(def orig-caller (atom {}))
(def sub-flow-blocks (atom {}))
(def custom-flow-blocks (ut/thaw-atom {} "./data/atoms/custom-flow-blocks-atom.edn"))
(def screens-atom (ut/thaw-atom {} "./data/atoms/screens-atom.edn"))
(def server-atom (ut/thaw-atom {} "./data/atoms/server-atom.edn"))

(def last-signals-atom (ut/thaw-atom {} "./data/atoms/last-signals-atom.edn"))
(defonce signal-parts-atom (atom []))
(def last-signals-history-atom (ut/thaw-atom {} "./data/atoms/last-signals-history-atom.edn"))
(def last-signal-value-atom (ut/thaw-atom {} "./data/atoms/last-signal-value-atom.edn"))
(def last-signals-atom-stamp (ut/thaw-atom {} "./data/atoms/last-signals-atom-stamp.edn"))
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
(def last-solvers-data-atom (ut/thaw-atom {} "./data/atoms/last-solvers-data-atom.edn"))
(def last-solvers-atom-meta (ut/thaw-atom {} "./data/atoms/last-solvers-atom-meta.edn"))
(def last-solvers-history-atom (ut/thaw-atom {} "./data/atoms/last-solvers-history-atom.edn"))

(def autocomplete-view-atom (ut/thaw-atom [] "./data/atoms/autocomplete-view-atom.edn"))
(def autocomplete-clover-param-atom (ut/thaw-atom [] "./data/atoms/autocomplete-clover-param-atom.edn"))

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

(defn enqueue-task [task] (.put task-queue task))

(defn worker-loop [] (loop [] (when @running (let [task (.take task-queue)] (task)))) (recur))

(defn start-worker [] (ut/pp [:starting-sync-worker-thread 1]) (reset! running true) (reset! worker (future (worker-loop))))

(defn stop-worker
  []
  (reset! running false)
  (when-let [w @worker]
    (future-cancel w)
    (while (not (.isDone w)) ; Ensure the future is cancelled before proceeding
      (Thread/sleep 60))))

(defn recycle-worker [] (reset! stats-cnt 0) (stop-worker) (start-worker))

(def task-queue2 (java.util.concurrent.LinkedBlockingQueue.))
(def running2 (atom true))
(def worker2 (atom nil)) ; Holds the future of the worker thread

(defn enqueue-task2 [task] (.put task-queue2 task))

(defn worker-loop2 [] (loop [] (when @running2 (let [task (.take task-queue2)] (task)))) (recur))

(defn start-worker2 [] (ut/pp [:starting-sync-worker-thread 2]) (reset! running2 true) (reset! worker2 (future (worker-loop2))))

(defn stop-worker2
  []
  (reset! running2 false)
  (when-let [w @worker2]
    (future-cancel w)
    (while (not (.isDone w)) ; Ensure the future is cancelled before proceeding
      (Thread/sleep 60))))

(defn recycle-worker2 [] (stop-worker2) (start-worker2))

(def task-queue3 (java.util.concurrent.LinkedBlockingQueue.))
(def running3 (atom true))
(def worker3 (atom nil)) ; Holds the future of the worker thread

(defn enqueue-task3 [task] (.put task-queue3 task))

(defn worker-loop3 [] (loop [] (when @running3 (let [task (.take task-queue3)] (task)))) (recur))

(defn start-worker3 [] (ut/pp [:starting-sync-worker-thread 3]) (reset! running3 true) (reset! worker3 (future (worker-loop3))))

(defn stop-worker3
  []
  (reset! running3 false)
  (when-let [w @worker3]
    (future-cancel w)
    (while (not (.isDone w)) ; Ensure the future is cancelled before proceeding
      (Thread/sleep 60))))

(defn recycle-worker3 [] (stop-worker3) (start-worker3))

(def task-queue3a (java.util.concurrent.LinkedBlockingQueue.))
(def running3a (atom true))
(def worker3a (atom nil)) ; Holds the future of the worker thread

(defn enqueue-task3a [task] (.put task-queue3a task))

(defn worker-loop3a [] (loop [] (when @running3a (let [task (.take task-queue3a)] (task)))) (recur))

(defn start-worker3a
  []
  (ut/pp [:starting-sync-worker-thread 3 :a])
  (reset! running3a true)
  (reset! worker3a (future (worker-loop3a))))

(defn stop-worker3a
  []
  (reset! running3a false)
  (when-let [w @worker3a]
    (future-cancel w)
    (while (not (.isDone w)) ; Ensure the future is cancelled before proceeding
      (Thread/sleep 60))))

(defn recycle-worker3a [] (stop-worker3a) (start-worker3a))

(def task-queue4 (java.util.concurrent.LinkedBlockingQueue.))
(def running4 (atom true))
(def workers4 (atom nil)) ; Holds the futures of the worker threads

(defn enqueue-task4 [task] (.put task-queue4 task))

(defn worker-loop4 [] (loop [] (when @running4 (let [task (.take task-queue4)] (task)))) (recur))

(defn start-workers4
  [num-workers]
  (ut/pp [:starting-sync-worker-thread-*pool 4])
  (reset! running4 true)
  (reset! workers4 (doall (map (fn [_] (future (worker-loop4))) (range num-workers)))))


(defn stop-workers4
  []
  (reset! running4 false)
  (doseq [w @workers4]
    (future-cancel w)
    (while (not (.isDone w)) ; Ensure the future is cancelled before proceeding
      (Thread/sleep 60)))
  (while (not (.isEmpty task-queue4)) ; Wait until the task queue is empty
    (Thread/sleep 60)))

(defn recycle-workers4 [num-workers] (stop-workers4) (start-workers4 num-workers))

(def task-queue-sql-meta (java.util.concurrent.LinkedBlockingQueue.))
(def running-sql-meta (atom true))
(def workers-sql-meta (atom nil)) ; Holds the futures of the worker threads

(defn enqueue-task-sql-meta [task] (.put task-queue-sql-meta task))

(defn worker-loop-sql-meta [] (loop [] (when @running-sql-meta (let [task (.take task-queue-sql-meta)] (task)))) (recur))

(defn start-workers-sql-meta
  [num-workers]
  (ut/pp [:starting-sync-worker-thread-*pool :sql-meta])
  (reset! running-sql-meta true)
  (reset! workers-sql-meta (doall (map (fn [_] (future (worker-loop-sql-meta))) (range num-workers)))))


(defn stop-workers-sql-meta
  []
  (reset! running-sql-meta false)
  (doseq [w @workers-sql-meta]
    (future-cancel w)
    (while (not (.isDone w)) ; Ensure the future is cancelled before proceeding
      (Thread/sleep 60)))
  (while (not (.isEmpty task-queue-sql-meta)) ; Wait until the task queue is empty
    (Thread/sleep 60)))

(defn recycle-workers-sql-meta [num-workers] (stop-workers-sql-meta) (start-workers-sql-meta num-workers))

(def task-queue5 (java.util.concurrent.LinkedBlockingQueue.))
(def running5 (atom true))
(def workers5 (atom nil)) ; Holds the futures of the worker threads

(defn enqueue-task5 [task] (.put task-queue5 task))

(defn worker-loop5 [] (loop [] (when @running5 (let [task (.take task-queue5)] (task)))) (recur))


(defn start-workers5
  [num-workers]
  (ut/pp [:starting-sync-worker-thread-*pool 5])
  (reset! running5 true)
  (reset! workers5 (doall (map (fn [_] (future (worker-loop5))) (range num-workers)))))


(defn stop-workers5
  []
  (reset! running5 false)
  (doseq [w @workers5]
    (future-cancel w)
    (while (not (.isDone w)) ; Ensure the future is cancelled before proceeding
      (Thread/sleep 60)))
  (while (not (.isEmpty task-queue5)) ; Wait until the task queue is empty
    (Thread/sleep 60)))

(defn recycle-workers5 [num-workers] (stop-workers5) (start-workers5 num-workers))






(def task-queue5d (java.util.concurrent.LinkedBlockingQueue.))
(def running5d (atom true))
(def workers5d (atom nil)) ; Holds the futures of the worker threads
(defonce desired-workers (atom 0))

(defn enqueue-task5d [task] (.put task-queue5d task))

(defn worker-loop5d
  []
  (loop []
    (when (and @running5d (pos? @desired-workers)) (let [task (.take task-queue5d)] (task)) (swap! desired-workers dec))
    (recur)))

(defn start-workers5d
  [num-workers]
  (ut/pp [:starting-sync-worker-thread-*pool 5 :dynamic])
  (reset! running5d true)
  (swap! desired-workers + num-workers)
  (reset! workers5d (doall (map (fn [_] (future (worker-loop5d))) (range num-workers)))))

(defn stop-workers5d
  []
  (reset! running5d false)
  (doseq [w @workers5d]
    (future-cancel w)
    (while (not (.isDone w)) ; Ensure the future is cancelled before proceeding
      (Thread/sleep 60)))
  (while (not (.isEmpty task-queue5d)) ; Wait until the task queue is empty
    (Thread/sleep 60)))

(defn recycle-workers5d [num-workers] (stop-workers5d) (start-workers5d num-workers))

(defn set-desired-workers [num-workers] (reset! desired-workers num-workers))






(def task-queues-slot (atom {})) ; Holds the queues for each keyword
(def running-slot (atom true))
(def workers-slot (atom {})) ; Holds the futures of the worker threads for each keyword

(defn worker-loop-slot
  [keyword]
  (loop []
    (when @running-slot (let [queue (@task-queues-slot keyword) task (when queue (.take queue))] (when task (task))))
    (recur)))

(defn start-workers-slot
  [keyword num-workers]
  (reset! running-slot true)
  (swap! workers-slot assoc keyword (doall (map (fn [_] (future (worker-loop-slot keyword))) (range num-workers)))))

(defn enqueue-task-slot
  [keyword task]
  (let [queue (or (@task-queues-slot keyword)
                  (do (swap! task-queues-slot assoc keyword (java.util.concurrent.LinkedBlockingQueue.))
                      (@task-queues-slot keyword)))]
    (.put queue task)
    (when (not (@workers-slot keyword)) (start-workers-slot keyword 1))))

(defn stop-workers-slot
  [keyword]
  (reset! running-slot false)
  (doseq [w (@workers-slot keyword)]
    (future-cancel w)
    (while (not (.isDone w)) ; Ensure the future is cancelled before proceeding
      (Thread/sleep 60)))
  (let [queue (@task-queues-slot keyword)]
    (while (and queue (not (.isEmpty queue))) ; Wait until the task queue is empty
      (Thread/sleep 60))))

(defn recycle-workers-slot [keyword num-workers] (stop-workers-slot keyword) (start-workers-slot keyword num-workers))






























(def processes (atom {}))

(defn- read-stream
  [input-stream process-id output-key]
  (let [reader (BufferedReader. (InputStreamReader. input-stream))]
    (loop [line (.readLine reader)]
      (when line (swap! processes update-in [process-id :output output-key] #(str % line "\n")) (recur (.readLine reader))))))

(defn get-output [process-id] (get-in @processes [process-id :output]))


(defn start-process
  [process-id command & [wait? ssh-host ssh-user ssh-pass]]
  (if (not (empty? ssh-host))
    (let [session (ssh/session ssh-host ssh-user ssh-pass)]
      (ssh/with-connection session
                           (let [result   (ssh/ssh session {:cmd (cstr/join " " command)})
                                 end-time (System/currentTimeMillis)]
                             (swap! processes assoc
                               process-id
                               {:output    {:stdout (:out result) :stderr (:err result)}
                                :command   (cstr/join " " command)
                                :*running? false
                                :start     (System/currentTimeMillis)
                                :end       end-time
                                :exit-code (:exit result)}))))
    (let [process-builder (ProcessBuilder. command)
          process         (.start process-builder)]
      (swap! processes assoc
        process-id
        {:process   process
         :output    {:stdout "" :stderr ""}
         :command   (cstr/join " " command)
         :*running? true
         :start     (System/currentTimeMillis)})
      (future (read-stream (.getInputStream process) process-id :stdout))
      (future (read-stream (.getErrorStream process) process-id :stderr))
      (if wait?
        (let [exit-code (.waitFor process)
              end-time  (System/currentTimeMillis)]
          (swap! processes assoc-in [process-id :exit-code] exit-code)
          (swap! processes assoc-in [process-id :end] end-time)
          (swap! processes assoc-in [process-id :*running?] false)
          (get-output process-id))
        process))))

(defn stop-process
  [process-id]
  (when-let [process-info (@processes process-id)]
    (.destroy (:process process-info))
    (swap! processes assoc-in [process-id :*running?] false)))

(defn process-running? [process-id] (get-in @processes [process-id :*running?]))

(defn process-exit-code [process-id] (get-in @processes [process-id :exit-code]))

(defn sql-formatter
  [sql-str]
  (try (let [res (pr-str (SqlFormatter/format sql-str))] (if (nil? res) sql-str res)) (catch Exception _ sql-str)))

(defn flatten-map
  ([m] (flatten-map m '()))
  ([m prefix]
   (if (map? m)
     (reduce-kv (fn [acc k v] (let [new-prefix (conj prefix k)] (merge acc (flatten-map v new-prefix)))) {} m)
     (let [key-str   (clojure.string/join "-" (map name prefix))
           final-key (if (re-matches #"\d.*" key-str) (str "_" key-str) key-str)]
       {(keyword final-key) m}))))

(defn filter-not-done
  [m]
  (into {}
        (group-by first
                  (vec
                    (for [kp (filter #(= (count %) 3) (ut/kvpaths m)) :let [v (get-in m kp)] :when (not (= :done v))] [v kp])))))


(defn clean-key [k] (if (keyword? k) (keyword (clojure.string/replace (name k) #"/" "")) k))

(defn recursive-clean
  [data]
  (cond (map? data)  (into {} (map (fn [[k v]] [(clean-key k) (recursive-clean v)]) data))
        (coll? data) (mapv recursive-clean data)
        :else        data))


(defn http-call
  [req]
  (try
    (let [{:keys [url headers query-params method body file save-to] :or {method :get}} req
          http-method (case method
                        :get     client/get
                        :post    client/post
                        :put     client/put
                        :delete  client/delete
                        :head    client/head
                        :options client/options
                        :patch   client/patch
                        :GET     client/get
                        :POST    client/post
                        :PUT     client/put
                        :DELETE  client/delete
                        :HEAD    client/head
                        :OPTIONS client/options
                        :PATCH   client/patch
                        (throw (IllegalArgumentException. {:error (str "Unknown http method: " method)})))
          body2 (if (nil? body)
                  {:query-params query-params}
                  (if (nil? file)
                    {:body (json/generate-string body)}
                    {:multipart [{:name "file" :content (slurp file) :filename (last (clojure.string/split file #"/"))}
                                 {:name "purpose" :content "assistants"}]}))
          response (try (http-method url (merge {:as (if save-to :byte-array :json) :headers headers :debug false} body2))
                        (catch Exception e {:error (str e) :message (str (.getMessage e)) :msg (str e) :class (str (type e))}))]
      (cond (:error response)                            (do (ut/pp [:http-call-error response :body-sent body2])
                                                             {:error response :message (:error response)})
            (cstr/includes? (str response) "status 400") (do (ut/pp [:http-call-error-400 response :body-sent body2])
                                                             {:error response :message "400 code"})
            :else                                        (if save-to
                                                           (do (spit save-to (:body response)) {:success true :path save-to})
                                                           (let [resp   (recursive-clean (get response
                                                                                              :body
                                                                                              {:success true
                                                                                               :status  (get response :status)}))
                                                                 kp     (ut/kvpaths resp)
                                                                 dumped (str "/home/ryanr/rvbbit-out/b64-decoded-"
                                                                             (rand-int 10000000)
                                                                             ".webp")
                                                                 kps    (first (filter #(cstr/includes? (str %) ":b64_json") kp))
                                                                 b64?   (not (empty? kps))
                                                                 _ (when b64? (ut/save-base64-to-webp (get-in resp kps) dumped))
                                                                 resp   (ut/deep-remove-keys resp [:b64_json])]
                                                             (if b64? ;(not (empty? kp))
                                                               (assoc-in resp kps dumped)
                                                               (let [rr  (recursive-clean resp)
                                                                     rrs (pr-str rr) ;; i hate everyone, why does client-http
                                                                                     ;; keywording
                                                                     tts (cstr/replace rrs #":/" ":")]
                                                                 (edn/read-string tts)))))))
    (catch Exception e {:error (str e) :message (str (.getMessage e)) :msg (str e) :class (str (type e))})))




(defn throwable->serializable-map
  [t]
  {:message     (.getMessage t)
   :class       (str (.getName (.getClass t)))
   :cause       (when-let [cause (.getCause t)] {:message (.getMessage cause) :class (str (.getName (.getClass cause)))})
   :stack-trace (map
                  (fn [ste]
                    {:class (.getClassName ste) :method (.getMethodName ste) :file (.getFileName ste) :line (.getLineNumber ste)})
                  (.getStackTrace t))})

(defn parse-error-body
  [e]
  (try (let [response-body (-> e
                               ex-data
                               :body
                               json2/read-str)]
         {:error (get-in response-body ["error" "message"]) :class (str (type e))})
       (catch Exception _ {:error (str e) :class "ERROR IN PARSE-ERROR-BODY. lol"})))

(defn make-http-call
  [req]
  (try
    (let [{:keys [url headers query-params method body file save-to] :or {method :get}} req
          http-method                                                                   (case method
                                                                                          :get     client/get
                                                                                          :post    client/post
                                                                                          :put     client/put
                                                                                          :delete  client/delete
                                                                                          :head    client/head
                                                                                          :options client/options
                                                                                          :patch   client/patch
                                                                                          :GET     client/get
                                                                                          :POST    client/post
                                                                                          :PUT     client/put
                                                                                          :DELETE  client/delete
                                                                                          :HEAD    client/head
                                                                                          :OPTIONS client/options
                                                                                          :PATCH   client/patch
                                                                                          (throw (IllegalArgumentException.
                                                                                                   {:error
                                                                                                      (str "Unknown http method: "
                                                                                                           method)})))
          body2                                                                         (if (nil? body)
                                                                                          {:query-params query-params}
                                                                                          (if (nil? file)
                                                                                            {:body (json/generate-string body)}
                                                                                            {:multipart
                                                                                               [{:name     "file"
                                                                                                 :content  (slurp file)
                                                                                                 :filename (last
                                                                                                             (clojure.string/split
                                                                                                               file
                                                                                                               #"/"))}
                                                                                                {:name    "purpose"
                                                                                                 :content "assistants"}]}))
          response                                                                      (try (http-method url
                                                                                                          (merge {:as
                                                                                                                    (if save-to
                                                                                                                      :byte-array
                                                                                                                      :json)
                                                                                                                  :headers headers
                                                                                                                  :debug false}
                                                                                                                 body2))
                                                                                             (catch Exception e
                                                                                               {;:error (str e)
                                                                                                :error (parse-error-body e)
                                                                                                :class (str (type e))}))]
      (cond (:error response)                            (do ;(ut/pp [:http-call2-error response
                                                             ;:body-sent body2])
                                                           {:error response :message (:error response)})
            (cstr/includes? (str response) "status 400") (do ;(ut/pp [:http-call2-error-400 response
                                                             ;:body-sent body2])
                                                           {:error response :message "400 code"})
            :else                                        (if save-to
                                                           (do (spit save-to (:body response)) {:success true :path save-to})
                                                           (let [resp (recursive-clean (get response
                                                                                            :body
                                                                                            {:success true
                                                                                             :status  (get response :status)}))
                                                                 rr   (recursive-clean resp)
                                                                 rrs  (pr-str rr) ;; i hate
                                                                                  ;; everyone, why
                                                                                  ;; does
                                                                                  ;; client-http
                                                                                  ;; keywording
                                                                 tts  (cstr/replace rrs #":/" ":")
                                                                 ttsr (edn/read-string tts)]
                                                             ttsr))))
    (catch Exception e ;{:error (str e)
      {;:error (str e)
       :error (parse-error-body e)
       :class (str (type e))})))


(defn- run-task
  [executor task timeout-ms]
  (let [future (.submit executor task)]
    (try (.get future timeout-ms TimeUnit/MILLISECONDS)
         (catch TimeoutException e (println "Task timed out, cancelling!") (.cancel future true)) ; mayInterruptIfRunning
                                                                                                  ; = true
         (catch Exception e (println "Task failed with an uncaught exception:" (.getMessage e)))
         (finally))))

(defmacro timed-expr
  [expr]
  `(let [start#  (System/currentTimeMillis)
         result# ~expr
         end#    (System/currentTimeMillis)]
     {:result result# :elapsed-ms (- end# start#)}))





(def import-db
  {:datasource
     @(pool-create
        {:jdbc-url ;;"jdbc:sqlite:db/csv-imports.db"
           "jdbc:sqlite:file:./db/csv-imports.db?cache=shared&journal_mode=WAL&busy_timeout=5000&locking_mode=NORMAL&mmap_size=268435456"
         :cache    "shared"}
        "imports-db-pool")})


(def cache-db
  {:datasource
     @(pool-create
        {:jdbc-url
           ;;"jdbc:sqlite:file:cachedb?mode=memory&cache=shared&transaction_mode=IMMEDIATE&journal_mode=WAL&auto_vacuum=FULL"
           "jdbc:sqlite:file:./db/cache.db?cache=shared&journal_mode=WAL&busy_timeout=5000&locking_mode=NORMAL&mmap_size=268435456"
         :cache    "shared"}
        "cache-db-pool")})




(def running-system-queries -1) ;;(counter instrument/metric-registry ["queries" "counters" "running-system-queries"]))
(def running-user-queries -1)  ;;(counter instrument/metric-registry ["queries" "counters" "running-user-queries"]))




(defonce deep-run-list (atom []))
(defonce q-calls (atom 0))
(defonce q-calls2 (atom 0))
(defonce literal-data-map (atom {}))
(defonce literal-data-output (atom {}))

(defn insert-rowset-csv ;; [rowset query & columns-vec]
  "takes a 'rowset' (vector of uniform maps) or a vector with a vector of column names
   - inserts it into an in memory SQL db, executes a SQL query on it
   (via a honey-sql map) and returns it"
  [rowset table-name client-name op-name & columns-vec]
  (ut/pp [:importing-csv-to-sql table-name])
  (let [rowset-type     (cond (and (map? (first rowset)) (vector? rowset))       :rowset
                              (and (not (map? (first rowset))) (vector? rowset)) :vectors)
        columns-vec-arg (first columns-vec)
        batch-size      100
        rowset-fixed    (if (= rowset-type :vectors) (vec (for [r rowset] (zipmap columns-vec-arg r))) rowset)
        columns         (keys (first rowset-fixed))
        values          (vec (for [r rowset-fixed] (vals r)))
        ddl-str         (ddl/create-attribute-sample table-name rowset-fixed)
        extra           [ddl-str columns-vec-arg table-name table-name]]
    (sql-exec system-db
              (to-sql {:insert-into [:status]
                       :columns     [:client_name :op_name :status]
                       :values      [[client-name op-name (str "inserting " (ut/nf (count rowset-fixed)) " rows... ")]]}))
    (sql-exec import-db (str "drop table if exists " table-name " ; ") extra)
    (sql-exec import-db ddl-str extra)
    (doseq [batch (partition-all batch-size values)] ; (cp/pdoseq pool
      (dorun (sql-exec import-db (to-sql {:insert-into [(keyword table-name)] :columns columns :values batch}) extra)))
    (ut/pp {:sql-csv-table table-name :rows (count rowset)})))

(defn insert-rowset-old ;; [rowset query & columns-vec]
  "takes a 'rowset' (vector of uniform maps) or a vector with a vector of column names
   - inserts it into an in memory SQL db, executes a SQL query on it
   (via a honey-sql map) and returns it"
  [rowset table-name & columns-vec]
  (try (let [rowset-type     (cond (and (map? (first rowset)) (vector? rowset))       :rowset
                                   (and (not (map? (first rowset))) (vector? rowset)) :vectors)
             columns-vec-arg (first columns-vec)
             db-conn         cache-db ;cruiser/mem-db2
             rowset-fixed    (if (= rowset-type :vectors) (vec (for [r rowset] (zipmap columns-vec-arg r))) rowset)
             columns         (keys (first rowset-fixed))
             values          (vec (for [r rowset-fixed] (vals r)))
             table-name-str  (ut/unkeyword table-name)
             ddl-str         (sqlite-ddl/create-attribute-sample table-name-str rowset-fixed)
             insert-sql      (to-sql {:insert-into [table-name] :columns columns :values values})
             extra           [ddl-str columns-vec-arg table-name table-name-str]]
         (sql-exec db-conn (str "drop table if exists " table-name-str " ; ") extra)
         (sql-exec db-conn ddl-str extra)
         (sql-exec db-conn insert-sql extra)
         (ut/pp [:INSERTED-SUCCESS! (count rowset) :into table-name-str])
         {:sql-cache-table table-name :rows (count rowset)})
       (catch Exception e (ut/pp [:INSERT-ERROR! (str e) table-name]))))

(def transit-file-mapping (ut/thaw-atom {} "./data/atoms/transit-file-mapping-atom.edn"))

(defn write-transit-data
  [data query-key client-name table-name]
  (let [base-dir        "./transit-data"
        client-name-str (cstr/replace (str client-name) ":" "")
        filepath        (str base-dir "/" client-name-str)
        file            (str filepath "/" table-name ".transit")]
    (ut/pp [:save-transit file (count data) :rows])
    (swap! transit-file-mapping assoc [client-name query-key] {:file file :ts (ut/today-yyyymmdd-hhmm)})
    (ext/create-dirs filepath)
    (with-open [out (io/output-stream file)] (transit/write (transit/writer out :msgpack) data))))

(defn insert-rowset
  [rowset table-name keypath client-name & columns-vec]
  (ut/pp [:insert-into-cache-db!! (first rowset) (count rowset) table-name columns-vec])
  (if (ut/ne? rowset)
    (try (let [rowset-type     (cond (and (map? (first rowset)) (vector? rowset))       :rowset
                                     (and (not (map? (first rowset))) (vector? rowset)) :vectors)
               columns-vec-arg (first columns-vec)
               db-conn         cache-db
               rowset-fixed    (if (= rowset-type :vectors) (for [r rowset] (zipmap columns-vec-arg r)) rowset)
               columns         (keys (first rowset-fixed))
               table-name-str  (ut/unkeyword table-name)
               ddl-str         (sqlite-ddl/create-attribute-sample table-name-str rowset-fixed)
               extra           [ddl-str columns-vec-arg table-name table-name-str]]
           ;;(enqueue-task5d (fn [] (write-transit-data rowset-fixed keypath client-name table-name-str)))
           (swap! last-solvers-data-atom assoc keypath rowset-fixed) ;; full data can be clover
           (write-transit-data rowset-fixed keypath client-name table-name-str)
           (sql-exec db-conn (str "drop table if exists " table-name-str " ; ") extra)
           (sql-exec db-conn ddl-str extra)
           (doseq [batch (partition-all 100 rowset-fixed)
                   :let  [values     (vec (for [r batch] (vals r)))
                          insert-sql (to-sql {:insert-into [table-name] :columns columns :values values})]]
             (sql-exec db-conn insert-sql extra))
           (ut/pp [:INSERTED-SUCCESS! (count rowset) :into table-name-str])
           {:sql-cache-table table-name :rows (count rowset)})
         (catch Exception e (ut/pp [:INSERT-ERROR! (str e) table-name])))
    (ut/pp [:cowardly-wont-insert-empty-rowset table-name :puttem-up-puttem-up!])))

(def snap-pushes (atom {}))

(defn insert-rowset-snap
  [rowset table-name keypath client-name & columns-vec]
  (ut/pp [:insert-into-cache-dbss!! (first rowset) (count rowset) table-name columns-vec])
  (if (ut/ne? rowset)
    (try (let [;;rowset-type (cond (and (map? (first rowset)) (vector? rowset)) :rowset
               db-conn        cache-db
               ds             (ut/today-yyyymmdd-hhmm)
               rowset-fixed   (mapv #(assoc % :snapshot_ds ds) rowset)
               _ (ut/pp [:snap-rows2 ds (take 1 rowset) (take 1 rowset-fixed)])
               columns        (keys (first rowset-fixed))
               table-name-str (ut/unkeyword table-name)
               ddl-str        (sqlite-ddl/create-attribute-sample table-name-str rowset-fixed)
               extra          [ddl-str table-name table-name-str]
               same-schema?   (= ddl-str (get @snap-pushes table-name))]
          ;;  (enqueue-task5d (fn [] (write-transit-data rowset-fixed keypath client-name table-name-str)))
           (swap! last-solvers-data-atom assoc keypath rowset-fixed) ;; full data can be clover
           (write-transit-data rowset-fixed keypath client-name table-name-str)
           (when (not same-schema?)
             (sql-exec db-conn (str "drop table if exists " table-name-str " ; ") extra)
             (sql-exec db-conn ddl-str extra))
           (swap! snap-pushes assoc table-name ddl-str)
           (doseq [batch (partition-all 100 rowset-fixed)
                   :let  [values     (vec (for [r batch] (vals r)))
                          insert-sql (to-sql {:insert-into [table-name] :columns columns :values values})]]
             (sql-exec db-conn insert-sql extra))
           (enqueue-task5 (fn []
                            (cruiser/captured-sniff "cache.db"
                                                    db-conn
                                                    db-conn
                                                    cache-db
                                                    (hash rowset-fixed)
                                                    [:= :table-name table-name]
                                                    true
                                                    rowset-fixed)))
           (ut/pp [:SNAP-INSERTED-SUCCESS2! (count rowset) :into table-name-str])
           {:sql-cache-table table-name :rows (count rowset)})
         (catch Exception e (ut/pp [:INSERT-ERROR! (str e) table-name])))
    (ut/pp [:cowardly-wont-insert-empty-rowset table-name :puttem-up-puttem-up!])))




(defn insert-rowset-plus-one ;; [rowset query & columns-vec]
  "takes a 'rowset' (vector of uniform maps) or a vector with a vector of column names
   - inserts it into an in memory SQL db, executes a SQL query on it
   (via a honey-sql map) and returns it"
  [rowset table-name & columns-vec]
  (let [rowset-type     (cond (and (map? (first rowset)) (vector? rowset))       :rowset
                              (and (not (map? (first rowset))) (vector? rowset)) :vectors)
        columns-vec-arg (first columns-vec)
        db-conn         cache-db ;cruiser/mem-db2
        rowset-fixed    (if (= rowset-type :vectors) (vec (for [r rowset] (zipmap columns-vec-arg r))) rowset)
        columns         (keys (first rowset-fixed))
        values          (vec (for [r rowset-fixed] (vals r)))
        table-name-str  (ut/unkeyword table-name)
        ddl-str         (ddl/create-attribute-sample table-name-str rowset-fixed)
        insert-sql      (to-sql {:insert-into [table-name] :columns columns :values values})
        extra           [ddl-str columns-vec-arg table-name table-name-str]]
    (sql-exec db-conn (str "drop table if exists " table-name-str " ; ") extra)
    (sql-exec db-conn ddl-str extra)
    (sql-exec db-conn insert-sql extra)
    {:sql-cache-table table-name :rows (count rowset)}))

(def external-changes (async/chan)) ;; anything we dump on here goes to the client (single client)










(defonce push-queue (atom clojure.lang.PersistentQueue/EMPTY)) ;; needs to be on a PER CLIENT BASIS !!!!! TODO

(defonce tracker-history (atom {}))

(defonce queue-status (atom {}))
(defonce queue-data (atom {}))





(def all-pushes (atom 0))

(defn inc-score!
  [client-name key & [ts?]]
  (swap! all-pushes inc)
  (swap! ack-scoreboard assoc-in
    [client-name key]
    (if ts? [(System/currentTimeMillis) (ut/get-current-timestamp)] (inc (get-in @ack-scoreboard [client-name key] 0)))))

(declare client-statuses)

(defmethod wl/handle-request :ack
  [{:keys [client-name memory flow-subs]}]
  (inc-score! client-name :ack)
  (swap! client-latency assoc
    client-name
    (vec (conj (get @client-latency client-name [])
               (try (- (System/currentTimeMillis) (get @ping-ts client-name)) (catch Exception _ -2)))))
  (let [cstats  (client-statuses)
        ins-sql {:insert-into [:client-memory]
                 :values      [(-> memory
                                   (assoc :latency (get-in cstats [client-name :client-latency]))
                                   (assoc :server_subs (get-in cstats [client-name :server-subs]))
                                   (assoc :client_subs (get-in cstats [client-name :client-subs]))
                                   (assoc :recent-messages-per-second (get-in @ack-scoreboard
                                                                              [client-name :recent-messages-per-second]))
                                   (assoc :messages-per-second (get-in @ack-scoreboard [client-name :messages-per-second]))
                                   (assoc :mem_used_mb (ut/bytes-to-mb (get memory :mem_used))))]}]
    (sql-exec system-db (to-sql ins-sql)))
  (swap! ack-scoreboard assoc-in [client-name :memory] (ut/bytes-to-mb (get memory :mem_used)))
  (swap! ack-scoreboard assoc-in [client-name :client-sub-list] flow-subs)
  (swap! ack-scoreboard assoc-in [client-name :client-subs] (count flow-subs))
  (inc-score! client-name :last-ack true)
  {})


(defmethod wl/handle-request :ack2 [{:keys [client-name body]}] (inc-score! client-name :last-ack true) {})



(defonce client-queues (atom {}))
(defonce client-queues-2 (atom {}))
(defonce client-queues-3 (atom {}))
(defonce client-queue-atoms [client-queues]) ;; [client-queues client-queues-2 client-queues-3 ])

(defonce queue-distributions (atom {}))

(defn new-client
  [client-name]
  (ut/pp [:new-client-is-alive! client-name :opening (count client-queue-atoms) :queues])
  (swap! ack-scoreboard assoc-in [client-name :booted-ts] (System/currentTimeMillis))
  (doseq [cq client-queue-atoms]
    (let [new-queue-atom (atom clojure.lang.PersistentQueue/EMPTY)] (swap! cq assoc client-name new-queue-atom))))



(def valid-groups #{:flow-runner :tracker-blocks :acc-tracker :tracker :alert1})

(defn sub-push-loop
  [client-name data cq sub-name] ;; version 2, tries to remove dupe task ids
  (when (not (get @cq client-name)) (new-client client-name)) ;; new? add to atom, create queue
  (inc-score! client-name :booted true)
  (let [results (async/chan (async/sliding-buffer 300))] ;; was 100, was (async/sliding-buffer
    (try (async/go-loop []
           (async/<! (async/timeout 450)) ;; was 70 ?
           (if-let [queue-atom (get @cq client-name (atom clojure.lang.PersistentQueue/EMPTY))]
             (let [items            (loop [res []]
                                      (if-let [item (ut/dequeue! queue-atom)]
                                        (recur (conj res item))
                                        res))
                   items-by-task-id (group-by :task-id items)
                   latest-items     (mapv (fn [group] (if (contains? valid-groups (first group)) group (last group)))
                                      (vals items-by-task-id))]
               (if (not-empty latest-items)
                 (let [message (if (= 1 (count latest-items)) (first latest-items) latest-items)]
                   (when (async/>! results message) (recur)))
                 (recur)))
             (recur)))
         (catch Throwable e (ut/pp [:subscription-err!! sub-name (str e) data]) (async/go-loop [] (recur))))
    results))


(defmethod wl/handle-subscription :server-push2
  [{:keys [kind client-name] :as data}]
  (sub-push-loop client-name data client-queues kind))



(defn push-to-client
  [ui-keypath data client-name queue-id task-id status & [reco-count elapsed-ms]]
  (enqueue-task-slot
    client-name
    (fn []
      (try
        (let [rr                0 ;(rand-int 3)
              cq                (get client-queue-atoms rr)
              _ (swap! queue-distributions assoc client-name (vec (conj (get @queue-distributions client-name []) rr)))
              client-queue-atom (get @cq client-name)]
          (swap! queue-status assoc-in [client-name task-id ui-keypath] status)
          (swap! queue-data assoc-in [client-name task-id ui-keypath] {:data data :reco-count reco-count :elapsed-ms elapsed-ms})
          (if client-queue-atom
            (do (inc-score! client-name :push)
                (inc-score! client-name :last-push true)
                (swap! client-queue-atom conj
                  {:ui-keypath  ui-keypath
                   :status      status
                   :elapsed-ms  elapsed-ms
                   :reco-count  reco-count
                   :queue-id    queue-id
                   :task-id     task-id
                   :data        [data ;; data is likely needed for :payload and :payload-kp that
                                 (try (get (first reco-count) :cnt) (catch Exception _ reco-count))]
                   :client-name client-name}))
            (let [] ;[new-queue-atom (atom clojure.lang.PersistentQueue/EMPTY)]
              (new-client client-name)
              (push-to-client ui-keypath data client-name queue-id task-id status reco-count elapsed-ms)))) ;)
        (catch Throwable e (ut/pp [:push-to-client-err!! (str e) data]))))));)

























(defn react-to-file-changes
  [{:keys [path type]}]
  (doall
    (let [;file-data (try (edn/read-string (slurp (str path))) (catch Exception e
          splt                (cstr/split (str path) #"/")
          client-name         (keyword (cstr/replace (str (get splt 2)) #".edn" ""))
          root-file           (str "./live/" (ext/fixstr client-name) ".edn")
          tab-name            (str (get splt 3))
          block-name          (str (get splt 4))
          source              (try (read-string (slurp root-file)) (catch Exception e {:invalid-edn! (str e)}))
          name-mapping-raw    (into {} (for [[k v] source] {k (get v :name)}))
          name-mapping        (ut/generate-unique-names name-mapping-raw)
          rev-name-mapping    (ut/reverse-map name-mapping)
          panel-key           (get rev-name-mapping block-name)
          valid-edn?          true
          repacked            (ut/remove-empty-sub-keys (try (ext/repack-block client-name tab-name block-name panel-key)
                                                             (catch Exception e {:repack-err! (str e)})))
          oldpacked           (ut/remove-empty-sub-keys (get source panel-key))
          new-from-server?    (and (nil? oldpacked) (= type :create))
          delete-from-server? (and (nil? repacked) (= type :delete))
          we-did-it?          (= repacked oldpacked) ;; either we have authority over this
          ignore?             (or (cstr/ends-with? path ".swp") (= type :create) (= type :delete) (= (str path) root-file))]
      (when (and new-from-server? false) ;; disable for now... WAAAAAY too fucky.
        (let [;panel-key (if (nil? panel-key) (keyword (str "ren-block-" (rand-int 1234)))
              subd       (str "./live/" (ext/fixstr client-name) "/" tab-name "/")
              curr-names (ext/get-subdirs (cstr/replace subd "//" ""))
              cntsplt    (count splt)
              type-of    (cond (= cntsplt 3) :nothing
                               (= cntsplt 5) :block-rename
                               :else         :?)
              missing    (cset/difference (set (keys rev-name-mapping)) (set curr-names))
              panel-key  (if (and (= 1 (count missing)) (nil? panel-key)) (keyword (first missing)) panel-key)
              block-data (assoc (merge (get repacked panel-key) (dissoc repacked panel-key)) :tab tab-name)]
          (when (= cntsplt 5)                                                                                         ;; block-rename
                                                                                                                      ;; or move
            (do (ut/pp [:create-on-server new-from-server? :pushing-new curr-names (vec missing) subd cntsplt type-of ;;panel-key
                                                                                                                      ;;;we-did-it?
                        {:path (str path) :type type :panel-key panel-key :block-data block-data}])
                (push-to-client [:file-changed]
                                {:path (str path) :type type :panel-key panel-key :block-data block-data}
                                client-name
                                0
                                :file-change
                                :done)))))
      (when (and valid-edn? (not ignore?))
        (do (ut/pp [:changed-file! (str path) type client-name panel-key :splt-cnt (count splt)
                    (when (not we-did-it?)
                      {;:repacked repacked :oldpacked oldpacked
                       :diff (data/diff repacked oldpacked)})])
            (when (not we-did-it?)
              (push-to-client [:file-changed]
                              {:path (str path) :type type :panel-key panel-key :block-data repacked}
                              client-name
                              0
                              :file-change
                              :done)))))))

(defn subscribe-to-session-changes
  []
  (let [file-path0 (str "./live/")]
    (do (ut/pp [:beholder-watching file-path0])
        (beholder/watch #(send-off ext/file-work-agent (react-to-file-changes %)) file-path0))))

(defonce last-panels (atom {}))

(defn hash-objects
  [panels]
  (into {}
        (for [[k v] panels]
          {k {:base    (hash v) ; (hash (-> v (dissoc :views) (dissoc :queries)))
              :views   (into {} (for [[k v] (get v :views {})] {k (hash v)}))
              :queries (into {} (for [[k v] (get v :queries {})] {k (hash v)}))}})))

(defn data-objects
  [panels]
  (into {}
        (for [[k v] panels]
          {k {:base    v ;(-> v (dissoc :views) (dissoc :queries))
              :views   (into {} (for [[k v] (get v :views {})] {k v}))
              :queries (into {} (for [[k v] (get v :queries {})] {k v}))}})))

(def panel-history (agent nil))
(set-error-mode! panel-history :continue)

(defmethod wl/handle-request :current-panels
  [{:keys [panels client-name resolved-panels]}] ;; TODO add these
  (ext/write-panels client-name panels) ;; pushn to file system for beholder cascades
  (swap! panels-atom assoc client-name resolved-panels) ;; save to master atom for reactions,
  (let [;resolved-panels resolved-panels ;; remove _ keys? Panels panels ;; remove _ keys?
        prev-hashes (hash-objects (get @last-panels client-name))
        this-hashes (hash-objects panels)
        diffy       (data/diff this-hashes prev-hashes)
        diffy-kps   (vec (filter #(and (not (= (count %) 1)) (not (= (last %) :views)) (not (= (last %) :queries)))
                           (ut/kvpaths (first diffy))))
        dd          (data-objects panels)
        pdd         (data-objects (get @last-panels client-name))]
    (send panel-history ;; shouldnt the whole thing be in an agent sync block? rapid updates
          (fn [_]
            (let [;dd (data-objects panels)
                  rows          (vec (for [kp   diffy-kps
                                           :let [data  (get-in dd kp)
                                                 pdata (get-in pdd kp)
                                                 pdiff (first (data/diff data pdata))]]
                                       {:kp          (str kp)
                                        :client_name (str client-name)
                                        :data        (pr-str data)
                                        :pre_data    (pr-str pdata)
                                        :diff        (pr-str pdiff)
                                        :diff_kp     (pr-str (ut/kvpaths pdiff))
                                        :panel_key   (str (get kp 0))
                                        :key         (str (get kp 2))
                                        :type        (str (get kp 1))}))
                  ins-sql       {:insert-into [:panel-history] :values rows}
                  board-ins-sql {:insert-into [:board_history] :values [{:client_name (str client-name) :data (pr-str panels)}]}]
              (sql-exec system-db (to-sql board-ins-sql))
              (when (not (empty? rows)) (sql-exec system-db (to-sql ins-sql))))))
    (swap! last-panels assoc client-name panels) ;; lastly update last
  ))

(defn run-shell-command
  "execute a generic shell command and return output as a map of timing and seq of str lines"
  [command]
  (let [output                   (shell/sh "/bin/bash" "-c" (str "mkdir -p shell-root ; cd shell-root ; " command))
        split-lines              (vec (remove empty? (cstr/split-lines (get output :out))))
        exit-code                (get output :exit)
        error                    (vec (remove empty? (cstr/split-lines (get output :err))))
        has-timing?              (if (not (empty? error)) (cstr/starts-with? (get error 0) "real") false)
        error-data               (if has-timing? [] error)
        timing-values-to-seconds #(let [split-timing (cstr/split (get (cstr/split % #"\t") 1) #"m")
                                        minutes      (edn/read-string (get split-timing 0))
                                        seconds      (edn/read-string (cstr/join "" (drop-last (get split-timing 1))))]
                                    (+ (* 60 minutes) seconds))
        timing-data              (if has-timing? (into [] (for [x error] (timing-values-to-seconds x))) [])]
    {;;  :output [{:output split-lines
     :output    split-lines
     :exception error-data
     :seconds   timing-data
     :command   (str command)}))

(defn read-local-file
  [full-path]
  (let [fqd?      (or (cstr/starts-with? full-path "/") (cstr/starts-with? full-path "~"))
        output    (run-shell-command "pwd")
        pwd       (first (get-in output [:output :output] []))
        full-path (if fqd? full-path (str pwd "/" full-path))]
    (ut/pp [:reading-file full-path])
    (try {:file-data (str (slurp full-path)) :error nil}
         (catch Exception e
           {:file-data (str "\n" (str (.getMessage e)) "\n")
            :error     nil ;(str "read-local-file, caught exception: " (.getMessage e))
           }))))

(defn write-local-file
  [full-path file-data]
  (let [fqd?      (or (cstr/starts-with? full-path "/") (cstr/starts-with? full-path "~"))
        output    (run-shell-command "pwd")
        pwd       (first (get-in output [:output :output] []))
        full-path (if fqd? full-path (str pwd "/" full-path))]
    (ut/pp [:writing-file full-path])
    (do (try (spit full-path file-data)
             (catch Exception e
               (do (println "err")
                   {;:file-data file-data
                    :status    :error
                    :file-path full-path
                    :error     (str "caught exception: " (.getMessage e))})))
        {:status :ok :file-path full-path})))


(defn get-audio
  [request]
  (let [jss  (get request :json-params)
        path (get jss :path)
        file (if (clojure.string/starts-with? path "http")
               (let [url          (java.net.URL. path)
                     connection   (.openConnection url)
                     input-stream (.getInputStream connection)
                     file-path    (str "/tmp/" (last (clojure.string/split path #"/")))
                     file         (java.io.File. file-path)]
                 (with-open [output-stream (java.io.FileOutputStream. file)] (clojure.java.io/copy input-stream output-stream))
                 file)
               (java.io.File. (str path)))]
    (if (.exists file)
      (-> (ring-resp/response (java.io.FileInputStream. file))
          (ring-resp/content-type "audio/mpeg"))
      (ring-resp/not-found "File not found"))))

(defmethod wl/handle-request :get-settings
  [{:keys [client-name]}]
  (ut/pp [:client client-name :just-booted])
  (merge config/settings
         {:kits    config/kit-fns
          :screens (vec (map :screen_name
                          (sql-query system-db
                                     (to-sql {:select   [:screen_name]
                                              :from     [[:screens :jj24a7a]]
                                              :group-by [:screen_name]
                                              :order-by [[1 :asc]]}))))}))

(def times-atom (ut/thaw-atom {} "./data/atoms/times-atom.edn"))
(def params-atom (ut/thaw-atom {} "./data/atoms/params-atom.edn"))
(def atoms-and-watchers (atom {}))

(def last-values (ut/thaw-atom {} "./data/atoms/last-values.edn"))
(def last-values-per (ut/thaw-atom {} "./data/atoms/last-values-per.edn"))

(defmethod wl/handle-request :autocomplete
  [{:keys [client-name surrounding panel-key view]}]
  (ut/pp [:get-smart-autocomplete-vector client-name panel-key view {:context surrounding}])
  {:clover-params (conj @autocomplete-clover-param-atom :*) :view-keywords (conj @autocomplete-view-atom :*)})


(defmethod wl/handle-request :client-ui
  [{:keys [client-name atom-name value]}]
  (swap! params-atom assoc-in [client-name (keyword atom-name)] value)
  (ut/pp [:client-ui-atom client-name atom-name value])
  {}) ;; send nothing

(defn client-statuses
  []
  (into {}
        (for [[k v] @ack-scoreboard]
          {k (let [seconds-ago (int (/ (- (System/currentTimeMillis) (get-in v [:last-ack 0] 0)) 1000))
                   never?      (nil? (get-in v [:last-ack 0]))]
               (-> v
                   (assoc :queue-distro (frequencies (get @queue-distributions k)))
                   (assoc :queue-size (count @(get @client-queues k)))
                   (assoc :client-latency (ut/avg (take-last 20 (get @client-latency k []))))
                   (assoc :server-subs (count (keys (get @atoms-and-watchers k))))
                   (assoc :last-seen-seconds (if never? -1 seconds-ago))
                   (dissoc :client-sub-list)
                   (assoc :last-seen (cond (< seconds-ago 0)       "not since boot"
                                           (= k :rvbbit-scheduler) "n/a"
                                           :else                   (ut/format-duration-seconds seconds-ago)))))})));)

(declare flow-kill!)
(declare alert!)

(defn flow-statuses
  []
  (into
    {}
    (for [[k {:keys [*time-running *running? *started-by *finished overrides started waiting-blocks running-blocks]}] @flow-status
          :let [chans               (count (get @flow-db/channels-atom k))
                chans-open          (count (doall (map (fn [[_ ch]]
                                                         (let [vv (try (not (ut/channel-open? ch)) (catch Throwable e (str e)))]
                                                           (if (cstr/includes? (str vv) "put nil on channel") :open vv)))
                                                    (get @flow-db/channels-atom k))))
                channels-open?      (true? (> chans-open 0))
                last-update-seconds (int (/ (- (System/currentTimeMillis) (get @watchdog-atom k)) 1000))
                human-elapsed       (if *running? (ut/format-duration started (System/currentTimeMillis)) *time-running)
                last-update         (get @last-block-written k)
                _ (when (and (> last-update-seconds 240) (empty? running-blocks) (not (nil? last-update)) *running?)
                    (alert! *started-by
                            [:box :style {:color "red"} :child
                             (str "ATTN: flow " k " killed by rabbit watchdog for going idle - TODO")]
                            15
                            1
                            60)
                    (flow-kill! k :rvbbit-watchdog))]]
      {k {:time-running        *time-running
          :*running?           *running?
          :retries-left        (get @restart-map k -1)
          :*started-by         *started-by
          :last-update-seconds (when *running? last-update-seconds)
          :last-updated        (when *running? (ut/format-duration-seconds last-update-seconds))
          :last-update         last-update
          :running-blocks      running-blocks
          :block-overrides     (vec (keys overrides))
          :since-start         (str human-elapsed)
          :waiting-blocks      waiting-blocks
          :channels-open?      channels-open?
          :channels            chans
          :channels-open       chans-open
          :blocks_finished     *finished}})))


(defn flow-waiter
  [in-flow uid & [opts]]
  (let [a         (atom nil)
        post-id   (if (get opts :increment-id? false) ;; if auto increment idx, lets use that
                    (str uid "-" (count (filter #(cstr/starts-with? % uid) (keys @flow-db/channel-history))))
                    uid)
        debug?    false ;true ; false
        overrides (get opts :overrides nil)
        flow-opts (merge {:debug?         debug?
                          :close-on-done? true ; false ;true
                          :flow-id        uid}
                         opts)
        uid       post-id] ;; swap to post-run ID ;; temp
    (doall (flow/flow in-flow flow-opts a overrides))
    (while (and (nil? @a) (not (some #(get % :error) (map :value (get @flow-db/channel-history uid)))))
      (let [;chist (get @flow-db/channel-history uid)
           ]
        (Thread/sleep 100))) ;; check the atom every 100 ms
    (let [err? (some #(get % :error) (map :value (get @flow-db/channel-history uid)))]
      (when err? (flow/close-channels! uid))
      (or @a (some #(get % :error) (map :value (get @flow-db/channel-history uid)))))))

(defn gn [x] (try (name x) (catch Exception _ x)))
(defn gns [x] (try (namespace x) (catch Exception _ x)))

(defmacro create-fn
  [k v]
  (let [str?     (string? v)
        inputs   (get v :inputs)
        syms     (vec (map (fn [_] (gensym)) inputs))
        bindings (into {} (map vector inputs syms))]
    `(def ~k
       (fn ~syms (let [walkmap ~bindings] (if ~str? (ut/template-replace walkmap ~v) (walk/postwalk-replace walkmap ~v)))))))

(declare materialize-flowmap)




(defn process-flowmaps
  [flowmap sub-map]
  (doall
    (let [flowmap-comps
            (into
              {}
              (for [[k v] (get flowmap :components)
                    :let  [;vv v ;; for debugging
                           v (if (get v :flow-path)
                               (materialize-flowmap (get sub-map :client-name) (get v :flow-path) (get v :sub-flow-id) {} true)
                               v)]]
                (walk/postwalk-replace
                  {:block-id k :block-id-str (str k) :bid k :bids (str k)}
                  (cond (not (empty? (get v :raw-fn))) ;; user provided anon fn
                          (let [fn-raw (get v :raw-fn)]
                            {k (-> v
                                   (assoc :fn fn-raw)
                                   (dissoc :raw-fn))})
                        (not (empty? (get v :components))) ;; unpacked subflow! (legacy saved flows
                          {k (assoc (process-flowmaps v sub-map) :description [(get v :file-path nil) (get v :flow-id nil)])} ;; sneaky
                        (and (not (empty? (get v :data))) (not (empty? (get v :inputs)))) ;; templated
                                                                                          ;; input
                                                                                          ;; "static"
                          (let [vv         (get v :data)
                                str?       (string? vv)
                                inputs     (get v :inputs)
                                replace-fn (if str? 'rvbbit-backend.util/template-replace 'clojure.walk/postwalk-replace)
                                fn-form    (walk/postwalk-replace
                                             {:inputs inputs :replace-fn replace-fn :data vv}
                                             '(fn [& args] (let [m (zipmap :inputs args)] (:replace-fn m :data))))]
                            {k {:fn fn-form :inputs inputs}})
                        (and (or (not (map? v)) (and (map? v) (nil? (get v :fn-key)))) ;; static value
                             (empty? (get v :inputs))) ;; no inputs dbl check
                          {k v}
                        :else (let [fn-key (try (vec (conj (vec (get v :fn-key)) :fn)) (catch Exception _ [:issue :oops])) ;; lookup
                                    fn     (get-in cruiser/default-flow-functions fn-key)]
                                {k (-> v
                                       (assoc :fn fn)
                                       (dissoc :fn-key))})))))
          conns (get flowmap :connections)
          gen-comps (atom {})
          conns (ut/flatten-one-level
                  (for [[c1 c2] conns ;; remap mapped outputs with generated GET blocks
                        :let    [mapped? (try (cstr/includes? (str c1) "/") (catch Exception _ false))]]
                    (if mapped?
                      (let [spt      (-> (str c1)
                                         (cstr/replace #":" "")
                                         (cstr/split #"/"))
                            base     (keyword (first spt))
                            kkey     (keyword (last spt))
                            kkey     (if (cstr/starts-with? (str kkey) ":idx") ;; vector get index
                                       (try (edn/read-string (cstr/replace (str kkey) ":idx" "")) (catch Exception _ -1))
                                       kkey)
                            gen-get  (-> (str c1)
                                         (cstr/replace #":" "")
                                         (cstr/replace #"/" "-"))
                            gen-get1 (keyword (str gen-get "/in"))
                            gen-get2 (keyword gen-get)]
                        (if (= kkey :*)
                          [[base c2]] ;; if user wants the whole map instead with :* then just pass it
                          (do (swap! gen-comps assoc
                                gen-get2 ;; swap in the literal, but keep "unread"
                                {:fn (walk/postwalk-replace {:kkey kkey} '(fn [m] (get m :kkey 0))) :inputs [:in]})
                              [[base gen-get1] [gen-get2 c2]])))
                      [c1 c2])))
          flowmap-comps (merge flowmap-comps @gen-comps) ;; if we have to realize one-get hops,
          gen-multi-arities (atom [])
          conns (into []
                      (for [[c1 c2] conns] ;; need to detect multi-aritiy ports and generate the
                        (if (cstr/ends-with? (str c2) "+")
                          (let [spl      (cstr/split (cstr/replace (str c2) ":" "") #"/")
                                bid      (keyword (first spl))
                                cntr     (count (get (group-by first @gen-multi-arities) bid []))
                                new-port (str (last spl) (str cntr))
                                new-c2   (keyword (str (first spl) "/" new-port))
                                _ (swap! gen-multi-arities conj [bid (keyword new-port) new-c2])]
                            [c1 new-c2])
                          [c1 c2])))
          multi-arity-flowmap-comps (into {}
                                          (for [[k v] (group-by first @gen-multi-arities)
                                                :let  [other-inputs (vec (remove #(cstr/ends-with? (str %) "+") ;; for
                                                                           (get-in flowmap-comps [k :inputs] [])))
                                                       inputs       (into other-inputs (vec (map second v)))]]
                                            {k (merge (get flowmap-comps k) {:inputs inputs})}))
          flowmap-comps (merge flowmap-comps multi-arity-flowmap-comps)
          all-inputs (vec (distinct (ut/deep-flatten (for [[k v] flowmap-comps]
                                                       (for [kk (get v :inputs)] (keyword (str (gn k) "/" (gn kk))))))))
          input-refs (distinct (ut/deep-flatten conns))
          uk (fn [x] (cstr/replace (str x) #":" ""))
          uk2 (fn [x]
                (-> (str x)
                    uk
                    (cstr/replace #"/" "")))
          defaults-map (apply merge
                         (for [[k v] (get flowmap :components) ;; get the default values for each
                               :let  [fn-key        (get v :fn-key)
                                      defs          (get-in cruiser/default-flow-functions (vec (conj (vec fn-key) :defaults)))
                                      override-defs (get-in v [:default-overrides] {}) ;; new
                                      defs          (merge defs override-defs)]]
                           (into {} (for [[kk vv] defs] {(keyword (str (uk k) "/" (uk kk))) vv}))))
          defaults-map (walk/postwalk-replace sub-map defaults-map) ;; sub in any run-specific
          missing-inputs (vec (cset/difference (set all-inputs) (set input-refs))) ;; general
          defaults-missing (or (select-keys defaults-map missing-inputs) {}) ;; get the default
          missing-inputs (or (cset/difference (set missing-inputs) (set (keys defaults-missing))) []) ;; remove
          defaults-comps (or (into {} (for [[k v] defaults-missing] {(keyword (uk2 (str k))) v})) {}) ;; create
          defaults-conns (or (vec (for [[k _] defaults-missing] [(keyword (uk2 (str k))) k])) []) ;; create
          nil-inputs-conns (vec (for [i missing-inputs] [:nilly i])) ;; what is left over gets
          finished-flowmap (-> flowmap ;; package that shit together for christmas dinner
                               (assoc :components (merge flowmap-comps {:nilly :skip} defaults-comps))
                               (assoc :connections (vec (remove empty? (into (into conns nil-inputs-conns) defaults-conns)))))]
      finished-flowmap))) ;; tonight we feast on the values!


(defn wrap-payload
  [payload thread-id thread-desc message-name]
  (let [thread-id (ut/keywordize-string thread-id)]
    {thread-id {:data        [{;:ask-mutates
                               :content        (vec (remove #(or (nil? %) (= 0 %)) (first payload)))
                               :name           (str message-name)
                               :order          (ut/unix-timestamp)
                               :parameters     {}
                               :forced-mutates {} ;; TODO, not implemented, but should work
                               :step-mutates   {}}]
                :description (str thread-desc)
                :mutates     {}
                :options     {:actions? false :pages? false :search? false}
                :parameters  {}}}))

(declare insert-kit-data)

(defn replace-keywords
  [sub-task subbed-subs]
  (let [replacements (into {} (map (fn [[k v]] [v k]) subbed-subs))] (map (fn [k] (get replacements k k)) sub-task)))

(defn kick
  [client-name task-id sub-task thread-id thread-desc message-name & args]
  (let [ui-keypath   [:kick] ;;; ^ sub-task is the UI item-key in push ops
        payload      (vec args)
        payload?     (not (empty? payload))
        heartbeat?   (= sub-task :heartbeat)
        payload      (when (and payload? (not heartbeat?)) (wrap-payload payload thread-id thread-desc message-name))
        _ (when (and payload? (not heartbeat?))
            (insert-kit-data payload (hash payload) sub-task task-id ui-keypath 0 client-name "flow-id-here!"))
        data         (merge {:sent! task-id :to client-name :at (str (ut/get-current-timestamp)) :payload payload}
                            (when payload? {:payload-kp [sub-task task-id]}))
        queue-id     -1
        destinations (cond (= client-name :all)   (vec (keys @client-queues))
                           (keyword? client-name) [client-name]
                           :else                  client-name)]
    (doseq [cid destinations]
      (let [hb?      (= sub-task :heartbeat)
            sub-task (if hb?
                       (let [ssubs         (vec (keys (get @atoms-and-watchers cid {})))
                             subbed-subs   (vec (distinct (get @param-var-key-mapping cid [])))
                             sss-map       (into {} (for [[orig subbb] subbed-subs] {subbb orig}))
                             replaced-subs (walk/postwalk-replace sss-map ssubs)]
                         replaced-subs)
                       sub-task)]
        (when hb? (swap! ping-ts assoc cid (System/currentTimeMillis)))
        (push-to-client ui-keypath data cid queue-id task-id sub-task)))
    :sent!))

(defn process-flow-map
  [fmap]
  (into
    {}
    (for [[k v] fmap
          :let  [ttype (get-in v [:data :drag-meta :type])]]
      (cond
        (= ttype :query)      (let [pfull (first (vals (get-in v [:data :queries])))
                                    ddata pfull ;(first @(re-frame/subscribe [::resolver/logic-and-params
                                    dtype (ut/data-typer ddata)
                                    ports {:out {:out (keyword dtype)}}
                                    ports (cond
                                            (= dtype "map") {:out (assoc (into {}
                                                                               (for [[k v] ddata]
                                                                                 {k (keyword (ut/data-typer v))}))
                                                                    :* :map)}
                                            (or (= dtype "vector") (= dtype "rowset")) {:out (assoc (into
                                                                                                      {}
                                                                                                      (for
                                                                                                        [k    (range (count
                                                                                                                       ddata))
                                                                                                         :let [v (get ddata k)]]
                                                                                                        {(keyword (str "idx" k))
                                                                                                           (keyword (ut/data-typer
                                                                                                                      v))}))
                                                                                               :* :vector)}
                                            :else ports)
                                    full  (-> v
                                              (assoc-in [:data :user-input] ddata)
                                              (assoc-in [:ports] ports))]
                                {k full})
        (= ttype :param)      (let [pfull (get-in v [:data :drag-meta :param-full])
                                    ddata pfull ; (first pfull) ;;@(re-frame/subscribe
                                    dtype (ut/data-typer ddata)
                                    ports {:out {:out (keyword dtype)}}
                                    ports (cond (= dtype "map")    {:out (assoc (into {}
                                                                                      (for [[k v] ddata]
                                                                                        {k (keyword (ut/data-typer v))}))
                                                                           :* ddata)}
                                                (= dtype "vector") {:out (assoc (into {}
                                                                                      (for [k    (range (count ddata))
                                                                                            :let [v (get ddata k)]]
                                                                                        {(keyword (str "idx" k))
                                                                                           (keyword (ut/data-typer v))}))
                                                                           :* ddata)}
                                                :else              ports)
                                    full  (-> v
                                              (assoc-in [:data :user-input] ddata)
                                              (assoc-in [:ports] ports))]
                                {k full})
        (= ttype :cell)       (let [pfull (get-in v [:data :drag-meta :param-full])
                                    ddata pfull ;(first pfull) ;(first @(re-frame/subscribe
                                    dtype (ut/data-typer ddata)
                                    ports {:out {:out (keyword dtype)}}
                                    ports (cond (= dtype "map")    {:out (assoc (into {}
                                                                                      (for [[k v] ddata]
                                                                                        {k (keyword (ut/data-typer v))}))
                                                                           :* ddata)}
                                                (= dtype "vector") {:out (assoc (into {}
                                                                                      (for [k    (range (count ddata))
                                                                                            :let [v (get ddata k)]]
                                                                                        {(keyword (str "idx" k))
                                                                                           (keyword (ut/data-typer v))}))
                                                                           :* ddata)}
                                                :else              ports)
                                    full  (-> v
                                              (assoc-in [:data :user-input] ddata)
                                              (assoc-in [:ports] ports))]
                                {k full})
        (= ttype :open-block) (let [pfull (if (= (get-in v [:data :syntax] "clojure") "clojure")
                                            (get-in v [:data :user-input])
                                            (cstr/join "\n" (get-in v [:data :user-input])))
                                    ddata pfull ; (first pfull) ;(first @(re-frame/subscribe
                                    dtype (ut/data-typer ddata)
                                    ports {;:in (get-in v [:ports :in])
                                           :out {:out (keyword dtype)}}
                                    ports (cond (= dtype "map")    {;:in (get-in v [:ports :in])
                                                                    :out (assoc (into {}
                                                                                      (for [[k v] ddata]
                                                                                        {k (keyword (ut/data-typer v))}))
                                                                           :* ddata)}
                                                (= dtype "vector") {;:in (get-in v [:ports :in])
                                                                    :out (assoc (into {}
                                                                                      (for [k    (range (count ddata))
                                                                                            :let [v (get ddata k)]]
                                                                                        {(keyword (str "idx" k))
                                                                                           (keyword (ut/data-typer v))}))
                                                                           :* ddata)}
                                                :else              ports)
                                    ports (merge (get v :ports) ports)
                                    full  (-> v
                                              (assoc-in [:data :user-input] ddata)
                                              (assoc-in [:ports] ports))]
                                {k full})
        :else                 {k v}))))

(declare save!)
(declare ttap>)

(defn process-flowmap2
  [flowmap flowmaps-connections fid]
  (let [canvas-key (into {} (for [[k {:keys [w h x y]}] flowmap] {k {:w w :h h :x x :y y :view-mode "text"}}))
        flowmaps-connections (vec (for [[c1 c2] flowmaps-connections]
                                    (if (cstr/ends-with? (str c1) "/*")
                                      [(keyword (-> (ut/unkeyword (str c1))
                                                    (cstr/replace "/*" "")
                                                    (cstr/replace ":" ""))) c2]
                                      [c1 c2])))
        components-key
          (into
            {}
            (for [[k {:keys [data ports view file-path flow-path raw-fn sub-flow-id flow-id sub-flow]}] flowmap ;; <-- flow-id
                                                                                                                ;; refers to
                                                                                                                ;; the subflow
                                                                                                                ;; embed, not
                                                                                                                ;; the parent
                  :let [ttype       (or (get-in data [:flow-item :type]) (get-in data [:drag-meta :type]))
                        try-read    (fn [x] (try (edn/read-string x) (catch Exception _ x)))
                        view-swap   (fn [obody flow-id bid push-key]
                                      (let [pkey      (keyword (str (cstr/replace (str push-key) ":" "") ">"))
                                            kps       (ut/extract-patterns obody pkey 2)
                                            logic-kps (into {}
                                                            (for [v kps]
                                                              (let [[_ that] v] {v [:push> [flow-id (str bid) that]]})))]
                                        (walk/postwalk-replace logic-kps obody)))
                        view-swap2  (fn [obody flow-id bid push-key]
                                      (let [pkey      (keyword (str (cstr/replace (str push-key) ":" "") ">"))
                                            kps       (ut/extract-patterns obody pkey 1)
                                            logic-kps (into {} (for [v kps] (let [[_] v] {v [:push>> [flow-id (str bid)]]})))]
                                        (walk/postwalk-replace logic-kps obody)))
                        view-swaps  (fn [obody flow-id push-key-bid-pairs]
                                      (reduce (fn [body [push-key bid]]
                                                (-> body
                                                    (view-swap flow-id bid push-key)
                                                    (view-swap2 flow-id bid push-key)))
                                        obody
                                        push-key-bid-pairs))
                        view        (when view
                                      (let [conns              (vec (filter #(cstr/includes? (str (first %)) "/push-path")
                                                                      flowmaps-connections))
                                            push-key-bid-pairs (vec (for [[c1 c2] conns]
                                                                      [(keyword (last (cstr/split (str c1) #"/"))) c2]))
                                            view               (view-swaps view fid push-key-bid-pairs)]
                                        view))
                        nname       (get-in data [:flow-item :name] ":unknown!")
                        fn-key      (if flow-path nname (try-read nname))
                        fn-category (try-read (get-in data [:flow-item :category] ":unknown!"))]]
              (cond
                (and (= ttype :open-block) (ut/ne? (get ports :in))) ;; open block with inputs
                  {k {:data              (get data :user-input)
                      :default-overrides (get-in data [:flow-item :defaults] {})
                      :inputs            (vec (keys (get ports :in)))}}
                (or (= ttype :open-block) (= ttype :cell) (= ttype :param)) {k (if (= (get data :syntax "clojure") "clojure")
                                                                                 (get data :user-input)
                                                                                 (get data :user-input))}
                (= ttype :open-fn)                                          {k (merge (when view {:view view})
                                                                                      {:fn                raw-fn
                                                                                       :raw-fn            raw-fn
                                                                                       :default-overrides (get-in data
                                                                                                                  [:flow-item
                                                                                                                   :defaults]
                                                                                                                  {})
                                                                                       :inputs            (vec (keys (get
                                                                                                                       ports
                                                                                                                       :in)))})}
                (= ttype :query)                                            {k {:fn     (get data :user-input) ;'(fn [x] x)
                                                                                                               ;;:raw-fn '(fn
                                                                                                               ;[x] x)
                                                                                :inputs (vec (keys (get ports :in)))}}
                flow-path                                                   {k {:flow-path         flow-path
                                                                                :sub-flow-id       sub-flow-id
                                                                                :default-overrides (get-in data
                                                                                                           [:flow-item :defaults]
                                                                                                           {})
                                                                                :inputs            (vec (keys (get ports :in)))}}
                (= ttype :sub-flow)                                         {k (-> (process-flowmap2 (get sub-flow :map)
                                                                                                     (get sub-flow :connections)
                                                                                                     fid)
                                                                                   (assoc :file-path file-path)
                                                                                   (assoc :flow-id flow-id))} ;; flow-id of the
                                                                                                              ;; embdeeded
                                                                                                              ;; flow, NOT the
                :else                                                       {k {:fn-key            [fn-category fn-key]
                                                                                :default-overrides (get-in data
                                                                                                           [:flow-item :defaults]
                                                                                                           {})
                                                                                :inputs            (if false ;expandable-in?
                                                                                                     [(vec (keys (get ports
                                                                                                                      :in)))] ;; send
                                                                                                                              ;; as
                                                                                                                              ;; single
                                                                                                     (vec (keys (get ports
                                                                                                                     :in))))}})))
        components-key (into {} ;; deconstruct conditional paths to single condi key
                             (for [[k v] flowmap]
                               (if (not (empty? (get v :cond)))
                                 (let [ccond (into {}
                                                   (for [[c1 c2] flowmaps-connections
                                                         :let    [link  (keyword (str (cstr/replace (str k) ":" "") "/cond-path"))
                                                                  ff    (cstr/split (str c1) #"/")
                                                                  cname (keyword (last ff))
                                                                  c2wo  (keyword (first (cstr/split (cstr/replace (str c2) ":" "")
                                                                                                    #"/")))] ;; w/o port id....
                                                         :when   (cstr/starts-with? (str c1) (str link))]
                                                     {c2wo (get-in v [:cond cname :fn])}))]
                                   {k (assoc (get components-key k) :cond ccond)})
                                 {k (get components-key k)})))
        flowmaps-connections (vec (filter #(and (not (cstr/includes? (str (first %)) "/cond-path"))
                                                (not (cstr/includes? (str (first %)) "/push-path")))
                                    flowmaps-connections))
        server-flowmap {:canvas canvas-key :components components-key :connections flowmaps-connections}]
    server-flowmap))

(defn materialize-flowmap
  [client-name flowmap flow-id opts & [no-eval?]]
  (let [flowmap          (if (string? flowmap) ;; load from disk and run client sync post
                           (let [ppath       (if (cstr/ends-with? (cstr/lower-case flowmap) ".edn")
                                               flowmap ;; already a file path
                                               (str "./flows/" flowmap ".edn"))
                                 raw         (try (edn/read-string (slurp ppath))
                                                  (catch Exception _
                                                    (do (ut/pp [:error-reading-flow-from-disk flow-id client-name]) {})))
                                 flowmaps    (process-flow-map (get raw :flowmaps))
                                 connections (get raw :flowmaps-connections)
                                 fmap        (process-flowmap2 flowmaps connections flow-id)]
                             (ut/pp [:materialized-flowmap fmap])
                             fmap)
                           flowmap)
        uid              (str flow-id) ;(ut/generate-name)
        post-id          (if (get opts :increment-id? false) ;; if auto increment idx, lets use
                           (str uid "-" (count (filter #(cstr/starts-with? % uid) (keys @flow-db/channel-history))))
                           uid)
        sub-map          {:flow-id uid :client-name client-name}
        finished-flowmap (if no-eval? flowmap (process-flowmaps flowmap sub-map))]
    (if no-eval? finished-flowmap (eval finished-flowmap))))

(declare get-flow-open-ports)

(defn create-flowblock
  [file-path]
  (let [data    (get-flow-open-ports file-path nil nil)
        fid0    (cstr/split file-path #"/")
        fid     (cstr/replace (last fid0) ".edn" "")
        out     (first
                  (for [[_ v] (get data :blocks) :when (true? (get v :last?))] (get-in v [:type :out] (get-in v [:type :*]))))
        flow-id (get data :flow-id)
        block   {:flow-id     flow-id
                 :flow-path   file-path
                 :fn          '()
                 :inputs      (vec (keys (get data :open-inputs [])))
                 :required    (vec (keys (get data :open-inputs [])))
                 :defaults    (into {} (for [[k v] (get data :open-inputs [])] {k (get v :user-input)}))
                 :description (get data :description)
                 :icon        "zmdi-puzzle-piece"
                 :types       (assoc (into {} (for [[k v] (get data :open-inputs [])] {k (get-in v [:type :out] (get v :type))})) ;; work
                                                                                                                                  ;; around
                                :out out)}]
    (async/thread ;; really expensive logging below. temp
      (let [fp (str "./flow-blocks/" fid ".edn")]
        (ext/create-dirs "./flow-blocks/")
        (ut/pretty-spit fp block 225)))
    (swap! sub-flow-blocks assoc flow-id block)
    {flow-id block}))

(defn alert!
  [client-name content w h duration & [type]]
  (push-to-client [:alerts] [content w h duration] client-name -1 :alert1 :alert2)
  :alerted!)

(defn ttap> [client-name x & [w h d]] (alert! client-name (if (number? x) (str x) x) (or w 10) (or h 1.35) (or d 12)))



(defn save!
  [kp v & [client-name]]
  (let [kp (if (keyword? kp) [kp] kp)] ;; just incase
    (swap! server-atom assoc-in kp v)))

(def last-times (atom {})) ;; stopgap for error times TODO, april 2024

(defn flow!
  [client-name flowmap file-image flow-id opts & [no-return?]]
  (try
    (let [orig-flowmap     flowmap
          from-string?     (true? (string? flowmap))
          _ (swap! orig-caller assoc flow-id client-name) ;; caller for the flow in case of
          orig-opts        opts ;; contains overrides
          oo-flowmap       (if (string? flowmap) ;; load from disk and run client sync post
                             (let [fss         (str "./flows/" flowmap ".edn")
                                   raw         (try (edn/read-string (slurp fss))
                                                    (catch Exception _
                                                      (do (ut/pp [:error-reading-flow-from-disk-oo-flowmap-flow! fss flow-id
                                                                  client-name])
                                                          {})))
                                   flowmaps    (process-flow-map (get raw :flowmaps))
                                   connections (get raw :flowmaps-connections)
                                   fmap        (process-flowmap2 flowmaps connections flow-id)
                                   fmap        (merge fmap {:opts (get raw :opts)})]
                               fmap)
                             flowmap)
          file-image       (or file-image
                               (try (edn/read-string (slurp (str "./flows/" flowmap ".edn")))
                                    (catch Exception _
                                      (do (when (not= flow-id "client-keepalive") ;; since no
                                            (ut/pp [:error-reading-flow-from-disk-file-image-flow! flow-id client-name]))
                                          {}))))
          user-opts        (get oo-flowmap :opts (get opts :opts))
          opts             (assoc opts :opts user-opts) ;; kinda weird, but we need to account
          walk-map         (into {}
                                 (for [[k v] (get @params-atom client-name)]
                                   {(keyword (str "param/" (cstr/replace (str k) ":" ""))) v}))
          flowmap          (walk/postwalk-replace walk-map oo-flowmap)
          uid              (get opts :instance-id (str flow-id)) ;(ut/generate-name)
          post-id          (if (get opts :increment-id? false) ;; if auto increment idx, lets
                             (str uid "-" (count (filter #(cstr/starts-with? % uid) (keys @flow-db/channel-history))))
                             uid)
          uuid             (str (java.util.UUID/randomUUID))
          sub-map          {:flow-id uid :client-name client-name}
          finished-flowmap (process-flowmaps flowmap sub-map)
          finished-flowmap (walk/postwalk-replace (merge ;; need to do recursively eventually?
                                                    {:client-name client-name
                                                     :cid         client-name
                                                     :flow-id     flow-id
                                                     :fid         flow-id
                                                     :client-id   client-name
                                                     'save!       'rvbbit-backend.websockets/save!
                                                     'tap>        'rvbbit-backend.websockets/ttap>}
                                                    (ut/deselect-keys (get opts :opts) ;; TODO,
                                                                      [:retries :retry-on-error? :close-on-done? :debug?
                                                                       :timeout]))
                                                  finished-flowmap)
          opts             (merge opts (select-keys (get opts :opts {}) [:close-on-done? :debug? :timeout]))
          opts             (merge {:client-name client-name} opts)
          finished-flowmap (assoc finished-flowmap :opts opts)
          _ (swap! watchdog-atom assoc flow-id 0) ;; clear watchdog counter
          _ (swap! tracker-history assoc flow-id []) ;; clear loop step history
          _ (swap! last-times assoc-in [flow-id :start] (System/currentTimeMillis))
          _ (swap! tracker-client-only assoc flow-id {})
          _ (swap! acc-trackers assoc flow-id [])
          _ (swap! temp-error-blocks assoc flow-id [])
          base-flow-id     (if (cstr/includes? (str flow-id) "-SHD-") (first (cstr/split flow-id #"-SHD-")) flow-id) ;; if
                                                                                                                     ;; history
                                                                                                                     ;; run,
                                                                                                                     ;; only for
                                                                                                                     ;; estimates
                                                                                                                     ;; for now
          prev-times       (get @times-atom base-flow-id [-1])
          ship-est         (fn [client-name]
                             (try (let [times (ut/avg ;; avg based on last 10 runs, but only if >
                                                (vec (take-last 10 (vec (remove #(< % 1) prev-times)))))]
                                    (when (not (nil? times))
                                      (kick client-name [:estimate] {flow-id {:times times :run-id uuid}} nil nil nil)))
                                  (catch Exception e (ut/pp [:error-shipping-estimates (Throwable->map e) (str e)]) 0)))
          _ (ship-est client-name)
          image-map        (assoc file-image :opts (merge (get file-image :opts) (select-keys orig-opts [:overrides])))
          _ (do (ext/create-dirs "./flow-history") ;; just in case
                (spit ;; a partial flow-history map so if someone wants to "jump into" a
                  (str "./flow-history/" flow-id ".edn")
                  {:image       image-map ;; a partial flow-history map so if someone wants to
                   :source-map  finished-flowmap
                   :client-name client-name
                   :flow-id     flow-id}))
          return-val       (flow-waiter (eval finished-flowmap) uid opts) ;; eval to realize fn
          retry?           (get-in opts [:opts :retry-on-error?] false)
          restarts         (if retry? (- (get-in opts [:opts :retries] 0) (get @restart-map flow-id 0)) 0)
          restarts-left?   (> restarts 0)
          relevant-keys    (vec (filter #(cstr/starts-with? (str %) (str post-id)) (keys @flow-db/results-atom)))
          working-data-ref (into {} (for [[k v] (select-keys @flow-db/working-data relevant-keys)] {k (get v :description)}))
          run-id           (get-in @flow-db/results-atom [flow-id :run-id] "no-run-id")
          output           {:client-name     client-name
                            :flow-id         flow-id
                            :run-id          run-id
                            :fn-history      {} ;(select-keys @flow-db/fn-history
                            :run-refs        working-data-ref
                            :tracker         (select-keys @flow-db/tracker relevant-keys) ;; generally
                            :tracker-history (ut/accumulate-unique-runs (get @tracker-history flow-id []))
                            :source-map      finished-flowmap ;(if no-return? {}
                            :return-maps     (if no-return? {} (select-keys @flow-db/results-atom relevant-keys))
                            :return-map      {} ;(get @flow-db/results-atom post-id)
                            :return-val      (if no-return? nil return-val)}]
      (do
        (swap! restart-map assoc flow-id (inc (get @restart-map flow-id 0)))
        (when (not (cstr/includes? flow-id "keepalive"))
          (ut/pp [:flowmap-returned-val (ut/limited (ut/replace-large-base64 return-val)) :flowmap-returned-val]))
        (let [cnt         (count (str return-val))
              limit       160
              over?       (> cnt limit)
              error?      (or (cstr/includes? (str return-val) ":error") (cstr/includes? (str return-val) ":timeout"))
              sample      (str ; (str (get-in @flow-status [flow-id :*time-running]) " ")
                            (if over? (str (subs (str return-val) 0 limit) "...") (str return-val)))
              result-code (cond (cstr/includes? (str return-val) ":error")   "error"
                                (cstr/includes? (str return-val) ":timeout") "timeout"
                                :else                                        "success")
              sample      (if error? (get-in return-val [:error :error] sample) sample)]
          (alert! client-name
                  [:v-box :justify :center :style {:margin-top "-6px" :color (if error? "red" "inherit")} :children
                   [[:box :child (str "flow " flow-id " has finished" (when error? (str " in error")))]
                    (when (and error? restarts-left?)
                      [:box :style {:font-size "9px"} :child (str "  " restarts " attempts left, restarting in 10 seconds...")])
                    (when (not error?)
                      [:box :style {:font-weight 700 :font-size "10px" :opacity 0.7} :child
                       (str (get-in @flow-status [flow-id :*time-running]) " ")])
                    [:box :style {:font-weight 700 :font-size (if error? "13px" "10px") :opacity (if error? 1.0 0.7)} :child
                     (if error? (str sample) (str "returns: " sample))]]]
                  10
                  (if (and error? restarts-left?) 1.5 1.35)
                  (if error? 25 9))
          (when (and error? restarts-left?)
            (async/thread (do (Thread/sleep 10000) (flow! client-name orig-flowmap file-image flow-id opts))))
          (when (not error?) (swap! restart-map dissoc flow-id)) ;; reset the counter on
          (do (when (not (= flow-id "client-keepalive")) ;; no need to track heartbeats..
                (swap! latest-run-id assoc flow-id run-id)
                (ut/pp [:saving-flow-exec-for flow-id result-code run-id])
                (do (ext/create-dirs "./flow-history") ;; just in case
                    (spit ;; ut/pretty-spit
                      (str "./flow-history/" run-id ".edn")
                      (ut/replace-large-base64 (merge output
                                                      {;;:image file-image ;; as unprocessed as we can w/o being a file path
                                                       :image       image-map
                                                       :return-maps (select-keys @flow-db/results-atom relevant-keys) ;; in
                                                                                                                      ;; case
                                                       :return-val  return-val} ;; in case no-return? is true
                                               ))))) ;; just for the record
              output))))
    (catch Throwable e ;; mostly for mal-formed raw-fns, etc TODO make more informative
      (let [error-map (Throwable->map e)]
        (do (ut/pp [:error-in-flow-processing error-map])
            (alert! client-name
                    [:v-box :children
                     [[:box :child (str flow-id ":error in flow processing!")] [:box :child (str (:phase error-map))]
                      [:box :style {:font-size "11px"} :child (str (:cause error-map))]]]
                    10
                    2.1
                    8)
            {:error error-map})))))

(defn schedule!
  [time-seq1 flowmap & [opts]]
  (let [;[opts chan-out override] args
        opts            (if (nil? opts) {} opts)
        client-name     :rvbbit-scheduler
        raw             (if (string? flowmap)
                          (try (edn/read-string (slurp (str "./flows/" flowmap ".edn")))
                               (catch Exception _
                                 (do (ut/pp [:error-reading-flow-from-disk-raw-schedule flowmap client-name]) {})))
                          {})
        user-opts       (get raw :opts)
        opts            (merge {:client-name client-name} opts user-opts)
        flow-id         (get opts :flow-id (str "unnamed-flow-sched" (hash flowmap)))
        times           (if (and (vector? time-seq1) (keyword? (first time-seq1)))
                          (doall (take 1000 (ut/time-seq time-seq1)))
                          time-seq1)
        times-unlimited (if (and (vector? time-seq1) (keyword? (first time-seq1))) (ut/time-seq time-seq1) time-seq1)
        ch              (chime/chime-at times-unlimited ;; [] chime time seq,
                                        (fn [time]
                                          (when (not @shutting-down?)
                                            (let [opts (merge opts {:schedule-started (str time)})]
                                              (ut/pp [:*scheduled-run! flow-id :at (str time)])
                                              (flow! client-name flowmap nil flow-id opts))))
                                        {:on-finished   (fn [] (ut/pp [:schedule-finished! opts time-seq1]))
                                         :error-handler (fn [e] (ut/pp [:scheduler-error e]))})] ;; if custom
    (swap! flow-db/live-schedules conj
      {:flow-id    flow-id ;(get opts :flow-id "unnamed-flow-sched")
       :override   (get opts :overrides)
       :next-times (try (vec (map str times)) (catch Exception _ times))
       :schedule   (if (vector? time-seq1) time-seq1 [:custom-time-fn])
       :channel    ch})))

(defn unschedule!
  [flow-id]
  (let [schedule-to-remove (some #(when (= (:flow-id %) flow-id) %) @flow-db/live-schedules)]
    (when schedule-to-remove
      (async/close! (:channel schedule-to-remove))
      (swap! flow-db/live-schedules #(remove (fn [x] (= (:flow-id x) flow-id)) %)))))

(defn get-flow-open-ports
  [flowmap flow-id client-name] ;; generally a local path, not a map, but hey w/e
  (let [raw                 (try (edn/read-string (slurp (if (cstr/ends-with? (cstr/lower-case flowmap) ".edn")
                                                           flowmap ;; file path, use as is
                                                           (str "./flows/" flowmap ".edn"))))
                                 (catch Exception _
                                   (do (ut/pp [:error-reading-flow-from-disk-raw-open-ports flow-id client-name]) {})))
        [flowmap raw-conns] [(if (string? flowmap) ;; load from disk and run client sync post
                               (let [flowmaps    (process-flow-map (get raw :flowmaps))
                                     connections (get raw :flowmaps-connections)
                                     fmap        (process-flowmap2 flowmaps connections flow-id)]
                                 fmap)
                               flowmap) (get raw :flowmaps-connections)]
        vblocks             (vec (for [[k v] (get flowmap :components)
                                       :when (get v :view)]
                                   (keyword (str (cstr/replace (str k) ":" "") "-vw"))))
        blocks              (set (into vblocks (vec (keys (get flowmap :components)))))
        flow-id             (or flow-id (get raw :flow-id)) ;; if not passed in, get from file
        blocks-types        (into {}
                                  (for [b blocks]
                                    (let [actual-data       (get-in @flow-db/results-atom [flow-id b])
                                          trunc-num         60
                                          view?             (some #(= % b) vblocks)
                                          llen              (count (str actual-data))
                                          declared-out-type (get-in raw [:flowmaps b :ports :out])]
                                      {b {:type     (if actual-data (keyword (ut/data-typer actual-data)) declared-out-type)
                                          :ttype    (or (get-in raw [:flowmaps b :data :drag-meta :type])
                                                        (get-in raw [:flowmaps b :data :flow-item :type]))
                                          :meta     (get-in raw [:flowmaps b :data :flow-item :meta])
                                          :last?    (true? (some true?
                                                                 (for [[f1 f2] (get flowmap :connections)
                                                                       :when   (and (= f1 b) (= f2 :done))]
                                                                   true)))
                                          :defaults (get-in raw [:flowmaps b :data :defaults] {})
                                          :sample   (if view?
                                                      "(renderable view)"
                                                      (if actual-data
                                                        (if (> llen trunc-num)
                                                          (str (subs (str actual-data) 0 trunc-num)
                                                               (when (> llen trunc-num) "..."))
                                                          (str actual-data))
                                                        "no sample data"))}})))
        base-conns          (set (for [[_ v2] raw-conns ;(get flowmap :connections)
                                      ]
                                   (keyword (gns v2))))
        desc                (get-in raw [:opts :description])
        no-inputs           (vec (remove #(cstr/ends-with? (str %) "-vw") (cset/difference blocks base-conns)))
        flow-inputs         (into {}
                                  (for [i    no-inputs
                                        :let [outs (keys (get-in raw [:flowmaps i :ports :out]))]]
                                    {i {:user-input (get-in raw [:flowmaps i :data :user-input])
                                        :defaults   (get-in raw [:flowmaps i :data :flow-item :defaults] {})
                                        :type       (get-in raw
                                                            [:flowmaps i :ports :out
                                                             (if (some #(= % :*) outs) :* (first outs))])}}))]
    {:flow-id flow-id :description desc :open-inputs flow-inputs :blocks blocks-types :found? (true? (ut/ne? raw))}))

(defmethod wl/handle-request :get-flow-open-ports
  [{:keys [client-name flowmap flow-id]}]
  (get-flow-open-ports flowmap flow-id client-name))

(defmethod wl/handle-request :run-flow
  [{:keys [client-name flowmap file-image flow-id opts no-return?]}]
  (ut/pp [:running-flow-map-from client-name])
  (when (not (get-in (flow-statuses) [flow-id :*running?] false)) (flow! client-name flowmap file-image flow-id opts true)))

(defmethod wl/handle-push :run-flow
  [{:keys [client-name flowmap file-image flow-id opts no-return?]}]
  (ut/pp [:running-flow-map-from client-name])
  (when (not (get-in (flow-statuses) [flow-id :*running?] false)) (flow! client-name flowmap file-image flow-id opts true)))

(declare run-solver)

;; TODO, these 3 can all be combined into one "smarter" version base don args, but I'm still iterating, so it's fine
(defmethod wl/handle-request :run-solver
  [{:keys [solver-name client-name override-map]}]
  (ut/pp [:manual-solver-run! solver-name :from client-name :override override-map])
  (swap! last-solvers-atom-meta assoc-in
    [solver-name :output]
    [:warning! {:solver-running-manually-via client-name :with-override-map override-map}])
  ;; (enqueue-task4 (fn [] (run-solver solver-name client-name override-map)))
  (run-solver solver-name client-name override-map))

(defmethod wl/handle-push :run-solver-custom
  [{:keys [solver-name temp-solver-name client-name override-map input-map]}]
  (ut/pp [:custom-solver-run! temp-solver-name :from client-name :input-map input-map])
  (swap! last-solvers-atom-meta assoc-in
    [temp-solver-name :output]
    [:warning! {:solver-running-custom-inputs-via client-name :with-input-map input-map :override-map? override-map}])
  ;; (enqueue-task4 (fn [] (run-solver solver-name nil input-map temp-solver-name)))
  (run-solver solver-name client-name override-map input-map temp-solver-name)
  temp-solver-name)

(defn flow-kill!
  [flow-id client-name]
  (future (swap! flow-status assoc
            flow-id
            {:*done?         true
             :*result        (str "killed-by-user-" client-name)
             :*finished      -1
             :*running       []
             :*done          []
             :*ms-elapsed    -1
             :*time-running  "killed"
             :*not-started   []
             :*open-channels []
             :*running?      false}))
  (swap! flow-db/status assoc-in [flow-id :end] (System/currentTimeMillis))
  (swap! flow-db/results-atom assoc-in [flow-id :done] :you-killed-it!) ;; to force the idea
  (ttap> client-name
         [:v-box :children
          [[:box :style {:font-size "10px"} :child (str "force stop " flow-id)]
           [:box :child (str "killing " (count (get @flow-db/channels-atom flow-id)) " channels")]]]
         14
         1.1
         10)
  (doseq [[k c] (get @flow-db/channels-atom flow-id)]
    (try #_{:clj-kondo/ignore [:redundant-do]}
         (do ;(ut/ppln [:closing-channel k c])
           (try (when (not (nil? c)) (do (swap! flow-db/channels-atom ut/dissoc-in [flow-id k]) (async/close! c)))
                (catch Throwable e
                  (do (swap! flow-db/channels-atom assoc-in [flow-id k] c) (ut/ppln [:error-closing-channelS-inner e k c])))) ;; close
                                                                                                                              ;; channel
         ) ;; remove from "live" channel atom
         (catch Throwable e
           (do (swap! flow-db/channels-atom assoc-in [flow-id k] c) ;; if failed, keep key val
               (ut/ppln [:error-closing-channelS-outer e k c]))))))

(defmethod wl/handle-request :kill-flow
  [{:keys [client-name flow-id process?]}]
  (ut/pp [:kill-flow flow-id :from client-name :!])
  (if process? (stop-process flow-id) (flow-kill! flow-id client-name))
  [:assassin-sent!])


(defn warren-flow-map
  [connections]
  (let [x-offset              784
        y-offset              862
        children              (reduce (fn [m [p c]] (update m p conj c)) {} connections)
        assign-levels         (fn assign-levels [levels node level]
                                (if (contains? levels node)
                                  levels
                                  (reduce (fn [acc child] (assign-levels acc child (inc level)))
                                    (assoc levels node level)
                                    (children node []))))
        levels                (reduce (fn [acc node] (assign-levels acc node 0)) {} (keys children))
        level-nodes           (group-by #(get levels %) (keys levels))
        node-positions        (reduce-kv (fn [m lvl nodes]
                                           (let [child-nodes  (mapcat children nodes)
                                                 parent-avg-x (if (seq child-nodes)
                                                                (/ (reduce +
                                                                     (keep #(when-let [pos (get m %)] (first pos))
                                                                           (map #(get m % [0]) child-nodes)))
                                                                   (count child-nodes))
                                                                0)
                                                 spacing      150
                                                 start-x      (+ x-offset (max 100 parent-avg-x))]
                                             (reduce (fn [acc [node idx]]
                                                       (assoc acc node [(+ start-x (* idx spacing)) (* 125 lvl)]))
                                               m
                                               (map vector nodes (range)))))
                                         {}
                                         level-nodes)
        flowmaps              (reduce-kv (fn [acc k [x y]]
                                           (assoc acc
                                             k {:y     (+ y y-offset)
                                                :x     x
                                                :w     125
                                                :h     60
                                                :icon  "zmdi-functions"
                                                :z     0
                                                :ports {:in {:x :any} :out {:out :any}}
                                                :data  {}}))
                                         {}
                                         node-positions)
        connections-formatted (map (fn [[from to]] [(keyword from) (keyword (cstr/replace (str to "/x") ":" ""))]) connections)]
    {:flowmaps flowmaps :flowmaps-connections connections-formatted :opts {} :flow-id "generated-flow-map"}))

(defn boomerang-client-subs
  [cid]
  (let [sub-task (vec (keys (get @atoms-and-watchers cid {})))]
    (push-to-client [:kick]
                    {:at "" :payload nil :payload-kp [:heartbeat :heartbeat] :sent! :heartbeat :to :all}
                    cid
                    -1
                    :heartbeat
                    sub-task)))

(declare remove-watchers-for-flow)

(defmethod wl/handle-request :remove-flow-watcher
  [{:keys [client-name flow-id]}]
  (ut/pp [::remove-flow-watcher flow-id :from client-name :!])
  (remove-watchers-for-flow flow-id client-name)
  (boomerang-client-subs client-name)
  [:watcers-removed!])



(defmethod wl/handle-request :session-snaps
  [{:keys [client-name]}]
  (take 9 (sort-by second (ut/get-file-vectors-with-time-diff "./snaps/" "edn"))))


(defmethod wl/handle-request :push-value
  [{:keys [client-name flow-id bid value alert?]}]
  (let [start (System/currentTimeMillis)]
    (ut/pp [:pushing-value value :to flow-id bid :from client-name :!])
    (doseq [[channel-name c] (get @flow-db/channels-atom flow-id)
            :let             [from  (first channel-name)
                              bid   (if (string? bid) (try (edn/read-string bid) (catch Exception _ bid)) bid) ;; we need to
                                                                                                               ;; stringify it
                                                                                                               ;; to avoid
                                                                                                               ;; pre-mature
                              cbid  (try (keyword (first (cstr/split (cstr/replace (str (first channel-name)) ":" "") #"/")))
                                         (catch Exception _ :none))
                              cbid2 (try (keyword (first (cstr/split (cstr/replace (str (second channel-name)) ":" "") #"/")))
                                         (catch Exception _ :none))
                              _ (ut/pp [:push-channel-logic cbid channel-name bid])]
            :when            (and (or (= (first channel-name) bid) (= bid cbid))
                                  (not (or (= (second channel-name) bid) (= bid cbid2))))]
      (ut/pp [:pushing-to cbid channel-name bid])
      (when alert?
        (ttap> client-name
               [:v-box :children
                [[:box :style {:font-size "10px"} :child (str [:pushing-to cbid cbid2 channel-name bid])]
                 [:box :child (str value)]]]
               14
               1.1
               10))
      (swap! flow-db/tracker assoc-in [flow-id bid :end] (System/currentTimeMillis))
      (swap! flow-db/results-atom assoc-in [flow-id bid] value) ;; to force the idea that we've
      (swap! flow-db/fn-history assoc
        flow-id
        (conj (get @flow-db/fn-history flow-id [])
              {:block      from
               :from       :static
               :path       [:from :static from]
               :value      (ut/limited value flow-id)
               :type       :function
               :dest       from
               :channel    [from]
               :data-type  (ut/data-typer (ut/limited value flow-id))
               :start      start
               :end        (System/currentTimeMillis)
               :elapsed-ms (- (System/currentTimeMillis) start)}))
      (async/put! c ;(get-in @flow-db/channels-atom [flow-id channel-name])
                  {:sender (last channel-name) :value value})
      [:value-pushed!])))

(defmethod wl/handle-request :flow-status
  [{:keys [client-name flow-id]}]
  (ut/pp [:run-status-req-from client-name :for flow-id])
  {:results-atom (get @flow-db/results-atom flow-id)
   :status       (get @flow-status flow-id)
   :tracker      (get @flow-db/results-atom flow-id)
   :flow-id      flow-id})

(defmethod wl/handle-request :signals-map
  [{:keys [client-name]}]
  (ut/pp [:get-signals-map client-name])
  (let [bm {}] @signals-atom))

(defmethod wl/handle-request :rules-map [{:keys [client-name]}] (ut/pp [:get-rules-map client-name]) (let [bm {}] @rules-atom))

(defmethod wl/handle-request :solvers-map
  [{:keys [client-name]}]
  (ut/pp [:get-solvers-map client-name])
  (let [bm {}] @solvers-atom))

(defonce client-helper-atom (atom {}))

(defmethod wl/handle-request :signals-history
  [{:keys [client-name signal-name]}]
  (inc-score! client-name :push)
  (let [cc      (get-in @signals-atom [signal-name :signal])
        ccw     (vec (ut/where-dissect cc))
        history (select-keys (get @last-signals-history-atom signal-name {}) ccw)]
    history))

(defmethod wl/handle-request :save-signals-map
  [{:keys [client-name data-map]}]
  (ut/pp [:saving-signals-map client-name])
  (let [bm {}] (reset! signals-atom data-map)))

(defmethod wl/handle-request :save-rules-map
  [{:keys [client-name data-map]}]
  (ut/pp [:saving-rules-map client-name])
  (let [bm {}] (reset! rules-atom data-map)))

(defmethod wl/handle-request :save-solvers-map
  [{:keys [client-name data-map]}]
  (ut/pp [:saving-solvers-map client-name])
  (let [bm {}] (reset! solvers-atom data-map)))

(defmethod wl/handle-request :save-custom-flow-block
  [{:keys [client-name name block-map]}]
  (ut/pp [:saving-custom-flow-block client-name :for name])
  (let [bm {name block-map}] (ut/pp [:saving-new-custom-block bm])))


(defmethod wl/handle-request :sync-client-params
  [{:keys [client-name params-map]}]
  (let [params     (or (dissoc params-map nil) {})
        kps        (ut/kvpaths params)
        make-key   (fn [x] (str :client-param "-" (cstr/join ">" x)))
        these-keys (map make-key kps)]
    (swap! params-atom assoc client-name params) ;; push the actual params
    [:got-it!]))

(defmethod wl/handle-request :run-flow2
  [{:keys [client-name flowmap flow-id]}]
  (ut/pp [:running-flow-map-from client-name])
  (let []
    (do ;(ut/pp [:flowmap-returned-val return-val])
      {:client-name client-name
       :flow-id     flow-id
       :server-map  "finished-flowmap"
       :return-map  "return-map"
       :return-val  "return-val"})))

(defonce trigger-words-atom (atom {}))

(defmethod wl/handle-request :schedule-flow
  [{:keys [schedule client-name flow-id trigger-words trigger-word-insert]}]
  (if (not (nil? trigger-words))
    (do (ut/pp [:flow-start-trigger-words flow-id :when trigger-words :send-to trigger-word-insert :created-by client-name])
        (swap! trigger-words-atom assoc
          (cstr/lower-case trigger-words)
          {:flow-id flow-id :created-by client-name :trigger-word-insert trigger-word-insert})
        (sql-exec flows-db
                  (to-sql {:insert-into [:live_schedules]
                           :values      [{:flow-id flow-id :override (str trigger-word-insert) :schedule (str trigger-words)}]}))
        :scheduled!)
    (when schedule
      (do (ut/pp [:schedule-flow-start flow-id :for schedule :by client-name])
          (let [sched-vec [(keyword (get schedule :period)) ;; needs to be keyword
                           (edn/read-string (get schedule :every))]] ;; [:seconds 45]
            (ut/pp [:schedule-flow flow-id :for sched-vec :by client-name])
            (schedule! sched-vec
                       (materialize-flowmap :server flow-id flow-id {})
                       {:flow-id flow-id :increment-id? false :close-on-done? true :debug? false}) ;; add
                                                                                                   ;; overrides
                                                                                                   ;; to
                                                                                                   ;; opts-map
            :scheduled!)))))

(defmethod wl/handle-request :voice-trigger
  [{:keys [client-name voice-text]}]
  (ut/pp [:voice-text-trigger-analyzing voice-text :from client-name])
  (doseq [[k {:keys [flow-id trigger-words trigger-word-insert]}] @trigger-words-atom
          :let                                                    [match? (cstr/starts-with?
                                                                            (cstr/replace (cstr/lower-case voice-text) "," "")
                                                                            k)]
          :when                                                   match?]
    (flow! client-name
           flow-id
           flow-id
           nil
           {:increment-id?  false
            :close-on-done? true ; false ;true
            :debug?         false
            :overrides      {trigger-word-insert voice-text}})))

(defmethod wl/handle-request :get-status
  [{:keys [client-name]}]
  (ut/pp [:client-status-check-from client-name])
  (inc-score! client-name :push)
  (ut/pp [:PUSH-get-status! client-name])
  {:statuses (get @queue-status client-name) :data (get @queue-data client-name)})

(defmethod wl/handle-request :get-flow-statuses
  [{:keys [client-name]}]
  (inc-score! client-name :push)
  (ut/pp [:PUSH-get-flow-status! client-name])
  (let [fss     (flow-statuses)
        fssk    (vec (keys fss))
        payload (merge (select-keys fss fssk)
                       (into {}
                             (for [[k v] @processes]
                               {k (-> v
                                      (assoc :process? true)
                                      (dissoc :output)
                                      (dissoc :process))})))]
    payload))

(defmethod wl/handle-request :delete-kit-rows
  [{:keys [client-name where]}]
  (ut/pp [:delete-kit-rows-from client-name])
  (let [sql-stmt {:delete-from [:kits] :where where}
        sql-str  (to-sql sql-stmt)]
    (sql-exec system-db sql-str)
    [:done]))








(defonce screen-child-atoms (atom {}))
(defonce param-child-atoms (atom {}))
(defonce panel-child-atoms (atom {}))
(defonce flow-child-atoms (atom {})) ;; flows

(defn get-atom-splitter
  [key ttype child-atom parent-atom]
  (if-let [child-atom (get @child-atom key)]
    child-atom
    (let [;base-dir (str "./data/atoms/" (cstr/replace (str ttype) ":" ""))
          new-child-atom (atom {})]
      (swap! child-atom assoc key new-child-atom)
      (swap! new-child-atom assoc key (get @parent-atom key))
      (get @child-atom key))))


(add-watch flow-db/results-atom
           :master-watcher ;; flow watcher split of results-atom
           (fn [_ _ old-state new-state]
             (doseq [key (keys new-state)]
               (if-let [child-atom (get @flow-child-atoms key)]
                 (swap! child-atom assoc key (get new-state key))
                 (let [new-child-atom (atom {})]
                   (swap! flow-child-atoms assoc key new-child-atom)
                   (swap! new-child-atom assoc key (get new-state key)))))))


(defn break-up-flow-key-ext
  [key]
  (let [ff             (cstr/split (-> (str key)
                                       (cstr/replace #":" ""))
                                   #"/")
        ff2            (cstr/split (last ff) #">")
        keyword-or-int (fn [s] (if (re-matches #"\d+" s) (Integer/parseInt s) (keyword s)))] ;; dont
    (vec (into [(keyword (first ff)) (first ff2)] (rest (for [e ff2] (keyword-or-int e)))))))

(defn make-watcher
  [keypath flow-key client-name handler-fn & [no-save]]
  (fn [kkey atom old-state new-state]
    (let [client-name        :all ;; test, no need for individual cache for clients. kinda
          old-value          (get-in old-state keypath)
          new-value          (get-in new-state keypath)
          sub-path           (break-up-flow-key-ext flow-key)
          base-type          (first sub-path)
          last-value         (get-in @last-values-per [client-name keypath])
          all-clients-subbed (for [c     (keys @atoms-and-watchers)
                                   :when (some #(= % flow-key) (keys (get @atoms-and-watchers c)))]
                               c)]
      (when ;(or
        (and (not (nil? new-value)) (not= old-value new-value))
        (doseq [client-name all-clients-subbed] (handler-fn base-type keypath client-name new-value))
        (when (and (not no-save) (ut/serializable? new-value)) ;; dont want to cache tracker
          (swap! last-values assoc keypath new-value)
          (swap! last-values-per assoc-in [client-name keypath] new-value)) ;; if a new client
      ))))

(defn get-atom-from-keys
  [base-type sub-type sub-path keypath]
  (let [tracker?        (= sub-type :tracker)
        flow?           (= base-type :flow) ;; (cstr/starts-with? (str flow-key) ":flow/")
        status?         (and (cstr/includes? (str keypath) ":*") flow?)
        client?         (= base-type :client)
        panel?          (= base-type :panel)
        screen?         (= base-type :screen) ;; (cstr/starts-with? (str flow-key) ":screen/")
        time?           (= base-type :time)
        signal?         (= base-type :signal)
        solver?         (= base-type :solver)
        data?           (= base-type :data)
        solver-meta?    (= base-type :solver-meta)
        signal-history? (= base-type :signal-history)
        server?         (= base-type :server)]
    (cond status?         flow-status
          tracker?        flow-db/tracker
          signal?         last-signals-atom ;; no need to split for now, will keep an eye on it
          solver?         last-solvers-atom ;; no need to split for now, will keep an eye on it -
          solver-meta?    last-solvers-atom-meta
          data?           last-solvers-data-atom
          signal-history? last-signals-history-atom
          server?         server-atom ;; no need to split for now, will keep an eye on it - don't
          time?           time-atom ;; zero need to split ever. lol, its like 6 keys
          panel?          (get-atom-splitter (keyword (second sub-path)) :panel panel-child-atoms panels-atom)
          client?         (get-atom-splitter (keyword (second sub-path)) :client param-child-atoms params-atom)
          flow?           (get-atom-splitter (first keypath) :flow flow-child-atoms flow-db/results-atom)
          screen?         (get-atom-splitter (second sub-path) :screen screen-child-atoms screens-atom)
          :else           (get-atom-splitter (first keypath) :flow flow-child-atoms flow-db/results-atom))))

(defn add-watcher
  [keypath client-name fn flow-key sub-type & [flow-id]] ;; flow id optional is for unsub stuff
  (let [;;client-name :all ;; test
        sub-path        (break-up-flow-key-ext flow-key)
        watch-key       (str :all "-" (str keypath) "-" sub-type "-" flow-key)
        base-type       (first sub-path)
        tracker?        (= sub-type :tracker)
        flow?           (= base-type :flow) ;; (cstr/starts-with? (str flow-key) ":flow/")
        status?         (and (cstr/includes? (str keypath) ":*") flow?)
        client?         (= base-type :client)
        panel?          (= base-type :panel)
        screen?         (= base-type :screen) ;; (cstr/starts-with? (str flow-key) ":screen/")
        time?           (= base-type :time)
        signal?         (= base-type :signal)
        solver?         (= base-type :solver)
        data?           (= base-type :data)
        solver-meta?    (= base-type :solver-meta)
        signal-history? (= base-type :signal-history)
        server?         (= base-type :server)
        keypath         (cond ;flow? keypath
                          signal?         (vec (rest keypath))
                          solver?         (vec (into [(keyword (second sub-path))] (vec (rest (rest sub-path))))) ;;(vec
                          data?           (vec (into [(keyword (second sub-path))] (vec (rest (rest sub-path)))))
                          solver-meta?    (vec (into [(keyword (second sub-path))] (vec (rest (rest sub-path))))) ;;(vec
                          signal-history? (vec (into [(keyword (second sub-path))] (vec (rest (rest sub-path))))) ;;(vec
                          time?           (vec (rest keypath))
                          server?         (vec (rest keypath))
                          screen?         (vec (rest sub-path))
                          panel?          (vec (into [(keyword (second sub-path))] (vec (rest (rest sub-path)))))
                          client?         (vec (into [(keyword (second sub-path))] (vec (rest (rest sub-path)))))
                          :else           keypath)
        watcher         (make-watcher keypath flow-key :all fn (= sub-type :tracker))
        atom-to-watch   (cond
                          status?         flow-status
                          tracker?        flow-db/tracker
                          signal?         last-signals-atom ;; no need to split for now, will keep an eye on
                          solver?         last-solvers-atom ;; no need to split for now, will keep an eye on
                          data?           last-solvers-data-atom
                          solver-meta?    last-solvers-atom-meta
                          signal-history? last-signals-history-atom
                          server?         server-atom ;; no need to split for now, will keep an eye on it - I
                          time?           time-atom ;; zero need to split ever. lol, its like 6 keys
                          panel?          (get-atom-splitter (keyword (second sub-path)) :panel panel-child-atoms panels-atom)
                          client?         (get-atom-splitter (keyword (second sub-path)) :client param-child-atoms params-atom)
                          flow?           (get-atom-splitter (first keypath) :flow flow-child-atoms flow-db/results-atom)
                          screen?         (get-atom-splitter (second sub-path) :screen screen-child-atoms screens-atom)
                          :else           (get-atom-splitter (first keypath) :flow flow-child-atoms flow-db/results-atom))]
    (remove-watch atom-to-watch watch-key)
    (add-watch atom-to-watch watch-key watcher)
    (swap! atoms-and-watchers assoc-in
      [client-name flow-key]
      {:created   (ut/get-current-timestamp)
       :sub-type  sub-type
       :sub-path  sub-path
       :base-type base-type
       :flow-key  flow-key
       :watch-key watch-key
       :flow-id   flow-id
       :keypath   keypath})))


(defn remove-watcher
  [keypath client-name sub-type flow-id flow-key]
  (try
    (let [sub-path (break-up-flow-key-ext flow-key)
          keypath-wildcards-map {"*client-name*" client-name ;; ALREADY IN SUB TO VALUE, NOT
                                 :*client-name*  client-name}
          keypath (walk/postwalk-replace keypath-wildcards-map keypath) ;; keypath wildcards...
          sub-path (walk/postwalk-replace keypath-wildcards-map sub-path)
          watch-key (str :all "-" (str keypath) "-" sub-type "-" flow-key)
          base-type (first sub-path)
          tracker? (= sub-type :tracker)
          flow? (= base-type :flow) ;; (cstr/starts-with? (str flow-key) ":flow/")
          status? (and (cstr/includes? (str keypath) ":*") flow?)
          client? (= base-type :client)
          panel? (= base-type :panel)
          screen? (= base-type :screen) ;; (cstr/starts-with? (str flow-key)
                                        ;; ":screen/")
          time? (= base-type :time)
          signal? (= base-type :signal)
          solver? (= base-type :solver)
          data? (= base-type :data)
          solver-meta? (= base-type :solver-meta)
          signal-history? (= base-type :signal-history)
          server? (= base-type :server)
          keypath (cond ;flow? keypath
                    signal?         (vec (rest keypath))
                    data?           (vec (into [(keyword (second sub-path))] (vec (rest (rest sub-path)))))
                    solver?         (vec (into [(keyword (second sub-path))] (vec (rest (rest sub-path)))))
                    solver-meta?    (vec (into [(keyword (second sub-path))] (vec (rest (rest sub-path)))))
                    signal-history? (vec (into [(keyword (second sub-path))] (vec (rest (rest sub-path)))))
                    time?           (vec (rest keypath))
                    screen?         (vec (rest sub-path))
                    server?         (vec (rest sub-path))
                    panel?          (vec (into [(keyword (second sub-path))] (vec (rest (rest sub-path)))))
                    client?         (vec (into [(keyword (second sub-path))] (vec (rest (rest sub-path)))))
                    :else           keypath)
          atom-to-watch (cond
                          status?         flow-status
                          tracker?        flow-db/tracker
                          signal?         last-signals-atom ;; no need to split for now,
                                                            ;; will keep an eye
                          solver?         last-solvers-atom ;; no need to split for now,
                                                            ;; will keep an eye
                          data?           last-solvers-data-atom ;; ^^ same as above -
                                                                 ;; take a hint, choom.
                          solver-meta?    last-solvers-atom-meta
                          signal-history? last-signals-history-atom
                          server?         server-atom ;; no need to split for now, will keep an eye on it -
                          time?           time-atom ;; zero need to split ever. lol, its like 6 keys
                          panel?          (get-atom-splitter (keyword (second sub-path)) :panel panel-child-atoms panels-atom)
                          client?         (get-atom-splitter (keyword (second sub-path)) :client param-child-atoms params-atom)
                          flow?           (get-atom-splitter (first keypath) :flow flow-child-atoms flow-db/results-atom)
                          screen?         (get-atom-splitter (second sub-path) :screen screen-child-atoms screens-atom)
                          :else           (get-atom-splitter (first keypath) :flow flow-child-atoms flow-db/results-atom))]
      (ut/pp [:removing-watcher keypath client-name sub-type flow-id watch-key])
      (remove-watch atom-to-watch watch-key)
      (swap! atoms-and-watchers ut/dissoc-in [client-name flow-key]))
    (catch Throwable e (ut/pp [:remove-watcher e]))))


(defn replace-flow-key-vars
  [flow-key client-name]
  (let [client-name-str (cstr/replace (str client-name) ":" "")]
    (edn/read-string ;; should always be a keyword coming in
      (ut/replace-multiple (str flow-key) {"*client-name*" client-name-str}))))

(defmethod wl/handle-request :unsub-to-flow-value
  [{:keys [client-name flow-key]}]
  (let [;;orig-flow-key flow-key
        subbed-sub (get-in @param-var-crosswalk [client-name flow-key])
        flow-id    nil]
    (if (ut/ne? subbed-sub) ;; clean up ugly crosswalk keys; what in gods name have we done?
      (let [[srv-flow-key mapping-key] subbed-sub
            sub                        (get-in @atoms-and-watchers [client-name srv-flow-key] {})]
        (ut/pp [:unsubbing*w.var! client-name flow-key sub])
        (remove-watcher (:keypath sub) client-name (:sub-type sub) flow-id (:flow-key sub))
        (swap! param-var-mapping dissoc [client-name mapping-key]) ;; compound (single) key
        (swap! param-var-crosswalk ut/dissoc-in [client-name flow-key])
        (swap! param-var-key-mapping assoc
          client-name
          (vec (filter #(not (= (first %) flow-key)) (get @param-var-key-mapping client-name)))))
      (let [sub (get-in @atoms-and-watchers [client-name flow-key] {})]
        (ut/pp [:unsubbing! client-name flow-key sub])
        (remove-watcher (:keypath sub) client-name (:sub-type sub) flow-id (:flow-key sub))))))

(defn remove-watchers-for-flow
  [flow-id & [client-name]]
  (doseq [[c-name subs] @atoms-and-watchers
          :let          [matching-subs (filter #(= (:flow-id %) flow-id) (vals subs))]
          :when         (ut/ne? matching-subs)]
    (ut/pp [:matching-subs c-name matching-subs])
    (doseq [sub matching-subs]
      (do (ut/pp [:removing (count matching-subs) :watchers :for flow-id c-name
                  [[(:keypath sub) c-name (:sub-type sub) flow-id (:flow-key sub)]]])
          (remove-watcher (:keypath sub) c-name (:sub-type sub) flow-id (:flow-key sub))))))


(defn break-up-flow-key
  [key]
  (let [ff  (cstr/split (-> (str key)
                            (cstr/replace #":" ""))
                        #"/")
        ff2 (cstr/split (last ff) #">")]
    [(first ff2) (keyword (last ff2))]))

(declare client-kp)

(defn clover-lookup
  [client-name flow-key & [signal?]]
  (let [flow-key-orig           flow-key
        flow-key-sub            (replace-flow-key-vars flow-key client-name)
        flow-key-split          (break-up-flow-key flow-key)
        flow-key-split-sub      (break-up-flow-key flow-key-sub)
        vars?                   (not (= flow-key flow-key-sub))
        flow-key                (if vars? flow-key-sub flow-key)
        flow-key-split          (if vars? flow-key-split-sub flow-key-split)
        [flow-id step-id]       flow-key-split ;(break-up-flow-key flow-key)
        keypath                 [flow-id step-id]
        sub-path                (break-up-flow-key-ext flow-key)
        base-type               (first sub-path)
        flow-client-param-path  (keyword (cstr/replace (str (first keypath) (last keypath)) #":" ">"))
        other-client-param-path (keyword (cstr/replace (cstr/join ">" (vec (rest sub-path))) ":" ""))
        client-param-path       (if (= base-type :flow) flow-client-param-path other-client-param-path)
        client-keypath          (client-kp flow-key keypath base-type sub-path client-param-path)
        ssp                     (break-up-flow-key-ext flow-key-orig)
        req-client-kp           (client-kp flow-key-orig
                                           (vec (break-up-flow-key flow-key-orig))
                                           base-type
                                           ssp
                                           (keyword (cstr/replace (cstr/join ">" (vec (rest ssp))) ":" "")))
        _ (ut/pp [:flow-key flow-key vars? flow-key-sub flow-key-split flow-key-split-sub]) ;; this
        _ (when vars? (swap! param-var-mapping assoc [client-name client-keypath] req-client-kp))
        _ (when vars? (swap! param-var-crosswalk assoc-in [client-name flow-key-orig] [flow-key [client-name client-keypath]])) ;; all
                                                                                                                                ;; the
                                                                                                                                ;; precomputed
                                                                                                                                ;; values
                                                                                                                                ;; to
                                                                                                                                ;; delete
        _ (when vars?
            (swap! param-var-key-mapping assoc
              client-name
              (vec (distinct (conj (get @param-var-key-mapping client-name []) [flow-key-orig flow-key])))))
        lv                      (get @last-values keypath)]
    (ut/pp [:solver-lookup! flow-key base-type client-param-path keypath {:sub-path sub-path}])
    (cond
      (cstr/includes? (str flow-key) "*running?") false
      (= base-type :time)                         (get @time-atom client-param-path)
      (= base-type :signal)                       (get @last-signals-atom client-param-path)
      (= base-type :data)                         (get-in @last-solvers-data-atom
                                                          (vec (into [(keyword (second sub-path))] (vec (rest (rest sub-path)))))
                                                          lv)
      (= base-type :solver)                       (get-in @last-solvers-atom
                                                          (vec (into [(keyword (second sub-path))] (vec (rest (rest sub-path)))))
                                                          lv)
      (= base-type :solver-meta)                  (get-in @last-solvers-atom-meta
                                                          (vec (into [(keyword (second sub-path))] (vec (rest (rest sub-path)))))
                                                          lv)
      (= base-type :signal-history)               (get-in @last-signals-history-atom
                                                          (vec (into [(keyword (second sub-path))] (vec (rest (rest sub-path)))))
                                                          lv)
      (= base-type :server)                       (get @server-atom client-param-path lv)
      (= base-type :screen)                       (get-in @screens-atom (vec (rest sub-path)) lv)
      (= base-type :client)                       (get-in @params-atom
                                                          (vec (into [(keyword (second sub-path))] (vec (rest (rest sub-path)))))
                                                          lv)
      (= base-type :panel)                        (get-in @panels-atom
                                                          (vec (into [(keyword (second sub-path))] (vec (rest (rest sub-path)))))
                                                          lv)
      :else                                       (get-in @flow-db/results-atom keypath lv) ;; assume
    )))



(defn millis-to-date
  [millis]
  (let [zdt       (java.time.ZonedDateTime/ofInstant (java.time.Instant/ofEpochMilli millis) (java.time.ZoneId/systemDefault))
        formatter (java.time.format.DateTimeFormatter/ofPattern "yyyy-MM-dd HH:mm:ss SSS")]
    (.format zdt formatter)))

(declare query-runstream)

(defn solver-sql
  [solver-name honey-sql snapshot?] ;; basically same as front-end - convert the query to a
  (ut/pp [:solver-sql [solver-name (str honey-sql) snapshot?]])
  (let [style-rules    (get honey-sql :style-rules)
        orig-honey-sql honey-sql
        connection-id  (get honey-sql :connection-id)
        has-rules?     (and (not (nil? style-rules)) (ut/ne? style-rules))
        rules          (when has-rules?
                         (vec (for [[[col name] logic] style-rules]
                                [[:case (:logic logic) 1 :else 0] (keyword (str "styler_" (ut/safe-name name)))])))
        clover-sql     (assoc honey-sql :connection-id "system-db")
        honey-sql      (ut/clean-sql-from-ui-keys honey-sql)
        hselect        (get honey-sql :select)
        flat           (ut/deep-flatten honey-sql)
        literal-data?  (and (some #(= % :data) flat) (not (some #(= % :panel_history) flat)))
        honey-modded   (if has-rules? (assoc honey-sql :select (apply merge hselect rules)) honey-sql)
        client-name    :rvbbit-solver
        honey-modded   (walk/postwalk-replace {:*client-name client-name :*client-name-str (pr-str client-name)} honey-modded)
        client-cache?  (if literal-data? (get honey-sql :cache? true) false)]
    (ut/pp [:solver-sql! solver-name orig-honey-sql honey-sql honey-modded connection-id client-name snapshot?])
    (query-runstream :honey-xcall
                     [:solvers solver-name]
                     honey-modded
                     client-cache?
                     false
                     connection-id
                     client-name
                     -1
                     nil
                     clover-sql
                     false
                     snapshot?)))


(defn run-solver
  [solver-name client-name & [override-map override-input temp-solver-name]]
  (let [solver-map            (if (ut/ne? override-map) override-map (get @solvers-atom solver-name))
        input-map             (get solver-map :input-map {})
        input-map             (if (ut/ne? override-input) (merge input-map override-input) input-map)
        use-cache?            (true? (get solver-map :cache? false))
        vdata                 (walk/postwalk-replace input-map (get solver-map :data)) ;; if we have input maps or
                                                                                       ;; overrides, do that first
                                                                                       ;; so it's part of vdata now
        vdata-clover-kps      (vec (filter #(and (keyword? %) ;; get any resolvable keys in the struct before we operate on
                                                              ;; it
                                                 (cstr/includes? (str %) "/")
                                                 (not= % (keyword (str "solver/" (cstr/replace (str solver-name) ":" "")))))
                                     (ut/deep-flatten vdata)))
        solver-name           (if temp-solver-name ;; we might be passed a blank input-map, but as long as we have
                                                   ;; a temp-solver-name, we can still run it independently
                                temp-solver-name
                                solver-name)
        vdata-clover-walk-map (into {}
                                    (for [kp vdata-clover-kps]
                                      {kp (try (clover-lookup :rvbbit-solver kp)
                                               (catch Exception e
                                                 (ut/pp [:clover-lookup-error kp e])
                                                 (str "clover-param-lookup-error " kp)))}))
        vdata                 (if (ut/ne? vdata-clover-walk-map) (walk/postwalk-replace vdata-clover-walk-map vdata) vdata) 
        runner-name           (get solver-map :type :clojure)
        runner-map            (get-in config/settings [:runners runner-name] {})
        runner-type           (get runner-map :type runner-name)
        timestamp             (System/currentTimeMillis)
        cache-key             [vdata runner-name] ;; since we could have 2 connections with the
        cache-val             (get @solvers-cache-atom cache-key)
        cache-hit?            (and (and use-cache? (ut/ne? cache-val)) (not= runner-type :sql))
        err?                  (fn [s]
                                (let [s (str s)]
                                  (or (cstr/includes? s "Exception") (cstr/includes? s ":err") (cstr/includes? s ":error"))))
        timestamp-str         (cstr/trim (str (when use-cache? "^") (when cache-hit? "*") " " (millis-to-date timestamp)))]
    ;; (when override-input (ut/pp [:SOLVER-INPUT-OVERRIDE! temp-solver-name :wants input-map]))
    (cond
      cache-hit? (let [[output output-full] cache-val
                       meta-extra           {:extra {:last-processed timestamp-str :cache-hit? cache-hit? :elapsed-ms 0}}
                       timestamp-str        (str timestamp-str " (cache hit)")
                       new-history          (vec (conj (get @last-solvers-history-atom solver-name []) timestamp-str))] ;; regardless
                   (ut/pp [:cached-solver-hit! solver-name])
                   (swap! last-solvers-atom assoc solver-name output)
                   (swap! last-solvers-atom-meta assoc
                     solver-name
                     (merge meta-extra {:history (vec (reverse (take-last 20 new-history))) :error "none" :output output-full}))
                   (swap! last-solvers-history-atom assoc solver-name new-history))
      
      (= runner-type :sql)
        (try
          (let [snapshot?                   (true? (get solver-map :snapshot? false))
                {:keys [result elapsed-ms]} (ut/timed-exec (solver-sql solver-name vdata snapshot?))
                cache-table-name            (ut/keypath-munger [:solvers solver-name])
                output                      (get result :result)
                output                      (vec (for [e output] (into {} e))) ;; had some odd
                _ (if snapshot? ;; [rowset table-name keypath client-name & columns-vec]
                    (insert-rowset-snap output cache-table-name solver-name client-name)
                    (insert-rowset      output cache-table-name solver-name client-name))
                data-key                    (try (keyword (str "data/" (cstr/replace (str solver-name) ":" "")))
                                                 (catch Exception e (str e)))
                test-query-sql              {:select        [:*]
                                             :connection-id "cache.db"
                                             :_cached-at    (get (ut/current-datetime-parts) :now) ;; will
                                             :_cache-query  (str data-key)
                                             :from          [[(keyword cache-table-name) :extract]]}
                rows                        (count output)
                output-full                 (-> result
                                                (dissoc :result)
                                                (assoc :panel-key solver-name)
                                                (assoc :rowcount rows)
                                                (assoc :full-data-param (str data-key))
                                                (assoc :cached-table-name cache-table-name)
                                                (assoc :test-query test-query-sql))
                error?                      (err? output-full)
                runs                        (get @last-solvers-history-atom solver-name [])
                meta-extra                  {:extra {:last-processed timestamp-str
                                                     :cache-hit?     cache-hit?
                                                     :elapsed-ms     elapsed-ms
                                                     :runs           (count runs)
                                                     :error?         error?}}
                timestamp-str               (str timestamp-str " (" elapsed-ms "ms, " rows " rows)")
                new-history                 (vec (conj runs timestamp-str))]
            (ut/pp [:running-sql-solver solver-name :data-key data-key])
            (swap! last-solvers-atom assoc solver-name test-query-sql)
            ;(swap! last-solvers-data-atom assoc solver-name output) ;; full data can be clover
            (swap! last-solvers-atom-meta assoc
              solver-name
              (merge meta-extra {:history (vec (reverse (take-last 20 new-history))) :error "none" :output output-full}))
            (swap! last-solvers-history-atom assoc solver-name new-history)
            (swap! solvers-cache-atom assoc cache-key [output output-full]))
          (catch Throwable e
            (do (ut/pp [:SOLVER-SQL-ERROR!!! (str e) :tried vdata :for solver-name :runner-type runner-type])
                (swap! last-solvers-atom-meta assoc solver-name {:error (str e)}))))
      
      (= runner-type :nrepl)
        (try (let [repl-host                   (get-in runner-map [:runner :host])
                   repl-port                   (get-in runner-map [:runner :port])
                   {:keys [result elapsed-ms]} (ut/timed-exec (evl/repl-eval vdata repl-host repl-port))
                   output-full                 result
                   output                      (last (get-in output-full [:evald-result :value]))
                   output-full                 (-> output-full
                                                   (assoc-in [:evald-result :output-lines]
                                                             (try (count (remove empty?
                                                                           (get-in output-full [:evald-result :out])))
                                                                  (catch Exception _ 0)))
                                                   (assoc-in [:evald-result :values]
                                                             (try (count (last (get-in output-full [:evald-result :value])))
                                                                  (catch Exception _ 0))))
                   error?                      (err? output-full)
                   runs                        (get @last-solvers-history-atom solver-name [])
                   meta-extra                  {:extra {:last-processed timestamp-str
                                                        :cache-hit?     cache-hit?
                                                        :elapsed-ms     elapsed-ms
                                                        :runs           (count runs)
                                                        :error?         error?}}
                   timestamp-str               (str timestamp-str " (" elapsed-ms "ms)")
                   new-history                 (vec (conj runs timestamp-str))]
               (swap! last-solvers-atom assoc solver-name output)
               (swap! last-solvers-atom-meta assoc
                 solver-name
                 (merge meta-extra {:history (vec (reverse (take-last 20 new-history))) :error "none" :output output-full}))
               (swap! last-solvers-history-atom assoc solver-name new-history)
               (swap! solvers-cache-atom assoc cache-key [output output-full]))
             (catch Throwable e
               (do (ut/pp [:SOLVER-REPL-ERROR!!! (str e) :tried vdata :for solver-name :runner-type runner-type])
                   (swap! last-solvers-atom-meta assoc solver-name {:error (str e)}))))
      
      (= runner-type :flow) ;; no runner def needed for anon flow pulls
        (try (let [client-name                 :rvbbit-solver
                   flowmap                     (get vdata :flowmap)
                   opts                        (merge {:close-on-done? true :increment-id? false :timeout 10000}
                                                      (get vdata :opts {}))
                   return                      (get vdata :return) ;; alternate block-id to fetch
                   flow-id                     (str (cstr/replace (str solver-name) ":" "") "-solver-flow-")
                   {:keys [result elapsed-ms]} (ut/timed-exec (flow! client-name flowmap nil flow-id opts))
                   output                      result
                   output                      (ut/remove-namespaced-keys (ut/replace-large-base64 output))
                   output-val                  (get-in output [:return-val])
                   output-val                  (if return ;;(keyword? return)
                                                 (get-in output [:return-maps flow-id return] output-val)
                                                 output-val)
                   output-full                 {:req        vdata
                                                :value      (-> (assoc output :return-maps
                                                                       (select-keys (get output :return-maps) [flow-id]))
                                                                (dissoc :tracker)
                                                                (dissoc :tracker-history))
                                                :output-val output-val
                                                :flow-id    flow-id
                                                :return     return}
                   error?                      (err? output-full)
                   runs                        (get @last-solvers-history-atom solver-name [])
                   meta-extra                  {:extra {:last-processed timestamp-str
                                                        :cache-hit?     cache-hit?
                                                        :elapsed-ms     elapsed-ms
                                                        :runs           (count runs)
                                                        :error?         error?}}
                   timestamp-str               (str timestamp-str " (" elapsed-ms "ms)")
                   new-history                 (conj runs timestamp-str)]
               (swap! last-solvers-atom assoc solver-name output-val)
               (swap! last-solvers-atom-meta assoc
                 solver-name
                 (merge meta-extra {:history (vec (reverse (take-last 20 new-history))) :error "none" :output output-full}))
               (swap! last-solvers-history-atom assoc solver-name new-history)
               (swap! solvers-cache-atom assoc cache-key [output-val output-full])
               (swap! flow-db/results-atom dissoc flow-id)) ;; <-- clear out the flow atom right
             (catch Throwable e
               (do (ut/pp [:SOLVER-FLOW-ERROR!!! (str e) :tried vdata :for solver-name :runner-type runner-type])
                   (swap! last-solvers-atom-meta assoc solver-name {:error (str e)}))))
      :else ;; else we assume it's just data and keep it as is
        (let [output-full   {:static-data vdata}
              runs          (get @last-solvers-history-atom solver-name [])
              meta-extra    {:extra {:last-processed timestamp-str :cache-hit? cache-hit? :elapsed-ms -1 runs (count runs)}}
              timestamp-str (str timestamp-str " (static)")
              new-history   (conj runs timestamp-str)]
          (ut/pp [:solver-static solver-name vdata])
          (swap! last-solvers-atom assoc solver-name vdata)
          (swap! last-solvers-atom-meta assoc
            solver-name
            (merge meta-extra {:history (vec (reverse (take-last 20 new-history))) :error "none" :output output-full}))
          (swap! last-solvers-history-atom assoc solver-name new-history)
          vdata))))

(defonce last-signals-history-atom-temp (atom {}))

(defn process-signal
  [signal-name & [solver-dep?]]
  (doall
    (let [signals-map (select-keys @signals-atom [signal-name])
          signals-parts-map (into {} (for [[k {:keys [signal]}] signals-map] {k (vec (distinct (ut/where-dissect signal)))}))
          resolve-changed-fn (fn [obody sigk]
                               (let [kps       (ut/extract-patterns obody :changed? 2)
                                     logic-kps (into {}
                                                     (for [v kps]
                                                       (let [[_ & ss] v
                                                             ss       (first ss)
                                                             v2       (get-in @last-signal-value-atom [sigk ss]) ;; should
                                                                                                                 ;; be
                                                             new      [:not [:= v2 ss]] ;; has it
                                                            ]
                                                         {v new}))) ;; compare against the last
                                    ]
                                 (walk/postwalk-replace logic-kps obody)))
          signals-resolve-map
            (into
              {}
              (for [[k {:keys [signal]}] signals-map]
                {k (let [cmpkeys (vec (filter #(and (keyword? %) (cstr/includes? (str %) "/")) (ut/deep-flatten signal)))
                         vvals   (select-keys (get-in @atoms-and-watchers [:rvbbit-signals]) cmpkeys)]
                     (into {}
                           (for [[k {:keys [base-type sub-type sub-path keypath]}] vvals
                                 :let                                              [;;_ (when (not= base-type :time)
                                                                                    ;;(ut/pp [:resolver-map!!? k
                                                                                    v3 (get-in @(get-atom-from-keys base-type
                                                                                                                    sub-type
                                                                                                                    sub-path
                                                                                                                    keypath)
                                                                                               keypath
                                                                                               (get @last-values keypath))]] ;; extra
                                                                                                                             ;; cache
                                                                                                                             ;; for
                                                                                                                             ;; last
                                                                                                                             ;; value
                                                                                                                             ;; due
                                                                                                                             ;; to
                                                                                                                             ;; diff
                                                                                                                             ;; update
                                                                                                                             ;; cadence
                             {k v3}))                                                                                        ;; keypath
                                                                                                                             ;; is
                                                                                                                             ;; used
                                                                                                                             ;; as
                                                                                                                             ;; the
                                                                                                                             ;; actual
                                                                                                                             ;; key
                                                                                                                             ;; here,
                                                                                                                             ;; btw
                   )}))
          client-name :rvbbit-solver
          nowah (System/currentTimeMillis) ;; want to have a consistent timestamp for this
          signals-resolved (into {}
                                 (for [[k v] signals-map]
                                   {k (walk/postwalk-replace (get signals-resolve-map k)
                                                             (resolve-changed-fn (get v :signal) k))}))
          parts-work
            (into
              {} ;; important side effects / will transition to a doseq after debugging phase
              (for [[kk vv] signals-parts-map]
                {kk (into
                      {}
                      (for [vvv  vv
                            :let [rvv           (walk/postwalk-replace (get signals-resolve-map kk) (resolve-changed-fn vvv kk))
                                  honey-sql-str (to-sql {:select [[1 :vv]] :where rvv})
                                  result        (true? (ut/ne? (sql-query ghost-db honey-sql-str [:ghost-signal-resolve kk])))
                                  part-key      (keyword (str "part-" (cstr/replace (str kk) ":" "") "-" (ut/index-of vv vvv)))
                                  _ (when true ;(not= result (get @last-signals-atom part-key))
                                      (swap! last-signals-history-atom-temp assoc-in
                                        [kk part-key]
                                        (into (vec (take-last 12
                                                              (sort-by second
                                                                       (get-in @last-signals-history-atom-temp [kk part-key]))))
                                              [[result nowah]])))
                                  _ (when true ;(not= result (get @last-signals-atom vvv)) ;; no
                                      (swap! last-signals-history-atom-temp assoc-in
                                        [kk vvv]
                                        (into (vec (take-last 12
                                                              (sort-by second (get-in @last-signals-history-atom-temp [kk vvv]))))
                                              [[result nowah]])))
                                  _ (swap! last-signals-atom-stamp assoc vvv nowah)
                                  _ (swap! last-signals-atom assoc vvv result)
                                  _ (swap! last-signals-atom assoc part-key result)]] ;; last time
                        {[vvv rvv] result}))}))
          full-parts-work
            (into {} ;; all side effects / will transition to a doseq after debugging phase
                  (for [[kk vv] signals-resolved
                        :let    [honey-sql-str (to-sql {:select [[1 :vv]] :where vv})
                                 result        (true? (ut/ne? (sql-query ghost-db honey-sql-str [:ghost-signal-resolve kk])))
                                 _ (when true ;(not= result (get @last-signals-atom kk))  ;; no
                                     (swap! last-signals-history-atom-temp assoc-in
                                       [kk kk]
                                       (into (vec (take-last 19
                                                             (sort-by second (get-in @last-signals-history-atom-temp [kk kk]))))
                                             [[result nowah]])))
                                 _ (swap! last-signals-atom assoc kk result)
                                 _ (when (and solver-dep? (true? result)) ;; singal resolved as
                                     (doseq [[sk sv] @solvers-atom
                                             :when   (or (= (get sv :signal) kk)
                                                         (= (get sv :signal)
                                                            (keyword (str "signal/" (cstr/replace (str kk) ":" "")))))]
                                       (enqueue-task4 (fn [] (run-solver sk client-name)))))
                                 _ (swap! last-signals-history-atom assoc kk (get @last-signals-history-atom-temp kk)) ;; one
                                                                                                                       ;; write,
                                                                                                                       ;; to
                                 _ (swap! last-signals-atom-stamp assoc kk nowah)]]
                    {[kk vv] result}))]
      (doseq [[k v] signals-resolve-map] (doseq [[kk vv] v] (swap! last-signal-value-atom assoc-in [k kk] vv))) ;; when all
                                                                                                                ;; done, set
    )))

(defn process-signals-reaction
  [base-type keypath new-value client-param-path]
  (let [re-con-key    (keyword (str (cstr/replace (str base-type) ":" "") "/" (cstr/replace (str client-param-path) ":" "")))
        valid-signals (map first (vec (filter #(some (fn [x] (= x re-con-key)) (last %)) @signal-parts-atom)))]
    (doseq [signal valid-signals
            :let   [solver-deps    (vec (distinct (for [[_ v] @solvers-atom]
                                                    (keyword (-> (get v :signal)
                                                                 str
                                                                 (cstr/replace ":signal/" "")
                                                                 (cstr/replace ":" ""))))))
                    solver-to-run? (true? (some #(= signal %) solver-deps))]] ;; run in parallel
      (process-signal signal solver-to-run?))))

(declare process-solver)

(defn process-solvers-reaction
  [base-type keypath new-value client-param-path]
  (let [re-con-key    (keyword (str (cstr/replace (str base-type) ":" "") "/" (cstr/replace (str client-param-path) ":" "")))
        valid-signals (map first (vec (filter #(some (fn [x] (= x re-con-key)) (last %)) @signal-parts-atom)))]
    (doseq [signal valid-signals] ;; run in parallel later perhaps
      (process-solver signal))))


(defn send-reaction
  [base-type keypath client-name new-value]
  (let [;;_ (ut/pp [:send-reaction-keypath keypath client-name])
        keypath                 (get @param-var-mapping [client-name keypath] keypath) ;; get the
        flow-client-param-path  (keyword (cstr/replace (str (first keypath) (last keypath)) #":" ">"))
        other-client-param-path (keyword (cstr/replace (cstr/join ">" keypath) ":" "")) ;;(keyword
        client-param-path       (if (= base-type :flow) flow-client-param-path other-client-param-path)
        signal?                 (= client-name :rvbbit-signals)]
    (if (not signal?)
      (kick client-name [(or base-type :flow) client-param-path] new-value nil nil nil)
      (do ;;(ut/pp [:signal-or-solver base-type keypath client-name])
        (process-signals-reaction base-type keypath new-value client-param-path)))))





(defn send-reaction-runner
  [base-type keypath client-name new-value]
  (let [flow-id (first keypath)]
    (ut/pp [:reaction-runner flow-id keypath client-name]) ;; (ut/replace-large-base64
    (kick client-name (vec (cons :flow-runner keypath)) new-value nil nil nil)))

(defn purge-dead-client-watchers
  []
  (let [cc (dissoc (client-statuses) :rvbbit-scheduler)]
    (doseq [[k {:keys [last-seen-seconds]}] cc
            :let                            [subs (get @atoms-and-watchers k)]
            :when                           (> last-seen-seconds 6000)]
      (ut/pp [:dead-client :cleaning-up k])
      (swap! atoms-and-watchers dissoc k) ;; remove client from watchers atom
      (swap! ack-scoreboard dissoc k)     ;; remove client client board
    )))

(defn accumulate-unique-runs
  [data]
  (let [merge-runs (fn [runs run]
                     (let [run-start (:start run)
                           run-end   (:end run)]
                       (cond (some #(and (= (:start %) run-start) (= (:end %) run-end)) runs) runs
                             (some #(and (= (:start %) run-start) (nil? (:end %)) run-end) runs)
                               (conj (vec (remove #(and (= (:start %) run-start) (nil? (:end %))) runs)) run)
                             :else (conj runs run))))]
    (reduce (fn [acc entry]
              (reduce (fn [inner-acc [block-id run]] (update inner-acc block-id (fn [runs] (merge-runs (or runs []) run))))
                acc
                (into [] entry)))
      {}
      data)))

(defn send-tracker-runner
  [base-type keypath client-name new-value]
  (let [flow-id (first keypath)]
    (let [orig-tracker   (get @flow-db/tracker flow-id)
          tracker        (into {}
                               (for [[k v] orig-tracker ;; remove condi non-starts. dont send
                                     :when (not (get v :in-chan?))]
                                 {k v}))
          condis         (get-in @flow-db/status [flow-id :condis])
          running?       (get @flow-status flow-id :*running?)
          first-tracker? (empty? (get-in @tracker-client-only [flow-id client-name]))]
      (when true ;running? ;; true ;(and new? running?) ;; (not= tracker last-tracker)
        (swap! tracker-client-only assoc-in [flow-id client-name] tracker)
        (when false ;; DEPRECATED ;(not running?) ;; test to stop thrash... worth it?
          (kick client-name (vec (cons :tracker keypath)) (if first-tracker? (assoc tracker :*start! [{}]) tracker) nil nil nil))
        (when (ut/ne? condis) ;; still need this little guy tho
          (kick client-name (vec (cons :condis keypath)) condis nil nil nil))))))

(defn send-acc-tracker-runner
  [base-type keypath client-name new-value]
  (let [flow-id      (first keypath)
        orig-tracker (get @flow-db/tracker flow-id)
        tracker      (into {}
                           (for [[k v] orig-tracker ;; remove condi non-starts. dont send to
                                 :when (not (get v :in-chan?))]
                             {k v}))
        all-tracks   (vec (conj (get @acc-trackers flow-id []) tracker))
        acc-tracker  (accumulate-unique-runs all-tracks)]
    (swap! acc-trackers assoc flow-id all-tracks)   ;; for next acc
    (kick client-name (vec (cons :acc-tracker keypath)) acc-tracker nil nil nil)))

(defn send-tracker-block-runner
  [base-type keypath client-name new-value]
  (let [flow-id (first keypath)]
    (let []
      (kick client-name
            (vec (cons :tracker-blocks keypath))
            (select-keys (get @flow-status flow-id) [:running-blocks :done-blocks :error-blocks :waiting-blocks])
            nil
            nil
            nil))))

(defn client-kp
  [flow-key keypath base-type sub-path client-param-path]
  (cond (cstr/includes? (str flow-key) "*running?") false
        (= base-type :time)                         client-param-path
        (= base-type :signal)                       client-param-path
        (= base-type :solver)                       (vec (into [(keyword (second sub-path))] (vec (rest (rest sub-path)))))
        (= base-type :solver-meta)                  (vec (into [(keyword (second sub-path))] (vec (rest (rest sub-path)))))
        (= base-type :signal-history)               (vec (into [(keyword (second sub-path))] (vec (rest (rest sub-path))))) ;;(vec
        (= base-type :server)                       client-param-path
        (= base-type :screen)                       (vec (rest sub-path))
        (= base-type :client)                       (vec (into [(keyword (second sub-path))] (vec (rest (rest sub-path)))))
        (= base-type :panel)                        (vec (into [(keyword (second sub-path))] (vec (rest (rest sub-path)))))
        :else                                       keypath ;; assume flow
  ))

(defn sub-to-value
  [client-name flow-key & [signal?]] ;;; IF CHANGED, REMEMBER TO ALSO UPDATE "CLOVER-LOOKUP" -
  (let [flow-key-orig           flow-key
        flow-key-sub            (replace-flow-key-vars flow-key client-name)
        flow-key-split          (break-up-flow-key flow-key)
        flow-key-split-sub      (break-up-flow-key flow-key-sub)
        vars?                   (not (= flow-key flow-key-sub))
        flow-key                (if vars? flow-key-sub flow-key)
        flow-key-split          (if vars? flow-key-split-sub flow-key-split)
        [flow-id step-id]       flow-key-split ;(break-up-flow-key flow-key)
        signal?                 (true? signal?)
        keypath                 [flow-id step-id]
        sub-path                (break-up-flow-key-ext flow-key)
        base-type               (first sub-path)
        flow-client-param-path  (keyword (cstr/replace (str (first keypath) (last keypath)) #":" ">"))
        other-client-param-path (keyword (cstr/replace (cstr/join ">" (vec (rest sub-path))) ":" ""))
        client-param-path       (if (= base-type :flow) flow-client-param-path other-client-param-path)
        client-keypath          (client-kp flow-key keypath base-type sub-path client-param-path)
        ssp                     (break-up-flow-key-ext flow-key-orig)
        req-client-kp           (client-kp flow-key-orig
                                           (vec (break-up-flow-key flow-key-orig))
                                           base-type
                                           ssp
                                           (keyword (cstr/replace (cstr/join ">" (vec (rest ssp))) ":" "")))
        _ (ut/pp [:flow-key flow-key vars? flow-key-sub flow-key-split flow-key-split-sub]) ;; this
        _ (when vars? (swap! param-var-mapping assoc [client-name client-keypath] req-client-kp))
        _ (when vars? (swap! param-var-crosswalk assoc-in [client-name flow-key-orig] [flow-key [client-name client-keypath]])) ;; all
                                                                                                                                ;; the
                                                                                                                                ;; precomputed
                                                                                                                                ;; values
                                                                                                                                ;; to
                                                                                                                                ;; delete
        _ (when vars?
            (swap! param-var-key-mapping assoc
              client-name
              (vec (distinct (conj (get @param-var-key-mapping client-name []) [flow-key-orig flow-key])))))
        lv                      (get @last-values keypath)]
    (ut/pp [:client-sub! (if signal? :signal! :regular!) client-name :wants base-type client-param-path keypath
            {:sub-path sub-path} flow-key])
    (when (get-in @flow-db/results-atom keypath) (ut/pp [:react (get-in @flow-db/results-atom keypath)]))
    (add-watcher keypath client-name send-reaction flow-key :param-sub)
    (when (not signal?)
      (kick client-name
            [base-type client-param-path]
            (cond (cstr/includes? (str flow-key) "*running?") false
                  (= base-type :time)                         (get @time-atom client-param-path)
                  (= base-type :signal)                       (get @last-signals-atom client-param-path)
                  (= base-type :data)                         (get-in @last-solvers-data-atom
                                                                      (vec (into [(keyword (second sub-path))]
                                                                                 (vec (rest (rest sub-path)))))
                                                                      lv)
                  (= base-type :solver)                       (get-in @last-solvers-atom
                                                                      (vec (into [(keyword (second sub-path))]
                                                                                 (vec (rest (rest sub-path)))))
                                                                      lv)
                  (= base-type :solver-meta)                  (get-in @last-solvers-atom-meta
                                                                      (vec (into [(keyword (second sub-path))]
                                                                                 (vec (rest (rest sub-path)))))
                                                                      lv)
                  (= base-type :signal-history)               (get-in @last-signals-history-atom
                                                                      (vec (into [(keyword (second sub-path))]
                                                                                 (vec (rest (rest sub-path)))))
                                                                      lv)
                  (= base-type :server)                       (get @server-atom client-param-path lv)
                  (= base-type :screen)                       (get-in @screens-atom (vec (rest sub-path)) lv)
                  (= base-type :client)                       (get-in @params-atom
                                                                      (vec (into [(keyword (second sub-path))]
                                                                                 (vec (rest (rest sub-path)))))
                                                                      lv)
                  (= base-type :panel)                        (get-in @panels-atom
                                                                      (vec (into [(keyword (second sub-path))]
                                                                                 (vec (rest (rest sub-path)))))
                                                                      lv)
                  :else                                       (get-in @flow-db/results-atom keypath lv) ;; assume
            )
            nil
            nil
            nil))
    [:client-sub-request flow-id :step-id step-id :client-name client-name]))


(defmethod wl/handle-request :sub-to-flow-value [{:keys [client-name flow-key]}] (doall (sub-to-value client-name flow-key)))


(defn remove-watchers-for-flow22
  [flow-id & [client-name]]
  (doseq [[c-name subs] @atoms-and-watchers
          :let          [matching-subs (filter #(= (:flow-id %) flow-id) (vals subs))]
          :when         (not (empty? matching-subs))]
    (ut/pp [:matching-subs c-name matching-subs])
    (doseq [sub matching-subs]
      (do (ut/pp [:removing (count matching-subs) :watchers :for flow-id c-name
                  [[(:keypath sub) c-name (:sub-type sub) flow-id (:flow-key sub)]]])
          (remove-watcher (:keypath sub) c-name (:sub-type sub) flow-id (:flow-key sub))))))

(defn reload-signals-subs
  []
  (let [parts         (vec (for [[signal-name {:keys [signal]}] @signals-atom]
                             [signal-name
                              (vec (filter #(and (keyword? %) (cstr/includes? (str %) "/")) ;; get
                                     (ut/deep-flatten signal)))]))
        curr-keyparts (vec (keys (get @atoms-and-watchers :rvbbit-signals)))
        all-keyparts  (vec (distinct (ut/deep-flatten (map last parts))))
        to-remove     (vec (cset/difference (set curr-keyparts) (set all-keyparts)))]
    (reset! signal-parts-atom parts) ;; faster than running it every time - since we need it
    (ut/pp [:reload-signals-subs! parts curr-keyparts all-keyparts {:remove! to-remove}])
    (doseq [rm to-remove] ;; clean up ones we dont need to watch anymore
      (let [{:keys [sub-type keypath flow-key]} (get-in @atoms-and-watchers [:rvbbit-signals rm])]
        (remove-watcher keypath :rvbbit-signals sub-type nil flow-key)))
    (doseq [kk all-keyparts] ;; re-add them all - TODO, only add new ones (however, they get
      (ut/pp [:signal-sub-to kk])
      (sub-to-value :rvbbit-signals kk true))
    (reset! last-signals-atom (select-keys @last-signals-atom
                                           (vec (filter #(not (cstr/includes? (str %) "/part-")) (keys @last-signals-atom))))) ;; clear
                                                                                                                               ;; cache
                                                                                                                               ;; of
    (doseq [signal (keys @signals-atom)] ;; (re)process everythiung since we just got updated
      (ut/pp [:re-processing-signal signal])
      (process-signal signal))))

(defn process-solver
  [signal-name]
  (let [signals-map (select-keys @signals-atom [signal-name])
        signals-parts-map (into {} (for [[k {:keys [signal]}] signals-map] {k (vec (distinct (ut/where-dissect signal)))}))
        resolve-changed-fn (fn [obody sigk]
                             (let [kps       (ut/extract-patterns obody :changed? 2)
                                   logic-kps (into {}
                                                   (for [v kps]
                                                     (let [[_ & ss] v
                                                           ss       (first ss)
                                                           v2       (get-in @last-signal-value-atom [sigk ss]) ;; should
                                                           new      [:not [:= v2 ss]] ;; has it
                                                          ]
                                                       {v new}))) ;; compare against the last
                                  ]
                               (walk/postwalk-replace logic-kps obody)))
        signals-resolve-map
          (into
            {}
            (for [[k {:keys [signal]}] signals-map]
              {k (let [cmpkeys (vec (filter #(and (keyword? %) (cstr/includes? (str %) "/")) (ut/deep-flatten signal)))
                       vvals   (select-keys (get-in @atoms-and-watchers [:rvbbit-signals]) cmpkeys)]
                   (into {}
                         (for [[k {:keys [base-type sub-type sub-path keypath]}] vvals
                               :let                                              [;;_ (when (not= base-type :time) (ut/pp
                                                                                  v3 (get-in @(get-atom-from-keys base-type
                                                                                                                  sub-type
                                                                                                                  sub-path
                                                                                                                  keypath)
                                                                                             keypath
                                                                                             (get @last-values keypath))]] ;; extra
                                                                                                                           ;; cache
                                                                                                                           ;; for
                                                                                                                           ;; last
                                                                                                                           ;; value
                                                                                                                           ;; due
                                                                                                                           ;; to
                                                                                                                           ;; diff
                                                                                                                           ;; update
                           {k v3}))                                                                                        ;; keypath
                                                                                                                           ;; is
                                                                                                                           ;; used
                                                                                                                           ;; as
                                                                                                                           ;; the
                                                                                                                           ;; actual
                                                                                                                           ;; key
                                                                                                                           ;; here,
                                                                                                                           ;; btw
                 )}))
        nowah (System/currentTimeMillis) ;; want to have a consistent timestamp for this
        signals-resolved (into {}
                               (for [[k v] signals-map]
                                 {k (walk/postwalk-replace (get signals-resolve-map k) (resolve-changed-fn (get v :signal) k))}))
        parts-work
          (into {} ;; important side effects / will transition to a doseq after debugging phase
                (for [[kk vv] signals-parts-map]
                  {kk (into
                        {}
                        (for [vvv  vv
                              :let [rvv           (walk/postwalk-replace (get signals-resolve-map kk) (resolve-changed-fn vvv kk))
                                    honey-sql-str (to-sql {:select [[1 :vv]] :where rvv})
                                    result        (true? (ut/ne? (sql-query ghost-db honey-sql-str [:ghost-signal-resolve kk])))
                                    part-key      (keyword (str "part-" (cstr/replace (str kk) ":" "") "-" (ut/index-of vv vvv)))
                                    _ (when true ;(not= result (get @last-signals-atom part-key))
                                        (swap! last-signals-history-atom assoc-in
                                          [kk part-key]
                                          (into (vec (take-last 12
                                                                (sort-by second
                                                                         (get-in @last-signals-history-atom [kk part-key]))))
                                                [[result nowah]])))
                                    _ (when true ;(not= result (get @last-signals-atom vvv)) ;; no
                                        (swap! last-signals-history-atom assoc-in
                                          [kk vvv]
                                          (into (vec (take-last 12 (sort-by second (get-in @last-signals-history-atom [kk vvv]))))
                                                [[result nowah]])))
                                    _ (swap! last-signals-atom-stamp assoc vvv nowah)
                                    _ (swap! last-signals-atom assoc vvv result)
                                    _ (swap! last-signals-atom assoc part-key result)]] ;; last time
                          {[vvv rvv] result}))}))
        full-parts-work
          (into {} ;; important side effects / will transition to a doseq after debugging phase
                (for [[kk vv] signals-resolved
                      :let    [honey-sql-str (to-sql {:select [[1 :vv]] :where vv})
                               result        (true? (ut/ne? (sql-query ghost-db honey-sql-str [:ghost-signal-resolve kk])))
                               _ (when true ;(not= result (get @last-signals-atom kk))  ;; no
                                   (swap! last-signals-history-atom assoc-in
                                     [kk kk]
                                     (into (vec (take-last 19 (sort-by second (get-in @last-signals-history-atom [kk kk]))))
                                           [[result nowah]])))
                               _ (swap! last-signals-atom assoc kk result)
                               _ (swap! last-signals-atom-stamp assoc kk nowah)]]
                  {[kk vv] result}))]
    (doseq [[k v] signals-resolve-map] (doseq [[kk vv] v] (swap! last-signal-value-atom assoc-in [k kk] vv))) ;; when all done,
                                                                                                              ;; set
                                                                                                              ;; "last-value"
  ))
(defn reload-solver-subs
  []
  (let [parts         (vec (for [[solver-name {:keys [signal]}] @solvers-atom]
                             [solver-name
                              (vec (filter #(and (keyword? %) (cstr/includes? (str %) "/")) ;; get
                                     (ut/deep-flatten [signal])))])) ;; enclose in case its
        curr-keyparts (vec (keys (get @atoms-and-watchers :rvbbit-signals)))
        all-keyparts  (vec (distinct (ut/deep-flatten (map last parts))))
        to-remove     (vec (filter #(cstr/starts-with? (str %) ":solver/")
                             (cset/difference (set curr-keyparts) (set all-keyparts))))]
    (ut/pp [:reload-solver-subs! {:parts parts :curr-keypaths curr-keyparts :all-keyparts all-keyparts :to-remove to-remove}])
    (doseq [kk all-keyparts]
      (ut/pp [:solver-sub-to kk])
      (sub-to-value :rvbbit-signals kk true))
    (doseq [signal (keys @solvers-atom)] ;; (re)process everythiung since we just got updated
      (ut/pp [:re-processing-solver signal]) ;; as if it was brand new... like an initial sub
    )))
(defn gen-flow-keys
  [flow-id client-name]
  (let [ppath             (if (cstr/ends-with? (cstr/lower-case flow-id) ".edn")
                            flow-id ;; already a file path
                            (str "./flows/" flow-id ".edn"))
        raw               (try (edn/read-string (slurp ppath))
                               (catch Exception _
                                 (do (ut/pp [:error-reading-flow-from-disk-gen-flow-keys flow-id client-name]) {})))
        flowmaps          (process-flow-map (get raw :flowmaps))
        connections       (get raw :flowmaps-connections)
        server-flowmap    (process-flowmap2 flowmaps connections flow-id)
        comps             (get server-flowmap :components) ;; (assoc (get server-flowmap
        running-view-subs (vec
                            (for [[k v] comps :when (get v :view)] [flow-id (keyword (str (cstr/replace (str k) ":" "") "-vw"))]))
        running-subs      (vec (for [k (keys comps)] [flow-id k]))
        running-subs      (vec (into running-subs running-view-subs))]
    running-subs))

(defmethod wl/handle-request :sub-to-running-values
  [{:keys [client-name flow-keys flow-id]}]
  (ut/pp [:sub-to-running-values client-name flow-keys])
  (let [flow-keys (if (empty? flow-keys) (gen-flow-keys flow-id client-name) flow-keys)] ;; jumping
    (doseq [keypath flow-keys]
      (let [[flow-id step-id] keypath]
        (ut/pp [:client-sub-flow-runner flow-id :step-id step-id :client-name client-name])
        (add-watcher keypath
                     client-name
                     send-reaction-runner
                     (keyword (str "runner||" flow-id "||" (hash keypath)))
                     :flow-runner
                     flow-id)
        (add-watcher keypath
                     client-name
                     send-tracker-runner
                     (keyword (str "tracker||" flow-id "||" (hash keypath)))
                     :tracker
                     flow-id)
        (add-watcher keypath
                     client-name
                     send-tracker-block-runner
                     (keyword (str "blocks||" flow-id "||" (hash keypath)))
                     :tracker
                     flow-id)
        (add-watcher keypath
                     client-name
                     send-acc-tracker-runner
                     (keyword (str "acc-tracker||" flow-id "||" (hash keypath)))
                     :tracker
                     flow-id)))
    (boomerang-client-subs client-name))
  [:copy-that client-name])

(defmethod wl/handle-request :open-ai-push
  [{:keys [kind convo panels client-name]}]
  (doall (let [resp (assistants/chat convo)]
           (do ;(ut/write-csv recos-csv "./recos.csv")
             (ut/pp [:chat-resp resp])
             {:convo resp :client-name client-name}))))


(def sql-cache (atom (cache/lru-cache-factory {} :threshold 1000)))

(defn lookup-cache-exists? [key] (cache/has? @sql-cache key))

(defn get-from-cache [key] (cache/lookup @sql-cache key nil))

(defn insert-into-cache [key value] (swap! sql-cache assoc key value))

(defonce conn-map (atom {}))

(defn get-connection-string
  [connection-id]
  (let [;conn (sql-query-one system-db (to-sql {:select [:original_connection_str] :from
        conn (get @conn-map connection-id)]
    conn))

(defn data-type-value
  [v]
  (cond (or (cstr/includes? (str (type v)) "DateTime") (cstr/includes? (str (type v)) "TimeStamp")) "datetime"
        (cstr/includes? (str (type v)) "Date")                                       "date"
        (or (and (cstr/starts-with? (str v) "@@") (string? v)) (vector? v) (map? v)) "rabbit-code"
        (string? v)                                                                  "string"
        (integer? v)                                                                 "integer"
        (float? v)                                                                   "float"
        (boolean? v)                                                                 "boolean"
        :else                                                                        "unknown"))

(defn get-query-metadata
  [rowset query]
  (let [sample                rowset ;(repeatedly 100 (fn [] (rand-nth rowset)))
        sample-size           (count sample)
        selects               (get query :select)
        group-bys             (get query :group-by)
        no-group-by?          (and (empty? group-bys)
                                   (or (= selects [:*])
                                       (and (not (= 1 (count selects)))
                                            (or (not (some #(= % :count) (flatten selects)))
                                                (not (some #(= % :sum) (flatten selects)))
                                                (not (some #(= % :min) (flatten selects)))
                                                (not (some #(= % :max) (flatten selects)))))))
        selects-non-aliased   (into {} (for [s selects] (if (vector? s) {(first s) (last s)} {s s})))
        selects-only-aliased  (vec (for [s selects] (if (vector? s) (last s) s)))
        materialize-group-bys (vec (for [g group-bys]
                                     (cond (integer? g) (get selects-only-aliased (- g 1))
                                           (vector? g)  (get selects-non-aliased g)
                                           :else        g)))
        fields                (keys (first sample))
        field-data            (into {}
                                    (for [f fields]
                                      {f (let [fsamples         (map f sample)
                                               distinct-samples (count (distinct fsamples))
                                               commons          (into {} (take 3 (reverse (sort-by last (frequencies fsamples)))))
                                               data-type        (get-in (vec (reverse (sort-by last
                                                                                               (frequencies (map data-type-value
                                                                                                              fsamples)))))
                                                                        [0 0])]
                                           {:data-type   data-type
                                            :distinct    distinct-samples
                                            :group-by?   (if (not no-group-by?)
                                                           (true? (try (some #(= f %) materialize-group-bys)
                                                                       (catch Exception _ false)))
                                                           true)
                                            :commons     (if (cstr/includes? data-type "date")
                                                           (into {} (for [[k v] commons] {(str k) v}))
                                                           commons)
                                            :cardinality (int (* 100 (float (/ distinct-samples sample-size))))})}))]
    {:fields field-data :rowcount (count sample)}))









(def per-page-limit 200) ;; for sending to the UI, never allow more than this in each chunk sent (unless overridden by

(defmethod wl/handle-request :honey-call
  [{:keys [kind ui-keypath honey-sql client-name]}]
  (swap! q-calls2 inc)
  (inc-score! client-name :push)
  (doall
    (let [;req-hash (hash [kind ui-keypath honey-sql client-name])
          cache?         false ;(not (nil? (get @sql-cache req-hash)))
          per-page-limit 600 ;; override temp TODO (internal data only)
         ]
      (try (let [;honey-sql (dissoc honey-sql :last-known-fields)
                 page-num      (get honey-sql :page)
                 honey-sql     (if page-num (assoc (dissoc honey-sql :page) :offset (* page-num per-page-limit)) honey-sql)
                 honey-sql-str (to-sql honey-sql)
                 honey-result  (sql-query system-db honey-sql-str ui-keypath)
                 honey-meta    (get-query-metadata honey-result honey-sql)
                 fields        (get honey-meta :fields)
                 dates         (remove nil? (for [[k v] fields] (when (cstr/includes? (get v :data-type) "date") k)))
                 result        (if (empty? dates)
                                 (vec (take per-page-limit honey-result))
                                 (vec (for [r (take per-page-limit honey-result)]
                                        (into {} (for [[k v] r] (if (some #(= % k) dates) {k (str v)} {k v}))))))
                 output        {:kind        kind
                                :ui-keypath  ui-keypath
                                :result      result
                                :result-meta honey-meta
                                :client-name client-name
                                :map-order   (get @sql/map-orders honey-sql-str)}]
             (ut/ppln {:cached?     cache?
                       :kind        kind
                       :rows        (count result)
                       :ui-keypath  ui-keypath
                       :honey-sql   honey-sql
                       :client-name client-name})
             (do ;; (swap! sql-cache assoc req-hash output)
               output))
           (catch Exception e (ut/ppln [:error! e]))))))


(defn sniff-meta
  [ui-keypath honey-sql fields target-db client-name & [deep?]] ;; enqueue-task-sql-meta
  (try (let [honey-sql (-> honey-sql
                           (dissoc :offset)
                           (dissoc :page)
                           (dissoc :limit)
                           (dissoc :cache?))
             cnts      (into {}
                             (for [[[name f] hsql] (merge {[:rowcount :*] {:select [[[:count 1] :rowcnt]]
                                                                           :from   [[honey-sql :subq]]}}
                                                          (when deep?
                                                            (into {}
                                                                  (for [field (keys fields)]
                                                                    {[:distinct field] {:select [[[:count [:distinct field]]
                                                                                                  :distinct-values]]
                                                                                        :from   [[honey-sql :subq]]}}))))]
                               (let [str-sql    (to-sql hsql)
                                     sql-result (first (vals (first (sql-query target-db str-sql [ui-keypath :post-meta]))))
                                     res        {f sql-result}]
                                 (when (not (string? sql-result)) res))))] ;; smaller for websocket
                                                                           ;; js memory read-string
                                                                           ;; issues (distinct or
                                                                           ;; rowcount is
        ;;  (ut/pp [:running-meta-cnts ui-keypath client-name cnts])
         (push-to-client ui-keypath [:cnts-meta (first ui-keypath)] client-name 1 :cnts cnts))
       (catch Exception e (ut/pp [:error-w-sql-meta-cnts! e]))))

(defmethod wl/handle-request :selected-reco
  [{:keys [kind context_hash dm-type drag-meta combo_hash client-name]}]
  (cond
    (= dm-type :viz-reco)
      (let [combo-row-sql       {:select [:*] :from [:combos] :where [:= :combo_hash combo_hash]}
            combo-row           (sql-query system-db (to-sql combo-row-sql))
            shape-name          (get (first combo-row) :shape_name)
            key-hashes          (edn/read-string (get (first combo-row) :key_hashes))
            get-combo-field-sql (fn [k v]
                                  {:select [:yy501/connection_id ;:yy501/context_hash
                                            :yy501/data_type :yy501/db_catalog :yy501/db_schema :yy501/db_type :yy501/derived_calc
                                            :yy501/derived_name :yy501/field_name :yy501/field_type :bb977/logic_map
                                            :bb977/axes_key]
                                   :from   [[:fields :yy501]]
                                   :join   [[{:select [:axes_key :connection_id ;:context_hash
                                                       :db_catalog :db_schema :db_type :derived_calc :derived_name :field_name
                                                       :key_hash :logic_map ;:run_id
                                                      ]
                                              :from   [[:found_fields :xx420]]} :bb977]
                                            [:and [:= :bb977/key_hash :yy501/key_hash] [:= v :yy501/key_hash]
                                             [:= :bb977/axes_key k]]]})
            combo-fields-sql    {:union-all (vec (for [[k v] key-hashes] (get-combo-field-sql k v)))}]
        (doseq [row (sql-query system-db (to-sql combo-fields-sql))]
          (async/thread ;;; TODO replace with a serial agent send
            (let [lm        (edn/read-string (get row :logic_map))
                  fixed-row (into (sorted-map)
                                  (-> (merge lm row)
                                      (dissoc :logic_map)
                                      (dissoc :database_version)
                                      (dissoc :key_hash)
                                      (assoc :table_name (cstr/join "_" (drop-last (cstr/split (get lm :table_name) #"_"))))
                                      (dissoc :context_hash)
                                      (dissoc :user_name)
                                      (dissoc :table_type)
                                      (dissoc :total_rows)
                                      (assoc :shape_name shape-name)))]
              (swap! em/selected-recos conj fixed-row))))
        (ut/pp [:received-selected-reco kind dm-type context_hash combo_hash client-name])
        {}) ;; return is useless, but wl/handle-push does not work.. :/
    (= dm-type :meta-fields)
      (let [shape-name       "basic-group-by"
            combo-fields-sql (walk/postwalk-replace
                               {:target :field_name :data-type :data_type :source_table :table_name :connection-id :connection_id}
                               (merge drag-meta {:shape_name shape-name :axes_key "group-by"}))]
        (doseq [row (sql-query system-db (to-sql combo-fields-sql))]
          (async/thread
            (let [lm        (edn/read-string (get row :logic_map))
                  fixed-row (into (sorted-map)
                                  (-> (merge lm row)
                                      (dissoc :logic_map)
                                      (dissoc :database_version)
                                      (dissoc :key_hash)
                                      (assoc :table_name (cstr/join "_"
                                                                    (drop-last (cstr/split
                                                                                 (cstr/replace (str (get lm :table_name)) ":" "")
                                                                                 #"_"))))
                                      (dissoc :context_hash)
                                      (dissoc :user_name)
                                      (dissoc :table_type)
                                      (dissoc :total_rows)
                                      (assoc :shape_name shape-name)))])))
        (ut/pp [:received-selected-reco kind dm-type context_hash combo_hash client-name])
        {}))) ;; return is useless, but wl/handle-push does not work.. :/

(def pivot-cache (atom {}))

(defn extract-rql
  [ui-keypath obody rql-holder]
  (let [;kps (into (ut/extract-patterns obody :*code* 1)
        kps       (vals (filter #(let [[_ v] %]
                                   (or (cstr/starts-with? (str v) "[:*code")
                                       (cstr/starts-with? (str v) "[:*read-edn")
                                       (cstr/starts-with? (str v) "[:*render")))
                          (into {} (for [kp (ut/kvpaths obody)] {kp (get-in obody kp)}))))
        logic-kps (into {}
                        (for [v kps]
                          (let [;[_ l this] v
                                rql-key (str "@@r" (rand-int 123455))]
                            (swap! rql-holder assoc-in (conj ui-keypath rql-key) v)
                            {v rql-key})))]
    (walk/postwalk-replace logic-kps obody)))

(defonce pre-sql-cache (ut/thaw-atom [] "./data/atoms/pre-sql-cache.edn")) ;; no need to persist for now...

(defn replace-pre-sql
  [honey-sql]
  (let [if-walk-map2   (fn [query]
                         (let [kps       (ut/extract-patterns query :*if 4) ;(kv-map-fn obody)
                               logic-kps (into {}
                                               (for [v kps]
                                                 (let [[_ l this that] v] {v (if (not (or (empty? l) (nil? l))) this that)})))]
                           (walk/postwalk-replace logic-kps query)))
        =-walk-map2    (fn [query]
                         (let [kps       (ut/extract-patterns query :*= 3)
                               logic-kps (into {} (for [v kps] (let [[_ that this] v] {v (= (str that) (str this))})))]
                           (walk/postwalk-replace logic-kps query)))
        when-walk-map2 (fn [query]
                         (let [kps       (ut/extract-patterns query :*when 3)
                               logic-kps (into {}
                                               (for [v kps]
                                                 (let [[_ that this] v] {v (when (or (true? that) (ut/ne? that)) this)})))]
                           (walk/postwalk-replace logic-kps query)))
        fix-nil-ins    (fn [query]
                         (let [kps       (ut/extract-patterns query :in 3)
                               logic-kps (into {} (for [v kps :when (nil? (last v))] (let [[_ that this] v] {v nil})))]
                           (walk/postwalk-replace logic-kps query)))
        all=-map2-inc  (fn [query]
                         (let [kps       (ut/extract-patterns query :*all= 3)
                               logic-kps (into {}
                                               (for [v kps]
                                                 (let [[_ fmap incvec] v]
                                                   {v (vec (conj (for [[k v] (select-keys fmap incvec)
                                                                       :let  [in? (vector? v)]]
                                                                   (if in? [:in k v] [:= k v]))
                                                                 :and))})))]
                           (walk/postwalk-replace logic-kps query)))
        all=-map2      (fn [query]
                         (let [kps       (ut/extract-patterns query :*all= 2)
                               logic-kps (into {}
                                               (for [v kps]
                                                 (let [[_ fmap] v]
                                                   {v (vec (conj (for [[k v] fmap ;(select-keys
                                                                       :let  [in? (vector? v)]]
                                                                   (if in? [:in k v] [:= k v]))
                                                                 :and))})))]
                           (walk/postwalk-replace logic-kps query)))
        pre-sql-out    (-> honey-sql
                           =-walk-map2
                           when-walk-map2
                           if-walk-map2
                           all=-map2
                           all=-map2-inc
                           fix-nil-ins
                           ut/deep-remove-nil-values)
        pre-sql-out    (ut/lists-to-vectors pre-sql-out)]
    (reset! pre-sql-cache (vec (distinct (conj @pre-sql-cache pre-sql-out))))
    pre-sql-out))






















(defn ded-thread
  [task-fn]
  (let [thread (Thread. (fn [] (try (task-fn) (catch Exception e (println "Exception in thread:" (.getMessage e))) (finally))))]
    (.start thread)))












(def execution-channel (chan 100))

(go (loop []
      (when-let [{:keys [run-fn result-chan]} (<! execution-channel)]
        (try (let [result (run-fn)] (>! result-chan result))
             (catch Exception e
               (println "Error in runstream:" e) ; Error handling
               (>! result-chan {:error e})))
        (recur))))

(defn queue-runstream
  [runstream-fn]
  (let [result-chan (chan)]
    (println "Queueing runstream") ; Debugging
    (>!! execution-channel {:run-fn runstream-fn :result-chan result-chan})
    result-chan))




(defn get-all-from [m] (if (map? m) (into [] (concat (when-let [where (:from m)] [where]) (mapcat get-all-from (vals m)))) []))

(defn group-by-field-name
  [data]
  (into {}
        (for [[table-key records] data]
          [table-key
           (into {}
                 (for [[field-key grouped-records] (group-by :field_name records)]
                   [(keyword field-key)
                    (first (map #(dissoc % :connection_id :table_name :db_type :field_name) grouped-records))]))])))

(defn get-clover-sql-training
  [clover-sql honey-sql-str2]
  (try (let [clover-sql          (ut/deep-remove-keys2 clover-sql [:_last-run :connection-id])
             connection-id       (get clover-sql :connection-id)
             data-dict-honey-sql {:select [:db_type :table_name :field_name :connection_id :field_type :data_type]
                                  :where  [:and [:= :connection_id connection-id] [:<> :field_name "*"]]
                                  :from   [[:fields :ee473as]]}
             sql-str             (to-sql data-dict-honey-sql)
             res                 (sql-query system-db sql-str [:data-dict-for-training-clover-sql])
             group-res           (group-by-field-name (group-by (comp keyword :table_name) res))
             tables              (filter keyword? (ut/deep-flatten (get-all-from clover-sql)))
             metadata            (select-keys group-res tables)
             data-map            (merge (get @clover-sql-training-atom clover-sql) ;; in case we
                                        {:sql-string     honey-sql-str2
                                         :db-type        (get-in res [0 :db_type])
                                         :clover-sql     clover-sql
                                         :table-metadata metadata})]
         (swap! clover-sql-training-atom assoc clover-sql data-map))
       (catch Throwable e (ut/pp [:error-in-clover-sql-training-harvest! (str e)]))))

(defn query-runstream
  [kind ui-keypath honey-sql client-cache? sniff? connection-id client-name page panel-key clover-sql deep-meta? snapshot-cache?]
  (doall
    (let [;;_ (ut/pp [:honey-sql honey-sql])
          post-process-fn (get honey-sql :post-process-fn) ;; allowed at the top level only - will
          honey-sql (if (get honey-sql :limit) ;; this is a crap solution since we can't
                      {:select [:*] :from [honey-sql]}
                      honey-sql)
          honey-sql (ut/deep-remove-keys honey-sql [:post-process-fn]) ;; disregard if we have
          has-rql? (try (true? (some #(or (= % :*render*) (= % :*read-edn*) (= % :*code*)) (ut/deep-flatten honey-sql)))
                        (catch Exception _ false))
          data-call? (true? (and (not has-rql?) (ut/ne? (first (filter #(= (last %) :data) (ut/kvpaths honey-sql))))))
          runstream
            (fn []
              (try
                (doall ;;;; ? fixed the socket laziness? (no pun intended)
                  (let [;last-known-fields (get honey-sql :last-known-fields [])
                        repl-host (get-in honey-sql (or (first (filter #(= (last %) :repl-host) (ut/kvpaths honey-sql))) [:nope]))
                        repl-port (get-in honey-sql (or (first (filter #(= (last %) :repl-port) (ut/kvpaths honey-sql))) [:nope]))
                        honey-sql (ut/deep-remove-keys honey-sql [:repl-host :repl-port])
                        _ (when (and repl-host repl-port) (ut/pp [:external-repl! repl-host repl-port]))
                        honey-sql (walk/postwalk-replace {[:*all= {}]       nil ;; take care of empty
                                                          :*client-name-str (pr-str client-name)
                                                          :*client-name     (str client-name)}
                                                         honey-sql)
                        orig-honey-sql honey-sql ;; for transform later
                        query-meta-subq? (true? (some #(or (= % :query_meta_subq) (= % :query-meta-subq))
                                                      (ut/deep-flatten honey-sql)))
                        literal-data? (and (get orig-honey-sql :data) ;false
                                           (vector? (get orig-honey-sql :data)))
                        rql-holder (atom {})
                        post-sniffed-literal-data? (and (not literal-data?) ;false
                                                        (ut/ne? (filter #(= (last %) :data) (ut/kvpaths honey-sql)))
                                                        (not has-rql?)) ;; <-- look into, clashing
                        data-literals (or (first (filter #(= (last %) :data) (ut/kvpaths honey-sql))) [:nope])
                        data-literal-code? (false? (when (or literal-data? post-sniffed-literal-data?)
                                                     (let [dl (get-in honey-sql data-literals)]
                                                       (and (vector? dl) (map? (first dl))))))
                        data-literals-data (get-in honey-sql data-literals)
                        honey-sql (cond post-sniffed-literal-data?                      (walk/postwalk-replace @literal-data-map
                                                                                                               orig-honey-sql)
                                        literal-data?                                   (get orig-honey-sql :data)
                                        (ut/ne? (get orig-honey-sql :transform-select)) (-> (first (get orig-honey-sql :from))
                                                                                            (dissoc :limit)
                                                                                            (assoc :page -1))
                                        :else                                           orig-honey-sql)
                        honey-sql (if has-rql? (extract-rql ui-keypath honey-sql rql-holder) honey-sql)
                        honey-sql (replace-pre-sql honey-sql) ;; runs a subset of clover replacements
                        target-db (cond query-meta-subq? system-db ;; override for sidecar meta queries
                                        (= connection-id "system-db") system-db
                                        (= connection-id "flows-db") flows-db
                                        (= connection-id "system") system-db
                                        (or (= connection-id :cache) (= connection-id "cache.db") (nil? connection-id)) cache-db ;mem-db2
                                        :else (get-connection-string connection-id))
                        has-pivot?
                          #_{:clj-kondo/ignore [:not-empty?]}
                          (not (empty? (ut/extract-patterns honey-sql :pivot-by 2)))
                        honey-sql (if has-pivot?
                                    (let [dim-lookups (distinct (map :pivot-by (pivot/find-select-maps honey-sql)))
                                          hold        (atom honey-sql)] ;; temp to replace all instances of
                                      (doseq [d    dim-lookups
                                              :let [field   (first (first d))
                                                    vls0    (last (last (last d))) ;; either static vals or a
                                                    sql?    (and (map? vls0)
                                                                 (or (contains? vls0 :select) (contains? vls0 :select-distinct)))
                                                    cached? (true? (ut/ne? (get @pivot-cache vls0)))
                                                    vls     (if sql?
                                                              (if cached? ;; if got exact cache, send it.
                                                                (get @pivot-cache vls0)
                                                                (try (let [sql-str   (to-sql (assoc vls0 :limit 50))
                                                                           sres      (sql-query target-db
                                                                                                sql-str
                                                                                                [:get-pivot-vals-for field :kp
                                                                                                 ui-keypath])
                                                                           just-vals (vec (map (first (keys (first sres))) sres))]
                                                                       (swap! pivot-cache assoc vls0 just-vals)
                                                                       just-vals)
                                                                     (catch Exception _ ["error" "in" "pivot" "get-vals"])))
                                                              vls0)]]
                                        (when sql? (reset! hold (walk/postwalk-replace {vls0 vls} @hold))))
                                      (pivot/pivot-each @hold 25))
                                    honey-sql)
                        req-hash (hash [kind ui-keypath honey-sql client-name])
                        filtered-req-hash (hash [kind ui-keypath (vec (filter keyword? (ut/deep-flatten honey-sql))) ;; eyes
                                                 client-name])
                        cache? (and ;;(not (nil? (get @sql-cache req-hash))) ;; disabled for testing,
                                 (lookup-cache-exists? req-hash)
                                 (not post-sniffed-literal-data?) ;; questionable, we SHOULD be able
                                 (not literal-data?) ;; questionable, we SHOULD be able to invalidate
                                 (not (= connection-id "flows-db"))
                                 (not (= connection-id "system-log"))
                                 (not (= target-db system-db)))
                        cache-table-name ;(if (or post-sniffed-literal-data? literal-data?)
                          (ut/keypath-munger ui-keypath)
                        completion-channel (async/chan) ;; moved to outer
                        data-literal-insert-error (atom nil)
                        honey-sql
                          (if (or post-sniffed-literal-data? literal-data?)
                            (let [;_ (ut/pp [:last-let? honey-sql])
                                  cache-table cache-table-name ;(str (ut/keypath-munger ui-keypath)
                                  honey-sql   (if data-literal-code? honey-sql (ut/lists-to-vectors honey-sql))
                                  literals    (if data-literal-code?
                                                data-literals-data
                                                (get-in honey-sql
                                                        (or (first (filter #(= (last %) :data) (ut/kvpaths honey-sql))) [:nope])))
                                  cached?     (if (not client-cache?)
                                                false
                                                (try (not (empty? (get @literal-data-map {:data literals})))
                                                     (catch Exception _ false)))]
                              (if (and (not cached?) (or (list? literals) (vector? literals))) ;(and (not
                                                                                               ;cached?)
                                                                                               ;(not
                                                                                               ;post-sniffed-literal-data?))
                                (do (if data-literal-code?
                                      (enqueue-task
                                        (fn []
                                          (let [;output (evl/run literals) ;; (and repl-host repl-port)
                                                literals    (walk/postwalk-replace
                                                              {'cie-to-hex     'rvbbit-backend.util/cie-to-hex
                                                               'hue-to-hex     'rvbbit-backend.util/hue-to-hex
                                                               'hex-to-cie     'rvbbit-backend.util/hex-to-cie
                                                               'hex-to-hue-sat 'rvbbit-backend.util/hex-to-hue-sat
                                                               'http-call      'rvbbit-backend.websockets/http-call
                                                               'flatten-map    'rvbbit-backend.websockets/flatten-map}
                                                              literals)
                                                output-full (evl/repl-eval literals repl-host repl-port)
                                                output      (last (get-in output-full [:evald-result :value]))
                                                output-full (-> output-full
                                                                (assoc-in [:evald-result :output-lines]
                                                                          (try (count (remove empty?
                                                                                        (get-in output-full
                                                                                                [:evald-result :out])))
                                                                               (catch Exception _ 0)))
                                                                (assoc-in [:evald-result :values]
                                                                          (try (count (last (get-in output-full
                                                                                                    [:evald-result :value])))
                                                                               (catch Exception _ 0))))]
                                            (swap! literal-data-output assoc ui-keypath output-full)
                                            (try (insert-rowset output
                                                                cache-table
                                                                (first ui-keypath)
                                                                client-name
                                                                (keys (first output)))
                                                 (catch Exception e
                                                   (do (reset! data-literal-insert-error
                                                         ["Data struct not a proper 'rowset', see console log ^" e])
                                                       nil)))
                                            (async/>!! completion-channel true) ;; unblock
                                          )))
                                      (enqueue-task (fn []
                                                      (insert-rowset literals
                                                                     cache-table
                                                                     (first ui-keypath)
                                                                     client-name
                                                                     (keys (first literals)))
                                                      (async/>!! completion-channel true) ;; unblock
                                                    )))
                                    (async/<!! completion-channel) ;; BLOCK until our threaded job is
                                    (swap! literal-data-map assoc {:data literals} cache-table)
                                    (walk/postwalk-replace @literal-data-map honey-sql))
                                (walk/postwalk-replace @literal-data-map honey-sql)))
                            honey-sql)]
                    (if (and (not (= :styles (last ui-keypath))) (not sniff?) cache? client-cache?)
                      (do (ut/pp [:*cache-hit @q-calls kind ui-keypath connection-id client-name] ;:honey-sql
                          )
                          (-> ;(get @sql-cache req-hash)
                            (get-from-cache req-hash)
                            (assoc :cached? true)
                            (assoc :query-ms nil))) ;; return output ;; :query-ms query-ms
                      (let [page-num page ;(get honey-sql :page)
                            per-page-limit (cond (= page-num -1) 50000
                                                 (= page-num -2) 1000000 ;; yikes. revisit TODO
                                                 :else           per-page-limit)
                            honey-sql (if literal-data?
                                        honey-sql ;; dont mutate literal data rowsets
                                        (cond (and page-num (and (not (= page-num -2)) (not (= page-num -1))))
                                                (assoc (dissoc honey-sql :page) :offset (* (- page-num 1) per-page-limit))
                                              (or (= page-num -1) (= page-num -2)) (dissoc honey-sql :page)
                                              :else honey-sql))
                            honey-sql-str (when (not literal-data?) ;; no query!
                                            (if (or (= page-num -1) (= page-num -2)) ;; or limit
                                              (to-sql honey-sql)
                                              (to-sql (assoc honey-sql :limit 500))))
                            honey-sql-str2 (first honey-sql-str)
                            _ (async/thread (get-clover-sql-training clover-sql honey-sql-str2)) ;; ONLY
                            honey-result (timed-expr (if literal-data?
                                                       honey-sql ;; which in this case IS the
                                                       (sql-query target-db honey-sql-str ui-keypath)))
                            query-ms (get honey-result :elapsed-ms)
                            honey-result (get honey-result :result)
                            query-error? (get (first honey-result) :query_error)
                            honey-meta (get-query-metadata honey-result honey-sql)
                            fields (get honey-meta :fields)
                            dates (remove nil? (for [[k v] fields] (when (cstr/includes? (get v :data-type) "date") k)))
                            is-meta? (= 3 (count ui-keypath))
                            is-condi? (= (keys fields) '(:v)) ;; condi eval call
                            result (if (empty? dates)
                                     (vec (take per-page-limit honey-result))
                                     (vec (for [r (take per-page-limit honey-result)]
                                            (into {} (for [[k v] r] (if (some #(= % k) dates) {k (str v)} {k v}))))))
                            result (if (and query-error? @data-literal-insert-error)
                                     (vec (into result
                                                [{:query_error (str (first @data-literal-insert-error))}
                                                 {:query_error (str (last @data-literal-insert-error))}]))
                                     result) ;; add extra error data from code evaL
                            result (if (get orig-honey-sql :transform-select)
                                     (do (ut/pp [:transform (assoc orig-honey-sql :from [:data])])
                                         (let [res (ts/transform (assoc orig-honey-sql :from [:data]) result)] res))
                                     result)
                            result (if has-rql?
                                     (let [walk-map (get-in @rql-holder ui-keypath)
                                           replaced
                                             (vec
                                               (for [row-map result]
                                                 (let [;with-code (walk/postwalk-replace walk-map
                                                       safe-keys     (into {}
                                                                           (apply (fn [x]
                                                                                    {x (keyword (cstr/replace (str x) #":_" ""))})
                                                                             (filter #(cstr/starts-with? (str %) ":_")
                                                                               (distinct (ut/deep-flatten orig-honey-sql)))))
                                                       safe-keys-rev (into {} (for [[k v] safe-keys] [v k])) ; {:key
                                                       from-kp       [1 :queries :gen-viz-609 :from] ;(first
                                                       walk-map      (into {}
                                                                           (for [[k v] walk-map
                                                                                 :let  [protect-where (get-in v from-kp)]]
                                                                             {k (assoc-in v
                                                                                  from-kp
                                                                                  (walk/postwalk-replace safe-keys-rev
                                                                                                         protect-where))}))
                                                       new-field     (walk/postwalk-replace row-map walk-map)
                                                       new-field     (into {}
                                                                           (for [[k v] new-field]
                                                                             (if (= (first v) :*read-edn*)
                                                                               (let [bd  (get v 1) ;; edn body
                                                                                     kp  (get v 2) ;; keypath
                                                                                     cst (get v 3) ;; cast if
                                                                                                   ;; asked?
                                                                                     rr  (try
                                                                                           (let [vv (get-in (edn/read-string bd)
                                                                                                            kp)]
                                                                                             (if cst (ut/cast-to-type vv cst) vv))
                                                                                           (catch Exception e
                                                                                             (str ":*read-edn-error*:" e bd)))]
                                                                                 {k rr})
                                                                               {k v})))
                                                       new-row       (walk/postwalk-replace new-field row-map)
                                                       new-row       (walk/postwalk-replace safe-keys new-row)] ;; replace
                                                   new-row)))]
                                       (println (first replaced))
                                       replaced)
                                     result)
                            result (if (not (nil? post-process-fn))
                                     (try ((eval post-process-fn) result)
                                          (catch Throwable e
                                            (let [res [{:error "post-process-fn error" :vval (str e)}]]
                                              (ut/pp [:post-process-fn-error res])
                                              res)))
                                     result)
                            honey-meta (if (or (get orig-honey-sql :transform-select)
                                               has-rql? ;query-error?
                                               post-process-fn
                                               (get orig-honey-sql :data)) ;; TODO< this is ugly
                                         (get-query-metadata result honey-sql) ;; get new meta on
                                         honey-meta)
                            fields (get honey-meta :fields) ;; lol, refactor, this is cheesy (will
                            sniff-worthy? (and (not is-meta?)
                                               (not has-rql?) ;;; temp since insert will fail dur to not being
                                               (not (= (ut/dissoc-recursive honey-sql)
                                                       (ut/dissoc-recursive (get-in @sql/query-history
                                                                                    [cache-table-name :honey-sql])))) ;; <--
                                               (ut/ne? (flatten result))
                                               (not (cstr/includes? (str ui-keypath) "-hist-")) ;; undo
                                                                                                ;; history
                                               (not query-error?)
                                               (not is-condi?)
                                               (not (cstr/starts-with? cache-table-name "kick"))
                                               (not (= (get honey-sql :limit) 111))
                                               (not (some #(cstr/starts-with? (str %) ":query-preview") ui-keypath)))
                            sniffable? (or sniff? ;; <-- req from client only?
                                           (and (not is-meta?)
                                                false ;; otherwise never? only on manual for now
                                                (not query-error?)
                                                (not is-condi?)
                                                (not (cstr/starts-with? cache-table-name "kick"))
                                                (not (= (get honey-sql :limit) 111)) ;; a "base table
                                                (not (some #(cstr/starts-with? (str %) ":query-preview") ui-keypath)) ;; browsing
                                                                                                                      ;; previously
                                           ))
                            repl-output (ut/limited (get-in @literal-data-output [ui-keypath :evald-result] {}))
                            output {:kind           kind
                                    :ui-keypath     ui-keypath
                                    :result         result
                                    :result-meta    honey-meta
                                    :sql-str        honey-sql-str2
                                    :query-ms       query-ms
                                    :cached?        false
                                    :connection-id  connection-id
                                    :repl-output    repl-output
                                    :original-honey orig-honey-sql
                                    :panel-key      panel-key
                                    :client-name    client-name
                                    :map-order      (if (or (get orig-honey-sql :transform-select)
                                                            query-error?
                                                            post-process-fn
                                                            (get orig-honey-sql :data))
                                                      (keys fields)
                                                      (get @sql/map-orders honey-sql-str))}
                            result-hash (hash result)]
                        (enqueue-task-sql-meta (fn [] (sniff-meta ui-keypath honey-sql fields target-db client-name deep-meta?)))
                        (if sniffable?
                          (doall
                            (do
                              (swap! deep-run-list conj filtered-req-hash) ;; mark this as run
                              (enqueue-task
                                (fn []
                                  (ut/pp :kick-sniff)
                                  (push-to-client ui-keypath [:reco-status (first ui-keypath)] client-name 1 :reco :started)
                                  (swap! sql/query-history assoc
                                    cache-table-name
                                    {:honey-sql honey-sql :connection-id connection-id})
                                  (let [result (vec (for [r result] (assoc r :rows 1)))] ;;; hack
                                    (insert-rowset result cache-table-name (first ui-keypath) client-name (keys (first result))))
                                  (doall
                                    (do
                                      (do (ut/pp [[:recos-started-for (first ui-keypath)] :via client-name ui-keypath
                                                  filtered-req-hash])
                                          (push-to-client ui-keypath
                                                          [:reco-status (first ui-keypath)]
                                                          client-name
                                                          1
                                                          :reco
                                                          :started))
                                      (doall
                                        (let [run-it         (timed-expr (cruiser/captured-sniff "cache.db"
                                                                                                 connection-id
                                                                                                 target-db
                                                                                                 cache-db
                                                                                                 result-hash
                                                                                                 [:= :table-name
                                                                                                  cache-table-name]))
                                              reco-count-sql {:select [[[:count 1] :cnt]]
                                                              :from   [:combos]
                                                              :where  [:= :table-name cache-table-name]}
                                              reco-count     (get-in (sql-query system-db
                                                                                (to-sql reco-count-sql)
                                                                                [:reco-count :client-status])
                                                                     [0 :cnt])]
                                          (ut/pp [[:recos-finished-for (first ui-keypath)] :via client-name ui-keypath
                                                  filtered-req-hash])
                                          (push-to-client ui-keypath
                                                          [:reco-status (first ui-keypath)]
                                                          client-name
                                                          1
                                                          :reco
                                                          :done
                                                          reco-count
                                                          (get run-it :elapsed-ms)))))) ;; vertica
                                ))))
                          (when sniff-worthy? ;; want details, but not yet the full expensive meta
                            (enqueue-task
                              (fn []
                                (swap! sql/query-history assoc
                                  cache-table-name
                                  {:honey-sql honey-sql :connection-id connection-id})
                                (doall
                                  (let [resultv (vec (for [r result] (assoc r :rows 1)))] ;;; hack to
                                    (when (not= (first ui-keypath) :solvers) ;; solvers have their
                                      (if false ;snapshot-cache?
                                        (insert-rowset-snap result
                                                            cache-table-name
                                                            (first ui-keypath)
                                                            client-name
                                                            (conj (keys (first result)) :snapshot_ds))
                                        (insert-rowset result
                                                       cache-table-name
                                                       (first ui-keypath)
                                                       client-name
                                                       (keys (first result)))))
                                    (cruiser/captured-sniff "cache.db"
                                                            connection-id
                                                            target-db
                                                            cache-db
                                                            result-hash
                                                            [:= :table-name cache-table-name]
                                                            true
                                                            resultv)
                                    (ut/pp [[:quick-sniff-for (first ui-keypath)] :via client-name ui-keypath
                                            filtered-req-hash])))))))
                        (do ;(swap! sql-cache assoc req-hash output)
                          (when client-cache? (insert-into-cache req-hash output)) ;; no point to
                          output)))))
                (catch Exception e
                  (do (ut/pp [:honeyx-OUTER-LOOP-EXCEPTION (str e) :connection-id connection-id ui-keypath (str honey-sql)])
                      {:kind        kind
                       :ui-keypath  ui-keypath
                       :result      [{:query_error "execution error"} {:query_error (str e)}]
                       :result-meta {}
                       :sql-str     ""
                       :query-ms    nil
                       :cached?     false
                       :repl-output (assoc (ut/limited (get-in @literal-data-output [ui-keypath :evald-result] {}))
                                      :error (str e))
                       :client-name client-name
                       :map-order   []}))))]
      (doall (if data-call?
               (doall (let [async-result-chan (go (let [result-chan (queue-runstream runstream)]
                                                    (println "Waiting for result...") ; Debugging
                                                    (<! result-chan)))] ; This returns a channel
                        (do (println "Getting result from async operation...") (async/<!! async-result-chan)))) ; Blocking take
                                                                                                                ; from the
                                                                                                                ; channel
               (runstream))))));)

(defn kit-to-kit [payload])

(defn insert-kit-data
  [output query-hash ui-keypath ttype kit-name elapsed-ms & [client-name flow-id]]
  (enqueue-task3 ;; blocking for inserts and deletes (while we are one sqlite, will be
    (fn []
      (let [output-is-valid? (map? output) ;; basic spec checking
            kit-keys         (keys output)
            kkit-name        (if (vector? kit-name) (str (first kit-name)) (str kit-name))
            output-rows      (vec
                               (apply (if (= kit-keys 1) flatten into)
                                 (for [[k v] output
                                       :let  [;descr (pr-str (get v :description))
                                              rows   (get v :data)
                                              rowset (vec
                                                       (for [idx  (range (count rows))
                                                             :let [i       (get rows idx)
                                                                   row-map {:item_hash    query-hash
                                                                            :item_name    (if (vector? ui-keypath)
                                                                                            (str (first ui-keypath))
                                                                                            (str ui-keypath))
                                                                            :item_type    (str ttype)
                                                                            :item_key     (str k)
                                                                            :kit_name     kkit-name
                                                                            :item_idx     idx
                                                                            :client_name  (str client-name)
                                                                            :flow_id      (str flow-id)
                                                                            :item_options (pr-str (select-keys
                                                                                                    v
                                                                                                    [:options :parameters :mutates
                                                                                                     :description]))
                                                                            :item_data    (pr-str i)}]]
                                                         row-map))]]
                                   rowset)))]
        (when (not (= kkit-name ":kick")) ;; let kick "messages" pile up, dont swap out like a
          (sql-exec system-db
                    (to-sql {:delete-from [:kits]
                             :where       [:and [:= :kit_name kkit-name]
                                           [:= :item_name
                                            (if (vector? ui-keypath) (str (first ui-keypath)) (str ui-keypath))]]})))
        (sql-exec system-db (to-sql {:insert-into [:kits] :values output-rows}))))))

(defmethod wl/handle-request :honey-xcall
  [{:keys [kind ui-keypath honey-sql client-cache? sniff? connection-id panel-key client-name page kit-name clover-sql
           deep-meta?]}]
  (swap! q-calls inc)
  (inc-score! client-name :push)
  (when (keyword? kit-name) ;;; all kit stuff. deprecated?
    (enqueue-task2
      (fn []
        (try ;; save off the full thing... or try
          (let [;kit-name :outliers
                _ (push-to-client ui-keypath [:kit-status (first ui-keypath)] client-name 2 kit-name :started)
                output     (query-runstream kind
                                            ui-keypath
                                            honey-sql
                                            false
                                            false
                                            connection-id
                                            client-name
                                            -2
                                            panel-key
                                            clover-sql
                                            deep-meta?
                                            false) ;; -2 has 1m row limit. yikes.
                fullrows   (get output :result)
                fullrows   (vec (for [r fullrows] (assoc r :rows 1)))
                kp         (str (ut/unkeyword (get-in output [:ui-keypath 0] "unknown")))
                rows       (count fullrows)
                meta       (-> (get output :result-meta)
                               (assoc :original-honey (get output :original-honey))
                               (assoc :connection-id (get output :connection-id)))
                mpath      (str "./kit-rowsets/" kp ".meta.edn")
                path       (str "./kit-rowsets/" kp ".edn")
                query-hash (hash honey-sql)
                ttype      :queries]
            (spit path (pr-str fullrows) :append false)
            (spit mpath (pr-str meta) :append false)
            (ut/pp [:saved-full-to path rows :rows])
            (ut/pp [:repl-exec-for kit-name])
            (let [repl-fn      (get-in config/kit-fns [kit-name :fn]) ;'(+ (rand-int 12345)
                  repl-command (walk/postwalk-replace
                                 {:query honey-sql :query-name (first ui-keypath) :kit-name kit-name :panel-name panel-key}
                                 repl-fn)
                  _ (when kit-name (ut/pp [:running-kit kit-name]))
                  repl-host    (get-in config/kit-fns [kit-name :repl :host])
                  repl-port    (get-in config/kit-fns [kit-name :repl :port])
                  output-full  (timed-expr (evl/repl-eval repl-command repl-host repl-port))
                  elapsed-ms   (get output-full :elapsed-ms)
                  output-full  (get output-full :result) ;; from timed map
                  output       (last (get-in output-full [:evald-result :value]))
                  _ (ut/pp [:**************************** :repl-output :****************************])
                  _ (ut/pp (ut/limited (get-in output-full [:evald-result :out]) 5))
                  _ (ut/pp [:**************************** :repl-output :****************************])
                  kit-keys     (count (keys output))]
              (insert-kit-data output query-hash ui-keypath ttype kit-name elapsed-ms)
              (push-to-client ui-keypath [:kit-status (first ui-keypath)] client-name 2 kit-name :done kit-keys elapsed-ms))) ;)
          (catch Exception e (ut/pp [:save-full-error (str e) :err-end]))))))
  (doall
    (if (or (cstr/includes? (str ui-keypath) "query-preview-block") ;; queue up the inviz
            (cstr/includes? (str ui-keypath) "query-preview-inviz"))
      (doall
        (let [async-result-chan (go (let [result-chan (queue-runstream (fn []
                                                                         (query-runstream kind
                                                                                          ui-keypath
                                                                                          honey-sql
                                                                                          client-cache?
                                                                                          sniff?
                                                                                          connection-id
                                                                                          client-name
                                                                                          page
                                                                                          panel-key
                                                                                          clover-sql
                                                                                          deep-meta?
                                                                                          false)))]
                                      (println "inviz: Waiting for result...") ; Debugging
                                      (<! result-chan)))] ; This returns a channel; This returns
          (do (println "inviz: Getting result from async operation...") (async/<!! async-result-chan)))) ; Blocking take from
                                                                                                         ; the channel
      (query-runstream kind
                       ui-keypath
                       honey-sql
                       client-cache?
                       sniff?
                       connection-id
                       client-name
                       page
                       panel-key
                       clover-sql
                       deep-meta?
                       false))))

(defn send-file-success
  [content filename]
  (assoc (http/edn-response content)
    :headers {"Content-Type"        "application/octet-stream" ;;"application/edn"
              "Content-Disposition" "attachment"
              "filename"            filename}))  ;"text/plain"

(defn send-edn-success [content] (assoc (http/edn-response content) :headers {"Content-Type" "application/edn"}))  ;"text/plain"

(def pool (cp/threadpool 5))

(defn process-csv
  [request & [local-file]]
  (ut/pp [:incoming-file (if local-file local-file (get-in request [:edn-params :fname]))])
  (time
    (let [seed-id        (rand-int 123124) ;; for status table
          file-name      (if local-file local-file (get-in request [:edn-params :fname]))
          client-name    (if local-file "system" (str (get-in request [:edn-params :client-name])))
          fdata          (if local-file (slurp local-file) (get-in request [:edn-params :image]))
          file-base-name (first (cstr/split (last (cstr/split file-name #"/")) #"\."))
          file-path      (str "./data/" file-name)
          fdata-map      (doall (ut/csv-data->maps (csv/read-csv fdata)))
          ftypes         (into {} (for [f (keys (first fdata-map))] {f (flatten (take 5 (map (juxt f) fdata-map)))}))
          ftypes-coerce  (into {}
                               (for [[k v] ftypes]
                                 {k (cond (try (integer? (Long/parseLong (first v))) (catch Exception _ false)) :integer
                                          (try (float? (Float/parseFloat (first v))) (catch Exception _ false)) :float
                                          :else                                                                 :string)}))
          fdata-map-mod  (vec (cp/pfor pool
                                       [row fdata-map] ;; cp/pfor pool
                                       (into {}
                                             (for [[k tt] ftypes-coerce]
                                               {k ;[tt (get row k)]
                                                  (try (let [ff  (get row k nil)
                                                             fff (if (or (empty? ff) (= ff " ") (= ff "")) nil ff)]
                                                         (cond (= tt :float)   (Float/parseFloat fff)
                                                               (= tt :integer) (Long/parseLong fff)
                                                               :else           (str fff)))
                                                       (catch Exception e nil))}))))
          op-name        (str "csv: " file-path)]
      (ut/pp [:processing op-name])
      (sql-exec system-db
                (to-sql {:insert-into [:status]
                         :columns     [:client_name :op_name :status]
                         :values      [[client-name op-name "processing csv..."]]}))
      (insert-rowset-csv fdata-map-mod file-base-name client-name op-name)
      (cruiser/lets-give-it-a-whirl-no-viz file-path
                                           import-db
                                           system-db
                                           cruiser/default-sniff-tests
                                           cruiser/default-field-attributes
                                           cruiser/default-derived-fields
                                           cruiser/default-viz-shapes)
      (do (ut/pp [:saved-csv file-path file-base-name (first fdata-map) ftypes ftypes-coerce (first fdata-map-mod)])))))

(defn save-csv
  [request]
  (time (do (process-csv request)))
  (send-edn-success {:status "saved-csv" :screen (first (get request :edn-params))}))

(defn save-alert-notification
  [client-name name fpath flow?]
  (alert! client-name
          [:v-box :justify :center :style
           {;:margin-top "-6px"
            :opacity 0.7} ;:color (if error? "red" "inherit")}
           :children
           [[:box :style {:font-weight 700 :font-size "11px" :opacity 0.6} :child
             (str (str "successfully saved " (if flow? "flow" "screen")))]
            [:box :style {:font-weight 700 :font-size "18px"} :child (str name)]
            [:box :style {:font-weight 700 :font-size "11px" :opacity 0.6} :child (str "@ " fpath)]]]
          10
          1.6
          8))

(defn save-alert-notification-pre
  [client-name name fpath flow?]
  (alert! client-name
          [:v-box :justify :center :style
           {;:margin-top "-6px"
            :opacity 0.6} ;:color (if error? "red" "inherit")}
           :children [[:box :style {:font-weight 700 :font-size "13px"} :child (str "saving " name "...")]]]
          10
          0.6
          8))

(defn save
  [request]
  (try (let [screen-name    (get-in request [:edn-params :screen-name])
             client-name    (get-in request [:edn-params :client-name] "unknown")
             file-base-name (ut/sanitize-name (str screen-name))
             file-path      (str "./screens/" file-base-name ".edn")
             fpath          (ut/abs-file-path file-path)]
         (save-alert-notification-pre client-name screen-name nil false)
         (do (ut/pp [:saved-file file-path])
             (try (ut/pretty-spit file-path (get-in request [:edn-params :image]))
                  (catch Throwable e
                    (do (ut/pp [:pretty-spit-error-bad-edn? e :saving-raw])
                        (spit file-path (get-in request [:edn-params :image])))))
             (save-alert-notification client-name screen-name fpath false)))
       (catch Exception e
         (ut/pp [:error-saving-screen-outer (get-in request [:edn-params :screen-name])
                 (get-in request [:edn-params :client-name] "unknown") e])))
  (send-edn-success {:status "saved" :screen (first (get request :edn-params))}))


(defn save-snap
  [request]
  (try (let [image       (get-in request [:edn-params :image])
             session     (get-in request [:edn-params :session])
             client-name (get-in request [:edn-params :client-name] "unknown")
             client-name (cstr/replace (str client-name) ":" "")
             file-path   (str "./snaps/" client-name ".jpg")
             ifile-path  (str "/home/ryanr/rvbbit/frontend/resources/public/snaps/" client-name ".jpg")
             sess-path   (str "./snaps/" client-name ".edn")]
         (spit sess-path session)
         (ut/save-base64-to-jpeg image file-path)
         (ut/save-base64-to-jpeg image ifile-path)) ;)
       (catch Exception e (ut/pp [:save-snap-error e])))
  (send-edn-success {:status "snap-saved" :flow (first (get request :edn-params))}))

(defn save-screen-snap
  [request]
  (try (let [image       (get-in request [:edn-params :image])
             screen-name (get-in request [:edn-params :screen-name] "unknown")
             file-path   (str "./screen-snaps/" screen-name ".jpg")
             ifile-path  (str "/home/ryanr/rvbbit/frontend/resources/public/screen-snaps/" screen-name ".jpg")]
         (ut/save-base64-to-jpeg image file-path)
         (ut/save-base64-to-jpeg image ifile-path)) ;)
       (catch Exception e (ut/pp [:save-snap-error e])))
  (send-edn-success {:status "screen-snap-saved" :flow (first (get request :edn-params))}))

(defn save-flow
  [request]
  (time (do (let [screen-name    (get-in request [:edn-params :flow-id]) ;(ut/generate-name) ;
                  client-name    (get-in request [:edn-params :client-name] "unknown")
                  file-base-name (ut/sanitize-name (str screen-name))
                  file-path      (str "./flows/" file-base-name ".edn")
                  fpath          (ut/abs-file-path file-path)]
              (save-alert-notification-pre client-name screen-name nil false)
              (do (ut/pp [:saved-flow file-path])
                  (ut/pretty-spit file-path (get-in request [:edn-params :image]) 125)
                  (save-alert-notification client-name screen-name fpath true)))))
  (send-edn-success {:status "saved" :flow (first (get request :edn-params))}))


(defn load-screen
  [request]
  (let [file-path      (or (get-in request [:edn-params :file-path]) (get-in request [:query-params :file-path]))
        flow-data-file (edn/read-string (slurp file-path))]
    (ut/ppln [:loading-screen-from-file file-path])
    (send-edn-success {:image flow-data-file})))

(defn load-flow
  [request]
  (let [file-path      (or (get-in request [:edn-params :file-path]) (get-in request [:query-params :file-path]))
        flow-data-file (edn/read-string (slurp file-path))]
    (ut/ppln [:loading-flow-from-file file-path])
    (send-edn-success {:image flow-data-file})))


(defn load-flow-history
  [request]
  (let [run-id         (str (or (get-in request [:edn-params :run-id]) (get-in request [:query-params :run-id])))
        start-ts       (str (or (get-in request [:edn-params :start-ts]) (get-in request [:query-params :start-ts])))
        runner?        (true? (or (get-in request [:edn-params :runner?]) (get-in request [:query-params :runner?])))
        runner?        (or (= start-ts "null") runner?)
        running?       (true? (when runner? (get-in (flow-statuses) [run-id :*running?])))
        last-run-id    (when runner? (get @latest-run-id run-id))
        start-ts       (-> (str start-ts)
                           (cstr/replace ":" "")
                           (cstr/replace " " "-"))
        run-id         (if (and runner? (not running?)) last-run-id run-id)
        file-path      (str "./flow-history/" run-id ".edn")
        _ (ut/pp [:load-flow-history file-path (or (empty? start-ts) runner?) start-ts run-id])
        flow-data-file (edn/read-string (slurp file-path))
        flow-id        (get flow-data-file :flow-id)
        hist-flow-id   (if runner? flow-id (str flow-id "-SHD-" start-ts))
        flow-data-file (if runner?
                         flow-data-file           ;; literal null from JSON conversion. odd.
                         (walk/postwalk-replace {flow-id hist-flow-id} flow-data-file))
        flow-data-file (if (and runner? running?) ;; add some history if we have it, since its
                         (merge flow-data-file
                                {:return-maps     (select-keys @flow-db/results-atom [flow-id])
                                 :tracker-history (ut/accumulate-unique-runs (get @tracker-history flow-id []))})
                         flow-data-file)]
    (ut/ppln [:loading-flow-history-from-file (when runner? :as-runner!) file-path flow-id hist-flow-id])
    (send-edn-success flow-data-file)))


(defn jvm-memory-used
  []
  (let [mm (int (Math/floor (/ (float (/ (- (-> (java.lang.Runtime/getRuntime)
                                                (.totalMemory))
                                            (-> (java.lang.Runtime/getRuntime)
                                                (.freeMemory)))
                                         1024))
                               1024)))]
    (ut/pp ["     " :jvm-stats :*cached-queries (ut/nf (count @sql-cache)) :*jvm-memory-used (ut/nf mm) :mb])))


(defn stringify-except
  [m exclude-keys]
  (reduce (fn [acc k]
            (if (not (contains? (set exclude-keys) k))
              (let [v          (get m k) ;; double quoted strings in SQL is not ideal
                    serializer (if (or (map? v) (vector? v)) pr-str str)]
                (update acc k (fn [_] (serializer v))))
              acc))
    m
    (keys m)))

(defn strunc [s] (let [s (str s) limit 80] (if (and s (> (count s) limit)) (subs s 0 limit) s)))

(defn param-sql-sync
  [] ;; placeholder function, will not scale - output is correct however
  (let [display-name (fn [x & [num]] (cstr/join " " (vec (drop (or num 2) x))))
        flow-rows    (vec (filter #(and (= (count %) 2)
                                        (not= (second %) :opts-map)
                                        (not (cstr/includes? (str (get % 1 "")) "/")) ;; debatable
                                   )
                            (ut/keypaths @flow-db/results-atom)))
        flow-rows    (for [e flow-rows]
                       [(str (first e)) "flow-values" (cstr/replace (str (first e)) ":" "")
                        (str ":flow/" (cstr/replace (cstr/join ">" e) ":" "")) nil
                        (strunc (ut/replace-large-base64 (get-in @flow-db/results-atom e))) (display-name e 1)])
        param-rows   (filter #(and (not (cstr/includes? (str %) ">"))
                                   (not (cstr/includes? (str %) "-sys"))
                                   (not (cstr/includes? (str %) ":sys"))
                                   (not (cstr/includes? (str %) ":theme")))
                       (ut/keypaths2 @params-atom))
        param-rows   (vec (distinct (map (fn [x] (vec (take 3 x))) param-rows)))
        live-clients (keys (client-statuses))
        param-rows   (for [e param-rows]
                       [(cstr/replace (str (first e)) ":" "") "client-params" (cstr/replace (str (second e)) ":" "")
                        (str ":client/" (cstr/replace (cstr/join ">" e) ":" "")) (true? (some #(= (first e) %) live-clients))
                        (strunc (ut/replace-large-base64 (get-in @params-atom e))) (display-name e) nil])
        solver-rows  (ut/keypaths2 @last-solvers-atom)
        solver-rows  (vec (distinct (map (fn [x] (vec (take 3 x))) solver-rows)))
        solver-rows  (for [e solver-rows]
                       [(cstr/replace (str (first e)) ":" "") "solvers" (cstr/replace (str (second e)) ":" "")
                        (str ":solver/" (cstr/replace (cstr/join ">" e) ":" "")) false ;(true? (some #(= (first
                                                                                       ;e) %) live-clients))
                        (strunc (ut/replace-large-base64 (get-in @last-solvers-atom e))) (display-name e) nil])
        signal-rows  (ut/keypaths2 (into {} (filter (fn [[k _]] (keyword? k)) @last-signals-atom)))
        signal-rows  (vec (distinct (map (fn [x] (vec (take 3 x))) signal-rows)))
        signal-rows  (for [e signal-rows]
                       [(cstr/replace (str (first e)) ":" "") "signals" (cstr/replace (str (second e)) ":" "")
                        (str ":signal/" (cstr/replace (cstr/join ">" e) ":" "")) false ;(true? (some #(= (first
                                                                                       ;e) %) live-clients))
                        (strunc (ut/replace-large-base64 (get-in @last-signals-atom e))) (display-name e) nil])
        panel-rows   (vec (filter #(and (or (= (get % 2) :views) (= (get % 2) :queries))) (ut/kvpaths @panels-atom)))
        panel-rows   (vec (distinct (map (fn [x] (vec (take 4 x))) panel-rows)))
        panel-rows   (vec (filter #(= (count %) 4) panel-rows)) ;; hacking - merge all this once
        panel-rows   (for [e    panel-rows
                           :let [param? (= :param (str (get e 2)))]]
                       [(cstr/replace (str (first e)) ":" "") (str "client-" (cstr/replace (str (get e 2)) ":" ""))
                        (cstr/replace (str (second e)) ":" "") (str ":panel/" (cstr/replace (cstr/join ">" e) ":" ""))
                        (true? (some #(= (first e) %) live-clients)) (strunc (ut/replace-large-base64 (get-in @panels-atom e)))
                        (display-name e 3)
                        (when (not param?)
                          (try (str (ut/replace-large-base64 (-> (get-in @panels-atom (vec (drop-last (drop-last e))))
                                                                 (dissoc :views)
                                                                 (dissoc :root)
                                                                 (assoc :natural-key (get e 3))
                                                                 (dissoc :tab)
                                                                 (dissoc :queries))))
                               (catch Exception ee (str ee))))])
        block-rows   (vec (filter #(and (or (= (count %) 5) (or (= (last %) :views) (= (last %) :queries)))
                                        (or (= (get % 3) :views) (= (get % 3) :queries))
                                        (= (second %) :panels))
                            (ut/kvpaths @screens-atom)))
        block-rows   (for [e    block-rows
                           :let [block? (or (= (last e) :views) (= (last e) :queries))
                                 e      (if block? (drop-last e) e)
                                 sample (if (not block?)
                                          (try (ut/replace-large-base64 (-> (get-in @screens-atom (vec (drop-last (drop-last e))))
                                                                            (dissoc :views)
                                                                            (dissoc :root)
                                                                            (dissoc :tab)
                                                                            (dissoc :queries)
                                                                            (assoc :natural-key (if block? (get e 3) (get e 4)))))
                                               (catch Exception ee (str ee)))
                                          (try (ut/replace-large-base64 (-> (get-in @screens-atom (vec e))
                                                                            (dissoc :views)
                                                                            (dissoc :root)
                                                                            (assoc :natural-key (last e))
                                                                            (dissoc :tab)
                                                                            (dissoc :queries)))
                                               (catch Exception ee (str ee))))]]
                       [(cstr/replace (str (first e)) ":" "")
                        (str "saved-" (if block? "block" (cstr/replace (str (get e 3)) ":" "")))
                        (cstr/replace (if block?
                                        (str (get sample :name (str (get e 3)))) ;; (str (get e 3))
                                        (str (get e 4)))
                                      ":"
                                      "") (str ":screen/" (cstr/replace (cstr/join ">" e) ":" "")) nil
                        (strunc (ut/replace-large-base64 (get-in @screens-atom e))) (display-name e) (str sample)])
        prows        (vec (apply concat [param-rows flow-rows block-rows panel-rows solver-rows signal-rows]))
        rows         (vec (for [r prows]
                            (zipmap [:item_key :item_type :item_sub_type :value :is_live :sample :display_name :block_meta] r)))
        _ (reset! autocomplete-clover-param-atom (vec (distinct (filter #(and (not (or (cstr/starts-with? (str %) ":panel/")
                                                                                       (cstr/starts-with? (str %) ":client/")))
                                                                              (<= (count (re-seq #"/" (str %))) 1))
                                                                  (mapv :value rows)))))
        delete-sql   {:delete-from [:client_items] :where [:= 1 1]} ;; (cons :or (vec (for [k
       ]
    (enqueue-task3 (fn []
                     (sql-exec system-db (to-sql delete-sql))
                     (doseq [rr (partition-all 50 rows)]
                       (sql-exec system-db (to-sql {:insert-into [:client_items] :values rr})))))))

(defn update-flow-results>sql
  []
  (let [rows (try (vec (apply concat
                         (for [[flow-id v] (ut/replace-large-base64 (dissoc @flow-db/results-atom "client-keepalive"))]
                           (vec (for [[block_key block_value] v]
                                  (stringify-except {:flow_id     flow-id
                                                     :block_key   block_key
                                                     :block_value (try (subs (str block_value) 0 1000)
                                                                       (catch Exception _ (str block_value)))
                                                     :data_type   (ut/data-typer block_value)}
                                                    [:start :end]))))))
                  (catch Exception e (do (ut/pp [:update-flow-results>sql-error (str e)]) [])))]
    (when (ut/ne? rows)
      (sql-exec flows-db (to-sql {:delete-from [:flow_results]}))
      (doseq [chunk (partition-all 50 rows)] (sql-exec flows-db (to-sql {:insert-into [:flow_results] :values (vec chunk)}))))))

(defn update-channel-history>sql
  []
  (let [rows          (try (apply concat
                             (for [[flow-id v] (ut/replace-large-base64 (dissoc @flow-db/channel-history "client-keepalive"))]
                               (for [vv v]
                                 (stringify-except (walk/postwalk-replace {:data-type :data_type}
                                                                          (merge
                                                                            {:flow_id  flow-id
                                                                             :start_ts (ut/millis-to-date-string (get vv :start))
                                                                             :end_ts   (ut/millis-to-date-string (get vv :end))}
                                                                            vv))
                                                   [:start :end]))))
                           (catch Exception e (do (ut/pp [:update-channel-history>sql-error (str e)]) [])))
        le            (sql-query flows-db (to-sql {:select [[[:max :end] :last_end]] :from [:channel_history]}))
        lee           (get-in le [0 :last_end] 0)
        lee           (or lee 0) ;; weird, but sometimes less was nil - TODO
        rows-filtered (vec (filter #(> (get % :end) lee) rows))
        rows-filtered (vec (for [r    rows-filtered
                                 :let [v (str (if (> (count (get r :value)) 3000) (subs (get r :value) 0 3000) (get r :value)))]]
                             (assoc r :value v)))
        insert-sql    {:insert-into [:channel_history] :values rows-filtered}]
    (when (not (empty? rows-filtered))
      (do ;(ut/pp [:channel-history-added>sql :last-end lee :full (count rows) :filtered (count
        (sql-exec flows-db (to-sql insert-sql) :update-channel-history>sql)))))

(defn update-fn-history>sql
  []
  (let [rows          (try (apply concat
                             (for [[flow-id v] (ut/replace-large-base64 (dissoc @flow-db/fn-history "client-keepalive"))]
                               (for [vv v]
                                 (stringify-except (walk/postwalk-replace {:from :from_block :data-type :data_type}
                                                                          (merge
                                                                            {:flow_id  flow-id
                                                                             :start_ts (ut/millis-to-date-string (get vv :start))
                                                                             :end_ts   (ut/millis-to-date-string (get vv :end))}
                                                                            vv))
                                                   [:start :end :elapsed-ms]))))
                           (catch Exception e (do (ut/pp [:update-fn-history>sql-error (str e)]) [])))
        le            (sql-query flows-db (to-sql {:select [[[:max :end] :last_end]] :from [:fn_history]}))
        lee           (get-in le [0 :last_end] 0)
        lee           (or lee 0) ;; weird, but sometimes less was nil - TODO
        rows-filtered (vec (filter #(> (get % :end) lee) rows))
        rows-filtered (vec (for [r    rows-filtered
                                 :let [v (if (> (count (get r :value)) 3000) (subs (get r :value) 0 3000) (get r :value))]]
                             (assoc r :value v)))
        insert-sql    {:insert-into [:fn_history] :values rows-filtered}]
    (when (not (empty? rows-filtered))
      (do ;(ut/pp [:fn-history-added>sql :last-end lee :full (count rows) :filtered (count
        (sql-exec flows-db (to-sql insert-sql) :update-fn-history>sql)))))

(defn update-live-schedules>sql
  []
  (let [rows       (try (vec (for [v @flow-db/live-schedules] (stringify-except v [:start :end])))
                        (catch Exception e (do (ut/pp [:update-flow-results>sql-error (str e)]) [])))
        insert-sql {:insert-into [:live_schedules] :values rows}]
    (when (not (empty? rows))
      (do (sql-exec flows-db (to-sql {:delete-from [:live_schedules]})) (sql-exec flows-db (to-sql insert-sql))))))

(def checkpoint-atom (atom {}))

(defn execute-if-changed
  [input-atom f name]
  (let [current-hash (hash @input-atom)]
    (when (not= (get @checkpoint-atom name) current-hash)
      (swap! checkpoint-atom assoc name current-hash)
      (do ;(ut/pp [:changed-flow-atom! :running name])
        (f)))))

(defn flow-atoms>sql
  []
  (execute-if-changed flow-db/live-schedules update-live-schedules>sql :live-schedules)
  (execute-if-changed flow-db/results-atom update-flow-results>sql :flow-results)
  (execute-if-changed flow-db/channel-history update-channel-history>sql :channel-history)
  (execute-if-changed flow-db/fn-history update-fn-history>sql :fn-history))


(defn count-watchers
  [atom]
  (try (let [field (.getDeclaredField clojure.lang.Atom "watches")]
         (.setAccessible field true)
         (-> atom
             (.get field)
             (count)))
       (catch Exception e (println "Error counting watchers:" (.getMessage e)) nil)))

(def channel-counts (atom {}))
(def stats-shadow (ut/thaw-atom [] "./data/atoms/stats-shadow-atom.edn"))

(defn last-x-items [v x] (let [start (max 0 (- (count v) x))] (subvec v start)))

(defn average-chunks
  [data]
  (vec (map (fn [chunk]
              {:mem     (float (/ (reduce + (map :mem chunk)) (count chunk)))
               :threads (float (/ (reduce + (map :threads chunk)) (count chunk)))
               :load    (float (/ (reduce + (map :load chunk)) (count chunk)))
               :subs    (float (/ (reduce + (map :subs chunk)) (count chunk)))})
         (partition 50 50 [] data))))

(defn find-max-key
  [data]
  (let [max-threads (apply max (map :threads data))
        max-subs    (apply max (map :subs data))]
    (if (> max-threads max-subs) :threads :subs)))

(def mps-helper (atom {}))
(def last-stats-row (atom {}))
(def booted (atom nil))
(def clients-alive (atom nil))

(defn jvm-stats
  []
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
            ttl (try (apply + (for [[_ v] @atoms-and-watchers] (count (keys v)))) (catch Exception _ -1))
            _ (swap! stats-shadow conj {:mem mm :tick (count @stats-shadow) :threads thread-count :subs ttl :load sys-load})
            chart-view (fn [data]
                         (let [max-key (find-max-key data)]
                           [:> :ResponsiveContainer {:width "100%" :height :panel-height+50}
                            [:> :ComposedChart
                             {:data   data ;(last-x-items @stats-shadow 50)
                              :margin {:top 5 :bottom 5 :right 30 :left 20}}
                             [:> :CartesianGrid {:strokeDasharray "1 4" :opacity 0.33}]
                             [:> :Tooltip {:contentStyle {:backgroundColor "#00000099"}}] [:> :XAxis {:dataKey :tick :hide true}]
                             [:> :YAxis {:yAxisId "left" :hide true :dataKey :mem}]
                             [:> :YAxis
                              {:yAxisId     "right"
                               :hide        true
                               :dataKey     max-key ;:threads
                               :orientation "right"}]
                             [:> :Bar
                              {:yAxisId           "left"
                               :dataKey           :mem
                               :isAnimationActive false
                               :stroke            :theme/editor-outer-rim-color
                               :fill              [:string :theme/editor-outer-rim-color "33"]}]
                             [:> :Line
                              {:yAxisId           "right"
                               :strokeWidth       4
                               :type              "monotone"
                               :dot               false
                               :dataKey           :threads
                               :isAnimationActive false
                               :stroke            :theme/editor-grid-selected-background-color
                               :fill              :theme/editor-grid-selected-background-color}]
                             [:> :Line
                              {:yAxisId           "right"
                               :strokeWidth       3
                               :type              "monotone"
                               :dot               false
                               :dataKey           :subs
                               :isAnimationActive false
                               :stroke            :theme/block-tab-selected-font-color
                               :fill              :theme/block-tab-selected-font-color
                               :strokeDasharray   "5 5"}]]]))
            ack-scoreboardv (into {}
                                  (for [[k v] (client-statuses)
                                        :when (not= (get v :last-seen-seconds) -1)]
                                    {k (ut/deselect-keys v [:booted :last-ack :last-push])}))
            cli-rows
              (vec
                (for [[k v] ack-scoreboardv
                      :let  [booted                     (get v :booted-ts)
                             now                        (System/currentTimeMillis)
                             pushed                     (get v :push)
                             [last-now last-pushed]     (get @mps-helper k)
                             _ (swap! mps-helper assoc k [now pushed])
                             recent-messages-per-second (try (Double/parseDouble (format "%.2f"
                                                                                         (/ (- pushed last-pushed)
                                                                                            (/ (- now last-now) 1000.0))))
                                                             (catch Exception _ -1))
                             uptime-seconds             (try (/ (- now booted) 1000.0) (catch Exception _ -1))
                             msg-per-second             (try (Double/parseDouble (format "%.2f" (/ pushed uptime-seconds)))
                                                             (catch Exception _ -1))
                             uptime-str                 (ut/format-duration-seconds uptime-seconds)
                             _ (swap! ack-scoreboard assoc-in [k :uptime] uptime-str) ;; bad
                             _ (swap! ack-scoreboard assoc-in [k :messages-per-second] msg-per-second) ;; ^^ this.
                             _ (swap! ack-scoreboard assoc-in [k :recent-messages-per-second] recent-messages-per-second) ;; ^^
                                                                                                                          ;; this.
                             queue-distro               (get v :queue-distro)]]
                  (merge
                    {:client-name (str k) :uptime-seconds uptime-seconds :messages-per-second msg-per-second :uptime uptime-str}
                    (assoc v :queue-distro (pr-str queue-distro)))))
            _ (doseq [cli-row cli-rows]
                (swap! params-atom assoc-in [(edn/read-string (get cli-row :client-name)) :stats] cli-row))
            insert-cli {:insert-into [:client_stats] :values cli-rows}
            seconds-since-last (try (/ (- (System/currentTimeMillis) (get @last-stats-row :unix_ms)) 1000) (catch Exception _ -1))
            seconds-since-boot (try (/ (- (System/currentTimeMillis) @booted) 1000) (catch Exception _ -1))
            last-messages (- @all-pushes (get @last-stats-row :messages 0))
            queries-since-last (- (+ @q-calls @q-calls2)
                                  (+ (get @last-stats-row :queries_run 0) (get @last-stats-row :internal_queries_run 0)))
            as-double (fn [x] (Double/parseDouble (clojure.pprint/cl-format nil "~,2f" x)))
            jvm-stats-vals
              {:used_memory_mb             mm
               :messages                   @all-pushes
               :unix_ms                    (System/currentTimeMillis)
               :uptime_seconds             seconds-since-boot
               :seconds_since_last_update  seconds-since-last
               :messages_per_second        (as-double (try (/ @all-pushes seconds-since-boot) (catch Exception _ -1)))
               :recent_messages_per_second (as-double (try (/ last-messages seconds-since-last) (catch Exception _ -1)))
               :recent_queries_run         queries-since-last
               :recent_messages            (- @all-pushes last-messages)
               :recent_queries_per_second  (as-double (try (/ queries-since-last seconds-since-last) (catch Exception _ -1)))
               :queries_per_second         (try (/ (+ @q-calls @q-calls2) seconds-since-boot) (catch Exception _ -1))
               :thread_count               thread-count
               :sql_cache_size             (count @sql-cache)
               :ws_peers                   (count @wl/sockets)
               :subscriptions              ttl
               :open_flow_channels         (apply + (for [[_ v] (flow-statuses)] (get v :channels-open)))
               :queries_run                @q-calls
               :internal_queries_run       @q-calls2
               :sniffs_run                 @cruiser/sniffs
               :sys_load                   (as-double sys-load) ;;sys-load to 2 decimal places
              }
            _ (reset! last-stats-row jvm-stats-vals)
            insert-sql {:insert-into [:jvm_stats] :values [jvm-stats-vals]}
            _ (swap! server-atom assoc :uptime (ut/format-duration-seconds seconds-since-boot))]
        (swap! stats-cnt inc)
        (sql-exec system-db (to-sql {:delete-from [:client_stats]}))
        (sql-exec system-db (to-sql insert-cli))
        (sql-exec system-db (to-sql insert-sql))
        (when booted?
          (println " ")
          (reset! booted (System/currentTimeMillis))
          (ut/print-ansi-art "nname.ans")
          (ut/pp [:version 0 :june 2024 "Hi."])
          (println " "))
        (let [fss (flow-statuses) fssk (vec (keys fss))] (ut/pp [:flow-status (select-keys fss fssk)]))
        ;;(ut/pp [:date-map @time-atom])
        (ut/pp [:sql-errors!
                {:ttl     (count @sql/errors)
                 :freq    (frequencies (mapv first @sql/errors))
                 :freq-db (frequencies (mapv second @sql/errors))}])
        (try (let [peers       (count @wl/sockets)
                   uptime-str  (ut/format-duration-seconds (ut/uptime-seconds))
                   server-subs ttl]
               (swap! server-atom assoc :uptime uptime-str :clients peers :threads thread-count :memory mm)
               (ut/pp ["     " :jvm-stats
                       {:*cached-queries              (count @sql-cache)
                        :clover-sql-training-queries  (count (keys @clover-sql-training-atom))
                        :clover-sql-training-enriched (count (keys @clover-sql-enriched-training-atom))
                        :ws-peers                     peers
                        :sys-load                     sys-load
                        :cwidth                       (ut/get-terminal-width)
                        :uptime                       uptime-str
                        :server-subs                  server-subs
                        :*jvm-memory-used             [(ut/nf mm) :mb]
                        :*current-threads             thread-count}]))
             (catch Throwable e (ut/pp [:printing-shit-error? (str e)])))
        (when (or booted? (zero? (mod @stats-cnt 100)))
          (doseq [[client-name v] @atoms-and-watchers]
            (let [clients (count @wl/sockets)]
              (alert! client-name
                      [:v-box :justify :center :style
                       {;:margin-top "-6px"
                        :opacity 0.7} ;:color (if error? "red" "inherit")}
                       :children
                       [[:box :style {:font-weight 700 :font-size "18px"} :child (str "[sys-stats] " client-name)]
                        [:box :style {:font-weight 700 :font-size "11px" :opacity 0.6} :child
                         (str thread-count
                              " threads,  "
                              (ut/nf mm)
                              " MB used on server, "
                              (ut/nf ttl)
                              " active client subs, "
                              clients
                              " client"
                              (when (> clients 1) "s")
                              " connected")]
                        [:box :style {:font-weight 700 :font-size "11px" :opacity 0.6} :child
                         (str "uptime: " (ut/format-duration-seconds (ut/uptime-seconds)))]
                        [:box :style {:font-weight 700 :font-size "11px" :opacity 0.6} :child
                         (str "you have " (count (keys v)) " (server) watcher subs")]
                        (when booted?
                          [:box :style {:color :theme/editor-outer-rim-color :font-weight 700} :child
                           [:speak-always (str "Hello. data rabbit system is now online.")]])]]
                      10
                      (if booted? 2.2 1.7)
                      6)
              (alert! client-name (chart-view (last-x-items (average-chunks @stats-shadow) 10)) 10 4 5)
              (alert! client-name (chart-view (last-x-items @stats-shadow 50)) 10 4 5)))))
      (catch Exception e (ut/pp [:jvm-stats (str e)])))))






















(defn web-handler
  [request]
  {:status  200
   :headers {"Content-Type" "text/html"}
   :body    "<html><head></head><body>youre never going to see this, bro</body></html>"})

(def websocket-port 3030)


(def ws-endpoints {"/ws" (net/websocket-handler {:encoding :edn})})

(def ring-options-old
  {:port websocket-port :join? false :async? true :max-idle-time 5000 :websockets ws-endpoints :allow-null-path-info true})


(def ring-options
  {:port                 websocket-port
   :join?                false
   :async?               true
   :min-threads          300
   :max-threads          1000 ;; Increased max threads
   :idle-timeout         1000000 ;; Increased idle timeout
   :max-idle-time        6000000 ;; Increased max idle time
   :input-buffer-size    131072 ;;32768  ;; Increased buffer sizes
   :output-buffer-size   131072 ;;32768
   :max-message-size     6291456 ;;2097152  ;; Increased max message size
   :websockets           ws-endpoints
   :allow-null-path-info true})




(defonce websocket-server (atom nil))





(defn destroy-websocket-server!
  []
  (ut/ppa [:shutting-down-websocket-server :port websocket-port])
  (try (do (when @websocket-server (.stop @websocket-server) @websocket-server (reset! websocket-server nil)) @websocket-server)
       (catch Throwable _ nil)))













(defn home-page [request] (ring-resp/response "Hello World! Home!"))
(defn static-root [request] (ring-resp/content-type (ring-resp/resource-response "index.html" {:root "public"}) "text/html"))
(def common-interceptors [(body-params/body-params) http/html-body])

(def routes
  #{["/" :get (conj common-interceptors `static-root)] ["/save" :post (conj common-interceptors `save)]
    ["/save-flow" :post (conj common-interceptors `save-flow)] ["/save-snap" :post (conj common-interceptors `save-snap)]
    ["/save-screen-snap" :post (conj common-interceptors `save-screen-snap)]
    ["/save-csv" :post (conj common-interceptors `save-csv)] ["/load" :get (conj common-interceptors `load-screen)]
    ["/audio" :post (conj common-interceptors `get-audio)] ["/load-flow" :get (conj common-interceptors `load-flow)]
    ["/load-flow-history" :get (conj common-interceptors `load-flow-history)]})

(def web-server-port 8888) ;; 8888

(def service
  {:env                     :prod
   ::http/routes            routes
   ::http/allowed-origins   {:creds false :allowed-origins (constantly true)}
   ::http/secure-headers    {:content-security-policy-settings {:object-src "none"}}
   ::http/resource-path     "/public"
   :max-threads             150
   ::http/type              :jetty
   ::http/host              "0.0.0.0"
   ::http/port              web-server-port
   ::http/container-options {:h2c? true :h2? false :ssl? false}})

(defonce runnable-service (server/create-server service))

(def web-server (atom nil))

(defn create-web-server!
  []
  (ut/ppa [:starting-web-server :port web-server-port])
  (reset! web-server (server/start runnable-service)))

(defn stop-web-server!
  []
  (ut/ppa [:shutting-down-web-server :port web-server-port])
  (when @web-server (server/stop @web-server) (reset! web-server nil)))
