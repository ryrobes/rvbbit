(ns rvbbit-backend.websockets
  (:require
   ;[rvbbit-backend.xtdb-init :as xtdb-init]
   [clojure.core.async        :as    async
    :refer [<! >! <!! >!! go chan]]
   [websocket-layer.core      :as wl]
   [ring.adapter.jetty9       :as jetty]
   [clojure.pprint            :as pprint]
   [websocket-layer.network   :as net]
   [cognitect.transit         :as transit]
   [nextjournal.beholder      :as beholder]
   [io.pedestal.http          :as http]
   [clojure.java.shell        :as shell]
   [clojure.java.jdbc         :as jdbc]
   [flowmaps.db               :as flow-db]
   [clojure.data              :as data]
   [datalevin.core :as d]
   [rvbbit-backend.external   :as ext]
   [rvbbit-backend.endpoints   :as edp]
   [rvbbit-backend.db         :as db]
   [rvbbit-backend.shape-rotator :as rota]
   [rvbbit-backend.leaves     :as leaves]
   [rvbbit-backend.nested-pivot :as npiv]
   [rvbbit-backend.assistants :as assistants]
   [rvbbit-backend.evaluator  :as evl]
   [rvbbit-backend.stacks     :as stacks]
   [rvbbit-backend.rabbit-script     :as script]
   [io.pedestal.http.body-params :as body-params]
   [chime.core                :as chime]
   [flowmaps.core             :as flow]
   [rvbbit-backend.embeddings :as em]
   [rvbbit-backend.pool-party :as ppy]
   [clj-ssh.ssh               :as ssh]
   [cheshire.core             :as json]
   [ring.util.response        :as ring-resp]
   [io.pedestal.http          :as server]
   [taskpool.taskpool         :as tp]
   [clojure.edn               :as edn]
   [clojure.core.memoize      :as memoize]
   [rvbbit-backend.util       :as    ut
    :refer [ne?]]
   [io.pedestal.http.route    :as route]
   [hikari-cp.core            :as hik]
   [clojure.string            :as cstr]
   [rvbbit-backend.surveyor   :as surveyor]
   [rvbbit-backend.realms     :as realms]
   [rvbbit-backend.freezepop  :as fpop]
   [rvbbit-backend.surveyor   :as svy]
   [rvbbit-backend.queue-party :as qp]
   [rvbbit-backend.transform  :as ts]
   [puget.printer             :as puget]
   [rvbbit-backend.pivot      :as pivot]
   [rvbbit-backend.sql        :as    sql
    :refer [sql-exec sql-query sql-query-one system-db system-reporting-db import-db systemh2-db stack-db
            cache-db cache-db-memory ghost-db insert-error-row! to-sql pool-create]]
   [clojure.data.csv          :as csv]
   [csv-map.core              :as ccsv]
   [clojure.core.cache        :as cache]
   [clojure.java.io           :as io]
   [rvbbit-backend.ddl        :as ddl]
   [rvbbit-backend.ddl        :as sqlite-ddl] ;; needed for hardcoded rowset filter fn
  ;;  [rvbbit-backend.cruiser    :as cruiser]
   [com.climate.claypoole     :as cp]
   [clj-figlet.core           :as figlet]
   [clj-http.client           :as client]
   [clojure.data.json         :as json2]
   [clojure.set               :as cset]
   [clojure.walk              :as walk]
   [clj-time.coerce           :as coerce]
   [rvbbit-backend.config     :as config]
   [honey.sql                 :as honey])
  (:import
   ;;;(org.eclipse.jetty.util.log Log)
   [com.github.vertical_blank.sqlformatter SqlFormatter]
   [org.eclipse.jetty.util.thread QueuedThreadPool]
   [java.nio.file Paths]
   [java.util.concurrent                  ThreadFactory Executors ThreadPoolExecutor SynchronousQueue TimeUnit TimeoutException ArrayBlockingQueue ThreadPoolExecutor$CallerRunsPolicy]
   [java.lang                              ProcessBuilder]
   [java.io                                BufferedReader InputStreamReader StringWriter]))

;; =======================================================================================================================================
;; Note to readers: lots of refactoring, namespace, mess cleanup to do, I just came out of a very long "move fast and break things" season
;; Note to readers: lots of refactoring, namespace, mess cleanup to do, I just came out of a very long "move fast and break things" season
;; Note to readers: lots of refactoring, namespace, mess cleanup to do, I just came out of a very long "move fast and break things" season
;; =======================================================================================================================================


(defonce viz-reco-usage (atom []))
(defonce leaf-eval-usage (atom []))
(defonce solver-usage (atom []))
(defonce push-usage (atom []))
(defonce peer-usage (atom []))
(defonce pool-tasks (atom []))
(defonce pool-task-usage (atom []))
(defonce queue-tasks (atom []))
(defonce watcher-usage (atom []))
(defonce sub-usage (atom []))
(defonce sub-client-usage (atom []))
(defonce clover-params-usage (atom []))
(defonce sql-exec-usage (atom []))
(defonce sql-query-usage (atom []))
(defonce flow-usage (atom []))
(defonce nrepl-usage (atom []))
(defonce nrepl-intros-usage (atom []))



(defonce client-panels-in-sync? (atom {})) ;; client-name bool

(defonce sent-reactions (atom 0))
(defonce sent-signal-reactions (atom 0))

(defonce reaction-usage (atom []))
(defonce signal-reaction-usage (atom []))
;(defonce kit-client-maps (atom {}))

(defonce client-metrics (atom {})) ;; (fpop/thaw-atom {} "./data/atoms/client-metrics-atom.msgpack.transit")) ;;(atom {}))
(defonce sql-metrics (atom {}))
(defonce atom-metrics (atom {}))

(defonce sys-load (atom []))
(defonce thread-usage (atom []))

(defonce non-heap-mem-usage (atom []))
;(defonce time-usage (atom []))
(defonce scheduler-atom (atom {}))

;; (defonce conn-map (atom {}))

;; (defonce watcher-log (atom {}))

(defonce timekeeper-failovers (atom {}))

;; (def client-panels (atom {})) ;; (fpop/thaw-atom {} "./data/atoms/client-panels-atom.msgpack.transit"))
;; (def client-panels-resolved (atom {}))
;; (def client-panels-materialized (atom {}))
;; (def client-panels-data (atom {})) ;; (fpop/thaw-atom {} "./data/atoms/client-panels-data-atom.msgpack.transit")) ;; TEMP
;; (def client-panels-metadata (atom {})) ;; (fpop/thaw-atom {} "./data/atoms/client-panels-metadata-atom.msgpack.transit"))

(def materialized-full-queries (fpop/thaw-atom {} "./data/atoms/materialized-full-queries.msgpack.transit"))

;; (def client-panels-history (atom {}))
;;(def client-panels-history (fpop/thaw-atom {} "./data/atoms/client-panel-history-atom.edn"))

(defonce quick-sniff-hash (atom {}))
(defonce honey-echo (atom {})) ;; (fpop/thaw-atom {} "./data/atoms/honey-echo-atom.msgpack.transit"))

(def heartbeat-seconds 15) ;; 15

;;(def num-groups 8) ;; atom segments to split solvers master atom into
(def num-groups 64) ;; eyes emoji

;; 20 seems fine, testing with 8... impacts thread count mostly. not good isolated perf testing yet.

(def stats-cnt (atom 0))
(def restart-map (atom {}))
(def orig-caller (atom {}))
(def sub-flow-blocks (atom {}))
(def custom-flow-blocks (atom {})) ;; (fpop/thaw-atom {} "./data/atoms/custom-flow-blocks-atom.msgpack.transit"))

(defonce watchdog-atom (atom {}))
(defonce last-block-written (atom {})) ;; kind of wasteful, but it's a small atom and is clean.
(defonce latest-run-id (fpop/thaw-atom {} "./data/atoms/latest-run-id-atom.edn"))
(defonce shutting-down? (atom false))
(defonce tracker-client-only (atom {}))
(defonce acc-trackers (atom {}))
(defonce temp-error-blocks (atom {}))
(defonce tracker-history (atom {}))

;; (def ack-scoreboard (atom {}))
(def ping-ts (atom {}))
(def client-latency (atom {}))

;; (defonce screens-atom (atom {})) ;; (fpop/thaw-atom {} "./data/atoms/screens-atom.edn"))
;; (defonce server-atom (fpop/thaw-atom {} "./data/atoms/server-atom.edn"))
;; (defonce flow-status (atom {}))
;; (defonce kit-status (atom {}))
;; (defonce last-signals-atom (fpop/thaw-atom {} "./data/atoms/last-signals-atom.edn"))
(defonce signal-parts-atom (atom []))
;; (defonce last-signals-history-atom (fpop/thaw-atom {} "./data/atoms/last-signals-history-atom.edn"))
;; (defonce last-signal-value-atom (fpop/thaw-atom {} "./data/atoms/last-signal-value-atom.edn"))
;; (defonce last-signals-atom-stamp (fpop/thaw-atom {} "./data/atoms/last-signals-atom-stamp.edn"))
;; (defonce panels-atom (fpop/thaw-atom {} "./data/atoms/panels-atom.edn"))
;; (defonce solver-status (atom {}))
(defonce signals-atom (fpop/thaw-atom {} "./defs/signals.edn"))
(defonce rules-atom (fpop/thaw-atom {} "./defs/rules.edn"))
(defonce solvers-atom (fpop/thaw-atom {} "./defs/solvers.edn"))
(defonce solvers-cache-atom (atom {})) ;(fpop/thaw-atom {} "./data/atoms/solvers-cache.msgpack.transit"))
(defonce solvers-cache-hits-atom (atom {})) ;(fpop/thaw-atom {} "./data/atoms/solvers-cache-hits.msgpack.transit"))
;; (defonce last-solvers-atom (fpop/thaw-atom {} "./data/atoms/last-solvers-atom.edn"))
;; (defonce last-solvers-data-atom (fpop/thaw-atom {} "./data/atoms/last-solvers-data-atom.edn"))
;; (defonce last-solvers-atom-meta (fpop/thaw-atom {} "./data/atoms/last-solvers-atom-meta.edn"))
;; (defonce last-solvers-history-atom (fpop/thaw-atom {} "./data/atoms/last-solvers-history-atom.edn"))
;; (defonce last-solvers-history-counts-atom (fpop/thaw-atom {} "./data/atoms/last-solvers-history-counts-atom.edn"))

(defonce autocomplete-view-atom (fpop/thaw-atom [] "./data/atoms/autocomplete-view-atom.msgpack.transit"))
(defonce autocomplete-clover-param-atom (fpop/thaw-atom [] "./data/atoms/autocomplete-clover-param-atom.msgpack.transit"))

;; (defonce kit-atom (fpop/thaw-atom {} "./data/atoms/kit-atom.edn"))
(defonce time-atom (fpop/thaw-atom {} "./data/atoms/time-atom.edn"))
;; (defonce father-time (fpop/thaw-atom {} "./data/atoms/father-time-atom.edn")) ;; a hedge; since thread starvation has as times been an issue, cascading into the scheduler itself.

(def pool-stats-atom  (atom {})) ;; (fpop/thaw-atom {} "./data/atoms/pool-stats-atom.edn"))

(def times-atom (atom {})) ;; (fpop/thaw-atom {} "./data/atoms/times-atom.edn"))
;;(def params-atom (fpop/thaw-atom {} "./data/atoms/params-atom.edn"))
;; (def params-atom (atom  {})) ;; stop persisting params, they are dynamic and can be reloaded live (do we *really* care about dead rabbit session params? no)
;; (def atoms-and-watchers (atom {}))

;; (def param-var-mapping (atom {}))
;; (def param-var-crosswalk (atom {}))
;; (def param-var-key-mapping (atom {}))





(def jvm-stats-every 30)




;;; experiment to start and stop running background processes to use, i.e. a python flask code REST server exec, etc
;;;   - is a flow component, but mostly unused and untested.

(def processes (atom {}))

(defn- read-stream
  [input-stream process-id output-key]
  (let [reader (BufferedReader. (InputStreamReader. input-stream))]
    (loop [line (.readLine reader)]
      (when line (swap! processes update-in [process-id :output output-key] #(str % line "\n")) (recur (.readLine reader))))))

(defn get-output [process-id] (get-in @processes [process-id :output]))

(defn start-process
  [process-id command & [wait? ssh-host ssh-user ssh-pass]]
  (if (ut/ne? ssh-host)
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
  (try (let [res (pr-str (SqlFormatter/format sql-str))]
         (if (nil? res) sql-str res))
       (catch Exception _ sql-str)))

(defn flatten-map
  ([m] (flatten-map m '()))
  ([m prefix]
   (if (map? m)
     (reduce-kv (fn [acc k v]
                  (let [new-prefix (conj prefix k)]
                    (merge acc (flatten-map v new-prefix)))) {} m)
     (let [key-str   (clojure.string/join "-" (map name prefix))
           final-key (if (re-matches #"\d.*" key-str) (str "_" key-str) key-str)]
       {(keyword final-key) m}))))

(defn filter-not-done [m]
  (into {}
        (group-by first
                  (vec
                   (for [kp (filter #(= (count %) 3) (ut/kvpaths m))
                         :let [v (get-in m kp)]
                         :when (not (= :done v))] [v kp])))))

(defn clean-key [k]
  (if (keyword? k)
    (keyword (clojure.string/replace (name k) #"/" "")) k))

(defn recursive-clean [data]
  (cond (map? data)  (into {} (map (fn [[k v]] [(clean-key k) (recursive-clean v)]) data))
        (coll? data) (mapv recursive-clean data)
        :else        data))

(defn http-call [req] ;; old version w more hardcoded nonsense
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

(defn throwable->serializable-map [t]
  {:message     (.getMessage t)
   :class       (str (.getName (.getClass t)))
   :cause       (when-let [cause (.getCause t)] {:message (.getMessage cause) :class (str (.getName (.getClass cause)))})
   :stack-trace (map
                 (fn [ste]
                   {:class (.getClassName ste) :method (.getMethodName ste) :file (.getFileName ste) :line (.getLineNumber ste)})
                 (.getStackTrace t))})

(defn parse-error-body [e]
  (try (let [response-body (-> e
                               ex-data
                               :body
                               json2/read-str)]
         {:error (get-in response-body ["error" "message"]) :class (str (type e))})
       (catch Exception _ {:error (str e) :class "ERROR IN PARSE-ERROR-BODY. lol"})))

(defn make-http-call [req]
  (ppy/execute-in-thread-pools-but-deliver :make-http-call ;; try to combat random stackoverflows from this call - new stack in thread pool wrapper
                                           (fn []
                                             (try
                                               (let [{:keys [url headers query-params method body file save-to] :or {method :get}} req
                                                     http-method     (case method
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
                                                     body2             (if (nil? body)
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
                                                     response            (try (http-method url
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
                                                                                                            rrs  (pr-str rr)
                                                                                                            tts  (cstr/replace rrs #":/" ":")
                                                                                                            ttsr (edn/read-string tts)]
                                                                                                        ttsr))))
                                               (catch Exception e ;{:error (str e)
                                                 {;:error (str e)
                                                  :error [(parse-error-body e) (str e)]
                                                  :class (str (type e))})))))


(defmacro timed-expr
  [expr]
  `(let [start#  (System/currentTimeMillis)
         result# ~expr
         end#    (System/currentTimeMillis)]
     {:result result# :elapsed-ms (- end# start#)}))

;; (def cache-db
;;   {:datasource
;;    @(pool-create
;;      {:auto-commit        true
;;       ;:read-only          true
;;       :connection-timeout 30000
;;       ;:validation-timeout 5000
;;       ;:idle-timeout       600000
;;       ;:max-lifetime       1800000
;;       ;:minimum-idle       10
;;       ;:maximum-pool-size  10
;;       :jdbc-url           "jdbc:postgresql://ryanr:notofox@10.174.1.255:5432" ;; needs both? wtf, worked
;;       ;:jdbc-url           "jdbc:postgresql://postgres:notofox@localhost:5432" ;; needs both? wtf, worked
;;       :adapter            "postgresql"
;;       :username           "rvbbit"
;;       ;:username           "postgres"
;;       :password           "notofox"
;;       :database-name      "rvbbit_cache" ;;"postgres" ;; where citus is installed apparently
;;       :server-name        "10.174.1.248"
;;       ;:server-name        "localhost"
;;       :port-number        5432
;;       :register-mbeans    false}
;;      "cache-db")})










(def running-system-queries -1) ;;(counter instrument/metric-registry ["queries" "counters" "running-system-queries"]))
(def running-user-queries -1)  ;;(counter instrument/metric-registry ["queries" "counters" "running-user-queries"]))

(defonce deep-run-list (atom []))
(defonce auto-viz-runs (atom []))
(defonce q-calls (atom 0))
(defonce q-calls2 (atom 0))
(defonce literal-data-map (atom {}))
(defonce literal-data-output (atom {}))

(declare alert!)

(defn insert-rowset-csv ;; [rowset query & columns-vec]
  "takes a 'rowset' (vector of uniform maps) or a vector with a vector of column names
   - inserts it into an in memory SQL db, executes a SQL query on it (via a honey-sql map) and returns it"
  [rowset table-name client-name op-name & columns-vec]
  (ut/pp [:importing-csv-to-sql table-name])
  (let [rowset-type     (cond (and (map? (first rowset)) (vector? rowset))       :rowset
                              (and (not (map? (first rowset))) (vector? rowset)) :vectors)
        table-name-str  (-> table-name str (cstr/replace "-" "_") (cstr/replace ":" ""))
        columns-vec-arg (first columns-vec)
        batch-size      100
        destination-db  import-db
        rowset-fixed    (if (= rowset-type :vectors) (vec (for [r rowset] (zipmap columns-vec-arg r))) rowset)
        columns         (keys (first rowset-fixed))
        values          (vec (for [r rowset-fixed] (vals r)))
        ddl-str         (ddl/create-attribute-sample table-name-str rowset-fixed "SQLite") ;;  "DuckDB") ;;
        extra           [ddl-str columns-vec-arg table-name table-name]]
    (sql-exec system-db
              (to-sql {:insert-into [:status]
                       :columns     [:client_name :op_name :status]
                       :values      [[client-name op-name (str "inserting " (ut/nf (count rowset-fixed)) " rows... ")]]}))
    (sql-exec destination-db (str "drop table if exists " table-name-str " ; ") extra)
    (sql-exec destination-db ddl-str extra)
    (doseq [batch (partition-all batch-size values)] ; (cp/pdoseq pool
      (dorun (sql-exec destination-db (to-sql {:insert-into [(keyword table-name-str)] :columns columns :values batch}) extra)))
    (ut/pp {:sql-csv-table table-name-str :rows (count rowset)})))

;; (defn insert-rowset-old ;; [rowset query & columns-vec]
;;   "takes a 'rowset' (vector of uniform maps) or a vector with a vector of column names
;;    - inserts it into an in memory SQL db, executes a SQL query on it
;;    (via a honey-sql map) and returns it"
;;   [rowset table-name & columns-vec]
;;   (try (let [rowset-type     (cond (and (map? (first rowset)) (vector? rowset))       :rowset
;;                                    (and (not (map? (first rowset))) (vector? rowset)) :vectors)
;;              columns-vec-arg (first columns-vec)
;;              db-conn         cache-db ;cruiser/mem-db2
;;              rowset-fixed    (if (= rowset-type :vectors) (vec (for [r rowset] (zipmap columns-vec-arg r))) rowset)
;;              columns         (keys (first rowset-fixed))
;;              values          (vec (for [r rowset-fixed] (vals r)))
;;              table-name-str  (ut/unkeyword table-name)
;;              ddl-str         (sqlite-ddl/create-attribute-sample table-name-str rowset-fixed)
;;              insert-sql      (to-sql {:insert-into [table-name] :columns columns :values values})
;;              extra           [ddl-str columns-vec-arg table-name table-name-str]]
;;          (sql-exec db-conn (str "drop table if exists " table-name-str " ; ") extra)
;;          (sql-exec db-conn ddl-str extra)
;;          (sql-exec db-conn insert-sql extra)
;;          (ut/pp [:INSERTED-SUCCESS! (count rowset) :into table-name-str])
;;          {:sql-cache-table table-name :rows (count rowset)})
;;        (catch Exception e (ut/pp [:INSERT-ERROR! (str e) table-name]))))

;; (ut/pp (svy/db-typer system-db))


(defn edn->parquet
  "Convert vector of maps to parquet file using DuckDB as intermediary"
  [data parquet-path & {:keys [batch-size] :or {batch-size 5000}}]
  (if (ut/ne? data)
    (try
      (let [table-name     (str "temp_" (random-uuid))
            columns        (keys (first data))
            ddl-str        (sqlite-ddl/create-attribute-sample table-name data "DuckDB")
            columns-str    (cstr/join "," (map name columns))]

        ;; Create temp table
        (sql-exec cache-db (str "drop table if exists " table-name " ; "))
        (sql-exec cache-db ddl-str)

        ;; Insert data in batches
        (doseq [batch  (partition-all batch-size data)
                :let [values     (vec (for [r batch] (vals r)))
                      insert-sql (to-sql {:insert-into [(keyword table-name)]
                                          :columns columns
                                          :values values})]]
          (sql-exec cache-db insert-sql))

        ;; Export to Parquet with compression
        (sql-exec cache-db
                  (str "COPY " table-name " TO '" parquet-path
                       "' (FORMAT PARQUET, COMPRESSION 'ZSTD')"))

        ;; Cleanup
        (sql-exec cache-db (str "DROP TABLE " table-name))

        {:success true
         :path parquet-path
         :rows (count data)})
      (catch Exception e
        {:error (str e)
         :path parquet-path}))
    {:error "Empty dataset"
     :path parquet-path}))

(declare push-to-client)

;;   (edn->parquet large-sample "/tmp/output2222.parquet")
;;   (edn->parquet full-pull "/tmp/big-export444.parquet" :batch-size 10000)

(def active-builds (atom {}))

(defn mark-build-for-kill!
  "Mark a specific build for termination"
  [client-name orig-keypath]
  (swap! active-builds assoc-in [client-name orig-keypath :kill?] true))

(defn start-build!
  [client-name orig-keypath]
  (swap! active-builds assoc-in [client-name orig-keypath :kill?] false))

(defn clear-build!
  "Remove a build from tracking"
  [client-name orig-keypath]
  (swap! active-builds dissoc [client-name orig-keypath]))

(defn should-kill-build?
  "Check if current build should be killed"
  [client-name orig-keypath]
  (get-in @active-builds [client-name orig-keypath :kill?]))

(defn build-running?
  [client-name orig-keypath]
  (ut/ne? (get-in @active-builds [client-name orig-keypath])))

(defn 🦆💬> [client-name ui-keypath status & [format-type]]
  (push-to-client ui-keypath [:duck-status (first ui-keypath)] client-name 1 :duck-status
                  (if format-type
                    [(str "(" format-type ") " (first status)) (last status)]
                    status)))

(defn calculate-optimal-batch-size
  "Calculate optimal batch size targeting ~3000-4000 rows per batch"
  [sample-rows]
  (let [sample-size 100
        samples (take sample-size sample-rows)
        ;; Adjust divisor to 7 for slightly larger batches
        row-size (/ (/ (reduce + (mapv ut/sizeof samples)) sample-size) 7)
        sql-overhead 1.2
        estimated-sql-size #(* % row-size sql-overhead)
        ;; Increase max size slightly since we're being conservative with row_size
        max-sql-size (* 600 1024)

        candidates [1000 1500 2000 2500 3000 3500 4000 5000]

        _ (ut/pp
           {:debug-steps
            {:sample-count (count samples)
             :adjusted-row-size row-size
             :size-estimates
             (into {}
                   (for [size [2000 3000 4000]]
                     [size {:bytes (estimated-sql-size size)
                            :kb (int (/ (estimated-sql-size size) 1024))
                            :under-max? (< (estimated-sql-size size) max-sql-size)}]))}})

        optimal-size (or (->> candidates
                              reverse
                              (filter #(< (estimated-sql-size %) max-sql-size))
                              first)
                         1000)]

    (ut/pp [:final-selection
            {:chosen-size optimal-size
             :estimated-kb (int (/ (estimated-sql-size optimal-size) 1024))
             :max-kb (/ max-sql-size 1024)}])

    optimal-size))

(defn rebuild-cache-table
  [rowset table-name keypath client-name & [columns-vec db-conn queue-name orig-keypath]]
  (let [format :arrow ;:parquet
        build-key [client-name orig-keypath]
        💀? (fn []
              (when (should-kill-build? client-name orig-keypath)
                (🦆💬> client-name orig-keypath ["💀 KILLED! 💀" -1] format)
                (clear-build! client-name orig-keypath)
                (throw (ex-info "Cache table build cancelled - deprecated query!"
                                {:type :build-cancelled
                                 :client client-name
                                 :table table-name}))))]
    ;; Register this run... (which should have already happened, but still)
    (swap! active-builds assoc build-key {:kill? false
                                          :timestamp (System/currentTimeMillis)})
  (🦆💬> client-name orig-keypath ["munging rowset data" 10] format) (💀?)
  (ut/pp ["😈" :rebuild-parquet-cache-table! (first rowset) (count rowset) table-name columns-vec])
  (if (ut/ne? rowset)
    (try
      (let [rowset-type     (cond (and (map? (first rowset)) (vector? rowset))       :rowset
                                  (and (not (map? (first rowset))) (vector? rowset)) :vectors)
            columns-vec-arg  columns-vec
            db-type         (svy/db-typer db-conn)
            rowset-fixed    (if (= rowset-type :vectors)
                              (for [r rowset] (zipmap columns-vec-arg r))
                              rowset)
            columns         (keys (first rowset-fixed))
            table-name-str  (ut/unkeyword table-name)
            view-name       (str table-name-str "_v")
            ddl-str        (sqlite-ddl/create-attribute-sample (str table-name-str "_temp") rowset-fixed db-type)
            extra          {:queue (if (= db-conn cache-db) nil queue-name)
                            :extras [ddl-str columns-vec-arg table-name table-name-str]}

            ;; Dashboard-optimized settings
            row-count       (count rowset-fixed)

            ;; Analyze columns for partitioning
            _ (🦆💬> client-name orig-keypath ["analyzing dimensions" 20] format)
            _ (💀?)
            sample-size     (min 10000 row-count)
            column-stats   (for [col columns
                                 :let [values (take sample-size (map col rowset-fixed))
                                       unique (count (distinct values))
                                       is-numeric? (number? (first (remove nil? values)))]]
                             {:column col
                              :unique-count unique
                              :unique-ratio (/ unique sample-size)
                              :null-ratio (/ (count (filter nil? values)) sample-size)
                              :is-numeric? is-numeric?})

            ;; Smart partition selection for dashboard use
            partition-cols  (->> column-stats
                                 (filter #(and
                                           (not (:is-numeric? %))
                                           (> (:unique-count %) 2)
                                           (< (:unique-count %) 500)
                                           (< (:null-ratio %) 0.1)))
                                 (sort-by :unique-count)
                                 (take 3)
                                 (map (comp name :column)))

            is-partitioned (and (= format :parquet) (seq partition-cols))
            storage-base-path (case format
                                :parquet (str "db/parquet/" table-name-str)
                                :arrow   (str "/tmp/" table-name-str ".arrow"))
            storage-path (if (and is-partitioned (= format :parquet))
                           storage-base-path              ; Just the directory for partitioned parquet
                           (str storage-base-path ".parquet"))

            ;; Dashboard-optimized Parquet settings
            row-group-size  (cond
                              (< row-count 100000)     65536    ; Small datasets
                              (< row-count 1000000)    131072   ; Medium datasets
                              :else                    262144)  ; Large datasets

            page-size      (min 65536 (/ row-group-size 4))  ; Optimize for random access

            format-settings (case format
                              :parquet {:row-group-size (cond
                                                          (< row-count 100000)     65536
                                                          (< row-count 1000000)    131072
                                                          :else                    262144)
                                        :page-size (min 65536 (/ row-group-size 4))
                                        :partition-str (when (and (= format :parquet) (seq partition-cols))
                                                         (str ", PARTITION_BY ("
                                                              (cstr/join "," (map name partition-cols))
                                                              ")"))}
                              :arrow   {:batch-size (min 100000 (max 10000 (/ row-count 10)))})

            _ (when (= format :parquet) (ut/pp [:using-partitions partition-cols]))
            export-options (case format
                             :parquet (str "FORMAT PARQUET, "
                                           "COMPRESSION 'ZSTD', "
                                           "OVERWRITE TRUE, "
                                           "ROW_GROUP_SIZE " (:row-group-size format-settings)
                                           (:partition-str format-settings))
                             :arrow   (str "FORMAT ARROW, "
                                           "COMPRESSION 'LZ4', "
                                           "OVERWRITE TRUE, "
                                           "BATCH_SIZE " (:batch-size format-settings)))

            batch-size (calculate-optimal-batch-size rowset)
            _ (ut/pp [:using-batch-size batch-size])
            ;total-batches (Math/ceil (/ row-count optimal-batch-size))
            ;batch-size 10000
            total-batches (Math/ceil (/ row-count batch-size))
            current-batch (atom 0)]

        ;; Write transit data
        (swap! db/last-solvers-data-atom assoc keypath rowset-fixed)
        (ext/write-transit-data rowset-fixed keypath client-name table-name-str)

        (🦆💬> client-name orig-keypath ["creating temp table" 25] format)

        ;; Create temp table and load data
        (sql-exec db-conn (str "DROP TABLE IF EXISTS " table-name-str "_temp;") extra)
        (sql-exec db-conn ddl-str extra)

        ;; Insert data in transaction
        (doseq [batch (partition-all batch-size rowset-fixed)
                :let [values     (vec (for [r batch] (vals r)))
                      insert-sql (to-sql {:insert-into [(keyword (str table-name-str "_temp"))]
                                          :columns columns
                                          :values values})]]

          ;; Monitor SQL string size, ~100k optimal max?
          (when (> (count insert-sql) (* 100 1024))
            (ut/pp [:warning-large-sql-string (count insert-sql)]))

          (sql-exec db-conn insert-sql extra)
          (swap! current-batch inc)
           ;; Calculate loading progress (25% to 70%)
          (let [load-progress (+ 25 (* 45 (/ @current-batch total-batches)))]
            (💀?)
            (try
              (🦆💬> client-name orig-keypath
                     [(str "loading " @current-batch " / " (int total-batches) " batches")
                      (Math/floor load-progress)] format)
              (catch Exception _ nil))))

        (🦆💬> client-name orig-keypath [(str "exporting to " format) 70] format)
        (💀?)
        (sql-exec db-conn
                  (str "COPY " table-name-str "_temp TO '"
                       storage-base-path  ; Just the base path
                       "' ("
                       export-options
                       ")")
                  extra)

        (🦆💬> client-name orig-keypath ["creating view" 90] format)
        ;; Create/Replace view for querying
        (sql-exec db-conn (str "DROP VIEW IF EXISTS " view-name ";") extra)
        (sql-exec db-conn
                  (str "CREATE VIEW " view-name " AS SELECT * FROM "
                       (case format
                         :parquet
                         (if is-partitioned
                           (str "read_parquet('" storage-base-path "/**/*.parquet')")
                           (str "parquet_scan('" storage-base-path ".parquet')"))
                         :arrow   (str "arrow_scan('" storage-path "')")))
                  extra)

        (when (= format :arrow)
          (🦆💬> client-name orig-keypath ["optimizing arrow memory layout" 95] format)
          (sql-exec db-conn (str "SELECT * FROM " view-name " WHERE false;"))

          (when (< row-count 1000000)
            (doseq [col partition-cols]
              (sql-exec db-conn
                        (str "CREATE INDEX IF NOT EXISTS " table-name-str "_" col
                             " ON " view-name " (" col ");")))))

        (🦆💬> client-name orig-keypath ["cleaning up" 95] format)
        ;; Cleanup temp table
        ;; (sql-exec db-conn (str "DROP TABLE " table-name-str "_temp;") extra)

        ;; (🦆💬> client-name orig-keypath ["generating query stats" 98] format)
        ;; (sql-exec db-conn (str "ANALYZE " view-name ";") extra)

        ;; Run shape rotation (keeping your existing functionality)
        (ppy/execute-in-thread-pools :shape-rotation-jobs
                                     (fn [] (rota/shape-rotation-run db-conn)))

        ;; (🦆💬> client-name orig-keypath [(str "materialization complete " (ut/nf  row-count) " rows") 100] format)
        (clear-build! client-name orig-keypath)
        {:sql-cache-table table-name
         :rows row-count
         :storage-path storage-path
         :view-name view-name
         :partitioned-by (vec partition-cols)
         :settings {:row-group-size row-group-size
                    :page-size page-size}})

      (catch Exception e
        (do (clear-build! client-name orig-keypath)
          (ut/pp [:REBUILD-CACHE-ERROR! (str e) table-name]))))
    (do (clear-build! client-name orig-keypath)
      (ut/pp [:cowardly-wont-build-empty-rowset table-name :puttem-up-puttem-up!])))))



(defn execute-ddl!
  "Execute DDL statement directly without transaction handling"
  [db-conn sql extra]
  (try
    (with-open [conn (.getConnection (:datasource db-conn))
                stmt (.createStatement conn)]
      (.setAutoCommit conn true)  ; Ensure autocommit is on for DDL
      (.execute stmt sql))
    (catch Exception e
      (ut/pp [:ddl-execution-error sql (.getMessage e)]))))

(defn get-table-indexes
  "Get all indexes for a given table"
  [db-conn table-name extra]
  (try
    (let [query (str "SELECT index_name FROM duckdb_indexes() WHERE table_name = '" table-name "'")]
      (vec (map :index_name (sql-query db-conn query extra))))
    (catch Exception e
      (ut/pp [:get-indexes-error table-name (.getMessage e)])
      [])))

;; (reset! active-builds {})
;; (ut/pp (get-table-indexes stack-db "all_offenses_1" nil))

(defn rebuild-duck-cache-table
  [rowset table-name keypath client-name & [columns-vec db-conn queue-name orig-keypath]]
  (let [format :duckdb
        build-key [client-name orig-keypath]
        💀? (fn []
              (when (should-kill-build? client-name orig-keypath)
                (🦆💬> client-name orig-keypath ["💀 KILLED! 💀" -1] format)
                (clear-build! client-name orig-keypath)
                (throw (ex-info "Cache table build cancelled - deprecated query!"
                                {:type :build-cancelled
                                 :client client-name
                                 :table table-name}))))]
    ;; Register this run... (which should have already happened, but still)
    (swap! active-builds assoc build-key {:kill? false
                                          :timestamp (System/currentTimeMillis)})
    (🦆💬> client-name orig-keypath ["munging rowset data" 10] format) (💀?)
    (ut/pp ["😈" :rebuild-duck-cache-table! (first rowset) (count rowset) table-name columns-vec])
    (if (ut/ne? rowset)
      (try
        (let [rowset-type     (cond (and (map? (first rowset)) (vector? rowset))       :rowset
                                    (and (not (map? (first rowset))) (vector? rowset)) :vectors)
              columns-vec-arg  columns-vec
              db-type         (svy/db-typer db-conn)
              rowset-fixed    (if (= rowset-type :vectors)
                                (for [r rowset] (zipmap columns-vec-arg r))
                                rowset)
              kkeys-walk      (into {} (for [k (keys (first rowset-fixed))] {k (keyword (ut/keypath-munger [k]))}))
              _ (ut/pp [:kkeys-walk kkeys-walk])
              rowset-fixed    (walk/postwalk-replace kkeys-walk rowset-fixed)
              columns         (keys (first rowset-fixed))
              table-name-str  (ut/unkeyword table-name)
              row-count       (count rowset-fixed)

              sample-size     (min 10000 row-count)
              column-stats   (for [col columns
                                   :let [values (take sample-size (map col rowset-fixed))
                                         unique (count (distinct values))
                                         is-numeric? (number? (first (remove nil? values)))]]
                               {:column col
                                :unique-count unique
                                :unique-ratio (/ unique sample-size)
                                :null-ratio (/ (count (filter nil? values)) sample-size)
                                :is-numeric? is-numeric?})

             ;; Smart partition selection for dashboard use
              partition-cols  (->> column-stats
                                   (filter #(and
                                             (not (:is-numeric? %))
                                             (> (:unique-count %) 2)
                                             (< (:unique-count %) 500)
                                             (< (:null-ratio %) 0.1)))
                                   (sort-by :unique-count)
                                   (take 3)
                                   (map (comp name :column)))

              ddl-str        (sqlite-ddl/create-attribute-sample (str table-name-str "_temp") rowset-fixed db-type)
              extra          {:queue (if (= db-conn cache-db) nil queue-name)
                              :extras [ddl-str columns-vec-arg table-name table-name-str]}
              _ (💀?)
              batch-size (calculate-optimal-batch-size rowset)
              _ (ut/pp [:using-batch-size batch-size])
              total-batches (Math/ceil (/ row-count batch-size))
              current-batch (atom 0)
              _ (🦆💬> client-name orig-keypath ["creating temp table" 25] format)
              _ (sql-exec db-conn (str "DROP TABLE IF EXISTS " table-name-str "_temp") extra)
              _ (sql-exec db-conn (cstr/replace ddl-str ";" "") extra)]

        ;; Insert data in transaction
          (doseq [batch (partition-all batch-size rowset-fixed)
                  :let [values     (vec (for [r batch] (vals r)))
                        insert-sql (to-sql {:insert-into [(keyword (str table-name-str "_temp"))]
                                            :columns columns
                                            :values values})]]
            (sql-exec db-conn insert-sql extra)
            (swap! current-batch inc)
            (let [load-progress (+ 25 (* 45 (/ @current-batch total-batches)))]
              (💀?)
              (try
                (🦆💬> client-name orig-keypath
                       [(str "loading " @current-batch " / " (int total-batches) " batches")
                        (Math/floor load-progress)] format)
                (catch Exception _ nil))))

          (🦆💬> client-name orig-keypath ["swapping tables" 90] format)

          (let [table-exists? (try
                                (sql-exec db-conn (str "SELECT 1 FROM " table-name-str " LIMIT 1") extra)
                                true
                                (catch Exception _ false))]

            (when table-exists?
              (🦆💬> client-name orig-keypath ["dropping indicies" 90] format)
              (doseq [index-name (get-table-indexes db-conn table-name-str extra)]
                (sql-exec db-conn (str "DROP INDEX IF EXISTS " index-name) extra)))


            (if table-exists?
              ;; If table exists, do atomic swap
              (do
                (sql-exec db-conn (str "ALTER TABLE " table-name-str " RENAME TO " table-name-str "_old") extra)
                (sql-exec db-conn (str "ALTER TABLE " table-name-str "_temp RENAME TO " table-name-str) extra)
                (sql-exec db-conn (str "DROP TABLE " table-name-str "_old;") extra)
                (🦆💬> client-name orig-keypath ["tables swapped successfully" 95] format))

              ;; If table doesn't exist, just rename temp to final
              (do
                (sql-exec db-conn (str "ALTER TABLE " table-name-str "_temp RENAME TO " table-name-str) extra)
                (🦆💬> client-name orig-keypath ["new table created" 95] format))))

          (when (< row-count 1000000)
            (🦆💬> client-name orig-keypath ["creating indicies" 95] format)
            (doseq [col partition-cols]
              (sql-exec db-conn
                        (str "CREATE INDEX IF NOT EXISTS " table-name-str "_" col
                             " ON " table-name-str " (" col ")"))))

          (🦆💬> client-name orig-keypath ["analyze table" 95] format)
          (sql-exec db-conn (str "ANALYZE " table-name-str) extra)

          ;; Run shape rotation (keeping your existing functionality)
          (ppy/execute-in-thread-pools :shape-rotation-jobs (fn [] (rota/shape-rotation-run db-conn)))
          (clear-build! client-name orig-keypath)
          (vec (keys (first rowset-fixed))))

        (catch Throwable e
          (do (clear-build! client-name orig-keypath)
              (ut/pp [:REBUILD-CACHE-ERROR! (str e) table-name]))))
      (do (clear-build! client-name orig-keypath)
          (ut/pp [:cowardly-wont-build-empty-rowset table-name :puttem-up-puttem-up!])))))




(defn insert-rowset
  [rowset table-name keypath client-name & [columns-vec db-conn queue-name]]
  (ut/pp ["😈 " :insert-into-cache-db!! (first rowset) (count rowset) table-name columns-vec])
  (if (ut/ne? rowset)
    (try (let [rowset-type     (cond (and (map? (first rowset)) (vector? rowset))       :rowset
                                     (and (not (map? (first rowset))) (vector? rowset)) :vectors)
               columns-vec-arg columns-vec
               db-conn         (or db-conn cache-db)
               db-type         (svy/db-typer db-conn)
               rowset-fixed    (if (= rowset-type :vectors)
                                 (for [r rowset] (zipmap columns-vec-arg r))
                                 rowset)
               columns         (keys (first rowset-fixed))
               table-name-str  (ut/unkeyword table-name)
               ;table-name-str  (ut/unkeyword (str client-name "_" table-name))
               ddl-str         (sqlite-ddl/create-attribute-sample table-name-str rowset-fixed db-type)
               extra           {:queue (if (= db-conn cache-db) nil queue-name)
                                :extras [ddl-str columns-vec-arg table-name table-name-str]}]
           ;;(enqueue-task5d (fn [] (write-transit-data rowset-fixed keypath client-name table-name-str)))
           (swap! db/last-solvers-data-atom assoc keypath rowset-fixed) ;; full data can be clover
           (ext/write-transit-data rowset-fixed keypath client-name table-name-str)
           (sql-exec db-conn (str "drop table if exists " table-name-str " ; ") extra)
           (sql-exec db-conn ddl-str extra)

           (doseq [batch (partition-all 5000 rowset-fixed)
                   :let  [values     (vec (for [r batch] (vals r)))
                          insert-sql (to-sql {:insert-into [table-name] :columns columns :values values})]]
             (sql-exec db-conn insert-sql extra))

           (ppy/execute-in-thread-pools :shape-rotation-jobs (fn [] (rota/shape-rotation-run db-conn))) ;; get full metadata

           ;;(ut/pp [:INSERTED-SUCCESS! (count rowset) :into table-name-str db-type ddl-str (first rowset-fixed)])
           {:sql-cache-table table-name :rows (count rowset)})
         (catch Exception e (ut/pp [:INSERT-ERROR! (str e) table-name])))
    (ut/pp [:cowardly-wont-insert-empty-rowset table-name :puttem-up-puttem-up!])))

(def snap-pushes (atom {}))

(defn insert-rowset-snap
  [rowset table-name keypath client-name & [columns-vec db-conn]]
  ;; (ut/pp [:insert-into-cache-dbss!! (first rowset) (count rowset) table-name columns-vec])
  (if (ut/ne? rowset)
    (try (let [;;rowset-type (cond (and (map? (first rowset)) (vector? rowset)) :rowset
               db-conn        cache-db ;;(or db-conn cache-db)
               db-type         (svy/db-typer db-conn)
               ds             (ut/today-yyyymmdd-hhmm)
               rowset-fixed   (mapv #(assoc % :snapshot_ds ds) rowset)
              ;;  _ (ut/pp [:snap-rows2 ds (take 1 rowset) (take 1 rowset-fixed)])
               columns        (keys (first rowset-fixed))
               table-name-str (ut/unkeyword table-name)
               ddl-str        (sqlite-ddl/create-attribute-sample table-name-str rowset-fixed db-type)
               extra          [ddl-str table-name table-name-str]
               same-schema?   (= ddl-str (get @snap-pushes table-name))]
          ;;  (enqueue-task5d (fn [] (write-transit-data rowset-fixed keypath client-name table-name-str)))
           ;(swap! last-solvers-data-atom assoc keypath rowset-fixed) ;; full data can be clover
           (ext/write-transit-data rowset-fixed keypath client-name table-name-str)
           (when (not same-schema?)
             (sql-exec db-conn (str "drop table if exists " table-name-str " ; ") extra)
             (sql-exec db-conn ddl-str extra))
           (swap! snap-pushes assoc table-name ddl-str)
           (doseq [batch (partition-all 10 rowset-fixed)
                   :let  [values     (vec (for [r batch] (vals r)))
                          insert-sql (to-sql {:insert-into [table-name] :columns columns :values values})]]
             (sql-exec db-conn insert-sql extra))

          ;;  (ut/pp [:SNAP-INSERTED-SUCCESS2! (count rowset) :into table-name-str])
           ;;(ut/pp [:INSERTED-SUCCESS-SNAP! (count rowset) :into table-name-str (str db-conn) ddl-str (first rowset-fixed)])
           {:sql-cache-table table-name :rows (count rowset)})
         (catch Exception e (ut/pp [:INSERT-ERROR! (str e) table-name])))
    (ut/pp [:cowardly-wont-insert-empty-rowset table-name :puttem-up-puttem-up!])))

(declare sub-to-value)

(def all-pushes (atom 0))

(defn inc-score!
  [client-name key & [ts?]]
  (swap! all-pushes inc)
  (swap! db/ack-scoreboard assoc-in
         [client-name key]
         (if ts? [(System/currentTimeMillis) (ut/get-current-timestamp)] (inc (get-in @db/ack-scoreboard [client-name key] 0)))))

(declare client-statuses)

;; (ut/pp (get @db/params-atom :independent-rectangular-sea-urchin-27))

(defmethod wl/handle-push :ack
  [{:keys [client-name memory flow-subs selected-tab screen-name in-sync?]}]
  (inc-score! client-name :ack)
  (swap! client-panels-in-sync? assoc client-name in-sync?)
  (swap! client-latency assoc
         client-name
         (vec (conj (get @client-latency client-name [])
                    (try (- (System/currentTimeMillis) (get @ping-ts client-name)) (catch Exception _ -2)))))

  (ppy/execute-in-thread-pools :boomerang-heartbeat
                               (fn []
                                 ;;(when (odd? (get-in @db/ack-scoreboard [client-name key] 0))
                                   (doseq [fk (vec (keys (get @db/atoms-and-watchers client-name {})))]
                                     (sub-to-value client-name fk true)))
                                 ) ;; resub just in case?

  (let [cstats  (client-statuses)
        latency (get-in cstats [client-name :client-latency])
        server-subs (get-in cstats [client-name :server-subs])
        client-subs (get-in cstats [client-name :client-subs])
        msgs-per-recent (get-in @db/ack-scoreboard [client-name :recent-messages-per-second])
        msgs (get-in @db/ack-scoreboard [client-name :messages-per-second])
        mem-mb (ut/bytes-to-mb (get memory :mem_used))
        _ (swap! client-metrics assoc client-name
                 (conj (get @client-metrics client-name [])
                       {:latency latency
                        :server-subs server-subs
                        :client-subs client-subs
                        :recent-messages-per-second msgs-per-recent
                        :messages-per-second msgs
                        :mem-mb mem-mb}))
        ins-sql {:insert-into [:client-memory]
                 :values      [(-> memory
                                   (assoc :latency latency)
                                   (assoc :server_subs server-subs)
                                   (assoc :client_subs client-subs)
                                   (assoc :recent-messages-per-second msgs-per-recent)
                                   (assoc :messages-per-second msgs)
                                   (assoc :mem_used_mb mem-mb))]}]
    ;; (ut/pp [:hb-ackd client-name])
    (sql-exec systemh2-db (to-sql ins-sql) {:queue :client-memory}))
  (swap! db/ack-scoreboard assoc-in [client-name :memory] (ut/bytes-to-mb (get memory :mem_used)))
  (swap! db/ack-scoreboard assoc-in [client-name :client-sub-list] flow-subs)
  (swap! db/ack-scoreboard assoc-in [client-name :selected-tab] selected-tab)
  (swap! db/ack-scoreboard assoc-in [client-name :screen-name] screen-name)
  (swap! db/ack-scoreboard assoc-in [client-name :client-subs] (count flow-subs))
  (inc-score! client-name :last-ack true))


(defmethod wl/handle-request :ack2 [{:keys [client-name body]}] (inc-score! client-name :last-ack true) {})

(defmethod wl/handle-request :get-theme [{:keys [client-name screen-name]}]
  (get-in @db/screens-atom [screen-name :click-param :theme] {}))

(defmethod wl/handle-request :puget-document [{:keys [text data-colors width opts-map]}]
  (try
    (let [data-colors (walk/keywordize-keys data-colors)
        color-map (assoc (walk/postwalk-replace``
                {:integer :number
                 :universal-pop-color :delimiter
                 :rabbit-code :function-symbol}
                (into {} (for [[k v] data-colors]
                           {k (if (or (= k :universal-pop-color)
                                      (= k :keyword)
                                      (= k :nil))
                               ; [:bold v]
                                [v]
                                [v])})))
                         :tag [(get data-colors :universal-pop-color)])
        ;; _ (ut/pp [:color-map color-map])
        text (edn/read-string text)
        output (with-out-str
                 (puget/with-options
                   (merge {:width width
                           :color-scheme color-map} opts-map)
                   (puget/cprint text)))]
    ;; (println output)
    output)
    (catch Exception e
      (do
        ;; (ut/pp [:error-pugent-document-req (str e)
        ;;                        ;(str (subs (str text) 0 320 ) "...")
        ;;                        ;(str text)
        ;;         data-colors width opts-map])
        (with-out-str
          (puget/with-options
            (merge {:width width} opts-map)
            (puget/cprint (str "[:puget-document :format-error " e "]"))))
        )
      )))

(defonce client-queues (atom {}))
(defonce client-queues-2 (atom {}))
(defonce client-queues-3 (atom {}))
(defonce client-queue-atoms [client-queues]) ;; [client-queues client-queues-2 client-queues-3 ])

;;;(defonce queue-distributions (atom {}))
(defonce dynamic-timeouts (atom {}))

;;(keys @client-queues)

(defn get-adaptive-timeout
  "Calculate an adaptive timeout based on the client's recent average latency.
   Returns a lower timeout value in milliseconds."
  [client-name]
  (try
    (let [latency-data (get @client-latency client-name [])
          ;; _ (when (empty? latency-data)
          ;;     (ut/pp [:warning-no-latency-data client-name]))
          avg-latency (if (seq latency-data)
                        (ut/avg (take-last 5 latency-data))
                        1000) ; Default to 1000ms if no data
          adapted-timeout
          (cond
            (= client-name :rvbbit) 100 ; internal
            (<= avg-latency 200) 250    ; Very fast clients
            (<= avg-latency 400) 400    ; Fast clients
            (<= avg-latency 600) 550    ; Good performance clients
            (<= avg-latency 800) 700    ; Above average clients
            (<= avg-latency 1000) 850   ; Average clients
            (<= avg-latency 1500) 1100  ; Below average clients
            (<= avg-latency 2000) 1350  ; Slow clients
            (<= avg-latency 2500) 1600  ; Very slow clients
            (<= avg-latency 3000) 1850  ; Extremely slow clients
            (<= avg-latency 5000) 3050  ; Call the police
            (<= avg-latency 7000) 5550  ; Call an ambulance
            (<= avg-latency 9000) 7500  ; Call the morgue
            :else 10000)]               ;; ...
      (swap! dynamic-timeouts assoc client-name [avg-latency adapted-timeout])
      adapted-timeout)
    (catch Exception e
      (ut/pp [:error-in-get-adaptive-timeout client-name (.getMessage e)])
      1000))) ; Default timeout in case of any error

(defn safe-get-timeout [client-name]
  (try
    (get-adaptive-timeout client-name)
    (catch Exception e
      (ut/pp [:error-getting-timeout client-name (.getMessage e)])
      1000))) ; Default timeout if anything goes wrong

;; (defn safe-get-timeout [_] 450)

;; custom-watcher-thread-pool ;; websocket-thread-pool
(defn adjust-thread-pool-size [thread-pool new-core-size new-max-size]
  (.setMaximumPoolSize thread-pool new-max-size)
  (.setCorePoolSize thread-pool new-core-size))

;; (defn client-sized-pools []
;;   (let [client-count (max (count @wl/sockets) 1)
;;           ;new-core-size (max 10 (min client-count 50))   ; Min core size of 10, max core size of 50
;;           ;new-max-size  (max 20 (min (* 2 client-count) 100))
;;         watcher-core (+ (* client-count 45) 50)
;;         watcher-max (* client-count 120)
;;         ws-core (+ (* client-count 2) 5)
;;         ws-max  (* client-count 5)]
;;     (ut/pp [:adjusting-pool-sizes-for client-count :clients
;;             [[:websocket-pool ws-core ws-max] [:watcher-pool watcher-core watcher-max]]])
;;     (adjust-thread-pool-size custom-watcher-thread-pool watcher-core watcher-max)
;;     (adjust-thread-pool-size websocket-thread-pool ws-core ws-max)))

;; (cruiser/create-sqlite-sys-tables-if-needed! (sql/create-or-get-client-db-pool :nutritious-byzantium-hound-32))

(defn new-client [client-name]
  (ut/pp [:new-client-is-alive! client-name :opening (count client-queue-atoms) :websocket-queues])
  ;(client-sized-pools)
  ;; (assistants/runstream-boot-agent "Calliope" client-name) ;;; TEMP TESTING!!
  (swap! db/ack-scoreboard assoc-in [client-name :booted-ts] (System/currentTimeMillis))
;;  (sql/create-or-get-client-db-pool client-name)
  ;; (cruiser/create-sqlite-sys-tables-if-needed! (sql/create-or-get-client-db-pool client-name))
  (doseq [cq client-queue-atoms]
    (let [new-queue-atom (atom clojure.lang.PersistentQueue/EMPTY)]
      (swap! cq assoc client-name new-queue-atom))))

(def client-batches (atom {}))

(def sub-task-ids #{:flow :screen :time :signal :server :ext-param :solver :data
                    :kit :solver-status :solver-meta :repl-ns :flow-status :signal-history :panel :client})

(def valid-groups #{:flow-runner :tracker-blocks :acc-tracker :flow :flow-status :kit-status :solver-status :leaf ;;:solver
                    :estimate [:estimate] :heartbeat [:heartbeat] :push-assocs
                    :tracker :condis [:tracker] :alert :alerts :alert1 :alert2 :alert3 :reco :duck-status :reco-status :cnts-meta}) ;; to not skip old dupes

(defn sub-push-loop ;; legacy, but flawed?
  [client-name data cq sub-name] ;; version 2, tries to remove dupe task ids
  (when (not (get @cq client-name)) (new-client client-name)) ;; new? add to atom, create queue
  (inc-score! client-name :booted true)
  (let [results (async/chan (async/sliding-buffer 500))] ;; was 100
    (try (async/go-loop []
           (async/<! (async/timeout
                      (safe-get-timeout client-name)
                      ;;750
                      )) ;; was 1100 ?
           (if-let [queue-atom (get @cq client-name (atom clojure.lang.PersistentQueue/EMPTY))]
             (let [items            (loop [res []]
                                      (if-let [item (ut/dequeue! queue-atom)]
                                        (recur (conj res item))
                                        res))
                   items-by-task-id (group-by :task-id items)
                  ;;  latest-items     (mapv (fn [group] (if
                  ;;                                      (contains? valid-groups (first group))
                  ;;                                       group (last group)))
                  ;;                         (vals items-by-task-id))
                   latest-items     (mapv (fn [[task-id group]] ;; actually works now. lol
                                            (if (contains? valid-groups task-id)
                                              group
                                              [(last group)]))
                                          items-by-task-id)
                   ]
               (if (not-empty latest-items)
                 (let [_ (swap! client-batches update client-name (fnil inc 0))
                      ;;; _ (when (> 1 (count latest-items)) (ut/pp [:sending (count latest-items) :items-to client-name]))
                       message (if (= 1 (count latest-items)) (first latest-items) latest-items)
                       ;message (if (= 1 (count latest-items))
                       ;          [(first latest-items)]
                       ;          latest-items)
                       ]
                   (when (async/>! results message) (recur)))
                 (recur)))
             (recur)))
         (catch Throwable e (ut/pp [:subscription-err!! sub-name (str e) data]) (async/go-loop [] (recur))))
    results))




(defmethod wl/handle-subscription :server-push2
  [{:keys [kind client-name] :as data}]
  (sub-push-loop client-name data client-queues kind))


(defn push-to-client [ui-keypath data client-name queue-id task-id status & [reco-count elapsed-ms]]
  ;(qp/serial-slot-queue :push-to-client :hold ;;client-name
  ;(enqueue-task-slot2 client-name
  ;(ppy/execute-in-thread-pools (keyword (str "client/push-to-client." (cstr/replace (str client-name) ":" "")))
                 ;(fn []
  ;;  (when (cstr/starts-with? (str client-name) ":gor")
  ;;(ut/pp [:running-push! ui-keypath (str (ut/estimate-size-kb data) "kb") client-name queue-id (str (ut/estimate-size-kb status) "kb") task-id  (keys status) (keys data)]) ;)
  ;;  (when (or
  ;;         (cstr/includes? (str client-name) "live")
  ;;         (cstr/includes? (str client-name) "ryanr")) (ut/pp [:push :NONCLIENT? :to-client client-name queue-id task-id]))

  (when true ;(keyword? client-name)
    (try
      (let [rr                0 ;(rand-int 3)
            cq                (get client-queue-atoms rr)
           ;;; _ (swap! queue-distributions assoc client-name (vec (conj (get @queue-distributions client-name []) rr)))
            client-queue-atom (get @cq client-name)]
      ;(swap! queue-status assoc-in [client-name task-id ui-keypath] status)
      ;(swap! queue-data assoc-in [client-name task-id ui-keypath] {:data data :reco-count reco-count :elapsed-ms elapsed-ms})
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
                      :data        (when (try (some #(= (first task-id) %) sub-task-ids) (catch Exception _ true)) ;; server sub doenst need :data, just :status (as val)
                                     [data ;; data is likely needed for :payload and :payload-kp that
                                      (try (get (first reco-count) :cnt) (catch Exception _ reco-count))])
                      :client-name client-name}))
          (do ;[new-queue-atom (atom clojure.lang.PersistentQueue/EMPTY)]
            (new-client client-name)
            (push-to-client ui-keypath data client-name queue-id task-id status reco-count elapsed-ms))))
      (catch Throwable e (ut/pp [:push-to-client-err!! (str e) data])))))
      ;))



(defn react-to-file-changes [{:keys [path type]}]
  (doall
   (let [;path1 (str "." (last (cstr/split (str path) #"/backend")))
         ;_ (ut/pp [:path path])
         splt                (vec (reverse (cstr/split (str path) #"/")))
         ;;_ (ut/pp splt)
         client-name         (keyword (cstr/replace (str (get splt 4)) #".edn" ""))
         root-file           (str "./live/" (ext/fixstr client-name) ".edn")
         tab-name            (str (get splt 3))
         block-name          (str (get splt 2))
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
         (when false ;(= cntsplt 5)
           (do (ut/pp [:create-on-server new-from-server? :pushing-new curr-names (vec missing) subd cntsplt type-of
                       {:path (str path) :type type :panel-key panel-key :block-data block-data}])
               (push-to-client [:file-changed]
                               {:path (str path) :type type :panel-key panel-key :block-data block-data}
                               client-name
                               0
                               :file-change
                               :done)))))
     (when (and valid-edn? (not ignore?) (some #{client-name} (keys @db/ack-scoreboard)))
       (do ;;(ut/pp [:changed-file! (str path) type client-name panel-key :splt-cnt (count splt)
           ;;        (when (not we-did-it?)
           ;;          {;:repacked repacked :oldpacked oldpacked
           ;;             :diff (data/diff repacked oldpacked)})])
           (when (not we-did-it?)
             (push-to-client [:file-changed]
                             {:path (str path) :type type :panel-key panel-key :block-data repacked}
                             client-name
                             0
                             :file-change
                             :done)))))))

;; (defn react-to-file-changes [{:keys [path type]}]
;;   (ut/pp [:react-to-file-changes! path type])
;;   (let [existing-hash-data (get @ext/file-versions path)
;;         incoming-hash-data (ext/calculate-file-hash path)]
;;     (ut/pp [:react-to-file-changes-inner! [path type]
;;             {:existing-hash-data existing-hash-data
;;              :incoming-hash-data incoming-hash-data}])
;;     (ut/pp [:react-to-file-changes! path type])))

(defn subscribe-to-session-changes []
  (let [file-path0 (str "live/")]
    (ut/pp [:subscribe-to-session-changes!])
    (beholder/watch
     #(send-off ext/file-work-agent (react-to-file-changes %))
     ;;(fn [x] (ppy/execute-in-thread-pools :external-editing-watcher  (fn [] (react-to-file-changes x))))
     ;;#(react-to-file-changes %)
     file-path0)))

(defonce last-panels (atom {}))
(defonce last-panels-resolved (atom {}))
(defonce last-panels-materialized (atom {}))

;; (defn hash-objects
;;   [panels]
;;   (into {}
;;         (for [[k v] panels]
;;           {k {:base    (hash v) ; (hash (-> v (dissoc :views) (dissoc :queries)))
;;               :views   (into {} (for [[k v] (get v :views {})] {k (hash v)}))
;;               :queries (into {} (for [[k v] (get v :queries {})] {k (hash v)}))}})))

;; (defn data-objects
;;   [panels]
;;   (into {}
;;         (for [[k v] panels]
;;           {k {:base    v ;(-> v (dissoc :views) (dissoc :queries))
;;               :views   (into {} (for [[k v] (get v :views {})] {k v}))
;;               :queries (into {} (for [[k v] (get v :queries {})] {k v}))}})))


(defn process-runner-keys [v runner-keys process-fn]
  (into {} (for [key runner-keys
                 :let [data (get v key {})]]
             {key (process-fn data)})))

(defn hash-objects
  [panels runner-keys]
  (into {}
        (for [[k v] panels]
          {k (merge
              {:base (hash v)}
              (process-runner-keys v runner-keys #(into {} (for [[k v] %] {k (hash v)}))))})))

(defn data-objects
  [panels runner-keys]
  (into {}
        (for [[k v] panels]
          {k (merge
              {:base v}
              (process-runner-keys v runner-keys #(into {} (for [[k v] %] {k v}))))})))


;(def panel-history (agent nil))
;(set-error-mode! panel-history :continue)

;; new :updated-panels with just one or more and mat,resolved, source versions




;; (defmethod wl/handle-push :updated-panels ;; NOT BEING USED!!!!
;;   [{:keys [panels client-name resolved-panels materialized-panels]}]

;;   (qp/serial-slot-queue :panel-update-serial :serial
;;    (fn [] (ext/write-panels client-name (merge (get @client-panels client-name {}) panels)))) ;; push to file system for beholder cascades

;;   (doseq [p (keys panels)]
;;     (swap! client-panels-history assoc-in [client-name p (System/currentTimeMillis)] {:source (get panels p)
;;                                                                                       :resolved (get resolved-panels p)
;;                                                                                       :materialized (get materialized-panels p)}))

;;   (swap! client-panels assoc client-name
;;          (merge (get @client-panels client-name {}) panels))
;;   (swap! client-panels-resolved assoc client-name
;;          (merge (get @client-panels-resolved client-name {}) resolved-panels))
;;   (swap! client-panels-materialized assoc client-name
;;          (merge (get @client-panels-materialized client-name {}) materialized-panels))

;;   (ut/pp [:single-panel-push! client-name (keys panels)]))


;; (defn push-to-history-db [client-name panels comp-atom tname]
;;   (let [;resolved-panels resolved-panels ;; remove _ keys? Panels panels ;; remove _ keys?
;;         runner-keys (vec (keys (get (config/settings) :runners)))
;;         _ (ut/pp [:runner-keys runner-keys])
;;         prev-data   (get @comp-atom client-name)
;;         prev-hashes (hash-objects prev-data  runner-keys)
;;         this-hashes (hash-objects panels runner-keys)
;;         _           (ut/pp [:push-to-hist client-name tname (keys @comp-atom)])
;;         diffy       (data/diff this-hashes prev-hashes)
;;         diffy-kps   (vec (filter #(and (not (= (count %) 1))
;;                                        ;(not (= (last %) :views))
;;                                        ;(not (= (last %) :queries))
;;                                        (not (some (fn [x] (= (last %) x)) runner-keys)))
;;                                  (ut/kvpaths (first diffy))))
;;         _ (ut/pp [:diffy  diffy-kps tname])
;;         dd          (data-objects panels runner-keys)
;;         pdd         (data-objects prev-data runner-keys)
;;           ;dd (data-objects panels)
;;         _ (ut/pp [:diffy2 tname (keys dd)])
;;         table-name  (keyword (str tname  "-history"))
;;         rows        (vec (for [kp   diffy-kps
;;                                :let [data  (get-in dd kp)
;;                                      pdata (get-in pdd kp)
;;                                      pdiff (first (data/diff data pdata))
;;                                      _ (ut/pp  [:diff-in kp]) ;; error in this for loop when the "data" is lists and symbols...
;;                                      ]]
;;                            {:kp          (str kp)
;;                             :client_name (str client-name)
;;                             :data        (pr-str data)
;;                             :pre_data    (pr-str pdata)
;;                             :diff        (pr-str pdiff)
;;                             :diff_kp     (pr-str (ut/kvpaths pdiff))
;;                             :panel_key   (str (get kp 0))
;;                             :key         (str (get kp 2))
;;                             :type        (str (get kp 1))}))
;;         _ (ut/pp [:diffy3 (count rows) tname])
;;         ins-sql       {:insert-into [table-name] :values rows}
;;         ;;board-ins-sql {:insert-into [:board-history] :values [{:client_name (str client-name) :data (pr-str panels)}]}
;;         ]
;;     ;;(sql-exec history-db (to-sql board-ins-sql))
;;     (when (ut/ne? rows)
;;       (sql-exec history-db (to-sql ins-sql))))
;;   (swap! comp-atom assoc client-name panels))




(declare deep-diff)

(defn kvpaths
  "Generate keypaths for both associative and non-associative structures."
  ([m] (kvpaths [] m ()))
  ([prev m result]
   (cond
     (map? m)
     (reduce-kv (fn [res k v]
                  (let [kp (conj prev k)]
                    (kvpaths kp v (conj res kp))))
                result
                m)

     (or (vector? m) (seq? m))
     (reduce (fn [res [idx v]]
               (let [kp (conj prev idx)]
                 (kvpaths kp v (conj res kp))))
             result
             (map-indexed vector m))

     :else
     (conj result prev))))

(defn list-diff
  "Find differences between two lists, returning a map of changed indices."
  [old-list new-list]
  (let [max-length (max (count old-list) (count new-list))]
    (into {}
          (for [idx (range max-length)
                :let [old-item (nth old-list idx nil)
                      new-item (nth new-list idx nil)]
                :when (not= old-item new-item)]
            [idx (deep-diff old-item new-item)]))))

(defn deep-diff
  "Recursively diff two structures, handling both associative and non-associative types."
  [a b]
  (cond
    (and (map? a) (map? b))
    (let [keys (cset/union (set (keys a)) (set (keys b)))]
      (into {}
            (for [k keys
                  :let [v1 (get a k)
                        v2 (get b k)]
                  :when (not= v1 v2)]
              [k (deep-diff v1 v2)])))

    (and (vector? a) (vector? b))
    (vec (keep-indexed
          (fn [idx [v1 v2]]
            (when (not= v1 v2)
              [idx (deep-diff v1 v2)]))
          (map vector a b)))

    (and (seq? a) (seq? b))
    (let [diff (list-diff a b)]
      (if (empty? diff)
        {}
        {:list-diff diff}))

    (not= a b)
    {:old a :new b}

    :else
    {}))

(defn find-changed-paths
  "Find all changed paths in two structures."
  [old-data new-data]
  (let [diff (deep-diff old-data new-data)]
    (if (or (seq? diff) (vector? diff))
      (kvpaths [] diff ())
      (kvpaths diff))))

(defn push-to-history-db [client-name panels comp-atom tname]
  (let [;;runner-keys (vec (keys (get (config/settings) :runners)))
        ;;_ (ut/pp [:runner-keys runner-keys])
        panels (ut/deep-remove-keys panels []) ;; will remove :_keys
        prev-data   (get @comp-atom client-name)
        ;;_           (ut/pp [:push-to-hist client-name tname (keys @comp-atom)])
        changed-paths (find-changed-paths prev-data panels)
        diffy-kps   (vec (filter #(and ;(not (= (count %) 1))
                                   (= (count %) 3)
                                       ;(not (some (fn [x] (= (last %) x)) runner-keys))
                                   )
                                 changed-paths))
        ;;_ (ut/pp [:diffy  diffy-kps tname])
        ;;_ (ut/pp [:diffy2 tname  ])
        table-name  (keyword (str tname  "-history"))
        rows        (vec (for [kp diffy-kps
                               :let [data  (get-in panels kp)
                                     pdata (get-in prev-data kp)
                                     pdiff (deep-diff pdata data)
                                     ;;_ (ut/pp [:diff-in kp data pdata pdiff])
                                     ]]
                           {:kp          (str kp)
                            :client_name (str client-name)
                            :data        (pr-str data)
                            :pre_data    (pr-str pdata)
                            :diff        (pr-str pdiff)
                            :diff_kp     (pr-str (kvpaths pdiff))
                            :panel_key   (str (first kp))
                            :vkey         (str (last kp))
                            :type        (str (second kp))}))
        ;;_ (ut/pp [:diffy3 (count rows) tname])
        ins-sql     {:insert-into [table-name] :values rows}]
    (when (ut/ne? rows)
      (sql-exec systemh2-db (to-sql ins-sql))))
  (swap! comp-atom assoc client-name panels))


;; (ut/pp (keys (get @client-panels :agreeable-long-squirrel-2)))
;; (ut/pp (keys @client-panels))
;; (ut/pp (keys @db/runstream-atom))
;; (ut/pp (get @db/runstream-atom "add-then-multiply"))
;; (ut/pp (get @db/atoms-and-watchers :independent-rectangular-sea-urchin-27))
;; (ut/pp (get @db/runstream-atom "add-then-multiply"))
;; (ut/pp (db/clover-lookup :independent-rectangular-sea-urchin-27 :flow/a-random-wow>unpack-results-map>poster ))
;; (ut/pp (db/parse-coded-keypath :flow/a-random-wow>unpack-results-map>poster ))
;; (ut/pp (db/create-coded-keypath :flow ["a-random-wow" :unpack-results-map :poster]))

;; (ut/pp [:db/drag-body-map @db/drag-body-map])


(declare pre-bake-the-leaves client-mutate)

(defmethod wl/handle-push :delete-panel
  [{:keys [panel-key client-name]}]
  (swap! db/client-panels ut/dissoc-in [client-name panel-key])
  (swap! db/client-panels-metadata ut/dissoc-in [client-name panel-key])

  (let [dbody-keys (filterv #(not= panel-key (first %)) (keys (get @db/drag-body-map client-name)))]
    (ut/pp [:removing panel-key client-name ])
    (swap! db/drag-body-map assoc client-name (select-keys (get @db/drag-body-map client-name) dbody-keys)))

  (let [panels (get @db/client-panels client-name {})
        scrub-fn (fn [panels-map] ;(ut/deep-sort
                                   (ut/deep-remove-keys-and-underscore panels-map [:h :w :root])
                                  ; )
                   )
        old-panels-scrubbed (scrub-fn panels)]
    (swap! db/leaf-brute-force-last-panel-hash assoc client-name (hash old-panels-scrubbed))) ;; reset hash key
  ;; remove from atoms
  ;; remove drag bodies
)

(defmethod wl/handle-push :current-panels
  [{:keys [panels client-name resolved-panels materialized-panels drag-body-map everything?]}]
  ;; (ut/pp ["🍞" :panels-push! client-name (count (keys panels)) :panels :is-everything? everything?])
  (let [panels (dissoc panels nil)
        panel-count (count (keys panels))
        breads (apply str (repeat panel-count "🍞"))]

    (ut/pp ["🍞" :panels-push! breads client-name panel-count :panels])

    (if (or (> panel-count 0) everything?)
      (let [;panels-size-kb (ut/estimate-size-kb panels)
         ; _ (ut/pp [:panels-push! client-name (count (keys panels)) :panels :is-everything? everything?])
          ;panels-hash (hash (ut/deep-remove-keys (merge (get @db/client-panels client-name) panels) [:_last-run]))
            _ (swap! db/drag-body-map assoc client-name (merge (get @db/drag-body-map client-name) drag-body-map))
            old-panels (get @db/client-panels client-name {})
            panels (merge old-panels panels)
            scrub-fn  (fn [panels-map] ;(ut/deep-sort
                                        (ut/deep-remove-keys-and-underscore panels-map [:h :w :root])
                                        ;)
                        )
            old-panels-scrubbed (scrub-fn panels)
            panels-hash (hash old-panels-scrubbed)
            _ (swap! db/leaf-brute-force-last-panel-hash assoc client-name panels-hash)
            changed-for-reval? (not= old-panels-scrubbed (scrub-fn panels))
            ;;_ (ut/pp [:panels-hash-changed? changed-for-reval? client-name])

            panels (ut/deep-remove-keys panels [:_last-run]) ;; so they dont show up in diffs  for hist, etc

          ;; panels (if everything? panels
          ;;            (merge (get @db/client-panels client-name) panels))
            ]

      ;; (ut/pretty-spit "/home/ryanr/spit1.edn" (scrub-fn old-panels))
      ;; (ut/pretty-spit "/home/ryanr/spit2.edn" (scrub-fn panels))


        ;; (qp/serial-slot-queue :panel-update-serial :serial ;;; last known good configuration 11/11/24
        ;;                       (fn [] (ext/write-panels client-name panels))) ;; push to file system for beholder cascades

  ;; (qp/serial-slot-queue :panel-update-serial :serial
  ;;  (fn [] (ext/write-panels client-name panels))) ;; push to file system for beholder cascades

    ;; (doseq [p (keys panels)]
    ;;   (swap! client-panels-history assoc-in [client-name p (System/currentTimeMillis)] {:source (get panels p)
    ;;                                                                                     :resolved (get resolved-panels p)
    ;;                                                                                     :materialized (get materialized-panels p)}))

        (swap! db/client-panels assoc client-name panels)

    ;; (when (ut/ne? resolved-panels)
    ;;   (swap! client-panels-resolved assoc client-name resolved-panels))
    ;; (swap! client-panels-materialized assoc client-name materialized-panels)

        (swap! db/panels-atom assoc client-name resolved-panels)

        ;; (ppy/execute-in-thread-pools
        ;;  :leaf-pre-bakes
        ;;  (pre-bake-the-leaves panels-hash client-name))

      ;; (ut/pp [:panels-changed-enough-for-reval? changed-for-reval?])
      ;(qp/serial-slot-queue (keyword (str "leaf-brute-" (cstr/replace (str client-name) ":" "") "-serial")) :serial
      ;; (when (and false changed-for-reval?)
      ;;   (ppy/execute-in-thread-pools (keyword (str "leaf-brute-serial-" (cstr/replace (str client-name) ":" "") ""))
      ;;                                (fn []
      ;;                                ;(swap! db/leaf-brute-force-map dissoc client-name)
      ;;                                  (swap! db/leaf-atom ut/dissoc-in [client-name :actions]) ;; single use "realtime" results
      ;;                                  (swap! db/leaf-atom assoc-in [client-name :all-actions] {[:running] []}) ;; pre-baked dump
      ;;                                ;(client-mutate client-name {[:baked-leaves] {}})
      ;;                                ;(leaves/generate-all-drag-possibles-pmap! panels-hash client-name)
      ;;                                  (pre-bake-the-leaves panels-hash client-name))))

        (qp/serial-slot-queue :panel-update-serial :serial
                              (fn [] (push-to-history-db client-name panels last-panels "panel")))

      ;; (when (> (count (keys panels)) 0) ;; partial cache warm up for hot drags

      ;;   ;; (leaves/leaf-eval-runstream-parallel :fields client-name)
      ;;   ;; (leaves/leaf-eval-runstream-parallel :views  client-name)
      ;;   ;(leaves/leaf-eval-runstream-parallel-cached client-name)
      ;;   ;; (leaves/leaf-eval-runstream3 client-name [:canvas :canvas :canvas])
      ;;   ;; (swap! db/leaf-atom assoc client-name (leaves/leaf-eval-runstream3 client-name
      ;;   ;;                                                                    (get @db/leaf-drags-atom client-name [:canvas :canvas :canvas])))

      ;;   ;; (qp/serial-slot-queue :leaf-brute-serial client-name
      ;;   ;;                       (fn [] (leaves/generate-all-drag-possibles-pmap! client-name)))
      ;;   )

    ;; (push-to-history-db client-name panels last-panels "panel")
        )
      (ut/pp ["🍞" :zero-panels-changed :doing-nothing breads client-name panel-count :panels :is-everything? everything?]))))

(defn run-shell-command
  "execute a generic shell command and return output as a map of timing and seq of str lines"
  [command]
  (let [;;shell                    (or (System/getenv "SHELL") "/bin/sh")
        ;;output                   (shell/sh shell "-c" (str "mkdir -p shell-root ; cd shell-root ; " command))
        output                   (shell/sh "/bin/bash" "-c" (str "mkdir -p shell-root ; cd shell-root ; " command))
        split-lines              (vec (remove empty? (cstr/split-lines (get output :out))))
        exit-code                (get output :exit)
        error                    (vec (remove empty? (cstr/split-lines (get output :err))))
        has-timing?              (if (ut/ne? error) (cstr/starts-with? (get error 0) "real") false)
        error-data               (if has-timing? [] error)
        timing-values-to-seconds #(let [split-timing (cstr/split (get (cstr/split % #"\t") 1) #"m")
                                        minutes      (edn/read-string (get split-timing 0))
                                        seconds      (edn/read-string (cstr/join "" (drop-last (get split-timing 1))))]
                                    (+ (* 60 minutes) seconds))
        timing-data              (if has-timing? (into [] (for [x error] (timing-values-to-seconds x))) [])]
    {:output     split-lines
     :exception  error-data
     :seconds    timing-data
     :exit-code  exit-code
     :command    (str command)}))

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
  "write local file with given data (forked fabric version)"
  [full-path file-data & [append?]]
  (let [fqd?      (or (cstr/starts-with? full-path "/") (cstr/starts-with? full-path "~"))
        output    (run-shell-command "pwd")
        pwd       (first (get-in output [:output] []))
        full-path (if fqd? full-path (str pwd "/" full-path))]
    (ut/pp [:writing-file full-path])
    (do (try (spit full-path file-data :append (true? append?))
             (catch Exception e
               (do (println "err")
                   {;:file-data file-data
                    :status    :error
                    :file-path full-path
                    :error     (str "caught exception: " (.getMessage e))})))
        {:status :ok :file-path full-path})))

(declare get-fabric-patterns)
(declare get-fabric-models)

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

(defn package-settings-for-client [& [client-name]]
  (let [settings (config/settings)
        valid-runners-map (ppy/execute-in-thread-pools-but-deliver
                           :serial-runner-when-expr-checks
                           (fn [] (into {}
                                        (for [[k v] (get settings :runners)
                                              :let [oo (evl/repl-eval (get v :when-expr true)
                                                                      "127.0.0.1" 8181
                                                                      :rvbbit (keyword (str "config-when-runner" (cstr/replace (str k) ":" ""))) [])]]
                                          {k (get-in oo [:evald-result :value 0] (get-in oo [:evald-result :out]))}))))
        valid-runners (vec (keys (filter #(last %) valid-runners-map)))
        ;; _ (when true ;(= client-name :rvbbit)
        ;;     (ut/pp [valid-runners-map valid-runners]))
        settings (assoc settings :runners (select-keys (get settings :runners) valid-runners))
        settings (merge (ut/deep-remove-keys settings [:when-fn :kit-expr :when-expr])
                        {:clover-templates (edn/read-string (slurp "./defs/clover-templates.edn"))
                         :kits    {} ;;config/kit-fns
                         :screens (vec (map :screen_name
                                            (sql-query system-db
                                                       (to-sql {:select   [:screen_name]
                                                                :from     [[:screens :jj24a7a]]
                                                                :group-by [:screen_name]
                                                                :order-by [[1 :asc]]}))))})]
    (reset! db/latest-settings-map settings)
    settings))

(defmethod wl/handle-request :get-settings
  [{:keys [client-name]}]
  (ut/pp [:client client-name :just-booted])
  (package-settings-for-client client-name))

(defmethod wl/handle-request :autocomplete
  [{:keys [client-name surrounding panel-key view]}]
  ;;(ut/pp [:get-smart-autocomplete-vector client-name panel-key view {:context surrounding}])
  {:clover-params (conj @autocomplete-clover-param-atom :*)
   :block-names (vec @db/unique-block-set)
   :view-names (vec @db/unique-view-set)
   :view-keywords (conj @autocomplete-view-atom :*)})

(def cowabunga-dupes (atom #{}))

(defn client-mutate [client-name assoc-map & [always?]]
  ;; assoc map is {[:full :client :keypath] {body}}
  ;(ut/pp [:client-mutations-sent ["🐢🐢🐢" "🐀" "🍕"] client-name (vec(keys assoc-map)) :keys])
  (let [cow-vec [client-name (hash assoc-map)]]
    (if (and (@cowabunga-dupes cow-vec) (not always?))
      ;(ut/pp [:client-mutations-skipped ["🐢🐢🐢" "🐀" "🍕"] client-name (vec (keys assoc-map)) :we-done-it])
      nil
      (do (swap! cowabunga-dupes conj cow-vec)
          ;(ut/pp [(if always? :client-mutations-sent-always! :client-mutations-sent) ["🐢🐢🐢" "🐀" "🍕"] client-name (vec (keys assoc-map)) :keys])
          (push-to-client [] [] client-name 1 :push-assocs assoc-map)))))

 ;;:client :superb-Prussian-blue-gnu-36 [:block-10889 :queries :all-offenses :field :DISTRICT]]
;;  (ut/pp (select-keys
;;          (get-in @db/leaf-brute-force-map [:superb-Prussian-blue-gnu-36 [:block-10889 :queries :all-offenses :field :DISTRICT]])
;;          [:actions :metadata])
;; )

 ;;:client :superb-Prussian-blue-gnu-36 [:block-10889 :queries :all-offenses :field :DISTRICT]]
;;  (ut/pp (select-keys
;;          (leaves/leaf-eval-runstream3 :superb-Prussian-blue-gnu-36 [:block-10889 :queries :all-offenses :field :DISTRICT])
;;          [:actions :metadata])
;; )

;; (defn pre-bake-the-leaves [panel-hash client-name]
;;   (let [start-time (System/nanoTime)
;;         updates (atom 0)
;;         ;; all-keypaths (conj (distinct (into (leaves/field-keypaths client-name)
;;         ;;                                    (leaves/view-keypaths client-name))) [:canvas :canvas :canvas])
;;         all-keypaths (into (sorted-set)  ; using sorted-set for deterministic ordering and deduplication
;;                            (comp (mapcat identity)  ; flatten the results
;;                                  (map identity))    ; ensure we can append canvas
;;                            (concat [(leaves/field-keypaths client-name)
;;                                     (leaves/view-keypaths client-name)
;;                                     [[:canvas :canvas :canvas]]])) ; add canvas as sequence
;;         ;; chunk-size (max 1 (quot (count all-keypaths) (* 2 (.. Runtime getRuntime availableProcessors))))
;;         _ (ut/pp ["🍁" :started :generate-all-drag-possibles-pmap! client-name (count all-keypaths) :keypaths ])
;;         results (->> all-keypaths
;;                      (pmap (fn [sel]
;;                              (let [resss (leaves/leaf-eval-runstream3 client-name sel)
;;                                    last-actions (get-in @db/leaf-brute-force-map [client-name sel])]

;;                                ;(qp/serial-slot-queue (keyword (str "leaf-push-" (cstr/replace (str client-name) ":" "") "-serial")) :serial
;;                                (when (not= resss last-actions)
;;                                  (ppy/execute-in-thread-pools (keyword (str "leaf-bake-" (cstr/replace (str client-name) ":" "") ""))
;;                                                               (fn []
;;                                                                 (swap! updates inc)
;;                                                                 (client-mutate client-name {[:baked-leaves sel] (get resss :actions)})
;;                                                                 (swap! db/leaf-brute-force-map assoc-in [client-name sel] resss))))

;;                                {sel (get resss :actions)})))
;;                      (apply merge))
;;         end-time (System/nanoTime)
;;         execution-time (/ (- end-time start-time) 1e9)]
;;     (swap! db/leaf-atom assoc-in [client-name :all-actions] results)
;;     (ut/pp ["🍁" :finished :generate-all-drag-possibles-pmap! client-name panel-hash :updated @updates
;;             :execution-time (str execution-time " seconds")])))


;; (defn pre-bake-the-leaves [panel-hash client-name]
;;   (let [start-time (System/nanoTime)
;;         updates (atom 0)
;;         parallel-batch-size 10  ;  best?
;;         all-keypaths (into (sorted-set)
;;                           (concat (leaves/field-keypaths client-name)
;;                                  (leaves/view-keypaths client-name)
;;                                  [[:canvas :canvas :canvas]]))

;;         _ (ut/pp ["🍁" :started :generate-all-drag-possibles-pmap! client-name (count all-keypaths) :keypaths])
;;         ;; Process in smaller parallel batches but send updates immediately
;;         _ (->> all-keypaths
;;                (partition-all parallel-batch-size)
;;                (run! (fn [keypath-batch]
;;                       ;; eval batch
;;                       (let [results
;;                             (->> keypath-batch
;;                                  (pmap (fn [sel]
;;                                         (let [resss (leaves/leaf-eval-runstream3 client-name sel)
;;                                               last-actions (get-in @db/leaf-brute-force-map [client-name sel])]
;;                                           (when (not= resss last-actions)
;;                                             {:sel sel
;;                                              :actions (get resss :actions)
;;                                              :full resss}))))
;;                                  (remove nil?))]
;;                         ;; send res as they come
;;                         (doseq [result results]
;;                           (swap! updates inc)
;;                           (swap! db/leaf-evals inc)
;;                           (ppy/execute-in-thread-pools
;;                            (keyword (str "leaf-bake-" (cstr/replace (str client-name) ":" "") ""))
;;                            (fn []
;;                              (client-mutate
;;                               client-name
;;                               {[:baked-leaves (:sel result)] (:actions result)})

;;                              ;; update tracking map for comps
;;                              (swap! db/leaf-brute-force-map
;;                                    assoc-in
;;                                    [client-name (:sel result)]
;;                                    (:full result))))))))
;;                (doall))

;;         end-time (System/nanoTime)
;;         execution-time (/ (- end-time start-time) 1e9)]

;;     (ut/pp ["🍁" :finished :generate-all-drag-possibles-pmap! client-name panel-hash
;;             :updated @updates
;;             :execution-time (str execution-time " seconds")])))




;; (defn pre-bake-the-leaves [panel-hash client-name]
;;   (let [start-time (System/nanoTime)
;;         updates (atom 0)
;;         all-keypaths (conj (distinct (into (leaves/field-keypaths client-name)
;;                                            (leaves/view-keypaths client-name))) [:canvas :canvas :canvas])
;;         _ (ut/pp ["🍁" :started :generate-all-drag-possibles-pmap! client-name (count all-keypaths) :keypaths])
;;         results (->> all-keypaths
;;                      (pmap (fn [sel]
;;                              (let [resss (leaves/leaf-eval-runstream3 client-name sel)]
;;                                {sel (get resss :actions)})))
;;                      (apply merge))
;;         end-time (System/nanoTime)
;;         execution-time (/ (- end-time start-time) 1e9)]
;;     (swap! db/leaf-atom assoc-in [client-name :all-actions] results)
;;     (ut/pp ["🍁" :finished :generate-all-drag-possibles-pmap! client-name panel-hash :updated @updates
;;             :execution-time (str execution-time " seconds")])))





;; (defn pre-bake-the-leaves [panel-hash client-name]
;;   (let [start-time (System/nanoTime)
;;         updates (atom 0)
;;         batch-size 50  ; Adjust based on your system's capabilities

;;         ;; Get unique keypaths once
;;         all-keypaths (vec
;;                       (conj (distinct
;;                              (into (leaves/field-keypaths client-name)
;;                                   (leaves/view-keypaths client-name)))
;;                            [:canvas :canvas :canvas]))

;;         _ (ut/pp ["🍁" :started :generate-all-drag-possibles-pmap! client-name (count all-keypaths) :keypaths])

;;         ;; Process in batches
;;         results
;;         (->> all-keypaths
;;              (partition-all batch-size)
;;              (pmap (fn [keypath-batch]
;;                     ;; Process each batch in parallel
;;                     (->> keypath-batch
;;                          (pmap (fn [sel]
;;                                 (let [resss (leaves/leaf-eval-runstream3 client-name sel)
;;                                       last-actions (get-in @db/leaf-brute-force-map [client-name sel])]

;;                                   ;; Collect updates for batch processing
;;                                   (when (not= resss last-actions)
;;                                     (swap! updates inc)
;;                                     ;; Return update data instead of executing immediately
;;                                     {:sel sel
;;                                      :actions (get resss :actions)
;;                                      :full-result resss})
;;                                 ;; Always return the sel->actions mapping for final results
;;                                 {sel (get resss :actions)})))
;;                          ;; Combine batch results
;;                          (apply merge))))
;;              ;; Combine all batch results
;;              (apply merge))

;;         ;; Process all updates in a single batch
;;         _ (let [update-batch (->> all-keypaths
;;                                  (keep (fn [sel]
;;                                         (let [resss (get-in results [sel])
;;                                               last-actions (get-in @db/leaf-brute-force-map [client-name sel])]
;;                                           (when (not= resss last-actions)
;;                                             {:sel sel :actions resss}))))
;;                                  (vec))]

;;             ;; Only create thread pool task if we have updates
;;             (when (seq update-batch)
;;               (ppy/execute-in-thread-pools
;;                (keyword (str "leaf-bake-" (cstr/replace (str client-name) ":" "") ""))
;;                (fn []
;;                  ;; Batch update client mutations
;;                  (client-mutate
;;                   client-name
;;                   (into {}
;;                         (map (fn [{:keys [sel actions]}]
;;                                [[:baked-leaves sel] actions])
;;                              update-batch)))

;;                  ;; Batch update leaf-brute-force-map
;;                  (swap! db/leaf-brute-force-map
;;                         (fn [m]
;;                           (reduce (fn [acc {:keys [sel actions]}]
;;                                   (assoc-in acc [client-name sel] actions))
;;                                 m
;;                                 update-batch)))))))

;;         end-time (System/nanoTime)
;;         execution-time (/ (- end-time start-time) 1e9)]

;;     ;; Single atomic update to leaf-atom
;;     (swap! db/leaf-atom assoc-in [client-name :all-actions] results)

;;     (ut/pp ["🍁" :finished :generate-all-drag-possibles-pmap! client-name panel-hash
;;             :updated @updates
;;             :execution-time (str execution-time " seconds")])))

(defmethod wl/handle-push :give-viz-recos-for [{:keys [client-name panel-key data-key existing]}]
  (let [;;recos-for (get-in @db/shapes-result-map [client-name panel-key data-key :shapes])
        dd (d/get-value db/ddb "shapes-map" (hash [client-name panel-key data-key]))
        recos-for (get
                   ;;(db/ddb-get "honeyhash-map" (hash [client-name panel-key data-key]))
                   dd
                   :shapes)
        recos-for-grouped (group-by (fn [m] (get-in m [:shape-rotator :shape-name])) recos-for)
        one-of-each (mapv (fn [[_ v]] (first v)) recos-for-grouped)
        count-found (count recos-for)]
    (ut/pp ["🦐" :give-viz-recos-for :client client-name panel-key data-key :had existing :found count-found (count one-of-each)])
    (when (not= count-found existing)
      (client-mutate client-name {[:shapes panel-key data-key] one-of-each}))))



(def leaf-eval-cache (atom {}))

;; (defn leaf-eval-cached [client-name dragged-kp panels-hash & [warm?]]
;;   (let [kp-target [:click-param :leaf (keyword (str (cstr/replace (str client-name) ":" "") ">actions"))]
;;         res (if true
;;   ;;  (and ;; is is safe to cache this res, or should we raw dog it for now?
;;   ;;   (get @client-panels-in-sync? client-name) ;; do we have a full copy of the clients panels?
;;   ;;   (ut/ne? (get-in @db/drag-body-map [client-name dragged-kp]))) ;; do we at least have a drag body map for this kp?
;;               (let [cache-key [client-name dragged-kp panels-hash]]
;;                 (if-let [cached-result (get @leaf-eval-cache cache-key)]
;;                   (do (when (not warm?)
;;                         (ut/pp ["💰" :cache-hit-leaf! client-name dragged-kp panels-hash (keys (get cached-result :actions))]))
;;                       cached-result)
;;       ;;  cached-result
;;                   (let [result (select-keys (leaves/leaf-eval-runstream3 client-name dragged-kp) [:actions :metadata])] ;; [:actions :metadata]
;;                     (swap! db/leaf-evals inc)
;;                     (swap! leaf-eval-cache assoc cache-key result)
;;                     result)))
;;               (select-keys (leaves/leaf-eval-runstream3 client-name dragged-kp) [:actions :metadata]))]
;;     (client-mutate client-name {kp-target res}) ;; push also? why not.
;;     res))


(defn leaf-eval-cached [client-name dragged-kp panels-hash & [warm?]] ;; no longer cached
  (let [kp-target [:click-param :leaf (keyword (str (cstr/replace (str client-name) ":" "") ">actions"))]
        res (select-keys (leaves/leaf-eval-runstream3 client-name dragged-kp) [:actions :metadata])
        _ (swap! db/leaf-evals inc)]
    (client-mutate client-name {kp-target (get res :actions)} true) ;; push also? why not.
    res))


;;(keyword (str "leaf/" client-name-str ">actions"))

(defn pre-bake-the-leaves [panel-hash client-name]
  (let [start-time (System/nanoTime)
        ;; Get keypaths efficiently - do this once
        all-keypaths (into (sorted-set)  ; using sorted-set for deterministic ordering and deduplication
                           (comp (mapcat identity)  ; flatten the results
                                 (map identity))    ; ensure we can append canvas
                           (concat [(leaves/field-keypaths client-name)
                                    (leaves/view-keypaths client-name)
                                    [[:canvas :canvas :canvas]]])) ; add canvas as sequence
        kp-count (count all-keypaths)
        _ (ut/pp ["🍁" :started :generate-all-drag-possibles-pmap! client-name kp-count :keypaths])
        _ (doseq [kp all-keypaths] (leaf-eval-cached client-name kp panel-hash))
        end-time (System/nanoTime)
        execution-time (/ (- end-time start-time) 1e9)]
    (ut/pp ["🍁" :end :generate-all-drag-possibles-pmap! client-name kp-count :keypaths :in execution-time :seconds])))

(defn leaf-eval-cache-warmer [client-name panels-hash work-targets]
  (let [parallel-batch-size 10]
    (->> work-targets
         (partition-all parallel-batch-size)
         (run! (fn [batch]
                 (->> batch
                      (pmap (fn [dragged-kp]
                              (leaf-eval-cached client-name dragged-kp panels-hash)))
                      doall)))
         doall)))

;;; (swap! db/shapes-result-map assoc-in [client-name panel-key (first ui-keypath)] res)

(defmethod wl/handle-push :get-shape-rotations
  [{:keys [panel-key source-panel shape-name query-key client-name]}]
  (ut/pp [:req-shape-rotations client-name panel-key :src source-panel shape-name query-key])
  (let [;full-recos (get-in @db/shapes-result-map [client-name source-panel query-key :shapes]) ;; :fields
                ;;dd (db/ddb-get "honeyhash-map" (hash [client-name panel-key query-key]))
        key-hash (if (vector? source-panel)
                   (hash source-panel) ;; only for fresh-table spawns that have no block to reference
                   ;; will be something like [:fresh-table! "connection-id" :keyword_sql_table], else it's a normal panel-key keyword from the client
                   (hash [client-name source-panel query-key]))
        dd (d/get-value db/ddb "shapes-map" key-hash)
        full-recos (get dd :shapes)
        ;; _ (ut/pretty-spit (str "/tmp/shape-recos-" key-hash "-panel-key.edn") dd 220)
        ;;_ (ut/pp [:wut key-hash dd])
        _ (ut/pp [:full-recos (count full-recos) [client-name source-panel query-key]])
        all-shapes (vec (distinct (map (fn [m] (get-in m [:shape-rotator :shape-name])) full-recos)))
        shape-recos (filterv #(= (get-in % [:shape-rotator :shape-name]) shape-name) full-recos)
        _ (ut/pp [:req-shape-rotations :shape-recos (count shape-recos) :of (count full-recos)])
        axes-combos (mapv (fn [m] (get-in m [:shape-rotator :axes])) shape-recos)
        axes (keys (first axes-combos))
        options (into {} (for [a axes] {a (vec (distinct (map a axes-combos)))}))
        pkg {:options options
             :recos shape-recos
             :shapes all-shapes}]
    (client-mutate client-name {[:shape-rotations panel-key] pkg} true)))

(defmethod wl/handle-push :warm-leaf-evals
  [{:keys [client-name work-targets drag-bodies]}]
  (let [panels-hash (get @db/leaf-brute-force-last-panel-hash client-name)]
    (swap! db/drag-body-map assoc client-name (merge (get @db/drag-body-map client-name) (or drag-bodies {})))
    ;(ut/pp ["🔥" :warming-leaf-cache-for client-name (first work-targets) (get-in @db/drag-body-map [client-name (first work-targets)])])
    ;(leaf-eval-cache-warmer client-name panels-hash work-targets)
    (leaf-eval-cached client-name (first work-targets) panels-hash true)
    ))

(defmethod wl/handle-push :quickfetch-leaf [{:keys [client-name dragged-kp dragging-body]}]
  (ut/pp ["🍂" :quick-leaf :client client-name dragged-kp])
  (swap! db/leaf-drags-atom assoc client-name dragged-kp)
  (swap! db/drag-body-map assoc-in [client-name dragged-kp] dragging-body))

(defmethod wl/handle-request :quickfetch-leaf [{:keys [client-name dragged-kp dragging-body]}]
  (ut/pp ["🍂" :quick-leaf :client client-name dragged-kp])
  (swap! db/leaf-drags-atom assoc client-name dragged-kp)
  (swap! db/drag-body-map assoc-in [client-name dragged-kp] dragging-body)
  (let [dragged-kp (if (= (first dragged-kp) :canvas) [:canvas :canvas :canvas] dragged-kp)
        ;res (leaves/leaf-eval-runstream-parallel-cached client-name)
        ;{:keys [elapsed-ms result]} (ut/timed-exec (leaves/clean-leaf-pull client-name dragged-kp))
        panels-hash (get @db/leaf-brute-force-last-panel-hash client-name)
        {:keys [elapsed-ms result]} (ut/timed-exec (leaf-eval-cached client-name dragged-kp panels-hash))
        ;;{:keys [elapsed-ms result]} (ut/timed-exec (leaves/leaf-eval-runstream3 client-name dragged-kp))


        ;; {:keys [elapsed-ms result]} (ut/timed-exec (leaves/leaf-eval-runstream-parallel-cached client-name))
        ;; {:keys [elapsed-ms result]} (ut/timed-exec
        ;;                              ;(leaves/leaf-eval-runstream-parallel-cached client-name)
        ;;                              (get-in @db/leaf-brute-force-map [client-name dragged-kp] ;; see if we have a pre-baked answer for the client state
        ;;                                      (leaves/leaf-eval-runstream-parallel-cached client-name)) ;; else do it live
        ;;                              )
        ;; {:keys [elapsed-ms result]} (ut/timed-exec
        ;;                              (get-in @db/leaf-brute-force-map [client-name dragged-kp] ;; see if we have a pre-baked answer for the client state
        ;;                                      (leaves/leaf-eval-runstream3 client-name dragged-kp)) ;; else do it live
        ;;                              )
        ;; res (get-in @db/leaf-brute-force-map [client-name dragged-kp]
        ;;             (leaves/leaf-eval-runstream-parallel-cached client-name))
        ;; res (get-in @db/leaf-brute-force-map [client-name
        ;;                                       ;(get @db/leaf-brute-force-last-panel-hash client-name)
        ;;                                       dragged-kp])
        ;_ (ut/pretty-spit "/home/ryanr/res1.edn" res1)
        ;_ (ut/pretty-spit "/home/ryanr/res.edn" res)
        _ (ut/pp ["🍂🐇" :quickfetch-leaf! elapsed-ms :ms client-name dragged-kp (keys (select-keys result [:actions :metadata])) ])]
    (select-keys result [:actions :metadata])
    ;{:actions (get result :actions)}
    ;;(get result :actions)
    ;;result
    ))

;; (defmethod wl/handle-request :quickfetch-leaf-one [{:keys [client-name dragged-kp target-kp]}]
;;   (let [dragged-kp (if (= (first dragged-kp) :canvas) [:canvas :canvas :canvas] dragged-kp)]
;;     (ut/pp ["🍂" :quick-leaf-one :client client-name dragged-kp target-kp])
;;     (swap! db/leaf-drags-atom assoc client-name dragged-kp)
;;   ;;(leaves/leaf-eval-runstream-parallel-cached client-name)
;;     (leaves/leaf-eval-runstream2 client-name dragged-kp))
;;   )

(defmethod wl/handle-request :run-leaf-action-new [{:keys [client-name src-keypath target-keypath dragging-body dragging-kp leaf-action-cat leaf-action drag?]}]
  (let [leaf-defs (leaves/load-leaf-fns client-name target-keypath dragging-kp)
        {:keys [actions-fns]} leaf-defs
        leaf-fn (get-in actions-fns [leaf-action-cat leaf-action])
        fn-output (leaves/leaf-action-fn-eval client-name src-keypath target-keypath leaf-action-cat leaf-action leaf-fn dragging-body)]
    ;; (ut/pp ["🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂"])
    ;; (ut/pp ["🍂" (if drag? :run-leaf-action-drag-body :run-leaf-action-new) client-name :src src-keypath :tgt target-keypath leaf-action-cat leaf-action])
    ;; (ut/pp ["🍂" :leaf-fn leaf-fn])
    ;; (ut/pp ["🍂" :leaf-out fn-output])
    ;; (ut/pp ["🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂"])
    (if (map? fn-output) fn-output
        (if drag?
          {} ;; empty drag body
          {:h 5 :w 5
           :views {:error [:v-box
                           :padding "10px"
                           :style {:color "red"}
                           :size "auto"
                           :align :center :justify :center
                           :gap "12px"
                           :children [[:box :child [:str "new leaf drop fn should return a valid clover map, this returned: "]]
                                      [:box :child (str fn-output)]
                                      (when (empty? (str fn-output))
                                        [:box :child "(nothing)"])]]}})) ;; should return a valid clover block
      ;; clean up client/server state flags
    ;(swap! db/leaf-drags-atom dissoc client-name) ;; clear the dragging keypath
    ;(swap! db/leaf-atom assoc-in [client-name :actions] {})
    ))

(defmethod wl/handle-push :run-leaf-action
  [{:keys [client-name src-keypath target-keypath dragging-kp leaf-action-cat dragging-body leaf-action]}]
  (let [leaf-defs (leaves/load-leaf-fns client-name target-keypath dragging-kp)
        {:keys [actions-fns]} leaf-defs
        leaf-fn (get-in actions-fns [leaf-action-cat leaf-action])
        fn-output (leaves/leaf-action-fn-eval client-name src-keypath target-keypath leaf-action-cat leaf-action leaf-fn dragging-body)]
    ;; (ut/pp ["🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂"])
    ;; (ut/pp ["🍂" :run-leaf-action client-name :src src-keypath :tgt target-keypath leaf-action-cat leaf-action])
    ;; (ut/pp ["🍂" :leaf-fn leaf-fn])
    ;; (ut/pp ["🍂" :leaf-out fn-output])
    ;; (ut/pp ["🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂"])
    (if (and (ut/ne? fn-output) (map? fn-output))
      (client-mutate client-name fn-output true)
      (ut/pp ["🍂" :leaf-fn-output-error "fn was successful, but does not seem like an assoc-in-map?" fn-output]))
    ;; clean up client/server state flags
    (swap! db/leaf-drags-atom dissoc client-name) ;; clear the dragging keypath
    (swap! db/leaf-atom assoc-in [client-name :actions] {})))


(defmethod wl/handle-request :run-leaf-action-preview
  [{:keys [client-name src-keypath target-keypath leaf-action-cat dragging-kp dragging-body leaf-action]}]
  (let [leaf-defs (leaves/load-leaf-fns client-name target-keypath dragging-kp)
        {:keys [actions-fns]} leaf-defs
        leaf-fn (get-in actions-fns [leaf-action-cat leaf-action])
        fn-output (leaves/leaf-action-fn-eval client-name src-keypath target-keypath leaf-action-cat leaf-action leaf-fn dragging-body)]
    ;; (ut/pp ["🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂"])
    ;; (ut/pp ["🍂" :run-leaf-action-preview client-name :src src-keypath :tgt target-keypath leaf-action-cat leaf-action])
    ;; (ut/pp ["🍂" :leaf-fn leaf-fn])
    ;; (ut/pp ["🍂" :leaf-out fn-output])
    ;; (ut/pp ["🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂🍂"])
    ;; (if (and (ut/ne? fn-output) (map? fn-output))
    ;;   (client-mutate client-name fn-output)
    ;;   (ut/pp ["🍂" :leaf-fn-output-error "fn was successful, but does not seem like an assoc-in-map?" fn-output]))
    ;; clean up client/server state flags
    ;; (swap! db/leaf-drags-atom dissoc client-name) ;; clear the dragging keypath
    ;; (swap! db/leaf-atom assoc-in [client-name :actions] {})
    fn-output))

;; (ut/pp [:dggg @db/leaf-brute-force-map])

(defn push-drag-related-shape-viz  [client-name dragged-kp & [pre-bake?]]
  (let [[panel-key runner data-key _ field-name] dragged-kp
        hash-key (if (string? runner)
                   (hash dragged-kp) ;; [:fresh-table!  "bigfoot-ufos" :some_table_name]
                   (hash [client-name panel-key data-key]))]
    (when (or (= runner :queries) (string? runner)) ;; for fresh-table spawns runner pos is connection-id str
      (when-let [;;done-shapes (get-in @db/shapes-result-map [client-name panel-key data-key])
                 ;;done-shapes (db/ddb-get "honeyhash-map" (hash [client-name panel-key data-key]))
                 done-shapes (d/get-value db/ddb "shapes-map" hash-key)]
        (let [dim? (get (first (filter #(= (get % :field-name) (cstr/replace (str field-name) ":" "")) (get done-shapes :fields))) :dimension? false)
              reco-count (count (get done-shapes :shapes))
              _ (when field-name (ut/pp [:BOO! field-name dim? reco-count]))
              query-id (get dragged-kp 2)
              relevant-shapes (cond
                                (nil? field-name)
                                (filterv #(contains? (set (ut/deep-flatten %)) :rrows) (get done-shapes :shapes))

                                dim?
                                (filterv #(and
                                           (or
                                            (contains? (set (ut/deep-flatten %)) :rrows)
                                            (contains? (set (ut/deep-flatten %)) :count)
                                            (contains? (set (ut/deep-flatten %)) :sum))
                                           (contains? (set (ut/deep-flatten %)) field-name)) (get done-shapes :shapes))
                                :else (filterv #(contains? (set (ut/deep-flatten %)) field-name) (get done-shapes :shapes)))
              _ (when (ut/ne? relevant-shapes)
                  (ppy/execute-in-thread-pools
                   :sql-drag-rotations
                   (fn [] (let [compact-relevant-shapes (vec (apply concat
                                                                    (for [{:keys [shape-rotator] :as full-shape} relevant-shapes]
                                                                      (let [{:keys [context shape-name axes shape-set honey-hash items source-panel]} shape-rotator
                                                                            [connection-id _ _ table-name] context
                                                                            rotation-hash (hash [shape-rotator client-name])]
                                                                        (for [[k v] axes]
                                                                          (mapv str [dragged-kp rotation-hash client-name connection-id
                                                                                     table-name shape-set shape-name k v context source-panel full-shape]))))))
                                reco-count-post (count relevant-shapes)]
                            (sql-exec systemh2-db (to-sql
                                                   {:delete-from [:shape_rotations]
                                                    :where [:and
                                                            [:= :dragged_kp (str dragged-kp)]
                                                            [:= :client_name (str client-name)]]}))
                            (sql-exec systemh2-db (to-sql
                                                   {:delete-from [:shape_rotation_maps]
                                                    :where [:and
                                                            [:= :dragged_kp (str dragged-kp)]
                                                            [:= :client_name (str client-name)]]}))
                            (sql-exec systemh2-db (to-sql
                                                   {:insert-into [:shape_rotations]
                                                    :columns [:dragged_kp :rotation_hash :client_name :connection_id :table_name
                                                              :shape_set :shape_name :axis_name :field_name :context :source_panel]
                                                    :values (mapv drop-last compact-relevant-shapes)}))
                            (sql-exec systemh2-db (to-sql
                                                   {:insert-into [:shape_rotation_maps]
                                                    :columns [:dragged_kp :rotation_hash :client_name :shape_map]
                                                    :values (vec (distinct (map (fn [v] [(get v 0) (get v 1) (get v 2) (last v)]) compact-relevant-shapes)))}))
                            (when pre-bake?
                              (push-to-client [query-id] [:reco-status query-id] client-name 1 :reco :done reco-count-post 0))
                            #_(ut/pp [(if pre-bake? "🍖🥧" "🍖") :sql-shapes-inserted! [client-name dragged-kp] reco-count-post (count compact-relevant-shapes)])))))

              #_(ut/pp [:take-5-compact-relevant-shapes (take 5 compact-relevant-shapes)])
              recos-for-grouped (group-by (fn [m] (get-in m [:shape-rotator :shape-name])) relevant-shapes)
              #_ (ut/pp ["🧀" :shape-rotator-DRAG field-name :honey-hash-cached-loaded client-name data-key (count relevant-shapes) :/ reco-count :viz-cnt (count recos-for-grouped)])
              one-of-each (mapv (fn [[_ v]] (first v)) recos-for-grouped)]
          (client-mutate client-name {[:shapes dragged-kp] one-of-each} true))))))

(defmethod wl/handle-push :client-ui
  [{:keys [client-name atom-name value dragged-kp dragging-body]}]
  (ut/pp  ["👀" :client-ui-drag-in atom-name client-name dragging-body])
  (swap! db/drag-body-map assoc-in [client-name dragged-kp] dragging-body)
  (swap! db/params-atom assoc-in [client-name (keyword atom-name)] value)
  (let [panels-hash (get @db/leaf-brute-force-last-panel-hash client-name)]
    (swap! db/leaf-atom assoc client-name
           (select-keys (leaves/leaf-eval-runstream3 client-name dragged-kp dragging-body) [:actions :metadata])
           ;;(leaf-eval-cached client-name dragged-kp panels-hash)
           ))


  (ppy/execute-in-thread-pools ;; send one of each recos to the client
   (keyword (cstr/replace (str "shape-rotator-send-blocks-serial-" client-name) ":" ""))
   (fn [] (push-drag-related-shape-viz client-name dragged-kp)))

  ;; (swap! db/leaf-atom assoc client-name
  ;;        (get-in @db/leaf-brute-force-map [client-name dragged-kp] ;; see if we have a pre-baked answer for the client state
  ;;                (leaves/leaf-eval-runstream3 client-name dragged-kp)))
  ;(ut/pp ["🍂🍂🍂" client-name :dragging :re-evaluated-leaves!])
  )

;; (ut/pp [:stuff (get-in @db/shapes-result-map [:celebrated-pyramidal-pronghorn-15 :block-10235 :all-ufo-sightings-1 :fields])])


;; (defn get-shape-rotation-field-meta [client-name dragged-kp]
;;   (let [[panel-key _ query-key _ field] dragged-kp
;;         str-field (cstr/replace (str field) ":" "")
;;         kp [client-name panel-key query-key :fields]]
;;     (or (some-> (filter #(= (get % :field-name) str-field)
;;                         (get-in @db/shapes-result-map kp))
;;                 first)
;;         {})))


(def shape-rotation-meta-cache (atom {}))
;; (reset! shape-rotation-meta-cache {})

;; (defn get-shape-rotation-field-meta [client-name dragged-kp]
;;   (let [cache-key [client-name dragged-kp]]
;;     (if-let [cached-result nil] ;(get @shape-rotation-meta-cache cache-key)]
;;       cached-result
;;       (let [[panel-key _ query-key _ field] dragged-kp
;;             str-field (cstr/replace (str field) ":" "")
;;             kp [client-name panel-key query-key :fields]
;;             result (or (some-> (filter #(= (get % :field-name) str-field)
;;                                        (get-in @db/shapes-result-map kp))
;;                                first)
;;                        {})]
;;         (swap! shape-rotation-meta-cache assoc cache-key result)
;;         result))))

(defn get-shape-rotation-field-meta [client-name dragged-kp]
  (let [cache-key [client-name dragged-kp]]
    (if-let [cached-result (get @shape-rotation-meta-cache cache-key)]
      cached-result
      (let [[panel-key _ query-key _ field] dragged-kp
            str-field (name field)
            ;kp [client-name panel-key query-key :fields]
            dd (d/get-value db/ddb "shapes-map" (hash [client-name panel-key query-key]))
            data (get
                  ;;(db/ddb-get "honeyhash-map" (hash [client-name panel-key query-key]))
                  dd
                  :fields)
            result (or (first (get (group-by :field-name
                                             ;(get-in @db/shapes-result-map kp)
                                             data
                                             ) str-field))
                       {})]
        (swap! shape-rotation-meta-cache assoc cache-key result)
        result))))

;; (time (get-shape-rotation-field-meta :celebrated-pyramidal-pronghorn-15 [:block-10235 :queries :all-ufo-sightings-1 :field :country]))
;; (time (get-shape-rotation-field-meta2 :celebrated-pyramidal-pronghorn-15 [:block-10235 :queries :all-ufo-sightings-1 :field :country]))


(defmethod wl/handle-push :leaf-push
  [{:keys [client-name dragged-kp dragging-body]}]
  ;; (ut/pp  ["👀🍂" :leaf-push client-name dragged-kp dragging-body])
  (swap! db/leaf-evals inc)
  (swap! db/drag-body-map assoc-in [client-name dragged-kp] dragging-body)
  (swap! db/leaf-drags-atom assoc client-name dragged-kp)

  (let [;;_ (ut/pp [:test! client-name dragged-kp])
        field-map (if (> (count dragged-kp) 3) ;; only for field drags
                    (get-shape-rotation-field-meta client-name dragged-kp) {})
        {:keys [result elapsed-ms]} (ut/timed-exec (leaves/leaf-eval-runstream3 client-name dragged-kp dragging-body field-map))
        client-str (cstr/replace (str client-name) ":" "")
        kp-targeta [:click-param :leaf (keyword (str client-str ">actions"))]
        kp-targetm [:click-param :leaf (keyword (str client-str ">metadata"))]
        meta (get result :metadata) ;;(select-keys (get res :metadata) [:action-labels :categories])
        actions (get result :actions)]
    (client-mutate client-name {kp-targeta actions
                                kp-targetm meta} true)
    (swap! db/leaf-atom assoc client-name {:actions actions :metadata meta})
    ;; (ut/pp ["👀🍂" :leaf-push-output (get res :actions)])
    (ut/pp ["👀🍂" :leaf-push elapsed-ms dragged-kp client-name])
    )

  (ppy/execute-in-thread-pools ;; send one of each recos to the client
   (keyword (cstr/replace (str "shape-rotator-send-blocks-serial-" client-name) ":" ""))
   (fn [] (push-drag-related-shape-viz client-name dragged-kp))))


;; (defmethod wl/handle-push :client-ui2
;;   [{:keys [client-name atom-name value dragged-kp]}]
;;   (swap! db/params-atom assoc-in [client-name (keyword atom-name)] value)

;;   (ut/pp [:client-ui-atom! client-name atom-name value])

;;   (when (= (str atom-name) "dragging-body")
;;     (try
;;       (let [;;field-drag? (= (get-in value [:drag-meta :type]) :field)
;;             ;; drag-leaf-kp (when (ut/ne? value)
;;             ;;                (if field-drag?
;;             ;;                  [(get value :source-panel) :queries (get-in value [:drag-meta :source-query]) :field (get-in value [:drag-meta :target])]
;;             ;;                  [(get value :source-panel) :queries (get-in value [:drag-meta :source-query])]))
;;             ;; drag-leaf-kp dragged-kp
;;             dragged-kp (if (= (first dragged-kp) :canvas) [:canvas :canvas :canvas] dragged-kp)
;;             _ (ut/pp [:drag-leaf-kp dragged-kp])

;;             _ (if ;;(empty? value)
;;                   (or (boolean? value) (and (map? value) (empty? value))) ;(try (empty? value) (catch Exception _ true)) ;; bool or dragging-body map
;;               ;; add it so it can be treated specially in leaf eval
;;                 (do
;;                   ;(swap! db/leaf-drags-atom dissoc client-name) ;; clear the dragging keypath
;;                   ;(swap! db/leaf-atom assoc-in [client-name :actions] nil) ;; clear the actions map
;;                   (ut/pp [:client client-name :NOT-dragging :re-evaluating-leaves!])

;;                   ;(leaves/leaf-eval-runstream-parallel-cached client-name [:canvas :canvas :canvas])
;;                   (swap! db/leaf-atom assoc client-name (leaves/leaf-eval-runstream3 client-name dragged-kp)) ;[:canvas :canvas :canvas]))
;;                   (ut/pp [:client client-name :right-click :re-evaluated-leaves!])

;;                 ;; (let [rt :p ;(rand-nth [:p :s])
;;                 ;;       f-run (get (ut/timed-exec ((if (= rt :p) leaves/leaf-eval-runstream-parallel leaves/leaf-eval-runstream) :fields client-name [:canvas :canvas :canvas])) :elapsed-ms)
;;                 ;;       v-run (get (ut/timed-exec ((if (= rt :p) leaves/leaf-eval-runstream-parallel leaves/leaf-eval-runstream) :views client-name [:canvas :canvas :canvas])) :elapsed-ms)]
;;                 ;;   (swap! db/leaf-bench assoc-in [client-name (if (= rt :p) :parallel :serial) :fields] (conj (get-in @db/leaf-bench [client-name (if (= rt :p) :parallel :serial) :fields] []) f-run))
;;                 ;;   (swap! db/leaf-bench assoc-in [client-name (if (= rt :p) :parallel :serial)  :views] (conj (get-in @db/leaf-bench [client-name (if (= rt :p) :parallel :serial)  :views] []) v-run))
;;                 ;;   (ut/pp ["🍂🍂🍂" :leaves-evald :zero-event client-name {:fields-in [f-run :ms] :views-in [v-run :ms]}]))
;;                   )

;;                 (do (swap! db/leaf-drags-atom assoc client-name dragged-kp) ;; add the dragging keypath
;;                   ;; (swap! db/leaf-atom assoc-in [client-name :actions] ;; insert value from pre-materialized results, if they are out of date, the per-kp run below will quickly overwrite...
;;                   ;;        (get-in @db/leaf-atom [client-name :all-actions drag-leaf-kp])) ;; TODO, this needs to be fast, if wss latency is a problem we need a workaround

;;                    ; (ut/pp [:client client-name :dragging :re-evaluating-leaves!])
;;                    ; (leaves/leaf-eval-runstream-parallel-cached client-name)
;;                    ;(leaves/leaf-eval-runstream3 client-name dragged-kp)
;;                     (swap! db/leaf-atom assoc client-name (leaves/leaf-eval-runstream3 client-name dragged-kp))
;;                     (ut/pp [:client client-name :dragging :re-evaluated-leaves!])

;;                   ;; (let [rt :p ;(rand-nth [:p :s])
;;                   ;;       f-run (get (ut/timed-exec ((if (= rt :p) leaves/leaf-eval-runstream-parallel leaves/leaf-eval-runstream) :fields client-name)) :elapsed-ms)
;;                   ;;       v-run (get (ut/timed-exec ((if (= rt :p) leaves/leaf-eval-runstream-parallel leaves/leaf-eval-runstream) :views client-name)) :elapsed-ms)]
;;                   ;;   (swap! db/leaf-bench assoc-in [client-name (if (= rt :p) :parallel :serial) :fields] (conj (get-in @db/leaf-bench [client-name (if (= rt :p) :parallel :serial) :fields] []) f-run))
;;                   ;;   (swap! db/leaf-bench assoc-in [client-name (if (= rt :p) :parallel :serial)  :views] (conj (get-in @db/leaf-bench [client-name (if (= rt :p) :parallel :serial)  :views] []) v-run))
;;                   ;;   (ut/pp ["🍂🍂🍂" :leaves-evald :drag-event client-name {:fields-in [f-run :ms] :views-in [v-run :ms]}]))
;;                     )) ;; re-evaluate views - to build a new actions map for the client

;;           ;; _ (do (leaves/leaf-eval-runstream-parallel :fields client-name)
;;           ;;       (leaves/leaf-eval-runstream-parallel :views client-name))
;;             ]
;;       )
;;       (catch Exception e (do (ut/pp [:error-dragging-body-ship (str e) (str atom-name) (str value)]) {}))))
;;   ;; (ut/pp [:client-ui-atom client-name atom-name value])
;;   )

;;;; {:keys [result elapsed-ms]} (ut/timed-exec

(defn client-statuses []
  (into {}
        (for [[k v] @db/ack-scoreboard]
          {k (let [seconds-ago (int (/ (- (System/currentTimeMillis) (get-in v [:last-ack 0] 0)) 1000))
                   never?      (nil? (get-in v [:last-ack 0]))
                   last-seconds (if never? -1 seconds-ago)
                   _ (swap! db/ack-scoreboard assoc-in [k :last-seen-seconds-ago] last-seconds)]
               (-> v
                   ;(assoc :queue-distro (frequencies (get @queue-distributions k)))
                   ;(assoc :queue-size (count @(get @client-queues k [])))
                   (assoc :client-latency (ut/avg (take-last 20 (get @client-latency k []))))
                   (assoc :server-subs (count (keys (get @db/atoms-and-watchers k))))
                   (assoc :last-seen-seconds last-seconds)
                   (dissoc :client-sub-list)
                   (assoc :last-seen (cond (< seconds-ago 0)       "not since boot"
                                           (= k :rvbbit) "n/a"
                                           :else                   (ut/format-duration-seconds seconds-ago)))))})));)

(declare flow-kill!)
(declare alert!)

;; {:client-name {:solver-name {:running? true}}}
(defn stop-solver [solver-name]
  (swap! db/solver-status
         (fn [status]
           (reduce-kv
            (fn [acc client solver-map]
              (if (contains? solver-map solver-name)
                (assoc acc client (assoc-in solver-map [solver-name :running?] false))
                (assoc acc client solver-map)))
            {}
            status))))

(def flow-status-cache (atom {}))

(defn flow-statuses [& [run?]]
  (try
    (if run? ;; only the scheduler runs the side effects, any other callers just get the cache? test.
      (let [fs-map (into {}
                         (for [[k {:keys [*time-running *running? *started-by *finished overrides started waiting-blocks running-blocks]}] @db/flow-status
                               :let [chans               (count (get @flow-db/channels-atom k))
                                     chans-open          (count (doall (map (fn [[_ ch]]
                                                                              (let [vv (try (not (ut/channel-open? ch)) (catch Throwable e (str e)))]
                                                                                (if (cstr/includes? (str vv) "put nil on channel") :open vv)))
                                                                            (get @flow-db/channels-atom k))))
                                     channels-open?      (true? (> chans-open 0))
                                     last-update-seconds (int (/ (- (System/currentTimeMillis) (get @watchdog-atom k)) 1000))
                                     human-elapsed       (if *running? (ut/format-duration started (System/currentTimeMillis)) *time-running)
                                     last-update         (get @last-block-written k)
                                     _ (when (and (> last-update-seconds 120) (empty? running-blocks) (not (nil? last-update)) *running?)
                                         (ut/pp [(str "ATTN: flow " k " killed by rabbit watchdog for going idle 2+ mins")])
                                   ;;(stop-solver k)
                                         (doseq [[client-name solvers] @db/solver-status]
                                           (doseq [[sk _] solvers
                                                   :let [sk-mod (str (cstr/replace (str sk) ":" "") "-solver-flow-")] ;; solver flow name is a keyword?
                                                   :when (= sk-mod k)]
                                             (ut/pp [:killswitch-on client-name sk k sk-mod])
                                      ;(swap! solver-status assoc-in [client-name sk :running?] false)
                                      ;(swap! solver-status assoc-in [client-name sk :stopped] (System/currentTimeMillis))
                                             (swap! db/solver-status update-in [client-name sk] merge {:running? false, :stopped (System/currentTimeMillis)})))
                                         (alert! *started-by
                                                 [:box :style {:color "red"} :child
                                                  (str "ATTN: flow " k " killed by rabbit watchdog for going idle 2+ mins")]
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
                               :blocks_finished     *finished}}))]
        (reset! flow-status-cache fs-map)
        fs-map)
      @flow-status-cache)
    (catch Exception e (ut/pp [:error-in-flow-statuses e]))))


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
             (cond (ut/ne? (get v :raw-fn)) ;; user provided anon fn
                   (let [fn-raw (get v :raw-fn)]
                     {k (-> v
                            (assoc :fn fn-raw)
                            (dissoc :raw-fn))})
                   (ut/ne? (get v :components)) ;; unpacked subflow! (legacy saved flows
                   {k (assoc (process-flowmaps v sub-map) :description [(get v :file-path nil) (get v :flow-id nil)])} ;; sneaky
                   (and (ut/ne? (get v :data)) (ut/ne? (get v :inputs)))
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
                               fn     (get-in @db/flow-function-map fn-key)]
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
                                          defs          (get-in @db/flow-function-map (vec (conj (vec fn-key) :defaults)))
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

(defonce kit-when-fn-cache (atom {}))

;; (ut/pp [:client-panels (get @db/client-panels :vigorous-round-serval-32)])
;; (ut/pp [:client-panels-data (get @db/client-panels-data :vigorous-round-serval-32)])
;; (ut/pp [:client-panels-metadata (get @db/client-panels-metadata :vigorous-round-serval-32)])

(def grid-actions-cache (atom nil))

(defn load-and-cache-grid-actions []
  (when (nil? @grid-actions-cache)
    (let [base-grid-actions (try (edn/read-string (slurp "./defs/custom-grid-actions.edn"))
                                 (catch Exception _
                                   (do (ut/pp [:*realm-custom-grid-actions :read-error :in "./defs/custom-grid-actions.edn"])
                                       {})))
          realm-folders (filter #(.isDirectory %) (file-seq (clojure.java.io/file "./realms")))
          _ (ut/pp [:*realm-custom-grid-actions :realm-folders (map #(.getName %) realm-folders)])
          realm-grid-actions (for [folder realm-folders
                                   :let [file-path (str (.getPath folder) "/custom-grid-actions.edn")
                                         file (clojure.java.io/file file-path)
                                         _ (ut/pp [:*realm-custom-grid-actions :checking-file file-path :exists? (.exists file)])]
                                   :when (.exists file)]
                               (do
                                 (ut/pp [:*realm-custom-grid-actions :found-custom-grid-actions :in (.getName folder) :path file-path])
                                 (try
                                   (let [content (slurp file)
                                         parsed (edn/read-string content)]
                                     (ut/pp [:*realm-custom-grid-actions :successfully-parsed :actions-count (count parsed)])
                                     parsed)
                                   (catch Exception e
                                     (ut/pp [:*realm-custom-grid-actions :custom-grid-actions :read-error :in file-path :error (.getMessage e)])
                                     {}))))
          grid-actions (apply merge base-grid-actions realm-grid-actions)]
      (reset! grid-actions-cache grid-actions)
      (ut/pp [:*realm-custom-grid-actions :total-merged-actions-count (count (keys grid-actions))])))
  @grid-actions-cache)

;; (ut/pp (keys @client-queues))
;; (ut/pp (vec (remove #(= % :rvbbit) (keys @client-queues))))


(defn kick [client-name task-id sub-task thread-id thread-desc message-name & args]
  ;; (when (= sub-task [:heartbeat]) (ut/pp [:kick [client-name task-id sub-task thread-id thread-desc message-name args]]))
  (let [ui-keypath   [:kick] ;;; ^ sub-task is the UI item-key in push ops
        ;payload      (vec args)
        ;payload?     (ut/ne? payload)
        ;heartbeat?   (= sub-task [:heartbeat])
        ;payload      (when (and payload? (not heartbeat?)) (wrap-payload payload thread-id thread-desc message-name))
        ;; _ (when (and payload? (not heartbeat?))
        ;;     (insert-kit-data payload (hash payload) sub-task task-id ui-keypath 0 client-name "flow-id-here!"))
        data         (merge {:sent! task-id :to client-name :at (str (ut/get-current-timestamp))
                             ;:payload payload
                             :extra (vec args)}
                            ;(when payload? {:payload-kp [sub-task task-id]})
                            )
        queue-id     -1
        destinations (cond (= client-name :all)   (vec (remove #(= % :rvbbit) (keys @client-queues)))
                           (keyword? client-name) [client-name]
                           :else                  client-name)
        hb?      (= sub-task [:heartbeat])
        grid-actions nil ;(when hb? (load-and-cache-grid-actions))
        ;; grid-actions (when hb? ;;; should probably cache all this and NOT run it every 15 seconds, but for now it is helpful for dev changes
        ;;               (let [base-grid-actions (try (edn/read-string (slurp "./defs/custom-grid-actions.edn"))
        ;;                                           (catch Exception _
        ;;                                             (do (ut/pp [:*realm-custom-grid-actions :read-error :in "./defs/custom-grid-actions.edn"])
        ;;                                                 {})))
        ;;                    realm-folders (filter #(.isDirectory %) (file-seq (clojure.java.io/file "./realms")))
        ;;                    _ (ut/pp [:*realm-custom-grid-actions :realm-folders (map #(.getName %) realm-folders)])
        ;;                    realm-grid-actions (for [folder realm-folders
        ;;                                             :let [file-path (str (.getPath folder) "/custom-grid-actions.edn")
        ;;                                                   file (clojure.java.io/file file-path)
        ;;                                                   _ (ut/pp [:*realm-custom-grid-actions :checking-file file-path :exists? (.exists file)])]
        ;;                                             :when (.exists file)]
        ;;                                         (do
        ;;                                           (ut/pp [:*realm-custom-grid-actions :found-custom-grid-actions :in (.getName folder) :path file-path])
        ;;                                           (try
        ;;                                             (let [content (slurp file)
        ;;                                                   ;;_ (ut/pp [:*realm-custom-grid-actions :file-content-length (count content)])
        ;;                                                   parsed (edn/read-string content)]
        ;;                                               (ut/pp [:*realm-custom-grid-actions :successfully-parsed :actions-count (count parsed)])
        ;;                                               parsed)
        ;;                                             (catch Exception e
        ;;                                               (ut/pp [:*realm-custom-grid-actions :custom-grid-actions :read-error :in file-path :error (.getMessage e)])
        ;;                                               {}))))
        ;;                    grid-actions (apply merge base-grid-actions realm-grid-actions)
        ;;                    _  (ut/pp [:*realm-custom-grid-actions :total-merged-actions-count (count (keys grid-actions))])]
        ;;                grid-actions))
                       ]
    (doseq [cid destinations]
      (let [sub-task (if hb?
                       (let [ssubs         (vec (keys (get @db/atoms-and-watchers cid {})))
                             subbed-subs   (vec (distinct (get @db/param-var-key-mapping cid [])))
                             sss-map       (into {} (for [[orig subbb] subbed-subs] {subbb orig}))
                             panel-keys    (vec (keys (get @db/client-panels cid)))
                             runners       (get (config/settings) :runners)
                              ;; _ (ut/pp [:heartbeat cid])
                            ;;  grid-actions  (try (edn/read-string (slurp "./defs/custom-grid-actions.edn"))
                            ;;                     (catch Exception _ (do (ut/pp [:custom-grid-actions :read-error]) {})))

                             ;;; this all needs to be moved out of heartbeat and into server-side panel-update side effects - 10/22/24
                             valid-grid-actions    ;(try
                             (into {}
                                   (for [[action-name {:keys [build-clover-fn]}] grid-actions]
                                     {action-name
                                      (try
                                        (let [res ((eval build-clover-fn)
                                                   (get @db/client-panels cid)
                                                   (get @db/client-panels-data cid)
                                                   (get @db/client-panels-metadata cid))]
                                          res)
                                        (catch Exception e (ut/pp [:build-clover-fn-error-grid-actions action-name cid (str e)])))}))
                                                    ; (catch Exception _ {}))
                             valid-kits    (try
                                             (into {} (for [[k v] runners
                                                            :when (get v :kits)]
                                                        (into {}
                                                              (for [[kit-name {:keys [when-fn]}] (get v :kits)]
                                                                {[k kit-name]
                                                                 (try
                                                                   (let [res ((eval when-fn) (get @db/client-panels cid)
                                                                                             (get @db/client-panels-data cid))]
                                                                  ;; not worth to cache given the moving parts needed to be hashed? revisit.
                                                                     res)
                                                                   (catch Exception e (ut/pp [:when-fn-error-kits k kit-name cid (str e)])))}))))
                                             (catch Exception _ {}))


                             replaced-subs (walk/postwalk-replace sss-map ssubs)
                            ;;  replaced-subs (vec (distinct (for [s replaced-subs]
                            ;;                                 (if (or (cstr/starts-with? (str s) ":solver/")
                            ;;                                         (cstr/starts-with? (str s) ":solver-meta/"))
                            ;;                                   (keyword (->
                            ;;                                             (cstr/replace
                            ;;                                              (cstr/replace (str s)
                            ;;                                                            (str (cstr/replace (str cid) ":" "") ">") "") ":" "")
                            ;;                                             (cstr/replace "rvbbit>rvbbit>" "rvbbit>"))) ;; phantom client-name
                            ;;                                   s))))
                            ;;  _ (ut/pp [:hb-boomerang cid replaced-subs])
                             _ (swap! db/actions-atom assoc-in [cid :grid-actions] valid-grid-actions)
                             _ (swap! db/actions-atom assoc-in [cid :valid-kits] valid-kits)
                             ]
                         {:subs replaced-subs
                          :panel-keys panel-keys
                          ;:grid-actions valid-grid-actions
                          ;:kits valid-kits
                          })
                       sub-task)]
        (when hb?
          (swap! ping-ts assoc cid (System/currentTimeMillis))
              ;; (ut/pp [:hb-sent cid client-name])
              )
        (push-to-client ui-keypath data cid queue-id task-id sub-task)))
    :sent!))


;; (ut/pp @db/actions-atom )
;; (ut/pp (distinct (mapv #(take 4 %) (ut/keypaths @client-panels-data))))
;; (ut/pp (distinct (mapv #(take 4 %) (ut/keypaths @client-panels))))
;; (ut/pp (get-in @client-panels-data [:efficient-fat-mallard-1 ]))

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
        ;(ext/create-dirs "./flow-blocks/")
        (ut/pretty-spit fp block 225)))
    (swap! sub-flow-blocks assoc flow-id block)
    {flow-id block}))

(defn alert!
  [client-name content w h duration & [type]]
  (push-to-client [:alerts] [content w h duration] client-name -1 (rand-nth [:alert1 :alert2 :alert3]) (or type :alert2))
  :alerted!)

(defn ttap> [client-name x & [w h d]] (alert! client-name (if (number? x) (str x) x) (or w 10) (or h 1.35) (or d 12)))



(defn save!
  [kp v & [client-name]]
  (let [kp (if (keyword? kp) [kp] kp)] ;; just incase
    (swap! db/server-atom assoc-in kp v)))

(def last-times (atom {})) ;; stopgap for error times TODO, april 2024

(def flows-run (atom 0))

(defn flow!
  [client-name flowmap file-image flow-id opts & [no-return?]]

  (ppy/execute-in-thread-pools-but-deliver :flow-runners ;; (keyword (str "flow-runner/" (cstr/replace client-name ":" "")))
                                           (fn []
                                             (try
                                               (let [_                (swap! flows-run inc)
                                                     orig-flowmap     flowmap
                                                     orig-flow-id     (get opts :orig-flow-id flow-id)
                                                     from-string?     (true? (string? flowmap))
                                                     _                (swap! orig-caller assoc flow-id client-name) ;; caller for the flow in case of
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
                                                                            (for [[k v] (get @db/params-atom client-name)]
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
                                                                                              (into {} (for [kk (keys @config/settings-atom)]  ;; TODO allow full clover keypath lookup
                                                                                                {(keyword (str "settings/" (cstr/replace (str kk) ":" "")))
                                                                                                 (get @config/settings-atom kk)}))
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
                                                     _ (do ;(ext/create-dirs "./flow-history") ;; just in case
                                                           (spit ;; a partial flow-history map so if someone wants to "jump into" a
                                                            (str "./flow-history/" flow-id ".edn")
                                                            {:image       image-map ;; a partial flow-history map so if someone wants to
                                                             :source-map  finished-flowmap
                                                             :client-name client-name
                                                             :flow-id     flow-id}))
                                                     return-val       (flow-waiter (eval finished-flowmap) uid opts) ;; eval to realize fn
                                                     retry?           (get-in opts [:opts :retry-on-error?] false)
                                                     rtty             (get-in opts [:opts :retries] 0)
                                                     rtty             (if (> rtty 25) 25 rtty)
                                                     restarts         (if retry? (- rtty (get @restart-map flow-id 0)) 0)
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
        ;; (when (not (cstr/includes? flow-id "keepalive"))
        ;;   (ut/pp [:flowmap-returned-val (ut/limited (ut/replace-large-base64 return-val)) :flowmap-returned-val]))
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
                                                                  (str (get-in @db/flow-status [flow-id :*time-running]) " ")])
                                                               [:box :style {:font-weight 700 :font-size (if error? "13px" "10px") :opacity (if error? 1.0 0.7)} :child
                                                                (if error? (str sample) (str "returns: " sample))]]]
                                                             10
                                                             (if (and error? restarts-left?) 1.5 1.35)
                                                             (if error? 25 9))
                                                     (when (and error? restarts-left?)
                                                       ;(async/thread ;; lets try blocking, we are already in a pool exec context
                                                         (do (Thread/sleep 2000)
                                                             (flow-statuses) ;; make sure the statuses are up to date.
                                                             (Thread/sleep 12000)
                                                             (flow! client-name orig-flowmap file-image flow-id opts)));)
                                                     (when (not error?) (swap! restart-map dissoc flow-id)) ;; reset the counter on
                                                     (do (when (not (= flow-id "client-keepalive")) ;; no need to track heartbeats..
                                                           (swap! latest-run-id assoc flow-id run-id)
                                                           (ut/pp [:saving-flow-exec-for flow-id result-code run-id])
                                                           (do ;(ext/create-dirs "./flow-history") ;; just in case
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
                                                       {:error error-map})))))))

(defn schedule!
  [time-seq1 flowmap & [opts]]
  (let [;[opts chan-out override] args
        opts            (if (nil? opts) {} opts)
        client-name     :rvbbit
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

(defn notify-leaf-defs-updated [client-name message]
  (let [message (or message "Leaf definitions have been updated...")]
    (alert! client-name
          [:box
           :style {:font-size "26px"}
           :child (str "🍂 " message " 🌰 🍂 🐿️")]
          12
          2
          2)))

(defn notify-general [client-name message]
  (let [message (or message "Something happened!...")]
    (alert! client-name
            [:box
             :style {:font-size "26px"}
             :child (str message)]
            12
            2
            2)))

(defn get-flow-open-ports
  [flowmap flow-id & [client-name]] ;; generally a local path, not a map, but hey w/e
  ;;(ut/pp [:get-flow-open-ports! flowmap flow-id (or client-name  :rvbbit)])
  (let [raw                 (try (edn/read-string (slurp (if (cstr/ends-with? (cstr/lower-case flowmap) ".edn")
                                                           flowmap ;; file path, use as is
                                                           (str "./flows/" flowmap ".edn"))))
                                 (catch Exception _
                                   (do
                                     ;;(ut/pp [:error-reading-flow-from-disk-raw-open-ports flow-id client-name])
                                     {})))
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

(defmethod wl/handle-push :start-conversation
  [{:keys [assistant-name client-name]}]
  (ut/pp [:ai-worker-conversation-started! client-name assistant-name])
  (ppy/execute-in-thread-pools
   :ai-workers
   (fn [] (let [thread-id (assistants/start-conversation client-name assistant-name)]
            (push-to-client [:new-conversation] [assistant-name (str thread-id)] client-name -1 [:ai-worker]
                            [assistant-name (str thread-id)])))))

(defmethod wl/handle-push :send-basic-message
  [{:keys [assistant-name client-name thread-id message image-path]}]
  (let [image-path (when image-path (get (assistants/analyze-image-path image-path) :absolute-path))]
    (ut/pp [:ai-worker-send-basic-message! client-name assistant-name])
    (ppy/execute-in-thread-pools
     :ai-workers
     (fn [] (assistants/send-message client-name assistant-name thread-id message image-path)))))

(defmethod wl/handle-push :delete-thread
  [{:keys [assistant-name client-name thread-id]}]
  (ut/pp [:ai-worker-delete-thread! client-name assistant-name])
  (ppy/execute-in-thread-pools
   :ai-workers
   (fn [] (assistants/delete-worker-thread client-name assistant-name thread-id))))

(defmethod wl/handle-request :get-flow-open-ports
  [{:keys [client-name flowmap flow-id]}]
  (get-flow-open-ports flowmap flow-id client-name))

(defmethod wl/handle-request :run-flow
  [{:keys [client-name flowmap file-image flow-id opts no-return?]}]
  (ut/pp [:running-flow-map-from client-name])
  (when (not (get-in (flow-statuses) [flow-id :*running?] false))
    (flow! client-name flowmap file-image flow-id opts no-return?)))

(defmethod wl/handle-push :run-flow
  [{:keys [client-name flowmap file-image flow-id opts no-return?]}]
  (ut/pp [:running-flow-map-from client-name])
  (when (not (get-in (flow-statuses) [flow-id :*running?] false))
   ; (ppy/execute-in-thread-pools (keyword (str "client/flow-runner." (cstr/replace (str client-name) ":" ""))) ;;:flow-runner
    ;                     (fn []
    (flow! client-name flowmap file-image flow-id opts true)
     ;                      ))
    ))

(defn rabbit-edit [{:keys [block-keypath block-body client-name]}]
  (try
    (let [block-keypath (edn/read-string block-keypath)
          block-body (edn/read-string block-body)]
      (ut/pp [:rabbit-edit-trying {block-keypath block-body} :for client-name])
      (push-to-client (vec (rest block-keypath)) [] client-name 1 :push-assocs {block-keypath block-body})
      (str "Canvas modified successfully - " block-keypath))
    (catch Exception e
      (ut/pp [:rabbit-edit-error (.getMessage e)])
      (str "Error modifying canvas - " (.getMessage e)))))

(declare ship-estimate query-runstream)

;; (ut/pp @transit-file-mapping)
;; (ut/pp (keys @transit-file-mapping))
;; (reset! transit-file-mapping {})

(defn create-ansi-box [content]
  (let [border-color "\u001b[38;5;213m"  ; Pink
        text-color "\u001b[36m"          ; Cyan
        bold "\u001b[1m"                 ; Bold
        reset "\u001b[0m"
        width 60
        top-bottom (str border-color "+" (apply str (repeat (- width 2) "-")) "+" reset)
        wrap-line (fn [line]
                    (if (<= (count line) (- width 2))
                      [line]
                      (let [words (clojure.string/split line #"\s+")
                            lines (reduce (fn [acc word]
                                            (let [last-line (last acc)
                                                  new-line (str last-line " " word)]
                                              (if (<= (count new-line) (- width 2))
                                                (conj (vec (butlast acc)) new-line)
                                                (conj acc word))))
                                          [""]
                                          words)]
                        (filter #(ut/ne? %) lines))))
        content-lines (mapcat wrap-line (clojure.string/split-lines content))
        formatted-lines (map-indexed
                         (fn [idx line]
                           (let [padding (quot (- width 2 (count line)) 2)
                                 left-padding (apply str (repeat padding " "))
                                 right-padding (apply str (repeat (- width 2 (count line) padding) " "))
                                 formatted (str left-padding line right-padding)]
                             (str border-color "│" reset text-color
                                  (if (zero? idx) bold "")
                                  formatted
                                  (if (zero? idx) reset "")
                                  reset border-color "│" reset)))
                         content-lines)]
    (clojure.string/join "\n"
                         (concat [top-bottom]
                                 formatted-lines
                                 [top-bottom]))))

;; (ut/pp [:clpp (get-in @client-panels [:worthy-bronze-grasshopper-42 :block-6308])])
;; (ut/pp (keys (:worthy-bronze-grasshopper-42 @db/params-atom)))
;; (ut/pp (ut/dissoc-in db/params-atom [:worthy-bronze-grasshopper-42 :block-8309]))
;; (ut/pp @db/query-metadata)
;; (reset! times-atom {})
;; (ut/pp (keys (config/settings) ))

(defmethod wl/handle-push :run-kit
  [{:keys [client-name ui-keypath data-key panel-key tab-name runner kit-keypath kit-runner-key opts-map]}]
  (ut/pp [:run-kit-pre client-name ui-keypath data-key panel-key tab-name runner kit-keypath kit-runner-key opts-map])
  (try
    (let [_ (swap! db/kit-atom assoc-in [:kicks client-name] ;; so the UI has the :kicks :buffy option to watch before completeted
                   (vec (distinct (conj (get-in @db/kit-atom [:kicks client-name] []) data-key))))
          opts-map (when opts-map (into {} (for [[k v] opts-map] {(keyword (str "*" (cstr/replace (str k) ":" ""))) v}))) ;; mostly got fabric pre-loaded reqs
          _ (ut/pp [:kit-pre-opts-map opts-map])
          kit-runner-key  (if (string? kit-runner-key) (keyword kit-runner-key) kit-runner-key)
          kit-runner-key-str (cstr/replace (str kit-runner-key) ":" "")
          times-key       kit-keypath ;;(into [:kit-run] kit-keypath) ;;(into kit-keypath ui-keypath)
          [kit-runner
           kit-name]      kit-keypath
          runner-map      (config/settings)
          kit-runner-fn   (get-in runner-map [:runners kit-runner :kits kit-name :kit-expr])
          kit-view-fns    (get-in runner-map [:runners kit-runner :kits kit-name :kit-view-exprs])
          kit-view-opts   (get-in runner-map [:runners kit-runner :kits kit-name :kit-view-opts] {})
          output-type     (get-in runner-map [:runners kit-runner :kits kit-name :output])
          repl-host       (get-in runner-map [:runners kit-runner :runner :host])
          repl-port       (get-in runner-map [:runners kit-runner :runner :port])
          _ (ship-estimate client-name kit-runner-key times-key)
          _ (ut/pp [:running-kit-from client-name ui-keypath kit-runner-key])
          _ (swap! db/kit-status assoc-in [kit-runner-key :running?] true)
          host-runner     runner
          kit-view?       (ut/ne? kit-view-fns)
          clover-kw1       (filterv #(and (keyword? %) (cstr/includes? (str %) "/")) (ut/deep-flatten kit-runner-fn))
          clover-kw2       (filterv #(and (keyword? %) (cstr/includes? (str %) "/")) (ut/deep-flatten kit-view-fns))
          clover-kw        (set (into clover-kw1 clover-kw2))
          clover-lookup-map (into {} (for [kw clover-kw] {kw (db/clover-lookup client-name kw)}))
          context-data    (merge
                           clover-lookup-map
                           {:client-panels (get @db/client-panels client-name)
                            :client-panels-data (get @db/client-panels-data client-name)
                            :client-panels-metadata (get @db/client-panels-metadata client-name)
                            :query-metadata (get-in @db/query-metadata [client-name data-key])
                            :height-int (* (get-in @db/client-panels [client-name panel-key :h] 0) 50)
                            :width-int (* (get-in @db/client-panels [client-name panel-key :w] 0) 50)
                            :panel-key panel-key
                            :data-key data-key
                            :tab-name tab-name
                            :host-runner host-runner
                            :ui-keypath [:panels panel-key host-runner data-key]})
          transit-file    (ext/write-transit-data context-data kit-runner-key-str client-name kit-runner-key-str)
          limited-postwalk-map (merge
                                {:transit-file transit-file}
                                (into {} (for [[k v] (dissoc (config/settings) :runners)] ;; we need to replace all this with a selective clover lookup walk map TODO (clover resolver)
                                           {(keyword (str "settings/" (cstr/replace (str k) ":" ""))) v}))
                                clover-lookup-map
                                (select-keys context-data [:panel-key :data-key :ui-keypath :query-metadata :height-int :width-int :tab-name :host-runner]))
                                ;; ^^ only the smaller strucs will we allow to be postwalked, else they need to be loaded from the transit file(s)

          ;; big blocking step for shipping kit fn view UI, but times out after a minute and cancels
          _ (when kit-view? ;; render and set kit views to gather options for the user before we execute the kit
              ;; (alert! client-name ;; temp, way too many alerts
              ;;         [:box :child "waiting on user kit options..."]
              ;;         10
              ;;         nil
              ;;         4)
               ;; set kit defaults on client
              (when (ut/ne? kit-view-opts)
                (push-to-client ui-keypath [] client-name 1 :kit-view-opts kit-view-opts))
               ;; generate view clover and push it to the client, then we wait for the go! signal
              (let [response-path [client-name panel-key :go!]
                    timeout-ms (* 1000 240) ;; 4 minutes
                    start-time (System/currentTimeMillis)
                    loaded-kit-view-fns (walk/postwalk-replace limited-postwalk-map kit-view-fns)
                    rendered-views (ut/timed-exec
                                    (ppy/execute-in-thread-pools-but-deliver
                                     (keyword (str "serial-kit-instance/" kit-runner-key-str))
                                     (fn []
                                       (evl/repl-eval (first loaded-kit-view-fns) repl-host repl-port client-name kit-runner-key ui-keypath))))
                    view-fn-error (get-in rendered-views [:result :evald-result :error])
                    _ (do
                ;; Send things to the client here
                        ;; [ui-keypath data client-name queue-id task-id status & [reco-count elapsed-ms]]
                        (push-to-client ui-keypath [] client-name 1 :kit-view (if view-fn-error
                                                                                [:box
                                                                                 :align :center :justify :center
                                                                                 :size "auto"
                                                                                 :child [:str "error: " view-fn-error]]
                                                                                (get rendered-views :result))))
                    _ (swap! db/kit-atom assoc-in [kit-runner-key :incremental]
                             (create-ansi-box
                              (str "waiting for user input... " (ut/millis-to-date-string (System/currentTimeMillis)) "\n ")))
                    user-response (loop []
                                    (let [current-value (get-in @db/params-atom response-path)
                                          elapsed-time (- (System/currentTimeMillis) start-time)]
                                      (cond
                                        (= current-value :go!) :continue
                                        (= current-value :cancel!) :abort
                                        (> elapsed-time timeout-ms) :timeout
                                        :else (do (Thread/sleep 100) (recur)))))
                    _ (when user-response
                        (push-to-client ui-keypath [] client-name 1 :kit-view-remove []))
                    _ (when (= user-response :abort)
                        (swap! db/kit-status assoc-in [kit-runner-key :running?] false)
                        (swap! db/kit-atom assoc-in [kit-runner-key :incremental]
                               (create-ansi-box
                                (str "kit run cancelled by user \n"
                                     (ut/millis-to-date-string (System/currentTimeMillis)) "\n ")))
                        (throw (ex-info "User cancelled the operation" {:type :user-cancel})))
                    _ (when (= user-response :timeout)
                        (swap! db/kit-status assoc-in [kit-runner-key :running?] false)
                        (swap! db/kit-atom assoc-in [kit-runner-key :incremental]
                               (create-ansi-box
                                (str "kit run timeout waiting for user response \n"
                                     (ut/millis-to-date-string (System/currentTimeMillis)) "\n ")))
                        (throw (ex-info "Operation timed out waiting for user response" {:type :timeout})))]
                (Thread/sleep 2000)))
           ;;kit-opts (when kit-view? )

          opts-map (merge (dissoc (get-in @db/params-atom [client-name panel-key]) :go!) opts-map)
          added-postwalk-map (merge limited-postwalk-map ;; add in view opts if any
                                    opts-map)

          _ (swap! db/kit-atom assoc-in [kit-runner-key :incremental]
                   (create-ansi-box
                    (str "starting kit run " (ut/millis-to-date-string (System/currentTimeMillis)) "\n "
                         (str kit-keypath "\n " ui-keypath "\n"
                              (when (ut/ne? added-postwalk-map)
                                (str "added user opts: \n"
                                     (doall
                                      (apply str
                                             (for [[k v] (dissoc added-postwalk-map :query-metadata)]
                                               (str k "  " v "\n"))))))))))
          _ (Thread/sleep 3000)  ;; temp until we know the client has caught up
          _ (when (= host-runner :queries) ;; we need the FULL table
              (alert! client-name
                      [:v-box
                       :padding "5px"
                       :gap "10px"
                       :children
                       [[:box :child (str "pull *full* query rows for analysis...")]
                        [:box :style {:font-size "11px" :opacity 0.7}
                         :child (str (ut/millis-to-date-string (System/currentTimeMillis)))]]]
                      nil ;9
                      nil
                      6)
              ;; (ut/pp (get-in @client-panels-resolved [client-name host-runner data-key :connection-id]
              ;;                (get-in @client-panels-resolved [client-name :connection-id])))
              ;;(ut/pp (get-in @honey-echo [client-name data-key]))
              ;;(ut/pp @honey-echo)
              ;;(ut/pp (get @client-panels-resolved :polished-octohedral-mouse-27))
              ;; (ut/pp @client-panels-resolved)
              ;;(ut/pp [:HE (get-in @honey-echo [client-name data-key])])
              (query-runstream :honey-xcall
                               [data-key] ;;ui-keypath
                               (dissoc (get-in @honey-echo [client-name data-key]) :connection-id)
                               ;(get-in @client-panels-resolved [client-name host-runner data-key]) ;; we need the fully resolved SQL...
                               false
                               false
                               (get-in @honey-echo [client-name data-key :connection-id])
                              ;;  (get-in @client-panels-resolved [client-name host-runner data-key :connection-id]
                              ;;          (get-in @client-panels-resolved [client-name :connection-id]))
                               client-name
                               -2 ;; -2 limits at 1M rows...
                               panel-key
                               nil
                               false
                               false)
              (alert! client-name
                      [:v-box
                       :padding "5px"
                       :gap "10px"
                       :children
                       [[:box :child (str "data pulled, running analysis...")]
                        [:box :style {:font-size "11px" :opacity 0.7}
                         :child (str (ut/millis-to-date-string (System/currentTimeMillis)))]]]
                      nil ;9
                      nil
                      6))

          ;; clover-kw        (filterv #(and (keyword? %) (cstr/includes? (str %) "/")) (ut/deep-flatten kit-runner-fn))
          ;; clover-lookup-map (into {} (for [kw clover-kw] {kw (db/clover-lookup client-name kw)}))
          ;; context-data    (merge
          ;;                  clover-lookup-map
          ;;                  {:client-panels (get @client-panels client-name)
          ;;                   :client-panels-data (get @client-panels-data client-name)
          ;;                   :client-panels-metadata (get @client-panels-metadata client-name)
          ;;                   :panel-key panel-key
          ;;                   :data-key data-key
          ;;                   :ui-keypath [:panels panel-key host-runner data-key]})
          ;; transit-file    (ext/write-transit-data context-data kit-runner-key-str client-name kit-runner-key-str)
          limited-postwalk-map (merge added-postwalk-map {:opts-map opts-map}
                                   ;; ^^ re-adding a 'literal' opts-map key just for redundancy - even thought the keys already exist in the root map
                                      {:transit-rowset (get-in @ext/transit-file-mapping [client-name data-key :file])
                                       :transit-rowset-meta (get-in @ext/transit-file-mapping [client-name data-key :meta-file])}
                                      limited-postwalk-map)
          _ (ut/pp [:debug-kit runner kit-keypath kit-runner-fn repl-host repl-port transit-file])
          loaded-kit-runner-fn (walk/postwalk-replace limited-postwalk-map
                                                      kit-runner-fn)
          _ (ut/pp [:debug-kit2 runner kit-keypath loaded-kit-runner-fn repl-host repl-port transit-file])
          {:keys [result elapsed-ms]} (ut/timed-exec
                                       (ppy/execute-in-thread-pools-but-deliver
                                        (keyword (str "serial-kit-instance/" kit-runner-key-str))
                                        (fn []
                                          (evl/repl-eval loaded-kit-runner-fn repl-host repl-port client-name kit-runner-key ui-keypath))))
          _ (ut/pp [:debug-kit3 runner kit-keypath result elapsed-ms])
          error?   (cstr/includes? (cstr/lower-case (str result)) " error ")  ;; lame , get codes later
          output   (get-in result [:evald-result :value 0])
          ;console (get-in result [:evald-result :out])
          ]

      ;;(ut/pp [:kit-runner-output result {:elapsed-ms? elapsed-ms}])

      ;;(ut/pp  [:result result])
      (when (and ;(= output-type :kit-map)
                 (ut/ne? output)
                 (map? output))

        (ut/pp [:inserting-into-kit-results-table.. (count output) (vec (keys output))])

        (let [push-assocs (get output :push-assocs)
              push-alerts (get output :push-alerts)
              output (dissoc output :push-assocs :push-alerts)]

          (when (and (ut/ne? push-assocs)
                     (map? push-assocs))
            (push-to-client ui-keypath [] client-name 1 :push-assocs push-assocs))

          (when (and ;; send any push alerts
                 (ut/ne? push-alerts)
                 (or (vector? push-alerts)
                     (map? push-alerts)))
            (if (vector? push-alerts)
              (doseq [alert push-alerts
                      :let [{:keys [body seconds w h]} alert]]
                (alert! client-name body w h (or seconds 45)))
              (let [{:keys [body seconds w h]} push-alerts]
                (alert! client-name body w h (or seconds 45)))))
          ;; send the rest of the kit data for the client's UI to render
          (insert-kit-data output
                           (hash output)
                           data-key ;(last ui-keypath)
                           "query-log" ;;kit-expr-push"
                           :kick
                           elapsed-ms
                           client-name)))

      ;;(Thread/sleep 6000)

      (alert! client-name
              [:v-box
               :padding "5px"
               :gap "10px"
               :children
               [;;[:box :child (str "kit run finished " kit-keypath " " kit-runner-key)]
                [:box :child (str "kit run finished")]
                ;; [:box
                ;;  ;:style {:font-size "11px"}
                ;;  :child (str "done")]
                ;; (when (ut/ne? console)
                ;;   [:box
                ;;    :style {:border "1px solid #ffffff22"
                ;;            :border-radius "14px"}
                ;;    :child [:terminal-custom [console (* 17 50) 250]]])
                [:box :style {:font-size "11px" :opacity 0.7}
                 :child (str (ut/millis-to-date-string (System/currentTimeMillis)))]]]
              nil
              nil
              5)
      (swap! db/kit-status assoc-in [kit-runner-key :running?] false)
      (when true ;; (not error?)
        (swap! times-atom assoc times-key (conj (get @times-atom times-key []) elapsed-ms)))
      (ut/pp [:finished-kit-from client-name ui-keypath]))
    (catch Exception e
      (let [kit-runner-key (if (string? kit-runner-key) (keyword kit-runner-key) kit-runner-key)]
        (ut/pp [:kit-runner-fn-error client-name ui-keypath data-key panel-key runner kit-keypath kit-runner-key (str e)])
        (swap! db/kit-status assoc-in [kit-runner-key :running?] false)
        (alert! client-name
                [:v-box
                 :padding "5px"
                 :gap "10px"
                 :children
                 [[:box :child (str "kit runner fn error...")]
                  [:box :style {:font-size "11px" :opacity 0.7}
                   :child (str e)]]]
                nil
                nil
                240)
        (swap! db/kit-atom assoc-in [kit-runner-key :incremental]
               (create-ansi-box
                (str "kit run error \n"
                     (ut/millis-to-date-string (System/currentTimeMillis)) "\n " e "\n")))
        (throw (ex-info "kit run error" {:type :error})))
      )))

;; (ut/pp @flow-status)

(declare run-solver)

;;; run-solver  [solver-name client-name & [override-map override-input temp-solver-name keypath]]

;;; TODO, these 3 can all be combined into one "smarter" version base don args, but I'm still iterating, so it's fine
(defmethod wl/handle-request :run-solver
  [{:keys [solver-name client-name override-map ui-keypath]}]
  (ut/pp [:manual-solver-run! solver-name :from client-name :override override-map])
  (swap! db/last-solvers-atom-meta assoc-in
         [solver-name :output]
         [:warning! {:solver-running-manually-via client-name :with-override-map override-map}])
  ;(enqueue-task-slot-pool client-name (fn []
  (ppy/execute-in-thread-pools :client/run-solver ; (keyword (str "client/run-solver." (cstr/replace (str client-name) ":" "")))
  ;(qp/slot-queue :solvers client-name
                               (fn []  (run-solver solver-name client-name override-map))))

(defmethod wl/handle-push :run-solver
  [{:keys [solver-name client-name override-map ui-keypath]}]
  (ut/pp [:manual-solver-run! solver-name :from client-name :override override-map])
  (swap! db/last-solvers-atom-meta assoc-in
         [solver-name :output]
         [:warning! {:solver-running-manually-via client-name :with-override-map override-map}])
  ;(enqueue-task-slot-pool client-name (fn []
  (ppy/execute-in-thread-pools :client/run-solver ; (keyword (str "client/run-solver." (cstr/replace (str client-name) ":" "")))
  ;(qp/slot-queue :solvers client-name
                               (fn []  (run-solver solver-name client-name override-map))))

(defmethod wl/handle-push :run-solver-custom
  [{:keys [solver-name temp-solver-name client-name override-map input-map ui-keypath]}]
  ;; (ut/pp [:custom-solver-run! temp-solver-name :from client-name :input-map input-map])
  (swap! db/last-solvers-atom-meta assoc-in
         [temp-solver-name :output]
         [:warning! {:solver-running-custom-inputs-via client-name :with-input-map input-map :override-map? override-map}])
;;  (run-solver solver-name client-name override-map input-map temp-solver-name)
  ;(enqueue-task-slot-pool client-name (fn []
  (ppy/execute-in-thread-pools :client/run-solver ; (keyword (str "client/run-solver." (cstr/replace (str client-name) ":" "")))
  ;(qp/slot-queue :solvers client-name
                               (fn []  (run-solver solver-name client-name override-map input-map temp-solver-name ui-keypath)))
  temp-solver-name)

(defn flow-kill!
  [flow-id client-name]
  (future (swap! db/flow-status assoc
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
  (let [sub-task (vec (keys (get @db/atoms-and-watchers cid {})))]
    ;; (ppy/execute-in-thread-pools :boomerang-heartbeat
    ;;                              (doseq [fk sub-task]
    ;;                                (sub-to-value cid fk))) ;; resub just in case?
    (push-to-client [:kick]
                    {:at "" :payload nil :payload-kp [:heartbeat :heartbeat] :sent! :heartbeat :to :all}
                    cid
                    -1
                    [:heartbeat]
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
   :status       (get @db/flow-status flow-id)
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
        history (select-keys (get @db/last-signals-history-atom signal-name {}) ccw)]
    history))

;; (defmethod wl/handle-request :save-signals-map
;;   [{:keys [client-name data-map]}]
;;   (ut/pp [:saving-signals-map client-name])
;;   (let [bm {}] (reset! signals-atom data-map)))

;; (defmethod wl/handle-request :save-rules-map
;;   [{:keys [client-name data-map]}]
;;   (ut/pp [:saving-rules-map client-name])
;;   (let [bm {}] (reset! rules-atom data-map)))

;; (defmethod wl/handle-request :save-solvers-map
;;   [{:keys [client-name data-map]}]
;;   (ut/pp [:saving-solvers-map client-name])
;;   (let [bm {}] (reset! solvers-atom data-map)))

(defmethod wl/handle-request :save-custom-flow-block
  [{:keys [client-name name block-map]}]
  (ut/pp [:saving-custom-flow-block client-name :for name])
  (let [bm {name block-map}] (ut/pp [:saving-new-custom-block bm])))


(defmethod wl/handle-push :sync-client-params
  [{:keys [client-name params-map]}]
  (let [params     (or (dissoc params-map nil) {})
        kps        (ut/kvpaths params)
        make-key   (fn [x] (str :client-param "-" (cstr/join ">" x)))
        these-keys (map make-key kps)]
    (swap! db/params-atom assoc client-name params) ;; push the actual params
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
        (sql-exec systemh2-db
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

;; (defmethod wl/handle-request :get-status
;;   [{:keys [client-name]}]
;;   (ut/pp [:client-status-check-from client-name])
;;   (inc-score! client-name :push)
;;   (ut/pp [:PUSH-get-status! client-name])
;;   {:statuses (get @queue-status client-name) :data (get @queue-data client-name)})

(defmethod wl/handle-request :get-flow-statuses
  [{:keys [client-name]}]
  (inc-score! client-name :push)
  ;; (ut/pp [:PUSH-get-flow-status! client-name])
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



(defn grab-string-chunk [s]
  (try (subs (str s) 0 100)
       (catch Throwable _ (str s))))


(defn unsub-value [client-name flow-key]
  (let [;;orig-flow-key flow-key
        subbed-sub (get-in @db/param-var-crosswalk [client-name flow-key])
        flow-id    nil]
    (if (ut/ne? subbed-sub) ;; clean up ugly crosswalk keys; what in gods name have we done?
      (let [[srv-flow-key mapping-key] subbed-sub
            sub                        (get-in @db/atoms-and-watchers [client-name srv-flow-key] {})]
        (ut/pp [:unsubbing*w.var! client-name flow-key sub])

        (db/remove-watcher (:keypath sub) client-name (:sub-type sub) flow-id (:flow-key sub))

        (swap! db/param-var-mapping dissoc [client-name mapping-key]) ;; compound (single) key
        (swap! db/param-var-crosswalk ut/dissoc-in [client-name flow-key])
        (swap! db/param-var-key-mapping assoc
               client-name
               (vec (filter #(not (= (first %) flow-key)) (get @db/param-var-key-mapping client-name)))))
      (let [sub (get-in @db/atoms-and-watchers [client-name flow-key] {})]
        (ut/pp [:unsubbing! client-name flow-key sub])

        (db/remove-watcher (:keypath sub) client-name (:sub-type sub) flow-id (:flow-key sub))))))

(defmethod wl/handle-push :unsub-to-flow-value
  [{:keys [client-name flow-key]}]
  (unsub-value client-name flow-key))

;;(ut/pp (get @db/atoms-and-watchers :keen-aquamarine-raven-32))

(defn remove-watchers-for-flow
  [flow-id & [client-name]]
  (doseq [[c-name subs] (if client-name
                          (select-keys @db/atoms-and-watchers [client-name])
                            @db/atoms-and-watchers)
          :let          [matching-subs (filter #(= (first (:keypath %)) flow-id) (vals subs))]
          :when         (ut/ne? matching-subs)]
    (ut/pp [:matching-subs c-name matching-subs])
    (doseq [sub matching-subs]
      (do ;; (ut/pp [:removing (count matching-subs) :watchers :for flow-id c-name
          ;;         [[(:keypath sub) c-name (:sub-type sub) flow-id (:flow-key sub)]]])
          (db/remove-watcher (:keypath sub) c-name (:sub-type sub) flow-id (:flow-key sub))
                             ;;[keypath client-name sub-type flow-id flow-key]
          ))))




(declare client-kp)

(defn millis-to-date
  [millis]
  (let [zdt       (java.time.ZonedDateTime/ofInstant (java.time.Instant/ofEpochMilli millis) (java.time.ZoneId/systemDefault))
        formatter (java.time.format.DateTimeFormatter/ofPattern "yyyy-MM-dd HH:mm:ss SSS")]
    (.format zdt formatter)))

(declare query-runstream)

(defn solver-sql
  [solver-name honey-sql snapshot? & [client-name]] ;; basically same as front-end - convert the query to a
  ;; (ut/pp [:solver-sql [solver-name (str honey-sql) snapshot?]])
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
        ;flat           (ut/deep-flatten honey-sql)
        ;literal-data?  (and (some #(= % :data) flat) (not (some #(= % :panel_history) flat)))
        honey-modded   (if has-rules? (assoc honey-sql :select (apply merge hselect rules)) honey-sql)
        client-name    (or client-name :rvbbit)
        honey-modded   (walk/postwalk-replace {:*client-name client-name
                                               :*client-name* client-name
                                               :*client-name-str (pr-str client-name)} honey-modded)
        ;client-name    (keyword (str (cstr/replace (str client-name) ":" "") ".via-solver"))
        client-cache?  false ;(if literal-data? (get honey-sql :cache? true) false)
        ]
    ;; (ut/pp [:solver-sql! solver-name orig-honey-sql honey-sql honey-modded connection-id client-name snapshot?])
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

;;(defonce running-solvers (atom []))


;; (def solver-cache (atom (cache/lru-cache-factory {} :threshold 2000)))
;; (defn lookup-scache-exists? [key] (cache/has? @solver-cache key))
;; (defn get-from-scache [key] (cache/lookup @solver-cache key nil))
;; ;;(defn insert-into-scache [key value] (swap! solver-cache assoc key value))
;; ;;(defn insert-into-scache [key value] (swap! solver-cache cache/miss key value))
;; (defn insert-into-scache [key value]
;;   (swap! solver-cache cache/through key (constantly value)))



(def solvers-run (atom 0))

(defn err? [s]
  (let [s (str s)]
    (or (cstr/includes? s "Exception") (cstr/includes? s ":err") (cstr/includes? s ":error"))))

(defn sample-alert-clover [sample-message-lines sample-error solver-name]
  [:v-box
   :gap "6px"
   :children
   [;[:box :style {:font-size "13px"} :child (str solver-name " - " (get-in output-full [:sampled :message]))]
    [:box
     :style {:font-size "16px"}
     :child (str (first sample-message-lines) ".")]

    [:h-box
     :style {:font-size "13px"}
     :gap "12px"
     :children (vec (for [x (rest sample-message-lines)]
                      [:v-box
                       :gap "6px"
                       :children (let [ss (cstr/split x #"\: ")]
                                   [[:box
                                     :style {:font-size "15px"}
                                     :child (str (first ss))]
                                    [:v-box
                                     :style {:opacity 0.9 :font-weight 700}
                                     :children (vec (for [e (rest (cstr/split (str (last ss)) #"\, "))
                                                          :let [e (cond
                                                                    (cstr/ends-with? e "items")
                                                                    [:h-box
                                                                     :gap "8px"
                                                                     :justify :between
                                                                     :children
                                                                     [[:box :child (ut/nf (edn/read-string (first (cstr/split e #" "))))]
                                                                      [:box
                                                                       :style {:opacity 0.5}
                                                                       :child "values"]]]

                                                                    (cstr/includes? e "size")
                                                                    [:h-box
                                                                     :gap "8px"
                                                                     :justify :between
                                                                     :children
                                                                     [[:box
                                                                       :style {:opacity 0.5}
                                                                       :child "est size"]
                                                                      [:box :child (let [result (last (cstr/split e #"size "))
                                                                                         truncated (if (.endsWith result ".")
                                                                                                     (.substring result 0 (- (.length result) 1))
                                                                                                     result)]
                                                                                     truncated)]]]

                                                                    (cstr/starts-with? e "dimensions")
                                                                    [:h-box
                                                                     :gap "8px"
                                                                     :justify :between
                                                                     :children
                                                                     [[:box
                                                                       :style {:opacity 0.5}
                                                                       :child "key depth"]
                                                                      [:box :child (cstr/join " x " (map #(-> % edn/read-string ut/nf str)
                                                                                                         (cstr/split (last (cstr/split e #" ")) #"x")))]]]
                                                                    :else e)]]
                                                      [:box
                                                       :child e]))]])]))]

    [:box :style {:font-size "11px"} :child (str sample-error)]
    [:box :style {:font-size "11px" :opacity 0.4} :child (str solver-name)]]])

;; (defn parse-text-and-code [input]
;;   (let [input-str (if (vector? input) (cstr/join "\n" input) input)
;;         parts (cstr/split input-str #"(?s)```")
;;         process-part (fn [part]
;;                        (let [trimmed (cstr/trim part)]
;;                          (if (cstr/starts-with? trimmed "{")
;;                            ; Code section
;;                            (try
;;                              (edn/read-string trimmed)
;;                              (catch Exception e
;;                                (println "Warning: Failed to parse code section:" (.getMessage e))
;;                                trimmed))
;;                            ; Text section
;;                            trimmed)))]
;;     (mapv process-part parts)))

(defn parse-text-and-code [input]
  (let [input (walk/postwalk-replace (into {} (for [f (filter #(cstr/starts-with? (str %) "```") input)] {f "```"})) input)
        input-str (if (vector? input) (cstr/join "\n" input) input)
        parts (cstr/split input-str #"(?s)```")
        remove-comments (fn [code-str]
                          (-> code-str
                              ; Remove full-line comments
                              (cstr/replace #"(?m)^\s*;.*$" "")
                              ; Remove inline comments
                              (cstr/replace #"(?m);.*$" "")
                              ; Remove empty lines
                              (cstr/replace #"(?m)^\s*$\n" "")
                              cstr/trim))
        process-part (fn [part]
                       (let [trimmed (cstr/trim part)]
                         (if (cstr/starts-with? trimmed "{")
                           ; Code section
                           (try
                             (-> trimmed
                                 remove-comments
                                 edn/read-string)
                             (catch Exception e
                               (println "Warning: Failed to parse code section:" (.getMessage e))
                               trimmed))
                           ; Text section
                           trimmed)))]
    (mapv process-part parts)))


(defn fabric-post-process [client-name fabric-opts-map output elapsed-ms ui-keypath is-history?]
  (let [{:keys [pattern id input model context]} fabric-opts-map
        ;; code-proc? (and (=  pattern "clover-canvas")
        ;;                 (cstr/includes? (str output) "```"))
        code-proc?  (=  pattern "clover-canvas")
        kit-content (try (edn/read-string (cstr/join "/n" (flatten output))) (catch Exception e {:error (str e)}))
        kit-out {(keyword model) {:data [{:name (str client-name " request")
                                          :content [[:v-box :size "auto"
                                                     :children [[:box :size "auto"
                                                                 :child (str (ut/millis-to-date-string (- (System/currentTimeMillis) elapsed-ms)))]
                                                                [:box :size "auto"
                                                                 :child (str input)]]]]}
                                         {:name (str model " reply")
                                          :content [[:v-box :size "auto"
                                                     :children [[:box :size "auto"
                                                                 :child (str (ut/millis-to-date-string (System/currentTimeMillis)))]
                                                                [:box :size "auto"
                                                                 :child (str output)]]]]}]}}]
    (ut/pp  [:fabric-post-process client-name fabric-opts-map elapsed-ms is-history?])
    ;;(Thread/sleep 1000) ;; testing alert collisions
    (when (not is-history?)
      (alert! client-name
              [:v-box
               :justify :center
             ;:style {:opacity 0.7}
               :children
               [[:box
                 :style {:color :theme/editor-outer-rim-color
                         :font-weight 700}
                 :child
                 [:v-box
                  :size "auto"
                  :gap "10px"
                  :children [[:box
                              :style {:font-size "16px"}
                              :child (str "Fabric has completed it's '" pattern "' pattern on " model ".")]
                             [:box
                              :style {:font-size "14px"}
                              :width (str (* 16 50) "px") ;; alert size from other arg X "brick size" in UI
                              ;; :child (str (if (vector? output)
                              ;;               (cstr/join "\n" output)
                              ;;               output))
                              ;;:child [:speak (cstr/join " " (filter string? (parse-text-and-code output)))]
                              :child (cstr/join " " (filter string? (parse-text-and-code output)))
                              ;:child (str (keys kit-content) " modded")
                              ]
                            ;;  (try
                            ;;    ;;(when code-proc? [:execute (into {} (for [[k v] (apply merge (filter map? kit-content))] {k (edn/read-string v)}))])
                            ;;    (when (and (not (get kit-content :error)) code-proc?) [:execute kit-content])
                            ;;    (catch Exception _ nil))
                             ]]]]]
              16
              nil
              20 :fabric-response)

      (insert-kit-data kit-out
                       (hash kit-out)
                       (last ui-keypath)
                       "solver-log"  ;; thread name
                          ;;(str "solver-run" (cstr/join " " (rest ui-keypath))) ;; header title
                       :kick ;;(last ui-keypath)
                       elapsed-ms
                       client-name)
      (when code-proc?
        (let [step-maps (apply merge (filter map? kit-content))
              target    (last (ffirst step-maps))
              kit-out   {(keyword model) {:data  [{:name (first (cstr/split (cstr/join " " (filterv string? (parse-text-and-code (str input)))) #" ##SPECIAL BLOCK TYPE INSTRUCTIONS:")) ;; (filterv string (parse-text-and-code (str input)))
                                                   :step-mutates (into {} (for [[kp v] context] {kp (edn/read-string v)})) ;; was stringified on the FE to protect from transfer issues, but now we need it back as a struct
                                                   :content (into ["(context during req)"] (vec (vals context)))}
                                                  {:name (str model " response")
                                                   ;;:step-mutates step-maps
                                                   :step-mutates (when (and (not (get kit-content :error)) code-proc?) [:execute kit-content])
                                                   :content [:edn (str (keys kit-content) " modded")]}]}}]
          (ut/pp [:kit-response step-maps target context kit-out])
          (insert-kit-data kit-out
                           (hash kit-out)
                           target
                           "solver-log"  ;; thread name
                           :kick ;;(last ui-keypath)
                           elapsed-ms
                           client-name))))))

(defn ship-estimate [client-name solver-name times-key]
  (try (let [prev-times       (get @times-atom times-key [-1])
             times (ut/avg ;; avg based on last 10 runs, but only if >
                    (vec (take-last 10 (vec (remove #(< % 1) prev-times)))))]
         (when (not (nil? times))
           (kick client-name [:estimate] {solver-name ;;(cstr/replace (str solver-name) ":" "solver/")
                                          {:times (/ times 1000)
                                           :run-id solver-name}}
                 nil nil nil)))
       (catch Exception e (ut/pp [:error-shipping-estimates (Throwable->map e) (str e)]) 0)))

(defn nrepl-solver-run [vdata client-name solver-name timestamp-str runner-map runner-name runner-type cache-hit? use-cache? is-history? cache-key ui-keypath]
  (try (let [repl-host                   (get-in runner-map [:runner :host])
             repl-port                   (get-in runner-map [:runner :port])
             is-fabric?                  (cstr/includes? (str vdata) "fabric-run")
            ;;  _ (ut/pp [:nrepl-call runner-name is-fabric? solver-name client-name])
             pre-opts-map                (when is-fabric? (ut/extract-map vdata #{:pattern :model}))
             times-key                   (if is-fabric?
                                           (vec (flatten (remove symbol? (select-keys pre-opts-map [:pattern :model]))))
                                           (into [:nrepl-solver] ui-keypath))
             _                           (ship-estimate client-name solver-name times-key)
             {:keys [result elapsed-ms]} (ut/timed-exec
                                          (ppy/execute-in-thread-pools-but-deliver (keyword (str "serial-nrepl-instance/" (cstr/replace (str client-name "-" solver-name) ":" "")))
                                                                                   (fn []
                                                                                     (evl/repl-eval vdata repl-host repl-port client-name solver-name ui-keypath))))
            ;;  {:keys [result elapsed-ms]} (ut/timed-exec
            ;;                               (ppy/execute-in-thread-pools-but-deliver :nrepl-solver-pool
            ;;                                                            (fn []
            ;;                                                              (evl/repl-eval vdata repl-host repl-port client-name solver-name ui-keypath))))
             output-full                 result
             sampled?                    (get-in output-full [:sampled :sampling-details])
             output                      (if sampled?
                                           (get-in output-full [:sampled :data])
                                           (last (get-in output-full [:evald-result :value])))
             output                      (if (nil? output) "(returns nil value)" output)
             fabric-opts-map             (when is-fabric? (last output))
             output                      (if is-fabric? (first output) output)
             _                           (ext/write-transit-data output (last ui-keypath) client-name (ut/unkeyword (last ui-keypath))) ;; for :data/usage
             ;;_                           (swap! db/last-solvers-data-atom assoc-in [(last ui-keypath)] output)
             _                           (ext/write-transit-data output (last ui-keypath) client-name (cstr/replace (str solver-name (last ui-keypath) client-name) ":" ""))
             ;;_ (when (cstr/includes? (str client-name) "square") (ut/pp [:writing-nrepl-data [(last ui-keypath)] output]))

             ;;_ (when is-fabric? (ut/pp [:fabric fabric-opts-map output]))
             output-full                 (-> output-full
                                             (assoc-in [:evald-result :output-lines]
                                                       (try (count (remove empty?
                                                                           (get-in output-full [:evald-result :out])))
                                                            (catch Exception _ 0)))
                                             (assoc-in [:evald-result :values]
                                                       (try (count (last (get-in output-full [:evald-result :value])))
                                                            (catch Exception _ 0))))
             error?                      (err? output-full)
             _                          (when (not error?) (swap! times-atom assoc times-key (conj (get @times-atom times-key) elapsed-ms)))
             runs                        (get @db/last-solvers-history-atom solver-name [])
             meta-extra                  {:extra {:last-processed timestamp-str
                                                  :cache-hit?     cache-hit?
                                                  :elapsed-ms     elapsed-ms
                                                  :runs           (get @db/last-solvers-history-counts-atom solver-name) ;; (count runs)
                                                  :error?         error?}}
             timestamp-str               (str timestamp-str " (" elapsed-ms "ms)")
             new-history                 (vec (conj runs timestamp-str))
             output                      (if error?
                                           (select-keys (get output-full :evald-result) [:root-ex :ex :err])
                                           output)
             sqlized?                    (ut/ne? (get result :sqlized))

             _ (when sqlized?
                 (ppy/execute-in-thread-pools :sqlize-repl-rowset-push ;; dont want to delay the natural value returns
                                              (fn [] (let [query-map (get result :sqlized)]
                                                       (ut/pp [:sqlized (get result :sqlized)])
                                                       (ut/delay-execution 6000
                                                                           (do (ut/pp [:sqlized query-map])
                                                                               (push-to-client ui-keypath [] client-name 2 :new-slice (into [:queries] query-map))))))))

             kit-out {:tester {:data
                               [{:ask-mutates {"Highlight these in your source query?"
                                               {[:panels :block-984 :queries :OFFENSE-CODE-GROUP-drag-40 :style-rules [:* :higlight-1584150552]]
                                                {:logic [:and [:= :DISTRICT "B2"]]
                                                 :style {:background-color "#008b8b66" :border "1px solid #00000000"}}}}
                                 :content [[:v-box :size "auto" :width "490px" :style {:font-size "13px" :opacity 0.33} :children
                                            [[:h-box :size "auto" :justify :between :children
                                              [[:box :child ":mean"] [:box :child "1185.7693" :style {:font-weight 400}]]]
                                             [:h-box :size "auto" :justify :between :children
                                              [[:box :child ":standard-deviation"] [:box :child "713.1985540610642" :style {:font-weight 400}]]]
                                             [:h-box :size "auto" :justify :between :children
                                              [[:box :child ":sample-size"] [:box :child "13" :style {:font-weight 400}]]]
                                             [:h-box :size "auto" :justify :between :children
                                              [[:box :child ":fields"] [:box :child "[:DISTRICT]" :style {:font-weight 400}]]]
                                             [:h-box :size "auto" :justify :between :children
                                              [[:box :child ":calc-used"] [:box :child "[:percentiles [0.01 0.99]]" :style {:font-weight 400}]]]]]
                                           [:v-box :size "auto" :width "490px" :style {:font-size "16px"} :children
                                            [[:h-box :size "auto" :justify :between :children
                                              [[:box :child ":DISTRICT"] [:box :child "\"B2\"" :style {:font-weight 400}]]]]] "as a wider aggregate:"
                                           {:_h            4
                                            :_query-id     :kit-query--15841505520-agg
                                            :connection-id "boston-crime"
                                            :from          [:query/OFFENSE-CODE-GROUP-drag-40]
                                            :group-by      [:DISTRICT]
                                            :order-by      [[2 :desc]]
                                            :select        [:DISTRICT [[:count 1] :rows]]
                                            :style-rules   {[:* :highlight-group] {:logic [:and [:= :DISTRICT "B2"]]
                                                                                   :style {:background-color "#008b8b66"
                                                                                           :border           "1px solid #00000000"}}}}
                                           "ALL rows in this group:"
                                           {:_h            8
                                            :_query-id     :kit-query--15841505520-detail
                                            :connection-id "boston-crime"
                                            :from          [:query/OFFENSE-CODE-GROUP-drag-40]
                                            :select        [:*]
                                            :where         [:*if :kit-query--15841505520-agg/*.clicked
                                                            [:*all= :kit-query--15841505520-agg/*.clicked [:DISTRICT]] [:and [:= :DISTRICT "B2"]]]}]
                                 :highlight-columns {:OFFENSE-CODE-GROUP-drag-40 [:DISTRICT]}
                                 :name "When grouping by district and aggregating by :rows there are values that are higher than the mean (1185 for this particular aggregation) by 1250."
                                 :order 0
                                 :parameters {:test-param123 ["fart fx service"]}
                                 :step-mutates {[:panels :block-5394 :views :hare-vw-4]
                                                [:box
                                                 :align
                                                 :center
                                                 :justify
                                                 :center
                                                 :style
                                                 {:font-size   (str (rand-int 123) "px")
                                                  :font-weight  700
                                                  :padding-top  "6px"
                                                  :padding-left "14px"
                                                  :margin-top   "-8px"
                                                  :color        :theme/editor-outer-rim-color
                                                  :font-family  :theme/base-font}
                                                 :child
                                                 "YO. Have I gone mad? I'm afraid so, but let me tell you something, the best people usually are."]}}]}}]

         (when is-fabric? (fabric-post-process client-name fabric-opts-map output elapsed-ms ui-keypath is-history?))

         (when sampled?
           (try
             (let [sample-message (get-in output-full [:sampled :message] "")
                   sample-message-lines (cstr/split sample-message #"\. ")
                   ;;_ (ut/pp [:sample-message sample-message-lines])
                   sample-error   (get-in output-full [:sampled :error] "")
                   sample-details (get-in output-full [:sampled :sampling-details] {})]
               (alert! client-name
                       (sample-alert-clover sample-message-lines sample-error solver-name)
                       13
                       nil ;2.1
                       (if sample-details 8 20) :sample-detail))
             (catch Exception  e (ut/pp [:solver-repl-alert-error (str e) e]))))

         (swap! db/last-solvers-atom assoc-in [solver-name] output)
        ;;  (ut/pp [:last-solvers-atom-assoc client-name solver-name])
         (swap! db/last-solvers-atom-meta assoc-in [solver-name]
                (merge meta-extra {:history (vec (reverse (take-last 20 new-history)))
                                   :error "none"
                                   :output (ut/dissoc-in output-full [:evald-result :value]) ;;(ut/limit-sample output-full) ;;; replace with sampler now?
                                   }))
         (swap! db/last-solvers-history-atom assoc solver-name (vec (take 100 new-history)))
         (swap! db/last-solvers-history-counts-atom update solver-name (fnil inc 0))
        ;;;disable-cache;;(swap! solvers-cache-atom assoc cache-key [output output-full])
         (when (not error?) ;;(and use-cache? (not error?))
           (swap! solvers-cache-atom assoc cache-key [output output-full]))
         (swap! db/solver-status update-in [client-name solver-name] merge {:running? false, :stopped (System/currentTimeMillis)})
         (when (vector? ui-keypath) (swap! db/client-panels-data assoc-in (into [client-name] ui-keypath) output)) ;; <--- pricey, but its usefulness outweighs the memory ATM, external db, Redis, etc later.

        ;;  (kick  client-name "kick-solver!" (last ui-keypath)
        ;;         "solver-log"  ;; thread name
        ;;         (str "solver-run" (cstr/join " " (rest ui-keypath))) ;; header title
        ;;         (str "query-log-" (first ui-keypath))
        ;;         ;[(str (ut/get-current-timestamp) " - solver ran in " elapsed-ms " ms.")]



        ;;         )

        ;;  (insert-kit-data kit-out
        ;;                   (hash kit-out )
        ;;                   (last ui-keypath)
        ;;                   "solver-log"  ;; thread name
        ;;                   ;;(str "solver-run" (cstr/join " " (rest ui-keypath))) ;; header title
        ;;                   :kick ;;(last ui-keypath)
        ;;                   elapsed-ms
        ;;                   client-name
        ;;                   )
         ;;(insert-kit-data payload (hash payload) sub-task task-id ui-keypath 0 client-name "flow-id-here!"))

         output)

       (catch Throwable e
         (do (ut/pp [:SOLVER-REPL-ERROR!!! (str e) :tried vdata :for solver-name :runner-type runner-type])
                     ;(swap! solvers-running assoc-in [client-name solver-name] false)
             (swap! db/solver-status update-in [client-name solver-name]
                    (fn [solver]
                      (-> solver
                          (assoc :stopped (System/currentTimeMillis))
                          (assoc :running? false)
                          (assoc :error? true)
                          (assoc :time-running (ut/format-duration
                                                (:started solver)
                                                (System/currentTimeMillis))))))
             (swap! db/last-solvers-atom-meta assoc-in [solver-name] {:error (str e)})
             (swap! db/last-solvers-atom assoc-in [solver-name] {:error (str e)})))))

;;      ;;kick [client-name task-id       sub-task           thread-id     thread-desc                           message-name & args]                    payload-vec
;;      (kick  client-name "kick-solver!" (last ui-keypath) "solver-log"  (str "query-log-" (first ui-keypath)) (str "query-log-" (first ui-keypath))  [(str (ut/get-current-timestamp) " - query ran in " -1 " ms.")])

;;  (ut/pp (keys @client-panels))
;;  (ut/pp (keys @client-panels-data))

(defn running-elsewhere? [temp-solver-name]
  (and temp-solver-name
       (or
        (get-in (flow-statuses) [temp-solver-name :*running?])
        ;; ^^ is it a flow that is already started?
        (some (fn [[_ solvers]]
                (some (fn [[solver-name solver-data]]
                        (and (= solver-name temp-solver-name)
                             (:running? solver-data)))
                      solvers))
              @db/solver-status))
       ;; ^^ is it running from another client?
       ))

;; (reset! solvers-cache-atom {})

(defn run-solver
  [solver-name client-name & [override-map override-input temp-solver-name ui-keypath]]
  (let [temp-running-elsewhere? (atom (running-elsewhere? temp-solver-name))
        client-name (if (nil? client-name) :rvbbit client-name)
        _                     (swap! solvers-run inc)
        timestamp             (System/currentTimeMillis)
        data-serial? (ut/ne? (filter #(cstr/starts-with? (str %) ":data/")
                                     (ut/deep-flatten (get override-map :data))))
        ;; ^^ if we have a :data/* to populate we should go into the same serial channel as it's writer.... else we might try to fetch before it's written
        _ (when data-serial? (Thread/sleep 300)) ;; slight hiccup to allow the data thread to start (just in case the render evals before the query)
        ;; _ (ut/pp  [:solver-run ui-keypath solver-name temp-solver-name client-name @temp-running-elsewhere? ])
        ]

  (if @temp-running-elsewhere?

    ;(when @temp-running-elsewhere?
        ;; pretend we started while we wait for someone else to finish the work
      (let [vdata0 (get override-map :data) ;;[(get override-map :data) override-input]
            vdata0 (if (> (count (str vdata0)) 60)
                     (subs (str vdata0) 0 60)
                     (str vdata0))]
        (swap! db/solver-status assoc-in [client-name solver-name]
               {:started timestamp
                :stopped nil
                :running? false ;true
                :waiting? true
                :time-running ""
                :ref (str "waiting: " vdata0)})

        ;; (ut/pp [:*waiting!!! client-name :for-another temp-solver-name  ])

      (loop []
        (reset! temp-running-elsewhere? (running-elsewhere? temp-solver-name))
        (when @temp-running-elsewhere?
          (Thread/sleep 1500) ;; wait a sec...
          (recur)))
        ;; when done, celebrate and pretend we did it!
      (swap! db/solver-status update-in [client-name solver-name]
             (fn [solver]
               (-> solver
                   (assoc :stopped (System/currentTimeMillis))
                   (assoc :waiting? false)
                   (assoc :cache-served? true)
                   (assoc :time-running (ut/format-duration
                                         (:started solver)
                                         (System/currentTimeMillis))))))
      ;; (ut/delay-execution ;; we shouldnt have to do this, atom watcher reactions SHOULD be enough.
      ;;  1800
      ;;  (let [vv (get @last-solvers-atom temp-solver-name)]
      ;;    (when (not (nil? vv))
      ;;      (kick client-name [:solver temp-solver-name] (get @last-solvers-atom temp-solver-name) nil nil nil))))
      )



    (ppy/execute-in-thread-pools
     (if data-serial?
       (keyword (str "serial-" client-name "data-ops"))
         :solver-runner)

     (fn []
       (let [ _ (when data-serial? (ut/pp [:data-serial-RESOLVE client-name solver-name]))
             solver-map            (if (ut/ne? override-map) override-map (get @solvers-atom solver-name))
             input-map             (get solver-map :input-map {})
             input-map             (if (ut/ne? override-input) (merge input-map override-input) input-map)
             is-history?           (true? (get solver-map :history? false))
             use-cache?            (or is-history? (true? (get solver-map :cache? false)))
          ;; use-cache?            (true?
          ;;                        (or is-history?
          ;;                            (true? (not (nil? temp-solver-name))) ;; temp test
          ;;                            (true? (get solver-map :cache? false))))
          ;; use-cache? false
             vdata                 (walk/postwalk-replace input-map (get solver-map :data))
             vdata-clover-kps      (vec (filter #(and (keyword? %) ;; get any resolvable keys in the struct before we operate on
                                                              ;; it
                                                      (cstr/includes? (str %) "/")
                                                      (not= % (keyword (str "solver/" (cstr/replace (str solver-name) ":" "")))))
                                                (ut/deep-flatten vdata)))

             solver-name           (if temp-solver-name ;; we might be passed a blank input-map, but as long as we have
                                                   ;; a temp-solver-name, we can still run it independently
                                     temp-solver-name
                                     solver-name)
             runner-name           (get solver-map :type :clojure)
             runner-map            (get-in (config/settings) [:runners runner-name] {})
             runner-type           (get runner-map :type runner-name)
             vdata-clover-walk-map (into {}
                                         (for [kp vdata-clover-kps]
                                           {kp (if (and (= runner-type :nrepl)
                                                        (cstr/starts-with? (str kp) ":data/"))
                                                 (try
                                                   (let [pkp (db/parse-coded-keypath kp)
                                                         extra-kp (vec (drop 2 pkp))
                                                         base-query-key (get pkp 1)
                                                         t-file (get-in @ext/transit-file-mapping [client-name base-query-key :file])
                                                         raw-val (get-in @db/last-solvers-data-atom [base-query-key])
                                                         raw-val-str (pr-str raw-val)
                                                         raw-val-size (count raw-val-str)
                                                         size-threshold 1000 ;; chars
                                                      ;; _ (ut/pp [:vdata-sub :raw-val-size raw-val-size :kp kp :extra-kp extra-kp :cid client-name
                                                      ;;           :base-query-key base-query-key :raw-val-str raw-val-str])
                                                         ccode (cond (and raw-val (<= raw-val-size size-threshold)) ;; If raw-val is small enough, use it directly
                                                                     raw-val

                                                                     (and raw-val (= (get-in runner-map [:runner :port]) 8181)) ;; if not small, but we are in the same repl, send the atom deref
                                                                     `(get-in @rvbbit-backend.db/last-solvers-data-atom [~base-query-key])

                                                                    ;; otherwise, load it from local disk
                                                                     :else `(do (println ~t-file (System/currentTimeMillis))
                                                                                (get-in (cognitect.transit/read
                                                                                         (cognitect.transit/reader
                                                                                          (clojure.java.io/input-stream
                                                                                           ~t-file)
                                                                                          :msgpack)) ~extra-kp)))]
                                                  ;; (ut/pp [:t-file t-file ui-keypath kp base-query-key ccode pkp])
                                                     ccode)
                                                   (catch Exception e
                                                     (ut/pp [:nrepl-post-walk-erroor e])
                                                     (throw e)))
                                                 (try (db/clover-lookup :rvbbit kp)
                                                      (catch Exception e
                                                        (ut/pp [:clover-lookup-error kp e])
                                                        (str "clover-param-lookup-error " kp))))}))
          ;;  _ (when (= client-name :terrific-mauve-falcon-18)
          ;;      (ut/pp [:solver-clover override-map input-map ui-keypath vdata vdata-clover-kps vdata-clover-walk-map vdata (get solver-map :data)]))
          ;; prev-times       (get @times-atom solver-name [-1])
          ;; ship-est         (fn [client-name]
          ;;                    (try (let [times (ut/avg ;; avg based on last 10 runs, but only if >
          ;;                                      (vec (take-last 10 (vec (remove #(< % 1) prev-times)))))]
          ;;                           (when (not (nil? times))
          ;;                             (kick client-name [:estimate] {solver-name {:times times :run-id solver-name}} nil nil nil)))
          ;;                         (catch Exception e (ut/pp [:error-shipping-estimates (Throwable->map e) (str e)]) 0)))
          ;; _ (ship-est client-name)

          ;; vdata                 (if (and (= runner-type :nrepl) (string? vdata))
          ;;                         (edn/read-string vdata) vdata)
             vdata                 (if (ut/ne? vdata-clover-walk-map) (walk/postwalk-replace vdata-clover-walk-map vdata) vdata)

          ;;cache-key             (pr-str [solver-name vdata runner-name])
             sanitized-req         (ut/deep-remove-keys ;; only for cache uniqueness
                                    (ut/lists-to-vectors [solver-map input-map vdata runner-name])
                                    [:id :history? :cache?])
             cache-key             (hash sanitized-req) ;; mostly for fabric and history cache forcing
          ;; _ (when (cstr/includes? (str client-name) "powerful") (ut/pp [solver-name client-name cache-key sanitized-req]))
             cache-val             (when use-cache? (get @solvers-cache-atom cache-key))
             cache-hit?            (true?  (and (and use-cache?
                                                     (ut/ne? cache-val)
                                          ;;(lookup-scache-exists? cache-key)
                                                     )
                                                (not= runner-type :sql)
                                     ;(not= runner-type :nrepl)
                                                ))
          ;; err?                  (fn [s]
          ;;                         (let [s (str s)]
          ;;                           (or (cstr/includes? s "Exception") (cstr/includes? s ":err") (cstr/includes? s ":error"))))
             timestamp-str         (cstr/trim (str (when use-cache? "^") (when cache-hit? "*") " " (millis-to-date timestamp)))
             vdata-ref             (if (> (count (str vdata)) 60)
                                     (subs (str vdata) 0 60)
                                     (str vdata))]

         (swap! db/solver-status assoc-in [client-name solver-name]
                {:started timestamp
                 :stopped nil
                 :running? true
                 :time-running ""
                 :ref vdata-ref})

      ;; (ut/pp [:*running!!! use-cache? cache-hit? client-name :for temp-solver-name vdata-ref])
    ;; (when override-input (ut/pp [:SOLVER-INPUT-OVERRIDE! temp-solver-name :wants input-map]))
    ;;(when override-map (ut/pp [:SOLVER-MAP-OVERRIDE! temp-solver-name :wants input-map]))
         (cond
           cache-hit? (let [[output output-full] cache-val
                            meta-extra           {:extra {:last-processed timestamp-str :cache-hit? cache-hit? :elapsed-ms 0}}
                            timestamp-str        (str timestamp-str " (cache hit)")
                            new-history          (vec (conj (get @db/last-solvers-history-atom solver-name []) timestamp-str))] ;; regardless
                    ;;  (ut/pp [:cached-solver-hit! solver-name])
                        (swap! db/last-solvers-atom assoc-in [solver-name] output)
                        (swap! db/last-solvers-atom-meta assoc-in [solver-name]
                               (merge meta-extra {:history (vec (reverse (take-last 20 new-history))) :error "none"
                                                  :output (ut/limit-sample output-full)}))
                        (swap! db/last-solvers-history-atom assoc solver-name (vec (take 100 new-history)))
                        (swap! db/last-solvers-history-counts-atom update solver-name (fnil inc 0))
                    ;;  (ut/pp [:*cacheddd!!! client-name :for temp-solver-name vdata-ref])
                        (swap! solvers-cache-hits-atom update cache-key (fnil inc 0))
                     ;(swap! solver-status assoc-in [client-name solver-name :running?] false)
                     ;(swap! solver-status assoc-in [client-name solver-name :cache-served?] true)
                     ;(swap! solver-status assoc-in [client-name solver-name :stopped] (System/currentTimeMillis))
                        (swap! db/solver-status update-in [client-name solver-name] merge {:running? false, :cache-served? true, :stopped (System/currentTimeMillis)})
                        (when (vector? ui-keypath) (swap! db/client-panels-data assoc-in (into [client-name] ui-keypath) output)) ;; <--- pricey, but its usefulness outweighs the memory ATM, external db, Redis, etc later.
                        output)

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
                   runs                        (get @db/last-solvers-history-atom solver-name [])
                   meta-extra                  {:extra {:last-processed timestamp-str
                                                        :cache-hit?     cache-hit?
                                                        :elapsed-ms     elapsed-ms
                                                        :runs           (get @db/last-solvers-history-counts-atom solver-name) ;; (count runs)
                                                        :error?         error?}}
                   timestamp-str               (str timestamp-str " (" elapsed-ms "ms, " rows " rows)")
                   new-history                 (vec (conj runs timestamp-str))]
               (ut/pp [:running-sql-solver solver-name :data-key data-key])
               (swap! db/last-solvers-atom assoc-in [solver-name] test-query-sql)
            ;(swap! last-solvers-data-atom assoc solver-name output) ;; full data can be clover
               (swap! db/last-solvers-atom-meta assoc-in [solver-name]
                      (merge meta-extra {:history (vec (reverse (take-last 20 new-history))) :error "none"
                                         :output (ut/limit-sample output-full)}))
               (swap! db/last-solvers-history-atom assoc solver-name (vec (take 100 new-history)))
               (swap! db/last-solvers-history-counts-atom update solver-name (fnil inc 0))
               (when (not error?) ;(and use-cache? (not error?))
                 (swap! solvers-cache-atom assoc cache-key [output output-full]))
            ;;;(swap! solvers-cache-atom assoc cache-key [output output-full])
            ;(swap! solvers-running assoc-in [client-name solver-name] false)
            ;(swap! solver-status assoc-in [client-name solver-name :running?] false)
            ;(swap! solver-status assoc-in [client-name solver-name :stopped] (System/currentTimeMillis))
               (swap! db/solver-status update-in [client-name solver-name] merge {:running? false, :stopped (System/currentTimeMillis)})
               (when (vector? ui-keypath) (swap! db/client-panels-data assoc-in (into [client-name] ui-keypath) rows)) ;; <--- pricey, but its usefulness outweighs the memory ATM, external db, Redis, etc later.
               rows)

             (catch Throwable e
               (do (ut/pp [:SOLVER-SQL-ERROR!!! (str e) :tried vdata :for solver-name :runner-type runner-type])
                ;(swap! solvers-running assoc-in [client-name solver-name] false)
                   (swap! db/solver-status update-in [client-name solver-name]
                          (fn [solver]
                            (-> solver
                                (assoc :stopped (System/currentTimeMillis))
                                (assoc :running? false)
                                (assoc :error? true)
                                (assoc :time-running (ut/format-duration
                                                      (:started solver)
                                                      (System/currentTimeMillis))))))
                   (swap! db/last-solvers-atom-meta assoc-in [solver-name] {:error (str e)}))))



           (= runner-type :nrepl)
           (nrepl-solver-run vdata client-name solver-name timestamp-str runner-map runner-name runner-type cache-hit? use-cache? is-history? cache-key ui-keypath)

           (= runner-type :flow) ;; no runner def needed for anon flow pulls
           (try
             (let [client-name                 (or client-name :rvbbit)
                ;client-name                 (keyword (str (cstr/replace (str client-name) ":" "") ".via-solver"))
                   flowmap                     (get vdata :flowmap)
                   opts                        (merge {:close-on-done? true
                                                       :increment-id? false
                                                       :orig-flow-id flowmap
                                                       :timeout 10000}
                                                      (get vdata :opts {}))
                   return                      (get vdata :return) ;; alternate block-id to fetch
                   flow-id                     (str (cstr/replace (str solver-name) ":" "") "-solver-flow-")
                   ;;_ (ut/pp [:solver-flow-start solver-name flow-id opts])
                   {:keys [result elapsed-ms]} (ut/timed-exec (flow! client-name flowmap nil flow-id opts))
                   ;;_ (ut/pp [:solver-flow-finsihed solver-name flow-id])
                   output                      result
                   output                      (ut/remove-namespaced-keys (ut/replace-large-base64 output))
                   output-val                  (get-in output [:return-val])
                   output-val                  (if return ;;(keyword? return)
                                                 (get-in output [:return-maps flow-id return] output-val)
                                                 output-val)
                   output-val                  (if (nil? output-val) "(returns nil value)" output-val)
                  ;;  _ (ut/pp [:solver-flow-return-val solver-name flow-id output-val])
                   output-full                 {:req        vdata
                                                :value      (-> (assoc output :return-maps
                                                                       (select-keys (get output :return-maps) [flow-id]))
                                                                (dissoc :tracker)
                                                                (dissoc :source-map) ;; dont need if we have SQL + flow history UI logs
                                                                (dissoc :tracker-history))
                                                :output-val output-val
                                                :flow-id    flow-id
                                                :return     return}
                   error?                      (err? output-full)
                   runs                        (get @db/last-solvers-history-atom solver-name [])
                   meta-extra                  {:extra {:last-processed timestamp-str
                                                        :cache-hit?     cache-hit?
                                                        :elapsed-ms     elapsed-ms
                                                        :runs           (get @db/last-solvers-history-counts-atom solver-name) ;; (count runs)
                                                        :error?         error?}}
                   timestamp-str               (str timestamp-str " (" elapsed-ms "ms)")
                   new-history                 (conj runs timestamp-str)]
               (do
                 (swap! db/last-solvers-atom assoc-in [solver-name] output-val)
                 (swap! db/last-solvers-atom-meta assoc-in [solver-name]
                        (merge meta-extra {:history (vec (reverse (take-last 20 new-history))) :error "none"
                                           :output (ut/limit-sample output-full)}))
                 (swap! db/last-solvers-history-atom assoc solver-name (vec (take 100 new-history)))
                 (swap! db/last-solvers-history-counts-atom update solver-name (fnil inc 0))

                 (when (not error?) ;; (and use-cache? (not error?))
                   (swap! solvers-cache-atom assoc cache-key [output-val output-full]))

                 (swap! flow-db/results-atom dissoc flow-id)  ;; <-- clear out the flow atom
               ;(swap! solvers-running assoc-in [client-name solver-name] false)
              ;(swap! solver-status assoc-in [client-name solver-name :running?] false)
              ;(swap! solver-status assoc-in [client-name solver-name :mid?] true)
              ;(swap! solver-status assoc-in [client-name solver-name :stopped] (System/currentTimeMillis))
                 (swap! db/solver-status update-in [client-name solver-name] merge {:running? false, :mid? true, :stopped (System/currentTimeMillis)}))
              ;;  (swap! solver-status update-in [client-name solver-name]
              ;;         (fn [solver]
              ;;           (-> solver
              ;;               (assoc :stopped (System/currentTimeMillis))
              ;;               (assoc :running? false)
              ;;               (assoc :time-running (ut/format-duration
              ;;                                     (:started solver)
              ;;                                     (System/currentTimeMillis))))))
               output-val)
             (catch Exception e
               (do (ut/pp [:SOLVER-FLOW-ERROR!!! (str e) e :tried vdata :for solver-name :runner-type runner-type])
                   ;(swap! solvers-running assoc-in [client-name solver-name] false)
                ;(swap! solver-status assoc-in [client-name solver-name :running?] false)
                ;(swap! solver-status assoc-in [client-name solver-name :error?] true)
                ;(swap! solver-status assoc-in [client-name solver-name :stopped] (System/currentTimeMillis))
                   (swap! db/solver-status update-in [client-name solver-name] merge {:running? false, :error? true, :stopped (System/currentTimeMillis)})
                   (swap! db/last-solvers-atom-meta assoc-in [solver-name] {:error (str e)})
                   (swap! db/last-solvers-atom assoc-in [solver-name] {:error (str e)}))))


           :else ;; else we assume it's just data and keep it as is
           (let [output-full   {:static-data vdata}
                 runs          (get @db/last-solvers-history-atom solver-name [])
                 meta-extra    {:extra {:last-processed timestamp-str
                                        :cache-hit? cache-hit?
                                        :elapsed-ms -1
                                        :runs (get @db/last-solvers-history-counts-atom solver-name)}}
                 timestamp-str (str timestamp-str " (static)")
                 new-history   (conj runs timestamp-str)]
             (ut/pp [:solver-static solver-name vdata])
             (swap! db/last-solvers-atom assoc-in [solver-name] vdata)
             (swap! db/last-solvers-atom-meta assoc-in [solver-name]
                    (merge meta-extra {:history (vec (reverse (take-last 20 new-history))) :error "none"
                                       :output (ut/limit-sample output-full)}))
             (swap! db/last-solvers-history-atom assoc solver-name (vec (take 100 new-history)))
             (swap! db/last-solvers-history-counts-atom update solver-name (fnil inc 0))
          ;(swap! solvers-running assoc-in [client-name solver-name] false)
          ;(swap! solver-status assoc-in [client-name solver-name :running?] false)
          ;(swap! solver-status assoc-in [client-name solver-name :stopped] (System/currentTimeMillis))
             (swap! db/solver-status update-in [client-name solver-name] merge {:running? false, :stopped (System/currentTimeMillis)})
             (when (vector? ui-keypath) (swap! db/client-panels-data assoc-in (into [client-name] ui-keypath) vdata)) ;; <--- pricey, but its usefulness outweighs the memory ATM, external db, Redis, etc later.
             vdata))

      ;; just in case?
      ;(swap! solver-status assoc-in [client-name solver-name :running?] false)
      ;(swap! solver-status assoc-in [client-name solver-name :stopped] (System/currentTimeMillis))
         (swap! db/solver-status update-in [client-name solver-name] merge {:running? false, :stopped (System/currentTimeMillis)})

      ;(swap! solvers-running assoc-in [client-name solver-name] false)
      ;; (swap! solver-status update-in [client-name solver-name]
      ;;        (fn [solver]
      ;;          (-> solver
      ;;              (assoc :stopped (System/currentTimeMillis))
      ;;              (assoc :running? false)
      ;;              (assoc :time-running (ut/format-duration
      ;;                                    (:started solver)
      ;;                                    (System/currentTimeMillis))))))
         ))))) )

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
                                                            v2       (get-in @db/last-signal-value-atom [sigk ss]) ;; should
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
                     vvals   (select-keys (get-in @db/atoms-and-watchers [:rvbbit]) cmpkeys)]
                 (into {}
                       (for [[k {:keys [base-type sub-type sub-path keypath]}] vvals
                             :let   [;;_ (when (not= base-type :time)
                                     ;;(ut/pp [:resolver-map!!? k
                                     v3 (get-in @(db/get-atom-from-keys base-type
                                                                        sub-type
                                                                        sub-path
                                                                        keypath)
                                                keypath
                                                ;(get @db/last-values keypath)
                                                (db/ddb-get "last-values" (hash keypath))
                                                )]]
                         {k v3})))}))
         client-name :rvbbit
         nowah (System/currentTimeMillis) ;; want to have a consistent timestamp for this
         signals-resolved (into {}
                                (for [[k v] signals-map
                                      :let [res-changed (resolve-changed-fn (get v :signal) k)]]
                                  {k (walk/postwalk-replace (get signals-resolve-map k)
                                                            res-changed)}))
         parts-work
         (into
          {} ;; important side effects / will transition to a doseq after debugging phase
          (for [[kk vv] signals-parts-map]
            {kk (into
                 {}
                 (for [vvv  vv
                       :let [res-changed   (resolve-changed-fn vvv kk)
                             rvv           (walk/postwalk-replace (get signals-resolve-map kk) res-changed)
                             honey-sql-str (to-sql {:select [[1 :vv]] :where rvv})
                             result        (true? (ut/ne? (sql-query ghost-db honey-sql-str [:ghost-signal-resolve1 kk res-changed])))
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
                             _ (swap! db/last-signals-atom-stamp assoc vvv nowah)
                             _ (swap! db/last-signals-atom assoc vvv result)
                             _ (swap! db/last-signals-atom assoc part-key result)]] ;; last time
                   {[vvv rvv] result}))}))
         full-parts-work
         (into {} ;; all side effects / will transition to a doseq after debugging phase
               (for [[kk vv] signals-resolved
                     :let    [honey-sql-str (to-sql {:select [[1 :vv]] :where vv})
                              result        (true? (ut/ne? (sql-query ghost-db honey-sql-str [:ghost-signal-resolve2 kk vv])))
                              _ (when true ;(not= result (get @last-signals-atom kk))  ;; no
                                  (swap! last-signals-history-atom-temp assoc-in
                                         [kk kk]
                                         (into (vec (take-last 19
                                                               (sort-by second (get-in @last-signals-history-atom-temp [kk kk]))))
                                               [[result nowah]])))
                              _ (swap! db/last-signals-atom assoc kk result)
                              _ (when (and solver-dep? (true? result)) ;; singal resolved as
                                  (doseq [[sk sv] @solvers-atom
                                          :when   (or (= (get sv :signal) kk)
                                                      (= (get sv :signal)
                                                         (keyword (str "signal/" (cstr/replace (str kk) ":" "")))))]
                                    ;(enqueue-task4 (fn []
                                    (ppy/execute-in-thread-pools :signal-run-solvers ;; (keyword (str "signal/run-solver." (cstr/replace (str client-name) ":" "")))
                                    ;(qp/slot-queue :solvers-int sk ;;client-name
                                                                 (fn []
                                                                   (run-solver sk client-name)))))
                              _ (swap! db/last-signals-history-atom assoc kk (get @last-signals-history-atom-temp kk)) ;; one
                                                                                                                       ;; write,
                                                                                                                       ;; to
                              _ (swap! db/last-signals-atom-stamp assoc kk nowah)]]
                 {[kk vv] result}))]
     (doseq [[k v] signals-resolve-map] (doseq [[kk vv] v] (swap! db/last-signal-value-atom assoc-in [k kk] vv))) ;; when all
                                                                                                                ;; done, set
     )))

(def rvbbit-client-sub-values (atom {})) ;; the rvbbit "client app-db"
;;; (ut/pp @rvbbit-client-sub-values)

(defn process-signals-reaction
  [base-type keypath new-value client-param-path]
  (let [_             (swap! rvbbit-client-sub-values assoc-in keypath new-value)
        re-con-key    (keyword (str (cstr/replace (str base-type) ":" "") "/" (cstr/replace (str client-param-path) ":" "")))
        valid-signals (map first (vec (filter #(some (fn [x] (= x re-con-key)) (last %)) @signal-parts-atom)))]
    (swap! sent-signal-reactions inc)

    (doseq [signal valid-signals
            :let   [solver-deps    (vec (distinct (for [[_ v] @solvers-atom]
                                                    (keyword (-> (get v :signal)
                                                                 str
                                                                 (cstr/replace ":signal/" "")
                                                                 (cstr/replace ":" ""))))))
                    solver-to-run? (true? (some #(= signal %) solver-deps))]] ;; run in parallel
      (process-signal signal solver-to-run?))))

;; (declare process-solver)

;; (defn process-solvers-reaction ;; mostly uneeded. reg reactions are used here now
;;   [base-type keypath new-value client-param-path]
;;   (let [re-con-key    (keyword (str (cstr/replace (str base-type) ":" "") "/" (cstr/replace (str client-param-path) ":" "")))
;;         valid-signals (map first (vec (filter #(some (fn [x] (= x re-con-key)) (last %)) @signal-parts-atom)))]
;;     (doseq [signal valid-signals] ;; run in parallel later perhaps
;;       (process-solver signal))))


(defn send-reaction
  [base-type keypath client-name new-value & [extra]]
  (let [;;_ (ut/pp [:send-reaction-keypath keypath client-name])
        keypath                 (get @db/param-var-mapping [client-name keypath] keypath) ;; get the
        ;;flow-client-param-path  (keyword (cstr/replace (str (first keypath) (last keypath)) #":" ">"))
        other-client-param-path (keyword (cstr/replace (cstr/join ">" keypath) ":" "")) ;;(keyword
        ;;client-param-path       (if (= base-type :flow) flow-client-param-path other-client-param-path)
        client-param-path       other-client-param-path
        signal?                 (= client-name :rvbbit)]
    (swap! sent-reactions inc)

    (if (not signal?)
      (kick client-name [(or base-type :flow) client-param-path] new-value nil nil nil extra)
      (do ;;(ut/pp [:signal-or-solver base-type keypath client-name])
        (process-signals-reaction base-type keypath new-value client-param-path)))))


(defn send-reaction-runner
  [base-type keypath client-name new-value & [extra]]
  (let [flow-id (first keypath)]
    (ut/pp [:reaction-runner flow-id keypath client-name]) ;; (ut/replace-large-base64
    (kick client-name (vec (cons :flow-runner keypath)) new-value nil nil nil)))

(defn purge-dead-client-watchers []
  (try
    (let [cc (dissoc (client-statuses) :rvbbit)]
      (ut/pp [:purging-dead-clients...])
      (doseq [[k {:keys [last-seen-seconds]}] cc
              :let                            [subs (mapv first (get @db/atoms-and-watchers k))]
              :when                           (or (= last-seen-seconds -1)
                                                  (> last-seen-seconds 600))]
      ; unsub from all watchers before atom cleanup - or else the pools will keep getting created via reactions!
        (ut/pp [:dead-client :cleaning-up k :last-seen-seconds last-seen-seconds])
        (swap! client-metrics dissoc k)
        (doseq [s subs]
          (ut/pp [:trying-to-unsub k s])
          (unsub-value k s))
        (doseq [p (filter #(cstr/includes? (str %) (cstr/replace (str k) ":" "")) (keys @ppy/dyn-pools))]
          (ppy/close-cached-thread-pool p))
        ;; (let [conn (get @db/conn-map k)
        ;;       cn (get conn :datasource)]
        ;;   (ut/ppa [:shutting-down-connection-pool k cn])
        ;;   (sql/close-pool cn))
        (swap! db/atoms-and-watchers dissoc k) ;; remove client from watchers atom (should already be empty from unsub, but just in case)
        (swap! client-queues dissoc k)
        (swap! db/client-panels dissoc k)
        (swap! db/client-panels-data dissoc k)
        (swap! db/client-panels-metadata dissoc k)
        ;(swap! db/shapes-result-map dissoc k)
        ;; (swap! client-panels-history dissoc k)
        (swap! db/ack-scoreboard dissoc k)))
    (catch Exception e (ut/pp [:purge-client-queues-error e]))))

(defn unsub-all-client-watchers [client-name]
  (let [subs (mapv first (get @db/atoms-and-watchers client-name))]
    (ut/pp [:force-unsub-all-client-watchers client-name (count subs) :subs])
    (doseq [s subs]
      (ut/pp [:trying-to-unsub client-name s])
      (unsub-value client-name s))
    (swap! db/ack-scoreboard dissoc client-name)
    (swap! db/atoms-and-watchers dissoc client-name)))

(defn get-client-watchers [] ;; debug repl
  (try
    (let [cc (dissoc (client-statuses) :rvbbit)]
      (into {} (for [[k {:keys [last-seen-seconds]}] cc
                     :let [subs (mapv first (get @db/atoms-and-watchers k))]]
                 {k {:subs (count subs)
                     :queue-items (count @(get @client-queues k))
                     :last-seen-seconds last-seen-seconds}})))
    (catch Exception e (ut/pp [:purge-client-queues-error e]))))

;; (get-client-watchers)

;; (count (keys @db/ack-scoreboard))
;; (count (keys @atoms-and-watchers))
;; (purge-dead-client-watchers)
;; (unsub-all-client-watchers :kind-spherical-ox-25)
;; (doseq [cc (keys (client-statuses))]
;;   (unsub-all-client-watchers cc))
;;  (reset! db/ack-scoreboard (select-keys @db/ack-scoreboard (keys @atoms-and-watchers)))
;; (count (keys (client-statuses)))



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
  [base-type keypath client-name new-value & [extra]]
  (let [flow-id (first keypath)]
    (let [orig-tracker   (get @flow-db/tracker flow-id)
          tracker        (into {}
                               (for [[k v] orig-tracker ;; remove condi non-starts. dont send
                                     :when (not (get v :in-chan?))]
                                 {k v}))
          condis         (get-in @flow-db/status [flow-id :condis])
          running?       (get @db/flow-status flow-id :*running?)
          first-tracker? (empty? (get-in @tracker-client-only [flow-id client-name]))]
      (when true ;running? ;; true ;(and new? running?) ;; (not= tracker last-tracker)
        (swap! tracker-client-only assoc-in [flow-id client-name] tracker)
        (when false ;true ;; DEPRECATED ;(not running?) ;; test to stop thrash... worth it?
          (kick client-name (vec (cons :tracker keypath)) (if first-tracker? (assoc tracker :*start! [{}]) tracker) nil nil nil))
        (when (ut/ne? condis) ;; still need this little guy tho
          (kick client-name (vec (cons :condis keypath)) condis nil nil nil))))))

(defn send-acc-tracker-runner
  [base-type keypath client-name new-value & [extra]]
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
  [base-type keypath client-name new-value & [extra]]
  (let [flow-id (first keypath)]
    (let []
      (kick client-name
            (vec (cons :tracker-blocks keypath))
            (select-keys (get @db/flow-status flow-id) [:running-blocks :done-blocks :error-blocks :waiting-blocks])
            nil
            nil
            nil))))

(defn sub-to-value
  [client-name flow-key & [signal?]] ;;; IF CHANGED, REMEMBER TO ALSO UPDATE "CLOVER-LOOKUP" -
  (let [flow-key-orig           flow-key
        ;flow-key-orig           (keyword (cstr/replace (str flow-key-orig) ":" "")) ;; normalized the old weird colon inserted clover kws
        flow-key-sub            (db/replace-flow-key-vars flow-key client-name)
        flow-key-split          (db/break-up-flow-key flow-key)
        flow-key-split-sub      (db/break-up-flow-key flow-key-sub)
        vars?                   (not (= flow-key flow-key-sub))
        flow-key                (if vars? flow-key-sub flow-key)
        flow-key-split          (if vars? flow-key-split-sub flow-key-split)
        [flow-id step-id]       flow-key-split ;(break-up-flow-key flow-key)
        ;signal?                 (true? signal?)
        keypath                 [flow-id step-id]
        sub-path                (db/break-up-flow-key-ext flow-key)
        base-type               (first sub-path)
        ;;flow-client-param-path  (keyword (cstr/replace (str (first keypath) (last keypath)) #":" ">"))
        other-client-param-path (keyword (cstr/replace (cstr/join ">" (vec (rest sub-path))) ":" ""))
        ;; client-param-path       (if (or (= base-type :flow-status)
        ;;                                 ;;(= base-type :kit-status)
        ;;                                 (= base-type :flow)) flow-client-param-path other-client-param-path)
        client-param-path       other-client-param-path
        client-keypath          (db/client-kp flow-key keypath base-type sub-path client-param-path client-name)
        ssp                     (db/break-up-flow-key-ext flow-key-orig)
        req-client-kp           (db/client-kp flow-key-orig
                                              (vec (db/break-up-flow-key flow-key-orig))
                                              base-type
                                              ssp
                                              (keyword (cstr/replace (cstr/join ">" (vec (rest ssp))) ":" ""))
                                              client-name)
        ;;_ (ut/pp [:flow-key flow-key vars? flow-key-sub flow-key-split flow-key-split-sub]) ;; this
        _ (when vars? (swap! db/param-var-mapping assoc [client-name client-keypath] req-client-kp))
        _ (when vars? (swap! db/param-var-crosswalk assoc-in [client-name flow-key-orig] [flow-key [client-name client-keypath]]))
        _ (when vars?
            (swap! db/param-var-key-mapping assoc
                   client-name
                   (vec (distinct (conj (get @db/param-var-key-mapping client-name []) [flow-key-orig flow-key])))))
        lv                      (db/ddb-get "last-values" (hash keypath)) ;;(get @db/last-values keypath)
        ]
    ;; (ut/pp [:client-sub! (if signal? :signal! :regular!) client-name :wants base-type client-param-path keypath
    ;;         ;{:sub-path sub-path}
    ;;         flow-key])
    ;; (when (get-in @flow-db/results-atom keypath) (ut/pp [:react (get-in @flow-db/results-atom keypath)]))
    (db/add-watcher keypath client-name send-reaction flow-key :param-sub)
    (when (or (not signal?)
              (cstr/ends-with? (str flow-key) "running?"))
      (kick client-name
            [base-type client-param-path]
            (db/get-starting-value base-type client-param-path sub-path keypath lv client-name)
            nil
            nil
            nil))
    [:client-sub-request flow-id :step-id step-id :client-name client-name]))


(defn sync-client-subs []
  (let [late-clients (db/client-subs-late-delivery 30000)
        ;sub-reacts (distinct (apply concat (for [[_ v] @splitter-stats] (keys v))))
        tardy  (select-keys @db/atoms-and-watchers late-clients)
        ;sub-reqs (distinct (flatten (for [[_ v] tardy] (keys v))))
        sub-reqs-map (into {} (for [[k v] tardy] {k (keys v)}))]
    (when (ut/ne? late-clients)
      (ut/pp [:syncing-late-client-subs late-clients])
      (doseq [[client-name flow-keys] sub-reqs-map]
        (swap! db/atoms-and-watchers (fn [atom] (dissoc atom client-name)))
        (doseq [flow-key flow-keys
                :let [base-type (first (db/parse-coded-keypath flow-key))
                      _ (db/get-atom-splitter-deep flow-key (get db/master-reactor-atoms base-type))]]
          (sub-to-value client-name flow-key))))))

(defmethod wl/handle-push :sub-to-flow-value [{:keys [client-name flow-key]}]
  (doall (sub-to-value client-name flow-key)))

(defmethod wl/handle-request :sub-to-flow-value [{:keys [client-name flow-key]}]
  (ut/pp [:why-is-client  client-name :still-requesting?])
  (doall (sub-to-value client-name flow-key)))

;; (sub-to-value :rvbbit :time/unix-ms true)
;; (ut/pp @rvbbit-client-sub-values)

;; (defn remove-watchers-for-flow22
;;   [flow-id & [client-name]]
;;   (doseq [[c-name subs] @db/atoms-and-watchers
;;           :let          [matching-subs (filter #(= (:flow-id %) flow-id) (vals subs))]
;;           :when         (ut/ne? matching-subs)]
;;     (ut/pp [:matching-subs c-name matching-subs])
;;     (doseq [sub matching-subs]
;;       (do (ut/pp [:removing (count matching-subs) :watchers :for flow-id c-name
;;                   [[(:keypath sub) c-name (:sub-type sub) flow-id (:flow-key sub)]]])
;;           (db/remove-watcher (:keypath sub) c-name (:sub-type sub) flow-id (:flow-key sub))))))

(defn reload-signals-subs []
  (let [_             (sub-to-value :rvbbit :time/unix-ms true)
        parts         (vec (for [[signal-name {:keys [signal]}] @signals-atom]
                             [signal-name
                              (vec (filter #(and (keyword? %) (cstr/includes? (str %) "/")) ;; get
                                           (ut/deep-flatten signal)))]))
        curr-keyparts (vec (keys (get @db/atoms-and-watchers :rvbbit)))
        all-keyparts  (vec (distinct (ut/deep-flatten (map last parts))))
        to-remove     (vec (cset/difference (set curr-keyparts) (set all-keyparts)))]
    (ut/pretty-spit (str "./defs/backup/signals." (System/currentTimeMillis) ".edn") @signals-atom)
    (reset! signal-parts-atom parts) ;; faster than running it every time - since we need it
    (ut/pp [:reload-signals-subs! parts curr-keyparts all-keyparts {:remove! to-remove}])
    (doseq [rm to-remove] ;; clean up ones we dont need to watch anymore
      (let [{:keys [sub-type keypath flow-key]} (get-in @db/atoms-and-watchers [:rvbbit rm])]
        (db/remove-watcher keypath :rvbbit sub-type nil flow-key)))
    (doseq [kk all-keyparts] ;; re-add them all - TODO, only add new ones (however, they get
      (ut/pp [:signal-sub-to kk])
      (sub-to-value :rvbbit kk true))
    (reset! db/last-signals-atom (select-keys @db/last-signals-atom
                                              (vec (filter #(not (cstr/includes? (str %) "/part-")) (keys @db/last-signals-atom))))) ;; clear
                                                                                                                               ;; cache
                                                                                                                               ;; of
    (doseq [signal (keys @signals-atom)] ;; (re)process everythiung since we just got updated
      #_(ut/pp [:re-processing-signal signal])
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
                                                           v2       (get-in @db/last-signal-value-atom [sigk ss]) ;; should
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
                    vvals   (select-keys (get-in @db/atoms-and-watchers [:rvbbit]) cmpkeys)]
                (into {}
                      (for [[k {:keys [base-type sub-type sub-path keypath]}] vvals
                            :let                                              [;;_ (when (not= base-type :time) (ut/pp
                                                                               v3 (get-in @(db/get-atom-from-keys base-type
                                                                                                                  sub-type
                                                                                                                  sub-path
                                                                                                                  keypath)
                                                                                          keypath
                                                                                          ;;(get @db/last-values keypath)
                                                                                          (db/ddb-get "last-values" (hash keypath))
                                                                                          )]] ;; extra
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
                                     (swap! db/last-signals-history-atom assoc-in
                                            [kk part-key]
                                            (into (vec (take-last 12
                                                                  (sort-by second
                                                                           (get-in @db/last-signals-history-atom [kk part-key]))))
                                                  [[result nowah]])))
                                 _ (when true ;(not= result (get @last-signals-atom vvv)) ;; no
                                     (swap! db/last-signals-history-atom assoc-in
                                            [kk vvv]
                                            (into (vec (take-last 12 (sort-by second (get-in @db/last-signals-history-atom [kk vvv]))))
                                                  [[result nowah]])))
                                 _ (swap! db/last-signals-atom-stamp assoc vvv nowah)
                                 _ (swap! db/last-signals-atom assoc vvv result)
                                 _ (swap! db/last-signals-atom assoc part-key result)]] ;; last time
                       {[vvv rvv] result}))}))
        full-parts-work
        (into {} ;; important side effects / will transition to a doseq after debugging phase
              (for [[kk vv] signals-resolved
                    :let    [honey-sql-str (to-sql {:select [[1 :vv]] :where vv})
                             result        (true? (ut/ne? (sql-query ghost-db honey-sql-str [:ghost-signal-resolve kk])))
                             _ (when true ;(not= result (get @last-signals-atom kk))  ;; no
                                 (swap! db/last-signals-history-atom assoc-in
                                        [kk kk]
                                        (into (vec (take-last 19 (sort-by second (get-in @db/last-signals-history-atom [kk kk]))))
                                              [[result nowah]])))
                             _ (swap! db/last-signals-atom assoc kk result)
                             _ (swap! db/last-signals-atom-stamp assoc kk nowah)]]
                {[kk vv] result}))]
    (doseq [[k v] signals-resolve-map] (doseq [[kk vv] v] (swap! db/last-signal-value-atom assoc-in [k kk] vv))) ;; when all done,
                                                                                                              ;; set
                                                                                                              ;; "last-value"
    ))
(defn reload-solver-subs []
  (let [_             (sub-to-value :rvbbit :time/unix-ms true)
        parts         (vec (for [[solver-name {:keys [signal]}] @solvers-atom]
                             [solver-name
                              (vec (filter #(and (keyword? %) (cstr/includes? (str %) "/")) ;; get
                                           (ut/deep-flatten [signal])))])) ;; enclose in case its
        curr-keyparts (vec (keys (get @db/atoms-and-watchers :rvbbit)))
        all-keyparts  (vec (distinct (ut/deep-flatten (map last parts))))
        to-remove     (vec (filter #(cstr/starts-with? (str %) ":solver/")
                                   (cset/difference (set curr-keyparts) (set all-keyparts))))]
    (ut/pretty-spit (str "./defs/backup/solvers." (System/currentTimeMillis) ".edn") @solvers-atom)
    #_(ut/pp [:reload-solver-subs! {:parts parts :curr-keypaths curr-keyparts :all-keyparts all-keyparts :to-remove to-remove}])
    (doseq [kk all-keyparts]
      #_(ut/pp [:solver-sub-to kk])
      (sub-to-value :rvbbit kk true))
    #_(doseq [signal (keys @solvers-atom)] ;; (re)process everythiung since we just got updated
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
  (let [ff flow-id
        flow-keys (if (empty? flow-keys)
                    (gen-flow-keys flow-id client-name)
                    flow-keys)
        ] ;; jumping
    (ut/pp [:flow-keys flow-keys])

;;  (db/add-watcher [flow-id :tracker-blocks]
;;                  client-name
;;                  send-tracker-block-runner
;;                   ;(keyword (str "blocks||" flow-id "||" (hash keypath)))
;;                  (keyword (str "flow-status/" flow-id ))
;;                  :tracker
;;                  flow-id)

    (doseq [keypath flow-keys]
      (let [[flow-id step-id] keypath
            step-id (cstr/replace (str step-id) ":" "")]
        ;;(ut/pp [:client-sub-flow-runner flow-id :step-id step-id :client-name client-name])
        ;;(db/add-watcher keypath client-name send-reaction flow-key :param-sub)

                ;; (db/add-watcher keypath
                ;;                 client-name
                ;;                 send-tracker-runner
                ;;         ;(keyword (str "tracker||" flow-id "||" (hash keypath)))
                ;;                 (keyword (str "flow/" flow-id ">" step-id))
                ;;                 :tracker
                ;;                 flow-id)

;; :running-blocks :done-blocks :error-blocks :waiting-blocks]

        (db/add-watcher keypath
                        client-name
                        (fn [base-type keypath client-name new-value]
                          ;(send-tracker-runner base-type keypath client-name new-value)
                          (send-reaction-runner base-type keypath client-name new-value)
                          ;(send-acc-tracker-runner base-type keypath client-name new-value)
                          ;(send-tracker-block-runner base-type keypath client-name new-value)
                          )
                        ;(keyword (str "tracker||" flow-id "||" (hash keypath)))
                        (keyword (str "flow-runner/" flow-id ">" step-id))
                        :flow-runner
                        ff)


        (db/add-watcher keypath
                        client-name
                        (fn [base-type keypath client-name new-value]
                          (send-tracker-block-runner base-type keypath client-name new-value)
                          (send-tracker-runner base-type keypath client-name new-value)
                          (send-acc-tracker-runner base-type keypath client-name new-value)
                                                  ;(send-acc-tracker-runner base-type keypath client-name new-value)
                                                  ;(send-tracker-block-runner base-type keypath client-name new-value)
                          )
                                                ;(keyword (str "tracker||" flow-id "||" (hash keypath)))
                        (keyword (str "tracker/" flow-id ">" step-id))
                        :tracker
                        ff)

        ;; (db/add-watcher keypath
        ;;                 client-name
        ;;                 send-reaction-runner
        ;;                 ;(keyword (str "runner||" flow-id "||" (hash keypath)))
        ;;                 (keyword (str "flow/" flow-id ">" step-id))
        ;;                 :flow-runner
        ;;                 flow-id)
        ;; (db/add-watcher keypath
        ;;                 client-name
        ;;                 send-tracker-runner
        ;;                 (keyword (str "tracker||" flow-id "||" (hash keypath)))
        ;;                 :tracker
        ;;                 flow-id)
        ;; (db/add-watcher keypath
        ;;                 client-name
        ;;                 send-tracker-block-runner
        ;;                 ;(keyword (str "blocks||" flow-id "||" (hash keypath)))
        ;;                 (keyword (str "flow/" flow-id ">" step-id))
        ;;                 :tracker
        ;;                 flow-id)

        ;; (db/add-watcher keypath
        ;;                 client-name
        ;;                 send-acc-tracker-runner
        ;;                 (keyword (str "flow/" flow-id ">" step-id))
        ;;                 :tracker
        ;;                 flow-id)
        ))
    ;; (boomerang-client-subs client-name)
    )
  [:copy-that client-name])

;; (defmethod wl/handle-request :open-ai-push
;;   [{:keys [kind convo panels client-name]}]
;;   (doall (let [resp (assistants/chat convo)]
;;            (do ;(ut/write-csv recos-csv "./recos.csv")
;;              (ut/pp [:chat-resp resp])
;;              {:convo resp :client-name client-name}))))

(def sql-cache (atom (cache/lru-cache-factory {} :threshold 1000)))

(defn lookup-cache-exists? [key]
  (cache/has? @sql-cache key))

(defn get-from-cache [key]
  (cache/lookup @sql-cache key nil))

(defn insert-into-cache [key value]
  (swap! sql-cache assoc key value))



(defn get-connection-string [connection-id]
  (let [;conn (sql-query-one system-db (to-sql {:select [:original_connection_str] :from
        conn (get @db/conn-map connection-id)]
    conn))

(defn data-type-value [v]
  (cond (or (cstr/includes? (str (type v)) "DateTime")
            (cstr/includes? (str (type v)) "TimeStamp"))                              "datetime"
        (cstr/includes? (str (type v)) "Date")                                        "date"
        (or (and (cstr/starts-with? (str v) "@@") (string? v)) (vector? v) (map? v))  "rabbit-code"
        (and (string? v)
             (or (cstr/starts-with? (cstr/trim v) "[")
                 (cstr/starts-with? (cstr/trim v) "{"))
             (try (edn/read-string v) (catch Exception _ false)))                    "rabbit-code"
        (string? v)                                                                   "string"
        (integer? v)                                                                  "integer"
        (float? v)                                                                    "float"
        (boolean? v)                                                                  "boolean"
        :else                                                                         "unknown"))

(defn sql-like-min [coll]
  (when (seq coll)
    (reduce (fn [a b]
              (if (neg? (compare (str a) (str b))) a b))
            coll)))

(defn sql-like-max [coll]
  (when (seq coll)
    (reduce (fn [a b]
              (if (pos? (compare (str a) (str b))) a b))
            coll)))

(defn safe-median [coll]
  (when (seq coll)
    (let [sorted (sort-by str coll)
          cnt (count sorted)
          halfway (quot cnt 2)]
      (if (odd? cnt)
        (nth sorted halfway)
        (let [bottom (nth sorted (dec halfway))
              top (nth sorted halfway)]
          (if (every? number? [bottom top])
            (/ (+ bottom top) 2)
            (str bottom)))))))

(defn safe-avg [coll]
  (when (seq coll)
    (if (every? number? coll)
      (/ (apply + coll) (count coll))
      (let [cnt (count coll)]
        (if (pos? cnt)
          (str (first coll) " (non-numeric average)")
          nil)))))

(defn safe-stats [fsamples]
  (try
    (cond
      (every? number? fsamples)
      {:min (apply min fsamples)
       :max (apply max fsamples)
       :median (safe-median fsamples)
       :avg (safe-avg fsamples)}

      (every? string? fsamples)
      {:min (sql-like-min fsamples)
       :max (sql-like-max fsamples)
       :median (safe-median fsamples)
       :avg (safe-avg fsamples)}

      :else
      (let [str-samples (map str fsamples)]
        {:min (sql-like-min str-samples)
         :max (sql-like-max str-samples)
         :median (safe-median str-samples)
         :avg (safe-avg str-samples)}))
    (catch Exception _
      {:min nil, :max nil, :median nil, :avg nil})))

(defn get-query-metadata [rowset query dbtype connection-id]
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
        ;; field-data            (into {}
        ;;                             (for [f fields]
        ;;                               {f (let [fsamples         (map f sample)
        ;;                                        distinct-samples (count (distinct fsamples))
        ;;                                        commons          (into {} (take 3 (reverse (sort-by last (frequencies fsamples)))))
        ;;                                        data-type        (get-in (vec (reverse (sort-by last
        ;;                                                                                        (frequencies (map data-type-value
        ;;                                                                                                          fsamples)))))
        ;;                                                                 [0 0])]
        ;;                                    {:data-type   data-type
        ;;                                     :distinct    distinct-samples
        ;;                                     :min (try (apply min fsamples) (catch Exception _ nil))
        ;;                                     :max (try (apply max fsamples) (catch Exception _ nil))
        ;;                                     :avg (try (ut/avg fsamples) (catch Exception _ nil))
        ;;                                     :group-by?   (if (not no-group-by?)
        ;;                                                    (true? (try (some #(= f %) materialize-group-bys)
        ;;                                                                (catch Exception _ false)))
        ;;                                                    true)
        ;;                                     :commons     (if (cstr/includes? data-type "date")
        ;;                                                    (into {} (for [[k v] commons] {(str k) v}))
        ;;                                                    commons)
        ;;                                     :cardinality (int (* 100 (float (/ distinct-samples sample-size))))})}))]
        field-data            (into {}
                                    (for [f fields]
                                      {f (let [fsamples         (map f sample)
                                               distinct-samples (count (distinct fsamples))
                                               commons          (into {} (take 3 (reverse (sort-by last (frequencies fsamples)))))
                                               data-type        (get-in (vec (reverse (sort-by last
                                                                                               (frequencies (map data-type-value
                                                                                                                 fsamples)))))
                                                                        [0 0])
                                               {:keys [min max median avg]} (safe-stats fsamples)]
                                           {:data-type   data-type
                                            :distinct    distinct-samples
                                            :min         min
                                            :max         max
                                            :median      median
                                            :avg         avg
                                            :group-by?   (if (not no-group-by?)
                                                           (true? (try (some #(= f %) materialize-group-bys)
                                                                       (catch Exception _ false)))
                                                           true)
                                            :commons     (if (cstr/includes? data-type "date")
                                                           (into {} (for [[k v] commons] {(str k) v}))
                                                           commons)
                                            :cardinality (int (* 100 (float (/ distinct-samples sample-size))))})}))]
    {:fields field-data
     :connection-id connection-id
     :database-type (str dbtype)
     :rowcount (count sample)}))









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
                honey-meta    (get-query-metadata honey-result honey-sql (surveyor/db-typer system-db) "system-db")
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

(def sniff-meta-guard (atom {}))

(defn sniff-meta
  [ui-keypath honey-sql fields target-db client-name connection-id & [deep?]] ;; enqueue-task-sql-meta
  (try (let [ohoney-sql honey-sql
             honey-sql (-> honey-sql
                           (dissoc :offset)
                           (dissoc :page)
                           (dissoc :limit)
                           (dissoc :_last-run) ;; just in case
                           (dissoc :cache?))
             cnts      (into {}
                             (for [[[name f] hsql] (merge {[:rowcount :*] {:select [[[:count 1] :rowcnt]]
                                                                           :from   [[honey-sql :subq]]}}
                                                          (when deep?
                                                            (into {}
                                                                  (for [field (keys fields)]
                                                                    {[:distinct field] {:select [[[:count [:distinct field]]
                                                                                                  :distinct-values]]
                                                                                        :from   [[honey-sql :subq]]}
                                                                    ;;  [:avg field] {:select [[[:avg field] :avgvalue]]
                                                                    ;;                :from   [[honey-sql :subq]]}
                                                                    ;;  [:min field] {:select [[[:min field] :minvalue]]
                                                                    ;;                :from   [[honey-sql :subq]]}
                                                                    ;;  [:max field] {:select [[[:max field] :maxvalue]]
                                                                    ;;                :from   [[honey-sql :subq]]}
                                                                     }
                                                                    ))))]
                               (let [str-sql    (to-sql hsql)
                                     sql-data   (sql-query target-db str-sql [ui-keypath :post-meta])
                                    ;;  sql-data    (query-runstream :honey-xcall [ui-keypath :post-meta] hsql false false connection-id client-name -1 nil nil nil nil)
                                    ;;  sql-data    (get sql-data :result)
                                     ;; ^^ we need to run this through the regular query-runstream pipeline so we can get meta data on extra things like post-fn call data
                                     ;; ^^ BUT we can't call post-fn on subqueries, we wed need to materializa the whole query and then run deep meta on the result. TODO
                                     ;; ^^ also, caching here would be nice... TODO
                                     sql-result (first (vals (first sql-data)))
                                     res        {f sql-result}]
                                 (when (not (string? sql-result)) res))))
                                 _ (ut/pp ["NOSE" :sniff-meta-cnts cnts])]
        ;;  (ut/pp [:running-meta-cnts ui-keypath client-name cnts])
         (swap! sniff-meta-guard assoc-in [ui-keypath client-name] (hash ohoney-sql))
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

;; (defonce pre-sql-cache (fpop/thaw-atom [] "./data/atoms/pre-sql-cache.edn"))

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
    ;; (reset! pre-sql-cache (vec (distinct (conj @pre-sql-cache pre-sql-out))))
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

(defn group-by-field-name [data]
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
             data-map            (merge (get @db/clover-sql-training-atom clover-sql)
                                        {:sql-string     honey-sql-str2
                                         :db-type        (get-in res [0 :db_type])
                                         :clover-sql     clover-sql
                                         :table-metadata metadata})]
         (swap! db/clover-sql-training-atom assoc clover-sql data-map))
       (catch Throwable e (ut/pp [:error-in-clover-sql-training-harvest! (str e)]))))


(defn wait-for-value
  [target-value check-fn ui-keypath client-name]
  (let [start-time (System/currentTimeMillis)
        timeout (* 15 60 1000)] ; 15 minutes in milliseconds
    (loop [last-value nil
           last-change-time (System/currentTimeMillis)]
      (let [current-value (try (check-fn) (catch Exception e
                                            (println "Error in check-fn:" (.getMessage e))
                                            nil))
            current-time (System/currentTimeMillis)
            elapsed-time (- current-time start-time)
            time-since-last-change (- current-time last-change-time)]

        (when (number? current-value)
          (let [progress (min 100 (max 0 (* 100 (/ current-value target-value))))]
            ;;(println ui-keypath client-name "Progress:" progress "% Current:" current-value "Target:" target-value)
            (push-to-client ui-keypath [] client-name 1 :materialized-pct progress)))

        (cond
          (= current-value target-value)
          {:success true, :value current-value, :elapsed-ms elapsed-time}

          (>= elapsed-time timeout)
          {:success false, :value current-value, :elapsed-ms elapsed-time, :reason "global timeout"}

          (and (= current-value last-value) (> time-since-last-change 60000))
          {:success false, :value current-value, :elapsed-ms elapsed-time, :reason "value unchanged for 60 seconds"}

          :else
          (do
            (Thread/sleep 3000) ; Wait for 3 seconds
            (recur current-value
                   (if (= current-value last-value) last-change-time current-time))))))))

;; (wait-for-value 100 (fn [] 50))

(defn api-call-wrapper [{:keys [input client-name solver-name ui-keypath]}]
  (ut/pp [:api-call-wrapper input client-name solver-name ui-keypath] {:width 50})
  (try
    (let [output (edp/api-query input)
          rowset? (try (and (vector? output) (map? (first output))) (catch Exception _ false))]
      output)
    (catch Exception e
      (let [data (ex-data e)
            error-message (str (.getMessage e) "\n\n" (get data :cause))
            ;; error-lines (concat
            ;;              (for [i (range 0 (count error-message) 60)]
            ;;                (subs error-message i (min (+ i 90) (count error-message))))
            ;;              [(str input)])
            ;; error-rows (vec (for [e error-lines] {:database_says (str e)}))
            error-rows [{:database_says error-message}]]
        (ut/pp [:api-query-wrapper-error error-message data] {:width 50})
        (swap! db/last-solvers-atom assoc-in [solver-name] error-rows)
        error-rows))))

(defn sql-query-raw
  [db-spec query ui-keypath client-name solver-name & extra]
  (swap! sql/sql-queries-run inc)
  (let [start-time (System/nanoTime)]
    (jdbc/with-db-connection [t-con db-spec]
      (ut/pp [query db-spec])
      (try
        (let [result (sql/wrap-maps query (jdbc/query t-con query {:identifiers keyword :as-arrays? true :result-set-fn doall}))
              end-time (System/nanoTime)
              execution-time-ms (/ (- end-time start-time) 1e6)]
          ;; (swap! sql/sql-query-log conj [(-> (last (cstr/split (str (:datasource db-spec)) #" ")) (cstr/replace "(" "") (cstr/replace ")" "") keyword)
          ;;                            (str (try (subs (str query) 0 300)
          ;;                                      (catch Throwable _ (str query))) "-" (hash query)) execution-time-ms])
          (ut/pp [:query-success (count result) :rows execution-time-ms :ms])
          result)
        (catch Exception e
          (let [end-time (System/nanoTime)
                execution-time-ms (/ (- end-time start-time) 1e6)
                error-message (str (.getMessage e))
                ;; error-lines (concat
                ;;              (for [i (range 0 (count error-message) 60)]
                ;;                (subs error-message i (min (+ i 90) (count error-message))))
                ;;              [(sql/get-connection-string-info db-spec)])
                ;; error-rows (vec (for [e error-lines] {:database_says (str e)}))
                error-rows [{:database_says error-message}]]
            (ut/pp [:database_says query db-spec (.getMessage e) execution-time-ms :ms])
            (swap! db/last-solvers-atom assoc-in [solver-name] error-rows)
            (insert-error-row! db-spec query e)
            error-rows))))))

(defn run-raw-sql [{:keys [input client-name connection-id ui-keypath solver-name]}]
  (let [sys-connections ["system-db" "import-db" "systemh2-db"
                          "system-reporting-db" "cache.db.memory"
                         "cache.db"]
        user-connections (vec (keys @db/conn-map))
        conn-string-names (vec (into sys-connections user-connections))
        comment-lines (filter #(cstr/starts-with? (cstr/trim (str %)) "--") (cstr/split-lines input))
        first-connection (cstr/join " " comment-lines)
        connection-id (or
                       (first (filter #(cstr/includes? first-connection %) conn-string-names))
                       connection-id)
        ;; connection-type (cond
        ;;                   (some #(= parsed-connection-id %) sys-connections) :system
        ;;                   (some #(= parsed-connection-id %) user-connections) :user
        ;;                   :else :unknown)
        _ (ut/pp [:parsed-db connection-id ui-keypath solver-name])
        ;;formatted-sql (sql-formatter input)
        ;;formatted-sql-vec (vec (rest (cstr/split formatted-sql #"\\n")))
        ;_ (ut/pp formatted-sql-vec)
        ;;_ (ut/pp [:formatted-sql (vec  (rest formatted-sql-vec))])
        ;;_ (push-to-client ui-keypath [] client-name 1 :push-assocs {ui-keypath formatted-sql-vec})
        ;; connection-id (try
        ;;                     (case connection-type
        ;;                       :system (symbol parsed-connection-id)
        ;;                       :user (or (try (get-connection-string parsed-connection-id) (catch Exception _ 'system-db)) 'system-db)
        ;;                       'system-db)
        ;;                     (catch Exception e
        ;;                       (do (ut/pp [:parsed-db-error parsed-connection-id connection-type (str e)])
        ;;                           'system-db)))
        target-db (cond ;;query-meta-subq? system-db ;; override for sidecar meta queries
                    (keyword? connection-id)           (sql/create-or-get-client-db-pool client-name)
                    (or (nil? connection-id)
                        (nil? empty?))                  system-db
                    (= connection-id "import-db")       import-db
                    (= connection-id "realms-db")       systemh2-db
                    (= connection-id "stack-db")        stack-db
                    (= connection-id "system-db")       system-db
                    (= connection-id "systemh2-db")     systemh2-db
                    (= connection-id "flows-db")        systemh2-db
                    ;; (= connection-id "autocomplete-db") autocomplete-db
                    ;; (= connection-id "history-db")      history-db
                    (= connection-id "system")          system-db
                    (= connection-id "system-reporting-db") system-reporting-db
                    (= connection-id "cache.db.memory") cache-db-memory
                    (or (= connection-id :cache)
                        (= connection-id "cache.db")
                        (nil? connection-id))           cache-db
                    :else (get-connection-string connection-id))
        target-db (or target-db system-db) ;; if nil
        _ (ut/pp [:parsed-db target-db])]
    (sql-query-raw
     target-db
     input
     ui-keypath
     client-name
     solver-name
     {:client-name client-name
      :input input
      :connection-id target-db
      :parsed-connection-id connection-id})))

;; (ut/pp (get @materialized-full-queries :honorable-spherical-seastar-3))

(defn get-nested-grid [{:keys [input client-name connection-id extra ui-keypath solver-name] :as mmap}]
  (let [data-key (first (get input :rowset-keypath))
        {:keys [rowset-keypath row-keys column-keys agg-specs raw?]} input
        ;queries (first (filter #(= (last %) data-key) (distinct (map (fn [x] (vec (take 3 x))) (ut/kvpaths (get @client-panels client-name))))))
        ;src-query (get-in @client-panels (vec (into [client-name] queries)))
        ;src-query-conn (get-in @client-panels (into (vec (into [client-name] (drop-last 2 queries))) [:connection-id])
        ;                       (get-in @client-panels (into (vec (into [client-name] queries)) [:connection-id])))

        row-keys (if (> (count row-keys) 1) (vec (remove string? row-keys)) row-keys) ;; remove the placeholder "all" entry
        column-keys (if (> (count column-keys) 1) (vec (remove string? column-keys)) column-keys) ;; remove the placeholder "all" entry

        src-query-conn (get extra :connection-id)
        last-mat-q (get-in @materialized-full-queries [client-name data-key])
        grp-query-int {:select (into (vec (distinct (into row-keys column-keys)))
                                     (for [[al ag d ff] (conj agg-specs [:rowcnt :count 1 :*])]
                                       (if (= ag :countd)
                                         [[:count [:distinct ff]] al]
                                         [[ag ff] al])))
                       :connection-id src-query-conn
                       :from [[(keyword (cstr/replace (str "query/" data-key) ":" "")) :agg45]]
                       :group-by (vec (distinct (into row-keys column-keys)))}
        grp-query {:select (into (vec (distinct (into row-keys column-keys)))
                                 (for [[al ag d ff] (conj agg-specs [:rowcnt :count 1 :*])]
                                   (if (= ag :countd)
                                     [[:count [:distinct ff]] al]
                                     [[ag ff] al])))
                   :from [[last-mat-q :agg45]]
                   ;:limit -1
                   :group-by (vec (distinct (into row-keys column-keys)))}

        {:keys [result]} (query-runstream :honey-xcall [(keyword (str (cstr/replace (str data-key) ":" "") "-pvt-gen"))]
                                          grp-query false false src-query-conn client-name -1 :pivot grp-query false false)

        ;; result (vec (for [row result] (assoc row :cl1 (if (> (get row :rowcnt) 25) {:background-color "#ffffff33"} {}))))

        modded-agg-specs (vec (for [[al ag d ff]
                                    (conj agg-specs [:rowcnt :count 1 :*])
                                    :let [ag (if (or (= ag :countd) (= ag :count)) :sum ag)]]
                                [al ag d al])) ;; since we are SQL pre-agging, we need to swap the alias for the real...
        ;; modded-agg-specs (if (> (count modded-agg-specs) 1) (vec (remove #(= (last %) :rowcnt) modded-agg-specs)) modded-agg-specs)
        _ (ut/pp ["🏋" :modded-agg-specs modded-agg-specs])
        _ (when true ; (ut/ne? queries)
            (ut/pp ["🏋" :gen-pre-pivot-query {:extra extra} src-query-conn grp-query :sample3 (vec (take 3 result)) :modded-agg-specs modded-agg-specs] {:width 60}))

        mmap (-> mmap
                 (assoc-in [:input :agg-specs] modded-agg-specs)
                 (assoc-in [:input :query-data] (vec result)))
        {:keys [grid data]} (npiv/get-grid-data mmap)
        pivot-data-kw (keyword (cstr/replace (str (last ui-keypath) "-pivot") ":" ""))
        pre-pivot-data-kw (keyword (cstr/replace (str (last ui-keypath) "-flat") ":" ""))
        reaction-clover (get grid :reaction-clover)]
    (ut/pp ["🏋" :pivot-data! (ut/dissoc-in mmap [:input :query-data]) grid data])
    (push-to-client (vec (rest ui-keypath)) [] client-name 1 :push-assocs {(vec (conj ui-keypath :reaction-clover)) reaction-clover
                                                                           (vec (conj ui-keypath :pivot-data-kw)) pivot-data-kw
                                                                           (vec (into (vec (take 2 ui-keypath)) [:queries pre-pivot-data-kw])) grp-query-int
                                                                           [:data pivot-data-kw] data}) ;; push pivoted data to client...

    grid))

(def qcache (atom {})) ;; test

(defn cached-sql-query [target-db honey-sql-str ui-keypath & [connection-id ]]
  (if (and (not (cstr/includes? (str connection-id) "cache"))
           (not (cstr/includes? (str connection-id) "flows"))
           (not (cstr/includes? (str connection-id) "system")))
    (let [cache-key (hash [honey-sql-str connection-id])]
      (if-let [cached-result (@qcache cache-key)]
        (do (ut/pp ["🤑" :sql-cache-used ui-keypath connection-id])
            cached-result)
        (let [result (sql-query target-db honey-sql-str ui-keypath)]
          (swap! qcache assoc cache-key result)
          result)))
    (sql-query target-db honey-sql-str ui-keypath)))



(defn query-shape-rotation [client-name panel-key ui-keypath connection-id honey-hash honey-sql row-count]
  (ppy/execute-in-thread-pools
   #_(keyword (cstr/replace (str "shape-rotator-for-each-sql-query-serial-" client-name) ":" ""))
   (keyword (cstr/replace (str "shape-rotator-for-each-sql-query-" client-name) ":" ""))
   (fn []
     (let [system-query? (cstr/includes? (str connection-id) "system")]
       (try
         (do
           (if-let [cached (when true ;(not system-query?) ;;
                           ;;(get @db/shapes-result-map-by-honey-hash honey-hash)
                           ;(db/ddb-get "honeyhash-map" honey-hash)
                             (d/get-value db/ddb "honeyhash-map" honey-hash))]

             (let [{:keys [shapes fields]} cached
                   reco-count (count shapes)
                 ;;_ (when (cstr/includes? (str ui-keypath) "bigfoot") (ut/pp [:fields fields]))
                   field-counts (into {} (for [r fields] {(keyword (get r :field-name)) (get r :distinct-count)
                                                          :* (or row-count (get r :total-rows))}))
                   modded-shapes (assoc cached :shapes (mapv #(assoc-in % [:shape-rotator :source-panel] panel-key)
                                                             shapes))
                   hash-key (hash [client-name panel-key (first ui-keypath)])]
               (push-to-client ui-keypath [:cnts-meta (first ui-keypath)] client-name 1 :cnts field-counts)
               #_(ut/pp ["🧀🐀" :shape-rotator (when system-query? :SYSTEM-QUERY) :honey-hash-cached-loaded
                       client-name (first ui-keypath) reco-count :viz-cnt (get-in fields [0 :total-rows]) :rows :hk hash-key])
             ;(swap! db/shapes-result-map assoc-in [client-name panel-key (first ui-keypath)] modded-shapes)
               (db/ddb-put! "shapes-map" hash-key modded-shapes)

               (ppy/execute-in-thread-pools ;; send one of each recos to the client
                (keyword (cstr/replace (str "shape-rotator-send-blocks-serial-" client-name) ":" ""))
                (fn [] (let [_ (Thread/sleep 500) ;; pace out the client packets a bit
                             recos-for-grouped (group-by (fn [m] (get-in m [:shape-rotator :shape-name])) (get modded-shapes :shapes))
                             one-of-each (mapv (fn [[_ v]] (first v)) recos-for-grouped)]
                         (client-mutate client-name {[:shapes panel-key (first ui-keypath)] one-of-each} true))))

               #_(push-to-client ui-keypath [:reco-status (first ui-keypath)] client-name 1 :reco :done reco-count 0))

           ;; non honeyhash cache
             (let [;;_ (ut/pp [:hh client-name honey-hash (str honey-sql)])
                   {fields-ms :elapsed-ms fields :result} (ut/timed-exec
                                                           (rota/get-field-maps
                                                            {:f-path connection-id ;target-db
                                                             :shape-test-defs @db/sniff-test-map ;; cruiser/default-sniff-tests
                                                             :shape-attributes-defs @db/field-attribute-map ;; cruiser/default-field-attributes
                                                             :query-name (first ui-keypath)
                                                             :client-name client-name ;; <-- only for queries
                                                             :honey-sql honey-sql}))
                   {shapes-ms :elapsed-ms shapes :result} (ut/timed-exec
                                                           (rota/get-viz-shape-blocks fields @db/shapes-map))
                   res {:fields fields :shapes shapes}
                   reco-count (count shapes)
                   field-counts (into {} (for [r fields] {(keyword (get r :field-name)) (get r :distinct-count)
                                                          :* (get r :total-rows)}))
                   modded-shapes (assoc res :shapes (mapv #(assoc-in % [:shape-rotator :source-panel] panel-key)
                                                          (get res :shapes)))
                   hash-key (hash [client-name panel-key (first ui-keypath)])]
               (push-to-client ui-keypath [:cnts-meta (first ui-keypath)] client-name 1 :cnts field-counts)
               #_(ut/pp ["🧀" :shape-rotator-run (when system-query? :SYSTEM-QUERY) (+ fields-ms shapes-ms) :ms [fields-ms shapes-ms]
                       client-name (first ui-keypath) reco-count :viz-cnt (get-in fields [0 :total-rows]) :rows :hk hash-key])
             ;;(swap! db/shapes-result-map-by-honey-hash assoc honey-hash res) ;; multi-client cache
             ;(db/ddb-put! "honeyhash-map" honey-hash res)
             ;(swap! db/shapes-result-map assoc-in [client-name panel-key (first ui-keypath)] modded-shapes)
             ;(db/ddb-put! "honeyhash-map" hash-key modded-shapes)
               (d/transact-kv db/ddb
                              [[:put "honeyhash-map" honey-hash res]
                               [:put "shapes-map" hash-key modded-shapes]])

               (ppy/execute-in-thread-pools ;; send one of each recos to the client
                (keyword (cstr/replace (str "shape-rotator-send-blocks-serial-" client-name) ":" ""))
                (fn [] (let [_ (Thread/sleep 500) ;; pace out the client packets a bit
                             recos-for-grouped (group-by (fn [m] (get-in m [:shape-rotator :shape-name])) (get modded-shapes :shapes))
                             one-of-each (mapv (fn [[_ v]] (first v)) recos-for-grouped)]
                         (client-mutate client-name {[:shapes panel-key (first ui-keypath)] one-of-each} true))))

               #_(push-to-client ui-keypath [:reco-status (first ui-keypath)] client-name 1 :reco :done reco-count 0)))

           ;; pre-bake up sql-shapes-table for rotations UI
           (ppy/execute-in-thread-pools :shape-rotation-pre-bakes
                                        (fn [] (push-drag-related-shape-viz client-name [panel-key :queries (first ui-keypath)] true))))
         (catch Throwable e (ut/pp [:shape-rotator-for-each-error! (str e) client-name ui-keypath])))))))

(defn insert-stacked-duck-shadows [panel-key ui-keypath honey-sql connection-id client-name]
  (when (build-running? client-name ui-keypath)
    (mark-build-for-kill! client-name ui-keypath))
  (ppy/execute-in-thread-pools
   (keyword (cstr/replace (str "duck-stacking-shadows-" (first ui-keypath) "-" client-name) ":" ""))
   (fn []
     (try
       (let [og-ui-keypath ui-keypath
             query-key (first ui-keypath)
             ui-keypath [(keyword (str "stacked/" (cstr/replace (str query-key) ":" "")))]
             cache-table-name (ut/keypath-munger [query-key])
             _ (ut/pp ["🦆" :stacked-duck-shadow client-name query-key :-> cache-table-name ui-keypath])
             _ (🦆💬> client-name og-ui-keypath ["running full query pull" 0])
             {:keys [result]} (query-runstream :honey-xcall ui-keypath honey-sql false false
                                               connection-id client-name -4 nil honey-sql false false)  ;; -4 = 5M max rows
             _ (start-build! client-name og-ui-keypath)
             _ (🦆💬> client-name og-ui-keypath ["adding row count measure" 5])
             resultv (mapv #(assoc % :rrows 1) result)
             {:keys [elapsed-ms result]} (ut/timed-exec (rebuild-duck-cache-table
                                                             resultv
                                                             cache-table-name
                                                             (first ui-keypath)
                                                             client-name
                                                             (keys (first resultv))
                                                             stack-db
                                                             client-name
                                                             og-ui-keypath))
             row-count (count resultv)
             extract-query-key (keyword (str "stacked-" (cstr/replace (str query-key) ":" "")))
             extract-query (get-in @db/client-panels [client-name panel-key :queries extract-query-key])]
         (🦆💬> client-name og-ui-keypath [(str "materialized "
                                                (ut/nf  row-count) " rows, "
                                                (count (keys (first resultv))) "  cols - "
                                                (ut/format-duration-seconds-compact (/ elapsed-ms 1000))) 100])
         ;; push sqlized rowset to the spawning block...
         (when true ;(not (map? extract-query))
           (client-mutate client-name {[:panels panel-key :queries extract-query-key]
                                       {:select result
                                        :connection-id "stack-db"
                                        :_last-updated (str (ut/millis-to-date-string (System/currentTimeMillis)))
                                        :_downstream-of (keyword (str "data-hash/" (cstr/replace (str query-key) ":" "")))
                                        :from [[(keyword (ut/keypath-munger og-ui-keypath)) :tt45]]}} true))
         (ut/pp ["🦆" :finished-duck-shadow client-name query-key :-> cache-table-name ui-keypath
                 {:rows row-count :seconds (ut/format-duration-seconds-compact (/ elapsed-ms 1000))}]))
       (catch Exception e (ut/pp [:duck-shadow-error (first ui-keypath) (str e)]))))))

(defn query-runstream
  [kind ui-keypath honey-sql client-cache? sniff? connection-id client-name page panel-key clover-sql deep-meta? snapshot-cache? & [stack?]]
  (doall ;; TODO - all this logic can be wrapped up in a much better package. Q4 lets unwrap the logic and reroll
   (let [;;_ (ut/pp [:honey-sql honey-sql])
         honey-sql (ut/deep-remove-keys honey-sql [:style-rules])
         og-honey-sql honey-sql
         stack-maps (get honey-sql :stacks)
         stack-maps? (pos? (count stack-maps))
         duck-run? (cstr/includes? (str (first ui-keypath)) "stacked/")
         duck-shadow? (and stack-maps? (not duck-run?))
         real-client-name client-name
         client-name (if duck-run? :rvbbit client-name)
         _ (when stack-maps?
             (ut/pp [ (apply str (repeat (count stack-maps) "🥞")) :stack-maps (count stack-maps) ui-keypath]))
         honey-sql (ut/deep-remove-keys honey-sql [:stacks]) ;; temp until stacks is done
         dest (get honey-sql :dest) ;; only for materialization
         honey-sql (dissoc honey-sql :dest)
         _ (swap! honey-echo assoc-in [client-name (first ui-keypath)] (assoc honey-sql :connection-id connection-id))
         post-process-fn (get honey-sql :post-process-fn) ;; allowed at the top level only - will

         honey-sql (if (get honey-sql :limit) ;; this is a crap solution since we can't
                     {:select [:*] :from [honey-sql]}
                     honey-sql)
         ;sniff? true
         connection-id (if (= connection-id "system") "system-db" connection-id) ; workaround for legacy code. TODO
         ;deep-meta? true
         honey-sql (ut/deep-remove-keys honey-sql [:post-process-fn]) ;; disregard if we have
         has-rql? (try (true? (some #(or (= % :*render*) (= % :*read-edn*) (= % :*code*)) (ut/deep-flatten honey-sql)))
                       (catch Exception _ false))
         data-call? (true? (and (not has-rql?) (ut/ne? (first (filter #(= (last %) :data) (ut/kvpaths honey-sql))))))
         runstream
         (fn []
           (try
             (doall ;;;; ? fixed the socket laziness? (no pun intended)
              (let [;last-known-fields (get honey-sql :last-known-fields [])
                    times-key (into [:sql-query] ui-keypath)
                    _ (ship-estimate client-name (first ui-keypath) times-key)
                    repl-host (get-in honey-sql (or (first (filter #(= (last %) :repl-host) (ut/kvpaths honey-sql))) [:nope]))
                    repl-port (get-in honey-sql (or (first (filter #(= (last %) :repl-port) (ut/kvpaths honey-sql))) [:nope]))
                    honey-sql (ut/deep-remove-keys honey-sql [:repl-host :repl-port])
                    _ (when (and repl-host repl-port) (ut/pp [:external-repl! repl-host repl-port]))
                    honey-sql (walk/postwalk-replace {[:*all= {}]       nil ;; take care of empty
                                                      :*client-name-str (pr-str client-name)
                                                      :*client-name     (str client-name)
                                                      :*client-name*    client-name}
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
                    ;; data-literals [:nope] ;(or (first (filter #(= (last %) :data) (ut/kvpaths honey-sql))) [:nope])
                    ;; data-literal-code? (false? (when (or literal-data? post-sniffed-literal-data?) ;; TODO REVIST - DEPRECATED DUE TO RAW REPL?
                    ;;                              (let [dl (get-in honey-sql data-literals)]
                    ;;                                (and (vector? dl) (map? (first dl))))))
                    ;; data-literals-data (get-in honey-sql data-literals)
                    honey-sql (cond post-sniffed-literal-data?                      (walk/postwalk-replace @literal-data-map
                                                                                                           orig-honey-sql)
                                    literal-data?                                   (get orig-honey-sql :data)
                                    (ut/ne? (get orig-honey-sql :transform-select)) (-> (first (get orig-honey-sql :from))
                                                                                        (dissoc :limit)
                                                                                        (assoc :page -1))
                                    :else                                           orig-honey-sql)
                    honey-sql (if has-rql? (extract-rql ui-keypath honey-sql rql-holder) honey-sql)
                    honey-sql (replace-pre-sql honey-sql) ;; runs a subset of clover replacements
                    client-db-strs (filterv #(cstr/includes? (str %) "-")
                                            (mapv #(cstr/replace (str %) ":" "")
                                                  (keys @sql/client-db-pools)))
                    target-db (cond query-meta-subq? system-db ;; override for sidecar meta queries
                                    (keyword? connection-id)           (sql/create-or-get-client-db-pool client-name)
                                    (some #(= % connection-id) client-db-strs) (sql/create-or-get-client-db-pool (keyword connection-id))
                                    (= connection-id "import-db")       import-db
                                    (= connection-id "realms-db")       systemh2-db
                                    (= connection-id "stack-db")        stack-db
                                    (= connection-id "system-db")       system-db
                                    (= connection-id "systemh2-db")     systemh2-db
                                    (= connection-id "flows-db")        systemh2-db
                                    ;; (= connection-id "autocomplete-db") autocomplete-db
                                    ;; (= connection-id "history-db")      history-db
                                    (= connection-id "system")          system-db ;system-db
                                    (= connection-id "system-reporting-db") system-reporting-db
                                    (= connection-id "cache.db.memory") cache-db-memory
                                    (or (= connection-id :cache)
                                        (= connection-id "cache.db")
                                        (nil? connection-id))           cache-db
                                    :else (get-connection-string connection-id))
                    target-db-type [(surveyor/db-typer-fn target-db)]
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
                                                            (try (let [sql-str   (to-sql (assoc vls0 :limit 50) target-db-type)
                                                                       sres      ((if client-cache?
                                                                                    cached-sql-query
                                                                                    sql-query) target-db
                                                                                               sql-str
                                                                                               [:get-pivot-vals-for field :kp
                                                                                                ui-keypath] connection-id)
                                                                       just-vals (vec (map (first (keys (first sres))) sres))]
                                                                   (swap! pivot-cache assoc vls0 just-vals)
                                                                   just-vals)
                                                                 (catch Exception _ ["error" "in" "pivot" "get-vals"])))
                                                          vls0)]]
                                    (when sql? (reset! hold (walk/postwalk-replace {vls0 vls} @hold))))
                                  (pivot/pivot-each @hold 25))
                                honey-sql)
                    req-hash (hash [kind ui-keypath honey-sql client-name])
                    ;; filtered-req-hash (hash [kind ui-keypath (vec (filter keyword? (ut/deep-flatten honey-sql))) ;; eyes
                    ;;                          client-name])
                    cache? (and ;;(not (nil? (get @sql-cache req-hash))) ;; disabled for testing,
                            (lookup-cache-exists? req-hash)
                            (not post-sniffed-literal-data?) ;; questionable, we SHOULD be able
                            (not literal-data?) ;; questionable, we SHOULD be able to invalidate
                            (not (= connection-id "flows-db"))
                            (not (keyword? connection-id))
                            ;; (not (= connection-id "autocomplete-db"))
                            (not (= connection-id "system-log"))
                            (not (= target-db system-db)))
                    cache-table-name ;(if (or post-sniffed-literal-data? literal-data?)
                    (ut/keypath-munger ui-keypath)
                    ;; completion-channel (async/chan) ;; moved to outer
                    data-literal-insert-error (atom nil)

                    ;; data literal might NOT be deprecated... REVISIT 10/2/23
                    ;; honey-sql
                    ;; (if (or post-sniffed-literal-data? literal-data?) ;; 8/11/23 - ALL this logic is deprecated. double-check.
                    ;;   (let [;_ (ut/pp [:last-let? honey-sql])
                    ;;         cache-table cache-table-name ;(str (ut/keypath-munger ui-keypath)
                    ;;         honey-sql   (if data-literal-code? honey-sql (ut/lists-to-vectors honey-sql))
                    ;;         literals    (if data-literal-code?
                    ;;                       data-literals-data
                    ;;                       (get-in honey-sql
                    ;;                               (or (first (filter #(= (last %) :data) (ut/kvpaths honey-sql))) [:nope])))
                    ;;         cached?     (if (not client-cache?)
                    ;;                       false
                    ;;                       (try (ut/ne? (get @literal-data-map {:data literals}))
                    ;;                            (catch Exception _ false)))]
                    ;;     (if (and (not cached?) (or (list? literals) (vector? literals)))
                    ;;       (do (if data-literal-code?
                    ;;             ;(enqueue-task
                    ;;             (qp/serial-slot-queue :serial-data-literal-code client-name ;:general
                    ;;             ;(ppy/execute-in-thread-pools :general-serial
                    ;;                                   (fn []
                    ;;                                     (let [;output (evl/run literals) ;; (and repl-host repl-port)
                    ;;                                           literals    (walk/postwalk-replace
                    ;;                                                        {'cie-to-hex     'rvbbit-backend.util/cie-to-hex
                    ;;                                                         'hue-to-hex     'rvbbit-backend.util/hue-to-hex
                    ;;                                                         'hex-to-cie     'rvbbit-backend.util/hex-to-cie
                    ;;                                                         'hex-to-hue-sat 'rvbbit-backend.util/hex-to-hue-sat
                    ;;                                                         'http-call      'rvbbit-backend.websockets/http-call
                    ;;                                                         'flatten-map    'rvbbit-backend.websockets/flatten-map}
                    ;;                                                        literals)
                    ;;                                           output-full (evl/repl-eval literals repl-host repl-port client-name (keyword (str "honey-literal-" (hash honey-sql))) ui-keypath)
                    ;;                                           output      (last (get-in output-full [:evald-result :value]))
                    ;;                                           output-full (-> output-full
                    ;;                                                           (assoc-in [:evald-result :output-lines]
                    ;;                                                                     (try (count (remove empty?
                    ;;                                                                                         (get-in output-full
                    ;;                                                                                                 [:evald-result :out])))
                    ;;                                                                          (catch Exception _ 0)))
                    ;;                                                           (assoc-in [:evald-result :values]
                    ;;                                                                     (try (count (last (get-in output-full
                    ;;                                                                                               [:evald-result :value])))
                    ;;                                                                          (catch Exception _ 0))))]
                    ;;                                       (swap! literal-data-output assoc ui-keypath output-full)
                    ;;                                       (try (insert-rowset output
                    ;;                                                           cache-table
                    ;;                                                           (first ui-keypath)
                    ;;                                                           client-name
                    ;;                                                           (keys (first output))
                    ;;                                                           (sql/create-or-get-client-db-pool client-name)
                    ;;                                                           client-name)
                    ;;                                            (catch Exception e
                    ;;                                              (do (reset! data-literal-insert-error
                    ;;                                                          ["Data struct not a proper 'rowset', see console log ^" e])
                    ;;                                                  nil)))
                    ;;                                       (async/>!! completion-channel true) ;; unblock
                    ;;                                       )))
                    ;;             ;(enqueue-task
                    ;;             (qp/serial-slot-queue :serial-data-literal client-name ;:general
                    ;;             ;(ppy/execute-in-thread-pools :general-serial
                    ;;                                   (fn []
                    ;;                                     (insert-rowset literals
                    ;;                                                    cache-table
                    ;;                                                    (first ui-keypath)
                    ;;                                                    client-name
                    ;;                                                    (keys (first literals))
                    ;;                                                    (sql/create-or-get-client-db-pool client-name)
                    ;;                                                    client-name)
                    ;;                                     (async/>!! completion-channel true) ;; unblock
                    ;;                                     )))
                    ;;           (async/<!! completion-channel) ;; BLOCK until our threaded job is
                    ;;           (swap! literal-data-map assoc {:data literals} cache-table)
                    ;;           (walk/postwalk-replace @literal-data-map honey-sql))
                    ;;       (walk/postwalk-replace @literal-data-map honey-sql)))
                    ;;   honey-sql)
                    ]

                (swap! materialized-full-queries assoc-in [client-name (last ui-keypath)] (ut/deep-remove-keys2 honey-sql [:page :offset :limit]))
                ;; ^^ needed for pivot table look ups and proper backend subq creation

                (if (and (not (= :styles (last ui-keypath))) (not sniff?) cache? client-cache?)
                  (do (ut/pp [:*cache-hit @q-calls kind ui-keypath connection-id client-name] ;:honey-sql
                             )
                      (-> ;(get @sql-cache req-hash)
                       (get-from-cache req-hash)
                       (assoc :cached? true)
                       (assoc :query-ms nil))) ;; return output ;; :query-ms query-ms
                  (let [page-num page ;(get honey-sql :page)
                        per-page-limit (cond
                                         ;;(cstr/includes? (str ui-keypath) "-hist-") 50 ;; tiny sample for history view
                                         (= page-num -1) 50000   ;; meh
                                         (= page-num -2) 1000000 ;; yikes? revisit TODO
                                         (= page-num -3) 1000000 ;; yikes. revisit TODO
                                         (= page-num -4) 5000000 ;; yikes! revisit TODO
                                         :else           per-page-limit)
                        honey-sql (if literal-data?
                                    honey-sql ;; dont mutate literal data rowsets
                                    (cond (and page-num (and (not (= page-num -2))
                                                             (not (= page-num -3))
                                                             (not (= page-num -4))
                                                             (not (= page-num -1))))
                                          (assoc (dissoc honey-sql :page) :offset (* (- page-num 1) per-page-limit))
                                          (or (= page-num -1)
                                              (= page-num -3)
                                              (= page-num -4)
                                              (= page-num -2)) (dissoc honey-sql :page)
                                          :else honey-sql))
                        honey-sql-str (when (not literal-data?) ;; no query!
                                        (if (or (= page-num -1)
                                                (= page-num -3)
                                                (= page-num -4)
                                                (= page-num -2)) ;; or limit
                                          (to-sql honey-sql target-db-type)
                                          (to-sql (assoc honey-sql
                                                         :limit (if
                                                                 (or
                                                                  (cstr/includes? (str ui-keypath) "query-preview")
                                                                  (cstr/includes? (str ui-keypath) "query_preview")
                                                                  (cstr/includes? (str ui-keypath) "-hist-"))
                                                                  50 200)) target-db-type)))
                        honey-sql-str2 (first honey-sql-str)
                        ;;;_ (async/thread (get-clover-sql-training clover-sql honey-sql-str2))
                        ;; _ (swap! materialized-full-queries assoc-in [client-name (last ui-keypath)] honey-sql)
                       ;; _ (ppy/execute-in-thread-pools :harvest-clover-training (fn [] (get-clover-sql-training clover-sql honey-sql-str2)))
                        honey-result (timed-expr (if literal-data?
                                                   honey-sql ;; which in this case IS the data
                                                   ((if client-cache?
                                                      cached-sql-query
                                                      sql-query) target-db honey-sql-str ui-keypath connection-id)))
                        row-count (try
                                    (get-in
                                     (sql-query target-db
                                                (-> (str "select count(1) as cnt from (" honey-sql-str2 ") subq")
                                                    (cstr/replace "LIMIT 200" "")
                                                    (cstr/replace "limit 200" ""))
                                                ui-keypath connection-id) [0 :cnt]) (catch Exception _ -1))
                        query-ms (get honey-result :elapsed-ms)
                        honey-result (get honey-result :result)
                        query-error? (get (first honey-result) :database_says)
                        safe-result (mapv ut/stringify-non-primitives2 honey-result)
                        honey-meta (get-query-metadata safe-result honey-sql (surveyor/db-typer target-db) connection-id)
                        fields (get honey-meta :fields)
                        ;; dates (remove nil? (for [[k v] fields] (when (cstr/includes? (get v :data-type) "date") k)))
                        is-meta? (= 3 (count ui-keypath))
                        is-condi? (= (keys fields) '(:v)) ;; condi eval call
                        ;; result (if (empty? dates)
                        ;;          (vec (take per-page-limit honey-result))
                        ;;          (vec (for [r (take per-page-limit honey-result)]
                        ;;                 (into {} (for [[k v] r] (if (some #(= % k) dates) {k (str v)} {k v}))))))
                        result honey-result ;; temp removing date stripper
                        result (if (and query-error? @data-literal-insert-error)
                                 (vec (into result
                                            [{:database_says (str (first @data-literal-insert-error))}
                                             {:database_says (str (last @data-literal-insert-error))}]))
                                 result) ;; add extra error data from code evaL
                        result (if (get orig-honey-sql :transform-select)
                                 (do (ut/pp [:transform (assoc orig-honey-sql :from [:data])])
                                     (let [res (ts/transform (assoc orig-honey-sql :from [:data]) result)] res))
                                 result)
                        result (if has-rql? ;; RQL deprecated as well due to proper :post-process-fn REPL integration? ... except read-edn? double-check. TODO
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
                                                from-kp       [1 :queries (last ui-keypath) :from]
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
                        ;; nivo-calendar-rowset? (and (get-in result [0 :dday]) (get-in result [0 :vvalue]))
                        ;; result (if nivo-calendar-rowset?
                        ;;          ;; ^^ ugly hack for nivo calendar requiring H2 reserved words as keys...
                        ;;          (walk/postwalk-replace {:dday :day :vvalue :value} result) result)


                          ;; [{:formula "excel-formula"
                          ;;   :params {:formula "=sales / OFFSET(sales, -1)"
                          ;;            :result-field "growth_rate"}}]



                        ;; _ (ut/pp [:fffff (vec (for [s stack-maps]
                        ;;                         {:formula "excel-formula"
                        ;;                          :params {:formula (get s :formula)
                        ;;                                   :result-field (get s :name)}}))])

                        ;;_ (ut/pp [:duck-shadow duck-shadow?])
                        orig-keypath (when duck-run? [(keyword (cstr/replace (str (first ui-keypath)) ":stacked/" ""))])
                        result (cond stack-maps?
                                     (let [_ (when orig-keypath
                                               (🦆💬> real-client-name orig-keypath ["running stack pipelines" 15]))
                                           {:keys [result steps]}
                                           (try
                                             (stacks/execute-pipeline result
                                                                      (vec (for [s stack-maps
                                                                                 :when (or
                                                                                        (= (get s :stack-type) "clojure")
                                                                                        (= (get s :stack-type) "excel-formula"))]
                                                                             {:formula (get s :stack-type)
                                                                              ;;;:client-name client-name :query-key (first ui-keypath) ;; add meta for error pathing
                                                                              :params {:formula (get s :formula)
                                                                                       :result-field (get s :name)}})) nil
                                                                      (when orig-keypath (fn [status] (🦆💬> real-client-name orig-keypath [status 15]))))
                                             (catch Throwable e
                                               (let [res [{:error "stack-maps-fn error" :vval (str e) :ui-keypath ui-keypath}]]
                                                 (ut/pp [:stack-maps-fn-error res ui-keypath])
                                                 res)))
                                           _ (🦆💬> real-client-name orig-keypath ["stack pipelines complete" 15])]
                                                                           ;;  (ut/pp [:steps steps])
                                                                            ;(when (not duck-shadow?) ;; push added fields back to client for each transform since some are implicit
                                       (client-mutate client-name {[:panels panel-key :stacks-meta (first ui-keypath)] steps} true);)
                                       result)

                                     (not (nil? post-process-fn))
                                     (try ((eval post-process-fn) result)
                                          (catch Throwable e
                                            (let [res [{:error "post-process-fn error" :vval (str e) :ui-keypath ui-keypath}]]
                                              (ut/pp [:post-process-fn-error res ui-keypath])
                                              res)))
                                     :else result)
                        ;; _ (ut/pp [:fields (keys (first result))])

                        ;;_ (when orig-keypath (🦆💬> real-client-name orig-keypath ["fetching simple metadata" 15]))
                        honey-meta (when (not duck-run?)
                                     (if (or (get orig-honey-sql :transform-select)
                                             has-rql? ;query-error?
                                           ;;nivo-calendar-rowset?
                                             post-process-fn
                                             (ut/ne? stack-maps)
                                             (get orig-honey-sql :data)) ;; TODO< this is ugly
                                       (get-query-metadata result honey-sql (surveyor/db-typer target-db) connection-id) ;; get new meta on
                                       honey-meta))
                        rabbit-code-fields (vec (for [[k v] (get honey-meta :fields)
                                                      :when (= (get v :data-type) "rabbit-code")] k))
                        result (if (empty? rabbit-code-fields)
                                 result
                                 (mapv (fn [row-map]
                                         (reduce (fn [acc field]
                                                   (let [value (get row-map field)]
                                                     (assoc acc field
                                                            (if (or (map? value)
                                                                    (vector? value)
                                                                    (list? value)
                                                                    (set? value))
                                                              value  ; Leave collections unchanged
                                                              (try
                                                                (edn/read-string value)
                                                                (catch Exception e
                                                                  {:error (str "Failed to parse EDN: " (.getMessage e))
                                                                   :raw-value value}))))))
                                                 row-map
                                                 rabbit-code-fields))
                                       result))
                        ;; _ (ut/pp [:honey-meta ui-keypath honey-meta])
                        fields (get honey-meta :fields) ;; lol, refactor, this is cheese
                        ;; sniff-worthy? (and (not is-meta?)
                        ;;                    (not= page -3)
                        ;;                    (not has-rql?) ;;; temp since insert will fail dur to not being
                        ;;                    ;(not (= (ut/dissoc-recursive honey-sql)
                        ;;                    ;        (ut/dissoc-recursive (get-in @sql/query-history
                        ;;                    ;                                     [cache-table-name :honey-sql]))))
                        ;;                    (ut/ne? (flatten result))
                        ;;                    (not (cstr/includes? (str ui-keypath) "-hist-"))
                        ;;                    (not query-error?)
                        ;;                    (not is-condi?)
                        ;;                    (not (cstr/starts-with? cache-table-name "kick"))
                        ;;                    (not (= (get honey-sql :limit) 111))
                        ;;                    (not (some #(cstr/starts-with? (str %) ":query-preview") ui-keypath)))
                        sniffable? (or sniff? ;; <-- req from client only?
                                       (and (not is-meta?)
                                            false ;; otherwise never? only on manual for now
                                            (not query-error?)
                                            (not is-condi?)
                                            (not (cstr/starts-with? cache-table-name "kick"))
                                            (not (= (get honey-sql :limit) 111)) ;; a "base table
                                            (not (some #(cstr/starts-with? (str %) ":query-preview") ui-keypath))))
                        _ (when orig-keypath (🦆💬> real-client-name orig-keypath ["sending for insertion" 15]))
                        repl-output (ut/limited (get-in @literal-data-output [ui-keypath :evald-result] {}))
                        honey-hash (hash [honey-sql ui-keypath])
                        output {:kind           kind
                                :ui-keypath     ui-keypath
                                :result         (mapv ut/stringify-non-primitives2 result) ;; safe-result
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
                                                        ;;nivo-calendar-rowset?
                                                        post-process-fn
                                                        (ut/ne? stack-maps)
                                                        (get orig-honey-sql :data))
                                                  (keys fields)
                                                  (get @sql/map-orders honey-sql-str))}
                        ;result-hash (hash result)
                        error-result? (true? (get (first result) :database_says))
                        ]

                    ;; (when (cstr/includes? (str ui-keypath) "advanced") (ut/pp [:output (get output :honey-meta) :!!]))
                     ;(enqueue-task-sql-meta

                    ;; (when true
                    ;; ;;  (or deep-meta?
                    ;; ;;     (and (not (= client-name :rvbbit))
                    ;; ;;              ;;  (not= (hash honey-sql)
                    ;; ;;              ;;        (get-in @sniff-meta-guard [ui-keypath client-name]))
                    ;; ;;     )) ;; sniff & push meta only on new sql construct
                    ;;   ;; (ut/pp [:sniff-meta! ui-keypath client-name])

                    ;;   (qp/slot-queue :sql-meta client-name
                    ;;                  (fn [] (sniff-meta ui-keypath honey-sql fields target-db client-name connection-id deep-meta?)))
                    ;;   )

                    ;;(if sniffable? ;; aka reco check. need to rename since it's hella confusing with all the 'sniffing'.
                      ;; (doall
                      ;;  (do
                      ;;    (swap! deep-run-list conj filtered-req-hash) ;; mark this as run
                      ;;    ;(enqueue-task
                      ;;    ;(qp/serial-slot-queue :general-serial :general
                      ;;    (ppy/execute-in-thread-pools :general-serial
                      ;;                          (fn []
                      ;;                            (ut/pp :kick-sniff)
                      ;;                            (push-to-client ui-keypath [:reco-status (first ui-keypath)] client-name 1 :reco :started)
                      ;;                            (swap! sql/query-history assoc cache-table-name {:honey-sql honey-sql :connection-id connection-id})
                      ;;                            (let [result (vec (for [r result] (assoc r :rrows 1)))] ;;; hack
                      ;;                              ;(insert-rowset result cache-table-name (first ui-keypath) client-name (keys (first result)))
                      ;;                              ;(write-transit-data honey-meta (first ui-keypath) client-name (ut/unkeyword cache-table-name) true)
                      ;;                              ;)
                      ;;                            (doall
                      ;;                             (do
                      ;;                               (do (ut/pp [[:recos-started-for (first ui-keypath)] :via client-name ui-keypath filtered-req-hash])
                      ;;                                   (push-to-client ui-keypath
                      ;;                                                   [:reco-status (first ui-keypath)]
                      ;;                                                   client-name
                      ;;                                                   1
                      ;;                                                   :reco
                      ;;                                                   :started))
                      ;;                               (doall
                      ;;                                ;;; captured-sniff [src-conn-id base-conn-id target-db src-conn result-hash & [sql-filter quick? resultset client-name]]
                      ;;                                (let [run-it         (timed-expr (cruiser/captured-sniff "cache.db"
                      ;;                                                                                         connection-id
                      ;;                                                                                         target-db
                      ;;                                                                                         cache-db
                      ;;                                                                                         result-hash
                      ;;                                                                                         [:= :table-name cache-table-name]
                      ;;                                                                                         false ;nil
                      ;;                                                                                         result ;;nil
                      ;;                                                                                         client-name))
                      ;;                                      reco-count-sql {:select [[[:count 1] :cnt]]
                      ;;                                                      :from   [:combos]
                      ;;                                                      :where  [:= :table-name cache-table-name]}
                      ;;                                      reco-count     (get-in (sql-query system-db
                      ;;                                                                        (to-sql reco-count-sql)
                      ;;                                                                        [:reco-count :client-status])
                      ;;                                                             [0 :cnt])]

                      ;;                                  (ut/pp [[:recos-finished-for (first ui-keypath)] :via client-name ui-keypath filtered-req-hash  reco-count
                      ;;                                                                                                                                  (get run-it :elapsed-ms)])
                      ;;                                  (push-to-client ui-keypath
                      ;;                                                  [:reco-status (first ui-keypath)]
                      ;;                                                  client-name
                      ;;                                                  1
                      ;;                                                  :reco
                      ;;                                                  :done
                      ;;                                                  reco-count
                      ;;                                                  (get run-it :elapsed-ms))))))) ;; vertica
                      ;;                            ))))

                      ;(ut/pp [:sniff-worthy? sniff-worthy?  :ui-keypath ui-keypath :client-name client-name])

                      ;; (when false ;;sniff-worthy? ;; want details, but not yet the full expensive meta (again, naming is terrible and confusing)
                      ;;   ;(enqueue-task
                      ;;   ;(ut/pp [:sniff-worthy? sniff-worthy?  :ui-keypath ui-keypath :client-name client-name])

                      ;;   (when
                      ;;    (not= (hash honey-sql)
                      ;;               (get @quick-sniff-hash [cache-table-name client-name])
                      ;;               ;(not= target-db cache-db)
                      ;;               )
                      ;;     (qp/serial-slot-queue :general-serial :general
                      ;;   ;(ppy/execute-in-thread-pools :general-serial
                      ;;                           (fn []
                      ;;                             (swap! sql/query-history assoc
                      ;;                                    cache-table-name
                      ;;                                    {:honey-sql honey-sql :connection-id connection-id})
                      ;;                             (doall
                      ;;                              (let [resultv (vec (for [r result] (assoc r :rrows 1)))
                      ;;                                    resultv (mapv ut/stringify-non-primitives resultv)] ;;; hack to
                      ;;                             ;;  (when (not= (first ui-keypath) :solvers) ;; solvers have their
                      ;;                             ;;    (if false ;snapshot-cache?
                      ;;                             ;;      (insert-rowset-snap result
                      ;;                             ;;                          cache-table-name
                      ;;                             ;;                          (first ui-keypath)
                      ;;                             ;;                          client-name
                      ;;                             ;;                          (conj (keys (first result)) :snapshot_ds))
                      ;;                             ;;      (insert-rowset resultv
                      ;;                             ;;                     cache-table-name
                      ;;                             ;;                     (first ui-keypath)
                      ;;                             ;;                     client-name
                      ;;                             ;;                     (keys (first resultv)))))
                      ;;                                (cruiser/captured-sniff "mem-cache.db"
                      ;;                                                        connection-id
                      ;;                                                        target-db
                      ;;                                                        sql/mem-cache-db
                      ;;                                                        result-hash
                      ;;                                                        [:= :table-name cache-table-name]
                      ;;                                                        true
                      ;;                                                        resultv
                      ;;                                                        client-name)
                      ;;               ;; (ut/pp [[:quick-sniff-for (first ui-keypath)] :via client-name ui-keypath
                      ;;               ;;         filtered-req-hash])
                      ;;                                ))))
                      ;;     (swap! quick-sniff-hash assoc [cache-table-name client-name] (hash honey-sql))))

                          ;)


                    ;; _  _ _ ___     ___  ____ ____ ___ _   _    ____ ____ ____    ____ _    _    __.
                    ;; |  | |   /     |__] |__| |__/  |   \_/     |___ |  | |__/    |__| |    |     _]
                    ;;  \/  |  /__    |    |  | |  \  |    |      |    |__| |  \    |  | |___ |___  .

                    ;; (almost) everyone gets shape rotated  ;;
                    (when (or sniffable? ;; manual sniff. almost completely unneeded now?
                              (and ;;;false
                               ;(not= connection-id "system-db")
                               ;(not (cstr/includes? (str connection-id) "system"))
                               (not (cstr/ends-with? (str (first ui-keypath)) "-search-like"))
                               (not error-result?)
                               (not= connection-id client-name)
                               (not= client-name :rvbbit)
                               (not (cstr/includes? (str panel-key) "reco-preview"))
                               (not (cstr/includes? (str cache-table-name) "query_preview"))
                               (not (cstr/includes? (str cache-table-name) "query-preview"))
                               (not (cstr/starts-with? (str cache-table-name) "kick"))
                               (not= connection-id (cstr/replace (str client-name) ":" ""))
                               ;(not (some #(= % [client-name (hash (select-keys honey-sql [:select :from :group-by]))]) @auto-viz-runs))
                               ))
                      (query-shape-rotation client-name panel-key ui-keypath connection-id honey-hash honey-sql row-count))


                    (when (and (not error-result?) duck-shadow? stack?)
                      ;;(ut/pp [:running-duck-shadow?])
                      (insert-stacked-duck-shadows panel-key ui-keypath og-honey-sql connection-id client-name))


                    (do ;(swap! sql-cache assoc req-hash output)
                      ;; (kick client-name "kick-test!" (first ui-keypath) ;; <-- adds a kick kit panel entry for each query run, but adds up fast...
                      ;;       "query-log"
                      ;;       (str "query-log " (first ui-keypath))
                      ;;       (str "query-log " (first ui-keypath))
                      ;;       [(str (ut/get-current-timestamp) " - query ran in " query-ms " ms. ")
                      ;;        [:text honey-sql-str2]
                      ;;        [:edn (ut/truncate-nested (get honey-meta :fields))]
                      ;;        ])
                      (when (and (not error-result?) (not (cstr/ends-with? (str (first ui-keypath)) "-search-like"))) ;;true ;(not (cstr/starts-with? (str panel-key) ":reco-"))
                        ((if (= page -2) ;;  -2 = materialize a full pull transit file for kit injestion
                           ppy/execute-in-thread-pools-but-deliver ;; we DO want to block in this situation...
                           ppy/execute-in-thread-pools) :data-io-ops
                                                        (fn []
                                                          (try
                                                            (let [modded-meta (-> (get output :result-meta)
                                                                                  (assoc :original-honey (get output :original-honey))
                                                                                  (assoc :connection-id (get output :connection-id)))
                                                                  solver-edn-fp "./defs/solvers.edn"
                                                                  materialize-solver-name (keyword (str "materialize-" (cstr/replace (str (first ui-keypath)) ":" "")))
                                                                  cli-cache-table-name (keyword (cstr/replace (str "cached-" (ut/unkeyword cache-table-name)) "_" "-"))
                                                                  resultv (vec (for [r result] (assoc r :rrows 1)))
                                                                  db-dest (if (= dest :memory)
                                                                            ["cache.db.memory" cache-db-memory]
                                                                            ["cache.db" cache-db])
                                                                  db-cn (last db-dest)
                                                                  db-str (first db-dest)]
                                                              (swap! db/query-metadata assoc-in [client-name (first ui-keypath)] modded-meta)
                                                              (try
                                                                (when (or (= page -2) (= page -3)) ;; no need to write out normal queries
                                                                  (ext/write-transit-data modded-meta (first ui-keypath) client-name (ut/unkeyword cache-table-name) true)
                                                                  (ext/write-transit-data result (first ui-keypath) client-name (ut/unkeyword cache-table-name)))
                                                                (catch Exception e (ut/pp [:error-writing-transit-file ui-keypath client-name (str e)])))
                                                              (when (not query-error?)
                                                                (swap! times-atom assoc times-key (conj (get @times-atom times-key) query-ms)))
                                                              (swap! db/client-panels-metadata assoc-in [client-name panel-key :queries (first ui-keypath)] (get output :result-meta))
                                                              (swap! db/client-panels-data assoc-in [client-name panel-key :queries (first ui-keypath)] result)


                                                              (when true ;(not (get-in @db/last-solvers-data-atom [(first ui-keypath)]))
                                                                (ppy/execute-in-thread-pools ;; revist lol
                                                                 (keyword (str "serial-" client-name "data-ops"))
                                                                 (fn []
                                                                   (let [;_ (ut/pp [:serial-data-start client-name ui-keypath])
                                                                         full-res (sql-query target-db (to-sql (dissoc honey-sql :limit)))]
                                                                     (swap! db/last-solvers-data-atom assoc-in [(first ui-keypath)] full-res)
                                                                     (d/transact-kv db/ddb [[:put "repl-data" (hash [client-name (first ui-keypath)]) full-res]])
                                                                     (ext/write-transit-data modded-meta (first ui-keypath) client-name (ut/unkeyword cache-table-name) true)
                                                                     (ext/write-transit-data full-res (first ui-keypath) client-name (ut/unkeyword cache-table-name))
                                                                     ;(ut/pp ["🐖" :inserted :data client-name (first ui-keypath) (count full-res)])
                                                                     ))))

                                                              ;(swap! db/last-solvers-data-atom assoc-in [(first ui-keypath)] result)
                                                              (swap! db/solver-status assoc-in [:metadata (first ui-keypath)] (hash (get output :result-meta)))
                                                          ;; ^^ <--- expensive mem-wise, will pivot to off memory DB later if use cases pan out
                                                              (when (= page -3) ;; materialize and schedule a new solver job if not exists
                                                            ;;; insert a new solver job if not exists :materialize-query-name
                                                            ;(when true ;(not (get @solvers-atom materialize-solver-name))
                                                                (alert! client-name
                                                                        [:box :child (str "Materializing " cache-table-name " into " db-str "...")]
                                                                        16
                                                                        2.1
                                                                        8)
                                                                (swap! solvers-atom assoc materialize-solver-name
                                                                       {:signal :signal/daily-at-7am
                                                                        :type :sql
                                                                        :snapshot? false
                                                                        :data (-> orig-honey-sql
                                                                                  (assoc :page -4)
                                                                                  (assoc :connection-id connection-id))})
                                                                (Thread/sleep 300) ;; small block for grace.
                                                                (fpop/freeze-atom solver-edn-fp)
                                                              ;; (ut/zprint-file solver-edn-fp {:style [:justified-original] :parse-string? true
                                                              ;;                                :comment {:count? nil :wrap? nil} :width 120
                                                              ;;                                :map {:comma? false :sort? false}})

                                                              ;; (push-to-client [ui-keypath panel-key cli-cache-table-name] [] client-name 1 :push-query
                                                              ;;                 {:select (vec (remove #{:rows} (keys (first resultv))))
                                                              ;;                  :connection-id "cache.db"
                                                              ;;                  :from [[(keyword cache-table-name)
                                                              ;;                          (ut/gen-sql-sql-alias)]]})
                                                                (future ;; wait a bit so the first refresh of the new table isn't a missing table error
                                                                  (Thread/sleep 3000) ; Delay for 3 seconds
                                                                  (push-to-client [ui-keypath panel-key cli-cache-table-name] [] client-name 1 :push-query
                                                                                  {:select (vec (remove #{:rows} (keys (first resultv))))
                                                                                   :connection-id db-str ;;"cache.db"
                                                                                   :from [[(keyword cache-table-name)
                                                                                           (ut/gen-sql-sql-alias)]]}))
                                                                (ppy/execute-in-thread-pools
                                                                 :materialize-insert-counter
                                                                 (fn [] (wait-for-value (count resultv)
                                                                                        (fn [] (get-in (try
                                                                                                         (sql-query db-cn (str "select count(*) as tt from " cache-table-name))
                                                                                                         (catch Exception _ 0)) [0 :tt] 0))
                                                                                        [cli-cache-table-name] ;;ui-keypath
                                                                                        client-name)))
                                                                (reload-solver-subs)
                                                                (ut/zprint-file solver-edn-fp {:style [:justified-original] :parse-string? true
                                                                                               :comment {:count? nil :wrap? nil} :width 120
                                                                                               :map {:comma? false :sort? false}});)

                                                              ;;; do the inserts to cache.db
                                                                (insert-rowset resultv ;; insert into client cache sql db for potential cross-joins
                                                                               cache-table-name ;; ^^ (TODO make optional and add option for full row cache, but regular UI SQL behavior)
                                                                               (first ui-keypath)
                                                                               client-name
                                                                               (keys (first resultv))
                                                                               db-cn ;;(sql/create-or-get-client-db-pool client-name)
                                                                               client-name)
                                                                ;; (cruiser/lets-give-it-a-whirl-no-viz db-str ;"cache.db" ;; need to 'post sniff' to give metadata for the UI
                                                                ;;                                      db-cn
                                                                ;;                                      system-db
                                                                ;;                                      cruiser/default-sniff-tests
                                                                ;;                                      cruiser/default-field-attributes
                                                                ;;                                      cruiser/default-derived-fields
                                                                ;;                                      cruiser/default-viz-shapes
                                                                ;;                                    ;[:= :table-name table-name-str]
                                                                ;;                                      )

                                                                (alert! client-name
                                                                        [:box :child (str "Materializing " (ut/nf (count resultv)) " rows done for " cache-table-name " into " db-str "...")]
                                                                        16
                                                                        2.1
                                                                        8)))
                                                            (catch Exception e
                                                            ;(ut/pp [:data-ops-inner-loop-exception e])
                                                              (println "Error :data-ops-inner-loop" (.getMessage e))
                                                              (.printStackTrace e))))))

                      ;; (when client-cache? (insert-into-cache req-hash output))

                      (🦆💬> real-client-name ui-keypath [(str (ut/nf query-ms) " ms") 100])

                      (when (and (and
                                  (not (= connection-id "system-db"))
                                  (not (= connection-id "systemh2-db"))
                                  ;; (not (= connection-id "history-db"))
                                  (not (= connection-id "flow-db"))
                                  (not (cstr/starts-with? (str ui-keypath) "[:c")) ;; condi queries
                                  (not (cstr/includes? (str ui-keypath) "query-preview")))
                                 (get @config/settings-atom :show-query-times? false))
                        (alert! client-name
                                [:h-box :children
                                 [[:md-icon :md-icon-name "ri-dashboard-line"
                                   :style {:font-size "43px" :padding-right "10px" :opacity 0.5}]
                                  [:v-box
                                   :style {:font-size "16px"}
                                   :children [[:box
                                               :style {:font-size "24px"}
                                               :child (str (ut/nf (count result)) " row(s) in " (ut/nf query-ms) " ms")]
                                              [:box
                                               :style {:font-size "9px"}
                                               :child [:edn honey-sql]]
                                              [:box
                                               :style {:opacity 0.5}
                                               :child (str ui-keypath " @ " connection-id)]]]]]
                                12
                                5.1
                                5))

                      (if (or (= page -2) (= page -3)) ;; otherwise we ruin the client with a huge dump
                        (assoc output :result (vec (take 200 (get output :result))))
                        output))))))
             (catch Exception e
               (do (ut/pp [:honeyx-OUTER-LOOP-EXCEPTION (str e) :connection-id connection-id ui-keypath (str honey-sql)])
                   {:kind        kind
                    :ui-keypath  ui-keypath
                    :result      [{:database_says "execution error"} {:database_says (str e)}]
                    :result-meta {}
                    :sql-str     ""
                    :query-ms    nil
                    :cached?     false
                    :repl-output (assoc (ut/limited (get-in @literal-data-output [ui-keypath :evald-result] {}))
                                        :error (str e))
                    :client-name client-name
                    :map-order   []}))))]
     (doall (if data-call? ;; deprecated?
              (doall (let [async-result-chan (go (let [result-chan (queue-runstream runstream)]
                                                   (println "Waiting for result...") ; Debugging
                                                   (<! result-chan)))] ; This returns a channel
                       (do (println "Getting result from async operation...")
                           (async/<!! async-result-chan)))) ; Blocking take from the chan
              (runstream))))))





(def kit-client-mapped (atom {}))

;; (ut/pp @kit-client-mapped)

(defn read-if-keyworded-str [x]
  (if (cstr/starts-with? (str x) ":") (try (edn/read-string x) (catch Exception _ :error!)) (keyword x)))

(defn kit-rows-to-map [rowset]
  (try (let [mapped (into {}
                          (for [[k v] (group-by :kit_name rowset)]
                            {(read-if-keyworded-str k)
                             {(read-if-keyworded-str (get (first v) :item_name))
                              (into {}
                                    (for [[k v] (group-by :item_key v)]
                                      {(edn/read-string k) (merge (edn/read-string (get (first v) :item_options))
                                                                  {;:id (get (first v) :id)
                                                                   :data (vec (for [d v]
                                                                                (merge {:id (get d :id)}
                                                                                       (edn/read-string
                                                                                        (get d :item_data)))))})}))}}))]
         {:kits mapped})
       (catch Exception _ {})))

(defn insert-kit-data
  [output query-hash ui-keypath ttype kit-name elapsed-ms & [client-name flow-id]]
  ;(enqueue-task3 ;; blocking for inserts and deletes (while we are one sqlite, will be
  (qp/serial-slot-queue :general-serial :general
  ;(ppy/execute-in-thread-pools :general-serial
                        (fn []
                          (let [output-is-valid? (map? output) ;; basic spec checking
                                kit-keys         (keys output)
                                kkit-name        (if (vector? kit-name) (str (first kit-name)) (str kit-name))
                                output-rows      (vec
                                                  (apply (if (= kit-keys 1) flatten into)
                                                         (for [[k v] output
                                                               :let  [;descr (pr-str (get v :description))
                                                                      rows (get v :data [])
                                                                      rows (if (empty? rows) [{:name "no content sent from kit"
                                                                                               :parameters {}
                                                                                               :step-mutates {}
                                                                                               :ask-mutates {}
                                                                                               :content [[:box :child "Kit executed successfully, but no data sent. Perhaps by design."]]}]
                                                                               rows)
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
                                                                                                   :item_options (pr-str (select-keys v
                                                                                                                                      [:options :parameters :mutates
                                                                                                                                       :description]))
                                                                                                   :item_data    (pr-str i)}]]
                                                                                row-map))]]
                                                           rowset)))]
                            (ut/pp [:inserting-kit-data output-rows])

                            (when (ut/ne? output-rows) ;; give the client a hint through a kick sub that we have data for X
                              (swap! db/kit-atom assoc-in [:kicks client-name]
                                     (vec (distinct (conj (get-in @db/kit-atom [:kicks client-name] [])
                                                          (if (vector? ui-keypath)
                                                            (first ui-keypath)
                                                            ui-keypath))))))

                            (when true ;;(not (= kkit-name ":kick")) ;; clear older kick data for this item name - TODO what about multiple kits on the same card?
                              (sql-exec system-db
                                        (to-sql {:delete-from [:kits]
                                                 :where       [:and
                                                               [:in :item_key (mapv :item_key output-rows)]
                                                               [:= :kit_name kkit-name]
                                                               [:= :item_name (if (vector? ui-keypath) (str (first ui-keypath)) (str ui-keypath))]]})))
                            (swap! kit-client-mapped assoc-in [client-name kkit-name] (kit-rows-to-map output-rows)) ;; *very* weird client row munging moved to server... TODO Aug'24
                            (sql-exec system-db (to-sql {:insert-into [:kits] :values output-rows}))))))

(defmethod wl/handle-request :honey-xcall
  [{:keys [kind ui-keypath honey-sql client-cache? sniff? connection-id panel-key client-name page kit-name clover-sql stack?
           deep-meta?]}]
  (swap! q-calls inc)
  (inc-score! client-name :push)
  ;; (when (keyword? kit-name) ;;; all kit stuff. deprecated?
  ;;   ;(enqueue-task2
  ;;   (qp/serial-slot-queue :general-serial :general
  ;;   ;(ppy/execute-in-thread-pools :general-serial
  ;;                         (fn []
  ;;                           (try ;; save off the full thing... or try
  ;;                             (let [;kit-name :outliers
  ;;                                   _ (push-to-client ui-keypath [:kit-status (first ui-keypath)] client-name 2 kit-name :started)
  ;;                                   output     (query-runstream kind
  ;;                                                               ui-keypath
  ;;                                                               honey-sql
  ;;                                                               false
  ;;                                                               false
  ;;                                                               connection-id
  ;;                                                               client-name
  ;;                                                               -2
  ;;                                                               panel-key
  ;;                                                               clover-sql
  ;;                                                               deep-meta?
  ;;                                                               false) ;; -2 has 1m row limit. yikes.
  ;;                                   fullrows   (get output :result)
  ;;                                   fullrows   (vec (for [r fullrows] (assoc r :rrows 1)))
  ;;                                   kp         (str (ut/unkeyword (get-in output [:ui-keypath 0] "unknown")))
  ;;                                   rows       (count fullrows)
  ;;                                   meta       (-> (get output :result-meta)
  ;;                                                  (assoc :original-honey (get output :original-honey))
  ;;                                                  (assoc :connection-id (get output :connection-id)))
  ;;                                   mpath      (str "./kit-rowsets/" kp ".meta.edn")
  ;;                                   path       (str "./kit-rowsets/" kp ".edn")
  ;;                                   query-hash (hash honey-sql)
  ;;                                   ttype      :queries]
  ;;                               (spit path (pr-str fullrows) :append false)
  ;;                               (spit mpath (pr-str meta) :append false)
  ;;                               (ut/pp [:saved-full-to path rows :rows])
  ;;                               (ut/pp [:repl-exec-for kit-name])
  ;;                               (let [repl-fn      (get-in config/kit-fns [kit-name :fn]) ;'(+ (rand-int 12345)
  ;;                                     repl-command (walk/postwalk-replace
  ;;                                                   {:query honey-sql :query-name (first ui-keypath) :kit-name kit-name :panel-name panel-key}
  ;;                                                   repl-fn)
  ;;                                     _ (when kit-name (ut/pp [:running-kit kit-name]))
  ;;                                     repl-host    (get-in config/kit-fns [kit-name :repl :host])
  ;;                                     repl-port    (get-in config/kit-fns [kit-name :repl :port])
  ;;                                     output-full  (timed-expr (evl/repl-eval repl-command repl-host repl-port client-name kit-name))
  ;;                                     elapsed-ms   (get output-full :elapsed-ms)
  ;;                                     output-full  (get output-full :result) ;; from timed map
  ;;                                     output       (last (get-in output-full [:evald-result :value]))
  ;;                                     _ (ut/pp [:**************************** :repl-output :****************************])
  ;;                                     _ (ut/pp (ut/limited (get-in output-full [:evald-result :out]) 5))
  ;;                                     _ (ut/pp [:**************************** :repl-output :****************************])
  ;;                                     kit-keys     (count (keys output))]
  ;;                                 (insert-kit-data output query-hash ui-keypath ttype kit-name elapsed-ms)
  ;;                                 (push-to-client ui-keypath [:kit-status (first ui-keypath)] client-name 2 kit-name :done kit-keys elapsed-ms))) ;)
  ;;                             (catch Exception e (ut/pp [:save-full-error (str e) :err-end]))))))
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
        (do
          (println "inviz: Getting result from async operation...")
          (async/<!! async-result-chan)))) ; Blocking take from the channel

     (ppy/execute-in-thread-pools-but-deliver :query-runstreams  ; (keyword (str "query-runstream/" (cstr/replace client-name ":" "")))

                                              (fn []
                                                (query-runstream kind ui-keypath honey-sql client-cache? sniff? connection-id
                                                                 client-name page panel-key clover-sql deep-meta? false stack?))

                                              ;; (fn []
                                              ;;   (let [cache-key [kind ui-keypath honey-sql client-cache? sniff? connection-id client-name page panel-key clover-sql deep-meta?]
                                              ;;         cached-result (get @db/query-runstream-cache cache-key)]
                                              ;;     (if (and (not (cstr/includes? (str connection-id) "system"))
                                              ;;              (not (keyword? connection-id))
                                              ;;              cached-result)
                                              ;;       (do
                                              ;;         (ut/pp ["💰" :sql-cache-hit connection-id ui-keypath])
                                              ;;         cached-result)
                                              ;;       (let [result (query-runstream kind ui-keypath honey-sql client-cache? sniff? connection-id client-name page panel-key clover-sql deep-meta? false)]
                                              ;;         (swap! db/query-runstream-cache assoc cache-key result)
                                              ;;         result))))

                                              ))))





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
  (doall
   (let [file-name      (if local-file local-file (get-in request [:edn-params :fname]))
         client-name    (if local-file "system" (str (get-in request [:edn-params :client-name])))
         fdata          (if local-file (slurp local-file) (get-in request [:edn-params :image]))
         file-base-name (first (cstr/split (last (cstr/split file-name #"/")) #"\."))
         file-path      (str "./data/" file-name)
         _   (alert! client-name
                     [:box :child (str "inserting " file-name ".csv into import db...")]
                     10
                     2.1
                     13)
         fdata-map      (doall (ut/csv-data->maps (csv/read-csv fdata)))
         ftypes         (into {} (for [f (keys (first fdata-map))] {f (flatten (take 5 (map (juxt f) fdata-map)))}))
         ftypes-coerce  (into {}
                              (for [[k v] ftypes]
                                {k (cond (try (integer? (Long/parseLong (first v))) (catch Exception _ false)) :integer
                                         (try (float? (Float/parseFloat (first v))) (catch Exception _ false)) :float
                                         :else                                                                 :string)}))
         fdata-map-mod  (vec #_{:clj-kondo/ignore [:unresolved-symbol]}
                         (cp/pfor pool
                                  [row fdata-map] ;; cp/pfor pool
                                  (into {}
                                        (for [[k tt] ftypes-coerce]
                                          {k ;[tt (get row k)]
                                           (try (let [ff  #_{:clj-kondo/ignore [:unresolved-symbol]}
                                                      (get row k nil)
                                                      fff (if (or (empty? ff) (= ff " ") (= ff "")) nil ff)]
                                                  (cond (= tt :float)   (Float/parseFloat fff)
                                                        (= tt :integer) (Long/parseLong fff)
                                                        :else           (str fff)))
                                                (catch Exception _ nil))}))))
         table-name-str  (-> file-base-name str (cstr/replace "-" "_") (cstr/replace ":" ""))
         op-name        (str "csv: " file-path)]
     (ut/pp [:processing op-name])
     (sql-exec system-db
               (to-sql {:insert-into [:status]
                        :columns     [:client_name :op_name :status]
                        :values      [[client-name op-name "processing csv..."]]}))
     (insert-rowset-csv fdata-map-mod file-base-name client-name op-name)
    ;;  (cruiser/lets-give-it-a-whirl-no-viz "import-db" ;;file-path ;; need to 'post sniff' to give metadata for the UI
    ;;                                       import-db
    ;;                                       system-db
    ;;                                       cruiser/default-sniff-tests
    ;;                                       cruiser/default-field-attributes
    ;;                                       cruiser/default-derived-fields
    ;;                                       cruiser/default-viz-shapes
    ;;                                       ;[:= :table-name table-name-str]
    ;;                                       )
     (ut/pp [:saved-csv file-path file-base-name (first fdata-map) ftypes ftypes-coerce (first fdata-map-mod)]))))

(defn save-csv [request]
  (push-to-client [:alerts] [[:box :child (str "inserting "  ".csv into import db...")] 10 3 15] (get-in request [:edn-params :client-name]) -1 (rand-nth [:alert1 :alert2 :alert3]) (or type :alert2))
  (process-csv request)
  (send-edn-success {:status "saved-csv"
                     :screen (first (get request :edn-params))}))

(defn save-alert-notification [client-name name fpath flow? & [theme?]]
  (alert! client-name
          [:v-box :justify :center :style
           {;:margin-top "-6px"
            :opacity 0.7} ;:color (if error? "red" "inherit")}
           :children
           [[:box :style {:font-weight 700 :font-size "11px" :opacity 0.6} :child
             (str (str "successfully saved " (if theme? "theme" (if flow? "flow" "screen"))))]
            [:box :style {:font-weight 700 :font-size "18px"} :child (str name)]
            [:box :style {:font-weight 700 :font-size "11px" :opacity 0.6} :child (str "@ " fpath)]]]
          10
          1.6
          8))

(defn save-alert-notification-pre [client-name nname fpath flow?]
  (alert! client-name
          [:v-box :justify :center :style
           {;:margin-top "-6px"
            :opacity 0.6} ;:color (if error? "red" "inherit")}
           :children [[:box :style {:font-weight 700 :font-size "13px"} :child (str "saving " nname "...")]]]
          10
          0.6
          8))

(defn save [request]
  (ut/pp [:save-coming-in])
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
         (do
           (ut/pp [:error-saving-screen-outer (get-in request [:edn-params :screen-name])
                   (get-in request [:edn-params :client-name] "unknown") e])
           (alert! (get-in request [:edn-params :client-name] "unknown")
                   [:v-box :justify :center :style {:opacity 0.6}
                    :children [[:box :style {:font-weight 700 :font-size "13px"}
                                :child (str "error saving " (get-in request [:edn-params :screen-name]) "...")]
                               [:box :style {:font-weight 700 :font-size "13px"}
                                :child (str e)]]]
                   10
                   0.6
                   8))))
  (send-edn-success {:status "saved" :screen (first (get request :edn-params))}))

(defn save-theme [request]
  (ut/pp [:save-theme-coming-in])
  (try (let [theme-name    (get-in request [:edn-params :theme-name])
             client-name    (get-in request [:edn-params :client-name] "unknown")
             file-base-name (ut/sanitize-name (str theme-name))
             file-path      (str "./themes/" file-base-name ".edn")
             fpath          (ut/abs-file-path file-path)]
         (save-alert-notification-pre client-name theme-name nil false)
         (do (ut/pp [:saved-theme-file file-path])
             (try (ut/pretty-spit file-path (get-in request [:edn-params :image]))
                  (catch Throwable e
                    (do (ut/pp [:pretty-spit-error-bad-edn? e :saving-raw])
                        (spit file-path (get-in request [:edn-params :image])))))
             (save-alert-notification client-name theme-name fpath false true)))
       (catch Exception e
         (do
           (ut/pp [:error-saving-theme-outer (get-in request [:edn-params :theme-name])
                   (get-in request [:edn-params :client-name] "unknown") e])
           (alert! (get-in request [:edn-params :client-name] "unknown")
                   [:v-box :justify :center :style {:opacity 0.6}
                    :children [[:box :style {:font-weight 700 :font-size "13px"}
                                :child (str "error saving " (get-in request [:edn-params :theme-name]) "...")]
                               [:box :style {:font-weight 700 :font-size "13px"}
                                :child (str e)]]]
                   10
                   0.6
                   8))))
  (send-edn-success {:status "saved" :screen (first (get request :edn-params))}))

(defn save-snap [request]
  (try (let [image       (get-in request [:edn-params :image])
             session     (get-in request [:edn-params :session])
             ttime       (get-in request [:edn-params :ttime] (System/currentTimeMillis))
             client-name (get-in request [:edn-params :client-name] "unknown")
             client-name (cstr/replace (str client-name) ":" "")
             nname       (str client-name "-" ttime)
             session     (assoc session :screen-name nname)
             file-path   (str "./assets/snaps/" nname ".jpg")
             sess-path   (str "./assets/snaps/" nname ".edn")]
         (spit sess-path session)
         (ut/save-base64-to-jpeg image file-path))
       (catch Exception e (ut/pp [:save-snap-error e])))
  (send-edn-success {:status "snap-saved" }))

(defn save-snap-block [request]
  (try (let [image       (get-in request [:edn-params :image])
             panel-key   (get-in request [:edn-params :panel-key])
             client-name (get-in request [:edn-params :client-name] "unknown")
             client-name (cstr/replace (str client-name) ":" "")
             panel-key   (cstr/replace (str panel-key) ":" "")
             nname       (str panel-key)
             folder-path (str "./assets/snaps/" client-name "/")
             file-path   (str folder-path nname ".jpg")]
         (ext/create-dirs folder-path)
         (ut/save-base64-to-jpeg image file-path))
       (catch Exception e (ut/pp [:save-snap-error e])))
  (send-edn-success {:status "snap-saved"}))

(defn save-screen-snap [request]
  (try (let [image       (get-in request [:edn-params :image])
             screen-name (get-in request [:edn-params :screen-name] "unknown")
             file-path   (str "./assets/screen-snaps/" screen-name ".jpg")]
         (ut/save-base64-to-jpeg image file-path))
       (catch Exception e (ut/pp [:save-snap-error e])))
  (send-edn-success {:status "screen-snap-saved" }))

(defn save-flow [request]
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


(defn load-screen [request]
  (let [file-path      (or (get-in request [:edn-params :file-path]) (get-in request [:query-params :file-path]))
        flow-data-file (edn/read-string (slurp file-path))]
    (ut/ppln [:loading-screen-from-file file-path])
    (send-edn-success {:image flow-data-file})))

(defn load-flow [request]
  (let [file-path      (or (get-in request [:edn-params :file-path]) (get-in request [:query-params :file-path]))
        flow-data-file (edn/read-string (slurp file-path))]
    (ut/ppln [:loading-flow-from-file file-path])
    (send-edn-success {:image flow-data-file})))


(defn load-flow-history [request]
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


(defn jvm-memory-used []
  (let [mm (ut/memory-used)]
    (ut/pp ["     " :jvm-stats :*cached-queries (ut/nf (count @sql-cache)) :*jvm-memory-used (ut/nf mm) :mb])))


(defn stringify-except [m exclude-keys]
  (reduce (fn [acc k]
            (if (not (contains? (set exclude-keys) k))
              (let [v          (get m k) ;; double quoted strings in SQL is not ideal
                    serializer (if (or (map? v) (vector? v)) pr-str str)]
                (update acc k (fn [_] (serializer v))))
              acc))
          m
          (keys m)))

(defn strunc [s]
  (let [s (str s) limit 80]
    (if (and s (> (count s) limit)) (subs s 0 limit) s)))

(def param-sql-sync-rows (atom 0))

(defn param-sql-sync [] ;; placeholder function, not very scalable - output is correct however
  (let [create-ddl
        "drop table if exists client_items;
         create table if not exists client_items
               (item_key text NULL,
                item_type text NULL,
                item_sub_type text NULL,
                vvalue text NULL,
                is_live boolean NULL,
                sample text NULL,
                display_name text NULL,
                block_meta text NULL,
                ts TIMESTAMP DEFAULT (datetime('now', 'localtime')) NULL) ;"
        display-name (fn [x & [num]] (cstr/join " " (vec (drop (or num 2) x))))
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
                             (ut/keypaths2 @db/params-atom))
        param-rows   (vec (distinct (map (fn [x] (vec (take 3 x))) param-rows)))
        live-clients (keys (client-statuses))
        param-rows   (for [e param-rows]
                       [(cstr/replace (str (first e)) ":" "") "client-params" (cstr/replace (str (second e)) ":" "")
                        (str ":client/" (cstr/replace (cstr/join ">" e) ":" "")) (true? (some #(= (first e) %) live-clients))
                        (strunc (ut/replace-large-base64 (get-in @db/params-atom e))) (display-name e) nil])

        incoming-rows  (ut/keypaths2 @db/incoming-atom)
        incoming-rows  (vec (distinct (map (fn [x] (vec (take 3 x))) incoming-rows)))
        incoming-rows  (for [e incoming-rows]
                         [(cstr/replace (str (first e)) ":" "") "incoming" (cstr/replace (str (second e)) ":" "")
                          (str ":incoming/" (cstr/replace (cstr/join ">" e) ":" "")) false
                          (strunc (ut/replace-large-base64 (get-in @db/incoming-atom e))) (display-name e) nil])

        ;; data-rows  (ut/keypaths2 @db/last-solvers-data-atom)
        ;; data-rows  (vec (distinct (map (fn [x] (vec (take 3 x))) data-rows)))
        ;; data-rows  (for [e data-rows]
        ;;                [(cstr/replace (str (first e)) ":" "") "data" (cstr/replace (str (second e)) ":" "")
        ;;                 (str ":data/" (cstr/replace (cstr/join ">" e) ":" "")) false
        ;;                 (strunc (ut/replace-large-base64 (get-in @db/last-solvers-data-atom e))) (display-name e) nil])

        solver-rows  (ut/keypaths2 @db/last-solvers-atom)
        solver-rows  (vec (distinct (map (fn [x] (vec (take 3 x))) solver-rows)))
        solver-rows  (for [e solver-rows]
                       [(cstr/replace (str (first e)) ":" "") "solvers" (cstr/replace (str (second e)) ":" "")
                        (str ":solver/" (cstr/replace (cstr/join ">" e) ":" "")) false
                        (strunc (ut/replace-large-base64 (get-in @db/last-solvers-atom e))) (display-name e) nil])

        signal-rows  (ut/keypaths2 (into {} (filter (fn [[k _]] (keyword? k)) @db/last-signals-atom)))
        signal-rows  (vec (distinct (map (fn [x] (vec (take 3 x))) signal-rows)))
        signal-rows  (for [e signal-rows]
                       [(cstr/replace (str (first e)) ":" "") "signals" (cstr/replace (str (second e)) ":" "")
                        (str ":signal/" (cstr/replace (cstr/join ">" e) ":" "")) false
                        (strunc (ut/replace-large-base64 (get-in @db/last-signals-atom e))) (display-name e) nil])
        panel-rows   (vec (filter #(or (= (get % 2) :views) (= (get % 2) :queries)) (ut/kvpaths @db/panels-atom)))
        panel-rows   (vec (distinct (map (fn [x] (vec (take 4 x))) panel-rows)))
        panel-rows   (vec (filter #(= (count %) 4) panel-rows)) ;; hacking - merge all this once
        panel-rows   (for [e    panel-rows
                           :let [param? (= :param (str (get e 2)))]]
                       [(cstr/replace (str (first e)) ":" "") (str "client-" (cstr/replace (str (get e 2)) ":" ""))
                        (cstr/replace (str (second e)) ":" "") (str ":panel/" (cstr/replace (cstr/join ">" e) ":" ""))
                        (true? (some #(= (first e) %) live-clients)) (strunc (ut/replace-large-base64 (get-in @db/panels-atom e)))
                        (display-name e 3)
                        (when (not param?)
                          (try (str (ut/replace-large-base64 (-> (get-in @db/panels-atom (vec (drop-last (drop-last e))))
                                                                 (dissoc :views)
                                                                 (dissoc :root)
                                                                 (assoc :natural-key (get e 3))
                                                                 (dissoc :tab)
                                                                 (dissoc :queries))))
                               (catch Exception ee (str ee))))])
        block-rows   (vec (filter #(and (or (= (count %) 5) (or (= (last %) :views) (= (last %) :queries)))
                                        (or (= (get % 3) :views) (= (get % 3) :queries))
                                        (= (second %) :panels))
                                  (ut/kvpaths @db/screens-atom)))
        block-rows   (for [e    block-rows
                           :let [block? (or (= (last e) :views) (= (last e) :queries))
                                 e      (if block? (drop-last e) e)
                                 sample (if (not block?)
                                          (try (ut/replace-large-base64 (-> (get-in @db/screens-atom (vec (drop-last (drop-last e))))
                                                                            (dissoc :views)
                                                                            (dissoc :root)
                                                                            (dissoc :tab)
                                                                            (dissoc :queries)
                                                                            (assoc :natural-key (if block? (get e 3) (get e 4)))))
                                               (catch Exception ee (str ee)))
                                          (try (ut/replace-large-base64 (-> (get-in @db/screens-atom (vec e))
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
                        (strunc (ut/replace-large-base64 (get-in @db/screens-atom e))) (display-name e) (str sample)])
        prows        (vec (apply concat [param-rows flow-rows block-rows panel-rows solver-rows signal-rows incoming-rows  ]))
        _ (reset! param-sql-sync-rows (count prows)) ;; so we can add to stats logger and keep an eye on this easier than SQL querying it
        rows         (vec (for [r prows]
                            (zipmap [:item_key :item_type :item_sub_type :value :is_live :sample :display_name :block_meta] r)))
        _ (reset! autocomplete-clover-param-atom (vec (distinct (filter #(and (not (or (cstr/starts-with? (str %) ":panel/")
                                                                                       (cstr/starts-with? (str %) ":client/")))
                                                                              (<= (count (re-seq #"/" (str %))) 1))
                                                                        (mapv :value rows)))))
        delete-sql   {:truncate :client_items}
        _ (sql-exec systemh2-db (to-sql delete-sql) {:queue :autocomplete})
        rows (mapv #(assoc (dissoc % :value) :vvalue (:value %)) rows)] ;; :value reserved word in H2

    (qp/serial-slot-queue :autocomplete-sql :sql
    ;(ppy/execute-in-thread-pools :general-serial
                          (fn []
                            (doseq [rr (partition-all 100 rows)]
                              (sql-exec systemh2-db
                                        (to-sql {:insert-into [:client_items] :values rr})
                                        {:queue :autocomplete}))))))

(defn update-flow-results>sql []
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
      (sql-exec systemh2-db (to-sql {:delete-from [:flow_results]}))
      (doseq [chunk (partition-all 50 rows)] (sql-exec systemh2-db (to-sql {:insert-into [:flow_results] :values (vec chunk)}))))))

(defn update-channel-history>sql []
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
        le            (sql-query systemh2-db (to-sql {:select [[[:max :eend] :last_end]] :from [:channel_history]}))
        lee           (get-in le [0 :last_end] 0)
        lee           (or lee 0) ;; weird, but sometimes less was nil - TODO
        rows-filtered (vec (filter #(> (get % :end) lee) rows))
        rows-filtered (vec (for [r    rows-filtered
                                 :let [v (str (if (> (count (get r :value)) 3000) (subs (get r :value) 0 3000) (get r :value)))]]
                             (assoc r :value v)))
        rows-filtered (mapv #(assoc (dissoc % :end) :eend (:end %)) rows-filtered)
        rows-filtered (mapv #(assoc (dissoc % :value) :vvalue (:value %)) rows-filtered)
        insert-sql    {:insert-into [:channel_history] :values rows-filtered}]
    (when (ut/ne? rows-filtered)
      (do ;(ut/pp [:channel-history-added>sql :last-end lee :full (count rows) :filtered (count
        (sql-exec systemh2-db (to-sql insert-sql) :update-channel-history>sql)))))

(defn update-fn-history>sql []
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
        le            (sql-query systemh2-db (to-sql {:select [[[:max :eend] :last_end]] :from [:fn_history]}))
        lee           (get-in le [0 :last_end] 0)
        lee           (or lee 0) ;; weird, but sometimes less was nil - TODO
        rows-filtered (vec (filter #(> (get % :end) lee) rows))
        rows-filtered (vec (for [r    rows-filtered
                                 :let [v (if (> (count (get r :value)) 3000) (subs (get r :value) 0 3000) (get r :value))]]
                             (assoc r :value v)))
        rows-filtered (mapv #(assoc (dissoc % :end) :eend (:end %)) rows-filtered)
        rows-filtered (mapv #(assoc (dissoc % :value) :vvalue (:value %)) rows-filtered)
        insert-sql    {:insert-into [:fn_history] :values rows-filtered}]
    (when (ut/ne? rows-filtered)
      (do ;(ut/pp [:fn-history-added>sql :last-end lee :full (count rows) :filtered (count
        (sql-exec systemh2-db (to-sql insert-sql) :update-fn-history>sql)))))

(defn update-live-schedules>sql []
  (let [rows       (try (vec (for [v @flow-db/live-schedules] (stringify-except v [:start :end])))
                        (catch Exception e (do (ut/pp [:update-flow-results>sql-error (str e)]) [])))
        insert-sql {:insert-into [:live_schedules] :values rows}]
    (when (ut/ne? rows)
      (do
        (sql-exec systemh2-db (to-sql {:truncate :live_schedules}))
        (sql-exec systemh2-db (to-sql insert-sql))))))

(def checkpoint-atom (atom {}))

(defn execute-if-changed [input-atom f name]
  (let [current-hash (hash @input-atom)]
    (when (not= (get @checkpoint-atom name) current-hash)
      (swap! checkpoint-atom assoc name current-hash)
      (do ;(ut/pp [:changed-flow-atom! :running name])
        (f)))))

(defn flow-atoms>sql []
  (execute-if-changed flow-db/live-schedules update-live-schedules>sql :live-schedules)
  (execute-if-changed flow-db/results-atom update-flow-results>sql :flow-results)
  (execute-if-changed flow-db/channel-history update-channel-history>sql :channel-history)
  (execute-if-changed flow-db/fn-history update-fn-history>sql :fn-history))


(defn count-watchers [atom]
  (try (let [field (.getDeclaredField clojure.lang.Atom "watches")]
         (.setAccessible field true)
         (-> atom
             (.get field)
             (count)))
       (catch Exception e (println "Error counting watchers:" (.getMessage e)) nil)))

(def channel-counts (atom {}))
(def stats-shadow (fpop/thaw-atom [] "./data/atoms/stats-shadow-atom.edn"))

(defn last-x-items [v x] (let [start (max 0 (- (count v) x))] (subvec v start)))

(defn average-chunks [data]
  (vec (map (fn [chunk]
              {:mem     (float (/ (reduce + (map :mem chunk)) (count chunk)))
               :threads (float (/ (reduce + (map :threads chunk)) (count chunk)))
               :load    (float (/ (reduce + (map :load chunk)) (count chunk)))
               :subs    (float (/ (reduce + (map :subs chunk)) (count chunk)))})
            (partition 50 50 [] data))))

(defn find-max-key [data]
  (let [max-threads (apply max (map :threads data))
        max-subs    (apply max (map :subs data))]
    (if (> max-threads max-subs) :threads :subs)))

(def mps-helper (atom {}))
(def last-stats-row (atom {}))
(def booted (atom nil))
(def clients-alive (atom nil))


;; (ut/pp (client-statuses))


(def agg-cache (atom {}))

;; (ut/pp [:math-cache (count (keys @agg-cache))])

(defn avg-chunks [chunk]
  (let [cache-key (pr-str chunk)
        cache (get @agg-cache cache-key)]
    (if cache
      cache
      (let [sum (apply + chunk)
            count (count chunk)
            average (if (zero? sum)
                      0 ;(do (println "Chunk with sum zero encountered.") 0) ;; Log and return 0 for sum zero
                      (/ sum count))] ;; Calculate average normally
        (swap! agg-cache assoc cache-key average)
        average))))

(defn average-in-chunks [data chunk-size]
  (->> data
       (partition-all chunk-size) ;; vector into chunks
      ;;  (pmap (fn [chunk]
      ;;         (let [sum (apply + chunk)
      ;;               count (count chunk)
      ;;               average (if (zero? sum)
      ;;                         0 ;(do (println "Chunk with sum zero encountered.") 0) ;; Log and return 0 for sum zero
      ;;                         (/ sum count))] ;; Calculate average normally
      ;;           average)))
       (map avg-chunks)))

(defn add-chunks [chunk]
  (let [cache-key (pr-str chunk)
        cache (get @agg-cache cache-key)]
    (if cache
      cache
      (let [sum (reduce + 0 chunk)]
        (swap! agg-cache assoc cache-key sum)
        sum))))

(defn sum-in-chunks [data chunk-size]
  (->> data
       (partition-all chunk-size) ;; vector into chunks
      ;;  (map (fn [chunk]
      ;;         (reduce + 0 chunk)))
       (map add-chunks)))


;; (def ansi-colors
;;   {:red "\u001b[31m"
;;    :green "\u001b[32m"
;;    :yellow "\u001b[33m"
;;    :blue "\u001b[34m"
;;    :magenta "\u001b[35m"
;;    :cyan "\u001b[36m"
;;    :white "\u001b[37m"
;;    :bright-red "\u001B[91m"
;;    :bright-green "\u001B[92m"
;;    :bright-yellow "\u001B[93m"
;;    :bright-blue "\u001B[94m"
;;    :bright-magenta "\u001B[95m"
;;    :bright-cyan "\u001B[96m"
;;    :bright-white "\u001B[97m"})

(def ansi-colors-ext
  {;:reset              "\u001b[0m"
   ;:bold               "\u001b[1m"
   ;:dim                "\u001b[2m"
   ;:italic             "\u001b[3m"
   ;:underline          "\u001b[4m"
   ;:blink              "\u001b[5m"
   ;:reverse            "\u001b[7m"
   ;:hidden             "\u001b[8m"
   ;:strikethrough      "\u001b[9m"

   ;; Regular colors
   ;:black              "\u001b[30m"
   :red                "\u001b[31m"
   :green              "\u001b[32m"
   :yellow             "\u001b[33m"
   :blue               "\u001b[34m"
   :magenta            "\u001b[35m"
   :cyan               "\u001b[36m"
   :white              "\u001b[37m"

   ;; Bright colors
   ;:bright-black       "\u001b[90m"
   :bright-red         "\u001b[91m"
   :bright-green       "\u001b[92m"
   :bright-yellow      "\u001b[93m"
   :bright-blue        "\u001b[94m"
   :bright-magenta     "\u001b[95m"
   :bright-cyan        "\u001b[96m"
   :bright-white       "\u001b[97m"

   ;; Background colors
   ;:bg-black           "\u001b[40m"
   ;:bg-red             "\u001b[41m"
   ;:bg-green           "\u001b[42m"
   ;:bg-yellow          "\u001b[43m"
   ;:bg-blue            "\u001b[44m"
   ;:bg-magenta         "\u001b[45m"
   ;:bg-cyan            "\u001b[46m"
   ;:bg-white           "\u001b[47m"

   ;; Bright background colors
   ;:bg-bright-black    "\u001b[100m"
   ;:bg-bright-red      "\u001b[101m"
   ;:bg-bright-green    "\u001b[102m"
   ;:bg-bright-yellow   "\u001b[103m"
   ;:bg-bright-blue     "\u001b[104m"
   ;:bg-bright-magenta  "\u001b[105m"
   ;:bg-bright-cyan     "\u001b[106m"
   ;:bg-bright-white    "\u001b[107m"
   })

(defn generate-256-color [n]
  {:pre [(and (integer? n) (<= 0 n 255))]}
  (str "\u001b[38;5;" n "m"))

;; (defn generate-256-bg-color [n]
;;   {:pre [(and (integer? n) (<= 0 n 255))]}
;;   (str "\u001b[48;5;" n "m"))

(defn generate-rgb-color [r g b]
  {:pre [(every? #(and (integer? %) (<= 0 % 255)) [r g b])]}
  (str "\u001b[38;2;" r ";" g ";" b "m"))

(defn bright-color? [n]
  (let [color-cube-start 16
        color-cube-end 231]
    (if (and (>= n color-cube-start) (<= n color-cube-end))
      (let [base (- n color-cube-start)
            modo 36
            red (* modo (mod (quot base modo) 6))
            green (* modo (mod (quot base 6) 6))
            blue (* modo (mod base 6))]
        (and (> red 89) (> green 89) (> blue 89)))  ;; was 128
      false)))

(defn generate-bright-256-colors []
  (let [bright-colors (filter bright-color? (range 256))]
    (into {}
          (for [b bright-colors]
            {(keyword (str "color" b))
             (str "\u001b[38;5;" b "m")}))))

;;  (ut/pp [:generate-bright-256-colors (count (generate-bright-256-colors))])

;; (defn generate-rgb-bg-color [r g b]
;;   {:pre [(every? #(and (integer? %) (<= 0 % 255)) [r g b])]}
;;   (str "\u001b[48;2;" r ";" g ";" b "m"))

(def ansi-colors ;; extended-colorss
  (merge ansi-colors-ext
         (generate-bright-256-colors)
         {:orange (generate-256-color 208)
          :pink (generate-256-color 13)
          :purple (generate-256-color 93)
          ;:bg-orange (generate-256-bg-color 208)
          ;:bg-pink (generate-256-bg-color 13)
          ;:bg-purple (generate-256-bg-color 93)
          :custom-red (generate-rgb-color 255 50 50)
          :custom-blue (generate-rgb-color 0 100 255)
          :custom-blue2 (generate-rgb-color 30 120 255)
          ;:bg-custom-blue (generate-rgb-bg-color 0 100 255)
          }))


(def flf1 (figlet/load-flf "data/ansi-regular.flf"))

;; (defn stacktrace-element->map [^StackTraceElement element]
;;   {:class-name (.getClassName element)
;;    :file-name (.getFileName element)
;;    :line-number (.getLineNumber element)
;;    :method-name (.getMethodName element)
;;    :native? (.isNativeMethod element)})

;; (defn stacktrace->map [^Throwable thrown-error]
;;   (let [cause (.getCause thrown-error)]
;;     {:type (class thrown-error)
;;      :message (.getMessage thrown-error)
;;      :stacktrace (mapv stacktrace-element->map (.getStackTrace thrown-error))
;;      :cause (when cause (stacktrace->map cause))}))

(defn fig-render
  [text & [color flf]]
  (let [;_ (println color (keyword? color))
        flf (if (string? flf) (figlet/load-flf flf) flf1)
         ;color-code (get ansi-colors color (:red ansi-colors))
        color-code (str (get ansi-colors color))
         ;_ (println (contains? ansi-colors color)  (get ansi-colors color "x"))
        reset-code "\u001b[0m"]
    (println
     (cstr/join
      \newline
      (conj (vec (cons color-code (vec (figlet/render flf (str text))))) reset-code)))))


  ;; (fig-render "DooM" :pink "data/Doom.flf")

(defn split-vector-at-x [v]
  (loop [input v
         current-segment []
         result []]
    (if (empty? input)
      (if (empty? current-segment)
        result
        (conj result current-segment))
      (let [first-elem (first input)]
        (if (= first-elem "X")
          (recur (rest input)
                 []
                 (if (empty? current-segment)
                   result
                   (conj result current-segment)))
          (recur (rest input)
                 (conj current-segment first-elem)
                 result))))))

(defn safe-percentage-change [old-val new-val]
  (if (< (Math/abs old-val) 1e-10)
    (if (< (Math/abs new-val) 1e-10)
      0  ; Both values are essentially zero
      100  ; Old value is zero, new value is not
      )
    (* (/ (- new-val old-val) old-val) 100)))

(defn draw-bar-graph [usage-vals label-str symbol-str & {:keys [color freq agg width] :or {color :default freq 1 agg "avg"}}]
  (try
    (let [console-width (- (if width width (- (ut/get-terminal-width) 10)) 5)
          rows 5
          border-width 2
          label-padding 4
          time-marker-interval 30
          ;values-per-marker (/ time-marker-interval 1) ; Assuming 1 value per second
          max-values (- console-width border-width label-padding 5)
          truncated (take-last max-values usage-vals)
          mmax (apply max truncated)
          mmin (apply min truncated)
          adjusted-min (* mmin 0.9) ; 90% to avoid zero bar
          value-range (- mmax adjusted-min)
          normalized (if (zero? value-range)
                       (repeat (count truncated) 0)
                       (map #(int (/ (* (- % adjusted-min) (* rows 8)) value-range)) truncated))
          bar-chars [" " "▁" "▂" "▃" "▄" "▅" "▆" "▇" "█"]
          color-code (get ansi-colors color "")
          reset-code "\u001B[0m"
          colorize (fn [s] (str color-code s reset-code))
          get-bar-char (fn [height row]
                         (let [row-height (- height (* row 8))]
                           (cond
                             (and (zero? height) (not= row 0)) " "
                             (and (zero? height) (zero? row)) "▁"
                             (<= height (* row 8)) " "
                             (> row-height 8) "█"
                             :else (nth bar-chars row-height))))
          border-line (apply str (repeat (- console-width 2) "─"))
          border-top (str "╭" border-line "╮")
          border-bottom (str "╰" border-line "╯")
          time-span (count truncated)
          seconds (* time-span freq)
          trend (try
                  (let [hchunk (Math/floor (/ (count truncated) 2))
                        first-half (take hchunk truncated)
                        second-half (drop hchunk truncated)
                        avg-sec (ut/avgf second-half)
                        avg-first (ut/avgf first-half)
                        raw-pct (* (/ (- avg-sec avg-first) avg-first) 100)
                        pct-chg (Math/abs (Math/ceil raw-pct))
                        direction (cond (> raw-pct 0) "up"
                                        (< raw-pct 0) "down"
                                        :else "stable")
                        magnitude (cond
                                    (>= pct-chg 50) "dramatically"
                                    (>= pct-chg 25) "significantly"
                                    (>= pct-chg 10) "noticeably"
                                    (>= pct-chg 5) "moderately"
                                    (>= pct-chg 1) "slightly"
                                    :else "marginally")]
                    (cond
                      (and (= direction "stable")
                           (= avg-first avg-sec))
                      "flat"
                      (and (= direction "stable")
                           (= (Math/floor avg-sec) (Math/floor avg-first)))
                      "mostly stable"
                      :else (str magnitude " " direction)))
                  (catch Throwable _ "not enough data"))
          label (str label-str
                     (let [pref (str " (last " (ut/format-duration-seconds seconds) " / " trend ")")
                           suff (str " max " (ut/nf (float (apply max truncated))) (when (not (= "%" symbol-str)) " ") symbol-str
                                     ", min " (ut/nf (float (apply min truncated))) (when (not (= "%" symbol-str)) " ") symbol-str
                                     ", avg " (ut/nf (float (ut/avgf truncated))) (when (not (= "%" symbol-str)) " ") symbol-str)
                           fwidth (count (str  label-str pref suff))
                           room? (< fwidth (- console-width 6))]
                       (str pref
                            (if room? (apply str (repeat (- (- console-width 6) fwidth) " ")) "")
                            suff)))
          max-label-length (- console-width 6)
          fitted-label (if (<= (count label) max-label-length)
                         label
                         (str (subs label 0 (- max-label-length 3)) "..."))
          padding (str (apply str (repeat (- console-width (count fitted-label) 4) " ")) " ")
          label-row (str "│ " (str "\u001B[1m" (colorize fitted-label) reset-code) padding "│")
          start-index (- (count usage-vals) (count truncated))

          chunked-segments (atom [])
          draw-row (fn [row-data row]
                     (let [graph-data (apply str
                                             (map-indexed
                                              (fn [idx ch]
                                                (if (zero? (rem (+ idx start-index) time-marker-interval))
                                                  (do (when (= row 0) (swap! chunked-segments conj "X"))
                                                      (str "." (colorize ch)))
                                                  (do (when (= row 0) (swap! chunked-segments conj (nth truncated idx)))
                                                      (colorize ch))))
                                              row-data))
                           padding (apply str (repeat (- console-width (count (cstr/replace graph-data #"\u001B\[[0-9;]*[mGK]" "")) 3) " "))]
                       (str "│ " graph-data padding "│")))
          agg-label (str ", " (if (= agg "avg") "averaged" "summed"))
          legend (str (when (> freq 1)
                        (str " freq: " freq))
                      " (each segment is " (ut/format-duration-seconds (* time-marker-interval freq)) " - each tick is " (ut/format-duration-seconds freq) (when (> freq 1) (str agg-label)) ")")
          legend (cstr/replace legend ", -" " -")
          legend (str legend (apply str (repeat (- console-width (count legend) 9) " ")) (.format (java.text.SimpleDateFormat. "HH:mm:ss") (java.util.Date.)))]
      (println border-top)
      (println label-row)
      (println (str "│" (apply str (repeat (- console-width 2) " ")) "│"))
      (doseq [row (range (dec rows) -1 -1)]
        (println (draw-row (map #(get-bar-char % row) normalized) row)))
         ;;(ut/pp [:start-index start-index (count truncated) (for [e (split-vector-at-x @chunked-segments)] (count e))])
      (defn safe-percentage-change [old-val new-val]
        (if (< (Math/abs old-val) 1e-10)
          (if (< (Math/abs new-val) 1e-10)
            0  ; Both values are essentially zero
            100  ; Old value is zero, new value is not
            )
          (* (/ (- new-val old-val) old-val) 100)))

      (println
       (let [chunks (vec (split-vector-at-x @chunked-segments))
             fchunks (vec (take-last (count chunks) (partition-all time-marker-interval usage-vals)))
             value-strings
             (apply str
                    (for [chunk-idx (range (count fchunks))
                          :let [chunk (vec (get chunks chunk-idx))
                                chunks-size (count chunk)
                                final-chunk? (= chunk-idx (dec (count chunks)))
                                last-chunk-full (if (> chunk-idx 0) (vec (get fchunks (- chunk-idx 1))) [0])
                                chunk-full (vec (get fchunks chunk-idx))]]
                      (let [avg-last (ut/avgf last-chunk-full)
                            avg-this (ut/avgf chunk-full)
                            vv (safe-percentage-change avg-last avg-this)
                            lb (if (= chunk-idx 0)
                                 ""
                                 (str (when (> vv 0) "+") (ut/nf (ut/rnd vv 0)) "%"))
                            lb (cstr/trim (str "(μ " (ut/nf avg-this) ") " lb))
                            lbc (count (str lb))]
                        (if (< lbc chunks-size)
                          (str lb (apply str (repeat (- (+ 1 chunks-size) lbc (if final-chunk? 2 0)) " ")))
                          (apply str (repeat (+ 1 chunks-size (if final-chunk? -2 0)) " "))))))
             vals-line (str "│" "\u001B[1m" (colorize value-strings) reset-code)
             padding (str (apply str (repeat (- console-width (count value-strings) 4) " ")) " ")]
         (str vals-line padding " │")))
      (println border-bottom)
      (println (str "\u001B[1m" (colorize legend) reset-code)))
    (catch Throwable e
      (ut/pp [:bar-graph-error! (str e)
              label-str (count usage-vals) :ex-vals-passed (vec (take 10 usage-vals))]))))



;; (ut/pp @db/conn-map)

;; (draw-client-stats nil [30] nil true 300 {:metrics-atom sql-metrics})
;; (draw-stats [:cpu] [1 15 60 90] true)
;; ⬉ ⬈ ⬊ ⬋⬅➡ ⬆ ⬇

;; (ut/pp (range (count [1 2 3])))

(defn pool-monitor
  [thread-pool pool-name]
  (let [active-threads (.getActiveCount thread-pool)
        max-pool-size (.getMaximumPoolSize thread-pool)
        current-pool-size (.getPoolSize thread-pool)
        max-pool-size (if (> max-pool-size 200000) current-pool-size max-pool-size) ;; fixed pools report billions
        percentage-full (if (> max-pool-size 0)
                          (* (/ current-pool-size (float max-pool-size)) 100)
                          0) ; Avoid division by zero if max-pool-size is 0
        percentage-full (if (= max-pool-size current-pool-size) 100.00 percentage-full)
        pct-str (str (format "%.2f" percentage-full) "%")
        timing-vec (get @ppy/pool-exec-times pool-name [])]
    [pct-str {:current-pool-size current-pool-size
              :max-pool-size max-pool-size
              :tasks-run (count timing-vec)
              :tasks-avg-ms (ut/avgf timing-vec)
              :active-threads active-threads
              :percent-float percentage-full
              :largest-pool-size (.getLargestPoolSize thread-pool)}]))

(defn pool-tasks-count [] (apply + (for [[_ v] @ppy/pool-exec-times] (count v))))

(defn pools []
  (vec (for [[k v] (merge {:websocket-thread-pool @ppy/websocket-thread-pool-atom ;; could move to organic?
                           :general-scheduler-thread-pool ppy/general-scheduler-thread-pool} ;; has to be hardcoded since its a .FixedScheudulePool
                          @ppy/dyn-pools)] [v k])))

(defn query-pool-sizes []
  (into {} (for [[pool pname] (pools)]
             {pname (pool-monitor pool pname)})))

;; (defn drop-last-from-chunks [chunks]
;;   (if (empty? chunks)
;;     chunks
;;     (let [last-chunk (peek chunks)]
;;       (if (= 1 (count last-chunk))
;;         (pop chunks)
;;         (assoc chunks (dec (count chunks)) (pop last-chunk))))))

;; (def tt3 [[1 2 3] [1 2 3] [1 2 3] [1 2]])
;; (ut/pp [:dl (drop-last-from-chunks tt3)])

(defn draw-pool-stats [& [kks freqs label? width]]
  (let [width (or width (ut/get-terminal-width))
        sample-size (* width 1.5)]
    (doseq [pp (cond (keyword? kks) [kks]
                     (vector? kks)   kks ;;(sort-by str (keys @pool-stats-atom))
                     (string? kks)  (vec (sort-by str (filter #(cstr/includes? (str " " (cstr/replace (str %) ":" "") " ") kks) (keys @pool-stats-atom))))
                     :else (vec (sort-by str (keys @pool-stats-atom))))
            :let [draw-it (fn [kkey sym ff color]
                            (ut/pp (draw-bar-graph
                                    (if (= ff 1)
                                      (mapv kkey (get @pool-stats-atom pp))
                                      (average-in-chunks (mapv kkey (get @pool-stats-atom pp)) ff))
                                    (str pp " - " kkey) sym :color color :freq ff :width width)))
                  freqs (if (nil? freqs)
                        ;[1 15]
                          [15 90]
                          freqs)
                  colors (vec (keys ansi-colors))
                  color-index (mod (hash pp) (count colors))
                  color (nth colors color-index)]]

    ;(ut/pp [:pool pp])

    ;(ut/pp [:current-pool-size])

      (doseq [ff freqs] (draw-it :current-pool-size "threads" ff color))
    ;(ut/pp [:active-threads])
      (doseq [ff freqs] (draw-it :active-threads "threads" ff color))
      (when label? (fig-render (str pp) color))
    ;(ut/pp [:percent-float])
    ;(doseq [ff freqs] (draw-it :percent-float "%" ff color))
      ))
    ;;(ut/pp [:total-pools (try (count kks) (catch Exception _ -1))])
  )

(defn get-namespace-atoms [ns-sym]
  (let [ns-vars (ns-interns ns-sym)]
    (into {}
          (for [[sym var] ns-vars
                :let [str-name (str ns-sym "/" sym)
                      str-name (-> str-name (cstr/replace "rvbbit-backend." "") (cstr/replace "websockets" "wss") (cstr/replace "evaluator" "evl"))]
                :when (instance? clojure.lang.Atom @var)]
            [str-name var]))))

(def monitored-atoms
  (vec
   (reduce into {}
           (map get-namespace-atoms
                [;;'rvbbit-backend.cruiser
                 'rvbbit-backend.sql
                 'rvbbit-backend.config
                 'rvbbit-backend.util
                 'rvbbit-backend.core
                 'rvbbit-backend.evaluator
                 'rvbbit-backend.endpoints
                 'rvbbit-backend.assistants
                 'rvbbit-backend.pool-party
                 'rvbbit-backend.queue-party
                 'rvbbit-backend.shape-rotator
                 'rvbbit-backend.leaves
                 'rvbbit-backend.websockets
                 'rvbbit-backend.db
                 'websocket-layer.network
                 'websocket-layer.core
                 'flowmaps.db
                 'rvbbit-backend.embeddings
                 'flowmaps.core]))))

(defn get-sys-atom-sizes []
  (doseq [[nn rr] monitored-atoms]
    (try
      (swap! atom-metrics update nn
             (fnil conj [])
             {:size-mb (ut/calculate-atom-size-special nn rr)})
      (catch Exception e (ut/pp [:error-in-get-sys-atom-sizes-loop nn e])))))

;; (ut/pp monitored-atoms)

;; (reset! pool-stats-atom {})

;; (ut/pp (sort-by last (for [[k v] @atom-metrics] [k (get (last v) :size-mb)])))

;; (ut/pp @db/shape-rotation-status)

;; (reset! atom-metrics {})

;; (mapv first (take 10 (sort-by second > (for [[k v] (deref atom-metrics)] [k (get (last v) :size-mb)]))))

;; (ns rvbbit-backend.websockets)
;; rvbbit block slice
;; (let [top (mapv first
;;                 (take
;;                  10
;;                  (sort-by
;;                   second
;;                   >
;;                   (for [[k v] (deref
;;                                atom-metrics)]
;;                     [k
;;                      (get (last v) :size-mb)]))))]
;;   (fig-render "< rvbbit sys atoms sizes >")
;;   (println
;;    "(only if enabled in config.edn, very expensive - only for debugging)")
;;   (draw-client-stats top
;;                      [30]
;;                      nil
;;                      true
;;                      (+ 30 :col-width)
;;                      {:metrics-atom
;;                       atom-metrics}))

;; (draw-client-stats nil [30] nil true 165 {:metrics-atom atom-metrics})
;; (draw-client-stats "leaf" [30] nil false 165 {:metrics-atom atom-metrics})

;; (get-sys-atom-sizes)

;; (ut/pp @atom-metrics)


(defn draw-client-stats [& [kks freqs stats label? width {:keys [metrics-atom force-color] :or {metrics-atom client-metrics}}]]
  (try
    (let [width (or width (ut/get-terminal-width))
          sample-size (* width 1.5)]
      (doseq [pp (cond (keyword? kks) [kks]
                       (vector? kks)   kks ;;(sort-by str (keys @metrics-atom))
                       (string? kks)  (vec (sort-by str (filter #(cstr/includes? (str " " (cstr/replace (str %) ":" "") " ") kks) (keys @metrics-atom))))
                       :else (vec (sort-by str (keys @metrics-atom))))
              :let [draw-it (fn [kkey sym ff color]
                              (ut/pp (let [data0 (mapv kkey (get @metrics-atom pp))
                                           data0 (mapv (fn [x] (if (nil? x) 0 x)) data0) ;; replace nil
                                           data0 (if (= metrics-atom atom-metrics)
                                                   (vec (mapcat #(repeat 15 %) data0))
                                                   data0)
                                           data (vec (mapcat (fn [item] (repeat heartbeat-seconds item)) data0))]
                                       (draw-bar-graph
                                        (if (= ff 1) data (average-in-chunks data ff))
                                        (str pp " - " kkey) sym :color color :freq ff :width width))))
                    stats (cond (vector? stats) stats
                                (keyword? stats) [stats]
                                (nil? stats) (keys (first (get @metrics-atom (first (keys @metrics-atom)))))
                                :else [:mem-bm])
                    freqs (if (nil? freqs)
                        ;[1 15]
                            [15 90]
                            freqs)
                    colors (vec (keys ansi-colors))
                    color-index (mod (hash pp) (count colors))
                    color (or force-color(nth colors color-index))]]
        (doseq [s stats]
          (doseq [ff freqs]
            (draw-it s "val" ff color)))

        (when label? (fig-render (str pp) color))))
    (catch Exception e (ut/pp [:draw-client-stats-error e]))))

(defn get-table-sizes []
  (let [dbs [system-db systemh2-db cache-db]
        cnts-all (into {} (for [db dbs
                                :let [dbname (-> (last (cstr/split (str (:datasource db)) #" ")) (cstr/replace "(" "") (cstr/replace ")" "") keyword)]]
                            {dbname
                             (let [tables (sql-query db "SELECT name FROM sqlite_master WHERE type='table'")
                                   table-names (vec (for [t tables] (get t :name)))
                                   table-cnts (into {} (for [t table-names]
                                                         {t (get-in (sql-query db (str "SELECT count(*) as c FROM " t)) [0 :c])}))]
                               table-cnts)}))]
    cnts-all))

;; (ut/pp (get-table-sizes))

(defn database-sizes []
  (into {}  (for [[dbname dbtables] (get-table-sizes)]
              {dbname {:tables (count (keys dbtables))
                       :avg-exec (ut/avgf (map last (filter #(= (first %) dbname) @sql/sql-query-log)))
                       :rows (apply + (vals dbtables))}})))





;;(ut/pp (database-sizes))

;; (ut/pp (sql-exec system-db "delete from client_memory where 1=1;"))
;; (ut/pp (sql-exec system-db "delete from jvm_stats where 1=1;"))
;; (ut/pp (sql-exec system-db "CREATE INDEX idx_client_name ON client_memory(client_name);"))
;; (ut/pp (sql-exec system-db "CREATE INDEX idx_ts ON client_memory(ts);"))
;; (ut/pp (sql-exec system-db "CREATE INDEX idx_client_name2 ON client_memory(client_name, ts);"))
;; (ut/pp (sql-exec system-db "CREATE INDEX idx_ts1 ON jvm_stats(ts);"))

;;(ut/pp [:test])

;; (draw-stats [:cpu :mem :threads :clients :flows :solvers :nrepl-calls :websockets :load] [15] false 200 true)

;; (ut/pp @sql-metrics)

;; (ut/pp @client-metrics)
;; (draw-client-stats nil [30 90 240] [:mem-mb :latency :messages-per-second] true 200)
;; (draw-client-stats nil [30] [:mem-mb :latency :recent-messages-per-second] true 300)
;; (draw-client-stats nil [10 ] nil true 260 {:metrics-atom sql-metrics})
;; (draw-pool-stats)
;; (draw-stats :cpu)

;; (defn draw-cpu-stats []
;;   (ut/pp (draw-bar-graph @cpu-usage "cpu usage" "%" :color :cyan))
;;   (ut/pp (draw-bar-graph (average-in-chunks @cpu-usage 15) "cpu usage" "%" :color :cyan :freq 15))
;;   (ut/pp (draw-bar-graph (average-in-chunks @cpu-usage 60) "cpu usage" "%" :color :cyan :freq 60))
;;   (ut/pp (draw-bar-graph (average-in-chunks @cpu-usage 600) "cpu usage" "%" :color :cyan :freq 600)))

;; (defn draw-msg-stats []
;;   (ut/pp (draw-bar-graph (ut/cumulative-to-delta @push-usage) "msgs/sec" "client pushes" :color :magenta))
;;   (ut/pp (draw-bar-graph (average-in-chunks (ut/cumulative-to-delta @push-usage) 15) "msgs/sec" "client pushes" :color :magenta :freq 15))
;;   (ut/pp (draw-bar-graph (average-in-chunks (ut/cumulative-to-delta @push-usage) 60) "msgs/sec" "client pushes" :color :magenta :freq 60))
;;   (ut/pp (draw-bar-graph (average-in-chunks (ut/cumulative-to-delta @push-usage) 600) "msgs/sec" "client pushes" :color :magenta :freq 600)))

;; (defn draw-mem-stats []
;;   (ut/pp (draw-bar-graph @mem-usage "memory usage" "mb" :color :yellow))
;;   (ut/pp (draw-bar-graph (average-in-chunks @mem-usage 15) "memory usage" "mb" :color :yellow :freq 15))
;;   (ut/pp (draw-bar-graph (average-in-chunks @mem-usage 60) "memory usage" "mb" :color :yellow :freq 60))
;;   (ut/pp (draw-bar-graph (average-in-chunks @mem-usage 600) "memory usage" "mb" :color :yellow :freq 600)))

;; (defn draw-stats [kks & [freqs]]
;;   (let [data-base (case kks
;;                     :cpu [@cpu-usage "cpu usage" "%"]
;;                     :mem [@mem-usage "msgs/sec" "client pushes"]
;;                     :msgs [@push-usage "memory usage" "mb"]
;;                     :queues [(get @qp/queue-stats-history :total-queues) "queues" "queues"]
;;                     :workers [(get @qp/queue-stats-history :total-workers) "workers" "workers"]
;;                     :tasks [(get @qp/queue-stats-history :total-tasks) "tasks" "tasks"])
;;         draw-it (fn [ff color data]
;;                   (let [[data-vec kkey sym] data]
;;                     (ut/pp (draw-bar-graph
;;                             (if (= ff 1)
;;                               data-vec
;;                               (average-in-chunks data-vec ff))
;;                             (str kkey " : " kks) sym :color color :freq ff))))
;;         freqs (if (nil? freqs) [1 15 60] freqs)
;;         colors [:red :green :yellow
;;                 :blue :magenta :cyan
;;                 :white :bright-red :bright-green
;;                 :bright-yellow :bright-blue :bright-magenta
;;                 :bright-cyan :bright-white]
;;         color-index (mod (hash kks) (count colors))
;;         color (nth colors color-index)]
;;     (doseq [ff freqs] (draw-it ff color data-base))))

(defn stats-keywords [] ;; add a "+" to the end of the kw for sum instead of avg..
  (let [base-avg {:cpu [@db/cpu-usage "cpu usage" "%"]
                  :mem [@db/mem-usage "heap memory usage" "mb"]
                  :non-heap-mem [@non-heap-mem-usage "non-heap memory usage" "mb"]
                  :msgs-cum [@push-usage "messages/sec" "client 'pushes'"]
                  :load [@sys-load "system load" "load"]
                  :websockets [@peer-usage "websocket connections" "peers"]
                  ;;:clients TODO get actual connected client counts
                  :threads [@thread-usage "total threads" "threads"]
                  :watchers [@watcher-usage "total watcher" "watchers"]
                  :subs [@sub-usage "unique subs" "subs"]
                  :subs-client [@sub-client-usage "client subs" "subs"]
                  :clover-params [@clover-params-usage "clover params" "params"]
                  :flows [(ut/cumulative-to-delta @flow-usage) "flows run" "flows"]
                  :nrepl-calls [(ut/cumulative-to-delta @nrepl-usage) "nrepl calls run" "evals"]
                  :nrepl-intro-calls [(ut/cumulative-to-delta @nrepl-intros-usage) "nrepl introspection calls run" "intro evals"]
                  :sql-queries [(ut/cumulative-to-delta @sql-query-usage) "sql queries run" "sql reads"]
                  :sql-exec [(ut/cumulative-to-delta @sql-exec-usage) "sql execs run" "sql writes"]
                  :queue-tasks [(ut/cumulative-to-delta @queue-tasks) "queue tasks run" "tasks"]
                  :solvers [(ut/cumulative-to-delta @solver-usage) "solvers running" "solvers"]
                  :pool-tasks-run [(ut/cumulative-to-delta @pool-task-usage) "pool tasks run" "tasks run"]
                  :msgs [(ut/cumulative-to-delta @push-usage) "msgs/sec" "client pushes"]
                  :viz-recos [(ut/cumulative-to-delta @viz-reco-usage) "viz recos run" "viz recos"]
                  :leaf-evals [(ut/cumulative-to-delta @leaf-eval-usage) "leaf evals run" "leaf evals"]
                  :reactions [(ut/cumulative-to-delta @reaction-usage) "reactions/sec" "reactions"]
                  :signal-reactions [(ut/cumulative-to-delta @signal-reaction-usage) "signal reactions/sec" "signal reactions"]
                  :pool-tasks [(ut/cumulative-to-delta @pool-tasks) "pool tasks" "tasks run"]
                  ;;:websockets [(get-in @pool-stats-atom ["websocket-thread-pool" :current-pool-size]) "threads" "threads"]
                  :queues [(get @qp/queue-stats-history :total-queues) "queues" "queues"]
                  :queued [(get @qp/queue-stats-history :total-queued) "queued" "tasks queued"]
                  :workers [(get @qp/queue-stats-history :total-workers) "workers" "workers"]
                  :tasks [(get @qp/queue-stats-history :total-tasks) "tasks" "tasks"]}
        summed (into {} (for [[k v] base-avg
                              :let [k (keyword (str (cstr/replace (str k) ":" "") "+"))]]
                          {k (conj v true)}))]
    (merge base-avg summed)))

;; (ut/pp (filterv #(not (cstr/ends-with? (str %) "+")) (keys ( stats-keywords))))

;; (defn get-stats [kkey agg num]
;;   (let [stat (get (stats-keywords) kkey)]
;;     (let [[data-vec kkey sym] stat]
;;       (if agg
;;         (last (sum-in-chunks data-vec num))
;;         (last (average-in-chunks data-vec num))))))


(defn draw-stats
  ([]
   (ut/pp [:draw-stats-needs-help
           (str "Hi. Draw what? " (clojure.string/join ", " (map str (keys (stats-keywords)))))]))
  ([kks & [freqs label? width limit?]]
   (let [width (or width (ut/get-terminal-width))
         ;;sample-size (* width 1.5)
         kksv (cond (or (= kks :all) (= kks :*)) (vec (sort-by str (keys (stats-keywords))))
                    (keyword? kks) [kks]
                    (vector? kks)   kks ;;(sort-by str (keys @pool-stats-atom))
                    (string? kks)  (vec (sort-by str (filter #(cstr/includes? (str " " (cstr/replace (str %) ":" "") " ") kks) (keys (stats-keywords)))))
                    :else (vec (sort-by str (keys (stats-keywords)))))]
     (doseq [kks kksv]
       (let [data-base (get (stats-keywords) kks)
         ;;_ (ut/pp data-base)
             draw-label (fn [data color]
                          (let [[dv kkey _ _] data]
                            (fig-render (str kkey
                                             (let [vl (str (ut/nf (ut/rnd (ut/avg (take-last 10 dv)) 0)))
                                                   vll (count vl)
                                                   labl (count (str kkey))
                                                   ;multi 6
                                                   spc (* (+ vll labl) 9)
                                                   ;_ (ut/pp [vl vll labl spc width])
                                                   ]
                                               ;(str (apply str (repeat (Math/ceil (/ (- width spc) 3)) " ")) vl)
                                               (str " " vl))) color
                                        ;;"data/Cybermedium.flf" ;;(figlet/load-flf "data/Cybermedium.flf")
                                        )))
             draw-it (fn [ff color data]
                       (let [[data-vec kkey sym sum?] data
                             data-vec (if limit? (take-last
                                                  ;(* (* ff width) 1.2)
                                                  (* ff width)
                                                  data-vec) data-vec)]
                         (draw-bar-graph
                          (if (= ff 1)
                            data-vec
                            (let [dd (if (and (true? sum?) (not (nil? sum?)))
                                       (sum-in-chunks data-vec ff)
                                       (average-in-chunks data-vec ff))
                                 ;;; _ (ut/pp [:dd dd])
                                  ;;dd (if (>= ff 15) (vec (drop-last dd)) dd)
                                  ;;dd (pop dd)
                                  ]dd))
                          ;(str kkey " : " kks)
                          (str kkey)
                          sym :color color :freq ff :agg (if sum? "sum" "avg") :width width)))
             freqs (cond (nil? freqs) [1 15 60]
                         (number? freqs) [freqs]
                         :else freqs)
             colors (vec (keys ansi-colors))
             color-index (mod (hash kks) (count colors))
             color (nth colors color-index)]
         (if data-base
           (do (doseq [ff freqs] (draw-it ff color data-base))
               (when label? (draw-label data-base color)))
           (ut/pp [:draw-stats-needs-help
                   (str kks "? Nope. Invalid data type, bro! Gimmie something: " (clojure.string/join ", " (map str (keys (stats-keywords)))))])))))))

;; (draw-stats :cpu)
;; (draw-stats)
;; (draw-stats [:workers] [15] false nil true)
;; (draw-stats [:reactions+ :signal-reactions] [15] false nil true)

;; (time (draw-stats [:cpu ] [15] false nil true))
 ;;  (time (draw-stats [:cpu ] [15] false nil false))

(defonce pool-ttls-last (atom {}))

(defn show-pool-sizes-report [ & [opts-map]]
  (ut/pp [:pool-sizes
          (let [pool-sizes (query-pool-sizes)
                pairs (vec (sort-by (comp str first) (for [[k v] pool-sizes
                                                           :let [runs (get-in v [1 :tasks-run] 0)
                                                                 avgs (get-in v [1 :tasks-avg-ms] 0)]
                                                           ;:when (> (get-in v [1 :current-pool-size]) 0)
                                                           ]
                                                       [k (get-in v [1 :current-pool-size])
                                                        {:runs runs
                                                         :ttl-secs (try (ut/rnd (/ (* runs avgs) 1000) 2) (catch Exception _  -1))
                                                         :avg avgs}])))
                ttls {:pools (count pairs) :threads (apply + (map second pairs))}
                prev @pool-ttls-last]
            (reset! pool-ttls-last ttls)
            (into (sorted-map)
                  {:pool-counts pairs
                   :pool-groups-counts (sort-by val (reduce (fn [acc [k _ {:keys [runs]}]]
                                                              (let [group-key (first (clojure.string/split (str k) #"\."))]
                                                                (update acc group-key (fn [existing-runs]
                                                                                        (if existing-runs
                                                                                          (+ existing-runs runs)
                                                                                          runs)))))
                                                            {}
                                                            pairs))
                   :zdiff-pools (format "%+d"  (- (get prev :pools 0) (get ttls :pools 0)))
                   :zdiff-threads (format "%+d"  (- (get prev :threads 0) (get ttls :threads 0)))
                   :prev prev
                   :now ttls}))] opts-map))

;; (show-pool-sizes-report {:width 10})

(defn show-reactor-sizes-report [& [opts-map]]
  (ut/pp (vec (filter #(cstr/starts-with? (str %) ":time/") (distinct (apply concat (for [[_ v] @db/splitter-stats] (keys v)))))) opts-map)

  (ut/pp [:reactor (into {} (for [[k v] @db/splitter-stats]
                              {k (into {} (for [[kk vv] v] {kk (get vv :cnt)}))}))] opts-map)
  (ut/pp [:reactor {:counts-by-type (into {}  (for [[k v] @db/splitter-stats] {k (count (keys v))}))}] opts-map)
  (ut/pp [:reactor (let [react-counts (apply concat (for [[_ v] @db/splitter-stats] (vals v)))
                         updates (apply + (map :cnt react-counts))
                         uptime  (db/reactor-uptime-seconds) ;; (ut/uptime-seconds)
                         per-sec (ut/rnd (/ updates uptime) 2)]
                     [:key-watchers-w-reactions (count react-counts)
                      :watchers (last @watcher-usage)
                      :updates updates
                      :uptime-secs uptime
                      :uptime (ut/format-duration-seconds  uptime)
                      :per-sec per-sec
                      :max-key-depth @db/key-depth-limit
                      :clients (count @wl/sockets)
                      :avg-cpu (ut/avgf @db/cpu-usage)])]  opts-map))

;; (show-reactor-sizes-report)

;;  (show-pool-sizes-report)
;;  (show-reactor-sizes-report)

;; (ppy/reboot-websocket-thread-pool!)

;; (ut/pp @wl/sockets)

;;;(mapv (fn [x] (cstr/replace (str x) ":" "")) (keys (rvbbit-backend.websockets/stats-keywords)))
;;;(mapv #(cstr/replace (str %) ":" "") (keys (stats-keywords)))

(defn print-reactor-stats []
  (ut/pp [:reactor (into {} (for [[k v] @db/splitter-stats]
                              {k (into {} (for [[kk vv] v] {kk (get vv :cnt)}))}))])
  (ut/pp [:reactor {:counts-by-type (into {}  (for [[k v] @db/splitter-stats] {k (count (keys v))}))}])
  (ut/pp [:reactor (let [react-counts (apply concat (for [[_ v] @db/splitter-stats] (vals v)))
                         updates (apply + (map :cnt react-counts))
                         uptime  (db/reactor-uptime-seconds) ;; (ut/uptime-seconds)
                         per-sec (ut/rnd (/ updates uptime) 2)]
                     [:key-watchers-w-reactions (count react-counts)
                      :watchers (last @watcher-usage)
                      :updates updates
                      :uptime-secs uptime
                      :uptime (ut/format-duration-seconds  uptime)
                      :per-sec per-sec
                      :max-key-depth @db/key-depth-limit
                      :sockets (count @wl/sockets)
                      :avg-cpu (ut/avgf @db/cpu-usage)])]))

(defn normalize [data min-val max-val]
  (let [range (- max-val min-val)]
    (map #(-> % (- min-val) (/ range)) data)))

(defn sparkline [data]
  (let [blocks ["▁" "▂" "▃" "▄" "▅" "▆" "▇" "█"]
        min-val (apply min data)
        max-val (apply max data)
        normalized (normalize data min-val max-val)
        scaled (map #(int (* % (dec (count blocks)))) normalized)]
    (apply str (map #(nth blocks %) scaled))))

;; (ut/pp (rand-nth (vals ansi-colors)))
;; (ut/pp (count (filterv #(not (= (first %) :rvbbit)) @db/ack-scoreboard)))

(defn print-client-stats-vector []
  (let [cwidth (ut/get-terminal-width)
        colors (vec (vals ansi-colors))
        clients-vec (filterv #(not (= (first %) :rvbbit)) @db/ack-scoreboard)
        client-maps (sort-by (fn [[k _]] (count (get @client-metrics k))) clients-vec)
        reset-code "\u001b[0m"]
    (println)
    (ut/pp [:active-clients (count (keys client-maps)) :max-spark-line-width (str (ut/format-duration-seconds-compact (* 15 cwidth)))])
    (println "------------------------------------------------------------------------------------------------------------------------")
    (mapv
     (fn [[cid x]]
       (let [color-index (mod (hash cid) (count colors))
             color (nth colors color-index)
             cc (fn [s] (str color (str s) reset-code))
             mem-vec (mapv :mem-mb (get @client-metrics cid))
             spark-vec (take-last cwidth mem-vec)]
         (ut/pp [(str color "web-client-" (.indexOf clients-vec [cid x]) reset-code)
                 (cc (cstr/replace
                      (str {:acks (:ack x) :seen (:last-seen-seconds-ago x)
                  :mem [(ut/nf (:memory x)) (str "μ " (ut/nf (ut/avg mem-vec)))] :subs (:client-subs x) :latency (get @dynamic-timeouts cid) :screen (:screen-name x)
                  :up (:uptime x) :mps (:messages-per-second x)}) "\"" "'")) cid])
         (try (if (ut/ne? mem-vec)
                (println (str color (sparkline spark-vec) reset-code))
                (println (str color "(no data)" reset-code)))
              (catch Exception _ (println (str color "(data error*)" reset-code))))))
     client-maps)
    (println "------------------------------------------------------------------------------------------------------------------------")
    (println)))

;; (print-client-stats-vector)

;; (def data [1 2 3 4 5 6 7 8 9 10 3 4 5 6 7 8 3 4 5 6 7 8 3 4 5 6 7 8 3 4 5 6 7 8])
;; (ut/pp (sparkline data))

;; (ut/pp (sparkline (take-last 120 (mapv :mem-mb (get @client-metrics :super-elliptic-snake-32)))))

(defn jvm-stats []
  (when (not @shutting-down?)
    (try
      (let [runtime (java.lang.Runtime/getRuntime)
            total-memory (.totalMemory runtime)
            free-memory (.freeMemory runtime)
            used-memory (/ (- total-memory free-memory) (* 1024 1024))
            mm (int (Math/floor used-memory))
            sys-load (ut/avgf (take-last 15 @db/cpu-usage)) ;;(ut/get-jvm-cpu-usage)  ;; (ut/get-system-load-average)
            thread-mx-bean (java.lang.management.ManagementFactory/getThreadMXBean)
            thread-count (.getThreadCount thread-mx-bean)
            booted? (= @stats-cnt 0)
            ttl (try (apply + (for [[_ v] @db/atoms-and-watchers] (count (keys v)))) (catch Exception _ -1))
            ;;_ (swap! stats-shadow conj {:mem mm :tick (count @stats-shadow) :threads thread-count :subs ttl :load sys-load})

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
                                    {k (ut/deselect-keys v [:booted :last-ack :last-push :last-seen-seconds-ago])}))
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
                          uptime-str                 (ut/format-duration-seconds-compact uptime-seconds)
                          _ (swap! db/ack-scoreboard assoc-in [k :uptime] uptime-str) ;; bad
                          _ (swap! db/ack-scoreboard assoc-in [k :messages-per-second] msg-per-second)
                          _ (swap! db/ack-scoreboard assoc-in [k :recent-messages-per-second] recent-messages-per-second)
                          queue-distro               (get v :queue-distro)]]
               (merge
                {:client-name (str k) :uptime-seconds uptime-seconds :messages-per-second msg-per-second :uptime uptime-str}
                (assoc v :queue-distro (pr-str queue-distro)))))
            ;; _ (ut/pp [:cli-rows cli-rows])
            _ (doseq [cli-row cli-rows]
                (swap! db/params-atom assoc-in [(edn/read-string (get cli-row :client-name)) :stats] cli-row))
            insert-cli {:insert-into [:client_stats] :values cli-rows}
            all-batches (apply + (vals @client-batches))
            seconds-since-last (try (/ (- (System/currentTimeMillis) (get @last-stats-row :unix_ms)) 1000) (catch Exception _ -1))
            seconds-since-boot (try (/ (- (System/currentTimeMillis) @booted) 1000) (catch Exception _ -1))
            last-messages (- @all-pushes (get @last-stats-row :messages 0))
            last-batches  (- all-batches (get @last-stats-row :batches 0))
            queries-since-last (- (+ @q-calls @q-calls2)
                                  (+ (get @last-stats-row :queries_run 0) (get @last-stats-row :internal_queries_run 0)))
            as-double (fn [x] (Double/parseDouble (clojure.pprint/cl-format nil "~,2f" x)))
            jvm-stats-vals
            {:used_memory_mb             mm
             :messages                   @all-pushes
             :batches                    all-batches
             :batches_per_second         (as-double (try (/ all-batches seconds-since-boot) (catch Exception _ -1)))
             :recent_batches_per_second  (as-double (try (/ last-batches seconds-since-last) (catch Exception _ -1)))
             :recent_batches             (- all-batches last-batches)
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
             :open_flow_channels         -1 ;; (apply + (for [[_ v] (flow-statuses)] (get v :channels-open)))
             :queries_run                @q-calls
             :internal_queries_run       @q-calls2
             ;:sniffs_run                 @cruiser/sniffs
             :sys_load                   (as-double sys-load) ;;sys-load to 2 decimal places
             }
            _ (reset! last-stats-row jvm-stats-vals)
            insert-sql {:insert-into [:jvm_stats] :values [jvm-stats-vals]}
            _ (swap! db/server-atom assoc :uptime (ut/format-duration-seconds seconds-since-boot))
            ;flow-status-map (flow-statuses) ;; <--- important, has side effects, TODO refactor into timed instead of hitched to jvm console stats output
            ;pool-sizes (query-pool-sizes)
            ]

        ;; (ut/pp [:post-cli-rows (count cli-rows)])
        (swap! stats-cnt inc)
        (sql-exec systemh2-db (to-sql {:delete-from [:client_stats]}))
        (sql-exec systemh2-db (to-sql insert-cli))
        (sql-exec systemh2-db (to-sql insert-sql))

        (when booted?
          (println " ")
          (reset! booted (System/currentTimeMillis))
          (println " "))

        ;;(let [fss (flow-statuses) fssk (vec (keys fss))] (ut/pp [:flow-status (select-keys fss fssk)]))

        ;;(ut/pp [:date-map @time-atom])

        ;; (ut/pp [:sql-errors!
        ;;         {:ttl     (count @sql/errors)
        ;;          :freq    (frequencies (mapv first @sql/errors))
        ;;          :freq-db (frequencies (mapv second @sql/errors))}])

        ;;(ut/pp [:solvers-running? @solver-status])

        ;; (ut/pp [:solver-cache (ut/calculate-atom-size :solver-cache solvers-cache-atom)])

          ;(ut/pp [:solver-runner-pool-stats (get-slot-pool-queue-sizes)])

        (try (let [peers       (count @wl/sockets)
                   uptime-str  (ut/format-duration-seconds (ut/uptime-seconds))
                   watchers    (last @watcher-usage)
                   sub-types (try
                               (frequencies (flatten (apply concat (for [[_ v] @db/atoms-and-watchers]
                                                                     (for [k (keys v)]
                                                                       (edn/read-string (first (cstr/split (str k) #"/"))))))))
                               (catch Exception e {:error-getting-sub-types (str e)}))
                   sub-types (select-keys sub-types (filter #(not (cstr/includes? (str %) "||")) (keys sub-types)))
                   ;; ^^ flow tracker subs have messy keypath parents and are generally one-offs. noise.
                   server-subs ttl
                   col-width 128]

               (swap! db/server-atom merge
                      {:cpu-chart      (ut/capture-output #(draw-stats [:cpu] [15 90 120 600 1800 3600] false col-width true))
                       :mem-chart      (ut/capture-output #(draw-stats [:mem] [15 90 120 600 1800 3600] false col-width true))
                       :threads-chart  (ut/capture-output #(draw-stats [:threads] [15 90 120 600 1800 3600] false col-width true))
                       :nrepl-chart    (ut/capture-output #(draw-stats [:nrepl-calls] [15 90 120 600 1800 3600] false col-width true))
                       :solvers-chart  (ut/capture-output #(draw-stats [:solvers] [15 90 120 600 1800 3600] false col-width true))
                       :flows-chart    (ut/capture-output #(draw-stats [:flows] [15 90 120 600 1800 3600] false col-width true))
                       :uptime         uptime-str
                       :clients        peers
                       :threads        thread-count
                       :memory         mm
                       :watchers       watchers
                       :subs           ttl})

              ;;  (swap! server-atom assoc :cpu-chart (ut/capture-output #(draw-stats [:cpu] [15 90 120 600 1800 3600] false 103 true)))
              ;;  (swap! server-atom assoc :mem-chart (ut/capture-output #(draw-stats [:mem] [15 90 120 600 1800 3600] false 103 true)))
              ;;  (swap! server-atom assoc :threads-chart (ut/capture-output #(draw-stats [:threads] [15 90 120 600 1800 3600] false 103 true)))
              ;;  (swap! server-atom assoc :nrepl-chart (ut/capture-output #(draw-stats [:nrepl-calls] [15 90 120 600 1800 3600] false 103 true)))
              ;;  (swap! server-atom assoc :solvers-chart (ut/capture-output #(draw-stats [:solvers] [15 90 120 600 1800 3600] false 103 true)))
              ;;  (swap! server-atom assoc :flows-chart (ut/capture-output #(draw-stats [:flows] [15 90 120 600 1800 3600] false 103 true)))

               ;(swap! server-atom assoc :uptime uptime-str :clients peers :threads thread-count :memory mm :watchers watchers :subs ttl)

              ;;  (ut/pp [(get @db/father-time :now-seconds)
              ;;          :jvm-stats
              ;;          {;:*cached-queries              (count @sql-cache)

              ;;           ;:clover-sql-training-queries  (count (keys @db/clover-sql-training-atom))
              ;;           ;:clover-sql-training-enriched (count (keys @db/clover-sql-enriched-training-atom))
              ;;           :schedulers-last-run          @scheduler-atom
              ;;           :ws-peers                     (do ;(reset! peer-usage (vec (take-last 600 @peer-usage)))
              ;;                                           peers)
              ;;           :sys-load                     sys-load
              ;;           :sys-load-avg                 (do ;(reset! cpu-usage (vec (take-last 600 @cpu-usage)))
              ;;                                           (ut/avgf @db/cpu-usage))
              ;;           :avg-memory                 (do ;(reset! mem-usage (vec (take-last 600 @mem-usage)))
              ;;                                         (ut/avgf @db/mem-usage)
              ;;                                           ;@mem-usage
              ;;                                         )
              ;;           :pushes-avg                   (do ;(reset! push-usage (vec (take-last 600 @push-usage)))
              ;;                                           (ut/avgf (ut/cumulative-to-delta @push-usage)))
              ;;           :cwidth                       (ut/get-terminal-width)
              ;;           :uptime                       uptime-str
              ;;           :sub-types                    sub-types
              ;;           :server-subs                  server-subs
              ;;           :watchers                     watchers
              ;;           :*jvm-memory-used             [(ut/nf mm) :mb]
              ;;           :*current-threads             thread-count}])

               (ut/pp ["☢️" (get @db/father-time :now-seconds)
                       :sys-load sys-load :server-subs server-subs
                       :watchers watchers :clients (count (keys (dissoc @db/ack-scoreboard :rvbbit)))])
               (ut/pp ["☢️" :reactor-sub-types sub-types]))
             (catch Throwable e (ut/pp [:printing-shit-error? (str e)])))



        ;;(ut/pp [:latency-adaptations @dynamic-timeouts])

                  ;; [kks & [freqs label? width limit?]]
        ;; (draw-stats [:cpu :mem ; :threads :websockets ;:viz-recos :leaf-evals
        ;;              ][15] false nil true)

        (let [[kks freqs label? width]
              (get @db/latest-settings-map :console-viz-settings [])]
          (draw-stats (or kks [:cpu :mem]) (or freqs [15]) (or label? false) width true))

        ;; ([kks & [freqs label? width limit?]]

        ;; (draw-stats [:cpu :mem :threads] [15] false nil false)

        (when (get @db/latest-settings-map :console-viz-client-sparklines? false)
          (print-client-stats-vector)) ;; bad ass sparklines !

        ;(ut/pp [:repl-introspections @evl/repl-introspection-atom])

        ;(ut/pp [:timekeeper-failovers? @timekeeper-failovers])

        ;;;(ut/pp [:pool-sizes pool-sizes])

        ;; (ut/pp [:pool-sizes
        ;;         (let [pool-sizes (query-pool-sizes)
        ;;               pairs (vec (sort-by (comp str first) (for [[k v] pool-sizes
        ;;                                                          :let [runs (get-in v [1 :tasks-run] 0)
        ;;                                                                avgs (get-in v [1 :tasks-avg-ms] 0)]
        ;;                                                          :when (> (get-in v [1 :current-pool-size]) 0)]
        ;;                                                      [k (get-in v [1 :current-pool-size])
        ;;                                                       {:runs runs
        ;;                                                        :ttl-secs (try (ut/rnd (/ (* runs avgs) 1000) 2) (catch Exception _  -1))
        ;;                                                        :avg avgs}])))
        ;;               ttls {:pools (count pairs) :threads (apply + (map second pairs))}
        ;;               prev @pool-ttls-last]
        ;;           (reset! pool-ttls-last ttls)
        ;;           (into (sorted-map)
        ;;                 {:pool-counts pairs
        ;;                  :pool-groups-counts (sort-by val (reduce (fn [acc [k _ {:keys [runs]}]]
        ;;                                                             (let [group-key (first (clojure.string/split (str k) #"\."))]
        ;;                                                               (update acc group-key (fn [existing-runs]
        ;;                                                                                       (if existing-runs
        ;;                                                                                         (+ existing-runs runs)
        ;;                                                                                         runs)))))
        ;;                                                           {}
        ;;                                                           pairs))
        ;;                  :zdiff-pools (format "%+d"  (- (get prev :pools 0) (get ttls :pools 0)))
        ;;                  :zdiff-threads (format "%+d"  (- (get prev :threads 0) (get ttls :threads 0)))
        ;;                  :prev prev
        ;;                  :now ttls}))])

        ;; (ut/pp [:client-cost (let [pool-sizes (query-pool-sizes)
        ;;                            clients (distinct
        ;;                                     (for [[k v] pool-sizes]
        ;;                                      (last (cstr/split (str k) #"\."))))]
        ;;                        (for [c clients]  ))])

        ;; (ut/pp (generate-report shard-atoms))

        ;; (ut/pp @watcher-log)

        ;; (ut/pp (vec (filter #(cstr/starts-with? (str %) ":time/") (distinct (apply concat (for [[_ v] @splitter-stats] (keys v)))))))



      ;;  (print-reactor-stats)



        ;;(get @atoms-and-watchers :okay-short-crow-1)

        ;; (ut/pp @db/atoms-and-watchers)

        ;; (let [ss (qp/get-queue-stats+)]
        ;;   (ut/pp [:queue-party-stats+ ss]))

        ;; (ut/pp [:freeze-pop? @(get-atom-splitter-deep :time/now father-time)
        ;;         (get @father-time :now)
        ;;         :rvbbit-sub-ms-diff (- (System/currentTimeMillis)  (get-in @rvbbit-client-sub-values [:unix-ms]))])

        ;; (ut/pp [:clients-with-tardy-subs? (count (db/client-subs-late-delivery 30000))])

        ;; (ut/pp (ut/pp @rvbbit-client-sub-values))

        ;; (ut/pp [:child-atoms (count-parent-keys)])


              ;(ut/pp (draw-bar-graph @peer-usage "clients" "peers" :color :green))

              ;;(draw-bar-graph @cpu-usage)

        ;; (when (or booted? (zero? (mod @stats-cnt 100)))
        ;;   (doseq [[client-name v] @atoms-and-watchers]
        ;;     (let [clients (count @wl/sockets)]
        ;;       (alert! client-name
        ;;               [:v-box :justify :center :style
        ;;                {;:margin-top "-6px"
        ;;                 :opacity 0.7} ;:color (if error? "red" "inherit")}
        ;;                :children
        ;;                [[:box :style {:font-weight 700 :font-size "18px"} :child (str "[sys-stats] " client-name)]
        ;;                 [:box :style {:font-weight 700 :font-size "11px" :opacity 0.6} :child
        ;;                  (str thread-count
        ;;                       " threads,  "
        ;;                       (ut/nf mm)
        ;;                       " MB used on server, "
        ;;                       (ut/nf ttl)
        ;;                       " active client subs, "
        ;;                       clients
        ;;                       " client"
        ;;                       (when (> clients 1) "s")
        ;;                       " connected")]
        ;;                 [:box :style {:font-weight 700 :font-size "11px" :opacity 0.6} :child
        ;;                  (str "uptime: " (ut/format-duration-seconds (ut/uptime-seconds)))]
        ;;                 [:box :style {:font-weight 700 :font-size "11px" :opacity 0.6} :child
        ;;                  (str "you have " (count (keys v)) " (server) watcher subs")]
        ;;                 (when booted?
        ;;                   [:box :style {:color :theme/editor-outer-rim-color :font-weight 700} :child
        ;;                    [:speak-always (str "Hello. data rabbit system is now online.")]])]]
        ;;               10
        ;;               (if booted? 2.2 1.7)
        ;;               6)
        ;;       (alert! client-name (chart-view (last-x-items (average-chunks @stats-shadow) 10)) 10 4 5)
        ;;       (alert! client-name (chart-view (last-x-items @stats-shadow 50)) 10 4 5))))
        )
      (catch Exception e (ut/pp [:jvm-stats (str e)]))
      ;;(catch clojure.lang.ExceptionInfo e (ut/pp [:error-in-jvm-stats-fn (.getData e)]))

      )))





;; (defn execute-websocket-task [f]
;;   (.execute @ppy/websocket-thread-pool-atom f))

;; (defn wrap-websocket-handler [handler]
;;   (fn [request]
;;     (let [response-promise (promise)]
;;       (execute-websocket-task
;;        (fn []
;;          (try
;;            (let [response (handler request)]
;;              (deliver response-promise response))
;;            (catch Exception _
;;              (deliver response-promise {:status 500
;;                                         :headers {"Content-Type" "text/plain"}
;;                                         :body "Internal Server Error"})))))
;;       @response-promise)))



(defn create-custom-thread-factory [prefix]
  (let [thread-number (atom 0)
        thread-group (Thread/currentThread)]
    (reify ThreadFactory
      (newThread [_ r]
        (let [thread-name (format "%s-%d" prefix (swap! thread-number inc))]
          (doto (Thread. thread-group r thread-name 0)
            (.setDaemon true)
            (.setPriority Thread/NORM_PRIORITY)))))))

(defn web-handler [request]
  {:status  200
   :headers {"Content-Type" "text/html"}
   :body    "<html><head></head><body>youre never going to see this, bro</body></html>"})

(def websocket-port 3030)

(def ws-endpoints {"/ws" (net/websocket-handler {:encoding :edn})})

;; (def ring-options ;; using stock jetty pool (JVM shared, assumably)
;;   {:port                 websocket-port
;;    :join?                false
;;    :async?               true
;;    ;:min-threads          10
;;    ;:max-threads          1500 ;; Increased max threads
;;    :tcp-no-delay true
;;    :max-sessions 1000
;;    :max-idle-time 300000
;;    :websocket-max-sessions 1000
;;    :websocket-max-idle-time 300000
;;    :queue-capacity 500
;;    :idle-timeout         300000
;;    :thread-idle-timeout  120000 ; 30 seconds
;;    ;:max-idle-time        15000
;;    :acceptors 6
;;    :selectors 6
;;    ;:max-idle-time        15000
;;    ;:input-buffer-size    131072 ;; default is 8192
;;    ;:output-buffer-size   131072 ;; default is 32768
;;    :thread-factory       (create-custom-thread-factory "ring-ws")
;;    ;:input-buffer-size    32768 ;;131072 ;;32768
;;    ;:output-buffer-size   131072
;;    ;:max-message-size     4291456
;;    :websockets           ws-endpoints
;;    :allow-null-path-info true})

(def ring-options
  {:port                 websocket-port
   :join?                false
   :async?               true
   :tcp-no-delay         true
   :min-threads 20 ;; 12 for acceptors and selectors + extra
   :max-threads 200
   :max-sessions         1000
   :max-idle-time        60000   ;; 1 minute idle timeout
   :websocket-max-sessions 1000
   :websocket-max-idle-time 60000 ;; 1 minute idle timeout
   :queue-capacity       500
   :idle-timeout         60000   ;; 1 minute idle timeout
   :thread-idle-timeout  120000  ;; 2 minutes thread idle timeout
   :acceptors            6 ;; thread count
   :selectors            6
   :input-buffer-size    32768 ;; 32KB
   :output-buffer-size   32768 ;; 32KB
   :max-message-size     4194304 ;; 4MB
   :thread-factory       (create-custom-thread-factory "ring-ws")
   :websockets           ws-endpoints
   :allow-null-path-info true})

(defonce websocket-server (atom nil))

;; (reset! websocket-server (jetty/run-jetty #'web-handler ring-options)) ;;; to start

(defn stop-websocket-server []
  (when-let [server @websocket-server]
    (.stop server)
    (reset! websocket-server nil)))

(defn destroy-websocket-server! []
  (ut/ppa [:shutting-down-websocket-server :port websocket-port])
  (try (do (when @websocket-server
             (.stop @websocket-server)
             @websocket-server
             (reset! websocket-server nil)) @websocket-server)
       (catch Throwable _ nil)))










(defn resolve-user-space-dir []
  (let [working-dir (System/getProperty "user.dir")
        user-space (io/file working-dir "assets")]
    (when (.exists user-space)
      (.getCanonicalPath user-space))))

(def user-space-dir (resolve-user-space-dir))

(defn serve-user-asset [request]
  (if user-space-dir
    (let [asset-path (subs (:uri request) (count "/assets/"))
          file (io/file user-space-dir asset-path)]
      (if (.exists file)
        (ring-resp/file-response (.getPath file))
        (ring-resp/not-found "Asset not found")))
    (ring-resp/not-found "User space directory not configured")))

(defn static-root [request] (ring-resp/content-type (ring-resp/resource-response "index.html" {:root "public"}) "text/html"))
(def common-interceptors [(body-params/body-params) http/html-body])

(def routes
  #{["/" :get (conj common-interceptors `static-root)]
    ["/save" :post (conj common-interceptors `save)]
    ["/save-theme" :post (conj common-interceptors `save-theme)]
    ["/assets/*" :get (conj common-interceptors `serve-user-asset)]
    ["/save-flow" :post (conj common-interceptors `save-flow)]
    ["/save-snap" :post (conj common-interceptors `save-snap)]
    ["/save-snap-block" :post (conj common-interceptors `save-snap-block)]
    ["/save-screen-snap" :post (conj common-interceptors `save-screen-snap)]
    ["/save-csv" :post (conj common-interceptors `save-csv)]
    ["/load" :get (conj common-interceptors `load-screen)]
    ["/audio" :post (conj common-interceptors `get-audio)]
    ["/load-flow" :get (conj common-interceptors `load-flow)]
    ["/load-flow-history" :get (conj common-interceptors `load-flow-history)]
    ["/reactor/:type-keyword/:keypath-keyword" :get `db/get-reactor-value :route-name :get-reactor-value]
    ["/reactor/:type-keyword/:keypath-keyword" :put `db/put-reactor-value :route-name :put-reactor-value]
    ["/reactor-hook/:type-keyword/:keypath-keyword" :post `db/manage-hook :route-name :manage-reactor-hook]})



(def web-server-port 8888)

(defn create-custom-thread-pool [prefix min-threads max-threads idle-timeout]
  (doto (QueuedThreadPool. (int max-threads) (int min-threads) (int idle-timeout))
    (.setName prefix)))

(def service
  {:env                     :prod
   ::http/routes            routes
   ::http/allowed-origins   {:creds false :allowed-origins (constantly true)}
   ::http/secure-headers    {:content-security-policy-settings {:object-src "none"}}
   ::http/resource-path     "/public"
   :max-threads             1000
   ::http/type              :jetty
   ::http/host              "0.0.0.0"
   ::http/port              web-server-port
   ::http/container-options {:h2c? true :h2? false
                             :thread-pool (create-custom-thread-pool "RVBBIT-HTTP" 4 400 10000)
                             :ssl? false}})

(defonce runnable-service (server/create-server service))

(def web-server (atom nil))

(defn init-user-space! []
  (when-let [dir (resolve-user-space-dir)]
    (ut/pp ["User space directory configured as:" dir])
    (alter-var-root #'user-space-dir (constantly dir))))

(defn create-web-server! []
  (ut/ppa [:starting-web-server :port web-server-port])
  (init-user-space!)
  (reset! web-server (server/start runnable-service)))

(defn stop-web-server! []
  (ut/ppa [:shutting-down-web-server :port web-server-port])
  (when @web-server (server/stop @web-server) (reset! web-server nil)))

(defn reboot-web-server! []
  (stop-web-server!)
  (Thread/sleep 5000)
  (create-web-server!))
















(defn run-shell-command
  "execute a generic shell command and return output as a map of timing and seq of str lines (forked fabric version)"
  [command]
  (let [;;shell                    (or (System/getenv "SHELL") "/bin/sh")
        ;;output                   (shell/sh shell "-c" (str "mkdir -p shell-root ; cd shell-root ; " command))
        output                   (shell/sh "/bin/bash" "-c" (str "mkdir -p shell-root ; cd shell-root ; " command))
        split-lines              (vec (remove empty? (cstr/split-lines (get output :out))))
        exit-code                (get output :exit)
        error                    (vec (remove empty? (cstr/split-lines (get output :err))))
        has-timing?              (if (ut/ne? error) (cstr/starts-with? (get error 0) "real") false)
        error-data               (if has-timing? [] error)
        timing-values-to-seconds #(let [split-timing (cstr/split (get (cstr/split % #"\t") 1) #"m")
                                        minutes      (edn/read-string (get split-timing 0))
                                        seconds      (edn/read-string (cstr/join "" (drop-last (get split-timing 1))))]
                                    (+ (* 60 minutes) seconds))
        timing-data              (if has-timing? (into [] (for [x error] (timing-values-to-seconds x))) [])]
    {:output     split-lines
     :exception  error-data
     :seconds    timing-data
     :exit-code  exit-code
     :command    (str command)}))

;; (defn models-list-to-map [models-list]
;;   (let [is-header? #(cstr/ends-with? % ":")]
;;     (loop [remaining models-list
;;            current-header nil
;;            current-items []
;;            result {}]
;;       (if-let [item (first remaining)]
;;         (if (is-header? item)
;;           (recur (rest remaining)
;;                  (cstr/replace item ":" "")
;;                  []
;;                  (cond-> result
;;                    current-header (assoc current-header current-items)))
;;           (recur (rest remaining)
;;                  current-header
;;                  (conj current-items item)
;;                  result))
;;         ;; last group and empty groups
;;         (cond-> result
;;           current-header (assoc current-header current-items))))))

;; (def get-fabric-models
;;   (memoize
;;    (fn []
;;      (let [output (run-shell-command "fabric --listmodels")]
;;        (if (= (get output :error-code 0) 0)
;;          (models-list-to-map (get output :output)) ;; check for malformed first?
;;          (get output :exception))))))

(defn models-list-to-map [models-list]
  (let [is-vendor? #(and (not (cstr/blank? %))
                         (not (cstr/starts-with? % "\t"))
                         (not (cstr/starts-with? % "Available")))]
    (loop [remaining models-list
           current-vendor nil
           result {}]
      (if-let [item (first remaining)]
        (cond
          (is-vendor? item)
          (recur (rest remaining)
                 (cstr/trim item)
                 result)

          (and current-vendor (cstr/starts-with? item "\t"))
          (let [model-name (cstr/trim (subs item (inc (cstr/index-of item "]"))))]
            (recur (rest remaining)
                   current-vendor
                   (update result current-vendor (fnil conj []) model-name)))

          :else
          (recur (rest remaining) current-vendor result))

        result))))

(def get-fabric-models
  (memoize
   (fn []
     (let [output (run-shell-command "fabric --listmodels")]
       (if (= (get output :exit-code 0) 0)
         (models-list-to-map (get output :output))
         (get output :exception))))))

(def get-fabric-patterns
  (memoize
   (fn []
     (let [output (run-shell-command "fabric --listpatterns")]
       (if (= (get output :error-code 0) 0)
         (mapv #(cstr/replace % #"^\t+" "") (rest (get output :output)))
         (get output :exception))))))

(defn- kebab-case
  "Convert a string to kebab-case"
  [s]
  (-> s
      (cstr/replace #"([a-z0-9])([A-Z])" "$1-$2")
      (cstr/replace #"([A-Z])([A-Z][a-z])" "$1-$2")
      (cstr/replace #"_" "-")
      (cstr/replace #":" "")
      cstr/lower-case))

(defn shell-escape [s]
  (-> s
      (cstr/replace "\\" "\\\\")   ; Double up backslashes
      (cstr/replace "'" "'\\''")   ; Escape single quotes
      (cstr/replace "\"" "\\\"")   ; Escape double quotes
      (cstr/replace "$" "\\$")     ; Escape dollar signs
      (cstr/replace "`" "\\`")     ; Escape backticks
      (cstr/replace "\n" "'\n'")   ; Handle newlines
      (->> (format "'%s'"))))      ; Wrap in single quotes

;; (defn get-fabric-models []
;;   (let [output (run-shell-command "fabric --listmodels")]
;;     (if (= (get output :error-code 0) 0)
;;       (models-list-to-map (get output :output)) ;; check for malformed first?
;;       (get output :exception))))

;; (defn get-fabric-patterns []
;;   (let [output (run-shell-command "fabric --list")]
;;     (if (= (get output :error-code 0) 0)
;;       (get output :output)
;;       (get output :exception))))



(defn fabric
  "Wrapper for the fabric CLI application.
   Required arguments:
   :input   - The input text (will be passed as --text)
   :pattern - The pattern to use

   Optional arguments (all others from the CLI help):
   :copy, :agents, :output, :session, :clear-session, :session-log,
   :list-sessions, :gui, :stream, :list, :temp, :top-p, :frequency-penalty,
   :presence-penalty, :update, :setup, :change-default-model, :model,
   :list-models, :remote-ollama-server, :context"
  [& {:keys [input pattern] :as opts}]
  (when (or (nil? input) (nil? pattern))
    (throw (IllegalArgumentException. "Both :input and :pattern are required arguments.")))

  (let [id (get opts :id)
        client-name (get opts :client-name)
        opts (dissoc opts :id :client-name :input)  ; Remove :input from opts
        opts (if id (assoc opts :output (str "/tmp/" id)) opts)
        temp-file (str "/tmp/fabric-input-" (java.util.UUID/randomUUID) ".txt")
        _ (spit temp-file input)  ; Write input to temp file
        cli-args (reduce-kv
                  (fn [args k v]
                    (into args
                          (if (boolean? v)
                            [(str "--" (name (kebab-case k)))]
                            [(str "--" (name (kebab-case k))) (str v)])))
                  ["fabric"]  ; Start with the command name
                  opts)
        command (str "cat " temp-file " | " (cstr/join " " cli-args))]
    (write-local-file (str "../fabric-sessions/" id)
                      (str "==============================================================================" "\n"
                           (ut/millis-to-date-string (System/currentTimeMillis)) " " client-name "\n \n"
                           input "\n"
                           (assoc opts :input "...") "\n \n \n" command "\n \n \n"
                           "==============================================================================" "\n")
                      true)

    (ut/pp [:fabric-cli command])

    ;; Return the command
    command))

(defn fabric-run [opts-map]
  (doall
   (let [_ (ut/pp [:fabric-run opts-map])
         ;context (get opts-map :context)
         ;pattern (get opts-map :pattern)
         {:keys [context pattern model client-name metadata runner id]} opts-map
          ;view-name (when context (last (ffirst context)))
         opts-map (cond ;(and context (= pattern "clover"))
                         ;(assoc opts-map :input (str (get opts-map :input) "\n\n ```" (pr-str context) "``` \n\n "))

                    (and context (= pattern "clover-canvas"))
                    (let [runner-hint (get-in (config/settings) [:runners runner :pattern-hint])]
                      (assoc opts-map :input (str

                                              (when
                                               (= pattern "clover-canvas")
                                               ;;(and (or (= model "o1-preview") (= model "o1-mini")) (= pattern "clover-canvas"))
                                                (slurp "/home/ryanr/.config/fabric/patterns/clover-canvas/context.md")) ;; inject manually due to fabric issue

                                              "\n\n"

                                              (get opts-map :input) "\n\n ```" (pr-str context) "``` \n\n "
                                                  (when runner-hint
                                                    (str "##SPECIAL BLOCK TYPE INSTRUCTIONS:  \n" (str runner-hint)))
                                                  (when (ut/ne? metadata)
                                                    (str "##QUERY/TABLE METADATA: \n" (pr-str metadata))))))

                    :else opts-map)

          ;; prev-times       (get @times-atom [pattern model] [-1])
          ;; ship-est         (fn [client-name]
          ;;                    (try (let [times (ut/avg ;; avg based on last 10 runs, but only if >
          ;;                                      (vec (take-last 10 (vec (remove #(< % 1) prev-times)))))]
          ;;                           (when (not (nil? times))
          ;;                             (kick client-name [:estimate] {view-name {:times times :run-id (hash opts-map)}} nil nil nil)))
          ;;                         (catch Exception e (ut/pp [:error-shipping-estimates (Throwable->map e) (str e)]) 0)))
          ;; _ (ship-est client-name)

         opts-mod (-> opts-map (assoc :temperature 1) (dissoc opts-map :context :runner) (assoc :topp 1))
         command (fabric opts-mod)
         ;;command (fabric (assoc (dissoc opts-map :context :runner) :temperature 1))
         result (run-shell-command command)
        ;;  _ (ut/pp [:fabric-result result])
         output (get result :output)]
     (write-local-file (str "../fabric-sessions/" id)
                       (str (ut/millis-to-date-string (System/currentTimeMillis)) " :" (get opts-map :model)
                            "\n \n" output "\n \n \n") true)
     [output opts-map])))


;; (kick :powerful-oval-gorilla-17 [:estimate] {:test {:times [2 3 2 33 22] :run-id "fsdfsdfsdff2f23f"}} nil nil nil)

;; (fabric-run {:model "claude-3-opus-20240229"
;;              :input "tell a very short story about vampire rabbits ransacking a sleepy new england town"
;;              :client-name :beneficial-fat-badger-0
;;              :pattern "tweet"})



