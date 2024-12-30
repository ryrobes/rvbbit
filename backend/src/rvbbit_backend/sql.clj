(ns rvbbit-backend.sql
  (:require
   ;[rvbbit-backend.xtdb-init :as xtdb-init]
   [clj-time.coerce     :as tc]
   [clj-time.core       :as time]
  ;;  [clj-time.jdbc]
   [clojure.edn         :as edn]
   [clojure.java.jdbc   :as jdbc]
   [clojure.string      :as cstr]
   [clojure.walk        :as walk]
   [hikari-cp.core      :as hik]
   [honey.sql           :as honey]
   [honey.sql.helpers :as helpers]
   [cheshire.core       :as json]
   [rvbbit-backend.util :as ut]
   [rvbbit-backend.db :as db]
   [rvbbit-backend.external :as ext]
   [rvbbit-backend.queue-party :as qp]
   [rvbbit-backend.pool-party :as ppy]
   [rvbbit-backend.surveyor :as svy]
   [taskpool.taskpool   :as tp])
  (:import (java.time Instant)
           (java.time.format DateTimeFormatter)))

(def query-history (atom {}))

(defn pool-create
  [jdbc-body name]
  (delay (let [base  {:idle-timeout 600000 :max-lifetime 1800000 :pool-name name}
               bbase (merge base jdbc-body)
               ;pre-filter-fn (get jdbc-body :pre-filter-fn)
               conn (hik/make-datasource bbase)]
           (swap! db/conn-map assoc name {:datasource conn})
           conn)))

(defn close-pool [ds]
  (try (hik/close-datasource ds)
       (catch Exception e (ut/pp [:close-pool-error! e]))))

(def client-db-pools (atom {}))

(declare sql-exec sql-query system-db)

(defn create-or-get-client-db-pool [db-name]
  (let [db-key (keyword db-name)]
    (if-let [pool (@client-db-pools db-key)]
      {:datasource pool}
      (let [_ (ut/pp [:creating-new-client-db-pool db-name])
            _ (ext/create-dirs "db/client-temp") ;; just in case
            ;;client-name-str (-> (str db-name) (cstr/replace  ":" "") (cstr/replace  "-" "_"))
            ;;_ (sql-query system-db (str "CREATE DATABASE " client-name-str ";"))
            pool @(pool-create

                   {:jdbc-url (str "jdbc:sqlite:file:./db/client-temp/" ;; should move to duckdb anyways
                                   (cstr/replace (str db-name) ":" "")
                                   ".db"
                                   "?cache=shared&journal_mode=WAL&mode=memory&busy_timeout=5000&locking_mode=NORMAL" ;; mem
                                   ;;"?cache=shared&journal_mode=WAL&busy_timeout=50000&locking_mode=NORMAL&mmap_size=268435456&auto_vacuum=FULL" ;; disk
                                   )
                    ;:idle-timeout      600000
                    :maximum-pool-size 1
                    ;:max-lifetime      1800000
                    :cache             "shared"}

                  ;;  {:auto-commit        true
                  ;;   :connection-timeout 30000
                  ;;   :validation-timeout 5000
                  ;;   :idle-timeout       600000
                  ;;   :max-lifetime       1800000
                  ;;   ;:minimum-idle       10
                  ;;   :maximum-pool-size  1
                  ;;   :jdbc-url           "jdbc:postgresql://localhost:5432/postgres"
                  ;;   :adapter            "postgresql"
                  ;;   :username           "postgres"
                  ;;   :password           "notofox"
                  ;;   :database-name      client-name-str
                  ;;   :server-name        "localhost"
                  ;;   :port-number        5432
                  ;;   :register-mbeans    false}


                   (cstr/replace (str db-name) ":" ""))]
        (swap! client-db-pools assoc db-key pool)
        {:datasource pool}))))

(defn close-client-db-pool
  [db-name]
  (let [db-key (keyword db-name)]
    (when-let [pool (@client-db-pools db-key)]
      (close-pool pool)
      (swap! client-db-pools dissoc db-key))))

;; (ut/pp (filterv #(cstr/includes? (str %) "-") (mapv #(cstr/replace (str %) ":" "") (keys @client-db-pools))))

(def system-db
  {:datasource
   @(pool-create
     {:jdbc-url
      ;;"jdbc:sqlite:file:./db/system.db?cache=shared&journal_mode=WAL&mode=memory&busy_timeout=5000&locking_mode=NORMAL"
      "jdbc:sqlite:file:./db/system.db?cache=shared&journal_mode=WAL&busy_timeout=50000&locking_mode=NORMAL&mmap_size=268435456&auto_vacuum=FULL"
      ;:idle-timeout      600000
      :maximum-pool-size  20  ;5 ;20 ;1 ;5 ;20
      ;:max-lifetime      1800000
      :cache             "shared"}
     "system-db")})

;; (def systemh2-db
;;   {:datasource
;;    @(pool-create
;;      {:adapter    "h2"
;;       :url        "jdbc:h2:file:./db/systemh2-db;MODE=PostgreSQL;DATABASE_TO_UPPER=false;CACHE_SIZE=65536;WRITE_DELAY=500;PAGE_SIZE=4096;"
;;       :username     "sa"
;;       :password     ""
;;       :maximum-pool-size 50}
;;      "systemh2-db")})

(def systemh2-db
  {:datasource
   @(pool-create
     {:adapter    "h2"
      :url        "jdbc:h2:file:./db/systemh2-db;MODE=PostgreSQL;DATABASE_TO_UPPER=false;CACHE_SIZE=32768;WRITE_DELAY=500;PAGE_SIZE=4096;LOCK_MODE=0;COMPRESS=TRUE;MAX_MEMORY_ROWS=10000;LOCK_TIMEOUT=10000;DB_CLOSE_DELAY=-1;QUERY_CACHE_SIZE=8;"
      :username   "sa"
      :password   ""
      :maximum-pool-size 50}
     "systemh2-db")})

;;       ;;:url   "jdbc:h2:file:./db/systemh2-db?MODE=PostgreSQL&DATABASE_TO_UPPER=false&AUTO_SERVER=TRUE&CACHE_SIZE=65536&COMPRESS=TRUE&WRITE_DELAY=500&PAGE_SIZE=4096"

;; (def system-db
;;   {:datasource
;;    @(pool-create
;;      {:auto-commit        true
;;       :connection-timeout 30000
;;       :validation-timeout 5000
;;       :idle-timeout       600000
;;       :max-lifetime       1800000
;;       ;:minimum-idle       10
;;       :maximum-pool-size  1
;;       :jdbc-url           "jdbc:postgresql://localhost:5432/postgres"
;;       :adapter            "postgresql"
;;       :username           "postgres"
;;       :password           "notofox"
;;       ;; :connection-init-sql (str "DO
;;       ;;                      $do$
;;       ;;                      BEGIN
;;       ;;                         IF EXISTS (SELECT FROM pg_database WHERE datname = 'rabbit') THEN
;;       ;;                            RAISE NOTICE 'Database already exists';  -- optional
;;       ;;                         ELSE
;;       ;;                            PERFORM dblink_exec('dbname=' || current_database()  -- current db
;;       ;;                                              , 'CREATE DATABASE rabbit');
;;       ;;                         END IF;
;;       ;;                      END
;;       ;;                      $do$;")
;;       :database-name      "rabbit"
;;       :server-name        "localhost"
;;       :port-number        5432
;;       :register-mbeans    false}
;;      "system-db")})

;; (def system-db
;;   {:datasource
;;    @(pool-create
;;      {:jdbc-url "jdbc:h2:./db/system.h2?MVCC=TRUE&DATABASE_TO_UPPER=FALSE"
;;       :idle-timeout      600000
;;       ;:maximum-pool-size 5 ;20
;;       :max-lifetime      1800000}
;;      "system-db")})

;; (def system-xtdb
;;   {:datasource
;;    @(pool-create
;;      {;:auto-commit        true
;;       ;:read-only          true
;;       ;:connection-timeout 30000
;;       ;:validation-timeout 5000
;;       :idle-timeout       600000
;;       :max-lifetime       1800000
;;       ;:minimum-idle       10
;;       ;:maximum-pool-size  10
;;       :jdbc-url           "jdbc:postgresql://localhost:5432/postgres"
;;       :adapter            "postgresql"
;;      ;; :username           "postgres"
;;      ;; :password           "test"
;;      ;; :database-name      "rabbit"
;;       :server-name        "localhost"
;;       :port-number        5432
;;       :register-mbeans    false}
;;      "system-xtdb")})

;; (def system-db ;; duck test
;;   {:datasource
;;    @(pool-create
;;      {:jdbc-url "jdbc:duckdb:./db/system.duck"
;;       ;:driver-class-name "org.duckdb.DuckDBDriver"
;;       :idle-timeout      600000
;;       :maximum-pool-size 20
;;       :max-lifetime      1800000}
;;      "system-db")})


;; (def history-db
;;   {:datasource
;;    @(pool-create
;;      {:jdbc-url
;;       ;;;"jdbc:sqlite:file:./db/history.db?cache=shared&journal_mode=WAL&mode=memory&busy_timeout=50000&locking_mode=NORMAL&auto_vacuum=FULL"
;;            ;"jdbc:sqlite:file::memory:?cache=shared&journal_mode=WAL&busy_timeout=5000&locking_mode=NORMAL"
;;            ;"jdbc:sqlite:file::memory:?cache=shared&journal_mode=WAL&busy_timeout=5000&locking_mode=NORMAL&cache_size=-20000"
;;       "jdbc:sqlite:file:./db/history.db?cache=shared&journal_mode=WAL&busy_timeout=5000&locking_mode=NORMAL&mmap_size=268435456&auto_vacuum=FULL"
;;       :idle-timeout      600000
;;       :maximum-pool-size 20
;;       :max-lifetime      1800000
;;       :cache             "shared"}
;;      "history-db")})

;; (def autocomplete-db
;;   {:datasource
;;    @(pool-create
;;      {:jdbc-url
;;       ;;"jdbc:sqlite:file:./db/autocomplete.db?cache=shared&journal_mode=WAL&mode=memory&busy_timeout=50000&locking_mode=NORMAL"
;;       "jdbc:sqlite:file:./db/autocomplete.db?cache=shared&journal_mode=WAL&busy_timeout=5000&locking_mode=NORMAL&mmap_size=268435456&auto_vacuum=FULL"
;;       :idle-timeout      600000
;;       :maximum-pool-size 20
;;       :max-lifetime      1800000
;;       :cache             "shared"}
;;      "autocomplete-db")})


;; (def system-db
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
;;       :database-name      "rvbbit_system" ;;"postgres" ;; where citus is installed apparently
;;       :server-name        "10.174.1.248"
;;       ;:server-name        "localhost"
;;       :port-number        5432
;;       :register-mbeans    false}
;;      "system-db")})



;; (def flows-db
;;   {:datasource
;;    @(pool-create
;;      {:jdbc-url
;;            ;"jdbc:sqlite:file:./db/flow.db?cache=shared&journal_mode=WAL&auto_vacuum=FULL"
;;            ;;&journal_mode=WAL"
;;       "jdbc:sqlite:file:./db/flow.db?cache=shared&journal_mode=WAL&busy_timeout=50000&locking_mode=NORMAL&mmap_size=268435456&auto_vacuum=FULL" ;
;;       :idle-timeout 600000
;;       :max-lifetime 1800000
;;       :auto_vacuum  "FULL"
;;       :cache        "shared"}
;;      "flows-db")})

;; (def flows-db
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
;;       :database-name      "rvbbit_flow" ;;"postgres" ;; where citus is installed apparently
;;       :server-name        "10.174.1.248"
;;       ;:server-name        "localhost"
;;       :port-number        5432
;;       :register-mbeans    false}
;;      "flows-db")})

;; (def ghost-db
;;   {:datasource
;;    @(pool-create
;;      {:jdbc-url     "jdbc:sqlite:file:./db/ghost.db?cache=shared&journal_mode=WAL&mode=memory" ;&journal_mode=WAL"
;;       :idle-timeout 600000
;;       :max-lifetime 1800000
;;       :cache        "shared"}
;;      "ghost-db")})

(def ghost-db
  {:datasource
   @(pool-create
     {:jdbc-url     "jdbc:sqlite:file::memory:?journal_mode=OFF&synchronous=OFF&temp_store=MEMORY" ;; all memory logic ops
      :idle-timeout 600000
      :max-lifetime 1800000}
     "ghost-db")})

(def import-db
  {:datasource
   @(pool-create
     {:jdbc-url
      "jdbc:sqlite:file:./db/import.db?cache=shared&journal_mode=WAL&busy_timeout=50000&locking_mode=NORMAL&mmap_size=268435456&auto_vacuum=FULL" ;
      :idle-timeout 600000
      :max-lifetime 1800000
      :auto_vacuum  "FULL"
      :cache        "shared"}
     "import-db")})

(def mem-cache-db
  {:datasource
   @(pool-create ;; these tables get removed after being used, so it causes too much write thrash to put in the user-space memory db
                 ;; (plus this is more transatiotional and SQLIte works better)
     {:jdbc-url  ;; used for ephemeral table testing / metadata sniffing. fast and doesn't need to be persistent.
      ;;"jdbc:sqlite:file:./db/temp-cache.db?mode=memory&cache=shared&journal_mode=WAL"
      "jdbc:sqlite:file:./db/temp-cache.db?cache=shared&journal_mode=WAL&busy_timeout=50000&locking_mode=NORMAL&mmap_size=268435456"
      :cache    "shared"}
     "mem-cache-db")})

;; (def realms-db
;;   {:datasource
;;    @(pool-create
;;      {:jdbc-url
;;       "jdbc:sqlite:file:./db/realms.db?mode=memory&cache=shared&journal_mode=WAL"
;;       ;;"jdbc:sqlite:file:./db/realms.db?cache=shared&journal_mode=WAL&busy_timeout=50000&locking_mode=NORMAL&mmap_size=268435456"
;;       :cache    "shared"}
;;      "realms-db")})

;; (def realms-db
;;   {:datasource
;;    @(pool-create
;;      {:jdbc-url "jdbc:duckdb:./db/realms.duck"
;;       :idle-timeout      600000
;;       :maximum-pool-size 200
;;       ;;:connection-init-sql (str "CREATE SCHEMA IF NOT EXISTS " default-schema "; USE " default-schema ";")
;;       :max-lifetime      1800000}
;;      "realms-db-pool")})

(def system-reporting-db
  {:datasource
   @(pool-create
     {:jdbc-url "jdbc:duckdb:./db/system-reporting.duck"
      :idle-timeout      600000
      :maximum-pool-size 200
      ;;:connection-init-sql (str "CREATE SCHEMA IF NOT EXISTS " default-schema "; USE " default-schema ";")
      :max-lifetime      1800000}
     "system-reporting-db")})

(def default-schema "base")

(def cache-db
  {:datasource
   @(pool-create
     {:jdbc-url "jdbc:duckdb:./db/cache.duck"
      :idle-timeout      600000
      :maximum-pool-size 200
      ;;:connection-init-sql (str "CREATE SCHEMA IF NOT EXISTS " default-schema "; USE " default-schema ";")
      :max-lifetime      1800000}
     "cache-db")})

(def stack-db
  {:datasource
   @(pool-create
     {:jdbc-url "jdbc:duckdb:./db/stack.duck"
      :idle-timeout      600000
      :maximum-pool-size 200
      :connection-init-sql (str "CREATE SCHEMA IF NOT EXISTS " default-schema "; USE " default-schema ";")
      :max-lifetime      1800000}
     "stack-db")})

(def metrics-kpi-db
  {:datasource
   @(pool-create
     {:jdbc-url "jdbc:duckdb:./db/metrics-kpi.duck"
      :idle-timeout      600000
      :maximum-pool-size 200
      ;;:connection-init-sql (str "CREATE SCHEMA IF NOT EXISTS " default-schema "; USE " default-schema ";")
      :max-lifetime      1800000}
     "metrics-kpi-db")})

(def cache-db-memory
  {:datasource
   @(pool-create
     {:jdbc-url "jdbc:duckdb::memory:cache-db-memory" ;; named instance should use a shared cache
      :idle-timeout      600000
      :maximum-pool-size 200
      ;;:connection-init-sql (str "CREATE SCHEMA IF NOT EXISTS " default-schema "; USE " default-schema ";")
      :max-lifetime      1800000}
     "cache-db-memory")})



(defn insert-error-row-OLD! [error-db-conn query error]
  (jdbc/with-db-connection
    [sdb system-db] ;; ?
    (let [ins (-> {:insert-into [:errors] :values [{:db_conn (str error-db-conn) :sql_stmt (str query) :error_str (str error)}]}
                  (honey/format {:pretty false :inline true}))]
      (try (jdbc/db-do-commands sdb ins) (catch Exception e (ut/pp {:error-inserting-error e :query query}))))))

(defonce errors (atom [])) ;; (fpop/thaw-atom [] "./data/atoms/sql-errors.edn")) ;; no need to persist for now...

;; (ut/pp (take 100 (filter #(cstr/includes? (str %) "cache-db-pool") @errors)))

(defn insert-error-row! [error-db-conn query error & [silent?]]
  (swap! errors conj [(str error) (str error-db-conn) query])
  (when (and (not (cstr/includes? (str error) "already exists"))
             ;(not (cstr/includes? (str query) "client_memory"))
             ;(not (cstr/includes? (str query) "client_stats"))
             (not (cstr/includes? (str query) "No results were returned by the query"))
             (not (cstr/includes? (str error) "no such column: diff"))
             (not (cstr/includes? (str error) "no such column: mb")))
    ;; ^^ dont care about redundant create view ddl failing for now. TODO - just clearing console output for debugging 'real' messages
    (when (not silent?)
      (ut/pp {:sql-error! error :error-db-conn error-db-conn :query (try (subs (str query) 0 400) (catch Throwable _ (str query)))}))))

(defn get-connection-string-info [db-spec]
  (when-let [datasource (:datasource db-spec)]
    (when (instance? com.zaxxer.hikari.HikariDataSource datasource)
      (let [config (.getHikariConfigMXBean datasource)]
        (str "HikariCP connection: "
             "jdbc:" (.getJdbcUrl config)
             " (pool: " (.getPoolName config) ")")))))

(def sql-queries-run (atom 0))
(def sql-query-log (atom []))

(defn instant->str [instant]
  (.format (java.time.format.DateTimeFormatter/ISO_INSTANT) instant))

(defn convert-value [v]
  (cond
    (instance? java.time.Instant v) (instant->str v)
    (instance? java.sql.Timestamp v) (instant->str (.toInstant v))
    (instance? java.sql.Date v) (.toString v)
    (instance? java.time.LocalDate v) (.toString v)

    ;; ;; SQL Server specific types
    ;; (and (try (Class/forName "microsoft.sql.DateTimeOffset") true
    ;;           (catch ClassNotFoundException _ false))
    ;;      (instance? (Class/forName "microsoft.sql.DateTimeOffset") v))
    ;; (.toString v)  ;; Convert SQL Server's DateTimeOffset to string

    ;; ;; For SQL Server JSON strings that are already parsed by the driver
    ;; (and (string? v)
    ;;      (try
    ;;        (let [trimmed (cstr/trim v)]
    ;;          (and (or (cstr/starts-with? trimmed "{")
    ;;                   (cstr/starts-with? trimmed "["))
    ;;               (or (cstr/ends-with? trimmed "}")
    ;;                   (cstr/ends-with? trimmed "]"))
    ;;               (json/parse-string v true)))
    ;;        (catch Exception _ false)))
    ;; (let [parsed (json/parse-string v true)]
    ;;   (if (sequential? parsed)
    ;;     (vec parsed)
    ;;     parsed))

    ;; DuckDB JSON/Struct handling
    (instance? org.duckdb.DuckDBStruct v)
    (let [fields (.getFields v)]
      (if (= 1 (count fields))  ; Special case for single-value structs like {=1}
        (.get v (first fields))  ; Just return the value
        (str (json/generate-string  ; Otherwise handle as regular struct
              (into {} (map (fn [k]
                              [k (.get v k)])
                            fields))))))

    ;; PostgreSQL JSON
    (instance? org.postgresql.util.PGobject v)
    (let [type (.getType v)
          value (.getValue v)]
      (when value
        (case type
          ("json" "jsonb") (str
                            (let [parsed (json/parse-string value true)]
                              (if (sequential? parsed)
                                (vec parsed)
                                parsed)))
          value)))

    ;; ;; Oracle JSON handling
    ;; (and (try (Class/forName "oracle.sql.json.OracleJsonDatum") true
    ;;           (catch ClassNotFoundException _ false))
    ;;      (instance? (Class/forName "oracle.sql.json.OracleJsonDatum") v))  ;; Modern Oracle JSON type
    ;; (when v
    ;;   (let [parsed (json/parse-string (.getString v) true)]
    ;;     (if (sequential? parsed)
    ;;       (vec parsed)
    ;;       parsed)))

    ;; ;;  Catch-all for any other objects
    ;; (instance? Object v)
    ;; (try
    ;;   (str v)  ; Convert any object to string representation
    ;;   (catch Exception _
    ;;     (str "#object[" (.getName (class v)) "]")))  ; Ultra-safe fallback
    :else v))

(defn asort [m order] ;; we have to ensure the order of the keys is consistent to the query, as a SQL normie would expect (and I agree!)
  (let [order-map (apply hash-map (interleave order (range)))]
    (into (sorted-map-by #(compare (order-map %1) (order-map %2)))
          (select-keys m order))))

(defonce map-orders (atom {}))

(defn wrap-maps [query arrays]
  (let [headers (first arrays)
        mm      (vec (doall (for [r (rest arrays)]
                              (asort (into {}
                                           (for [i (map vector headers r)]
                                             {(first i) (convert-value (last i))}))
                                     headers))))]
    (swap! map-orders assoc query headers)
    mm))

(defn sql-query
  [db-spec query & extra]
  (swap! sql-queries-run inc)
  (let [start-time (System/nanoTime)]
    (jdbc/with-db-connection [t-con db-spec]
      (ut/ppln [query db-spec])
      (try
        (let [result (wrap-maps query (jdbc/query t-con query {:identifiers keyword :as-arrays? true :result-set-fn doall}))
              end-time (System/nanoTime)
              execution-time-ms (/ (- end-time start-time) 1e6)]
          ;; (swap! sql-query-log conj [(-> (last (cstr/split (str (:datasource db-spec)) #" ")) (cstr/replace "(" "") (cstr/replace ")" "") keyword)
          ;;                            (str (try (subs (str query) 0 300)
          ;;                                      (catch Throwable _ (str query))) "-" (hash query)) execution-time-ms])
          result)
        (catch Exception e
          (let [end-time (System/nanoTime)
                execution-time-ms (/ (- end-time start-time) 1e6)
                ;_ (ut/pp [:sql-error-extra extra (not (get-in extra [0 :no-error?] false))])
                ]
            ;(when (not (get (first extra) :no-error? false))
              (insert-error-row! db-spec query e (get (first extra) :no-error? false))
            ;  )
            [{:database_says (str (.getMessage e))}
             {:database_says "(from database connection)"}
             {:database_says (get-connection-string-info db-spec)}
             {:execution_time_ms execution-time-ms}]))))))




(defn sql-query-one
  [db-spec query & extra]
  (swap! sql-queries-run inc)
  (let [start-time (System/nanoTime)]
    (jdbc/with-db-connection [t-con db-spec]
      (ut/ppln [query db-spec])
      (try
        (let [result (wrap-maps query (jdbc/query t-con query {:identifiers keyword :as-arrays? true :result-set-fn doall}))
              result (first (vals (first result)))
              end-time (System/nanoTime)
              execution-time-ms (/ (- end-time start-time) 1e6)]
          ;; (swap! sql-query-log conj [(-> (last (cstr/split (str (:datasource db-spec)) #" ")) (cstr/replace "(" "") (cstr/replace ")" "") keyword)
          ;;                            (str (try (subs (str query) 0 300)
          ;;                                      (catch Throwable _ (str query))) "-" (hash query)) execution-time-ms])
          result)
        (catch Exception e
          (let [end-time (System/nanoTime)
                execution-time-ms (/ (- end-time start-time) 1e6)]
            (ut/pp {:sql-query-one-fn-error e :db (-> (last (cstr/split (str (:datasource db-spec)) #" ")) (cstr/replace "(" "") (cstr/replace ")" "") keyword) :query query :extra (when extra extra)})
            (insert-error-row! db-spec query e)
            [{:database_says (str (.getMessage e))}
             {:database_says "(from database connection)"}
             {:database_says (get-connection-string-info db-spec)}
             {:execution_time_ms execution-time-ms}]))))))


;; (defn sql-query-one
;;   [db-spec query & extra]
;;   (jdbc/with-db-connection ;; for connection hygiene - atomic query
;;     [t-con db-spec]
;;     (ut/ppln [query db-spec])
;;     (try (first (vals (first (jdbc/query t-con query {:identifiers keyword}))))
;;          (catch Exception e
;;            (do (ut/pp {:sql-query-one-fn-error e :query query :extra (when extra extra)}) (insert-error-row! db-spec query e))))))


(defn sql-query-meta
  [db-spec query]
  (jdbc/with-db-connection ;; for connection hygiene - atomic query
    [t-con db-spec]
    (jdbc/metadata-result (.getMetaData (jdbc/query t-con query {:identifiers keyword})))));)








(defn sql-exec2-old ;; used for rowset-sql-query, diff use case, not need to run serially
  [db-spec query & [extra]]
  (jdbc/with-db-transaction [t-con db-spec {:isolation :read-uncommitted}]
    (try (jdbc/db-do-commands t-con query)
         (catch Exception e
           (do (ut/pp (merge {:sql-exec-fn-error e :db-spec db-spec :query query}
                             (if extra {:ex extra} {})))
               (insert-error-row! db-spec query e))))))








;; (def task-queue-sql (java.util.concurrent.LinkedBlockingQueue.))
;; (def running-sql (atom true))
;; (def worker-sql (atom nil)) ; Holds the future of the worker thread

;; (defn enqueue-task-sql [task] (.put task-queue-sql task))

;; (defn worker-loop-sql
;;   []
;;   (loop [] (when @running-sql (let [task (.take task-queue-sql)] (task))))
;;   (recur))

;; (defn start-worker-sql
;;   []
;;   (ut/pp [:starting-sync-worker-thread :sql-sync])
;;   (reset! running-sql true)
;;   (reset! worker-sql (future (worker-loop-sql))))

;; (defn stop-worker-sql
;;   []
;;   (reset! running-sql false)
;;   (when-let [w @worker-sql]
;;     (future-cancel w)
;;     (while (not (.isDone w)) ; Ensure the future is cancelled before proceeding
;;       (Thread/sleep 60))))

;; (defn recycle-worker-sql [] (stop-worker-sql) (start-worker-sql))



;; (def task-queues-sql (atom {})) ; Holds the queues for each keyword
;; (def running-sql (atom true))
;; (def workers-sql (atom {})) ; Holds the futures of the worker threads for each keyword

;; (defn worker-loop-sql
;;   [keyword]
;;   (loop []
;;     (when @running-sql (let [queue (@task-queues-sql keyword) task (when queue (.take queue))] (when task (task))))
;;     (recur)))

;; (defn start-workers-sql
;;   [keyword num-workers]
;;   (reset! running-sql true)
;;   (swap! workers-sql assoc keyword (doall (map (fn [_] (future (worker-loop-sql keyword))) (range num-workers)))))

;; (defn enqueue-task-sql
;;   [keyword task]
;;   (let [queue (or (@task-queues-sql keyword)
;;                   (do (swap! task-queues-sql assoc keyword (java.util.concurrent.LinkedBlockingQueue.))
;;                       (@task-queues-sql keyword)))]
;;     (.put queue task)
;;     (when (not (@workers-sql keyword)) (start-workers-sql keyword 1))))
;; ;;; ^^ move over to queue party generic  blocking queue handling...

;; (defn stop-workers-sql
;;   [keyword]
;;   (reset! running-sql false)
;;   (doseq [w (@workers-sql keyword)]
;;     (future-cancel w)
;;     (while (not (.isDone w)) ; Ensure the future is cancelled before proceeding
;;       (Thread/sleep 60)))
;;   (let [queue (@task-queues-sql keyword)]
;;     (while (and queue (not (.isEmpty queue))) ; Wait until the task queue is empty
;;       (Thread/sleep 60))))

;; (defn recycle-workers-sql [keyword num-workers] (stop-workers-sql keyword) (start-workers-sql keyword num-workers))

(def sql-exec-run (atom 0))
(def sql-exec-log (atom []))

;; (ut/pp (reverse (sort-by last (frequencies @sql-exec-log))))
;; (reset! sql-exec-log  [])

(defn sql-exec
  [db-spec query & [extra]]
  ;(enqueue-task-sql db-spec
  (let [;queue-name (or (try (get extra :queue :general-sql) (catch Exception _ :general-sql)) :general-sql)
        db-kw (-> (last (cstr/split (str (:datasource db-spec)) #" ")) (cstr/replace "(" "") (cstr/replace ")" "") keyword)]
     ;(qp/serial-slot-queue :sql-serial :sql
    (ppy/execute-in-thread-pools-but-deliver (keyword (str "sqlite-serial-writes" (hash (str db-spec)))) ;; :sqlite-serial-writes
     ;(keyword (str "sql-serial/" (cstr/replace (str db-kw) ":" ""))) ;; :sql-serial db-kw ;; (str db-spec) ;; queue-name
                                             (fn []
                                               (swap! sql-exec-run inc)
                                               (swap! sql-exec-log conj [db-kw
                                                                         (str (try (subs (str query) 0 300)
                                                                                   (catch Throwable _ (str query))))])
                                               (jdbc/with-db-connection [t-con db-spec]
                                                 (try (jdbc/db-do-commands t-con query)
                                                      (catch Exception e
                                                        (do ;(ut/pp (merge {:sql-exec-fn-error e
                    ;               :db-spec           db-spec
                    ;               :query             (try (subs (str query) 0 1000)
                    ;                                       (catch Throwable _ (str query)))}
                    ;              (if extra {:ex extra} {})))
                                                          (insert-error-row! db-spec query e)))))))))



(defn sql-exec2
  [db-spec query & [extra]]
  ;(enqueue-task-sql db-spec
  (let [queue-name (str db-spec)]
    ;(qp/serial-slot-queue :sql-serial2 queue-name
    (ppy/execute-in-thread-pools-but-deliver :sqlite-serial-writes ;;:serial-filter-sql-db
                                             (fn []
                                               (swap! sql-exec-run inc)
                                               (swap! sql-exec-log conj [(-> (last (cstr/split (str (:datasource db-spec)) #" ")) (cstr/replace "(" "") (cstr/replace ")" "") keyword)
                                                                         (str (try (subs (str query) 0 300)
                                                                                   (catch Throwable _ (str query))))])
                                               (jdbc/with-db-connection [t-con db-spec]
                                                 (try (jdbc/db-do-commands t-con query)
                                                      (catch Exception e
                                                        (do ;(ut/pp (merge {:sql-exec-fn-error e
                    ;               :db-spec           db-spec
                    ;               :query             (try (subs (str query) 0 1000)
                    ;                                       (catch Throwable _ (str query)))}
                    ;              (if extra {:ex extra} {})))
                                                          (insert-error-row! db-spec query e)))))))))






(defn sql-exec-no-t ;; older experiment from ducksdb - jdbc driver didnt support db-do-commands
  [db-spec query & [extra]]
  (jdbc/with-db-connection [conn db-spec]
    (try (.execute conn query) ; Directly executing the query
         (catch Exception e
           (do (ut/pp (merge {:sql-exec-fn-error e :db-spec db-spec :query query}
                             (if extra {:ex extra} {})))
               (insert-error-row! db-spec query e))))))

(defn sql-exec-33333
  [db-spec query & [extra]]
  (let [query      (if (vector? query) (first query) query)
        datasource (if (instance? com.zaxxer.hikari.HikariDataSource (:datasource db-spec)) (:datasource db-spec) nil)
        conn       (if datasource (.getConnection datasource) nil)]
    (try (let [stmt (.createStatement conn)]
           (.setAutoCommit conn true)
           (try (.execute stmt query) (finally (.close stmt))))
         (catch Exception e
           (do (ut/pp (merge {:sql-exec-fn-error e :db-spec db-spec :query query} (if extra {:ex extra} {})))
               (insert-error-row! db-spec query e)))
         (finally (when conn (.close conn))))))








(defn lookup-value
  [key-vector lookup-key system-db]
  (let [db-type       (nth key-vector 0)
        connection_id (nth key-vector 1) ;; key to the connections table
        table-type    (nth key-vector 2)
        db_schema     (nth key-vector 3)
        db_catalog    (nth key-vector 4)
        table_name    (nth key-vector 5)
        field_name    (nth key-vector 6)
        field_type    (nth key-vector 7)
        data_type     (nth key-vector 8)
        context       (cstr/split (str lookup-key) #"/")
        src-table     (read-string (first context))
        src-field     (last context)
        lookup-honey  (cond (= src-table :tests)      {:select [[[:raw
                                                                  "coalesce(test_val_string, test_val_integer, test_val_float)"]
                                                                 :v]]
                                                       :from   [:tests]
                                                       :where  [:and [:= :test-name src-field] [:= :connection-id connection_id]
                                                                [:= :table-name table_name] [:= :db-schema db_schema]
                                                                [:= :db-catalog db_catalog] [:= :field-name field_name]]
                                                       :limit  1}
                            (= src-table :attributes) {:select [[:attribute_value :v]]
                                                       :from   [:attributes]
                                                       :where  [:and [:= :attribute-name src-field]
                                                                [:= :connection-id connection_id] [:= :table-name table_name]
                                                                [:= :db-schema db_schema] [:= :db-catalog db_catalog]
                                                                [:= :field-name field_name]]
                                                       :limit  1}
                            (= src-table :sample)     {:select [[:key_hash :v]]
                                                       :from   [:tests]
                                                       :where  [:and [:= :test-name src-field] [:= :connection-id connection_id]
                                                                [:= :table-name table_name] [:= :db-schema db_schema]
                                                                [:= :db-catalog db_catalog] [:= :field-name field_name]]
                                                       :limit  1})
        lookup-value  (sql-query-one system-db
                                     (-> lookup-honey
                                         (honey/format {:pretty false :inline true})))
        val           (cond (= src-table :attributes) (= 1 lookup-value)
                            (= src-table :sample)     (keyword lookup-value)
                            :else                     lookup-value)]
    val))


(defn to-sql
  [honey-map & [key-vector system-db]] ;; db-typer-fn TODO to do proper MSSQL dialect here
  (let [sql-dialect (when key-vector
                      (cond (cstr/includes? (cstr/lower-case (first key-vector)) "microsoft") :sqlserver
                            (cstr/includes? (cstr/lower-case (first key-vector)) "mysql")     :mysql
                            (cstr/includes? (cstr/lower-case (first key-vector)) "oracle")    :oracle
                            :else                                                             nil))
        key-farm (ut/deep-flatten honey-map)
        value-keys (filter #(or (cstr/starts-with? (str %) ":tests/")
                                (cstr/starts-with? (str %) ":sample/")
                                (cstr/starts-with? (str %) ":attributes/"))
                           key-farm)
        value-keys?
        #_{:clj-kondo/ignore [:not-empty?]}
        (and (not (empty? value-keys)) (not (nil? key-vector)) (not (nil? system-db)))
        value-mapping (if value-keys? (into {} (for [v value-keys] {v (lookup-value key-vector v system-db)})) {})
        hm (walk/postwalk-replace value-mapping honey-map) ; (walk/postwalk {} honey-map)
        fixed-honey-map ;;hm
        (cond (and (= sql-dialect :sqlserver) (get hm :limit)) (let [limit           (get hm :limit)
                                                                     select-distinct (get hm :select-distinct)
                                                                     select          (get hm :select)]
                                                                 (cond select-distinct (dissoc (dissoc
                                                                                                (assoc hm
                                                                                                       :select-distinct-top
                                                                                                       (vec (cons
                                                                                                             [limit]
                                                                                                             select-distinct)))
                                                                                                :select-distinct)
                                                                                               :limit)
                                                                       select          (dissoc (dissoc (assoc hm
                                                                                                              :select-top
                                                                                                              (vec (cons [limit]
                                                                                                                         select)))
                                                                                                       :select)
                                                                                               :limit)
                                                                       :else           ["sqlserver top issue"]))
              (and (= sql-dialect :oracle) (get hm :limit))    (let [limit (get hm :limit)]
                                                                 (dissoc (assoc hm :fetch limit) :limit))
              :else                                            hm)
              ]
    (try (-> fixed-honey-map
             (honey/format (merge {:pretty true :inline true} (if (nil? sql-dialect) {} {:dialect sql-dialect}))))
         (catch Exception e
           (do (ut/pp [:honey-format-err (str e) :input honey-map])
               (insert-error-row! :honey-format honey-map e)
               (to-sql {:union-all [{:select [[(str (.getMessage e)) :database_says]]}
                                    {:select [["(from HoneySQL formatter)" :database_says]]}]}))))))

;; (ut/pp
;;  ;(to-sql {:select [:*] :from [:tests] :limit 123} )

;;      (honey/format {:select [:*] :from [:tests] :limit 123} {:pretty true :inline true :dialect :sqlserver})
;;  )

(defn fetch-all-tables
  [db-conn & [quick?]]
  (let [;table-names-query (to-sql {:select [:name]
        table-names (if quick?
                      [:fields :attributes :tests]
                      [:combos :found_fields :attributes :tests :fields :combo_rows :connections]) ;(map
        fetch-table (fn [table-name] (sql-query db-conn (to-sql {:select [:*] :from [table-name]})))
        table-data  (doall (walk/keywordize-keys (into {} (map #(hash-map % (fetch-table %)) table-names))))]
    table-data))

(defn sql-exec-risk
  [db-spec query & [extra]]
  (jdbc/with-db-transaction [t-con db-spec {:isolation :repeatable-read}] ;; {:isolation
                                                                          ;; :read-uncommitted}
    (jdbc/db-do-commands t-con query)))

(defn safe-sql-exec-helper
  [db query]
  (try (sql-exec-risk db query)
       (catch Exception e
         (println "Error during SQL execution, retrying..." (.getMessage e) " query: " (hash query) query)
         (Thread/sleep (rand-nth [200 100 600 345 684 900 400 200 400 300 599 1400 300]))
         (safe-sql-exec-helper db query))))

(defn safe-sql-exec [db query] (safe-sql-exec-helper db query))

(defn insert-all-tables-SAFE?
  [fetched-data tbl] ;; system-db is inherited from ns global, gross, I know. Deal.
  (doseq [[table-name records] fetched-data]
    (let [delete-query (to-sql {:delete-from [table-name] :where [:= :table_name tbl]})]
      (safe-sql-exec system-db delete-query) ;; old recos
      (doseq [recs (partition-all 100 records)] ;;; insert in chunks of 100
        (let [columns      (keys (first recs))
              values       (vec (for [r recs] (vals r)))
              insert-query (to-sql {:insert-into [table-name] :values values :columns columns})]
          (safe-sql-exec system-db insert-query))))))

(defn insert-all-tables
  [fetched-data tbl] ;; system-db is inherited from ns global, gross, I know. Deal.
  (doseq [[table-name records] fetched-data]
    (let [delete-query (to-sql {:delete-from [table-name] :where [:= :table_name tbl]})]
      (sql-exec system-db delete-query) ;; old recos
      (doseq [recs (vec (partition-all 100 records))] ;;; insert in chunks of 100
        (let [columns      (keys (first recs))
              values       (vec (for [r recs] (vals r)))
              insert-query (to-sql {:insert-into [table-name] :values values :columns columns})]
          (sql-exec system-db insert-query))))))
