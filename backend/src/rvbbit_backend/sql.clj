(ns rvbbit-backend.sql
  (:require
   [hikari-cp.core :as hik]
   [clojure.java.jdbc :as jdbc]
   [clojure.edn :as edn]
   [taskpool.taskpool :as tp]
   [honey.sql :as honey]
   [clojure.string :as cstr]
   [clojure.walk :as walk]
   [clj-time.core :as time]
   [clj-time.coerce :as tc]
   [rvbbit-backend.util :as ut]
   [rvbbit-backend.instrument :as instrument]
   [clj-time.jdbc]))  ;; enables joda time returns

;; https://github.com/tomekw/hikari-cp

;; (def datasource-options {:auto-commit        true
;;                          :read-only          false
;;                          :connection-timeout 30000
;;                          :validation-timeout 5000
;;                          :idle-timeout       600000
;;                          :max-lifetime       1800000
;;                          :minimum-idle       10
;;                          :maximum-pool-size  10
;;                          :pool-name          "db-pool"
;;                          :adapter            "postgresql"
;;                          :username           "username"
;;                          :password           "password"
;;                          :database-name      "database"
;;                          :server-name        "localhost"
;;                          :port-number        5432
;;                          :register-mbeans    false})

;; (defonce datasources
;;   (delay (hik/make-datasource datasource-options)))

;; (def system-db2 {:classname   "org.sqlite.JDBC"
;;                  :subprotocol "sqlite"
;;                  :busy_timeout 20000
;;                  :cache "shared"
;;                  :subname "./db/system.db"}) ;; ?busy_timeout=20000&cache=shared


(def query-history (atom {}))

(defn pool-create [jdbc-body name]
  (delay (let [base {;:jdbc-url "jdbc:sqlite:db/system.db"
                     ;:minimum-idle 10 ;; min iodle connections to keep open?
                     ;:cache "shared"
                     :idle-timeout       600000
                     :max-lifetime       1800000
                     :pool-name name ;"system-db-pool"
                     :metric-registry instrument/metric-registry
                     ;:maximum-pool-size 10
                     }
               bbase (merge base jdbc-body)]
           (ut/pp [:creating-conn-pool name])
           (hik/make-datasource bbase))))

;; (defonce system-db0 (delay (hik/make-datasource 
;;                             {:jdbc-url "jdbc:sqlite:db/system.db"
;;                              :minimum-idle 10
;;                              :cache "shared"
;;                              :pool-name "system-db-pool"
;;                              :metric-registry instrument/metric-registry
;;                              :maximum-pool-size 15})))
;; (defonce system-db {:datasource @system-db0})




;; (def system-db {:datasource @(pool-create {:jdbc-url "jdbc:sqlite:file:systemdb?mode=memory&cache=shared&transaction_mode=IMMEDIATE&journal_mode=WAL" ; "jdbc:sqlite:db/system.db"
;;                                            :idle-timeout        600000  ;;;; LAST KNOW GOOD SQLITE 10/25/23  - fastest?
;;                                            :max-lifetime       1800000
;;                                            ;:minimum-idle 12
;;                                            ;:maximum-pool-size 60
;;                                            :transaction_mode "IMMEDIATE"
;;                                            :journal_mode "WAL"
;;                                            :cache "shared"} "system-db")})

;; (def system-db {:datasource @(pool-create {:jdbc-url "jdbc:sqlite:file:systemdb?mode=memory&cache=shared&journal_mode=WAL&auto_vacuum=FULL" ;&transaction_mode=IMMEDIATE&journal_mode=WAL" ; "jdbc:sqlite:db/system.db"
;;                                            :idle-timeout        600000  ;;;; LAST KNOW GOOD SQLITE 10/25/23 
;;                                            :max-lifetime       1800000
;;                                            :memory true
;;                                            :mode "memory"
;;                                            :auto_vacuum "FULL"
;;   ;;"real"                                ;:minimum-idle 12
;;                                           ;;;; :maximum-pool-size 2
;;                                            ;:transaction_mode "IMMEDIATE"
;;                                            ;:journal_mode "WAL"
;;                                            :cache "shared"
;;                                            } "system-db")})


(def system-db {:datasource @(pool-create {:jdbc-url "jdbc:sqlite:file:./db/system.db?cache=shared&journal_mode=WAL&auto_vacuum=FULL" ;&transaction_mode=IMMEDIATE&journal_mode=WAL" &mode=memory ; "jdbc:sqlite:db/system.db"
                                           :idle-timeout        600000  ;;;; LAST KNOW GOOD SQLITE 10/25/23 
                                           :max-lifetime       1800000
                                           :auto_vacuum        "FULL"
                                           :cache "shared"} "system-db")})

(def flows-db {:datasource @(pool-create {:jdbc-url "jdbc:sqlite:file:./db/flow.db?cache=shared&journal_mode=WAL&auto_vacuum=FULL" ;&transaction_mode=IMMEDIATE&journal_mode=WAL" ; "jdbc:sqlite:db/system.db"
                                          :idle-timeout        600000  ;;;; LAST KNOW GOOD SQLITE 10/25/23 
                                          :max-lifetime       1800000
                                          :auto_vacuum        "FULL"
                                          :cache "shared"} "flows-db")})


;; (def system-db {:datasource @(pool-create {:jdbc-url "jdbc:h2:mem:systemdb;DATABASE_TO_UPPER=FALSE" ;"jdbc:sqlite:db/system.db"
;;                                            ;:cache "shared"
;;                                            } "system-db")})

;; (def system-db {:datasource @(pool-create {:jdbc-url "jdbc:duckdb:db/duck.system" ; "jdbc:sqlite:db/system.db"
;;                                            :idle-timeout        600000   
;;                                            :max-lifetime       1800000
;;                                            :minimum-idle 12
;;                                            ;:maximum-pool-size 60
;;                                            ;:maximum-pool-size 1
;;                                            ;:transaction_mode "IMMEDIATE"
;;                                            ;:journal_mode "WAL"
;;                                            ;:cache "shared"
;;                                            } "system-db-pool")})


;; (def system-db {:datasource @(pool-create {:jdbc-url "jdbc:clickhouse://10.174.1.150:8123/rabbit_system"
;;                                            :username "rabbit_system"
;;                                            :password "notofox"
;;                                            :driver-class-name "ru.yandex.clickhouse.ClickHouseDriver"
;;                                            :connection-timeout 5000
;;                                            ;:maximum-pool-size 20
;;                                            :max-lifetime 300000} "system-db-pool")})

;; (def system-db {:datasource @(pool-create {:jdbc-url "jdbc:sqlite:file:systemdb?mode=memory&cache=shared&transaction_mode=IMMEDIATE&journal_mode=WAL" ; "jdbc:sqlite:db/system.db"
;;                                            :idle-timeout        600000
;;                                            :max-lifetime       1800000
;;                                            :transaction_mode "IMMEDIATE"
;;                                            :journal_mode "WAL"
;;                                            :cache "shared"} "system-db-pool")})




;; (def system-db {:datasource @(pool-create {:jdbc-url "jdbc:mysql://root:notofox@10.174.1.248:3306/rabbit"} "system-db-pool")})


;; (def system-db {:datasource @(pool-create {:auto-commit        true
;;                                            ;:read-only          true
;;                                            :connection-timeout 30000
;;                                            ;:validation-timeout 5000
;;                                            ;:idle-timeout       600000
;;                                            ;:max-lifetime       1800000
;;                                            ;:minimum-idle       10
;;                                            ;:maximum-pool-size  10
;;                                            :jdbc-url           "jdbc:postgresql://ryanr:notofox@10.174.1.248:5432" ;; needs both? wtf, worked
;;                                            ;:jdbc-url           "jdbc:postgresql://postgres:notofox@localhost:5432" ;; needs both? wtf, worked
;;                                            :adapter            "postgresql"
;;                                            :username           "ryanr"
;;                                            ;:username           "postgres"
;;                                            :password           "notofox"
;;                                            :database-name      "rabbit" ;;"postgres" ;; where citus is installed apparently
;;                                            :server-name        "10.174.1.248"
;;                                            ;:server-name        "localhost"
;;                                            :port-number        5432
;;                                            :register-mbeans    false}
;;                                           "system-db")})



;; (def system-db-old "jdbc:postgresql://postgres:postgrespw@localhost:49153/postgres")

;; (declare sql-exec)


(defn insert-error-row! [error-db-conn query error] ;; warning uses shit outside of what it is passed!!!
  (jdbc/with-db-connection [sdb system-db] ;; ?
    (let [ins (-> {:insert-into [:errors]
                   ;:columns [:db_conn :sql_stmt :error_str]
                   ;:values [(str error-db-conn) (str query) (str error)]
                   :values [{:db_conn (str error-db-conn)
                             :sql_stmt (str query)
                             :error_str (str error)}]}

                  (honey/format {:pretty false :inline true}))]
      ;(ut/pp ins)
      (try
        (jdbc/db-do-commands sdb ins)
        ;(sql-exec sdb ins)
        (catch Exception e
          (ut/pp {:error-inserting-error e :query query}))))))

;(defn insert-error-row! [error-db-conn query error] (ut/pp [:insert-error-row! query error]))


;; (defn insert-log-row! [log caller] ;; warning uses shit outside of what it is passed!!!
;;   (jdbc/with-db-connection [sdb system-log-db] ;; ?
;;     (let [ins (-> {:insert-into [:logs]
;;                    :values [{:ts (/ (.getTime (java.util.Date.)) 1000)
;;                              :hts (str (-> (java.text.SimpleDateFormat. "yyyy-MM-dd HH:mm:ss")
;;                                            (.format (java.util.Date.))))
;;                              :caller (str caller)
;;                              :log_str (str log)}]}
;;                   (honey/format {:pretty false :inline true}))]
;;       ;(ut/pp ins)
;;       (try
;;         (jdbc/db-do-commands sdb ins)
;;         (catch Exception e
;;           (ut/pp {:error-inserting-error e :log-table log}))))))


(defn asort [m order]
  (let [order-map (apply hash-map (interleave order (range)))]
    (conj
     (sorted-map-by #(compare (order-map %1) (order-map %2))) ; empty map with the desired ordering
     (select-keys m order))))

(defonce map-orders (atom {}))

(defn wrap-maps [query arrays]
  (let [headers (first arrays)
        mm (vec (doall (for [r (rest arrays)]
                         (asort
                          (into {} ;(sorted-map)
                                (for [i (map vector headers r)]
                                  {(first i) (last i)}))
                          headers))))]
              ;;;  (ut/pp mm)
    (swap! map-orders assoc query headers)
    mm))

(defn sql-query [db-spec query & extra]
  (jdbc/with-db-connection ;; for connection hygiene - atomic query
    [t-con db-spec]
    (ut/ppln [query db-spec])
    (try
      ;(vec 
      (wrap-maps query
                 (jdbc/query t-con query {:identifiers keyword
                                          :as-arrays? true
                                    ;:row-fn #(zipmap (first %) (rest %))
                                          :result-set-fn doall}))
     ;  )
      (catch Exception e
        (do (ut/pp {:sql-query-fn-error e :query query :extra (when extra extra)})
            (insert-error-row! db-spec query e)
            [;{:query_error (str (.getType e))}
             {:query_error (str (.getMessage e))}
             {:query_error "(from database connection)"}])))))

(defn sql-query-meta [db-spec query]
  (jdbc/with-db-connection ;; for connection hygiene - atomic query
    [t-con db-spec]
    ;(ut/ppln [:meta query db-spec])
    ;(try
      ;(vec 
    (jdbc/metadata-result
     (.getMetaData
      (jdbc/query t-con query {:identifiers keyword
                               ;:as-arrays? :cols-as-is ;true
                               ;:row-fn map
                               ;:result-set-fn doall
                               })))
      ; )
    ;  (catch Exception e
    ;    (do (ut/pp {:sql-query-metadata-fn-error e :query query})
    ;        (insert-error-row! db-spec query e)))
));)

(defn sql-query-one [db-spec query & extra]
  (jdbc/with-db-connection ;; for connection hygiene - atomic query
    [t-con db-spec]
    (ut/ppln [query db-spec])
    (try
      (first (vals (first (jdbc/query t-con query {:identifiers keyword
                                                   ;:as-arrays? :cols-as-is ;true
                                                   ;:row-fn map
                                                   ;:result-set-fn doall
                                                   }))))
      (catch Exception e
        (do (ut/pp {:sql-query-one-fn-error e :query query :extra (when extra extra)})
            (insert-error-row! db-spec query e))))))

;; (defn execute-sql! [db-spec sql-string]
;;   (let [conn (jdbc/get-connection db-spec)]
;;     (try
;;       (let [stmt (.createStatement conn)]
;;         (try
;;           (.execute stmt sql-string)
;;           (finally
;;             (.close stmt))))
;;       (finally
;;         (.close conn)))))


(defn sql-exec2 [db-spec query & [extra]]
  (jdbc/with-db-transaction
    [t-con db-spec {:isolation :read-uncommitted}]
    (try
      (jdbc/db-do-commands t-con query)
      (catch Exception e
        (do (ut/pp (merge {:sql-exec-fn-error e :db-spec db-spec :query query}
                          (if extra {:ex extra} {})))
            (insert-error-row! db-spec query e))))))


(def task-queue-sql (java.util.concurrent.LinkedBlockingQueue.))
(def running-sql (atom true))
(def worker-sql (atom nil)) ; Holds the future of the worker thread

(defn enqueue-task-sql [task]
  (.put task-queue-sql task))

(defn worker-loop-sql []
  (loop []
    (when @running-sql
      (let [task (.take task-queue-sql)]
        (task))))
  (recur))

(defn start-worker-sql []
  ;(Thread/sleep 3000)
  (ut/pp [:starting-sync-worker-thread :sql-sync])
  (reset! running-sql true)
  (reset! worker-sql (future (worker-loop-sql))))

(defn stop-worker-sql []
  (reset! running-sql false)
  (when-let [w @worker-sql]
    (future-cancel w)
    (while (not (.isDone w)) ; Ensure the future is cancelled before proceeding
      (Thread/sleep 60))))

(defn recycle-worker-sql []
  ;(ut/pp [:REBOOTING-WORKER-THREAD3!!])
  ;(System/gc) ;;; hehe, fuck it
  (stop-worker-sql)
  (start-worker-sql))






(defn sql-exec [db-spec query & [extra]]
  (enqueue-task-sql
   (fn [] ;; test
     (jdbc/with-db-transaction
       [t-con db-spec {:isolation :read-uncommitted}]
       (try
         (jdbc/db-do-commands t-con query)
         (catch Exception e
           (do (ut/pp (merge {:sql-exec-fn-error e :db-spec db-spec
                              :query (try (subs (str query) 0 1000) (catch Throwable _ (str query)))}
                             (if extra {:ex extra} {})))
               (insert-error-row! db-spec query e))))))))

;; (defn sql-exec-no-t [db-spec query & [extra]]
;;   (jdbc/with-db-connection
;;     [t-con db-spec]
;;     (try
;;       (jdbc/db-do-commands t-con query)
;;       (catch Exception e
;;         (do (ut/pp (merge {:sql-exec-fn-error e :db-spec db-spec :query query}
;;                           (if extra {:ex extra} {})))
;;             (insert-error-row! db-spec query e))))))

(defn sql-exec-no-t [db-spec query & [extra]]
  (jdbc/with-db-connection [conn db-spec]
    (try
      (.execute conn query)  ; Directly executing the query
      (catch Exception e
        (do
          ;; Replace `ut/pp` and `insert-error-row!` with your actual error handling logic
          (ut/pp (merge {:sql-exec-fn-error e :db-spec db-spec :query query}
                        (if extra {:ex extra} {})))
          (insert-error-row! db-spec query e))))))

(defn sql-exec-33333 [db-spec query & [extra]]
  (let [query (if (vector? query) (first query) query)
        datasource (if (instance? com.zaxxer.hikari.HikariDataSource (:datasource db-spec))
                     (:datasource db-spec)
                     nil)
        conn (if datasource (.getConnection datasource) nil)]
    (try
      (let [stmt (.createStatement conn)]
        (.setAutoCommit conn true)
        (try
          ;(do 
          (.execute stmt query)
          ;(.commit conn))
          (finally
            (.close stmt))))
      (catch Exception e
        (do (ut/pp (merge {:sql-exec-fn-error e :db-spec db-spec :query query}
                          (if extra {:ex extra} {})))
            (insert-error-row! db-spec query e)))
      (finally
        (when conn
          (.close conn))))))










;; (defn sql-exec2 [db-spec query & [extra]]
;;   (jdbc/with-db-connection ;; for connection hygiene - atomic query
;;     [t-con db-spec]
;;     (try
;;       (jdbc/db-do-commands t-con query)
;;       (catch Exception e
;;         (do (ut/pp (merge {:sql-exec-fn-error e :query query}
;;                           (if extra {:ex extra} {})))
;;             (insert-error-row! db-spec query e))))))


(defn lookup-value [key-vector lookup-key system-db]
  (let [db-type (nth key-vector 0)
        connection_id (nth key-vector 1) ;; key to the connections table 
        table-type (nth key-vector 2)
        db_schema (nth key-vector 3)
        db_catalog (nth key-vector 4)
        table_name (nth key-vector 5)
        field_name (nth key-vector 6)
        field_type (nth key-vector 7)
        data_type (nth key-vector 8)
        context (cstr/split (str lookup-key) #"/")
        src-table (read-string (first context))
        src-field (last context)
        lookup-honey (cond (= src-table :tests)
                           {:select [[[:raw "coalesce(test_val_string, test_val_integer, test_val_float)"] :v]]
                            :from [:tests]
                            :where [:and [:= :test-name src-field]
                                    [:= :connection-id connection_id]
                                    [:= :table-name table_name]
                                    [:= :db-schema db_schema]
                                    [:= :db-catalog db_catalog]
                                    [:= :field-name field_name]]
                            :limit 1}

                           (= src-table :attributes)
                           {:select [[:attribute_value :v]]
                            :from [:attributes]
                            :where [:and [:= :attribute-name src-field]
                                    [:= :connection-id connection_id]
                                    [:= :table-name table_name]
                                    [:= :db-schema db_schema]
                                    [:= :db-catalog db_catalog]
                                    [:= :field-name field_name]]
                            :limit 1}

                           (= src-table :sample)
                           {:select [[:key_hash :v]]
                            :from [:tests]
                            :where [:and [:= :test-name src-field]
                                    [:= :connection-id connection_id]
                                    [:= :table-name table_name]
                                    [:= :db-schema db_schema]
                                    [:= :db-catalog db_catalog]
                                    [:= :field-name field_name]]
                            :limit 1})
        lookup-value (sql-query-one system-db (-> lookup-honey (honey/format {:pretty false :inline true})))
        val (cond (= src-table :attributes) (= 1 lookup-value)
                  (= src-table :sample) (keyword lookup-value)
                  :else lookup-value)]
    ;(ut/pp {lookup-key val})
    val))

(defn to-sql [honey-map & [key-vector system-db]]
  (let [sql-dialect (when key-vector
                      (cond (cstr/includes? (cstr/lower-case (first key-vector)) "microsoft") :sqlserver
                            (cstr/includes? (cstr/lower-case (first key-vector)) "mysql") :mysql
                            (cstr/includes? (cstr/lower-case (first key-vector)) "oracle") :oracle
                            :else nil))
        key-farm (ut/deep-flatten honey-map)
        ;value-keys? (true? (some #(or (cstr/starts-with? (str %) ":test-value/") 
        ;                       (cstr/starts-with? (str %) ":attribute-value/")) key-farm))
        value-keys (filter #(or (cstr/starts-with? (str %) ":tests/")
                                (cstr/starts-with? (str %) ":sample/")
                                (cstr/starts-with? (str %) ":attributes/")) key-farm)
        value-keys? #_{:clj-kondo/ignore [:not-empty?]}
        (and (not (empty? value-keys))
             (not (nil? key-vector))
             (not (nil? system-db)))
        value-mapping (if value-keys?
                        (into {} (for [v value-keys]
                                   {v (lookup-value key-vector v system-db)})) {})
        hm (walk/postwalk-replace value-mapping honey-map) ; (walk/postwalk {} honey-map)
        fixed-honey-map (cond (and (= sql-dialect :sqlserver) (get hm :limit))
                          ;; I wish HoneySQL translated :limit to :select-top for :sqlserver dialect, or to :fetch for :oracle but alas...
                              (let [limit (get hm :limit)
                                    select-distinct (get hm :select-distinct)
                                    select (get hm :select)]
                                (cond select-distinct
                                      (dissoc (dissoc (assoc hm
                                                             :select-distinct-top (vec (cons [limit] select-distinct)))
                                                      :select-distinct) :limit)
                                      select
                                      (dissoc (dissoc (assoc hm
                                                             :select-top (vec (cons [limit] select)))
                                                      :select) :limit)
                                      :else ["sqlserver top issue"]))

                              (and (= sql-dialect :oracle) (get hm :limit))

                              (let [limit (get hm :limit)]
                                (dissoc (assoc hm :fetch limit) :limit))

                              :else hm)]
    ;(when (cstr/includes? (str fixed-honey-map) "derived") (ut/pp fixed-honey-map))
    (try (-> fixed-honey-map (honey/format (merge {:pretty true :inline true}
                                                  (if (nil? sql-dialect) {}
                                                      {:dialect sql-dialect}))))
         (catch Exception e
           (do (ut/pp [:honey-format-err (str e) :input honey-map])
               (insert-error-row! :honey-format honey-map e)
               ;[{:sql-parse-error (str e)}]
               ;;(str "select '" "' as error, 'sql parse error' as type ")
               (to-sql {:union-all
                        [{:select [[(str (.getMessage e)) :query_error]]}
                         {:select [["(from HoneySQL formatter)" :query_error]]}]}))))))

(defn fetch-all-tables [db-conn & [quick?]]
  (let [;table-names-query (to-sql {:select [:name]
        ;                           :from [:sqlite_master]
        ;                           :where [:= :type "table"]})
        table-names (if quick?
                      [:fields :attributes :tests]
                      [:combos :found_fields :attributes :tests :fields :combo_rows :connections]) ;(map :name (sql-query db-conn table-names-query))
        fetch-table (fn [table-name]
                      (sql-query db-conn (to-sql {:select [:*] :from [table-name]})))
        table-data (doall (walk/keywordize-keys (into {} (map #(hash-map % (fetch-table %)) table-names))))]
    table-data))

(defn sql-exec-risk [db-spec query & [extra]]
  (jdbc/with-db-transaction
    [t-con db-spec {:isolation :repeatable-read}] ;; {:isolation :read-uncommitted}
    (jdbc/db-do-commands t-con query)))

(defn safe-sql-exec-helper [db query]
  (try
    (sql-exec-risk db query)
    (catch Exception e
      (println "Error during SQL execution, retrying..." (.getMessage e) " query: " (hash query) query)
      (Thread/sleep (rand-nth [200 100 600 345 684 900 400 200 400 300 599 1400 300]))
      (safe-sql-exec-helper db query))))

(defn safe-sql-exec [db query]
  (safe-sql-exec-helper db query))

(defn insert-all-tables-SAFE? [fetched-data tbl] ;; system-db is inherited from ns global, gross, I know. Deal.
  (doseq [[table-name records] fetched-data]
    (let [delete-query (to-sql {:delete-from [table-name]
                                :where [:= :table_name tbl]})]
      (safe-sql-exec system-db delete-query) ;; old recos
      (doseq [recs (partition-all 100 records)] ;;; insert in chunks of 100 
        (let [columns (keys (first recs))
              values (vec (for [r recs] (vals r)))
              insert-query (to-sql {:insert-into [table-name]
                                    :values values
                                    :columns columns})]
          (safe-sql-exec system-db insert-query))))))

(defn insert-all-tables [fetched-data tbl] ;; system-db is inherited from ns global, gross, I know. Deal.
  ;(ut/pp [:__inserting-tables (keys fetched-data)])
  (doseq [[table-name records] fetched-data]
    (let [delete-query (to-sql {:delete-from [table-name]
                                :where [:= :table_name tbl]})]
      (sql-exec system-db delete-query) ;; old recos
      ;(ut/pp [:___deleting-from-table table-name tbl])
      (doseq [recs (vec (partition-all 100 records))] ;;; insert in chunks of 100 
        (let [columns (keys (first recs))
              values (vec (for [r recs] (vals r)))
              insert-query (to-sql {:insert-into [table-name]
                                    :values values
                                    :columns columns})]
          ;(ut/pp [:___inserting-into-table table-name tbl])
          (sql-exec system-db insert-query))))))
