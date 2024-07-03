(ns rvbbit-backend.sql
  (:require
   [clj-time.coerce     :as tc]
   [clj-time.core       :as time]
   [clj-time.jdbc]
   [clojure.edn         :as edn]
   [clojure.java.jdbc   :as jdbc]
   [clojure.string      :as cstr]
   [clojure.walk        :as walk]
   [hikari-cp.core      :as hik]
   [honey.sql           :as honey]
   [rvbbit-backend.util :as ut]
   [rvbbit-backend.queue-party :as qp]
   [taskpool.taskpool   :as tp]))






(def query-history (atom {}))

(defn pool-create
  [jdbc-body name]
  (delay (let [base  {:idle-timeout 600000 :max-lifetime 1800000 :pool-name name}
               bbase (merge base jdbc-body)]
           (ut/pp [:creating-conn-pool name])
           (hik/make-datasource bbase))))








(def system-db
  {:datasource
     @(pool-create
        {:jdbc-url
           ;;"jdbc:sqlite:file:./db/system.db?cache=shared&journal_mode=WAL&mode=memory&transaction_mode=IMMEDIATE&busy_timeout=5000&locking_mode=EXCLUSIVE"
           ;"jdbc:sqlite:file::memory:?cache=shared&journal_mode=WAL&transaction_mode=IMMEDIATE&busy_timeout=5000&locking_mode=NORMAL"
           ;"jdbc:sqlite:file::memory:?cache=shared&journal_mode=WAL&busy_timeout=5000&locking_mode=NORMAL&cache_size=-20000"
           "jdbc:sqlite:file:./db/system.db?cache=shared&journal_mode=WAL&busy_timeout=5000&locking_mode=NORMAL&mmap_size=268435456" 
         :idle-timeout      600000
         :maximum-pool-size 20
         :max-lifetime      1800000
         :cache             "shared"}
        "system-db")})

(def flows-db
  {:datasource
     @(pool-create
        {:jdbc-url
           ;"jdbc:sqlite:file:./db/flow.db?cache=shared&journal_mode=WAL&auto_vacuum=FULL"
           ;;&transaction_mode=IMMEDIATE&journal_mode=WAL"
           "jdbc:sqlite:file:./db/flow.db?cache=shared&journal_mode=WAL&busy_timeout=5000&locking_mode=NORMAL&mmap_size=268435456" ;
         :idle-timeout 600000
         :max-lifetime 1800000
         :auto_vacuum  "FULL"
         :cache        "shared"}
        "flows-db")})

(def ghost-db
  {:datasource @(pool-create {:jdbc-url     "jdbc:sqlite:file:./db/ghost.db?cache=shared&journal_mode=WAL&mode=memory" ;&transaction_mode=IMMEDIATE&journal_mode=WAL"
                              :idle-timeout 600000
                              :max-lifetime 1800000
                              :cache        "shared"}
                             "ghost-db")})


















(defn insert-error-row-OLD!
  [error-db-conn query error] ;; warning uses shit outside of what it is passed!!!
  (jdbc/with-db-connection
    [sdb system-db] ;; ?
    (let [ins (-> {:insert-into [:errors] :values [{:db_conn (str error-db-conn) :sql_stmt (str query) :error_str (str error)}]}
                  (honey/format {:pretty false :inline true}))]
      (try (jdbc/db-do-commands sdb ins) (catch Exception e (ut/pp {:error-inserting-error e :query query}))))))

(defonce errors (atom [])) ;; (ut/thaw-atom [] "./data/atoms/sql-errors.edn")) ;; no need to persist for now...

(defn insert-error-row!
  [error-db-conn query error] ;; warning uses shit outside of what it is passed!!!
  (swap! errors conj [(str error) (str error-db-conn) query])
  (ut/pp {:sql-error! error :error-db-conn error-db-conn :query (try (subs (str query) 0 1000) (catch Throwable _ (str query)))}))





(defn asort
  [m order]
  (let [order-map (apply hash-map (interleave order (range)))]
    (conj (sorted-map-by #(compare (order-map %1) (order-map %2))) ; empty map with the desired
          (select-keys m order))))

(defonce map-orders (atom {}))

(defn wrap-maps
  [query arrays]
  (let [headers (first arrays)
        mm      (vec (doall (for [r (rest arrays)]
                              (asort (into {} ;(sorted-map)
                                           (for [i (map vector headers r)] {(first i) (last i)}))
                                     headers))))]
    (swap! map-orders assoc query headers)
    mm))

(defn sql-query
  [db-spec query & extra]
  (jdbc/with-db-connection ;; for connection hygiene - atomic query
    [t-con db-spec]
    (ut/ppln [query db-spec])
    (try (wrap-maps query (jdbc/query t-con query {:identifiers keyword :as-arrays? true :result-set-fn doall}))
         (catch Exception e
           (do (ut/pp {:sql-query-fn-error e :query query :extra (when extra extra)})
               (insert-error-row! db-spec query e)
               [;{:query_error (str (.getType e))}
                {:query_error (str (.getMessage e))} {:query_error "(from database connection)"}])))))

(defn sql-query-meta
  [db-spec query]
  (jdbc/with-db-connection ;; for connection hygiene - atomic query
    [t-con db-spec]
    (jdbc/metadata-result (.getMetaData (jdbc/query t-con query {:identifiers keyword})))));)

(defn sql-query-one
  [db-spec query & extra]
  (jdbc/with-db-connection ;; for connection hygiene - atomic query
    [t-con db-spec]
    (ut/ppln [query db-spec])
    (try (first (vals (first (jdbc/query t-con query {:identifiers keyword}))))
         (catch Exception e
           (do (ut/pp {:sql-query-one-fn-error e :query query :extra (when extra extra)}) (insert-error-row! db-spec query e))))))



(defn sql-exec2
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


(defn sql-exec
  [db-spec query & [extra]]
  ;(enqueue-task-sql db-spec
  (qp/serial-slot-queue :sql-serial :sql ;;(str db-spec) 
                    (fn []
                      (jdbc/with-db-connection [t-con db-spec]
                                               (try (jdbc/db-do-commands t-con query)
                                                    (catch Exception e
                                                      (do (ut/pp (merge {:sql-exec-fn-error e
                                                                         :db-spec           db-spec
                                                                         :query             (try (subs (str query) 0 1000)
                                                                                                 (catch Throwable _ (str query)))}
                                                                        (if extra {:ex extra} {})))
                                                          (insert-error-row! db-spec query e))))))))



(defn sql-exec-no-t
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
  [honey-map & [key-vector system-db]]
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
        fixed-honey-map
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
                :else                                            hm)]
    (try (-> fixed-honey-map
             (honey/format (merge {:pretty true :inline true} (if (nil? sql-dialect) {} {:dialect sql-dialect}))))
         (catch Exception e
           (do (ut/pp [:honey-format-err (str e) :input honey-map])
               (insert-error-row! :honey-format honey-map e)
               (to-sql {:union-all [{:select [[(str (.getMessage e)) :query_error]]}
                                    {:select [["(from HoneySQL formatter)" :query_error]]}]}))))))

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
