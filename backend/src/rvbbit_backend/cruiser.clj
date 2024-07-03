(ns rvbbit-backend.cruiser
  (:require ;[datalevin.core :as d]
    [clojure.java.jdbc         :as jdbc]
    [clojure.core.async        :as async]
    [clojure.edn               :as edn]
    [hikari-cp.core            :as hik]
    [rvbbit-backend.embeddings :as em]
    [taskpool.taskpool         :as tp]
    [honey.sql                 :as honey]
    [clojure.string            :as cstr]
    [clojure.walk              :as walk]
    [com.climate.claypoole     :as cp]
    [rvbbit-backend.clickhouse-ddl :as clickhouse-ddl]
    [rvbbit-backend.ddl        :as sqlite-ddl] ;; needed for hardcoded rowset-sql-query usage
    [rvbbit-backend.util       :as ut]
    [rvbbit-backend.surveyor   :as surveyor]
    [rvbbit-backend.sql        :as    sql
                               :refer [sql-exec sql-exec-no-t sql-exec2 sql-query sql-query-meta sql-query-one system-db
                                       insert-error-row! to-sql pool-create]]
    [clojure.math.combinatorics :as combo]
    [clj-time.jdbc]) ;; enables joda time returns
  (:import
    [java.util Date UUID]))




(defn general-data-type-infer
  [field-type]
  (let [ft (cstr/lower-case field-type)]
    (cond (cstr/includes? ft "char")    "string"
          (cstr/includes? ft "string")  "string"
          (cstr/includes? ft "lob")     "string"
          (cstr/includes? ft "text")    "string"
          (cstr/includes? ft "time")    "datetime"
          (cstr/includes? ft "date")    "date"
          (cstr/includes? ft "bool")    "boolean"
          (cstr/includes? ft "int")     "integer"
          (cstr/includes? ft "real")    "float"
          (cstr/includes? ft "float")   "float"
          (cstr/includes? ft "decimal") "float"
          (cstr/includes? ft "num")     "integer"
          :else                         "unknown")))

(defn general-data-type-value
  [v]
  (cond (string? v)  "string"
        (integer? v) "integer"
        (float? v)   "float"
        (boolean? v) "boolean"
        :else        "unknown"))
















(def filter-db
  {:datasource @(pool-create {:jdbc-url    "jdbc:sqlite:file:./db/filter.db?auto_vacuum=FULL" ; "jdbc:sqlite:file:filterdb?mode=memory&auto_vacuum=FULL"
                              :auto_vacuum "FULL"
                              :cache       "shared"}
                             "filter-db")})

(defn rowset-sql-query ;; [rowset query & columns-vec]
  "takes a 'rowset' (vector of uniform maps) or a vector with a vector of column names
   - inserts it into an in memory SQL db, executes a SQL query on it 
   (via a honey-sql map) and returns it"
  [rowset query & columns-vec]
  (when (ut/ne? rowset)
    (let [rando-name                  (str "rowset" (rand-int 12345567)) ;; prevent
          rowset-type                 (cond (and (map? (first rowset)) (vector? rowset))       :rowset
                                            (and (not (map? (first rowset))) (vector? rowset)) :vectors)
          columns-vec-arg             (first columns-vec)
          columns-vec-arg-underscores (edn/read-string (cstr/replace (str columns-vec-arg) #"-" "_")) ;; ugly,
          db-conn-old                 {:classname   "org.sqlite.JDBC"
                                       :subprotocol "sqlite"
                                       :auto_vacuum "FULL"
                                       :subname     "::memory::?auto_vacuum=FULL" ;; "file:filterdb?mode=memory&auto_vacuum=FULL"
                                      }
          db-conn                     filter-db ;; ^^^
          rowset-fixed                (if (= rowset-type :vectors) (vec (for [r rowset] (zipmap columns-vec-arg r))) rowset)
          asort                       (fn [m order]
                                        (let [order-map (apply hash-map (interleave order (range)))]
                                          (conj (sorted-map-by #(compare (order-map %1) (order-map %2))) ; empty
                                                (select-keys m order))))
          columns                     (keys (first rowset-fixed))
          values                      (vec (for [r rowset-fixed] (vals r)))
          ddl-str                     (sqlite-ddl/create-attribute-sample rando-name rowset-fixed) ;; has
          insert-sql                  (to-sql {:insert-into [(keyword rando-name)] :columns columns :values values})
          query-sql                   (to-sql (assoc query :from [(keyword rando-name)]))]
      (sql-exec2 db-conn ddl-str)
      ;(sql-exec db-conn ddl-str)
      (sql-exec2 db-conn insert-sql)
      ;(sql-exec db-conn insert-sql)
      (let [results (sql-query db-conn query-sql)]
        (sql-exec2 db-conn (str "drop table if exists " rando-name ";"))
        ;(sql-exec db-conn (str "drop table if exists " rando-name ";"))
        (if (= rowset-type :vectors) ;; give it back in the format it was given
          (vec (for [r results]
                 (vec ;; need to return the vec in the same field order as we got it, not just
                   (vals (asort r columns-vec-arg-underscores)))))
          results)))))




(defn keyhash-from-field-vector
  [f]
  (str (hash (str (nth f 0) ; db-type (Mysql, etc)
                  (nth f 2) ; table-type (view, table)
                  (nth f 3) ; db-schema
                  (nth f 4) ; db-catalog
                  (nth f 5) ; table-name
                  (nth f 6) ; field-name
                  (try (nth f 9) (catch Exception e nil)) ;; derived-calc if exists
             ))))

(defn contexthash-from-field-vector
  [f]
  (str (hash (str (nth f 0) ; db-type (Mysql, etc)
                  (nth f 2) ; table-type (view, table)
                  (nth f 3) ; db-schema
                  (nth f 4) ; db-catalog
                  (nth f 5) ; table-name
             ))))

(defn sqlize-keywords
  [honey-map]
  (walk/postwalk-replace (into {}
                               (for [k (filter keyword? (ut/deep-flatten honey-map))]
                                 (if (cstr/ends-with? (str k) "?")
                                   {k [:=
                                       (read-string (-> (str k)
                                                        (cstr/replace #"-" "_")
                                                        (cstr/replace "?" ""))) true]}
                                   {k (read-string (cstr/replace (str k) #"-" "_"))})))
                         honey-map))


(defn create-sqlite-sys-tables-if-needed!
  [system-db] ;; db-type
  (ut/pp [:creating-sys-tables])
  (let [] ; [ddl-ns (if (= db-type :clickhouse) clickhouse-ddl sqlite-ddl )]
    (sql-exec system-db sqlite-ddl/create-jvm-stats)
    (sql-exec system-db sqlite-ddl/create-client-stats)
    (sql-exec system-db sqlite-ddl/create-kits)
    (sql-exec system-db sqlite-ddl/create-screens)
    (sql-exec system-db sqlite-ddl/create-blocks)
    (sql-exec system-db sqlite-ddl/create-boards)
    (sql-exec system-db sqlite-ddl/create-tests)
    (sql-exec system-db sqlite-ddl/create-fields)
    (sql-exec system-db sqlite-ddl/create-connections)
    (sql-exec system-db sqlite-ddl/create-attributes)
    (sql-exec system-db sqlite-ddl/create-found-fields)
    (sql-exec system-db sqlite-ddl/create-combos)
    (sql-exec system-db sqlite-ddl/create-combo-rows)
    (sql-exec system-db sqlite-ddl/create-rules-table0)
    (sql-exec system-db sqlite-ddl/create-rules-table1)
    (sql-exec system-db sqlite-ddl/create-rules-table2)
    (sql-exec system-db sqlite-ddl/create-rules-table3)
    (sql-exec system-db sqlite-ddl/create-flow-functions)
    (sql-exec system-db sqlite-ddl/create-panel-history)
    (sql-exec system-db sqlite-ddl/create-board-history)
    (sql-exec system-db sqlite-ddl/create-errors)
    (sql-exec system-db sqlite-ddl/create-reco-vw)
    (sql-exec system-db sqlite-ddl/create-reco-vw2)
    (sql-exec system-db sqlite-ddl/create-status)
    (sql-exec system-db sqlite-ddl/create-client-items)
    (sql-exec system-db sqlite-ddl/create-client-memory)
    (sql-exec system-db sqlite-ddl/create-status-vw)))

(defn create-sqlite-flow-sys-tables-if-needed!
  [flows-db] ;; db-type
  (ut/pp [:creating-flow-sys-tables])
  (let []
    (sql-exec flows-db sqlite-ddl/create-flow-results)
    (sql-exec flows-db sqlite-ddl/create-flow-schedules)
    (sql-exec flows-db sqlite-ddl/create-flows)
    (sql-exec flows-db sqlite-ddl/create-flow-history)
    (sql-exec flows-db sqlite-ddl/create-live-schedules)
    (sql-exec flows-db sqlite-ddl/create-channel-history)
    (sql-exec flows-db sqlite-ddl/create-fn-history)))

(defn create-sniff-tests-for
  [system-db key-vector sniff-tests-map] ;; gets run once for each key-vector being processed
  (ut/ppln [:working-on-sniff-test-gen key-vector])
  (let [k                     key-vector
        db-type               (nth k 0)
        connection-hash       (nth k 1) ;; key to the connections table
        table-type            (nth k 2)
        db-schema             (nth k 3)
        db-catalog            (nth k 4)
        table-name            (nth k 5)
        field-name            (nth k 6)
        field-type            (nth k 7)
        data-type             (nth k 8)
        derived-calc          (if (= field-type "derived") (nth k 9) nil)
        derived-name          (if (= field-type "derived") (nth k 10) nil)
        tests                 sniff-tests-map ;default-sniff-tests
        tests-checked         (into {}
                                    (for [[k v] tests] ;; check if :sql-map is vector shorthand
                                      (cond ;(and (vector? (get v :sql-map)) (not (= data-type
                                        (vector? (get v :sql-map)) ;; regular sniff test def
                                          {k (assoc v :sql-map {:select (get v :sql-map) :from :table :limit 1})}
                                        :else                      {k v}))) ;; regular sniff
        granularity-map       (into {}
                                    (for [[k v] tests-checked] ;; is this a field test or
                                      (let [gg (some #{:field} (ut/deep-flatten (get v :sql-map)))]
                                        {k (if (nil? gg) :table gg)})))
        tests-checked         (if (= field-type "derived")
                                (walk/postwalk-replace {:field [derived-calc]} tests-checked)
                                tests-checked)
        postwalk-map          {:field      (keyword field-name) ;(if (not (= data-type
                               :table      (cond (= db-schema "none") (keyword table-name)
                                                 (and (cstr/includes? (cstr/lower-case db-type) "mysql") (= db-schema "none"))
                                                   (keyword (str db-catalog "." table-name))
                                                 :else (keyword (str db-schema "." table-name))) ;; mysql
                               :db-type    (str db-type)
                               :field-type (str field-type)
                               :db-catalog db-catalog
                               :data-type  (str data-type)
                               :table-name (str table-name)
                               :field-name (str field-name)
                               :db-schema  (str db-schema)
                               :table-type (str table-type)}
        derived-calc-passthru (walk/postwalk-replace postwalk-map derived-calc) ;;; replaced
        tests-w-params        (walk/postwalk-replace ;; materialize all the values we have
                                postwalk-map
                                tests-checked)]
    (ut/ppln [:sniffy k
              (-> (get tests :when)
                  (honey/format {:pretty false :inline true}))])
    (into
      {}
      (doall
        (for [[k v] tests-w-params] ;; filter out table tests during field loops and vice versa
          (let [table-sniff-and-*?     (and (= (get granularity-map k) :table) (= (keyword field-name) :*))
                field-sniff-and-not-*? (and (= (get granularity-map k) :field) (not (= (keyword field-name) :*)))
                valid-combo?           (or table-sniff-and-*? ;; we only want table level sniff
                                           field-sniff-and-not-*?)]
            (when valid-combo? (ut/ppln [k key-vector]))
            (when valid-combo?
              (let [when-vector (get v :when) ;; optional
                    wwhen-sql   (when (not (nil? when-vector))
                                  (str (first (to-sql {:select 1
                                                       :where  [:and [:= :connection_id connection-hash] when-vector]
                                                       :from   :connections
                                                       :limit  1}))))
                    wwhen-exec  (when (not (nil? when-vector))
                                  (try (sql-query system-db wwhen-sql)
                                       (catch Exception e {:error (.getMessage e) :sql wwhen-sql})))
                    wwhen?      (= (get-in (vec wwhen-exec) [0 :1]) 1)
                    hsql        (get v :sql-map)
                    mget        (get v :fetch-one?)
                    sql-string  (to-sql hsql key-vector system-db)
                    sniff-body  {:src          (str (first sql-string))
                                 :derived-calc derived-calc ;derived-calc-passthru
                                 :derived-name derived-name
                                 :type         (if mget :one :many)}
                    valid-test? (cond (not (nil? when-vector)) wwhen?
                                      :else                    true)]
                (do (do (ut/ppln [k {:valid valid-test?} when-vector wwhen? wwhen-sql]))
                    (when valid-test? {k sniff-body}) ;; skip queuing ones that fail the :when
                )))))))))

(defn get-sniff-test-results
  [system-db target-db field-vectors this-run-id sniff-tests-map]
  (vec
    (doall
      (for [k field-vectors]
        (let [sniffs        (create-sniff-tests-for system-db k sniff-tests-map)
              sniff-results (doall
                              (into {}
                                    (for [s (keys sniffs)]
                                      (let [result-type (get-in sniffs [s :type])
                                            sql-str     (get-in sniffs [s :src]) ;(try (get-in sniffs [s
                                            calc        (get-in sniffs [s :derived-calc])
                                            calc-name   (get-in sniffs [s :derived-name])
                                            s-result    (sql-query target-db [sql-str])
                                            val         (if (= result-type :one)
                                                          (let [inner-map? (map? (first s-result))]
                                                            (try (if inner-map? (first (vals (first s-result))) (first s-result))
                                                                 (catch Exception e
                                                                   (do (ut/pp {:sniff-test-exec-error e
                                                                               :result                s-result
                                                                               :test                  s
                                                                               :result-type           result-type
                                                                               :sql-str               sql-str})
                                                                       (insert-error-row! target-db
                                                                                          sql-str
                                                                                          {:sniff-test-rowset-err e
                                                                                           :result-type           result-type
                                                                                           :inner-map?            inner-map?
                                                                                           :s-result              s-result})
                                                                       s-result))))
                                                          (vec s-result))]
                                        {s {:val val :calc calc :calc-name calc-name :sql sql-str}}))))]
          {k sniff-results})))))

(defn insert-sniff-test-results!
  [system-db sniff-test-results this-run-id]
  (doall
    (for [row sniff-test-results] ;; (take 3 res)
      (let [vector-key    (first (first row))
            values        (last (last row))
            db_type       (nth vector-key 0)
            connection_id (nth vector-key 1)
            table_type    (nth vector-key 2)
            db_schema     (nth vector-key 3)
            db_catalog    (nth vector-key 4)
            table_name    (nth vector-key 5)
            field_name    (nth vector-key 6)
            field_type    (nth vector-key 7)
            data_type     (general-data-type-infer field_type)]
        (ut/ppln values)
        (doall
          (for [[test_name test-values] values]
            (let [test-value            (:val test-values)
                  test-sql              (:sql test-values)
                  derived_calc          (:calc test-values)
                  derived_name          (:calc-name test-values)
                  sample?               (vector? test-value)
                  key_hash              (keyhash-from-field-vector vector-key) ;; (str (hash
                  context_hash          (contexthash-from-field-vector vector-key)
                  sample_key_hash       (hash (str key_hash test_name))
                  sample-table-name-str (str "sample_" (cstr/replace sample_key_hash #"-" "_"))
                  test_name             (ut/unkeyword test_name)
                  empty-resultset?      (true? (when sample? (= (count test-value) 0)))
                  has-results?          (not empty-resultset?) ;; lazy
                  test_val_string       (when (and (not sample?)
                                                   (or (string? test-value)
                                                       (cstr/includes? (str (type test-value)) "DateTime") ;; find
                                                       (cstr/includes? (str (type test-value)) "Date")))
                                          (str test-value))
                  test_val_integer      (when (and (not sample?) (integer? test-value)) test-value)
                  test_val_float        (when (and (not sample?) (float? test-value)) test-value)
                  remove-old            (to-sql {:delete-from [:tests]
                                                 :where       [:and [:= :key_hash (if sample? sample-table-name-str key_hash)]
                                                               [:= :test_name test_name]]})
                  stmt                  (to-sql {:insert-into [:tests]
                                                 :columns     [:db_type :connection_id :table_type :db_schema :db_catalog
                                                               :table_name :field_name :field_type :data_type :test_sql
                                                               :test_raw_val :derived_calc :derived_name :key_hash :is_sample
                                                               :test_name :test_val_string :test_val_integer :test_val_float
                                                               :context_hash :run_id]
                                                 :values      [[db_type connection_id table_type db_schema db_catalog table_name
                                                                field_name field_type data_type test-sql (str test-value)
                                                                (str derived_calc) (str derived_name)
                                                                (if sample? sample-table-name-str key_hash) (if sample? 1 0)
                                                                test_name test_val_string test_val_integer test_val_float
                                                                context_hash this-run-id]]})]
              (ut/ppln {:test-name test_name :vector-key vector-key})
              (if sample? ;; meaning the sniff test returns a rowset the needs it own table
                (try
                  (let [create-stmt           (when has-results? ;; has to be hardcoded
                                                                 ;; Sqlite-ddl!!!
                                                (sqlite-ddl/create-attribute-sample sample-table-name-str
                                                                                    (walk/postwalk-replace {:case :case_stmt}
                                                                                                           test-value)))
                        drop-old-sample-table (str "drop table if exists " sample-table-name-str ";")
                        multi-insert          (when has-results?
                                                (to-sql
                                                  {:insert-into [(keyword sample-table-name-str)]
                                                   :columns     (if (nil? (keys (first test-value)))
                                                                  []
                                                                  (walk/postwalk-replace {:case :case_stmt}
                                                                                         (keys (first test-value)))) ;; honey
                                                   :values      (vec (for [row test-value]
                                                                       (vec (for [[_ v] row]
                                                                              (if (or (cstr/includes? (str (type v)) "DateTime") ;; find
                                                                                      (cstr/includes? (str (type v)) "Date"))
                                                                                (str v)
                                                                                v)))))}))]
                    #_{:clj-kondo/ignore [:redundant-do]}
                    (do (sql-exec system-db drop-old-sample-table)
                        (sql-exec system-db remove-old)
                        (when has-results? ;; dont create a table based on empty vector
                          (sql-exec system-db create-stmt))
                        (sql-exec system-db stmt)
                        (when has-results? ;; dont try to insert an empty vector
                          (sql-exec system-db multi-insert))))
                  (catch Exception e
                    (insert-error-row! (str :creating-sample-table-from-rowset)
                                       (str test-value " " test_name " " vector-key " " test-sql)
                                       (str e))))
                (let []
                  #_{:clj-kondo/ignore [:redundant-do]}
                  (do (sql-exec system-db remove-old) (sql-exec system-db stmt)))))))))))

(defn get-attribute-flag-results
  [system-db field-vectors this-run-id field-attributes-map]
  (doall
    (vec
      (flatten
        (remove nil?
          (for [vector-key field-vectors] ;(distinct (keys (into {} flat-meta)))]
            (let [db_type       (nth vector-key 0)
                  connection_id (nth vector-key 1)
                  table_type    (nth vector-key 2)
                  db_schema     (nth vector-key 3)
                  db-catalog    (nth vector-key 4)
                  table_name    (nth vector-key 5)
                  field_name    (nth vector-key 6)
                  field_type    (nth vector-key 7)
                  data_type     (nth vector-key 8)
                  derived_calc  (try (nth vector-key 9) (catch Exception _ nil))
                  derived_name  (try (nth vector-key 10) (catch Exception _ nil))
                  derived?      (not (nil? derived_calc))
                  key-map       {:db_type       db_type
                                 :connection_id connection_id
                                 :table_type    table_type
                                 :db_schema     db_schema
                                 :db_catalog    db-catalog
                                 :table_name    table_name
                                 :field_name    field_name}
                  attributes    field-attributes-map ;default-field-attributes
                  granularity   (if (= field_name "*") :table :field)]
              (when (= granularity :field) ;; diff attrib for table level?
                (doall
                  (for [[k v] attributes] ;; TODO
                    (let
                      [;attribute-logic (read-string (cstr/replace (str v) #"-" "_"))
                       attribute-logic (walk/postwalk-replace (into {}
                                                                    (for [k (filter keyword? (ut/deep-flatten v))]
                                                                      {k (read-string (cstr/replace (str k) #"-" "_"))}))
                                                              v)
                       available-single-value-tests-sql
                         (to-sql {:select-distinct [:test_name]
                                  :from            [:tests]
                                  :where           [:and [:= :connection_id connection_id] [:= :table_name table_name]
                                                    [:= :db_schema db_schema] [:= :field_name field_name]
                                                    [:= :derived_calc (str derived_calc)] [:= :is_sample 0]]})
                       available-single-value-types-sql
                         (to-sql
                           {:select-distinct
                              [:test_name
                               [[:raw
                                 "case when test_val_string is not null then 'test_val_string' 
                                                                                                                   when test_val_integer is not null then 'test_val_integer' 
                                                                                                                   when test_val_float is not null then 'test_val_float' 
                                                                                                                   else test_val_string end"]
                                :type]]
                            :from [:tests]
                            :where [:and [:= :connection_id connection_id] [:= :table_name table_name] [:= :db_schema db_schema]
                                    [:= :field_name field_name] [:= :derived_calc (str derived_calc)] [:= :is_sample 0]]})
                       available-single-value-types (into {}
                                                          (for [r (sql-query system-db available-single-value-types-sql)]
                                                            {(keyword (get r :test_name)) (keyword (get r :type))}))
                       available-field-tests [:db_type :table_type :table_name :field_name :field_type :data_type]
                       available-connection-tests [:database_name :database_version :user_name]
                       available-single-value-tests (vec (for [r (sql-query system-db available-single-value-tests-sql)]
                                                           (keyword (get r :test_name))))
                       single-value-tests-join
                         (read-string
                           (str "[" ;; weird workaround for honeysql not allowing a vector of
                                (cstr/join
                                  ""
                                  (drop-last
                                    (subs (str (into {}
                                                     (for [f available-single-value-tests]
                                                       (let [sub-q-alias   (str "tmp_" (ut/unkeyword f))
                                                             test-val-type (get available-single-value-types f :test_raw_val)]
                                                         {[{:select [[test-val-type :v]]
                                                            :from   :tests
                                                            :where  [[:and [:= :connection_id connection_id]
                                                                      [:= :table_name table_name] [:= :db_schema db_schema]
                                                                      [:= :field_name field_name]
                                                                      [:= :derived_calc (str derived_calc)]
                                                                      [:= :test_name (ut/unkeyword f)] [:= :is_sample 0]]]
                                                            :limit  1} (keyword sub-q-alias)]
                                                            [:= 1 1] ;; a ""real join"" is really
                                                         }))))
                                          1)))
                                "]"))
                       single-value-tests-select-aliases (vec (for [f available-single-value-tests]
                                                                [(keyword (str "tmp_" (ut/unkeyword f) ".v")) f]))
                       final-select-vec (into (into available-field-tests available-connection-tests)
                                              single-value-tests-select-aliases)
                       pivot-honey {:select-distinct final-select-vec
                                    :from            [:tests :connections]
                                    :left-join       single-value-tests-join
                                    :where           [[:and [:= :tests.connection_id :connections.connection_id]
                                                       [:= :tests.connection_id connection_id] [:= :tests.table_name table_name]
                                                       [:= :tests.db_schema db_schema] [:= :tests.field_name field_name]
                                                       [:= :tests.derived_calc (str derived_calc)] [:= :tests.is_sample 0]]]}
                       attribute-test-pivot-query-honey {:select 1
                                                         :from   [[pivot-honey :sub1]] ;; the
                                                         :where  attribute-logic
                                                         :limit  1}
                       attribute? (= 1
                                     (sql-query-one
                                       system-db
                                       (to-sql attribute-test-pivot-query-honey)
                                       {:hs attribute-test-pivot-query-honey :fn [field_name derived_name] :c "get-attributes"}))
                       key-map-w-attributes (merge key-map
                                                   {:run_id          this-run-id
                                                    :key_hash        (keyhash-from-field-vector vector-key)
                                                    :context_hash    (contexthash-from-field-vector vector-key)
                                                    :derived_calc    (str derived_calc)
                                                    :derived_name    derived_name
                                                    :attribute_name  (ut/unkeyword k)
                                                    :attribute_value attribute?})]
                      key-map-w-attributes)))))))))))


(defn insert-attribute-flag-results!
  [system-db attribute-flag-results this-run-id]
  (dorun
    (for [attribute-map attribute-flag-results]         ;; TODO make this one big multi-insert, but
      (do (sql-exec system-db
                    (to-sql {:delete-from [:attributes] ;; remove any previous taste test (no
                             :where       (cons :and
                                                (vec (doall (for [[k v] ;(merge
                                                                    (select-keys attribute-map
                                                                                 [:connection_id :db_schema :db_type
                                                                                  :attribute_name :derived_calc :field_name
                                                                                  :table_name :table_type :key_hash])]
                                                              [:= k v]))))}))
          (when (> (count attribute-map) 0)
            (sql-exec system-db
                      (to-sql {:insert-into [:attributes] :columns (keys attribute-map) :values [(vals attribute-map)]})))))))

(defn get-possible-derived-fields
  [system-db field-vectors this-run-id derived-field-map]
  (vec
    (first
      (doall
        (remove nil? ;; ghetto multi-for flatten one layer
          (for [vector-key field-vectors] ;(distinct (keys (into {} flat-meta)))]
            (let [db_type       (nth vector-key 0)
                  connection_id (nth vector-key 1)
                  table_type    (nth vector-key 2)
                  db_schema     (nth vector-key 3)
                  db_catalog    (nth vector-key 4)
                  table_name    (nth vector-key 5)
                  field_name    (nth vector-key 6)
                  field_type    (nth vector-key 7)
                  key-map       {:db_type       db_type
                                 :connection_id connection_id
                                 :table_type    table_type
                                 :db_schema     db_schema
                                 :db_catalog    db_catalog
                                 :table_name    table_name
                                 :field_name    field_name}
                  field-tests   derived-field-map
                  granularity   (if (= field_name "*") :table :field)]
              (when (= granularity :field) ;; diff attrib for table level?
                (remove nil?
                  (doall
                    (for [[derived-key v] field-tests] ;; TODO
                      (let
                        [;derived-field-logic (read-string (cstr/replace (str v) #"-" "_")) ;;
                         derived-field-logic (walk/postwalk-replace (into {}
                                                                          (for [k (filter keyword? (ut/deep-flatten v))]
                                                                            (if (cstr/ends-with? (str k) "?")
                                                                              {k [:=
                                                                                  (read-string (-> (str k)
                                                                                                   (cstr/replace #"-" "_")
                                                                                                   (cstr/replace "?" ""))) true]}
                                                                              {k (read-string (cstr/replace (str k) #"-" "_"))})))
                                                                    (get v :when))
                         calc-field (get v :calc)
                         available-attributes-sql (to-sql {:select-distinct [:attribute_name] ;; add another set
                                                           :from            [:attributes]
                                                           :where           [:and [:= :connection_id connection_id]
                                                                             [:= :table_name table_name] [:= :db_schema db_schema]
                                                                             [:= :field_name field_name]]})
                         available-single-value-tests-sql
                           (to-sql {:select-distinct [:test_name]
                                    :from            [:tests]
                                    :where           [:and [:= :connection_id connection_id] [:= :table_name table_name]
                                                      [:= :db_schema db_schema] [:= :field_name field_name] [:= :is_sample 0]]})
                         available-single-value-types-sql
                           (to-sql
                             {:select-distinct
                                [:test_name
                                 [[:raw
                                   "case when test_val_string is not null then 'test_val_string' 
                                                                                                                   when test_val_integer is not null then 'test_val_integer' 
                                                                                                                   when test_val_float is not null then 'test_val_float' 
                                                                                                                   else test_val_string end"]
                                  :type]]
                              :from [:tests]
                              :where [:and [:= :connection_id connection_id] [:= :table_name table_name] [:= :db_schema db_schema]
                                      [:= :field_name field_name] [:= :is_sample 0]]})
                         available-single-value-types (into {}
                                                            (for [r (sql-query system-db available-single-value-types-sql)]
                                                              {(keyword (get r :test_name)) (keyword (get r :type))}))
                         available-field-tests [:db_type :table_type :table_name :field_name :field_type :data_type]
                         available-connection-tests [:database_name :database_version :user_name]
                         available-single-value-tests (vec (for [r (sql-query system-db available-single-value-tests-sql)]
                                                             (keyword (get r :test_name))))
                         available-attributes (vec (for [r (sql-query system-db available-attributes-sql)]
                                                     (keyword (get r :attribute_name))))
                         merged-join
                           (read-string
                             (str "[" ;; weird workaround for honeysql not allowing a vector of
                                  (cstr/join
                                    ""
                                    (drop-last
                                      (subs (str
                                              (merge
                                                (into {}
                                                      (for [f available-single-value-tests]
                                                        (let [sub-q-alias   (str "tmp_" (ut/unkeyword f))
                                                              test-val-type (get available-single-value-types f :test_raw_val)]
                                                          {[{:select [[test-val-type :v]]
                                                             :from   :tests
                                                             :where  [[:and [:= :connection_id connection_id]
                                                                       [:= :table_name table_name] [:= :db_schema db_schema]
                                                                       [:= :field_name field_name]
                                                                       [:= :test_name (ut/unkeyword f)] [:= :is_sample 0]]]
                                                             :limit  1} (keyword sub-q-alias)]
                                                             [:= 1 1] ;; a ""real join"" is really
                                                          })))
                                                (into {}
                                                      (for [f available-attributes]
                                                        (let [sub-q-alias (cstr/replace (str "tmp_" (ut/unkeyword f)) "?" "")]
                                                          {[{:select [[:attribute_value :v]]
                                                             :from   :attributes
                                                             :where  [[:and [:= :connection_id connection_id]
                                                                       [:= :table_name table_name] [:= :db_schema db_schema]
                                                                       [:= :field_name field_name]
                                                                       [:= :attribute_name (ut/unkeyword f)]]]
                                                             :limit  1} (keyword sub-q-alias)]
                                                             [:= 1 1] ;; a ""real join"" is really
                                                          })))))
                                            1)))
                                  "]"))
                         single-value-tests-select-aliases (vec (for [f available-single-value-tests]
                                                                  [(keyword (str "tmp_" (ut/unkeyword f) ".v")) f]))
                         attributes-select-aliases (vec (for [f available-attributes]
                                                          [(keyword (cstr/replace (str "tmp_" (ut/unkeyword f) ".v") "?" ""))
                                                           (keyword (cstr/replace (str (ut/unkeyword f)) "?" ""))]))
                         final-select-vec (into (into (into available-field-tests available-connection-tests)
                                                      single-value-tests-select-aliases)
                                                attributes-select-aliases)
                         pivot-honey {:select-distinct final-select-vec
                                      :from            [:tests :connections]
                                      :left-join       merged-join
                                      :where           [[:and [:= :tests.connection_id :connections.connection_id]
                                                         [:= :tests.connection_id connection_id] [:= :tests.table_name table_name]
                                                         [:= :tests.db_schema db_schema] [:= :tests.field_name field_name]
                                                         [:= :tests.is_sample 0]]]}
                         attribute-test-pivot-query-honey {:select 1
                                                           :from   [[pivot-honey :subq1]] ;; the
                                                           :where  derived-field-logic
                                                           :limit  1}
                         valid? (= 1 (sql-query-one system-db (to-sql attribute-test-pivot-query-honey)))
                         new-derived-name (-> (str derived-key "_" field_name)
                                              (clojure.string/replace " " "_")
                                              (clojure.string/replace ":" "")
                                              (clojure.string/replace "-" "_"))
                         derived-vector-key [db_type connection_id table_type db_schema db_catalog table_name field_name "derived"
                                             "derived" calc-field new-derived-name]]
                        (when valid? derived-vector-key) ; key-map-w-attributes)
                      ))))))))))))







(defn insert-connection-data!
  [target-db system-db this-run-id connect-meta sql-filter]
  (do (sql-exec system-db (to-sql {:delete-from [:connections] :where [:= :connection_id (get connect-meta :connection_id)]}))
      (sql-exec system-db
                (to-sql
                  {:insert-into [:connections]
                   :columns     (conj (conj (conj (vec (keys connect-meta)) :run_id) :original_connection_str) :metadata_filter)
                   :values      [(conj (conj (conj (vec (vals connect-meta)) this-run-id) (str target-db)) (str sql-filter))]}))))

(defn update-connection-data!
  [system-db this-run-id connection-id]
  (sql-exec system-db
            (to-sql {:update [:connections]
                     :set    {:ended_at [:raw "date('now')"]}
                     :where  [:and [:= :connection_id connection-id] [:= :run_id this-run-id]]})))

(defn create-target-field-vectors
  [target-db connect-meta]
  (let [flat-meta     (let [mm (surveyor/get-jdbc-conn-meta target-db)
                            fm (surveyor/flatten-jdbc-conn-meta connect-meta mm)]
                        (into {} fm))
        field-vectors (vec (for [[k v] flat-meta]
                             (let [db-col-type        (get v :column_type)
                                   general-data-infer (general-data-type-infer db-col-type)]
                               (conj (conj k db-col-type) general-data-infer))))] ;; TODO [eyes
    field-vectors))

(defn get-latest-run-id
  [system-db connect-meta]
  (let [last-run-sql (to-sql {:select [[[:max :run_id] :last_run_id]]
                              :from   [:connections]
                              :where  [:= :connection_id (get connect-meta :connection_id)]})
        last-run-id  (sql-query-one system-db last-run-sql)]
    (if (nil? last-run-id) 1 (+ last-run-id 1))))

(defn find-fields
  [system-db context-map logic]
  (let
    [connection_id (:connection_id context-map)
     db_schema (:db_schema context-map)
     table_name (:table_name context-map)
     field-logic (sqlize-keywords logic)
     available-attributes-sql (to-sql {:select-distinct [:attribute_name] ;; add another set of
                                       :from            [:attributes]
                                       :where           [:and [:= :connection_id connection_id] [:= :table_name table_name]
                                                         [:= :db_schema db_schema]]})
     available-single-value-tests-sql (to-sql {:select-distinct [:test_name]
                                               :from            [:tests]
                                               :where           [:and [:= :connection_id connection_id]
                                                                 [:= :table_name table_name] [:= :db_schema db_schema]
                                                                 [:= :is_sample 0]]})
     available-single-value-types-sql
       (to-sql
         {:select-distinct
            [:test_name
             [[:raw
               "case when test_val_string is not null then 'test_val_string' 
                                                                                   when test_val_integer is not null then 'test_val_integer' 
                                                                                   when test_val_float is not null then 'test_val_float' 
                                                                                   else test_val_string end"]
              :type]]
          :from [:tests]
          :where [:and [:= :connection_id connection_id] [:= :table_name table_name] [:= :db_schema db_schema]
                  [:= :is_sample 0]]})
     available-single-value-types (into {}
                                        (for [r (sql-query system-db available-single-value-types-sql)]
                                          {(keyword (get r :test_name)) (keyword (get r :type))}))
     field-meta-fields [:db_type :table_type :table_name :f.field_name :field_type :data_type :key_hash :f.connection_id
                        :db_schema :db_catalog :context_hash :derived_name :derived_calc]
     available-connection-tests [:database_name :database_version :user_name]
     available-single-value-tests (vec (for [r (sql-query system-db available-single-value-tests-sql)]
                                         (keyword (get r :test_name))))
     available-attributes (vec (for [r (sql-query system-db available-attributes-sql)] (keyword (get r :attribute_name))))
     merged-join (read-string
                   (str "[" ;; weird workaround for honeysql not allowing a vector of vectors for join key..
                        (cstr/join
                          ""
                          (drop-last
                            (subs (str
                                    (merge (into {}
                                                 (for [f available-single-value-tests]
                                                   (let [sub-q-alias   (str "tmp_" (ut/unkeyword f))
                                                         keyhash_alias (keyword (str sub-q-alias ".kh"))
                                                         test-val-type (get available-single-value-types f :test_raw_val)]
                                                     {[{:select [[:key_hash :kh] [test-val-type :v]]
                                                        :from   :tests
                                                        :where  [[:and [:= :connection_id connection_id]
                                                                  [:= :table_name table_name] [:= :db_schema db_schema]
                                                                  [:= :test_name (ut/unkeyword f)] [:= :is_sample 0]]]}
                                                       (keyword sub-q-alias)]
                                                        [:= :f.key_hash keyhash_alias]})))
                                           (into {}
                                                 (for [f available-attributes]
                                                   (let [sub-q-alias   (cstr/replace (str "tmp_" (ut/unkeyword f)) "?" "")
                                                         keyhash_alias (keyword (str sub-q-alias ".kh"))]
                                                     {[{:select [[:key_hash :kh] [:attribute_value :v]]
                                                        :from   :attributes
                                                        :where  [[:and [:= :connection_id connection_id]
                                                                  [:= :table_name table_name] [:= :db_schema db_schema]
                                                                  [:= :attribute_name (ut/unkeyword f)]]]} (keyword sub-q-alias)]
                                                        [:= :f.key_hash keyhash_alias]})))))
                                  1)))
                        "]"))
     single-value-tests-select-aliases (vec (for [f available-single-value-tests]
                                              [(keyword (str "tmp_" (ut/unkeyword f) ".v")) f]))
     attributes-select-aliases (vec (for [f available-attributes]
                                      [(keyword (cstr/replace (str "tmp_" (ut/unkeyword f) ".v") "?" ""))
                                       (keyword (cstr/replace (str (ut/unkeyword f)) "?" ""))]))
     final-select-vec (into (into (into field-meta-fields available-connection-tests) single-value-tests-select-aliases)
                            attributes-select-aliases)
     pivot-honey {:select-distinct final-select-vec
                  :from            [[:fields :f]]
                  :join            [[:connections :c] [:= :f.connection_id :c.connection_id]]
                  :left-join       merged-join
                  :where           [[:and [:= :f.connection_id connection_id] [:= :f.table_name table_name]
                                     [:= :f.db_schema db_schema]]]}
     logic-test-filter {:select :* :from [[pivot-honey :subq1]] :where field-logic}
     results (sql-query system-db (to-sql logic-test-filter))]
    results))

(defn insert-found-fields!
  [system-db context-map axes-key shape-name fields-rowset]
  (let [connection_id   (:connection_id context-map)
        shape-keys      {:axes_key (ut/unkeyword axes-key) :shape_name (ut/unkeyword shape-name)}
        db_schema       (:db_schema context-map)
        table_name      (:table_name context-map)
        insert-keys     [:db_type :connection_id :table_type :db_schema :db_catalog :table_name :axes_key :shape_name :field_name
                         :key_hash :context_hash :logic_map :derived_calc :derived_name] ;; fields coming from found-fields
                                                                                         ;; are arbitrary except
        rowset          (vec (for [r fields-rowset]
                               (merge {:logic_map (pr-str r)} (merge shape-keys (select-keys r insert-keys)))))
        remove-old-rows (to-sql {:delete-from [:found_fields]
                                 :where       [:and [:= :connection_id connection_id] [:= :table_name table_name]
                                               [:= :db_schema db_schema] [:= :axes-key (ut/unkeyword axes-key)]
                                               [:= :shape_name (ut/unkeyword shape-name)]]})
        rowset-insert   (to-sql {:insert-into [:found_fields]
                                 :values      rowset ;insert-vals
                                })]
    (sql-exec system-db remove-old-rows)
    (sql-exec system-db rowset-insert)))

(def target-db ;"jdbc:sqlite:./rabbit-sample-data.db")
  {:classname "org.sqlite.JDBC" :subprotocol "sqlite" :subname "./data/rabbit-sample-data.db"})

(def target-db2 {:classname "org.sqlite.JDBC" :subprotocol "sqlite" :subname "./data/boston-crime-data.db"})

(def target-db3 "jdbc:postgresql://postgres:postgrespw@localhost:49154/postgres")

(def target-db4 ;"jdbc:clickhouse://localhost:8123/test")
  {:classname "ru.yandex.clickhouse.ClickHouseDriver" :subprotocol "clickhouse" :subname "//localhost:8123/default"})

(def target-db5 ;"jdbc:vertica://dbadmin@localhost:5433/VMart")
  {:classname   "com.vertica.jdbc.Driver" ;; "org.clojars.prepor.Driver"
   :subprotocol "vertica"
   :subname     "_VMart" ; first char gets cut somehow?
   :user        "dbadmin"
   :host        "localhost" ;"0.0.0.0" ;"172.27.144.1" ;"localhost"
   :port        5433})

(def target-db6 "jdbc:mysql://root@localhost:3306/imdb")

(def target-db7 "jdbc:sqlserver://localhost:1433;databaseName=imdb;encrypt=false;username=sa;password=yourStrong(!)Password") ;"jdbc:sqlserver://sa:yourStrong(!)Password@localhost:1433;database=imdb")


(def target-db8 "jdbc:oracle:scott/tiger@localhost:1521:XE") ; "jdbc:oracle://system:tiger@localhost:1521:XE")

(defn slread
  [file]
  (try (edn/read-string (slurp file)) ;; just in case the file is empty or can't parse as EDN
       (catch Exception _ {})))

(def default-flow-functions (slread "./defs/flow-functions.edn"))
(def default-sniff-tests (slread "./defs/sniff-tests.edn"))
(def default-field-attributes (slread "./defs/field-attributes.edn"))
(def default-derived-fields (slread "./defs/derived-fields.edn"))
(def default-viz-shapes (slread "./defs/viz-shapes.edn"))

(defn insert-field-vectors!
  [field-vectors system-db & sql-filter]
  (try
    (doall
      (for [f     field-vectors
            :when (not (empty? f))] ;; todo, just make this a map and pass to both selectively
        (let [query                 (get @sql/query-history (nth f 5))
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
                                                 :else        g))) ;; assume its a reg keyword
              is-group-by?          (if (not no-group-by?)
                                      (true? (try (some #(= (keyword (nth f 6)) %) materialize-group-bys)
                                                  (catch Exception _ false)))
                                      true)]
          (sql-exec system-db
                    (to-sql {:delete-from [:fields]
                             :where       [:and [:= :db_type (nth f 0)] [:= :connection_id (nth f 1)] [:= :table_type (nth f 2)]
                                           [:= :db_schema (nth f 3)] [:= :db_catalog (nth f 4)] [:= :table_name (nth f 5)]
                                           [:= :field_name (nth f 6)]
                                           [:= :derived_calc (try (str (nth f 9)) (catch Exception _ nil))]
                                           [:= :derived_name (try (str (nth f 10)) (catch Exception _ nil))]]}))
          (sql-exec system-db
                    (to-sql
                      {:insert-into [:fields]
                       :columns     [:db_type :connection_id :table_type :db_schema :db_catalog :table_name :field_name
                                     :field_type :data_type :derived_calc :derived_name :key_hash :context_hash :is_group_by]
                       :values      [[(nth f 0) (nth f 1) (nth f 2) (nth f 3) (nth f 4) (nth f 5) (nth f 6) (nth f 7) (nth f 8)
                                      (try (str (nth f 9)) (catch Exception _ nil)) (try (str (nth f 10)) (catch Exception _ nil))
                                      (keyhash-from-field-vector f) (contexthash-from-field-vector f) is-group-by?]]})))))
    (catch Exception e
      (do (insert-error-row! target-db (str (str e)) {:sql-filter sql-filter :field-vectors field-vectors})
          (ut/pp [:insert-field-vectors-ERROR! :f field-vectors :error e])))))

(defn taste-test-derived-fields
  [target-db field-vectors]
  (vec
    (for [vector-key field-vectors]
      (let [db_type        (nth vector-key 0)
            connection_id  (nth vector-key 1)
            table_type     (nth vector-key 2)
            db_schema      (nth vector-key 3)
            db_catalog     (nth vector-key 4)
            table_name     (nth vector-key 5)
            field_name     (nth vector-key 6)
            field_type     (nth vector-key 7)
            data_type      (nth vector-key 8)
            derived_calc   (nth vector-key 9)
            derived_name   (nth vector-key 10)
            sql-test       {:select [[derived_calc]] :from [:table] :limit 1}
            honey-map      (walk/postwalk-replace {:table (cond (= db_schema "none") (keyword table_name) ;; dupe
                                                                (and (cstr/includes? (cstr/lower-case db_type) "mysql")
                                                                     (= db_schema "none"))
                                                                  (keyword (str db_catalog "." table_name))
                                                                :else (keyword (str db_schema "." table_name)))
                                                   :field (keyword field_name)}
                                                  sql-test)
            sql-stmt       (to-sql honey-map vector-key system-db)
            test-value     (sql-query-one target-db sql-stmt)
            post-data-type (general-data-type-value test-value)]
        [db_type connection_id table_type db_schema db_catalog table_name field_name field_type post-data-type derived_calc
         derived_name]))))

(defn process-field-vectors
  [system-db target-db field-vectors this-run-id sniff-tests-map field-attributes-map derived-field-map & derived?]
  (insert-sniff-test-results! system-db
                              (get-sniff-test-results system-db target-db field-vectors this-run-id sniff-tests-map)
                              this-run-id)
  (insert-attribute-flag-results! system-db
                                  (get-attribute-flag-results system-db field-vectors this-run-id field-attributes-map)
                                  this-run-id)
  (when (not derived?)
    (let [derived-field-vectors        (get-possible-derived-fields system-db field-vectors this-run-id derived-field-map)
          derived-field-vectors-tasted (taste-test-derived-fields target-db derived-field-vectors)]
      (when #_{:clj-kondo/ignore [:not-empty?]}
        (not (empty? derived-field-vectors-tasted))
        (dorun ;(ut/pp derived-field-vectors-tasted)
          (insert-field-vectors! derived-field-vectors-tasted system-db)
          (process-field-vectors system-db
                                 target-db
                                 derived-field-vectors-tasted
                                 this-run-id
                                 sniff-tests-map
                                 field-attributes-map
                                 derived-field-map
                                 true))))))

(def task-pool (tp/create-pool "field-processing-pool" 8))

(defn find-all-viz-fields!
  [system-db viz-shapes connection_id & sql-filter]
  (ut/pp [:finding-fields-for-viz-shapes (if sql-filter {:where (first sql-filter)} :all-in-database)])
  (let [base-honey {:select-distinct [:connection_id :db_schema :table_name] :from [:fields]}
        honey-map  (if sql-filter (assoc base-honey :where (first (sqlize-keywords sql-filter))) base-honey)
        rowset     (sql-query system-db (to-sql honey-map))]
    (doall (for [context rowset]
             (doall (for [[shape-name v] viz-shapes]
                      (let [axes  (get v :axes)
                            score (get v :base-score)]
                        (dorun (for [[axis-name logic] axes]
                                 (let [found-fields (find-fields system-db context logic)]
                                   (when (not (empty? found-fields))
                                     (insert-found-fields! system-db context axis-name shape-name found-fields))))))))))))



(defonce score-atom0 (atom {}))
(defonce score-atom1 (atom {}))
(defonce score-atom2 (atom {}))
(defonce rekey-atom (atom {}))
(defonce lookup-field-cache (atom {}))

(defn rekey
  [k]
  (let [cache (get @rekey-atom k)]
    (if cache
      cache
      (let [re (keyword (cstr/replace k #"_" "-"))]
        (swap! rekey-atom assoc k re)
        re))))

(def pool0 (cp/threadpool 4))
(def pool1 (cp/threadpool 4))
(def pool2 (cp/threadpool 4))

(defn find-all-viz-combos!
  [system-db viz-shape connection_id & sql-filter]
  (ut/pp [:finding-viz-shape-combos (if sql-filter {:where (first sql-filter)} :all-in-database)])
  (let [base-honey {:select-distinct [:context_hash :shape_name :axes_key :key_hash :derived_name :derived_calc]
                    :from            [:found_fields]}
        honey-map  (if sql-filter (assoc base-honey :where (first (sqlize-keywords sql-filter))) base-honey)
        rowset     (sql-query system-db (to-sql honey-map))
        row-map    (into {}
                         (for [[k v] (group-by :context_hash rowset)]
                           {k (into {}
                                    (for [[k0 v0] (group-by :shape_name v)]
                                      {k0 (into {}
                                                (for [[k1 v1] (group-by :axes_key v0)]
                                                  {k1 (vec (for [e v1] (:key_hash e)))}))}))}))]
    (doall
      (doseq [[ctx ctxval] row-map] ;; NOTE: pdoseq geneates false lint errors!
        (do
          (sql-exec system-db (to-sql {:delete-from [:combos] :where [:= :context_hash ctx]}))
          (doall
            (doseq [[shape-name axes] ctxval]
              (let [axes-keys          (vec (keys axes))
                    combo-vecs         (vec (for [[_ avals] axes] avals))
                    valid?             (= (count axes-keys) (count (keys (get-in viz-shape [(rekey shape-name) :axes]))))
                    cartesians         (apply combo/cartesian-product combo-vecs)
                    filtered-carts     (remove nil? (for [c cartesians] (when (= (count axes-keys) (count (distinct c))) c)))
                    zip-filtered-carts (for [c filtered-carts] (zipmap axes-keys c))]
                (when valid?
                  (doall
                    (doseq [c zip-filtered-carts]
                      (dorun
                        (try
                          (let [;lookup-field-data-map
                                lookup-field-data-map (fn [key-hash]
                                                        (let [cache   (get @lookup-field-cache key-hash)
                                                              cached? (not (nil? cache))]
                                                          (if cached?
                                                            cache
                                                            (let [vv (first (sql-query
                                                                              system-db
                                                                              (to-sql {:select [:*]
                                                                                       :from   [:fields]
                                                                                       :where  [:and [:= :key_hash key-hash]
                                                                                                [:= :connection_id connection_id]]
                                                                                       :limit  1})))]
                                                              (swap! lookup-field-cache assoc key-hash vv)
                                                              vv)))) ;)
                                sample-lookup (lookup-field-data-map (first (vals c)))
                                sample-key-vec (let [f sample-lookup]
                                                 [(:db_type f) (:connection_id f) (:table_type f) (:db_schema f) (:db_catalog f)
                                                  (:table_name f) nil nil nil])
                                db-type (:db_type sample-lookup)
                                db-schema (:db_schema sample-lookup)
                                table-name (:table_name sample-lookup)
                                parent-keys (select-keys sample-lookup
                                                         [:db_type :db_schema :db_catalog :table_name :table_type :connection_id])
                                is-mysql? (and (cstr/includes? (cstr/lower-case db-type) "mysql") (= db-schema "none"))
                                field-replacements {:table (str "query/" table-name)}
                                aliased-selects
                                  (into {}
                                        (apply concat
                                          (remove empty?
                                            (for [q (get-in viz-shape [(rekey shape-name) :sql-maps])] ;; hash
                                              (when (filter #(and (vector? %)
                                                                  (= (count %) 2)
                                                                  (some (fn [x] (= x (first %)))
                                                                        (remove nil?
                                                                          (for [[k v] c]
                                                                            (when (not (nil? (:derived_name (lookup-field-data-map
                                                                                                              (str v)))))
                                                                              (keyword (str (ut/unkeyword k) "-field")))))))
                                                      (get q :select))
                                                {[(.indexOf (get-in viz-shape [(rekey shape-name) :sql-maps]) q)]
                                                   (filter #(and (vector? %)
                                                                 (= (count %) 2)
                                                                 (some (fn [x] (= x (first %)))
                                                                       (remove nil?
                                                                         (for [[k v] c]
                                                                           (when (not (nil? (:derived_name (lookup-field-data-map
                                                                                                             (str v)))))
                                                                             (keyword (str (ut/unkeyword k) "-field")))))))
                                                     (get q :select))})))))
                                aliased-selects2 (into {}
                                                       (apply concat
                                                         (for [[k v] c]
                                                           (when (not (nil? (:derived_name (lookup-field-data-map (str v)))))
                                                             {(keyword (str (ut/unkeyword k) "-field"))
                                                                (walk/postwalk-replace
                                                                  {:field (keyword (:field_name (lookup-field-data-map (str v))))}
                                                                  (read-string (:derived_calc (lookup-field-data-map (str
                                                                                                                       v)))))}))))
                                aliased-selects3 (into {}
                                                       (apply concat
                                                         (for [[k a] aliased-selects]
                                                           {(first (flatten k))
                                                              (first (let [tt (walk/postwalk-replace aliased-selects2 a)]
                                                                       (for [idx (range (count tt))]
                                                                         {(nth a idx) (nth tt idx)})))})))
                                query-replacement-map
                                  (merge (into {} ;; alias and logic - for selected clause
                                               (for [[k v] c]
                                                 {(keyword (str (ut/unkeyword k) "-field"))
                                                    (let [calc?      (not (nil? (:derived_name (lookup-field-data-map (str v)))))
                                                          base-field (:field_name (lookup-field-data-map (str v)))]
                                                      (if calc?
                                                        [(walk/postwalk-replace {:field (keyword base-field)}
                                                                                (read-string (:derived_calc (lookup-field-data-map
                                                                                                              (str v)))))
                                                         (keyword (:derived_name (lookup-field-data-map (str v))))]
                                                        (keyword base-field)))}))
                                         field-replacements)
                                query-replacement-map2
                                  (merge (into {} ;; only the alias name / field name
                                               (for [[k v] c]
                                                 {(keyword (str (ut/unkeyword k) "-field"))
                                                    (let [calc?      (not (nil? (:derived_name (lookup-field-data-map (str v)))))
                                                          base-field (:field_name (lookup-field-data-map (str v)))]
                                                      (if calc?
                                                        (keyword (:derived_name (lookup-field-data-map (str v))))
                                                        (keyword base-field)))
                                                  (keyword (str "panel-key/" (ut/unkeyword k) "-field")) ;; for
                                                    (let [calc?      (not (nil? (:derived_name (lookup-field-data-map (str v)))))
                                                          base-field (:field_name (lookup-field-data-map (str v)))]
                                                      (if calc?
                                                        (keyword (str "panel-key/"
                                                                      (:derived_name (lookup-field-data-map (str v)))))
                                                        (keyword (str "panel-key/" base-field))))}))
                                         field-replacements)
                                query-replacement-map3
                                  (merge (into {} ;; only the logic (group-by)
                                               (for [[k v] c]
                                                 {(keyword (str (ut/unkeyword k) "-field"))
                                                    (let [calc?      (not (nil? (:derived_name (lookup-field-data-map (str v)))))
                                                          base-field (:field_name (lookup-field-data-map (str v)))]
                                                      (if calc?
                                                        (walk/postwalk-replace {:field (keyword base-field)}
                                                                               (read-string (:derived_calc (lookup-field-data-map
                                                                                                             (str v)))))
                                                        (keyword base-field)))}))
                                         field-replacements)
                                query-map
                                  (vec
                                    (for [q (get-in viz-shape [(rekey shape-name) :sql-maps])]
                                      (let [new-query-map (walk/postwalk-replace
                                                            query-replacement-map2 ;field-replacements
                                                            (merge q
                                                                   {:select   (if true ; (not (empty? (get (hash
                                                                                (walk/postwalk-replace
                                                                                  query-replacement-map
                                                                                  (walk/postwalk-replace
                                                                                    (get aliased-selects3
                                                                                         (.indexOf (get-in viz-shape
                                                                                                           [(rekey shape-name)
                                                                                                            :sql-maps])
                                                                                                   q)
                                                                                         {}) ;; do custom
                                                                                             ;; aliases
                                                                                    (get q :select {})))
                                                                                (walk/postwalk-replace query-replacement-map
                                                                                                       (get q :select {})))
                                                                    :vselect  (walk/postwalk-replace query-replacement-map2
                                                                                                     (get q :vselect {}))
                                                                    :group-by (walk/postwalk-replace query-replacement-map3
                                                                                                     (get q :group-by {}))}))
                                            new-query-map (if (empty? (get new-query-map :vselect))
                                                            (dissoc new-query-map :vselect)
                                                            new-query-map)
                                            new-query-map (if (empty? (get new-query-map :group-by))
                                                            (dissoc new-query-map :group-by)
                                                            new-query-map)
                                            new-query-map (if (empty? (get new-query-map :select))
                                                            (dissoc new-query-map :select)
                                                            new-query-map)]
                                        new-query-map))) ;; empty vselect or group-by is no
                                condis (get-in viz-shape [(rekey shape-name) :conditionals])
                                h (get-in viz-shape [(rekey shape-name) :h])
                                w (get-in viz-shape [(rekey shape-name) :w])
                                viz-map (walk/postwalk-replace query-replacement-map2
                                                               (get-in viz-shape [(rekey shape-name) :library-shapes]))
                                combo-hash (hash [shape-name (pr-str c) ctx connection_id])
                                combo-map
                                  {:context-hash    ctx
                                   :connection-id   connection_id
                                   :combo-hash      combo-hash
                                   :shape-name      shape-name
                                   :table-name      (cstr/replace table-name #"-" "_")
                                   :conditionals    (str condis)
                                   :query-map       (str query-map)
                                   :query-map-str   (to-sql (first query-map) sample-key-vec system-db)
                                   :viz-map         (str viz-map)
                                   :key-hashes      (pr-str (into {} (sort-by key c)))
                                   :key-hashes-hash (str (hash (pr-str (into {} (sort-by key c)))))
                                   :h               h
                                   :w               w
                                   :selected-view   (str (get-in viz-shape [(rekey shape-name) :selected-view]))
                                   :combo-edn       (pr-str (cstr/join
                                                              ", "
                                                              (sort
                                                                (for [v (vals c)]
                                                                  (let [calc?      (not (nil? (:derived_name
                                                                                                (lookup-field-data-map (str v)))))
                                                                        calc-field (:derived_name (lookup-field-data-map (str v)))
                                                                        base-field (:field_name (lookup-field-data-map (str v)))]
                                                                    (if calc? (str calc-field "*") base-field))))))}
                                cid (hash combo-map)
                                sel-recos (filter #(= (get % :connection_id) connection_id) @em/selected-recos)
                                calc-score0 (fn [k v]
                                              (let [cache (get @score-atom0 [k v])]
                                                (if cache
                                                  cache
                                                  (let [res (for [_ (filter #(and (= (get % :shape_name) shape-name)
                                                                                  (= (get % :field_name)
                                                                                     (:field_name (lookup-field-data-map (str
                                                                                                                           v))))
                                                                                  (= (get % :axes_key) k))
                                                                      sel-recos)]
                                                              0.8)]
                                                    (swap! score-atom0 assoc [k v] res)
                                                    res))))
                                calc-score1 (fn [k v]
                                              (let [cache (get @score-atom1 [k v])]
                                                (if cache
                                                  cache
                                                  (let [res (for [_ (filter #(and (= (get % :field_name)
                                                                                     (:field_name (lookup-field-data-map (str
                                                                                                                           v))))
                                                                                  (= (get % :axes_key) k))
                                                                      sel-recos)]
                                                              0.75)]
                                                    (swap! score-atom1 assoc [k v] res)
                                                    res))))
                                calc-score2 (fn [k v]
                                              (let [cache (get @score-atom2 [k v])]
                                                (if cache
                                                  cache
                                                  (let [res (for [_ (filter #(and (= (get % :field_name)
                                                                                     (:field_name (lookup-field-data-map (str
                                                                                                                           v)))))
                                                                      sel-recos)]
                                                              0.45)]
                                                    (swap! score-atom2 assoc [k v] res)
                                                    res))))
                                score0 (apply + (flatten (for [[k v] c] (calc-score0 k v))))
                                score1 (apply + (flatten (for [[k v] c] (calc-score1 k v))))
                                score2 (apply + (flatten (for [[k v] c] (calc-score2 k v))))
                                score (+ score0 score1 score2)
                                combo-map (-> combo-map
                                              (assoc :uuid cid)
                                              (assoc :score score))]
                            (let [combo-rows
                                    (into []
                                          (for [[k v] c]
                                            (let [talias (keyword (str (ut/unkeyword k) "-field"))]
                                              (->
                                                (merge (merge
                                                         parent-keys
                                                         {:context-hash ctx :combo-hash (str combo-hash) :shape-name shape-name})
                                                       (select-keys (lookup-field-data-map (str v))
                                                                    [:field_name :field_type :data_type :key_hash :derived_calc
                                                                     :derived_name]))
                                                (assoc :axes k)
                                                (assoc :talias (pr-str talias))
                                                (assoc :score0 (apply + (flatten (calc-score0 k v))))
                                                (assoc :score1 (apply + (flatten (calc-score1 k v))))
                                                (assoc :score2 (apply + (flatten (calc-score2 k v))))
                                                (assoc :walk1 (pr-str (select-keys query-replacement-map [talias])))
                                                (assoc :walk2 (pr-str (select-keys query-replacement-map2 [talias])))
                                                (assoc :walk3 (pr-str (select-keys query-replacement-map3 [talias])))
                                                (assoc :walka (pr-str (into {}
                                                                            (for [[qidx qq] aliased-selects3]
                                                                              {qidx (into {}
                                                                                          (filter (fn [[k _]]
                                                                                                    (or (= k talias)
                                                                                                        (try (= (first k) talias)
                                                                                                             (catch Exception _
                                                                                                               false))))
                                                                                            qq))}))))))))]
                              (sql-exec system-db (to-sql {:insert-into [:combo-rows] :values combo-rows})))
                            (sql-exec system-db (to-sql {:insert-into [:combos] :values [combo-map]})))
                          (catch Exception e
                            (let [sw (java.io.StringWriter.)
                                  pw (java.io.PrintWriter. sw)]
                              (.printStackTrace e pw)
                              (ut/pp [:find-viz-inner (str e) (.toString sw)]))))))))))))))))

(defn insert-current-rules!
  [system-db connection_id this-run-id sniff-tests-map field-attributes-map derived-field-map viz-shape-map flow-functions-map]
  (sql-exec system-db (to-sql {:delete-from [:rule_maps_tests] :where [:= :connection_id connection_id]}))
  (doseq [[rk r] sniff-tests-map]
    (sql-exec system-db
              (to-sql {:insert-into [:rule_maps_tests]
                       :columns     [:connection_id :run_id :test_name :sql_map :when_logic :fetch_one]
                       :values      [[connection_id this-run-id (ut/unkeyword rk) (str (get r :sql-map)) (str (get r :when))
                                      (str (get r :fetch-one?))]]})))
  (sql-exec system-db (to-sql {:delete-from [:rule_maps_attributes] :where [:= :connection_id connection_id]}))
  (doseq [[rk r] field-attributes-map]
    (sql-exec system-db
              (to-sql {:insert-into [:rule_maps_attributes]
                       :columns     [:connection_id :run_id :attribute_name :when_logic]
                       :values      [[connection_id this-run-id (ut/unkeyword rk) (str (get r :when))]]})))
  (sql-exec system-db (to-sql {:delete-from [:rule_maps_derived_fields] :where [:= :connection_id connection_id]}))
  (doseq [[rk r] derived-field-map]
    (sql-exec system-db
              (to-sql {:insert-into [:rule_maps_derived_fields]
                       :columns     [:connection_id :run_id :field_name :when_logic :calc_logic]
                       :values      [[connection_id this-run-id (ut/unkeyword rk) (str (get r :when)) (str (get r :calc))]]})))
  (sql-exec system-db (to-sql {:delete-from [:rule_maps_viz_shapes] :where [:= :connection_id connection_id]}))
  (doseq [[rk r] viz-shape-map]
    (sql-exec system-db
              (to-sql {:insert-into [:rule_maps_viz_shapes]
                       :columns     [:connection_id :run_id :shape_name :axes_logic :sql_maps :library_shapes :base_score
                                     :selected_view]
                       :values      [[connection_id this-run-id (ut/unkeyword rk) (str (get r :axes)) (str (get r :sql-maps))
                                      (str (get r :library-shapes)) (get r :base-score) (str (get r :selected-view))]]})))
  (sql-exec system-db (to-sql {:delete-from [:flow_functions] :where [:= :connection_id connection_id]}))
  (doseq [{:keys [category name description file-path inputs icon types]} (ut/flatten-map flow-functions-map)]
    (sql-exec system-db
              (to-sql {:insert-into [:flow_functions]
                       :columns     [:connection_id :run_id :category :name :full_map :description :inputs :icon :input_types
                                     :output_types :file_path]
                       :values      [[connection_id this-run-id (str category) (str name)
                                      (str (get-in flow-functions-map [category name])) ;; full
                                      (str description) (str inputs) (str icon) (str (vec (vals (dissoc types :out))))
                                      (str (get types :out)) (str file-path)]]}))))


(defn lets-give-it-a-whirl
  [f-path target-db system-db sniff-tests-map field-attributes-map derived-field-map viz-shape-map & sql-filter]
  (doall
    (try
      (let [connect-meta      (surveyor/jdbc-connect-meta target-db f-path)
            orig-conn         (if (cstr/ends-with? (cstr/lower-case f-path) "edn")
                                (edn/read-string (slurp f-path))
                                target-db ;; in order to not disrupt hardcoded cache.db and
                              )
            connection_id     (get connect-meta :connection_id)
            this-run-id       (get-latest-run-id system-db connect-meta) ;; for result-hash?
            field-vectors-all (create-target-field-vectors target-db connect-meta) ;; this is a
            sys-schemas?      (fn [v]
                                (let [catalog-name (str (nth v 4))
                                      schema-name  (str (nth v 3))
                                      table-name   (str (nth v 5))]
                                  (or (= "information_schema" (cstr/lower-case schema-name))
                                      (= "information_schema" (cstr/lower-case catalog-name))
                                      (cstr/includes? (cstr/lower-case schema-name) "sys")
                                      (cstr/includes? (cstr/lower-case catalog-name) "sys")
                                      (cstr/includes? (cstr/lower-case table-name) "$") ;; no $
                                      (= "system" (cstr/lower-case schema-name)))))
            field-vectors-all (vec (remove sys-schemas? field-vectors-all))
            field-vectors     (if (and sql-filter ;; filter them out of field-vectors-all ...
                                       #_{:clj-kondo/ignore [:not-empty?]}
                                       (not (empty? sql-filter)))
                                (let [col-names       [:db-type :connection-id :table-type :db-schema :db-catalog :table-name
                                                       :field-name :field-type :data-type]
                                      full-sql-filter {:select col-names :from :rowset :where sql-filter}]
                                  (rowset-sql-query field-vectors-all full-sql-filter col-names))
                                field-vectors-all)]
        (insert-current-rules! system-db
                               connection_id
                               this-run-id
                               sniff-tests-map
                               field-attributes-map
                               derived-field-map
                               viz-shape-map
                               default-flow-functions)
        (insert-field-vectors! field-vectors system-db sql-filter)
        (insert-connection-data! orig-conn ; target-db ;; getting original connection map
                                 system-db
                                 this-run-id
                                 connect-meta
                                 sql-filter)
        (ut/pp [:processing-field-vectors (count field-vectors)])
        (dorun (for [f field-vectors] ;(partition-all 10 field-vectors)]
                 (tp/add-task task-pool
                              (process-field-vectors system-db
                                                     target-db
                                                     [f]
                                                     this-run-id
                                                     sniff-tests-map
                                                     field-attributes-map
                                                     derived-field-map))))
        (if (and sql-filter #_{:clj-kondo/ignore [:not-empty?]} (not (empty? sql-filter)))
          (find-all-viz-fields! system-db viz-shape-map connection_id [:and [:= :connection-id connection_id] (first sql-filter)])
          (find-all-viz-fields! system-db viz-shape-map connection_id [:= :connection-id connection_id]))
        (if (and sql-filter #_{:clj-kondo/ignore [:not-empty?]} (not (empty? sql-filter)))
          (find-all-viz-combos! system-db viz-shape-map connection_id [:and [:= :connection-id connection_id] (first sql-filter)])
          (find-all-viz-combos! system-db viz-shape-map connection_id [:= :connection-id connection_id]))
        (update-connection-data! system-db this-run-id connection_id))
      (catch Exception e
        (let [sw (java.io.StringWriter.)
              pw (java.io.PrintWriter. sw)]
          (.printStackTrace e pw)
          (ut/pp [:error-running-whirl-full f-path sql-filter target-db (str e) (.toString sw)]))))))

(defn lets-give-it-a-whirl-no-viz
  [f-path target-db system-db _ _ _ _ & sql-filter]
  (doall
    (try
      (let [connect-meta      (surveyor/jdbc-connect-meta target-db f-path)
            orig-conn         (if (cstr/ends-with? (cstr/lower-case f-path) "edn") (edn/read-string (slurp f-path)) target-db) 
            connection_id     (get connect-meta :connection_id)
            this-run-id       (get-latest-run-id system-db connect-meta)
            field-vectors-all (create-target-field-vectors target-db connect-meta) ;; this is a
            sys-schemas?      (fn [v]
                                (let [catalog-name (str (nth v 4))
                                      schema-name  (str (nth v 3))
                                      table-name   (str (nth v 5))]
                                  (or (= "information_schema" (cstr/lower-case schema-name))
                                      (= "information_schema" (cstr/lower-case catalog-name))
                                      (cstr/includes? (cstr/lower-case schema-name) "sys")
                                      (cstr/includes? (cstr/lower-case catalog-name) "sys")
                                      (cstr/includes? (cstr/lower-case table-name) "$") ;; no $
                                      (= "system" (cstr/lower-case schema-name)))))
            field-vectors-all (vec (remove sys-schemas? field-vectors-all))
            field-vectors     (if (and sql-filter ;; filter them out of field-vectors-all ...
                                       #_{:clj-kondo/ignore [:not-empty?]}
                                       (not (empty? sql-filter)))
                                (let [col-names       [:db-type :connection-id :table-type :db-schema :db-catalog :table-name
                                                       :field-name :field-type :data-type]
                                      full-sql-filter {:select col-names :from :rowset :where sql-filter}]
                                  (rowset-sql-query field-vectors-all full-sql-filter col-names))
                                field-vectors-all)]
        (insert-field-vectors! field-vectors system-db sql-filter)
        (insert-connection-data! orig-conn ; target-db ;; getting original connection map
                                 system-db
                                 this-run-id
                                 connect-meta
                                 sql-filter)
        (update-connection-data! system-db this-run-id connection_id))
      (catch Exception e
        (let [sw (java.io.StringWriter.)
              pw (java.io.PrintWriter. sw)]
          (.printStackTrace e pw)
          (ut/pp [:error-running-whirl f-path sql-filter target-db (str e) (.toString sw)])
          (insert-error-row! target-db (str (str e) (.toString sw)) {:sql-filter sql-filter}))))))

(defn insert-rowset
  [rowset table-name db-conn & columns-vec] ;; special version for captured sniff
  (let [rowset-type     (cond (and (map? (first rowset)) (vector? rowset))       :rowset
                              (and (not (map? (first rowset))) (vector? rowset)) :vectors)
        columns-vec-arg (first columns-vec)
        rowset-fixed    (if (= rowset-type :vectors) (vec (for [r rowset] (zipmap columns-vec-arg r))) rowset)
        columns         (keys (first rowset-fixed))
        values          (vec (for [r rowset-fixed] (vals r)))
        table-name-str  (ut/unkeyword table-name)
        ddl-str         (sqlite-ddl/create-attribute-sample table-name-str rowset-fixed)
        insert-sql      (to-sql {:insert-into [table-name] :columns columns :values values})
        extra           [ddl-str columns-vec-arg table-name table-name-str]]
    (sql-exec db-conn (str "drop table if exists " table-name-str " ; ") extra)
    (sql-exec db-conn ddl-str extra)
    (sql-exec db-conn insert-sql extra)))

(def sniff-cache (atom {}))



(def tmp-db-src1
  {:datasource @(pool-create {;:jdbc-url
                              :jdbc-url    "jdbc:sqlite:file:tmpsniffdb1?mode=memory&cache=shared&auto_vacuum=FULL"
                              :auto_vacuum "FULL"
                              :cache       "shared"}
                             "tmp-db-src1")})

(def dest
  {:datasource @(pool-create {;:jdbc-url
                              :jdbc-url    "jdbc:sqlite:file:sniffdbdb2?mode=memory&cache=shared&auto_vacuum=FULL"
                              :auto_vacuum "FULL"
                              :cache       "shared"}
                             "tmp-db-dest")})

(create-sqlite-sys-tables-if-needed! dest)

(def sniffs (atom 0))


(defn captured-sniff
  [src-conn-id base-conn-id target-db src-conn result-hash & [sql-filter quick? resultset]]
  (swap! sniffs inc)
  (doall
    (let [res?     (not (nil? resultset))
          cols     (keys (first resultset))
          src-conn (if res? tmp-db-src1 src-conn)]
      (when res? (insert-rowset resultset (last sql-filter) src-conn cols)) ;; if passed
      ((if quick? lets-give-it-a-whirl-no-viz lets-give-it-a-whirl)
        src-conn-id
        src-conn
        dest
        default-sniff-tests
        default-field-attributes
        default-derived-fields
        default-viz-shapes
        sql-filter) ;[:= :table-name "viz_recos_vw"]
      (let [sniff-rows (sql/fetch-all-tables dest quick?)
            sniff-rows (-> sniff-rows
                           (dissoc :rules_maps_attributes)
                           (dissoc :rules_maps_derived_fields)
                           (dissoc :rules_maps_tests)
                           (dissoc :rules_maps_viz_shapes)
                           (dissoc :status))
            sniff-rows (dissoc sniff-rows :connections)]
        (sql/insert-all-tables sniff-rows (last sql-filter))
        (doseq [k (keys sniff-rows)] (sql-exec dest (to-sql {:delete-from [k]})))
        (when res? (sql-exec src-conn (to-sql {:drop-table [(last sql-filter)]}))) ;)
        nil))))
















(def mem-db-sqlite2 "jdbc:sqlite::memory:?cache=shared") ;"jdbc:sqlite::memory:"

(def mem-db-sqlite3 {:classname "org.sqlite.JDBC" :subprotocol "sqlite" :cache "shared" :subname ":memory:"})

(def mem-db-sqlite
  {:classname "org.sqlite.JDBC" :subprotocol "sqlite" :subname "db/query-cache.db?busy_timeout=20000&cache=shared"})

(def mem-db-mysql "jdbc:mysql://root:mysqlpw@localhost:3306")

(def mem-db-vertica ;"jdbc:vertica://dbadmin@localhost:5433/VMart")
  {:classname   "com.vertica.jdbc.Driver" ;; "org.clojars.prepor.Driver"
   :subprotocol "vertica"
   :subname     "_VMart" ; first char gets cut somehow?
   :user        "dbadmin"
   :host        "localhost" ;"0.0.0.0" ;"172.27.144.1" ;"localhost"
   :port        5433})

(def mem-db-pgsql "jdbc:postgresql://postgres:postgrespw@localhost:5555/postgres")

(def mem-db-clickhouse ;"jdbc:clickhouse://localhost:8123/test")
  {:classname "ru.yandex.clickhouse.ClickHouseDriver" :subprotocol "clickhouse" :subname "//localhost:8123/default"})


