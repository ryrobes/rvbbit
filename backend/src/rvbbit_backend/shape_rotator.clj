(ns rvbbit-backend.shape-rotator
  (:require
   [clojure.java.jdbc         :as jdbc]
   [clojure.core.async        :as async]
   [clojure.edn               :as edn]
   [clojure.set               :as cset]
   [hikari-cp.core            :as hik]
   [honey.sql                 :as honey]
   [clojure.string            :as cstr]
   [clojure.walk              :as walk]
   [rvbbit-backend.evaluator  :as evl]
   [rvbbit-backend.util       :as ut]
   [rvbbit-backend.surveyor   :as surveyor]
   [rvbbit-backend.pool-party :as ppy]
  ;;  [rvbbit-backend.cruiser    :as cruiser]
   [datalevin.core            :as d]
   [clojure.set               :as set]
   [rvbbit-backend.sql        :as sql
    :refer [sql-query sql-exec ghost-db to-sql pool-create system-db system-db systemh2-db]]
   [clojure.math.combinatorics :as combo]
   [rvbbit-backend.db :as db])
  (:import
   [java.util Date UUID]))

;; 11/3/24 cruiser/viz-recos/sql-meta rewritten with EDN/atoms and the rabbit reactor... (with DB use later if needed)

;; read connections
;; surveyor/jdbc-connect-meta - get fields, tables map

;; [need the ability to run an entire DB of tables OR a single query]

;; for each table / OR query
;; create-target-field vectors (analyze each field)
;; flattening the metadata for processing (into a flat sql form)
;; sep process for Duck since it doesn't support legit jdbc meta (instead query sys tables)
;; flat vectors w meta - each is a field with all data parts
;; remove system schemas depending on the DB type "information_schema" etc.
;; add more enriching data - like "is-group-by?" useful for analysis - by looking at the query, if query.

;; insert connection data - simple meta, linked to tables and queries

;; -- extra for viz reco

;; for each field vector

;; -- base shapes -- (FKA sniff tests)
;; create sniff tests SQL - give new name (shapesx)
;; run sniff tests
;; insert sniff test results - do NOT worry about sample data

;; -- compound shapes -- (FKA attributes)
;; get attribute flag results
;; insert attribute flag results

;; -- derived fields --
;; get possible derived fields
;; taste test derived fields
;; - send through to sniff -> attribute processes as new "fields"

;; -- shape rotation  --
;; find all viz fields
;; looks at viz shape defs, finds all applicable fields based on attribs

;; find all viz combos
;; based on the attrib data, cartesian join of all possible combos
;; does scoring based on how often used, etc

;;;; old runstream ;;;;
 ;(cruiser/lets-give-it-a-whirl-no-viz
 ; f-path - connection file path on disk
 ;  conn  - connection file hikari db pool
 ;  system-db - target - deprecated.
 ; cruiser/default-sniff-tests       shapes
 ; cruiser/default-field-attributes  compound-shapes
 ; cruiser/default-derived-fields    derived-fields
 ; cruiser/default-viz-shapes)       complete-shapes


(defonce master-metadata (atom {})) ;; combine queries and tables?
(defonce field-maps (atom {}))
(defonce field-maps-cache (atom {}))
(defonce viz-maps-cache (atom {}))
(defonce honey-hashes (atom {}))

(defn pmapv [f coll]
  ;(vec (pmap f coll))
  (mapv f coll)
  )

(defn db-type-from-connection-id [connection-id-str]
  (let [pool-map (get @db/conn-map connection-id-str)]
    (surveyor/db-typer pool-map)))

;; (ut/pp (db-type-from-connection-id "xtdb-pg")) ;; "PostgreSQL"

(defn general-data-type-infer [field-type]
  (let [ft (cstr/lower-case field-type)]
    (cond
      (cstr/includes? ft "json")    "rabbit-code"
      (cstr/includes? ft "map")    "rabbit-code"
      (cstr/includes? ft "struct")    "rabbit-code"
      (cstr/includes? ft "char")    "string"
      (cstr/includes? ft "string")  "string"
      (cstr/includes? ft "lob")     "string"
      (cstr/includes? ft "text")    "string"
      (cstr/includes? ft "time")    "datetime"
      (cstr/includes? ft "date")    "date"
      (cstr/includes? ft "bool")    "boolean"
      (cstr/includes? ft "int")     "integer"
      (cstr/includes? ft "serial")     "integer"
      (cstr/includes? ft "real")    "float"
      (cstr/includes? ft "float")   "float"
      (cstr/includes? ft "decimal") "float"
      (cstr/includes? ft "num")     "integer"
      :else                         "unknown")))

(defn general-data-type-value [v] ;; deprecated taster
  (cond (string? v)  "string"
        (integer? v) "integer"
        (float? v)   "float"
        (boolean? v) "boolean"
        :else        "unknown"))

(defn path-to-connection-id [f-path]
  (let [conn-id          (str (cstr/replace (cstr/lower-case (last (cstr/split f-path #"/"))) ".edn" ""))
        conn-id          (if (cstr/ends-with? (cstr/lower-case conn-id) ".csv") "imported" conn-id)]
    conn-id))

;; (ut/pp (path-to-connection-id "bigfoot-ufos"))

(defn get-jdbc-db-meta [db-conn f-path]
  (let [user-name        (jdbc/with-db-metadata [m db-conn] (jdbc/metadata-result (.getUserName m)))
        database-name    (jdbc/with-db-metadata [m db-conn] (jdbc/metadata-result (.getDatabaseProductName m)))
        database-version (jdbc/with-db-metadata [m db-conn] (jdbc/metadata-result (.getDatabaseProductVersion m)))
        connection-str   (jdbc/with-db-metadata [m db-conn] (jdbc/metadata-result (.getURL m)))]
    {:user-name        user-name
     :database-name    database-name
     :database-version database-version
     :connection-id    (path-to-connection-id f-path)
     :connection-str   connection-str}))

(declare make-error-box)

(defn create-pool [f-path]
  (let [conn-name (path-to-connection-id f-path) ;;(str (cstr/replace (cstr/lower-case (last (cstr/split f-path #"/"))) ".edn" ""))
        conn      (cond (and (string? f-path)  (cstr/includes? (str f-path) "system")) system-db
                        :else (edn/read-string (slurp f-path)))
        poolable? (try (and (map? conn) (find conn :jdbc-url)) (catch Exception _ false))
        conn      (if poolable?
                    (try {:pre-filter-fn (get conn :pre-filter-fn)
                          :datasource @(pool-create conn (str conn-name "-pool"))}
                         (catch Exception e (do
                                              ;;(ut/pp [:connection-error conn-name e])
                                              (println (make-error-box conn-name e "CONNECTION"))
                                              {:datasource nil})))
                    conn)]
    (swap! db/conn-map assoc conn-name conn)
    conn))

;; (ut/pp [:gg @db/conn-map])

(defn get-or-create-db-pool [f-path]
  (try
    (let [conn-name (str (cstr/replace (cstr/lower-case (last (cstr/split f-path #"/"))) ".edn" ""))
          f-path (cstr/replace (str f-path) "cache.db" "cache-db") ;; for some legacy queries
          conn (get @db/conn-map conn-name (get @db/conn-map f-path))]
      (if conn conn
          (create-pool f-path)))
    (catch Exception e (ut/pp [:shape-rotator :get-or-create-db-pool :f-path f-path (str e)]))))

(defn flatten-jdbc-conn-meta [connect-meta db-meta]
  (let [;db-meta (jdbc-connect-meta connect-meta)
        conn-type      (get connect-meta :database-name)
        conn-id        (get connect-meta :connection-id)
        db-meta-with-* (vec (for [t db-meta] ;; adding a * so table-level granularity sniffs
                              (assoc t :fields (merge (:fields t) {:column_name "*" :type_name "special"}))))]
    ;; (ut/ppln db-meta-with-*)
    (vec (flatten (for [table-map db-meta-with-*]
                    (for [field (:fields table-map)]
                      {[conn-type conn-id (:table_type table-map)
                        (if (nil? (:table_schem table-map)) "none" (:table_schem table-map)) (:table_cat table-map)
                        (:table_name table-map) (:column_name field)]
                       {:column_type (:type_name field)}}))))))

(defn get-jdbc-conn-meta [db-conn]
  (into [] (for [t (vec (jdbc/with-db-metadata [m db-conn]
                          (jdbc/metadata-result (.getTables m
                                                            nil ;; catalog pattern
                                                            nil ;; schema pattern
                                                            nil ;; table name
                                                            (into-array ["TABLE" "VIEW"])))))]
             (merge t {:fields (for [f (jdbc/with-db-metadata [m db-conn]
                                         (jdbc/metadata-result (.getColumns
                                                                m
                                                                (:table_cat t)
                                                                (:table_schem t)
                                                                (:table_name t)
                                                                nil ;; field pattern match
                                                                )))]
                                 f)}))))

(defn get-query-meta-like-table [db-conn query & [query-name]]
  (jdbc/with-db-connection [conn db-conn]
    (with-open [stmt (jdbc/prepare-statement
                      (:connection conn)
                      query
                      {:result-set-type :forward-only
                       :result-set-concurrency :read-only})]
      (let [rs (.executeQuery stmt)
            meta (.getMetaData rs)]
        (into []
              (for [i (range 1 (inc (.getColumnCount meta)))
                    :let [schem (.getSchemaName meta i)
                          ccat (.getCatalogName meta i)
                          parent-table (.getTableName meta i)]]
                {:field-name (.getColumnName meta i)
                 :table-type "QUERY"
                 :honey-hash (hash query)
                 :table-name (cstr/replace (str (or query-name (str "query-" (hash query)))) ":" "")
                 :field-type (.getColumnTypeName meta i)
                 :parent-table-name parent-table
                 :data-type (general-data-type-infer (.getColumnTypeName meta i))
                 :db-schema (if (empty? schem) "none" schem)
                 :db-catalog (if (= ccat parent-table) nil ccat)}))))))

(defn flat-db-meta [db connect-meta]
  (let [h2?   (= (get connect-meta :database-name) "H2")
        duck? (= (get connect-meta :database-name) "DuckDB") ;; doesnt support jdbc meta
        ;;_ (ut/pp [:flat-meta-in h2? duck? connect-meta])
        fm (cond
             duck?
             (let [rrows (sql-query db (to-sql {:select [:table_name :column_name :data_type]
                                                :from [:information_schema.columns]
                                                :order-by [:table_name :column_name]} "DuckDB"))
                   tables (vec (distinct (map :table_name rrows)))
                   conn-id (str (get connect-meta :connection_id))
                   table-rows (into {} (for [t tables] {["DuckDB" conn-id "TABLE" "none" nil t "*"]
                                                        {:column_type "special"}}))
                   flat (into {} (for [r rrows] {["DuckDB" conn-id "TABLE" "none" nil (get r :table_name) (get r :column_name)]
                                                 {:column_type (get r :data_type)}}))]
               (merge table-rows flat))

             h2? (let [rrows (sql-query db
                                        "SELECT TABLE_NAME as table_name, COLUMN_NAME as column_name, DATA_TYPE as data_type
                                         FROM INFORMATION_SCHEMA.COLUMNS
                                         WHERE TABLE_SCHEMA NOT IN ('INFORMATION_SCHEMA', 'sys')
                                         AND TABLE_NAME NOT LIKE 'PG_%'")
                       tables (vec (distinct (map :table_name rrows)))
                       conn-id (str (get connect-meta :connection_id))
                       table-rows (into {} (for [t tables] {["H2" conn-id "TABLE" "none" nil t "*"]
                                                            {:column_type "special"}}))
                       flat (into {} (for [r rrows] {["H2" conn-id "TABLE" "none" nil (get r :table_name) (get r :column_name)]
                                                     {:column_type (get r :data_type)}}))]
                   (merge table-rows flat))
             :else (into {} (surveyor/flatten-jdbc-conn-meta connect-meta (surveyor/get-jdbc-conn-meta db))))]
    ;; (ut/pp [:fm fm])
    fm))

;; (ut/pp (get-query-meta-like-table (get-or-create-db-pool "connections/bigfoot-ufos.edn") "select * from bigfoot_sightings"))

(defn create-target-field-vectors [target-db connect-meta]
  (let [flat-meta     (flat-db-meta target-db connect-meta)
        ;;_ (ut/pp [:flat-meta flat-meta])
        field-vectors (vec (for [[k v] flat-meta]
                             (let [db-col-type        (get v :column_type)
                                   general-data-infer (general-data-type-infer db-col-type)]
                               (conj (conj k db-col-type) general-data-infer))))
        ;; fields (into [] (sort (map #(keyword (get % 5)) field-vectors)))
        keys [:db-type :connection-id :table-type :db-schema :db-catalog :table-name :field-name :field-type :data-type]
        field-vectors (pmapv (partial zipmap keys) field-vectors)
        field-maps (pmapv #(merge connect-meta %) field-vectors)
        grouped-temp (group-by (juxt :connection-id :table-name :table-type) field-maps)
        un-grouped (vec (apply concat
                               (for [[_ v] grouped-temp
                                     :let [fields (into [] (sort-by str (map #(keyword (get % :field-name)) v)))
                                           table (get (first v) :table-name)
                                           false-field (-> (get v 0)
                                                           (assoc :field-name "rrows")
                                                           (assoc :field-type "INTEGER")
                                                           (assoc :data-type "integer"))
                                           implied-query {:select fields :from [[(keyword table) :suball1]]}
                                           honey-hash (hash implied-query)
                                           _ (swap! honey-hashes assoc honey-hash implied-query)]]
                                 (pmapv #(assoc % :honey-hash honey-hash) (conj v false-field)))))]
    un-grouped))

(defn create-query-field-vectors [target-db connect-meta query & [query-name]]
  (let [field-vectors (get-query-meta-like-table target-db query query-name)
        honey-hash (hash query)
        false-field (-> (get field-vectors 0)
                        (assoc :field-name "rrows")
                        (assoc :field-type "INTEGER")
                        (assoc :data-type "integer"))
        field-vectors (conj field-vectors false-field)
        _ (swap! honey-hashes assoc honey-hash query)]
    (pmapv #(merge {:honey-hash honey-hash} connect-meta %) field-vectors)))

;; (def memoized-sql-query
;;   (memoize
;;    (fn [db sql & [extra]]
;;      (sql-query db sql {:extra extra :no-error? true}))))

(def sql-query-cache (atom {}))

(defn cached-sql-query [db sql & [extra]]
  (let [cache-key (pr-str db sql extra)]
    (if-let [cached-result (@sql-query-cache cache-key)]
      cached-result
      (let [result (sql-query db sql {:extra extra :no-error? true})]
        (swap! sql-query-cache assoc cache-key result)
        result))))

(defn slread
  [file]
  (try (edn/read-string (slurp file))
       (catch Exception _ {})))

;; (def shape-tests (slread "defs/sniff-tests.edn"))

;; (ut/pp shape-tests)



;query-runstream
;[kind ui-keypath honey-sql client-cache? sniff? connection-id client-name page panel-key clover-sql deep-meta? snapshot-cache?]

(defn add-shape-tests [fields-map-vector conn shape-test-defs & [base-query]]
        (vec (for [field-map fields-map-vector]
               (merge field-map (into {} (for [[shape-key {:keys [sql-map when fetch-one?] :or {fetch-one? true}}] shape-test-defs]
                                           (let [when-where (walk/postwalk-replace field-map when)
                                                 sql-map (if (vector? sql-map)  {:select sql-map :from [:table]} sql-map) ;; for lazy ass EDN
                                                 fname (get field-map :field-name)
                                                 sql-map-resolved (walk/postwalk-replace {:field (if (= fname "rrows") [1 :rrows] (keyword fname))
                                                                                          :table (if base-query
                                                                                                   [(dissoc base-query :order-by) :subq1]
                                                                                                   (keyword (get field-map :table-name)))} sql-map)
                                                 runnable? (= [{:1 1}] (sql-query ghost-db (to-sql {:select [1] :where (or when-where [:= 1 1])}) {:no-error? true}))
                                                 ;query-runstream (resolve 'rvbbit-backend.websockets/query-runstream)
                                                 ;conn-str (get (ut/reverse-map @db/conn-map) conn)
                                                 vval (if runnable?
                                                        (if (= fname "*") ;; for row count
                                                          (sql-query conn (to-sql sql-map-resolved) {:extra [:shape-test shape-key] :no-error? true})
                                                          ;(@query-runstream :honey-xcall [:shape-rotator (hash fields-map-vector)] sql-map-resolved true false conn-str :rvbbit -1 nil sql-map-resolved false false)
                                                          (cached-sql-query conn (to-sql sql-map-resolved))
                                                          ;(@query-runstream :honey-xcall [:shape-rotator (hash fields-map-vector)] sql-map-resolved true false conn-str :rvbbit -1 nil sql-map-resolved false false)
                                                          ) nil)]
                                             {shape-key (if (get-in vval [0 :database_says]) ;; indicates sql error, return nil for now
                                                          nil
                                                          (if fetch-one?
                                                            (first (vals (first vval)))
                                                            (pmapv (ffirst (first vval)) vval)))})))))))

;; (def shape-attributes (slread "defs/field-attributes.edn"))
;; (ut/pp shape-attributes)

(defn add-shape-attributes [fields-map-vector shape-attrib-defs & {:keys [min-iterations max-iterations] :or {min-iterations 3 max-iterations 20}}]
  (loop [current fields-map-vector
         prev nil
         iterations 0]
    (let [next-result
          (vec (for [field-map current]
                 (reduce (fn [acc-map [shape-key where-vector]]
                           (let [where-vector (walk/postwalk-replace acc-map where-vector)
                                 is-true? (= [{:1 1}]
                                             (sql-query ghost-db
                                                                 (to-sql {:select [1]
                                                                          :where (or where-vector [:= 1 1])}) {:no-error? true}))]
                             (assoc acc-map shape-key is-true?)))
                         field-map  ; Start with existing attributes
                         shape-attrib-defs)))

        ;;   changes (remove nil?
        ;;                   (map #(when (not= %1 %2)
        ;;                           {:from (select-keys %1 (keys shape-attrib-defs))
        ;;                            :to (select-keys %2 (keys shape-attrib-defs))})
        ;;                        current
        ;;                        next-result))

        ;;   _ (ut/pp [:shape-attributes-iteration iterations
        ;;             :changes-count (count changes)
        ;;             ;;:sample-changes (take 2 changes)
        ;;             ])

          done? (and (>= iterations min-iterations)
                     (or (= current next-result)
                         (= prev next-result)
                         (>= iterations max-iterations)))]

      (if done?
        (do
        ;;   (ut/pp [:shape-attributes-complete
        ;;           :iterations iterations
        ;;           :reason (cond
        ;;                     (= current next-result) "no changes"
        ;;                     (= prev next-result) "oscillating"
        ;;                     :else "max iterations")])
          next-result)
        (recur next-result current (inc iterations))))))

;(def viz-shapes (slread "defs/viz-shapes-only-recharts.edn"))
(def viz-shapes @db/shapes-map) ;; (slread "defs/viz-shapes.edn"))

(defn find-viz-fields [vector-group viz-shapes-defs]
  (let [vector-group (filterv #(not= (get % :field-name) "*") vector-group)] ;; take out special table level fields
    (vec (apply concat
                (for [[shape-name {:keys [axes score category]}] viz-shapes-defs]
                  (apply concat
                         (for [[axis-name logic] axes]
                           (for [field-map vector-group
                                 :let [walked-map (walk/postwalk-replace field-map logic)
                                       res (= [{:1 1}] (sql-query ghost-db (to-sql {:select [1] :where walked-map}) {:no-error? true}))]
                                 :when res]
                             [shape-name axis-name (get field-map :field-name) (get field-map :table-name)]))))))))

 (defn get-fields-by-chart-and-axis [found-fields]
   (->> found-fields
        (group-by (juxt first second #(nth % 2)))  ; group by [context shape-name axis-type]
        (map (fn [[[context shape-name axis-type] entries]]
               [[context shape-name axis-type] (set (map #(nth % 3) entries))]))
        (into {})))

(defn create-all-chart-configs [found-fields]
  (let [fields-by-chart-axis (get-fields-by-chart-and-axis found-fields)
        by-context (group-by first found-fields)
        context-charts (map (fn [[context entries]]
                              [context (group-by second entries)])
                            by-context)]
    (vec (mapcat
          (fn [[context chart-groups]]
            (mapcat
             (fn [[shape-name entries]]
               (let [real-required-axes (set (keys (get-in @db/shapes-map [shape-name :axes])))
                     found-axes (set (map #(nth % 2) entries))]
                 ;; Only proceed if we have all required axes
                 (when (set/subset? real-required-axes found-axes)
                   (let [axis-fields (map #(vector % (get fields-by-chart-axis [context shape-name %]))
                                          real-required-axes)
                         axes-vec (vec real-required-axes)]
                     (->> (apply combo/cartesian-product (map second axis-fields))
                          ;; Filter out combinations where the same field is used multiple times
                          (filter #(= (count %) (count (set %))))
                          (map (fn [field-combo]
                                 {:shape-name shape-name
                                  :context context
                                  :axes (zipmap axes-vec field-combo)})))))))
             chart-groups))
          context-charts))))


;; {:from [:bigfoot_sightings] :select [[[:count [:distinct :nearesttown]] :cnt]]}

(defn add-query-attributes [field-maps query]
  (pmapv (fn [m]
          (let [selects               (get query :select)
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
                                        (true? (try (some #(= (keyword (get m :field-name)) %) materialize-group-bys)
                                                    (catch Exception _ false)))
                                        true)]
            (assoc m :is-group-by? is-group-by?))) field-maps))

(defn insert-into-fields [field-maps]
  (let [context-grouped (group-by (fn [m] [(get m :context) (get m :table-type)]) field-maps)]
    (doseq [[[k ttype] field-maps1] context-grouped]
      (let [;;_ (ut/pp ["🦁" :inserting-fields-h2-context! [k ttype] (count field-maps1)])
            _ (sql-exec systemh2-db
                        (to-sql {:delete-from [:fields]
                                 :where [:and
                                         [:= :context (str k)]
                                         [:= :table_type (str ttype)]]}))
            ;fm-hash (hash field-maps1)
            res (walk/postwalk-replace {:is-group-by? :is-group-by
                                        :measure? :is-measure
                                        :dimension? :is-dimension}
                                       (pmapv #(select-keys % [:database-name
                                                              :is-group-by?
                                                              :measure?
                                                              :sum-val
                                                              :dimension?
                                                              :data-type
                                                              :field-type
                                                              ;:example-value
                                                              :connection-str
                                                              :honey-hash
                                                              :total-rows
                                                              :user-name
                                                              :field-name
                                                              :distinct-count
                                                              :db-schema
                                                              :parent-table-name
                                                              :max-val
                                                              :db-catalog
                                                              :database-version
                                                              :context-key
                                                              :context
                                                              :connection-id
                                                              :table-name
                                                              :table-type
                                                              :avg-val
                                                              :min-val])
                                             field-maps1))
            res (pmapv #(-> %
                           (assoc :context (str (get % :context)))
                           (assoc :max-val (let [val (str (get % :max-val))] (if (> (count val) 250) (subs val 0 250) val)))
                           (assoc :min-val (let [val (str (get % :min-val))] (if (> (count val) 250) (subs val 0 250) val)))
                           (assoc :avg-val (let [val (str (get % :avg-val))] (if (> (count val) 250) (subs val 0 250) val)))
                           (assoc :context-key (str (get % :context-key)))
                           (assoc :table-fields (when (= (get % :field-name) "*") (str (vec (remove #{"*"} (mapv :field-name field-maps1))))))
                           (assoc :table-type (str (get % :table-type)))) res)
            parts (partition-all 50 res)
            parts-count (count parts)]
        (doseq [r parts]
          (do ;(ut/pp [:inserting-fields-h2-batch fm-hash parts-count :parts :this (count r)])
              (sql-exec systemh2-db (to-sql {:insert-into [:fields] :values (vec r)}))))))))

(defn get-connection-id [f-path]
  (let [conn (if (map? f-path) f-path (get-or-create-db-pool f-path))
        connection-id (get (cset/map-invert @db/conn-map) conn)]
    connection-id))

(defn get-field-maps [{:keys [f-path shape-test-defs shape-attributes-defs honey-sql client-name query-name filter-fn pre-filter-fn]
                       :or {shape-test-defs @db/sniff-test-map shape-attributes-defs @db/field-attribute-map}
                       :as args}]
  (swap! db/viz-reco-sniffs inc)
  (let [k (hash args)]
    (if-let [cached nil] ;;(get @field-maps-cache k)]
      cached  ; Return cached result if it exists
      (let [result
            (let [conn (if (map? f-path) f-path (get-or-create-db-pool f-path))
                  f-path (if (map? f-path) "" f-path)
                  ;;connection-id (get-connection-id f-path)
                  connection-id (get (cset/map-invert @db/conn-map) conn)
                  ;;_ (swap! db/shape-rotation-status assoc-in [connection-id :started] (System/currentTimeMillis))
                  connect-meta (get-jdbc-db-meta conn f-path)
                  ;;;_ (ut/pp [:connect-meta connection-id connect-meta])
                  field-maps (if (map? honey-sql)
                               (add-query-attributes
                                (create-query-field-vectors conn connect-meta (first (to-sql honey-sql)) query-name) honey-sql)
                               (create-target-field-vectors conn connect-meta))
                  field-maps (pmapv #(assoc % :connection-id connection-id) field-maps)
                  ;; _ (ut/pp [:sniff (first field-maps)])
                  ;;field-maps field-maps ;; filter out system
                  _ (when (nil? honey-sql)
                      (swap! db/shape-rotation-status assoc-in [connection-id :all :fields] (count field-maps))
                      (swap! db/shape-rotation-status assoc-in [connection-id :all :tables] (count (distinct (mapv :table-name field-maps)))))
                  field-maps (into [] (pmapv #(assoc % :table-type (-> (get % :table-type) cstr/lower-case keyword)) field-maps))
                  field-maps (if (list? pre-filter-fn)
                               (try (filterv (eval pre-filter-fn) field-maps)
                                    (catch Exception e (do (ut/pp [:get-fields-pre-filter-fn-error! (str pre-filter-fn) (str e) e {:sample-map (first field-maps)}])
                                                           field-maps)))
                               field-maps)
                  field-maps (mapv (fn [m] (assoc m :db-catalog (if (= (get m :connection-id) (get m :db-catalog)) nil (get m :db-catalog)))) field-maps)
                  ;; ^^ odd workaround for system H2 db metadata strangeness

                  with-shape-tests (add-shape-tests field-maps conn shape-test-defs honey-sql)
                  with-shape-attribs (add-shape-attributes with-shape-tests shape-attributes-defs)
                  context-juxt (juxt :connection-id :db-catalog :db-schema :table-name)
                  field-maps (into []
                                   (pmapv #(-> %
                                              ;(assoc :honey-hash (hash honey-sql))
                                               (assoc :context (context-juxt %))
                                               (assoc :context-key (conj (context-juxt %) (:field-name %))))
                                          (if filter-fn
                                            (filterv filter-fn with-shape-attribs)
                                            with-shape-attribs)))
                  ;; _ (when client-name
                  ;;     (ppy/execute-in-thread-pools
                  ;;      :shape-rotator-to-leaf-atom!
                  ;;      (fn [] (doseq [[kp field-map]
                  ;;                     (group-by (fn [m] [client-name
                  ;;                                        (get m :connection-id)
                  ;;                                        (keyword (get m :table-name))
                  ;;                                        (keyword (get m :field-name))]) field-maps)]
                  ;;               (swap! db/leaf-field-meta-map assoc-in kp field-map)))))
                  _ (ppy/execute-in-thread-pools
                     :shape-rotator-to-h2!
                     (fn [] (insert-into-fields field-maps)))]
              field-maps)]
        ;; (swap! field-maps-cache assoc k result)
        result))))


(defn materialize-viz-shape [field-maps-by-context viz-shape-map viz-shapes-defs]
  (try
    (let [{:keys [axes context shape-name]} viz-shape-map
          this-viz-shape (get viz-shapes-defs shape-name)
          {:keys [library-shapes sql-maps runner selected-mode] :or {runner :views}} this-viz-shape
          req-axes (vec (keys (get this-viz-shape :axes)))
          relevant-field-maps (get field-maps-by-context context) ;;(filterv #(= (get % :context) context) field-maps) ;; pre-index with a group-by map?
          {:keys [table-type connection-id table-name honey-hash]} (first relevant-field-maps)

          axes-walk (into {} (for [[k v] axes]
                               {(keyword (str (cstr/replace (str k) ":" "") "-field")) (keyword v)
                                (keyword (str "*this-block*/" (cstr/replace (str k) ":" "") "-field")) (keyword (str "*this-block*/" (str v)))
                                (keyword (str "*this-view*/" (cstr/replace (str k) ":" "") "-field")) (keyword (str "*this-view*/" (str v)))
                                (keyword (str (cstr/replace (str k) ":" "") "-field-str"))
                                ;(ut/snake-case (str v))
                                (str v)
                                }))
          ;; _ (ut/pp [:axes-walk req-axes (vec (keys axes)) axes-walk])
          sql-table-walk {:table [(if (= table-type :query)
                                    (keyword (cstr/replace (str "query/" table-name) ":" ""))
                                    (keyword table-name)) :subq1]}
          queries-map (into {}
                            (for [qidx (range (count sql-maps))
                                  :let [q-map (get sql-maps qidx)
                                        q-key (if (= qidx 0)
                                                :query-preview
                                                (keyword (str "query-preview-" qidx)))]] {q-key q-map}))
          add-data-pre (fn [kw] (keyword (cstr/replace (str "data/" kw) ":" "")))
          ;; queries-map (merge
          ;;              queries-map ;; in case some viz recos use BE data fetching clover method w :data/*
          ;;              (into {} (for [[k v] queries-map]
          ;;                         {(add-data-pre k) (add-data-pre v)})))

          library-shapes (walk/postwalk-replace axes-walk library-shapes)

          queries-map  (walk/postwalk-replace (merge sql-table-walk axes-walk) queries-map)
          queries-map  (walk/postwalk-replace {[:sum :rrows] [:count 1]} queries-map) ;; false field to generate rowcnt recos
          queries-map  (walk/postwalk-replace {[[:count 1]] [:count 1]} queries-map) ;; wut

          base-block (merge {:h 5 :w 8} (select-keys this-viz-shape [:h :w :selected-view]))
          block-name (str (cstr/join ", " (vals axes)))
          library-shapes (if (not (map? library-shapes)) {:oz library-shapes} library-shapes)
          library-shapes (into {}
                               (for [[k v] library-shapes]
                                 {k (if (try (= (first v) :vega-lite) (catch Exception _ false)) ;; add some "beautiful defaults", just in case
                                      (-> v
                                          (assoc-in [1 :data :values] :query-preview)
                                          (assoc-in [1 :config] :theme/vega-defaults)
                                          (assoc-in [1 :width] "container") ;:panel-width)
                                          (assoc-in [1 :height] :panel-height)
                                          (assoc-in [1 :padding] 4)
                                          (assoc-in [1 :background] "transparent"))
                                      v)}))
          #_ (ut/pp [:library-shapes library-shapes])]
      (merge base-block
             {runner library-shapes
              :connection-id connection-id
              :shape-rotator {:context context :shape-name shape-name :axes axes
                              :honey-hash honey-hash
                              :shape-set (get this-viz-shape :shape-set)
                              :items (into (vec (for [v (keys library-shapes)] [runner v]))
                                           (vec (for [v (keys queries-map)] [:queries v])))}
              :selected-view (first (keys library-shapes))
              :name block-name
              :queries queries-map}
             (when selected-mode {:selected-mode {(first (keys library-shapes)) selected-mode}})))
            (catch Exception e (do (ut/pp [:materialize-viz-shape-error! e viz-shape-map]) {}))))

(defn materialize-viz-shapes [combos field-maps viz-shapes-defs]
  (let [field-maps-by-context (group-by :context field-maps)]
    (pmapv #(materialize-viz-shape field-maps-by-context % viz-shapes-defs) combos)))

(defn get-viz-shapes [field-maps viz-shapes-defs]
  (let [grouped (group-by :context field-maps)
        found-fields (vec (apply concat (for [[grp fields] grouped]
                                          (pmapv #(into [grp] %) (find-viz-fields fields viz-shapes-defs)))))
        ;; _ (ut/pp [:found-fields found-fields])
        combos (create-all-chart-configs found-fields)
        combos (pmapv #(-> %
                          (assoc :fields (into [] (vals (get % :axes))))
                          (assoc :table (get-in % [:context 3]))
                          (assoc :connection-id (get-in % [:context 0]))) combos)
        ;; _ (ut/pp [:combos1 combos])
        ;combos (filterv #(= (count (get % :fields)) (count (distinct (get % :fields)))) combos)
        ]
    combos))

(defn get-viz-shape-blocks [field-maps viz-shapes-defs & [filter-fn]]
  (swap! db/viz-reco-sniffs inc)
  (let [] ; [k (hash [field-maps viz-shapes-defs filter-fn])]
    (if-let [cached nil] ;(get @viz-maps-cache k)]
      cached
      (let [combos (get-viz-shapes field-maps viz-shapes-defs)
            combos (if filter-fn (filterv filter-fn combos) combos)
            ;; _ (ut/pp [:combos combos])
            combo-viz (materialize-viz-shapes combos field-maps viz-shapes-defs)]
        ;(swap! viz-maps-cache assoc k combo-viz)
        combo-viz))))

(defn make-error-box [msg f-path ttype]
  (let [cwidth (ut/get-terminal-width)
        border-color "\u001b[38;5;196m"   ; bright red
        title-color  "\u001b[38;5;219m"   ; pink
        text-color   "\u001b[38;5;255m"   ; bright white
        cyan-man     "\u001b[96m"         ; bright cyan
        reset        "\u001b[0m"
        inner-width (- cwidth 4)
        separator (str border-color "║" reset)
        top (str border-color "╔" (apply str (repeat inner-width "═")) "╗" reset)
        bottom (str border-color "╚" (apply str (repeat inner-width "═")) "╝" reset)
        title-line (format "%s║%s 🛢️ DATABASE %s ERROR ⚡ %s%s║%s"
                           border-color
                           title-color
                           ttype
                           (apply str (repeat (- inner-width 33) " ")) ;; 31
                           border-color
                           reset)
        path-line (format "%s║%s Path: %s%s%s║%s"
                          border-color
                          cyan-man ;text-color
                          f-path
                          (apply str (repeat (- inner-width 7 (count (str f-path))) " "))
                          border-color
                          reset)
        error-line (format "%s║%s Error: %s%s%s║%s"
                           border-color
                           text-color
                           msg
                           (apply str (repeat (- inner-width 8 (count (str msg))) " "))
                           border-color
                           reset)]
    (str "\n" top "\n"
         title-line "\n"
         path-line "\n"
         error-line "\n"
         bottom "\n")))



(defn shape-rotation-run [f-path & [pre-filter-fn]]
  (try
    (let [connection-id (get-connection-id f-path)
          _ (ut/pp ["🥩" :running-full-shape-rotation-job connection-id (str f-path)])
          startedms (System/currentTimeMillis)
          _ (swap! db/shape-rotation-status assoc-in [connection-id :all :started] startedms)
          conn-map (if (map? f-path) f-path (get @db/conn-map connection-id))
          _ (sql-exec systemh2-db
                      (to-sql {:delete-from [:fields] ;; clear last - in case a table was deleted (we clear each table before we add, but dont address orphans)
                               :where [:= :connection_id (str connection-id)]}))
          pre-filter-fn (when (= connection-id "systemh2-db") '(fn [m] (and (not= (get m :db-schema) "INFORMATION_SCHEMA")
                                                                            (not= (get m :db-schema) "PG_CATALOG"))))
          res (get-field-maps {:f-path f-path
                               :shape-test-defs @db/sniff-test-map ;; cruiser/default-sniff-tests
                               :shape-attributes-defs @db/field-attribute-map ;; cruiser/default-field-attributes
                                  ;;:filter-fn nil ;(fn [m] (true? (get m :very-low-cardinality?)))
                               :pre-filter-fn (or pre-filter-fn (get conn-map :pre-filter-fn))})
          combo-viz (get-viz-shape-blocks res @db/shapes-map)
          shapes-by (group-by (fn [m] [(get-in m [:shape-rotator :honey-hash])
                                       (keyword (get-in m [:shape-rotator :context 3]))]) combo-viz)
          fields-by (group-by (fn [m] [(get m :honey-hash)
                                       (keyword (get m :table-name))]) res)
        ;; _ (ut/pp [:kk (keys shapes-by) (keys fields-by)])
          _ (doseq [kk (keys fields-by)
                    :let [[honey-hash table-name-kw] kk
                          shapes (get shapes-by kk)
                          fields (get fields-by kk)
                          res {:shapes shapes :fields fields}
                          hash-key (hash [:fresh-table! connection-id table-name-kw])
                          modded-shapes (assoc res :shapes (mapv #(assoc-in % [:shape-rotator :source-panel] [:fresh-table! connection-id table-name-kw])
                                                                 (get res :shapes)))]]
              (d/transact-kv db/ddb
                             [[:put "honeyhash-map" honey-hash res]
                              [:put "shapes-map" hash-key modded-shapes]]))
        ;; group by honeyhash - do artificial inserts into datalevin
        ;; "[\"none!\" nil :Crime_Data_from_2020_to_Present]"
        ;;_ (ut/pp ["🥩" :shape-rotator-db {:fields (take 2 res) :shapes (take 2 combo-viz)}])
          _ (ut/pp ["🥩" :full-shape-rotation-job-done connection-id (str f-path)
                    [:fields (count res) :viz (count combo-viz)
                     :took (try
                             (ut/format-duration-seconds (/ (- (System/currentTimeMillis) startedms) 1000))
                             (catch Exception _ -1))]])
          _ (swap! db/shape-rotation-status assoc-in [connection-id :all :ended] (System/currentTimeMillis))
          _ (swap! db/shape-rotation-status assoc-in [connection-id :all :viz-combos] (count combo-viz))])
    (catch Throwable e
      ;; (ut/pp ["🛢️☠️☠️☠️" :shape-rotation-run-error f-path :DB-issue-generally (str e)])
      (do
        (println (make-error-box (str e) f-path "*METADATA*"))
        (Thread/sleep 1200))
      )))


;; (ut/pp [:field-maps (let [res (get-field-maps {:f-path "connections/superstore.edn"
;;                                                :shape-test-defs @db/sniff-test-map ;; shape-tests
;;                                                :shape-attributes-defs @db/field-attribute-map
;;                                                :honey-sql {:select [:*]
;;                                                            :from   [[:superstore :ssb99]]}
;;                                                :query-name :my-query-name-from-ui
;;                                                :filter-fn nil ;(fn [m] (true? (get m :very-low-cardinality?)))
;;                                                :pre-filter-fn nil})
;;                           vshapes @db/shapes-map ;(select-keys @db/shapes-map [:mapbox-point-map  ])
;;                           combos (get-viz-shapes res vshapes)
;;                           ;combo-viz (pmapv #(materialize-viz-shape res % viz-shapes) combos)
;;                           combo-viz (materialize-viz-shapes combos res vshapes)
;;                          ; _ (ut/pp [:sent (first combos)])
;;                          ; _ (ut/pp (pmapv #(get % :fields) combos))
;;                          ; combo-viz (materialize-viz-shape res (first combos) viz-shapes)
;;                           ;combo-viz (materialize-viz-shapes combos res viz-shapes)
;;                           ;combo-viz (get-viz-shape-blocks res viz-shapes)

;;                           ]
;;                       ;[res combos (count res) (count combos) combos combo-viz ]
;;                       ;(count res)
;;                       ;[combo-viz (count res) (count combo-viz)]
;;                       ;combo-viz

;;                       ;[res combo-viz]
;;                       res
;;                       ;@db/shapes-map
;;                       )] {:width 120})



;; exmaple values
;; rowcount
;; db_type text NULL,
;; connection_id text NULL,
;; table_type text NULL,
;; db_schema text NULL,
;; db_catalog text NULL,
;; table_name text NULL,
;; field_name text NULL,
;; field_type text NULL,
;; data_type text NULL,
;; context_hash text NULL,
;; key_hash text NULL,
;; is_group_by boolean NULL,

;; (ut/pp [:all (let [res (get-field-maps {:f-path "bigfoot-ufos"
;;                                         :shape-test-defs shape-tests
;;                                         :shape-attributes-defs shape-attributes
;;                                                :honey-sql {:from [:bigfoot_sightings]
;;                                                            :select [:class :season [[:count [:distinct :nearesttown]] :cnt]]
;;                                                            :order-by [[1 :desc]] :group-by [:class :season]}
;;                                                :query-name :my-query-name-from-ui
;;                                         :filter-fn nil ;(fn [m] (true? (get m :very-low-cardinality?)))
;;                                         :pre-filter-fn nil})
;;                           combo-viz (get-viz-shape-blocks res viz-shapes )] ;; (fn [m] (= (get m :shape-name) :recharts-pie))
;;                       [res (count res) (count combo-viz) combo-viz])])

;; (ut/pp (let [f-path "connections/bigfoot-ufos.edn"
;;              conn (get-or-create-db-pool f-path)
;;              connect-meta (get-jdbc-db-meta conn f-path)
;;              field-maps (create-target-field-vectors conn connect-meta)
;;              with-shape-tests (add-shape-tests field-maps conn shape-tests)
;;              with-shape-attribs (add-shape-attributes with-shape-tests shape-attributes)
;;              ;; final field vec above
;;              grouped (group-by (juxt :connection-id :db-catalog :db-schema :table-name) with-shape-attribs)
;;              found-fields (vec (apply concat (for [[grp fields] grouped]
;;                                                (pmapv #(into [grp] %) (find-viz-fields fields viz-shapes)))))
;;              combinations (create-all-chart-configs found-fields)]
;;          ;connect-meta
;;          ;field-maps
;;          ;with-shape-tests
;;          ;with-shape-attribs
;;          ;(print-grouped-fields grouped-viz)
;;          ;combinations
;;          ;grouped-viz
;;          ;found-fields
;;          (pmapv #(assoc % :context ((juxt :connection-id :db-catalog :db-schema :table-name) %)) with-shape-attribs)
;;          ))


;   [["bigfoot-ufos" nil "none" "bigfoot_sightings_locations"] :recharts-line :x "submitted" "bigfoot_sightings_locations"]
;   [["bigfoot-ufos" nil "none" "bigfoot_sightings_locations"] :recharts-line :x "season" "bigfoot_sightings_locations"]
;   [["bigfoot-ufos" nil "none" "bigfoot_sightings_locations"] :recharts-line :x "submitted_date" "bigfoot_sightings_locations"]
;   [["bigfoot-ufos" nil "none" "bigfoot_sightings_locations"] :recharts-line :y "land_area" "bigfoot_sightings_locations"]
;   [["bigfoot-ufos" nil "none" "bigfoot_sightings_locations"] :recharts-line :y "population" "bigfoot_sightings_locations"]
;   [["bigfoot-ufos" nil "none" "bigfoot_sightings_locations"] :recharts-line :y "water_area" "bigfoot_sightings_locations"]
;   [["bigfoot-ufos" nil "none" "bigfoot_sightings_locations"] :recharts-line :y "zip_lat" "bigfoot_sightings_locations"]
;   [["bigfoot-ufos" nil "none" "bigfoot_sightings_locations"] :recharts-line :y "zip_long" "bigfoot_sightings_locations"]
;   [["bigfoot-ufos" nil "none" "bigfoot_sightings_locations"] :recharts-line :y "housing_units" "bigfoot_sightings_locations"]
;   [["bigfoot-ufos" nil "none" "bigfoot_sightings_locations"] :recharts-area :x "fixed_month" "bigfoot_sightings_locations"]
;   [["bigfoot-ufos" nil "none" "bigfoot_sightings_locations"] :recharts-area :x "class" "bigfoot_sightings_locations"]
;   [["bigfoot-ufos" nil "none" "bigfoot_sightings_locations"] :recharts-area :x "month" "bigfoot_sightings_locations"]
;   [["bigfoot-ufos" nil "none" "bigfoot_sightings_locations"] :recharts-area :x "run_time" "bigfoot_sightings_locations"]
;   [["bigfoot-ufos" nil "none" "bigfoot_sightings_locations"] :recharts-area :x "submitted" "bigfoot_sightings_locations"]
;   [["bigfoot-ufos" nil "none" "bigfoot_sightings_locations"] :recharts-area :x "season" "bigfoot_sightings_locations"]
;   [["bigfoot-ufos" nil "none" "bigfoot_sightings_locations"] :recharts-area :x "submitted_date" "bigfoot_sightings_locations"]
;   [["bigfoot-ufos" nil "none" "bigfoot_sightings_locations"] :recharts-area :y "land_area" "bigfoot_sightings_locations"]
;   [["bigfoot-ufos" nil "none" "bigfoot_sightings_locations"] :recharts-area :y "population" "bigfoot_sightings_locations"]
;   [["bigfoot-ufos" nil "none" "bigfoot_sightings_locations"] :recharts-area :y "water_area" "bigfoot_sightings_locations"]
;   [["bigfoot-ufos" nil "none" "bigfoot_sightings_locations"] :recharts-area :y "zip_lat" "bigfoot_sightings_locations"]
;   [["bigfoot-ufos" nil "none" "bigfoot_sightings_locations"] :recharts-area :y "zip_long" "bigfoot_sightings_locations"]
;   [["bigfoot-ufos" nil "none" "bigfoot_sightings_locations"] :recharts-area :y "housing_units" "bigfoot_sightings_locations"]
;   [["bigfoot-ufos" nil "none" "bigfoot_sightings_locations"] :recharts-pie :segment "fixed_month" "bigfoot_sightings_locations"]
;   [["bigfoot-ufos" nil "none" "bigfoot_sightings_locations"] :recharts-pie :segment "class" "bigfoot_sightings_locations"]
;   [["bigfoot-ufos" nil "none" "bigfoot_sightings_locations"] :recharts-pie :segment "month" "bigfoot_sightings_locations"]
;   [["bigfoot-ufos" nil "none" "bigfoot_sightings_locations"] :recharts-pie :segment "season" "bigfoot_sightings_locations"]
;   [["bigfoot-ufos" nil "none" "bigfoot_sightings_locations"] :recharts-pie :value "land_area" "bigfoot_sightings_locations"]
;   [["bigfoot-ufos" nil "none" "bigfoot_sightings_locations"] :recharts-pie :value "population" "bigfoot_sightings_locations"]
;   [["bigfoot-ufos" nil "none" "bigfoot_sightings_locations"] :recharts-pie :value "water_area" "bigfoot_sightings_locations"]
;   [["bigfoot-ufos" nil "none" "bigfoot_sightings_locations"] :recharts-pie :value "zip_lat" "bigfoot_sightings_locations"]
;   [["bigfoot-ufos" nil "none" "bigfoot_sightings_locations"] :recharts-pie :value "zip_long" "bigfoot_sightings_locations"]
;   [["bigfoot-ufos" nil "none" "bigfoot_sightings_locations"] :recharts-pie :value "housing_units" "bigfoot_sightings_locations"]
;   [["bigfoot-ufos" nil "none" "bigfoot_sightings_locations"] :drop-down-filter :dim "class" "bigfoot_sightings_locations"]
;   [["bigfoot-ufos" nil "none" "bigfoot_sightings_locations"] :drop-down-filter :dim "season" "bigfoot_sightings_locations"]])

;  {:axes {:x "month", :y "zip_lat"}, :shape-name :recharts-area, :context ["bigfoot-ufos" nil "none" "bigfoot_sightings_locations"]}
;  {:axes {:x "submitted_date", :y "zip_lat"}, :shape-name :recharts-area, :context ["bigfoot-ufos" nil "none" "bigfoot_sightings_locations"]}
;  {:axes {:segment "season", :value "housing_units"}, :shape-name :recharts-pie, :context ["bigfoot-ufos" nil "none" "bigfoot_sightings_locations"]}
;  {:axes {:segment "class", :value "housing_units"}, :shape-name :recharts-pie, :context ["bigfoot-ufos" nil "none" "bigfoot_sightings_locations"]}
;  {:axes {:segment "fixed_month", :value "housing_units"}, :shape-name :recharts-pie, :context ["bigfoot-ufos" nil "none" "bigfoot_sightings_locations"]}
;  {:axes {:segment "month", :value "housing_units"}, :shape-name :recharts-pie, :context ["bigfoot-ufos" nil "none" "bigfoot_sightings_locations"]}
;  {:axes {:segment "season", :value "land_area"}, :shape-name :recharts-pie, :context ["bigfoot-ufos" nil "none" "bigfoot_sightings_locations"]}
;  {:axes {:segment "class", :value "land_area"}, :shape-name :recharts-pie, :context ["bigfoot-ufos" nil "none" "bigfoot_sightings_locations"]}
;  {:axes {:segment "fixed_month", :value "land_area"}, :shape-name :recharts-pie, :context ["bigfoot-ufos" nil "none" "bigfoot_sightings_locations"]}
;  {:axes {:segment "month", :value "land_area"}, :shape-name :recharts-pie, :context ["bigfoot-ufos" nil "none" "bigfoot_sightings_locations"]}
;  {:axes {:segment "season", :value "water_area"}, :shape-name :recharts-pie, :context ["bigfoot-ufos" nil "none" "bigfoot_sightings_locations"]}
;  {:axes {:segment "class", :value "water_area"}, :shape-name :recharts-pie, :context ["bigfoot-ufos" nil "none" "bigfoot_sightings_locations"]}
;  {:axes {:segment "fixed_month", :value "water_area"}, :shape-name :recharts-pie, :context ["bigfoot-ufos" nil "none" "bigfoot_sightings_locations"]}
;  {:axes {:segment "month", :value "water_area"}, :shape-name :recharts-pie, :context ["bigfoot-ufos" nil "none" "bigfoot_sightings_locations"]}
;  {:axes {:segment "season", :value "population"}, :shape-name :recharts-pie, :context ["bigfoot-ufos" nil "none" "bigfoot_sightings_locations"]}
;  {:axes {:segment "class", :value "population"}, :shape-name :recharts-pie, :context ["bigfoot-ufos" nil "none" "bigfoot_sightings_locations"]}
;  {:axes {:segment "fixed_month", :value "population"}, :shape-name :recharts-pie, :context ["bigfoot-ufos" nil "none" "bigfoot_sightings_locations"]}
;  {:axes {:segment "month", :value "population"}, :shape-name :recharts-pie, :context ["bigfoot-ufos" nil "none" "bigfoot_sightings_locations"]}
;  {:axes {:segment "season", :value "zip_long"}, :shape-name :recharts-pie, :context ["bigfoot-ufos" nil "none" "bigfoot_sightings_locations"]}
;  {:axes {:segment "class", :value "zip_long"}, :shape-name :recharts-pie, :context ["bigfoot-ufos" nil "none" "bigfoot_sightings_locations"]}
;  {:axes {:segment "fixed_month", :value "zip_long"}, :shape-name :recharts-pie, :context ["bigfoot-ufos" nil "none" "bigfoot_sightings_locations"]}
;  {:axes {:segment ;35m"month", :value "zip_long"}, :shape-name :recharts-pie, :context ["bigfoot-ufos" nil "none" "bigfoot_sightings_locations"]}
;  {:axes {:segment "season", :value "zip_lat"}, :shape-name :recharts-pie, :context ["bigfoot-ufos" nil "none" "bigfoot_sightings_locations"]}
;  {:axes {:segment "class", :value "zip_lat"}, :shape-name :recharts-pie, :context ["bigfoot-ufos" nil "none" "bigfoot_sightings_locations"]}
;  {:axes {:segment "fixed_month", :value "zip_lat"}, :shape-name :recharts-pie, :context ["bigfoot-ufos" nil "none" "bigfoot_sightings_locations"]}
;  {:axes {:segment "month", :value "zip_lat"}, :shape-name :recharts-pie, :context ["bigfoot-ufos" nil "none" "bigfoot_sightings_locations"]}
;  {:axes {:dim "season"}, :shape-name :drop-down-filter, :context ["bigfoot-ufos" nil "none" "bigfoot_sightings_locations"]}
;  {:axes {:dim "class"}, ;33m:shape-name :drop-down-filter, :context ["bigfoot-ufos" nil "none" "bigfoot_sightings_locations"]})


;; (ut/pp (let [f-path "connections/bigfoot-ufos.edn"
;;              conn (get-or-create-db-pool f-path)
;;              connect-meta (get-jdbc-db-meta conn f-path)]
;;          (create-query-field-vectors conn connect-meta (first (to-sql {:from [:bigfoot_sightings] :select [[[:count [:distinct :nearesttown]] :cnt]]})))))


;; (ut/pp (get-or-create-db-pool "connections/bigfoot-ufos.edn"))
;; (ut/pp (get @connection-pools "bigfoot-ufos"))



