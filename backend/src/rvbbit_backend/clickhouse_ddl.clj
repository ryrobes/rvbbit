(ns rvbbit-backend.clickhouse-ddl
  (:require
   [rvbbit-backend.util :as ut]
   [clojure.string :as cstr]))

(def create-reco-vw
  " CREATE or replace VIEW viz_recos_vw AS
SELECT c.context_hash, c.combo_hash, c.connection_id, c.shape_name, c.query_map, c.query_map_str,
c.viz_map, c.combo_edn, c.selected_view, c2.table_name AS table_name, c.conditionals AS condis, c.h AS h, c.w AS w, c.score AS score
FROM combos c
LEFT JOIN
(SELECT DISTINCT connection_id, table_name, context_hash FROM found_fields) c2
ON c.connection_id = c2.connection_id AND c.context_hash = c2.context_hash;")

(def create-reco-vw2
  "CREATE or replace VIEW viz_recos_vw2 AS 
WITH RankedCombos AS (
    SELECT 
        c.context_hash, 
        c.combo_hash, 
        c.connection_id, 
        c.shape_name, 
        c.query_map, 
        c.query_map_str,
        c.viz_map, 
        c.combo_edn, 
        c.selected_view,
        c2.table_name AS table_name, 
        c.conditionals AS condis, 
        c.h AS h, 
        c.w AS w, 
        c.score AS score,
        ROW_NUMBER() OVER(PARTITION BY c2.table_name, c.shape_name ORDER BY c.score DESC) AS rn
    FROM 
        combos c 
    LEFT JOIN 
        (SELECT DISTINCT connection_id, table_name, context_hash FROM found_fields) c2
    ON 
        c.connection_id = c2.connection_id AND c.context_hash = c2.context_hash
)
SELECT 
    context_hash, 
    combo_hash, 
    connection_id, 
    shape_name, 
    query_map, 
    query_map_str,
    viz_map, 
    combo_edn, 
    selected_view,
    table_name, 
    condis, 
    h, 
    w, 
    score
FROM 
    RankedCombos
WHERE 
    rn = 1;")

(def create-status-vw
  "CREATE or replace VIEW latest_status AS
WITH
    duration_calc AS (
        SELECT
            client_name,
            op_name,
            max(ts) AS max_ts,
            ROUND((toUnixTimestamp(max(ts)) - toUnixTimestamp(min(ts)))) AS durr
        FROM status
        GROUP BY client_name, op_name
    )

SELECT
    s.client_name,
    s.op_name,
    s.status,
    s.ts,
    CAST(duration_calc.durr AS String) AS duration
FROM status AS s
JOIN duration_calc
    ON s.client_name = duration_calc.client_name
    AND s.op_name = duration_calc.op_name
    AND s.ts = duration_calc.max_ts;")

(def create-status
  "CREATE TABLE IF NOT EXISTS status 
  (client_name String,
   op_name String,
   status String,
   ts DateTime DEFAULT now()
  ) ENGINE =   MergeTree()
   ORDER BY ts
   ;")

(def create-errors
  "CREATE TABLE IF NOT EXISTS errors 
  (db_conn String,
   sql_stmt String,
   error_str String,
   ts DateTime DEFAULT now()
  ) ENGINE =   MergeTree()
   ORDER BY ts
   ;")

(def create-screens
  "CREATE TABLE IF NOT EXISTS screens 
  (file_path String,
   screen_name String,
   blocks Int32,
   queries Int32,
   ts DateTime DEFAULT now()
  ) ENGINE =   MergeTree()
   ORDER BY ts
   ;")

(def create-blocks
  "CREATE TABLE IF NOT EXISTS blocks 
  (file_path String, 
   screen_name String,
   block_key String,
   block_name String,
   views Int32,
   queries Int32,
   view_names String,
   query_names String, 
   block_data String,
   ts DateTime DEFAULT now()
  ) ENGINE =   MergeTree()
   ORDER BY ts
   ;")

(def create-tests
  "CREATE TABLE IF NOT EXISTS tests
(
    db_type String,
    connection_id String,
    table_type String,
    db_schema String,
    db_catalog String,
    table_name String,
    field_name String,
    field_type String,
    data_type String,
    key_hash String,
    context_hash String,
    derived_calc String,
    derived_name String,
    is_sample Int32,
    test_sql String,
    test_raw_val String,
    test_name String,
    test_val_string String,
    test_val_integer Int32,
    test_val_float Float32,
    updated DateTime DEFAULT now(),
    run_id Int32
) ENGINE =   MergeTree()
ORDER BY (db_type, connection_id, table_name, field_name)
    ;")

(def create-attributes
  "CREATE TABLE IF NOT EXISTS attributes
  (db_type String,
   connection_id String,
   table_type String,
   db_schema String,
   db_catalog String,
   table_name String,
   field_name String,
   key_hash String,
   context_hash String,
   derived_calc String,
   derived_name String,
   attribute_name String,
   attribute_value UInt8 DEFAULT 0, -- Use UInt8 for boolean, 0 for false and 1 for true
   updated DateTime DEFAULT now(),
   run_id Int32
  ) ENGINE =   MergeTree()
   ORDER BY updated
   ; 
    -- Will add the primary key constraint once update semantics are figured out
  ")

(def create-found-fields
  "CREATE TABLE IF NOT EXISTS found_fields 
  (db_type String,
   connection_id String,
   table_type String,
   db_schema String,
   db_catalog String,
   table_name String,
   field_name String,
   derived_calc String,
   derived_name String,
   context_hash String, 
   key_hash String,
   shape_name String, 
   axes_key String,
   logic_map String, 
   updated DateTime DEFAULT now(),
   run_id Int32
  ) ENGINE =   MergeTree()
   ORDER BY updated
   ;
    -- Will add the primary key constraint once update semantics are figured out
  ")

(def create-connections
  "CREATE TABLE IF NOT EXISTS connections
   (connection_id String, 
    connection_str String, 
    user_name String, 
    database_name String, 
    database_version String,
    updated DateTime DEFAULT now(),
    original_connection_str String, 
    metadata_filter String,
    run_id Int32,
    started_at DateTime DEFAULT now(),
    ended_at DateTime
  ) ENGINE =   MergeTree()
   ORDER BY updated
   ;
    -- Will add the primary key constraint later
  ")

(def create-fields
  "CREATE TABLE IF NOT EXISTS fields
  (db_type String,
   connection_id String,
   table_type String,
   db_schema String,
   db_catalog String,
   table_name String,
   field_name String, 
   field_type String,
   data_type String, 
   context_hash String,
   key_hash String,
   is_group_by UInt8 DEFAULT 0, -- Use UInt8 for boolean, 0 for false and 1 for true
   derived_calc String, 
   derived_name String,
   updated DateTime DEFAULT now(),
   run_id Int32
  ) ENGINE =   MergeTree()
   ORDER BY updated
   ;
    -- Will add the primary key constraint later
  ")

(def create-combos
  "CREATE TABLE IF NOT EXISTS combos
  (context_hash String,
   uuid String,
   combo_hash String,
   connection_id String,                    
   shape_name String,
   table_name String,    
   selected_view String,
   query_map String,
   query_map_str String,
   viz_map String,
   conditionals String,
   key_hashes String,
   key_hashes_hash String,
   h Int32,
   w Int32,
   score Float64,
   combo_edn String
  ) ENGINE =   MergeTree()
   ORDER BY uuid
    ; 
  ")

(def create-combo-rows
  "CREATE TABLE IF NOT EXISTS combo_rows
  (context_hash String,
   combo_hash String,
   connection_id String,                    
   shape_name String,
   axes String,   
   talias String,                        
   walk1 String,                        
   walk2 String,
   walk3 String,
   walka String,                        
   key_hash String,
   table_type String,                
   field_type String,
   data_type String, 
   db_type String,
   db_schema String,
   db_catalog String,
   table_name String,
   field_name String,
   derived_calc String,
   derived_name String,
   score0 Float64,
   score1 Float64,
   score2 Float64
  ) ENGINE =   MergeTree()
   ORDER BY (context_hash, combo_hash)
   ;
  ")

(def create-rules-table0
  "CREATE TABLE IF NOT EXISTS rule_maps_tests
   (connection_id String,
    run_id Int32,
    test_name String, 
    sql_map String, 
    when_logic String, 
    fetch_one String
   ) ENGINE =   MergeTree()
   ORDER BY (run_id, connection_id)
   ;
  ")

(def create-rules-table1
  "CREATE TABLE IF NOT EXISTS rule_maps_attributes
   (connection_id String,
    run_id Int32,
    attribute_name String, 
    when_logic String
   ) ENGINE =   MergeTree()
   ORDER BY (run_id, connection_id)
   ;
  ")

(def create-rules-table2
  "CREATE TABLE IF NOT EXISTS rule_maps_derived_fields
   (connection_id String,
    run_id Int32,
    field_name String, 
    when_logic String, 
    calc_logic String
   ) ENGINE =   MergeTree()
   ORDER BY (run_id, connection_id)
   ;
  ")

(def create-rules-table3
  "CREATE TABLE IF NOT EXISTS rule_maps_viz_shapes
   (connection_id String,
    run_id Int32,
    shape_name String, 
    axes_logic String,    
    sql_maps String, 
    base_score Int32,
    selected_view String,
    library_shapes String
   ) ENGINE =   MergeTree()
   ORDER BY (run_id, connection_id)
   ;
  ")

(def create-logs
  "CREATE TABLE IF NOT EXISTS logs
   (booted String,
    ts Float64, 
    hts String, 
    caller String,
    type_key String,              
    log_str String
   ) ENGINE =   MergeTree()
   ORDER BY (ts, hts)
   ;
  ")

(def create-tables
  "CREATE TABLE IF NOT EXISTS connections
   (connection_hash String, 
    connection_str String, 
    user_name String, 
    product_name String, 
    product_version String,
    updated DateTime DEFAULT now(),
    run_id Int32
   ) ENGINE =   MergeTree()
   ORDER BY (updated, connection_hash)
   ;
  ")

(def create-viz
  "CREATE TABLE IF NOT EXISTS viz
   (connection_hash String, 
    connection_str String, 
    user_name String, 
    product_name String, 
    product_version String,
    updated DateTime DEFAULT now(),
    run_id Int32
   ) ENGINE =   MergeTree()
   ORDER BY (updated, connection_hash)
   ;
  ")


;; (defn create-attribute-sample [sample-table-name-str rowset]
;;   (try
;;     (let [field-types (cstr/join ""
;;                                  (for [[k v] (first rowset)]
;;                                    (let [type (cond (string? v) "varchar"
;;                                                     (integer? v) "integer"
;;                                                     (float? v) "float"
;;                                                     (cstr/includes? (str (type v)) "DateTime") "timestamp"
;;                                                     (cstr/includes? (str (type v)) "Date") "date"
;;                                                     :else "varchar"
;;                                                     )
;;                                          kk (ut/unkeyword k)]
;;                                      (str kk " " type " NULL,"))))]
;;       (ut/pp [:debug field-types (keys (first rowset))])
;;       (str " -- drop table if exists " sample-table-name-str ";
;;           create table if not exists " sample-table-name-str "
;;   (" (cstr/join "" (drop-last field-types)) ")   ;"))
;;     (catch Exception e (ut/pp [:error-creating-ddl-for-sample-based-on-rowset sample-table-name-str
;;                                :error (str e)
;;                                :rowset rowset]))))


(defn create-attribute-sample [sample-table-name-str rowset]
  (try
    (let [field-types (cstr/join ""
                                 (for [[k v] (first rowset)]
                                   (let [type (cond (string? v) "String"
                                                    (integer? v) "Int32"
                                                    (float? v) "Float64"
                                                    (cstr/includes? (str (type v)) "DateTime") "DateTime"
                                                    (cstr/includes? (str (type v)) "Date") "Date"
                                                    :else "String")
                                         kk (ut/unkeyword k)]
                                     (str kk " " type " ,"))))
          ddl (str "-- DROP TABLE IF EXISTS " sample-table-name-str "; \n
                         CREATE TABLE IF NOT EXISTS " sample-table-name-str "
                (" (cstr/join "" (drop-last field-types)) ") ENGINE =  Memory;")]
      ;(ut/pp [:debug field-types (keys (first rowset)) ddl])
      ddl)
    (catch Exception e (ut/pp [:error-creating-ddl-for-sample-based-on-rowset sample-table-name-str
                               :error (str e)
                               :rowset rowset]))))

(defn insert-attributes [])
