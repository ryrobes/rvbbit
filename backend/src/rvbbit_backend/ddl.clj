(ns rvbbit-backend.ddl
  (:require
    [clojure.string      :as cstr]
    [rvbbit-backend.util :as ut]))

;; (def drop-view1 "drop view if exists viz_recos_vw;")
;; (def drop-view2 "drop view if exists viz_recos_vw2;")
;; (def drop-view3 "drop view if exists latest_status;")

;; (def create-user-subs-vw "create view user_subs_vw as
;;                           SELECT is_live, item_key, item_sub_type, item_type, value, sample FROM
;;                           client_items
;;                              union all
;;                           SELECT NULL AS is_live,
;;                           screen_name AS item_key,
;;                           block_name AS item_sub_type,
;;                           ':block' AS item_type,
;;                           ':block/' || item_key '>' || block_name AS value,
;;                           NULL AS sample
;;                           FROM blocks
;;                           ;")

(def create-reco-vw
  "create view viz_recos_vw as 
   select distinct c.context_hash, c.combo_hash, c.connection_id, c.shape_name, c.query_map, c.query_map_str,
       c.viz_map, c.combo_edn, c.selected_view, c2.table_name as table_name, c.conditionals as condis, c.h as h, c.w as w, c.score as score   
from combos c left join (select distinct connection_id, table_name, context_hash from found_fields) c2
on c.connection_id = c2.connection_id and c.context_hash = c2.context_hash
;")

(def create-reco-vw2
  "create view viz_recos_vw2 as 
WITH RankedCombos AS (
    SELECT distinct 
        c.context_hash, 
        c.combo_hash, 
        c.connection_id, 
        c.shape_name, 
        c.query_map, 
        c.query_map_str,
        c.viz_map, 
        c.combo_edn, 
        c.selected_view,
        c2.table_name as table_name, 
        c.conditionals as condis, 
        c.h as h, 
        c.w as w, 
        c.score as score,
        ROW_NUMBER() OVER(PARTITION BY c2.table_name, c.shape_name ORDER BY c.score DESC) AS rn
    FROM 
        combos c 
    LEFT JOIN 
        (SELECT DISTINCT connection_id, table_name, context_hash  FROM found_fields) c2
    ON 
        c.connection_id = c2.connection_id AND c.context_hash = c2.context_hash
)

SELECT distinct 
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
    rn = 1
;")

(def create-status-vw
  "create view latest_status as
select s.client_name, s.op_name, s.status, s.ts, cast(tt.durr as varchar(400)) as duration
from status s
    join
(select client_name, op_name, max(ts) as max_ts,
       -- (max(ts) - min(ts)) as durr
       ROUND((JULIANDAY(max(ts)) - JULIANDAY(min(ts))) * 86400) as durr
from status
group by 1, 2) tt on s.client_name = tt.client_name
                         and s.op_name = tt.op_name
                         and s.ts = tt.max_ts ;")

(def create-status
  "create table if not exists status 
  (client_name text NULL,
   op_name text NULL,
   status text NULL,
   -- ts TIMESTAMP DEFAULT (datetime('now', 'localtime')) NULL
   ts TIMESTAMP DATETIME DEFAULT(STRFTIME('%Y-%m-%d %H:%M:%f', 'NOW')) 
   ) ;")

(def create-errors
  "create table if not exists errors 
  (db_conn text NULL,
   sql_stmt text NULL,
   error_str text NULL,
   ts TIMESTAMP DEFAULT (datetime('now', 'localtime')) NULL
   ) ;")

(def create-screens
  "create table if not exists screens 
  (file_path text NULL,
   screen_name text NULL,
   blocks integer NULL,
   queries integer NULL,
   ts TIMESTAMP DEFAULT (datetime('now', 'localtime')) NULL
   ) ;")

(def create-blocks
  "create table if not exists blocks 
  (file_path text NULL, 
   screen_name text NULL,
   block_key text NULL,
   block_name text NULL,
   views integer NULL,
   queries integer NULL,
   view_names text NULL,
   query_names text NULL, 
   block_data text NULL,
   tab_name text NULL,
   ts TIMESTAMP DEFAULT (datetime('now', 'localtime')) NULL
   ) ;")

(def create-boards
  "create table if not exists boards
  (file_path text NULL, 
   screen_name text NULL,
   board_name text NULL,
   board_data text NULL,
   ts TIMESTAMP DEFAULT (datetime('now', 'localtime')) NULL
   ) ;")

(def create-tests
  "create table if not exists tests
  (db_type text NULL,
   connection_id text NULL,
   table_type text NULL,
   db_schema text NULL,
   db_catalog text NULL,
   table_name text NULL,
   field_name text NULL, 
   field_type text NULL,
   data_type text NULL, 
   key_hash text NULL,
   context_hash text NULL,
   derived_calc text NULL,
   derived_name text NULL,
   is_sample integer NULL,  
   test_sql text NULL, 
   test_raw_val text NULL,
   test_name text NULL,
   test_val_string text NULL,
   test_val_integer integer NULL,
   test_val_float float NULL,
   updated TIMESTAMP DEFAULT (datetime('now', 'localtime')) NULL,
   run_id integer NULL) ;")

(def create-attributes
  "create table if not exists attributes
  (db_type text NULL,
   connection_id text NULL,
   table_type text NULL,
   db_schema text NULL,
   db_catalog text NULL,
   table_name text NULL,
   field_name text NULL,
   key_hash text NULL,
   context_hash text NULL,
   derived_calc text NULL,
   derived_name text NULL,
   attribute_name text NULL,
   attribute_value boolean NULL,
   updated TIMESTAMP DEFAULT (datetime('now', 'localtime')) NULL,
   run_id integer NULL
   -- primary key (connection_id, attribute_name)
   -- PK later when we figure out update semantics 
   ) ;")

(def create-found-fields
  "create table if not exists found_fields 
  (db_type text NULL,
   connection_id text NULL,
   table_type text NULL,
   db_schema text NULL,
   db_catalog text NULL,
   table_name text NULL,
   field_name text NULL,
   derived_calc text NULL,
   derived_name text NULL,
   context_hash text NULL, 
   key_hash text NULL,
   shape_name text NULL, 
   axes_key text NULL,
   logic_map text NULL, 
   updated TIMESTAMP DEFAULT (datetime('now', 'localtime')) NULL,
   run_id integer NULL
   -- primary key (connection_id, attribute_name)
   -- PK later when we figure out update semantics 
   ) ;")

(def create-connections
  "create table if not exists connections
   (connection_id text, 
    connection_str text, 
    user_name text NULL, 
    database_name text NULL, 
    database_version text NULL,
    updated TIMESTAMP DEFAULT (datetime('now', 'localtime')) NULL,
    original_connection_str text NULL, 
    metadata_filter text NULL,
    run_id integer NULL,
    started_at TIMESTAMP DEFAULT (datetime('now', 'localtime')) NULL,
    ended_at TIMESTAMP NULL --,
    -- primary key (connection_id)
    ) ;")

(def create-fields
  "create table if not exists fields
  (db_type text NULL,
   connection_id text NULL,
   table_type text NULL,
   db_schema text NULL,
   db_catalog text NULL,
   table_name text NULL,
   field_name text NULL, 
   field_type text NULL,
   data_type text NULL, 
   context_hash text NULL,
   key_hash text NULL,
   is_group_by boolean NULL,
   derived_calc text NULL, 
   derived_name text NULL,
   updated TIMESTAMP DEFAULT (datetime('now', 'localtime')) NULL,
   run_id integer NULL --,
   -- primary key (db_type, connection_id, table_type, db_schema, db_catalog, table_name, field_name, derived_calc)
   ) ;")

(def create-combos
  "create table if not exists combos
  (context_hash text NULL,
   uuid text NULL,
   combo_hash text NULL,
   connection_id text NULL,                    
   shape_name text NULL,
   table_name text NULL,    
   selected_view text NULL,
   query_map text NULL,
   query_map_str text NULL,
   viz_map text NULL,
   conditionals text NULL,
   key_hashes text NULL,
   key_hashes_hash text NULL,
   h integer NULL,
   w integer NULL,
   score real NULL,
   combo_edn text NULL) ;")

(def create-combo-rows
  "create table if not exists combo_rows
  (context_hash text NULL,
   combo_hash text NULL,
   connection_id text NULL,                    
   shape_name text NULL,
   axes text NULL,   
   talias text NULL,                        
   walk1 text NULL,                        
   walk2 text NULL,
   walk3 text NULL,
   walka text NULL,                        
   key_hash text NULL,
   table_type text NULL,                
   field_type text NULL,
   data_type text NULL, 
   db_type text NULL,
   db_schema text NULL,
   db_catalog text NULL,
   table_name text NULL,
   field_name text NULL,
   derived_calc text NULL,
   derived_name text NULL,
   score0 real NULL,
   score1 real NULL,
   score2 real NULL) ;")

(def create-rules-table0
  "create table if not exists rule_maps_tests
   (connection_id text NULL,
    run_id integer NULL,
   test_name text NULL, 
    sql_map text NULL, 
    when_logic text NULL, 
    fetch_one text NULL
    ) ;")

(def create-rules-table1
  "create table if not exists rule_maps_attributes
   (connection_id text NULL,
    run_id integer NULL,
    attribute_name text NULL, 
    when_logic text NULL
    ) ;")

(def create-rules-table2
  " create table if not exists rule_maps_derived_fields
   (connection_id text NULL,
    run_id integer NULL,
   field_name text NULL, 
    when_logic text NULL, 
    calc_logic text NULL
    ) ;")

(def create-rules-table3
  " create table if not exists rule_maps_viz_shapes
   (connection_id text NULL,
    run_id integer NULL,
    shape_name text NULL, 
    axes_logic text NULL,    
    sql_maps text NULL, 
    base_score integer NULL,
    selected_view text NULL,
    library_shapes text NULL
    ) ;")

(def create-logs
  "create table if not exists logs
   (booted text NULL,
    ts number NULL, 
    hts text NULL, 
    caller text NULL,
    type_key text NULL,              
    log_str text NULL) ;")

(def create-tables
  "create table if not exists connections
   (connection_hash text NULL, 
    connection_str text NULL, 
    user_name text NULL, 
    product_name text NULL, 
    product_version text NULL,
    updated TIMESTAMP DEFAULT (datetime('now', 'localtime')) NULL,
    run_id integer NULL) ;")

(def create-viz
  "create table if not exists connections
   (connection_hash text NULL, 
    connection_str text NULL, 
    user_name text NULL, 
    product_name text NULL, 
    product_version text NULL,
    updated TIMESTAMP DEFAULT (datetime('now', 'localtime')) NULL,
    run_id integer NULL) ;")

;; {:kp (str kp) :client_name (str client-name) :data (pr-str data) :panel-key (str (get kp 0))
;; :key
;; (str (get
;; kp 2)) :type (str (get kp 1))}

(def create-panel-history
  "create table if not exists panel_history
   (kp text NULL, 
    client_name text NULL, 
    data text NULL, 
    pre_data text NULL,
    diff text NULL,     
    diff_kp text NULL,                           
    panel_key text NULL, 
    key text NULL,
    type text NULL,
    updated TIMESTAMP DEFAULT (datetime('now', 'localtime')) NULL) ;")

(def create-panel-resolved-history
  "create table if not exists panel_resolved_history
   (kp text NULL, 
    client_name text NULL, 
    data text NULL, 
    pre_data text NULL,
    diff text NULL,     
    diff_kp text NULL,                           
    panel_key text NULL, 
    key text NULL,
    type text NULL,
    updated TIMESTAMP DEFAULT (datetime('now', 'localtime')) NULL) ;")

(def create-panel-materialized-history
  "create table if not exists panel_materialized_history
   (kp text NULL, 
    client_name text NULL, 
    data text NULL, 
    pre_data text NULL,
    diff text NULL,     
    diff_kp text NULL,                           
    panel_key text NULL, 
    key text NULL,
    type text NULL,
    updated TIMESTAMP DEFAULT (datetime('now', 'localtime')) NULL) ;")



(def create-board-history
  "create table if not exists board_history
   (client_name text NULL, 
    data text NULL, 
    updated TIMESTAMP DEFAULT (datetime('now', 'localtime')) NULL) ;")

(def create-flow-functions
  "create table if not exists flow_functions
   (connection_id text NULL,
    run_id integer NULL,
    category text NULL, 
    sub_flow text NULL,
    name text NULL, 
    full_map text NULL,
    description text NULL,
    file_path text NULL,
    inputs text NULL,
    icon text NULL,
    input_types text NULL,
    output_types text NULL,
    updated TIMESTAMP DEFAULT (datetime('now', 'localtime')) NULL) ;")

(def create-kits
  "create table if not exists kits
   (id INTEGER PRIMARY KEY AUTOINCREMENT,
    item_hash text NULL, 
    item_name text NULL,
    kit_name text NULL,
    item_type text NULL,
    item_key text NULL,
    item_idx integer NULL,   
    item_options text NULL,
    item_data text NULL, 
    client_name text NULL,
    flow_id text NULL,
    updated TIMESTAMP DEFAULT (datetime('now', 'localtime')) NULL) ;")

(def create-flow-schedules
  "create table if not exists flow_schedules
   (schedule text NULL,
    flow_id text NULL,
    opts text NULL,
    updated TIMESTAMP DEFAULT (datetime('now', 'localtime')) NULL) ;")

;; ^^^ query_hash, type, query_id, kit_name,   key,    idx,  edn_data
;; (no client-id so we can "reuse" persistent calls if wanted)
;; use the existing insert-queue?

;; :used_memory_mb :thread_count :sql_cache_size
(def create-client-stats
  "create table if not exists client_stats
   (client_name text NULL,
    ack integer NULL,
    client_latency integer NULL,
    client_subs integer NULL,
    last_seen text NULL,
    memory text NULL,
    push integer NULL,
    queue_size integer NULL,
    server_subs integer NULL,     
    last_seen_seconds integer NULL,
    booted_ts integer NULL,
    queue_distro text NULL,
    uptime_seconds integer NULL,
    uptime text NULL,
    messages_per_second integer NULL,
    recent_messages_per_second integer NULL,
    ts TIMESTAMP DEFAULT (datetime('now', 'localtime')) NULL) ;")

(def create-jvm-stats
  "create table if not exists jvm_stats
   (used_memory_mb integer NULL, 
    thread_count integer NULL, 
    messages integer NULL,
    batches integer NULL,
    recent_batches integer NULL,
    recent_batches_per_second float NULL,
    batches_per_second float NULL,
    unix_ms integer NULL,
    messages_per_second float NULL,
    recent_messages integer NULL,
    recent_messages_per_second float NULL,
    recent_queries_run integer NULL,
    recent_queries_per_second float NULL,
    queries_per_second float NULL,
    sql_cache_size integer NULL, 
    ws_peers integer NULL,
    subscriptions integer NULL,
    open_flow_channels integer NULL,
    uptime_seconds integer NULL,
    seconds_since_last_update integer NULL,
    queries_run integer NULL,
    internal_queries_run integer NULL,
    sniffs_run integer NULL,
    sys_load real NULL,
    ts TIMESTAMP DEFAULT (datetime('now', 'localtime')) NULL) ;")

(def create-flows
  "create table if not exists flows
  (file_path text NULL,
   flow_id text NULL,
   components integer NULL,
   connections integer NULL,
   last_modified text NULL,
   body text NULL,
   ts TIMESTAMP DEFAULT (datetime('now', 'localtime')) NULL
   ) ;")

(def create-flow-history
  "create table if not exists flow_history
  (flow_id text NULL,
   run_id text NULL,
   parent_run_id text NULL,
   client_name text NULL,
   in_error boolean NULL,
   started integer NULL,
   start_ts text NULL,
   ended integer NULL,
   elapsed real NULL,
   elapsed_seconds real NULL,
   human_elapsed text NULL,
   overrides text NULL,
   ts TIMESTAMP DEFAULT (datetime('now', 'localtime')) NULL
   ) ;")

(def create-flow-results
  "create table if not exists flow_results
   (flow_id text NULL,
    block_key text NULL,
    block_value text NULL,                      
    data_type text NULL,
    ts TIMESTAMP DEFAULT (datetime('now', 'localtime')) NULL) ;")

(def create-live-schedules
  "create table if not exists live_schedules
   (flow_id text NULL,
    override text NULL,
    schedule text NULL,
    channel text NULL,
    next_times text NULL,
    ts TIMESTAMP DEFAULT (datetime('now', 'localtime')) NULL) ;")

(def create-channel-history
  "create table if not exists channel_history
   (flow_id text NULL,
    run_id text NULL,
    base_flow_id text NULL,
    channel text NULL,
    data_type text NULL,                      
    dest text NULL,
    end_ts text NULL,
    start_ts text NULL,
    end number NULL,
    path text NULL,
    start number NULL,
    type text NULL,
    value text NULL,
    ts TIMESTAMP DEFAULT (datetime('now', 'localtime')) NULL) ;")

(def create-fn-history
  "create table if not exists fn_history
   (flow_id text NULL,
    run_id text NULL,
    base_flow_id text NULL,
    channel text NULL,
    block text NULL,                        
    data_type text NULL,         
    end_ts text NULL,
    start_ts text NULL,
    dbgn text NULL,
    elapsed_ms integer NULL,
    from_block text NULL,
    dest text NULL,
    end number NULL,
    path text NULL,
    view text NULL,
    start number NULL,
    type text NULL,
    value text NULL,
    ts TIMESTAMP DEFAULT (datetime('now', 'localtime')) NULL) ;")

(def create-client-items
  "create table if not exists client_items
   (item_key text NULL,
    item_type text NULL,
    item_sub_type text NULL,
    value text NULL,
    is_live boolean NULL,
    sample text NULL,
    display_name text NULL,
    block_meta text NULL,
    ts TIMESTAMP DEFAULT (datetime('now', 'localtime')) NULL) ;")

(def create-client-memory
  "create table if not exists client_memory
   (mem_time text NULL,
    client_name text NULL,
    packets integer NULL,
    batches integer NULL,                           
    mem_limit integer NULL,
    mem_used integer NULL,
    mem_used_mb text NULL, 
    mem_total integer NULL,
    latency integer NULL,
    client_subs integer NULL,
    server_subs integer NULL,
    messages_per_second integer NULL,
    recent_messages_per_second integer NULL,
    ts TIMESTAMP DEFAULT (datetime('now', 'localtime')) NULL) ;")







;; (def create-reco-vw
;;   "create view viz_recos_vw as 
;;    select distinct c.context_hash, c.combo_hash, c.connection_id, c.shape_name, c.query_map, c.query_map_str,
;;        c.viz_map, c.combo_edn, c.selected_view, c2.table_name as table_name, c.conditionals as condis, c.h as h, c.w as w, c.score as score   
;; from combos c left join (select distinct connection_id, table_name, context_hash from found_fields) c2
;; on c.connection_id = c2.connection_id and c.context_hash = c2.context_hash
;; ;")

;; (def create-reco-vw2
;;   "create view viz_recos_vw2 as 
;; WITH RankedCombos AS (
;;     SELECT distinct 
;;         c.context_hash, 
;;         c.combo_hash, 
;;         c.connection_id, 
;;         c.shape_name, 
;;         c.query_map, 
;;         c.query_map_str,
;;         c.viz_map, 
;;         c.combo_edn, 
;;         c.selected_view,
;;         c2.table_name as table_name, 
;;         c.conditionals as condis, 
;;         c.h as h, 
;;         c.w as w, 
;;         c.score as score,
;;         ROW_NUMBER() OVER(PARTITION BY c2.table_name, c.shape_name ORDER BY c.score DESC) AS rn
;;     FROM 
;;         combos c 
;;     LEFT JOIN 
;;         (SELECT DISTINCT connection_id, table_name, context_hash  FROM found_fields) c2
;;     ON 
;;         c.connection_id = c2.connection_id AND c.context_hash = c2.context_hash
;; )

;; SELECT distinct 
;;     context_hash, 
;;     combo_hash, 
;;     connection_id, 
;;     shape_name, 
;;     query_map, 
;;     query_map_str,
;;     viz_map, 
;;     combo_edn, 
;;     selected_view,
;;     table_name, 
;;     condis, 
;;     h, 
;;     w, 
;;     score
;; FROM 
;;     RankedCombos
;; WHERE 
;;     rn = 1
;; ;")

;; (def create-status-vw
;;   "create view latest_status as
;; select s.client_name, s.op_name, s.status, s.ts, cast(tt.durr as varchar(400)) as duration
;; from status s
;;     join
;; (select client_name, op_name, max(ts) as max_ts,
;;        ROUND(EXTRACT(EPOCH FROM (max(ts) - min(ts)))) as durr
;; from status
;; group by 1, 2) tt on s.client_name = tt.client_name
;;                          and s.op_name = tt.op_name
;;                          and s.ts = tt.max_ts ;")

;; (def create-status
;;   "create table if not exists status 
;;   (client_name text NULL,
;;    op_name text NULL,
;;    status text NULL,
;;    ts TIMESTAMP DEFAULT CURRENT_TIMESTAMP
;;    ) ;")

;; (def create-errors
;;   "create table if not exists errors 
;;   (db_conn text NULL,
;;    sql_stmt text NULL,
;;    error_str text NULL,
;;    ts TIMESTAMP DEFAULT CURRENT_TIMESTAMP
;;    ) ;")

;; (def create-screens
;;   "create table if not exists screens 
;;   (file_path text NULL,
;;    screen_name text NULL,
;;    blocks integer NULL,
;;    queries integer NULL,
;;    ts TIMESTAMP DEFAULT CURRENT_TIMESTAMP
;;    ) ;")

;; (def create-blocks
;;   "create table if not exists blocks 
;;   (file_path text NULL, 
;;    screen_name text NULL,
;;    block_key text NULL,
;;    block_name text NULL,
;;    views integer NULL,
;;    queries integer NULL,
;;    view_names text NULL,
;;    query_names text NULL, 
;;    block_data text NULL,
;;    tab_name text NULL,
;;    ts TIMESTAMP DEFAULT CURRENT_TIMESTAMP
;;    ) ;")

;; (def create-boards
;;   "create table if not exists boards
;;   (file_path text NULL, 
;;    screen_name text NULL,
;;    board_name text NULL,
;;    board_data text NULL,
;;    ts TIMESTAMP DEFAULT CURRENT_TIMESTAMP
;;    ) ;")

;; (def create-tests
;;   "create table if not exists tests
;;   (db_type text NULL,
;;    connection_id text NULL,
;;    table_type text NULL,
;;    db_schema text NULL,
;;    db_catalog text NULL,
;;    table_name text NULL,
;;    field_name text NULL, 
;;    field_type text NULL,
;;    data_type text NULL, 
;;    key_hash text NULL,
;;    context_hash text NULL,
;;    derived_calc text NULL,
;;    derived_name text NULL,
;;    is_sample integer NULL,  
;;    test_sql text NULL, 
;;    test_raw_val text NULL,
;;    test_name text NULL,
;;    test_val_string text NULL,
;;    test_val_integer integer NULL,
;;    test_val_float float NULL,
;;    updated TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
;;    run_id integer NULL) ;")

;; (def create-attributes
;;   "create table if not exists attributes
;;   (db_type text NULL,
;;    connection_id text NULL,
;;    table_type text NULL,
;;    db_schema text NULL,
;;    db_catalog text NULL,
;;    table_name text NULL,
;;    field_name text NULL,
;;    key_hash text NULL,
;;    context_hash text NULL,
;;    derived_calc text NULL,
;;    derived_name text NULL,
;;    attribute_name text NULL,
;;    attribute_value boolean NULL,
;;    updated TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
;;    run_id integer NULL
;;    ) ;")

;; (def create-found-fields
;;   "create table if not exists found_fields 
;;   (db_type text NULL,
;;    connection_id text NULL,
;;    table_type text NULL,
;;    db_schema text NULL,
;;    db_catalog text NULL,
;;    table_name text NULL,
;;    field_name text NULL,
;;    derived_calc text NULL,
;;    derived_name text NULL,
;;    context_hash text NULL, 
;;    key_hash text NULL,
;;    shape_name text NULL, 
;;    axes_key text NULL,
;;    logic_map text NULL, 
;;    updated TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
;;    run_id integer NULL
;;    ) ;")

;; (def create-connections
;;   "create table if not exists connections
;;    (connection_id text, 
;;     connection_str text, 
;;     user_name text NULL, 
;;     database_name text NULL, 
;;     database_version text NULL,
;;     updated TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
;;     original_connection_str text NULL, 
;;     metadata_filter text NULL,
;;     run_id integer NULL,
;;     started_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
;;     ended_at TIMESTAMP NULL
;;     ) ;")

;; (def create-fields
;;   "create table if not exists fields
;;   (db_type text NULL,
;;    connection_id text NULL,
;;    table_type text NULL,
;;    db_schema text NULL,
;;    db_catalog text NULL,
;;    table_name text NULL,
;;    field_name text NULL, 
;;    field_type text NULL,
;;    data_type text NULL, 
;;    context_hash text NULL,
;;    key_hash text NULL,
;;    is_group_by boolean NULL,
;;    derived_calc text NULL, 
;;    derived_name text NULL,
;;    updated TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
;;    run_id integer NULL
;;    ) ;")

;; (def create-combos
;;   "create table if not exists combos
;;   (context_hash text NULL,
;;    uuid text NULL,
;;    combo_hash text NULL,
;;    connection_id text NULL,                    
;;    shape_name text NULL,
;;    table_name text NULL,    
;;    selected_view text NULL,
;;    query_map text NULL,
;;    query_map_str text NULL,
;;    viz_map text NULL,
;;    conditionals text NULL,
;;    key_hashes text NULL,
;;    key_hashes_hash text NULL,
;;    h integer NULL,
;;    w integer NULL,
;;    score real NULL,
;;    combo_edn text NULL) ;")

;; (def create-combo-rows
;;   "create table if not exists combo_rows
;;   (context_hash text NULL,
;;    combo_hash text NULL,
;;    connection_id text NULL,                    
;;    shape_name text NULL,
;;    axes text NULL,   
;;    talias text NULL,                        
;;    walk1 text NULL,                        
;;    walk2 text NULL,
;;    walk3 text NULL,
;;    walka text NULL,                        
;;    key_hash text NULL,
;;    table_type text NULL,                
;;    field_type text NULL,
;;    data_type text NULL, 
;;    db_type text NULL,
;;    db_schema text NULL,
;;    db_catalog text NULL,
;;    table_name text NULL,
;;    field_name text NULL,
;;    derived_calc text NULL,
;;    derived_name text NULL,
;;    score0 real NULL,
;;    score1 real NULL,
;;    score2 real NULL) ;")

;; (def create-rules-table0
;;   "create table if not exists rule_maps_tests
;;    (connection_id text NULL,
;;     run_id integer NULL,
;;    test_name text NULL, 
;;     sql_map text NULL, 
;;     when_logic text NULL, 
;;     fetch_one text NULL
;;     ) ;")

;; (def create-rules-table1
;;   "create table if not exists rule_maps_attributes
;;    (connection_id text NULL,
;;     run_id integer NULL,
;;     attribute_name text NULL, 
;;     when_logic text NULL
;;     ) ;")

;; (def create-rules-table2
;;   " create table if not exists rule_maps_derived_fields
;;    (connection_id text NULL,
;;     run_id integer NULL,
;;    field_name text NULL, 
;;     when_logic text NULL, 
;;     calc_logic text NULL
;;     ) ;")

;; (def create-rules-table3
;;   " create table if not exists rule_maps_viz_shapes
;;    (connection_id text NULL,
;;     run_id integer NULL,
;;     shape_name text NULL, 
;;     axes_logic text NULL,    
;;     sql_maps text NULL, 
;;     base_score integer NULL,
;;     selected_view text NULL,
;;     library_shapes text NULL
;;     ) ;")

;; (def create-logs
;;   "create table if not exists logs
;;    (booted text NULL,
;;     ts bigint NULL, 
;;     hts text NULL, 
;;     caller text NULL,
;;     type_key text NULL,              
;;     log_str text NULL) ;")

;; (def create-panel-history
;;   "create table if not exists panel_history
;;    (kp text NULL, 
;;     client_name text NULL, 
;;     data text NULL, 
;;     pre_data text NULL,
;;     diff text NULL,     
;;     diff_kp text NULL,                           
;;     panel_key text NULL, 
;;     key text NULL,
;;     type text NULL,
;;     updated TIMESTAMP DEFAULT CURRENT_TIMESTAMP) ;")

;; (def create-board-history
;;   "create table if not exists board_history
;;    (client_name text NULL, 
;;     data text NULL, 
;;     updated TIMESTAMP DEFAULT CURRENT_TIMESTAMP) ;")

;; (def create-flow-functions
;;   "create table if not exists flow_functions
;;    (connection_id text NULL,
;;     run_id integer NULL,
;;     category text NULL, 
;;     sub_flow text NULL,
;;     name text NULL, 
;;     full_map text NULL,
;;     description text NULL,
;;     file_path text NULL,
;;     inputs text NULL,
;;     icon text NULL,
;;     input_types text NULL,
;;     output_types text NULL,
;;     updated TIMESTAMP DEFAULT CURRENT_TIMESTAMP) ;")

;; (def create-kits
;;   "create table if not exists kits
;;    (id SERIAL PRIMARY KEY,
;;     item_hash text NULL, 
;;     item_name text NULL,
;;     kit_name text NULL,
;;     item_type text NULL,
;;     item_key text NULL,
;;     item_idx integer NULL,   
;;     item_options text NULL,
;;     item_data text NULL, 
;;     client_name text NULL,
;;     flow_id text NULL,
;;     updated TIMESTAMP DEFAULT CURRENT_TIMESTAMP) ;")

;; (def create-flow-schedules
;;   "create table if not exists flow_schedules
;;    (schedule text NULL,
;;     flow_id text NULL,
;;     opts text NULL,
;;     updated TIMESTAMP DEFAULT CURRENT_TIMESTAMP) ;")

;; (def create-client-stats
;;   "create table if not exists client_stats
;;    (client_name text NULL,
;;     ack integer NULL,
;;     client_latency integer NULL,
;;     client_subs integer NULL,
;;     last_seen text NULL,
;;     memory text NULL,
;;     push integer NULL,
;;     queue_size integer NULL,
;;     server_subs integer NULL,     
;;     last_seen_seconds integer NULL,
;;     booted_ts integer NULL,
;;     queue_distro text NULL,
;;     uptime_seconds integer NULL,
;;     uptime text NULL,
;;     messages_per_second integer NULL,
;;     recent_messages_per_second integer NULL,
;;     ts TIMESTAMP DEFAULT CURRENT_TIMESTAMP) ;")

;; (def create-jvm-stats
;;   "create table if not exists jvm_stats
;;    (used_memory_mb integer NULL, 
;;     thread_count integer NULL, 
;;     messages integer NULL,
;;     batches integer NULL,
;;     recent_batches integer NULL,
;;     recent_batches_per_second float NULL,
;;     batches_per_second float NULL,
;;     unix_ms bigint NULL,
;;     messages_per_second float NULL,
;;     recent_messages integer NULL,
;;     recent_messages_per_second float NULL,
;;     recent_queries_run integer NULL,
;;     recent_queries_per_second float NULL,
;;     queries_per_second float NULL,
;;     sql_cache_size integer NULL, 
;;     ws_peers integer NULL,
;;     subscriptions integer NULL,
;;     open_flow_channels integer NULL,
;;     uptime_seconds integer NULL,
;;     seconds_since_last_update integer NULL,
;;     queries_run integer NULL,
;;     internal_queries_run integer NULL,
;;     sniffs_run integer NULL,
;;     sys_load real NULL,
;;     ts TIMESTAMP DEFAULT CURRENT_TIMESTAMP) ;")

;; (def create-flows
;;   "create table if not exists flows
;;   (file_path text NULL,
;;    flow_id text NULL,
;;    components integer NULL,
;;    connections integer NULL,
;;    last_modified text NULL,
;;    body text NULL,
;;    ts TIMESTAMP DEFAULT CURRENT_TIMESTAMP
;;    ) ;")

;; (def create-flow-history
;;   "create table if not exists flow_history
;;   (flow_id text NULL,
;;    run_id text NULL,
;;    parent_run_id text NULL,
;;    client_name text NULL,
;;    in_error boolean NULL,
;;    started integer NULL,
;;    start_ts text NULL,
;;    ended integer NULL,
;;    elapsed real NULL,
;;    elapsed_seconds real NULL,
;;    human_elapsed text NULL,
;;    overrides text NULL,
;;    ts TIMESTAMP DEFAULT CURRENT_TIMESTAMP
;;    ) ;")

;; (def create-flow-results
;;   "create table if not exists flow_results
;;    (flow_id text NULL,
;;     block_key text NULL,
;;     block_value text NULL,                      
;;     data_type text NULL,
;;     ts TIMESTAMP DEFAULT CURRENT_TIMESTAMP) ;")

;; (def create-live-schedules
;;   "create table if not exists live_schedules
;;    (flow_id text NULL,
;;     override text NULL,
;;     schedule text NULL,
;;     channel text NULL,
;;     next_times text NULL,
;;     ts TIMESTAMP DEFAULT CURRENT_TIMESTAMP) ;")

;; (def create-channel-history
;;   "create table if not exists channel_history
;;    (flow_id text NULL,
;;     run_id text NULL,
;;     base_flow_id text NULL,
;;     channel text NULL,
;;     data_type text NULL,                      
;;     dest text NULL,
;;     end_ts text NULL,
;;     start_ts text NULL,
;;     \"end\" bigint NULL,
;;     path text NULL,
;;     start bigint NULL,
;;     type text NULL,
;;     value text NULL,
;;     ts TIMESTAMP DEFAULT CURRENT_TIMESTAMP) ;")

;; (def create-fn-history
;;   "create table if not exists fn_history
;;    (flow_id text NULL,
;;     run_id text NULL,
;;     base_flow_id text NULL,
;;     channel text NULL,
;;     block text NULL,                        
;;     data_type text NULL,         
;;     end_ts text NULL,
;;     start_ts text NULL,
;;     dbgn text NULL,
;;     elapsed_ms integer NULL,
;;     from_block text NULL,
;;     dest text NULL,
;;     \"end\" bigint NULL,
;;     path text NULL,
;;     view text NULL,
;;     start bigint NULL,
;;     type text NULL,
;;     value text NULL,
;;     ts TIMESTAMP DEFAULT CURRENT_TIMESTAMP) ;")

;; (def create-client-items
;;   "create table if not exists client_items
;;    (item_key text NULL,
;;     item_type text NULL,
;;     item_sub_type text NULL,
;;     value text NULL,
;;     is_live boolean NULL,
;;     sample text NULL,
;;     display_name text NULL,
;;     block_meta text NULL,
;;     ts TIMESTAMP DEFAULT CURRENT_TIMESTAMP) ;")

;; (def create-client-memory
;;   "create table if not exists client_memory
;;    (mem_time text NULL,
;;     client_name text NULL,
;;     packets bigint NULL,
;;     batches bigint NULL,                           
;;     mem_limit bigint NULL,
;;     mem_used bigint NULL,
;;     mem_used_mb text NULL, 
;;     mem_total bigint NULL,
;;     latency bigint NULL,
;;     client_subs bigint NULL,
;;     server_subs bigint NULL,
;;     messages_per_second bigint NULL,
;;     recent_messages_per_second bigint NULL,
;;     ts TIMESTAMP DEFAULT CURRENT_TIMESTAMP) ;")









(defn create-attribute-sample
  [sample-table-name-str rowset]
  ;; TODO should rewrite this in honeysql, just to be consistent...
  ;(ut/ppln [:sample-table-name-str sample-table-name-str :rowset rowset])
  (try (let [;key-hash sample-table-name-str (cstr/replace key-hash #"-" "_") ;; hashes are often negative. No bueno.
             field-types (cstr/join ""
                                    (for [[k v] (first rowset)]
                                      (let [type (cond (string? v)                                "varchar" ;"text"
                                                       (integer? v)                               "integer"
                                                       (float? v)                                 "float"
                                                       (cstr/includes? (str (type v)) "DateTime") "timestamp"
                                                       (cstr/includes? (str (type v)) "Date")     "date"
                                                       :else                                      "varchar" ;"text"
                                                 )
                                            kk   (ut/unkeyword k)]
                                        (str kk " " type " NULL,"))))]
         ;(ut/pp [:debug field-types (keys (first rowset))])
         (str  ;; " -- drop table if exists "
              ;; sample-table-name-str
              ;; ";
           " create table if not exists "
              sample-table-name-str
              "
  ("
              (cstr/join "" (drop-last field-types))
              ")   ;"))
       (catch Exception e
         (ut/pp [:error-creating-ddl-for-sample-based-on-rowset sample-table-name-str :error (str e) :rowset rowset]))))

(def sample-kit-output
  
  {:top-20-outliers
     {:data
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
                    "All rows in this group:"
                    {:_h            8
                     :_query-id     :kit-query--15841505520-detail
                     :connection-id "boston-crime"
                     :from          [:query/OFFENSE-CODE-GROUP-drag-40]
                     :select        [:*]
                     :where         [:*if :kit-query--15841505520-agg/*.clicked
                                     [:*all= :kit-query--15841505520-agg/*.clicked [:DISTRICT]] [:and [:= :DISTRICT "B2"]]]}]
          :highlight-columns {:OFFENSE-CODE-GROUP-drag-40 [:DISTRICT]}
          :name
            "When grouping by district and aggregating by :rows there are values that are higher than the mean (1185 for this particular aggregation) by 1250."
          :order 0
          :parameters {}
          :step-mutates {}}
         
         {:ask-mutates {"Highlight these in your source query?"
                          {[:panels :block-984 :queries :OFFENSE-CODE-GROUP-drag-40 :style-rules [:* :higlight1640170157]]
                             {:logic [:and [:= :YEAR 2016]]
                              :style {:background-color "#008b8b66" :border "1px solid #00000000"}}}}
          :content [[:v-box :size "auto" :width "490px" :style {:font-size "13px" :opacity 0.33} :children
                     [[:h-box :size "auto" :justify :between :children
                       [[:box :child ":mean"] [:box :child "3853.75" :style {:font-weight 400}]]]
                      [:h-box :size "auto" :justify :between :children
                       [[:box :child ":standard-deviation"] [:box :child "1099.883942741233" :style {:font-weight 400}]]]
                      [:h-box :size "auto" :justify :between :children
                       [[:box :child ":sample-size"] [:box :child "4" :style {:font-weight 400}]]]
                      [:h-box :size "auto" :justify :between :children
                       [[:box :child ":fields"] [:box :child "[:YEAR]" :style {:font-weight 400}]]]
                      [:h-box :size "auto" :justify :between :children
                       [[:box :child ":calc-used"] [:box :child "[:percentiles [0.01 0.99]]" :style {:font-weight 400}]]]]]
                    [:v-box :size "auto" :width "490px" :style {:font-size "16px"} :children
                     [[:h-box :size "auto" :justify :between :children
                       [[:box :child ":YEAR"] [:box :child "2016" :style {:font-weight 400}]]]]] "as a wider aggregate:"
                    {:_h            4
                     :_query-id     :kit-query-16401701570-agg
                     :connection-id "boston-crime"
                     :from          [:query/OFFENSE-CODE-GROUP-drag-40]
                     :group-by      [:YEAR]
                     :order-by      [[2 :desc]]
                     :select        [:YEAR [[:count 1] :rows]]
                     :style-rules   {[:* :highlight-group] {:logic [:and [:= :YEAR 2016]]
                                                            :style {:background-color "#008b8b66"
                                                                    :border           "1px solid #00000000"}}}}
                    "All rows in this group:"
                    {:_h            8
                     :_query-id     :kit-query-16401701570-detail
                     :connection-id "boston-crime"
                     :from          [:query/OFFENSE-CODE-GROUP-drag-40]
                     :select        [:*]
                     :where         [:*if :kit-query-16401701570-agg/*.clicked
                                     [:*all= :kit-query-16401701570-agg/*.clicked [:YEAR]] [:and [:= :YEAR 2016]]]}]
          :highlight-columns {:OFFENSE-CODE-GROUP-drag-40 [:YEAR]}
          :name
            "When grouping by year and aggregating by :rows there are values that are higher than the mean (3853 for this particular aggregation) by 1209."
          :order 1
          :parameters {}
          :step-mutates {}}
         {:ask-mutates {"Highlight these in your source query?"
                          {[:panels :block-984 :queries :OFFENSE-CODE-GROUP-drag-40 :style-rules [:* :higlight-1838160550]]
                             {:logic [:and [:= :YEAR 2015]]
                              :style {:background-color "#008b8b66" :border "1px solid #00000000"}}}}
          :content [[:v-box :size "auto" :width "490px" :style {:font-size "13px" :opacity 0.33} :children
                     [[:h-box :size "auto" :justify :between :children
                       [[:box :child ":mean"] [:box :child "3853.75" :style {:font-weight 400}]]]
                      [:h-box :size "auto" :justify :between :children
                       [[:box :child ":standard-deviation"] [:box :child "1099.883942741233" :style {:font-weight 400}]]]
                      [:h-box :size "auto" :justify :between :children
                       [[:box :child ":sample-size"] [:box :child "4" :style {:font-weight 400}]]]
                      [:h-box :size "auto" :justify :between :children
                       [[:box :child ":fields"] [:box :child "[:YEAR]" :style {:font-weight 400}]]]
                      [:h-box :size "auto" :justify :between :children
                       [[:box :child ":calc-used"] [:box :child "[:percentiles [0.01 0.99]]" :style {:font-weight 400}]]]]]
                    [:v-box :size "auto" :width "490px" :style {:font-size "16px"} :children
                     [[:h-box :size "auto" :justify :between :children
                       [[:box :child ":YEAR"] [:box :child "2015" :style {:font-weight 400}]]]]] "as a wider aggregate:"
                    {:_h            4
                     :_query-id     :kit-query--18381605500-agg
                     :connection-id "boston-crime"
                     :from          [:query/OFFENSE-CODE-GROUP-drag-40]
                     :group-by      [:YEAR]
                     :order-by      [[2 :desc]]
                     :select        [:YEAR [[:count 1] :rows]]
                     :style-rules   {[:* :highlight-group] {:logic [:and [:= :YEAR 2015]]
                                                            :style {:background-color "#008b8b66"
                                                                    :border           "1px solid #00000000"}}}}
                    "All rows in this group:"
                    {:_h            8
                     :_query-id     :kit-query--18381605500-detail
                     :connection-id "boston-crime"
                     :from          [:query/OFFENSE-CODE-GROUP-drag-40]
                     :select        [:*]
                     :where         [:*if :kit-query--18381605500-agg/*.clicked
                                     [:*all= :kit-query--18381605500-agg/*.clicked [:YEAR]] [:and [:= :YEAR 2015]]]}]
          :highlight-columns {:OFFENSE-CODE-GROUP-drag-40 [:YEAR]}
          :name
            "When grouping by year and aggregating by :rows there are values that are higher than the mean (3853 for this particular aggregation) by 1152."
          :order 2
          :parameters {}
          :step-mutates {}}
         {:ask-mutates {"Highlight these in your source query?"
                          {[:panels :block-984 :queries :OFFENSE-CODE-GROUP-drag-40 :style-rules [:* :higlight455761056]]
                             {:logic [:and [:= :DISTRICT nil]]
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
                       [[:box :child ":DISTRICT"] [:box :child "nil" :style {:font-weight 400}]]]]] "as a wider aggregate:"
                    {:_h            4
                     :_query-id     :kit-query-4557610560-agg
                     :connection-id "boston-crime"
                     :from          [:query/OFFENSE-CODE-GROUP-drag-40]
                     :group-by      [:DISTRICT]
                     :order-by      [[2 :desc]]
                     :select        [:DISTRICT [[:count 1] :rows]]
                     :style-rules   {[:* :highlight-group] {:logic [:and [:= :DISTRICT nil]]
                                                            :style {:background-color "#008b8b66"
                                                                    :border           "1px solid #00000000"}}}}
                    "All rows in this group:"
                    {:_h            8
                     :_query-id     :kit-query-4557610560-detail
                     :connection-id "boston-crime"
                     :from          [:query/OFFENSE-CODE-GROUP-drag-40]
                     :select        [:*]
                     :where         [:*if :kit-query-4557610560-agg/*.clicked
                                     [:*all= :kit-query-4557610560-agg/*.clicked [:DISTRICT]] [:and [:= :DISTRICT nil]]]}]
          :highlight-columns {:OFFENSE-CODE-GROUP-drag-40 [:DISTRICT]}
          :name
            "When grouping by district and aggregating by :rows there are values that are higher than the mean (1185 for this particular aggregation) by 1134."
          :order 3
          :parameters {}
          :step-mutates {}}
         {:ask-mutates {"Highlight these in your source query?"
                          {[:panels :block-984 :queries :OFFENSE-CODE-GROUP-drag-40 :style-rules [:* :higlight-135911983]]
                             {:logic [:and [:= :DISTRICT "C11"] [:= :YEAR 2016]]
                              :style {:background-color "#008b8b66" :border "1px solid #00000000"}}}}
          :content [[:v-box :size "auto" :width "490px" :style {:font-size "13px" :opacity 0.33} :children
                     [[:h-box :size "auto" :justify :between :children
                       [[:box :child ":mean"] [:box :child "296.44232" :style {:font-weight 400}]]]
                      [:h-box :size "auto" :justify :between :children
                       [[:box :child ":standard-deviation"] [:box :child "203.27949813964352" :style {:font-weight 400}]]]
                      [:h-box :size "auto" :justify :between :children
                       [[:box :child ":sample-size"] [:box :child "52" :style {:font-weight 400}]]]
                      [:h-box :size "auto" :justify :between :children
                       [[:box :child ":fields"] [:box :child "[:DISTRICT :YEAR]" :style {:font-weight 400}]]]
                      [:h-box :size "auto" :justify :between :children
                       [[:box :child ":calc-used"] [:box :child "[:percentiles [0.01 0.99]]" :style {:font-weight 400}]]]]]
                    [:v-box :size "auto" :width "490px" :style {:font-size "16px"} :children
                     [[:h-box :size "auto" :justify :between :children
                       [[:box :child ":DISTRICT"] [:box :child "\"C11\"" :style {:font-weight 400}]]]
                      [:h-box :size "auto" :justify :between :children
                       [[:box :child ":YEAR"] [:box :child "2016" :style {:font-weight 400}]]]]] "as a wider aggregate:"
                    {:_h            4
                     :_query-id     :kit-query--1359119830-agg
                     :connection-id "boston-crime"
                     :from          [:query/OFFENSE-CODE-GROUP-drag-40]
                     :group-by      [:DISTRICT :YEAR]
                     :order-by      [[3 :desc]]
                     :select        [:DISTRICT :YEAR [[:count 1] :rows]]
                     :style-rules   {[:* :highlight-group] {:logic [:and [:= :DISTRICT "C11"] [:= :YEAR 2016]]
                                                            :style {:background-color "#008b8b66"
                                                                    :border           "1px solid #00000000"}}}}
                    "All rows in this group:"
                    {:_h            8
                     :_query-id     :kit-query--1359119830-detail
                     :connection-id "boston-crime"
                     :from          [:query/OFFENSE-CODE-GROUP-drag-40]
                     :select        [:*]
                     :where         [:*if :kit-query--1359119830-agg/*.clicked
                                     [:*all= :kit-query--1359119830-agg/*.clicked [:DISTRICT :YEAR]]
                                     [:and [:= :DISTRICT "C11"] [:= :YEAR 2016]]]}]
          :highlight-columns {:OFFENSE-CODE-GROUP-drag-40 [:DISTRICT :YEAR]}
          :name
            "When grouping by district, year and aggregating by :rows there are values that are higher than the mean (296 for this particular aggregation) by 516."
          :order 4
          :parameters {}
          :step-mutates {}}
         {:ask-mutates {"Highlight these in your source query?"
                          {[:panels :block-984 :queries :OFFENSE-CODE-GROUP-drag-40 :style-rules [:* :higlight522156221]]
                             {:logic [:and [:= :MONTH 8]] :style {:background-color "#008b8b66" :border "1px solid #00000000"}}}}
          :content [[:v-box :size "auto" :width "490px" :style {:font-size "13px" :opacity 0.33} :children
                     [[:h-box :size "auto" :justify :between :children
                       [[:box :child ":mean"] [:box :child "1284.5834" :style {:font-weight 400}]]]
                      [:h-box :size "auto" :justify :between :children
                       [[:box :child ":standard-deviation"] [:box :child "224.17681203807757" :style {:font-weight 400}]]]
                      [:h-box :size "auto" :justify :between :children
                       [[:box :child ":sample-size"] [:box :child "12" :style {:font-weight 400}]]]
                      [:h-box :size "auto" :justify :between :children
                       [[:box :child ":fields"] [:box :child "[:MONTH]" :style {:font-weight 400}]]]
                      [:h-box :size "auto" :justify :between :children
                       [[:box :child ":calc-used"] [:box :child "[:percentiles [0.01 0.99]]" :style {:font-weight 400}]]]]]
                    [:v-box :size "auto" :width "490px" :style {:font-size "16px"} :children
                     [[:h-box :size "auto" :justify :between :children
                       [[:box :child ":MONTH"] [:box :child "8" :style {:font-weight 400}]]]]] "as a wider aggregate:"
                    {:_h            4
                     :_query-id     :kit-query-5221562210-agg
                     :connection-id "boston-crime"
                     :from          [:query/OFFENSE-CODE-GROUP-drag-40]
                     :group-by      [:MONTH]
                     :order-by      [[2 :desc]]
                     :select        [:MONTH [[:count 1] :rows]]
                     :style-rules   {[:* :highlight-group] {:logic [:and [:= :MONTH 8]]
                                                            :style {:background-color "#008b8b66"
                                                                    :border           "1px solid #00000000"}}}}
                    "All rows in this group:"
                    {:_h            8
                     :_query-id     :kit-query-5221562210-detail
                     :connection-id "boston-crime"
                     :from          [:query/OFFENSE-CODE-GROUP-drag-40]
                     :select        [:*]
                     :where         [:*if :kit-query-5221562210-agg/*.clicked
                                     [:*all= :kit-query-5221562210-agg/*.clicked [:MONTH]] [:and [:= :MONTH 8]]]}]
          :highlight-columns {:OFFENSE-CODE-GROUP-drag-40 [:MONTH]}
          :name
            "When grouping by month and aggregating by :rows there are values that are higher than the mean (1284 for this particular aggregation) by 510."
          :order 5
          :parameters {}
          :step-mutates {}}
         {:ask-mutates {"Highlight these in your source query?"
                          {[:panels :block-984 :queries :OFFENSE-CODE-GROUP-drag-40 :style-rules [:* :higlight1898947211]]
                             {:logic [:and [:= :STREET "WASHINGTON ST"]]
                              :style {:background-color "#008b8b66" :border "1px solid #00000000"}}}}
          :content
            [[:v-box :size "auto" :width "490px" :style {:font-size "13px" :opacity 0.33} :children
              [[:h-box :size "auto" :justify :between :children
                [[:box :child ":mean"] [:box :child "6.781786" :style {:font-weight 400}]]]
               [:h-box :size "auto" :justify :between :children
                [[:box :child ":standard-deviation"] [:box :child "19.093579791187643" :style {:font-weight 400}]]]
               [:h-box :size "auto" :justify :between :children
                [[:box :child ":sample-size"] [:box :child "2273" :style {:font-weight 400}]]]
               [:h-box :size "auto" :justify :between :children
                [[:box :child ":fields"] [:box :child "[:STREET]" :style {:font-weight 400}]]]
               [:h-box :size "auto" :justify :between :children
                [[:box :child ":calc-used"] [:box :child "[:percentiles [0.01 0.99]]" :style {:font-weight 400}]]]]]
             [:v-box :size "auto" :width "490px" :style {:font-size "16px"} :children
              [[:h-box :size "auto" :justify :between :children
                [[:box :child ":STREET"] [:box :child "\"WASHINGTON ST\"" :style {:font-weight 400}]]]]] "as a wider aggregate:"
             {:_h            4
              :_query-id     :kit-query-18989472110-agg
              :connection-id "boston-crime"
              :from          [:query/OFFENSE-CODE-GROUP-drag-40]
              :group-by      [:STREET]
              :order-by      [[2 :desc]]
              :select        [:STREET [[:count 1] :rows]]
              :style-rules   {[:* :highlight-group] {:logic [:and [:= :STREET "WASHINGTON ST"]]
                                                     :style {:background-color "#008b8b66" :border "1px solid #00000000"}}}}
             "All rows in this group:"
             {:_h            8
              :_query-id     :kit-query-18989472110-detail
              :connection-id "boston-crime"
              :from          [:query/OFFENSE-CODE-GROUP-drag-40]
              :select        [:*]
              :where         [:*if :kit-query-18989472110-agg/*.clicked [:*all= :kit-query-18989472110-agg/*.clicked [:STREET]]
                              [:and [:= :STREET "WASHINGTON ST"]]]}]
          :highlight-columns {:OFFENSE-CODE-GROUP-drag-40 [:STREET]}
          :name
            "When grouping by street and aggregating by :rows there are values that are higher than the mean (6 for this particular aggregation) by 508."
          :order 6
          :parameters {}
          :step-mutates {}}
         {:ask-mutates {"Highlight these in your source query?"
                          {[:panels :block-984 :queries :OFFENSE-CODE-GROUP-drag-40 :style-rules [:* :higlight1105646847]]
                             {:logic [:and [:= :HOUR 4]] :style {:background-color "#008b8b66" :border "1px solid #00000000"}}}}
          :content [[:v-box :size "auto" :width "490px" :style {:font-size "13px" :opacity 0.33} :children
                     [[:h-box :size "auto" :justify :between :children
                       [[:box :child ":mean"] [:box :child "642.2917" :style {:font-weight 400}]]]
                      [:h-box :size "auto" :justify :between :children
                       [[:box :child ":standard-deviation"] [:box :child "221.0048037122471" :style {:font-weight 400}]]]
                      [:h-box :size "auto" :justify :between :children
                       [[:box :child ":sample-size"] [:box :child "24" :style {:font-weight 400}]]]
                      [:h-box :size "auto" :justify :between :children
                       [[:box :child ":fields"] [:box :child "[:HOUR]" :style {:font-weight 400}]]]
                      [:h-box :size "auto" :justify :between :children
                       [[:box :child ":calc-used"] [:box :child "[:percentiles [0.01 0.99]]" :style {:font-weight 400}]]]]]
                    [:v-box :size "auto" :width "490px" :style {:font-size "16px"} :children
                     [[:h-box :size "auto" :justify :between :children
                       [[:box :child ":HOUR"] [:box :child "4" :style {:font-weight 400}]]]]] "as a wider aggregate:"
                    {:_h            4
                     :_query-id     :kit-query-11056468470-agg
                     :connection-id "boston-crime"
                     :from          [:query/OFFENSE-CODE-GROUP-drag-40]
                     :group-by      [:HOUR]
                     :order-by      [[2 :desc]]
                     :select        [:HOUR [[:count 1] :rows]]
                     :style-rules   {[:* :highlight-group] {:logic [:and [:= :HOUR 4]]
                                                            :style {:background-color "#008b8b66"
                                                                    :border           "1px solid #00000000"}}}}
                    "All rows in this group:"
                    {:_h            8
                     :_query-id     :kit-query-11056468470-detail
                     :connection-id "boston-crime"
                     :from          [:query/OFFENSE-CODE-GROUP-drag-40]
                     :select        [:*]
                     :where         [:*if :kit-query-11056468470-agg/*.clicked
                                     [:*all= :kit-query-11056468470-agg/*.clicked [:HOUR]] [:and [:= :HOUR 4]]]}]
          :highlight-columns {:OFFENSE-CODE-GROUP-drag-40 [:HOUR]}
          :name
            "When grouping by hour and aggregating by :rows there are values that are higher than the mean (642 for this particular aggregation) by 426."
          :order 7
          :parameters {}
          :step-mutates {}}
         {:ask-mutates {"Highlight these in your source query?"
                          {[:panels :block-984 :queries :OFFENSE-CODE-GROUP-drag-40 :style-rules [:* :higlight-763922059]]
                             {:logic [:and [:= :MONTH 9] [:= :YEAR 2018]]
                              :style {:background-color "#008b8b66" :border "1px solid #00000000"}}}}
          :content
            [[:v-box :size "auto" :width "490px" :style {:font-size "13px" :opacity 0.33} :children
              [[:h-box :size "auto" :justify :between :children
                [[:box :child ":mean"] [:box :child "385.375" :style {:font-weight 400}]]]
               [:h-box :size "auto" :justify :between :children
                [[:box :child ":standard-deviation"] [:box :child "80.21461447267573" :style {:font-weight 400}]]]
               [:h-box :size "auto" :justify :between :children
                [[:box :child ":sample-size"] [:box :child "40" :style {:font-weight 400}]]]
               [:h-box :size "auto" :justify :between :children
                [[:box :child ":fields"] [:box :child "[:MONTH :YEAR]" :style {:font-weight 400}]]]
               [:h-box :size "auto" :justify :between :children
                [[:box :child ":calc-used"] [:box :child "[:percentiles [0.01 0.99]]" :style {:font-weight 400}]]]]]
             [:v-box :size "auto" :width "490px" :style {:font-size "16px"} :children
              [[:h-box :size "auto" :justify :between :children
                [[:box :child ":MONTH"] [:box :child "9" :style {:font-weight 400}]]]
               [:h-box :size "auto" :justify :between :children
                [[:box :child ":YEAR"] [:box :child "2018" :style {:font-weight 400}]]]]] "as a wider aggregate:"
             {:_h            4
              :_query-id     :kit-query--7639220590-agg
              :connection-id "boston-crime"
              :from          [:query/OFFENSE-CODE-GROUP-drag-40]
              :group-by      [:MONTH :YEAR]
              :order-by      [[3 :desc]]
              :select        [:MONTH :YEAR [[:count 1] :rows]]
              :style-rules   {[:* :highlight-group] {:logic [:and [:= :MONTH 9] [:= :YEAR 2018]]
                                                     :style {:background-color "#008b8b66" :border "1px solid #00000000"}}}}
             "All rows in this group:"
             {:_h            8
              :_query-id     :kit-query--7639220590-detail
              :connection-id "boston-crime"
              :from          [:query/OFFENSE-CODE-GROUP-drag-40]
              :select        [:*]
              :where         [:*if :kit-query--7639220590-agg/*.clicked
                              [:*all= :kit-query--7639220590-agg/*.clicked [:MONTH :YEAR]] [:and [:= :MONTH 9] [:= :YEAR 2018]]]}]
          :highlight-columns {:OFFENSE-CODE-GROUP-drag-40 [:MONTH :YEAR]}
          :name
            "When grouping by month, year and aggregating by :rows there are values that are higher than the mean (385 for this particular aggregation) by 353."
          :order 8
          :parameters {}
          :step-mutates {}}
         {:ask-mutates {"Highlight these in your source query?"
                          {[:panels :block-984 :queries :OFFENSE-CODE-GROUP-drag-40 :style-rules [:* :higlight-1104265126]]
                             {:logic [:and [:= :HOUR 18]] :style {:background-color "#008b8b66" :border "1px solid #00000000"}}}}
          :content [[:v-box :size "auto" :width "490px" :style {:font-size "13px" :opacity 0.33} :children
                     [[:h-box :size "auto" :justify :between :children
                       [[:box :child ":mean"] [:box :child "642.2917" :style {:font-weight 400}]]]
                      [:h-box :size "auto" :justify :between :children
                       [[:box :child ":standard-deviation"] [:box :child "221.0048037122471" :style {:font-weight 400}]]]
                      [:h-box :size "auto" :justify :between :children
                       [[:box :child ":sample-size"] [:box :child "24" :style {:font-weight 400}]]]
                      [:h-box :size "auto" :justify :between :children
                       [[:box :child ":fields"] [:box :child "[:HOUR]" :style {:font-weight 400}]]]
                      [:h-box :size "auto" :justify :between :children
                       [[:box :child ":calc-used"] [:box :child "[:percentiles [0.01 0.99]]" :style {:font-weight 400}]]]]]
                    [:v-box :size "auto" :width "490px" :style {:font-size "16px"} :children
                     [[:h-box :size "auto" :justify :between :children
                       [[:box :child ":HOUR"] [:box :child "18" :style {:font-weight 400}]]]]] "as a wider aggregate:"
                    {:_h            4
                     :_query-id     :kit-query--11042651260-agg
                     :connection-id "boston-crime"
                     :from          [:query/OFFENSE-CODE-GROUP-drag-40]
                     :group-by      [:HOUR]
                     :order-by      [[2 :desc]]
                     :select        [:HOUR [[:count 1] :rows]]
                     :style-rules   {[:* :highlight-group] {:logic [:and [:= :HOUR 18]]
                                                            :style {:background-color "#008b8b66"
                                                                    :border           "1px solid #00000000"}}}}
                    "All rows in this group:"
                    {:_h            8
                     :_query-id     :kit-query--11042651260-detail
                     :connection-id "boston-crime"
                     :from          [:query/OFFENSE-CODE-GROUP-drag-40]
                     :select        [:*]
                     :where         [:*if :kit-query--11042651260-agg/*.clicked
                                     [:*all= :kit-query--11042651260-agg/*.clicked [:HOUR]] [:and [:= :HOUR 18]]]}]
          :highlight-columns {:OFFENSE-CODE-GROUP-drag-40 [:HOUR]}
          :name
            "When grouping by hour and aggregating by :rows there are values that are higher than the mean (642 for this particular aggregation) by 334."
          :order 9
          :parameters {}
          :step-mutates {}}
         {:ask-mutates {"Highlight these in your source query?"
                          {[:panels :block-984 :queries :OFFENSE-CODE-GROUP-drag-40 :style-rules [:* :higlight685297608]]
                             {:logic [:and [:= :DISTRICT nil] [:= :YEAR 2015]]
                              :style {:background-color "#008b8b66" :border "1px solid #00000000"}}}}
          :content [[:v-box :size "auto" :width "490px" :style {:font-size "13px" :opacity 0.33} :children
                     [[:h-box :size "auto" :justify :between :children
                       [[:box :child ":mean"] [:box :child "296.44232" :style {:font-weight 400}]]]
                      [:h-box :size "auto" :justify :between :children
                       [[:box :child ":standard-deviation"] [:box :child "203.27949813964352" :style {:font-weight 400}]]]
                      [:h-box :size "auto" :justify :between :children
                       [[:box :child ":sample-size"] [:box :child "52" :style {:font-weight 400}]]]
                      [:h-box :size "auto" :justify :between :children
                       [[:box :child ":fields"] [:box :child "[:DISTRICT :YEAR]" :style {:font-weight 400}]]]
                      [:h-box :size "auto" :justify :between :children
                       [[:box :child ":calc-used"] [:box :child "[:percentiles [0.01 0.99]]" :style {:font-weight 400}]]]]]
                    [:v-box :size "auto" :width "490px" :style {:font-size "16px"} :children
                     [[:h-box :size "auto" :justify :between :children
                       [[:box :child ":DISTRICT"] [:box :child "nil" :style {:font-weight 400}]]]
                      [:h-box :size "auto" :justify :between :children
                       [[:box :child ":YEAR"] [:box :child "2015" :style {:font-weight 400}]]]]] "as a wider aggregate:"
                    {:_h            4
                     :_query-id     :kit-query-6852976080-agg
                     :connection-id "boston-crime"
                     :from          [:query/OFFENSE-CODE-GROUP-drag-40]
                     :group-by      [:DISTRICT :YEAR]
                     :order-by      [[3 :desc]]
                     :select        [:DISTRICT :YEAR [[:count 1] :rows]]
                     :style-rules   {[:* :highlight-group] {:logic [:and [:= :DISTRICT nil] [:= :YEAR 2015]]
                                                            :style {:background-color "#008b8b66"
                                                                    :border           "1px solid #00000000"}}}}
                    "All rows in this group:"
                    {:_h            8
                     :_query-id     :kit-query-6852976080-detail
                     :connection-id "boston-crime"
                     :from          [:query/OFFENSE-CODE-GROUP-drag-40]
                     :select        [:*]
                     :where         [:*if :kit-query-6852976080-agg/*.clicked
                                     [:*all= :kit-query-6852976080-agg/*.clicked [:DISTRICT :YEAR]]
                                     [:and [:= :DISTRICT nil] [:= :YEAR 2015]]]}]
          :highlight-columns {:OFFENSE-CODE-GROUP-drag-40 [:DISTRICT :YEAR]}
          :name
            "When grouping by district, year and aggregating by :rows there are values that are higher than the mean (296 for this particular aggregation) by 293."
          :order 10
          :parameters {}
          :step-mutates {}}
         {:ask-mutates {"Highlight these in your source query?"
                          {[:panels :block-984 :queries :OFFENSE-CODE-GROUP-drag-40 :style-rules [:* :higlight2091151179]]
                             {:logic [:and [:= :DAY_OF_WEEK "0001-01-07"] [:= :YEAR 2016]]
                              :style {:background-color "#008b8b66" :border "1px solid #00000000"}}}}
          :content [[:v-box :size "auto" :width "490px" :style {:font-size "13px" :opacity 0.33} :children
                     [[:h-box :size "auto" :justify :between :children
                       [[:box :child ":mean"] [:box :child "550.5357" :style {:font-weight 400}]]]
                      [:h-box :size "auto" :justify :between :children
                       [[:box :child ":standard-deviation"] [:box :child "164.0140416773727" :style {:font-weight 400}]]]
                      [:h-box :size "auto" :justify :between :children
                       [[:box :child ":sample-size"] [:box :child "28" :style {:font-weight 400}]]]
                      [:h-box :size "auto" :justify :between :children
                       [[:box :child ":fields"] [:box :child "[:DAY_OF_WEEK :YEAR]" :style {:font-weight 400}]]]
                      [:h-box :size "auto" :justify :between :children
                       [[:box :child ":calc-used"] [:box :child "[:percentiles [0.01 0.99]]" :style {:font-weight 400}]]]]]
                    [:v-box :size "auto" :width "490px" :style {:font-size "16px"} :children
                     [[:h-box :size "auto" :justify :between :children
                       [[:box :child ":DAY_OF_WEEK"] [:box :child "\"0001-01-07\"" :style {:font-weight 400}]]]
                      [:h-box :size "auto" :justify :between :children
                       [[:box :child ":YEAR"] [:box :child "2016" :style {:font-weight 400}]]]]] "as a wider aggregate:"
                    {:_h            4
                     :_query-id     :kit-query-20911511790-agg
                     :connection-id "boston-crime"
                     :from          [:query/OFFENSE-CODE-GROUP-drag-40]
                     :group-by      [:DAY_OF_WEEK :YEAR]
                     :order-by      [[3 :desc]]
                     :select        [:DAY_OF_WEEK :YEAR [[:count 1] :rows]]
                     :style-rules   {[:* :highlight-group] {:logic [:and [:= :DAY_OF_WEEK "0001-01-07"] [:= :YEAR 2016]]
                                                            :style {:background-color "#008b8b66"
                                                                    :border           "1px solid #00000000"}}}}
                    "All rows in this group:"
                    {:_h            8
                     :_query-id     :kit-query-20911511790-detail
                     :connection-id "boston-crime"
                     :from          [:query/OFFENSE-CODE-GROUP-drag-40]
                     :select        [:*]
                     :where         [:*if :kit-query-20911511790-agg/*.clicked
                                     [:*all= :kit-query-20911511790-agg/*.clicked [:DAY_OF_WEEK :YEAR]]
                                     [:and [:= :DAY_OF_WEEK "0001-01-07"] [:= :YEAR 2016]]]}]
          :highlight-columns {:OFFENSE-CODE-GROUP-drag-40 [:DAY_OF_WEEK :YEAR]}
          :name
            "When grouping by day_of_week, year and aggregating by :rows there are values that are higher than the mean (550 for this particular aggregation) by 279."
          :order 11
          :parameters {}
          :step-mutates {}}
         {:ask-mutates {"Highlight these in your source query?"
                          {[:panels :block-984 :queries :OFFENSE-CODE-GROUP-drag-40 :style-rules [:* :higlight-1094684804]]
                             {:logic [:and [:and [:= :STREET nil]] [:and [:= :STREET "BLUE HILL AVE"]]]
                              :style {:background-color "#008b8b66" :border "1px solid #00000000"}}}}
          :content
            [[:v-box :size "auto" :width "490px" :style {:font-size "13px" :opacity 0.33} :children
              [[:h-box :size "auto" :justify :between :children
                [[:box :child ":mean"] [:box :child "6.781786" :style {:font-weight 400}]]]
               [:h-box :size "auto" :justify :between :children
                [[:box :child ":standard-deviation"] [:box :child "19.093579791187643" :style {:font-weight 400}]]]
               [:h-box :size "auto" :justify :between :children
                [[:box :child ":sample-size"] [:box :child "2273" :style {:font-weight 400}]]]
               [:h-box :size "auto" :justify :between :children
                [[:box :child ":fields"] [:box :child "[:STREET]" :style {:font-weight 400}]]]
               [:h-box :size "auto" :justify :between :children
                [[:box :child ":calc-used"] [:box :child "[:percentiles [0.01 0.99]]" :style {:font-weight 400}]]]]]
             [:v-box :size "auto" :width "490px" :style {:font-size "16px"} :children
              [[:h-box :size "auto" :justify :between :children
                [[:box :child ":STREET"] [:box :child "nil" :style {:font-weight 400}]]]]] "as a wider aggregate:"
             {:_h            4
              :_query-id     :kit-query--10946848040-agg
              :connection-id "boston-crime"
              :from          [:query/OFFENSE-CODE-GROUP-drag-40]
              :group-by      [:STREET]
              :order-by      [[2 :desc]]
              :select        [:STREET [[:count 1] :rows]]
              :style-rules   {[:* :highlight-group] {:logic [:and [:= :STREET nil]]
                                                     :style {:background-color "#008b8b66" :border "1px solid #00000000"}}}}
             "All rows in this group:"
             {:_h            8
              :_query-id     :kit-query--10946848040-detail
              :connection-id "boston-crime"
              :from          [:query/OFFENSE-CODE-GROUP-drag-40]
              :select        [:*]
              :where         [:*if :kit-query--10946848040-agg/*.clicked [:*all= :kit-query--10946848040-agg/*.clicked [:STREET]]
                              [:and [:= :STREET nil]]]}
             [:v-box :size "auto" :width "490px" :style {:font-size "16px"} :children
              [[:h-box :size "auto" :justify :between :children
                [[:box :child ":STREET"] [:box :child "\"BLUE HILL AVE\"" :style {:font-weight 400}]]]]] "as a wider aggregate:"
             {:_h            4
              :_query-id     :kit-query--10946848041-agg
              :connection-id "boston-crime"
              :from          [:query/OFFENSE-CODE-GROUP-drag-40]
              :group-by      [:STREET]
              :order-by      [[2 :desc]]
              :select        [:STREET [[:count 1] :rows]]
              :style-rules   {[:* :highlight-group] {:logic [:and [:= :STREET "BLUE HILL AVE"]]
                                                     :style {:background-color "#008b8b66" :border "1px solid #00000000"}}}}
             "All rows in this group:"
             {:_h            8
              :_query-id     :kit-query--10946848041-detail
              :connection-id "boston-crime"
              :from          [:query/OFFENSE-CODE-GROUP-drag-40]
              :select        [:*]
              :where         [:*if :kit-query--10946848041-agg/*.clicked [:*all= :kit-query--10946848041-agg/*.clicked [:STREET]]
                              [:and [:= :STREET "BLUE HILL AVE"]]]}]
          :highlight-columns {:OFFENSE-CODE-GROUP-drag-40 [:STREET]}
          :name
            "When grouping by street and aggregating by :rows there are values that are higher than the mean (6 for this particular aggregation) by 270."
          :order 12
          :parameters {}
          :step-mutates {}}
         {:ask-mutates {"Highlight these in your source query?"
                          {[:panels :block-984 :queries :OFFENSE-CODE-GROUP-drag-40 :style-rules [:* :higlight-683042327]]
                             {:logic [:and [:= :STREET "DORCHESTER AVE"]]
                              :style {:background-color "#008b8b66" :border "1px solid #00000000"}}}}
          :content
            [[:v-box :size "auto" :width "490px" :style {:font-size "13px" :opacity 0.33} :children
              [[:h-box :size "auto" :justify :between :children
                [[:box :child ":mean"] [:box :child "6.781786" :style {:font-weight 400}]]]
               [:h-box :size "auto" :justify :between :children
                [[:box :child ":standard-deviation"] [:box :child "19.093579791187643" :style {:font-weight 400}]]]
               [:h-box :size "auto" :justify :between :children
                [[:box :child ":sample-size"] [:box :child "2273" :style {:font-weight 400}]]]
               [:h-box :size "auto" :justify :between :children
                [[:box :child ":fields"] [:box :child "[:STREET]" :style {:font-weight 400}]]]
               [:h-box :size "auto" :justify :between :children
                [[:box :child ":calc-used"] [:box :child "[:percentiles [0.01 0.99]]" :style {:font-weight 400}]]]]]
             [:v-box :size "auto" :width "490px" :style {:font-size "16px"} :children
              [[:h-box :size "auto" :justify :between :children
                [[:box :child ":STREET"] [:box :child "\"DORCHESTER AVE\"" :style {:font-weight 400}]]]]] "as a wider aggregate:"
             {:_h            4
              :_query-id     :kit-query--6830423270-agg
              :connection-id "boston-crime"
              :from          [:query/OFFENSE-CODE-GROUP-drag-40]
              :group-by      [:STREET]
              :order-by      [[2 :desc]]
              :select        [:STREET [[:count 1] :rows]]
              :style-rules   {[:* :highlight-group] {:logic [:and [:= :STREET "DORCHESTER AVE"]]
                                                     :style {:background-color "#008b8b66" :border "1px solid #00000000"}}}}
             "All rows in this group:"
             {:_h            8
              :_query-id     :kit-query--6830423270-detail
              :connection-id "boston-crime"
              :from          [:query/OFFENSE-CODE-GROUP-drag-40]
              :select        [:*]
              :where         [:*if :kit-query--6830423270-agg/*.clicked [:*all= :kit-query--6830423270-agg/*.clicked [:STREET]]
                              [:and [:= :STREET "DORCHESTER AVE"]]]}]
          :highlight-columns {:OFFENSE-CODE-GROUP-drag-40 [:STREET]}
          :name
            "When grouping by street and aggregating by :rows there are values that are higher than the mean (6 for this particular aggregation) by 235."
          :order 13
          :parameters {}
          :step-mutates {}}
         {:ask-mutates {"Highlight these in your source query?"
                          {[:panels :block-984 :queries :OFFENSE-CODE-GROUP-drag-40 :style-rules [:* :higlight-1556752600]]
                             {:logic [:and [:= :DAY_OF_WEEK "0001-01-06"]]
                              :style {:background-color "#008b8b66" :border "1px solid #00000000"}}}}
          :content
            [[:v-box :size "auto" :width "490px" :style {:font-size "13px" :opacity 0.33} :children
              [[:h-box :size "auto" :justify :between :children
                [[:box :child ":mean"] [:box :child "2202.1428" :style {:font-weight 400}]]]
               [:h-box :size "auto" :justify :between :children
                [[:box :child ":standard-deviation"] [:box :child "154.30158648515814" :style {:font-weight 400}]]]
               [:h-box :size "auto" :justify :between :children
                [[:box :child ":sample-size"] [:box :child "7" :style {:font-weight 400}]]]
               [:h-box :size "auto" :justify :between :children
                [[:box :child ":fields"] [:box :child "[:DAY_OF_WEEK]" :style {:font-weight 400}]]]
               [:h-box :size "auto" :justify :between :children
                [[:box :child ":calc-used"] [:box :child "[:percentiles [0.01 0.99]]" :style {:font-weight 400}]]]]]
             [:v-box :size "auto" :width "490px" :style {:font-size "16px"} :children
              [[:h-box :size "auto" :justify :between :children
                [[:box :child ":DAY_OF_WEEK"] [:box :child "\"0001-01-06\"" :style {:font-weight 400}]]]]] "as a wider aggregate:"
             {:_h            4
              :_query-id     :kit-query--15567526000-agg
              :connection-id "boston-crime"
              :from          [:query/OFFENSE-CODE-GROUP-drag-40]
              :group-by      [:DAY_OF_WEEK]
              :order-by      [[2 :desc]]
              :select        [:DAY_OF_WEEK [[:count 1] :rows]]
              :style-rules   {[:* :highlight-group] {:logic [:and [:= :DAY_OF_WEEK "0001-01-06"]]
                                                     :style {:background-color "#008b8b66" :border "1px solid #00000000"}}}}
             "All rows in this group:"
             {:_h            8
              :_query-id     :kit-query--15567526000-detail
              :connection-id "boston-crime"
              :from          [:query/OFFENSE-CODE-GROUP-drag-40]
              :select        [:*]
              :where         [:*if :kit-query--15567526000-agg/*.clicked
                              [:*all= :kit-query--15567526000-agg/*.clicked [:DAY_OF_WEEK]]
                              [:and [:= :DAY_OF_WEEK "0001-01-06"]]]}]
          :highlight-columns {:OFFENSE-CODE-GROUP-drag-40 [:DAY_OF_WEEK]}
          :name
            "When grouping by day_of_week and aggregating by :rows there are values that are higher than the mean (2202 for this particular aggregation) by 234."
          :order 14
          :parameters {}
          :step-mutates {}}
         {:ask-mutates {"Highlight these in your source query?"
                          {[:panels :block-984 :queries :OFFENSE-CODE-GROUP-drag-40 :style-rules [:* :higlight-1726351629]]
                             {:logic [:and [:= :MONTH 2]] :style {:background-color "#008b8b66" :border "1px solid #00000000"}}}}
          :content [[:v-box :size "auto" :width "490px" :style {:font-size "13px" :opacity 0.33} :children
                     [[:h-box :size "auto" :justify :between :children
                       [[:box :child ":mean"] [:box :child "1284.5834" :style {:font-weight 400}]]]
                      [:h-box :size "auto" :justify :between :children
                       [[:box :child ":standard-deviation"] [:box :child "224.17681203807757" :style {:font-weight 400}]]]
                      [:h-box :size "auto" :justify :between :children
                       [[:box :child ":sample-size"] [:box :child "12" :style {:font-weight 400}]]]
                      [:h-box :size "auto" :justify :between :children
                       [[:box :child ":fields"] [:box :child "[:MONTH]" :style {:font-weight 400}]]]
                      [:h-box :size "auto" :justify :between :children
                       [[:box :child ":calc-used"] [:box :child "[:percentiles [0.01 0.99]]" :style {:font-weight 400}]]]]]
                    [:v-box :size "auto" :width "490px" :style {:font-size "16px"} :children
                     [[:h-box :size "auto" :justify :between :children
                       [[:box :child ":MONTH"] [:box :child "2" :style {:font-weight 400}]]]]] "as a wider aggregate:"
                    {:_h            4
                     :_query-id     :kit-query--17263516290-agg
                     :connection-id "boston-crime"
                     :from          [:query/OFFENSE-CODE-GROUP-drag-40]
                     :group-by      [:MONTH]
                     :order-by      [[2 :desc]]
                     :select        [:MONTH [[:count 1] :rows]]
                     :style-rules   {[:* :highlight-group] {:logic [:and [:= :MONTH 2]]
                                                            :style {:background-color "#008b8b66"
                                                                    :border           "1px solid #00000000"}}}}
                    "All rows in this group:"
                    {:_h            8
                     :_query-id     :kit-query--17263516290-detail
                     :connection-id "boston-crime"
                     :from          [:query/OFFENSE-CODE-GROUP-drag-40]
                     :select        [:*]
                     :where         [:*if :kit-query--17263516290-agg/*.clicked
                                     [:*all= :kit-query--17263516290-agg/*.clicked [:MONTH]] [:and [:= :MONTH 2]]]}]
          :highlight-columns {:OFFENSE-CODE-GROUP-drag-40 [:MONTH]}
          :name
            "When grouping by month and aggregating by :rows there are values that are higher than the mean (1284 for this particular aggregation) by 225."
          :order 15
          :parameters {}
          :step-mutates {}}
         {:ask-mutates {"Highlight these in your source query?"
                          {[:panels :block-984 :queries :OFFENSE-CODE-GROUP-drag-40 :style-rules [:* :higlight1296097415]]
                             {:logic [:and [:= :DAY_OF_WEEK "0001-01-07"] [:= :DISTRICT "C11"]]
                              :style {:background-color "#008b8b66" :border "1px solid #00000000"}}}}
          :content [[:v-box :size "auto" :width "490px" :style {:font-size "13px" :opacity 0.33} :children
                     [[:h-box :size "auto" :justify :between :children
                       [[:box :child ":mean"] [:box :child "169.3956" :style {:font-weight 400}]]]
                      [:h-box :size "auto" :justify :between :children
                       [[:box :child ":standard-deviation"] [:box :child "103.99576059838641" :style {:font-weight 400}]]]
                      [:h-box :size "auto" :justify :between :children
                       [[:box :child ":sample-size"] [:box :child "91" :style {:font-weight 400}]]]
                      [:h-box :size "auto" :justify :between :children
                       [[:box :child ":fields"] [:box :child "[:DAY_OF_WEEK :DISTRICT]" :style {:font-weight 400}]]]
                      [:h-box :size "auto" :justify :between :children
                       [[:box :child ":calc-used"] [:box :child "[:percentiles [0.01 0.99]]" :style {:font-weight 400}]]]]]
                    [:v-box :size "auto" :width "490px" :style {:font-size "16px"} :children
                     [[:h-box :size "auto" :justify :between :children
                       [[:box :child ":DAY_OF_WEEK"] [:box :child "\"0001-01-07\"" :style {:font-weight 400}]]]
                      [:h-box :size "auto" :justify :between :children
                       [[:box :child ":DISTRICT"] [:box :child "\"C11\"" :style {:font-weight 400}]]]]] "as a wider aggregate:"
                    {:_h            4
                     :_query-id     :kit-query-12960974150-agg
                     :connection-id "boston-crime"
                     :from          [:query/OFFENSE-CODE-GROUP-drag-40]
                     :group-by      [:DAY_OF_WEEK :DISTRICT]
                     :order-by      [[3 :desc]]
                     :select        [:DAY_OF_WEEK :DISTRICT [[:count 1] :rows]]
                     :style-rules   {[:* :highlight-group] {:logic [:and [:= :DAY_OF_WEEK "0001-01-07"] [:= :DISTRICT "C11"]]
                                                            :style {:background-color "#008b8b66"
                                                                    :border           "1px solid #00000000"}}}}
                    "All rows in this group:"
                    {:_h            8
                     :_query-id     :kit-query-12960974150-detail
                     :connection-id "boston-crime"
                     :from          [:query/OFFENSE-CODE-GROUP-drag-40]
                     :select        [:*]
                     :where         [:*if :kit-query-12960974150-agg/*.clicked
                                     [:*all= :kit-query-12960974150-agg/*.clicked [:DAY_OF_WEEK :DISTRICT]]
                                     [:and [:= :DAY_OF_WEEK "0001-01-07"] [:= :DISTRICT "C11"]]]}]
          :highlight-columns {:OFFENSE-CODE-GROUP-drag-40 [:DAY_OF_WEEK :DISTRICT]}
          :name
            "When grouping by day_of_week, district and aggregating by :rows there are values that are higher than the mean (169 for this particular aggregation) by 215."
          :order 16
          :parameters {}
          :step-mutates {}}
         {:ask-mutates {"Highlight these in your source query?"
                          {[:panels :block-984 :queries :OFFENSE-CODE-GROUP-drag-40 :style-rules [:* :higlight512674475]]
                             {:logic [:and [:= :DAY_OF_WEEK "0001-01-03"] [:= :YEAR 2015]]
                              :style {:background-color "#008b8b66" :border "1px solid #00000000"}}}}
          :content [[:v-box :size "auto" :width "490px" :style {:font-size "13px" :opacity 0.33} :children
                     [[:h-box :size "auto" :justify :between :children
                       [[:box :child ":mean"] [:box :child "550.5357" :style {:font-weight 400}]]]
                      [:h-box :size "auto" :justify :between :children
                       [[:box :child ":standard-deviation"] [:box :child "164.0140416773727" :style {:font-weight 400}]]]
                      [:h-box :size "auto" :justify :between :children
                       [[:box :child ":sample-size"] [:box :child "28" :style {:font-weight 400}]]]
                      [:h-box :size "auto" :justify :between :children
                       [[:box :child ":fields"] [:box :child "[:DAY_OF_WEEK :YEAR]" :style {:font-weight 400}]]]
                      [:h-box :size "auto" :justify :between :children
                       [[:box :child ":calc-used"] [:box :child "[:percentiles [0.01 0.99]]" :style {:font-weight 400}]]]]]
                    [:v-box :size "auto" :width "490px" :style {:font-size "16px"} :children
                     [[:h-box :size "auto" :justify :between :children
                       [[:box :child ":DAY_OF_WEEK"] [:box :child "\"0001-01-03\"" :style {:font-weight 400}]]]
                      [:h-box :size "auto" :justify :between :children
                       [[:box :child ":YEAR"] [:box :child "2015" :style {:font-weight 400}]]]]] "as a wider aggregate:"
                    {:_h            4
                     :_query-id     :kit-query-5126744750-agg
                     :connection-id "boston-crime"
                     :from          [:query/OFFENSE-CODE-GROUP-drag-40]
                     :group-by      [:DAY_OF_WEEK :YEAR]
                     :order-by      [[3 :desc]]
                     :select        [:DAY_OF_WEEK :YEAR [[:count 1] :rows]]
                     :style-rules   {[:* :highlight-group] {:logic [:and [:= :DAY_OF_WEEK "0001-01-03"] [:= :YEAR 2015]]
                                                            :style {:background-color "#008b8b66"
                                                                    :border           "1px solid #00000000"}}}}
                    "All rows in this group:"
                    {:_h            8
                     :_query-id     :kit-query-5126744750-detail
                     :connection-id "boston-crime"
                     :from          [:query/OFFENSE-CODE-GROUP-drag-40]
                     :select        [:*]
                     :where         [:*if :kit-query-5126744750-agg/*.clicked
                                     [:*all= :kit-query-5126744750-agg/*.clicked [:DAY_OF_WEEK :YEAR]]
                                     [:and [:= :DAY_OF_WEEK "0001-01-03"] [:= :YEAR 2015]]]}]
          :highlight-columns {:OFFENSE-CODE-GROUP-drag-40 [:DAY_OF_WEEK :YEAR]}
          :name
            "When grouping by day_of_week, year and aggregating by :rows there are values that are higher than the mean (550 for this particular aggregation) by 208."
          :order 17
          :parameters {}
          :step-mutates {}}
         {:ask-mutates {"Highlight these in your source query?"
                          {[:panels :block-984 :queries :OFFENSE-CODE-GROUP-drag-40 :style-rules [:* :higlight-1114554230]]
                             {:logic [:and [:= :STREET "COMMONWEALTH AVE"]]
                              :style {:background-color "#008b8b66" :border "1px solid #00000000"}}}}
          :content
            [[:v-box :size "auto" :width "490px" :style {:font-size "13px" :opacity 0.33} :children
              [[:h-box :size "auto" :justify :between :children
                [[:box :child ":mean"] [:box :child "6.781786" :style {:font-weight 400}]]]
               [:h-box :size "auto" :justify :between :children
                [[:box :child ":standard-deviation"] [:box :child "19.093579791187643" :style {:font-weight 400}]]]
               [:h-box :size "auto" :justify :between :children
                [[:box :child ":sample-size"] [:box :child "2273" :style {:font-weight 400}]]]
               [:h-box :size "auto" :justify :between :children
                [[:box :child ":fields"] [:box :child "[:STREET]" :style {:font-weight 400}]]]
               [:h-box :size "auto" :justify :between :children
                [[:box :child ":calc-used"] [:box :child "[:percentiles [0.01 0.99]]" :style {:font-weight 400}]]]]]
             [:v-box :size "auto" :width "490px" :style {:font-size "16px"} :children
              [[:h-box :size "auto" :justify :between :children
                [[:box :child ":STREET"] [:box :child "\"COMMONWEALTH AVE\"" :style {:font-weight 400}]]]]]
             "as a wider aggregate:"
             {:_h            4
              :_query-id     :kit-query--11145542300-agg
              :connection-id "boston-crime"
              :from          [:query/OFFENSE-CODE-GROUP-drag-40]
              :group-by      [:STREET]
              :order-by      [[2 :desc]]
              :select        [:STREET [[:count 1] :rows]]
              :style-rules   {[:* :highlight-group] {:logic [:and [:= :STREET "COMMONWEALTH AVE"]]
                                                     :style {:background-color "#008b8b66" :border "1px solid #00000000"}}}}
             "All rows in this group:"
             {:_h            8
              :_query-id     :kit-query--11145542300-detail
              :connection-id "boston-crime"
              :from          [:query/OFFENSE-CODE-GROUP-drag-40]
              :select        [:*]
              :where         [:*if :kit-query--11145542300-agg/*.clicked [:*all= :kit-query--11145542300-agg/*.clicked [:STREET]]
                              [:and [:= :STREET "COMMONWEALTH AVE"]]]}]
          :highlight-columns {:OFFENSE-CODE-GROUP-drag-40 [:STREET]}
          :name
            "When grouping by street and aggregating by :rows there are values that are higher than the mean (6 for this particular aggregation) by 203."
          :order 18
          :parameters {}
          :step-mutates {}}
         {:ask-mutates {"Highlight these in your source query?"
                          {[:panels :block-984 :queries :OFFENSE-CODE-GROUP-drag-40 :style-rules [:* :higlight1434988132]]
                             {:logic [:and [:= :DISTRICT "C11"] [:= :STREET "DORCHESTER AVE"]]
                              :style {:background-color "#008b8b66" :border "1px solid #00000000"}}}}
          :content
            [[:v-box :size "auto" :width "490px" :style {:font-size "13px" :opacity 0.33} :children
              [[:h-box :size "auto" :justify :between :children
                [[:box :child ":mean"] [:box :child "6.0403605" :style {:font-weight 400}]]]
               [:h-box :size "auto" :justify :between :children
                [[:box :child ":standard-deviation"] [:box :child "11.367280405615126" :style {:font-weight 400}]]]
               [:h-box :size "auto" :justify :between :children
                [[:box :child ":sample-size"] [:box :child "2552" :style {:font-weight 400}]]]
               [:h-box :size "auto" :justify :between :children
                [[:box :child ":fields"] [:box :child "[:DISTRICT :STREET]" :style {:font-weight 400}]]]
               [:h-box :size "auto" :justify :between :children
                [[:box :child ":calc-used"] [:box :child "[:percentiles [0.01 0.99]]" :style {:font-weight 400}]]]]]
             [:v-box :size "auto" :width "490px" :style {:font-size "16px"} :children
              [[:h-box :size "auto" :justify :between :children
                [[:box :child ":DISTRICT"] [:box :child "\"C11\"" :style {:font-weight 400}]]]
               [:h-box :size "auto" :justify :between :children
                [[:box :child ":STREET"] [:box :child "\"DORCHESTER AVE\"" :style {:font-weight 400}]]]]] "as a wider aggregate:"
             {:_h            4
              :_query-id     :kit-query-14349881320-agg
              :connection-id "boston-crime"
              :from          [:query/OFFENSE-CODE-GROUP-drag-40]
              :group-by      [:DISTRICT :STREET]
              :order-by      [[3 :desc]]
              :select        [:DISTRICT :STREET [[:count 1] :rows]]
              :style-rules   {[:* :highlight-group] {:logic [:and [:= :DISTRICT "C11"] [:= :STREET "DORCHESTER AVE"]]
                                                     :style {:background-color "#008b8b66" :border "1px solid #00000000"}}}}
             "All rows in this group:"
             {:_h            8
              :_query-id     :kit-query-14349881320-detail
              :connection-id "boston-crime"
              :from          [:query/OFFENSE-CODE-GROUP-drag-40]
              :select        [:*]
              :where         [:*if :kit-query-14349881320-agg/*.clicked
                              [:*all= :kit-query-14349881320-agg/*.clicked [:DISTRICT :STREET]]
                              [:and [:= :DISTRICT "C11"] [:= :STREET "DORCHESTER AVE"]]]}]
          :highlight-columns {:OFFENSE-CODE-GROUP-drag-40 [:DISTRICT :STREET]}
          :name
            "When grouping by district, street and aggregating by :rows there are values that are higher than the mean (6 for this particular aggregation) by 187."
          :order 19
          :parameters {}
          :step-mutates {}}]
      :description "Top 20 Outlier by Severity"
      :mutates {}
      :options {:actions? false :pages? true :search? false}
      :parameters {}}
   ;;        :dupoes {:data [{:ask-mutates {"Highlight these in your source query?" {[:panels :block-984 :queries
   ;;        :OFFENSE-CODE-GROUP-drag-40 :style-rules [:* :higlight-1584150552]] {:logic [:and
   ;;        [:= :DISTRICT "B2"]],
   ;;                                                                                                                                                                   :style
   ;;                                                                                                                                                                   {:background-color
   ;;                                                                                                                                                                   "#008b8b66",
   ;;                                                                                                                                                                   :border
   ;;                                                                                                                                                                   "1px
   ;;                                                                                                                                                                   solid
   ;;                                                                                                                                                                   #00000000"}}}},
   ;;          :content [[:v-box
   ;;                     :size "auto" :width "490px" :style
   ;;                     {:font-size "13px", :opacity 0.33}
   ;;                     :children
   ;;                     [[:h-box :size "auto" :justify :between :children [[:box :child
   ;;                     ":mean"] [:box
   ;;                     :child "1185.7693" :style {:font-weight 400}]]]
   ;;                      [:h-box :size "auto" :justify :between :children [[:box :child
   ;;                      ":standard-deviation"] [:box :child "713.1985540610642" :style
   ;;                      {:font-weight
   ;;                      400}]]]
   ;;                      [:h-box :size "auto" :justify :between :children [[:box :child
   ;;                      ":sample-size"]
   ;;                      [:box :child "13" :style {:font-weight 400}]]]
   ;;                      [:h-box :size "auto" :justify :between :children [[:box :child
   ;;                      ":fields"]
   ;;                      [:box :child "[:DISTRICT]" :style {:font-weight 400}]]]
   ;;                      [:h-box :size "auto" :justify :between :children [[:box :child
   ;;                      ":calc-used"]
   ;;                      [:box :child "[:percentiles [0.01 0.99]]" :style {:font-weight
   ;;                      400}]]]]]
   ;;                    [:v-box :size "auto" :width "490px" :style {:font-size "16px"}
   ;;                    :children
   ;;                    [[:h-box :size "auto" :justify :between :children [[:box :child
   ;;                    ":DISTRICT"]
   ;;                    [:box :child "\"B2\"" :style {:font-weight 400}]]]]]
   ;;                    "as a wider aggregate:"
   ;;                    {:_h 4,
   ;;                     :_query-id :kit-query--15841505520-agg, :connection-id "boston-crime", :from
   ;;                     [:query/OFFENSE-CODE-GROUP-drag-40], :group-by [:DISTRICT], :order-by
   ;;                     [[2
   ;;                     :desc]], :select [:DISTRICT [[:count 1] :rows]], :style-rules {[:*
   ;;                     :highlight-group] {:logic [:and [:= :DISTRICT "B2"]], :style
   ;;                     {:background-color "#008b8b66", :border "1px solid #00000000"}}}}
   ;;                    "All rows in this group:"
   ;;                    {:_h 8,
   ;;                     :_query-id :kit-query--15841505520-detail, :connection-id "boston-crime", :from
   ;;                     [:query/OFFENSE-CODE-GROUP-drag-40], :select [:*], :where [:*if
   ;;                     :kit-query--15841505520-agg/*.clicked [:*all= :kit-query--15841505520-agg/*.clicked
   ;;                     [:DISTRICT]] [:and [:= :DISTRICT "B2"]]]}],
   ;;          :highlight-columns {:OFFENSE-CODE-GROUP-drag-40 [:DISTRICT]}, :name "When
   ;;          grouping by district and aggregating by :rows there are values that are higher
   ;;          than the mean (1185 for this particular aggregation) by 1250.", :order 0,
   ;;          :parameters {},
   ;;          :step-mutates {}}
   ;;         {:ask-mutates {"Highlight these in your source query?" {[:panels :block-984
   ;;         :queries :OFFENSE-CODE-GROUP-drag-40 :style-rules [:* :higlight1640170157]]
   ;;         {:logic [:and
   ;;         [:= :YEAR 2016]],
   ;;                                                                                                                                                                  :style
   ;;                                                                                                                                                                  {:background-color
   ;;                                                                                                                                                                  "#008b8b66",
   ;;                                                                                                                                                                  :border
   ;;                                                                                                                                                                  "1px
   ;;                                                                                                                                                                  solid
   ;;                                                                                                                                                                  #00000000"}}}},
   ;;          :content [[:v-box
   ;;                     :size "auto" :width "490px" :style
   ;;                     {:font-size "13px", :opacity 0.33}
   ;;                     :children
   ;;                     [[:h-box :size "auto" :justify :between :children [[:box :child
   ;;                     ":mean"] [:box
   ;;                     :child "3853.75" :style {:font-weight 400}]]]
   ;;                      [:h-box :size "auto" :justify :between :children [[:box :child
   ;;                      ":standard-deviation"] [:box :child "1099.883942741233" :style
   ;;                      {:font-weight
   ;;                      400}]]]
   ;;                      [:h-box :size "auto" :justify :between :children [[:box :child
   ;;                      ":sample-size"]
   ;;                      [:box :child "4" :style {:font-weight 400}]]]
   ;;                      [:h-box :size "auto" :justify :between :children [[:box :child
   ;;                      ":fields"]
   ;;                      [:box :child "[:YEAR]" :style {:font-weight 400}]]]
   ;;                      [:h-box :size "auto" :justify :between :children [[:box :child
   ;;                      ":calc-used"]
   ;;                      [:box :child "[:percentiles [0.01 0.99]]" :style {:font-weight
   ;;                      400}]]]]]
   ;;                    [:v-box :size "auto" :width "490px" :style {:font-size "16px"}
   ;;                    :children
   ;;                    [[:h-box :size "auto" :justify :between :children [[:box :child
   ;;                    ":YEAR"] [:box
   ;;                    :child "2016" :style {:font-weight 400}]]]]]
   ;;                    "as a wider aggregate:"
   ;;                    {:_h 4,
   ;;                     :_query-id :kit-query-16401701570-agg, :connection-id "boston-crime", :from
   ;;                     [:query/OFFENSE-CODE-GROUP-drag-40], :group-by [:YEAR], :order-by [[2
   ;;                     :desc]], :select [:YEAR [[:count 1] :rows]], :style-rules {[:*
   ;;                     :highlight-group] {:logic [:and [:= :YEAR 2016]], :style
   ;;                     {:background-color "#008b8b66", :border "1px solid #00000000"}}}}
   ;;                    "All rows in this group:"
   ;;                    {:_h 8,
   ;;                     :_query-id :kit-query-16401701570-detail, :connection-id "boston-crime", :from
   ;;                     [:query/OFFENSE-CODE-GROUP-drag-40], :select [:*], :where [:*if
   ;;                     :kit-query-16401701570-agg/*.clicked [:*all= :kit-query-16401701570-agg/*.clicked
   ;;                     [:YEAR]] [:and [:= :YEAR 2016]]]}],
   ;;          :highlight-columns {:OFFENSE-CODE-GROUP-drag-40 [:YEAR]}, :name "When grouping
   ;;          by year and aggregating by :rows there are values that are higher than the mean
   ;;          (3853 for this particular aggregation) by 1209.", :order 1, :parameters {},
   ;;          :step-mutates {}}
   ;;         {:ask-mutates {"Highlight these in your source query?" {[:panels :block-984
   ;;         :queries :OFFENSE-CODE-GROUP-drag-40 :style-rules [:* :higlight-1838160550]]
   ;;         {:logic [:and
   ;;         [:= :YEAR 2015]],
   ;;                                                                                                                                                                   :style
   ;;                                                                                                                                                                   {:background-color
   ;;                                                                                                                                                                   "#008b8b66",
   ;;                                                                                                                                                                   :border
   ;;                                                                                                                                                                   "1px
   ;;                                                                                                                                                                   solid
   ;;                                                                                                                                                                   #00000000"}}}},
   ;;          :content [[:v-box
   ;;                     :size "auto" :width "490px" :style
   ;;                     {:font-size "13px", :opacity 0.33}
   ;;                     :children
   ;;                     [[:h-box :size "auto" :justify :between :children [[:box :child
   ;;                     ":mean"] [:box
   ;;                     :child "3853.75" :style {:font-weight 400}]]]
   ;;                      [:h-box :size "auto" :justify :between :children [[:box :child
   ;;                      ":standard-deviation"] [:box :child "1099.883942741233" :style
   ;;                      {:font-weight
   ;;                      400}]]]
   ;;                      [:h-box :size "auto" :justify :between :children [[:box :child
   ;;                      ":sample-size"]
   ;;                      [:box :child "4" :style {:font-weight 400}]]]
   ;;                      [:h-box :size "auto" :justify :between :children [[:box :child
   ;;                      ":fields"]
   ;;                      [:box :child "[:YEAR]" :style {:font-weight 400}]]]
   ;;                      [:h-box :size "auto" :justify :between :children [[:box :child
   ;;                      ":calc-used"]
   ;;                      [:box :child "[:percentiles [0.01 0.99]]" :style {:font-weight
   ;;                      400}]]]]]
   ;;                    [:v-box :size "auto" :width "490px" :style {:font-size "16px"}
   ;;                    :children
   ;;                    [[:h-box :size "auto" :justify :between :children [[:box :child
   ;;                    ":YEAR"] [:box
   ;;                    :child "2015" :style {:font-weight 400}]]]]]
   ;;                    "as a wider aggregate:"
   ;;                    {:_h 4,
   ;;                     :_query-id :kit-query--18381605500-agg, :connection-id "boston-crime", :from
   ;;                     [:query/OFFENSE-CODE-GROUP-drag-40], :group-by [:YEAR], :order-by [[2
   ;;                     :desc]], :select [:YEAR [[:count 1] :rows]], :style-rules {[:*
   ;;                     :highlight-group] {:logic [:and [:= :YEAR 2015]], :style
   ;;                     {:background-color "#008b8b66", :border "1px solid #00000000"}}}}
   ;;                    "All rows in this group:"
   ;;                    {:_h 8,
   ;;                     :_query-id :kit-query--18381605500-detail, :connection-id "boston-crime", :from
   ;;                     [:query/OFFENSE-CODE-GROUP-drag-40], :select [:*], :where [:*if
   ;;                     :kit-query--18381605500-agg/*.clicked [:*all= :kit-query--18381605500-agg/*.clicked
   ;;                     [:YEAR]] [:and [:= :YEAR 2015]]]}],
   ;;          :highlight-columns {:OFFENSE-CODE-GROUP-drag-40 [:YEAR]}, :name "When grouping
   ;;          by year and aggregating by :rows there are values that are higher than the mean
   ;;          (3853 for this particular aggregation) by 1152.", :order 2, :parameters {},
   ;;          :step-mutates {}}
   ;;         {:ask-mutates {"Highlight these in your source query?" {[:panels :block-984
   ;;         :queries :OFFENSE-CODE-GROUP-drag-40 :style-rules [:* :higlight455761056]]
   ;;         {:logic [:and
   ;;         [:= :DISTRICT nil]],
   ;;                                                                                                                                                                 :style
   ;;                                                                                                                                                                 {:background-color
   ;;                                                                                                                                                                 "#008b8b66",
   ;;                                                                                                                                                                 :border
   ;;                                                                                                                                                                 "1px
   ;;                                                                                                                                                                 solid
   ;;                                                                                                                                                                 #00000000"}}}},
   ;;          :content [[:v-box
   ;;                     :size "auto" :width "490px" :style
   ;;                     {:font-size "13px", :opacity 0.33}
   ;;                     :children
   ;;                     [[:h-box :size "auto" :justify :between :children [[:box :child
   ;;                     ":mean"] [:box
   ;;                     :child "1185.7693" :style {:font-weight 400}]]]
   ;;                      [:h-box :size "auto" :justify :between :children [[:box :child
   ;;                      ":standard-deviation"] [:box :child "713.1985540610642" :style
   ;;                      {:font-weight
   ;;                      400}]]]
   ;;                      [:h-box :size "auto" :justify :between :children [[:box :child
   ;;                      ":sample-size"]
   ;;                      [:box :child "13" :style {:font-weight 400}]]]
   ;;                      [:h-box :size "auto" :justify :between :children [[:box :child
   ;;                      ":fields"]
   ;;                      [:box :child "[:DISTRICT]" :style {:font-weight 400}]]]
   ;;                      [:h-box :size "auto" :justify :between :children [[:box :child
   ;;                      ":calc-used"]
   ;;                      [:box :child "[:percentiles [0.01 0.99]]" :style {:font-weight
   ;;                      400}]]]]]
   ;;                    [:v-box :size "auto" :width "490px" :style {:font-size "16px"}
   ;;                    :children
   ;;                    [[:h-box :size "auto" :justify :between :children [[:box :child
   ;;                    ":DISTRICT"]
   ;;                    [:box :child "nil" :style {:font-weight 400}]]]]]
   ;;                    "as a wider aggregate:"
   ;;                    {:_h 4,
   ;;                     :_query-id :kit-query-4557610560-agg, :connection-id "boston-crime", :from
   ;;                     [:query/OFFENSE-CODE-GROUP-drag-40], :group-by [:DISTRICT], :order-by
   ;;                     [[2
   ;;                     :desc]], :select [:DISTRICT [[:count 1] :rows]], :style-rules {[:*
   ;;                     :highlight-group] {:logic [:and [:= :DISTRICT nil]], :style
   ;;                     {:background-color "#008b8b66", :border "1px solid #00000000"}}}}
   ;;                    "All rows in this group:"
   ;;                    {:_h 8,
   ;;                     :_query-id :kit-query-4557610560-detail, :connection-id "boston-crime", :from
   ;;                     [:query/OFFENSE-CODE-GROUP-drag-40], :select [:*], :where [:*if
   ;;                     :kit-query-4557610560-agg/*.clicked [:*all=
   ;;                     :kit-query-4557610560-agg/*.clicked
   ;;                     [:DISTRICT]] [:and [:= :DISTRICT nil]]]}],
   ;;          :highlight-columns {:OFFENSE-CODE-GROUP-drag-40 [:DISTRICT]}, :name "When
   ;;          grouping by district and aggregating by :rows there are values that are higher
   ;;          than the mean (1185 for this particular aggregation) by 1134.", :order 3,
   ;;          :parameters {},
   ;;          :step-mutates {}}
   ;;         {:ask-mutates {"Highlight these in your source query?" {[:panels :block-984
   ;;         :queries :OFFENSE-CODE-GROUP-drag-40 :style-rules [:* :higlight-135911983]]
   ;;         {:logic [:and
   ;;         [:= :DISTRICT "C11"] [:= :YEAR 2016]],
   ;;                                                                                                                                                                  :style
   ;;                                                                                                                                                                  {:background-color
   ;;                                                                                                                                                                  "#008b8b66",
   ;;                                                                                                                                                                  :border
   ;;                                                                                                                                                                  "1px
   ;;                                                                                                                                                                  solid
   ;;                                                                                                                                                                  #00000000"}}}},
   ;;          :content [[:v-box
   ;;                     :size "auto" :width "490px" :style
   ;;                     {:font-size "13px", :opacity 0.33}
   ;;                     :children
   ;;                     [[:h-box :size "auto" :justify :between :children [[:box :child
   ;;                     ":mean"] [:box
   ;;                     :child "296.44232" :style {:font-weight 400}]]]
   ;;                      [:h-box :size "auto" :justify :between :children [[:box :child
   ;;                      ":standard-deviation"] [:box :child "203.27949813964352" :style
   ;;                      {:font-weight
   ;;                      400}]]]
   ;;                      [:h-box :size "auto" :justify :between :children [[:box :child
   ;;                      ":sample-size"]
   ;;                      [:box :child "52" :style {:font-weight 400}]]]
   ;;                      [:h-box :size "auto" :justify :between :children [[:box :child
   ;;                      ":fields"]
   ;;                      [:box :child "[:DISTRICT :YEAR]" :style {:font-weight 400}]]]
   ;;                      [:h-box :size "auto" :justify :between :children [[:box :child
   ;;                      ":calc-used"]
   ;;                      [:box :child "[:percentiles [0.01 0.99]]" :style {:font-weight
   ;;                      400}]]]]]
   ;;                    [:v-box
   ;;                     :size "auto" :width "490px" :style
   ;;                     {:font-size "16px"}
   ;;                     :children
   ;;                     [[:h-box :size "auto" :justify :between :children [[:box :child
   ;;                     ":DISTRICT"]
   ;;                     [:box :child "\"C11\"" :style {:font-weight 400}]]]
   ;;                      [:h-box :size "auto" :justify :between :children [[:box :child
   ;;                      ":YEAR"] [:box
   ;;                      :child "2016" :style {:font-weight 400}]]]]]
   ;;                    "as a wider aggregate:"
   ;;                    {:_h 4,
   ;;                     :_query-id :kit-query--1359119830-agg, :connection-id "boston-crime", :from
   ;;                     [:query/OFFENSE-CODE-GROUP-drag-40], :group-by [:DISTRICT :YEAR],
   ;;                     :order-by [[3 :desc]], :select [:DISTRICT :YEAR [[:count 1] :rows]], :style-rules
   ;;                     {[:* :highlight-group] {:logic [:and [:= :DISTRICT "C11"] [:= :YEAR
   ;;                     2016]], :style {:background-color "#008b8b66", :border "1px solid
   ;;                     #00000000"}}}}
   ;;                    "All rows in this group:"
   ;;                    {:_h 8,
   ;;                     :_query-id :kit-query--1359119830-detail, :connection-id "boston-crime", :from
   ;;                     [:query/OFFENSE-CODE-GROUP-drag-40], :select [:*], :where [:*if
   ;;                     :kit-query--1359119830-agg/*.clicked [:*all= :kit-query--1359119830-agg/*.clicked
   ;;                     [:DISTRICT :YEAR]] [:and [:= :DISTRICT "C11"] [:= :YEAR 2016]]]}],
   ;;          :highlight-columns {:OFFENSE-CODE-GROUP-drag-40 [:DISTRICT :YEAR]}, :name "When
   ;;          grouping by district, year and aggregating by :rows there are values that are
   ;;          higher than the mean
   ;;          (296 for this particular aggregation) by 516.", :order 4, :parameters {},
   ;;          :step-mutates {}}
   ;;         {:ask-mutates {"Highlight these in your source query?" {[:panels :block-984
   ;;         :queries :OFFENSE-CODE-GROUP-drag-40 :style-rules [:* :higlight522156221]]
   ;;         {:logic [:and
   ;;         [:= :MONTH 8]],
   ;;                                                                                                                                                                 :style
   ;;                                                                                                                                                                 {:background-color
   ;;                                                                                                                                                                 "#008b8b66",
   ;;                                                                                                                                                                 :border
   ;;                                                                                                                                                                 "1px
   ;;                                                                                                                                                                 solid
   ;;                                                                                                                                                                 #00000000"}}}},
   ;;          :content [[:v-box
   ;;                     :size "auto" :width "490px" :style
   ;;                     {:font-size "13px", :opacity 0.33}
   ;;                     :children
   ;;                     [[:h-box :size "auto" :justify :between :children [[:box :child
   ;;                     ":mean"] [:box
   ;;                     :child "1284.5834" :style {:font-weight 400}]]]
   ;;                      [:h-box :size "auto" :justify :between :children [[:box :child
   ;;                      ":standard-deviation"] [:box :child "224.17681203807757" :style
   ;;                      {:font-weight
   ;;                      400}]]]
   ;;                      [:h-box :size "auto" :justify :between :children [[:box :child
   ;;                      ":sample-size"]
   ;;                      [:box :child "12" :style {:font-weight 400}]]]
   ;;                      [:h-box :size "auto" :justify :between :children [[:box :child
   ;;                      ":fields"]
   ;;                      [:box :child "[:MONTH]" :style {:font-weight 400}]]]
   ;;                      [:h-box :size "auto" :justify :between :children [[:box :child
   ;;                      ":calc-used"]
   ;;                      [:box :child "[:percentiles [0.01 0.99]]" :style {:font-weight
   ;;                      400}]]]]]
   ;;                    [:v-box :size "auto" :width "490px" :style {:font-size "16px"}
   ;;                    :children
   ;;                    [[:h-box :size "auto" :justify :between :children [[:box :child
   ;;                    ":MONTH"] [:box
   ;;                    :child "8" :style {:font-weight 400}]]]]]
   ;;                    "as a wider aggregate:"
   ;;                    {:_h 4,
   ;;                     :_query-id :kit-query-5221562210-agg, :connection-id "boston-crime", :from
   ;;                     [:query/OFFENSE-CODE-GROUP-drag-40], :group-by [:MONTH], :order-by
   ;;                     [[2
   ;;                     :desc]], :select [:MONTH [[:count 1] :rows]], :style-rules {[:*
   ;;                     :highlight-group] {:logic [:and [:= :MONTH 8]], :style
   ;;                     {:background-color "#008b8b66", :border "1px solid #00000000"}}}}
   ;;                    "All rows in this group:"
   ;;                    {:_h 8,
   ;;                     :_query-id :kit-query-5221562210-detail, :connection-id "boston-crime", :from
   ;;                     [:query/OFFENSE-CODE-GROUP-drag-40], :select [:*], :where [:*if
   ;;                     :kit-query-5221562210-agg/*.clicked [:*all=
   ;;                     :kit-query-5221562210-agg/*.clicked
   ;;                     [:MONTH]] [:and [:= :MONTH 8]]]}],
   ;;          :highlight-columns {:OFFENSE-CODE-GROUP-drag-40 [:MONTH]}, :name "When grouping
   ;;          by month and aggregating by :rows there are values that are higher than the mean
   ;;          (1284 for this particular aggregation) by 510.", :order 5, :parameters {},
   ;;          :step-mutates {}}
   ;;         {:ask-mutates {"Highlight these in your source query?" {[:panels :block-984
   ;;         :queries :OFFENSE-CODE-GROUP-drag-40 :style-rules [:* :higlight1898947211]]
   ;;         {:logic [:and
   ;;         [:= :STREET "WASHINGTON ST"]],
   ;;                                                                                                                                                                  :style
   ;;                                                                                                                                                                  {:background-color
   ;;                                                                                                                                                                  "#008b8b66",
   ;;                                                                                                                                                                  :border
   ;;                                                                                                                                                                  "1px
   ;;                                                                                                                                                                  solid
   ;;                                                                                                                                                                  #00000000"}}}},
   ;;          :content [[:v-box
   ;;                     :size "auto" :width "490px" :style
   ;;                     {:font-size "13px", :opacity 0.33}
   ;;                     :children
   ;;                     [[:h-box :size "auto" :justify :between :children [[:box :child
   ;;                     ":mean"] [:box
   ;;                     :child "6.781786" :style {:font-weight 400}]]]
   ;;                      [:h-box :size "auto" :justify :between :children [[:box :child
   ;;                      ":standard-deviation"] [:box :child "19.093579791187643" :style
   ;;                      {:font-weight
   ;;                      400}]]]
   ;;                      [:h-box :size "auto" :justify :between :children [[:box :child
   ;;                      ":sample-size"]
   ;;                      [:box :child "2273" :style {:font-weight 400}]]]
   ;;                      [:h-box :size "auto" :justify :between :children [[:box :child
   ;;                      ":fields"]
   ;;                      [:box :child "[:STREET]" :style {:font-weight 400}]]]
   ;;                      [:h-box :size "auto" :justify :between :children [[:box :child
   ;;                      ":calc-used"]
   ;;                      [:box :child "[:percentiles [0.01 0.99]]" :style {:font-weight
   ;;                      400}]]]]]
   ;;                    [:v-box :size "auto" :width "490px" :style {:font-size "16px"}
   ;;                    :children
   ;;                    [[:h-box :size "auto" :justify :between :children [[:box :child
   ;;                    ":STREET"] [:box
   ;;                    :child "\"WASHINGTON ST\"" :style {:font-weight 400}]]]]]
   ;;                    "as a wider aggregate:"
   ;;                    {:_h 4,
   ;;                     :_query-id :kit-query-18989472110-agg, :connection-id "boston-crime", :from
   ;;                     [:query/OFFENSE-CODE-GROUP-drag-40], :group-by [:STREET], :order-by
   ;;                     [[2
   ;;                     :desc]], :select [:STREET [[:count 1] :rows]], :style-rules {[:*
   ;;                     :highlight-group] {:logic [:and [:= :STREET "WASHINGTON
   ;;                     ST"]], :style {:background-color "#008b8b66", :border "1px solid
   ;;                     #00000000"}}}}
   ;;                    "All rows in this group:"
   ;;                    {:_h 8,
   ;;                     :_query-id :kit-query-18989472110-detail, :connection-id "boston-crime", :from
   ;;                     [:query/OFFENSE-CODE-GROUP-drag-40], :select [:*], :where [:*if
   ;;                     :kit-query-18989472110-agg/*.clicked [:*all= :kit-query-18989472110-agg/*.clicked
   ;;                     [:STREET]] [:and [:= :STREET "WASHINGTON
   ;;                     ST"]]]}],
   ;;          :highlight-columns {:OFFENSE-CODE-GROUP-drag-40 [:STREET]}, :name "When grouping
   ;;          by street and aggregating by :rows there are values that are higher than the
   ;;          mean (6 for this particular aggregation) by 508.", :order 6, :parameters {},
   ;;          :step-mutates {}}
   ;;         {:ask-mutates {"Highlight these in your source query?" {[:panels :block-984
   ;;         :queries :OFFENSE-CODE-GROUP-drag-40 :style-rules [:* :higlight1105646847]]
   ;;         {:logic [:and
   ;;         [:= :HOUR 4]],
   ;;                                                                                                                                                                  :style
   ;;                                                                                                                                                                  {:background-color
   ;;                                                                                                                                                                  "#008b8b66",
   ;;                                                                                                                                                                  :border
   ;;                                                                                                                                                                  "1px
   ;;                                                                                                                                                                  solid
   ;;                                                                                                                                                                  #00000000"}}}},
   ;;          :content [[:v-box
   ;;                     :size "auto" :width "490px" :style
   ;;                     {:font-size "13px", :opacity 0.33}
   ;;                     :children
   ;;                     [[:h-box :size "auto" :justify :between :children [[:box :child
   ;;                     ":mean"] [:box
   ;;                     :child "642.2917" :style {:font-weight 400}]]]
   ;;                      [:h-box :size "auto" :justify :between :children [[:box :child
   ;;                      ":standard-deviation"] [:box :child "221.0048037122471" :style
   ;;                      {:font-weight
   ;;                      400}]]]
   ;;                      [:h-box :size "auto" :justify :between :children [[:box :child
   ;;                      ":sample-size"]
   ;;                      [:box :child "24" :style {:font-weight 400}]]]
   ;;                      [:h-box :size "auto" :justify :between :children [[:box :child
   ;;                      ":fields"]
   ;;                      [:box :child "[:HOUR]" :style {:font-weight 400}]]]
   ;;                      [:h-box :size "auto" :justify :between :children [[:box :child
   ;;                      ":calc-used"]
   ;;                      [:box :child "[:percentiles [0.01 0.99]]" :style {:font-weight
   ;;                      400}]]]]]
   ;;                    [:v-box :size "auto" :width "490px" :style {:font-size "16px"}
   ;;                    :children
   ;;                    [[:h-box :size "auto" :justify :between :children [[:box :child
   ;;                    ":HOUR"] [:box
   ;;                    :child "4" :style {:font-weight 400}]]]]]
   ;;                    "as a wider aggregate:"
   ;;                    {:_h 4,
   ;;                     :_query-id :kit-query-11056468470-agg, :connection-id "boston-crime", :from
   ;;                     [:query/OFFENSE-CODE-GROUP-drag-40], :group-by [:HOUR], :order-by [[2
   ;;                     :desc]], :select [:HOUR [[:count 1] :rows]], :style-rules {[:*
   ;;                     :highlight-group] {:logic [:and [:= :HOUR 4]], :style
   ;;                     {:background-color "#008b8b66", :border "1px solid #00000000"}}}}
   ;;                    "All rows in this group:"
   ;;                    {:_h 8,
   ;;                     :_query-id :kit-query-11056468470-detail, :connection-id "boston-crime", :from
   ;;                     [:query/OFFENSE-CODE-GROUP-drag-40], :select [:*], :where [:*if
   ;;                     :kit-query-11056468470-agg/*.clicked [:*all= :kit-query-11056468470-agg/*.clicked
   ;;                     [:HOUR]] [:and [:= :HOUR 4]]]}],
   ;;          :highlight-columns {:OFFENSE-CODE-GROUP-drag-40 [:HOUR]}, :name "When grouping
   ;;          by hour and aggregating by :rows there are values that are higher than the mean
   ;;          (642 for this particular aggregation) by 426.", :order 7, :parameters {},
   ;;          :step-mutates {}}
   ;;         {:ask-mutates {"Highlight these in your source query?" {[:panels :block-984
   ;;         :queries :OFFENSE-CODE-GROUP-drag-40 :style-rules [:* :higlight-763922059]]
   ;;         {:logic [:and
   ;;         [:= :MONTH 9] [:= :YEAR 2018]],
   ;;                                                                                                                                                                  :style
   ;;                                                                                                                                                                  {:background-color
   ;;                                                                                                                                                                  "#008b8b66",
   ;;                                                                                                                                                                  :border
   ;;                                                                                                                                                                  "1px
   ;;                                                                                                                                                                  solid
   ;;                                                                                                                                                                  #00000000"}}}},
   ;;          :content [[:v-box
   ;;                     :size "auto" :width "490px" :style
   ;;                     {:font-size "13px", :opacity 0.33}
   ;;                     :children
   ;;                     [[:h-box :size "auto" :justify :between :children [[:box :child
   ;;                     ":mean"] [:box
   ;;                     :child "385.375" :style {:font-weight 400}]]]
   ;;                      [:h-box :size "auto" :justify :between :children [[:box :child
   ;;                      ":standard-deviation"] [:box :child "80.21461447267573" :style
   ;;                      {:font-weight
   ;;                      400}]]]
   ;;                      [:h-box :size "auto" :justify :between :children [[:box :child
   ;;                      ":sample-size"]
   ;;                      [:box :child "40" :style {:font-weight 400}]]]
   ;;                      [:h-box :size "auto" :justify :between :children [[:box :child
   ;;                      ":fields"]
   ;;                      [:box :child "[:MONTH :YEAR]" :style {:font-weight 400}]]]
   ;;                      [:h-box :size "auto" :justify :between :children [[:box :child
   ;;                      ":calc-used"]
   ;;                      [:box :child "[:percentiles [0.01 0.99]]" :style {:font-weight
   ;;                      400}]]]]]
   ;;                    [:v-box
   ;;                     :size "auto" :width "490px" :style
   ;;                     {:font-size "16px"}
   ;;                     :children
   ;;                     [[:h-box :size "auto" :justify :between :children [[:box :child
   ;;                     ":MONTH"] [:box
   ;;                     :child "9" :style {:font-weight 400}]]]
   ;;                      [:h-box :size "auto" :justify :between :children [[:box :child
   ;;                      ":YEAR"] [:box
   ;;                      :child "2018" :style {:font-weight 400}]]]]]
   ;;                    "as a wider aggregate:"
   ;;                    {:_h 4,
   ;;                     :_query-id :kit-query--7639220590-agg, :connection-id "boston-crime", :from
   ;;                     [:query/OFFENSE-CODE-GROUP-drag-40], :group-by [:MONTH :YEAR],
   ;;                     :order-by [[3 :desc]], :select [:MONTH :YEAR [[:count 1] :rows]],
   ;;                     :style-rules {[:* :highlight-group] {:logic [:and [:= :MONTH 9] [:=
   ;;                     :YEAR 2018]], :style {:background-color "#008b8b66", :border "1px
   ;;                     solid
   ;;                     #00000000"}}}}
   ;;                    "All rows in this group:"
   ;;                    {:_h 8,
   ;;                     :_query-id :kit-query--7639220590-detail, :connection-id "boston-crime", :from
   ;;                     [:query/OFFENSE-CODE-GROUP-drag-40], :select [:*], :where [:*if
   ;;                     :kit-query--7639220590-agg/*.clicked [:*all= :kit-query--7639220590-agg/*.clicked [:MONTH
   ;;                     :YEAR]] [:and [:= :MONTH 9] [:= :YEAR 2018]]]}],
   ;;          :highlight-columns {:OFFENSE-CODE-GROUP-drag-40 [:MONTH :YEAR]}, :name "When
   ;;          grouping by month, year and aggregating by :rows there are values that are
   ;;          higher than the mean (385 for this particular aggregation) by 353.", :order 8,
   ;;          :parameters {},
   ;;          :step-mutates {}}
   ;;         {:ask-mutates {"Highlight these in your source query?" {[:panels :block-984
   ;;         :queries :OFFENSE-CODE-GROUP-drag-40 :style-rules [:* :higlight-1104265126]]
   ;;         {:logic [:and
   ;;         [:= :HOUR 18]],
   ;;                                                                                                                                                                   :style
   ;;                                                                                                                                                                   {:background-color
   ;;                                                                                                                                                                   "#008b8b66",
   ;;                                                                                                                                                                   :border
   ;;                                                                                                                                                                   "1px
   ;;                                                                                                                                                                   solid
   ;;                                                                                                                                                                   #00000000"}}}},
   ;;          :content [[:v-box
   ;;                     :size "auto" :width "490px" :style
   ;;                     {:font-size "13px", :opacity 0.33}
   ;;                     :children
   ;;                     [[:h-box :size "auto" :justify :between :children [[:box :child
   ;;                     ":mean"] [:box
   ;;                     :child "642.2917" :style {:font-weight 400}]]]
   ;;                      [:h-box :size "auto" :justify :between :children [[:box :child
   ;;                      ":standard-deviation"] [:box :child "221.0048037122471" :style
   ;;                      {:font-weight
   ;;                      400}]]]
   ;;                      [:h-box :size "auto" :justify :between :children [[:box :child
   ;;                      ":sample-size"]
   ;;                      [:box :child "24" :style {:font-weight 400}]]]
   ;;                      [:h-box :size "auto" :justify :between :children [[:box :child
   ;;                      ":fields"]
   ;;                      [:box :child "[:HOUR]" :style {:font-weight 400}]]]
   ;;                      [:h-box :size "auto" :justify :between :children [[:box :child
   ;;                      ":calc-used"]
   ;;                      [:box :child "[:percentiles [0.01 0.99]]" :style {:font-weight
   ;;                      400}]]]]]
   ;;                    [:v-box :size "auto" :width "490px" :style {:font-size "16px"}
   ;;                    :children
   ;;                    [[:h-box :size "auto" :justify :between :children [[:box :child
   ;;                    ":HOUR"] [:box
   ;;                    :child "18" :style {:font-weight 400}]]]]]
   ;;                    "as a wider aggregate:"
   ;;                    {:_h 4,
   ;;                     :_query-id :kit-query--11042651260-agg, :connection-id "boston-crime", :from
   ;;                     [:query/OFFENSE-CODE-GROUP-drag-40], :group-by [:HOUR], :order-by [[2
   ;;                     :desc]], :select [:HOUR [[:count 1] :rows]], :style-rules {[:*
   ;;                     :highlight-group] {:logic [:and [:= :HOUR 18]], :style
   ;;                     {:background-color "#008b8b66", :border "1px solid #00000000"}}}}
   ;;                    "All rows in this group:"
   ;;                    {:_h 8,
   ;;                     :_query-id :kit-query--11042651260-detail, :connection-id "boston-crime", :from
   ;;                     [:query/OFFENSE-CODE-GROUP-drag-40], :select [:*], :where [:*if
   ;;                     :kit-query--11042651260-agg/*.clicked [:*all= :kit-query--11042651260-agg/*.clicked
   ;;                     [:HOUR]] [:and [:= :HOUR 18]]]}],
   ;;          :highlight-columns {:OFFENSE-CODE-GROUP-drag-40 [:HOUR]}, :name "When grouping
   ;;          by hour and aggregating by :rows there are values that are higher than the mean
   ;;          (642 for this particular aggregation) by 334.", :order 9, :parameters {},
   ;;          :step-mutates {}}
   ;;         {:ask-mutates {"Highlight these in your source query?" {[:panels :block-984
   ;;         :queries :OFFENSE-CODE-GROUP-drag-40 :style-rules [:* :higlight685297608]]
   ;;         {:logic [:and
   ;;         [:= :DISTRICT nil] [:= :YEAR 2015]],
   ;;                                                                                                                                                                 :style
   ;;                                                                                                                                                                 {:background-color
   ;;                                                                                                                                                                 "#008b8b66",
   ;;                                                                                                                                                                 :border
   ;;                                                                                                                                                                 "1px
   ;;                                                                                                                                                                 solid
   ;;                                                                                                                                                                 #00000000"}}}},
   ;;          :content [[:v-box
   ;;                     :size "auto" :width "490px" :style
   ;;                     {:font-size "13px", :opacity 0.33}
   ;;                     :children
   ;;                     [[:h-box :size "auto" :justify :between :children [[:box :child
   ;;                     ":mean"] [:box
   ;;                     :child "296.44232" :style {:font-weight 400}]]]
   ;;                      [:h-box :size "auto" :justify :between :children [[:box :child
   ;;                      ":standard-deviation"] [:box :child "203.27949813964352" :style
   ;;                      {:font-weight
   ;;                      400}]]]
   ;;                      [:h-box :size "auto" :justify :between :children [[:box :child
   ;;                      ":sample-size"]
   ;;                      [:box :child "52" :style {:font-weight 400}]]]
   ;;                      [:h-box :size "auto" :justify :between :children [[:box :child
   ;;                      ":fields"]
   ;;                      [:box :child "[:DISTRICT :YEAR]" :style {:font-weight 400}]]]
   ;;                      [:h-box :size "auto" :justify :between :children [[:box :child
   ;;                      ":calc-used"]
   ;;                      [:box :child "[:percentiles [0.01 0.99]]" :style {:font-weight
   ;;                      400}]]]]]
   ;;                    [:v-box
   ;;                     :size "auto" :width "490px" :style
   ;;                     {:font-size "16px"}
   ;;                     :children
   ;;                     [[:h-box :size "auto" :justify :between :children [[:box :child
   ;;                     ":DISTRICT"]
   ;;                     [:box :child "nil" :style {:font-weight 400}]]]
   ;;                      [:h-box :size "auto" :justify :between :children [[:box :child
   ;;                      ":YEAR"] [:box
   ;;                      :child "2015" :style {:font-weight 400}]]]]]
   ;;                    "as a wider aggregate:"
   ;;                    {:_h 4,
   ;;                     :_query-id :kit-query-6852976080-agg, :connection-id "boston-crime", :from
   ;;                     [:query/OFFENSE-CODE-GROUP-drag-40], :group-by [:DISTRICT :YEAR],
   ;;                     :order-by [[3 :desc]], :select [:DISTRICT :YEAR [[:count 1] :rows]], :style-rules
   ;;                     {[:* :highlight-group] {:logic [:and [:= :DISTRICT nil] [:= :YEAR
   ;;                     2015]], :style {:background-color "#008b8b66", :border "1px solid
   ;;                     #00000000"}}}}
   ;;                    "All rows in this group:"
   ;;                    {:_h 8,
   ;;                     :_query-id :kit-query-6852976080-detail, :connection-id "boston-crime", :from
   ;;                     [:query/OFFENSE-CODE-GROUP-drag-40], :select [:*], :where [:*if
   ;;                     :kit-query-6852976080-agg/*.clicked [:*all=
   ;;                     :kit-query-6852976080-agg/*.clicked
   ;;                     [:DISTRICT :YEAR]] [:and [:= :DISTRICT nil] [:= :YEAR 2015]]]}],
   ;;          :highlight-columns {:OFFENSE-CODE-GROUP-drag-40 [:DISTRICT :YEAR]}, :name "When
   ;;          grouping by district, year and aggregating by :rows there are values that are
   ;;          higher than the mean
   ;;          (296 for this particular aggregation) by 293.", :order 10, :parameters {},
   ;;          :step-mutates {}}
   ;;         {:ask-mutates {"Highlight these in your source query?" {[:panels :block-984
   ;;         :queries :OFFENSE-CODE-GROUP-drag-40 :style-rules [:* :higlight2091151179]]
   ;;         {:logic [:and
   ;;         [:= :DAY_OF_WEEK "0001-01-07"] [:= :YEAR 2016]],
   ;;                                                                                                                                                                  :style
   ;;                                                                                                                                                                  {:background-color
   ;;                                                                                                                                                                  "#008b8b66",
   ;;                                                                                                                                                                  :border
   ;;                                                                                                                                                                  "1px
   ;;                                                                                                                                                                  solid
   ;;                                                                                                                                                                  #00000000"}}}},
   ;;          :content [[:v-box
   ;;                     :size "auto" :width "490px" :style
   ;;                     {:font-size "13px", :opacity 0.33}
   ;;                     :children
   ;;                     [[:h-box :size "auto" :justify :between :children [[:box :child
   ;;                     ":mean"] [:box
   ;;                     :child "550.5357" :style {:font-weight 400}]]]
   ;;                      [:h-box :size "auto" :justify :between :children [[:box :child
   ;;                      ":standard-deviation"] [:box :child "164.0140416773727" :style
   ;;                      {:font-weight
   ;;                      400}]]]
   ;;                      [:h-box :size "auto" :justify :between :children [[:box :child
   ;;                      ":sample-size"]
   ;;                      [:box :child "28" :style {:font-weight 400}]]]
   ;;                      [:h-box :size "auto" :justify :between :children [[:box :child
   ;;                      ":fields"]
   ;;                      [:box :child "[:DAY_OF_WEEK :YEAR]" :style {:font-weight 400}]]]
   ;;                      [:h-box :size "auto" :justify :between :children [[:box :child
   ;;                      ":calc-used"]
   ;;                      [:box :child "[:percentiles [0.01 0.99]]" :style {:font-weight
   ;;                      400}]]]]]
   ;;                    [:v-box
   ;;                     :size "auto" :width "490px" :style
   ;;                     {:font-size "16px"}
   ;;                     :children
   ;;                     [[:h-box :size "auto" :justify :between :children [[:box :child
   ;;                     ":DAY_OF_WEEK"]
   ;;                     [:box :child "\"0001-01-07\"" :style {:font-weight 400}]]]
   ;;                      [:h-box :size "auto" :justify :between :children [[:box :child
   ;;                      ":YEAR"] [:box
   ;;                      :child "2016" :style {:font-weight 400}]]]]]
   ;;                    "as a wider aggregate:"
   ;;                    {:_h 4,
   ;;                     :_query-id :kit-query-20911511790-agg, :connection-id "boston-crime", :from
   ;;                     [:query/OFFENSE-CODE-GROUP-drag-40], :group-by [:DAY_OF_WEEK :YEAR],
   ;;                     :order-by
   ;;                     [[3 :desc]], :select [:DAY_OF_WEEK :YEAR [[:count 1] :rows]],
   ;;                     :style-rules {[:* :highlight-group] {:logic [:and [:= :DAY_OF_WEEK
   ;;                     "0001-01-07"] [:= :YEAR 2016]], :style {:background-color
   ;;                     "#008b8b66", :border
   ;;                     "1px solid #00000000"}}}}
   ;;                    "All rows in this group:"
   ;;                    {:_h 8,
   ;;                     :_query-id :kit-query-20911511790-detail, :connection-id "boston-crime", :from
   ;;                     [:query/OFFENSE-CODE-GROUP-drag-40], :select [:*], :where [:*if
   ;;                     :kit-query-20911511790-agg/*.clicked [:*all= :kit-query-20911511790-agg/*.clicked
   ;;                     [:DAY_OF_WEEK :YEAR]] [:and [:= :DAY_OF_WEEK "0001-01-07"] [:= :YEAR 2016]]]}],
   ;;          :highlight-columns {:OFFENSE-CODE-GROUP-drag-40 [:DAY_OF_WEEK :YEAR]}, :name
   ;;          "When grouping by day_of_week, year and aggregating by :rows there are values
   ;;          that are higher than the mean (550 for this particular aggregation) by 279.",
   ;;          :order 11, :parameters {},
   ;;          :step-mutates {}}
   ;;         {:ask-mutates {"Highlight these in your source query?" {[:panels :block-984
   ;;         :queries :OFFENSE-CODE-GROUP-drag-40 :style-rules [:* :higlight-1094684804]]
   ;;         {:logic [:and
   ;;         [:and [:= :STREET nil]] [:and [:= :STREET "BLUE HILL AVE"]]],
   ;;                                                                                                                                                                   :style
   ;;                                                                                                                                                                   {:background-color
   ;;                                                                                                                                                                   "#008b8b66",
   ;;                                                                                                                                                                   :border
   ;;                                                                                                                                                                   "1px
   ;;                                                                                                                                                                   solid
   ;;                                                                                                                                                                   #00000000"}}}},
   ;;          :content [[:v-box
   ;;                     :size "auto" :width "490px" :style
   ;;                     {:font-size "13px", :opacity 0.33}
   ;;                     :children
   ;;                     [[:h-box :size "auto" :justify :between :children [[:box :child
   ;;                     ":mean"] [:box
   ;;                     :child "6.781786" :style {:font-weight 400}]]]
   ;;                      [:h-box :size "auto" :justify :between :children [[:box :child
   ;;                      ":standard-deviation"] [:box :child "19.093579791187643" :style
   ;;                      {:font-weight
   ;;                      400}]]]
   ;;                      [:h-box :size "auto" :justify :between :children [[:box :child
   ;;                      ":sample-size"]
   ;;                      [:box :child "2273" :style {:font-weight 400}]]]
   ;;                      [:h-box :size "auto" :justify :between :children [[:box :child
   ;;                      ":fields"]
   ;;                      [:box :child "[:STREET]" :style {:font-weight 400}]]]
   ;;                      [:h-box :size "auto" :justify :between :children [[:box :child
   ;;                      ":calc-used"]
   ;;                      [:box :child "[:percentiles [0.01 0.99]]" :style {:font-weight
   ;;                      400}]]]]]
   ;;                    [:v-box :size "auto" :width "490px" :style {:font-size "16px"}
   ;;                    :children
   ;;                    [[:h-box :size "auto" :justify :between :children [[:box :child
   ;;                    ":STREET"] [:box
   ;;                    :child "nil" :style {:font-weight 400}]]]]]
   ;;                    "as a wider aggregate:"
   ;;                    {:_h 4,
   ;;                     :_query-id :kit-query--10946848040-agg, :connection-id "boston-crime", :from
   ;;                     [:query/OFFENSE-CODE-GROUP-drag-40], :group-by [:STREET], :order-by
   ;;                     [[2
   ;;                     :desc]], :select [:STREET [[:count 1] :rows]], :style-rules {[:*
   ;;                     :highlight-group] {:logic [:and [:= :STREET nil]], :style
   ;;                     {:background-color "#008b8b66", :border "1px solid #00000000"}}}}
   ;;                    "All rows in this group:"
   ;;                    {:_h 8,
   ;;                     :_query-id :kit-query--10946848040-detail, :connection-id "boston-crime", :from
   ;;                     [:query/OFFENSE-CODE-GROUP-drag-40], :select [:*],
   ;;                     :where [:*if :kit-query--10946848040-agg/*.clicked [:*all= :kit-query--10946848040-agg/*.clicked
   ;;                     [:STREET]] [:and [:= :STREET
   ;;                     nil]]]}
   ;;                    [:v-box :size "auto" :width "490px" :style {:font-size "16px"}
   ;;                    :children
   ;;                    [[:h-box :size "auto" :justify :between :children [[:box :child
   ;;                    ":STREET"] [:box
   ;;                    :child "\"BLUE HILL AVE\"" :style {:font-weight 400}]]]]]
   ;;                    "as a wider aggregate:"
   ;;                    {:_h 4,
   ;;                     :_query-id :kit-query--10946848041-agg, :connection-id "boston-crime", :from
   ;;                     [:query/OFFENSE-CODE-GROUP-drag-40], :group-by [:STREET], :order-by
   ;;                     [[2
   ;;                     :desc]], :select [:STREET [[:count 1] :rows]], :style-rules {[:*
   ;;                     :highlight-group] {:logic [:and [:= :STREET "BLUE HILL
   ;;                     AVE"]], :style {:background-color "#008b8b66", :border "1px solid
   ;;                     #00000000"}}}}
   ;;                    "All rows in this group:"
   ;;                    {:_h 8,
   ;;                     :_query-id :kit-query--10946848041-detail, :connection-id "boston-crime", :from
   ;;                     [:query/OFFENSE-CODE-GROUP-drag-40], :select [:*], :where [:*if
   ;;                     :kit-query--10946848041-agg/*.clicked [:*all= :kit-query--10946848041-agg/*.clicked
   ;;                     [:STREET]] [:and [:= :STREET "BLUE HILL
   ;;                     AVE"]]]}],
   ;;          :highlight-columns {:OFFENSE-CODE-GROUP-drag-40 [:STREET]}, :name "When grouping
   ;;          by street and aggregating by :rows there are values that are higher than the
   ;;          mean (6 for this particular aggregation) by 270.", :order 12, :parameters {},
   ;;          :step-mutates {}}
   ;;         {:ask-mutates {"Highlight these in your source query?" {[:panels :block-984
   ;;         :queries :OFFENSE-CODE-GROUP-drag-40 :style-rules [:* :higlight-683042327]]
   ;;         {:logic [:and
   ;;         [:= :STREET "DORCHESTER AVE"]],
   ;;                                                                                                                                                                  :style
   ;;                                                                                                                                                                  {:background-color
   ;;                                                                                                                                                                  "#008b8b66",
   ;;                                                                                                                                                                  :border
   ;;                                                                                                                                                                  "1px
   ;;                                                                                                                                                                  solid
   ;;                                                                                                                                                                  #00000000"}}}},
   ;;          :content [[:v-box
   ;;                     :size "auto" :width "490px" :style
   ;;                     {:font-size "13px", :opacity 0.33}
   ;;                     :children
   ;;                     [[:h-box :size "auto" :justify :between :children [[:box :child
   ;;                     ":mean"] [:box
   ;;                     :child "6.781786" :style {:font-weight 400}]]]
   ;;                      [:h-box :size "auto" :justify :between :children [[:box :child
   ;;                      ":standard-deviation"] [:box :child "19.093579791187643" :style
   ;;                      {:font-weight
   ;;                      400}]]]
   ;;                      [:h-box :size "auto" :justify :between :children [[:box :child
   ;;                      ":sample-size"]
   ;;                      [:box :child "2273" :style {:font-weight 400}]]]
   ;;                      [:h-box :size "auto" :justify :between :children [[:box :child
   ;;                      ":fields"]
   ;;                      [:box :child "[:STREET]" :style {:font-weight 400}]]]
   ;;                      [:h-box :size "auto" :justify :between :children [[:box :child
   ;;                      ":calc-used"]
   ;;                      [:box :child "[:percentiles [0.01 0.99]]" :style {:font-weight
   ;;                      400}]]]]]
   ;;                    [:v-box :size "auto" :width "490px" :style {:font-size "16px"}
   ;;                    :children
   ;;                    [[:h-box :size "auto" :justify :between :children [[:box :child
   ;;                    ":STREET"] [:box
   ;;                    :child "\"DORCHESTER AVE\"" :style {:font-weight 400}]]]]]
   ;;                    "as a wider aggregate:"
   ;;                    {:_h 4,
   ;;                     :_query-id :kit-query--6830423270-agg, :connection-id "boston-crime", :from
   ;;                     [:query/OFFENSE-CODE-GROUP-drag-40], :group-by [:STREET], :order-by
   ;;                     [[2
   ;;                     :desc]], :select [:STREET [[:count 1] :rows]], :style-rules {[:*
   ;;                     :highlight-group] {:logic [:and [:= :STREET "DORCHESTER
   ;;                     AVE"]], :style {:background-color "#008b8b66", :border "1px solid
   ;;                     #00000000"}}}}
   ;;                    "All rows in this group:"
   ;;                    {:_h 8,
   ;;                     :_query-id :kit-query--6830423270-detail, :connection-id "boston-crime", :from
   ;;                     [:query/OFFENSE-CODE-GROUP-drag-40], :select [:*], :where [:*if
   ;;                     :kit-query--6830423270-agg/*.clicked [:*all= :kit-query--6830423270-agg/*.clicked
   ;;                     [:STREET]] [:and [:= :STREET "DORCHESTER
   ;;                     AVE"]]]}],
   ;;          :highlight-columns {:OFFENSE-CODE-GROUP-drag-40 [:STREET]}, :name "When grouping
   ;;          by street and aggregating by :rows there are values that are higher than the
   ;;          mean (6 for this particular aggregation) by 235.", :order 13, :parameters {},
   ;;          :step-mutates {}}
   ;;         {:ask-mutates {"Highlight these in your source query?" {[:panels :block-984
   ;;         :queries :OFFENSE-CODE-GROUP-drag-40 :style-rules [:* :higlight-1556752600]]
   ;;         {:logic [:and
   ;;         [:= :DAY_OF_WEEK "0001-01-06"]],
   ;;                                                                                                                                                                   :style
   ;;                                                                                                                                                                   {:background-color
   ;;                                                                                                                                                                   "#008b8b66",
   ;;                                                                                                                                                                   :border
   ;;                                                                                                                                                                   "1px
   ;;                                                                                                                                                                   solid
   ;;                                                                                                                                                                   #00000000"}}}},
   ;;          :content [[:v-box
   ;;                     :size "auto" :width "490px" :style
   ;;                     {:font-size "13px", :opacity 0.33}
   ;;                     :children
   ;;                     [[:h-box :size "auto" :justify :between :children [[:box :child
   ;;                     ":mean"] [:box
   ;;                     :child "2202.1428" :style {:font-weight 400}]]]
   ;;                      [:h-box :size "auto" :justify :between :children [[:box :child
   ;;                      ":standard-deviation"] [:box :child "154.30158648515814" :style
   ;;                      {:font-weight
   ;;                      400}]]]
   ;;                      [:h-box :size "auto" :justify :between :children [[:box :child
   ;;                      ":sample-size"]
   ;;                      [:box :child "7" :style {:font-weight 400}]]]
   ;;                      [:h-box :size "auto" :justify :between :children [[:box :child
   ;;                      ":fields"]
   ;;                      [:box :child "[:DAY_OF_WEEK]" :style {:font-weight 400}]]]
   ;;                      [:h-box :size "auto" :justify :between :children [[:box :child
   ;;                      ":calc-used"]
   ;;                      [:box :child "[:percentiles [0.01 0.99]]" :style {:font-weight
   ;;                      400}]]]]]
   ;;                    [:v-box :size "auto" :width "490px" :style {:font-size "16px"}
   ;;                    :children
   ;;                    [[:h-box :size "auto" :justify :between :children [[:box :child
   ;;                    ":DAY_OF_WEEK"]
   ;;                    [:box :child "\"0001-01-06\"" :style {:font-weight 400}]]]]]
   ;;                    "as a wider aggregate:"
   ;;                    {:_h 4,
   ;;                     :_query-id :kit-query--15567526000-agg, :connection-id "boston-crime", :from
   ;;                     [:query/OFFENSE-CODE-GROUP-drag-40], :group-by [:DAY_OF_WEEK],
   ;;                     :order-by [[2 :desc]], :select [:DAY_OF_WEEK [[:count 1] :rows]],
   ;;                     :style-rules {[:* :highlight-group] {:logic [:and [:= :DAY_OF_WEEK
   ;;                     "0001-01-06"]], :style {:background-color "#008b8b66", :border "1px
   ;;                     solid
   ;;                     #00000000"}}}}
   ;;                    "All rows in this group:"
   ;;                    {:_h 8,
   ;;                     :_query-id :kit-query--15567526000-detail, :connection-id "boston-crime", :from
   ;;                     [:query/OFFENSE-CODE-GROUP-drag-40], :select [:*], :where [:*if
   ;;                     :kit-query--15567526000-agg/*.clicked [:*all= :kit-query--15567526000-agg/*.clicked
   ;;                     [:DAY_OF_WEEK]] [:and [:= :DAY_OF_WEEK "0001-01-06"]]]}],
   ;;          :highlight-columns {:OFFENSE-CODE-GROUP-drag-40 [:DAY_OF_WEEK]}, :name "When
   ;;          grouping by day_of_week and aggregating by :rows there are values that are
   ;;          higher than the mean (2202 for this particular aggregation) by 234.", :order 14,
   ;;          :parameters {},
   ;;          :step-mutates {}}
   ;;         {:ask-mutates {"Highlight these in your source query?" {[:panels :block-984
   ;;         :queries :OFFENSE-CODE-GROUP-drag-40 :style-rules [:* :higlight-1726351629]]
   ;;         {:logic [:and
   ;;         [:= :MONTH 2]],
   ;;                                                                                                                                                                   :style
   ;;                                                                                                                                                                   {:background-color
   ;;                                                                                                                                                                   "#008b8b66",
   ;;                                                                                                                                                                   :border
   ;;                                                                                                                                                                   "1px
   ;;                                                                                                                                                                   solid
   ;;                                                                                                                                                                   #00000000"}}}},
   ;;          :content [[:v-box
   ;;                     :size "auto" :width "490px" :style
   ;;                     {:font-size "13px", :opacity 0.33}
   ;;                     :children
   ;;                     [[:h-box :size "auto" :justify :between :children [[:box :child
   ;;                     ":mean"] [:box
   ;;                     :child "1284.5834" :style {:font-weight 400}]]]
   ;;                      [:h-box :size "auto" :justify :between :children [[:box :child
   ;;                      ":standard-deviation"] [:box :child "224.17681203807757" :style
   ;;                      {:font-weight
   ;;                      400}]]]
   ;;                      [:h-box :size "auto" :justify :between :children [[:box :child
   ;;                      ":sample-size"]
   ;;                      [:box :child "12" :style {:font-weight 400}]]]
   ;;                      [:h-box :size "auto" :justify :between :children [[:box :child
   ;;                      ":fields"]
   ;;                      [:box :child "[:MONTH]" :style {:font-weight 400}]]]
   ;;                      [:h-box :size "auto" :justify :between :children [[:box :child
   ;;                      ":calc-used"]
   ;;                      [:box :child "[:percentiles [0.01 0.99]]" :style {:font-weight
   ;;                      400}]]]]]
   ;;                    [:v-box :size "auto" :width "490px" :style {:font-size "16px"}
   ;;                    :children
   ;;                    [[:h-box :size "auto" :justify :between :children [[:box :child
   ;;                    ":MONTH"] [:box
   ;;                    :child "2" :style {:font-weight 400}]]]]]
   ;;                    "as a wider aggregate:"
   ;;                    {:_h 4,
   ;;                     :_query-id :kit-query--17263516290-agg, :connection-id "boston-crime", :from
   ;;                     [:query/OFFENSE-CODE-GROUP-drag-40], :group-by [:MONTH], :order-by
   ;;                     [[2
   ;;                     :desc]], :select [:MONTH [[:count 1] :rows]], :style-rules {[:*
   ;;                     :highlight-group] {:logic [:and [:= :MONTH 2]], :style
   ;;                     {:background-color "#008b8b66", :border "1px solid #00000000"}}}}
   ;;                    "All rows in this group:"
   ;;                    {:_h 8,
   ;;                     :_query-id :kit-query--17263516290-detail, :connection-id "boston-crime", :from
   ;;                     [:query/OFFENSE-CODE-GROUP-drag-40], :select [:*], :where [:*if
   ;;                     :kit-query--17263516290-agg/*.clicked [:*all= :kit-query--17263516290-agg/*.clicked
   ;;                     [:MONTH]] [:and [:= :MONTH 2]]]}],
   ;;          :highlight-columns {:OFFENSE-CODE-GROUP-drag-40 [:MONTH]}, :name "When grouping
   ;;          by month and aggregating by :rows there are values that are higher than the mean
   ;;          (1284 for this particular aggregation) by 225.", :order 15, :parameters {},
   ;;          :step-mutates {}}
   ;;         {:ask-mutates {"Highlight these in your source query?" {[:panels :block-984
   ;;         :queries :OFFENSE-CODE-GROUP-drag-40 :style-rules [:* :higlight1296097415]]
   ;;         {:logic [:and
   ;;         [:= :DAY_OF_WEEK "0001-01-07"] [:= :DISTRICT "C11"]],
   ;;                                                                                                                                                                  :style
   ;;                                                                                                                                                                  {:background-color
   ;;                                                                                                                                                                  "#008b8b66",
   ;;                                                                                                                                                                  :border
   ;;                                                                                                                                                                  "1px
   ;;                                                                                                                                                                  solid
   ;;                                                                                                                                                                  #00000000"}}}},
   ;;          :content [[:v-box
   ;;                     :size "auto" :width "490px" :style
   ;;                     {:font-size "13px", :opacity 0.33}
   ;;                     :children
   ;;                     [[:h-box :size "auto" :justify :between :children [[:box :child
   ;;                     ":mean"] [:box
   ;;                     :child "169.3956" :style {:font-weight 400}]]]
   ;;                      [:h-box :size "auto" :justify :between :children [[:box :child
   ;;                      ":standard-deviation"] [:box :child "103.99576059838641" :style
   ;;                      {:font-weight
   ;;                      400}]]]
   ;;                      [:h-box :size "auto" :justify :between :children [[:box :child
   ;;                      ":sample-size"]
   ;;                      [:box :child "91" :style {:font-weight 400}]]]
   ;;                      [:h-box :size "auto" :justify :between :children [[:box :child
   ;;                      ":fields"]
   ;;                      [:box :child "[:DAY_OF_WEEK :DISTRICT]" :style {:font-weight 400}]]]
   ;;                      [:h-box :size "auto" :justify :between :children [[:box :child
   ;;                      ":calc-used"]
   ;;                      [:box :child "[:percentiles [0.01 0.99]]" :style {:font-weight
   ;;                      400}]]]]]
   ;;                    [:v-box
   ;;                     :size "auto" :width "490px" :style
   ;;                     {:font-size "16px"}
   ;;                     :children
   ;;                     [[:h-box :size "auto" :justify :between :children [[:box :child
   ;;                     ":DAY_OF_WEEK"]
   ;;                     [:box :child "\"0001-01-07\"" :style {:font-weight 400}]]]
   ;;                      [:h-box :size "auto" :justify :between :children [[:box :child
   ;;                      ":DISTRICT"]
   ;;                      [:box :child "\"C11\"" :style {:font-weight 400}]]]]]
   ;;                    "as a wider aggregate:"
   ;;                    {:_h 4,
   ;;                     :_query-id :kit-query-12960974150-agg, :connection-id "boston-crime", :from
   ;;                     [:query/OFFENSE-CODE-GROUP-drag-40], :group-by [:DAY_OF_WEEK
   ;;                     :DISTRICT],
   ;;                     :order-by [[3 :desc]], :select [:DAY_OF_WEEK :DISTRICT [[:count 1]
   ;;                     :rows]], :style-rules {[:* :highlight-group] {:logic [:and [:=
   ;;                     :DAY_OF_WEEK "0001-01-07"] [:= :DISTRICT "C11"]], :style
   ;;                     {:background-color "#008b8b66",
   ;;                     :border "1px solid #00000000"}}}}
   ;;                    "All rows in this group:"
   ;;                    {:_h 8,
   ;;                     :_query-id :kit-query-12960974150-detail, :connection-id "boston-crime", :from
   ;;                     [:query/OFFENSE-CODE-GROUP-drag-40], :select [:*], :where [:*if
   ;;                     :kit-query-12960974150-agg/*.clicked [:*all= :kit-query-12960974150-agg/*.clicked
   ;;                     [:DAY_OF_WEEK :DISTRICT]] [:and
   ;;                     [:= :DAY_OF_WEEK "0001-01-07"] [:= :DISTRICT "C11"]]]}],
   ;;          :highlight-columns {:OFFENSE-CODE-GROUP-drag-40 [:DAY_OF_WEEK :DISTRICT]}, :name
   ;;          "When grouping by day_of_week, district and aggregating by :rows there are
   ;;          values that are higher than the mean (169 for this particular aggregation) by
   ;;          215.", :order 16, :parameters {},
   ;;          :step-mutates {}}
   ;;         {:ask-mutates {"Highlight these in your source query?" {[:panels :block-984
   ;;         :queries :OFFENSE-CODE-GROUP-drag-40 :style-rules [:* :higlight512674475]]
   ;;         {:logic [:and
   ;;         [:= :DAY_OF_WEEK "0001-01-03"] [:= :YEAR 2015]],
   ;;                                                                                                                                                                 :style
   ;;                                                                                                                                                                 {:background-color
   ;;                                                                                                                                                                 "#008b8b66",
   ;;                                                                                                                                                                 :border
   ;;                                                                                                                                                                 "1px
   ;;                                                                                                                                                                 solid
   ;;                                                                                                                                                                 #00000000"}}}},
   ;;          :content [[:v-box
   ;;                     :size "auto" :width "490px" :style
   ;;                     {:font-size "13px", :opacity 0.33}
   ;;                     :children
   ;;                     [[:h-box :size "auto" :justify :between :children [[:box :child
   ;;                     ":mean"] [:box
   ;;                     :child "550.5357" :style {:font-weight 400}]]]
   ;;                      [:h-box :size "auto" :justify :between :children [[:box :child
   ;;                      ":standard-deviation"] [:box :child "164.0140416773727" :style
   ;;                      {:font-weight
   ;;                      400}]]]
   ;;                      [:h-box :size "auto" :justify :between :children [[:box :child
   ;;                      ":sample-size"]
   ;;                      [:box :child "28" :style {:font-weight 400}]]]
   ;;                      [:h-box :size "auto" :justify :between :children [[:box :child
   ;;                      ":fields"]
   ;;                      [:box :child "[:DAY_OF_WEEK :YEAR]" :style {:font-weight 400}]]]
   ;;                      [:h-box :size "auto" :justify :between :children [[:box :child
   ;;                      ":calc-used"]
   ;;                      [:box :child "[:percentiles [0.01 0.99]]" :style {:font-weight
   ;;                      400}]]]]]
   ;;                    [:v-box
   ;;                     :size "auto" :width "490px" :style
   ;;                     {:font-size "16px"}
   ;;                     :children
   ;;                     [[:h-box :size "auto" :justify :between :children [[:box :child
   ;;                     ":DAY_OF_WEEK"]
   ;;                     [:box :child "\"0001-01-03\"" :style {:font-weight 400}]]]
   ;;                      [:h-box :size "auto" :justify :between :children [[:box :child
   ;;                      ":YEAR"] [:box
   ;;                      :child "2015" :style {:font-weight 400}]]]]]
   ;;                    "as a wider aggregate:"
   ;;                    {:_h 4,
   ;;                     :_query-id :kit-query-5126744750-agg, :connection-id "boston-crime", :from
   ;;                     [:query/OFFENSE-CODE-GROUP-drag-40], :group-by [:DAY_OF_WEEK :YEAR],
   ;;                     :order-by
   ;;                     [[3 :desc]], :select [:DAY_OF_WEEK :YEAR [[:count 1] :rows]],
   ;;                     :style-rules {[:* :highlight-group] {:logic [:and [:= :DAY_OF_WEEK
   ;;                     "0001-01-03"] [:= :YEAR 2015]], :style {:background-color
   ;;                     "#008b8b66", :border
   ;;                     "1px solid #00000000"}}}}
   ;;                    "All rows in this group:"
   ;;                    {:_h 8,
   ;;                     :_query-id :kit-query-5126744750-detail, :connection-id "boston-crime", :from
   ;;                     [:query/OFFENSE-CODE-GROUP-drag-40], :select [:*], :where [:*if
   ;;                     :kit-query-5126744750-agg/*.clicked [:*all=
   ;;                     :kit-query-5126744750-agg/*.clicked
   ;;                     [:DAY_OF_WEEK :YEAR]] [:and [:= :DAY_OF_WEEK "0001-01-03"] [:= :YEAR
   ;;                     2015]]]}],
   ;;          :highlight-columns {:OFFENSE-CODE-GROUP-drag-40 [:DAY_OF_WEEK :YEAR]}, :name
   ;;          "When grouping by day_of_week, year and aggregating by :rows there are values
   ;;          that are higher than the mean (550 for this particular aggregation) by 208.",
   ;;          :order 17, :parameters {},
   ;;          :step-mutates {}}
   ;;         {:ask-mutates {"Highlight these in your source query?" {[:panels :block-984
   ;;         :queries :OFFENSE-CODE-GROUP-drag-40 :style-rules [:* :higlight-1114554230]]
   ;;         {:logic [:and
   ;;         [:= :STREET "COMMONWEALTH AVE"]],
   ;;                                                                                                                                                                   :style
   ;;                                                                                                                                                                   {:background-color
   ;;                                                                                                                                                                   "#008b8b66",
   ;;                                                                                                                                                                   :border
   ;;                                                                                                                                                                   "1px
   ;;                                                                                                                                                                   solid
   ;;                                                                                                                                                                   #00000000"}}}},
   ;;          :content [[:v-box
   ;;                     :size "auto" :width "490px" :style
   ;;                     {:font-size "13px", :opacity 0.33}
   ;;                     :children
   ;;                     [[:h-box :size "auto" :justify :between :children [[:box :child
   ;;                     ":mean"] [:box
   ;;                     :child "6.781786" :style {:font-weight 400}]]]
   ;;                      [:h-box :size "auto" :justify :between :children [[:box :child
   ;;                      ":standard-deviation"] [:box :child "19.093579791187643" :style
   ;;                      {:font-weight
   ;;                      400}]]]
   ;;                      [:h-box :size "auto" :justify :between :children [[:box :child
   ;;                      ":sample-size"]
   ;;                      [:box :child "2273" :style {:font-weight 400}]]]
   ;;                      [:h-box :size "auto" :justify :between :children [[:box :child
   ;;                      ":fields"]
   ;;                      [:box :child "[:STREET]" :style {:font-weight 400}]]]
   ;;                      [:h-box :size "auto" :justify :between :children [[:box :child
   ;;                      ":calc-used"]
   ;;                      [:box :child "[:percentiles [0.01 0.99]]" :style {:font-weight
   ;;                      400}]]]]]
   ;;                    [:v-box :size "auto" :width "490px" :style {:font-size "16px"}
   ;;                    :children
   ;;                    [[:h-box :size "auto" :justify :between :children [[:box :child
   ;;                    ":STREET"] [:box
   ;;                    :child "\"COMMONWEALTH AVE\"" :style {:font-weight 400}]]]]]
   ;;                    "as a wider aggregate:"
   ;;                    {:_h 4,
   ;;                     :_query-id :kit-query--11145542300-agg, :connection-id "boston-crime", :from
   ;;                     [:query/OFFENSE-CODE-GROUP-drag-40], :group-by [:STREET], :order-by
   ;;                     [[2
   ;;                     :desc]], :select [:STREET [[:count 1] :rows]], :style-rules {[:*
   ;;                     :highlight-group] {:logic [:and [:= :STREET "COMMONWEALTH
   ;;                     AVE"]], :style {:background-color "#008b8b66", :border "1px solid
   ;;                     #00000000"}}}}
   ;;                    "All rows in this group:"
   ;;                    {:_h 8,
   ;;                     :_query-id :kit-query--11145542300-detail, :connection-id "boston-crime", :from
   ;;                     [:query/OFFENSE-CODE-GROUP-drag-40], :select [:*], :where [:*if
   ;;                     :kit-query--11145542300-agg/*.clicked [:*all= :kit-query--11145542300-agg/*.clicked
   ;;                     [:STREET]] [:and [:= :STREET "COMMONWEALTH AVE"]]]}],
   ;;          :highlight-columns {:OFFENSE-CODE-GROUP-drag-40 [:STREET]}, :name "When grouping
   ;;          by street and aggregating by :rows there are values that are higher than the
   ;;          mean (6 for this particular aggregation) by 203.", :order 18, :parameters {},
   ;;          :step-mutates {}}
   ;;         {:ask-mutates {"Highlight these in your source query?" {[:panels :block-984
   ;;         :queries :OFFENSE-CODE-GROUP-drag-40 :style-rules [:* :higlight1434988132]]
   ;;         {:logic [:and
   ;;         [:= :DISTRICT "C11"] [:= :STREET "DORCHESTER AVE"]],
   ;;                                                                                                                                                                  :style
   ;;                                                                                                                                                                  {:background-color
   ;;                                                                                                                                                                  "#008b8b66",
   ;;                                                                                                                                                                  :border
   ;;                                                                                                                                                                  "1px
   ;;                                                                                                                                                                  solid
   ;;                                                                                                                                                                  #00000000"}}}},
   ;;          :content [[:v-box
   ;;                     :size "auto" :width "490px" :style
   ;;                     {:font-size "13px", :opacity 0.33}
   ;;                     :children
   ;;                     [[:h-box :size "auto" :justify :between :children [[:box :child
   ;;                     ":mean"] [:box
   ;;                     :child "6.0403605" :style {:font-weight 400}]]]
   ;;                      [:h-box :size "auto" :justify :between :children [[:box :child
   ;;                      ":standard-deviation"] [:box :child "11.367280405615126" :style
   ;;                      {:font-weight
   ;;                      400}]]]
   ;;                      [:h-box :size "auto" :justify :between :children [[:box :child
   ;;                      ":sample-size"]
   ;;                      [:box :child "2552" :style {:font-weight 400}]]]
   ;;                      [:h-box :size "auto" :justify :between :children [[:box :child
   ;;                      ":fields"]
   ;;                      [:box :child "[:DISTRICT :STREET]" :style {:font-weight 400}]]]
   ;;                      [:h-box :size "auto" :justify :between :children [[:box :child
   ;;                      ":calc-used"]
   ;;                      [:box :child "[:percentiles [0.01 0.99]]" :style {:font-weight
   ;;                      400}]]]]]
   ;;                    [:v-box
   ;;                     :size "auto" :width "490px" :style
   ;;                     {:font-size "16px"}
   ;;                     :children
   ;;                     [[:h-box :size "auto" :justify :between :children [[:box :child
   ;;                     ":DISTRICT"]
   ;;                     [:box :child "\"C11\"" :style {:font-weight 400}]]]
   ;;                      [:h-box :size "auto" :justify :between :children [[:box :child
   ;;                      ":STREET"]
   ;;                      [:box :child "\"DORCHESTER AVE\"" :style {:font-weight 400}]]]]]
   ;;                    "as a wider aggregate:"
   ;;                    {:_h 4,
   ;;                     :_query-id :kit-query-14349881320-agg, :connection-id "boston-crime", :from
   ;;                     [:query/OFFENSE-CODE-GROUP-drag-40], :group-by [:DISTRICT :STREET],
   ;;                     :order-by
   ;;                     [[3 :desc]], :select [:DISTRICT :STREET [[:count 1] :rows]],
   ;;                     :style-rules {[:* :highlight-group] {:logic [:and [:= :DISTRICT
   ;;                     "C11"] [:= :STREET "DORCHESTER AVE"]], :style {:background-color
   ;;                     "#008b8b66", :border "1px
   ;;                     solid #00000000"}}}}
   ;;                    "All rows in this group:"
   ;;                    {:_h 8,
   ;;                     :_query-id :kit-query-14349881320-detail, :connection-id "boston-crime", :from
   ;;                     [:query/OFFENSE-CODE-GROUP-drag-40], :select [:*], :where [:*if
   ;;                     :kit-query-14349881320-agg/*.clicked [:*all= :kit-query-14349881320-agg/*.clicked
   ;;                     [:DISTRICT :STREET]] [:and [:= :DISTRICT "C11"] [:= :STREET "DORCHESTER AVE"]]]}],
   ;;          :highlight-columns {:OFFENSE-CODE-GROUP-drag-40 [:DISTRICT :STREET]}, :name
   ;;          "When grouping by district, street and aggregating by :rows there are values
   ;;          that are higher than the mean (6 for this particular aggregation) by 187.",
   ;;          :order 19, :parameters {}, :step-mutates {}}],
   ;;  :description "Top 20 Outlier by Severity", :mutates {}, :options {:actions? false, :pages? true, :search? false},
   ;;  :parameters {}}
  })

(defn insert-attributes [])
