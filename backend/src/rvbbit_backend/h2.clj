(ns rvbbit-backend.h2
  (:require
   [clojure.string      :as cstr]
   [rvbbit-backend.util :as ut]))

(def create-realms
  "create table if not exists realms
  (realm_name text NULL,
   file_path text NULL,
   block_name text NULL,
   block_key text NULL,
   block_type text NULL,
   block_data text NULL,
   ts TIMESTAMP DEFAULT CURRENT_TIMESTAMP NULL
   ) ;")

(def create-screens
  "create table if not exists screens
  (file_path text NULL,
   screen_name text NULL,
   blocks integer NULL,
   queries integer NULL,
   ts TIMESTAMP DEFAULT CURRENT_TIMESTAMP NULL
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
   ts TIMESTAMP DEFAULT CURRENT_TIMESTAMP NULL
   ) ;")

;; (def create-ft-index "CREATE ALIAS IF NOT EXISTS FT_INIT FOR \"org.h2.fulltext.FullText.init\";
;; CALL FT_INIT();
;; CALL FT_CREATE_INDEX('PUBLIC', 'fields',
;;                       'database_name, sum_val, data_type, field_type, field_name, db_schema, max_val, db_catalog,
;;                       database_version, table_name, table_type, avg_val, min_val');")

(def create-fields-view
  "create view if not exists fields_base as select * from fields where table_type <> ':query';")

(def create-ft-index "CREATE ALIAS IF NOT EXISTS FT_INIT FOR \"org.h2.fulltext.FullText.init\";
CALL FT_INIT();
CALL FT_CREATE_INDEX('PUBLIC', 'fields', NULL);")

;; (def create-ft-index "CREATE ALIAS IF NOT EXISTS FTL_INIT FOR \"org.h2.fulltext.FullTextLucene.init\";
;; CALL FTL_INIT();
;; CALL FTL_CREATE_INDEX('PUBLIC', 'fields', NULL);")

(def create-fields
  "CREATE TABLE IF NOT EXISTS fields (
    is_group_by BOOLEAN NULL,
    database_name TEXT NULL,
    is_measure BOOLEAN NULL,
    sum_val TEXT NULL,
    data_type TEXT NULL,
    field_type TEXT NULL,
    example_value TEXT NULL,
    honey_hash TEXT NULL,
    total_rows BIGINT NULL,
    user_name TEXT NULL,
    is_dimension BOOLEAN NULL,
    field_name TEXT NULL,
    distinct_count BIGINT NULL,
    db_schema TEXT NULL,
    parent_table_name TEXT NULL,
    max_val TEXT NULL,
    db_catalog TEXT NULL,
    database_version TEXT NULL,
    context_key TEXT NULL, --- NOT NULL PRIMARY KEY,
    context TEXT NULL,
    connection_id TEXT NULL,
    connection_str TEXT NULL,
    table_name TEXT NULL,
    table_type TEXT NULL,
    table_fields TEXT NULL,
    avg_val TEXT NULL,
    min_val TEXT NULL,
    updated TIMESTAMP DEFAULT CURRENT_TIMESTAMP NULL
);")

(def create-panel-history
  "create table if not exists panel_history
   (kp text NULL,
    client_name text NULL,
    data text NULL,
    pre_data text NULL,
    diff text NULL,
    diff_kp text NULL,
    panel_key text NULL,
    vkey text NULL,
    type text NULL,
    updated TIMESTAMP DEFAULT CURRENT_TIMESTAMP NULL);")

(def create-panel-resolved-history
  "create table if not exists panel_resolved_history
   (kp text NULL,
    client_name text NULL,
    data text NULL,
    pre_data text NULL,
    diff text NULL,
    diff_kp text NULL,
    panel_key text NULL,
    vkey text NULL,
    type text NULL,
    updated TIMESTAMP DEFAULT CURRENT_TIMESTAMP NULL);")

(def create-panel-materialized-history
  "create table if not exists panel_materialized_history
   (kp text NULL,
    client_name text NULL,
    data text NULL,
    pre_data text NULL,
    diff text NULL,
    diff_kp text NULL,
    panel_key text NULL,
    vkey text NULL,
    type text NULL,
    updated TIMESTAMP DEFAULT CURRENT_TIMESTAMP NULL);")

(def create-client-memory
  "create table if not exists client_memory
   (mem_time text NULL,
    client_name text NULL,
    selected_tab text NULL,
    screen_name text NULL,
    packets integer NULL,
    batches integer NULL,
    mem_limit bigint NULL,
    mem_used bigint NULL,
    mem_used_mb text NULL,
    mem_total bigint NULL,
    latency integer NULL,
    client_subs integer NULL,
    server_subs integer NULL,
    messages_per_second integer NULL,
    recent_messages_per_second integer NULL,
    ts TIMESTAMP DEFAULT CURRENT_TIMESTAMP NULL) ;")

(def create-client-stats
  "create table if not exists client_stats
   (client_name text NULL,
    ack integer NULL,
    client_latency integer NULL,
    client_subs integer NULL,
    last_seen text NULL,
    memory text NULL,
    screen_name text NULL,
    selected_tab text NULL,
    push integer NULL,
    queue_size integer NULL,
    server_subs integer NULL,
    last_seen_seconds integer NULL,
    booted_ts bigint NULL,
    queue_distro text NULL,
    uptime_seconds integer NULL,
    uptime text NULL,
    messages_per_second integer NULL,
    recent_messages_per_second integer NULL,
    ts TIMESTAMP DEFAULT CURRENT_TIMESTAMP NULL) ;")

(def create-jvm-stats
  "create table if not exists jvm_stats
   (used_memory_mb integer NULL,
    thread_count integer NULL,
    messages integer NULL,
    batches integer NULL,
    recent_batches integer NULL,
    recent_batches_per_second float NULL,
    batches_per_second float NULL,
    unix_ms bigint NULL,
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
    ts TIMESTAMP DEFAULT CURRENT_TIMESTAMP NULL) ;")

(def create-client-items
  "create table if not exists client_items
   (item_key text NULL,
    item_type text NULL,
    item_sub_type text NULL,
    vvalue text NULL,
    is_live boolean NULL,
    sample text NULL,
    display_name text NULL,
    block_meta text NULL,
    ts TIMESTAMP DEFAULT CURRENT_TIMESTAMP NULL) ;")

(def create-flows
  "create table if not exists flows
  (file_path text NULL,
   flow_id text NULL,
   components integer NULL,
   connections integer NULL,
   last_modified text NULL,
   body text NULL,
   ts TIMESTAMP DEFAULT CURRENT_TIMESTAMP NULL) ;")

(def create-flow-schedules
  "create table if not exists flow_schedules
   (schedule text NULL,
    flow_id text NULL,
    opts text NULL,
    updated TIMESTAMP DEFAULT CURRENT_TIMESTAMP NULL) ;")

(def create-flow-history
  "create table if not exists flow_history
  (flow_id text NULL,
   run_id text NULL,
   parent_run_id text NULL,
   client_name text NULL,
   orig_flow_id text NULL,
   in_error boolean NULL,
   started bigint NULL,
   start_ts text NULL,
   ended bigint NULL,
   elapsed real NULL,
   elapsed_seconds real NULL,
   human_elapsed text NULL,
   overrides text NULL,
   ts TIMESTAMP DEFAULT CURRENT_TIMESTAMP NULL) ;")

(def create-flow-results
  "create table if not exists flow_results
   (flow_id text NULL,
    block_key text NULL,
    block_value text NULL,
    data_type text NULL,
    ts TIMESTAMP DEFAULT CURRENT_TIMESTAMP NULL) ;")

(def create-live-schedules
  "create table if not exists live_schedules
   (flow_id text NULL,
    override text NULL,
    schedule text NULL,
    channel text NULL,
    next_times text NULL,
    ts TIMESTAMP DEFAULT CURRENT_TIMESTAMP NULL) ;")

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
    eend bigint NULL,
    path text NULL,
    start bigint NULL,
    type text NULL,
    vvalue text NULL,
    ts TIMESTAMP DEFAULT CURRENT_TIMESTAMP NULL) ;")

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
    eend bigint NULL,
    path text NULL,
    view text NULL,
    start bigint NULL,
    type text NULL,
    vvalue text NULL,
    ts TIMESTAMP DEFAULT CURRENT_TIMESTAMP NULL) ;")