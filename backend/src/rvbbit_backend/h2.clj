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
    database_name VARCHAR(255) NULL,
    is_measure BOOLEAN NULL,
    sum_val VARCHAR(255) NULL,
    data_type VARCHAR(255) NULL,
    field_type VARCHAR(255) NULL,
    example_value VARCHAR(255) NULL,
    honey_hash VARCHAR(255) NULL,
    total_rows BIGINT NULL,
    user_name VARCHAR(255) NULL,
    is_dimension BOOLEAN NULL,
    field_name VARCHAR(255) NULL,
    distinct_count BIGINT NULL,
    db_schema VARCHAR(255) NULL,
    parent_table_name VARCHAR(255) NULL,
    max_val VARCHAR(255) NULL,
    db_catalog VARCHAR(255) NULL,
    database_version VARCHAR(255) NULL,
    context_key VARCHAR(555) NOT NULL PRIMARY KEY,
    context VARCHAR(255) NULL,
    connection_id VARCHAR(255) NULL,
    connection_str VARCHAR(3255) NULL,
    table_name VARCHAR(255) NULL,
    table_type VARCHAR(255) NULL,
    table_fields VARCHAR(50500) NULL,
    avg_val VARCHAR(255) NULL,
    min_val VARCHAR(255) NULL,
    updated TIMESTAMP DEFAULT CURRENT_TIMESTAMP NULL
);")