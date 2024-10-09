(ns rvbbit-backend.realms
  (:require [clojure.string :as cstr]
            [clojure.edn :as edn]
            [rvbbit-backend.db  :as db]
            [rvbbit-backend.evaluator  :as evl]
            [rvbbit-backend.util       :as    ut
             :refer [ne?]]
            [rvbbit-backend.sql        :as    sql
             :refer [sql-exec sql-query   sql-query-one system-db history-db autocomplete-db system-reporting-db realms-db
                     cache-db cache-db-memory ghost-db flows-db insert-error-row! to-sql pool-create]]
            [clojure.java.io :as io]))

;; read the file
;; generate the map struction of basic blocks for now
;; save the output
;; put it in an atom
;; have an ""atom way"" to browse the hierachy..
;; drag in the basic api queries...
;;   - have some options with diff hardcoded groupings for now    

;; drag out dim field into a filter widget 

;; enumerate the dims and meaures on the side
;; drag out editor aggs and dims to ADD to the current query

;; allow dims and aggs to be REMOVED from the current query 

;; worry about files later 

;; this would be the master map.


(defn load-realm-maps []
  (let [current-dir (System/getProperty "user.dir")
        ;;_ (ut/pp [:current-dir current-dir])
        realm-dir (io/file (str current-dir "/realms"))
        realm-folders (->> (.listFiles realm-dir)
                           (filter #(.isDirectory %)))
        realm-maps (into {}
                         (for [folder realm-folders
                               :let [realm-map-file (io/file folder "realm-map.clj")
                                     rid (keyword (.getName folder))
                                     pool-name (keyword (str (.getName folder) "-realm-builder")) 
                                     relative-path (.relativize (.toPath (io/file current-dir))
                                                                (.toPath realm-map-file))
                                     output (evl/repl-eval (str relative-path) "127.0.0.1" 8181 :rvbbit pool-name [])
                                     data (get-in output [:evald-result :value 0])
                                     sql-rows (vec
                                               (for [[k v] data
                                                     :let [realm-type (get v :realm-type "data")]]
                                                 {:realm_name (str rid)
                                                  :file_path (str folder)
                                                  :block_name (get v :name)
                                                  :block_key (str k)
                                                  :block_type realm-type
                                                  :block_data (pr-str v)}))
                                     _ (sql-exec realms-db (to-sql {:delete-from [:realms] :where [:= :realm_name (str rid)]}))
                                     _ (sql-exec realms-db (to-sql {:insert-into [:realms] :values sql-rows}))]
                               :when (.exists realm-map-file)]
                           {rid data}))]
    (ut/pp [:rebuild-realms (vec  (keys realm-maps))])
    (swap! db/realm-atom merge realm-maps)))

;; (load-realm-maps)

;; (ut/pp [:re @db/realm-atom])


;;   "create table if not exists realms 
;;   (realm_name text NULL, 
;;    file_path text NULL, 
;;    block_name text NULL,
;;    block_key text NULL,
;;    block_type text NULL,
;;    block_data text NULL,
;;    ts TIMESTAMP DEFAULT (datetime('now', 'localtime')) NULL"