(ns rvbbit-backend.surveyor
  (:require
    [clojure.java.jdbc   :as jdbc]
    [clojure.string      :as cstr]
    [java-time.api       :as jt]
    [rvbbit-backend.util :as ut]
    ;[tea-time.core       :as tt]
   ))

(defn jdbc-conn-good? [conn] true)

(defn normalize-jdbc-data-type [x] x)

(defn get-test-conn-meta
  [db-conn]
  (into []
        (for [t (vec (jdbc/with-db-metadata [m db-conn]
                                            (jdbc/metadata-result (.getTables m
                                                                              nil ;; catalog pattern
                                                                              nil ;; schema pattern
                                                                              nil ;; table name
                                                                              (into-array ["TABLE" "VIEW"]) ;; types
                                                                  ))))]
          (do ;(ut/ppln t)
            (merge t ;(select-keys t [:table_schem :table_name :table_type])
                   {:fields (for [f (jdbc/with-db-metadata [m db-conn]
                                                           (jdbc/metadata-result (.getColumns m
                                                                                              (:table_cat t) ;nil
                                                                                                             ;;(get
                                                                                                             ;db-conn
                                                                                              (:table_schem t) ;nil
                                                                                              (:table_name t)
                                                                                              nil)))]
                              (do ;(ut/ppln f)
                                f))})))))

(defn get-jdbc-conn-meta
  [db-conn]
  (into
    []
    (for [t (vec (jdbc/with-db-metadata [m db-conn]
                                        (jdbc/metadata-result (.getTables m
                                                                          nil ;; catalog pattern
                                                                          nil ;; schema pattern
                                                                          nil ;; table name
                                                                          (into-array ["TABLE" "VIEW"]) ;; types
                                                                                                        ;; of
                                                                                                        ;; objects
                                                              ))))]
      (do ;(ut/ppln t)
        (merge t ;(select-keys t [:table_schema :table_name :table_type])
               {:fields (for [f (jdbc/with-db-metadata [m db-conn]
                                                       (jdbc/metadata-result (.getColumns
                                                                               m
                                                                               (:table_cat t) ;nil
                                                                                              ;;(get
                                                                                              ;db-conn
                                                                               (:table_schem t) ;nil
                                                                                                ;;
                                                                                                ;schema name
                                                                               (:table_name t) ;; table name
                                                                                               ;;  pattern
                                                                               nil ;; field pattern match
                                                                             )))]
                          f)})))))

(defn jdbc-connect-meta
  [db-conn f-name]
  (let [user-name        (jdbc/with-db-metadata [m db-conn] (jdbc/metadata-result (.getUserName m)))
        database-name    (jdbc/with-db-metadata [m db-conn] (jdbc/metadata-result (.getDatabaseProductName m)))
        database-version (jdbc/with-db-metadata [m db-conn] (jdbc/metadata-result (.getDatabaseProductVersion m)))
        connection-str   (jdbc/with-db-metadata [m db-conn] (jdbc/metadata-result (.getURL m)))
        conn-id          (str (cstr/replace (cstr/lower-case (last (cstr/split f-name #"/"))) ".edn" ""))
        conn-id          (if (cstr/ends-with? (cstr/lower-case conn-id) ".csv") "imported" conn-id)] ;; bizarre
    {:user_name        user-name
     :database_name    database-name
     :database_version database-version
     :connection_str   connection-str
     :connection_id    conn-id}))

(defn flatten-jdbc-conn-meta
  [connect-meta db-meta]
  (let [;db-meta (jdbc-connect-meta connect-meta)
        conn-type      (get connect-meta :database_name)
        conn-id        (get connect-meta :connection_id)
        db-meta-with-* (vec (for [t db-meta] ;; adding a * so table-level granularity sniffs
                              (assoc t :fields (merge (:fields t) {:column_name "*" :type_name "special"}))))]
    (ut/ppln db-meta-with-*)
    (vec (flatten (for [table-map db-meta-with-*]
                    (for [field (:fields table-map)]
                      {[conn-type conn-id (:table_type table-map)
                        (if (nil? (:table_schem table-map)) "none" (:table_schem table-map)) (:table_cat table-map)
                        (:table_name table-map) (:column_name field)]
                         {:column_type (:type_name field) :meta_ddl_gen (jt/instant)}}))))))

