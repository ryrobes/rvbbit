(ns rvbbit-backend.core
  (:require
   [clojure.java.jdbc :as jdbc]
   [clojure.edn :as edn]
   [clojure.core.async :as async :refer [<! >! <!! >!! go chan]]
   [nextjournal.beholder :as beholder]
   [shutdown.core :as shutdown]
   [flowmaps.core :as flow]
   [flowmaps.examples.simple-flows :as fex]
   [flowmaps.web :as flow-web]
   [flowmaps.db :as flow-db]
   [ring.adapter.jetty9 :as jetty]
   [clojure.pprint :as ppt]
   ;[rvbbit-backend.flowmaps :as flow]
   [rvbbit-backend.transform :as ts]
   [rvbbit-backend.embeddings :as em]
   [clj-time.coerce :as tcc]
   [clj-time.format :as f]
   [clj-time.core :as t]
   ;[websocket-layer.core :as wl]
   [hikari-cp.core :as hik]
   [clojure.java.shell :as shell]
   [rvbbit-backend.external :as ext]
   [puget.printer :as puget]
   [clojure.set :as cset]
   [tea-time.core :as tt]
   [clojure.data :as data]
   [taskpool.taskpool :as tp]
   [clojure.java.io :as io]
   [honey.sql :as honey]
   [clojure.string :as cstr]
   [clojure.walk :as walk]
   [clojure.core.async :refer [<! timeout]]
   [rvbbit-backend.websockets :as wss]
   [rvbbit-backend.search :as search]
   [rvbbit-backend.cruiser :as cruiser]
   [rvbbit-backend.util :as ut :refer [ne?]]
   [rvbbit-backend.evaluator :as evl]
   [rvbbit-backend.config :as config]
   [rvbbit-backend.assistants :as ass]
   [rvbbit-backend.surveyor :as surveyor]
   [rvbbit-backend.sql :as sql :refer [sql-exec sql-query sql-query-meta sql-query-one system-db flows-db insert-error-row! to-sql pool-create]]
   [clojure.math.combinatorics :as combo]
   [metrics.jvm.core :refer [instrument-jvm]]
   [metrics.core :refer [new-registry]]
   [metrics.gauges :refer [gauge-fn gauge]]
   [metrics.timers :refer [timer]]
   [metrics.reporters.csv :as csv]
   [rvbbit-backend.instrument :as instrument]
   [clj-time.jdbc]
   [clj-http.client :as client]) ;; enables joda jdbc time returns
  (:import [java.util Date]
           
           java.nio.file.Files
           java.nio.file.Paths
           java.nio.file.attribute.FileTime
           java.nio.file.LinkOption
           java.time.format.DateTimeFormatter
           java.time.ZoneId))

(def harvest-on-boot? (get config/settings :harvest-on-boot? true))

;; lazy result sets? https://clojure-doc.org/articles/ecosystem/java_jdbc/using_sql/
;; 3rd party connection pooling ? https://clojure-doc.org/articles/ecosystem/java_jdbc/reusing_connections/

;;(ut/pp default-derived-fields)

;(def mem-db {:classname   "org.sqlite.JDBC"
;             :subprotocol "sqlite"
;             :subname     "db/query-cache.db"})


(defn get-last-modified-time [f-path]
  (let [path (Paths/get f-path (into-array String []))
        lastModifiedTime (Files/getLastModifiedTime path (into-array LinkOption [LinkOption/NOFOLLOW_LINKS]))
        instant (.toInstant lastModifiedTime)
        zonedDateTime (.atZone instant (ZoneId/systemDefault))
        formatter (DateTimeFormatter/ofPattern "yyyy-MM-dd HH:mm:ss")]
    (.format formatter zonedDateTime)))

;; reagent has special keywords that are invalid in edn, so we need to convert them to valid keywords. lame, but needed!
(def invalid-to-valid-keywords {":>" ":reagent-gt"})
(def valid-to-invalid-keywords (zipmap (vals invalid-to-valid-keywords) (keys invalid-to-valid-keywords)))
(defn convert-to-valid-edn [s]
  (reduce (fn [s [invalid valid]]
            (clojure.string/replace s invalid valid))
          s
          invalid-to-valid-keywords))

(defn convert-back-to-original [m]
  (clojure.walk/postwalk
   (fn [x]
     (if (and (keyword? x) (valid-to-invalid-keywords x))
       (valid-to-invalid-keywords x)
       x))
   m))

(defn read-screen [f-path]
  (let [screen-str (slurp f-path)
        valid-edn-str (convert-to-valid-edn screen-str)
        screen-data (try (edn/read-string valid-edn-str)
                         (catch Exception e (ut/pp [:read-screen-error!!!!! f-path e]) {}))]
    (convert-back-to-original screen-data)))

(defn update-screen-meta [f-path]
  (let [;file-path "./screens/"
        ;; screen-str (slurp (str f-path))
        ;; screen-data (try (edn/read-string screen-str) 
        ;;                  (catch Exception e (ut/pp [:read-screen-error!!!!! f-path e]) {}))
        screen-data (read-screen f-path)
        screen-name (get screen-data :screen-name "unnamed-screen!")
        resolved-queries (get screen-data :resolved-queries)
        screen-data (if (not (empty? resolved-queries))
                      (assoc screen-data :panels
                             (into {}
                                   (for [[k v] (get screen-data :panels)]
                                     {k (assoc v :queries (select-keys resolved-queries (keys (get v :queries))))}))) 
                      screen-data)
        theme-map (get-in screen-data [:click-param :theme])
        has-theme? (and (not (nil? theme-map)) (not (empty? theme-map)))
        theme-name (if (and has-theme? (not (nil? (get theme-map :theme-name))))
                     (str ", " (get theme-map :theme-name) " ") "")
        blocks (remove nil? (conj (keys (get-in screen-data [:panels])) (when has-theme? :*theme*)))
        boards (distinct (for [[_ v] (get-in screen-data [:panels])] (get v :tab)))
        blocks (into blocks boards) ; (map #(str "board/" %) boards))
        params (get-in screen-data [:click-param])
        rel-params (fn [obj params]
                     (let [kks (filter #(and (keyword? %)
                                             (cstr/starts-with? (str %) ":param")
                                             (cstr/includes? (str %) "/")) (ut/deep-flatten obj))
                           kps (vec (for [e kks] (vec (map keyword (cstr/split (str (ut/unkeyword e)) #"/")))))
                           pp1 (ut/select-keypaths params kps)]
                       ;;(ut/pp [pp1 kps])
                       (or pp1 {})))
        board-map (into {} (for [b boards] {b (into {} (for [[k v] (get-in screen-data [:panels])
                                                             :when (= (get v :tab) b)] {k v}))}))
        queries (into {} (for [b blocks] (get-in screen-data [:panels b :queries])))]
    (swap! wss/screens-atom assoc screen-name screen-data) ;; update master screen atom for param reactions etc 
    ;; (search/add-or-update-document search/index-writer (str :screen "-" screen-name) 
    ;;                                {:content screen-name ;screen-data 
    ;;                                 :type :screen 
    ;;                                 :row {:file-path f-path :screen-name screen-name :blocks 0 :queries 0}})
    (ut/pp [:updating-screen-meta-for f-path])
    (sql-exec system-db (to-sql {:delete-from [:screens] :where [:= :file_path f-path]}))
    (sql-exec system-db (to-sql {:delete-from [:blocks] :where [:= :file_path f-path]}))
    ;(sql-exec system-db (to-sql {:delete-from [:boards] :where [:= :file_path f-path]}))
    (sql-exec system-db (to-sql {:insert-into [:screens]
                                 :columns [:file_path :screen_name :blocks :queries]
                                 :values [[f-path screen-name (count blocks) (count (keys queries))]]}))
    (sql-exec system-db (to-sql {:insert-into [:blocks]
                                 :columns [:file_path :screen_name :block_key :block_name :views :queries :view_names :query_names :block_data :tab_name]
                                 :values (for [b blocks]
                                           (let [theme? (true? (= b :*theme*))
                                                 board? (some #(= b %) boards) ;(cstr/starts-with? (str b) "board/")
                                                 queries (get-in screen-data [:panels b :queries])
                                                 views (if theme? theme-map (get-in screen-data [:panels b :views]))
                                                 block-name (str (cond theme? (str "(meta: this screen's theme" theme-name ")")
                                                                       board? (str "board: " b)
                                                                       :else (get-in screen-data [:panels b :name])))
                                                ;;  _ (search/add-or-update-document search/index-writer (str :block "-" screen-name b)
                                                ;;                                   {:content (str screen-name " " b " " (get-in screen-data [:panels b :name])) ;(get-in screen-data [:panels b]) 
                                                ;;                                    :type :block
                                                ;;                                    :row {:file-path f-path :screen-name screen-name 
                                                ;;                                          :block_key (str b) :block_name block-name}})
                                                 ]
                                             ;; do side-effect insert for deeper table?
                                             [f-path screen-name (str b) block-name
                                              (count (keys views))
                                              (count (keys queries))
                                              (try (str (cstr/join " " (keys views))) (catch Exception _ ""))
                                              (try (str (cstr/join " " (keys queries))) (catch Exception _ ""))
                                              (cond theme? (str theme-map)
                                                    board? (pr-str {:panels (get board-map b)
                                                                    :click-param {:param (get params :param)}
                                                                    ;(rel-params (get board-map b) params)
                                                                    })
                                                    :else (str (get-in screen-data [:panels b])))
                                              (str (get-in screen-data [:panels b :tab]))
                                              ]))}))

    ;(ut/pp (keys screen-data))
    ))

(defn update-all-screen-meta []
  (sql-exec system-db (to-sql {:delete-from [:screens]})) ;; just in case
  (let [prefix "./screens/"
        files (ut/get-file-vectors-simple prefix ".edn")]
    (doseq [f files] (update-screen-meta (str prefix f)))))

(defn update-flow-meta [f-path]
  (try (let [screen-str (slurp (str f-path))
             flow-data (edn/read-string screen-str)
             flow-id (get flow-data :flow-id "unnamed-flow!")
             insert-sql {:insert-into [:flows]
                         :values [{:flow_id (str flow-id)
                                   :components (count (get flow-data :components))
                                   :last_modified (get-last-modified-time f-path)
                                   :connections (count (get flow-data :connections))
                                   :file_path (str f-path)
                                   :body (pr-str flow-data)}]}]
        ;;  (search/add-or-update-document search/index-writer (str :flow "-" flow-id)
        ;;                                 {:content flow-data 
        ;;                                  :type :flow
        ;;                                  :row {:file-path f-path :flow-id flow-id}})
         (sql-exec flows-db (to-sql {:delete-from [:flows] :where [:= :file_path f-path]}))
         (sql-exec flows-db (to-sql insert-sql)))
       (catch Exception e (ut/pp [:read-flow-error f-path e]))))

(defn update-all-flow-meta []
  (sql-exec flows-db (to-sql {:delete-from [:flows]})) ;; just in case
  (let [prefix "./flows/"
        files (ut/get-file-vectors-simple prefix ".edn")]
    (doseq [f files]
      (let [f-path (str prefix f)]
        (wss/create-flowblock f-path)
        (update-flow-meta f-path)))))

(defn refresh-flow-fn [flow-functions-map]
  (doseq [{:keys [category name description file-path inputs icon types]} (ut/flatten-map flow-functions-map)]
    (wss/enqueue-task3 ;; since out base sqlite db hate concurrency, with a "real" db, this can likely be done in parallel TODO
     (fn [] (let [connection_id "system-db"]
              (sql-exec system-db (to-sql {:delete-from [:flow_functions] :where [:and [:= :name name] [:= :category category]]}))
              (sql-exec system-db (to-sql {:insert-into [:flow_functions]
                                           :columns [:connection_id :run_id :category :name :full_map :description :inputs :icon :input_types :output_types :file_path]
                                           :values [[connection_id 0 ;;this-run-id
                                                     (str category)
                                                     (str name)
                                                     (str (get-in flow-functions-map [category name])) ;; full map
                                                     (str description)
                                                     (str inputs)
                                                     (str icon)
                                                     (str (vec (vals (dissoc types :out))))
                                                     (str (get types :out))
                                                     (str file-path)]]})))))))

(defn watch-flows-folder []
  (let [file-path "./flows/"]
    ;(do
      ;(shell/sh "/bin/bash" "-c" (str "mkdir -p " file-path0))
    (beholder/watch
     #(when (cstr/ends-with? (str (get % :path)) ".edn")
        (let [f-path (str (get % :path))
              f-op (get % :type)
              ffn (wss/create-flowblock f-path)]
          (refresh-flow-fn {:sub-flow ffn}) ;; change the sql for the sub-flow block
          (ut/pp [:flow-change! f-op f-path])
          (update-flow-meta f-path)))
     file-path)))

(defn watch-screens-folder []
  (let [file-path "./screens/"]
    ;(do
      ;(shell/sh "/bin/bash" "-c" (str "mkdir -p " file-path0))
    (beholder/watch
     #(when (cstr/ends-with? (str (get % :path)) ".edn")
        (let [f-path (str (get % :path))
              f-op (get % :type)]
          (ut/pp [:screen-change! f-op f-path])
          (update-screen-meta f-path)))
     file-path)))

(defn update-all-conn-meta []
  ;(sql-exec system-db (to-sql {:delete-from [:screens]})) ;; just in case
  (let [prefix "connections/"
        files (ut/get-file-vectors-simple prefix ".edn")]
    (doseq [f files
            :let [f-path (str prefix f)
                  conn-name (str (cstr/replace (cstr/lower-case (last (cstr/split f-path #"/"))) ".edn" ""))
                  conn (edn/read-string (slurp f-path))
                  poolable? (try (and (map? conn)
                                      (find conn :jdbc-url))
                                 (catch Exception _ false))
                  conn (if poolable?
                         (try
                           {:datasource @(pool-create conn (str conn-name "-pool"))}
                           (catch Exception e (do (ut/pp [:connection-error conn-name e])
                                                  {:datasource nil})))
                         conn)]]
      (try ;; connection errors, etc
        (do (when poolable?
              (swap! wss/conn-map assoc conn-name conn))
            (when harvest-on-boot?
              (cruiser/lets-give-it-a-whirl-no-viz
               f-path
               conn
               system-db
               cruiser/default-sniff-tests
               cruiser/default-field-attributes
               cruiser/default-derived-fields
               cruiser/default-viz-shapes)))
        (catch Exception e (do (swap! wss/conn-map dissoc conn-name)
                                    ;; remove it from the user conn map
                               (ut/pp [:sniff-error conn-name e])))))))

(defn watch-connections-folder []
  (let [file-path "./connections/"]
    (beholder/watch
     #(cond (cstr/ends-with? (str (get % :path)) ".edn")
            (let [f-path (str (get % :path))
                  f-op (get % :type)
                  conn (edn/read-string (slurp f-path))
                  conn-name (str (cstr/replace (cstr/lower-case (last (cstr/split f-path #"/"))) ".edn" ""))
              ;conn f-data ;(edn/read-string (slurp f-path))
                  conn (if (try (and (map? conn)
                                     (find conn :jdbc-url))
                                (catch Exception _ false))
                         {:datasource @(pool-create conn (str conn-name "-pool"))}
                         conn)]
              (ut/pp [:connection-change! f-op f-path])
              (swap! wss/conn-map assoc conn-name conn)
              (cruiser/lets-give-it-a-whirl-no-viz
               f-path
               conn ;f-data
               system-db
               cruiser/default-sniff-tests
               cruiser/default-field-attributes
               cruiser/default-derived-fields
               cruiser/default-viz-shapes))
            (cstr/ends-with? (str (get % :path)) ".csv")
            (let [f-path (str (get % :path))
                  f-op (get % :type)]
              (ut/pp [:csv-file-change! f-op f-path])
              (wss/process-csv {} f-path)))
     file-path)))


(defn thaw-flow-results []
  (ut/pp [:thawing-flow-results-atom...])
  (let [file-path "./data/atoms/flow-db-results-atom.edn"
        file (io/file file-path)
        state (if (.exists file)
                (with-open [rdr (io/reader file)]
                  (try (edn/read (java.io.PushbackReader. rdr))
                       (catch Exception e
                         (do
                           (ut/pp [:flow-results-thaw-atom-error!!!! file e])
                           (System/exit 0)))))
                {})]
    (reset! flow-db/results-atom state)))


  
(defn start-services []
  (ut/pp [:starting-services...])
  (evl/create-nrepl-server!)
  (wss/create-web-server!)
  (wss/create-websocket-server!)
  ;; (do (ut/pp [:STYING??!])
  ;;   (reset! wss/websocket-server (jetty/run-jetty wss/web-handler wss/ring-options))
  ;; (.start @wss/websocket-server))
  (evl/create-nrepl-server!))

(defn delayed-start [ms f]
  (Thread. #(do (Thread/sleep ms) (f))))

;; (defn delay-execution [ms f]
;;   (let [thread (Thread. (fn []
;;                           (Thread/sleep ms)
;;                           (f)))]
;;     (.start thread)
;;     thread))

(defn delay-execution [ms f]
   (let [thread (Thread. (fn []
                           (Thread/sleep ms)
                           (f)))]
   (.start thread)
   thread))


  (defn -main [& args]

    ;;(ut/thaw-atom {} "./data/atoms/flow-db-results-atom.edn" flow-db/results-atom true)
    (thaw-flow-results)
    (sql/start-worker-sql) ;; test

    #_{:clj-kondo/ignore [:inline-def]}
    (defonce start-conn-watcher (watch-connections-folder))
    #_{:clj-kondo/ignore [:inline-def]}
    (defonce start-screen-watcher (watch-screens-folder))
    #_{:clj-kondo/ignore [:inline-def]}
    (defonce start-flow-watcher (watch-flows-folder))

    ;(defonce jvm-monitor (tt/every! 240 2 (bound-fn [] (wss/jvm-memory-used))))

  ;"The entry-point for 'lein run'"
  ;;;(ut/pp (surveyor/get-test-conn-meta target-db5))
  ;(lets-give-it-a-whirl target-db8 system-db default-sniff-tests default-field-attributes default-derived-fields default-viz-shapes [:like :table-name "%TESTIMDB%"]) ;; oracle
  ;(cruiser/lets-give-it-a-whirl cruiser/target-db2 system-db cruiser/default-sniff-tests cruiser/default-field-attributes cruiser/default-derived-fields cruiser/default-viz-shapes) ;; sqlite
  ;(cruiser/lets-give-it-a-whirl cruiser/target-db  system-db cruiser/default-sniff-tests cruiser/default-field-attributes cruiser/default-derived-fields cruiser/default-viz-shapes)  ;; sqlite
  ;(cruiser/lets-give-it-a-whirl cruiser/target-db7 system-db cruiser/default-sniff-tests cruiser/default-field-attributes cruiser/default-derived-fields cruiser/default-viz-shapes)  ;; sqlserver
  ;(cruiser/lets-give-it-a-whirl cruiser/target-db6 system-db cruiser/default-sniff-tests cruiser/default-field-attributes cruiser/default-derived-fields cruiser/default-viz-shapes) ;; mysql
  ;(cruiser/lets-give-it-a-whirl cruiser/target-db5 system-db cruiser/default-sniff-tests cruiser/default-field-attributes cruiser/default-derived-fields cruiser/default-viz-shapes) ;; vertica [:= :db-schema "online_sales"]
  ;(cruiser/lets-give-it-a-whirl cruiser/target-db4 system-db cruiser/default-sniff-tests cruiser/default-field-attributes cruiser/default-derived-fields cruiser/default-viz-shapes) ;; clickhouse  [:like :table-name "%name%"]
  ;(cruiser/lets-give-it-a-whirl cruiser/target-db3 system-db cruiser/default-sniff-tests cruiser/default-field-attributes cruiser/default-derived-fields cruiser/default-viz-shapes) ;; postgres

  ;(find-all-viz-fields! system-db default-viz-shapes [:= :connection-id "-1245280001"])
  ;(find-all-viz-combos! system-db default-viz-shapes [:= :connection-id "-1245280001"])

  ;(cruiser/lets-give-it-a-whirl mem-db system-db default-sniff-tests default-field-attributes default-derived-fields default-viz-shapes)
  ;(lets-give-it-a-whirl mem-db system-db default-sniff-tests default-field-attributes default-derived-fields default-viz-shapes [:= :table-name ""])

  ;; (ut/print-ansi-art "name-smol.ans")
  ;; (ut/print-ansi-art "rvbbit-smol.ans")


;; begin bootup runstream procs
    (shutdown/add-hook! ::heads-up-msg #(ut/ppa "Shutting down now, commander!"))



  ;(sql-exec system-db "drop table if exists found_fields ;")
  ;(sql-exec system-db "drop table if exists combos ;")


    (sql-exec system-db "drop table if exists jvm_stats;")
    (sql-exec system-db "drop table if exists errors;")
    (sql-exec system-db "drop view  if exists viz_recos_vw;")
    (sql-exec system-db "drop view  if exists viz_recos_vw2;")
    (sql-exec system-db "drop view  if exists user_subs_vw;")
    (sql-exec system-db "drop view  if exists latest_status;")

    (cruiser/create-sqlite-sys-tables-if-needed! system-db)
    (cruiser/create-sqlite-flow-sys-tables-if-needed! flows-db)
    ;(cruiser/create-sys-tables-if-needed! system-db) ;; sqlite


    ;; (defn fridge-child-atom [ttype key]
    ;;   (let [base-dir (str "./data/atoms/" (cstr/replace (str ttype) ":" ""))
    ;;         _ (ext/create-dirs base-dir) ;; just in case...
    ;;         new-child-atom  (ut/thaw-atom {} (str base-dir "/" key "-atom.edn"))]
    ;;     new-child-atom))

    (add-watch wss/screens-atom :master-screen-watcher ;; watcher splitter
               (fn [_ _ old-state new-state]
                 ;;(ut/pp [:SCREEN-WATCHER-ACTIVATE!])
                 (doseq [key (keys new-state)]
                   (if-let [child-atom (get @wss/screen-child-atoms key)]
                     (swap! child-atom assoc key (get new-state key))
                     (let [new-child-atom (atom {})
                           ;new-child-atom (fridge-child-atom :screens key)
                           ]
                       (swap! wss/screen-child-atoms assoc key new-child-atom)
                       (swap! new-child-atom assoc key (get new-state key)))))))

    (add-watch wss/params-atom :master-params-watcher ;; watcher splitter
               (fn [_ _ old-state new-state]
                 (doseq [key (keys new-state)]
                   (if-let [child-atom (get @wss/param-child-atoms key)]
                     (swap! child-atom assoc key (get new-state key))
                     (let [new-child-atom (atom {})
                           ;new-child-atom (fridge-child-atom :params key)
                           ]
                       (swap! wss/param-child-atoms assoc key new-child-atom)
                       (swap! new-child-atom assoc key (get new-state key)))))))

    (add-watch wss/panels-atom :master-panels-watcher ;; watcher splitter
               (fn [_ _ old-state new-state]
                 (doseq [key (keys new-state)]
                   (if-let [child-atom (get @wss/panel-child-atoms key)]
                     (swap! child-atom assoc key (get new-state key))
                     (let [new-child-atom (atom {})
                           ;new-child-atom (fridge-child-atom :panels key)
                           ]
                       (swap! wss/panel-child-atoms assoc key new-child-atom)
                       (swap! new-child-atom assoc key (get new-state key)))))))

    (add-watch wss/signals-atom :master-signal-def-watcher ;; watcher signals defs
               (fn [_ _ old-state new-state]
                 ;;(ut/pp [:SCREEN-WATCHER-ACTIVATE!])
                 (ut/pp [:signals-defs-changed :reloading-signals-sys-subs....])
                 (wss/reload-signals-subs)
                 ))

    (wss/reload-signals-subs) ;; run once on boot
    (update-all-screen-meta)
    (update-all-flow-meta)



    (cruiser/insert-current-rules! system-db "system-db" 0
                                   cruiser/default-sniff-tests
                                   cruiser/default-field-attributes
                                   cruiser/default-derived-fields
                                   cruiser/default-viz-shapes
                                   (merge
                                    cruiser/default-flow-functions
                                  ;; (let [] ;; custom categories instead of :sub-flow
                                  ;;   (doseq [[k v] @wss/sub-flow-blocks] {k v}))
                                    {:custom @wss/custom-flow-blocks}
                                    {:sub-flows @wss/sub-flow-blocks}))

  ;(when harvest-on-boot?
    ;(update-all-conn-meta)
  ;)

    ;; (cruiser/lets-give-it-a-whirl-no-viz  ;;; force system-db as a conn, takes a sec
    ;;  "system-db"
    ;;  system-db
    ;;  system-db
    ;;  cruiser/default-sniff-tests
    ;;  cruiser/default-field-attributes
    ;;  cruiser/default-derived-fields
    ;;  cruiser/default-viz-shapes)

    ;; (cruiser/lets-give-it-a-whirl-no-viz  ;;; force system-db as a conn, takes a sec
    ;;  "flows-db"
    ;;  flows-db
    ;;  system-db
    ;;  cruiser/default-sniff-tests
    ;;  cruiser/default-field-attributes
    ;;  cruiser/default-derived-fields
    ;;  cruiser/default-viz-shapes)

    ;; (cruiser/lets-give-it-a-whirl-no-viz  ;;; force cache-db as a conn, takes a sec
    ;;  "cache.db"
    ;;  wss/cache-db
    ;;  system-db
    ;;  cruiser/default-sniff-tests
    ;;  cruiser/default-field-attributes
    ;;  cruiser/default-derived-fields
    ;;  cruiser/default-viz-shapes)


    (shell/sh "/bin/bash" "-c" (str "rm -rf " "live/*"))

    (tt/start!)
    (def mon (tt/every! 30 5 (bound-fn [] (wss/jvm-stats)))) ;; update stats table every 30 seconds (excessive, lenghten in prod..)
    ;(instrument-jvm instrument/metric-registry)
    (wss/subscribe-to-session-changes)

      ;;(wss/start-thread-worker) ;; used for side-car sniffs
      ;(def worker-thread (wss/start-thread-worker))
      ;(def lunchbreak (tt/every! 90 2 (bound-fn [] (wss/recycle-worker worker-thread)))) ;; "reboot" the sniffer worker thread every 30 mins

    (def lunchbreak  (tt/every! 36000 2 (bound-fn [] (wss/recycle-worker)))) ;; "reboot" the sniffer worker thread every hour
    (def lunchbreak2 (tt/every! 36000 2 (bound-fn [] (wss/recycle-worker2))))
    (def lunchbreak3 (tt/every! 36000 2 (bound-fn [] (wss/recycle-worker3))))

        ;;(def lucene-commiter (tt/every! 30 2 (bound-fn [] (search/commit-writer search/index-writer))))
    (def param-sync (tt/every! 30 2 (bound-fn [] (wss/param-sql-sync))))

    (def refresh-flow-tables (tt/every! 5 2 (bound-fn [] (wss/flow-atoms>sql)))) ;; was 30. 5 too much?

    (def purge (tt/every! 30 2 (bound-fn [] (wss/purge-dead-client-watchers))))

    (def timekeeper (tt/every! 1 3 (bound-fn [] (reset! wss/time-atom (ut/current-datetime-parts)))))

    (def last-look (atom {}))
    (def saved-uids (atom []))


    (shutdown/add-hook! ::the-pool-is-now-closing
                        #(do (tt/stop!)
                             (wss/stop-worker)
                             (wss/stop-worker2)
                             (wss/stop-worker3)
                             (reset! wss/shutting-down? true)))

    (shutdown/add-hook! ::the-pool-is-now-closed
                        #(doseq [[conn-name conn] @wss/conn-map]
                           (do (ut/ppa [:shutting-down-connection-pool conn-name conn])
                               (try (hik/close-datasource (get conn :datasource))
                                    (catch Exception e (ut/pp [:close-error e]))))))

    (shutdown/add-hook! ::close-system-pools #(do
                                                (let [destinations (vec (keys @wss/client-queues))]
                                                  (doseq [d destinations]
                                                    (wss/alert! d [:v-box
                                                                   :justify :center
                                                                   :style {:opacity 0.7}
                                                                   :children [[:box
                                                                               :style {:color :theme/editor-outer-rim-color :font-weight 700}
                                                                               :child
                                                                               ;[:speak-always (str "Heads up: R-V-B-B-I-T system going offline.")]
                                                                               [:box :child (str "Heads up: R-V-B-B-I-T system going offline.")]]]] 10 1 5)))
                                                (Thread/sleep 1000)
                                                ;; (do (ut/pp [:shutting-down-lucene-index-writers])
                                                ;;     (search/close-index-writer search/index-writer))
                                                (wss/destroy-websocket-server!)
                                                (wss/stop-web-server!)
                                                (wss/stop-worker)
                                                (wss/stop-worker2)
                                                (wss/stop-worker3)
                                                (ut/ppa [:shutting-down-system-pools])
                                                (hik/close-datasource (get system-db :datasource))))

    (shutdown/add-hook! ::clear-cache #(do (ut/ppa [:freezing-system-atoms])
                                           (ut/freeze-atoms)
                                           (ut/ppa [:freezing-flow-results-atom])
                                           (with-open [wtr (io/writer "./data/atoms/flow-db-results-atom.edn")]
                                             (binding [*out* wtr]
                                               (prn @flow-db/results-atom)))
                                           (ut/ppa [:clearing-cache-db])
                                           (shell/sh "/bin/bash" "-c" (str "rm " "db/cache.db"))
                                           ;(shell/sh "/bin/bash" "-c" (str "rm " "db/flow.db"))
                                           ;(shell/sh "/bin/bash" "-c" (str "rm " "flow-history/src-maps/*"))
                                           ;(shell/sh "/bin/bash" "-c" (str "rm " "flow-history/src-maps-pre/*"))
                                           ;(shell/sh "/bin/bash" "-c" (str "rm " "flow-history/*"))
                                           (shell/sh "/bin/bash" "-c" (str "rm " "status-change-logs/*"))
                                           (shell/sh "/bin/bash" "-c" (str "rm " "tracker-logs/*"))
                                           (shell/sh "/bin/bash" "-c" (str "rm " "data/search-index/*"))
                                           (shell/sh "/bin/bash" "-c" (str "rm " "reaction-logs/*"))
                                           (shell/sh "/bin/bash" "-c" (str "rm " "db/system.db"))))



  ;; (let [create-out (ass/create-assistant)
  ;;     list-assistants (first (ass/list-assistants))]
  ;; (ut/pp [:CREATE])
  ;; (ut/pp create-out)

  ;; (ut/pp [:LIST])
  ;; (ut/pp list-assistants)

  ;; (ut/pp [:UPLOAD])
  ;; (ut/pp (ass/upload-file "./meta.csv"))
  ;; (ut/pp (ass/upload-file "./recos.csv"))
  ;; (ut/pp (ass/upload-file "./canvas.json"))
  ;; ;(ut/pp [:START-THREAD])
  ;; )


  ;; TODO, dangerous as hell
  ;;(io/delete-file "./live/" :recursive true)





    (defn update-stat-atom [kks]
      (let [;flows (if (or (nil? flows) (empty? flows))
          ;        (keys @flow-db/results-atom)
          ;        (vec flows))
          ;flows (or flows (keys @flow-db/results-atom))
            ]
      ;(ut/pp [:update-stats-atom-for flows])
        (swap! wss/flow-status merge
             ;(merge @wss/flow-status
               (into {}
                     (for [k kks ;(keys @flow-db/results-atom)
                           :when (not (= k "client-keepalive"))] ;; dont want to track heartbeats
                       (let [;; _ (ut/pp [:flow-status-atom-all! k (get @flow-db/results-atom k)
                            ;;           :status! (get @flow-db/status k)
                            ;;           :tracker! (get @flow-db/tracker k)])
                            ;; condis (get-in @flow-db/status [k :condis])

                             cc (into {} (for [[k v] @flow-db/results-atom] {k (count v)})) ;; wtf? doing way too much work here
                             blocks (get-in @flow-db/working-data [k :components-list])
                             running-blocks (vec (for [[k v] (get @flow-db/tracker k)
                                                       :when (nil? (get v :end))]
                                                   k))
                             done-blocks (vec (for [[k v] (get @flow-db/tracker k)
                                                    :when (not (nil? (get v :end)))]
                                                k))
                             not-started-yet (vec (cset/difference
                                                   (set blocks)
                                                   (set (into running-blocks done-blocks))))
                             running-blocks (vec (cset/difference (set blocks) (set (into done-blocks not-started-yet))))

                             res (get-in @flow-db/results-atom [k :done] :nope)
                             running? (or (= res :nope) (ut/chan? res) (= res :skip))
                             done? (not (= res :nope))
                             error? (try (some #(or (= % :timeout) (= % :error)) (ut/deep-flatten res)) (catch Exception _ false))
                             _ (when error? (swap! wss/temp-error-blocks assoc k not-started-yet))
                             start (try (apply min (or (for [[_ v] (get @flow-db/tracker k)] (get v :start)) [-1]))
                                        (catch Exception _ (System/currentTimeMillis)))
                             start-ts (ut/millis-to-date-string start)
                             end (try (apply max (or (remove nil? (for [[_ v] (get @flow-db/tracker k)]
                                                                    (get v :end))) [-1]))
                                      (catch Exception e (do ;(ut/pp [:exception-in-getting-time-duration!! k (str e)
                                                             ;        (for [[_ v] (get @flow-db/tracker k)] (get v :end))
                                                             ;        (get @flow-db/tracker k)
                                                             ;        (get @flow-db/last-tracker k)
                                                             ;        [:last-good-time? (get @wss/last-times k) (vec (take-last 10 (get @wss/last-times k)))]
                                                             ;        @wss/last-times
                                                             ;        :start start])
                                                           (System/currentTimeMillis))))
                            ;;  elapsed (if (or error? (empty? (get @flow-db/tracker k)))
                            ;;            (- (System/currentTimeMillis) (get-in @wss/last-times [k :start]))
                            ;;            (- end start))
                             start (if error? (get-in @wss/last-times [k :start]) start)
                             end (if error? (System/currentTimeMillis) end)
                             elapsed (try (- end start) (catch Throwable e (do (ut/pp [:elapsed-error?? k e :start start :end end]) 0)))
                             ;;_ (when (and (get @flow-db/tracker k) (> elapsed 1) (not (nil? elapsed))) (swap! last-times assoc k elapsed))
                             ;;_ (swap! wss/last-times assoc k (conj (take-last 10 (remove #(<= % 0) (get @wss/last-times k []))) elapsed))
                             _ (swap! wss/last-times assoc-in [k :end] (System/currentTimeMillis))
                             human-elapsed (ut/format-duration start end)
                             run-id (str (get-in @flow-db/results-atom [k :run-id]))
                             parent-run-id (str (get-in @flow-db/results-atom [k :parent-run-id]))
                            ;; _ (ut/pp [:uid (str (get-in @flow-db/results-atom [k :run-id]))
                            ;;           (str (get-in @flow-db/results-atom [k :opts-map :client-id] "n/a"))
                            ;;           done? (get @last-look k)])
                             overrides (get-in @flow-db/subflow-overrides [k run-id])
                             cname (get-in @flow-db/results-atom [k :opts-map :client-name]
                                           (get @wss/orig-caller k :rvbbit-scheduler))
                             client-name (str cname)
                            ;; orig-tracker (get @flow-db/tracker k)
                            ;; tracker (into {} (for [[k v] orig-tracker ;; remove condi non-starts. dont send to client. data noise. neccessary noise for reactions, but noise
                            ;;                        :when (not (get v :in-chan?))]
                            ;;                    {k v}))
                            ;_ (ut/pp [:fid k client-name (get-in @flow-db/results-atom [k :opts-map] "?")])
                             run-sql? (and done?
                                           (not (some #(= % run-id) @saved-uids))
                                          ;(not (get @last-look k))
                                           )
                             _ (when (and done? (not error?))
                                 (swap! wss/times-atom assoc k (conj (get @wss/times-atom k []) (int (/ elapsed 1000)))))
                             _ (when run-sql?
                                 (try
                                   (let [row {:client_name client-name
                                              :flow_id (str k)
                                              :started start ;(get-in @flow-db/results-atom [k :start])
                                              :start_ts start-ts
                                              :ended end ;(get-in @flow-db/results-atom [k :end])
                                              :run_id run-id
                                              :parent-run_id parent-run-id
                                              :elapsed elapsed
                                              :overrides (pr-str overrides)
                                              :elapsed_seconds (float (/ elapsed 1000))
                                              :in_error (cstr/includes? (str res) ":error")
                                              :human_elapsed (str human-elapsed)}
                                        ;;_ (ut/pp [:inserting?])
                                         insert-sql {:insert-into [:flow_history] :values [row]}]
                                                                ;(ut/pp [:flow-done k])
                                     (wss/enqueue-task3 ;; temp remove to test some shit 3/9/24
                                      (fn []
                                        (sql-exec flows-db (to-sql insert-sql)))))
                                   (catch Exception e (ut/pp [:flow-history-insert-error k (str e)]))))

                             _ (swap! last-look assoc k done?)
                             _ (when run-sql? (swap! saved-uids conj run-id))
                            ;;  _ (when (and run-sql? (not error?) done?) ;; TEMP REMOVE FOR CHANNEL PUSHING - causes flow to stay in "running" status?
                            ;;      (ut/delay-execution 4000 (fn [] (wss/remove-watchers-for-flow (str k)))))
                            ;chans (count (get @flow-db/channels-atom k))
                           ;_ (when done? (ut/delay-execution 5000 (fn [] (swap! wss/flow-status assoc-in [k :*running?] false))))
                             chans-open (count
                                         (doall
                                          (map (fn [[_ ch]]
                                                 (let [vv (try (not (ut/channel-open? ch))
                                                               (catch Throwable e (str e)))]
                                                   (if (cstr/includes? (str vv) "put nil on channel") :open vv)))
                                               (get @flow-db/channels-atom k))))
                             channels-open? (true? (> chans-open 0))]

                         {k {:*done? done? ;(true? (not (nil? res)))
                             :*started-by cname
                             :*channels-open? channels-open?
                             :*channels-open chans-open
                             ;:tracker-events (count (get @wss/tracker-history k []))

                             :running-blocks running-blocks
                             :done-blocks done-blocks
                             :waiting-blocks not-started-yet
                             :error-blocks (get @wss/temp-error-blocks k [])

                             :overrides overrides
                             :started start
                             ;:*result (str res)
                             :*finished (count done-blocks)
                             :*running running-blocks
                             :*done done-blocks
                             :*ms-elapsed elapsed
                             :*time-running (str human-elapsed)
                             :*not-started not-started-yet
                             :*open-channels (get cc k)
                             :*running? running?}}))))))

    (defn tracker-changed [key ref old-state new-state]
      (let [[_ b _] (data/diff old-state new-state)
            kks (try (keys b) (catch Exception _ nil))]
        (when (> (count (remove #(= "client-keepalive" %) kks)) 0)

          ;;(ut/pp [:trigger-status-changes! kks])

          (async/thread ;; really expensive logging below. temp
            (let [fp (str "./status-change-logs/" (-> (cstr/join "-" kks) (cstr/replace " " "_") (cstr/replace "/" ":")) "@" (str (System/currentTimeMillis)) ".edn")]
              (ext/create-dirs "./status-change-logs/")
              (ut/pretty-spit fp {:kks kks
                                  :res (ut/replace-large-base64 (select-keys @flow-db/results-atom kks)) ;(select-keys @flow-db/results-atom (filter #(cstr/includes? (str %) "node-js-color-thief-script") (keys @flow-db/results-atom)))
                                  :tracker (ut/replace-large-base64 (select-keys @flow-db/tracker kks))
                                  :diff (ut/replace-large-base64 b)} 125)))

          (update-stat-atom kks))
      ;(when kks (ut/pp [:flow-finished kks]))
        ))

    (add-watch flow-db/status :tracker-watch tracker-changed)
  ;(add-watch flow-db/status :tracker-watch update-stat-atom)
  ;(add-watch flow-db/tracker :tracker-watch update-stat-atom) ;; tracker could be too much...


    (defn log-tracker [kks]
      (doseq [flow-id kks]
        (let [orig-tracker (get @flow-db/tracker flow-id)
              tracker (into {} (for [[k v] orig-tracker ;; remove condi non-starts. dont send to client. data noise. neccessary noise for reactions, but noise
                                     :when (not (get v :in-chan?))]
                                 {k v}))]
          (swap! wss/tracker-history assoc flow-id (vec (conj (get @wss/tracker-history flow-id []) tracker))) ;; for loop step history saving..
          )))


    ;; expensive, tracking down some issues
    (defn tracker-changed2 [key ref old-state new-state]
      (let [[_ b _] (data/diff old-state new-state)
            kks (try (keys b) (catch Exception _ nil))]

        (when (> (count (remove #(= "client-keepalive" %) kks)) 0)

          ;(ut/pp [:tracker-changes! kks])

          (async/thread ;; really expensive logging below. temp

            (let [;kks-string (-> (cstr/join "-" kks) (cstr/replace " " "_") (cstr/replace "/" ":"))
                  ;block-changed (first (keys (get b (first kks))))
                  ;what-changed (first (remove #(= % :in-chan?) (keys (get-in b [(first kks) (first (keys (get b (first kks))))]))))
                  ;summary (-> (str block-changed "=" what-changed) (cstr/replace " " "_") (cstr/replace "/" ":"))
                  ;fp (str "./tracker-logs/" (str (System/currentTimeMillis)) "@@" kks-string "=" summary ".edn")
                  ]

              ;; (ext/create-dirs "./tracker-logs/")
              ;; (ut/pretty-spit fp {:kks kks
              ;;                     :value (ut/replace-large-base64 (get-in @flow-db/results-atom [(first kks) block-changed]))
              ;;                     ;:res (ut/replace-large-base64 (select-keys @flow-db/results-atom kks)) ;(select-keys @flow-db/results-atom (filter #(cstr/includes? (str %) "node-js-color-thief-script") (keys @flow-db/results-atom)))
              ;;                     ;:tracker (ut/replace-large-base64 (select-keys @flow-db/tracker kks))
              ;;                     :diff (ut/replace-large-base64 b)
              ;;                     :full (ut/replace-large-base64 (get-in @flow-db/results-atom [(first kks)]))} 125)

              (ext/create-dirs "./flow-logs/")
              (doseq [flow-id kks
                      :let [run-id (str (get-in @flow-db/results-atom [flow-id :run-id]))
                            fp (str "./flow-logs/" (cstr/replace flow-id "/" "-CALLING-") "--" run-id ".edn")
                            mst (System/currentTimeMillis)
                            _ (swap! wss/watchdog-atom assoc flow-id mst)
                            _ (swap! wss/watchdog-atom assoc flow-id mst)
                            diffy (get (ut/replace-large-base64 b) flow-id)
                            diffy-keys (into {} (for [[k v] diffy] {k (first (keys v))}))
                            _ (swap! wss/last-block-written assoc flow-id diffy-keys)
                            blocks (keys (get (ut/replace-large-base64 b) flow-id))]]
                (let [data [[(ut/millis-to-date-string mst) blocks]
                            {:values (select-keys (ut/replace-large-base64 (get-in @flow-db/results-atom [flow-id])) blocks)
                             :diff diffy}]
                      pretty-data (with-out-str (ppt/pprint data))] ; Use clojure.pprint/pprint instead of puget/cprint
                  (spit fp (str pretty-data "\n") :append true)))))

          (log-tracker kks)
          (update-stat-atom kks))
      ;(when kks (ut/pp [:flow-finished kks]))
        ))
    (add-watch flow-db/tracker :tracker-watch tracker-changed2)


  ;; ;;; temp, SUPER expensive logging
  ;;   (defn tracker-changed2 [key ref old-state new-state]
  ;;   (let [[_ b _] (data/diff old-state new-state)
  ;;         kks (try (keys b) (catch Exception _ nil))]
  ;;     ;(when (> (count (remove #(= "client-keepalive" %) kks)) 0)

  ;;         ;(ut/pp [:trigger-status-change! kks {:diff b}])

  ;;       (when (not (nil? b))
  ;;         (async/thread ;; really expensive logging below. temp
  ;;           (let [fp (str "./status-change-logs/" (str (System/currentTimeMillis)) ".tracker.edn")]
  ;;             (ext/create-dirs "./status-change-logs/")
  ;;             (ut/pretty-spit fp {:kks kks
  ;;                               ;:res (select-keys @flow-db/results-atom kks) ;(select-keys @flow-db/results-atom (filter #(cstr/includes? (str %) "node-js-color-thief-script") (keys @flow-db/results-atom)))
  ;;                                 :diff (ut/replace-large-base64 b)} 125))))

  ;;       ;)
  ;;       ;(when kks (ut/pp [:flow-finished kks]))
  ;;     ))

  ;;     (add-watch flow-db/tracker :tracker-watch tracker-changed2)



  ;; (def flow-status1 (tt/every! 1 (bound-fn []
  ;;                                    (update-stat-atom) )))

  ;(def lunchbreak (tt/every! 200 2 (bound-fn [] (wss/cancel-all-tasks))))



  ;(ut/pp (evl/repl-eval "(println \"heys\") 123 {:mapkpy 123} true"))
  ;; (ut/pp (evl/repl-eval '(let [] (println (str "heys!" (rand-int 45))) 123 {:mapkpy 123})))
  ;; (ut/pp (evl/repl-eval '(let [] (vec (map
  ;;                                      (fn [id]
  ;;                                        (let [titles
  ;;                                              ["Adventures in Clojure"
  ;;                                               "The Functional Artist"
  ;;                                               "Mystery of the Monads" "Lambda's Journey"
  ;;                                               "The Cursed Algorithm"
  ;;                                               "Secrets of the Compiler" "Recursive Dreams"
  ;;                                               "Shadow in the Data"]
  ;;                                              genres ["Fantasy" "Science Fiction" "Mystery"
  ;;                                                      "Adventure" "Non-Fiction"]]
  ;;                                          (println (str id "..."))
  ;;                                          {:book-id id
  ;;                                           :title (rand-nth titles)
  ;;                                           :genre (rand-nth genres)
  ;;                                           :page-count (rand-int 800)
  ;;                                           :rating (Double/parseDouble
  ;;                                                    (format "%.1f"
  ;;                                                            (+ 1 (* 0.1 (rand-int 50)))))
  ;;                                           :year (+ 1980 (rand-int 44))}))
  ;;                                      (range 10))))))


    (ut/pp {:settings config/settings})
    ;; (ut/pp {:kits-loaded config/kits})


  ;; (defn create-windows-test []
  ;;   (let [gg (fn [q] (vec (sql-query system-db
  ;;                                    (to-sql q))))
  ;;         databases (gg {:select [:connection_id
  ;;                                 [[:count [:distinct :table_name]]
  ;;                                  :tables]]
  ;;                        :from
  ;;                        [[{:select [:connection_id :context_hash
  ;;                                    :data_type :db_catalog :db_schema
  ;;                                    :db_type :derived_calc
  ;;                                    :derived_name :field_name
  ;;                                    :field_type :is_group_by
  ;;                                    :key_hash :run_id :table_name
  ;;                                    :table_type :updated]
  ;;                           :from   [[:fields :hh447]]} :aa29]]
  ;;                        :group-by [:connection_id]
  ;;                        :order-by [[2 :desc]]})
  ;;         tables (gg {:select [:table_name :connection_id
  ;;                              [[:count 1] :fields]]
  ;;                     :from
  ;;                     [[{:select [:connection_id :context_hash
  ;;                                 :data_type :db_catalog :db_schema
  ;;                                 :db_type :derived_calc
  ;;                                 :derived_name :field_name
  ;;                                 :field_type :is_group_by
  ;;                                 :key_hash :run_id :table_name
  ;;                                 :table_type :updated]
  ;;                        :from   [[:fields :hh447]]} :kk765]]
  ;;                     :group-by [:table_name :connection_id]
  ;;                     :order-by [[3 :desc]]})
  ;;         database-block (fn [] )
  ;;         ]


  ;;                     )
  ;;   )


  ;(flow/flow first-flow {:debug? true})


  ;(keys @wss/client-queues) to get all clients

  ;(defn get-client-subs [client-name] (vec (keys (get @wss/atoms-and-watchers client-name))))

  ;; quoted the functions here because im using flow! that evals them, if you use regular flowmaps.core/flow you don't need to quote then
  ;; since flowmaps assumes that they are already compiled function objects, but here we have fns as data, so my wrapper assumes they are
  ;; quoted so it evals them into fn objects before passing them to flowmaps.core/flow... TODO, detect if needed. 2/1/24


    (defn heartbeat [dest] {:components
                            {:kick-1
                             {:inputs [:destination :name :sub-task :thread-id :thread-desc :message-name],
                              :fn '(fn [destination name sub-task thread-id thread-desc message-name & args]
                                     (rvbbit-backend.websockets/kick destination name sub-task thread-id thread-desc message-name args))
                              :raw-fn '(fn [destination name sub-task thread-id thread-desc message-name & args]
                                         (rvbbit-backend.websockets/kick destination name sub-task thread-id thread-desc message-name args))}
                             :open-input :heartbeat
                             ;:get-client-subs get-client-subs
                             ;; last step, remove queue for delinq clients, etc - save :last-seen timestamps
                             :kick-1destination dest}
                            :connections [[:kick-1 :done]
                                          [:open-input :kick-1/name]
                                          [:open-input :kick-1/sub-task]
                                          [:open-input :kick-1/thread-id]
                                          [:open-input :kick-1/thread-desc]
                                          [:open-input :kick-1/message-name]
                                          [:kick-1destination :kick-1/destination]]})


  ;; (ut/pp [:fucking-http-call (wss/http-call {:url "https://oaidalleapiprodscus.blob.core.windows.net/private/org-YLngjih2M4oju0tnnZtKCAYg/user-shufdPruUBHCac1ylMZBDu4J/img-xElH7oGe63C8W9422OXz3shM.png?st=2024-01-06T08%3A30%3A21Z&se=2024-01-06T10%3A30%3A21Z&sp=r&sv=2021-08-06&sr=b&rscd=inline&rsct=image/png&skoid=6aaadede-4fb3-4698-a8f6-684d7786b067&sktid=a48cca56-e6da-484e-a814-9c849652bcb3&skt=2024-01-05T17%3A08%3A33Z&ske=2024-01-06T17%3A08%3A33Z&sks=b&skv=2021-08-06&sig=OswRl0Yu8nGMmfaMZCwby4IbYllGj%2BUvgH7zW/2Xq0c%3D"
  ;;                                            :save-to "/tmp/tester.png"
  ;;                                            :method :get
  ;;                                            :headers {"Authorization"
  ;;                                                      "Bearer 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"
  ;;                                                      "Accept" "image/*"
  ;;                                                      ;"Content-Type" "application/json"
  ;;                                                      }})])
  ;(System/exit 0)

  ;; (ut/pp (ut/save-base64-to-png (slurp "/home/ryanr/b64-blob.txt") "/home/ryanr/tester22.png"))
  ;; (ut/pp (ut/save-base64-to-png2 (slurp "/home/ryanr/b64-blob.txt") "/home/ryanr/tester22b.png"))
  ;; (ut/pp (ut/process-and-save-image (str "data:image/png;base64," (slurp "/home/ryanr/b64-blob.txt")) "/home/ryanr/tester22ccc.png"))
  ;; (ut/pp (ut/write-base64-image (slurp "/home/ryanr/b64-blob1.txt") "/home/ryanr/tester22a.png"))

  ;; (println (subs (slurp "/home/ryanr/b64-blob.txt") 0 100))

;;  curl -H "Authorization: Bearer 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" -o output_image.png "https://oaidalleapiprodscus.blob.core.windows.net/private/org-YLngjih2M4oju0tnnZtKCAYg/user-shufdPruUBHCac1ylMZBDu4J/img-xElH7oGe63C8W9422OXz3shM.png?st=2024-01-06T08%3A30%3A21Z&se=2024-01-06T10%3A30%3A21Z&sp=r&sv=2021-08-06&sr=b&rscd=inline&rsct=image/png&skoid=6aaadede-4fb3-4698-a8f6-684d7786b067&sktid=a48cca56-e6da-484e-a814-9c849652bcb3&skt=2024-01-05T17%3A08%3A33Z&ske=2024-01-06T17%3A08%3A33Z&sks=b&skv=2021-08-06&sig=OswRl0Yu8nGMmfaMZCwby4IbYllGj%2BUvgH7zW/2Xq0c%3D"

  ;(wss/jvm-stats true)

  ;; (flow-web/start!)
  ;(flow/schedule! [:seconds 5] (test-client :elegant-sangria-serval-banned-from-seamount) {:flow-id "client-1" :increment-id? false :close-on-done? true :debug? false})
  ;(flow/schedule! [:seconds 6] (test-client :quick-blue-dinosaur-banned-from-erg) {:flow-id "client-1" :increment-id? false :close-on-done? true :debug? false})
  ;(flow/schedule! [:seconds 4] (test-client :appealing-white-hyena-hailing-from-hogback) {:flow-id "client-1" :increment-id? false :close-on-done? true :debug? false})


    (wss/schedule! [:seconds 45] (heartbeat :all)
                   {:flow-id "client-keepalive" :increment-id? false :close-on-done? true :debug? false})

  ;; (wss/schedule! [:minutes 5] fex/looping-net
  ;;                {:flow-id "loop-time1!" :close-on-done? true :debug? false})

  ;; (wss/schedule! [:seconds 30] (wss/materialize-flowmap :server "map-pull-test2" "map-pull-test2" {})
  ;;                {:debug? false :close-on-done? true :increment-id? false :flow-id "map-pull-test2"})

    (wss/schedule! [:minutes 20] "crow-flow-201a"
                   {:close-on-done? true :increment-id? false :flow-id "crow-flow-201a" :debug? false})

    (wss/schedule! [:minutes 30] "counting-loop"
                   {:flow-id "counting-loop" :increment-id? false :close-on-done? true :debug? false})

  ;(ut/pp [:flow-answer1 (wss/flow-waiter ff "boot-test")])
  ;(ut/pp [:flow-answer2 (flow-waiter fex/looping-net)])
  ;(ut/pp [:flow-answer3 (flow-waiter fex/looping-net)])

  ;; (doall
  ;;  (let [_ (ut/pp [:start-flow])
  ;;        res (flow/flow-waiter flow/my-network)]
  ;;    (ut/pp [:flow-answer res])))

    ;;(try (wss/flow-atoms>sql) (catch Exception e (ut/pp [:flow-atoms>sql-error (str e)])))



  ;(def rez (atom nil))
  ;(doall (flow/flow flow/my-network {:debug? true :flow-id "yoyoyoy"} wss/rez))


  ;;  (defn delay-execution [ms f]
  ;;    (future (do (Thread/sleep ms) (f nil))))

  ;;   (ut/pp [:waiting-for-background-systems...])
  ;;   @(delay-execution
  ;;    30000
  ;;    (fn [_]
  ;;      (evl/create-nrepl-server!)
  ;;      (wss/create-web-server!)
  ;;      (wss/create-websocket-server!)
  ;;      (evl/create-nrepl-server!)))

;; (defn delay-execution [ms f]
;;   (.start (Thread. (fn [] (Thread/sleep ms) (f)))))

;; (def delayed-startup (delay (do
;;                               (evl/create-nrepl-server!)
;;                               (wss/create-web-server!)
;;                               (wss/create-websocket-server!)
;;                               (evl/create-nrepl-server!))))

;; (ut/pp [:waiting-for-background-systems...])
;; (delay-execution 30000 #(force delayed-startup))

    (ut/pp [:waiting-for-background-systems...])
    ;; (delayed-start 30000 start-services)

    (let [fut (future  (Thread/sleep 15000) (start-services))
          ;fut2 (future (Thread/sleep 15000) (wss/create-websocket-server!))
          ]
      ;[@fut @fut2]
      @fut)

    (println " ")

;;  (ut/print-ansi-art "rvbbit.ans")

;; end runstream boot








  ;;(ut/pp [:websocket-state wl/get-state])

;; (ts/transform {:transform-select [[[:count :id] :cnt2]
;;                                   :country :state]
;;                :pivot-by [:shape]
;;                :from [:query/ufo-sightings-drag-304]} ts/data2)

  ;; (cruiser/lets-give-it-a-whirl-no-viz cruiser/target-db2 system-db cruiser/default-sniff-tests cruiser/default-field-attributes cruiser/default-derived-fields cruiser/default-viz-shapes) ;; sqlite
  ;; (cruiser/lets-give-it-a-whirl-no-viz cruiser/target-db  system-db cruiser/default-sniff-tests cruiser/default-field-attributes cruiser/default-derived-fields cruiser/default-viz-shapes)  ;; sqlite
  ;; (cruiser/lets-give-it-a-whirl-no-viz cruiser/target-db5 system-db cruiser/default-sniff-tests cruiser/default-field-attributes cruiser/default-derived-fields cruiser/default-viz-shapes) ;; vertica
  ;; (cruiser/lets-give-it-a-whirl-no-viz cruiser/target-db4 system-db cruiser/default-sniff-tests cruiser/default-field-attributes cruiser/default-derived-fields cruiser/default-viz-shapes) ;; clickhouse

  ;(wss/process-csv {} "data/ref_us_counties.csv")
    )

