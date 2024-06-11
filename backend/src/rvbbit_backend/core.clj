(ns rvbbit-backend.core
  (:require
    [clj-http.client :as client]
    [clj-time.coerce :as tcc]
    [clj-time.core :as t]
    [clj-time.format :as f]
    [clj-time.jdbc]
    [clojure.core.async :as    async
                        :refer [<! <!! >! >!! chan go]]
    [clojure.core.async :refer [<! timeout]]
    [clojure.data :as data]
    [clojure.edn :as edn]
    [clojure.java.io :as io]
    [clojure.java.jdbc :as jdbc]
    [clojure.java.shell :as shell]
    [clojure.math.combinatorics :as combo]
    [clojure.pprint :as ppt]
    [clojure.set :as cset]
    [clojure.string :as cstr]
    [clojure.walk :as walk]
    [flowmaps.core :as flow]
    [flowmaps.db :as flow-db]
    [flowmaps.examples.simple-flows :as fex]
    [flowmaps.web :as flow-web]
    [hikari-cp.core :as hik]
    [honey.sql :as honey]
    [nextjournal.beholder :as beholder]
    [puget.printer :as puget]
    [ring.adapter.jetty9 :as jetty]
    [rvbbit-backend.assistants :as ass]
    [rvbbit-backend.config :as config]
    [rvbbit-backend.cruiser :as cruiser]
    [rvbbit-backend.embeddings :as em]
    [rvbbit-backend.evaluator :as evl]
    [rvbbit-backend.external :as ext]
    [rvbbit-backend.sql :as    sql
                        :refer [flows-db insert-error-row! pool-create sql-exec sql-query sql-query-meta sql-query-one system-db
                                to-sql]]
    [rvbbit-backend.surveyor :as surveyor]
    [rvbbit-backend.transform :as ts]
    [rvbbit-backend.util :as    ut
                         :refer [ne?]]
    [rvbbit-backend.websockets :as wss]
    [shutdown.core :as shutdown]
    [taskpool.taskpool :as tp]
    [tea-time.core :as tt]
    [websocket-layer.network :as net]) ;; enables joda jdbc time returns
  (:import
    [java.util Date]
    java.nio.file.Files
    java.nio.file.Paths
    java.nio.file.attribute.FileTime
    java.nio.file.LinkOption
    java.time.format.DateTimeFormatter
    java.time.ZoneId))

(def harvest-on-boot? (get config/settings :harvest-on-boot? true))




(defn get-last-modified-time
  [f-path]
  (let [path          (Paths/get f-path (into-array String []))
        instant       (.toInstant (Files/getLastModifiedTime path (into-array LinkOption [LinkOption/NOFOLLOW_LINKS])))
        zonedDateTime (.atZone instant (ZoneId/systemDefault))
        formatter     (DateTimeFormatter/ofPattern "yyyy-MM-dd HH:mm:ss")]
    (.format formatter zonedDateTime)))

(def invalid-to-valid-keywords {":>" ":reagent-gt"})
(def valid-to-invalid-keywords (zipmap (vals invalid-to-valid-keywords) (keys invalid-to-valid-keywords)))
(defn convert-to-valid-edn
  [s]
  (reduce (fn [s [invalid valid]] (clojure.string/replace s invalid valid)) s invalid-to-valid-keywords))

(defn convert-back-to-original
  [m]
  (clojure.walk/postwalk (fn [x] (if (and (keyword? x) (valid-to-invalid-keywords x)) (valid-to-invalid-keywords x) x)) m))

(defn read-screen
  [f-path]
  (let [screen-str    (slurp f-path)
        valid-edn-str (convert-to-valid-edn screen-str)
        screen-data   (try (edn/read-string valid-edn-str) (catch Exception e (ut/pp [:read-screen-error!!!!! f-path e]) {}))]
    (convert-back-to-original screen-data)))

(defn update-screen-meta
  [f-path]
  (let [;file-path "./screens/"
        screen-data      (read-screen f-path)
        screen-name      (get screen-data :screen-name "unnamed-screen!")
        resolved-queries (get screen-data :resolved-queries)
        screen-data      (if (ut/ne? resolved-queries)
                           (assoc screen-data
                             :panels (into {}
                                           (for [[k v] (get screen-data :panels)]
                                             {k (assoc v :queries (select-keys resolved-queries (keys (get v :queries))))})))
                           screen-data)
        theme-map        (get-in screen-data [:click-param :theme])
        has-theme?       (and (not (nil? theme-map)) (ut/ne? theme-map))
        theme-name       (if (and has-theme? (not (nil? (get theme-map :theme-name))))
                           (str ", " (get theme-map :theme-name) " ")
                           "")
        blocks           (remove nil? (conj (keys (get-in screen-data [:panels])) (when has-theme? :*theme*)))
        boards           (distinct (for [[_ v] (get-in screen-data [:panels])] (get v :tab)))
        blocks           (into blocks boards) ; (map #(str "board/" %) boards))
        params           (get-in screen-data [:click-param])
        board-map        (into {}
                               (for [b boards]
                                 {b (into {} (for [[k v] (get-in screen-data [:panels]) :when (= (get v :tab) b)] {k v}))}))
        queries          (into {} (for [b blocks] (get-in screen-data [:panels b :queries])))]
    (doseq [b blocks] ;; collect keywords from all views for autocomplete - might get
      (doseq [[_ vv] (get-in screen-data [:panels b :views])]
        (reset! wss/autocomplete-view-atom (vec (filter #(and (not (cstr/includes? (str %) "/")) (keyword? %))
                                                  (distinct (into (vec (distinct (ut/deep-flatten vv)))
                                                                  @wss/autocomplete-view-atom)))))))
    (swap! wss/screens-atom assoc screen-name screen-data) ;; update master screen atom for
    (sql-exec system-db (to-sql {:delete-from [:screens] :where [:= :file_path f-path]}))
    (sql-exec system-db (to-sql {:delete-from [:blocks] :where [:= :file_path f-path]}))
    (sql-exec system-db
              (to-sql {:insert-into [:screens]
                       :columns     [:file_path :screen_name :blocks :queries]
                       :values      [[f-path screen-name (count blocks) (count (keys queries))]]}))
    (sql-exec system-db
              (to-sql
                {:insert-into [:blocks]
                 :columns     [:file_path :screen_name :block_key :block_name :views :queries :view_names :query_names :block_data
                               :tab_name]
                 :values      (for [b blocks]
                                (let [theme?     (true? (= b :*theme*))
                                      board?     (some #(= b %) boards) ;(cstr/starts-with? (str b)
                                      queries    (get-in screen-data [:panels b :queries])
                                      views      (if theme? theme-map (get-in screen-data [:panels b :views]))
                                      block-name (str (cond theme? (str "(meta: this screen's theme" theme-name ")")
                                                            board? (str "board: " b)
                                                            :else  (get-in screen-data [:panels b :name])))]
                                  [f-path screen-name (str b) block-name (count (keys views)) (count (keys queries))
                                   (try (str (cstr/join " " (keys views))) (catch Exception _ ""))
                                   (try (str (cstr/join " " (keys queries))) (catch Exception _ ""))
                                   (cond theme? (str theme-map)
                                         board? (pr-str {:panels (get board-map b) :click-param {:param (get params :param)}})
                                         :else  (str (get-in screen-data [:panels b])))
                                   (str (get-in screen-data [:panels b :tab]))]))}))))

(defn update-all-screen-meta
  []
  (sql-exec system-db (to-sql {:delete-from [:screens]})) ;; just in case
  (let [prefix "./screens/"
        files  (ut/get-file-vectors-simple prefix ".edn")]
    (doseq [f files] (update-screen-meta (str prefix f)))))

(defn update-flow-meta
  [f-path]
  (try (let [screen-str (slurp (str f-path))
             flow-data  (edn/read-string screen-str)
             flow-id    (get flow-data :flow-id "unnamed-flow!")
             insert-sql {:insert-into [:flows]
                         :values      [{:flow_id       (str flow-id)
                                        :components    (count (get flow-data :components))
                                        :last_modified (get-last-modified-time f-path)
                                        :connections   (count (get flow-data :connections))
                                        :file_path     (str f-path)
                                        :body          (pr-str flow-data)}]}]
         (sql-exec flows-db (to-sql {:delete-from [:flows] :where [:= :file_path f-path]}))
         (sql-exec flows-db (to-sql insert-sql)))
       (catch Exception e (ut/pp [:read-flow-error f-path e]))))

(defn update-all-flow-meta
  []
  (sql-exec flows-db (to-sql {:delete-from [:flows]})) ;; just in case
  (let [prefix "./flows/"
        files  (ut/get-file-vectors-simple prefix ".edn")]
    (doseq [f files]
      (let [f-path (str prefix f)]
        (wss/create-flowblock f-path)
        (update-flow-meta f-path)))))

(defn refresh-flow-fn
  [flow-functions-map]
  (doseq [{:keys [category name description file-path inputs icon types]} (ut/flatten-map flow-functions-map)]
    (wss/enqueue-task3 ;; since out base sqlite db hate concurrency, with a "real" db, this can
      (fn []
        (let [connection_id "system-db"]
          (sql-exec system-db (to-sql {:delete-from [:flow_functions] :where [:and [:= :name name] [:= :category category]]}))
          (sql-exec system-db
                    (to-sql {:insert-into [:flow_functions]
                             :columns     [:connection_id :run_id :category :name :full_map :description :inputs :icon
                                           :input_types :output_types :file_path]
                             :values      [[connection_id 0 ;;this-run-id
                                            (str category) (str name) (str (get-in flow-functions-map [category name])) ;; full
                                            (str description) (str inputs) (str icon) (str (vec (vals (dissoc types :out))))
                                            (str (get types :out)) (str file-path)]]})))))))

(defn watch-flows-folder
  []
  (let [file-path "./flows/"]
    (beholder/watch #(when (cstr/ends-with? (str (get % :path)) ".edn")
                       (let [f-path (str (get % :path))
                             f-op   (get % :type)
                             ffn    (wss/create-flowblock f-path)]
                         (refresh-flow-fn {:sub-flow ffn}) ;; change the sql for the sub-flow block
                         (ut/pp [:flow-change! f-op f-path])
                         (update-flow-meta f-path)))
                    file-path)))

(defn watch-screens-folder
  []
  (let [file-path "./screens/"]
    (beholder/watch #(when (cstr/ends-with? (str (get % :path)) ".edn")
                       (let [f-path (str (get % :path))
                             f-op   (get % :type)]
                         (ut/pp [:screen-change! f-op f-path])
                         (update-screen-meta f-path)))
                    file-path)))

(defn update-all-conn-meta
  []
  (let [prefix "connections/"
        files  (ut/get-file-vectors-simple prefix ".edn")]
    (doseq [f    files
            :let [f-path    (str prefix f)
                  conn-name (str (cstr/replace (cstr/lower-case (last (cstr/split f-path #"/"))) ".edn" ""))
                  conn      (edn/read-string (slurp f-path))
                  poolable? (try (and (map? conn) (find conn :jdbc-url)) (catch Exception _ false))
                  conn      (if poolable?
                              (try {:datasource @(pool-create conn (str conn-name "-pool"))}
                                   (catch Exception e (do (ut/pp [:connection-error conn-name e]) {:datasource nil})))
                              conn)]]
      (try ;; connection errors, etc
        (do (when poolable? (swap! wss/conn-map assoc conn-name conn))
            (when harvest-on-boot?
              (cruiser/lets-give-it-a-whirl-no-viz f-path
                                                   conn
                                                   system-db
                                                   cruiser/default-sniff-tests
                                                   cruiser/default-field-attributes
                                                   cruiser/default-derived-fields
                                                   cruiser/default-viz-shapes)))
        (catch Exception e (do (swap! wss/conn-map dissoc conn-name) (ut/pp [:sniff-error conn-name e])))))))

(defn watch-connections-folder
  []
  (let [file-path "./connections/"]
    (beholder/watch
      #(cond (cstr/ends-with? (str (get % :path)) ".edn") (let [f-path    (str (get % :path))
                                                                f-op      (get % :type)
                                                                conn      (edn/read-string (slurp f-path))
                                                                conn-name (str (cstr/replace (cstr/lower-case
                                                                                               (last (cstr/split f-path #"/")))
                                                                                             ".edn"
                                                                                             ""))
                                                                conn      (if (try (and (map? conn) (find conn :jdbc-url))
                                                                                   (catch Exception _ false))
                                                                            {:datasource @(pool-create conn
                                                                                                       (str conn-name "-pool"))}
                                                                            conn)]
                                                            (ut/pp [:connection-change! f-op f-path])
                                                            (swap! wss/conn-map assoc conn-name conn)
                                                            (cruiser/lets-give-it-a-whirl-no-viz f-path
                                                                                                 conn ;f-data
                                                                                                 system-db
                                                                                                 cruiser/default-sniff-tests
                                                                                                 cruiser/default-field-attributes
                                                                                                 cruiser/default-derived-fields
                                                                                                 cruiser/default-viz-shapes))
             (cstr/ends-with? (str (get % :path)) ".csv") (let [f-path (str (get % :path))
                                                                f-op   (get % :type)]
                                                            (ut/pp [:csv-file-change! f-op f-path])
                                                            (wss/process-csv {} f-path)))
      file-path)))

(defn thaw-flow-results
  []
  (ut/pp [:thawing-flow-results-atom...])
  (let [file-path "./data/atoms/flow-db-results-atom.edn"
        file      (io/file file-path)
        state     (if (.exists file)
                    (with-open [rdr (io/reader file)]
                      (try (edn/read (java.io.PushbackReader. rdr))
                           (catch Exception e (do (ut/pp [:flow-results-thaw-atom-error!!!! file e]) (System/exit 0)))))
                    {})]
    (reset! flow-db/results-atom state)))


(defn start-services
  []
  (ut/pp [:starting-services...])
  (evl/create-nrepl-server!) ;; needs to start
  (wss/reload-signals-subs) ;; run once on boot
  (wss/reload-solver-subs) ;; run once on boot (needs nrepl(s) to start first...)
  (wss/create-web-server!)
  (let [flows (vec (filter #(not (cstr/includes? (str %) "-solver-flow-")) (keys @flow-db/results-atom)))]
    (reset! flow-db/results-atom (select-keys (ut/replace-large-base64 @flow-db/results-atom) flows))))

(defn delayed-start [ms f] (Thread. #(do (Thread/sleep ms) (f))))


(defn delay-execution
  [ms f]
  (let [thread (Thread. (fn [] (Thread/sleep ms) (f)))]
    (.start thread)
    thread))

(defn -main
  [& args]
  (ut/print-ansi-art "rrvbbit.ans")
  (thaw-flow-results)
  ;(sql/start-worker-sql)
  (wss/recycle-workers-sql-meta 5) ;; sql meta cnts
  #_{:clj-kondo/ignore [:inline-def]}
  (defonce start-conn-watcher (watch-connections-folder))
  #_{:clj-kondo/ignore [:inline-def]}
  (defonce start-screen-watcher (watch-screens-folder))
  #_{:clj-kondo/ignore [:inline-def]}
  (defonce start-flow-watcher (watch-flows-folder))
  (shutdown/add-hook! ::heads-up-msg #(ut/ppa "Shutting down now, commander!"))
  (sql-exec system-db "drop table if exists jvm_stats;")
  (sql-exec system-db "drop table if exists errors;")
  (sql-exec system-db "drop view  if exists viz_recos_vw;")
  (sql-exec system-db "drop view  if exists viz_recos_vw2;")
  (sql-exec system-db "drop view  if exists user_subs_vw;")
  (sql-exec system-db "drop view  if exists latest_status;")
  (cruiser/create-sqlite-sys-tables-if-needed! system-db)
  (cruiser/create-sqlite-flow-sys-tables-if-needed! flows-db)
  (add-watch wss/screens-atom
             :master-screen-watcher ;; watcher splitter
             (fn [_ _ old-state new-state]
               (doseq [key (keys new-state)]
                 (if-let [child-atom (get @wss/screen-child-atoms key)]
                   (swap! child-atom assoc key (get new-state key))
                   (let [new-child-atom (atom {})]
                     (swap! wss/screen-child-atoms assoc key new-child-atom)
                     (swap! new-child-atom assoc key (get new-state key)))))))
  (add-watch wss/params-atom
             :master-params-watcher ;; watcher splitter
             (fn [_ _ old-state new-state]
               (doseq [key (keys new-state)]
                 (if-let [child-atom (get @wss/param-child-atoms key)]
                   (swap! child-atom assoc key (get new-state key))
                   (let [new-child-atom (atom {})]
                     (swap! wss/param-child-atoms assoc key new-child-atom)
                     (swap! new-child-atom assoc key (get new-state key)))))))
  (add-watch wss/panels-atom
             :master-panels-watcher ;; watcher splitter
             (fn [_ _ old-state new-state]
               (doseq [key (keys new-state)]
                 (if-let [child-atom (get @wss/panel-child-atoms key)]
                   (swap! child-atom assoc key (get new-state key))
                   (let [new-child-atom (atom {})]
                     (swap! wss/panel-child-atoms assoc key new-child-atom)
                     (swap! new-child-atom assoc key (get new-state key)))))))
  (add-watch
    wss/signals-atom
    :master-signal-def-watcher ;; watcher signals defs
    (fn [_ _ old-state new-state] (ut/pp [:signals-defs-changed :reloading-signals-sys-subs....]) (wss/reload-signals-subs)))
  (add-watch wss/rules-atom :master-rule-def-watcher (fn [_ _ old-state new-state] (ut/pp [:rules-defs-changed :reloading....])))
  (add-watch wss/solvers-atom
             :master-solver-def-watcher
             (fn [_ _ old-state new-state] (ut/pp [:solvers-defs-changed :reloading....]) (wss/reload-solver-subs)))
  (update-all-screen-meta)
  (update-all-flow-meta)
  (cruiser/insert-current-rules!
    system-db
    "system-db"
    0
    cruiser/default-sniff-tests
    cruiser/default-field-attributes
    cruiser/default-derived-fields
    cruiser/default-viz-shapes
    (merge cruiser/default-flow-functions {:custom @wss/custom-flow-blocks} {:sub-flows @wss/sub-flow-blocks}))
  (when harvest-on-boot? (update-all-conn-meta))
  (cruiser/lets-give-it-a-whirl-no-viz ;;; force system-db as a conn, takes a sec
    "system-db"
    system-db
    system-db
    cruiser/default-sniff-tests
    cruiser/default-field-attributes
    cruiser/default-derived-fields
    cruiser/default-viz-shapes)
  (cruiser/lets-give-it-a-whirl-no-viz ;;; force flows-db as a conn, takes a sec
    "flows-db"
    flows-db
    system-db
    cruiser/default-sniff-tests
    cruiser/default-field-attributes
    cruiser/default-derived-fields
    cruiser/default-viz-shapes)
  (cruiser/lets-give-it-a-whirl-no-viz ;;; force cache-db as a conn, takes a sec
    "cache.db"
    wss/cache-db
    system-db
    cruiser/default-sniff-tests
    cruiser/default-field-attributes
    cruiser/default-derived-fields
    cruiser/default-viz-shapes)
  (shell/sh "/bin/bash" "-c" (str "rm -rf " "live/*"))
  (tt/start!)
  (def mon (tt/every! 15 5 (bound-fn [] (wss/jvm-stats)))) ;; update stats table every 30
  (wss/recycle-worker) ;; single blocking
  (wss/recycle-worker2) ;; single blocking
  (wss/recycle-worker3) ;; single blocking
  (wss/recycle-workers4 45) ;; run-solver only
  (wss/recycle-workers5 8) ;; push-to-client only (not used at the moment, we have a per-client
                           ;; blocking queue setup)
  (wss/recycle-workers5d 2) ;; for writing transit files

  (def param-sync (tt/every! 30 2 (bound-fn [] (wss/param-sql-sync))))
  (def refresh-flow-tables (tt/every! 5 2 (bound-fn [] (wss/flow-atoms>sql)))) ;; was 30. 5 too
  (def purge (tt/every! wss/jvm-stats-every 2 (bound-fn [] (wss/purge-dead-client-watchers))))
  (def timekeeper (tt/every! 1 3 (bound-fn [] (reset! wss/time-atom (ut/current-datetime-parts)))))
  (def last-look (atom {}))
  (def saved-uids (atom []))
  (let [signals  (vec (keys @wss/signals-atom))
        signalsv (vec (apply concat
                        (for [[k v] @wss/signals-atom]
                          (for [e (filter #(and (keyword? %) (or (some (fn [x] (= x %)) signals) (cstr/includes? (str %) "/")))
                                    (ut/deep-flatten (get v :signal)))]
                            [e (keyword (str "signal/" (cstr/replace (str k) ":" "")))]))))
        solvers  (vec (for [[k v] @wss/solvers-atom
                            :when (keyword? (get v :signal))]
                        [(get v :signal) (keyword (str "solver/" (cstr/replace (str k) ":" "")))]))
        conns    (vec (distinct (into signalsv solvers)))
        fmap     (wss/warren-flow-map conns)]
    (ut/pretty-spit "./flows/generated-flow-map.edn" fmap)
    (ut/pp [:warren-flow? conns]))
  (shutdown/add-hook! ::the-pool-is-now-closing
                      #(do (reset! wss/shutting-down? true)
                           ;;  (let [destinations (vec (keys @wss/client-queues))]
                           ;;    (doseq [d destinations]
                           ;;      (wss/alert! d
                           ;;                  [:v-box :justify :center :style {:opacity 0.7} :children
                           ;;                   [[:box :style {:color :theme/editor-outer-rim-color :font-weight 700}
                           ;;                     :child
                           ;;                     [:box :child (str "Heads up: R-V-B-B-I-T system going offline.")]]]]
                           ;;                  10 1
                           ;;                  5)))
                           ;;  (Thread/sleep 2000)
                           (wss/destroy-websocket-server!)
                           (tt/stop!)
                           (wss/stop-worker)
                           (wss/stop-worker2)
                           (wss/stop-worker3)
                           (wss/stop-workers4)
                           (wss/stop-workers5)
                           (Thread/sleep 2000)
                           (wss/destroy-websocket-server!)))
  (shutdown/add-hook! ::the-pool-is-now-closed
                      #(doseq [[conn-name conn] @wss/conn-map]
                         (do (ut/ppa [:shutting-down-connection-pool conn-name conn])
                             (try (hik/close-datasource (get conn :datasource)) (catch Exception e (ut/pp [:close-error e]))))))
  (shutdown/add-hook! ::close-system-pools
                      #(do (wss/destroy-websocket-server!)
                           (Thread/sleep 1000)
                           (wss/stop-web-server!)
                           (wss/stop-worker)
                           (wss/stop-worker2)
                           (wss/stop-worker3)
                           (ut/ppa [:shutting-down-system-pools])
                           (hik/close-datasource (get system-db :datasource))
                           (wss/destroy-websocket-server!)))
  (shutdown/add-hook! ::clear-cache
                      #(do (ut/ppa [:freezing-system-atoms])
                           (ut/freeze-atoms)
                           (ut/ppa [:freezing-flow-results-atom])
                           (with-open [wtr (io/writer "./data/atoms/flow-db-results-atom.edn")]
                             (binding [*out* wtr] (prn @flow-db/results-atom)))
                           (ut/ppa [:clearing-cache-db])
                           ;(shell/sh "/bin/bash" "-c" (str "rm " "db/cache.db"))
                           (shell/sh "/bin/bash" "-c" (str "rm " "flow-logs/*"))
                           (shell/sh "/bin/bash" "-c" (str "rm " "reaction-logs/*"))
                           (shell/sh "/bin/bash" "-c" (str "rm " "status-change-logs/*"))
                           (shell/sh "/bin/bash" "-c" (str "rm " "tracker-logs/*"))
                           (shell/sh "/bin/bash" "-c" (str "rm " "reaction-logs/*"))
                           (shell/sh "/bin/bash" "-c" (str "rm " "db/system.db"))))
  (defn update-stat-atom
    [kks]
    (let [;flows (if (or (nil? flows) (empty? flows))
         ]
      (swap! wss/flow-status merge
        (into
          {}
          (for [k     kks ;(keys @flow-db/results-atom)
                :when (not (= k "client-keepalive"))] ;; dont want to track heartbeats
            (let [;; _ (ut/pp [:flow-status-atom-all! k (get @flow-db/results-atom k)
                  cc              (into {} (for [[k v] @flow-db/results-atom] {k (count v)})) ;; wtf?
                  blocks          (get-in @flow-db/working-data [k :components-list])
                  running-blocks  (vec (for [[k v] (get @flow-db/tracker k) :when (nil? (get v :end))] k))
                  done-blocks     (vec (for [[k v] (get @flow-db/tracker k) :when (not (nil? (get v :end)))] k))
                  not-started-yet (vec (cset/difference (set blocks) (set (into running-blocks done-blocks))))
                  running-blocks  (vec (cset/difference (set blocks) (set (into done-blocks not-started-yet))))
                  res             (get-in @flow-db/results-atom [k :done] :nope)
                  running?        (or (= res :nope) (ut/chan? res) (= res :skip))
                  done?           (not (= res :nope))
                  error?          (try (some #(or (= % :timeout) (= % :error)) (ut/deep-flatten res)) (catch Exception _ false))
                  _ (when error? (swap! wss/temp-error-blocks assoc k not-started-yet))
                  start           (try (apply min (or (for [[_ v] (get @flow-db/tracker k)] (get v :start)) [-1]))
                                       (catch Exception _ (System/currentTimeMillis)))
                  start-ts        (ut/millis-to-date-string start)
                  end             (try (apply max (or (remove nil? (for [[_ v] (get @flow-db/tracker k)] (get v :end))) [-1]))
                                       (catch Exception e
                                         (do ;(ut/pp [:exception-in-getting-time-duration!! k
                                           (System/currentTimeMillis))))
                  start           (if error? (get-in @wss/last-times [k :start]) start)
                  end             (if error? (System/currentTimeMillis) end)
                  elapsed         (try (- end start)
                                       (catch Throwable e (do (ut/pp [:elapsed-error?? k e :start start :end end]) 0)))
                  _ (swap! wss/last-times assoc-in [k :end] (System/currentTimeMillis))
                  human-elapsed   (ut/format-duration start end)
                  run-id          (str (get-in @flow-db/results-atom [k :run-id]))
                  parent-run-id   (str (get-in @flow-db/results-atom [k :parent-run-id]))
                  overrides       (get-in @flow-db/subflow-overrides [k run-id])
                  cname           (get-in @flow-db/results-atom
                                          [k :opts-map :client-name]
                                          (get @wss/orig-caller k :rvbbit-scheduler))
                  client-name     (str cname)
                  run-sql?        (and done? (not (some #(= % run-id) @saved-uids)))
                  _ (when (and done? (not error?))
                      (swap! wss/times-atom assoc k (conj (get @wss/times-atom k []) (int (/ elapsed 1000)))))
                  _ (when run-sql?
                      (try (let [row        {:client_name     client-name
                                             :flow_id         (str k)
                                             :started         start ;(get-in @flow-db/results-atom
                                             :start_ts        start-ts
                                             :ended           end ;(get-in @flow-db/results-atom
                                             :run_id          run-id
                                             :parent-run_id   parent-run-id
                                             :elapsed         elapsed
                                             :overrides       (pr-str overrides)
                                             :elapsed_seconds (float (/ elapsed 1000))
                                             :in_error        (cstr/includes? (str res) ":error")
                                             :human_elapsed   (str human-elapsed)}
                                 insert-sql {:insert-into [:flow_history] :values [row]}]
                             ;(wss/enqueue-task3 ;; temp remove to test some shit 3/9/24
                             ;  (fn []
                             (sql-exec flows-db (to-sql insert-sql)) ;; sql-exec has its own queue now
                             ;    ))
                           )
                           (catch Exception e (ut/pp [:flow-history-insert-error k (str e)]))))
                  _ (swap! last-look assoc k done?)
                  _ (when run-sql? (swap! saved-uids conj run-id))
                  chans-open      (count (doall (map (fn [[_ ch]]
                                                       (let [vv (try (not (ut/channel-open? ch)) (catch Throwable e (str e)))]
                                                         (if (cstr/includes? (str vv) "put nil on channel") :open vv)))
                                                  (get @flow-db/channels-atom k))))
                  channels-open?  (true? (> chans-open 0))]
              {k {:*done?          done? ;(true? (not (nil? res)))
                  :*started-by     cname
                  :*channels-open? channels-open?
                  :*channels-open  chans-open
                  :running-blocks  running-blocks
                  :done-blocks     done-blocks
                  :waiting-blocks  not-started-yet
                  :error-blocks    (get @wss/temp-error-blocks k [])
                  :overrides       overrides
                  :started         start
                  :*finished       (count done-blocks)
                  :*running        running-blocks
                  :*done           done-blocks
                  :*ms-elapsed     elapsed
                  :*time-running   (str human-elapsed)
                  :*not-started    not-started-yet
                  :*open-channels  (get cc k)
                  :*running?       running?}}))))))
  (defn tracker-changed
    [key ref old-state new-state]
    (let [[_ b _] (data/diff old-state new-state)
          kks     (try (keys b) (catch Exception _ nil))]
      (when (> (count (remove #(= "client-keepalive" %) kks)) 0) (update-stat-atom kks))))
  (add-watch flow-db/status :tracker-watch tracker-changed)
  (defn log-tracker
    [kks]
    (doseq [flow-id kks]
      (let [orig-tracker (get @flow-db/tracker flow-id)
            tracker      (into {}
                               (for [[k v] orig-tracker ;; remove condi non-starts. dont send
                                     :when (not (get v :in-chan?))]
                                 {k v}))]
        (swap! wss/tracker-history assoc flow-id (vec (conj (get @wss/tracker-history flow-id []) tracker))) ;; for
      )))
  (defn tracker-changed2
    [key ref old-state new-state]
    (let [[_ b _] (data/diff old-state new-state)
          kks     (try (keys b) (catch Exception _ nil))]
      (when (> (count (remove #(= "client-keepalive" %) kks)) 0)
        (async/thread ;; really expensive logging below. temp
          (let [;kks-string (-> (cstr/join "-" kks) (cstr/replace " " "_") (cstr/replace "/"
               ]
            (ext/create-dirs "./flow-logs/")
            (doseq [flow-id kks
                    :let    [run-id     (str (get-in @flow-db/results-atom [flow-id :run-id]))
                             fp         (str "./flow-logs/" (cstr/replace flow-id "/" "-CALLING-") "--" run-id ".edn")
                             mst        (System/currentTimeMillis)
                             _ (swap! wss/watchdog-atom assoc flow-id mst)
                             _ (swap! wss/watchdog-atom assoc flow-id mst)
                             diffy      (get (ut/replace-large-base64 b) flow-id)
                             diffy-keys (into {} (for [[k v] diffy] {k (first (keys v))}))
                             _ (swap! wss/last-block-written assoc flow-id diffy-keys)
                             blocks     (keys (get (ut/replace-large-base64 b) flow-id))]]
              (let [data        [[(ut/millis-to-date-string mst) blocks]
                                 {:values (select-keys (ut/replace-large-base64 (get-in @flow-db/results-atom [flow-id])) blocks)
                                  :diff   diffy}]
                    pretty-data (with-out-str (ppt/pprint data))] ; Use clojure.pprint/pprint
                (spit fp (str pretty-data "\n") :append true)))))
        (log-tracker kks)
        (update-stat-atom kks))))
  (add-watch flow-db/tracker :tracker-watch tracker-changed2)
  (ut/pp {:settings config/settings})
  (defn heartbeat
    [dest]
    {:components
       {:kick-1            {:inputs [:destination :name :sub-task :thread-id :thread-desc :message-name]
                            :fn     '(fn
                                      [destination name sub-task thread-id thread-desc message-name & args]
                                      (rvbbit-backend.websockets/kick
                                       destination
                                       name
                                       sub-task
                                       thread-id
                                       thread-desc
                                       message-name
                                       args))
                            :raw-fn '(fn
                                      [destination name sub-task thread-id thread-desc message-name & args]
                                      (rvbbit-backend.websockets/kick
                                       destination
                                       name
                                       sub-task
                                       thread-id
                                       thread-desc
                                       message-name
                                       args))}
        :open-input        :heartbeat
        :kick-1destination dest}
     :connections [[:kick-1 :done] [:open-input :kick-1/name] [:open-input :kick-1/sub-task] [:open-input :kick-1/thread-id]
                   [:open-input :kick-1/thread-desc] [:open-input :kick-1/message-name]
                   [:kick-1destination :kick-1/destination]]})
  (wss/schedule! [:seconds 15]
                 (heartbeat :all)
                 {:flow-id "client-keepalive" :increment-id? false :close-on-done? true :debug? false})
  (wss/schedule! [:minutes 20]
                 "game-of-life-test1"
                 {:close-on-done? true
                  :increment-id?  false
                  :flow-id        "game-of-life-test1"
                  :debug?         false
                  :overrides      {:iterations-max 1000 :tick-delay-ms 800}})
  (wss/schedule! [:minutes 30] "counting-loop" {:flow-id "counting-loop" :increment-id? false :close-on-done? true :debug? false})
  (let [_ (ut/pp [:waiting-for-background-systems...])
        _ (Thread/sleep 13000) ;; 10 enough? want everything cranking before clients come
        _ (reset! wss/websocket-server (jetty/run-jetty #'wss/web-handler wss/ring-options))] ;; wut?
    (start-services)
    (let [destinations (vec (keys @wss/client-queues))]
      (doseq [d destinations]
        (wss/alert! d
                    [:v-box :justify :center :style {:opacity 0.7} :children
                     [[:box :style {:color :theme/editor-outer-rim-color :font-weight 700} :child
                       [:box :child (str "Heads up: R-V-B-B-I-T system going offline.")]]]]
                    10
                    1
                    5)))
    (ut/pp [" GO: end of main-fn "])))
