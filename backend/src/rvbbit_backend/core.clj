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
   [rvbbit-backend.pool-party :as ppy]
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
   [rvbbit-backend.queue-party  :as qp]
   [rvbbit-backend.assistants :as ass]
   ;[rvbbit-backend.reactor :as rkt]
   [rvbbit-backend.config :as config]
   [rvbbit-backend.db :as db]
   [rvbbit-backend.cruiser :as cruiser]
   [rvbbit-backend.embeddings :as em]
   [rvbbit-backend.evaluator :as evl]
   [rvbbit-backend.external :as ext]
   [rvbbit-backend.sql :as    sql
    :refer [flows-db insert-error-row! pool-create sql-exec sql-query sql-query-meta sql-query-one system-db  history-db
            to-sql]]
   [rvbbit-backend.surveyor :as surveyor]
   [rvbbit-backend.transform :as ts]
   [rvbbit-backend.util :as    ut
    :refer [ne?]]
   [rvbbit-backend.websockets :as wss]
   [shutdown.core :as shutdown]
   [taskpool.taskpool :as tp]
   [websocket-layer.core      :as wl]
   [tea-time.core :as tt]
   [websocket-layer.network :as net]) ;; enables joda jdbc time returns
  (:import
   [java.util Date]
   ;;[java.util.concurrent Executors TimeUnit]
   [java.util.concurrent                  Executors ThreadPoolExecutor SynchronousQueue TimeUnit TimeoutException ThreadPoolExecutor$CallerRunsPolicy]
   java.nio.file.Files
   java.nio.file.Paths
   java.nio.file.attribute.FileTime
   java.nio.file.LinkOption
   java.time.format.DateTimeFormatter
   java.time.ZoneId))

(def harvest-on-boot? (get (config/settings) :harvest-on-boot? true))

;; move all this gross stuff to -main
(println " ")
(ut/print-ansi-art "nname.ans")
(ut/pp [:version 0 :august 2024 "Hi."])
(println " ")

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
  ;(qp/slot-queue :update-screen-meta f-path
  (ppy/execute-in-thread-pools :update-screen-meta
                               (fn []
                                 (try
                                   (let [;file-path "./screens/"
                                         screen-data      (read-screen f-path)
                                         screen-name      (get screen-data :screen-name "unnamed-screen!")
                                         resolved-queries (get screen-data :resolved-queries)
                                         screen-data      (if (ut/ne? resolved-queries)
                                                            (assoc screen-data
                                                                   :panels (into {}
                                                                                 (for [[k v] (get screen-data :panels)]
                                                                                   (try
                                                                                     {k (assoc v :queries (select-keys resolved-queries (keys (get v :queries))))}
                                                                                     (catch Throwable e (ut/pp [:error-reading-screen-panels f-path e k v]) {}))))) ;; issue with pomp-girl2? TODO
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
                                     (swap! db/screens-atom assoc screen-name screen-data) ;; update master screen atom for
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
                                                                   (str (get-in screen-data [:panels b :tab]))]))})))
                                   (catch Throwable e
      ;(ut/pp [:update-screen-meta-error! f-path e])
                                     (println "Error in update-screen-meta:" (.getMessage e))
                                     (.printStackTrace e))))))

(defn update-all-screen-meta
  []
  (sql-exec system-db (to-sql {:delete-from [:screens]})) ;; just in case
  (let [prefix "./screens/"
        files  (ut/get-file-vectors-simple prefix ".edn")]
    (doseq [f files] (update-screen-meta (str prefix f)))))

(defn update-flow-meta
  [f-path]
  ;(qp/slot-queue :update-flow-meta f-path
  (ppy/execute-in-thread-pools :update-flow-meta
                               (fn []
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
                                      (catch Exception e (ut/pp [:read-flow-error f-path e]))))))

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
    ;(wss/enqueue-task3 ;; since out base sqlite db hate concurrency, with a "real" db, this can
    (qp/serial-slot-queue :general-serial :general
    ;(ppy/execute-in-thread-pools :general-serial
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
              ;(qp/slot-queue :schema-sniff f
              (ppy/execute-in-thread-pools :conn-schema-sniff
                                           (fn []
                                             (cruiser/lets-give-it-a-whirl-no-viz f-path
                                                                                  conn
                                                                                  system-db
                                                                                  cruiser/default-sniff-tests
                                                                                  cruiser/default-field-attributes
                                                                                  cruiser/default-derived-fields
                                                                                  cruiser/default-viz-shapes)))))
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


(defn watch-config-files []
  (let [file-path "./defs/"]
    (beholder/watch #(when (or (cstr/ends-with? (str (get % :path)) "config.edn")
                               (cstr/ends-with? (str (get % :path)) "clover-templates.edn"))
                       (let [destinations (vec (keys @wss/client-queues))]
                         (doseq [d destinations]
                           (wss/alert! d
                                       [:v-box :justify :center :style {:opacity 0.7} :children
                                        [[:box :style {:color :theme/editor-outer-rim-color :font-weight 700}
                                          :child (str "Note: Server config has been updated & received")]
                                         [:box :child (str (get % :path))]]]
                                       13 2
                                       5)
                           (wss/kick d [:settings] (wss/package-settings-for-client) 1 :none (str "file updated " (get % :path))))))
                    file-path)))

(defn watch-solver-files []
  (let [file-path "./defs/"]
    (beholder/watch #(when (or (cstr/ends-with? (str (get % :path)) "signals.edn")
                               (cstr/ends-with? (str (get % :path)) "solvers.edn"))
                       (let [signals? (cstr/ends-with? (str (get % :path)) "signals.edn")
                             destinations (vec (keys @wss/client-queues))
                             map-atom (if signals? wss/signals-atom wss/solvers-atom)
                             _ (reset! map-atom (edn/read-string (slurp (str (get % :path)))))
                             _ (ut/pp [:solver-file-change! signals? (get % :path)])]
                         (doseq [d destinations]
                           (wss/alert! d
                                       [:v-box :justify :center :style {:opacity 0.7} :children
                                        [[:box :style {:color :theme/editor-outer-rim-color :font-weight 700}
                                          :child (str "Note: Server " (if signals? "signals" "solvers") " have been updated & received")]
                                         [:box :child (str (get % :path))]]]
                                       13 2
                                       5)
                           (wss/kick d (if signals? :signals-file :solvers-file) @map-atom 1 :none (str "file updated " (get % :path)))
                           )))
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








;; (defn start-time-scheduler []
;;   (ut/pp [:staring-time-scheduler])
;;   (.scheduleAtFixedRate wss/time-scheduler
;;                         (reify Runnable
;;                           (run [_]
;;                             (log-and-update-atom)))
;;                         0
;;                         1
;;                         TimeUnit/SECONDS))

;; (defn stop-time-scheduler []
;;   (.shutdownNow wss/time-scheduler))

;; (defn time-marches-on-or-does-it? [key ref old-state new-state]
;;   (try
;;     (doseq [key (keys new-state)]
;;       (let [group (ut/hash-group key wss/num-groups)]
;;         (if-let [child-atom (get @wss/time-child-atoms group)]
;;           (swap! child-atom assoc key (get new-state key))
;;           (let [new-child-atom (atom {})]
;;             (swap! wss/time-child-atoms assoc group new-child-atom)
;;             (swap! new-child-atom assoc key (get new-state key))))))
;;     ;(println "Watcher updated child atoms successfully.")
;;     (catch Exception e
;;       (println "Error in time-marches-on:" key ref (.getMessage e))
;;       (throw e))))

;; (add-watch wss/father-time
;;            :time-marches-on-or-does-it?
;;            time-marches-on-or-does-it?)






(defonce scheduled-tasks (atom {}))

(defn start-scheduler [secs f name-str & [delay]]
  (ut/pp ["Starting scheduler" name-str "every" secs "seconds"])
  (let [delay (or delay 0)
        task (.scheduleAtFixedRate ppy/general-scheduler-thread-pool
                                   (reify Runnable
                                     (run [_]
                                       (try
                                         (f)
                                         (swap! wss/scheduler-atom assoc name-str (merge
                                                                                   {:last-run (ut/get-current-timestamp)
                                                                                    :times-run (inc (get-in @wss/scheduler-atom [name-str :times-run] 0))}
                                                                                   (dissoc (get @wss/scheduler-atom name-str) :last-run :times-run)))
                                         (catch Exception e
                                           (ut/pp [e "Error in scheduler" name-str])))))
                                   delay
                                   secs
                                   TimeUnit/SECONDS)]
    (swap! scheduled-tasks assoc name-str task)))

(defn stop-all-schedulers []
  (ut/pp ["Stopping all schedulers"])
  (doseq [[name task] @scheduled-tasks]
    (ut/pp ["Stopping scheduler" name])
    (.cancel task true))
  (reset! scheduled-tasks {}))



(defn -main
  [& args]
  (ut/print-ansi-art "rrvbbit.ans")
  (qp/create-slot-queue-system)
  (thaw-flow-results)
  ;(sql/start-worker-sql)
  ;(wss/recycle-workers-sql-meta 5) ;; sql meta cnts
  #_{:clj-kondo/ignore [:inline-def]}
  (defonce start-conn-watcher (watch-connections-folder))
  #_{:clj-kondo/ignore [:inline-def]}
  (defonce start-screen-watcher (watch-screens-folder))
  #_{:clj-kondo/ignore [:inline-def]}
  (defonce start-flow-watcher (watch-flows-folder))
  #_{:clj-kondo/ignore [:inline-def]}
  (defonce start-settings-watcher (watch-config-files))
#_{:clj-kondo/ignore [:inline-def]}
  (defonce start-solver-watcher (watch-solver-files))

  (shutdown/add-hook! ::heads-up-msg #(ut/ppa "Shutting down now, commander!"))
  (sql-exec system-db "drop table if exists jvm_stats;")
  (sql-exec system-db "drop table if exists errors;")
  (sql-exec system-db "drop view  if exists viz_recos_vw;")
  (sql-exec system-db "drop view  if exists viz_recos_vw2;")
  (sql-exec system-db "drop view  if exists user_subs_vw;")
  (sql-exec system-db "drop view  if exists latest_status;")

  (cruiser/create-sqlite-sys-tables-if-needed! system-db)
  (cruiser/create-sqlite-flow-sys-tables-if-needed! flows-db)

  ;; temp test indexes
  (ut/pp (sql-exec system-db "CREATE INDEX idx_client_name ON client_memory(client_name);"))
  ;; (ut/pp (sql-exec system-db "CREATE INDEX idx_ts ON client_memory(ts);"))
  ;; (ut/pp (sql-exec system-db "CREATE INDEX idx_client_name2 ON client_memory(client_name, ts);"))
  ;; (ut/pp (sql-exec system-db "CREATE INDEX idx_ts1 ON jvm_stats(ts);"))




;; lazy atom sharding watcher experiment - 7/8/24
  ;; (rkt/dynamic-rabbit-reactor
  ;;  rkt/shard-maps
  ;;  wss/master-reactor-atoms
  ;;  :rvbbit-reactor1
  ;;  rkt/tracking-atom)






  ;; (ppy/add-watch+ wss/father-time
  ;;                 :master-time-watcher
  ;;                 (fn [_ _ old-state new-state]
  ;;                   (when (not= old-state new-state)
  ;;                     (let [changes (reduce-kv
  ;;                                    (fn [m k v]
  ;;                                      (if (not= v (get old-state k))
  ;;                                        (assoc m k v)
  ;;                                        m))
  ;;                                    {}
  ;;                                    new-state)]
  ;;                       (doseq [[k v] changes]
  ;;                         (swap! wss/time-child-atoms
  ;;                                (fn [child-atoms]
  ;;                                  (if-let [child-atom (get child-atoms k)]
  ;;                                    (do (swap! child-atom assoc k v)
  ;;                                        child-atoms)
  ;;                                    (let [new-child-atom (atom {k v})]
  ;;                                      (assoc child-atoms k new-child-atom)))))))))
  ;;                 :master-time-watcher)



  ;; (doseq [args wss/master-atom-map]
  ;;   (apply wss/master-watch-splitter-2deep args))


  ;; boot master watchers for defined atoms 
  (doseq [[type atom] db/master-reactor-atoms]
    ;(swap! wss/sharded-atoms assoc type (atom {})) ;; bootstrap the child atom shard
    ;(swap! wss/sharded-atoms (fn [current-state] (assoc current-state type (atom {}))))
    (db/master-watch-splitter-deep
     ;;(keyword (str "watcher-" (name type)))
     type
     atom))

  ;; (ppy/add-watch+ wss/screens-atom
  ;;                 :master-screen-watcher
  ;;                 (fn [_ _ old-state new-state]
  ;;                   (when (not= old-state new-state)
  ;;                     (let [changes (reduce-kv
  ;;                                    (fn [m k v]
  ;;                                      (if (not= v (get old-state k))
  ;;                                        (assoc m k v)
  ;;                                        m))
  ;;                                    {}
  ;;                                    new-state)]
  ;;                       (doseq [[k v] changes]
  ;;                         (swap! wss/screen-child-atoms
  ;;                                (fn [child-atoms]
  ;;                                  (if-let [child-atom (get child-atoms k)]
  ;;                                    (do (swap! child-atom assoc k v)
  ;;                                        child-atoms)
  ;;                                    (let [new-child-atom (atom {k v})]
  ;;                                      (assoc child-atoms k new-child-atom)))))))))
  ;;                 :master-screen-watcher)

  ;; (ppy/add-watch+ wss/params-atom
  ;;                 :master-params-watcher
  ;;                 (fn [_ _ old-state new-state]
  ;;                   (when (not= old-state new-state)
  ;;                     (let [changes (reduce-kv
  ;;                                    (fn [m k v]
  ;;                                      (if (not= v (get old-state k))
  ;;                                        (assoc m k v)
  ;;                                        m))
  ;;                                    {}
  ;;                                    new-state)]
  ;;                       (doseq [[k v] changes]
  ;;                         (swap! wss/param-child-atoms
  ;;                                (fn [child-atoms]
  ;;                                  (if-let [child-atom (get child-atoms k)]
  ;;                                    (do (swap! child-atom assoc k v)
  ;;                                        child-atoms)
  ;;                                    (let [new-child-atom (atom {k v})]
  ;;                                      (assoc child-atoms k new-child-atom)))))))))
  ;;                 :master-params-watcher)

  ;; (ppy/add-watch+ wss/panels-atom
  ;;                 :master-panels-watcher
  ;;                 (fn [_ _ old-state new-state]
  ;;                   (when (not= old-state new-state)
  ;;                     (let [changes (reduce-kv
  ;;                                    (fn [m k v]
  ;;                                      (if (not= v (get old-state k))
  ;;                                        (assoc m k v)
  ;;                                        m))
  ;;                                    {}
  ;;                                    new-state)]
  ;;                       (doseq [[k v] changes]
  ;;                         (swap! wss/panel-child-atoms
  ;;                                (fn [child-atoms]
  ;;                                  (if-let [child-atom (get child-atoms k)]
  ;;                                    (do (swap! child-atom assoc k v)
  ;;                                        child-atoms)
  ;;                                    (let [new-child-atom (atom {k v})]
  ;;                                      (assoc child-atoms k new-child-atom)))))))))
  ;;                 :master-panels-watcher)

  ;; (ppy/add-watch+ flow-db/results-atom
  ;;                 :master-flow-watcher
  ;;                 (fn [_ _ old-state new-state]
  ;;                   (when (not= old-state new-state)
  ;;                     (let [changes (reduce-kv
  ;;                                    (fn [m k v]
  ;;                                      (if (not= v (get old-state k))
  ;;                                        (assoc m k v)
  ;;                                        m))
  ;;                                    {}
  ;;                                    new-state)]
  ;;                       (doseq [[k v] changes]
  ;;                         (swap! wss/flow-child-atoms
  ;;                                (fn [child-atoms]
  ;;                                  (if-let [child-atom (get child-atoms k)]
  ;;                                    (do (swap! child-atom assoc k v)
  ;;                                        child-atoms)
  ;;                                    (let [new-child-atom (atom {k v})]
  ;;                                      (assoc child-atoms k new-child-atom)))))))))
  ;;                 :master-flow-watcher)



  ;; (ppy/add-watch+ evl/repl-introspection-atom
  ;;                 :master-nrepl-instrospection-watcher
  ;;                 (fn [_ _ old-state new-state]
  ;;                   (when (not= old-state new-state)
  ;;                     (let [changes (reduce-kv
  ;;                                    (fn [m k v]
  ;;                                      (if (not= v (get old-state k))
  ;;                                        (assoc m k v)
  ;;                                        m))
  ;;                                    {}
  ;;                                    new-state)]
  ;;                       (doseq [[k v] changes]
  ;;                         (swap! evl/repl-introspection-child-atoms
  ;;                                (fn [child-atoms]
  ;;                                  (if-let [child-atom (get child-atoms k)]
  ;;                                    (do (swap! child-atom assoc k v)
  ;;                                        child-atoms)
  ;;                                    (let [new-child-atom (atom {k v})]
  ;;                                      (assoc child-atoms k new-child-atom)))))))))
  ;;                 :master-nrepl-instrospection-watcher)


  ;; (ppy/add-watch+ wss/solver-status  ;; 2 keys deep partitioning
  ;;                 :master-solver-status-watcher
  ;;                 (fn [_ _ old-state new-state]
  ;;                   (when (not= old-state new-state)
  ;;                     (let [changes (reduce-kv
  ;;                                    (fn [m parent-key parent-value]
  ;;                                      (let [old-parent-value (get old-state parent-key)]
  ;;                                        (if (not= parent-value old-parent-value)
  ;;                                          (assoc m parent-key
  ;;                                                 (reduce-kv
  ;;                                                  (fn [child-m child-key child-value]
  ;;                                                    (if (not= child-value (get old-parent-value child-key))
  ;;                                                      (assoc child-m child-key child-value)
  ;;                                                      child-m))
  ;;                                                  {}
  ;;                                                  parent-value))
  ;;                                          m)))
  ;;                                    {}
  ;;                                    new-state)]
  ;;                       (doseq [[parent-key parent-changes] changes]
  ;;                         (doseq [[child-key child-value] parent-changes]
  ;;                           (let [composite-key [parent-key child-key]]
  ;;                             (swap! wss/solver-status-child-atoms
  ;;                                    (fn [child-atoms]
  ;;                                      (if-let [child-atom (get child-atoms composite-key)]
  ;;                                        (do
  ;;                                          (swap! child-atom update parent-key assoc child-key child-value)
  ;;                                          child-atoms)
  ;;                                        (let [new-child-atom (atom {parent-key {child-key child-value}})]
  ;;                                          (assoc child-atoms composite-key new-child-atom)))))))))))
  ;;                 :master-solver-status-watcher)



  ;; (ppy/add-watch+ wss/last-signals-atom
  ;;                 :master-signals-watcher
  ;;                 (fn [_ _ old-state new-state]
  ;;                   (when (not= old-state new-state)
  ;;                     (let [changes (reduce-kv
  ;;                                    (fn [m k v]
  ;;                                      (if (not= v (get old-state k))
  ;;                                        (assoc m k v)
  ;;                                        m))
  ;;                                    {}
  ;;                                    new-state)]
  ;;                       (doseq [[k v] changes]
  ;;                         (let [group (ut/hash-group k wss/num-groups)]
  ;;                           (swap! wss/signal-child-atoms
  ;;                                  (fn [child-atoms]
  ;;                                    (if-let [child-atom (get child-atoms group)]
  ;;                                      (do (swap! child-atom assoc k v)
  ;;                                          child-atoms)
  ;;                                      (let [new-child-atom (atom {k v})]
  ;;                                        (assoc child-atoms group new-child-atom))))))))))
  ;;                 :master-signals-watcher)


  ;; (ppy/add-watch+ wss/last-solvers-atom
  ;;                 :master-solver-watcher
  ;;                 (fn [_ _ old-state new-state]
  ;;                   (when (not= old-state new-state)
  ;;                     (let [changes (reduce-kv
  ;;                                    (fn [m k v]
  ;;                                      (if (not= v (get old-state k))
  ;;                                        (assoc m k v)
  ;;                                        m))
  ;;                                    {}
  ;;                                    new-state)]
  ;;                       (doseq [[k v] changes]
  ;;                         (swap! wss/solver-child-atoms
  ;;                                (fn [child-atoms]
  ;;                                  (if-let [child-atom (get child-atoms k)]
  ;;                                    (do (swap! child-atom assoc k v)
  ;;                                        child-atoms)
  ;;                                    (let [new-child-atom (atom {k v})]
  ;;                                      (assoc child-atoms k new-child-atom)))))))))
  ;;                 :master-solver-watcher)




  ;; (ppy/add-watch+ wss/last-solvers-atom-meta  ;; 1+2 keys deep partitioning, BUT keeping parent atoms also... further optimziations coming..
  ;;                 :master-solver-meta-watcher
  ;;                 (fn [_ _ old-state new-state]
  ;;                   (let [changes (reduce-kv
  ;;                                  (fn [m parent-key parent-value]
  ;;                                    (let [old-parent-value (get old-state parent-key)]
  ;;                                      (if (not= parent-value old-parent-value)
  ;;                                        (assoc m parent-key
  ;;                                               {:parent parent-value
  ;;                                                :children (reduce-kv
  ;;                                                           (fn [child-m child-key child-value]
  ;;                                                             (if (not= child-value (get old-parent-value child-key))
  ;;                                                               (assoc child-m child-key child-value)
  ;;                                                               child-m))
  ;;                                                           {}
  ;;                                                           parent-value)})
  ;;                                        m)))
  ;;                                  {}
  ;;                                  new-state)]
  ;;                     (doseq [[parent-key {:keys [parent children]}] changes]
  ;;         ;; Update parent-level child atom
  ;;                       (when-let [parent-child-atom (get @wss/solver-meta-child-atoms parent-key)]
  ;;                         (swap! parent-child-atom assoc parent-key parent))

  ;;         ;; Update child-level child atoms
  ;;                       (doseq [[child-key child-value] children]
  ;;                         (let [composite-key [parent-key child-key]]
  ;;                           (when-let [child-atom (get @wss/solver-meta-child-atoms composite-key)]
  ;;                             (swap! child-atom update parent-key assoc child-key child-value)))))))
  ;;                 :master-solver-meta-watcher)

  ;; (ppy/add-watch+ wss/flow-status
  ;;                 :master-flow-status-watcher
  ;;                 (fn [_ _ old-state new-state]
  ;;                   (when (not= old-state new-state)
  ;;                     (let [changes (reduce-kv
  ;;                                    (fn [m k v]
  ;;                                      (if (not= v (get old-state k))
  ;;                                        (assoc m k v)
  ;;                                        m))
  ;;                                    {}
  ;;                                    new-state)]
  ;;                       (doseq [[k v] changes]
  ;;                         (swap! wss/flow-status-child-atoms
  ;;                                (fn [child-atoms]
  ;;                                  (if-let [child-atom (get child-atoms k)]
  ;;                                    (do (swap! child-atom assoc k v)
  ;;                                        child-atoms)
  ;;                                    (let [new-child-atom (atom {k v})]
  ;;                                      (assoc child-atoms k new-child-atom)))))))))
  ;;                 :master-flow-status-watcher)





  (ppy/add-watch+ wss/signals-atom
                  :master-signal-def-watcher ;; watcher signals defs
                  (fn [_ _ old-state new-state]
                    (ut/pp [:signals-defs-changed :reloading-signals-sys-subs....])
                    (wss/reload-signals-subs))
                  :master-signal-def-watcher)

  (ppy/add-watch+ wss/rules-atom
                  :master-rule-def-watcher
                  (fn [_ _ old-state new-state]
                    (ut/pp [:rules-defs-changed :reloading....]))
                  :master-rule-def-watcher)

  (ppy/add-watch+ wss/solvers-atom
                  :master-solver-def-watcher
                  (fn [_ _ old-state new-state]
                    (ut/pp [:solvers-defs-changed :reloading....])
                    (wss/reload-solver-subs))
                  :master-solver-def-watcher)

  ;; (add-watch wss/father-time
  ;;            :fake-time-pusher
  ;;            (fn [_ _ old-state new-state]
  ;;              (try
  ;;                (let [ttt (ut/current-datetime-parts)]
  ;;                  (reset! wss/time-atom ttt)  ;; the OG
  ;;                  (reset! wss/time-atom-1 ttt)
  ;;                  (reset! wss/time-atom-2 ttt)
  ;;                  (reset! wss/time-atom-3 ttt)
  ;;                  (reset! wss/time-atom-4 ttt)
  ;;                  (reset! wss/time-atom-5 ttt)
  ;;                  (reset! wss/time-atom-6 ttt)
  ;;                  (reset! wss/time-atom-7 ttt)
  ;;                  (reset! wss/time-atom-8 ttt)
  ;;                  (reset! wss/time-atom-9 ttt)
  ;;                  (reset! wss/time-atom-10 ttt)) 
  ;;                (catch Throwable e (ut/pp [:father-time-error e])))))

  ;; (add-watch wss/father-time
  ;;            :time-marches-on-or-does-it? ;; having issues with a single atom...
  ;;            (fn [_ _ old-state new-state]
  ;;              ;(future ;; going back to blocking for now, since it add some back-pressure under heavy client load. experiment 
  ;;              (doseq [key (keys new-state)]
  ;;                (let [group (ut/hash-group key wss/num-groups)]
  ;;                  (if-let [child-atom (get @wss/time-child-atoms group)]
  ;;                    (swap! child-atom assoc key (get new-state key))
  ;;                    (let [new-child-atom (atom {})]
  ;;                      (swap! wss/time-child-atoms assoc group new-child-atom)
  ;;                      (swap! new-child-atom assoc key (get new-state key))))))));)

;; (defn update-time-atom [atom new-time]
;;   (reset! atom new-time))

;;   (add-watch fake-time
;;              :fake-time-pusher
;;              (fn [_ _ old-state new-state]
;;                (let [ttt (ut/current-datetime-parts)]
;;                  (doall (pmap #(update-time-atom % ttt) wss/time-atoms)))))

  (update-all-screen-meta) ;; has queue per screen 

  (update-all-flow-meta) ;; has queue per flow 

  (cruiser/insert-current-rules!
   system-db
   "system-db"
   0
   cruiser/default-sniff-tests
   cruiser/default-field-attributes
   cruiser/default-derived-fields
   cruiser/default-viz-shapes
   (merge cruiser/default-flow-functions {:custom @wss/custom-flow-blocks} {:sub-flows @wss/sub-flow-blocks}))

  (when harvest-on-boot?
    (update-all-conn-meta))

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

  (cruiser/lets-give-it-a-whirl-no-viz ;;; force cache-db as a conn, takes a sec
   "history-db"
   history-db
   system-db
   cruiser/default-sniff-tests
   cruiser/default-field-attributes
   cruiser/default-derived-fields
   cruiser/default-viz-shapes)

  (shell/sh "/bin/bash" "-c" (str "rm -rf " "live/*"))

  ;;(tt/start!)

  ;; (wss/subscribe-to-session-changes) ;; DISABLE BEHOLDER FOR NOW - mostly for external editing. cpu hog?


  ;(wss/recycle-worker) ;; single blocking
  ;(wss/recycle-worker2) ;; single blocking
  ;(wss/recycle-worker3) ;; single blocking
  ;(wss/recycle-workers4 45) ;; run-solver only
  ;(wss/recycle-workers5 8) ;; push-to-client only (not used at the moment, we have a per-client
  ;                         ;; blocking queue setup)
  ;(wss/recycle-workers5d 2) ;; for writing transit files

  ;; (def mon (tt/every! 15 5 (bound-fn [] (wss/jvm-stats)))) ;; update stats table every 15
  ;; (def param-sync (tt/every! 30 2 (bound-fn [] (wss/param-sql-sync))))
  ;; (def refresh-flow-tables (tt/every! 5 2 (bound-fn [] (wss/flow-atoms>sql)))) ;; was 30. 5 too
  ;; (def purge (tt/every! wss/jvm-stats-every 2 (bound-fn [] (wss/purge-dead-client-watchers))))
  ;; (def timekeeper (tt/every! 1 3 (bound-fn [] (reset! wss/time-atom (ut/current-datetime-parts)))))
  ;; (def cpukeeper  (tt/every! 1 3 (bound-fn [] (swap! wss/cpu-usage conj (ut/get-jvm-cpu-usage)))))
  ;; (def pushkeeper (tt/every! 1 3 (bound-fn [] (swap! wss/push-usage conj @wss/all-pushes))))
  ;; (def peerkeeper (tt/every! 1 3 (bound-fn [] (swap! wss/peer-usage conj (count @wl/sockets)))))
  ;; (def purge-solver-cache (tt/every! 600 600 (bound-fn [] (do (ut/pp [:CLEARING-OUT-SOLVER-CACHE! (ut/calculate-atom-size :current-size wss/solvers-cache-atom)])
  ;;                                                             (reset! wss/solvers-cache-hits-atom {})
  ;;                                                             (reset! wss/solvers-cache-atom {})))))

  ;; (def solver-statuses (tt/every! 1 3 (bound-fn [] ;; update "time runnning" in solver status atom
  ;;                                       (doseq [[client-name solvers] @wss/solver-status]
  ;;                                         (doseq [[solver-name v] solvers
  ;;                                                 :when (get v :running?)]
  ;;                                           (swap! wss/solver-status assoc-in [client-name solver-name :time-running] (ut/format-duration (get v :started) (System/currentTimeMillis))))))))

  ;; (defn logger [name edn]
  ;;   (let [dir         (str "./logs/" (str (java.time.LocalDate/now)) "/")
  ;;         _           (ext/create-dirs dir)
  ;;         fp          (str dir name ".log.edn")
  ;;         mst         (System/currentTimeMillis)
  ;;         data        [(ut/millis-to-date-string mst) edn]
  ;;         pretty-data (with-out-str (ppt/pprint data))]
  ;;     (spit fp (str pretty-data "\n") :append true)))

  ;; (defn start-scheduler [interval f task-name]
  ;;   (let [stop-ch (async/chan)]
  ;;     (ut/pp [:starting-async-job-scheduler! task-name [:every interval :ms]])
  ;;     (async/go-loop []
  ;;       (let [[_ ch] (async/alts! [(async/timeout interval) stop-ch])]
  ;;         (when-not (= ch stop-ch)
  ;;           (try
  ;;             (do
  ;;               (swap! wss/scheduler-atom assoc task-name
  ;;                      (ut/millis-to-date-string (System/currentTimeMillis)))
  ;;               (f))
  ;;             (catch Throwable e
  ;;               (ut/pp [:core.scheduler-error! task-name e])))
  ;;           (recur))))
  ;;     stop-ch))

  ;; (defn start-scheduler [interval f task-name]
  ;;   (let [running (atom true)]
  ;;     (future
  ;;       (while @running
  ;;         (try
  ;;           (Thread/sleep interval)
  ;;           (swap! wss/scheduler-atom assoc task-name
  ;;                  (ut/millis-to-date-string (System/currentTimeMillis)))
  ;;           (f)
  ;;           (catch Throwable e
  ;;             (ut/pp [:core.scheduler-error! task-name e])))))
  ;;     (fn [] (reset! running false))))

  ;; (defn start-scheduler [interval f task-name]
  ;;   (let [stop-ch (async/chan)
  ;;         error-ch (async/chan (async/sliding-buffer 10))]

  ;;     (ut/pp [:starting-async-job-scheduler! task-name [:every interval :ms]])

  ;;   ;; Error logging go-block
  ;;     (async/go-loop []
  ;;       (when-let [error (<! error-ch)]
  ;;         (ut/pp [:core.scheduler-error! task-name error])
  ;;         (recur)))

  ;;   ;; Main scheduler go-block
  ;;     (async/go-loop [last-run (System/currentTimeMillis)]
  ;;       (let [now (System/currentTimeMillis)
  ;;             wait-time (max 0 (- interval (- now last-run)))
  ;;             [_ ch] (async/alts! [(async/timeout wait-time) stop-ch])]
  ;;         (if (= ch stop-ch)
  ;;           (ut/pp ["Scheduler" task-name "stopped."])
  ;;           (do
  ;;             (try
  ;;               (swap! wss/scheduler-atom assoc task-name
  ;;                      (ut/millis-to-date-string (System/currentTimeMillis)))
  ;;               (f)
  ;;               (catch Throwable e
  ;;                 (async/>! error-ch (pr-str e))))
  ;;             (recur (System/currentTimeMillis))))))

  ;;   ;; Return a function to stop the scheduler
  ;;     #(async/close! stop-ch)))

;; (ut/pp (doseq [[name conn] @sql/client-db-pools]
;;          (qp/serial-slot-queue
;;           :re-sniff-client-sql-dbs :single
;;           (fn []
;;             (cruiser/lets-give-it-a-whirl-no-viz
;;              (cstr/replace (str name) ":" "")
;;              {:datasource conn}
;;              system-db
;;              cruiser/default-sniff-tests
;;              cruiser/default-field-attributes
;;              cruiser/default-derived-fields
;;              cruiser/default-viz-shapes)))))

;;  (qp/cleanup-inactive-queues 10)

  ;;  all schedulers

 ;;  (- (System/currentTimeMillis)  (get-in @rvbbit-client-sub-values [:unix-ms]))
 ;;  (ut/pp (wss/client-statuses))
 ;;  (ut/pp @watcher-log-atom)

  (defn reboot-reactor-and-resub []
    ;(evl/graceful-restart-nrepl-server!)
    ;(Thread/sleep 10000)
    (db/reboot-reactor!)
    ;(Thread/sleep 10000)
    (wss/reload-signals-subs)
    (wss/reload-solver-subs))

  ;;;  (reboot-reactor-and-resub)

  ;; (start-scheduler 15
  ;;                  #(let [ddiff (- (System/currentTimeMillis)  (get-in @wss/rvbbit-client-sub-values [:unix-ms]))]
  ;;                     (when
  ;;                      (> ddiff 31000)
  ;;                       (ut/pp [:WATCHDOG-REBOOTING-REACTOR-AND-SUBS! ddiff :ms :behind!])
  ;;                       (swap! wss/scheduler-atom assoc :executed! (inc (get-in @wss/scheduler-atom [:executed!] 0)))
  ;;                       (reboot-reactor-and-resub)))
  ;;                  "Watchdog - Reboot Reactor and Subs" 30)

  (start-scheduler 1
                   #(try
                      (let [ddate (ut/current-datetime-parts)
                            stamp (System/currentTimeMillis)
                            ddate (-> ddate
                                      (assoc (keyword (str "second=" (get ddate :second))) stamp)
                                      (assoc (keyword (str "minute=" (get ddate :minute))) stamp)
                                      (assoc (keyword (str "hour=" (get ddate :hour))) stamp))]
                        (reset! db/father-time ddate))
                      (catch Exception e
                        (println "Error updating atom:" (.getMessage e))
                        (throw e)))
                   "Father Time")

  (start-scheduler 15
                   wss/jvm-stats
                   "JVM Stats" 30)

  (start-scheduler 15
                   #(wss/flow-statuses true)
                   "Flow Stats & Watchdog" 15)

  (start-scheduler 240 ;; was 30
                   #(ppy/execute-in-thread-pools
                     :param-sql-sync
                     (fn [] (wss/param-sql-sync)))
                   "Parameter Sync" 30)

  ;; (start-scheduler 45
  ;;                  #(when (> (count (wss/client-subs-late-delivery 30000)) 0)
  ;;                     (wss/sync-client-subs))
  ;;                  "Sync Client Subs" 120)

  ;; (start-scheduler 30
  ;;                  #(doseq [[name conn] @sql/client-db-pools]
  ;;                     (qp/serial-slot-queue
  ;;                      :re-sniff-client-sql-dbs :single
  ;;                      (fn []
  ;;                        (cruiser/lets-give-it-a-whirl-no-viz
  ;;                         (cstr/replace (str name) ":" "")
  ;;                         {:datasource conn}
  ;;                         system-db
  ;;                         cruiser/default-sniff-tests
  ;;                         cruiser/default-field-attributes
  ;;                         cruiser/default-derived-fields
  ;;                         cruiser/default-viz-shapes))))
  ;;                  "(Re)Sniff Client SQL DBs" 30)

  ;; (start-scheduler 600 ;; sketch for little gain?
  ;;                  db/clean-up-reactor
  ;;                  "Remove unneeded watchers from Reactor" 120)

  (start-scheduler 300 ;; was 5
                   #(ppy/execute-in-thread-pools
                     :flow-atoms>sql
                     (fn [] (wss/flow-atoms>sql)))
                   "Refresh Flow Tables" 120)

  (start-scheduler 600 ;; 10 minutes
                   wss/purge-dead-client-watchers
                   "Purge Dead Clients" 720)

  (start-scheduler (* 3600 3) ;; 3 hours
                   reboot-reactor-and-resub
                   "Reboot Reactor, Subs, Pools" (* 3600 3))

  ;; (start-scheduler (* 3600 8) ;; 8 hours - caused CPU freakout... queue party / sql-meta related? todo
  ;;                  reboot-reactor-and-resub
  ;;                  "Reboot Atom Reactor & nREPL server" (* 3600 8))

  ;; (start-scheduler 6000 ;; 10 mins - was causing problems?? TODO: investigate, not critical, we barely use the queues except for sqlite writes
  ;;                  #(qp/cleanup-inactive-queues 10) ;; MINUTES
  ;;                  "Purge Idle Queues" 7200)

;; (qp/cleanup-inactive-queues 0) 

  ;;(when debug? 
  (start-scheduler 1
                   #(do (swap! wss/peer-usage conj (count @wl/sockets))
                        (swap! wss/push-usage conj @wss/all-pushes)
                        (swap! wss/solver-usage conj @wss/solvers-run)
                        (swap! wss/queue-tasks conj @qp/queue-tasks-run)
                        (swap! wss/nrepl-intros-usage conj @evl/nrepls-intros-run)
                        (swap! wss/sql-exec-usage conj @sql/sql-exec-run)
                        (swap! wss/sql-query-usage conj @sql/sql-queries-run)
                        (swap! wss/nrepl-usage conj @evl/nrepls-run)
                        (swap! wss/flow-usage conj @wss/flows-run)
                        (swap! wss/clover-params-usage conj @wss/param-sql-sync-rows)
                        (swap! wss/watcher-usage conj (count (db/current-watchers)))
                        (swap! wss/sub-usage conj (count (db/current-subs)))
                        (swap! wss/sub-client-usage conj (count (db/current-all-subs)))
                        (swap! wss/mem-usage conj (ut/memory-used))
                        (let [thread-mx-bean (java.lang.management.ManagementFactory/getThreadMXBean)
                              thread-count (.getThreadCount thread-mx-bean)]
                          (swap! wss/thread-usage conj thread-count))
                        (swap! wss/pool-task-usage conj @ppy/pool-tasks-run)
                        (swap! wss/pool-tasks conj (wss/pool-tasks-count))
                        (swap! wss/sys-load conj (ut/get-system-load-average))
                        (swap! wss/non-heap-mem-usage conj (ut/get-non-heap-memory-usage))
                        (swap! wss/time-usage conj (System/currentTimeMillis)) ;; in case we want to easily ref w/o generating
                        ;(qp/update-queue-stats-history) ;; moved to 15 second scheduler
                        ;(qp/update-specific-queue-stats-history [:watcher-to-client-serial :update-stat-atom-serial])
                        (swap! wss/cpu-usage conj (ut/get-jvm-cpu-usage)))
                   "Stats Keeper");;)

  (start-scheduler (* 3600 8) ;; 8 hours
                   #(do
                      (ut/pp [:CLEARING-OUT-DEEP-FLATTEN-CACHE]) ;; (ut/pp (ut/calculate-atom-size :current-size wss/solvers-cache-atom)) ;; super expensive on big atoms 
                      ;(ut/pp (sql-exec system-db "delete from client_memory where 1=1;"))
                      ;(ut/pp (sql-exec system-db "delete from jvm_stats where 1=1;"))
                      (reset! wss/solvers-cache-hits-atom {})
                      (reset! wss/solvers-cache-atom {})
                      ;;(reset! sql/errors {}) ;; just for now
                      (reset! wss/agg-cache {}) ;; <--- bigggger
                      (reset! ut/df-cache {}))  ;; <--- big boy
                   "Purge Solver & Deep-Flatten Cache" (* 3600 8))

  ;; (ut/calculate-atom-size :current-size ut/df-cache)

  ;; (start-scheduler 1
  ;;                  #(doseq [[client-name solvers] @wss/solver-status]
  ;;                     (doseq [[solver-name v] solvers
  ;;                             :when (get v :running?)]
  ;;                       (swap! wss/solver-status assoc-in
  ;;                              [client-name solver-name :time-running]
  ;;                              (ut/format-duration (get v :started) (System/currentTimeMillis)))))
  ;;                  "Update Solver Statuses")

  ;; (start-scheduler (* 3600 3) ;; 3 hours
  ;;                  #(do
  ;;                     (let [destinations (vec (keys @wss/client-queues))]
  ;;                       (doseq [d destinations]
  ;;                         (wss/alert! d
  ;;                                     [:v-box
  ;;                                      :justify :center
  ;;                                      :style {:opacity 0.7}
  ;;                                      :children [[:box :style {:color :theme/editor-outer-rim-color :font-weight 700}
  ;;                                                  :child [:box :child (str "Restarting Websocket Server. See you in ~45 seconds...")]]]]
  ;;                                     10 1
  ;;                                     45)))
  ;;                     (Thread/sleep 10000)
  ;;                     (wss/destroy-websocket-server!)
  ;;                     (Thread/sleep 30000)
  ;;                     (reset! wss/websocket-server (jetty/run-jetty #'wss/web-handler wss/ring-options)))
  ;;                  "Restart Websocket Server, Test Debug" (* 3600 3))

  ;; (start-scheduler 15
  ;;                  #(let [dbs (wss/database-sizes)]
  ;;                     (qp/update-queue-stats-history) ;; has it's own timestamp key
  ;;                     (doseq [[db dbv] dbs]
  ;;                       (swap! wss/sql-metrics assoc db
  ;;                              (conj (get @wss/sql-metrics db [])
  ;;                                    dbv))))
  ;;                  "Simple SQLite Db Stats" 60)


  (start-scheduler 15
                   #(let [pst (wss/query-pool-sizes)]
                      (doseq [pp (keys pst)]
                        (swap! wss/pool-stats-atom assoc pp (conj (get @wss/pool-stats-atom pp []) (get-in pst [pp 1])))))
                   "Query Pool Sizes")

  ;; (start-scheduler 1
  ;;                  #(let [total-updated (reduce ;; run side effects, but return how many are active
  ;;                                        (fn [acc [client-name solvers]]
  ;;                                          (let [updated-count
  ;;                                                (reduce-kv (fn [inner-acc solver-name v]
  ;;                                                             (if (get v :running?)
  ;;                                                               (do ;(swap! wss/solver-status assoc-in ;; causes a lot of reactions... but worth it?
  ;;                                                                   ;       [client-name solver-name :time-running]
  ;;                                                                   ;       (ut/format-duration (get v :started) (System/currentTimeMillis)))
  ;;                                                                   (inc inner-acc))
  ;;                                                               inner-acc))
  ;;                                                           0
  ;;                                                           solvers)]
  ;;                                            (+ acc updated-count)))
  ;;                                        0
  ;;                                        @wss/solver-status)]
  ;;                     (swap! wss/solver-usage conj total-updated))
  ;;                  "Update Solver Statuses")

  ;; (defn stop-all-schedulers []
  ;;   (doseq [scheduler [mon param-sync refresh-flow-tables purge ;timekeeper
  ;;                      cpukeeper pushkeeper peerkeeper
  ;;                      purge-solver-cache solver-statuses]]
  ;;     (ut/pp [:stopping-async-job-scheduler! (str scheduler)])
  ;;     ;;(async/close! scheduler)
  ;;     scheduler))


  (def last-look (atom {}))
  (def saved-uids (atom []))

  ;; create a flow of all the signals and solvers relations (or an attempt to) - just a fun little experiment for now
  ;; (future
  ;;   (let [signals  (vec (keys @wss/signals-atom))
  ;;         signalsv (vec (apply concat
  ;;                              (for [[k v] @wss/signals-atom]
  ;;                                (for [e (filter #(and (keyword? %) (or (some (fn [x] (= x %)) signals) (cstr/includes? (str %) "/")))
  ;;                                                (ut/deep-flatten (get v :signal)))]
  ;;                                  [e (keyword (str "signal/" (cstr/replace (str k) ":" "")))]))))
  ;;         solvers  (vec (for [[k v] @wss/solvers-atom
  ;;                             :when (keyword? (get v :signal))]
  ;;                         [(get v :signal) (keyword (str "solver/" (cstr/replace (str k) ":" "")))]))
  ;;         conns    (vec (distinct (into signalsv solvers)))
  ;;         fmap     (wss/warren-flow-map conns)]
  ;;     (ut/pretty-spit "./flows/generated-flow-map.edn" fmap)
  ;;   ;;(ut/pp [:warren-flow? conns])
  ;;     ))

  (shutdown/add-hook! ::the-pool-is-now-closing
                      #(do (reset! wss/shutting-down? true)
                           (wss/destroy-websocket-server!)
                          ;;  (do (ut/pp [:saving-shutdown-metrics...])
                          ;;      (ext/create-dirs "./shutdown-logs")
                          ;;      (ut/println-to-file (str "./shutdown-logs/" (cstr/replace (ut/get-current-timestamp) " " "_") ".log")
                          ;;                          (fn [] (let [freqs [1 10 30 60 120 240 600]
                          ;;                                       stats [:cpu :threads :mem :sql-queries+ :sql-exec+ :nrepl-calls+ :solvers+ :flows+ :pool-tasks-run+ :workers :msgs+]]
                          ;;                                   (wss/draw-stats      nil freqs true) ;; all stats, with labels 
                          ;;                                   (wss/draw-pool-stats nil freqs true)
                          ;;                                   (wss/draw-client-stats nil freqs nil true {:metrics-atom wss/sql-metrics})
                          ;;                                   (wss/draw-client-stats nil freqs nil true)))))
                           (qp/stop-slot-queue-system)
                           (stop-all-schedulers)
                          ;;  (reset! flow-db/results-atom (select-keys @flow-db/results-atom 
                          ;;                                            (filter (fn [x] ;; clean up temp runs
                          ;;                                                      (not (or (cstr/includes? (str x) "solver-flow")
                          ;;                                                               (cstr/includes? (str %) "raw")))) 
                          ;;                                                    (keys @flow-db/results-atom))))
                          ;;  (let [destinations (vec (keys @wss/client-queues))]
                          ;;    (doseq [d destinations]
                          ;;      (wss/alert! d
                          ;;                  [:v-box 
                          ;;                   :justify :center 
                          ;;                   :style {:opacity 0.7}
                          ;;                   :children [[:box :style {:color :theme/editor-outer-rim-color :font-weight 700}
                          ;;                               :child [:box :child (str "Heads up: R-V-B-B-I-T system going offline.")]]]]
                          ;;                  10 1
                          ;;                  5)))
                           ;;  (Thread/sleep 2000)
                           (wss/destroy-websocket-server!)
                           ;;(tt/stop!)
                           ;(wss/stop-worker)
                           ;(wss/stop-worker2)
                           ;(wss/stop-worker3)
                           ;(wss/stop-workers4)
                           ;(wss/stop-workers5)
                           (Thread/sleep 2000)
                           (wss/destroy-websocket-server!)))
  (shutdown/add-hook! ::the-pool-is-now-closed
                      #(do
                         (ut/pp [:the-pool-is-now-closing :please-take-your-towels-with-you :have-a-nice-day-citizen!])
                         (doseq [[conn-name conn] @wss/conn-map
                                 :let [cn (get conn :datasource)]]
                           (ut/ppa [:shutting-down-connection-pool conn-name cn])
                           (sql/close-pool cn))
                         (doseq [[conn-name conn] @sql/client-db-pools]
                           (ut/ppa [:shutting-down-client-connection-pool (cstr/replace (str conn-name) ":" "") conn])
                           (sql/close-pool conn))))
  (shutdown/add-hook! ::close-system-pools
                      #(do (wss/destroy-websocket-server!)
                           (Thread/sleep 1000)
                           (wss/stop-web-server!)
                           ;(wss/stop-worker)
                           ;(wss/stop-worker2)
                           ;(wss/stop-worker3)
                           (ut/ppa [:shutting-down-system-pools])
                           (hik/close-datasource (get system-db :datasource))
                           (wss/destroy-websocket-server!)))
  (shutdown/add-hook! ::clear-cache
                      #(do (ut/ppa [:freezing-system-atoms])
                           (ut/freeze-atoms)
                           (ut/ppa [:freezing-flow-results-atom])
                           (with-open [wtr (io/writer "./data/atoms/flow-db-results-atom.edn")]
                             (binding [*out* wtr] (prn @flow-db/results-atom)))
                           (ut/zprint-file "./defs/signals.edn" {:style [:justified-original] :parse-string? true :comment {:count? nil :wrap? nil} :width 120 :map {:comma? false :sort? false}})
                           (ut/zprint-file "./defs/solvers.edn" {:style [:justified-original] :parse-string? true :comment {:count? nil :wrap? nil} :width 120 :map {:comma? false :sort? false}})
                           ;(ut/ppa [:clearing-cache-db])
                           ;(shell/sh "/bin/bash" "-c" (str "rm " "db/cache.db"))
                           (shell/sh "/bin/bash" "-c" (str "rm " "flow-logs/*"))
                           (shell/sh "/bin/bash" "-c" (str "rm " "reaction-logs/*"))
                           (shell/sh "/bin/bash" "-c" (str "rm " "status-change-logs/*"))
                           (shell/sh "/bin/bash" "-c" (str "rm " "tracker-logs/*"))
                           (shell/sh "/bin/bash" "-c" (str "rm " "reaction-logs/*"))
                           (shell/sh "/bin/bash" "-c" (str "rm " "db/system.db"))))

  (defn update-stat-atom [kks]
    (qp/serial-slot-queue :update-stat-atom-serial :serial
    ;(ppy/execute-in-thread-pools :update-stat-atom
                          (fn []
                            (let [;flows (if (or (nil? flows) (empty? flows))
                                  ]
                              (swap! db/flow-status merge
                                     (into
                                      {}
                                      (for [k     kks ;(keys @flow-db/results-atom)
                                            :when (not (= k "client-keepalive"))] ;; dont want to track heartbeats
                    ;(qp/serial-slot-queue :update-stat-atom-serial :serial (fn []
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
                                                                      (get @wss/orig-caller k :rvbbit))
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
                                              :*running?       running?}}))))))))

  (defn tracker-changed
    [_ _ old-state new-state]
    (when (not= old-state new-state)
      (let [[_ b _] (data/diff old-state new-state)
            kks     (try (keys b) (catch Exception _ []))
            kks     (vec (remove #(= "client-keepalive" %) kks))
            ;kks     (remove #(= "client-keepalive" %) kks)
            ]
        (when (> (count kks) 0)
          (update-stat-atom kks)))))

  (ppy/add-watch+ flow-db/status
                  :flow-db-status
                  tracker-changed
                  :flow-db-status)

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

  ;; (defn tracker-changed2
  ;;   [key ref old-state new-state]
  ;;   (let [[_ b _] (data/diff old-state new-state)
  ;;         kks     (try (keys b) (catch Exception _ nil))]
  ;;     (when (> (count (remove #(= "client-keepalive" %) kks)) 0)
  ;;       ;(async/thread ;; really expensive logging below. temp
  ;;       (ppy/execute-in-thread-pools :flow-tracker-side-effects*  
  ;;         (fn []
  ;;           (let [;kks-string (-> (cstr/join "-" kks) (cstr/replace " " "_") (cstr/replace "/"
  ;;                 ]
  ;;             (ext/create-dirs "./flow-logs/")
  ;;             (doseq [flow-id kks
  ;;                     :let    [run-id     (str (get-in @flow-db/results-atom [flow-id :run-id]))
  ;;                              fp         (str "./flow-logs/" (cstr/replace flow-id "/" "-CALLING-") "--" run-id ".edn")
  ;;                              mst        (System/currentTimeMillis)
  ;;                              _ (swap! wss/watchdog-atom assoc flow-id mst)
  ;;                              _ (swap! wss/watchdog-atom assoc flow-id mst)
  ;;                              diffy      (get (ut/replace-large-base64 b) flow-id)
  ;;                              diffy-keys (into {} (for [[k v] diffy] {k (first (keys v))}))
  ;;                              _ (swap! wss/last-block-written assoc flow-id diffy-keys)
  ;;                              blocks     (keys (get (ut/replace-large-base64 b) flow-id))]]
  ;;               (let [data        [[(ut/millis-to-date-string mst) blocks]
  ;;                                  {:values (select-keys (ut/replace-large-base64 (get-in @flow-db/results-atom [flow-id])) blocks)
  ;;                                   :diff   diffy}]
  ;;                     pretty-data (with-out-str (ppt/pprint data))] ; Use clojure.pprint/pprint
  ;;                 (spit fp (str pretty-data "\n") :append true))))))
  ;;       (log-tracker kks)
  ;;       (update-stat-atom kks))))

  ;; (ppy/add-watch+ flow-db/tracker 
  ;;                 :master-flow-db-tracker-watcher* ;; only side effects
  ;;                 tracker-changed2 
  ;;                 :master-flow-db-tracker-watcher*)

  ;; (ppy/add-watch+ flow-db/tracker
  ;;                 :master-flow-db-tracker-watcher
  ;;                 (fn [_ _ old-state new-state]
  ;;                   (let [changes (reduce-kv
  ;;                                  (fn [m k v]
  ;;                                    (if (not= v (get old-state k))
  ;;                                      (assoc m k v)
  ;;                                      m))
  ;;                                  {}
  ;;                                  new-state)]
  ;;                     (doseq [[k v] changes]
  ;;                       (swap! wss/flow-tracker-child-atoms
  ;;                              (fn [child-atoms]
  ;;                                (if-let [child-atom (get child-atoms k)]
  ;;                                  (do (swap! child-atom assoc k v)
  ;;                                      child-atoms)
  ;;                                  (let [new-child-atom (atom {k v})]
  ;;                                    (assoc child-atoms k new-child-atom))))))))
  ;;                 :master-flow-db-tracker-watcher)


  (defn combined-tracker-watcher
    [key ref old-state new-state]
    (when (not= new-state old-state)

     ; (qp/serial-slot-queue :combined-tracker-watcher-serial :serial-queues
      (ppy/execute-in-thread-pools :combined-tracker-watcher-side-effects
                                   (fn []

                                     (let [[_ b _] (data/diff old-state new-state)
                                           kks     (try (keys b) (catch Exception _ nil))]
                                       (when (> (count (remove #(= "client-keepalive" %) kks)) 0)
      ;; Side effects part (originally from tracker-changed2)
                                         (ppy/execute-in-thread-pools :flow-log-file-writing
                                                                      (fn []
                                                                        (ext/create-dirs "./flow-logs/")
                                                                        (doseq [flow-id kks
                                                                                :let [run-id     (str (get-in @flow-db/results-atom [flow-id :run-id]))
                                                                                      fp         (str "./flow-logs/" (cstr/replace flow-id "/" "-CALLING-") "--" run-id ".edn")
                                                                                      mst        (System/currentTimeMillis)
                                                                                      _          (swap! wss/watchdog-atom assoc flow-id mst)
                                                                                      diffy      (get (ut/replace-large-base64 b) flow-id)
                                                                                      diffy-keys (into {} (for [[k v] diffy] {k (first (keys v))}))
                                                                                      _          (swap! wss/last-block-written assoc flow-id diffy-keys)
                                                                                      blocks     (keys (get (ut/replace-large-base64 b) flow-id))]]
                                                                          (let [data        [[(ut/millis-to-date-string mst) blocks]
                                                                                             {:values (select-keys (ut/replace-large-base64 (get-in @flow-db/results-atom [flow-id])) blocks)
                                                                                              :diff   diffy}]
                                                                                pretty-data (with-out-str (ppt/pprint data))]
                                                                            (spit fp (str pretty-data "\n") :append true)))))
                                         (log-tracker kks)
                                         (update-stat-atom kks))

    ;; Child atom update part (originally from the second watcher)
                                ;; (let [changes (reduce-kv
                                ;;                (fn [m k v]
                                ;;                  (if (not= v (get old-state k))
                                ;;                    (assoc m k v)
                                ;;                    m))
                                ;;                {}
                                ;;                new-state)]
                                ;;   (doseq [[k v] changes]
                                ;;     (swap! wss/flow-tracker-child-atoms
                                ;;            (fn [child-atoms]
                                ;;              (if-let [child-atom (get child-atoms k)]
                                ;;                (do (reset! child-atom {k v})
                                ;;                    child-atoms)
                                ;;                (let [new-child-atom (atom {k v})]
                                ;;                  (assoc child-atoms k new-child-atom)))))))
                                       )))))

;; Add the combined watcher
  (ppy/add-watch+ flow-db/tracker
                  :flow-db-tracker
                  combined-tracker-watcher
                  :flow-db-tracker)

  (wss/sub-to-value :rvbbit :time/unix-ms true) ;; importante! 

  ;; (ut/pp {:settings config/settings})
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

  (wss/schedule! [:seconds wss/heartbeat-seconds]
                 (heartbeat :all)
                 {:flow-id "client-keepalive" :increment-id? false :close-on-done? true :debug? false})

  ;; (wss/schedule! [:minutes 20]
  ;;                "game-of-life-test1"
  ;;                {:close-on-done? true
  ;;                 :increment-id?  false
  ;;                 :flow-id        "game-of-life-test1"
  ;;                 :debug?         false
  ;;                 :overrides      {:iterations-max 1000 :tick-delay-ms 800}})

  ;; (wss/schedule! [:minutes 30] "counting-loop" {:flow-id "counting-loop" :increment-id? false :close-on-done? true :debug? false})

  ;; (wss/destroy-websocket-server!)
  ;; (reset! wss/websocket-server (jetty/run-jetty #'wss/web-handler wss/ring-options))

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
