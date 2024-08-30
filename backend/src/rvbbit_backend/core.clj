(ns rvbbit-backend.core
   (:require
    [clj-http.client :as client]
    [clj-time.coerce :as tcc]
    [clj-time.core :as t]
    [clj-time.format :as f]
    [clj-time.jdbc]
    [clojure.core.async :as    async :refer [<! <!! >! >!! chan go]]
    [clojure.core.async :refer [<! timeout]]
    [clojure.data :as data]
    [clojure.edn :as edn]
    [clojure.java.io :as io]
    [clojure.java.jdbc :as jdbc]
    [clojure.java.shell :as shell]
   ;[clojure.math.combinatorics :as combo]
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
   ;[honey.sql :as honey]
    [nextjournal.beholder :as beholder]
   ;[puget.printer :as puget]
    [ring.adapter.jetty9 :as jetty]
    [rvbbit-backend.queue-party  :as qp]
   ;[rvbbit-backend.assistants :as ass]
   ;[rvbbit-backend.reactor :as rkt]
    [rvbbit-backend.config :as config]
    [rvbbit-backend.db :as db]
    [rvbbit-backend.freezepop :as fpop]
    [rvbbit-backend.cruiser :as cruiser]
   ;[rvbbit-backend.embeddings :as em]
    [rvbbit-backend.evaluator :as evl]
    [rvbbit-backend.external :as ext]
    [rvbbit-backend.sql :as    sql
     :refer [flows-db insert-error-row! pool-create sql-exec sql-query sql-query-meta sql-query-one system-db cache-db cache-db-memory history-db
             to-sql]]
   ;[rvbbit-backend.surveyor :as surveyor]
   ;[rvbbit-backend.transform :as ts]
    [rvbbit-backend.util :as    ut
     :refer [ne?]]
    [rvbbit-backend.websockets :as wss]
   ;[cognitect.transit :as transit]
    [shutdown.core :as shutdown]
   ;[taskpool.taskpool :as tp]
    [websocket-layer.core      :as wl])
   (:import
    [java.util Date]
   ;;[java.util.concurrent Executors TimeUnit]
    [java.io ByteArrayInputStream ByteArrayOutputStream]
    [java.util.concurrent Executors ThreadPoolExecutor SynchronousQueue TimeUnit TimeoutException ThreadPoolExecutor$CallerRunsPolicy]
    java.nio.file.Files
    java.nio.file.Paths
    java.nio.file.attribute.FileTime
    java.nio.file.LinkOption
    java.time.format.DateTimeFormatter
    java.time.ZoneId)
   (:gen-class))

;; =======================================================================================================================================
;; Note to readers: lots of refactoring, namespace, mess cleanup to do, I just came out of a very long "move fast and break things" season
;; Note to readers: lots of refactoring, namespace, mess cleanup to do, I just came out of a very long "move fast and break things" season
;; Note to readers: lots of refactoring, namespace, mess cleanup to do, I just came out of a very long "move fast and break things" season
;; =======================================================================================================================================

 (def harvest-on-boot? (get (config/settings) :harvest-on-boot? true))

 (defn get-last-modified-time
   [f-path]
   (let [path          (Paths/get f-path (into-array String []))
         instant       (.toInstant (Files/getLastModifiedTime path (into-array LinkOption [LinkOption/NOFOLLOW_LINKS])))
         zonedDateTime (.atZone instant (ZoneId/systemDefault))
         formatter     (DateTimeFormatter/ofPattern "yyyy-MM-dd HH:mm:ss")]
     (.format formatter zonedDateTime)))

 (def invalid-to-valid-keywords {":>" ":reagent-gt"})
 (def valid-to-invalid-keywords (zipmap (vals invalid-to-valid-keywords) (keys invalid-to-valid-keywords)))

 (defn convert-to-valid-edn [s]
   (reduce (fn [s [invalid valid]]
             (clojure.string/replace s invalid valid)) s invalid-to-valid-keywords))

 (defn convert-back-to-original [m]
   (clojure.walk/postwalk
    (fn [x] (if (and (keyword? x) (valid-to-invalid-keywords x))
              (valid-to-invalid-keywords x) x)) m))

 (defn read-screen [f-path]
   (let [screen-str    (slurp f-path)
         valid-edn-str (convert-to-valid-edn screen-str)
         screen-data   (try (edn/read-string valid-edn-str) (catch Exception e (ut/pp [:read-screen-error!!!!! f-path e]) {}))]
     (convert-back-to-original screen-data)))

 (defn update-screen-meta [f-path]
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

 (defn update-all-screen-meta []
   (sql-exec system-db (to-sql {:delete-from [:screens]})) ;; just in case
   (let [prefix "./screens/"
         files  (ut/get-file-vectors-simple prefix ".edn")]
     (doseq [f files] (update-screen-meta (str prefix f)))))

 (defn update-flow-meta [f-path]
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

 (defn update-all-flow-meta []
   (sql-exec flows-db (to-sql {:delete-from [:flows]})) ;; just in case
   (let [prefix "./flows/"
         files  (ut/get-file-vectors-simple prefix ".edn")]
     (doseq [f files]
       (let [f-path (str prefix f)]
         (wss/create-flowblock f-path)
         (update-flow-meta f-path)))))

 (defn refresh-flow-fn [flow-functions-map]
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

 (defn watch-flows-folder []
   (let [file-path "./flows/"]
     (beholder/watch #(when (cstr/ends-with? (str (get % :path)) ".edn")
                        (let [f-path (str (get % :path))
                              f-op   (get % :type)
                              ffn    (wss/create-flowblock f-path)]
                          (refresh-flow-fn {:sub-flow ffn}) ;; change the sql for the sub-flow block
                          (ut/pp [:flow-change! f-op f-path])
                          (update-flow-meta f-path)))
                     file-path)))

 (defn watch-screens-folder []
   (let [file-path "./screens/"]
     (beholder/watch #(when (cstr/ends-with? (str (get % :path)) ".edn")
                        (let [f-path (str (get % :path))
                              f-op   (get % :type)]
                          (ut/pp [:screen-change! f-op f-path])
                          (update-screen-meta f-path)))
                     file-path)))

 (defn db-sniff-solver-default [f-path conn-name]
   {:signal :signal/daily-at-9am
    :type   :clojure
    :cache? false
    :data   (walk/postwalk-replace
             {:conn-name conn-name
              :f-path f-path}
             '(do
                (ns rvbbit-backend.cruiser
                  (:require [rvbbit-backend.websockets :as wss]
                            [rvbbit-backend.pool-party :as ppy]
                            [rvbbit-backend.sql :as sql]))
                (let [conn (get (deref wss/conn-map) :conn-name)]
                 ;(ppy/execute-in-thread-pools
                 ; :conn-schema-sniff-serial
                 ; (fn []
                  (lets-give-it-a-whirl-no-viz
                   :f-path
                   conn
                   sql/system-db
                   default-sniff-tests
                   default-field-attributes
                   default-derived-fields
                   default-viz-shapes))))})

;; (ut/pp (db-sniff-solver-default "./connections/postgres.edn" "postgres"))
;; (ut/pp (keys @wss/conn-map))

 (defn update-all-conn-meta []
   (let [prefix "connections/"
         files  (ut/get-file-vectors-simple prefix ".edn")
         current-conn-ids (vec (for [f files ;; remove "missing" DBs - TODO more robust archive and reappear
                                     :let [f-path    (str prefix f)
                                           conn-name (str (cstr/replace (cstr/lower-case (last (cstr/split f-path #"/"))) ".edn" ""))]]
                                 conn-name))]
     (doseq [f    files
             :let [f-path    (str prefix f)
                   conn-name (str (cstr/replace (cstr/lower-case (last (cstr/split f-path #"/"))) ".edn" ""))
                   solver-name (keyword (str "refresh-" conn-name))
                   solver-edn-fp "./defs/solvers.edn"
                   solver-exist? (get @wss/solvers-atom solver-name)
                   _ (when (not solver-exist?)
                       (ut/pp [:refresh-metadata-schedule-being-created-for conn-name :as solver-name])
                       (swap! wss/solvers-atom assoc solver-name (db-sniff-solver-default f-path conn-name))
                       (fpop/freeze-atom solver-edn-fp)
                       (ut/zprint-file solver-edn-fp {:style [:justified-original] :parse-string? true
                                                      :comment {:count? nil :wrap? nil} :width 120
                                                      :map {:comma? false :sort? false}})
                       (wss/reload-solver-subs))
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
               (ppy/execute-in-thread-pools :conn-schema-sniff-serial
                                            (fn []
                                              (cruiser/lets-give-it-a-whirl-no-viz f-path
                                                                                   conn
                                                                                   system-db
                                                                                   cruiser/default-sniff-tests
                                                                                   cruiser/default-field-attributes
                                                                                   cruiser/default-derived-fields
                                                                                   cruiser/default-viz-shapes)))))
         (catch Exception e (do (swap! wss/conn-map dissoc conn-name) (ut/pp [:sniff-error conn-name e])))))
     (cruiser/clean-up-some-db-metadata current-conn-ids)))

;; (update-all-conn-meta)

 (defn watch-connections-folder []
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
                                                                f-op   (get % :type)
                                                                clients (vec (keys @wss/client-queues))]
                                                            (let [destinations clients]
                                                              (doseq [d destinations]
                                                                (wss/alert! d
                                                                            [:v-box :justify :center :style {:opacity 0.9} :children
                                                                             [[:box :style {:color :theme/editor-outer-rim-color :font-weight 700}
                                                                               :child (str "Note: CSV file being imported to cache-db")]
                                                                              [:box :child (str (get % :path))]]]
                                                                            14 2
                                                                            11)))
                                                            (ut/pp [:csv-file-change! f-op f-path])
                                                            (wss/process-csv {} f-path)
                                                            (let [destinations clients]
                                                              (doseq [d destinations]
                                                                (wss/alert! d
                                                                            [:v-box :justify :center :style {:opacity 0.9} :children
                                                                             [[:box :style {:color :theme/editor-outer-rim-color :font-weight 700}
                                                                               :child (str "Note: CSV file import finished")]
                                                                              [:box :child (str (get % :path))]]]
                                                                            14 2
                                                                            11)))))
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
                            (wss/kick d [:settings] (wss/package-settings-for-client :rvbbit) 1 :none (str "file updated " (get % :path))))))
                     file-path)))

 (defn watch-solver-files []
   (let [file-path "./defs/"]
     (beholder/watch #(ppy/execute-in-thread-pools
                       :watch-solver-files-serial
                       (fn [] (when (and
                                     (or (cstr/ends-with? (str (get % :path)) "signals.edn")
                                         (cstr/ends-with? (str (get % :path)) "solvers.edn"))
                                     (not @wss/shutting-down?))
                                (let [signals? (cstr/ends-with? (str (get % :path)) "signals.edn")
                                      destinations (vec (keys @wss/client-queues))
                                      map-atom (if signals? wss/signals-atom wss/solvers-atom)
                                      _ (reset! map-atom (edn/read-string (slurp (str (get % :path)))))
                                      _ (ut/pp [(if signals? :signals :solvers) :file-change! signals? (get % :path)])
                                      _ (if signals?
                                          (wss/reload-signals-subs)
                                          (wss/reload-solver-subs))]
                                  (doseq [d destinations]
                                    (wss/alert! d
                                                [:v-box :justify :center :style {:opacity 0.7} :children
                                                 [[:box :style {:color :theme/editor-outer-rim-color :font-weight 700}
                                                   :child (str "Note: Server " (if signals? "signals" "solvers") " have been updated & received")]
                                                  [:box :child (str (get % :path))]]]
                                                13 2
                                                5)
                                    (wss/kick d (if signals? :signals-file :solvers-file) @map-atom 1 :none (str "file updated " (get % :path))))))))
                     file-path)))

 (defn start-services []
   (ut/pp [:starting-services...])
   (evl/create-nrepl-server!) ;; needs to start
   (update-all-screen-meta) ;; has queue per screen 
   (update-all-flow-meta) ;; has queue per flow 
   (update-all-conn-meta)
   (wss/reload-signals-subs) ;; run once on boot
   (wss/reload-solver-subs) ;; run once on boot (needs nrepl(s) to start first...)
   (wss/create-web-server!)
   (let [flows (vec (filter #(not (cstr/includes? (str %) "-solver-flow-")) (keys @flow-db/results-atom)))]
     (reset! flow-db/results-atom (select-keys (ut/replace-large-base64 @flow-db/results-atom) flows))))

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

 (defn reboot-websocket-server-long []
   (let [destinations (vec (keys @wss/client-queues))]
     (doseq [d destinations]
       (wss/alert! d
                   [:v-box
                    :justify :center
                    :style {:opacity 0.7}
                    :children [[:v-box :style {:color :theme/editor-outer-rim-color :font-weight 700 :font-size "14px"}
                                :children [[:box :child (str "Restarting Websocket Server...")]
                                           [:progress-bar [280 150 (str (rand-int 12345))]]]]]]
                   10 1
                   160)))
   (Thread/sleep 10000)
   (wss/destroy-websocket-server!)
   (Thread/sleep 120000)
   (reset! wss/websocket-server (jetty/run-jetty #'wss/web-handler wss/ring-options)))

 (defn reboot-reactor-and-resub []
    ;(evl/graceful-restart-nrepl-server!)
    ;(Thread/sleep 10000)
   (db/reboot-reactor!)
    ;(Thread/sleep 10000)
   (wss/reload-signals-subs)
   (wss/reload-solver-subs))

 (def last-look (atom {}))
 (def saved-uids (atom []))

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
                                       (let [cc              (into {} (for [[k v] @flow-db/results-atom] {k (count v)})) ;; wtf?
                                             blocks          (get-in @flow-db/working-data [k :components-list])
                                             running-blocks  (vec (for [[k v] (get @flow-db/tracker k) :when (nil? (get v :end))] k))
                                             done-blocks     (vec (for [[k v] (get @flow-db/tracker k) :when (not (nil? (get v :end)))] k))
                                             not-started-yet (vec (cset/difference (set blocks) (set (into running-blocks done-blocks))))
                                             running-blocks  (vec (cset/difference (set blocks) (set (into done-blocks not-started-yet))))
                                             res             (get-in @flow-db/results-atom [k :done] :nope)
                                             running?        (ut/ne? running-blocks) ;;(and (or (= res :nope) (ut/chan? res) (= res :skip)) (ut/ne? running-blocks))
                                             done?           (and (empty? not-started-yet) (empty? running-blocks)) ;;(and (empty? running-blocks) (not (= res :nope)))
                                             error?          (try (some #(or (= % :timeout) (= % :error)) (ut/deep-flatten res)) (catch Exception _ false))
                                              ;;;_ (ut/pp [:flow-status! k :running? running? :done? done?   (get-in @flow-db/tracker [k :done :end])  ])
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
                                             orig-flow-id    (str (get-in @flow-db/results-atom [k :opts-map :orig-flow-id] k))
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
                                                                        :orig_flow_id    orig-flow-id
                                                                        :elapsed         elapsed
                                                                        :overrides       (pr-str overrides)
                                                                        :elapsed_seconds (float (/ elapsed 1000))
                                                                        :in_error        (cstr/includes? (str res) ":error")
                                                                        :human_elapsed   (str human-elapsed)}
                                                            insert-sql {:insert-into [:flow_history] :values [row]}]
                                                        (sql-exec flows-db (to-sql insert-sql))) ;; sql-exec has it's own serial queue 
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

 (defn tracker-changed [_ _ old-state new-state]
   (when (not= old-state new-state)
     (let [[_ b _] (data/diff old-state new-state)
           kks     (try (keys b) (catch Exception _ []))
           kks     (vec (remove #(= "client-keepalive" %) kks))
            ;kks     (remove #(= "client-keepalive" %) kks)
           ]
       (when (> (count kks) 0)
         (update-stat-atom kks)))))

 (defn log-tracker [kks]
   (doseq [flow-id kks]
     (let [orig-tracker (get @flow-db/tracker flow-id)
           tracker      (into {}
                              (for [[k v] orig-tracker ;; remove condi non-starts. dont send
                                    :when (not (get v :in-chan?))]
                                {k v}))]
       (swap! wss/tracker-history assoc flow-id (vec (conj (get @wss/tracker-history flow-id []) tracker))))))

 (defn combined-tracker-watcher [key ref old-state new-state]
   (when (not= new-state old-state)

     ; (qp/serial-slot-queue :combined-tracker-watcher-serial :serial-queues
     (ppy/execute-in-thread-pools
      :combined-tracker-watcher-side-effects
      (fn []

        (let [[_ b _] (data/diff old-state new-state)
              kks     (try (keys b) (catch Exception _ nil))]
          (when (> (count (remove #(= "client-keepalive" %) kks)) 0)
      ;; Side effects part (originally from tracker-changed2)
            (ppy/execute-in-thread-pools :flow-log-file-writing
                                         (fn []
                                          ;(ext/create-dirs "./flow-logs/")
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
            (ppy/execute-in-thread-pools
             :runstream-pre-package
             (fn [] (doseq [k kks] (swap! db/runstream-atom assoc k (wss/get-flow-open-ports k k)))))
              ;; get-flow-open-ports [flowmap flow-id client-name]
            (update-stat-atom kks)))))))

 (defn heartbeat [dest] ;; flowmaps flow for websocket client heartbeats
   {:components {:kick-1 {:inputs [:destination :name :sub-task :thread-id :thread-desc :message-name]
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
    :connections [[:kick-1 :done]
                  [:open-input :kick-1/name]
                  [:open-input :kick-1/sub-task]
                  [:open-input :kick-1/thread-id]
                  [:open-input :kick-1/thread-desc]
                  [:open-input :kick-1/message-name]
                  [:kick-1destination :kick-1/destination]]})

 (defn -main [& args]
   (println " ")
   (ut/print-ansi-art "data/nname.ans")
   (ut/print-ansi-art "data/rrvbbit.ans")
   (ut/pp [:version 0 :august 2024 "Hi."])
   (ut/pp [:pre-alpha "lots of bugs, lots of things to do - but, and I hope you'll agree.. lots of potential."])
   (ut/pp ["Ryan Robitaille" "@ryrobes" ["rvbbit.com" "ryrob.es"] "ryan.robitaille@gmail.com"])
  ;; (println " ")
  ;; (wss/fig-render "Curiouser and curiouser!" :pink)

   (shell/sh "/usr/bin/env" "bash" "-c" (str "rm -rf " "live/*"))

   (qp/create-slot-queue-system)
   (fpop/thaw-flow-results)

   (sql-exec system-db "drop table if exists jvm_stats;")
   (sql-exec system-db "drop table if exists errors;")
   (sql-exec system-db "drop view  if exists viz_recos_vw;")
   (sql-exec system-db "drop view  if exists viz_recos_vw2;")
   (sql-exec system-db "drop view  if exists user_subs_vw;")
   (sql-exec system-db "drop view  if exists latest_status;")

   (cruiser/create-sqlite-sys-tables-if-needed! system-db)
   (cruiser/create-sqlite-flow-sys-tables-if-needed! flows-db)

   (cruiser/create-sqlite-sys-tables-if-needed! cruiser/tmp-db-dest1)
   (cruiser/create-sqlite-sys-tables-if-needed! cruiser/tmp-db-dest2)
   (cruiser/create-sqlite-sys-tables-if-needed! cruiser/tmp-db-dest3)

   (when harvest-on-boot?
     (update-all-conn-meta))

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
   #_{:clj-kondo/ignore [:inline-def]}
   (defonce session-file-watcher (wss/subscribe-to-session-changes))



  ;; create dirs for various artifacts, if not already present
   (doseq [dir ["assets" "assets/snaps" "assets/screen-snaps" "defs/backup"
                "flow-logs" "flow-blocks" "flow-history" "fabric-sessions" "live"]]
     (ext/create-dirs dir))

  ;; temp test indexes
  ;; (ut/pp (sql-exec system-db "CREATE INDEX idx_client_name ON client_memory(client_name);"))
  ;; (ut/pp (sql-exec system-db "CREATE INDEX idx_ts ON client_memory(ts);"))
  ;; (ut/pp (sql-exec system-db "CREATE INDEX idx_client_name2 ON client_memory(client_name, ts);"))
  ;; (ut/pp (sql-exec system-db "CREATE INDEX idx_ts1 ON jvm_stats(ts);"))

  ;; RABBIT REACTOR
  ;; boot master watchers for defined atoms 
   (doseq [[type atom] db/master-reactor-atoms]
     (db/master-watch-splitter-deep type atom))

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

   (cruiser/insert-current-rules!
    system-db
    "system-db"
    0
    cruiser/default-sniff-tests
    cruiser/default-field-attributes
    cruiser/default-derived-fields
    cruiser/default-viz-shapes
    (merge cruiser/default-flow-functions {:custom @wss/custom-flow-blocks} {:sub-flows @wss/sub-flow-blocks}))

   (cruiser/lets-give-it-a-whirl-no-viz
    "system-db"
    system-db
    system-db
    cruiser/default-sniff-tests
    cruiser/default-field-attributes
    cruiser/default-derived-fields
    cruiser/default-viz-shapes)

   (cruiser/lets-give-it-a-whirl-no-viz
    "flows-db"
    flows-db
    system-db
    cruiser/default-sniff-tests
    cruiser/default-field-attributes
    cruiser/default-derived-fields
    cruiser/default-viz-shapes)

   (cruiser/clean-up-db-metadata "cache.db")
   (cruiser/lets-give-it-a-whirl-no-viz
    "cache.db"
    cache-db
    system-db
    cruiser/default-sniff-tests
    cruiser/default-field-attributes
    cruiser/default-derived-fields
    cruiser/default-viz-shapes)

   (cruiser/clean-up-db-metadata "cache.db.memory")
   (cruiser/lets-give-it-a-whirl-no-viz
    "cache.db.memory"
    cache-db-memory
    system-db
    cruiser/default-sniff-tests
    cruiser/default-field-attributes
    cruiser/default-derived-fields
    cruiser/default-viz-shapes)

   (cruiser/lets-give-it-a-whirl-no-viz
    "history-db"
    history-db
    system-db
    cruiser/default-sniff-tests
    cruiser/default-field-attributes
    cruiser/default-derived-fields
    cruiser/default-viz-shapes)

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
                      (fn []
                        (cruiser/lets-give-it-a-whirl-no-viz
                         "cache.db"
                         cache-db
                         system-db
                         cruiser/default-sniff-tests
                         cruiser/default-field-attributes
                         cruiser/default-derived-fields
                         cruiser/default-viz-shapes)
                        (wss/param-sql-sync)))
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

    ;; (start-scheduler 30
    ;;                #(qp/serial-slot-queue
    ;;                  :re-sniff-cache-db :single
    ;;                  (fn []
    ;;                    (cruiser/lets-give-it-a-whirl-no-viz
    ;;                     "cache.db"
    ;;                     wss/cache-db
    ;;                     system-db
    ;;                     cruiser/default-sniff-tests
    ;;                     cruiser/default-field-attributes
    ;;                     cruiser/default-derived-fields
    ;;                     cruiser/default-viz-shapes)))
    ;;                "(Re)Sniff Cache SQL DB" 30)

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

  ;; (start-scheduler (* 3600 3) ;; 3 hours (causes big cpu jump whne lots of clients present?)
  ;;                  reboot-reactor-and-resub
  ;;                  "Reboot Reactor, Subs, Pools" (* 3600 3))

  ;; (start-scheduler (* 3600 3)
  ;;                  db/pool-cleaner!
  ;;                  "Pool Cleaner" (* 3600 3))

  ;; (start-scheduler 6000 ;; 10 mins - was causing problems?? TODO: investigate, not critical, we barely use the queues except for sqlite writes
  ;;                  #(qp/cleanup-inactive-queues 10) ;; MINUTES
  ;;                  "Purge Idle Queues" 7200)

;; (qp/cleanup-inactive-queues 0) 

  ;;(when debug? 
   (start-scheduler 1
                    #(do (swap! wss/peer-usage conj (count @wl/sockets))
                         (swap! wss/push-usage conj @wss/all-pushes)
                         (swap! wss/reaction-usage conj @wss/sent-reactions)
                         (swap! wss/signal-reaction-usage conj @wss/sent-signal-reactions)
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
                       (reset! sql/sql-query-log [])
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

  ;; (start-scheduler (* 3600 4) ;; every 4 hours, after 6 hours
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
  ;;                     (wss/reboot-websocket-handler!)
  ;;                     ;(ppy/reboot-websocket-thread-pool!)
  ;;                     (Thread/sleep 30000)
  ;;                     (reset! wss/websocket-server (jetty/run-jetty #'wss/web-handler wss/ring-options)))
  ;;                  "Restart Websocket Server, Test Debug" (* 3600 6))

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

   (shutdown/add-hook! ::heads-up-msg #(ut/ppa "Shutting down now!"))

   (shutdown/add-hook! ::the-pool-is-now-closing
                       #(do (reset! wss/shutting-down? true)
                            (doseq [electric-eye [start-conn-watcher start-screen-watcher start-flow-watcher
                                                  start-settings-watcher start-solver-watcher session-file-watcher]]
                              (do
                                (ut/pp [:stopping-beholder-watch (str electric-eye)])
                                (beholder/stop electric-eye)))
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
                            (fpop/freeze-atoms)
                            (ut/ppa [:freezing-flow-results-atom])
                            (fpop/freeze-flow-results)
                            (ut/zprint-file "./defs/signals.edn" {:style [:justified-original] :parse-string? true :comment {:count? nil :wrap? nil} :width 120 :map {:comma? false :sort? false}})
                            (ut/zprint-file "./defs/solvers.edn" {:style [:justified-original] :parse-string? true :comment {:count? nil :wrap? nil} :width 120 :map {:comma? false :sort? false}})
                           ;(ut/ppa [:clearing-cache-db])
                           ;(shell/sh "/usr/bin/env" "bash" "-c" (str "rm " "db/cache.db"))
                            (shell/sh "/usr/bin/env" "bash" "-c" (str "rm " "flow-logs/*"))
                            (shell/sh "/usr/bin/env" "bash" "-c" (str "rm " "reaction-logs/*"))
                           ;(shell/sh "/usr/bin/env" "bash" "-c" (str "rm " "status-change-logs/*"))
                            (shell/sh "/usr/bin/env" "bash" "-c" (str "rm " "tracker-logs/*"))
                            (shell/sh "/usr/bin/env" "bash"  "-c" (str "rm " "reaction-logs/*"))
                           ;(shell/sh "/usr/bin/env" "bash" "-c" (str "rm " "db/system.db"))
                            ))

   (ppy/add-watch+
    flow-db/status
    :flow-db-status
    tracker-changed
    :flow-db-status)

   (ppy/add-watch+
    flow-db/tracker
    :flow-db-tracker
    combined-tracker-watcher
    :flow-db-tracker)

   (wss/sub-to-value :rvbbit :time/unix-ms true) ;; importante! 

  ;; (ut/pp {:settings config/settings})

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
     (Thread/sleep 13000)
     (let [destinations (vec (keys @wss/client-queues))]
       (doseq [d destinations]
         (wss/alert! d
                     [:v-box :justify :center :style {:opacity 0.7} :children
                      [[:box :style {:color :theme/editor-outer-rim-color :font-weight 700} :child
                        [:box :child (str "Heads up: R-V-B-B-I-T system online.")]]]]
                     10
                     1
                     5)))
     (ut/pp [" GO: end of main-fn "])))
