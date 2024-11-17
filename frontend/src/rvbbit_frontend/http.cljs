(ns rvbbit-frontend.http
  (:require
   ["react-zoom-pan-pinch"  :as zpan]
   [ajax.core               :as ajax]
   [ajax.edn                :as ajax-edn]
   [clojure.set             :as cset]
   [clojure.string          :as cstr]
   [clojure.walk            :as walk]
   [day8.re-frame.http-fx]
   [day8.re-frame.undo      :as    undo
    :refer [undoable]]
   ;[re-frame.alpha          :as rfa]
   [re-frame.core           :as re-frame]
   [reagent.core            :as reagent]
   ;[re-pollsive.core        :as poll]
   [rvbbit-frontend.db      :as db]
  ;;  [rvbbit-frontend.block-patrol      :as bp]
   [rvbbit-frontend.utility :as ut]
   [websocket-fx.core       :as wfx]))

(def socket-id :default)

(defn subscription
  [x sub-name]
  {:message    {:kind        sub-name ;; :server-push2
                :client-name x
                :ui-keypath  [:server]
                :extras      {:hello? true}}
   :on-message [::simple-response]})

(re-frame/reg-sub
 ::pending-requests
 (fn [db {:keys [socket-id]}] ;; modded for re-frame.alpha/sub usage with a destruct map arg
   (vals (get-in db [:websocket-fx.core/sockets socket-id :requests]))))

(re-frame/reg-sub
 ::open-subscriptions
 (fn [db {:keys [socket-id]}] ;; modded for re-frame.alpha/sub usage with a destruct map arg
   (get-in db [:websocket-fx.core/sockets socket-id :subscriptions])))

(re-frame/reg-event-db
 ::refresh-all
 (fn [db _]
   (-> db
       (dissoc :query-history)
       (dissoc :query-history-meta))))

(defonce last-http-panel-push (atom nil))
(defonce force-once? (atom false))

(re-frame/reg-event-db
 ::update-panels-hash
 (fn [db [_ & [force?]]]
   (if  (or (nil? @last-http-panel-push) ;; debounce this expensive client call
            (and force? (not @force-once?))
            (> (- (get-in db [:re-pollsive.core/polling :counter]) @last-http-panel-push) 60))
     (let [pp          (get db :panels)
           _ (reset! last-http-panel-push (get-in db [:re-pollsive.core/polling :counter]))
              ;;ppr         {} ;;; TEMP!
              ;; ppr         (into {} (for [[k v] pp] ;; super slow and lags out clients when panels edited
              ;;                        {k (assoc v :queries (into {} (for [[kk vv] (get v :queries)] {kk (sql-alias-replace vv)})))}))
              ;; ppm         (into {}  (for [[k v] pp] ;; super slow and lags out clients when panels edited
              ;;                         {k (materialize-values v)}))
           ;new-h       (hash pp) ;; (hash (ut/remove-underscored pp))
           _ (when force? (reset! force-once? true))
           client-name (get db :client-name)]
       (ut/tapp>> [:running :update-panels-hash-FROM-HTTP :event :expensive! "full send of all panels to server"])
       (ut/dispatch-delay 800 [::insert-alert [:v-box :children [[:box :child "Syncing panel data with RVBBIT server..."]
                                                                 [:box :child "Warming Leaf caches..."]]] 12 1 5])
       (ut/dispatch-delay 1500 [::refresh-all]) ;; repopulate the client db caches after a server reboot...
       (ut/dispatch-delay 3500 [::update-panels-hash true])
     ;;(conn/push-panels-to-server pp ppr client-name)
       (ut/tracked-dispatch
        [::wfx/push :default
         {:kind :current-panels
          :panels pp
          :everything? true
          ;:drag-body-map @db/drag-body-map
          :materialized-panels {} ;ppm
          :resolved-panels {} ;ppr
          :client-name client-name}])
     ;;(when (get db :buffy?) (ut/dispatch-delay 2000 [::refresh-history-log]))
       ;(assoc db :panels-hash new-h)
       db) db)))

;;  (ut/tracked-dispatch [::update-panels-hash])

(defn options [x]
  (let [protocol          "ws"
        host              (.-host js/window.location)
        host-without-port (cstr/replace host #":\d+$" "")
        ws-port           "3030"
        url               (str protocol "://" host-without-port ":" ws-port "/ws")]
    (ut/tapp>> [:http-ws-connect-url! url])
    {:url           url
     :format        :edn ;;:transit-json
     :on-disconnect [::dispatch-unsubscriptions]
     :on-connect    [::dispatch-subscriptions x]}))

(defn options-secondary [socket-id]
  (let [protocol          "ws"
        host              (.-host js/window.location)
        host-without-port (cstr/replace host #":\d+$" "")
        ws-port           "3030"
        url               (str protocol "://" host-without-port ":" ws-port "/ws")]
    (ut/pp [:http-ws-connect-url! socket-id url])
    {:url           url
     :format        :edn
     ;:on-disconnect [::dispatch-unsubscriptions]
     ;:on-connect    [::dispatch-subscriptions :default]
     }))

(re-frame/reg-event-fx
 ::dispatch-subscriptions
 (fn [_ [_ x]]
   (reset! db/last-mouse-activity (js/Date.)) ;; just to juice the mouse timer
   (reset! db/running-queries #{})
   {:dispatch-n [[::wfx/subscribe socket-id :server-push2 (subscription x :server-push2)]
                 [::update-panels-hash] ;; expensive to re-pop the server data for client panels
                 [::wfx/request :default
                  {:message     {:kind :get-settings :client-name x}
                   :on-response [::simple-response-boot-no-load]
                   :on-timeout  [::timeout-response [:boot :get-settings]]
                   :timeout     15000}]]}))

(re-frame/reg-event-fx
 ::dispatch-unsubscriptions
 (fn [_ _]
   {:dispatch-n [[::wfx/unsubscribe socket-id :server-push2]
                 ;;[::remove-ai-worker-thread-refs]
                 [::remove-all-running?-params]]}))



(def server-http-port 8888)
(def url-base (str (cstr/join ":" (drop-last (ut/splitter (.. js/document -location -href) #":"))) ":" server-http-port)) ;; no trailing slash

(re-frame/reg-event-db
 ::remove-ai-worker-thread-refs
 (fn [db _]
   (ut/dissoc-in db [:click-param :ai-worker])))

(re-frame/reg-event-db
 ::remove-all-running?-params
 (fn [db _]
   (let [click-param (get db :click-param)
         keys-to-remove (filter #(cstr/ends-with? (str %) "running?]") (ut/keypaths click-param))]
     ;;(ut/tapp>> [:removing-running?-keys keys-to-remove])
     (reduce (fn [acc path]
               (ut/dissoc-in acc (into [:click-param] path)))
             db
             keys-to-remove))))

(re-frame/reg-sub
 ::url-vec
 (fn [_ _]
   (let [url-str (.. js/document -location -href)
         url-vec (if (and (cstr/includes? url-str "/#/") (> (count (ut/splitter url-str "/#/")) 1))
                   (ut/splitter (last (ut/splitter url-str "/#/")) "/")
                   [])]
     url-vec)))

(defn start-listening-to-url-changes []
  (let [on-hashchange (fn [_]
                        (let [new-url (-> js/window
                                          .-location
                                          .-hash)]
                          (ut/tracked-dispatch [::url-changed new-url])))]
    (.addEventListener js/window "hashchange" on-hashchange)))

(defn change-url [new-hash]
  (let [new-url (str "/#" new-hash)]
    (.pushState js/history nil "" new-url)
    (.replaceState js/history nil "" new-url)))

(re-frame/reg-event-db
 ::url-changed
 (fn [db [_ new-url]]
   (let [url-vec  (if (and (cstr/includes? new-url "#") (> (count (ut/splitter new-url "#")) 1))
                    (ut/splitter (last (ut/splitter new-url "#")) "/")
                    [])
         base-dir "./screens/"
         screen   (str base-dir (last url-vec) ".edn")]
     (ut/tapp>> [:url-changed new-url url-vec :loading-new-screen])
     (ut/tracked-dispatch [::load screen])
     (assoc db :current-url new-url))))

(re-frame/reg-event-db
 ::simple-response-boot
 (fn [db [_ result]]
   (let [screens        (get result :screens)
         url            @(ut/tracked-subscribe [::url-vec])
         default-screen (get result :default)]
     (ut/tapp>> [:screens screens :url-vec url])
     (when default-screen ;; load default canvas settings
       (ut/tracked-dispatch [::load default-screen]))
     (if false ;default-screen
       (-> db
           (assoc :screen-name (str (get db :screen-name) (rand-int 12345))))
       (assoc-in db [:server :settings] result)))))

(re-frame/reg-event-db
 ::simple-response-boot-no-load
 (fn [db [_ result]]
   (let [] ;screens (get result :screens)
     (if false ;default-screen
       (-> db
           (assoc :screen-name (str (get db :screen-name) (rand-int 12345))))
       (assoc-in db [:server :settings] result)))))

(re-frame/reg-event-db
 ::value-response-flow
 (fn [db [_ result]]
   (let [base-key (get result :base-key)
         sub-key  (get result :sub-key) ;; including .step
         value    (get result :value)]
     (assoc-in db [:click-param base-key sub-key] value))))


(re-frame/reg-event-db
 ::autocomplete-response
 (fn [db [_ result]]
   (let [;;codes [:theme/base-font :font-family :height]
         server-params (get result :clover-params [])
         view-codes    (get result :view-keywords [])
         flow-subs     (get db :flow-subs)
        ;;  data-keys     (vec (for [e (map last (filter #(= (count %) 3) (ut/keypaths (get db :panels))))]
        ;;                       (keyword (str "data/" (cstr/replace (str e) ":" "")))))
         data-keys     (vec (for [e (map last (remove empty? (filter #(and (not (number? (last %))) (= (count %) 3)) (ut/kvpaths (get db :panels)))))]
                              (keyword (str "data/" (cstr/replace (str e) ":" "")))))
         click-params  (vec (for [e (keys (get-in db [:click-param :param]))]
                              (keyword (str "param/" (ut/replacer (str e) ":" "")))))
         themes        (vec (for [e (keys (get-in db [:click-param :theme]))]
                              (keyword (str "theme/" (ut/replacer (str e) ":" "")))))
         codes         (vec (apply concat [server-params view-codes themes flow-subs click-params data-keys]))]
     (reset! db/autocomplete-keywords (set (map str codes)))
     db)))


(re-frame/reg-event-db
 ::timeout-response
 (fn [db [_ result what-req]]
   (let [client-name (get db :client-name)]
     (ut/tapp>> [:websocket-timeout! client-name result what-req])
     db)))


(re-frame/reg-event-db
 ::get-autocomplete-values
 (fn [db _]
   (let [client-name (get db :client-name)]
     (ut/tracked-dispatch
      [::wfx/request :default
       {:message {:kind :autocomplete :surrounding nil :panel-key nil :view nil :client-name client-name}
        :on-response [::autocomplete-response]
        :on-timeout [::timeout-response ::autocomplete]
        :timeout 15000000}]))
   db))

;; (re-frame/reg-event-db ::sub-to-flow-value
;;                        (fn [db [_ key]]
;;                          (let [client-name (get db :client-name)]
;;                            (ut/tracked-dispatch
;;                             [::wfx/request :default
;;                              {:message     {:kind :sub-to-flow-value :flow-key key :client-name client-name}
;;                               :on-response [::value-response-flow]
;;                               :on-timeout  [::timeout-response :get-flow-value key]
;;                               :timeout     15000000}]))
;;                          db))

(defn sub-alert [key]
  (let [un? (cstr/includes? (str key) "un-sub")]
    [:h-box
     :align :center
     :style {:padding-left "6px"}
     :gap "10px"
     :children [[:md-icon
                 :style {:font-size "12px"}
                 :md-icon-name (if un?
                                 "ri-skull-fill"
                                 "ri-shake-hands-fill")]
                [:box :child (str key)]]
     :style {:font-size "12px"
             :opacity (when un? 0.6)
             :text-decoration (when un? "line-through")}]))

;(ut/tracked-dispatch [::insert-alert (sub-alert (str "sub to thing")) 10 0.75 5])
;(ut/tracked-dispatch [::insert-alert (sub-alert (str "un-sub to thing")) 10 0.75 5])

(re-frame/reg-event-db
 ::sub-to-flow-value
 (fn [db [_ key]]
   (let [client-name (get db :client-name)
         hide-sub-alerts? (get-in db [:server :settings :hide-sub-and-unsub-alerts?] false)]
     (when (not hide-sub-alerts?)
       (ut/tracked-dispatch [::insert-alert (sub-alert (str "sub to " key)) 10 0.75 5]))
     (ut/tracked-dispatch
      [::wfx/push :default
       {:kind :sub-to-flow-value
        :flow-key key
        :client-name client-name}]))
   db))

(re-frame/reg-event-db
 ::unsub-to-flow-value
 (fn [db [_ key]]
   (let [client-name (get db :client-name)
         hide-sub-alerts? (get-in db [:server :settings :hide-sub-and-unsub-alerts?] false)]
     (when (not hide-sub-alerts?)
       (ut/tracked-dispatch [::insert-alert (sub-alert (str "un-sub to " key)) 10 0.75 5]))
     (ut/tracked-dispatch
      [::wfx/push :default
       {:kind :unsub-to-flow-value
        :flow-key key
        :client-name client-name}]))
   db))

(defn update-context-boxes
  [result task-id ms reco-count]
  (when (and ;(= (first (get result :ui-keypath)) :reeco-status)
         false ;; disable for now
         (= task-id :reco)
         (= (get result :status) :done))

    ;; (swap! db/context-box assoc
    ;;   (last (get result :ui-keypath))
    ;;   (str "shapes: found " (ut/nf reco-count) " viz suggestions in ~" ms " secs"))
    (ut/tracked-dispatch [::insert-alert [:box
                                          :style {:font-size "18px"}
                                          :child (str (last (get result :ui-keypath)) ": viz shapes: found " (ut/nf reco-count) " viz suggestions")] 12 nil 5]))

  (when (and (= task-id :outliers) ;;(not (= task-id :reco))
             (= (get result :status) :done))

    ;; (swap! db/context-box assoc
    ;;   (last (get result :ui-keypath))
    ;;   (str task-id " - " (ut/nf reco-count) " results in ~" ms " secs"))
    (ut/tracked-dispatch [::insert-alert (str task-id " - " (ut/nf reco-count) " results in ~" ms " secs") 10 nil 5])))

(re-frame/reg-event-db ::refresh-kits (fn [db [_]] (ut/dissoc-in db [:query-history :kit-results-sys])))

(def last-hash (reagent/atom []))

(re-frame/reg-event-db
 ::insert-alert
 (fn [db [_ content w h duration & [alert-id]]]
                         ;;(ut/tapp>> [:insert-alert content w h duration] )
   (let [ticktock (get-in db [:re-pollsive.core/polling :counter])
         alert-id (or alert-id (hash content))
         duration (or duration 30)]
     (if (some #(= % (str content)) (for [[cc _ _ _ _] (get db :alerts [])] (str cc)))
       db ;; no dupes now
       (assoc db :alerts (vec (conj (get db :alerts [])
                                    [[:box
                                      :style {:font-size "15px"}
                                      :child content] w h (+ ticktock duration) alert-id])))))))

(re-frame/reg-event-db
 ::runstream-item
 (fn [db [_ result]]
   (-> db
       (assoc-in [:runstreams-lookups (get result :flow-id) :open-inputs] (get result :open-inputs))
       (assoc-in [:runstreams-lookups (get result :flow-id) :blocks] (get result :blocks)))))

;; (re-frame/reg-event-db
;;   ::refresh-runstreams-sly
;;   (fn [db _]
;;     ;(ut/tapp>> [:refresh-runstream-panelF (keys (get db :runstreams))])
;;     (when (ut/not-empty? (keys (get db :runstreams)))
;;       (doseq [flow-id (keys (get db :runstreams))]
;;         (ut/tracked-dispatch
;;           [::wfx/request :default
;;            {:message     {:kind :get-flow-open-ports :flow-id flow-id :flowmap flow-id :client-name (get db :client-name)}
;;             :on-response [::runstream-item]
;;             :timeout     500000}])))
;;     db))

;; (re-frame/reg-event-db
;;  ::refresh-runstreams
;;  (fn [db [_]]
;;    (dissoc db :runstreams-lookups)))

(re-frame/reg-event-db
 ::set-flow-results ;; temp from converting from atom to app-db
 (fn [db [_ data kkey & [flow-id]]]
   (cond kkey (assoc-in db (if (vector? kkey) (vec (into [:flow-results] kkey)) [:flow-results kkey]) data)
         (and (= data {:status :started}) flow-id) (-> db
                                                       (assoc :flow-results (merge (get db :flow-results)
                                                                                   data))
                                                       (ut/dissoc-in [:flow-results :return-maps flow-id])
                                                       (ut/dissoc-in [:flow-results :tracker flow-id]))
         :else (assoc db :flow-results data))))

(re-frame/reg-sub ::flow-results (fn [db _] (get db :flow-results {})))

(re-frame/reg-sub ::flows (fn [db _] (get db :flows {})))

(re-frame/reg-sub ::flow-results-tracker (fn [db [_ flow-id]] (get-in db [:flow-results :tracker flow-id] {})))

(defn accumulate-unique-runs
  [data]
  (let [merge-runs (fn [runs run]
                     (let [run-start (:start run)
                           run-end   (:end run)]
                       (cond (some #(and (= (:start %) run-start) (= (:end %) run-end)) runs) runs
                             (some #(and (= (:start %) run-start) (nil? (:end %)) run-end) runs)
                             (conj (vec (remove #(and (= (:start %) run-start) (nil? (:end %))) runs)) run)
                             :else (conj runs run))))]
    (reduce (fn [acc entry]
              (reduce (fn [inner-acc [block-id run]] (update inner-acc block-id (fn [runs] (merge-runs (or runs []) run))))
                      acc
                      (into [] entry)))
            {}
            data)))

(defn is-rowset? [data]
  (let  [all-maps (and (vector? data) (every? map? data))
         all-keys-match (and all-maps
                             (let [first-keys (set (keys (first data)))]
                               (every? #(and (= first-keys (set (keys %)))
                                             (every? keyword? (keys %)))
                                       data)))
         all-single-values-vector (and (vector? data) (every? (complement coll?) data))
         all-single-values-set (and (set? data) (every? (complement coll?) data))]
    (or all-keys-match all-single-values-set all-single-values-vector)))

(defonce packets-received (atom 0))
(defonce batches-received (atom 0))

(re-frame/reg-event-db
 ::update-sub-counts
 (fn [db [_ sub-results]]
   (let [smm (vec (for [s sub-results] (keyword (cstr/replace (cstr/join "/" (get s :task-id)) ":" ""))))
         flow-sub-cnts (get db :flow-sub-cnts)
         ;smmc (into {} (for [fk smm] (update flow-sub-cnts fk (fnil inc 0))))
         smmc (reduce (fn [acc fk] (update acc fk (fnil inc 0))) flow-sub-cnts smm)
         flow-subs (get db :flow-subs)]
     ;(ut/tapp>>  [:smmc smmc])
     (assoc db :flow-sub-cnts (select-keys smmc flow-subs)))))

(defn rowset-fit [data]
  (let [all-single-values-vector (and (vector? data) (every? (complement coll?) data))
        all-single-values-set (and (set? data) (every? (complement coll?) data))
        data (cond all-single-values-vector (mapv #(hash-map :vec-val %) data)
                   all-single-values-set (mapv #(hash-map :set-val %) data)
                   :else data)]
    data))

(re-frame/reg-sub
 ::clover-data-exists?
 (fn [db {:keys [data-key data]}]
   (let [data (rowset-fit data)
         is-flow-key (cstr/includes? (str data-key) "/")
         view-map (into {}
                        (for [[k v] @db/solver-fn-lookup]
                          {v (last k)}))
         data-key (if is-flow-key
                    (get view-map data-key)
                    data-key)]
     (= (get-in db [:data data-key]) data))))

(re-frame/reg-event-db
 ::update-rowset-metadata
 (fn [db [_ data data-key]]
   (let [rows (count data)
         meta-map (into {}
                        (for [field-name (keys (first data))
                              :let [distinct-vals (count (distinct (map #(get % field-name) data)))]]
                          {field-name {:data-type (ut/data-typer (get-in data [0 field-name]))
                                       :distinct distinct-vals
                                       :group-by? true
                                       :commons {}
                                       :cardinality (try (int (* 100 (float (/ distinct-vals rows))))
                                                         (catch :default _ 0))}}))
         full-map {:fields meta-map
                   :rowcount rows}]
     (assoc-in db [:meta data-key] full-map))))

(re-frame/reg-event-db
 ::insert-implicit-rowset
 (fn [db [_ data flow-key & [non-solver?]]]
   (let [data (rowset-fit data)
         view-map (into {}
                        (for [[k v] @db/solver-fn-lookup]
                          {v (last k)}))
         data-key (if non-solver?
                    flow-key
                    (get view-map flow-key))

         tracking-key [:implicit-rowsets (if non-solver? :clover :solver)]]
     (ut/tracked-dispatch [::update-rowset-metadata data data-key])
     (-> db
         (assoc-in tracking-key (vec (distinct (conj (get-in db tracking-key []) data-key))))
         (assoc-in [:data data-key] data)))))


;; (re-frame/reg-event-fx
;;  ::update-poller-interval
;;  (fn [{:keys [db]} [_ event-name new-interval]]
;;    (ut/tracked-dispatch [::insert-alert (str "updating poller interval for " event-name " to " new-interval " seconds") 10 nil 5])
;;    (let [current-rules (get db ::poll/rules [])
;;          updated-rules (mapv (fn [rule]
;;                                (if (= (first (:event rule)) event-name)
;;                                  (assoc rule :interval new-interval)
;;                                  rule))
;;                              current-rules)]
;;      {:dispatch [::poll/set-rules updated-rules]})))

;; (ut/tracked-dispatch [::update-poller-interval [:rvbbit-frontend.bricks/save-snap-periodically] 15])

(re-frame/reg-event-db
 ::refresh-kits
 (fn [db [_]]
   (ut/dissoc-in db [:query-history :kit-results-sys])))

;;(def valid-task-ids #{:flow :screen :time :signal :server :ext-param :solver :data :solver-status :solver-meta :kit-status :kit :repl-ns :flow-status :signal-history :panel :client})
;; moved to db/reactor-types

(re-frame/reg-event-db
 ::assoc-push-undoable
 (undoable)
 (fn [db [_ clover-map]]
   (let [kp-value-map clover-map]
     (reduce-kv
      (fn [db keypath value]
        (assoc-in db keypath value))
      db
      kp-value-map))))

(re-frame/reg-event-db
 ::simple-response
 (fn [db [_ result & [batched?]]]
    (let [;result (try (if (vector? (first result)) (vector (for [r result] (first r))) result) (catch :default _ result))
          result (try (if (get-in result [0 0 :ui-keypath]) (vec (apply concat result)) result) (catch :default _ result))
          ] ;; double nesting?
      ;; (ut/tapp>> [:simple-response-debug batched? (try
      ;;                                (let [tt (if (map? result) (get result :task-id) (mapv #(get % :task-id) result))
      ;;                                      tt (if (keyword? tt) [[tt]] tt)]
      ;;                                (str (count tt) " " (when (vector? (first tt)) (set (mapv first tt)))
      ;;                                     " " (str tt)
      ;;                                     ;; (when (or (empty? tt)
      ;;                                     ;;           (cstr/includes? (str tt) "nil"))
      ;;                                     ;;   (vec (apply concat result)))
      ;;                                     ))
      ;;                                (catch :default e [:error (str e) (map? result) (get result :task-id)])) result])


  ;;  (ut/tapp>> [:simple-response2 (str (if (map? result) (get result :status) (mapv #(get % :status) result)))])
      (if (vector? result)
      ;; (let [grouped-results (group-by #(let [ui-keypath                  (first (get % :ui-keypath))
      ;;                                        kick?                       (= ui-keypath :kick)
      ;;                                        task-id                     (get % :task-id)
      ;;                                        heartbeat?                  (= task-id :heartbeat)]
      ;;                                    (and kick?
      ;;                                         (contains? db/reactor-types (get-in % [:task-id 0]))
      ;;                                         (not heartbeat?))) result)
      ;;       result-subs (get grouped-results true)
      ;;       result (get grouped-results false)]
      ;;   (ut/tapp>> [:batch-of-messages (count result)
      ;;               (vec (for [r result] (str (get r :task-id))))])
      ;;   (swap! batches-received inc)
      ;;   (doseq [res result] (re-frame/dispatch [::simple-response res true])) ;; process the other batches
      ;;   db) ;;

      ;; take all the server subs and apply them all at once instead of feeding them through one by one..
        (let [grouped-results (group-by #(let [ui-keypath                  (first (get % :ui-keypath))
                                               kick?                       (= ui-keypath :kick)
                                               task-id                     (get % :task-id)
                                               heartbeat?                  (= task-id :heartbeat)
                                               reactor-types               (disj db/reactor-types :leaf)]
                                           (and kick?
                                                (contains? reactor-types (get-in % [:task-id 0]))
                                                (not (= task-id :cnts))
                                                (not heartbeat?))) result)
              result-subs (get grouped-results true)
              result-subs (filterv #(not (nil? (get % :status))) result-subs) ;; no point in nil updates.
              result (get grouped-results false)
            ;; _ (doseq [rr result-subs] ;; is it rowset? if so lets put it in :data also
            ;;     (let [data (get rr :status)
            ;;           all-maps (and (vector? data) (every? map? data))
            ;;           all-keys-match (and all-maps
            ;;                               (let [first-keys (set (keys (first data)))]
            ;;                                 (every? #(and (= first-keys (set (keys %)))
            ;;                                               (every? keyword? (keys %)))
            ;;                                         data)))
            ;;           all-single-values-vector (and (vector? data) (every? (complement coll?) data))
            ;;           all-single-values-set (and (set? data) (every? (complement coll?) data))]
            ;;       (when all-keys-match
            ;;         (ut/tracked-dispatch [::insert-implicit-rowset data
            ;;                               (keyword (cstr/replace (cstr/join "/" (get rr :task-id)) ":" ""))]))))
            ;; _ (doseq [rr result-subs]
            ;;     (let [data (get rr :status)
            ;;           all-single-values-vector (and (vector? data) (every? (complement coll?) data))
            ;;           all-single-values-set (and (set? data) (every? (complement coll?) data))]
            ;;       (cond
            ;;         all-single-values-vector
            ;;         (ut/tracked-dispatch [::insert-implicit-rowset (mapv #(hash-map :vec-val %) data)
            ;;                               (keyword (cstr/replace (cstr/join "/" (get rr :task-id)) ":" ""))])

            ;;         all-single-values-set
            ;;         (ut/tracked-dispatch [::insert-implicit-rowset (mapv #(hash-map :set-val %) data)
            ;;                               (keyword (cstr/replace (cstr/join "/" (get rr :task-id)) ":" ""))]))))
              _ (doseq [rr result-subs
                      ;:when (not (nil? (get rr  :status)))
] ;; populate the :data key with the rowset data if it exists
                  (let [data (get rr :status)
                      ;; all-maps (and (vector? data) (every? map? data))
                      ;; all-keys-match (and all-maps
                      ;;                     (let [first-keys (set (keys (first data)))]
                      ;;                       (every? #(and (= first-keys (set (keys %)))
                      ;;                                     (every? keyword? (keys %)))
                      ;;                               data)))
                        data-rowset? (is-rowset? data)
                      ;all-single-values-vector (and (vector? data) (every? (complement coll?) data))
                      ;all-single-values-set (and (set? data) (every? (complement coll?) data))
                        flow-key (keyword (cstr/replace (cstr/join "/" (get rr :task-id)) ":" ""))]

                    (when (and data-rowset?
                               (not @(ut/tracked-sub ::clover-data-exists? {:data-key flow-key :data data})))
                      (ut/tracked-dispatch [::insert-implicit-rowset data flow-key]))

                  ;; (cond
                  ;;   all-keys-match
                  ;;   (ut/tracked-dispatch [::insert-implicit-rowset data
                  ;;                         (keyword (cstr/replace (cstr/join "/" (get rr :task-id)) ":" ""))])

                  ;;   all-single-values-vector
                  ;;   (ut/tracked-dispatch [::insert-implicit-rowset (mapv #(hash-map :vec-val %) data)
                  ;;                         (keyword (cstr/replace (cstr/join "/" (get rr :task-id)) ":" ""))])

                  ;;   all-single-values-set
                  ;;   (ut/tracked-dispatch [::insert-implicit-rowset (mapv #(hash-map :set-val %) data)
                  ;;                         (keyword (cstr/replace (cstr/join "/" (get rr :task-id)) ":" ""))]))
                    ))
              updates (reduce (fn [acc res]
                                (let [task-id (get res :task-id)]

                                  (when (and (= (first task-id) :leaf)
                                             (cstr/ends-with? (last task-id) "all-actions"))
                                  (ut/pp [:task-id :bactched-update (str task-id) (str (get res :status))]))

                                  (assoc-in acc (vec (cons :click-param task-id)) (get res :status))))
                              {} result-subs)]

        ;; (when (cstr/includes? (str (get db :client-name)) "beaming")
        ;;   (doseq [r result-subs]
        ;;     (ut/tapp>> [:grouped-update (str (get r :task-id)) (str (get r :status)) (str r)])))

        ;; (ut/tapp>> [;:batch-of-messages (count result)
        ;;             :grouped-update (count result-subs)
        ;;             (str (vec (for [r result] (str (get r :task-id)))))
        ;;             (str (vec (for [r result-subs] (str (get r :task-id)))))])

          (re-frame/dispatch [::update-sub-counts result-subs])
          (swap! batches-received inc)
          (doseq [res result]
            (re-frame/dispatch [::simple-response res true])) ;; process the other batches as a side-effect
          (reduce-kv (fn [db k v] (update db k #(merge-with merge % v))) db updates))


        (try
          (let [ui-keypath                  (first (get result :ui-keypath))
                client-name                 (get db :client-name)
                ms                          (try (js/Math.ceil (/ (get result :elapsed-ms) 1000)) (catch :default _ nil))
                file-push?                  (= ui-keypath :file-changed)
                external-enabled?           (get db :external? false)
                task-id                     (get result :task-id)
                elapsed-ms                  (get result :elapsed-ms)
                reco-count                  (get result :reco-count)
                kick?                       (= ui-keypath :kick)
                new-thread?                 (= ui-keypath :new-conversation)
                counts?                     (= task-id :cnts)
                heartbeat?                  (or (= task-id :heartbeat) (= task-id [:heartbeat]))
                new-slice?                  (= task-id :new-slice)
                push-query?                 (= task-id :push-query)
                kit-view?                   (= task-id :kit-view)
                kit-view-opts?              (= task-id :kit-view-opts)
                kit-view-remove?            (= task-id :kit-view-remove)
                signals-file?               (= task-id :signals-file)
                solvers-file?               (= task-id :solvers-file)
                push-assocs?                (= task-id :push-assocs)
                materialized-pct?           (= task-id :materialized-pct)
                alert?                      (cstr/starts-with? (str task-id) ":alert")
                server-sub?                 (and kick? ;; likely already batched and applied above, but just in case somehow
                                                 (contains? db/reactor-types (get-in result [:task-id 0]))
                                                 (not heartbeat?))
                runstream-sub?              (and kick? (= (get-in result [:task-id 0]) :runstream)
                                                 (not (cstr/includes? (str (get-in result [:task-id 1])) ">")) ;; its a base call, not a reactor kp
                                                 (not heartbeat?))
                tracker?                    (and kick? (= (get-in result [:task-id 0]) :tracker) (not heartbeat?))
                flow-runner-sub?            (and kick? (= (get-in result [:task-id 0]) :flow-runner) (not heartbeat?))
                settings-update?            (and kick? (= (get-in result [:task-id 0]) :settings) (not heartbeat?))
                flow-runner-tracker-blocks? (and kick? (= (get-in result [:task-id 0]) :tracker-blocks) (not heartbeat?))
                flow-runner-acc-tracker?    (and kick? (= (get-in result [:task-id 0]) :acc-tracker) (not heartbeat?))
                condi-tracker?              (and kick? (= (get-in result [:task-id 0]) :condis) (not heartbeat?))
                estimate?                   (and kick? (= (get-in result [:task-id 0]) :estimate) (not heartbeat?))]

            (when settings-update? (ut/tapp>> [:settings-update (get result :status)]))

            ;; (when (and alert? (or (cstr/includes? (cstr/lower-case (str result)) "fabric ") ;; gross. TODO
            ;;                       (cstr/includes? (cstr/lower-case (str result)) "kit ")))
            ;;   (ut/tracked-dispatch [::refresh-kits]))

            ;; (when alert? (ut/tapp>> [:ALERT result]))

            ;; (when heartbeat? (ut/tapp>> [:heartbeat-in result]))

          ;; (when (and (cstr/includes? (str client-name) "bronze")  (not runstream-sub?))
          ;;   (ut/tapp>> [:msg-in
          ;;               file-push? (not (nil? (get-in result [:data 0 :panel-key]))) external-enabled?
          ;;               (str task-id) (get result :status)  (str ui-keypath)
          ;;               ;(str (get-in result [:status :evald-result :value 0]))
          ;;               (str (get result :ui-keypath)) (str result)]))

            ;; (when (not batched?)
            ;;   (ut/tapp>> [:single server-sub? (str (get result :task-id)) result]))

            (when push-assocs? (ut/pp [:push-assoc! (str (keys (get result :status))) (get result :status)]))

            (when new-thread? (ut/tapp>> [:new-thread result (get result :status)]))

            (swap! packets-received inc)

            (when heartbeat? ;; test
              (let [;server-panels (set (get db :server-panels))
                    ;client-panels (set (remove nil? (filter #(not (cstr/starts-with? (str %) ":reco")) (keys (get db :panels)))))
                    ;diffy (cset/difference  client-panels server-panels)
                    ;not-in-sync? (true? (> (count diffy) 0))
                    ]
                (ut/tapp>> [:heart-beat-ack! :tab (get db :selected-tab)])

                ;; (when not-in-sync?
                ;;   (ut/pp [:panels-out-of-sync-with-server! :resolving... (str diffy)])
                ;;   (ut/tracked-dispatch [::wfx/push :default ;:secondary
                ;;                         {:kind :current-panels
                ;;                          :panels   (select-keys (get db :panels) (vec diffy))
                ;;                          ;:timeout  500000
                ;;                          :materialized-panels {} ;; ppm
                ;;                          :resolved-panels {} ;; ppr
                ;;                          :drag-body-map {} ;;drag-body-map-this-tab
                ;;                          :client-name client-name}]))
                (ut/tracked-dispatch
                 [::wfx/push :default
                  {:kind        :ack
                   ;:in-sync? (not not-in-sync?)
                   :selected-tab (get db :selected-tab)
                   :screen-name (get db :screen-name)
                   :memory      (let [mem     (when (exists? js/window.performance.memory)
                                                [(.-totalJSHeapSize js/window.performance.memory)
                                                 (.-usedJSHeapSize js/window.performance.memory)
                                                 (.-jsHeapSizeLimit js/window.performance.memory)])
                                      mem-row {:mem_time    (str (.toISOString (js/Date.)))
                                               :mem_total   (first mem)
                                               :packets     @packets-received
                                               :batches     @batches-received
                                               :mem_used    (second mem)
                                               :selected-tab (get db :selected-tab)
                                               :screen-name (get db :screen-name)
                                               :client-name (str client-name)
                                               :mem_limit   (last mem)}]
                                  mem-row)
                   :flow-subs   (get db :flow-subs)
                   :client-name (get db :client-name)}])))




            (when (and alert?
                       (not (get db :alert-mute?)))
            ;; ^^ if they are muted, just ignore (else they create a back log, when reenabled Chrome shits)
              (let [cnt      (get-in result [:data 0 0])
                    fstr     (str cnt)
                    w        (get-in result [:data 0 1] (/ (count fstr) 3.9))
                    h        (get-in result [:data 0 2] 1)
                    duration (get-in result [:data 0 3] 6)]
                (ut/dispatch-delay 400 [::insert-alert [:h-box :children [cnt]] w h duration])))

            (when (or (= task-id :reco)
                      (= task-id :outliers)) ;; kinda deprecated, won't need
              (update-context-boxes result task-id ms reco-count))

            (cond

            ;; push-assocs? (let [kp-value-map (get result :status)]
            ;;                (reduce-kv
            ;;                 (fn [db keypath value]
            ;;                   (assoc-in db keypath value))
            ;;                 db
            ;;                 kp-value-map))

              push-assocs? (do ;; using sep event because we want it to be specifically undoable, but NOT other streams here. hack.
                             (ut/tracked-dispatch [::assoc-push-undoable (get result :status)])
                             db)

              alert? db ;; do nothing, alerts are a handled side-effect event ^^^

              new-thread? (-> db
                              (assoc :selected-ai-worker (first (get result :status)))
                              (assoc :selected-ai-worker-thread (last (get result :status))))



              materialized-pct? (assoc-in db [:mat-pct ui-keypath] (get result :status))

              (and file-push? (not (nil? (get-in result [:data 0 :panel-key]))) external-enabled?)
              (assoc-in db [:panels (get-in result [:data 0 :panel-key])] (get-in result [:data 0 :block-data]))

              runstream-sub? (let [dd (get result :status)]
                               (-> db
                                   (assoc-in [:runstreams-lookups (get dd :flow-id) :open-inputs] (get dd :open-inputs))
                                   (assoc-in [:runstreams-lookups (get dd :flow-id) :blocks] (get dd :blocks))))

              signals-file? (assoc db :signals-map (get result :status))

              solvers-file? (assoc db :solvers-map (get result :status))

              kit-view? (-> db
                            (assoc-in [:panels (get-in result [:ui-keypath 1]) :views :_kvw1]
                                      (get-in result [:status :evald-result :value 0]))
                            (assoc-in [:panels (get-in result [:ui-keypath 1]) :selected-view]
                                      :_kvw1))

              push-query? (let [panel-key (get-in result [:ui-keypath 1])
                                q-name (ut/safe-key (get-in result [:ui-keypath 2]))]
                            (ut/tapp>> [:push-query q-name panel-key (get-in result [:status])])
                            (-> db
                                (assoc-in [:panels panel-key :queries q-name] (get-in result [:status]))
                                (assoc-in [:panels panel-key :selected-view] q-name)))

              new-slice?    (let [kp [:panels (get-in result [:ui-keypath 0]) (get-in result [:status 0]) (get-in result [:status 1])]
                                  query-map (get-in result [:status 2])]
                            ;(ut/tapp>> [:new-slice kp (get-in result [:status 2])])
                              (-> db
                                  (assoc-in [:panels (get-in result [:ui-keypath 0]) :selected-view] (get-in result [:status 1]))
                                  (assoc-in kp query-map)))

              kit-view-opts? (update-in db [:click-param (get-in result [:ui-keypath 1])]
                                        #(merge % (get result :status)))

              kit-view-remove? (-> db
                                   (ut/dissoc-in  [:panels (get-in result [:ui-keypath 1]) :views :_kvw1])
                                   (ut/dissoc-in  [:click-param (get-in result [:ui-keypath 1]) :go!]))

              server-sub?
            ;;(assoc-in db (vec (cons :click-param task-id)) (get result :status))
              (let [flow-key (keyword (cstr/replace (cstr/join "/" task-id) ":" ""))]
                (-> db
                    (assoc-in [:flow-sub-cnts flow-key] (inc (get-in db [:flow-sub-cnts flow-key] 0)))
                    (assoc-in (vec (cons :click-param task-id)) (get result :status))))

              settings-update? (assoc-in db [:server :settings] (get result :status))

              counts? (let [;emeta-map (get-in db [:meta ui-keypath])
                            ss              (get result :status)
                            post-meta-shape (into {} (for [[k v] ss] {k {(if (= k :*) :rowcount :distinct) v}}))
                            _ (ut/pp [:counts-incoming (get ss :*) ui-keypath ss post-meta-shape])]
                        (-> db
                            (assoc-in [:post-meta ui-keypath] post-meta-shape)))



              condi-tracker? (assoc-in db [:flow-results :condis (get-in result [:task-id 1])] (get result :status))

              flow-runner-tracker-blocks? (let [block-keys      (keys (get-in db [:flows (get db :selected-flow) :map]))
                                                flow-id         (get-in result [:task-id 1])
                                                filtered-blocks (into {}
                                                                      (for [[k v] (get result :status)]
                                                                        {k (vec (cset/intersection (set block-keys)
                                                                                                   (set v)))}))]
                                            (assoc-in db [:flow-results :tracker-blocks flow-id] filtered-blocks))

              flow-runner-acc-tracker? (let [block-keys (keys (get-in db [:flows (get db :selected-flow) :map]))
                                             flow-id    (get-in result [:task-id 1])
                                             trackers   (select-keys (get result :status) block-keys)]
                                         (-> db
                                             (ut/dissoc-in (if (or (empty? trackers) (= (count (keys trackers)) 1))
                                                             [:flow-results :return-maps flow-id]
                                                             [:skip-me :yo :yo]))
                                             (assoc-in [:flow-results :tracker flow-id] trackers)))

              estimate? (assoc db :flow-estimates (merge (get db :flow-estimates) (get result :status)))

              flow-runner-sub? (let [rtn         (get result :status)
                                     mps-key     (vec (ut/postwalk-replacer {:flow-runner :return-maps} task-id))
                                     return-maps (get-in db [:flow-results :return-maps] {})]
                                 (if (= rtn :started) ;; TODO, this eliminates the nice animations, but ensures data delivery per block.
                                   (assoc-in db task-id rtn)
                                   (-> db
                                       (assoc-in [:flow-results :return-maps] ;; (vec (into [:flow-results]
                                                 (assoc-in return-maps (vec (drop 1 mps-key)) rtn)) ;; keeping
                                       (assoc-in task-id rtn))))

              (and tracker? (get-in result [:status :start]))
              (let [rtn         (get result :status)
                    partial-flow-key (cstr/replace (str (last (get result :task-id))) ":" "")
                    [flow-id step-id] (cstr/split partial-flow-key #">")
                    step-id (keyword step-id)]
                (ut/tapp>> [:tracker-blocks flow-id step-id rtn])
                (assoc-in db [:flow-results :tracker flow-id step-id]
                          rtn))

              tracker? (let [rtn         (get result :status)
                             partial-flow-key (cstr/replace (str (last (get result :task-id))) ":" "")
                             [flow-id step-id] (cstr/split partial-flow-key #">")
                             step-id (keyword step-id)
                           ;mps-key     (vec (ut/postwalk-replacer {:flow-runner :return-maps} task-id))
                           ;return-maps (get-in db [:flow-results :return-maps] {})
                             ]
                       ;(ut/tapp>> [:tracker-update flow-id step-id rtn])
                         (if (= rtn :started) ;; TODO, this eliminates the nice animations, but ensures data delivery per block.
                           (assoc-in db task-id rtn)
                           (-> db
                               (assoc-in [:flow-results :return-maps flow-id step-id] rtn) ;; keeping
                             ;(assoc-in [:flow-results :tracker flow-id step-id] rtn)
                               )))

              heartbeat? (-> db
                             (assoc-in [:status task-id ui-keypath] (get-in result [:status :subs]))
                             (assoc :server-panels (get-in result [:status :panel-keys]))
                             (assoc-in [:status-data task-id ui-keypath]
                                       {:data (get result :data) :elapsed-ms elapsed-ms :reco-count reco-count})
                             (assoc :flow-subs (get-in result [:status :subs]))
                             ;(assoc :valid-grid-actions (get-in result [:status :grid-actions]))
                             ;(assoc :valid-kits (get-in result [:status :kits]))
                             )

              :else (-> db
                        (assoc-in [:status task-id ui-keypath] (get result :status))
                        (assoc-in [:status-data task-id ui-keypath]
                                  {:data (get result :data) :elapsed-ms elapsed-ms :reco-count reco-count})
                        (ut/dissoc-in [:query-history :recos-sys])
                        (ut/dissoc-in [:query-history :viz-shapes-sys])
                        (ut/dissoc-in [:query-history :viz-shapes0-sys])
                        (ut/dissoc-in [:query-history :viz-tables-sys]))))
          (catch :default e (do (ut/tapp>> [:simple-response-error! (str e)]) db)))))))

(re-frame/reg-event-db ::status-response
                       (fn [db [_ result]]
                         (let [;fixedmap (into {} (for [[k [v1 v2]] result] {k {(first v1) v2}}))
                               data      (get result :data)
                               statuses  (get result :statuses)
                               fixedmap  (into {} (for [[k v] statuses] {k (into {} (map (fn [[kk vv]] {(first kk) vv}) v))}))
                               fixeddata (into {} (for [[k v] data] {k (into {} (map (fn [[kk vv]] {(first kk) vv}) v))}))]
                           (-> db
                               (assoc :status fixedmap)
                               (assoc :status-data fixeddata)))))

(re-frame/reg-event-db ::timeout-response
                       (fn [db [_ result what-req]]
                         (let [client-name (get db :client-name)]
                           (ut/tapp>> [:websocket-timeout! client-name result what-req])
                           db)))

(re-frame/reg-event-db ::socket-response
                       (fn [db [_ result]]
                         (let [ui-keypath      (get result :ui-keypath)
                               sql-str         (get result :sql-str) ; (first (get result :sql-str))
                               repl-output     (get result :repl-output)
                               map-order       (get result :map-order)
                               new-map         (try (vec (for [r (get result :result)] (ut/asort r map-order)))
                                                    (catch :default _ (get result :result)))
                               res-meta        (get result :result-meta)
                               res-meta-fields (try (ut/asort (get res-meta :fields) map-order)
                                                    (catch :default _ (get res-meta :fields)))
                               meta            (merge res-meta {:fields res-meta-fields})
                               new-map         (if (= new-map '(1)) [{:sql :error :recvd (:result result)}] new-map)]
                           (swap! db/running-queries disj (last ui-keypath))
                           (if (not (nil? map-order)) ;; indicates bigger problem, too risky
                             (if (and (not (nil? repl-output)) (ut/ne? repl-output))
                               (-> db
                                   (assoc-in (cons :repl-output ui-keypath) repl-output)
                                   (assoc-in (cons :sql-str ui-keypath) sql-str)
                                   (assoc-in (cons :data ui-keypath) new-map)
                                   (assoc-in (cons :orders ui-keypath) map-order)
                                   (assoc-in (cons :meta ui-keypath) meta))
                               (-> db
                                   (assoc-in (cons :sql-str ui-keypath) sql-str)
                                   (assoc-in (cons :data ui-keypath) new-map)
                                   (assoc-in (cons :orders ui-keypath) map-order)
                                   (assoc-in (cons :meta ui-keypath) meta)))
                             db))))

(re-frame/reg-event-db ::socket-response-post-meta
                       (fn [db [_ result]]
                         (let [ui-keypath (get result :ui-keypath)
                               new-map    (first (vals (first (:result result))))
                               new-map    (if (= new-map '(1)) [{:sql :error :recvd (:result result)}] new-map)]
                           (assoc-in db (cons :post-meta ui-keypath) new-map))))

(re-frame/reg-event-db ::socket-response-post-style
                       (fn [db [_ style result]]
                         (let [ui-keypath (drop-last (get result :ui-keypath))]
                           (-> db
                               (assoc-in (conj (vec (cons :post-styles ui-keypath)) :results) (get result :result))
                               (assoc-in (conj (vec (cons :post-styles ui-keypath)) :styles) style)))))

(re-frame/reg-event-db ::socket-response-post-tab
                       (fn [db [_ panel-key result]]
                         (let [ui-keypath (get result :ui-keypath)]
                           (assoc-in db ;(conj
                                     (vec (cons :post-tab ui-keypath))
                                     (get result :result)))))


(re-frame/reg-event-db ::socket-response-post-condi
                       (fn [db [_ result]]
                         (let [ui-keypath (get result :ui-keypath)]
                           (assoc-in db ;(conj
                                     (vec (cons :post-condi ui-keypath))
                                     (get result :result)))))

(re-frame/reg-sub ::websocket-subs
                  (fn [db _]
                    (let [subs     (vec (vals (get-in db [:websocket-fx.core/sockets socket-id :subscriptions]))) ;;; trying
                          subs1    (vec (for [s subs] (get-in s [:message :kind])))]
                      subs1)))

(re-frame/reg-sub ::websocket-status
                  (fn [db _]
                    (let [;subs (vec (doall
                          subs     (vec (vals (get-in db [:websocket-fx.core/sockets socket-id :subscriptions]))) ;;; trying
                          subs1    (vec (for [s subs] (get-in s [:message :kind])))
                          datas    (count (keys (get db :data)))
                          panels   (count (keys (get db :panels)))
                          pendings (vals (get-in db [:websocket-fx.core/sockets socket-id :requests]))]
                      {;;:status @(ut/tracked-subscribe [::wfx/status socket-id])
                       :status    (get-in db [:websocket-fx.core/sockets socket-id :status]) ;;; trying to not
                       :waiting   (count pendings)
                       :datasets  datas
                       :panels    panels
                       :open-subs subs1
                       :subs      subs})))

(re-frame/reg-event-db ::failure-http-save-flowset
                       (fn [db [_ result]]
                         (let [old-status (get-in db [:http-reqs :save-flowset])]
                           (ut/tapp>> [:failure-http-save-flowset result])
                           (assoc-in db
                                     [:http-reqs :save-flowset] ; comp key from ::get-http-data
                                     (merge old-status {:status "failed" :ended-unix (.getTime (js/Date.)) :message result})))))

(re-frame/reg-event-db ::success-http-save-flowset
                       (fn [db [_ result]]
                         (let [old-status (get-in db [:http-reqs :save-flowset])]
                           (assoc-in db
                                     [:http-reqs :save-flowset] ; comp key from ::get-http-data
                                     (merge old-status
                                            {:keys       (count result)
                                             :result     (select-keys result [:status :flowset])
                                             :ended-unix (.getTime (js/Date.))
                                             :status     "success"})))))

(defn find-bogus-keywords [m] (filter (fn [[k v]] (re-find #"[^\w?-]" (name k))) m))

(re-frame/reg-event-fx
 ::save
 (fn [{:keys [db]} [_ save-type screen-name resolved-queries materialized-theme]]
   (let [method      :post
         url         (str url-base "/save")
         pp          (doall (for [r (filter #(cstr/starts-with? (str %) ":reco-preview") (keys (get-in db [:panels])))] r))
         pl          (keys (get db :panels))
         client-name (get db :client-name)
         p0          (cset/difference (set pl) (set pp))
         base-keys   (filter (comp not namespace) (remove nil? (keys db)))
         _ (ut/tapp>> [:is? base-keys])
          ;; temp-sub-click-params (filter
          ;;                        #(or
          ;;                          (not (cstr/starts-with? (str %) ":solver-meta/"))
          ;;                          (not (cstr/starts-with? (str %) ":signal/"))
          ;;                          (and (not (cstr/starts-with? (str %) ":solver/"))
          ;;                               (re-matches #".*\d" (str %)))
          ;;                          (not (cstr/ends-with? (str %) "*running?")))
          ;;                        (keys (get db :click-params)))
          ;;flow-subs-child-keys (for [e (get db :flow-subs)]
          ;;                       (remove nil? (try (keyword (last (cstr/split (str e) #"/"))) (catch :default _ nil))))
         image       (-> db ;; dehydrated... as it were
                         (select-keys base-keys)
                         (dissoc :query-history)
                         (dissoc :query-history-condi)
                         (dissoc :query-history-meta)
                         (dissoc :flow-results)
                         (dissoc :webcam-feed)
                         (dissoc :rules-map)
                         (dissoc :leaf-actions)
                         (dissoc :sessions)
                         (dissoc :status-data)
                         (dissoc :solvers-map)
                         (dissoc :status)
                         (dissoc :flow-statuses)
                         (dissoc :sub-flow-incoming)
                         (dissoc :flow-subs)
                         (dissoc :flow-runner)
                         (dissoc :flow-sub-cnts)
                         (dissoc :signals-map)
                         (dissoc :repl-output)
                         (dissoc :solver-fn)
                         (dissoc :server)
                          ;(ut/dissoc-in [:runstreams-lookups])
                         (ut/dissoc-in [:solver-fn :runs])
                         (ut/dissoc-in [:click-param :signal-history])
                         (ut/dissoc-in [:click-param :solver-meta])
                         (ut/dissoc-in [:click-param :repl-ns])
                         (ut/dissoc-in [:click-param :panel-hash])
                         (ut/dissoc-in [:click-param :solver-status])
                         (ut/dissoc-in [:click-param :solver])
                         (ut/dissoc-in [:click-param :flow-status])
                         (ut/dissoc-in [:click-param :runstream])
                         (ut/dissoc-in [:click-param :kit-status])
                         (ut/dissoc-in [:click-param :kit])
                         (ut/dissoc-in [:click-param :data])
                         (ut/dissoc-in [:click-param :signal])
                         (as-> db' (reduce #(ut/dissoc-in %1 [:click-param %2]) db' db/reactor-types))
                         (dissoc :data) ;; but NOT clover edn data TODO
                         (dissoc :flows) ;;; mostly ephemeral with the UI....
                         (dissoc :http-reqs)
                         (dissoc :orders)
                         (dissoc :solver-status)
                         (dissoc :sql-source)
                         (dissoc :shape-rotations)
                         (dissoc :base-sniff-queries)
                         (dissoc :meta)
                         (dissoc :alerts)
                         (dissoc :audio-data-recorded) ;;; this was it
                         (dissoc :sql-str)
                         (dissoc :server)
                         (ut/dissoc-in [:server :settings])
                         (dissoc :file-changed)
                         (assoc :panels (select-keys (get db :panels) p0)))
         image (ut/replace-large-base64 image) ;; sneaky foot gun.
         click-params-running (filter #(cstr/ends-with? (str %) "running?") (keys (get image :click-param)))
         image (assoc image :click-param (apply dissoc (get image :click-param) click-params-running))
         bogus-kw    (vec (find-bogus-keywords image))
         image       (apply dissoc image (map first bogus-kw))
         request     {:image (-> image
                                 (assoc :resolved-queries resolved-queries)
                                 (assoc :materialized-theme materialized-theme))
                      :client-name client-name
                      :screen-name screen-name}
         _ (ut/tapp>> [:saving screen-name "!" image :removed-bogus-keywords bogus-kw])
          ;clean-db (assoc-in db [:http-reqs :save-flowset] {:status "running" :url url :start-unix (.getTime (js/Date.))})
          ;;is-valid-edn? (ut/is-valid-edn? request) ;; TODO, invalid EDN will prevent saving...
         ]
     {:db         (assoc-in db [:http-reqs :save-flowset] {:status "running" :url url :start-unix (.getTime (js/Date.))})
      :http-xhrio {:method          method
                   :uri             url
                   :params          request
                   :timeout         28000
                   :format          (ajax-edn/edn-request-format)
                   :response-format (ajax-edn/edn-response-format)
                   :on-success      [::success-http-save-flowset]
                   :on-failure      [::failure-http-save-flowset]}})))

(re-frame/reg-event-db ::failure-http-save-flow
                       (fn [db [_ result]]
                         (let [old-status (get-in db [:http-reqs :save-flow])]
                           (assoc-in db
                                     [:http-reqs :save-flow] ; comp key from ::get-http-data
                                     (merge old-status {:status "failed" :ended-unix (.getTime (js/Date.)) :message result})))))

(re-frame/reg-event-db ::success-http-save-flow
                       (fn [db [_ result]]
                         (let [old-status (get-in db [:http-reqs :save-flow])]
                           (assoc-in db
                                     [:http-reqs :save-flow] ; comp key from ::get-http-data
                                     (merge old-status
                                            {:keys       (count result)
                                             :result     (select-keys result [:status :flowset])
                                             :ended-unix (.getTime (js/Date.))
                                             :status     "success"})))))

(re-frame/reg-event-fx
 ::save-flow
 (fn [{:keys [db]} [_ flow-body flow-name]]
   (let [method      :post
         url         (str url-base "/save-flow")
         client-name (get db :client-name)
         request     {:image flow-body :client-name client-name :flow-id flow-name}]
     (ut/tapp>> [:save-flow request])
     {:db         (assoc-in db [:http-reqs :save-flow] {:status "running" :url url :start-unix (.getTime (js/Date.))})
      :http-xhrio {:method          method
                   :uri             url
                   :params          request
                   :timeout         28000
                   :format          (ajax-edn/edn-request-format)
                   :response-format (ajax-edn/edn-response-format)
                   :on-success      [::success-http-save-flow]
                   :on-failure      [::failure-http-save-flow]}})))



(re-frame/reg-event-db ::failure-http-save-snap
                       (fn [db [_]]
                         (let [old-status (get-in db [:http-reqs :save-snap])]
                           (assoc-in db
                                     [:http-reqs :save-snap] ; comp key from ::get-http-data
                                     (merge old-status {:status "failed" :ended-unix (.getTime (js/Date.))})))))

(re-frame/reg-event-db ::success-http-save-snap
                       (fn [db [_]]
                         (let [old-status (get-in db [:http-reqs :save-snap])]
                           (assoc-in db
                                     [:http-reqs :save-snap] ; comp key from ::get-http-data
                                     (merge old-status {:ended-unix (.getTime (js/Date.)) :status "success"})))))

(re-frame/reg-event-fx
 ::save-snap
 (fn [{:keys [db]} [_ image]]
   (let [method       :post
         url          (str url-base "/save-snap")
         client-name  (get db :client-name)
         compund-keys (vec (into (for [k (keys db) :when (cstr/includes? (str k) "/")] k) [:http-reqs]))
         sess         (-> (ut/deselect-keys db compund-keys) (dissoc :client-name) (dissoc :audio-data-recorded))
         request      {:image image :session sess :client-name client-name :ttime (.getTime (js/Date.))}]
     {:db         (assoc-in db [:http-reqs :save-snap] {:status "running" :url url :start-unix (.getTime (js/Date.))})
      :http-xhrio {:method          method
                   :uri             url
                   :params          request
                   :timeout         28000
                   :format          (ajax-edn/edn-request-format)
                   :response-format (ajax-edn/edn-response-format)
                   :on-success      [::success-http-save-snap]
                   :on-failure      [::failure-http-save-snap]}})))





(re-frame/reg-event-db
 ::failure-http-save-snap-block
 (fn [db [_]]
   (let [old-status (get-in db [:http-reqs :save-snap-block])]
     (assoc-in db
               [:http-reqs :save-snap-block] ; comp key from ::get-http-data
               (merge old-status {:status "failed" :ended-unix (.getTime (js/Date.))})))))

(re-frame/reg-event-db
 ::success-http-save-snap-block
 (fn [db [_]]
   (let [old-status (get-in db [:http-reqs :save-snap-block])]
     (assoc-in db
               [:http-reqs :save-snap-block] ; comp key from ::get-http-data
               (merge old-status {:ended-unix (.getTime (js/Date.)) :status "success"})))))

(re-frame/reg-event-fx
 ::save-snap-block
 (fn [{:keys [db]} [_ image panel-key]]
   (let [method       :post
         url          (str url-base "/save-snap-block")
         client-name  (get db :client-name)
         request      {:image image :panel-key panel-key :client-name client-name :ttime (.getTime (js/Date.))}]
     {:db         (assoc-in db [:http-reqs :save-snap-block] {:status "running" :url url :start-unix (.getTime (js/Date.))})
      :http-xhrio {:method          method
                   :uri             url
                   :params          request
                   :timeout         28000
                   :format          (ajax-edn/edn-request-format)
                   :response-format (ajax-edn/edn-response-format)
                   :on-success      [::success-http-save-snap-block]
                   :on-failure      [::failure-http-save-snap-block]}})))







(re-frame/reg-event-db
 ::failure-http-save-screen-snap
 (fn [db [_]]
   (let [old-status (get-in db [:http-reqs :save-screen-snap])]
     (ut/tapp>> [:saving-screen-snap-for (get db :client-name)])
     (assoc-in db
               [:http-reqs :save-screen-snap] ; comp key from ::get-http-data
               (merge old-status {:status "failed" :ended-unix (.getTime (js/Date.))})))))

(re-frame/reg-event-db
 ::success-http-save-screen-snap
 (fn [db [_]]
   (let [old-status (get-in db [:http-reqs :save-screen-snap])]
     (assoc-in db
               [:http-reqs :save-screen-snap] ; comp key from ::get-http-data
               (merge old-status {:ended-unix (.getTime (js/Date.)) :status "success"})))))

(re-frame/reg-event-fx
 ::save-screen-snap
 (fn [{:keys [db]} [_ image]]
   (let [method      :post
         url         (str url-base "/save-screen-snap")
         screen-name (get db :screen-name)
         request     {:image image :screen-name screen-name}]
     (ut/tapp>> [:save-screen-snap! (get db :client-name) screen-name])
     {:db         (assoc-in db [:http-reqs :save-screen-snap] {:status "running" :url url :start-unix (.getTime (js/Date.))})
      :http-xhrio {:method          method
                   :uri             url
                   :params          request
                   :timeout         28000
                   :format          (ajax-edn/edn-request-format)
                   :response-format (ajax-edn/edn-response-format)
                   :on-success      [::success-http-save-screen-snap]
                   :on-failure      [::failure-http-save-screen-snap]}})))








(re-frame/reg-event-db
 ::failure-http-save-csv
 (fn [db [_ result]]
   (let [old-status (get-in db [:http-reqs :save-csv])]
     (assoc-in db
               [:http-reqs :save-csv] ; comp key from ::get-http-data
               (merge old-status {:status "failed" :ended-unix (.getTime (js/Date.)) :message result})))))

(re-frame/reg-event-db
 ::success-http-save-csv
 (fn [db [_ result]]
   (let [old-status (get-in db [:http-reqs :save-csv])]
     (assoc-in db
               [:http-reqs :save-csv] ; comp key from ::get-http-data
               (merge old-status {:keys (count result) :ended-unix (.getTime (js/Date.)) :status "success"})))))

(re-frame/reg-event-fx
 ::save-csv
 (fn [{:keys [db]} [_ fname fdata]]
   (when (cstr/ends-with? (cstr/lower-case fname) ".csv")
     (let [method  :post
           url     (str url-base "/save-csv")
           request {:image fdata :client-name (get db :client-name) :fname fname}]
       {:db         (assoc-in db [:http-reqs :save-csv] {:status "running" :url url :start-unix (.getTime (js/Date.))})
        :http-xhrio {:method          method
                     :uri             url
                     :params          request
                     :timeout         28000
                     :format          (ajax-edn/edn-request-format)
                     :response-format (ajax-edn/edn-response-format)
                     :on-success      [::success-http-save-csv]
                     :on-failure      [::failure-http-save-csv]}}))))

(re-frame/reg-event-db
 ::failure-http-load-flowset
 (undoable)
 (fn [db [_ result]]
   (let [old-status (get-in db [:http-reqs :load-flowset])]
     (assoc-in db
               [:http-reqs :load-flowset] ; comp key from ::get-http-data
               (merge old-status {:status "failed" :ended-unix (.getTime (js/Date.)) :message result})))))

(re-frame/reg-event-db
 ::success-http-load-flowset
 (undoable)
 (fn [db [_ snap? result]]
   (let [old-status (get-in db [:http-reqs :load-flowset])
         curr-screen-name (get db :screen-name)
         client-name-str (cstr/replace (str (get db :client-name)) ":" "")
         snap-history-path [:click-param :client (keyword (str "macro-undo-map>" client-name-str))]
         snap-history (get-in db snap-history-path)
         new-db     (dissoc (get result :image) :resolved-queries)
         _ (ut/tapp>> [:new-name (get new-db :screen-name) :curr curr-screen-name :snap? snap?])]
     (-> db
         (assoc-in [:http-reqs :load-flowset]
                   (merge old-status {:result result
                                      :ended-unix (.getTime (js/Date.))
                                      :status "success"}))
         (assoc :query-history (get new-db :query-history))
         (ut/dissoc-in [:query-history :blocks-sys])
         (ut/dissoc-in [:query-history :fields-sys])
         (ut/dissoc-in [:query-history :tables-sys])
         (ut/dissoc-in [:query-history :files-sys])
         (ut/dissoc-in [:query-history :connections-sys])
         (assoc :runstreams (get new-db :runstreams))
         (assoc :runstream-drops (get new-db :runstream-drops))
         (assoc :panels (get new-db :panels))
         (assoc :tabs (get new-db :tabs))
         (assoc :snapshots (get new-db :snapshots))
         (assoc :selected-tab (get new-db :selected-tab))
         (assoc :selected-block "none!")
         (assoc :meta (get new-db :meta))
         (assoc :screen-name (if snap? curr-screen-name (get new-db :screen-name)))
         (assoc :data (get new-db :data))
         (assoc :click-param (get new-db :click-param))
         (assoc-in snap-history-path snap-history)))))

(re-frame/reg-event-fx
 ::load
 (fn [{:keys [db]} [_ file-path]]
   (ut/tapp>> [:loads? file-path])
   (let [url     (str url-base "/load")
         method  :get
         request {:file-path file-path}
         snap? (cstr/includes? file-path "assets/snaps")
         _ (when snap?
             (change-url (str "/" (ut/replacer (str (last (ut/splitter file-path "/"))) #".edn" ""))))]
     {:db         (assoc-in db [:http-reqs :load-flowset] {:status "running" :url url :start-unix (.getTime (js/Date.))})
      :http-xhrio {:method          method
                   :uri             url
                   :params          request
                   :timeout         28000
                   :format          (ajax-edn/edn-request-format)
                   :response-format (ajax-edn/edn-response-format)
                   :on-success      [::success-http-load-flowset snap?]
                   :on-failure      [::failure-http-load-flowset]}})))

(re-frame/reg-event-db
 ::failure-http-load-session
 (fn [db [_ result]]
   (let [old-status (get-in db [:http-reqs :load-session])]
     (assoc-in db
               [:http-reqs :load-session] ; comp key from ::get-http-data
               (merge old-status {:status "failed" :ended-unix (.getTime (js/Date.)) :message result})))))

(re-frame/reg-event-db
 ::success-http-load-session
 (fn [db [_ result]]
   (undoable)
   (let [;old-status (get-in db [:http-reqs :load-session])
         new-db    (get result :image)
         good-keys (vec (for [k (keys db) :when (cstr/includes? (str k) "/")] k))
         sess      (get db :sessions) ;; ^^ system stuff like re-poll/* re-pressed/* etc
         alerts    (get db :alerts)
         wind      (get db :window)
         undo      (get db :undo)]
     (merge (select-keys db good-keys) new-db {:session-modal? true :undo undo :window wind :alerts alerts :sessions sess}))))

(re-frame/reg-event-fx
 ::load-session
 (fn [{:keys [db]} [_ file-path]]
   (ut/tapp>> [:loads? file-path])
   (let [url     (str url-base "/load")
         method  :get
         request {:file-path file-path}]
     {:db         (assoc-in db [:http-reqs :load-session] {:status "running" :url url :start-unix (.getTime (js/Date.))})
      :http-xhrio {:method          method
                   :uri             url
                   :params          request
                   :timeout         28000
                   :format          (ajax-edn/edn-request-format)
                   :response-format (ajax-edn/edn-response-format)
                   :on-success      [::success-http-load-session]
                   :on-failure      [::failure-http-load-session]}})))














(re-frame/reg-event-db
 ::failure-http-load-flow
 (fn [db [_ result]]
   (let [old-status (get-in db [:http-reqs :load-flow])]
     (assoc-in db
               [:http-reqs :load-flow] ; comp key from ::get-http-data
               (merge old-status {:status "failed" :ended-unix (.getTime (js/Date.)) :message result})))))

(re-frame/reg-event-db
 ::success-http-load-flow
 (fn [db [_ result]]
   (let [old-status           (get-in db [:http-reqs :load-flow])
         new-db               (get result :image)
         flowmaps             (get new-db :flowmaps)
         opts                 (get new-db :opts)
         flow-id              (get new-db :flow-id)
         coords               (get new-db :zoom db/based)
         _ (ut/tapp>> coords)
         flowmaps-connections (get new-db :flowmaps-connections)]
     (-> db
         (assoc :zoom-start coords)
         (assoc-in [:http-reqs :load-flow]
                   (merge old-status {:result result :ended-unix (.getTime (js/Date.)) :status "success"}))
         (assoc-in [:flows flow-id :map] flowmaps)
         (assoc-in [:flows flow-id :opts] opts)
         (assoc-in [:flows flow-id :connections] flowmaps-connections)
         (assoc :selected-flow flow-id)))))

(re-frame/reg-event-fx
 ::load-flow
 (fn [{:keys [db]} [_ file-path]]
   (let [url     (str url-base "/load-flow")
         method  :get
         request {:file-path file-path}] ;; TODO, sketchy w/o checking
     {:db         (assoc-in db [:http-reqs :load-flow] {:status "running" :url url :start-unix (.getTime (js/Date.))})
      :http-xhrio {:method          method
                   :uri             url
                   :params          request
                   :timeout         28000
                   :format          (ajax-edn/edn-request-format)
                   :response-format (ajax-edn/edn-response-format)
                   :on-success      [::success-http-load-flow]
                   :on-failure      [::failure-http-load-flow]}})))


(re-frame/reg-event-db
 ::failure-http-load-flow-history
 (fn [db [_ result]]
   (let [old-status (get-in db [:http-reqs :load-flow-history])]
     (assoc-in db
               [:http-reqs :load-flow-history] ; comp key from ::get-http-data
               (merge old-status {:status "failed" :ended-unix (.getTime (js/Date.)) :message result})))))

(re-frame/reg-event-db
 ::success-http-load-flow-history
 (fn [db [_ result]]
   (let [old-status           (get-in db [:http-reqs :load-flow-history])
         new-db               (get result :image)
         flowmaps             (get new-db :flowmaps)
         opts                 (get new-db :opts)
         flow-id              (get new-db :flow-id)
         coords               (get new-db :zoom db/based)
         _ (ut/tapp>> coords)
         flowmaps-connections (get new-db :flowmaps-connections)]
     (reset! db/last-update -1)
     (-> db
         (assoc-in [:http-reqs :load-flow-history]
                   (merge old-status {:result result :ended-unix (.getTime (js/Date.)) :status "success"}))
         (assoc-in [:flows flow-id :map] flowmaps)
         (assoc-in [:flows flow-id :opts] opts)
         (assoc-in [:flows flow-id :connections] flowmaps-connections)
         (assoc-in [:flow-results :tracker flow-id] (get result :tracker-history)) ;;(get-in
         (assoc-in [:flow-results :return-maps]
                   (merge (get-in db [:flow-results :return-maps] {}) (get result :return-maps)))
         (assoc :selected-flow flow-id)))))

(re-frame/reg-event-fx
 ::load-flow-history
 (fn [{:keys [db]} [_ run-id start-ts]]
   (let [url     (str url-base "/load-flow-history")
         method  :get
         request (merge {:run-id run-id :start-ts start-ts} (when (nil? start-ts) {:runner? true}))]
     {:db         (assoc-in db [:http-reqs :load-flow-history] {:status "running" :url url :start-unix (.getTime (js/Date.))})
      :http-xhrio {:method          method
                   :uri             url
                   :params          request
                   :timeout         28000
                   :format          (ajax-edn/edn-request-format)
                   :response-format (ajax-edn/edn-response-format)
                   :on-success      [::success-http-load-flow-history]
                   :on-failure      [::failure-http-load-flow-history]}})))






(re-frame/reg-event-db
 ::failure-http-load-flow-alias
 (fn [db [_ result]]
   (let [old-status (get-in db [:http-reqs :load-flow-alias])]
     (assoc-in db
               [:http-reqs :load-flow-alias] ; comp key from ::get-http-data
               (merge old-status {:status "failed" :ended-unix (.getTime (js/Date.)) :message result})))))

(re-frame/reg-event-db
 ::success-http-load-flow-alias
 (fn [db [_ alias result]]
   (let [old-status           (get-in db [:http-reqs :load-flow-alias])
         new-db               (get result :image)
         flowmaps             (get new-db :flowmaps)
         flow-id              alias ;(get new-db :flow-id)
         flowmaps-connections (get new-db :flowmaps-connections)]
     (-> db
         (assoc-in [:http-reqs :load-flow-alias]
                   (merge old-status {:result result :ended-unix (.getTime (js/Date.)) :status "success"}))
         (assoc-in [:flows flow-id :map] flowmaps)
         (assoc-in [:flows flow-id :connections] flowmaps-connections)
         (assoc :selected-flow flow-id)))))

(re-frame/reg-event-fx
 ::load-flow-w-alias
 (fn [{:keys [db]} [_ file-path alias]]
   (let [url     (str url-base "/load-flow")
         method  :get
         request {:file-path file-path}] ;; TODO, sketchy w/o checking
     {:db         (assoc-in db [:http-reqs :load-flow-alias] {:status "running" :url url :start-unix (.getTime (js/Date.))})
      :http-xhrio {:method          method
                   :uri             url
                   :params          request
                   :timeout         28000
                   :format          (ajax-edn/edn-request-format)
                   :response-format (ajax-edn/edn-response-format)
                   :on-success      [::success-http-load-flow-alias alias]
                   :on-failure      [::failure-http-load-flow-alias]}})))

(re-frame/reg-event-db
 ::failure-http-load-sub-flow
 (fn [db [_ file-path result]]
   (let [old-status (get-in db [:http-reqs :load-sub-flow])]
     (assoc-in db
               [:http-reqs :load-flowset] ; comp key from ::get-http-data
               (merge old-status {:status "failed" :ended-unix (.getTime (js/Date.)) :message result})))))

(re-frame/reg-event-db
 ::success-http-load-sub-flow
 (fn [db [_ file-path result]]
   (let [old-status           (get-in db [:http-reqs :load-sub-flow])
         new-db               (get result :image)
         flowmaps             (get new-db :flowmaps)
         flow-id              (get new-db :flow-id)
         flowmaps-connections (get new-db :flowmaps-connections)]
     (-> db
         (assoc-in [:http-reqs :load-sub-flow]
                   (merge old-status {:result result :ended-unix (.getTime (js/Date.)) :status "success"}))
         (assoc-in [:sub-flow-incoming :flow-id] flow-id)
         (assoc-in [:sub-flow-incoming :file-path] file-path)
         (assoc-in [:sub-flow-incoming :map] flowmaps)
         (assoc-in [:sub-flow-incoming :connections] flowmaps-connections)))))

(re-frame/reg-event-fx
 ::load-sub-flow
 (fn [{:keys [db]} [_ file-path]]
   (let [url     (str url-base "/load-flow")
         method  :get
         request {:file-path file-path}] ;; TODO, sketchy w/o checking
     {:db         (assoc-in db [:http-reqs :load-sub-flow] {:status "running" :url url :start-unix (.getTime (js/Date.))})
      :http-xhrio {:method          method
                   :uri             url
                   :params          request
                   :timeout         28000
                   :format          (ajax-edn/edn-request-format)
                   :response-format (ajax-edn/edn-response-format)
                   :on-success      [::success-http-load-sub-flow file-path]
                   :on-failure      [::failure-http-load-sub-flow file-path]}})))
