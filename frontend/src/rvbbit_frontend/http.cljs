(ns rvbbit-frontend.http
  (:require
   [re-frame.core :as re-frame]
   [ajax.core :as ajax]
   [ajax.edn :as ajax-edn]
   [clojure.string :as cstr]
   [websocket-fx.core :as wfx]
   [day8.re-frame.undo :as undo :refer [undoable]]
   [clojure.set :as cset]
   [clojure.walk :as walk]
   ["react-zoom-pan-pinch" :as zpan]
   [rvbbit-frontend.utility :as ut]
   [rvbbit-frontend.db :as db]
   [day8.re-frame.http-fx]
   [reagent.core :as reagent]))

(def socket-id :default)
;(def socket-id2 :default2)

;;(rf/subscribe [::wfx/pending-requests])
;;(rf/subscribe [::wfx/open-subscriptions])

;(rf/reg-event-fx ::subscribe
;                 (fn [{:keys [db]} [_ socket-id topic {:keys [message] :as command}]]
;                   (let [path    [::sockets socket-id :subscriptions topic]
;                         payload {:id topic :proto :subscription :data message}]
;                     {:db          (assoc-in db path command)
;                      ::ws-message {:socket-id socket-id :message payload}})))

(defn subscription [x]
  {:message    {:kind :server-push2
                :client-name x
                :ui-keypath [:server]
                :extras {:hello? true}}
   :on-message [::simple-response]})

;; (defn options [x]
;;   {:url    "ws://localhost:3030/ws"
;;    :format :edn
;;    :on-disconnect [::wfx/unsubscribe socket-id :server-push2]
;;    :on-connect [::wfx/subscribe socket-id :server-push2 (subscription x)]})

(defn options [x]
  (let [;protocol (.-protocol js/window.location)
        protocol "ws"
        host (.-host js/window.location)
        host-without-port (clojure.string/replace host #":\d+$" "")
        ws-port "3030"
        url (str protocol "://" host-without-port ":" ws-port "/ws")]
    {:url    url
     :format :edn
     :on-disconnect [::wfx/unsubscribe socket-id :server-push2]
     :on-connect [::wfx/subscribe socket-id :server-push2 (subscription x)]}))

(def server-http-port 8888)
(def url-base (str (cstr/join ":" (drop-last (cstr/split (.. js/document -location -href) #":"))) ":" server-http-port)) ;; no trailing slash


(re-frame/reg-sub
 ::url-vec
 (fn [_ _]
   (let [url-str (.. js/document -location -href)
         url-vec (if (and (cstr/includes? url-str "/#/") (> (count (cstr/split url-str "/#/")) 1))
                   (cstr/split
                    (last (cstr/split url-str "/#/"))
                    "/") [])]
     url-vec)))

(defn start-listening-to-url-changes []
  (let [on-hashchange (fn [_]
                        (let [new-url (-> js/window .-location .-hash)]
                          (re-frame/dispatch [::url-changed new-url])))]
    (.addEventListener js/window "hashchange" on-hashchange)))

(defn change-url [new-hash]
  (let [new-url (str "/#" new-hash)]
    (.pushState js/history nil "" new-url)
    (.replaceState js/history nil "" new-url)))

(re-frame/reg-event-db
 ::url-changed
 (fn [db [_ new-url]]
   (let [url-vec (if (and (cstr/includes? new-url "#") (> (count (cstr/split new-url "#")) 1))
                   (cstr/split
                    (last (cstr/split new-url "#"))
                    "/") [])
         base-dir "./screens/"
         screen (str base-dir (last url-vec) ".edn")]
     (tap> [:url-changed new-url url-vec :loading-new-screen])
     (re-frame/dispatch [::load screen])
     (assoc db :current-url new-url))))

(re-frame/reg-event-db
 ::simple-response-boot
 (fn [db [_ result]]
   (let [screens (get result :screens)
         url @(re-frame/subscribe [::url-vec])
         default-screen (get result :default)]
    ;ui-keypath (first (get result :ui-keypath))
         ;server-map (get db ui-keypath)
         ;new-map (:data result)
         ;name (:name result)
         ;field (:field result)
         ;res-meta (:result-meta result)
         ;new-map (if (= new-map '(1)) [{:sql :error :recvd (:result result)}] new-map)


     (tap> [:screens screens :url-vec url])

     (when default-screen ;; load default canvas settings
       (re-frame/dispatch [::load default-screen]))

     (if default-screen
       (-> db
           ;(assoc-in [:server :settings] (dissoc result :kits))
           ;(assoc-in [:server :kits] (get result :kits))
           (assoc :screen-name (str (get db :screen-name) (rand-int 12345))))
       (assoc-in db [:server :settings] result)))))

(re-frame/reg-event-db
 ::simple-response-boot-no-load
 (fn [db [_ result]]
   (let [];screens (get result :screens)
         ;url @(re-frame/subscribe [::url-vec])
         ;default-screen (get result :default)


    ;; (tap> [:def (get result :default)])

    ;;  (when default-screen ;; load default canvas settings
    ;;    (re-frame/dispatch [::load default-screen]))

     (if false ;default-screen
       (-> db
           ;(assoc-in [:server :settings] (dissoc result :kits))
           ;(assoc-in [:server :kits] (get result :kits))
           (assoc :screen-name (str (get db :screen-name) (rand-int 12345))))
       (assoc-in db [:server :settings] result)))))

(re-frame/reg-event-db
 ::value-response-flow
 (fn [db [_ result]]
   (let [base-key (get result :base-key)
         sub-key (get result :sub-key) ;; including .step
        ; client-name (get db :client-name)
         value (get result :value)]
     ;(tap> [:subbed-to-server-value result])
     (assoc-in db [:click-param base-key sub-key] value))))

(re-frame/reg-event-db
 ::timeout-response
 (fn [db [_ result what-req]]
   (let []
     (tap> [:websocket-timeout! result what-req])
     db)))

(re-frame/reg-event-db
 ::sub-to-flow-value
 (fn [db [_ key]]
   ;(tap> [:ran-condi])
   (let [client-name (get db :client-name)]
     (re-frame/dispatch [::wfx/request :default
                         {:message    {:kind :sub-to-flow-value
                                       :flow-key key
                                       :client-name client-name}
                          :on-response [::value-response-flow]
                          :on-timeout  [::timeout-response :get-flow-value key]
                          :timeout    15000000}]))
   db))



;; (re-frame/dispatch [::sub-to-flow-value :flow/map-pull-test2>open-fn-6])
;; (re-frame/dispatch [::sub-to-flow-value :flow/map-pull-test2>open-fn-9])
;; (re-frame/dispatch [::sub-to-flow-value :flow/map-pull-test2>write-file-1])


(defn update-context-boxes [result task-id ms reco-count]
  (when (and ;(= (first (get result :ui-keypath)) :reeco-status)
         (= task-id :reco)
                  ;(= (first (get result :data)) :done)
         (= (get result :status) :done))
    (swap! db/context-box assoc (last (get result :ui-keypath))
           (str "shapes: found " (ut/nf reco-count) " viz suggestions in ~" ms " secs")))
              ;(str "fook" (get-in result [:data 1] "?"))

   ;(tap> [:? (str "...found " (ut/nf (get-in result [:data 1])) " recos!")])


  (when (and (= task-id :outliers) ;;(not (= task-id :reco))
             (= (get result :status) :done))
    (swap! db/context-box assoc (last (get result :ui-keypath))
           (str task-id " - " (ut/nf reco-count) " results in ~" ms " secs"))))

(re-frame/reg-event-db
 ::refresh-kits
 (fn [db [_]]
   (ut/dissoc-in db [:query-history :kit-results-sys])))

(def last-hash (reagent/atom []))

(re-frame/reg-event-db
 ::insert-alert
 (fn [db [_ content w h duration & [alert-id]]]
   (let [ticktock (get-in db [:re-pollsive.core/polling :counter])
         alert-id (or alert-id (hash content))
         duration (or duration 30)]
     ;(tap> [:insert-alert [w h duration] (map first (get db :alerts []))])
     (if (some #(= % (str content)) (for [[cc _ _ _ _] (get db :alerts [])] (str cc)))
       db ;; no dupes now
       (assoc db :alerts (vec (conj (get db :alerts []) [content w h (+ ticktock duration) alert-id])))))))

(re-frame/reg-event-db
 ::runstream-item
 ;;(undoable)
 (fn [db [_ result]]
   (-> db
       (assoc-in [:runstreams-lookups (get result :flow-id) :open-inputs] (get result :open-inputs))
       (assoc-in [:runstreams-lookups (get result :flow-id) :blocks] (get result :blocks)))))

(re-frame/reg-event-db
 ::refresh-runstreams-sly
 ;;(undoable)
 (fn [db _]
   (tap> [:refresh-runstream-panelF (keys (get db :runstreams))])
   (when  (not (empty? (keys (get db :runstreams))))
     (doseq [flow-id (keys (get db :runstreams))]
       (re-frame/dispatch [::wfx/request :default
                           {:message    {:kind :get-flow-open-ports
                                         :flow-id flow-id
                                         :flowmap flow-id
                                         :client-name (get db :client-name)}
                            :on-response [::runstream-item]
                        ;:on-timeout [::http/timeout-response]
                            :timeout    500000}])))
   db))

(re-frame/reg-event-db
 ::refresh-runstreams
 (fn [db [_]]
   (dissoc db :runstreams-lookups)))

(re-frame/reg-event-db
 ::set-flow-results ;; temp from converting from atom to app-db
 (fn [db [_ data kkey & [flow-id]]]
   (cond kkey
         (assoc-in db (if (vector? kkey)
                        (vec (into [:flow-results] kkey))
                        [:flow-results kkey]) data)

         (and (= data {:status :started}) flow-id)
         (-> db
             (assoc :flow-results (merge (get db :flow-results) data))
             (ut/dissoc-in [:flow-results :return-maps flow-id])
             ;(ut/dissoc-in [:flow-results :return-map])
             (ut/dissoc-in [:flow-results :tracker flow-id]))

         :else (assoc db :flow-results data))))

(re-frame/reg-sub
 ::flow-results
 (fn [db _]
   (get db :flow-results {})))

(re-frame/reg-sub
 ::flows 
 (fn [db _]
   (get db :flows {})))

(re-frame/reg-sub
 ::flow-results-tracker
 (fn [db [_ flow-id]]
   (get-in db [:flow-results :tracker flow-id] {})))

(defn accumulate-unique-runs [data]
  (let [merge-runs (fn [runs run]
                     (let [run-start (:start run)
                           run-end (:end run)]
                       (cond
                         ; If run with same start and end exists, return runs as is.
                         (some #(and (= (:start %) run-start)
                                     (= (:end %) run-end)) runs) runs
                         ; If run with same start exists without an end, replace if current has an end.
                         (some #(and (= (:start %) run-start) (nil? (:end %)) run-end) runs)
                         (conj (vec (remove #(and (= (:start %) run-start) (nil? (:end %))) runs)) run)
                         ; Otherwise, add run.
                         :else (conj runs run))))]
    (reduce (fn [acc entry]
              (reduce (fn [inner-acc [block-id run]]
                        (update inner-acc block-id (fn [runs]
                                                     (merge-runs (or runs []) run))))
                      acc (into [] entry)))
            {} data)))

(defonce subsequent-runs (reagent/atom []))

(re-frame/reg-event-db
 ::simple-response
 (fn [db [_ result]]
   (try
     (let [ui-keypath (first (get result :ui-keypath))
           ;new-map (:data result)
           ;name (:name result)
           ;field (:field result)
           client-name (get db :client-name)
           ms (try (js/Math.ceil (/ (get result :elapsed-ms) 1000)) (catch :default _ nil))
           file-push? (= ui-keypath :file-changed)
           external-enabled? (get db :external? false)
           task-id (get result :task-id)
           payload-kp (get-in result [:data 0 :payload-kp])
           elapsed-ms (get result :elapsed-ms)
           reco-count (get result :reco-count)
           kick? (= ui-keypath :kick)
           heartbeat? (= task-id :heartbeat)
           alert? (= task-id :alert1)
           not-sys-stats? (not (cstr/includes? (str (get result :data)) "[sys-stats]"))
           server-sub? (and kick? (or (= (get-in result [:task-id 0]) :flow)
                                      (= (get-in result [:task-id 0]) :screen)
                                      (= (get-in result [:task-id 0]) :ext-param)
                                      (= (get-in result [:task-id 0]) :panel)
                                      (= (get-in result [:task-id 0]) :client)) 
                            (not heartbeat?)) ;; server mutate only for click-param
           flow-runner-sub? (and kick? (= (get-in result [:task-id 0]) :flow-runner) (not heartbeat?)) ;; server mutate only for click-param
           flow-runner-tracker? (and kick? (= (get-in result [:task-id 0]) :tracker) (not heartbeat?))
           condi-tracker? (and kick? (= (get-in result [:task-id 0]) :condis) (not heartbeat?))
           estimate? (and kick? (= (get-in result [:task-id 0]) :estimate) (not heartbeat?))
           refresh? (or (not (empty? (get-in result [:data 0 :payload])))
                        ;;; ^^ calliope test TEMP
                        (and payload-kp
                             (try (= (first (vals @db/chat-mode)) ui-keypath) (catch :default _ false))))]
         ;res-meta (:result-meta result)
         ;new-map (if (= new-map '(1)) [{:sql :error :recvd (:result result)}] new-map)
           ;sys-queries (filter #(cstr/ends-with? (str %) "-sys") (keys (get db :query-history)))

       ;(tap> [:payload! (get db :client-name) result])

      ;;  (when estimate? (tap> [:estimate! (get db :client-name) result]))

      ;;  (when (and (not heartbeat?) not-sys-stats?
      ;;             (= client-name :whole-coffee-pronghorn-hailing-from-natural-arch))
      ;;    (tap> [:payload! (get db :client-name) result]))

      ;;  (when server-sub?
      ;;    (tap> [:server-sub-in! (get result :task-id) (get result :status)]))
       
       (when (and (not heartbeat?) (cstr/starts-with? (str client-name) ":spirited"))
         (tap> [:payload! client-name task-id result]))

       (when heartbeat?
         (when (or (cstr/starts-with? (str client-name) ":spirited")
                   ;(cstr/starts-with? (str client-name) ":stirr")
                   ) (tap> [:heartbeat! client-name task-id result]))
         (re-frame/dispatch [::wfx/request :default
                             {:message    {:kind :ack
                                           :flow-subs (get db :flow-subs)
                                           :client-name (get db :client-name)}
                              :timeout    500000}]))

     ;(assoc-in db [:post-meta ui-keypath field name] new-map)
     ;(ut/dissoc-in db [:query-history])

        ;; (when (and (not heartbeat?) not-sys-stats?)
        ;;   (tap> [:incoming-push! result]))


       (when alert? ;(and alert? not-sys-stats?) ; (and alert? (string? (first (get result :data))))
         (let [cnt (get-in result [:data 0 0])
               fstr (str cnt)
               w (get-in result [:data 0 1] (/ (count fstr) 3.9))
               h (get-in result [:data 0 2] 1)
               duration (get-in result [:data 0 3] 6)]

          ;;  (when (and (cstr/includes? (str fstr) "has finished") ;; complete refresh, causes a UI flash. disabling for now.
          ;;             (get db :buffy?) not-sys-stats?) ;; refresh runstream samples when DONE
          ;;    (ut/dispatch-delay 200 [::refresh-runstreams]))

           (ut/dispatch-delay 400 [::insert-alert
                                   [:h-box
                                    :children [cnt]]
                                   w h duration])))



      ;;  (when (and kick? (not heartbeat?))
      ;;    (tap> [:kick task-id ;@db/kit-keys
      ;;           ;@db/kit-mode
      ;;           refresh?
      ;;           result
      ;;           payload-kp]))


       (when (and refresh?
                  (= (get result :task-id) "evening-ryan-3456"))
                  ;(not (some #(= (hash result) %) @last-hash))

         (do (swap! last-hash conj (hash result))
             (tap> [:rr result])
             (re-frame/dispatch [::refresh-kits])))

      ;;  (when (or flow-runner-sub? flow-runner-tracker?)
      ;;    (tap> [(if flow-runner-tracker?
      ;;             :flow-runner-tracker :flow-runner-sub)
      ;;           result]))

       (update-context-boxes result task-id ms reco-count)
       ;(tap> [:simple-response result])

       (cond (and file-push? (not (nil? (get-in result [:data 0 :panel-key]))) external-enabled?)
             (assoc-in db [:panels (get-in result [:data 0 :panel-key])] (get-in result [:data 0 :block-data]))

             server-sub? (assoc-in db (vec (cons :click-param task-id)) (get result :status))
             ;;; ^^^ mutate push to click-params

             condi-tracker? (assoc-in db [:flow-results :condis (get-in result [:task-id 1])] (get result :status))

             (and ;(get db :flow-gantt?) ;; no need if view isnt up!
                  ;(not (= (get @(re-frame/subscribe [::flow-results]) :status) :done)) ;; or channel running? we want animations!
              (get db :flow?)
              flow-runner-tracker?)

             (let [flow-id (get-in result [:task-id 1])
                                        ;ui-keypath (get-in result [:ui-keypath])
                   block-keys (keys (get-in db [:flows (get db :selected-flow) :map]))
                   expected (into {} (for [e block-keys]
                                       {e {:start (+ (.now js/Date) 1000) :fake? true}}
                                       ; {e {}}
                                       ))
                   pstatus (select-keys (get result :status) block-keys)
                  ;;  _ (swap! subsequent-runs assoc flow-id
                  ;;           (vec (conj (get @subsequent-runs flow-id) (get result :status))))
                   _ (when (not (empty? pstatus)) (swap! subsequent-runs conj pstatus))
                   status  (merge expected pstatus)
                   ;status (get result :status)
                  ; _ (tap> [:s pstatus expected status @subsequent-runs])
                  ; _ (tap> [:subruns? (accumulate-unique-runs @subsequent-runs)])
                   tracker-history (accumulate-unique-runs @subsequent-runs)
                   rr (vec (for [[k v] tracker-history
                                 :when (and
                                        (not (get (last v) :in-chan?))
                                        (nil? (get (last v) :end)))] k))
                  ;;  rrr (for [[k v] status
                  ;;            :when (nil? (get (last v) :end))] k)
                   running (vec (->> status
                                     (filter (fn [[_ v]]
                                               (and true ;(not (get v :in-chan?))
                                                    (nil? (:end v)))))
                                     (map first)))
                  ;;  rrunning (vec (->> pstatus
                  ;;                     (filter (fn [[k v]] (nil? (:end v))))
                  ;;                     (map first)))
                   ;;_ (tap> [:sss rr status tracker-history running ])
                   ]

               (swap! db/running-blocks assoc flow-id running)
               (swap! db/real-running-blocks assoc flow-id rr)
                ; _ (tap> [:running? running])
               ;;(swap! db/flow-results assoc-in [:tracker flow-id] status)
               ;(re-frame/dispatch [::set-flow-results status [:tracker flow-id]])
               (let []
                 (if (not (empty? tracker-history))
                   (assoc-in db [:flow-results :tracker flow-id]
                         ;status
                         ;(into {} (for [[k v] status] {k [v]}))
                             tracker-history) db)))

             estimate?        (assoc db :flow-estimates (merge (get db :flow-estimates) (get result :status)))

             flow-runner-sub? (let [rtn (get result :status)
                                    mp-key (walk/postwalk-replace {:flow-runner :return-map} task-id)
                                    mp-key [(first mp-key) (last mp-key)] ;; remove flow-id
                                    mps-key (vec (walk/postwalk-replace {:flow-runner :return-maps} task-id))
                                    return-maps (get-in db [:flow-results :return-maps] {})]
                                ;;;(tap> [:sub-return-maps! task-id mp-key mps-key rtn (vec (drop 1  mps-key)) return-maps])
                                (if (= rtn :started)
                                  (assoc-in db task-id rtn)
                                  ;; (do ;(swap! db/flow-results assoc-in mp-key rtn)
                                  ;;     (re-frame/dispatch [::set-flow-results rtn mp-key])
                                  ;;     ;(swap! db/flow-results assoc-in mps-key rtn)
                                  ;;     (re-frame/dispatch [::set-flow-results rtn mps-key])
                                  ;;   (assoc-in db task-id rtn)
                                  ;;   ;  (assoc-in db task-id rtn)

                                  (-> db
                                      ;(assoc-in (vec (into [:flow-results] mp-key)) rtn)
                                      (assoc-in [:flow-results :return-maps] ;; (vec (into [:flow-results] mps-key)) 
                                                (assoc-in return-maps (vec (drop 1  mps-key)) rtn)) ;; keeping the rest of the map intact
                                      (assoc-in task-id rtn))

                                  ;;  )
                                  ))
             ;;; ^^^ mutate push to flow-runner keys

             heartbeat? (-> db
                            (assoc-in [:status task-id ui-keypath] (get result :status))
                            (assoc-in [:status-data task-id ui-keypath] {:data (get result :data)
                                                                         :elapsed-ms elapsed-ms
                                                                         :reco-count reco-count})
                            (assoc :flow-subs (get result :status)))
       ;; db
             :else (-> db
             ;(assoc-in (get result :ui-keypath) (get result :data))
                       (assoc-in [:status task-id ui-keypath] (get result :status))
                       (assoc-in [:status-data task-id ui-keypath] {:data (get result :data)
                                                                    :elapsed-ms elapsed-ms
                                                                    :reco-count reco-count})
                       (ut/dissoc-in [:query-history :recos-sys])
                       (ut/dissoc-in [:query-history :viz-shapes-sys])
                       (ut/dissoc-in [:query-history :viz-shapes0-sys])
                       (ut/dissoc-in [:query-history :viz-tables-sys]))))
     (catch :default e (do (tap> [:simple-response-error! (str e)])
                           db)))))

(re-frame/reg-event-db
 ::status-response
 (fn [db [_ result]]
   (let [;fixedmap (into {} (for [[k [v1 v2]] result] {k {(first v1) v2}}))
         data (get result :data)
         statuses (get result :statuses)
         client-name (get db :client-name)
         new-statuses (last (clojure.data/diff (get db :status) statuses))
         new-status-data (last (clojure.data/diff (get db :status) statuses))
         fixedmap (into {} (for [[k v] statuses] {k (into {} (map (fn [[kk vv]] {(first kk) vv}) v))}))
         fixeddata (into {} (for [[k v] data] {k (into {} (map (fn [[kk vv]] {(first kk) vv}) v))}))]

     (when (= client-name :wealthy-apricot-skunk-hailing-from-cuspate-foreland)
       (tap> [:status-response data]))

     (when (or (not (nil? new-statuses)) (not (nil? new-status-data)))
       (tap> [:new-statues-diffs! :statuses new-statuses :data new-status-data]))
     ;(doseq (update-context-boxes result task-id ms reco-count))
     ;(tap> [:server-running-says fixeddata fixedmap])
     (-> db
         (assoc :status fixedmap)
         (assoc :status-data fixeddata)))))

(re-frame/reg-event-db
 ::timeout-response
 (fn [db [_ result what-req]]
   (let []
     (tap> [:websocket-timeout! result what-req])
     db)))

(re-frame/reg-event-db
 ::socket-response
 (fn [db [_ result]]
   (let [ui-keypath (get result :ui-keypath)
         sql-str (get result :sql-str) ; (first (get result :sql-str))
         sql-source (get result :original-honey)
         repl-output (get result :repl-output)
         client-name (get db :client-name)
         ;server-map (get db ui-keypath)
         map-order (get result :map-order)
         new-map (try (vec (for [r (get result :result)] (ut/asort r map-order))) (catch :default _ (get result :result)))
         res-meta (get result :result-meta)
         res-meta-fields (try (ut/asort (get res-meta :fields) map-order) (catch :default _ (get res-meta :fields)))
         meta (merge res-meta {:fields res-meta-fields})
         new-map (if (= new-map '(1)) [{:sql :error :recvd (:result result)}] new-map)]

     (when (= client-name :wealthy-apricot-skunk-hailing-from-cuspate-foreland)
       (tap> [:socket-response result]))

     ;(tap> [:socket-response result ui-keypath new-map map-order meta :insertable? (not (nil? map-order))])
     (if (not (nil? map-order)) ;; indicates bigger problem, too risky
       (if (and (not (nil? repl-output)) (not (empty? repl-output)))
         (-> db
             (assoc-in (cons :repl-output ui-keypath) repl-output)
             (assoc-in (cons :sql-str ui-keypath) sql-str)
            ;(assoc-in (cons :sql-source ui-keypath) sql-source)
             (assoc-in (cons :data ui-keypath) new-map)
             (assoc-in (cons :orders ui-keypath) map-order)
             (assoc-in (cons :meta ui-keypath) meta))
         (-> db
            ;;(assoc-in (cons :repl-output ui-keypath) repl-output)
             (assoc-in (cons :sql-str ui-keypath) sql-str)
            ;(assoc-in (cons :sql-source ui-keypath) sql-source)
             (assoc-in (cons :data ui-keypath) new-map)
             (assoc-in (cons :orders ui-keypath) map-order)
             (assoc-in (cons :meta ui-keypath) meta)))
       db))))

(re-frame/reg-event-db
 ::socket-response-post-meta
 (fn [db [_ result]]
   ;(tap> [db-key result])
   (let [ui-keypath (get result :ui-keypath)
         ;server-map (get db ui-keypath)
         new-map (first (vals (first (:result result))))
         ;res-meta (:result-meta result)
         new-map (if (= new-map '(1)) [{:sql :error :recvd (:result result)}] new-map)]
     (assoc-in db (cons :post-meta ui-keypath) new-map))))

(re-frame/reg-event-db
 ::socket-response-post-style
 (fn [db [_  style result]]
   ;(tap> [db-key result])
   (let [ui-keypath (drop-last (get result :ui-keypath))]
         ;server-map (get db ui-keypath)
      ;   new-map (first (vals (first (:result result))))
         ;res-meta (:result-meta result)
      ;   new-map (if (= new-map '(1)) [{:sql :error :recvd (:result result)}] new-map)

     (-> db
         (assoc-in (conj (vec (cons :post-styles ui-keypath)) :results) (get result :result))
         (assoc-in (conj (vec (cons :post-styles ui-keypath)) :styles) style)))))

(re-frame/reg-event-db
 ::socket-response-post-tab
 (fn [db [_  panel-key result]]
   (let [ui-keypath (get result :ui-keypath)]
     ;(-> db
     (assoc-in db ;(conj
               (vec (cons :post-tab ui-keypath))
                      ;     panel-key)
               (get result :result)))))
         ;(assoc-in (conj (vec (cons :post-styles ui-keypath)) :styles) style)


(re-frame/reg-event-db
 ::socket-response-post-condi
 (fn [db [_  result]]
   (let [ui-keypath (get result :ui-keypath)]
     ;(-> db
     (assoc-in db ;(conj
               (vec (cons :post-condi ui-keypath))
                       ;     panel-key)
               (get result :result)))))
         ;(assoc-in (conj (vec (cons :post-styles ui-keypath)) :styles) style)


(re-frame/reg-sub
 ::websocket-status
 (fn [db _]
   (let [subs (vec (doall @(re-frame/subscribe [::wfx/open-subscriptions socket-id])))
         subs1 (vec (for [s subs] (get-in s [:message :kind])))
         datas (count (keys (get db :data)))
         panels (count (keys (get db :panels)))
         pendings @(re-frame/subscribe [::wfx/pending-requests socket-id])]
     {:status @(re-frame/subscribe [::wfx/status socket-id])
      :waiting (count pendings)
      :datasets datas
      :panels panels
      :open-subs subs1
      :subs subs})))

(re-frame/reg-event-db
 ::failure-http-save-flowset
 (fn [db [_ result]]
   (let [old-status (get-in db [:http-reqs :save-flowset])]
     (tap> [:failure-http-save-flowset result])
     (assoc-in db [:http-reqs :save-flowset] ; comp key from ::get-http-data
               (merge old-status
                      {:status "failed"
                       :ended-unix (.getTime (js/Date.))
                       :message result})))))

(re-frame/reg-event-db
 ::success-http-save-flowset
 (fn [db [_ result]]
   (let [old-status (get-in db [:http-reqs :save-flowset])]
     (assoc-in db [:http-reqs :save-flowset] ; comp key from ::get-http-data
               (merge old-status
                      {:keys (count result)
                       ;:cleared-undo (undo/clear-history!)
                       :result (select-keys result [:status :flowset])
                       :ended-unix (.getTime (js/Date.))
                       :status "success"})))))

(re-frame/reg-event-fx
 ::save
 (fn [{:keys [db]} [_ save-type screen-name resolved-queries]]
   (let [method :post
         url (str url-base "/save")
         pp (doall (for [r (filter #(cstr/starts-with? (str %) ":reco-preview") (keys (get-in db [:panels])))] r))
         pl (keys (get db :panels))
         client-name (get db :client-name)
         p0 (cset/difference (set pl) (set pp))
        ;;  _ (tap> [:resolved-queries resolved-queries])

         ;; (assoc db :panels (select-keys (get db :panels) p0)
     ;(tap> p0)
     ;(doseq [p pp] (ut/dissoc-in db p))

         ;prevs (for [r (filter #(cstr/starts-with? (str %) ":reco-preview") (keys (get-in db [:panels])))] [:panels r])
         base-keys (filter (comp not namespace) (keys db))
         image (if (= save-type :skinny)
                 (-> db ;; dehydrated... as it were
                     (select-keys base-keys)
                     (dissoc :query-history)
                     (dissoc :query-history-condi)
                     (dissoc :query-history-meta)
                     (dissoc :flow-results)
                     (dissoc :data)
                     (dissoc :flows) ;;; mostly ephemeral with the UI....
                     (dissoc :http-reqs)
                     (dissoc :sql-str)
                     (dissoc :file-changed)
                     (assoc :panels (select-keys (get db :panels) p0)))
                 db)
         request {:image (assoc image :resolved-queries resolved-queries)
                  :client-name client-name
                  :screen-name screen-name}
         _ (tap> [:saving screen-name])]
     {:db   (assoc-in db [:http-reqs :save-flowset]
                      {:status "running"
                       :url url
                       :start-unix (.getTime (js/Date.))})
      :http-xhrio {:method          method
                   :uri             url
                   :params          request
                   :timeout         28000
                   :format          (ajax-edn/edn-request-format)
                   :response-format (ajax-edn/edn-response-format)
                   :on-success      [::success-http-save-flowset]
                   :on-failure      [::failure-http-save-flowset]}})))

(re-frame/reg-event-db
 ::failure-http-save-flow
 (fn [db [_ result]]
   (let [old-status (get-in db [:http-reqs :save-flow])]
     (assoc-in db [:http-reqs :save-flow] ; comp key from ::get-http-data
               (merge old-status
                      {:status "failed"
                       :ended-unix (.getTime (js/Date.))
                       :message result})))))

(re-frame/reg-event-db
 ::success-http-save-flow
 (fn [db [_ result]]
   (let [old-status (get-in db [:http-reqs :save-flow])]
     ;;(reset! editor-tooltip-atom (str))
     (assoc-in db [:http-reqs :save-flow] ; comp key from ::get-http-data
               (merge old-status
                      {:keys (count result)
                       ;:cleared-undo (undo/clear-history!)
                       :result (select-keys result [:status :flowset])
                       :ended-unix (.getTime (js/Date.))
                       :status "success"})))))

(re-frame/reg-event-fx
 ::save-flow
 (fn [{:keys [db]} [_ flow-body flow-name]]
   (let [method :post
         url (str url-base "/save-flow")
         client-name (get db :client-name)
         ;pp (doall (for [r (filter #(cstr/starts-with? (str %) ":reco-preview") (keys (get-in db [:panels])))] r))
         ;pl (keys (get db :panels))
         ;p0 (cset/difference (set pl) (set pp))
         ;; (assoc db :panels (select-keys (get db :panels) p0)
     ;(tap> p0)
     ;(doseq [p pp] (ut/dissoc-in db p))

         ;prevs (for [r (filter #(cstr/starts-with? (str %) ":reco-preview") (keys (get-in db [:panels])))] [:panels r])
         request {:image flow-body
                  :client-name client-name
                  :flow-id flow-name}]
     (tap> [:save-flow request])
     {:db   (assoc-in db [:http-reqs :save-flow]
                      {:status "running"
                       :url url
                       :start-unix (.getTime (js/Date.))})
      :http-xhrio {:method          method
                   :uri             url
                   :params          request
                   :timeout         28000
                   :format          (ajax-edn/edn-request-format)
                   :response-format (ajax-edn/edn-response-format)
                   :on-success      [::success-http-save-flow]
                   :on-failure      [::failure-http-save-flow]}})))

(re-frame/reg-event-db
 ::failure-http-save-snap
 (fn [db [_]]
   (let [old-status (get-in db [:http-reqs :save-snap])]
     (assoc-in db [:http-reqs :save-snap] ; comp key from ::get-http-data
               (merge old-status
                      {:status "failed"
                       :ended-unix (.getTime (js/Date.))})))))

(re-frame/reg-event-db
 ::success-http-save-snap
 (fn [db [_]]
   (let [old-status (get-in db [:http-reqs :save-snap])]
     (assoc-in db [:http-reqs :save-snap] ; comp key from ::get-http-data
               (merge old-status
                      {:ended-unix (.getTime (js/Date.))
                       :status "success"})))))

(re-frame/reg-event-fx
 ::save-snap
 (fn [{:keys [db]} [_ image]]
   (let [method :post
         url (str url-base "/save-snap")
         client-name (get db :client-name)
         compund-keys (vec (into (for [k (keys db) :when (cstr/includes? (str k) "/")] k) [:http-reqs]))
         request {:image image
                  :session (ut/deselect-keys db compund-keys)
                  :client-name client-name}]
     ;(tap> [:save-flow request])
     {:db   (assoc-in db [:http-reqs :save-snap]
                      {:status "running"
                       :url url
                       :start-unix (.getTime (js/Date.))})
      :http-xhrio {:method          method
                   :uri             url
                   :params          request
                   :timeout         28000
                   :format          (ajax-edn/edn-request-format)
                   :response-format (ajax-edn/edn-response-format)
                   :on-success      [::success-http-save-snap]
                   :on-failure      [::failure-http-save-snap]}})))






(re-frame/reg-event-db
 ::failure-http-save-screen-snap
 (fn [db [_]]
   (let [old-status (get-in db [:http-reqs :save-screen-snap])]
     (assoc-in db [:http-reqs :save-screen-snap] ; comp key from ::get-http-data
               (merge old-status
                      {:status "failed"
                       :ended-unix (.getTime (js/Date.))})))))

(re-frame/reg-event-db
 ::success-http-save-screen-snap
 (fn [db [_]]
   (let [old-status (get-in db [:http-reqs :save-screen-snap])]
     (assoc-in db [:http-reqs :save-screen-snap] ; comp key from ::get-http-data
               (merge old-status
                      {:ended-unix (.getTime (js/Date.))
                       :status "success"})))))

(re-frame/reg-event-fx
 ::save-screen-snap
 (fn [{:keys [db]} [_ image]]
   (let [method :post
         url (str url-base "/save-screen-snap")
         screen-name (get db :screen-name)
         request {:image image
                  :screen-name screen-name}]
     (tap> [:save-screen-snap!])
     {:db   (assoc-in db [:http-reqs :save-screen-snap]
                      {:status "running"
                       :url url
                       :start-unix (.getTime (js/Date.))})
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
     (assoc-in db [:http-reqs :save-csv] ; comp key from ::get-http-data
               (merge old-status
                      {:status "failed"
                       :ended-unix (.getTime (js/Date.))
                       :message result})))))

(re-frame/reg-event-db
 ::success-http-save-csv
 (fn [db [_ result]]
   (let [old-status (get-in db [:http-reqs :save-csv])]
     (assoc-in db [:http-reqs :save-csv] ; comp key from ::get-http-data
               (merge old-status
                      {:keys (count result)
                       ;:cleared-undo (undo/clear-history!)
                       ;:result (select-keys result [:status :flowset])
                       :ended-unix (.getTime (js/Date.))
                       :status "success"})))))

(re-frame/reg-event-fx
 ::save-csv
 (fn [{:keys [db]} [_ fname fdata]]
   (when (cstr/ends-with? (cstr/lower-case fname) ".csv")
     (let [method :post
           url (str url-base "/save-csv")
           request {:image fdata
                    :client-name (get db :client-name)
                    :fname fname}]
       {:db   (assoc-in db [:http-reqs :save-csv]
                        {:status "running"
                         :url url
                         :start-unix (.getTime (js/Date.))})
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
 (fn [db [_ result]]
   (let [old-status (get-in db [:http-reqs :load-flowset])]
     (assoc-in db [:http-reqs :load-flowset] ; comp key from ::get-http-data
               (merge old-status
                      {:status "failed"
                       :ended-unix (.getTime (js/Date.))
                       :message result})))))

(re-frame/reg-event-db
 ::success-http-load-flowset
 (fn [db [_ result]]
   (let [old-status (get-in db [:http-reqs :load-flowset])
         new-db (dissoc (get result :image) :resolved-queries)]
     (-> db
         (assoc-in [:http-reqs :load-flowset]
                   (merge old-status
                          {:result result
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
         (assoc :screen-name (get new-db :screen-name))
         (assoc :data (get new-db :data))
         (assoc :click-param (get new-db :click-param))))))

(re-frame/reg-event-fx
 ::load
 (fn [{:keys [db]} [_ file-path]]
   (tap> [:loads? file-path])
   (let [url (str url-base "/load")
         method :get
         request {:file-path file-path}
         ;_ (change-url (cstr/replace (str (last (cstr/split file-path "/"))) #".edn" ""))
         _ (change-url (str "/" (cstr/replace (str (last (cstr/split file-path "/"))) #".edn" "")))]
        ; _ (tap> [file-path (str (last (cstr/split file-path "/"))) (cstr/replace (str (last (cstr/split file-path "/"))) #".edn" "")])
          ;; TODO, sketchy w/o checking
     {:db   (assoc-in db [:http-reqs :load-flowset]
                      {:status "running"
                       :url url
                       :start-unix (.getTime (js/Date.))})
      :http-xhrio {:method          method
                   :uri             url
                   :params          request
                   :timeout         28000
                   :format          (ajax-edn/edn-request-format)
                   :response-format (ajax-edn/edn-response-format)
                   :on-success      [::success-http-load-flowset]
                   :on-failure      [::failure-http-load-flowset]}})))

(re-frame/reg-event-db
 ::failure-http-load-session
 (fn [db [_ result]]
   (let [old-status (get-in db [:http-reqs :load-session])]
     (assoc-in db [:http-reqs :load-session] ; comp key from ::get-http-data
               (merge old-status
                      {:status "failed"
                       :ended-unix (.getTime (js/Date.))
                       :message result})))))

(re-frame/reg-event-db
 ::success-http-load-session
 (fn [db [_ result]]
   (undoable)
   (let [;old-status (get-in db [:http-reqs :load-session])
         new-db (get result :image)
         good-keys (vec (for [k (keys db) :when (cstr/includes? (str k) "/")] k))
         sess (get db :sessions) ;; ^^ system stuff like re-poll/* re-pressed/* etc
         alerts (get db :alerts)
         wind (get db :window)
         undo (get db :undo)]
     (merge (select-keys db good-keys)
            new-db
            {:session-modal? true
             :undo undo
             :window wind
             :alerts alerts
             :sessions sess}))))

(re-frame/reg-event-fx
 ::load-session
 (fn [{:keys [db]} [_ file-path]]
   (tap> [:loads? file-path])
   (let [url (str url-base "/load")
         method :get
         request {:file-path file-path}]
     {:db   (assoc-in db [:http-reqs :load-session]
                      {:status "running"
                       :url url
                       :start-unix (.getTime (js/Date.))})
      :http-xhrio {:method          method
                   :uri             url
                   :params          request
                   :timeout         28000
                   :format          (ajax-edn/edn-request-format)
                   :response-format (ajax-edn/edn-response-format)
                   :on-success      [::success-http-load-session]
                   :on-failure      [::failure-http-load-session]}})))











;; (defn set-zoom-pan [[positionX positionY scale]]
;;   (tap> [:set-zoom-pan [positionX positionY scale]])
;;   (when-let [z @db/zoomer]
;;     (.setTransform z positionX positionY scale 0)))

;; (defn set-zoom-pan [[positionX positionY scale]]
;;   (js/requestAnimationFrame
;;    (fn []
;;      (when-let [z @db/zoomer]
;;        ;(.setTransform z positionX positionY scale 0)
;;        (.setZoom z 1)
;;        ))))


(re-frame/reg-event-db
 ::failure-http-load-flow
 (fn [db [_ result]]
   (let [old-status (get-in db [:http-reqs :load-flow])]
     (assoc-in db [:http-reqs :load-flow] ; comp key from ::get-http-data
               (merge old-status
                      {:status "failed"
                       :ended-unix (.getTime (js/Date.))
                       :message result})))))

(re-frame/reg-event-db
 ::success-http-load-flow
 (fn [db [_ result]]
   (let [old-status (get-in db [:http-reqs :load-flow])
         new-db (get result :image)
         flowmaps (get new-db :flowmaps)
         opts (get new-db :opts)
         flow-id (get new-db :flow-id)
         coords (get new-db :zoom db/based)
         _ (tap> coords)
         flowmaps-connections (get new-db :flowmaps-connections)]
     ;(set-zoom-pan (if (empty? coords) db/based coords))
     ;(set-zoom-pan coords)
     (-> db
         (assoc :zoom-start coords)
         (assoc-in [:http-reqs :load-flow]
                   (merge old-status
                          {:result result
                           :ended-unix (.getTime (js/Date.))
                           :status "success"}))
         (assoc-in [:flows flow-id :map] flowmaps)
         (assoc-in [:flows flow-id :opts] opts)
         (assoc-in [:flows flow-id :connections] flowmaps-connections)
         (assoc :selected-flow flow-id)))))

(re-frame/reg-event-fx
 ::load-flow
 (fn [{:keys [db]} [_ file-path]]
   (let [url (str url-base "/load-flow")
         method :get
         request {:file-path file-path}] ;; TODO, sketchy w/o checking
     {:db   (assoc-in db [:http-reqs :load-flow]
                      {:status "running"
                       :url url
                       :start-unix (.getTime (js/Date.))})
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
     (assoc-in db [:http-reqs :load-flow-history] ; comp key from ::get-http-data
               (merge old-status
                      {:status "failed"
                       :ended-unix (.getTime (js/Date.))
                       :message result})))))

(re-frame/reg-event-db
 ::success-http-load-flow-history
 (fn [db [_ result]]
   (let [old-status (get-in db [:http-reqs :load-flow-history])
         new-db (get result :image)
         flowmaps (get new-db :flowmaps)
         opts (get new-db :opts)
         flow-id (get new-db :flow-id)
         coords (get new-db :zoom db/based)
         _ (tap> coords)
         flowmaps-connections (get new-db :flowmaps-connections)]
     ;;(tap> [:load-flow-history result])
     ;(set-zoom-pan (if (empty? coords) db/based coords))
     ;(set-zoom-pan coords)
     ;(swap! db/last-gantt assoc flow-id {})
     (reset! db/last-update -1)
     (-> db
         ;(assoc :zoom-start coords)
         (assoc-in [:http-reqs :load-flow-history]
                   (merge old-status
                          {:result result
                           :ended-unix (.getTime (js/Date.))
                           :status "success"}))
         (assoc-in [:flows flow-id :map] flowmaps)
         (assoc-in [:flows flow-id :opts] opts)
         (assoc-in [:flows flow-id :connections] flowmaps-connections)
         (assoc-in [:flow-results :tracker flow-id] (get result :tracker-history)) ;;(get-in result [:tracker flow-id]))
         ;(assoc-in [:flow-results :return-map] (get result :return-map)) ;; is this even used anymore? TODO
         (assoc-in [:flow-results :return-maps] (merge (get-in db [:flow-results :return-maps] {}) (get result :return-maps)))
         (assoc :selected-flow flow-id)))))

(re-frame/reg-event-fx
 ::load-flow-history
 (fn [{:keys [db]} [_ run-id start-ts]]
   (let [url (str url-base "/load-flow-history")
         method :get
         request (merge 
                  {:run-id run-id
                  :start-ts start-ts}
                  (when (nil? start-ts) 
                    {:runner? true}))] 
     ;(tap> [:req request])
     {:db   (assoc-in db [:http-reqs :load-flow-history]
                      {:status "running"
                       :url url
                       :start-unix (.getTime (js/Date.))})
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
     (assoc-in db [:http-reqs :load-flow-alias] ; comp key from ::get-http-data
               (merge old-status
                      {:status "failed"
                       :ended-unix (.getTime (js/Date.))
                       :message result})))))

(re-frame/reg-event-db
 ::success-http-load-flow-alias
 (fn [db [_ alias result]]
   (let [old-status (get-in db [:http-reqs :load-flow-alias])
         new-db (get result :image)
         flowmaps (get new-db :flowmaps)
         flow-id alias ;(get new-db :flow-id)
         flowmaps-connections (get new-db :flowmaps-connections)]
     (-> db
         (assoc-in [:http-reqs :load-flow-alias]
                   (merge old-status
                          {:result result
                           :ended-unix (.getTime (js/Date.))
                           :status "success"}))
         (assoc-in [:flows flow-id :map] flowmaps)
         (assoc-in [:flows flow-id :connections] flowmaps-connections)
         (assoc :selected-flow flow-id)))))

(re-frame/reg-event-fx
 ::load-flow-w-alias
 (fn [{:keys [db]} [_ file-path alias]]
   (let [url (str url-base "/load-flow")
         method :get
         request {:file-path file-path}] ;; TODO, sketchy w/o checking
     {:db   (assoc-in db [:http-reqs :load-flow-alias]
                      {:status "running"
                       :url url
                       :start-unix (.getTime (js/Date.))})
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
     (assoc-in db [:http-reqs :load-flowset] ; comp key from ::get-http-data
               (merge old-status
                      {:status "failed"
                       :ended-unix (.getTime (js/Date.))
                       :message result})))))

(re-frame/reg-event-db
 ::success-http-load-sub-flow
 (fn [db [_  file-path result]]
   (let [old-status (get-in db [:http-reqs :load-sub-flow])
         new-db (get result :image)
         flowmaps (get new-db :flowmaps)
         flow-id (get new-db :flow-id)
         flowmaps-connections (get new-db :flowmaps-connections)]
     (-> db
         (assoc-in [:http-reqs :load-sub-flow]
                   (merge old-status
                          {:result result
                           :ended-unix (.getTime (js/Date.))
                           :status "success"}))
         (assoc-in [:sub-flow-incoming :flow-id] flow-id)
         (assoc-in [:sub-flow-incoming :file-path] file-path)
         (assoc-in [:sub-flow-incoming :map] flowmaps)
         (assoc-in [:sub-flow-incoming :connections] flowmaps-connections)))))

(re-frame/reg-event-fx
 ::load-sub-flow
 (fn [{:keys [db]} [_ file-path]]
   (let [url (str url-base "/load-flow")
         method :get
         request {:file-path file-path}] ;; TODO, sketchy w/o checking
     {:db   (assoc-in db [:http-reqs :load-sub-flow]
                      {:status "running"
                       :url url
                       :start-unix (.getTime (js/Date.))})
      :http-xhrio {:method          method
                   :uri             url
                   :params          request
                   :timeout         28000
                   :format          (ajax-edn/edn-request-format)
                   :response-format (ajax-edn/edn-response-format)
                   :on-success      [::success-http-load-sub-flow file-path]
                   :on-failure      [::failure-http-load-sub-flow file-path]}})))
