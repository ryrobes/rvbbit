(ns rvbbit-frontend.flows
  (:require
   [reagent.core :as reagent]
   [re-frame.core :as re-frame]
   [re-com.core :as re-com :refer [at]]
   [re-com.util :refer [px]]
   [rvbbit-frontend.connections :as conn]
   [re-catch.core :as rc]
   [rvbbit-frontend.utility :as ut]
   [rvbbit-frontend.buffy :as buffy]
   [clojure.data :as cdata]
   ;[re-pressed.core :as rp]
   [rvbbit-frontend.audio :as audio]
   [cljs.tools.reader :refer [read-string]]
   ;[rvbbit-frontend.shapes2 :as shape2]
   ;[rvbbit-frontend.shapes :as scrubbers]
   [garden.color :as c :refer [hex->hsl hsl->hex hsl hsla color? hex? invert mix
                               complement shades triad tetrad split-complement
                               analogous rgb->hsl rgb->hex]]
   [talltale.core :as tales]
   [day8.re-frame.undo :as undo :refer [undoable]]
   [cljs.core.async :as async :refer [<! >! chan]]
   ;["react" :as react]
   [rvbbit-frontend.resolver :as resolver]
   [clojure.edn :as edn]
   [rvbbit-frontend.db :as db]
   [rvbbit-frontend.subs :as subs]
   [rvbbit-frontend.http :as http]
   [goog.dom :as gdom]
   [rvbbit-frontend.bricks :as bricks :refer [theme-pull]]
   [goog.events :as gevents]
   [clojure.walk :as walk]
   [clojure.string :as cstr]
   [clojure.set :as cset]
   ["react-drag-and-drop" :as rdnd]
   ["react-codemirror2" :as cm]
   ["codemirror/mode/sql/sql.js"]
   ["codemirror/addon/edit/matchbrackets.js"]
   ["codemirror/addon/edit/closebrackets.js"]
   ["codemirror/mode/clojure/clojure.js"]
   ["codemirror/mode/python/python.js"]
   ["codemirror/mode/r/r.js"]
   ["codemirror/mode/julia/julia.js"]
   [rvbbit-frontend.connections :refer [sql-data]]
   ["react-zoom-pan-pinch" :as zpan]
   [websocket-fx.core :as wfx]
   ;[cljs.tools.reader :refer [read-string]]
   ;[oz.core :as oz]
   ;[reagent.dom :as rdom]
   ;[websocket-fx.core :as wfx]
   [cljs-drag-n-drop.core :as dnd2]
   [clojure.walk :as w])
  (:import [goog.events EventType]
           [goog.async Debouncer]))

(defonce flow-details-panel? (reagent/atom false))
(def flow-drop-hover (reagent/atom nil))
(def flow-hover (reagent/atom nil))
;(def flow-hover2 (reagent/atom nil))
(def involved (reagent/atom []))
(def pan-lock? (reagent/atom false))
;(defonce flow-select (reagent/atom nil))
(def dragging-port? (reagent/atom false))
(defonce lookup-modal? (reagent/atom false))
(def dragged-port (reagent/atom []))
(defonce dragging? (reagent/atom false))
(defonce dragging-flow? (reagent/atom false))
;(defonce bid-over (reagent/atom nil))
;(defonce db/flow-results (reagent/atom {})) ;; @(re-frame/subscribe [::http/flow-results])
(def editor-tooltip-atom (reagent/atom nil))
;(def flow-id (reagent/atom "live-scratch-flow"))
(defonce title-edit-idx (reagent/atom nil))
(defonce drop-toggle? (reagent/atom false))
(def snap-to-grid 25)

(defonce channel-holster (reagent/atom {}))

(def canvas-width 7500)
(def canvas-height 6000)

(defonce flow-details-block-container-atom (reagent/atom {}))

(def port-hover (reagent/atom {}))
(def port-hover2 (reagent/atom {}))

;;(def zoomer (reagent/atom nil))

(defn gn [x] (try (name x) (catch :default _ x)))
(defn gns [x] (try (namespace x) (catch :default _ x)))
(defn gns? [x] (not (nil? (try (namespace x) (catch :default _ false)))))

;; (defonce flowmaps (reagent/atom {:my-data {:w 200 :h 90 :x 1225 :y 1250 :z 0 :ports {:in {:in :string}
;;                                                                                  :out {:out :rowset}}}
;;                                  :blockw {:w 200 :h 90 :x 1325 :y 1350 :z 0 :ports {:in {:in :string}
;;                                                                                   :out {:out1 :string
;;                                                                                         :out2 :integer}}}
;;                                  :blockooo {:w 200 :h 90 :x 1325 :y 1350 :z 0 :ports {:in {:in :string}
;;                                                                                     :out {:out :string}}}
;;                                  :block0 {:w 200 :h 90 :x 1730 :y 1240 :z 0 :ports {:in {:max-count :integer
;;                                                                                       :prompt :string
;;                                                                                       :in33 :integer
;;                                                                                       :in33b :integer
;;                                                                                       :in33a :integer
;;                                                                                       :data :rowset}
;;                                                                                  :out {:out :string}}}
;;                                  :block1 {:w 200 :h 90 :x 2290 :y 1540 :z 0 :ports {:in {:in :string}
;;                                                                                   :out {:out :string}}}
;;                                  :block2 {:w 200 :h 90 :x 3000 :y 1605 :z 0 :ports {:in {:in :string}
;;                                                                                    :out {:out :string}}}}))
;; (defonce flowmaps-connections (reagent/atom [[:block0 :block1]
;;                                              [:my-data :block0/data]
;;                                              [:blockw/out2 :block0/in33]
;;                                              [:block1 :block2]]))

;(defonce flowmaps (reagent/atom {}))
;(defonce flowmaps-connections (reagent/atom []))
;(def db/running-blocks (reagent/atom [:rabbit-sql-query :block0 ]))
;;(def db/running-blocks (reagent/atom {}))
(defonce editor-mode (reagent/atom :flow-browser))

(defonce detached-coords
  (reagent/atom (let [hh (.-innerHeight js/window) ;; starting size set on load
                      ww (.-innerWidth js/window) ;; starting size set on load
                      topper (* hh 0.06)
                      lefty (* ww 0.06)]
                  [lefty topper])))

(re-frame/reg-sub
 ::flowmap-raw
 (fn [db]
   (let [fmaps (get-in db [:flows (get db :selected-flow) :map] {})
         fmaps (into {} (for [[k v] fmaps
                              :when (get v :w)]
                          {k v}))]
     fmaps)))

(re-frame/reg-sub
 ::flowmap-raw-block
 (fn [db [_ bid]]
   (get-in db [:flows (get db :selected-flow) :map bid] {})))

(re-frame/reg-sub
 ::selected-flow
 (fn [db]
   (let [animal (cstr/replace (str (tales/animal)) #" " "-")
         flow-id (str animal "-flow-" (rand-int 1000))]
     (get db :selected-flow flow-id))))

(re-frame/reg-event-db
 ::set-selected-flow
 (fn [db [_ flow-id]]
   (tap> [:set-flow flow-id])
   (let [flow-id (str flow-id)
         curr-flow-id (get db :selected-flow)
         curr-flow (get-in db [:flows curr-flow-id] {})]
    ;(reset! db/flow-results (walk/postwalk-replace {curr-flow-id flow-id} @(re-frame/subscribe [::http/flow-results]))) ;; unnecessary, but keeps the UI in sync
     (-> db
        ;(assoc-in [:flows flow-id] curr-flow)
        ;(ut/dissoc-in [:flows curr-flow-id])
         (assoc :selected-flow flow-id)))))

(re-frame/reg-event-db
 ::rename-flow
 (fn [db [_ old-flow-id new-flow-id]]
   (tap> [:rename-flow old-flow-id :to new-flow-id])
   (let [];flow-id (str flow-id)
        ;curr-flow-id (get db :selected-flow)
        ;curr-flow (get-in db [:flows curr-flow-id] {})

     ;(reset! db/flow-results (walk/postwalk-replace {old-flow-id new-flow-id} @(re-frame/subscribe [::http/flow-results]))) ;; unnecessary, but keeps the UI in sync
     (re-frame/dispatch [::http/set-flow-results (walk/postwalk-replace {old-flow-id new-flow-id} @(re-frame/subscribe [::http/flow-results]))])
     (-> db
        ;(assoc-in [:flows flow-id] curr-flow)
        ;(ut/dissoc-in [:flows curr-flow-id])
         (assoc :flows (walk/postwalk-replace {old-flow-id new-flow-id} (get db :flows)))
         (assoc :selected-flow new-flow-id)))))

(re-frame/reg-event-db
 ::rename-block
 (fn [db [_ old-bid new-bid]]

   (let [curr-flow-id (get db :selected-flow)
         block-ids (keys (get-in db [:flows curr-flow-id :map]))
         inputs (keys (get-in db [:flows curr-flow-id :map old-bid :ports :in]))
         outputs (keys (get-in db [:flows curr-flow-id :map old-bid :ports :out]))
         new-bid (keyword new-bid)
         new-bid (ut/safe-key new-bid @(re-frame/subscribe [::reserved-type-keywords]))
         input-replacements (into {}
                                  (for [k inputs] {(keyword (str (ut/unkeyword old-bid) "/" (ut/unkeyword k)))
                                                   (keyword (str (ut/unkeyword new-bid) "/" (ut/unkeyword k)))}))
         output-replacements (into {}
                                   (for [k outputs] {(keyword (str (ut/unkeyword old-bid) "/" (ut/unkeyword k)))
                                                     (keyword (str (ut/unkeyword new-bid) "/" (ut/unkeyword k)))}))
         pw-replace-map (merge input-replacements output-replacements {old-bid new-bid})]
     (tap> [:rename-block old-bid :to new-bid :with pw-replace-map]) ;; block id needs to be keyword,
     (if (not (some #(= new-bid %) block-ids)) ;; only if is unique, else do nothing
       (do
;         (reset! db/flow-results (walk/postwalk-replace pw-replace-map @(re-frame/subscribe [::http/flow-results]))) ;; unnecessary, but keeps the UI in sync
         (re-frame/dispatch [::http/set-flow-results (walk/postwalk-replace pw-replace-map @(re-frame/subscribe [::http/flow-results]))])
         (-> db
             (assoc :selected-flow-block new-bid)
             (assoc-in [:flows curr-flow-id] (walk/postwalk-replace pw-replace-map (get-in db [:flows curr-flow-id])))))
       db))))

(re-frame/reg-sub
 ::flowmap-connections
 (fn [db]
   (vec (get-in db [:flows (get db :selected-flow) :connections] []))))

(re-frame/reg-sub
 ::flowmap-connections-parsed
 (fn [db [_ bid]]
   (vec
    (for [[o i] (get-in db [:flows (get db :selected-flow) :connections] [])
          :let [ns-i? (gns i)
                ns-o? (gns o)
                bid-i (if ns-i? (keyword (gns i)) i)
                pid-i (if ns-i? (keyword (gn i)) :in)
                bid-o (if ns-o? (keyword (gns o)) o)
                pid-o (if ns-o? (keyword (gn o)) :out)]
          :when (if (nil? bid) true (or (= bid bid-i)
                                        (= bid bid-o)))]
      [(if (= bid bid-i)
         :in :out) bid-o pid-o bid-i pid-i]))))

(re-frame/reg-event-db
 ::clean-connections
 (undoable)
 (fn [db _]
   (let [port-map-in (into {} (for [[k v] (get-in db [:flows (get db :selected-flow) :map])]
                                {k (merge {:in :in} (get-in v [:ports :in] {}))}))
         port-map-out  (into {} (for [[k v] (get-in db [:flows (get db :selected-flow) :map])]
                                  {k (merge {:out :out}
                                            (let [oo (get-in v [:ports :out] {})]
                                              (if (map? (get oo :out)) (get oo :out) oo)))})) ;; weird edge case w auto-port data

         conns (get-in db [:flows (get db :selected-flow) :connections] [])
         clean-conns (vec
                      (for [[o i] conns
                            :let [ns-i? (gns i)
                                  ns-o? (gns o)
                                  bid-i (if ns-i? (keyword (gns i)) i)
                                  pid-i (if ns-i? (keyword (gn i)) :in)
                                  bid-o (if ns-o? (keyword (gns o)) o)
                                  pid-o (if ns-o? (keyword (gn o)) :out)
                                  valid? (or (and (or (get-in port-map-in [bid-i pid-i]) (= bid-i :done))
                                                  (get-in port-map-out [bid-o pid-o]))
                                             (get-in db [:flows (get db :selected-flow) :map bid-o :cond pid-o]) ;; condi port
                                             (get-in db [:flows (get db :selected-flow) :map bid-o :push pid-o]) ;; push port
                                             )
                                  _ (when (not valid?)
                                      (tap> [:removed-invalid-connection
                                             [o i] [[bid-i pid-i] [bid-o pid-o]]
                                             (or (get-in port-map-in [bid-i pid-i]) (= bid-i :done))
                                             (get-in port-map-out [bid-o pid-o])
                                             port-map-in port-map-out]))]
                            :when valid?]
                        [o i]))
         clean-conns (vec (remove #(or (nil? (first %)) (nil? (second %))) clean-conns))]
     (assoc-in db [:flows (get db :selected-flow) :connections] clean-conns))))

(defn mouse-move-handler [offset]
  (fn [evt]
    (let [start-x (.-clientX evt)
          start-y (.-clientY evt)
          off-x (:x offset)
          off-y (:y offset)
          x      (- start-x off-x)
          y      (- start-y off-y)]
      (reset! detached-coords [x y]))))

(defn mouse-up-handler [on-move]
  (fn [evt] ;; removed "me" wtf? why did that even work? TODO check other implementations of this fn
    (reset! db/dragging-flow-editor? false)
    (reset! flow-hover nil)
    (do
      (gevents/unlisten js/window EventType.MOUSEMOVE on-move))))

(defn mouse-down-handler [e]
  (let [{:keys [left top]} (bricks/get-client-rect e)
        offset             {:x (- (.-clientX e) left)
                            :y (- (.-clientY e) top)}
        on-move            (mouse-move-handler offset)]
    (reset! db/dragging-flow-editor? true)
    (do
      (gevents/listen js/window EventType.MOUSEMOVE on-move))
    (gevents/listen js/window EventType.MOUSEUP
                    (mouse-up-handler on-move))))

(re-frame/reg-event-db
 ::new-flow
 (undoable)
 (fn [db _]
   (let [animal (cstr/replace (str (tales/animal)) #" " "-")
         flow-id (str animal "-flow-" (rand-int 1000))]
     (reset! editor-mode :part-browser)
     (-> db
        ;(dissoc :flows) ;; we should really clean up old flows that aren't open anymore  TODO
         (assoc :selected-flow flow-id)
         (assoc-in [:flows flow-id] {:map {}
                                     :connections []})))))
                                    ;:name "new-flow"
                                    ;:description "new-flow"

(def trig-atom-test (reagent/atom nil))

(re-frame/reg-event-db
 ::refresh-live-dat
 (fn [db _]
   (let [kks (filter #(cstr/starts-with? (str %) ":live-dat-") (keys (get db :data)))]
     ;(tap> [::kks kks])
     (reset! trig-atom-test (str "refreshed live dat " (rand-int 1000)))
     (doseq [k kks]
       (re-frame/dispatch [::conn/clear-query-history k])))))
    ;;  (ut/dissoc-in db [:data
    ;;                    (set (keys (get db :data)))

    ;;                    ])


(defn create-out-map-ports [ddata fid bid]
  (let [;;ddata (read-string (cstr/join " " (ut/cm-deep-values %)))
        dtype (ut/data-typer ddata)
        ports {:out (keyword dtype)}
        ports (cond (= dtype "map")
                    (assoc (into {} (for [[k v] ddata] {k (keyword (ut/data-typer v))})) :* :map)
                    (or (= dtype "vector") (= dtype "rowset")) ;(= dtype "vector")
                    (assoc (into {} (for [k (range (count ddata)) :let [v (get ddata k)]] {(keyword (str "idx" k)) (keyword (ut/data-typer v))})) :* :vector)
                    :else ports)
        _ (tap> [:create-out-map-ports ddata dtype ports])
        full (-> (get @(re-frame/subscribe [::flowmap]) bid)
                ;(assoc-in [:data :user-input] ddata)
                 (assoc-in [:ports :out] ports))]
    (re-frame/dispatch [::update-flowmap-key-others bid fid nil full])))

(re-frame/reg-event-db
 ::delay-tracker
 (fn [db [_ tracker flow-id]]

   (swap! db/running-blocks assoc flow-id [])
   (swap! db/real-running-blocks assoc flow-id [])

   (-> db
       (assoc-in [:flow-results :tracker] tracker)
       (assoc-in [:flow-results :status] :done))

  ;;  (re-frame/dispatch [::wfx/request :default
  ;;                   {:message    {:kind :kill-flow
  ;;                                 :flow-id flow-id
  ;;                                 :client-name @(re-frame/subscribe [::bricks/client-name])}
  ;;                    :timeout    15000000}])
   ))

(re-frame/reg-event-db
 ::socket-response-flow
 (fn [db [_ result]]
   (let [flow-id @(re-frame/subscribe [::selected-flow])
         return-maps (get result :return-maps)
         return-maps (ut/replace-large-base64 return-maps) ;; since we loop over this, lets make it less expensive, no? wont change the data types anyways
         _ (doseq [[fid fb] return-maps] ;; backport map keys to out ports
             (doseq [[bid ddata] fb
                     :when (and (not (string? ddata)) ;;(or (map? ddata) (vector? ddata)) ;; why not just do all so old shit can get cleared? 1/31/24
                                (= (get ddata :syntax "clojure") "clojure")
                                (not (get ddata :port-in?))
                                (not (= bid :done)))]
               (create-out-map-ports ddata fid bid)))]
                        ;{[fid bid] ddata}

     (tap> [:flow-in result @http/subsequent-runs])
     (ut/dispatch-delay 600 [::refresh-live-dat])
    ;;  (tap> [:fn-history-order (vec (for [{:keys [block start]}  ;; doesnt really give what youd want...
    ;;                           (sort-by :start
    ;;                                    (get-in result [:fn-history flow-id])
    ;;                                    )
    ;;                                      ]
    ;;                       block))])
  ;(swap! db/flow-results result)
     ;(swap! db/running-blocks assoc flow-id [])

     (reset! editor-tooltip-atom (str "finished. returned value: " (get result :return-val)))
    ;(swap! db/flow-results assoc flow-id (merge {:status :done} result))

    ;;  (re-frame/dispatch [::http/set-flow-results {:status :done
    ;;                                               :return-maps (get result :return-maps)
    ;;                                               :tracker (get result :tracker)
    ;;                                               :run-refs (get result :run-refs)}])

    ;;  (reset! db/flow-results (merge {:status :done}
    ;;                                ; result
    ;;                                 {:status :done
    ;;                                  :return-maps (get result :return-maps)
    ;;                                  :tracker (get result :tracker)
    ;;                                  :run-refs (get result :run-refs)}))
     (ut/dispatch-delay 1500 [::delay-tracker (get result :tracker-history) flow-id]) ;; TODO delayed atom side effects
     (swap! bricks/progress-bars assoc flow-id 100) ;; just in case we finish early
  ;(let [] db)
     (-> db
         ;;(assoc-in [:flow-results :status] :done)
         ;;(assoc-in [:flow-results :tracker] (get result :tracker))
         (assoc-in [:flow-results :tracker] (get result :tracker-history))
         (assoc-in [:flow-results :run-refs] (get result :run-refs))))))

(re-frame/reg-event-db
 ::timeout-response
 (fn [db [_ result what-req]]
   (let []
     (tap> [:websocket-timeout! result what-req])
     db)))

(defn process-flow-map [fmap]  ;;; IF CHANGING LOGIC HERE, ALSO CHANGE IN THE SERVER VERSION
  ;; basically for turning web-specific flow stuff into a more server-friendly flowmaps format
  (into {} (for [[k v] fmap
                 :let [ttype (get-in v [:data :drag-meta :type])]]
             (cond

             ;;  (= ttype :sub-flow222) ;; kek
             ;;  (let [subs (get v :sub-flow)
             ;;        post-subs (process-flow-map (get subs :map))]
             ;;   ; (tap> [:sub-flow-preporocess subs post-subs])
             ;;    {k (-> v
             ;;           (assoc :sub-flow post-subs))})

               (= ttype :query)
               (let [pfull (first (vals (get-in v [:data :queries])))
                      ;; _ (tap> [:param-fmaps ttype k pfull])
                    ;ddata @(re-frame/subscribe [::conn/clicked-parameter-key [pfull]])
                     ddata (first @(re-frame/subscribe [::resolver/logic-and-params [pfull]]))
                     dtype (ut/data-typer ddata)
                     ports {:out {:out (keyword dtype)}}
                     ports (cond (= dtype "map")
                                 {:out (assoc (into {} (for [[k v] ddata] {k (keyword (ut/data-typer v))})) :* :map)}
                                 (or (= dtype "vector") (= dtype "rowset"))
                                 {:out (assoc (into {} (for [k (range (count ddata)) :let [v (get ddata k)]] {(keyword (str "idx" k)) (keyword (ut/data-typer v))})) :* :vector)}
                                 :else ports)
                     full (-> v
                              (assoc-in [:data :user-input] ddata)
                              (assoc-in [:ports] ports))]
                 {k full})

               (= ttype :param)
               (let [pfull (get-in v [:data :drag-meta :param-full])
                      ;; _ (tap> [:param-fmaps ttype k pfull])
                     ddata @(re-frame/subscribe [::conn/clicked-parameter-key [pfull]])
                     dtype (ut/data-typer ddata)
                   ;;  ports {:out {:out (keyword dtype)}}
                   ;;  ports (cond (= dtype "map")
                   ;;              {:out (assoc (into {} (for [[k v] ddata] {k (keyword (ut/data-typer v))})) :* :map)}
                   ;;              (or (= dtype "vector") (= dtype "rowset")) ;(= dtype "vector")
                   ;;              {:out (assoc (into {} (for [k (range (count ddata)) :let [v (get ddata k)]] {(keyword (str "idx" k)) (keyword (ut/data-typer v))})) :* :vector)}
                   ;;              :else ports)
                     full (-> v
                              (assoc-in [:data :user-input] ddata)
                              (assoc-in [:ports] {:out {:out (keyword dtype)}}))]

                 {k full})
                     ;; (first (resolver/logic-and-params [cell-ref] nil))

               (= ttype :cell)
               (let [pfull (get-in v [:data :drag-meta :param-full])
                      ;; _ (tap> [:param-fmaps ttype k pfull])
                           ;ddata @(re-frame/subscribe [::conn/clicked-parameter-key [pfull]])
                     ddata (first @(re-frame/subscribe [::resolver/logic-and-params [pfull]]))
                     dtype (ut/data-typer ddata)
                     ports {:out {:out (keyword dtype)}}
                     ports (cond (= dtype "map")
                                 {:out (assoc (into {} (for [[k v] ddata] {k (keyword (ut/data-typer v))})) :* :map)}
                                 (or (= dtype "vector") (= dtype "rowset")) ;(= dtype "vector")
                                 {:out (assoc (into {} (for [k (range (count ddata)) :let [v (get ddata k)]] {(keyword (str "idx" k)) (keyword (ut/data-typer v))})) :* :vector)}
                                 :else ports)
                     full (-> v
                              (assoc-in [:data :user-input] ddata)
                             ;(assoc-in [:ports] ports) ;;; ?
                              (assoc-in [:ports] {:out {:out (keyword dtype)}}))]

                 {k full})

               (= ttype :open-block)
               (let [pfull (if (cstr/includes? (str (get-in v [:data :syntax] "clojure")) "clojure")
                             (get-in v [:data :user-input])
                             (try
                               (cstr/join "\n" (get-in v [:data :user-input]))
                               ;(get-in v [:data :user-input])
                               (catch :default _
                                 (do (tap> [:error-convering-string-types v :line 347 :flows-cljs])
                                     [(str (get-in v [:data :user-input]))])))) ;; converting raw data to stringified data, to avoid issues with edn/read-string

                       ;;     _ (tap> [:user-input-fmaps ttype k pfull])
                           ;ddata (resolver/logic-and-params pfull nil) ;@(re-frame/subscribe [::conn/clicked-parameter-key [pfull]])
                     ddata (first @(re-frame/subscribe [::resolver/logic-and-params [pfull]]))
                     dtype (ut/data-typer ddata)
                     ports {;:in (get-in v [:ports :in])
                            :out {:out (keyword dtype)}}
                     ports (cond (= dtype "map")
                                 {;:in (get-in v [:ports :in])
                                  :out (assoc (into {} (for [[k v] ddata] {k (keyword (ut/data-typer v))})) :* :map)}
                                 (or (= dtype "vector") (= dtype "rowset")) ;(= dtype "vector")
                                 {;:in (get-in v [:ports :in])
                                  :out (assoc (into {} (for [k (range (count ddata)) :let [v (get ddata k)]] {(keyword (str "idx" k)) (keyword (ut/data-typer v))})) :* :vector)}
                                 :else ports)
                     ports (merge (get v :ports) ports)
                     full (-> v
                              (assoc-in [:data :user-input] ddata)
                              (assoc-in [:ports] ports))]
             ; (tap> [:param-step {k full}])
                 {k full})
               :else {k v}))))

(defn process-flowmap2 [flowmap flowmaps-connections fid] ;;; IF CHANGING LOGIC HERE, ALSO CHANGE IN THE SERVER VERSION
  ;; the flowmap param is derived from ::flowmaps and has already been procced by process-flow-map
  (let [canvas-key (into {} (for [[k {:keys [w h x y]}] flowmap]
                              {k {:w w :h h :x x :y y :view-mode "text"}}))
        flowmaps-connections (vec (for [[c1 c2] flowmaps-connections]
                                    (if (cstr/ends-with? (str c1) "/*")
                                      [(keyword (-> (ut/unkeyword (str c1)) (cstr/replace "/*" "") (cstr/replace ":" ""))) c2] [c1 c2])))
        components-key (into {} (for [[k {:keys [data ports view file-path raw-fn flow-id sub-flow]}] flowmap ;; <-- flow-id refers to the subflow embed, not the parent
                                      :let [ttype (or (get-in data [:flow-item :type])                        ;; ^^-- fid is the parent flow-id
                                                      (get-in data [:drag-meta :type]))
                                            try-read  (fn [x] (try (edn/read-string x) (catch :default _ x)))
                                            view-swap (fn [obody flow-id bid push-key]
                                                        (let [pkey       (keyword (str (cstr/replace (str push-key) ":" "") ">"))
                                                              kps        (ut/extract-patterns obody pkey 2)
                                                              logic-kps  (into {} (for [v kps]
                                                                                    (let [[_ that] v]
                                                                                      {v [:push> [flow-id (str bid) that]]})))]
                                                          (walk/postwalk-replace logic-kps obody)))
                                            view-swap2 (fn [obody flow-id bid push-key]
                                                         (let [pkey       (keyword (str (cstr/replace (str push-key) ":" "") ">"))
                                                               kps        (ut/extract-patterns obody pkey 1)
                                                               logic-kps  (into {} (for [v kps]
                                                                                     (let [[_] v]
                                                                                       {v [:push>> [flow-id (str bid)]]})))]
                                                           (walk/postwalk-replace logic-kps obody)))
                                            view-swaps (fn [obody flow-id push-key-bid-pairs]
                                                         (reduce (fn [body [push-key bid]]
                                                                   (-> body
                                                                       (view-swap flow-id bid push-key)
                                                                       (view-swap2 flow-id bid push-key)))
                                                                 obody
                                                                 push-key-bid-pairs))
                                            view (when view (let [conns (vec (filter #(cstr/includes? (str (first %)) "/push-path") flowmaps-connections))
                                                                  push-key-bid-pairs (vec (for [[c1 c2] conns] [(keyword (last (cstr/split (str c1) #"/"))) c2]))
                                                                  view (view-swaps view fid push-key-bid-pairs)]
                                                              ;(tap> [:view-builder k push-key-bid-pairs view fid])
                                                              view))
                                            fn-key (try-read (get-in data [:flow-item :name] ":unknown!"))
                                            fn-category (try-read (get-in data [:flow-item :category] ":unknown!"))]]
                                  (cond

                                    (and (= ttype :open-block) (not (empty? (get ports :in)))) ;; open block with inputs
                                    {k {:data (get data :user-input)
                                        :default-overrides (get-in data [:flow-item :defaults] {})
                                        :inputs (vec (keys (get ports :in)))}}

                                    (or (= ttype :open-block) (= ttype :cell) (= ttype :param))
                                    {k (if (= (get data :syntax "clojure") "clojure")
                                         (get data :user-input)
                                         ;(cstr/join "\n" (get data :user-input))
                                         (get data :user-input))}

                                    (= ttype :open-fn)
                                    {k (merge
                                        (when view {:view view})
                                        {:fn raw-fn :raw-fn raw-fn
                                         :default-overrides (get-in data [:flow-item :defaults] {})
                                         :inputs (vec (keys (get ports :in)))})}

                                    (= ttype :query)
                                    {k {:fn (get data :user-input) ;'(fn [x] x) ;:raw-fn '(fn [x] x)
                                        :inputs (vec (keys (get ports :in)))}}

                                    (= ttype :sub-flow)
                                    {k (-> (process-flowmap2 (get sub-flow :map) (get sub-flow :connections) fid)
                                           (assoc :file-path file-path)
                                           (assoc :flow-id flow-id))} ;; flow-id of the embdeeded flow, NOT the parent
                                               ;; {k {:components (get sub-flow :map)
                                               ;;     :connections (get sub-flow :connections)}}
                                            ;;(= ttype :param) {k (get data :user-input)}

                                    :else {k {:fn-key [fn-category fn-key]
                                              :default-overrides (get-in data [:flow-item :defaults] {})
                                              :inputs (if false ;expandable-in?
                                                        [(vec (keys (get ports :in)))] ;; send as single value to apply%
                                                        (vec (keys (get ports :in))))}})))
        components-key (into {} ;; deconstruct conditional paths to single condi key
                             (for [[k v] flowmap]
                               (if (not (empty? (get v :cond)))
                                 (let [ccond (into {}
                                                   (for [[c1 c2] flowmaps-connections
                                                         :let [link (keyword (str (cstr/replace (str k) ":" "") "/cond-path"))
                                                               ff (cstr/split (str c1) #"/")
                                                               cname (keyword (last ff))
                                                               c2wo (keyword (first (cstr/split (cstr/replace (str c2) ":" "") #"/")))] ;; w/o port id.... TEST
                                                         :when (cstr/starts-with? (str c1) (str link))]
                                                     {c2wo (get-in v [:cond cname :fn])}))]
                                   {k (assoc (get components-key k) :cond ccond)})
                                 {k (get components-key k)})))
        flowmaps-connections (vec (filter #(and
                                            (not (cstr/includes? (str (first %)) "/cond-path"))
                                            (not (cstr/includes? (str (first %)) "/push-path"))) flowmaps-connections))
        server-flowmap {:canvas canvas-key :components components-key :connections flowmaps-connections}]
    server-flowmap))

;;(defn make-server-version)

(defn has-done? []
  (true? (some #(= % :done)
               (apply concat @(re-frame/subscribe [::flowmap-connections])))))

(re-frame/reg-event-db
 ::run-current-flowmap
 (fn [db _]
  ;(tap> [:ran-condi ])
   (when (get db :flow?) ;; and has-done?
   ;; only when flow is up and runnable
     (doall (let [flowmap @(re-frame/subscribe [::flowmap])
                  flow-id @(re-frame/subscribe [::selected-flow])
                  flowmaps-connections @(re-frame/subscribe [::flowmap-connections])
                  client-name @(re-frame/subscribe [::conn/client-name])
                  opts-map (get-in db [:flows flow-id :opts] {})
                  server-flowmap (process-flowmap2 flowmap flowmaps-connections flow-id)
                 ;running-subs (vec (conj (for [k (keys (get server-flowmap :components))] [flow-id k]) [flow-id :*running?]))
                  running-view-subs (vec (for [[k v] (get server-flowmap :components) :when (get v :view)]
                                           [flow-id (keyword (str (cstr/replace (str k) ":" "") "-vw"))]))
                  running-subs (vec (for [k (keys (get server-flowmap :components))] [flow-id k]))
                  running-subs (vec (into running-subs running-view-subs))
                  fstr (str "running flow " flow-id)
                  w (/ (count fstr) 4.1)]
              (reset! editor-tooltip-atom (str flow-id " is running"))
           ;(swap! db/flow-results assoc flow-id {:status :started})
            ;(reset! db/flow-results {:status :started})
              (re-frame/dispatch [::http/set-flow-results {:status :started}])
              (swap! db/running-blocks assoc flow-id (vec (keys flowmap)))
              (tap> [:flowmap-send-it flowmap server-flowmap running-subs])
              (reset! http/subsequent-runs [])
              (re-frame/dispatch [::wfx/request :default
                                  {:message    {:kind :sub-to-running-values
                                                :flow-keys running-subs
                                                :client-name client-name}
                                   :timeout    15000000}])
            ;;  (re-frame/dispatch [::audio/text-to-speech11 :audio :speak "/home/ryanr/fight-like-hell.mp3" true])
              (re-frame/dispatch [::wfx/request :default
                                  {:message    {:kind :run-flow
                                                :flow-id flow-id
                                                :no-return? true ;;false ; true ;false ;true  ;; if we arent subbing to running values, we need to return dump
                                                :opts {:increment-id? false
                                                       :opts opts-map}
                                                :flowmap server-flowmap
                                                :client-name client-name}
                                   :on-response [::socket-response-flow]
                                   :on-timeout  [::timeout-response :run-flow flowmap] ;; requeue?
                                   :timeout    15000000}])
              (ut/dispatch-delay 800 [::http/insert-alert fstr w 1 5])
            ;;  (re-frame/dispatch [::conn/click-parameter ;; kinda cheating, but feels better
            ;;                      [:flow (keyword (str flow-id ">*running?"))] true])
              (dissoc db :flow-runner))))))

(re-frame/reg-sub
 ::flow-runner
 (fn [db [_ flow-id bid]]
   (and (not (empty? (get @db/running-blocks flow-id [])))
        (= (get-in db [:flow-runner flow-id bid] :started) :started))))

;(tap> [:results-atom @(re-frame/subscribe [::http/flow-results])])


                     ;@flowmaps-connections

(defn is-running? [bid flow-id & [only?]]
  (let [started? @(re-frame/subscribe [::flow-runner flow-id bid])
        rblocks (get @db/running-blocks flow-id [])
        chans-open? @(re-frame/subscribe [::bricks/flow-channels-open? flow-id])
        server-running? (or (if only? false
                                chans-open?)
                         ;chans-open?
                            @(re-frame/subscribe [::conn/clicked-parameter-key [(keyword (str "flow/" flow-id ">*running?"))]]))
        rrblocks (get @db/real-running-blocks flow-id [])]
    (if (= bid :*)
      server-running?
      (and (some #(= bid %) rrblocks)
           server-running?))))

(re-frame/reg-sub
 ::is-running?
 (fn [_ [_ bid flow-id & [only?]]]
   (if only?
     (is-running? bid flow-id true)
     (is-running? bid flow-id))))

(re-frame/reg-sub
 ::is-waiting?
 (fn [_ [_ bid flow-id]]
   (let [started? @(re-frame/subscribe [::flow-runner flow-id bid])
         rblocks (get @db/running-blocks flow-id [])
         rrblocks (get @db/real-running-blocks flow-id [])]
     (and
      (some #(= bid %) rblocks)
      (not (some #(= bid %) rrblocks))
      started?))))

(def tentacle-pos (reagent/atom [0 0]))
(def tentacle-start (reagent/atom [0 0]))



;(defn build-ports-by-value [v])
; (re-frame/subscribe [::flowmap])


(re-frame/reg-sub
 ::flowmap
 (fn [db _]
   (let [;fmap @flowmaps
         selected-flow (get db :selected-flow)
         fmap (get-in db [:flows selected-flow :map])
         fmap (into {} (for [[k v] fmap
                             :when (get v :w)]
                         {k v}))]
     (process-flow-map fmap))))

(defn calc-block-widths-fn [inputs outputs w]
  (let [safe-w 70 ;(- w 50)
        px-per-char 7

        longesti (apply max (map #(count (str %)) (conj (keys inputs) 10))) ;; 10 avoid empty list
        longesti-w (+ (* px-per-char longesti) 4)
        longesti-w (if (< longesti-w safe-w) safe-w longesti-w)

        longesto (try (apply max (map #(count (str %)) (conj (keys outputs) 10))) (catch :default _ 0))
        longesto-w (+ (* px-per-char longesto) 4)
        longesto-w (if (< longesto-w safe-w) safe-w longesto-w)

       ;longesto-w (if (< longesto-w safe-w) safe-w longesti-w)
        both (+ longesti-w longesto-w)
        cw 180 ;; forcing 180
        spacer 2
        w (+ cw both spacer)]
    [w cw longesti-w longesto-w]))

(def calc-block-widths-atom (reagent/atom {}))

(defn calc-block-widths [inputs outputs w]
  (let [cache (get @calc-block-widths-atom [inputs outputs w])]
    (if cache cache
        (let [c (calc-block-widths-fn inputs outputs w)]
          (swap! calc-block-widths-atom assoc [inputs outputs w] c)
          c))))

(defn map-value-box [s k-val-type]
  (let [render-values? true
        function? (or (fn? s) (try (fn? s) (catch :default _ false)))]
    (cond (ut/hex-color? s) [re-com/h-box
                             :gap "3px"
                             :children [[re-com/box :child (str s)]
                                        [re-com/box :child " "
                                         :width "13px"
                                         :height "13px"
                                            ;:size "auto"
                                         :style {:background-color (str s)
                                                 :border-radius "2px"}]]]

          (ut/is-base64? s)
          ;;[re-com/box :child (str "base64: " (count s) " bytes")]
          ;[re-com/box :child (str "huge base64 string: ~" (/ (* (count s) 6) 8 1024 1024) " MB")]
          [re-com/v-box
           :size "auto"
           :gap "10px"
           :children
           [[re-com/box
             :align :end :justify :end
             :style {:opacity 0.5}
             :child (str "**huge base64 string: ~" (.toFixed (/ (* (count s) 6) 8 1024 1024) 2) " MB")]
            ;; [re-com/box
            ;;  :style {:opacity 0.5}
            ;;  :child "(attempted image preview below - warning: data element is still a huge string)"]
            [re-com/box
             :size "auto"
             :child [:img {:src (str "data:image/png;base64," s)}]]]]

          (or function?
              (= k-val-type "function")) (str (.-name s) "!") ;; temp

          (string? s) [re-com/box
                       :size "auto"
                       :align :end :justify :end
                       :style {:word-break "break-all"}
                       :child (str "\"" s "\"")]
          :else (cstr/replace (str s) #"clojure.core/" ""))))

(declare draggable-port)

(defn draggable-pill [m _ element]
  (draggable-port element (get m :flow-id) (get m :from) m _ nil nil)
  ;;[draggable-port icon flow-id bid k v x y]
  ;;element
) ;; stub

(defn sql-explanations-kp []
  {[:from 0 0] "table-name"
   [:from 0 1] "table-alias"
   [:from] "the table we are selecting from"})

;(defn _draggable-piu)

(defn map-boxes2 [data block-id flow-name keypath kki init-data-type & [draggable?]]
  ;(tap> [:pre-data data])
  (let [;data (if (seq? data) data [data])
        sql-explanations (sql-explanations-kp)
        ;eval? false
        data (if (or (string? data) (number? data)) [data] data)
        base-type-vec? (or (vector? data) (list? data))
        iter (if base-type-vec? (range (count data)) (keys data))
        ;only-body false ;(mapify-data block-id true)
        font-size "11px"
        ;iii @hovered-input
        ;options {} ; (into {} (for [{:keys [block-keypath current]} @(re-frame/subscribe [::options block-id])] {(last block-keypath) current}))
        ;show-code-map-opt? (get options :show-code-map? true)
        add-kp? (try (keyword? block-id) (catch :default _ false)) ;;true ;(not show-code-map-opt?) ;; for drag outs only
        cells? (= block-id :cells)
        main-boxes [re-com/v-box ;(if cells? re-com/h-box re-com/v-box)
                    :size "auto"
                    :padding "5px"
                    :width (when cells? "280px")
                    :gap "2px"
                    :style {:color "black"
                            :font-family "Poppins"
                            :font-size font-size; "11px"
                            :font-weight 500}
                    :children
                    (for [kk iter] ;; (keys data)
                      (let [k-val (get-in data [kk])
                            k-val-type (ut/data-typer k-val)
                            in-body? true ;(ut/contains-data? only-body k-val)
                            hovered? false ;(ut/contains-data? mat-hovered-input k-val)
                            border-ind (if in-body? "solid" "dashed")
                            val-color (get @(re-frame/subscribe [::conn/data-colors]) k-val-type)
                            keypath-in (conj keypath kk)
                            keystyle {:background-color (if hovered? (str val-color 66) "#00000000")
                                      ;:font-weight 700
                                      :color val-color
                                      :border-radius "12px"
                                      :border (str "3px " border-ind " " val-color)}
                            valstyle {:background-color (if hovered? (str val-color 22) "#00000000") ;"#00000000"
                                      :color val-color
                                      :border-radius "12px"
                                      :border (str "3px " border-ind " " val-color 40)}]
                        ^{:key (str block-id keypath kki kk)}
                        [re-com/box
                         :child

                         (cond (= k-val-type "map")
                               ^{:key (str block-id keypath kki kk k-val-type)}
                               [re-com/h-box
                                :children
                                [[draggable-pill
                                  {:from block-id
                                   :new-block [:artifacts "text"]
                                   :idx 0 ;idx
                                   :keypath-in keypath-in
                                   :flow-id flow-name
                                   ;:keypath (if is-map? [:map [idx]] ref)
                                   :keypath [:map (if add-kp?
                                                    (vec (cons :v keypath-in))
                                                    keypath-in)]}

                                  block-id ;; (when (not (= block-id :inline-render)) block-id)

                                  ^{:key (str block-id keypath kki kk k-val-type 1)}
                                  [re-com/v-box
                                   :min-width (px (*
                                                   (count (str kk))
                                                   11)) ;"110px"
                                   ;:size "auto"
                                   :style {:cursor (when draggable? "grab")
                                           ;:border "1px solid white"
                                           }
                                   :children [^{:key (str block-id keypath kki kk k-val-type 124)}
                                              [re-com/box :child (str kk)]
                                              ^{:key (str block-id keypath kki kk k-val-type 134)}
                                              [re-com/box :child (str k-val-type)
                                               :style {:opacity 0.3
                                                       :font-size font-size; "9px"
                                                       ;:font-weight 400
                                                       :padding-top "7px"}]
                                              (when (> (count k-val) 1)
                                                ^{:key (str block-id keypath kki kk k-val-type 156)}
                                                [re-com/box
                                                 :style {:opacity 0.3}
                                                 :child (str "(" (count k-val) ")")])]
                                   :padding "8px"]]
                                 (map-boxes2 k-val block-id flow-name keypath-in kk nil draggable?)]
                                :style keystyle]

                               (or (= k-val-type "vector") (= k-val-type "list") ; (= k-val-type "function")
                                   (= k-val-type "rowset")
                                   (= k-val-type "jdbc-conn") (= k-val-type "render-object"))

                               ^{:key (str block-id keypath kki kk k-val-type 2)}
                               [re-com/h-box
                                :style {:border-radius "12px"
                                        :border "3px solid black"}
                                :children
                                [[draggable-pill
                                  {:from block-id
                                   :new-block [:artifacts "text"]
                                   :idx 0 ;idx
                                   :keypath-in keypath-in
                                   :flow-id flow-name
                                   :keypath [:map (if add-kp?
                                                    (vec (cons :v keypath-in))
                                                    keypath-in)]}
                                  block-id
                                  ^{:key (str block-id keypath kki kk k-val-type 3)}
                                  [re-com/v-box
                                   ;:size "auto"
                                   :min-width (px (*
                                                   (count (str kk))
                                                   11)) ;"110px"
                                   :style {:cursor (when draggable? "grab") ;;; skeptical, ryan 5/24/33
                                           }
                                   :children (if (and (= k-val-type "list")
                                                      (= (ut/data-typer (first k-val)) "function"))

                                               [^{:key (str block-id keypath kki kk k-val-type 4)}
                                                [re-com/h-box
                                                 :style {:margin-top "4px"}
                                                 :gap "1px"
                                                 :children [;[re-com/box :child (str kk) :style {:opacity 0.25}]
                                                            [re-com/box :child "("
                                                             :style {;:opacity 0.5
                                                                     :color "orange"
                                                                     :margin-top "-2px"
                                                                     :font-size "14px"
                                                                     :font-weight 700}]
                                                            ^{:key (str block-id keypath kki kk k-val-type 5)}
                                                            [re-com/box :child (str (first k-val)) ; (str  "(")
                                                             :style {:color "#ffffff"
                                                                     ;:margin-left "-5px"
                                                                     :font-size font-size ;"17px"
                                                                     ;:opacity 0.7
                                                                     }]]]
                                                 ;[re-com/box :child (str kk)]
                                                ^{:key (str block-id keypath kki kk k-val-type 6)}
                                                [re-com/box :child (str k-val-type)
                                                 :style {:opacity 0.3
                                                         :font-size font-size ; "9px"
                                                         ;:font-weight 400
                                                         :padding-top "16px"}]
                                                (when (> (count k-val) 1)
                                                  ^{:key (str block-id keypath kki kk k-val-type 827)}
                                                  [re-com/box
                                                   :style {:opacity 0.3}
                                                   :child (str "(" (count k-val) ")")])]

                                               [(when true ;(not (get sql-explanations keypath ""))
                                                  ^{:key (str block-id keypath kki kk k-val-type 7)}
                                                  [re-com/box :child (str kk)])

                                                (when true ; (not (get sql-explanations keypath ""))
                                                  ^{:key (str block-id keypath kki kk k-val-type 8)}
                                                  [re-com/box :child (str (when (= (count k-val) 0) "empty ") k-val-type)
                                                   :style {:opacity 0.3
                                                           :font-size font-size ; "9px"
                                                         ;:font-weight 400
                                                           :padding-top "7px"}])

                                                (when (not (empty? (get sql-explanations keypath "")))
                                                  ^{:key (str block-id keypath kki kk k-val-type 82)}

                                                  [re-com/md-icon-button
                                                   :md-icon-name "zmdi-info"
                                                   :tooltip (str "FOOO" (get sql-explanations keypath ""))
                                                   ;:on-click #(re-frame/dispatch [::change-page panel-key (first data-keypath) (- page-num 1) ttl-pages])
                                                   :style {:font-size "16px"
                                                           :cursor "pointer"
                                                           :opacity 0.5
                                                           :padding "0px"
                                                           :margin-top "-1px"}])])

                                   :padding "8px"]]
                                 [map-boxes2
                                  (if (= k-val-type "rowset")
                                    (zipmap (iterate inc 0) (take 10 k-val))
                                    (zipmap (iterate inc 0) k-val))
                                  ; (zipmap (iterate inc 0) k-val)
                                  block-id flow-name keypath-in kk nil draggable?]]
                                :style keystyle]

                               :else
                               ^{:key (str block-id keypath kki kk k-val-type 9)}
                               [re-com/h-box
                                :children [^{:key (str block-id keypath kki kk k-val-type 10)}
                                           [re-com/h-box
                                            :gap "6px"
                                            :children [[draggable-pill
                                                        {:from block-id
                                                         :new-block [:artifacts "text"]
                                   ;:override (when is-map? src-block-id)
                                                         :idx 0 ;idx
                                   ;:keypath (if is-map? [:map [idx]] ref)
                                                         :keypath-in keypath-in
                                                         :flow-id flow-name
                                                         :keypath [:map (if add-kp?
                                                                          (vec (cons :v keypath-in))
                                                                          keypath-in)]}
                                                        block-id
                                                        ^{:key (str block-id keypath kki kk k-val-type 11)}
                                                        [re-com/box :child (str kk)
                                                         :style {:cursor (when draggable? "grab")
                                                                ;:border "1px solid white"
                                                                 }]]

                                                       (when true ;(not (get sql-explanations (vec (conj keypath kk)) ""))
                                                         ^{:key (str block-id keypath kki kk k-val-type 12)}
                                                         [re-com/box :child (str k-val-type)
                                                          :style {:opacity 0.3
                                                                  :font-size font-size  ;"9px"
                                                                  :font-style (if (=  k-val-type "function")
                                                                                "italic" "normal")
                                                                ;:font-weight 400
                                                                  }])
                                                       (when (get sql-explanations (vec (conj keypath kk)) "")
                                                         ^{:key (str block-id keypath kki kk k-val-type 822)}
                                                         [re-com/box
                                                          :style {:opacity 0.3}
                                                          :child (str  (get sql-explanations (vec (conj keypath kk)) ""))])]]
                                           ^{:key (str block-id keypath kki kk k-val-type 13)}
                                           [re-com/box

                                            :size "auto"
                                            :align :end :justify :end
                                            :child

                                            [map-value-box k-val]
                                            :style {;:font-weight 500
                                                    :line-height "1.2em"
                                                    :padding-left "5px"}]]
                                :justify :between
                                :padding "5px"
                                :style valstyle])]))]]
        ;main-boxes
    (if (= keypath [])

      (let [k-val-type (ut/data-typer data)
            nin? (not (= block-id :inline-render))
            val-color (get @(re-frame/subscribe [::conn/data-colors]) k-val-type)]
        [re-com/v-box
         :size "auto"
         :children
         [[re-com/v-box
           :style {:word-wrap "break-word"
                   :overflow-wrap "break-word"}
           :children [(when nin?
                        [re-com/v-box
                       ;:justify :end
                         :padding "6px"
                     ;:size "300px"
                         :children [^{:key (str block-id keypath kki 00 k-val-type 00002)}
                                    [re-com/box :child (str flow-name) ;(str k-val-type)
                                     :align :end
                                     :style {:opacity 0.3
                                           ;:font-weight 400
                                           ;:font-size "14px"
                                             :font-size font-size  ;"9px"
                                             :margin-top "-7px"
                                             :margin-bottom "-5px"
                                           ;:padding-top "7px"
                                             }]]])
                    ;[re-com/box :child  :size "auto"]
                      main-boxes]
           :padding "0px"]]
         :style {:font-family "Poppins"
                 :color val-color
                 :margin-top (if nin? "0px" "-10px")
                 :border-radius "12px"}])
      main-boxes)))

(defn coord-cacher333 [conns flow-map]
  (let [cachekey (hash (str conns flow-map))
        cache (get @ut/coord-cache cachekey)]
    (if false cache ;; cache cache ;; disable for now
        (let [coords (for [[o i] conns
                           :let [ns-i? (gns i)
                                 ns-o? (gns o)
                                 bid-i (if ns-i? (keyword (gns i)) i)
                                 pid-i (if ns-i? (keyword (gn i)) :in)
                                 bid-o (if ns-o? (keyword (gns o)) o)
                                 pid-o (if ns-o? (keyword (gn o)) :out)
                                 ii (get-in flow-map [bid-i :ports :in] {:in "unknown"})
                                 oo (get-in flow-map [bid-o :ports :out] {:out "unknown"})
                                 [w cw iox iix] (calc-block-widths ii oo 0)
                                 w (- w 15) ;; spacer
                                 ik (keys ii)
                                 ok (keys oo)
                                 [ix iy] [(get-in flow-map [bid-i :x]) (get-in flow-map [bid-i :y])]
                                                    ;(get-in flow-map [bid-i :w]) (get-in flow-map [bid-i :h])

                                 [ox oy ow] [(get-in flow-map [bid-o :x])
                                             (get-in flow-map [bid-o :y])
                                            ;(get-in flow-map [bid-o :w])
                                             w]
                                                    ;(get-in flow-map [bid-o :h])
                                 >=zero (fn [x] (if (< x 0) (- (count ok) 1) x)) ;; in case we can't find the port, default to the last position
                                 i-idx (>=zero (.indexOf ik pid-i))
                                 o-idx (>=zero (.indexOf ok pid-o))
                               ; _ (tap> [o i i-idx o-idx ox oy])
                                 y-offset 44
                                 port-size 17
                                 x1 (+ ox w -25)
                                 y1 (+ (+ 3 oy (* port-size o-idx)) y-offset)
                                 x2 (+ ix 11)
                                 y2 (+ (+ 3 iy (* port-size i-idx)) y-offset)
                                 dcolor (get (theme-pull :theme/data-colors db/data-colors)
                                             (gn (get-in flow-map [bid-o :ports :out pid-o]))
                                                 ;(gn (get-in flow-map [bid-i :ports :in pid-i]))
                                             (get (theme-pull :theme/data-colors db/data-colors) "unknown" "#FFA500"))]]
                       [x1 y1 x2 y2 true dcolor bid-o bid-i])]
         ;(tap> [:coords coords])
         ;(swap! ut/coord-cache assoc cachekey coords)
          coords))))

(defn coord-cacher [conns flow-map]
  (let [results-hash (hash (get @(re-frame/subscribe [::http/flow-results]) @(re-frame/subscribe [::selected-flow])))
        cachekey (str (hash (str conns flow-map)) "." results-hash)
        flow-id @(re-frame/subscribe [::selected-flow])
        react! [@(re-frame/subscribe [::http/flow-results])]
        cache (get @ut/coord-cache cachekey)]
   ;(tap> [:conns conns])
    (if false cache ;; disable for now
        (let [coords (for [[o i] conns
                           :let [ns-i? (gns i)
                                 ns-o? (gns o)
                                 bid-i (if ns-i? (keyword (gns i)) i)
                                 pid-i (if ns-i? (keyword (gn i)) :in)
                                 bid-o (if ns-o? (keyword (gns o)) o)
                                 pid-o (if ns-o? (keyword (gn o)) :out)

                                 ii (get-in flow-map [bid-i :ports :in] {:in "unknown"})

                                 oop (get-in flow-map [bid-o :cond] {}) ;; conditional pathing
                                 oopp (get-in flow-map [bid-o :push] {}) ;; push pathing
                                 ooo (get-in flow-map [bid-o :ports :out] {:out "unknown"})
                                 oo (merge ooo oop)
                                ;;_ (tap> [:ii bid-i ii :oo bid-o oo])

                                ;[w cw iox iix] (calc-block-widths ii oo 0)
                                ;w 25
                               ; w (- w 15) ;; spacer

                                 ik (keys ii)
                                 ok (try (into (into (vec (sort-by str (keys ooo))) (vec (keys oop))) (vec (keys oopp)))
                                         (catch :default e (do (tap> [:error-sorting-keys e (keys oo) :flows.cljs :ln 1148])
                                                               (into (into (vec (sort-by str (keys ooo))) (vec (keys oop))) (vec (keys oopp))))))
                                ;;  _ (when
                                ;;     (= bid-o :open-fn-3)
                                ;;      (tap> [:ok bid-o  ok oop]))
                                 [ix iy iw] [(get-in flow-map [bid-i :x])
                                             (get-in flow-map [bid-i :y])
                                             (get-in flow-map [bid-i :w])]
                                               ;(- (get-in flow-map [bid-i :h]) 6)


                                 [ox oy ow oh] [(get-in flow-map [bid-o :x])
                                                (get-in flow-map [bid-o :y])
                                                (get-in flow-map [bid-o :w])
                                                (- (get-in flow-map [bid-o :h]) 3)]

                                 >=zero (fn [x] (if (< x 0) (- (count ok) 1) x)) ;; in case we can't find the port, default to the last position

                                 i-idx (try (>=zero (.indexOf ik pid-i)) (catch :default _ -1))
                                 o-idx (try (>=zero (.indexOf ok pid-o)) (catch :default _ -1))
                               ;;  _ (tap> [:t o i i-idx o-idx ox oy])
                                 y-offset 0 ;44
                                 iport-size (/ (- iw 4) (count ik))
                                 oport-size (/ (- ow 4) (count ok))
                                ;x1 (+ ox w -25)
                                 x1 (+ ox (* o-idx oport-size) 10.5)
                                 y1 (+ (+ 3 oy) oh -6.5)

                                 x2 (+ ix (* i-idx iport-size) 10.5)
                                 y2 (+ (+ 3 iy) 3)
                               ;;  _ (tap> [:zz ox oy :ff x1 y1 x2 y2])
                                ;;  dcolor (get (theme-pull :theme/data-colors db/data-colors)
                                ;;              (gn (get-in flow-map [bid-o :ports :out pid-o]))
                                ;;              (get (theme-pull :theme/data-colors db/data-colors) "unknown" "#FFA500"))
                                 ;[ibid ivk] (if input? @(re-frame/subscribe [::get-incoming-port bid :in k]) [nil nil])
                                 ;port-color @(re-frame/subscribe [::port-color flow-id (or ibid bid) (or ivk k)])
                                 dcolor @(re-frame/subscribe [::port-color flow-id bid-o pid-o])]]
                                ;;dcolor "#ffffff"


                       [x1 y1 x2 y2 true dcolor bid-o bid-i pid-o pid-i])]
                      ;[500 500 1200 1200 true "#ffffff" :tet :tete]

         ;(tap> [:coords coords])
        ; (swap! ut/coord-cache assoc cachekey coords)
          coords))))

(defn translate-x-y [[x y]]
  (let [zoom-multi (get @db/pan-zoom-offsets 2)
        zoom-offset-x (get @db/pan-zoom-offsets 0)
        zoom-offset-y (get @db/pan-zoom-offsets 1)
        drop-x      (- (/ x zoom-multi) (/ zoom-offset-x zoom-multi))
        drop-y      (- (/ y zoom-multi) (/ zoom-offset-y zoom-multi))]
    [drop-x drop-y]))

(defn generate-tentacle-coord22 []
  (let [[x2 y2] (translate-x-y @tentacle-pos)
        [x1 y1] (translate-x-y @tentacle-start)
        [xx yy] @detached-coords
        dcolor  (get (theme-pull :theme/data-colors db/data-colors)
                     (gn (last @dragged-port))
                     (get (theme-pull :theme/data-colors db/data-colors) "unknown" "#FFA500"))
        row [x1 (+ y1 11)
             (- x2 xx)
             (- y2 yy)
             false dcolor nil nil]]
   ;(tap> [:tentacle-row @tentacle-pos @tentacle-start row])
    row))

(defn generate-tentacle-coord []
  (let [[x2 y2] @tentacle-pos
        [x1 y1] @tentacle-start
        [xx yy] @detached-coords
        dcolor  (get (theme-pull :theme/data-colors db/data-colors)
                     (gn (last @dragged-port))
                     (get (theme-pull :theme/data-colors db/data-colors) "unknown" "#FFA500"))
        row [x1 (+ y1 11)
             (- x2 xx 8) (- y2 yy 25)
             false dcolor nil nil]]
   ;(tap> [:tentacle-row @tentacle-pos @tentacle-start row])
    row))

(defn generate-coords [xx yy]
  (let [conns (vec (filter #(not (= (last %) :done))
                          ;@flowmaps-connections
                           @(re-frame/subscribe [::flowmap-connections])))
       ;;tt @tentacle-pos
        flow-map @(re-frame/subscribe [::flowmap])
        fconns (coord-cacher conns flow-map)]
   ;;(tap> [:generate-coords conns fconns])
   ;; (if @dragging-port?
   ;;   (conj fconns (generate-tentacle-coord))
   ;;   fconns)
    fconns))

(defn draw-tentacle [coords]
  (doall (for [[x1 y1 x2 y2 involved? color z1 z2] coords]
           ^{:key (hash (str x1 y1 x2 y2 "lines-flow"))}
           (let [zoom-multi (get @db/pan-zoom-offsets 2)] ;;[flow-id @(re-frame/subscribe [::selected-flow])]
             [:path {:stroke-width (* 8 zoom-multi) ;(if involved? 16 13)
                     :stroke       color ;;(if involved? color "#ffffff22") ;(if (or involved? nothing-selected?)
                    ;:style      {:animation "rotate linear infinite"}
                    ;:animation "rotate linear infinite"
                    ;:z-index 20
                   ; :class (when (is-running? z1 flow-id) "flow-right") ;;(some #(= z1 %) @db/running-blocks)
                    ;(if (nil? color) "pink" color) "#E6E6FA")
                   ; :fill         "green" ;"none"
                     :stroke-dasharray (str (* 10 zoom-multi) " " (* 8 zoom-multi))
                    ;:stroke-linecap "round"
                     :fill         "none" ;;(str color 45) ;; could be a cool "running effect"
                     :filter       "drop-shadow(0.25rem 0.35rem 0.4rem rgba(0, 0, 0, 0.44))"
                    ;:d            (ut/curved-path-h x1 y1 x2 y2)
                     :d            (ut/curved-path-v x1 y1 x2 y2)}]))))
                    ;:d            (ut/stepped-path-v x1 y1 x2 y2)


(defn draw-lines [coords]
  (doall (for [[x1 y1 x2 y2 involved? color z1 z2 z1a z2a] coords]
           ^{:key (hash (str x1 y1 x2 y2 "lines-flow"))}
           (let [flow-id @(re-frame/subscribe [::selected-flow])
                 flow-select @(re-frame/subscribe [::selected-flow-block])
                 selected-i (get-in @flow-details-block-container-atom [flow-id flow-select :port-panel :in])
                 selected-o (get-in @flow-details-block-container-atom [flow-id flow-select :port-panel :out])
                 not-selected? (true? (and (nil? selected-i) (nil? selected-o)))
                 selected? (true? (or (= selected-i [z2 z2a]) (= selected-o [z1 z1a])
                                      (= selected-i [z1 z1a]) (= selected-o [z2 z2a])))
                 condi-path? (cstr/starts-with? (str z1a) ":cond-path")
                 push-path? (cstr/starts-with? (str z1a) ":push-path")
                 ;running? (is-running? z1 flow-id)
                 react! [@db/running-blocks]
                 flow-running? @(re-frame/subscribe [::is-running? :* flow-id])
                 running? (and flow-running? @(re-frame/subscribe [::is-running? z2 flow-id]))
                 waiting? (and flow-running? @(re-frame/subscribe [::is-waiting? z2 flow-id]))
                 opacity (if (or running? selected?) 1.0 0.5)]

                ;;  (when true ;(cstr/starts-with? (str z1a) ":push-path") ;; (or (= z1 :open-fn-1) (= z2 :open-fn-1))
                ;;    (tap> [:cc push-path? waiting? selected? selected-i selected-o  z1 z1a z2 z2a]))

                 ;selected-dash "" ;@(re-frame/subscribe [::selected-dash]) ;;; (get (theme-pull :theme/data-colors db/data-colors) "unknown")
                ; relations "" ;@(re-frame/subscribe [::relations selected-dash])
                ; selected  "" ;@(re-frame/subscribe [::selected])
                ; involved (vec (remove nil? (distinct (apply concat (filter #(some (fn [x] (= x selected)) %) relations)))))
                ; nothing-selected? (nil? selected)
                ;  involved1? (some #(= n1 %) involved)
                ; involved2? (some #(= n2 %) involved)
                ; involved? (and involved1? involved2?)
                ; peek? @(re-frame/subscribe [::peek?])

           ;; (tap> [:coords coords])
            ; (tap> [:lines z1 z2 same-tab? @(re-frame/subscribe [::what-tab z1]) @(re-frame/subscribe [::what-tab z2])])
             [:path {:stroke-width 7 ;(if selected? 11 6) ;(if involved? 16 13)
                     :stroke       (if (or selected? involved?) color (str color 75)) ;(if (or involved? nothing-selected?)
                    ;;  :opacity      (if not-selected? 1.0
                    ;;                    (if (or involved? selected?) 1.0 0.2)

                    ;;                    )
                     :opacity (if (and (not running?) (or push-path? condi-path?))
                                (- opacity 0.22)
                                opacity)
                    ; :transition "all 0.2s ease-in-out"
                    ;:style      {:animation "rotate linear infinite"}
                    ;:animation "rotate linear infinite"
                    ;:z-index 20
                     :class        (when running? "flow-right") ;;(some #(= z1 %) @db/running-blocks)
                     ;:class        (when (some #(= z1 %) @db/running-blocks) "flow-right") ;;(some #(= z1 %) @db/running-blocks)
                    ;(if (nil? color) "pink" color) "#E6E6FA")
                    ;:fill         "#ffffff33" ;"none"
                     ;:stroke-dasharray (if waiting? "10,15" (when condi-path? "15,10")) ;; TODO, kinda weird logic
                     :stroke-dasharray (cond waiting? "10,15"
                                             condi-path? "15,10"
                                             push-path? "5,5"
                                             :else nil)
                    ; :stroke-linecap (if (not selected?) "round" "square")
                     :stroke-linecap (cond condi-path? "butt"
                                           push-path? "butt"
                                           :else "round")
                     :fill         "none" ;;(str color 45) ;; could be a cool "running effect"
                     :filter       (if selected? (str "brightness(200%) drop-shadow(2px 1px 4px " color ")")
                                       (str "drop-shadow(2px 1px 4px #000000)"))
                    ;:d            (ut/curved-path-h x1 y1 x2 y2)
                     :d            (ut/curved-path-v x1 y1 x2 y2)}]))))

                    ;:d            (ut/stepped-path-h x1 y1 x2 y2)


(defn get-client-rect [evt]
  (let [r (.getBoundingClientRect (.-target evt))]
    {:left   (.-left r), :top (.-top r)
     :width  (.-width r) :height (.-height r)
     :bottom (.-bottom r) :right (.-right r)}))

;; (defn mouse-move-handler-block [offset bid xx yy]
;;   (fn [evt]
;;     (let [start-x (.-clientX evt)
;;           start-y (.-clientY evt)
;;           off-x (:x offset)
;;           off-y (:y offset)
;;           x      (- start-x off-x)
;;           y      (- start-y off-y)]
;;       ;(reset! detached-coords [x y])
;;       (swap! flowmaps assoc bid (->
;;                                  (get @(re-frame/subscribe [::flowmap]) bid)
;;                                  (assoc :x (+ (- x xx 78)))
;;                                  ;(assoc :x (- x 0))
;;                                  (assoc :y  (- y yy 4)))))))

(re-frame/reg-event-db
 ::update-flowmap-key
 (undoable)
 (fn [db [_ bid kkey vval]]
   ;(tap> [:update bid (get db :selected-flow-block) kkey vval])
   (let [bid (get db :selected-flow-block)] ;; override due to slow reacting?
     (if (nil? kkey) ;; fulll body update
       (assoc-in db [:flows (get db :selected-flow) :map bid] vval)
       (assoc-in db [:flows (get db :selected-flow) :map bid kkey] vval)))))

(re-frame/reg-event-db
 ::update-flowmap-key-in
 (undoable)
 (fn [db [_ bid kkey vval]]
   (tap> [:update bid (get db :selected-flow-block) kkey vval])
   (let [bid (get db :selected-flow-block)
         pin? (= kkey [:ports :in]) ;; function-to-inputs :args+ overrides
         ;;f2i (function-to-inputs lookup-map)
         arg-walks (if pin?
                     (into {}
                           (for [e (keys vval)
                                 :when (cstr/ends-with? (str e) "+")]
                             {(-> (str e)
                                  (cstr/replace "+" "")
                                  (cstr/replace ":" "")
                                  keyword) e})) {})]
     (if (nil? kkey) ;; fulll body update
       (assoc-in db [:flows (get db :selected-flow) :map bid] vval)
       (if pin?
         (-> db
             (assoc-in (vec (into [:flows (get db :selected-flow) :map bid] kkey)) vval)
             (assoc-in [:flows (get db :selected-flow) :map bid :types]
                       (walk/postwalk-replace arg-walks
                                              (get-in db [:flows (get db :selected-flow) :map bid :types])))
             (assoc-in [:flows (get db :selected-flow) :map bid :inputs]
                       (walk/postwalk-replace arg-walks
                                              (get-in db [:flows (get db :selected-flow) :map bid :inputs]))))
         (assoc-in db (vec (into [:flows (get db :selected-flow) :map bid] kkey)) vval))))))

(re-frame/reg-event-db
 ::update-flowmap-key2 ;;; test, just used for add block
 (undoable)
 (fn [db [_ bid kkey vval]]
   ;(tap> [:update bid (get db :selected-flow-block)  kkey vval])
   (let [] ; [bid (get db :selected-flow-block)] ;; override due to slow reacting?
     (if (nil? kkey) ;; fulll body update
       (assoc-in db [:flows (get db :selected-flow) :map bid] vval)
       (assoc-in db [:flows (get db :selected-flow) :map bid kkey] vval)))))

(re-frame/reg-event-db
 ::update-flowmap-key-others
 (undoable)
 (fn [db [_ bid fid kkey vval]]
   ;(tap> [:update bid (get db :selected-flow-block) fid kkey vval])
   (if (nil? kkey) ;; fulll body update
     (assoc-in db [:flows fid :map bid] vval)
     (assoc-in db [:flows fid :map bid kkey] vval))))

(re-frame/reg-event-db
 ::remove-flowmap-bid
 (fn [db [_ bid]]
   (ut/dissoc-in db [:flows (get db :selected-flow) :map bid])))

(re-frame/reg-event-db
 ::update-connections
 (fn [db [_ new-conns]]
   (assoc-in db [:flows (get db :selected-flow) :connections] new-conns)))

(defn sort-map-keys
  [ordered-keys unsorted-map]
  (let [all-keys (keys unsorted-map)
        remaining-keys (filter #(not (contains? (set ordered-keys) %))
                               all-keys)
        ordered-vec
        (into []
              (concat (map (fn [k] [k (unsorted-map k)]) ordered-keys)
                      (map (fn [k] [k (unsorted-map k)]) remaining-keys)))]
    (apply array-map (apply concat ordered-vec))))

(re-frame/reg-event-db
 ::add-port
 (fn [db [_ bid direction]]
   (let [ports (get-in db [:flows (get db :selected-flow) :map bid :ports direction] {})
         label (str (get-in db [:flows (get db :selected-flow) :map bid :data :flow-item :pill-name] "value"))
         cnt (count (keys ports))
         inputs (try (edn/read-string (get-in db [:flows (get db :selected-flow) :map bid :data :flow-item :inputs] [])) (catch :default _ []))
         new-key (ut/safe-key (keyword (str label "-" cnt)) (keys ports))
         updated-ports (assoc ports new-key :any)
        ;sorted-inputs (sort-new-keys inputs (keys updated-ports) new-key)
         sorted-ports (sort-map-keys inputs updated-ports)]
     (-> db
         (assoc-in [:flows (get db :selected-flow) :map bid :ports direction] sorted-ports)))))

(re-frame/reg-event-db
 ::delete-port
 (fn [db [_ bid pid direction]]
   (let [ports (get-in db [:flows (get db :selected-flow) :map bid :ports direction] {})
       ;cnt (count (keys ports))
        ;label (try (first (cstr/split (-> (str (first (keys ports))) (cstr/replace #":" "") (cstr/replace "*" "")) #"-"))
        ;           (catch :default _ "val"))
         inputs (try (edn/read-string (get-in db [:flows (get db :selected-flow) :map bid :data :flow-item :inputs] [])) (catch :default _ []))
        ;sorted-inputs (sort-new-keys inputs (keys ports))
         new-ports (dissoc ports pid)
         sorted-ports (sort-map-keys inputs new-ports)]
     (-> db
         (assoc-in [:flows (get db :selected-flow) :map bid :ports direction] sorted-ports)))))
        ;(assoc-in [:flows (get db :selected-flow) :map bid :data :flow-item (if (= direction :in) :inputs :outputs-nah)] (vec (keys new-ports)))


(defn zoom-to-element [element-id padding]
  (let [element (js/document.getElementById element-id)]
    (when-let [z @db/zoomer]
      (.zoomToElement z element padding))))
      ;(.setZoom z 1)


(defn reset-zoom-and-pan []
  (when-let [z @db/zoomer]
    (.resetTransform z)))

(defn center-zoom-and-pan []
  (when-let [z @db/zoomer]
    (.centerView z)))
   ;(.zoomOut z)

(defn set-zoom-pan [[positionX positionY scale]]
  (when-let [z @db/zoomer]
    (.setTransform z positionX positionY scale 0)))

(re-frame/reg-sub
 ::zoom-start
 (fn [db _]
   (get db :zoom-start)))

(re-frame/reg-sub
 ::get-flow-brick
 (fn [db _]
   (let [ordered1 (reverse (sort-by last (for [[k v] (get-in db [:flows (get db :selected-flow) :map])] [k (get v :x)])))]
         ;_ (tap> [:ordered1 ordered1])
         ;bb (keys (get-in db [:flows (get db :selected-flow) :map]))

     (str "flow-brick-" (first (first ordered1))))))

(defn center-to-saved-coords []
  (when-let [z @db/zoomer]
    (let [[positionX positionY scale] @(re-frame/subscribe [::zoom-start])]
          ;bb @(re-frame/subscribe [::get-flow-brick])

      (tap> [positionX positionY scale])
      ;(.setTransform z positionX positionY scale 0)
      ;(.setTransform z 100 100 1)
      ;(.zoomToElement z bb 1200)
      (js/setTimeout (fn []
                       ;(.centerView z)
                       (.zoomToElement z @(re-frame/subscribe [::get-flow-brick]) 0.8))
                       ;(let [[positionX positionY scale] @(re-frame/subscribe [::zoom-start])] (.setTransform z positionX positionY scale 0 ))
                       ;(.zoomOut z 5)
                     1000))))
      ;(.resetTransform z)
      ;(.centerView z)


(re-frame/reg-event-db
 ::select-block
 (undoable)
 (fn [db [_ bid]]
   ;(tap> [:selecting-block bid])
  ;(zoom-to-element (str "flow-brick-" bid) 100)
  ;(reset-zoom-and-pan)
 ;;  (center-zoom-and-pan)
 ;;  (when-let [z @db/zoomer]
 ;;    (.setTransform z 100 100 2 0.5))
   (assoc db :selected-flow-block bid)))

(re-frame/reg-event-db
 ::select-flow
 (fn [db [_ bid]]
   (assoc db :selected-flow bid)))

(re-frame/reg-event-db
 ::update-flowmap-connections
 (undoable)
 (fn [db [_ vval]]
   (assoc-in db [:flows (get db :selected-flow) :connections] vval)))

(re-frame/reg-event-db
 ::update-flowmap-coords
 (fn [db [_ bid x y]]
   (-> db
       (assoc-in [:flows (get db :selected-flow) :map bid :x] x)
       (assoc-in [:flows (get db :selected-flow) :map bid :y] y))))

;; (defn mouse-move-handler-block [offset bid]
;;   (fn [evt]
;;     (let [raw-x (.-clientX evt)
;;           raw-y (.-clientY evt)
;;           [xx yy] @detached-coords
;;           zoom-multi (get @db/pan-zoom-offsets 2)
;;           bx (* 3 zoom-multi) ;; border offset of container
;;           by (* 18.25 zoom-multi) ;; border offset of container
;;           raw-x (- raw-x bx)
;;           raw-y (- raw-y by)
;;           zoom-offset-x (get @db/pan-zoom-offsets 0)
;;           zoom-offset-y (get @db/pan-zoom-offsets 1)
;;           off-x (/ (+ (:x offset) xx) zoom-multi)
;;           off-y (/ (+ (:y offset) yy) zoom-multi)
;;           x      (- (- (/ raw-x zoom-multi) (/ zoom-offset-x zoom-multi)) off-x)
;;           y      (- (- (/ raw-y zoom-multi) (/ zoom-offset-y zoom-multi)) off-y)]
;;       (re-frame/dispatch-sync [::update-flowmap-coords bid x y]))))

;; (defn mouse-move-handler-block [offset bid]
;;   (let [last-coords (atom nil)]
;;     (fn [evt]
;;       (let [raw-x (.-clientX evt)
;;             raw-y (.-clientY evt)
;;             [xx yy] @detached-coords
;;             zoom-multi (get @db/pan-zoom-offsets 2)
;;             bx (* 3 zoom-multi) ;; border offset of container
;;             by (* 18.25 zoom-multi) ;; border offset of container
;;             raw-x (- raw-x bx)
;;             raw-y (- raw-y by)
;;             zoom-offset-x (get @db/pan-zoom-offsets 0)
;;             zoom-offset-y (get @db/pan-zoom-offsets 1)
;;             off-x (/ (+ (:x offset) xx) zoom-multi)
;;             off-y (/ (+ (:y offset) yy) zoom-multi)
;;             x      (- (- (/ raw-x zoom-multi) (/ zoom-offset-x zoom-multi)) off-x)
;;             y      (- (- (/ raw-y zoom-multi) (/ zoom-offset-y zoom-multi)) off-y)
;;             grid-size snap-to-grid ;20
;;             snapped-x (* grid-size (Math/round (/ x grid-size)))
;;             snapped-y (* grid-size (Math/round (/ y grid-size)))]
;;         ;(tap> [:snapper bid snapped-x snapped-y])
;;         (when (or (not= snapped-x (first @last-coords))
;;                   (not= snapped-y (second @last-coords)))
;;           (reset! last-coords [snapped-x snapped-y])
;;           (re-frame/dispatch-sync [::update-flowmap-coords bid snapped-x snapped-y]))))))

(defn mouse-move-handler-block [offset bid]
  (let [last-coords (atom nil)]
    (fn [evt]
      (let [raw-x (.-clientX evt)
            raw-y (.-clientY evt)
            [xx yy] @detached-coords
            zoom-multi (get @db/pan-zoom-offsets 2)
            bx (* 3 zoom-multi) ;; border offset of container
            by (* 18.25 zoom-multi) ;; border offset of container
            raw-x (- raw-x bx)
            raw-y (- raw-y by)
            zoom-offset-x (get @db/pan-zoom-offsets 0)
            zoom-offset-y (get @db/pan-zoom-offsets 1)
            off-x (/ (+ (:x offset) xx) zoom-multi)
            off-y (/ (+ (:y offset) yy) zoom-multi)
            x      (- (- (/ raw-x zoom-multi) (/ zoom-offset-x zoom-multi)) off-x)
            y      (- (- (/ raw-y zoom-multi) (/ zoom-offset-y zoom-multi)) off-y)
            grid-size snap-to-grid ;20
            snapped-x (* grid-size (Math/round (/ x grid-size)))
            snapped-y (* grid-size (Math/round (/ y grid-size)))
            bounded-x (min (max snapped-x 0) (- canvas-width grid-size))
            bounded-y (min (max snapped-y 0) (- canvas-height grid-size))]
        (when (or (not= bounded-x (first @last-coords))
                  (not= bounded-y (second @last-coords)))
          (reset! last-coords [bounded-x bounded-y])
          (re-frame/dispatch-sync [::update-flowmap-coords bid bounded-x bounded-y]))))))

;:width "3200px" ;"6400px" ;(px ww)
;:height "2400px" ;"3600px" ;(px hh)


(defn tag-tentacle-pos [evt]
  (let [raw-x (.-clientX evt)
        raw-y (.-clientY evt)
        [xx yy] @detached-coords
        offset {:x 0 :y 0}
        zoom-multi (get @db/pan-zoom-offsets 2)
        bx 0;(* 3 zoom-multi) ;; border offset of container
        by 0;(* 18.25 zoom-multi) ;; border offset of container
        raw-x (- raw-x bx)
        raw-y (- raw-y by)
        zoom-offset-x (get @db/pan-zoom-offsets 0)
        zoom-offset-y (get @db/pan-zoom-offsets 1)
        off-x (/ (+ (:x offset) xx) zoom-multi)
        off-y (/ (+ (:y offset) yy) zoom-multi)
        x      (- (- (/ raw-x zoom-multi) (/ zoom-offset-x zoom-multi)) off-x)
        y      (- (- (/ raw-y zoom-multi) (/ zoom-offset-y zoom-multi)) off-y)]
    (reset! tentacle-pos [x y])))

(defn tag-screen-position [evt]

 ;; (let [zm (get @db/pan-zoom-offsets 2)
 ;;       x (.-clientX evt)
 ;;       y (.-clientY evt)
 ;;       [xx yy] @detached-coords
 ;;       x (- x (* 25 zm))
 ;;       ]
 ;;   (reset! tentacle-pos [x y]))
  (let [;[xx yy] @detached-coords
       ;zoom-offset-x (get @db/pan-zoom-offsets 0)
       ;zoom-offset-y (get @db/pan-zoom-offsets 1)
       ;zoom-multi (get @db/pan-zoom-offsets 2)
        raw-x (.-clientX evt)
        raw-y (.-clientY evt)]
       ;raw-y (- raw-y (* yy zoom-multi))
       ;_ (tap> [:offs @db/pan-zoom-offsets raw-x raw-y])
       ;; x      (- (- (/ raw-x zoom-multi) (/ zoom-offset-x zoom-multi)) xx)
       ;; y      (- (- (/ raw-y zoom-multi) (/ zoom-offset-y zoom-multi)) yy)

    (reset! tentacle-pos [raw-x raw-y])))

(def mouse-dragging-panel? (reagent/atom false))

(re-frame/reg-event-db
 ::resize-block
 (fn [db [_ bid w h]]
   (let [ff (get db :selected-flow)]
     (-> db
         (assoc-in [:flows ff :map bid :h] h)
         (assoc-in [:flows ff :map bid :w] w)))))

(defn mouse-up-handler-block [on-move]
  (fn me [evt]
    (do ;(reset! mouse-dragging-panel? false)
      (reset! dragging? false)
      (reset! mouse-dragging-panel? false)
      (reset! dragging-flow? false))
       ;(reset! dragging-body {})
       ;(reset! dyn-dropper-hover nil)
    (gevents/unlisten js/window EventType.MOUSEMOVE on-move)))

(defn debounce [f interval]
  (let [timer (atom nil)]
    (fn [& args]
      (when-let [prev-timer @timer]
        (js/clearTimeout prev-timer))
      (reset! timer (js/setTimeout (fn [] (apply f args)) interval)))))

(defn mouse-down-handler-block [e bid]
  (let [{:keys [left top]} (get-client-rect e)
        offset  {:x (- (.-clientX e) left)
                 :y (- (.-clientY e) top -25)}
        _ (reset! dragging-flow? true)
        on-move (mouse-move-handler-block offset bid)]
    (do (reset! dragging? true)
        (gevents/listen js/window EventType.MOUSEMOVE on-move))
    (gevents/listen js/window EventType.MOUSEUP
                    (mouse-up-handler-block on-move))))

(re-frame/reg-sub
 ::size
 (fn [db [_ bid]]
   (let [selected-flow (get db :selected-flow)
         fmap (get-in db [:flows selected-flow :map])]
     [(get-in fmap [bid :w])
      (get-in fmap [bid :h])])))

;; [raw-x (.-clientX evt)
;;  raw-y (.-clientY evt)
;;  [xx yy] @detached-coords
;;  zoom-multi (get @db/pan-zoom-offsets 2)
;;  bx (* 3 zoom-multi) ;; border offset of container
;;  by (* 18.25 zoom-multi) ;; border offset of container
;;  raw-x (- raw-x bx)
;;  raw-y (- raw-y by)
;;  zoom-offset-x (get @db/pan-zoom-offsets 0)
;;  zoom-offset-y (get @db/pan-zoom-offsets 1)
;;  off-x (/ (+ (:x offset) xx) zoom-multi)
;;  off-y (/ (+ (:y offset) yy) zoom-multi)
;;  x      (- (- (/ raw-x zoom-multi) (/ zoom-offset-x zoom-multi)) off-x)
;;  y      (- (- (/ raw-y zoom-multi) (/ zoom-offset-y zoom-multi)) off-y)]

;; (defn resize-mouse-move-handler [offset sw sh sx sy bid]
;;   (fn [evt]
;;     (let [[min-w min-h]   [100 60] ;@(re-frame/subscribe [::min-size block-id])
;;           is-mask?        false ;(= block-type "mask")
;;           is-square?      false ;(= block-type "image")
;;           zoom-multi      (get @db/pan-zoom-offsets 2)
;;           zoom-offset-x   (get @db/pan-zoom-offsets 0)
;;           zoom-offset-y   (get @db/pan-zoom-offsets 1)
;;           raw-x (.-clientX evt)
;;           raw-y (.-clientY evt)
;;           bx 0 ;(* 3 zoom-multi) ;; border offset of container
;;           by 0 ;(* 18.25 zoom-multi) ;; border offset of container
;;           raw-x (- raw-x bx)
;;           raw-y (- raw-y by)
;;           pos-x           (- (/ raw-x zoom-multi) (/ zoom-offset-x zoom-multi))
;;           pos-y           (- (/ raw-y zoom-multi) (/ zoom-offset-y zoom-multi))
;;           sx              (- (/ sx zoom-multi) (/ zoom-offset-x zoom-multi))
;;           sy              (- (/ sy zoom-multi) (/ zoom-offset-y zoom-multi))
;;           w               (+ sw (- pos-x sx))
;;           h               (+ sh (- pos-y sy))
;;           wmax            (if (< w min-w) min-w w)
;;           hmax            (cond is-mask?  (+ (if (< w min-w) min-w w) 0)
;;                                 is-square? (+ (if (< w min-w) min-w w) 0)
;;                                 :else (if (< h min-h) min-h h))]
;;       (when (or (not (= hmax sh)) (not (= wmax sw)))
;;         (re-frame/dispatch [::resize-block bid wmax hmax])))))


(defn resize-mouse-move-handler [offset sw sh sx sy bid]
  (let [last-size (atom [sw sh])]
    (fn [evt]
      (let [[min-w min-h]   [100 60] ;@(re-frame/subscribe [::min-size block-id])
            is-mask?        false ;(= block-type "mask")
            is-square?      false ;(= block-type "image")
            zoom-multi      (get @db/pan-zoom-offsets 2)
            zoom-offset-x   (get @db/pan-zoom-offsets 0)
            zoom-offset-y   (get @db/pan-zoom-offsets 1)
            raw-x (.-clientX evt)
            raw-y (.-clientY evt)
            bx 0 ;(* 3 zoom-multi) ;; border offset of container
            by 0 ;(* 18.25 zoom-multi) ;; border offset of container
            raw-x (- raw-x bx)
            raw-y (- raw-y by)
            pos-x           (- (/ raw-x zoom-multi) (/ zoom-offset-x zoom-multi))
            pos-y           (- (/ raw-y zoom-multi) (/ zoom-offset-y zoom-multi))
            sx              (- (/ sx zoom-multi) (/ zoom-offset-x zoom-multi))
            sy              (- (/ sy zoom-multi) (/ zoom-offset-y zoom-multi))
            w               (+ sw (- pos-x sx))
            h               (+ sh (- pos-y sy))
            grid-size       snap-to-grid ;20
            snapped-w       (* grid-size (Math/round (/ w grid-size)))
            snapped-h       (* grid-size (Math/round (/ h grid-size)))
            wmax            (if (< snapped-w min-w) min-w snapped-w)
            hmax            (cond is-mask?  (+ (if (< snapped-w min-w) min-w snapped-w) 0)
                                  is-square? (+ (if (< snapped-w min-w) min-w snapped-w) 0)
                                  :else (if (< snapped-h min-h) min-h snapped-h))]
        (when (or (not= hmax (second @last-size)) (not= wmax (first @last-size)))
          (reset! last-size [wmax hmax])
          (re-frame/dispatch [::resize-block bid wmax hmax]))))))

(defn resize-mouse-down-handler [e bid]
  (let [[w h] @(re-frame/subscribe [::size bid])
        offset             {:x (- (.-clientX e) w)
                            :y (- (.-clientY e) h)}
        on-move            (resize-mouse-move-handler offset w h (.-clientX e) (.-clientY e) bid)]
    (do (reset! mouse-dragging-panel? true)
        (reset! flow-hover bid)
        (gevents/listen js/window EventType.MOUSEMOVE on-move))
    (gevents/listen js/window EventType.MOUSEUP
                    (mouse-up-handler on-move))))

(defn draggable-port [element flow-id bid pid ttype xs ys]
  [(reagent/adapt-react-class rdnd/Draggable)
   (let [browser-pull? (true? (map? pid))
         keypath (if browser-pull? (get pid :keypath-in) nil)
         input-pid (if browser-pull? (get pid :input-pid) nil)
         port-meta (if browser-pull? (get pid :port-meta) nil)
          ;;_ (when browser-pull? (tap> [:draggable-port keypath browser-pull? bid pid])) ;; pid is a map in browser-pull
         pid (if browser-pull? :* pid)
        ;;  [_ _ sx sy] (if browser-pull? (first (filter
        ;;                                        #(cstr/includes? (str %) (str " " bid " "))
        ;;                                               (generate-coords nil nil))) [-1 -1])


         data (let [subkey (str flow-id ">" (cstr/replace (str bid) #":" ""))
                    param-full (keyword (str "flow/" subkey)) ;:flow/map-pull-test2>open-fn-6
                    param-field (keyword subkey)]
                {:h 3
                 :w 7
                 :root [0 0]
                 :browser-pull? browser-pull?
                 :drag-meta
                 {:type :param
                  :param-full param-full
                  :port-meta port-meta
                  :keypath keypath
                  :src-bid bid :src-pid (if input-pid input-pid pid)
                  :param-table :flow
                  :param-field param-field}})
         [xx yy] @detached-coords]

     {:type          :flow-port
   ;:on-drag (debounce #(tag-screen-position %) 100)
     ;:on-drag tag-tentacle-pos
      :on-drag (when (not browser-pull?) tag-screen-position)
     ;:on-drag (debounce tag-screen-position 100)
     ;:on-drag #(tap> [(.-clientX %) (.-clientY %)])
    ; :on-drag       #(reset! tentacle-pos [(+ (.-clientX %) xx) (+ (.-clientY %) yy)])
      :on-drag-end   #(do (reset! dragging-port? false))
                       ;(reset! dyn-dropper-hover false)
                       ;(reset! dragging-size [0 0])
                       ;(reset! on-block? false)
                       ;(tap> [(.-clientX %) (.-clientY %)])
                       ; (tag-screen-position %)
                       ;(reset! dragging-body false)

      :on-drag-start #(do
                        (reset! dragging-port? true)
                        (reset! dragged-port [bid pid ttype])
                        ;; (reset! tentacle-start [(if browser-pull? sx (- (.-clientX %) xx 8))
                        ;;                         (if browser-pull? sy (- (.-clientY %) yy 40))
                        ;;                         ])
                        (reset! tentacle-start [(- (.-clientX %) xx 8)
                                                (- (.-clientY %) yy 40)])
                        (reset! tentacle-pos [0 0])
                       ;(reset! tentacle-start [xs ys])
                       ;(tap> [:tstart xs ys])
                       ;(reset! tentacle-pos [xs ys])
                       ;(reset! on-block? false)
                        (reset! bricks/dragging-size [(get data :w) (get data :h)])
                        (reset! bricks/dragging-body data))
                       ;(reset! dragging-body data)
      :data          (pr-str data)}) ;[bid pid]

   [re-com/box
    :size "auto"
    :child element
    :style {:cursor (if @dragging-port? "grabbing" "grab")}]])

(defn draggable-play [element flow-id]
  [(reagent/adapt-react-class rdnd/Draggable)
   (let [data (let [];subkey (str flow-id ">" (cstr/replace (str bid) #":" ""))
                   ;param-full (keyword (str "flow/" subkey)) ;:flow/map-pull-test2>open-fn-6
                   ;param-field (keyword subkey)

                {:h 2
                 :w 7
                 :drag-meta
                 {:source-table :hi,
                  :table-fields [:*],
                  :connection-id nil,
                  :source-panel-key :block-7034,
                  :type :view},
                 :views
                 {:flow-play
                  [:box
                   :align
                   :center
                   :justify
                   :center
                 ;;  :attr
                 ;;  {;:id ":block-7034.:hi"
                 ;;   :on-click }
                   :style
                   {:font-size "25px",
                    :font-weight 700,
                    :padding-top "6px",
                    :padding-left "14px",
                    :margin-top "-8px",
                    :color :theme/editor-outer-rim-color,
                    :font-family :theme/base-font}
                   :child [:run-flow [(str flow-id) (str "run " flow-id)]]]},
                 :name "flow-play"})]
     {:type          :flow-port
   ;:on-drag (debounce #(tag-screen-position %) 100)
   ;:on-drag tag-screen-position
   ;:on-drag (debounce tag-screen-position 10)
     ;:on-drag #(tap> [(.-clientX %) (.-clientY %)])
      :on-drag-end   #(do (reset! dragging-port? false))
      :on-drag-start #(do
                        (reset! dragging-port? true)
                        (reset! bricks/dragging-size [(get data :w) (get data :h)])
                        (reset! bricks/dragging-body data))
                       ;(reset! dragging-body data)

      :data          (pr-str data)}) ;[bid pid]

   [re-com/box
    :size "none"
    :child element
    :style {:cursor (if @dragging-port? "grabbing" "grab")}]])
           ;:padding-bottom "14px"


;; (defn droppable-port [element bid pid]
;;   ;(tap> [:dropped? types-vec root element])
;;   (if true ; @dragging-port? ;; no point since it doesnt render anyways
;;     [(reagent/adapt-react-class rdnd/Droppable)
;;      {:types   [:flow-port]
;;       ;:on-drag #(tag-screen-position %)
;;       :on-drop #(let [[srcf srcl _] @dragged-port
;;                       src (if (= srcl :out) srcf ;; no alias for simple */out
;;                               (keyword (str (gn srcf) "/" (gn srcl))))
;;                       dest (if (= pid :in) bid
;;                                (keyword (str (gn bid) "/" (gn pid))))]
;;                   (tap> [:dropped src :to dest])
;;                   (reset! flowmaps-connections (conj @flowmaps-connections [src dest])))}
;;      [re-com/box
;;       :size "auto"
;;       :child element]] element))

(defn function-to-inputs [lookup-map]
  (let [ordered (atom [])
        _ (tap> [:lookup-map lookup-map])
        port-map (if (or (vector? lookup-map) (map? lookup-map) (list? lookup-map))
                   (try
                    ;;  (into {} (for [e (take-while #(not= "&" (str %)) (second (get lookup-map :fn))) ;; quit before optional params ??
                    ;;                 :let [kk (keyword (str e))
                    ;;                       _ (swap! ordered conj kk)]]
                    ;;             {kk (get-in lookup-map [:types kk] :any)}))
                     (let [input (cond (map? lookup-map)  (second (get lookup-map :fn))
                                       (list? lookup-map)  (second lookup-map)
                                       :else lookup-map)
                           after-ampersand? (atom false)]
                       (into {} (for [e input
                                      :let [kko (keyword (str e))
                                            kk (if @after-ampersand?
                                                 (keyword (str e "+"))
                                                 (keyword (str e)))
                                            _ (do (when (= "&" (str e)) (reset! after-ampersand? true))
                                                  (swap! ordered conj kk))]]
                                  (when-not (= "&" (str e))
                                    {kk (get-in lookup-map [:types kko] :any)}))))
                     (catch :default _ {:value :any}))
                   (try ;; passed a raw list
                     (into {} (for [e (take-while #(not= "&" (str %)) (second lookup-map)) ;; quit before optional params ??
                                    :let [kk (keyword (str e))
                                          _ (swap! ordered conj kk)]]
                                {kk :any})) ;; we dont know what types yet
                     (catch :default _ {:value :any})))]
    ;(tap> [:ordered-arities @ordered (map? lookup-map)])
    port-map))

(defn ifnil [x n] (if (nil? x) n x))

(defn remove-connections [bid pid input?]
  (let [flowmaps-connections @(re-frame/subscribe [::flowmap-connections])
        side-fn (if input? last first)
        conns-count (count (filter #(or (= (side-fn %) (keyword (str (gn bid) "/" (gn pid))))
                                        (if (or (= pid :in) (= pid :out))
                                          (= (side-fn %) bid)
                                          false)) ;; unnamed ports assumed
                                   flowmaps-connections))
        new-connections (remove #(or (= (side-fn %) (keyword (str (gn bid) "/" (gn pid))))
                                     (if (or (= pid :in) (= pid :out))
                                       (= (side-fn %) bid)
                                       false)) ;; unnamed ports assumed
                                flowmaps-connections)]
    (tap> [:removing bid pid (if input? :in :out) conns-count])
    (re-frame/dispatch [::update-flowmap-connections new-connections])
    conns-count))

(defn get-all-values [m k]
  (if (map? m)
    (into [] (concat
              (when (contains? m k) [(get m k)])
              (mapcat #(get-all-values % k) (vals m))))
    []))

;; Usage:
;; (get-all-values your-map :type)

(re-frame/reg-sub
 ::reserved-type-keywords ;; @(re-frame/subscribe [::reserved-type-keywords])
 (fn [db]
   (let [lib-keys (try (vec (map edn/read-string (map :name (get-in db [:data :flow-fn-all-sys])))) (catch :default _ []))]
     (vec (into lib-keys
                (into
                 (keys (get-in db [:flows (get db :selected-flow) :map]))
                 (vec (distinct (get-all-values (get-in db [:flows (get db :selected-flow)]) :type)))))))))

(defn add-block [x y & [body bid no-select?]]
  (let [;bid (if bid bid (keyword (str "open-input-" (count @(re-frame/subscribe [::flowmap])))))
        _ (tap> [:bid bid])
        bid (keyword (cstr/replace (str (gn bid)) #"/" "-"))
        bid (if bid bid :open-input)
        _ (tap> [:ADD-pre-safe-bid bid])
        safe-keys-reserved @(re-frame/subscribe [::reserved-type-keywords])
        bid (ut/safe-key bid safe-keys-reserved)
        _ (tap> [:ADD-post-safe-bid bid])
        zoom-multi (get @db/pan-zoom-offsets 2)
        zoom-offset-x (get @db/pan-zoom-offsets 0)
        zoom-offset-y (get @db/pan-zoom-offsets 1)
        drop-x      (- (/ x zoom-multi) (/ zoom-offset-x zoom-multi))
        drop-y      (- (/ y zoom-multi) (/ zoom-offset-y zoom-multi))
        grid-size snap-to-grid
        drop-x       (* grid-size (Math/round (/ drop-x grid-size)))
        drop-y       (* grid-size (Math/round (/ drop-y grid-size)))
        body (if body (if (or (cstr/includes? (str bid) "open-input")
                              (cstr/includes? (str bid) "open-fn")
                              (get body :right-click? false))
                        (merge body {:x drop-x :y drop-y :z 0}) body) ;; override for right click open block
                 {:w 200 :h 50
                  :x drop-x :y drop-y :z 0
                  :ports {:in {:in :string}
                          :out {:out :string}}})]
   ;(tap> [:adding-flow-block bid x y @(re-frame/subscribe [::flowmap])])
   ;(swap! flowmaps assoc bid body)
    (when (not no-select?) (re-frame/dispatch [::select-block bid]))
    (re-frame/dispatch [::update-flowmap-key2 bid nil body])))

(defn remove-block [bid]
  (let [flowmaps-connections @(re-frame/subscribe [::flowmap-connections])
        new-connections (remove #(or (= (first %) bid) (= (last %) bid)
                                     (cstr/starts-with? (str (first %)) (str bid "/"))
                                     (cstr/starts-with? (str (last %)) (str bid "/")))
                               ;; base refs and name ports
                                flowmaps-connections)]
    (tap> [:removing-block bid])
;;     (reset! flowmaps-connections
;;             (remove #(or (= (first %) bid) (= (last %) bid)
;;                          (cstr/starts-with? (str (first %)) (str bid "/"))
;;                          (cstr/starts-with? (str (last %)) (str bid "/")))
;;                   ;; base refs and name ports
;;                     @flowmaps-connections))
;;     (swap! flowmaps dissoc bid)
    (reset! flow-hover nil)
    (reset! port-hover2 nil)
    (re-frame/dispatch [::select-block nil])
    (re-frame/dispatch [::remove-flowmap-bid bid])
    (re-frame/dispatch [::update-flowmap-connections new-connections])))

(re-frame/reg-sub
 ::selected-flow-block
 (fn [db]
   (get db :selected-flow-block)))

(re-frame/reg-event-db
 ::flow-block-meta
 (fn [db [_ bid key value]]
   (assoc-in db [:flows (get db :selected-flow) :map bid :data key] value)))

(defn select-block [bid]
;   (dorun (smooth-scroll-to-element "scrolly-boy" bid)
 ;(reset! flow-select (if (= bid @flow-select) nil bid))
 ;(reset! flow-select bid)
  (tap> [:selecting-block2 bid])
  (re-frame/dispatch [::select-block bid]))

(defn connect-ports [src dest]
  (tap> [:connecting src dest])
  (let [flowmaps-connections @(re-frame/subscribe [::flowmap-connections])
        conns (vec (filter #(not (= (last %) :done)) flowmaps-connections))
        src (if (cstr/ends-with? (str src) "/*") ;; no need for *, will only confuse things. w/o it is implied. (since the multi-output ports are "fake" anyways. lol)
              (keyword (first (-> (str src) (cstr/replace #":" "") (cstr/split #"/")))) src)
        ;base-src (if (cstr/includes? (str src) "/") (keyword (gns src)) src)
        ;; src (if (cstr/ends-with? (str src) "/*") (keyword (-> (str src) (cstr/replace ":" "") (cstr/replace "/*" ""))) src)
        done? (= dest :done)
        conn-vec (vec (distinct
                       (conj
                        (if done? conns flowmaps-connections)
                        [src ;(if done? base-src src)
                         dest])))]
    (tap> [:new-conn-vec conn-vec])
;;       (reset! flowmaps-connections
;;               (conj conns [src dest])))
;;     (reset! flowmaps-connections
;;             (conj @flowmaps-connections [src dest])
;;             (assoc-in db [:flows (get db :selected-flow) :connections] (conj flowmaps-connections)))
    (re-frame/dispatch [::update-flowmap-connections conn-vec])
    ;(re-frame/dispatch [::clean-connections])
    ))

(defn render-icon [icon bcolor & [size fsize]]
  (let [size (or size 20)
        fsize (or fsize 14)]
    (if (not (nil? icon))
      (if (not (cstr/starts-with? (str icon) "http")) ;(cstr/includes? icon "zmdi")
        [re-com/md-icon-button :src (at)
         :md-icon-name icon
         :style {:color bcolor
                 ;:cursor "grab"
                 :font-size (px fsize)}
         :attr {}]
        [re-com/box
         :style {:margin-left "-10px"}
         :size "none"
         :width (px size)
         ;:height (px size)
         :child [:img {:src icon
            ;:height "50px"
            ;:height "auto"
            ;:width "50px"
                       :width "100%"}]])
      " ")))

(def ports-react-render (reagent/atom nil))

(def auto-font-atom (atom {}))

(defn auto-font-size-px-fn [value h w]
  (let [brick-size 2.22
        md            (ut/data-typer value) ;(get-in metadata [:fields label :data-type])
        num?          (or (= md "integer") (= md "float"))
        formatted-val (if num? (str (ut/nf value)) (str value))
        len           (count (str formatted-val))
        charw         (js/Math.ceil (/ (* (js/Math.ceil (/ (+ h w 1) 2)) brick-size) len))]
    charw))

(defn auto-font-size-px [value h w]
  (let [hx [value h w]
        cache (get @auto-font-atom hx)]
    (if cache cache (let [deep (auto-font-size-px-fn value h w)]
                      (swap! auto-font-atom assoc hx deep)
                      deep))))

(defn droppable-port [element bid pid]
  [(reagent/adapt-react-class rdnd/Droppable)
   {:types   [:flow-port]
    :on-drop #(let [_ (tap> [:dropped-on-port bid pid @dragged-port])
                    [srcf srcl _] @dragged-port
                    src (if (= srcl :out) srcf ;; no alias for simple */out
                            (keyword (str (gn srcf) "/" (gn srcl))))
                    dest (if (= pid :in) bid
                             (keyword (str (gn bid) "/" (gn pid))))]
                (reset! dragging-port? false)
                (tap> [:dropped src :to dest])
                (connect-ports src dest))}
   element])

(defn valid-drop-ports [inputs] ;; @dragging-port?=
  (let [filtered-inputs (filter #(or (try (some (fn [x] (= x (last @dragged-port))) (val %)) (catch :default _ false))
                                     (= (val %)
                                        (try (last @dragged-port) ;; in case of nil, etc
                                             (catch :default _ nil)))
                                     (= (val %) :any))
                                inputs)] (keys filtered-inputs)))

(defn code-box [width-int height-int value & [syntax]]
  (let [syntax (or (if (= syntax "raw (clojure)") "clojure" syntax) "clojure")]
    [re-com/box
     :size "none"
     :width (px (- width-int 24))
     :height (px (- height-int 24))
     :style {:font-family   (theme-pull :theme/monospaced-font nil) ; "Chivo Mono" ;"Fira Code"
             :font-size     "14px"
             :overflow      "auto"
             :border-radius "12px"
             ;:border "1px solid yellow"
             :font-weight   700}
     :child [(reagent/adapt-react-class cm/UnControlled)
             {:value   (ut/format-map (- width-int 24)
                                      (str value))
            ; :onBlur  #(re-frame/dispatch-sync [::update-selected-field kp (read-string (cstr/join " " (ut/cm-deep-values %)))])
              :options {:mode              syntax ; "clojure"
                        :lineWrapping      true
                        :lineNumbers       false ;true
                        :matchBrackets     true
                        :autoCloseBrackets true
                        :autofocus         false
                        :autoScroll        false
                        :detach            true
                        :readOnly          true            ;true
                        :theme             (theme-pull :theme/codemirror-theme nil)}}]]))

(re-frame/reg-event-db
 ::set-flow-opts
 (fn [db [_ opts-map]]
   (assoc-in db [:flows (get db :selected-flow) :opts] opts-map)))

(defn code-box-options [flow-id width-int height-int value]
  (let [scrubber? (get-in @flow-details-block-container-atom [flow-id :opts-scrubber?] false)
        react! [@flow-details-block-container-atom]] ;; just in case
    [re-com/v-box
     :size "none"
     :width (px (- width-int 24))
     :height (px (- height-int 24))
     :style {:font-family   (theme-pull :theme/monospaced-font nil) ; "Chivo Mono" ;"Fira Code"
             :font-size     "14px"
             :overflow      "auto"
             :border-radius "12px"
             :font-weight   700}
     :children [(if scrubber?
                  [bricks/scrubber-panel
                   true ; view?
                   (let [oo @(re-frame/subscribe [::bricks/keypaths-in-flow-opts])
                         oo (if (empty? oo) ;; grab defaults if somehow they havent been written in yet
                              @(re-frame/subscribe [::opts-map])
                              oo)] oo)
                   [:opts flow-id] ;nil ;key-type
                   nil ;(get @param-search key-type)
                   {:fm true :opts? true}]
                  [(reagent/adapt-react-class cm/UnControlled)
                   {:value   (ut/format-map (- width-int 24)
                                            (str value))
                    :onBlur  #(let [opts (try (read-string (cstr/join " " (ut/cm-deep-values %)))
                                              (catch :default _ :err))]
                                (when (map? opts)
                                  (re-frame/dispatch [::set-flow-opts opts])))
                    :options {:mode              "clojure"
                              :lineWrapping      true
                              :lineNumbers       true
                              :matchBrackets     true
                              :autoCloseBrackets true
                              :autofocus         false
                              :autoScroll        false
                              :detach            true
                              :readOnly          false
                              :theme             (theme-pull :theme/codemirror-theme nil)}}])
                [re-com/box
                 :width "100%"
                 :align :end
                 :attr {:on-click #(swap! flow-details-block-container-atom assoc-in [flow-id :opts-scrubber?] (not scrubber?))}
                 :style {:font-size "11px"
                         :padding-left "10px"
                         :cursor "pointer"
                                       ;:padding-right "10px"
                         :color (str (theme-pull :theme/editor-outer-rim-color nil) 98)}
                 :child (str "scrubber " (if scrubber? "on" "off"))]]]))

(defn code-box-smol [width-int height-int value & [syntax]]
  (let [syntax (or (if (= syntax "raw (clojure)") "clojure" syntax) "clojure")
        stringify? (not (= syntax "clojure"))]
    ;(tap> [:smol stringify? syntax value])
    [re-com/box
     :size "none"
     :width (px (- width-int 24))
     ;:height (px (- height-int 25))
     :style {:font-family   (theme-pull :theme/monospaced-font nil) ; "Chivo Mono" ;"Fira Code"
             :font-size     "6px"
             :overflow      "hidden" ;"auto"
             ;:border-radius "12px"
             :font-weight   700}
     :child [(reagent/adapt-react-class cm/UnControlled)
             {:value   (if stringify? (str (edn/read-string value)) ; pr-str double quoted it, so we have to read it back to single quote it... weird TODO
                           (ut/format-map width-int (str value)))
            ; :onBlur  #(re-frame/dispatch-sync [::update-selected-field kp (read-string (cstr/join " " (ut/cm-deep-values %)))])
              :options {:mode              syntax ; "clojure"
                        :lineWrapping      true ;false
                        :lineNumbers       false
                        :matchBrackets     true
                        :autoCloseBrackets true
                        :autofocus         false
                        :autoScroll        false
                        :detach            true
                        :readOnly          true            ;true
                        :theme             (theme-pull :theme/codemirror-theme nil)}}]]))

(defn swap-delay! [atom f value delay-ms]
  (swap! atom f value)
  (async/go
    (async/<! (async/timeout delay-ms))
    (swap! atom #(remove (partial = value) %))))

(defn reset-delay! [atom value delay-ms]
  (reset! atom value)
  (async/go
    (async/<! (async/timeout delay-ms))
    (reset! atom nil)))

(defn set-delay! [atom value delay-ms]
  (async/go
    (async/<! (async/timeout delay-ms))
    (reset! atom value)))

(re-frame/reg-sub
 ::port-color ;; [::port-color flow-id bid vk nil :data-type])
 (fn [_ [_ flow-id bid & [pid return-type]]]
   (let [flow-map @(re-frame/subscribe [::flowmap])
         override-value (get-in @(re-frame/subscribe [::http/flow-results]) [:return-maps (str flow-id) bid])
         fmap (get flow-map bid)
         ports (get fmap :ports)
         data (get fmap :data)
         value (if override-value override-value (get data :user-input))
         port-multi? (true? (or (map? value) (vector? value)))]
     ;(when (= bid :open-fn-1) (tap> [:pc1 bid pid return-type override-value value]))
     (if (nil? pid)
       (let [flow-select @(re-frame/subscribe [::selected-flow-block])
             selected? (= bid flow-select)
             python? (= (get data :syntax) "python")
             ttype (get-in fmap [:data :flow-item :type]
                           (get-in fmap [:data :drag-meta :type]))
             outputs (get ports :out)
             outputs (if (and override-value (= (vec (keys outputs)) [:out])) ;; if we have return data,
                       {:out (keyword (ut/data-typer override-value))} ;; change output data types to be more accurate
                       outputs)
             error? (get value :error false)
             styler (get-in data [:flow-item :style] {})
             styler-selected (get-in data [:flow-item :style-selected] {})
             styler (if selected? styler-selected styler)
           ;; sub-flow-lookup (when (= ttype :sub-flow)
           ;;                   (let [subby-results (get-in @(re-frame/subscribe [::http/flow-results]) [:return-maps])
           ;;                         sub-flow-lookup (into {} (for [k (keys subby-results)] {(keyword (last (cstr/split (str k) "/"))) k}))
           ;;                         sfl (get sub-flow-lookup bid)] sfl))
            ;; dt (if (some #(= % :*) (keys outputs)) :map ;; if its a map always use map color for base block
            ;;        (gn (first (vals outputs))))
             ;value (if port-multi? )
             dt (ut/data-typer value)
            ; _ (when (= bid :open-fn-1) (tap> [:pc2 dt bid pid return-type override-value value]))
            ;;  dt (gn (first (vals outputs)))
             ;;_ (tap> [:dt dt bid pid return-type])
             bcolor (try
                      (cond error? "red"
                            python? "#F0E68C"
                            :else (or (get styler :color nil)
                                      (get (theme-pull :theme/data-colors db/data-colors)
                                           dt
                                           (get (theme-pull :theme/data-colors db/data-colors) "unknown" "#FFA500"))))
                      (catch :default _ "orange"))]
         (cond (= return-type :data-type) dt
               (= return-type :data-value) value
               :else bcolor)) ;; assume color

       (let [pid (if (and port-multi? (= pid :out)) :* pid) ;; change up the default out for rendering reasons
             value (if (and port-multi? (not (= pid :*))) ;; if :* just pass the whole value
                     (try
                       (let [idx? (cstr/starts-with? (str pid) ":idx")
                             pkey (if idx? (edn/read-string (cstr/replace (str pid) ":idx" ""))
                                      pid)]
                         (get value pkey))
                       (catch :default _ value))
                     value)
             dt (if (nil? value) (get-in flow-map [bid :ports :out pid]) (ut/data-typer value))
             color (get (theme-pull :theme/data-colors db/data-colors)
                        (gn dt)
                        (get (theme-pull :theme/data-colors db/data-colors) "unknown" "#FFA500"))
             ;_ (when (= bid :static-value-2-out) (tap> [:pc3 dt value bid pid return-type override-value value]))
             ]
        ;;  (when (and (= bid :open-input-3) (= pid :rando*))
        ;;    (tap> [:cc dt color bid pid return-type]))
         (cond (= return-type :data-type) dt ;(or dt :none)
               (= return-type :data-value) value
               :else color))))))

(re-frame/reg-sub
 ::block-inputs
 (fn [db [_ bid]]
   (get-in db [:flows (get db :selected-flow) :map bid :data :flow-item :inputs])))

(re-frame/reg-sub
 ::input-defaults
 (fn [db [_ bid]]
   (get-in db [:flows (get db :selected-flow) :map bid :data :flow-item :defaults] {})))

(re-frame/reg-sub
 ::meta-bundle
 (fn [db [_ bid pid]]
   {:meta (get-in db [:flows (get db :selected-flow) :map bid :data :flow-item :meta pid] {})
    :default (get-in db [:flows (get db :selected-flow) :map bid :data :flow-item :defaults pid] {})}))

(def sniffy-sniff (reagent/atom nil))

(defn step-ports [bid ports w input? flow-id x y & [done? bcolor]]
  (let [;;ports (into {} (for [e (range (+ 1 (rand-int 6)))] {(keyword (str "a" (rand-int 123))) (rand-nth [:any :string :vector :integer :float :boolean])})) ;; test
        flow-select @(re-frame/subscribe [::selected-flow-block])
        valid-drops (when (and @dragging-port?
                               (not (= bid (first @dragged-port)))) ;; dont want to highlight self drop
                      (valid-drop-ports ports))
        react! [@port-hover @dragged-port @flow-details-block-container-atom]
        binputs @(re-frame/subscribe [::block-inputs bid])
        defaults (when input? @(re-frame/subscribe [::input-defaults bid]))
        condi-ports (when (not input?) @(re-frame/subscribe [::condi-ports bid]))
        push-ports (when (not input?) @(re-frame/subscribe [::push-ports bid]))
        condi-valves (when condi-ports @(re-frame/subscribe [::bricks/condi-valves flow-id])) ;; react!
        outport-keys (into (into (vec (sort-by str (keys ports))) (keys condi-ports)) (keys push-ports))
        ;; _ (when (not (empty? condi-ports))
        ;;     (tap> [:ports bid input? condi-ports ports ]))
        ports (if (not input?) (let [cports (into {} (for [n (into (keys condi-ports) (keys push-ports))] {n (get ports :out :any)}))]
                                 (merge ports cports)) ports)
        inputs-vec (try
                     (edn/read-string binputs) ;; some legacy flows have a string here...
                     (catch :default _ binputs))
        port-width (/ (- w 5) (count ports))]
   ;(tap> [:valid-drops @dragged-port valid-drops])
    [re-com/h-box
     :size "none"
     :height "15px"
     :justify :between
     :attr {:on-mouse-enter  #(reset! port-hover2 bid)
            :on-mouse-over   #(when (not (= @port-hover2 bid))
                                (reset! port-hover2 bid))
            :on-mouse-leave  #(reset! port-hover2 nil)}
     :align :center
     :style {:font-size "10px"
             :z-index 687}
     :children (for [[k v] (if input?
                             (sort-map-keys (vec (or inputs-vec (keys ports))) ports)
                             ;ports
                             ;ports
                             (sort-map-keys
                              (try outport-keys ;(sort (keys ports))
                                   (catch :default e (do (tap> [:key-sorting-error e
                                                                ;(keys ports)
                                                                outport-keys
                                                                :flows.cljs :ln 1802])
                                                         ;(keys ports)
                                                         outport-keys)))
                              ports))

                     :let [hover-key [bid input? k (str " " k)]
                           valid-drop? (true? (and input? @dragging-port? (some #(= k %) valid-drops)))
                           hovered? (true? (= hover-key @port-hover))
                           hover-path (get-in @flow-details-block-container-atom [flow-id flow-select :port-panel (if input? :in :out)])
                           panel-hovered? (true? (= [bid k] hover-path))
                           [ibid ivk] (if input? @(re-frame/subscribe [::get-incoming-port bid :in k]) [nil nil])
                           port-color @(re-frame/subscribe [::port-color flow-id (or ibid bid) (or ivk k)])
                           has-default? (when input? (not (nil? (get defaults k))))
                           filled? (if input?
                                     @(re-frame/subscribe [::get-incoming-port bid :in k])
                                     (not (empty? @(re-frame/subscribe [::get-outgoing-port bid (if input? :in :out) k]))))
                          ;;  _ (when (= bid :unpack-results-map) (tap> [:filler @(re-frame/subscribe [::get-outgoing-port bid :out k]) bid k]))
                           ;ptype @(re-frame/subscribe  [::port-color flow-id (or ibid bid) (or ivk k) :data-type])
                           multi? (cstr/ends-with? (str k) "+")
                           pval1 (when (> (count ports) 1)
                                   @(re-frame/subscribe [::port-color flow-id (or ibid bid) (or ivk k) :data-value])
                                   ;@(re-frame/subscribe [::port-color flow-id bid nil :data-value])
                                   )
                           ;;<i class= "fa-solid fa-circle-notch" ></i>
                           condi? (and (not input?) (cstr/starts-with? (str k) ":cond-path"))
                           push? (and (not input?) (cstr/starts-with? (str k) ":push-path"))
                           valve-open? (when condi? @(re-frame/subscribe [::bricks/condi-valve-open? flow-id bid k])) ;; TODO needs bid
                          ; port-color @(re-frame/subscribe [::port-color flow-id bid k])
                          ;out? (not input?)
                          ;[xs ys] [(if out? (+ x w) x) (+ y 4 (+ 6 (* 1 22)))]
                           ;;_ (tap> [:port-color bid input? port-color k ivk pval1 @(re-frame/subscribe [::port-color flow-id bid k :data-type])])
                           icon [re-com/md-icon-button
                                 :src (at)
                                ;;  :md-icon-name (if done?
                                ;;                  "zmdi-check-circle"
                                ;;                  (if (or hovered? (and (= bid (first @dragged-port))
                                ;;                                        @dragging-port?
                                ;;                                        (= k (second @dragged-port)))
                                ;;                          (and filled? panel-hovered?))
                                ;;                    "zmdi-circle"
                                ;;                    "zmdi-circle-o"))
                                ;; :style (if condi? {:font-size "9px"} {})
                                 :md-icon-name (cond
                                                 ;condi? "fa-solid fa-circle-notch"
                                                 push? "zmdi-swap-vertical-circle" ;;"zmdi-caret-down-circle"
                                                 valve-open? "zmdi-dot-circle"
                                                 multi? "zmdi-plus-circle-o-duplicate"
                                                 done? "zmdi-check-circle"
                                                 has-default? "zmdi-dot-circle-alt"
                                                 (or hovered?
                                                     (and (= bid (first @dragged-port))
                                                          @dragging-port?
                                                          (= k (second @dragged-port)))
                                                     (and filled? panel-hovered?)) "zmdi-circle"
                                                 :else "zmdi-circle-o")
                                 :attr (if valid-drop?
                                         {:on-drag-over #(do ;(tap> [:h hovered? hover-key @port-hover])
                                                           (when (not (= @port-hover hover-key))
                                                             (reset! port-hover hover-key)))
                                          :on-drag-leave #(do
                                                            (reset! port-hover nil))
                                          :on-drop #(let [connected-string (str (first @dragged-port) "/" (second @dragged-port))
                                                          ts (str (.toISOString (js/Date.)))]
                                                     ;(buffy/render-honey-comb-fragments [:play "/home/ryanr/notify2.mp3"])
                                                     ;(alert! "dropped")
                                                      (reset! port-hover [:dropped])
                                                      (set-delay! port-hover nil 3500)
                                                      (swap! involved into [bid (first @dragged-port)])
                                                     ;(swap-delay! involved disj (first @dragged-port) 200)
                                                      (reset-delay! involved nil 3000)
                                                     ;(reset-delay! db/speech-log nil 5000)
                                                      (reset! tentacle-pos [0 0])
                                                      (ut/dispatch-delay 200
                                                                         [::http/insert-alert
                                                                          [:h-box
                                                                           :gap "7px"
                                                                           :children
                                                                           [[:box :child connected-string]
                                                                            [:md-icon :md-icon-name "zmdi-chevron-right"
                                                                             :style {:font-size "24px"}]
                                                                            [:box :child (str bid "/" k)]
                                                                            [:play1 ["/home/ryanr/rechamber.mp3" ts]]
                                                                            [:md-icon :md-icon-name "zmdi-input-composite"
                                                                             :style {:font-size "24px"}]]] 9 1 5]))}
                                         (merge
                                          (when ;(and
                                           (> (count ports) 1)
                                                 ;    (not input?)) ;; no we def want to peek inputs too
                                            {:on-mouse-over #(when (not (= @sniffy-sniff [input? k pval1]))
                                                               (reset! sniffy-sniff [input? k pval1]))
                                             :on-mouse-enter #(reset! sniffy-sniff [input? k pval1])
                                             :on-mouse-leave #(reset! sniffy-sniff nil)})
                                          {:on-context-menu #(let [removed-count (remove-connections bid k input?)]
                                                               ;(tap> [:click!])
                                                               (doseq [e (range removed-count)
                                                                       :let [disconnected-string (str bid "/" k "(" e "/" removed-count ")")
                                                                             ts (str (.toISOString (js/Date.)))]]
                                                                 (do (swap! involved conj bid)
                                                                     (reset-delay! involved nil 3000)
                                                                   ;(reset-delay! db/speech-log nil 5000)
                                                                     (ut/dispatch-delay (rand-int 300)
                                                                                        [::http/insert-alert
                                                                                         [:h-box
                                                                                          :gap "7px"
                                                                                          :children
                                                                                          [[:box :child disconnected-string]
                                                                                           [:md-icon :md-icon-name "zmdi-close"
                                                                                            :style {:font-size "24px"}]
                                                                                           [:box :child (str bid "/" k)]
                                                                                           [:play1 ["/home/ryanr/dechamber.mp3" ts]]
                                                                                           [:md-icon :md-icon-name "zmdi-input-composite"
                                                                                            :style {:font-size "24px"}]]] 9 1 5]))))}))

                               ;;  :attr (when (not @dragging-flow?)
                               ;;          {:on-mouse-over #(when (or (not (= @flow-hover bid))
                               ;;                                     (not (= @port-hover hover-key)))
                               ;;                             (reset! flow-hover bid)
                               ;;                             (reset! port-hover hover-key))
                               ;;           :on-mouse-leave #(do
                               ;;                              (reset! flow-hover nil)
                               ;;                              (reset! port-hover nil))})
                                ;; :class (if (and hovered? valid-drop?) "pulsate" "")
                                 :style {:color (if panel-hovered? port-color (str port-color 99)) ;(get (theme-pull :theme/data-colors db/data-colors) (gn v))
                                         :cursor (if (and
                                                      (not (and input? has-default?))
                                                      input?) "pointer"  "grab")
                                         :transition "all 0.5s ease-in-out"
                                         ;:filter (when panel-hovered? (str "brightness(200%) drop-shadow(2px 1px 4px " port-color ")"))
                                         :filter (when panel-hovered? (str "brightness(200%)"))
                                         ;:mix-blend-mode "lighten"
                                         ;:margin-left (if hovered? "-5px" "inherit")
                                         :margin-left (cond
                                                        valid-drop? "-5px"
                                                        hovered? "-9px"
                                                        panel-hovered? "-2px"
                                                        (and @dragging-port? (not (= bid (first @dragged-port)))) "3px"
                                                        :else "inherit")
                                        ;;  :margin-left (if valid-drop?
                                        ;;                 (if hovered? "-9px" "-5px")
                                        ;;                 (if (and @dragging-port? (not (= bid (first @dragged-port))))
                                        ;;                   "3px"
                                        ;;                   "inherit"))
                                         :z-index (if (or panel-hovered? hovered?) 999 900)
                                         :font-size (cond ;condi? "14px"
                                                      hovered? "34px"
                                                      panel-hovered? "20px"
                                                      valid-drop? "26px"
                                                      (and @dragging-port? (not (= bid (first @dragged-port)))) "10px"
                                                      :else "16px")}]]]

                 ^{:key (str "flow-ports-" bid (if input? "-in-" "-out-") k)}
                 [re-com/box
                  :size "none"
                  :align (if input? :end :start)
                  :width (px port-width)
                  :height "15px"
                 ;; :style {:border (if input?
                 ;;                   "1px solid pink"
                 ;;                   "1px solid purple")}
                  :child (cond
                           (not input?)  [draggable-port icon flow-id bid k v x y]
                           valid-drop?   [droppable-port icon bid k]
                           (and input? has-default?) [draggable-port icon flow-id bid {:from bid
                                                                                       :input-pid k
                                                                                       :port-meta @(re-frame/subscribe [::meta-bundle bid k])
                                                                                       :flow-id flow-id
                                                                                       :keypath {}} v x y]

                           :else         icon)])]))
(re-frame/reg-sub
 ::input-out-of-date?
 (fn [db [_ bid]]
   (if (= (get-in db [:flows (get db :selected-flow) :map bid :data :drag-meta :type]) :open-block)
     (let [user-input (get-in db [:flows (get db :selected-flow) :map bid :data :user-input])
           resolved-val @(re-frame/subscribe [::port-color (get db :selected-flow) bid :out :data-value])]
       (not (= user-input resolved-val))) false)))

(re-frame/reg-sub
 ::syntax
 (fn [db [_ bid]]
   (get-in db [:flows (get db :selected-flow) :map bid :data :syntax] "clojure")))

(re-frame/reg-sub
 ::image-check
 (fn [db _]
   (vec (for [kp (ut/kvpaths db)
              :when (ut/is-base64? (get-in db kp))]
          kp))))

(defn step-center [bid flow-id bcolor h w selected? icon]
  (let [hovered?      (and (= bid (first @port-hover))
                           (not @dragging-flow?))
        ;ccolor (hsl->hex (complement bcolor))
        shades-color (try (vec (for  [s (shades bcolor)] (hsl->hex s))) (catch :default _ ["#ffffff"]))
        ;triad-color (vec (for  [s (triad bcolor)] (hsl->hex s)))
        syntax @(re-frame/subscribe [::syntax bid])
        tetrad-color (try (vec (for  [s (tetrad bcolor)] (hsl->hex s))) (catch :default _ ["#ffffff"])) ;; bad colors will crash it.
        mouse-down-fn #(mouse-down-handler-block % bid)
        ;running? (is-running? bid flow-id)
        running? @(re-frame/subscribe [::is-running? bid flow-id])
        tbid (let [cc (/ w 7) ;; px to char width
                   llen (count (str bid))]
               ;;(tap> [:tbid cc bid w])
               (if (< cc llen) (str (subs (str bid) 0 cc) "..") bid))]
    ;(tap> [:center bid h w])
    ;;(tap> [:image-check (count @(re-frame/subscribe [::image-check])) @(re-frame/subscribe [::image-check])])
    ;(tap> [:checker (ut/is-base64? "Lato") (ut/is-base64? "")])
    ^{:key (str "flow-brick-center-" bid)}
    [re-com/v-box
     :height (px (- h 36))
     :size "none"
     :padding "4px"
    ;:align (if hovered? :end :start)
     :justify :center
    ;;  :attr {:on-mouse-down mouse-down-fn
    ;;        ;:on-double-click #(when sub-flow-lookup
    ;;        ;                    (re-frame/dispatch [::http/load-flow-w-alias
    ;;        ;                                        (get-in @(re-frame/subscribe [::http/flow-results]) [:run-refs sub-flow-lookup 0]) (str sub-flow-lookup)]))
    ;;         :on-click #(select-block bid)}
     ;:gap "4px"
     :children [[re-com/h-box
                 :height "21px"
                 :style {:margin-top "-4px"}
                 :justify :between :align :center
                 :children [[re-com/box
                             :size "auto"
                             :attr {:on-mouse-down mouse-down-fn
           ;:on-double-click #(when sub-flow-lookup
           ;                    (re-frame/dispatch [::http/load-flow-w-alias
           ;                                        (get-in @(re-frame/subscribe [::http/flow-results]) [:run-refs sub-flow-lookup 0]) (str sub-flow-lookup)]))
                                    :on-click #(select-block bid)}
                             :style {:font-size "10px"
                                     :font-weight 700
                                     :color (if selected?
                                              (first shades-color)
                                              (last shades-color))}
                             :child (if (and @sniffy-sniff (= @port-hover2 bid))
                                      ;(str (if (first @sniffy-sniff) :in-port :out-port) " " (get @sniffy-sniff 1))
                                      [re-com/h-box :size "auto"
                                       ;:style {:transition "all 0.5s ease-in-out"}
                                       :justify :between :align :center
                                       :children (for [e [(if (first @sniffy-sniff) :in-port :out-port) (get @sniffy-sniff 1)]]
                                                   [re-com/box :child (str e)])]
                                      (if hovered? (last @port-hover) (str tbid)))]

                            (if running?
                              [re-com/md-icon-button :src (at)
                               :md-icon-name "zmdi-spinner" ;""zmdi-refresh-sync"
                               :class "rotate-reverse linear infinite"
                               :style {:color (if selected?
                                                (last tetrad-color)
                                                (first tetrad-color))
                                       :cursor "pointer"
                                       :margin-top "4px"
;                                       :transform-origin "21.5px 22px"
                                       :transform-origin "11px 11px"
                                       ;:filter "drop-shadow(0.25rem 0.35rem 0.4rem rgba(0, 0, 0, 0.44))"
                                       :font-size "22px"}
                               ;:on-click #(re-frame/dispatch [::run-current-flowmap])
                               ]
                              (when (and icon (>= w 125))
                                [render-icon icon
                               ;(if selected? (ut/invert-hex-color bcolor)
                               ;                     bcolor)
                                 (if selected?
                                   (last tetrad-color)
                                   (first tetrad-color))
                                 20 17]))]]
                (when (>= h 75)
                  (let [pval @(re-frame/subscribe [::port-color flow-id bid :out :data-value])
                        view-pval @(re-frame/subscribe [::port-color flow-id (keyword (str (cstr/replace (str bid) ":" "") "-vw")) :out :data-value])
                        out-of-date? @(re-frame/subscribe [::input-out-of-date? bid])]

                    ;; [re-com/box
                    ;;  :style {;:border-top (str "1px dashed " (ut/invert-hex-color bcolor) "45")
                    ;;          :font-size "9px"}
                    ;;  :size "auto" :justify :center :align :end
                    ;;  :child (str pval)]

                    [re-com/box
                     :size "none"
                     :height (px (- h 60))
                     :style {:border-radius "4px"
                             :margin-top "-3px"
                             :overflow "hidden"
                             :border (when out-of-date? (str "2px dashed " (if selected?
                                                                             (last tetrad-color)
                                                                             (first tetrad-color))))
                             :background-color "#00000099"}
                     :child

                     (let [;pval (try (if (or (vector? pval) (map? pval) (list? pval))
                           ;       (ut/replace-large-base64 pval) pval) (catch :default _ pval))
                           pval (if (or (vector? pval) (map? pval) (list? pval)) (ut/replace-large-base64 pval) pval)
                           view-pval (if (or (vector? view-pval) (map? view-pval) (list? view-pval)) (ut/replace-large-base64 view-pval) view-pval)
                           b64? (ut/is-base64? (str pval))
                           simple-value? (and (or (string? pval) (number? pval) (boolean? pval)) (not b64?))
                           rabbit-code-view? (or (and (vector? view-pval) (keyword? (first view-pval))) ;; is it renderable rabbit-code?
                                                 (and (map? view-pval) (contains? view-pval :queries) (contains? view-pval :view)))
                           rabbit-code? (or (and (vector? pval) (keyword? (first pval))) ;; is it renderable rabbit-code?
                                            (and (map? pval) (contains? pval :queries) (contains? pval :view)))]
                       (cond

                         rabbit-code-view?
                         [re-com/box
                          :padding "5px"
                                      ;:size "auto"
                                      ;:align :center :justify :center
                          :child [buffy/render-honey-comb-fragments view-pval
                                      ;13 9
                                  ;(/ w 41) (/ h 41) ; nil ; (/ h 50)
                                  (/ (- w 22) 50) (/ h 50) ; nil ; (/ h 50)
                                  ;nil nil
                                  ]]

                         rabbit-code?
                         [re-com/box
                          :padding "5px"
                                      ;:size "auto"
                                      ;:align :center :justify :center
                          :child [buffy/render-honey-comb-fragments pval
                                      ;13 9
                                  ;(/ w 41) (/ h 41) ; nil ; (/ h 50)
                                  (/ (- w 22) 50) (/ h 50) ; nil ; (/ h 50)
                                  ]]

                         simple-value?
                         (let [;ss (ut/auto-font-size pval (- h 50) (- w 25))
                               ;ww (js/Math.ceil (/ w 50))
                               ;ss (/ ss 2)
                               ;ss (if (< ss 8) 8 ss)
                               ;ss (if (> ss 25) 25 ss)
                               ss 10]
                           [re-com/box :child (if (string? pval) (pr-str pval) (str pval))
                            :style {:font-size (px ss)
                                    :padding "5px"
                                    :opacity 0.77
                                    :color (theme-pull :theme/editor-font-color nil)
                                    :font-family (theme-pull :theme/monospaced-font nil)
                                    ;:padding-left "5px"
                                    ;:padding-right "5px"
                                  ;:color (if selected?
                                  ;         (last tetrad-color)
                                  ;         (first tetrad-color))
                                  ;:font-weight 700
                                  ;:overflow "hidden"
                                  ;:text-overflow "ellipsis"
                                  ;:white-space "nowrap"
                                  ;:max-width (px (- w 10))
                                    }])

                         (and (try (not (empty? pval)) (catch :default _ false)) b64?)
                         [re-com/v-box
                          ;:padding "16px"
                                                                ;:width "400px"
                          :size "auto"
                          :align :center :justify :center
                          ;:gap "10px"
                          :children
                          [;[re-com/box :child (str "(**base64 string: ~" (.toFixed (/ (* (count (str pval)) 6) 8 1024 1024) 2) " MB)")]
                           [re-com/box
                            ;:size "auto"
                            :child [:img {:src (str "data:image/png;base64," pval)
                                          :width (px (- w 10))}]]]]
                         :else [code-box-smol
                                w
                                (- h 36)
                                (if (and @sniffy-sniff (= @port-hover2 bid))
                                  (pr-str (last @sniffy-sniff))
                                  (pr-str pval)) syntax]))]))]

     :style {;:border-top (str "1px solid " bcolor)
             :z-index 800
             :cursor (if selected? "grab" "pointer")
             ;:font-family (theme-pull :theme/monospaced-font nil)
              ;:border (str "1px solid white")
             ;:font-size "12px"
             :background-color (str bcolor (if selected? "" 66))}]))
            ;:border-bottom (str "1px solid " bcolor)


;; (re-frame/reg-sub
;;  ::block-run-time
;;  (fn [db [_ flow-id bid]]
;;    (get-in db/flow-results [:tracker flow-id bid])
;;     ))


(defn flow-grid [ww hh xx yy flowmaps-connections flow-id flow-map]
  (let [selected-bid @(re-frame/subscribe [::selected-flow-block])
        playing?     @(re-frame/subscribe [::audio/audio-playing?])
        done-block   (get (first (filter #(= (last %) :done) flowmaps-connections)) 0 :?)
        zoom-unlocked? @(re-frame/subscribe [::zoom-unlocked?])
        grid-line-color (str (theme-pull :theme/editor-outer-rim-color nil) 33)
        grid-line-color2 (str (theme-pull :theme/editor-outer-rim-color nil) 10)
        ;audio-playing? @(re-frame/subscribe [::audio/audio-playing?])
        reactions!   [@port-hover @involved @flow-details-block-container-atom @(re-frame/subscribe [::http/flow-results])]
        read-only-flow? (true? (cstr/includes? flow-id "/"))
        bounds-x (when read-only-flow? (apply min (for [[_ {:keys [x]}] flow-map :let []] x)))
        bounds-y (when read-only-flow? (- (apply min (for [[_ {:keys [y]}] flow-map :let []] y)) 30))
        bounds-x2 (when read-only-flow? (apply max (for [[_ {:keys [x w]}] flow-map :let []] (+ w x))))
        bounds-y2 (when read-only-flow? (apply max (for [[_ {:keys [y h]}] flow-map :let []] (+ y h))))
        done-data-type-color (when read-only-flow?
                               (get (theme-pull :theme/data-colors db/data-colors)
                                    (if (some #(= % :*) (keys (get-in flow-map [done-block :ports :out]))) :map
                                        (gn (first (vals (get-in flow-map [done-block :ports :out])))))
                                    (get (theme-pull :theme/data-colors db/data-colors) "unknown" "#FFA500")))]

   ;;(tap> [:run @(re-frame/subscribe [::http/flow-results])])

   ;(tap> [:flow-hover @flow-hover])
;;    (zoom-to-element "flow-brick-:open-input-1" 10)
    [re-com/h-box
     :size "none" :align :center :justify :center
     :width (px canvas-width) ;;"3200px" ;"6400px" ;(px ww)
     :height (px canvas-height) ;;"2400px" ;"3600px" ;(px hh)
     :style {:overflow "hidden"
             ;:display "grid"
             ;:grid-template-columns "repeat(auto-fill, 50px)"
             ;:grid-template-rows "repeat(auto-fill, 50px)"
             :background-image (if zoom-unlocked?
                                 (str "linear-gradient(to right, " grid-line-color
                                      " 1px, transparent 1px), linear-gradient(to bottom, "
                                      grid-line-color " 1px, transparent 1px)")
                                 (str "linear-gradient(to right, " grid-line-color2
                                      " 1px, transparent 1px), linear-gradient(to bottom, "
                                      grid-line-color2 " 1px, transparent 1px)"))
             :background-size (when true ;zoom-unlocked?
                                "75px 75px")
             :border (when zoom-unlocked? (str "3px dashed " (theme-pull :theme/editor-outer-rim-color nil)))}
     :children (conj (for [[bid {:keys [x y h w z data icon ports]} :as fmap] flow-map
                           :let [in-ports-map    (get ports :in)
                                 out-ports-map   (get ports :out)
                                 selected?       (= bid selected-bid)
                                 done-block?     (= bid done-block)
                                 hover-involved? (true? (some #(= % bid) (get-in @flow-details-block-container-atom [flow-id :involved])))
                           ;running?        (is-running? bid flow-id)
                                 running?         @(re-frame/subscribe [::is-running? bid flow-id])
                           ;_ (tap> [:inny? (get-in @flow-details-block-container-atom [flow-id :involved])])
                           ;; python?         (= (get data :syntax) "python")
                           ;; styler          (get-in data [:flow-item :style] {})
                                 did             (str "flow-brick-" bid)
                           ;; override-value  (get-in @(re-frame/subscribe [::http/flow-results]) [:return-maps (str flow-id) bid])
                           ;; value           (if override-value override-value (get data :user-input))
                                 shake?          false ;(and playing? (some #(= % bid) @involved))
                         ;;styler-selected (get-in data [:flow-item :style-selected] {})
                           ;; error? (get value :error false)
                           ;; bcolor (try
                           ;;          (cond error? "red"
                           ;;                python? "#F0E68C"
                           ;;                :else (or (get styler :color nil)
                           ;;                          (get (theme-pull :theme/data-colors db/data-colors)
                           ;;                               (if (some #(= % :*) (keys out-ports-map)) :map ;; if its a map always use map color for base block
                           ;;                                   (gn (first (vals out-ports-map))))
                           ;;                               (get (theme-pull :theme/data-colors db/data-colors) "unknown" "#FFA500"))))
                           ;;          (catch :default _ "orange"))
                                 bcolor @(re-frame/subscribe [::port-color flow-id bid])
                           ;_ (when (= bid :open-fn-1) (tap> [bid :bcolor bcolor]))
                           ;_ (tap> selected?)
                                 ]

                           :when true]
                       ^{:key did}
                       [re-com/v-box
                        :attr {:id did
                               :on-mouse-enter #(reset! flow-hover bid)
                               :on-mouse-over  #(when (not (= @flow-hover bid)) (reset! flow-hover bid))
                               :on-mouse-leave #(reset! flow-hover nil)}
                        :class (when shake? "rotate-fast")
                        :style {:position "fixed"
                                :z-index 610
                                :border-radius "5px"
                                :filter (when (and (not (nil? selected-bid)) (not running?))
                                          (when (and (not hover-involved?)
                                                     (not selected?)) "brightness(45%)")) ;(str "drop-shadow(2px 1px 4px #000000)")
                          ;:filter (when selected? (str "brightness(200%) drop-shadow(2px 1px 4px " bcolor ")"))
                          ;:transform (when selected? "scale(1.1)")
                          ;:opacity (when (not selected?) 1.0 0.3)
                          ;; :box-shadow (if (and audio-playing? selected?)
                          ;;               (let [block-id :audio
                          ;;                     talking-block? true]
                          ;;                 (cond (and audio-playing? talking-block?)
                          ;;                       (str "1px 1px " (px (* 80 (+ 0.1
                          ;;                                                    (get @db/audio-data block-id))))  " "
                          ;;                            (theme-pull :theme/editor-outer-rim-color nil))
                          ;;                                  ;(str "1px 1px 70px red")
                          ;;         ;(or (and hovered? @on-block) kit-drop?)
                          ;;         ;(str "1px 1px 30px " (get db/block-codes block-type) 87)
                          ;;                       :else "none"))
                          ;;               (if (or hover-involved? selected? running?)
                          ;;                 (str "2px 1px 15px " bcolor)
                          ;;                 (str "2px 1px 15px #00000045")))
                                :box-shadow (if (or hover-involved? selected? running?)
                                              (str "2px 1px 15px " bcolor)
                                              (str "2px 1px 15px #00000045"))

                                :user-select "none"
                                :border (str "3px " (if selected? "dashed" "solid") " " bcolor (if selected? "" 45))
                                :background-color (str bcolor (if (or hover-involved?
                                                                      selected?) 75 35))
                                :width (px w)
                                :height (px h)
                                :left x :top y}
                        :children [[step-ports bid in-ports-map w true flow-id x y]
                                   [step-center bid flow-id bcolor h w selected? icon]
                                   [step-ports bid out-ports-map w false flow-id x y done-block? bcolor]
                                   (when selected?
                                     [re-com/box ;; resize box
                                      :attr {:on-mouse-down #(resize-mouse-down-handler % bid)}
                                      :style {:position "fixed"
                                              :z-index 985
                                              :size "none"
                                       ;:background-color "blue"
                                              :cursor "se-resize" ;; (theme-pull :theme/editor-outer-rim-color nil)
                                              :border-right (str "5px solid " bcolor)
                                              :border-bottom (str "5px solid " bcolor)
                                              :left (- (+ x w) 12)
                                              :top (- (+ y h) 12)}
                                      :height "12px" :width "12px"
                                      :child " "])
                                   (when selected?
                                     [re-com/box ;; close box
                                      :style {:position "fixed"
                                              :z-index 985
                                              :size "none"
                                       ;:background-color "blue"
                                              :left (- (+ x w) 17)
                                              :top (- y 2)}
                                      :height "12px" :width "12px"
                                      :child [re-com/md-icon-button :src (at)
                                              :md-icon-name "zmdi-close"
                                              :on-click #(remove-block bid)
                                              :style {:color bcolor
                                                      :cursor "pointer"
                                                      :height "12px"
                                                      :font-size "16px"}]])]])

                     (when (cstr/includes? (str flow-id) "/")
                       [re-com/box
                        :style {:position "fixed"
                                :left (- bounds-x 50)
                                :top (- bounds-y 30)
                                :width (px (+ (- bounds-x2 bounds-x) 150))
                                :height (px (+ (- bounds-y2 bounds-y) 100))
                                :border-radius "14px"
                                :font-size "10px"
                                :border (str "6px solid " done-data-type-color)
                                :z-index 500}
                        :padding "5px"
                        :child "read-only sub-flow"]))]))



;; (defn flow-grid2 [ww hh xx yy flowmaps-connections flow-id flow-map]
;;   (let [ww (- ww 16)
;;         hh (- hh 37)
;;         [xx yy] @detached-coords
;;        ;flowmaps-connections @(re-frame/subscribe [::flowmap-connections])
;;        ;flow-id @(re-frame/subscribe [::selected-flow])
;;        ;flow-map @(re-frame/subscribe [::flowmap])
;;         done-block (get (first (filter #(= (last %) :done) flowmaps-connections)) 0 :?)
;;         read-only-flow? (true? (cstr/includes? flow-id "/"))
;;        ;coords (generate-coords xx yy)
;;         react-hack [@dragging-port? @flow-hover @dragging-flow? @flow-drop-hover
;;                     @(re-frame/subscribe [::http/flow-results]) @db/pan-zoom-offsets @dragging? @ports-react-render] ;; important to force re-render
;;        ;flow-selected :my-flow-1
;;         bounds-x (when read-only-flow? (apply min (for [[_ {:keys [x]}] flow-map :let []] x)))
;;         bounds-y (when read-only-flow? (apply min (for [[_ {:keys [y]}] flow-map :let []] y)))
;;         bounds-x2 (when read-only-flow? (apply max (for [[_ {:keys [x w]}] flow-map :let []] (+ w x))))
;;         bounds-y2 (when read-only-flow? (apply max (for [[_ {:keys [y h]}] flow-map :let []] (+ y h))))
;;        ;done-data-type (get-in flow-map [done-block :ports :out :out])
;;        ;done-data-type-color (get (theme-pull :theme/data-colors db/data-colors) done-data-type "#FFA500")
;;         done-data-type-color (when read-only-flow?
;;                                (get (theme-pull :theme/data-colors db/data-colors)
;;                                     (if (some #(= % :*) (keys (get-in flow-map [done-block :ports :out]))) :map ;; if its a map always use map color for base block
;;                                         (gn (first (vals (get-in flow-map [done-block :ports :out])))))
;;                                     (get (theme-pull :theme/data-colors db/data-colors) "unknown" "#FFA500")))]

;;    ;(tap> [:done-block done-block flow-map])
;;    ;(tap> [:reserved-type-keywords @(re-frame/subscribe [::reserved-type-keywords])])
;;    ;(tap> [:generate-coords coords])
;;    ;(tap> [:db/flow-results @(re-frame/subscribe [::http/flow-results])])

;;     ^{:key (str "flow-brick-grid")}
;;     [re-com/h-box
;;      :size "none" :align :center :justify :center
;;      :width "6400px" ;(px ww)
;;      :height "3600px" ;(px hh)
;;      :style {:overflow "hidden"}
;;      :children (conj
;;                 (for [[bid {:keys [x y h w z data icon ports]} :as fmap] flow-map
;;                       :let [x (+ x 4) ;; offsets from borders
;;                             y (+ y 30)
;;                             orig-h h
;;                             ttype (get-in data [:drag-meta :type])
;;                             pill-size 17
;;                             done-block? (= bid done-block)
;;                             inputs (get ports :in {})
;;                             python? (= (get data :syntax) "python")
;;                             icon (if python? "zmdi-language-python" icon)
;;                             expandable-in? (get-in data [:flow-item :expandable?] false)
;;                             outputs (get ports :out)
;;                             override-value (get-in @(re-frame/subscribe [::http/flow-results]) [:return-maps (str flow-id) bid])
;;                             outputs (if (and override-value (= (vec (keys outputs)) [:out])) ;; if we have return data,
;;                                       {:out (keyword (ut/data-typer override-value))} ;; change output data types to be more accurate
;;                                       outputs)
;;                             [w cw longesti-w longesto-w] (calc-block-widths inputs outputs w)
;;                             zoom-multi (get @db/pan-zoom-offsets 2)
;;                             ox (* 4 zoom-multi)
;;                             oy (* 30 zoom-multi)
;;                             mouse-down-fn #(mouse-down-handler-block % bid)
;;                             only-overridden? (and (empty? inputs)
;;                                                   (not (nil? (get-in @(re-frame/subscribe [::http/flow-results]) [:return-maps (str flow-id) bid])))
;;                                                   (not (= (get-in @(re-frame/subscribe [::http/flow-results]) [:return-maps (str flow-id) bid]) (get data :user-input))))
;;                             value (if override-value override-value (get data :user-input))
;;                             error? (get value :error false)
;;                             flow-select @(re-frame/subscribe [::selected-flow-block])
;;                             selected? (= bid flow-select)
;;                             hovered? (= bid @flow-hover)
;;                             defaults (get-in data [:flow-item :defaults] {})
;;                             required (get-in data [:flow-item :required] [])
;;                             styler (get-in data [:flow-item :style] {})
;;                             styler-selected (get-in data [:flow-item :style-selected] {})
;;                             most-pills (try
;;                                          (apply max [(+ (count outputs) (if (get-in ports [:out :*]) 1 0))
;;                                                      (+ (count inputs) (if expandable-in? 1 0))])
;;                                          (catch :default _ 1))
;;                             too-tall? (> (* most-pills pill-size) h)
;;                             out-taller? (> (count outputs) (+ (count inputs) (if expandable-in? 1 0)))
;;                             h (+ (if out-taller? 40 10) (* most-pills pill-size))
;;                             h (if (< h orig-h) orig-h h)
;;                             sub-flow-lookup (when (= ttype :sub-flow)
;;                                               (let [subby-results (get-in @(re-frame/subscribe [::http/flow-results]) [:return-maps])
;;                                                     sub-flow-lookup (into {} (for [k (keys subby-results)] {(keyword (last (cstr/split (str k) "/"))) k}))
;;                                                     sfl (get sub-flow-lookup bid)] sfl))
;;                             bcolor (try
;;                                      (cond error? "red"
;;                                            python? "#F0E68C"
;;                                            :else (or (get styler :color nil)
;;                                                      (get (theme-pull :theme/data-colors db/data-colors)
;;                                                           (if (some #(= % :*) (keys outputs)) :map ;; if its a map always use map color for base block
;;                                                               (gn (first (vals outputs))))
;;                                                           (get (theme-pull :theme/data-colors db/data-colors) "unknown" "#FFA500"))))
;;                                      (catch :default _ "orange"))]]

;;                   ^{:key (str "flow-brick-" bid)}
;;                   [re-com/box
;;                    :style (merge
;;                            {:box-shadow       (when hovered? (str "inset 0px 0px 30px " bcolor))
;;                             :position "fixed"
;;                             :z-index 610
;;                             :user-select "none"
;;                             :border-radius "12px" ; "0px 0px 12px 12px"
;;                             :border (if only-overridden? (str "5px dashed " bcolor) (str "5px solid " bcolor))
;;                             :width (px w)
;;                             :height (px h)
;;                             :left x :top y}

;;                            (if selected?
;;                              (merge
;;                               {:background (when selected? (str "linear-gradient(" bcolor 75 ", transparent)"))
;;                                :background-size (when selected? "30%")
;;                                :background-position (when selected? "200%")}
;;                               styler-selected)
;;                              (merge
;;                               {:background-color "#00000066"}
;;                               styler)))
;;                          ;; styler

;;                    :attr (if (not @dragging-port?)
;;                            {:on-mouse-enter #(reset! flow-hover bid)
;;                             :on-mouse-over #(when (not (= @flow-hover bid))
;;                                               (reset! flow-hover bid))
;;                             :on-double-click #(when sub-flow-lookup
;;                                                 (re-frame/dispatch [::http/load-flow-w-alias
;;                                                                     (get-in @(re-frame/subscribe [::http/flow-results]) [:run-refs sub-flow-lookup 0]) (str sub-flow-lookup)]))
;;                             :on-click #(select-block bid)
;;                             :on-mouse-leave #(reset! flow-hover nil)}
;;                            {})
;;                    :child (let [pill-fn (fn [src out?]
;;                                           (vec (for [[k v] src
;;                                                      :let [idx (.indexOf (keys src) k)
;;                                                            is-last? (= k (last (keys src)))
;;                                                            is-first? (= k (first (keys src)))
;;                                                            dcolor (try (get (theme-pull :theme/data-colors db/data-colors) (name v))
;;                                                                        (catch :default _ (get (theme-pull :theme/data-colors db/data-colors) (str v))))
;;                                                            added? (and expandable-in?
;;                                                                        (not (some #(= % k) required))) ;; dont allow delete of "required" flow block ports
;;                                                            [xs ys] [(if out? (+ x w) x) (+ y 4 (+ 6 (* idx pill-size)))]
;;                                                            dcolor (ifnil dcolor (get (theme-pull :theme/data-colors db/data-colors) "unknown" "#FFA500"))
;;                                                            body
;;                                                            ^{:key (str "port" idx bid k v)}
;;                                                            [re-com/box
;;                                                             :child (str k) ;; (if added? (str k " -") (str k))
;;                                                             :attr (cond out? {}
;;                                                                         (and (not out?) added?) {:on-context-menu #(do (remove-input bid k)
;;                                                                                                                        (re-frame/dispatch [::delete-port bid k :in])
;;                                                                                                                        (reset! ports-react-render (rand-int 12345)))}
;;                                                                         :else {:on-context-menu #(remove-input bid k)})
;;                                                             :justify (if out? :end :start)
;;                                                             :size "none"
;;                                                             :height (px pill-size)
;;                                                             :width (if out?
;;                                                                      (px longesto-w) ;"45px"
;;                                                                      (px longesti-w))
;;                                                             :style {:background-color dcolor
;;                                                                     :border-radius (cond (and is-first? is-last?)
;;                                                                                          (if out?
;;                                                                                            "0px 10px 0px 12px"
;;                                                                                            "10px 0px 12px 0px")
;;                                                                                          is-last? (if out?
;;                                                                                                     "0px 0px 0px 12px"
;;                                                                                                     (if (and too-tall?
;;                                                                                                              (not expandable-in?))
;;                                                                                                       "0px 0px 12px 12px"
;;                                                                                                       "0px 0px 12px 0px"))
;;                                                                                          is-first? (if out?
;;                                                                                                      "0px 7px 0px 0px"
;;                                                                                                      "10px 0px 0px 0px")
;;                                                                                          :else "none")
;;                                                                     :font-size "11px"
;;                                                                     (if out? :padding-right :padding-left) "4px"
;;                                                                     :user-select "none"
;;                                                                     :cursor (if out? "grab" "inherit")
;;                                                                     :color "#000000" :font-weight 700}]]]
;;                                                  (if out? [draggable-port body flow-id bid k v xs ys] body))))
;;                                 in-pills (when (not @dragging-port?) (pill-fn inputs false))
;;                                 in-pills (if (empty? in-pills) [[re-com/box
;;                                                                  :child (str (cond (= ttype :open-block)  (if only-overridden? :overwritten! :input)
;;                                                                                    expandable-in? :no-inputs
;;                                                                                    :else ttype)) ;"no inputs"
;;                                                                  :align :start
;;                                                                  :justify :center
;;                                                                  :size "none"
;;                                                                  :height (px (+ 3 pill-size))
;;                                                                  :width  (px longesti-w) ;"80px"
;;                                                                  :style {:background-color "#00000000"
;;                                                                          :border-radius "12px 0px 12px 0px"
;;                                                                          :border-right (str "1px solid " bcolor)
;;                                                                          :border-bottom (str "1px solid " bcolor)
;;                                                                          :padding-left "9px"
;;                                                                          :user-select "none"
;;                                                                          :font-size "11px"
;;                                                                          :opacity 0.55
;;                                                                          :color bcolor :font-weight 400}]] in-pills)
;;                                 in-pills (if expandable-in? (conj in-pills
;;                                                                   [re-com/box
;;                                                                    :child "+"
;;                                                                    :align :start
;;                                                                    :justify :center
;;                                                                    :size "none"
;;                                                                    :attr {:on-click #(do (re-frame/dispatch [::add-port bid :in])
;;                                                                                          (reset! ports-react-render (rand-int 12345)))}
;;                                                                    :height (px (+ 3 pill-size))
;;                                                                    :width  (px (- longesti-w 10))
;;                                                                    :style {:background-color "#00000000"
;;                                                                            :cursor "pointer"
;;                                                                            :padding-left "9px"
;;                                                                            :user-select "none"
;;                                                                            :font-size "18px"
;;                                                                            :opacity 0.55
;;                                                                            :color bcolor :font-weight 400}]) in-pills) ;; TODO compress all this logic into single cond binding
;;                                 out-pills (conj (pill-fn outputs true)
;;                                                 (if (and hovered? (not @dragging-flow?))
;;                                                   [re-com/box
;;                                                    :width (px longesto-w)
;;                                                    :size "none" :height "15px"
;;                                                    :style {:left (px (+ x (- w longesto-w)))
;;                                                            :position "fixed"
;;                                                            :padding-right "7px"
;;                                                            :top (px (+ y (- h 29)))}
;;                                                    :align :end :justify :center
;;                                                    :child [re-com/md-icon-button :src (at)
;;                                                            :md-icon-name "zmdi-close"
;;                                                            :on-click #(remove-block bid)
;;                                                            :style {:color bcolor ;(theme-pull :theme/editor-font-color nil)
;;                                                                    :cursor "pointer"
;;                                                                    :height "15px"
;;                                                                    :font-size "19px"}]]
;;                                                   [re-com/box
;;                                                    :align :end :justify :center
;;                                                    :width (px longesto-w) ;:size "auto"
;;                                                    :style {:color bcolor ;(theme-pull :theme/editor-font-color nil)
;;                                                            :cursor "pointer"
;;                                                            :position "fixed"
;;                                                            :padding-right "7px"
;;                                                            :left (px (+ x (- w longesto-w)))
;;                                                            :top (px (+ y (- h 25)))
;;                                                            :height "15px"
;;                                                            :font-size "19px"}
;;                                                    :child [render-icon icon bcolor]]))]


;;                             [re-com/h-box
;;                              :style {:z-index 605}
;;                              :width (px w) :size "none"
;;                              :children [[re-com/box
;;                                          :size "auto"
;;                                          :width (px (- w 10)) ;(px (* w 2.25))
;;                                          :attr {:on-mouse-down mouse-down-fn}
;;                                          :style {:color bcolor
;;                                                  :cursor (if @dragging? "grabbing" "grab")
;;                                                  :position "fixed"
;;                                                  :overflow "visible"
;;                                                  :white-space "nowrap"
;;                                                  :left (+ 5 x) ;(if hovered? (- x xx) x)
;;                                                  :top (- y 23)}
;;                                          :align :center
;;                                          :justify :start
;;                                          :child (str bid)]

;;                                         (when done-block?
;;                                           [re-com/box
;;                                            :size "auto"
;;                                            :width (px (- w 5))
;;                                            :style {:color bcolor
;;                                                    :position "fixed"
;;                                                    :padding-right "6px"
;;                                                    :left (+ 5 x) ;(if hovered? (- x xx) x)
;;                                                    :top (+ y h)}
;;                                            :align :center
;;                                            :justify :end
;;                                            :child ":end-step"])

;;                                         (when (and (not @dragging-port?))

;;                                           [re-com/v-box
;;                                           ;:width (px 80)
;;                                            :width (px longesti-w)
;;                                            :size "none"
;;                                            :height (px (- h 8))
;;                                            :align :start :justify :start
;;                                            :style {:z-index 505
;;                                                    :margin-top "-1px"
;;                                                    :margin-left "-5px"}
;;                                            :children (conj
;;                                                       (conj in-pills
;;                                                             (when (not too-tall?)
;;                                                               [re-com/box
;;                                                                :width (px 80)
;;                                                                :attr {:on-mouse-down mouse-down-fn}
;;                                                                :size "1" :align :center :justify :center
;;                                                                :child " " :style {;:border "0px solid cyan"
;;                                                                                   :cursor (if @dragging? "grabbing" "grab")}]))
;;                                                       (when false ;(not (<= longest-w 100))
;;                                                         [re-com/v-box
;;                                                          :style {:margin-top "15px" :margin-left "10px"}
;;                                                          :height "30px"
;;                                                          :align :start :justify :center
;;                                                          :children [(when value [re-com/box
;;                                                                                  :style {:font-size "10px"}
;;                                                                                  :child (ut/truncate-string (str value) 30)])
;;                                                                     (when true ;(or (= ttype :open-block) (= ttype :param))
;;                                                                       [re-com/box
;;                                                                        :style {:opacity 0.33 :font-size "6px"}
;;                                                                        :child (or (get-in ports [:out :out])
;;                                                                                   (when (get-in ports [:out :*]) :map) "?")])]]))])

;;                                         (if @dragging-port?
;;                                           (let [filtered-inputs (filter #(or (try (some (fn [x] (= x (last @dragged-port))) (val %)) (catch :default _ false))
;;                                                                              (= (val %)
;;                                                                                 (try (last @dragged-port) ;; in case of nil, etc
;;                                                                                      (catch :default _ nil)))
;;                                                                              (= (val %) :any))
;;                                                                         inputs)
;;                                                 opts-cnt (count filtered-inputs)
;;                                                 opts-cnt (if (odd? opts-cnt) (+ 1 opts-cnt) opts-cnt)
;;                                                 single? (= opts-cnt 1)]

;;                                             [re-com/v-box
;;                                              :size "auto"
;;                                              :width (px (- w 8))
;;                                              :height (px (- h 10))
;;                                              :children (for [ins (partition-all 2 filtered-inputs)
;;                                                              :let [h (- h 12)
;;                                                                    w (- w 13)
;;                                                                    row-height (if single? h (/ h (/ opts-cnt 2)))]]
;;                                                          [re-com/h-box
;;                                                           :size "none"
;;                                                           :height (px row-height)
;;                                                           :width (px (- w 8))
;;                                                           :children (for [i ins
;;                                                                           :let [ddcolor (get (theme-pull :theme/data-colors db/data-colors) (gn (val i))
;;                                                                                              (get (theme-pull :theme/data-colors db/data-colors) "unknown" "#FFA500"))
;;                                                                                 hovered? (= @flow-drop-hover [bid (key i)])
;;                                                                                 pid (key i)
;;                                                                                 droppable-port (fn [element] [(reagent/adapt-react-class rdnd/Droppable)
;;                                                                                                               {:types   [:flow-port]
;;                                                                                                                :on-drop #(let [[srcf srcl _] @dragged-port
;;                                                                                                                                src (if (= srcl :out) srcf ;; no alias for simple */out
;;                                                                                                                                        (keyword (str (gn srcf) "/" (gn srcl))))
;;                                                                                                                                dest (if (= pid :in) bid
;;                                                                                                                                         (keyword (str (gn bid) "/" (gn pid))))]
;;                                                                                                                            (reset! dragging-port? false)
;;                                                                                                                            (tap> [:dropped src :to dest])
;;                                                                                                                            (connect-ports src dest))}
;;                                                                                                               [re-com/box
;;                                                                                                                :align :center :justify :center
;;                                                                                                                :height (px row-height)
;;                                                                                                                :width (px (/ w (count ins)))
;;                                                                                                                :size "none"
;;                                                                                                                :child element]])]]
;;                                                                       [re-com/box
;;                                                                        :size "none"
;;                                                                        :style {:color ddcolor
;;                                                                                :background-color (if hovered? (str ddcolor 33) "inherit")
;;                                                                                :border (str "2px solid " ddcolor)}
;;                                                                        :align :center :justify :center
;;                                                                        :child [droppable-port (str (key i))]])])])


;;                                           (let [valuestr (if (number? value) (ut/nf value) value)
;;                                                 tstr (ut/truncate-string (str valuestr) (* (/ cw 5) (apply max [(count inputs) (count outputs)])))
;;                                                 slen (count tstr)
;;                                                 hex? (ut/is-hex-color? valuestr)
;;                                                ;slen (if (< slen 9) 9 slen)
;;                                                 fsize (if (< slen 45) (auto-font-size-px tstr h cw) 10)
;;                                                 fsize (if (> fsize 55) 55 fsize)]
;;                                             [re-com/v-box
;;                                              :size "none"
;;                                              :width (px cw)
;;                                              :attr {:on-mouse-down mouse-down-fn}
;;                                              :style {:color (if hex? (ut/invert-hex-color valuestr) bcolor)
;;                                                      :background-color (if hex? valuestr "inherit")
;;                                                      :border-radius (when hex? "14px")
;;                                                      :cursor (if @dragging? "grabbing" "grab")}
;;                                              :align :center :justify :center
;;                                              :children (if true ; (<= longesti-w 100)
;;                                                          [(when value [re-com/box
;;                                                                        :width (px cw) :size "none"
;;                                                                        :height (px (- h 30))
;;                                                                       ;:align :center
;;                                                                        :justify :center
;;                                                                        :style {:font-size (if hex? "38px" (px fsize))
;;                                                                               ;:font-size "10px"
;;                                                                             ;:border "1px solid white"
;;                                                                                :overflow "hidden"
;;                                                                                :font-weight 700
;;                                                                                :overflow-wrap "break-word"
;;                                                                                :word-wrap "break-word"}
;;                                                                        :child (str tstr)])
;;                                                           (when true ;(or (= ttype :open-block) (= ttype :param))
;;                                                             [re-com/box
;;                                                              :style {:opacity 0.33 :font-size "8px"}
;;                                                              :child (if hex?
;;                                                                       "css hex color string"
;;                                                                       (or (keyword (ut/data-typer value))
;;                                                                           (get-in ports [:out :out])
;;                                                                           (when (get-in ports [:out :*]) :map) "?"))])]
;;                                                          [])]))



;;                                         [re-com/v-box
;;                                          :size "none"
;;                                          :style {:z-index 505
;;                                                  :margin-top "-1px"
;;                                                  :margin-left "2px"
;;                                                  :display (if (not @dragging-port?) "inherit" "none")}
;;                                          :children (conj out-pills
;;                                                          [re-com/box
;;                                                           :attr {:on-mouse-down mouse-down-fn}
;;                                                           :size "1" :align :center :justify :center
;;                                                           :child " " :style {:cursor (if @dragging? "grabbing" "grab")}])]]])])

;;                 (when (cstr/includes? (str flow-id) "/")
;;                   [re-com/box
;;                    :style {:position "fixed"
;;                            :left (- bounds-x 50)
;;                            :top (- bounds-y 30)
;;                            :width (px (+ (- bounds-x2 bounds-x) 150))
;;                            :height (px (+ (- bounds-y2 bounds-y) 100))
;;                            :border-radius "14px"
;;                            :font-size "10px"
;;                            :border (str "6px solid " done-data-type-color)
;;                            :z-index 500}
;;                    :padding "5px"
;;                    :child "read-only sub-flow"]))]))


(defonce hide-menu-panel? (reagent/atom false))

(defn menu-panel []
  (let [menu-pills ["thiessen" "missles" "hope" "cope" "rope"]]
    [re-com/h-box
     :children [(when (not @hide-menu-panel?)
                  [re-com/v-box
                   :style {:position "fixed" :left 0 :top 35
                           :z-index 700}
            ;:border "1px solid yellow"

                   :gap "5px"
                   :width "110px"
                   :children (for [p menu-pills]
                               ^{:key (str "pills" p)}
                               [re-com/box
                                :padding "5px"
                                :child (str p)
                                :style {:background-color "#000000"
                                        :border-top "2px solid darkcyan"
                                        :border-right "2px solid darkcyan"
                                        :border-bottom "2px solid darkcyan"
                                        :font-size "16px"
                                        :font-weight 700
                                        :z-index 770
                                        :border-radius "0px 12px 12px 0px"
                                        :color "cyan"}])])
                [re-com/box
                 :style {:color "#ffffff" :cursor "pointer"
                         :position "fixed" :left (if @hide-menu-panel? 15 125) :top 300}
                 :attr {:on-click #(reset! hide-menu-panel? (not @hide-menu-panel?))}
                 :child (if @hide-menu-panel? ">" "<")]]]))

(defn safe-cpk [k]
  (-> (str k)
      (ut/replacer #":" "")
      (ut/replacer #"/" ">")
      keyword))

(defn unsafe-cpk [k]
  (-> (str k)
      (ut/replacer #":" "")
      (ut/replacer #">" "/")
      keyword))

;(tap> [:safe-cpk (safe-cpk :theme/panel-1) (unsafe-cpk :theme>panel-1)])

(defn get-flow-deps [panel-key body]
  (let [all-sql-call-keys      @(re-frame/subscribe [::bricks/all-sql-call-keys])
        sql-aliases-used       @(re-frame/subscribe [::bricks/panel-sql-aliases-in-views-body panel-key body])
        valid-body-params      (vec (ut/deep-flatten @(re-frame/subscribe [::bricks/valid-body-params-in body])))
        possible-datasets-used (set (for [e valid-body-params] (keyword (nth (cstr/split (ut/unkeyword e) #"/") 0))))
        used-datasets          (vec (cset/union (set sql-aliases-used) (cset/intersection possible-datasets-used (set all-sql-call-keys))))
        value-walks-targets    [] ;(filter (fn [x] (and (cstr/includes? (str x) ".") (not (cstr/includes? (str %) ".*")))) valid-body-params)
        condi-walks-targets    [] ;(distinct (filter (fn [x] (cstr/includes? (str x) "condi/")) valid-body-params))
                                       ;; ^^ we arent actually *looking these up*, so they SHOULD be caught by valid-body-params-in since they are compound keys
        all-params             (into (into (into valid-body-params used-datasets) value-walks-targets) condi-walks-targets)
        all-params             (vec (filter (fn [x] (and (not (cstr/starts-with? (str x) ":theme/"))
                                                         (not (cstr/starts-with? (str x) ":query/")))) all-params))
        params-map             (into {} (for [k all-params] {(str k) k}))
        res-params-map         (resolver/logic-and-params params-map nil)
        res-params-map-types   (merge
                                (into {} (for [[k v] res-params-map]
                                           (try {(edn/read-string k) (keyword (ut/data-typer v))}
                                                (catch :default _ nil))))
                                (into {} (for [u used-datasets] {u :rabbitsql})))
        res-params-map-types   (into {} (for [[k v] res-params-map-types] {(safe-cpk k) v}))]
    res-params-map-types))

(re-frame/reg-sub
 ::sub-flow-loaded?
 (fn [db [_]]
   (get-in db [:sub-flow-incoming :file-path])))

(re-frame/reg-sub
 ::sub-flow-incoming
 (fn [db [_]]
   (get db :sub-flow-incoming)))

(def last-loaded (atom nil))

(defn flow-droppable [types-vec root element]
 ;(tap> [:dropped?  @port-hover types-vec root @bricks/dragging-body element ])
  (if true ;(not @dragging-port?)                                                  ; @dragging?
    [(reagent/adapt-react-class rdnd/Droppable)
     {:types   types-vec
     ;:on-drag #(tag-screen-position %)
      :on-drag-enter #(when (or (and (= (get-in @bricks/dragging-body [:drag-meta :source-query]) :flows-sys)
                                     @drop-toggle?)
                                (not (empty? (get-in @bricks/dragging-body [:flow-item :file-path]))))
                        (let [inc-path (if (not (empty? (get-in @bricks/dragging-body [:flow-item :file-path])))
                                         (get-in @bricks/dragging-body [:flow-item :file-path])
                                         @(re-frame/subscribe [::conn/clicked-parameter-key [:flows-sys/file_path]]))
                              inc-loaded @(re-frame/subscribe [::sub-flow-loaded?])]
                          (when (and (not (= inc-path inc-loaded)) (not (= inc-path @last-loaded)))
                            (reset! last-loaded inc-path)
                            (tap> [:loading-sub-flow! inc-path inc-loaded])
                            (re-frame/dispatch [::http/load-sub-flow inc-path]))))
      :on-drop #(when (or (empty? @port-hover) (nil? @port-hover)) ;; is not a port drop and intended to be a canvas drop
                  ;(do
                  (tap> [:dropped? @port-hover @dragged-port types-vec root @bricks/dragging-body element])
                  (if (and (= (get-in @bricks/dragging-body [:drag-meta :source-query]) :flows-sys) (not @drop-toggle?))
                    (let [file-path @(re-frame/subscribe [::conn/clicked-parameter-key [:flows-sys/file_path]])]
                      (re-frame/dispatch [::http/load-flow file-path])
                      (tap> [:load-flow file-path])
                    ;(center-zoom-and-pan)
                      (center-to-saved-coords))

                    (let [incoming   (js->clj %)
                     ;_ (tap> [:start? types-vec (last (last incoming)) (map? (last (last incoming))) (try (read-string (last (last incoming))) (catch :default e (str e)))])
                            ;data       (read-string (get incoming "meta-menu"))  ;(read-string (last (last incoming)))
                          _ (tap> [:incoming? incoming (first incoming) (get (first incoming) "meta-menu") (get incoming "meta-menu") (read-string (last (last incoming)))])
                          data         (read-string (get incoming "meta-menu"))
                          cdata        (read-string (get incoming "flow-port"))
                          from-port?   (not (empty? cdata))
                          data         (if from-port? cdata data)

                          browser-pull? (or (get data :browser-pull?) (get cdata :browser-pull?))

                          _ (tap> [:incoming-data data])
                          [x y]      root
                            ;port-drop?     (get incoming "flow-port")
                          sub-flow-part? (not (empty? (get-in @bricks/dragging-body [:flow-item :file-path])))
                          sub-flow-drop? (and ;(not from-port?)
                                          (or (and (= (get-in @bricks/dragging-body [:drag-meta :source-query]) :flows-sys) @drop-toggle?) sub-flow-part?))
                          file-path (when sub-flow-drop? (if sub-flow-part?
                                                           (get-in @bricks/dragging-body [:flow-item :file-path])
                                                           @(re-frame/subscribe [::conn/clicked-parameter-key [:flows-sys/file_path]])))
                          _ (tap> [:dropper file-path sub-flow-part? sub-flow-drop?])
                          sub-flow (when sub-flow-drop? @(re-frame/subscribe [::sub-flow-incoming]))
                          _ (when sub-flow-drop? (tap> [:sub-flow-drop! (get sub-flow :file-path)]))
                          zoom-multi (get @db/pan-zoom-offsets 2)
                          zoom-offset-x (get @db/pan-zoom-offsets 0)
                          zoom-offset-y (get @db/pan-zoom-offsets 1)
                          drop-x      (- (/ x zoom-multi) (/ zoom-offset-x zoom-multi))
                          drop-y      (- (/ y zoom-multi) (/ zoom-offset-y zoom-multi))
                     ;type        (first (first incoming))
                          rooted-data (assoc data :root root)
                          kp (get-in rooted-data [:drag-meta :keypath])
                          port-meta (get-in rooted-data [:drag-meta :port-meta])
                          port-meta? (and (not (empty? port-meta)) (map? port-meta))
                          get-in-drag? (and (vector? kp) (not (empty? kp)))
                          dm-type     (get-in rooted-data [:drag-meta :type])
                          _ (tap> [:dropeed-offsets root dm-type zoom-multi zoom-offset-x zoom-offset-y drop-x drop-y rooted-data])
                          flow-item (get @bricks/dragging-body :flow-item)
                          flow-body (cond
                                        ;; from-port? {:w 125 :h 60
                                        ;;             :x drop-x :y drop-y :z 0
                                        ;;             :data (merge @bricks/dragging-body {:flow-item {:types {:in :any :out :any}}})
                                        ;;             :icon "zmdi-puzzle-piece"
                                        ;;             :ports {:in {:in :any}
                                        ;;                     :out {:out :any}}}
                                      port-meta? (let []
                                                   {:w            125
                                                    :h            60
                                                    :z            0
                                                    :data         {:drag-meta  {:type :open-block}
                                                                   :flow-item  {:expandable? true
                                                                                :meta {:* (get port-meta :meta)}}
                                                                   :user-input (get port-meta :default)}
                                                      ;;:right-click? true ;; only for right-click positioning
                                                    :ports        {:in {} :out {:out :any}}
                                                    :x drop-x :y drop-y})
                                      from-port? (let [;kp (get-in rooted-data [:drag-meta :keypath])
                                                       canned-fn (if (vector? kp)
                                                                   '(fn [x] (get-in x :kp))
                                                                   '(fn [x] x))
                                                       canned-fn (walk/postwalk-replace {:kp kp} canned-fn)]
                                                   {:fn canned-fn ;'(fn [x] x)
                                                    :w 125
                                                    :raw-fn canned-fn ;'(fn [x] x)
                                                    ;:right-click? true
                                                    :icon "zmdi-functions"
                                                    :z 0
                                                    :ports {:in {:x :any} :out {:out :any}}
                                                    :h 60
                                                    :x drop-x :y drop-y
                                                    :data {:flow-item {:category ":rabbit-base"
                                                                       :fn canned-fn ;'(fn [x] x)
                                                                       :name ":open-fn"
                                                                       :raw-fn canned-fn ;'(fn [x] x)
                                                                       :type :open-fn
                                                                       :icon "zmdi-functions"
                                                                       :types {:x :any :out :any}
                                                                       :expandable? true
                                                                       :drag-meta {:type :open-fn}}}})
                                      sub-flow-drop?
                                      (let [;poss-inputs (vec (for [[k v] (get sub-flow :map)]
                                         ;                   (for [i (get-in v [:ports :in])]
                                         ;                     (keyword (str (ut/unkeyword k) "/" (ut/unkeyword i))))))
                                            blocks     (set (keys (get sub-flow :map)))
                                            base-conns (set (for [[_ v2] (get sub-flow :connections)] (keyword (gns v2))))
                                            no-inputs (cset/difference blocks base-conns)
                                            flow-inputs (into {}
                                                              (for [i no-inputs]
                                                                {i
                                                                 (get-in sub-flow [:map i :ports :out (first (keys (get-in sub-flow [:map i :ports :out])))])}))
                                            done-block (try (first (first (filter (fn [x] (= (second x) :done)) (get sub-flow :connections))))
                                                            (catch :default _ :error))]
                                        (tap> [:sub-flow-build no-inputs base-conns])
                                        {:w 200 :h 100
                                         :x drop-x :y drop-y :z 0
                                         :data (-> rooted-data
                                                   (assoc-in [:drag-meta :done-block] done-block)
                                                   (assoc-in [:drag-meta :type] :sub-flow))
                                         :sub-flow sub-flow
                                         :file-path file-path
                                         :flow-id (get sub-flow :flow-id)
                                         :icon "zmdi-puzzle-piece"
                                         :ports {:in flow-inputs ;(dissoc (get flow-item :types) :out)
                                                 :out (get-in sub-flow [:map done-block :ports :out])}}) ;(select-keys (get flow-item :types) [:out])


                                      flow-item ;; item picked from grid rows
                                      (let []
                                        (tap> [:flow-item flow-item])
                                        (merge
                                         {:w 200 :h 100
                                          :x drop-x :y drop-y :z 0
                                          :data (if (get flow-item :type)
                                                  (assoc-in rooted-data [:drag-meta :type] (get flow-item :type))
                                                  rooted-data)
                                          :icon (get flow-item :icon "zmdi-pizza")
                                          :ports {:in (dissoc (get flow-item :types) :out)
                                                  :out (select-keys (get flow-item :types) [:out])}}
                                         (if (= (get flow-item :type) :open-fn) {:raw-fn (get flow-item :fn)} {})))

                                      (= dm-type :view) ;; prarm dragged from editor bar
                                      (let [view-ref               (keyword
                                                                    (str "view/" (gn (get-in rooted-data [:drag-meta :source-panel-key])) "."
                                                                         (gn (get-in rooted-data [:drag-meta :source-table]))))
                                            panel-key              (get-in rooted-data [:drag-meta :source-panel-key])
                                            body                   @(re-frame/subscribe [::bricks/resolve-view-alias view-ref])
                                            res-params-map-types (get-flow-deps panel-key body)]
                                   ;(tap> [:view view-ref body all-params res-params-map res-params-map-types])
                                        {:w 200 :h 100
                                         :x drop-x :y drop-y :z 0
                                         :data (-> @bricks/dragging-body
                                                   (dissoc :queries)
                                                   (assoc-in [:drag-meta :body] body)
                                                   (assoc-in [:drag-meta :keypath] [(get-in rooted-data [:drag-meta :source-panel-key])
                                                                                    :views (get-in rooted-data [:drag-meta :source-table])])
                                                   (assoc-in [:drag-meta :param-full] view-ref))
                                         :icon "zmdi-chart"
                                         :ports {:in res-params-map-types
                                                 :out {:out :rabbit-code}}})

                                      (= dm-type :param) ;; prarm dragged from editor bar
                                      {:w 200 :h 90
                                       :x drop-x :y drop-y :z 0
                                       :data @bricks/dragging-body
                                       :icon "zmdi-tune"
                                       :ports {:in {}
                                               :out {:out (keyword (ut/data-typer
                                                                    @(re-frame/subscribe
                                                                      [::conn/clicked-parameter-key
                                                                       [(get-in rooted-data [:drag-meta :param-full])]])))}}}

                                      (and (= dm-type :where) ;; cell value pull from query grid (acts as a parameter here)
                                           (get-in rooted-data [:drag-meta :row-num]))
                                      (let [cell-ref (keyword
                                                      (str (gn (get-in rooted-data [:drag-meta :param-table])) "/"
                                                           (gn (get-in rooted-data [:drag-meta :param-field])) "."
                                                           (get-in rooted-data [:drag-meta :row-num])))]
                                   ;(tap> [:hit? cell-ref (resolver/logic-and-params [cell-ref] nil)])
                                        {:w 200 :h 60
                                         :x drop-x :y drop-y :z 0
                                         :data (-> @bricks/dragging-body
                                                   (dissoc :queries)
                                                   (assoc-in [:drag-meta :type] :cell)
                                                   (assoc-in [:drag-meta :param-full] cell-ref))
                                         :icon "zmdi-grid"
                                         :ports {:in {}
                                                 :out {:out (keyword (ut/data-typer
                                                                 ;@(re-frame/subscribe [::conn/clicked-parameter-key [cell-ref]])
                                                                      (first (resolver/logic-and-params [cell-ref] nil))))}}})

                                      (= dm-type :query)
                                      (let [panel-key              (get-in rooted-data [:drag-meta :source-panel-key])
                                            qid                    (get-in rooted-data [:drag-meta :source-table])
                                            body                   @(re-frame/subscribe [::bricks/find-query qid])
                                            res-params-map-types   (get-flow-deps panel-key body)]
                                        (tap> [:qbody body])
                                        {:w 200 :h 60
                                         :x drop-x :y drop-y :z 0
                                         :data @bricks/dragging-body
                                         :icon "zmdi-shape" ;(get flow-item :icon "zmdi-dns")
                                         :ports {:in res-params-map-types
                                                 :out {:out :rabbitsql}}})

                                      :else {:w 200 :h 50
                                             :x drop-x :y drop-y :z 0
                                             :data @bricks/dragging-body
                                             :icon "zmdi-pizza"
                                             :ports {:in {}
                                                     :out {}}})
                          flow-id (cond
                                    sub-flow-drop? (ut/safe-key
                                                    (try (edn/read-string (get sub-flow :flow-id)) (catch :default _ :name-issue!))
                                                 ;(vec (keys @(re-frame/subscribe [::flowmap])))
                                                    @(re-frame/subscribe [::reserved-type-keywords]))

                                    flow-item (ut/safe-key
                                               (try (edn/read-string (get flow-item :name)) (catch :default _ :name-issue!))
                                            ;(vec (keys @(re-frame/subscribe [::flowmap])))
                                               @(re-frame/subscribe [::reserved-type-keywords]))

                                    :else (or (get-in rooted-data [:drag-meta :source-table])
                                              (get-in rooted-data [:drag-meta :param-full])))
                          safe-keys-reserved @(re-frame/subscribe [::reserved-type-keywords])
                          src-block (cstr/replace (str (get-in rooted-data [:drag-meta :src-bid])) ":" "")
                          src-port (cstr/replace (str (get-in rooted-data [:drag-meta :src-pid])) ":" "")
                          check-port-drop (cond
                                            port-meta? (ut/safe-key (keyword (str "<" src-port)) safe-keys-reserved)
                                            get-in-drag? ;; use the get-in keypath to name
                                            (let [;kp-str (cstr/replace (str (apply (comp merge str) kp)) ":" ">")
                                                  kp-str (cstr/join ">" kp)
                                                  kp-str (-> kp-str (cstr/replace ":" "") (cstr/replace " " "") (cstr/replace "'" ""))]
                                              (ut/safe-key (keyword (str src-block "_" kp-str)) safe-keys-reserved))
                                            :else (ut/safe-key (keyword (str src-block "_" src-port)) safe-keys-reserved))]

                     ;new-key (str "block-" (rand-int 12345))
                     ;new-keyw (keyword new-key)
                   ;;   ok-new?     (or
                   ;;                (and (not @on-block?)
                   ;;                     (or (false? @dyn-dropper-hover)
                   ;;                         (nil? @dyn-dropper-hover)))
                   ;;                (= dm-type :viz-reco))


                 ;(tap> [:dropped type :ok-new? :drag-meta-type dm-type [@on-block? @dyn-dropper-hover] ok-new? rooted-data root])
                      (reset! dragging? false)
                      (reset! bricks/dragging? false)
                 ;(reset! bricks/over-block? false)
                      (cond
                        false "hey"

                   ; (get rooted-data :file_path) ;; (get @(re-frame/subscribe [::conn/clicked-parameter [:files-sys]]) :file_path)
                        :else (do ;;(tap> [:flow-inherits-drag-body? @bricks/dragging-body data drop-x drop-y flow-body flow-id check-port-drop])
                                (add-block drop-x drop-y flow-body (if from-port? check-port-drop flow-id) browser-pull?)
                                (when from-port?
                                  (if port-meta? ;; in to out
                                    (connect-ports ;(keyword (if (= src-port "out") src-block (str src-block "/" src-port)))
                                     check-port-drop ;(keyword (str (cstr/replace (str check-port-drop) ":" "")))
                                     (keyword (if (= src-port "out") src-block (str src-block "/" src-port))))
                                    (connect-ports (keyword (if (= src-port "out") src-block (str src-block "/" src-port)))
                                                   (keyword (str (cstr/replace (str check-port-drop) ":" "") "/x")))))))))

                                    ;(do (tap> [:port-drop-on-port :do-nothing (get (js->clj %"flow-port"))]) nil)
                  )}

     [re-com/box :child element]] element))

(def scroll-id (reagent/atom "1"))
;(def start (reagent/atom nil))

;; (defn smooth-scroll-to-element [container-id element-id]
;;   (let [container (gdom/getElement container-id)
;;         element (gdom/getElement element-id)
;;         container-left (.-scrollLeft container)
;;         element-left (- (.-offsetLeft element) 40) ;; 40 is the width of the sidebar, so offset
;;         start (atom nil)
;;         duration 500]
;;     (letfn [(step [timestamp]
;;               (when (nil? @start) (reset! start timestamp))
;;               (let [progress (/ (- timestamp @start) duration)
;;                     new-scroll-left (+ container-left (* progress (- element-left container-left)))]
;;                 (set! (.-scrollLeft container) new-scroll-left)
;;                 (when (< progress 1)
;;                   (.requestAnimationFrame js/window step))))]
;;       (.requestAnimationFrame js/window step))))


;;@(re-frame/subscribe [::bricks/flow-editor?])

(def current-index (atom 0))
(def scrolly-pos (reagent/atom nil))

(defn handle-wheel [event]
  (let [direction (if (> (.-deltaY event) 0) 1 -1)
        items (vec (keys @(re-frame/subscribe [::flowmap])))
       ;curr-value @flow-hover
        new-index (max 0 (min (count items) (+ @current-index direction)))]
    (tap> [:scrolled-to new-index])
    (reset! current-index new-index)
    (reset! flow-hover (get items new-index))))

(defn add-wheel-listener [element-id]
  (let [element (gdom/getElement element-id)]
    (tap> [:add-wheel-listener])
    (.removeEventListener element "wheel" handle-wheel)
    (.addEventListener element "wheel" handle-wheel)))

(defn remove-wheel-listener [element-id]
  (let [element (gdom/getElement element-id)]
    (tap> [:REMOVE-wheel-listener])
    (.removeEventListener element "wheel" handle-wheel)))

;;(defonce db/flow-browser-panel? (reagent/atom true))

(defn smooth-scroll-to-element [container-id element-id]
  (let [container (gdom/getElement container-id)
        element (gdom/getElement element-id)
        container-left (.-scrollLeft container)
       ;element-left (.-offsetLeft element)
        element-left (- (.-offsetLeft element) (if @(re-frame/subscribe [::bricks/flow-editor?]) 640 40))]
        ;; 40 is the width of the sidebar, so offset

    (when true ;(not (= @flow-select @scrolly-pos))
      (let [start (atom nil)
            duration 500]
        (letfn [(step [timestamp]
                  (when (nil? @start) (reset! start timestamp))
                  (let [progress (/ (- timestamp @start) duration)
                        flow-select @(re-frame/subscribe [::selected-flow-block])
                        new-scroll-left (+ container-left (* progress (- element-left container-left)))]
                   ;(tap> [:scroll-assholes container-left element-left (Math/floor new-scroll-left)])
                    (reset! scrolly-pos flow-select)
                    (set! (.-scrollLeft container) new-scroll-left)
                    (when (< progress 1)
                      (.requestAnimationFrame js/window step))))]
          (.requestAnimationFrame js/window step))))))

(defn scroll-to-element [container-id element-id]
  (let [container (gdom/getElement container-id)
        element (gdom/getElement element-id)
        container-left (.-scrollLeft container)
       ;element-left (.-offsetLeft element)
        element-left (- (.-offsetLeft element) (if @(re-frame/subscribe [::bricks/flow-editor?]) 610 10))]
    (set! (.-scrollLeft container) (- element-left container-left))))

(defn code-box-rw [width-int height-int value bid]
  (let [];sql-hint? (cstr/includes? (str value) ":::sql-string")
       ;kp        [:panels panel-id :queries query-key :select idx]
    [re-com/box
     :size "none"
     :width (px (- width-int 24))
     :height (px (- height-int 24))
     :style {:font-family   (theme-pull :theme/monospaced-font nil) ; "Chivo Mono" ;"Fira Code"
             :font-size     "14px"
             :overflow      "auto"
             :border-radius "12px"
             :font-weight   700}
     :child [(reagent/adapt-react-class cm/UnControlled)
             {:value   (ut/format-map (- width-int 24)
                                      (str value))
             ;:onBlur  #(re-frame/dispatch-sync [::update-selected-field kp (read-string (cstr/join " " (ut/cm-deep-values %)))])
             ;:onBlur  #(try (swap! flowmaps assoc bid (read-string (cstr/join " " (ut/cm-deep-values %)))) (catch :default _ (swap! flowmaps assoc bid value)))
              :onBlur #(try
                         (let [new-vals (read-string (cstr/join " " (ut/cm-deep-values %)))]
                           (re-frame/dispatch [::update-flowmap-key bid nil new-vals]))
                         (catch :default _ (re-frame/dispatch [::update-flowmap-key bid nil value])))
              :options {:mode              "clojure"
                        :lineWrapping      true
                        :lineNumbers       true
                        :matchBrackets     true
                        :autoCloseBrackets true
                        :autofocus         false
                        :autoScroll        false
                        :detach            true
                        :readOnly          false
                        :theme             (theme-pull :theme/codemirror-theme nil)}}]]))

(defn code-box-channels [width-int height-int value flow-id bid]
  (let [];sql-hint? (cstr/includes? (str value) ":::sql-string")
       ;kp        [:panels panel-id :queries query-key :select idx]
    [re-com/box
     :size "none"
     :width (px (- width-int 24))
     :height (px (- height-int 24))
     :style {:font-family   (theme-pull :theme/monospaced-font nil) ; "Chivo Mono" ;"Fira Code"
             :font-size     "14px"
             :overflow      "auto"
             :border-radius "12px"
             :font-weight   700}
     :child [(reagent/adapt-react-class cm/UnControlled)
             {:value   (ut/format-map (- width-int 24)
                                      (str value))
             ;:onBlur  #(re-frame/dispatch-sync [::update-selected-field kp (read-string (cstr/join " " (ut/cm-deep-values %)))])
             ;:onBlur  #(try (swap! flowmaps assoc bid (read-string (cstr/join " " (ut/cm-deep-values %)))) (catch :default _ (swap! flowmaps assoc bid value)))
              :onBlur #(try
                         (let [new-vals (read-string (cstr/join " " (ut/cm-deep-values %)))]
                           (swap! channel-holster assoc-in [flow-id bid :value] new-vals))
                         (catch :default _ (tap> [:error-saving-channel-data flow-id bid])))
              :options {:mode              "clojure"
                        :lineWrapping      true
                        :lineNumbers       true
                        :matchBrackets     true
                        :autoCloseBrackets true
                        :autofocus         false
                        :autoScroll        false
                        :detach            true
                        :readOnly          false
                        :theme             (theme-pull :theme/codemirror-theme nil)}}]]))

(defn code-box-rwc [width-int height-int value bid]
  (let []
    [re-com/box
     :size "none"
     :width (px (- width-int 24))
     :height (px (- height-int 24))
     :style {:font-family   (theme-pull :theme/monospaced-font nil) ; "Chivo Mono" ;"Fira Code"
             :font-size     "14px"
             :overflow      "auto"
             :border-radius "12px"
             :font-weight   700}
     :child [(reagent/adapt-react-class cm/UnControlled)
             {:value   (ut/format-map (- width-int 24)
                                      (str value))
             ;:onBlur  #(re-frame/dispatch-sync [::update-selected-field kp (read-string (cstr/join " " (ut/cm-deep-values %)))])
             ;:onBlur  #(try (swap! flowmaps assoc bid (read-string (cstr/join " " (ut/cm-deep-values %)))) (catch :default _ (swap! flowmaps assoc bid value)))
              :onBlur #(try
                         (let [new-vals (read-string (cstr/join " " (ut/cm-deep-values %)))]
                           (re-frame/dispatch [::update-connections new-vals]))
                         (catch :default _ (re-frame/dispatch [::update-flowmap-key bid nil value])))
              :options {:mode              "clojure"
                        :lineWrapping      true
                        :lineNumbers       true
                        :matchBrackets     true
                        :autoCloseBrackets true
                        :autofocus         false
                        :autoScroll        false
                        :detach            true
                        :readOnly          false
                        :theme             (theme-pull :theme/codemirror-theme nil)}}]]))

(defn text-box-rw [width-int height-int value bid]
  (let [];sql-hint? (cstr/includes? (str value) ":::sql-string")
       ;kp        [:panels panel-id :queries query-key :select idx]
    [re-com/box
     :size "none"
     :width (px (- width-int 24))
     :height (px (- height-int 24))
     :style {:font-family   (theme-pull :theme/monospaced-font nil) ; "Chivo Mono" ;"Fira Code"
             :font-size     "11px"
             :overflow      "auto"
             :border-radius "12px"
             :font-weight   600}
     :child [(reagent/adapt-react-class cm/UnControlled)
             {:value   value
              :onBlur #(try
                         (let [new-vals (ut/cm-deep-values %)]
                           (re-frame/dispatch [::update-flowmap-key bid :description new-vals]))
                         (catch :default _ (tap> [:issue-saving bid :text-rw (ut/cm-deep-values %)])))
              :options {:mode              "text"
                        :lineWrapping      true
                        :lineNumbers       false ;true
                        :matchBrackets     true
                        :autoCloseBrackets true
                        :autofocus         false
                        :autoScroll        false
                        :detach            true
                        :readOnly          false
                        :theme             (theme-pull :theme/codemirror-theme nil)}}]]))

(defn code-box-fn [width-int height-int value bid]
  (let [value-keys (try
                     (if (string? value)
                       (try (filter keyword? (ut/deep-flatten (edn/read-string value)))
                            (catch :default _ []))
                       (filter keyword? (ut/deep-flatten value)))
                     (catch :default _ []))
        ;_ (tap> [:value value])
        opts @(re-frame/subscribe [::opts-map])
        uses (vec (cset/intersection (set value-keys) (set (keys opts))))
        using-opts? (not (empty? uses))]
    [re-com/v-box
     :size "none"
     :width (px (- width-int 24))
     :height (px (- height-int 24))
     :style {:font-family   (theme-pull :theme/monospaced-font nil) ; "Chivo Mono" ;"Fira Code"
             :font-size     "14px"
             :overflow      "auto"
             :border-radius "12px"
             :font-weight   700}
     :children [[(reagent/adapt-react-class cm/UnControlled)
                 {:value   (ut/format-map (- width-int 50)
                                          (str value))
                  :onBlur #(try
                             (let [new-vals (read-string (cstr/join " " (ut/cm-deep-values %)))
                                   fin (function-to-inputs new-vals)]
                              ;;  ports (try (second new-vals)
                              ;;             (catch :default _ (do (tap> [:error-making-arity-to-ports bid value new-vals]) [])))

                               (re-frame/dispatch [::update-flowmap-key-in bid [:ports :in] fin])
                               (re-frame/dispatch [::update-flowmap-key bid :raw-fn new-vals])
                               (re-frame/dispatch [::clean-connections]))
                             (catch :default e (do (tap> [:error-saving-fn! (str e)])
                                                   (re-frame/dispatch [::update-flowmap-key bid :raw-fn value]))))
                  :options {:mode              "clojure"
                            :lineWrapping      true
                            :lineNumbers       true
                            :matchBrackets     true
                            :autoCloseBrackets true
                            :autofocus         false
                            :autoScroll        false
                            :detach            true
                            :readOnly          false
                            :theme             (theme-pull :theme/codemirror-theme nil)}}]
                (when using-opts?
                  [re-com/box
                   :style {:opacity 0.5}
                   :child [code-box width-int nil (str ";; using flow opts:  "
                                                       ;(pr-str opts)
                                                       (pr-str (select-keys opts uses)))]])]]))

(defn code-box-condi [width-int height-int value bid name]
  (let [value-keys (filter keyword? (ut/deep-flatten value))
        opts @(re-frame/subscribe [::opts-map])
        uses (vec (cset/intersection (set value-keys) (set (keys opts))))
        using-opts? (not (empty? uses))]
    [re-com/v-box
     :size "none"
     :width (px (- width-int 24))
     :height (px (- height-int 24))
     :style {:font-family   (theme-pull :theme/monospaced-font nil) ; "Chivo Mono" ;"Fira Code"
             :font-size     "14px"
             :overflow      "auto"
             :border-radius "12px"
             :font-weight   700}
     :children [[(reagent/adapt-react-class cm/UnControlled)
                 {:value   (ut/format-map (- width-int 50)
                                          (str value))
                  :onBlur #(re-frame/dispatch [::edit-condi-port bid name (read-string (cstr/join " " (ut/cm-deep-values %)))])
                  :options {:mode              "clojure"
                            :lineWrapping      true
                            :lineNumbers       true
                            :matchBrackets     true
                            :autoCloseBrackets true
                            :autofocus         false
                            :autoScroll        false
                            :detach            true
                            :readOnly          false
                            :theme             (theme-pull :theme/codemirror-theme nil)}}]
                (when using-opts?
                  [re-com/box
                   :style {:opacity 0.5}
                   :child [code-box width-int nil (str ";; using flow opts:  "
                                                       ;(pr-str opts)
                                                       (pr-str (select-keys opts uses)))]])]]))

(defn code-box-view [width-int height-int fmap bid flow-id]
  (let [value (get fmap :view) ;; we want nil if not being used. '(fn [x] [:box :child (str x)]))
        value-keys (filter keyword? (ut/deep-flatten value))
        ;;bid @(re-frame/subscribe [::selected-flow-block]) ;; force rerender due to react "stickiness"
        bg-shade "#00000055"
        opts @(re-frame/subscribe [::opts-map])
        uses (vec (cset/intersection (set value-keys) (set (keys opts))))
        using-opts? (not (empty? uses))]
    [re-com/box
     :style {:border-radius "9px"
             :padding-top "8px"
             ;:padding-bottom "8px"
             :background-color bg-shade}
     :child
     [re-com/v-box
      :size "none"
      ;:width (px (- width-int 24))
      :height (px (- height-int 24))
      :style {:font-family   (theme-pull :theme/monospaced-font nil) ; "Chivo Mono" ;"Fira Code"
              :font-size     "14px"
              :overflow      "auto"
              :border-radius "12px"
              :font-weight   700}
      :children [[re-com/box
                  :width (px (- width-int 24))
                  :child [(reagent/adapt-react-class cm/UnControlled)
                          {:value   (ut/format-map (- width-int 50)
                                                   (str value))
                           :onBlur #(do (tap> [:edit-view-fn bid (cstr/join " " (ut/cm-deep-values %))
                                               (try (read-string (cstr/join " " (ut/cm-deep-values %)))
                                                    (catch :default e (str :error e)))])
                                        (re-frame/dispatch [::edit-view-fn bid (read-string (cstr/join " " (ut/cm-deep-values %)))]))
                           :options {:mode              "clojure"
                                     :lineWrapping      true
                                     :lineNumbers       true
                                     :matchBrackets     true
                                     :autoCloseBrackets true
                                     :autofocus         false
                                     :autoScroll        false
                                     :detach            true
                                     :readOnly          false
                                     :theme             (theme-pull :theme/codemirror-theme nil)}}]]

                 (when using-opts?
                   [re-com/box
                    :style {:opacity 0.5}
                    :child [code-box width-int nil (str ";; using flow opts:  "
                                                        (pr-str (select-keys opts uses)))]])

                 (let [condi-ports @(re-frame/subscribe [::push-ports bid])
                       selected (get-in @flow-details-block-container-atom [flow-id bid :port-panel :out])
                       cnt (count (keys condi-ports))
                       open? (true? (get-in @flow-details-block-container-atom [flow-id bid :push-paths :open?] false))]
                   [re-com/v-box
                    ;:width (px (- width-int 10))
                    :style {:background "#00000055"
                            :padding-top "8px"
                            :font-family (theme-pull :theme/base-font nil)
                            :font-weight 400
                            :font-size "12px"
                            :border-radius "8px"}
                    :children [[re-com/h-box
                                :size "auto"
                                :padding "4px"
                                :justify :between :align :center
                                :style {:color  (if open?
                                                  (str (theme-pull :theme/editor-outer-rim-color nil) 99)
                                                  (str (theme-pull :theme/editor-outer-rim-color nil) 45))
                                        :cursor "pointer"}
                                :children [[re-com/h-box
                                           ;:padding "3px"
                                            :align :center :justify :center
                                            :style {:padding-left "3px"}
                                            :gap "5px" ;:style {;:margin-left "-3px"
                                                      ;        ;:opacity (if (> cnt 0) 1 0.4)
                                                      ;        }
                                            :attr {:on-click #(swap! flow-details-block-container-atom assoc-in [flow-id bid :push-paths :open?] (not open?))}
                                            :children [[re-com/md-icon-button
                                                        :md-icon-name "fa-solid fa-carrot"
                                                        :style {:font-size "17px"
                                                                :cursor "pointer"}]
                                                       [re-com/box :child (str "push paths (" cnt ")")]]]
                                           [re-com/md-icon-button
                                            :on-click #(re-frame/dispatch [::add-push-port bid])
                                            :md-icon-name "zmdi-plus"
                                            :style {:font-size "22px"
                                                    :cursor "pointer"}]]]

                               (when (and open? (> cnt 0))
                                 [re-com/gap :size "8px"])

                               (when (and open? (> cnt 0))
                                 [re-com/v-box
                                  :style {:border-radius "8px"
                                          :background-color "#00000044"}
                                  :gap "8px"
                                  :padding "4px"
                                  :children (for [[k {:keys [dest]}] condi-ports
                                                  :let [select-vec [bid k]
                                                        valve-open? false ;@(re-frame/subscribe [::bricks/condi-valve-open? flow-id bid k])
                                                        selected? (or (and @sniffy-sniff (= @port-hover2 bid)
                                                                           (= k (get @sniffy-sniff 1)))
                                                                      (= select-vec selected))
                                                        is-first? (= k :push-path)
                                                        short-code (str ";; use for an on-click fn                      [" k "> \"*some-value*\"] or  "
                                                                        (when is-first? "  ")
                                                                        "          [" k ">] for passing %, on-change")
                                                        pcolor (theme-pull :theme/editor-outer-rim-color nil)]]
                                              [re-com/v-box
                                               :padding "4px"
                                               :attr {:on-mouse-enter #(do (swap! flow-details-block-container-atom assoc-in [flow-id bid :port-panel :out] select-vec))
                                                      :on-mouse-leave #(do (swap! flow-details-block-container-atom assoc-in [flow-id bid :port-panel :out] nil))}
                                               :style {:border (if (or selected? valve-open?)
                                                                 (str "1px solid " pcolor)
                                                                 (str "1px solid "  pcolor 25))
                                                       :background-color (if (or selected? valve-open?)
                                                                           (str pcolor "22") (str pcolor "08"))
                                                       :border-radius "6px"}
                                               :gap "8px"
                                               :children
                                               [[re-com/h-box
                                                 :size "auto"
                                                 :justify :between :align :center
                                                 :style {:color (str (theme-pull :theme/editor-outer-rim-color nil) 88)
                                                         :padding-left "4px"
                                                         :padding-right "1px"
                                                         :padding-top "4px"}
                                                 :children [[re-com/box
                                                             :style {:font-size "15px" :font-weight 700}
                                                             :child (str k)]
                                                            [re-com/h-box
                                                             :width "300px" :height "100%"
                                                             :justify :between :align :center
                                                             :children [[re-com/v-box :children (for [d dest] [re-com/box :child (str d)])]
                                                                        [re-com/md-icon-button :src (at)
                                                                         :md-icon-name "zmdi-close"
                                                                         :on-click #(re-frame/dispatch [::remove-push-port bid k])
                                                                         :style {:cursor "pointer"
                                                                                 :font-size "16px"}]]]]]
                                                [re-com/h-box
                                                 :justify :between :align :center
                                                 :children [;;[code-box-condi 500 nil fn bid k]
                                                            [code-box 500 nil short-code]
                                                          ;;  [re-com/box
                                                          ;;   :justify :center :align :center
                                                          ;;   :width "40px" :size "none" :height "100%"
                                                          ;;   :style {:color (theme-pull :theme/editor-outer-rim-color nil)
                                                          ;;           :background-color (str (theme-pull :theme/editor-outer-rim-color nil) 11)
                                                          ;;           :font-weight 700
                                                          ;;           :border-radius "8px"
                                                          ;;           :opacity (if valve-open? 1 0.4)}
                                                          ;;   :child (str valve-open?)]
                                                            ]]]])])]])]]]))

(defn code-box-rwo [width-int height-int value bid & [syntax]]
  (let [syntax (or (if (= syntax "raw (clojure)") "clojure" syntax) "clojure") ;; Error: true is not ISeqable
        stringify? (not (= syntax "clojure"))
        _ (tap> [:code-box-rwo bid syntax stringify? value])
        value (if (and stringify? (empty? value)) " " value) ;; just in case we get in a bad way with raw text edit...
        ]
        ;; value (if (and stringify?
        ;;                (not (string? value))
        ;;                (not (vector? value)))
        ;;         (vec (cstr/split (str value) "\n"))
        ;;         value)

    [rc/catch [re-com/box
               :size "none"
               :width (px (- width-int 24))
               :height (px (- height-int 24))
               :style {:font-family   (theme-pull :theme/monospaced-font nil) ; "Chivo Mono" ;"Fira Code"
                       :font-size     "14px"
                       :overflow      "auto"
                       :border-radius "12px"
                       :font-weight   700}
               :child [(reagent/adapt-react-class cm/UnControlled)
                       {:value   (if stringify?
                                   value
                         ;(str (cstr/join "\n" value))
                                   (ut/format-map (- width-int 24) (pr-str value)))
             ;:onBlur  #(re-frame/dispatch-sync [::update-selected-field kp (read-string (cstr/join " " (ut/cm-deep-values %)))])
                        :onBlur  #(if stringify?

                                    (let [ddata (ut/cm-deep-values %)
                                          inputs (ut/template-find (str ddata))
                                          _ (tap> [:code-box-rwo_template-find inputs])
                                          _ (tap> [:code-box-rwo_saving-string ddata inputs])
                                          input-map (into {} (for [e (range (count inputs))
                                                                   :let [name (keyword (cstr/replace (str (ut/unkeyword (get inputs e))) "/" "-"))]]
                                                               {name :any}))]

                                      (re-frame/dispatch [::update-flowmap-key-in bid [:ports :in] input-map])
                                      (re-frame/dispatch [::update-flowmap-key-in bid [:data :user-input] ddata])
                                      (re-frame/dispatch [::update-flowmap-key-in bid [:data :flow-item :inputs] (vec inputs)])
                                      (re-frame/dispatch [::update-flowmap-key-in bid [:ports :out] {:out :any}])
                                      (re-frame/dispatch [::clean-connections]))
                            ;(re-frame/dispatch [::update-flowmap-key bid nil full])


                                    (try (let [ddata (read-string (cstr/join " " (ut/cm-deep-values %)))
                                               dtype (ut/data-typer ddata)
                                               _ (tap> [:code-box-rwo_write-clj ddata dtype])
                                               ports {;:in (get-in v [:ports :in])
                                                      :out {:out (keyword dtype)}}
                                     ;;_ (tap> [:fn-to-inputs (function-to-inputs ddata)])
                                               ports (cond (= dtype "map")
                                                           {:out (assoc (into {} (for [[k v] ddata] {k (keyword (ut/data-typer v))})) :* :map)}
                                                           (or (= dtype "vector") (= dtype "rowset")) ; = dtype "vector")
                                                           {:out (assoc (into {} (for [k (range (count ddata))
                                                                                       :let [v (get ddata k)]] {(keyword (str "idx" k)) (keyword (ut/data-typer v))})) :* :vector)}
                                                           :else ports)
                                               inputs (filter (fn [x] (and (keyword? x)
                                                                           (or
                                                                            (cstr/starts-with? (str x) ":input-")
                                                                            (cstr/starts-with? (str x) ":*")
                                                                            (cstr/ends-with? (str x) "*"))))
                                                              (ut/deep-flatten ddata))
                                               flow-id @(re-frame/subscribe [::selected-flow])
                                               existing-inputs (get-in @(re-frame/subscribe [::flowmap]) [flow-id bid :ports :in])
                                               inputs-map (into {} (for [i inputs] {i (get existing-inputs i :any)}))]
                                     ;; full (-> (get @(re-frame/subscribe [::flowmap]) bid)
                                     ;;          (assoc-in [:data :user-input] ddata)
                                     ;;          (assoc-in [:ports] ports))

                              ;(swap! flowmaps assoc bid full)
                                 ;(re-frame/dispatch [::update-flowmap-key bid nil full])
                                           (re-frame/dispatch [::update-flowmap-key-in bid [:ports :in] inputs-map])
                                           (re-frame/dispatch [::update-flowmap-key-in bid [:data :user-input] ddata])
                                           (re-frame/dispatch [::update-flowmap-key-in bid [:ports :out] ports])
                                           (re-frame/dispatch [::clean-connections]))
                                         (catch :default e (tap> [:saving-issue :code-box-rwo bid (str e) :parse-issue?]))))
                                                ;;(swap! flowmaps assoc-in [bid :data :user-input] value)
                                                ;;(re-frame/dispatch [::update-flowmap-key bid nil vval])
                        :options {:mode              syntax ;"clojure"
                                  :lineWrapping      true
                                  :lineNumbers       true
                                  :matchBrackets     true
                                  :autoCloseBrackets true
                                  :autofocus         false
                                  :autoScroll        false
                                  :detach            true
                                  :readOnly          false
                                  :theme             (theme-pull :theme/codemirror-theme nil)}}]]])) ;"ayu-mirage" ;"hopscotch"

(def rename-block (reagent/atom nil))

(defn flow-details-block-container [title flow-id bid content & [icon]]
  (let [open? (true? (get-in @flow-details-block-container-atom [flow-id bid title :open?]
                             (not
                              (or (cstr/ends-with? (str title) "*")
                                  (cstr/ends-with? (str title) "browser")))))]
    ;;(tap> [:panel-deets open? title flow-id bid])
    ;;(tap> [:dbg? (cstr/includes? (str title) "debugger")])
    [re-com/v-box
     ;:padding "2px"
     :width "586px"
     :style {:border (str "1px solid " (theme-pull :theme/editor-outer-rim-color nil) 45)
             :font-size "12px"
             :border-radius "7px"
             :background-color (str (theme-pull :theme/editor-rim-color nil) "18")}
     :children [[re-com/h-box
                 :padding "5px"
                 :height "30px"
                 ;:style {:border "1px solid white"}
                 :justify :between :align :center
                 :children [;[re-com/box :child "header1"]
                            [re-com/h-box
                             :gap "3px" :align :center :justify :center
                             :children (into
                                        (if icon
                                          (cond (string? icon)
                                                [[re-com/md-icon-button :src (at)
                                                  :md-icon-name icon
                                                  :style {:color (if open?
                                                                   (str (theme-pull :theme/editor-outer-rim-color nil) 99)
                                                                   (str (theme-pull :theme/editor-outer-rim-color nil) 45))
                                                          :font-size "14px"}]]
                                                (vector? icon)
                                                [[re-com/h-box
                                                  :gap "0px"
                                                  :style {:padding-left "3px"}
                                                  :children (vec (for [i icon]
                                                                   [re-com/md-icon-button :src (at)
                                                                    :md-icon-name i
                                                                    :style {:margin-left "-1px"
                                                                            :color (if open?
                                                                                     (str (theme-pull :theme/editor-outer-rim-color nil) 99)
                                                                                     (str (theme-pull :theme/editor-outer-rim-color nil) 45))
                                                                            :font-size "15px"}]))]]) [])
                                        [[re-com/box
                                          :child (str title)
                                          :attr {:on-click #(swap! flow-details-block-container-atom assoc-in [flow-id bid title :open?] (not open?))}
                                          :style {:color (str (theme-pull :theme/editor-outer-rim-color nil) 99)
                                                  :cursor "pointer"
                                                  :filter "drop-shadow(0 2px 2px black)" ;; nice and subtle
                                                  :padding-left "4px"}]])]

                            [re-com/md-icon-button :src (at)
                             :md-icon-name "zmdi-more"
                             :on-click #(swap! flow-details-block-container-atom assoc-in [flow-id bid title :open?] (not open?))
                             :style {:color (if open?
                                              (str (theme-pull :theme/editor-rim-color nil) 99)
                                              (theme-pull :theme/editor-outer-rim-color nil))
                                     :cursor "pointer"
                                     :font-size "17px"}]]]

                            ;[re-com/box :child "header2"]

                (when open? ;(get-in @flow-details-block-container-atom [flow-id bid title :open?] true)
                  [re-com/box
                   :padding "8px"
                   :child content])
                ;[re-com/box :child "footer?"]
                [re-com/gap :size "6px"]]]))

(defn flow-details-block [cch ccw id fmap show-output?]
  (let [;honeycomb? (try (and (keyword? (first body)) (vector? body)) (catch :default _ false))
        react-hacks [@scroll-id]
        type (cond (= (get-in fmap [:data :source-panel]) :flow-fn-list*) :flow-part
                   :else (get-in fmap [:data :drag-meta :type]))
        ttype (get-in fmap [:data :drag-meta :type])
        flow-select @(re-frame/subscribe [::selected-flow-block])
        selected? (= (str id) (str flow-select))
        hovered? (= (str id) (str @flow-hover))
        raw-fmap @(re-frame/subscribe [::flowmap-raw-block id]) ;;(get @flowmaps id)
        flow-id @(re-frame/subscribe [::selected-flow])
        cvalue (get-in @(re-frame/subscribe [::http/flow-results]) [:return-maps flow-id id] "(not run yet)")
        error? (get cvalue :error false)
        styler (get-in fmap [:data :flow-item :style] {})
        ;styler-selected (get-in fmap [:data :flow-item :style-selected] {})
        outputs (get-in fmap [:ports :out])
        override-value (get-in @(re-frame/subscribe [::http/flow-results]) [:return-maps (str flow-id) id])
        outputs (if (and override-value (= (vec (keys outputs)) [:out])) ;; if we have return data,
                  {:out (keyword (ut/data-typer override-value))} ;; change output data types to be more accurate
                  outputs)
        python? (= (get-in fmap [:data :syntax]) "python")
        out-color (try
                    (or (get styler :color nil)
                        (get (theme-pull :theme/data-colors db/data-colors)
                             (if (some #(= % :*) (keys outputs)) :map ;; if its a map always use map color for base block
                                 (gn (first (vals outputs))))
                             (get (theme-pull :theme/data-colors db/data-colors) "unknown" "#FFA500")))
                    (catch :default _ "orange"))
        ;_ (tap> [ :works? id])
        ccolor (cond error? "red"
                     python? "#F0E68C"
                     :else (if (or selected? hovered?) out-color (str out-color 75)))
        flow-id-regex #"^[a-zA-Z0-9_-]+$" ;; alpha, underscores, hypens, numbers
        read-only-flow? (true? (cstr/includes? flow-id "/"))
        str-id (cstr/replace (str id) #":" "")
        block-renamer (if (= @rename-block id)
                        [re-com/input-text
                         :src (at)
                         :model             str-id ;(str id)
                         :width             "200px"
                         :on-change #(do (if (= str-id (str %)) ;; regex doesnt allow : so we need to remove before
                                           (reset! rename-block nil)
                                           (do (re-frame/dispatch [::rename-block id %])
                                               (reset! rename-block nil))))
                         :validation-regex  flow-id-regex
                         :change-on-blur?   true
                         :style  {:text-decoration "underline"
                                  :color "inherit" :margin-top "3px"
                                  :margin-left "-4px"
                                  :text-align "left" :background-color "#00000000"}]

                        [re-com/box
                         :attr (if selected? {:on-double-click #(reset! rename-block id)} {})
                         :child (str id)])
        ;show-output? (or (get-in fmap [:data :view-output?] false) output?)
        output-toggle [re-com/md-icon-button :src (at)
                       :md-icon-name "zmdi-more"
                       :on-click #(re-frame/dispatch [::flow-block-meta id :view-output? (not show-output?)])
                       :style {:color ccolor ; (theme-pull :theme/editor-outer-rim-color nil)
                               :cursor "pointer"
                               ;:height "15px"
                               ;:margin-left (if @(re-frame/subscribe [::bricks/flow-editor?]) "-9px" "9px")
                               :font-size "17px"}]
        ;cvalue (if error?
        ;         (get cvalue :error)
        ;           cvalue)
        return-val (get-in fmap [:data :user-input] cvalue)
        default-output [re-com/v-box
                        :gap "9px" :padding "8px"
                        :width (px ccw) ;"350px"
                        :height "380px" :size "none"
                        :align :center :justify :center
                        :style {:margin-top "-9px"}
                        :children [;[re-com/box :child (str id " (viewer)")]
                                   [re-com/h-box
                                    :width "350px" :size "none"  :padding "6px"
                                    :justify :between :align :center
                                    :padding "8px" :style {:margin-top "17px"}
                                    :children [;[re-com/box :child (str id)]
                                               block-renamer
                                               [re-com/box :child "(viewer)"]]]
                                   [render-icon (get fmap :icon) ccolor]
                                   [re-com/box :padding "6px"
                                    :style {:font-size "11px"}
                                    :child (get-in fmap [:data :flow-item :description] " ")]
                                  ;;  [re-com/box
                                  ;;   :child (str (get-in fmap [:data :user-input] cvalue))]
                                   ;[code-box 370 280 (str (get-in fmap [:data :user-input] cvalue))]

                                   (cond (and (vector? return-val) (keyword? (first return-val))) ;; try to render as rabbit-code
                                         [re-com/box
                                          :size "none" :height "280px" :width (px ccw) ;"350px"
                                          :style {:overflow "auto"}
                                          :child [buffy/render-honey-comb-fragments return-val 7 5]]

                                         (vector? return-val)
                                         [re-com/box
                                          :size "none" :height "280px" :width (px ccw) ;"350px"
                                          :style {:overflow "auto"}
                                          :child [map-boxes2 return-val :sub-flows-used nil [] :output "vector"]]

                                         (map? return-val)
                                         [re-com/box
                                          :size "none" :height "280px" :width (px ccw) ;"350px"
                                          :style {:overflow "auto"}
                                          :child [map-boxes2 return-val :sub-flows-used nil [] :output "map"]]

                                         :else
                                         [re-com/box
                                          :size "none" :height "280px" :width (px ccw) ;"350px"
                                          :child [code-box ccw 280 (if (string? return-val) (pr-str return-val) return-val)]])]]]
   ; (tap> [:blocks id fmap])
    [re-com/v-box
     :attr {:id (str id)
            :on-drag-over #(when @bricks/swap-layers? (reset! bricks/swap-layers? false))
            :on-click #(select-block id)}
     ;:width "505px"
     :height (px cch)
     :size "none"
     :padding "10px"
     ;:min-width "130px"
     :width (px ccw)
     :align :center
     :justify :center
     :style {:color ccolor ;(str (theme-pull :theme/editor-font-color nil) 35)
            ;;  :border (str "4px " (if selected? "dashed" "solid")  " "
            ;;               (if (or selected? hovered?) out-color
            ;;                   (str (theme-pull :theme/editor-outer-rim-color nil) 16)))
             :border "1px solid cyan"
             :background-color (str ccolor 10)
             ;:border-radius "12px"
             :margin-top "13px"}
     ;:attr {:on-click #(select-block id)}
     :children [;; flow-details-block-container [content title flow-id bid]
                ;[flow-details-block-container @editor-mode flow-id id default-output]
                ;[flow-details-block-container @editor-mode flow-id id
                (cond
              ;true [code-box 500 400 fmap]
                  show-output? ;; force default output render
                  default-output

                  (= @editor-mode :debug)
                  [re-com/box :child [(if read-only-flow? code-box code-box-rw) 600 440
                                    ;(get @(re-frame/subscribe [::flowmap]) @flow-select)
                                      raw-fmap ;(get fmap flow-select)
                                      flow-select]
                   :width "600px" :height "430px" :size "1"]

                  (= ttype :open-fn)
                  [re-com/v-box :size "none" :gap "9px" :padding "6px"
                   :align :center :justify :center
                   :width (px ccw) ;"350px"
                   :height "380px"
                   :style {:margin-top "-5px"}
                   :children [[re-com/h-box
                               :width "350px" :size "none"
                               :justify :between :align :center
                               :padding "8px" :style {:margin-top "17px"}
                               :children [;[re-com/box :child (str id)]
                                          block-renamer
                                          [re-com/box :child "clojure"]]]
                              [re-com/box
                               :child [code-box-fn ccw 378 (str (get fmap :raw-fn "(fn [x] x)")) id]]]]

                  (= type :sub-flow)
                  (let [;flowmaps-connections @(re-frame/subscribe [::flowmap-connections])
                    ;flow-id @(re-frame/subscribe [::selected-flow])
                    ;flow-map @(re-frame/subscribe [::flowmap])
                        subby-results (get-in @(re-frame/subscribe [::http/flow-results]) [:return-maps])
                        sub-flow-lookup (into {} (for [k (keys subby-results)] {(keyword (last (cstr/split (str k) "/"))) k}))
                        sfl (get sub-flow-lookup id)]
                    ;(tap> [:lookups sub-flow-lookup sfl])
                    [re-com/box
                     ;:width "460px"
                     :height "380px" :size "none"
                     :style {;:border "1px solid pink"
                             :overflow "auto"}
                     :child [re-com/v-box
                             :width (px ccw) ;"455px" ;:height (px (+ 600 (* 6 bricks/brick-size)))
                             :height "380px"
                             :size "none" :align :center :justify :between
                             :children [[re-com/h-box :width "440px"
                                                    ;:style {:border "1px solid white"}
                                         :justify :between :align :center
                                         :children [[re-com/box :child (str type) :style {:color out-color}]
                                                    [re-com/box :child (str sfl) :style {:color out-color}]]
                                                ;[re-com/box :child "return values" :style {:color out-color}]

                                         :padding "6px"]
                                        [re-com/box
                                     ;:align :center :justify :center
                                                   ; :style {:border "1px solid white"}
                                         ;:width "440px"
                                         :height "340px"
                                         :size "none"
                                         :child [re-com/v-box
                                                 :gap "18px"
                                                 :children (vec (for [i (filter #(not (= (str %) (str flow-id))) (keys subby-results))]
                                                                  [re-com/v-box
                                                                   :children
                                                                   [[re-com/box :child (str i) :style {:font-size "19px"}]
                                                                    [map-boxes2 (get subby-results i) :sub-flows-used nil [] :sub-flows-used "map"]]]))]]]]])
                                                    ;:child (buffy/render-honey-comb-fragments body 10 10)

                  (= type :view) (let [qid (get-in fmap [:data :drag-meta :source-table])
                                       body (get-in fmap [:data :drag-meta :body])]
                                   [re-com/box
                                    :width "500px"
                                    :height "380px" :size "none"
                                    :style {;:border "1px solid pink"
                                            :overflow "auto"}
                                    :child [re-com/v-box
                                            :width "495px" ;:height (px (+ 600 (* 6 bricks/brick-size)))
                                            :height "400px"
                                            :size "auto" :align :center :justify :between
                                            :children [[re-com/h-box :width "490px"
                                                    ;:style {:border "1px solid white"}
                                                        :justify :between :align :center
                                                        :children [[re-com/box :child (str qid) :style {:color out-color}]
                                                                   [re-com/box :child (str type) :style {:color out-color}]] :padding "6px"]
                                                       [re-com/box
                                                        :align :center :justify :center
                                                   ; :style {:border "1px solid white"}
                                                        :width "490px" :height "360px"
                                                        :size "none"
                                                        :child [buffy/render-honey-comb-fragments body 9 7]]]]])
                                                    ;:child (buffy/render-honey-comb-fragments body 10 10)

                  (= type :open-block) (let [;edit-style :fragment
                                             syntaxes ["raw (clojure)" "python" "r" "julia"]
                                             widths [200 350 750]
                                             width (get-in fmap [:data :width] 350)
                                             syntax (get-in fmap [:data :syntax] "raw (clojure)")]
                                         ;(tap> [:oo id fmap])
                                         [re-com/v-box :size "none" :gap "9px" :padding "6px"
                                          :align :center :justify :center
                                          :width (px width) :height "380px"
                                          :style {;:border "1px solid green"
                                                  :margin-top "-5px"}
                                          :children [[re-com/h-box
                                                      :width (px width) :size "none"
                                                      :justify :between :align :center
                                                      :padding "8px" :style {:margin-top "17px"}
                                                      :children [;[re-com/box :child (str id)]
                                                                 block-renamer
                                                                 [re-com/box :child (str type)]]]
                                                     [(if read-only-flow? code-box code-box-rwo) width 330
                                                      (get-in raw-fmap [:data :user-input] (pr-str "feed me, seymour!"))
                                                      id syntax]
                                                     [re-com/h-box
                                                      :width (px width) :height "40px" :size "none"
                                                      :justify :between :align :center
                                                      :gap "8px"
                                                      :children [[re-com/h-box
                                                                  :gap "8px" :style {:font-size "11px"}
                                                                  :children (for [e widths
                                                                                  :let [selected? (= e width)]]
                                                                              [re-com/box
                                                                               :attr (when (not selected?) {:on-click #(re-frame/dispatch [::flow-block-meta id :width e])})
                                                                               :style (if selected? {} {:opacity 0.4 :cursor "pointer"})
                                                                               :child (str e)])]
                                                                 [re-com/h-box
                                                                  :gap "8px" :style {:font-size "11px"}
                                                                  :children (for [e syntaxes
                                                                                  :let [selected? (= e syntax)]]
                                                                              [re-com/box
                                                                               :attr (when (not selected?) {:on-click #(re-frame/dispatch [::flow-block-meta id :syntax e])})
                                                                               :style (if selected? {} {:opacity 0.4 :cursor "pointer"})
                                                                               :child (str e)])]]]]])
                  (= type :flow-part) default-output
                  ;; [re-com/v-box
                  ;;                      :gap "9px"
                  ;;                      :width "200px"  :height "380px" :size "none"
                  ;;                      :align :center :justify :center
                  ;;                      :children [[re-com/box :child (str id)]
                  ;;                                 [render-icon (get fmap :icon) ccolor]
                  ;;                                 [re-com/box :padding "6px"
                  ;;                                  :style {:font-size "11px"}
                  ;;                                  :child (get-in fmap [:data :flow-item :description])]
                  ;;                                 [re-com/box
                  ;;                                  :child (str (get-in fmap [:data :user-input] cvalue))]]]
                  (= type :query) (let [qid (get-in fmap [:data :drag-meta :source-table]) ;; original query
                                        connection-id (get-in fmap [:data :connection-id])
                                        query-body @(re-frame/subscribe [::bricks/find-query qid])
                                    ;q (get @(re-frame/subscribe [::conn/clicked-parameter-key [:queries-sys]]) qid)
                                        packed-query (merge (-> query-body
                                                                (assoc :connection-id connection-id))
                                                            {:_w 10 :_h 5})]
                                ;[buffy/render-honey-comb-fragments]
                                    [re-com/box
                                     :width "500px" :height "380px" :size "none"
                                     :style {;:border "1px solid pink"
                                             :overflow "auto"}
                                     :child [re-com/v-box
                                             :width "495px" ;:height (px (+ 600 (* 6 bricks/brick-size)))
                                             :height "368px"
                                             :size "none" :align :center :justify :center
                                             :children [[re-com/h-box :width "490px"
                                                         :justify :between :align :center
                                                         :children [[re-com/box :child (str qid) :style {:color out-color}]
                                                                    [re-com/box :child (str type) :style {:color out-color}]] :padding "6px"]
                                                        [buffy/render-honey-comb-fragments packed-query]
                                                        [code-box 500 163 query-body]]]])
              ;[re-com/box :child "query"]
                  :else [re-com/box :child (str type "?")])
                (if
                 (or (= (get-in fmap [:data :drag-meta :type]) :open-fn)
                     (and ;(not (= type :open-block))
                          ;(not (= type :flow-part))
                      (not (= type :query))))
                ;;  [re-com/box
                ;;   :align :start :justify :start
                ;;   :child output-toggle]
                  [re-com/gap :size "17px"])]]))

(defn run-gantt-chart [flow-id]
  [buffy/render-honey-comb-fragments (let [reaction! [@trig-atom-test]]
                                       (walk/postwalk-replace
                                        {:fflow-id flow-id
                                         :ttttt @trig-atom-test}
                                        {:queries {:live-dat {:select [:*]
                                                              :_last :ttttt
                                                              :from
                                                              [{:data
                                                                '(vec (filter
                                                                       (fn [x]
                                                                         (not (clojure.string/includes?
                                                                               (get x :block)
                                                                               "/")))
                                                                       (for [row (get (clojure.core/deref
                                                                                       flowmaps.db/fn-history)
                                                                                                 ;"quick-maths-v2"
                                                                                      :fflow-id)]
                                                                         {:start (get row :start 0)
                                                                          :end (+ 5 (get row :end 0))
                                                                          :block (str (get row :block "?"))})))}]}}

                                                              ;;  :live-dat-2 {:select [:block [[:count 1] :rowcnt]
                                                              ;;                        [[:sum :elapsed_ms]
                                                              ;;                         :elapsed_ms_sum_205] :start :end
                                                              ;;                        [[:+ :end 1] :end_plus] :run_id]
                                                              ;;               :from
                                                              ;;               [[{:select [:base_flow_id :block :channel
                                                              ;;                           :data_type :dbgn :dest
                                                              ;;                           :elapsed_ms :end :end_ts :flow_id
                                                              ;;                           :from_block :path :run_id :start
                                                              ;;                           :start_ts :ts :type :value]
                                                              ;;                  :from   [[:fn_history :qq205]]
                                                              ;;                  :where  [:= :base_flow_id
                                                              ;;                            ;"loop-time1!"
                                                              ;;                           :fflow-id]} :xx662]]
                                                              ;;               :group-by [:block :start :end :run_id]
                                                              ;;               :order-by [[:rowcnt :desc]]
                                                              ;;               :where
                                                              ;;               [:and [:not [:like :block "%/%"]]
                                                              ;;                [:= :run_id
                                                              ;;                 "0b229078-720b-44f1-be25-5c53071a8535"]]}

                                         :view [:vega-lite
                                                {:data {:values :live-dat}
                                                 :mark "bar"
                                                 :encoding
                                                 {:x {:field :start
                                                      :type "quantitative"
                                                                    ;:type "temporal"
                                                      :timeUnit "yearmonthdatehoursminutessecondsmilliseconds"
                                                                    ;:timeUnit "yearmonthdatehoursminutesseconds"
                                                                ;;  :scale {:domain
                                                                ;;          [:start-drag-26/start_min.0
                                                                ;;           :end-drag-43/end_max.0]}
                                                      :scale {:zero false :nice false}
                                                      :axis {:title ""
                                                             :format "%Y-%m-%d %H:%M:%S"}}

                                                  :x2 {:field :end
                                                       :scale {:zero false :nice false}}
                                                                  ;; :scale {:domain
                                                                  ;;         [:start-drag-26/start_min.0
                                                                  ;;          :end-drag-43/end_max.0]}

                                                  :y {:field :block
                                                      :type "ordinal"
                                                      :axis {:title ""}}

                                                  :tooltip
                                                  [{:field "start"
                                                    :type "temporal"
                                                    :timeUnit
                                                    "utcyearmonthdatehoursminutessecondsmilliseconds"}
                                                   {:field "value"}
                                                   {:field "ms"
                                                    :type "temporal"
                                                    :timeUnit "minutessecondsmilliseconds"}
                                                   {:field "type"}
                                                   {:field "path"}
                                                   {:field "dest"}
                                                   {:field "end"
                                                    :type "temporal"
                                                    :timeUnit
                                                    "utcyearmonthdatehoursminutessecondsmilliseconds"}]
                                                  :color {:scale
                                                          :theme/vega-default-color-scheme
                                                          :legend nil
                                                          :field :block
                                                          :type "ordinal"}}
                                                 :config :theme/vega-defaults
                                                 :width "container"
                                                 :height :panel-height
                                                 :padding 4
                                                 :background "transparent"}
                                                {:actions false}]})) 7 11])
(defonce scheduler-atom (reagent/atom {}))

(defn edit-flow-title [flow-id w]
  (let [read-only-flow? (true? (cstr/includes? flow-id "/"))
        flow-id-regex #"^[a-zA-Z0-9_-]+$"] ;; alpha, underscores, hypens, numbers
    (if (not @title-edit-idx)
      [re-com/box
       :size "none"
       :height            "45px"
       :width (px w)
       :align :center :justify :center
       :attr {:on-double-click #(when (not read-only-flow?) (reset! title-edit-idx (str flow-id)))}
       :style {;:text-decoration "underline"
                           ;:margin-top "-5px"
               :cursor "pointer"
               :padding-left "8px"
                           ;:margin-right "13px"
                           ;:border "2px solid lime"
                           ;:border (str "2px solid #00000000")
               :font-size "30px"}
       :child (str flow-id)]

      [re-com/input-text
       :src (at)
       :model             (str flow-id)
       :width             (px w)
       :height            "45px"
       :on-change         #(do (re-frame/dispatch [::rename-flow flow-id %])
                               (reset! title-edit-idx nil))
       :validation-regex  flow-id-regex
       :change-on-blur?   true

       :style  {;:font-size "20px"
                                                   ;:margin-top "28px"
                :border (str "2px dashed " (theme-pull :theme/editor-outer-rim-color nil))
                :font-size "30px"
                :text-decoration "underline"
                :color (theme-pull :theme/editor-outer-rim-color nil)
                                                                  ;:font-weight 700
                :font-style "underline"
                                                   ;:border "0px solid black"
                                                   ;:padding "8px"
                :text-align "center"
                                                            ; :float "right"
                                                                ;:margin-top "10px"
                                                                ;:padding-top "10px"
                                                      ;          :text-align "right"
                :background-color "#00000000"}])))

(defn run-history-bar [flow-id flow-select]
  [re-com/box
   :style {:margin-left "25px" :margin-top "15px"}
   :child [buffy/render-honey-comb-fragments
           {:queries
            {:runstream-chart-simple-python-exec
             {:select        [[[[:min :elapsed]] :elapsed]
                              :started]
                            ;:refresh-every 10
                            ;:cache?        false
              :_ttttt @trig-atom-test
              :connection-id "flows-db"
              :from          [{:select [:client_name :elapsed
                                        :ended :flow_id
                                        :human_elapsed :in_error
                                        :started :ts]
                               :from   [[:flow_history :mm134]]
                               :where  [:= :flow_id
                                        flow-id]}]
              :group-by      [:started]}}
            :view [:> :ResponsiveContainer
                   {:width "100%" :height :panel-height+50}
                   [:> :BarChart
                    {:data   :runstream-chart-simple-python-exec
                     :margin {:top 5 :bottom 5 :right 30 :left 20}}
                    [:> :CartesianGrid
                     {:strokeDasharray "1 4" :opacity 0.33}]
                    [:> :Tooltip]
                    [:> :XAxis {:dataKey :started}]
                    [:> :Bar
                     {:dataKey :elapsed
                      :stroke  :theme/editor-outer-rim-color
                      :fill    :theme/editor-outer-rim-color}]]]}
           5.5 11]])

(defn settings-block [flow-id]
  (let [flow-select @(re-frame/subscribe [::selected-flow-block])
        flowmaps @(re-frame/subscribe [::flowmap-raw])
        ;flow-id @(re-frame/subscribe [::selected-flow])
        read-only-flow? (true? (cstr/includes? flow-id "/"))]
    (cond
      (= @editor-mode :run-history)
      (let [viz1 {:queries
                  {:gen-viz-1090
                   {:select   [[[[:count 1]] :value]
                               [[:substr :start_ts 0 11] :day]]
                    :connection-id "flows-db"
                    :from     [[:fn_history :cc393]]
                    :group-by [[:substr :start_ts 0 11]]}}
                  :view [:nivo-calendar
                         {;:labelTextColor "#ffffff" ;"#423939"
                          :emptyColor "#00000000"
                          :axisLeft {:tickRotation   0
                                     :legendPosition "middle"
                                     :legendOffset   -65}
                          :dayBorderColor "#ffffff10"
                          :enableLabel false
                          :motionDamping 10
                          :axisBottom {:tickRotation   0
                                       :legendPosition "middle"
                                       :legendOffset   40}
                          :inner-padding 0
                          :width 564
                          :monthBorderColor "#ffffff15"
                          :colors ["#2a4858" "#294d5d" "#275163"
                                   "#255667" "#225b6c" "#1e6071"
                                   "#1a6575" "#156a79" "#0e6f7d"
                                   "#057480" "#007983" "#007e86"
                                   "#008488" "#00898a" "#008e8c"
                                   "#00938d" "#00988e" "#039d8f"
                                   "#12a28f" "#1ea78f" "#28ac8f"
                                   "#32b18e" "#3cb68d" "#46bb8c"
                                   "#50bf8b" "#5ac489" "#64c987"
                                   "#6ecd85" "#79d283" "#83d681"
                                   "#8eda7f" "#99de7c" "#a5e27a"
                                   "#b0e678" "#bcea75" "#c8ed73"
                                   "#d4f171" "#e0f470" "#edf76f"
                                   "#fafa6e" "#fafa6e" "#faf568"
                                   "#faf162" "#faec5d" "#fae757"
                                   "#f9e352" "#f9de4c" "#f9d947"
                                   "#f9d441" "#f9cf3c" "#f8cb37"
                                   "#f8c632" "#f8c12c" "#f8bc27"
                                   "#f7b722" "#f7b21d" "#f6ad17"
                                   "#f6a811" "#f6a30a" "#f59e04"
                                   "#f49800" "#f49300" "#f38e00"
                                   "#f28800" "#f28300" "#f17d00"
                                   "#f07800" "#ef7200" "#ee6c00"
                                   "#ed6600" "#ec6000" "#eb5a00"
                                   "#ea5300" "#e84c00" "#e74500"
                                   "#e53d00" "#e43502" "#e22b05"
                                   "#e11e08" "#df0b0b"]
                                  ;:theme :theme/nivo-defaults
                          :click {:x :day :y :value}
                          :padding 0.1
                          :enableGridX true
                          :border-radius 2
                          :enableGridY true
                          :height 210
                          :margin {:top 0 :right 5 :bottom 80 :left 45}
                          :data :gen-viz-1090}]}
            grid1 {:select
                   [;:base_flow_id
                    :flow_id
                    :run_id
                                ;[[:min :start_ts] :start]
                    [[:max :end_ts] :end]
                    [[:count 1] :evts]
                                ;[[:count [:distinct :run_id]] :runs]
                                ;; [[:avg [:round
                    [[:round [:*
                              [:- [:julianday [:max :end_ts]]
                               [:julianday [:min :start_ts]]] 86400] 2] :seconds]
                                ;;  :avg_mins]
                                ;; [;[:avg ;[:round
                                ;;         [:- [:max :end_ts]
                                ;;          [:min :start_ts]]
                                ;;        ; 2
                                ;;        ; ]
                                ;; ; ]
                                ;; :avg_mins]
                    ]

                   :connection-id "flows-db"
                   :group-by [1 2]
                   :order-by [[3 :desc]]
                   :from [[:channel_history :tt336]]}]
        [re-com/v-box
                 ;:align :center
                 ;:justify :center
         :height "430px"
                 ;:style {:border "1px solid white"}
         :gap "10px"
         :children [[re-com/box
                     :size "none"
                     :width "595px"
                     :height "125px"
                     :style {:font-size "15px"}
                                     ;:border "1px solid white" ;:margin-top "-30px"

                     :child [buffy/render-honey-comb-fragments viz1 12 12]]
                                ;; [re-com/box
                                ;;  :align :center :justify :center
                                ;;  :child "hey hey hye"]
                    [re-com/box
                     :size "none"
                     :width "595px"
                     :height "275px"
                             ;:padding "10px"
                     :align :center :justify :center
                     :style {:font-size "15px"}
                                     ;:border "1px solid white"

                     :child [buffy/render-honey-comb-fragments grid1 11 6]]]])
      (= @editor-mode :flow-browser)
      [re-com/h-box
             ;:gap "6px"
       :children [[re-com/box
                   :size "auto"
                   :child [bricks/magic-table :flow-list* [:flows-sys] 12 9 []]]]]
      (= @editor-mode :part-browser)
      [re-com/h-box
             ;:gap "6px"
       :children [[re-com/box
                   :size "auto"
                           ;:style {:margin-left "-10px"}
                   :child ;[bricks/magic-table :system-connections-list* [:connections-sys] 3 9 [:database_name]]
                   [bricks/magic-table :flow-cat-list* [:flow-fn-categories-sys] 3 9 []]]
                  [re-com/box
                   :size "auto"
                           ;:style {:margin-left "-10px"}
                   :child [bricks/magic-table :flow-fn-list* [:flow-fn-sys] 9 9 [:full_map :category]]]]]

      (= @editor-mode :debug)
      (if flow-select
                ;[code-box 480 400 (get @(re-frame/subscribe [::flowmap]) @flow-select)]
        [re-com/box :child [(if read-only-flow? code-box code-box-rw) 600 440
                                    ;(get @(re-frame/subscribe [::flowmap]) @flow-select)
                            (get flowmaps flow-select)
                            flow-select]
         :width "600px" :height "430px" :size "1"]
                    ;[re-com/box :child "no block selected"]
        [code-box 600 450 flowmaps nil])

      (= @editor-mode :scheduler)
      (let [grid1 {:select [:flow_id :override :schedule :ts]
                           ;:where [:= :flow_id (str flow-id)]
                   :connection-id "flows-db"
                           ;:col-widths {:override 25} ;; doesnt work for system tables? wtaf TODO
                           ;:group-by [1 2]
                   :order-by [[:ts :desc]]
                   :from [[:live_schedules :tt336]]}
            vec2choices (fn [x] (vec (for [i x] {:label (str i) :id i})))
            comps (keys flowmaps)
            flow-names (vec (distinct (map :flow_id @(re-frame/subscribe [::conn/sql-data [:flows-sys]]))))
            ddown (fn [x xdef w kkey] [re-com/box
                                       :size "none"
                                       :align :center
                                       :width (px (or w 100))
                                              ; :style {:opacity (if (nil? (get-in @scheduler-atom [flow-id kkey])) 0.4 1.0)}
                                       :child [re-com/single-dropdown
                                               :width (px (or w 100))
                                                  ;:placholder (str xdef)
                                               :style {:border (str "1px solid " (theme-pull :theme/editor-outer-rim-color nil) 45)
                                                               ;:opacity (if (nil? (get-in @scheduler-atom [flow-id kkey])) 0.4 1.0)
                                                       :color "inherit"
                                                          ;:background-color "green"
                                                       :focus {:outline "none"}
                                                       :font-size "18px"}
                                               :parts {:chosen-drop {:style {:color "green"}}}
                                               :choices (vec2choices x)
                                               :on-change #(swap! scheduler-atom assoc-in [flow-id kkey] %)
                                               :model (get-in @scheduler-atom [flow-id kkey] xdef)]])
            date-input (fn [input-regex w dd kkey]
                         [re-com/input-text
                          :src (at)
                          :model             (get-in @scheduler-atom [flow-id kkey] dd)
                          :width             (px (or w 90))
                          :on-change         #(swap! scheduler-atom assoc-in [flow-id kkey] %)
                                  ;:validation-regex  input-regex
                          :change-on-blur?   true
                          :style  {;:font-size "20px"
                                        ;:margin-top "28px"
                                   :opacity (if (nil? (get-in @scheduler-atom [flow-id kkey])) 0.4 1.0)
                                                ;:border (str "2px dashed " (theme-pull :theme/editor-outer-rim-color nil))
                                   :font-size "16px"
                                   :border (str "2px solid " (theme-pull :theme/editor-outer-rim-color nil) 45)
                                                ;:text-decoration "underline"
                                   :color "inherit"
                                   :height "36px"
                                        ;:border "0px solid black"
                                        ;:padding "8px"
                                                ;:text-align "right"
                                                 ; :float "right"
                                                     ;:margin-top "10px"
                                                     ;:padding-top "10px"
                                           ;          :text-align "right"
                                   :background-color "#00000000"}])]
                                           ;_ (tap> [:flow-list comps])

                                                   ;:color (theme-pull :theme/editor-font-color nil)
                                           ;          :padding-top "1px"
                                            ; "dd MMM, yyyy"])]
        [re-com/v-box
         :align :center
         :justify :center
                 ;:height "430px"
                 ;:style {:border "1px solid white"}
                 ;:gap "10px"
         :children [[re-com/h-box
                     :size "none"
                     :width "595px"
                     :padding "10px"
                     :height "55px"
                     :align :center :gap "6px"
                     :style {:color (theme-pull :theme/editor-outer-rim-color nil)
                                     ;:border "1px solid white"
                             :margin-top "15px"
                             :font-size "20px"}
                     :children [[re-com/md-icon-button :src (at)
                                 :md-icon-name "zmdi-timer"
                                 :style {:font-size "26px"}]
                                [re-com/box :child "every" :style {:opacity (if (nil? (get-in @scheduler-atom [flow-id :every])) 0.4 1.0)}]
                                [date-input #"^\d{2}$" 45 "10" :every]
                                [ddown ["seconds" "minutes" "hours" "days"] "hours" 110 :period]
                                [re-com/box :child "starting" :style {:opacity (if (nil? (get-in @scheduler-atom [flow-id :starting])) 0.4 1.0)}]
                                [date-input #"^\d{0,4}-?\d{0,2}-?\d{0,2}$" 130 (ut/today) :starting]
                                [re-com/box :child "at" :style {:opacity (if (nil? (get-in @scheduler-atom [flow-id :at])) 0.4 1.0)}]
                                [date-input #"^\d{4}$" 70 "1500" :at]]]
                    [re-com/h-box
                     :size "none"
                     :width "595px"
                     :padding "10px"
                     :height "44px"
                     :align :center :gap "6px"
                     :style {:color (theme-pull :theme/editor-outer-rim-color nil)
                             :opacity 0.2
                                     ;:border "1px solid white"
                             :font-size "20px"}
                     :children [[re-com/md-icon-button :src (at)
                                 :md-icon-name "zmdi-alarm-plus"
                                 :style {:font-size "26px"}]
                                [re-com/box :child "when"]
                                [ddown flow-names (first flow-names) 233 :trigger-flow-id]
                                [re-com/box :child "is"]
                                [ddown [:= :> :>= :<= :< :<>] := 60 :trigger-operator]
                                        ;[date-input #"^\d{0,4}-?\d{0,2}-?\d{0,2}$" 130 "2024-01-01"]
                                        ;[re-com/box :child "at"]
                                [date-input #"^\d{4}$" 145 "<some-val>" :trigger-value]]]

                    [re-com/h-box
                     :size "none"
                     :width "595px"
                     :padding "10px"
                     :height "44px"
                     :align :center :gap "6px"
                     :style {:color (theme-pull :theme/editor-outer-rim-color nil)
                                     ;:opacity 0.2
                                     ;:border "1px solid white"
                             :font-size "20px"}
                     :children [[re-com/md-icon-button :src (at)
                                 :md-icon-name "zmdi-hearing"
                                 :style {:font-size "26px"}]
                                [re-com/box :child "called with "]
                                        ;[ddown flow-names (first flow-names) 233 :trigger-flow-id]
                                [date-input #"." 158 nil :trigger-words]
                                [re-com/box :child "sent to"]
                                        ;[ddown [:= :> :>= :<= :< :<>] := 60 :trigger-operator]
                                [ddown comps (first comps) 180 :trigger-word-insert]]]
                                        ;[date-input #"^\d{0,4}-?\d{0,2}-?\d{0,2}$" 130 "2024-01-01"]
                                        ;[re-com/box :child "at"]
                                        ;[date-input #"^\d{4}$" 145 "<some-val>" :trigger-value]

                    [re-com/h-box
                     :justify :between ;:align :center
                     :padding "10px"
                     :children [[re-com/box
                                 :size "auto" :padding "6px"
                                 :style {:border "0px dashed lime"
                                         :opacity 0.22}
                                 :child (str @scheduler-atom)]
                                [re-com/gap :size "20px"]
                                [re-com/h-box :gap "14px"
                                 :children (for [b ["+" "schedule!"]]
                                             [re-com/box
                                              :padding "5px"
                                              :align :center :justify :center
                                              :min-width "30px" :height "36px"
                                              :attr (if (= b "schedule!")
                                                      {:on-click #(do
                                                                    (re-frame/dispatch
                                                                     [::wfx/request :default
                                                                      {:message    (merge
                                                                                    (get @scheduler-atom flow-id)
                                                                                    {:kind :schedule-flow
                                                                                     :flow-id flow-id
                                                                                            ;:schedule (get @scheduler-atom flow-id)
                                                                                     :client-name @(re-frame/subscribe [::bricks/client-name])})

                                                                       :on-response [::simple-response]
                                                                       :on-timeout  [::timeout-response :run-flow [:schedule flow-id]] ;; requeue?
                                                                       :timeout    15000000}])
                                                                    (swap! scheduler-atom dissoc flow-id))} {})
                                              :child (str b)
                                              :style {:border (str "2px solid " (theme-pull :theme/editor-outer-rim-color nil) 45)
                                                      :filter "brightness(150%)"
                                                      :cursor "pointer"
                                                      :background-color (str (theme-pull :theme/editor-outer-rim-color nil) 11)
                                                      :font-size "18px"
                                                      :color (theme-pull :theme/editor-outer-rim-color nil)}])]]
                             ;:style {:border "1px solid white"}
                     :height "58px"
                     :width "565px"
                     :size "none"]
                            ;;  [re-com/box
                            ;;   :align :center :justify :center
                            ;;   :child "hey hey hye"]
                    [re-com/box :size "auto"
                     :style {:margin-left "18px"
                                     ;:border "1px solid white"
                             :margin-top "15px"}
                     :child [buffy/render-honey-comb-fragments grid1 12 5]]]])

      :else [re-com/box :child (str "unknown editor mode: " @editor-mode)])))

(defn server-flows []
  (let [ss @(re-frame/subscribe [::bricks/flow-statuses])
        ss (vec (sort-by first (for [[k v] ss] [k v])))] ;; since maps wont keep key order in cljs, vectorize it
    [re-com/v-box
     :padding "3px"
     :style {;:border "1px dashed white"
             :color (theme-pull :theme/editor-outer-rim-color nil)}
     ;:gap "4px"
     :children (for [[fid v] ss
                     :let [time-running (get v :*time-running)
                           open-channels (get v :channels-open?)
                           channels (get v :channels-open)
                           started-by (get v :*started-by)
                           process? (get v :process?)
                           command (get v :command)
                           tracker-events (get v :tracker-events)
                           running? (get v :*running?)]]
                 [re-com/v-box
                  :style {:border (str "1px solid " (theme-pull :theme/editor-outer-rim-color nil) (if running? 77 22))
                          :background-color (when running? (str (theme-pull :theme/editor-outer-rim-color nil) 15))
                          :filter (when running? "brightness(200%)")}
                  :children
                  [[re-com/h-box
                    :padding "4px"
                    :height "35px"
                    :align :center
                    :justify :between
                   ;:style {:border (str "1px solid " (theme-pull :theme/editor-outer-rim-color nil) 22)}
                    :children [[re-com/box :child (if (cstr/includes? (str fid) "/")
                                                    (let [spl (cstr/split (str fid) #"/")
                                                          parent (first spl)
                                                          sub (last spl)]
                                                      [re-com/v-box
                                                       :children [[re-com/box :child (str "(sub-flow called from) " parent) :style {:font-size "9px"}]
                                                                  [re-com/box :child (str sub)]]])
                                                    (str fid))
                                :width "50%"
                                :style {:padding-left "4px"
                                        :font-size "15px"
                                        :font-weight 700}]
                             ;[re-com/box :child (str open-channels) :width "33%"]
                               [re-com/box
                                :child (cond running? (str "running (" channels " chans)")
                                             open-channels (str "idling (" channels " chans)")
                                             :else "stopped")

                                :width "34%"]
                               ;[re-com/box :child (str time-running) :width "30%"]
                               (if (not process?)

                                 [re-com/md-icon-button
                                  :style {:font-size "20px"
                                ;:font-size "10px"
                                      ;:color pcolor
                                      ;:margin-right "8px"
                                          }
                                  :md-icon-name "zmdi-open-in-browser"]
                                 [re-com/gap :size "20px"])
                               [re-com/md-icon-button
                                :style {:font-size "20px"
                                      ;:color pcolor
                                      ;:margin-right "8px"
                                        }
                                :on-click #(re-frame/dispatch [::wfx/request :default
                                                               {:message    {:kind :kill-flow
                                                                             :flow-id fid
                                                                             :process? process?
                                                                             :client-name @(re-frame/subscribe [::bricks/client-name])}
                                                                :timeout    15000000}])
                                :md-icon-name "zmdi-stop"]]]

                   [re-com/h-box
                    :padding "4px"
                    :height "35px"
                    :align :center
                    :style {:font-size "10px" :padding-right "20px"}
                    :justify :between
                   ;:style {:border (str "1px solid " (theme-pull :theme/editor-outer-rim-color nil) 22)}
                    :children [[re-com/box :child (if process? (str "(external process) " command)
                                                      (str "started by: " started-by))

                                :style {:padding-left "4px"
                                        :font-weight 300}]
                             ;[re-com/box :child (str open-channels) :width "33%"]

                              ;[re-com/box :child (str tracker-events " events")  ]
                               ]]]])]))

(re-frame/reg-sub
 ::opts-map
 (fn [db _]
   (get-in db [:flows (get db :selected-flow) :opts]
           {:retry-on-error? true :retries 5 :close-on-done? true})))

(declare gantt-container)

(defn flow-editor [w h]
  (let [react-hack [@editor-mode @trig-atom-test]
        sql-params (into {} (for [k [:flow-fn-categories-sys/category]]
                              {k @(re-frame/subscribe [::conn/clicked-parameter-key [k]])}))
        flow-select @(re-frame/subscribe [::selected-flow-block])
        ;flowmaps @(re-frame/subscribe [::flowmap-raw])
        flow-id @(re-frame/subscribe [::selected-flow])
        blocks @(re-frame/subscribe [::flowmap])
        orderb (vec (sort-by str (keys blocks)))
        opts-map @(re-frame/subscribe [::opts-map])
        gantt? @(re-frame/subscribe [::bricks/flow-gantt?])
        ;read-only-flow? (true? (cstr/includes? flow-id "/"))
        ;flow-id-regex #"^[a-zA-Z0-9_-]+$" ;; alpha, underscores, hypens, numbers
        sql-calls {:flow-fn-categories-sys {:select [:category]
                                            :from [:flow_functions]
                                            :group-by [1]}
                   :flow-fn-all-sys {:select [:name]
                                     :from [:flow_functions]
                                     :group-by [1]}
                   :flow-fn-sys {:select [:name :full_map :description :inputs :icon :input_types :output_types :category]
                                 :from [:flow_functions]
                                 :col-widths {:name 215 :description 500}
                                 :where (if (get sql-params :flow-fn-categories-sys/category)
                                          [:= :category (str (get sql-params :flow-fn-categories-sys/category))]
                                          [:= 1 1])}
                   :flows-sys {:select [:flow_id :file_path :last_modified]
                               :from [:flows]
                               :connection-id "flows-db"
                               :order-by [[3 :desc]]}}]

    (dorun (for [[k v] sql-calls]
             (let [query (walk/postwalk-replace sql-params v)
                   data-exists? @(re-frame/subscribe [::conn/sql-data-exists? [k]])
                   unrun-sql? @(re-frame/subscribe [::conn/sql-query-not-run? [k] query])]
               (when (or (not data-exists?) unrun-sql?)
                 (if (get query :connection-id)
                   (conn/sql-data [k] query (get query :connection-id))
                   (conn/sql-data [k] query))))))

    [re-com/v-box
     :size "none"
     :width (px w)
     :height (px h)
     :gap "10px"
     ;:align :start :justify :center
     :style {;:border-right (str "6px solid " (theme-pull :theme/editor-outer-rim-color nil))
             :border-top (str "6px solid " (theme-pull :theme/editor-outer-rim-color nil))
             ;:border-right (str "1px solid " "#00000098")
            ;; :backdrop-filter "blur(1px)"
             :position "fixed"
             :left 0 ;:top 0
             :overflow "auto"
             ;:margin-top "-1px"
          ; :filter "drop-shadow(0.35rem 0.35rem 0.4rem rgba(0, 0, 0, 0.8))"
           ;:mix-blend-mode "screen"
            ;; :background-color (str (theme-pull :theme/editor-outer-rim-color nil) 19)
             :font-family (theme-pull :theme/base-font nil)
             :color (theme-pull :theme/editor-font-color nil)
            ;:background-color (str (theme-pull :theme/editor-background-color nil) 99) ; "#000000"
             :backdrop-filter "blur(5px)"
             :background-color "#00000022"}
     :align :center :justify :start
     :children [[re-com/h-box
                 :size "none"
                 :height "40px"
                 :width (px w)
                 :align :start :justify :between
                 :style {:padding-top "5px"
                         :color (theme-pull :theme/editor-outer-rim-color nil)
                         :font-size "16px"
                         :font-weight 700
                         :padding-left "12px"}
                 :children [[re-com/box
                            ; :size "auto"
                             :align :center
                             :height "40px"
                             :style {:font-size "14px"
                                     ;;:border "1px solid white"
                                     }
                             :child "flow settings"]
                            ;; [re-com/h-box
                            ;;  ;:size "auto"
                            ;;  :justify :center
                            ;;  :align :center
                            ;;  :height "40px"
                            ;;  :gap "9px"
                            ;;  :style {:font-size "12px" :border "1px solid white"}
                            ;;  :children [[re-com/checkbox
                            ;;              ;:style {:margin-top "-2px"}
                            ;;              :model true :on-change #()]
                            ;;             [re-com/box :child  "retry on error?"]
                            ;;             [re-com/single-dropdown
                            ;;              :style {:height "40px"
                            ;;                      :font-zie "10px" :border "1px solid white"}
                            ;;              :choices [{:label "1" :id 1} {:label "2" :id 2} {:label "3" :id 3}]
                            ;;              :model 1
                            ;;              :on-change #()]]]
                            [re-com/gap :size "10px"]]]
                ;[re-com/gap :size "5px"]

                ;; [flow-details-block-container "last run gantt" flow-id flow-select
                ;;  [run-gantt-chart "system"]
                ;;  "zmdi-view-web"]

                [edit-flow-title flow-id w]

                ;; [flow-details-block-container "run history bar" flow-id flow-select
                ;;  [run-history-bar "system"]
                ;;  "zmdi-view-web"]

                [flow-details-block-container "options*" :system :system
                 [code-box-options flow-id 580 nil opts-map]
                 "zmdi-code"]

                (when (not gantt?)
                  [flow-details-block-container "in-line flow gantt" :system :system
                   [gantt-container flow-id orderb true 570 nil ;366
                    ]"zmdi-chart-donut"])

                [flow-details-block-container "server flow statuses" :system :system
                 [server-flows] ; flow-id orderb true 570 nil ;366
                 "zmdi-dns"]

;                [run-gantt-chart flow-id]
                ;; [edit-flow-title flow-id w]
;;                [settings-block flow-id]

                [flow-details-block-container "selected module*" :system :system
                 [settings-block flow-id] "zmdi-chart-donut"]]

                ;; [run-history-bar flow-id]
     ]))

(defn waffles [data w]
  (let [data (into [] (for [[k v] @(re-frame/subscribe [::flowmap])]
                        {:name k :type (get-in v [:ports :out :out]) :number 1}))
                        ;[k {:name k :type (get-in v [:ports :out :out]) :number 1}]

        _ (tap> [:waffled-data data])
        square-size 20
        max-width w
        num-squares-max-width (quot max-width square-size)
        data-length (count data)
        num-squares-height (int (Math/ceil (/ data-length num-squares-max-width.toDouble)))
        chart-width (if (< data-length num-squares-max-width)
                      (* data-length square-size)
                      max-width)
        ;; chart-data (map-indexed (fn [i {:keys [name type number]}]
        ;;                           {:row (quot i num-squares-max-width)
        ;;                            :col (mod i num-squares-max-width)
        ;;                            :filled type
        ;;                            :number number
        ;;                            :tooltip name}) data)
        chart-data (map-indexed (fn [i {:keys [name type number]}]
                                  {:row (quot i num-squares-max-width)
                                   :col (mod i num-squares-max-width)
                                   :filled type
                                   :number number
                                   :tooltip name}) data)
        ccolors (theme-pull :theme/data-colors db/data-colors)
        _ (tap> [:waffle-chart-data chart-data])]
    [:vega-lite
     {:width chart-width
      :height (* square-size num-squares-height)
      :layer [; modify chart to use layer
              {:data {:values chart-data}
               :mark "rect"
               :encoding
               {:y {:field "row"
                    :type "ordinal"
                    :scale {:rangeStep square-size}
                    :axis nil}
                :x {:field "col"
                    :type "ordinal"
                    :scale {:rangeStep square-size}
                    :axis nil}
                :fill {:field "filled"
                       :type "nominal"
                       :scale {:domain (keys ccolors)
                               :range (vals ccolors)}
                       :legend nil}
                :tooltip {:field "tooltip"
                          :type "nominal"}}}
              {:data {:values chart-data}
               :mark {:type "text"
                      :font "Poppins"
                      :fontSize 9}
               :encoding
               {:color {:value "black"}
                :opacity {:value 0.4}
                :y {:field "row"
                    :type "ordinal"
                    :scale {:rangeStep square-size}}
                :x {:field "col"
                    :type "ordinal"
                    :scale {:rangeStep square-size}}
                :text {:field "number"
                       :type "quantitative"
                       :condition {:test "datum.number <= 1"
                                   :value ""}}}}]
      :background "transparent"
      :config {:view {:stroke "transparent"}}}
     {:actions false}]))

(re-frame/reg-sub
 ::get-raw-port-types-set
 (fn [db [_ flow-id bid dir pid]]
   (let [vv (get-in db [:flows flow-id :map bid :ports dir pid] :any)]
     (set (if (keyword? vv) [vv] vv)))))

(re-frame/reg-event-db
 ::set-raw-port-types
 (fn [db [_ flow-id bid dir pid types]]
   (let [types (vec types) ;;(vec (remove #(= % :none) (remove empty? (vec types))))
         types (if (= (count types) 1) (first types) types)]
         ;types (if (not (empty? types)) types :any)

     (tap> [:set-port-type [_ flow-id bid dir pid] :to types])
     (assoc-in db [:flows flow-id :map bid :ports dir pid] types))))

(re-frame/reg-sub
 ::get-incoming-port
 (fn [_ [_ bid type vk]]
   (let [connections @(re-frame/subscribe [::flowmap-connections-parsed bid])
         conn-map (group-by first connections)
         conns (for [[o1 o2 i1 i2] (map rest (get conn-map type))
                     :when (if (= type :in) (= vk i2) (= vk o2))]
                 (if (= type :out)
                   [i1 i2] [o1 o2]))]
     (first conns))))

(re-frame/reg-sub
 ::get-outgoing-port
 (fn [_ [_ bid type vk]]
   (let [connections @(re-frame/subscribe [::flowmap-connections-parsed bid])
         conn-map (group-by first connections)
         filled (filter #(= % [bid vk]) (for [[b k _ _] (map rest (get conn-map type))] [b k]))]
        ;;  _ (when (= bid :unpack-results-map)
        ;;      (tap> [:parsed-connections
        ;;             type filled?
        ;;             conn-map [vk bid]
        ;;             (for [[b k _ _] (map rest (get conn-map type))] [b k])]))
        ;;  conns (for [[o1 o2 i1 i2] (map rest (get conn-map type))
        ;;              :when (if (= type :in) (= vk i2) (= vk o2))]
        ;;          (if (= type :out)
        ;;            [i1 i2] [o1 o2]))

     filled)))

;; (re-frame/reg-event-db
;;  ::re-order-connection
;;  (fn [db [_ conn direction]] ;; direction = :up or :down

;;    (let [conn (vec (map #(cstr/replace (str %) ":" "") conn))
;;          conn [(keyword (str (first conn) (when (not= (second conn) "out") (str "/" (second conn)))))
;;                (keyword (str (nth conn 2) "/" (nth conn 3)))] ;; item to match what is in conns
;;          conns (get-in db [:flows (get db :selected-flow) :connections])

;;          ]
;;      (tap> [:re-order direction conn conns])
;;      db)))

(defn index-of [coll item]
  (first (keep-indexed #(when (= %2 item) %1) coll)))

(defn swap [coll i j]
  (-> coll
      (assoc i (coll j))
      (assoc j (coll i))))

(re-frame/reg-event-db
 ::re-order-connection
 (fn [db [_ conn direction]] ;; direction = :up or :down
   (let [conn (vec (map #(cstr/replace (str %) ":" "") conn))
         conn [(keyword (str (first conn) (when (not= (second conn) "out") (str "/" (second conn)))))
               (keyword (str (nth conn 2) "/" (nth conn 3)))] ;; item to match what is in conns
         conns (get-in db [:flows (get db :selected-flow) :connections])
         idx (index-of conns conn)] ;; find the index of conn in conns

     (if (nil? idx)
       db
       (cond
         (= direction :up) (when (> idx 0)
                             (assoc-in db [:flows (get db :selected-flow) :connections]
                                       (swap conns idx (dec idx))))
         (= direction :down) (when (< idx (dec (count conns)))
                               (assoc-in db [:flows (get db :selected-flow) :connections]
                                         (swap conns idx (inc idx))))
         :else db)))))

(re-frame/reg-sub
 ::get-meta
 (fn [db [_ bid]]
   (get-in db [:flows (get db :selected-flow) :map bid :data :flow-item :meta])))

(re-frame/reg-sub
 ::is-open-in?
 (fn [db [_ bid]]
   (let [tt (get-in db [:flows (get db :selected-flow) :map bid :data :drag-meta :type])]
     (true? (= tt :open-block)))))

(re-frame/reg-sub
 ::condi-ports
 (fn [db [_ bid]]
   (get-in db [:flows (get db :selected-flow) :map bid :cond] {})))

(re-frame/reg-sub
 ::push-ports
 (fn [db [_ bid]]
   (get-in db [:flows (get db :selected-flow) :map bid :push] {})))

(re-frame/reg-event-db
 ::add-push-port
 (undoable)
 (fn [db [_ bid]]
   (let [fn '(fn [out] (not (nil? out)))
         ckeys (vec (keys (get-in db [:flows (get db :selected-flow) :map bid :push])))
         name (ut/safe-key :push-path ckeys)]
     (assoc-in db [:flows (get db :selected-flow) :map bid :push name]
               {:fn fn :dest []}))))

(re-frame/reg-event-db
 ::remove-push-port
 (undoable)
 (fn [db [_ bid name]]
   (-> db (assoc-in [:flows (get db :selected-flow) :connections]
                    (vec (remove #(cstr/ends-with? (str (first %)) (str "/" (cstr/replace (str name) ":" "")))
                                 (get-in db [:flows (get db :selected-flow) :connections]))))
       (ut/dissoc-in [:flows (get db :selected-flow) :map bid :push name]))))

(re-frame/reg-event-db
 ::add-condi-port
 (undoable)
 (fn [db [_ bid]]
   (let [fn '(fn [out] (not (nil? out)))
         ckeys (vec (keys (get-in db [:flows (get db :selected-flow) :map bid :cond])))
         name (ut/safe-key :cond-path ckeys)]
     (assoc-in db [:flows (get db :selected-flow) :map bid :cond name]
               {:fn fn :dest []}))))

(re-frame/reg-event-db
 ::edit-condi-port
 (undoable)
 (fn [db [_ bid name fn]]
   (assoc-in db [:flows (get db :selected-flow) :map (get db :selected-flow-block) :cond name]
             {:fn fn :dest []})))

(re-frame/reg-event-db
 ::edit-view-fn
 (undoable)
 (fn [db [_ bid fn]]
   ;(tap> [:bid bid])
   (assoc-in db [:flows (get db :selected-flow) :map (get db :selected-flow-block) :view] fn)))

(re-frame/reg-event-db
 ::remove-condi-port
 (undoable)
 (fn [db [_ bid name]]
   (-> db (assoc-in [:flows (get db :selected-flow) :connections]
                    (vec (remove #(cstr/ends-with? (str (first %)) (str "/" (cstr/replace (str name) ":" "")))
                                 (get-in db [:flows (get db :selected-flow) :connections]))))
       (ut/dissoc-in [:flows (get db :selected-flow) :map bid :cond name]))))

(defn port-panel [flow-id bid fmap type]
  (let [;blocks @(re-frame/subscribe [::flowmap])
        ;flow-id @(re-frame/subscribe [::selected-flow])
        ;flow-select @(re-frame/subscribe [::selected-flow-block])
        ;connections @(re-frame/subscribe [::flowmap-connections])
        reactions! [@(re-frame/subscribe [::http/flow-results]) @sniffy-sniff @port-hover2]
        [xx yy] @detached-coords
        selected (get-in @flow-details-block-container-atom [flow-id bid :port-panel type])
        connections @(re-frame/subscribe [::flowmap-connections-parsed bid])
        defaults  (when (= type :in) @(re-frame/subscribe [::input-defaults bid]))
        port-meta (when (= type :in) @(re-frame/subscribe [::get-meta bid]))
        is-open-in? @(re-frame/subscribe [::is-open-in? bid])
        ;; scrubber-opts-map (into {} (for [[k v] port-meta
        ;;                                  :when (map? (get v :scrubber))]
        ;;                              {[k] (get v :scrubber)}))
        conn-map (group-by first connections)
        inputs-vec (try
                     (edn/read-string (get-in fmap [:data :flow-item :inputs]))
                     (catch :default _ (get-in fmap [:data :flow-item :inputs])))]
                     ;; important for keeping defined order!

    ;;(tap> [:parsed-connections conn-map (map rest (get conn-map type))])
    [re-com/v-box
     :gap "8px"
     :children
     [[re-com/v-box
       :style {:background "#00000055"
               :padding-top "8px"
               :padding-bottom "8px"
               :border-radius "8px"}
       :children (for [[e v] (get fmap :ports)
                       :when (= type e)]
                   [re-com/v-box
                  ;:style {:background-color "maroon"}
                  ;:padding "4px"
                  ;:gap "4px"
                    :children
                    (into [[re-com/h-box
                            :justify :between
                            :align :center
                            :height "17px"
                            :style {:margin-left "4px"
                                    :font-size "10px"
                                    :font-weight 300
                                    :opacity 0.35}
                            :children [[re-com/box :child "name" :padding "4px"
                                      ;:style {:border "1px solid lime"}
                                        :width "160px"]
                                       [re-com/box
                                        :child (if (= type :in) "allowed" "expected")
                                        :padding "4px"
                                      ;:style {:border "1px solid lime"}
                                      ;:style {:margin-left "4px"}
                                        :width "124px"]
                                       [re-com/box :padding "4px"
                                      ;:style {:border "1px solid lime"}
                                        :width "203px"
                                        :child (if (= type :in) "coming from" "going to")]
                                       [re-com/box
                                        :child "actual"
                                      ;:style {:border "1px solid lime"}
                                        :width "60px"
                                        :padding "4px"]]]]
                          (for [[vk vv] (if (= type :in) (sort-map-keys (vec (or inputs-vec (sort-by str (keys v)))) v)
                                          ;v
                                            (sort-map-keys (try (sort-by str (keys v)) (catch :default e (do (tap> [:fucking-sorting-error e :flows.cljs :ln 4216 (keys v)]) (keys v)))) v))

                                :let [select-vec [bid vk]
                                      selected? (or (and @sniffy-sniff (= @port-hover2 bid)
                                                         (= vk (get @sniffy-sniff 1)))
                                                        ;; hover from canvas sniffer
                                                    (= select-vec selected))
                                      default (get defaults vk) ;;(walk/postwalk-replace {:cline} (get defaults vk))
                                      default? (not (nil? default))
                                      scrubber (get-in port-meta [vk :scrubber])
                                    ;_ (tap> [:port-meta port-meta scrubber])
                                      desc (get-in port-meta [vk :desc])
                                    ;_ (when selected? (tap> [:inputs? (get-in fmap [:data :flow-item :inputs] (keys v))]))
                                    ;_ (tap> [:sniff bid @sniffy-sniff @port-hover2 vk])

                                    ;react? [@(re-frame/subscribe [::get-raw-port-types flow-id bid type vk])]
                                    ;vv @(re-frame/subscribe [::get-raw-port-types flow-id bid type vk])
                                      [ibid ivk] (if (= type :in) @(re-frame/subscribe [::get-incoming-port bid type vk]) [nil nil])

                                      pcolor @(re-frame/subscribe [::port-color flow-id (or ibid bid) (or ivk vk)])
                                      ptype @(re-frame/subscribe  [::port-color flow-id (or ibid bid) (or ivk vk) :data-type])
                                      pval1 @(re-frame/subscribe  [::port-color flow-id (or ibid bid) (or ivk vk) :data-value])

                                    ;; ttoggle #(swap! flow-details-block-container-atom assoc-in [flow-id bid :port-panel type]
                                    ;;                 (if (= selected select-vec) nil select-vec))
                                      vopen? (true? (get-in @flow-details-block-container-atom [flow-id bid :port-values type vk :open?] false))
                                      vttoggle #(swap! flow-details-block-container-atom assoc-in [flow-id bid :port-values type vk :open?] (not vopen?))
                                      conns (for [[o1 o2 i1 i2] (map rest (get conn-map type))
                                                  :when (if (= type :in) (= vk i2) (= vk o2))]
                                              (if (= type :out)
                                                [i1 i2] [o1 o2]))
                                      conn-list (for [[a _] conns] a)
                                      connected? (not (empty? conns))
                                      involved (vec (distinct (conj conn-list bid)))
                                      multi? (cstr/ends-with? (str vk) "+")
                                    ;;_ (tap> [:involved involved])
                                      hover-bundle-only {:on-mouse-enter #(do
                                                                            (swap! flow-details-block-container-atom assoc-in [flow-id :involved] involved)
                                                                            (swap! flow-details-block-container-atom assoc-in [flow-id bid :port-panel type] select-vec))
                                                         :on-mouse-leave #(do
                                                                            (swap! flow-details-block-container-atom assoc-in [flow-id :involved] [])
                                                                            (swap! flow-details-block-container-atom assoc-in [flow-id bid :port-panel type] nil))}
                                      hover-bundle-clicks {:on-click (when true ;(= type :in)
                                                                       vttoggle)
                                                           :on-double-click (when (not (empty? conn-list)) #(select-block (first conn-list)))}
                                      ;hover-bundle (merge hover-bundle-only hover-bundle-clicks)
                                      ]]
                            [re-com/v-box
                           ;:attr hover-bundle
                             :attr hover-bundle-only
                             :style {:border-radius "6px"
                                     :border (if selected?
                                               (str "1px solid " pcolor) ;(theme-pull :theme/editor-outer-rim-color nil))
                                               "1px solid transparent")}
                             :children
                             [[re-com/h-box
                           ;:padding "2px"
                          ;;  :attr {:on-click #(swap! flow-details-block-container-atom assoc-in [flow-id bid :port-panel type]
                          ;;                           (if (= selected select-vec) nil select-vec))}
                               :attr hover-bundle-clicks
                               :style {;:margin-left "4px"
                                       ;:margin-right "4px"
                                       :border-radius "6px" ;(when selected? "6px")
                                       :cursor "pointer"
                                       :color (if selected? "#ffffff"
                                                  "#ffffff80")

                                     ;:background-color (str (get (theme-pull :theme/data-colors db/data-colors)
                                     ;                            (cstr/replace (str vv) ":" "") "black") 19)
                                     ;:background-color (theme-pull :theme/editor-outer-rim-color nil)
                                       :background-color (str pcolor
                                                              (if (and (not connected?) (not selected?) (= type :out))
                                                                "18" "30"))}
                             ;:justify :between
                               :align :center
                               :size "auto"
                               :min-height "42px"
                               :children [[re-com/box
                                           :child (str vk)
                                           :padding "4px"
                                        ;; :attr {:on-click ttoggle}
                                       ;:style {:text-decoration "underline"}
                                       ;:style {:border "1px solid lime"}
                                           :style {:font-size "17px"
                                                 ;;:text-decoration (when (not connected?) "underline")
                                                   :opacity (if (and (not connected?) (not selected?) (= type :out))
                                                              0.4 1.0)
                                                   :overflow "hidden"
                                                   :margin-left "3px"
                                                 ;:border "1px solid lime"
                                                   :font-weight 700}
                                           :width "160px"]
                                          [re-com/box
                                        ;; :attr (if (not selected?) {:on-click ttoggle} {})
                                       ;:justify :center
                                           :child (if true ;selected?
                                                ;; [re-com/single-dropdown
                                                ;;  :width "95px"
                                                ;;  ;:style {:z-index 999}
                                                ;;  :model ":any" :choices [{:id ":any" :label ":any"} {:id ":string" :label ":string"}] :on-change #()]
                                                    [re-com/tag-dropdown ;:width "95px"
                                                ;;  :model (if (keyword? ptype)
                                                ;;           (set [ptype])
                                                ;;             (set ptype)) ;; dumb re-com shit
                                                     :model (re-frame/subscribe [::get-raw-port-types-set flow-id bid type vk])
                                                     :parts {;:selection-list {:style {:margin-top (px (+ (* -1 yy) -45))
                                                         ;                         :margin-left (px (+ (* -1 xx) -20))
                                                         ;                         ;:position "relative"
                                                         ;                         ;:position "absolute"
                                                         ;                         ;:top yy :left xx
                                                         ;                         ;:width "200px"
                                                         ;                         }}
                                                         ;:radio-button {:style {:background-color "green"}}
                                                         ;:popover-anchor-wrapper {:style {:width "10px"}}
                                                             :main {:style {:background-color "#00000000" ;(str (theme-pull :theme/editor-rim-color nil) "11")
                                                                            :color (theme-pull :theme/editor-font-color nil)
                                                                            :border (str "0px solid " (theme-pull :theme/editor-outer-rim-color nil))
                                                                            :outline "none"}}
                                                         ;:tags {:style {:color "black"}}
                                                         ;:tags {:style {:min-width "10px"}}
                                                             :list-group-item {:style {:background-color (str (theme-pull :theme/editor-rim-color nil) "99")
                                                                                       :backdrop-filter "blur(4px)"
                                                                                       :border (str "1px solid " (theme-pull :theme/editor-outer-rim-color nil) "44")
                                                                                       :color (theme-pull :theme/editor-font-color nil)}} ;; popover
                                                             :list-group {:style {:margin-top (px (+ (* -1 yy) -45))
                                                                                  :margin-left (px (+ (* -1 xx) -20))
                                                                                  :background-color "#00000000"}}}
                                                     :abbrev-fn (fn [m] [re-com/box
                                                                         :child (subs (str (get m :label)) 1 2)
                                                                         :style {:font-weight 700
                                                                                 :color (get m :color)}])
                                                     :required? true
                                                     :max-width "130px"
                                                 ;:min-width "10px"
                                                     :disabled? (not selected?)
                                                 ;:height "100px"
                                                ;;  :description-fn (fn [m] [re-com/box
                                                ;;                           :child (str "data type of "
                                                ;;                                       (cstr/replace (str (get m :label)) ":" "")
                                                ;;                                       " according to Clojure(script) conventions")
                                                ;;                           :style {;:font-weight 700
                                                ;;                                   :color (str (ut/invert-hex-color (theme-pull :theme/editor-font-color nil)) 89)}])
                                                     :label-fn (fn [m] [re-com/box
                                                                        :child (str (get m :label))
                                                                        :style {:font-weight 700
                                                                                :color (get m :color)}])
                                                 ;;:short-label-fn (fn [m] (str (get m :id)))
                                                     :style {:font-size   "12px"
                                                             :font-family (theme-pull :theme/base-font nil)
                                                             :color       (theme-pull :theme/editor-font-color nil)}
                                                     :abbrev-threshold 10
                                                     :choices (vec (for [[k v] (ut/sort-map-by-key
                                                                                (merge {"any" "#c32148"}
                                                                                       (theme-pull :theme/data-colors db/data-colors)))]
                                                                     {:id (keyword k)
                                                                      :label (str (keyword k))
                                                                    ;:color (ut/invert-hex-color v)
                                                                      :color (ut/choose-text-color v)
                                                                      :background-color v}))
                                                     :on-change #(re-frame/dispatch [::set-raw-port-types flow-id bid type vk %])]
                                                    (str vv))
                                           :padding "4px"
                                       ;:style (if (not selected?) {:margin-left "9px"} {})
                                       ;:style {:border "1px solid lime"}
                                           :width "130px"]

                                          (if (not (empty? conns))
                                            [re-com/v-box
                                             :padding "4px"
                                          ;; :attr {:on-click ttoggle}
                                          ;;  :style (when multi? {:border "1px solid lime"})
                                             :width "200px"
                                             :children (for [e conns]
                                                         [re-com/box :child (str e)])]
                                            (if default?
                                              [re-com/box
                                               :size "none"
                                             ;:align :start
                                               :width "200px"
                                               :style {;:margin-right "8px"
                                                       :font-size "9px"
                                                       :opacity 0.45
                                                     ;:border "1px solid green"
                                                       }
                                               :child "(has a default value)"]
                                              [re-com/box
                                               :width "200px"
                                             ;:style {:border "1px solid green"}
                                          ;; :attr {:on-click ttoggle}
                                               :child [re-com/md-icon-button
                                                       :style {:font-size "10px"
                                                               :color pcolor
                                                               :margin-right "8px"}
                                                       :md-icon-name "zmdi-close"]]))

                                          [re-com/box
                                           :child (str ptype)
                                           :width "45px" :size "none" ;:height "24px"
                                           :style {:font-size "9px"
                                                ; :border "1px solid green"
                                                   :color pcolor}
                                           :padding "2px"]
                                        ;; :attr {:on-click ttoggle}

                                       ;:style {:border "1px solid lime"}
                                       ;:width "70px"

                                          [re-com/v-box
                                           :width "20px" :size "none"
                                           :style {:margin-top "3px"
                                                 ;:border "1px solid lime"
                                                   }
                                        ; :attr (when (= type :in) {:on-click vttoggle})
                                           :children [[re-com/md-icon-button
                                                       :style {:font-size "13px"
                                                               :color pcolor
                                                               :margin-right "8px"}
                                                       :md-icon-name (cond
                                                                       default? "zmdi-dot-circle-alt"
                                                                       (empty? conns) "zmdi-circle-o"
                                                                       :else "zmdi-circle")]
                                                    ;; (when multi?
                                                    ;;   [re-com/v-box :children
                                                    ;;    [[re-com/md-icon-button
                                                    ;;      :style {:font-size "13px"
                                                    ;;              :margin-top "-16px"
                                                    ;;              :margin-right "8px"
                                                    ;;              :color pcolor}
                                                    ;;      :md-icon-name "zmdi-chevron-up"]
                                                    ;;     [re-com/md-icon-button
                                                    ;;      :style {:font-size "13px"
                                                    ;;              :margin-top "-16px"
                                                    ;;              :margin-right "8px"
                                                    ;;              :color pcolor}
                                                    ;;      :md-icon-name "zmdi-chevron-down"]]])
                                                      ]]]]
                              (when true ;(= type :in)
                              ;;(tap> [:pp1 bid default?  conns vk type pval1 ])
                                (for [conn (cond
                                             (and (= type :in) (empty? conns)) [nil]
                                             (= type :in) conns
                                             :else [(first conns)])]
                                  (let [[b1 p1] conn ;(first conns) ;; discarding others, since we can only get one data per port
                                        pval (if (= type :in)
                                               @(re-frame/subscribe [::port-color flow-id b1 p1 :data-value])
                                               pval1)
                                        c-line (vec (into conn [bid vk]))
                                      ;_ (when (= type :in) (tap> [:conn c-line]))
                                        show-default? (and (nil? pval)
                                                           (empty? conn) ;conns ;(empty? (remove nil? conns))
                                                           default?)
                                        show-scrubber? (and scrubber default? show-default?)
                                        pval (if show-default? default pval)]
                                  ;(tap> [:pp bid default? show-default? conns vk type pval1 b1 p1 pval])
                                ;(tap> [:ss @(re-frame/subscribe [::bricks/keypaths-in-flow bid])])
                                ;(when (nil? pval) (tap> [:pp bid vk type pval1 b1 p1 pval]))
                                    (when vopen?
                                      [re-com/v-box
                                       :children [[re-com/h-box
                                                   :children [[re-com/box
                                 ;:align :center :justify :center
                                                               :width "535px"
                                                               :style {;:border-radius "9px"
                                                                       :padding-top "5px"
                                                                     ;:border "1px solid white"
                                                                       :opacity (when show-default? 0.4)
                                                                       :padding-bottom "5px"}

                                                               :child

                                                               (let [pval (if (or (vector? pval) (map? pval) (list? pval)) (ut/replace-large-base64 pval) pval)
                                                                     b64? (ut/is-base64? (str pval))]
                                                                 (if (and ;(try (not (empty? pval)) (catch :default _ false))
                                                                      b64?)
                                                                   [re-com/v-box
                                                                    :padding "16px"
                                                                ;:width "400px"
                                                                    :size "auto"
                                                                    :gap "10px"
                                                                    :children
                                                                    [[re-com/box :child (str "(**base64 string: ~" (.toFixed (/ (* (count (str pval)) 6) 8 1024 1024) 2) " MB)")]
                                                                     [re-com/box
                                                                      :size "auto"
                                                                      :child [:img {:src (str "data:image/png;base64," pval)}]]]]
                                                                   [code-box 555 nil (pr-str pval)]))]

                                                              [re-com/v-box
                                                               :width "31px"
                                                               :align :center :justify :center
                                               ;:style {:border "1px solid white"}
                                                               :children [[re-com/md-icon-button
                                                                           :attr {:on-click #(re-frame/dispatch [::re-order-connection c-line :up])}
                                                                           :style {:font-size "12px"
                                                                                 ;:margin-top "4px"
                                                                                   :color (str (theme-pull :theme/editor-font-color nil) "75")}
                                                                           :md-icon-name "zmdi-chevron-up"]
                                                                          [re-com/md-icon-button
                                                                           :attr {:on-click #(re-frame/dispatch [::re-order-connection c-line :down])}
                                                                           :style {:font-size "12px"
                                                                                 ;:margin-bottom "4px"
                                                                                   :color (str (theme-pull :theme/editor-font-color nil) "75")}
                                                                           :md-icon-name "zmdi-chevron-down"]]]]]

                                                  (when show-scrubber? ;; scrubber?
                                                         ;;(tap> [:kk @(re-frame/subscribe [::bricks/keypaths-in-flow bid])])

                                                    [re-com/box
                                                          ;:padding "4px"
                                                     :style {:padding-top "8px" :padding-bottom "8px"}
                                                     :child [bricks/scrubber-panel true
                                                             @(re-frame/subscribe [::bricks/keypaths-in-flow bid])
                                                             [:flow bid] vk ;[:flow-item :defaults vk] ;;kk pm
                                                                  ;:flow bid
                                                             (if (map? scrubber)
                                                               {:fm true
                                                                :opts scrubber
                                                                :canvas? true
                                                                :flow? true}
                                                               {:fm true
                                                                :canvas? true
                                                                :flow? true})]]

                                                        ;;  [buffy/render-honey-comb-fragments
                                                        ;; ;[:box :child "hey"]
                                                        ;; [:scrubber [0] ]
                                                        ;; 2 2]
                                                    )
                                                  (when desc ;; :meta :desc for the port
                                                    [re-com/box
                                                     :style {:margin-left "15px"
                                                             :font-weight 400
                                                             :color (str (theme-pull :theme/editor-font-color nil) 55)
                                                                           ;:border "1px solid white"
                                                             :padding-bottom "10px"}
                                                     :width "535px"
                                                     :child (str desc)])

                                                       ;;  scrubber-panel [view? keypath-map view-key & [wildcard full-map]]
                                                  ]]))))]]))])]

      (when (and (= type :out) (not is-open-in?)) ;; if we can have conditional ports!

        (let [condi-ports @(re-frame/subscribe [::condi-ports bid])
              cnt (count (keys condi-ports))
              open? (true? (get-in @flow-details-block-container-atom [flow-id bid :condi-paths :open?] false))]
          [re-com/v-box
                                  ; :padding "8px"
                                 ;:style {:border "1px solid white"}
           :style {:background "#00000055"
                   :padding-top "8px"
                                           ;:padding-bottom (when open? "8px")
                                           ;:padding-left "5px"
                                           ;:padding-right "5px"
                   :border-radius "8px"}
           :children [[re-com/h-box
                       :size "auto"
                       :padding "4px"
                       :justify :between :align :center
                       :style {:color ;; (str (theme-pull :theme/editor-outer-rim-color nil) 88)
                               (if open?
                                 (str (theme-pull :theme/editor-outer-rim-color nil) 99)
                                 (str (theme-pull :theme/editor-outer-rim-color nil) 45))
                               :cursor "pointer"}
                       :children [[re-com/h-box
                                   :align :center :justify :center
                                   :style {:padding-left "3px"}
                                   :gap "5px" ;:style {;:margin-left "-3px"
                                                                      ;        ;:opacity (if (> cnt 0) 1 0.4)
                                                                      ;        }
                                   :attr {:on-click #(swap! flow-details-block-container-atom assoc-in [flow-id bid :condi-paths :open?] (not open?))}
                                   :children [[re-com/md-icon-button
                                               :src (at)
                                                                       ;:on-click #(re-frame/dispatch [::add-condi-port bid])
                                               :md-icon-name "fa-solid fa-tree" ;; <i class= "fa-solid fa-bezier-curve" ></i> <i class= "fa-solid fa-tree" ></i>
                                               :style {;:color (theme-pull :theme/editor-outer-rim-color nil)
                                                       :font-size "17px"
                                                       :cursor "pointer"}]
                                              [re-com/box :child (str "conditional paths (" cnt ")")]]]
                                  [re-com/md-icon-button
                                   :src (at)
                                   :on-click #(re-frame/dispatch [::add-condi-port bid])
                                   :md-icon-name "zmdi-plus"
                                   :style {;:color (theme-pull :theme/editor-outer-rim-color nil)
                                           :font-size "22px"
                                           :cursor "pointer"}]]]

                      (when (and open? (> cnt 0))
                        [re-com/gap :size "8px"])

                      (when (and open? (> cnt 0))
                        [re-com/v-box
                                               ;:padding "4px"
                         :style {;:border "1px solid #ffffff33"
                                 :border-radius "8px"
                                 :background-color "#00000044"
                                                       ;:background-color "maroon"
                                 }
                         :gap "8px"
                         :padding "4px"
                         :children (for [[k {:keys [fn dest]}] condi-ports
                                         :let [select-vec [bid k]
                                               valve-open? @(re-frame/subscribe [::bricks/condi-valve-open? flow-id bid k])
                                               selected? (or (and @sniffy-sniff (= @port-hover2 bid)
                                                                  (= k (get @sniffy-sniff 1)))
                                                                                     ;; hover from canvas sniffer
                                                             (= select-vec selected))
                                               pcolor (theme-pull :theme/editor-outer-rim-color nil) ;; @(re-frame/subscribe [::port-color flow-id bid :out])
                                                                       ;; TODO, ^^ will it ALWAYS be out? I think so... ? for out ports, non open
                                               ]]
                                     [re-com/v-box
                                      :padding "4px"
                                      :attr {:on-mouse-enter #(do
                                                                                        ;(swap! flow-details-block-container-atom assoc-in [flow-id :involved] involved)
                                                                (swap! flow-details-block-container-atom assoc-in [flow-id bid :port-panel type] select-vec))
                                             :on-mouse-leave #(do
                                                                                        ;(swap! flow-details-block-container-atom assoc-in [flow-id :involved] [])
                                                                (swap! flow-details-block-container-atom assoc-in [flow-id bid :port-panel type] nil))}
                                                            ;:padding "4px"
                                      :style {:border (if (or selected? valve-open?)
                                                        (str "1px solid " pcolor) ;(theme-pull :theme/editor-outer-rim-color nil))
                                                        (str "1px solid "  pcolor 25))
                                              :background-color (if (or selected? valve-open?)
                                                                  (str pcolor "22") (str pcolor "08"))
                                              :border-radius "6px"}
                                      :gap "8px"
                                      :children
                                      [[re-com/h-box
                                                              ;:padding "4px"
                                        :size "auto"
                                        :justify :between :align :center
                                        :style {:color (str (theme-pull :theme/editor-outer-rim-color nil) 88)
                                                :padding-left "4px"
                                                :padding-right "1px"
                                                :padding-top "4px"
                                                                    ;:padding-bottom "4px"
                                                                                 ;:border "1px solid white"
                                                }
                                        :children [[re-com/box
                                                    :style {:font-size "15px" :font-weight 700}
                                                    :child (str k)]
                                                   [re-com/h-box
                                                    :width "300px" :height "100%"
                                                    :justify :between :align :center
                                                                            ;:style {:border "1px solid green"}
                                                    :children [[re-com/v-box :children (for [d dest] [re-com/box :child (str d)])]
                                                                                   ;[re-com/box :child (str dest)]
                                                               [re-com/md-icon-button :src (at)
                                                                :md-icon-name "zmdi-close"
                                                                :on-click #(re-frame/dispatch [::remove-condi-port bid k])
                                                                :style {;:margin-top "-5px"
                                                                        :cursor "pointer"
                                                                                           ; :margin-right "-3px"
                                                                                            ;:height "12px"
                                                                        :font-size "16px"}]]]]]
                                       [re-com/h-box
                                                                ;:style {:border "1px solid cyan"}
                                        :justify :between :align :center
                                        :children  ;;(str fn)
                                        [;[code-box 500 nil fn]
                                         [code-box-condi 500 nil fn bid k]
                                         [re-com/box
                                          :justify :center :align :center
                                          :width "40px" :size "none" :height "100%"
                                          :style {;:border "1px solid yellow"
                                                  :color (theme-pull :theme/editor-outer-rim-color nil)
                                                  :background-color (str (theme-pull :theme/editor-outer-rim-color nil) 11)
                                                  :font-weight 700
                                                  :border-radius "8px"
                                                  :opacity (if valve-open? 1 0.4)}
                                          :child (str valve-open?)]]]]])])]]))]]))
                                ;[re-com/box :child (str pval)]


(defn debug-box [flow-select raw-fmap]
  (let [read-only-flow? (true? (cstr/includes? (str flow-select) "/"))]
    [re-com/box :child [(if read-only-flow? code-box code-box-rw) 600 nil ;440
                                    ;(get @(re-frame/subscribe [::flowmap]) @flow-select)
                        raw-fmap ;(get fmap flow-select)
                        flow-select]
     :width "600px"]))

(defn channel-box [flow-id flow-select val]
  (let [read-only-flow? (true? (cstr/includes? (str flow-select) "/"))
        connections (filter #(= flow-select (second %)) @(re-frame/subscribe [::flowmap-connections-parsed flow-select]))
        cc (count connections)]
    [re-com/v-box
     :gap "10px"
     :children [[re-com/h-box :width "570px"
                 :style {;:border "1px solid white"
                         :color (str (theme-pull :theme/editor-outer-rim-color nil) 88)}
                 :size "auto" :padding "4px"
                 :align :center :justify :center
                 :gap "10px"
                 :children [[re-com/md-icon-button :md-icon-name "fa-solid fa-radiation" :style {:font-size "14px" :color (theme-pull :theme/editor-outer-rim-color nil)}]
                            [re-com/box :child "(use to push a value downstream manually - ATTN: be careful. lol)"]
                            [re-com/md-icon-button :md-icon-name "fa-solid fa-radiation" :style {:font-size "14px" :color (theme-pull :theme/editor-outer-rim-color nil)}]]]
                [re-com/box
                 :width "570px"
                 :style {:border (str "3px dashed " (theme-pull :theme/editor-outer-rim-color nil) 66)
                         :border-radius "12px"}
                 :child [code-box-channels 570 nil
                 ;(get raw-fmap :raw-fn "(fn [x] x)")
                 ;last-output
                         (pr-str val)
                         flow-id flow-select]]
                [re-com/box
                 :width "570px"
                 :style {:border (str "1px solid " (str (theme-pull :theme/editor-outer-rim-color nil) 66))
                         :border-radius "10px"
                         :font-size "18px"
                         :background-color (str (theme-pull :theme/editor-outer-rim-color nil) 25)}
                 :size "auto" :padding "4px"
                 :align :center :justify :center
                 :child [re-com/h-box
                         :gap "10px"
                         :attr {:on-click #(re-frame/dispatch [::bricks/push-value flow-id flow-select val true])}
                         :style {:color (theme-pull :theme/editor-outer-rim-color nil) :cursor "pointer"}
                         :padding "6px"
                         :children [[re-com/box :child (str "I understand - push to " cc " channel" (when (> cc 1) "s"))]
                                    [re-com/md-icon-button :md-icon-name "fa-solid fa-gun" :style {:font-size "18px"}]
                                    ;; <i class= "fa-solid fa-gun" ></i>
                                    ;; <i class= "fa-solid fa-radiation" ></i>
                                    ]]]
                ;;[re-com/box :child (str connections)]
                ;; [re-com/v-box
                ;;  :children (for [c connections]
                ;;              [re-com/box
                ;;               :size "auto"
                ;;               :style {:border "1px solid white"}
                ;;               :padding "4px"
                ;;               :child (str (get c 3) (get c 4))])]
                ]

     :width "600px"]))

(re-frame/reg-sub
 ::get-raw-data-input
 (fn [db [_ flow-id bid]]
   (get-in db [:flows flow-id :map bid :data :user-input])))

(re-frame/reg-sub
 ::get-raw-block-map
 (fn [db [_ flow-id bid]]
   (get-in db [:flows flow-id :map bid])))

(defn flow-code-editor-block [w h fmap flow-select ttype flow-id]
  (let [read-only-flow? (true? (cstr/includes? (str flow-select) "/"))
        bcolor @(re-frame/subscribe [::port-color flow-id flow-select])
        btype @(re-frame/subscribe [::port-color flow-id flow-select nil :data-type])
        bg-shade "#00000055"]
    ;(tap> [:fmap? flow-select fmap ttype flow-id])

    (if (= ttype :open-fn)

      [re-com/box
       :style {:border-radius "9px"
               :padding-top "8px"
               :padding-bottom "8px"
               :background-color bg-shade}
       :child [code-box-fn ;;w h
               (+ 13 w) nil
               (str (get fmap :raw-fn "(fn [x] x)")) flow-select]]

      (let [syntaxes ["raw (clojure)" "python" "r" "julia"]
            syntax (get-in fmap [:data :syntax] "raw (clojure)")
            ;_ (tap> [:syntax syntax ttype flow-select])
            ]
        [re-com/v-box :size "none"
        ;:width (px (- w 8)) ;:height (px h)
         ;:style {:border "1px solid yellow"}
         :children [[re-com/box
                     :style {:border-radius "9px"
                             :padding-top "8px"
                             :padding-bottom "8px"
                             :background-color bg-shade}
                     :child [(if read-only-flow?
                               code-box
                               code-box-rwo)
                             ;(- w 5) h ;(- h 5)
                             (+ 10 w) nil ;h
                             ;;(get-in fmap [:data :user-input] (pr-str "feed me, seymour!")) ;; fmap is preprocessed..
                             @(re-frame/subscribe [::get-raw-data-input flow-id flow-select])
                             flow-select syntax]]
                    [re-com/h-box
                     :width (px (- w 10))
                     :height "25px"
                     :size "none"
                     :justify :between
                     :align :center
                     ;:style {:border "1px solid green"}
                     :gap "8px"
                     :children [;[re-com/gap :size "5px"]
                                [re-com/h-box
                                 :gap "8px" :style {:font-size "11px"
                                                    :padding-left "10px"
                                                    :padding-right "10px"}
                                 :children (for [e syntaxes
                                                 :let [selected? (= e syntax)]]
                                             [re-com/box
                                              :attr (when (not selected?) {:on-click #(re-frame/dispatch [::flow-block-meta flow-select :syntax e])})
                                              :style (if selected? {:background-color bg-shade ;; (str (theme-pull :theme/editor-outer-rim-color nil) 25)
                                                                    :color (theme-pull :theme/editor-font-color nil)
                                                                    ;:border (str "1px solid " (theme-pull :theme/editor-outer-rim-color nil) 45)
                                                                    :border-radius "0px 0px 7px 7px"
                                                                    :font-weight 700}
                                                         {:opacity 0.4 :cursor "pointer"})
                                              :padding "5px"
                                              :child (str e)])]
                                [re-com/box
                                 :style {:padding-right "10px"
                                         :color bcolor}
                                         ;:background-color (str bcolor 22)

                                 :child (str btype)]]]]]))))

(defn output-viewer [w h flow-id bid & [pid]]
  (let [value @(re-frame/subscribe [::port-color flow-id bid nil :data-value])
        bg-shade "#00000055"
        value (if (or (vector? value) (map? value) (list? value)) (ut/replace-large-base64 value) value)
        b64? (ut/is-base64? (str value))]
    [re-com/box
     :style {:border-radius "9px"
             :padding-top "8px"
             :padding-bottom "8px"
             :background-color bg-shade}
     :child (cond (and (try (not (empty? value)) (catch :default _ false)) b64?)
                  [re-com/v-box
                   :size "auto"
                   :gap "10px"
                   :children
                   [[re-com/box :child (str "(**base64 string: ~" (.toFixed (/ (* (count (str value)) 6) 8 1024 1024) 2) " MB)")]
                    [re-com/box
                     :size "auto"
                     :child [:img {:src (str "data:image/png;base64," value)}]]]]

                  (and (string? value) (cstr/starts-with? value "http")
                       (or (cstr/ends-with? (cstr/lower-case value) "png")
                           (cstr/ends-with? (cstr/lower-case value) "gif")
                           (cstr/ends-with? (cstr/lower-case value) "webp")
                           (cstr/ends-with? (cstr/lower-case value) "jpeg")
                           (cstr/ends-with? (cstr/lower-case value) "jpg")))
                  [re-com/v-box
                   :gap "10px" :align :center :justify :center
                   :children [;[re-com/box :child (str "image-url-string: " value)]
                              [code-box
                               (+ 13 w) nil
                               (pr-str value)]
                              [:img {:src (str value) :width "90%"}]]]

                  (and (string? value) (cstr/starts-with? value "http")
                       (or (cstr/ends-with? (cstr/lower-case value) "mp4")))
                  [re-com/v-box
                   :gap "10px" ;:align :center :justify :center
                   :children [;[re-com/box :child (str "video-url-string: " value)]
                              [code-box
                               (+ 13 w) nil
                               (pr-str value)]
                                           ;[:img {:src (str value) :width "100%"}]
                              [:iframe ;; :video ? html5 shit instead?
                               {:src (str value)
                                             ;:width "570px"
                                :height "340px"
                                :style {:border "none"
                                                     ;:width "100%"
                                                     ;:height :panel-height+80-px
                                        }}]]]

                  :else [code-box
                         (+ 13 w) nil
                         (pr-str value)])]))

(defn block-renamer [bid]
  (let [flow-id-regex #"^[a-zA-Z0-9_-]+$" ;; alpha, underscores, hypens, numbers
        str-bid (cstr/replace (str bid) ":" "")]
    (if (= @rename-block bid)
      [re-com/input-text
       :src (at)
       :model             str-bid
       :width             "200px"
       :on-change #(do (if (= str-bid (str %)) ;; regex doesnt allow : so we need to remove before
                         (reset! rename-block nil)
                         (do (re-frame/dispatch [::rename-block bid %])
                             (reset! rename-block nil))))
       :validation-regex  flow-id-regex
       :change-on-blur?   true
       :style  {:text-decoration "underline"
                :color "inherit"
                :margin-top "3px"
                :margin-left "-4px"
                :text-align "left"
                :background-color "#00000000"}]
      [re-com/box
       :style {:cursor "pointer"
               :color (theme-pull :theme/editor-outer-rim-color nil)}
       :attr {:on-double-click #(reset! rename-block bid)}
       :child (str bid)])))

(re-frame/reg-sub
 ::block-details
 (fn [db [_ bid]]
   (get-in db [:flows (get db :selected-flow) :map bid :description] "")))

(defn details-panel [bid]
  (let [value @(re-frame/subscribe [::block-details bid])]
    [text-box-rw nil nil value bid]))
    ;; [re-com/box
    ;;  :style {:color (theme-pull :theme/editor-font-color nil)
    ;;          :opacity 0.8
    ;;          ;:background-color (theme-pull :theme/editor-outer-rim-color nil)
    ;;          }
    ;;  :padding "6px"
    ;;  :child "label, description, etc?"]

;; (defn transform-data [data svg-width]
;;   (let [start-times (mapv #(-> % :start) (vals data))
;;         end-times (mapv #(-> % :end) (vals data))
;;         min-start (apply min start-times)
;;         max-end (apply max end-times)
;;         total-duration (- max-end min-start)]
;;     (mapv (fn [[step {:keys [start end]}]]
;;             (let [normalized-start (/ (- start min-start) total-duration)
;;                   normalized-end (/ (- end min-start) total-duration)
;;                   x (int (* normalized-start svg-width))
;;                   width (max 1 (int (* (- normalized-end normalized-start) svg-width)))] ;; Ensure width is at least 1px
;;              {:step step
;;               :x x
;;               :width width}))
;;           data)))

;; (defn gantt-chart [data svg-width]
;;   (let [transformed-data (transform-data data svg-width)
;;         height 20]
;;     (fn []
;;       [:svg {:width (str svg-width "px") :height (str (* height (count transformed-data)) "px")}
;;        (doall (map-indexed (fn [idx {:keys [step x width]}]
;;                              [:rect {:x x
;;                                      :y (* idx height)
;;                                      :width (+ width 5)
;;                                      :height height
;;                                      :fill (if (even? idx) "#76c7c0" "#c77676")
;;                                      :on-click #(js/alert step)}])
;;                            transformed-data))])))

;; (defn transform-data [data svg-width flow-id]
;;   (let [blocks @(re-frame/subscribe [::flowmap])
;;         orderb (vec (keys (get blocks flow-id)))

;;         data (into {}
;;                    (for [[k v] data
;;                          :when (not (cstr/includes? (str k) "/"))]
;;                      {k v}))
;;         data (sort-map-keys (sort orderb) data)
;;         start-times (mapv #(-> % :start) (vals data))
;;         end-times (mapv #(-> % :end) (vals data))
;;         min-start (apply min start-times)
;;         max-end (apply max end-times)
;;         total-duration (- max-end min-start)
;;         ; Define a constant to add before log transformation
;;         log-offset 1]
;;     (mapv (fn [[step {:keys [start end]}]]
;;             (let [duration-ms (- end start)
;;                   ; Add log-offset to the duration before log transformation
;;                   log-scaled-duration (Math/log1p (+ duration-ms log-offset))
;;                   ; Normalize the log-scaled duration to the svg width
;;                   normalized-log-width (int (* (/ log-scaled-duration (Math/log1p (+ total-duration log-offset))) svg-width))
;;                   ; Calculate normalized start
;;                   normalized-start (/ (- start min-start) total-duration)
;;                   ; Determine the x position on the SVG
;;                   x (int (* normalized-start svg-width))]
;;               {:step step
;;                :x x
;;                :width (max 2 normalized-log-width) ; Ensure at least 2px width
;;                :raw-duration duration-ms})) ; Keep the raw duration for reference
;;           data)))

;; (defn transform-data [data svg-width flow-id]
;;   (let [react! [@(re-frame/subscribe [::http/flow-results])]
;;         blocks @(re-frame/subscribe [::flowmap])
;;         orderb (vec (sort (keys blocks)))
;;         data (select-keys data orderb)
;;         data-pairs (remove #(or (= (first %) :done)
;;                              (cstr/includes? (str (first %)) "/")) data)
;;         sorted-data-pairs (sort-by (fn [[k _]] (.indexOf orderb k)) data-pairs)
;;         sorted-data-pairs (sort-by first sorted-data-pairs)
;;         start-times (mapv #(-> % second :start) sorted-data-pairs)
;;         end-times (mapv #(-> % second :end) sorted-data-pairs)
;;         min-start (apply min start-times)
;;         max-end (apply max end-times)
;;         total-duration (- max-end min-start)
;;         log-offset 1]
;;     (mapv (fn [[step {:keys [start end]}]]
;;             (let [end (if (nil? end) (- start 1) end)
;;                   duration-ms (- end start)
;;                   log-scaled-duration (Math/log1p (+ duration-ms log-offset))
;;                   normalized-log-width (int (* (/ log-scaled-duration (Math/log1p (+ total-duration log-offset))) svg-width))
;;                   normalized-start (/ (- start min-start) total-duration)
;;                   x (int (* normalized-start svg-width))]
;;               {:step step
;;                :x x
;;                :color @(re-frame/subscribe [::port-color flow-id step])
;;                :width (max 2 normalized-log-width)
;;                :raw-duration duration-ms}))
;;           sorted-data-pairs)))

(defonce gantt-log (reagent/atom true))

(re-frame/reg-sub
 ::ccounter
 (fn [db _]
   (get-in db [:re-pollsive.core/polling :counter])))

(defonce last-gantt (reagent/atom {}))
(defonce stopper (reagent/atom false))

;; (defn gantt-chart [data svg-width flow-id]
;;   (let [gap 11 ;3.5
;;         react! [@(re-frame/subscribe [::http/flow-results])]
;;         running? (is-running? :* flow-id)
;;         counter (if running? @(re-frame/subscribe [::ccounter]) 0)
;;         ;jj (.now js/Date)
;;         ;; transformed-data (if (and (zero? (mod counter 3)) running? (not @stopper))
;;         ;;                           (do (reset! stopper true)
;;         ;;                             (transform-data data svg-width flow-id (.now js/Date)))
;;         ;;                           (transform-data data svg-width flow-id))
;;         transformed-data (transform-data data svg-width flow-id (.now js/Date))
;;         height 26]

;;     ;;(when (zero? (mod counter 4)) (reset! stopper false))

;;     (when
;;            (and (not (empty? transformed-data))
;;                (not (= transformed-data (get @last-gantt flow-id))))

;;         ;; (tap> [:gantt "updating" counter (zero? (mod counter 5))
;;         ;;           (or (and running? (zero? (mod counter 5)))
;;         ;;               (and (not (empty? transformed-data))
;;         ;;                    (not (= transformed-data (get @last-gantt flow-id)))))
;;         ;;           running?
;;         ;;           (= transformed-data (get @last-gantt flow-id))
;;         ;;           flow-id])
;;         ;(tap> [:gantt "updating" ])
;;            (swap! last-gantt assoc flow-id transformed-data))

;(def last-duration (reagent/atom nil))


(def last-update (reagent/atom 0))

;; (defn transform-data [data svg-width flow-id]
;;   (let [log-scale? @gantt-log ;false ;true
;;         react! [@(re-frame/subscribe [::http/flow-results]) @(re-frame/subscribe [::http/flow-results]) @gantt-log]
;;         blocks @(re-frame/subscribe [::flowmap])
;;         running? @(re-frame/subscribe [::is-running? :* flow-id])
;;         orderb (vec (sort (keys blocks)))
;;         data (select-keys data orderb)
;;         data-pairs (remove #(or (= (first %) :done)
;;                                 (cstr/includes? (str (first %)) "/")) data)
;;         sorted-data-pairs (sort-by (fn [[k _]] (.indexOf orderb k)) data-pairs)
;;         sorted-data-pairs (sort-by first sorted-data-pairs)
;;         start-times (mapv #(-> % second :start) sorted-data-pairs)
;;         end-times (mapv #(-> % second :end) sorted-data-pairs)
;;         min-start (apply min start-times)
;;         max-end (if running? (.now js/Date) (apply max end-times))
;;         total-duration (- max-end min-start)
;;         log-offset 1]
;;     (mapv (fn [[step {:keys [start end fake?]}]]
;;             (let [end (if (nil? end) (+ (.now js/Date) 1000) end)
;;                   start (if (nil? start) (.now js/Date) start)
;;                   end (if fake? 0 end)
;;                   start (if fake? 0 start)
;;                   duration-ms (- end start)
;;                   scaled-duration (if log-scale?
;;                                     (Math/log1p (+ duration-ms log-offset))
;;                                     duration-ms)
;;                   normalized-width (int (* (/ scaled-duration (if log-scale? (Math/log1p (+ total-duration log-offset)) total-duration)) svg-width))
;;                   normalized-start (/ (- start min-start) total-duration)
;;                   x (int (* normalized-start svg-width))]
;;               {:step step
;;                :x x
;;                :fake? fake?
;;                :color @(re-frame/subscribe [::port-color flow-id step])
;;                :width (max 2 normalized-width)
;;                :raw-duration duration-ms}))
;;           sorted-data-pairs)))

;; (defn gantt-chart [data svg-width flow-id]
;;   (let [gap 11 ;3.5
;;         react! [@(re-frame/subscribe [::http/flow-results]) @gantt-log]
;;         running? @(re-frame/subscribe [::is-running? :* flow-id])
;;         counter (if running? @(re-frame/subscribe [::ccounter]) 0) ;; react!
;;         transformed-data (transform-data data svg-width flow-id)
;;         height 26
;;         current-time (.now js/Date)]
;;     ;; (tap> [:gantt-data data])
;;     (when
;;      (and (not (empty? transformed-data))
;;           (not (= transformed-data (get @last-gantt flow-id)))
;;           (if running? (>= (- current-time @last-update) 1000) true))

;;       (reset! last-update current-time)
;;       (swap! last-gantt assoc flow-id transformed-data))

;;       (let [transformed-data (get @last-gantt flow-id)
;;             flow-select @(re-frame/subscribe [::selected-flow-block])]
;;         [:svg {:width "110%"
;;                :height (str (+ 20 (* height (count transformed-data)) (* gap (dec (count transformed-data)))) "px")}
;;          (doall (map-indexed (fn [idx {:keys [step x color width raw-duration]}]
;;                                (let [selected? (= step flow-select)
;;                                      hover-involved? (true? (some #(= % step) (get-in @flow-details-block-container-atom [flow-id :involved])))
;;                                      selected? (or selected? hover-involved?)]
;;                                  [:g {:transform (str "translate(0," (* idx (+ height gap)) ")")}
;;                                   [:rect {:x x
;;                                           :y 0
;;                                           :width width
;;                                           :height height
;;                                           ;;:z-index 1100
;;                                           :filter (when selected? (str "drop-shadow(0 0 0.75rem " color ")"))
;;                                           :on-click #(do (select-block step))
;;                                           :on-mouse-enter #(reset! editor-tooltip-atom (str "select block " step))
;;                                           :on-mouse-leave #(reset! editor-tooltip-atom nil)
;;                                           :stroke color
;;                                           :cursor "pointer"
;;                                           :fill (if selected? color (str color 35))}]
;;                                   [:text {:x (+ x width gap -5) ; Adjust the text position as well
;;                                           :y (+ height (/ gap 2) (if selected? -12 -15)) ; Center text in the gap
;;                                           :font-family (theme-pull :theme/base-font nil)
;;                                           :opacity (if selected? 1.0 0.5)
;;                                           :fill (if selected? color "#ffffff")
;;                                           :font-weight (when selected?  700)
;;                                           :font-size (if selected? "16px" "10px")}
;;                                    (str
;;                                     raw-duration " ms")]]))
;;                              transformed-data))])))


(defn transform-data-old [data svg-width flow-id]
  (let [log-scale? @gantt-log ;false ;true
        react! [@(re-frame/subscribe [::http/flow-results]) @(re-frame/subscribe [::http/flow-results]) @gantt-log]
        blocks @(re-frame/subscribe [::flowmap])
        running? @(re-frame/subscribe [::is-running? :* flow-id])
        orderb (vec (sort-by str (keys blocks)))
        data (select-keys data orderb)
        data-pairs (remove #(or (= (first %) :done)
                                (cstr/includes? (str (first %)) "/")) data)
        sorted-data-pairs (sort-by (fn [[k _]] (.indexOf orderb k)) data-pairs)
        sorted-data-pairs (sort-by first sorted-data-pairs)
        start-times (mapv #(-> % second :start) sorted-data-pairs)
        end-times (mapv #(-> % second :end) sorted-data-pairs)
        min-start (apply min start-times)
        max-end (if running? (.now js/Date) (apply max end-times))
        total-duration (- max-end min-start)
        log-offset 1]
    (mapv (fn [[step {:keys [start end fake?]}]]
            (let [end (if (nil? end) (+ (.now js/Date) 1000) end)
                  start (if (nil? start) (.now js/Date) start)
                  end (if fake? 0 end)
                  start (if fake? 0 start)
                  duration-ms (- end start)
                  scaled-duration (if log-scale?
                                    (Math/log1p (+ duration-ms log-offset))
                                    duration-ms)
                  normalized-width (int (* (/ scaled-duration (if log-scale? (Math/log1p (+ total-duration log-offset)) total-duration)) svg-width))
                  normalized-start (/ (- start min-start) total-duration)
                  x (int (* normalized-start svg-width))]
              {:step step
               :x x
               :fake? fake?
               :color @(re-frame/subscribe [::port-color flow-id step])
               :width (max 2 normalized-width)
               :raw-duration duration-ms}))
          sorted-data-pairs)))

(defn sort-by-order [sorted-data-pairs orderb]
  (let [order-map (zipmap orderb (range))]
    (sort-by (fn [[k _]] (order-map k)) sorted-data-pairs)))

;; (defn transform-data [data svg-width flow-id ]
;;   (let [log-scale? @gantt-log
;;         react! [@(re-frame/subscribe [::http/flow-results]) @(re-frame/subscribe [::http/flow-results]) @gantt-log]
;;         blocks @(re-frame/subscribe [::flowmap])
;;         running? @(re-frame/subscribe [::is-running? :* flow-id])
;;         orderb (vec (sort (keys blocks)))
;;         data (select-keys data orderb)
;;         sorted-data-pairs (mapcat (fn [[k v]] (map (fn [run] [k run]) v)) data)
;;         sorted-data-pairs (vec (sort-by-order sorted-data-pairs orderb))
;;         _ (tap> [:sorted-data-pairs sorted-data-pairs orderb])
;;         start-times (mapv #(-> % second :start) sorted-data-pairs)
;;         end-times (mapv #(-> % second :end) sorted-data-pairs)
;;         min-start (apply min start-times)
;;         max-end (apply max end-times)
;;         ;; Define the scale based on the total display time in ms
;;         total-display-time-ms (- max-end min-start)
;;         _ (tap> [:tt-data total-display-time-ms max-end min-start (count sorted-data-pairs) sorted-data-pairs data])
;;         scale-factor (/ svg-width total-display-time-ms)
;;         output (mapv (fn [[step run]]
;;                        (let [{:keys [start end fake?]} run
;;                              end (if (nil? end) (+ (.now js/Date) 1000) end)
;;                              start (if (nil? start) (.now js/Date) start)
;;                              duration-ms (- end start)
;;                              scaled-duration (if log-scale?
;;                                                (Math/log1p (+ duration-ms 1))
;;                                                duration-ms)
;;                              normalized-width (int (* (/ scaled-duration total-display-time-ms) svg-width))
;;                   ;; Calculate x position as an offset from the earliest event
;;                              x (int (* (- start min-start) scale-factor))]
;;                          {:step step
;;                           :x x
;;                           :fake? fake?
;;                           :color @(re-frame/subscribe [::port-color flow-id step])
;;                           :width (max 2 normalized-width) ;; Ensure a minimum width for visibility
;;                           :raw-duration duration-ms}))
;;                      sorted-data-pairs)]
;;     output))

(defn transform-data [data svg-width flow-id]
  (let [log-scale? @gantt-log
        react! [@(re-frame/subscribe [::http/flow-results]) @(re-frame/subscribe [::http/flow-results]) @gantt-log]
        blocks @(re-frame/subscribe [::flowmap])
        running? @(re-frame/subscribe [::is-running? :* flow-id])
        orderb (vec (sort-by str (keys blocks)))
        sorted-data-pairs (mapcat (fn [[k v]] (map (fn [run] [k run]) v)) data)
        ;; Sort by orderb to maintain the order for rendering
        sorted-data-pairs (sort-by (fn [[k _]] (.indexOf orderb k)) sorted-data-pairs)
        grouped-and-sorted (->> sorted-data-pairs
                                (group-by first)
                                (map (fn [[k v]]
                                       [k (sort-by (comp :end second) v)]))
                                (into {}))
        ;; Find the last event for each step
        last-events (mapv (fn [[k v]] [k (-> v last second :end)]) grouped-and-sorted)
        last-event-set (set (map second last-events))
        ;; Define the scale based on the total display time in ms
        start-times (mapv #(-> % second :start) sorted-data-pairs)
        end-times (mapv #(-> % second :end) sorted-data-pairs)
        min-start (apply min start-times)
        max-end (apply max end-times)
        total-display-time-ms (- max-end min-start)
        scale-factor (/ (* svg-width 1.5)
                        total-display-time-ms)]
    (mapv (fn [[step run]]
            (let [{:keys [start in-chan? end fake?]} run
                  end (if running? (if (nil? end) (+ (.now js/Date) 1000) end) end)
                  start (if running? (if (nil? start) (.now js/Date) start) start)
                  duration-ms (- end start)
                  duration-ms (if (>= duration-ms 0) duration-ms 1)
                  scaled-duration (if log-scale?
                                    (Math/log1p (+ duration-ms 1))
                                    duration-ms)
                  normalized-width (int (* (/ scaled-duration total-display-time-ms) svg-width))
                  x (int (* (- start min-start) scale-factor))
                  last? (contains? last-event-set end)]
              (when (not in-chan?)
                {:step step
                 :x x
                 :last? last?
                 :fake? fake?
                 :color @(re-frame/subscribe [::port-color flow-id step])
                 :width (max 2 normalized-width)
                 :raw-duration duration-ms})))
          sorted-data-pairs)))

(defn ordered-group [grouped-by-step orderb]
  (mapv (fn [k] [k (grouped-by-step k)]) orderb))

(defn gantt-chart [data svg-width flow-id]
  (let [gap 11
        react! [@(re-frame/subscribe [::http/flow-results]) @gantt-log]
        blocks @(re-frame/subscribe [::flowmap])
        orderb (vec (sort-by str (keys blocks)))
        running? @(re-frame/subscribe [::is-running? :* flow-id])
        transformed-data (transform-data data svg-width flow-id)
        height 26
        current-time (.now js/Date)
        ;; Group runs by step - here we lose our correct order!
        grouped-by-step (group-by :step transformed-data)
        grouped-by-step (ordered-group grouped-by-step orderb)
        has-data? (and (not (empty? transformed-data))
                       ;(not (empty? (vals grouped-by-step)))
                       (not (= grouped-by-step (get @last-gantt flow-id {})))
                       (if running? (>= (- current-time @last-update) 1000) true))]

    ;; (tap> [:t-group has-data? grouped-by-step transformed-data])

    (when has-data?
      (reset! last-update current-time)
      (swap! last-gantt assoc flow-id grouped-by-step))

    ;; Render SVG
    (let [flow-select @(re-frame/subscribe [::selected-flow-block])
          grouped-by-stepc (get @last-gantt flow-id {})]
      [:svg {:width "110%"
             :height (str (+ 20 (* height (count grouped-by-stepc)) (* gap (dec (count grouped-by-stepc)))) "px")}
       (try
         (doall (map-indexed (fn [idx [step runs]]
                               (doall (map-indexed (fn [run-idx {:keys [x color width last? raw-duration]}]
                                                     (let [y (+ (* idx (+ height gap)) (* run-idx 0)) ; Adjust y for multiple rects if needed
                                                           selected? (= step flow-select)
                                                           hover-involved? (true? (some #(= % step) (get-in @flow-details-block-container-atom [flow-id :involved])))]
                                                       [:g {:transform (str "translate(0," y ")")}
                                                        [:rect {:x x
                                                                :y 0
                                                                :width width
                                                                :height height
                                                                :filter (when selected? (str "drop-shadow(0 0 0.75rem " color ")"))
                                                                :on-click #(do (select-block step))
                                                                :on-mouse-enter #(reset! editor-tooltip-atom (str "select block " step))
                                                                :on-mouse-leave #(reset! editor-tooltip-atom nil)
                                                                :stroke color
                                                                :cursor "pointer"
                                                                :fill (if selected? color (str color 35))}]
                                                        (when last? [:text {:x (+ x width gap -5)
                                                                            :y (+ height (/ gap 2) (if selected? -12 -15))
                                                                            :font-family "Arial"
                                                                            :opacity (if selected? 1.0 0.5)
                                                                            :fill (if selected? color "#ffffff")
                                                                            :font-weight (when selected? "bold")
                                                                            :font-size (if selected? "16px" "10px")}
                                                                     (str raw-duration " ms")])]))
                                                   runs)))
                             grouped-by-stepc))
         (catch :default _ nil))])))

(declare block-icons)

(defn gantt-container [flow-id orderb in-sidebar? pw & [max-height]]
  (let [opw pw ;; orig
        pw (* pw 0.33)
        gw (* 0.46 pw)
        left 35 top 73
        react! [@(re-frame/subscribe [::http/flow-results]) @gantt-log @last-update]
        bg-height (+ (* (count orderb) 37) 36)
        running? @(re-frame/subscribe [::is-running? :* flow-id])
        ;data (get-in @(re-frame/subscribe [::http/flow-results]) [:tracker flow-id] {})
        data  @(re-frame/subscribe [::http/flow-results-tracker flow-id])
        ddata (apply concat (for [[_ v] data] v))
        ;;_ (tap> [:ddata ddata])
        sstart (apply min (for [{:keys [start]} ddata] start))
        eend (if running? (.now js/Date) ;@last-update
                 (apply max (for [{:keys [end]} ddata] end)))
        duration (str (ut/nf (- eend sstart)) " ms")
        bdr (str "6px solid " (theme-pull :theme/editor-outer-rim-color nil))]

    (if in-sidebar?

      (let [blocks @(re-frame/subscribe [::flowmap])
            flow-select @(re-frame/subscribe [::selected-flow-block])]
        ;(tap> [:blocks blocks])
        [re-com/h-box
         :size "none"
         :gap "6px"
         :height (if max-height (px max-height)
                     (px (* (count (keys blocks)) 36)))
         :style (when max-height {:overflow "auto"})
         :children [[re-com/v-box
                     :style {;:border "1px solid white"
                             :margin-left "3px"
                             :padding-left "5px"
                             :padding-right "5px"}
                     :width "28px"
                     :gap "10px"
                     :children (block-icons blocks flow-id flow-select)]
                    [gantt-chart data (* 0.47 opw) flow-id]]
         :width (px opw)])

      [re-com/v-box
       :children [[re-com/box
                   :width (px pw)
                   :height (px bg-height)
                   :style {:position "fixed"
                           :background-color "#00000066"
                           :border-radius "0px 12px 0px 0px"
                           :z-index 999
                           :overflow "hidden"
                           :left left
                           :padding-top "18px"
                           :padding-bottom "18px"
                           :padding-left "8px"
                           :padding-right "18px"
                           :margin-left "4px"
                           :border-top bdr
                           :backdrop-filter "blur(4px)"
                           :top top}
                   :child [gantt-chart data gw flow-id]]

                  [re-com/h-box
                   :justify :between :align :center
                   :height "30px"
                   :size "none"
                   :width (px (+ 3 pw))
                   :children [[re-com/box
                               :style {:cursor "pointer"
                                       :text-decoration (if @gantt-log "none" "line-through")}
                               :attr {:on-click #(reset! gantt-log (not @gantt-log))}
                               :child (str "use log widths?")]
                              [re-com/box :child
                               (str
                                (if running? "so far... " "total ")
                                duration)]]
                   :style {:position "fixed"
                           :padding-left "14px"
                           :padding-right "10px"
                           :z-index 1111
                           :color (str (theme-pull :theme/editor-outer-rim-color nil) 88)
                           :font-size "11px"
                           :border-radius "0px 0px 12px 0px"
                           :background-color "#00000066"
                           :top (+ bg-height top) :left left}]
                  [re-com/box
                   :width (px pw)
                   :height (px (+ 30 bg-height))
                   :style {:position "fixed"
                           :background-color "#00000066"
                           :border-radius "0px 12px 12px 0px"
                           :z-index 995
                           :left left
                           :filter "drop-shadow(0.35rem 0.35rem 0.4rem rgba(0, 0, 0, 0.8))"
                           :padding-top "18px"
                           :padding-bottom "18px"
                           :padding-left "8px"
                           :padding-right "18px"
                           :margin-left "4px"
                           :top top}
                   :child " "]]])))

(defn ordered-for [order keys-map]
  (map #(vector % (keys-map %)) order))

(defn block-icons [blocks flow-id flow-select]
  (vec (for ;[[k {:keys [icon]}] (ordered-for orderb blocks)]
        [[k icon] (sort-by first (for [[k v] blocks] [k (get v :icon)]))]
         (let [bcolor @(re-frame/subscribe [::port-color flow-id k])
               waiting? @(re-frame/subscribe [::is-waiting? k flow-id])
               running? @(re-frame/subscribe [::is-running? k flow-id])
               selected? (= k flow-select)]
           [re-com/box
            :size "none"
            :height "27px"
            :width "27px"
            :align :center :justify :center
            :class (when running? "rotate infinite")
            :style {:background-color (str bcolor "28")
                    :opacity (if (or running? selected?) 1.0 0.4)
                    :border-radius "5px"
                    :transform (when waiting? "rotate(45deg)")
                    :z-index (when running? 1200)
                    :zoom (when running? 1.5)
                    :margin-top (when running? "-4px")
                    :margin-bottom (when running? "-5px")
                    :border (str "1px dotted " bcolor)
                    :margin-left (if @(re-frame/subscribe [::bricks/flow-editor?]) "-7px" "9px")}
            :child [re-com/md-icon-button :src (at)
                    :md-icon-name (or icon "zmdi-view-quilt")
                    :on-click #(do (select-block k))
                                                                              ;(zoom-to-element (str "flow-brick-" flow-select) 2)

                    :attr {:on-mouse-enter #(reset! editor-tooltip-atom (str "select block " k))
                           :on-mouse-leave #(reset! editor-tooltip-atom nil)}
                    :style {;:background-color (theme-pull :theme/editor-rim-color nil) ;"#00000000"
                                                ;:opacity 0.6
                            :color bcolor ;(theme-pull :theme/editor-outer-rim-color nil)
                            :cursor "pointer"
                            :filter (when (or running? selected?)
                                      (str "brightness(1.5) drop-shadow(0.2rem 0.2rem 0.4rem " bcolor ")"))

                            :font-size "24px"}]]))))

(defn flow-details-panel [panel-height panel-width details-panel-height]
  (let [react-hacks [@flow-hover @scroll-id @editor-mode @(re-frame/subscribe [::http/flow-results])]
        ;details-panel-height (/ panel-height 1.25)
        blocks @(re-frame/subscribe [::flowmap])
        orderb (vec (sort-by str (keys blocks)))
        flow-id @(re-frame/subscribe [::selected-flow])
        flow-select @(re-frame/subscribe [::selected-flow-block])
        ;running? (is-running? :* flow-id)
        ;panel-width (- panel-width 28) ;; modded
        browser-panel-width 600
        gantt? @(re-frame/subscribe [::bricks/flow-gantt?])
        ;browser-panel-height (- details-panel-height 42)
        ;read-only-flow? (true? (cstr/includes? flow-id "/"))
        ;flow-id-regex #"(.|\s)*\S(.|\s)*"
        ;flow-id-regex #"^[a-zA-Z0-9_-]+$" ;; alpha, underscores, hypens, numbers
        ;cch (px (- details-panel-height 70))
        full-height (+ details-panel-height panel-height -40)]

    ;; (when (and (not @dragging-port?) (not @db/dragging-flow-editor?) (not @bricks/dragging-editor?) (not @dragging?) (not @bricks/dragging?))
    ;;   (reagent.core/next-tick #(smooth-scroll-to-element "scrolly-boy" (str flow-select))))

    [re-com/v-box
     :children [;; [re-com/h-box
                ;;  :width (px panel-width)
                ;;  :size "none" ;:height "38px"
                ;;  :style {:padding-left "4px"
                ;;          :font-size "18px"
                ;;         ;:margin-top "-10px"
                ;;         ;:border "1px solid yellow"
                ;;          :padding-right "4px"}
                ;;  :justify :between
                ;;  :children [[re-com/box
                ;;              :child (if (and (empty? @editor-tooltip-atom) running?) (str :flow-name " is running")
                ;;                         (str (or @editor-tooltip-atom "r")))
                ;;               ;:max-width (px (- panel-width 300))
                ;;              :size "none" :height "30px"
                ;;              :width "300px" :align :end :justify :end
                ;;              :style ;(merge
                ;;              {;:border "2px solid green"
                ;;               :position "fixed"
                ;;               :bottom (if @(re-frame/subscribe [::bricks/flow-editor?]) 3 10)
                ;;               :left (if @(re-frame/subscribe [::bricks/flow-editor?]) 293 (- panel-width 345))
                ;;               :overflow "hidden"}]]]

                [re-com/h-box
                 :children [;[re-com/gap :size "8px"]
                            (when (and @(re-frame/subscribe [::bricks/flow-editor?]) (not flow-select))
                              [re-com/box
                               :style {:margin-top "-2px"}
                              ;:style {:border "1px solid yellow"  }
                               :child [flow-editor browser-panel-width full-height] ; browser-panel-height]
                               :width (px browser-panel-width)
                               :height (px full-height)])
                            (when (and @(re-frame/subscribe [::bricks/flow-editor?]) flow-select)
                              (let [;body (get @(re-frame/subscribe [::flowmap]) flow-select)
                                    ;top-px (* (- full-height 45) 0.47)
                                    fmap (get blocks flow-select)
                                    inputs? (not (empty? (keys (get-in fmap [:ports :in]))))
                                    pval @(re-frame/subscribe [::port-color flow-id flow-select :out :data-value])
                                    view-pval @(re-frame/subscribe [::port-color flow-id (keyword (str (cstr/replace (str flow-select) ":" "") "-vw")) :out :data-value])
                                    browsable? (true? (or (vector? pval) (map? pval)))
                                    rabbit-code? (or (and (vector? pval) (keyword? (first pval))) ;; is it renderable rabbit-code?
                                                     (and (map? pval) (contains? pval :queries) (contains? pval :view)))
                                    rabbit-code-view? (or (and (vector? view-pval) (keyword? (first view-pval))) ;; is it renderable rabbit-code?
                                                          (and (map? view-pval) (contains? view-pval :queries) (contains? view-pval :view)))
                                    block-type (get-in blocks [flow-select :data :flow-item :type]
                                                       (get-in blocks [flow-select :data :drag-meta :type] :no-type?))
                                    desc (get-in blocks [flow-select :data :flow-item :description])
                                    ;outputs? (not (empty? (keys (get-in fmap [:ports :in]))))
                                    header-row-height 56
                                    ttype (get-in fmap [:data :flow-item :type]
                                                  (get-in fmap [:data :drag-meta :type]))
                                    open? (true? (or (= ttype :open-fn) (= ttype :open-block) (= ttype :open-input)))
                                    port-meta @(re-frame/subscribe [::get-meta flow-select])
                                    single-scrubber? (and (or (= ttype :open-block) (= ttype :open-input))
                                                          (not (nil? (get-in port-meta [:* :scrubber]))))
                                    flow-select @(re-frame/subscribe [::selected-flow-block]) ;; dupe here from above to force re-render
                                    ;; _ (tap> [:single-scrubber? flow-select single-scrubber?
                                    ;;          @(re-frame/subscribe [::bricks/keypaths-in-flow flow-select true])
                                    ;;          port-meta (get-in port-meta [:*] )])
                                    ]
                               ;; (tap> [:ss @(re-frame/subscribe [::bricks/keypaths-in-flow flow-select])])
                                ;; [re-com/box
                                ;;  :style {:position "fixed"
                                ;;          :border "1px solid lime"
                                ;;          :left 10 :top 430}
                                ;;  :child [flow-details-block 540 577 flow-select body]]
                                ;; :style {:background "linear-gradient(0deg, transparent 85%, black 100%)"}
                                ;; :background "linear-gradient(180deg, black 15%, transparent 100%)"
                                [re-com/v-box
                                 :align :center
                                 :height (px full-height)
                                 :width "600px"
                                 :size "none"
                                 :style {:font-size "15px"
                                         :background-color "#00000088"}
                                         ;:margin-right "-8px"

                                 :children
                                 [[re-com/h-box
                                   :children [[block-renamer flow-select]



                                              ;[re-com/md-icon-button :md-icon-name "zmdi-pizza"]
                                              ;[re-com/box :child (str flow-select)]
                                              ;[re-com/box :child "fixed block header"]


                                              [re-com/h-box
                                               :gap "8px"
                                               :children
                                               [[re-com/box
                                                 :attr {:on-click #(zoom-to-element (str "flow-brick-" flow-select) 1.3)}
                                                 :child [render-icon (str (get fmap :icon "zmdi-layers")) "inherit" 29 20]]
                                                ;[re-com/box :child (str (get fmap :icon))]
                                                [re-com/box :child (str block-type)]]]]
                                   :width "601px" :height (px header-row-height) ;; width overflow on purpose to connect border lines
                                   :style {:margin-left "1px"
                                           :padding-right "16px"
                                           :font-weight 700
                                           :color (str (theme-pull :theme/editor-outer-rim-color nil) 87)
                                           :padding-top "5px"
                                           :padding-left "13px"
                                           :padding-bottom "5px"
                                           ;:border-bottom (str "1px solid " (theme-pull :theme/editor-outer-rim-color nil) 35)
                                           :margin-top "-1px"
                                           :border-top (str "6px solid " (theme-pull :theme/editor-outer-rim-color nil))
                                           :background-color "#00000088"}
                                   :size "none" :justify :between :align :center]

                                  [re-com/v-box
                                 ;:size "auto"
                                   ;:width "585px"
                                   :size "none"
                                   :gap "6px"
                                   :height (px (- full-height header-row-height))
                                   :style {:position "fixed"
                                           ;:border "1px dotted pink"
                                           :overflow-y "auto"
                                           :overflow-x "hidden"
                                         ;:border-radius "10px"
                                         ;:border-top "5px solid pink"
                                           :left 6 :top header-row-height}
                                 ;:gap "3px"
                                 ;:justify :between
                                 ;:width "610px"
                                   :children [;[re-com/box :child (str "debug some shit: "  ttype open?)]

                                              (when (not gantt?)
                                                [flow-details-block-container "in-line flow gantt*" :system :system
                                                 [gantt-container flow-id orderb true 570] "zmdi-chart-donut"])

                                              ;; [flow-details-block-container "test gantt" flow-id flow-select
                                              ;;  [gantt-chart (get-in @(re-frame/subscribe [::http/flow-results]) [:tracker flow-id]) 560 flow-id] "zmdi-comment-alt-text"]

                                              [flow-details-block-container "step notes*" flow-id flow-select
                                               [details-panel flow-select] "zmdi-comment-alt-text"]

                                              ;[flow-details-block top-px 580 flow-select body false]
                                              (when inputs?
                                                [flow-details-block-container "input ports" flow-id flow-select
                                                 [port-panel flow-id flow-select (get blocks flow-select) :in]
                                                 ["zmdi-long-arrow-right" "zmdi-circle-o"]])

                                              (when (and (not inputs?) single-scrubber?)
                                                [flow-details-block-container "input scrubber" flow-id flow-select
                                                ;;  [re-com/box :child "ffff"]
                                                 [re-com/box
                                                  ;:height "60px"
                                                  :size "auto"
                                                  :style {:padding-top "8px"
                                                          ;:background-color "gray"
                                                          :padding-bottom "8px"}
                                                  :child [bricks/scrubber-panel true
                                                          @(re-frame/subscribe [::bricks/keypaths-in-flow flow-select true])
                                                          [:flow flow-select] :*
                                                          ;nil nil
                                                          {:fm true :canvas? true :flow? true}]]
                                                 ["zmdi-long-arrow-right" "zmdi-circle-o"]])

                                              (if open?
                                                [flow-details-block-container
                                                 (if (= ttype :open-fn) "code editor" "code / value editor")
                                                 flow-id flow-select
                                                 [flow-code-editor-block 580 378 fmap flow-select ttype flow-id]
                                                 (if (= ttype :open-fn)
                                                   "zmdi-functions"
                                                   "zmdi-view-subtitles")]

                                                (when (not (empty? desc))
                                                  [flow-details-block-container
                                                   "flow block description"
                                                   flow-id flow-select
                                                   [re-com/box
                                                    :style {:padding-left "6px"
                                                            :font-size "12px"
                                                            :color (str (theme-pull :theme/editor-outer-rim-color nil))
                                                            :padding-right "6px"}
                                                    :child (str desc)]
                                                   "zmdi-view-toc"]))

                                              (when (and (= ttype :open-fn) open?)
                                                [flow-details-block-container
                                                 "view editor*"
                                                 flow-id flow-select
                                                 [code-box-view 580 nil fmap flow-select flow-id]
                                                 "zmdi-functions"])

                                              (when rabbit-code-view?
                                                [flow-details-block-container "rabbit render (view)" flow-id flow-select
                                                 ;[map-boxes2 pval flow-select flow-id [] :output (if (vector? pval) "vector" "map") true]
                                                 [re-com/box
                                                  :align :center :justify :center
                                                  :child [buffy/render-honey-comb-fragments view-pval
                                                  ;11 9
                                                          nil nil]]
                                                 "zmdi-aspect-ratio"])

                                              [flow-details-block-container "output ports" flow-id flow-select
                                               [port-panel flow-id flow-select (get blocks flow-select) :out]
                                               ["zmdi-circle" "zmdi-long-arrow-right"]]

                                              [flow-details-block-container "output value(s)" flow-id flow-select
                                               [output-viewer 580 378 flow-id flow-select nil]
                                               "zmdi-view-web"]

                                              ;;[map-boxes2 return-val :sub-flows-used nil [] :output "vector"]

                                              (when browsable?
                                                [flow-details-block-container "output browser" flow-id flow-select
                                                 [map-boxes2 pval flow-select flow-id [] :output (if (vector? pval) "vector" "map") true]
                                                 "zmdi-shape"])

                                              (when rabbit-code?
                                                [flow-details-block-container "rabbit render" flow-id flow-select
                                                 ;[map-boxes2 pval flow-select flow-id [] :output (if (vector? pval) "vector" "map") true]
                                                 [re-com/box
                                                  :align :center :justify :center
                                                  :child [buffy/render-honey-comb-fragments pval
                                                          ;11 9
                                                          nil nil]]
                                                 "zmdi-aspect-ratio"])



                                              ;[flow-details-block top-px 580 flow-select body false]
                                              ;[flow-details-block top-px 580 flow-select body true]
                                              ;; debug-box [flow-select raw-fmap]


                                              [flow-details-block-container "flow block map - debugger*" flow-id flow-select
                                               [debug-box flow-select
                                                ;fmap ;; fmap is preprocessed....
                                                @(re-frame/subscribe [::get-raw-block-map flow-id flow-select])]
                                               "zmdi-bug"]

                                              [flow-details-block-container "channel push - debugger*" flow-id flow-select
                                               (let [last-output (ut/replace-large-base64
                                                                  @(re-frame/subscribe [::port-color flow-id flow-select nil :data-value]))
                                                     val (get-in @channel-holster [flow-id flow-select :value] last-output)
                                                     copen? @(re-frame/subscribe [::bricks/flow-channels-open? flow-id])]
                                                 (if copen? [channel-box flow-id flow-select val]
                                                     [re-com/v-box
                                                      :align :center :justify :center
                                                      :style {:color (theme-pull :theme/editor-outer-rim-color nil)}
                                                      :children [[re-com/box
                                                                  :style {:font-size "15px"}
                                                                  :child "channels are all closed"]
                                                                 [re-com/box
                                                                  :style {:font-size "11px"}
                                                                  :child "if you'd like to push channel values, run again with option :close-on-done? false"]]]))
                                               "zmdi-bug"]

                                              [flow-details-block-container "connections - debugger*" flow-id flow-select
                                               [re-com/box ;;; edit-conns
                                                :child [code-box-rwc 580 nil
                                                        (str @(re-frame/subscribe [::flowmap-connections]))]]
                                               "zmdi-bug"]]]]]))
                                              ;; [re-com/h-box
                                              ;;  :justify :between
                                              ;;  :children [[re-com/box :child "weird footer? THE END" :size "auto" :justify :start :padding "5px"]
                                              ;;             [re-com/box :child (str flow-select) :size "auto" :justify :end :padding "5px"]]
                                              ;;  :align :center :justify :center
                                              ;;  :size "none" :height "30px"
                                              ;;  :width "580px" :style {:border "1px solid darkcyan"}]

                                            ;[flow-details-block top-px 580 flow-select body true]


                            [re-com/v-box
                             :gap (if (or gantt? flow-select) "10px" "20px")
                             :size "none"
                             :height (px full-height) ; (px browser-panel-height)
                             :style {:background-color "#00000099"
                                     :border-radius "0px 16px 0px 0px"
                                     :position "fixed"
                                     :left (if @(re-frame/subscribe [::bricks/flow-editor?]) 600 0)
                                     :backdrop-filter "blur(3px)"
                                     :top -1 ;28
                                     :padding-top "10px"
                                     :padding-right (if (not @(re-frame/subscribe [::bricks/flow-editor?])) "5px" "inherit")
                                     :border-top (str "6px solid " (theme-pull :theme/editor-outer-rim-color nil))
                                     :border-right (str "1px solid " (theme-pull :theme/editor-outer-rim-color nil) 25)}
                                     ;:background-color (str (theme-pull :theme/editor-outer-rim-color nil) 15)
                             ;;:align :center :justify :center
                             :width "40px"
                             :align :center :justify :start
                             :children (into [[re-com/md-icon-button :src (at)
                                               :md-icon-name (if  @(re-frame/subscribe [::bricks/flow-editor?]) "zmdi-chevron-left" "zmdi-chevron-right")
                                               :on-click #(do (re-frame/dispatch [::bricks/toggle-flow-editor])
                                                              (reset! editor-tooltip-atom nil))
                                               :attr {:on-mouse-enter #(reset! editor-tooltip-atom (str "toggle flow work panel " (if @(re-frame/subscribe [::bricks/flow-editor?]) "open" "closed")))
                                                      :on-mouse-leave #(reset! editor-tooltip-atom nil)}
                                               :style {:opacity 0.45
                                                       :color (theme-pull :theme/editor-outer-rim-color nil)
                                                       :cursor "pointer"
                                                       :height "15px"
                                                       :margin-left (if @(re-frame/subscribe [::bricks/flow-editor?]) "-9px" "9px")
                                                       :font-size "33px"}]

                                              [re-com/md-icon-button :src (at)
                                               :md-icon-name "zmdi-chart"
                                               :on-click #(do (re-frame/dispatch [::bricks/toggle-flow-gantt])
                                                              (reset! editor-tooltip-atom nil))
                                               :attr {:on-mouse-enter #(reset! editor-tooltip-atom
                                                                               (str "toggle run gantt chart "
                                                                                    (if gantt? "open" "closed")))
                                                      :on-mouse-leave #(reset! editor-tooltip-atom nil)}
                                               :style {:opacity (if gantt? 1.0 0.45)
                                                       :color (theme-pull :theme/editor-outer-rim-color nil)
                                                       :cursor "pointer"
                                                       :height "15px"
                                                       :margin-top "14px"
                                                       :margin-left (if @(re-frame/subscribe [::bricks/flow-editor?]) "-9px" "9px")
                                                       :font-size "23px"}]

                                              [re-com/gap :size (if flow-select "3px" "3px")]

                                              (if  gantt?
                                                [gantt-container flow-id orderb false panel-width]
                                                [re-com/gap :size "0px"])]

                                             (if (or flow-select gantt?)
                                               (block-icons blocks flow-id flow-select)

                                               [[re-com/md-icon-button :src (at)
                                                 :md-icon-name "zmdi-view-list"
                                        ;:on-click #(re-frame/dispatch [::bricks/toggle-flow-editor])
                                                 :attr {:on-mouse-enter #(reset! editor-tooltip-atom (str "flow browser"))
                                                        :on-click #(reset! editor-mode :flow-browser)
                                                        :on-mouse-leave #(reset! editor-tooltip-atom nil)}
                                                 :style {;:background-color (theme-pull :theme/editor-rim-color nil) ;"#00000000"
                                                         :opacity (when (not (= :flow-browser @editor-mode)) 0.33)
                                                         :color (theme-pull :theme/editor-outer-rim-color nil)
                                                         :cursor "pointer"
                                                         :height "15px"
                                                         :margin-left (if @(re-frame/subscribe [::bricks/flow-editor?]) "-9px" "9px")
                                                         :font-size "19px"}]
                                                [re-com/md-icon-button :src (at)
                                                 :md-icon-name "zmdi-shape"
                                        ;:on-click #(re-frame/dispatch [::bricks/toggle-flow-editor])
                                                 :attr {:on-mouse-enter #(reset! editor-tooltip-atom (str "flow part browser"))
                                                        :on-click #(reset! editor-mode :part-browser)
                                                        :on-mouse-leave #(reset! editor-tooltip-atom nil)}
                                                 :style {;:background-color (theme-pull :theme/editor-rim-color nil) ;"#00000000"
                                                         :opacity (when (not (= :part-browser @editor-mode)) 0.33)
                                                         :color (theme-pull :theme/editor-outer-rim-color nil)
                                                         :cursor "pointer"
                                                         :height "15px"
                                                         :margin-left (if @(re-frame/subscribe [::bricks/flow-editor?]) "-9px" "9px")
                                                         :font-size "19px"}]
                                                [re-com/md-icon-button :src (at)
                                                 :md-icon-name "zmdi-time-interval"
                                        ;:on-click #(re-frame/dispatch [::bricks/toggle-flow-editor])
                                                 :attr {:on-mouse-enter #(reset! editor-tooltip-atom (str "flow scheduler"))
                                                        :on-click #(reset! editor-mode :scheduler)
                                                        :on-mouse-leave #(reset! editor-tooltip-atom nil)}
                                                 :style {;:background-color (theme-pull :theme/editor-rim-color nil) ;"#00000000"
                                                         :opacity (when (not (= :scheduler @editor-mode)) 0.33)
                                                         :color (theme-pull :theme/editor-outer-rim-color nil)
                                                         :cursor "pointer"
                                                         :height "15px"
                                                         :margin-left (if @(re-frame/subscribe [::bricks/flow-editor?]) "-9px" "9px")
                                                         :font-size "19px"}]
                                                [re-com/md-icon-button :src (at)
                                                 :md-icon-name "zmdi-view-subtitles"
                                        ;:on-click #(re-frame/dispatch [::bricks/toggle-flow-editor])
                                                 :attr {:on-mouse-enter #(reset! editor-tooltip-atom (str "run history"))
                                                        :on-click #(reset! editor-mode :run-history)
                                                        :on-mouse-leave #(reset! editor-tooltip-atom nil)}
                                                 :style {;:background-color (theme-pull :theme/editor-rim-color nil) ;"#00000000"
                                                         :opacity (when (not (= :run-history @editor-mode)) 0.33)
                                                         :color (theme-pull :theme/editor-outer-rim-color nil)
                                                         :cursor "pointer"
                                                         :height "15px"
                                                         :margin-left (if @(re-frame/subscribe [::bricks/flow-editor?]) "-9px" "9px")
                                                         :font-size "19px"}]

                                                [re-com/md-icon-button :src (at)
                                                 :md-icon-name "zmdi-bug"
                                        ;:on-click #(re-frame/dispatch [::bricks/toggle-flow-editor])
                                                 :attr {:on-mouse-enter #(reset! editor-tooltip-atom (str "flow block debug editor"))
                                                        :on-click #(reset! editor-mode :debug)
                                                        :on-mouse-leave #(reset! editor-tooltip-atom nil)}
                                                 :style {;:background-color (theme-pull :theme/editor-rim-color nil) ;"#00000000"
                                                         :opacity (when (not (= :debug @editor-mode)) 0.33)
                                                         :color (theme-pull :theme/editor-outer-rim-color nil)
                                                         :cursor "pointer"
                                                         :height "15px"
                                                         :margin-left (if @(re-frame/subscribe [::bricks/flow-editor?]) "-9px" "9px")
                                                         :font-size "19px"}]]))]]]]


                                ;;        [re-com/md-icon-button :src (at)
                                ;;         :md-icon-name "zmdi-view-list"
                                ;;         ;:on-click #(re-frame/dispatch [::bricks/toggle-flow-editor])
                                ;;         :attr {:on-mouse-enter #(reset! editor-tooltip-atom (str "server-side flow browser"))
                                ;;                :on-click #(reset! editor-mode :server-flows)
                                ;;                :on-mouse-leave #(reset! editor-tooltip-atom nil)}
                                ;;         :style {;:background-color (theme-pull :theme/editor-rim-color nil) ;"#00000000"
                                ;;                 :opacity (when (not (= :server-flows @editor-mode)) 0.33)
                                ;;                 :color (theme-pull :theme/editor-outer-rim-color nil)
                                ;;                 :cursor "pointer"
                                ;;                 :height "15px"
                                ;;                 :margin-left (if @(re-frame/subscribe [::bricks/flow-editor?]) "-9px" "9px")
                                ;;                 :font-size "19px"}]


;; commented out accordian
                      ;;       (if (not (= :run-history @editor-mode))

                      ;;         [re-com/h-box
                      ;;          :attr {:id "scrolly-boy"}
                      ;;  ;:on-mouse-enter #(add-wheel-listener "scrolly-boy")
                      ;;  ;:on-mouse-leave #(remove-wheel-listener "scrolly-boy")

                      ;;          :size "none"
                      ;;          :style {:overflow "scroll"
                      ;;                  :padding-left "8px"}
                      ;;          :width (px (- panel-width (if @(re-frame/subscribe [::bricks/flow-editor?]) browser-panel-width 0) 12))
                      ;;          :align :center :justify :start
                      ;;          :gap "5px"
                      ;;          :children [[re-com/h-box
                      ;;                      :gap "6px"
                      ;;                      :children (for [id block-keys
                      ;;                                      :let [body (get @(re-frame/subscribe [::flowmap]) id)]]
                      ;;                                  [flow-details-block cch id body])]]]

                      ;;         (let [qq1 {:select [:base_flow_id :block :channel
                      ;;                             :data_type :dbgn :dest :elapsed_ms
                      ;;                             :end :end_ts :flow_id :from_block
                      ;;                             :path :start :start_ts :ts :type
                      ;;                             :value]
                      ;;                    :connection-id "flows-db"
                      ;;                    :order-by [[8 :desc]]
                      ;;                    :from   [[:fn_history :rr582]]}]
                      ;;           [re-com/v-box
                      ;;            :size "none" ;:padding "25px"
                      ;;            :gap "14px"
                      ;;            :width (px (- panel-width 611))
                      ;;            :height (px browser-panel-height)
                      ;;            :style {:border "1px solid white" ;:margin-top "16px"
                      ;;                    :padding-left "14px"}
                      ;;            :children [[re-com/box
                      ;;                        :height "56px" :width (px (- panel-width 650))
                      ;;                        :style {:border "1px solid purple"}
                      ;;                        :child [buffy/render-honey-comb-fragments (waffles [] (- panel-width 650)) 21.75 2]]
                      ;;                       [buffy/render-honey-comb-fragments qq1 21.75 8]]]))


     :size "none"
     :align :center :justify :center
     :style {:z-index 200
            ;:id "scrolly-boy"
             :font-family (theme-pull :theme/base-font nil)
             :color (theme-pull :theme/editor-font-color nil)
            ;:background-color (str (theme-pull :theme/editor-background-color nil) 99) ; "#000000"
             :backdrop-filter "blur(5px)"
             :background-color "#00000022"
             :border-radius "10px"
             ;:border "1px solid orange"
             :position "fixed"
             :top 28} ;(- panel-height 8)
             ;:left 13


            ;:margin-left "-6px"
            ;:border-radius "0px 0px 16px 16px"
            ;:border-left (str "6px solid " (theme-pull :theme/editor-outer-rim-color nil))
            ;:border-right (str "6px solid " (theme-pull :theme/editor-outer-rim-color nil))
            ;:border-bottom (str "6px solid " (theme-pull :theme/editor-outer-rim-color nil))

     :height (px full-height) ; (px details-panel-height)
   ;:width (px (- panel-width 12))
     :width (if @(re-frame/subscribe [::bricks/flow-editor?]) (px 600)
                ;(px panel-width)
                (px 45))]))



;; (defn alert-box2 []
;;   (let [rekt @db/kick-alert
;;         alerts @(re-frame/subscribe [::bricks/alerts])]

;;     [re-com/box
;;      :child [re-com/h-box
;;              :children
;;              [;[:img {:src "images/test-kick-icon.png" :width 30 :height 30}]
;;               [re-com/md-icon-button :src (at)
;;                :md-icon-name (if @db/kick-alert "zmdi-star" "zmdi-star-outline")
;;                :style {;:color "red"
;;                        :margin-right "12px"
;;                        :margin-top "-4px"
;;                        :font-size "34px"}]
;;               [re-com/box
;;                ;:style {:opacity (if @db/kick-alert 1.0 0.0)}
;;                :child
;;                [buffy/render-honey-comb-fragments (first (first alerts))]]]]
;;      :width "420px"
;;      :height "50px"
;;      :padding "9px"
;; ;     attr {:on-click #(reset! db/kick-alert (not @db/kick-alert))}

;;      :style {:position "fixed"
;;              :font-size "18px"
;;              :border-left (str "2px solid " (theme-pull :theme/editor-outer-rim-color nil) (if @db/kick-alert "" "01"))
;;              :border-top (str "2px solid " (theme-pull :theme/editor-outer-rim-color nil) (if @db/kick-alert "" "01"))
;;              :border-bottom (str "2px solid " (theme-pull :theme/editor-outer-rim-color nil) (if @db/kick-alert "" "01"))
;;              :font-weight 700
;;              :cursor "pointer"
;;              :z-index 98
;;              :border-radius "19px 0px 0px 19px"
;;              :bottom 25
;;              :transition "all 0.6s ease-in-out"
;;              :right (if @db/kick-alert 0 -370)
;;              :backdrop-filter "blur(4px)"
;;              :background-color (str "#000000" (if @db/kick-alert 88 11))
;;              :color "white"}]))


(re-frame/reg-sub
 ::estimates
 (fn [db _]
   (get db :flow-estimates)))

(defn alert-box []
  [rc/catch
   (let [rekt [@db/kick-alert @db/pause-alerts]
         rs-running @(re-frame/subscribe [::bricks/runstreams-running])
         rs-running-list @(re-frame/subscribe [::bricks/runstreams-running-list])
         alerts @(re-frame/subscribe [::bricks/alerts])
         ;;tick (rand-int 12345)
         ;tick "" ;(str (js/Date.now) "-" (rand-int 1000000))
         estimates @(re-frame/subscribe [::estimates])
         max-w (apply max (for [a alerts
                                :let [width (* (get a 1) bricks/brick-size)]]
                            width))
         max-w (if (or (nil? max-w) (< max-w 50))
                 300 ;420
                 max-w)
         alerts (if (> rs-running 0)
                  (conj alerts [[:v-box
                                 :children
                                 (vec (into [[:box
                                              ;:style {:border "1px solid red"}
                                              :child (str rs-running " flow" (when (> rs-running 1) "s") " running")]]
                                            (vec (for [e rs-running-list
                                                       :let [fid (ut/replacer e ":" "")
                                                             run-id (get-in estimates [fid :run-id])
                                                             est (+ (js/Math.round (get-in estimates [fid :times] 0)) 1)
                                                             est? (> est 0)]]
                                                   [:v-box
                                                    :padding "3px"
                                                    :width (px max-w) ;;"215px"
                                                    :size "auto" ;:justify :center
                                                    :style {:font-size "11px"
                                                            ;:border "1px solid red"
                                                            }
                                                    :children [[:box
                                                                :size "auto"
                                                                :style {:padding-left "5px"}
                                                                :child (str fid)]
                                                               (when true ;est?
                                                                 [:box :child [:progress-bar [(- max-w 15) est (str e run-id)]] :height "25px" :padding "3px"])
                                                               (when true ;est?
                                                                 [:box :align :end
                                                                  :style {:font-size "10px" :font-weight 400 :padding-right "5px"}
                                                                  :child (str "estimate: " (ut/format-duration-seconds est))])]]))))]
                                4 (+ 1.25 (* 1.05 rs-running)
                                     (when (= 1 rs-running) -0.275)) 0])
                  alerts)
         ;;alerts (reverse alerts)
         ;;alerts (conj alerts (when (> rs-running 0) [[:box :child (str rs-running " flows running")] 8 1 0]))

         alerts-cnt (try (count alerts) (catch :default _ 0))
         gap 1 ;11
         all-h (apply + (for [a alerts
                              :let [h (* (get a 2) bricks/brick-size)]]
                          h))
         all-h (if (> alerts-cnt 1) (+ all-h (* gap (dec alerts-cnt))) all-h)
         all-h (if (or (nil? all-h) (< all-h 50)) 50 all-h)
         box-height 50
         box-width (+ 80 max-w) ;420

         edge-hide (* (- box-width 50) -1)]
     ;(tap> [:widths max-w all-h edge-hide])
     (when (= alerts-cnt 0) (reset! db/kick-alert false))
     (when (> alerts-cnt 0) (reset! db/kick-alert true))
     [re-com/box
      :child [re-com/h-box
              :attr {:on-mouse-enter #(reset! db/pause-alerts true)
                     :on-mouse-over #(when (not @db/pause-alerts)
                                       (reset! db/pause-alerts true))
                     :on-mouse-leave #(reset! db/pause-alerts false)}
              :children
              [;[:img {:src "images/test-kick-icon.png" :width 30 :height 30}]
               [re-com/md-icon-button :src (at)
                :md-icon-name (if @db/kick-alert "zmdi-star" "zmdi-star-outline")
                :style {;:color "red"
                        :transition "all 0.4s ease-in-out"
                        :color (if @db/pause-alerts (theme-pull :theme/editor-outer-rim-color nil) "inherit")
                        :position "fixed"
                        :bottom 17
                        :margin-right "12px"
                        :margin-top "-4px"
                        :font-size "34px"}]
               [re-com/gap :size "44px"]
               [re-com/v-box
               ;:style {:opacity (if @db/kick-alert 1.0 0.0)}
               ;:gap "4px"
                :children ;(into (for [e (range rs-running)] [re-com/box :child "o"])
                (for [a alerts
                      :let [abody (first a)
                            push-codes? (try (some #(or (= % :push) (= % :dialog-push)) (ut/deep-flatten abody)) (catch :default _ false))
                            width (+ 60 (* (get a 1 0) bricks/brick-size))
                            height (* (get a 2 0) bricks/brick-size)
                            alert-id (last a)]]
                  [re-com/box
                   :size "none"
                   :attr (when (not push-codes?)
                           (if @db/kick-alert
                            ;{:on-click #(re-frame/dispatch [::bricks/prune-alerts true])}
                             {:on-click #(re-frame/dispatch [::bricks/prune-alert alert-id])}
                             {}))
                   :width (when (> width 0) (px width))
                   :height (when (> height 0) (px height))
                   :child [buffy/render-honey-comb-fragments abody (get a 1) (get a 2)]])]]]

      :width (px (+ 0 box-width))
      :height (px (if @db/kick-alert all-h box-height))
      :padding "9px"
     ;:attr {:on-click #(reset! db/kick-alert (not @db/kick-alert))}
      ;:attr (if @db/kick-alert {:on-click #(re-frame/dispatch [::bricks/prune-alerts true])} {})
      :style {:position "fixed"
              :font-size "18px"
              :border-left (str "2px solid " (theme-pull :theme/editor-outer-rim-color nil) (if @db/kick-alert "" "01"))
              :border-top (str "2px solid " (theme-pull :theme/editor-outer-rim-color nil) (if @db/kick-alert "" "01"))
              :border-bottom (str "2px solid " (theme-pull :theme/editor-outer-rim-color nil) (if @db/kick-alert "" "01"))
              :font-weight 700
              :cursor "pointer"
              ;:z-index 98
              :border-radius "19px 0px 0px 19px"
              :bottom 25
              :z-index 9999
              :transition "all 0.6s ease-in-out"
              :right (if @db/kick-alert 0 edge-hide)
              :backdrop-filter "blur(4px)"
              :background-color (str "#000000" (if @db/kick-alert 88 11))
              :color "white"}])])

(re-frame/reg-sub
 ::flow-parts-lookup
 (fn [_ [_ & [part-key]]]
   (let [fflowparts-sys @(re-frame/subscribe [::conn/sql-data [:fflowparts-sys]])
         fflow-sys  @(re-frame/subscribe [::conn/sql-data [:fflows-sys]])
         flow-parts (vec (sort-by :name (into fflowparts-sys fflow-sys)))
         flow-parts-key (when part-key
                          (let [ss (cstr/split (cstr/replace (str part-key) ":" "") "/")
                                cat (keyword (first ss))
                                name (keyword (second ss))]
                            (first (filter #(and (= (get % :category) (str cat))
                                                 (= (get % :name) (str name))) flow-parts))))]
     (if flow-parts-key
       flow-parts-key
       flow-parts))))

(defn spawn-open-input-block [starting-val] ;;; need to simplify and merge logic from flow-droppable (flows, params, etc)
  (let [starting-val (try (edn/read-string starting-val) (catch :default _ starting-val))
        dtype (keyword (ut/data-typer starting-val))
        try-read (fn [x] (try (edn/read-string x) (catch :default _ x)))
        [x y] @detached-coords
        flow?  (= (get starting-val :category) ":flow") ;; also packaged flows... not raws... how to?
        part-key? (true? (and (map? starting-val) (not flow?)
                              (not (nil? (get starting-val :category)))
                              (not (nil? (get starting-val :name)))
                              (not (nil? (get starting-val :full_map)))))
        lookup-map (if part-key? ;; (or part-key? flow?)
                    ;; (merge starting-val (try-read (get starting-val :full_map)))
                     (try (merge
                           (-> (edn/read-string
                                (get starting-val :full_map)))
                           starting-val)
                          (catch :default _ {}))
                     {})
        _ (tap> [:full-lookup-map part-key? flow? starting-val lookup-map])
        raw-fn?         (and (= dtype :list)
                             (= (first starting-val) 'fn))

        _ (when flow? (re-frame/dispatch [::http/load-sub-flow (get starting-val :file_path)]))
        sub-flow (when flow? @(re-frame/subscribe [::sub-flow-incoming]))
        open-block-body {:w 125 :h 60 :z 0
                         :data {:drag-meta {:type :open-block}
                                :flow-item {:expandable? true}
                                :user-input starting-val}
                         :right-click? true  ;; important for add-block to get current coords
                         :ports {:in {}
                                 :out {:out dtype}}}
        flow-as-fn (if flow?
                     (try
                       (let [blocks     (set (keys (get sub-flow :map)))
                             base-conns (set (for [[_ v2] (get sub-flow :connections)] (keyword (gns v2))))
                             no-inputs (cset/difference blocks base-conns)
                             flow-inputs (into {}
                                               (for [i no-inputs]
                                                 {i
                                                  (get-in sub-flow [:map i :ports :out (first (keys (get-in sub-flow [:map i :ports :out])))])}))
                             done-block (try (first (first (filter (fn [x] (= (second x) :done)) (get sub-flow :connections))))
                                             (catch :default _ :error))]
                         (tap> [:sub-flow-build no-inputs base-conns])
                         {:w 200 :h 100 :z 0
                          :data (-> {} ;; (edn/read-string (get starting-val :full_map)) ;;rooted-data
                                    (assoc-in [:drag-meta :done-block] done-block)
                                    (assoc-in [:drag-meta :type] :sub-flow))
                          :sub-flow sub-flow
                          :file-path (get starting-val :file_path)
                          :flow-id (get sub-flow :flow-id)
                          :icon "zmdi-puzzle-piece"
                          :ports {:in flow-inputs ;(dissoc (get flow-item :types) :out)
                                  :out (get-in sub-flow [:map done-block :ports :out])}})
                       (catch :default _ {})) {})
        other-fn (let [f2i (function-to-inputs lookup-map)
                       arg-walks (into {}
                                       (for [e (keys f2i)
                                             :when (cstr/ends-with? (str e) "+")]
                                         {(-> (str e)
                                              (cstr/replace "+" "")
                                              (cstr/replace ":" "")
                                              keyword) e}))]
                   {:w 125 :h 60 :z 0
                    :data
                    {:flow-item
                     {:category (get lookup-map :category) ;; important for lookup server-side! (string fine)
                      :type (try-read (get lookup-map :name))  ;; important for client render (expects keyword)
                      :name (get lookup-map :name)  ;; important for lookup server-side! (string fine)
                      :icon (get lookup-map :icon)
                      :inputs (walk/postwalk-replace arg-walks (get lookup-map :inputs))
                      :defaults (get lookup-map :defaults)
                      :types (walk/postwalk-replace arg-walks (get lookup-map :types))
                      :style (get lookup-map :style)
                      :selected-style (get lookup-map :selected-style)
                      :expandable? true
                      :required (get lookup-map :required)}
                     :drag-meta {:type (try-read (get lookup-map :name))}}
                    :right-click? true ;; important for add-block to get current coords
                    :ports {:in
                          ;; (try
                          ;;       (into {} (for [e (second (get lookup-map :fn))
                          ;;                      :let [kk (keyword (str e))]]
                          ;;                  {kk (get-in lookup-map [:types kk] :any)}))
                          ;;       (catch :default _ {:value :any}))

                            f2i

                            :out {:out (get-in lookup-map [:types :out] :any)}}
                    :icon (get lookup-map :icon)})
        open-fn-body (let [;port-map (try
                           ;           (into {} (for [e (second starting-val)]
                           ;                      {(keyword (str e)) :any}))
                           ;           (catch :default _ {:value :any}))
                           port-map (try (function-to-inputs (second starting-val)) (catch :default _ {:value :any}))
                           ;; (fn [aa bb & ff] (+ aa bb))
                          ;;  arg-walks (into {}
                          ;;                  (for [e (keys port-map)
                          ;;                        :when (cstr/ends-with? (str e) "+")]
                          ;;                    {(-> (str e)
                          ;;                         (cstr/replace "+" "")
                          ;;                         (cstr/replace ":" "")
                          ;;                         keyword) e}))
                           ]
                       {:fn starting-val
                        :w 125 :h 60 :z 0
                        :raw-fn starting-val
                        :icon "zmdi-functions"
                        :right-click? true  ;; important for add-block to get current coords
                        :ports {:in port-map
                                :out {:out :any}}
                        :data
                        {:flow-item
                         {:category ":rabbit-base"
                          :fn starting-val
                          :name ":open-fn"
                          :raw-fn starting-val
                          :type :open-fn
                          :icon "zmdi-functions"
                          :types (merge port-map {:out :any})
                          :expandable? true
                          :drag-meta {:type :open-fn}}}})
        grid-size snap-to-grid
        sx (- (first @db/context-modal-pos) x 10)
        sy (- (last @db/context-modal-pos) y 33)
        snapped-x       (* grid-size (Math/round (/ sx grid-size)))
        snapped-y       (* grid-size (Math/round (/ sy grid-size)))
        block-body (cond raw-fn? open-fn-body
                         part-key? other-fn
                         flow? flow-as-fn
                         :else open-block-body)
        bname (cond raw-fn? :open-fn
                    part-key? (try-read (get lookup-map :name)) ;;:test ;(get lookup-map )
                    flow? (try-read (get sub-flow :flow-id))
                    :else :open-input)]
    ;(tap> [:spawner @(re-frame/subscribe [::flow-parts-lookup :clojure-base/*])])
    (add-block snapped-x snapped-y block-body bname)))

(def lookup-val (reagent/atom nil))

(defn lookup-modal []
  (let [[left top] @db/context-modal-pos
        ;;val (reagent/atom nil)
        ;react! [@lookup-val]
        [xx yy] @detached-coords
        left (- left xx)
        top (- top yy)
        flow-parts @(re-frame/subscribe [::flow-parts-lookup])
        sql-calls {:fflows-sys {:select [[":flow" :category] ["" :description] [:flow_id :name] :file_path [:body :full_map] ["zmdi-developer-board" :icon]]
                                :from [:flows]
                                :connection-id "flows-db"
                                :order-by [[3 :asc]]}
                   :fflowparts-sys {:select [:category :description :name :file_path :full_map :icon]
                                    :order-by [[3 :asc]]
                                    :from   [:flow_functions]}}]
    ;; get all parts, AND get all other flows to import as a part... PLUS we allow literals and function starters
    ;; showertime first
    (dorun (for [[k query] sql-calls]
             (let [data-exists? @(re-frame/subscribe [::conn/sql-data-exists? [k]])
                   unrun-sql? @(re-frame/subscribe [::conn/sql-query-not-run? [k] query])]
               (when (or (not data-exists?) unrun-sql?)
                 (if (get query :connection-id)
                   (conn/sql-data [k] query (get query :connection-id))
                   (conn/sql-data [k] query))))))

    ;;(tap> [:flow-parts flow-parts ])

    [re-com/modal-panel
     :style {:z-index 999
             ;:color "#ffffff33"
             :padding "0px"}
     :parts {:child-container
             {:style {:left left
                      :transform "scale(1.5)"
                      :top top
                      :font-size "8px"
                      :border (str "5px solid " (theme-pull :theme/editor-outer-rim-color nil))
                      ;:background-color "orange"
                      :position "fixed"}}}
     :backdrop-opacity 0.63
     :backdrop-on-click #(reset! lookup-modal? false)
     :child [re-com/h-box
             ;:size "none"
             ;:width "500px"
             :justify :between :align :center :gap "10px"
             :style {:color "#000000"
                     ;:background-color "blue"
                     :font-size "18px"}
             :children [;[re-com/box :child "topper"]
                        [re-com/typeahead
                         :width "250px"
                         :suggestion-to-string (fn [item]
                                                 (str (get item :category) "/" (cstr/replace (str (get item :name)) ":" ""))) ;; render in input box (must be string, no hiccup :())
                         :render-suggestion (fn [ss _] ;; render in dropdown
                                              [re-com/h-box
                                               :style {:border-top  "1px solid #00000009"}
                                               :size "none" :width "240px"
                                               :justify :between :align :center
                                               :children
                                               [[re-com/v-box
                                                 :children [[re-com/h-box
                                                            ;;:width "200px" :size "none"
                                                             :justify :between :align :center
                                                             :children [[re-com/box
                                                                         :style {:font-weight 700
                                                                                 :font-size "12px"}
                                                                         :child (str (get ss :name))]]]
                                                            (when (not (empty? (get ss :description)))
                                                              [re-com/box
                                                               :style {:font-size "7px"}
                                                               :child (str (get ss :description))])]]
                                                [re-com/v-box
                                                 :align :end :justify :end
                                                 :children
                                                 [[render-icon (str (get ss :icon)) "#00000044" 20 15]
                                                  [re-com/box
                                                   :style {:font-size "6px"
                                                           :color "#00000066"
                                                           :font-weight 700}
                                                   :child (str (get ss :category))]]]]])
                         :on-change #(reset! lookup-val %)
                         :rigid? false
                         ;:immediate-model-update? true
                         ;:change-on-blur? false
                         :placeholder "what do you want to add?"
                        ;;  :data-source (fn [x] (let [xt (cstr/lower-case (cstr/trim x))]
                        ;;                         (if (or (nil? xt) (empty? xt)) flow-parts
                        ;;                             (filter #(or (cstr/includes? (cstr/lower-case (str (get % :category))) xt)
                        ;;                                          (cstr/includes? (cstr/lower-case (str (get % :name))) xt)
                        ;;                                          (cstr/includes? (cstr/lower-case (str (get % :description))) xt))
                        ;;                                     flow-parts))))
                         :data-source (fn [x]
                                        (let [words (cstr/split (cstr/lower-case (cstr/trim x)) #" ")
                                              matches-word (fn [field word] (cstr/includes? (cstr/lower-case (str field)) word))]
                                          (if (or (nil? x) (empty? x)) flow-parts
                                              (filter (fn [item]
                                                        (let [category (get item :category)
                                                              name (get item :name)
                                                              description (get item :description)]
                                                          (every? (fn [word]
                                                                    (or (matches-word category word)
                                                                        (matches-word name word)
                                                                        (matches-word description word)))
                                                                  words)))
                                                      flow-parts))))]
                        [re-com/box
                         :child [re-com/md-icon-button
                                 :src (at)
                                 :md-icon-name "zmdi-plus"
                                 :style {;:color (theme-pull :theme/editor-outer-rim-color nil)
                                         :font-size "22px"
                                         :cursor "pointer"}]
                         :padding "6px"
                         :style {:background-color (str (theme-pull :theme/editor-outer-rim-color nil) 33)
                                 :color "#00000089"
                                 ;:font-size "10px"
                                 :font-weight 700
                                 :border-radius "8px"
                                 :cursor "pointer"}
                         :attr {:on-click #(do (spawn-open-input-block @lookup-val)
                                               (reset! lookup-modal? false))}]

                        [re-com/box
                         :child [re-com/md-icon-button
                                 :src (at)
                                 :md-icon-name "zmdi-refresh"
                                 :style {:color "#00000038"
                                         :font-size "15px"
                                         :cursor "pointer"}]
                         :padding "6px"
                         :style {:font-weight 700
                                 :border-radius "8px"
                                 :cursor "pointer"}
                         :attr {:on-click #(do (re-frame/dispatch [::conn/clear-query-history :fflows-sys])
                                               (re-frame/dispatch [::conn/clear-query-history :fflowparts-sys]))}]]]]))

(re-frame/reg-event-db
 ::flip-drop-toggle
 (fn [db _]
   (reset! drop-toggle? (not @drop-toggle?))
   db))

(re-frame/reg-sub
 ::zoom-unlocked?
 (fn [db _]
   (get db :alt-key-held? false)))

(re-frame/reg-event-db
 ::toggle-lock
 (fn [db _]
   (assoc db :alt-key-held? (not (get db :alt-key-held?)))))

(defn flow-panel []
  (let [[x y] @detached-coords
        x-px (px x)
        y-px (px y)
        [wpct hpct] [0.85 0.50]
        panel-width (* (.-innerWidth js/window) wpct)
        hh @(re-frame/subscribe [::subs/h]) ;; to ensure we get refreshed when the browser changes
        ww @(re-frame/subscribe [::subs/w])
        flow-id @(re-frame/subscribe [::selected-flow])
        zoom-unlocked? @(re-frame/subscribe [::zoom-unlocked?])
        flowmaps @(re-frame/subscribe [::flowmap-raw])
       ;; flowmappp [@(re-frame/subscribe [::flowmap]) @ports-react-render]
        flowmaps-connections @(re-frame/subscribe [::flowmap-connections])
       ;flowmaps-connections @(re-frame/subscribe [::flowmap-connections])
        ;flow-id @(re-frame/subscribe [::selected-flow])
        flow-map @(re-frame/subscribe [::flowmap])
        audio-playing? @(re-frame/subscribe [::audio/audio-playing?])
        ;running? (is-running? :* flow-id)
        running? @(re-frame/subscribe [::is-running? :* flow-id true])
        chans-open? @(re-frame/subscribe [::bricks/flow-channels-open? flow-id])
        has-done? (has-done?)
        ;single-panel-size 600
        panel-height (* (.-innerHeight js/window) hpct) ;;600 ;;(* hh 0.65) ;;400
        ;; _ (tap> [:sizes ww hh
        ;;          (.-innerWidth js/window)
        ;;          (.-innerHeight js/window)])

        details-panel-height (/ panel-height 1.25)
        ppanel-height (+ panel-height details-panel-height)]
        ;flow-id @(re-frame/subscribe [::selected-flow])

        ;coords (generate-coords x y)
        ;fart (cstr/replace nil "5" "55") ;; error test
        ;choices [[:yo :yo] [:yo1 :yo1] [:fook :fook]]
        ;; choices [[:o (str "@bricks/over-flow? " @bricks/over-flow?)]
        ;;          [:o0 (str "@bricks/swap-layers?" @bricks/swap-layers?)]
        ;;          [:o1 (str "@dragging-port? " @dragging-port?)]
        ;;          ;[:o1 (str "@flow-select " @flow-select)]
        ;;          [:o2 (str :coords @db/pan-zoom-offsets)]]


        ;;choices []

    ;; (tap> [:port-hover @port-hover])
    ;;(tap> [:db/flow-results @(re-frame/subscribe [::http/flow-results])])
    ;(tap> [:wtf-man flow-id running? chans-open?])

    [rc/catch
     [re-com/box
      :size "none"
      :width (px panel-width)
      :height (px ppanel-height) ;(if @flow-details-panel? (px (+ panel-height (/ panel-height 1.33))) (px panel-height))
      :attr (if (and (not @db/dragging-flow-editor?) (not @bricks/dragging-editor?))
              {;:on-mouse-enter #(do (reset! bricks/over-flow? true)
               ;                     (reset! bricks/over-block? true))
               :on-mouse-over #(when (not @bricks/over-flow?)
                                 (do (reset! bricks/over-flow? true)
                                     (reset! bricks/over-block? true)))
               :on-mouse-leave #(when
                                 (not (contains? (.-target %) (.-relatedTarget %)))
                                  (do
                                    (reset! bricks/over-flow? false)
                                    (reset! dragging-port? false)
                                    (reset! dragged-port [])
                                    (reset! bricks/over-block? false)))
               :on-drag-over #(do (when (not @bricks/over-flow?) ;(contains? (.-target %) (.-relatedTarget %)) ;(not @bricks/over-flow?)
                                    (reset! bricks/over-flow? true)))
                                ;  (reset! bricks/over-block? true)

        ;;        :on-drag-leave #(do
        ;;                          (reset! bricks/over-flow? false)
        ;;                        ; (reset! bricks/over-block? false)
        ;;                          )
               :on-drag-leave #(when (not (contains? (.-target %) (.-relatedTarget %)))
                                 ;(reset! bricks/swap-layers? true) ;; turned off for now. ghost block dragging out flow steps
                                 (reset! bricks/over-flow? false))}
            ;;    :on-drag-enter #(do
            ;;                      (reset! bricks/over-flow? true)
            ;;                    ; (reset! bricks/over-block? false)
            ;;                      )

              {})
      :style {:position "fixed"
              :top y-px
              :left x-px
              :border-radius "16px" ; (if (and @flow-details-panel? (not @db/dragging-flow-editor?)) "16px 16px 0px 0px" "16px")
              :z-index 200
              :font-family (theme-pull :theme/base-font nil)
              :color (theme-pull :theme/editor-font-color nil)
              :background-color (str (theme-pull :theme/editor-background-color nil) 99) ; "#000000"
              :backdrop-filter "blur(3px)"
              :border (str "6px solid " (theme-pull :theme/editor-outer-rim-color nil))
              :box-shadow (let [block-id :audio
                               ; block-type :speak
                                talking-block? true]
                            (cond (and audio-playing? talking-block?)
                                  (str "1px 1px " (px (* 80 (+ 0.1
                                                               (get @db/audio-data block-id))))  " "
                                       (theme-pull :theme/editor-outer-rim-color nil))
                                                           ;(str "1px 1px 70px red")
                                  ;(or (and hovered? @on-block) kit-drop?)
                                  ;(str "1px 1px 30px " (get db/block-codes block-type) 87)
                                  :else "none"))
              ;:border-left (str "6px solid " (theme-pull :theme/editor-outer-rim-color nil))
              ;:border-right (str "6px solid " (theme-pull :theme/editor-outer-rim-color nil))
              ;:border-bottom (if (and @flow-details-panel? (not @db/dragging-flow-editor?))
              ;                 (str "6px solid " (str (theme-pull :theme/editor-background-color nil) 99))
              ;                 (str "6px solid " (theme-pull :theme/editor-outer-rim-color nil)))
              :filter "drop-shadow(0.35rem 0.35rem 0.4rem rgba(0, 0, 0, 0.8))"}
      :child (if false ;@db/dragging-flow-editor?
               [re-com/box
                :size "auto" :align :center :justify :center
                :child "dragging panel. paused"]
               [re-com/v-box
                :width (px panel-width)
              ;:style {:margin-top "5px"}
                :children [[re-com/h-box
                            :justify :between :align :center
                            :padding "6px"
                            :children [[re-com/md-icon-button :src (at)
                                        :md-icon-name "zmdi-arrows"
                                        :style {;:background-color (theme-pull :theme/editor-rim-color nil) ;"#00000000"
                                                :color (theme-pull :theme/editor-font-color nil)
                                                :cursor "grab"
                                              ;:margin-left "3px"
                                                :height "15px"
                                                :margin-top "-9px"
                                                :font-size "19px"}
                                        :attr {:on-mouse-down mouse-down-handler}]

                                      ;;  [re-com/box :child (str @drop-toggle?)]

                                      ;;  [re-com/h-box
                                      ;;   :children (for [[_ v] choices]
                                      ;;               [re-com/box :child (str v)
                                      ;;               ;;  :attr {:on-click #(do (swap! chat-mode assoc kp c)
                                      ;;               ;;                        (swap! kit-mode assoc kp v))}
                                      ;;                :style (if false ;(and (= mode c) (= kmode v))
                                      ;;                        ; {:background-color (get (theme-pull :theme/data-colors db/data-colors) "unknown" "#FFA500") :color "black"}
                                      ;;                         {:text-decoration "underline"}
                                      ;;                         {:cursor "pointer" :opacity 0.6})
                                      ;;                :padding "4px"])]

                                       [re-com/h-box
                                        :gap "2px"
                                        :children  [[re-com/h-box
                                                     :children (for [[v [file-path name]] (get @(re-frame/subscribe [::http/flow-results]) :run-refs [])]
                                                                 [re-com/box :child (str v)
                                                    ;;  :attr {:on-click #(do (swap! chat-mode assoc kp c)
                                                    ;;                        (swap! kit-mode assoc kp v))}
                                                                  :attr {:on-click #(let [sub-flow-exec? (not (nil? file-path))]
                                                                                      (if sub-flow-exec?
                                                                                        (re-frame/dispatch [::http/load-flow-w-alias file-path v])
                                                                                        (re-frame/dispatch [::set-selected-flow (str v)]))
                                                                                      (js/setTimeout (fn []
                                                                                                       (.zoomToElement @db/zoomer @(re-frame/subscribe [::get-flow-brick]) 0.8))
                                                                                                     1000))}

                                                                  :style (if (= (str v) (str flow-id))
                                                             ; {:background-color (get (theme-pull :theme/data-colors db/data-colors) "unknown" "#FFA500") :color "black"}
                                                                           {:text-decoration "underline"}
                                                                           {:cursor "pointer" :opacity 0.6})
                                                                  :padding "4px"])]

                                                   ; [re-com/box :child "_"]

                                                    [re-com/md-icon-button
                                                     :md-icon-name "zmdi-window-minimize"
                                                     :on-click #(re-frame/dispatch [::bricks/toggle-flow])
                                                     :style {:font-size "15px"
                                                             :opacity   0.33
                                                             :cursor    "pointer"}]]]]





                                       ;;(get-in @(re-frame/subscribe [::http/flow-results]) [flow-id :return-maps])

                                      ;;  (if (not @title-edit-idx)
                                      ;;    [re-com/box
                                      ;;     :padding "8px"
                                      ;;     :attr {:on-double-click #(reset! title-edit-idx (str flow-id))}
                                      ;;     :style {;:text-decoration "underline"
                                      ;;             :margin-top "28px"
                                      ;;             :cursor "pointer"
                                      ;;             ;:border "1px solid lime"
                                      ;;             :font-size "38px"}
                                      ;;     :child (str flow-id)]

                                      ;;    [re-com/input-text
                                      ;;     :src (at)
                                      ;;     :model             (str flow-id)
                                      ;;     :width             "600px"
                                      ;;  ; :height            "30px"
                                      ;;     :on-change         #(do (re-frame/dispatch [::set-selected-flow %])
                                      ;;                             (reset! title-edit-idx nil))
                                      ;;     :validation-regex  flow-id-regex
                                      ;;     :change-on-blur?   true

                                      ;;     :style  {;:font-size "20px"
                                      ;;              :margin-top "28px"
                                      ;;              :border (str "2px dashed " (theme-pull :theme/editor-outer-rim-color nil))
                                      ;;              :font-size "38px"
                                      ;;              :text-decoration "underline"
                                      ;;              :color (theme-pull :theme/editor-outer-rim-color nil)
                                      ;;                  ;:font-weight 700
                                      ;;              :font-style "underline"
                                      ;;            ;:border "0px solid black"
                                      ;;              :padding "8px"
                                      ;;              :text-align "right"
                                      ;;            ; :float "right"
                                      ;;                ;:margin-top "10px"
                                      ;;                ;:padding-top "10px"
                                      ;;      ;          :text-align "right"
                                      ;;              :background-color "#00000000"
                                      ;;              ;:color (theme-pull :theme/editor-font-color nil)
                                      ;;      ;          :padding-top "1px"
                                      ;;              }])


                            :size "none"
                            :width (px (- panel-width 12))
                            :height "25px"
                            :style ;{:background-color (theme-pull :theme/editor-rim-color nil)
                                   ; :border-radius "10px 10px 0px 0px"
                                   ; :color (theme-pull :theme/editor-outer-rim-color nil)}
                            {:background (str "linear-gradient("
                                              (theme-pull :theme/editor-rim-color nil)
                                              ", transparent)")
                             :border-radius "10px 10px 0px 0px"
                             :color (theme-pull :theme/editor-outer-rim-color nil)}]

                           ;[re-com/box :child "content!" :align :center :justify :center]

                           (flow-droppable ["meta-menu" :flow-port]
                                           [(- (first @db/context-modal-pos) x 10)
                                            (- (last @db/context-modal-pos) y 33)]
                                           [re-com/box
                                            :style {;:background-color "#000000"
                                                    :border-radius "12px"}
                                            :attr {:id "flow-canvas"
                              ;:transition_NOT "all 0.1s ease-in-out"
                              ;:transform-style "preserve-3d"
                              ;:transform "scale(0.5)"
                              ;:on-click (fn [x] (tap> [(.-clientX x)
                              ;                         (.-clientY x)]))
                              ;:on-drag mouse-down-handler2
                             ; :on-mouse-over #(reset! on-canvas? true)
                             ; :on-mouse-leave #(reset! on-canvas? false)
                             ; :on-mouse-down mouse-down-handler2
                                                 ;    :on-drag-over #(bricks/tag-screen-position %)  ;;; was redundant? 12/15/23
                                                 ;    :on-mouse-enter #(reset! bricks/over-flow? true)
                                                 ;    :on-mouse-leave #(reset! bricks/over-flow? false)
                        ;;   :on-context-menu (re-com/handler-fn
                        ;;                     #_{:clj-kondo/ignore [:unresolved-symbol]}
                        ;;                     (bricks/tag-screen-position event) ;; event is magic in handler-fn
                        ;;                     (when (not @bricks/over-block?)
                        ;;                       (bricks/insert-new-block [(js/Math.floor (/ (first @db/context-modal-pos) bricks/brick-size))
                        ;;                                                 (js/Math.floor (/ (last @db/context-modal-pos) bricks/brick-size))] 5 4))
                        ;;                     #_{:clj-kondo/ignore [:unresolved-symbol]}
                        ;;                     (.preventDefault event))
                                                   :on-mouse-down #(when (= (.-button %) 1)
                                                                     (do
                                                                       #_{:clj-kondo/ignore [:unresolved-symbol]}
                                                                       (bricks/tag-screen-position %) ;; event is magic in handler-fn
                                                                       (when (nil? @flow-hover) ;(and ;(not @bricks/over-block?)  (not @flow-hover))
                                                                         (let [;block-body {:w 200 :h 60
                                                                              ;            :data {:drag-meta {:type :open-block}
                                                                              ;                   :user-input "feed me, seymour!"}
                                                                              ;                ;:x drop-x :y drop-y
                                                                              ;            :z 0
                                                                              ;            :ports {;;;:in {:in :string}
                                                                              ;                    :out {:out :string}}}
                                                                               block-body {:w 125
                                                                                           :icon "zmdi-functions"
                                                                                           :z 0
                                                                                           :ports {:in {:x :any}
                                                                                                   :out {:out :any}}
                                                                                           :h 60
                                                                                           :right-click? true
                                                                                           :fn '(fn [x] x)
                                                                                           :raw-fn '(fn [x] x)
                                                                                           :data {:flow-item {:category ":rabbit-base"
                                                                                                              :fn '(fn [x] x)
                                                                                                              :name ":open-fn"
                                                                                                              :raw-fn '(fn [x] x)
                                                                                                              :type :open-fn
                                                                                                              :icon "zmdi-functions"
                                                                                                              :types {:x :any :out :any}
                                                                                                              :expandable? true
                                                                                                              :drag-meta {:type :open-fn}}}}]
                                                                           (add-block (- (first @db/context-modal-pos) x 10)
                                                                                      (- (last @db/context-modal-pos) y 33) block-body :open-fn)))
                                                                       #_{:clj-kondo/ignore [:unresolved-symbol]}
                                                                       (.preventDefault %)))
                                                  ;;  :on-context-menu (re-com/handler-fn ;; original spawn open input block (1/28/24)
                                                  ;;                    #_{:clj-kondo/ignore [:unresolved-symbol]}
                                                  ;;                    (bricks/tag-screen-position event) ;; event is magic in handler-fn
                                                  ;;                    (when (nil? @flow-hover) ;(and ;(not @bricks/over-block?)  (not @flow-hover))
                                                  ;;                      (let [starting-val "feed me, seymour!"
                                                  ;;                            open-block-body {:w 200 :h 100
                                                  ;;                                             :data {:drag-meta {:type :open-block}
                                                  ;;                                                    :flow-item {:expandable? true}
                                                  ;;                                                    :user-input starting-val}
                                                  ;;                                             ;:x drop-x :y drop-y
                                                  ;;                                             :z 0
                                                  ;;                                             :ports {;;;:in {:in :string}
                                                  ;;                                                     :in {}
                                                  ;;                                                     :out {:out :string}}}]
                                                  ;;                        (add-block (- (first @db/context-modal-pos) x 10)
                                                  ;;                                   (- (last @db/context-modal-pos) y 33) open-block-body :open-input)))
                                                  ;;                    #_{:clj-kondo/ignore [:unresolved-symbol]}
                                                  ;;                    (.preventDefault event))
                                                   :on-context-menu (re-com/handler-fn
                                                                     #_{:clj-kondo/ignore [:unresolved-symbol]}
                                                                     (bricks/tag-screen-position event) ;; event is magic in handler-fn
                                                                     (when (and (empty? @port-hover2)
                                                                                (empty? @flow-hover)) ; (nil? @flow-hover) ;(and ;(not @bricks/over-block?)  (not @flow-hover))
                                                                       ;; spawn lookup modal at position?
                                                                       (reset! lookup-modal? true))
                                                                     #_{:clj-kondo/ignore [:unresolved-symbol]}
                                                                     (.preventDefault event))}
                                            :size "none"
                                            :width (px (- panel-width 12))
                                            ;; :height (if (and ;; @flow-details-panel?
                                            ;;                  (not @bricks/dragging-editor?)
                                            ;;                  (not @db/dragging-flow-editor?))
                                            ;;           (px (- ppanel-height 35))
                                            ;;           (px (- panel-height 35)))
                                            :height (px (- ppanel-height 35))
                                            :child [re-com/v-box
                                                    :size "1"
                                                    :style (merge ;(theme-pull :theme/canvas-background-css nil)
                                                            {:font-family (theme-pull :theme/base-font nil)
                                                             :z-index 200
                                                             ;:border "2px solid lime"
                                                             ;:overflow "hidden"
                                                             :border-radius "12px"}) ; "0px 0px 12px 12px"

                               ;; {:font-family "Alata" ;"Roboto" ;"Lato"
                              ;;          ;:background-size "50px 50px"
                              ;;          ;:background-image "linear-gradient(to right, #00000055 1px, transparent 1px),linear-gradient(to bottom, #00000055 1px, transparent 1px)"
                              ;;          :background-image "url(images/fake-brick.png)"
                              ;;          :background-repeat "repeat"
                              ;;          :background-color "#47555e"}

                                                    :children [;;   [tab-menu]
                                    ;;   [snapshot-menu]

                                    ;;   (when (and editor? (not @bricks/mouse-dragging-panel?))
                                    ;;     [rc/catch [editor-panel 33 10]])

                                    ;;  [menu-panel]

                                    ;;; drop drawing
                                                               (when (and @bricks/dragging? @bricks/over-flow? (not @dragging-port?)
                                                                          (not @bricks/on-block?)) ;; AND when inside flow canvas bounds
                                                                 (let [rel-x (first @db/context-modal-pos) ;(* (js/Math.floor (/ (first @db/context-modal-pos) bricks/brick-size)) bricks/brick-size)
                                                                       left-pos (- rel-x x)
                                                                       rel-y (last @db/context-modal-pos) ;(* (js/Math.floor (/ (last @db/context-modal-pos) bricks/brick-size)) bricks/brick-size)
                                                                       top-pos (- rel-y y)
                                                                       dt @drop-toggle?
                                                                       x-bounds (+ x panel-width)
                                                                       y-bounds (+ y ppanel-height)
                                                                       load-flow? (= (get-in @bricks/dragging-body [:drag-meta :source-query]) :flows-sys)
                                                                       in-bounds? (and  @bricks/over-flow?
                                                                                        (>= rel-x x)
                                                                                        (>= rel-y y)
                                                                                        (<= rel-x x-bounds)
                                                                                        (<= rel-y y-bounds))]

                                                                  ; (tap> [:stuff @bricks/dragging? @bricks/on-block? @bricks/over-flow?])

                                                                   (if (not in-bounds?) (do ;very weird, but have lots of drop/drag in-fighting with crossing the streams. TODO
                                                                                            ; it works though!
                                                                                          (reset! bricks/over-flow? false)
                                                                                          (reset! bricks/swap-layers? true)
                                                                                          (reset! bricks/over-block? false))
                                                                       (reset! bricks/swap-layers? false))

                                                                   (when in-bounds? ;; draw that shit
                                                                     [re-com/box
                                                                      :child (if load-flow?
                                                                               (if @drop-toggle?
                                                                                 " *ADD to flow "
                                                                                 " *LOAD this flow ")
                                                                               " add to flow canvas ")
                                                                      :align :center :justify :center
                                                                      :style {:background-color (str (theme-pull :theme/editor-background-color nil) 22) ;; "#00000022"
                                                                              ;:filter "drop-shadow(16px 16px 20px orange)" ;;  invert(175%)
                                                                              :border "3px solid orange"
                                                                              :position "fixed"
                                                                              :font-size "26px"
                                                                              :font-weight 700
                                                                              :border-radius "14px"
                                                                              :left left-pos
                                                                              :top top-pos}
                                                                      :width (px (*
                                                                                  5 ;(get @bricks/dragging-body :w)
                                                                                  bricks/brick-size))
                                                                      :height (px (*
                                                                                   2 ;(get @bricks/dragging-body :h)
                                                                                   bricks/brick-size))])))

                                                               (when (and (or (nil? @flow-hover) (not @flow-hover))
                                                                          @lookup-modal?)
                                                                 [rc/catch [lookup-modal]])

                                                                    ;(let [] ; [pan-zoom-offsets (reagent/atom [10 -33 1])] ;;[_ (when-let [f (-> @db/zoomer-state .-resetTransform)] (f))] ;;[react-hack [@db/pan-zoom-offsets]]
                                                               [re-com/box
                                                                :size "none" :style {;:border "2px solid red"
                                                                                     :overflow "hidden"}
                                                                :width (px (- panel-width 12))
                                                                :height (px (- ppanel-height 35))
                                                                :child [(reagent/adapt-react-class zpan/TransformWrapper)
                                                                        {;:key (str @db/pan-zoom-offsets)
                                                                         :ref #(reset! db/zoomer %)
                                                                         :onInit (fn [comp] (reset! db/zoomer-state comp))
                                                                         :limitToBounds false ;true ; false ; true ; false ;true ;false
                                                                         :centerZoomedOut false ;true
                                                                         :disabled (or @flow-hover (not zoom-unlocked?) ;; @pan-lock?
                                                                                       (not @bricks/over-flow?))
                                                                ;;       :disabled (or @canvas/dragging? @canvas/mouse-dragging-panel? @canvas/dragging-pill?
                                                                ;;                     @canvas/on-block) ; @canvas/on-block )
                                                                         :onPanningStop #(do (reset! db/pan-zoom-offsets  [(.-positionX (.-state %)) (.-positionY (.-state %)) (.-scale (.-state %))])
                                                                                             (reset! db/panning? false))
                                                                         :onPanningStart #(reset! db/panning? true)
                                                                         :onWheelStop #(do (reset! db/pan-zoom-offsets  [(.-positionX (.-state %)) (.-positionY (.-state %)) (.-scale (.-state %))])
                                                                                           (reset! db/zooming? false))
                                                                         :onWheelStart #(reset! db/zooming? true)
                                                                         :doubleClick {:disabled false}
                                                                        ;:onDoubleClick #(when-let [f (-> @db/zoomer-state .-resetTransform)] (f))
                                                                         :initialScale (nth @db/pan-zoom-offsets 2)
                                                                         :initialPositionX (nth @db/pan-zoom-offsets 0)
                                                                         :initialPositionY (nth @db/pan-zoom-offsets 1)
                                                                         ;:wheel {:step 0.05}
                                                                         :minScale 0.2}
                                                                        [(reagent/adapt-react-class zpan/TransformComponent)
                                                                         [re-com/v-box ;:style {:border "1px dashed hotpink"}
                                                                          :children [[rc/catch
                                                                                      [flow-grid panel-width ppanel-height x y flowmaps-connections flow-id flow-map]]

                                                                                     (let [ph 600
                                                                                           pw (* (.-innerWidth js/window) 0.7)
                                                                                           [xx yy] @detached-coords]
                                                                                 ;offsets @db/pan-zoom-offsets
                                                                                 ;tx (first offsets)
                                                                                 ;ty (second offsets)
                                                                                 ;scale (nth offsets 2)

                                                                                       [:svg
                                                                                        {:style {;; :width  (px pw) ;(px ww) ;"6200px" ;; big ass render nothing gets cut off svg-wise + zoom and pannable
                                                                                         ;; :height (px ph) ;(px hh) ;"6200px"
                                                                                                 :width canvas-width ;"3200px" ;"6400px" ;(px ww)
                                                                                                 :height canvas-height ;"2400px" ;"3600px" ;(px hh)
                                                         ;:width   (px ww) ;"6200px" ;; big ass render nothing gets cut off svg-wise + zoom and pannable
                                                         ;:height  (px hh) ;"6200px"
                                                                                                 ;:border "2px solid white"
                                                                                                 :position "fixed"
                                                                                                 :pointer-events "none"
                                                                                       ;:left xx :top yy
                                                                                       ;:transform (str "translate(" tx "px, " ty "px) scale(" scale ")")
                                                         ;:z-index 499
                                                                                                 :z-index 100}}
                                                                                        (draw-lines (generate-coords xx yy))])]]]]]
                                                                                        ;; (when @dragging-port?
                                                                                        ;;   (draw-lines [(generate-tentacle-coord)]))
;)





                                                        ;;        [re-com/box
                                                        ;;         :child [re-com/md-icon-button :src (at)
                                                        ;;                 :md-icon-name "zmdi-pizza"
                                                        ;;                 :tooltip "  toggle floating editor panel (SPACE)"
                                                        ;;                 :style {:color "#ffffff"
                                                        ;;                         :cursor "pointer"
                                                        ;;                         :margin-top "-2px"
                                                        ;;                         :padding-left "2px"
                                                        ;;                         :font-size "15px"}
                                                        ;;                 :attr {:on-click #(re-frame/dispatch [::bricks/toggle-editor])}]
                                                        ;;         :width "20px"
                                                        ;;         :height "20px"
                                                        ;;         :style {:position "fixed"
                                                        ;;                 :z-index 98
                                                        ;;                 :top (- panel-height 33) :left 0
                                                        ;;                 :background-color "#00000022"
                                                        ;;                 :color "white"}]

                                                        ;;        [re-com/box
                                                        ;;         :child [re-com/md-icon-button :src (at)
                                                        ;;                 :md-icon-name "zmdi-layers"
                                                        ;;                 :tooltip "show lineage / sub-query lines? (L)"
                                                        ;;                 :style {:color "#ffffff"
                                                        ;;                         :cursor "pointer"
                                                        ;;                         :margin-top "-2px"
                                                        ;;                         :padding-left "2px"
                                                        ;;                         :font-size "15px"}
                                                        ;;                 :attr {:on-click #(re-frame/dispatch [::bricks/toggle-lines])}]
                                                        ;;         :width "20px"
                                                        ;;         :height "20px"
                                                        ;;         :style {:position "fixed"
                                                        ;;                 :z-index 98
                                                        ;;                 :border-radius "0px 7px 0px 0px"
                                                        ;;                 :top (- panel-height 33)
                                                        ;;                 :left 23
                                                        ;;                 :background-color "#00000022"
                                                        ;;                 :color "white"}]

                                                        ;;        [re-com/box
                                                        ;;         :child [re-com/md-icon-button :src (at)
                                                        ;;                 :md-icon-name "zmdi-eye"
                                                        ;;                 :tooltip "toggle peek mode (P)"
                                                        ;;                 :style {:color "#ffffff"
                                                        ;;                         :cursor "pointer"
                                                        ;;                         :margin-top "-2px"
                                                        ;;                         :padding-left "2px"
                                                        ;;                         :font-size "15px"}
                                                        ;;                 :attr {:on-click #(re-frame/dispatch [::bricks/toggle-peek])}]
                                                        ;;         :width "20px"
                                                        ;;         :height "20px"
                                                        ;;         :style {:position "fixed"
                                                        ;;                 :z-index 98
                                                        ;;                 :border-radius "0px 7px 0px 0px"
                                                        ;;                 :top (- panel-height 33)
                                                        ;;                 :left 46
                                                        ;;                 :background-color "#00000022"
                                                        ;;                 :color "white"}]

                                                        ;;        [re-com/box
                                                        ;;         :child [re-com/md-icon-button :src (at)
                                                        ;;                 :md-icon-name "zmdi-refresh-sync"
                                                        ;;                 :tooltip "toggle auto-refresh (O)"
                                                        ;;                 :style {:color "#ffffff"
                                                        ;;                         :cursor "pointer"
                                                        ;;                         :margin-top "-2px"
                                                        ;;                         :padding-left "2px"
                                                        ;;                         :font-size "15px"}
                                                        ;;                 :attr {:on-click #(re-frame/dispatch [::bricks/toggle-auto-run])}]
                                                        ;;         :width "20px"
                                                        ;;         :height "20px"
                                                        ;;         :style {:position "fixed"
                                                        ;;                 :z-index 98
                                                        ;;                 :border-radius "0px 7px 0px 0px"
                                                        ;;                 :top (- panel-height 33)
                                                        ;;                 :left 69
                                                        ;;                 :background-color "#00000022"
                                                        ;;                 :color "white"}]

                                                        ;;        [re-com/box
                                                        ;;         :child [re-com/md-icon-button :src (at)
                                                        ;;                 :md-icon-name "zmdi-developer-board"
                                                        ;;                 :tooltip "  toggle external editing"
                                                        ;;                 :style {;:;color (if external? "red" "#ffffff")
                                                        ;;                         :cursor "pointer"
                                                        ;;                         :margin-top "-2px"
                                                        ;;                         :padding-left "2px"
                                                        ;;                         :font-size "15px"}
                                                        ;;                 :attr {:on-click #(re-frame/dispatch [::bricks/toggle-external])}]
                                                        ;;         :width "20px"
                                                        ;;         :height "20px"
                                                        ;;         :style {:position "fixed"
                                                        ;;                 :z-index 98
                                                        ;;                 :top (- panel-height 33)
                                                        ;;                 :left 92
                                                        ;;                 :background-color "#00000022"
                                                        ;;                 :color "white"}]

                                                        ;;        [re-com/box
                                                        ;;         :child [re-com/md-icon-button :src (at)
                                                        ;;                 :md-icon-name "zmdi-close"
                                                        ;;                 :tooltip "un-select block (ESC)"
                                                        ;;                 :style {:color "#ffffff"
                                                        ;;                         :cursor "pointer"
                                                        ;;                         :margin-top "-2px"
                                                        ;;                         :padding-left "2px"
                                                        ;;                         :font-size "15px"}
                                                        ;;                 :attr {:on-click #(re-frame/dispatch [::bricks/select-block "none!"])}]
                                                        ;;         :width "20px"
                                                        ;;         :height "20px"
                                                        ;;         :style {:position "fixed"
                                                        ;;                 :z-index 98
                                                        ;;                 :border-radius "0px 7px 0px 0px"
                                                        ;;                 :top (- panel-height 33)
                                                        ;;                 :left 115
                                                        ;;                 :background-color "#00000022"
                                                        ;;                 :color "white"}]

                                                        ;;        (when @(re-frame/subscribe [::audio/audio-playing?])
                                                        ;;          [re-com/md-icon-button :src (at)
                                                        ;;           :md-icon-name "zmdi-speaker"
                                                        ;;           :style {;:margin-top "4px"
                                                        ;;                   :position "fixed" :left 138 :bottom 0
                                                        ;;                   :color "red"
                                                        ;;                   :font-size "22px"}])

                                                        ;;        (when @(re-frame/subscribe [::audio/recording?])
                                                        ;;          [re-com/md-icon-button :src (at)
                                                        ;;           :md-icon-name "zmdi-mic"
                                                        ;;           :style {;:margin-top "-0px"
                                                        ;;                   :position "fixed" :left 161 :bottom 0
                                                        ;;                   :color "red"
                                                        ;;                   :font-size "22px"}])

                                                        ;;        [re-com/box
                                                        ;;         :size "none"
                                                        ;;         :style {:position "fixed" :left 138
                                                        ;;                 :top (- panel-height 33)
                                                        ;;                 :font-weight 700 :color "#ffffff77"}
                                                        ;;         :child (str 45 " " @bricks/over-flow? " " @dragging-port?)]

                                                        ;;        [re-com/box
                                                        ;;         :child [re-com/md-icon-button :src (at)
                                                        ;;                 :md-icon-name "zmdi-save"
                                                        ;;                 :tooltip "save board (Ctrl-S)"
                                                        ;;                 :style {:color "#ffffff"
                                                        ;;                         :cursor "pointer"
                                                        ;;                         :margin-top "-2px"
                                                        ;;                         :padding-left "2px"
                                                        ;;                         :font-size "15px"}]
                                                        ;;         :width "20px"
                                                        ;;         :height "20px"
                                                        ;;         :style {:position "fixed"
                                                        ;;                 :z-index 98
                                                        ;;                 :border-radius "0px 7px 0px 0px"
                                                        ;;                 :top (- panel-height 33)
                                                        ;;                 :right 23
                                                        ;;                 :background-color "#00000022"
                                                        ;;                 :color "white"}]

                                                        ;;        [re-com/box
                                                        ;;         :child [re-com/md-icon-button :src (at)
                                                        ;;                 :md-icon-name "zmdi-file-plus"
                                                        ;;                 :tooltip "save board *w data*"
                                                        ;;                 :style {:color "#ffffff"
                                                        ;;                         :cursor "pointer"
                                                        ;;                         :margin-top "-2px"
                                                        ;;                         :padding-left "2px"
                                                        ;;                         :font-size "15px"}]
                                                        ;;         :width "20px"
                                                        ;;         :height "20px"
                                                        ;;         :style {:position "fixed"
                                                        ;;                 :z-index 98
                                                        ;;                 :border-radius "0px 7px 0px 0px"
                                                        ;;                 :top (- panel-height 33)
                                                        ;;                 :right 0
                                                        ;;                 :background-color "#00000022"
                                                        ;;                 :color "white"}]

                                                              ;;  [re-com/box
                                                              ;;   :child [re-com/md-icon-button :src (at)
                                                              ;;           :md-icon-name (if @flow-details-panel?
                                                              ;;                           "zmdi-chevron-up"
                                                              ;;                           "zmdi-chevron-down")
                                                              ;;           :style {:color (str (theme-pull :theme/editor-outer-rim-color nil))
                                                              ;;                   :cursor "pointer"
                                                              ;;                   :font-size "35px"}
                                                              ;;           :on-click #(reset! flow-details-panel? (not @flow-details-panel?))]
                                                              ;;   :style {:position "fixed"
                                                              ;;           :opacity 0.45
                                                              ;;           :z-index 98
                                                              ;;           :top (- panel-height 45)
                                                              ;;           :right (/ panel-width 2)
                                                              ;;           :background-color "#00000022"
                                                              ;;           :color "white"}]


                                                               [re-com/h-box
                                                                :gap "6px"
                                                                ;:size "1"
                                                                :width (if @(re-frame/subscribe [::bricks/flow-editor?])
                                                                         (px (* ww 0.5035))
                                                                         (px (* ww 0.793)))
                                                                :style {:position "fixed"
                                                                        ;:border "1px solid green"
                                                                        :height "60px"

                                                                        :left (if @(re-frame/subscribe [::bricks/flow-editor?]) 640 45)
                                                                        :bottom 0}
                                                                :children [(when has-done?
                                                                             (if (or running? chans-open?)
                                                                               [re-com/box
                                                                                :child [re-com/md-icon-button :src (at)
                                                                                        :md-icon-name "zmdi-refresh-sync"
                                                                                        :class (if (not running?)
                                                                                                 "rotate-reverse linear infinite"
                                                                                                 "rotate linear infinite")
                                                                                        :style {:color (str (theme-pull :theme/editor-outer-rim-color nil))
                                                                                                :cursor "pointer"
                                                                                                :transform-origin "21.5px 22px"
                                                                                                :filter "drop-shadow(0.25rem 0.35rem 0.4rem rgba(0, 0, 0, 0.44))"
                                                                                                :font-size "43px"}
                                                                                        :on-click #(re-frame/dispatch [::run-current-flowmap])]

                                                                                :style {:z-index 98
                                                                                      ;:background-color "#00000022"
                                                                                        :color "white"}]

                                                                               [re-com/box
                                                                                ;:width "50px" :height "50px" :size "none"
                                                                                :child [draggable-play
                                                                                        [re-com/md-icon-button :src (at)
                                                                                         ;:height "50px"
                                                                                         :md-icon-name "zmdi-play"
                                                                                         :attr {:on-mouse-enter #(reset! editor-tooltip-atom (str "run flow live"))
                                                                                 ;:on-click #(reset! editor-mode :part-browser)
                                                                                                :on-mouse-leave #(reset! editor-tooltip-atom nil)}
                                                                                         :on-click #(re-frame/dispatch [::run-current-flowmap])
                                                                                         :style {:color (str (theme-pull :theme/editor-outer-rim-color nil))
                                                                                                 :cursor "pointer"
                                                                                                 :filter "drop-shadow(0.25rem 0.35rem 0.4rem rgba(0, 0, 0, 0.44))"
                                                                                                 ;:height "60px"
                                                                                                 ;;:margin-bottom "8px"
                                                                                                 ;:border "1px solid white"
                                                                                  ;:margin-top "-5px"
                                                                                  ;:padding-left "2px"
                                                                                                 ;:margin-bottom "8px"
                                                                                                 :font-size "55px"}]
                                                                                        flow-id]
                                                                        ;:on-click #(reset! flow-details-panel? (not @flow-details-panel?))

                                                                                :style {;:position "fixed"
                                                                                        :opacity 0.45
                                                                                        :z-index 98
                                                                                        ;:top (- panel-height 65)
                                                                                        ;:left 15
                                                                                        ;:background-color "#00000022"
                                                                                        :color "white"}]))

                                                                           (when true ;has-done?
                                                                             [re-com/box
                                                                              :child [re-com/md-icon-button :src (at)
                                                                                      :md-icon-name "zmdi-stop"
                                                                                        ;:on-click #(re-frame/dispatch [::run-current-flowmap])
                                                                                      :attr {:on-click #(re-frame/dispatch [::wfx/request :default
                                                                                                                            {:message    {:kind :kill-flow
                                                                                                                                          :flow-id flow-id
                                                                                                                                          :client-name @(re-frame/subscribe [::bricks/client-name])}
                                                                                                                               ;:on-response [::simple-response]
                                                                                                                               ;:on-timeout  [::timeout-response :run-flow flowmap] ;; requeue?
                                                                                                                             :timeout    15000000}])
                                                                                             :on-mouse-enter #(reset! editor-tooltip-atom (str "kill flow (on server) and all open channels"))
                                                                                             :on-mouse-leave #(reset! editor-tooltip-atom nil)}
                                                                                      :style {:color (str (theme-pull :theme/editor-outer-rim-color nil))
                                                                                              :cursor "pointer"
                                                                                              :filter "drop-shadow(0.25rem 0.35rem 0.4rem rgba(0, 0, 0, 0.44))"
                                                                                              :font-size "55px"}]

                                                                              :style {;:position "fixed"
                                                                                      :opacity 0.45
                                                                                      :z-index 98
                                                                                      :margin-top "-1px"
                                                                                        ;:top (- panel-height 65)
                                                                                        ;:left 15
                                                                                        ;:background-color "#00000022"
                                                                                      :color "white"}])

                                                                           (when (not running?)
                                                                             [re-com/box
                                                                              :child [re-com/md-icon-button :src (at)
                                                                                      :md-icon-name "zmdi-time-interval"
                                                                                      :on-click #(reset! editor-mode :scheduler)
                                                                                      :attr {:on-mouse-enter #(reset! editor-tooltip-atom (str "set flow schedule (on server)"))
                                                                                 ;:on-click #(reset! editor-mode :part-browser)
                                                                                             :on-mouse-leave #(reset! editor-tooltip-atom nil)}
                                                                                      :style {:color (str (theme-pull :theme/editor-outer-rim-color nil))
                                                                                              :cursor "pointer"
                                                                                              :filter "drop-shadow(0.25rem 0.35rem 0.4rem rgba(0, 0, 0, 0.44))"
                                                                                  ;:margin-top "-5px"
                                                                                  ;:padding-left "2px"
                                                                                              :font-size "43px"}]
                                                                        ;:on-click #(reset! flow-details-panel? (not @flow-details-panel?))

                                                                              :style {;:position "fixed"
                                                                                      :opacity 0.22
                                                                                      :margin-top "4px"
                                                                                      :z-index 98
                                                                                      ;:top (- panel-height 60)
                                                                                      ;:left 62
                                                                                      ;:background-color "#00000022"
                                                                                      :color "white"}])

                                                                           (when (not running?)
                                                                             [re-com/box
                                                                              :child [re-com/md-icon-button :src (at)
                                                                                      :md-icon-name "zmdi-save"
                                                                                      :attr {:on-mouse-enter #(reset! editor-tooltip-atom (str "save flow"))
                                                                                 ;:on-click #(reset! editor-mode :part-browser)
                                                                                             :on-mouse-leave #(reset! editor-tooltip-atom nil)}
                                                                                      :on-click #(do
                                                                                                   (re-frame/dispatch [::http/save-flow
                                                                                                                       {:flowmaps flowmaps
                                                                                                                        :opts @(re-frame/subscribe [::opts-map])
                                                                                                                        :zoom @db/pan-zoom-offsets
                                                                                                                        :flow-id flow-id
                                                                                                                        :flowmaps-connections flowmaps-connections}
                                                                                                                       flow-id])
                                                                                                   (ut/dispatch-delay 3000 [::conn/clear-query-history :flows-sys]))
                                                                                      :style {:color (str (theme-pull :theme/editor-outer-rim-color nil))
                                                                                              :cursor "pointer"
                                                                                              :filter "drop-shadow(0.25rem 0.35rem 0.4rem rgba(0, 0, 0, 0.44))"
                                                                                  ;:margin-top "-5px"
                                                                                  ;:padding-left "2px"
                                                                                              :font-size "43px"}]

                                                                              :style {;:position "fixed"
                                                                                      :opacity 0.22
                                                                                      :z-index 98
                                                                                      :margin-top "4px"
                                                                                      ;:top (- panel-height 60)
                                                                                     ; :left 113
                                                                                      ;:background-color "#00000022"
                                                                                      :color "white"}])

                                                                           (when true ; (not running?)
                                                                             [re-com/box
                                                                              :child [re-com/md-icon-button :src (at)
                                                                                      :attr {:on-mouse-enter #(reset! editor-tooltip-atom (str "new flow"))
                                                                                 ;:on-click #(reset! editor-mode :part-browser)
                                                                                             :on-mouse-leave #(reset! editor-tooltip-atom nil)}
                                                                                      :on-click #(re-frame/dispatch [::new-flow])
                                                                                      :md-icon-name "zmdi-widgets"
                                                                                      :style {:color (str (theme-pull :theme/editor-outer-rim-color nil))
                                                                                              :cursor "pointer"
                                                                                              :filter "drop-shadow(0.25rem 0.35rem 0.4rem rgba(0, 0, 0, 0.44))"

                                                                                  ;:margin-top "-5px"
                                                                                  ;:padding-left "2px"
                                                                                              :font-size "43px"}]

                                                                              :style {;:position "fixed"
                                                                                      :opacity 0.22
                                                                                      :z-index 98
                                                                                      :margin-top "4px"
                                                                                      ;:top (- panel-height 60)
                                                                                      ;:right 10
                                                                                      ;:background-color "#00000022"
                                                                                      :color "white"}])

                                                                           (if (and @dragging-port? @bricks/over-flow?)
                                                                             [(reagent/adapt-react-class rdnd/Droppable)
                                                                              {:types   [:flow-port]
                                                                               :on-drop #(let [[srcf srcl _] @dragged-port
                                                                                               src (if (= srcl :out) srcf ;; no alias for simple */out
                                                                                                       (keyword (str (gn srcf) "/" (gn srcl))))]
                                                                                           (reset! dragging-port? false)
                                                                                           (tap> [:dropped src :to :done])
                                                                                           (set-delay! port-hover nil 2500)
                                                                                           (connect-ports src :done))}
                                                                              (let [ccolor (theme-pull :theme/editor-outer-rim-color nil)]
                                                                                    ;old-dyn-margin (* -1 (* 10 (get @db/pan-zoom-offsets 2)))

                                                                                [re-com/h-box
                                                                                 :attr {:on-drag-enter #(reset! port-hover [:done :done])
                                                                                        :on-drag-over #(when (not (= @port-hover [:done :done]))
                                                                                                         (reset! port-hover [:done :done]))
                                                                                        :on-drag-leave #(reset! port-hover nil)}
                                                                                 :gap "10px"
                                                                                 :children [[re-com/box
                                                                                             :child "drop here for: "]
                                                                                            [re-com/box
                                                                                             :style {:font-weight 700}
                                                                                             :child "all done & return value"]]
                                                                                 :size "none" :align :center :justify :center
                                                                                 :style {:font-size "27px"
                                                                                         :background-color (str ccolor "33")
                                                                                         :backdrop-filter "blur(3px)"
                                                                                         :border (str "4px dashed " ccolor)
                                                                                         :margin-top "4px"}
                                                                                 :height "55px"
                                                                                 :width (px (- (if @(re-frame/subscribe [::bricks/flow-editor?])
                                                                                                 (* ww 0.5035)
                                                                                                 (* ww 0.793)) 210))])]
                                                                     ;:width "80px"


                                                                             [re-com/box
                                                                              :child (if (and (empty? @editor-tooltip-atom) running?) (str :flow-name " is running")
                                                                                         (str (or @editor-tooltip-atom "")))
                                                                              :size "none"
                                                                              :height "80%"
                                                                              :width (px (- (if @(re-frame/subscribe [::bricks/flow-editor?])
                                                                                              (* ww 0.5035)
                                                                                              (* ww 0.793)) 210))
                                                                              :align :center :justify :center
                                                                              :style
                                                                              {;:border "2px solid green"
                                                                               :font-size "19px"
                                                                               :font-weight 700
                                                                               :overflow "hidden"}])]]

                                                               [re-com/box
                                                                :style {:position "fixed"
                                                                        :top 34 :right 23
                                                                        :opacity 0.33
                                                                        :filter "drop-shadow(0.25rem 0.35rem 0.4rem rgba(0, 0, 0, 0.44))"
                                                                        :font-weight 700
                                                                        :color (str (theme-pull :theme/editor-outer-rim-color nil))
                                                                        :font-size "29px"}
                                                                :child (str flow-id)]

                                                               [re-com/box
                                                                :child [re-com/md-icon-button :src (at)
                                                                        :md-icon-name (if zoom-unlocked? "zmdi-lock-open" "zmdi-lock")
                                                                        ;:on-click #(reset! pan-lock? (not @pan-lock?))
                                                                        :on-click #(re-frame/dispatch [::toggle-lock])
                                                                        :attr {:on-mouse-enter #(reset! editor-tooltip-atom (str "hold C to unlock flow pan & zoom"))
                                                                               :on-mouse-leave #(reset! editor-tooltip-atom nil)}
                                                                        :style {:color (if zoom-unlocked? "red"
                                                                                           (str (theme-pull :theme/editor-outer-rim-color nil)))
                                                                                :cursor "pointer"
                                                                                :font-size "43px"}]
                                                                :style {:opacity (if zoom-unlocked? 1.0 0.22)
                                                                        :margin-top "4px"
                                                                        :z-index 98
                                                                        :position "fixed"
                                                                        :right 10 :bottom 30
                                                                        :background-color "#00000022"
                                                                        :color "white"}]

                                                               (when (and @dragging-port?
                                                                          (not (= [0 0] @tentacle-pos)))
                                                                 [:svg
                                                                  {:style {:width "100%"
                                                                           :height "100%"
                                                                           :position "fixed"
                                                                           :pointer-events "none"
                                                                           :z-index 100}}
                                                                  (draw-tentacle [(generate-tentacle-coord)])])






                                                              ;;  (when (and @flow-details-panel?
                                                              ;;             (not @bricks/dragging-editor?)
                                                              ;;             (not @db/dragging-flow-editor?))


                                                               [rc/catch [flow-details-panel panel-height panel-width details-panel-height]]]]])]])]]))

                                                                ;;  )

