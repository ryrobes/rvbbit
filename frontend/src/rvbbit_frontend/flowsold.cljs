(ns rvbbit-frontend.flowsold
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
   [rvbbit-frontend.shapes :as shape]
   [talltale.core :as tales]
   [day8.re-frame.undo :as undo :refer [undoable]]
   [cljs.core.async :as async :refer [<! >! chan]]
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
;(defonce flow-select (reagent/atom nil))
(def dragging-port? (reagent/atom false))
(def dragged-port (reagent/atom []))
(defonce dragging? (reagent/atom false))
(defonce dragging-flow? (reagent/atom false))
;(defonce bid-over (reagent/atom nil))
;(defonce db/flow-results (reagent/atom {}))
(def editor-tooltip-atom (reagent/atom nil))
;(def flow-id (reagent/atom "live-scratch-flow"))
(defonce title-edit-idx (reagent/atom nil))
(defonce drop-toggle? (reagent/atom false))

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
;(def running-blocks (reagent/atom [:rabbit-sql-query :block0 ]))
(def running-blocks (reagent/atom {}))
(defonce editor-mode (reagent/atom :flow-browser))

(defonce detached-coords
  (reagent/atom (let [hh (.-innerHeight js/window) ;; starting size set on load
                      ww (.-innerWidth js/window) ;; starting size set on load
                      topper 200
                      lefty 200]
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
   (get db :selected-flow "live-scratch-flow")))

(re-frame/reg-event-db
 ::set-selected-flow
 (fn [db [_ flow-id]]
   (tap> [:set-flow flow-id])
   (let [flow-id (str flow-id)
         curr-flow-id (get db :selected-flow)
         curr-flow (get-in db [:flows curr-flow-id] {})]
     ;(reset! db/flow-results (walk/postwalk-replace {curr-flow-id flow-id} @db/flow-results)) ;; unnecessary, but keeps the UI in sync
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

     (reset! db/flow-results (walk/postwalk-replace {old-flow-id new-flow-id} @db/flow-results)) ;; unnecessary, but keeps the UI in sync
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
         new-bid (keyword new-bid)
         new-bid (ut/safe-key new-bid @(re-frame/subscribe [::reserved-type-keywords]))
         input-replacements (into {}
                                  (for [k inputs] {(keyword (str (ut/unkeyword old-bid) "/" (ut/unkeyword k)))
                                                   (keyword (str (ut/unkeyword new-bid) "/" (ut/unkeyword k)))}))
         pw-replace-map (merge input-replacements {old-bid new-bid})]
     (tap> [:rename-block old-bid :to new-bid :with pw-replace-map]) ;; block id needs to be keyword,
     (if (not (some #(= new-bid %) block-ids)) ;; only if is unique, else do nothing 
       (do
         (reset! db/flow-results (walk/postwalk-replace pw-replace-map @db/flow-results)) ;; unnecessary, but keeps the UI in sync
         (-> db
             (assoc :selected-flow-block new-bid)
             (assoc-in [:flows curr-flow-id] (walk/postwalk-replace pw-replace-map (get-in db [:flows curr-flow-id])))))
       db))))

(re-frame/reg-sub
 ::flowmap-connections
 (fn [db]
   (vec (get-in db [:flows (get db :selected-flow) :connections] []))))

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


(defn create-out-map-ports [ddata fid bid]
  (let [;;ddata (read-string (cstr/join " " (ut/cm-deep-values %)))
        dtype (ut/data-typer ddata)
        ports {:out {:out (keyword dtype)}}
        ports (cond (= dtype "map")
                    (assoc (into {} (for [[k v] ddata] {k (keyword (ut/data-typer v))})) :* :map)
                    (or (= dtype "vector") (= dtype "rowset")) ;(= dtype "vector")
                    (assoc (into {} (for [k (range (count ddata)) :let [v (get ddata k)]] {(keyword (str "idx" k)) (keyword (ut/data-typer v))})) :* :vector)
                    :else ports)
        full (-> (get @(re-frame/subscribe [::flowmap]) bid)
                 ;(assoc-in [:data :user-input] ddata)
                 (assoc-in [:ports :out] ports))]
    (re-frame/dispatch [::update-flowmap-key-others bid fid nil full])))

(re-frame/reg-event-db
 ::socket-response-flow
 (fn [db [_ result]]
   (let [flow-id @(re-frame/subscribe [::selected-flow])
         _ (doseq [[fid fb] (get result :return-maps)] ;; backport map keys to out ports
             (doseq [[bid ddata] fb
                     :when (and (or (map? ddata) (vector? ddata))
                                (not (get ddata :port-in?)) (not (= bid :done)))]
               (create-out-map-ports ddata fid bid)))]
                         ;{[fid bid] ddata}

     (tap> [:flow-in result])
   ;(swap! db/flow-results result)
     (swap! running-blocks assoc flow-id [])
     (reset! editor-tooltip-atom (str "finished. returned value: " (get result :return-val)))
     ;(swap! db/flow-results assoc flow-id (merge {:status :done} result))
     (reset! db/flow-results (merge {:status :done} result))
   ;(let [] db)
     db)))

(re-frame/reg-event-db
 ::timeout-response
 (fn [db [_ result what-req]]
   (let []
     (tap> [:websocket-timeout! result what-req])
     db)))

(defn process-flow-map [fmap]
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
               (let [pfull (if (= (get-in v [:data :syntax] "clojure") "clojure")
                             (get-in v [:data :user-input])
                             (cstr/join "\n" (get-in v [:data :user-input])))
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

(defn process-flowmap2 [flowmap flowmaps-connections]
  (let [canvas-key (into {} (for [[k {:keys [w h x y]}] flowmap]
                              {k {:w w :h h :x x :y y :view-mode "text"}}))
        components-key (into {} (for [[k {:keys [data ports file-path raw-fn flow-id sub-flow]}] flowmap ;; <-- flow-id refers to the subflow embed, not the parent
                                      :let [ttype (get-in data [:drag-meta :type])
                                            try-read (fn [x] (try (edn/read-string x) (catch :default _ x)))
                                                    ;expandable-in? (try (cstr/ends-with? (str (first (keys (get ports :in)))) "*") (catch :default _ false))
                                            fn-key (try-read (get-in data [:flow-item :name] ":unknown!"))
                                            fn-category (try-read (get-in data [:flow-item :category] ":unknown!"))]]
                                  (cond

                                    (and (= ttype :open-block) (not (empty? (get ports :in)))) ;; open block with inputs   
                                    {k {:data (get data :user-input)
                                        :inputs (vec (keys (get ports :in)))}}

                                    (or (= ttype :open-block) (= ttype :cell) (= ttype :param))
                                    {k (if (= (get data :syntax "clojure") "clojure")
                                         (get data :user-input)
                                         (cstr/join "\n" (get data :user-input)))}

                                    (= ttype :open-fn)
                                    {k {:fn raw-fn :raw-fn raw-fn
                                        :inputs (vec (keys (get ports :in)))}}

                                    (= ttype :query)
                                    {k {:fn (get data :user-input) ;'(fn [x] x) ;:raw-fn '(fn [x] x)
                                        :inputs (vec (keys (get ports :in)))}}

                                    (= ttype :sub-flow)
                                    {k (-> (process-flowmap2 (get sub-flow :map) (get sub-flow :connections))
                                           (assoc :file-path file-path)
                                           (assoc :flow-id flow-id))} ;; flow-id of the embdeeded flow, NOT the parent
                                                ;; {k {:components (get sub-flow :map)
                                                ;;     :connections (get sub-flow :connections)}}
                                             ;;(= ttype :param) {k (get data :user-input)}

                                    :else {k {:fn-key [fn-category fn-key]
                                              :inputs (if false ;expandable-in?
                                                        [(vec (keys (get ports :in)))] ;; send as single value to apply%
                                                        (vec (keys (get ports :in))))}})))
        flowmaps-connections (vec (for [[c1 c2] flowmaps-connections]
                                    (if (cstr/ends-with? (str c1) "/*")
                                      [(keyword (-> (ut/unkeyword (str c1)) (cstr/replace "/*" "") (cstr/replace ":" ""))) c2] [c1 c2])))
        server-flowmap {:canvas canvas-key :components components-key :connections flowmaps-connections}]
    server-flowmap))

;;(defn make-server-version)


(re-frame/reg-event-db
 ::run-current-flowmap
 (fn [db _]
   ;(tap> [:ran-condi ])
   (doall (let [flowmap @(re-frame/subscribe [::flowmap])
                flow-id @(re-frame/subscribe [::selected-flow])
                flowmaps-connections @(re-frame/subscribe [::flowmap-connections])
                client-name @(re-frame/subscribe [::conn/client-name])
                server-flowmap (process-flowmap2 flowmap flowmaps-connections)]
            (reset! editor-tooltip-atom (str flow-id " is running"))
            ;(swap! db/flow-results assoc flow-id {:status :started})
            (swap! db/flow-results {:status :started})
            (swap! running-blocks assoc flow-id (vec (keys flowmap)))
            (tap> [:flowmap-send-it flowmap server-flowmap])
            (re-frame/dispatch [::wfx/request :default
                                {:message    {:kind :sub-to-running-values
                                              :flow-keys (vec (for [k (keys (get server-flowmap :components))] [flow-id k]))
                                              :client-name client-name}
                                 :on-response [::simple-response]
                                 :on-timeout  [::timeout-response :run-flow flowmap] ;; requeue?
                                 :timeout    15000000}])
            (re-frame/dispatch [::wfx/request :default
                                {:message    {:kind :run-flow
                                              :flow-id flow-id
                                              :opts {:increment-id? false}
                                              :flowmap server-flowmap
                                              :client-name client-name}
                                 :on-response [::socket-response-flow]
                                 :on-timeout  [::timeout-response :run-flow flowmap] ;; requeue?
                                 :timeout    15000000}])
            (dissoc db :flow-runner)))))

(re-frame/reg-sub
 ::flow-runner
 (fn [db [_ flow-id bid]]
   (and (not (empty? (get @running-blocks flow-id [])))
        (= (get-in db [:flow-runner flow-id bid] :started) :started))))

;(tap> [:results-atom @db/flow-results])

(defn is-running? [bid flow-id]
  (let [started? @(re-frame/subscribe [::flow-runner flow-id bid])
        rblocks (get @running-blocks flow-id [])]
    (if (= bid :*)
      (and (= bid :*) (not (empty? rblocks)))
      (and started?
           (or (and (= bid :*) (not (empty? rblocks)))
               (some #(= bid %) rblocks))))))

(defn has-done? []
  (true? (some #(= % :done)
               (apply concat @(re-frame/subscribe [::flowmap-connections])))))
                      ;@flowmaps-connections


(def tentacle-pos (reagent/atom [0 0]))
(def tentacle-start (reagent/atom [0 0]))

(defn gn [x] (try (name x) (catch :default _ x)))
(defn gns [x] (try (namespace x) (catch :default _ x)))
(defn gns? [x] (not (nil? (try (namespace x) (catch :default _ false)))))

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

(defn coord-cacher [conns flow-map]
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

(defn translate-x-y [[x y]]
  (let [zoom-multi (get @db/pan-zoom-offsets 2)
        zoom-offset-x (get @db/pan-zoom-offsets 0)
        zoom-offset-y (get @db/pan-zoom-offsets 1)
        drop-x      (- (/ x zoom-multi) (/ zoom-offset-x zoom-multi))
        drop-y      (- (/ y zoom-multi) (/ zoom-offset-y zoom-multi))]
    [drop-x drop-y]))

(defn generate-coords [xx yy]
  (let [conns (vec (filter #(not (= (last %) :done))
                           ;@flowmaps-connections
                           @(re-frame/subscribe [::flowmap-connections])))
        ;;tt @tentacle-pos
        flow-map @(re-frame/subscribe [::flowmap])
        fconns (coord-cacher conns flow-map)]
    ;(tap> [:generate-coords conns fconns])
    (if false ;@dragging-port?
      (let [[x2 y2] (translate-x-y @tentacle-pos)
            [x1 y1] (translate-x-y @tentacle-start)
            dcolor (get (theme-pull :theme/data-colors db/data-colors)
                        (gn (last @dragged-port))
                        (get (theme-pull :theme/data-colors db/data-colors) "unknown" "#FFA500"))]
        (conj fconns [x1 (+ y1 11) (- x2 xx) (- y2 yy) false dcolor nil nil]))
      fconns)))

(defn draw-lines [coords]
  (doall (for [[x1 y1 x2 y2 involved? color z1 z2] coords]
           ^{:key (hash (str x1 y1 x2 y2 "lines-flow"))}
           (let [flow-id @(re-frame/subscribe [::selected-flow])];selected-dash "" ;@(re-frame/subscribe [::selected-dash]) ;;; (get (theme-pull :theme/data-colors db/data-colors) "unknown")
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
             [:path {:stroke-width 8 ;(if involved? 16 13)
                     :stroke       (if involved? color "#ffffff22") ;(if (or involved? nothing-selected?)
                     ;:style      {:animation "rotate linear infinite"}
                     ;:animation "rotate linear infinite"
                     ;:z-index 300
                     :class (when (is-running? z1 flow-id) "flow-right") ;;(some #(= z1 %) @running-blocks)
                     ;(if (nil? color) "pink" color) "#E6E6FA")
                    ; :fill         "green" ;"none"
                    ; :stroke-dasharray "10,10"
                    ; :stroke-linecap "round"
                     :fill         "none" ;;(str color 45) ;; could be a cool "running effect"
                   ;  :filter       "drop-shadow(0.25rem 0.35rem 0.4rem rgba(0, 0, 0, 0.44))"
                     :d            (ut/curved-path-h x1 y1 x2 y2)}]))))
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
   (tap> [:update bid (get db :selected-flow-block) kkey vval])
   (let [bid (get db :selected-flow-block)] ;; override due to slow reacting?
     (if (nil? kkey) ;; fulll body update
       (assoc-in db [:flows (get db :selected-flow) :map bid] vval)
       (assoc-in db [:flows (get db :selected-flow) :map bid kkey] vval)))))

(re-frame/reg-event-db
 ::update-flowmap-key2 ;;; test, just used for add block
 (undoable)
 (fn [db [_ bid kkey vval]]
   (tap> [:update bid (get db :selected-flow-block)  kkey vval])
   (let [] ; [bid (get db :selected-flow-block)] ;; override due to slow reacting?
     (if (nil? kkey) ;; fulll body update
       (assoc-in db [:flows (get db :selected-flow) :map bid] vval)
       (assoc-in db [:flows (get db :selected-flow) :map bid kkey] vval)))))

(re-frame/reg-event-db
 ::update-flowmap-key-others
 (undoable)
 (fn [db [_ bid fid kkey vval]]
   (tap> [:update bid (get db :selected-flow-block) fid kkey vval])
   (if (nil? kkey) ;; fulll body update
     (assoc-in db [:flows fid :map bid] vval)
     (assoc-in db [:flows fid :map bid kkey] vval))))

(re-frame/reg-event-db
 ::remove-flowmap-bid
 (fn [db [_ bid]]
   (ut/dissoc-in db [:flows (get db :selected-flow) :map bid])))

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


(re-frame/reg-event-db
 ::select-block
 (fn [db [_ bid]]
   (tap> [:selecting-block bid])
   (assoc db :selected-flow-block bid)))

(re-frame/reg-event-db
 ::select-flow
 (fn [db [_ bid]]
   (assoc db :selected-flow bid)))

(re-frame/reg-event-db
 ::update-flowmap-connections
 (fn [db [_ vval]]
   (assoc-in db [:flows (get db :selected-flow) :connections] vval)))

(re-frame/reg-event-db
 ::update-flowmap-coords
 (fn [db [_ bid x y]]
   (-> db
       (assoc-in [:flows (get db :selected-flow) :map bid :x] x)
       (assoc-in [:flows (get db :selected-flow) :map bid :y] y))))

(defn mouse-move-handler-block [offset bid xx yy]
  (fn [evt]
    (let [raw-x (.-clientX evt)
          raw-y (.-clientY evt)

          ;; w pan-zoom
          zoom-multi (get @db/pan-zoom-offsets 2)
          zoom-offset-x (get @db/pan-zoom-offsets 0)
          zoom-offset-y (get @db/pan-zoom-offsets 1)
          off-x (/ (:x offset) zoom-multi)
          off-y (/ (:y offset) zoom-multi)
          x      (- (- (/ raw-x zoom-multi) (/ zoom-offset-x zoom-multi)) off-x)
          y      (- (- (/ raw-y zoom-multi) (/ zoom-offset-y zoom-multi)) off-y)

          xx (/ xx zoom-multi)
          yy (/ yy zoom-multi)

          ofx1 (/ 78 zoom-multi)
          ofy1 (/ 4 zoom-multi)]
        ;;   start-x (.-clientX evt)
        ;;   start-y (.-clientY evt)
        ;;   off-x (:x offset)
        ;;   off-y (:y offset)
        ;;   x      (- start-x off-x)
        ;;   y      (- start-y off-y)

      ;(reset! detached-coords [x y])
;;       (swap! flowmaps assoc bid (->
;;                                  (get @(re-frame/subscribe [::flowmap]) bid)
;;                                  (assoc :x (+ (- x xx ofx1)))
;;                                  (assoc :y (- y yy ofy1))))
      (re-frame/dispatch-sync [::update-flowmap-coords bid (+ (- x xx ofx1)) (- y yy ofy1)]))))

                                 ;(assoc :x (+ (- x xx 78)))
                                 ;(assoc :y (- y yy 4))


(defn tag-screen-position [evt]
  (reset! tentacle-pos [(.-clientX evt) (.-clientY evt)]))

(def mouse-dragging-panel? (reagent/atom false))

(defn mouse-up-handler-block [on-move]
  (fn me [evt]
    (do ;(reset! mouse-dragging-panel? false)
      (reset! dragging? false)
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

(defn mouse-down-handler-block [e bid xx yy]
  (let [{:keys [left top]} (get-client-rect e)
        offset  {:x (- (.-clientX e) left)
                 :y (- (.-clientY e) top)}
        _ (reset! dragging-flow? true)
        ;enter?             (= (.-key e) "Enter")
        on-move (mouse-move-handler-block offset bid xx yy)]
    ;(tap> enter?)
    (do (reset! dragging? true)
        (gevents/listen js/window EventType.MOUSEMOVE on-move))
    (gevents/listen js/window EventType.MOUSEUP
                    (mouse-up-handler-block on-move))))

(defn draggable-port [element flow-id bid pid ttype xs ys]
  [(reagent/adapt-react-class rdnd/Draggable)
   (let [data (let [subkey (str flow-id ">" (cstr/replace (str bid) #":" ""))
                    param-full (keyword (str "flow/" subkey)) ;:flow/map-pull-test2>open-fn-6
                    param-field (keyword subkey)]
                {:h 3
                 :w 7
                 :root [0 0]
                 :drag-meta
                 {:type :param
                  :param-full param-full
                  :param-table :flow
                  :param-field param-field}})]
     {:type          :flow-port
    ;:on-drag (debounce #(tag-screen-position %) 100)
    ;:on-drag tag-screen-position
    ;:on-drag (debounce tag-screen-position 10)
    ;:on-drag #(tap> [(.-clientX %) (.-clientY %)])
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
                        ;(reset! tentacle-start [xs ys])
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
                        ;(reset! dyn-dropper-hover false)
                        ;(reset! dragging-size [0 0])
                        ;(reset! on-block? false)
                        ;(tap> [(.-clientX %) (.-clientY %)])
                        ; (tag-screen-position %)
                        ;(reset! dragging-body false)

      :on-drag-start #(do
                        (reset! dragging-port? true)
                       ; (reset! dragged-port [bid pid ttype])
                        ;(reset! tentacle-start [xs ys])
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

(defn ifnil [x n] (if (nil? x) n x))

(defn remove-input [bid pid]
  (let [flowmaps-connections @(re-frame/subscribe [::flowmap-connections])
        new-connections (remove #(or (= (last %) (keyword (str (gn bid) "/" (gn pid))))
                                     (if (= pid :in) (= (last %) bid) false)) ;; unnamed ports assumed 
                                flowmaps-connections)]
    (tap> [:removing bid pid :ins])
;;   (reset! flowmaps-connections
;;           (remove #(or (= (last %) (keyword (str (gn bid) "/" (gn pid))))
;;                        (if (= pid :in) (= (last %) bid) false)) ;; unnamed ports assumed 
;;                   @flowmaps-connections))
    (re-frame/dispatch [::update-flowmap-connections new-connections])))

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
     ;(tap> [:lib-keys lib-keys])
     (vec (into lib-keys
                (into
                 (keys (get-in db [:flows (get db :selected-flow) :map]))
                 (vec (distinct (get-all-values (get-in db [:flows (get db :selected-flow)]) :type)))))))))

(defn add-block [x y & [body bid]]
  (let [;bid (if bid bid (keyword (str "open-input-" (count @(re-frame/subscribe [::flowmap])))))
        _ (tap> [:bid bid])
        bid (keyword (cstr/replace (str (gn bid)) #"/" "-"))
        bid (if bid bid :open-input)
        _ (tap> [:bid bid])
        ;fmaps @(re-frame/subscribe [::flowmap-raw])
        ;flow-map @(re-frame/subscribe [::flowmap])
        ;safe-keys-reserved (vec (keys fmaps))
        safe-keys-reserved @(re-frame/subscribe [::reserved-type-keywords])
        bid (ut/safe-key bid safe-keys-reserved)
        _ (tap> [:bid bid])
        zoom-multi (get @db/pan-zoom-offsets 2)
        zoom-offset-x (get @db/pan-zoom-offsets 0)
        zoom-offset-y (get @db/pan-zoom-offsets 1)
        drop-x      (- (/ x zoom-multi) (/ zoom-offset-x zoom-multi))
        drop-y      (- (/ y zoom-multi) (/ zoom-offset-y zoom-multi))
        body (if body (if (or (cstr/includes? (str bid) "open-input")
                              (and (cstr/includes? (str bid) "open-fn") (get body :right-click? false)))
                        (merge body {:x drop-x :y drop-y :z 0}) body) ;; override for right click open block
                 {:w 200 :h 50
                  :x drop-x :y drop-y :z 0
                  :ports {:in {:in :string}
                          :out {:out :string}}})]
    ;(tap> [:adding-flow-block bid x y @(re-frame/subscribe [::flowmap])])
    ;(swap! flowmaps assoc bid body)
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
  (let [flowmaps-connections @(re-frame/subscribe [::flowmap-connections])
        conns (vec (filter #(not (= (last %) :done)) flowmaps-connections))
        base-src (if (cstr/includes? (str src) "/") (keyword (gns src)) src)
        done? (= dest :done)
        conn-vec (conj
                  (if done? conns flowmaps-connections)
                  [src ;(if done? base-src src)
                   dest])]
    (tap> [:new-conn-vec conn-vec])
;;       (reset! flowmaps-connections
;;               (conj conns [src dest])))
;;     (reset! flowmaps-connections
;;             (conj @flowmaps-connections [src dest])
;;             (assoc-in db [:flows (get db :selected-flow) :connections] (conj flowmaps-connections)))
    (re-frame/dispatch [::update-flowmap-connections
                        conn-vec])))

(defn render-icon [icon bcolor]
  (if (not (nil? icon))
    (if (cstr/includes? icon "zmdi")
      [re-com/md-icon-button :src (at)
       :md-icon-name icon
       :style {:color bcolor
               :cursor "grab"
               :font-size "19px"}
       :attr {}]
      [re-com/box
       :style {:margin-left "-10px"}
       :size "none" :width "30px" :height "30px"
       :child [:img {:src icon
             ;:height "50px"
             ;:height "auto"
             ;:width "50px"
                     :width "100%"}]])

    " "))

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

(defn flow-grid [ww hh xx yy flowmaps-connections flow-id flow-map]
  (let [ww (- ww 16)
        hh (- hh 37)
        [xx yy] @detached-coords
        ;flowmaps-connections @(re-frame/subscribe [::flowmap-connections])
        ;flow-id @(re-frame/subscribe [::selected-flow])
        ;flow-map @(re-frame/subscribe [::flowmap])
        done-block (get (first (filter #(= (last %) :done) flowmaps-connections)) 0 :?)
        read-only-flow? (true? (cstr/includes? flow-id "/"))
        ;coords (generate-coords xx yy)
        react-hack [@dragging-port? @flow-hover @dragging-flow? @flow-drop-hover @db/flow-results @db/pan-zoom-offsets @dragging? @ports-react-render] ;; important to force re-render
        ;flow-selected :my-flow-1
        bounds-x (when read-only-flow? (apply min (for [[_ {:keys [x]}] flow-map :let []] x)))
        bounds-y (when read-only-flow? (apply min (for [[_ {:keys [y]}] flow-map :let []] y)))
        bounds-x2 (when read-only-flow? (apply max (for [[_ {:keys [x w]}] flow-map :let []] (+ w x))))
        bounds-y2 (when read-only-flow? (apply max (for [[_ {:keys [y h]}] flow-map :let []] (+ y h))))
        ;done-data-type (get-in flow-map [done-block :ports :out :out])
        ;done-data-type-color (get (theme-pull :theme/data-colors db/data-colors) done-data-type "#FFA500")
        done-data-type-color (when read-only-flow?
                               (get (theme-pull :theme/data-colors db/data-colors)
                                    (if (some #(= % :*) (keys (get-in flow-map [done-block :ports :out]))) :map ;; if its a map always use map color for base block
                                        (gn (first (vals (get-in flow-map [done-block :ports :out])))))
                                    (get (theme-pull :theme/data-colors db/data-colors) "unknown" "#FFA500")))]
    ;(tap> [:done-block done-block flow-map])
    ;(tap> [:reserved-type-keywords @(re-frame/subscribe [::reserved-type-keywords])]) 
    ;(tap> [:generate-coords coords])
    ;(tap> [:db/flow-results @db/flow-results])

    ^{:key (str "flow-brick-grid")}
    [re-com/h-box
     :size "none" :align :center :justify :center
     :width "6400px" ;(px ww) 
     :height "3600px" ;(px hh)
     :style {:overflow "hidden"}
             ;:z-index 400
             ;:border "1px solid pink"

     :children (conj
                (for [[bid {:keys [x y h w z data icon ports]} :as fmap] flow-map
                      :let [x (+ x 4) ;; offsets from borders
                            y (+ y 30)
                            orig-h h
                            ttype (get-in data [:drag-meta :type])
                            pill-size 17
                            done-block? (= bid done-block)
                            inputs (get ports :in {})
                            python? (= (get data :syntax) "python")
                            icon (if python? "zmdi-language-python" icon)
                           ;; _ (tap> [:fmap bid fmap])
                            ;; expandable-in? (try (and true ;(= 1 (count (keys inputs)))
                            ;;                          (cstr/ends-with? (str (first (keys inputs))) "*"))
                            ;;                     (catch :default _ false))
                            expandable-in? (get-in data [:flow-item :expandable?] false)
                            ;_ (tap> [:? bid ports])
                            outputs (get ports :out)
                            override-value (get-in @db/flow-results [:return-maps (str flow-id) bid])
                            outputs (if (and override-value (= (vec (keys outputs)) [:out])) ;; if we have return data, 
                                      {:out (keyword (ut/data-typer override-value))} ;; change output data types to be more accurate
                                      outputs)
                            ;_ (tap> [:outputs bid outputs])
                            ;; safe-w (- w 50)
                            ;; px-per-char 8

                            ;; longesti (apply max (map #(count (str %)) (conj (keys inputs) 10))) ;; 10 avoid empty list
                            ;; longesti-w (+ (* px-per-char longesti) 4)
                            ;; longesti-w (if (> longesti-w safe-w) safe-w longesti-w)

                            ;; longesto (apply max (map #(count (str %)) (conj (keys outputs) 10))) ;; 10 avoid empty list
                            ;; longesto-w (+ (* px-per-char longesto) 4)
                            ;; longesto-w (if (> longesto-w safe-w) safe-w longesti-w)

                            ;; _ (tap> [bid :cacl-block-widths [w longesti-w longesto-w] (calc-block-widths inputs outputs w)])

                            [w cw longesti-w longesto-w] (calc-block-widths inputs outputs w)
                            ;_ (tap> [bid :cacl-block-widths [w cw longesti-w longesto-w]])

                            ;mouse-down-fn #(mouse-down-handler-block % bid 0 0)
                            zoom-multi (get @db/pan-zoom-offsets 2)
                            ox (* 4 zoom-multi)
                            oy (* 30 zoom-multi)
                            mouse-down-fn #(mouse-down-handler-block % bid (+ xx ox) (+ yy oy))
                                  ;mouse-down-fn #(mouse-down-handler-block % bid xx yy)
                                  ;longest-w-diff (- longest-w 80)
                                  ;;w (+ w longest-w-diff)

                            only-overridden? (and (empty? inputs)
                                                  (not (nil? (get-in @db/flow-results [:return-maps (str flow-id) bid])))
                                                  (not (= (get-in @db/flow-results [:return-maps (str flow-id) bid]) (get data :user-input))))
                            ;_ (tap> [:overwritten bid (empty? inputs) (get-in @db/flow-results [:return-maps (str flow-id) bid]) (get data :user-input)])
                            ;value (get data :user-input (get-in @db/flow-results [:return-maps (str flow-id) bid] "no value"))
                            value (if override-value override-value (get data :user-input))
                            ;value (get data :user-input (get-in @db/flow-results [flow-id :return-map bid] "no value"))
                            error? (get value :error false)
                            flow-select @(re-frame/subscribe [::selected-flow-block])
                            selected? (= bid flow-select)
                            hovered? (= bid @flow-hover)
                            defaults (get-in data [:flow-item :defaults] {})
                            required (get-in data [:flow-item :required] [])
                            styler (get-in data [:flow-item :style] {})
                            styler-selected (get-in data [:flow-item :style-selected] {})
                            ;_ (tap> [bid :outputs outputs :inputs inputs ])
                            most-pills (try
                                         (apply max [(+ (count outputs) (if (get-in ports [:out :*]) 1 0))
                                                     (+ (count inputs) (if expandable-in? 1 0))])
                                         (catch :default _ 1))
                            too-tall? (> (* most-pills pill-size) h)
                            out-taller? (> (count outputs) (+ (count inputs) (if expandable-in? 1 0)))
                            ;_ (tap> [:most-pills bid most-pills too-tall?])
                            ;h (if too-tall? (+ (if out-taller? 40 10) (* most-pills pill-size)) h)
                            h (+ (if out-taller? 40 10) (* most-pills pill-size))
                            h (if (< h orig-h) orig-h h)
                            sub-flow-lookup (when (= ttype :sub-flow)
                                              (let [subby-results (get-in @db/flow-results [:return-maps])
                                                    sub-flow-lookup (into {} (for [k (keys subby-results)] {(keyword (last (cstr/split (str k) "/"))) k}))
                                                    sfl (get sub-flow-lookup bid)] sfl))
                                  ;_ (tap> [:db/flow-results bid @db/flow-results])
                            bcolor (try
                                     (cond error? "red"
                                           python? "#F0E68C"
                                           :else (or (get styler :color nil)
                                                     (get (theme-pull :theme/data-colors db/data-colors)
                                                          (if (some #(= % :*) (keys outputs)) :map ;; if its a map always use map color for base block
                                                              (gn (first (vals outputs))))
                                                          (get (theme-pull :theme/data-colors db/data-colors) "unknown" "#FFA500"))))
                                     (catch :default _ "orange"))]]
                            ;w (+ w (- longesti-w 0))


                      ;:when (and (< x ww) (< y hh)
                      ;           (> x 0) (> y 0))

                  ^{:key (str "flow-brick-" bid)}
                  [re-com/box
                   ;:class (when (some #(= bid %) @running-blocks) "heartbeat")
                   :style (merge
                           {;:background-color (if hovered? (str bcolor 25) "#00000055")
                            :box-shadow       (when hovered? (str "inset 0px 0px 30px " bcolor))
                            :position "fixed"
                            :z-index 610
                            :user-select "none"


                           ;:animation "fill 5s linear forwards"
                           ;:transform "translate(0)"
                          ; :transform (when (some #(= bid %) @running-blocks) "skewer(25deg, 54deg)")

                         ;  :filter (when hovered?
                         ;            (str ;theme-pull :theme/base-block-filter-selected
                         ;                        "drop-shadow(0.35rem 0.35rem 0.4rem " bcolor ") 
                         ;                         drop-shadow(-0.35rem -0.35rem 0.4rem " bcolor ")"))


                            :border-radius "12px" ; "0px 0px 12px 12px"
                           ;:border (str "5px " (if selected? "dashed" "solid")  " " bcolor)
                            :border (if only-overridden? (str "5px dashed " bcolor) (str "5px solid " bcolor))
                            :width (px w)
                            :height (px h)
                        ;;    :width (if (< (+ w x) ww) (px w)
                        ;;               (px (+ 0 (- w (- (+ w x) ww)))))
                        ;;    :height (if (< (+ h y) hh) (px h)
                        ;;                (px (+ 18 (- h (- (+ h y) hh)))))

                            :left x :top y}

                           (if selected?
                             (merge
                              {:background (when selected? (str "linear-gradient(" bcolor 75 ", transparent)"))
                               :background-size (when selected? "30%")
                                                     ;:backdrop-filter "blur(5px) brightness(50%)"
                               :background-position (when selected? "200%")}
                              styler-selected)
                             (merge
                              {:background-color "#00000066"}
                                                     ;:backdrop-filter "blur(5px)"
                              styler)))
                          ;; styler

                   :attr (if (not @dragging-port?)
                           {:on-mouse-enter #(reset! flow-hover bid)
                            :on-mouse-over #(when (not (= @flow-hover bid))
                                              (reset! flow-hover bid))
                                  ;:on-mouse-down mouse-down-fn
                            ;:on-double-click #(select-block sub-flow-lookup)
                            :on-double-click #(when sub-flow-lookup
                                                (re-frame/dispatch [::http/load-flow-w-alias
                                                                    (get-in @db/flow-results [:run-refs sub-flow-lookup 0]) (str sub-flow-lookup)]))
                            :on-click #(select-block bid)
                            :on-mouse-leave #(reset! flow-hover nil)}
                           {})
                   :child (let [;in-max  (apply max (map count (vals inputs)))
                                pill-fn (fn [src out?]
                                          (vec (for [[k v] src
                                                     :let [idx (.indexOf (keys src) k)
                                                           is-last? (= k (last (keys src)))
                                                           is-first? (= k (first (keys src)))
                                                           dcolor (try (get (theme-pull :theme/data-colors db/data-colors) (name v))
                                                                       (catch :default _ (get (theme-pull :theme/data-colors db/data-colors) (str v))))
                                                           added? (and expandable-in?
                                                                       (not (some #(= % k) required))) ;; dont allow delete of "required" flow block ports
                                                           [xs ys] [(if out? (+ x w) x) (+ y 4 (+ 6 (* idx pill-size)))]
                                                           dcolor (ifnil dcolor (get (theme-pull :theme/data-colors db/data-colors) "unknown" "#FFA500"))
                                                           body
                                                           ^{:key (str "port" idx bid k v)}
                                                           [re-com/box
                                                            :child (str k) ;; (if added? (str k " -") (str k))
                                                            :attr (cond out? {}
                                                                        (and (not out?) added?) {:on-context-menu #(do (remove-input bid k)
                                                                                                                       (re-frame/dispatch [::delete-port bid k :in])
                                                                                                                       (reset! ports-react-render (rand-int 12345)))}
                                                                        :else {:on-context-menu #(remove-input bid k)})
                                                            ;:align :center ; (if out? :end :start)
                                                            :justify (if out? :end :start)
                                                            ;:justify :center
                                                            :size "none"
                                                            :height (px pill-size)
                                                            :width (if out?
                                                                     (px longesto-w) ;"45px" 
                                                                     (px longesti-w))
                                                       ;:width "100px"
                                                            :style {:background-color dcolor
                                                                    :border-radius (cond (and is-first? is-last?)
                                                                                         (if out?
                                                                                           "0px 10px 0px 12px"
                                                                                           "10px 0px 12px 0px")
                                                                                         is-last? (if out?
                                                                                                    "0px 0px 0px 12px"
                                                                                                    (if (and too-tall?
                                                                                                             (not expandable-in?))
                                                                                                      "0px 0px 12px 12px"
                                                                                                      "0px 0px 12px 0px"))
                                                                                         is-first? (if out?
                                                                                                     "0px 7px 0px 0px"
                                                                                                     "10px 0px 0px 0px")
                                                                                         :else "none")
                                                                    :font-size "11px"
                                                                    (if out? :padding-right :padding-left) "4px"
                                                                    :user-select "none"
                                                                    :cursor (if out? "grab" "inherit")
                                                                    :color "#000000" :font-weight 700}]]]
                                                 (if out? [draggable-port body flow-id bid k v xs ys] body))))
                                in-pills (when (not @dragging-port?) (pill-fn inputs false))
                                in-pills (if (empty? in-pills) [[re-com/box
                                                                 :child (str (cond (= ttype :open-block)  (if only-overridden? :overwritten! :input)
                                                                                   expandable-in? :no-inputs
                                                                                   :else ttype)) ;"no inputs"
                                                                        ;:attr (if out? {} {:on-context-menu #(remove-input bid k)})
                                                                 :align :start
                                                                 :justify :center
                                                                 :size "none"
                                                                 :height (px (+ 3 pill-size))
                                                                 :width  (px longesti-w) ;"80px"
                                                                 :style {:background-color "#00000000"
                                                                         :border-radius "12px 0px 12px 0px"
                                                                         :border-right (str "1px solid " bcolor)
                                                                         :border-bottom (str "1px solid " bcolor)
                                                                         :padding-left "9px"
                                                                         :user-select "none"
                                                                         :font-size "11px"
                                                                         :opacity 0.55
                                                                         :color bcolor :font-weight 400}]] in-pills)
                                in-pills (if expandable-in? (conj in-pills
                                                                  [re-com/box
                                                                   :child "+"
                                                                   :align :start
                                                                   :justify :center
                                                                   :size "none"
                                                                   :attr {:on-click #(do (re-frame/dispatch [::add-port bid :in])
                                                                                         (reset! ports-react-render (rand-int 12345)))}
                                                                   :height (px (+ 3 pill-size))
                                                                   :width  (px (- longesti-w 10))
                                                                   :style {:background-color "#00000000"
                                                                           :cursor "pointer"
                                                                                 ;:border-radius "12px 0px 12px 0px"
                                                                                 ;:border-right (str "1px solid " bcolor)
                                                                                 ;:border-bottom (str "1px solid " bcolor)
                                                                           :padding-left "9px"
                                                                           :user-select "none"
                                                                           :font-size "18px"
                                                                           :opacity 0.55
                                                                           :color bcolor :font-weight 400}]) in-pills) ;; TODO compress all this logic into single cond binding
                                out-pills (conj (pill-fn outputs true)
                                                (if (and hovered? (not @dragging-flow?))
                                                  [re-com/box
                                                   :width (px longesto-w)
                                                   :size "none" :height "15px"
                                                   :style {:left (px (+ x (- w longesto-w)))
                                                           :position "fixed"
                                                           :padding-right "7px"
                                                           :top (px (+ y (- h 29)))}
                                                   :align :end :justify :center
                                                   :child [re-com/md-icon-button :src (at)
                                                           :md-icon-name "zmdi-close"
                                                           :on-click #(remove-block bid)
                                                           :style {;:background-color (theme-pull :theme/editor-rim-color nil) ;"#00000000"
                                                                   :color bcolor ;(theme-pull :theme/editor-font-color nil)
                                                                   :cursor "pointer"
                                                                   ;:position "fixed"
                                                                   ;:left (px (- (+ x longesti-w cw (/ longesto-w 2)) 8))
                                                                   ;:top (px (+ y (- h 17)))
                                                                   :height "15px"
                                                                   ;:margin-top "-9px"
                                                                   :font-size "19px"}]]
                                                  [re-com/box
                                                   :align :end :justify :center
                                                   :width (px longesto-w) ;:size "auto"
                                                   :style {;:background-color (theme-pull :theme/editor-rim-color nil) ;"#00000000"
                                                           :color bcolor ;(theme-pull :theme/editor-font-color nil)
                                                           :cursor "pointer"
                                                           :position "fixed"
                                                           :padding-right "7px"
                                                           ;:border "1px solid white"
                                                           ;:left (px (+ x (- w 30)))
                                                           :left (px (+ x (- w longesto-w)))
                                                           :top (px (+ y (- h 25)))
                                                           :height "15px"
                                                           ;:margin-top "-14px"
                                                           :font-size "19px"}
                                                   :child [render-icon icon bcolor]]))]
                           ;; (tap> [:bb bid inputs])


                            [re-com/h-box
                             :style {:z-index 605}
                             :width (px w) :size "none"
                             ;:justify :between ;:align :center
                             ;:height (px h)
                             :children [[re-com/box
                                         :size "auto"
                                         :width (px (- w 10)) ;(px (* w 2.25))
                                         :attr {:on-mouse-down mouse-down-fn}
                                         :style {:color bcolor
                                                 :cursor (if @dragging? "grabbing" "grab")
                                                 :position "fixed"
                                                 :overflow "visible"
                                                 :white-space "nowrap"
                                                       ;:border "1px solid orange"
                                                 :left (+ 5 x) ;(if hovered? (- x xx) x)
                                                 :top (- y 23)}
                                         :align :center
                                         :justify :start
                                         :child (str bid)]

                                        (when done-block?
                                          [re-com/box
                                           :size "auto"
                                           :width (px (- w 5))
                                           :style {:color bcolor
                                                   :position "fixed"
                                                         ;:border "1px solid orange"
                                                   :padding-right "6px"
                                                   :left (+ 5 x) ;(if hovered? (- x xx) x)
                                                   :top (+ y h)}
                                           :align :center
                                           :justify :end
                                           :child ":end-step"])

                                        (when (and (not @dragging-port?))
                                                   ;(not (empty? in-pills))

                                          [re-com/v-box
                                           ;:width (px 80)
                                           :width (px longesti-w)
                                           :size "none"
                                           :height (px (- h 8))
                                           :align :start :justify :start
                                         ;:align :center :justify :start
                                           :style {:z-index 505
                                                   ;:border "1px solid lime"
                                                   :margin-top "-1px"
                                                   :margin-left "-5px"}
                                           :children (conj
                                                      (conj in-pills
                                                            (when (not too-tall?)
                                                              [re-com/box
                                                               :width (px 80)
                                                               :attr {:on-mouse-down mouse-down-fn}
                                                               :size "1" :align :center :justify :center
                                                               :child " " :style {;:border "0px solid cyan" 
                                                                                  :cursor (if @dragging? "grabbing" "grab")}]))
                                                      (when false ;(not (<= longest-w 100))
                                                        [re-com/v-box
                                                         :style {:margin-top "15px" :margin-left "10px"}
                                                         :height "30px"
                                                         :align :start :justify :center
                                                         :children [;(when (= ttype :param) [re-com/box :child (str :param)])
                                                            ;(when (= ttype :open-block) [re-com/box :child (str :input)])
                                                                    (when value [re-com/box
                                                                                 :style {:font-size "10px"}
                                                                                 :child (ut/truncate-string (str value) 30)])
                                                        ;;     (when (not (or (= ttype :open-block) (= ttype :param)))
                                                        ;;       [render-icon icon bcolor])
                                                                    (when true ;(or (= ttype :open-block) (= ttype :param))
                                                                      [re-com/box
                                                                       :style {:opacity 0.33 :font-size "6px"}
                                                                       :child (or (get-in ports [:out :out])
                                                                                  (when (get-in ports [:out :*]) :map) "?")])]]))])

                                        (if @dragging-port?
                                          (let [filtered-inputs (filter #(or (try (some (fn [x] (= x (last @dragged-port))) (val %)) (catch :default _ false))
                                                                             (= (val %)
                                                                                (try (last @dragged-port) ;; in case of nil, etc
                                                                                     (catch :default _ nil)))
                                                                             (= (val %) :any))
                                                                        inputs)
                                                opts-cnt (count filtered-inputs)
                                                opts-cnt (if (odd? opts-cnt) (+ 1 opts-cnt) opts-cnt)
                                                single? (= opts-cnt 1)]
                                            ;(tap> [bid (last @dragged-port) inputs filtered-inputs ])

                                            [re-com/v-box
                                             :size "auto"
                                             :width (px (- w 8))
                                             :height (px (- h 10))
                                             :children (for [ins (partition-all 2 filtered-inputs)
                                                             :let [h (- h 12)
                                                                   w (- w 13)
                                                                   row-height (if single? h (/ h (/ opts-cnt 2)))]]
                                                         [re-com/h-box
                                                          :size "none"
                                                          :height (px row-height)
                                                          :width (px (- w 8))
                                                          :children (for [i ins
                                                                          :let [ddcolor (get (theme-pull :theme/data-colors db/data-colors) (gn (val i))
                                                                                             (get (theme-pull :theme/data-colors db/data-colors) "unknown" "#FFA500"))
                                                                                hovered? (= @flow-drop-hover [bid (key i)])
                                                                                pid (key i)
                                                                                droppable-port (fn [element] [(reagent/adapt-react-class rdnd/Droppable)
                                                                                                              {:types   [:flow-port]
                                                                                                               :on-drop #(let [[srcf srcl _] @dragged-port
                                                                                                                               src (if (= srcl :out) srcf ;; no alias for simple */out 
                                                                                                                                       (keyword (str (gn srcf) "/" (gn srcl))))
                                                                                                                               dest (if (= pid :in) bid
                                                                                                                                        (keyword (str (gn bid) "/" (gn pid))))]
                                                                                                                           (reset! dragging-port? false)
                                                                                                                           (tap> [:dropped src :to dest])
                                                                                                                           (connect-ports src dest))}
                                                                                                              [re-com/box
                                                                                                               :align :center :justify :center
                                                                                                               :height (px row-height)
                                                                                                               :width (px (/ w (count ins)))
                                                                                                            ;;    :attr {;:on-drag-enter #(reset! flow-drop-hover [bid (key i)])
                                                                                                            ;;           :on-drag-over #(when (not hovered?)
                                                                                                            ;;                            (reset! flow-drop-hover [bid (key i)]))
                                                                                                            ;;         ;;   :on-drag-over #(do
                                                                                                            ;;         ;;                    (.stopPropagation %)
                                                                                                            ;;         ;;                    (when (not hovered?)
                                                                                                            ;;         ;;                      (reset! flow-drop-hover [bid (key i)])))
                                                                                                            ;;           :on-drag-leave #(when (not (contains? (.-target %) (.-relatedTarget %)))
                                                                                                            ;;                             (reset! flow-drop-hover nil))}
                                                                                                               :size "none"
                                                                                                               :child element]])]]
                                                                      [re-com/box ;:child (str i)
                                                                       :size "none" ;:width (px (- w 40))
                                                                       :style {:color ddcolor
                                                                             ;:width (if (and single? hovered?) "100%" "inherit")
                                                                             ;:height (if (and single? hovered?) "100%" "inherit")
                                                                              ; :transition "all 0.3s ease-in-out"
                                                                               :background-color (if hovered? (str ddcolor 33) "inherit")
                                                                             ;:font-size (if hovered? "20px" "14px")
                                                                               :border (str "2px solid " ddcolor)}
                                                                       :align :center :justify :center
                                                                       ;:child [droppable-port (str (key i)) bid (key i)]
                                                                       :child [droppable-port (str (key i))]])])])
                                                                       ;:child "hey"


                                          (let [valuestr (if (number? value) (ut/nf value) value)
                                                tstr (ut/truncate-string (str valuestr) (* (/ cw 5) (apply max [(count inputs) (count outputs)])))
                                                slen (count tstr)
                                                hex? (ut/is-hex-color? valuestr)
                                                ;slen (if (< slen 9) 9 slen)
                                                fsize (if (< slen 45) (auto-font-size-px tstr h cw) 10)
                                                fsize (if (> fsize 55) 55 fsize)]
                                            ;(tap> [:font-szie bid tstr fsize value])
                                            [re-com/v-box
                                             :size "none"
                                           ;:width (px (+ (- w (+ 80 45) 5)))
                                           ;:align :start :justify :start
                                             :width (px cw)
                                             :attr {:on-mouse-down mouse-down-fn}
                                             :style {:color (if hex? (ut/invert-hex-color valuestr) bcolor)
                                                     :background-color (if hex? valuestr "inherit")
                                                     :border-radius (when hex? "14px")
                                                     ;:margin (when hex? "5px")
                                                   ;:border "1px solid white"
                                                     :cursor (if @dragging? "grabbing" "grab")}
                                             :align :center :justify :center
                                             :children (if true ; (<= longesti-w 100)
                                                         [;(when (= ttype :param) [re-com/box :child (str :param)])
                                                            ;(when (= ttype :open-block) [re-com/box :child (str :input)])
                                                          (when value [re-com/box
                                                                       :width (px cw) :size "none"
                                                                       :height (px (- h 30))
                                                                       ;:align :center 
                                                                       :justify :center
                                                                       :style {:font-size (if hex? "38px" (px fsize))
                                                                               ;:font-size "10px"
                                                                             ;:border "1px solid white"
                                                                               :overflow "hidden"
                                                                               :font-weight 700
                                                                               :overflow-wrap "break-word"
                                                                               :word-wrap "break-word"}
                                                                       :child (str tstr)])
                                                        ;;     (when (not (or (= ttype :open-block) (= ttype :param)))
                                                        ;;       [render-icon icon bcolor])
                                                          (when true ;(or (= ttype :open-block) (= ttype :param))
                                                            [re-com/box
                                                             :style {:opacity 0.33 :font-size "8px"}
                                                             :child (if hex?
                                                                      "css hex color string"
                                                                      (or (keyword (ut/data-typer value))
                                                                          (get-in ports [:out :out])
                                                                          (when (get-in ports [:out :*]) :map) "?"))])]
                                                         [])]))

                                        [re-com/v-box
                                         ;:width (px 45)
                                         ;:width (px longesto-w)
                                         :size "none"
                                         ;:align :end :justify :end
                                         ;:align :start :justify :start
                                         :style {:z-index 505
                                                 ;:border "1px solid yellow"
                                                       ;:margin-right "-4px"
                                                 :margin-top "-1px"
                                                 :margin-left "2px"
                                                 ;:margin-right "-10px"
                                                 :display (if (not @dragging-port?) "inherit" "none")}
                                         :children (conj out-pills
                                                         [re-com/box
                                                          :attr {:on-mouse-down mouse-down-fn}
                                                          :size "1" :align :center :justify :center
                                                          :child " " :style {;:border "1px solid cyan" 
                                                                             :cursor (if @dragging? "grabbing" "grab")}])]]])])

                (when (cstr/includes? (str flow-id) "/")
                  [re-com/box
                   :style {:position "fixed"
                           :left (- bounds-x 50)
                           :top (- bounds-y 30)
                           :width (px (+ (- bounds-x2 bounds-x) 150))
                           :height (px (+ (- bounds-y2 bounds-y) 100))
                                                                                      ;:background-color "red"
                           :border-radius "14px"
                           :font-size "10px"
                           :border (str "6px solid " done-data-type-color)
                           :z-index 500}
                   :padding "5px"
                   :child "read-only sub-flow"]))]))
                ;;      (when true ;@dragging-port? 
                ;;        (let [[xxx yyy] (translate-x-y [(- ww 100) 10])
                ;;              zoom-multi 1 ;(get @db/pan-zoom-offsets 2)
                ;;              widthx (px (* zoom-multi 80))
                ;;              heightx (px (* zoom-multi 900))]
                ;;          [re-com/box
                ;;           :size "none"
                ;;           :width  widthx
                ;;           :height heightx
                ;;           :style {:position "fixed"
                ;;                   :background-color "maroon"
                ;;                   :left xxx
                ;;                   ;:right xxx
                ;;                   :top yyy
                ;;                   :z-index 700
                ;;                   :border "1px solid yellow"}
                ;;           :child "fart"]))


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
  ;(tap> [:dropped? types-vec root @bricks/dragging-body element])
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
      :on-drop #(if (and (= (get-in @bricks/dragging-body [:drag-meta :source-query]) :flows-sys) (not @drop-toggle?))
                  (let [file-path @(re-frame/subscribe [::conn/clicked-parameter-key [:flows-sys/file_path]])]
                    (re-frame/dispatch [::http/load-flow file-path])
                    (tap> [:load-flow file-path]))
                  (let [incoming   (js->clj %)
                      ;_ (tap> [:start? types-vec (last (last incoming)) (map? (last (last incoming))) (try (read-string (last (last incoming))) (catch :default e (str e)))])
                        data       (read-string (last (last incoming)))
                        [x y]      root
                        sub-flow-part? (not (empty? (get-in @bricks/dragging-body [:flow-item :file-path])))
                        sub-flow-drop? (or (and (= (get-in @bricks/dragging-body [:drag-meta :source-query]) :flows-sys) @drop-toggle?) sub-flow-part?)
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
                        dm-type     (get-in rooted-data [:drag-meta :type])
                        _ (tap> [:dropeed-offsets root dm-type zoom-multi zoom-offset-x zoom-offset-y drop-x drop-y rooted-data])
                        flow-item (get @bricks/dragging-body :flow-item)
                        flow-body (cond
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
                                            (get-in rooted-data [:drag-meta :param-full])))]

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
                      :else (do (tap> [:flow-inherits-drag-body? @bricks/dragging-body drop-x drop-y flow-body flow-id])
                                (add-block drop-x drop-y flow-body flow-id)))))}
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

(defonce flow-browser-panel? (reagent/atom true))

(defn smooth-scroll-to-element [container-id element-id]
  (let [container (gdom/getElement container-id)
        element (gdom/getElement element-id)
        container-left (.-scrollLeft container)
        ;element-left (.-offsetLeft element)
        element-left (- (.-offsetLeft element) (if @flow-browser-panel? 640 40))]
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
        element-left (- (.-offsetLeft element) (if @flow-browser-panel? 610 10))]
    (set! (.-scrollLeft container) (- element-left container-left))))

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
             :font-weight   700}
     :child [(reagent/adapt-react-class cm/UnControlled)
             {:value   (ut/format-map (- width-int 24)
                                      (str value))
             ; :onBlur  #(re-frame/dispatch-sync [::update-selected-field kp (read-string (cstr/join " " (ut/cm-deep-values %)))])
              :options {:mode              syntax ; "clojure"
                        :lineWrapping      true
                        :lineNumbers       true
                        :matchBrackets     true
                        :autoCloseBrackets true
                        :autofocus         false
                        :autoScroll        false
                        :detach            true
                        :readOnly          true            ;true
                        :theme             (theme-pull :theme/codemirror-theme nil)}}]])) ;"ayu-mirage" ;"hopscotch"


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
                        :theme             (theme-pull :theme/codemirror-theme nil)}}]])) ;"ayu-mirage" ;"hopscotch"


(defn code-box-fn [width-int height-int value bid]
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
             {:value   (ut/format-map (- width-int 50)
                                      (str value))
              :onBlur #(try
                         (let [new-vals (read-string (cstr/join " " (ut/cm-deep-values %)))]
                           (re-frame/dispatch [::update-flowmap-key bid :raw-fn new-vals]))
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
                        :theme             (theme-pull :theme/codemirror-theme nil)}}]])) ;"ayu-mirage" ;"hopscotch"


(defn code-box-rwo [width-int height-int value bid & [syntax]]
  (let [syntax (or (if (= syntax "raw (clojure)") "clojure" syntax) "clojure")
        stringify? (not (= syntax "clojure"))]
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
             {:value   (if stringify?
                         (str (cstr/join "\n" value))
                         (ut/format-map (- width-int 24)
                                        (pr-str value)))
              ;:onBlur  #(re-frame/dispatch-sync [::update-selected-field kp (read-string (cstr/join " " (ut/cm-deep-values %)))])
              :onBlur  #(if stringify?
                          (let [ddata (ut/cm-deep-values %) ;(str "\"" (str (cstr/join "\n" (ut/cm-deep-values %))) "\"") 
                                full (-> (get @(re-frame/subscribe [::flowmap]) bid)
                                         (assoc-in [:ports :out] {:out :any})
                                         (assoc-in [:data :user-input] ddata))]
                            (re-frame/dispatch [::update-flowmap-key bid nil full]))

                          (try (let [ddata (read-string (cstr/join " " (ut/cm-deep-values %)))
                                     dtype (ut/data-typer ddata)
                                     ports {;:in (get-in v [:ports :in])
                                            :out {:out (keyword dtype)}}
                                     ports (cond (= dtype "map")
                                                 {:out (assoc (into {} (for [[k v] ddata] {k (keyword (ut/data-typer v))})) :* :map)}
                                                 (or (= dtype "vector") (= dtype "rowset")) ; (= dtype "vector")
                                                 {:out (assoc (into {} (for [k (range (count ddata)) :let [v (get ddata k)]] {(keyword (str "idx" k)) (keyword (ut/data-typer v))})) :* :vector)}
                                                 :else ports)
                                     full (-> (get @(re-frame/subscribe [::flowmap]) bid)
                                              (assoc-in [:data :user-input] ddata)
                                              (assoc-in [:ports] ports))]
                               ;(swap! flowmaps assoc bid full)
                                 (re-frame/dispatch [::update-flowmap-key bid nil full]))
                               (catch :default e (let [] ;; [vval (get @(re-frame/subscribe [::flowmap-raw]) bid)]
                                                   (tap> [:saving-issue :code-box-rwo bid (str e) :parse-issue?])))))
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
                        :theme             (theme-pull :theme/codemirror-theme nil)}}]])) ;"ayu-mirage" ;"hopscotch"

(def rename-block (reagent/atom nil))

(defn flow-details-block [cch id fmap]
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
        cvalue (get-in @db/flow-results [:return-maps flow-id id] "(not run yet)")
        error? (get cvalue :error false)
        styler (get-in fmap [:data :flow-item :style] {})
        ;styler-selected (get-in fmap [:data :flow-item :style-selected] {})
        outputs (get-in fmap [:ports :out])
        override-value (get-in @db/flow-results [:return-maps (str flow-id) id])
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
                         ;:height           "30px"
                         ;:on-change         #(do (re-frame/dispatch [::rename-flow flow-id %])
                         ;                        (reset! title-edit-idx nil))
                         :on-change #(do (if (= str-id (str %)) ;; regex doesnt allow : so we need to remove before
                                           (reset! rename-block nil)
                                           (do (re-frame/dispatch [::rename-block id %])
                                               (reset! rename-block nil))))
                         :validation-regex  flow-id-regex
                         :change-on-blur?   true
                         :style  {;:font-size "20px"
                                        ;:margin-top "28px"
                                 ; :border (str "2px dashed " (theme-pull :theme/editor-outer-rim-color nil))
                                 ; :font-size "30px"
                                  :text-decoration "underline"
                                  :color "inherit" ;(theme-pull :theme/editor-outer-rim-color nil)
                                                       ;:font-weight 700
                                 ; :font-style "underline"
                                        ;:border "0px solid black"
                                        ;:padding "8px"
                                  ;:margin-top "-3px"
                                  :margin-top "3px"
                                  :margin-left "-4px"
                                  :text-align "left"
                                                 ; :float "right"
                                                     ;:margin-top "10px"
                                                     ;:padding-top "10px"
                                           ;          :text-align "right"
                                  :background-color "#00000000"}]
                                                   ;:color (theme-pull :theme/editor-font-color nil)
                                           ;          :padding-top "1px"

                        [re-com/box
                         :attr (if selected? {:on-double-click #(reset! rename-block id)} {})
                         :child (str id)])
        show-output? (get-in fmap [:data :view-output?] false)
        output-toggle [re-com/md-icon-button :src (at)
                       :md-icon-name "zmdi-more"
                       :on-click #(re-frame/dispatch [::flow-block-meta id :view-output? (not show-output?)])
                       :style {:color ccolor ; (theme-pull :theme/editor-outer-rim-color nil)
                               :cursor "pointer"
                               ;:height "15px"
                               ;:margin-left (if @flow-browser-panel? "-9px" "9px")
                               :font-size "17px"}]
        ;cvalue (if error? 
        ;         (get cvalue :error) 
        ;           cvalue)
        return-val (get-in fmap [:data :user-input] cvalue)
        default-output [re-com/v-box
                        :gap "9px" :padding "8px"
                        :width "350px" :height "380px" :size "none"
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
                                          :size "none" :height "280px" :width "350px"
                                          :style {:overflow "auto"}
                                          :child [buffy/render-honey-comb-fragments return-val 7 5]]

                                         (vector? return-val)
                                         [re-com/box
                                          :size "none" :height "280px" :width "350px"
                                          :style {:overflow "auto"}
                                          :child [shape/map-boxes2 return-val :sub-flows-used nil [] :output "vector"]]

                                         (map? return-val)
                                         [re-com/box
                                          :size "none" :height "280px" :width "350px"
                                          :style {:overflow "auto"}
                                          :child [shape/map-boxes2 return-val :sub-flows-used nil [] :output "map"]]

                                         :else
                                         [re-com/box
                                          :size "none" :width "350px" :height "280px"
                                          :child [code-box 370 280 (if (string? return-val) (pr-str return-val) return-val)]])]]]
   ; (tap> [:blocks id fmap])
    [re-com/v-box
     :attr {:id (str id)
            :on-drag-over #(when @bricks/swap-layers? (reset! bricks/swap-layers? false))
            :on-click #(select-block id)}
     ;:width "505px"
     :height cch
     :size "none"
     :padding "10px"
     :min-width "130px"
     :align :center
     :justify :center
     :style {:color ccolor ;(str (theme-pull :theme/editor-font-color nil) 35)
             :border (str "4px " (if selected? "dashed" "solid")  " "
                          (if (or selected? hovered?) out-color
                              (str (theme-pull :theme/editor-outer-rim-color nil) 16)))
             :border-radius "12px"
             :margin-top "13px"}
     ;:attr {:on-click #(select-block id)}
     :children [(cond
              ;true [code-box 500 400 fmap]
                  show-output? ;; force default output render
                  default-output

                  (= ttype :open-fn)
                  [re-com/v-box :size "none" :gap "9px" :padding "6px"
                   :align :center :justify :center
                   :width "350px" :height "380px"
                   :style {:margin-top "-5px"}
                   :children [[re-com/h-box
                               :width "350px" :size "none"
                               :justify :between :align :center
                               :padding "8px" :style {:margin-top "17px"}
                               :children [;[re-com/box :child (str id)]
                                          block-renamer
                                          [re-com/box :child "clojure"]]]
                              [re-com/box
                               :child [code-box-fn 350 378 (str (get fmap :raw-fn "(fn [x] x)")) id]]]]

                  (= type :sub-flow)
                  (let [;flowmaps-connections @(re-frame/subscribe [::flowmap-connections])
                    ;flow-id @(re-frame/subscribe [::selected-flow])
                    ;flow-map @(re-frame/subscribe [::flowmap])
                        subby-results (get-in @db/flow-results [:return-maps])
                        sub-flow-lookup (into {} (for [k (keys subby-results)] {(keyword (last (cstr/split (str k) "/"))) k}))
                        sfl (get sub-flow-lookup id)]
                    ;(tap> [:lookups sub-flow-lookup sfl])
                    [re-com/box
                     :width "460px" :height "380px" :size "none"
                     :style {;:border "1px solid pink" 
                             :overflow "auto"}
                     :child [re-com/v-box
                             :width "455px" ;:height (px (+ 600 (* 6 bricks/brick-size)))
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
                                         :width "440px"
                                         :height "340px"
                                         :size "none"
                                         :child [re-com/v-box
                                                 :gap "18px"
                                                 :children (vec (for [i (filter #(not (= (str %) (str flow-id))) (keys subby-results))]
                                                                  [re-com/v-box
                                                                   :children
                                                                   [[re-com/box :child (str i) :style {:font-size "19px"}]
                                                                    [shape/map-boxes2 (get subby-results i) :sub-flows-used nil [] :sub-flows-used "map"]]]))]]]]])
                                                    ;:child (buffy/render-honey-comb-fragments body 10 10)

                  (= type :view) (let [qid (get-in fmap [:data :drag-meta :source-table])
                                       body (get-in fmap [:data :drag-meta :body])]
                                   [re-com/box
                                    :width "500px" :height "380px" :size "none"
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
                  [re-com/box
                   :align :start :justify :start
                   :child output-toggle]
                  [re-com/gap :size "17px"])]]))

(defonce scheduler-atom (reagent/atom {}))

(defn flow-editor [w h]
  (let [react-hack [@editor-mode]
        sql-params (into {} (for [k [:flow-fn-categories-sys/category]]
                              {k @(re-frame/subscribe [::conn/clicked-parameter-key [k]])}))
        flow-select @(re-frame/subscribe [::selected-flow-block])
        flowmaps @(re-frame/subscribe [::flowmap-raw])
        flow-id @(re-frame/subscribe [::selected-flow])
        read-only-flow? (true? (cstr/includes? flow-id "/"))
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
                                ;:group-by [:db_schema :db_catalog :connection_id :table_name]
                                ;:order-by [:schema_cat :table_name]

    (dorun (for [[k v] sql-calls]
             (let [query (walk/postwalk-replace sql-params v)
                   data-exists? @(re-frame/subscribe [::conn/sql-data-exists? [k]])
                   unrun-sql? @(re-frame/subscribe [::conn/sql-query-not-run? [k] query])]
               (when (or (not data-exists?) unrun-sql?)
                 (if (get query :connection-id)
                   (conn/sql-data [k] query (get query :connection-id))
                   (conn/sql-data [k] query))))))
    [re-com/box
     :size "none"
     :width (px w) :height (px h)
     :style {;:border-right (str "6px solid " (theme-pull :theme/editor-outer-rim-color nil))
             :border-top (str "6px solid " (theme-pull :theme/editor-outer-rim-color nil))
             ;:border-right (str "1px solid " "#00000098")
             :backdrop-filter "blur(1px)"
          ; :filter "drop-shadow(0.35rem 0.35rem 0.4rem rgba(0, 0, 0, 0.8))"
           ;:mix-blend-mode "screen"
             :background-color (str (theme-pull :theme/editor-outer-rim-color nil) 19)}
             ;:border-radius "0px 16px 0px 0px"

     :align :center :justify :center
     :child (cond
              (= @editor-mode :run-history)
              (let [viz1 {:queries
                          {:gen-viz-1090
                           {:select   [[[[:count 1]] :value]
                                       [[:substr :start_ts 0 11] :day]]
                            :connection-id "flows-db"
                            :from     [:query/channel-history-drag-13]
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
                           [:base_flow_id :flow_id [[:max :end_ts] :end]
                            [[:min :start_ts] :start]
                            [[:count 1] :evts]
                            [[:round
                              [:*
                               [:- [:julianday [:max :end_ts]]
                                [:julianday [:min :start_ts]]] 1440] 2]
                             :mins]]
                           :connection-id "flows-db"
                           :group-by [1 2]
                           :order-by [[4 :desc]]
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
                            [re-com/box
                             :align :center :justify :center
                             :child "hey hey hye"]
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
                [re-com/box :child "no block selected"])

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
                                           :background-color "#00000000"}])
                                           ;_ (tap> [:flow-list comps])
                    ]
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
                                        [ddown comps (first comps) 180 :trigger-word-insert]
                                        ;[date-input #"^\d{0,4}-?\d{0,2}-?\d{0,2}$" 130 "2024-01-01"]
                                        ;[re-com/box :child "at"]
                                        ;[date-input #"^\d{4}$" 145 "<some-val>" :trigger-value]
                                        ]]

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

              :else [re-com/box :child (str "unknown editor mode: " @editor-mode)])]))

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

(defn flow-details-panel [panel-height panel-width details-panel-height]
  (let [react-hacks [@flow-hover @scroll-id @editor-mode]
        ;details-panel-height (/ panel-height 1.25)
        block-keys (keys @(re-frame/subscribe [::flowmap]))
        flow-id @(re-frame/subscribe [::selected-flow])
        flow-select @(re-frame/subscribe [::selected-flow-block])
        running? (is-running? :* flow-id)
        panel-width (- panel-width 28) ;; modded
        browser-panel-width 600
        browser-panel-height (- details-panel-height 42)
        read-only-flow? (true? (cstr/includes? flow-id "/"))
        ;flow-id-regex #"(.|\s)*\S(.|\s)*"
        flow-id-regex #"^[a-zA-Z0-9_-]+$" ;; alpha, underscores, hypens, numbers
        cch (px (- details-panel-height 70))]

    (when (and (not @dragging-port?) (not @db/dragging-flow-editor?) (not @bricks/dragging-editor?) (not @dragging?) (not @bricks/dragging?))
      (reagent.core/next-tick #(smooth-scroll-to-element "scrolly-boy" (str flow-select))))

    [re-com/v-box
     :children [[re-com/h-box
                 :width (px panel-width) :size "none" :height "38px"
                 :style {:padding-left "4px"
                         :font-size "18px"
                        ;:margin-top "-10px"
                        ;:border "1px solid yellow"
                         :padding-right "4px"}
                 :justify :between
                 :children [[re-com/box
                             :child (if (and (empty? @editor-tooltip-atom) running?) (str :flow-name " is running")
                                        (str (or @editor-tooltip-atom "")))
                             :max-width (px (- panel-width 300))
                             :style {;:border "2px solid green"
                                     :overflow "hidden"}]

                            ;; [re-com/box
                            ;;  :style (if (or @flow-hover flow-select)
                            ;;           (let [kkey (cond (and (not @flow-hover) flow-select) flow-select
                            ;;                            @flow-hover @flow-hover :else nil)
                            ;;                 out-color (get (theme-pull :theme/data-colors db/data-colors)
                            ;;                                (gn (get-in @(re-frame/subscribe [::flowmap]) [kkey :ports :out :out])) (get (theme-pull :theme/data-colors db/data-colors) "unknown" "#FFA500"))]
                            ;;             {:color out-color})
                            ;;             ;:text-decoration (if (and (not @flow-hover) @flow-select) "underline" "none")
                            ;;           {})
                            ;;  :child (str (cond
                            ;;               ;(and (empty? @editor-tooltip-atom) running?) (str :flow-name " is running")
                            ;;                (and (not (nil? @flow-hover)) (= @flow-hover flow-select)) (str flow-select " selected")
                            ;;                @flow-hover (str "hovering over " @flow-hover)
                            ;;                :else (when flow-select (str
                            ;;                                         (when @flow-hover ", ")
                            ;;                                         flow-select " selected"))))]

                            ;[re-com/box :child "hey asshole"]
                            (if (not @title-edit-idx)
                              [re-com/box
                               ;:padding "8px"
                               :size "none"
                               ;:height            "35px"
                               :attr {:on-double-click #(when (not read-only-flow?) (reset! title-edit-idx (str flow-id)))}
                               :style {;:text-decoration "underline"
                                       :margin-top "-5px"
                                       :cursor "pointer"
                                       :margin-right "13px"
                                       ;:border "2px solid lime"
                                       ;:border (str "2px solid #00000000")
                                       :font-size "30px"}
                               :child (str flow-id)]

                              [re-com/input-text
                               :src (at)
                               :model             (str flow-id)
                               :width             "600px"
                               ;:height            "35px"
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
                                        :text-align "right"
                                                 ; :float "right"
                                                     ;:margin-top "10px"
                                                     ;:padding-top "10px"
                                           ;          :text-align "right"
                                        :background-color "#00000000"}])]]
                                                   ;:color (theme-pull :theme/editor-font-color nil)
                                           ;          :padding-top "1px"

                [re-com/h-box
                 :children [;[re-com/gap :size "8px"]
                            (when @flow-browser-panel?
                              [re-com/box
                              ;:style {:margin-top "30px"}
                              ;:style {:border "1px solid yellow"  }
                               :child [flow-editor browser-panel-width browser-panel-height]
                               :width (px browser-panel-width)
                               :height (px browser-panel-height)])
                            [re-com/v-box
                             :gap "20px"
                             :height (px browser-panel-height)
                             :style {;:background-color (theme-pull :theme/editor-rim-color nil)
                                     :border-radius "0px 16px 0px 0px"
                                    ;:border-right (str "6px solid " (theme-pull :theme/editor-outer-rim-color nil))
                                    ;:border "1px solid yellow"
                                     :padding-top "10px" :padding-right (if (not @flow-browser-panel?) "5px" "inherit")
                                     :border-top (str "6px solid " (theme-pull :theme/editor-outer-rim-color nil))
                                     :border-right (str "1px solid " (theme-pull :theme/editor-outer-rim-color nil) 25)
                                     :background-color (str (theme-pull :theme/editor-outer-rim-color nil) 15)}
                             :width "40px"
                             :align :center :justify :start
                             :children [[re-com/md-icon-button :src (at)
                                         :md-icon-name (if  @flow-browser-panel? "zmdi-chevron-left" "zmdi-chevron-right")
                                         :on-click #(do (reset! flow-browser-panel? (not @flow-browser-panel?))
                                                        (reset! editor-tooltip-atom nil))
                                         :attr {:on-mouse-enter #(reset! editor-tooltip-atom (str "toggle flow work panel " (if @flow-browser-panel? "open" "closed")))
                                                :on-mouse-leave #(reset! editor-tooltip-atom nil)}
                                         :style {;:background-color (theme-pull :theme/editor-rim-color nil) ;"#00000000"
                                                ;:opacity 0.6
                                                 :color (theme-pull :theme/editor-outer-rim-color nil)
                                                 :cursor "pointer"
                                                 :height "15px"
                                                 :margin-left (if @flow-browser-panel? "-9px" "9px")
                                                 :font-size "33px"}]
                                        [re-com/gap :size  "3px"]
                                        [re-com/md-icon-button :src (at)
                                         :md-icon-name "zmdi-view-list"
                                        ;:on-click #(reset! flow-browser-panel? (not @flow-browser-panel?))
                                         :attr {:on-mouse-enter #(reset! editor-tooltip-atom (str "flow browser"))
                                                :on-click #(reset! editor-mode :flow-browser)
                                                :on-mouse-leave #(reset! editor-tooltip-atom nil)}
                                         :style {;:background-color (theme-pull :theme/editor-rim-color nil) ;"#00000000"
                                                 :opacity (when (not (= :flow-browser @editor-mode)) 0.33)
                                                 :color (theme-pull :theme/editor-outer-rim-color nil)
                                                 :cursor "pointer"
                                                 :height "15px"
                                                 :margin-left (if @flow-browser-panel? "-9px" "9px")
                                                 :font-size "19px"}]
                                        [re-com/md-icon-button :src (at)
                                         :md-icon-name "zmdi-shape"
                                        ;:on-click #(reset! flow-browser-panel? (not @flow-browser-panel?))
                                         :attr {:on-mouse-enter #(reset! editor-tooltip-atom (str "flow part browser"))
                                                :on-click #(reset! editor-mode :part-browser)
                                                :on-mouse-leave #(reset! editor-tooltip-atom nil)}
                                         :style {;:background-color (theme-pull :theme/editor-rim-color nil) ;"#00000000"
                                                 :opacity (when (not (= :part-browser @editor-mode)) 0.33)
                                                 :color (theme-pull :theme/editor-outer-rim-color nil)
                                                 :cursor "pointer"
                                                 :height "15px"
                                                 :margin-left (if @flow-browser-panel? "-9px" "9px")
                                                 :font-size "19px"}]
                                        [re-com/md-icon-button :src (at)
                                         :md-icon-name "zmdi-time-interval"
                                        ;:on-click #(reset! flow-browser-panel? (not @flow-browser-panel?))
                                         :attr {:on-mouse-enter #(reset! editor-tooltip-atom (str "flow scheduler"))
                                                :on-click #(reset! editor-mode :scheduler)
                                                :on-mouse-leave #(reset! editor-tooltip-atom nil)}
                                         :style {;:background-color (theme-pull :theme/editor-rim-color nil) ;"#00000000"
                                                 :opacity (when (not (= :scheduler @editor-mode)) 0.33)
                                                 :color (theme-pull :theme/editor-outer-rim-color nil)
                                                 :cursor "pointer"
                                                 :height "15px"
                                                 :margin-left (if @flow-browser-panel? "-9px" "9px")
                                                 :font-size "19px"}]
                                        [re-com/md-icon-button :src (at)
                                         :md-icon-name "zmdi-view-subtitles"
                                        ;:on-click #(reset! flow-browser-panel? (not @flow-browser-panel?))
                                         :attr {:on-mouse-enter #(reset! editor-tooltip-atom (str "run history"))
                                                :on-click #(reset! editor-mode :run-history)
                                                :on-mouse-leave #(reset! editor-tooltip-atom nil)}
                                         :style {;:background-color (theme-pull :theme/editor-rim-color nil) ;"#00000000"
                                                 :opacity (when (not (= :run-history @editor-mode)) 0.33)
                                                 :color (theme-pull :theme/editor-outer-rim-color nil)
                                                 :cursor "pointer"
                                                 :height "15px"
                                                 :margin-left (if @flow-browser-panel? "-9px" "9px")
                                                 :font-size "19px"}]

                                        [re-com/md-icon-button :src (at)
                                         :md-icon-name "zmdi-bug"
                                        ;:on-click #(reset! flow-browser-panel? (not @flow-browser-panel?))
                                         :attr {:on-mouse-enter #(reset! editor-tooltip-atom (str "flow block debug editor"))
                                                :on-click #(reset! editor-mode :debug)
                                                :on-mouse-leave #(reset! editor-tooltip-atom nil)}
                                         :style {;:background-color (theme-pull :theme/editor-rim-color nil) ;"#00000000"
                                                 :opacity (when (not (= :debug @editor-mode)) 0.33)
                                                 :color (theme-pull :theme/editor-outer-rim-color nil)
                                                 :cursor "pointer"
                                                 :height "15px"
                                                 :margin-left (if @flow-browser-panel? "-9px" "9px")
                                                 :font-size "19px"}]]]


                                ;;        [re-com/md-icon-button :src (at)
                                ;;         :md-icon-name "zmdi-view-list"
                                ;;         ;:on-click #(reset! flow-browser-panel? (not @flow-browser-panel?))
                                ;;         :attr {:on-mouse-enter #(reset! editor-tooltip-atom (str "server-side flow browser"))
                                ;;                :on-click #(reset! editor-mode :server-flows)
                                ;;                :on-mouse-leave #(reset! editor-tooltip-atom nil)}
                                ;;         :style {;:background-color (theme-pull :theme/editor-rim-color nil) ;"#00000000"
                                ;;                 :opacity (when (not (= :server-flows @editor-mode)) 0.33)
                                ;;                 :color (theme-pull :theme/editor-outer-rim-color nil)
                                ;;                 :cursor "pointer"
                                ;;                 :height "15px"
                                ;;                 :margin-left (if @flow-browser-panel? "-9px" "9px")
                                ;;                 :font-size "19px"}]


                            (if (not (= :run-history @editor-mode))
                              [re-com/h-box
                               :attr {:id "scrolly-boy"}
                       ;:on-mouse-enter #(add-wheel-listener "scrolly-boy")
                       ;:on-mouse-leave #(remove-wheel-listener "scrolly-boy")

                               :size "none"
                               :style {:overflow "scroll"
                                       :padding-left "8px"}
                               :width (px (- panel-width (if @flow-browser-panel? browser-panel-width 0) 12))
                               :align :center :justify :start
                               :gap "5px"
                               :children [[re-com/h-box
                                           :gap "6px"
                                           :children (for [id block-keys
                                                           :let [body (get @(re-frame/subscribe [::flowmap]) id)]]
                                                       [flow-details-block cch id body])]]]

                              (let [qq1 {:select [:base_flow_id :block :channel
                                                  :data_type :dbgn :dest :elapsed_ms
                                                  :end :end_ts :flow_id :from_block
                                                  :path :start :start_ts :ts :type
                                                  :value]
                                         :connection-id "flows-db"
                                         :order-by [[8 :desc]]
                                         :from   [[:fn_history :rr582]]}]
                                [re-com/v-box
                                 :size "none" ;:padding "25px"
                                 :gap "14px"
                                 :width (px (- panel-width 611))
                                 :height (px browser-panel-height)
                                 :style {:border "1px solid white" ;:margin-top "16px"
                                         :padding-left "14px"}
                                 :children [[re-com/box
                                             :height "56px" :width (px (- panel-width 650))
                                             :style {:border "1px solid purple"}
                                             :child [buffy/render-honey-comb-fragments (waffles [] (- panel-width 650)) 21.75 2]]
                                            [buffy/render-honey-comb-fragments qq1 21.75 8]]]))]]]
     :size "none"
     :align :center
     :justify :center
     :style {:z-index 200
            ;:id "scrolly-boy"
             :font-family (theme-pull :theme/base-font nil)
             :color (theme-pull :theme/editor-font-color nil)
            ;:background-color (str (theme-pull :theme/editor-background-color nil) 99) ; "#000000" 
            ;:backdrop-filter "blur(5px)"
             :border "1px solid white"
             :position "fixed"

             :top (- panel-height 8)
             :left 13}
            ;:margin-left "-6px"
            ;:border-radius "0px 0px 16px 16px"
            ;:border-left (str "6px solid " (theme-pull :theme/editor-outer-rim-color nil))
            ;:border-right (str "6px solid " (theme-pull :theme/editor-outer-rim-color nil))
            ;:border-bottom (str "6px solid " (theme-pull :theme/editor-outer-rim-color nil))

     :height (px details-panel-height)
   ;:width (px (- panel-width 12))
     :width (px panel-width)]))

(defn alert-box2 []
  (let [rekt @db/kick-alert
        alerts @(re-frame/subscribe [::bricks/alerts])]
    [re-com/box
     :child [re-com/h-box
             :children
             [;[:img {:src "images/test-kick-icon.png" :width 30 :height 30}]
              [re-com/md-icon-button :src (at)
               :md-icon-name (if @db/kick-alert "zmdi-star" "zmdi-star-outline")
               :style {;:color "red"
                       :margin-right "12px"
                       :margin-top "-4px"
                       :font-size "34px"}]
              [re-com/box
               ;:style {:opacity (if @db/kick-alert 1.0 0.0)}
               :child
               [buffy/render-honey-comb-fragments (first (first alerts))]]]]
     :width "420px"
     :height "50px"
     :padding "9px"
;     attr {:on-click #(reset! db/kick-alert (not @db/kick-alert))}

     :style {:position "fixed"
             :font-size "18px"
             :border-left (str "2px solid " (theme-pull :theme/editor-outer-rim-color nil) (if @db/kick-alert "" "01"))
             :border-top (str "2px solid " (theme-pull :theme/editor-outer-rim-color nil) (if @db/kick-alert "" "01"))
             :border-bottom (str "2px solid " (theme-pull :theme/editor-outer-rim-color nil) (if @db/kick-alert "" "01"))
             :font-weight 700
             :cursor "pointer"
             :z-index 98
             :border-radius "19px 0px 0px 19px"
             :bottom 25
             :transition "all 0.6s ease-in-out"
             :right (if @db/kick-alert 0 -370)
             :backdrop-filter "blur(4px)"
             :background-color (str "#000000" (if @db/kick-alert 88 11))
             :color "white"}]))

(defn alert-box []
  [rc/catch
   (let [rekt @db/kick-alert
         rs-running @(re-frame/subscribe [::bricks/runstreams-running])
         alerts @(re-frame/subscribe [::bricks/alerts])
         alerts (if (> rs-running 0)
                  (conj alerts [[:box :child (str rs-running " flow" (when (> rs-running 1) "s") " running")] 4 1 0])
                  alerts)
         ;;alerts (reverse alerts)
         ;;alerts (conj alerts (when (> rs-running 0) [[:box :child (str rs-running " flows running")] 8 1 0]))
         max-w (apply max (for [a alerts
                                :let [width (* (get a 1) bricks/brick-size)]]
                            width))
         max-w (if (or (nil? max-w) (< max-w 50)) 420 max-w)
         alerts-cnt (try (count alerts) (catch :default _ 0))
         gap 1 ;11
         all-h (apply + (for [a alerts
                              :let [h (* (get a 2) bricks/brick-size)]]
                          h))
         all-h (if (> alerts-cnt 1) (+ all-h (* gap (dec alerts-cnt))) all-h)
         all-h (if (or (nil? all-h) (< all-h 50)) 50 all-h)
         box-height 50
         box-width (+ 20 max-w) ;420

         edge-hide (* (- box-width 50) -1)]
     ;(tap> [:widths max-w all-h edge-hide])
     (when (= alerts-cnt 0) (reset! db/kick-alert false))
     (when (> alerts-cnt 0) (reset! db/kick-alert true))
     [re-com/box
      :child [re-com/h-box
              :children
              [;[:img {:src "images/test-kick-icon.png" :width 30 :height 30}]
               [re-com/md-icon-button :src (at)
                :md-icon-name (if @db/kick-alert "zmdi-star" "zmdi-star-outline")
                :style {;:color "red"
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
                            width (* (get a 1) bricks/brick-size)
                            height (* (get a 2) bricks/brick-size)]]
                  [re-com/box
                   :size "none" :width (px width) :height (px height)
                            ;:on-click ()
                            ;:style {:border "1px solid white"}
                   :child [buffy/render-honey-comb-fragments abody (get a 1) (get a 2)]])]]]
      :width (px box-width)
      :height (px (if @db/kick-alert all-h box-height))
      :padding "9px"
     ;:attr {:on-click #(reset! db/kick-alert (not @db/kick-alert))}
      :attr (if @db/kick-alert {:on-click #(re-frame/dispatch [::bricks/prune-alerts true])} {})
      :style {:position "fixed"
              :font-size "18px"
              :border-left (str "2px solid " (theme-pull :theme/editor-outer-rim-color nil) (if @db/kick-alert "" "01"))
              :border-top (str "2px solid " (theme-pull :theme/editor-outer-rim-color nil) (if @db/kick-alert "" "01"))
              :border-bottom (str "2px solid " (theme-pull :theme/editor-outer-rim-color nil) (if @db/kick-alert "" "01"))
              :font-weight 700
              :cursor "pointer"
              :z-index 98
              :border-radius "19px 0px 0px 19px"
              :bottom 25
              :transition "all 0.6s ease-in-out"
              :right (if @db/kick-alert 0 edge-hide)
              :backdrop-filter "blur(4px)"
              :background-color (str "#000000" (if @db/kick-alert 88 11))
              :color "white"}])])

(re-frame/reg-event-db
 ::flip-drop-toggle
 (fn [db _]
   (reset! drop-toggle? (not @drop-toggle?))
   db))

(defn flow-panel []
  (let [[x y] @detached-coords
        x-px (px x)
        y-px (px y)
        panel-width (* (.-innerWidth js/window) 0.7)
        hh @(re-frame/subscribe [::subs/h]) ;; to ensure we get refreshed when the browser changes
        ww @(re-frame/subscribe [::subs/w])
        flow-id @(re-frame/subscribe [::selected-flow])
        flowmaps @(re-frame/subscribe [::flowmap-raw])
       ;; flowmappp [@(re-frame/subscribe [::flowmap]) @ports-react-render]
        flowmaps-connections @(re-frame/subscribe [::flowmap-connections])
       ;flowmaps-connections @(re-frame/subscribe [::flowmap-connections])
        ;flow-id @(re-frame/subscribe [::selected-flow])
        flow-map @(re-frame/subscribe [::flowmap])
        running? (is-running? :* flow-id)
        has-done? (has-done?)
        ;single-panel-size 600
        panel-height 600
        details-panel-height (/ panel-height 1.25)
        ppanel-height (+ panel-height details-panel-height)
        flow-id @(re-frame/subscribe [::selected-flow])

        ;coords (generate-coords x y)
        ;fart (cstr/replace nil "5" "55") ;; error test
        ;choices [[:yo :yo] [:yo1 :yo1] [:fook :fook]]
        choices [[:o (str "@bricks/over-flow? " @bricks/over-flow?)]
                 [:o0 (str "@bricks/swap-layers?" @bricks/swap-layers?)]
                 [:o1 (str "@dragging-port? " @dragging-port?)]
                 ;[:o1 (str "@flow-select " @flow-select)]
                 [:o2 (str :coords @db/pan-zoom-offsets)]]]

        ;;choices []


    [rc/catch
     [re-com/box
      :size "none"
      :width (px panel-width)
      ;:height (if @flow-details-panel? (px (+ panel-height (/ panel-height 1.33))) (px panel-height))
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
              :backdrop-filter "blur(5px)"
              :border (str "6px solid " (theme-pull :theme/editor-outer-rim-color nil))
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
                                        :children (for [[v [file-path name]] (get @db/flow-results :run-refs [])]
                                                    [re-com/box :child (str v)
                                                    ;;  :attr {:on-click #(do (swap! chat-mode assoc kp c)
                                                    ;;                        (swap! kit-mode assoc kp v))}
                                                     :attr {:on-click #(let [sub-flow-exec? (not (nil? file-path))]
                                                                         (if sub-flow-exec?
                                                                           (re-frame/dispatch [::http/load-flow-w-alias file-path v])
                                                                           (re-frame/dispatch [::set-selected-flow (str v)])))}

                                                     :style (if (= (str v) (str flow-id))
                                                             ; {:background-color (get (theme-pull :theme/data-colors db/data-colors) "unknown" "#FFA500") :color "black"}
                                                              {:text-decoration "underline"}
                                                              {:cursor "pointer" :opacity 0.6})
                                                     :padding "4px"])]]



                                       ;;(get-in @db/flow-results [flow-id :return-maps])

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

                           (flow-droppable ["meta-menu"]
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
                                                                               block-body {;:y 1286.9742504454496
                                                                                           :w 200
                                                                                          ;:raw-fn '(fn [x] x)
                                                                                          ;:fn '(fn [x] x)
                                                                                           :icon "zmdi-functions"
                                                                                           :z 0
                                                                                           :ports {:in {:value :any} :out {:out :any}}
                                                                                           :h 100
                                                                                           :right-click? true
                                                                                           :fn '(fn [x] x)
                                                                                           :raw-fn '(fn [x] x)
                                                                                                ;:x 1609.0022576583058
                                                                                           :data
                                                                                           {:flow-item
                                                                                            {:description "Write you own Clojure function! Godspeed!"
                                                                                             :category ":rabbit-base"
                                                                                             :fn '(fn [x] x)
                                                                                             :name ":open-fn"
                                                                                             :raw-fn '(fn [x] x)
                                                                                             :type :open-fn
                                                                                             :icon "zmdi-functions"
                                                                                             :types {:value :any :out :any}
                                                                                             :input_types "[:any]"
                                                                                             :inputs "[:value]"
                                                                                             :output_types ":any"
                                                                                             :expandable? true
                                                                                             :full_map
                                                                                             "{:description \"Write you own Clojure function! Godspeed!\", :fn (fn [x] x), :raw-fn (fn [x] x), :type :open-fn, :icon \"zmdi-functions\", :types {:value :any, :out :any}, :inputs [:value], :expandable? true, :view (fn [_])}"}
                                                                                           ; :view (fn [_])

                                                                                            :name "drag-from-"
                                                                                            :w 5
                                                                                            :source-panel :flow-fn-list*
                                                                                           ;:root [288.1770935058594 355.171875]
                                                                                            :h 6
                                                                                            :connection-id nil
                                                                                            :queries {:name-drag-25 {:select [:name :full_map :description
                                                                                                                              :inputs :icon :input_types
                                                                                                                              :output_types :category]
                                                                                                                     :from [[:query/flow-fn-sys :ii11]]
                                                                                                                     :where [:= :name ":open-fn"]}}
                                                                                            :drag-meta {:data-type "string"
                                                                                                        :source-query :flow-fn-sys
                                                                                                        :param-full ":open-fn"
                                                                                                        :param-field :name
                                                                                                        :source-panel-key :flow-fn-list*
                                                                                                        :type :open-fn
                                                                                                        :param-table :flow-fn-sys
                                                                                                        :source-table :query/flow-fn-sys
                                                                                                        :connection-id nil
                                                                                                        :target :name
                                                                                                        :row-num 5}}}]
                                                                           (add-block (- (first @db/context-modal-pos) x 10)
                                                                                      (- (last @db/context-modal-pos) y 33) block-body :open-fn)))
                                                                       #_{:clj-kondo/ignore [:unresolved-symbol]}
                                                                       (.preventDefault %)))
                                                   :on-context-menu (re-com/handler-fn
                                                                     #_{:clj-kondo/ignore [:unresolved-symbol]}
                                                                     (bricks/tag-screen-position event) ;; event is magic in handler-fn
                                                                     (when (nil? @flow-hover) ;(and ;(not @bricks/over-block?)  (not @flow-hover))
                                                                       (let [starting-val "feed me, seymour!"
                                                                             open-block-body {:w 200 :h 100
                                                                                              :data {:drag-meta {:type :open-block}
                                                                                                     :flow-item {:expandable? true}
                                                                                                     :user-input starting-val}
                                                                                              ;:x drop-x :y drop-y 
                                                                                              :z 0
                                                                                              :ports {;;;:in {:in :string}
                                                                                                      :in {}
                                                                                                      :out {:out :string}}}]
                                                                         (add-block (- (first @db/context-modal-pos) x 10)
                                                                                    (- (last @db/context-modal-pos) y 33) open-block-body :open-input)))
                                                                     #_{:clj-kondo/ignore [:unresolved-symbol]}
                                                                     (.preventDefault event))}
                                            :size "none"
                                            :width (px (- panel-width 12))
                                            :height (if (and @flow-details-panel?
                                                             (not @bricks/dragging-editor?)
                                                             (not @db/dragging-flow-editor?)) (px (- ppanel-height 35)) (px (- panel-height 35)))
                                            :child [re-com/v-box
                                                    :size "1"
                                                    :style (merge ;(theme-pull :theme/canvas-background-css nil)
                                                            {:font-family (theme-pull :theme/base-font nil)
                                                             :z-index 200
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
                                                                       y-bounds (+ y panel-height)
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
                                                                              :filter "drop-shadow(16px 16px 20px orange)" ;;  invert(175%)
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




                                                                    ;(let [] ; [pan-zoom-offsets (reagent/atom [10 -33 1])] ;;[_ (when-let [f (-> @db/zoomer-state .-resetTransform)] (f))] ;;[react-hack [@db/pan-zoom-offsets]]


                                                               [re-com/box
                                                                :size "none" :style {;:border "2px solid red" 
                                                                                     :overflow "hidden"}
                                                                :width (px (- panel-width 12))
                                                                :height (px (- panel-height 35))
                                                                :child [(reagent/adapt-react-class zpan/TransformWrapper)
                                                                        {;:key (str @db/pan-zoom-offsets)
                                                                         :onInit (fn [comp] (reset! db/zoomer-state comp))
                                                                         :limitToBounds false ;true ;false 
                                                                         :disabled (or @flow-hover
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
                                                                         :wheel {:step 0.05}
                                                                         :minScale 0.025}
                                                                        [(reagent/adapt-react-class zpan/TransformComponent)
                                                                         [re-com/v-box ;:style {:border "1px dashed hotpink"}
                                                                          :children [[rc/catch
                                                                                      [flow-grid panel-width panel-height x y flowmaps-connections flow-id flow-map]]

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
                                                                                                 :width "6400px" ;(px ww) 
                                                                                                 :height "3600px" ;(px hh)
                                                         ;:width   (px ww) ;"6200px" ;; big ass render nothing gets cut off svg-wise + zoom and pannable
                                                         ;:height  (px hh) ;"6200px"
                                                                                                 ;:border "2px solid white"
                                                                                                 :position "fixed"
                                                                                                 :pointer-events "none"
                                                                                       ;:left xx :top yy
                                                                                       ;:transform (str "translate(" tx "px, " ty "px) scale(" scale ")")
                                                         ;:z-index 499
                                                                                                 :z-index 200}}
                                                                                        (draw-lines (rvbbit-frontend.flows/generate-coords xx yy))])]]]]];)





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


                                                               [re-com/box
                                                                :child [re-com/md-icon-button :src (at)
                                                                        :md-icon-name (if @flow-details-panel?
                                                                                        "zmdi-chevron-up"
                                                                                        "zmdi-chevron-down")
                                                                        :style {:color (str (theme-pull :theme/editor-outer-rim-color nil))
                                                                                :cursor "pointer"
                                                                                :font-size "35px"}
                                                                        :on-click #(reset! flow-details-panel? (not @flow-details-panel?))]
                                                                :style {:position "fixed"
                                                                        :opacity 0.45
                                                                        :z-index 98
                                                                        :top (- panel-height 45)
                                                                        :right (/ panel-width 2)
                                                                        :background-color "#00000022"
                                                                        :color "white"}]

                                                               (if running?
                                                                 [re-com/box
                                                                  :child [re-com/md-icon-button :src (at)
                                                                          :md-icon-name "zmdi-refresh-sync"
                                                                          :class "rotate linear infinite"
                                                                          :style {:color (str (theme-pull :theme/editor-outer-rim-color nil))
                                                                                  :cursor "pointer"
                                                                                  :transform-origin "21.5px 22px"
                                                                                  ;:margin-top "-5px"
                                                                                  ;:padding-left "2px"
                                                                                  :font-size "43px"}
                                                                          :on-click #(re-frame/dispatch [::run-current-flowmap])]
                                                                        ;:on-click #(reset! flow-details-panel? (not @flow-details-panel?))

                                                                  :style {:position "fixed"
                                                                         ; :opacity 0.45
                                                                          :z-index 98
                                                                          :top (- panel-height 60)
                                                                          :left 15
                                                                          :background-color "#00000022"
                                                                          :color "white"}]

                                                                 (if has-done?
                                                                   [re-com/box
                                                                    :child [draggable-play
                                                                            [re-com/md-icon-button :src (at)
                                                                             :md-icon-name "zmdi-play"
                                                                             :attr {:on-mouse-enter #(reset! editor-tooltip-atom (str "run flow live"))
                                                                                 ;:on-click #(reset! editor-mode :part-browser)
                                                                                    :on-mouse-leave #(reset! editor-tooltip-atom nil)}
                                                                             :on-click #(re-frame/dispatch [::run-current-flowmap])
                                                                             :style {:color (str (theme-pull :theme/editor-outer-rim-color nil))
                                                                                     :cursor "pointer"
                                                                                  ;:margin-top "-5px"
                                                                                  ;:padding-left "2px"
                                                                                     :font-size "55px"}]
                                                                            flow-id]
                                                                        ;:on-click #(reset! flow-details-panel? (not @flow-details-panel?))

                                                                    :style {:position "fixed"
                                                                            :opacity 0.45
                                                                            :z-index 98
                                                                            :top (- panel-height 65)
                                                                            :left 15
                                                                            :background-color "#00000022"
                                                                            :color "white"}]

                                                                   [re-com/box
                                                                    :child [re-com/md-icon-button :src (at)
                                                                            :md-icon-name "zmdi-pause"
                                                                            ;:on-click #(re-frame/dispatch [::run-current-flowmap])
                                                                            :style {:color (str (theme-pull :theme/editor-outer-rim-color nil))
                                                                                    :cursor "pointer"
                                                                                    :font-size "55px"}]

                                                                    :style {:position "fixed"
                                                                            :opacity 0.45
                                                                            :z-index 98
                                                                            :top (- panel-height 65)
                                                                            :left 15
                                                                            :background-color "#00000022"
                                                                            :color "white"}]))

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
                                                                                  ;:margin-top "-5px"
                                                                                  ;:padding-left "2px"
                                                                                  :font-size "43px"}]
                                                                        ;:on-click #(reset! flow-details-panel? (not @flow-details-panel?))

                                                                  :style {:position "fixed"
                                                                          :opacity 0.22
                                                                          :z-index 98
                                                                          :top (- panel-height 60)
                                                                          :left 62
                                                                          :background-color "#00000022"
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
                                                                                                            :flow-id flow-id
                                                                                                            :flowmaps-connections flowmaps-connections}
                                                                                                           flow-id])
                                                                                       (ut/dispatch-delay 3000 [::conn/clear-query-history :flows-sys]))
                                                                          :style {:color (str (theme-pull :theme/editor-outer-rim-color nil))
                                                                                  :cursor "pointer"
                                                                                  ;:margin-top "-5px"
                                                                                  ;:padding-left "2px"
                                                                                  :font-size "43px"}]

                                                                  :style {:position "fixed"
                                                                          :opacity 0.22
                                                                          :z-index 98
                                                                          :top (- panel-height 60)
                                                                          :left 113
                                                                          :background-color "#00000022"
                                                                          :color "white"}])

                                                               (when true ; (not running?)
                                                                 [re-com/box
                                                                  :child [re-com/md-icon-button :src (at)
                                                                          :attr {:on-mouse-enter #(reset! editor-tooltip-atom (str "new flow"))
                                                                                 ;:on-click #(reset! editor-mode :part-browser)
                                                                                 :on-mouse-leave #(reset! editor-tooltip-atom nil)}
                                                                          :on-click #(re-frame/dispatch [::new-flow])
                                                                          :md-icon-name "zmdi-widgets"
                                                                          ;; :on-click #(re-frame/dispatch [::http/save-flow
                                                                          ;;                                {:flowmaps flowmaps
                                                                          ;;                                 :flow-id flow-id
                                                                          ;;                                 :flowmaps-connections flowmaps-connections}
                                                                          ;;                                "name"])
                                                                          :style {:color (str (theme-pull :theme/editor-outer-rim-color nil))
                                                                                  :cursor "pointer"
                                                                                  ;:margin-top "-5px"
                                                                                  ;:padding-left "2px"
                                                                                  :font-size "43px"}]

                                                                  :style {:position "fixed"
                                                                          :opacity 0.22
                                                                          :z-index 98
                                                                          :top (- panel-height 60)
                                                                          :right 10
                                                                          :background-color "#00000022"
                                                                          :color "white"}])

                                                               (when (and @dragging-port?
                                                                         ; @bricks/swap-layers? ;(not @bricks/swap-layers?)
                                                                          @bricks/over-flow?)
                                                                 [(reagent/adapt-react-class rdnd/Droppable)
                                                                  {:types   [:flow-port]
                                                                   :on-drop #(let [[srcf srcl _] @dragged-port
                                                                                   src (if (= srcl :out) srcf ;; no alias for simple */out 
                                                                                           (keyword (str (gn srcf) "/" (gn srcl))))]
                                                                               (reset! dragging-port? false)
                                                                               (tap> [:dropped src :to :done])
                                                                               (connect-ports src :done))}
                                                                  (let [ccolor (theme-pull :theme/editor-outer-rim-color nil)]
                                                                    [re-com/box
                                                                     :child [re-com/box
                                                                             ;:height "80px"
                                                                             :width "500px"
                                                                             :padding "8px"
                                                                             :align :end :justify :center
                                                                             :style {:transform "rotate(90deg)"
                                                                                     :margin-bottom (* -1 (* 10 (get @db/pan-zoom-offsets 2)))
                                                                                     :font-size "38px"}
                                                                             :child "done & return value"]
                                                                     :style {:position "fixed" :top 33 :right 0
                                                                             :border-left (str (* 6 (get @db/pan-zoom-offsets 2)) "px dashed " ccolor)
                                                                             :border-top (str (* 6 (get @db/pan-zoom-offsets 2)) "px dashed " ccolor)
                                                                             :border-bottom (str (* 6 (get @db/pan-zoom-offsets 2)) "px dashed " ccolor)
                                                                             :border-radius "16px 0px 0px 16px"}
                                                                     :height "500px"
                                                                     :width "80px"])])

                                                               (when (and @flow-details-panel?
                                                                          (not @bricks/dragging-editor?)
                                                                          (not @db/dragging-flow-editor?))
                                                                 [rc/catch [flow-details-panel panel-height panel-width details-panel-height]])]]])]])]]))
