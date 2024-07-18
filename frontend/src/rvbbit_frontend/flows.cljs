(ns rvbbit-frontend.flows
  (:require
    ["codemirror/addon/edit/closebrackets.js"]
    ["codemirror/addon/edit/matchbrackets.js"]
    ["codemirror/mode/clojure/clojure.js"]
    ["codemirror/mode/julia/julia.js"]
    ["codemirror/mode/python/python.js"]
    ["codemirror/mode/r/r.js"]
    ["codemirror/mode/sql/sql.js"]
    ["react-codemirror2" :as cm]
    ["react-drag-and-drop" :as rdnd]
    ["react-zoom-pan-pinch" :as zpan]
    [cljs-drag-n-drop.core :as dnd2]
    [cljs.core.async :as    async
                     :refer [<! >! chan]]
    [cljs.tools.reader :refer [read-string]]
    [clojure.data :as cdata]
    [clojure.edn :as edn]
    [clojure.set :as cset]
    [clojure.string :as cstr]
    [clojure.walk :as walk]
    [clojure.walk :as w]
    [day8.re-frame.undo :as    undo
                        :refer [undoable]]
    [garden.color :as    c
                  :refer [analogous color? complement hex->hsl hex? hsl hsl->hex hsla invert mix rgb->hex rgb->hsl shades
                          split-complement tetrad triad]]
    [goog.dom :as gdom]
    [goog.events :as gevents]
    ;[re-catch.core :as rc]
    [re-com.core :as    re-com
                 :refer [at]]
    [re-com.util :refer [px]]
    [re-frame.core :as re-frame]
    [reagent.core :as reagent]
    [rvbbit-frontend.audio :as audio]
    [rvbbit-frontend.bricks :as    bricks
                            :refer [theme-pull]]
    [rvbbit-frontend.buffy :as buffy]
    [rvbbit-frontend.connections :as conn]
    [rvbbit-frontend.connections :refer [sql-data]]
    [rvbbit-frontend.db :as db]
    [rvbbit-frontend.http :as http]
    [rvbbit-frontend.resolver :as resolver]
    [rvbbit-frontend.signals :as sig]
    [rvbbit-frontend.subs :as subs]
    [rvbbit-frontend.utility :as ut]
    [talltale.core :as tales]
    [websocket-fx.core :as wfx])
  (:import
    [goog.events EventType]
    [goog.async  Debouncer]))

(defonce flow-details-panel? (reagent/atom false))
(def flow-drop-hover (reagent/atom nil))
(def flow-hover (reagent/atom nil))
(def involved (reagent/atom []))
(def pan-lock? (reagent/atom false))
(def dragging-port? (reagent/atom false))
(defonce lookup-modal? (reagent/atom false))
(def dragged-port (reagent/atom []))
(defonce dragging? (reagent/atom false))
(defonce dragging-flow? (reagent/atom false))
(def editor-tooltip-atom (reagent/atom nil))
(defonce title-edit-idx (reagent/atom nil))
(defonce drop-toggle? (reagent/atom false))
(defonce channel-holster (reagent/atom {}))

(def canvas-width 7500)
(def canvas-height 6000)

(defonce flow-details-block-container-atom (reagent/atom {}))

(def port-hover (reagent/atom {}))
(def port-hover2 (reagent/atom {}))



(defn gn [x] (try (name x) (catch :default _ x)))
(defn gns [x] (try (namespace x) (catch :default _ x)))
(defn gns? [x] (not (nil? (try (namespace x) (catch :default _ false)))))


(defonce editor-mode (reagent/atom :flow-browser))


(re-frame/reg-sub ::flowmap-raw
                  (fn [db]
                    (let [fmaps (get-in db [:flows (get db :selected-flow) :map] {})
                          fmaps (into {} (for [[k v] fmaps :when (get v :w)] {k v}))]
                      fmaps)))

(re-frame/reg-sub ::flowmap-raw-block (fn [db [_ bid]] (get-in db [:flows (get db :selected-flow) :map bid] {})))

(re-frame/reg-sub ::selected-flow
                  (fn [db]
                    (let [animal  (ut/replacer (str (tales/animal)) #" " "-")
                          flow-id (str animal "-flow-" (rand-int 1000))]
                      (get db :selected-flow flow-id))))

(re-frame/reg-event-db ::set-selected-flow
                       (fn [db [_ flow-id]]
                         (ut/tapp>> [:set-flow flow-id])
                         (let [flow-id      (str flow-id)
                               curr-flow-id (get db :selected-flow)
                               curr-flow    (get-in db [:flows curr-flow-id] {})]
                           (-> db
                               (dissoc :selected-flow-block)
                               (assoc :selected-flow flow-id)))))

(re-frame/reg-event-db ::rename-flow
                       (fn [db [_ old-flow-id new-flow-id]]
                         (ut/tapp>> [:rename-flow old-flow-id :to new-flow-id])
                         (let [] ;flow-id (str flow-id)
                           (ut/tracked-dispatch [::http/set-flow-results
                                                 (ut/postwalk-replacer {old-flow-id new-flow-id}
                                                                       @(ut/tracked-subscribe [::http/flow-results]))])
                           (-> db
                               (assoc :flows (ut/postwalk-replacer {old-flow-id new-flow-id} (get db :flows)))
                               (assoc :selected-flow new-flow-id)))))

(re-frame/reg-event-db
  ::rename-block
  (fn [db [_ old-bid new-bid]]
    (let [curr-flow-id        (get db :selected-flow)
          block-ids           (keys (get-in db [:flows curr-flow-id :map]))
          inputs              (keys (get-in db [:flows curr-flow-id :map old-bid :ports :in]))
          outputs             (keys (get-in db [:flows curr-flow-id :map old-bid :ports :out]))
          new-bid             (keyword new-bid)
          new-bid             (ut/safe-key new-bid @(ut/tracked-subscribe [::conn/reserved-type-keywords]))
          input-replacements  (into {}
                                    (for [k inputs]
                                      {(keyword (str (ut/safe-name old-bid) "/" (ut/safe-name k)))
                                         (keyword (str (ut/safe-name new-bid) "/" (ut/safe-name k)))}))
          output-replacements (into {}
                                    (for [k outputs]
                                      {(keyword (str (ut/safe-name old-bid) "/" (ut/safe-name k)))
                                         (keyword (str (ut/safe-name new-bid) "/" (ut/safe-name k)))}))
          pw-replace-map      (merge input-replacements output-replacements {old-bid new-bid})]
      (ut/tapp>> [:rename-block old-bid :to new-bid :with pw-replace-map]) ;; block id needs to
      (if (not (some #(= new-bid %) block-ids)) ;; only if is unique, else do nothing
        (do (ut/tracked-dispatch [::http/set-flow-results
                                  (ut/postwalk-replacer pw-replace-map @(ut/tracked-subscribe [::http/flow-results]))])
            (-> db
                (assoc :selected-flow-block new-bid)
                (assoc-in [:flows curr-flow-id] (ut/postwalk-replacer pw-replace-map (get-in db [:flows curr-flow-id])))))
        db))))

(re-frame/reg-sub ::flowmap-connections (fn [db] (vec (get-in db [:flows (get db :selected-flow) :connections] []))))

(re-frame/reg-sub ::flowmap-connections-parsed
                  (fn [db [_ bid]]
                    (vec (for [[o i] (get-in db [:flows (get db :selected-flow) :connections] [])
                               :let  [ns-i? (gns i)
                                      ns-o? (gns o)
                                      bid-i (if ns-i? (keyword (gns i)) i)
                                      pid-i (if ns-i? (keyword (gn i)) :in)
                                      bid-o (if ns-o? (keyword (gns o)) o)
                                      pid-o (if ns-o? (keyword (gn o)) :out)]
                               :when (if (nil? bid) true (or (= bid bid-i) (= bid bid-o)))]
                           [(if (= bid bid-i) :in :out) bid-o pid-o bid-i pid-i]))))

(re-frame/reg-event-db
  ::clean-connections
  (undoable)
  (fn [db _]
    (let [port-map-in  (into {}
                             (for [[k v] (get-in db [:flows (get db :selected-flow) :map])]
                               {k (merge {:in :in} (get-in v [:ports :in] {}))}))
          port-map-out (into {}
                             (for [[k v] (get-in db [:flows (get db :selected-flow) :map])]
                               {k (merge {:out :out}
                                         (let [oo (get-in v [:ports :out] {})] (if (map? (get oo :out)) (get oo :out) oo)))})) ;; weird
          conns        (get-in db [:flows (get db :selected-flow) :connections] [])
          clean-conns  (vec
                         (for [[o i] conns
                               :let  [ns-i?  (gns i)
                                      ns-o?  (gns o)
                                      bid-i  (if ns-i? (keyword (gns i)) i)
                                      pid-i  (if ns-i? (keyword (gn i)) :in)
                                      bid-o  (if ns-o? (keyword (gns o)) o)
                                      pid-o  (if ns-o? (keyword (gn o)) :out)
                                      valid? (or (and (or (get-in port-map-in [bid-i pid-i]) (= bid-i :done))
                                                      (get-in port-map-out [bid-o pid-o]))
                                                 (get-in db [:flows (get db :selected-flow) :map bid-o :cond pid-o]) ;; condi
                                                                                                                     ;; port
                                                 (get-in db [:flows (get db :selected-flow) :map bid-o :push pid-o]) ;; push
                                                                                                                     ;; port
                                             )
                                      _ (when (not valid?)
                                          (ut/tapp>> [:removed-invalid-connection [o i] [[bid-i pid-i] [bid-o pid-o]]
                                                      (or (get-in port-map-in [bid-i pid-i]) (= bid-i :done))
                                                      (get-in port-map-out [bid-o pid-o]) port-map-in port-map-out]))]
                               :when valid?]
                           [o i]))
          clean-conns  (vec (remove #(or (nil? (first %)) (nil? (second %))) clean-conns))]
      (assoc-in db [:flows (get db :selected-flow) :connections] clean-conns))))

(defn mouse-move-handler
  [offset]
  (fn [evt]
    (let [start-x (.-clientX evt)
          start-y (.-clientY evt)
          off-x   (:x offset)
          off-y   (:y offset)
          x       (- start-x off-x)
          y       (- start-y off-y)]
      (reset! db/flow-detached-coords [x y]))))

(defn mouse-up-handler
  [on-move]
  (fn [evt] ;; removed "me" wtf? why did that even work? TODO check other implementations of
    (reset! db/dragging-flow-editor? false)
    (reset! flow-hover nil)
    (do (gevents/unlisten js/window EventType.MOUSEMOVE on-move))))

(defn mouse-down-handler
  [e]
  (let [{:keys [left top]} (bricks/get-client-rect e)
        offset             {:x (- (.-clientX e) left) :y (- (.-clientY e) top)}
        on-move            (mouse-move-handler offset)]
    (reset! db/dragging-flow-editor? true)
    (do (gevents/listen js/window EventType.MOUSEMOVE on-move))
    (gevents/listen js/window EventType.MOUSEUP (mouse-up-handler on-move))))

(re-frame/reg-event-db ::new-flow
                       (undoable)
                       (fn [db _]
                         (let [animal  (ut/replacer (str (tales/animal)) #" " "-")
                               flow-id (str animal "-flow-" (rand-int 1000))]
                           (reset! editor-mode :part-browser)
                           (-> db
                               (assoc :selected-flow flow-id)
                               (assoc-in [:flows flow-id] {:map {} :connections []})))))

(def trig-atom-test (reagent/atom nil))

(re-frame/reg-event-db ::refresh-live-dat
                       (fn [db _]
                         (let [kks (filter #(cstr/starts-with? (str %) ":live-dat-") (keys (get db :data)))]
                           (reset! trig-atom-test (str "refreshed live dat " (rand-int 1000)))
                           (doseq [k kks] (ut/tracked-dispatch [::conn/clear-query-history k])))))



(defn create-out-map-ports
  [ddata fid bid]
  (let [;;ddata (read-string (cstr/join " " (ut/cm-deep-values %)))
        dtype (ut/data-typer ddata)
        ports {:out (keyword dtype)}
        ports (cond (= dtype "map")                            (assoc (into {}
                                                                            (for [[k v] ddata] {k (keyword (ut/data-typer v))}))
                                                                 :* :map)
                    (or (= dtype "vector") (= dtype "rowset")) ;(= dtype "vector")
                      (assoc (into {}
                                   (for [k    (range (count ddata))
                                         :let [v (get ddata k)]]
                                     {(keyword (str "idx" k)) (keyword (ut/data-typer v))}))
                        :* :vector)
                    :else                                      ports)
        _ (ut/tapp>> [:create-out-map-ports ddata dtype ports])
        full  (-> (get @(ut/tracked-subscribe [::flowmap]) bid)
                  (assoc-in [:ports :out] ports))]
    (ut/tracked-dispatch [::update-flowmap-key-others bid fid nil full])))

(re-frame/reg-event-db ::delay-tracker
                       (fn [db [_ tracker flow-id]]
                         (swap! db/running-blocks assoc flow-id [])
                         (swap! db/real-running-blocks assoc flow-id [])
                         (-> db
                             (assoc-in [:flow-results :tracker flow-id] tracker)
                             (assoc-in [:flow-results :status] :done))))

(re-frame/reg-event-db ::socket-response-flow
                       (fn [db [_ result]]
                         (let [flow-id     @(ut/tracked-subscribe [::selected-flow])
                               return-maps (get result :return-maps)
                               return-maps (ut/replace-large-base64 return-maps) ;; since we loop over this, lets
                               _ (doseq [[fid fb] return-maps] ;; backport map keys to out ports
                                   (doseq [[bid ddata] fb
                                           :when       (and (not (string? ddata)) ;;(or (map? ddata) (vector?
                                                            (= (get ddata :syntax "clojure") "clojure")
                                                            (not (get ddata :port-in?))
                                                            (not (= bid :done)))]
                                     (create-out-map-ports ddata fid bid)))]
                           (ut/tapp>> [:flow-in result])
                           (ut/dispatch-delay 600 [::refresh-live-dat])
                           (reset! editor-tooltip-atom (str "finished. returned value: " (get result :return-val)))
                           (ut/dispatch-delay 1500 [::delay-tracker (get result :tracker-history) flow-id]) ;; TODO
                           (swap! bricks/progress-bars assoc flow-id 100) ;; just in case we finish early
                           (-> db
                               (assoc-in [:flow-results :tracker flow-id] (get result :tracker-history))
                               (assoc-in [:flow-results :run-refs] (get result :run-refs))))))

(re-frame/reg-event-db ::timeout-response
                       (fn [db [_ result what-req]]
                         (let [client-name (get db :client-name)]
                           (ut/tapp>> [:websocket-timeout! client-name result what-req])
                           db)))

(defn process-flow-map
  [fmap] ;;; IF CHANGING LOGIC HERE, ALSO CHANGE IN THE SERVER VERSION
  (into
    {}
    (for [[k v] fmap
          :let  [ttype (get-in v [:data :drag-meta :type])]]
      (cond (= ttype :query) (let [pfull (first (vals (get-in v [:data :queries])))
                                   ddata (first @(ut/tracked-sub ::resolver/logic-and-params {:m [pfull] :p nil}))
                                   dtype (ut/data-typer ddata)
                                   ports {:out {:out (keyword dtype)}}
                                   ports (cond
                                           (= dtype "map")
                                             {:out (assoc (into {} (for [[k v] ddata] {k (keyword (ut/data-typer v))})) :* :map)}
                                           (or (= dtype "vector") (= dtype "rowset"))
                                             {:out (assoc (into {}
                                                                (for [k    (range (count ddata))
                                                                      :let [v (get ddata k)]]
                                                                  {(keyword (str "idx" k)) (keyword (ut/data-typer v))}))
                                                     :* :vector)}
                                           :else ports)
                                   full  (-> v
                                             (assoc-in [:data :user-input] ddata)
                                             (assoc-in [:ports] ports))]
                               {k full})
            (= ttype :param) (let [pfull (get-in v [:data :drag-meta :param-full])
                                   ddata @(ut/tracked-subscribe [::conn/clicked-parameter-key [pfull]])
                                   dtype (ut/data-typer ddata)
                                   full  (-> v
                                             (assoc-in [:data :user-input] ddata)
                                             (assoc-in [:ports] {:out {:out (keyword dtype)}}))]
                               {k full})
            (= ttype :cell) (let [pfull (get-in v [:data :drag-meta :param-full])
                                  ddata (first @(ut/tracked-sub ::resolver/logic-and-params {:m [pfull] :p nil}))
                                  dtype (ut/data-typer ddata)
                                  ports {:out {:out (keyword dtype)}}
                                  ports (cond
                                          (= dtype "map")
                                            {:out (assoc (into {} (for [[k v] ddata] {k (keyword (ut/data-typer v))})) :* :map)}
                                          (or (= dtype "vector") (= dtype "rowset")) ;(= dtype "vector")
                                            {:out (assoc (into {}
                                                               (for [k    (range (count ddata))
                                                                     :let [v (get ddata k)]]
                                                                 {(keyword (str "idx" k)) (keyword (ut/data-typer v))}))
                                                    :* :vector)}
                                          :else ports)
                                  full  (-> v
                                            (assoc-in [:data :user-input] ddata)
                                            (assoc-in [:ports] {:out {:out (keyword dtype)}}))]
                              {k full})
            (= ttype :open-block)
              (let [pfull (if (cstr/includes? (str (get-in v [:data :syntax] "clojure")) "clojure")
                            (get-in v [:data :user-input])
                            (try (cstr/join "\n" (get-in v [:data :user-input]))
                                 (catch :default _
                                   (do (ut/tapp>> [:error-convering-string-types v :line 347 :flows-cljs])
                                       [(str (get-in v [:data :user-input]))])))) ;; converting raw
                    ddata (first @(ut/tracked-sub ::resolver/logic-and-params {:m [pfull] :p nil}))
                    dtype (ut/data-typer ddata)
                    ports {;:in (get-in v [:ports :in])
                           :out {:out (keyword dtype)}}
                    ports (cond (= dtype "map")                            {;:in (get-in v [:ports :in])
                                                                            :out (assoc (into {}
                                                                                              (for [[k v] ddata]
                                                                                                {k (keyword (ut/data-typer v))}))
                                                                                   :* :map)}
                                (or (= dtype "vector") (= dtype "rowset")) ;(= dtype "vector")
                                  {;:in (get-in v [:ports :in])
                                   :out (assoc (into {}
                                                     (for [k    (range (count ddata))
                                                           :let [v (get ddata k)]]
                                                       {(keyword (str "idx" k)) (keyword (ut/data-typer v))}))
                                          :* :vector)}
                                :else                                      ports)
                    ports (merge (get v :ports) ports)
                    full  (-> v
                              (assoc-in [:data :user-input] ddata)
                              (assoc-in [:ports] ports))]
                {k full})
            :else {k v}))))

(defn process-flowmap2
  [flowmap flowmaps-connections fid] ;;; IF CHANGING LOGIC HERE, ALSO CHANGE IN THE SERVER
  (let [canvas-key (into {} (for [[k {:keys [w h x y]}] flowmap] {k {:w w :h h :x x :y y :view-mode "text"}}))
        flowmaps-connections (vec (for [[c1 c2] flowmaps-connections]
                                    (if (cstr/ends-with? (str c1) "/*")
                                      [(keyword (-> (ut/safe-name (str c1))
                                                    (ut/replacer "/*" "")
                                                    (ut/replacer ":" ""))) c2]
                                      [c1 c2])))
        components-key
          (into
            {}
            (for [[k {:keys [data ports view file-path flow-path raw-fn sub-flow-id flow-id sub-flow]}] flowmap ;; <-- flow-id
                                                                                                                ;; refers to
                                                                                                                ;; the subflow
                                                                                                                ;; embed, not
                                                                                                                ;; the parent
                  :let [ttype       (or (get-in data [:flow-item :type]) ;; ^^-- fid is the
                                        (get-in data [:drag-meta :type]))
                        try-read    (fn [x] (try (edn/read-string x) (catch :default _ x)))
                        view-swap   (fn [obody flow-id bid push-key]
                                      (let [pkey      (keyword (str (ut/replacer (str push-key) ":" "") ">"))
                                            kps       (ut/extract-patterns obody pkey 2)
                                            logic-kps (into {}
                                                            (for [v kps]
                                                              (let [[_ that] v] {v [:push> [flow-id (str bid) that]]})))]
                                        (ut/postwalk-replacer logic-kps obody)))
                        view-swap2  (fn [obody flow-id bid push-key]
                                      (let [pkey      (keyword (str (ut/replacer (str push-key) ":" "") ">"))
                                            kps       (ut/extract-patterns obody pkey 1)
                                            logic-kps (into {} (for [v kps] (let [[_] v] {v [:push>> [flow-id (str bid)]]})))]
                                        (ut/postwalk-replacer logic-kps obody)))
                        view-swaps  (fn [obody flow-id push-key-bid-pairs]
                                      (reduce (fn [body [push-key bid]]
                                                (-> body
                                                    (view-swap flow-id bid push-key)
                                                    (view-swap2 flow-id bid push-key)))
                                        obody
                                        push-key-bid-pairs))
                        view        (when view
                                      (let [conns              (vec (filter #(cstr/includes? (str (first %)) "/push-path")
                                                                      flowmaps-connections))
                                            push-key-bid-pairs (vec (for [[c1 c2] conns]
                                                                      [(keyword (last (ut/splitter (str c1) #"/"))) c2]))
                                            view               (view-swaps view fid push-key-bid-pairs)]
                                        view))
                        nname       (get-in data [:flow-item :name] ":unknown!")
                        fn-key      (if flow-path nname (try-read nname))
                        fn-category (try-read (get-in data [:flow-item :category] ":unknown!"))]]
              (cond
                (and (= ttype :open-block) (ut/ne? (get ports :in))) ;; open block with inputs
                  {k {:data              (get data :user-input)
                      :default-overrides (get-in data [:flow-item :defaults] {})
                      :inputs            (vec (keys (get ports :in)))}}
                (or (= ttype :open-block) (= ttype :cell) (= ttype :param)) {k (if (= (get data :syntax "clojure") "clojure")
                                                                                 (get data :user-input)
                                                                                 (get data :user-input))}
                (= ttype :open-fn)                                          {k (merge (when view {:view view})
                                                                                      {:fn                raw-fn
                                                                                       :raw-fn            raw-fn
                                                                                       :default-overrides (get-in data
                                                                                                                  [:flow-item
                                                                                                                   :defaults]
                                                                                                                  {})
                                                                                       :inputs            (vec (keys (get
                                                                                                                       ports
                                                                                                                       :in)))})}
                (= ttype :query)                                            {k {:fn     (get data :user-input) ;'(fn [x] x)
                                                                                                               ;;:raw-fn '(fn
                                                                                                               ;[x] x)
                                                                                :inputs (vec (keys (get ports :in)))}}
                flow-path                                                   {k {:flow-path         flow-path
                                                                                :sub-flow-id       sub-flow-id
                                                                                :default-overrides (get-in data
                                                                                                           [:flow-item :defaults]
                                                                                                           {})
                                                                                :inputs            (vec (keys (get ports :in)))}}
                (= ttype :sub-flow)                                         {k (-> (process-flowmap2 (get sub-flow :map)
                                                                                                     (get sub-flow :connections)
                                                                                                     fid)
                                                                                   (assoc :file-path file-path)
                                                                                   (assoc :flow-id flow-id))} ;; flow-id of the
                                                                                                              ;; embdeeded
                                                                                                              ;; flow, NOT the
                :else                                                       {k {:fn-key            [fn-category fn-key]
                                                                                :default-overrides (get-in data
                                                                                                           [:flow-item :defaults]
                                                                                                           {})
                                                                                :inputs            (if false ;expandable-in?
                                                                                                     [(vec (keys (get ports
                                                                                                                      :in)))] ;; send
                                                                                                                              ;; as
                                                                                                                              ;; single
                                                                                                     (vec (keys (get ports
                                                                                                                     :in))))}})))
        components-key (into {} ;; deconstruct conditional paths to single condi key
                             (for [[k v] flowmap]
                               (if (not (empty? (get v :cond)))
                                 (let [ccond (into {}
                                                   (for [[c1 c2] flowmaps-connections
                                                         :let    [link  (keyword (str (ut/replacer (str k) ":" "") "/cond-path"))
                                                                  ff    (ut/splitter (str c1) #"/")
                                                                  cname (keyword (last ff))
                                                                  c2wo  (keyword (first (ut/splitter (ut/replacer (str c2) ":" "")
                                                                                                     #"/")))] ;; w/o port
                                                         :when   (cstr/starts-with? (str c1) (str link))]
                                                     {c2wo (get-in v [:cond cname :fn])}))]
                                   {k (assoc (get components-key k) :cond ccond)})
                                 {k (get components-key k)})))
        flowmaps-connections (vec (filter #(and (not (cstr/includes? (str (first %)) "/cond-path"))
                                                (not (cstr/includes? (str (first %)) "/push-path")))
                                    flowmaps-connections))
        server-flowmap {:canvas canvas-key :components components-key :connections flowmaps-connections}]
    server-flowmap))


(defn has-done? [] (true? (some #(= % :done) (apply concat @(ut/tracked-subscribe [::flowmap-connections])))))

(re-frame/reg-event-db ::add-live-flow-subs
                       (fn [db [_ running-subs]] (assoc db :flow-subs (vec (into running-subs (get db :flow-subs))))))

;; (re-frame/reg-event-db
;;  ::add-live-flow-subs
;;  (fn [db [_ running-subs]] 
;;    (assoc-in db [:click-param ])))

(re-frame/reg-event-db
  ::run-current-flowmap
  (fn [db _]
    (when (get db :flow?) ;; and has-done?
      (doall
        (let [flowmap              @(ut/tracked-subscribe [::flowmap])
              flow-id              @(ut/tracked-subscribe [::selected-flow])
              flowmaps-connections @(ut/tracked-subscribe [::flowmap-connections])
              client-name          @(ut/tracked-subscribe [::conn/client-name])
              opts-map             (get-in db [:flows flow-id :opts] {})
              server-flowmap       (process-flowmap2 flowmap flowmaps-connections flow-id)
              comps                (get server-flowmap :components) ;; (assoc (get
              running-view-subs    (vec (for [[k v] comps
                                              :when (get v :view)]
                                          [flow-id (keyword (str (ut/replacer (str k) ":" "") "-vw"))]))
              running-subs         (vec (for [k (keys comps)] [flow-id k]))
              running-subs         (vec (into running-subs running-view-subs))
              watched?             (ut/ne? (get @(ut/tracked-subscribe [::bricks/flow-watcher-subs-grouped]) flow-id))
              _ (ut/tapp>> [:comps comps running-subs watched?])
              fstr                 (str "running flow " flow-id)
              w                    (/ (count fstr) 4.1)]
          (reset! editor-tooltip-atom (str flow-id " is running"))
          (ut/tracked-dispatch [::http/set-flow-results {:status :started} nil flow-id])
          (swap! db/running-blocks assoc flow-id (vec (keys flowmap)))
          (ut/tapp>> [:flowmap-send-it flowmap server-flowmap running-subs])
          (when true
            (ut/tracked-dispatch [::add-live-flow-subs running-subs])
            (ut/tracked-dispatch
              [::wfx/request :default
               {:message {:kind :sub-to-running-values :flow-id flow-id :flow-keys running-subs :client-name client-name}
                :timeout 15000}]))
          (ut/tracked-dispatch
            [::wfx/request :default
             {:message     {:kind        :run-flow
                            :flow-id     flow-id
                            :no-return?  false ;true  ;false ;true ;;false ; true ;false ;true  ;; if we arent
                            :file-image  {:flowmaps             @(ut/tracked-subscribe [::flowmap-raw])
                                          :opts                 @(ut/tracked-subscribe [::opts-map])
                                          :zoom                 @db/pan-zoom-offsets
                                          :flow-id              flow-id
                                          :flowmaps-connections flowmaps-connections}
                            :opts        (merge {:increment-id? false :opts opts-map}
                                                (when (ut/ne? (get opts-map :overrides)) ;; per fab
                                                  {:overrides (get opts-map :overrides)}))
                            :flowmap     server-flowmap
                            :client-name client-name}
              :on-response [::socket-response-flow]
              :on-timeout  [::timeout-response :run-flow flowmap] ;; requeue?
              :timeout     150000000}])
          (ut/dispatch-delay 800 [::http/insert-alert fstr w 1 5])
          (ut/dissoc-in db [:flow-runner flow-id]))))))

(re-frame/reg-sub ::flow-runner
                  (fn [db [_ flow-id bid]]
                    (and (not (empty? (get @db/running-blocks flow-id [])))
                         (= (get-in db [:flow-runner flow-id bid] :started) :started))))




(defn is-running?
  [bid flow-id & [only?]]
  (let [;started? @(ut/tracked-subscribe [::flow-runner flow-id bid])
        chans-open?     @(ut/tracked-subscribe [::bricks/flow-channels-open? flow-id])
        server-running? (or (if only? false chans-open?)
                            @(ut/tracked-subscribe [::conn/clicked-parameter-key [(keyword (str "flow-status/" flow-id ">*running?"))]]))
        rrblocks        (get @db/real-running-blocks flow-id [])]
    
    (if (= bid :*) server-running? (and (some #(= bid %) rrblocks) server-running?))
    ;true
    ))





(re-frame/reg-sub ::is-running?
                  (fn [db [_ bid flow-id & [only?]]]
                    (let [;chans-open? @(ut/tracked-subscribe [::bricks/flow-channels-open? flow-id])
                          flow-running? @(ut/tracked-subscribe [::conn/clicked-parameter-key
                                                                [(keyword (str "flow-status/" flow-id ">*running?"))]])
                          running       (get-in db [:flow-results :tracker-blocks flow-id :running-blocks])]
                      (cond (= bid :*) flow-running?
                            :else      (true? (and flow-running? (some #(= bid %) running))))
                      ;true
                      )))

(re-frame/reg-sub ::is-waiting?
                  (fn [db [_ bid flow-id]]
                    (let [waiting       (get-in db [:flow-results :tracker-blocks flow-id :waiting-blocks])
                          flow-running? @(ut/tracked-subscribe [::conn/clicked-parameter-key
                                                                [(keyword (str "flow-status/" flow-id ">*running?"))]])]
                      (true? (and flow-running? (some #(= bid %) waiting))))))

(re-frame/reg-sub ::is-done?
                  (fn [db [_ bid flow-id]]
                    (let [done (get-in db [:flow-results :tracker-blocks flow-id :done-blocks])] (true? (some #(= bid %) done)))))

(re-frame/reg-sub ::is-error?
                  (fn [db [_ bid flow-id]]
                    (let [err (get-in db [:flow-results :tracker-blocks flow-id :error-blocks])] (true? (some #(= bid %) err)))))


(def tentacle-pos (reagent/atom [0 0]))
(def tentacle-start (reagent/atom [0 0]))





(re-frame/reg-sub ::flowmap
                  (fn [db _]
                    (let [;fmap @flowmaps
                          selected-flow (get db :selected-flow)
                          fmap          (get-in db [:flows selected-flow :map])
                          fmap          (into {} (for [[k v] fmap :when (get v :w)] {k v}))]
                      (process-flow-map fmap))))

(defn calc-block-widths-fn
  [inputs outputs w]
  (let [safe-w      70 ;(- w 50)
        px-per-char 7
        longesti    (apply max (map #(count (str %)) (conj (keys inputs) 10))) ;; 10 avoid empty
        longesti-w  (+ (* px-per-char longesti) 4)
        longesti-w  (if (< longesti-w safe-w) safe-w longesti-w)
        longesto    (try (apply max (map #(count (str %)) (conj (keys outputs) 10))) (catch :default _ 0))
        longesto-w  (+ (* px-per-char longesto) 4)
        longesto-w  (if (< longesto-w safe-w) safe-w longesto-w)
        both        (+ longesti-w longesto-w)
        cw          180 ;; forcing 180
        spacer      2
        w           (+ cw both spacer)]
    [w cw longesti-w longesto-w]))

(def calc-block-widths-atom (reagent/atom {}))

(defn calc-block-widths
  [inputs outputs w]
  (let [cache (get @calc-block-widths-atom [inputs outputs w])]
    (if cache
      cache
      (let [c (calc-block-widths-fn inputs outputs w)]
        (swap! calc-block-widths-atom assoc [inputs outputs w] c)
        c))))

(defn map-value-box
  [s k-val-type]
  (let [render-values? true
        function?      (or (fn? s) (try (fn? s) (catch :default _ false)))]
    (cond (ut/hex-color? s)                        [re-com/h-box :gap "3px" :children
                                                    [[re-com/box :child (str s)]
                                                     [re-com/box :child " " :width "13px" :height "13px" :style
                                                      {:background-color (str s) :border-radius "2px"}]]]
          (ut/is-base64? s)                        [re-com/v-box :size "auto" :gap "10px" :children
                                                    [[re-com/box :align :end :justify :end :style {:opacity 0.5} :child
                                                      (str "**huge base64 string: ~"
                                                           (.toFixed (/ (* (count s) 6) 8 1024 1024) 2)
                                                           " MB")]
                                                     [re-com/box :size "auto" :child
                                                      [:img {:src (str "data:image/png;base64," s)}]]]]
          (or function? (= k-val-type "function")) (str (.-name s) "!") ;; temp
          (string? s)                              [re-com/box :size "auto" :align :end :justify :end :style
                                                    {:word-break "break-all"} :child (str "\"" s "\"")]
          :else                                    (ut/replacer (str s) #"clojure.core/" ""))))

(declare draggable-port)

(defn draggable-pill [m _ element] (draggable-port element (get m :flow-id) (get m :from) m _ nil nil)) ;; stub

(defn sql-explanations-kp [] {[:from 0 0] "table-name" [:from 0 1] "table-alias" [:from] "the table we are selecting from"})


(defn map-boxes2
  [data block-id flow-name keypath kki init-data-type & [draggable?]]
  (let [;data (if (seq? data) data [data])
        sql-explanations (sql-explanations-kp)
        data (if (or (string? data) (number? data)) [data] data)
        base-type-vec? (or (vector? data) (list? data))
        iter (if base-type-vec? (range (count data)) (keys data))
        font-size "11px"
        add-kp? (try (keyword? block-id) (catch :default _ false)) ;;true ;(not
        cells? (= block-id :cells)
        main-boxes
          [re-com/v-box ;(if cells? re-com/h-box re-com/v-box)
           :size "auto" :padding "5px" :width (when cells? "280px") :gap "2px" :style
           {:color       "black"
            :font-family (theme-pull :theme/base-font nil)
            :font-size   font-size ; "11px"
            :font-weight 500} :children
           (for [kk iter] ;; (keys data)
             (let [k-val      (get-in data [kk])
                   k-val-type (ut/data-typer k-val)
                   in-body?   true ;(ut/contains-data? only-body k-val)
                   hovered?   false ;(ut/contains-data? mat-hovered-input k-val)
                   border-ind (if in-body? "solid" "dashed")
                   val-color  (get @(ut/tracked-subscribe [::conn/data-colors]) k-val-type)
                   keypath-in (conj keypath kk)
                   keystyle   {:background-color (if hovered? (str val-color 66) "#00000000")
                               :color            val-color
                               :border-radius    "12px"
                               :border           (str "3px " border-ind " " val-color)}
                   valstyle   {:background-color (if hovered? (str val-color 22) "#00000000") ;"#00000000"
                               :color            val-color
                               :border-radius    "12px"
                               :border           (str "3px " border-ind " " val-color 40)}]
               ^{:key (str block-id keypath kki kk)}
               [re-com/box :child
                (cond
                  (= k-val-type "map") ^{:key (str block-id keypath kki kk k-val-type)}
                                       [re-com/h-box :children
                                        [[draggable-pill
                                          {:from       block-id
                                           :new-block  [:artifacts "text"]
                                           :idx        0 ;idx
                                           :keypath-in keypath-in
                                           :flow-id    flow-name
                                           :keypath    [:map (if add-kp? (vec (cons :v keypath-in)) keypath-in)]} block-id 
                                          ^{:key (str block-id keypath kki kk k-val-type 1)}
                                          [re-com/v-box :min-width (px (* (count (str kk)) 11)) ;"110px"
                                           :style {:cursor (when draggable? "grab")} :children
                                           [^{:key (str block-id keypath kki kk k-val-type 124)} [re-com/box :child (str kk)]
                                            ^{:key (str block-id keypath kki kk k-val-type 134)}
                                            [re-com/box :child (str k-val-type) :style
                                             {:opacity     0.3
                                              :font-size   font-size ; "9px"
                                              :padding-top "7px"}]
                                            (when (> (count k-val) 1)
                                              ^{:key (str block-id keypath kki kk k-val-type 156)}
                                              [re-com/box :style {:opacity 0.3} :child (str "(" (count k-val) ")")])] :padding
                                           "8px"]] (map-boxes2 k-val block-id flow-name keypath-in kk nil draggable?)] :style
                                        keystyle]
                  (or (= k-val-type "vector")
                      (= k-val-type "list") ; (= k-val-type "function")
                      (= k-val-type "rowset")
                      (= k-val-type "jdbc-conn")
                      (= k-val-type "render-object"))
                    ^{:key (str block-id keypath kki kk k-val-type 2)}
                    [re-com/h-box :style {:border-radius "12px" :border "3px solid black"} :children
                     [[draggable-pill
                       {:from       block-id
                        :new-block  [:artifacts "text"]
                        :idx        0 ;idx
                        :keypath-in keypath-in
                        :flow-id    flow-name
                        :keypath    [:map (if add-kp? (vec (cons :v keypath-in)) keypath-in)]} block-id
                       ^{:key (str block-id keypath kki kk k-val-type 3)}
                       [re-com/v-box :min-width (px (* (count (str kk)) 11)) ;"110px"
                        :style
                        {:cursor (when draggable? "grab") ;;; skeptical, ryan 5/24/33
                        } :children
                        (if (and (= k-val-type "list") (= (ut/data-typer (first k-val)) "function"))
                          [^{:key (str block-id keypath kki kk k-val-type 4)}
                           [re-com/h-box :style {:margin-top "4px"} :gap "1px" :children
                            [;[re-com/box :child (str kk) :style {:opacity 0.25}]
                             [re-com/box :child "(" :style
                              {;:opacity 0.5
                               :color       "orange"
                               :margin-top  "-2px"
                               :font-size   "14px"
                               :font-weight 700}]
                             ^{:key (str block-id keypath kki kk k-val-type 5)}
                             [re-com/box :child (str (first k-val)) ; (str  "(")
                              :style
                              {:color     "#ffffff"
                               :font-size font-size ;"17px"
                              }]]]
                           ^{:key (str block-id keypath kki kk k-val-type 6)}
                           [re-com/box :child (str k-val-type) :style
                            {:opacity     0.3
                             :font-size   font-size ; "9px"
                             :padding-top "16px"}]
                           (when (> (count k-val) 1)
                             ^{:key (str block-id keypath kki kk k-val-type 827)}
                             [re-com/box :style {:opacity 0.3} :child (str "(" (count k-val) ")")])]
                          [(when true ;(not (get sql-explanations keypath ""))
                             ^{:key (str block-id keypath kki kk k-val-type 7)} [re-com/box :child (str kk)])
                           (when true ; (not (get sql-explanations keypath ""))
                             ^{:key (str block-id keypath kki kk k-val-type 8)}
                             [re-com/box :child (str (when (= (count k-val) 0) "empty ") k-val-type) :style
                              {:opacity     0.3
                               :font-size   font-size ; "9px"
                               :padding-top "7px"}])
                           (when (not (empty? (get sql-explanations keypath "")))
                             ^{:key (str block-id keypath kki kk k-val-type 82)}
                             [re-com/md-icon-button :md-icon-name "zmdi-info" :tooltip
                              (str "FOOO" (get sql-explanations keypath "")) :style
                              {:font-size "16px" :cursor "pointer" :opacity 0.5 :padding "0px" :margin-top "-1px"}])]) :padding
                        "8px"]]
                      [map-boxes2
                       (if (= k-val-type "rowset") (zipmap (iterate inc 0) (take 10 k-val)) (zipmap (iterate inc 0) k-val))
                       block-id flow-name keypath-in kk nil draggable?]] :style keystyle]
                  :else ^{:key (str block-id keypath kki kk k-val-type 9)}
                        [re-com/h-box :children
                         [^{:key (str block-id keypath kki kk k-val-type 10)}
                          [re-com/h-box :gap "6px" :children
                           [[draggable-pill
                             {:from       block-id
                              :new-block  [:artifacts "text"]
                              :idx        0 ;idx
                              :keypath-in keypath-in
                              :flow-id    flow-name
                              :keypath    [:map (if add-kp? (vec (cons :v keypath-in)) keypath-in)]} block-id
                             ^{:key (str block-id keypath kki kk k-val-type 11)}
                             [re-com/box :child (str kk) :style {:cursor (when draggable? "grab")}]]
                            (when true ;(not (get sql-explanations (vec (conj keypath kk)) ""))
                              ^{:key (str block-id keypath kki kk k-val-type 12)}
                              [re-com/box :child (str k-val-type) :style
                               {:opacity    0.3
                                :font-size  font-size ;"9px"
                                :font-style (if (= k-val-type "function") "italic" "normal")}])
                            (when (get sql-explanations (vec (conj keypath kk)) "")
                              ^{:key (str block-id keypath kki kk k-val-type 822)}
                              [re-com/box :style {:opacity 0.3} :child (str (get sql-explanations (vec (conj keypath kk)) ""))])]]
                          ^{:key (str block-id keypath kki kk k-val-type 13)}
                          [re-com/box :size "auto" :align :end :justify :end :child [map-value-box k-val] :style
                           {;:font-weight 500
                            :line-height  "1.2em"
                            :padding-left "5px"}]] :justify :between :padding "5px" :style valstyle])]))]]
    (if (= keypath [])
      (let [k-val-type (ut/data-typer data)
            nin?       (not (= block-id :inline-render))
            val-color  (get @(ut/tracked-subscribe [::conn/data-colors]) k-val-type)]
        [re-com/v-box :size "auto" :children
         [[re-com/v-box :style {:word-wrap "break-word" :overflow-wrap "break-word"} :children
           [(when nin?
              [re-com/v-box :padding "6px" :children
               [^{:key (str block-id keypath kki 00 k-val-type 00002)}
                [re-com/box :child (str flow-name) ;(str k-val-type)
                 :align :end :style
                 {:opacity       0.3
                  :font-size     font-size ;"9px"
                  :margin-top    "-7px"
                  :margin-bottom "-5px"}]]]) main-boxes] :padding "0px"]] :style
         {:font-family   (theme-pull :theme/base-font nil)
          :color         val-color
          :margin-top    (if nin? "0px" "-10px")
          :border-radius "12px"}])
      main-boxes)))

(defn coord-cacher333
  [conns flow-map]
  (let [cachekey (hash (str conns flow-map))
        cache    (get @ut/coord-cache cachekey)]
    (if false
      cache ;; cache cache ;; disable for now
      (let [coords (for [[o i] conns
                         :let  [ns-i?          (gns i)
                                ns-o?          (gns o)
                                bid-i          (if ns-i? (keyword (gns i)) i)
                                pid-i          (if ns-i? (keyword (gn i)) :in)
                                bid-o          (if ns-o? (keyword (gns o)) o)
                                pid-o          (if ns-o? (keyword (gn o)) :out)
                                ii             (get-in flow-map [bid-i :ports :in] {:in "unknown"})
                                oo             (get-in flow-map [bid-o :ports :out] {:out "unknown"})
                                [w cw iox iix] (calc-block-widths ii oo 0)
                                w              (- w 15) ;; spacer
                                ik             (keys ii)
                                ok             (keys oo)
                                [ix iy]        [(get-in flow-map [bid-i :x]) (get-in flow-map [bid-i :y])]
                                [ox oy ow]     [(get-in flow-map [bid-o :x]) (get-in flow-map [bid-o :y]) w]
                                >=zero         (fn [x] (if (< x 0) (- (count ok) 1) x)) ;; in case we
                                i-idx          (>=zero (.indexOf ik pid-i))
                                o-idx          (>=zero (.indexOf ok pid-o))
                                y-offset       44
                                port-size      17
                                x1             (+ ox w -25)
                                y1             (+ (+ 3 oy (* port-size o-idx)) y-offset)
                                x2             (+ ix 11)
                                y2             (+ (+ 3 iy (* port-size i-idx)) y-offset)
                                dcolor         (get (theme-pull :theme/data-colors db/data-colors)
                                                    (gn (get-in flow-map [bid-o :ports :out pid-o]))
                                                    (get (theme-pull :theme/data-colors db/data-colors) "unknown" "#FFA500"))]]
                     [x1 y1 x2 y2 true dcolor bid-o bid-i])]
        coords))))

(defn coord-cacher
  [conns flow-map]
  (let [results-hash (hash (get @(ut/tracked-subscribe [::http/flow-results]) @(ut/tracked-subscribe [::selected-flow])))
        cachekey     (str (hash (str conns flow-map)) "." results-hash)
        flow-id      @(ut/tracked-subscribe [::selected-flow])
        react!       [@(ut/tracked-subscribe [::http/flow-results])]
        cache        (get @ut/coord-cache cachekey)]
    (if false
      cache ;; disable for now
      (let [coords (for [[o i] conns
                         :let  [ns-i?         (gns i)
                                ns-o?         (gns o)
                                bid-i         (if ns-i? (keyword (gns i)) i)
                                pid-i         (if ns-i? (keyword (gn i)) :in)
                                bid-o         (if ns-o? (keyword (gns o)) o)
                                pid-o         (if ns-o? (keyword (gn o)) :out)
                                ii            (get-in flow-map [bid-i :ports :in] {:in "unknown"})
                                oop           (get-in flow-map [bid-o :cond] {}) ;; conditional pathing
                                oopp          (get-in flow-map [bid-o :push] {}) ;; push pathing
                                ooo           (get-in flow-map [bid-o :ports :out] {:out "unknown"})
                                oo            (merge ooo oop)
                                ik            (keys ii)
                                ok            (try (into (into (vec (sort-by str (keys ooo))) (vec (keys oop))) (vec (keys oopp)))
                                                   (catch :default e
                                                     (do (ut/tapp>> [:error-sorting-keys e (keys oo) :flows.cljs :ln 1148])
                                                         (into (into (vec (sort-by str (keys ooo))) (vec (keys oop)))
                                                               (vec (keys oopp))))))
                                [ix iy iw]    [(get-in flow-map [bid-i :x]) (get-in flow-map [bid-i :y])
                                               (get-in flow-map [bid-i :w])]
                                [ox oy ow oh] [(get-in flow-map [bid-o :x]) (get-in flow-map [bid-o :y])
                                               (get-in flow-map [bid-o :w]) (- (get-in flow-map [bid-o :h]) 3)]
                                >=zero        (fn [x] (if (< x 0) (- (count ok) 1) x)) ;; in case we
                                i-idx         (try (>=zero (.indexOf ik pid-i)) (catch :default _ -1))
                                o-idx         (try (>=zero (.indexOf ok pid-o)) (catch :default _ -1))
                                y-offset      0 ;44
                                iport-size    (/ (- iw 4) (count ik))
                                oport-size    (/ (- ow 4) (count ok))
                                x1            (+ ox (* o-idx oport-size) 10.5)
                                y1            (+ (+ 3 oy) oh -6.5)
                                x2            (+ ix (* i-idx iport-size) 10.5)
                                y2            (+ (+ 3 iy) 3)
                                dcolor        @(ut/tracked-subscribe [::port-color flow-id bid-o pid-o])]]
                     [x1 y1 x2 y2 true dcolor bid-o bid-i pid-o pid-i])]
        coords))))

(defn translate-x-y
  [[x y]]
  (let [zoom-multi    (get @db/pan-zoom-offsets 2)
        zoom-offset-x (get @db/pan-zoom-offsets 0)
        zoom-offset-y (get @db/pan-zoom-offsets 1)
        drop-x        (- (/ x zoom-multi) (/ zoom-offset-x zoom-multi))
        drop-y        (- (/ y zoom-multi) (/ zoom-offset-y zoom-multi))]
    [drop-x drop-y]))

(defn generate-tentacle-coord22
  []
  (let [[x2 y2] (translate-x-y @tentacle-pos)
        [x1 y1] (translate-x-y @tentacle-start)
        [xx yy] @db/flow-detached-coords
        dcolor  (get (theme-pull :theme/data-colors db/data-colors)
                     (gn (last @dragged-port))
                     (get (theme-pull :theme/data-colors db/data-colors) "unknown" "#FFA500"))
        row     [x1 (+ y1 11) (- x2 xx) (- y2 yy) false dcolor nil nil]]
    row))

(defn generate-tentacle-coord
  []
  (let [[x2 y2] @tentacle-pos
        [x1 y1] @tentacle-start
        [xx yy] @db/flow-detached-coords
        dcolor  (get (theme-pull :theme/data-colors db/data-colors)
                     (gn (last @dragged-port))
                     (get (theme-pull :theme/data-colors db/data-colors) "unknown" "#FFA500"))
        row     [x1 (+ y1 11) (- x2 xx 8) (- y2 yy 25) false dcolor nil nil]]
    row))

(defn generate-coords
  [xx yy]
  (let [conns    (vec (filter #(not (= (last %) :done)) @(ut/tracked-subscribe [::flowmap-connections])))
        flow-map @(ut/tracked-subscribe [::flowmap])
        fconns   (coord-cacher conns flow-map)]
    fconns))

(defn draw-tentacle
  [coords]
  (doall (for [[x1 y1 x2 y2 involved? color z1 z2] coords]
           ^{:key (hash (str x1 y1 x2 y2 "lines-flow"))}
           (let [zoom-multi (get @db/pan-zoom-offsets 2)] ;;[flow-id @(ut/tracked-subscribe
             [:path
              {:stroke-width     (* 8 zoom-multi) ;(if involved? 16 13)
               :stroke           color ;;(if involved? color "#ffffff22") ;(if (or involved?
               :stroke-dasharray (str (* 10 zoom-multi) " " (* 8 zoom-multi))
               :fill             "none" ;;(str color 45) ;; could be a cool "running effect"
               :filter           "drop-shadow(0.25rem 0.35rem 0.4rem rgba(0, 0, 0, 0.44))"
               :d                (ut/curved-path-v x1 y1 x2 y2)}]))))


(defn draw-lines
  [coords]
  (doall
    (for [[x1 y1 x2 y2 involved? color z1 z2 z1a z2a] coords]
      ^{:key (hash (str x1 y1 x2 y2 "lines-flow"))}
      (let [flow-id       @(ut/tracked-subscribe [::selected-flow])
            flow-select   @(ut/tracked-subscribe [::selected-flow-block])
            selected-i    (get-in @flow-details-block-container-atom [flow-id flow-select :port-panel :in])
            selected-o    (get-in @flow-details-block-container-atom [flow-id flow-select :port-panel :out])
            not-selected? (true? (and (nil? selected-i) (nil? selected-o)))
            selected?     (true?
                            (or (= selected-i [z2 z2a]) (= selected-o [z1 z1a]) (= selected-i [z1 z1a]) (= selected-o [z2 z2a])))
            condi-path?   (cstr/starts-with? (str z1a) ":cond-path")
            push-path?    (cstr/starts-with? (str z1a) ":push-path")
            uid-hash      (hash [x1 y1 x2 y2 involved? color z1 z2 z1a z2a])
            react!        [@db/running-blocks]
            flow-running? @(ut/tracked-subscribe [::is-running? :* flow-id])
            running?      (and flow-running? @(ut/tracked-subscribe [::is-running? z2 flow-id]))
            waiting?      (and flow-running? @(ut/tracked-subscribe [::is-waiting? z2 flow-id]))
            error?        @(ut/tracked-subscribe [::is-error? z2 flow-id])
            opacity       1.0 ;(if (or running? selected?) 1.0 0.5)
            done?         (and flow-running? @(ut/tracked-subscribe [::is-done? z2 flow-id]))]
        ^{:key (str uid-hash)}
        [:path
         {:stroke-width     (if (or done? running?) 10 7) ;(if selected? 11 6) ;(if involved?
          :stroke           (if (or selected? involved?) color (str color 75)) ;(if (or involved?
          :opacity          (cond running?                                         1.0
                                  waiting?                                         0.3
                                  error?                                           0.25
                                  done?                                            1.0
                                  (and (not running?) (or push-path? condi-path?)) (- opacity 0.22)
                                  :else                                            opacity)
          :class            (when running? "flow-right") ;;(some #(= z1 %) @db/running-blocks)
          :stroke-dasharray (cond running?             "4,12"
                                  (or error? waiting?) "10,15"
                                  condi-path?          "15,10"
                                  push-path?           "5,5"
                                  :else                nil)
          :stroke-linecap   (cond condi-path? "butt"
                                  push-path?  "butt"
                                  :else       "round")
          :fill             "none" ;;(str color 45) ;; could be a cool "running effect"
          :filter           (if (or running? selected?)
                              (str "brightness(200%) drop-shadow(2px 1px 4px " color ")")
                              (str "drop-shadow(2px 1px 4px #000000)"))
          :d                (ut/curved-path-v x1 y1 x2 y2)}]))))



(defn get-client-rect
  [evt]
  (let [r (.getBoundingClientRect (.-target evt))]
    {:left (.-left r) :top (.-top r) :width (.-width r) :height (.-height r) :bottom (.-bottom r) :right (.-right r)}))


(re-frame/reg-event-db ::update-flowmap-key
                       (undoable)
                       (fn [db [_ bid kkey vval]]
                         (let [bid (get db :selected-flow-block)] ;; override due to slow reacting?
                           (if (nil? kkey) ;; fulll body update
                             (assoc-in db [:flows (get db :selected-flow) :map bid] vval)
                             (assoc-in db [:flows (get db :selected-flow) :map bid kkey] vval)))))

(re-frame/reg-event-db
  ::update-flowmap-key-in
  (undoable)
  (fn [db [_ bid kkey vval]]
    (ut/tapp>> [:update bid (get db :selected-flow-block) kkey vval])
    (let [bid       (get db :selected-flow-block)
          pin?      (= kkey [:ports :in]) ;; ut/function-to-inputs :args+ overrides
          arg-walks (if pin?
                      (into {}
                            (for [e     (keys vval)
                                  :when (cstr/ends-with? (str e) "+")]
                              {(-> (str e)
                                   (ut/replacer "+" "")
                                   (ut/replacer ":" "")
                                   keyword)
                                 e}))
                      {})]
      (if (nil? kkey) ;; fulll body update
        (assoc-in db [:flows (get db :selected-flow) :map bid] vval)
        (if pin?
          (-> db
              (assoc-in (vec (into [:flows (get db :selected-flow) :map bid] kkey)) vval)
              (assoc-in [:flows (get db :selected-flow) :map bid :types]
                        (ut/postwalk-replacer arg-walks (get-in db [:flows (get db :selected-flow) :map bid :types])))
              (assoc-in [:flows (get db :selected-flow) :map bid :inputs]
                        (ut/postwalk-replacer arg-walks (get-in db [:flows (get db :selected-flow) :map bid :inputs]))))
          (assoc-in db (vec (into [:flows (get db :selected-flow) :map bid] kkey)) vval))))))


(re-frame/reg-event-db ::update-flowmap-key-others
                       (undoable)
                       (fn [db [_ bid fid kkey vval]]
                         (if (nil? kkey) ;; fulll body update
                           (assoc-in db [:flows fid :map bid] vval)
                           (assoc-in db [:flows fid :map bid kkey] vval))))

(re-frame/reg-event-db ::remove-flowmap-bid (fn [db [_ bid]] (ut/dissoc-in db [:flows (get db :selected-flow) :map bid])))

(re-frame/reg-event-db ::update-connections
                       (fn [db [_ new-conns]] (assoc-in db [:flows (get db :selected-flow) :connections] new-conns)))

(defn sort-map-keys
  [ordered-keys unsorted-map]
  (let [all-keys       (keys unsorted-map)
        remaining-keys (filter #(not (contains? (set ordered-keys) %)) all-keys)
        ordered-vec    (into []
                             (concat (map (fn [k] [k (unsorted-map k)]) ordered-keys)
                                     (map (fn [k] [k (unsorted-map k)]) remaining-keys)))]
    (apply array-map (apply concat ordered-vec))))

(re-frame/reg-event-db
  ::add-port
  (fn [db [_ bid direction]]
    (let [ports         (get-in db [:flows (get db :selected-flow) :map bid :ports direction] {})
          label         (str (get-in db [:flows (get db :selected-flow) :map bid :data :flow-item :pill-name] "value"))
          cnt           (count (keys ports))
          inputs        (try (edn/read-string (get-in db [:flows (get db :selected-flow) :map bid :data :flow-item :inputs] []))
                             (catch :default _ []))
          new-key       (ut/safe-key (keyword (str label "-" cnt)) (keys ports))
          updated-ports (assoc ports new-key :any)
          sorted-ports  (sort-map-keys inputs updated-ports)]
      (-> db
          (assoc-in [:flows (get db :selected-flow) :map bid :ports direction] sorted-ports)))))

(re-frame/reg-event-db
  ::delete-port
  (fn [db [_ bid pid direction]]
    (let [ports        (get-in db [:flows (get db :selected-flow) :map bid :ports direction] {})
          inputs       (try (edn/read-string (get-in db [:flows (get db :selected-flow) :map bid :data :flow-item :inputs] []))
                            (catch :default _ []))
          new-ports    (dissoc ports pid)
          sorted-ports (sort-map-keys inputs new-ports)]
      (-> db
          (assoc-in [:flows (get db :selected-flow) :map bid :ports direction] sorted-ports)))))


(defn zoom-to-element
  [element-id padding]
  (let [element (js/document.getElementById element-id)] (when-let [z @db/zoomer] (.zoomToElement z element padding))))


(defn reset-zoom-and-pan [] (when-let [z @db/zoomer] (.resetTransform z)))

(defn center-zoom-and-pan [] (when-let [z @db/zoomer] (.centerView z)))

(defn set-zoom-pan [[positionX positionY scale]] (when-let [z @db/zoomer] (.setTransform z positionX positionY scale 0)))

(re-frame/reg-sub ::zoom-start (fn [db _] (get db :zoom-start)))

(re-frame/reg-sub
  ::get-flow-brick
  (fn [db _]
    (let [ordered1 (reverse (sort-by last (for [[k v] (get-in db [:flows (get db :selected-flow) :map])] [k (get v :x)])))]
      (str "flow-brick-" (first (first ordered1))))))

(defn center-to-saved-coords
  []
  (when-let [z @db/zoomer]
    (let [[positionX positionY scale] @(ut/tracked-subscribe [::zoom-start])]
      (ut/tapp>> [positionX positionY scale])
      (js/setTimeout (fn [] (.zoomToElement z @(ut/tracked-subscribe [::get-flow-brick]) 0.8)) 1000))))


(re-frame/reg-event-db ::select-block (undoable) (fn [db [_ bid]] (assoc db :selected-flow-block bid)))

(re-frame/reg-event-db ::select-flow (fn [db [_ bid]] (assoc db :selected-flow bid)))

(re-frame/reg-event-db ::update-flowmap-connections
                       (undoable)
                       (fn [db [_ vval]] (assoc-in db [:flows (get db :selected-flow) :connections] vval)))

(re-frame/reg-event-db ::update-flowmap-coords
                       (fn [db [_ bid x y]]
                         (-> db
                             (assoc-in [:flows (get db :selected-flow) :map bid :x] x)
                             (assoc-in [:flows (get db :selected-flow) :map bid :y] y))))



(defn mouse-move-handler-block
  [offset bid]
  (let [last-coords (atom nil)]
    (fn [evt]
      (let [raw-x         (.-clientX evt)
            raw-y         (.-clientY evt)
            [xx yy]       @db/flow-detached-coords
            zoom-multi    (get @db/pan-zoom-offsets 2)
            bx            (* 3 zoom-multi) ;; border offset of container
            by            (* 18.25 zoom-multi) ;; border offset of container
            raw-x         (- raw-x bx)
            raw-y         (- raw-y by)
            zoom-offset-x (get @db/pan-zoom-offsets 0)
            zoom-offset-y (get @db/pan-zoom-offsets 1)
            off-x         (/ (+ (:x offset) xx) zoom-multi)
            off-y         (/ (+ (:y offset) yy) zoom-multi)
            x             (- (- (/ raw-x zoom-multi) (/ zoom-offset-x zoom-multi)) off-x)
            y             (- (- (/ raw-y zoom-multi) (/ zoom-offset-y zoom-multi)) off-y)
            grid-size     db/snap-to-grid ;20
            snapped-x     (* grid-size (Math/round (/ x grid-size)))
            snapped-y     (* grid-size (Math/round (/ y grid-size)))
            bounded-x     (min (max snapped-x 0) (- canvas-width grid-size))
            bounded-y     (min (max snapped-y 0) (- canvas-height grid-size))]
        (when (or (not= bounded-x (first @last-coords)) (not= bounded-y (second @last-coords)))
          (reset! last-coords [bounded-x bounded-y])
          (ut/tracked-dispatch-sync [::update-flowmap-coords bid bounded-x bounded-y]))))))



(defn tag-tentacle-pos
  [evt]
  (let [raw-x         (.-clientX evt)
        raw-y         (.-clientY evt)
        [xx yy]       @db/flow-detached-coords
        offset        {:x 0 :y 0}
        zoom-multi    (get @db/pan-zoom-offsets 2)
        bx            0 ;(* 3 zoom-multi) ;; border offset of container
        by            0 ;(* 18.25 zoom-multi) ;; border offset of container
        raw-x         (- raw-x bx)
        raw-y         (- raw-y by)
        zoom-offset-x (get @db/pan-zoom-offsets 0)
        zoom-offset-y (get @db/pan-zoom-offsets 1)
        off-x         (/ (+ (:x offset) xx) zoom-multi)
        off-y         (/ (+ (:y offset) yy) zoom-multi)
        x             (- (- (/ raw-x zoom-multi) (/ zoom-offset-x zoom-multi)) off-x)
        y             (- (- (/ raw-y zoom-multi) (/ zoom-offset-y zoom-multi)) off-y)]
    (reset! tentacle-pos [x y])))

(defn tag-screen-position
  [evt]
  (let [;[xx yy] @db/flow-detached-coords
        raw-x (.-clientX evt)
        raw-y (.-clientY evt)]
    (reset! tentacle-pos [raw-x raw-y])))

(def mouse-dragging-panel? (reagent/atom false))

(re-frame/reg-event-db ::resize-block
                       (fn [db [_ bid w h]]
                         (let [ff (get db :selected-flow)]
                           (-> db
                               (assoc-in [:flows ff :map bid :h] h)
                               (assoc-in [:flows ff :map bid :w] w)))))

(defn mouse-up-handler-block
  [on-move]
  (fn me [evt]
    (do ;(reset! mouse-dragging-panel? false)
      (reset! dragging? false)
      (reset! mouse-dragging-panel? false)
      (reset! dragging-flow? false))
    (gevents/unlisten js/window EventType.MOUSEMOVE on-move)))

(defn debounce
  [f interval]
  (let [timer (atom nil)]
    (fn [& args]
      (when-let [prev-timer @timer] (js/clearTimeout prev-timer))
      (reset! timer (js/setTimeout (fn [] (apply f args)) interval)))))

(defn mouse-down-handler-block
  [e bid]
  (let [{:keys [left top]} (get-client-rect e)
        offset             {:x (- (.-clientX e) left) :y (- (.-clientY e) top -25)}
        _ (reset! dragging-flow? true)
        on-move            (mouse-move-handler-block offset bid)]
    (do (reset! dragging? true) (gevents/listen js/window EventType.MOUSEMOVE on-move))
    (gevents/listen js/window EventType.MOUSEUP (mouse-up-handler-block on-move))))

(re-frame/reg-sub ::size
                  (fn [db [_ bid]]
                    (let [selected-flow (get db :selected-flow)
                          fmap          (get-in db [:flows selected-flow :map])]
                      [(get-in fmap [bid :w]) (get-in fmap [bid :h])])))




(defn resize-mouse-move-handler
  [offset sw sh sx sy bid]
  (let [last-size (atom [sw sh])]
    (fn [evt]
      (let [[min-w min-h] [100 60] ;@(ut/tracked-subscribe [::min-size block-id])
            is-mask?      false ;(= block-type "mask")
            is-square?    false ;(= block-type "image")
            zoom-multi    (get @db/pan-zoom-offsets 2)
            zoom-offset-x (get @db/pan-zoom-offsets 0)
            zoom-offset-y (get @db/pan-zoom-offsets 1)
            raw-x         (.-clientX evt)
            raw-y         (.-clientY evt)
            bx            0 ;(* 3 zoom-multi) ;; border offset of container
            by            0 ;(* 18.25 zoom-multi) ;; border offset of container
            raw-x         (- raw-x bx)
            raw-y         (- raw-y by)
            pos-x         (- (/ raw-x zoom-multi) (/ zoom-offset-x zoom-multi))
            pos-y         (- (/ raw-y zoom-multi) (/ zoom-offset-y zoom-multi))
            sx            (- (/ sx zoom-multi) (/ zoom-offset-x zoom-multi))
            sy            (- (/ sy zoom-multi) (/ zoom-offset-y zoom-multi))
            w             (+ sw (- pos-x sx))
            h             (+ sh (- pos-y sy))
            grid-size     db/snap-to-grid ;20
            snapped-w     (* grid-size (Math/round (/ w grid-size)))
            snapped-h     (* grid-size (Math/round (/ h grid-size)))
            wmax          (if (< snapped-w min-w) min-w snapped-w)
            hmax          (cond is-mask?   (+ (if (< snapped-w min-w) min-w snapped-w) 0)
                                is-square? (+ (if (< snapped-w min-w) min-w snapped-w) 0)
                                :else      (if (< snapped-h min-h) min-h snapped-h))]
        (when (or (not= hmax (second @last-size)) (not= wmax (first @last-size)))
          (reset! last-size [wmax hmax])
          (ut/tracked-dispatch [::resize-block bid wmax hmax]))))))

(defn resize-mouse-down-handler
  [e bid]
  (let [[w h]   @(ut/tracked-subscribe [::size bid])
        offset  {:x (- (.-clientX e) w) :y (- (.-clientY e) h)}
        on-move (resize-mouse-move-handler offset w h (.-clientX e) (.-clientY e) bid)]
    (do (reset! mouse-dragging-panel? true) (reset! flow-hover bid) (gevents/listen js/window EventType.MOUSEMOVE on-move))
    (gevents/listen js/window EventType.MOUSEUP (mouse-up-handler on-move))))

(defn draggable-port
  [element flow-id bid pid ttype xs ys]
  [(reagent/adapt-react-class rdnd/Draggable)
   (let [browser-pull? (true? (map? pid))
         keypath       (if browser-pull? (get pid :keypath-in) nil)
         input-pid     (if browser-pull? (get pid :input-pid) nil)
         port-meta     (if browser-pull? (get pid :port-meta) nil)
         pid           (if browser-pull? :* pid)
         data          (let [subkey      (str flow-id ">" (ut/replacer (str bid) #":" ""))
                             param-full  (keyword (str "flow/" subkey)) ;:flow/map-pull-test2>open-fn-6
                             param-field (keyword subkey)]
                         {:h             3
                          :w             7
                          :root          [0 0]
                          :browser-pull? browser-pull?
                          :drag-meta     {:type        :param
                                          :param-full  param-full
                                          :port-meta   port-meta
                                          :keypath     keypath
                                          :src-bid     bid
                                          :src-pid     (if input-pid input-pid pid)
                                          :param-table :flow
                                          :param-field param-field}})
         [xx yy]       @db/flow-detached-coords]
     {:type          :flow-port
      :on-drag       (when (not browser-pull?) tag-screen-position)
      :on-drag-end   #(do (reset! dragging-port? false))
      :on-drag-start #(do (reset! dragging-port? true)
                          (reset! dragged-port [bid pid ttype])
                          (reset! tentacle-start [(- (.-clientX %) xx 8) (- (.-clientY %) yy 40)])
                          (reset! tentacle-pos [0 0])
                          (reset! bricks/dragging-size [(get data :w) (get data :h)])
                          (reset! bricks/dragging-body data))
      :data          (pr-str data)}) ;[bid pid]
   [re-com/box :size "auto" :child element :style {:cursor (if @dragging-port? "grabbing" "grab")}]])

(defn draggable-play
  [element flow-id]
  [(reagent/adapt-react-class rdnd/Draggable)
   (let [data (let [] ;subkey (str flow-id ">" (ut/replacer (str bid) #":" ""))
                {:h         2
                 :w         7
                 :drag-meta {:source-table :hi :table-fields [:*] :connection-id nil :source-panel-key :block-7034 :type :view}
                 :views     {:flow-play [:box :align :center :justify :center :style
                                         {:font-size    "25px"
                                          :font-weight  700
                                          :padding-top  "6px"
                                          :padding-left "14px"
                                          :margin-top   "-8px"
                                          :color        :theme/editor-outer-rim-color
                                          :font-family  :theme/base-font} :child
                                         [:run-flow [(str flow-id) (str "run " flow-id)]]]}
                 :name      "flow-play"})]
     {:type          :flow-port
      :on-drag-end   #(do (reset! dragging-port? false))
      :on-drag-start #(do (reset! dragging-port? true)
                          (reset! bricks/dragging-size [(get data :w) (get data :h)])
                          (reset! bricks/dragging-body data))
      :data          (pr-str data)}) ;[bid pid]
   [re-com/box :size "none" :child element :style {:cursor (if @dragging-port? "grabbing" "grab")}]])





(defn ifnil [x n] (if (nil? x) n x))

(defn remove-connections
  [bid pid input?]
  (let [flowmaps-connections @(ut/tracked-subscribe [::flowmap-connections])
        side-fn              (if input? last first)
        conns-count          (count (filter #(or (= (side-fn %) (keyword (str (gn bid) "/" (gn pid))))
                                                 (if (or (= pid :in) (= pid :out)) (= (side-fn %) bid) false)) ;; unnamed ports
                                                                                                               ;; assumed
                                      flowmaps-connections))
        new-connections      (remove #(or (= (side-fn %) (keyword (str (gn bid) "/" (gn pid))))
                                          (if (or (= pid :in) (= pid :out)) (= (side-fn %) bid) false)) ;; unnamed ports
                                                                                                        ;; assumed
                               flowmaps-connections)]
    (ut/tapp>> [:removing bid pid (if input? :in :out) conns-count])
    (ut/tracked-dispatch [::update-flowmap-connections new-connections])
    conns-count))





(defn remove-block
  [bid]
  (let [flowmaps-connections @(ut/tracked-subscribe [::flowmap-connections])
        new-connections      (remove #(or (= (first %) bid)
                                          (= (last %) bid)
                                          (cstr/starts-with? (str (first %)) (str bid "/"))
                                          (cstr/starts-with? (str (last %)) (str bid "/")))
                               flowmaps-connections)]
    (ut/tapp>> [:removing-block bid])
    (reset! flow-hover nil)
    (reset! port-hover2 nil)
    (ut/tracked-dispatch [::select-block nil])
    (ut/tracked-dispatch [::remove-flowmap-bid bid])
    (ut/tracked-dispatch [::update-flowmap-connections new-connections])))

(re-frame/reg-sub ::selected-flow-block (fn [db] (get db :selected-flow-block)))

(re-frame/reg-event-db ::flow-block-meta
                       (fn [db [_ bid key value]] (assoc-in db [:flows (get db :selected-flow) :map bid :data key] value)))

(defn select-block [bid] (ut/tapp>> [:selecting-block2 bid]) (ut/tracked-dispatch [::select-block bid]))

(defn connect-ports
  [src dest]
  (ut/tapp>> [:connecting src dest])
  (let [flowmaps-connections @(ut/tracked-subscribe [::flowmap-connections])
        conns                (vec (filter #(not (= (last %) :done)) flowmaps-connections))
        src                  (if (cstr/ends-with? (str src) "/*") ;; no need for *, will only
                               (keyword (first (-> (str src)
                                                   (ut/replacer #":" "")
                                                   (ut/splitter #"/"))))
                               src)
        done?                (= dest :done)
        conn-vec             (vec (distinct (conj (if done? conns flowmaps-connections)
                                                  [src ;(if done? base-src src)
                                                   dest])))]
    (ut/tapp>> [:new-conn-vec conn-vec])
    (ut/tracked-dispatch [::update-flowmap-connections conn-vec])))

(defn render-icon
  [icon bcolor & [size fsize]]
  (let [size  (or size 20)
        fsize (or fsize 14)]
    (if (not (nil? icon))
      (if (not (cstr/starts-with? (str icon) "http")) ;(cstr/includes? icon "zmdi")
        [re-com/md-icon-button :src (at) :md-icon-name icon :style {:color bcolor :font-size (px fsize)} :attr {}]
        [re-com/box :style {:margin-left "-10px"} :size "none" :width (px size) :child [:img {:src icon :width "100%"}]])
      " ")))

(def ports-react-render (reagent/atom nil))

(def auto-font-atom (atom {}))

(defn auto-font-size-px-fn
  [value h w]
  (let [brick-size    2.22
        md            (ut/data-typer value) ;(get-in metadata [:fields label :data-type])
        num?          (or (= md "integer") (= md "float"))
        formatted-val (if num? (str (ut/nf value)) (str value))
        len           (count (str formatted-val))
        charw         (js/Math.ceil (/ (* (js/Math.ceil (/ (+ h w 1) 2)) brick-size) len))]
    charw))

(defn auto-font-size-px
  [value h w]
  (let [hx    [value h w]
        cache (get @auto-font-atom hx)]
    (if cache
      cache
      (let [deep (auto-font-size-px-fn value h w)]
        (swap! auto-font-atom assoc hx deep)
        deep))))

(defn droppable-port
  [element bid pid]
  [(reagent/adapt-react-class rdnd/Droppable)
   {:types   [:flow-port]
    :on-drop #(let [_ (ut/tapp>> [:dropped-on-port bid pid @dragged-port])
                    [srcf srcl _] @dragged-port
                    src           (if (= srcl :out)
                                    srcf ;; no alias for simple */out
                                    (keyword (str (gn srcf) "/" (gn srcl))))
                    dest          (if (= pid :in) bid (keyword (str (gn bid) "/" (gn pid))))]
                (reset! dragging-port? false)
                (ut/tapp>> [:dropped src :to dest])
                (connect-ports src dest))} element])

(defn valid-drop-ports
  [inputs] ;; @dragging-port?=
  (let [filtered-inputs (filter #(or (try (some (fn [x] (= x (last @dragged-port))) (val %)) (catch :default _ false))
                                     (= (val %)
                                        (try (last @dragged-port) ;; in case of nil, etc
                                             (catch :default _ nil)))
                                     (= (val %) :any))
                          inputs)]
    (if (= :any (last @dragged-port))
      (keys inputs) ;; if we dont know what it is (like a fresh open-fn) just allow whatever
      (keys filtered-inputs))))

(defn code-box
  [width-int height-int value & [syntax]]
  (let [syntax (or (if (= syntax "raw (clojure)") "clojure" syntax) "clojure")]
    [re-com/box :size "none" :width (px (- width-int 24)) :height (px (- height-int 24)) :style
     {:font-family   (theme-pull :theme/monospaced-font nil) ; "Chivo Mono" ;"Fira Code"
      :font-size     "14px"
      :overflow      "auto"
      :border-radius "12px"
      :font-weight   700} :child
     [(reagent/adapt-react-class cm/UnControlled)
      {:value   (ut/format-map (- width-int 24) (str value))
       :options {:mode              syntax ; "clojure"
                 :lineWrapping      true
                 :lineNumbers       false ;true
                 :matchBrackets     true
                 :autoCloseBrackets true
                 :autofocus         false
                 :autoScroll        false
                 :detach            true
                 :readOnly          true ;true
                 :theme             (theme-pull :theme/codemirror-theme nil)}}]]))

(re-frame/reg-event-db ::set-flow-opts (fn [db [_ opts-map]] (assoc-in db [:flows (get db :selected-flow) :opts] opts-map)))

(defn code-box-options
  [flow-id width-int height-int value]
  (let [scrubber? (get-in @flow-details-block-container-atom [flow-id :opts-scrubber?] false)
        react!    [@flow-details-block-container-atom]] ;; just in case
    [re-com/v-box :size "none" :width (px (- width-int 24)) :height (px (- height-int 24)) :style
     {:font-family   (theme-pull :theme/monospaced-font nil) ; "Chivo Mono" ;"Fira Code"
      :font-size     "14px"
      :overflow      "auto"
      :border-radius "12px"
      :font-weight   700} :children
     [(if scrubber?
        [bricks/scrubber-panel true ; view?
         (let [oo @(ut/tracked-subscribe [::bricks/keypaths-in-flow-opts])
               oo (if (empty? oo) ;; grab defaults if somehow they havent been written in yet
                    @(ut/tracked-subscribe [::opts-map])
                    oo)]
           oo) [:opts flow-id] ;nil ;key-type
         nil ;(get @param-search key-type)
         {:fm true :opts? true}]
        [(reagent/adapt-react-class cm/UnControlled)
         {:value   (ut/format-map (- width-int 24) (str value))
          :onBlur  #(let [opts (try (read-string (cstr/join " " (ut/cm-deep-values %))) (catch :default _ :err))]
                      (when (map? opts) (ut/tracked-dispatch [::set-flow-opts opts])))
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
      [re-com/box :width "100%" :align :end :attr
       {:on-click #(swap! flow-details-block-container-atom assoc-in [flow-id :opts-scrubber?] (not scrubber?))} :style
       {:font-size "11px" :padding-left "10px" :cursor "pointer" :color (str (theme-pull :theme/editor-outer-rim-color nil) 98)}
       :child (str "scrubber " (if scrubber? "on" "off"))]]]))

(defn code-box-smol
  [width-int height-int value & [syntax]]
  (let [syntax     (or (if (= syntax "raw (clojure)") "clojure" syntax) "clojure")
        stringify? (not (= syntax "clojure"))]
    [re-com/box :size "none" :width (px (- width-int 24)) :style
     {:font-family (theme-pull :theme/monospaced-font nil) ; "Chivo Mono" ;"Fira Code"
      :font-size   "6px"
      :overflow    "hidden" ;"auto"
      :font-weight 700} :child
     [(reagent/adapt-react-class cm/UnControlled)
      {:value   (if stringify?
                  (str (edn/read-string value)) ; pr-str double quoted it, so we have to read
                  (ut/format-map width-int (str value)))
       :options {:mode              syntax ; "clojure"
                 :lineWrapping      true ;false
                 :lineNumbers       false
                 :matchBrackets     true
                 :autoCloseBrackets true
                 :autofocus         false
                 :autoScroll        false
                 :detach            true
                 :readOnly          true ;true
                 :theme             (theme-pull :theme/codemirror-theme nil)}}]]))

(defn swap-delay!
  [atom f value delay-ms]
  (swap! atom f value)
  (async/go (async/<! (async/timeout delay-ms)) (swap! atom #(remove (partial = value) %))))

(defn reset-delay! [atom value delay-ms] (reset! atom value) (async/go (async/<! (async/timeout delay-ms)) (reset! atom nil)))

(defn set-delay! [atom value delay-ms] (async/go (async/<! (async/timeout delay-ms)) (reset! atom value)))

(re-frame/reg-sub ::port-color ;; [::port-color flow-id bid vk nil :data-type])
                  (fn [_ [_ flow-id bid & [pid return-type]]]
                    (let [flow-map       @(ut/tracked-subscribe [::flowmap])
                          override-value (get-in @(ut/tracked-subscribe [::http/flow-results]) [:return-maps (str flow-id) bid])
                          fmap           (get flow-map bid)
                          ports          (get fmap :ports)
                          data           (get fmap :data)
                          value          (if override-value override-value (get data :user-input))
                          port-multi?    (true? (or (map? value) (vector? value)))]
                      (if (nil? pid)
                        (let [flow-select     @(ut/tracked-subscribe [::selected-flow-block])
                              selected?       (= bid flow-select)
                              python?         (= (get data :syntax) "python")
                              ttype           (get-in fmap [:data :flow-item :type] (get-in fmap [:data :drag-meta :type]))
                              outputs         (get ports :out)
                              outputs         (if (and override-value (= (vec (keys outputs)) [:out])) ;; if we
                                                {:out (keyword (ut/data-typer override-value))} ;; change output
                                                outputs)
                              error?          (get value :error false)
                              styler          (get-in data [:flow-item :style] {})
                              styler-selected (get-in data [:flow-item :style-selected] {})
                              styler          (if selected? styler-selected styler)
                              dt              (ut/data-typer value)
                              bcolor          (try (cond error?  "red"
                                                         python? "#F0E68C"
                                                         :else   (or (get styler :color nil)
                                                                     (get (theme-pull :theme/data-colors db/data-colors)
                                                                          dt
                                                                          (get (theme-pull :theme/data-colors db/data-colors)
                                                                               "unknown"
                                                                               "#FFA500"))))
                                                   (catch :default _ "orange"))]
                          (cond (= return-type :data-type)  dt
                                (= return-type :data-value) value
                                :else                       bcolor)) ;; assume color
                        (let [pid   (if (and port-multi? (= pid :out)) :* pid) ;; change up the default out for
                              value (if (and port-multi? (not (= pid :*))) ;; if :* just pass the whole value
                                      (try (let [idx? (cstr/starts-with? (str pid) ":idx")
                                                 pkey (if idx? (edn/read-string (ut/replacer (str pid) ":idx" "")) pid)]
                                             (get value pkey))
                                           (catch :default _ value))
                                      value)
                              dt    (if (nil? value) (get-in flow-map [bid :ports :out pid]) (ut/data-typer value))
                              color (get (theme-pull :theme/data-colors db/data-colors)
                                         (gn dt)
                                         (get (theme-pull :theme/data-colors db/data-colors) "unknown" "#FFA500"))]
                          (cond (= return-type :data-type)  dt ;(or dt :none)
                                (= return-type :data-value) value
                                :else                       color))))))

(re-frame/reg-sub ::block-inputs (fn [db [_ bid]] (get-in db [:flows (get db :selected-flow) :map bid :data :flow-item :inputs])))

(re-frame/reg-sub ::input-defaults
                  (fn [db [_ bid]] (get-in db [:flows (get db :selected-flow) :map bid :data :flow-item :defaults] {})))

(re-frame/reg-sub ::meta-bundle
                  (fn [db [_ bid pid]]
                    {:meta    (get-in db [:flows (get db :selected-flow) :map bid :data :flow-item :meta pid] {})
                     :default (get-in db [:flows (get db :selected-flow) :map bid :data :flow-item :defaults pid] {})}))

(def sniffy-sniff (reagent/atom nil))

(defn step-ports
  [bid ports w input? flow-id x y & [done? bcolor]]
  (let [;;ports (into {} (for [e (range (+ 1 (rand-int 6)))] {(keyword (str "a" (rand-int
        flow-select  @(ut/tracked-subscribe [::selected-flow-block])
        valid-drops  (when (and @dragging-port? (not (= bid (first @dragged-port)))) ;; dont want to
                                                                                     ;; highlight
                       (valid-drop-ports ports))
        react!       [@port-hover @dragged-port @flow-details-block-container-atom]
        binputs      @(ut/tracked-subscribe [::block-inputs bid])
        defaults     (when input? @(ut/tracked-subscribe [::input-defaults bid]))
        condi-ports  (when (not input?) @(ut/tracked-subscribe [::condi-ports bid]))
        push-ports   (when (not input?) @(ut/tracked-subscribe [::push-ports bid]))
        condi-valves (when condi-ports @(ut/tracked-subscribe [::bricks/condi-valves flow-id])) ;; react!
        outport-keys (into (into (vec (sort-by str (keys ports))) (keys condi-ports)) (keys push-ports))
        ports        (if (not input?)
                       (let [cports (into {} (for [n (into (keys condi-ports) (keys push-ports))] {n (get ports :out :any)}))]
                         (merge ports cports))
                       ports)
        inputs-vec   (try (edn/read-string binputs) ;; some legacy flows have a string here...
                          (catch :default _ binputs))
        port-width   (/ (- w 5) (count ports))]
    [re-com/h-box :size "none" :height "15px" :justify :between :attr
     {:on-mouse-enter #(reset! port-hover2 bid)
      :on-mouse-over  #(when (not (= @port-hover2 bid)) (reset! port-hover2 bid))
      :on-mouse-leave #(reset! port-hover2 nil)} :align :center :style {:font-size "10px" :z-index 687} :children
     (for [[k v] (if input?
                   (sort-map-keys (vec (or inputs-vec (keys ports))) ports)
                   (sort-map-keys (try outport-keys ;(sort (keys ports))
                                       (catch :default e
                                         (do (ut/tapp>> [:key-sorting-error e outport-keys :flows.cljs :ln 1802]) outport-keys)))
                                  ports))
           :let  [hover-key      [bid input? k (str " " k)]
                  valid-drop?    (true? (and input? @dragging-port? (some #(= k %) valid-drops)))
                  hovered?       (true? (= hover-key @port-hover))
                  hover-path     (get-in @flow-details-block-container-atom
                                         [flow-id flow-select :port-panel (if input? :in :out)])
                  panel-hovered? (true? (= [bid k] hover-path))
                  [ibid ivk]     (if input? @(ut/tracked-subscribe [::get-incoming-port bid :in k]) [nil nil])
                  port-color     @(ut/tracked-subscribe [::port-color flow-id (or ibid bid) (or ivk k)])
                  has-default?   (when input? (not (nil? (get defaults k))))
                  filled?        (if input?
                                   @(ut/tracked-subscribe [::get-incoming-port bid :in k])
                                   (ut/ne? @(ut/tracked-subscribe [::get-outgoing-port bid (if input? :in :out) k])))
                  multi?         (cstr/ends-with? (str k) "+")
                  pval1          (when (> (count ports) 1)
                                   @(ut/tracked-subscribe [::port-color flow-id (or ibid bid) (or ivk k) :data-value]))
                  condi?         (and (not input?) (cstr/starts-with? (str k) ":cond-path"))
                  push?          (and (not input?) (cstr/starts-with? (str k) ":push-path"))
                  valve-open?    (when condi? @(ut/tracked-subscribe [::bricks/condi-valve-open? flow-id bid k])) ;; TODO
                  icon           [re-com/md-icon-button :src (at) :md-icon-name
                                  (cond push? "zmdi-swap-vertical-circle" ;;"zmdi-caret-down-circle"
                                        valve-open? "zmdi-dot-circle"
                                        multi? "zmdi-plus-circle-o-duplicate"
                                        done? "zmdi-check-circle"
                                        has-default? "zmdi-dot-circle-alt"
                                        (or hovered?
                                            (and (= bid (first @dragged-port)) @dragging-port? (= k (second @dragged-port)))
                                            (and filled? panel-hovered?))
                                          "zmdi-circle"
                                        :else "zmdi-circle-o") :attr
                                  (if valid-drop?
                                    {:on-drag-over  #(do ;(ut/tapp>> [:h hovered? hover-key @port-hover])
                                                       (when (not (= @port-hover hover-key)) (reset! port-hover hover-key)))
                                     :on-drag-leave #(do (reset! port-hover nil))
                                     :on-drop       #(let [connected-string (str (first @dragged-port) "/" (second @dragged-port))
                                                           ts               (str (.toISOString (js/Date.)))]
                                                       (reset! port-hover [:dropped])
                                                       (set-delay! port-hover nil 3500)
                                                       (swap! involved into [bid (first @dragged-port)])
                                                       (reset-delay! involved nil 3000)
                                                       (reset! tentacle-pos [0 0])
                                                       (ut/dispatch-delay 200
                                                                          [::http/insert-alert
                                                                           [:h-box :gap "7px" :children
                                                                            [[:box :child connected-string]
                                                                             [:md-icon :md-icon-name "zmdi-chevron-right" :style
                                                                              {:font-size "24px"}] [:box :child (str bid "/" k)]
                                                                             [:play1 ["/home/ryanr/rechamber.mp3" ts]]
                                                                             [:md-icon :md-icon-name "zmdi-input-composite" :style
                                                                              {:font-size "24px"}]]] 9 1 5]))}
                                    (merge (when ;(and
                                             (> (count ports) 1)
                                             {:on-mouse-over  #(when (not (= @sniffy-sniff [input? k pval1]))
                                                                 (reset! sniffy-sniff [input? k pval1]))
                                              :on-mouse-enter #(reset! sniffy-sniff [input? k pval1])
                                              :on-mouse-leave #(reset! sniffy-sniff nil)})
                                           {:on-context-menu
                                              #(let [removed-count (remove-connections bid k input?)]
                                                 (doseq [e    (range removed-count)
                                                         :let [disconnected-string (str bid "/" k "(" e "/" removed-count ")")
                                                               ts                  (str (.toISOString (js/Date.)))]]
                                                   (do (swap! involved conj bid)
                                                       (reset-delay! involved nil 3000)
                                                       (ut/dispatch-delay (rand-int 300)
                                                                          [::http/insert-alert
                                                                           [:h-box :gap "7px" :children
                                                                            [[:box :child disconnected-string]
                                                                             [:md-icon :md-icon-name "zmdi-close" :style
                                                                              {:font-size "24px"}] [:box :child (str bid "/" k)]
                                                                             [:play1 ["/home/ryanr/dechamber.mp3" ts]]
                                                                             [:md-icon :md-icon-name "zmdi-input-composite" :style
                                                                              {:font-size "24px"}]]] 9 1 5]))))})) :style
                                  {:color       (if panel-hovered? port-color (str port-color 99)) ;(get
                                   :cursor      (if (and (not (and input? has-default?)) input?) "pointer" "grab")
                                   :transition  "all 0.5s ease-in-out"
                                   :filter      (when panel-hovered? (str "brightness(200%)"))
                                   :margin-left (cond valid-drop?                                               "-5px"
                                                      hovered?                                                  "-9px"
                                                      panel-hovered?                                            "-2px"
                                                      (and @dragging-port? (not (= bid (first @dragged-port)))) "3px"
                                                      :else                                                     "inherit")
                                   :z-index     (if (or panel-hovered? hovered?) 999 900)
                                   :font-size   (cond ;condi? "14px"
                                                  hovered?                                                  "34px"
                                                  panel-hovered?                                            "20px"
                                                  valid-drop?                                               "26px"
                                                  (and @dragging-port? (not (= bid (first @dragged-port)))) "10px"
                                                  :else                                                     "16px")}]]]
       ^{:key (str "flow-ports-" bid (if input? "-in-" "-out-") k)}
       [re-com/box :size "none" :align (if input? :end :start) :width (px port-width) :height "15px" :child
        (cond (not input?)              [draggable-port icon flow-id bid k v x y]
              valid-drop?               [droppable-port icon bid k]
              (and input? has-default?) [draggable-port icon flow-id bid
                                         {:from      bid
                                          :input-pid k
                                          :port-meta @(ut/tracked-subscribe [::meta-bundle bid k])
                                          :flow-id   flow-id
                                          :keypath   {}} v x y]
              :else                     icon)])]))
(re-frame/reg-sub ::input-out-of-date?
                  (fn [db [_ bid]]
                    (if (= (get-in db [:flows (get db :selected-flow) :map bid :data :drag-meta :type]) :open-block)
                      (let [user-input   (get-in db [:flows (get db :selected-flow) :map bid :data :user-input])
                            resolved-val @(ut/tracked-subscribe [::port-color (get db :selected-flow) bid :out :data-value])]
                        (not (= user-input resolved-val)))
                      false)))

(re-frame/reg-sub ::syntax (fn [db [_ bid]] (get-in db [:flows (get db :selected-flow) :map bid :data :syntax] "clojure")))

(re-frame/reg-sub ::image-check (fn [db _] (vec (for [kp (ut/kvpaths db) :when (ut/is-base64? (get-in db kp))] kp))))

(defn step-center
  [bid flow-id bcolor h w selected? icon]
  (let [hovered?      (and (= bid (first @port-hover)) (not @dragging-flow?))
        running?      @(ut/tracked-subscribe [::is-running? bid flow-id])
        bcolor        (if running? (theme-pull :theme/editor-outer-rim-color nil) bcolor)
        shades-color  (try (vec (for [s (shades bcolor)] (hsl->hex s))) (catch :default _ ["#ffffff"]))
        syntax        @(ut/tracked-subscribe [::syntax bid])
        tetrad-color  (try (vec (for [s (tetrad bcolor)] (hsl->hex s))) (catch :default _ ["#ffffff"])) ;; bad colors will
                                                                                                        ;; crash it.
        mouse-down-fn #(mouse-down-handler-block % bid)
        opts-map      @(ut/tracked-subscribe [::opts-map])
        has-override? (ut/ne? (get-in opts-map [:overrides bid]))
        tbid          (let [cc   (/ w 7) ;; px to char width
                            llen (count (str bid))]
                        (if (< cc llen) (str (subs (str bid) 0 cc) "..") bid))]
    ^{:key (str "flow-brick-center-" bid)}
    [re-com/v-box :height (px (- h 36)) :size "none" :padding "4px" :justify :center :children
     [[re-com/h-box :height "21px" :style {:margin-top "-4px"} :justify :between :align :center :children
       [[re-com/box :size "auto" :attr {:on-mouse-down mouse-down-fn :on-click #(select-block bid)} :style
         {:font-size "10px" :font-weight 700 :color (if selected? (first shades-color) (last shades-color))} :child
         (if (and @sniffy-sniff (= @port-hover2 bid))
           [re-com/h-box :size "auto" :justify :between :align :center :children
            (for [e [(if (first @sniffy-sniff) :in-port :out-port) (get @sniffy-sniff 1)]] [re-com/box :child (str e)])]
           (if hovered?
             (last @port-hover)
             (if has-override?
               [re-com/h-box :width "100%" :gap "6px" :align :center :justify :between :children
                [[re-com/box :child (str tbid)]
                 [re-com/box :child "*" :style
                  {:color (last shades-color) :font-weight 700 :font-size "18px" :margin-top "3px"}]]]
               (str tbid))))]
        (if running?
          [re-com/md-icon-button :src (at) :md-icon-name "zmdi-spinner" ;""zmdi-refresh-sync"
           :class "rotate-reverse linear infinite" :style
           {:color            (if selected? (last tetrad-color) (first tetrad-color))
            :cursor           "pointer"
            :margin-top       "4px"
            :transform-origin "11px 11px"
            :font-size        "22px"}]
          (when (and icon (>= w 125)) [render-icon icon (if selected? (last tetrad-color) (first tetrad-color)) 20 17]))]]
      (when (>= h 75)
        (let [pval         @(ut/tracked-subscribe [::port-color flow-id bid :out :data-value])
              view-pval    @(ut/tracked-subscribe [::port-color flow-id (keyword (str (ut/replacer (str bid) ":" "") "-vw")) :out
                                                   :data-value])
              out-of-date? @(ut/tracked-subscribe [::input-out-of-date? bid])]
          [re-com/box :size "none" :height (px (- h 60)) :style
           {:border-radius    "4px"
            :margin-top       "-3px"
            :overflow         "hidden"
            :border           (when out-of-date? (str "2px dashed " (if selected? (last tetrad-color) (first tetrad-color))))
            :background-color "#00000099"} :child
           (let [;pval (try (if (or (vector? pval) (map? pval) (list? pval))
                 pval              (if (or (vector? pval) (map? pval) (list? pval)) (ut/replace-large-base64 pval) pval)
                 view-pval         (if (or (vector? view-pval) (map? view-pval) (list? view-pval))
                                     (ut/replace-large-base64 view-pval)
                                     view-pval)
                 b64?              (ut/is-base64? (str pval))
                 simple-value?     (and (or (string? pval) (number? pval) (boolean? pval)) (not b64?))
                 rabbit-code-view? (or (and (vector? view-pval) (keyword? (first view-pval))) ;; is
                                       (and (map? view-pval) (contains? view-pval :queries) (contains? view-pval :view)))
                 rabbit-code?      (or (and (vector? pval) (keyword? (first pval))) ;; is it
                                       (and (map? pval) (contains? pval :queries) (contains? pval :view)))]
             (cond rabbit-code-view?                                             [re-com/box :padding "5px" :child
                                                                                  [buffy/render-honey-comb-fragments view-pval
                                                                                   (/ (- w 22) 50) (/ h 50) ; nil ;
                                                                                                            ; (/
                                                                                                            ; h
                                                                                                            ; 50)
                                                                                  ]]
                   rabbit-code?                                                  [re-com/box :padding "5px" :child
                                                                                  [buffy/render-honey-comb-fragments pval
                                                                                   (/ (- w 22) 50) (/ h 50) ; nil ;
                                                                                                            ; (/
                                                                                                            ; h
                                                                                                            ; 50)
                                                                                  ]]
                   simple-value?                                                 (let [;ss (ut/auto-font-size pval (- h 50)
                                                                                       ;(- w 25))
                                                                                       ss 10]
                                                                                   [re-com/box :child
                                                                                    (if (string? pval) (pr-str pval) (str pval))
                                                                                    :style
                                                                                    {:font-size   (px ss)
                                                                                     :padding     "5px"
                                                                                     :opacity     0.77
                                                                                     :color       (theme-pull
                                                                                                    :theme/editor-font-color
                                                                                                    nil)
                                                                                     :font-family (theme-pull
                                                                                                    :theme/monospaced-font
                                                                                                    nil)}])
                   (and (try (not (empty? pval)) (catch :default _ false)) b64?) [re-com/v-box :size "auto" :align :center
                                                                                  :justify :center :children
                                                                                  [;[re-com/box :child (str "(**base64
                                                                                   ;string: ~" (.toFixed (/ (* (count (str
                                                                                   [re-com/box :child
                                                                                    [:img
                                                                                     {:src   (str "data:image/png;base64," pval)
                                                                                      :width (px (- w 10))}]]]]
                   :else                                                         [code-box-smol w (- h 36)
                                                                                  (if (and @sniffy-sniff (= @port-hover2 bid))
                                                                                    (pr-str (last @sniffy-sniff))
                                                                                    (pr-str pval)) syntax]))]))] :style
     {;:border-top (str "1px solid " bcolor)
      :z-index          800
      :cursor           (if selected? "grab" "pointer")
      :background-color (str bcolor (if selected? "" 66))}]))




(defn flow-grid
  [ww hh xx yy flowmaps-connections flow-id flow-map]
  (let [selected-bid         @(ut/tracked-subscribe [::selected-flow-block])
        playing?             @(ut/tracked-subscribe [::audio/audio-playing?])
        done-block           (get (first (filter #(= (last %) :done) flowmaps-connections)) 0 :?)
        zoom-unlocked?       @(ut/tracked-subscribe [::zoom-unlocked?])
        grid-line-color      (str (theme-pull :theme/editor-outer-rim-color nil) 33)
        grid-line-color2     (str (theme-pull :theme/editor-outer-rim-color nil) 10)
        reactions!           [@port-hover @involved @flow-details-block-container-atom
                              @(ut/tracked-subscribe [::http/flow-results])]
        read-only-flow?      (true? (cstr/includes? flow-id "/"))
        bounds-x             (when read-only-flow? (apply min (for [[_ {:keys [x]}] flow-map :let []] x)))
        bounds-y             (when read-only-flow? (- (apply min (for [[_ {:keys [y]}] flow-map :let []] y)) 30))
        bounds-x2            (when read-only-flow? (apply max (for [[_ {:keys [x w]}] flow-map :let []] (+ w x))))
        bounds-y2            (when read-only-flow? (apply max (for [[_ {:keys [y h]}] flow-map :let []] (+ y h))))
        done-data-type-color (when read-only-flow?
                               (get (theme-pull :theme/data-colors db/data-colors)
                                    (if (some #(= % :*) (keys (get-in flow-map [done-block :ports :out])))
                                      :map
                                      (gn (first (vals (get-in flow-map [done-block :ports :out])))))
                                    (get (theme-pull :theme/data-colors db/data-colors) "unknown" "#FFA500")))]
    [re-com/h-box :size "none" :align :center :justify :center :width (px canvas-width) ;;"3200px"
                                                                                        ;;;"6400px"
                                                                                        ;;;(px ww)
     :height (px canvas-height) ;;"2400px" ;"3600px" ;(px hh)
     :style
     {:overflow         "hidden"
      :background-image (if zoom-unlocked?
                          (str "linear-gradient(to right, "
                               grid-line-color
                               " 1px, transparent 1px), linear-gradient(to bottom, "
                               grid-line-color
                               " 1px, transparent 1px)")
                          (str "linear-gradient(to right, "
                               grid-line-color2
                               " 1px, transparent 1px), linear-gradient(to bottom, "
                               grid-line-color2
                               " 1px, transparent 1px)"))
      :background-size  (when true ;zoom-unlocked?
                          "75px 75px")
      :border           (when zoom-unlocked? (str "3px dashed " (theme-pull :theme/editor-outer-rim-color nil)))} :children
     (conj
       (for [[bid {:keys [x y h w z data icon ports]} :as fmap] flow-map
             :let                                               [in-ports-map    (get ports :in)
                                                                 out-ports-map   (get ports :out)
                                                                 selected?       (= bid selected-bid)
                                                                 done-block?     (= bid done-block)
                                                                 hover-involved? (true? (some #(= % bid)
                                                                                              (get-in
                                                                                                @flow-details-block-container-atom
                                                                                                [flow-id :involved])))
                                                                 running?        @(ut/tracked-subscribe [::is-running? bid
                                                                                                         flow-id])
                                                                 error?          @(ut/tracked-subscribe [::is-error? bid flow-id])
                                                                 did             (str "flow-brick-" bid)
                                                                 bcolor          @(ut/tracked-subscribe [::port-color flow-id
                                                                                                         bid])]
             :when                                              true]
         ^{:key did}
         [re-com/v-box :attr
          {:id             did
           :on-mouse-enter #(reset! flow-hover bid)
           :on-mouse-over  #(when (not (= @flow-hover bid)) (reset! flow-hover bid))
           :on-mouse-leave #(reset! flow-hover nil)} :style
          {:position         "fixed"
           :z-index          610
           :border-radius    "5px"
           :filter           (if running?
                               "brightness(145%)"
                               (when (and (not (nil? selected-bid)) (not running?))
                                 (when (and (not hover-involved?) (not selected?)) "brightness(45%)")))
           :box-shadow       (if (or hover-involved? selected? running?)
                               (str "2px 1px 15px " bcolor)
                               (str "2px 1px 15px #00000045"))
           :user-select      "none"
           :border           (str "3px " (if selected? "dashed" "solid") " " bcolor (if selected? "" 45))
           :background-color (cond running? (theme-pull :theme/editor-outer-rim-color nil)
                                   :else    (str bcolor (if (or hover-involved? selected?) 75 35)))
           :width            (px w)
           :height           (px h)
           :left             x
           :top              y} :children
          [[step-ports bid in-ports-map w true flow-id x y] [step-center bid flow-id bcolor h w selected? icon]
           [step-ports bid out-ports-map w false flow-id x y done-block? bcolor]
           (when selected?
             [re-com/box ;; resize box
              :attr {:on-mouse-down #(resize-mouse-down-handler % bid)} :style
              {:position      "fixed"
               :z-index       985
               :size          "none"
               :cursor        "se-resize" ;; (theme-pull :theme/editor-outer-rim-color nil)
               :border-right  (str "5px solid " bcolor)
               :border-bottom (str "5px solid " bcolor)
               :left          (- (+ x w) 12)
               :top           (- (+ y h) 12)} :height "12px" :width "12px" :child " "])
           (when selected?
             [re-com/box ;; close box
              :style {:position "fixed" :z-index 985 :size "none" :left (- (+ x w) 17) :top (- y 2)} :height "12px" :width "12px"
              :child
              [re-com/md-icon-button :src (at) :md-icon-name "zmdi-close" :on-click #(remove-block bid) :style
               {:color bcolor :cursor "pointer" :height "12px" :font-size "16px"}]])]])
       (when (cstr/includes? (str flow-id) "/")
         [re-com/box :style
          {:position      "fixed"
           :left          (- bounds-x 50)
           :top           (- bounds-y 30)
           :width         (px (+ (- bounds-x2 bounds-x) 150))
           :height        (px (+ (- bounds-y2 bounds-y) 100))
           :border-radius "14px"
           :font-size     "10px"
           :border        (str "6px solid " done-data-type-color)
           :z-index       500} :padding "5px" :child "read-only sub-flow"]))]))























(defonce hide-menu-panel? (reagent/atom false))

(defn menu-panel
  []
  (let [menu-pills ["thiessen" "missles" "hope" "cope" "rope"]]
    [re-com/h-box :children
     [(when (not @hide-menu-panel?)
        [re-com/v-box :style {:position "fixed" :left 0 :top 35 :z-index 700} :gap "5px" :width "110px" :children
         (for [p menu-pills]
           ^{:key (str "pills" p)}
           [re-com/box :padding "5px" :child (str p) :style
            {:background-color "#000000"
             :border-top       "2px solid darkcyan"
             :border-right     "2px solid darkcyan"
             :border-bottom    "2px solid darkcyan"
             :font-size        "16px"
             :font-weight      700
             :z-index          770
             :border-radius    "0px 12px 12px 0px"
             :color            "cyan"}])])
      [re-com/box :style {:color "#ffffff" :cursor "pointer" :position "fixed" :left (if @hide-menu-panel? 15 125) :top 300} :attr
       {:on-click #(reset! hide-menu-panel? (not @hide-menu-panel?))} :child (if @hide-menu-panel? ">" "<")]]]))

(defn safe-cpk
  [k]
  (-> (str k)
      (ut/replacer #":" "")
      (ut/replacer #"/" ">")
      keyword))

(defn unsafe-cpk
  [k]
  (-> (str k)
      (ut/replacer #":" "")
      (ut/replacer #">" "/")
      keyword))


(defn get-flow-deps
  [panel-key body]
  (let [;;all-sql-call-keys      @(ut/tracked-subscribe [::bricks/all-sql-call-keys])
        all-sql-call-keys      @(ut/tracked-sub ::bricks/all-sql-call-keys {})
        sql-aliases-used       @(ut/tracked-sub ::bricks/panel-sql-aliases-in-views-body {:panel-key panel-key :body body})
        valid-body-params      (vec (ut/deep-flatten @(ut/tracked-subscribe [::bricks/valid-body-params-in body])))
        possible-datasets-used (set (for [e valid-body-params] (keyword (nth (ut/splitter (ut/safe-name e) #"/") 0))))
        used-datasets          (vec (cset/union (set sql-aliases-used)
                                                (cset/intersection possible-datasets-used (set all-sql-call-keys))))
        value-walks-targets    [] ;(filter (fn [x] (and (cstr/includes? (str x) ".") (not
        condi-walks-targets    [] ;(distinct (filter (fn [x] (cstr/includes? (str x) "condi/"))
        all-params             (into (into (into valid-body-params used-datasets) value-walks-targets) condi-walks-targets)
        all-params             (vec (filter (fn [x]
                                              (and (not (cstr/starts-with? (str x) ":theme/"))
                                                   (not (cstr/starts-with? (str x) ":query/"))))
                                      all-params))
        params-map             (into {} (for [k all-params] {(str k) k}))
        res-params-map         (resolver/logic-and-params params-map nil)
        res-params-map-types   (merge (into {}
                                            (for [[k v] res-params-map]
                                              (try {(edn/read-string k) (keyword (ut/data-typer v))} (catch :default _ nil))))
                                      (into {} (for [u used-datasets] {u :rabbitsql})))
        res-params-map-types   (into {} (for [[k v] res-params-map-types] {(safe-cpk k) v}))]
    res-params-map-types))

(re-frame/reg-sub ::sub-flow-loaded? (fn [db [_]] (get-in db [:sub-flow-incoming :file-path])))

(re-frame/reg-sub ::sub-flow-incoming (fn [db [_]] (get db :sub-flow-incoming)))

(def last-loaded (atom nil))

(defn flow-droppable
  [types-vec root element]
  (if true ;(not @dragging-port?)  ;; for. fucks. sake.
    [(reagent/adapt-react-class rdnd/Droppable)
     {:types types-vec
      :on-drag-enter #(when (or (and (= (get-in @bricks/dragging-body [:drag-meta :source-query]) :flows-sys) @drop-toggle?)
                                (ut/ne? (get-in @bricks/dragging-body [:flow-item :file-path])))
                        (let [inc-path   (if (ut/ne? (get-in @bricks/dragging-body [:flow-item :file-path]))
                                           (get-in @bricks/dragging-body [:flow-item :file-path])
                                           @(ut/tracked-subscribe [::conn/clicked-parameter-key [:flows-sys/file_path]]))
                              inc-loaded @(ut/tracked-subscribe [::sub-flow-loaded?])]
                          (when (and (not (= inc-path inc-loaded)) (not (= inc-path @last-loaded)))
                            (reset! last-loaded inc-path)
                            (ut/tapp>> [:loading-sub-flow! inc-path inc-loaded])
                            (ut/tracked-dispatch [::http/load-sub-flow inc-path]))))
      :on-drop
        #(when (or (empty? @port-hover) (nil? @port-hover)) ;; is not a port drop and intended
           (ut/tapp>> [:dropped? @port-hover @dragged-port types-vec root @bricks/dragging-body element])
           (if (and (= (get-in @bricks/dragging-body [:drag-meta :source-query]) :flows-sys) (not @drop-toggle?))
             (let [file-path @(ut/tracked-subscribe [::conn/clicked-parameter-key [:flows-sys/file_path]])]
               (ut/tracked-dispatch [::http/load-flow file-path])
               (ut/tapp>> [:load-flow file-path]))
             (let [incoming (js->clj %)
                   _ (ut/tapp>> [:incoming? incoming (first incoming) (get (first incoming) "meta-menu")
                                 (get incoming "meta-menu") (read-string (last (last incoming)))])
                   data (read-string (get incoming "meta-menu"))
                   cdata (read-string (get incoming "flow-port"))
                   from-port? (not (empty? cdata))
                   data (if from-port? cdata data)
                   browser-pull? (or (get data :browser-pull?) (get cdata :browser-pull?))
                   _ (ut/tapp>> [:incoming-data data])
                   [x y] root
                   sub-flow-part? (not (empty? (get-in @bricks/dragging-body [:flow-item :file-path])))
                   sub-flow-drop? (and ;(not from-port?)
                                    (or (and (= (get-in @bricks/dragging-body [:drag-meta :source-query]) :flows-sys)
                                             @drop-toggle?)
                                        sub-flow-part?))
                   file-path (when sub-flow-drop?
                               (if sub-flow-part?
                                 (get-in @bricks/dragging-body [:flow-item :file-path])
                                 @(ut/tracked-subscribe [::conn/clicked-parameter-key [:flows-sys/file_path]])))
                   _ (ut/tapp>> [:dropper file-path sub-flow-part? sub-flow-drop?])
                   sub-flow (when sub-flow-drop? @(ut/tracked-subscribe [::sub-flow-incoming]))
                   _ (when sub-flow-drop? (ut/tapp>> [:sub-flow-drop! (get sub-flow :file-path)]))
                   zoom-multi (get @db/pan-zoom-offsets 2)
                   zoom-offset-x (get @db/pan-zoom-offsets 0)
                   zoom-offset-y (get @db/pan-zoom-offsets 1)
                   drop-x (- (/ x zoom-multi) (/ zoom-offset-x zoom-multi))
                   drop-y (- (/ y zoom-multi) (/ zoom-offset-y zoom-multi))
                   rooted-data (assoc data :root root)
                   kp (get-in rooted-data [:drag-meta :keypath])
                   port-meta (get-in rooted-data [:drag-meta :port-meta])
                   port-meta? (and (not (empty? port-meta)) (map? port-meta))
                   get-in-drag? (and (vector? kp) (not (empty? kp)))
                   dm-type (get-in rooted-data [:drag-meta :type])
                   _ (ut/tapp>> [:dropeed-offsets root dm-type zoom-multi zoom-offset-x zoom-offset-y drop-x drop-y rooted-data])
                   flow-item (get @bricks/dragging-body :flow-item)
                   flow-body
                     (cond
                       port-meta? (let []
                                    {:w     125
                                     :h     60
                                     :z     0
                                     :data  {:drag-meta  {:type :open-block}
                                             :flow-item  {:expandable? true :meta {:* (get port-meta :meta)}}
                                             :user-input (get port-meta :default)}
                                     :ports {:in {} :out {:out :any}}
                                     :x     drop-x
                                     :y     drop-y})
                       from-port? (let [;kp (get-in rooted-data [:drag-meta :keypath])
                                        canned-fn (if (vector? kp) '(fn [x] (get-in x :kp)) '(fn [x] x))
                                        canned-fn (ut/postwalk-replacer {:kp kp} canned-fn)]
                                    {:fn     canned-fn ;'(fn [x] x)
                                     :w      125
                                     :raw-fn canned-fn ;'(fn [x] x)
                                     :icon   "zmdi-functions"
                                     :z      0
                                     :ports  {:in {:x :any} :out {:out :any}}
                                     :h      60
                                     :x      drop-x
                                     :y      drop-y
                                     :data   {:flow-item {:category    ":rabbit-base"
                                                          :fn          canned-fn ;'(fn [x] x)
                                                          :name        ":open-fn"
                                                          :raw-fn      canned-fn ;'(fn [x] x)
                                                          :type        :open-fn
                                                          :icon        "zmdi-functions"
                                                          :types       {:x :any :out :any}
                                                          :expandable? true
                                                          :drag-meta   {:type :open-fn}}}})
                       sub-flow-drop?
                         (let [;poss-inputs (vec (for [[k v] (get sub-flow :map)]
                               blocks      (set (keys (get sub-flow :map)))
                               base-conns  (set (for [[_ v2] (get sub-flow :connections)] (keyword (gns v2))))
                               no-inputs   (cset/difference blocks base-conns)
                               flow-inputs (into {}
                                                 (for [i no-inputs]
                                                   {i (get-in sub-flow
                                                              [:map i :ports :out
                                                               (first (keys (get-in sub-flow [:map i :ports :out])))])}))
                               done-block  (try (first (first (filter (fn [x] (= (second x) :done)) (get sub-flow :connections))))
                                                (catch :default _ :error))]
                           (ut/tapp>> [:sub-flow-build no-inputs base-conns])
                           {:w         200
                            :h         100
                            :x         drop-x
                            :y         drop-y
                            :z         0
                            :data      (-> rooted-data
                                           (assoc-in [:drag-meta :done-block] done-block)
                                           (assoc-in [:drag-meta :type] :sub-flow))
                            :sub-flow  sub-flow
                            :file-path file-path
                            :flow-id   (get sub-flow :flow-id)
                            :icon      "zmdi-puzzle-piece"
                            :ports     {:in  flow-inputs ;(dissoc (get flow-item :types) :out)
                                        :out (get-in sub-flow [:map done-block :ports :out])}}) ;(select-keys
                       flow-item ;; item picked from grid rows
                         (let []
                           (ut/tapp>> [:flow-item flow-item])
                           (merge {:w     200
                                   :h     100
                                   :x     drop-x
                                   :y     drop-y
                                   :z     0
                                   :data  (if (get flow-item :type)
                                            (assoc-in rooted-data [:drag-meta :type] (get flow-item :type))
                                            rooted-data)
                                   :icon  (get flow-item :icon "zmdi-pizza")
                                   :ports {:in  (dissoc (get flow-item :types) :out)
                                           :out (select-keys (get flow-item :types) [:out])}}
                                  (if (= (get flow-item :type) :open-fn) {:raw-fn (get flow-item :fn)} {})))
                       (= dm-type :view) ;; prarm dragged from editor bar
                         (let [view-ref             (keyword (str "view/" (gn (get-in rooted-data [:drag-meta :source-panel-key]))
                                                                  "."     (gn (get-in rooted-data [:drag-meta :source-table]))))
                               panel-key            (get-in rooted-data [:drag-meta :source-panel-key])
                               body                 @(ut/tracked-subscribe [::bricks/resolve-view-alias view-ref])
                               res-params-map-types (get-flow-deps panel-key body)]
                           {:w     200
                            :h     100
                            :x     drop-x
                            :y     drop-y
                            :z     0
                            :data  (-> @bricks/dragging-body
                                       (dissoc :queries)
                                       (assoc-in [:drag-meta :body] body)
                                       (assoc-in [:drag-meta :keypath]
                                                 [(get-in rooted-data [:drag-meta :source-panel-key]) :views
                                                  (get-in rooted-data [:drag-meta :source-table])])
                                       (assoc-in [:drag-meta :param-full] view-ref))
                            :icon  "zmdi-chart"
                            :ports {:in res-params-map-types :out {:out :rabbit-code}}})
                       (= dm-type :param) ;; prarm dragged from editor bar
                         {:w     200
                          :h     90
                          :x     drop-x
                          :y     drop-y
                          :z     0
                          :data  @bricks/dragging-body
                          :icon  "zmdi-tune"
                          :ports {:in  {}
                                  :out {:out (keyword (ut/data-typer @(ut/tracked-subscribe
                                                                        [::conn/clicked-parameter-key
                                                                         [(get-in rooted-data [:drag-meta :param-full])]])))}}}
                       (and (= dm-type :where) ;; cell value pull from query grid (acts as a
                            (get-in rooted-data [:drag-meta :row-num]))
                         (let [cell-ref (keyword (str (gn (get-in rooted-data [:drag-meta :param-table]))
                                                      "/" (gn (get-in rooted-data [:drag-meta :param-field]))
                                                      "." (get-in rooted-data [:drag-meta :row-num])))]
                           {:w     200
                            :h     60
                            :x     drop-x
                            :y     drop-y
                            :z     0
                            :data  (-> @bricks/dragging-body
                                       (dissoc :queries)
                                       (assoc-in [:drag-meta :type] :cell)
                                       (assoc-in [:drag-meta :param-full] cell-ref))
                            :icon  "zmdi-grid"
                            :ports {:in  {}
                                    :out {:out (keyword (ut/data-typer (first (resolver/logic-and-params [cell-ref] nil))))}}})
                       (= dm-type :query) (let [panel-key            (get-in rooted-data [:drag-meta :source-panel-key])
                                                qid                  (get-in rooted-data [:drag-meta :source-table])
                                                body                 @(ut/tracked-subscribe [::bricks/find-query qid])
                                                res-params-map-types (get-flow-deps panel-key body)]
                                            (ut/tapp>> [:qbody body])
                                            {:w     200
                                             :h     60
                                             :x     drop-x
                                             :y     drop-y
                                             :z     0
                                             :data  @bricks/dragging-body
                                             :icon  "zmdi-shape" ;(get flow-item :icon "zmdi-dns")
                                             :ports {:in res-params-map-types :out {:out :rabbitsql}}})
                       :else {:w     200
                              :h     50
                              :x     drop-x
                              :y     drop-y
                              :z     0
                              :data  @bricks/dragging-body
                              :icon  "zmdi-pizza"
                              :ports {:in {} :out {}}})
                   flow-id (cond sub-flow-drop? (ut/safe-key (try (edn/read-string (get sub-flow :flow-id))
                                                                  (catch :default _ :name-issue!))
                                                             @(ut/tracked-subscribe [::conn/reserved-type-keywords]))
                                 flow-item      (ut/safe-key (try (edn/read-string (get flow-item :name))
                                                                  (catch :default _ :name-issue!))
                                                             @(ut/tracked-subscribe [::conn/reserved-type-keywords]))
                                 :else          (or (get-in rooted-data [:drag-meta :source-table])
                                                    (get-in rooted-data [:drag-meta :param-full])))
                   safe-keys-reserved @(ut/tracked-subscribe [::conn/reserved-type-keywords])
                   src-block (ut/replacer (str (get-in rooted-data [:drag-meta :src-bid])) ":" "")
                   src-port (ut/replacer (str (get-in rooted-data [:drag-meta :src-pid])) ":" "")
                   check-port-drop (cond port-meta?   (ut/safe-key (keyword (str "<" src-port)) safe-keys-reserved)
                                         get-in-drag? ;; use the get-in keypath to name
                                           (let [;kp-str (ut/replacer (str (apply (comp merge str)
                                                 ;kp)) ":" ">")
                                                 kp-str (cstr/join ">" kp)
                                                 kp-str (-> kp-str
                                                            (ut/replacer ":" "")
                                                            (ut/replacer " " "")
                                                            (ut/replacer "'" ""))]
                                             (ut/safe-key (keyword (str src-block "_" kp-str)) safe-keys-reserved))
                                         :else        (ut/safe-key (keyword (str src-block "_" src-port)) safe-keys-reserved))]
               (reset! dragging? false)
               (reset! bricks/dragging? false)
               (cond false "hey"
                     :else (do ;;(ut/tapp>> [:flow-inherits-drag-body? @bricks/dragging-body data
                             (conn/add-flow-block drop-x drop-y flow-body (if from-port? check-port-drop flow-id) browser-pull?)
                             (when from-port?
                               (if port-meta? ;; in to out
                                 (connect-ports ;(keyword (if (= src-port "out") src-block (str
                                   check-port-drop ;(keyword (str (ut/replacer (str
                                   (keyword (if (= src-port "out") src-block (str src-block "/" src-port))))
                                 (connect-ports (keyword (if (= src-port "out") src-block (str src-block "/" src-port)))
                                                (keyword (str (ut/replacer (str check-port-drop) ":" "") "/x"))))))))))}
     [re-com/box :child element]]
    element))

(def scroll-id (reagent/atom "1"))




(def current-index (atom 0))
(def scrolly-pos (reagent/atom nil))

(defn handle-wheel
  [event]
  (let [direction (if (> (.-deltaY event) 0) 1 -1)
        items     (vec (keys @(ut/tracked-subscribe [::flowmap])))
        new-index (max 0 (min (count items) (+ @current-index direction)))]
    (ut/tapp>> [:scrolled-to new-index])
    (reset! current-index new-index)
    (reset! flow-hover (get items new-index))))

(defn add-wheel-listener
  [element-id]
  (let [element (gdom/getElement element-id)]
    (ut/tapp>> [:add-wheel-listener])
    (.removeEventListener element "wheel" handle-wheel)
    (.addEventListener element "wheel" handle-wheel)))

(defn remove-wheel-listener
  [element-id]
  (let [element (gdom/getElement element-id)]
    (ut/tapp>> [:REMOVE-wheel-listener])
    (.removeEventListener element "wheel" handle-wheel)))


(defn smooth-scroll-to-element
  [container-id element-id]
  (let [container      (gdom/getElement container-id)
        element        (gdom/getElement element-id)
        container-left (.-scrollLeft container)
        element-left   (- (.-offsetLeft element) (if @(ut/tracked-subscribe [::bricks/flow-editor?]) 640 40))]
    (when true ;(not (= @flow-select @scrolly-pos))
      (let [start    (atom nil)
            duration 500]
        (letfn [(step [timestamp]
                  (when (nil? @start) (reset! start timestamp))
                  (let [progress        (/ (- timestamp @start) duration)
                        flow-select     @(ut/tracked-subscribe [::selected-flow-block])
                        new-scroll-left (+ container-left (* progress (- element-left container-left)))]
                    (reset! scrolly-pos flow-select)
                    (set! (.-scrollLeft container) new-scroll-left)
                    (when (< progress 1) (.requestAnimationFrame js/window step))))]
          (.requestAnimationFrame js/window step))))))

(defn scroll-to-element
  [container-id element-id]
  (let [container      (gdom/getElement container-id)
        element        (gdom/getElement element-id)
        container-left (.-scrollLeft container)
        element-left   (- (.-offsetLeft element) (if @(ut/tracked-subscribe [::bricks/flow-editor?]) 610 10))]
    (set! (.-scrollLeft container) (- element-left container-left))))

(defn code-box-rw
  [width-int height-int value bid]
  (let [] ;sql-hint? (cstr/includes? (str value) ":::sql-string")
    [re-com/box :size "none" :width (px (- width-int 24)) :height (px (- height-int 24)) :style
     {:font-family   (theme-pull :theme/monospaced-font nil) ; "Chivo Mono" ;"Fira Code"
      :font-size     "14px"
      :overflow      "auto"
      :border-radius "12px"
      :font-weight   700} :child
     [(reagent/adapt-react-class cm/UnControlled)
      {:value   (ut/format-map (- width-int 24) (str value))
       :onBlur  #(try (let [new-vals (read-string (cstr/join " " (ut/cm-deep-values %)))]
                        (ut/tracked-dispatch [::update-flowmap-key bid nil new-vals]))
                      (catch :default _ (ut/tracked-dispatch [::update-flowmap-key bid nil value])))
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

(defn code-box-channels
  [width-int height-int value flow-id bid]
  (let [] ;sql-hint? (cstr/includes? (str value) ":::sql-string")
    [re-com/box :size "none" :width (px (- width-int 24)) :height (px (- height-int 24)) :style
     {:font-family   (theme-pull :theme/monospaced-font nil) ; "Chivo Mono" ;"Fira Code"
      :font-size     "14px"
      :overflow      "auto"
      :border-radius "12px"
      :font-weight   700} :child
     [(reagent/adapt-react-class cm/UnControlled)
      {:value   (ut/format-map (- width-int 24) (str value))
       :onBlur  #(try (let [new-vals (read-string (cstr/join " " (ut/cm-deep-values %)))]
                        (swap! channel-holster assoc-in [flow-id bid :value] new-vals))
                      (catch :default _ (ut/tapp>> [:error-saving-channel-data flow-id bid])))
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

(defn code-box-rwc
  [width-int height-int value bid]
  (let []
    [re-com/box :size "none" :width (px (- width-int 24)) :height (px (- height-int 24)) :style
     {:font-family   (theme-pull :theme/monospaced-font nil) ; "Chivo Mono" ;"Fira Code"
      :font-size     "14px"
      :overflow      "auto"
      :border-radius "12px"
      :font-weight   700} :child
     [(reagent/adapt-react-class cm/UnControlled)
      {:value   (ut/format-map (- width-int 24) (str value))
       :onBlur  #(try (let [new-vals (read-string (cstr/join " " (ut/cm-deep-values %)))]
                        (ut/tracked-dispatch [::update-connections new-vals]))
                      (catch :default _ (ut/tracked-dispatch [::update-flowmap-key bid nil value])))
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

(defn text-box-rw
  [width-int height-int value bid]
  (let [] ;sql-hint? (cstr/includes? (str value) ":::sql-string")
    [re-com/box :size "none" :width (px (- width-int 24)) :height (px (- height-int 24)) :style
     {:font-family   (theme-pull :theme/monospaced-font nil) ; "Chivo Mono" ;"Fira Code"
      :font-size     "11px"
      :overflow      "auto"
      :border-radius "12px"
      :font-weight   600} :child
     [(reagent/adapt-react-class cm/UnControlled)
      {:value   value
       :onBlur  #(try (let [new-vals (ut/cm-deep-values %)]
                        (ut/tracked-dispatch [::update-flowmap-key bid :description new-vals]))
                      (catch :default _ (ut/tapp>> [:issue-saving bid :text-rw (ut/cm-deep-values %)])))
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

(defn code-box-fn
  [width-int height-int value bid]
  (let [value-keys  (try (if (string? value)
                           (try (filter keyword? (ut/deep-flatten (edn/read-string value))) (catch :default _ []))
                           (filter keyword? (ut/deep-flatten value)))
                         (catch :default _ []))
        opts        @(ut/tracked-subscribe [::opts-map])
        uses        (vec (cset/intersection (set value-keys) (set (keys opts))))
        using-opts? (not (empty? uses))]
    [re-com/v-box :size "none" :width (px (- width-int 24)) :height (px (- height-int 24)) :style
     {:font-family   (theme-pull :theme/monospaced-font nil) ; "Chivo Mono" ;"Fira Code"
      :font-size     "14px"
      :overflow      "auto"
      :border-radius "12px"
      :font-weight   700} :children
     [[(reagent/adapt-react-class cm/UnControlled)
       {:value   (ut/format-map (- width-int 50) (str value))
        :onBlur  #(try (let [new-vals (read-string (cstr/join " " (ut/cm-deep-values %)))
                             fin      (ut/function-to-inputs new-vals)]
                         (ut/tracked-dispatch [::update-flowmap-key-in bid [:ports :in] fin])
                         (ut/tracked-dispatch [::update-flowmap-key bid :raw-fn new-vals])
                         (ut/tracked-dispatch [::clean-connections]))
                       (catch :default e
                         (do (ut/tapp>> [:error-saving-fn! (str e)])
                             (ut/tracked-dispatch [::update-flowmap-key bid :raw-fn value]))))
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
        [re-com/box :style {:opacity 0.5} :child
         [code-box width-int nil (str ";; using flow opts:  " (pr-str (select-keys opts uses)))]])]]))

(defn code-box-condi
  [width-int height-int value bid name]
  (let [value-keys  (filter keyword? (ut/deep-flatten value))
        opts        @(ut/tracked-subscribe [::opts-map])
        uses        (vec (cset/intersection (set value-keys) (set (keys opts))))
        using-opts? (not (empty? uses))]
    [re-com/v-box :size "none" :width (px (- width-int 24)) :height (px (- height-int 24)) :style
     {:font-family   (theme-pull :theme/monospaced-font nil) ; "Chivo Mono" ;"Fira Code"
      :font-size     "14px"
      :overflow      "auto"
      :border-radius "12px"
      :font-weight   700} :children
     [[(reagent/adapt-react-class cm/UnControlled)
       {:value   (ut/format-map (- width-int 50) (str value))
        :onBlur  #(ut/tracked-dispatch [::edit-condi-port bid name (read-string (cstr/join " " (ut/cm-deep-values %)))])
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
        [re-com/box :style {:opacity 0.5} :child
         [code-box width-int nil (str ";; using flow opts:  " (pr-str (select-keys opts uses)))]])]]))

(defn code-box-view
  [width-int height-int fmap bid flow-id]
  (let [value       (get fmap :view) ;; we want nil if not being used. '(fn [x] [:box :child
        value-keys  (filter keyword? (ut/deep-flatten value))
        bg-shade    "#00000055"
        opts        @(ut/tracked-subscribe [::opts-map])
        uses        (vec (cset/intersection (set value-keys) (set (keys opts))))
        using-opts? (not (empty? uses))]
    [re-com/box :style {:border-radius "9px" :padding-top "8px" :background-color bg-shade} :child
     [re-com/v-box :size "none" :height (px (- height-int 24)) :style
      {:font-family   (theme-pull :theme/monospaced-font nil) ; "Chivo Mono" ;"Fira Code"
       :font-size     "14px"
       :overflow      "auto"
       :border-radius "12px"
       :font-weight   700} :children
      [[re-com/box :width (px (- width-int 24)) :child
        [(reagent/adapt-react-class cm/UnControlled)
         {:value   (ut/format-map (- width-int 50) (str value))
          :onBlur  #(do (ut/tapp>> [:edit-view-fn bid (cstr/join " " (ut/cm-deep-values %))
                                    (try (read-string (cstr/join " " (ut/cm-deep-values %))) (catch :default e (str :error e)))])
                        (ut/tracked-dispatch [::edit-view-fn bid (read-string (cstr/join " " (ut/cm-deep-values %)))]))
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
         [re-com/box :style {:opacity 0.5} :child
          [code-box width-int nil (str ";; using flow opts:  " (pr-str (select-keys opts uses)))]])
       (let [condi-ports @(ut/tracked-subscribe [::push-ports bid])
             selected    (get-in @flow-details-block-container-atom [flow-id bid :port-panel :out])
             cnt         (count (keys condi-ports))
             open?       (true? (get-in @flow-details-block-container-atom [flow-id bid :push-paths :open?] false))]
         [re-com/v-box :style
          {:background    "#00000055"
           :padding-top   "8px"
           :font-family   (theme-pull :theme/base-font nil)
           :font-weight   400
           :font-size     "12px"
           :border-radius "8px"} :children
          [[re-com/h-box :size "auto" :padding "4px" :justify :between :align :center :style
            {:color  (if open?
                       (str (theme-pull :theme/editor-outer-rim-color nil) 99)
                       (str (theme-pull :theme/editor-outer-rim-color nil) 45))
             :cursor "pointer"} :children
            [[re-com/h-box :align :center :justify :center :style {:padding-left "3px"} :gap "5px" ;:style
                                                                                                   ;{;:margin-left
                                                                                                   ;"-3px"
              :attr {:on-click #(swap! flow-details-block-container-atom assoc-in [flow-id bid :push-paths :open?] (not open?))}
              :children
              [[re-com/md-icon-button :md-icon-name "fa-solid fa-carrot" :style {:font-size "17px" :cursor "pointer"}]
               [re-com/box :child (str "push paths (" cnt ")")]]]
             [re-com/md-icon-button :on-click #(ut/tracked-dispatch [::add-push-port bid]) :md-icon-name "zmdi-plus" :style
              {:font-size "22px" :cursor "pointer"}]]] (when (and open? (> cnt 0)) [re-com/gap :size "8px"])
           (when (and open? (> cnt 0))
             [re-com/v-box :style {:border-radius "8px" :background-color "#00000044"} :gap "8px" :padding "4px" :children
              (for [[k {:keys [dest]}] condi-ports
                    :let               [select-vec  [bid k]
                                        valve-open? false ;@(ut/tracked-subscribe
                                        selected?   (or (and @sniffy-sniff (= @port-hover2 bid) (= k (get @sniffy-sniff 1)))
                                                        (= select-vec selected))
                                        is-first?   (= k :push-path)
                                        short-code  (str ";; use for an on-click fn                      ["
                                                         k
                                                         "> \"*some-value*\"] or  "
                                                         (when is-first? "  ")
                                                         "          ["
                                                         k
                                                         ">] for passing %, on-change")
                                        pcolor      (theme-pull :theme/editor-outer-rim-color nil)]]
                [re-com/v-box :padding "4px" :attr
                 {:on-mouse-enter #(do
                                     (swap! flow-details-block-container-atom assoc-in [flow-id bid :port-panel :out] select-vec))
                  :on-mouse-leave #(do (swap! flow-details-block-container-atom assoc-in [flow-id bid :port-panel :out] nil))}
                 :style
                 {:border           (if (or selected? valve-open?) (str "1px solid " pcolor) (str "1px solid " pcolor 25))
                  :background-color (if (or selected? valve-open?) (str pcolor "22") (str pcolor "08"))
                  :border-radius    "6px"} :gap "8px" :children
                 [[re-com/h-box :size "auto" :justify :between :align :center :style
                   {:color         (str (theme-pull :theme/editor-outer-rim-color nil) 88)
                    :padding-left  "4px"
                    :padding-right "1px"
                    :padding-top   "4px"} :children
                   [[re-com/box :style {:font-size "15px" :font-weight 700} :child (str k)]
                    [re-com/h-box :width "300px" :height "100%" :justify :between :align :center :children
                     [[re-com/v-box :children (for [d dest] [re-com/box :child (str d)])]
                      [re-com/md-icon-button :src (at) :md-icon-name "zmdi-close" :on-click
                       #(ut/tracked-dispatch [::remove-push-port bid k]) :style {:cursor "pointer" :font-size "16px"}]]]]]
                  [re-com/h-box :justify :between :align :center :children
                   [;;[code-box-condi 500 nil fn bid k]
                    [code-box 500 nil short-code]]]]])])]])]]]))

(defn code-box-rwo
  [width-int height-int value bid & [syntax]]
  (let [syntax     (or (if (= syntax "raw (clojure)") "clojure" syntax) "clojure") ;; Error: true
        stringify? (not (= syntax "clojure"))
        value      (if (and stringify? (empty? value)) " " value) ;; just in case we get in a
       ]
    [bricks/reecatch
     [re-com/box :size "none" :width (px (- width-int 24)) :height (px (- height-int 24)) :style
      {:font-family   (theme-pull :theme/monospaced-font nil) ; "Chivo Mono" ;"Fira Code"
       :font-size     "14px"
       :overflow      "auto"
       :border-radius "12px"
       :font-weight   700} :child
      [(reagent/adapt-react-class cm/UnControlled)
       {:value (if stringify? value (ut/format-map (- width-int 24) (pr-str value)))
        :onBlur
          #(if stringify?
             (let [ddata     (ut/cm-deep-values %)
                   inputs    (ut/template-find (str ddata))
                   input-map (into {}
                                   (for [e    (range (count inputs))
                                         :let [name (keyword (ut/replacer (str (ut/safe-name (get inputs e))) "/" "-"))]]
                                     {name :any}))]
               (ut/tracked-dispatch [::update-flowmap-key-in bid [:ports :in] input-map])
               (ut/tracked-dispatch [::update-flowmap-key-in bid [:data :user-input] ddata])
               (ut/tracked-dispatch [::update-flowmap-key-in bid [:data :flow-item :inputs] (vec inputs)])
               (ut/tracked-dispatch [::update-flowmap-key-in bid [:ports :out] {:out :any}])
               (ut/tracked-dispatch [::clean-connections]))
             (try (let [ddata           (read-string (cstr/join " " (ut/cm-deep-values %)))
                        dtype           (ut/data-typer ddata)
                        ports           {;:in (get-in v [:ports :in])
                                         :out {:out (keyword dtype)}}
                        ports           (cond
                                          (= dtype "map")
                                            {:out (assoc (into {} (for [[k v] ddata] {k (keyword (ut/data-typer v))})) :* :map)}
                                          (or (= dtype "vector") (= dtype "rowset")) ; = dtype
                                            {:out (assoc (into {}
                                                               (for [k    (range (count ddata))
                                                                     :let [v (get ddata k)]]
                                                                 {(keyword (str "idx" k)) (keyword (ut/data-typer v))}))
                                                    :* :vector)}
                                          :else ports)
                        inputs          (filter (fn [x]
                                                  (and (keyword? x)
                                                       (or (cstr/starts-with? (str x) ":input-")
                                                           (cstr/starts-with? (str x) ":*")
                                                           (cstr/ends-with? (str x) "*"))))
                                          (ut/deep-flatten ddata))
                        flow-id         @(ut/tracked-subscribe [::selected-flow])
                        existing-inputs (get-in @(ut/tracked-subscribe [::flowmap]) [flow-id bid :ports :in])
                        inputs-map      (into {} (for [i inputs] {i (get existing-inputs i :any)}))]
                    (ut/tracked-dispatch [::update-flowmap-key-in bid [:ports :in] inputs-map])
                    (ut/tracked-dispatch [::update-flowmap-key-in bid [:data :user-input] ddata])
                    (ut/tracked-dispatch [::update-flowmap-key-in bid [:ports :out] ports])
                    (ut/tracked-dispatch [::clean-connections]))
                  (catch :default e (ut/tapp>> [:saving-issue :code-box-rwo bid (str e) :parse-issue?]))))
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

(defn flow-details-block-container
  [title flow-id bid content & [icon]]
  (let [open?       (true? (get-in @flow-details-block-container-atom
                                   [flow-id bid title :open?]
                                   (not (or (cstr/ends-with? (str title) "*")))))
        flow-select @(ut/tracked-subscribe [::selected-flow-block])
        sys?        (nil? flow-select)
        dyn-width   (if (not sys?) 600 (last @db/flow-editor-system-mode))]
    [re-com/v-box :width "100%" ;(px (- dyn-width 15)) ;; "586px"
     :style
     {:border           (str "1px solid " (theme-pull :theme/editor-outer-rim-color nil) 45)
      :font-size        "12px"
      :transition       "width 0.2s"
      :border-radius    "7px"
      :background-color (str (theme-pull :theme/editor-rim-color nil) "18")} :children
     [[re-com/h-box :padding "5px" :height "30px" :justify :between :align :center :children
       [;[re-com/box :child "header1"]
        [re-com/h-box :gap "3px" :align :center :justify :center :children
         (into (if icon
                 (cond (string? icon) [[re-com/md-icon-button :src (at) :md-icon-name icon :style
                                        {:color     (if open?
                                                      (str (theme-pull :theme/editor-outer-rim-color nil) 99)
                                                      (str (theme-pull :theme/editor-outer-rim-color nil) 45))
                                         :font-size "14px"}]]
                       (vector? icon) [[re-com/h-box :gap "0px" :style {:padding-left "3px"} :children
                                        (vec (for [i icon]
                                               [re-com/md-icon-button :src (at) :md-icon-name i :style
                                                {:margin-left "-1px"
                                                 :color       (if open?
                                                                (str (theme-pull :theme/editor-outer-rim-color nil) 99)
                                                                (str (theme-pull :theme/editor-outer-rim-color nil) 45))
                                                 :font-size   "15px"}]))]])
                 [])
               [[re-com/box :child (str title) :attr
                 {:on-click #(swap! flow-details-block-container-atom assoc-in [flow-id bid title :open?] (not open?))} :style
                 {:color        (str (theme-pull :theme/editor-outer-rim-color nil) 99)
                  :cursor       "pointer"
                  :filter       "drop-shadow(0 2px 2px black)" ;; nice and subtle
                  :padding-left "4px"}]])]
        [re-com/md-icon-button :src (at) :md-icon-name "zmdi-more" :on-click
         #(swap! flow-details-block-container-atom assoc-in [flow-id bid title :open?] (not open?)) :style
         {:color     (if open? (str (theme-pull :theme/editor-rim-color nil) 99) (theme-pull :theme/editor-outer-rim-color nil))
          :cursor    "pointer"
          :font-size "17px"}]]]
      (when open? ;(get-in @flow-details-block-container-atom [flow-id bid title :open?] true)
        [re-com/box :padding "8px" :child content]) [re-com/gap :size "6px"]]]))

(defn flow-details-block
  [cch ccw id fmap show-output?]
  (let [;honeycomb? (try (and (keyword? (first body)) (vector? body)) (catch :default _ false))
        react-hacks     [@scroll-id]
        type            (cond (= (get-in fmap [:data :source-panel]) :flow-fn-list*) :flow-part
                              :else                                                  (get-in fmap [:data :drag-meta :type]))
        ttype           (get-in fmap [:data :drag-meta :type])
        flow-select     @(ut/tracked-subscribe [::selected-flow-block])
        selected?       (= (str id) (str flow-select))
        hovered?        (= (str id) (str @flow-hover))
        raw-fmap        @(ut/tracked-subscribe [::flowmap-raw-block id]) ;;(get @flowmaps id)
        flow-id         @(ut/tracked-subscribe [::selected-flow])
        cvalue          (get-in @(ut/tracked-subscribe [::http/flow-results]) [:return-maps flow-id id] "(not run yet)")
        error?          (get cvalue :error false)
        styler          (get-in fmap [:data :flow-item :style] {})
        outputs         (get-in fmap [:ports :out])
        override-value  (get-in @(ut/tracked-subscribe [::http/flow-results]) [:return-maps (str flow-id) id])
        outputs         (if (and override-value (= (vec (keys outputs)) [:out])) ;; if we have return
                          {:out (keyword (ut/data-typer override-value))} ;; change output data types
                          outputs)
        python?         (= (get-in fmap [:data :syntax]) "python")
        out-color       (try (or (get styler :color nil)
                                 (get (theme-pull :theme/data-colors db/data-colors)
                                      (if (some #(= % :*) (keys outputs))
                                        :map ;; if its a map always use map color for base block
                                        (gn (first (vals outputs))))
                                      (get (theme-pull :theme/data-colors db/data-colors) "unknown" "#FFA500")))
                             (catch :default _ "orange"))
        ccolor          (cond error?  "red"
                              python? "#F0E68C"
                              :else   (if (or selected? hovered?) out-color (str out-color 75)))
        flow-id-regex   #"^[a-zA-Z0-9_-]+$" ;; alpha, underscores, hypens, numbers
        read-only-flow? (true? (cstr/includes? flow-id "/"))
        str-id          (ut/replacer (str id) #":" "")
        block-renamer   (if (= @rename-block id)
                          [re-com/input-text :src (at) :model str-id ;(str id)
                           :width "200px" :on-change
                           #(do (if (= str-id (str %)) ;; regex doesnt allow : so we need to
                                  (reset! rename-block nil)
                                  (do (ut/tracked-dispatch [::rename-block id %]) (reset! rename-block nil)))) :validation-regex
                           flow-id-regex :change-on-blur? true :style
                           {:text-decoration  "underline"
                            :color            "inherit"
                            :margin-top       "3px"
                            :margin-left      "-4px"
                            :text-align       "left"
                            :background-color "#00000000"}]
                          [re-com/box :attr (if selected? {:on-double-click #(reset! rename-block id)} {}) :child (str id)])
        output-toggle   [re-com/md-icon-button :src (at) :md-icon-name "zmdi-more" :on-click
                         #(ut/tracked-dispatch [::flow-block-meta id :view-output? (not show-output?)]) :style
                         {:color     ccolor ; (theme-pull :theme/editor-outer-rim-color nil)
                          :cursor    "pointer"
                          :font-size "17px"}]
        return-val      (get-in fmap [:data :user-input] cvalue)
        default-output  [re-com/v-box :gap "9px" :padding "8px" :width (px ccw) ;"350px"
                         :height "380px" :size "none" :align :center :justify :center :style {:margin-top "-9px"} :children
                         [;[re-com/box :child (str id " (viewer)")]
                          [re-com/h-box :width "350px" :size "none" :padding "6px" :justify :between :align :center :padding "8px"
                           :style {:margin-top "17px"} :children
                           [;[re-com/box :child (str id)]
                            block-renamer [re-com/box :child "(viewer)"]]] [render-icon (get fmap :icon) ccolor]
                          [re-com/box :padding "6px" :style {:font-size "11px"} :child
                           (get-in fmap [:data :flow-item :description] " ")]
                          (cond (and (vector? return-val) (keyword? (first return-val))) ;; try to render as
                                  [re-com/box :size "none" :height "280px" :width (px ccw)                ;"350px"
                                   :style {:overflow "auto"} :child [buffy/render-honey-comb-fragments return-val 7 5]]
                                (vector? return-val)                                     [re-com/box :size "none" :height "280px"
                                                                                          :width (px ccw) ;"350px"
                                                                                          :style {:overflow "auto"} :child
                                                                                          [map-boxes2 return-val :sub-flows-used
                                                                                           nil [] :output "vector"]]
                                (map? return-val)                                        [re-com/box :size "none" :height "280px"
                                                                                          :width (px ccw) ;"350px"
                                                                                          :style {:overflow "auto"} :child
                                                                                          [map-boxes2 return-val :sub-flows-used
                                                                                           nil [] :output "map"]]
                                :else                                                    [re-com/box :size "none" :height "280px"
                                                                                          :width (px ccw) ;"350px"
                                                                                          :child
                                                                                          [code-box ccw 280
                                                                                           (if (string? return-val)
                                                                                             (pr-str return-val)
                                                                                             return-val)]])]]]
    [re-com/v-box :attr
     {:id (str id) :on-drag-over #(when @bricks/swap-layers? (reset! bricks/swap-layers? false)) :on-click #(select-block id)}
     :height (px cch) :size "none" :padding "10px" :width (px ccw) :align :center :justify :center :style
     {:color            ccolor ;(str (theme-pull :theme/editor-font-color nil) 35)
      :border           "1px solid cyan"
      :background-color (str ccolor 10)
      :margin-top       "13px"} :children
     [;; flow-details-block-container [content title flow-id bid]
      (cond
        show-output?                                                                         ;; force
                                                                                             ;; default
                                                                                             ;; output
                                                                                             ;; render
          default-output
        (= @editor-mode :debug) [re-com/box :child
                                 [(if read-only-flow? code-box code-box-rw) 600 440 raw-fmap ;(get
                                                                                             ;fmap
                                                                                             ;flow-select)
                                  flow-select] :width "600px" :height "430px" :size "1"]
        (= ttype :open-fn) [re-com/v-box :size "none" :gap "9px" :padding "6px" :align :center :justify :center :width (px ccw) ;"350px"
                            :height "380px" :style {:margin-top "-5px"} :children
                            [[re-com/h-box :width "350px" :size "none" :justify :between :align :center :padding "8px" :style
                              {:margin-top "17px"} :children
                              [;[re-com/box :child (str id)]
                               block-renamer [re-com/box :child "clojure"]]]
                             [re-com/box :child [code-box-fn ccw 378 (str (get fmap :raw-fn "(fn [x] x)")) id]]]]
        (= type :sub-flow)
          (let [;flowmaps-connections @(ut/tracked-subscribe [::flowmap-connections])
                subby-results   (get-in @(ut/tracked-subscribe [::http/flow-results]) [:return-maps])
                sub-flow-lookup (into {} (for [k (keys subby-results)] {(keyword (last (ut/splitter (str k) "/"))) k}))
                sfl             (get sub-flow-lookup id)]
            [re-com/box :height "380px" :size "none" :style
             {;:border "1px solid pink"
              :overflow "auto"} :child
             [re-com/v-box :width (px ccw) :height "380px" :size "none" :align :center :justify :between :children
              [[re-com/h-box :width "440px" :justify :between :align :center :children
                [[re-com/box :child (str type) :style {:color out-color}] [re-com/box :child (str sfl) :style {:color out-color}]]
                :padding "6px"]
               [re-com/box :height "340px" :size "none" :child
                [re-com/v-box :gap "18px" :children
                 (vec (for [i (filter #(not (= (str %) (str flow-id))) (keys subby-results))]
                        [re-com/v-box :children
                         [[re-com/box :child (str i) :style {:font-size "19px"}]
                          [map-boxes2 (get subby-results i) :sub-flows-used nil [] :sub-flows-used "map"]]]))]]]]])
        (= type :view) (let [qid  (get-in fmap [:data :drag-meta :source-table])
                             body (get-in fmap [:data :drag-meta :body])]
                         [re-com/box :width "500px" :height "380px" :size "none" :style
                          {;:border "1px solid pink"
                           :overflow "auto"} :child
                          [re-com/v-box :width "495px" :height "400px" :size "auto" :align :center :justify :between :children
                           [[re-com/h-box :width "490px" :justify :between :align :center :children
                             [[re-com/box :child (str qid) :style {:color out-color}]
                              [re-com/box :child (str type) :style {:color out-color}]] :padding "6px"]
                            [re-com/box :align :center :justify :center :width "490px" :height "360px" :size "none" :child
                             [buffy/render-honey-comb-fragments body 9 7]]]]])
        (= type :open-block)
          (let [;edit-style :fragment
                syntaxes ["raw (clojure)" "python" "r" "julia"]
                widths   [200 350 750]
                width    (get-in fmap [:data :width] 350)
                syntax   (get-in fmap [:data :syntax] "raw (clojure)")]
            [re-com/v-box :size "none" :gap "9px" :padding "6px" :align :center :justify :center :width (px width) :height "380px"
             :style
             {;:border "1px solid green"
              :margin-top "-5px"} :children
             [[re-com/h-box :width (px width) :size "none" :justify :between :align :center :padding "8px" :style
               {:margin-top "17px"} :children
               [;[re-com/box :child (str id)]
                block-renamer [re-com/box :child (str type)]]]
              [(if read-only-flow? code-box code-box-rwo) width 330
               (get-in raw-fmap [:data :user-input] (pr-str "feed me, seymour!")) id syntax]
              [re-com/h-box :width (px width) :height "40px" :size "none" :justify :between :align :center :gap "8px" :children
               [[re-com/h-box :gap "8px" :style {:font-size "11px"} :children
                 (for [e    widths
                       :let [selected? (= e width)]]
                   [re-com/box :attr (when (not selected?) {:on-click #(ut/tracked-dispatch [::flow-block-meta id :width e])})
                    :style (if selected? {} {:opacity 0.4 :cursor "pointer"}) :child (str e)])]
                [re-com/h-box :gap "8px" :style {:font-size "11px"} :children
                 (for [e    syntaxes
                       :let [selected? (= e syntax)]]
                   [re-com/box :attr (when (not selected?) {:on-click #(ut/tracked-dispatch [::flow-block-meta id :syntax e])})
                    :style (if selected? {} {:opacity 0.4 :cursor "pointer"}) :child (str e)])]]]]])
        (= type :flow-part) default-output
        (= type :query) (let [qid           (get-in fmap [:data :drag-meta :source-table]) ;; original
                              connection-id (get-in fmap [:data :connection-id])
                              query-body    @(ut/tracked-subscribe [::bricks/find-query qid])
                              packed-query  (merge (-> query-body
                                                       (assoc :connection-id connection-id))
                                                   {:_w 10 :_h 5})]
                          [re-com/box :width "500px" :height "380px" :size "none" :style
                           {;:border "1px solid pink"
                            :overflow "auto"} :child
                           [re-com/v-box :width "495px" :height "368px" :size "none" :align :center :justify :center :children
                            [[re-com/h-box :width "490px" :justify :between :align :center :children
                              [[re-com/box :child (str qid) :style {:color out-color}]
                               [re-com/box :child (str type) :style {:color out-color}]] :padding "6px"]
                             [buffy/render-honey-comb-fragments packed-query] [code-box 500 163 query-body]]]])
        :else [re-com/box :child (str type "?")])
      (if (or (= (get-in fmap [:data :drag-meta :type]) :open-fn)
              (and ;(not (= type :open-block))
                (not (= type :query))))
        [re-com/gap :size "17px"])]]))

(defn run-gantt-chart
  [flow-id]
  [buffy/render-honey-comb-fragments
   (let [reaction! [@trig-atom-test]]
     (ut/postwalk-replacer
       {:fflow-id flow-id :ttttt @trig-atom-test}
       {:queries
          {:live-dat
             {:select [:*]
              :_last :ttttt
              :from
                [{:data
                    '(vec
                      (filter
                       (fn [x] (not (cstr/includes? (get x :block) "/")))
                       (for
                        [row (get (clojure.core/deref flowmaps.db/fn-history) :fflow-id)]
                        {:start (get row :start 0) :end (+ 5 (get row :end 0)) :block (str (get row :block "?"))})))}]}}
        :view
          [:vega-lite
           {:data       {:values :live-dat}
            :mark       "bar"
            :encoding   {:x       {:field    :start
                                   :type     "quantitative"
                                   :timeUnit "yearmonthdatehoursminutessecondsmilliseconds"
                                   :scale    {:zero false :nice false}
                                   :axis     {:title "" :format "%Y-%m-%d %H:%M:%S"}}
                         :x2      {:field :end :scale {:zero false :nice false}}
                         :y       {:field :block :type "ordinal" :axis {:title ""}}
                         :tooltip [{:field "start" :type "temporal" :timeUnit "utcyearmonthdatehoursminutessecondsmilliseconds"}
                                   {:field "value"} {:field "ms" :type "temporal" :timeUnit "minutessecondsmilliseconds"}
                                   {:field "type"} {:field "path"} {:field "dest"}
                                   {:field "end" :type "temporal" :timeUnit "utcyearmonthdatehoursminutessecondsmilliseconds"}]
                         :color   {:scale :theme/vega-default-color-scheme :legend nil :field :block :type "ordinal"}}
            :config     :theme/vega-defaults
            :width      "container"
            :height     :panel-height
            :padding    4
            :background "transparent"} {:actions false}]})) 7 11])
(defonce scheduler-atom (reagent/atom {}))

(defn edit-flow-title
  [flow-id w]
  (let [read-only-flow? (true? (cstr/includes? flow-id "/"))
        flow-id-regex   #"^[a-zA-Z0-9_-]+$"] ;; alpha, underscores, hypens, numbers
    (if (not @title-edit-idx)
      [re-com/box :size "none" :height "45px" :width (px w) :align :center :justify :end :attr
       {:on-double-click #(when (not read-only-flow?) (reset! title-edit-idx (str flow-id)))} :style
       {:cursor "pointer" :padding-right "12px" :padding-top "1px" :border "2px solid transparent" :font-size "30px"} :child
       (str flow-id)]
      [re-com/input-text :src (at) :model (str flow-id) :width (px w) :height "45px" :on-change
       #(do (ut/tracked-dispatch [::rename-flow flow-id %]) (reset! title-edit-idx nil)) :validation-regex flow-id-regex
       :change-on-blur? true :style
       {:border           (str "2px dashed " (theme-pull :theme/editor-outer-rim-color nil))
        :font-size        "30px"
        :text-decoration  "underline"
        :color            (theme-pull :theme/editor-outer-rim-color nil)
        :font-style       "underline"
        :text-align       "right"
        :background-color "#00000000"}])))

(defn run-history-bar
  [flow-id flow-select]
  [re-com/box :style {:margin-left "25px" :margin-top "15px"} :child
   [buffy/render-honey-comb-fragments
    {:queries {:runstream-chart-simple-python-exec {:select        [[[[:min :elapsed]] :elapsed] :started]
                                                    :_ttttt        @trig-atom-test
                                                    :connection-id "flows-db"
                                                    :from          [{:select [:client_name :elapsed :ended :flow_id :human_elapsed
                                                                              :in_error :started :ts]
                                                                     :from   [[:flow_history :mm134]]
                                                                     :where  [:= :flow_id flow-id]}]
                                                    :group-by      [:started]}}
     :view    [:> :ResponsiveContainer {:width "100%" :height :panel-height+50}
               [:> :BarChart {:data :runstream-chart-simple-python-exec :margin {:top 5 :bottom 5 :right 30 :left 20}}
                [:> :CartesianGrid {:strokeDasharray "1 4" :opacity 0.33}] [:> :Tooltip] [:> :XAxis {:dataKey :started}]
                [:> :Bar {:dataKey :elapsed :stroke :theme/editor-outer-rim-color :fill :theme/editor-outer-rim-color}]]]} 5.5
    11]])

(defonce last-loaded-run-id (reagent/atom nil))

(defn settings-block
  [flow-id ttype]
  (let [flow-select         @(ut/tracked-subscribe [::selected-flow-block])
        flowmaps            @(ut/tracked-subscribe [::flowmap-raw])
        dyn-width           (last @db/flow-editor-system-mode)
        panel-height        (* (.-innerHeight js/window) 0.50)
        panel-height-bricks (/ panel-height db/brick-size)
        read-only-flow?     (true? (cstr/includes? flow-id "/"))]
    (cond
      (= ttype :run-history)
        (let [caller @(ut/tracked-subscribe [::conn/clicked-parameter-key [:virtual-panel/client_name]])
              status @(ut/tracked-subscribe [::conn/clicked-parameter-key [:virtual-panel/return_status]])
              dropdown1
                {:view    [:dropdown
                           {:choices     :gen-viz-812aaa
                            :width       "300px"
                            :placeholder "(all callers)"
                            :style       {:padding-top   "8px"
                                          :border-radius "8px" ;:background-color "#ffffff"
                                          :color         "#ffffff75"
                                          :outline       (str "1px solid " (theme-pull :theme/editor-outer-rim-color nil) 33)}
                            :model       :virtual-panel/client_name
                            :field       :client_name}]
                 :queries {:gen-viz-812aaa {:select        [[:client_name :id] [:client_name :label]]
                                            :connection-id "flows-db"
                                            :from          [{:select [:client_name] :from [[:flow_history :uu9]] :group-by [1]}]
                                            :group-by      [:client_name]
                                            :order-by      [:client_name]}}}
              dropdown2 [:dropdown
                         {:choices     [{:id 1 :label "error"} {:id 0 :label "success"} {:id -1 :label "timeout"}]
                          :width       "200px"
                          :placeholder "(all return statuses)"
                          :style       {:padding-top   "8px"
                                        :border-radius "8px" ;:background-color "#ffffff" :color
                                        :color         "#ffffff75"
                                        :outline       (str "1px solid " (theme-pull :theme/editor-outer-rim-color nil) 33)}
                          :model       :virtual-panel/return_status
                          :field       :return_status}]
              waffle1
                {:view    [:nivo-waffle-chart
                           {:total        10000 ;:waffle-totals10000 ;
                            :rows         2
                            :columns      12
                            :colors       (vec (reverse (for [e (range 25)]
                                                          (str (theme-pull :theme/editor-outer-rim-color nil) (- 99 (* e 3))))))
                            :padding      1
                            :borderRadius 1
                            :width        700
                            :theme        :theme/nivo-defaults
                            :click        {:c :waffle-hour}
                            :height       50
                            :margin       {:top 5 :right 5 :bottom 0 :left 5}
                            :data         :waffle-hour ;;[{:id "Hey1" :value 23} {:id
                           }]
                 :queries {:waffle-hour   {:select        [[[:cast [:substr :start_ts 11 3] :text] :id] [[[:count 1]] :value]]
                                           :connection-id "flows-db"
                                           :from          [[:fn_history :cc393]]
                                           :group-by      [1]}
                           :waffle-totals {:select [[:sum :value] :summy]
                                           :from   [{:select        [[[:cast [:substr :start_ts 11 3] :text] :id]
                                                                     [[[:count 1]] :value]]
                                                     :connection-id "flows-db"
                                                     :from          [[:fn_history :cc393]]
                                                     :group-by      [1]}]}}}
              viz1 {:queries {:flow-history-calendar-sys* {:select        [[[[:count [:distinct :run_id]]] :value]
                                                                           [[:substr :start_ts 0 11] :day]]
                                                           :connection-id "flows-db"
                                                           :from          [[:flow_history :cc393]]
                                                           :where         [:and
                                                                           (if caller [:= :client_name (str caller)] [:= 1 1]) ;; since
                                                                           (if status
                                                                             [:= :in_error :virtual-panel/return_status]
                                                                             [:= 1 1])]
                                                           :group-by      [[:substr :start_ts 0 11]]}}
                    :view    [:nivo-calendar
                              {:labelTextColor   "#ffffff" ;"#423939"
                               :emptyColor       "#00000000"
                               :axisLeft         {:tickRotation 0 :legendPosition "middle" :legendOffset -65}
                               :dayBorderColor   "#ffffff10"
                               :enableLabel      false
                               :motionDamping    10
                               :axisBottom       {:tickRotation 0 :legendPosition "middle" :legendOffset 40}
                               :inner-padding    0
                               :width            (- dyn-width 30)
                               :monthBorderColor "#ffffff15"
                               :colors           (vec (reverse (for [e (range 25)]
                                                                 (str (theme-pull :theme/editor-outer-rim-color nil)
                                                                      (- 99 (* e 3))))))
                               :theme            :theme/nivo-defaults
                               :click            {:x :flow-day :y :value}
                               :padding          0.1
                               :enableGridX      true
                               :border-radius    2
                               :enableGridY      true
                               :height           245
                               :margin           {:top 0 :right 5 :bottom 20 :left 25}
                               :data             :flow-history-calendar-sys*}]}
              vselected? @(ut/tracked-subscribe [::conn/clicked-parameter-key [:virtual-panel/flow-day]])
              grid-menu {:select        [:flow_id [[:count [:distinct :run_id]] :runs]]
                         :connection-id "flows-db"
                         :col-widths    {:runs 45 :flow_id 220}
                         :group-by      [1]
                         :where         [:and (if vselected? [:= [:substr :start_ts 0 11] :virtual-panel/flow-day] [:= 1 1])
                                         (if caller [:= :client_name (str caller)] [:= 1 1])
                                         (if status [:= :in_error :virtual-panel/return_status] [:= 1 1])]
                         :order-by      [[1 :asc]]
                         :from          [[:flow_history :tt336aa]]}
              gm-kw "grid-menu-sys*" ;; (str "kick-" (hash grid-menu) "-sys*")
              selected? @(ut/tracked-subscribe [::conn/clicked-parameter-key [(keyword (str gm-kw "/flow_id"))]])
              grid1 {:select [:flow_id :run_id :start_ts [:elapsed_seconds :seconds] :human_elapsed
                              [[:case [:= "nil" :overrides] "No" :else "Yes"] :overrides]
                              [[:case [:= :in_error 1] "error" :else "success"] :result]]
                     :connection-id "flows-db"
                     :group-by [1 2]
                     :style-rules {[:* :highlight-8369aaa1] {:logic [:= :in_error 1]
                                                             :style {;:background-color "#FF000021"
                                                                     :opacity    0.4
                                                                     :font-style "italic"}}}
                     :col-widths {:flow_id 280 :seconds 80 :overrides 70 :run_id 70 :human_elapsed 140 :result 65 :start_ts 145}
                     :where [:and (if selected? [:= :flow_id (keyword (str gm-kw "/flow_id"))] [:= 1 1])
                             (if vselected? [:= [:substr :start_ts 0 11] :virtual-panel/flow-day] [:= 1 1])
                             (if caller [:= :client_name (str caller)] [:= 1 1]) ;; since we
                             (if status [:= :in_error :virtual-panel/return_status] [:= 1 1])]
                     :order-by [[:start_ts :desc]]
                     :from [[:flow_history :tt336a]]}
              callout1 {:select        [[[:count [:distinct :run_id]] :cnt]]
                        :connection-id "flows-db"
                        :where         [:and (if selected? [:= :flow_id (keyword (str gm-kw "/flow_id"))] [:= 1 1])
                                        (if vselected? [:= [:substr :start_ts 0 11] :virtual-panel/flow-day] [:= 1 1])
                                        (if caller [:= :client_name (str caller)] [:= 1 1]) ;; since we
                                        (if status [:= :in_error :virtual-panel/return_status] [:= 1 1])]
                        :from          [[:flow_history :tt336a]]}
              grid-kw "grid1-sys*" ;;(str "kick-" (hash grid1) "-sys*")
              selected-run-id @(ut/tracked-subscribe [::conn/clicked-parameter-key [(keyword (str grid-kw "/run_id"))]])
              selected-start-ts @(ut/tracked-subscribe [::conn/clicked-parameter-key [(keyword (str grid-kw "/start_ts"))]])
              run-selected? (not (nil? selected-run-id))
              grid2 {:select        [:*]
                     :connection-id "flows-db"
                     :col-widths    {:value 500}
                     :where         [:and (if run-selected? [:= :run_id (keyword (str grid-kw "/run_id"))] [:= 1 0])]
                     :order-by      [[:start_ts :desc]]
                     :from          [[:fn_history :tt87336]]}
              grid3 {:select        [:*]
                     :connection-id "flows-db"
                     :col-widths    {:value 500}
                     :where         (if run-selected? [:= :run_id (keyword (str grid-kw "/run_id"))] [:= 1 0])
                     :order-by      [[:start_ts :desc]]
                     :from          [[:channel_history :tt87336]]}
              grid2-kw "grid2-sys*" ;; (str "kick-" (hash grid2) "-sys*")
             ]
          (when (and (not= @last-loaded-run-id selected-run-id) (not (nil? selected-run-id)))
            (reset! last-loaded-run-id selected-run-id)
            (ut/tracked-dispatch [::http/load-flow-history selected-run-id selected-start-ts]))
          [re-com/v-box :children
           [[re-com/h-box :size "auto" :height "60px" :width "750px" :justify :between :align :center :gap "40px" :style
             {:font-size "15px"} :children
             [;[re-com/box :child [buffy/render-honey-comb-fragments dropdown1 5 2 true] :width
              [buffy/render-honey-comb-fragments dropdown1 5 2 "dropdown1-sys*"]
              [buffy/render-honey-comb-fragments dropdown2 4 2 "dropdown2-sys*"] [re-com/box :child ""]]]
            [re-com/box :size "none" :height "255px" :style {:font-size "15px"} :child
             [buffy/render-honey-comb-fragments viz1 (/ dyn-width 50) 24 "viz1-sys*"]]
            [re-com/h-box :children
             [[re-com/box :size "none" :align :center :justify :center :style {:font-size "15px"} :child
               [buffy/render-honey-comb-fragments grid-menu (* (/ dyn-width 50) 0.25) 10 "grid-menu-sys*"]]
              [re-com/box :size "none" :align :center :justify :center :style {:font-size "15px"} :child
               [buffy/render-honey-comb-fragments grid1 (* (/ dyn-width 50) 0.75) 10 "grid1-sys*"]]]]
            [buffy/render-honey-comb-fragments
             [:h-box :size "auto" :justify :between :align :center :gap "10px" :padding "8px" :style
              {:font-size "19px" :opacity 0.33 :font-weight 700} :children
              [;"filters: "
               [:string :virtual-panel/flow-day] [:string [(keyword (str gm-kw "/flow_id")) " "]] caller
               [:string [(keyword (str grid-kw "/run_id")) " "]]]] (- (/ dyn-width 50) 1) 6 "filter-row-sys*"]
            [re-com/gap :size "10px"]
            (when run-selected?
              [flow-details-block-container "flow block results log*" :system :system
               [re-com/box :size "none" :align :center :justify :center :style {:font-size "15px"} :child
                [buffy/render-honey-comb-fragments grid2 (- (/ dyn-width 50) 1) (- (/ panel-height-bricks 2) 1) ;; 10
                 "grid2-sys*"]] ; flow-id orderb true 570 nil ;366
               "zmdi-dns"]) (when run-selected? [re-com/gap :size "10px"])
            (when run-selected?
              [flow-details-block-container "flow channel results log*" :system :system
               [re-com/box :size "none" :align :center :justify :center :style {:font-size "15px"} :child
                [buffy/render-honey-comb-fragments grid3 (- (/ dyn-width 50) 1) (- (/ panel-height-bricks 2) 1) ;;10
                 "grid3-sys*"]] ; flow-id orderb true 570 nil ;366
               "zmdi-dns"]) [re-com/gap :size "10px"]
            (let [pval @(ut/tracked-subscribe [::conn/clicked-parameter-key [(keyword (str grid2-kw "/value"))]])
                  blk  @(ut/tracked-subscribe [::conn/clicked-parameter-key [(keyword (str grid2-kw "/block"))]])
                  flw  @(ut/tracked-subscribe [::conn/clicked-parameter-key [(keyword (str grid2-kw "/flow_id"))]])
                  pval (try (edn/read-string pval) (catch :default _ (pr-str pval)))]
              (when pval
                [re-com/box :height "300px" :size "none" :style
                 {;:border "1px solid white"
                  :overflow "auto"} :child
                 (when pval
                   (if (or (map? pval) (vector? pval))
                     [map-boxes2 pval blk (str flw " / " blk) [] :output (if (vector? pval) "vector" "map") true]
                     [re-com/box :child
                      (pr-str @(ut/tracked-subscribe [::conn/clicked-parameter-key [(keyword (str grid2-kw "/value"))]]))]))]))]])
      (= ttype :flow-browser) [re-com/h-box :children
                               [[re-com/box :size "auto" :child [bricks/magic-table :flow-list* [:flows-sys] 12 19 []]]]]
      (= ttype :part-browser) [re-com/h-box :children
                               [[re-com/box :size "auto" :child ;[bricks/magic-table
                                                                ;:system-connections-list*
                                                                ;[:connections-sys] 3 9
                                 [bricks/magic-table :flow-cat-list* [:flow-fn-categories-sys] 3 19 []]]
                                [re-com/box :size "auto" :child
                                 [bricks/magic-table :flow-fn-list* [:flow-fn-sys] 9 19 [:full_map :category :file_path]]]]]
      (= ttype :debug) (if flow-select
                         [re-com/box :child
                          [(if read-only-flow? code-box code-box-rw) 600 440 (get flowmaps flow-select) flow-select] :width
                          "600px" :height "430px" :size "1"]
                         [code-box 600 450 flowmaps nil])
      (= ttype :scheduler)
        (let [grid1       {:select        [:flow_id :override :schedule :ts]
                           :connection-id "flows-db"
                           :order-by      [[:ts :desc]]
                           :from          [[:live_schedules :tt336]]}
              vec2choices (fn [x] (vec (for [i x] {:label (str i) :id i})))
              comps       (keys flowmaps)
              flow-names  (vec (distinct (map :flow_id @(ut/tracked-subscribe [::conn/sql-data [:flows-sys]]))))
              ddown       (fn [x xdef w kkey] [re-com/box :size "none" :align :center :width (px (or w 100)) :child
                                               [re-com/single-dropdown :width (px (or w 100)) :style
                                                {:border    (str "1px solid " (theme-pull :theme/editor-outer-rim-color nil) 45)
                                                 :color     "inherit"
                                                 :focus     {:outline "none"}
                                                 :font-size "18px"} :parts {:chosen-drop {:style {:color "green"}}} :choices
                                                (vec2choices x) :on-change #(swap! scheduler-atom assoc-in [flow-id kkey] %)
                                                :model (get-in @scheduler-atom [flow-id kkey] xdef)]])
              date-input  (fn [input-regex w dd kkey]
                            [re-com/input-text :src (at) :model (get-in @scheduler-atom [flow-id kkey] dd) :width (px (or w 90))
                             :on-change #(swap! scheduler-atom assoc-in [flow-id kkey] %) :change-on-blur? true :style
                             {;:font-size "20px" :margin-top "28px"
                              :opacity          (if (nil? (get-in @scheduler-atom [flow-id kkey])) 0.4 1.0)
                              :font-size        "16px"
                              :border           (str "2px solid " (theme-pull :theme/editor-outer-rim-color nil) 45)
                              :color            "inherit"
                              :height           "36px"
                              :background-color "#00000000"}])]
          [re-com/v-box :align :center :justify :center :children
           [[re-com/h-box :size "none" :width "595px" :padding "10px" :height "55px" :align :center :gap "6px" :style
             {:color (theme-pull :theme/editor-outer-rim-color nil) :margin-top "15px" :font-size "20px"} :children
             [[re-com/md-icon-button :src (at) :md-icon-name "zmdi-timer" :style {:font-size "26px"}]
              [re-com/box :child "every" :style {:opacity (if (nil? (get-in @scheduler-atom [flow-id :every])) 0.4 1.0)}]
              [date-input #"^\d{2}$" 45 "10" :every] [ddown ["seconds" "minutes" "hours" "days"] "hours" 110 :period]
              [re-com/box :child "starting" :style {:opacity (if (nil? (get-in @scheduler-atom [flow-id :starting])) 0.4 1.0)}]
              [date-input #"^\d{0,4}-?\d{0,2}-?\d{0,2}$" 130 (ut/today) :starting]
              [re-com/box :child "at" :style {:opacity (if (nil? (get-in @scheduler-atom [flow-id :at])) 0.4 1.0)}]
              [date-input #"^\d{4}$" 70 "1500" :at]]]
            [re-com/h-box :size "none" :width "595px" :padding "10px" :height "44px" :align :center :gap "6px" :style
             {:color (theme-pull :theme/editor-outer-rim-color nil) :opacity 0.2 :font-size "20px"} :children
             [[re-com/md-icon-button :src (at) :md-icon-name "zmdi-alarm-plus" :style {:font-size "26px"}]
              [re-com/box :child "when"] [ddown flow-names (first flow-names) 233 :trigger-flow-id] [re-com/box :child "is"]
              [ddown [:= :> :>= :<= :< :<>] := 60 :trigger-operator] [date-input #"^\d{4}$" 145 "<some-val>" :trigger-value]]]
            [re-com/h-box :size "none" :width "595px" :padding "10px" :height "44px" :align :center :gap "6px" :style
             {:color (theme-pull :theme/editor-outer-rim-color nil) :font-size "20px"} :children
             [[re-com/md-icon-button :src (at) :md-icon-name "zmdi-hearing" :style {:font-size "26px"}]
              [re-com/box :child "called with "] [date-input #"." 158 nil :trigger-words] [re-com/box :child "sent to"]
              [ddown comps (first comps) 180 :trigger-word-insert]]]
            [re-com/h-box :justify :between ;:align :center
             :padding "10px" :children
             [[re-com/box :size "auto" :padding "6px" :style {:border "0px dashed lime" :opacity 0.22} :child
               (str @scheduler-atom)] [re-com/gap :size "20px"]
              [re-com/h-box :gap "14px" :children
               (for [b ["+" "schedule!"]]
                 [re-com/box :padding "5px" :align :center :justify :center :min-width "30px" :height "36px" :attr
                  (if (= b "schedule!")
                    {:on-click #(do (ut/tracked-dispatch [::wfx/request :default
                                                          {:message     (merge (get @scheduler-atom flow-id)
                                                                               {:kind        :schedule-flow
                                                                                :flow-id     flow-id
                                                                                :client-name @(ut/tracked-subscribe
                                                                                                [::bricks/client-name])})
                                                           :on-response [::simple-response]
                                                           :on-timeout  [::timeout-response :run-flow [:schedule flow-id]] ;; requeue?
                                                           :timeout     15000000}])
                                    (swap! scheduler-atom dissoc flow-id))}
                    {}) :child (str b) :style
                  {:border           (str "2px solid " (theme-pull :theme/editor-outer-rim-color nil) 45)
                   :filter           "brightness(150%)"
                   :cursor           "pointer"
                   :background-color (str (theme-pull :theme/editor-outer-rim-color nil) 11)
                   :font-size        "18px"
                   :color            (theme-pull :theme/editor-outer-rim-color nil)}])]] :height "58px" :width "565px" :size
             "none"]
            [re-com/box :size "auto" :style {:margin-left "18px" :margin-top "15px"} :child
             [buffy/render-honey-comb-fragments grid1 12 5]]]])
      :else [re-com/box :child (str "unknown editor mode: " @editor-mode)])))

(defn server-flows
  [hh]
  (let [ss @(ut/tracked-sub ::bricks/flow-statuses {})
        ss (vec (sort-by first (for [[k v] ss] [k v])))] ;; since maps wont keep key order in
    [re-com/box :height (px hh) :style {:overflow "auto"} :child
     [re-com/v-box :padding "3px" :style
      {;:border "1px dashed white"
       :color (theme-pull :theme/editor-outer-rim-color nil)} :gap "11px" :children
      (for [[fid v] ss ;; could use key destructuring, but some keys are weird. meh, its fine w
            :let    [;time-running (get v :*time-running)
                     open-channels  (get v :channels-open?)
                     channels       (get v :channels-open)
                     started-by     (get v :*started-by)
                     process?       (get v :process?)
                     command        (get v :command)
                     since-start    (get v :since-start)
                     running-blocks (get v :running-blocks)
                     running?       (get v :*running?)]]
        [re-com/v-box :style
         {:border           (str "1px solid " (theme-pull :theme/editor-outer-rim-color nil) (if running? 77 22))
          :background-color (when running? (str (theme-pull :theme/editor-outer-rim-color nil) 15))
          :filter           (when running? "brightness(200%)")} :children
         [[re-com/h-box :padding "4px" :height "35px" :align :center :justify :between :children
           [[re-com/box :child
             (if (cstr/includes? (str fid) "/")
               (let [spl    (ut/splitter (str fid) #"/")
                     parent (first spl)
                     sub    (last spl)]
                 [re-com/v-box :children
                  [[re-com/box :child (str "(sub-flow called from) " parent) :style {:font-size "9px"}]
                   [re-com/box :child (str sub)]]])
               (str fid)) :width "50%" :style {:padding-left "4px" :font-size "15px" :font-weight 700}]
            [re-com/box :child
             (cond running?      (str "running " (cstr/join " " running-blocks) " (" channels " chans) ")
                   open-channels (str "idling (" channels " chans)")
                   :else         "stopped") :width "34%"]
            (if (not process?)
              [re-com/md-icon-button :style {:font-size "20px"} :on-click
               #(let [;flowmap @(ut/tracked-subscribe [::flowmap])
                      client-name          @(ut/tracked-sub ::conn/client-name {})
                      flowmap              @(ut/tracked-subscribe [::flowmap])
                      flow-id              @(ut/tracked-subscribe [::selected-flow])
                      flowmaps-connections @(ut/tracked-subscribe [::flowmap-connections])
                      server-flowmap       (process-flowmap2 flowmap flowmaps-connections flow-id)
                      comps                (get server-flowmap :components) ;; (assoc (get
                      running-view-subs    (vec (for [[k v] comps
                                                      :when (get v :view)]
                                                  [flow-id (keyword (str (ut/replacer (str k) ":" "") "-vw"))]))
                      running-subs         (vec (for [k (keys comps)] [flow-id k]))
                      running-subs         (vec (into running-subs running-view-subs))]
                  (ut/tracked-dispatch [::http/load-flow-history fid nil])
                  (ut/tracked-dispatch [::add-live-flow-subs running-subs])
                  (ut/tracked-dispatch [::wfx/request :default
                                        {:message {:kind        :sub-to-running-values
                                                   :flow-id     fid
                                                   :flow-keys   [] ;;running-subs
                                                   :client-name client-name}
                                         :timeout 15000000}])) :md-icon-name "zmdi-open-in-browser"]
              [re-com/gap :size "20px"])
            [re-com/md-icon-button :style {:font-size "20px"} :on-click
             #(ut/tracked-dispatch [::wfx/request :default
                                    {:message {:kind        :kill-flow
                                               :flow-id     fid
                                               :process?    process?
                                               :client-name @(ut/tracked-subscribe [::bricks/client-name])}
                                     :timeout 15000000}]) :md-icon-name "zmdi-stop"]]]
          [re-com/h-box :padding "4px" :height "35px" :align :center :style {:font-size "10px" :padding-right "20px"} :justify
           :between :children
           [[re-com/box :child (if process? (str "(external process) " command) (str "started by: " started-by)) :style
             {:padding-left "4px" :font-weight 300}] (when (not process?) [re-com/box :child (str since-start)])]]]])]]))

(re-frame/reg-sub ::opts-map
                  (fn [db _]
                    (get-in db [:flows (get db :selected-flow) :opts] {:retry-on-error? false :retries 5 :close-on-done? true})))

(declare gantt-container)



(defn flow-editor
  [w h]
  (let [react-hack [@editor-mode @trig-atom-test @db/flow-editor-system-mode]
        sql-params (into {}
                         (for [k [:flow-fn-categories-sys/category]]
                           {k @(ut/tracked-subscribe [::conn/clicked-parameter-key [k]])}))
        flow-id    @(ut/tracked-subscribe [::selected-flow])
        dyn-width  (last @db/flow-editor-system-mode)
        o-modes    [["flows running" 800] ["flow browser" 600] ["flow parts" 600] ["metrics" 990] ["KPIs" 990] ["signals" 990]
                    ["rules" 990] ["flow history" 1200]]
        sql-calls  {:flow-fn-categories-sys {:select [:category] :from [:flow_functions] :group-by [1]}
                    :flow-fn-all-sys        {:select [:name] :from [:flow_functions] :group-by [1]}
                    :flow-fn-sys            {:select     [:name :description :full_map :inputs :icon :input_types :output_types
                                                          :category]
                                             :from       [:flow_functions]
                                             :col-widths {:name 215 :description 500}
                                             :where      (if (get sql-params :flow-fn-categories-sys/category)
                                                           [:= :category (str (get sql-params :flow-fn-categories-sys/category))]
                                                           [:= 1 1])}
                    :flows-sys              {:select        [:flow_id :file_path :last_modified]
                                             :from          [:flows]
                                             :connection-id "flows-db"
                                             :order-by      [[3 :desc]]}}]
    (dorun
      (for [[k v] sql-calls]
        (let [query        (ut/postwalk-replacer sql-params v)
              ;data-exists? @(ut/tracked-subscribe [::conn/sql-data-exists? [k]])
              ;unrun-sql?   @(ut/tracked-subscribe [::conn/sql-query-not-run? [k] query])
              data-exists?   @(ut/tracked-sub ::conn/sql-data-exists-alpha? {:keypath [k]})
              unrun-sql?     @(ut/tracked-sub ::conn/sql-query-not-run-alpha? {:keypath [k] :query query})]
          (when (or (not data-exists?) unrun-sql?)
            (if (get query :connection-id) (conn/sql-data [k] query (get query :connection-id)) (conn/sql-data [k] query))))))
    [re-com/v-box :size "none" :width (px w) :height (px h) :gap "10px" :style
     {;:border-right (str "6px solid " (theme-pull :theme/editor-outer-rim-color nil))
      :border-top       (str "6px solid " (theme-pull :theme/editor-outer-rim-color nil))
      :position         "fixed"
      :transition       "width 0.3s"
      :left             0 ;:top 0
      :overflow         "auto"
      :font-family      (theme-pull :theme/base-font nil)
      :color            (theme-pull :theme/editor-font-color nil)
      :backdrop-filter  "blur(5px)"
      :background-color "#00000022"} :align :center :justify :start :children
     [[re-com/h-box :size "none" :height "40px" :width (px w) :align :start :justify :between :style
       {:padding-top  "5px"
        :transition   "width 0.3s"
        :color        (theme-pull :theme/editor-outer-rim-color nil)
        :font-size    "16px"
        :font-weight  700
        :padding-left "12px"} :children
       [[re-com/h-box :align :center :justify :center :style {:font-size "14px"} :size "auto" :children
         (for [o    o-modes
               :let [selected? (= o @db/flow-editor-system-mode)]]
           [re-com/box :height "50px" :align :center :justify :center :size "auto" :attr
            (when (not selected?) {:on-click #(reset! db/flow-editor-system-mode o)}) :style
            {;:border "1px solid white"
             :cursor           "pointer" ;; (when (not selected?) "pointer")
             :text-decoration  (when selected? "underline")
             :background-color (if selected? (str (theme-pull :theme/editor-outer-rim-color nil) 55) "#00000000")} :child
            (str (first o))])] [re-com/gap :size "10px"]]] [re-com/gap :size "8px"]
      (cond (= (first @db/flow-editor-system-mode) "flows running") [flow-details-block-container "server flow statuses" :system
                                                                     :system
                                                                     [re-com/box :height (px (* h 0.6)) :child
                                                                      [server-flows (* h 0.6)]] ; flow-id
                                                                                                ; orderb
                                                                                                ; true
                                                                                                ; 570
                                                                                                ; nil
                                                                                                ; ;366
                                                                     "zmdi-dns"]
            (= (first @db/flow-editor-system-mode) "flow browser")  [flow-details-block-container
                                                                     (first @db/flow-editor-system-mode) :system :system
                                                                     [settings-block flow-id :flow-browser] "zmdi-chart-donut"]
            (= (first @db/flow-editor-system-mode) "flow parts")    [flow-details-block-container
                                                                     (first @db/flow-editor-system-mode) :system :system
                                                                     [settings-block flow-id :part-browser] "zmdi-chart-donut"])
      (cond (some #(= (first @db/flow-editor-system-mode) %) ["flow browser" "flow parts"])
              [flow-details-block-container "server flow statuses" :system :system [server-flows 240] ; flow-id
                                                                                                      ; orderb
                                                                                                      ; true
                                                                                                      ; 570
                                                                                                      ; nil
                                                                                                      ; ;366
               "zmdi-dns"]
            (= (first @db/flow-editor-system-mode) "scheduler") [flow-details-block-container (first @db/flow-editor-system-mode)
                                                                 :system :system [settings-block flow-id :scheduler]
                                                                 "zmdi-chart-donut"]
            (= (first @db/flow-editor-system-mode) "signals") [sig/signals-panel]
            (= (first @db/flow-editor-system-mode) "flow history") [flow-details-block-container
                                                                    (first @db/flow-editor-system-mode) :system :system
                                                                    [settings-block flow-id :run-history] "zmdi-chart-donut"]
            (= (first @db/flow-editor-system-mode) "flows running")
              [re-com/v-box :children
               [(let [qq1 {:queries {:gen-viz-192 {:select   [[[[:sum :threads]] :threads] :ts]
                                                   :from     [{:select   [[:thread_count :threads] :ts [:used_memory_mb :used_mb]
                                                                          [:ws_peers :clients] :sys_load]
                                                               :limit    45
                                                               :order-by [[:ts :desc]]
                                                               :from     [[:jvm_stats :rr70]]}]
                                                   :group-by [:ts]}}
                           :view    [:> :ResponsiveContainer {:width (/ dyn-width 2) :height 200}
                                     [:> :AreaChart {:data :gen-viz-192 :margin {:top 5 :bottom 25 :right 30 :left 20}}
                                      [:> :CartesianGrid {:strokeDasharray "1 5" :opacity 0.1}] [:> :Tooltip] ;[:> :XAxis
                                                                                                              ;{:dataKey :ts}]
                                      [:> :Area
                                       {:type              "monotone"
                                        :dataKey           :threads
                                        :isAnimationActive false
                                        :stroke            (str (theme-pull :theme/editor-outer-rim-color nil)) ;;"#8884d8"
                                        :fill              (str (theme-pull :theme/editor-outer-rim-color nil) 55) ;;"#8884d8"
                                        :activeDot         {:r 8}}]]]}
                      qq2 {:queries {:gen-viz-545 {:select   [[[[:sum :used_mb]] :threads] :ts]
                                                   :from     [{:select   [[:thread_count :threads] :ts [:used_memory_mb :used_mb]
                                                                          [:ws_peers :clients] :sys_load]
                                                               :limit    45
                                                               :order-by [[:ts :desc]]
                                                               :from     [[:jvm_stats :rr70]]}]
                                                   :group-by [:ts]}}
                           :view    [:> :ResponsiveContainer {:width (/ dyn-width 2) :height 200}
                                     [:> :BarChart {:data :gen-viz-545 :margin {:top 5 :bottom 25 :right 30 :left 20}}
                                      [:> :CartesianGrid {:strokeDasharray "1 5" :opacity 0.1}] [:> :Tooltip] ;[:> :XAxis
                                                                                                              ;{:dataKey :ts}]
                                      [:> :Bar
                                       {:dataKey           :threads
                                        :isAnimationActive false
                                        :stroke            (str (theme-pull :theme/editor-outer-rim-color nil)) ;;"#8884d8"
                                        :fill              (str (theme-pull :theme/editor-outer-rim-color nil) 55) ;;"#8884d8"
                                       }]]]}]
                  [re-com/h-box :children
                   [[re-com/v-box :width "50%" :children
                     [[re-com/box :child "threads" :align :center :justify :center :size "auto" :style {:opacity 0.7}]
                      [re-com/box :child [buffy/render-honey-comb-fragments qq1 (/ dyn-width 50) 2]]]]
                    [re-com/v-box :width "50%" :children
                     [[re-com/box :child "memory" :align :center :justify :center :size "auto" :style {:opacity 0.7}]
                      [re-com/box :child [buffy/render-honey-comb-fragments qq2 (/ dyn-width 50) 2]]]]]])
                [re-com/box :style {:padding-left "12px"} :child
                 (let [qq {:select        [:ts [:thread_count :threads] [:used_memory_mb :used_mb] :sys_load [:ws_peers :clients]]
                           :connection-id "system-db"
                           :limit         1
                           :order-by      [[:ts :desc]]
                           :from          [[:jvm_stats :rr70]]}]
                   [buffy/render-honey-comb-fragments qq (/ dyn-width 50) 3])]]]
            :else [re-com/box :child "oops"])]]))

(defn waffles
  [data w]
  (let [data                  (into []
                                    (for [[k v] @(ut/tracked-subscribe [::flowmap])]
                                      {:name k :type (get-in v [:ports :out :out]) :number 1}))
        _ (ut/tapp>> [:waffled-data data])
        square-size           20
        max-width             w
        num-squares-max-width (quot max-width square-size)
        data-length           (count data)
        num-squares-height    (int (Math/ceil (/ data-length num-squares-max-width.toDouble)))
        chart-width           (if (< data-length num-squares-max-width) (* data-length square-size) max-width)
        chart-data            (map-indexed (fn [i {:keys [name type number]}]
                                             {:row     (quot i num-squares-max-width)
                                              :col     (mod i num-squares-max-width)
                                              :filled  type
                                              :number  number
                                              :tooltip name})
                                           data)
        ccolors               (theme-pull :theme/data-colors db/data-colors)
        _ (ut/tapp>> [:waffle-chart-data chart-data])]
    [:vega-lite
     {:width      chart-width
      :height     (* square-size num-squares-height)
      :layer      [; modify chart to use layer
                   {:data     {:values chart-data}
                    :mark     "rect"
                    :encoding {:y       {:field "row" :type "ordinal" :scale {:rangeStep square-size} :axis nil}
                               :x       {:field "col" :type "ordinal" :scale {:rangeStep square-size} :axis nil}
                               :fill    {:field  "filled"
                                         :type   "nominal"
                                         :scale  {:domain (keys ccolors) :range (vals ccolors)}
                                         :legend nil}
                               :tooltip {:field "tooltip" :type "nominal"}}}
                   {:data     {:values chart-data}
                    :mark     {:type "text" :font (theme-pull :theme/base-font nil) :fontSize 9}
                    :encoding {:color   {:value "black"}
                               :opacity {:value 0.4}
                               :y       {:field "row" :type "ordinal" :scale {:rangeStep square-size}}
                               :x       {:field "col" :type "ordinal" :scale {:rangeStep square-size}}
                               :text    {:field "number" :type "quantitative" :condition {:test "datum.number <= 1" :value ""}}}}]
      :background "transparent"
      :config     {:view {:stroke "transparent"}}} {:actions false}]))

(re-frame/reg-sub ::get-raw-port-types-set
                  (fn [db [_ flow-id bid dir pid]]
                    (let [vv (get-in db [:flows flow-id :map bid :ports dir pid] :any)] (set (if (keyword? vv) [vv] vv)))))

(re-frame/reg-event-db ::set-raw-port-types
                       (fn [db [_ flow-id bid dir pid types]]
                         (let [types (vec types) ;;(vec (remove #(= % :none) (remove empty? (vec
                                                 ;;types))))
                               types (if (= (count types) 1) (first types) types)]
                           (ut/tapp>> [:set-port-type [_ flow-id bid dir pid] :to types])
                           (assoc-in db [:flows flow-id :map bid :ports dir pid] types))))

(re-frame/reg-sub ::get-incoming-port
                  (fn [_ [_ bid type vk]]
                    (let [connections @(ut/tracked-subscribe [::flowmap-connections-parsed bid])
                          conn-map    (group-by first connections)
                          conns       (for [[o1 o2 i1 i2] (map rest (get conn-map type))
                                            :when         (if (= type :in) (= vk i2) (= vk o2))]
                                        (if (= type :out) [i1 i2] [o1 o2]))]
                      (first conns))))

(re-frame/reg-sub ::get-outgoing-port
                  (fn [_ [_ bid type vk]]
                    (let [connections @(ut/tracked-subscribe [::flowmap-connections-parsed bid])
                          conn-map    (group-by first connections)
                          filled      (filter #(= % [bid vk]) (for [[b k _ _] (map rest (get conn-map type))] [b k]))]
                      filled)))




(defn index-of [coll item] (first (keep-indexed #(when (= %2 item) %1) coll)))

(defn swap
  [coll i j]
  (-> coll
      (assoc i (coll j))
      (assoc j (coll i))))

(re-frame/reg-event-db
  ::re-order-connection
  (fn [db [_ conn direction]] ;; direction = :up or :down
    (let [conn  (vec (map #(ut/replacer (str %) ":" "") conn))
          conn  [(keyword (str (first conn) (when (not= (second conn) "out") (str "/" (second conn)))))
                 (keyword (str (nth conn 2) "/" (nth conn 3)))] ;; item to match what is in
          conns (get-in db [:flows (get db :selected-flow) :connections])
          idx   (index-of conns conn)] ;; find the index of conn in conns
      (if (nil? idx)
        db
        (cond (= direction :up)   (when (> idx 0)
                                    (assoc-in db [:flows (get db :selected-flow) :connections] (swap conns idx (dec idx))))
              (= direction :down) (when (< idx (dec (count conns)))
                                    (assoc-in db [:flows (get db :selected-flow) :connections] (swap conns idx (inc idx))))
              :else               db)))))

(re-frame/reg-sub ::get-meta (fn [db [_ bid]] (get-in db [:flows (get db :selected-flow) :map bid :data :flow-item :meta])))

(re-frame/reg-sub ::is-open-in?
                  (fn [db [_ bid]]
                    (let [tt (get-in db [:flows (get db :selected-flow) :map bid :data :drag-meta :type])]
                      (true? (= tt :open-block)))))

(re-frame/reg-sub ::condi-ports (fn [db [_ bid]] (get-in db [:flows (get db :selected-flow) :map bid :cond] {})))

(re-frame/reg-sub ::push-ports (fn [db [_ bid]] (get-in db [:flows (get db :selected-flow) :map bid :push] {})))

(re-frame/reg-event-db ::add-push-port
                       (undoable)
                       (fn [db [_ bid]]
                         (let [fn    '(fn [out] (not (nil? out)))
                               ckeys (vec (keys (get-in db [:flows (get db :selected-flow) :map bid :push])))
                               name  (ut/safe-key :push-path ckeys)]
                           (assoc-in db [:flows (get db :selected-flow) :map bid :push name] {:fn fn :dest []}))))

(re-frame/reg-event-db ::remove-push-port
                       (undoable)
                       (fn [db [_ bid name]]
                         (-> db
                             (assoc-in [:flows (get db :selected-flow) :connections]
                                       (vec (remove #(cstr/ends-with? (str (first %)) (str "/" (ut/replacer (str name) ":" "")))
                                              (get-in db [:flows (get db :selected-flow) :connections]))))
                             (ut/dissoc-in [:flows (get db :selected-flow) :map bid :push name]))))

(re-frame/reg-event-db ::add-condi-port
                       (undoable)
                       (fn [db [_ bid]]
                         (let [fn    '(fn [out] (not (nil? out)))
                               ckeys (vec (keys (get-in db [:flows (get db :selected-flow) :map bid :cond])))
                               name  (ut/safe-key :cond-path ckeys)]
                           (assoc-in db [:flows (get db :selected-flow) :map bid :cond name] {:fn fn :dest []}))))

(re-frame/reg-event-db
  ::edit-condi-port
  (undoable)
  (fn [db [_ bid name fn]]
    (assoc-in db [:flows (get db :selected-flow) :map (get db :selected-flow-block) :cond name] {:fn fn :dest []})))

(re-frame/reg-event-db ::edit-view-fn
                       (undoable)
                       (fn [db [_ bid fn]]
                         (assoc-in db [:flows (get db :selected-flow) :map (get db :selected-flow-block) :view] fn)))

(re-frame/reg-event-db ::remove-condi-port
                       (undoable)
                       (fn [db [_ bid name]]
                         (-> db
                             (assoc-in [:flows (get db :selected-flow) :connections]
                                       (vec (remove #(cstr/ends-with? (str (first %)) (str "/" (ut/replacer (str name) ":" "")))
                                              (get-in db [:flows (get db :selected-flow) :connections]))))
                             (ut/dissoc-in [:flows (get db :selected-flow) :map bid :cond name]))))

(defn port-panel
  [flow-id bid fmap type]
  (let [;blocks @(ut/tracked-subscribe [::flowmap])
        reactions!  [@(ut/tracked-subscribe [::http/flow-results]) @sniffy-sniff @port-hover2]
        [xx yy]     @db/flow-detached-coords
        selected    (get-in @flow-details-block-container-atom [flow-id bid :port-panel type])
        connections @(ut/tracked-subscribe [::flowmap-connections-parsed bid])
        defaults    (when (= type :in) @(ut/tracked-subscribe [::input-defaults bid]))
        port-meta   (when (= type :in) @(ut/tracked-subscribe [::get-meta bid]))
        is-open-in? @(ut/tracked-subscribe [::is-open-in? bid])
        conn-map    (group-by first connections)
        inputs-vec  (try (edn/read-string (get-in fmap [:data :flow-item :inputs]))
                         (catch :default _ (get-in fmap [:data :flow-item :inputs])))]
    [re-com/v-box :gap "8px" :children
     [[re-com/v-box :style {:background "#00000055" :padding-top "8px" :padding-bottom "8px" :border-radius "8px"} :children
       (for [[e v] (get fmap :ports)
             :when (= type e)]
         [re-com/v-box :children
          (into
            [[re-com/h-box :justify :between :align :center :height "17px" :style
              {:margin-left "4px" :font-size "10px" :font-weight 300 :opacity 0.35} :children
              [[re-com/box :child "name" :padding "4px" :width "160px"]
               [re-com/box :child (if (= type :in) "allowed" "expected") :padding "4px" :width "124px"]
               [re-com/box :padding "4px" :width "203px" :child (if (= type :in) "coming from" "going to")]
               [re-com/box :child "actual" :width "60px" :padding "4px"]]]]
            (for [[vk vv] (if (= type :in)
                            (sort-map-keys (vec (or inputs-vec (sort-by str (keys v)))) v)
                            (sort-map-keys (try (sort-by str (keys v))
                                                (catch :default e
                                                  (do (ut/tapp>> [:fucking-sorting-error e :flows.cljs :ln 4216 (keys v)])
                                                      (keys v))))
                                           v))
                  :let    [select-vec          [bid vk]
                           selected?           (or (and @sniffy-sniff (= @port-hover2 bid) (= vk (get @sniffy-sniff 1)))
                                                   (= select-vec selected))
                           default             (get defaults vk) ;;(ut/postwalk-replacer
                           default?            (not (nil? default))
                           scrubber            (get-in port-meta [vk :scrubber])
                           desc                (get-in port-meta [vk :desc])
                           [ibid ivk]          (if (= type :in)
                                                 @(ut/tracked-subscribe [::get-incoming-port bid type vk])
                                                 [nil nil])
                           pcolor              @(ut/tracked-subscribe [::port-color flow-id (or ibid bid) (or ivk vk)])
                           ptype               @(ut/tracked-subscribe [::port-color flow-id (or ibid bid) (or ivk vk) :data-type])
                           pval1               @(ut/tracked-subscribe [::port-color flow-id (or ibid bid) (or ivk vk)
                                                                       :data-value])
                           vopen?              (true? (get-in @flow-details-block-container-atom
                                                              [flow-id bid :port-values type vk :open?]
                                                              false))
                           vttoggle            #(swap! flow-details-block-container-atom assoc-in
                                                  [flow-id bid :port-values type vk :open?]
                                                  (not vopen?))
                           conns               (for [[o1 o2 i1 i2] (map rest (get conn-map type))
                                                     :when         (if (= type :in) (= vk i2) (= vk o2))]
                                                 (if (= type :out) [i1 i2] [o1 o2]))
                           conn-list           (for [[a _] conns] a)
                           connected?          (not (empty? conns))
                           involved            (vec (distinct (conj conn-list bid)))
                           multi?              (cstr/ends-with? (str vk) "+")
                           hover-bundle-only   {:on-mouse-enter #(do (swap! flow-details-block-container-atom assoc-in
                                                                       [flow-id :involved]
                                                                       involved)
                                                                     (swap! flow-details-block-container-atom assoc-in
                                                                       [flow-id bid :port-panel type]
                                                                       select-vec))
                                                :on-mouse-leave #(do (swap! flow-details-block-container-atom assoc-in
                                                                       [flow-id :involved]
                                                                       [])
                                                                     (swap! flow-details-block-container-atom assoc-in
                                                                       [flow-id bid :port-panel type]
                                                                       nil))}
                           hover-bundle-clicks {:on-click        (when true ;(= type :in)
                                                                   vttoggle)
                                                :on-double-click (when (not (empty? conn-list))
                                                                   #(select-block (first conn-list)))}]]
              [re-com/v-box :attr hover-bundle-only :style
               {:border-radius "6px"
                :border        (if selected?
                                 (str "1px solid " pcolor) ;(theme-pull
                                 "1px solid transparent")} :children
               [[re-com/h-box :attr hover-bundle-clicks :style
                 {;:margin-left "4px" :margin-right "4px"
                  :border-radius    "6px" ;(when selected? "6px")
                  :cursor           "pointer"
                  :color            (if selected? "#ffffff" "#ffffff80")
                  :background-color (str pcolor (if (and (not connected?) (not selected?) (= type :out)) "18" "30"))} :align
                 :center :size "auto" :min-height "42px" :children
                 [[re-com/box :child (str vk) :padding "4px" :style
                   {:font-size   "17px"
                    :opacity     (if (and (not connected?) (not selected?) (= type :out)) 0.4 1.0)
                    :overflow    "hidden"
                    :margin-left "3px"
                    :font-weight 700} :width "160px"]
                  [re-com/box :child
                   (if true ;selected?
                     [re-com/tag-dropdown ;:width "95px"
                      :model (ut/tracked-subscribe [::get-raw-port-types-set flow-id bid type vk]) :parts
                      {;:selection-list {:style {:margin-top (px (+ (* -1 yy) -45))
                       :main            {:style {:background-color "#00000000" ;(str (theme-pull
                                                 :color            (theme-pull :theme/editor-font-color nil)
                                                 :border           (str "0px solid "
                                                                        (theme-pull :theme/editor-outer-rim-color nil))
                                                 :outline          "none"}}
                       :list-group-item {:style {:background-color (str (theme-pull :theme/editor-rim-color nil) "99")
                                                 :backdrop-filter  "blur(4px)"
                                                 :border           (str "1px solid "
                                                                        (theme-pull :theme/editor-outer-rim-color nil)
                                                                        "44")
                                                 :color            (theme-pull :theme/editor-font-color nil)}} ;; popover
                       :list-group      {:style {:margin-top       (px (+ (* -1 yy) -45))
                                                 :margin-left      (px (+ (* -1 xx) -20))
                                                 :background-color "#00000000"}}} :abbrev-fn
                      (fn [m] [re-com/box :child (subs (str (get m :label)) 1 2) :style {:font-weight 700 :color (get m :color)}])
                      :required? true :max-width "130px" :disabled? (not selected?) :label-fn
                      (fn [m] [re-com/box :child (str (get m :label)) :style {:font-weight 700 :color (get m :color)}]) :style
                      {:font-size   "12px"
                       :font-family (theme-pull :theme/base-font nil)
                       :color       (theme-pull :theme/editor-font-color nil)} :abbrev-threshold 10 :choices
                      (vec (for [[k v] (ut/sort-map-by-key (merge {"any" "#c32148"}
                                                                  (theme-pull :theme/data-colors db/data-colors)))]
                             {:id (keyword k) :label (str (keyword k)) :color (ut/choose-text-color v) :background-color v}))
                      :on-change #(ut/tracked-dispatch [::set-raw-port-types flow-id bid type vk %])]
                     (str vv)) :padding "4px" :width "130px"]
                  (if (not (empty? conns))
                    [re-com/v-box :padding "4px" :width "200px" :children (for [e conns] [re-com/box :child (str e)])]
                    (if default?
                      [re-com/box :size "none" :width "200px" :style
                       {;:margin-right "8px"
                        :font-size "9px"
                        :opacity   0.45} :child "(has a default value)"]
                      [re-com/box :width "200px" :child
                       [re-com/md-icon-button :style {:font-size "10px" :color pcolor :margin-right "8px"} :md-icon-name
                        "zmdi-close"]]))
                  [re-com/box :child (str ptype) :width "45px" :size "none" ;:height "24px"
                   :style {:font-size "9px" :color pcolor} :padding "2px"]
                  [re-com/v-box :width "20px" :size "none" :style {:margin-top "3px"} :children
                   [[re-com/md-icon-button :style {:font-size "13px" :color pcolor :margin-right "8px"} :md-icon-name
                     (cond default?       "zmdi-dot-circle-alt"
                           (empty? conns) "zmdi-circle-o"
                           :else          "zmdi-circle")]]]]]
                (when true ;(= type :in)
                  (for [conn (cond (and (= type :in) (empty? conns)) [nil]
                                   (= type :in)                      conns
                                   :else                             [(first conns)])]
                    (let [[b1 p1]        conn ;(first conns) ;; discarding others, since we can
                          pval           (if (= type :in) @(ut/tracked-subscribe [::port-color flow-id b1 p1 :data-value]) pval1)
                          c-line         (vec (into conn [bid vk]))
                          show-default?  (and (nil? pval)
                                              (empty? conn) ;conns ;(empty? (remove nil?
                                              default?)
                          show-scrubber? (and scrubber default? show-default?)
                          pval           (if show-default? default pval)]
                      (when vopen?
                        [re-com/v-box :children
                         [[re-com/h-box :children
                           [[re-com/box :width "535px" :style
                             {;:border-radius "9px"
                              :padding-top    "5px"
                              :opacity        (when show-default? 0.4)
                              :padding-bottom "5px"} :child
                             (let [pval (if (or (vector? pval) (map? pval) (list? pval)) (ut/replace-large-base64 pval) pval)
                                   b64? (ut/is-base64? (str pval))]
                               (if (and ;(try (not (empty? pval)) (catch :default _ false))
                                     b64?)
                                 [re-com/v-box :padding "16px" :size "auto" :gap "10px" :children
                                  [[re-com/box :child
                                    (str "(**base64 string: ~" (.toFixed (/ (* (count (str pval)) 6) 8 1024 1024) 2) " MB)")]
                                   [re-com/box :size "auto" :child [:img {:src (str "data:image/png;base64," pval)}]]]]
                                 [code-box 555 nil (pr-str pval)]))]
                            (when multi?
                              [re-com/v-box :width "31px" :align :center :justify :center :children
                               [[re-com/md-icon-button :attr {:on-click #(ut/tracked-dispatch [::re-order-connection c-line :up])}
                                 :style {:font-size "12px" :color (str (theme-pull :theme/editor-font-color nil) "75")}
                                 :md-icon-name "zmdi-chevron-up"]
                                [re-com/md-icon-button :attr
                                 {:on-click #(ut/tracked-dispatch [::re-order-connection c-line :down])} :style
                                 {:font-size "12px" :color (str (theme-pull :theme/editor-font-color nil) "75")} :md-icon-name
                                 "zmdi-chevron-down"]]])]]
                          (when show-scrubber? ;; scrubber?
                            [re-com/box :style {:padding-top "8px" :padding-bottom "8px"} :child
                             [bricks/scrubber-panel true @(ut/tracked-subscribe [::bricks/keypaths-in-flow bid]) [:flow bid] vk ;[:flow-item
                                                                                                                                ;:defaults
                                                                                                                                ;vk]
                                                                                                                                ;;;kk
                                                                                                                                ;pm
                              (if (map? scrubber)
                                {:fm true :opts scrubber :canvas? true :flow? true}
                                {:fm true :canvas? true :flow? true})]])
                          (when desc ;; :meta :desc for the port
                            [re-com/box :style
                             {:margin-left    "15px"
                              :font-weight    400
                              :color          (str (theme-pull :theme/editor-font-color nil) 55)
                              :padding-bottom "10px"} :width "535px" :child (str desc)])]]))))]]))])]
      (when (and (= type :out) (not is-open-in?)) ;; if we can have conditional ports!
        (let [condi-ports @(ut/tracked-subscribe [::condi-ports bid])
              cnt         (count (keys condi-ports))
              open?       (true? (get-in @flow-details-block-container-atom [flow-id bid :condi-paths :open?] false))]
          [re-com/v-box :style {:background "#00000055" :padding-top "8px" :border-radius "8px"} :children
           [[re-com/h-box :size "auto" :padding "4px" :justify :between :align :center :style
             {:color ;; (str (theme-pull :theme/editor-outer-rim-color nil) 88)
                (if open?
                  (str (theme-pull :theme/editor-outer-rim-color nil) 99)
                  (str (theme-pull :theme/editor-outer-rim-color nil) 45))
              :cursor "pointer"} :children
             [[re-com/h-box :align :center :justify :center :style {:padding-left "3px"} :gap "5px" ;:style
                                                                                                    ;{;:margin-left
                                                                                                    ;"-3px"
               :attr {:on-click #(swap! flow-details-block-container-atom assoc-in [flow-id bid :condi-paths :open?] (not open?))}
               :children
               [[re-com/md-icon-button :src (at) :md-icon-name "fa-solid fa-tree" ;; <i class=
                                                                                  ;; "fa-solid
                                                                                  ;; fa-bezier-curve"
                                                                                  ;; ></i>
                 :style
                 {;:color (theme-pull :theme/editor-outer-rim-color nil)
                  :font-size "17px"
                  :cursor    "pointer"}] [re-com/box :child (str "conditional paths (" cnt ")")]]]
              [re-com/md-icon-button :src (at) :on-click #(ut/tracked-dispatch [::add-condi-port bid]) :md-icon-name "zmdi-plus"
               :style
               {;:color (theme-pull :theme/editor-outer-rim-color nil)
                :font-size "22px"
                :cursor    "pointer"}]]] (when (and open? (> cnt 0)) [re-com/gap :size "8px"])
            (when (and open? (> cnt 0))
              [re-com/v-box :style
               {;:border "1px solid #ffffff33"
                :border-radius    "8px"
                :background-color "#00000044"} :gap "8px" :padding "4px" :children
               (for [[k {:keys [fn dest]}] condi-ports
                     :let                  [select-vec  [bid k]
                                            valve-open? @(ut/tracked-subscribe [::bricks/condi-valve-open? flow-id bid k])
                                            selected?   (or (and @sniffy-sniff (= @port-hover2 bid) (= k (get @sniffy-sniff 1)))
                                                            (= select-vec selected))
                                            pcolor      (theme-pull :theme/editor-outer-rim-color nil) ;; @(ut/tracked-subscribe
                                           ]]
                 [re-com/v-box :padding "4px" :attr
                  {:on-mouse-enter #(do (swap! flow-details-block-container-atom assoc-in
                                          [flow-id bid :port-panel type]
                                          select-vec))
                   :on-mouse-leave #(do (swap! flow-details-block-container-atom assoc-in [flow-id bid :port-panel type] nil))}
                  :style
                  {:border           (if (or selected? valve-open?)
                                       (str "1px solid " pcolor) ;(theme-pull
                                       (str "1px solid " pcolor 25))
                   :background-color (if (or selected? valve-open?) (str pcolor "22") (str pcolor "08"))
                   :border-radius    "6px"} :gap "8px" :children
                  [[re-com/h-box :size "auto" :justify :between :align :center :style
                    {:color         (str (theme-pull :theme/editor-outer-rim-color nil) 88)
                     :padding-left  "4px"
                     :padding-right "1px"
                     :padding-top   "4px"} :children
                    [[re-com/box :style {:font-size "15px" :font-weight 700} :child (str k)]
                     [re-com/h-box :width "300px" :height "100%" :justify :between :align :center :children
                      [[re-com/v-box :children (for [d dest] [re-com/box :child (str d)])]
                       [re-com/md-icon-button :src (at) :md-icon-name "zmdi-close" :on-click
                        #(ut/tracked-dispatch [::remove-condi-port bid k]) :style
                        {;:margin-top "-5px"
                         :cursor    "pointer"
                         :font-size "16px"}]]]]]
                   [re-com/h-box :justify :between :align :center :children ;;(str fn)
                    [;[code-box 500 nil fn]
                     [code-box-condi 500 nil fn bid k]
                     [re-com/box :justify :center :align :center :width "40px" :size "none" :height "100%" :style
                      {;:border "1px solid yellow"
                       :color            (theme-pull :theme/editor-outer-rim-color nil)
                       :background-color (str (theme-pull :theme/editor-outer-rim-color nil) 11)
                       :font-weight      700
                       :border-radius    "8px"
                       :opacity          (if valve-open? 1 0.4)} :child (str valve-open?)]]]]])])]]))]]))


(defn debug-box
  [flow-select raw-fmap]
  (let [read-only-flow? (true? (cstr/includes? (str flow-select) "/"))]
    [re-com/box :child
     [(if read-only-flow? code-box code-box-rw) 600 nil ;440
      raw-fmap ;(get fmap flow-select)
      flow-select] :width "600px"]))

(defn channel-box
  [flow-id flow-select val]
  (let [read-only-flow? (true? (cstr/includes? (str flow-select) "/"))
        connections     (filter #(= flow-select (second %)) @(ut/tracked-subscribe [::flowmap-connections-parsed flow-select]))
        cc              (count connections)]
    [re-com/v-box :gap "10px" :children
     [[re-com/h-box :width "570px" :style
       {;:border "1px solid white"
        :color (str (theme-pull :theme/editor-outer-rim-color nil) 88)} :size "auto" :padding "4px" :align :center :justify
       :center :gap "10px" :children
       [[re-com/md-icon-button :md-icon-name "fa-solid fa-radiation" :style
         {:font-size "14px" :color (theme-pull :theme/editor-outer-rim-color nil)}]
        [re-com/box :child "(use to push a value downstream manually - ATTN: be careful. lol)"]
        [re-com/md-icon-button :md-icon-name "fa-solid fa-radiation" :style
         {:font-size "14px" :color (theme-pull :theme/editor-outer-rim-color nil)}]]]
      [re-com/box :width "570px" :style
       {:border (str "3px dashed " (theme-pull :theme/editor-outer-rim-color nil) 66) :border-radius "12px"} :child
       [code-box-channels 570 nil (pr-str val) flow-id flow-select]]
      [re-com/box :width "570px" :style
       {:border           (str "1px solid " (str (theme-pull :theme/editor-outer-rim-color nil) 66))
        :border-radius    "10px"
        :font-size        "18px"
        :background-color (str (theme-pull :theme/editor-outer-rim-color nil) 25)} :size "auto" :padding "4px" :align :center
       :justify :center :child
       [re-com/h-box :gap "10px" :attr {:on-click #(ut/tracked-dispatch [::bricks/push-value flow-id flow-select val true])}
        :style {:color (theme-pull :theme/editor-outer-rim-color nil) :cursor "pointer"} :padding "6px" :children
        [[re-com/box :child (str "I understand - push to " cc " channel" (when (> cc 1) "s"))]
         [re-com/md-icon-button :md-icon-name "fa-solid fa-gun" :style {:font-size "18px"}]]]]] :width "600px"]))

(re-frame/reg-sub ::get-raw-data-input (fn [db [_ flow-id bid]] (get-in db [:flows flow-id :map bid :data :user-input])))

(re-frame/reg-sub ::get-raw-block-map (fn [db [_ flow-id bid]] (get-in db [:flows flow-id :map bid])))

(defn flow-code-editor-block
  [w h fmap flow-select ttype flow-id]
  (let [read-only-flow? (true? (cstr/includes? (str flow-select) "/"))
        bcolor          @(ut/tracked-subscribe [::port-color flow-id flow-select])
        btype           @(ut/tracked-subscribe [::port-color flow-id flow-select nil :data-type])
        opts-map        @(ut/tracked-subscribe [::opts-map])
        has-override?   (ut/ne? (get-in opts-map [:overrides flow-select]))
        bg-shade        "#00000055"]
    (if (= ttype :open-fn)
      [re-com/box :style {:border-radius "9px" :padding-top "8px" :padding-bottom "8px" :background-color bg-shade} :child
       [code-box-fn ;;w h
        (+ 13 w) nil (str (get fmap :raw-fn "(fn [x] x)")) flow-select]]
      (let [syntaxes ["raw (clojure)" "python" "r" "julia"]
            syntax   (get-in fmap [:data :syntax] "raw (clojure)")]
        [re-com/v-box :size "none" :children
         [(when has-override?
            [re-com/box :justify :end :align :center :style
             {:background-color (theme-pull :theme/editor-outer-rim-color nil)
              :color            (ut/choose-text-color (theme-pull :theme/editor-outer-rim-color nil))
              :font-size        "14px"
              :font-weight      700
              :padding          "4px"
              :border-radius    "9px"} :child "Note! This is overridden at run time! See flow options :overrides map above."])
          [re-com/box :style {:border-radius "9px" :padding-top "8px" :padding-bottom "8px" :background-color bg-shade} :child
           [(if read-only-flow? code-box code-box-rwo) (+ 10 w) nil ;h
            @(ut/tracked-subscribe [::get-raw-data-input flow-id flow-select]) flow-select syntax]]
          [re-com/h-box :width (px (- w 10)) :height "25px" :size "none" :justify :between :align :center :gap "8px" :children
           [;[re-com/gap :size "5px"]
            [re-com/h-box :gap "8px" :style {:font-size "11px" :padding-left "10px" :padding-right "10px"} :children
             (for [e    syntaxes
                   :let [selected? (= e syntax)]]
               [re-com/box :attr
                (when (not selected?) {:on-click #(ut/tracked-dispatch [::flow-block-meta flow-select :syntax e])}) :style
                (if selected?
                  {:background-color bg-shade ;; (str (theme-pull :theme/editor-outer-rim-color
                   :color            (theme-pull :theme/editor-font-color nil)
                   :border-radius    "0px 0px 7px 7px"
                   :font-weight      700}
                  {:opacity 0.4 :cursor "pointer"}) :padding "5px" :child (str e)])]
            [re-com/box :style {:padding-right "10px" :color bcolor} :child (str btype)]]]]]))))

(defn output-viewer
  [w h flow-id bid & [pid]]
  (let [value    @(ut/tracked-subscribe [::port-color flow-id bid nil :data-value])
        bg-shade "#00000055"
        value    (if (or (vector? value) (map? value) (list? value)) (ut/replace-large-base64 value) value)
        b64?     (ut/is-base64? (str value))]
    [re-com/box :style {:border-radius "9px" :padding-top "8px" :padding-bottom "8px" :background-color bg-shade} :child
     (cond (and (try (not (empty? value)) (catch :default _ false)) b64?)
             [re-com/v-box :size "auto" :gap "10px" :children
              [[re-com/box :child (str "(**base64 string: ~" (.toFixed (/ (* (count (str value)) 6) 8 1024 1024) 2) " MB)")]
               [re-com/box :size "auto" :child [:img {:src (str "data:image/png;base64," value)}]]]]
           (and (string? value)
                (cstr/starts-with? value "http")
                (or (cstr/ends-with? (cstr/lower-case value) "png")
                    (cstr/ends-with? (cstr/lower-case value) "gif")
                    (cstr/ends-with? (cstr/lower-case value) "webp")
                    (cstr/ends-with? (cstr/lower-case value) "jpeg")
                    (cstr/ends-with? (cstr/lower-case value) "jpg")))
             [re-com/v-box :gap "10px" :align :center :justify :center :children
              [;[re-com/box :child (str "image-url-string: " value)]
               [code-box (+ 13 w) nil (pr-str value)] [:img {:src (str value) :width "90%"}]]]
           (and (string? value) (cstr/starts-with? value "http") (or (cstr/ends-with? (cstr/lower-case value) "mp4")))
             [re-com/v-box :gap "10px" ;:align :center :justify :center
              :children
              [;[re-com/box :child (str "video-url-string: " value)]
               [code-box (+ 13 w) nil (pr-str value)]
               [:iframe ;; :video ? html5 shit instead?
                {:src (str value) :height "340px" :style {:border "none"}}]]]
           :else [code-box (+ 13 w) nil (pr-str value)])]))

(defn block-renamer
  [bid]
  (let [flow-id-regex #"^[a-zA-Z0-9_-]+$" ;; alpha, underscores, hypens, numbers
        str-bid       (ut/replacer (str bid) ":" "")]
    (if (= @rename-block bid)
      [re-com/input-text :src (at) :model str-bid :width "200px" :on-change
       #(do (if (= str-bid (str %)) ;; regex doesnt allow : so we need to remove before
              (reset! rename-block nil)
              (do (ut/tracked-dispatch [::rename-block bid %]) (reset! rename-block nil)))) :validation-regex flow-id-regex
       :change-on-blur? true :style
       {:text-decoration  "underline"
        :color            "inherit"
        :margin-top       "3px"
        :margin-left      "-4px"
        :text-align       "left"
        :background-color "#00000000"}]
      [re-com/box :style {:cursor "pointer" :color (theme-pull :theme/editor-outer-rim-color nil)} :attr
       {:on-double-click #(reset! rename-block bid)} :child (str bid)])))

(re-frame/reg-sub ::block-details (fn [db [_ bid]] (get-in db [:flows (get db :selected-flow) :map bid :description] "")))

(defn details-panel [bid] (let [value @(ut/tracked-subscribe [::block-details bid])] [text-box-rw nil nil value bid]))






(defonce gantt-log (reagent/atom false))

(re-frame/reg-sub ::ccounter (fn [db _] (get-in db [:re-pollsive.core/polling :counter])))


(defonce stopper (reagent/atom false))














(defn transform-data-old
  [data svg-width flow-id]
  (let [log-scale?        @gantt-log ;false ;true
        react!            [@(ut/tracked-subscribe [::http/flow-results]) @(ut/tracked-subscribe [::http/flow-results]) @gantt-log]
        blocks            @(ut/tracked-subscribe [::flowmap])
        running?          @(ut/tracked-subscribe [::is-running? :* flow-id])
        orderb            (vec (sort-by str (keys blocks)))
        data              (select-keys data orderb)
        data-pairs        (remove #(or (= (first %) :done) (cstr/includes? (str (first %)) "/")) data)
        sorted-data-pairs (sort-by (fn [[k _]] (.indexOf orderb k)) data-pairs)
        sorted-data-pairs (sort-by first sorted-data-pairs)
        start-times       (mapv #(-> %
                                     second
                                     :start)
                            sorted-data-pairs)
        end-times         (mapv #(-> %
                                     second
                                     :end)
                            sorted-data-pairs)
        min-start         (apply min start-times)
        max-end           (if running? (.now js/Date) (apply max end-times))
        total-duration    (- max-end min-start)
        log-offset        1]
    (mapv (fn [[step {:keys [start end fake?]}]]
            (let [end              (if (nil? end) (+ (.now js/Date) 1000) end)
                  start            (if (nil? start) (.now js/Date) start)
                  end              (if fake? 0 end)
                  start            (if fake? 0 start)
                  duration-ms      (- end start)
                  scaled-duration  (if log-scale? (Math/log1p (+ duration-ms log-offset)) duration-ms)
                  normalized-width (int (* (/ scaled-duration
                                              (if log-scale? (Math/log1p (+ total-duration log-offset)) total-duration))
                                           svg-width))
                  normalized-start (/ (- start min-start) total-duration)
                  x                (int (* normalized-start svg-width))]
              {:step         step
               :x            x
               :fake?        fake?
               :color        @(ut/tracked-subscribe [::port-color flow-id step])
               :width        (max 2 normalized-width)
               :raw-duration duration-ms}))
      sorted-data-pairs)))

(defn sort-by-order
  [sorted-data-pairs orderb]
  (let [order-map (zipmap orderb (range))] (sort-by (fn [[k _]] (order-map k)) sorted-data-pairs)))


(defn transform-data
  [data svg-width flow-id]
  (let [log-scale?            @gantt-log
        react!                [@(ut/tracked-subscribe [::http/flow-results]) @(ut/tracked-subscribe [::http/flow-results])
                               @gantt-log]
        blocks                @(ut/tracked-subscribe [::flowmap])
        running?              @(ut/tracked-subscribe [::is-running? :* flow-id])
        orderb                (vec (sort-by str (keys blocks)))
        sorted-data-pairs     (mapcat (fn [[k v]] (map (fn [run] [k run]) v)) data)
        sorted-data-pairs     (sort-by (fn [[k _]] (.indexOf orderb k)) sorted-data-pairs)
        grouped-and-sorted    (->> sorted-data-pairs
                                   (group-by first)
                                   (map (fn [[k v]] [k (sort-by (comp :end second) v)]))
                                   (into {}))
        last-events           (mapv (fn [[k v]] [k
                                                 (-> v
                                                     last
                                                     second
                                                     :end)])
                                grouped-and-sorted)
        last-event-set        (set (map second last-events))
        start-times           (mapv #(-> %
                                         second
                                         :start)
                                sorted-data-pairs)
        end-times             (mapv #(-> %
                                         second
                                         :end)
                                sorted-data-pairs)
        min-start             (apply min start-times)
        max-end               (apply max end-times)
        total-display-time-ms (- max-end min-start)
        scale-factor          (/ (* svg-width 1.5) total-display-time-ms)]
    (mapv (fn [[step run]]
            (let [{:keys [start in-chan? end fake?]} run
                  end                                (if running? (if (nil? end) (+ (.now js/Date) 1000) end) end)
                  start                              (if running? (if (nil? start) (.now js/Date) start) start)
                  duration-ms                        (- end start)
                  duration-ms                        (cond (>= duration-ms 999999999999) -1 ;duration-ms
                                                           (> duration-ms 0)             duration-ms
                                                           :else                         1)
                  scaled-duration                    (if log-scale? (Math/log1p (+ duration-ms 1)) duration-ms)
                  normalized-width                   (int (* (/ scaled-duration total-display-time-ms) svg-width))
                  x                                  (int (* (- start min-start) scale-factor))
                  last?                              (contains? last-event-set end)]
              (when (not in-chan?)
                {:step         step
                 :x            x
                 :last?        last?
                 :fake?        fake?
                 :color        @(ut/tracked-subscribe [::port-color flow-id step])
                 :width        (max 2 normalized-width)
                 :raw-duration duration-ms})))
      sorted-data-pairs)))

(defn ordered-group [grouped-by-step orderb] (mapv (fn [k] [k (grouped-by-step k)]) orderb))

(defn gantt-chart
  [data svg-width flow-id]
  (let [gap              11
        react!           [@(ut/tracked-subscribe [::http/flow-results]) @gantt-log]
        blocks           @(ut/tracked-subscribe [::flowmap])
        orderb           (vec (sort-by str (keys blocks)))
        running?         @(ut/tracked-subscribe [::is-running? :* flow-id])
        transformed-data (transform-data data svg-width flow-id)
        height           26
        current-time     (.now js/Date)
        grouped-by-step  (group-by :step transformed-data)
        grouped-by-step  (ordered-group grouped-by-step orderb)
        has-data?        (and (ut/ne? transformed-data)
                              (not (= grouped-by-step (get @db/last-gantt flow-id {})))
                              (if running? (>= (- current-time @db/last-update) 1000) true))]
    (when (or has-data? ;;true
              (= -1 @db/last-update)
              (empty? (get @db/last-gantt flow-id {})))
      (reset! db/last-update current-time)
      (swap! db/last-gantt assoc flow-id grouped-by-step))
    (let [flow-select      @(ut/tracked-subscribe [::selected-flow-block])
          grouped-by-stepc (get @db/last-gantt flow-id {})]
      [:svg {:width "110%" :height (str (+ 20 (* height (count grouped-by-stepc)) (* gap (dec (count grouped-by-stepc)))) "px")}
       (try
         (doall
           (map-indexed
             (fn [idx [step runs]]
               (doall
                 (map-indexed (fn [run-idx {:keys [x color width last? raw-duration]}]
                                (let [y               (+ (* idx (+ height gap)) (* run-idx 0)) ; Adjust y for
                                      selected?       (= step flow-select)
                                      hover-involved? (true? (some #(= % step)
                                                                   (get-in @flow-details-block-container-atom
                                                                           [flow-id :involved])))]
                                  [:g {:transform (str "translate(0," y ")")}
                                   [:rect
                                    {:x              x
                                     :y              0
                                     :width          width
                                     :height         height
                                     :filter         (when selected? (str "drop-shadow(0 0 0.75rem " color ")"))
                                     :on-click       #(do (select-block step))
                                     :on-mouse-enter #(reset! editor-tooltip-atom (str "select block " step))
                                     :on-mouse-leave #(reset! editor-tooltip-atom nil)
                                     :stroke         color
                                     :cursor         "pointer"
                                     :fill           (if selected? color (str color 35))}]
                                   (when last?
                                     [:text
                                      {:x           (+ x width gap -5)
                                       :y           (+ height (/ gap 2) (if selected? -12 -15))
                                       :font-family "Arial"
                                       :opacity     (if selected? 1.0 0.5)
                                       :fill        (if selected? color "#ffffff")
                                       :font-weight (when selected? "bold")
                                       :font-size   (if selected? "16px" "10px")} (str raw-duration " ms")])]))
                              runs)))
             grouped-by-stepc))
         (catch :default _ nil))])))

(declare block-icons)

(defn gantt-container
  [flow-id orderb in-sidebar? pw & [max-height]]
  (let [opw       pw ;; orig
        pw        (* pw 0.33)
        gw        (* 0.46 pw)
        left      35
        top       73
        react!    [@(ut/tracked-subscribe [::http/flow-results]) @gantt-log @db/last-update
                   @(ut/tracked-subscribe [::http/flow-results-tracker flow-id])]
        bg-height (+ (* (count orderb) 37) 36)
        running?  @(ut/tracked-subscribe [::is-running? :* flow-id])
        data      @(ut/tracked-subscribe [::http/flow-results-tracker flow-id])
        ddata     (apply concat (for [[_ v] data] v))
        sstart    (apply min (for [{:keys [start]} ddata] start))
        eend      (if running?
                    (.now js/Date) ;@db/last-update
                    (apply max (for [{:keys [end]} ddata] end)))
        duration  (str (ut/nf (- eend sstart)) " ms")
        bdr       (str "6px solid " (theme-pull :theme/editor-outer-rim-color nil))]
    (if in-sidebar?
      (let [blocks      @(ut/tracked-subscribe [::flowmap])
            flow-select @(ut/tracked-subscribe [::selected-flow-block])]
        [re-com/h-box :size "none" :gap "6px" :height (if max-height (px max-height) (px (* (count (keys blocks)) 36))) :style
         (when max-height {:overflow "auto"}) :children
         [[re-com/v-box :style
           {;:border "1px solid white"
            :margin-left   "3px"
            :padding-left  "5px"
            :padding-right "5px"} :width "28px" :gap "10px" :children (block-icons blocks flow-id flow-select)]
          [gantt-chart data (* 0.47 opw) flow-id]] :width (px opw)])
      [re-com/v-box :children
       [[re-com/box :width (px pw) :height (px bg-height) :style
         {:position         "fixed"
          :background-color "#00000066"
          :border-radius    "0px 12px 0px 0px"
          :z-index          999
          :overflow         "hidden"
          :left             left
          :padding-top      "18px"
          :padding-bottom   "18px"
          :padding-left     "8px"
          :padding-right    "18px"
          :margin-left      "4px"
          :border-top       bdr
          :backdrop-filter  "blur(4px)"
          :top              top} :child [gantt-chart data gw flow-id]]
        [re-com/h-box :justify :between :align :center :height "30px" :size "none" :width (px (+ 3 pw)) :children
         [[re-com/box :style {:cursor "pointer" :text-decoration (if @gantt-log "none" "line-through")} :attr
           {:on-click #(reset! gantt-log (not @gantt-log))} :child (str "use log widths?")]
          [re-com/box :child (str (if running? "so far... " "total ") duration)]] :style
         {:position         "fixed"
          :padding-left     "14px"
          :padding-right    "10px"
          :z-index          1111
          :color            (str (theme-pull :theme/editor-outer-rim-color nil) 88)
          :font-size        "11px"
          :border-radius    "0px 0px 12px 0px"
          :background-color "#00000066"
          :top              (+ bg-height top)
          :left             left}]
        [re-com/box :width (px pw) :height (px (+ 30 bg-height)) :style
         {:position         "fixed"
          :background-color "#00000066"
          :border-radius    "0px 12px 12px 0px"
          :z-index          995
          :left             left
          :filter           "drop-shadow(0.35rem 0.35rem 0.4rem rgba(0, 0, 0, 0.8))"
          :padding-top      "18px"
          :padding-bottom   "18px"
          :padding-left     "8px"
          :padding-right    "18px"
          :margin-left      "4px"
          :top              top} :child " "]]])))

(defn ordered-for [order keys-map] (map #(vector % (keys-map %)) order))

(defn block-icons
  [blocks flow-id flow-select]
  (vec
    (for ;[[k {:keys [icon]}] (ordered-for orderb blocks)]
      [[k icon] (sort-by first (for [[k v] blocks] [k (get v :icon)]))]
      (let [bcolor    @(ut/tracked-subscribe [::port-color flow-id k])
            waiting?  @(ut/tracked-subscribe [::is-waiting? k flow-id])
            running?  @(ut/tracked-subscribe [::is-running? k flow-id])
            selected? (= k flow-select)]
        [re-com/box :size "none" :height "27px" :width "27px" :align :center :justify :center :class
         (when running? "rotate infinite") :style
         {:background-color (str bcolor "28")
          :opacity          (if (or running? selected?) 1.0 0.4)
          :border-radius    "5px"
          :transform        (when waiting? "rotate(45deg)")
          :z-index          (when running? 1200)
          :zoom             (when running? 1.5)
          :margin-top       (when running? "-4px")
          :margin-bottom    (when running? "-5px")
          :border           (str "1px dotted " bcolor)
          :margin-left      (if @(ut/tracked-subscribe [::bricks/flow-editor?]) "-7px" "9px")} :child
         [re-com/md-icon-button :src (at) :md-icon-name (or icon "zmdi-view-quilt") :on-click #(do (select-block k)) :attr
          {:on-mouse-enter #(reset! editor-tooltip-atom (str "select block " k))
           :on-mouse-leave #(reset! editor-tooltip-atom nil)} :style
          {;:background-color (theme-pull :theme/editor-rim-color nil) ;"#00000000" :opacity
           :color     bcolor ;(theme-pull :theme/editor-outer-rim-color nil)
           :cursor    "pointer"
           :filter    (when (or running? selected?) (str "brightness(1.5) drop-shadow(0.2rem 0.2rem 0.4rem " bcolor ")"))
           :font-size "24px"}]]))))


(defn save-custom-block
  [flow-id flow-select]
  [re-com/v-box :gap "4px" :style {:font-size "10px" :font-weight 400 :color (theme-pull :theme/editor-outer-rim-color nil)}
   :children
   [[re-com/box :child
     "This will save this block definition to the general 'flow block parts' menu and will be accessible under the 'custom' category."]
    [re-com/h-box :padding "5px" :gap "6px" :children
     [[re-com/box :attr
       {:on-click
          #(ut/tracked-dispatch
             [::wfx/request :default
              {:message     {:kind        :save-custom-flow-block
                             :name        flow-select
                             :block-map   (let [flowmap              @(ut/tracked-subscribe [::flowmap])
                                                flowmap-raw          @(ut/tracked-subscribe [::flowmap-raw])
                                                flow-id              @(ut/tracked-subscribe [::selected-flow])
                                                flowmaps-connections @(ut/tracked-subscribe [::flowmap-connections])
                                                server-flowmap       (process-flowmap2 flowmap flowmaps-connections flow-id)]
                                            (merge {:types       (get-in flowmap-raw [flow-select :data :flow-item :types])
                                                    :description (cstr/join " " (get-in flowmap-raw [flow-select :description]))}
                                                   (select-keys (get flowmap-raw flow-select) [:description])
                                                   (get-in server-flowmap [:components flow-select])))
                             :client-name @(ut/tracked-subscribe [::bricks/client-name])}
               :on-response [::simple-response]
               :timeout     15000000}])} :align :center :justify :center :size "auto" :child "save" :padding "5px" :style
       {:font-weight      700
        :border-radius    "6px"
        :cursor           "pointer"
        :background-color (str (theme-pull :theme/editor-outer-rim-color nil) 22)}]
      [re-com/box :align :center :justify :center :size "auto" :child "delete" :padding "5px" :style
       {:font-weight      700
        :border-radius    "6px"
        :cursor           "pointer"
        :background-color (str (theme-pull :theme/editor-outer-rim-color nil) 22)}]]]
    [re-com/box :height "14px" :size "auto" :align :center :justify :center :child " "]]])


(defn flow-details-panel
  [panel-height panel-width details-panel-height]
  (let [react-hacks         [@flow-hover @scroll-id @editor-mode @(ut/tracked-subscribe [::http/flow-results])]
        blocks              @(ut/tracked-subscribe [::flowmap])
        orderb              (vec (sort-by str (keys blocks)))
        flow-id             @(ut/tracked-subscribe [::selected-flow])
        flow-select         @(ut/tracked-subscribe [::selected-flow-block])
        sys-panel-width     (or (last @db/flow-editor-system-mode) 900)
        browser-panel-width (if (nil? flow-select) sys-panel-width 600)
        gantt?              @(ut/tracked-subscribe [::bricks/flow-gantt?])
        full-height         (+ details-panel-height panel-height -40)]
    [re-com/v-box :children
     [[re-com/h-box :children
       [;[re-com/gap :size "8px"]
        (when (and @(ut/tracked-subscribe [::bricks/flow-editor?]) (not flow-select))
          [re-com/box :style {:margin-top "-2px"} :child [flow-editor browser-panel-width full-height] ; browser-panel-height]
           :width (px browser-panel-width) :height (px full-height)])
        (when (and @(ut/tracked-subscribe [::bricks/flow-editor?]) flow-select)
          (let [;body (get @(ut/tracked-subscribe [::flowmap]) flow-select)
                fmap              (get blocks flow-select)
                inputs?           (not (empty? (keys (get-in fmap [:ports :in]))))
                pval              @(ut/tracked-subscribe [::port-color flow-id flow-select :out :data-value])
                view-pval         @(ut/tracked-subscribe [::port-color flow-id
                                                          (keyword (str (ut/replacer (str flow-select) ":" "") "-vw")) :out
                                                          :data-value])
                browsable?        (true? (or (vector? pval) (map? pval)))
                rabbit-code?      (or (and (vector? pval) (keyword? (first pval))) ;; is it
                                      (and (map? pval) (contains? pval :queries) (contains? pval :view)))
                rabbit-code-view? (or (and (vector? view-pval) (keyword? (first view-pval))) ;; is
                                      (and (map? view-pval) (contains? view-pval :queries) (contains? view-pval :view)))
                block-type        (get-in blocks
                                          [flow-select :data :flow-item :type]
                                          (get-in blocks [flow-select :data :drag-meta :type] :no-type?))
                desc              (get-in blocks [flow-select :data :flow-item :description])
                header-row-height 56
                ttype             (get-in fmap [:data :flow-item :type] (get-in fmap [:data :drag-meta :type]))
                open?             (true? (or (= ttype :open-fn) (= ttype :open-block) (= ttype :open-input)))
                port-meta         @(ut/tracked-subscribe [::get-meta flow-select])
                single-scrubber?  (and (or (= ttype :open-block) (= ttype :open-input))
                                       (not (nil? (get-in port-meta [:* :scrubber]))))
                flow-select       @(ut/tracked-subscribe [::selected-flow-block]) ;; dupe here
                opts-map          @(ut/tracked-subscribe [::opts-map])]
            [re-com/v-box :align :center :height (px full-height) :width (px browser-panel-width) :size "none" :style
             {:font-size "15px" :background-color "#00000088"} :children
             [[re-com/h-box :children
               [[block-renamer flow-select]
                [re-com/h-box :gap "8px" :children
                 [[re-com/box :attr {:on-click #(zoom-to-element (str "flow-brick-" flow-select) 1.3)} :child
                   [render-icon (str (get fmap :icon "zmdi-layers")) "inherit" 29 20]] [re-com/box :child (str block-type)]]]]
               :width "601px" :height (px header-row-height) ;; width overflow on purpose to
               :style
               {:margin-left      "1px"
                :padding-right    "16px"
                :font-weight      700
                :color            (str (theme-pull :theme/editor-outer-rim-color nil) 87)
                :padding-top      "5px"
                :padding-left     "13px"
                :padding-bottom   "5px"
                :margin-top       "-1px"
                :border-top       (str "6px solid " (theme-pull :theme/editor-outer-rim-color nil))
                :background-color "#00000088"} :size "none" :justify :between :align :center]
              [re-com/v-box :size "none" :gap "6px" :height (px (- full-height header-row-height)) :style
               {:position "fixed" :overflow-y "auto" :overflow-x "hidden" :left 6 :top header-row-height} :children
               [;[re-com/box :child (str "debug some shit: "  ttype open?)]
                [flow-details-block-container "flow options*" :system :system [code-box-options flow-id 570 nil opts-map]
                 "zmdi-code"]
                (when (not gantt?)
                  [flow-details-block-container "in-line flow gantt*" :system :system [gantt-container flow-id orderb true 580]
                   "zmdi-chart-donut"])
                [flow-details-block-container "step notes*" flow-id flow-select [details-panel flow-select]
                 "zmdi-comment-alt-text"]
                (when inputs?
                  [flow-details-block-container "input ports" flow-id flow-select
                   [port-panel flow-id flow-select (get blocks flow-select) :in] ["zmdi-long-arrow-right" "zmdi-circle-o"]])
                (when (and (not inputs?) single-scrubber?)
                  [flow-details-block-container "input scrubber" flow-id flow-select
                   [re-com/box :size "auto" :style {:padding-top "8px" :padding-bottom "8px"} :child
                    [bricks/scrubber-panel true @(ut/tracked-subscribe [::bricks/keypaths-in-flow flow-select true])
                     [:flow flow-select] :* {:fm true :canvas? true :flow? true}]] ["zmdi-long-arrow-right" "zmdi-circle-o"]])
                (if open?
                  [flow-details-block-container (if (= ttype :open-fn) "code editor" "code / value editor") flow-id flow-select
                   [flow-code-editor-block (- browser-panel-width 20) ;; 580
                    378 fmap flow-select ttype flow-id] (if (= ttype :open-fn) "zmdi-functions" "zmdi-view-subtitles")]
                  (when (ut/ne? desc) ;; (not (empty? desc))
                    [flow-details-block-container "flow block description" flow-id flow-select
                     [re-com/box :style
                      {:padding-left  "6px"
                       :font-size     "12px"
                       :color         (str (theme-pull :theme/editor-outer-rim-color nil))
                       :padding-right "6px"} :child (str desc)] "zmdi-view-toc"]))
                (when (and (= ttype :open-fn) open?)
                  [flow-details-block-container "view editor*" flow-id flow-select
                   [code-box-view (- browser-panel-width 20) ;; 580
                    nil fmap flow-select flow-id] "zmdi-functions"])
                (when rabbit-code-view?
                  [flow-details-block-container "rabbit render (view)" flow-id flow-select
                   [re-com/box :align :center :justify :center :child [buffy/render-honey-comb-fragments view-pval nil nil]]
                   "zmdi-aspect-ratio"])
                [flow-details-block-container "output ports" flow-id flow-select
                 [port-panel flow-id flow-select (get blocks flow-select) :out] ["zmdi-circle" "zmdi-long-arrow-right"]]
                [flow-details-block-container "output value(s)" flow-id flow-select
                 [output-viewer (- browser-panel-width 20) ;; 580
                  378 flow-id flow-select nil] "zmdi-view-web"]
                (when browsable?
                  [flow-details-block-container "output browser" flow-id flow-select
                   [map-boxes2 pval flow-select flow-id [] :output (if (vector? pval) "vector" "map") true] "zmdi-shape"])
                (when rabbit-code?
                  [flow-details-block-container "rabbit render" flow-id flow-select
                   [re-com/box :align :center :justify :center :child [buffy/render-honey-comb-fragments pval nil nil]]
                   "zmdi-aspect-ratio"])
                (when (= ttype :open-fn)
                  [flow-details-block-container "save as a flow block*" flow-id flow-select
                   [save-custom-block flow-id flow-select] "fa-solid fa-cube"])
                [flow-details-block-container "flow block map - debugger*" flow-id flow-select
                 [debug-box flow-select @(ut/tracked-subscribe [::get-raw-block-map flow-id flow-select])] "zmdi-bug"]
                [flow-details-block-container "channel push - debugger*" flow-id flow-select
                 (let [last-output (ut/replace-large-base64 @(ut/tracked-subscribe [::port-color flow-id flow-select nil
                                                                                    :data-value]))
                       val         (get-in @channel-holster [flow-id flow-select :value] last-output)
                       copen?      @(ut/tracked-subscribe [::bricks/flow-channels-open? flow-id])]
                   (if copen?
                     [channel-box flow-id flow-select val]
                     [re-com/v-box :align :center :justify :center :style {:color (theme-pull :theme/editor-outer-rim-color nil)}
                      :children
                      [[re-com/box :style {:font-size "15px"} :child "channels are all closed"]
                       [re-com/box :style {:font-size "11px"} :child
                        "if you'd like to push channel values, run again with option :close-on-done? false"]]])) "zmdi-bug"]
                [flow-details-block-container "connections - debugger*" flow-id flow-select
                 [re-com/box ;;; edit-conns
                  :child
                  [code-box-rwc (- browser-panel-width 20) ;; 580
                   nil (str @(ut/tracked-subscribe [::flowmap-connections]))]] "zmdi-bug"] [re-com/gap :size "10px"]]]]]))
        [re-com/v-box :gap "10px" ;; (if (or gantt? flow-select) "10px" "20px")
         :size "none" :height (px full-height) ; (px browser-panel-height)
         :style
         {:background-color "#00000099"
          :border-radius    "0px 16px 0px 0px"
          :position         "fixed"
          :left             (if @(ut/tracked-subscribe [::bricks/flow-editor?]) browser-panel-width 0)
          :backdrop-filter  "blur(3px)"
          :transition       "all 0.3s"
          :top              -1 ;28
          :padding-top      "10px"
          :padding-right    (if (not @(ut/tracked-subscribe [::bricks/flow-editor?])) "5px" "inherit")
          :border-top       (str "6px solid " (theme-pull :theme/editor-outer-rim-color nil))
          :border-right     (str "1px solid " (theme-pull :theme/editor-outer-rim-color nil) 25)} :width "40px" :align :center
         :justify :start :children
         (into [[re-com/md-icon-button :src (at) :md-icon-name
                 (if @(ut/tracked-subscribe [::bricks/flow-editor?]) "zmdi-chevron-left" "zmdi-chevron-right") :on-click
                 #(do (ut/tracked-dispatch [::bricks/toggle-flow-editor]) (reset! editor-tooltip-atom nil)) :attr
                 {:on-mouse-enter #(reset! editor-tooltip-atom (str "toggle flow work panel "
                                                                    (if @(ut/tracked-subscribe [::bricks/flow-editor?])
                                                                      "open"
                                                                      "closed")))
                  :on-mouse-leave #(reset! editor-tooltip-atom nil)} :style
                 {:opacity     0.45
                  :color       (theme-pull :theme/editor-outer-rim-color nil)
                  :cursor      "pointer"
                  :height      "15px"
                  :margin-left (if @(ut/tracked-subscribe [::bricks/flow-editor?]) "-9px" "9px")
                  :font-size   "33px"}]
                [re-com/md-icon-button :src (at) :md-icon-name "zmdi-chart" :on-click
                 #(do (ut/tracked-dispatch [::bricks/toggle-flow-gantt]) (reset! editor-tooltip-atom nil)) :attr
                 {:on-mouse-enter #(reset! editor-tooltip-atom (str "toggle run gantt chart " (if gantt? "open" "closed")))
                  :on-mouse-leave #(reset! editor-tooltip-atom nil)} :style
                 {:opacity     (if gantt? 1.0 0.45)
                  :color       (theme-pull :theme/editor-outer-rim-color nil)
                  :cursor      "pointer"
                  :height      "15px"
                  :margin-top  "14px"
                  :margin-left (if @(ut/tracked-subscribe [::bricks/flow-editor?]) "-9px" "9px")
                  :font-size   "23px"}] [re-com/gap :size (if flow-select "3px" "3px")]
                (if gantt? [gantt-container flow-id orderb false panel-width] [re-com/gap :size "0px"])]
               (if (or flow-select gantt?)
                 (block-icons blocks flow-id flow-select)
                 [] ;; old menu buttons.... deprecated in lieu of top text label buttons
               ))]]]] :size "none" :align :center :justify :center :style
     {:z-index          200
      :transition       "width 0.3s"
      :font-family      (theme-pull :theme/base-font nil)
      :color            (theme-pull :theme/editor-font-color nil)
      :backdrop-filter  "blur(5px)"
      :background-color "#00000022"
      :border-radius    "10px"
      :position         "fixed"
      :top              28} ;(- panel-height 8)
     :height (px full-height) :width (if @(ut/tracked-subscribe [::bricks/flow-editor?]) (px browser-panel-width) (px 45))]))







(re-frame/reg-sub ::estimates (fn [db _] (get db :flow-estimates)))

(defn alert-box
  []
  [bricks/reecatch
   (let [rekt            [@db/kick-alert @db/pause-alerts]
         rs-running      @(ut/tracked-subscribe [::bricks/runstreams-running])
         rs-running-list @(ut/tracked-subscribe [::bricks/runstreams-running-list])
         alerts          @(ut/tracked-subscribe [::bricks/alerts])
         estimates       @(ut/tracked-subscribe [::estimates])
         max-w           (apply max (for [a alerts :let [width (* (get a 1) db/brick-size)]] width))
         max-w           (if (or (nil? max-w) (< max-w 50))
                           300 ;420
                           max-w)
         alerts          (if (> rs-running 0)
                           (conj alerts
                                 [[:v-box :children
                                   (vec
                                     (into [[:box :child (str rs-running " flow" (when (> rs-running 1) "s") " running")]]
                                           (vec
                                             (for [e    rs-running-list
                                                   :let [fid    (ut/replacer e ":" "")
                                                         run-id (get-in estimates [fid :run-id])
                                                         est    (+ (js/Math.round (get-in estimates [fid :times] 0)) 1)
                                                         est?   (> est 1)]]
                                               [:v-box :padding "3px" :width (px max-w) ;;"215px"
                                                :size "auto" ;:justify :center
                                                :style {:font-size "11px"} :children
                                                [[:box :size "auto" :style {:padding-left "5px"} :child (str fid)]
                                                 (when est? ;true ;est?
                                                   [:box :child [:progress-bar [(- max-w 15) est (str e run-id)]] :height "25px"
                                                    :padding "3px"])
                                                 (when est? ;true ;est?
                                                   [:box :align :end :style
                                                    {:font-size "10px" :font-weight 400 :padding-right "5px"} :child
                                                    (str "estimate: " (ut/format-duration-seconds est))])]]))))] 4
                                  (+ 1.25 (* 1.05 rs-running) (when (= 1 rs-running) -0.275)) 0])
                           alerts)
         alerts-cnt      (try (count alerts) (catch :default _ 0))
         ;gap             1 ;11
         ;all-h           (apply + (for [a alerts :let [h (* (get a 2) db/brick-size)]] h))
         ;all-h           (if (> alerts-cnt 1) (+ all-h (* gap (dec alerts-cnt))) all-h)
         ;all-h           (if (or (nil? all-h) (< all-h 50)) 50 all-h)
         ;box-height      50
         box-width       (+ 80 max-w) ;420
         edge-hide       (* (- box-width 50) -1)]
     (when (= alerts-cnt 0) (reset! db/kick-alert false))
     (when (> alerts-cnt 0) (reset! db/kick-alert true))
     [re-com/box :child
      [re-com/h-box :attr
       {:on-mouse-enter #(reset! db/pause-alerts true)
        :on-mouse-over  #(when (not @db/pause-alerts) (reset! db/pause-alerts true))
        :on-mouse-leave #(reset! db/pause-alerts false)} :children
       [;[:img {:src "images/test-kick-icon.png" :width 30 :height 30}]
        [re-com/md-icon-button :src (at) :md-icon-name (if @db/kick-alert "zmdi-star" "zmdi-star-outline") :style
         {;:color "red"
          :transition   "all 0.4s ease-in-out"
          :color        (if @db/pause-alerts (theme-pull :theme/editor-outer-rim-color nil) "inherit")
          :position     "fixed"
          :bottom       17
          :margin-right "12px"
          :margin-top   "-4px"
          :font-size    "34px"}] [re-com/gap :size "44px"]
        [re-com/v-box
         :gap "12px"
         :children ;(into (for [e (range rs-running)] [re-com/box :child "o"])
         (for [a    alerts
               :let [abody       (first a)
                     push-codes? (try (some #(or (= % :push) (= % :dialog-push)) (ut/deep-flatten abody))
                                      (catch :default _ false))
                     width       (+ 60 (* (get a 1 0) db/brick-size))
                     ;;height      (* (get a 2 0) db/brick-size)
                     alert-id    (last a)]]
           [re-com/box :size "none" :attr
            (when (not push-codes?) (if @db/kick-alert {:on-click #(ut/tracked-dispatch [::bricks/prune-alert alert-id])} {}))
            :width (when (> width 0) (px width))
            :height "auto" ;;(when (> height 0) (px height))
            :child [buffy/render-honey-comb-fragments
                    abody
                    (get a 1) ;; width 
                    (get a 2) ;; height
                    ]])]]]
      :width (px (+ 0 box-width))
      ;:height "auto" (px (if @db/kick-alert all-h box-height)) ;; experimental for auto sizing!!!
      :padding "9px"
      :style {:position         "fixed"
              :font-size        "18px"
              :padding-top "8px"  :padding-bottom "8px" ;; part of auto sizing test
              :border-left      (str "2px solid " (theme-pull :theme/editor-outer-rim-color nil) (if @db/kick-alert "" "01"))
              :border-top       (str "2px solid " (theme-pull :theme/editor-outer-rim-color nil) (if @db/kick-alert "" "01"))
              :border-bottom    (str "2px solid " (theme-pull :theme/editor-outer-rim-color nil) (if @db/kick-alert "" "01"))
              :font-weight      700
              :cursor           "pointer"
              :border-radius    "19px 0px 0px 19px"
              :bottom           25
              :z-index          9999
       ;:transition       "all 0.6s ease-in-out"
              :right            (if @db/kick-alert 0 edge-hide)
              :backdrop-filter  "blur(4px)"
              :background-color (str "#000000" (if @db/kick-alert 88 11))
              :color            "white"}])])

(re-frame/reg-sub ::flow-parts-lookup
                  (fn [_ [_ & [part-key]]]
                    (let [fflowparts-sys @(ut/tracked-subscribe [::conn/sql-data [:fflowparts-sys]])
                          flow-parts     (vec (sort-by :name fflowparts-sys))
                          flow-parts-key (when part-key
                                           (let [ss   (ut/splitter (ut/replacer (str part-key) ":" "") "/")
                                                 cat  (keyword (first ss))
                                                 name (keyword (second ss))]
                                             (first (filter #(and (= (get % :category) (str cat)) (= (get % :name) (str name)))
                                                      flow-parts))))]
                      (if flow-parts-key flow-parts-key flow-parts))))






(def lookup-val (reagent/atom nil))

(defn lookup-modal
  []
  (let [[left top] @db/context-modal-pos
        [xx yy]    @db/flow-detached-coords
        left       (- left xx)
        top        (- top yy)
        flow-parts @(ut/tracked-subscribe [::flow-parts-lookup])
        sql-calls  {:fflows-sys     {:select        [[":flow" :category] ["" :description] [:flow_id :name] :file_path
                                                     [:body :full_map] ["zmdi-developer-board" :icon]]
                                     :from          [:flows]
                                     :connection-id "flows-db"
                                     :order-by      [[3 :asc]]}
                    :fflowparts-sys {:select   [:category :description :name :file_path :full_map :icon]
                                     :order-by [[3 :asc]]
                                     :from     [:flow_functions]}}]
    (dorun
      (for [[k query] sql-calls]
        (let [;data-exists? @(ut/tracked-subscribe [::conn/sql-data-exists? [k]])
              ;unrun-sql?   @(ut/tracked-subscribe [::conn/sql-query-not-run? [k] query])
              data-exists?   @(ut/tracked-sub ::conn/sql-data-exists-alpha? {:keypath [k]})
              unrun-sql?     @(ut/tracked-sub ::conn/sql-query-not-run-alpha? {:keypath [k] :query query})]
          (when (or (not data-exists?) unrun-sql?)
            (if (get query :connection-id) (conn/sql-data [k] query (get query :connection-id)) (conn/sql-data [k] query))))))
    [re-com/modal-panel :style {:z-index 999 :padding "0px"} :parts
     {:child-container {:style {:left      left
                                :transform "scale(1.5)"
                                :top       top
                                :font-size "8px"
                                :border    (str "5px solid " (theme-pull :theme/editor-outer-rim-color nil))
                                :position  "fixed"}}} :backdrop-opacity 0.63 :backdrop-on-click #(reset! lookup-modal? false)
     :child
     [re-com/h-box :justify :between :align :center :gap "10px" :style {:color "#000000" :font-size "18px"} :children
      [;[re-com/box :child "topper"]
       [re-com/typeahead :width "250px" :suggestion-to-string
        (fn [item] (str (get item :category) "/" (ut/replacer (str (get item :name)) ":" ""))) ;; render in
        :render-suggestion
        (fn [ss _] ;; render in dropdown
          [re-com/h-box :style {:border-top "1px solid #00000009"} :size "none" :width "240px" :justify :between :align :center
           :children
           [[re-com/v-box :children
             [[re-com/h-box :justify :between :align :center :children
               [[re-com/box :style {:font-weight 700 :font-size "12px"} :child (str (get ss :name))]]]
              (when (ut/ne? (get ss :description)) [re-com/box :style {:font-size "7px"} :child (str (get ss :description))])]]
            [re-com/v-box :align :end :justify :end :children
             [[render-icon (str (get ss :icon)) "#00000044" 20 15]
              [re-com/box :style {:font-size "6px" :color "#00000066" :font-weight 700} :child (str (get ss :category))]]]]])
        :on-change #(reset! lookup-val %) :rigid? false :placeholder "what do you want to add?" :data-source
        (fn [x]
          (let [words        (ut/splitter (cstr/lower-case (cstr/trim x)) #" ")
                matches-word (fn [field word] (cstr/includes? (cstr/lower-case (str field)) word))]
            (if (or (nil? x) (empty? x))
              flow-parts
              (filter (fn [item]
                        (let [category    (get item :category)
                              name        (get item :name)
                              description (get item :description)]
                          (every? (fn [word]
                                    (or (matches-word category word) (matches-word name word) (matches-word description word)))
                                  words)))
                flow-parts))))]
       [re-com/box :child
        [re-com/md-icon-button :src (at) :md-icon-name "zmdi-plus" :style
         {;:color (theme-pull :theme/editor-outer-rim-color nil)
          :font-size "22px"
          :cursor    "pointer"}] :padding "6px" :style
        {:background-color (str (theme-pull :theme/editor-outer-rim-color nil) 33)
         :color            "#00000089"
         :font-weight      700
         :border-radius    "8px"
         :cursor           "pointer"} :attr
        {:on-click #(let [lval (conn/spawn-open-input-block @lookup-val)]
                      (ut/tapp>> [:adding-block :conn/spawn-open-input-block @lookup-val :w lval])
                      (apply conn/add-flow-block lval) ;; being passed [snapped-x snapped-y
                      (reset! lookup-modal? false))}]
       [re-com/box :child
        [re-com/md-icon-button :src (at) :md-icon-name "zmdi-refresh" :style
         {:color "#00000038" :font-size "15px" :cursor "pointer"}] :padding "6px" :style
        {:font-weight 700 :border-radius "8px" :cursor "pointer"} :attr
        {:on-click #(do (ut/tracked-dispatch [::conn/clear-query-history :fflows-sys])
                        (ut/tracked-dispatch [::conn/clear-query-history :fflowparts-sys]))}]]]]))

(re-frame/reg-event-db ::flip-drop-toggle (fn [db _] (reset! drop-toggle? (not @drop-toggle?)) db))



(re-frame/reg-event-db ::unload-flow-db
                       (fn [db [_ flow-id]]
                         (-> db
                             (ut/dissoc-in [:flows flow-id])
                             (ut/dissoc-in [:flow-results :condis flow-id])
                             (ut/dissoc-in [:flow-results :tracker flow-id])
                             (ut/dissoc-in [:flow-results :return-maps flow-id])
                             (ut/dissoc-in [:flow-runner flow-id]))))

(re-frame/reg-event-fx ::unload-flow
                       (fn [{:keys [db]} [_ flow-id]]
                         {:db       (ut/tracked-dispatch [::unload-flow-db flow-id])
                          :dispatch [::wfx/request :default
                                     {:message {:kind        :remove-flow-watcher ;; just in case we are
                                                :client-name @(ut/tracked-subscribe [::conn/client-name])
                                                :flow-id     flow-id}
                                      :timeout 50000}]}))


(re-frame/reg-sub ::flow-drop-down? (fn [db _] (get db :flow-drop-down? false)))

(re-frame/reg-event-db ::toggle-flow-drop-down (fn [db _] (assoc db :flow-drop-down? (not (get db :flow-drop-down? false)))))


(re-frame/reg-sub ::zoom-unlocked? (fn [db _] (get db :alt-key-held? false)))

(re-frame/reg-event-db ::toggle-lock (fn [db _] (assoc db :alt-key-held? (not (get db :alt-key-held?)))))

(defn flow-panel
  []
  (let [[x y]                @db/flow-detached-coords
        x-px                 (px x)
        y-px                 (px y)
        [wpct hpct]          db/flow-panel-pcts ;; [0.85 0.50]
        panel-width          (* (.-innerWidth js/window) wpct)
        hh                   @(ut/tracked-sub ::subs/h {}) ;; to ensure we get refreshed when
        ww                   @(ut/tracked-sub ::subs/w {})
        flow-id              @(ut/tracked-sub ::selected-flow {})
        watched?             (ut/ne? (get @(ut/tracked-sub ::bricks/flow-watcher-subs-grouped {}) flow-id))
        zoom-unlocked?       @(ut/tracked-sub ::zoom-unlocked? {})
        flow-drop-down?      @(ut/tracked-sub ::flow-drop-down? {})
        flowmaps             @(ut/tracked-sub ::flowmap-raw {})
        flowmaps-connections @(ut/tracked-sub ::flowmap-connections {})
        flow-map             @(ut/tracked-sub ::flowmap {})
        audio-playing?       @(ut/tracked-sub ::audio/audio-playing? {})
        running?             @(ut/tracked-subscribe [::is-running? :* flow-id true])
        has-done?            (has-done?)
        panel-height         (* (.-innerHeight js/window) hpct)
        flow-select          @(ut/tracked-sub ::selected-flow-block {})
        opts-map             @(ut/tracked-sub ::opts-map {})
        has-override?        (ut/ne? (get opts-map :overrides))
        warren-open?         (and (= (get @db/flow-editor-system-mode 0) "signals") ;; signals tab
                                  @(ut/tracked-subscribe [::bricks/flow?]) ;; flow panel open
                             )
        details-panel-height (/ panel-height 1.25)
        ppanel-height        (+ panel-height details-panel-height)]
    [bricks/reecatch
     [re-com/box :size "none" :width (px panel-width) :height (px ppanel-height) ;(if
                                                                                 ;@flow-details-panel?
                                                                                 ;(px (+
                                                                                 ;panel-height (/
                                                                                 ;panel-height
      :attr
      (if (and (not @db/dragging-flow-editor?) (not @bricks/dragging-editor?))
        {;:on-mouse-enter #(do (reset! bricks/over-flow? true)
         :on-mouse-over  #(when (not @bricks/over-flow?) (do (reset! bricks/over-flow? true) (reset! bricks/over-block? true)))
         :on-mouse-leave #(when (not (contains? (.-target %) (.-relatedTarget %)))
                            (do (reset! bricks/over-flow? false)
                                (reset! dragging-port? false)
                                (reset! dragged-port [])
                                (reset! bricks/over-block? false)))
         :on-drag-over   #(do (when (not @bricks/over-flow?) ;(contains? (.-target %)
                                (reset! bricks/over-flow? true)))
         :on-drag-leave  #(when (not (contains? (.-target %) (.-relatedTarget %))) (reset! bricks/over-flow? false))}
        {}) :style
      {:position         "fixed"
       :top              y-px
       :left             x-px
       :border-radius    "16px" ; (if (and @flow-details-panel? (not
       :z-index          200
       :font-family      (theme-pull :theme/base-font nil)
       :color            (theme-pull :theme/editor-font-color nil)
       :background-color (str (theme-pull :theme/editor-background-color nil) 99) ; "#000000"
       :backdrop-filter  "blur(3px)"
       :border           (str "6px solid " (theme-pull :theme/editor-outer-rim-color nil))
       :box-shadow       (let [block-id       :audio
                               talking-block? true]
                           (cond (and audio-playing? talking-block?) (str
                                                                       "1px 1px " (px (* 80
                                                                                         (+ 0.1 (get @db/audio-data block-id))))
                                                                       " "        (theme-pull :theme/editor-outer-rim-color nil))
                                 :else                               "none"))
       :filter           "drop-shadow(0.35rem 0.35rem 0.4rem rgba(0, 0, 0, 0.8))"} :child
      (if false ;@db/dragging-flow-editor?
        [re-com/box :size "auto" :align :center :justify :center :child "dragging panel. paused"]
        [re-com/v-box :width (px panel-width) :children
         [[re-com/h-box :justify :between :align :center :padding "6px" :children
           [[re-com/md-icon-button :src (at) :md-icon-name "zmdi-arrows" :style
             {;:background-color (theme-pull :theme/editor-rim-color nil) ;"#00000000"
              :color      (theme-pull :theme/editor-font-color nil)
              :cursor     "grab"
              :height     "15px"
              :margin-top "-9px"
              :font-size  "19px"} :attr {:on-mouse-down mouse-down-handler}]
            [re-com/h-box :gap "6px" :children
             [;; [re-com/h-box
              [re-com/md-icon-button :style
               {;:font-size "13px" :margin-top "-16px"
                :width "28px"} :on-click #(ut/tracked-dispatch [::toggle-flow-drop-down]) :md-icon-name
               (if flow-drop-down? "zmdi-chevron-up" "zmdi-chevron-down")]
              [re-com/md-icon-button :md-icon-name "zmdi-window-minimize" :on-click #(ut/tracked-dispatch [::bricks/toggle-flow])
               :style {:font-size "15px" :opacity 0.33 :cursor "pointer"}]
              (when flow-drop-down?
                [re-com/v-box :padding "8px" :size "auto" ;:width "400px"
                 :style
                 {:background-color "#000000"
                  :color            (theme-pull :theme/editor-outer-rim-color nil)
                  :position         "fixed"
                  :z-index          900
                  :border-radius    "0px 0px 0px 12px"
                  :border-left      (str "3px solid " (theme-pull :theme/editor-outer-rim-color nil))
                  :border-bottom    (str "3px solid " (theme-pull :theme/editor-outer-rim-color nil))
                  :top              24 ;(px y) ;(+ y 70))
                  :font-size        "19px"
                  :right            0} :children
                 (for [[v [file-path name]] (vec (sort (distinct (into (for [[k v] @(ut/tracked-subscribe [::http/flows])]
                                                                         [k nil])))))
                       :let                 [selected? (= (str v) (str flow-id))]]
                   [re-com/h-box :gap "10px" :justify :between :children
                    [[re-com/box :attr
                      {:on-click #(let [sub-flow-exec? (and (string? file-path) (not (nil? file-path)))]
                                    (if sub-flow-exec?
                                      (ut/tracked-dispatch [::http/load-flow-w-alias file-path v])
                                      (ut/tracked-dispatch [::set-selected-flow (str v)])))} :child (str v)]
                     [re-com/md-icon-button :src (at) :md-icon-name "zmdi-close"
                      :on-click
                      #(ut/tracked-dispatch [::unload-flow (str v)])
                      :style
                      {;:color bcolor
                       :cursor    "pointer"
                       :height    "12px"
                       :font-size "16px"}]] :style
                    (if selected?
                      {:text-decoration  "underline"
                       :font-weight      700
                       :border-radius    "8px"
                       :background-color (str (theme-pull :theme/editor-outer-rim-color nil) 40)}
                      {:cursor "pointer" :opacity 0.6}) :padding "4px"])])]]] :size "none" :width (px (- panel-width 12)) :height
           "25px" :style ;{:background-color (theme-pull
                         ;:theme/editor-rim-color nil)
           {:background    (str "linear-gradient(" (theme-pull :theme/editor-rim-color nil) ", transparent)")
            :border-radius "10px 10px 0px 0px"
            :color         (theme-pull :theme/editor-outer-rim-color nil)}]
          (flow-droppable
            ["meta-menu" :flow-port]
            [(- (first @db/context-modal-pos) x 10) (- (last @db/context-modal-pos) y 33)]
            [re-com/box :style
             {;:background-color "#000000"
              :border-radius "12px"} :attr
             {:id              "flow-canvas"
              :on-mouse-down   #(when (= (.-button %) 1)
                                  (do #_{:clj-kondo/ignore [:unresolved-symbol]}
                                      (bricks/tag-screen-position %) ;; event is magic in
                                      (when (nil? @flow-hover) ;(and ;(not @bricks/over-block?)
                                        (let [;block-body {:w 200 :h 60
                                              block-body {:w            125
                                                          :icon         "zmdi-functions"
                                                          :z            0
                                                          :ports        {:in {:x :any} :out {:out :any}}
                                                          :h            60
                                                          :right-click? true
                                                          :fn           '(fn [x] x)
                                                          :raw-fn       '(fn [x] x)
                                                          :data         {:flow-item {:category    ":rabbit-base"
                                                                                     :fn          '(fn [x] x)
                                                                                     :name        ":open-fn"
                                                                                     :raw-fn      '(fn [x] x)
                                                                                     :type        :open-fn
                                                                                     :icon        "zmdi-functions"
                                                                                     :types       {:x :any :out :any}
                                                                                     :expandable? true
                                                                                     :drag-meta   {:type :open-fn}}}}]
                                          (conn/add-flow-block (- (first @db/context-modal-pos) x 10)
                                                               (- (last @db/context-modal-pos) y 33)
                                                               block-body
                                                               :open-fn)))
                                      #_{:clj-kondo/ignore [:unresolved-symbol]}
                                      (.preventDefault %)))
              :on-context-menu (re-com/handler-fn #_{:clj-kondo/ignore [:unresolved-symbol]}
                                                  (bricks/tag-screen-position event) ;; event is magic in
                                                  (when (and (empty? @port-hover2) (empty? @flow-hover)) ; (nil?
                                                                                                         ; @flow-hover)
                                                                                                         ; ;(and
                                                    (reset! lookup-modal? true))
                                                  #_{:clj-kondo/ignore [:unresolved-symbol]}
                                                  (.preventDefault event))} :size "none" :width (px (- panel-width 12)) :height
             (px (- ppanel-height 35)) :child
             [re-com/v-box :size "1" :style
              (merge ;(theme-pull :theme/canvas-background-css nil)
                {:font-family (theme-pull :theme/base-font nil) :z-index 200 :border-radius "12px"}) ; "0px 0px 12px 12px"
              :children
              [;;   [tab-menu]
               (when (and @bricks/dragging? @bricks/over-flow? (not @dragging-port?) (not @bricks/on-block?)) ;; AND when
                                                                                                              ;; inside flow
                                                                                                              ;; canvas bounds
                 (let [rel-x      (first @db/context-modal-pos) ;(* (js/Math.floor (/ (first
                       left-pos   (- rel-x x)
                       rel-y      (last @db/context-modal-pos) ;(* (js/Math.floor (/ (last
                       top-pos    (- rel-y y)
                       dt         @drop-toggle?
                       x-bounds   (+ x panel-width)
                       y-bounds   (+ y ppanel-height)
                       load-flow? (= (get-in @bricks/dragging-body [:drag-meta :source-query]) :flows-sys)
                       in-bounds? (and @bricks/over-flow? (>= rel-x x) (>= rel-y y) (<= rel-x x-bounds) (<= rel-y y-bounds))]
                   (if (not in-bounds?)
                     (do ;very weird, but have lots of drop/drag in-fighting with crossing the
                       (reset! bricks/over-flow? false)
                       (reset! bricks/swap-layers? true)
                       (reset! bricks/over-block? false))
                     (reset! bricks/swap-layers? false))
                   (when in-bounds? ;; draw that shit
                     [re-com/box :child
                      (if load-flow? (if @drop-toggle? " *ADD to flow " " *LOAD this flow ") " add to flow canvas ") :align
                      :center :justify :center :style
                      {:background-color (str (theme-pull :theme/editor-background-color nil) 22) ;; "#00000022"
                       :border           "3px solid orange"
                       :position         "fixed"
                       :font-size        "26px"
                       :font-weight      700
                       :border-radius    "14px"
                       :left             left-pos
                       :top              top-pos} :width
                      (px (* 5 ;(get @bricks/dragging-body :w)
                             db/brick-size)) :height
                      (px (* 2 ;(get @bricks/dragging-body :h)
                             db/brick-size))])))
               (when (and (or (nil? @flow-hover) (not @flow-hover)) @lookup-modal?) [bricks/reecatch [lookup-modal]])
               [re-com/box :size "none" :style
                {;:border "2px solid red"
                 :overflow "hidden"} :width (px (- panel-width 12)) :height (px (- ppanel-height 35)) :child
                [(reagent/adapt-react-class zpan/TransformWrapper)
                 {;:key (str @db/pan-zoom-offsets)
                  :ref              #(reset! db/zoomer %)
                  :onInit           (fn [comp] (reset! db/zoomer-state comp))
                  :limitToBounds    false ;true ; false ; true ; false ;true ;false
                  :centerZoomedOut  false ;true
                  :disabled         (or @flow-hover
                                        (not zoom-unlocked?) ;; @pan-lock?
                                        (not @bricks/over-flow?))
                  :onPanningStop    #(do (reset! db/pan-zoom-offsets [(.-positionX (.-state %)) (.-positionY (.-state %))
                                                                      (.-scale (.-state %))])
                                         (reset! db/panning? false))
                  :onPanningStart   #(reset! db/panning? true)
                  :onWheelStop      #(do (reset! db/pan-zoom-offsets [(.-positionX (.-state %)) (.-positionY (.-state %))
                                                                      (.-scale (.-state %))])
                                         (reset! db/zooming? false))
                  :onWheelStart     #(reset! db/zooming? true)
                  :doubleClick      {:disabled false}
                  :initialScale     (nth @db/pan-zoom-offsets 2)
                  :initialPositionX (nth @db/pan-zoom-offsets 0)
                  :initialPositionY (nth @db/pan-zoom-offsets 1)
                  :minScale         0.2}
                 [(reagent/adapt-react-class zpan/TransformComponent)
                  [re-com/v-box ;:style {:border "1px dashed hotpink"}
                   :children
                   [[bricks/reecatch
                     (if warren-open?
                       [re-com/gap :size "10px"] ;; no need to render a flow if we have other
                       [flow-grid panel-width ppanel-height x y flowmaps-connections flow-id flow-map])]
                    (let [;ph 6-0-0 ;; siz hundo
                          [xx yy] @db/flow-detached-coords]
                      [:svg
                       {:style {;; :width  (px pw) ;(px ww) ;"6200px" ;; big ass render nothing
                                :width          canvas-width ;"3200px" ;"6400px" ;(px ww)
                                :height         canvas-height ;"2400px" ;"3600px" ;(px hh)
                                :position       "fixed"
                                :pointer-events "none"
                                :z-index        100}} (draw-lines (generate-coords xx yy))])]]]]]
               [re-com/h-box :gap "6px" :width
                (if @(ut/tracked-subscribe [::bricks/flow-editor?]) (px (* ww 0.5035)) (px (* ww 0.793))) :style
                {:position "fixed"
                 :height   "60px"
                 :left     (if @(ut/tracked-subscribe [::bricks/flow-editor?])
                             (if (nil? flow-select) (+ (last @db/flow-editor-system-mode) 40) 640)
                             45)
                 :bottom   0} :children
                [(when has-done?
                   (if running? ;; (or running? chans-open?)
                     [re-com/box :child
                      [re-com/md-icon-button :src (at) :md-icon-name "zmdi-refresh-sync" :class
                       (if (not running?) "rotate-reverse linear infinite" "rotate linear infinite") :style
                       {:color            (str (theme-pull :theme/editor-outer-rim-color nil))
                        :cursor           "pointer"
                        :transform-origin "21.5px 22px"
                        :filter           "drop-shadow(0.25rem 0.35rem 0.4rem rgba(0, 0, 0, 0.44))"
                        :font-size        "43px"} :on-click #(ut/tracked-dispatch [::run-current-flowmap])] :style
                      {:z-index 98 :color "white"}]
                     [re-com/box :child
                      [draggable-play
                       [re-com/md-icon-button :src (at) :md-icon-name "zmdi-play" :attr
                        {:on-mouse-enter #(reset! editor-tooltip-atom (str "run flow live"))
                         :on-mouse-leave #(reset! editor-tooltip-atom nil)} :on-click
                        #(ut/tracked-dispatch [::run-current-flowmap]) :style
                        {:color     (str (theme-pull :theme/editor-outer-rim-color nil))
                         :cursor    "pointer"
                         :filter    "drop-shadow(0.25rem 0.35rem 0.4rem rgba(0, 0, 0, 0.44))"
                         :font-size "55px"}] flow-id] :style
                      {;:position "fixed"
                       :opacity 0.45
                       :z-index 98
                       :color   "white"}]))
                 (when true ;has-done?
                   [re-com/box :child
                    [re-com/md-icon-button :src (at) :md-icon-name "zmdi-stop" :attr
                     {:on-click       #(ut/tracked-dispatch [::wfx/request :default
                                                             {:message {:kind        :kill-flow
                                                                        :flow-id     flow-id
                                                                        :client-name @(ut/tracked-subscribe
                                                                                        [::bricks/client-name])}
                                                              :timeout 15000000}])
                      :on-mouse-enter #(reset! editor-tooltip-atom (str "kill flow (on server) and all open channels"))
                      :on-mouse-leave #(reset! editor-tooltip-atom nil)} :style
                     {:color     (str (theme-pull :theme/editor-outer-rim-color nil))
                      :cursor    "pointer"
                      :filter    "drop-shadow(0.25rem 0.35rem 0.4rem rgba(0, 0, 0, 0.44))"
                      :font-size "55px"}] :style
                    {;:position "fixed"
                     :opacity    0.45
                     :z-index    98
                     :margin-top "-1px"
                     :color      "white"}])
                 (when (not running?)
                   [re-com/box :child
                    [re-com/md-icon-button :src (at) :md-icon-name "zmdi-time-interval" :on-click #(reset! editor-mode :scheduler)
                     :attr
                     {:on-mouse-enter #(reset! editor-tooltip-atom (str "set flow schedule (on server)"))
                      :on-mouse-leave #(reset! editor-tooltip-atom nil)} :style
                     {:color     (str (theme-pull :theme/editor-outer-rim-color nil))
                      :cursor    "pointer"
                      :filter    "drop-shadow(0.25rem 0.35rem 0.4rem rgba(0, 0, 0, 0.44))"
                      :font-size "43px"}] :style
                    {;:position "fixed"
                     :opacity    0.22
                     :margin-top "4px"
                     :z-index    98
                     :color      "white"}])
                 (when (not running?)
                   [re-com/box :child
                    [re-com/md-icon-button :src (at) :md-icon-name "zmdi-save" :attr
                     {:on-mouse-enter #(reset! editor-tooltip-atom (str "save flow"))
                      :on-mouse-leave #(reset! editor-tooltip-atom nil)} :on-click
                     #(do (ut/tracked-dispatch [::http/save-flow
                                                {:flowmaps             flowmaps ;; @(ut/tracked-subscribe
                                                 :opts                 @(ut/tracked-subscribe [::opts-map])
                                                 :zoom                 @db/pan-zoom-offsets
                                                 :flow-id              flow-id
                                                 :flowmaps-connections flowmaps-connections} flow-id])
                          (ut/dispatch-delay 3000 [::conn/clear-query-history :flows-sys])) :style
                     {:color     (str (theme-pull :theme/editor-outer-rim-color nil))
                      :cursor    "pointer"
                      :filter    "drop-shadow(0.25rem 0.35rem 0.4rem rgba(0, 0, 0, 0.44))"
                      :font-size "43px"}] :style
                    {;:position "fixed"
                     :opacity    0.22
                     :z-index    98
                     :margin-top "4px"
                     :color      "white"}])
                 (when true ; (not running?)
                   [re-com/box :child
                    [re-com/md-icon-button :src (at) :attr
                     {:on-mouse-enter #(reset! editor-tooltip-atom (str "new flow"))
                      :on-mouse-leave #(reset! editor-tooltip-atom nil)} :on-click #(ut/tracked-dispatch [::new-flow])
                     :md-icon-name "zmdi-widgets" :style
                     {:color     (str (theme-pull :theme/editor-outer-rim-color nil))
                      :cursor    "pointer"
                      :filter    "drop-shadow(0.25rem 0.35rem 0.4rem rgba(0, 0, 0, 0.44))"
                      :font-size "43px"}] :style
                    {;:position "fixed"
                     :opacity    0.22
                     :z-index    98
                     :margin-top "4px"
                     :color      "white"}])
                 (if (and @dragging-port? @bricks/over-flow?)
                   [(reagent/adapt-react-class rdnd/Droppable)
                    {:types   [:flow-port]
                     :on-drop #(let [[srcf srcl _] @dragged-port
                                     src           (if (= srcl :out)
                                                     srcf ;; no alias for simple */out
                                                     (keyword (str (gn srcf) "/" (gn srcl))))]
                                 (reset! dragging-port? false)
                                 (ut/tapp>> [:dropped src :to :done])
                                 (set-delay! port-hover nil 2500)
                                 (connect-ports src :done))}
                    (let [ccolor (theme-pull :theme/editor-outer-rim-color nil)]
                      [re-com/h-box :attr
                       {:on-drag-enter #(reset! port-hover [:done :done])
                        :on-drag-over  #(when (not (= @port-hover [:done :done])) (reset! port-hover [:done :done]))
                        :on-drag-leave #(reset! port-hover nil)} :gap "10px" :children
                       [[re-com/box :child "drop here for: "]
                        [re-com/box :style {:font-weight 700} :child "all done & return value"]] :size "none" :align :center
                       :justify :center :style
                       {:font-size        "27px"
                        :background-color (str ccolor "33")
                        :backdrop-filter  "blur(3px)"
                        :border           (str "4px dashed " ccolor)
                        :margin-top       "4px"} :height "55px" :width
                       (px (- (if @(ut/tracked-subscribe [::bricks/flow-editor?]) (* ww 0.5035) (* ww 0.793)) 210))])]
                   [re-com/box :child (str (or @editor-tooltip-atom "")) :size "none" :height "80%" :width
                    (px (- (if @(ut/tracked-subscribe [::bricks/flow-editor?]) (* ww 0.5035) (* ww 0.793)) 210)) :align :center
                    :justify :center :style
                    {;:border "2px solid green"
                     :font-size   "19px"
                     :font-weight 700
                     :overflow    "hidden"}])]]
               [re-com/h-box :style
                {:position    "fixed"
                 :top         34
                 :right       23
                 :opacity     0.33
                 :font-weight 700
                 :color       (str (theme-pull :theme/editor-outer-rim-color nil))
                 :font-size   "29px"} :gap "10px" :children
                [[edit-flow-title flow-id 1450]
                 [re-com/md-icon-button :src (at) :md-icon-name (if watched? "zmdi-eye" "zmdi-eye-off") 
                  :on-click
                  #(if watched?
                     (ut/tracked-dispatch [::wfx/request :default
                                           {:message {:kind        :remove-flow-watcher
                                                      :client-name @(ut/tracked-subscribe [::conn/client-name])
                                                      :flow-id     flow-id}
                                            :timeout 50000}])
                     (let [flowmap              @(ut/tracked-subscribe [::flowmap])
                           flowmaps-connections @(ut/tracked-subscribe [::flowmap-connections])
                           client-name          @(ut/tracked-subscribe [::conn/client-name])
                           server-flowmap       (process-flowmap2 flowmap flowmaps-connections flow-id)
                           comps                (get server-flowmap :components) ;; (assoc (get
                           running-view-subs    (vec (for [[k v] comps
                                                           :when (get v :view)]
                                                       [flow-id (keyword (str (ut/replacer (str k) ":" "") "-vw"))]))
                           running-subs         (vec (for [k (keys comps)] [flow-id k]))
                           running-subs         (vec (into running-subs running-view-subs))]
                       (ut/tracked-dispatch [::wfx/request :default
                                             {:message {:kind        :sub-to-running-values
                                                        :flow-keys   running-subs
                                                        :flow-id     flow-id
                                                        :client-name client-name}
                                              :timeout 15000000}]))) 
                                              :style
                  {:cursor "pointer" :margin-top "8px" :font-size "30px"}]]]
               (when has-override?
                 [re-com/h-box :align :center :justify :center :gap "5px" :style
                  {:position    "fixed"
                   :font-size   "18px"
                   :font-weight 700
                   :opacity     0.45
                   :color       (str (theme-pull :theme/editor-outer-rim-color nil))
                   :top         63
                   :right       79} :children
                  [[re-com/box :style {:font-size "32px" :margin-top "6px"} :child "*"]
                   [re-com/box :child "includes overrides, check 'flow options' map"]]])
               [re-com/box :child
                [re-com/md-icon-button :src (at) :md-icon-name (if zoom-unlocked? "zmdi-lock-open" "zmdi-lock") :on-click
                 #(ut/tracked-dispatch [::toggle-lock]) :attr
                 {:on-mouse-enter #(reset! editor-tooltip-atom (str "hold C to unlock flow pan & zoom"))
                  :on-mouse-leave #(reset! editor-tooltip-atom nil)} :style
                 {:color     (if zoom-unlocked? "red" (str (theme-pull :theme/editor-outer-rim-color nil)))
                  :cursor    "pointer"
                  :font-size "43px"}] :style
                {:opacity          (if zoom-unlocked? 1.0 0.22)
                 :margin-top       "4px"
                 :z-index          98
                 :position         "fixed"
                 :right            10
                 :bottom           30
                 :background-color "#00000022"
                 :color            "white"}]
               (when (and @dragging-port? (not (= [0 0] @tentacle-pos)))
                 [:svg {:style {:width "100%" :height "100%" :position "fixed" :pointer-events "none" :z-index 100}}
                  (draw-tentacle [(generate-tentacle-coord)])])
               [bricks/reecatch [flow-details-panel panel-height panel-width details-panel-height]]]]])]])]]))


