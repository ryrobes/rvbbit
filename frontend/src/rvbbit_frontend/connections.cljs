(ns rvbbit-frontend.connections
  (:require
    [re-frame.core           :as re-frame]
    [re-frame.alpha          :as rfa]
    [rvbbit-frontend.http    :as http]
    [rvbbit-frontend.db      :as db]
    [rvbbit-frontend.utility :as ut]
    [reagent.core            :as reagent]
    [clojure.walk            :as walk]
    [clojure.edn             :as edn]
    [re-com.core             :as    re-com
                             :refer [at]]
    [re-com.util             :refer [px]]
    [day8.re-frame.undo      :as    undo
                             :refer [undoable]]
    [clojure.string          :as cstr]
    [clojure.set             :as cset]
    [cljs-time.core] ;; womp[ womp]
    [websocket-fx.core       :as wfx]))

;; (defn namespaced [x] (try (namespace x) (catch :default _ nil))) ;; yes this is a hacky override

(re-frame/reg-sub ::sub-flow-incoming (fn [db [_]] (get db :sub-flow-incoming)))

(re-frame/reg-event-fx
 ::copy-to-clipboard
 (fn [_ [_ text]]
   {:clipboard/copy text}))

(re-frame/reg-fx
 :clipboard/copy
 (fn [text]
   (let [temp-element (js/document.createElement "textarea")]
     (set! (.-value temp-element) text)
     (.appendChild js/document.body temp-element)
     (.select temp-element)
     (.setSelectionRange temp-element 0 (.-length (.-value temp-element)))
     (js/document.execCommand "copy")
     (.removeChild js/document.body temp-element))))

(defn gn [x] (try (name x) (catch :default _ x)))
(defn gns [x] (try (namespace x) (catch :default _ x)))
(defn gns? [x] (not (nil? (try (namespace x) (catch :default _ false)))))

(defn spawn-open-input-block
  [starting-val] ;;; need to simplify and merge logic from flow-droppable (flows, params, etc)
  (let [starting-val (try (edn/read-string starting-val) (catch :default _ starting-val))
        dtype (keyword (ut/data-typer starting-val))
        try-read (fn [x] (try (edn/read-string x) (catch :default _ x)))
        [x y] @db/flow-detached-coords
        flow? (= (get starting-val :category) ":flow") ;; also packaged flows... not raws...
        sub-flow? (= (get starting-val :category) ":sub-flows")
        part-key? (true? (and (map? starting-val)
                              (not flow?)
                              (not (nil? (get starting-val :category)))
                              (not (nil? (get starting-val :name)))
                              (not (nil? (get starting-val :full_map)))))
        lookup-map (if part-key? ;; (or part-key? flow?)
                     (try (merge (-> (edn/read-string (get starting-val :full_map)))
                                 starting-val)
                          (catch :default _ {}))
                     {})
        raw-fn? (and (= dtype :list) (= (first starting-val) 'fn))
        _ (when flow? (ut/tracked-dispatch [::http/load-sub-flow (get starting-val :file_path)]))
        sub-flow (when flow? @(ut/tracked-subscribe [::sub-flow-incoming]))
        open-block-body {:w            125
                         :h            60
                         :z            0
                         :data         {:drag-meta {:type :open-block} :flow-item {:expandable? true} :user-input starting-val}
                         :right-click? true ;; important for add-block to get current coords
                         :ports        {:in {} :out {:out dtype}}}
        flow-as-fn (if flow?
                     (try (let [blocks      (set (keys (get sub-flow :map)))
                                base-conns  (set (for [[_ v2] (get sub-flow :connections)] (keyword (gns v2))))
                                no-inputs   (cset/difference blocks base-conns)
                                flow-inputs (into {}
                                                  (for [i no-inputs]
                                                    {i (get-in sub-flow
                                                               [:map i :ports :out
                                                                (first (keys (get-in sub-flow [:map i :ports :out])))])}))
                                done-block  (try (first (first (filter (fn [x] (= (second x) :done))
                                                                 (get sub-flow :connections))))
                                                 (catch :default _ :error))]
                            {:w         200
                             :h         100
                             :z         0
                             :data      (-> {} ;; (edn/read-string (get starting-val :full_map))
                                            (assoc-in [:drag-meta :done-block] done-block)
                                            (assoc-in [:drag-meta :type] :sub-flow))
                             :sub-flow  sub-flow
                             :file-path (get starting-val :file_path)
                             :flow-id   (get sub-flow :flow-id)
                             :icon      "zmdi-puzzle-piece"
                             :ports     {:in  flow-inputs ;(dissoc (get flow-item :types) :out)
                                         :out (get-in sub-flow [:map done-block :ports :out])}})
                          (catch :default _ {}))
                     {})
        other-fn
          (let [f2i       (ut/function-to-inputs lookup-map)
                arg-walks (into {}
                                (for [e     (keys f2i)
                                      :when (cstr/ends-with? (str e) "+")]
                                  {(-> (str e)
                                       (ut/replacer "+" "")
                                       (ut/replacer ":" "")
                                       keyword)
                                     e}))]
            (merge
              (when sub-flow?
                {:flow-path   (get lookup-map :flow-path) ;; for unpacking later
                 :sub-flow-id (get lookup-map :flow-id)})
              {:w            125
               :h            60
               :z            0
               :data         {:flow-item {:category       (get lookup-map :category) ;; important
                                                                                     ;; for lookup
                                          :type           (if (not sub-flow?)
                                                            (try-read (get lookup-map :name))
                                                            (get lookup-map :name)) ;; important
                                          :name           (get lookup-map :name) ;; important for
                                                                                 ;; lookup
                                          :icon           (get lookup-map :icon)
                                          :inputs         (ut/postwalk-replacer arg-walks (get lookup-map :inputs))
                                          :defaults       (get lookup-map :defaults)
                                          :types          (ut/postwalk-replacer arg-walks (get lookup-map :types))
                                          :style          (get lookup-map :style)
                                          :selected-style (get lookup-map :selected-style)
                                          :expandable?    true
                                          :required       (get lookup-map :required)}
                              :drag-meta {:type (if (not sub-flow?) (try-read (get lookup-map :name)) (get lookup-map :name))}}
               :right-click? true ;; important for add-block to get current coords
               :ports        {:in  (if sub-flow? (select-keys (get lookup-map :types) (get lookup-map :inputs)) f2i)
                              :out {:out (get-in lookup-map [:types :out] :any)}}
               :icon         (get lookup-map :icon)}))
        open-fn-body (let [;port-map (try
                           port-map (try (ut/function-to-inputs (second starting-val)) (catch :default _ {:value :any}))]
                       {:fn           starting-val
                        :w            125
                        :h            60
                        :z            0
                        :raw-fn       starting-val
                        :icon         "zmdi-functions"
                        :right-click? true ;; important for add-block to get current coords
                        :ports        {:in port-map :out {:out :any}}
                        :data         {:flow-item {:category    ":rabbit-base"
                                                   :fn          starting-val
                                                   :name        ":open-fn"
                                                   :raw-fn      starting-val
                                                   :type        :open-fn
                                                   :icon        "zmdi-functions"
                                                   :types       (merge port-map {:out :any})
                                                   :expandable? true
                                                   :drag-meta   {:type :open-fn}}}})
        grid-size db/snap-to-grid
        sx (- (first @db/context-modal-pos) x 10)
        sy (- (last @db/context-modal-pos) y 33)
        snapped-x (* grid-size (Math/round (/ sx grid-size)))
        snapped-y (* grid-size (Math/round (/ sy grid-size)))
        block-body (cond raw-fn?   open-fn-body
                         part-key? other-fn
                         flow?     flow-as-fn
                         :else     open-block-body)
        bname (cond raw-fn?   :open-fn
                    part-key? (try-read (get lookup-map :name)) ;;:test ;(get lookup-map )
                    flow?     (try-read (get sub-flow :flow-id))
                    :else     :open-input)]
    [snapped-x snapped-y block-body bname]))

(re-frame/reg-event-db ::select-block ;; flow block
                       (undoable)
                       (fn [db [_ bid]] (assoc db :selected-flow-block bid)))

(re-frame/reg-event-db ::update-flowmap-key2 ;;; just used for add block
                       (undoable)
                       (fn [db [_ bid kkey vval]]
                         (if (nil? kkey) ;; fulll body update
                           (assoc-in db [:flows (get db :selected-flow) :map bid] vval)
                           (assoc-in db [:flows (get db :selected-flow) :map bid kkey] vval))))

(re-frame/reg-sub
  ::reserved-type-keywords ;; @(ut/tracked-subscribe [::reserved-type-keywords])
  (fn [db]
    (let [lib-keys (try (vec (map edn/read-string (map :name (get-in db [:data :flow-fn-all-sys])))) (catch :default _ []))]
      (vec (into lib-keys
                 (into (keys (get-in db [:flows (get db :selected-flow) :map]))
                       (vec (distinct (ut/get-all-values (get-in db [:flows (get db :selected-flow)]) :type)))))))))

(defn add-flow-block
  [x y & [body bid no-select?]]
  (let [;bid (if bid bid (keyword (str "open-input-" (count @(ut/tracked-subscribe
        _ (ut/tapp>> [:add-block bid x y body])
        bid                (keyword (ut/replacer (str (gn bid)) #"/" "-"))
        bid                (if bid bid :open-input)
        safe-keys-reserved @(ut/tracked-subscribe [::reserved-type-keywords])
        bid                (ut/safe-key bid safe-keys-reserved)
        zoom-multi         (get @db/pan-zoom-offsets 2)
        zoom-offset-x      (get @db/pan-zoom-offsets 0)
        zoom-offset-y      (get @db/pan-zoom-offsets 1)
        drop-x             (- (/ x zoom-multi) (/ zoom-offset-x zoom-multi))
        drop-y             (- (/ y zoom-multi) (/ zoom-offset-y zoom-multi))
        grid-size          db/snap-to-grid
        drop-x             (* grid-size (Math/round (/ drop-x grid-size)))
        drop-y             (* grid-size (Math/round (/ drop-y grid-size)))
        body               (if body
                             (if (or (cstr/includes? (str bid) "open-input")
                                     (cstr/includes? (str bid) "open-fn")
                                     (get body :right-click? false))
                               (merge body {:x drop-x :y drop-y :z 0})
                               body) ;; override for right click open block
                             {:w 200 :h 50 :x drop-x :y drop-y :z 0 :ports {:in {:in :string} :out {:out :string}}})
        body               (if (get-in body [:data :sub-flow-id])
                             (-> body
                                 (assoc :sub-flow-id (get-in body [:data :sub-flow-id]))
                                 (assoc :flow-path (get-in body [:data :flow-path])))
                             body)
        body               (-> body
                               (assoc :data (select-keys (get body :data) [:flow-item :drag-meta]))
                               (assoc-in [:data :flow-item :inputs] (vec (keys (get-in body [:ports :in]))))) ;; clean up in
       ]
    (when (not no-select?) (ut/tracked-dispatch [::select-block bid]))
    (ut/tracked-dispatch [::update-flowmap-key2 bid nil body])))



(re-frame/reg-event-db
 ::update-solver-fn-runs
 (fn [db [_ keypath value]]
   (assoc-in db (into [:solver-fn :runs] keypath) value)))

(re-frame/reg-sub
 ::solver-fn-runs
 (fn [db {:keys [keypath]}]
   (get-in db (into [:solver-fn :runs] keypath))))

(re-frame/reg-sub
 ::solver-fn-runs-keys
 (fn [db {:keys [keypath]}]
   (vec (keys (get-in db (into [:solver-fn :runs] keypath))))))


(re-frame/reg-event-db
 ::update-solver-fn-lookup
 (fn [db [_ keypath value]] ;;; not *really* a keypath, used as an assoc with a compound key
   (assoc-in db [:solver-fn :lookup keypath] value)))

(re-frame/reg-sub
 ::solver-fn-lookup
 (fn [db {:keys [keypath]}] ;;; not *really* a keypath, used as an assoc with a compound key
   (get-in db [:solver-fn :lookup keypath])))



(declare logic-and-params-fn)

(re-frame/reg-sub ::logic-and-params (fn [_ {:keys [m p]}] (logic-and-params-fn m p))) ;; cheeky sub hack under certain conditions?

(defn logic-and-params [m p] @(ut/tracked-sub ::logic-and-params {:m m :p p}))

(defn logic-and-params-fn
  [block-map panel-key]
  (if (try (and (ut/ne? block-map) (or (map? block-map) (vector? block-map) (keyword? block-map))) (catch :default _ false))
    (let [;;valid-body-params      (vec (filter #(and (keyword? %) (cstr/includes? (str %)
          client-name @(ut/tracked-sub ::client-name {})
          valid-body-params (vec (ut/get-compound-keys block-map))
          workspace-params (into {}
                                 (for [k valid-body-params] ;; deref here?
                                   {k @(ut/tracked-sub ::clicked-parameter-key-alpha {:keypath [k]})}))
          value-walks-targets (filter #(and (cstr/includes? (str %) ".") (not (cstr/includes? (str %) ".*"))) valid-body-params)
          value-walks (into {}
                            (for [k value-walks-targets] ;all-sql-call-keys]
                              (let [fs    (ut/splitter (ut/safe-name k) "/")
                                    gs    (ut/splitter (last fs) ".")
                                    ds    (keyword (first fs))
                                    row   (try (int (last gs)) (catch :default _ "label"))
                                    field (keyword (first gs))]
                                {k (if (not (integer? row))
                                     (str field)
                                     (get-in @(ut/tracked-sub ::sql-data-alpha {:keypath [ds]}) [row field]))})))
          condi-walks-targets (distinct (filter #(cstr/includes? (str %) "condi/") valid-body-params))
          condi-walks (into {}
                            (for [k condi-walks-targets]
                              {k @(ut/tracked-sub ::condi-value {:condi-key (keyword (last (ut/splitter (ut/safe-name k) "/")))})}))
          into-walk-map2 (fn [obody]
                           (let [;obody (ut/postwalk-replacer condi-walks orig-body)
                                 kps       (ut/extract-patterns obody :into 3) ;(kv-map-fn
                                                                               ;obody)
                                 logic-kps (into {} (for [v kps] (let [[_ this that] v] {v (into this that)})))]
                             (ut/postwalk-replacer logic-kps obody)))
          if-walk-map2 (fn [obody]
                         (let [;obody (ut/postwalk-replacer condi-walks orig-body)
                               kps       (ut/extract-patterns obody :if 4) ;(kv-map-fn
                                                                           ;obody)
                               logic-kps (into {}
                                               (for [v kps]
                                                 (let [[_ l this that] v]
                                                   {v (if (logic-and-params-fn l nil) ;; l
                                                        this
                                                        that)})))]
                           (ut/postwalk-replacer logic-kps obody)))
          when-walk-map2 (fn [obody]
                           (let [kps       (ut/extract-patterns obody :when 3)
                                 logic-kps (into {} (for [v kps] (let [[_ l this] v] {v (when l this)})))]
                             (ut/postwalk-replacer logic-kps obody)))
          =-walk-map2 (fn [obody]
                        (let [kps       (ut/extract-patterns obody := 3)
                              logic-kps (into {} (for [v kps] (let [[_ that this] v] {v (= (str that) (str this))})))]
                          (ut/postwalk-replacer logic-kps obody)))
          ;; string-walk (fn [num obody]
          ;;               (let [kps       (ut/extract-patterns obody :str num)
          ;;                     logic-kps (into {} (for [v kps] (let [[_ & this] v] {v (apply str this)})))]
          ;;                 (ut/postwalk-replacer logic-kps obody)))

          string-walk (fn [obody]
                        (let [process-string3 (fn [form]
                                                (if (and (vector? form)
                                                         (= (first form) :str)
                                                         (> (count form) 1))
                                                  (apply str (rest form))
                                                  form))]
                          (walk/postwalk process-string3 obody)))

          case-walk (fn [obody]
                      (let [kps       (ut/extract-patterns obody :case 2)
                            logic-kps (into {} (for [v kps] (let [[_ l] v] {v (ut/vectorized-case l)})))]
                        (ut/postwalk-replacer logic-kps obody)))
          get-in-walk (fn [obody]
                        (let [kps       (ut/extract-patterns obody :get-in 2)
                              logic-kps (into {} (for [v kps] (let [[_ [data kp]] v] {v (get-in data kp)})))]
                          (ut/postwalk-replacer logic-kps obody)))
          invert-hex-color-walk (fn [obody]
                                  (let [kps       (ut/extract-patterns obody :invert-hex-color 2)
                                        logic-kps (into {} (for [v kps] (let [[_ hhex] v] {v (ut/invert-hex-color hhex)})))]
                                    (ut/postwalk-replacer logic-kps obody)))
          tetrads-walk (fn [obody]
                         (let [kps       (ut/extract-patterns obody :tetrads 2)
                               logic-kps (into {} (for [v kps] (let [[_ hhex] v] {v (ut/tetrads hhex)})))]
                           (ut/postwalk-replacer logic-kps obody)))
          complements-walk (fn [obody]
                             (let [kps       (ut/extract-patterns obody :complements 2)
                                   logic-kps (into {} (for [v kps] (let [[_ hhex] v] {v (ut/complements hhex)})))]
                               (ut/postwalk-replacer logic-kps obody)))
          split-complements-walk (fn [obody]
                                   (let [kps       (ut/extract-patterns obody :split-complements 2)
                                         logic-kps (into {} (for [v kps] (let [[_ hhex] v] {v (ut/split-complements hhex)})))]
                                     (ut/postwalk-replacer logic-kps obody)))
          triads-walk (fn [obody]
                        (let [kps       (ut/extract-patterns obody :triads 2)
                              logic-kps (into {} (for [v kps] (let [[_ hhex] v] {v (ut/triads hhex)})))]
                          (ut/postwalk-replacer logic-kps obody)))
          singles {:text   str
                   :>>     (fn [[x y]] (true? (> x y)))
                   :<<     (fn [[x y]] (true? (< x y)))
                   ;:str    (fn [args] (if (vector? args) (cstr/join "" (apply str args)) (str args)))
                   ;:string (fn [args] (if (vector? args) (cstr/join "" (apply str args)) (str args)))
                   }
          solver-clover-walk
          (fn [obody]
            (let [;kps       (ut/extract-patterns obody :run-solver 2)
                  kps        (ut/extract-patterns-with-keypath obody :run-solver 2)
                  logic-kps (into
                             {}
                             (for [[fkp v] kps]
                               (let [[_ & this]                v
                                     fkp                       (vec (into [:conns panel-key] fkp))
                                     override?                 (try (map? (first this)) (catch :default _ false)) ;; not a vec input call, completely new solver map
                                     [[solver-name input-map]] (if override? [[:raw-custom-override {}]] this)
                                     unresolved-req-hash       (hash (if false ;override?
                                                                       fkp ;this 
                                                                       ;[solver-name fkp client-name]
                                                                       [solver-name fkp]))
                                     rtype                     (get (first this) :type :unknown)
                                     clover-kps                (vec (filter #(cstr/includes? (str %) "/") (ut/deep-flatten (conj [(first this)] input-map))))
                                     resolved-input-map        (logic-and-params input-map panel-key)
                                     resolved-full-map         (when override? (logic-and-params (first this) panel-key))
                                     unique-resolved-map       (if override? resolved-full-map resolved-input-map) ;; for tracker atom key triggers
                                     new-solver-name           (str (ut/replacer (str solver-name) ":" "") unresolved-req-hash)
                                     sub-param                 (keyword (str "solver/" new-solver-name))
                                     req-map                   (merge
                                                                {:kind             :run-solver-custom
                                                                 :solver-name      solver-name
                                                                 :ui-keypath       [panel-key]
                                                                 :temp-solver-name (keyword new-solver-name)
                                                                 :input-map        resolved-input-map
                                                                 :client-name      client-name}
                                                                (when override? {:override-map resolved-full-map}))
                                     websocket-status          (get @(ut/tracked-sub ::http/websocket-status {}) :status)
                                     online?                   (true? (= websocket-status :connected))
                                     run?                      (=
                                                                (get-in @db/solver-fn-runs [panel-key sub-param])
                                                                ;@(ut/tracked-sub ::solver-fn-runs-keys {:keypath [panel-key sub-param]})
                                                                unique-resolved-map)
                                     lets-go?                  (and online? (not run?))
                                    ;;  _ (when lets-go?
                                    ;;      (ut/tapp>> [:run-solver-req-map-conns! override? (str (first this)) lets-go? (not run?) req-map
                                    ;;                  @db/solver-fn-runs]))
                                     _ (when lets-go?
                                         (swap! db/kit-run-ids assoc (keyword new-solver-name) (ut/generate-uuid))
                                         (ut/tracked-dispatch [::wfx/push :default req-map])
                                         (swap! db/solver-fn-lookup assoc fkp sub-param)
                                         ;(ut/tracked-dispatch [::update-solver-fn-lookup fkp sub-param])
                                         (swap! db/solver-fn-runs assoc-in [panel-key sub-param] unique-resolved-map)
                                         ;(ut/tracked-dispatch [::update-solver-fn-runs [panel-key sub-param] unique-resolved-map])
                                         )
                                     _ (when (and lets-go?
                                                  (not (some #(= % :time/now-seconds) clover-kps))
                                                  (not (some #(= % :time/second) clover-kps)))
                                         (ut/dispatch-delay 100 [::http/insert-alert (ut/solver-alert-clover fkp clover-kps :conn rtype) 11 1.7 3]))]
                                 {v sub-param})))]
              (ut/postwalk-replacer logic-kps obody)))
          obody-key-set (ut/deep-flatten block-map)
          has-fn? (fn [k] (some #(= % k) obody-key-set))
          out-block-map (cond->> block-map
                          true                      (ut/namespaced-swapper "this-block" (ut/replacer (str panel-key) #":" ""))
                          true                      (ut/postwalk-replacer {:*this-block* panel-key})
                          (has-fn? :run-solver)     solver-clover-walk ;;(solver-clover-walk client-name panel-key)
                          (ut/ne? value-walks)      (ut/postwalk-replacer value-walks)
                          (ut/ne? condi-walks)      (ut/postwalk-replacer condi-walks)
                          (ut/ne? workspace-params) (ut/postwalk-replacer workspace-params)
                          ;; (has-fn? :str)        (string-walk 2) ;; TODO, remove all these
                          ;; (has-fn? :str)        (string-walk 3)
                          ;; (has-fn? :str)        (string-walk 4)
                          ;; (has-fn? :str)        (string-walk 5)
                          ;; (has-fn? :str)        (string-walk 6) ;; TODO REMOVE ALL THIS
                          (has-fn? :str)        string-walk
                          (has-fn? :get-in)         get-in-walk
                          (has-fn? :=)              =-walk-map2
                          (has-fn? :if)             if-walk-map2
                          (has-fn? :when)           when-walk-map2
                          (has-fn? :into)           into-walk-map2
                          (has-fn? :invert-hex-color) invert-hex-color-walk
                          (has-fn? :tetrads)        tetrads-walk
                          (has-fn? :complements)    complements-walk
                          (has-fn? :split-complements)    split-complements-walk
                          (has-fn? :triads)          triads-walk
                          (ut/ne? singles)          (ut/postwalk-replacer singles)
                          (has-fn? :case)           case-walk)
          templated-strings-vals (vec (filter #(cstr/includes? (str %) "/") (ut/deep-template-find out-block-map))) ;; ignore
                                                                                                                    ;; non
          templates? (ut/ne? templated-strings-vals)
          _ (when templates? (ut/tapp>> [:replacing-string-templates... templated-strings-vals out-block-map]))
          templated-strings-walk (if templates?
                                   (ut/postwalk-replacer {nil ""}
                                                         (into {}
                                                               (for [k templated-strings-vals]
                                                                 {k @(ut/tracked-sub ::clicked-parameter-key-alpha {:keypath [k]})})))
                                   {})
          out-block-map (if templates? (ut/deep-template-replace templated-strings-walk out-block-map) out-block-map)]
      (if (ut/ne? (vec (ut/get-compound-keys out-block-map))) (logic-and-params-fn out-block-map panel-key) out-block-map))
    block-map))

(re-frame/reg-sub ::list (fn [db] (get-in db [:connections])))

(re-frame/reg-sub ::selected (fn [db] (get-in db [:selected-connection])))

(re-frame/reg-event-db ::select (fn [db [_ connection_id]] (assoc db :selected-connection connection_id)))

(re-frame/reg-sub ::selected-field (fn [db] (get-in db [:selected-field])))

(re-frame/reg-event-db ::select-field (fn [db [_ key_hash]] (assoc db :selected-field key_hash)))

(re-frame/reg-event-db ::set-reco-status
                       (fn [db [_ query-key d]]
                         (let [panel-id (first (for [[k v] (get db :panels) :when (get-in v [:queries query-key])] k))]
                           (-> db
                               (assoc-in [:status :reco query-key] d)))))

(re-frame/reg-sub ::selected-table (fn [db] (get-in db [:selected-table])))

(re-frame/reg-event-db ::select-table (fn [db [_ context_hash]] (assoc db :selected-table context_hash)))

(re-frame/reg-sub ::selected-shape (fn [db] (get-in db [:selected-shape])))

(re-frame/reg-event-db ::select-shape (fn [db [_ context_hash]] (assoc db :selected-shape context_hash)))

(defn refresh
  []
  (ut/tracked-dispatch [::wfx/request :default
                        {:message     {:kind       :honey-call
                                       :ui-keypath [:connections]
                                       :honey-sql  {:select-distinct [:connection_id :database_name :database_version]
                                                    :from            [:connections]}
                                       :extras     {:requested? true :rando (rand-int 1234234525)}}
                         :on-response [::http/socket-response]
                         :on-timeout  [::http/timeout-response :conn/refresh]
                         :timeout     50000}]))

(re-frame/reg-sub ::field-count (fn [db] (get-in db [:field-count])))

(defn field-count
  []
  (ut/tracked-dispatch [::wfx/request :default
                        {:message     {:kind       :honey-call
                                       :ui-keypath [:field-count]
                                       :honey-sql  {:select [[[:count 1] :cnt]]
                                                    :from   [:fields]
                                                    :where  [:and [:= :context_hash @(ut/tracked-subscribe [::selected-table])]
                                                             [:= :connection_id @(ut/tracked-subscribe [::selected])]]}
                                       :extras     {:requested? true :rando (rand-int 1234234525)}}
                         :on-response [::http/socket-response]
                         :on-timeout  [::http/timeout-response :conn/field-count]
                         :timeout     50000}]))

(re-frame/reg-sub ;; RESOLVE COMPOUND KEY
  ::resolve-click-param
  (fn [db {:keys [long-keyword]}]
    (let [get-it (fn [kw]
                   (let [slp (vec (map keyword ;; memoize this in ut?
                                    (-> kw
                                        str
                                        (ut/replacer ":" "")
                                        (ut/splitter #"/")
                                        (vec))))
                         val (get-in db (vec (cons :click-param slp)))]
                     val))]
      (cond (keyword? long-keyword)                          (get-it long-keyword)
            (or (list? long-keyword) (vector? long-keyword)) (into {} (for [k long-keyword] {k (get-it k)}))
            :else                                            nil))))

(re-frame/reg-sub
 ::clicked-parameter
 (fn [db [_ keypath]]
   ;;(ut/tapp>>  [:keypath (str keypath)])
   (if false ;(= (first keypath) :param)
     (let [kk (keyword (cstr/join "/" (map #(cstr/replace (str %) ":" "") keypath)))]
       @(ut/tracked-sub ::clicked-parameter-key-alpha {:keypath [kk]}))
     (let [val (get-in db (cons :click-param keypath))] val))
                    ;; (let [kk (keyword (ut/replacer (cstr/join "/" keypath) ":" ""))] ;; resolved...
                    ;;   (ut/tapp>> [:resolve-click-param kk keypath
                    ;;               (edn/read-string (str ":" (cstr/replace (str (first keypath)) ":" "") "/"
                    ;;                    (cstr/replace (str (last keypath)) ":" "")))])
                    ;;   @(ut/tracked-sub ::clicked-parameter-key {:keypath [kk]}))
   ))

(re-frame/reg-sub
 ::clicked-parameter-alpha
 (fn [db {:keys [keypath]}]
   ;;(ut/tapp>>  [:keypath-alpha (str keypath)])
   (get-in db (cons :click-param keypath))))

(defn contains-namespaced-keyword?
  [data]
  (cond (map? data)     (some contains-namespaced-keyword? (concat (keys data) (vals data)))
        (vector? data)  (some contains-namespaced-keyword? data)
        (keyword? data) (boolean (namespace data))
        :else           false))

(re-frame/reg-sub ;; RESOLVE KEYPATH
  ::clicked-parameter-key ;;; important, common, and likely more expensive than need be. TODO
  (fn [db [_ keypath]]
    (let [cmp              (ut/splitter (ut/safe-name (nth keypath 0)) "/")
          kkey             (keyword (ut/replacer (nth cmp 0) "-parameter" ""))
          vkey             (keyword (peek cmp))
          val0             (get-in db (cons :click-param [kkey vkey]))
          if-walk-map2     (fn [obody]
                             (let [kps       (ut/extract-patterns obody :if 4)
                                   logic-kps (into {}
                                                   (for [v kps]
                                                     (let [kws             (vec (filter #(cstr/includes? (str %) "/")
                                                                                  (ut/deep-flatten v)))
                                                           wm              @(ut/tracked-sub ::resolve-click-param {:long-keyword kws})
                                                           v0              (ut/postwalk-replacer wm v)
                                                           [_ l this that] v0]
                                                       {v (if l this that)})))]
                               (ut/postwalk-replacer logic-kps obody)))
          val0             (try (if (= (first val0) :if) (if-walk-map2 val0) val0) (catch :default _ val0)) ;; TODO
          ns-kw?           (and (cstr/starts-with? (str val0) ":") (cstr/includes? (str val0) "/") (keyword? val0))
          val              (cond ns-kw?      (get-in db
                                                     (let [sp (ut/splitter (str val0) "/")]
                                                       [:click-param (ut/unre-qword (ut/replacer (str (first sp)) ":" ""))
                                                        (ut/unre-qword (last sp))]))
                                 (map? val0) (let [km      (into {}
                                                                 (distinct (map #(when (>= (count %) 2)
                                                                                   {(str (ut/replacer (str (get % 0)) ":" "")
                                                                                         "/"
                                                                                         (ut/replacer (str (get % 1)) ":" ""))
                                                                                      [(get % 0) (get % 1)]})
                                                                             (ut/keypaths (get db :click-param)))))
                                                   rep-map (into {}
                                                                 (for [[k v] km]
                                                                   {(keyword k) (get-in db (cons :click-param v))}))]
                                               (ut/postwalk-replacer rep-map val0))
                                 :else       val0)
          val              (if (and (string? val) (cstr/starts-with? (str val) ":") (not (cstr/includes? (str val) " ")))
                             (edn/read-string val) ;; temp hacky param work around (since we
                             val)
          contains-params? (contains-namespaced-keyword? val)]
      (if contains-params? ;; (and contains-params? (not contains-sql-alias?))
        (logic-and-params-fn val nil) ;; process again, no recursion here for now... we want to
        val))))

;; (re-frame/reg-sub
;;  ::runner-crosswalk-map
;;  (fn [_ _] 
;;    (let [;; runner-keys       (keys (get-in db [:server :settings :runners] {}))
;;         ;;  all-runners       (apply concat
;;         ;;                           (for [r runner-keys]
;;         ;;                             (mapcat (fn [[_ v]] (keys (get v r))) (get db :panels))))
;;          modded-map (into {} (for [[k v] 
;;                                    @db/solver-fn-lookup
;;                                    ;(get-in db [:solver-fn :lookup])
;;                                    :let [base-k (last k)
;;                                          base-k (if (vector? base-k) (last base-k) base-k)]] ;; bit of a hack, since our keys are not fully congruent
;;                                {base-k v}))
;;          modded-map (into {} (for [[k v] modded-map]
;;                                (if (cstr/includes? (str k) "/")
;;                                  {k v}
;;                                  {k v ;; add the server compat alias just in case
;;                                   (keyword (str "data/" (cstr/replace (str k) ":" ""))) v})))
;;          ;;_ (ut/tapp>> [:modded-map modded-map])
;;          ]
;;      modded-map)))

(re-frame/reg-sub
 ::runner-crosswalk-map
 (fn [_ _]
  ; (try 
     (let [lkup-atom (into {}(for [[k v] @db/solver-fn-lookup ;; see comment below.... need to dissect this...
                                   :when (not (integer? (last k)))] {k v}))
           process-key (fn [k]
                       (let [base-k (if (vector? k) (last k) k)]
                         (if (namespace base-k) ;;(try (namespace base-k) (catch :default _ false))
                           [base-k]
                           [base-k (keyword "data" (name base-k))])))]
     (into {}
           (comp 
            (mapcat (fn [[k v]]
                      (map #(vector % v) (process-key (last k)))))
            (distinct))
           lkup-atom))
   ;  (catch :default _ {}))
   ))

;; filtered out for now, but def need to deal with it. TODO - 9/4/24
;;,throws when keypath ends in a vector idx.. Ex:
;; {[:conns [:settings/clover-templates] :clj :body] :solver/raw-custom-override-12435591,
;;  [:conns [:settings/runners] :clojure2 :clover-fn] :solver/raw-custom-override1704129243,
;;  [:conns [:settings/clover-templates] :clj2 :body] :solver/raw-custom-override1244140058,
;;  [:panels :block-10760 :new-clojure-1] :solver/raw-custom-override762115230,
;;  [:conns [:settings/runners] :outliers :clover-fn] :solver/raw-custom-override-1437442864,
;;  [:panels :virtual-panel :virtual-view] :solver/raw-custom-override-1733932054,
;;  [:conns [:settings/runners] :clojure :clover-fn] :solver/raw-custom-override1538021924,
;;  [:conns [:settings/runners] :create-image :clover-fn] :solver/raw-custom-override-551405075,
;;  [:conns [:settings/runners] :clojure-intro :clover-fn] :solver/raw-custom-override276070878,
;;  [:conns [:settings/clover-templates] :color-theft :body 1] :solver/get-my-colors-236804234,
;;  [:conns [:settings/runners] :shell :clover-fn] :solver/raw-custom-override-290714520,
;;  [:conns [:settings/runners] :fabric :clover-fn] :solver/raw-custom-override1526306838}

;;(ut/tapp>> [:modded-map @(ut/tracked-sub ::runner-crosswalk-map {})]) 



(defn break-up-flow-key
  [key]
  (let [ff  (cstr/split (-> (str key)
                            (cstr/replace #":" ""))
                        #"/")
        ff2 (cstr/split (last ff) #">")]
    (vec (for [k ff2] (if (try (number? (edn/read-string k)) (catch :default _ false)) (edn/read-string k) (keyword k))))))

(defn split-back [x] (keyword (ut/replacer (first (cstr/split (str x) #">")) ":" "")))

(re-frame/reg-sub ;; RESOLVE KEYPATH TEMP FOR TESTING
  ::clicked-parameter-key-alpha ;;; important, common, and likely more expensive than need be.
  (fn [db {:keys [keypath]}]
    (let [cmp                 (ut/splitter (ut/safe-name (nth keypath 0)) "/")
          runner-crosswalk-map ;(try 
                                 @(ut/tracked-sub ::runner-crosswalk-map {}) 
                               ;     (catch :default e (do (ut/tapp>> [:runner-crosswalk-map-error e :db/solver-fn-lookup (str @db/solver-fn-lookup)]) {})))
          kkey                (keyword (ut/replacer (nth cmp 0) "-parameter" ""))
          ;vkey                (keyword (peek cmp))
          vkey                (keyword (last cmp))
          ;; kp-encoded-param?   (and (or (= kkey :theme)
          ;;                              (= kkey :param)) (cstr/includes? (str keypath) ">"))
          kp-encoded-param? (and (#{:theme :param} kkey) (cstr/includes? (str keypath) ">"))
          ;; param-has-fn? (when kp-encoded-param? ;;(= kkey :param) ;;kp-encoded-param?
          ;;                 (some #(= % :run-solver) (ut/deep-flatten (get-in db (vec (cons :click-param [kkey
          ;;                 (split-back vkey)]))))))
          ;; param-has-fn?       (when (or (= kkey :param) (= kkey :theme))
          ;;                       (= (get-in db (vec (cons :click-param [kkey (split-back vkey) 0]))) :run-solver))
          param-has-fn?       (when (#{:param :theme} kkey)
                                (= (get-in db [:click-param kkey (split-back vkey) 0]) :run-solver))
          full-kp             (cons :click-param (if kp-encoded-param? (vec (cons kkey (break-up-flow-key vkey))) [kkey vkey]))
          val0                (cond

                                (get runner-crosswalk-map (nth keypath 0))  ;; more sketchy recursion... beware!
                                @(ut/tracked-sub ::clicked-parameter-key-alpha {:keypath [(get runner-crosswalk-map (nth keypath 0))]})

                                (and param-has-fn? kp-encoded-param?)

                                (get-in @(ut/tracked-sub
                                          ::clicked-parameter-key-alpha
                                          {:keypath [(keyword (ut/replacer (first (cstr/split (str (first keypath)) #">"))
                                                                           ":"
                                                                           ""))]})
                                        (vec (rest (break-up-flow-key vkey))))

                                param-has-fn?                         (try (get-in
                                                                            db
                                                                            (vec (cons :click-param
                                                                                       (map keyword
                                                                                            (map #(cstr/replace (str %) ":" "")
                                                                                                 (cstr/split
                                                                                                  (get @db/solver-fn-lookup [:conns keypath])
                                                                                                      ;@(ut/tracked-sub ::solver-fn-lookup {:keypath [:conns keypath]})
                                                                                                  #"/"))))))
                                                                           (catch :default _ nil)) ;; get the sub ref and
                                                                                                       ;; return that data
                                kp-encoded-param?                      (get-in @(ut/tracked-sub
                                                                                 ::clicked-parameter-key-alpha
                                                                                 {:keypath [(keyword (ut/replacer (first (cstr/split (str (first keypath)) #">"))
                                                                                                                  ":"
                                                                                                                  ""))]})
                                                                               (vec (rest (break-up-flow-key vkey))))

                                (let [vv (get-in db full-kp)]  ;; if we get back a clover keyword, try and resolve it. CAREFUL, this could stackoverflow us easy...
                                  (and (keyword? vv) (cstr/includes? (str vv) "/")))

                                @(ut/tracked-sub ::clicked-parameter-key-alpha {:keypath [(get-in db full-kp)]})

                                :else                                 (get-in db full-kp))
          val0                (if (and param-has-fn? (nil? val0)) ;; hasn't run yet - should check atom as well...
                                (get-in db full-kp)
                                val0) ;; we need the solver code below to trigger it if need be.
          ;;_ (when (= (first keypath) :param/huger22) (ut/tapp>> [val0]))
          ;; _ (when kp-encoded-param? (ut/tapp>> [:clicked-parameter-key-alpha-w-ext-kp keypath kp-encoded-param?
          ;; param-has-fn?
          ;;                                       (vec (rest (break-up-flow-key vkey)))
          ;;                                       @(ut/tracked-sub ::clicked-parameter-key-alpha
          ;;                                                        {:keypath [(keyword (ut/replacer (first
          ;;                                                        (cstr/split (str (first keypath))  #">"))  ":"
          ;;                                                        ""))]})
          ;;                                       (keyword (ut/replacer (first (cstr/split (str (first keypath))
          ;;                                       #">"))  ":" ""))
          ;;                                       ;;(keyword? (first (cstr/split (str (first keypath))  #">")))
          ;;                                       ;(keyword (str "param/" (ut/replacer (first (cstr/split (str
          ;;                                       (first keypath))  #">")) ":" "")))
          ;;                                       (vec (cons :click-param [kkey (split-back vkey)])) val0 cmp
          ;;                                       full-kp kkey vkey val0
          ;;                                       ;(keyword (ut/replacer (first (cstr/split (str keypath)  #">"))
          ;;                                       ":" ""))
          ;;                                       ]))
          if-walk-map2        (fn [obody]
                                (let [kps       (ut/extract-patterns obody :if 4)
                                      logic-kps (into {}
                                                      (for [v kps]
                                                        (let [kws             (vec (filter #(cstr/includes? (str %) "/")
                                                                                     (ut/deep-flatten v)))
                                                              wm              @(ut/tracked-sub ::resolve-click-param {:long-keyword kws})
                                                              v0              (ut/postwalk-replacer wm v)
                                                              [_ l this that] v0]
                                                          {v (if l this that)})))]
                                  (ut/postwalk-replacer logic-kps obody)))
          val0                (try (if (= (first val0) :if) (if-walk-map2 val0) val0) (catch :default _ val0)) ;; TODO
          ;ns-kw?              (and (cstr/starts-with? (str val0) ":") (cstr/includes? (str val0) "/") (keyword? val0))
          ns-kw?              (and (keyword? val0) (namespace val0))
          val                 (cond ns-kw?      (get-in db
                                                        (let [sp (ut/splitter (str val0) "/")]
                                                          [:click-param (ut/unre-qword (ut/replacer (str (first sp)) ":" ""))
                                                           (ut/unre-qword (last sp))]))
                                    ;; (map? val0) (let [km      (into {}
                                    ;;                                 (distinct (map #(when (>= (count %) 2)
                                    ;;                                                   {(str (ut/replacer (str (get % 0)) ":" "")
                                    ;;                                                         "/"
                                    ;;                                                         (ut/replacer (str (get % 1)) ":" ""))
                                    ;;                                                      [(get % 0) (get % 1)]})
                                    ;;                                             (ut/keypaths (get db :click-param)))))
                                    ;;                   rep-map (into {}
                                    ;;                                 (for [[k v] km]
                                    ;;                                   {(keyword k) (get-in db (cons :click-param v))}))]
                                    ;;               (ut/postwalk-replacer rep-map val0))
                                    (map? val0)     (let [km (into {}
                                                                   (comp
                                                                    (filter #(>= (count %) 2))
                                                                    (map (fn [[k1 k2 :as path]]
                                                                           [(str (ut/replacer (str k1) ":" "") "/" (ut/replacer (str k2) ":" ""))
                                                                            [k1 k2]]))
                                                                    (distinct))
                                                                   (ut/keypaths (get db :click-param)))
                                                          rep-map (into {}
                                                                        (map (fn [[k v]]
                                                                               [(keyword k) (get-in db (cons :click-param v))]))
                                                                        km)]
                                                      (ut/postwalk-replacer rep-map val0))
                                    :else       val0)
          val                 (if (and (string? val) (cstr/starts-with? (str val) ":") (not (cstr/includes? (str val) " ")))
                                (edn/read-string val) ;; temp hacky param work around (since we
                                val)
          contains-params?    (contains-namespaced-keyword? val)
          contains-solver-fn? (some #(= % :run-solver) (ut/deep-flatten [val (get-in db full-kp)]))] 
      ;;(ut/tapp>> [:kk (str keypath) val])
      (if (or contains-solver-fn?
              contains-params?) ;; (and contains-params? (not contains-sql-alias?))
        (logic-and-params-fn (if contains-solver-fn? (get-in db full-kp) val) ;;val
                             keypath) ;; process again, no recursion here for now... we want to
        val))))


(re-frame/reg-event-db
 ::click-parameter
 (fn [db [_ keypath value]]
   (let [cc (get-in db (cons :click-param keypath))]
     (assoc-in db
               (cons :click-param keypath)
               (if (and (not (= (first keypath) :param)) (= cc value))
                 nil
                 value)))))

(re-frame/reg-event-db 
 ::declick-parameter 
 (undoable) 
 (fn [db [_ keypath]] 
   (ut/tapp>> [:unclick-param (str keypath) (str (vec (cons :click-param keypath)))])
   (ut/dissoc-in db (vec (cons :click-param keypath)))))

(re-frame/reg-event-db
 ::cell-click-parameter
 (fn [db [_ keypath value]]
   (ut/tapp>> [:cell-click-parameter keypath value])
    (let [cc      (get-in db (cons :click-param keypath))
          new     (vec (distinct (cond (vector? cc) (vec (conj cc value))
                                       (nil? cc)    [value]
                                       :else        (vec (conj [cc] value)))))
          new-vec (vec (flatten (cond ;(= cc value) nil ;; drop it?
                                  (try (some #(= % value) cc) (catch :default _ false)) (remove #(= % value) cc)
                                  :else                                                 new)))]
      (ut/tapp>> [:cell-click-parameter-existing-val (cons :click-param keypath) value cc new new-vec (empty? new-vec)])
      (if (empty? new-vec)
        (ut/dissoc-in db (vec (cons :click-param keypath))) ;; empty list, remove param
        (assoc-in db (cons :click-param keypath) new-vec))))) ;; otherwise commit new list

(re-frame/reg-sub ::sql-data (fn [db [_ keypath]] (get-in db (cons :data keypath))))

(re-frame/reg-sub ::sql-data-alpha (fn [db {:keys [keypath]}] (get-in db (cons :data keypath))))

(re-frame/reg-sub ::sql-metadata (fn [db [_ keypath]] (get-in db (cons :meta keypath))))

(re-frame/reg-sub ::sql-metadata-alpha (fn [db {:keys [keypath]}] (get-in db (cons :meta keypath))))

(re-frame/reg-sub ::sql-post-metadata (fn [db [_ keypath]] (get-in db (cons :post-meta keypath))))

(re-frame/reg-sub ::sql-post-metadata-alpha (fn [db {:keys [keypath]}] (get-in db (cons :post-meta keypath))))

(re-frame/reg-sub ::sql-post-styles (fn [db [_ keypath]] (get-in db (cons :post-styles keypath))))

(re-frame/reg-sub ::sql-post-styles-row-lookups
                  (fn [db [_ keypath]]
                    (let [res (get (get-in db (cons :post-styles keypath)) :*)]
                      (into {}
                            (for [[name {:keys [results styles]}] res]
                              {name {:rows   (vec (remove nil?
                                                    (for [i (range (count results))] (when (= (get-in results [i :v]) 1) i))))
                                     :styles styles}})))))

(re-frame/reg-sub ::sql-post-styles-cell-lookups
                  (fn [db [_ keypath]]
                    (let [res (get-in db (cons :post-styles keypath))]
                      (dissoc (into {}
                                    (for [f (keys res)]
                                      {f (into {}
                                               (for [[name {:keys [results styles]}] (get res f)]
                                                 {name {:rows   (vec (remove nil?
                                                                       (for [i (range (count results))]
                                                                         (when (= (get-in results [i :v]) 1) i))))
                                                        :styles styles}}))}))
                        :*))))

(re-frame/reg-sub ::sql-merged-metadata
                  (fn [db [_ keypath]]
                    (let [pm (get-in db (cons :post-meta keypath))
                          m  (get-in db (cons :meta keypath))]
                      {:fields   (into {} (for [[k v] (get m :fields)] {k (merge v (get pm k))}))
                       :rowcount (get-in pm [:* :rowcount])})))

(re-frame/reg-sub 
 ::sql-data-exists? 
 (fn [db [_ keypath]] 
   (not (nil? (get-in db (cons :data keypath))))))

(re-frame/reg-sub
 ::sql-data-exists-alpha?
 (fn [db {:keys [keypath]}]
   (not (nil? (get-in db (cons :data keypath))))))

(re-frame/reg-sub ::sql-data-text (fn [db [_ keypath]] (str (get-in db (cons :data keypath)))))

(re-frame/reg-sub
  ::sql-data-boxes
  (fn [db [_ keypath clickable? style]]
    (let [styles (if (and (map? style) (not (nil? style))) style {})]
      (vec (for [r (get-in db (cons :data keypath))]
             [re-com/box :size "auto" :padding "4px" :attr
              (if clickable? {:on-click #(ut/tracked-dispatch [::click-parameter keypath r])} {}) :child (str r) :style
              (merge styles
                     {:cursor           (if clickable? "pointer" "inherit")
                      :background-color (if (= r @(ut/tracked-subscribe [::clicked-parameter keypath])) "grey" "inherit")})])))))

(re-frame/reg-sub
  ::sql-data-boxes-values
  (fn [db [_ keypath clickable? style]]
    (let [styles (if (and (map? style) (not (nil? style))) style {})]
      (vec (for [r (get-in db (cons :data keypath))]
             [re-com/box :size "auto" :padding "4px" :attr
              (if clickable?
                {;:on-click #(ut/tracked-dispatch [::click-parameter keypath r])
                 :on-click (re-com/handler-fn (ut/tracked-dispatch [::click-parameter keypath r]))}
                {}) :child (str (vals r)) :style
              (merge styles
                     {:cursor           (if clickable? "pointer" "inherit")
                      :background-color (if (= r @(ut/tracked-subscribe [::clicked-parameter keypath])) "grey" "inherit")})])))))

(re-frame/reg-sub
 ::sql-query-not-run?
 (fn [db [_ keypath query]] ;; LOGIC HERE NEEDS TO BE THE SAME AS conn/sql-data ore shit gets
   (let [;query (dissoc query :col-widths)
         query        (ut/clean-sql-from-ui-keys query)
         not-col-sel? (not (= (first keypath) (get-in db [:selected-cols 1])))]
     (and not-col-sel? (not (= (hash query) (get-in db (cons :query-history keypath))))))))

(re-frame/reg-sub
 ::sql-query-not-run-alpha?
 (fn [db {:keys [keypath query]}] ;; LOGIC HERE NEEDS TO BE THE SAME AS conn/sql-data ore shit gets
   (let [query        (ut/clean-sql-from-ui-keys query)
         not-col-sel? (not (= (first keypath) (get-in db [:selected-cols 1])))]
     (and not-col-sel? (not (= (hash query) (get-in db (cons :query-history keypath))))))))

(re-frame/reg-event-db
 ::add-to-sql-history ;; :ran tap is good
 (fn [db [_ keypath query]]
   (let [;query (dissoc query :col-widths)
         query       (ut/clean-sql-from-ui-keys query)
         base-sniff? (true? (= (get query :limit) 111))
         ;;_ (ut/tapp>> [:add-to-sql-history (str keypath) (hash query) (str query)])
         ]
     (if base-sniff?
       (-> db
           (assoc-in (cons :base-sniff-queries keypath) (hash query))
           (assoc-in (cons :query-history keypath) (hash query)))
       (assoc-in db (cons :query-history keypath) (hash query))))))

(re-frame/reg-event-db
 ::add-to-sql-history-meta ;; :ran tap is good
 (fn [db [_ keypath query]]
   (let [;query (dissoc query :col-widths)
         query (ut/clean-sql-from-ui-keys query)]
     (assoc-in db (cons :query-history-meta keypath) (hash query)))))

(re-frame/reg-sub
 ::lookup-panel-key-by-query-key-alpha
 (fn [db {:keys [query-key]}]
   (first (remove nil?
                  (for [[k v] (get db :panels)] (when (some #(= query-key %) (keys (get v :queries))) k))))))

(re-frame/reg-event-db
 ::clear-query-history
 (fn [db [_ query-id]]
   (let [;panel @(ut/tracked-subscribe [::lookup-panel-key-by-query-key query-id])
         panel @(ut/tracked-sub ::lookup-panel-key-by-query-key-alpha {:query-key query-id})]
     (-> db
         (assoc-in [:panels panel :queries query-id :_last-run] (ut/get-time-format-str))
         (ut/dissoc-in [:query-history query-id])
         (ut/dissoc-in [:query-history-meta query-id])))))

(re-frame/reg-sub
 ::sql-meta-not-run?
 (fn [db {:keys [keypath query]}]
   (let [;query (dissoc query :col-widths)
         query        (ut/clean-sql-from-ui-keys query)
         not-col-sel? (not (= (first keypath) (get-in db [:selected-cols 1])))]
     (and not-col-sel? (not (= (hash query) (get-in db (cons :query-history-meta keypath))))))))

(re-frame/reg-sub
 ::sql-style-not-run?
 (fn [db {:keys [keypath query pquery]}]
   (let [;query (dissoc query :col-widths)
         query        (ut/clean-sql-from-ui-keys query)
         not-col-sel? (not (= (first keypath) (get-in db [:selected-cols 1])))]
     (and not-col-sel? (not (= (hash (str pquery query)) (get-in db (cons :query-history-style keypath))))))))

(re-frame/reg-sub
 ::sql-tab-not-run?
 (fn [db [_ keypath query pquery]]
   (let [;query (dissoc query :col-widths)
         query        (ut/clean-sql-from-ui-keys query)
         not-col-sel? (not (= (first keypath) (get-in db [:selected-cols 1])))]
     (and not-col-sel? (not (= (hash (str pquery query)) (get-in db (cons :query-history-tab keypath))))))))

(re-frame/reg-event-db
 ::add-to-sql-history-tab
 (fn [db [_ keypath query pquery]]
   (let [;query (dissoc query :col-widths)
         query (ut/clean-sql-from-ui-keys query)]
     (assoc-in db (cons :query-history-tab keypath) (hash (str pquery query))))))

(re-frame/reg-event-db
 ::add-to-sql-history-style ;; :ran tap is good
 (fn [db [_ keypath query pquery]]
   (ut/tapp>> [:ran-style keypath query])
   (let [;query (dissoc query :col-widths)
         query (ut/clean-sql-from-ui-keys query)]
     (assoc-in db (cons :query-history-style keypath) (hash (str pquery query))))))

(re-frame/reg-sub
 ::sql-condi-not-run?
 (fn [db {:keys [keypath query pquery]}]
   (let [;query (dissoc query :col-widths)
         query        (ut/clean-sql-from-ui-keys query)
         not-col-sel? (not (= (first keypath) (get-in db [:selected-cols 1])))]
     (and not-col-sel? (not (= (hash (str pquery query)) (get-in db (cons :query-history-condi keypath))))))))

(re-frame/reg-event-db
 ::add-to-sql-history-condi
 (fn [db [_ keypath query pquery]]
   (let [;query (dissoc query :col-widths)
         query (ut/clean-sql-from-ui-keys query)]
     (assoc-in db (cons :query-history-condi keypath) (hash (str pquery query))))))

(re-frame/reg-sub 
 ::condi-value 
 (fn [db {:keys [condi-key]}] (true? (= 1 (get-in db [:post-condi condi-key 0 :v])))))

(re-frame/reg-sub 
 ::client-name 
 (fn [db] (get db :client-name)))

(defn theme-pull
  [cmp-key fallback & test-fn]
  (let [;v                   @(ut/tracked-subscribe [::clicked-parameter-key [cmp-key]])
        v                   @(ut/tracked-sub ::clicked-parameter-key-alpha {:keypath [cmp-key]})
        t0                  (ut/splitter (str (ut/safe-name cmp-key)) #"/")
        t1                  (keyword (first t0))
        t2                  (keyword (last t0))
        ;;self-ref-keys       (distinct (filter #(and (keyword? %) (namespace %)) (ut/deep-flatten db/base-theme)))
        ;; new deep-flatten is already a set with only keywords
        ;self-ref-keys       (filter #(namespace %) (ut/deep-flatten db/base-theme))
        self-ref-keys       (into #{} (filter namespace) (ut/deep-flatten db/base-theme))
        ;; self-ref-pairs      (into {}
        ;;                           (for [k    self-ref-keys ;; todo add a reurziver version of
        ;;                                 :let [bk (keyword (ut/replacer (str k) ":theme/" ""))]]
        ;;                             {k (get db/base-theme bk)}))
        self-ref-pairs (reduce (fn [acc k]
                                 (let [bk (keyword (cstr/replace (name k) "theme/" ""))]
                                   (if-let [value (get db/base-theme bk)]
                                     (assoc acc k value)
                                     acc)))
                               {}
                               self-ref-keys)
        resolved-base-theme (ut/postwalk-replacer self-ref-pairs db/base-theme)
        base-theme-keys     (keys resolved-base-theme)
        theme-key?          (true? (and (= t1 :theme) (some #(= % t2) base-theme-keys)))
        fallback0           (if theme-key? (get resolved-base-theme t2) fallback)]
    (if (not (nil? v)) v fallback0)))

(re-frame/reg-sub 
 ::data-colors 
 (fn [_] (theme-pull :theme/data-colors db/data-colors)))

(def data-colors @(ut/tracked-sub ::data-colors {})) ;; kinda weird usage

(re-frame/reg-sub ::sql-source (fn [db {:keys [kkey]}] (get-in db [:sql-source kkey] {})))


(def format-puget-atom (reagent/atom {}))

(re-frame/reg-event-db
 ::save-puget
 (fn [db [_ cache-key res ]]
   (ut/tapp>> [:save-puget res cache-key])
   (swap! format-puget-atom assoc cache-key res)
   db))

(re-frame/reg-sub
 ::puget-data-color-map
 (fn [_] (let [data-colors (theme-pull :theme/data-colors db/data-colors)
               data-colors (assoc data-colors :universal-pop-color (theme-pull :theme/universal-pop-color nil))
               data-colors (walk/keywordize-keys data-colors)
               color-map (assoc (walk/postwalk-replace
                                 {:integer :number
                                  :universal-pop-color :delimiter
                                  :rabbit-code :function-symbol}
                                 (into {} (for [[k v] data-colors]
                                            {k (if (or (= k :universal-pop-color)
                                                       (= k :keyword)
                                                       (= k :nil))
                                              ; [:bold v]
                                                 [v]
                                                 [v])})))
                                :tag [(get data-colors :universal-pop-color)])]
           color-map)))

(defn format-edn-puget [w s & [tsplit]]
  (let [cache-key (hash [w s tsplit :puget])
        data-colors @(ut/tracked-sub ::data-colors {})
       ;; _ (ut/tapp>> [:cc data-colors :dd (theme-pull :theme/data-colors nil)])
        react! [@format-puget-atom]
        w (Math/floor (/ w 9))
        cache (get @format-puget-atom cache-key)]
    (ut/tapp>> [:format-edn-puget w  cache-key])
    (if (not (nil? cache))
      cache
      (do (ut/tracked-dispatch
           [::wfx/request :default
            {:message {:kind :puget-document
                       :text s :data-colors (assoc data-colors :universal-pop-color (theme-pull :theme/universal-pop-color nil)) 
                       :width w :opts-map {:map-coll-separator :line
                                           ;:map-delimiter :line
                                           }}
             :on-response [::save-puget cache-key] :timeout 15000}])
          (str (get @format-puget-atom cache-key))))))

(defn sql-deep-meta
  [keypath honey-sql connection-id & [deeps?]]
  (when (not (cstr/starts-with? (str (first keypath)) ":query-preview")) ;; lets not sniff rando
    (let [fields        (get @(ut/tracked-subscribe [::sql-metadata keypath]) :fields [])
          honey-sql     (if (empty? (ut/clean-sql-from-ui-keys honey-sql))
                          @(ut/tracked-sub ::sql-source {:kkey (first keypath)})
                          honey-sql)
          connection-id (or (get honey-sql :connection-id) (if (nil? connection-id) "cache.db" connection-id))
          deep-meta?    (or deeps? (get honey-sql :deep-meta?))
          honey-sql     (ut/clean-sql-from-ui-keys honey-sql)]
      (when (cstr/includes? (str keypath) ":kick") (ut/tapp>> [:deep-meta? keypath fields honey-sql connection-id]))
      (doseq [[[name f] hsql] (merge {[:rowcount :*] {:select [[[:count 1] :rowcnt]] :from [[honey-sql :subq]]}}
                                     (if deep-meta? ;; get distinct counts and other shiz if deep-meta (sad that
                                       (into {}
                                             (for [field (filter #(not (cstr/starts-with? (str %) ":styler_")) (keys fields)) ;; no
                                                                                                                              ;; styler
                                                                                                                              ;; fields...
                                                  ]
                                               {[:distinct field] {:select [[[:count [:distinct field]] :distinct-values]]
                                                                   :from   [[honey-sql :subq]]}}))
                                       {}))]
        (when ;true
          (and (not (= keypath [:query-preview])) ;; no need to meta on browsing viz recos
               @(ut/tracked-sub ::sql-meta-not-run? {:keypath (conj (conj keypath f) name) :query hsql}))
          (dorun (ut/tracked-dispatch [::wfx/request :default
                                       {:message     {:kind          :honey-xcall
                                                      :ui-keypath    (conj (conj keypath f) name)
                                                      :honey-sql     hsql
                                                      :connection-id connection-id
                                                      :client-name   @(ut/tracked-subscribe [::client-name])}
                                        :on-response [::http/socket-response-post-meta]
                                        :on-timeout  [::http/timeout-response (conj (conj keypath f) name)] ;; requeue?
                                        :timeout     50000}])
                 (ut/tracked-dispatch [::add-to-sql-history-meta (conj (conj keypath f) name) hsql])))))))

(def deep-meta-on-deck (atom nil))

(re-frame/reg-event-db
 ::run-sql-deep-meta-for
 (fn [db [_ panel-key query-key honey-sql]]
   (let [connection-id (get-in db [:panels panel-key :connection-id])]
     (ut/tapp>> [:manual-deep-meta query-key :on connection-id])
     (reset! deep-meta-on-deck query-key)
     (ut/tracked-dispatch [::clear-query-history query-key])
     db)))

(defn sql-style-meta
  [keypath honey-sql connection-id]
  (let [style-rules (get honey-sql :style-rules)]
    (doseq [[[f name] {:keys [style logic]}] style-rules]
      (let [kp   (conj (conj (conj keypath f) name) :styles)
            hsql {:select [[[:case logic 1 :else 0] :v]] :from [(ut/keypath-munger keypath)]}]
        (when (and @(ut/tracked-sub ::sql-style-not-run? {:keypath kp :query hsql :pquery honey-sql})
                   (not @(ut/tracked-subscribe [::sql-query-not-run? keypath honey-sql])))
          (dorun (ut/tracked-dispatch [::wfx/request :default
                                       {:message     {:kind          :honey-xcall
                                                      :ui-keypath    kp
                                                      :honey-sql     hsql
                                                      :connection-id :cache ;connection-id
                                                      :client-name   @(ut/tracked-subscribe [::client-name])}
                                        :on-response [::http/socket-response-post-style style]
                                        :on-timeout  [::http/timeout-response [keypath honey-sql]]
                                        :timeout     50000}])
                 (ut/tracked-dispatch [::add-to-sql-history-style kp hsql honey-sql])))))))

(defn sql-tab-meta
  [keypath rules panel-key]
  (let [;style-rules (get honey-sql :style-rules)
        kk (str rules panel-key)]
    (doseq [[[f name] logic] [rules]]
      (let [kp ;(conj
              (conj (conj keypath f) name)
            hsql {:select [[[:case logic 1 :else 0] :v]]}]
        (when ;true
          @(ut/tracked-subscribe [::sql-tab-not-run? kp hsql kk])
          (dorun (ut/tracked-dispatch [::wfx/request :default
                                       {:message     {:kind          :honey-xcall
                                                      :ui-keypath    kp
                                                      :honey-sql     hsql
                                                      :connection-id :cache ;connection-id
                                                      :client-name   @(ut/tracked-subscribe [::client-name])}
                                        :on-response [::http/socket-response-post-tab panel-key]
                                        :on-timeout  [::http/timeout-response [keypath rules]]
                                        :timeout     50000}])
                 (ut/tracked-dispatch [::add-to-sql-history-tab kp hsql kk])))))))

(defn sql-condi-meta
  [keypath rules]
  (let [;style-rules (get honey-sql :style-rules)
        kk (str rules)]
    (doseq [logic [rules]]
      (let [kp ;(conj
              keypath ;(conj keypath name)
            hsql {:select [[[:case logic 1 :else 0] :v]]}]
        (when ;true
          @(ut/tracked-sub ::sql-condi-not-run? {:keypath kp :query hsql :pquery kk})
          (dorun (ut/tracked-dispatch [::wfx/request :default
                                       {:message     {:kind          :honey-xcall
                                                      :ui-keypath    kp
                                                      :honey-sql     hsql
                                                      :connection-id :cache ;connection-id
                                                      :client-name   @(ut/tracked-subscribe [::client-name])}
                                        :on-response [::http/socket-response-post-condi]
                                        :on-timeout  [::http/timeout-response [keypath rules]]
                                        :timeout     50000}])
                 (ut/tracked-dispatch [::add-to-sql-history-condi kp hsql kk])))))))

;; (defn push-panels-to-server
;;   [panels-map resolved-panels-map client-name]
;;   (let []
;;     (dorun (ut/tracked-dispatch
;;              [::wfx/push :default
;;               {:kind :current-panels 
;;                :panels panels-map 
;;                :resolved-panels resolved-panels-map 
;;                :client-name client-name}]))))

(re-frame/reg-event-db ::set-query-schedule
                       (fn [db [_ query-id schedule]]
                         (let [timer (+ (get-in db [:re-pollsive.core/polling :counter]) schedule)]
                           (assoc-in db [:sched query-id] timer))))

(re-frame/reg-sub ;;; dupe from bricks!
  ::lookup-panel-key-by-query-key
  (fn [db [_ query-key]]
    (first (remove nil? (for [[k v] (get db :panels)] (when (some #(= query-key %) (keys (get v :queries))) k))))))

(re-frame/reg-sub ;;; dupe from bricks!
  ::client-name
  (fn [db _] (get db :client-name)))

(defn sql-data
  ([keypath honey-sql]
   (let [style-rules   (get honey-sql :style-rules)
         orig-honey-sql honey-sql
         deep-meta?    (true? (= @deep-meta-on-deck (first keypath)))
        ;;  _             (when deep-meta?
        ;;                  (ut/tapp>> [:deep-meta! (str keypath)])
        ;;                  (swap! db/running-deep-meta-on conj (first keypath)))
         sniff?        (= (get @db/sniff-deck (first keypath)) :reco)
         kit-name      (when (and (not sniff?) (keyword? (get @db/sniff-deck (first keypath))))
                         (get @db/sniff-deck (first keypath)))
         has-rules?    (and (not (nil? style-rules)) (ut/ne? style-rules))
         rules         (when has-rules?
                         (vec (for [[[col name] logic] style-rules]
                                [[:case (:logic logic) 1 :else 0] (keyword (str "styler_" (ut/safe-name name)))])))
         panel-key     @(ut/tracked-sub ::lookup-panel-key-by-query-key-alpha {:query-key (first keypath)})
         clover-sql    (assoc honey-sql :connection-id "system-db")
         connection-id (get honey-sql :connection-id "system-db")
         honey-sql     (ut/clean-sql-from-ui-keys honey-sql)
         hselect       (get honey-sql :select)
         flat          (ut/deep-flatten honey-sql)
         client-name   @(ut/tracked-sub ::client-name {})
         honey-sql     (ut/postwalk-replacer {:*client-name client-name
                                              :*client-name* client-name
                                              :*client-name-str (str client-name)} orig-honey-sql)

         ;;;;honey-sql     (assoc honey-sql :connection-id connection-id)
         literal-data? (and (some #(= % :data) flat) (not (some #(= % :panel_history) flat)))
         honey-modded  (if has-rules? (assoc honey-sql :select (apply merge hselect rules)) honey-sql)
         honey-modded  (ut/clean-sql-from-ui-keys honey-modded) ;; only needed here, below it is handled server-side
         ;client-name   @(ut/tracked-subscribe [::client-name])
         ;honey-modded  (ut/postwalk-replacer {:*client-name client-name
         ;                                     :*client-name* client-name
         ;                                     :*client-name-str (pr-str client-name)} honey-modded)
         ;_ (ut/tapp>> [:sql-run1 (str keypath) (str honey-sql)])
         ]
     (swap! db/kit-run-ids assoc (first keypath) (ut/generate-uuid))
     (ut/tracked-dispatch
      [::wfx/request :default
       {:message     {:kind          :honey-xcall ;; (if (or connection-id literal-data?) :honey-xcall :honey-call) 
                      :ui-keypath    keypath
                      :panel-key     panel-key
                      :deep-meta?    deep-meta?
                      :kit-name      kit-name
                      :clover-sql    clover-sql
                      :honey-sql     honey-modded
                      :connection-id connection-id
                      :client-cache? (if literal-data? (get honey-sql :cache? true) false)
                      :page          (get honey-sql :page)
                      :sniff?        sniff? ;; on used for ext tables
                      :client-name   client-name}
        :on-response [::http/socket-response]
        :on-timeout  [::http/timeout-response [keypath honey-sql]]
        :timeout     50000}])

     (when deep-meta?
       ;(reset! db/running-deep-meta-on (vec (remove #(= % (first keypath)) @db/running-deep-meta-on)))
       (reset! deep-meta-on-deck nil)))

   (when (not (nil? (get honey-sql :refresh-every)))
     (ut/tracked-dispatch [::set-query-schedule (first keypath) (get honey-sql :refresh-every)]))

   (ut/tracked-dispatch [::add-to-sql-history keypath honey-sql])
   ;(swap! db/running-queries disj honey-sql)
   )
   
  ([keypath honey-sql connection-id]
   (doall
     (let [style-rules   (get honey-sql :style-rules)
           orig-honey-sql honey-sql
           deep-meta?    (true? (= @deep-meta-on-deck (first keypath)))
          ;;  _             (when deep-meta?
          ;;                  (ut/tapp>> [:deep-metax! (str keypath)])
          ;;                  (swap! db/running-deep-meta-on conj (first keypath)))
           sniff?        (= (get @db/sniff-deck (first keypath)) :reco)
           kit-name      (when (and (not sniff?) (keyword? (get @db/sniff-deck (first keypath))))
                           (get @db/sniff-deck (first keypath)))
           has-rules?    (and (not (nil? style-rules)) (ut/ne? style-rules))
           rules         (when has-rules?
                           (vec (for [[[col name] logic] style-rules]
                                  [[:case (:logic logic) 1 :else 0] (keyword (str "styler_" (ut/safe-name name)))])))
           client-name   @(ut/tracked-sub ::client-name {})
           honey-sql  (ut/postwalk-replacer {:*client-name client-name
                                             :*client-name* client-name
                                             :*client-name-str (str client-name)} honey-sql)
           connection-id (cond (get honey-sql :connection-id) (get honey-sql :connection-id)
                               (nil? connection-id)           "cache.db"
                               (= connection-id "system")     "system-db"
                               :else                          connection-id)
           clover-sql    (assoc honey-sql :connection-id connection-id)
           refresh-every (get honey-sql :refresh-every)
           cache?        (get honey-sql :cache? false)
           page          (get honey-sql :page)
           panel-key     @(ut/tracked-sub ::lookup-panel-key-by-query-key-alpha {:query-key (first keypath)})
           honey-sql     (ut/clean-sql-from-ui-keys honey-sql)
           hselect       (get honey-sql :select)
           honey-modded  (if has-rules? (assoc honey-sql :select (apply merge hselect rules)) honey-sql)
           ;client-name   @(ut/tracked-subscribe [::client-name])
           ;honey-modded  (ut/postwalk-replacer {:*client-name client-name 
           ;                                     :*client-name* client-name 
           ;                                     :*client-name-str (str client-name)} honey-modded)
            ;_ (ut/tapp>> [:sql-run2 (str keypath) (str honey-modded) (str honey-sql) (hash honey-sql)])
           ]
       (do (when (not (nil? refresh-every))
             (ut/tracked-dispatch [::set-query-schedule (first keypath) refresh-every]))
           (swap! db/kit-run-ids assoc (first keypath) (ut/generate-uuid))
           (ut/tracked-dispatch [::wfx/request :default
                                 {:message     {:kind          :honey-xcall
                                                :ui-keypath    keypath
                                                :panel-key     panel-key
                                                :kit-name      kit-name
                                                :clover-sql    clover-sql
                                                :deep-meta?    deep-meta?
                                                :page          page
                                                :client-cache? cache?
                                                :sniff?        sniff?
                                                :honey-sql     honey-modded
                                                :connection-id connection-id
                                                :client-name   client-name}
                                  :on-response [::http/socket-response]
                                  :on-timeout  [::http/timeout-response [keypath honey-sql]]
                                  :timeout     50000}])
           (when deep-meta?
             ;(reset! db/running-deep-meta-on (vec (remove #(= % (first keypath)) @db/running-deep-meta-on)))
             (reset! deep-meta-on-deck nil))

           (when (or kit-name ;; clear on deck atom
                     sniff?)
             (swap! db/sniff-deck dissoc (first keypath)))

           (ut/tracked-dispatch [::add-to-sql-history keypath orig-honey-sql])
           ;(swap! db/running-queries disj honey-sql)
           )))))
















