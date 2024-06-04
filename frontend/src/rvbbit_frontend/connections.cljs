(ns rvbbit-frontend.connections
  (:require
   [re-frame.core :as re-frame]
   [re-frame.alpha :as rfa]
   [rvbbit-frontend.http :as http]
   [rvbbit-frontend.db :as db]
   [rvbbit-frontend.utility :as ut]
   [clojure.walk :as walk]
   [clojure.edn :as edn]
   [re-com.core :as re-com :refer [at]]
   [re-com.util :refer [px]]
   [day8.re-frame.undo :as undo :refer [undoable]]
   ;[rvbbit-frontend.resolver :as resolver]
   [clojure.string :as cstr]
   [clojure.set :as cset]
   [cljs-time.core] ;; womp[ womp]
   [websocket-fx.core :as wfx]))

(re-frame/reg-sub
 ::sub-flow-incoming
 (fn [db [_]]
   (get db :sub-flow-incoming)))

(defn gn [x] (try (name x) (catch :default _ x)))
(defn gns [x] (try (namespace x) (catch :default _ x)))
(defn gns? [x] (not (nil? (try (namespace x) (catch :default _ false)))))

(defn spawn-open-input-block [starting-val] ;;; need to simplify and merge logic from flow-droppable (flows, params, etc)
  (let [starting-val (try (edn/read-string starting-val) (catch :default _ starting-val))
        dtype (keyword (ut/data-typer starting-val))
        try-read (fn [x] (try (edn/read-string x) (catch :default _ x)))
        [x y] @db/flow-detached-coords
        flow?  (= (get starting-val :category) ":flow") ;; also packaged flows... not raws... how to?
        sub-flow?  (= (get starting-val :category) ":sub-flows")
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
        ;;_ (ut/tapp>> [:full-lookup-map part-key? flow? starting-val lookup-map])
        raw-fn?         (and (= dtype :list)
                             (= (first starting-val) 'fn))

        _ (when flow? (ut/tracked-dispatch [::http/load-sub-flow (get starting-val :file_path)]))
        sub-flow (when flow? @(ut/tracked-subscribe [::sub-flow-incoming]))
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
                         ;;(ut/tapp>> [:sub-flow-build no-inputs base-conns])
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
        other-fn (let [f2i (ut/function-to-inputs lookup-map)
                       arg-walks (into {}
                                       (for [e (keys f2i)
                                             :when (cstr/ends-with? (str e) "+")]
                                         {(-> (str e)
                                              (ut/replacer "+" "")
                                              (ut/replacer ":" "")
                                              keyword) e}))]
                   (merge
                    (when sub-flow? {:flow-path (get lookup-map :flow-path)  ;; for unpacking later
                                   :sub-flow-id (get lookup-map :flow-id)})
                    {:w 125 :h 60 :z 0
                     :data
                     {:flow-item
                      {:category (get lookup-map :category) ;; important for lookup server-side! (string fine)
                       :type (if (not sub-flow?) (try-read (get lookup-map :name)) (get lookup-map :name))  ;; important for client render (expects keyword)
                       :name (get lookup-map :name)  ;; important for lookup server-side! (string fine)
                       :icon (get lookup-map :icon)
                       :inputs (ut/postwalk-replacer arg-walks (get lookup-map :inputs))
                       :defaults (get lookup-map :defaults)
                       :types (ut/postwalk-replacer arg-walks (get lookup-map :types))
                       :style (get lookup-map :style)
                       :selected-style (get lookup-map :selected-style)
                       :expandable? true
                       :required (get lookup-map :required)}
                      :drag-meta {:type (if (not sub-flow?) (try-read (get lookup-map :name)) (get lookup-map :name))}}
                     :right-click? true ;; important for add-block to get current coords
                     :ports {:in
                          ;; (try
                          ;;       (into {} (for [e (second (get lookup-map :fn))
                          ;;                      :let [kk (keyword (str e))]]
                          ;;                  {kk (get-in lookup-map [:types kk] :any)}))
                          ;;       (catch :default _ {:value :any}))

                             (if sub-flow?
                               (select-keys (get lookup-map :types) (get lookup-map :inputs))
                               f2i)

                             :out {:out (get-in lookup-map [:types :out] :any)}}
                     :icon (get lookup-map :icon)}))
        open-fn-body (let [;port-map (try
                           ;           (into {} (for [e (second starting-val)]
                           ;                      {(keyword (str e)) :any}))
                           ;           (catch :default _ {:value :any}))
                           port-map (try (ut/function-to-inputs (second starting-val)) (catch :default _ {:value :any}))
                           ;; (fn [aa bb & ff] (+ aa bb))
                          ;;  arg-walks (into {}
                          ;;                  (for [e (keys port-map)
                          ;;                        :when (cstr/ends-with? (str e) "+")]
                          ;;                    {(-> (str e)
                          ;;                         (ut/replacer "+" "")
                          ;;                         (ut/replacer ":" "")
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
        grid-size db/snap-to-grid
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
    ;(ut/tapp>> [:spawner @(ut/tracked-subscribe [::flow-parts-lookup :clojure-base/*])])
    ;(add-block snapped-x snapped-y block-body bname)
;;    (ut/tapp>> [:spawner snapped-x snapped-y block-body bname])
    [snapped-x snapped-y block-body bname]
    ))

(re-frame/reg-event-db
 ::select-block ;; flow block
 (undoable)
 (fn [db [_ bid]]
   (assoc db :selected-flow-block bid)))

(re-frame/reg-event-db
 ::update-flowmap-key2 ;;; just used for add block
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
                (into
                 (keys (get-in db [:flows (get db :selected-flow) :map]))
                 (vec (distinct (ut/get-all-values (get-in db [:flows (get db :selected-flow)]) :type)))))))))

(defn add-flow-block [x y & [body bid no-select?]]
  (let [;bid (if bid bid (keyword (str "open-input-" (count @(ut/tracked-subscribe [::flowmap])))))
        _ (ut/tapp>> [:add-block bid x y  body])
        bid (keyword (ut/replacer (str (gn bid)) #"/" "-"))
        bid (if bid bid :open-input)
        ;;_ (ut/tapp>> [:ADD-pre-safe-bid bid])
        safe-keys-reserved @(ut/tracked-subscribe [::reserved-type-keywords])
        bid (ut/safe-key bid safe-keys-reserved)
        ;;_ (ut/tapp>> [:ADD-post-safe-bid bid])
        zoom-multi (get @db/pan-zoom-offsets 2)
        zoom-offset-x (get @db/pan-zoom-offsets 0)
        zoom-offset-y (get @db/pan-zoom-offsets 1)
        drop-x      (- (/ x zoom-multi) (/ zoom-offset-x zoom-multi))
        drop-y      (- (/ y zoom-multi) (/ zoom-offset-y zoom-multi))
        grid-size db/snap-to-grid
        drop-x       (* grid-size (Math/round (/ drop-x grid-size)))
        drop-y       (* grid-size (Math/round (/ drop-y grid-size)))
        body (if body (if (or (cstr/includes? (str bid) "open-input")
                              (cstr/includes? (str bid) "open-fn")
                              (get body :right-click? false))
                        (merge body {:x drop-x :y drop-y :z 0}) body) ;; override for right click open block
                 {:w 200 :h 50
                  :x drop-x :y drop-y :z 0
                  :ports {:in {:in :string}
                          :out {:out :string}}})
        body (if (get-in body [:data :sub-flow-id])
               (-> body
                   (assoc :sub-flow-id (get-in body [:data :sub-flow-id]))
                   (assoc :flow-path (get-in body [:data :flow-path])))
               body)
        ;; body (ut/dissoc-in-many body [[:data :sub-flow-id]
        ;;                               [:data :flow-path]])
        body (-> body 
                 (assoc :data (select-keys (get body :data) [:flow-item :drag-meta]))
                 (assoc-in [:data :flow-item :inputs] (vec (keys (get-in body [:ports :in]))))) ;; clean up in case was row-dragged
        ]
   ;(ut/tapp>> [:adding-flow-block bid x y @(ut/tracked-subscribe [::flowmap])])
   ;(swap! flowmaps assoc bid body)
    (when (not no-select?) (ut/tracked-dispatch [::select-block bid]))
    (ut/tracked-dispatch [::update-flowmap-key2 bid nil body])))


(defn logic-and-params-fn [block-map panel-key]
  (if (try
        (and (ut/ne? block-map)
             (or (map? block-map)
                 (vector? block-map)
                 (keyword? block-map)))
        (catch :default _ false))
    ;; try to save some work
    (let [;;valid-body-params      (vec (filter #(and (keyword? %) (cstr/includes? (str %) "/")) (ut/deep-flatten block-map)))
          valid-body-params      (vec (ut/get-compound-keys block-map))
          workspace-params       (into {} (for [k valid-body-params] ;; deref here?
                                            {k
                                             ;;@(ut/tracked-subscribe [::clicked-parameter-key [k]])
                                             @(rfa/sub ::clicked-parameter-key-alpha {:keypath [k]})}))
          value-walks-targets    (filter #(and (cstr/includes? (str %) ".") (not (cstr/includes? (str %) ".*"))) valid-body-params)
          value-walks            (into {} (for [k value-walks-targets] ;all-sql-call-keys]
                                            (let [fs    (ut/splitter (ut/safe-name k) "/")
                                                  gs    (ut/splitter (last fs) ".")
                                                  ds    (keyword (first fs))
                                                  row   (try (int (last gs)) (catch :default _ "label"))
                                                ;row (int (last gs))
                                                  field (keyword (first gs))]
                                              {k (if (not (integer? row))
                                                 ;(get-in @(ut/tracked-subscribe [::conn/sql-data [ds]]) [row field])
                                                   (str field)
                                                   (get-in
                                                    ;;@(ut/tracked-subscribe [::sql-data [ds]])
                                                    @(rfa/sub ::sql-data-alpha {:keypath [ds]})
                                                    [row field]))})))

          condi-walks-targets    (distinct (filter #(cstr/includes? (str %) "condi/") valid-body-params))
          condi-walks            (into {} (for [k condi-walks-targets]
                                            {k @(rfa/sub ::condi-value {:condi-key (keyword (last (ut/splitter (ut/safe-name k) "/")))})}))
          into-walk-map2           (fn [obody] (let [;obody (ut/postwalk-replacer condi-walks orig-body)
                                                     kps       (ut/extract-patterns obody :into 3) ;(kv-map-fn obody) ;(into {} (for [p (ut/kvpaths obody)] {p (get-in obody p)}))
                                                     logic-kps (into {} (for [v kps]
                                                                          (let [[_ this that] v]
                                                                                ;(ut/tapp>> [:if-walk panel-key kps l this that])
                                                                            {v (into this that)})))]
                                                 (ut/postwalk-replacer logic-kps obody)))
          if-walk-map2           (fn [obody] (let [;obody (ut/postwalk-replacer condi-walks orig-body)
                                                   kps       (ut/extract-patterns obody :if 4) ;(kv-map-fn obody) ;(into {} (for [p (ut/kvpaths obody)] {p (get-in obody p)}))
                                                   logic-kps (into {} (for [v kps]
                                                                        (let [[_ l this that] v]
                                                                        ;(ut/tapp>> [:if-walk panel-key kps l this that])
                                                                          {v (if 
                                                                              (logic-and-params-fn l nil) ;; l
                                                                               this that)})))]
                                               (ut/postwalk-replacer logic-kps obody)))
        ;; when-walk-map          (fn [obody] (let [kps       (kv-map-fn obody) ;(into {} (for [p (ut/kvpaths obody)] {p (get-in obody p)}))
        ;;                                          logic-kps (into {} (for [[_ v]
        ;;                                                                   (into {} (filter #(cstr/starts-with? (str (last %)) "[:when") kps))]
        ;;                                                               (let [[_ l this] v]
        ;;                                                                 {v (when l this)})))]
        ;;                                      (ut/postwalk-replacer logic-kps obody)))
          when-walk-map2         (fn [obody] (let [kps       (ut/extract-patterns obody :when 3)
                                                   logic-kps (into {} (for [v kps]
                                                                        (let [[_ l this] v]
                                                                          {v (when l this)})))]
                                               (ut/postwalk-replacer logic-kps obody)))
        ;; =-walk-map             (fn [obody] (let [kps       (kv-map-fn obody) ;(into {} (for [p (ut/kvpaths obody)] {p (get-in obody p)}))
        ;;                                          logic-kps (into {} (for [[_ v]
        ;;                                                                   (into {} (filter #(cstr/starts-with? (str (last %)) "[:=") kps))]
        ;;                                                               (let [[_ that this] v]
        ;;                                                                 ;(ut/tapp>> [:=-walk panel-key kps this that])
        ;;                                                                 {v (= (str that) (str this))})))]
        ;;                                      ;(ut/tapp>> [:=-walk/logic-kps logic-kps kps workspace-params])
        ;;                                      (ut/postwalk-replacer logic-kps obody)))
          =-walk-map2            (fn [obody] (let [kps       (ut/extract-patterns obody := 3)
                                                   logic-kps (into {} (for [v kps]
                                                                        (let [[_ that this] v]
                                                                        ;(ut/tapp>> [:=-walk panel-key kps this that])
                                                                          {v (= (str that) (str this))})))]
                                             ;(ut/tapp>> [:=-walk/logic-kps logic-kps kps workspace-params])
                                               (ut/postwalk-replacer logic-kps obody)))
        ;; auto-size-walk-map2            (fn [obody] (let [kps       (ut/extract-patterns obody :auto-size-px 2)
        ;;                                                  logic-kps (into {} (for [v kps]
        ;;                                                                       (let [[_ l] v]
        ;;                                                                 ;(ut/tapp>> [:=-walk panel-key kps this that])
        ;;                                                                         {v (ut/auto-font-size-px l h w) ;(= (str that) (str this))
        ;;                                                                          })))]
        ;;                                      ;(ut/tapp>> [:=-walk/logic-kps logic-kps kps workspace-params])
        ;;                                              (ut/postwalk-replacer logic-kps obody)))
        ;; onclick-walk-map       (fn [obody] (let [kps       (kv-map-fn obody) ;(into {} (for [p (ut/kvpaths obody)] {p (get-in obody p)}))
        ;;                                          logic-kps (into {} (for [[_ v]
        ;;                                                                   (into {} (filter #(cstr/starts-with? (str (last %)) "[:set-parameter") kps))]
        ;;                                                               (let [[_ pkey pval] v
        ;;                                                                     raw-param-key (get-in vsql-calls (conj (vec (first (filter #(= (last %) :on-click)
        ;;                                                                                                                                (ut/kvpaths vsql-calls)))) 1) pkey)]
        ;;                                                                 ;(ut/tapp>> [:on-click-hack panel-key v raw-param-key])
        ;;                                                                 {v (fn [] (ut/tracked-dispatch [::conn/click-parameter [panel-key] {raw-param-key pval}]))})))]
        ;;                                      ;(ut/tapp>> [:set-param/logic-kps logic-kps kps])
        ;;                                      (ut/postwalk-replacer logic-kps obody)))
        ;; onclick-walk-map2      (fn [obody] (let [kps       (ut/extract-patterns obody :set-parameter 3)
        ;;                                          logic-kps (into {} (for [v kps]
        ;;                                                               (let [[_ pkey pval] v
        ;;                                                                     raw-param-key (get-in vsql-calls (conj (vec (first (filter #(= (last %) :on-click)
        ;;                                                                                                                                (ut/kvpaths vsql-calls)))) 1) pkey)]
        ;;                                                                 ;(ut/tapp>> [:on-click-hack panel-key v raw-param-key])
        ;;                                                                 {v (fn [] (ut/tracked-dispatch [::conn/click-parameter [panel-key] {raw-param-key pval}]))})))]
        ;;                                      ;(ut/tapp>> [:set-param/logic-kps logic-kps kps])
        ;;                                      (ut/postwalk-replacer logic-kps obody)))
        ;; map-walk-map2            (fn [obody] (let [kps       (ut/extract-patterns obody :map 3)
        ;;                                            logic-kps (into {} (for [v kps]
        ;;                                                                 (let [[_ that this] v]
        ;;                                                                 ;(ut/tapp>> [:=-walk panel-key kps this that])
        ;;                                                                   {v (if (vector? this)
        ;;                                                                        (vec (for [r this] (last (get r that))))
        ;;                                                                        (vec (for [r @(ut/tracked-subscribe [::conn/sql-data [this]])] (last (get r that)))))
        ;;                                                                  ;(= (str that) (str this))
        ;;                                                                    })))]
        ;;                                      ;(ut/tapp>> [:=-walk/logic-kps logic-kps kps workspace-params])
        ;;                                        (ut/postwalk-replacer logic-kps obody)))

          string-walk            (fn [num obody] (let [kps       (ut/extract-patterns obody :string3 num)
                                                       logic-kps (into {} (for [v kps]
                                                                            (let [[_ & this] v]
                                                                              {v (apply str this)})))]
                                                   (ut/postwalk-replacer logic-kps obody)))

          case-walk         (fn [obody] (let [kps       (ut/extract-patterns obody :case 2)
                                              logic-kps (into {} (for [v kps]
                                                                   (let [[_ l] v]
                                                                     {v (ut/vectorized-case l)})))]
                                          (ut/postwalk-replacer logic-kps obody)))

        ;; scrubber-walk-map2            (fn [obody] (let [kps       (ut/extract-patterns obody :scrubber 2)
        ;;                                                 kps2       (ut/extract-patterns obody :scrubber 3)
        ;;                                                 logic-kps (into {} (for [v kps]
        ;;                                                                      (let [[_ this] v]
        ;;                                                                        (ut/tapp>> [:scrubber panel-key kps this])
        ;;                                                                        {v [scrubber-panel true
        ;;                                                                            @(ut/tracked-subscribe [::keypaths-in-params :param])
        ;;                                                                            :param
        ;;                                                                            (str (last (ut/splitter (str this) #"/")))
        ;;                                                                            {:fm true :canvas? true}]})))
        ;;                                                 logic-kps2 (into {} (for [v kps2]
        ;;                                                                       (let [[_ this opts] v]
        ;;                                                                         (ut/tapp>> [:scrubber panel-key kps this])
        ;;                                                                         {v [scrubber-panel true
        ;;                                                                             @(ut/tracked-subscribe [::keypaths-in-params :param])
        ;;                                                                             :param
        ;;                                                                             (str (last (ut/splitter (str this) #"/")))
        ;;                                                                             {:fm true :canvas? true :opts opts}]})))]
        ;;                                             (ut/postwalk-replacer (merge logic-kps2 logic-kps) obody)))
          singles               {:text                  str
                                                                 ;:case (fn [x] (ut/vectorized-case x))
                                 :>> (fn [[x y]] (true? (> x y)))
                                 :<< (fn [[x y]] (true? (< x y)))
                                 :str (fn [args]
                                        (if (vector? args)
                                          (cstr/join "" (apply str args))
                                          (str args)))
                                 :string (fn [args]
                                           (if (vector? args)
                                             (cstr/join "" (apply str args))
                                             (str args)))}
                               ;:string str
                              ; :strings               (fn [_ x] (apply str x))
                              ; :string                (fn [x] (try
                              ;                                  (let [x (for [e x] (ut/safe-name e))] (apply str x))
                              ;                                  (catch :default _ (str x))))
                               ;:number                (fn [x] (str (nf x)))
                               ;:percent               (fn [x] (str (nf x) "%"))
          obody-key-set (ut/body-set block-map)
          has-fn? (fn [k] (some #(= % k) obody-key-set))

          out-block-map (cond->> block-map
                          true (ut/namespaced-swapper "this-block" (ut/replacer (str panel-key) #":" ""))
                          true (ut/postwalk-replacer {:*this-block* panel-key})
                          (ut/ne? value-walks) (ut/postwalk-replacer value-walks)
                          (ut/ne? condi-walks) (ut/postwalk-replacer condi-walks)
                          (ut/ne? workspace-params) (ut/postwalk-replacer workspace-params)

                          (has-fn? :string3) (string-walk 2) ;; TODO, remove all these extra string replacements and make a [:string & x] ver
                          (has-fn? :string3) (string-walk 3)
                          (has-fn? :string3) (string-walk 4)
                          (has-fn? :string3) (string-walk 5)
                          (has-fn? :string3) (string-walk 6) ;; TODO REMOVE ALL THIS FUCKERY - we already have a better way w &[args] mapping!

                          (has-fn? :=) =-walk-map2
                          (has-fn? :if) if-walk-map2
                          (has-fn? :when) when-walk-map2
                          (has-fn? :into) into-walk-map2
                          (ut/ne? singles) (ut/postwalk-replacer singles)
                          (has-fn? :case) case-walk)

          templated-strings-vals (vec (filter #(cstr/includes? (str %) "/")
                                              (ut/deep-template-find out-block-map))) ;; ignore non compounds, let the server deal with it
          templates?              (ut/ne? templated-strings-vals)
          _ (when templates? (ut/tapp>> [:replacing-string-templates... templated-strings-vals out-block-map]))
          templated-strings-walk (if templates?
                                   (ut/postwalk-replacer {nil ""}
                                                          (into {} (for [k templated-strings-vals]
                                                                     {k
                                                                      ;;@(ut/tracked-subscribe [::clicked-parameter-key [k]])
                                                                      @(rfa/sub ::clicked-parameter-key-alpha {:keypath [k]})}))) {})
          out-block-map (if templates? (ut/deep-template-replace templated-strings-walk out-block-map) out-block-map)]

      ;;(ut/tapp>> [:pp panel-key (ut/deep-template-find out-block-map)])
      ;;(ut/tapp>> [:resolver-val-walks panel-key valid-body-params workspace-params valid-body-params out-block-map])
      (if (ut/ne? (vec 
                   ;;(filter #(and (keyword? %) (cstr/includes? (str %) "/")) (ut/deep-flatten out-block-map))
                   (ut/get-compound-keys out-block-map)
                   ))
        (logic-and-params-fn out-block-map panel-key)
        out-block-map))
    block-map))

(re-frame/reg-sub
 ::list
 (fn [db]
   (get-in db [:connections])))

(re-frame/reg-sub
 ::selected
 (fn [db]
   (get-in db [:selected-connection])))

(re-frame/reg-event-db
 ::select
 (fn [db [_ connection_id]]
   ;(ut/tapp>> [db-key result])
   (assoc db :selected-connection connection_id)))

(re-frame/reg-sub
 ::selected-field
 (fn [db]
   (get-in db [:selected-field])))

(re-frame/reg-event-db
 ::select-field
 (fn [db [_ key_hash]]
   ;(ut/tapp>> [db-key result])
   (assoc db :selected-field key_hash)))

(re-frame/reg-event-db
 ::set-reco-status
 (fn [db [_ query-key d]]
   ;(ut/tapp>> [db-key result])
   (let [panel-id (first (for [[k v] (get db :panels)
                               :when (get-in v [:queries query-key])] k))]
     (-> db
         ;(assoc-in [:panels panel-id :recos-done?] true)
         ;(assoc-in [:reeco-status query-key] d)
         (assoc-in [:status :reco query-key] d)))))

(re-frame/reg-sub
 ::selected-table
 (fn [db]
   (get-in db [:selected-table])))

(re-frame/reg-event-db
 ::select-table
 (fn [db [_ context_hash]]
   ;(ut/tapp>> [db-key result])
   (assoc db :selected-table context_hash)))

(re-frame/reg-sub
 ::selected-shape
 (fn [db]
   (get-in db [:selected-shape])))

(re-frame/reg-event-db
 ::select-shape
 (fn [db [_ context_hash]]
   ;(ut/tapp>> [db-key result])
   (assoc db :selected-shape context_hash)))

(defn refresh []
  (ut/tracked-dispatch [::wfx/request :default
                      {:message    {:kind :honey-call
                                    :ui-keypath [:connections]
                                    :honey-sql {:select-distinct [:connection_id :database_name :database_version]
                                                :from [:connections]}
                                    :extras {:requested? true
                                             :rando (rand-int 1234234525)}}
                       :on-response [::http/socket-response]
                       :on-timeout [::http/timeout-response]
                       :timeout    50000}]))

(re-frame/reg-sub
 ::field-count
 (fn [db]
   (get-in db [:field-count])))

(defn field-count []
  (ut/tracked-dispatch [::wfx/request :default
                      {:message    {:kind :honey-call
                                    :ui-keypath [:field-count]
                                    :honey-sql {:select [[[:count 1] :cnt]]
                                                :from [:fields]
                                                :where [:and
                                                        [:= :context_hash @(ut/tracked-subscribe [::selected-table])]
                                                        [:= :connection_id @(ut/tracked-subscribe [::selected])]]}
                                    :extras {:requested? true
                                             :rando (rand-int 1234234525)}}
                       :on-response [::http/socket-response]
                       :on-timeout [::http/timeout-response]
                       :timeout    50000}]))

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
     (cond (keyword? long-keyword) (get-it long-keyword)
           (or (list? long-keyword)
               (vector? long-keyword))  (into {} (for [k long-keyword] {k (get-it k)}))
           :else nil))))

(re-frame/reg-sub
 ::clicked-parameter
 (fn [db [_ keypath]]
   (let [val (get-in db (cons :click-param keypath))]
     ;(if (nil? val) (str "(nil row)") val)
    ; (when (cstr/includes? (str keypath) "theme") (ut/tapp>> [:clicked-param-fetch keypath val]))
     val)))

(defn contains-namespaced-keyword? [data]
  (cond
    (map? data) (some contains-namespaced-keyword? (concat (keys data) (vals data)))
    (vector? data) (some contains-namespaced-keyword? data)
    (keyword? data) (boolean (namespace data))
    :else false))

(re-frame/reg-sub ;; RESOLVE KEYPATH
 ::clicked-parameter-key  ;;; important, common, and likely more expensive than need be. TODO
 (fn [db [_ keypath]]
   (let [cmp (ut/splitter (ut/safe-name (nth keypath 0)) "/")
         kkey (keyword (ut/replacer (nth cmp 0) "-parameter" ""))
         vkey (keyword (peek cmp))
;;         _ (when (cstr/includes? (str keypath) ":query") (ut/tapp>> [:? keypath kkey vkey (get-in db (cons :click-param [kkey vkey]))]))
         val0 (get-in db (cons :click-param [kkey vkey]))
         if-walk-map2           (fn [obody] (let [kps       (ut/extract-patterns obody :if 4)
                                                  logic-kps (into {} (for [v kps]
                                                                       (let [kws (vec (filter #(cstr/includes? (str %) "/") (ut/deep-flatten v)))
                                                                             ;wm @(ut/tracked-subscribe [::resolve-click-param kws])
                                                                             wm @(rfa/sub ::resolve-click-param {:long-keyword kws})
                                                                             v0 (ut/postwalk-replacer wm v)
                                                                             [_ l this that] v0]
                                                                        ;(ut/tapp>> [:if-walk keypath v kps l this that kws (if l this that)])
                                                                         {v (if l this that)})))]
                                              (ut/postwalk-replacer logic-kps obody)))
         ;;val0 (resolver/logic-and-params val0 nil)
         val0 (try (if (= (first val0) :if) (if-walk-map2 val0) val0) (catch :default _ val0)) ;; TODO
        ;;  val0 (ut/postwalk-replacer {:text                  str
        ;;                               :case (fn [x] (ut/vectorized-case x))
        ;;                               :str (fn [args]
        ;;                                 ;  (cstr/join "" args)
        ;;                                 ; (do (ut/tapp>> [:str args])
        ;;                                      (apply str args);)
        ;;                                      )} val0)
         ns-kw? (and (cstr/starts-with? (str val0) ":") (cstr/includes? (str val0) "/") (keyword? val0))
         ;val (if (cstr/starts-with? ":theme/" (str val))
         ;      
         ;      )
         val (cond ns-kw?
                   (get-in db (let [sp (ut/splitter (str val0) "/")]
                                [:click-param (ut/unre-qword (ut/replacer (str (first sp)) ":" ""))
                                 (ut/unre-qword (last sp))]))
                   (map? val0)
                   (let [km (into {} (distinct (map #(when (>= (count %) 2) {(str (ut/replacer (str (get % 0)) ":" "") "/"
                                                                                  (ut/replacer (str (get % 1)) ":" "")) [(get % 0) (get % 1)]})
                                                    (ut/keypaths (get db :click-param)))))
                         rep-map (into {} (for [[k v] km] {(keyword k) (get-in db (cons :click-param v))}))]
                     ;(ut/tapp>> [:replace-map val0 rep-map])
                     (ut/postwalk-replacer rep-map val0))
                   :else val0)
         ;;val (ut/postwalk-replacer {:case (fn [x] (ut/vectorized-case x))} val)
        ;;  _ (ut/tapp>> [:val keypath val])


         ;;; recursize resolve parameter IF WHEN ETC here! TODO - need to recur see if-walk, else will be true due to keywords
         val      (if (and (string? val) (cstr/starts-with? (str val) ":") (not (cstr/includes? (str val) " ")))
       ;(ut/replacer (str val) ":" "")
                    (edn/read-string val) ;; temp hacky param work around (since we cant save keywords in the DB).. TODO :/
                    val)
         ;_ (ut/tapp>> [:resolver (logic-and-params-fn val nil)])
         contains-params? (contains-namespaced-keyword? val)
         ;;contains-sql-alias? (cstr/includes? (str val) ":query/")
         ;_ (when (cstr/includes? (str val) ":query/") (ut/tapp>> [:? val keypath kkey vkey (get-in db (cons :click-param [kkey vkey]))]))
         ;_ (when contains-sql-alias? (ut/tapp>> [:contains-sql-alias? keypath val0 val]))
         ;_ (when contains-params? (ut/tapp>> [:contains-params? keypath val0 val]))
         ]
     ;;(ut/tapp>> [:rec-param (ut/deep-template-find val0)])
    ; (when (cstr/includes? (str val) "theme") 
    ;   (ut/tapp>> [:clicked-param-key? keypath val (if-walk-map2 val) val0]))
    ; (when ns-kw?
    ;   (ut/tapp>> [:click-param ns-kw? val val0 (when ns-kw? (let [sp (ut/splitter (str val0) "/")]
    ;                                                      [:click-param (ut/unre-qword (ut/replacer (str (first sp)) ":" ""))
    ;                                                       (ut/unre-qword (last sp))])) val [kkey vkey]]))
     ;(if (nil? val) (str "(nil val)") val)

     ;(resolver/logic-and-params val nil)
     (if contains-params? ;; (and contains-params? (not contains-sql-alias?))
       (logic-and-params-fn val nil) ;; process again, no recursion here for now... we want to contain this reaction chain
       val)
     ;val
     )))


(re-frame/reg-sub ;; RESOLVE KEYPATH TEMP FOR TESTING
 ::clicked-parameter-key-alpha  ;;; important, common, and likely more expensive than need be. TODO
 (fn [db {:keys [keypath]}]
   (let [cmp (ut/splitter (ut/safe-name (nth keypath 0)) "/")
         kkey (keyword (ut/replacer (nth cmp 0) "-parameter" ""))
         vkey (keyword (peek cmp))
;;         _ (when (cstr/includes? (str keypath) ":query") (ut/tapp>> [:? keypath kkey vkey (get-in db (cons :click-param [kkey vkey]))]))
         val0 (get-in db (cons :click-param [kkey vkey]))
         if-walk-map2           (fn [obody] (let [kps       (ut/extract-patterns obody :if 4)
                                                  logic-kps (into {} (for [v kps]
                                                                       (let [kws (vec (filter #(cstr/includes? (str %) "/") (ut/deep-flatten v)))
                                                                             ;wm @(ut/tracked-subscribe [::resolve-click-param kws])
                                                                             wm @(rfa/sub ::resolve-click-param {:long-keyword kws})
                                                                             v0 (ut/postwalk-replacer wm v)
                                                                             [_ l this that] v0]
                                                                        ;(ut/tapp>> [:if-walk keypath v kps l this that kws (if l this that)])
                                                                         {v (if l this that)})))]
                                              (ut/postwalk-replacer logic-kps obody)))
         ;;val0 (resolver/logic-and-params val0 nil)
         val0 (try (if (= (first val0) :if) (if-walk-map2 val0) val0) (catch :default _ val0)) ;; TODO
        ;;  val0 (ut/postwalk-replacer {:text                  str
        ;;                               :case (fn [x] (ut/vectorized-case x))
        ;;                               :str (fn [args]
        ;;                                 ;  (cstr/join "" args)
        ;;                                 ; (do (ut/tapp>> [:str args])
        ;;                                      (apply str args);)
        ;;                                      )} val0)
         ns-kw? (and (cstr/starts-with? (str val0) ":") (cstr/includes? (str val0) "/") (keyword? val0))
         ;val (if (cstr/starts-with? ":theme/" (str val))
         ;      
         ;      )
         val (cond ns-kw?
                   (get-in db (let [sp (ut/splitter (str val0) "/")]
                                [:click-param (ut/unre-qword (ut/replacer (str (first sp)) ":" ""))
                                 (ut/unre-qword (last sp))]))
                   (map? val0)
                   (let [km (into {} (distinct (map #(when (>= (count %) 2) {(str (ut/replacer (str (get % 0)) ":" "") "/"
                                                                                  (ut/replacer (str (get % 1)) ":" "")) [(get % 0) (get % 1)]})
                                                    (ut/keypaths (get db :click-param)))))
                         rep-map (into {} (for [[k v] km] {(keyword k) (get-in db (cons :click-param v))}))]
                     ;(ut/tapp>> [:replace-map val0 rep-map])
                     (ut/postwalk-replacer rep-map val0))
                   :else val0)
         ;;val (ut/postwalk-replacer {:case (fn [x] (ut/vectorized-case x))} val)
        ;;  _ (ut/tapp>> [:val keypath val])


         ;;; recursize resolve parameter IF WHEN ETC here! TODO - need to recur see if-walk, else will be true due to keywords
         val      (if (and (string? val) (cstr/starts-with? (str val) ":") (not (cstr/includes? (str val) " ")))
       ;(ut/replacer (str val) ":" "")
                    (edn/read-string val) ;; temp hacky param work around (since we cant save keywords in the DB).. TODO :/
                    val)
         ;_ (ut/tapp>> [:resolver (logic-and-params-fn val nil)])
         contains-params? (contains-namespaced-keyword? val)
         ;contains-sql-alias? (cstr/includes? (str val) ":query/")
         ;_ (when (cstr/includes? (str val) ":query/") (ut/tapp>> [:? val keypath kkey vkey (get-in db (cons :click-param [kkey vkey]))]))
         ;_ (when contains-sql-alias? (ut/tapp>> [:contains-sql-alias? keypath val0 val]))
         ;_ (when contains-params? (ut/tapp>> [:contains-params? keypath val0 val]))
         ]
     ;;(ut/tapp>> [:rec-param (ut/deep-template-find val0)])
    ; (when (cstr/includes? (str val) "theme") 
    ;   (ut/tapp>> [:clicked-param-key? keypath val (if-walk-map2 val) val0]))
    ; (when ns-kw?
    ;   (ut/tapp>> [:click-param ns-kw? val val0 (when ns-kw? (let [sp (ut/splitter (str val0) "/")]
    ;                                                      [:click-param (ut/unre-qword (ut/replacer (str (first sp)) ":" ""))
    ;                                                       (ut/unre-qword (last sp))])) val [kkey vkey]]))
     ;(if (nil? val) (str "(nil val)") val)

     ;(resolver/logic-and-params val nil)
     (if contains-params? ;; (and contains-params? (not contains-sql-alias?))
       (logic-and-params-fn val nil) ;; process again, no recursion here for now... we want to contain this reaction chain
       val)
     ;val
     )))


(re-frame/reg-event-db
 ::click-parameter
 (fn [db [_ keypath value]]
   ;;(ut/tapp>> [:click-parameter keypath value])
   (let [cc (get-in db (cons :click-param keypath))]
     (assoc-in db (cons :click-param keypath)
               (if (and (not (= (first keypath) :param)) (= cc value)) ;; unset to nil if same value UNLESS is a user param... (breaks open-input UI)
                 nil value)))))

(re-frame/reg-event-db
 ::declick-parameter
 (undoable)
 (fn [db [_ keypath]]
  ;;  (ut/tapp>> [:declick (cons :click-param keypath)])
   (ut/dissoc-in db (vec (cons :click-param keypath)))))

(re-frame/reg-event-db
 ::cell-click-parameter
 (fn [db [_ keypath value]]
   (ut/tapp>> [:cell-click-parameter keypath value])

   (let [cc (get-in db (cons :click-param keypath))
         new (vec (distinct (cond (vector? cc)
                                  (vec (conj cc value))
                                  (nil? cc) [value]
                                  :else (vec (conj [cc] value)))))
         new-vec (vec (flatten (cond ;(= cc value) nil ;; drop it?
                                 (try (some #(= % value) cc) (catch :default _ false)) (remove #(= % value) cc)
                                 :else new)))]
     (ut/tapp>> [:cell-click-parameter-existing-val (cons :click-param keypath) value cc new new-vec (empty? new-vec)])
     (if (empty? new-vec)
       (ut/dissoc-in db (vec (cons :click-param keypath))) ;; empty list, remove param
       ;(assoc-in db (cons :click-param keypath) nil)
       (assoc-in db (cons :click-param keypath) new-vec))))) ;; otherwise commit new list

(re-frame/reg-sub
 ::sql-data
 (fn [db [_ keypath]]
   (get-in db (cons :data keypath))))

(re-frame/reg-sub
 ::sql-data-alpha
 (fn [db {:keys [keypath]}]
   (get-in db (cons :data keypath))))

(re-frame/reg-sub
 ::sql-metadata
 (fn [db [_ keypath]]
   (get-in db (cons :meta keypath))))

(re-frame/reg-sub
 ::sql-metadata-alpha
 (fn [db {:keys [keypath]}]
   (get-in db (cons :meta keypath))))

(re-frame/reg-sub
 ::sql-post-metadata
 (fn [db [_ keypath]]
   (get-in db (cons :post-meta keypath))))

(re-frame/reg-sub
 ::sql-post-metadata-alpha
 (fn [db {:keys [keypath]}]
   (get-in db (cons :post-meta keypath))))

(re-frame/reg-sub
 ::sql-post-styles
 (fn [db [_ keypath]]
   (get-in db (cons :post-styles keypath))))

(re-frame/reg-sub
 ::sql-post-styles-row-lookups
 (fn [db [_ keypath]]
   (let [res (get (get-in db (cons :post-styles keypath)) :*)]
     (into {}
           (for [[name {:keys [results styles]}] res]
             {name {:rows (vec (remove nil? (for [i (range (count results))]
                                              (when (= (get-in results [i :v]) 1)
                                                i))))
                    :styles styles}})))))

(re-frame/reg-sub
 ::sql-post-styles-cell-lookups
 (fn [db [_ keypath]]
   (let [res (get-in db (cons :post-styles keypath))]
     (dissoc (into {} (for [f (keys res)]
                        {f (into {}
                                 (for [[name {:keys [results styles]}] (get res f)]
                                   {name {:rows (vec (remove nil? (for [i (range (count results))]
                                                                    (when (= (get-in results [i :v]) 1)
                                                                      i))))
                                          :styles styles}}))})) :*))))

(re-frame/reg-sub
 ::sql-merged-metadata
 (fn [db [_ keypath]]
   (let [pm (get-in db (cons :post-meta keypath))
         m (get-in db (cons :meta keypath))]
     {:fields (into {} (for [[k v] (get m :fields)]
                         {k (merge v (get pm k))}))
      :rowcount (get-in pm [:* :rowcount])})))

(re-frame/reg-sub
 ::sql-data-exists?
 (fn [db [_ keypath]]
   (not (nil? (get-in db (cons :data keypath))))))

(re-frame/reg-sub
 ::sql-data-text
 (fn [db [_ keypath]]
   (str (get-in db (cons :data keypath)))))

(re-frame/reg-sub
 ::sql-data-boxes
 (fn [db [_ keypath clickable? style]]
   (let [styles (if (and (map? style) (not (nil? style))) style {})]
     (vec (for [r (get-in db (cons :data keypath))]
            [re-com/box
             :size "auto" :padding "4px"
             :attr (if clickable?
                     {:on-click #(ut/tracked-dispatch [::click-parameter keypath r])}
                     {})
             :child (str r)
             :style (merge styles {:cursor (if clickable? "pointer" "inherit")
                                   :background-color (if (= r @(ut/tracked-subscribe [::clicked-parameter keypath]))
                                                       "grey"
                                                       "inherit")})])))))

(re-frame/reg-sub
 ::sql-data-boxes-values
 (fn [db [_ keypath clickable? style]]
   (let [styles (if (and (map? style) (not (nil? style))) style {})]
     (vec (for [r (get-in db (cons :data keypath))]
            [re-com/box
             :size "auto" :padding "4px"
             :attr (if clickable?
                     {;:on-click #(ut/tracked-dispatch [::click-parameter keypath r])
                      :on-click (re-com/handler-fn (ut/tracked-dispatch [::click-parameter keypath r]))}
                     {})
             :child (str (vals r))
             :style (merge styles {:cursor (if clickable? "pointer" "inherit")
                                   :background-color (if (= r @(ut/tracked-subscribe [::clicked-parameter keypath]))
                                                       "grey"
                                                       "inherit")})])))))

(re-frame/reg-sub
 ::sql-query-not-run?
 (fn [db [_ keypath query]] ;; LOGIC HERE NEEDS TO BE THE SAME AS conn/sql-data ore shit gets weird
   (let [;query (dissoc query :col-widths)
        ;;  style-rules (get query :style-rules) ;; rules code dupe from conn/sql-data
        ;;  has-rules? (and (not (nil? style-rules)) (ut/ne? style-rules))
        ;;  rules (when has-rules?
        ;;          (vec (for [[[col name] logic] style-rules]
        ;;                 [[:case (:logic logic) 1 :else 0]
        ;;                  (keyword (str "styler_" (ut/safe-name name)))])))
         query (ut/clean-sql-from-ui-keys query)
         ;;_ (ut/tapp>> [keypath 0 (hash query) (str query)])
        ;;  hselect (get query :select)
        ;;  query (if has-rules?
        ;;          (assoc query :select (apply merge hselect rules))
        ;;          query)
         not-col-sel? (not (= (first keypath) (get-in db [:selected-cols 1])))]
    ;;  (ut/tapp>> [:not-col-sel? keypath (hash query) (get-in db (cons :query-history keypath))  ])
     (and not-col-sel?
          (not (= (hash query) (get-in db (cons :query-history keypath))))))))

(re-frame/reg-event-db
 ::add-to-sql-history ;; :ran tap is good
 (fn [db [_ keypath query]]
   ;(ut/tapp>> [:ran keypath query])
   (let [;query (dissoc query :col-widths)
        ;;  style-rules (get query :style-rules) ;; rules code dupe from conn/sql-data
        ;;  has-rules? (and (not (nil? style-rules)) (ut/ne? style-rules))
        ;;  rules (when has-rules?
        ;;          (vec (for [[[col name] logic] style-rules]
        ;;                 [[:case (:logic logic) 1 :else 0]
        ;;                  (keyword (str "styler_" (ut/safe-name name)))])))
         query (ut/clean-sql-from-ui-keys query)
        ;;  hselect (get query :select)
        ;;  query (if has-rules?
        ;;          (assoc query :select (apply merge hselect rules))
        ;;          query)
         base-sniff? (true? (= (get query :limit) 111))]
     ;;(ut/tapp>> [keypath (hash query) (str query)])
     (if base-sniff?
       (-> db
           (assoc-in (cons :base-sniff-queries keypath) (hash query))
           (assoc-in (cons :query-history keypath) (hash query)))
       (assoc-in db (cons :query-history keypath) (hash query))))))

(re-frame/reg-event-db
 ::add-to-sql-history-meta ;; :ran tap is good
 (fn [db [_ keypath query]]
   ;(ut/tapp>> [:ran-meta keypath query])
   (let [;query (dissoc query :col-widths)
         query (ut/clean-sql-from-ui-keys query)
         ]
     (assoc-in db (cons :query-history-meta keypath) (hash query)))))

(re-frame/reg-sub
 ::lookup-panel-key-by-query-key-alpha
 (fn [db {:keys [query-key]}]
   (first (remove nil? (for [[k v] (get db :panels)]
                         (when (some #(= query-key %)
                                     (keys (get v :queries))) k))))))

(re-frame/reg-event-db
 ::clear-query-history
 (fn [db [_ query-id]]
   ;(ut/tapp>> [:cleared-query-history query-id])
   (let [;panel @(ut/tracked-subscribe [::lookup-panel-key-by-query-key query-id])
         panel @(rfa/sub ::lookup-panel-key-by-query-key-alpha {:query-key query-id})
         ;selected (get db :selected-block)
         ]
     (-> db
       ;(ut/dissoc-in [:data query-id])
        ;;  (assoc-in (if (not (= panel selected))
        ;;              [:panels panel :queries query-id :_last-run]
        ;;              [:last-run-throwaway])
        ;;            (ut/get-time-format-str)) ;; this is weird. was not needed before, but code-tables were not refreshing w/o a render kick... 1/12/24
         
         (assoc-in [:panels panel :queries query-id :_last-run] (ut/get-time-format-str))
         ;; ^^ this is weird. was not needed before, but code-tables were not refreshing w/o a render kick... 1/12/24
         (ut/dissoc-in [:query-history query-id])
         (ut/dissoc-in [:query-history-meta query-id])))))

(re-frame/reg-sub
 ::sql-meta-not-run?
 (fn [db {:keys [keypath query]}]
   (let [;query (dissoc query :col-widths)
         query (ut/clean-sql-from-ui-keys query)
         not-col-sel? (not (= (first keypath) (get-in db [:selected-cols 1])))]
     (and not-col-sel?
          (not (= (hash query) (get-in db (cons :query-history-meta keypath))))))))

(re-frame/reg-sub
 ::sql-style-not-run?
 (fn [db {:keys [keypath query pquery]}]
   (let [;query (dissoc query :col-widths)
         query (ut/clean-sql-from-ui-keys query)
         not-col-sel? (not (= (first keypath) (get-in db [:selected-cols 1])))]
     (and not-col-sel?
          (not (= (hash (str pquery query)) (get-in db (cons :query-history-style keypath))))))))

(re-frame/reg-sub
 ::sql-tab-not-run?
 (fn [db [_ keypath query pquery]]
   (let [;query (dissoc query :col-widths)
         query (ut/clean-sql-from-ui-keys query)
         not-col-sel? (not (= (first keypath) (get-in db [:selected-cols 1])))]
     (and not-col-sel?
          (not (= (hash (str pquery query)) (get-in db (cons :query-history-tab keypath))))))))

(re-frame/reg-event-db
 ::add-to-sql-history-tab
 (fn [db [_ keypath query pquery]]
   ;(ut/tapp>> [:ran-tab keypath query])
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
         query (ut/clean-sql-from-ui-keys query)
         not-col-sel? (not (= (first keypath) (get-in db [:selected-cols 1])))]
     (and not-col-sel?
          (not (= (hash (str pquery query)) (get-in db (cons :query-history-condi keypath))))))))

(re-frame/reg-event-db
 ::add-to-sql-history-condi
 (fn [db [_ keypath query pquery]]
   ;(ut/tapp>> [:ran-condi keypath query])
   (let [;query (dissoc query :col-widths)
         query (ut/clean-sql-from-ui-keys query)]
     (assoc-in db (cons :query-history-condi keypath) (hash (str pquery query))))))

(re-frame/reg-sub
 ::condi-value
 (fn [db {:keys [condi-key]}]
   (true? (= 1 (get-in db [:post-condi condi-key 0 :v])))))

(re-frame/reg-sub
 ::client-name
 (fn [db]
   (get db :client-name)))

(defn theme-pull [cmp-key fallback & test-fn]
  (let [;v                   @(ut/tracked-subscribe [::clicked-parameter-key [cmp-key]])
        v                   @(rfa/sub ::clicked-parameter-key-alpha {:keypath [cmp-key]})
        t0                  (ut/splitter (str (ut/safe-name cmp-key)) #"/")
        t1                  (keyword (first t0))
        t2                  (keyword (last t0))
        self-ref-keys       (distinct (filter #(and (keyword? %) (namespace %)) (ut/deep-flatten db/base-theme)))
        self-ref-pairs      (into {}
                                  (for [k self-ref-keys     ;; todo add a reurziver version of this
                                        :let [bk (keyword (ut/replacer (str k) ":theme/" ""))]]
                                    {k (get db/base-theme bk)}))
        resolved-base-theme (ut/postwalk-replacer self-ref-pairs db/base-theme)
        base-theme-keys     (keys resolved-base-theme)
        theme-key?          (true? (and (= t1 :theme) (some #(= % t2) base-theme-keys)))
        fallback0           (if theme-key? (get resolved-base-theme t2) fallback)]
    ;(ut/tapp>> [:theme-test theme-key? resolved-base-theme])
    ;(ut/tapp>> [:color-pull cmp-key t1 t2 fallback fallback0 theme-key? (get db/base-theme t2) base-theme-keys])
    (if (not (nil? v)) v fallback0)))

;; (def data-colors (theme-pull :theme/data-colors db/data-colors)) 

(re-frame/reg-sub
 ::data-colors
 (fn [_]
   (theme-pull :theme/data-colors db/data-colors)))

(def data-colors 
  ;@(ut/tracked-subscribe [::data-colors])
  @(re-frame.alpha/sub ::data-colors)
  ) ;; kinda weird usage

(re-frame/reg-sub
 ::sql-source
 (fn [db {:keys [kkey]}]
   (get-in db [:sql-source kkey] {})))

(defn sql-deep-meta [keypath honey-sql connection-id & [deeps?]]
  ;(ut/tapp>> [:sql-deep-meta keypath (str (first keypath)) (cstr/starts-with? (str (first keypath)) ":query-preview" )])
  (when (not (cstr/starts-with? (str (first keypath)) ":query-preview")) ;; lets not sniff rando recos, ok?
    (let [fields (get @(ut/tracked-subscribe [::sql-metadata keypath]) :fields [])
          ;;_ (when (cstr/includes? (str keypath) ":kick") (ut/tapp>> [:deep-meta keypath honey-sql]))
          honey-sql (if (empty? (ut/clean-sql-from-ui-keys honey-sql))
                      ;;@(ut/tracked-subscribe [::sql-source (first keypath)])
                      @(rfa/sub ::sql-source {:kkey (first keypath)})
                      honey-sql)
          ;; ^^ kicks dont get passed honey-sql map correctly, so we look it up
          connection-id (or (get honey-sql :connection-id)
                            (if (nil? connection-id) "cache.db" connection-id))
          deep-meta? (or deeps? (get honey-sql :deep-meta?))
          ;; honey-sql (-> honey-sql
          ;;               (dissoc :page)
          ;;               (dissoc :col-widths)
          ;;               (dissoc :row-height)
          ;;               (dissoc :render-all?)
          ;;               (dissoc :cache?)
          ;;               (dissoc :refresh-every)
          ;;               (dissoc :deep-meta?)
          ;;               (dissoc :clicked-row-height)
          ;;               (dissoc :style-rules))
          honey-sql (ut/clean-sql-from-ui-keys honey-sql)]
      (when (cstr/includes? (str keypath) ":kick") (ut/tapp>> [:deep-meta? keypath fields honey-sql connection-id]))
   ; (dorun
      (doseq
      ;(for 
       [[[name f] hsql] (merge {[:rowcount :*] {:select [[[:count 1] :rowcnt]] :from [[honey-sql :subq]]}}

                               (if deep-meta? ;; get distinct counts and other shiz if deep-meta (sad that its too expensive for everything)
                                 (into {} (for [field
                                                 ;(keys fields)
                                                (filter #(not (cstr/starts-with? (str %) ":styler_")) (keys fields)) ;; no styler fields...
                                                ]
                                            {[:distinct field]
                                             {:select [[[:count [:distinct field]] :distinct-values]] :from [[honey-sql :subq]]}

                                             ; [:min field]
                                             ; {:select [[[:min field] :min-value]] :from [[honey-sql :subq]]}

                                             ; [:max field]
                                             ; {:select [[[:max field] :max-value]] :from [[honey-sql :subq]]}
                                             })){}))]
        (when ;true 
        ;(or @(ut/tracked-subscribe [::sql-query-not-run? keypath honey-sql])
         (and (not (= keypath [:query-preview])) ;; no need to meta on browsing viz recos 
                ; (= (count @(ut/tracked-subscribe [::wfx/pending-requests http/socket-id])) 0)
                ;; false
              ;;@(ut/tracked-subscribe [::sql-meta-not-run? (conj (conj keypath f) name) hsql])
              @(rfa/sub ::sql-meta-not-run? {:keypath (conj (conj keypath f) name) :query hsql}))

          (dorun
           (ut/tracked-dispatch [::wfx/request :default
                                 {:message    {:kind :honey-xcall
                                               :ui-keypath (conj (conj keypath f) name)
                                               :honey-sql hsql
                                               :connection-id connection-id
                                               :client-name @(ut/tracked-subscribe [::client-name])}
                                  :on-response [::http/socket-response-post-meta]
                                  :on-timeout [::http/timeout-response (conj (conj keypath f) name)] ;; requeue?
                                  :timeout    50000}])
           (ut/tracked-dispatch [::add-to-sql-history-meta (conj (conj keypath f) name) hsql])))))))

(def deep-meta-on-deck (atom nil))

(re-frame/reg-event-db
 ::run-sql-deep-meta-for
 (fn [db [_ panel-key query-key honey-sql]]
   (let [connection-id (get-in db [:panels panel-key :connection-id])] 
     (ut/tapp>> [:manual-deep-meta query-key :on connection-id])
     ;(sql-deep-meta [query-key] honey-sql connection-id true)
     (reset! deep-meta-on-deck query-key)
     (ut/tracked-dispatch [::clear-query-history query-key])
     db)))

(defn sql-style-meta [keypath honey-sql connection-id] 
  (let [style-rules (get honey-sql :style-rules)]
    (doseq
     [[[f name] {:keys [style logic]}] style-rules]
      (let [kp (conj (conj (conj keypath f) name) :styles)
            hsql {:select [[[:case logic 1 :else 0] :v]]
                  :from [(ut/keypath-munger keypath)]}]
        (when
         (and
          ;;@(ut/tracked-subscribe [::sql-style-not-run? kp hsql honey-sql])
          @(rfa/sub ::sql-style-not-run? {:keypath kp :query hsql :pquery honey-sql})
          (not @(ut/tracked-subscribe [::sql-query-not-run? keypath honey-sql])))

          (dorun
           (ut/tracked-dispatch [::wfx/request :default
                               {:message    {:kind :honey-xcall
                                             :ui-keypath kp
                                             :honey-sql hsql
                                             :connection-id :cache ;connection-id
                                             :client-name @(ut/tracked-subscribe [::client-name])}
                                :on-response [::http/socket-response-post-style style]
                                :on-timeout [::http/timeout-response [keypath honey-sql]]
                                :timeout    50000}])
           (ut/tracked-dispatch [::add-to-sql-history-style kp hsql honey-sql])))))))

(defn sql-tab-meta [keypath rules panel-key]
  (let [;style-rules (get honey-sql :style-rules) 
        kk (str rules panel-key)]
    ;(ut/tapp>> [keypath rules panel-key])
    (doseq
     [[[f name] logic] [rules]]
      (let [kp ;(conj 
            (conj (conj keypath f) name)
               ; :tabs)
            hsql {:select [[[:case logic 1 :else 0] :v]]
                  ;:from [(ut/keypath-munger keypath)] 
                  }]
        ;(ut/tapp>> [kp hsql])
        (when ;true
         ;(and
         @(ut/tracked-subscribe [::sql-tab-not-run? kp hsql kk])
          ;(not @(ut/tracked-subscribe [::sql-query-not-run? keypath rules]))
         ; )

          (dorun
           (ut/tracked-dispatch [::wfx/request :default
                               {:message    {:kind :honey-xcall
                                             :ui-keypath kp
                                             :honey-sql hsql
                                             :connection-id :cache ;connection-id
                                             :client-name @(ut/tracked-subscribe [::client-name])}
                                :on-response [::http/socket-response-post-tab panel-key]
                                :on-timeout [::http/timeout-response [keypath rules]]
                                :timeout    50000}])
           (ut/tracked-dispatch [::add-to-sql-history-tab kp hsql kk])))))))

(defn sql-condi-meta [keypath rules]
  (let [;style-rules (get honey-sql :style-rules) 
        kk (str rules)]
    ;(ut/tapp>> [keypath rules ])
    (doseq
     [logic [rules]]
      (let [kp ;(conj 
            keypath ;(conj keypath name)
               ; :tabs)
            hsql {:select [[[:case logic 1 :else 0] :v]]
                  ;:from [(ut/keypath-munger keypath)] 
                  }]
        ;(ut/tapp>> [kp hsql])
        (when ;true
         ;(and
         ;@(ut/tracked-subscribe [::sql-condi-not-run? kp hsql kk])
         @(rfa/sub ::sql-condi-not-run? {:keypath kp :query hsql :pquery kk})
          ;(not @(ut/tracked-subscribe [::sql-query-not-run? keypath rules]))
         ; )

          (dorun
           (ut/tracked-dispatch [::wfx/request :default
                                 {:message    {:kind :honey-xcall
                                               :ui-keypath kp
                                               :honey-sql hsql
                                               :connection-id :cache ;connection-id
                                               :client-name @(ut/tracked-subscribe [::client-name])}
                                  :on-response [::http/socket-response-post-condi]
                                  :on-timeout [::http/timeout-response [keypath rules]]
                                  :timeout    50000}])
           (ut/tracked-dispatch [::add-to-sql-history-condi kp hsql kk])))))))

(defn push-panels-to-server [panels-map resolved-panels-map client-name]
  (let []
    (dorun
     (ut/tracked-dispatch [::wfx/request   :default
                         {:message    {:kind :current-panels
                                       :panels panels-map
                                       :resolved-panels resolved-panels-map
                                       ;:ui-keypath kp
                                       ;:honey-sql hsql
                                       ;:connection-id :cache ;connection-id
                                       :client-name client-name}
                          :timeout 50000;;@(ut/tracked-subscribe [::client-name])}
                          ;:on-response [::http/socket-response-post-condi]
                          ;:on-timeout [::http/timeout-response [keypath rules]]
                          ;:timeout    150000
                          }]))))

(re-frame/reg-event-db
 ::set-query-schedule
 (fn [db [_ query-id schedule]]
  ;;(ut/tapp>> [:set-query-schedule query-id schedule (+ (get-in db [:re-pollsive.core/polling :counter]) schedule)])
   (let [timer (+ (get-in db [:re-pollsive.core/polling :counter]) schedule)]
     (assoc-in db [:sched query-id] timer))))

(re-frame/reg-sub ;;; dupe from bricks!
 ::lookup-panel-key-by-query-key
 (fn [db [_ query-key]]
   (first (remove nil? (for [[k v] (get db :panels)]
                         (when (some #(= query-key %)
                                     (keys (get v :queries))) k))))))

(re-frame/reg-sub ;;; dupe from bricks!
 ::client-name
 (fn [db _]
   (get db :client-name)))

(defn sql-data
  ([keypath honey-sql]
   (let [style-rules (get honey-sql :style-rules)
         deep-meta? (true? (= @deep-meta-on-deck (first keypath)))
          ;sniff? (true? (ut/not-empty? (get @db/sniff-deck (first keypath))))
         ;clover-sql honey-sql
         sniff? (= (get @db/sniff-deck (first keypath)) :reco)
         kit-name (when (and (not sniff?)
                             (keyword? (get @db/sniff-deck (first keypath))))
                    (get @db/sniff-deck (first keypath)))
         has-rules? (and (not (nil? style-rules)) (ut/ne? style-rules))
         rules (when has-rules?
                 (vec (for [[[col name] logic] style-rules]
                        [[:case (:logic logic) 1 :else 0]
                         (keyword (str "styler_" (ut/safe-name name)))])))
         panel-key @(rfa/sub ::lookup-panel-key-by-query-key-alpha {:query-key (first keypath)})
         clover-sql (assoc honey-sql :connection-id "system-db")
         honey-sql (ut/clean-sql-from-ui-keys honey-sql)
         hselect (get honey-sql :select)
         flat (ut/deep-flatten honey-sql)
         connection-id nil ;(get honey-sql :connection-id)

         literal-data? (and (some #(= % :data) flat)
                            (not (some #(= % :panel_history) flat)))
         honey-modded (if has-rules?
                        (assoc honey-sql :select (apply merge hselect rules))
                        honey-sql)
         client-name @(ut/tracked-subscribe [::client-name])
         honey-modded (ut/postwalk-replacer {:*client-name client-name
                                             :*client-name-str (pr-str client-name)} honey-modded)]
     (ut/tracked-dispatch [::wfx/request :default
                           {:message    {:kind (if (or connection-id literal-data?)
                                                 :honey-xcall
                                                 :honey-call) ;; override for data literals
                                       ;:kind :honey-xcall
                                         :ui-keypath keypath
                                         :panel-key panel-key
                                         :deep-meta? deep-meta?
                                         :kit-name kit-name
                                         :clover-sql clover-sql
                                         :honey-sql honey-modded
                                         :client-cache? (if literal-data? (get honey-sql :cache? true) false)
                                         :page (get honey-sql :page)
                                         :sniff? sniff? ;; on used for ext tables
                                      ;;  :connection-id connection-id
                                    ;;  :honey-sql (-> honey-sql
                                    ;;                 (dissoc :col-widths)
                                    ;;                 (dissoc :row-height)
                                    ;;                 (dissoc :render-all?)
                                    ;;                 (dissoc :cache?)
                                    ;;                 (dissoc :deep-meta?)
                                    ;;                 (dissoc :refresh-every)
                                    ;;                 (dissoc :clicked-row-height)
                                    ;;                 (dissoc :style-rules)) 
                                         :client-name @(ut/tracked-subscribe [::client-name])}
                            :on-response [::http/socket-response]
                            :on-timeout [::http/timeout-response [keypath honey-sql]]
                            :timeout    50000}])

     (when deep-meta? (reset! deep-meta-on-deck nil)))

   (when (not (nil? (get honey-sql :refresh-every)))
     (ut/tracked-dispatch [::set-query-schedule (first keypath) (get honey-sql :refresh-every)]))
   

  ;;  (when (and (some #(= % :data) (ut/deep-flatten honey-sql)) 
  ;;             (not (= 111 (get honey-sql :limit)))) ;; <- code for a base table sniff 
  ;;    (ut/tracked-dispatch-sync [::set-reco-status (first keypath) :started]))
   (ut/tracked-dispatch [::add-to-sql-history keypath honey-sql]))

  ([keypath honey-sql connection-id]
   (doall
    ;(ut/tapp>> [:hey? (first keypath)])
    (let [style-rules (get honey-sql :style-rules)
          deep-meta? (true? (= @deep-meta-on-deck (first keypath)))
          ;sniff? (true? (ut/not-empty? (get @db/sniff-deck (first keypath))))
          sniff? (= (get @db/sniff-deck (first keypath)) :reco)
          kit-name (when (and (not sniff?)
                              (keyword? (get @db/sniff-deck (first keypath))))
                     (get @db/sniff-deck (first keypath)))
          has-rules? (and (not (nil? style-rules)) (ut/ne? style-rules))
          rules (when has-rules?
                  (vec (for [[[col name] logic] style-rules]
                         [[:case (:logic logic) 1 :else 0]
                          (keyword (str "styler_" (ut/safe-name name)))])))
          connection-id (cond (get honey-sql :connection-id) (get honey-sql :connection-id)
                              (nil? connection-id) "cache.db"
                              (= connection-id "system") "system-db"
                              :else connection-id)
          clover-sql (assoc honey-sql :connection-id connection-id)
          refresh-every (get honey-sql :refresh-every)
          ;;_ (ut/tapp>> refresh-every)
          cache? (get honey-sql :cache? false)
          page (get honey-sql :page)
          ;;panel-key @(ut/tracked-subscribe [::lookup-panel-key-by-query-key (first keypath)])
          panel-key @(rfa/sub ::lookup-panel-key-by-query-key-alpha {:query-key (first keypath)})
          honey-sql (ut/clean-sql-from-ui-keys honey-sql)
          ;; honey-sql (-> honey-sql
          ;;               (dissoc :col-widths)
          ;;               (dissoc :row-height)
          ;;               (dissoc :render-all?)
          ;;               (dissoc :cache?)
          ;;               (dissoc :refresh-every)
          ;;               (dissoc :deep-meta?)
          ;;               (dissoc :clicked-row-height)
          ;;               (dissoc :style-rules))
          hselect (get honey-sql :select)
          honey-modded (if has-rules?
                         (assoc honey-sql :select (apply merge hselect rules))
                         honey-sql)
          client-name @(ut/tracked-subscribe [::client-name])
          honey-modded (ut/postwalk-replacer {:*client-name client-name
                                               :*client-name-str (str client-name)} honey-modded)]
          

      (do (when (not (nil? refresh-every)) (ut/tracked-dispatch [::set-query-schedule (first keypath) refresh-every]))

          (ut/tracked-dispatch [::wfx/request :default
                              {:message    {:kind :honey-xcall
                                            :ui-keypath keypath
                                            :panel-key panel-key
                                            :kit-name kit-name
                                            :clover-sql clover-sql
                                            :deep-meta? deep-meta?
                                            :page page
                                            :client-cache? cache?
                                            :sniff? sniff?
                                            :honey-sql honey-modded
                                            :connection-id connection-id
                                            :client-name @(ut/tracked-subscribe [::client-name])}
                               :on-response [::http/socket-response]
                               :on-timeout [::http/timeout-response [keypath honey-sql]]
                               :timeout    50000}])
          
          (when deep-meta? (reset! deep-meta-on-deck nil))

          (when (or kit-name  ;; clear on deck atom 
                    sniff?) (swap! db/sniff-deck dissoc (first keypath)))
          ;; clear out the sniff deck (even if not needed)

    ;;  (when (not (= 111 (get honey-sql :limit)))
    ;;    (ut/tracked-dispatch-sync [::set-reco-status (first keypath) :started]))
          (ut/tracked-dispatch [::add-to-sql-history keypath honey-sql]))))
   ;(ut/tracked-dispatch-sync [::add-to-sql-history keypath honey-sql])
   ;(sql-deep-meta keypath honey-sql connection-id)
   ))



;;;  (assoc-in db [:post-meta ui-keypath field name] new-map)





;; (re-frame/reg-sub
;;  ::xsql-data
;;  (fn [db [_ keypath]]
;;    (get-in db (cons :xsql keypath))))

;; (defn xsql-data [keypath honey-sql connection-id]
;;   (ut/tracked-dispatch [::wfx/request :default
;;                       {:message    {:kind :honey-xcall
;;                                     :ui-keypath (cons :xsql keypath) ;keypath
;;                                     :honey-sql (-> honey-sql
;;                                                    (dissoc :col-widths)
;;                                                    (dissoc :style-rules))
;;                                     :connection-id connection-id
;;                                     :extras {}}
;;                        :on-response [::http/socket-response]
;;                        :on-timeout [::http/timeout-response]
;;                        :timeout    500000}]))





;; general sql query to keypath fn here
;; can we do it as a sub? so it executes AND subs w/o getting out of control?


