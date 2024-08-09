(ns rvbbit-frontend.resolver
  (:require
    [clojure.edn             :as edn]
    [clojure.string          :as cstr]
    [clojure.walk            :as walk]
    [day8.re-frame.undo      :as    undo
                             :refer [undoable]]
    [goog.i18n.NumberFormat.Format]
    [re-frame.alpha          :as rfa]
    [re-frame.core           :as re-frame]
    [reagent.core            :as reagent]
    [rvbbit-frontend.connections :as conn]
    [rvbbit-frontend.db      :as db]
    [rvbbit-frontend.http    :as http]
    [rvbbit-frontend.utility :as ut]
    [websocket-fx.core       :as wfx]))

(declare logic-and-params)

(re-frame/reg-sub
 ::sizes-alpha
 (fn [db {:keys [panel-key]}]
   (into
    (get-in db [:panels panel-key :root])
    [(get-in db [:panels panel-key :h])
     (get-in db [:panels panel-key :w])])))

(re-frame/reg-sub ;; dupe from bricks, consolidate to more category focused namespace for all subs and events 
 ::client-name 
 (fn [db] (get db :client-name)))

(re-frame/reg-sub ;; dupe from bricks, consolidate to more category focused namespace for all subs and events 
 ::all-roots-tab-sizes-current
 (fn [db _]
   (vec (for [[_ v] (into {} (filter #(and (not (get (val %) :minimized? false))
                                           (not (get (val %) :hidden? false))
                                           (= (get db :selected-tab) (get (val %) :tab ""))) (get db :panels)))]
          (vec (into (get v :root) [(get v :h) (get v :w)]))))))

(defn logic-and-params
  [block-map panel-key]
  (if (try (and (ut/ne? block-map) (or (map? block-map) (vector? block-map) (keyword? block-map))) (catch :default _ false))
    (let [;;valid-body-params      (vec (filter #(and (keyword? %) (cstr/includes? (str %) "/"))))
          valid-body-params (vec (ut/get-compound-keys block-map))
          ;; _ (ut/tapp>> [:valid-body-params panel-key block-map valid-body-params])
          workspace-params (into {}
                                 (for [k valid-body-params] ;; deref here?
                                   {k @(ut/tracked-sub ::conn/clicked-parameter-key-alpha {:keypath [k]})}))
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
                                     (get-in @(ut/tracked-sub ::conn/sql-data-alpha {:keypath [ds]}) [row field]))})))
          condi-walks-targets (distinct (filter #(cstr/includes? (str %) "condi/") valid-body-params))
          condi-walks (into {}
                            (for [k condi-walks-targets]
                              {k @(ut/tracked-sub ::conn/condi-value
                                                  {:condi-key (keyword (last (ut/splitter (ut/safe-name k) "/")))})}))
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
                                                   {v (if (logic-and-params l nil) ;; l
                                                        this
                                                        that)})))]
                           (ut/postwalk-replacer logic-kps obody)))
          sticky-border-radius (fn [obody]
                                 (let [kps       (ut/extract-patterns obody :sticky-border-radius 2)
                                       sel-size  @(ut/tracked-sub ::sizes-alpha {:panel-key panel-key})
                                       sizes     @(ut/tracked-sub ::all-roots-tab-sizes-current {})
                                       sizes     (filterv #(not= sel-size %) sizes)
                                       ;;_ (ut/tapp>> [:panel-key panel-key :sizes (str sizes) :sel-size (str sel-size)])
                                       logic-kps (into {} (for [v kps]
                                                            (let [[_ px-val] v]
                                                              {v (let [sbr (ut/sticky-border-radius px-val sel-size sizes)]
                                                                   ;(ut/tapp>> [:panel-key panel-key :sizes (str sizes) :sel-size (str sel-size) :sbr (str sbr)])
                                                                   sbr)})))]
                                   (walk/postwalk-replace logic-kps obody)))
          ;; sticky-border (fn [obody]
          ;;              (let [kps       (ut/extract-patterns obody :sticky-border 2)
          ;;                    sel-size  @(ut/tracked-sub ::sizes-alpha {:panel-key panel-key})
          ;;                    sizes     @(ut/tracked-sub ::all-roots-tab-sizes-current {})
          ;;                    sizes     (filterv #(not= sel-size %) sizes)
          ;;                              ;;_ (ut/tapp>> [:panel-key panel-key :sizes (str sizes) :sel-size (str sel-size)])
          ;;                    logic-kps (into {} (for [v kps]
          ;;                                         (let [[_ px-val] v]
          ;;                                           {v (let [sbr (ut/sticky-border px-val sel-size sizes)]
          ;;                                                ;(ut/tapp>> [:panel-key panel-key :sizes (str sizes) :sel-size (str sel-size) :sbr (str sbr)])
          ;;                                                sbr)})))]
          ;;                (walk/postwalk-replace logic-kps obody)))          
          when-walk-map2 (fn [obody]
                           (let [kps       (ut/extract-patterns obody :when 3)
                                 logic-kps (into {} (for [v kps] (let [[_ l this] v] {v (when l this)})))]
                             (ut/postwalk-replacer logic-kps obody)))
          =-walk-map2 (fn [obody]
                        (let [kps       (ut/extract-patterns obody := 3)
                              logic-kps (into {} (for [v kps] (let [[_ that this] v] {v (= (str that) (str this))})))]
                          (ut/postwalk-replacer logic-kps obody)))
          string-walk (fn [num obody]
                        (let [kps       (ut/extract-patterns obody :string3 num)
                              logic-kps (into {} (for [v kps] (let [[_ & this] v] {v (apply str this)})))]
                          (ut/postwalk-replacer logic-kps obody)))
          case-walk (fn [obody]
                      (let [kps       (ut/extract-patterns obody :case 2)
                            logic-kps (into {} (for [v kps] (let [[_ l] v] {v (ut/vectorized-case l)})))]
                        (ut/postwalk-replacer logic-kps obody)))
          get-in-walk (fn [obody]
                        (let [kps       (ut/extract-patterns obody :get-in 2)
                              logic-kps (into {} (for [v kps] (let [[_ [data kp]] v] {v (get-in data kp)})))]
                          (walk/postwalk-replace logic-kps obody)))
          invert-hex-color-walk (fn [obody]
                                  (let [kps       (ut/extract-patterns obody :invert-hex-color 2)
                                        logic-kps (into {} (for [v kps] (let [[_ hhex] v] {v (ut/invert-hex-color hhex)})))]
                                    (walk/postwalk-replace logic-kps obody)))
          tetrads-walk (fn [obody]
                         (let [kps       (ut/extract-patterns obody :tetrads 2)
                               logic-kps (into {} (for [v kps] (let [[_ hhex] v] {v (ut/tetrads hhex)})))]
                           (walk/postwalk-replace logic-kps obody)))
          complements-walk (fn [obody]
                             (let [kps       (ut/extract-patterns obody :complements 2)
                                   logic-kps (into {} (for [v kps] (let [[_ hhex] v] {v (ut/complements hhex)})))]
                               (walk/postwalk-replace logic-kps obody)))
          split-complements-walk (fn [obody]
                                   (let [kps       (ut/extract-patterns obody :split-complements 2)
                                         logic-kps (into {} (for [v kps] (let [[_ hhex] v] {v (ut/split-complements hhex)})))]
                                     (walk/postwalk-replace logic-kps obody)))
          triads-walk (fn [obody]
                        (let [kps       (ut/extract-patterns obody :triads 2)
                              logic-kps (into {} (for [v kps] (let [[_ hhex] v] {v (ut/triads hhex)})))]
                          (walk/postwalk-replace logic-kps obody)))
          singles {:text   str
                   :>>     (fn [[x y]] (true? (> x y)))
                   :<<     (fn [[x y]] (true? (< x y)))
                   :str    (fn [args] (if (vector? args) (cstr/join "" (apply str args)) (str args)))
                   :string (fn [args] (if (vector? args) (cstr/join "" (apply str args)) (str args)))}
          obody-key-set (ut/deep-flatten block-map)
          client-name @(ut/tracked-sub ::client-name {})
          solver-clover-walk
          (fn [obody]
            (let [;kps       (ut/extract-patterns obody :run-solver 2)
                  kps        (ut/extract-patterns-with-keypath obody :run-solver 2)
                  logic-kps (into
                             {}
                             (for [[fkp v] kps]
                               (let [[_ & this]                v
                                     fkp                       (vec (into [:resolver panel-key] fkp))
                                     override?                 (try (map? (first this)) (catch :default _ false)) ;; not a vec input call, completely new solver map
                                     [[solver-name input-map]] (if override? [[:raw-custom-override {}]] this)
                                     unresolved-req-hash       (hash (if false ;override?
                                                                       fkp ;this 
                                                                       [solver-name fkp client-name]))
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
                                                                ;;@(ut/tracked-sub ::solver-fn-runs-keys {:keypath [panel-key sub-param]})
                                                                unique-resolved-map)
                                     lets-go?                  (and online? (not run?))
                                    ;;  _ (when lets-go?
                                    ;;      (ut/tapp>> [:run-solver-req-map-resolver! override? (str (first this)) lets-go? (not run?) req-map
                                    ;;                  @db/solver-fn-runs]))
                                     _ (when lets-go?
                                         (swap! db/kit-run-ids assoc (keyword new-solver-name) (ut/generate-uuid))
                                         (ut/tracked-dispatch [::wfx/push :default req-map])
                                         (swap! db/solver-fn-lookup assoc fkp sub-param)
                                         ;(ut/tracked-dispatch [::conn/update-solver-fn-lookup fkp sub-param])
                                         (swap! db/solver-fn-runs assoc-in [panel-key sub-param] unique-resolved-map)
                                         ;(ut/tracked-dispatch [::conn/update-solver-fn-runs [panel-key sub-param] unique-resolved-map])
                                         )
                                     _ (when (and lets-go?
                                                  (not (some #(= % :time/now-seconds) clover-kps))
                                                  (not (some #(= % :time/second) clover-kps)))
                                         (ut/dispatch-delay 100 [::http/insert-alert (ut/solver-alert-clover fkp clover-kps :resolver rtype) 11 1.7 3]))]
                                 {v sub-param})))]
              (walk/postwalk-replace logic-kps obody)))
          has-fn? (fn [k] (some #(= % k) obody-key-set))
          out-block-map (cond->> block-map
                          true                      (ut/namespaced-swapper "this-block" (ut/replacer (str panel-key) #":" ""))
                          true                      (ut/postwalk-replacer {:*this-block* panel-key})
                          (has-fn? :run-solver)     solver-clover-walk
                          (ut/ne? value-walks)      (ut/postwalk-replacer value-walks)
                          (ut/ne? condi-walks)      (ut/postwalk-replacer condi-walks)
                          (ut/ne? workspace-params) (ut/postwalk-replacer workspace-params)
                          (has-fn? :string3)        (string-walk 2) ;; TODO, remove all these
                          (has-fn? :string3)        (string-walk 3)
                          (has-fn? :string3)        (string-walk 4)
                          (has-fn? :string3)        (string-walk 5)
                          (has-fn? :string3)        (string-walk 6) ;; TODO REMOVE ALL THIS
                          (has-fn? :get-in)         get-in-walk
                          (has-fn? :=)              =-walk-map2
                          (has-fn? :sticky-border-radius) sticky-border-radius
                          ;; (has-fn? :sticky-border) sticky-border
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
                                                                 {k @(ut/tracked-sub ::conn/clicked-parameter-key-alpha
                                                                                     {:keypath [k]})})))
                                   {})
          out-block-map (if templates? (ut/deep-template-replace templated-strings-walk out-block-map) out-block-map)]
      (if (ut/ne? (vec (ut/get-compound-keys out-block-map))) (logic-and-params out-block-map panel-key) out-block-map))
    block-map))


;; (re-frame/reg-sub ::logic-and-params (fn [_ {:keys [m p]}] (logic-and-params-fn m p)))

;; (defn logic-and-params [m p] @(ut/tracked-sub ::logic-and-params {:m m :p p}))
