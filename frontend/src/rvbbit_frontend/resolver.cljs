(ns rvbbit-frontend.resolver
  (:require
   [reagent.core :as reagent]
   [re-frame.core :as re-frame]
   [re-frame.alpha :as rfa]
    ;[re-pressed.core :as rp]
   [day8.re-frame.undo :as undo :refer [undoable]]
   [clojure.edn :as edn]
   [rvbbit-frontend.utility :as ut]
   [rvbbit-frontend.connections :as conn]
   [clojure.walk :as walk]
   [clojure.string :as cstr]
   [goog.i18n.NumberFormat.Format]))

(defn logic-and-params-fn [block-map panel-key]
  (if (try
        (and (ut/ne? block-map)
             (or (map? block-map)
                 (vector? block-map)
                 (keyword? block-map)))
        (catch :default _ false))
    ;; try to save some work
    (let [valid-body-params      (vec (filter #(and (keyword? %) (cstr/includes? (str %) "/")) (ut/deep-flatten block-map)))
          workspace-params       (into {} (for [k valid-body-params] ;; deref here?
                                            {k
                                             ;;@(ut/tracked-subscribe [::conn/clicked-parameter-key [k]])
                                             @(rfa/sub ::conn/clicked-parameter-key-alpha {:keypath [k]})}))
          value-walks-targets    (filter #(and (cstr/includes? (str %) ".") (not (cstr/includes? (str %) ".*"))) valid-body-params)
          value-walks            (into {} (for [k value-walks-targets] ;all-sql-call-keys]
                                            (let [fs    (cstr/split (ut/unkeyword k) "/")
                                                  gs    (cstr/split (last fs) ".")
                                                  ds    (keyword (first fs))
                                                  row   (try (int (last gs)) (catch :default _ "label"))
                                                ;row (int (last gs))
                                                  field (keyword (first gs))]
                                              {k (if (not (integer? row))
                                                 ;(get-in @(ut/tracked-subscribe [::conn/sql-data [ds]]) [row field])
                                                   (str field)
                                                   (get-in
                                                    ;;@(ut/tracked-subscribe [::conn/sql-data [ds]])
                                                    @(rfa/sub ::conn/sql-data-alpha {:keypath [ds]})
                                                    [row field]))})))

          condi-walks-targets    (distinct (filter #(cstr/includes? (str %) "condi/") valid-body-params))
          condi-walks            (into {} (for [k condi-walks-targets]
                                            {k @(ut/tracked-subscribe [::conn/condi-value (keyword (last (cstr/split (ut/unkeyword k) "/")))])}))
          into-walk-map2           (fn [obody] (let [;obody (walk/postwalk-replace condi-walks orig-body)
                                                     kps       (ut/extract-patterns obody :into 3) ;(kv-map-fn obody) ;(into {} (for [p (ut/kvpaths obody)] {p (get-in obody p)}))
                                                     logic-kps (into {} (for [v kps]
                                                                          (let [[_ this that] v]
                                                                                ;(tap> [:if-walk panel-key kps l this that])
                                                                            {v (into this that)})))]
                                                 (walk/postwalk-replace logic-kps obody)))
          if-walk-map2           (fn [obody] (let [;obody (walk/postwalk-replace condi-walks orig-body)
                                                   kps       (ut/extract-patterns obody :if 4) ;(kv-map-fn obody) ;(into {} (for [p (ut/kvpaths obody)] {p (get-in obody p)}))
                                                   logic-kps (into {} (for [v kps]
                                                                        (let [[_ l this that] v]
                                                                        ;(tap> [:if-walk panel-key kps l this that])
                                                                          {v (if l this that)})))]
                                               (walk/postwalk-replace logic-kps obody)))
        ;; when-walk-map          (fn [obody] (let [kps       (kv-map-fn obody) ;(into {} (for [p (ut/kvpaths obody)] {p (get-in obody p)}))
        ;;                                          logic-kps (into {} (for [[_ v]
        ;;                                                                   (into {} (filter #(cstr/starts-with? (str (last %)) "[:when") kps))]
        ;;                                                               (let [[_ l this] v]
        ;;                                                                 {v (when l this)})))]
        ;;                                      (walk/postwalk-replace logic-kps obody)))
          when-walk-map2         (fn [obody] (let [kps       (ut/extract-patterns obody :when 3)
                                                   logic-kps (into {} (for [v kps]
                                                                        (let [[_ l this] v]
                                                                          {v (when l this)})))]
                                               (walk/postwalk-replace logic-kps obody)))
        ;; =-walk-map             (fn [obody] (let [kps       (kv-map-fn obody) ;(into {} (for [p (ut/kvpaths obody)] {p (get-in obody p)}))
        ;;                                          logic-kps (into {} (for [[_ v]
        ;;                                                                   (into {} (filter #(cstr/starts-with? (str (last %)) "[:=") kps))]
        ;;                                                               (let [[_ that this] v]
        ;;                                                                 ;(tap> [:=-walk panel-key kps this that])
        ;;                                                                 {v (= (str that) (str this))})))]
        ;;                                      ;(tap> [:=-walk/logic-kps logic-kps kps workspace-params])
        ;;                                      (walk/postwalk-replace logic-kps obody)))
          =-walk-map2            (fn [obody] (let [kps       (ut/extract-patterns obody := 3)
                                                   logic-kps (into {} (for [v kps]
                                                                        (let [[_ that this] v]
                                                                        ;(tap> [:=-walk panel-key kps this that])
                                                                          {v (= (str that) (str this))})))]
                                             ;(tap> [:=-walk/logic-kps logic-kps kps workspace-params])
                                               (walk/postwalk-replace logic-kps obody)))
        ;; auto-size-walk-map2            (fn [obody] (let [kps       (ut/extract-patterns obody :auto-size-px 2)
        ;;                                                  logic-kps (into {} (for [v kps]
        ;;                                                                       (let [[_ l] v]
        ;;                                                                 ;(tap> [:=-walk panel-key kps this that])
        ;;                                                                         {v (ut/auto-font-size-px l h w) ;(= (str that) (str this))
        ;;                                                                          })))]
        ;;                                      ;(tap> [:=-walk/logic-kps logic-kps kps workspace-params])
        ;;                                              (walk/postwalk-replace logic-kps obody)))
        ;; onclick-walk-map       (fn [obody] (let [kps       (kv-map-fn obody) ;(into {} (for [p (ut/kvpaths obody)] {p (get-in obody p)}))
        ;;                                          logic-kps (into {} (for [[_ v]
        ;;                                                                   (into {} (filter #(cstr/starts-with? (str (last %)) "[:set-parameter") kps))]
        ;;                                                               (let [[_ pkey pval] v
        ;;                                                                     raw-param-key (get-in vsql-calls (conj (vec (first (filter #(= (last %) :on-click)
        ;;                                                                                                                                (ut/kvpaths vsql-calls)))) 1) pkey)]
        ;;                                                                 ;(tap> [:on-click-hack panel-key v raw-param-key])
        ;;                                                                 {v (fn [] (ut/tracked-dispatch [::conn/click-parameter [panel-key] {raw-param-key pval}]))})))]
        ;;                                      ;(tap> [:set-param/logic-kps logic-kps kps])
        ;;                                      (walk/postwalk-replace logic-kps obody)))
        ;; onclick-walk-map2      (fn [obody] (let [kps       (ut/extract-patterns obody :set-parameter 3)
        ;;                                          logic-kps (into {} (for [v kps]
        ;;                                                               (let [[_ pkey pval] v
        ;;                                                                     raw-param-key (get-in vsql-calls (conj (vec (first (filter #(= (last %) :on-click)
        ;;                                                                                                                                (ut/kvpaths vsql-calls)))) 1) pkey)]
        ;;                                                                 ;(tap> [:on-click-hack panel-key v raw-param-key])
        ;;                                                                 {v (fn [] (ut/tracked-dispatch [::conn/click-parameter [panel-key] {raw-param-key pval}]))})))]
        ;;                                      ;(tap> [:set-param/logic-kps logic-kps kps])
        ;;                                      (walk/postwalk-replace logic-kps obody)))
        ;; map-walk-map2            (fn [obody] (let [kps       (ut/extract-patterns obody :map 3)
        ;;                                            logic-kps (into {} (for [v kps]
        ;;                                                                 (let [[_ that this] v]
        ;;                                                                 ;(tap> [:=-walk panel-key kps this that])
        ;;                                                                   {v (if (vector? this)
        ;;                                                                        (vec (for [r this] (last (get r that))))
        ;;                                                                        (vec (for [r @(ut/tracked-subscribe [::conn/sql-data [this]])] (last (get r that)))))
        ;;                                                                  ;(= (str that) (str this))
        ;;                                                                    })))]
        ;;                                      ;(tap> [:=-walk/logic-kps logic-kps kps workspace-params])
        ;;                                        (walk/postwalk-replace logic-kps obody)))

          ;; string-walk            (fn [num obody] (let [kps       (ut/extract-patterns obody :string num)
          ;;                                              logic-kps (into {} (for [v kps]
          ;;                                                                   (let [[_ & this] v]
          ;;                                                                     {v (apply str this)})))]
          ;;                                          (walk/postwalk-replace logic-kps obody)))

          case-walk         (fn [obody] (let [kps       (ut/extract-patterns obody :case 2)
                                              logic-kps (into {} (for [v kps]
                                                                   (let [[_ l] v]
                                                                     {v (ut/vectorized-case l)})))]
                                          (walk/postwalk-replace logic-kps obody)))

        ;; scrubber-walk-map2            (fn [obody] (let [kps       (ut/extract-patterns obody :scrubber 2)
        ;;                                                 kps2       (ut/extract-patterns obody :scrubber 3)
        ;;                                                 logic-kps (into {} (for [v kps]
        ;;                                                                      (let [[_ this] v]
        ;;                                                                        (tap> [:scrubber panel-key kps this])
        ;;                                                                        {v [scrubber-panel true
        ;;                                                                            @(ut/tracked-subscribe [::keypaths-in-params :param])
        ;;                                                                            :param
        ;;                                                                            (str (last (cstr/split (str this) #"/")))
        ;;                                                                            {:fm true :canvas? true}]})))
        ;;                                                 logic-kps2 (into {} (for [v kps2]
        ;;                                                                       (let [[_ this opts] v]
        ;;                                                                         (tap> [:scrubber panel-key kps this])
        ;;                                                                         {v [scrubber-panel true
        ;;                                                                             @(ut/tracked-subscribe [::keypaths-in-params :param])
        ;;                                                                             :param
        ;;                                                                             (str (last (cstr/split (str this) #"/")))
        ;;                                                                             {:fm true :canvas? true :opts opts}]})))]
        ;;                                             (walk/postwalk-replace (merge logic-kps2 logic-kps) obody)))
          singles               {:text                  str
                                 ;:case (fn [x] (ut/vectorized-case x))
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
                              ;                                  (let [x (for [e x] (ut/unkeyword e))] (apply str x))
                              ;                                  (catch :default _ (str x))))
                               ;:number                (fn [x] (str (nf x)))
                               ;:percent               (fn [x] (str (nf x) "%"))
          obody-key-set (ut/body-set block-map)
          has-fn? (fn [k] (some #(= % k) obody-key-set))

          out-block-map (cond->> block-map
                          true (ut/namespaced-swapper "this-block" (ut/replacer (str panel-key) #":" ""))
                          true (walk/postwalk-replace {:*this-block* panel-key})
                          (ut/ne? value-walks) (walk/postwalk-replace value-walks)
                          (ut/ne? condi-walks) (walk/postwalk-replace condi-walks)
                          (ut/ne? workspace-params) (walk/postwalk-replace workspace-params)
                          (has-fn? :=) =-walk-map2
                          (has-fn? :if) if-walk-map2
                          (has-fn? :when) when-walk-map2
                          (has-fn? :into) into-walk-map2
                          (ut/ne? singles) (walk/postwalk-replace singles)
                          (has-fn? :case) case-walk)

          templated-strings-vals (vec (filter #(cstr/includes? (str %) "/")
                                              (ut/deep-template-find out-block-map))) ;; ignore non compounds, let the server deal with it
          templates?              (ut/ne? templated-strings-vals)
          _ (when templates? (tap> [:replacing-string-templates... templated-strings-vals out-block-map]))
          templated-strings-walk (if templates?
                                   (walk/postwalk-replace {nil ""}
                                                          (into {} (for [k templated-strings-vals]
                                                                     {k
                                                                      ;;@(ut/tracked-subscribe [::conn/clicked-parameter-key [k]])
                                                                      @(rfa/sub ::conn/clicked-parameter-key-alpha {:keypath [k]})}))) {})
          out-block-map (if templates? (ut/deep-template-replace templated-strings-walk out-block-map) out-block-map)]

      ;;(tap> [:pp panel-key (ut/deep-template-find out-block-map)])
      ;;(tap> [:resolver-val-walks panel-key valid-body-params workspace-params valid-body-params out-block-map])
      (if (ut/ne? (vec (filter #(and (keyword? %) (cstr/includes? (str %) "/")) (ut/deep-flatten out-block-map))))
        (logic-and-params-fn out-block-map panel-key)
        out-block-map))
    block-map))

;; (re-frame/reg-sub
;;  ::logic-and-params
;;  (fn [_ [_ m p]]
;;    (logic-and-params-fn m p)))

(re-frame/reg-sub
 ::logic-and-params
 (fn [_ {:keys [m p]}]
   (logic-and-params-fn m p)))

(defn logic-and-params [m p]
  ;;@(ut/tracked-subscribe [::logic-and-params m p])
  @(rfa/sub ::logic-and-params {:m m :p p})
  )