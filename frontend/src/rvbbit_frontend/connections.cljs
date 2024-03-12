(ns rvbbit-frontend.connections
  (:require
   [re-frame.core :as re-frame]
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
   [cljs-time.core] ;; womp[ womp]
   [websocket-fx.core :as wfx]))

(defn logic-and-params-fn [block-map panel-key]
  (if (try
        (and (not (empty? block-map))
             (or (map? block-map)
                 (vector? block-map)
                 (keyword? block-map)))
        (catch :default _ false))
    ;; try to save some work
    (let [valid-body-params      (vec (filter #(and (keyword? %) (cstr/includes? (str %) "/")) (ut/deep-flatten block-map)))
          workspace-params       (into {} (for [k valid-body-params] ;; deref here?
                                            {k @(re-frame/subscribe [::clicked-parameter-key [k]])}))
          value-walks-targets    (filter #(and (cstr/includes? (str %) ".") (not (cstr/includes? (str %) ".*"))) valid-body-params)
          value-walks            (into {} (for [k value-walks-targets] ;all-sql-call-keys]
                                            (let [fs    (cstr/split (ut/unkeyword k) "/")
                                                  gs    (cstr/split (last fs) ".")
                                                  ds    (keyword (first fs))
                                                  row   (try (int (last gs)) (catch :default _ "label"))
                                                ;row (int (last gs))
                                                  field (keyword (first gs))]
                                              {k (if (not (integer? row))
                                                 ;(get-in @(re-frame/subscribe [::conn/sql-data [ds]]) [row field])
                                                   (str field)
                                                   (get-in @(re-frame/subscribe [::sql-data [ds]]) [row field]))})))

          condi-walks-targets    (distinct (filter #(cstr/includes? (str %) "condi/") valid-body-params))
          condi-walks            (into {} (for [k condi-walks-targets]
                                            {k @(re-frame/subscribe [::condi-value (keyword (last (cstr/split (ut/unkeyword k) "/")))])}))
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
        ;;                                                                 {v (fn [] (re-frame/dispatch [::conn/click-parameter [panel-key] {raw-param-key pval}]))})))]
        ;;                                      ;(tap> [:set-param/logic-kps logic-kps kps])
        ;;                                      (walk/postwalk-replace logic-kps obody)))
        ;; onclick-walk-map2      (fn [obody] (let [kps       (ut/extract-patterns obody :set-parameter 3)
        ;;                                          logic-kps (into {} (for [v kps]
        ;;                                                               (let [[_ pkey pval] v
        ;;                                                                     raw-param-key (get-in vsql-calls (conj (vec (first (filter #(= (last %) :on-click)
        ;;                                                                                                                                (ut/kvpaths vsql-calls)))) 1) pkey)]
        ;;                                                                 ;(tap> [:on-click-hack panel-key v raw-param-key])
        ;;                                                                 {v (fn [] (re-frame/dispatch [::conn/click-parameter [panel-key] {raw-param-key pval}]))})))]
        ;;                                      ;(tap> [:set-param/logic-kps logic-kps kps])
        ;;                                      (walk/postwalk-replace logic-kps obody)))
        ;; map-walk-map2            (fn [obody] (let [kps       (ut/extract-patterns obody :map 3)
        ;;                                            logic-kps (into {} (for [v kps]
        ;;                                                                 (let [[_ that this] v]
        ;;                                                                 ;(tap> [:=-walk panel-key kps this that])
        ;;                                                                   {v (if (vector? this)
        ;;                                                                        (vec (for [r this] (last (get r that))))
        ;;                                                                        (vec (for [r @(re-frame/subscribe [::conn/sql-data [this]])] (last (get r that)))))
        ;;                                                                  ;(= (str that) (str this))
        ;;                                                                    })))]
        ;;                                      ;(tap> [:=-walk/logic-kps logic-kps kps workspace-params])
        ;;                                        (walk/postwalk-replace logic-kps obody)))

          string-walk            (fn [num obody] (let [kps       (ut/extract-patterns obody :string num)
                                                       logic-kps (into {} (for [v kps]
                                                                            (let [[_ & this] v]
                                                                              {v (apply str this)})))]
                                                   (walk/postwalk-replace logic-kps obody)))

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
        ;;                                                                            @(re-frame/subscribe [::keypaths-in-params :param])
        ;;                                                                            :param
        ;;                                                                            (str (last (cstr/split (str this) #"/")))
        ;;                                                                            {:fm true :canvas? true}]})))
        ;;                                                 logic-kps2 (into {} (for [v kps2]
        ;;                                                                       (let [[_ this opts] v]
        ;;                                                                         (tap> [:scrubber panel-key kps this])
        ;;                                                                         {v [scrubber-panel true
        ;;                                                                             @(re-frame/subscribe [::keypaths-in-params :param])
        ;;                                                                             :param
        ;;                                                                             (str (last (cstr/split (str this) #"/")))
        ;;                                                                             {:fm true :canvas? true :opts opts}]})))]
        ;;                                             (walk/postwalk-replace (merge logic-kps2 logic-kps) obody)))
          singles               {:text                  str
                                 ;:case (fn [x] (ut/vectorized-case x))
                                 :str (fn [args]
                                        ;  (cstr/join "" args)
                                        ; (do (tap> [:str args])
                                        (apply str args);)
                                        )}
                               ;:string str
                              ; :strings               (fn [_ x] (apply str x))
                              ; :string                (fn [x] (try
                              ;                                  (let [x (for [e x] (ut/unkeyword e))] (apply str x))
                              ;                                  (catch :default _ (str x))))
                               ;:number                (fn [x] (str (nf x)))
                               ;:percent               (fn [x] (str (nf x) "%"))

          out-block-map (->> block-map
                             (ut/namespaced-swapper "this-block" (ut/replacer (str panel-key) #":" ""))
                             (walk/postwalk-replace {:*this-block* panel-key})
                             (walk/postwalk-replace value-walks)
                             (walk/postwalk-replace condi-walks)
                             (walk/postwalk-replace workspace-params)
                             ;(string-walk 1)
                             (string-walk 2)
                             (string-walk 3)
                             (string-walk 4)
                             (string-walk 5)
                             (string-walk 6)
                           ;map-walk-map2
                             =-walk-map2
                             if-walk-map2
                             when-walk-map2
                             into-walk-map2
                             (walk/postwalk-replace singles)
                             case-walk)

          templated-strings-vals (vec (filter #(cstr/includes? (str %) "/")
                                              (ut/deep-template-find out-block-map))) ;; ignore non compounds, let the server deal with it
          templates?              (not (empty? templated-strings-vals))
          _ (when templates? (tap> [:replacing-string-templates... templated-strings-vals out-block-map]))
          templated-strings-walk (if templates?
                                   (walk/postwalk-replace {nil ""}
                                                          (into {} (for [k templated-strings-vals]
                                                                     {k @(re-frame/subscribe [::clicked-parameter-key [k]])}))) {})
          out-block-map (if templates? (ut/deep-template-replace templated-strings-walk out-block-map) out-block-map)]

      ;;(tap> [:pp panel-key (ut/deep-template-find out-block-map)])
      ;;(tap> [:resolver-val-walks panel-key valid-body-params workspace-params valid-body-params out-block-map])
      (if (not (empty? (vec (filter #(and (keyword? %) (cstr/includes? (str %) "/")) (ut/deep-flatten out-block-map)))))
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
   ;(tap> [db-key result])
   (assoc db :selected-connection connection_id)))

(re-frame/reg-sub
 ::selected-field
 (fn [db]
   (get-in db [:selected-field])))

(re-frame/reg-event-db
 ::select-field
 (fn [db [_ key_hash]]
   ;(tap> [db-key result])
   (assoc db :selected-field key_hash)))

(re-frame/reg-event-db
 ::set-reco-status
 (fn [db [_ query-key d]]
   ;(tap> [db-key result])
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
   ;(tap> [db-key result])
   (assoc db :selected-table context_hash)))

(re-frame/reg-sub
 ::selected-shape
 (fn [db]
   (get-in db [:selected-shape])))

(re-frame/reg-event-db
 ::select-shape
 (fn [db [_ context_hash]]
   ;(tap> [db-key result])
   (assoc db :selected-shape context_hash)))

(defn refresh []
  (re-frame/dispatch [::wfx/request :default
                      {:message    {:kind :honey-call
                                    :ui-keypath [:connections]
                                    :honey-sql {:select-distinct [:connection_id :database_name :database_version]
                                                :from [:connections]}
                                    :extras {:requested? true
                                             :rando (rand-int 1234234525)}}
                       :on-response [::http/socket-response]
                       :on-timeout [::http/timeout-response]
                       :timeout    500000}]))

(re-frame/reg-sub
 ::field-count
 (fn [db]
   (get-in db [:field-count])))

(defn field-count []
  (re-frame/dispatch [::wfx/request :default
                      {:message    {:kind :honey-call
                                    :ui-keypath [:field-count]
                                    :honey-sql {:select [[[:count 1] :cnt]]
                                                :from [:fields]
                                                :where [:and
                                                        [:= :context_hash @(re-frame/subscribe [::selected-table])]
                                                        [:= :connection_id @(re-frame/subscribe [::selected])]]}
                                    :extras {:requested? true
                                             :rando (rand-int 1234234525)}}
                       :on-response [::http/socket-response]
                       :on-timeout [::http/timeout-response]
                       :timeout    50000}]))

(re-frame/reg-sub ;; RESOLVE COMPOUND KEY 
 ::resolve-click-param
 (fn [db [_ long-keyword]]
   (let [get-it (fn [kw]
                  (let [slp (vec (map keyword ;; memoize this in ut?
                                      (-> kw
                                          str
                                          (cstr/replace ":" "")
                                          (cstr/split #"/")
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
    ; (when (cstr/includes? (str keypath) "theme") (tap> [:clicked-param-fetch keypath val]))
     val)))

(re-frame/reg-sub ;; RESOLVE KEYPATH
 ::clicked-parameter-key  ;;; important, common, and likely more expensive than need be. TODO
 (fn [db [_ keypath]]
   (let [cmp (cstr/split (ut/unkeyword (nth keypath 0)) "/")
         kkey (keyword (cstr/replace (nth cmp 0) "-parameter" ""))
         vkey (keyword (peek cmp))
         val0 (get-in db (cons :click-param [kkey vkey]))
         if-walk-map2           (fn [obody] (let [kps       (ut/extract-patterns obody :if 4)
                                                  logic-kps (into {} (for [v kps]
                                                                       (let [kws (vec (filter #(cstr/includes? (str %) "/") (ut/deep-flatten v)))
                                                                             wm @(re-frame/subscribe [::resolve-click-param kws])
                                                                             v0 (walk/postwalk-replace wm v)
                                                                             [_ l this that] v0]
                                                                        ;(tap> [:if-walk keypath v kps l this that kws (if l this that)])
                                                                         {v (if l this that)})))]
                                              (walk/postwalk-replace logic-kps obody)))
         ;;val0 (resolver/logic-and-params val0 nil)
         val0 (try (if (= (first val0) :if) (if-walk-map2 val0) val0) (catch :default _ val0)) ;; TODO
        ;;  val0 (walk/postwalk-replace {:text                  str
        ;;                               :case (fn [x] (ut/vectorized-case x))
        ;;                               :str (fn [args]
        ;;                                 ;  (cstr/join "" args)
        ;;                                 ; (do (tap> [:str args])
        ;;                                      (apply str args);)
        ;;                                      )} val0)
         ns-kw? (and (cstr/starts-with? (str val0) ":") (cstr/includes? (str val0) "/") (keyword? val0))
         ;val (if (cstr/starts-with? ":theme/" (str val))
         ;      
         ;      )
         val (cond ns-kw?
                   (get-in db (let [sp (cstr/split (str val0) "/")]
                                [:click-param (ut/unre-qword (cstr/replace (str (first sp)) ":" ""))
                                 (ut/unre-qword (last sp))]))
                   (map? val0)
                   (let [km (into {} (distinct (map #(when (>= (count %) 2) {(str (cstr/replace (str (get % 0)) ":" "") "/"
                                                                                  (cstr/replace (str (get % 1)) ":" "")) [(get % 0) (get % 1)]})
                                                    (ut/keypaths (get db :click-param)))))
                         rep-map (into {} (for [[k v] km] {(keyword k) (get-in db (cons :click-param v))}))]
                     ;(tap> [:replace-map val0 rep-map])
                     (walk/postwalk-replace rep-map val0))
                   :else val0)
         ;;val (walk/postwalk-replace {:case (fn [x] (ut/vectorized-case x))} val)
        ;;  _ (tap> [:val keypath val])


         ;;; recursize resolve parameter IF WHEN ETC here! TODO - need to recur see if-walk, else will be true due to keywords
         val      (if (and (string? val) (cstr/starts-with? (str val) ":") (not (cstr/includes? (str val) " ")))
       ;(cstr/replace (str val) ":" "")
                    (edn/read-string val) ;; temp hacky param work around (since we cant save keywords in the DB).. TODO :/
                    val)]
     ;;(tap> [:rec-param (ut/deep-template-find val0)])
    ; (when (cstr/includes? (str val) "theme") 
    ;   (tap> [:clicked-param-key? keypath val (if-walk-map2 val) val0]))
    ; (when ns-kw?
    ;   (tap> [:click-param ns-kw? val val0 (when ns-kw? (let [sp (cstr/split (str val0) "/")]
    ;                                                      [:click-param (ut/unre-qword (cstr/replace (str (first sp)) ":" ""))
    ;                                                       (ut/unre-qword (last sp))])) val [kkey vkey]]))
     ;(if (nil? val) (str "(nil val)") val)

     ;(resolver/logic-and-params val nil)
     ;(logic-and-params-fn val nil)
     val)))

(re-frame/reg-event-db
 ::click-parameter
 (fn [db [_ keypath value]]
   ;(tap> [:click-parameter keypath value])
   (let [cc (get-in db (cons :click-param keypath))]
     (assoc-in db (cons :click-param keypath)
               (if (and (not (= (first keypath) :param)) (= cc value)) ;; unset to nil if same value UNLESS is a user param... (breaks open-input UI)
                 nil value)))))

(re-frame/reg-event-db
 ::declick-parameter
 (undoable)
 (fn [db [_ keypath]]
   (tap> [:declick (cons :click-param keypath)])
   (ut/dissoc-in db (vec (cons :click-param keypath)))))

(re-frame/reg-event-db
 ::cell-click-parameter
 (fn [db [_ keypath value]]
   (tap> [:cell-click-parameter keypath value])

   (let [cc (get-in db (cons :click-param keypath))
         new (vec (distinct (cond (vector? cc)
                                  (vec (conj cc value))
                                  (nil? cc) [value]
                                  :else (vec (conj [cc] value)))))
         new-vec (vec (flatten (cond ;(= cc value) nil ;; drop it?
                                 (try (some #(= % value) cc) (catch :default _ false)) (remove #(= % value) cc)
                                 :else new)))]
     (tap> [:cell-click-parameter-existing-val (cons :click-param keypath) value cc new new-vec (empty? new-vec)])
     (if (empty? new-vec)
       (ut/dissoc-in db (vec (cons :click-param keypath))) ;; empty list, remove param
       ;(assoc-in db (cons :click-param keypath) nil)
       (assoc-in db (cons :click-param keypath) new-vec))))) ;; otherwise commit new list

(re-frame/reg-sub
 ::sql-data
 (fn [db [_ keypath]]
   (get-in db (cons :data keypath))))

(re-frame/reg-sub
 ::sql-metadata
 (fn [db [_ keypath]]
   (get-in db (cons :meta keypath))))

(re-frame/reg-sub
 ::sql-post-metadata
 (fn [db [_ keypath]]
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
                     {:on-click #(re-frame/dispatch [::click-parameter keypath r])}
                     {})
             :child (str r)
             :style (merge styles {:cursor (if clickable? "pointer" "inherit")
                                   :background-color (if (= r @(re-frame/subscribe [::clicked-parameter keypath]))
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
                     {;:on-click #(re-frame/dispatch [::click-parameter keypath r])
                      :on-click (re-com/handler-fn (re-frame/dispatch [::click-parameter keypath r]))}
                     {})
             :child (str (vals r))
             :style (merge styles {:cursor (if clickable? "pointer" "inherit")
                                   :background-color (if (= r @(re-frame/subscribe [::clicked-parameter keypath]))
                                                       "grey"
                                                       "inherit")})])))))

(re-frame/reg-sub
 ::sql-query-not-run?
 (fn [db [_ keypath query]] ;; LOGIC HERE NEEDS TO BE THE SAME AS conn/sql-data ore shit gets weird
   (let [;query (dissoc query :col-widths)
         style-rules (get query :style-rules) ;; rules code dupe from conn/sql-data
         has-rules? (and (not (nil? style-rules)) (not (empty? style-rules)))
         rules (when has-rules?
                 (vec (for [[[col name] logic] style-rules]
                        [[:case (:logic logic) 1 :else 0]
                         (keyword (str "styler_" (ut/unkeyword name)))])))
        ;;  query (-> query
        ;;            (dissoc :col-widths)
        ;;            (dissoc :row-height)
        ;;            (dissoc :render-all?)
        ;;            (dissoc :cache?)
        ;;            (dissoc :refresh-every)
        ;;            (dissoc :deep-meta?)
        ;;            (dissoc :clicked-row-height)
        ;;            (dissoc :style-rules))
         query (ut/clean-sql-from-ui-keys query)
         hselect (get query :select)
         query (if has-rules?
                 (assoc query :select (apply merge hselect rules))
                 query)
         not-col-sel? (not (= (first keypath) (get-in db [:selected-cols 1])))]
     (and not-col-sel?
          (not (= (hash query) (get-in db (cons :query-history keypath))))))))

(re-frame/reg-event-db
 ::add-to-sql-history ;; :ran tap is good
 (fn [db [_ keypath query]]
   ;(tap> [:ran keypath query])
   (let [query (dissoc query :col-widths)
         base-sniff? (true? (= (get query :limit) 111))]
     (if base-sniff?
       (-> db
           (assoc-in (cons :base-sniff-queries keypath) (hash query))
           (assoc-in (cons :query-history keypath) (hash query)))
       (assoc-in db (cons :query-history keypath) (hash query))))))

(re-frame/reg-event-db
 ::add-to-sql-history-meta ;; :ran tap is good
 (fn [db [_ keypath query]]
   ;(tap> [:ran-meta keypath query])
   (let [query (dissoc query :col-widths)]
     (assoc-in db (cons :query-history-meta keypath) (hash query)))))

(re-frame/reg-sub
 ::lookup-panel-key-by-query-key
 (fn [db [_ query-key]]
   (first (remove nil? (for [[k v] (get db :panels)]
                         (when (some #(= query-key %)
                                     (keys (get v :queries))) k))))))

(re-frame/reg-event-db
 ::clear-query-history
 (fn [db [_ query-id]]
   ;(tap> [:cleared-query-history query-id])
   (let [panel @(re-frame/subscribe [::lookup-panel-key-by-query-key query-id])
         selected (get db :selected-block)]
     (-> db
       ;(ut/dissoc-in [:data query-id])
         (assoc-in (if (not (= panel selected))
                     [:panels panel :queries query-id :_last-run]
                     [:last-run-throwaway])
                   (ut/get-time-format-str)) ;; this is weird. was not needed before, but code-tables were not refreshing w/o a render kick... 1/12/24
         (ut/dissoc-in [:query-history query-id])
         (ut/dissoc-in [:query-history-meta query-id])))))

(re-frame/reg-sub
 ::sql-meta-not-run?
 (fn [db [_ keypath query]]
   (let [query (dissoc query :col-widths)
         not-col-sel? (not (= (first keypath) (get-in db [:selected-cols 1])))]
     (and not-col-sel?
          (not (= (hash query) (get-in db (cons :query-history-meta keypath))))))))

(re-frame/reg-sub
 ::sql-style-not-run?
 (fn [db [_ keypath query pquery]]
   (let [query (dissoc query :col-widths)
         not-col-sel? (not (= (first keypath) (get-in db [:selected-cols 1])))]
     (and not-col-sel?
          (not (= (hash (str pquery query)) (get-in db (cons :query-history-style keypath))))))))

(re-frame/reg-sub
 ::sql-tab-not-run?
 (fn [db [_ keypath query pquery]]
   (let [query (dissoc query :col-widths)
         not-col-sel? (not (= (first keypath) (get-in db [:selected-cols 1])))]
     (and not-col-sel?
          (not (= (hash (str pquery query)) (get-in db (cons :query-history-tab keypath))))))))

(re-frame/reg-event-db
 ::add-to-sql-history-tab
 (fn [db [_ keypath query pquery]]
   ;(tap> [:ran-tab keypath query])
   (let [query (dissoc query :col-widths)]
     (assoc-in db (cons :query-history-tab keypath) (hash (str pquery query))))))

(re-frame/reg-event-db
 ::add-to-sql-history-style ;; :ran tap is good
 (fn [db [_ keypath query pquery]]
   (tap> [:ran-style keypath query])
   (let [query (dissoc query :col-widths)]
     (assoc-in db (cons :query-history-style keypath) (hash (str pquery query))))))

(re-frame/reg-sub
 ::sql-condi-not-run?
 (fn [db [_ keypath query pquery]]
   (let [query (dissoc query :col-widths)
         not-col-sel? (not (= (first keypath) (get-in db [:selected-cols 1])))]
     (and not-col-sel?
          (not (= (hash (str pquery query)) (get-in db (cons :query-history-condi keypath))))))))

(re-frame/reg-event-db
 ::add-to-sql-history-condi
 (fn [db [_ keypath query pquery]]
   ;(tap> [:ran-condi keypath query])
   (let [query (dissoc query :col-widths)]
     (assoc-in db (cons :query-history-condi keypath) (hash (str pquery query))))))

(re-frame/reg-sub
 ::condi-value
 (fn [db [_ condi-key]]
   (true? (= 1 (get-in db [:post-condi condi-key 0 :v])))))

(re-frame/reg-sub
 ::client-name
 (fn [db]
   (get db :client-name)))

(defn theme-pull [cmp-key fallback & test-fn]
  (let [v                   @(re-frame/subscribe [::clicked-parameter-key [cmp-key]])
        t0                  (cstr/split (str (ut/unkeyword cmp-key)) #"/")
        t1                  (keyword (first t0))
        t2                  (keyword (last t0))
        self-ref-keys       (distinct (filter #(and (keyword? %) (namespace %)) (ut/deep-flatten db/base-theme)))
        self-ref-pairs      (into {}
                                  (for [k self-ref-keys     ;; todo add a reurziver version of this
                                        :let [bk (keyword (ut/replacer (str k) ":theme/" ""))]]
                                    {k (get db/base-theme bk)}))
        resolved-base-theme (walk/postwalk-replace self-ref-pairs db/base-theme)
        base-theme-keys     (keys resolved-base-theme)
        theme-key?          (true? (and (= t1 :theme) (some #(= % t2) base-theme-keys)))
        fallback0           (if theme-key? (get resolved-base-theme t2) fallback)]
    ;(tap> [:theme-test theme-key? resolved-base-theme])
    ;(tap> [:color-pull cmp-key t1 t2 fallback fallback0 theme-key? (get db/base-theme t2) base-theme-keys])
    (if (not (nil? v)) v fallback0)))

;; (def data-colors (theme-pull :theme/data-colors db/data-colors)) 

(re-frame/reg-sub
 ::data-colors
 (fn [_]
   (theme-pull :theme/data-colors db/data-colors)))

(def data-colors @(re-frame/subscribe [::data-colors])) ;; kinda weird usage


(defn sql-deep-meta [keypath honey-sql connection-id & [deeps?]]
  ;(tap> [:sql-deep-meta keypath (str (first keypath)) (cstr/starts-with? (str (first keypath)) ":query-preview" )])
  (when (not (cstr/starts-with? (str (first keypath)) ":query-preview")) ;; lets not sniff rando recos, ok?
    (let [fields (get @(re-frame/subscribe [::sql-metadata keypath]) :fields [])
          connection-id (if (nil? connection-id) "cache.db" connection-id)
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
        ;(or @(re-frame/subscribe [::sql-query-not-run? keypath honey-sql])
         (and (not (= keypath [:query-preview])) ;; no need to meta on browsing viz recos 
                ; (= (count @(re-frame/subscribe [::wfx/pending-requests http/socket-id])) 0)
                ;; false
              @(re-frame/subscribe [::sql-meta-not-run?
                                   ;keypath 
                                    (conj (conj keypath f) name)
                                    hsql]))

          (dorun
           (re-frame/dispatch [::wfx/request :default
                               {:message    {:kind :honey-xcall
                                             :ui-keypath (conj (conj keypath f) name)
                                             :honey-sql hsql
                                             :connection-id connection-id
                                             :client-name @(re-frame/subscribe [::client-name])}
                                :on-response [::http/socket-response-post-meta]
                                :on-timeout [::http/timeout-response (conj (conj keypath f) name)] ;; requeue?
                                :timeout    50000}])
           (re-frame/dispatch [::add-to-sql-history-meta
                               (conj (conj keypath f) name)
                            ; keypath
                               hsql])))))));)

(re-frame/reg-event-db
 ::run-sql-deep-meta-for
 (fn [db [_ panel-key query-key honey-sql]]
   (let [connection-id (get-in db [:panels panel-key :connection-id])]
     (tap> [:manual-deep-meta query-key :on connection-id])
     (sql-deep-meta [query-key] honey-sql connection-id true)
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
          @(re-frame/subscribe [::sql-style-not-run? kp hsql honey-sql])
          (not @(re-frame/subscribe [::sql-query-not-run? keypath honey-sql])))

          (dorun
           (re-frame/dispatch [::wfx/request :default
                               {:message    {:kind :honey-xcall
                                             :ui-keypath kp
                                             :honey-sql hsql
                                             :connection-id :cache ;connection-id
                                             :client-name @(re-frame/subscribe [::client-name])}
                                :on-response [::http/socket-response-post-style style]
                                :on-timeout [::http/timeout-response [keypath honey-sql]]
                                :timeout    50000}])
           (re-frame/dispatch [::add-to-sql-history-style kp hsql honey-sql])))))))

(defn sql-tab-meta [keypath rules panel-key]
  (let [;style-rules (get honey-sql :style-rules) 
        kk (str rules panel-key)]
    ;(tap> [keypath rules panel-key])
    (doseq
     [[[f name] logic] [rules]]
      (let [kp ;(conj 
            (conj (conj keypath f) name)
               ; :tabs)
            hsql {:select [[[:case logic 1 :else 0] :v]]
                  ;:from [(ut/keypath-munger keypath)] 
                  }]
        ;(tap> [kp hsql])
        (when ;true
         ;(and
         @(re-frame/subscribe [::sql-tab-not-run? kp hsql kk])
          ;(not @(re-frame/subscribe [::sql-query-not-run? keypath rules]))
         ; )

          (dorun
           (re-frame/dispatch [::wfx/request :default
                               {:message    {:kind :honey-xcall
                                             :ui-keypath kp
                                             :honey-sql hsql
                                             :connection-id :cache ;connection-id
                                             :client-name @(re-frame/subscribe [::client-name])}
                                :on-response [::http/socket-response-post-tab panel-key]
                                :on-timeout [::http/timeout-response [keypath rules]]
                                :timeout    50000}])
           (re-frame/dispatch [::add-to-sql-history-tab kp hsql kk])))))))

(defn sql-condi-meta [keypath rules]
  (let [;style-rules (get honey-sql :style-rules) 
        kk (str rules)]
    ;(tap> [keypath rules ])
    (doseq
     [logic [rules]]
      (let [kp ;(conj 
            keypath ;(conj keypath name)
               ; :tabs)
            hsql {:select [[[:case logic 1 :else 0] :v]]
                  ;:from [(ut/keypath-munger keypath)] 
                  }]
        ;(tap> [kp hsql])
        (when ;true
         ;(and
         @(re-frame/subscribe [::sql-condi-not-run? kp hsql kk])
          ;(not @(re-frame/subscribe [::sql-query-not-run? keypath rules]))
         ; )

          (dorun
           (re-frame/dispatch [::wfx/request :default
                               {:message    {:kind :honey-xcall
                                             :ui-keypath kp
                                             :honey-sql hsql
                                             :connection-id :cache ;connection-id
                                             :client-name @(re-frame/subscribe [::client-name])}
                                :on-response [::http/socket-response-post-condi]
                                :on-timeout [::http/timeout-response [keypath rules]]
                                :timeout    50000}])
           (re-frame/dispatch [::add-to-sql-history-condi kp hsql kk])))))))

(defn push-panels-to-server [panels-map client-name]
  (let []
    (dorun
     (re-frame/dispatch [::wfx/request   :default
                         {:message    {:kind :current-panels
                                       :panels panels-map
                                       ;:ui-keypath kp
                                       ;:honey-sql hsql
                                       ;:connection-id :cache ;connection-id
                                       :client-name client-name} ;;@(re-frame/subscribe [::client-name])}
                          ;:on-response [::http/socket-response-post-condi]
                          ;:on-timeout [::http/timeout-response [keypath rules]]
                          ;:timeout    150000
                          }]))))

(re-frame/reg-event-db
 ::set-query-schedule
 (fn [db [_ query-id schedule]]
   ;(tap> [:set-query-schedule query-id schedule])
   (let [timer (+ (get-in db [:re-pollsive.core/polling :counter]) schedule)]
     (assoc-in db [:sched query-id] timer))))

(re-frame/reg-sub ;;; dupe from bricks!
 ::lookup-panel-key-by-query-key
 (fn [db [_ query-key]]
   (first (remove nil? (for [[k v] (get db :panels)]
                         (when (some #(= query-key %)
                                     (keys (get v :queries))) k))))))

(defn sql-data
  ([keypath honey-sql]
   (let [style-rules (get honey-sql :style-rules)
          ;sniff? (true? (ut/not-empty? (get @db/sniff-deck (first keypath))))
         sniff? (= (get @db/sniff-deck (first keypath)) :reco)
         kit-name (when (and (not sniff?)
                             (keyword? (get @db/sniff-deck (first keypath))))
                    (get @db/sniff-deck (first keypath)))
         has-rules? (and (not (nil? style-rules)) (not (empty? style-rules)))
         rules (when has-rules?
                 (vec (for [[[col name] logic] style-rules]
                        [[:case (:logic logic) 1 :else 0]
                         (keyword (str "styler_" (ut/unkeyword name)))])))
         panel-key @(re-frame/subscribe [::lookup-panel-key-by-query-key (first keypath)])
         honey-sql (ut/clean-sql-from-ui-keys honey-sql)
         hselect (get honey-sql :select)
         flat (ut/deep-flatten honey-sql)
         connection-id nil ;(get honey-sql :connection-id)
         literal-data? (and (some #(= % :data) flat)
                            (not (some #(= % :panel_history) flat)))
         honey-modded (if has-rules?
                        (assoc honey-sql :select (apply merge hselect rules))
                        honey-sql)]
     (re-frame/dispatch [::wfx/request :default
                         {:message    {:kind (if (or connection-id literal-data?)
                                               :honey-xcall
                                               :honey-call) ;; override for data literals
                                       :ui-keypath keypath
                                       :panel-key panel-key
                                       :kit-name kit-name
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
                                       :client-name @(re-frame/subscribe [::client-name])}
                          :on-response [::http/socket-response]
                          :on-timeout [::http/timeout-response [keypath honey-sql]]
                          :timeout    50000}]))

   (when (not (nil? (get honey-sql :refresh-every)))
     (re-frame/dispatch [::set-query-schedule (first keypath) (get honey-sql :refresh-every)]))

  ;;  (when (and (some #(= % :data) (ut/deep-flatten honey-sql)) 
  ;;             (not (= 111 (get honey-sql :limit)))) ;; <- code for a base table sniff 
  ;;    (re-frame/dispatch-sync [::set-reco-status (first keypath) :started]))
   (re-frame/dispatch [::add-to-sql-history keypath honey-sql]))

  ([keypath honey-sql connection-id]
   (doall
    ;(tap> [:hey? (first keypath)])
    (let [style-rules (get honey-sql :style-rules)
          ;sniff? (true? (ut/not-empty? (get @db/sniff-deck (first keypath))))
          sniff? (= (get @db/sniff-deck (first keypath)) :reco)
          kit-name (when (and (not sniff?)
                              (keyword? (get @db/sniff-deck (first keypath))))
                     (get @db/sniff-deck (first keypath)))
          has-rules? (and (not (nil? style-rules)) (not (empty? style-rules)))
          rules (when has-rules?
                  (vec (for [[[col name] logic] style-rules]
                         [[:case (:logic logic) 1 :else 0]
                          (keyword (str "styler_" (ut/unkeyword name)))])))
          connection-id (cond (get honey-sql :connection-id) (get honey-sql :connection-id)
                              (nil? connection-id) "cache.db"
                              (= connection-id "system") "system-db"
                              :else connection-id)
          refresh-every (get honey-sql :refresh-every)
          cache? (get honey-sql :cache? true)
          page (get honey-sql :page)
          panel-key @(re-frame/subscribe [::lookup-panel-key-by-query-key (first keypath)])
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
                         honey-sql)]

      (do (when (not (nil? refresh-every)) (re-frame/dispatch [::set-query-schedule (first keypath) refresh-every]))

          (re-frame/dispatch [::wfx/request :default
                              {:message    {:kind :honey-xcall
                                            :ui-keypath keypath
                                            :panel-key panel-key
                                            :kit-name kit-name
                                            :page page
                                            :client-cache? cache?
                                            :sniff? sniff?
                                            :honey-sql honey-modded
                                            :connection-id connection-id
                                            :client-name @(re-frame/subscribe [::client-name])}
                               :on-response [::http/socket-response]
                               :on-timeout [::http/timeout-response [keypath honey-sql]]
                               :timeout    50000}])

          (when (or kit-name  ;; clear on deck atom 
                    sniff?) (swap! db/sniff-deck dissoc (first keypath)))
          ;; clear out the sniff deck (even if not needed)

    ;;  (when (not (= 111 (get honey-sql :limit)))
    ;;    (re-frame/dispatch-sync [::set-reco-status (first keypath) :started]))
          (re-frame/dispatch [::add-to-sql-history keypath honey-modded]))))
   ;(re-frame/dispatch-sync [::add-to-sql-history keypath honey-sql])
   ;(sql-deep-meta keypath honey-sql connection-id)
   ))



;;;  (assoc-in db [:post-meta ui-keypath field name] new-map)





;; (re-frame/reg-sub
;;  ::xsql-data
;;  (fn [db [_ keypath]]
;;    (get-in db (cons :xsql keypath))))

;; (defn xsql-data [keypath honey-sql connection-id]
;;   (re-frame/dispatch [::wfx/request :default
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


