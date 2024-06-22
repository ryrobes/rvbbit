(ns rvbbit-frontend.views
  (:require
    [cljs-drag-n-drop.core   :as dnd2]
    [cljs.tools.reader       :refer [read-string]]
    [clojure.data            :as cdata]
    [clojure.edn             :as edn]
    [clojure.string          :as cstr]
    [clojure.walk            :as walk]
    [goog.events             :as gevents]
    ;[re-catch.core           :as rc]
    [re-com.core             :as    re-com
                             :refer [at]]
    [re-com.util             :refer [px]]
    [re-frame.alpha          :as rfa]
    [re-frame.core           :as re-frame]
    [reagent.core            :as reagent]
    [rvbbit-frontend.audio   :as audio]
    [rvbbit-frontend.bricks  :as    bricks
                             :refer [theme-pull]]
    [rvbbit-frontend.buffy   :as buffy]
    [rvbbit-frontend.connections :as conn]
    [rvbbit-frontend.db      :as db]
    [rvbbit-frontend.flows   :as flows]
    [rvbbit-frontend.http    :as http]
    [rvbbit-frontend.shapes  :as shape]
    [rvbbit-frontend.resolver    :as resolver]
    [rvbbit-frontend.subs    :as subs]
    [rvbbit-frontend.utility :as ut]
    [talltale.core           :as tales]
    [websocket-fx.core       :as wfx])
  (:import
    [goog.events EventType]
    [goog.async  Debouncer]))






(defn debounce [f interval] (let [dbnc (Debouncer. f interval)] (fn [& args] (.apply (.-fire dbnc) dbnc (to-array args)))))

(defonce detached-coords
  (reagent/atom (let [hh          (.-innerHeight js/window) ;; starting size set on load
                      bricks-high (+ (js/Math.floor (/ hh bricks/brick-size)) 1)
                      topper      (* (- bricks-high 12) bricks/brick-size) ;; size of editor
                      lefty       (* 2 bricks/brick-size) ;; always 2 brick from the left...
                     ]
                  [lefty topper])))

(defn mouse-move-handler
  [offset]
  (fn [evt]
    (let [start-x (.-clientX evt)
          start-y (.-clientY evt)
          off-x   (:x offset)
          off-y   (:y offset)
          x       (- start-x off-x) ;
          y       (- start-y off-y)]
      (reset! detached-coords [x y]))))

(defn mouse-up-handler
  [on-move]
  (fn me [evt] (reset! bricks/dragging-editor? false) (do (gevents/unlisten js/window EventType.MOUSEMOVE on-move))))

(defn mouse-down-handler
  [e]
  (let [{:keys [left top]} (bricks/get-client-rect e)
        offset             {:x (- (.-clientX e) left) :y (- (.-clientY e) top)}
        on-move            (mouse-move-handler offset)]
    (reset! bricks/dragging-editor? true)
    (do (gevents/listen js/window EventType.MOUSEMOVE on-move))
    (gevents/listen js/window EventType.MOUSEUP (mouse-up-handler on-move))))

(defonce view-browser-query (reagent/atom {}))

(defonce data-browser-mode (reagent/atom :data))
(defonce data-browser-system-mode (reagent/atom :data))

(defn editor-panel-metadata
  []
  (let [sql-calls  {:connections-sys {:select [;:database_name
                                               [[:case [:like :connection_str "%csv%"] "*csv-import-db*"
                                                 [:like :connection_str "%cache%"] "*cache-db*" :else :database_name]
                                                :database_name] :connection_id]
                                      :from   [:connections]}
                    :tables-sys      {:select   [:db_schema :db_catalog :connection_id
                                                 [[:|| :db_catalog "/" :db_schema] :schema_cat] :table_name [[:count 1] :fields]]
                                      :from     [:fields]
                                      :where    [:= :connection_id :connections-sys/connection_id]
                                      :group-by [:db_schema :db_catalog :connection_id :table_name]
                                      :order-by [:schema_cat :table_name]}}
        sql-params (into {}
                         (for [k [:connections-sys/connection_id]]
                           {k ;;@(ut/tracked-subscribe [::conn/clicked-parameter-key [k]])
                              @(ut/tracked-sub ::conn/clicked-parameter-key-alpha {:keypath [k]})}))]
    (dorun (for [[k v] sql-calls]
             (let [query        (ut/postwalk-replacer sql-params v)
                   data-exists? @(ut/tracked-subscribe [::conn/sql-data-exists? [k]])
                   unrun-sql?   @(ut/tracked-subscribe [::conn/sql-query-not-run? [k] query])]
               (when (or (not data-exists?) unrun-sql?) (conn/sql-data [k] query)))))
    [re-com/h-box :size "auto" :style {:color (str (theme-pull :theme/editor-font-color nil) 35)} ;; rows label under
     :children
     [[re-com/box :size "auto" :child [bricks/magic-table :system-connections-list* [:connections-sys] 3 10 [:database_name]]]
      [re-com/box :size "auto" :child
       [bricks/magic-table :system-tables-list* [:tables-sys] 9 10 [:db_schema :connection_id :db_catalog]]]]]))

(defn search-panel-metadata
  []
  (let [client-name (ut/replacer (str @(ut/tracked-subscribe [::bricks/client-name])) ":" "")
        sql-calls   {:searches-types-sys     {:select   [:item_type] ;; :searches-types-sys/item_type
                                              :where    [:and [:not [:like :item_sub_type "%preview%"]]
                                                         [:not [:= :item_type "saved-block"]]]
                                              :from     [:client_items]
                                              :group-by [1]}
                     :searches-sub-types-sys {:select      [:item_key :is_live [[:count 1] :items]] ;; :searches-types-sys/item_type
                                              :where       [:and [:not [:= :item_key client-name]]
                                                            [:not [:like :item_sub_type "%preview%"]]
                                                            [:not [:= :item_type "saved-block"]]
                                                            [:= :item_type :searches-types-sys/item_type]]
                                              :style-rules {[:* :highlight-4018a] {:logic [:= :is_live 1]
                                                                                   :style {:background-color
                                                                                             (str (theme-pull
                                                                                                    :theme/editor-outer-rim-color
                                                                                                    nil)
                                                                                                  34)
                                                                                           :color (theme-pull
                                                                                                    :theme/editor-outer-rim-color
                                                                                                    nil)}}}
                                              :from        [:client_items]
                                              :group-by    [1 2]}}
        sql-params  (into {}
                          (for [k [:searches-types-sys/item_type]]
                            {k ;;@(ut/tracked-subscribe [::conn/clicked-parameter-key [k]])
                               @(ut/tracked-sub ::conn/clicked-parameter-key-alpha {:keypath [k]})}))]
    (dorun (for [[k v] sql-calls]
             (let [query        (ut/postwalk-replacer sql-params v)
                   data-exists? @(ut/tracked-subscribe [::conn/sql-data-exists? [k]])
                   unrun-sql?   @(ut/tracked-subscribe [::conn/sql-query-not-run? [k] query])]
               (when (or (not data-exists?) unrun-sql?)
                 (conn/sql-data [k] query)
                 (ut/tracked-dispatch [::bricks/insert-sql-source k query])))))
    [re-com/h-box :size "auto" :style {:color (str (theme-pull :theme/editor-font-color nil) 35)} ;; rows label under
     :children
     [[re-com/box :size "auto" :child [bricks/magic-table :searches-types-list* [:searches-types-sys] 3 10 []]] ;; last
      [re-com/box :size "auto" :child
       [bricks/magic-table :searches-sub-types-sys-list* [:searches-sub-types-sys] 9 10 [:is_live]]]]]))

(defn search-panel-metadata-ext
  []
  (let [sql-params         (into {}
                                 (for [k [:searches-types-sys/item_type :searches-rows-sys/value
                                          :searches-sub-types-sys/item_key]]
                                   {k ;;@(ut/tracked-subscribe [::conn/clicked-parameter-key
                                      @(ut/tracked-sub ::conn/clicked-parameter-key-alpha {:keypath [k]})}))
        selected-shortcode (get sql-params :searches-rows-sys/value)
        sql-calls          {:searches-rows-sys
                              {:select [:item_sub_type :item_type :item_key :display_name :sample :value :is_live :block_meta] ;; :searches-types-sys/item_type
                               :where  [:and [:not [:like :item_sub_type "%preview%"]] [:not [:= :item_type "saved-block"]]
                                        [:= :item_key :searches-sub-types-sys/item_key]
                                        [:= :item_type :searches-types-sys/item_type]]
                               :from   [:client_items]}}]
    (dorun (for [[k v] sql-calls]
             (let [query        (ut/postwalk-replacer sql-params v)
                   data-exists? @(ut/tracked-subscribe [::conn/sql-data-exists? [k]])
                   unrun-sql?   @(ut/tracked-subscribe [::conn/sql-query-not-run? [k] query])]
               (when (or (not data-exists?) unrun-sql?) (conn/sql-data [k] query)))))
    [re-com/h-box :size "auto" :style {:color (str (theme-pull :theme/editor-font-color nil) 35)} :children
     [[re-com/v-box :size "auto" :children
       [[re-com/box :size "auto" :child
         [bricks/magic-table :searches-rows-sys-list* [:searches-rows-sys] 11 (if selected-shortcode 8.5 10)
          [:value :is_live :item_type :block_meta :item_key]]]
        (when selected-shortcode
          [re-com/box :child [bricks/shortcode-box 560 85 (str selected-shortcode " ;; rabbit-code parameter") "clojure"]])]]]]))



(defn insert-hidden-reco-preview
  [reco-selected reco-viz reco-query reco-condis combo-name shape-name single?]
  (let []
    (dorun
      (when single? (ut/tracked-dispatch [::bricks/new-reco-preview reco-selected]))
      (cond
        (= (first (read-string reco-viz)) :vega-lite)
          (let [incomingv     (read-string reco-viz) ;; read-string will ruin my internal
                ffromv        (-> (get-in incomingv [1 :data :values])
                                  (ut/replacer "_" "-")
                                  (ut/replacer "query/" ""))
                original-conn @(ut/tracked-subscribe [::bricks/lookup-connection-id-by-query-key (keyword ffromv)])
                vega-cfg      {:view {:stroke "transparent"}
                               :axis {:domainColor "#ffffff22"
                                      :grid        true
                                      :labelColor  "#ffffff88"
                                      :titleFont   "Lato"
                                      :axisFont    "Lato"
                                      :font        "Lato"
                                      :titleColor  "#ffffff99"
                                      :labelFont   "Lato"
                                      :domain      false
                                      :gridColor   "#ffffff22"
                                      :tickColor   "#ffffff22"}
                               :legend {:labelFont  "Lato"
                                        :legendFont "Lato"
                                        :labelColor "#ffffff99"
                                        :titleColor "#ffffff99"
                                        :stroke     "#ffffff99"
                                        :titleFont  "Lato"}
                               :header {:labelFont "Lato" :titleFont "Lato" :titleColor "#ffffff99"}
                               :mark {:font "Lato"}
                               :title
                                 {:font "Lato" :subtitleFont "Lato" :labelFont "Lato" :titleFont "Lato" :titleColor "#ffffff99"}}
                view          (-> incomingv
                                  (assoc-in [1 :data :values] :query-preview)
                                  (assoc-in [1 :config] :theme/vega-defaults)
                                  (assoc-in [1 :width] "container") ;:panel-width)
                                  (assoc-in [1 :height] :panel-height)
                                  (assoc-in [1 :padding] 4)
                                  (assoc-in [1 :background] "transparent"))
                q-data        (read-string reco-query)
                incoming      (first q-data) ;; read-string will ruin my internal namespaced
                ffrom         (ut/replacer (first (get incoming :from)) "_" "-")
                query         (vec (for [q q-data] (assoc q :from [(keyword ffrom)])))
                query         (ut/postwalk-replacer {[[:sum :rows]] [:count 1]} query)]
            (bricks/insert-rec-preview-block view
                                             query ;reco-h reco-w
                                             reco-condis
                                             original-conn ;reco-conn
                                             reco-selected
                                             combo-name
                                             shape-name
                                             single?
                                             true))
        :else (let [view          (read-string reco-viz) ;; read-string will ruin my internal
                    q-data        (read-string reco-query)
                    incoming      (first q-data) ;; read-string will ruin my internal
                    ffrom         (ut/replacer (first (get incoming :from)) "_" "-")
                    original-conn @(ut/tracked-subscribe [::bricks/lookup-connection-id-by-query-key
                                                          (keyword (last (ut/splitter ffrom "/")))])
                    query         (vec (for [q q-data] (if (nil? (find q :vselect)) (assoc q :from [(keyword ffrom)]) q)))
                    query         (ut/postwalk-replacer {[:sum :rows] [:count 1]} query)]
                (bricks/insert-rec-preview-block view
                                                 query ; reco-h reco-w
                                                 reco-condis
                                                 original-conn ;reco-conn
                                                 reco-selected
                                                 combo-name
                                                 shape-name
                                                 single?
                                                 true))))))

(defn editor-panel-viz2
  []
  (let [all-sql-call-keys @(ut/tracked-subscribe [::bricks/all-sql-call-keys])
        grid-reco? @(ut/tracked-subscribe [::bricks/grid-reco?])
        recos-page @(ut/tracked-subscribe [::bricks/recos-page])
        all-sql-call-keys-str (for [e all-sql-call-keys] (ut/replacer (ut/safe-name e) "-" "_"))
        sql-params (into {}
                         (for [k [:viz-tables-sys/table_name :viz-shapes0-sys/shape :viz-tables-sys/table_name
                                  :viz-shapes0-sys/shape :viz-shapes-sys/combo_edn :user-dropdown-sys/req-field]]
                           {k ;;@(ut/tracked-subscribe [::conn/clicked-parameter-key [k]])
                              @(ut/tracked-sub ::conn/clicked-parameter-key-alpha {:keypath [k]})}))
        combo-picked? (not (nil? (get sql-params :viz-shapes-sys/combo_edn)))
        shape-picked? (not (nil? (get sql-params :viz-shapes0-sys/shape)))
        clear-filters-fn (fn []
                           (do (ut/tracked-dispatch [::conn/click-parameter [:viz-shapes0-sys :shape] nil])
                               (ut/tracked-dispatch [::conn/click-parameter [:user-dropdown-sys :req-field] nil])
                               (ut/tracked-dispatch [::conn/click-parameter [:viz-shapes-sys :combo_edn] nil])))
        req-field-picked? (not (nil? (get sql-params :user-dropdown-sys/req-field)))
        sql-calls {:viz-tables-sys  {:select   [:table_name [[:count 1] :recos]]
                                     :from     [:viz_recos_vw]
                                     :where    [:and [:not [:like :table_name "query_preview%"]]
                                                (let [lst (vec (cons :or
                                                                     (vec (for [t all-sql-call-keys-str] [:= :table_name t]))))]
                                                  (if (> (count (flatten lst)) 1) lst false))]
                                     :order-by [:table_name]
                                     :group-by [:table_name]}
                   :viz-shapes0-sys {:select   [[:shape_name :shape] [[:count 1] :recos]]
                                     :from     [[:viz_recos_vw :vvt]]
                                     :where    [:and [:= :table_name :viz-tables-sys/table_name]
                                                (when req-field-picked?
                                                  [:like :combo_edn (str "%" (get sql-params :user-dropdown-sys/req-field) "%")])
                                                (when combo-picked? [:= :combo_edn :viz-shapes-sys/combo_edn])] ;; :viz-tables-sys/table_name-name
                                     :group-by [:shape_name]}
                   :viz-shapes-sys  {:select   [:combo_edn [[:count 1] :recos]]
                                     :from     [[:viz_recos_vw :vvt]]
                                     :where    [:and
                                                (when (not (nil? (get sql-params :viz-shapes0-sys/shape)))
                                                  [:= :shape_name :viz-shapes0-sys/shape])
                                                (when req-field-picked?
                                                  [:like :combo_edn (str "%" (get sql-params :user-dropdown-sys/req-field) "%")])
                                                [:= :table_name :viz-tables-sys/table_name]] ;; :viz-tables-sys/table_name-name
                                     :group-by [:combo_edn]}
                   :recos-sys       {:select   [:*]
                                     :from     [:viz_recos_vw]
                                     :order-by [[:score :desc]]
                                     :where    [:and [:= :table_name :viz-tables-sys/table_name]
                                                (when combo-picked? [:= :combo_edn :viz-shapes-sys/combo_edn])
                                                (when req-field-picked?
                                                  [:like :combo_edn (str "%" (get sql-params :user-dropdown-sys/req-field) "%")])
                                                (when shape-picked? [:= :shape_name :viz-shapes0-sys/shape])]}}
        block-list @(ut/tracked-subscribe [::conn/sql-data [:viz-tables-sys]])
        combo-list @(ut/tracked-subscribe [::conn/sql-data [:viz-shapes-sys]])
        full-recos @(ut/tracked-subscribe [::conn/sql-data [:recos-sys]])
        current-tab-queries (try (map #(-> %
                                           ut/sql-keyword
                                           name)
                                   @(ut/tracked-subscribe [::bricks/current-tab-queries]))
                                 (catch :default _ []))
        block-list (vec (filter (fn [x] (some #(= % (get x :table_name)) current-tab-queries)) block-list)) ;; overwrite above
                                                                                                            ;; with curr tab
                                                                                                            ;; only
        recos-count (count full-recos)
        block-list-boxes (for [{:keys [table_name recos]} block-list
                               :let                       [sel  (get sql-params :viz-tables-sys/table_name)
                                                           sel? (= table_name sel)]]
                           [re-com/box :attr
                            {:on-click #(do (ut/tracked-dispatch [::conn/click-parameter [:viz-tables-sys :table_name]
                                                                  table_name])
                                            (clear-filters-fn))} :align :center :justify :center :style
                            {:border-top       "1px solid #80008045"
                             :border-left      "1px solid #80008080"
                             :font-weight      700
                             :font-size        "13px"
                             :cursor           "pointer"
                             :color            (if sel?
                                                 (theme-pull :theme/editor-background-color nil) ;;"#000000"
                                                 (str (theme-pull :theme/editor-font-color nil) 77))
                             :background-color (if sel?
                                                 ;;"#9973e0"
                                                 (theme-pull :theme/universal-pop-color "#9973e0" )
                                                 (theme-pull :theme/editor-background-color nil))
                             :padding-left     "5px"
                             :padding-right    "5px"} :child (str table_name " (" recos ")")])
        combo-singles (vec (distinct (flatten (doall (for [{:keys [combo_edn recos]} combo-list]
                                                       (ut/splitter (ut/replacer combo_edn #"\"" "") ", "))))))
        pkeys @(ut/tracked-subscribe [::bricks/preview-keys])
        preview-keys (vec (for [k pkeys] (keyword (str "reco-preview" k))))
        preview-maps (into {}
                           (for [{:keys [combo_hash shape_name combo_edn query_map viz_map condis h w selected_view]} full-recos]
                             {(keyword (str "reco-preview" combo_hash)) {:shape_name    shape_name
                                                                         :query_map     query_map
                                                                         :combo_hash    combo_hash
                                                                         :selected_view selected_view
                                                                         :h             h
                                                                         :w             w
                                                                         :viz_map       viz_map
                                                                         :condis        condis
                                                                         :combo_edn     combo_edn}}))
        reco-selected (keyword (str "reco-preview"
                                    @(ut/tracked-sub ::conn/clicked-parameter-key-alpha {:keypath [:recos-sys/combo_hash]})))
        preview-container
          (fn [preview-maps pk pnum]
            (try
              (let [panel-key     (nth pk pnum)
                    ce            (get-in preview-maps [panel-key :combo_edn])
                    query_map     (get-in preview-maps [panel-key :query_map])
                    viz_map       (get-in preview-maps [panel-key :viz_map])
                    w             (get-in preview-maps [panel-key :w] 9)
                    h             (get-in preview-maps [panel-key :h] 9)
                    selected-view (try (edn/read-string (get-in preview-maps [panel-key :selected_view] nil))
                                       (catch :default _ nil))
                    condis        (get-in preview-maps [panel-key :condis])
                    combo_hash    (get-in preview-maps [panel-key :combo_hash])
                    combo_edn     (try (when (not (nil? ce)) (ut/replacer ce #"\"" "")) (catch :default _ "")) ;; TODO, why
                                                                                                               ;; bombing?
                    shape_name    (get-in preview-maps [panel-key :shape_name])
                    sel?          (= reco-selected panel-key)
                    body          [re-com/v-box :size "none" :attr
                                   {:on-click
                                      #(do (ut/tracked-dispatch [::conn/click-parameter [:recos-sys :combo_hash] combo_hash])
                                           (ut/tracked-dispatch [::conn/click-parameter [:recos-sys :combo_edn] combo_edn])
                                           (ut/tracked-dispatch [::conn/click-parameter [:recos-sys :shape_name] shape_name])
                                           (ut/tracked-dispatch [::conn/click-parameter [:recos-sys :selected_view]
                                                                 selected-view])
                                           (ut/tracked-dispatch [::conn/click-parameter [:recos-sys :h] h])
                                           (ut/tracked-dispatch [::conn/click-parameter [:recos-sys :w] w])
                                           (ut/tracked-dispatch [::conn/click-parameter [:recos-sys :query_map] query_map])
                                           (ut/tracked-dispatch [::conn/click-parameter [:recos-sys :viz_map] viz_map])
                                           (ut/tracked-dispatch [::conn/click-parameter [:recos-sys :condis] condis]))} :width
                                   "581px" :height "434px" ;"200px"
                                   :style
                                   {:zoom      0.44
                                    :border    "1px solid #ffffff22"
                                    :transform "translate(0)" ;; a known CSS hack for fixed position in
                                    :overflow  "auto"} :justify :between :children
                                   [[re-com/box :size "auto" :height "40px" :style
                                     {:padding-left  "14px"
                                      :cursor        "pointer"
                                      :padding-right "14px"
                                      :font-weight   400
                                      :font-size     "22px"} :child (str combo_edn)]
                                    [bricks/honeycomb panel-key (or selected-view :oz) 9 9]
                                    [re-com/box :size "auto" :height "40px" :justify :end :align :end :style
                                     {:padding-left "14px" :padding-right "14px" :font-weight 400 :font-size "22px"} :child
                                     (str shape_name)]]]]
                (if sel?
                  (bricks/draggable (bricks/sql-spawner-meta :viz-reco)
                                    "meta-menu"
                                    [re-com/box :style
                                     {;:border (str "1px dashed " (theme-pull :theme/editor-outer-rim-color
                                      :background-color (str (theme-pull :theme/editor-outer-rim-color nil) 22)
                                      :cursor           "grab"} :width "258px" :height "191px" :child body])
                  [re-com/box :style {:cursor "grab"} :size "auto" :child
                   (bricks/draggable (bricks/sql-spawner-meta panel-key) "meta-menu" body)]))
              (catch :default _
                [re-com/box :width "240px" :size "auto" :height "400px" :align :center :justify :center :style
                 {:color     (str (theme-pull :theme/editor-font-color nil) 22) ; "#ffffff22"
                  :zoom      0.44
                  :font-size "40px"} :child "n/a" ;(str e)
                ])))
        pages (/ recos-count 6)]
    (dorun (let [prev-preview-keys (for [k @(ut/tracked-subscribe [::bricks/preview-keys])] (keyword (str "reco-preview" k)))
                 per-page          6 ;; if we ever get more room...
                 grid-page         @(ut/tracked-subscribe [::bricks/recos-page])
                 grid-page         (if (> (- grid-page 1) pages) 0 grid-page)
                 recos             (take per-page
                                         (drop (* grid-page per-page) @(ut/tracked-subscribe [::conn/sql-data [:recos-sys]])))
                 recos-keys        (vec (for [{:keys [combo_hash]} recos] combo_hash))]
             (doseq [k prev-preview-keys] (ut/tracked-dispatch [::bricks/quick-delete-panel k]))
             (ut/tracked-dispatch [::bricks/set-preview-keys recos-keys])
             (doseq [{:keys [combo_hash query_map viz_map condis combo_edn shape_name]} recos]
               (insert-hidden-reco-preview combo_hash viz_map query_map condis combo_edn shape_name false))))
    (dorun (for [[k v] sql-calls]
             (let [query        (ut/postwalk-replacer sql-params v)
                   data-exists? @(ut/tracked-subscribe [::conn/sql-data-exists? [k]])
                   unrun-sql?   @(ut/tracked-subscribe [::conn/sql-query-not-run? [k] query])]
               (when (or (not data-exists?) unrun-sql?) (conn/sql-data [k] query)))))
    [re-com/v-box :height "433px" :children
     [[re-com/h-box :height "39px" :gap "4px" :style {:overflow "auto" :padding-bottom "5px" :padding-left "7px"} :children
       block-list-boxes]
      [re-com/h-box :height "40px" :gap "12px" :align :center :justify :start :style
       {;:background-color "maroon"
        :padding-left "6px"} :children
       [[re-com/single-dropdown :choices (conj (for [c combo-singles] {:id c :label c}) {:id nil :label "(all possible fields)"})
         :max-height "320px" :placeholder "(all possible fields)" :style {:font-size "13px"} :model
         (get sql-params :user-dropdown-sys/req-field) :on-change
         #(ut/tracked-dispatch [::conn/click-parameter [:user-dropdown-sys :req-field] %]) :width "285px"]
        [re-com/single-dropdown :choices
         (conj (for [{:keys [combo_edn recos]} combo-list] {:id combo_edn :label (str combo_edn " (" recos ")")})
               {:id nil :label "(all possible field combos)"}) :max-height "320px" :placeholder "(all possible field combos)"
         :style {:font-size "13px"} :model (get sql-params :viz-shapes-sys/combo_edn) :on-change
         #(ut/tracked-dispatch [::conn/click-parameter [:viz-shapes-sys :combo_edn] %]) :width "660px"]
        [re-com/box :child "grid" :attr {:on-click #(ut/tracked-dispatch [::bricks/set-grid-reco? true])} :style
         {:color       (if grid-reco? (theme-pull :theme/editor-font-color nil) "inherit")
          :cursor      "pointer"
          :margin-top  "-9px"
          :font-weight 700}]
        [re-com/box :child "previews" :attr {:on-click #(ut/tracked-dispatch [::bricks/set-grid-reco? false])} :style
         {:color       (if (not grid-reco?) (theme-pull :theme/editor-font-color nil) "inherit")
          :cursor      "pointer"
          :margin-top  "-9px"
          :font-weight 700}]]]
      [re-com/h-box :size "auto" :style {:color (str (theme-pull :theme/editor-font-color nil) 35)} :children
       [[re-com/box :size "none" :width "300px" :child [bricks/magic-table :viz-shapes0-list* [:viz-shapes0-sys] 7 8 []]]
        (if grid-reco?
          [re-com/box :size "none" :width "880px" :child
           [bricks/magic-table :recos-list* [:recos-sys] 19 8
            [;(when shape-picked? :shape_name)
             :query_map_str :table_name :context_hash :connection_id :h :w :condis :viz_map :combo_hash]]]
          (let [] ;; get the next 6 graphs and render?
            [re-com/h-box :children
             [[re-com/v-box :size "auto" :height "380px" :width "770px" :children
               [[re-com/h-box :size "auto" :children
                 [[preview-container preview-maps preview-keys 0] [preview-container preview-maps preview-keys 1]
                  [preview-container preview-maps preview-keys 2]]]
                [re-com/h-box :size "auto" :children
                 [[preview-container preview-maps preview-keys 3] [preview-container preview-maps preview-keys 4]
                  [preview-container preview-maps preview-keys 5]]]]]
              [re-com/v-box :size "auto" :height "380px" 
               :style {:border-top (str "1px solid " (theme-pull :theme/universal-pop-color "#9973e0") "66")
                       :overflow "hidden"} :children
               (let []
                 (for [c (range pages)]
                   [re-com/box :child (str c) :height "19px" :align :end :justify :center :attr
                    {:on-click #(ut/tracked-dispatch [::bricks/set-recos-page c])} :style
                    {:padding-right    "4px"
                     :cursor           "pointer"
                     :font-size        "12px"
                     :color            (if (= c recos-page)
                                         (theme-pull :theme/editor-background-color nil) ;;"#000000"
                                         (theme-pull :theme/editor-font-color nil))
                     :background-color (if (= c recos-page)
                                         ;;"#9973e0" ;"darkcyan"
                                         (theme-pull :theme/universal-pop-color "#9973e0")
                                         "inherit")
                     :border-bottom    (str "1px solid " (theme-pull :theme/universal-pop-color "#9973e0") "66")
                     :border-right     (str "1px solid " (theme-pull :theme/universal-pop-color "#9973e0") "66")}])) :width "30px"]]]))]]]]))

(defn editor-panel-viz
  []
  (let [all-sql-call-keys     @(ut/tracked-subscribe [::bricks/all-sql-call-keys])
        all-sql-call-keys-str (for [e all-sql-call-keys] (ut/replacer (ut/safe-name e) "-" "_"))
        sql-params            (into {}
                                    (for [k [:viz-tables-sys/table_name :viz-shapes0-sys/shape]]
                                      {k ;;@(ut/tracked-subscribe [::conn/clicked-parameter-key [k]])
                                         @(ut/tracked-sub ::conn/clicked-parameter-key-alpha {:keypath [k]})}))
        sql-calls             {:viz-tables-sys  {:select   [:table_name [[:count 1] :recos]]
                                                 :from     [:viz_recos_vw]
                                                 :where    [:and [:not [:like :table_name "query_preview%"]]
                                                            (let [lst (vec (cons :or
                                                                                 (vec (for [t all-sql-call-keys-str]
                                                                                        [:= :table_name t]))))]
                                                              (if (> (count (flatten lst)) 1) lst false))]
                                                 :order-by [:table_name]
                                                 :group-by [:table_name]}
                               :viz-shapes0-sys {:select   [[:shape_name :shape] [[:count 1] :recos]]
                                                 :from     [[:viz_recos_vw :vvt]]
                                                 :where    [:= :table_name :viz-tables-sys/table_name] ;; :viz-tables-sys/table_name-name
                                                 :group-by [:shape_name]}
                               :viz-shapes-sys  {:select   [:combo_edn [[:count 1] :recos]]
                                                 :from     [[:viz_recos_vw :vvt]]
                                                 :where    [:and
                                                            (when (not (nil? (get sql-params :viz-shapes0-sys/shape)))
                                                              [:= :shape_name :viz-shapes0-sys/shape])
                                                            [:= :table_name :viz-tables-sys/table_name]] ;; :viz-tables-sys/table_name-name
                                                 :group-by [:combo_edn]}} ;;  (ut/tracked-subscribe
                                                                          ;;  [::conn/clicked-parameter-key
       ]
    (dorun (for [[k v] sql-calls]
             (let [query        (ut/postwalk-replacer sql-params v)
                   data-exists? @(ut/tracked-subscribe [::conn/sql-data-exists? [k]])
                   unrun-sql?   @(ut/tracked-subscribe [::conn/sql-query-not-run? [k] query])]
               (when (or (not data-exists?) unrun-sql?) (conn/sql-data [k] query)))))
    [re-com/h-box :size "auto" :style {:color (str (theme-pull :theme/editor-font-color nil) 35)} :children
     [[re-com/v-box :children
       [[re-com/box :size "auto" :child [bricks/magic-table :viz-tables-list* [:viz-tables-sys] 7 5 []]]
        [re-com/box :size "auto" :child [bricks/magic-table :viz-shapes0-list* [:viz-shapes0-sys] 7 5 []]]]]
      [re-com/box :size "auto" :child [bricks/magic-table :viz-shapes-list* [:viz-shapes-sys] 7 10 []]]]]))

(defn editor-panel-status
  []
  (let [;all-sql-call-keys @(ut/tracked-subscribe [::bricks/all-sql-call-keys])
        sql-params  (into {}
                          (for [k [:viz-tables-sys/table_name :viz-shapes0-sys/shape]]
                            {k ;;@(ut/tracked-subscribe [::conn/clicked-parameter-key [k]])
                               @(ut/tracked-sub ::conn/clicked-parameter-key-alpha {:keypath [k]})}))
        client-name (str @(ut/tracked-subscribe [::bricks/client-name]))
        sql-calls   {:status-sys {:select [:*] :from [:latest_status] :where [:= :client_name client-name]}}]
    (dorun (for [[k v] sql-calls]
             (let [query        (ut/postwalk-replacer sql-params v)
                   data-exists? @(ut/tracked-subscribe [::conn/sql-data-exists? [k]])
                   unrun-sql?   @(ut/tracked-subscribe [::conn/sql-query-not-run? [k] query])]
               (when (or (not data-exists?) unrun-sql?) (conn/sql-data [k] query)))))
    [re-com/box :size "auto" :child [bricks/magic-table :status-sys-list* [:status-sys] 11 20 [:ts :client_name]]]))

(defn editor-panel-metadata-files
  []
  (let [sql-calls  {:files-sys {:select [:*] :order-by [[:screen_name :asc]] :from [:screens]}}
        sql-params (into {}
                         (for [k [:connections-sys/connection_id]]
                           {k ;;@(ut/tracked-subscribe [::conn/clicked-parameter-key [k]])
                              @(ut/tracked-sub ::conn/clicked-parameter-key-alpha {:keypath [k]})}))]
    (dorun (for [[k v] sql-calls]
             (let [query        (ut/postwalk-replacer sql-params v)
                   data-exists? @(ut/tracked-subscribe [::conn/sql-data-exists? [k]])
                   unrun-sql?   @(ut/tracked-subscribe [::conn/sql-query-not-run? [k] query])]
               (when (or (not data-exists?) unrun-sql?) (conn/sql-data [k] query)))))
    [re-com/h-box :size "auto" :style {:color (str (theme-pull :theme/editor-font-color nil) 35)} :children
     [;[re-com/box
      [re-com/box :size "auto" :child [bricks/magic-table :files-list* [:files-sys] 11.5 10 [:file_path :ts]]]]]))

(defn editor-panel-metadata-kits
  []
  (let [;market (group-by :package-name @(ut/tracked-subscribe [::bricks/all-kits])) ;
        market     (group-by :package-name @(ut/tracked-subscribe [::bricks/market-kits]))
        pkits1     (group-by :package-name @(ut/tracked-subscribe [::bricks/installed-kits]))
        curr       @(ut/tracked-sub ::conn/clicked-parameter-key-alpha {:keypath [:kits-sys/enabled]})
        kit-market {:installed-packages pkits1 :available-packages market}
        hackreact  [@db/kit-browser]]
    [re-com/v-box :size "auto" :style {:color (str (theme-pull :theme/editor-font-color nil)) :margin-top "6px" :overflow "auto"}
     :children
     (conj
       (for [[typer pkits] kit-market]
         [re-com/v-box :children
          [[re-com/box :padding "10px" :style
            {:font-size     "21px"
             :margin-top    "15px"
             :border-bottom (str "3px solid " (theme-pull :theme/editor-outer-rim-color nil))
             :color         (theme-pull :theme/editor-outer-rim-color nil)} :child
            [re-com/h-box :gap "5px" :justify :between :align :center :children
             [(str (try (name typer) (catch :default _ (str typer))))
              [re-com/box :style {:opacity 0.33 :font-size "15px"} :child
               (str " (" (count pkits) " package" (when (> (count pkits) 1) "s") ", " (count (flatten (vals pkits))) " kits)")]]]]
           (if (empty? pkits)
             [re-com/box :size "none" :align :center :justify :center :height "60px" :child
              (if (= typer :installed-packages) "(no kits installed)" "(no installable kits found)")]
             [re-com/v-box :style {:margin-left "10px"} :children
              (for [[package-name kits] pkits]
                [re-com/v-box :children
                 [[re-com/box :padding "9px" :style {:font-size "20px" :font-weight 700} :child
                   (str (try (name package-name) (catch :default _ (str package-name))))]
                  [re-com/v-box :style {:margin-left "19px"} :children
                   (for [{:keys [description fn when name icon author run-on repl installed? kit-name package-name]} kits
                         :let [disabled? (true? (not (some #(= % [package-name kit-name]) curr)))
                               selected? (= @db/kit-browser [package-name kit-name])]]
                     [re-com/h-box :padding "5px" :gap "22px" :attr
                      {:on-click #(reset! db/kit-browser (if selected? nil [package-name kit-name]))} :justify :between :align
                      :center :style
                      {;:border "1px solid #ffffff08"
                       :cursor "pointer"
                       :border (if (= [package-name kit-name] @db/kit-browser)
                                 (str "3px solid " (theme-pull :theme/editor-grid-selected-background-color nil))
                                 "3px solid #ffffff08")} :children
                      [[re-com/box :min-width "100px" :style
                        {:font-size       "17px"
                         :opacity         (if (and (not selected?) disabled?) 0.3 1)
                         :text-decoration (if (and installed? disabled?) "line-through" "")
                         :font-weight     700} :child (str name)]
                       [re-com/box :size "auto" :style {:opacity (if (and (not selected?) disabled?) 0.3 1)} :child
                        (str description)]]])]]])])]])
       [re-com/box :padding "8px" :style {:margin-left "10px" :opacity 0.33} :child
        "Kits are a great way to expand Rabbit functionality. Add customization for your data needs, or execute seperate arbitrary functions (even on another server) that receive query result & metadata - that can surface new insights, helpers, or even fun 'block builders'."])]))

(defn editor-panel-metadata-ext-kits
  []
  (let [pkits1            (group-by :package-name @(ut/tracked-subscribe [::bricks/installed-kits]))
        market            (group-by :package-name @(ut/tracked-subscribe [::bricks/market-kits]))
        pkits             (merge pkits1 market)
        hackreact         [@db/kit-browser]
        settings-param-kp [:kits-sys]
        kit-body          (filter #(= (get % :kit-name) (second @db/kit-browser)) (get pkits (first @db/kit-browser) {}))
        display-map       (dissoc (first kit-body) :fn :when)
        curr              @(ut/tracked-sub ::conn/clicked-parameter-key-alpha {:keypath [:kits-sys/enabled]})
        install-box       [re-com/h-box :height "60px" :size "none" :padding "10px" :justify :between :children
                           [[re-com/box :size "auto" :align :center :justify :center :style
                             {:border "1px solid white" :cursor "pointer"} :attr {:on-click #()} :child "download and install?"
                             :width "240px"]]]
        enabled?          (some (fn [x] (= x @db/kit-browser)) curr)
        enable-box        [re-com/h-box :height "60px" :size "none" :padding "10px" :justify :between :children
                           [[re-com/box :size "auto" :align :center :justify :center :style
                             {;:border "1px solid white"
                              :cursor    "pointer"
                              :font-size "28px"
                              :color     (when enabled? (theme-pull :theme/editor-grid-selected-background-color nil))
                              :border    (when enabled?
                                           (str "3px solid " (theme-pull :theme/editor-grid-selected-background-color nil)))}
                             :attr
                             {:on-click #(let [curr (if (nil? curr) [] curr)]
                                           (ut/tracked-dispatch [::conn/click-parameter settings-param-kp
                                                                 {:enabled (vec (conj curr @db/kit-browser))}]))} :child "enabled"
                             :width "240px"]
                            [re-com/box :size "auto" :align :center :justify :center :style
                             {;:border "1px solid white"
                              :cursor          "pointer"
                              :font-size       "28px"
                              :text-decoration (when (not enabled?) "line-through")
                              :color           (when (not enabled?) (theme-pull :theme/editor-grid-selected-background-color nil))
                              :border          (when (not enabled?)
                                                 (str "3px solid "
                                                      (theme-pull :theme/editor-grid-selected-background-color nil)))} :attr
                             {:on-click #(ut/tracked-dispatch [::conn/click-parameter settings-param-kp
                                                               {:enabled (vec (remove (fn [x] (= x @db/kit-browser)) curr))}])}
                             :child "disabled" :width "240px"]]]]
    (if (empty? kit-body)
      [re-com/box :size "auto" :align :center :justify :center :child "(select a kit function or package name)"]
      [re-com/v-box :size "auto" :padding "6px" :style
       {:color (str (theme-pull :theme/editor-font-color nil) 35) :overflow "auto"} :children
       [(if (get display-map :installed?) enable-box install-box)
        [re-com/v-box :children
         [[re-com/v-box :children
           [[re-com/gap :size "8px"]
            [re-com/v-box :padding "8px" :gap "4px" :style {:font-size "27px" :color (theme-pull :theme/editor-font-color nil)}
             :children
             [[re-com/h-box :justify :between :align :center :children
               [[re-com/box :child (get display-map :name)]
                (cond (cstr/starts-with? (str (get display-map :icon)) "zmdi") [re-com/md-icon-button :src (at) :md-icon-name
                                                                                (get display-map :icon) :style
                                                                                {;:background-color (theme-pull
                                                                                 ;:theme/editor-rim-color "#a3a3a3")
                                                                                }]
                      :else                                                    [re-com/box :style {:font-size "13px"} :child
                                                                                "no-icon"])]]
              [re-com/box :style {:font-size "17px"} :child (get display-map :description)]]] [re-com/gap :size "12px"]
            (when (get display-map :image-url)
              [re-com/box :align :center :justify :center :style {} :child
               [:img {:src (get display-map :image-url) :style {:border-radius "20px"} :width "480px"}]])
            (when (get display-map :image-url) [re-com/gap :size "12px"])]]
          [shape/map-boxes2 display-map "no-block" "full-config-map" [] "no-block" "map"]]]]])))

(defonce param-scrubber? (reagent/atom {}))
(defonce param-search (reagent/atom {}))

(defn editor-panel-metadata-params
  [single-width single-height key-type]
  (let [;theme-scrubber? false key-type :param
        param-map @(ut/tracked-subscribe [::bricks/user-parameters key-type])]
    [re-com/v-box :gap "2px" :children
     [[re-com/h-box :justify :between :children
       [[re-com/box :child (str key-type "/*") :align :center :style
         {:color        (theme-pull :theme/editor-outer-rim-color nil) ;"orange"
          :padding-left "8px"
          :font-weight  700
          :font-family  (theme-pull :theme/monospaced-font nil) ;"Fira Code"
         }]
        (when (get @param-scrubber? key-type)
          [re-com/box :child
           [re-com/input-text :model (get @param-search key-type) :on-change #(swap! param-search assoc key-type %) :height "24px"
            :width "200px" :style
            {:background-color (theme-pull :theme/editor-background-color nil)
             :border           (str "1px solid " (theme-pull :theme/editor-outer-rim-color nil))} :change-on-blur? false] :style
           {:color         (theme-pull :theme/editor-outer-rim-color nil) ;"orange"
            :padding-right "18px"
            :font-weight   700
            :font-family   (theme-pull :theme/monospaced-font nil) ;"Fira Code"
           }])]]
      (if (get @param-scrubber? key-type) ;theme-scrubber?
        [re-com/box :size "none" :width (px single-width) :height (px (- single-height 90)) ;;; [view?
                                                                                            ;;; keypath-map
                                                                                            ;;; view-key]
         :child
         [bricks/scrubber-panel true ; view?
          @(rfa/sub ::bricks/keypaths-in-params {:key-type key-type}) key-type (get @param-search key-type) {:fm true}] ; view-key
                                                                                                                        ; (update
                                                                                                                        ; kp)
         :style {:overflow "auto"}]
        [bricks/panel-param-box key-type nil (+ 17 single-width) (- single-height 66) param-map])
      [re-com/box :child (str "scrubber " (if (get @param-scrubber? key-type) "on" "off")) :attr
       {:on-click #(swap! param-scrubber? assoc key-type (not (get @param-scrubber? key-type false)))} :style
       {:padding-left "18px" :color (if (get @param-scrubber? key-type) "yellow" "inherit") :cursor "pointer"}]]]))

(defn editor-panel-metadata-ext-files
  []
  (let [sql-params (into {}
                         (for [k [:files-sys/file_path]]
                           {k ;;@(ut/tracked-subscribe [::conn/clicked-parameter-key [k]])
                              @(ut/tracked-sub ::conn/clicked-parameter-key-alpha {:keypath [k]})}))
        sql-calls  {:blocks-sys {:select   [:block_key :block_name :queries :views :block_data :view_names :query_names]
                                 :from     [:blocks]
                                 :where    [:= :file_path :files-sys/file_path]
                                 :order-by [:block_name]}}]
    (dorun (for [[k v] sql-calls]
             (let [query        (ut/postwalk-replacer sql-params v)
                   data-exists? @(ut/tracked-subscribe [::conn/sql-data-exists? [k]])
                   unrun-sql?   @(ut/tracked-subscribe [::conn/sql-query-not-run? [k] query])]
               (when (or (not data-exists?) unrun-sql?) (conn/sql-data [k] query)))))
    [re-com/h-box :size "auto" :style {:color (str (theme-pull :theme/editor-font-color nil) 35)} :children
     [;[re-com/box
      [re-com/v-box :size "auto" :children
       [[re-com/box :size "auto" :child
         [bricks/magic-table :blocks-list* [:blocks-sys] 11 10 [:screen_name :ts :block_data :view_names :query_names]]]]]]]))

(defn editor-panel-metadata-ext
  []
  (let [sql-params (into {}
                         (for [k [:connections-sys/connection_id :tables-sys/table_name]]
                           {k ;;@(ut/tracked-subscribe [::conn/clicked-parameter-key [k]])
                              @(ut/tracked-sub ::conn/clicked-parameter-key-alpha {:keypath [k]})}))
        sql-calls  {:fields-sys {:select   [:field_name :field_type :data_type]
                                 :from     [:fields]
                                 :where    [:and [:= :connection_id :connections-sys/connection_id]
                                            [:= :table_name :tables-sys/table_name] [:<> :field_type "derived"]
                                            [:<> :field_type "special"]]
                                 :order-by [:field_name]}}
        gmode      @data-browser-system-mode
        attribs?   (= gmode :attribs)
        combos?    (= gmode :combos)
        grid?      (= gmode :data)]
    (dorun (for [[k v] sql-calls]
             (let [query        (ut/postwalk-replacer sql-params v)
                   data-exists? @(ut/tracked-subscribe [::conn/sql-data-exists? [k]])
                   unrun-sql?   @(ut/tracked-subscribe [::conn/sql-query-not-run? [k] query])]
               (when (or (not data-exists?) unrun-sql?) (conn/sql-data [k] query)))))
    [re-com/h-box :size "auto" :style {:color (str (theme-pull :theme/editor-font-color nil) 35)} :children
     [;[re-com/box
      [re-com/v-box :size "auto" :children
       [[re-com/box :size "auto" :child
         [bricks/magic-table :system-fields-list*
          (cond grid?    [:fields-sys]
                attribs? [:attribs-sys]
                combos?  [:combos-sys]
                :else    [:fields-sys]) 11 10
          [:db_type :db_schema :table_type :database_name :db_catalog :table_name :connection_id :database_version :key_hash
           :context_has]]]]]]]))





(defn editor-panel-metadata-viz
  []
  (let [sql-params    (into {}
                            (for [k [:viz-tables-sys/table_name :viz-shapes0-sys/shape :viz-shapes-sys/combo_edn]]
                              {k ;;@(ut/tracked-subscribe [::conn/clicked-parameter-key [k]])
                                 @(ut/tracked-sub ::conn/clicked-parameter-key-alpha {:keypath [k]})}))
        combo-picked? (not (nil? (get sql-params :viz-shapes-sys/combo_edn)))
        shape-picked? (not (nil? (get sql-params :viz-shapes0-sys/shape)))
        sql-calls     {:recos-sys {:select   [:*]
                                   :from     [:viz_recos_vw]
                                   :order-by [[:score :desc]]
                                   :where    [:and [:= :table_name :viz-tables-sys/table_name]
                                              (when combo-picked? [:= :combo_edn :viz-shapes-sys/combo_edn])
                                              (when shape-picked? [:= :shape_name :viz-shapes0-sys/shape])]}}]
    (dorun (for [[k v] sql-calls]
             (let [query        (ut/postwalk-replacer sql-params v)
                   data-exists? @(ut/tracked-subscribe [::conn/sql-data-exists? [k]])
                   unrun-sql?   @(ut/tracked-subscribe [::conn/sql-query-not-run? [k] query])]
               (when (or (not data-exists?) unrun-sql?) (conn/sql-data [k] query)))))
    [re-com/h-box :size "auto" :style {:color (theme-pull :theme/editor-font-color nil)} :children
     [[re-com/v-box :size "auto" :children
       [[re-com/box :size "auto" :child
         [bricks/magic-table :recos-list* [:recos-sys] 11 10
          [(when shape-picked? :shape_name) :query_map_str :table_name :context_hash :connection_id :h :w :condis :viz_map
           (when combo-picked? :combo_edn)]]]]]]]))

(defn editor-panel-metadata-status
  []
  (let [pending     @(ut/tracked-subscribe [::wfx/pending-requests http/socket-id])
        client-name @(ut/tracked-subscribe [::bricks/client-name])
        pending     (remove #(cstr/ends-with? (str (get-in % [:message :ui-keypath 0])) "-sys") pending)]
    [re-com/box :height "430px" ;(px ttl-height)
     :size "none" :padding "3px" :style {:overflow "auto"} :child
     [re-com/v-box :size "none" :style {:font-size "9px" :color "#ffffff"} :children
      (for [e    pending
            :let [l  (get-in e [:message :honey-sql :select])
                  ff (get-in e [:message :honey-sql :from])
                  p  (get-in e [:message :ui-keypath])]]
        [re-com/v-box :size "none" :padding "4px" :justify :between ;;align :end
         :style {:border "1px solid green"} :children
         [[re-com/v-box :size "auto" :align :start :children
           (if (and (nil? l) (nil? ff))
             (do (when (= client-name :glamorous-carmine-leopard-exiled-from-archipelago)
                   (ut/tapp>> [:msg (get e :message) @(ut/tracked-subscribe [::bricks/runstream-running?])]))
                 [[re-com/box :child (str (get e :message))]])
             [[re-com/box :child (str ":s - " l)] [re-com/box :child (str ":f - " ff) :style {:color "#ffffff88"}]])]
          [re-com/box :size "auto" :justify :end :align :end :style {:color "#ffffff88"} :child (str ":kp - " p)]]])]]))






(defn ui-debugger
  []
  (let [;bw 33 bh 11
        sched      @(ut/tracked-subscribe [::bricks/query-schedule])
        params     (merge sched
                          {"client-name"                     @(ut/tracked-subscribe [::bricks/client-name])
                           "[::bricks/last-heartbeat]"       @(ut/tracked-subscribe [::bricks/last-heartbeat])
                           "db/context-modal-pos"            @db/context-modal-pos
                           "db/editor-mode"                  @db/editor-mode
                           "db/auto-refresh?"                @db/auto-refresh?
                           "db/param-filter"                 @db/param-filter
                           "data-browser-query"              @db/data-browser-query
                           "[::bricks/selected-block-map]"   @(ut/tracked-subscribe [::bricks/selected-block-map])
                           "bricks/dragging?"                @bricks/dragging?
                           "bricks/dyn-dropper-hover (!!!!)" @bricks/dyn-dropper-hover
                           "bricks/dragging-size"            @bricks/dragging-size
                           "bricks/dragging-body"            @bricks/dragging-body
                           "db/last-mouse-activity"          @db/last-mouse-activity
                           "bricks/mad-libs-view"            @bricks/mad-libs-view
                           "bricks/mad-libs-top?"            @bricks/mad-libs-top?
                           "bricks/mouse-dragging-panel?"    @bricks/mouse-dragging-panel?
                           "bricks/hover-square"             @bricks/hover-square
                           "bricks/on-block?"                @bricks/on-block?
                           "bricks/dragging-editor?"         @bricks/dragging-editor?
                           "bricks/over-block?"              @bricks/over-block?
                           "bricks/new-block-atom"           @bricks/new-block-atom})
        atom-sizes (ut/calculate-atom-sizes
                     {:ut/replacer-data               ut/replacer-data
                      :ut/replacer-cache              ut/replacer-cache
                      :ut/deep-flatten-data           ut/deep-flatten-data
                      :ut/deep-flatten-cache          ut/deep-flatten-cache
                      :ut/split-cache                 ut/split-cache
                      :ut/split-cache-data            ut/split-cache-data
                      :ut/postwalk-replace-data-cache ut/postwalk-replace-data-cache
                      :ut/postwalk-replace-cache      ut/postwalk-replace-cache
                      :db/flow-results                db/flow-results
                      :db/scrubbers                   db/scrubbers
                      :ut/is-base64-atom              ut/is-base64-atom
                      :ut/is-large-base64-atom        ut/is-large-base64-atom
                      :ut/safe-name-cache             ut/safe-name-cache
                      :re-frame.db/app-db             re-frame.db/app-db
                      :ut/clean-sql-atom              ut/clean-sql-atom
                      :ut/format-map-atom             ut/format-map-atom
                      :ut/body-set-atom               ut/body-set-atom
                      :ut/data-typer-atom             ut/data-typer-atom
                      :ut/coord-cache                 ut/coord-cache})
        params     (merge params atom-sizes)
        params     (vec (for [[k v] params] [k (str k) v]))] ;; fake middle key since they are
    [re-com/box :height "430px" ;(px ttl-height)
     :size "none" :padding "3px" :style {:overflow "auto"} :child
     [re-com/v-box :size "none" :style {:font-size "11px" :color "#ffffff"} :children
      (for [[l _ p] (sort-by second (seq params))] ;(for [[l p] (sort params)] ;(vec (sort-by
        [re-com/v-box :size "none" :padding "4px" :justify :between ;;align :end
         :style {:border "1px solid green"} :children
         [[re-com/box :size "auto" :align :start :style
           {:color (when (keyword? l)
                     (theme-pull :theme/editor-outer-rim-color nil) ;;"orange"
                   )} :child (str l)]
          [re-com/box :size "auto" :justify :end :align :end :style {:color "#ffffff88"} :child
           (if (integer? p) (str "*in " p " seconds") (str p))]]])]]))

(re-frame/reg-event-db ::save-search-results (fn [db [_ res]] (assoc db :search-results res)))

(re-frame/reg-sub ::search-results (fn [db _] (get db :search-results)))



(defn search-panel-left
  [single-width single-height]
  (let [client-name @(ut/tracked-subscribe [::bricks/client-name])
        results     @(ut/tracked-subscribe [::search-results])]
    [re-com/v-box :children
     [;[re-com/box
      [re-com/box :padding "10px" :child
       [re-com/input-text :model @db/search-box :on-change
        #(do (reset! db/search-box %)
             (when (ut/ne? (cstr/trim (str %)))
               (ut/tracked-dispatch
                 [::wfx/request :default
                  {:message     {:kind :search-groups :client-name client-name :max-hits 200 :search-str (str %)}
                   :on-response [::save-search-results]
                   :timeout     500000}]))) :height "50px" :width (px (- single-width 25)) :style
        {:background-color (theme-pull :theme/editor-background-color nil)
         :font-size        "25px"
         :font-weight      700
         :color            (theme-pull :theme/editor-font-color nil) ;; (theme-pull
         :border           (str "1px solid " (theme-pull :theme/editor-outer-rim-color nil) 75)}]]
      [re-com/box :height (px (- single-height 120)) :style {:border "1px solid cyan" :overflow "auto"} :size "none" :child
       [re-com/v-box :padding "10px" :style {:color (theme-pull :theme/editor-font-color nil)} :children
        (for [[r cnt] results]
          [re-com/h-box :padding "4px" :justify :between :align :center :height "30px" :style {:border "1px solid darkcyan"}
           :children [[re-com/box :child (str r)] [re-com/box :child (str (ut/nf cnt))]]])]]] :size "auto" :width
     (px single-width) :height (px single-height)]))

(defn search-panel-right
  [single-width single-height]
  [re-com/box :child "search" :size "auto" :width (px single-width) :height (px single-height) :style
   {:border "2px solid yellow"}])

(def view-title-edit-idx (reagent/atom nil))

(defn editor-panel
  [bricks-wide bricks-tall]
  (let [selected-panel-map  @(ut/tracked-subscribe [::bricks/selected-block-map])
        selected-block      @(ut/tracked-subscribe [::bricks/selected-block])
        sql-calls           @(rfa/sub ::bricks/panel-sql-calls {:panel-key selected-block})
        views               @(rfa/sub ::bricks/panel-views {:panel-key selected-block})
        runners             @(rfa/sub ::bricks/panel-runners {:panel-key selected-block})
        system-panel?       (or (= selected-block "none!") (nil? selected-block))
        first-data-key      (first (keys sql-calls))
        first-view-key      (first (keys views))
        block-runners-map   @(ut/tracked-subscribe [::bricks/block-runners])
        runners?            (ut/ne? runners)
        data-key            (if (get @db/data-browser-query selected-block)
                              (get @db/data-browser-query selected-block)
                              first-data-key)
        data-key            (if (nil? data-key) first-view-key data-key)
        queries?            (ut/ne? sql-calls)
        views?              (ut/ne? views)
        view-selected?      (true? (or (and views? (some #(= % (get @db/data-browser-query selected-block)) (keys views)))
                                       (not queries?)))
        panel-count         (if system-panel? 3 (if (or views? queries?) 3 2))
        bricks-wide         (if (or system-panel? queries? views?) bricks-wide (js/Math.ceil (* bricks-wide 0.66)))
        ttl-width           (* bricks-wide bricks/brick-size)
        ttl-width-px        (px ttl-width)
        ttl-height          (* bricks-tall bricks/brick-size)
        ttl-height-px       (px (+ 10 ttl-height))
        x-px                (px (first @detached-coords)) ;(px (* x bricks/brick-size))
        y-px                (px (last @detached-coords)) ;(px (* y bricks/brick-size))
        single-width-bricks (js/Math.floor (/ bricks-wide panel-count))
        single-width        (* single-width-bricks bricks/brick-size)
        single-width-px     (px single-width)
        hh1                 [@db/mad-libs-waiting-room @db/data-browser-query-con 
                             @db/item-browser-mode @db/scrubbers @db/solver-meta-spy
                             @db/value-spy] ;; reactivity hack! React!
        single-height       ttl-height
        single-height-px    ttl-height-px
        click-params        @(ut/tracked-subscribe [::bricks/all-click-params])
        sql-string          (bricks/materialize-one-sql selected-block data-key)
        reco-selected       @(ut/tracked-sub ::conn/clicked-parameter-key-alpha {:keypath [:recos-sys/combo_hash]})
        reco-combo          @(ut/tracked-sub ::conn/clicked-parameter-key-alpha {:keypath [:recos-sys/combo_edn]})
        shape-name          @(ut/tracked-sub ::conn/clicked-parameter-key-alpha {:keypath [:recos-sys/shape_name]})
        reco-query          @(ut/tracked-sub ::conn/clicked-parameter-key-alpha {:keypath [:recos-sys/query_map]})
        reco-viz            @(ut/tracked-sub ::conn/clicked-parameter-key-alpha {:keypath [:recos-sys/viz_map]})
        reco-condis         @(ut/tracked-sub ::conn/clicked-parameter-key-alpha {:keypath [:recos-sys/condis]})
        reco-preview        @(ut/tracked-subscribe [::bricks/reco-preview])
        reco-selected?      (not (nil? reco-selected))
        mad-libs-combo?     (ut/ne? (get selected-panel-map :mad-libs-combo-hash))
        mad-libs-combos     (when mad-libs-combo?
                              @(ut/tracked-subscribe [::bricks/get-combo-rows selected-block
                                                      (get selected-panel-map :mad-libs-combo-hash)]))
        screen-name         (ut/safe-name @(ut/tracked-subscribe [::bricks/screen-name]))
        screen-name-regex   #"(.|\s)*\S(.|\s)*"
        websocket-status    (select-keys @(ut/tracked-subscribe [::http/websocket-status]) [:status :datasets :panels :waiting])]
    (when (nil? (get @db/data-browser-query selected-block)) ;; at this point lets just
      (swap! db/data-browser-query assoc selected-block data-key))
    (when (and (not (= reco-selected reco-preview)) reco-selected?)
      (insert-hidden-reco-preview reco-selected reco-viz reco-query reco-condis reco-combo shape-name true))
    [bricks/reecatch
     [re-com/h-box :size "none" :children
      [[re-com/box :size "none" :child
        [re-com/v-box :size "1" :children
         [[re-com/box :padding "4px" :child
           [re-com/h-box :gap "6px" :children
            [[re-com/md-icon-button :src (at) :md-icon-name "zmdi-arrows" :style
              {;:background-color (theme-pull :theme/editor-rim-color "#a3a3a3") ;"#00000000"
               :color            (theme-pull :theme/editor-font-color nil)
               :cursor           "grab"
               :height           "15px"
               :margin-top       "-2px"
               :font-size        "19px"} :attr {:on-mouse-down mouse-down-handler}]
             [re-com/h-box :size "auto" :justify :between :children
              [[re-com/box :child
                (if system-panel?
                  [re-com/h-box :align :center :justify :center :gap "15px" :children
                   [[re-com/box :child (str "db browser") :style
                     {;:opacity (if @file-mode? 0.4 1.0)
                      :opacity (if (= @db/editor-mode :meta) 1.0 0.4)
                      :cursor  "pointer"} :attr {:on-click #(reset! db/editor-mode :meta)}]
                    [re-com/box :child (str "subscriptions") :style
                     {;:opacity (if @file-mode? 0.4 1.0)
                      :opacity (if (= @db/editor-mode :search) 1.0 0.4)
                      :cursor  "pointer"} :attr {:on-click #(reset! db/editor-mode :search)}]
                    [re-com/box :child (str "viz recos") :style {:opacity (if (= @db/editor-mode :vvv) 1.0 0.4) :cursor "pointer"}
                     :attr {:on-click #(reset! db/editor-mode :vvv)}]
                    [re-com/box :child (str "files") :style
                     {;:opacity (if @file-mode? 1.0 0.4)
                      :opacity (if (= @db/editor-mode :files) 1.0 0.4)
                      :cursor  "pointer"} :attr {:on-click #(reset! db/editor-mode :files)}]
                    [re-com/box :child (str "params") :style {:opacity (if (= @db/editor-mode :params) 1.0 0.4) :cursor "pointer"}
                     :attr {:on-click #(reset! db/editor-mode :params)}]
                    [re-com/box :child (str "kits") :style
                     {;:opacity (if @file-mode? 1.0 0.4)
                      :opacity (if (= @db/editor-mode :kits) 1.0 0.4)
                      :cursor  "pointer"} :attr {:on-click #(reset! db/editor-mode :kits)}]
                    [re-com/box :child ;(str "*")
                     [re-com/md-icon-button :src (at) :md-icon-name "zmdi-bug" :style
                      {;:background-color (theme-pull :theme/editor-rim-color "#a3a3a3")
                       :cursor        "pointer"
                       :height        "15px"
                       :padding-right "5px"
                       :margin-top    "-6px"
                       :font-size     "19px"}] :style {:opacity (if (= @db/editor-mode :status) 1.0 0.4) :cursor "pointer"} :attr
                     {:on-click #(reset! db/editor-mode :status)}]
                    [re-com/md-icon-button :src (at) :md-icon-name "zmdi-refresh" :style
                     {;:background-color (theme-pull :theme/editor-rim-color "#a3a3a3")
                      :color         (str (theme-pull :theme/editor-font-color nil) 44) ;;"#ffffff44"
                      :cursor        "pointer"
                      :height        "15px"
                      :padding-right "5px"
                      :margin-top    "-6px"
                      :font-size     "19px"} :attr
                     {:on-click #(ut/tracked-dispatch [::bricks/refresh-meta-tables @db/editor-mode])}]]]
                  (str selected-block "'s panel map"))]]]]] :style
           {:font-weight   500
            :color         (theme-pull :theme/editor-font-color nil)
            :border-radius "11px 0px 0px 0px"
            :background    (str "linear-gradient(" (theme-pull :theme/editor-rim-color nil) ", transparent)")
            :user-select   "none"}]
          (cond
            system-panel?                                            (condp = @db/editor-mode ; @file-mode?
                                                                       :files  [re-com/box :child [editor-panel-metadata-files]
                                                                                :style {:padding-top "10px"} :height
                                                                                (px (- ttl-height 40))]
                                                                       :kits   [re-com/box :child [editor-panel-metadata-kits]
                                                                                :style {:padding-top "10px"} :height
                                                                                (px (- ttl-height 40))]
                                                                       :params [re-com/box :child
                                                                                [editor-panel-metadata-params single-width
                                                                                 single-height :param] :style
                                                                                {:padding-top "10px"} :height
                                                                                (px (- ttl-height 40))]
                                                                       :meta   [re-com/box :child [editor-panel-metadata] :style
                                                                                {:padding-top "10px"} :height
                                                                                (px (- ttl-height 40))]
                                                                       :viz    [re-com/box :child [editor-panel-viz] :style
                                                                                {:padding-top "10px"} :height
                                                                                (px (- ttl-height 40))]
                                                                       :search [re-com/box :child [search-panel-metadata] ;;[search-panel-left
                                                                                                                          ;;single-width
                                                                                :style {:padding-top "10px"} :height
                                                                                (px (- ttl-height 40))]
                                                                       :vvv    [re-com/box :child [editor-panel-viz2] :style
                                                                                {:padding-top "10px"} :height
                                                                                (px (- ttl-height 40))]
                                                                       :status [re-com/box :child [ui-debugger] :style
                                                                                {:padding-top "10px"} :height
                                                                                (px (- ttl-height 40))])
            (= :viz-gen (get @db/data-browser-query selected-block)) ;; viz-gen?
              (let [src-table-id-str (last (get selected-panel-map :mad-libs-combo-hash))
                    combo-hash       (first (get selected-panel-map :mad-libs-combo-hash))
                    opts             @(ut/tracked-subscribe [::bricks/get-mad-libs-options selected-block src-table-id-str
                                                             combo-hash])
                    shape-name       (get opts :shape_name)
                    viz-shape        @(ut/tracked-subscribe [::bricks/get-viz-shape shape-name])
                    color-keys       [:YlGn :desc]
                    hacko            @db/mad-libs-box
                    color-keys-which (last (reverse (sort-by name (keys (get ut/colorbrewer (first color-keys))))))
                    colors1          (vec (if (= (last color-keys) :desc)
                                            (reverse (get-in ut/colorbrewer [(first color-keys) color-keys-which]))
                                            (get-in ut/colorbrewer [(first color-keys) color-keys-which])))
                    zero-color       (if (= (last color-keys) :desc) (first colors1) (last colors1))
                    value-to-color   (fn [value-vector color-vector value]
                                       (let [min-val    (apply min value-vector)
                                             max-val    (apply max value-vector)
                                             normalized (when (> (- max-val min-val) 0) ; Avoid
                                                          (/ (- value min-val) (- max-val min-val)))
                                             idx        (int (Math/floor (* normalized (dec (count color-vector)))))]
                                         (if (and normalized (>= idx 0) (< idx (count color-vector))) (color-vector idx) nil)))
                    viz-shape-rules  (first (filter #(= (get % :shape_name) shape-name)
                                              @(ut/tracked-subscribe [::conn/sql-data [:viz-shapes]])))
                    rounder          (fn [num] (/ (js/Math.round (* num 100)) 100))
                    open-shape?      (get-in @db/mad-libs-box [selected-block shape-name] false)
                    axes-logic-map   (try (edn/read-string (get viz-shape :axes_logic)) (catch :default _ {}))
                    opts-data        (get opts :data)]
                [re-com/v-box :children
                 [[re-com/h-box :justify :between :children
                   [[re-com/box ;:align :end  ;:align :center ;:justify :end
                     :style
                     {:color          (theme-pull :theme/editor-outer-rim-color nil)
                      :font-family    (theme-pull :theme/base-font nil)
                      :transition_NOT "all 0.2s ease-in-out"
                      :font-size      "21px"
                      :font-weight    700
                      :margin-left    "19px"} :child (str shape-name)]
                    [re-com/md-icon-button :src (at) :md-icon-name (if open-shape? "zmdi-chevron-down" "zmdi-chevron-up") :attr
                     {:on-click #(swap! db/mad-libs-box assoc-in [selected-block shape-name] (not open-shape?))} :style
                     {:color (theme-pull :theme/editor-outer-rim-color nil) :padding-right "16px" :font-size "25px"}]]]
                  (when open-shape?
                    [re-com/box :padding "8px" :height "435px" :size "none" :child
                     [bricks/read-only-clojure-box single-width 0
                      (str ";; rules, viz & query shapes for :"
                           shape-name
                           "\n"
                           ";;  (defined in ./data/viz-shapes.edn) \n"
                             ";; ------------------------------------------------------- \n"
                           ";; base query map(s) \n" (str (get viz-shape-rules :sql_maps))
                           ";; base viz / view shape \n" (str (get viz-shape-rules :library_shapes))
                           ";; axes rules / logic \n" (str (get viz-shape-rules :axes_logic)))]])
                  [re-com/v-box :padding "6px" :gap "8px" :height (px (- ttl-height 34)) :style
                   {:overflow "auto" :transition_NOT "all 0.2s ease-in-out"} :children
                   (for [[k o] (group-by #(first (first %)) opts-data)
                         :let  [short-axes (-> (str k)
                                               (ut/replacer #"-field" "")
                                               (ut/replacer #":" ""))
                                open?      (get-in @db/mad-libs-box [selected-block short-axes] false)]]
                     [re-com/v-box ;:size "auto"
                      :padding "9px" :justify :between :style
                      {:border         (str "2px dashed " (theme-pull :theme/editor-outer-rim-color nil) 44)
                       :transition_NOT "all 0.2s ease-in-out"
                       :border-radius  "16px"} :children
                      [[re-com/v-box :children
                        [[re-com/h-box :style {:transition_NOT "all 0.2s ease-in-out"} :align :center :justify :between :children
                          [[re-com/box :child (str short-axes) :style
                            {:color          (theme-pull :theme/editor-outer-rim-color nil)
                             :font-family    (theme-pull :theme/base-font nil)
                             :font-size      "19px"
                             :transition_NOT "all 0.2s ease-in-out"
                             :font-weight    700
                             :margin-left    "2px"}]
                           [re-com/h-box :gap "8px" :children
                            [(when open?
                               (let [q1     (keyword (str "show-me-query-" (rand-int 12345)))
                                     q2a    (str "show-me-attribs-" (rand-int 12345))
                                     q2     (keyword q2a)
                                     q2-ref (keyword (str "query/" q2a))
                                     q3     (keyword (str "pivoted-attribs-" (rand-int 12345)))]
                                 (bricks/draggable
                                   {:h             6
                                    :w             22
                                    :connection-id "system"
                                    :name          (str "show me why - fields " short-axes
                                                        " for "                 src-table-id-str
                                                        " / "                   shape-name)
                                    :queries       {q1 {:select     [:axes_key :connection_id :db_type
                                                                     [[:case [:= :derived_name nil] :field_name :else
                                                                       :derived_name] :field] :field_name :derived_calc
                                                                     :derived_name :shape_name :table_name]
                                                        :from       [[:found_fields]]
                                                        :where      [:and [:= :table_name src-table-id-str]
                                                                     [:= :shape_name shape-name] [:= :axes_key short-axes]]
                                                        :col-widths {:axes_key 90 :table_name 180 :connection_id 90}}
                                                    q2 {:select     [:attribute_name :attribute_value :connection_id ;:context_hash
                                                                     :db_type
                                                                     [[:case [:= :derived_name nil] :field_name :else
                                                                       :derived_name] :field] :field_name :derived_calc
                                                                     :derived_name :table_name ;:table_type
                                                                                               ;:updated
                                                                    ]
                                                        :from       [[:attributes :nn359]]
                                                        :where      [:and [:= :table_name src-table-id-str]]
                                                        :col-widths {:attribute_name 227}}
                                                    q3 {:transform-select [[[:sum :attribute_value] :val] :field :derived_name
                                                                           :derived_calc :table_name]
                                                        :from             [q2-ref]
                                                        :pivot-by         [:attribute_name]
                                                        :col-widths       {:field 240 :table_name 200}}}
                                    :tab           @(ut/tracked-subscribe [::bricks/selected-tab])
                                    :drag-meta     {:block-name (str "show-me-" (rand-int 1234)) :type :viz-reco}}
                                   "meta-menu"
                                   [re-com/box :child "show me why" :padding "5px" :style
                                    {:background-color (str (theme-pull :theme/editor-outer-rim-color nil) 33)
                                     :font-size        "10px"
                                     :cursor           "grab"
                                     :font-weight      700 ;:margin-top "4px"
                                     :color            (theme-pull :theme/editor-outer-rim-color nil)
                                     :border-radius    "10px"}])))
                             [re-com/md-icon-button :src (at) :md-icon-name (if open? "zmdi-chevron-down" "zmdi-chevron-up") :attr
                              {:on-click #(swap! db/mad-libs-box assoc-in [selected-block short-axes] (not open?))} :style
                              {:color (theme-pull :theme/editor-outer-rim-color nil) :font-size "25px"}]]]]]
                         (when open?
                           [re-com/box :style
                            {:font-size      "9px"
                             :padding-bottom "10px"
                             :transition_NOT "all 0.2s ease-in-out"
                             :padding-top    "10px"
                             :font-family    (theme-pull :theme/base-font nil)
                             :color          "green"} :child
                            [bricks/read-only-clojure-box single-width 0
                             (str ";; "                     k
                                  " in :"                   shape-name
                                  " rules defined as... \n" (get axes-logic-map (keyword short-axes)))]])
                         [re-com/v-box :children
                          (for [[oo ov] o
                                :let    [fname              (get (last ov) :field_name)
                                         drname             (get (last ov) :derived_name)
                                         all-axes-vec       (vec (for [[oo ov] o] (last ov)))
                                         hh                 @db/mad-libs-waiting-room ;; hack
                                         all-score2         (vec (map :score2 all-axes-vec))
                                         score2             (get (last ov) :score2)
                                         axes               (get (last ov) :axes)
                                         cur-axes           (get-in opts [:current-combo axes])
                                         cur-axes-keyhashes (into {}
                                                                  (for [[k v] (get opts :current-combo)] {k (get v :key_hash)}))
                                         cur-axes-rev       (for [r (vals (dissoc (get-in opts [:current-combo]) axes))]
                                                              (select-keys r [:field_name :derived_name]))
                                         cur-rev?           (true? (some #(= {:field_name   (get (last ov) :field_name)
                                                                              :derived_name (get (last ov) :derived_name)}
                                                                             %)
                                                                         cur-axes-rev))
                                         cur?               (and (= (get cur-axes :field_name) (get (last ov) :field_name))
                                                                 (= (get cur-axes :derived_name) (get (last ov) :derived_name)))
                                         new-axe            {axes (get (last ov) :key_hash)}
                                         new-curr-axe       (into {} (sort-by key (merge cur-axes-keyhashes new-axe)))
                                         heat-color         (value-to-color all-score2 colors1 score2)
                                         heat-color         (if (nil? heat-color) zero-color heat-color)
                                         loading?           (= new-curr-axe @db/mad-libs-waiting-room)]]
                            [re-com/h-box :style
                             {;:margin-left "3px"
                              :transition_NOT  "all 0.2s ease-in-out"
                              :text-decoration (if cur-rev? "line-through" "inherit")
                              :cursor          (if (or cur? cur-rev?) "inherit" "pointer")
                              :color           (if cur? "#ffffff" "inherit")} :gap "6px" :children
                             [[re-com/box :child " " :height "12px" :width "12px" :style
                               {:background-color heat-color
                                :opacity          (if (= score2 0) 0.6 "inherit")
                                :border-radius    "14px"
                                :margin-left      "2px"
                                :margin-top       "5px"}]
                              [re-com/h-box :gap "8px" :size "auto" :align :center :justify :between :style
                               {:font-size    "13px"
                                :font-weight  700
                                :padding-left "5px"
                                :font-style   (if (or (not (nil? drname)) (= fname "rows")) "italic" "inherit")
                                :font-family  (theme-pull :theme/base-font nil)} :children
                               [[re-com/box :style {:background-color (if loading? "#ffffff21" "inherit") :border-radius "14px"}
                                 :child (str (if (nil? drname) fname drname))]
                                [re-com/h-box :gap "5px" :children
                                 [(when open?
                                    [re-com/box :style {:font-weight 200 :opacity 0.7} :child (str "score: " (rounder score2))])
                                  (when loading? ;(= new-curr-axe @db/mad-libs-waiting-room)
                                    [:img {:src "images/loading-cropped.gif" :width "19px" :height "19px"}])]]]]] :attr
                             {:on-click #(when (not (or cur? cur-rev?))
                                           (let []
                                             (ut/tracked-dispatch [::bricks/get-combo-hash selected-block src-table-id-str
                                                                   shape-name new-curr-axe])))}])]]]]])]]])
            
            :else                                                    (let [selected-kp @(ut/tracked-sub ::bricks/editor-panel-selected-view {})
                                                                           selected-kp (if (nil? (first selected-kp))
                                                                                         nil
                                                                                         selected-kp)] ;; if viz-gen or
                                                                       ;;(ut/tapp>> [:sel-keypath (str selected-kp) (get @db/data-browser-query selected-block)])
                                                                       (if ;(and ;view-selected?
                                                                         (get-in @db/scrubbers [selected-block data-key] false)

                                                                         [re-com/box :size "none" :width (px single-width) :height
                                                                          (px (- single-height 40)) :child
                                                                          [bricks/scrubber-panel view-selected?
                                                                           (if view-selected?
                                                                             @(ut/tracked-subscribe [::bricks/keypaths-in-view
                                                                                                     selected-block data-key])
                                                                             @(ut/tracked-subscribe [::bricks/keypaths-in-query
                                                                                                     selected-block data-key]))
                                                                           data-key nil {:fm true}] :style {:overflow "auto"}]
                                                                         
                                                                         (let [selected-view-type @(ut/tracked-sub ::bricks/view-type {:panel-key selected-block :view data-key})
                                                                               syntax (get-in block-runners-map [selected-view-type :syntax])] 
                                                                           ;;(ut/tapp>> [selected-view-type  syntax])
                                                                           (if (or (nil? syntax) (= syntax "clojure"))
                                                                             [bricks/panel-code-box selected-block selected-kp
                                                                              (+ 17 single-width) (- single-height 20)
                                                                              (if (nil? selected-kp)
                                                                                selected-panel-map
                                                                                (get-in selected-panel-map selected-kp))]
                                                                             [bricks/panel-string-box selected-kp (+ 17 single-width) (- single-height 20)
                                                                              (if (nil? selected-kp)
                                                                                selected-panel-map
                                                                                (get-in selected-panel-map selected-kp))
                                                                              syntax
                                                                              ]
                                                                             )))))
          (when (and system-panel? (= @db/editor-mode :status))
            [re-com/box :child (str websocket-status) :style
             {:color        (str (theme-pull :theme/editor-font-color nil) 77) ;; "#ffffff44"
              :font-size    "12px"
              :margin-top   "-9px"
              :padding-left "8px"}])]] :height single-height-px :width
        (if (and (= @db/editor-mode :vvv) system-panel?) (px (* 2 single-width)) single-width-px) :style {:overflow "hidden"}]
       (cond
         (or views? queries? runners?) ; data-key
           (let [data-key-type @(ut/tracked-sub ::bricks/view-type {:panel-key selected-block :view data-key})
                 view-type-map @(ut/tracked-sub ::bricks/view-type-map {:view-type data-key-type})
                 modes (get view-type-map :modes)
                 current-view-mode @(ut/tracked-sub ::bricks/current-view-mode {:panel-key selected-block :data-key data-key})
                 ;;_ (ut/tapp>> [selected-block data-key data-key-type view-type-map])
                 ;data-key-type (if (nil? data-key-type) :* data-key-type)
                 ;selected-block (if (nil? (get @db/data-browser-query selected-block)) first-data-key data-key)
                 ]
             [re-com/box :size "none" :child
              [re-com/v-box :size "1" :children
               [[re-com/h-box :padding "4px" 
                 :justify :between :align :center 
                 :children [[re-com/box :child (str "queries & views")]
                            [re-com/box :child (str "runner: " data-key-type) 
                             :style {:border (str "1px solid " (str (theme-pull :theme/editor-outer-rim-color nil)))
                                     :color (str (theme-pull :theme/editor-outer-rim-color nil))
                                     ;:font-weight 700
                                     :padding-left "3px"
                                     :padding-right "3px"
                                     :margin-top "2px"
                                     :border-radius "7px"
                                     :font-size "13px"}
                             ]
                            [re-com/h-box 
                             :style {:padding-right "10px"}
                             :gap "4px"
                             :children (for [mode modes ;["clover" "text" "pretty" "rowset" "map-boxes"]
                                             :let [bkgrd (str (theme-pull :theme/editor-outer-rim-color nil))
                                                   selected? (= mode current-view-mode)
                                                   bkgrd (if (> (count bkgrd) 7) (subs bkgrd 0 7)  bkgrd)]]
                                         [re-com/box :child (str mode)
                                          :attr (when (not selected?)
                                                  {:on-click #(ut/tracked-dispatch [::bricks/set-view-mode selected-block data-key mode])})
                                          :style {:background-color (str bkgrd (if selected? "" 45))
                                                  :color "black"
                                                  :cursor (when (not selected?) "pointer")
                                                  :padding-left "3px"
                                                  :padding-right "3px"
                                                  :margin-top "2px"
                                                  :border-radius "7px"
                                                  :font-size "13px"
                                                  }])]] 
                 :style
                 {:font-weight 500
                  :color       (theme-pull :theme/editor-font-color nil)
                  :background  (str "linear-gradient(" (theme-pull :theme/editor-rim-color nil) ", transparent)")}]
                [re-com/h-box :justify :between :children
                 [[re-com/h-box :children
                   [(when mad-libs-combos
                      [re-com/h-box :gap "7px" :padding "5px" :style {:font-weight 700 :color "#dddddd"} :children
                       (for [k [(str (count mad-libs-combos))]]
                         (let [k :viz-gen] ;; hack TODO
                           [re-com/h-box :align :center :justify :center :style
                            {:margin-left      "-5px"
                             :border-top       "1px solid #80008045"
                             :border-left      "1px solid #80008080"
                             :color            (if (= k data-key)
                                                 (theme-pull :theme/editor-background-color nil)
                                                 (str (theme-pull :theme/editor-font-color nil) 77))
                             :background-color (if (= k data-key)
                                                 ;"#9973e0"
                                                 (theme-pull :theme/universal-pop-color "#9973e0")
                                                 "inherit")
                             :padding-right    "5px"
                             :padding-left     "5px"} :gap "4px" :children
                            [[re-com/box :padding "3px" :attr
                              {:on-click #(do (swap! db/data-browser-query assoc selected-block k))} :style
                              {:cursor "pointer" :font-size "13px" :padding-left "5px" :padding-right "5px"} :child (str k)]]]))])
                    [re-com/h-box :gap "7px" :padding "5px" :style {:font-weight 700 :color "#dddddd"} :children
                     [[re-com/h-box :style
                       {:margin-left      "-5px"
                        :border-top       "1px solid #80008045"
                        :border-left      "1px solid #80008080"
                        :color            (if (= :* data-key)
                                            (theme-pull :theme/editor-background-color nil) ;;"#000000"
                                            (theme-pull :theme/editor-font-color nil))
                        :background-color (if (= :* data-key) 
                                            ;"#9973e0" 
                                            (theme-pull :theme/universal-pop-color "#9973e0")
                                            "inherit")
                        :padding-right    "5px"
                        :padding-left     "5px"} :gap "4px" :children
                       [[re-com/box :padding "3px" :attr {:on-click #(do (swap! db/data-browser-query assoc selected-block :*))}
                         :style {:cursor "pointer" :font-size "13px" :padding-left "5px" :padding-right "5px"} :child
                         (str :*)]]]]]
                    
                    (when queries?
                      [re-com/h-box :gap "7px" :padding "5px" :style {:font-weight 700 :color "#dddddd"} :children
                       (for [k (keys sql-calls)]
                         (let [console-output @(ut/tracked-subscribe [::bricks/repl-output k])
                               repl?          (ut/ne? console-output)
                               dyn-color      (if (= k data-key)
                                                (theme-pull :theme/editor-background-color nil)
                                                (str (theme-pull :theme/editor-font-color nil) 77))]
                           [re-com/h-box :style
                            {:margin-left      "-5px"
                             :border-top       "1px solid #80008045"
                             :border-left      "1px solid #80008080"
                             :color            dyn-color
                             :background-color (if (= k data-key) 
                                                 ;"#9973e0" 
                                                 (theme-pull :theme/universal-pop-color "#9973e0")
                                                 "inherit")
                             :padding-right    "5px"
                             :padding-left     "5px"} :gap "4px" :children
                            [[re-com/box :padding "3px" :height "24px" :attr
                              {:on-click #(do (swap! db/data-browser-query assoc selected-block k)
                                              (when (and repl? (= k data-key))
                                                (swap! db/data-browser-query-con assoc
                                                  k
                                                  (let [b (get @db/data-browser-query-con k)] (if (nil? b) true (not b))))))}
                              :style
                              {:cursor        "pointer"
                               :font-size     "13px"
                               :border-bottom (when repl?
                                                (if (not (get @db/data-browser-query-con k))
                                                  (str "4px solid " dyn-color)
                                                  "inherit"))
                               :padding-left  "5px"
                               :padding-right "5px"} :child (str k)]
                             (when repl?
                               [re-com/md-icon-button :src (at) :md-icon-name "zmdi-code" :on-click
                                #(do (swap! db/data-browser-query assoc selected-block k)
                                     (swap! db/data-browser-query-con assoc
                                       k
                                       (let [b (get @db/data-browser-query-con k)] (if (nil? b) true (not b))))) :style
                                {:color         dyn-color
                                 :border-bottom (if (get @db/data-browser-query-con k) (str "4px solid " dyn-color) "inherit")
                                 :font-size     "15px"}])]]))])
                    
                    (when views?
                      [re-com/h-box :gap "7px" :padding "5px" :style {:font-weight 700 :color "#dddddd"} :children
                       (for [k (keys views)]
                         (let []
                           [re-com/h-box :style
                            {:margin-left      "-5px"
                             :border-top       "1px solid #80008045"
                             :border-left      "1px solid #80008080"
                             :color            (if (= k data-key)
                                                 (theme-pull :theme/editor-background-color nil) ;;"#000000"
                                                 (theme-pull :theme/editor-font-color nil))
                             :background-color (if (= k data-key) 
                                                 ;"#9973e0" 
                                                 (theme-pull :theme/universal-pop-color "#9973e0")
                                                 "inherit")
                             :padding-right    "5px"
                             :padding-left     "5px"} :gap "4px" :children
                            [[re-com/box :padding "3px" :attr
                              {:on-click #(do (swap! db/data-browser-query assoc selected-block k))} :style
                              {:cursor "pointer" :font-size "13px" :padding-left "5px" :padding-right "5px"} :child
                              ;(str "🍀" " " k)
                              (str k)]]]))])
                    
                    (when runners?
                      [re-com/h-box :gap "7px" :padding "5px" :style {:font-weight 700 :color "#dddddd"} :children
                       (let [runners-kps (vec (apply concat (for [[k v] runners] (for [vv (keys v)] [k vv]))))]
                         (for [k runners-kps]
                           (let [runner-icon (get-in runners (vec (conj k :icon)) "") ;; todo? as long as they match the theme..
                                 runner-color (get-in runners (vec (conj k :icon)) "cyan") ]
                             [re-com/h-box :style
                              {:margin-left      "-5px"
                               :border-top       "1px solid #80008045"
                               :border-left      "1px solid #80008080"
                               :color            (if (= (last k) data-key)
                                                   (theme-pull :theme/editor-background-color nil) ;;"#000000"
                                                   (theme-pull :theme/editor-font-color nil))
                               :background-color (if (= (last k) data-key) 
                                                   ;"#9973e0" 
                                                   (theme-pull :theme/universal-pop-color "#9973e0")
                                                   "inherit")
                               :padding-right    "5px"
                               :padding-left     "5px"} :gap "4px" :children
                              [[re-com/box :padding "3px" :attr
                                {:on-click #(do (swap! db/data-browser-query assoc selected-block (last k)))} :style
                                {:cursor "pointer" :font-size "13px" :padding-left "5px" :padding-right "5px"} :child
                                (str (last k))]]])))])
                    
                    ]]
                  
                  (let [combo-items {:view {:base-key :views
                                            :default [:box :child "heya" :align :center :justify :center :size "auto" :style
                                                      {:font-size "22px"}]}
                                     :query {:base-key :queries
                                             :default {:select [["heya, friendo" :greetings] [(rand-int 123) :count_stuff]]}}
                                     :code-query {:base-key :queries
                                                  :default {:select [:*]
                                                            :from
                                                            [{:data '(vec
                                                                      (for
                                                                       [i (range 45)]
                                                                        {:row_id i :type (rand-nth ["cat" "dog" "pizza"]) :name (str "row " i)}))}]}}}
                        runners-items (into {} (for [[k v] (-> block-runners-map (dissoc :views) (dissoc :queries))] 
                                                 {k {:base-key k  :default (get v :default)}}))
                        ;;_ (ut/tapp>> [:runners-items  runners runners-items combo-items])
                        combo-items (merge combo-items runners-items)]
                    
                    [re-com/single-dropdown 
                     :choices (vec (for [c (keys combo-items)] {:id c :label (str c)}))
                     :max-height "320px" 
                     :placeholder "add new slice" 
                     :style {:font-size "13px"} 
                     :model nil ;(get sql-params :user-dropdown-sys/req-field) 
                     :on-change #(ut/tracked-dispatch [::bricks/add-new selected-block (get-in combo-items [% :base-key])  (get-in combo-items [% :default])]) 
                     :width "125px"
                     ])
                  
                  ;; [re-com/h-box :gap "7px" :padding "5px"
                  ;;  :style {:font-weight 700
                  ;;          :color "#dddddd66"
                  ;;          :margin-top "3px"}

                  ;;  :children
                  ;;  (into
                  ;;   [[re-com/box :attr
                  ;;     {:on-click #(ut/tracked-dispatch [::bricks/add-new selected-block :queries
                  ;;                                       {:select [["heya, friendo" :greetings] [(rand-int 123) :count_stuff]]}])}
                  ;;     :style {:cursor "pointer"} :child "+query"]
                     
                  ;;    [re-com/box :attr
                  ;;     {:on-click
                  ;;      #(ut/tracked-dispatch
                  ;;        [::bricks/add-new selected-block :queries
                  ;;         {:select [:*]
                  ;;          :from
                  ;;          [{:data '(vec
                  ;;                    (for
                  ;;                     [i (range 45)]
                  ;;                      {:row_id i :type (rand-nth ["cat" "dog" "pizza"]) :name (str "row " i)}))}]}])} :style
                  ;;     {:cursor "pointer"} :child "+code"]
                     
                  ;;    [re-com/box :attr
                  ;;     {:on-click #(ut/tracked-dispatch [::bricks/add-new selected-block :views
                  ;;                                       [:box :child "heya" :align :center :justify :center :size "auto" :style
                  ;;                                        {:font-size "22px"}]])} :style {:cursor "pointer"} :child "+view"]
                  ;;    ]

                  ;;   (vec (for [[k v] block-runners-map
                  ;;              :when (not (cstr/includes? (str k) "clover"))]
                  ;;          [re-com/box :attr
                  ;;           {:on-click #(ut/tracked-dispatch [::bricks/add-new selected-block k ;:queries
                  ;;                                             (get v :default)])}
                  ;;           :style {:cursor "pointer" :opacity 0.5}
                  ;;           :child (str "+" (cstr/replace (str k) ":" ""))])))]
                   
                   ]]
                (if @db/bad-form?
                  [re-com/v-box :padding "12px" :style
                   {:color            (theme-pull :theme/editor-font-color nil)
                    :font-family      (theme-pull :theme/monospaced-font nil)
                    :font-weight      700
                    :background-color "#00000000"} :gap "10px" :children
                   [[re-com/h-box :size "auto" :children
                     [[re-com/md-icon-button :src (at) :md-icon-name "zmdi-chevron-left" :style
                       {:color (theme-pull :theme/editor-outer-rim-color nil) :font-size "31px" :margin-top "-5px"}]
                      [re-com/box :size "auto" :style {:color (theme-pull :theme/editor-outer-rim-color nil)} :child
                       (str "Issue with block code forms")]
                      [re-com/md-icon-button :src (at) :md-icon-name "zmdi-help" :tooltip "help?" :style
                       {:color (theme-pull :theme/editor-outer-rim-color nil) :font-size "15px" :opacity 0.22}]]]
                    [re-com/box :padding "8px" :size "auto" :child (str @db/bad-form-msg)]
                    [re-com/box :padding "8px" :style {:opacity 0.3} :size "auto" :child
                     "(Block cannot be saved until corrected)"]]]
                  (let [query-box? (or (some #(= % (get @db/data-browser-query selected-block)) (keys sql-calls))
                                       (some #(= % data-key) (keys sql-calls)))
                        solver-meta-spy?      (get-in @db/solver-meta-spy [selected-block data-key] false)
                        ;; runners-items (into {} (for [[k v] runners] {(first (first v)) {:base-key k}}))
                        ;;runners-items (into {} (for [[k [kk vv]]  runners]  {kk {:base-key k}}))
                        ;; _ (ut/tapp>> [:runners-items runners  (keys runners-items) (get @db/data-browser-query selected-block) ])
                        ;; runner-box? (or (some #(= % (get @db/data-browser-query selected-block)) (keys runners-items))
                        ;;                 (some #(= % data-key) (keys runners-items)))
                       ; is-layout? @(ut/tracked-subscribe [::bricks/is-layout? selected-block data-key])
                        viz-gen?   (= :viz-gen (get @db/data-browser-query selected-block))]
                    [re-com/v-box :children
                     [(cond query-box? [re-com/v-box :justify :between :children
                                        (if (get @db/data-browser-query-con data-key)
                                          [(let [repl-output (dissoc @(ut/tracked-subscribe [::bricks/repl-output data-key])
                                                                     :status)
                                                 console     (vec (remove empty? (get repl-output :out [])))]
                                             [re-com/v-box :padding "9px" :size "none" :height (px (- single-height 107)) :style
                                              {;:border "1px solid pink"
                                               :overflow "auto"} :children
                                              [(when (ut/ne? console)
                                                 [re-com/box :child "console output (limited to 50 items)"])
                                               (when (ut/ne? console)
                                                 [re-com/v-box :children (for [r console] [re-com/box :child (str r)])])
                                               [shape/map-boxes2 (dissoc repl-output :out) "no-block" "no-flow" [] "no-block"
                                                "map"]]])]
                                          [@(ut/tracked-subscribe [::bricks/sql-data-table-editor selected-block [data-key]
                                                                   single-width-bricks 6.05])
                                           (when (not @bricks/dragging?)
                                             [bricks/read-only-sql-box single-width (/ single-height 3.2) (str sql-string)])])]
                            solver-meta-spy? (let [are-solver        (get @db/solver-fn-lookup [:panels selected-block data-key])
                                                   meta-data          (when are-solver
                                                                        @(ut/tracked-sub ::conn/clicked-parameter-key-alpha
                                                                                         {:keypath [(keyword (str (ut/replacer are-solver
                                                                                                                               ":solver/" "solver-meta/")))]}))
                                                   running-status    (when are-solver
                                                                       @(ut/tracked-sub ::conn/clicked-parameter-key-alpha
                                                                                        {:keypath [(keyword (str (ut/replacer are-solver
                                                                                                                              ":solver/" "solver-status/*client-name*>")))]}))]
                                               [re-com/box :size "none" :style {:transform "translate(0)"}
                                                :height (px (- single-height 100))
                                                :style {:overflow "auto" :font-size "13px"}
                                                :child [re-com/v-box
                                                        :gap  "10px"
                                                        :children [[bricks/map-boxes2 {:meta meta-data} selected-block data-key [] :output nil]
                                                                   [bricks/map-boxes2 {:status running-status} selected-block data-key [] :output nil]]
                                                        ]])
                            ;; runner-box? [re-com/box :size "none" :style {:transform "translate(0)"}
                            ;;              :height (px (- single-height 100))
                            ;;              :child (str "render-loop for" data-key  " runner."  
                            ;;                          (get runners-items data-key) 
                            ;;                          (get block-runners-map [(get-in runners-items [data-key :base-key]) data-key]))]
                            viz-gen? ;[re-com/box :child (str (count mad-libs-combos) " rows. ")]
                            (let [;default-combo-view @(ut/tracked-subscribe
                                  src-table-id-str   (last (get selected-panel-map :mad-libs-combo-hash))
                                  combo-hash         (first (get selected-panel-map :mad-libs-combo-hash))
                                  opts               @(ut/tracked-subscribe [::bricks/get-mad-libs-options selected-block
                                                                             src-table-id-str combo-hash])
                                  shape-name         (get opts :shape_name)
                                  viz-shape          @(ut/tracked-subscribe [::bricks/get-viz-shape shape-name])
                                  default-shape-view (get viz-shape :selected_view)
                                  default-shape-view (try (when (ut/ne? default-shape-view)
                                                            (keyword (ut/replacer default-shape-view #":" "")))
                                                          (catch :default _ nil))]
                              (ut/tapp>> [:def-sel default-shape-view viz-shape])
                              [re-com/box :size "none" :style {:transform "translate(0)"}
                               :height (px (- single-height 100)) :child
                               [bricks/honeycomb selected-block (or default-shape-view :oz)]])
                            :else      [re-com/box :size "none" :style {:transform "translate(0)"}
                                        :height (px (- single-height 100))
                                        :child [bricks/honeycomb selected-block data-key]])
                      (if viz-gen? ;; dont show scrubber boolean if on viz-gen mode
                        [re-com/box :style {:font-size "10px" :margin-left "8px"} :child
                         (str "('viz-gen' debug - "
                              (count mad-libs-combos)
                              " c-rows, "
                              (count (distinct (map :axes mad-libs-combos)))
                              " axes, "
                              (count (distinct (map :combo_hash mad-libs-combos)))
                              " combos, "
                              " combo-hash: "
                              (get (first mad-libs-combos) :combo_hash)
                              ")")]
                        (let [view-scrubbers?       (get-in @db/scrubbers [selected-block data-key] false)
                              value-spy?            (get-in @db/value-spy [selected-block data-key] false)
                              solver-meta-spy?      (get-in @db/solver-meta-spy [selected-block data-key] false)
                              are-solver            (get @db/solver-fn-lookup [:panels selected-block data-key])
                              solver-running?       (when are-solver
                                                      @(ut/tracked-sub ::conn/clicked-parameter-key-alpha
                                                                       {:keypath [(keyword (str (ut/replacer are-solver 
                                                                                                             ":solver/" "solver-status/*client-name*>")  ">running?"))]}))]
                          ;; (ut/tapp>> [:solver-running? solver-running? are-solver (keyword (str (ut/replacer are-solver
                          ;;                                                                                    ":solver/" "solver-status/*client-name*>")  ">running?"))])
                          ;;;(ut/tapp>> [:dd @db/solver-fn-lookup])

                          [re-com/h-box
                           :style {:font-size   "11px"
                                   ;:z-index 29788
                                   :padding-right "14px"
                                   :font-weight 700}
                           :justify :between
                           :children [[re-com/h-box :gap "10px" :children
                                       [[re-com/box :size "none" :width "90px" :child (if view-scrubbers? "scrubber on" "scrubber off") :attr
                                         {:on-click #(swap! db/scrubbers assoc-in [selected-block data-key] (not view-scrubbers?))} :style
                                         {:color       (if view-scrubbers? (theme-pull :theme/universal-pop-color nil) "grey")
                                          :z-index     100
                                          :user-select "none"
                                          :margin-top  (if query-box? "9px" "inherit")
                                          :cursor      "pointer"}]
                                        [re-com/box :size "none" :width "90px" :child "value spy" :attr
                                         {:on-click #(swap! db/value-spy assoc-in [selected-block data-key] (not value-spy?))} :style
                                         {:color           (if value-spy? (theme-pull :theme/universal-pop-color nil) "grey")
                                          :z-index         100
                                          :user-select     "none"
                                          :text-decoration (when (not value-spy?) "strikethrough")
                                          :margin-top      (if query-box? "9px" "inherit")
                                          :cursor          "pointer"}]
                                        (when are-solver
                                          [re-com/box :size "none" :width "90px" :child "solver meta" :attr
                                           {:on-click #(swap! db/solver-meta-spy assoc-in [selected-block data-key] (not solver-meta-spy?))} :style
                                           {:color           (if solver-meta-spy? (theme-pull :theme/universal-pop-color nil) "grey")
                                            :z-index         100
                                            :user-select     "none"
                                            :text-decoration (when (not solver-meta-spy?) "strikethrough")
                                            :margin-top      (if query-box? "9px" "inherit")
                                            :cursor          "pointer"}])]]

                                      (when are-solver 
                                        [re-com/h-box 
                                         :gap  "6px"
                                         :style {:color (if solver-running? (theme-pull :theme/universal-pop-color nil) "grey")}
                                         :children 
                                         [(when solver-running?
                                            [re-com/md-icon-button :md-icon-name "zmdi-refresh" :class "rotate linear infinite"
                                             :style
                                             {:font-size "15px" :transform-origin "7.5px 12px"
                                              :margin-top "-4px"}])
                                          [re-com/box :child (str are-solver)]]]
                                        )]]
                            ))]]))]] :height (px (- ttl-height 24)) :width single-width-px :style
              {:overflow "hidden"}])
         system-panel?        [re-com/box :size "none" :child
                               [re-com/v-box :size "1" :children
                                [[re-com/box :padding "4px" :child (str "(fields, data-types)") :style
                                  {:font-weight 500
                                   :color       (theme-pull :theme/editor-font-color nil)
                                   :background  (str "linear-gradient("
                                                     (theme-pull :theme/editor-rim-color nil)
                                                     ", transparent)")}]
                                 [re-com/box :style {:padding-top "10px"} :child
                                  (condp = @db/editor-mode ; @file-mode?
                                    :files  [editor-panel-metadata-ext-files]
                                    :kits   [editor-panel-metadata-ext-kits]
                                    :params [editor-panel-metadata-params single-width single-height :theme]
                                    :meta   [editor-panel-metadata-ext]
                                    :viz    [editor-panel-metadata-viz] ;[re-com/box :child "poop"]
                                    :search [search-panel-metadata-ext] ;;[search-panel-right
                                                                        ;;single-width
                                    :vvv    [re-com/box :size "none" :child " " :width "0px"]
                                    :status [editor-panel-metadata-status] ;[editor-panel-status]
                                  ) :height (px (- ttl-height 40))]]] :height single-height-px :width
                               (if (= @db/editor-mode :vvv) "0px" single-width-px) :style
                               {;:border "1px solid white" :background-color "#1f2430"
                                :overflow "hidden"}])
       [re-com/v-box :children
        [(if (and (or (= @db/editor-mode :viz) (= @db/editor-mode :vvv))
                  reco-selected?
                  (or (= selected-block "none!") (nil? selected-block)))
           [re-com/v-box :size "auto" :height (px (- single-height 80)) :children
            [[re-com/box :style
              {:font-weight      500
               :color            (theme-pull :theme/editor-font-color nil)
               ;:background-color (theme-pull :theme/editor-rim-color "#a3a3a3")
               :padding-top      "4px"
               :padding-bottom   "4px" ;:margin-left "2px"
              } :child "reco preview"]
             [re-com/v-box :size "auto" :width (px (* single-width 0.95)) :gap "6px" :style
              {:margin-top "30px"
               :transform  "translate(0)" ;; a known CSS hack for fixed position in non fixed
               :overflow   "auto"} :children
              [;[re-com/box :child (str reco-selected)]
               [bricks/honeycomb :reco-preview :oz]]]]]
           [re-com/h-box :size "auto" :height (px (- single-height 20)) :children
            [[re-com/v-box :size "none" :width (px (* single-width 0.6)) :children
              [[re-com/box :style
                {:font-weight    500
                 :color          (theme-pull :theme/editor-font-color nil)
                 :background     (str "linear-gradient(" (theme-pull :theme/editor-rim-color nil) ", transparent)")
                 :padding-top    "4px"
                 :padding-bottom "4px" ;:margin-left "2px"
                } :child
                [re-com/h-box :width (px (* single-width 0.538)) :justify :between :children
                 [[re-com/box :child "parameters"]
                  [re-com/h-box :gap "3px" :style {:font-size "11px" :margin-top "3px"} :children
                   (vec (for [f (keys @db/param-filter)]
                          [re-com/box :child (ut/safe-name f) :attr
                           {:on-click #(swap! db/param-filter assoc f (not (get @db/param-filter f false)))} :style
                           {:text-decoration (if (get @db/param-filter f true) "none" "line-through")
                            :opacity         (if (get @db/param-filter f true) 1 0.4)
                            :cursor          "pointer"
                            :user-select     "none"
                            :padding-left    "4px"
                            :padding-right   "4px"}]))]]]]
               [bricks/click-param-browser click-params (* single-width 0.6) (- single-height 1.3)]]]
             [re-com/v-box :size "none" :width (px (* single-width 0.379)) :children
              [[re-com/h-box :style
                {:font-weight    500
                 :color          (theme-pull :theme/editor-font-color nil)
                 :background     (str "linear-gradient(" (theme-pull :theme/editor-rim-color nil) ", transparent)")
                 :border-radius  "0px 11px 0px 0px"
                 :padding-top    "4px"
                 :padding-bottom "4px"
                 ;:margin-left    "-21px"
                 } 
                
                :children
                [[re-com/h-box :gap "8px" 
                  :width (px (- (* single-width 0.405) 18)) 
                  :justify :end
                  :align :center ;:size "auto"
                  ;:style {:padding-right "8px"}
                  :children
                  (conj 
                   (vec (for [b    ["queries" "blocks"]
                        :let [selected? (= (keyword b) @db/item-browser-mode)]]
                    [re-com/box :attr {:on-click #(reset! db/item-browser-mode (keyword b))} 
                     :style
                     (if selected?
                       {:user-select "none" :opacity 1.0}
                       {:cursor "pointer" :user-select "none" :opacity 0.3}) :child
                     (str b)]))
                   [re-com/md-icon-button :md-icon-name "zmdi-window-minimize" :on-click
                    #(ut/tracked-dispatch [::bricks/toggle-editor]) :style {:font-size "15px" :opacity 0.33 :cursor "pointer"}]
                   )]
                 ;[re-com/gap :size "12px"]
                 ]]
               [re-com/box :style {:padding-right "3px"} :child
                (cond (= @db/item-browser-mode :queries) [bricks/screen-query-browser (* single-width 0.379)
                                                          (- single-height 1.3)]
                      (= @db/item-browser-mode :blocks)  [bricks/screen-block-browser (* single-width 0.379)
                                                          (- single-height 1.3)]
                      :else                              [re-com/box :child "nothing selected above?"])]]]] :height
            single-height-px :width single-width-px :style
            {;:border "1px solid white" :background-color "#1f2430"
             :overflow "hidden"}])
         [re-com/h-box :height "42px" :width (px (- (* single-width 0.98) 1)) :size "none" :align :center :justify :between :gap
          "9px" :children
          [(if (not (and (= @db/editor-mode :viz) reco-selected?))
             [re-com/h-box ;:padding "8px"
              :justify :between :align :center :size "auto" :children
              [;[re-com/box
               [re-com/input-text :src (at) :model (str screen-name) :width "487px" :height "30px" :on-change
                #(ut/tracked-dispatch [::bricks/set-screen-name %]) :validation-regex screen-name-regex :change-on-blur? true
                :style
                {:font-size        "20px"
                 :font-style       "underline"
                 :border           "0px solid black"
                 :padding          "8px"
                 :background-color "#00000000"
                 :color            (theme-pull :theme/editor-font-color nil)}]] :style
              {:color (theme-pull :theme/editor-font-color nil) :border-radius "7px"}]
             [re-com/box :align :center :justify :center :child (str "viz preview: " reco-combo) :style
              {:font-size        "16px"
               :font-weight      700
               :font-style       "underline"
               :border           "0px solid black"
               :padding          "8px"
               :margin-top       "-14px"
               :background-color "#00000000"
               :color            (str (theme-pull :theme/editor-font-color nil) 66) ;; "#ffffff66"
              }])] :style
          {:padding-left   "8px"
           :padding-top    "12px"
           :padding-bottom "10px"
           :padding-right  "10px"
           :border-radius  "10px 0px 10px 0px"
           :box-shadow     "0px -5px 5px 0px #00000099"
           :font-weight    500
           :color          (theme-pull :theme/editor-background-color nil) ;; "#000000"
           :background     (str "linear-gradient(" (theme-pull :theme/editor-rim-color nil) ", transparent)")}]]]] :width
      ttl-width-px :height ttl-height-px :margin "-2px" :style
      {:position         "fixed"
       :top              y-px
       :left             x-px
       :border-radius    "16px"
       :z-index          100
       :border           (str "6px solid " (theme-pull :theme/editor-outer-rim-color nil)) ; #b7e27c"
       :filter           "drop-shadow(0.35rem 0.35rem 0.4rem rgba(0, 0, 0, 0.8))"
       :background-color (str (theme-pull :theme/editor-background-color nil) 77) ; "#000000"
       :backdrop-filter  "blur(5px)"}]]))

(dnd2/subscribe! (js/document.querySelector "div")
                 :canvas02
                 {:drop (fn [_ files]
                          (when (not (= ""
                                        (-> files
                                            .-value)))
                            (let [^js/File file0 (-> files
                                                     (aget 0))
                                  fname          (-> file0
                                                     .-name)]
                              (ut/tapp>> [:run? (cstr/lower-case fname)])
                              (if (cstr/ends-with? (cstr/lower-case fname) ".csv")
                                (do (ut/tapp>> [:saving-csv-to-server fname])
                                    (ut/read-file file0 #(ut/tracked-dispatch [::http/save-csv fname (str %)]))
                                    (set! (-> files
                                              .-value)
                                          ""))
                                (ut/tapp>> [:invalid-file fname])))))})

(defn ifnil [x n] (if (nil? x) n x))

(defonce title-edit-idx (reagent/atom nil))

(defn tab-menu
  []
  (let [;tabs @(ut/tracked-subscribe [::bricks/tabs])
        tabs @(ut/tracked-subscribe [::bricks/visible-tabs])
        hidden-tabs @(ut/tracked-subscribe [::bricks/hidden-tabs])
        selected-tab @(ut/tracked-subscribe [::bricks/selected-tab])
        react-hack [@db/show-tabs? @title-edit-idx] ;; important
        tab-row
          (fn [top tabs & [hidden?]]
            [re-com/h-box :attr
             {:on-mouse-over  #(when (not @bricks/over-block?) (reset! bricks/over-block? true))
              :on-mouse-leave #(do (reset! bricks/over-block? false))} :children
             (conj
               (vec
                 (conj
                   (when (or (not hidden?) (and hidden? @db/show-hidden-tabs?))
                     (for [t    tabs
                           :let [selected? (= selected-tab t)
                                 tab-box [re-com/box :child
                                          [re-com/box :child
                                           (if (not @db/show-tabs?)
                                             (str (try (.indexOf tabs t) (catch :default _ "e")))
                                             (if (= @title-edit-idx t)
                                               [re-com/h-box :children
                                                [[re-com/input-text :model (str t) :on-change
                                                  #(do (ut/tapp>> [:changed-tab-name (str t) :to (str %)])
                                                       (when (and (ut/ne? (cstr/trim (str %))) (not (some (fn [x] (= x %)) tabs)))
                                                         (ut/tracked-dispatch [::bricks/rename-tab (str t) (str %)])) ;; dont
                                                       (reset! title-edit-idx nil)) :change-on-blur? true :width
                                                  (px (+ 15 (* (count (str t)) 11))) ;"inherit" ;
                                                  :style
                                                  {:background-color "#00000000"
                                                   :text-decoration  "underline"
                                                   :border           "none"
                                                   :padding-left     "0px"
                                                   :color            (theme-pull :theme/editor-outer-rim-color nil)}]
                                                 [re-com/md-icon-button :src (at) :md-icon-name "zmdi-delete" :tooltip
                                                  "delete this tab" :style
                                                  {:color       (theme-pull :theme/editor-outer-rim-color nil)
                                                   :font-size   "14px"
                                                   :padding-top "5px"
                                                   :width       "10px"
                                                   :height      "20px"} :attr
                                                  {:on-click #(ut/tracked-dispatch [::bricks/delete-tab (str t)])}]]]
                                               (str t))) :size "none" :align :center :justify (when (not @db/show-tabs?) :center)
                                           :padding "5px" :width (when (not @db/show-tabs?) "5px") :style
                                           {:transition_NOT "all 0.2s ease-in-out" :backdrop-filter "blur(8px)"}] :attr
                                          (merge (when (not (= @title-edit-idx t))
                                                   {:on-click #(do (ut/tracked-dispatch [::bricks/select-tab (str t)])
                                                                   (reset! title-edit-idx nil))})
                                                 (if selected? {:on-double-click #(reset! title-edit-idx (str t))} {})
                                                 {:on-context-menu #(ut/tracked-dispatch [::bricks/toggle-tab-visibility
                                                                                          (str t)])}) :style
                                          {:cursor           "pointer"
                                           :background-color (theme-pull :theme/base-block-color-selected nil)
                                           :color            (theme-pull :theme/editor-outer-rim-color nil)
                                           :opacity          (if selected? 1.0 0.4)
                                           :backdrop-filter  "blur(8px)"
                                           :padding-left     "5px"
                                           :padding-right    "5px"
                                           :height           "30px"
                                           :border           (when (not hidden?)
                                                               (if (= @title-edit-idx t)
                                                                 (str "3px dashed "
                                                                      (theme-pull :theme/block-tab-selected-font-color nil))
                                                                 (str "1px solid "
                                                                      (theme-pull :theme/block-tab-selected-font-color nil))))}]]]
                       (if (not selected?)
                         (bricks/draggable (let [[_ _ w h] @(ut/tracked-subscribe [::bricks/tab-recenter t])]
                                             {:w w :selected-view :vv :name (str t) :h h :ghosted? false :views {:vv [:grid t]}})
                                           "meta-menu"
                                           tab-box)
                         tab-box)))
                   (if hidden?
                     [re-com/md-icon-button :src (at) :md-icon-name (if @db/show-hidden-tabs? "zmdi-tab" "zmdi-tab-unselected")
                      :style
                      {:background-color "#00000099" ;(theme-pull
                       :color            (theme-pull :theme/editor-outer-rim-color nil)
                       :backdrop-filter  "blur(2px)"
                       :border-radius    (when (not @db/show-hidden-tabs?) "0px 0px 8px 0px")
                       :padding-left     "4px"
                       :margin-left      "-3px"
                       :font-size        "20px"
                       :padding-top      "3px"
                       :width            "30px"
                       :height           "30px"} :attr {:on-click #(reset! db/show-hidden-tabs? (not @db/show-hidden-tabs?))}]
                     [re-com/md-icon-button :src (at) :md-icon-name (if @db/show-tabs? "zmdi-chevron-left" "zmdi-chevron-right")
                      :style
                      {:background-color (theme-pull :theme/base-block-color-selected nil)
                       :color            (theme-pull :theme/editor-outer-rim-color nil)
                       :padding-left     "4px"
                       :margin-left      "-3px"
                       :font-size        "20px"
                       :padding-top      "3px"
                       :width            "30px"
                       :height           "30px"} :attr {:on-click #(reset! db/show-tabs? (not @db/show-tabs?))}])))
               (when (and @db/show-tabs? (not hidden?))
                 [re-com/md-icon-button :src (at) :md-icon-name "zmdi-plus" :tooltip "add new tab" :style
                  {:color        (theme-pull :theme/editor-outer-rim-color nil)
                   :padding-left "4px"
                   :margin-left  "-3px"
                   :font-size    "20px"
                   :padding-top  "3px"
                   :width        "30px"
                   :height       "30px"} :attr
                  {:on-click #(let [new-tab (ut/make-tab-name)]
                                (ut/tracked-dispatch [::bricks/add-tab new-tab])
                                (ut/tracked-dispatch [::bricks/select-tab new-tab]))}])) :height "30px" :style
             {:position        "fixed"
              :top             top
              :left            0
              :z-index         999
              :backdrop-filter "blur(8px)"
              :user-select     "none"
              :transition_NOT  "all 0.2s ease-in-out"
              :transform-style "preserve-3d"}])]
    [re-com/v-box :children [[tab-row 0 tabs] (when (ut/not-empty? hidden-tabs) [tab-row 30 hidden-tabs true])]]))

(defn snapshot-menu
  []
  (let [;tabs @(ut/tracked-subscribe [::bricks/tabs])
        matching   @(ut/tracked-subscribe [::bricks/matching-snapshots])
        snapshots  @(ut/tracked-subscribe [::bricks/snapshots])
        tabs       (vec (map :key (sort-by :key (vec (for [[k v] snapshots :when (get v :menu?)] (assoc v :key k))))))
        react-hack [@db/show-snaps? @db/vertical-snaps?] ;; important
       ]
    (when (ut/ne? tabs)
      [(if @db/vertical-snaps? re-com/v-box re-com/h-box) :children
       (reverse
         (conj
           (vec
             (conj
               (for [t (reverse tabs)]
                 [re-com/box :child
                  [re-com/box :child (if (not @db/show-snaps?) (str (try (.indexOf tabs t) (catch :default _ "err!"))) (str t))
                   :size "none" :align :center :justify (when (not @db/show-snaps?) :center) :padding "5px" :width
                   (when (not @db/show-snaps?) "5px") :attr {:on-click #(ut/tracked-dispatch [::bricks/swap-snapshot t])} :style
                   {:transition_NOT "all 0.2s ease-in-out" :backdrop-filter "blur(8px)"}] :style
                  {;:transform "rotate(90deg)"
                   :cursor           "pointer"
                   :background-color (theme-pull :theme/base-block-color-selected nil)
                   :color            (theme-pull :theme/editor-outer-rim-color nil)
                   :opacity          (if ;(= t selected-tab)
                                       (some #(= t %) matching)
                                       1.0
                                       0.4)
                   :backdrop-filter  "blur(8px)"
                   :padding-left     "5px"
                   :padding-right    "5px"
                   :height           "30px" ;:width (px (+ 0 (* (count (str t)) 11)))
                   :border           (str "1px solid " (theme-pull :theme/block-tab-selected-font-color nil))}])
               [(if (and (not @db/show-snaps?) @db/vertical-snaps?) re-com/v-box re-com/h-box) :align :end :justify :end :children
                ((if (and (not @db/show-snaps?) @db/vertical-snaps?) reverse vec)
                  [[re-com/md-icon-button :src (at) :md-icon-name (if @db/vertical-snaps? "zmdi-chevron-up" "zmdi-chevron-down")
                    :tooltip "minimize tab menu" :style
                    {:background-color (theme-pull :theme/base-block-color-selected nil)
                     :color            (theme-pull :theme/editor-outer-rim-color nil)
                     :padding-left     "4px"
                     :margin-left      "-3px"
                     :font-size        "20px"
                     :border-radius    (when @db/vertical-snaps? "0px 0px 0px 13px")
                     :padding-top      "3px"
                     :width            "30px"
                     :height           "30px"} :attr {:on-click #(reset! db/vertical-snaps? (not @db/vertical-snaps?))}]
                   [re-com/md-icon-button :src (at) :md-icon-name (if @db/show-snaps? "zmdi-chevron-right" "zmdi-chevron-left")
                    :tooltip "minimize tab menu" :style
                    {;:transform (when @db/show-tabs? "rotate(90deg)")
                     :background-color (theme-pull :theme/base-block-color-selected nil)
                     :color            (theme-pull :theme/editor-outer-rim-color nil)
                     :padding-left     "4px"
                     :margin-left      "-3px"
                     :font-size        "20px"
                     :padding-top      "3px"
                     :width            "30px"
                     :height           "30px"} :attr {:on-click #(reset! db/show-snaps? (not @db/show-snaps?))}]])]))
           (when (and @db/show-snaps? false)
             [re-com/md-icon-button :src (at) :md-icon-name "zmdi-plus" :tooltip "add new tab" :style
              {;:transform (when @db/show-tabs? "rotate(90deg)")
               :color        (theme-pull :theme/editor-outer-rim-color nil)
               :padding-left "4px"
               :margin-left  "-3px"
               :font-size    "20px"
               :padding-top  "3px"
               :width        "30px"
               :height       "30px"}]))) :style
       {:position        "fixed"
        :top             0
        :right           0
        :z-index         999
        :backdrop-filter "blur(8px)"
        :user-select     "none"
        :transition_NOT  "all 0.2s ease-in-out"
        :transform-style "preserve-3d"}])))

(def on-canvas? (reagent/atom false))

(defn mouse-move-handler2
  [offset]
  (fn [evt]
    (let [start-x (.-clientX evt)
          start-y (.-clientY evt)
          off-x   (:x offset)
          off-y   (:y offset)
          x       (- start-x off-x) ;
          y       (- start-y off-y)]
      (ut/tapp>> [:newey x y @on-canvas?]))))

(defn mouse-up-handler2 [on-move] (fn me [evt] (do (gevents/unlisten js/window EventType.MOUSEMOVE on-move))))

(defn mouse-down-handler2
  [e]
  (let [{:keys [left top]} (bricks/get-client-rect e)
        offset             {:x (- (.-clientX e) left) :y (- (.-clientY e) top)}
        on-move            (mouse-move-handler2 offset)]
    (do (gevents/listen js/window EventType.MOUSEMOVE on-move))
    (gevents/listen js/window EventType.MOUSEUP (mouse-up-handler2 on-move))))


(defn render-icon
  [icon]
  (if (and (ut/ne? icon) (not (nil? icon)))
    (if (cstr/includes? icon "zmdi")
      [re-com/md-icon-button :src (at) :md-icon-name icon :style
       {;:color bcolor :cursor "grab"
        :font-size "15px"} :attr {}]
      [re-com/box :size "none" :height "24px" :child [:img {:src icon :width "100%"}]])
    " "))

(defonce temp-atom (atom {}))

(defn create-clover-keys-from-data
  [data prefix] ;; prefix as in "theme/"  etc.
  (let [kw-tag (keyword (str (ut/replacer prefix "/" "") "-autocomplete-gen"))
        resolved-data (resolver/logic-and-params data kw-tag)] ;; if it was a fn, we need the result kp, not the request kp...
    (vec (distinct (flatten (for [[k v] resolved-data]
                              (if (or (map? v) (vector? v))
                                (conj (for [ee (ut/kvpaths v)]
                                        (str (keyword (ut/replacer (str prefix k ">" (cstr/join ">" ee)) ":" ""))))
                                      (str (keyword (str prefix (ut/unkeyword k)))))
                                (str (keyword (str prefix (ut/unkeyword k)))))))))))

(re-frame/reg-event-db ::update-user-params-hash
                       (fn [db _]
                         (let [fs                (vec (for [kk   (get db :flow-subs)
                                                            :let [[f1 f2] (ut/splitter (ut/replacer (str kk) ":" "") "/")]]
                                                        [(keyword f1) (keyword f2)]))
                               pp                (get db :click-param)
                               pp-without-fs     (ut/remove-keys pp
                                                                 (into (map first fs)
                                                                       [:flow :time :server :flows-sys :client :solver
                                                                        :signal-history :solver-meta nil]))
                               new-autocompletes (vec (into (create-clover-keys-from-data (get pp-without-fs :param) "param/")
                                                            (create-clover-keys-from-data (get pp-without-fs :theme) "theme/")))
                               new-h             (hash pp-without-fs)
                               client-name       (get db :client-name)]
                           ;; (ut/tapp>>  [:update-user-params-hash-event (count new-autocompletes) new-autocompletes])
                           (reset! db/autocomplete-keywords (set (into new-autocompletes @db/autocomplete-keywords))) ;; dont
                           (ut/tracked-dispatch [::wfx/request :default ;; just a push, no response handling
                                                 {:message {:kind        :sync-client-params
                                                            :params-map  pp-without-fs ;; pp ;; why send things we
                                                            :client-name client-name}
                                                  :timeout 10000}])
                           (reset! temp-atom pp-without-fs)
                           (assoc db :user-params-hash new-h))))

(re-frame/reg-sub ::watch-user-params
                  (fn [db]
                    (let [fs            (vec (for [kk   (get db :flow-subs)
                                                   :let [[f1 f2] (ut/splitter (ut/replacer (str kk) ":" "") "/")]]
                                               [(keyword f1) (keyword f2)]))
                          pp            (get db :click-param)
                          pp-without-fs (ut/remove-keys pp
                                                        (into (map first fs)
                                                              [:flow :time :server :flows-sys :client :solver :signal-history
                                                               :solver-meta nil]))]
                      (hash pp-without-fs)))) ;; was :param

(re-frame/reg-sub ::user-params-hash (fn [db] (get db :user-params-hash)))

(re-frame/reg-sub ::rs-overrides-hashmap (fn [db] (get db :rs-overrides-hashmap)))

(re-frame/reg-event-db ::set-rs-overrides-hashmap (fn [db [_ new]] (assoc db :rs-overrides-hashmap new)))

(re-frame/reg-sub ::theme-colors-hashmap (fn [db] (get db :theme-colors-hashmap)))

(re-frame/reg-event-db ::set-theme-colors-hashmap (fn [db [_ new]] (assoc db :theme-colors-hashmap new)))

(re-frame/reg-event-db ::rename-tab
                       (fn [db [_ old new]]
                         (-> db
                             (assoc :panels (ut/update-nested-tabs (get db :panels) old new))
                             (assoc :tabs (vec (map (fn [item] (if (= item old) new item)) (get db :tabs))))
                             (assoc :selected-tab new))))

(re-frame/reg-sub ::view-data (fn [db [_ kp]] (get-in db (into [:panels (get db :selected-block)] kp))))

(def panels [:flow? :buffy? :editor?])

(re-frame/reg-sub ::minimized-system-panels (fn [db _] (vec (for [p panels :when (not (get db p))] [p p]))))

(re-frame/reg-event-db ::toggle-sys-panel (fn [db [_ kkey]] (assoc db kkey (not (get db kkey)))))

(re-frame/reg-sub ::is-fire-on-change? (fn [db [_ flow-id]] (get-in db [:runstreams flow-id :fire?] false)))

(defn run-flow
  [flow-id overrides] ;; same as honeycomb logic w/o the render obvs
  (if @(ut/tracked-subscribe [::is-fire-on-change? flow-id])
    (let [client-name  @(ut/tracked-subscribe [::bricks/client-name])
          base-opts    {:increment-id? false}
          running-key  (keyword (str "flow/" flow-id ">*running?"))
          running?     @(ut/tracked-subscribe [::conn/clicked-parameter-key [running-key]])
          runstreamed? (= overrides :runstream-overrides)
          overrides    (if runstreamed? @(ut/tracked-subscribe [::bricks/runstream-overrides flow-id]) overrides)
          overrides?   (ut/ne? overrides)]
      (when (not running?) ;(not (some (fn [x] (= x text)) @db/speech-log))
        (let [fstr (str "changed! running flow " flow-id (when overrides? " (with overrides)"))
              w    (/ (count fstr) 4.1)]
          (ut/tracked-dispatch
            [::wfx/request :default
             {:message     {:kind        :run-flow
                            :flow-id     flow-id
                            :flowmap     flow-id
                            :opts        (if (map? overrides) (merge base-opts {:overrides overrides}) base-opts)
                            :client-name client-name}
              :on-response [::http/socket-response]
              :on-timeout  [::http/timeout-response :views/run-flow]
              :timeout     50000000}])
          (ut/dispatch-delay 800 [::http/insert-alert fstr w 1 5]))))
    (ut/tapp>> [:no-fire-on-change flow-id :skipping])))

(defn task-bar
  []
  (try
    (let [min-panels              @(ut/tracked-subscribe [::bricks/all-panels-minimized])
          user-param-hash2        @(ut/tracked-subscribe [::watch-user-params])
          selected-view           @(ut/tracked-subscribe [::bricks/editor-panel-selected-view])
          sselected-view          @(ut/tracked-subscribe [::conn/clicked-parameter [:param :selected-view]])
          editor?                 @(ut/tracked-subscribe [::bricks/editor?])
          user-param-hash1        @(ut/tracked-subscribe [::user-params-hash])
          minimized-system-panels @(ut/tracked-subscribe [::minimized-system-panels])
          audio-playing?          false ;@(ut/tracked-subscribe [::audio/audio-playing?])
          rs-overrides            @(ut/tracked-subscribe [::bricks/runstream-override-map])
          rs-overrides-hashmap    @(ut/tracked-subscribe [::rs-overrides-hashmap])
          theme-colors-hashmap    @(ut/tracked-subscribe [::theme-colors-hashmap])
          theme-colors            (theme-pull :theme/data-colors db/data-colors)
          min-panels              (vec (into min-panels minimized-system-panels))]
      (when (not= rs-overrides rs-overrides-hashmap) ;; execute flows when mutated
        (do (ut/tapp>> [:runstream-overides-change! rs-overrides rs-overrides-hashmap])
            (doseq [[k v] rs-overrides
                    :when (not= (get rs-overrides-hashmap k) v)]
              (ut/tapp>> [:runstream-override! k v])
              (when (ut/ne? rs-overrides-hashmap) ;; on first run, dont want to trigger all...
                (run-flow k v))))
        (ut/tracked-dispatch [::set-rs-overrides-hashmap rs-overrides]))
      (when (not= theme-colors theme-colors-hashmap) ;; execute flows when mutated
        (ut/apply-theme (bricks/code-mirror-theme))
        (ut/tracked-dispatch [::set-theme-colors-hashmap theme-colors]))
      (when true ; some atom to short-circuit this if needed later.. (putting it here since
        (doall (when (not (= user-param-hash1 user-param-hash2)) ;; core update for blind
                 (ut/tracked-dispatch [::update-user-params-hash]))))
      (when (and editor? (not (= selected-view sselected-view)))
        (let [view-data @(ut/tracked-subscribe [::view-data selected-view])]
          (ut/tracked-dispatch [::conn/click-parameter [:param :selected-view] selected-view])
          (ut/tracked-dispatch [::conn/click-parameter [:param :selected-view-data] view-data])))
      (when (ut/not-empty? min-panels)
        [re-com/h-box :style {:position "fixed" :bottom 0 :left "50%" :z-index 999 :transform "translateX(-50%)"} :children
         (for [[e name] min-panels
               :let     [nn         (try (if (empty? name) e name) (catch :default _ e))
                         sys-panel? (true? (some #(= name %) panels))
                         nn         (if sys-panel?
                                      (-> (str nn)
                                          (ut/replacer ":" "")
                                          (ut/replacer "?" ""))
                                      nn)
                         icon       (if sys-panel? "zmdi-developer-board" @(ut/tracked-subscribe [::bricks/panel-icon e]))]]
           [re-com/h-box :align :center :justify :center :height "30px" :gap "6px" :attr
            {:on-click #(if sys-panel?
                          (ut/tracked-dispatch [::toggle-sys-panel e])
                          (ut/tracked-dispatch [::bricks/toggle-minimize-block e]))} :style
            {:background-color "#00000099"
             :padding-left     "7px"
             :border           (str "1px solid " (theme-pull :theme/editor-outer-rim-color nil))
             :padding-right    "7px"
             :cursor           "pointer"
             :color            (theme-pull :theme/editor-outer-rim-color nil)
             :backdrop-filter  "blur(2px)"} :children [[re-com/box :child (str nn)] [render-icon icon]]])]))
    (catch :default e
      (do (ut/tapp>> [:TASK-BAR-ERROR! (str e) (.-message e) :stack (.-stack e)])
          (js/console.log (str :TASK-BAR-ERROR! (str e) (.-message e) :stack (.-stack e)))))))

(re-frame/reg-event-db ::set-session-loading (fn [db [_ session]] (assoc db :loading-session session)))

(re-frame/reg-sub ::session-loading (fn [db _] (get db :loading-session)))

(defn image-component
  [s t h]
  (let [timestamp (reagent/atom (js/Date.now))]
    (js/setInterval #(reset! timestamp (js/Date.now)) 5000) ; refresh every 5 seconds
    (fn []
      (let [url         (str "snaps/" (ut/replacer (str s) ".edn" ".jpg") "?timestamp=" @timestamp)
            client-name @(ut/tracked-subscribe [::bricks/client-name])
            client-name (ut/replacer (str client-name) ":" "")
            s-loading   @(ut/tracked-subscribe [::session-loading])
            current?    (= (ut/replacer (str s) ".edn" "") client-name)
            loading?    (= s s-loading)]
        [re-com/v-box :size "auto" :style
         {:cursor        "pointer"
          :border-radius "8px"
          :border        (cond loading? (str "3px dashed " (theme-pull :theme/editor-outer-rim-color nil) 88)
                               current? (str "3px solid " (theme-pull :theme/editor-outer-rim-color nil))
                               :else    "3px solid #00000000")} :attr
         {:on-click #(do (ut/tracked-dispatch [::set-session-loading s])
                         (ut/tracked-dispatch [::http/load-session (str "./snaps/" s)]))} :children
         [[re-com/box :size "auto" :child
           [:img {:src url :style {:max-width "100%" :max-height "100%" :border-radius "8px" :object-fit "contain"}}]]
          [re-com/h-box :size "auto" :align :end :justify :between :height "10px" :style
           {;:padding-right "5px"
            :padding-left  "4px"
            :margin-bottom "3px"
            :font-weight   700
            :font-size     "9px"} :children
           [[re-com/box :size "auto" :child (str t " ago")]
            (if loading?
              [re-com/md-icon-button :md-icon-name "zmdi-refresh" :class "rotate linear infinite" :style
               {:font-size "15px" :transform-origin "7.5px 12px" :margin-bottom "-8px"}]
              [re-com/gap :size "10px"])]]]]))))

(defn session-modal
  []
  (let [hh          @(ut/tracked-subscribe [::subs/h])
        ww          @(ut/tracked-subscribe [::subs/w])
        per-page    9
        w           (* ww 0.44) ;500
        h           (* hh 0.5) ;500
        client-name @(ut/tracked-subscribe [::bricks/client-name]) ;; reactions
        sessions    @(ut/tracked-subscribe [::bricks/sessions])
        left        (- (/ ww 2) (/ w 2))
        top         (- (/ hh 2) (/ h 2))
        items       (count sessions)
        items       (if (odd? items) items (inc items))
        sessions    (take per-page (sort-by last sessions))
        sessions    (partition-all 3 sessions)]
    [re-com/modal-panel :style
     {:z-index 99999 ;; on top of alerts?
      :padding "0px"} :frame-style {:background-color "#000000"} :parts
     {:child-container
        {:style {:left left :transform "scale(1.5)" :top top :font-size "8px" :background-color "#000000" :position "fixed"}}}
     :backdrop-opacity 0.75 :backdrop-on-click #(ut/tracked-dispatch [::bricks/disable-session-modal]) :child
     [re-com/v-box :size "auto" :gap "4px" :style {:background-color "#000000"} :width (px w) :height (px h) :children
      (for [sess sessions]
        [re-com/h-box :size "auto" :gap "4px" :children
         (for [ss sess :let [s (get ss 0) rowcnt (count sess) t (get ss 1)]] [image-component s t h rowcnt items])])]]))




(defn main-panel
  []
  (let [editor? (and @(ut/tracked-subscribe [::bricks/editor?]) (not @bricks/dragging?))
        buffy? @(ut/tracked-subscribe [::bricks/buffy?])
        flows? @(ut/tracked-subscribe [::bricks/flow?])
        external? @(ut/tracked-subscribe [::bricks/external?])
        session? @(ut/tracked-subscribe [::bricks/session-modal?])
        lines? @(ut/tracked-subscribe [::bricks/lines?])
        peek? @(ut/tracked-subscribe [::bricks/peek?])
        auto-run? @(ut/tracked-subscribe [::bricks/auto-run?])
        rekt [@db/kick-alert]
        websocket-status (select-keys @(ut/tracked-subscribe [::http/websocket-status]) [:status :datasets :panels :waiting])
        online? (true? (= (get websocket-status :status) :connected))
        hh @(ut/tracked-subscribe [::subs/h])
        ww @(ut/tracked-subscribe [::subs/w])
        selected-block @(ut/tracked-subscribe [::bricks/selected-block])
        selected-tab @(ut/tracked-subscribe [::bricks/selected-tab])
        selected-block? (true? (not (or (nil? selected-block) (= selected-block "none!"))))
        screen-name (ut/safe-name @(ut/tracked-subscribe [::bricks/screen-name]))
        client-name @(ut/tracked-subscribe [::bricks/client-name])
        flow-watcher-subs-grouped @(ut/tracked-subscribe [::bricks/flow-watcher-subs-grouped])
        server-subs @(ut/tracked-subscribe [::bricks/server-subs])
        coords (if lines? ;; expensive otherwise
                 (let [subq-mapping (if lines? @(ut/tracked-subscribe [::bricks/subq-mapping]) {})
                       dwn-from-here (vec (ut/cached-downstream-search subq-mapping selected-block))
                       up-from-here (vec (ut/cached-upstream-search subq-mapping selected-block))
                       involved (vec (distinct (into dwn-from-here up-from-here)))
                       subq-blocks @(ut/tracked-subscribe [::bricks/subq-panels selected-block])
                       smap (into {} (for [b (keys subq-mapping)] {b (ut/cached-downstream-search subq-mapping b)}))
                       lmap
                         (vec
                           (distinct
                             (apply concat
                               (for [[k downs] smap]
                                 (remove nil?
                                   (for [d     downs
                                         :when (and (not (cstr/starts-with? (str d) ":reco-preview"))
                                                    (not (cstr/starts-with? (str k) ":reco-preview")))]
                                     (let [src-r     @(ut/tracked-subscribe [::bricks/panel-px-root k])
                                           src-h     @(ut/tracked-subscribe [::bricks/panel-px-height k])
                                           src-w     @(ut/tracked-subscribe [::bricks/panel-px-width k])
                                           dd        (if (not (cstr/starts-with? (str d) ":block"))
                                                       @(ut/tracked-subscribe [::bricks/lookup-panel-key-by-query-key d])
                                                       d)
                                           involved? (some #(= % dd) involved)
                                           dest-r    @(ut/tracked-subscribe [::bricks/panel-px-root dd])
                                           dest-h    @(ut/tracked-subscribe [::bricks/panel-px-height dd])
                                           dest-w    @(ut/tracked-subscribe [::bricks/panel-px-width dd])
                                           t1        @(ut/tracked-subscribe [::bricks/what-tab k])
                                           t2        @(ut/tracked-subscribe [::bricks/what-tab d])
                                           x1        (+ (first src-r) src-w)
                                           y1        (+ (last src-r) (/ src-h 2))
                                           x2        (first dest-r)
                                           y2        (+ (last dest-r) (/ dest-h 2))]
                                       (when (and (not (= src-r dest-r)) (= t1 t2 selected-tab))
                                         (vec (flatten
                                                [(if peek? (- x1 (* src-w 0.15)) x1) y1  ;(if peek? (* y1
                                                                                         ;0.7) y1)
                                                 (if peek? (+ x2 (* dest-w 0.15)) x2) y2 ;(if peek? (* y2
                                                                                         ;0.7) y2)
                                                 involved?
                                                 (cond (= dd selected-block) 
                                                       ;;"#9973e0"
                                                       (theme-pull :theme/universal-pop-color "#9973e0")
                                                       (some #(= % dd) subq-blocks) "#e6ed21" ;; parent-of-selected
                                                       (some #(= % dd) (ut/cached-upstream-search subq-mapping selected-block))
                                                         "#7be073" ;; upstream?
                                                       (some #(= % dd) (ut/cached-downstream-search subq-mapping selected-block))
                                                         "#05dfff" ;; downstream?
                                                       :else "orange") k d nil]))))))))))]
                   lmap)
                 [])]
    (bricks/droppable
      (if @rvbbit-frontend.flows/dragging-port? ["meta-menu" :flow-port] ["meta-menu"]) ;; eyes
      [(js/Math.floor (/ (ifnil (first @db/context-modal-pos) 0) bricks/brick-size))
       (js/Math.floor (/ (ifnil (last @db/context-modal-pos) 0) bricks/brick-size))]
      [re-com/box :style {:background-color "black"} :attr
       {:id              "base-canvas"
        :on-drag-over    #(bricks/tag-screen-position %)
        :on-context-menu (re-com/handler-fn #_{:clj-kondo/ignore [:unresolved-symbol]}
                                            (bricks/tag-screen-position event) ;; event is magic in handler-fn
                                            (when (and (not @bricks/over-block?) (not @bricks/over-flow?))
                                              (bricks/insert-new-block
                                                [(js/Math.floor (/ (first @db/context-modal-pos) bricks/brick-size))
                                                 (js/Math.floor (/ (last @db/context-modal-pos) bricks/brick-size))]
                                                5
                                                4))
                                            #_{:clj-kondo/ignore [:unresolved-symbol]}
                                            (.preventDefault event))
        :on-mouse-down   #(when (= (.-button %) 1) ;; middle click
                            (do #_{:clj-kondo/ignore [:unresolved-symbol]}
                                (bricks/tag-screen-position %) ;; event is magic in handler-fn
                                (when (and (not @bricks/over-block?) (not @bricks/over-flow?))
                                  (let [x  (keyword (ut/replacer (str (talltale.core/quality-color-animal)) " " "-"))
                                        x  (ut/safe-key x)
                                        kp [:param x]]
                                    (bricks/insert-new-block [(js/Math.floor (/ (first @db/context-modal-pos) bricks/brick-size))
                                                              (js/Math.floor (/ (last @db/context-modal-pos) bricks/brick-size))]
                                                             5
                                                             4
                                                             {:drag-meta     {:type :open-input}
                                                              :w             6
                                                              :selected-view x
                                                              :h             2
                                                              :views         {x [:open-input
                                                                                 {:kp         kp ;[:param
                                                                                                 ;:remarkable-blush-louse]
                                                                                  :width-int  :panel-width+100
                                                                                  :height-int :panel-height+80
                                                                                  :syntax     "clojure"}]}})
                                    (let [default "new parameter value!" ;(if (nil? default) "new parameter
                                          vv      @(ut/tracked-subscribe [::conn/clicked-parameter kp])
                                          exists? (not (nil? vv))
                                          _ (when (not exists?) (ut/tracked-dispatch [::conn/click-parameter kp default]))])))
                                #_{:clj-kondo/ignore [:unresolved-symbol]}
                                (.preventDefault %)))} :size "1" :width (px ww) :height (px hh) :child
       [re-com/v-box :size "1" :style
        (let [custom-map (theme-pull :theme/canvas-background-css nil)
              custom-map (when (map? custom-map) custom-map)
              cc         (str ;(ut/invert-hex-color
                           (theme-pull :theme/editor-outer-rim-color nil)
                           87)]
          (merge custom-map
                 {:font-family (theme-pull :theme/base-font nil)
                  :transition  "filter 4s ease-in-out"
                  :filter      (when (not online?) "grayscale(100%)")}
                 (when (or @bricks/dragging? @bricks/mouse-dragging-panel?) ;; editor? ;;(not
                   {:background-image (str ;"linear-gradient(0deg, " cc " 1px, transparent
                                        "linear-gradient(0deg, "
                                        cc
                                        " 2px, transparent 8px), linear-gradient(90deg, "
                                        cc
                                        " 2px, transparent 8px)"
                                        (when (get custom-map :background-image) ", ")
                                        (get custom-map :background-image))
                    :background-size  (str "50px 50px, 50px 50px"
                                           (when (get custom-map :background-size) ", ")
                                           (get custom-map :background-size))}))) :children
        [[bricks/reecatch [tab-menu]] [bricks/reecatch [snapshot-menu]] (when session? [session-modal])
         (when (and editor? (not @bricks/mouse-dragging-panel?)) 
           [bricks/reecatch [editor-panel 33 10]]
           ;;[bricks/reecatch [editor-panel 50 22]]
           )
         (when (or (and @bricks/dragging? (not @bricks/on-block?) (not @bricks/over-flow?)) @bricks/swap-layers?)
           [re-com/box :child " " :style
            {:background-color (str (theme-pull :theme/editor-background-color nil) 22) ;; "#00000022"
             :filter           "drop-shadow(16px 16px 20px red) invert(75%)"
             :position         "fixed"
             :left             (* (js/Math.floor (/ (first @db/context-modal-pos) bricks/brick-size)) bricks/brick-size)
             :top              (* (js/Math.floor (/ (last @db/context-modal-pos) bricks/brick-size)) bricks/brick-size)} :width
            (px (* (get @bricks/dragging-body :w) bricks/brick-size)) :height
            (px (* (get @bricks/dragging-body :h) bricks/brick-size))])
         (when (and buffy? (not @bricks/mouse-dragging-panel?)) [bricks/reecatch [buffy/chat-panel]])
         (when flows? ;(and flows? (not @bricks/mouse-dragging-panel?))
           [bricks/reecatch [flows/flow-panel]]) [bricks/reecatch [bricks/grid]]
         [re-com/box :child
          [re-com/md-icon-button :src (at) :md-icon-name "zmdi-pizza" :tooltip "  toggle floating editor panel (SPACE)" :style
           {:color        "#ffffff"
            :cursor       "pointer"
            :opacity      (if editor? 1.0 0.3)
            :transform    (if editor? "rotate(-90deg)" "")
            :margin-top   "-2px"
            :padding-left "2px"
            :font-size    "15px"} :attr {:on-click #(ut/tracked-dispatch [::bricks/toggle-editor])}] :width "20px" :height "20px"
          :style {:position "fixed" :z-index 98 :bottom 0 :left 0 :background-color "#00000022" :color "white"}]
         [re-com/box :child
          [re-com/md-icon-button :src (at) :md-icon-name (if lines? "zmdi-layers" "zmdi-layers-off") :tooltip
           "show lineage / sub-query lines? (L)" :style
           {:color        "#ffffff"
            :cursor       "pointer"
            :opacity      (if lines? 1.0 0.3)
            :margin-top   "-2px"
            :padding-left "2px"
            :font-size    "15px"} :attr {:on-click #(ut/tracked-dispatch [::bricks/toggle-lines])}] :width "20px" :height "20px"
          :style
          {:position         "fixed"
           :z-index          98
           :border-radius    "0px 7px 0px 0px"
           :bottom           0
           :left             23
           :background-color "#00000022"
           :color            "white"}]
         [re-com/box :child
          [re-com/md-icon-button :src (at) :md-icon-name (if peek? "zmdi-eye" "zmdi-eye-off") :tooltip "toggle peek mode (P)"
           :style
           {:color        "#ffffff"
            :cursor       "pointer"
            :opacity      (if peek? 1.0 0.3)
            :margin-top   "-2px"
            :padding-left "2px"
            :font-size    "15px"} :attr {:on-click #(ut/tracked-dispatch [::bricks/toggle-peek])}] :width "20px" :height "20px"
          :style
          {:position         "fixed"
           :z-index          98
           :border-radius    "0px 7px 0px 0px"
           :bottom           0
           :left             46
           :background-color "#00000022"
           :color            "white"}]
         [re-com/box :child
          [re-com/md-icon-button :src (at) :md-icon-name (if auto-run? "zmdi-refresh-sync" "zmdi-refresh-sync-off") :tooltip
           "toggle auto-refresh (O)" :style
           {:color        "#ffffff"
            :cursor       "pointer"
            :opacity      (if auto-run? 1.0 0.3)
            :margin-top   "-2px"
            :padding-left "2px"
            :font-size    "15px"} :attr {:on-click #(ut/tracked-dispatch [::bricks/toggle-auto-run])}] :width "20px" :height
          "20px" :style
          {:position         "fixed"
           :z-index          98
           :border-radius    "0px 7px 0px 0px"
           :bottom           0
           :left             69
           :background-color "#00000022"
           :color            "white"}]
         [re-com/box :child
          [re-com/md-icon-button :src (at) :md-icon-name "zmdi-developer-board" :tooltip "  toggle external editing" :style
           {:color        (if external? "red" "#ffffff")
            :cursor       "pointer"
            :opacity      (if external? 1.0 0.3)
            :margin-top   "-2px"
            :padding-left "2px"
            :font-size    "15px"} :attr {:on-click #(ut/tracked-dispatch [::bricks/toggle-external])}] :width "20px" :height
          "20px" :style {:position "fixed" :z-index 98 :bottom 0 :left 92 :background-color "#00000022" :color "white"}]
         [re-com/box :child
          [re-com/md-icon-button :src (at) :md-icon-name "zmdi-close" :tooltip "un-select block (ESC)" :style
           {:color        "#ffffff"
            :cursor       "pointer"
            :opacity      (if selected-block? 1.0 0.3)
            :margin-top   "-2px"
            :padding-left "2px"
            :font-size    "15px"} :attr {:on-click #(ut/tracked-dispatch [::bricks/select-block "none!"])}] :width "20px" :height
          "20px" :style
          {:position         "fixed"
           :z-index          98
           :border-radius    "0px 7px 0px 0px"
           :bottom           0
           :left             115
           :background-color "#00000022"
           :color            "white"}]
         (when @(ut/tracked-subscribe [::audio/audio-playing?])
           [re-com/md-icon-button :src (at) :md-icon-name "zmdi-speaker" :style
            {;:margin-top "4px"
             :position  "fixed"
             :left      138
             :bottom    0
             :color     "red"
             :font-size "22px"}])
         (when @(ut/tracked-subscribe [::audio/recording?])
           [re-com/md-icon-button :src (at) :md-icon-name "zmdi-mic" :style
            {;:margin-top "-0px"
             :position  "fixed"
             :left      161
             :bottom    0
             :color     "red"
             :font-size "22px"}])
         [re-com/h-box :size "none" :gap "8px" :style
          {:position    "fixed"
           :left        138
           :bottom      0
           :font-weight 700
           :color       (if (not online?) "#ffffff" "#ffffff77")
           :font-family (theme-pull :theme/monospaced-font nil)} :children
          [[re-com/box :size "none" :child
            (str (get websocket-status :waiting -1)
                 (str ", " (ut/nf (count server-subs)))
                 (when (not online?) " (RVBBIT server is offline)"))]
           (when (ut/ne? flow-watcher-subs-grouped)
             [re-com/h-box :style {:padding-left "10px" :font-size "12px"} :gap "8px" :children
              (for [[kk cnt] flow-watcher-subs-grouped]
                [re-com/h-box :style
                 {:background-color (theme-pull :theme/editor-outer-rim-color nil)
                  :padding-left     "6px"
                  :border-radius    "5px 5px 0px 0px"
                  :padding-right    "6px"
                  :color            (theme-pull :theme/editor-background-color nil)} :gap "5px" :children
                 [[re-com/box :child (str kk)] [re-com/box :style {:opacity 0.5} :child (str cnt)]
                  [re-com/md-icon-button :md-icon-name "zmdi-close" :style
                   {:cursor "pointer" :color "white" :font-size "15px" :height "10px" :margin-top "-3px"} :on-click
                   #(do (ut/tapp>> [:remove-flow-watchers client-name kk])
                        (ut/tracked-dispatch [::wfx/request :default
                                              {:message {:kind :remove-flow-watcher :client-name client-name :flow-id kk}
                                               :timeout 50000}]))]]])])]]
         (let [mem @(ut/tracked-subscribe [::bricks/memory])]
           [re-com/box :size "none" :style {:position "fixed" :left 2 :bottom 20 :font-weight 700 :color "#ffffff99"} :child
            (str (ut/bytes-to-mb (get mem 1)) 
                 ;;(str " (lazy-grid? " (not (or @bricks/param-hover @bricks/query-hover)) ")")
                 )])
         [bricks/reecatch [task-bar]] 
         [bricks/reecatch [flows/alert-box]]
         [re-com/box :child
          [re-com/md-icon-button :src (at) :md-icon-name "zmdi-labels" :tooltip "toggle display mode" :on-click
           #(ut/tracked-dispatch [::bricks/toggle-no-ui]) :style
           {:color        "#ffffff"
            :cursor       "pointer"
            :opacity      (if @(ut/tracked-sub ::bricks/full-no-ui? {}) 1.0 0.3)
            :margin-top   "-2px"
            :padding-left "2px"
            :font-size    "15px"}] :width "20px" :height "20px" :style
          {:position "fixed" :z-index 98 :bottom 0 :right 49 :background-color "#00000022" :color "white"}]
         [re-com/box :child
          [re-com/md-icon-button :src (at) :md-icon-name "zmdi-refresh" :tooltip "refresh all" :on-click
           #(do (ut/tracked-dispatch [::bricks/refresh-all])) :style
           {:color "#ffffff" :cursor "pointer" :margin-top "-2px" :padding-left "2px" :font-size "15px"}] :width "20px" :height
          "20px" :style {:position "fixed" :z-index 98 :bottom 0 :right 26 :background-color "#00000022" :color "white"}]
         [re-com/box :child
          [re-com/md-icon-button :src (at) :md-icon-name "zmdi-save" :tooltip "save board (Ctrl-S)" :on-click
           #(do (ut/tracked-dispatch [::http/save :skinny screen-name])) :style
           {:color "#ffffff" :cursor "pointer" :margin-top "-2px" :padding-left "2px" :font-size "15px"}] :width "20px" :height
          "20px" :style {:position "fixed" :z-index 98 :bottom 0 :right 3 :background-color "#00000022" :color "white"}]
         (when (ut/ne? coords)
           [:svg
            {:style {:width          (px ww) ;"6200px" ;; big ass render nothing gets cut off
                     :height         (px hh) ;"6200px"
                     :pointer-events "none"
                     :z-index        8}} (bricks/draw-lines coords)])]]])))


