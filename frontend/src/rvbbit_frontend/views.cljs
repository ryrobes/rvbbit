(ns rvbbit-frontend.views
  (:require
   [reagent.core :as reagent]
   [re-frame.core :as re-frame]
   [re-com.core :as re-com :refer [at]]
   [re-com.util :refer [px]]
   [rvbbit-frontend.connections :as conn]
   [re-catch.core :as rc]
   [rvbbit-frontend.utility :as ut]
   [rvbbit-frontend.buffy :as buffy]
   [rvbbit-frontend.flows :as flows]
   [rvbbit-frontend.audio :as audio]
   ;[re-pressed.core :as rp]
   ;[rvbbit-frontend.events :as events]
   [cljs.tools.reader :refer [read-string]]
   [clojure.edn :as edn]
   [rvbbit-frontend.db :as db]
   [rvbbit-frontend.subs :as subs]
   [rvbbit-frontend.http :as http]
   [rvbbit-frontend.bricks :as bricks :refer [theme-pull]]
   [goog.events :as gevents]
   [clojure.walk :as walk]
   [clojure.string :as cstr]
   [rvbbit-frontend.shapes :as shape]
   ;[rvbbit-frontend.connections :as conn]
   ;[websocket-fx.core :as wfx]
   ;[cljs.tools.reader :refer [read-string]]
   ;[oz.core :as oz]
   ;[reagent.dom :as rdom]
   [websocket-fx.core :as wfx]
   [cljs-drag-n-drop.core :as dnd2]
   [talltale.core :as tales])
  (:import [goog.events EventType]
           [goog.async Debouncer]))




;(str (theme-pull :theme/monospaced-font nil) )
;(def (theme-pull :theme/editor-rim-color "#a3a3a3") (theme-pull :theme/(theme-pull :theme/editor-rim-color "#a3a3a3") "green")) ; "#a3a3a3") ; "#5c547d" ; "#a891ff" ;"#b7e27c"
;(def (theme-pull :theme/editor-outer-rim-color nil) "#a891ff")


(defn debounce [f interval]
  (let [dbnc (Debouncer. f interval)]
    ;; We use apply here to support functions of various arities
    (fn [& args] (.apply (.-fire dbnc) dbnc (to-array args)))))

(defonce detached-coords
  (reagent/atom (let [hh (.-innerHeight js/window) ;; starting size set on load
                      ;ww (.-innerWidth js/window) ;; starting size set on load
                      bricks-high (+ (js/Math.floor (/ hh bricks/brick-size)) 1)
                      ;bricks-wide (+ (js/Math.floor (/ ww bricks/brick-size)) 1)
                      topper (* (- bricks-high 12) bricks/brick-size) ;; size of editor plus 2 buffer bricks
                      lefty (* 2 bricks/brick-size) ;; always 2 brick from the left...
                      ]
                  ;(tap> [:coords-editor [lefty topper] bricks-high hh ww])
                  [lefty topper])))

(defn mouse-move-handler [offset]
  (fn [evt]
    (let [start-x (.-clientX evt)
          start-y (.-clientY evt)
          off-x (:x offset)
          off-y (:y offset)
          x      (- start-x off-x) ;
          y      (- start-y off-y)]
      (reset! detached-coords [x y]))))

(defn mouse-up-handler [on-move]
  (fn me [evt]
    (reset! bricks/dragging-editor? false)
    (do
      (gevents/unlisten js/window EventType.MOUSEMOVE on-move))))

(defn mouse-down-handler [e]
  (let [{:keys [left top]} (bricks/get-client-rect e)
        offset             {:x (- (.-clientX e) left)
                            :y (- (.-clientY e) top)}
        on-move            (mouse-move-handler offset)]
    (reset! bricks/dragging-editor? true)
    (do
      (gevents/listen js/window EventType.MOUSEMOVE on-move))
    (gevents/listen js/window EventType.MOUSEUP
                    (mouse-up-handler on-move))))

(defonce view-browser-query (reagent/atom {}))

(defonce data-browser-mode (reagent/atom :data))
(defonce data-browser-system-mode (reagent/atom :data))

(defn editor-panel-metadata []
  (let [sql-calls {:connections-sys {:select [;:database_name
                                              [[:case
                                                [:like :connection_str "%csv%"] "*csv-import-db*"
                                                [:like :connection_str "%cache%"] "*cache-db*"
                                                :else :database_name] :database_name]
                                              :connection_id]
                                     :from [:connections]
                                     ;:where [:<> :connection_id "-656776194"]
                                     }
                   :tables-sys {:select [:db_schema :db_catalog :connection_id [[:|| :db_catalog "/" :db_schema] :schema_cat] :table_name [[:count 1] :fields]]
                                :from [:fields]
                                :where [:= :connection_id :connections-sys/connection_id]
                                :group-by [:db_schema :db_catalog :connection_id :table_name]
                                :order-by [:schema_cat :table_name]}}
        sql-params (into {} (for [k [:connections-sys/connection_id]]
                              {k @(re-frame/subscribe [::conn/clicked-parameter-key [k]])}))]
    (dorun (for [[k v] sql-calls]
             (let [query (walk/postwalk-replace sql-params v)
                   data-exists? @(re-frame/subscribe [::conn/sql-data-exists? [k]])
                   unrun-sql? @(re-frame/subscribe [::conn/sql-query-not-run? [k] query])]
               (when (or (not data-exists?) unrun-sql?)
                 (conn/sql-data [k] query)))))
    [re-com/h-box
     :size "auto"
     :style {:color (str (theme-pull :theme/editor-font-color nil) 35)} ;; rows label under grids THEME
     :children [[re-com/box
                 :size "auto"
                 :child [bricks/magic-table :system-connections-list* [:connections-sys] 3 10 [:database_name]]]
                [re-com/box
                 :size "auto"
                 :child [bricks/magic-table :system-tables-list* [:tables-sys] 9 10 [:db_schema :connection_id :db_catalog]]]]]))

(defn search-panel-metadata []
  (let [client-name (cstr/replace (str @(re-frame/subscribe [::bricks/client-name])) ":" "")
        sql-calls {:searches-types-sys {:select [:item_type] ;; :searches-types-sys/item_type 
                                        :where [:and [:not [:like :item_sub_type "%preview%"]]
                                                [:not [:= :item_type "saved-block"]]]
                                        :from [:client_items] :group-by [1]}
                   :searches-sub-types-sys {:select [:item_key :is_live [[:count 1] :items]] ;; :searches-types-sys/item_type 
                                            :where [:and
                                                    [:not [:= :item_key client-name]]
                                                    [:not [:like :item_sub_type "%preview%"]] 
                                                    [:not [:= :item_type "saved-block"]]
                                                    [:= :item_type :searches-types-sys/item_type]]
                                             ;;:col-widths {:items 70}
                                             :style-rules {[:* :highlight-4018a]
                                                           {:logic [:= :is_live 1]
                                                            :style {:background-color (str (theme-pull :theme/editor-outer-rim-color nil) 34)
                                                                    :color (theme-pull :theme/editor-outer-rim-color nil)
                                                                    ;:border (str "1px solid " (theme-pull :theme/editor-outer-rim-color nil))
                                                                    }}}
                                            :from [:client_items] :group-by [1 2]}}
        sql-params (into {} (for [k [:searches-types-sys/item_type]]
                              {k @(re-frame/subscribe [::conn/clicked-parameter-key [k]])}))]
    ;;(tap> [:sql-params! sql-params])
    (dorun (for [[k v] sql-calls]
             (let [query (walk/postwalk-replace sql-params v)
                   data-exists? @(re-frame/subscribe [::conn/sql-data-exists? [k]])
                   unrun-sql? @(re-frame/subscribe [::conn/sql-query-not-run? [k] query])]
               (when (or (not data-exists?) unrun-sql?)
                 (conn/sql-data [k] query)
                 (re-frame/dispatch [::bricks/insert-sql-source k query])))))
    [re-com/h-box
     :size "auto"
     :style {:color (str (theme-pull :theme/editor-font-color nil) 35)} ;; rows label under grids THEME
     :children [[re-com/box
                 :size "auto"
                 :child [bricks/magic-table :searches-types-list* [:searches-types-sys] 3 10 []]] ;; last for hiding fields that you STILL want in the click parameter
                [re-com/box
                 :size "auto"
                 :child [bricks/magic-table :searches-sub-types-sys-list* [:searches-sub-types-sys] 9 10 [:is_live]]]]]))

(defn search-panel-metadata-ext []
  (let [sql-params (into {} (for [k [:searches-types-sys/item_type
                                     :searches-rows-sys/value
                                     :searches-sub-types-sys/item_key]]
                              {k @(re-frame/subscribe [::conn/clicked-parameter-key [k]])}))
        selected-shortcode (get sql-params :searches-rows-sys/value)
        sql-calls {:searches-rows-sys {:select [:item_sub_type :item_type :item_key :display_name :sample :value :is_live :block_meta] ;; :searches-types-sys/item_type 
                                       :where [:and
                                               [:not [:like :item_sub_type "%preview%"]]
                                               [:not [:= :item_type "saved-block"]]
                                               [:= :item_key  :searches-sub-types-sys/item_key]
                                               [:= :item_type :searches-types-sys/item_type]]
                                                               ;;:col-widths {:items 77}
                                       :from [:client_items]}}]

    (dorun (for [[k v] sql-calls]
             (let [query (walk/postwalk-replace sql-params v)
                   data-exists? @(re-frame/subscribe [::conn/sql-data-exists? [k]])
                   unrun-sql? @(re-frame/subscribe [::conn/sql-query-not-run? [k] query])]
               (when (or (not data-exists?) unrun-sql?)
                 (conn/sql-data [k] query)))))

    [re-com/h-box
     :size "auto"
     :style {:color (str (theme-pull :theme/editor-font-color nil) 35)}
     :children [[re-com/v-box
                 :size "auto"
                 :children
                 [[re-com/box
                   :size "auto"
                   :child [bricks/magic-table :searches-rows-sys-list*
                           [:searches-rows-sys] 11 (if selected-shortcode 8.5 10) [:value :is_live :item_type :block_meta :item_key]]]
                  (when selected-shortcode
                    [re-com/box
                   ;:align :center :justify :center
                     :child [bricks/shortcode-box 560 85 (str selected-shortcode " ;; rabbit-code parameter") "clojure"]])]]]]))


;(defonce grid-recos? (atom true))

(defn insert-hidden-reco-preview [reco-selected reco-viz reco-query reco-condis combo-name shape-name single?]
  (let []
    ;(tap> [:insert-hidden-reco-preview reco-selected reco-viz reco-query reco-condis single?])
    (dorun
     (when single? (re-frame/dispatch [::bricks/new-reco-preview reco-selected]))

     (cond (= (first (read-string reco-viz)) :vega-lite)

           (let [incomingv (read-string reco-viz) ;; read-string will ruin my internal namespaced keywords
                 ffromv (-> (get-in incomingv [1 :data :values])
                            (ut/replacer  "_" "-")
                            (ut/replacer  "query/" ""))
                 original-conn @(re-frame/subscribe [::bricks/lookup-connection-id-by-query-key (keyword ffromv)])
                 vega-cfg {:view {:stroke "transparent"}
                           :axis {:domainColor "#ffffff22"
                                  :grid true
                                  :labelColor "#ffffff88"
                                  :titleFont "Lato"
                                  :axisFont "Lato"
                                  :font "Lato"
                                  :titleColor "#ffffff99"
                                  :labelFont "Lato"
                                  :domain false
                                  :gridColor "#ffffff22"
                                  :tickColor "#ffffff22"}
                           :legend {:labelFont "Lato" :legendFont "Lato" :labelColor "#ffffff99"
                                    :titleColor "#ffffff99" :stroke "#ffffff99" :titleFont "Lato"}
                           :header {:labelFont "Lato" :titleFont "Lato" :titleColor "#ffffff99"}
                           :mark {:font "Lato"}
                           :title {:font "Lato" :subtitleFont "Lato" :labelFont "Lato"
                                   :titleFont "Lato" :titleColor "#ffffff99"}}
                 view (-> incomingv
                          (assoc-in [1 :data :values] :query-preview)
                          (assoc-in [1 :config] :theme/vega-defaults)
                          (assoc-in [1  :width] "container") ;:panel-width)
                          (assoc-in [1  :height] :panel-height)
                          (assoc-in [1 :padding] 4)
                          (assoc-in [1 :background] "transparent"))

                   ; incoming (first (read-string reco-query)) ;; read-string will ruin my internal namespaced keywords
                   ; ffrom (ut/replacer (first (get incoming :from)) "_" "-")
                   ; query (assoc incoming :from [(keyword ffrom)])
                   ; query (walk/postwalk-replace {[[:sum :rows]] [:count 1]} query)

                 q-data (read-string reco-query)
                 incoming (first q-data) ;; read-string will ruin my internal namespaced keywords
                 ffrom (ut/replacer (first (get incoming :from)) "_" "-")
                    ;original-conn @(re-frame/subscribe [::bricks/lookup-connection-id-by-query-key (keyword ffrom)])
                    ;      ;query (assoc incoming :from [(keyword ffrom)])
                 query (vec (for [q q-data] (assoc q :from [(keyword ffrom)])))
                 query (walk/postwalk-replace {[[:sum :rows]] [:count 1]} query)]
                ;(tap> [:rec-preview-block 1 view query original-conn reco-selected])
             (bricks/insert-rec-preview-block
              view
              query ;reco-h reco-w
              reco-condis
              original-conn ;reco-conn
              reco-selected combo-name shape-name single? true))

           :else (let [view (read-string reco-viz) ;; read-string will ruin my internal namespaced keywords
                       q-data (read-string reco-query)
                          ;q-cnt (count q-data)
                       incoming (first q-data) ;; read-string will ruin my internal namespaced keywords
                       ffrom (ut/replacer (first (get incoming :from)) "_" "-")
                       original-conn @(re-frame/subscribe [::bricks/lookup-connection-id-by-query-key (keyword (last (cstr/split ffrom "/")))])
                          ;query (assoc incoming :from [(keyword ffrom)])
                       query (vec (for [q q-data]
                                    (if (nil? (find q :vselect))
                                      (assoc q :from [(keyword ffrom)])
                                      q
                                        ;(assoc q :from (keyword ffrom))
                                      )))
                       query (walk/postwalk-replace {[:sum :rows] [:count 1]} query)]

                ;(tap> [:rec-preview-block 2 view query reco-condis original-conn reco-selected])
                   (bricks/insert-rec-preview-block
                    view
                    query; reco-h reco-w
                    reco-condis
                    original-conn ;reco-conn
                    reco-selected combo-name shape-name single? true))))))

(defn editor-panel-viz2 []
  (let [all-sql-call-keys @(re-frame/subscribe [::bricks/all-sql-call-keys])
        grid-reco? @(re-frame/subscribe [::bricks/grid-reco?])
        recos-page @(re-frame/subscribe [::bricks/recos-page])
        all-sql-call-keys-str (for [e all-sql-call-keys] (ut/replacer (ut/unkeyword e) "-" "_"))
        sql-params (into {} (for [k [:viz-tables-sys/table_name :viz-shapes0-sys/shape :viz-tables-sys/table_name
                                     :viz-shapes0-sys/shape :viz-shapes-sys/combo_edn :user-dropdown-sys/req-field]]
                              {k @(re-frame/subscribe [::conn/clicked-parameter-key [k]])}))
        combo-picked? (not (nil? (get sql-params :viz-shapes-sys/combo_edn)))
        shape-picked? (not (nil? (get sql-params :viz-shapes0-sys/shape)))
        clear-filters-fn (fn [] (do (re-frame/dispatch [::conn/click-parameter [:viz-shapes0-sys :shape] nil])
                                    (re-frame/dispatch [::conn/click-parameter [:user-dropdown-sys :req-field] nil])
                                    (re-frame/dispatch [::conn/click-parameter [:viz-shapes-sys :combo_edn] nil])))
        req-field-picked? (not (nil? (get sql-params :user-dropdown-sys/req-field)))
        sql-calls {:viz-tables-sys {:select [:table_name [[:count 1] :recos]]
                                    :from [:viz_recos_vw]
                                    :where [:and [:not [:like :table_name "query_preview%"]]
                                            (let [lst (vec (cons :or (vec (for [t all-sql-call-keys-str] [:= :table_name t]))))]
                                              (if (> (count (flatten lst)) 1) lst false))]
                                    :order-by [:table_name]
                                    :group-by [:table_name]}
                   :viz-shapes0-sys {:select [[:shape_name :shape] [[:count 1] :recos]]
                                     :from [[:viz_recos_vw :vvt]]
                                     :where [:and [:= :table_name :viz-tables-sys/table_name]
                                             (when req-field-picked?
                                               [:like :combo_edn (str "%" (get sql-params :user-dropdown-sys/req-field) "%")])
                                             (when combo-picked?
                                               [:= :combo_edn :viz-shapes-sys/combo_edn])] ;; :viz-tables-sys/table_name-name
                                     :group-by [:shape_name]}
                   :viz-shapes-sys {:select [:combo_edn [[:count 1] :recos]]
                                    :from [[:viz_recos_vw :vvt]]
                                    ;:order-by [[:score :desc]]
                                    :where [:and
                                            (when (not (nil? (get sql-params :viz-shapes0-sys/shape)))
                                              [:= :shape_name :viz-shapes0-sys/shape])
                                            (when req-field-picked?
                                              [:like :combo_edn (str "%" (get sql-params :user-dropdown-sys/req-field) "%")])
                                            [:= :table_name :viz-tables-sys/table_name]] ;; :viz-tables-sys/table_name-name
                                    :group-by [:combo_edn]}
                   :recos-sys {:select [:*]
                               :from [:viz_recos_vw]
                               :order-by [[:score :desc]]
                               :where [:and
                                       [:= :table_name :viz-tables-sys/table_name]
                                       (when combo-picked?
                                         [:= :combo_edn :viz-shapes-sys/combo_edn])
                                       (when req-field-picked?
                                         [:like :combo_edn (str "%" (get sql-params :user-dropdown-sys/req-field) "%")])
                                       (when shape-picked?
                                         [:= :shape_name :viz-shapes0-sys/shape])]}}
        block-list @(re-frame.core/subscribe [::conn/sql-data [:viz-tables-sys]])
        combo-list @(re-frame.core/subscribe [::conn/sql-data [:viz-shapes-sys]])
        full-recos @(re-frame.core/subscribe [::conn/sql-data [:recos-sys]])
        ;current-tab @(re-frame/subscribe [::bricks/selected-tab])
        current-tab-queries  (try (map #(-> % ut/sql-keyword name) @(re-frame/subscribe [::bricks/current-tab-queries]))
                                  (catch :default _ []))
        block-list (vec (filter (fn [x] (some #(= % (get x :table_name)) current-tab-queries)) block-list)) ;; overwrite above with curr tab only
        recos-count (count full-recos)
        block-list-boxes (for [{:keys [table_name recos]} block-list
                               :let [sel (get sql-params :viz-tables-sys/table_name)
                                     sel? (= table_name sel)]]
                           [re-com/box
                            :attr {:on-click #(do
                                                (re-frame/dispatch [::conn/click-parameter [:viz-tables-sys :table_name] table_name])
                                                (clear-filters-fn))
                                   ;#(reset! viz-block table_name)
                                   }
                            :align :center :justify :center
                            :style {:border-top "1px solid #80008045"
                                    :border-left "1px solid #80008080"
                                    :font-weight 700
                                    :font-size "13px"
                                    ;:transform "translate(0)"
                                    :cursor "pointer"
                                    :color (if sel? (theme-pull :theme/editor-background-color nil) ;;"#000000"
                                               ;;"#ffffff77"
                                               (str (theme-pull :theme/editor-font-color nil) 77))
                                    :background-color (if sel?
                                                        "#9973e0"
                                                        ;;"#000000"
                                                        (theme-pull :theme/editor-background-color nil))
                                    :padding-left "5px" :padding-right "5px"}
                            :child (str table_name " (" recos ")")])
        combo-singles (vec (distinct (flatten (doall (for [{:keys [combo_edn recos]} combo-list]
                                                       (cstr/split (ut/replacer combo_edn #"\"" "") ", "))))))
        pkeys @(re-frame/subscribe [::bricks/preview-keys])
        preview-keys (vec (for [k pkeys] (keyword (str "reco-preview" k))))
        preview-maps (into {} (for [{:keys [combo_hash shape_name combo_edn query_map viz_map condis h w selected_view]} full-recos]
                                {(keyword (str "reco-preview" combo_hash))
                                 {:shape_name shape_name
                                  :query_map query_map
                                  :combo_hash combo_hash
                                  :selected_view selected_view
                                  :h h :w w
                                  :viz_map viz_map
                                  :condis condis
                                  :combo_edn combo_edn}}))
        reco-selected (keyword (str "reco-preview" @(re-frame/subscribe [::conn/clicked-parameter-key [:recos-sys/combo_hash]])))
        preview-container (fn [preview-maps pk pnum]
                            (try (let [panel-key (nth pk pnum)
                                       ce (get-in preview-maps [panel-key :combo_edn])
                                       query_map (get-in preview-maps [panel-key :query_map])
                                       viz_map (get-in preview-maps [panel-key :viz_map])
                                       w (get-in preview-maps [panel-key :w] 9)
                                       h (get-in preview-maps [panel-key :h] 9)
                                       selected-view (try (edn/read-string (get-in preview-maps [panel-key :selected_view] nil)) (catch :default _ nil))
                                       condis (get-in preview-maps [panel-key :condis])
                                       combo_hash (get-in preview-maps [panel-key :combo_hash])
                                       combo_edn (try (when (not (nil? ce)) (ut/replacer ce #"\"" "")) (catch :default _ "")) ;; TODO, why bombing?
                                       shape_name (get-in preview-maps [panel-key :shape_name])
                                       sel? (= reco-selected panel-key)
                                       body [re-com/v-box
                                             :size "none"
                                             :attr {:on-click #(do (re-frame/dispatch [::conn/click-parameter [:recos-sys :combo_hash] combo_hash])
                                                                   (re-frame/dispatch [::conn/click-parameter [:recos-sys :combo_edn] combo_edn])
                                                                   (re-frame/dispatch [::conn/click-parameter [:recos-sys :shape_name] shape_name])
                                                                   (re-frame/dispatch [::conn/click-parameter [:recos-sys :selected_view] selected-view])
                                                                   (re-frame/dispatch [::conn/click-parameter [:recos-sys :h] h])
                                                                   (re-frame/dispatch [::conn/click-parameter [:recos-sys :w] w])
                                                                   (re-frame/dispatch [::conn/click-parameter [:recos-sys :query_map] query_map])
                                                                   (re-frame/dispatch [::conn/click-parameter [:recos-sys :viz_map] viz_map])
                                                                   (re-frame/dispatch [::conn/click-parameter [:recos-sys :condis] condis]))}
                                             :width "581px"
                                             :height "434px" ;"200px"
                                             :style {:zoom 0.44
                                                     :border "1px solid #ffffff22"
                                                     :transform "translate(0)" ;; a known CSS hack for fixed position in non fixed parents!!! TODO futureproof it?
                                                     :overflow "auto"}
                                             :justify :between
                                             :children [[re-com/box
                                                         :size "auto"
                                                         :height "40px"
                                                         :style {:padding-left "14px"
                                                                 :cursor "pointer"
                                                                 :padding-right "14px"
                                                                 :font-weight 400
                                                                 :font-size "22px"}
                                                         :child (str combo_edn)]
                                                        ;[re-com/box :child
                                                        [bricks/honeycomb panel-key (or selected-view :oz) 9 9]
                                                         ;:width "258px"
                                                         ;:height "191px"
                                                        ; ]
                                              ;; force 9x9 for preview grid
                                                        [re-com/box
                                                         :size "auto"
                                                         :height "40px"
                                                         :justify :end :align :end
                                                         :style {:padding-left "14px"
                                                                 :padding-right "14px"
                                                                 :font-weight 400
                                                                 :font-size "22px"}
                                                         :child (str shape_name)]]]]
                                   ;(tap> [:?? (get-in preview-maps [panel-key :selected_view] nil)])

                                   (if sel? (bricks/draggable
                                             (bricks/sql-spawner-meta :viz-reco) "meta-menu"
                                             [re-com/box
                                              :style {;:border (str "1px dashed " (theme-pull :theme/editor-outer-rim-color nil))
                                                      :background-color (str (theme-pull :theme/editor-outer-rim-color nil) 22)
                                                      :cursor "grab"}
                                              :width "258px"
                                              :height "191px"
                                              :child body])
                                       ;body

                                       [re-com/box
                                        :style {:cursor "grab"}
                                        :size "auto"
                                        :child (bricks/draggable
                                                (bricks/sql-spawner-meta panel-key) "meta-menu"
                                                body)]))

                                 (catch :default _
                                   [re-com/box
                                    :width "240px"
                                    :size "auto"
                                    :height "400px"
                                    :align :center
                                    :justify :center
                                    :style {:color (str (theme-pull :theme/editor-font-color nil) 22) ; "#ffffff22"
                                            :zoom 0.44
                                            :font-size "40px"}
                                    :child "n/a" ;(str e)
                                    ])))
        pages (/ recos-count 6)]
;(tap> [:full-recos (map :table_name block-list)
;       (filter (fn [x] (some #(= % (get x :table_name)) current-tab-queries)) block-list)
;       ;(filter #(= (get % :table_name) ) block-list)
;       current-tab-queries ])

    (dorun (let [prev-preview-keys (for [k @(re-frame/subscribe [::bricks/preview-keys])] (keyword (str "reco-preview" k)))
                 per-page 6 ;; if we ever get more room...
                 grid-page @(re-frame/subscribe [::bricks/recos-page])
                 grid-page (if (> (- grid-page 1) pages) 0 grid-page)
                 recos (take per-page (drop (* grid-page per-page) @(re-frame.core/subscribe [::conn/sql-data [:recos-sys]])))
                 recos-keys (vec (for [{:keys [combo_hash]} recos] combo_hash))]
             (doseq [k prev-preview-keys] (re-frame/dispatch [::bricks/quick-delete-panel k]))
             (re-frame/dispatch [::bricks/set-preview-keys recos-keys])
             (doseq [{:keys [combo_hash query_map viz_map condis combo_edn shape_name]} recos]
               (insert-hidden-reco-preview combo_hash viz_map query_map condis combo_edn shape_name false))))


    ;(tap> (vec (cons :or (vec (for [t all-sql-call-keys-str] [:= :table_name t])))))
    ;(tap> sql-params)
    ;(tap> combo-singles)


    (dorun (for [[k v] sql-calls]
             (let [query (walk/postwalk-replace sql-params v)
                   data-exists? @(re-frame/subscribe [::conn/sql-data-exists? [k]])
                   unrun-sql? @(re-frame/subscribe [::conn/sql-query-not-run? [k] query])]
               (when (or (not data-exists?) unrun-sql?)
                 (conn/sql-data [k] query)))))

    [re-com/v-box
     :height "433px"
     :children [[re-com/h-box
                 :height "39px"
                 :gap "4px"
                 :style {:overflow "auto"
                         :padding-bottom "5px"
                         :padding-left "7px"}
                 :children block-list-boxes]

                [re-com/h-box
                 :height "40px"
                 :gap "12px"
                 :align :center :justify :start
                 :style {;:background-color "maroon"
                         :padding-left "6px"}
                 :children [[re-com/single-dropdown
                             :choices (conj (for [c combo-singles] {:id c :label c})
                                            {:id nil :label "(all possible fields)"})
                             :max-height "320px"
                             :placeholder "(all possible fields)"

                             :style {:font-size "13px"
                                     ;:height "22px"
                                     ;:border-top (str "1px solid " (theme-pull :theme/editor-font-color nil) 11)
                                     ;:border-left (str "1px solid " (theme-pull :theme/editor-font-color nil) 11)
                                     ;:border-radius "8px"
                                     ;:color (theme-pull :theme/editor-font-color nil)
                                     }
                             ;:parts {:tooltip {:style {:background-color "blue"}}}
                                          ;:can-drop-above? true
                                          ;:est-item-height 230
                             :model (get sql-params :user-dropdown-sys/req-field)
                             :on-change #(re-frame/dispatch [::conn/click-parameter [:user-dropdown-sys :req-field] %])
                             :width "285px"]
                            ;[re-com/box :child "hey"]
                            [re-com/single-dropdown
                             :choices (conj (for [{:keys [combo_edn recos]} combo-list]
                                              {:id combo_edn :label (str combo_edn " (" recos ")")})
                                            {:id nil :label "(all possible field combos)"})
                             :max-height "320px"
                             :placeholder "(all possible field combos)"
                             ;:height "29px"
                             :style {:font-size "13px"}
                                          ;:can-drop-above? true
                                          ;:est-item-height 230
                             :model (get sql-params :viz-shapes-sys/combo_edn)
                             :on-change #(re-frame/dispatch [::conn/click-parameter [:viz-shapes-sys :combo_edn] %])
                             :width "660px"]

                            [re-com/box :child "grid"
                             :attr {:on-click #(re-frame/dispatch [::bricks/set-grid-reco? true])}
                             :style {:color (if grid-reco? (theme-pull :theme/editor-font-color nil) "inherit")
                                     :cursor "pointer"
                                     :margin-top "-9px"
                                     :font-weight 700}]

                            [re-com/box :child "previews"
                             :attr {:on-click #(re-frame/dispatch [::bricks/set-grid-reco? false])}
                             :style {:color (if (not grid-reco?) (theme-pull :theme/editor-font-color nil) "inherit")
                                     :cursor "pointer"
                                     :margin-top "-9px"
                                     :font-weight 700}]]]

                [re-com/h-box
                 :size "auto"
                 :style {:color (str (theme-pull :theme/editor-font-color nil) 35)
                         ;:border "2px solid yellow"
                         }
                 :children [[re-com/box
                             :size "none"
                             :width "300px"
                             :child [bricks/magic-table :viz-shapes0-list* [:viz-shapes0-sys] 7 8 []]]
                                        ;;  [re-com/box
                                        ;;   :size "auto"
                                        ;;   :child [bricks/magic-table :viz-shapes-list* [:viz-shapes-sys] 7 9 []]]

                            (if grid-reco?
                              [re-com/box
                               :size "none"
                               :width "880px"
                             ;:style {:padding-left "80px"}
                               :child [bricks/magic-table :recos-list* [:recos-sys] 19 8
                                       [;(when shape-picked? :shape_name)
                                        :query_map_str
                                        :table_name :context_hash :connection_id :h :w :condis :viz_map
                                                   ;(when combo-picked? :combo_edn)
                                        :combo_hash]]]

                              (let [] ;; get the next 6 graphs and render?
                                [re-com/h-box :children
                                 [[re-com/v-box
                                   :size "auto"
                                   :height "380px"
                                   :width "770px"
                                   :children [[re-com/h-box
                                               :size "auto"
                                               :children [[preview-container preview-maps preview-keys 0]
                                                          [preview-container preview-maps preview-keys 1]
                                                          [preview-container preview-maps preview-keys 2]]]
                                              [re-com/h-box
                                               :size "auto"
                                               :children [[preview-container preview-maps preview-keys 3]
                                                          [preview-container preview-maps preview-keys 4]
                                                          [preview-container preview-maps preview-keys 5]]]]]
                                  [re-com/v-box
                                   :size "auto"
                                   :height "380px"
                                   :style {:border-top "1px solid #9973e066"
                                           :overflow "hidden"}
                                   :children (let []
                                               (for [c (range pages)]
                                                 [re-com/box
                                                  :child (str c)
                                                  :height "19px"
                                                  :align :end
                                                  :justify :center
                                                  :attr {:on-click #(re-frame/dispatch [::bricks/set-recos-page c])}
                                                  :style {:padding-right "4px"
                                                          :cursor "pointer"
                                                          :font-size "12px"
                                                          :color (if (= c recos-page)
                                                                   (theme-pull :theme/editor-background-color nil) ;;"#000000"
                                                                   (theme-pull :theme/editor-font-color nil))
                                                          :background-color (if (= c recos-page)
                                                                              "#9973e0" ;"darkcyan"
                                                                              "inherit")
                                                          :border-bottom "1px solid #9973e066"
                                                          :border-right "1px solid #9973e066"}]))
                                   :width "30px"]]]))]]]]))

(defn editor-panel-viz []
  (let [all-sql-call-keys @(re-frame/subscribe [::bricks/all-sql-call-keys])
        all-sql-call-keys-str (for [e all-sql-call-keys] (ut/replacer (ut/unkeyword e) "-" "_"))
        sql-params (into {} (for [k [:viz-tables-sys/table_name :viz-shapes0-sys/shape]]
                              {k @(re-frame/subscribe [::conn/clicked-parameter-key [k]])}))
        sql-calls {:viz-tables-sys {:select [:table_name [[:count 1] :recos]]
                                    :from [:viz_recos_vw]
                                    ;:where [:in :table_name all-sql-call-keys-str]
                                    :where [:and [:not [:like :table_name "query_preview%"]]
                                            (let [lst (vec (cons :or (vec (for [t all-sql-call-keys-str] [:= :table_name t]))))]
                                              ;(tap> [lst])
                                              (if (> (count (flatten lst)) 1) lst false))]
                                    :order-by [:table_name]
                                    :group-by [:table_name]}
                   :viz-shapes0-sys {:select [[:shape_name :shape] [[:count 1] :recos]]
                                     :from [[:viz_recos_vw :vvt]]
                                     :where [:= :table_name :viz-tables-sys/table_name] ;; :viz-tables-sys/table_name-name
                                     :group-by [:shape_name]
                                    ;:group-by [:db_schema :db_catalog :connection_id :table_name]
                                    ;:order-by [:schema_cat :table_name]
                                     }
                   :viz-shapes-sys {:select [:combo_edn [[:count 1] :recos]]
                                    :from [[:viz_recos_vw :vvt]]
                                    ;:order-by [[:score :desc]]
                                    :where [:and
                                            (when (not (nil? (get sql-params :viz-shapes0-sys/shape)))
                                              [:= :shape_name :viz-shapes0-sys/shape])
                                            [:= :table_name :viz-tables-sys/table_name]] ;; :viz-tables-sys/table_name-name
                                    :group-by [:combo_edn]
                                    ;:group-by [:db_schema :db_catalog :connection_id :table_name]
                                    ;:order-by [:schema_cat :table_name]
}}  ;;  (re-frame/subscribe [::conn/clicked-parameter-key [:viz-tables-sys/table_name]])
        ]
    ;(tap> (vec (cons :or (vec (for [t all-sql-call-keys-str] [:= :table_name t])))))
    ;(tap> sql-params)
    (dorun (for [[k v] sql-calls]
             (let [query (walk/postwalk-replace sql-params v)
                   data-exists? @(re-frame/subscribe [::conn/sql-data-exists? [k]])
                   unrun-sql? @(re-frame/subscribe [::conn/sql-query-not-run? [k] query])]
               (when (or (not data-exists?) unrun-sql?)
                 (conn/sql-data [k] query)))))
    [re-com/h-box
     :size "auto"
     :style {:color (str (theme-pull :theme/editor-font-color nil) 35)}
     :children [[re-com/v-box
                 :children [[re-com/box
                             :size "auto"
                             :child [bricks/magic-table :viz-tables-list* [:viz-tables-sys] 7 5 []]]
                            [re-com/box
                             :size "auto"
                             :child [bricks/magic-table :viz-shapes0-list* [:viz-shapes0-sys] 7 5 []]]]]
                [re-com/box
                 :size "auto"
                 :child [bricks/magic-table :viz-shapes-list* [:viz-shapes-sys] 7 10 []]]]]))

(defn editor-panel-status []
  (let [;all-sql-call-keys @(re-frame/subscribe [::bricks/all-sql-call-keys])
       ; all-sql-call-keys-str (for [e all-sql-call-keys] (ut/replacer (ut/unkeyword e) "-" "_"))
        sql-params (into {} (for [k [:viz-tables-sys/table_name :viz-shapes0-sys/shape]]
                              {k @(re-frame/subscribe [::conn/clicked-parameter-key [k]])}))
        client-name (str @(re-frame/subscribe [::bricks/client-name]))
        sql-calls {:status-sys {:select [:*]
                                :from [:latest_status]
                                :where [:= :client_name client-name]
                                ;:where [:in :table_name all-sql-call-keys-str]
                                ;:where [:and [:not [:like :table_name "query_preview%"]]
                                ;        (vec (cons :or (vec (for [t all-sql-call-keys-str] [:= :table_name t]))))]
                                ;:order-by [:table_name]
                                ;:group-by [:table_name]
                                }}]
    ;(tap> (vec (cons :or (vec (for [t all-sql-call-keys-str] [:= :table_name t])))))
    ;(tap> sql-params)
    (dorun (for [[k v] sql-calls]
             (let [query (walk/postwalk-replace sql-params v)
                   data-exists? @(re-frame/subscribe [::conn/sql-data-exists? [k]])
                   unrun-sql? @(re-frame/subscribe [::conn/sql-query-not-run? [k] query])]
               (when (or (not data-exists?) unrun-sql?)
                 (conn/sql-data [k] query)))))
    [re-com/box
     :size "auto"
     :child [bricks/magic-table :status-sys-list* [:status-sys] 11 20 [:ts :client_name]]]))

(defn editor-panel-metadata-files []
  (let [sql-calls {:files-sys {:select [:*]
                               :order-by [[:screen_name :asc]]
                               :from [:screens]}
                  ; :blocks-sys {:select [:*]
                  ;              :from [:blocks]
                  ;              :where [:= :connection_id :connections-sys/connection_id]
                  ;              :group-by [:db_schema :db_catalog :connection_id :table_name]
                  ;              :order-by [:schema_cat :table_name]}
                   }
        sql-params (into {} (for [k [:connections-sys/connection_id]]
                              {k @(re-frame/subscribe [::conn/clicked-parameter-key [k]])}))]
    (dorun (for [[k v] sql-calls]
             (let [query (walk/postwalk-replace sql-params v)
                   data-exists? @(re-frame/subscribe [::conn/sql-data-exists? [k]])
                   unrun-sql? @(re-frame/subscribe [::conn/sql-query-not-run? [k] query])]
               (when (or (not data-exists?) unrun-sql?)
                 (conn/sql-data [k] query)))))
    [re-com/h-box
     :size "auto"
     :style {:color (str (theme-pull :theme/editor-font-color nil) 35)}
     :children [;[re-com/box
                ; :size "auto"
                ; :child [bricks/magic-table :system-connections-list* [:connections-sys] 3 10 [:connection_id]]]
                [re-com/box
                 :size "auto"
                 :child [bricks/magic-table :files-list* [:files-sys] 11.5 10 [:file_path :ts]]]]]))

(defn editor-panel-metadata-kits []
  (let [;market (group-by :package-name @(re-frame/subscribe [::bricks/all-kits])) ;
        ;; market {:boo [{:kit-name :boogers :name "boogs" :description "market placeholder" :package-name :boo}
        ;;               {:kit-name :boogers2 :name "boogs2" :description "market placeholder2" :package-name :boo}]}
        market (group-by :package-name @(re-frame/subscribe [::bricks/market-kits]))
        pkits1 (group-by :package-name @(re-frame/subscribe [::bricks/installed-kits]))
        curr @(re-frame/subscribe [::conn/clicked-parameter-key [:kits-sys/enabled]])
        kit-market {:installed-packages pkits1
                    :available-packages market}
        hackreact [@db/kit-browser]]
    ;(tap> [:pkits1 pkits1])
    [re-com/v-box
     :size "auto"
     :style {:color (str (theme-pull :theme/editor-font-color nil)) :margin-top "6px"
             :overflow "auto"}
     :children (conj
                (for [[typer pkits] kit-market]

                  [re-com/v-box
                   :children [[re-com/box
                               :padding "10px"
                               :style {:font-size "21px"
                                       :margin-top "15px"
                                       :border-bottom (str "3px solid " (theme-pull :theme/editor-outer-rim-color nil))
                                       :color (theme-pull :theme/editor-outer-rim-color nil)}
                               :child [re-com/h-box
                                       :gap "5px" :justify :between :align :center
                                       :children [(str (try (name typer) (catch :default _ (str typer))))
                                                  [re-com/box
                                                   :style {:opacity 0.33
                                                           :font-size "15px"}
                                                   :child (str " (" (count pkits) " package" (when (> (count pkits) 1) "s") ", " (count (flatten (vals pkits))) " kits)")]]]]

                              (if (empty? pkits)
                                [re-com/box
                                 :size "none" :align :center :justify :center :height "60px"
                                 :child (if (= typer :installed-packages)
                                          "(no kits installed)" "(no installable kits found)")]
                                [re-com/v-box
                                 :style {:margin-left "10px"}
                                 :children (for [[package-name kits] pkits]
                                             [re-com/v-box
                                          ; :style {:border "1px solid #ffffff03"}
                                              :children [[re-com/box
                                                          :padding "9px"
                                                          :style {:font-size "20px"
                                                                  :font-weight 700}
                                                          :child (str (try (name package-name) (catch :default _ (str package-name))))]
                                                         [re-com/v-box
                                                          :style {:margin-left "19px"}
                                                          :children (for [{:keys [description fn when name icon author run-on repl installed? kit-name package-name]} kits
                                                                          :let [disabled? (true? (not (some #(= % [package-name kit-name]) curr)))
                                                                                selected? (= @db/kit-browser [package-name kit-name])]]
                                                                      [re-com/h-box
                                                                       :padding "5px" :gap "22px"
                                                                       :attr {:on-click #(reset! db/kit-browser (if selected?
                                                                                                                  nil
                                                                                                                  [package-name kit-name]))}
                                                                       :justify :between :align :center
                                                                       :style {;:border "1px solid #ffffff08"
                                                                               :cursor "pointer"
                                                                              ; :color (if (= [package-name kit-name] @db/kit-browser)
                                                                              ;          (theme-pull :theme/editor-grid-selected-font-color nil)
                                                                              ;          "inherit")
                                                                               ;:background-color (if (= [package-name kit-name] @db/kit-browser)
                                                                               ;                    (theme-pull :theme/editor-grid-selected-background-color nil)
                                                                               ;                    "inherit")
                                                                               :border (if (= [package-name kit-name] @db/kit-browser)
                                                                                         (str "3px solid " (theme-pull :theme/editor-grid-selected-background-color nil))
                                                                                         "3px solid #ffffff08")}
                                                                       :children [[re-com/box
                                                                                   :min-width "100px"
                                                                                   :style {:font-size "17px"
                                                                                           :opacity (if (and (not selected?) disabled?) 0.3 1)
                                                                                           :text-decoration (if (and installed?
                                                                                                                     disabled?) "line-through" "")
                                                                                           :font-weight 700}
                                                                                   :child (str name)]
                                                                                  [re-com/box
                                                                                   :size "auto"
                                                                                   :style {:opacity (if (and (not selected?) disabled?) 0.3 1)}
                                                                                   :child (str description)]]])]]])])]])
                [re-com/box
                 :padding "8px"
                 :style {:margin-left "10px" :opacity 0.33}
                 :child "Kits are a great way to expand Rabbit functionality. Add customization for your data needs, or execute seperate arbitrary functions (even on another server) that receive query result & metadata - that can surface new insights, helpers, or even fun 'block builders'."])]))

(defn editor-panel-metadata-ext-kits []
  (let [pkits1 (group-by :package-name @(re-frame/subscribe [::bricks/installed-kits]))
        market (group-by :package-name @(re-frame/subscribe [::bricks/market-kits]))
        pkits (merge pkits1 market)
        hackreact [@db/kit-browser]
        settings-param-kp [:kits-sys]
        kit-body (filter #(= (get % :kit-name) (second @db/kit-browser)) (get pkits (first @db/kit-browser) {}))
        display-map (dissoc (first kit-body) :fn :when)
       ; display-map (assoc display-map :image-url "images/outliers-image.png")
        curr @(re-frame/subscribe [::conn/clicked-parameter-key [:kits-sys/enabled]])
        install-box [re-com/h-box
                     :height "60px"
                     :size "none"
                     :padding "10px"
                     :justify :between
                     :children [[re-com/box :size "auto" :align :center :justify :center
                                 :style {:border "1px solid white"
                                         :cursor "pointer"}
                                 :attr {:on-click #()}
                                 :child "download and install?"
                                 :width "240px"]]]
        enabled? (some (fn [x] (= x @db/kit-browser)) curr)
        enable-box [re-com/h-box
                    :height "60px"
                    :size "none"
                    :padding "10px"
                    :justify :between
                    :children [[re-com/box :size "auto" :align :center :justify :center
                                :style {;:border "1px solid white"
                                        :cursor "pointer"
                                        :font-size "28px"
                                        :color (when enabled? (theme-pull :theme/editor-grid-selected-background-color nil))
                                        :border (when enabled?
                                                  (str "3px solid " (theme-pull :theme/editor-grid-selected-background-color nil)))}
                                :attr {:on-click #(let [curr (if (nil? curr) [] curr)]
                                                    (re-frame/dispatch [::conn/click-parameter
                                                                        settings-param-kp
                                                                        {:enabled (vec (conj curr @db/kit-browser))}]))}
                                :child "enabled"
                                :width "240px"]
                               [re-com/box  :size "auto" :align :center :justify :center
                                :style {;:border "1px solid white"
                                        :cursor "pointer"
                                        :font-size "28px"
                                        :text-decoration (when (not enabled?) "line-through")
                                        :color (when (not enabled?) (theme-pull :theme/editor-grid-selected-background-color nil))
                                        :border (when (not enabled?)
                                                  (str "3px solid " (theme-pull :theme/editor-grid-selected-background-color nil)))}
                                :attr {:on-click #(re-frame/dispatch [::conn/click-parameter
                                                                      settings-param-kp
                                                                      {:enabled (vec (remove (fn [x] (= x @db/kit-browser)) curr))}])}
                                :child "disabled"
                                :width "240px"]]]]
    ;(tap> [:boo curr (merge market pkits1) pkits1])

    (if (empty? kit-body)
      [re-com/box
       :size "auto"
       :align :center :justify :center
       :child "(select a kit function or package name)"]

      [re-com/v-box
       :size "auto"
       :padding "6px"
       :style {:color (str (theme-pull :theme/editor-font-color nil) 35)
               :overflow "auto"
               ;:border "1px solid green"
               }
       :children    [(if (get display-map :installed?) enable-box install-box)
                     [re-com/v-box
                      :children [[re-com/v-box :children [[re-com/gap :size "8px"]
                                                          [re-com/v-box
                                                           :padding "8px" :gap "4px"
                                                           :style {:font-size "27px"
                                                                   :color (theme-pull :theme/editor-font-color nil)}
                                                           :children [[re-com/h-box
                                                                       :justify :between :align :center
                                                                       :children [[re-com/box
                                                                                   :child (get display-map :name)]
                                                                                  (cond (cstr/starts-with? (str (get display-map :icon)) "zmdi")
                                                                                        [re-com/md-icon-button :src (at)
                                                                                         :md-icon-name (get display-map :icon)
                                                                                         :style {;:background-color (theme-pull :theme/editor-rim-color "#a3a3a3") ;"#00000000"

                                                                                                ;:cursor "grab"
                                                                                                ;:height "15px"
                                                                                                ;:margin-top "-2px"
                                                                                                ;:font-size "19px"
                                                                                                 }]
                                                                                        :else [re-com/box
                                                                                               :style {:font-size "13px"}
                                                                                               :child "no-icon"])]]
                                                                      [re-com/box
                                                                         ;;:align :end :just
                                                                       :style {:font-size "17px"}
                                                                       :child (get display-map :description)]]]
                                                          [re-com/gap :size "12px"]
                                                          (when (get display-map :image-url)
                                                            [re-com/box
                                                             :align :center :justify :center
                                                             :style {}
                                                             :child [:img {:src (get display-map :image-url)
                                                                           :style {:border-radius "20px"}
                                                                           :width "480px"}]])
                                                          (when (get display-map :image-url) [re-com/gap :size "12px"])]]
                                 [shape/map-boxes2 display-map "no-block" "full-config-map" [] "no-block" "map"]]]]])))

(defonce param-scrubber? (reagent/atom {}))
(defonce param-search (reagent/atom {}))

(defn editor-panel-metadata-params [single-width single-height key-type]
  (let [;theme-scrubber? false
        ;key-type :param
        ;data-key [:params :views :params]
        param-map @(re-frame/subscribe [::bricks/user-parameters key-type])
        ;selected-panel-map {:param1 123
        ;                    :param2 [2 3 4]}
        ]
   ;;; (tap> [:param-map @(re-frame/subscribe [::bricks/user-parameter-doubles])])

    [re-com/v-box
     :gap "2px"
     :children [[re-com/h-box
                 :justify :between
                 :children
                 [[re-com/box
                   :child (str key-type "/*")
                   :align :center
                   :style {:color (theme-pull :theme/editor-outer-rim-color nil) ;"orange"
                           :padding-left "8px"
                           :font-weight 700
                           :font-family (theme-pull :theme/monospaced-font nil) ;"Fira Code"
                           }]
                  (when (get @param-scrubber? key-type)
                    [re-com/box
                     :child [re-com/input-text
                             :model (get @param-search key-type)
                             :on-change #(swap! param-search assoc key-type %)
                             :height "24px"
                             :width "200px"
                             :style {:background-color (theme-pull :theme/editor-background-color nil)
                                       ;:color (str (theme-pull :theme/editor-font-color nil) 33)
                                       ;:border (str "1px solid " (theme-pull :theme/editor-font-color nil) 22)
                                     :border (str "1px solid " (theme-pull :theme/editor-outer-rim-color nil))}
                             :change-on-blur? false
                               ;:placeholder "* wildcard search *"
                             ]
                     :style {:color (theme-pull :theme/editor-outer-rim-color nil) ;"orange"
                             :padding-right "18px"
                             :font-weight 700
                             :font-family (theme-pull :theme/monospaced-font nil) ;"Fira Code"
                             }])]]
                (if (get @param-scrubber? key-type) ;theme-scrubber?
                  [re-com/box
                   :size "none"
                   :width (px single-width)
                   :height (px (- single-height 90)) ;;; [view? keypath-map view-key]
                   :child [bricks/scrubber-panel
                           true ; view?
                             ;{} ;param-map ; keypath-map
                           @(re-frame/subscribe [::bricks/keypaths-in-params key-type])
                           key-type (get @param-search key-type) {:fm true}] ; view-key (update kp)
                   :style {:overflow "auto"}]

                  [bricks/panel-param-box key-type nil
                   (+ 17 single-width) (- single-height 66) param-map])

                [re-com/box
                 :child (str "scrubber " (if (get @param-scrubber? key-type) "on" "off"))
                 :attr {:on-click #(swap! param-scrubber? assoc key-type (not (get @param-scrubber? key-type false)))}
                 :style {:padding-left "18px"
                         :color (if (get @param-scrubber? key-type) "yellow" "inherit")
                         :cursor "pointer"}]]]))

(defn editor-panel-metadata-ext-files []
  (let [sql-params (into {} (for [k [:files-sys/file_path]]
                              {k @(re-frame/subscribe [::conn/clicked-parameter-key [k]])}))
        sql-calls {:blocks-sys {:select [:block_key :block_name :queries :views :block_data :view_names :query_names]
                                :from [:blocks]
                                :where [:= :file_path :files-sys/file_path]
                                ;:group-by [:db_schema :db_catalog :connection_id :table_name]
                                :order-by [:block_name]}}

        ;gmode @data-browser-system-mode
        ;attribs? (= gmode :attribs)
        ;combos? (= gmode :combos)
        ;grid? (= gmode :data)
        ]
    ;;(tap> [:test sql-params])
    (dorun (for [[k v] sql-calls]
             (let [query (walk/postwalk-replace sql-params v)
                   data-exists? @(re-frame/subscribe [::conn/sql-data-exists? [k]])
                   unrun-sql? @(re-frame/subscribe [::conn/sql-query-not-run? [k] query])]
               (when (or (not data-exists?) unrun-sql?)
                 (conn/sql-data [k] query)))))

    [re-com/h-box
     :size "auto"
     :style {:color (str (theme-pull :theme/editor-font-color nil) 35)}
     :children [;[re-com/box
                ; :size "auto"
                ; :child [bricks/magic-table :system-connections-list* [:connections-sys2] 3 10 [:connection_id]]]
                [re-com/v-box
                 :size "auto"
                 :children
                 [[re-com/box
                   :size "auto"
                   :child [bricks/magic-table :blocks-list*
                           [:blocks-sys] 11 10 [:screen_name :ts :block_data :view_names :query_names]]]]]]]))

(defn editor-panel-metadata-ext []
  (let [sql-params (into {} (for [k [:connections-sys/connection_id :tables-sys/table_name]]
                              {k @(re-frame/subscribe [::conn/clicked-parameter-key [k]])}))
        sql-calls {:fields-sys {:select [:field_name :field_type :data_type]
                                :from [:fields]
                                :where [:and
                                        [:= :connection_id :connections-sys/connection_id]
                                        [:= :table_name :tables-sys/table_name]
                                        [:<> :field_type "derived"]
                                        [:<> :field_type "special"]]
                                ;:group-by [:db_schema :db_catalog :connection_id :table_name]
                                :order-by [:field_name]}
                   ;:attribs-sys {:select [:*]
                   ;              :from [(keyword (str "vw_attribs__" (get sql-params :tables-sys/table_name)))]
                   ;              :where [:<> :field_name "*"]}
                   ;:combos-sys {:select [:*] :from [(keyword (str "vw_combos__" (get sql-params :tables-sys/table_name)))]}
                   }

        gmode @data-browser-system-mode
        attribs? (= gmode :attribs)
        combos? (= gmode :combos)
        grid? (= gmode :data)]
    ;;(tap> [:test sql-params])
    (dorun (for [[k v] sql-calls]
             (let [query (walk/postwalk-replace sql-params v)
                   data-exists? @(re-frame/subscribe [::conn/sql-data-exists? [k]])
                   unrun-sql? @(re-frame/subscribe [::conn/sql-query-not-run? [k] query])]
               (when (or (not data-exists?) unrun-sql?)
                 (conn/sql-data [k] query)))))

   ; (when (or attribs? combos?)
   ;   (let [vw-k (if attribs? (attrib-view k) (combo-view k))
   ;         query {:select [:*] :from [vw-k]}
   ;         data-exists? @(re-frame/subscribe [::conn/sql-data-exists? [vw-k]])
   ;         unrun-sql? @(re-frame/subscribe [::conn/sql-query-not-run? [k] query])]
   ;     (when (or (not data-exists?) unrun-sql?)
   ;       (conn/sql-data [vw-k] query))))

    [re-com/h-box
     :size "auto"
     :style {:color (str (theme-pull :theme/editor-font-color nil) 35)}
     :children [;[re-com/box
                ; :size "auto"
                ; :child [bricks/magic-table :system-connections-list* [:connections-sys2] 3 10 [:connection_id]]]
                [re-com/v-box
                 :size "auto"
                 :children
                 [[re-com/box
                   :size "auto"
                   :child [bricks/magic-table :system-fields-list*
                           (cond grid? [:fields-sys]
                                 attribs? [:attribs-sys]
                                 combos? [:combos-sys]
                                 :else [:fields-sys]) 11 10 [:db_type :db_schema :table_type :database_name
                                                             :db_catalog :table_name :connection_id
                                                             :database_version :key_hash :context_has]]]]]]]))





(defn editor-panel-metadata-viz []
  (let [sql-params (into {} (for [k [:viz-tables-sys/table_name :viz-shapes0-sys/shape :viz-shapes-sys/combo_edn]]
                              {k @(re-frame/subscribe [::conn/clicked-parameter-key [k]])}))
        combo-picked? (not (nil? (get sql-params :viz-shapes-sys/combo_edn)))
        shape-picked? (not (nil? (get sql-params :viz-shapes0-sys/shape)))
        sql-calls {:recos-sys {:select [:*]
                               :from [:viz_recos_vw]
                               :order-by [[:score :desc]]
                               :where [:and
                                       [:= :table_name :viz-tables-sys/table_name]
                                       (when combo-picked?
                                         [:= :combo_edn :viz-shapes-sys/combo_edn])
                                       (when shape-picked?
                                         [:= :shape_name :viz-shapes0-sys/shape])]}}]
    ;;(tap> [:test sql-params])
    (dorun (for [[k v] sql-calls]
             (let [query (walk/postwalk-replace sql-params v)
                   data-exists? @(re-frame/subscribe [::conn/sql-data-exists? [k]])
                   unrun-sql? @(re-frame/subscribe [::conn/sql-query-not-run? [k] query])]
               (when (or (not data-exists?) unrun-sql?)
                 (conn/sql-data [k] query)))))

    [re-com/h-box
     :size "auto"
     :style {:color (theme-pull :theme/editor-font-color nil)}
     :children [[re-com/v-box
                 :size "auto"
                 :children
                 [[re-com/box
                   :size "auto"
                   :child [bricks/magic-table :recos-list* [:recos-sys] 11 10 [(when shape-picked? :shape_name) :query_map_str
                                                                               :table_name :context_hash :connection_id :h :w :condis :viz_map
                                                                               (when combo-picked? :combo_edn)]]]]]]]))

(defn editor-panel-metadata-status []
  (let [pending @(re-frame/subscribe [::wfx/pending-requests http/socket-id])
        client-name @(re-frame/subscribe [::bricks/client-name])
        pending (remove #(cstr/ends-with? (str (get-in % [:message :ui-keypath 0])) "-sys") pending)]
    ;(tap> [:pending pending])
;;     [re-com/v-box
;;      :size "auto"
;;      :style {:color (theme-pull :theme/editor-font-color nil)}
;;      :children (for [e pending]
;;                  [re-com/v-box :children
;;                   [[re-com/box :child (subs (str (get-in e [:message :honey-sql :select])) 0 90)
;;                     :style {:opacity 0.5} :align :end]
;;                    [re-com/box :child (subs (str (get-in e [:message :ui-keypath])) 0 90)]]]
;;                  )]

    [re-com/box
     :height "430px" ;(px ttl-height)
     :size "none"
     :padding "3px"
     :style {:overflow "auto"}
     :child [re-com/v-box
             :size "none"
             :style {:font-size "9px"
                     ;:transition "all 5s ease-in-out"
                     :color "#ffffff"}
             :children (for [e pending
                             :let [l (get-in e [:message :honey-sql :select])
                                   ff (get-in e [:message :honey-sql :from])
                                   p (get-in e [:message :ui-keypath])]]
                         [re-com/v-box
                          :size "none" :padding "4px"
                          :justify :between ;;align :end
                          :style {:border "1px solid green"}
                          :children [[re-com/v-box
                                      :size "auto"
                                      :align :start
                                      ;:style {:font-size "9px"}
                                      :children (if (and (nil? l) (nil? ff))

                                                  (do (when (= client-name :glamorous-carmine-leopard-exiled-from-archipelago)
                                                        (tap> [:msg (get e :message)
                                                               @(re-frame/subscribe [::bricks/runstream-running?])]))
                                                      [[re-com/box :child (str (get e :message))]])

                                                  [[re-com/box :child (str ":s - " l)]
                                                   [re-com/box :child (str ":f - " ff)
                                                    :style {:color "#ffffff88"}]])]
                                     [re-com/box
                                      :size "auto"
                                      :justify :end :align :end
                                      :style {:color "#ffffff88"}
                                  ;:child (str (deref p))
                                      :child (str ":kp - " p)]]])]]))




;(defonce file-mode? (reagent/atom false))
;(defonce db/editor-mode (reagent/atom :meta))


(defn ui-debugger []
  (let [bw 33 bh 11
        ttl-height (* bh bricks/brick-size)
        sched @(re-frame/subscribe [::bricks/query-schedule])
        params (merge sched
                      {"client-name" @(re-frame/subscribe [::bricks/client-name])
                       "[::bricks/last-heartbeat]" @(re-frame/subscribe [::bricks/last-heartbeat])
                       "db/context-modal-pos" @db/context-modal-pos
                       "db/editor-mode" @db/editor-mode
                       "db/auto-refresh?" @db/auto-refresh?
                       "db/param-filter" @db/param-filter
                       "data-browser-query" @db/data-browser-query
                       "[::bricks/selected-block-map]" @(re-frame/subscribe [::bricks/selected-block-map])
                       "bricks/dragging?" @bricks/dragging?
                       "bricks/dyn-dropper-hover (!!!!)" @bricks/dyn-dropper-hover
                       "bricks/dragging-size" @bricks/dragging-size
                       "bricks/dragging-body" @bricks/dragging-body
                       "db/last-mouse-activity" @db/last-mouse-activity
                       ;"bricks/dyn-dropper-hover" @bricks/dyn-dropper-hover
                       "bricks/mad-libs-view" @bricks/mad-libs-view
                       "bricks/mad-libs-top?" @bricks/mad-libs-top?
                       "bricks/mouse-dragging-panel?" @bricks/mouse-dragging-panel?
                       "bricks/hover-square" @bricks/hover-square
                       "bricks/on-block?" @bricks/on-block?
                       "bricks/dragging-editor?" @bricks/dragging-editor?
                       "bricks/over-block?" @bricks/over-block?
                       "bricks/new-block-atom" @bricks/new-block-atom
                ;"[::wfx/pending-requests http/socket-id]" @(re-frame/subscribe [::wfx/pending-requests http/socket-id])
                       })
        atom-sizes (ut/calculate-atom-sizes {:replacer-atom ut/replacer-atom
                                             ;:db/flow-results db/flow-results
                                             :db/scrubbers db/scrubbers
                                             :ut/is-base64-atom ut/is-base64-atom
                                             :tu/is-large-base64-atom ut/is-large-base64-atom
                                             :websocket-fx.core/CONNECTIONS websocket-fx.core/CONNECTIONS
                                             :re-frame.db/app-db re-frame.db/app-db
                                             :ut/clean-sql-atom ut/clean-sql-atom
                                             :ut/deep-flatten-atom ut/deep-flatten-atom
                                             :ut/format-map-atom ut/format-map-atom
                                             :ut/body-set-atom ut/body-set-atom
                                             :ut/data-typer-atom ut/data-typer-atom
                                             :ut/coord-cache ut/coord-cache})
        ;; _ (tap> [:atom-sizes? atom-sizes])
        params (merge params atom-sizes)
        params (vec (for [[k v] params] [k (str k) v]))] ;; fake middle key since they are NOT all keywords or strings and cant be naturally sorted
    [re-com/box
     :height "430px" ;(px ttl-height)
     :size "none"
     :padding "3px"
     :style {:overflow "auto"}
     :child [re-com/v-box
             :size "none"
             :style {:font-size "11px" :color "#ffffff"}
             :children (for [[l _ p] (sort-by second (seq params))] ;(for [[l p] (sort params)] ;(vec (sort-by first (for [[ll pp] params] [ll pp])))] ;; ghetto cljs map->vec->fakemap sorting
                         [re-com/v-box
                          :size "none" :padding "4px"
                          :justify :between ;;align :end
                          :style {:border "1px solid green"}
                          :children [[re-com/box
                                      :size "auto"
                                      :align :start
                                      :style {:color (when (keyword? l)
                                                       (theme-pull :theme/editor-outer-rim-color nil) ;;"orange"
                                                       )}
                                      :child (str l)]
                                     [re-com/box
                                      :size "auto"
                                      :justify :end :align :end
                                      :style {:color "#ffffff88"}
                              ;:child (str (deref p))
                                      :child (if (integer? p) (str "*in " p " seconds")
                                                 ;(try (str (deref p)) (catch :default _ "err"))
                                                 (str p))]]])]]))

(re-frame/reg-event-db
 ::save-search-results
 (fn [db [_ res]]
   (assoc db :search-results res)))

(re-frame/reg-sub
 ::search-results
 (fn [db _]
   (get db :search-results)))

(defn search-panel-left [single-width single-height]
  (let [client-name @(re-frame/subscribe [::bricks/client-name])
        results @(re-frame/subscribe [::search-results])]
    [re-com/v-box
     :children [;[re-com/box
                ; :child "search-box"]
                [re-com/box
                 :padding "10px"
                 :child [re-com/input-text
                         :model @db/search-box
                 ;:change-on-blur? false
                         :on-change #(do (reset! db/search-box %)
                                         (when (not (empty? (cstr/trim (str %))))
                                           (re-frame/dispatch [::wfx/request :default
                                                               {:message    {:kind :search-groups
                                                                             :client-name client-name
                                                                             :max-hits 200
                                                                             :search-str (str %)}
                                                                :on-response [::save-search-results]
                                                                :timeout    500000}])))
                         :height "50px"
                         :width (px (- single-width 25))
                         :style {:background-color (theme-pull :theme/editor-background-color nil)
                                 :font-size "25px"
                                 ;:padding "9px"
                                 :font-weight 700
                                 :color (theme-pull :theme/editor-font-color nil) ;; (theme-pull :theme/editor-outer-rim-color nil)
                                 :border (str "1px solid " (theme-pull :theme/editor-outer-rim-color nil) 75)}]]
                [re-com/box
                 :height (px (- single-height 120))
                 :style {:border "1px solid cyan"
                         :overflow "auto"}
                 :size "none"
                 :child [re-com/v-box
                         :padding "10px"
                         :style {:color (theme-pull :theme/editor-font-color nil)}
                         :children (for [[r cnt] results]
                                     [re-com/h-box
                                      :padding "4px"
                                      :justify :between
                                      :align :center 
                                      :height "30px"
                                      :style {:border "1px solid darkcyan"}
                                      :children [[re-com/box :child (str r)]
                                                 [re-com/box :child (str (ut/nf cnt))]]])]]]
     :size "auto"
     :width (px single-width)
     :height (px single-height)
     ;:style {:border "2px solid red"}
     ]))

(defn search-panel-right [single-width single-height]
  [re-com/box
   :child "search"
   :size "auto"
   :width (px single-width)
   :height (px single-height)
   :style {:border "2px solid yellow"}])

(defn editor-panel [bricks-wide bricks-tall]
  (let [selected-panel-map @(re-frame/subscribe [::bricks/selected-block-map])
        selected-block @(re-frame/subscribe [::bricks/selected-block])
        sql-calls @(re-frame/subscribe [::bricks/panel-sql-calls selected-block])
        views @(re-frame/subscribe [::bricks/panel-views selected-block])
        system-panel? (or (= selected-block "none!") (nil? selected-block))
        ;we could hit (get selected-panel-map :queries), but the sub filters out the :vselects
        first-data-key (first (keys sql-calls))
        first-view-key (first (keys views))
        data-key (if
                  (get @db/data-browser-query selected-block)
                   (get @db/data-browser-query selected-block)
                   first-data-key)
        data-key (if (nil? data-key) first-view-key data-key)
         ; view-key (cond (get @view-browser-query selected-block) (get @view-browser-query selected-block)
         ;                (nil? first-view-key) :base
         ;                :else first-view-key)
        queries? (not (empty? sql-calls))
        views? (not (empty? views))
        view-selected? (true? (or (and views? (some #(= % (get @db/data-browser-query selected-block)) (keys views)))
                                  (not queries?)))
          ;view-scrubbers? (get-in @scrubbers [selected-block view-key] false)
        panel-count (if system-panel? 3 (if (or views? queries?) 3 2))
        bricks-wide (if (or system-panel? queries? views?) bricks-wide (js/Math.ceil (* bricks-wide 0.66)))
        ttl-width (* bricks-wide bricks/brick-size)
        ttl-width-px (px ttl-width)
        ttl-height (* bricks-tall bricks/brick-size)
        ttl-height-px (px (+ 10 ttl-height))
        x-px (px (first @detached-coords)) ;(px (* x bricks/brick-size))
        y-px (px (last @detached-coords)) ;(px (* y bricks/brick-size))
          ;x-px-sb (px (+ 398 (first @detached-coords))) ;(px (* x bricks/brick-size))
          ;y-px-sb (px (+ 100 (last @detached-coords))) ;(px (* y bricks/brick-size))
        single-width-bricks (js/Math.floor (/ bricks-wide panel-count))
        single-width (* single-width-bricks bricks/brick-size)
        single-width-px (px single-width)
        hh1 [@db/mad-libs-waiting-room @db/data-browser-query-con @db/item-browser-mode] ;; reactivity hack!
        ;double-width (* single-width-bricks bricks/brick-size)
        ;double-width-px (px single-width)
        ;single-height-px (px (* (js/Math.floor (/ bricks-tall 4)) bricks/brick-size))
        single-height ttl-height
        single-height-px ttl-height-px
        click-params @(re-frame/subscribe [::bricks/all-click-params])
        all-sql-call-keys @(re-frame/subscribe [::bricks/all-sql-call-keys])
        all-vsql-call-keys @(re-frame/subscribe [::bricks/all-vsql-call-keys])
        sql-string (bricks/materialize-one-sql selected-block data-key)
        reco-selected @(re-frame/subscribe [::conn/clicked-parameter-key [:recos-sys/combo_hash]])
        reco-combo @(re-frame/subscribe [::conn/clicked-parameter-key [:recos-sys/combo_edn]])
        shape-name @(re-frame/subscribe [::conn/clicked-parameter-key [:recos-sys/shape_name]])
        reco-query @(re-frame/subscribe [::conn/clicked-parameter-key [:recos-sys/query_map]])
        reco-viz @(re-frame/subscribe [::conn/clicked-parameter-key [:recos-sys/viz_map]])
        reco-condis @(re-frame/subscribe [::conn/clicked-parameter-key [:recos-sys/condis]])
        reco-preview @(re-frame/subscribe [::bricks/reco-preview])
        reco-selected? (not (nil? reco-selected))
        mad-libs-combo? (not (empty? (get selected-panel-map :mad-libs-combo-hash)))
        mad-libs-combos (when mad-libs-combo?
                          @(re-frame/subscribe [::bricks/get-combo-rows
                                                selected-block (get selected-panel-map :mad-libs-combo-hash)]))
        screen-name (ut/unkeyword @(re-frame/subscribe [::bricks/screen-name]))
        screen-name-regex #"(.|\s)*\S(.|\s)*"
        websocket-status (select-keys @(re-frame/subscribe [::http/websocket-status]) [:status :datasets :panels :waiting])
        ;websocket-status @(re-frame/subscribe [::http/websocket-status])
        ]

   ; (tap> @bricks/dragging-editor?)

    (when (nil? (get @db/data-browser-query selected-block)) ;; at this point lets just populate the atom key with the tab
      (swap! db/data-browser-query assoc selected-block data-key))

    ;(tap> [:mad-libs-scrubber mad-libs-combo? mad-libs-combo])

    ;(tap> [reco-selected reco-preview])
    ;(tap> [:pending (for [e @(re-frame/subscribe [::wfx/pending-requests http/socket-id])]
    ;                   (get-in e [:message :ui-keypath])
    ;                 ; e
    ;                  ) ])

    (when (and (not (= reco-selected reco-preview)) reco-selected?)
      (insert-hidden-reco-preview reco-selected reco-viz reco-query reco-condis reco-combo shape-name true)
             ;(insert-hidden-reco-preview reco-selected reco-viz reco-query reco-condis false)
      )

   ; (when (and (empty? (get websocket-status :open-subs)) (= (get websocket-status :status) :connected))
   ;   (debounce (re-frame/dispatch [::wfx/subscribe http/socket-id :server-push0 db/subscription]) 500) )
    ;(tap> [:views (get @db/data-browser-query selected-block) data-key view-key queries? views? view-selected? (keys sql-calls) (keys views)])
    ;(tap> [panel-count bricks-wide])
    ;(tap> @(re-frame/subscribe [::bricks/descendant-panels2 selected-block]))
    [rc/catch
     [re-com/h-box
      :size "none"
     ;:padding "6px"
      :children [[re-com/box
                  :size "none"
                  :child [re-com/v-box
                          :size "1"
                         ;:style {:background-color "#1f2430"}
                          :children [[re-com/box :padding "4px"
                                      :child [re-com/h-box
                                              :gap "6px"
                                              :children [[re-com/md-icon-button :src (at)
                                                          :md-icon-name "zmdi-arrows"
                                                          :style {:background-color (theme-pull :theme/editor-rim-color "#a3a3a3") ;"#00000000"
                                                                  :color (theme-pull :theme/editor-font-color nil)
                                                                  :cursor "grab"
                                                                  :height "15px"
                                                                  :margin-top "-2px"
                                                                  :font-size "19px"}
                                                          :attr {:on-mouse-down mouse-down-handler}]

                                                         [re-com/h-box
                                                          :size "auto"
                                                          :justify :between
                                                          :children [[re-com/box :child (if system-panel?
                                                                                          [re-com/h-box
                                                                                           :align :center
                                                                                           :justify :center
                                                                                           ;:justify :between
                                                                                           :gap "15px"
                                                                                           :children
                                                                                           [[re-com/box :child (str "db browser")
                                                                                             :style {;:opacity (if @file-mode? 0.4 1.0)
                                                                                                     :opacity (if (= @db/editor-mode :meta) 1.0 0.4)
                                                                                                     :cursor "pointer"}
                                                                                             :attr {:on-click #(reset! db/editor-mode :meta)}]
                                                                                            
                                                                                            [re-com/box :child (str "subscriptions")
                                                                                             :style {;:opacity (if @file-mode? 0.4 1.0)
                                                                                                     :opacity (if (= @db/editor-mode :search) 1.0 0.4)
                                                                                                     :cursor "pointer"}
                                                                                             :attr {:on-click #(reset! db/editor-mode :search)}]

                                                                                            ;; [re-com/box :child (str "viz suggestions")
                                                                                            ;;  :style {:opacity (if (= @db/editor-mode :viz) 1.0 0.4)
                                                                                            ;;          :cursor "pointer"}
                                                                                            ;;  :attr {:on-click #(reset! db/editor-mode :viz)}]

                                                                                            [re-com/box :child (str "viz recos")
                                                                                             :style {:opacity (if (= @db/editor-mode :vvv) 1.0 0.4)
                                                                                                     :cursor "pointer"}
                                                                                             :attr {:on-click #(reset! db/editor-mode :vvv)}]

                                                                                            [re-com/box :child (str "files")
                                                                                             :style {;:opacity (if @file-mode? 1.0 0.4)
                                                                                                     :opacity (if (= @db/editor-mode :files) 1.0 0.4)
                                                                                                     :cursor "pointer"}
                                                                                             :attr {:on-click #(reset! db/editor-mode :files)}]

                                                                                            [re-com/box :child (str "params")
                                                                                             :style {:opacity (if (= @db/editor-mode :params) 1.0 0.4)
                                                                                                     :cursor "pointer"}
                                                                                             :attr {:on-click #(reset! db/editor-mode :params)}]

                                                                                            [re-com/box :child (str "kits")
                                                                                             :style {;:opacity (if @file-mode? 1.0 0.4)
                                                                                                     :opacity (if (= @db/editor-mode :kits) 1.0 0.4)
                                                                                                     :cursor "pointer"}
                                                                                             :attr {:on-click #(reset! db/editor-mode :kits)}]

                                                                                            [re-com/box :child ;(str "*")
                                                                                             [re-com/md-icon-button :src (at)
                                                                                              :md-icon-name "zmdi-bug"
                                                                                              :style {;:background-color (theme-pull :theme/editor-rim-color "#a3a3a3") ;"#00000000"
                                                                                                      ;:color (str (theme-pull :theme/editor-font-color nil) 44) ;;"#ffffff44"
                                                                                                      :cursor "pointer"
                                                                                                      :height "15px"
                                                                                                      :padding-right "5px"
                                                                                                      :margin-top "-6px"
                                                                                                      :font-size "19px"}]
                                                                                             :style {:opacity (if (= @db/editor-mode :status) 1.0 0.4)
                                                                                                     :cursor "pointer"}
                                                                                             :attr {:on-click #(reset! db/editor-mode :status)}]

                                                                                            [re-com/md-icon-button :src (at)
                                                                                             :md-icon-name "zmdi-refresh"
                                                                                             :style {;:background-color (theme-pull :theme/editor-rim-color "#a3a3a3") ;"#00000000"
                                                                                                     :color (str (theme-pull :theme/editor-font-color nil) 44) ;;"#ffffff44"
                                                                                                     :cursor "pointer"
                                                                                                     :height "15px"
                                                                                                     :padding-right "5px"
                                                                                                     :margin-top "-6px"
                                                                                                     :font-size "19px"}
                                                                                             :attr {:on-click #(re-frame/dispatch
                                                                                                                [::bricks/refresh-meta-tables
                                                                                                                 @db/editor-mode])}]

                                                                                        ;;     [re-com/box
                                                                                        ;;      ;:size "auto"
                                                                                        ;;      :child (str "auto " (if @db/auto-refresh? "on" "off"))
                                                                                        ;;      ;:width "58px"
                                                                                        ;;      :attr {:on-click #(reset! db/auto-refresh? (not @db/auto-refresh?))}
                                                                                        ;;      :style {:background-color "pink"
                                                                                        ;;              :opacity (if @db/auto-refresh? 0.9 0.3)
                                                                                        ;;              :white-space "nowrap"
                                                                                        ;;              :overflow "hidden"
                                                                                        ;;              :padding-left "3px"
                                                                                        ;;              :padding-right "6px"
                                                                                        ;;              :cursor "pointer"
                                                                                        ;;              :font-weight 700
                                                                                        ;;              :color (theme-pull :theme/editor-background-color nil) ;;"#000000"
                                                                                        ;;              :border-radius "20px"}]
                                                                                            ]]

                                                                                          (str selected-block "'s panel map"))]
                                                         ;[re-com/box :child "refresh"
                                                         ; :style {:padding-right "15px"}]
                                                                     ]]]]
                                      :style {:font-weight 500
                                              :color (theme-pull :theme/editor-font-color nil)
                                              ;:cursor "grab"
                                              :border-radius "11px 0px 0px 0px"
                                              ;:background-color (theme-pull :theme/editor-rim-color "#a3a3a3")
                                              :background (str "linear-gradient(" (theme-pull :theme/editor-rim-color nil) ", transparent)")
                                              :user-select "none"}
                                      ;:attr {:on-mouse-down mouse-down-handler}
                                      ]

                                     (cond system-panel?

                                           (condp = @db/editor-mode ; @file-mode?

                                             :files [re-com/box :child [editor-panel-metadata-files]
                                                     :style {:padding-top "10px"}
                                                     :height (px (- ttl-height 40))]

                                             :kits [re-com/box :child [editor-panel-metadata-kits]
                                                    :style {:padding-top "10px"}
                                                    :height (px (- ttl-height 40))]

                                             :params [re-com/box :child [editor-panel-metadata-params single-width single-height :param]
                                                      :style {:padding-top "10px"}
                                                      :height (px (- ttl-height 40))]

                                             :meta [re-com/box :child [editor-panel-metadata]
                                                    :style {:padding-top "10px"}
                                                    :height (px (- ttl-height 40))]

                                             :viz [re-com/box :child [editor-panel-viz]
                                                   :style {:padding-top "10px"}
                                                   :height (px (- ttl-height 40))]

                                             :search [re-com/box 
                                                      :child [search-panel-metadata] ;;[search-panel-left single-width single-height]
                                                      :style {:padding-top "10px"}
                                                      :height (px (- ttl-height 40))]

                                             :vvv [re-com/box :child
                                              ; [re-com/box :child "hey" :size "1" :style {:border "2px solid yellow"}]
                                                   [editor-panel-viz2]
                                                   :style {:padding-top "10px"}
                                                   :height (px (- ttl-height 40))]

                                             :status [re-com/box :child [ui-debugger]
                                                      ;; :child [editor-panel-status] ;; <-- this is for the CSV "status table" system
                                                      :style {:padding-top "10px"}
                                                      :height (px (- ttl-height 40))])

                                           (= :viz-gen (get @db/data-browser-query selected-block)) ;; viz-gen?

                                           (let [src-table-id-str (last (get selected-panel-map :mad-libs-combo-hash))
                                                 combo-hash (first (get selected-panel-map :mad-libs-combo-hash))
                                                 opts @(re-frame/subscribe [::bricks/get-mad-libs-options selected-block src-table-id-str combo-hash])
                                                 shape-name (get opts :shape_name)
                                                 ;combo-name (get opts :combo_edn)
                                                 viz-shape @(re-frame/subscribe [::bricks/get-viz-shape shape-name])
                                                 color-keys [:YlGn :desc]
                                                 hacko @db/mad-libs-box
                                                 ;colors1 (vec (reverse (get-in ut/colorbrewer color-keys)))
                                                 color-keys-which (last (reverse (sort-by name (keys (get ut/colorbrewer (first color-keys))))))
                                                 colors1 (vec (if (= (last color-keys) :desc)
                                                                (reverse
                                                                 (get-in ut/colorbrewer [(first color-keys) color-keys-which]))
                                                                (get-in ut/colorbrewer [(first color-keys) color-keys-which])))
                                                 zero-color (if (= (last color-keys) :desc) (first colors1) (last colors1))
                                                 value-to-color (fn [value-vector color-vector value]
                                                                  (let [min-val (apply min value-vector)
                                                                        max-val (apply max value-vector)
                                                                        normalized (when (> (- max-val min-val) 0) ; Avoid division by zero
                                                                                     (/ (- value min-val) (- max-val min-val)))
                                                                        idx (int (Math/floor (* normalized (dec (count color-vector)))))]
                                                                    (if (and normalized (>= idx 0) (< idx (count color-vector)))
                                                                      (color-vector idx)
                                                                      nil)))
                                                 viz-shape-rules (first (filter #(= (get % :shape_name) shape-name) @(re-frame/subscribe [::conn/sql-data [:viz-shapes]])))
                                                 rounder (fn [num] (/ (js/Math.round (* num 100)) 100))
                                                 open-shape? (get-in @db/mad-libs-box [selected-block shape-name] false)
                                                 ;viz-shape (get viz-shapes shape-name)
                                                 axes-logic-map (try (edn/read-string (get viz-shape :axes_logic)) (catch :default _ {}))
                                                 opts-data (get opts :data)]
                                             ;(tap> [:is-shapes? viz-shape opts ])
                                             [re-com/v-box
                                              :children
                                              [[re-com/h-box
                                                :justify :between
                                                :children [[re-com/box ;:align :end  ;:align :center ;:justify :end
                                                            :style {:color (theme-pull :theme/editor-outer-rim-color nil)
                                                                    :font-family (theme-pull :theme/base-font nil)
                                                                    :transition_NOT "all 0.2s ease-in-out"
                                                                    :font-size "21px"
                                                                    :font-weight 700
                                                                    :margin-left "19px"}
                                                            :child (str shape-name)]

                                                           [re-com/md-icon-button :src (at)
                                                            :md-icon-name (if open-shape? "zmdi-chevron-down" "zmdi-chevron-up")
                                                            :attr {:on-click #(swap! db/mad-libs-box assoc-in [selected-block shape-name] (not open-shape?))}
                                                            :style {:color (theme-pull :theme/editor-outer-rim-color nil)
                                                                    :padding-right "16px"
                                                                    :font-size "25px"}]]]

                                               (when open-shape?
                                                 [re-com/box
                                                  :padding "8px"
                                                  ;:style {:overflow "auto"}
                                                  :height "435px"
                                                  :size "none"
                                                  ;:width (px (- single-width 60))
                                                  :child [bricks/read-only-clojure-box
                                                          single-width 0
                                                          (str ";; rules, viz & query shapes for :" shape-name "\n"
                                                               ";;  (defined in ./data/viz-shapes.edn) \n"
                                                               ";; ------------------------------------------------------- \n"
                                                               ";; base query map(s) \n"
                                                               (str (get viz-shape-rules :sql_maps))
                                                               ";; base viz / view shape \n"
                                                               (str (get viz-shape-rules :library_shapes))
                                                               ";; axes rules / logic \n"
                                                               (str (get viz-shape-rules :axes_logic)))]])

                                               [re-com/v-box
                                                :padding "6px"
                                                :gap "8px"
                                                :height (px (- ttl-height 34))
                                                :style {:overflow "auto" :transition_NOT "all 0.2s ease-in-out"}
                                                :children (for [[k o] (group-by #(first (first %)) opts-data)
                                                                :let [short-axes (-> (str k)
                                                                                     (ut/replacer  #"-field" "")
                                                                                     (ut/replacer  #":" ""))
                                                                      open? (get-in @db/mad-libs-box [selected-block short-axes] false)]]
                                                            [re-com/v-box ;:size "auto"
                                                             :padding "9px"
                                                             :justify :between
                                                             :style {:border (str "2px dashed " (theme-pull :theme/editor-outer-rim-color nil) 44)
                                                                     :transition_NOT "all 0.2s ease-in-out"
                                                                     :border-radius "16px"}
                                                             :children [[re-com/v-box
                                                                         :children
                                                                         [[re-com/h-box
                                                                         ;:style {:padding-bottom "10px"}
                                                                           :style {:transition_NOT "all 0.2s ease-in-out"}
                                                                           :align :center :justify :between
                                                                           :children [[re-com/box
                                                                                       :child (str short-axes)
                                                                                       :style {:color (theme-pull :theme/editor-outer-rim-color nil)
                                                                                               :font-family (theme-pull :theme/base-font nil)
                                                                                               :font-size "19px"
                                                                                               :transition_NOT "all 0.2s ease-in-out"
                                                                                               :font-weight 700
                                                                                               :margin-left "2px"}]

                                                                                      [re-com/h-box
                                                                                       :gap "8px"
                                                                                       :children [(when open?
                                                                                                    (let [q1 (keyword (str "show-me-query-" (rand-int 12345)))
                                                                                                          q2a (str "show-me-attribs-" (rand-int 12345))
                                                                                                          q2 (keyword q2a)
                                                                                                          q2-ref (keyword (str "query/" q2a))
                                                                                                          q3 (keyword (str "pivoted-attribs-" (rand-int 12345)))]
                                                                                                      (bricks/draggable
                                                                                                       {:h 6
                                                                                                        :w 22
                                                                                                        :connection-id "system"
                                                                                                        :name (str "show me why - fields " short-axes " for " src-table-id-str " / " shape-name)
                                                                                                        :queries {q1
                                                                                                                  {:select [:axes_key
                                                                                                                            :connection_id
                                                                                                                          ;:context_hash
                                                                                                                          ;:db_catalog :db_schema
                                                                                                                            :db_type
                                                                                                                            [[:case
                                                                                                                              [:= :derived_name nil] :field_name
                                                                                                                              :else :derived_name] :field]
                                                                                                                            :field_name :derived_calc :derived_name
                                                                                                                          ;:key_hash :logic_map
                                                                                                                          ;:run_id
                                                                                                                            :shape_name :table_name
                                                                                                                          ;:table_type :updated
                                                                                                                            ]
                                                                                                                   :from [[:found_fields]]
                                                                                                                   :where [:and
                                                                                                                           [:= :table_name src-table-id-str]
                                                                                                                           [:= :shape_name shape-name]
                                                                                                                           [:= :axes_key short-axes]]
                                                                                                                   :col-widths
                                                                                                                   {:axes_key 90 :table_name 180 :connection_id 90}}
                                                                                                                  q2
                                                                                                                  {:select [:attribute_name :attribute_value
                                                                                                                            :connection_id ;:context_hash
                                                                                                                         ; :db_catalog :db_schema
                                                                                                                            :db_type
                                                                                                                            [[:case
                                                                                                                              [:= :derived_name nil] :field_name
                                                                                                                              :else :derived_name] :field]
                                                                                                                            :field_name
                                                                                                                            :derived_calc :derived_name
                                                                                                                           ;:key_hash :run_id
                                                                                                                            :table_name ;:table_type :updated
                                                                                                                            ]
                                                                                                                   :from [[:attributes :nn359]]
                                                                                                                   :where [:and
                                                                                                                           [:= :table_name src-table-id-str]
                                                                                                                         ;[:= :shape_name shape-name]
                                                                                                                         ;[:= :axes_key short-axes]
                                                                                                                           ]
                                                                                                                   :col-widths {:attribute_name 227}}
                                                                                                                  q3
                                                                                                                  {:transform-select [[[:sum :attribute_value] :val]
                                                                                                                                      :field :derived_name :derived_calc :table_name]
                                                                                                                   :from [q2-ref]
                                                                                                                   :pivot-by [:attribute_name]
                                                                                                                   :col-widths {:field 240 :table_name 200}}}
                                                                                                      ;:root [12 11]
                                                                                                        :tab @(re-frame/subscribe [::bricks/selected-tab])
                                                                                                        :drag-meta {:block-name (str "show-me-" (rand-int 1234)) :type :viz-reco}}
                                                                                                       "meta-menu"
                                                                                                       [re-com/box :child "show me why" :padding "5px"
                                                                                                        :style {:background-color (str (theme-pull :theme/editor-outer-rim-color nil) 33)
                                                                                                                :font-size "10px"
                                                                                                                :cursor "grab"
                                                                                                                :font-weight 700 ;:margin-top "4px"
                                                                                                                :color (theme-pull :theme/editor-outer-rim-color nil)
                                                                                                                :border-radius "10px"}])))
                                                                                                  [re-com/md-icon-button :src (at)
                                                                                                   :md-icon-name (if open? "zmdi-chevron-down" "zmdi-chevron-up")
                                                                                                   :attr {:on-click #(swap! db/mad-libs-box assoc-in
                                                                                                                            [selected-block short-axes] (not open?))}
                                                                                                   :style {:color (theme-pull :theme/editor-outer-rim-color nil)
                                                                                                           :font-size "25px"}]]]]]

                                                                          (when open?
                                                                            [re-com/box
                                                                         ;:width "290px" ;; :width (px single-width)
                                                                             :style {:font-size "9px"
                                                                                     :padding-bottom "10px"
                                                                                     :transition_NOT "all 0.2s ease-in-out"
                                                                                     :padding-top "10px"
                                                                                     :font-family (theme-pull :theme/base-font nil)
                                                                                     :color "green"}
                                                                             ;:child (str k " = " (get axes-logic-map (keyword short-axes)))
                                                                             :child [bricks/read-only-clojure-box
                                                                                     single-width 0 (str ";; " k " in :" shape-name " rules defined as... \n"
                                                                                                         (get axes-logic-map (keyword short-axes)))]])

                                                                          [re-com/v-box
                                                                           :children
                                                                           (for [[oo ov] o
                                                                                 :let [fname (get (last ov) :field_name)
                                                                                       drname (get (last ov) :derived_name)
                                                                                       all-axes-vec (vec (for [[oo ov] o] (last ov)))
                                                                                       hh @db/mad-libs-waiting-room ;; hack
                                                                         ;all-score0 (vec (map :score0 all-axes-vec))
                                                                         ;all-score1 (vec (map :score1 all-axes-vec))
                                                                                       all-score2 (vec (map :score2 all-axes-vec))
                                                                         ;score0 (get (last ov) :score0)
                                                                         ;score1 (get (last ov) :score1)
                                                                                       score2 (get (last ov) :score2)
                                                                                       axes (get (last ov) :axes)
                                                                                       cur-axes (get-in opts [:current-combo axes])
                                                                                       cur-axes-keyhashes (into {} (for [[k v] (get opts :current-combo)]
                                                                                                                     {k (get v :key_hash)}))
                                                                                       cur-axes-rev (for [r (vals (dissoc (get-in opts [:current-combo]) axes))] (select-keys r [:field_name :derived_name]))
                                                                                       cur-rev? (true? (some #(= {:field_name (get (last ov) :field_name)
                                                                                                                  :derived_name (get (last ov) :derived_name)} %) cur-axes-rev))
                                                                         ;cur? (= cur-axes (last ov))
                                                                         ;cur? (= (select-keys cur-axes [:axes :combo_hash :key_hash :derived_name])
                                                                         ;        (select-keys (last ov) [:axes :combo_hash :key_hash :derived_name]))

                                                                                       cur? (and (= (get cur-axes :field_name)
                                                                                                    (get (last ov) :field_name))
                                                                                                 (= (get cur-axes :derived_name)
                                                                                                    (get (last ov) :derived_name)))
                                                                                       new-axe {axes (get (last ov) :key_hash)}
                                                                                       new-curr-axe (into {} (sort-by key
                                                                                                                      (merge cur-axes-keyhashes new-axe)))
                                                                                       heat-color (value-to-color all-score2 colors1 score2)
                                                                                       heat-color (if (nil? heat-color) zero-color heat-color)
                                                                                       loading? (= new-curr-axe @db/mad-libs-waiting-room)]]
                                                                             [re-com/h-box
                                                                              :style {;:margin-left "3px"
                                                                                      :transition_NOT "all 0.2s ease-in-out"
                                                                                      :text-decoration (if cur-rev? "line-through" "inherit")
                                                                                      :cursor (if (or cur? cur-rev?) "inherit" "pointer")
                                                                                      :color (if cur? "#ffffff" "inherit")}
                                                                              :gap "6px"
                                                                              :children [[re-com/box :child " "
                                                                                          :height "12px" :width "12px"
                                                                                          :style {:background-color heat-color
                                                                                                  :opacity (if (= score2 0) 0.6 "inherit")
                                                                                                  :border-radius "14px"
                                                                                                ;:border "1px solid #ffffff"
                                                                                                  :margin-left "2px"
                                                                                                  :margin-top "5px"}]
                                                                                       ;;[re-com/throbber :size :smaller]
                                                                                         [re-com/h-box
                                                                                          :gap "8px" :size "auto"
                                                                                          :align :center :justify :between
                                                                                          :style {:font-size "13px"
                                                                                                  :font-weight 700

                                                                                                  :padding-left "5px"
                                                                                                  :font-style (if (or (not (nil? drname))
                                                                                                                      (= fname "rows"))
                                                                                                                "italic" "inherit")
                                                                                                  :font-family (theme-pull :theme/base-font nil)}
                                                                                          :children [[re-com/box
                                                                                                      :style {:background-color (if loading?
                                                                                                                                  "#ffffff21" "inherit")
                                                                                                              :border-radius "14px"}
                                                                                                      :child (str (if (nil? drname) fname drname))]

                                                                                                     [re-com/h-box
                                                                                                      :gap "5px"

                                                                                                      :children [(when open?
                                                                                                                   [re-com/box
                                                                                                                    :style {:font-weight 200 :opacity 0.7}
                                                                                                                    :child (str "score: " (rounder score2))])

                                                                                                                 (when loading? ;(= new-curr-axe @db/mad-libs-waiting-room)
                                                                                                     ;[re-com/box :child " (hold on) "]
                                                                                                                   [:img {:src "images/loading-cropped.gif"
                                                                                                                          :width "19px" :height "19px"}])]]]]]
                                                                              :attr {:on-click #(when (not (or cur? cur-rev?))
                                                                                                  (let []
                                                                                                    (re-frame/dispatch [::bricks/get-combo-hash
                                                                                                                        selected-block src-table-id-str shape-name new-curr-axe])
                                                                                                ;;   (tap> [:lookup-combo-hash
                                                                                                ;;          @(re-frame/subscribe
                                                                                                ;;            [::bricks/lookup-combo-hash
                                                                                                ;;             selected-block
                                                                                                ;;             src-table-id-str
                                                                                                ;;             shape-name
                                                                                                ;;             new-curr-axe])])
                                                                                                ;;   (tap> [fname drname cur? cur-rev?
                                                                                                ;;          (value-to-color all-score2 colors1 score2)
                                                                                                ;;          score2 all-score2 colors1
                                                                                                ;;          cur-axes-rev (last ov) ;(first ov)
                                                                                                ;;          cur-axes-keyhashes new-axe
                                                                                                ;;          new-curr-axe])
                                                                                                    ))}
                                                                ;; on click.. find combo hash with that combo and swap block data
                                                                ;; have a swap combo update reg-fn
                                                                ;; no "out of band" recos for now... (since itll require query stuff, etc?)
                                                                ;; logic is in - bricks/insert-rec-preview-block & insert-hidden-reco-preview
                                                                ;; just look up the combo ID that corresponds with the chosen fields:
                                                                ;; 1 - grab it from COMBOS, run that logic on it
                                                                ;; 2 - replace :viz, and :queries only with undoable
                                                                ;;   - worry about merging map changes in later
                                                                ;:child (str fname " " (hash (last ov)) " || " (hash cur-axes))
                                                                              ])]]]]])]]])

                                           :else (let [selected-kp @(re-frame/subscribe [::bricks/editor-panel-selected-view])
                                                       selected-kp (if (nil? (first selected-kp)) nil selected-kp)] ;; if viz-gen or :* then nil
                                                   ;(tap> [:selected-kp selected-kp])
                                                   (if ;(and ;view-selected?
                                                    (get-in @db/scrubbers [selected-block data-key] false)
                                           ; )
                                                     [re-com/box
                                                      :size "none"
                                                      :width (px single-width)
                                                      :height (px (- single-height 40))
                                                      :child [bricks/scrubber-panel view-selected?
                                                              (if view-selected?
                                                                @(re-frame/subscribe [::bricks/keypaths-in-view selected-block data-key])
                                                                @(re-frame/subscribe [::bricks/keypaths-in-query selected-block data-key])) data-key nil {:fm true}]
                                                      :style {:overflow "auto"}]
                                                     [bricks/panel-code-box
                                                      selected-block
                                                      selected-kp
                                                      (+ 17 single-width)
                                                      (- single-height 20)
                                                      (if (nil? selected-kp)
                                                        selected-panel-map
                                                        (get-in selected-panel-map selected-kp))])))

                                     (when (and system-panel?
                                                ;(not (= @db/editor-mode :params))
                                                (= @db/editor-mode :status))
                                       [re-com/box
                                        ;:align :end :justify :end
                                        ;:attr {:on-click #(re-frame/dispatch [::bricks/resub!])}
                                        :child
                                        (str websocket-status)
                                        ;[bricks/click-param-browser (str websocket-status) (* single-width 0.6) 33]
                                       ; [bricks/read-only-clojure-box (* single-width 0.7) single-height (str websocket-status)]
                                       ;; (re-frame/dispatch [::wfx/subscribe socket-id :server-push subscription]) ;; resub on dev page reload
                                        :style {:color (str (theme-pull :theme/editor-font-color nil) 77) ;; "#ffffff44"
                                                :font-size "12px"
                                                :margin-top "-9px"
                                                :padding-left "8px"}])
                                   ; [re-com/box
                                   ;  :style {:color "white" :margin-top "-20px"}
                                   ;  :child (str @(re-frame/subscribe [::bricks/descendant-panels2 selected-block]))]
                                     ]]
                  :height single-height-px
                  :width (if (and (= @db/editor-mode :vvv)
                                  system-panel?)
                           (px (* 2 single-width)) single-width-px)
                  :style {:overflow "hidden"}]

                 (cond

                   (or views? queries?) ; data-key
                   (let []
                     [re-com/box
                      :size "none"
                      :child [re-com/v-box
                              :size "1"
                              :children [[re-com/box :padding "4px"
                                          :child (str "queries & views")
                                          :style {:font-weight 500
                                                  :color (theme-pull :theme/editor-font-color nil)
                                                  ;:background (str "linear-gradient(" (theme-pull :theme/editor-rim-color nil) ", transparent)")
                                                  :background (str "linear-gradient(" (theme-pull :theme/editor-rim-color nil) ", transparent)")
                                                  ;:background-color (theme-pull :theme/editor-rim-color "#a3a3a3")
                                                  }]
                                          ;(tap> [:dmenu @db/data-browser-query data-key])
                                             ;(when queries? ;data-key

                                         [re-com/h-box
                                          :justify :between
                                          :children [[re-com/h-box
                                          ;:justify :between
                                                      :children
                                                      [(when mad-libs-combos
                                                ;;  [re-com/box
                                                ;;   :width "40px" :height "20px"
                                                ;;   :child (str (count mad-libs-combos))]

                                                         [re-com/h-box
                                                          :gap "7px"
                                                          :padding "5px" :style {:font-weight 700 :color "#dddddd"}
                                                          :children (for [k [(str (count mad-libs-combos))]]
                                                                      (let [k :viz-gen] ;; hack TODO
                                                                        [re-com/h-box
                                                                         :align :center :justify :center
                                                                         :style {:margin-left "-5px"
                                                                                 :border-top "1px solid #80008045"
                                                                                 :border-left "1px solid #80008080"
                                                                                 :color (if (= k data-key)
                                                                                          (theme-pull :theme/editor-background-color nil)
                                                                                          (str (theme-pull :theme/editor-font-color nil) 77))
                                                                                 :background-color (if (= k data-key) "#9973e0" "inherit")
                                                                                 :padding-right "5px"
                                                                                 :padding-left "5px"}
                                                                         :gap "4px"
                                                                         :children [[re-com/box
                                                                                     :padding "3px"
                                                                                     :attr {:on-click #(do (swap! db/data-browser-query assoc selected-block k))}
                                                                                     :style {:cursor "pointer" :font-size "13px"
                                                                                             :padding-left "5px" :padding-right "5px"}
                                                                                     :child (str k)]]]))])

                                                       [re-com/h-box
                                                        :gap "7px"
                                                        :padding "5px" :style {:font-weight 700 :color "#dddddd"}
                                                        :children [[re-com/h-box
                                                                    :style {:margin-left "-5px"
                                                                            :border-top "1px solid #80008045"
                                                                            :border-left "1px solid #80008080"
                                                                            :color (if (= :* data-key)
                                                                                     (theme-pull :theme/editor-background-color nil) ;;"#000000"
                                                                                     (theme-pull :theme/editor-font-color nil))
                                                                            :background-color (if (= :* data-key) "#9973e0" "inherit")
                                                                            :padding-right "5px"
                                                                            :padding-left "5px"}
                                                                    :gap "4px"
                                                                    :children [[re-com/box
                                                                                :padding "3px"
                                                                                :attr {:on-click #(do (swap! db/data-browser-query assoc selected-block :*))}
                                                                                :style {:cursor "pointer" :font-size "13px"
                                                                                        :padding-left "5px" :padding-right "5px"}
                                                                                :child (str :*)]]]]]

                                                       (when queries?
                                                         [re-com/h-box
                                                          :gap "7px"
                                                          :padding "5px" :style {:font-weight 700 :color "#dddddd"}
                                                          :children (for [k (keys sql-calls)]
                                                                      (let [console-output @(re-frame/subscribe [::bricks/repl-output k])
                                                                            repl? (not (empty? console-output))
                                                                            dyn-color (if (= k data-key)
                                                                                        (theme-pull :theme/editor-background-color nil)
                                                                                        (str (theme-pull :theme/editor-font-color nil) 77))]
                                                                        [re-com/h-box
                                                                         :style {:margin-left "-5px"
                                                                                 :border-top "1px solid #80008045"
                                                                                 :border-left "1px solid #80008080"
                                                                                 :color dyn-color
                                                                                 :background-color (if (= k data-key) "#9973e0" "inherit")
                                                                                 :padding-right "5px"
                                                                                 :padding-left "5px"}
                                                                         :gap "4px"
                                                                         :children [[re-com/box
                                                                                     :padding "3px"
                                                                                     :height "24px"
                                                                                     :attr {:on-click #(do (swap! db/data-browser-query assoc selected-block k)
                                                                                                           (when (and repl? (= k data-key))
                                                                                                             (swap! db/data-browser-query-con assoc k
                                                                                                                    (let [b (get @db/data-browser-query-con k)]
                                                                                                                      (if (nil? b) true (not b))))))}
                                                                                     :style {:cursor "pointer" :font-size "13px"
                                                                                             :border-bottom (when repl?
                                                                                                              (if (not (get @db/data-browser-query-con k))
                                                                                                                (str "4px solid " dyn-color)
                                                                                                                "inherit"))
                                                                                             :padding-left "5px" :padding-right "5px"}
                                                                                     :child (str k)]
                                                                                    (when repl?
                                                                          ;[re-com/box :child "*"]
                                                                                      [re-com/md-icon-button :src (at)
                                                                                       :md-icon-name "zmdi-code"
                                                                                       :on-click #(do (swap! db/data-browser-query assoc selected-block k)
                                                                                                      (swap! db/data-browser-query-con assoc k
                                                                                                             (let [b (get @db/data-browser-query-con k)]
                                                                                                               (if (nil? b) true (not b)))))
                                                                                       :style {:color dyn-color
                                                                                               :border-bottom (if (get @db/data-browser-query-con k)
                                                                                                                (str "4px solid " dyn-color)
                                                                                                                "inherit")
                                                                                               :font-size "15px"
                                                                                   ;:opacity 0.22
                                                                                               }])]]))])

                                                       (when views?
                                                         [re-com/h-box
                                                          :gap "7px"
                                                          :padding "5px" :style {:font-weight 700 :color "#dddddd"}
                                                          :children (for [k (keys views)]
                                                                      (let []
                                                                        [re-com/h-box
                                                                         :style {:margin-left "-5px"
                                                                                 :border-top "1px solid #80008045"
                                                                                 :border-left "1px solid #80008080"
                                                                                 :color (if (= k data-key)
                                                                                          (theme-pull :theme/editor-background-color nil) ;;"#000000"
                                                                                          (theme-pull :theme/editor-font-color nil))
                                                                                 :background-color (if (= k data-key) "#9973e0" "inherit")
                                                                                 :padding-right "5px"
                                                                                 :padding-left "5px"}
                                                                         :gap "4px"
                                                                         :children [[re-com/box
                                                                                     :padding "3px"
                                                                                     :attr {:on-click #(do (swap! db/data-browser-query assoc selected-block k))}
                                                                                     :style {:cursor "pointer" :font-size "13px"
                                                                                             :padding-left "5px" :padding-right "5px"}
                                                                                     :child (str k)]]]))])]]

                                                     [re-com/h-box
                                                      :gap "7px"
                                                      :padding "5px" :style {:font-weight 700
                                                                             :color "#dddddd66"
                                                                             :margin-top "3px"}
                                                      :children [[re-com/box
                                                                  :attr {:on-click #(re-frame/dispatch
                                                                                     [::bricks/add-new selected-block :queries
                                                                                      {:select [["heya, friendo" :greetings]
                                                                                                [(rand-int 123) :count_stuff]]}])}
                                                                  :style {:cursor "pointer"}
                                                                  :child "+query"]
                                                                 [re-com/box
                                                                  :attr {:on-click #(re-frame/dispatch
                                                                                     [::bricks/add-new selected-block :queries
                                                                                      {:select [:*]
                                                                                       :from [{:data '(vec (for [i (range 45)]
                                                                                                             {:row_id i
                                                                                                              :type (rand-nth ["cat" "dog" "pizza"])
                                                                                                              :name (str "row " i)}))}]}])}
                                                                  :style {:cursor "pointer"}
                                                                  :child "+code"]
                                                                 [re-com/box
                                                                  :attr {:on-click #(re-frame/dispatch
                                                                                     [::bricks/add-new selected-block :views
                                                                                      [:box :child "heya"
                                                                                       :align :center
                                                                                       :justify :center
                                                                                       :size "auto"
                                                                                       :style {:font-size "22px"}]])}
                                                                  :style {:cursor "pointer"}
                                                                  :child "+view"]]]]]

                                         (if @db/bad-form?
                                           [re-com/v-box
                                            :padding "12px" :style {:color (theme-pull :theme/editor-font-color nil)
                                                                    :font-family (theme-pull :theme/monospaced-font nil)
                                                                    :font-weight 700
                                                                    :background-color "#00000000"}
                                                  ;:size "auto"
                                            :gap "10px"
                                            :children [[re-com/h-box
                                                              ;:style {:border "1px solid yellow"}
                                                        :size "auto"
                                                        :children [[re-com/md-icon-button :src (at)
                                                                    :md-icon-name "zmdi-chevron-left"
                                                                    :style {:color (theme-pull :theme/editor-outer-rim-color nil)
                                                                            :font-size "31px"
                                                                            :margin-top "-5px"
                                                                                  ;:width "10px"
                                                                                  ;:height "20px"
                                                                            }]
                                                                   [re-com/box :size "auto"
                                                                    :style {:color (theme-pull :theme/editor-outer-rim-color nil)}
                                                                    :child (str "Issue with block code forms")]
                                                                   [re-com/md-icon-button :src (at)
                                                                    :md-icon-name "zmdi-help"
                                                                    :tooltip "help?"
                                                                    :style {:color (theme-pull :theme/editor-outer-rim-color nil)
                                                                            :font-size "15px"
                                                                                  ;:margin-top "-5px"
                                                                            :opacity 0.22
                                                                                                                                                           ;:width "10px"
                                                                                                                                                           ;:height "20px"
                                                                            }]]]
                                                       [re-com/box
                                                        :padding "8px"
                                                        :size "auto" :child (str @db/bad-form-msg)]

                                                       [re-com/box
                                                        :padding "8px"
                                                        :style {:opacity 0.3}
                                                        :size "auto" :child "(Block cannot be saved until corrected)"]]]
                                           (let [query-box? (or (some #(= % (get @db/data-browser-query selected-block)) (keys sql-calls))
                                                                (some #(= % data-key) (keys sql-calls)))
                                                 is-layout? @(re-frame/subscribe [::bricks/is-layout? selected-block data-key])
                                                 viz-gen? (= :viz-gen (get @db/data-browser-query selected-block))]
                                               ;; (tap> [:as selected-block data-key] )
                                             [re-com/v-box
                                              :children [(cond query-box?

                                                               [re-com/v-box
                                                                :justify :between
                                                                :children (if (get @db/data-browser-query-con data-key)
                                                                            [(let [repl-output (dissoc @(re-frame/subscribe [::bricks/repl-output data-key]) :status)
                                                                                   console (vec (remove empty? (get repl-output :out [])))]
                                                                               [re-com/v-box
                                                                                :padding "9px"
                                                                                :size "none" :height (px (- single-height 107))
                                                                                :style {;:border "1px solid pink"
                                                                                        :overflow "auto"}
                                                                                :children [(when (not (empty? console))
                                                                                             [re-com/box
                                                                                            ;:padding "9px"
                                                                                              :child "console output (limited to 50 items)"])
                                                                                           (when (not (empty? console))
                                                                                             [re-com/v-box
                                                                                           ; :padding "9px"
                                                                                              :children (for [r console]
                                                                                                          [re-com/box :child (str r)])])
                                                                                           [shape/map-boxes2 (dissoc repl-output :out) "no-block" "no-flow" [] "no-block" "map"]]
                                                                                ;["fart fax incoming!" (str repl-output)]
                                                                                ])]
                                                                            [@(re-frame/subscribe [::bricks/sql-data-table-editor selected-block
                                                                                                   [data-key]
                                                                                                   single-width-bricks 6.05])
                                                                             (when (not @bricks/dragging?)
                                                                               [bricks/read-only-sql-box single-width (/ single-height 3.2) (str sql-string)])])]

                                                               viz-gen? ;[re-com/box :child (str (count mad-libs-combos) " rows. ")]
                                                               (let [;default-combo-view @(re-frame/subscribe [::bricks/lookup-mad-libs-row combohash])
                                                                     src-table-id-str (last (get selected-panel-map :mad-libs-combo-hash))
                                                                     combo-hash (first (get selected-panel-map :mad-libs-combo-hash))
                                                                     opts @(re-frame/subscribe [::bricks/get-mad-libs-options selected-block src-table-id-str combo-hash])
                                                                     shape-name (get opts :shape_name)
                                                                     viz-shape @(re-frame/subscribe [::bricks/get-viz-shape shape-name])
                                                                     default-shape-view (get viz-shape :selected_view)
                                                                     default-shape-view (try (when (not (empty? default-shape-view)) (keyword (ut/replacer default-shape-view #":" ""))) (catch :default _ nil))]
                                                                 (tap> [:def-sel default-shape-view viz-shape])
                                                                 [re-com/box
                                                                  :size "none"
                                                                     ;:style {:position "absolute" :right 0 :top 10}
                                                                  :style {:transform "translate(0)"}  ;; a known CSS hack for fixed position in non fixed parents!!! TODO futureproof it?
                                                                  :height (px (- single-height 100))
                                                                  :child [bricks/honeycomb selected-block (or default-shape-view :oz)]])

                                                                 ;; is-layout? [re-com/box :child "layout, no rendero!"]

                                                               :else [re-com/box
                                                                      :size "none"
                                                                      :style {:transform "translate(0)"}  ;; a known CSS hack for fixed position in non fixed parents!!! TODO futureproof it?
                                                                      :height (px (- single-height 100)) ;; (get @db/data-browser-query-con k)
                                                                      :child [bricks/honeycomb selected-block data-key]])

                                                         (if viz-gen? ;; dont show scrubber boolean if on viz-gen mode

                                                           [re-com/box
                                                            :style {:font-size "10px" :margin-left "8px"}
                                                            :child (str "('viz-gen' debug - " (count mad-libs-combos) " c-rows, "
                                                                        (count (distinct (map :axes mad-libs-combos))) " axes, "
                                                                        (count (distinct (map :combo_hash mad-libs-combos))) " combos, "
                                                                        " combo-hash: " (get (first mad-libs-combos) :combo_hash) ")")]

                                                           (let [view-scrubbers? (get-in @db/scrubbers [selected-block data-key] false)]

                                                             [re-com/box
                                                              ;:padding "3px"
                                                              :size "none"
                                                              :width "90px"
                                                              :child (if view-scrubbers? "scrubber on" "scrubber off")
                                                              :attr {:on-click #(swap! db/scrubbers assoc-in
                                                                                       [selected-block data-key]
                                                                                       (not view-scrubbers?))}
                                                              :style {:color (if view-scrubbers? "yellow" "grey")
                                                                      :z-index 100
                                                                      :user-select "none"
                                                                      :margin-top (if query-box? "9px" "inherit")
                                                                    ;:z-index 300
                                                                      :cursor "pointer"
                                                                      :font-size "11px"
                                                                      :font-weight 700}]))]]))]]
                      :height (px (- ttl-height 24))
                      :width single-width-px
                      :style {:overflow "hidden"}])

                   system-panel?
                   [re-com/box
                    :size "none"
                    :child [re-com/v-box
                            :size "1"
                            :children [[re-com/box :padding "4px"
                                        :child (str "(fields, data-types)")
                                        :style {:font-weight 500 :color (theme-pull :theme/editor-font-color nil)
                                                :background (str "linear-gradient(" (theme-pull :theme/editor-rim-color nil) ", transparent)")
                                                ;:background-color (theme-pull :theme/editor-rim-color "#a3a3a3")
                                                                       ;:cursor "grab" :user-select "none"
                                                }
                                            ;:width single-width-px ;  (if (= @db/editor-mode :vvv) "0px" single-width-px)
                                       ;:attr {:on-mouse-down mouse-down-handler}
                                        ]
                                       [re-com/box
                                        :style {:padding-top "10px"}
                                        :child (condp = @db/editor-mode ; @file-mode?
                                                 :files [editor-panel-metadata-ext-files]
                                                 :kits [editor-panel-metadata-ext-kits]
                                                     ;:params [editor-panel-metadata-ext-params]
                                                 :params [editor-panel-metadata-params single-width single-height :theme]
                                                 :meta [editor-panel-metadata-ext]
                                                 :viz [editor-panel-metadata-viz] ;[re-com/box :child "poop"]
                                                 :search [search-panel-metadata-ext] ;;[search-panel-right single-width single-height]
                                                 :vvv [re-com/box
                                                       :size "none"
                                                       :child " " :width "0px"]
                                                          ;[editor-panel-metadata-viz2]
                                                 :status [editor-panel-metadata-status] ;[editor-panel-status]
                                                 )
                                        :height (px (- ttl-height 40))
                                            ;:width (if (= @db/editor-mode :vvv) "0px" single-width-px)
                                        ]]]
                    :height single-height-px
                    :width (if (= @db/editor-mode :vvv) "0px" single-width-px)
                        ;:width single-width-px
                    :style {;:border "1px solid white"
                         ;:background-color "#1f2430"
                            :overflow "hidden"}])

                 [re-com/v-box
                  ;:size "none"
                  ;:height "490px"
                  :children
                  [(if (and (or (= @db/editor-mode :viz) 
                                (= @db/editor-mode :vvv))
                            reco-selected? 
                            (or (= selected-block "none!") 
                                (nil? selected-block)))

                     [re-com/v-box
                      :size "auto"
                      :height (px (- single-height 80))
                      :children [[re-com/box
                               ;:padding "4px"
                                  :style {:font-weight 500
                                          :color (theme-pull :theme/editor-font-color nil)
                                          :background-color (theme-pull :theme/editor-rim-color "#a3a3a3")
                                          ;:background (str "linear-gradient(" (theme-pull :theme/editor-rim-color nil) ", transparent)")
                                         ;:padding-left "22px"
                                          :padding-top "4px" :padding-bottom "4px" ;:margin-left "2px"
                                          }
                               ;:attr {:on-mouse-down mouse-down-handler}
                                  :child "reco preview"]
                                 [re-com/v-box
                                  :size "auto"
                                  ;:width (px (* single-width 0.6))
                                  :width (px (* single-width 0.95))
                                  :gap "6px"
                                  ;:padding "10px"
                                  :style {:margin-top "30px"
                                          :transform "translate(0)"  ;; a known CSS hack for fixed position in non fixed parents!!! TODO futureproof it?
                                          ;:position "fixed" :right 0 :top 0
                                          :overflow "auto"}
                                  :children
                                  [;[re-com/box :child (str reco-selected)]
                                   ;[re-com/box :child (str reco-query)]
                                   ;[re-com/box :child (str reco-viz)]
                                   [bricks/honeycomb :reco-preview :oz]
                                   ;[bricks/honeycomb :reco-preview]
                                   ]]]]

                     [re-com/h-box
                      :size "auto"
                      :height (px (- single-height 20))
                   ;:child (str click-params)
                      :children [[re-com/v-box
                                  :size "none"
                                  :width (px (* single-width 0.6))
                                  :children
                                  [[re-com/box
                               ;:padding "4px"
                                    :style {:font-weight 500
                                            :color (theme-pull :theme/editor-font-color nil)
                                            :background (str "linear-gradient(" (theme-pull :theme/editor-rim-color nil) ", transparent)")
                                            ;:background-color (theme-pull :theme/editor-rim-color "#a3a3a3")
                                         ;:padding-left "22px"
                                            :padding-top "4px" :padding-bottom "4px" ;:margin-left "2px"
                                            }
                               ;:attr {:on-mouse-down mouse-down-handler}
                                    :child [re-com/h-box
                                            :width (px (* single-width 0.538))
                                            :justify :between
                                            :children [[re-com/box :child "parameters"]
                                                       [re-com/h-box
                                                        :gap "3px"
                                                        ;:width "200px"
                                                        :style {:font-size "11px" :margin-top "3px"}
                                                        :children (vec (for [f (keys @db/param-filter)]
                                                                         [re-com/box :child (ut/unkeyword f)
                                                                          :attr {:on-click #(swap! db/param-filter assoc f (not (get @db/param-filter f false)))}
                                                                          :style {:text-decoration (if (get @db/param-filter f true) "none" "line-through")
                                                                                  :opacity (if (get @db/param-filter f true) 1 0.4)
                                                                                  :cursor "pointer"
                                                                                  :user-select "none"
                                                                                  :padding-left "4px" :padding-right "4px"}]))]]]]
                                ;[bricks/read-only-clojure-box (* single-width 0.6) single-height (str click-params)]
                                   [bricks/click-param-browser click-params (* single-width 0.6) (- single-height 1.3)]]]

                                 [re-com/v-box
                                  :size "none"
                                  :width (px (* single-width 0.379))
                                  :children
                                  [[re-com/h-box
                               ;:padding "4px"

                                    :style {:font-weight 500
                                            :color (theme-pull :theme/editor-font-color nil)
                                            :background (str "linear-gradient(" (theme-pull :theme/editor-rim-color nil) ", transparent)")
                                            ;:background-color (theme-pull :theme/editor-rim-color "#a3a3a3")
                                         ;:padding-left "22px"
                                            :border-radius "0px 11px 0px 0px"
                                            :padding-top "4px" :padding-bottom "4px"
                                            :margin-left "-21px"}
                               ;:attr {:on-mouse-down mouse-down-handler}
                                    :children [[re-com/h-box
                                                :gap "12px"
                                                :width (px (- (* single-width 0.405) 18))
                                                  ;:justify :between
                                           ; :children [[re-com/box :child "queries"]
                                           ;            [re-com/box :child "blocks" :style {:opacity 0.2}]]
                                            ;:style {:background (str "linear-gradient(" (theme-pull :theme/editor-rim-color nil) ", transparent)")}
                                                :children (for [b ["queries" "blocks"]
                                                                :let [selected? (= (keyword b) @db/item-browser-mode)]]
                                                            [re-com/box
                                                             :attr {:on-click #(reset! db/item-browser-mode (keyword b))}
                                                             :style (if selected?
                                                                      {:user-select "none"
                                                                   ;:background (str "linear-gradient(" (theme-pull :theme/editor-rim-color nil) ", transparent)")
                                                                       :opacity 1.0}
                                                                      {:cursor "pointer" :user-select "none" :opacity 0.3})
                                                             :child (str b)])]
                                               [re-com/md-icon-button
                                                :md-icon-name "zmdi-window-minimize"
                                                :on-click #(re-frame/dispatch [::bricks/toggle-editor])
                                                :style {:font-size "15px"
                                                        :opacity   0.33
                                                        :cursor    "pointer"}]]]



                                ;[bricks/read-only-clojure-box (* single-width 0.379) single-height (str all-sql-call-keys)]


                                   [re-com/box
                                    :style {:padding-right "3px"}
                                    :child (cond (= @db/item-browser-mode :queries)
                                                 [bricks/screen-query-browser (* single-width 0.379) (- single-height 1.3)]
                                                 (= @db/item-browser-mode :blocks)
                                                 [bricks/screen-block-browser (* single-width 0.379) (- single-height 1.3)]
                                                 :else [re-com/box :child "nothing selected above?"])]



                              ;  [re-com/box
                              ; ;:padding "4px"
                              ;   :style {:font-weight 500 :color (theme-pull :theme/editor-font-color nil)
                              ;           :padding-left "22px" :padding-top "4px" :padding-bottom "4px"}
                              ; ;:attr {:on-mouse-down mouse-down-handler}
                              ;   :child "all view-sql queries"]
                              ;  [bricks/read-only-clojure-box (* single-width 0.379) single-height (str all-vsql-call-keys)]
                                   ]]]

                      :height single-height-px :width single-width-px
                      :style {;:border "1px solid white"
                         ;:background-color "#1f2430"
                              :overflow "hidden"}])

                   [re-com/h-box
                    :height "42px"
                    :width (px (- (* single-width 0.98) 1))
                   ;:padding "10px"
                    :size "none"
                    :align :center
                    :justify :between
                    :gap "9px"
                    :children [(if (not (and (= @db/editor-mode :viz) reco-selected?))
                                 [re-com/h-box ;:padding "8px"
                                  :justify :between :align :center
                                  :size "auto"
                                 ;:height "40px"
                                  :children [;[re-com/box
                                           ; :align :center
                                           ; :child (str ":screen-name ") :style {:opacity 0.2}]
                                           ;[re-com/box :child (str screen-name)]


                                             [re-com/input-text :src (at)
                                              :model            (str screen-name)
                                              :width            "487px"
                                              :height "30px"
                                              :on-change        #(re-frame/dispatch [::bricks/set-screen-name %])
                                              :validation-regex screen-name-regex
                                              :change-on-blur? true
                                              :style  {:font-size "20px"
                                                       ;:font-weight 700
                                                       :font-style "underline"
                                                       :border "0px solid black"
                                                       :padding "8px"
                                                     ;:margin-top "10px"
                                                     ;:padding-top "10px"
                                           ;          :text-align "right"
                                                       :background-color "#00000000"
                                                       :color (theme-pull :theme/editor-font-color nil)
                                           ;          :padding-top "1px"
                                                       }]]
                                  :style {:color (theme-pull :theme/editor-font-color nil)
                                         ; :background-color (str (theme-pull :theme/editor-background-color nil) 44) ;; "#00000044"
                                        ;:cursor "pointer"
                                          :border-radius "7px"}
                                ;:attr {:on-click #(re-frame/dispatch [::http/save :skinny])}
                                  ]
                                 [re-com/box
                                  :align :center :justify :center
                                  :child (str "viz preview: " reco-combo)
                                  :style  {:font-size "16px"
                                           :font-weight 700
                                           :font-style "underline"
                                           :border "0px solid black"
                                           :padding "8px"
                                           :margin-top "-14px"

                                           ;:padding-top "10px"
                                           ;:text-align "right"
                                           :background-color "#00000000"
                                           :color (str (theme-pull :theme/editor-font-color nil) 66) ;; "#ffffff66"
                                           ;:padding-top "1px"
                                           }])]
                    :style {:padding-left "8px"  :padding-top "12px"
                            :padding-bottom "10px" :padding-right "10px"
                            :border-radius "10px 0px 10px 0px"
                            :box-shadow "0px -5px 5px 0px #00000099"
                            :font-weight 500
                            :color (theme-pull :theme/editor-background-color nil) ;; "#000000"
                           ;:filter "drop-shadow(0.15rem -0.7rem 0.2rem rgba(0, 0, 0, 0.3))"
                            ;:background-color (theme-pull :theme/editor-rim-color "#a3a3a3")
                            ;:border-top (str "2px solid " (theme-pull :theme/editor-rim-color nil))
                            :background (str "linear-gradient(" (theme-pull :theme/editor-rim-color nil) ", transparent)")}]



                ;;    (if (not (and (or (= @db/editor-mode :viz)
                ;;                      (= @db/editor-mode :vvv))
                ;;                  reco-selected?))

                ;;      [re-com/h-box
                ;;       :height "60px"
                ;;       :width (px (* single-width 0.98))
                ;;    ;:padding "10px"
                ;;       :size "none" :align :center
                ;;       :justify :between
                ;;       :gap "9px"
                ;;       :children [[re-com/box :padding "8px" :child "save to server (without row data)" :size "auto" :align :center :justify :start
                ;;                   :style {:color (theme-pull :theme/editor-font-color nil)
                ;;                           :background-color (str (theme-pull :theme/editor-background-color nil) 44) ;; "#00000044"
                ;;                           :cursor "pointer"
                ;;                           :border-radius "7px"
                ;;                           :font-size "13px"}
                ;;                   :attr {:on-click #(re-frame/dispatch [::http/save :skinny screen-name])}]

                ;;                  [re-com/box :padding "8px" :child "save to server (with row data)" :size "auto" :align :center :justify :start
                ;;                   :style {:color (theme-pull :theme/editor-font-color nil)
                ;;                           :background-color (str (theme-pull :theme/editor-background-color nil) 44) ;; "#00000044"
                ;;                           :cursor "pointer"
                ;;                           :border-radius "7px"
                ;;                           :font-size "13px"}
                ;;                   :attr {:on-click #(re-frame/dispatch [::http/save :fat screen-name])}]]
                ;;       :style {:padding-left "10px"
                ;;               :padding-top "0px"
                ;;               :padding-bottom "0px"
                ;;               :padding-right "10px"
                ;;               :border-radius "0px 0px 11px 0px"
                ;;               :font-weight 500
                ;;               :color (theme-pull :theme/editor-background-color nil) ;; "#000000"
                ;;               ;:filter "drop-shadow(0.15rem -0.7rem 0.2rem rgba(0, 0, 0, 0.3))"
                ;;               :background-color (theme-pull :theme/editor-rim-color "#a3a3a3")}])
                   ]]]

      :width ttl-width-px
      :height ttl-height-px
      :margin "-2px"
      :style {:position "fixed"
              :top y-px
              :left x-px
              :border-radius "16px"
             ; :transform "scale(1.5)"
             ; :zoom 1.5
             ;:margin-left "-3px"
             ;:margin-top "-3px"
              :z-index 100
              ;:background-color (theme-pull :theme/editor-background-color nil) ; "#000000"
              :border (str "6px solid " (theme-pull :theme/editor-outer-rim-color nil)) ; #b7e27c"
              :filter "drop-shadow(0.35rem 0.35rem 0.4rem rgba(0, 0, 0, 0.8))"
              :background-color (str (theme-pull :theme/editor-background-color nil) 99) ; "#000000"
              :backdrop-filter "blur(5px)"
             ;:font-size "44px"
             ;:font-family "Lato"
              }]]))

(dnd2/subscribe! (js/document.querySelector "div")  :canvas02
                 {:drop  (fn [_ files]
                           (when (not (= "" (-> files .-value)))
                             ;(doseq [file files]
                             (let [^js/File file0 (-> files (aget 0))
                                   fname (-> file0 .-name)]
                              ;(js/alert (-> file0 .-name)) ;; is .csv?
                               (tap> [:run? (cstr/lower-case fname)])
                               (if (cstr/ends-with? (cstr/lower-case fname) ".csv")
                                 (do
                                   (tap> [:saving-csv-to-server fname])
                                   (ut/read-file file0
                                             ;#(tap> (str %))
                                                 #(re-frame/dispatch [::http/save-csv fname (str %)]))
                                   (set! (-> files .-value) ""))
                                 (tap> [:invalid-file fname])))))})

(defn ifnil [x n] (if (nil? x) n x))

(defonce title-edit-idx (reagent/atom nil))

(defn tab-menu []
  (let [;tabs @(re-frame/subscribe [::bricks/tabs])
        tabs @(re-frame/subscribe [::bricks/visible-tabs])
        hidden-tabs @(re-frame/subscribe [::bricks/hidden-tabs])
        selected-tab @(re-frame/subscribe [::bricks/selected-tab])
        react-hack [@db/show-tabs? @title-edit-idx] ;; important
        ;is-last? (fn [t] (try (= (count tabs) (+ 1 (.indexOf tabs t))) (catch :default _ false)))
        tab-row (fn [top tabs & [hidden?]]
                  [re-com/h-box
                   :attr {:on-mouse-over  #(when (not @bricks/over-block?) (reset! bricks/over-block? true))
                          :on-mouse-leave  #(do (reset! bricks/over-block? false))}
                   :children
                   (conj (vec (conj (when (or (not hidden?)
                                              (and hidden? @db/show-hidden-tabs?))
                                      (for [t tabs
                                            :let [selected? (= selected-tab t)
                                                  tab-box [re-com/box
                                                           :child
                                                           [re-com/box
                                                            :child (if (not @db/show-tabs?) (str (try (.indexOf tabs t) (catch :default _ "e")))
                                                                       (if (= @title-edit-idx t)
                                                                         [re-com/h-box
                                                                          :children
                                                                          [[re-com/input-text
                                                                            :model (str t)
                                                                            :on-change #(do (tap> [:changed-tab-name (str t) :to (str %)])
                                                                                            (when (and (not (empty? (cstr/trim (str %))))
                                                                                                       (not (some (fn [x] (= x %)) tabs)))
                                                                                              (re-frame/dispatch [::bricks/rename-tab (str t) (str %)])) ;; dont want to update with empty string or dupe
                                                                                            (reset! title-edit-idx nil))
                                                                            :change-on-blur? true
                                                                            :width (px (+ 15 (* (count (str t)) 11))) ;"inherit" ; "200px"
                                                                            :style {:background-color "#00000000"
                                                                                    :text-decoration "underline"
                                                                                    :border "none"
                                                                                    :padding-left "0px"
                                                                                    :color (theme-pull :theme/editor-outer-rim-color nil)}]

                                                                           [re-com/md-icon-button :src (at)
                                                                            :md-icon-name "zmdi-delete"
                                                                            :tooltip "delete this tab"
                                                                            :style {:color (theme-pull :theme/editor-outer-rim-color nil)
                                                                                    :font-size "14px"
                                                                                    :padding-top "5px"
                                                                                    :width "10px"
                                                                                    :height "20px"}
                                                                            :attr {:on-click #(re-frame/dispatch [::bricks/delete-tab (str t)])}]]]
                                                                         (str t)))
                                                            :size "none"
                                                            :align :center :justify (when (not @db/show-tabs?) :center)
                                                            :padding "5px"
                                                            :width (when (not @db/show-tabs?) "5px")
                                                            :style {:transition_NOT "all 0.2s ease-in-out"
                                                                    :backdrop-filter "blur(8px)"}]
                                                           :attr (merge (when (not (= @title-edit-idx t))
                                                                          {:on-click #(do (re-frame/dispatch [::bricks/select-tab (str t)])
                                                                                          (reset! title-edit-idx nil))})
                                                                        (if selected?
                                                                          {:on-double-click #(reset! title-edit-idx (str t))}
                                                                          {})
                                                                        {:on-context-menu #(re-frame/dispatch [::bricks/toggle-tab-visibility (str t)])})
                                                           :style {:cursor "pointer"
                                                                   :background-color (theme-pull :theme/base-block-color-selected nil)
                                                                   :color (theme-pull :theme/editor-outer-rim-color nil)
                                                                   :opacity (if selected? 1.0 0.4)
                                                                   :backdrop-filter "blur(8px)"
                                                                   :padding-left "5px"
                                                                   :padding-right "5px"
                                                                   :height "30px"
                                                                   :border (when (not hidden?)
                                                                             (if (= @title-edit-idx t)
                                                                               (str "3px dashed " (theme-pull :theme/block-tab-selected-font-color nil))
                                                                               (str "1px solid " (theme-pull :theme/block-tab-selected-font-color nil))))}]]]
                                        (if (not selected?)
                                          (bricks/draggable
                                           (let [[_ _ w h] @(re-frame/subscribe [::bricks/tab-recenter t])]
                                             {:w              w
                                              :selected-view  :vv
                                              :name           (str t)
                                              :h              h
                                              :ghosted?       false
                                              :views          {:vv [:grid t]}})
                                           "meta-menu" tab-box)
                                          tab-box)))

                                    (if hidden?
                                      [re-com/md-icon-button :src (at)
                                       :md-icon-name (if @db/show-hidden-tabs? "zmdi-tab" "zmdi-tab-unselected")
                                       ;:tooltip "minimize tab menu"
                                       :style {:background-color "#00000099" ;(theme-pull :theme/base-block-color-selected nil)
                                               :color (theme-pull :theme/editor-outer-rim-color nil)
                                               :backdrop-filter "blur(2px)"
                                               :border-radius (when (not @db/show-hidden-tabs?) "0px 0px 8px 0px")
                                               :padding-left "4px"
                                               :margin-left "-3px"
                                               :font-size "20px"
                                               :padding-top "3px"
                                               :width "30px"
                                               :height "30px"}
                                       :attr {:on-click #(reset! db/show-hidden-tabs? (not @db/show-hidden-tabs?))}]
                                      [re-com/md-icon-button :src (at)
                                       :md-icon-name (if @db/show-tabs? "zmdi-chevron-left" "zmdi-chevron-right")
                                       ;:tooltip "minimize tab menu"
                                       :style {:background-color (theme-pull :theme/base-block-color-selected nil)
                                               :color (theme-pull :theme/editor-outer-rim-color nil)
                                               :padding-left "4px"
                                               :margin-left "-3px"
                                               :font-size "20px"
                                               :padding-top "3px"
                                               :width "30px"
                                               :height "30px"}
                                       :attr {:on-click #(reset! db/show-tabs? (not @db/show-tabs?))}])))

                         (when (and @db/show-tabs? (not hidden?))
                           [re-com/md-icon-button :src (at)
                            :md-icon-name "zmdi-plus"
                            :tooltip "add new tab"
                            :style {:color (theme-pull :theme/editor-outer-rim-color nil)
                                    :padding-left "4px"
                                 ;:opacity 0.4
                                    :margin-left "-3px"
                                    :font-size "20px"
                                    :padding-top "3px"
                                    :width "30px"
                                    :height "30px"}
                            :attr {:on-click #(let [new-tab (ut/make-tab-name)]
                                                (re-frame/dispatch [::bricks/add-tab new-tab])
                                                (re-frame/dispatch [::bricks/select-tab new-tab]))}]))
                   :height "30px"
                   :style {:position "fixed"
                           :top top :left 0
                           :z-index 999
                           :backdrop-filter "blur(8px)"
                           :user-select "none"
                           :transition_NOT "all 0.2s ease-in-out"
                           :transform-style "preserve-3d"}])]
    [re-com/v-box
     :children [[tab-row 0 tabs]
                (when (ut/not-empty? hidden-tabs)
                  [tab-row 30 hidden-tabs true])]]))

(defn snapshot-menu []
  (let [;tabs @(re-frame/subscribe [::bricks/tabs])
        matching @(re-frame/subscribe [::bricks/matching-snapshots])
        selected-tab nil ; @(re-frame/subscribe [::bricks/selected-tab])
        snapshots @(re-frame.core/subscribe [::bricks/snapshots])
        tabs (vec (map :key (sort-by :key (vec (for [[k v] snapshots
                                                     :when (get v :menu?)]
                                                 (assoc v :key k))))))
       ; curr-params @(re-frame.core/subscribe [::bricks/current-params])
        react-hack [@db/show-snaps? @db/vertical-snaps?] ;; important
        ;is-last? (fn [t] (try (= (count tabs) (+ 1 (.indexOf tabs t))) (catch :default _ false)))
        ]
    ;;(tap> [:matching matching tabs])
    (when (not (empty? tabs))
      [(if @db/vertical-snaps? re-com/v-box re-com/h-box)
       :children (reverse (conj (vec (conj (for [t (reverse tabs)] [re-com/box
                                                                    :child [re-com/box
                                                                            :child (if (not @db/show-snaps?)
                                                                                     (str (try (.indexOf tabs t)
                                                                                               (catch :default _ "err!")))
                                                                                     (str t))
                                                                            :size "none"
                                                                            :align :center :justify (when (not @db/show-snaps?) :center)
                                                                            :padding "5px"
                                                                            :width (when (not @db/show-snaps?) "5px")
                                                                            :attr {:on-click #(re-frame/dispatch [::bricks/swap-snapshot t])}
                                                                            :style {:transition_NOT "all 0.2s ease-in-out"
                                                                                    :backdrop-filter "blur(8px)"
                                                                         ;:transform-style "preserve-3d"
                                                                                    }
                                                 ;:height "40px"
                                                 ;:width "80px"
                                                 ;:width (px (* (count (str t)) 14))
                                                                            ]

                                                                    :style {;:transform "rotate(90deg)"
                                                                            :cursor "pointer"
                                                                            :background-color (theme-pull :theme/base-block-color-selected nil)
                                                                            :color  (theme-pull :theme/editor-outer-rim-color nil)
                                                              ;    )
                                                                            :opacity (if ;(= t selected-tab)
                                                                                      (some #(= t %) matching)
                                                                                       1.0 0.4)
                                                                            :backdrop-filter "blur(8px)"
                                            ;:border-radius (when (is-last? t) "0px 0px 10px 0px")
                                                                            :padding-left "5px"
                                                                            :padding-right "5px"
                                                                            :height "30px" ;:width (px (+ 0 (* (count (str t)) 11)))
                                                                            :border (str "1px solid " (theme-pull :theme/block-tab-selected-font-color nil))}])
                     ;[re-com/box :child ">"]
                                           [(if (and (not @db/show-snaps?) @db/vertical-snaps?) re-com/v-box re-com/h-box)
                                            :align :end :justify :end
                                            :children
                                            ((if (and (not @db/show-snaps?) @db/vertical-snaps?)
                                               reverse vec)
                                             [[re-com/md-icon-button :src (at)
                                               :md-icon-name (if @db/vertical-snaps? "zmdi-chevron-up" "zmdi-chevron-down")
                                               :tooltip "minimize tab menu"
                                               :style {:background-color (theme-pull :theme/base-block-color-selected nil)
                                                       :color (theme-pull :theme/editor-outer-rim-color nil)
                                                       :padding-left "4px"
                                                       :margin-left "-3px"
                                                       :font-size "20px"
                                                       :border-radius (when @db/vertical-snaps? "0px 0px 0px 13px")
                                                       :padding-top "3px"
                                                       :width "30px"
                                                       :height "30px"}
                                               :attr {:on-click #(reset! db/vertical-snaps? (not @db/vertical-snaps?))}]

                                              [re-com/md-icon-button :src (at)
                                               :md-icon-name (if @db/show-snaps? "zmdi-chevron-right" "zmdi-chevron-left")
                                               :tooltip "minimize tab menu"
                                               :style {;:transform (when @db/show-tabs? "rotate(90deg)")
                                                       :background-color (theme-pull :theme/base-block-color-selected nil)
                                                       :color (theme-pull :theme/editor-outer-rim-color nil)
                                                       :padding-left "4px"
                                                       :margin-left "-3px"
                                                       :font-size "20px"
                                                       :padding-top "3px"
                                                       :width "30px"
                                                       :height "30px"}
                                               :attr {:on-click #(reset! db/show-snaps? (not @db/show-snaps?))}]])]))

                                (when (and @db/show-snaps? false)
                                  [re-com/md-icon-button :src (at)
                                   :md-icon-name "zmdi-plus"
                                   :tooltip "add new tab"
                                   :style {;:transform (when @db/show-tabs? "rotate(90deg)")
                                 ;:background-color (theme-pull :theme/base-block-color-selected nil)
                                           :color (theme-pull :theme/editor-outer-rim-color nil)
                                           :padding-left "4px"
                                 ;:opacity 0.4
                                           :margin-left "-3px"
                                           :font-size "20px"
                                           :padding-top "3px"
                                           :width "30px"
                                           :height "30px"}
                        ;:attr {:on-click #(let [new-tab (ut/make-tab-name)]
                        ;                    (re-frame/dispatch [::bricks/add-tab new-tab])
                        ;                    (re-frame/dispatch [::bricks/select-tab new-tab]))}
                                   ]
                        ;[re-com/box :child "+"]
                                  )))
       :style {:position "fixed" :top 0 :right 0
               :z-index 999
               :backdrop-filter "blur(8px)"
               :user-select "none"
               :transition_NOT "all 0.2s ease-in-out"
               :transform-style "preserve-3d"}])))

(def on-canvas? (reagent/atom false))

(defn mouse-move-handler2 [offset]
  (fn [evt]
    (let [start-x (.-clientX evt)
          start-y (.-clientY evt)
          off-x (:x offset)
          off-y (:y offset)
          x      (- start-x off-x) ;
          y      (- start-y off-y)]
      ;(reset! detached-coords [x y])
      (tap> [:newey x y @on-canvas?]))))

(defn mouse-up-handler2 [on-move]
  (fn me [evt]
    (do
      (gevents/unlisten js/window EventType.MOUSEMOVE on-move))))

(defn mouse-down-handler2 [e]
  (let [{:keys [left top]} (bricks/get-client-rect e)
        offset             {:x (- (.-clientX e) left)
                            :y (- (.-clientY e) top)}
        on-move            (mouse-move-handler2 offset)]
    (do
      (gevents/listen js/window EventType.MOUSEMOVE on-move))
    (gevents/listen js/window EventType.MOUSEUP
                    (mouse-up-handler2 on-move))))

;; (defn calliope-alert []
;;   [re-com/h-box :children
;;    [;[:img {:src "images/test-kick-icon.png" :width 30 :height 30}]
;;     [re-com/md-icon-button :src (at)
;;      :md-icon-name (if @db/kick-alert "zmdi-star" "zmdi-star-outline")
;;      :style {;:color "red"
;;              :margin-top "-4px"
;;              :font-size "34px"}]
;;     [re-com/box :child "new kick from " :style {:font-weight 200 :padding-left "5px" :opacity 0.34}]
;;     [re-com/gap :size "6px"]
;;     [re-com/box :child " :ai-worker/ " :style {}]
;;     [re-com/box :child " Calliope"  ; " Buffy"
;;      :style {:font-family "Homemade Apple" :color "orange" :margin-top "2px"}]]])

(defn render-icon [icon]
  (if (and (not (empty? icon))
           (not (nil? icon)))
    (if (cstr/includes? icon "zmdi")
      [re-com/md-icon-button :src (at)
       :md-icon-name icon
       :style {;:color bcolor
               ;:cursor "grab"
               :font-size "15px"}
       :attr {}]
      [re-com/box
       ;:style {:margin-left "-10px"}
       :size "none"
       ;:width "30px"
       :height "24px"
       :child [:img {:src icon
             ;:height "50px"
             ;:height "auto"
             ;:width "50px"
                     :width "100%"}]])
    " "))

(re-frame/reg-event-db
 ::update-user-params-hash
 (fn [db _]
   (let [pp (get-in db [:click-param]) ;; was param
         new-h (hash pp)
         client-name (get db :client-name)]
     (tap> [:push-params! client-name (keys pp)])
     (re-frame/dispatch [::wfx/request   :default ;; just a push, no response handling
                         {:message      {:kind         :sync-client-params
                                         :params-map   pp
                                         ;:flow-params-map 
                                         :client-name  client-name}}])
     (assoc db :user-params-hash new-h))))

(re-frame/reg-sub
 ::watch-user-params
 (fn [db]
   (hash (get-in db [:click-param])))) ;; was :param

(re-frame/reg-sub
 ::user-params-hash
 (fn [db]
   (get db :user-params-hash)))

(re-frame/reg-sub
 ::rs-overrides-hashmap
 (fn [db]
   (get db :rs-overrides-hashmap)))

(re-frame/reg-event-db
 ::set-rs-overrides-hashmap
 (fn [db [_ new]]
   (assoc db :rs-overrides-hashmap new)))

(re-frame/reg-sub
 ::theme-colors-hashmap
 (fn [db]
   (get db :theme-colors-hashmap)))

(re-frame/reg-event-db
 ::set-theme-colors-hashmap
 (fn [db [_ new]]
   (assoc db :theme-colors-hashmap new)))

(re-frame/reg-event-db
 ::rename-tab
 (fn [db [_ old new]]
   (-> db
       (assoc :panels (ut/update-nested-tabs (get db :panels) old new))
       (assoc :tabs (vec (map (fn [item] (if (= item old) new item)) (get db :tabs))))
       (assoc :selected-tab new))))

(re-frame/reg-sub
 ::view-data
 (fn [db [_ kp]]
   (get-in db (into [:panels (get db :selected-block)] kp))))

(def panels [:flow? :buffy? :editor?])

(re-frame/reg-sub
 ::minimized-system-panels
 (fn [db _]
   (vec (for [p panels
              :when (not (get db p))]
          [p p]))))

(re-frame/reg-event-db
 ::toggle-sys-panel
 (fn [db [_ kkey]]
   (assoc db kkey (not (get db kkey)))))

(re-frame/reg-sub
 ::is-fire-on-change?
 (fn [db [_ flow-id]]
   (get-in db [:runstreams flow-id :fire?] false)))

(defn run-flow [flow-id overrides] ;; same as honeycomb logic w/o the render obvs
  (if @(re-frame/subscribe [::is-fire-on-change? flow-id])
    (let [client-name @(re-frame/subscribe [::bricks/client-name])
          base-opts {:increment-id? false}
          running-key (keyword (str "flow/" flow-id ">*running?"))
          running? @(re-frame/subscribe [::conn/clicked-parameter-key [running-key]])
          runstreamed? (= overrides :runstream-overrides)
          overrides (if runstreamed?
                      @(re-frame/subscribe [::bricks/runstream-overrides flow-id])
                      overrides)
          overrides? (not (empty? overrides))]
      (when (not running?) ;(not (some (fn [x] (= x text)) @db/speech-log))
        (let [fstr (str "changed! running flow " flow-id (when overrides? " (with overrides)"))
              w (/ (count fstr) 4.1)]
          (re-frame/dispatch [::wfx/request :default
                              {:message    {:kind :run-flow
                                            :flow-id flow-id
                                            :flowmap flow-id
                                            :opts (if (map? overrides)
                                                    (merge base-opts
                                                           {:overrides overrides})
                                                    base-opts)
                                            :client-name client-name}
                               :on-response [::http/socket-response]
                               :on-timeout [::http/timeout-response]
                               :timeout    500000}])
          (ut/dispatch-delay 800 [::http/insert-alert fstr w 1 5])
        ;;   (re-frame/dispatch [::conn/click-parameter ;; kinda cheating, but feels better
        ;;                       [:flow (keyword (str flow-id ">*running?"))] true])
          )))
    (tap> [:no-fire-on-change flow-id :skipping])))

(defn task-bar []
  (let [min-panels @(re-frame/subscribe [::bricks/all-panels-minimized])
        user-param-hash2 @(re-frame/subscribe [::watch-user-params])
        selected-view @(re-frame/subscribe [::bricks/editor-panel-selected-view])
        sselected-view  @(re-frame/subscribe [::conn/clicked-parameter [:param :selected-view]])
        editor? @(re-frame/subscribe [::bricks/editor?])
        user-param-hash1 @(re-frame/subscribe [::user-params-hash])
        minimized-system-panels @(re-frame/subscribe [::minimized-system-panels])
        audio-playing? false ;@(re-frame/subscribe [::audio/audio-playing?])
        rs-overrides @(re-frame/subscribe [::bricks/runstream-override-map])
        rs-overrides-hashmap @(re-frame/subscribe [::rs-overrides-hashmap])
        theme-colors-hashmap @(re-frame/subscribe [::theme-colors-hashmap])
        theme-colors (theme-pull :theme/data-colors db/data-colors)
        ;_ (tap> [:min min-panels minimized-system-panels])
        min-panels (vec (into min-panels minimized-system-panels))
        ;;mats @(re-frame/subscribe [::bricks/materialized-theme]) ;; prints out the materilized theme w/o refs
        ]
    ;;(tap> [selected-view sselected-view])

    (when (not= rs-overrides rs-overrides-hashmap) ;; execute flows when mutated
      (do
        (tap> [:runstream-overides-change! rs-overrides rs-overrides-hashmap])
        (doseq [[k v] rs-overrides
                :when (not= (get rs-overrides-hashmap k) v)]
          (tap> [:runstream-override! k v])
          (when (not (empty? rs-overrides-hashmap)) ;; on first run, dont want to trigger all...
            (run-flow k v))))
      (re-frame/dispatch [::set-rs-overrides-hashmap rs-overrides]))

    (when (not= theme-colors theme-colors-hashmap) ;; execute flows when mutated
      (tap> [:theme-colors-change! theme-colors theme-colors-hashmap])
      (ut/apply-theme (bricks/code-mirror-theme))
      (re-frame/dispatch [::set-theme-colors-hashmap theme-colors]))

    ;;(when (not= (theme-pull :theme/data-colors db/data-colors) ))


    (when true ; some atom to short-circuit this if needed later.. (putting it here since this is a "cheap loop" bricks/grid is NOT)
      (doall (when (not (= user-param-hash1 user-param-hash2))  ;; core update for blind backend updating / external editing 2-way
               (re-frame/dispatch [::update-user-params-hash]))))

    (when (and editor? (not (= selected-view sselected-view)))
      (let [view-data @(re-frame/subscribe [::view-data selected-view])]
        ;(tap> [:view-data selected-view view-data])
        (re-frame/dispatch [::conn/click-parameter [:param :selected-view] selected-view])
        (re-frame/dispatch [::conn/click-parameter [:param :selected-view-data] view-data])))

    (when (ut/not-empty? min-panels)
      [re-com/h-box
       :style {:position "fixed"
               :bottom 0
               :left "50%"
               :z-index 999
               ;:transition "all 0.6s ease-in-out"
        ;;        :box-shadow (let [block-id :audio
        ;;                        ; block-type :speak
        ;;                          talking-block? true]
        ;;                      (cond (and audio-playing? talking-block?)
        ;;                            (str "1px 1px " (px (* 80 (+ 0.1
        ;;                                                         (get @db/audio-data block-id))))  " "
        ;;                                 (theme-pull :theme/editor-outer-rim-color nil))
        ;;                                                    ;(str "1px 1px 70px red")
        ;;                           ;(or (and hovered? @on-block) kit-drop?)
        ;;                           ;(str "1px 1px 30px " (get db/block-codes block-type) 87)
        ;;                            :else "none"))
               :transform "translateX(-50%)"}
       ;:gap "4px"
       :children (for [[e name] min-panels
                       :let [nn (try (if (empty? name) e name) (catch :default _ e))
                             sys-panel? (true? (some #(= name %) panels))
                             nn (if sys-panel? (-> (str nn) (cstr/replace ":" "") (cstr/replace "?" "")) nn)
                             icon (if sys-panel? "zmdi-developer-board" @(re-frame/subscribe [::bricks/panel-icon e]))]]
                   [re-com/h-box
                    :align :center :justify :center
                    :height "30px"
                    :gap "6px"
                  ;:width "30px"
                    :attr {:on-click #(if sys-panel?
                                        (re-frame/dispatch [::toggle-sys-panel e])
                                        (re-frame/dispatch [::bricks/toggle-minimize-block e]))}
                    :style {:background-color "#00000099"
                            :padding-left "7px"
                            :border (str "1px solid "
                                         ;(theme-pull :theme/block-tab-selected-font-color nil)
                                         (theme-pull :theme/editor-outer-rim-color nil))
                            ;:transition "all 0.6s ease-in-out"
                            ;:transition-property "opacity, transform, overlay, display"
                            ;:transition-duration "0.7s"
                            ;:transition-behavior "allow-discrete"
                            :padding-right "7px"
                            :cursor "pointer"
                            :color (theme-pull :theme/editor-outer-rim-color nil)
                            :backdrop-filter "blur(2px)"}
                    :children [[re-com/box :child (str nn)]
                               [render-icon icon]]])])))

(re-frame/reg-event-db
 ::set-session-loading
 (fn [db [_ session]]
   (assoc db :loading-session session)))

(re-frame/reg-sub
 ::session-loading
 (fn [db _]
   (get db :loading-session)))

(defn image-component [s t h]
  (let [timestamp (reagent/atom (js/Date.now))]
    (js/setInterval #(reset! timestamp (js/Date.now)) 5000) ; refresh every 5 seconds
    (fn []
      (let [url (str "snaps/" (cstr/replace (str s) ".edn" ".jpg")
                     "?timestamp=" @timestamp)
            client-name @(re-frame/subscribe [::bricks/client-name])
            client-name (cstr/replace (str client-name) ":" "")
            s-loading @(re-frame/subscribe [::session-loading])
            current? (= (cstr/replace (str s) ".edn" "") client-name)
            loading? (= s s-loading)]
        [re-com/v-box
         :size "auto"
         ;:height (px (* h 0.3))
         :style {:cursor "pointer"
                 :border-radius "8px"
                 :border (cond
                           loading? (str "3px dashed " (theme-pull :theme/editor-outer-rim-color nil) 88)
                           current? (str "3px solid " (theme-pull :theme/editor-outer-rim-color nil))
                           :else "3px solid #00000000")}
         :attr {:on-click #(do (re-frame/dispatch [::set-session-loading s])
                               (re-frame/dispatch [::http/load-session (str "./snaps/" s)]))}
         :children [[re-com/box
                     :size "auto"
                     :child [:img {:src url
                                   :style {:max-width "100%"
                                           :max-height "100%"
                                           :border-radius "8px"
                                           :object-fit "contain"}
                                   ;:width "auto"
                                   ;:height "auto"
                                   ;:max-width "33%"
                                   ;:height (* h 0.45)
                                   }]]
                    [re-com/h-box
                     :size "auto"
                     :align :end
                     :justify :between
                     :height "10px"
                     :style {;:padding-right "5px"
                             :padding-left "4px"
                             :margin-bottom "3px" :font-weight 700 :font-size "9px"}
                     :children [[re-com/box
                                 :size "auto"
                                 :child (str t " ago")]

                                (if loading?
                                  [re-com/md-icon-button
                                   :md-icon-name "zmdi-refresh"
                                   :class "rotate linear infinite"
                                   :style {:font-size  "15px"
                                           :transform-origin "7.5px 12px"
                                           ;:opacity    0.5
                                           ;:padding    "0px"
                                           ;:margin-top "6px"
                                           :margin-bottom "-8px"}]
                                  [re-com/gap :size "10px"])]]]]))))

(defn session-modal []
  (let [hh @(re-frame/subscribe [::subs/h])
        ww @(re-frame/subscribe [::subs/w])
        per-page 9
        w (* ww 0.44) ;500
        h (* hh 0.5) ;500
        client-name @(re-frame/subscribe [::bricks/client-name]) ;; reactions
        sessions @(re-frame/subscribe [::bricks/sessions])
        left (- (/ ww 2) (/ w 2))
        top (- (/ hh 2) (/ h 2))
        items (count sessions)
        items (if (odd? items) items (inc items))
        sessions (take per-page (sort-by last sessions))
        sessions (partition-all 3 sessions)]
    [re-com/modal-panel
     :style {:z-index 99999 ;; on top of alerts?
             :padding "0px"}
     :frame-style {:background-color "#000000"}
     :parts {:child-container
             {:style {:left left
                      :transform "scale(1.5)"
                      :top top
                      :font-size "8px"
                      ;:border (str "5px solid " (theme-pull :theme/editor-outer-rim-color nil))
                      :background-color "#000000"
                      ;:background-color "orange"
                      :position "fixed"}}}
     :backdrop-opacity 0.75
     :backdrop-on-click #(re-frame/dispatch [::bricks/disable-session-modal])
     :child  [re-com/v-box
              :size "auto"
              :gap "4px"
              :style {:background-color "#000000"}
              :width (px w)
              :height (px h)
              :children (for [sess sessions]
                          [re-com/h-box
                           :size "auto" :gap "4px"
                           :children (for [ss sess
                                           :let [s (get ss 0)
                                                 rowcnt (count sess)
                                                 t (get ss 1)]]
                                       [image-component s t h rowcnt items])])]]))



(defn main-panel []
  (let [editor? (and @(re-frame/subscribe [::bricks/editor?]) (not @bricks/dragging?))
        buffy? @(re-frame/subscribe [::bricks/buffy?])
        flows? @(re-frame/subscribe [::bricks/flow?])
        external? @(re-frame/subscribe [::bricks/external?])
        session? @(re-frame/subscribe [::bricks/session-modal?])
        lines? @(re-frame/subscribe [::bricks/lines?])
        peek? @(re-frame/subscribe [::bricks/peek?])
        auto-run? @(re-frame/subscribe [::bricks/auto-run?])
        rekt [@db/kick-alert]
        ;audio-react-hack [@db/audio-data]
        ;selected-block @(re-frame/subscribe [::bricks/selected-block])
        ;editor-ready? (and editor? (not (= selected-block "none!")) (not (nil? selected-block)))
        ;;rr @db/context-modal-pos
        websocket-status (select-keys @(re-frame/subscribe [::http/websocket-status]) [:status :datasets :panels :waiting])
        online? (true? (= (get websocket-status :status) :connected))
        hh @(re-frame/subscribe [::subs/h])
        ww @(re-frame/subscribe [::subs/w])
        selected-block @(re-frame/subscribe [::bricks/selected-block])
        selected-tab @(re-frame/subscribe [::bricks/selected-tab])
        selected-block? (true? (not (or (nil? selected-block) (= selected-block "none!"))))
        screen-name (ut/unkeyword @(re-frame/subscribe [::bricks/screen-name]))
        client-name @(re-frame/subscribe [::bricks/client-name]) 
        ;bricks-high (+ (js/Math.floor (/ hh bricks/brick-size)) 1)
        ;bricks-wide (+ (js/Math.floor (/ ww bricks/brick-size)) 1)
        flow-watcher-subs-grouped @(re-frame/subscribe [::bricks/flow-watcher-subs-grouped])
        server-subs @(re-frame/subscribe [::bricks/server-subs])
        ;lines? true ;false
        coords (if lines? ;; expensive otherwise
                 (let [subq-mapping (if lines? @(re-frame/subscribe [::bricks/subq-mapping]) {})
                       dwn-from-here (vec (bricks/downstream-search subq-mapping selected-block))
                       up-from-here (vec (bricks/upstream-search subq-mapping selected-block))
                       involved (vec (distinct (into dwn-from-here up-from-here)))
                       subq-blocks @(re-frame/subscribe [::bricks/subq-panels selected-block])
                       ;parent-of-selected? (some #(= % brick-vec-key) subq-blocks)
                       smap (into {} (for [b (keys subq-mapping)] {b (bricks/downstream-search subq-mapping b)}))
                       lmap (vec (distinct
                                  (apply concat (for [[k downs] smap]
                                                  (remove nil?
                                                          (for [d downs
                                                                :when (and (not (cstr/starts-with? (str d) ":reco-preview"))
                                                                           (not (cstr/starts-with? (str k) ":reco-preview")))]
                                                            (let [src-r @(re-frame/subscribe [::bricks/panel-px-root k])
                                                                  src-h @(re-frame/subscribe [::bricks/panel-px-height k])
                                                                  src-w @(re-frame/subscribe [::bricks/panel-px-width k])
                                                                  dd (if (not (cstr/starts-with? (str d) ":block"))
                                                                       @(re-frame/subscribe [::bricks/lookup-panel-key-by-query-key d]) d)
                                                                  involved? (some #(= % dd) involved)
                                                                  dest-r @(re-frame/subscribe [::bricks/panel-px-root dd])
                                                                  dest-h @(re-frame/subscribe [::bricks/panel-px-height dd])
                                                                  dest-w @(re-frame/subscribe [::bricks/panel-px-width dd])

                                                                  ;same-tab? @(re-frame/subscribe [::bricks/same-tab? k d])
                                                                  t1 @(re-frame/subscribe [::bricks/what-tab k])
                                                                  t2 @(re-frame/subscribe [::bricks/what-tab d])

                                                                  x1 (+ (first src-r) src-w)
                                                                  y1 (+ (last src-r) (/ src-h 2))
                                                                  x2 (first dest-r)
                                                                  y2 (+ (last dest-r) (/ dest-h 2))]
                                                              (when (and (not (= src-r dest-r)) (= t1 t2 selected-tab))
                                                                (vec (flatten
                                                                      [(if peek? (- x1 (* src-w 0.15)) x1)
                                                                       y1 ;(if peek? (* y1 0.7) y1)
                                                                       (if peek? (+ x2 (* dest-w 0.15)) x2)
                                                                       y2 ;(if peek? (* y2 0.7) y2)
                                                                       involved?
                                                                       (cond (= dd selected-block) "#9973e0"
                                                                             (some #(= % dd) subq-blocks) "#e6ed21" ;; parent-of-selected
                                                                             (some #(= % dd) (bricks/upstream-search subq-mapping selected-block)) "#7be073" ;; upstream?
                                                                             (some #(= % dd) (bricks/downstream-search subq-mapping selected-block)) "#05dfff" ;; downstream?
                                                                             :else "orange")
                                                            ;"#E6E6FA"
                                                                       k d nil]))))))))))]
                   ;(tap> [:vv selected-block involved])
                   lmap) [])


        ;lines-test [[145 145 453 543 0 0 0] [456 456 65 457 0 0 0] [575 567 859 567 0 0 0]]
        ]

    ;;(tap> [:bgg (theme-pull :theme/canvas-background-css nil)])
    ;(tap> (px (js/Math.ceil (* 225 (+ 0.5 (get @db/audio-data :audio))))))
    ;(tap> [:view hh ww])
    ;(tap> [:coords coords])

    ;;;(tap> [@db/context-modal-pos @bricks/dyn-dropper-hover @bricks/on-block?])


    (bricks/droppable (if @rvbbit-frontend.flows/dragging-port? ["meta-menu" :flow-port] ["meta-menu"]) ;; eyes emoji. crossing the streams :flow-port
                      [(js/Math.floor (/ (ifnil (first @db/context-modal-pos) 0) bricks/brick-size))
                       (js/Math.floor (/ (ifnil (last @db/context-modal-pos) 0) bricks/brick-size))]
                      [re-com/box
                       :style {:background-color "black"}
                       :attr {:id "base-canvas"
                              ;:transition_NOT "all 0.1s ease-in-out"
                              ;:transform-style "preserve-3d"
                              ;:transform "scale(0.5)"
                              
                              ;:on-click (fn [x] (tap> [(.-clientX x)
                              ;                         (.-clientY x)]))
                              ;:on-drag mouse-down-handler2
                             ; :on-drag-enter #(tap> [:entered?])
                             ; :on-drag-leave #(when true (tap> [:enter?]))
                             ; :on-mouse-over #(reset! on-canvas? true)
                             ; :on-mouse-leave #(reset! on-canvas? false)
                             ; :on-mouse-down mouse-down-handler2
                              :on-drag-over #(bricks/tag-screen-position %)
                              :on-context-menu (re-com/handler-fn
                                                #_{:clj-kondo/ignore [:unresolved-symbol]}
                                                (bricks/tag-screen-position event) ;; event is magic in handler-fn
                                                (when (and (not @bricks/over-block?) (not @bricks/over-flow?))
                                                  (bricks/insert-new-block [(js/Math.floor (/ (first @db/context-modal-pos) bricks/brick-size))
                                                                            (js/Math.floor (/ (last @db/context-modal-pos) bricks/brick-size))] 5 4))
                                                #_{:clj-kondo/ignore [:unresolved-symbol]}
                                                (.preventDefault event))
                              :on-mouse-down #(when (= (.-button %) 1) ;; middle click
                                                (do
                                                  #_{:clj-kondo/ignore [:unresolved-symbol]}
                                                  (bricks/tag-screen-position %) ;; event is magic in handler-fn
                                                  (when (and (not @bricks/over-block?) (not @bricks/over-flow?))
                                                    (let [x  (keyword (cstr/replace (str (talltale.core/quality-color-animal)) " " "-"))
                                                          x (ut/safe-key x)
                                                          kp [:param x]]
                                                      (bricks/insert-new-block [(js/Math.floor (/ (first @db/context-modal-pos) bricks/brick-size))
                                                                                (js/Math.floor (/ (last @db/context-modal-pos) bricks/brick-size))] 5 4
                                                                               {:drag-meta {:type :open-input}
                                                                                :w 6
                                                                                :selected-view x
                                                                                :h 2
                                                                                :views
                                                                                ;; {x [:box :align :center :justify :center :style
                                                                                ;;     {:font-size "106px"
                                                                                ;;      :font-weight 700
                                                                                ;;      :padding-top "6px"
                                                                                ;;      :margin-top "-8px"
                                                                                ;;      :color :theme/editor-outer-rim-color
                                                                                ;;      :font-family :theme/base-font} :child
                                                                                ;;     [:open-input
                                                                                ;;      [kp :panel-width+100
                                                                                ;;       :panel-height+80]]]}
                                                                                {x [:open-input
                                                                                    {:kp kp ;[:param :remarkable-blush-louse]
                                                                                     :width-int :panel-width+100
                                                                                     :height-int :panel-height+80
                                                                                     :syntax "clojure"}]}
                                                                                ;; {x [:open-input
                                                                                ;;     [kp :panel-width+100
                                                                                ;;      :panel-height+80 "clojure"]]}
                                                                                })
                                                      (let [default "new parameter value!" ;(if (nil? default) "new parameter value!" default)
                                                            vv @(re-frame/subscribe [::conn/clicked-parameter kp])
                                                            exists? (not (nil? vv))
                                                            _ (when (not exists?) (re-frame/dispatch [::conn/click-parameter kp default]))])))
                                                  #_{:clj-kondo/ignore [:unresolved-symbol]}
                                                  (.preventDefault %)))}
                       :size "1"
                       :width (px ww)
                       :height (px hh)
                       :child [re-com/v-box
                               :size "1"
                               :style (let [custom-map (theme-pull :theme/canvas-background-css nil)
                                            cc (str ;(ut/invert-hex-color 
                                                     (theme-pull :theme/editor-outer-rim-color nil)
                                                    ;"#000000"
                                                    ; )
                                                    87)]
                                        (merge custom-map
                                               {:font-family (theme-pull :theme/base-font nil)
                                                :transition "filter 4s ease-in-out"
                                                :filter (when (not online?) "grayscale(100%)")
                                        ;;       :box-shadow (str "inset 0px 0px 400px " (theme-pull :theme/editor-outer-rim-color nil)) ;; talking animation ?
                                        ;;       :box-shadow (let [block-id :audio
                                        ;;                         ;audio-playing? @(re-frame/subscribe [::audio/audio-playing?])
                                        ;;                         ]
                                        ;;                     (when (not (empty? @db/audio-data)) ;audio-playing?
                                        ;;                       (str "inset 0px 0px " (px (js/Math.ceil (* 225 (+ 0.5 (get @db/audio-data :audio)))))  " "
                                        ;;                            ;(theme-pull :theme/editor-outer-rim-color nil)
                                        ;;                            "#000000"
                                        ;;                            )))
                                               }
                                                (when (or @bricks/dragging? @bricks/mouse-dragging-panel?) ;; editor? ;;(not online?)
                                                  {:background-image (str ;"linear-gradient(0deg, " cc " 1px, transparent 1px), linear-gradient(90deg, " cc " 1px, transparent 1px)"
                                                                          "linear-gradient(0deg, " cc " 2px, transparent 8px), linear-gradient(90deg, " cc " 2px, transparent 8px)" 
                                                                      (when (get custom-map :background-image) ", ")
                                                                          (get custom-map :background-image))
                                                   :background-size (str "50px 50px, 50px 50px"
                                                                         (when (get custom-map :background-size) ", ")
                                                                         (get custom-map :background-size))})))
                               ;; {:font-family "Alata" ;"Roboto" ;"Lato"
                              ;;          ;:background-size "50px 50px"
                              ;;          ;:background-image "linear-gradient(to right, #00000055 1px, transparent 1px),linear-gradient(to bottom, #00000055 1px, transparent 1px)"
                              ;;          :background-image "url(images/fake-brick.png)"
                              ;;          :background-repeat "repeat"
                              ;;          :background-color "#47555e"}

                               :children [[tab-menu]
                                          [snapshot-menu]

                                          (when session? [session-modal])

                                          (when (and editor? (not @bricks/mouse-dragging-panel?))
                                            [rc/catch [editor-panel 33 10]])

                                          (when (or (and @bricks/dragging? (not @bricks/on-block?) (not @bricks/over-flow?)) @bricks/swap-layers?)
                                            [re-com/box :child " "
                                             :style {:background-color (str (theme-pull :theme/editor-background-color nil) 22) ;; "#00000022"
                                                     :filter "drop-shadow(16px 16px 20px red) invert(75%)"
                                                     :position "fixed"
                                                     :left (* (js/Math.floor (/ (first @db/context-modal-pos) bricks/brick-size)) bricks/brick-size)
                                                     :top (* (js/Math.floor (/ (last @db/context-modal-pos) bricks/brick-size)) bricks/brick-size)}
                                             :width (px (* (get @bricks/dragging-body :w) bricks/brick-size))
                                             :height (px (* (get @bricks/dragging-body :h) bricks/brick-size))])

                                          (when (and buffy? (not @bricks/mouse-dragging-panel?))
                                            [rc/catch [buffy/chat-panel]])

                                          (when flows? ;(and flows? (not @bricks/mouse-dragging-panel?))
                                            [rc/catch [flows/flow-panel]])

                                         ; [bricks/squareql-logo (- bricks-wide 3) (- bricks-high 3)]

                                         ; [rc/catch
                                          [bricks/grid]
                                         ;  ]


                                          [re-com/box
                                           :child [re-com/md-icon-button :src (at)
                                                   :md-icon-name "zmdi-pizza"
                                                   :tooltip "  toggle floating editor panel (SPACE)"
                                                   :style {:color "#ffffff"
                                                           :cursor "pointer"
                                                           :opacity (if editor? 1.0 0.3)
                                                           :transform (if editor? "rotate(-90deg)" "")
                                                           :margin-top "-2px"
                                                           :padding-left "2px"
                                                           :font-size "15px"}
                                                   :attr {:on-click #(re-frame/dispatch [::bricks/toggle-editor])}]
                                           :width "20px"
                                           :height "20px"
                                           ;:tooltip "foo"
                                           :style {:position "fixed"
                                                   ;:opacity 0.3
                                                   :z-index 98
                                                  ; :border-radius "0px 7px 0px 0px"
                                                   :bottom 0 :left 0
                                                   :background-color "#00000022"
                                                   :color "white"}]

                                          [re-com/box
                                           :child [re-com/md-icon-button :src (at)
                                                   :md-icon-name (if lines? "zmdi-layers" "zmdi-layers-off")
                                                   :tooltip "show lineage / sub-query lines? (L)"
                                                   :style {:color "#ffffff"
                                                           :cursor "pointer"
                                                           :opacity (if lines? 1.0 0.3)
                                                           :margin-top "-2px"
                                                           :padding-left "2px"
                                                           :font-size "15px"}
                                                   :attr {:on-click #(re-frame/dispatch [::bricks/toggle-lines])}]
                                           :width "20px"
                                           :height "20px"
                                           ;:tooltip "foo"
                                           :style {:position "fixed"
                                                   ;:opacity 0.3
                                                   :z-index 98
                                                   :border-radius "0px 7px 0px 0px"
                                                   :bottom 0 :left 23
                                                   :background-color "#00000022"
                                                   :color "white"}]

                                          [re-com/box
                                           :child [re-com/md-icon-button :src (at)
                                                   :md-icon-name (if peek? "zmdi-eye" "zmdi-eye-off")
                                                   :tooltip "toggle peek mode (P)"
                                                   :style {:color "#ffffff"
                                                           :cursor "pointer"
                                                           :opacity (if peek? 1.0 0.3)
                                                           :margin-top "-2px"
                                                           :padding-left "2px"
                                                           :font-size "15px"}
                                                   :attr {:on-click #(re-frame/dispatch [::bricks/toggle-peek])}]
                                           :width "20px"
                                           :height "20px"
                                           ;:tooltip "foo"
                                           :style {:position "fixed"
                                                   ;:opacity 0.3
                                                   :z-index 98
                                                   :border-radius "0px 7px 0px 0px"
                                                   :bottom 0 :left 46
                                                   :background-color "#00000022"
                                                   :color "white"}]

                                          [re-com/box
                                           :child [re-com/md-icon-button :src (at)
                                                   :md-icon-name (if auto-run? "zmdi-refresh-sync" "zmdi-refresh-sync-off")
                                                   :tooltip "toggle auto-refresh (O)"
                                                   :style {:color "#ffffff"
                                                           :cursor "pointer"
                                                           :opacity (if auto-run? 1.0 0.3)
                                                           :margin-top "-2px"
                                                           :padding-left "2px"
                                                           :font-size "15px"}
                                                   :attr {:on-click #(re-frame/dispatch [::bricks/toggle-auto-run])}]
                                           :width "20px"
                                           :height "20px"
                                           ;:tooltip "foo"
                                           :style {:position "fixed"
                                                   ;:opacity 0.3
                                                   :z-index 98
                                                   :border-radius "0px 7px 0px 0px"
                                                   :bottom 0 :left 69
                                                   :background-color "#00000022"
                                                   :color "white"}]

                                          [re-com/box
                                           :child [re-com/md-icon-button :src (at)
                                                   :md-icon-name "zmdi-developer-board"
                                                   :tooltip "  toggle external editing"
                                                   :style {:color (if external? "red" "#ffffff")
                                                           :cursor "pointer"
                                                           :opacity (if external? 1.0 0.3)
                                                           :margin-top "-2px"
                                                           :padding-left "2px"
                                                           :font-size "15px"}
                                                   :attr {:on-click #(re-frame/dispatch [::bricks/toggle-external])}]
                                           :width "20px"
                                           :height "20px"
                                           ;:tooltip "foo"
                                           :style {:position "fixed"
                                                   ;:opacity 0.3
                                                   :z-index 98
                                                  ; :border-radius "0px 7px 0px 0px"
                                                   :bottom 0 :left 92
                                                   :background-color "#00000022"
                                                   :color "white"}]

                                          [re-com/box
                                           :child [re-com/md-icon-button :src (at)
                                                   :md-icon-name "zmdi-close"
                                                   :tooltip "un-select block (ESC)"
                                                   :style {:color "#ffffff"
                                                           :cursor "pointer"
                                                           :opacity (if selected-block? 1.0 0.3)
                                                           :margin-top "-2px"
                                                           :padding-left "2px"
                                                           :font-size "15px"}
                                                   :attr {:on-click #(re-frame/dispatch [::bricks/select-block "none!"])}]
                                           :width "20px"
                                           :height "20px"
                                           ;:tooltip "foo"
                                           :style {:position "fixed"
                                                   ;:opacity 0.3
                                                   :z-index 98
                                                   :border-radius "0px 7px 0px 0px"
                                                   :bottom 0 :left 115
                                                   :background-color "#00000022"
                                                   :color "white"}]

                                          (when @(re-frame/subscribe [::audio/audio-playing?])
                                            [re-com/md-icon-button :src (at)
                                             :md-icon-name "zmdi-speaker"
                                             :style {;:margin-top "4px"
                                                     :position "fixed" :left 138 :bottom 0
                                                     ;:opacity 0.2
                                                     :color "red"
                                                     :font-size "22px"}])

                                          (when @(re-frame/subscribe [::audio/recording?])
                                            [re-com/md-icon-button :src (at)
                                             :md-icon-name "zmdi-mic"
                                             :style {;:margin-top "-0px"
                                                     :position "fixed" :left 161 :bottom 0
                                                     ;:opacity 0.2
                                                     :color "red"
                                                     :font-size "22px"}])

                                          [re-com/h-box
                                           :size "none"
                                           :gap "8px"
                                           :style {:position "fixed" :left 138 :bottom 0 :font-weight 700
                                                   :color (if (not online?) "#ffffff" "#ffffff77")
                                                   :font-family (theme-pull :theme/monospaced-font nil)}
                                           :children [[re-com/box :size "none"
                                                       ;:style {:mid-width "167px"}
                                                       :child (str (get websocket-status :waiting -1)
                                                                   (str ", " (ut/nf (count server-subs)))
                                                                   (when (not online?) " (RVBBIT server is offline)"))]
                                                      (when (ut/ne? flow-watcher-subs-grouped)
                                                        [re-com/h-box
                                                         :style {:padding-left "10px" :font-size "12px"}
                                                         :gap "8px"
                                                         :children (for [[kk cnt] flow-watcher-subs-grouped]
                                                                     [re-com/h-box
                                                                      :style {:background-color (theme-pull :theme/editor-outer-rim-color nil)
                                                                              :padding-left "6px"
                                                                              :border-radius "5px 5px 0px 0px"
                                                                              :padding-right "6px"
                                                                              :color (theme-pull :theme/editor-background-color nil)}
                                                                      :gap "5px"
                                                                      :children [[re-com/box :child (str kk)]
                                                                                 [re-com/box :style {:opacity 0.5} :child (str cnt)]
                                                                                 [re-com/md-icon-button :md-icon-name "zmdi-close"
                                                                                  :style {:cursor "pointer" :color "white" :font-size "15px" :height "10px" :margin-top "-3px"}
                                                                                  :on-click #(do (tap> [:remove-flow-watchers client-name kk])
                                                                                                 (re-frame/dispatch [::wfx/request :default
                                                                                                                     {:message    {:kind :remove-flow-watcher
                                                                                                                                   :client-name client-name
                                                                                                                                   :flow-id kk}}]))]]])])]]

                                          (let [mem @(re-frame/subscribe [::bricks/memory])]
                                            [re-com/box
                                             :size "none"
                                             :style {:position "fixed" :left 2 :bottom 20 :font-weight 700 :color "#ffffff88"}
                                             :child (str (ut/bytes-to-mb (get mem 1)))
                                             ;:child (str mem)
                                             ])

                                        ;;   [re-com/box
                                        ;;    ;:child [:img {:src "images/pink-rabbit-head.png" :width 30 :height 20}]
                                        ;;    :child [:img {:src "images/test-kick-icon.png" :width 40 :height 40}]
                                        ;;    :width "40px"
                                        ;;    :height "40px"
                                        ;;    :style {:position "fixed"
                                        ;;            ;:opacity 0.3
                                        ;;            :z-index 98
                                        ;;            ;:border-radius "0px 7px 0px 0px"
                                        ;;            :bottom 2
                                        ;;            :right 2
                                        ;;            :background-color "#00000000"
                                        ;;            :color "white"}]

                                          [task-bar]

                                          [flows/alert-box]

                                          [re-com/box
                                           :child [re-com/md-icon-button :src (at)
                                                   :md-icon-name "zmdi-refresh"
                                                   :tooltip "refresh all"
                                                   :on-click #(do
                                                                ;(re-frame/dispatch [::bricks/clean-up-previews])
                                                                (re-frame/dispatch [::bricks/refresh-all]))
                                                   :style {:color "#ffffff"
                                                           :cursor "pointer"
                                                           :margin-top "-2px"
                                                           :padding-left "2px"
                                                           :font-size "15px"}]
                                           :width "20px"
                                           :height "20px"
                                           :style {:position "fixed"
                                                   ;:opacity 0.3
                                                   :z-index 98
                                                   ;:border-radius "0px 7px 0px 0px"
                                                   :bottom 0 :right 26
                                                   :background-color "#00000022"
                                                   :color "white"}]

                                          [re-com/box
                                           :child [re-com/md-icon-button :src (at)
                                                   :md-icon-name "zmdi-save"
                                                   :tooltip "save board (Ctrl-S)"
                                                   :on-click #(do
                                                                ;(re-frame/dispatch [::bricks/clean-up-previews])
                                                                (re-frame/dispatch [::http/save :skinny screen-name]))
                                                   :style {:color "#ffffff"
                                                           :cursor "pointer"
                                                           :margin-top "-2px"
                                                           :padding-left "2px"
                                                           :font-size "15px"}]
                                           :width "20px"
                                           :height "20px"
                                           :style {:position "fixed"
                                                   ;:opacity 0.3
                                                   :z-index 98
                                                   ;:border-radius "0px 7px 0px 0px"
                                                   :bottom 0 :right 3
                                                   :background-color "#00000022"
                                                   :color "white"}]


                                        ;;   [re-com/box
                                        ;;    :child [re-com/md-icon-button :src (at)
                                        ;;            :md-icon-name "zmdi-file-plus"
                                        ;;            :tooltip "save board *w data*"
                                        ;;            :on-click #(do
                                        ;;                         ;(re-frame/dispatch [::bricks/clean-up-previews])
                                        ;;                         (re-frame/dispatch [::http/save :fat screen-name]))
                                        ;;            :style {:color "#ffffff"
                                        ;;                    :cursor "pointer"
                                        ;;                    :margin-top "-2px"
                                        ;;                    :padding-left "2px"
                                        ;;                    :font-size "15px"}]
                                        ;;    :width "20px"
                                        ;;    :height "20px"
                                        ;;    :style {:position "fixed"
                                        ;;            ;:opacity 0.3
                                        ;;            :z-index 98
                                        ;;            ;:border-radius "0px 7px 0px 0px"
                                        ;;            :bottom 0 :right 0
                                        ;;            :background-color "#00000022"
                                        ;;            :color "white"}]


                                          (when (not (empty? coords))
                                            [:svg
                                             {:style {:width  (px ww) ;"6200px" ;; big ass render nothing gets cut off svg-wise + zoom and pannable
                                                      :height (px hh) ;"6200px"
                                                      :pointer-events "none"
                                                      :z-index 8}}
                                             (bricks/draw-lines coords)])


                                        ;;   (when (and flows? false true ; (not @db/dragging-flow-editor?)
                                        ;;              )
                                        ;;     (let [ph 600
                                        ;;           pw (* (.-innerWidth js/window) 0.7)
                                        ;;           [xx yy] @rvbbit-frontend.flows/detached-coords
                                        ;;           offsets @db/pan-zoom-offsets
                                        ;;           tx (first offsets)
                                        ;;           ty (second offsets)
                                        ;;           scale (nth offsets 2)]
                                        ;;       [:svg
                                        ;;        {:style {:width  (px pw) ;(px ww) ;"6200px" ;; big ass render nothing gets cut off svg-wise + zoom and pannable
                                        ;;                 :height (px ph) ;(px hh) ;"6200px"
                                        ;;                  ;:width   (px ww) ;"6200px" ;; big ass render nothing gets cut off svg-wise + zoom and pannable
                                        ;;                  ;:height  (px hh) ;"6200px"
                                        ;;                 :border "2px solid white"
                                        ;;                 :position "fixed"
                                        ;;                 :pointer-events "none"
                                        ;;                 :left xx :top yy
                                        ;;                 :transform (str "translate(" tx "px, " ty "px) scale(" scale ")")
                                        ;;                  ;:z-index 499
                                        ;;                 :z-index 200}}
                                        ;;        (rvbbit-frontend.flows/draw-lines (rvbbit-frontend.flows/generate-coords xx yy))]))
                                          ]]])))


