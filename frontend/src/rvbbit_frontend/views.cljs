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
   ["codemirror/addon/edit/closebrackets.js"]
   ["codemirror/addon/edit/matchbrackets.js"]
   ["codemirror/addon/hint/show-hint.js"]
   ["codemirror/mode/clojure/clojure.js"]
   ["codemirror/mode/shell/shell.js"]
   ["codemirror/mode/julia/julia.js"]
   ["codemirror/mode/markdown/markdown.js"]
   ["codemirror/mode/python/python.js"]
   ["codemirror/mode/r/r.js"]
   ["codemirror/mode/sql/sql.js"]
   ["react-codemirror2" :as cm]
   ["react-drag-and-drop" :as rdnd]
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



(def active-edge (reagent/atom nil))
(defonce editor-dimensions (reagent/atom {}))
(defonce editor-size (reagent/atom [33 10]))
(defonce last-sizes (atom nil))

(defn debounce [f interval] (let [dbnc (Debouncer. f interval)] (fn [& args] (.apply (.-fire dbnc) dbnc (to-array args)))))

(defonce detached-coords
  (reagent/atom (let [hh          (.-innerHeight js/window) ;; starting size set on load
                      bricks-high (+ (js/Math.floor (/ hh db/brick-size)) 1)
                      topper      (* (- bricks-high 12) db/brick-size) ;; size of editor
                      lefty       (* 2 db/brick-size) ;; always 2 brick from the left...
                     ]
                  [lefty topper])))

(defn mouse-move-handler
  [offset]
  (fn [evt]
    (let [start-x (.-clientX evt)
          start-y (.-clientY evt)
          off-x   (:x offset)
          off-y   (:y offset)
          x       (- start-x off-x)
          y       (- start-y off-y)]
      ;;(ut/tapp>> (str @detached-coords))
      (reset! detached-coords [x y]))))

(defn mutate-editor-as-edge [edge]
  (let [hh @(ut/tracked-subscribe [::subs/h])
        ww @(ut/tracked-subscribe [::subs/w])
        bb db/brick-size]
    (reset! last-sizes @editor-size) ;; save the old config
    (case edge
      :left (let [bw (Math/floor (* (/ ww bb) 0.3))
                  bh (- (Math/floor (/ hh bb)) 2)] 
              (reset! detached-coords [0 bb])
                (reset! editor-size [bw bh])) 
      :right (let [bw (Math/floor (* (/ ww bb) 0.3))
                   bwpx (* bw bb)
                   bh (- (Math/floor (/ hh bb)) 2)]
               (reset! detached-coords [(- ww bwpx) bb])
               (reset! editor-size [bw bh]))
      :top (let [bw (- (Math/floor (/ ww bb)) 2) 
                 bh (* (Math/floor (/ hh bb)) 0.3)]
             (reset! detached-coords [bb 32])
             (reset! editor-size [bw bh]))
      :bottom (let [bw (- (Math/floor (/ ww bb)) 2)
                    bh (* (Math/floor (/ hh bb)) 0.3)]
                (reset! detached-coords [bb (- (- hh 36) (* bh bb))])
                (reset! editor-size [bw bh])))
  ))

(defn clear-selection []
  (when (.-empty js/window.getSelection)
    (.empty js/window.getSelection))
  (when (.-removeAllRanges js/window.getSelection)
    (.removeAllRanges js/window.getSelection)))

(defn prevent-selection [e]
  (.preventDefault e))

;; Call this function when dragging ends


(defn mouse-up-handler
  [on-move]
  (fn me [evt] 
    (reset! bricks/dragging-editor? false) 
    (when @active-edge 
      (do (mutate-editor-as-edge @active-edge)
          (reset! active-edge nil)))
    (do ;(clear-selection)
       ; (.removeAttribute js/document.body "style")
        ;(js/setTimeout clear-selection 2000) ;;(clear-selection)  
        (gevents/unlisten js/window EventType.MOUSEMOVE on-move))))

(defn mouse-down-handler
  [e]
  (let [{:keys [left top]} (bricks/get-client-rect e)
        offset             {:x (- (.-clientX e) left) :y (- (.-clientY e) top)}
        on-move            (mouse-move-handler offset)]
    (when @last-sizes (do
                        (reset! editor-size @last-sizes)
                        (reset! last-sizes nil)))
    (reset! bricks/dragging-editor? true)
    ;(.setAttribute js/document.body "style" "user-select: none;")
    (clear-selection)
    (do (gevents/listen js/window EventType.MOUSEMOVE on-move))
    (gevents/listen js/window EventType.MOUSEUP (mouse-up-handler on-move))))

(defonce view-browser-query (reagent/atom {}))

(defonce data-browser-mode (reagent/atom :data))

(defonce data-browser-system-mode (reagent/atom :data))

(defn editor-panel-metadata
  []
  (let [{:keys [single-width-bricks single-width single-height bricks-wide bricks-tall]} @editor-dimensions
        client-name @(ut/tracked-sub ::bricks/client-name {})
        sql-calls  {:connections-sys {:select [;:database_name
                                               [[:case [:like :connection_str "%csv%"] "*csv-import-db*"
                                                 [:like :connection_str "%cache%"] "*cache-db*" :else :database_name]
                                                :database_name] :connection_id]
                                      :style-rules {[:* :highlight-8717ssa]
                                                    {:logic [:= :connection_str (cstr/replace (str client-name) ":" "")]
                                                     :style {;:background-color "red"
                                                             :border "1px solid red"}}}
                                      :where [:and [:not [:like :connection_str "%rvbbit%"]]
                                              [:in :connection_id {:select [:connection_id] :from [:fields] :group-by [1]}]] ;; only dbs that have tables logged
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
                   ;data-exists? @(ut/tracked-subscribe [::conn/sql-data-exists? [k]])
                   ;unrun-sql?   @(ut/tracked-subscribe [::conn/sql-query-not-run? [k] query])
                   data-exists?   @(ut/tracked-sub ::conn/sql-data-exists-alpha? {:keypath [k]})
                   unrun-sql?     @(ut/tracked-sub ::conn/sql-query-not-run-alpha? {:keypath [k] :query query})]
               (when (or (not data-exists?) unrun-sql?)
                 (conn/sql-data [k] query "system-db")))))
    [re-com/h-box 
     :size "auto" 
     :style {;:border "1px solid green"
             :color (str (theme-pull :theme/editor-font-color nil) 35)} ;; rows label under
     :children
     [[re-com/box :size "auto" :child 
       [bricks/magic-table :system-connections-list* [:connections-sys] (* single-width-bricks 0.3) bricks-tall [:database_name]]]
      [re-com/box :size "auto" :child
       [bricks/magic-table :system-tables-list* [:tables-sys] (* single-width-bricks 0.8) bricks-tall [:db_schema :connection_id :db_catalog]]]]]))

(defn search-panel-metadata
  []
  (let [{:keys [single-width-bricks single-width single-height bricks-wide bricks-tall]} @editor-dimensions
        client-name (ut/replacer (str @(ut/tracked-subscribe [::bricks/client-name])) ":" "")
        sql-calls   {:searches-types-sys     {:select   [:item_type] ;; :searches-types-sys/item_type
                                              :connection-id "autocomplete-db"
                                              :where    [:and [:not [:like :item_sub_type "%preview%"]]
                                                         [:not [:= :item_type "saved-block"]]]
                                              :from     [:client_items]
                                              :group-by [1]}
                     :searches-sub-types-sys {:select      [:item_key :is_live [[:count 1] :items]] ;; :searches-types-sys/item_type
                                              :connection-id "autocomplete-db"
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
                   ;data-exists? @(ut/tracked-subscribe [::conn/sql-data-exists? [k]])
                   ;unrun-sql?   @(ut/tracked-subscribe [::conn/sql-query-not-run? [k] query])
                   data-exists?   @(ut/tracked-sub ::conn/sql-data-exists-alpha? {:keypath [k]})
                   unrun-sql?     @(ut/tracked-sub ::conn/sql-query-not-run-alpha? {:keypath [k] :query query})]
               (when (or (not data-exists?) unrun-sql?)
                 (conn/sql-data [k] query )
                 (ut/tracked-dispatch [::bricks/insert-sql-source k query])))))
    [re-com/h-box :size "auto" :style {:color (str (theme-pull :theme/editor-font-color nil) 35)} ;; rows label under
     :children
     [[re-com/box :size "auto" :child [bricks/magic-table :searches-types-list* [:searches-types-sys] 
                                       (* single-width-bricks  0.25) ;3 
                                       bricks-tall  ;10
                                       []]] ;; last
      [re-com/box :size "auto" :child
       [bricks/magic-table :searches-sub-types-sys-list* [:searches-sub-types-sys] 
        (* single-width-bricks  0.75)  ;9 
        bricks-tall  ;10
        [:is_live]]]]]))

(defn search-panel-metadata-ext
  []
  (let [{:keys [single-width-bricks single-height-bricks single-width single-height bricks-wide bricks-tall]} @editor-dimensions
        sql-params         (into {}
                                 (for [k [:searches-types-sys/item_type :searches-rows-sys/value
                                          :searches-sub-types-sys/item_key]]
                                   {k ;;@(ut/tracked-subscribe [::conn/clicked-parameter-key
                                      @(ut/tracked-sub ::conn/clicked-parameter-key-alpha {:keypath [k]})}))
        selected-shortcode (get sql-params :searches-rows-sys/value)
        sql-calls          {:searches-rows-sys
                              {:select [:item_sub_type :item_type :item_key :display_name :sample :value :is_live :block_meta] ;; :searches-types-sys/item_type
                               :connection-id "autocomplete-db"
                               :where  [:and [:not [:like :item_sub_type "%preview%"]] [:not [:= :item_type "saved-block"]]
                                        [:= :item_key :searches-sub-types-sys/item_key]
                                        [:= :item_type :searches-types-sys/item_type]]
                               :from   [:client_items]}}]
    (dorun (for [[k v] sql-calls]
             (let [query        (ut/postwalk-replacer sql-params v)
                   ;data-exists? @(ut/tracked-subscribe [::conn/sql-data-exists? [k]])
                   ;unrun-sql?   @(ut/tracked-subscribe [::conn/sql-query-not-run? [k] query])
                   data-exists?   @(ut/tracked-sub ::conn/sql-data-exists-alpha? {:keypath [k]})
                   unrun-sql?     @(ut/tracked-sub ::conn/sql-query-not-run-alpha? {:keypath [k] :query query})]
               (when (or (not data-exists?) unrun-sql?) (conn/sql-data [k] query)))))
    [re-com/h-box :size "auto" :style {:color (str (theme-pull :theme/editor-font-color nil) 35)} :children
     [[re-com/v-box :size "auto" :children
       [[re-com/box :size "auto" :child
         [bricks/magic-table :searches-rows-sys-list* [:searches-rows-sys] 
          single-width-bricks ;;11 
          (if selected-shortcode (- single-height-bricks 1.5) single-height-bricks)
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

(defn editor-panel-viz2 []
  (let [{:keys [single-width-bricks vertical? single-height-bricks single-width single-height bricks-wide bricks-tall panel-count]} @editor-dimensions
        all-sql-call-keys @(ut/tracked-subscribe [::bricks/all-sql-call-keys])
        grid-reco? @(ut/tracked-subscribe [::bricks/grid-reco?])
        recos-page @(ut/tracked-subscribe [::bricks/recos-page])
        all-sql-call-keys-str (for [e all-sql-call-keys] (ut/replacer (ut/safe-name e) "-" "_"))
        sql-params (into {}
                         (for [k [:viz-tables-sys/table_name :viz-shapes0-sys/shape :viz-tables-sys/table_name
                                  :viz-shapes0-sys/shape :viz-shapes-sys/combo_edn :user-dropdown-sys/req-field :user-dropdown-sys/shape]]
                           {k ;;@(ut/tracked-subscribe [::conn/clicked-parameter-key [k]])
                            @(ut/tracked-sub ::conn/clicked-parameter-key-alpha {:keypath [k]})}))
        combo-picked? (not (nil? (get sql-params :viz-shapes-sys/combo_edn)))
        ;shape-picked? (not (nil? (get sql-params :viz-shapes0-sys/shape)))
        shape-picked? (not (nil? (get sql-params :user-dropdown-sys/shape)))
        react! [@editor-size]
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
                                                (when (not (nil? (get sql-params :user-dropdown-sys/shape)))
                                                  [:= :shape_name :user-dropdown-sys/shape])
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
                                                (when shape-picked? [:= :shape_name :user-dropdown-sys/shape])]}}

        shape-list @(ut/tracked-subscribe [::conn/sql-data [:viz-shapes0-sys]])
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
                                                 (theme-pull :theme/universal-pop-color "#9973e0")
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
        ;www (+ (if (= panel-count 3)
        ;         (* single-width 1)
        ;         (* single-width 2)) 300)
        www (if vertical?
              single-width
              (- (min (* 2 single-width) (* panel-count single-width)) 40))
        hhh (-
             (if vertical?
               (- (min (* 2 single-height)
                       (* panel-count single-height)) 40)
               single-height)
             120)
        w (Math/floor (/ www db/brick-size))
        h (Math/floor (/ hhh db/brick-size))
        charts-wide (js/Math.floor (/ w 7))
        charts-high (js/Math.floor (/ h 4.5))
        _ (ut/tapp>> [:ch w h charts-wide charts-high])
        h-multi 2.2 ;; offsets for css zoom math fuckery
        w-multi 2.3
        hh (js/Math.floor (* (/ (/ hhh charts-high) db/brick-size) h-multi))
        ww (js/Math.floor (* (/ (/ www charts-wide) db/brick-size) w-multi))
        ;;recos-per-page 9
        recos-per-page (* charts-wide charts-high)
        preview-container
        (fn [preview-maps pk pnum]
          (try
            (let [panel-key     (nth pk pnum)
                  ce            (get-in preview-maps [panel-key :combo_edn])
                  query_map     (get-in preview-maps [panel-key :query_map])
                  viz_map       (get-in preview-maps [panel-key :viz_map])
                    ;w             (get-in preview-maps [panel-key :w] 9)
                    ;h             (get-in preview-maps [panel-key :h] 9)
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
                                       (ut/tracked-dispatch [::conn/click-parameter [:recos-sys :condis] condis]))}
                                   ;:width  (px (* (/ single-width 3) 2.95)) ;;"581px" 
                                   ;:height (px (* (/ single-height 3) 1.55)) ;; "434px"
                                 :width (px (* ww db/brick-size))
                                 :height (px (* hh db/brick-size))
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
                                  [bricks/honeycomb panel-key (or selected-view :oz) ww hh]
                                  [re-com/box :size "auto" :height "40px" :justify :end :align :end :style
                                   {:padding-left "14px" :padding-right "14px" :font-weight 400 :font-size "22px"} :child
                                   (str shape_name)]]]]
              (if sel?
                (bricks/draggable (bricks/sql-spawner-meta :viz-reco)
                                  "meta-menu"
                                  [re-com/box :style
                                   {;:border (str "1px dashed " (theme-pull :theme/editor-outer-rim-color nil))
                                    :background-color (str (theme-pull :theme/editor-outer-rim-color nil) 22)
                                    :cursor           "grab"}
                                     ;:width "258px" 
                                     ;:height "191px" 

                                   :child body])
                [re-com/box :style {:cursor "grab"} :size "auto" :child
                 (bricks/draggable (bricks/sql-spawner-meta panel-key) "meta-menu" body)]))
            (catch :default _
              [re-com/box
               ;:width "240px"
               :size "auto"
               ;:height "400px"

                                                  
               :width (px (* ww db/brick-size))
               :height (px (* hh db/brick-size))

                 ;:width (px (* ww db/brick-size))
                 ;:height (px (* hh db/brick-size))
               :align :center :justify :center :style
               {:color     (str (theme-pull :theme/editor-font-color nil) 22) ; "#ffffff22"
                ;:border "1px solid white"
                :zoom      0.44
                :font-size "40px"} :child "n/a" ;(str e)
               ])))
        pages (/ recos-count recos-per-page)]
    (dorun (let [prev-preview-keys (for [k @(ut/tracked-subscribe [::bricks/preview-keys])] (keyword (str "reco-preview" k)))
                 per-page          recos-per-page ;;6 ;; if we ever get more room...
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
                   ;data-exists? @(ut/tracked-subscribe [::conn/sql-data-exists? [k]])
                   ;unrun-sql?   @(ut/tracked-subscribe [::conn/sql-query-not-run? [k] query])
                   data-exists?   @(ut/tracked-sub ::conn/sql-data-exists-alpha? {:keypath [k]})
                   unrun-sql?     @(ut/tracked-sub ::conn/sql-query-not-run-alpha? {:keypath [k] :query query})]
               (when (or (not data-exists?) unrun-sql?) (conn/sql-data [k] query)))))
    [re-com/v-box 
     :height (px hhh) ;;"433px" 
     :width (px www)
     ;:style {:border "1px solid pink"}
     :children
     [[re-com/h-box :height "39px" :gap "4px" :style {:overflow "auto" :padding-bottom "5px" :padding-left "7px"} :children
       block-list-boxes]
      [re-com/h-box :height "40px" :gap "12px" :align :center :justify :start :style
       {;:background-color "maroon"
        :color (theme-pull :theme/editor-font-color  nil)
        :padding-left "6px" :padding-right "6px"}
       :children [
                  [re-com/single-dropdown :choices (conj (for [{:keys [shape recos]} shape-list] {:id shape :label (str shape " (" recos ")")}) {:id nil :label "(all possible shapes)"})
                   :max-height "320px" :placeholder "(all possible shapes)" :style {:font-size "13px" }
                   :model (get sql-params :user-dropdown-sys/shape)
                   :on-change
                   #(ut/tracked-dispatch [::conn/click-parameter [:user-dropdown-sys :shape] %])
                           ;:width "285px"
                   ]
                  
                  
                  [re-com/single-dropdown :choices (conj (for [c combo-singles] {:id c :label c}) {:id nil :label "(all possible fields)"})
                   :max-height "320px" :placeholder "(all possible fields)" :style {:font-size "13px"} :model
                   (get sql-params :user-dropdown-sys/req-field) :on-change
                   #(ut/tracked-dispatch [::conn/click-parameter [:user-dropdown-sys :req-field] %])
         ;:width "285px"
                   ]
                  [re-com/single-dropdown :choices
                   (conj (for [{:keys [combo_edn recos]} combo-list] {:id combo_edn :label (str combo_edn " (" recos ")")})
                         {:id nil :label "(all possible field combos)"}) :max-height "320px" :placeholder "(all possible field combos)"
                   :style {:font-size "13px"} :model (get sql-params :viz-shapes-sys/combo_edn) :on-change
                   #(ut/tracked-dispatch [::conn/click-parameter [:viz-shapes-sys :combo_edn] %])
         ;:width "660px"
                   ]
                  [re-com/box
                   :child "grid"
                   :attr {:on-click #(ut/tracked-dispatch [::bricks/set-grid-reco? true])}
                   :style {:color       (if grid-reco? (theme-pull :theme/editor-font-color nil) 
                                            (str (theme-pull :theme/editor-font-color nil) 33))
                           :cursor      "pointer"
                           :margin-top  "-9px"
                           :font-weight 700}]
                  
                  [re-com/box
                   :child "previews"
                   :attr {:on-click #(ut/tracked-dispatch [::bricks/set-grid-reco? false])}
                   :style {:color       (if (not grid-reco?) (theme-pull :theme/editor-font-color nil) 
                                            (str (theme-pull :theme/editor-font-color nil) 33))
                           :cursor      "pointer"
                           :margin-top  "-9px"
                           :font-weight 700}]]]
      
      [(if vertical? re-com/v-box re-com/h-box) 
       :size "auto" 
       :style {:color (str (theme-pull :theme/editor-font-color nil) 35)} 
       :children
       [
        (when false ;; (and (not @hide-panel-2?) (not no-room-for-2?))
          [re-com/box
           :size "none"
         ;:width "300px" 
           :child [bricks/magic-table :viz-shapes0-list* [:viz-shapes0-sys] 
                   single-width-bricks  ;;7 
                   (- single-height-bricks 2.2) []]])
        
        (if grid-reco?
          [re-com/box 
           :size "none" 
           ;:width "880px" 
           :child [bricks/magic-table :recos-list* [:recos-sys] 
                   ;;(Math/floor (/ www db/brick-size)) ;;
                   (* 2 single-width-bricks) ;;(* 1.4 single-width-bricks) 
                   (- single-height-bricks 2.2)

            [;(when shape-picked? :shape_name)
             :query_map_str :table_name :context_hash :connection_id :h :w :condis :viz_map :combo_hash]]]
          
          (let [] ;; get the next 6 graphs and render?
            [re-com/h-box :children
             [
              ;; (let [charts-wide 3
              ;;       charts-hight 3]
                
              ;;   [re-com/v-box
              ;;    :size "auto"
              ;;    :style {:border "1px solid pink"}
              ;;    :height (px (-  single-height 120)) ;;"380px" 
              ;;    :width (px (* single-width 1.36)) ;;"770px" 
              ;;    :children
              ;;    [[re-com/h-box :size "auto" :children
              ;;      [[preview-container preview-maps preview-keys 0]
              ;;       [preview-container preview-maps preview-keys 1]
              ;;       [preview-container preview-maps preview-keys 2]]]
              ;;     [re-com/h-box :size "auto" :children
              ;;      [[preview-container preview-maps preview-keys 3]
              ;;       [preview-container preview-maps preview-keys 4]
              ;;       [preview-container preview-maps preview-keys 5]]]]])
                

                (let []
                  [re-com/v-box
                   :size "auto"
                   :style {:transform  "translate(0)"
                           ;:border     "1px solid pink"
                           }
                   :height (px hhh)
                   :width (px www)
                   :children
                   (for [h (range charts-high)]
                     [re-com/h-box :size "none" :children
                      (for [w (range charts-wide)] [bricks/reecatch [preview-container preview-maps preview-keys (+ (* h charts-wide) w)]])])])
                
                
                
              [re-com/v-box :size "auto" :height (px hhh) 
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
  (let [{:keys [single-width-bricks single-width single-height bricks-wide bricks-tall]} @editor-dimensions
        all-sql-call-keys     @(ut/tracked-subscribe [::bricks/all-sql-call-keys])
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
                   ;data-exists? @(ut/tracked-subscribe [::conn/sql-data-exists? [k]])
                   ;unrun-sql?   @(ut/tracked-subscribe [::conn/sql-query-not-run? [k] query])
                   data-exists?   @(ut/tracked-sub ::conn/sql-data-exists-alpha? {:keypath [k]})
                   unrun-sql?     @(ut/tracked-sub ::conn/sql-query-not-run-alpha? {:keypath [k] :query query})]
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
                   ;data-exists? @(ut/tracked-subscribe [::conn/sql-data-exists? [k]])
                   ;unrun-sql?   @(ut/tracked-subscribe [::conn/sql-query-not-run? [k] query])
                   data-exists?   @(ut/tracked-sub ::conn/sql-data-exists-alpha? {:keypath [k]})
                   unrun-sql?     @(ut/tracked-sub ::conn/sql-query-not-run-alpha? {:keypath [k] :query query})]
               (when (or (not data-exists?) unrun-sql?) (conn/sql-data [k] query)))))
    [re-com/box :size "auto" :child [bricks/magic-table :status-sys-list* [:status-sys] 11 20 [:ts :client_name]]]))

(defn editor-panel-metadata-files []  
  (let [{:keys [single-width-bricks single-width single-height bricks-wide bricks-tall]} @editor-dimensions
        sql-calls  {:files-sys {:select [:*] :order-by [[:screen_name :asc]] :from [:screens]}}
        sql-params (into {}
                         (for [k [:connections-sys/connection_id]]
                           {k ;;@(ut/tracked-subscribe [::conn/clicked-parameter-key [k]])
                              @(ut/tracked-sub ::conn/clicked-parameter-key-alpha {:keypath [k]})}))]
    (dorun (for [[k v] sql-calls]
             (let [query        (ut/postwalk-replacer sql-params v)
                   ;data-exists? @(ut/tracked-subscribe [::conn/sql-data-exists? [k]])
                   ;unrun-sql?   @(ut/tracked-subscribe [::conn/sql-query-not-run? [k] query])
                   data-exists?   @(ut/tracked-sub ::conn/sql-data-exists-alpha? {:keypath [k]})
                   unrun-sql?     @(ut/tracked-sub ::conn/sql-query-not-run-alpha? {:keypath [k] :query query})]
               (when (or (not data-exists?) unrun-sql?) (conn/sql-data [k] query)))))
    [re-com/h-box :size "auto" :style {:color (str (theme-pull :theme/editor-font-color nil) 35)} :children
     [;[re-com/box
      [re-com/box :size "auto" :child [bricks/magic-table :files-list* [:files-sys] 
                                       single-width-bricks ;11.5 
                                       bricks-tall ;10 
                                       [:file_path :ts]]]]]))

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

(defonce structured-editor? (reagent/atom {}))
(defonce param-scrubber? (reagent/atom {}))
(defonce param-search (reagent/atom {}))

(defn editor-panel-metadata-params [single-width single-height key-type]
  (let [;theme-scrubber? false key-type :param
        {:keys [single-width-bricks single-width single-height bricks-wide bricks-tall]} @editor-dimensions
        param-map @(ut/tracked-subscribe [::bricks/user-parameters key-type])]
    [re-com/v-box 
     :gap "2px" 
     ;:width (px single-width)
     ;:style {:border "1px solid white"}
     :children
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
        [re-com/box 
         :size "none" 
         :width (px single-width) 
         :height (px (- single-height 90)) 
         :child
         [bricks/scrubber-panel true ; view?
          @(ut/tracked-sub ::bricks/keypaths-in-params {:key-type key-type}) key-type (get @param-search key-type) {:fm true}] ; view-key
                                                                                                                        ; (update
                                                                                                                        ; kp)
         :style {:overflow "auto"}]
        [bricks/panel-param-box key-type nil (+ 17 single-width) (- single-height 66) param-map])
      [re-com/box
       :child (str "scrubber " (if (get @param-scrubber? key-type) "on" "off"))
       :attr {:on-click #(swap! param-scrubber? assoc key-type (not (get @param-scrubber? key-type false)))}
       :style {:padding-left "18px" 
               :color (if (get @param-scrubber? key-type) (theme-pull :theme/universal-pop-color nil) "inherit") 
               :cursor "pointer"}]
      
      ;; [re-com/box
      ;;  :child (str "struc-ed " (if (get @structured-editor? key-type) "on" "off"))
      ;;  :attr {:on-click #(swap! structured-editor? assoc key-type (not (get @structured-editor? key-type false)))}
      ;;  :style {:padding-left "18px" 
      ;;          :color (if (get @structured-editor? key-type) (theme-pull :theme/universal-pop-color nil) "inherit") 
      ;;          :cursor "pointer"}]
      
      ]]))

(defn editor-panel-metadata-ext-files []
  (let [{:keys [single-width-bricks single-width single-height bricks-wide bricks-tall]} @editor-dimensions
        sql-params (into {}
                         (for [k [:files-sys/file_path]]
                           {k ;;@(ut/tracked-subscribe [::conn/clicked-parameter-key [k]])
                              @(ut/tracked-sub ::conn/clicked-parameter-key-alpha {:keypath [k]})}))
        sql-calls  {:blocks-sys {:select   [:block_key :block_name :queries :views :block_data :view_names :query_names]
                                 :from     [:blocks]
                                 :where    [:= :file_path :files-sys/file_path]
                                 :order-by [:block_name]}}]
    (dorun (for [[k v] sql-calls]
             (let [query        (ut/postwalk-replacer sql-params v)
                   ;data-exists? @(ut/tracked-subscribe [::conn/sql-data-exists? [k]])
                   ;unrun-sql?   @(ut/tracked-subscribe [::conn/sql-query-not-run? [k] query])
                   data-exists?   @(ut/tracked-sub ::conn/sql-data-exists-alpha? {:keypath [k]})
                   unrun-sql?     @(ut/tracked-sub ::conn/sql-query-not-run-alpha? {:keypath [k] :query query})]
               (when (or (not data-exists?) unrun-sql?) (conn/sql-data [k] query)))))
    [re-com/h-box :size "auto" :style {:color (str (theme-pull :theme/editor-font-color nil) 35)} :children
     [;[re-com/box
      [re-com/v-box :size "auto" :children
       [[re-com/box :size "auto" :child
         [bricks/magic-table :blocks-list* [:blocks-sys] 
          single-width-bricks ;11 
          bricks-tall ;10
          [:screen_name :ts :block_data :view_names :query_names]]]]]]]))

(defn editor-panel-metadata-ext []
  (let [{:keys [single-width-bricks single-height-bricks single-width single-height bricks-wide bricks-tall]} @editor-dimensions
        sql-params (into {}
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
                   ;data-exists? @(ut/tracked-subscribe [::conn/sql-data-exists? [k]])
                   ;unrun-sql?   @(ut/tracked-subscribe [::conn/sql-query-not-run? [k] query])
                   data-exists?   @(ut/tracked-sub ::conn/sql-data-exists-alpha? {:keypath [k]})
                   unrun-sql?     @(ut/tracked-sub ::conn/sql-query-not-run-alpha? {:keypath [k] :query query})]
               (when (or (not data-exists?) unrun-sql?) (conn/sql-data [k] query)))))
    [re-com/h-box :size "auto" :style {:color (str (theme-pull :theme/editor-font-color nil) 35)} :children
     [;[re-com/box
      [re-com/v-box :size "auto" :children
       [[re-com/box :size "auto" :child
         [bricks/magic-table :system-fields-list*
          (cond grid?    [:fields-sys]
                attribs? [:attribs-sys]
                combos?  [:combos-sys]
                :else    [:fields-sys]) single-width-bricks single-height-bricks ;; 11 10
          [:db_type :db_schema :table_type :database_name :db_catalog :table_name :connection_id :database_version :key_hash
           :context_has]]]]]]]))





(defn editor-panel-metadata-viz []
  (let [{:keys [single-width-bricks single-height-bricks single-width single-height bricks-wide bricks-tall]} @editor-dimensions
        sql-params    (into {}
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
                   ;data-exists? @(ut/tracked-subscribe [::conn/sql-data-exists? [k]])
                   ;unrun-sql?   @(ut/tracked-subscribe [::conn/sql-query-not-run? [k] query])
                   data-exists?   @(ut/tracked-sub ::conn/sql-data-exists-alpha? {:keypath [k]})
                   unrun-sql?     @(ut/tracked-sub ::conn/sql-query-not-run-alpha? {:keypath [k] :query query})]
               (when (or (not data-exists?) unrun-sql?) (conn/sql-data [k] query)))))
    [re-com/h-box :size "auto" :style {:color (theme-pull :theme/editor-font-color nil)} :children
     [[re-com/v-box :size "auto" :children
       [[re-com/box :size "auto" :child
         [bricks/magic-table :recos-list* [:recos-sys] single-width-bricks single-height-bricks ;; 11 10
          [(when shape-picked? :shape_name) :query_map_str :table_name :context_hash :connection_id :h :w :condis :viz_map
           (when combo-picked? :combo_edn)]]]]]]]))

(defn editor-panel-metadata-status []
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






(defn ui-debugger []
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



(defn block-layer-rename [data-key-type t]
  (if (= @db/view-title-edit-idx t)
  (let [selected-block      @(ut/tracked-subscribe [::bricks/selected-block])
        block-layer-regex   #"^[a-zA-Z0-9_-]+$"]
    [re-com/h-box
     :style {:margin-top "-9px"}
     :children [[re-com/input-text
                 :model (cstr/replace (str t) ":" "")
                 :validation-regex block-layer-regex
                 :on-change #(do (ut/tapp>> [:changed-block-layer-name (str t) :to (str %)])
                                 (when (and (ut/ne? (cstr/trim (str %)))
                                            (string? %) 
                                            (boolean (not (re-matches #"\d" (subs (str %) 0 1))))
                                            (not= (str %) (cstr/replace (str t) ":" ""))
                                            ;(not (some (fn [x] (= x %)) block-layers))
                                            )
                                   (ut/tracked-dispatch [::bricks/rename-block-layer selected-block (str t) (str %)]))
                                 (reset! db/view-title-edit-idx nil))
                 :change-on-blur? true
                 :width (px (+ 15 (* (count (str t)) 7)))
                 :style {:background-color "#00000000"
                         :text-decoration  "underline"
                         :border           "none"
                         :padding-left     "0px"
                         :color            (theme-pull :theme/editor-outer-rim-color nil)}]
                [re-com/md-icon-button :src (at) :md-icon-name "zmdi-delete" :tooltip
                 "delete this tab"
                 :style {:color       (theme-pull :theme/editor-outer-rim-color nil)
                         :font-size   "14px"
                         :padding-top "5px"
                         :width       "10px"
                         :height      "20px"}
                 :attr {:on-click #(ut/tracked-dispatch [::bricks/delete-block-layer selected-block data-key-type t])}]]])
  (str t)))

(defn block-layer-tabs [selected-block data-key data-key-type mad-libs-combos runners sql-calls block-runners-map views views? runners? queries?]
  (let [react! [@db/data-browser-query-con 
                @db/data-browser-query]]
    [re-com/h-box
   ;:width "900px"
   ;:style {:border "1px solid white" 
   ;        ;:z-index 99999
   ;        }
     :justify :between
     :children [[re-com/h-box
                 :children [(when mad-libs-combos
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
                                                         ;"inherit"
                                                         (str (ut/invert-hex-color (theme-pull :theme/universal-pop-color "#9973e0")) 55)
                                                         )
                                     :padding-right    "5px"
                                     :padding-left     "5px"} :gap "4px" :children
                                    [[re-com/box :padding "3px" :attr
                                      {:on-click #(do (swap! db/data-browser-query assoc selected-block k))} :style
                                      {:cursor "pointer" :font-size "13px" :padding-left "5px" :padding-right "5px"} :child (str k)]]]))])

                            [re-com/h-box
                             :gap "7px"
                             :padding "5px"
                             :style {:font-weight 700 :color "#dddddd"}
                             :children [[re-com/h-box :style
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

                            (when (or runners? queries?)
                              [re-com/h-box :gap "7px" :padding "5px" :style {:font-weight 700 :color "#dddddd"} :children
                               (let [runners-kps (vec (apply concat (for [[k v] runners] (for [vv (keys v)] [k vv]))))]
                                 (for [k (into runners-kps (keys sql-calls))]
                                   (let [runner?        (some #(= % k) runners-kps)
                                         k              (if runner? (last k) k)

                                         console-output (if runner?
                                                          (let [are-solver        (get @db/solver-fn-lookup [:panels selected-block data-key])
                                                      ;; meta-data          (when are-solver
                                                      ;;                      @(ut/tracked-sub ::conn/clicked-parameter-key-alpha
                                                      ;;                                       {:keypath [(keyword (str (ut/replacer are-solver
                                                      ;;                                                                             ":solver/" "solver-meta/")))]}))
                                                                rdata             (when are-solver
                                                                                    @(ut/tracked-sub ::conn/clicked-parameter-key-alpha
                                                                                                     {:keypath [(keyword (str (ut/replacer are-solver
                                                                                                                                           ":" "")))]}))
                                                      ;; running-status    (when are-solver
                                                      ;;                     @(ut/tracked-sub ::conn/clicked-parameter-key-alpha
                                                      ;;                                      {:keypath [(keyword (str (ut/replacer are-solver
                                                      ;;                                                                            ":solver/" "solver-status/*client-name*>")))]}))
                                                                ]rdata)
                                                          @(ut/tracked-subscribe [::bricks/repl-output k]))
                              ;; _ (ut/tapp>>  [:console-output console-output])
                                         repl?          (or runner? (ut/ne? console-output))
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
                                      [[re-com/box
                                        :padding "3px"
                                        :height "24px"
                                        :attr (merge
                                               {:on-click #(do (swap! db/data-browser-query assoc selected-block k)
                                                               (when (and repl? (= k data-key))

                                                                 (swap! db/data-browser-query-con assoc
                                                                        k
                                                                        false
                                                              ;; (let [b (get @db/data-browser-query-con k)] 
                                                              ;;   (if 
                                                              ;;   (= (get @db/view-title-edit-idx (str k)) (str k)) false  
                                                              ;;   (if (nil? b) true (not b))))
                                                              ;true
                                                                        )))}
                                               (when (and
                                                      (= k data-key)
                                                      (not= (get @db/view-title-edit-idx (str k)) (str k)))
                                                 {:on-double-click #(reset! db/view-title-edit-idx (str k))}))
                                        :style {:cursor        "pointer"
                                                :font-size     "13px"
                                                :border-bottom (when repl?
                                                                 (if (not (get @db/data-browser-query-con k))
                                                                   (str "4px solid " dyn-color)
                                                                   "inherit"))
                                                :padding-left  "5px"
                                                :padding-right "5px"}
                                        :child [block-layer-rename data-key-type (str k)]]

                                       (when repl?
                                         [re-com/md-icon-button
                                          :src (at)
                                          :md-icon-name "zmdi-code"
                                          :on-click #(do (swap! db/data-browser-query assoc selected-block k)
                                                         (swap! db/data-browser-query-con assoc
                                                                k
                                                                (let [b (get @db/data-browser-query-con k)] (if (nil? b) true (not b))))) :style
                                          {:color         dyn-color
                                           :border-bottom (if (get @db/data-browser-query-con k) (str "4px solid " dyn-color) "inherit")
                                           :font-size     "15px"}])]])))])

                            (when views?
                              [re-com/h-box :gap "7px" :padding "5px" :style {:font-weight 700 :color "#dddddd"}
                               :children
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
                                    [[re-com/box
                                      :padding "3px"
                                      :attr (merge
                                             {:on-click #(do (swap! db/data-browser-query assoc selected-block k))}
                                             (when (and
                                                    (= k data-key)
                                                    (not= (get @db/view-title-edit-idx (str k)) (str k)))
                                               {:on-double-click #(reset! db/view-title-edit-idx (str k))}))
                                      :style {:cursor "pointer" :font-size "13px" :padding-left "5px" :padding-right "5px"}
                                      :child [block-layer-rename data-key-type (str k)]
                              ;(str "🍀" " " k)
                                      ]]]))])

                    ;; (when runners?
                    ;;   [re-com/h-box :gap "7px" :padding "5px" :style {:font-weight 700 :color "#dddddd"} :children
                    ;;    (let [runners-kps (vec (apply concat (for [[k v] runners] (for [vv (keys v)] [k vv]))))]
                    ;;      (for [k runners-kps]
                    ;;        (let [runner-icon (get-in runners (vec (conj k :icon)) "") ;; todo? as long as they match the theme..
                    ;;              runner-color (get-in runners (vec (conj k :icon)) "cyan") ]
                    ;;          [re-com/h-box :style
                    ;;           {:margin-left      "-5px"
                    ;;            :border-top       "1px solid #80008045"
                    ;;            :border-left      "1px solid #80008080"
                    ;;            :color            (if (= (last k) data-key)
                    ;;                                (theme-pull :theme/editor-background-color nil) ;;"#000000"
                    ;;                                (theme-pull :theme/editor-font-color nil))
                    ;;            :background-color (if (= (last k) data-key) 
                    ;;                                ;"#9973e0" 
                    ;;                                (theme-pull :theme/universal-pop-color "#9973e0")
                    ;;                                "inherit")
                    ;;            :padding-right    "5px"
                    ;;            :padding-left     "5px"} :gap "4px" :children
                    ;;           [[re-com/box :padding "3px" :attr
                    ;;             {:on-click #(do (swap! db/data-browser-query assoc selected-block (last k)))} :style
                    ;;             {:cursor "pointer" :font-size "13px" :padding-left "5px" :padding-right "5px"} :child
                    ;;             (str (last k))]]])))])
                            ]]

                (let [combo-items {:view {:base-key :views
                                          :default [:box :child "heya" :align :center :justify :center :size "auto" :style
                                                    {:font-size "22px"}]}
                                   :query {:base-key :queries
                                           :default {:select [["heya, friendo" :greetings] [(rand-int 123) :count_stuff]]}}
                                  ;;  :code-query {:base-key :queries
                                  ;;               :default {:select [:*]
                                  ;;                         :from
                                  ;;                         [{:data '(vec
                                  ;;                                   (for
                                  ;;                                    [i (range 45)]
                                  ;;                                     {:row_id i :type (rand-nth ["cat" "dog" "pizza"]) :name (str "row " i)}))}]}}
                                   }
                      runners-items (into {} (for [[k v] (-> block-runners-map (dissoc :views) (dissoc :queries))
                                                   :when (get v :slice-bar? true)]
                                               {k {:base-key k  :default (get v :default)}}))
                        ;;_ (ut/tapp>> [:runners-items  runners runners-items combo-items])
                      combo-items (merge combo-items runners-items)]

                  [re-com/single-dropdown
                   :choices (vec (for [c (keys combo-items)] {:id c :label (str c)}))
                   :max-height "320px"
                   :placeholder "add new slice"
                   :style {:font-size "13px"
                             ;:border "1px solid grey"
                           }
                   :model nil ;(get sql-params :user-dropdown-sys/req-field) 
                   :on-change #(ut/tracked-dispatch [::bricks/add-new selected-block (get-in combo-items [% :base-key])  (get-in combo-items [% :default])])
                   :width "125px"])

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
                ]]))

(defn get-client-rect [evt]
  (let [r (.getBoundingClientRect (.-target evt))]
    {:left (.-left r), :top (.-top r)
     :width (.-width r) :height (.-height r)
     :bottom (.-bottom r) :right (.-right r)}))

;; (defn mouse-up-handler [on-move]
;;   (fn me [evt]
;;     (gevents/unlisten js/window EventType.MOUSEMOVE on-move)))

(defn resize-mouse-move-handler [offset]
  (fn [evt]
    (let [start-x (.-clientX evt)
          start-y (.-clientY evt)
          off-x   (:x offset)
          off-y   (:y offset)
          x       (- start-x off-x)
          y       (- start-y off-y)
          ;xmax    (if (< x 650) 650 x)
          ;ymax    (if (< y 750) 750 y)
          ;xmax    (+ (Math/ceil (/ x db/brick-size)) (get @editor-size 0))
          ;ymax    (+ (Math/ceil (/ y db/brick-size)) (get @editor-size 1))
          xmax     (Math/floor (/ x db/brick-size))
          ymax     (Math/floor (/ y db/brick-size))
          xmax    (+ (if (< xmax 8) 8 xmax) 0.3)
          ymax    (if (< ymax 8) 8 ymax)]
      ;(when @bricks/dragging-editor? )
      
      ;(ut/tapp>> (str [:editor-size xmax ymax]))
      (reset! editor-size [xmax ymax]))))

(defn resize-mouse-down-handler [e]
  (let [{:keys [left top]} (get-client-rect e)
        width               (* (get @editor-size 0) db/brick-size)
        height              (* (get @editor-size 1) db/brick-size)
        offset             {:x (+ -10 (- (.-clientX e) width))
                            :y (+ -30 (- (.-clientY e) height))}

        on-move            (resize-mouse-move-handler offset)]
    (.preventDefault e) ;; to stop text selection from dragging
    
    (do
      (reset! bricks/dragging-editor? true)
      ;; (when @bricks/dragging-editor? (.addEventListener js/document "mousedown" prevent-selection))
      (gevents/listen js/window EventType.MOUSEMOVE on-move))
    (gevents/listen js/window EventType.MOUSEUP
                    (mouse-up-handler on-move))))

(defonce hide-panel-2? (reagent/atom false))
(defonce hide-panel-3? (reagent/atom false))

;; (defn docker-edges [bricks-wide bricks-tall]
;;   (let [cc (theme-pull :theme/editor-outer-rim-color nil)
;;         ccc (count cc)
;;         cc (str (if (> ccc 7) (subs cc 0 7) cc) 45)
;;         [x y] @detached-coords
;;         xb (Math/floor (/ x db/brick-size))
;;         yb (Math/floor (/ y db/brick-size))
;;         near? (<= xb 2)
;;         inside? (and (= xb 0) (or (>= yb 1) (<= yb (- bricks-tall 2))))]
;;     (when near?
;;       [re-com/h-box
;;        :children [[re-com/box
;;                    :align :center :justify :center
;;                    :child " " ;;(str xb " " yb)
;;                    :style {:background-color (if inside?  "#ffffff88" "#00000085")
;;                            :color "#ffffff"
;;                            :backdrop-filter "blur(4px)"
;;                            :border           (str "6px solid " (theme-pull :theme/editor-outer-rim-color nil))
;;                            :position "fixed"
;;                            :box-shadow     "0px -5px 5px 0px #00000099"
;;                            :z-index 9999999
;;                            :background-image (str "linear-gradient(0deg, " cc " 2px, transparent 8px), linear-gradient(90deg, " cc " 2px, transparent 8px)")
;;                            :background-size  (str "50px 50px, 50px 50px")
;;                            :left 0
;;                            :border-radius "0px 12px 12px 0px"
;;                            :height (* (- bricks-tall 2) db/brick-size)
;;                            :width db/brick-size
;;                            :top db/brick-size}]]])))



;; (defn edge-properties [edge bricks-wide bricks-tall]
;;   (case edge
;;     :left   {:width db/brick-size
;;              :height (* (- bricks-tall 2) db/brick-size)
;;              :top db/brick-size
;;              :left 0
;;              :border-radius "0px 12px 12px 0px"
;;              :near? #(<= (first %) 2)
;;              :inside? #(and (= (first %) 0) (or (>= (second %) 1) (<= (second %) (- bricks-tall 2))))}
;;     :right  {:width db/brick-size
;;              :height (* (- bricks-tall 2) db/brick-size)
;;              :top db/brick-size
;;              :right 0
;;              :border-radius "12px 0px 0px 12px"
;;              :near? #(>= (first %) (- bricks-wide 3))
;;              :inside? #(and (= (first %) (dec bricks-wide)) (or (>= (second %) 1) (<= (second %) (- bricks-tall 2))))}
;;     :top    {:width (* (- bricks-wide 2) db/brick-size)
;;              :height db/brick-size
;;              :top 0
;;              :left db/brick-size
;;              :border-radius "0px 0px 12px 12px"
;;              :near? #(<= (second %) 2)
;;              :inside? #(and (= (second %) 0) (or (>= (first %) 1) (<= (first %) (- bricks-wide 2))))}
;;     :bottom {:width (* (- bricks-wide 2) db/brick-size)
;;              :height db/brick-size
;;              :bottom 0
;;              :left db/brick-size
;;              :border-radius "12px 12px 0px 0px"
;;              :near? #(>= (second %) (- bricks-tall 3))
;;              :inside? #(and (= (second %) (dec bricks-tall)) (or (>= (first %) 1) (<= (first %) (- bricks-wide 2))))}))

;; (defn docker-edge [edge bricks-wide bricks-tall]
;;   (let [cc (theme-pull :theme/editor-outer-rim-color nil)
;;         ccc (count cc)
;;         ccx (if (> ccc 7) (subs cc 0 7) cc)
;;         cc (str ccx 45)
;;         [x y] @detached-coords
;;         xb (Math/floor (/ x db/brick-size))
;;         yb (Math/floor (/ y db/brick-size))
;;         props (edge-properties edge bricks-wide bricks-tall)
;;         near? ((:near? props) [xb yb])
;;         inside? ((:inside? props) [xb yb])]
;;     (when near?
;;       [re-com/box
;;        :align :center :justify :center
;;        :child " "
;;        :style (merge
;;                {:background-color (if inside? (str ccx 88) "#00000085")
;;                 :color "#ffffff"
;;                 :backdrop-filter "blur(4px)"
;;                 :border (str "6px solid " (theme-pull :theme/editor-outer-rim-color nil))
;;                 :position "fixed"
;;                 :box-shadow "0px -5px 5px 0px #00000099"
;;                 :z-index 9999999
;;                 :background-image (str "linear-gradient(0deg, " cc " 2px, transparent 8px), linear-gradient(90deg, " cc " 2px, transparent 8px)")
;;                 :background-size (str "50px 50px, 50px 50px")
;;                 :transition "all 0.3s ease-in-out"}
;;                (select-keys props [:width :height :top :bottom :left :right :border-radius]))])))

;;   (defn docker-edges [bricks-wide bricks-tall]
;;     [re-com/h-box
;;      :children [[docker-edge :left bricks-wide bricks-tall]
;;                 [docker-edge :right bricks-wide bricks-tall]
;;                 [docker-edge :top bricks-wide bricks-tall]
;;                 [docker-edge :bottom bricks-wide bricks-tall]]])





(defn edge-properties [edge bricks-wide bricks-tall]
  (case edge
    :left   {:width db/brick-size
             :height (* (- bricks-tall 2) db/brick-size)
             :top db/brick-size
             :left 0
             :border-radius "0px 12px 12px 0px"
             :near? #(<= (first %) 2)
             :inside? #(and (= (first %) 0) (or (>= (second %) 1) (<= (second %) (- bricks-tall 2))))}
    :right  {:width db/brick-size
             :height (* (- bricks-tall 2) db/brick-size)
             :top db/brick-size
             :right 0
             :border-radius "12px 0px 0px 12px"
             :near? #(>= (first %) (- bricks-wide 3))
             :inside? #(and (= (first %) (dec bricks-wide)) (or (>= (second %) 1) (<= (second %) (- bricks-tall 2))))}
    :top    {:width (* (- bricks-wide 2) db/brick-size)
             :height db/brick-size
             :top 0
             :left db/brick-size
             :border-radius "0px 0px 12px 12px"
             :near? #(<= (second %) 2)
             :inside? #(and (= (second %) 0) (or (>= (first %) 1) (<= (first %) (- bricks-wide 2))))}
    :bottom {:width (* (- bricks-wide 2) db/brick-size)
             :height db/brick-size
             :bottom 0
             :left db/brick-size
             :border-radius "12px 12px 0px 0px"
             :near? #(>= (second %) (- bricks-tall 3))
             :inside? #(and (= (second %) (dec bricks-tall)) (or (>= (first %) 1) (<= (first %) (- bricks-wide 2))))}))

(defn docker-edge [edge bricks-wide bricks-tall]
  (let [cc (theme-pull :theme/editor-outer-rim-color nil)
        ccc (count cc)
        ccx (if (> ccc 7) (subs cc 0 7) cc)
        cc (str ccx 45)
        [x y] @detached-coords
        xb (Math/floor (/ x db/brick-size))
        yb (Math/floor (/ y db/brick-size))
        props (edge-properties edge bricks-wide bricks-tall)
        near? ((:near? props) [xb yb])
        inside? ((:inside? props) [xb yb])]
    ;; Update the active-edge atom based on inside? status
    (when inside?
      (reset! active-edge edge))
    (when (and (not inside?) (= @active-edge edge))
      (reset! active-edge nil))
    (when near?
      [re-com/box
       :align :center :justify :center
       :child " "
       :style (merge
               {:background-color (if inside? (str ccx 88) "#00000085")
                :color "#ffffff"
                :backdrop-filter "blur(4px)"
                :border (str "6px solid " (theme-pull :theme/editor-outer-rim-color nil))
                :position "fixed"
                :box-shadow "0px -5px 5px 0px #00000099"
                :z-index 9999999
                :background-image (str "linear-gradient(0deg, " cc " 2px, transparent 8px), linear-gradient(90deg, " cc " 2px, transparent 8px)")
                :background-size (str "50px 50px, 50px 50px")
                :transition "all 0.3s ease-in-out"}
               (select-keys props [:width :height :top :bottom :left :right :border-radius]))])))

(defn docker-edges [bricks-wide bricks-tall]
  [re-com/h-box
   :children [[docker-edge :left bricks-wide bricks-tall]
              [docker-edge :right bricks-wide bricks-tall]
              [docker-edge :top bricks-wide bricks-tall]
              [docker-edge :bottom bricks-wide bricks-tall]]])

;; (defn console-text-box
;;   [width-int height-int value]
;;   [re-com/box :size "auto" :width (px (- width-int 24)) :max-height (px (- height-int 24)) :style
;;    {;:background-color "#00000085"
;;     :font-family   (theme-pull :theme/monospaced-font nil) ; "Fira Code" ;"Chivo Mono"
;;     :margin-left   "9px"
;;     :font-size     "20px"
;;     :overflow      "hidden"
;;     :border-radius "12px"
;;     :font-weight   400} :child
;;    [(reagent/adapt-react-class cm/UnControlled)
;;     {:value   value
;;      :options {:mode              "clojure"
;;                :lineWrapping      true ;false
;;                :lineNumbers       false
;;                :matchBrackets     true
;;                :autoCloseBrackets true
;;                :autofocus         false
;;                :autoScroll        false
;;                :detach            true
;;                :readOnly          false
;;                :theme             (theme-pull :theme/codemirror-theme nil)}}]])

;(def modes ["🍀" "🌞" "🌙" "🔥" [:img {:src "images/rabbit-console.png" :width "40px" :height "40px"}]])

(defn find-ns-declarations
  [data]
  (letfn [(extract-ns-from-form [form]
            (when (and (seq? form)
                       (>= (count form) 2)
                       (= 'ns (first form)))
              (str (second form))))

          (extract-ns-from-string [s]
            (let [matches (re-seq #"\(ns\s+([^\s\)]+)" s)]
              (map second matches)))

          (traverse [item]
            (cond
              (seq? item) (or (some-> item extract-ns-from-form vector)
                              (mapcat traverse item))
              (string? item) (extract-ns-from-string item)
              (vector? item) (mapcat traverse item)
              (map? item) (mapcat traverse (vals item))
              :else nil))]
    (vec (distinct (remove nil? (traverse data))))))

(re-frame/reg-sub
 ::local-namespaces-for
 (fn [db {:keys [runner-key]}]
   (let [;panels-map (get db :panels)
         ;selected-tab (get db :selected-tab)
         ;panel-keys (set (keys (bricks/only-relevant-tabs panels-map selected-tab)))
         ;;_ (ut/tapp>> [:panel-keys runner-key])
         panel-keys @(ut/tracked-sub ::bricks/relevant-tab-panels-set {})
         panels-map (select-keys (get db :panels) panel-keys)]
     (vec (distinct (find-ns-declarations (for [[_ v] panels-map] (vals (get v runner-key)))))))))

;; (ut/tapp>> [:ns @(ut/tracked-sub ::local-namespaces-for {:runner-key :clojure})]) 

(defonce base-hop-bar-runner (merge (select-keys ;; set default to clover ("views")
                                 (get @(ut/tracked-sub ::bricks/hop-bar-runners {}) :views)
                                 [:icon :description :type :syntax]) 
                                    {:name :clover
                                     :description "Evaluate a Clover expression - the main DSL of all Rabbit view blocks. Can also mutate data via special shortcodes."
                                     :icon "🍀"}))

(defonce selected-mode (reagent/atom base-hop-bar-runner))

(def hop-bar-tooltip (reagent/atom nil))
(defonce cm-instance (reagent/atom nil))
(defonce hide-responses? (reagent/atom false))
(def console-responses (reagent/atom {})) 
(def console-history (reagent/atom #{})) 
(defonce ns-selected (reagent/atom {}))
(defonce fbc-selected (reagent/atom {}))
(defonce fbc-selected-model (reagent/atom {}))
(defonce fbc-selected-pattern (reagent/atom {}))
(def over-hop-bar (reagent/atom false))
(def searcher-atom (reagent/atom nil))

(defn insert-response-block [w h data runner syntax opts-map no-selected?]
  (let [root (or (ut/find-safe-position h w) [5 5])]
    (bricks/insert-new-block-raw root w h data runner syntax opts-map no-selected?)))

(defn ensure-quoted-string
  "Adds double quotes to a string if they don't already exist.
   If the string already has double quotes, it's returned as-is."
  [s]
  (let [trimmed (cstr/trim s)]
    (if (and (cstr/starts-with? trimmed "\"")
             (cstr/ends-with? trimmed "\""))
      trimmed
      (str "\"" trimmed "\""))))

(defn run-console-command [command]
  (let [runner (get @selected-mode :name)
        syntax (get @selected-mode :syntax)
        ttype  (get @selected-mode :type)
        client-name @(ut/tracked-sub ::bricks/client-name {})
        [runner-src data-key] @(ut/tracked-sub ::bricks/editor-panel-selected-view {})
        selected-block    @(ut/tracked-sub ::bricks/selected-block {})
        no-selected?      (or (nil? selected-block) (= selected-block "none!"))
        ;;context-body      (pr-str @(ut/tracked-sub ::bricks/get-view-data {:panel-key selected-block :runner runner-src :data-key data-key}))
        context-body      (pr-str @(ut/tracked-sub ::bricks/get-block-data {:panel-key selected-block}))
        ;nss @(ut/tracked-sub ::local-namespaces-for {:runner-key (get @selected-mode :name)})
        ;; is-rf-event? (and (or (= runner :views) (= runner :clover))
        ;;                   (cstr/starts-with? (cstr/trim command) "[:dispatch"))
        ns-pushed (when (and
                         (cstr/includes? (str command) "(ns ")
                         (= ttype :nrepl)) (find-ns-declarations [command]))
        _ (when ns-pushed (swap! ns-selected assoc runner (first ns-pushed))) ;; set ns in UI 
        command (if (and (= ttype :nrepl)
                         (not (cstr/includes? (str command) "(ns "))
                         (ut/ne? (get @ns-selected runner)))
                  (str "(ns " (get @ns-selected runner) ") " command)
                  command)
        command (if (not= syntax "clojure")
                  command ;; the "implied string"
                  (ensure-quoted-string (cstr/replace command "\"" "\\\"")))
        opts-map (if (= runner :fabric)
                   (let [curr-tab @(ut/tracked-sub ::bricks/selected-tab {})
                         coords @(ut/tracked-sub ::bricks/all-roots-tab-sizes {:tab curr-tab})
                         hh @(ut/tracked-subscribe_ [::subs/h])
                         ww @(ut/tracked-subscribe_ [::subs/w])
                         mdata @(ut/tracked-sub ::conn/sql-metadata-alpha {:keypath [data-key]})
                         canvas-w (/ ww db/brick-size)
                         canvas-h (/ hh db/brick-size)]
                     {:model (get @fbc-selected-model runner)
                      :client-name client-name
                    ;:runner (if no-selected? :clojure (or runner-src :views))
                      :metadata (if (= runner-src :queries)
                                  {:connection-id @(ut/tracked-subscribe [::bricks/lookup-connection-id-by-query-key data-key])
                                   :database-type (get mdata :database-type "")
                                   :fields (get mdata :fields)} {})
                      :canvas-coords coords
                      :canvas-size [canvas-w canvas-h]
                      :input (str command)
                    ;:id          (cstr/replace (str client-name "++" selected-block "++" runner "++" data-key) ":" "")
                      :id       (cstr/replace (str client-name "++" selected-block "++" runner-src "++" data-key) ":" "")
                    ;;:context {[:panels selected-block runner-src data-key] context-body}
                      :context {[:panels selected-block] context-body}
                      :pattern (if (not no-selected?) "clover-canvas"
                                   (get @fbc-selected-pattern runner "tweet"))}) 
                   {})]
    
    (ut/tapp>> (str "Command entered: " command " " ttype " " runner " " (get @ns-selected runner)))
    (reset! hide-responses? false)

    (if (not (and (not no-selected?) 
                  (= runner :fabric)))
      
      (let [resp (insert-response-block 8 5 
                                        (if (= ttype :nrepl)
                                          (try (edn/read-string command) (catch :default _ command))
                                          command)
                                        runner syntax opts-map no-selected?)
            ee (str command "(" resp ")")
            _ (ut/tapp>> (str "insert-response-block: " command " " resp))]

          ;;(when (= resp :success) (re-frame/dispatch [::bricks/toggle-quake-console]))
          ;(re-frame/dispatch [::bricks/toggle-quake-console])
        ;(ut/tracked-dispatch [::bricks/toggle-quake-console-off])
        (ut/dispatch-delay 200 [::http/insert-alert
                                [:v-box :children [;[:box :child (str resp)]
                                                   [:box :child (str "running " command)
                                                    :style {:font-size "12px"}]]] 6 1.5 5])
        (swap! console-responses assoc command ee))
      
      (let [kit-runner-key (str "kit-runner" (hash (str client-name selected-block data-key :clojure :gen-ev)))
            kit-keypath [:clojure :gen-ev]
            running-key  (keyword (str "kit-status/" kit-runner-key ">running?"))
            output-key   (keyword (str "kit/" kit-runner-key ">incremental"))
            curr-tab       @(ut/tracked-sub ::bricks/selected-tab {})
            ;;running?     @(ut/tracked-sub ::conn/clicked-parameter-key-alpha {:keypath [running-key]})
            fstr (str "kit-runner " kit-runner-key)
            fstr2 [:v-box :children [[:box :child (str "You asked: \"" command "\" to " (get opts-map :model))]
                                     [:box :child (str "under the context of " selected-block " on " data-key)]]]
            w    (/ (count fstr) 4.1)]
        (swap! db/kit-run-ids assoc (keyword kit-runner-key) (ut/generate-uuid))
        (swap! bricks/waiting? assoc kit-runner-key true)
        (swap! bricks/temp-extra-subs conj running-key)
        (swap! bricks/temp-extra-subs conj output-key)
        (swap! db/kit-fn-lookup assoc [selected-block data-key] running-key)
        (ut/tracked-dispatch
         [::wfx/push   :default
          {:kind       :run-kit
           :kit-keypath kit-keypath
           :kit-runner-key kit-runner-key
           :panel-key   selected-block
           :data-key    data-key
           :tab-name    curr-tab
           :opts-map    opts-map
           :runner      :clojure
           :client-name client-name
           :ui-keypath  [:panels selected-block :queries data-key]}])
        (ut/tracked-dispatch [::http/insert-alert fstr2 w 2 15])
        (ut/dispatch-delay 800 [::http/insert-alert fstr w 1 5])
        (ut/tapp>> [:clicked-kit running-key])
        (js/setTimeout #(swap! bricks/waiting? assoc kit-runner-key false) 5000))
      )))



(defn console-text-box [width-int height-int value]
  (let [history-index (reagent/atom -1)
        ;nss @(ut/tracked-sub ::local-namespaces-for {:runner-key (get @selected-mode :name)})
        react! [@hop-bar-tooltip @db/console-voice-text]]
    (fn [width-int height-int value]
      [re-com/box
       :size "auto"
       :width (px (- width-int 24))
       :height (px height-int)
       :style
       {:font-family   (theme-pull :theme/monospaced-font nil)
        :margin-left   "9px"
        :font-size     "20px"
        :overflow      "hidden"
        :border-radius "12px"
        :font-weight   700}
       :child
       [(reagent/adapt-react-class cm/UnControlled)
        {:value   (or @db/console-voice-text value " ")
         :onBeforeChange (fn [editor _ _] ;; data value]
                           (reset! cm-instance editor))
         :onBlur #(when (and (not @over-hop-bar) 
                             (nil? @hop-bar-tooltip)) 
                    (re-frame/dispatch [::bricks/toggle-quake-console])) ;; when not hovered over? to detect a click off into the canvas?
         :options {:mode              (get @selected-mode :syntax "clojure") 
                   :lineWrapping      false
                   :lineNumbers       false
                   :matchBrackets     true
                   :autoCloseBrackets true
                   :autofocus         true
                   :autoScroll        false
                   :theme             (theme-pull :theme/codemirror-theme nil)
                   :extraKeys         (clj->js
                                       {;;"`" (fn [_] (re-frame/dispatch [::bricks/toggle-quake-console-off]))
                                        "Enter" (fn [cm]
                                                  (let [command (cstr/join "\n" (ut/cm-deep-values cm))]
                                                    (when (not-empty command)
                                                      (swap! console-history conj command)
                                                      (reset! history-index -1)
                                                      ;(ut/tapp>> (str "Command entered: " command))
                                                      (run-console-command command)
                                                      (.setValue cm ""))))
                                        "Down" (fn [cm]
                                                 (when (seq @console-history)
                                                   (let [new-index (if (>= @history-index (dec (count @console-history)))
                                                                     0
                                                                     (inc @history-index))]
                                                     (reset! history-index new-index)
                                                     (.setValue cm (nth (vec @console-history) new-index)))))
                                        "Up" (fn [cm]
                                               (when (seq @console-history)
                                                 (let [new-index (if (<= @history-index 0)
                                                                   (dec (count @console-history))
                                                                   (dec @history-index))]
                                                   (reset! history-index new-index)
                                                   (.setValue cm (nth (vec @console-history) new-index)))))})}}]])))


(defn cycle-mode [current-mode]
  (let [modes           @(ut/tracked-sub ::bricks/hop-bar-runners {})
        modes           (ut/postwalk-replacer {:views :clover} modes)
        modes           (vec (for [[k m] modes] (merge m {:name k})))
        current-index   (.indexOf modes current-mode)
        next-index      (mod (inc current-index) (count modes))]
    (get modes next-index base-hop-bar-runner)))

(defn hop-render-icon [i]
  (if (vector? i) [re-com/h-box
                   ;:style {:border "1px solid yellow"}
                   :height "40px" :gap "3px"
                   :children (map hop-render-icon i)]
      (cond

        (and (string? i) (cstr/includes? i "."))
        [:img {:src i :width "40px" :height "40px"}]

        (string? i) [re-com/box 
                     :child i 
                     :width "40px" 
                     :height "40px" 
                     :size "auto" 
                     :align :center 
                     :justify :center])))

(re-frame/reg-sub
 ::openai-key-valid?
 (fn [db _]
   (get-in db [:server :settings :openai-valid?] false)))

(declare custom-icon-button)

(defn recording-button []
  [custom-icon-button
   (let [audio-recording? @(ut/tracked-subscribe_ [::audio/recording?])]
     {:icon-name (if audio-recording? "ri-mic-line" "ri-mic-fill")
      :tooltip "transcribe voice to :param/voice"
      :on-click (if audio-recording?
                  #(ut/tracked-dispatch [::audio/stop-recording])
                  #(ut/tracked-dispatch [::audio/start-recording]))
      :active? audio-recording?
      :color (if audio-recording? "red" "#ffffff")})])

(defn quake-console [ww]
  (let [hh                  (* 2  db/brick-size)
        reacts!             [@console-responses @console-history @hide-responses? @ns-selected]
        nss                 @(ut/tracked-sub ::local-namespaces-for {:runner-key (get @selected-mode :name)})
        oai-valid?          @(ut/tracked-sub ::openai-key-valid? {})
        is-fabric?          (= (get @selected-mode :name) :fabric)
        fabric-settings     (when is-fabric? @(ut/tracked-sub ::bricks/fabric-settings {}))
        selected-block      @(ut/tracked-subscribe [::bricks/selected-block])
        no-selected?        (or (= selected-block "none!") (nil? selected-block))
        [runner data-key]   @(ut/tracked-sub ::bricks/editor-panel-selected-view {})
        has-nss?            (ut/ne? nss)
        ww                  (Math/floor (* ww  0.8))
        ww2                 (- ww 40)]
    (ut/tapp>> [:fabric-settings fabric-settings])
    [re-com/v-box
     :size "none"
     :attr {:on-mouse-enter #(reset! over-hop-bar true)
            :on-mouse-over #(when (not @over-hop-bar) (reset! over-hop-bar true))
            :on-mouse-leave #(reset! over-hop-bar false)}
     :style {:background-color "#00000099"
             :backdrop-filter "blur(4px) brightness(13%)"
             :box-shadow "0px -5px 5px 0px #00000099"
             :position "fixed" :bottom 0 :left "50%" :z-index 999 :transform "translateX(-50%)"
             :border-radius  "11px 11px 0px 0px"
             :width (px ww)
             :font-weight 700
             :transition "all 0.6s ease-in-out"
             :padding "8px"
             :height (px hh)}
     :children [[re-com/h-box
                 :justify :between

                 :children [[re-com/v-box
                             :children [[re-com/box
                                         :child (if @hop-bar-tooltip
                                                  (let [[title desc] @hop-bar-tooltip]
                                                    [re-com/h-box
                                                     :gap "14px"
                                                     :align :center
                                                     :children [[re-com/box :child (str title)
                                                                 :style {:font-size "18px"}]
                                                                [re-com/box :child (str desc)
                                                                 :style {:font-size "15px"
                                                                         :font-weight 400
                                                                         :opacity 0.8}]]])

                                                  (if false ;; (not @hide-responses?)
                                                    (str (get @console-responses (last @console-history) "")) ""))]
                                        (cond
                                          is-fabric?
                                          [re-com/v-box
                                           :children [(when (and (get @fbc-selected-model (get @selected-mode :name)) no-selected?)
                                                        (let [search-width 150
                                                              filtered-patterns (filterv #(cstr/includes? (str %) (str @searcher-atom))
                                                                                         (get fabric-settings :patterns))]
                                                          [re-com/h-box
                                                           :justify :center 
                                                           :children
                                                           [[re-com/h-box
                                                             :width (px search-width)
                                                             :justify :between 
                                                             ;:style {:border "1px solid lime"}
                                                             :style {:margin-top (when (> (count filtered-patterns) 5) "-6px")}
                                                             :align :center
                                                             :children [[re-com/input-text :src (at) :model searcher-atom :width "93%"
                                                                         :on-change #(reset! searcher-atom (let [vv (str %)]
                                                                                                             (if (empty? vv) nil vv)))
                                                                         :placeholder "(search patterns)"
                                                                         :change-on-blur? false
                                                                         :width            (px (- search-width 10))
                                                                         :style {:text-decoration  (when (ut/ne? @searcher-atom) "underline")
                                                                                 :color            "inherit"
                                                                                 :border           "none"
                                                                                 :outline          "none"
                                                                                 :font-size        "11px"
                                                                                 
                                                                                  ;:text-align       "center"
                                                                                 :background-color "#00000000"}]
                                                                        (when (ut/ne? @searcher-atom)
                                                                          [re-com/md-icon-button
                                                                           :md-icon-name "zmdi-close"
                                                                           :on-click #(reset! searcher-atom nil)
                                                                           :style {:cursor "pointer"
                                                                                   :font-size "10px"}])]]
                                                            [re-com/h-box
                                                             :padding "6px"
                                                             :gap "10px"
                                                             :width (px (- ww2 search-width))
                                                             :style {:font-size "11px"
                                                                     ;:border "1px solid pink"
                                                                     :overflow "auto"}
                                                             :children (vec (for [n filtered-patterns
                                                                                  :let [cc (str (theme-pull :theme/editor-outer-rim-color nil) 89)
                                                                                        ccx (ut/choose-text-color cc)
                                                                                        selected? (= n (get @fbc-selected-pattern (get @selected-mode :name)))]]
                                                                              [re-com/box
                                                                               :padding "5px"
                                                                               :attr {:on-click #(do
                                                                                                   (swap! fbc-selected-pattern assoc (get @selected-mode :name) (if selected? nil n))
                                                                                                   (when @cm-instance  (.focus @cm-instance)))}
                                                                               :style {:background-color (if selected? cc "#00000045")
                                                                                       :border (when (not selected?) (str "1px solid " cc 90))
                                                                                       :color (when selected? ccx)
                                                                                       :cursor "pointer"
                                                                                       :border-radius "5px"
                                                                                       :font-weight 500}
                                                                               :child (str n)]))]]]))
                                                      
                                                      (when (get @fbc-selected (get @selected-mode :name))
                                                        [re-com/h-box
                                                         :padding "6px"
                                                         :gap "10px"
                                                         :width (px ww2)
                                                         :style {:font-size "12px"
                                                                 :overflow "auto"
                                                                 ;:border "1px solid pink"
                                                                 }
                                                         :children (vec (for [n (get-in fabric-settings [:models (get @fbc-selected (get @selected-mode :name))])
                                                                              :let [cc (str (theme-pull :theme/editor-outer-rim-color nil) 89)
                                                                                    ccx (ut/choose-text-color cc)
                                                                                    selected? (= n (get @fbc-selected-model (get @selected-mode :name)))]]
                                                                          [re-com/box
                                                                           :padding "5px"
                                                                           :attr {:on-click #(do
                                                                                               (swap! fbc-selected-model assoc (get @selected-mode :name) (if selected? nil n))
                                                                                               (when @cm-instance  (.focus @cm-instance)))}
                                                                           :style {:background-color (if selected? cc "#00000045")
                                                                                   :border (when (not selected?) (str "1px solid " cc 90))
                                                                                   :color (when selected? ccx)
                                                                                   :cursor "pointer"
                                                                                   :border-radius "5px"
                                                                                   :font-weight 500}
                                                                           :child (str n)]))])

                                                      [re-com/h-box
                                                       :padding "6px"
                                                       :gap "10px"
                                                       :children (vec (for [n (keys (get fabric-settings :models))
                                                                            :let [cc (theme-pull :theme/editor-outer-rim-color nil)
                                                                                  ccx (ut/choose-text-color cc)
                                                                                  cnt (count (get-in fabric-settings [:models n]))
                                                                                  selected? (= n (get @fbc-selected (get @selected-mode :name)))]]
                                                                        [re-com/box
                                                                         :padding "5px"
                                                                         :attr {:on-click #(do
                                                                                             (swap! fbc-selected-model assoc (get @selected-mode :name) nil)
                                                                                             (swap! fbc-selected assoc (get @selected-mode :name) (if selected? nil n))
                                                                                             (when @cm-instance  (.focus @cm-instance)))}
                                                                         :style {:background-color (if selected? cc "#00000045")
                                                                                 :border (when (not selected?) (str "1px solid " cc 35))
                                                                                 :color (when selected? ccx)
                                                                                 :cursor "pointer"
                                                                                 :border-radius "5px"
                                                                                 :font-weight 700}
                                                                         :child [re-com/h-box
                                                                                 :gap "8px"
                                                                                 :children [[re-com/box
                                                                                             :child (str n)]
                                                                                            [re-com/box
                                                                                             :style {:opacity 0.5}
                                                                                             :child (str cnt)]]]]))]]]

                                          has-nss?
                                          [re-com/h-box
                                           :padding "6px"
                                           :gap "10px"
                                           :children (vec (for [n nss
                                                                :let [cc (theme-pull :theme/editor-outer-rim-color nil)
                                                                      ccx (ut/choose-text-color cc)
                                                                      selected? (= n (get @ns-selected (get @selected-mode :name)))]]
                                                            [re-com/box
                                                             :padding "5px"
                                                             :attr {:on-click #(do
                                                                                 (swap! ns-selected assoc (get @selected-mode :name) (if selected? nil n))
                                                                                 (when @cm-instance  (.focus @cm-instance)))}
                                                             :style {:background-color (when selected? cc)
                                                                     :color (when selected? ccx)
                                                                     :cursor "pointer"
                                                                     ;:padding-left "8px"
                                                                     :border-radius "5px"
                                                                     ;:font-size "14px"
                                                                     :font-weight 700}
                                                             :child (str n)]))]
                                          :else nil)]]
                            (when
                             (and (not @hide-responses?)
                                  (get @console-responses (last @console-history)))
                              [re-com/md-icon-button :md-icon-name "zmdi-close"
                               :on-click #(reset! hide-responses? true)
                               :style {:font-size "16px"
                                       :opacity 0.5}])]
                 :height (if
                          (or @hop-bar-tooltip has-nss? is-fabric? 
                              (and (not @hide-responses?)
                                   (get @console-responses (last @console-history)))) 
                           "auto" "0px")
                 :padding "9px"
                 :style {:position "fixed"
                         :bottom hh ;(if (get @console-responses (last @console-history)) hh (- hh 20))
                         :transition "all 0.6s ease-in-out"
                         :width (px (- ww 20))
                         :color "white"
                         :font-size "17px"
                         :border-radius  "11px 11px 0px 0px"
                         :background-color "#00000099"
                         :backdrop-filter "blur(4px) brightness(33%)"
                         :left "50%" :z-index 999 :transform "translateX(-50%)"}]
                [re-com/h-box
                 :size "none" :align :center :justify :center
                 :height (px (- hh 20))
                 :style {:border (str "3px dashed " (theme-pull :theme/universal-pop-color nil) 33)
                         :font-family (theme-pull :theme/monospaced-font nil)
                         :color (theme-pull :theme/universal-pop-color nil)
                         :border-radius "11px"
                         :padding-left "8px"
                         :overflow "hidden"
                         :font-size "22px"}
                 :children [[re-com/box
                             :child [hop-render-icon (get @selected-mode :icon "?")]
                             :attr {:on-click #(do (swap! selected-mode cycle-mode)
                                                   (reset! hop-bar-tooltip [(get @selected-mode :name)  (get @selected-mode :description)])
                                                   ;(js/setTimeout (fn [] (reset! hop-bar-tooltip nil)) 800)
                                                   (when @cm-instance  (.focus @cm-instance)))
                                    :on-mouse-enter #(reset! hop-bar-tooltip [(get @selected-mode :name)  (get @selected-mode :description)])
                                    :on-mouse-leave #(reset! hop-bar-tooltip nil)}
                             :style {:font-size "31px"
                                     :padding-left (if (vector? (get @selected-mode :icon "?")) "20px" "5px") 
                                     :margin-top "4px"
                                     :user-select "none"
                                     :cursor "pointer"
                                     ;:border "1px solid white"
                                     :font-weight 700}]
                            (when (and has-nss? (get @ns-selected (get @selected-mode :name)))
                              [re-com/box
                               :style {:opacity 0.33
                                       :font-weight 400
                                       :padding-left "12px"}
                               :child (str " " (get @ns-selected (get @selected-mode :name)) ":>")])
                            ;; (when is-fabric?
                            ;;   [re-com/box
                            ;;    :style {;:opacity 0.33
                            ;;            :font-weight 400
                            ;;            :padding-left "12px"}
                            ;;    :child [re-com/single-dropdown
                            ;;            :choices (vec (for [p (get fabric-settings :patterns)] {:id p :label p}))
                            ;;            :max-height "320px"
                            ;;            :can-drop-above? true
                            ;;            :est-item-height 100
                            ;;            :placeholder "(all possible shapes)"
                            ;;            :style {:font-size "13px"}
                            ;;            :model (get @fbc-selected-pattern (get @selected-mode :name))
                            ;;            :on-change #(swap! fbc-selected-pattern assoc (get @selected-mode :name) %)]
                            ;;    ])

                            [re-com/h-box 
                             :width (px (- ww 85))
                             ;:style {:border "1px solid green"}
                             :align :center
                             :justify :between
                             :attr {:on-click #(when @cm-instance  (.focus @cm-instance))}  
                             :children [[console-text-box nil nil " "] 
                                        
                                        (when (and is-fabric? (keyword? selected-block))
                                          [re-com/box
                                           :style {;:border "1px solid pink" 
                                                   :font-size "14px"
                                                   :padding-right "12px"}
                                           ;:child (str selected-block " "  runner " " data-key)
                                           :child (str selected-block " " data-key)
                                           ])
                                        [re-com/gap :size "6px"]
                                        (when oai-valid? [recording-button])
                                        [re-com/gap :size "6px"]
                                        ]]]]]]))



(defn draggable-editor
  [element block-id data-key-type data-key h w]
  [(reagent/adapt-react-class rdnd/Draggable)
   (let [nname    (str "editor-" (ut/replacer block-id ":" "") "-" (ut/replacer data-key ":" ""))
         view-key (keyword nname)
         data {:h         h
               :w         w
               :drag-meta {:source-table :hi :table-fields [:*] :connection-id nil :source-panel-key :block-7034 :type :view}
               :views     {view-key [:editor [block-id data-key-type data-key]]}
               :name      nname}]
     {:type          "meta-menu" ;:play-port
      :on-drag-end   #(do (reset! bricks/dragging? false))
      :on-drag-start #(do (reset! bricks/dragging? true)
                          (reset! bricks/dragging-size [(get data :w) (get data :h)])
                          (reset! bricks/dragging-body data))
      :data          (pr-str data)})
   [re-com/box
    :size "none"
    :child element
    :style {:cursor "grab"}]])

(defn image-exists? [url callback]
  (let [img (js/Image.)]
    (set! (.-onload img) #(do (println "Image loaded:" url) (callback true)))
    (set! (.-onerror img) #(do (println "Image failed to load:" url) (callback false)))
    (set! (.-src img) url)))

(defn image-or-alternative [clicked-screen]
  (let [image-loaded? (reagent/atom false)
        clicked-screen (ut/sanitize-name clicked-screen)  
        image-url (str "assets/screen-snaps/" clicked-screen ".jpg")]
    (image-exists? image-url #(reset! image-loaded? %))
    (fn []
      (if @image-loaded?
        [:img {:src image-url
               :height "100%" :width "100%"}]
        [re-com/box
         :size "auto" :align :center :justify :center
         :child (str "No image available for: " clicked-screen)
         :style {}]))))

(defn editor-panel
  [bricks-wide bricks-tall]
  (let [selected-panel-map  @(ut/tracked-subscribe [::bricks/selected-block-map])
        selected-block      @(ut/tracked-subscribe [::bricks/selected-block])
        selected-block?     (true? (and (not= selected-block "none!") (not (nil? selected-block))))
        sql-calls           @(ut/tracked-sub ::bricks/panel-sql-calls {:panel-key selected-block})
        client-name         @(ut/tracked-sub ::bricks/client-name {})
        views               @(ut/tracked-sub ::bricks/panel-views {:panel-key selected-block})
        runners             @(ut/tracked-sub ::bricks/panel-runners {:panel-key selected-block})
        runners-only        @(ut/tracked-sub ::bricks/panel-runners-only {:panel-key selected-block})
        system-panel?       (or (= selected-block "none!") (nil? selected-block))
        all-keys            (into (keys runners-only) (keys views))
        first-data-key      (first (keys sql-calls))
        ;first-data-key      (first all-keys)
        ;_ (ut/tapp>>  [:first-data-key first-data-key])
        first-view-key      (first all-keys)
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
        vertical?           (> bricks-tall bricks-wide)

        no-room-for-3?      (and (not @hide-panel-2?) (<= (if vertical? bricks-tall bricks-wide) 24))
        no-room-for-2?      (and (not @hide-panel-3?) (<= (if vertical? bricks-tall bricks-wide) 16))
        ;;_ (ut/tapp>> [:no-room-for-2? no-room-for-2? :no-room-for-3? no-room-for-3?])
        ;; panel-count         (if system-panel? 3
        ;;                         (if
        ;;                          (or views? queries? runners?)
        ;;                           (- 3 (count (filter #(= % true) [(or no-room-for-2? @hide-panel-2?)
        ;;                                                            (or no-room-for-3? @hide-panel-3?)])))
        ;;                           2))
        panel-count         (if (or views? queries? runners? system-panel?)
                              (- 3 (count (filter #(= % true) [(or no-room-for-2? @hide-panel-2?)
                                                               (or no-room-for-3? @hide-panel-3?)])))
                              3)

        ;;bricks-wide         (if (or system-panel? queries? views? runners?) bricks-wide (js/Math.ceil (* bricks-wide 0.66)))

        ttl-width           (* bricks-wide db/brick-size)
        ttl-width-px        (px ttl-width)

        single-height-bricks (if vertical? (js/Math.floor (/ bricks-tall panel-count)) bricks-tall)
        single-width-bricks (if vertical? bricks-wide (js/Math.floor (/ bricks-wide panel-count)))

        pdiff (if vertical?
                (- (Math/floor bricks-tall) (* single-height-bricks panel-count))
                (- (Math/floor bricks-wide) (* single-width-bricks panel-count)))

        single-height-bricks (if (and (> pdiff 0) vertical?)
                               (+ single-height-bricks (/ pdiff panel-count) 0.15)
                               single-height-bricks)

        single-width-bricks (if (and (> pdiff 0) (not vertical?))
                              (+ single-width-bricks (/ pdiff panel-count) 0.05)
                              single-width-bricks)



        ttl-height          (* bricks-tall db/brick-size)
        ttl-height-px       (px (+ 10 ttl-height))

        x-px                (px (first @detached-coords)) ;(px (* x db/brick-size))
        y-px                (px (last @detached-coords)) ;(px (* y db/brick-size))



        ;single-width-bricks 
        single-width        (if vertical?
                              (- (* single-width-bricks db/brick-size) 12)
                              (* single-width-bricks db/brick-size))
        single-width-px     (px single-width)

        hh1                 [@db/mad-libs-waiting-room @db/data-browser-query-con @db/view-title-edit-idx @db/value-spy
                             @db/item-browser-mode @db/scrubbers @db/solver-meta-spy @editor-size @db/structured-editor
                             @db/value-spy @bricks/dragging-editor? @hide-panel-2? @hide-panel-3?] ;; reactivity hack! React!

        single-height       (if vertical? (* single-height-bricks db/brick-size) ttl-height)
        single-height-px    (if vertical?
                              ;(px (+ single-height 10)) 
                              (px single-height)
                              ttl-height-px)


        ;hdiff (- (* single-height-bricks panel-count) (Math/floor bricks-tall))
        ;_  (ut/tapp>> [:ssw (* single-width-bricks panel-count) (Math/floor bricks-wide) vertical? pdiff])
        ;_  (ut/tapp>> [:ssh (* single-height-bricks panel-count) (Math/floor bricks-tall) vertical? pdiff])

        ;; {:keys [single-width-bricks single-width single-height bricks-wide bricks-tall]} @editor-dimensions
        atom-map            {:single-width-bricks single-width-bricks
                             :single-height-bricks single-height-bricks
                             :bricks-wide bricks-wide
                             :bricks-tall bricks-tall
                             :panel-count panel-count
                             :vertical? vertical?
                             :single-width single-width
                             :single-height single-height}
        _                   (when (not= @editor-dimensions atom-map) (reset! editor-dimensions atom-map))
        click-params        @(ut/tracked-sub ::bricks/all-click-params {})
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
       ;;; _ (ut/tapp>> [:mad-libs-combo? reco-selected? mad-libs-combo? mad-libs-combos (str (get selected-panel-map :mad-libs-combo-hash)) selected-block])
        data-key-type       @(ut/tracked-sub ::bricks/view-type {:panel-key selected-block :view data-key})
        screen-name         (ut/safe-name @(ut/tracked-subscribe [::bricks/screen-name]))
        screen-name-regex   #"(.|\s)*\S(.|\s)*"
        websocket-status    (select-keys @(ut/tracked-subscribe [::http/websocket-status]) [:status :datasets :panels :waiting])


        ;; are-solver            (get @db/solver-fn-lookup [:panels selected-block data-key])
        ;; solver-running-kw      (keyword
        ;;                         (cstr/replace
        ;;                          (str (ut/replacer are-solver
        ;;                                            ":solver/"
        ;;                                                                                    ;;"solver-status/*client-name*>"
        ;;                                            (str "solver-status/" (cstr/replace (str client-name) ":" ""))) ">running?")
        ;;                          ":" ""))
        ;; solver-running?       @(ut/tracked-sub ::conn/clicked-parameter-key-alpha {:keypath [solver-running-kw]})
                                
        ]
    
    ;; (when mad-libs-combo? (ut/tracked-dispatch [::bricks/update-reco-previews]))
    ;; (ut/tapp>> [:things-running @(ut/tracked-sub ::bricks/things-running {})])

    (when (nil? (get @db/data-browser-query selected-block)) ;; at this point lets just
      (swap! db/data-browser-query assoc selected-block data-key))
    (when (and (not (= reco-selected reco-preview)) reco-selected?)
      (insert-hidden-reco-preview reco-selected reco-viz reco-query reco-condis reco-combo shape-name true))
    [bricks/reecatch
     [(if vertical? re-com/v-box re-com/h-box)
      :size "none"
      :attr {:on-mouse-enter #(reset! bricks/over-block? true)
             :on-mouse-leave #(reset! bricks/over-block? false)}
      :children

      (if @bricks/dragging-editor?

        [(let [;coord-str (str (mapv Math/floor @editor-size))
               ;min? (= coord-str "[10 10]")
               ]

           [(if vertical? re-com/v-box re-com/h-box)
            :children (for [p (range panel-count)] 
                        [re-com/box
                         :size "auto"  
                         :align :center 
                         :justify :center
                         :height (px single-height)
                         :width (px single-width)
                         :style {:border (str "2px solid " (theme-pull :theme/universal-pop-color nil))
                                 :color (theme-pull :theme/universal-pop-color nil)}
                         :child [re-com/v-box :children [[re-com/box 
                                                          :size "auto" :align :center :justify :center 
                                                          :style {:font-size "40px"}
                                                          :child (str (+ p 1))]
                                                         [re-com/box :child (str " [" (Math/floor single-height-bricks) " " (Math/floor single-width-bricks) "]")]]]])
            :style {:user-select "none"
                    ;:pointer-events "none"
                    :font-size "33px"
                    :font-family (theme-pull :theme/base-font nil)}
            :size "1" :align :center :justify :center])]

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
               [re-com/h-box
                :size "auto"
                ;:justify :between
                :gap "10px"
                :children [[re-com/box
                            :child (if system-panel?
                                     [re-com/h-box :align :center :justify :center :gap "15px"
                                    ;;:style {:border "1px solid white"}
                                      :children [[re-com/box :child (str "db browser") :style
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
                                              ;;  [re-com/box :child (str "kits") :style
                                              ;;   {;:opacity (if @file-mode? 1.0 0.4)
                                              ;;    :opacity (if (= @db/editor-mode :kits) 1.0 0.4)
                                              ;;    :cursor  "pointer"} :attr {:on-click #(reset! db/editor-mode :kits)}]
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
                                     (str selected-block "'s panel map"))]

                           (let [style-map {:font-size "17px"
                                            :margin-top "-2px"
                                            :opacity 0.88}]
                             [re-com/h-box
                              :gap "2px"
                              :children [(if selected-block? 
                                           [draggable-editor
                                            [re-com/md-icon-button
                                             :md-icon-name "zmdi-n-1-square"
                                             :style (merge style-map {:cursor "grab"})]
                                            selected-block data-key-type data-key 
                                            (Math/floor single-height-bricks) 
                                            (Math/floor single-width-bricks)
                                            ]
                                           [re-com/md-icon-button
                                            :md-icon-name "zmdi-n-1-square"
                                            :style style-map])
                                         [re-com/md-icon-button
                                          :attr {:on-click #(reset! hide-panel-2? (not @hide-panel-2?))}
                                          :md-icon-name (if @hide-panel-2?   "zmdi-square-o" "zmdi-n-2-square")
                                          :style style-map]
                                         [re-com/md-icon-button
                                          :attr {:on-click #(reset! hide-panel-3? (not @hide-panel-3?))}
                                          :md-icon-name (if @hide-panel-3?   "zmdi-square-o" "zmdi-n-3-square")
                                          :style style-map]]])]]]]
             :style {:font-weight   500
                     :color         (theme-pull :theme/editor-font-color nil)
                     :border-radius (if vertical? "11px 11px 0px 0px" "11px 0px 0px 0px")
                     :background    (str "linear-gradient(" (theme-pull :theme/editor-rim-color nil) ", transparent)")
                     ;:pointer-events "none"
                     :user-select   "none"}]




            [re-com/box
             :style (when @bricks/dragging-editor?
                      {;:pointer-events "none"
                       :user-select  "none"})
             :child (cond
                      system-panel?                                            (condp = @db/editor-mode ; @file-mode?
                                                                                 :files  [re-com/box :child [editor-panel-metadata-files]
                                                                                          :style {:padding-top "10px"} :height
                                                                                          (px (- ttl-height 40))]
                                                                                 :kits   [re-com/box :child [editor-panel-metadata-kits]
                                                                                          :style {:padding-top "10px"} :height
                                                                                          (px (- ttl-height 40))]
                                                                                 :params [re-com/box :child [editor-panel-metadata-params single-width
                                                                                           single-height :param] :style
                                                                                          {:padding-top "10px"} :height
                                                                                          (px (- ttl-height 40))]
                                                                                 :meta   [re-com/box :child [editor-panel-metadata] :style
                                                                                          {:padding-top "10px"} :height
                                                                                          (px (- ttl-height 40))]
                                                                                 :viz    [re-com/box :child [editor-panel-viz] :style
                                                                                          {:padding-top "10px"} :height
                                                                                          (px (- ttl-height 40))]
                                                                                 :search [re-com/box :child [search-panel-metadata]
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
                            [re-com/box
                             :padding "8px"
                             :height "435px"
                             :size "none"
                             :child [bricks/read-only-clojure-box single-width 0
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
                                            [:img {:src "images/loading-cropped.gif" :width "19px" :height "19px"}])]]]]]
                                     :attr {:on-click #(when (not (or cur? cur-rev?))
                                                         (let []
                                                           (ut/tracked-dispatch [::bricks/get-combo-hash selected-block src-table-id-str
                                                                                 shape-name new-curr-axe])))}])]]]]])]]])


                      :else                                                    (let [s-kp @(ut/tracked-sub ::bricks/editor-panel-selected-view {})
                                                                                     ;;;;selected-kp @(ut/tracked-sub ::bricks/editor-panel-selected-view {})
                                                                                     selected-kp (if (nil? (first s-kp))
                                                                                                   nil
                                                                                                   s-kp)] ;; if viz-gen or
                                                                                ;;  (ut/tapp>> [:sel-keypath  (str selected-kp) 
                                                                                ;;              (get @db/data-browser-query selected-block)
                                                                                ;;              @(ut/tracked-sub ::bricks/selected-block-map-kp {:keypath selected-kp})
                                                                                ;;              ])
                                                                                 (cond

                                                                                   (get-in @db/structured-editor [selected-block data-key] false)
                                                                                   ;[re-com/box :child (str "editor!" selected-block " " data-key)]
                                                                                   [bricks/map-boxes2
                                                                                    @(ut/tracked-sub ::bricks/selected-block-map-kp {:keypath selected-kp})
                                                                                    selected-block data-key []
                                                                                    [single-height-bricks
                                                                                     single-width-bricks] nil]


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

                                                                                   :else (let [selected-view-type @(ut/tracked-sub ::bricks/view-type {:panel-key selected-block :view data-key})
                                                                                               repl? (true? (cstr/includes? (str selected-view-type) "clojure"))
                                                                                               all-block? (=  (get @db/data-browser-query selected-block) :*)
                                                                                               syntax (if all-block? "clojure"
                                                                                                          (get-in block-runners-map [selected-view-type :syntax]))]
                                                                           ;;(ut/tapp>> [selected-view-type repl? syntax])
                                                                                           (if (or (nil? syntax) (= syntax "clojure"))
                                                                                             (let [;;_ (reset! bricks/tt [selected-block s-kp (get @db/data-browser-query selected-block)])
                                                                                                   react! [@db/data-browser-query @db/value-spy]
                                                                                                   spy-hash (get @db/value-spy-hashes [selected-block data-key] "n/a")]
                                                                                               ^{:key (str "cm-" selected-block s-kp spy-hash   ;; important!
                                                                                                           (get-in @db/value-spy [selected-block data-key]))}
                                                                                               [re-com/h-box
                                                                                                :children [
                                                                                                          ;;  [bricks/reecatch
                                                                                                          ;;   [flows/alert-box selected-block data-key]]
                                                                                                           [bricks/panel-code-box
                                                                                                            (fn [] selected-block) ;; sneaky sneaky, react.... :/
                                                                                                            (fn [] s-kp)
                                                                                                            (+ 17 single-width) (- single-height 20)
                                                                                                            (if (nil? selected-kp)
                                                                                                              selected-panel-map
                                                                                                              (get-in selected-panel-map selected-kp)) repl? true nil]]])
                                                                                             
                                                                                            ;;  [bricks/panel-string-box 
                                                                                            ;;   selected-kp (+ 17 single-width) (- single-height 20)
                                                                                            ;;   (if (nil? selected-kp)
                                                                                            ;;     selected-panel-map
                                                                                            ;;     (get-in selected-panel-map selected-kp))
                                                                                            ;;   syntax]

                                                                                             (let [react! [@db/data-browser-query @db/value-spy]
                                                                                                   spy-hash (get @db/value-spy-hashes [selected-block data-key] "n/a")]

                                                                                               ^{:key (str "cm-string-" selected-block s-kp spy-hash   ;; important!
                                                                                                           (get-in @db/value-spy [selected-block data-key]))}
                                                                                               [re-com/h-box
                                                                                                :children [
                                                                                                          ;;  [bricks/reecatch
                                                                                                          ;;   [flows/alert-box selected-block data-key]]
                                                                                                           [bricks/panel-string-box
                                                                                                            (fn [] selected-block)
                                                                                                            (fn [] s-kp)
                                                                                                            (+ 17 single-width)  (- single-height 20)
                                                                                                            (if (nil? selected-kp)
                                                                                                              selected-panel-map
                                                                                                              (get-in selected-panel-map selected-kp))
                                                                                                            syntax
                                                                                                            repl? true nil]]]))))))]
            (when (and system-panel? (= @db/editor-mode :status))
              [re-com/box :child (str websocket-status) :style
               {:color        (str (theme-pull :theme/editor-font-color nil) 77) ;; "#ffffff44"
                :font-size    "12px"
                :margin-top   "-9px"
                :padding-left "8px"}])]]
          :height single-height-px
          :width (if 
                  (and (= @db/editor-mode :vvv) system-panel?  ) 
                   (px (* 2 single-width)) 
                   single-width-px) 
          :style {:overflow "hidden"}]

         (when (and (not @hide-panel-2?) (not no-room-for-2?))
           (cond
             (or views? queries? runners?) ; data-key 
             (let [data-key-type @(ut/tracked-sub ::bricks/view-type {:panel-key selected-block :view data-key})
                   view-type-map @(ut/tracked-sub ::bricks/view-type-map {:view-type data-key-type})
                   output-type   @(ut/tracked-sub ::bricks/repl-output-type {:panel-key selected-block :view-name data-key})
                   rowset-data?  @(ut/tracked-sub ::bricks/has-rowset-data? {:panel-key selected-block :view-name data-key})
                   modes         (vec (remove nil? (distinct (conj (get view-type-map :modes) (when rowset-data? :rowset)))))
                   current-view-mode @(ut/tracked-sub ::bricks/current-view-mode {:panel-key selected-block :data-key data-key})
                   runner? (and (not= data-key-type :views) (not= data-key-type :queries) (not= data-key-type :clover))


                 ;;_ (ut/tapp>> [selected-block data-key data-key-type view-type-map])
                 ;data-key-type (if (nil? data-key-type) :* data-key-type)
                 ;selected-block (if (nil? (get @db/data-browser-query selected-block)) first-data-key data-key)
                   ]
               [re-com/box
                :size "none"

                :child
                [re-com/v-box
                 :size "1"
                 ;:height single-height-px
                 :children
                 [[re-com/h-box :padding "4px"
                   :justify :between
                   :align :center
                   :children [;[re-com/box :child (str "queries & views")]
                              [re-com/h-box
                               :gap "1px"
                               :align :center
                               :justify :center
                               :children [[re-com/box :child "slices" 
                                           ;:align :center :justify :center
                                           ]
                                          [re-com/md-icon-button
                                           :attr {:on-click #(reset! hide-panel-2? true)}
                                           :md-icon-name "zmdi-chevron-left"
                                           :style {:font-size "17px"
                                                   ;:margin-top "-2px"
                                                   :opacity 0.88}]
                                          [re-com/box :child (str data-key-type)
                                           :style {;:border (str "1px solid " (str (theme-pull :theme/editor-outer-rim-color nil)))
                                                   :color (str (theme-pull :theme/editor-outer-rim-color nil))
                                                   :font-weight 700
                                                   :margin-left "4px"
                                                   :padding-left "5px"
                                                   :padding-right "5px"
                                                   :background-color "#00000033"
                                                   :border-radius "7px"
                                                   :font-size "12px"}]]]

                              (when runner?
                                [re-com/h-box
                                 :size "none"
                                 :align :center
                                 :justify :center
                                 :width "112px"
                                 :style {;:margin-top "-1px"
                                         :color (str (theme-pull :theme/editor-outer-rim-color nil))
                                         :font-size "12px"}
                              ; :justify :between 
                                 :children [[re-com/box
                                             :size "auto" :align :start  :justify :start
                                             :child "value"
                                             :attr {:on-click #(ut/tracked-dispatch [::bricks/select-output-type selected-block data-key :value])}
                                             :style {:border-radius "7px 0px 0px 7px"
                                                     :padding-right "4px"
                                                     :padding-left "4px"
                                                     :color (when (= output-type :value) "black")
                                                     :cursor (when (not (= output-type :value)) "pointer")
                                                     :background-color (if (= output-type :value)
                                                                         (str (theme-pull :theme/editor-outer-rim-color nil))
                                                                         "#00000033")}]
                                            [re-com/box
                                             :size "auto" :align :end :justify :end
                                             :child "live"
                                             :attr {:on-click #(ut/tracked-dispatch [::bricks/select-output-type selected-block data-key :output-live])}
                                             :style {;:border-radius "0px 7px 7px 0px"
                                                     :padding-left "4px"
                                                     :padding-right "4px"
                                                     :color (when (= output-type :output-live) "black")
                                                     :cursor (when (not (= output-type :output-live)) "pointer")
                                                     :background-color (if (= output-type :output-live)
                                                                         (str (theme-pull :theme/editor-outer-rim-color nil))
                                                                         "#00000033")}]
                                            [re-com/box
                                             :size "auto" :align :end :justify :end
                                             :child "output"
                                             :attr {:on-click #(ut/tracked-dispatch [::bricks/select-output-type selected-block data-key :output])}
                                             :style {:border-radius "0px 7px 7px 0px"
                                                     :padding-left "4px"
                                                     :padding-right "4px"
                                                     :color (when (= output-type :output) "black")
                                                     :cursor (when (not (= output-type :output)) "pointer")
                                                     :background-color (if (= output-type :output)
                                                                         (str (theme-pull :theme/editor-outer-rim-color nil))
                                                                         "#00000033")}]
                                            ;; (if (= output-type :output)
                                            ;;   [re-com/md-icon-button
                                            ;;    :src (at)
                                            ;;    :md-icon-name "zmdi-playlist-plus"
                                            ;;    :on-click #(ut/tracked-dispatch [::bricks/select-output-type selected-block data-key :output-live])
                                            ;;    :style {:color (theme-pull :theme/editor-outer-rim-color nil)
                                            ;;            :font-size "20px"
                                            ;;            :width "15px"
                                            ;;            :padding-left "2px"
                                            ;;            ;:margin-top "2px"
                                            ;;            }]
                                            ;;   [re-com/box :size "none" :width "15px" :child ""])
                                            ]])

                              [re-com/h-box
                               :style {:padding-right "10px"}
                               :gap "1px"
                               :children (for [mode-idx (range (count modes)) ;["clover" "text" "pretty" "rowset" "map-boxes"]
                                               :let [mode (get modes mode-idx)
                                                     is-last? (= mode-idx (dec (count modes)))
                                                     is-first? (= mode-idx 0)
                                                     only? (and is-last? is-first?)
                                                     bkgrd (str (theme-pull :theme/editor-outer-rim-color nil))
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
                                                    ;:margin-top "2px"
                                                    :border-radius (cond
                                                                     only? "7px"
                                                                     is-last? "0px 7px 7px 0px"
                                                                     is-first? "7px 0px 0px 7px"
                                                                     :else "0px")
                                                    :font-size "12px"}])]]
                   :style
                   {:font-weight 500
                    :color       (theme-pull :theme/editor-font-color nil)
                    :background  (str "linear-gradient(" (theme-pull :theme/editor-rim-color nil) ", transparent)")}]

                ;[block-layer-tabs selected-block data-key data-key-type mad-libs-combos runners sql-calls block-runners-map views views? runners? queries?]
                  [block-layer-tabs selected-block data-key data-key-type mad-libs-combos runners sql-calls block-runners-map views views? runners? queries?]

                ;[re-com/gap :size (px single-width) :style {:z-index 1}]

                  [re-com/box
                ;:style {:margin-top "50px"}
                   :child (if @db/bad-form?
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
                            (let [;{:keys [single-width-bricks single-width single-height bricks-wide bricks-tall]} @editor-dimensions
                                  query-box? (or (some #(= % (get @db/data-browser-query selected-block)) (keys sql-calls))
                                                 (some #(= % data-key) (keys sql-calls)))
                                  solver-meta-spy?      (get-in @db/solver-meta-spy [selected-block data-key] false)
                                  ;; runner-rowset? (and @(ut/tracked-sub ::has-rowset-data? {:panel-key selected-block :view-name data-key})
                                  ;;                     (= :rowset @(ut/tracked-sub ::current-view-mode {:panel-key selected-block :data-key data-key})))
                        ;; runners-items (into {} (for [[k v] runners] {(first (first v)) {:base-key k}}))
                        ;; runner-box? (or (some #(= % (get @db/data-browser-query selected-block)) (keys runners-items))
                        ;;                 (some #(= % data-key) (keys runners-items)))
                        ;; runner-data  (when runner-box?
                        ;;                (let [are-solver        (get @db/solver-fn-lookup [:panels selected-block data-key])
                        ;;                                                                                      ;; meta-data          (when are-solver
                        ;;                                                                                      ;;                      @(ut/tracked-sub ::conn/clicked-parameter-key-alpha
                        ;;                                                                                      ;;                                       {:keypath [(keyword (str (ut/replacer are-solver
                        ;;                                                                                      ;;                                                                             ":solver/" "solver-meta/")))]}))
                        ;;                      rdata             (when are-solver
                        ;;                                          @(ut/tracked-sub ::conn/clicked-parameter-key-alpha
                        ;;                                                           {:keypath [(keyword (str (ut/replacer are-solver
                        ;;                                                                                                 ":" "")))]}))
                        ;;                                                                                      ;; running-status    (when are-solver
                        ;;                                                                                      ;;                     @(ut/tracked-sub ::conn/clicked-parameter-key-alpha
                        ;;                                                                                      ;;                                      {:keypath [(keyword (str (ut/replacer are-solver
                        ;;                                                                                      ;;                                                                            ":solver/" "solver-status/*client-name*>")))]}))
                        ;;                      ] rdata))
                       ; is-layout? @(ut/tracked-subscribe [::bricks/is-layout? selected-block data-key])
                                  viz-gen?   (= :viz-gen (get @db/data-browser-query selected-block))]
                              [re-com/v-box
                               :children
                               [(cond query-box? [re-com/v-box :justify :between :children
                                                  (if (get @db/data-browser-query-con data-key)
                                                    [(let [repl-output (dissoc @(ut/tracked-subscribe [::bricks/repl-output data-key]) :status)
                                                           console     (vec (remove empty? (get repl-output :out [])))]
                                                       [re-com/v-box
                                                        :padding "9px"
                                                        :size "none"
                                                        :height (px (- single-height 107))
                                                        :style {;:border "1px solid pink"
                                                                :overflow "auto"} :children
                                                        [(when (ut/ne? console)
                                                           [re-com/box :child "console output (limited to 50 items)"])
                                                         (when (ut/ne? console)
                                                           [re-com/v-box :children (for [r console] [re-com/box :child (str r)])])
                                                         [shape/map-boxes2 (dissoc repl-output :out) "no-block" "no-flow" [] "no-block"
                                                          "map"]]])]

                                                    [[bricks/magic-table selected-block [data-key] single-width-bricks (- bricks-tall 3)] 
                                                     (when (not @bricks/dragging?)
                                                       [re-com/box 
                                                        :height (px (* 1.7 db/brick-size))
                                                        :style {:overflow "auto" :background-color "#00000038" :border-radius "12px"}
                                                        :child [bricks/read-only-sql-box single-width nil (str sql-string)]])]


                                                    ;;[[bricks/magic-table selected-block [data-key] single-width-bricks (* bricks-tall 0.8)]]
                                                    )]
                                      (or
                                       (get @db/data-browser-query-con data-key)
                                       solver-meta-spy?) (let [are-solver           (get @db/solver-fn-lookup [:panels selected-block data-key])
                                                               meta-data-ckp-str    (str (ut/replacer are-solver ":solver/" "solver-meta/"))
                                                               meta-data-ckp        (keyword meta-data-ckp-str)
                                                               out-type             @(ut/tracked-sub ::bricks/repl-output-type {:panel-key selected-block :view-name data-key})
                                                               ;meta-data-ckp-output (keyword (str meta-data-ckp-str ">output>evald-result>out"))
                                                               meta-data-ckp-output (if (= out-type :output-live)
                                                                                      (keyword (str meta-data-ckp-str ">incremental"))
                                                                                      (keyword (str meta-data-ckp-str ">output>evald-result>out")))
                                                               meta-data            (when are-solver
                                                                                      @(ut/tracked-sub ::conn/clicked-parameter-key-alpha
                                                                                                       {:keypath [meta-data-ckp]}))
                                                               running-status-ckp   (keyword (str (ut/replacer are-solver ":solver/" (str "solver-status/" (cstr/replace (str client-name) ":" "") ">"))))
                                                               running-status       (when are-solver
                                                                                      @(ut/tracked-sub ::conn/clicked-parameter-key-alpha
                                                                                                       {:keypath [running-status-ckp]}))
                                                               ;;console-output       (get-in meta-data [:output :evald-result :out] [])
                                                               console-output       (when are-solver
                                                                                      @(ut/tracked-sub ::conn/clicked-parameter-key-alpha
                                                                                                       {:keypath [meta-data-ckp-output]}))]
                                                  ;;(ut/tapp>>  [:console-output console-output])
                                                           [re-com/box
                                                            :size "none"
                                                            :style {:transform "translate(0)"}
                                                            :height (px (- single-height 100))
                                                            ;:width "98%"
                                                            :align :center
                                                            :style {:overflow "auto" :font-size "13px"}
                                                            :child [re-com/v-box
                                                                    :gap  "10px"
                                                                    :width "98%"
                                                                    :children [(when true  ;(ut/ne? (remove empty? console-output))
                                                                                 [re-com/v-box
                                                                                  :style {:border (str "3px solid " (theme-pull :theme/editor-outer-rim-color nil) 45)
                                                                                          :margin-top "10px"
                                                                                          :margin-left "3px"
                                                                                          :margin-right "3px"
                                                                                          :border-radius "12px"}
                                                                                  :children [[bricks/draggable-clover
                                                                                              [re-com/box
                                                                                               :style {:padding-top "4px"
                                                                                                       :color (str (theme-pull :theme/editor-outer-rim-color nil) 65)
                                                                                                       :padding-left "6px"}
                                                                                               :align :start :justify :center
                                                                                               :child "console output"]
                                                                                              [:console meta-data-ckp-output]
                                                                                              :views
                                                                                              (str "console " are-solver) :console-out
                                                                                              3 6]
                                                                                             ;; draggable-clover [element data runner & [nname h w]]
                                                                                             [bricks/console-viewer {:style {}
                                                                                                                     :text console-output
                                                                                                                     ;:width (+ px-width-int 70) 
                                                                                                                     ;:height (+ px-height-int 50)
                                                                                                                     }]
                                                                                            ;;  [re-com/v-box
                                                                                            ;;   :gap "0px"
                                                                                            ;;   :style {:padding-top "10px"}
                                                                                            ;;   :children (for [e console-output]
                                                                                            ;;               [:pre
                                                                                            ;;                {:style {:color (theme-pull :theme/editor-outer-rim-color nil)
                                                                                            ;;                         :background-color "#00000000"
                                                                                            ;;                         :border "none"
                                                                                            ;;                         :text-shadow "4px 4px 4px #00000088"
                                                                                            ;;                         :font-size "17px"
                                                                                            ;;                         :font-family (theme-pull :theme/monospaced-font nil)}}
                                                                                            ;;                e])]
                                                                                             ]])
                                                                               [re-com/v-box
                                                                                :style {:border (str "3px solid " (theme-pull :theme/editor-outer-rim-color nil) 45)
                                                                                        :margin-top "10px"
                                                                                        :margin-left "3px"
                                                                                        :margin-right "3px"
                                                                                        :border-radius "12px"}
                                                                                :children [[re-com/box
                                                                                            :style {:padding-top "4px"
                                                                                                    :color (str (theme-pull :theme/editor-outer-rim-color nil) 65)
                                                                                                    :padding-left "6px"}
                                                                                            :align :start :justify :center
                                                                                            :child "meta data"]
                                                                                           [bricks/map-boxes2
                                                                                            meta-data
                                                                                            ;selected-block
                                                                                            meta-data-ckp
                                                                                            data-key [] 
                                                                                            [single-height-bricks (- single-width-bricks 0.3)] ;;:output
                                                                                            nil]
                                                                                           ;;map-boxes2 [data block-id selected-view keypath kki init-data-type & [draggable? key-depth]]
                                                                                           ]]

                                                                               [re-com/v-box
                                                                                :style {:border (str "3px solid " (theme-pull :theme/editor-outer-rim-color nil) 45)
                                                                                        :margin-top "10px"
                                                                                        :margin-left "3px"
                                                                                        :margin-right "3px"
                                                                                        :border-radius "12px"}
                                                                                :children [[re-com/box
                                                                                            :style {:padding-top "4px"
                                                                                                    :color (str (theme-pull :theme/editor-outer-rim-color nil) 65)
                                                                                                    :padding-left "6px"}
                                                                                            :align :start :justify :center
                                                                                            :child "runner status"]
                                                                                           [bricks/map-boxes2
                                                                                            running-status
                                                                                            ;selected-block
                                                                                            running-status-ckp
                                                                                            data-key [] 
                                                                                            [single-height-bricks (- single-width-bricks 0.3)] ;;:output
                                                                                            nil]]]]]])
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
                                        [re-com/box
                                         :size "none"
                                         :style {:transform "translate(0)"}
                                         :height (px (- single-height 100))
                                         :width (px single-width)
                                         :child [bricks/honeycomb selected-block (or default-shape-view :oz) single-width-bricks (dec single-height-bricks)]])
                                      
                                      :else      (let [{:keys [single-width-bricks single-width single-height bricks-wide bricks-tall]} @editor-dimensions] ;; force reaction
                                                   [re-com/box :size "none"
                                                    :style {:transform "translate(0)"}
                                                    :height (px (- single-height 100))
                                                    :width (px single-width)
                                                    :child [bricks/honeycomb selected-block data-key  single-width-bricks (dec single-height-bricks)]]))
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
                                        structured-editor?    (get-in @db/structured-editor [selected-block data-key] false)
                                        value-spy?            (get-in @db/value-spy [selected-block data-key] false)
                                        ;solver-meta-spy?      (get-in @db/solver-meta-spy [selected-block data-key] false)
                                        are-solver            (get @db/solver-fn-lookup [:panels selected-block data-key])
                                        solver-running-kw      (keyword
                                                                (cstr/replace
                                                                 (str (ut/replacer are-solver
                                                                                   ":solver/"
                                                                                   ;;"solver-status/*client-name*>"
                                                                                   (str "solver-status/" (cstr/replace (str client-name) ":" "") ">")) ">running?")
                                                                 ":" ""))
                                        solver-running?       (when are-solver
                                                                @(ut/tracked-sub ::conn/clicked-parameter-key-alpha {:keypath [solver-running-kw]}))
                                        ]
                                    
                          ;; (ut/tapp>> [:solver-running? solver-running? are-solver (keyword (str (ut/replacer are-solver
                          ;;                                                                                    ":solver/" "solver-status/*client-name*>")  ">running?"))])
                          ;;;(ut/tapp>> [:dd @db/solver-fn-lookup])

                                    [re-com/h-box
                                     :style {:font-size   "11px"
                                             :margin-top "4px"
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

                                                  [re-com/box :size "none" :width "90px" :child (if structured-editor? "struc-ed on" "struc-ed off") :attr
                                                   {:on-click #(swap! db/structured-editor assoc-in [selected-block data-key] (not structured-editor?))} :style
                                                   {:color       (if structured-editor? (theme-pull :theme/universal-pop-color nil) "grey")
                                                    :z-index     100
                                                    :user-select "none"
                                                    :margin-top  (if query-box? "9px" "inherit")
                                                    :cursor      "pointer"}]

                                                  [re-com/box :size "none" :width "90px" :child "value spy"
                                                   :attr {:on-click #(swap! db/value-spy assoc-in [selected-block data-key] (not value-spy?))}
                                                   :style {:color           (if value-spy? (theme-pull :theme/universal-pop-color nil) "grey")
                                                           :z-index         100
                                                           :user-select     "none"
                                                           :text-decoration (when (not value-spy?) "strikethrough")
                                                           :margin-top      (if query-box? "9px" "inherit")
                                                           :cursor          "pointer"}]

                                        ;; (when are-solver

                                        ;;   [re-com/box
                                        ;;    :size "none"
                                        ;;    :width "90px"
                                        ;;    :child "solver meta"
                                        ;;    :attr {:on-click #(swap! db/solver-meta-spy assoc-in [selected-block data-key] (not solver-meta-spy?))}
                                        ;;    :style {:color           (if solver-meta-spy? (theme-pull :theme/universal-pop-color nil) "grey")
                                        ;;            :z-index         100
                                        ;;            :user-select     "none"
                                        ;;            :text-decoration (when (not solver-meta-spy?) "strikethrough")
                                        ;;            :margin-top      (if query-box? "9px" "inherit")
                                        ;;            :cursor          "pointer"}])
                                                  ]]

                                                (when are-solver
                                                  [re-com/h-box
                                                   :gap  "6px"
                                                   :style {:color (if solver-running? (theme-pull :theme/universal-pop-color nil) "grey")}
                                                   :children
                                                   [;[re-com/box :child (str solver-running-kw)]
                                                    ;[re-com/box :child (str "." @(ut/tracked-sub ::conn/clicked-parameter-key-alpha {:keypath [solver-running-kw]}))]
                                                    (when solver-running?
                                                      [re-com/md-icon-button :md-icon-name "zmdi-refresh" :class "rotate linear infinite"
                                                       :style
                                                       {:font-size "15px"
                                                        :transform-origin "7.5px 10px" ;; perfect spin!
                                                        :margin-top "-3px"}])
                                                    [re-com/box :child 
                                                     (str are-solver)
                                                     ;(str solver-running-kw)
                                                     ]]])]]))]]))]]]
                :height (px (- single-height 10)) ;; (px (- ttl-height 24))
                :width single-width-px
                :style {:overflow "hidden"}])

             system-panel?        [re-com/box :size "none" :child
                                   [re-com/v-box :size "1" :children
                                    [[re-com/box :padding "4px" :child (str "(fields, data-types)")
                                      :style {:font-weight 500
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
                                        :vvv    [re-com/box :size "none" :child "   " :width "0px"]
                                        :status [editor-panel-metadata-status] ;[editor-panel-status]
                                        )
                                      :height (px (- single-height 40))
                                      ;(px (- ttl-height 40))
                                      ]]]
                                   :height single-height-px
                                   :width
                                   (if 
                                    ;;false ;
                                    (= @db/editor-mode :vvv)
                                     "0px" single-width-px) 
                                   :style
                                   {;:border "1px solid white" :background-color "#1f2430"
                                    :overflow "hidden"}]
             :else [re-com/box :child "missing something?!"]))

         ;;; {:keys [single-width-bricks single-width single-height bricks-wide bricks-tall]} @editor-dimensions

         (when (and (not @hide-panel-3?) (not no-room-for-3?))
           (let [;{:keys [single-width-bricks bricks-wide]} @editor-dimensions
                 panels-open (if @hide-panel-2? 2 3)
                 offset-last (* db/brick-size (if (and vertical? (= panels-open 3)) -1 0)) ;(if vertical? 0 (* (- bricks-wide (* panels-open single-width-bricks)) db/brick-size))
                 ;offset-last (if @hide-panel-2?)
                 single-height (+ single-height offset-last)
                 single-height-px (px (- single-height 10))
                 ;single-width (+ single-width offset-last)
                 single-width-px (px single-width)] ;; when size is not cleanly mod/3 we need to add the extra offset
             ;(ut/tapp>> [:panel-3 vertical? offset-last single-width single-height])
             [re-com/v-box
              ;:margin (if vertical? "5px")
              ;:style {:border "1px solid white"}
              ;:height (px single-height)
              :width (px single-width)
              :children [(cond (and (or (= @db/editor-mode :viz)
                                        (= @db/editor-mode :vvv))
                                    reco-selected?
                                    (or (= selected-block "none!")
                                        (nil? selected-block)))

                               [re-com/v-box :size "auto"
                                :height (px (- single-height 80))
                                :children
                                [[re-com/box :style
                                  {:font-weight    500
                                   :color          (theme-pull :theme/editor-font-color nil)
                                   :background     (str "linear-gradient(" (theme-pull :theme/editor-rim-color nil) ", transparent)")
                                   :padding-top    "4px"
                                   :padding-bottom "4px"}
                                  :child [re-com/h-box
                                          :width (px (* single-width 0.538))
                                          :justify :between
                                          :children
                                          [[re-com/h-box
                                            :gap "1px"
                                            :align :center
                                            :justify :center
                                            :children [[re-com/box
                                                        :height "21px"
                                                        :style {:padding-left (when vertical? "5px")}
                                                        :child "reco preview"]]]]]]
                              ;;    [re-com/box :style
                              ;;     {:font-weight      500
                              ;;      :color            (theme-pull :theme/editor-font-color nil)
                              ;; ;:background-color (theme-pull :theme/editor-rim-color "#a3a3a3")
                              ;;      :padding-top      "4px"
                              ;;      :padding-bottom   "4px" ;:margin-left "2px"
                              ;;      }:child "reco preview"]
                                 [re-com/v-box :size "auto" :width (px (* single-width 0.95)) :gap "6px" :style
                                  {:margin-top "30px"
                                   :transform  "translate(0)" ;; a known CSS hack for fixed position in non fixed
                                   :overflow   "auto"} :children
                                  [;[re-com/box :child (str reco-selected)]
                                   [bricks/honeycomb :reco-preview :oz]]]]]

                               (= @db/editor-mode :files)

                               (let [clicked-screen @(ut/tracked-sub ::conn/clicked-parameter-key-alpha {:keypath [:files-sys/screen_name]})]
                                 ^{:key (str "preview-snap" clicked-screen)}
                                 [re-com/v-box :size "auto"
                                  :height (px (- single-height 80))
                                  :children
                                  [[re-com/box :style
                                    {:font-weight    500
                                     :color          (theme-pull :theme/editor-font-color nil)
                                     :background     (str "linear-gradient(" (theme-pull :theme/editor-rim-color nil) ", transparent)")
                                     :padding-top    "4px"
                                     :padding-bottom "4px"}
                                    :child [re-com/h-box
                                            :width (px (* single-width 0.538))
                                            :justify :between 
                                            :children
                                            [[re-com/h-box
                                              :gap "1px"
                                              :align :center
                                              :justify :center
                                              :children [[re-com/box
                                                          :height "21px"
                                                          :style {:padding-left (when vertical? "5px")}
                                                          :child ""]]]]]]
                                   [re-com/box
                                    :style
                                    {:font-weight      500
                                     :color            (theme-pull :theme/editor-font-color nil)
                                     :font-size        "18px"
                                     :padding-top      "4px"
                                     :padding-bottom   "4px"}
                                    :align :center :justify :center
                                    :child (str "screens/" clicked-screen ".edn")]
                                   [re-com/v-box
                                    :size "auto"
                                    :width (px (* single-width 0.95))
                                    :gap "6px"
                                    :style
                                    {:margin-top "20px"
                                 ;:transform  "translate(0)" ;; a known CSS hack for fixed position in non fixed
                                     :overflow   "hidden"}
                                    :children
                                    [[re-com/box
                                      :child [image-or-alternative clicked-screen]]]]]])

                               :else [re-com/h-box
                                      :size "auto"
                                      ;:style {:border "1px solid green"}
                                      :height (px (- single-height 20))
                                      :children [
                                                 (let [query? (or (some #(= % (get @db/data-browser-query selected-block)) (keys sql-calls))
                                                                   (some #(= % data-key) (keys sql-calls)))
                                                        react! [@db/params-or-meta-atom]]
                                                   [re-com/v-box
                                                    :size "none"
                                                    :width (px (* single-width 0.6))
                                                    :children [[re-com/box :style
                                                                {:font-weight    500
                                                                 :color          (theme-pull :theme/editor-font-color nil)
                                                                 :background     (str "linear-gradient(" (theme-pull :theme/editor-rim-color nil) ", transparent)")
                                                                 :padding-top    "4px"
                                                                 :padding-bottom "4px"}
                                                                :child [re-com/h-box
                                                                        :width (px (* single-width 0.538))
                                                                        :justify :between :children
                                                                        [[re-com/h-box
                                                                          :gap "1px"
                                                                          :align :center
                                                                          :justify :center
                                                                          :children [[re-com/h-box
                                                                                      :size "auto"
                                                                                      :style {:padding-left (when vertical? "5px")
                                                                                              :font-size "13px"}
                                                                                      :justify :between 
                                                                                      :gap "10px" 
                                                                                      :align :center
                                                                                      :children (if query?
                                                                                                  [[re-com/box
                                                                                                    :style {:cursor "pointer"
                                                                                                            :opacity (if (= @db/params-or-meta-atom :params) 1 0.5)}
                                                                                                    :attr {:on-click #(reset! db/params-or-meta-atom :params)}
                                                                                                    :child "params"]
                                                                                                   [re-com/box
                                                                                                    :style {:cursor "pointer"
                                                                                                            :opacity (if (= @db/params-or-meta-atom :meta) 1 0.5)}
                                                                                                    :attr {:on-click #(reset! db/params-or-meta-atom :meta)}
                                                                                                    :child "meta"]]
                                                                                                  [[re-com/box :child "parameters"]])]
                                                                                     [re-com/md-icon-button
                                                                                      :attr {:on-click #(reset! hide-panel-3? true)}
                                                                                      :md-icon-name "zmdi-chevron-left"
                                                                                      :style {:font-size "17px"
                                                                                              ;:margin-top "-2px"
                                                                                              :opacity 0.88}]]]
                                                                         [re-com/h-box
                                                                          :gap "3px"
                                                                          :style {:font-size "11px" :margin-top "3px"}
                                                                          :align :center
                                                                          :justify :center
                                                                          :children
                                                                          (vec (for [f (keys @db/param-filter)]
                                                                                 [re-com/box :child (ut/safe-name f) :attr
                                                                                  {:on-click #(swap! db/param-filter assoc f (not (get @db/param-filter f false)))} :style
                                                                                  {:text-decoration (if (get @db/param-filter f true) "none" "line-through")
                                                                                   :opacity         (if (get @db/param-filter f true) 1 0.4)
                                                                                   :cursor          "pointer"
                                                                                   :user-select     "none"
                                                                                   :padding-left    "4px"
                                                                                   :padding-right   "4px"}]))]]]]
                                                               (if (and query? (= @db/params-or-meta-atom :meta))  
                                                                 [bricks/query-meta-browser data-key (* single-width 0.6)  (- single-height 1.3)]
                                                                 [bricks/click-param-browser click-params (* single-width 0.6) (- single-height 1.3)])]])

                                                 (let [data-key-type @(ut/tracked-sub ::bricks/view-type {:panel-key selected-block :view data-key})
                                                       view-type-map @(ut/tracked-sub ::bricks/view-type-map {:view-type data-key-type})
                                                       nrepl? (= (get view-type-map :type) :nrepl)]
                                                   [re-com/v-box
                                                    :size "none"
                                                    :width (px (* single-width (if vertical? 0.4 0.379)))
                                                    :children
                                                    [[re-com/h-box :style
                                                      {:font-weight    500
                                                       :color          (theme-pull :theme/editor-font-color nil)
                                                       :background     (str "linear-gradient(" (theme-pull :theme/editor-rim-color nil) ", transparent)")
                                                       :border-radius  (when (not vertical?) "0px 11px 0px 0px")
                                                       :padding-top    "4px"
                                                       :padding-bottom "4px"}

                                                      :children
                                                      [[re-com/h-box :gap "8px"
                                                        :width (px (- (* single-width 0.405) (if vertical? 8 18)))
                                                        :justify :end
                                                        :style {:font-size "12.5px" :margin-top "-2px"}
                                                        :align :center
                                                        :children
                                                        (conj
                                                         (vec (for [b    (remove nil? [(when nrepl?
                                                                               ;"nrepl"
                                                                                         "namespace") "queries" "blocks"])
                                                                    :let [selected? (= (keyword b) @db/item-browser-mode)]]
                                                                [re-com/box :attr {:on-click #(reset! db/item-browser-mode (keyword b))}
                                                                 :style
                                                                 (if selected?
                                                                   {:user-select "none"
                                                                    :opacity 1.0}
                                                                   {:cursor "pointer" :user-select "none" :opacity 0.3}) :child
                                                                 (str b)]))
                                                         [re-com/md-icon-button :md-icon-name "zmdi-window-minimize" :on-click
                                                          #(ut/tracked-dispatch [::bricks/toggle-editor]) :style {:font-size "15px" :opacity 0.33 :cursor "pointer"}])]
                 ;[re-com/gap :size "12px"]
                                                       ]]
                                                     [re-com/box :style {:padding-right "3px"} :child
                                                      (cond (= @db/item-browser-mode :queries) [bricks/screen-query-browser
                                                                                                (* single-width 0.379)
                                                                                                (- single-height 1.3)]
                                                            (= @db/item-browser-mode :blocks)  [bricks/screen-block-browser
                                                                                                (* single-width 0.379)
                                                                                                (- single-height 1.3)]
                                                            (= @db/item-browser-mode :namespace)  [bricks/nrepl-introspection-browser
                                                                                                   selected-block data-key-type data-key
                                                                                                   (* single-width 0.379)
                                                                                                   (- single-height 1.3)
                                                                                                   @editor-dimensions]

                                                            :else                              [re-com/box :child "nothing selected above?"])]]])]
                                      :height single-height-px
                                      :width single-width-px
                                      :style
                                      {;:border "1px solid white" :background-color "#1f2430"
                                       :overflow "hidden"}])
                         [re-com/h-box
                          :height "42px"
                          ;:width (px (- (* single-width 0.98) 1))
                          :size "none"
                          :align :end
                          ;:justify :end
                          :gap "9px"
                          :children [(if (not (and (= @db/editor-mode :viz) reco-selected?))
                                       (let [cc (theme-pull :theme/editor-outer-rim-color nil)
                                             ccc (count cc)
                                             cc (if (> ccc 7) (subs cc 0 7) cc)]
                                         [re-com/h-box ;:padding "8px"
                                          ;:justify :end 
                                          :align :center ;:size "auto"
                                          :width (px (- single-width 10))
                                          :children [;[re-com/box
                                                     [re-com/input-text
                                                      :src (at)
                                                      :model (str screen-name)
                                                      :width (px (- single-width 20))
                                                      :height "30px"
                                                      :on-change #(ut/tracked-dispatch [::bricks/set-screen-name %]) :validation-regex screen-name-regex :change-on-blur? true
                                                      :style
                                                      {:font-size        "20px"
                                                       :font-style       "underline"
                                                       :border           "0px solid black"
                                                       :padding          "8px"
                                                       :background-color "#00000000"
                                                       :color            (theme-pull :theme/editor-font-color nil)}]]
                                          :style {:color (theme-pull :theme/editor-font-color nil)
                                                  :border-top  (str "3px solid " cc 55)
                                                  :border-left (str "3px solid " cc 55)
                                                   ;:background-color (str cc 35)
                                                  :border-radius "9px 0px 0px 0px"}])
                                       [re-com/box :align :center :justify :center :child (str "viz preview: " reco-combo) :style
                                        {:font-size        "16px"
                                         :font-weight      700
                                         :font-style       "underline"
                                         :border           "0px solid black"
                                         :padding          "8px"
                                         :margin-top       "-14px"
                                         :background-color "#00000000"
                                         :color            (str (theme-pull :theme/editor-font-color nil) 66) ;; "#ffffff66"
                                         }])]:style
                          {;:padding-left   "8px"
                           ;:margin-left    "8px"
                           :padding-top    "12px"
                           :padding-bottom "4px"
                           :padding-right  "10px"
                           :border-radius  "10px 0px 10px 0px"
                           :box-shadow     "0px -5px 5px 0px #00000099"
                           :font-weight    500
                           :color          (theme-pull :theme/editor-background-color nil) ;; "#000000"
                           :background     (str "linear-gradient(" (theme-pull :theme/editor-rim-color nil) ", transparent)")}]]]))

         [re-com/box :child " "
          :width "11px" :height "11px"
          :attr {:on-mouse-down resize-mouse-down-handler}
          :style {:background-color (theme-pull :theme/editor-outer-rim-color nil)
                  :position "fixed"
                  :z-index 99999
                  :right -1
                  :cursor "se-resize"
                  :border-radius "8px 0px 0px 0px"
                  :bottom -1}]])
      :width ttl-width-px
      :height ttl-height-px
      :margin "-2px"
      :style (merge
              (when vertical? {:padding-left  "2px"})
              {:position         "fixed"
               :top              y-px
               :left             x-px
               :border-radius    "16px"
               :user-select      (when @bricks/dragging-editor? "none")
               ;:pointer-events    (when @bricks/dragging-editor? "none")
               :z-index          100
               :border           (str "6px solid " (theme-pull :theme/editor-outer-rim-color nil)) ; #b7e27c"
               :filter           "drop-shadow(0.35rem 0.35rem 0.4rem rgba(0, 0, 0, 0.8))"
               :background-color (str (theme-pull :theme/editor-background-color nil) 77) ; "#000000"
               :backdrop-filter  "blur(5px)"}
              (when @bricks/dragging-editor?
                (let [cc (theme-pull :theme/editor-outer-rim-color nil)
                      ccc (count cc)
                      cc (str (if (> ccc 7) (subs cc 0 7) cc) 45)]
                  {:user-select "none"
                   :background-image (str ;"linear-gradient(0deg, " cc " 1px, transparent
                                      "linear-gradient(0deg, "
                                      cc
                                      " 2px, transparent 8px), linear-gradient(90deg, "
                                      cc
                                      " 2px, transparent 8px)"
                                   ;(when (get custom-map :background-image) ", ")
                                   ;(get custom-map :background-image)
                                      )
                   :background-size  (str "50px 50px, 50px 50px"
                                       ;(when (get custom-map :background-size) ", ")
                                       ;(get custom-map :background-size)
                                          )})))]]))

;; (dnd2/subscribe! (js/document.querySelector "div") ;; "base-canvas"
;;                  :canvas02
;;                  {:drop (fn [_ files]
;;                           (when (not (= ""
;;                                         (-> files
;;                                             .-value)))
;;                             (let [^js/File file0 (-> files
;;                                                      (aget 0))
;;                                   fname          (-> file0
;;                                                      .-name)]
;;                               (ut/tapp>> [:run? (cstr/lower-case fname)])
;;                               (if (cstr/ends-with? (cstr/lower-case fname) ".csv")
;;                                 (do (ut/tapp>> [:saving-csv-to-server fname])
;;                                     (ut/read-file file0 #(ut/tracked-dispatch [::http/save-csv fname (str %)]))
;;                                     (set! (-> files
;;                                               .-value)
;;                                           ""))
;;                                 (ut/tapp>> [:invalid-file fname])))))})

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
                                               [re-com/h-box
                                                :children [[re-com/input-text :model (str t) :on-change
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
                                               (str t)))
                                           :size "none"
                                           :align :center
                                           :justify (when (not @db/show-tabs?) :center)
                                           :padding "5px" :width (when (not @db/show-tabs?) "5px")
                                           :style {:transition_NOT "all 0.2s ease-in-out" :backdrop-filter "blur(8px)"}]
                                          :attr (merge (when (not (= @title-edit-idx t))
                                                         {:on-click #(do (ut/tracked-dispatch [::bricks/select-tab (str t)])
                                                                         (reset! title-edit-idx nil))})
                                                       (if selected? {:on-double-click #(reset! title-edit-idx (str t))} {})
                                                       {:on-context-menu #(ut/tracked-dispatch [::bricks/toggle-tab-visibility
                                                                                                (str t)])})
                                          :style {:cursor           "pointer"
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
                         (bricks/draggable (let [[_ _ w h] @(ut/tracked-sub ::bricks/tab-recenter-alpha {:tab t})]
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
              ;:pointer-events "none"
              :user-select     "none"
              :transition_NOT  "all 0.2s ease-in-out"
              :transform-style "preserve-3d"}])]
    [re-com/v-box :children [[tab-row 0 tabs] (when (ut/not-empty? hidden-tabs) [tab-row 30 hidden-tabs true])]]))

(defn snapshot-menu []
  (let [;tabs @(ut/tracked-subscribe [::bricks/tabs])
        matching   @(ut/tracked-subscribe [::bricks/matching-snapshots])
        snapshots  @(ut/tracked-subscribe [::bricks/snapshots])
        tabs       (vec (map :key (sort-by :key (vec (for [[k v] snapshots :when (get v :menu?)] (assoc v :key k))))))
        react-hack [@db/show-snaps? @db/vertical-snaps?]]
    (when (ut/ne? tabs)
      [(if @db/vertical-snaps? re-com/v-box re-com/h-box)
       :children  (reverse
                   (conj
                    (vec
                     (conj
                      (for [t (reverse tabs)]
                        [re-com/box :child
                         [re-com/box :child (if (not @db/show-snaps?) (str (try (.indexOf tabs t) (catch :default _ "err!"))) (str t))
                          :size "none" :align :center :justify (when (not @db/show-snaps?) :center) :padding "5px" :width
                          (when (not @db/show-snaps?) "5px") 
                          :attr {:on-click #(ut/tracked-dispatch [::bricks/swap-snapshot t])} 
                          :style {:transition_NOT "all 0.2s ease-in-out" :backdrop-filter "blur(8px)"}] :style
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
        ;:pointer-events "none"
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

(re-frame/reg-event-db
 ::update-user-params-hash
 (fn [db _]
   (let [fs                (vec (for [kk   (get db :flow-subs)
                                      :let [[f1 f2] (ut/splitter (ut/replacer (str kk) ":" "") "/")]]
                                  [(keyword f1) (keyword f2)]))
         pp                (get db :click-param)
         pp-without-fs     (ut/remove-keys pp
                                           (into (map first fs)
                                                                      ;;  [:flow :time :server :flows-sys :client :solver :kit :runstream
                                                                      ;;   :solver-status :flow-status :data :repl-ns :kit-status 
                                                                      ;;   :signal-history :solver-meta nil]
                                                 (vec db/reactor-types)))
         click-param-autos (vec (filter #(not (cstr/includes? (str %) "function"))
                                        (distinct (flatten
                                                   (for [kk (filter (fn [x] (not (cstr/includes? (str x) "-sys")))
                                                                    (keys pp))]
                                                     (create-clover-keys-from-data (get pp-without-fs kk) (str (cstr/replace (str kk) ":" "") "/")))))))
         new-autocompletes (vec (into click-param-autos
                                      (into (create-clover-keys-from-data (get pp-without-fs :param) "param/")
                                            (create-clover-keys-from-data (get pp-without-fs :theme) "theme/"))))
         data-keys     (vec (for [e (map last (remove empty? (filter #(and (not (number? (last %))) (= (count %) 3)) (ut/kvpaths (get db :panels)))))]
                              (keyword (str "data/" (cstr/replace (str e) ":" "")))))
        ;;  _ (ut/tapp>> [:click-param-autos data-keys click-param-autos new-autocompletes])
         new-h             (hash pp-without-fs)
         client-name       (get db :client-name)]
     (ut/tapp>>  [:update-user-params-hash-event (count new-autocompletes) new-autocompletes (count data-keys) data-keys])
     (reset! db/autocomplete-keywords (set (into data-keys (into new-autocompletes @db/autocomplete-keywords)))) ;; dont
     (ut/tracked-dispatch [::wfx/push    :default ;; just a push, no response handling
                           {:kind        :sync-client-params
                            :params-map  pp-without-fs ;; pp ;; why send things we
                            :client-name client-name}])
     (reset! temp-atom pp-without-fs)
     (assoc db :user-params-hash new-h))))

;;(re-frame/dispatch [::update-user-params-hash])

(re-frame/reg-sub ::watch-user-params
                  (fn [db]
                    (let [fs            (vec (for [kk   (get db :flow-subs)
                                                   :let [[f1 f2] (ut/splitter (ut/replacer (str kk) ":" "") "/")]]
                                               [(keyword f1) (keyword f2)]))
                          pp            (get db :click-param)
                          pp-without-fs (ut/remove-keys pp
                                                        (into (map first fs)
                                                              ;; [:flow :time :server :flows-sys :client :solver :signal-history :runstream 
                                                              ;;  :data :repl-ns :solver-status :flow-status :kit :kit-status :solver-meta :repl-ns nil]
                                                              (conj (vec db/reactor-types) :selected-block :selected-view)
                                                              ))]
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
          running-key  (keyword (str "flow-status/" flow-id ">*running?"))
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
    (let [min-panels              @(ut/tracked-sub ::bricks/all-panels-minimized {})
          user-param-hash2        @(ut/tracked-sub ::watch-user-params {})
          selected-block          @(ut/tracked-sub ::bricks/selected-block {})
          selected-block?         (not (or (= selected-block "none!") (nil? selected-block)))
          selected-view           @(ut/tracked-sub ::bricks/editor-panel-selected-view {})
          sselected-view          @(ut/tracked-sub ::conn/clicked-parameter-alpha {:keypath [:param :selected-view]})
          editor?                 @(ut/tracked-sub ::bricks/editor? {})
          user-param-hash1        @(ut/tracked-sub ::user-params-hash {})
          minimized-system-panels @(ut/tracked-sub ::minimized-system-panels {})
          audio-playing?          false ;@(ut/tracked-subscribe [::audio/audio-playing?])
          rs-overrides            @(ut/tracked-sub ::bricks/runstream-override-map {})
          rs-overrides-hashmap    @(ut/tracked-sub ::rs-overrides-hashmap {})
          theme-colors-hashmap    @(ut/tracked-sub ::theme-colors-hashmap {})
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
                 (ut/tapp>> [:user-params-changed-updating-autocomplete! (str [:run :views/update-user-params-hash])])
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
                         nn         (if (= nn "buffy")
                                      (if selected-block? "history" "runstreams")
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
              [re-com/md-icon-button :md-icon-name "zmdi-refresh" :class "rotate linear infinite" 
               :style {:font-size "15px" 
                       :transform-origin "7.5px 10px" 
                       :margin-bottom "-8px"}]
              [re-com/gap :size "10px"])]]]]))))

(defn session-modal []
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

;; (defn doom-modal []
;;   ;;(ut/tracked-dispatch [::audio/text-to-speech11 :audio :speak "/home/ryanr/hit.mp3" true])
;;   (let [hh          @(ut/tracked-subscribe [::subs/h])
;;         ww          @(ut/tracked-subscribe [::subs/w])
;;         ;per-page    9
;;         w           (* ww 0.33) ;500
;;         h           (* hh 0.45) ;500
;;         audio-playing?       @(ut/tracked-sub ::audio/audio-playing? {})
;;         client-name @(ut/tracked-subscribe [::bricks/client-name]) ;; reactions
;;         ;sessions    @(ut/tracked-subscribe [::bricks/sessions])
;;         left        (- (/ ww 2) (/ w 2))
;;         top         (- (/ hh 2) (/ h 2))
;;         ;items       (count sessions)
;;         ;items       (if (odd? items) items (inc items))
;;         ;sessions    (take per-page (sort-by last sessions))
;;         ;sessions    (partition-all 3 sessions)
;;         ]

  
;;     [re-com/modal-panel :style
;;      {:z-index 99999 ;; on top of alerts?
;;       :padding "0px"}
;;      :frame-style {:background-color "#000000"}
;;      :parts {:child-container
;;              {:style {:left left
;;                       :width w
;;                       :height h
;;                       :transform "scale(1.5)" :top top :font-size "8px" :background-color "#000000" :position "fixed"
;;                       :box-shadow       (let [block-id       :audio
;;                                               talking-block? true]
;;                                           (cond (and audio-playing? talking-block?) (str
;;                                                                                      "1px 1px " (px (* 80
;;                                                                                                        (+ 0.1 (get @db/audio-data block-id))))
;;                                                                                      " "        (theme-pull :theme/editor-outer-rim-color nil))
;;                                                 :else                               "none"))
;;                       :border "1px solid red"}}}
;;      :backdrop-opacity 0.75 :backdrop-on-click #(ut/tracked-dispatch [::bricks/disable-doom-modal])
;;      :child  [re-com/box :child "DOOM!"]]))


(defonce fanfare? (atom true)) 
(defonce time-freq (reagent/atom 45))
(defonce selected-stats-page (reagent/atom :help))

(defn stats-page [w h client-name ttt solver-map]
  (let [puget-data-color-map {} ;; @(ut/tracked-sub ::conn/puget-data-color-map {})
        ;;_ (ut/tapp>> [:puget-data-color-map puget-data-color-map])
        w (- w 45)
        h (- h 55)]
    [re-com/v-box
     :size "none"
     :height (px h)
     :width (px w)
     ;:style {:border "1px dotted pink"}
     :children [[re-com/h-box
                 :width (px w)
                 :height "30px"
                 :justify :between 
                 :align :center
                 :children
                 [[re-com/h-box
                   :children (vec (for [e [:help :client :system :reactor :pools :clients :sql-sizes]]
                                    [re-com/box
                                     :attr {:on-click #(reset! selected-stats-page e)}
                                     :style {:color (if (= e @selected-stats-page) "cyan" "white")
                                             :text-shadow (when (= e @selected-stats-page) "2px 1px 3px cyan") 
                                             :cursor "pointer"}
                                     :child (str e)]))
                   :align :center :justify :center
                   :gap "10px"
                   :padding "8px"
                   :style {:font-family "Fixedsys-Excelsior, monospace"
                           :font-weight 700
                           :font-size "12px"}]
                  [re-com/h-box
                   :children (vec (for [e [15 30 45 90 180 360 600]]
                                    [re-com/box
                                     :attr {:on-click #(reset! time-freq e)}
                                     :style {:color (if (= e @time-freq) "cyan" "white")
                                             :text-shadow (when (= e @time-freq) "2px 1px 3px cyan")
                                             :cursor "pointer"}
                                     :child (str e)]))
                   :align :center :justify :center
                   :gap "10px"
                   :padding "8px"
                   :style {:font-family "Fixedsys-Excelsior, monospace"
                           :font-weight 700
                           :font-size "12px"}]]]
                [bricks/honeycomb-fragments
                 (walk/postwalk-replace
                  {:client-name client-name
                   :color-map puget-data-color-map
                   :tt ttt
                   :freq @time-freq
                   :ww w}
                  [:v-box
                   :size "none"
                   ;:style {;:zoom 0.6
                   ;        :border "3px solid pink"}
                   :height (px h)
                   :width (px w)
                   :children [[:terminal-custom
                               [[:run-solver
                                 solver-map]
                                (- w 60) ;(* w 1.49) ;(Math/floor (/ w 7))
                                (if (= @selected-stats-page :help) 
                                  (- h 320) (- h 75)) ;(* (- h 40) 1.49) ;(Math/floor (/ h 1.55))
                                false
                                {:id (str "DooM-MenU-" (hash solver-map))
                                 :style {:font-size "11px"
                                         :line-height "0.9"}
                                 :px-line-height 13}]]
                              (when (= @selected-stats-page :help) 
                                [:iframe
                                 {:width "540"
                                  :height "310"
                                  :src
                                  "https://www.youtube.com/embed/rxYTSI7aenI?si=q_xwABn5x2ths3RB"
                                  :title "YouTube video player"
                                  :frameBorder "0"
                                  :allow
                                  "accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture;  "
                                  :referrerPolicy
                                  "strict-origin-when-cross-origin"
                                  :allowFullScreen true}])
                              ]])
                 (/ w 50)
                 (/ h 50)]]]))

(defn main-stats-page [w h client-name ttt]
  [stats-page w h client-name ttt
   (cond (= @selected-stats-page :client)
         {:signal false
          :cache? false
          :type :clojure
          :input-map {}
          :data '(do (ns rvbbit-backend.websockets)
                     (let [tt :tt]
                       (with-out-str
                         (fig-render ":client-stats" :bright-cyan)
                         (draw-client-stats
                          [:client-name] [:freq]
                          [:mem-mb :latency :messages-per-second :server-subs]
                          false
                          (Math/floor (/ :ww 5.66))
                          {:force-color :bright-cyan}))))}

         (= @selected-stats-page :pools)
         {:signal false
          :cache? false
          :type :clojure
          :input-map {}
          :data '(do (ns rvbbit-backend.websockets)
                     (let [tt :tt]
                       (with-out-str
                         (fig-render ":pool-party" :bright-cyan)
                         (draw-stats [:pool-tasks-run+ :pool-tasks] [:freq] false (Math/floor (/ :ww 5.66)) true)
                         (println "")
                         (show-pool-sizes-report {:width (Math/floor (/ :ww 5.66)) :color-scheme :color-map}))))}

         (= @selected-stats-page :sql-sizes)
         {:signal false
          :cache? false
          :type :clojure
          :input-map {}
          :data '(do (ns rvbbit-backend.websockets)
                     (let [tt :tt]
                       (with-out-str
                         (fig-render ":sql-sizes" :bright-cyan)
                         (draw-stats [:sql-queries :sql-exec] [:freq] false (Math/floor (/ :ww 5.66)) true)
                         (println "")
                         (ut/pp (get-table-sizes) {:width (Math/floor (/ :ww 5.66)) :color-scheme :color-map})
                         (ut/pp (database-sizes) {:width (Math/floor (/ :ww 5.66)) :color-scheme :color-map}))))}

         (= @selected-stats-page :sql)
         {:signal false
          :cache? false
          :type :clojure
          :input-map {}
          :data '(do (ns rvbbit-backend.websockets)
                     (let [tt :tt]
                       (with-out-str
                         (fig-render ":sql-stats" :bright-cyan)
                         (draw-client-stats nil [:freq] nil true (Math/floor (/ :ww 5.66)) {:metrics-atom sql-metrics}))))}

         (= @selected-stats-page :clients)
         {:signal false
          :cache? false
          :type :clojure
          :input-map {}
          :data '(do (ns rvbbit-backend.websockets)
                     (let [tt :tt]
                       (with-out-str
                         (fig-render ":clients" :bright-cyan)
                         (draw-client-stats nil [:freq] [:mem-mb :latency] false (Math/floor (/ :ww 5.66))))))}

         (= @selected-stats-page :help)
         {:signal false
          :cache? false
          :type :clojure
          :input-map {}
          :placeholder-on-running? true
          :data '(do (ns rvbbit-backend.websockets)
                     (let [tt :tt]
                       (with-out-str
                         (fig-render ":keybindings" :pink)
                         
                         (rvbbit-backend.util/pp [["general context sensitive keybindings"
                                                   {:SPACEBAR "toggle floating editor"
                                                  :SHIFT-SPACEBAR "toggle sidepanel / history / kits / snapshots"
                                                  :TILDE "toggle 'hop bar' for quick commands to spawn runners / configured kits, etc"
                                                  :ESC "un-selects selected card - or opens this panel if nothing selected"
                                                  :W-A-S-D "move selected card block by block around the canvas"
                                                  :SHIFT-W-A-S-D "resize selected card block by block"
                                                  :TAB "cycle through next card on canvas"
                                                  :Q-E "cycle through each TAB on the canvas"
                                                  :CTRL-Z "undo"
                                                  :SHIFT-CTRL-Z "redo"
                                                  :L "toggle sub-query lineage lines"
                                                  :CTRL-S "save the current screen or flow (if flow panel is up)"
                                                  :P "peek at all cards by minimizing them some, allowing the lines to stand out"
                                                  :SHIFT-R-F "move a card one layer up or down, z-index for overlapping cards"
                                                  :CTRL-SPACEBAR "toggle flow panel / solver / signal / system panel"}]
                                                  :_
                                                  ["on the open canvas deck"
                                                   {:LEFT-CLICK "select card by clicking on it's header"
                                                   :RIGHT-CLICK "creates a new card (with a clover view by default)"
                                                   :MIDDLE-CLICK "creates a new parameters and param editor card on canvas"}]
                                                  :_
                                                  "Please pay respects to Lord Engelbart & use a proper mouse. Touchpad has not been tested or optimized for."] 
                                                 
                                                 {:width 90})
                         
                         ;(println " ")
                         (rvbbit-backend.util/pp "latest docs will be on the GitHub page - linked below...")
                         
                         ;(println "")
                         )))}

         (= @selected-stats-page :reactor)
         {:signal false
          :cache? false
          :type :clojure
          :input-map {}
          :placeholder-on-running? true
          :data '(do (ns rvbbit-backend.websockets)
                     (let [tt :tt]
                       (with-out-str
                         (fig-render ":reactor" :pink)
                         (draw-stats [:reactions  :signal-reactions :watchers :subs :subs-client :clover-params] [:freq] false (Math/floor (/ :ww 5.66)) true)
                         (println "")
                         (show-reactor-sizes-report {:width (Math/floor (/ :ww 5.66)) :color-scheme :color-map}))))}

         (= @selected-stats-page :system)
         {:signal false
          :cache? false
          :type :clojure
          :input-map {}
          :data '(do (ns rvbbit-backend.websockets)
                     (let [tt :tt]
                       (with-out-str
                         (fig-render ":system-stats" :bright-cyan) ;; :solvers :nrepl-calls :websockets
                         (draw-stats [:cpu :mem :threads :clients :flows :solvers :nrepl-calls] [:freq] false (Math/floor (/ :ww 5.66)) true)
                         ;(draw-stats [:cpu :mem :threads :clients :flows :solvers :nrepl-calls :websockets :load] [15] false 200 true)
                         )))}

         (= @selected-stats-page :queues)
         {:signal false
          :cache? false
          :type :clojure
          :input-map {}
          :data '(do (ns rvbbit-backend.websockets)
                     (let [tt :tt]
                       (with-out-str
                         (fig-render ":queue-party" :bright-cyan)
                         (draw-stats [:workers :queues :queue-tasks] [:freq] false (Math/floor (/ :ww 5.66)) true)
                         (println "")
                         (let [ss (qp/get-queue-stats+)]
                           (ut/pp [:queue-party-stats+ ss] {:width (Math/floor (/ :ww 5.66)) :color-scheme :color-map})))))}

         :else {:signal false
                :cache? false
                :type :clojure
                :input-map {}
                :data '(do (ns rvbbit-backend.websockets)
                           (let [tt :tt]
                             (with-out-str
                               (fig-render ":pool-sizes" :bright-cyan)
                               (show-pool-sizes-report))))})])

(defn menu-page-item [w h client-name ttt text color]
  [stats-page w h client-name ttt (walk/postwalk-replace
                                   {:color (or color :bright-cyan)
                                    :text (str text)}
                                   {:signal false
                                    :cache? false
                                    :type :clojure
                                    :input-map {}
                                    :data '(do (ns rvbbit-backend.websockets)
                                               (with-out-str
                                                 (fig-render :text :color)))})])

(defonce hover-text (reagent/atom nil))

(defn doom-modal []
  (let [flipped (reagent/atom false)]
    (reagent/create-class
     {:component-did-mount
      (fn [this]
        (when (not @fanfare?)
          (ut/tracked-dispatch [::audio/text-to-speech11 :audio :speak
                                "/home/ryanr/hit.mp3"
                                true]))
        (js/setTimeout #(do (reset! flipped true)
                            (reset! fanfare? true))   
                       (if @fanfare? 1500 5000))) 
      :reagent-render
      (fn []
        (let [hh @(ut/tracked-subscribe [::subs/h])
              ww @(ut/tracked-subscribe [::subs/w])
              w (* ww 0.45)
              h (* hh 0.7)
              react! [@hover-text]
              audio-playing? @(ut/tracked-sub ::audio/audio-playing? {})
              client-name @(ut/tracked-subscribe [::bricks/client-name])
              left (- (/ ww 2) (/ w 2))
              top (- (/ hh 2) (/ h 2))]
          [re-com/modal-panel
           :style {:z-index 99999 :padding "0px"}
           :frame-style {:background-color "#000000" :border-radius "20px"}
           :parts {:child-container
                   {:style {:left left
                            :width w
                            :height h
                            :top top
                            :font-size "8px"
                            :background-color "#000000"
                            :position "fixed"
                            :box-shadow (let [block-id :audio
                                              talking-block? true]
                                          (when (and audio-playing? talking-block?)
                                            (str "1px 1px " (px (* 80
                                                                   (+ 0.1
                                                                      (- (get @db/audio-data2 block-id)
                                                                         (get @db/audio-data block-id)))))
                                                 " cyan")))
                            :border "5px solid cyan"
                            :transition "box-shadow 0.2s"
                            ;; :transform (let [block-id :audio
                            ;;             talking-block? true]
                            ;;         (when (and audio-playing? talking-block? (not @flipped)) 
                            ;;           (str "scale(" (max (min 1.1 (+ 0.1
                            ;;                                          (- (get @db/audio-data2 block-id)
                            ;;                                             (get @db/audio-data block-id)))) 1.6) ")")))
                            :perspective "1000px"  ; Add perspective for 3D effect
                            }}}
           :backdrop-opacity 0.75
           :backdrop-on-click #(ut/tracked-dispatch [::bricks/disable-doom-modal])
           :child  [re-com/v-box
                    :style {:width w
                            :height h
                            :position "relative"
                            :transition "transform 0.3s"
                            :transform-origin "top center"
                            :transform-style "preserve-3d"
                             ;:border (when @flipped "1px solid cyan")
                            :transform (if @flipped "rotateY(180deg)" "rotateY(0deg)")}
                    :children [[:div {:style {:position "absolute"
                                              :width w
                                              :height h
                                              :backface-visibility "hidden"}}
                                [:img {:src "images/rvbbit-logo-no-bkgrnd.png"
                                       :style {:width w
                                               :height h
                                               :object-fit "contain"
                                               :object-position "center"
                                               :max-width "100%"
                                               :max-height "100%"}}]]
                               [re-com/v-box
                                :justify :between
                                :style {:width w
                                        :height h
                                         ;:border "1px solid lime"
                                        :backface-visibility "hidden"
                                        :transform "rotateY(180deg)"
                                        :font-size "24px"}
                                :children [(let [ttt @(ut/tracked-sub ::conn/clicked-parameter-key-alpha {:keypath [:time/minute]})
                                                 react! [@time-freq]]
                                             [main-stats-page w h client-name ttt])
                                           [re-com/h-box
                                            :width (px (- w 40))
                                            :height "20px"
                                            :justify :between
                                            :align :center
                                            :style {:font-size "18px"
                                                    ;:border "1px solid lime"
                                                    :font-family "Fixedsys-Excelsior, monospace"}
                                            :children [(if @hover-text
                                                         [re-com/box
                                                          :style {:margin-top "4px"
                                                                  :text-shadow "2px 3px 5px #00000099"
                                                                  :color "darkcyan"}
                                                          :child (str @hover-text)]
                                                         [re-com/h-box
                                                          :gap "10px"
                                                          :style {:margin-top "4px"
                                                                  :text-shadow "2px 3px 5px #00000099"
                                                                  :color "darkcyan"}
                                                          :children [[re-com/box :child "RVBBIT"]
                                                                     [re-com/box :child (str db/version)]
                                                                     [re-com/box :child (str db/version-date)]]])
                                                       ;[re-com/gap :size "16px"]
                                                       [re-com/h-box
                                                        :gap "10px"
                                                        :style {:font-weight 700
                                                                :text-shadow "2px 3px 5px #00000099"
                                                                :color "brightcyan"
                                                                :margin-top "4px"}
                                                        :children [[re-com/box
                                                                    :attr {:on-mouse-over #(reset! hover-text "https://rvbbit.com")
                                                                           :on-mouse-out #(reset! hover-text nil)}
                                                                    :child [:a {:href "https://rvbbit.com"
                                                                                :target "_blank"
                                                                                :rel "noopener noreferrer"
                                                                                :style {:color "cyan"
                                                                                        :text-decoration "none"}}
                                                                            "rvbbit.com"]]
                                                                   
                                                                   [re-com/box
                                                                    :attr {:on-mouse-over #(reset! hover-text "https://twitter.com/ryrobes")
                                                                           :on-mouse-out #(reset! hover-text nil)}
                                                                    :child [:a {:href "https://twitter.com/ryrobes"
                                                                                :target "_blank"
                                                                                :rel "noopener noreferrer"
                                                                                :style {:color "cyan"
                                                                                        :text-decoration "none"}}
                                                                            "@ryrobes"]]
                                                                   
                                                                   [re-com/box
                                                                    :child [:a {:href "https://github.com/ryrobes/rvbbit"
                                                                                :target "_blank"
                                                                                :rel "noopener noreferrer"
                                                                                :style {:color "darkcyan"
                                                                                        :text-decoration "none"}}
                                                                            [re-com/md-icon-button :src (at)
                                                                             :md-icon-name "zmdi-github-alt"
                                                                             :attr {:on-mouse-over #(reset! hover-text "https://github.com/ryrobes/rvbbit")
                                                                                    :on-mouse-out #(reset! hover-text nil)}
                                                                             :style {:color        "darkcyan"
                                                                                     :z-index      99999
                                                                                     :cursor       "pointer"
                                                                                     :margin-top   "-3px"
                                                                                     :font-size    "15px"}]]]
                                                                   [re-com/box
                                                                    :child [:a {:href "https://youtube.com/ryrobes"
                                                                                :target "_blank"
                                                                                :rel "noopener noreferrer"
                                                                                :style {:color "darkcyan"
                                                                                        :text-decoration "none"}}
                                                                            [re-com/md-icon-button :src (at)
                                                                             :md-icon-name "zmdi-youtube-play"
                                                                             :attr {:on-mouse-over #(reset! hover-text "https://youtube.com/ryrobes")
                                                                                    :on-mouse-out #(reset! hover-text nil)}
                                                                             :style {:color        "darkcyan"
                                                                                     :z-index      99999
                                                                                     :cursor       "pointer"
                                                                                     :margin-top   "-3px"
                                                                                     :font-size    "15px"}]]]
                                                                   [re-com/box
                                                                    :child [:a {:href "mailto:ryan.robitaille@gmail.com"
                                                                                :target "_blank"
                                                                                :rel "noopener noreferrer"
                                                                                :style {:color "darkcyan"
                                                                                        :text-decoration "none"}}
                                                                            [re-com/md-icon-button :src (at)
                                                                             :md-icon-name "zmdi-email"
                                                                             :attr {:on-mouse-over #(reset! hover-text "ryan.robitaille@gmail.com")
                                                                                    :on-mouse-out #(reset! hover-text nil)}
                                                                             :style {:color        "darkcyan"
                                                                                     :z-index      99999
                                                                                     :cursor       "pointer"
                                                                                     :margin-top   "-3px"
                                                                                     :font-size    "15px"}]]]
                                                                   
                                                                   
                                                                   [re-com/box
                                                                    :attr {:on-mouse-over #(reset! hover-text "https://ryrob.es")
                                                                           :on-mouse-out #(reset! hover-text nil)}
                                                                    :child [:a {:href "https://ryrob.es"
                                                                                :target "_blank"
                                                                                :rel "noopener noreferrer"
                                                                                :style {:color "cyan"
                                                                                        :text-decoration "none"}}
                                                                            "ryrob.es"]]

                                                                   [re-com/box
                                                                    :style {:cursor "crosshair"}
                                                                    :attr {:on-mouse-over #(reset! hover-text "(proudly built in Florida, USA)")
                                                                           :on-mouse-out #(reset! hover-text nil)}
                                                                    :child "🇺🇸🐊"]]]]]]]]]]))})))



(defn custom-icon-button [{:keys [icon-name tooltip on-click active? color class]}]
  (let [hovered? (reagent/atom false)]
    (fn [{:keys [icon-name tooltip on-click active? class color]}]
      [re-com/md-icon-button
       :src (at)
       :md-icon-name icon-name
       :class class
       :on-click (fn [e]
                   (on-click e)
                   (reset! hovered? false))  ; Reset hover state after click
       :style {:color (if @hovered?
                        (theme-pull :theme/editor-outer-rim-color nil)
                        (or color "#ffffff"))
               :width "35px"
               :height "28px"
               :cursor "pointer"
               :z-index (when @db/bar-hover-text 99999999)
               ;:border-radius (when (= icon-name "zmdi-labels") "20px 0px 0px 0px") 
               :text-shadow "2px 1px 3px #000000"
               :filter "drop-shadow(2px 1px 3px #000000)"
               :opacity (if active? 1.0 (if @hovered? 0.8 0.45))
               :padding-top (when (not @hovered?) "4px")
               :padding-left "2px"
               :font-size (if @hovered? "33px" "18px")
               :transition "all 0.3s ease"}
       :attr {:on-mouse-over (fn []
                               (reset! hovered? true)
                               (reset! db/bar-hover-text tooltip))
              :on-mouse-out (fn []
                              (reset! hovered? false)
                              (reset! db/bar-hover-text nil))}])))

(defn custom-text-display [{:keys [text tooltip color on-click]}]
  (let [hovered? (reagent/atom false)]
    (fn [{:keys [text tooltip color on-click]}]
      [re-com/box
       :attr {:on-mouse-over (fn []
                               (reset! hovered? true)
                               (reset! db/bar-hover-text tooltip))
              :on-mouse-out (fn []
                              (reset! hovered? false)
                              (reset! db/bar-hover-text nil))
              :on-click (when on-click
                          (fn [e]
                            (on-click e)
                            (reset! hovered? false)))}
       :style {:color (if @hovered?
                        (theme-pull :theme/editor-outer-rim-color nil)
                        (or color "#ffffff99"))
               :cursor (if on-click "pointer" "default")
               :min-width "35px"
               :height "28px"
               :font-weight 700
               :z-index (when @db/bar-hover-text 99999999)
               ;:display "inline-flex"
               ;:align-items "center"
               ;:justify-content "center"
               :text-shadow "2px 1px 3px #00000099"
               :filter "drop-shadow(2px 1px 3px #00000099)"
               :opacity (if @hovered? 1.0 0.8)
               ;:padding-top (when (not @hovered?) "4px")
               :margin-top (if (not @hovered?) "-2px" "-9px")
               :font-size (if @hovered? "30px" "18px")
               :transition "all 0.3s ease"
               :padding "5px"}
       :child [:span text]])))


(defn kit-canvas-icons [panel-key query-key]
  (let [valid-kits  @(ut/tracked-subscribe [::bricks/valid-kits-for {:panel-key panel-key :data-key query-key :location :canvas}])
        ;;_ (tapp>>  [:valid-kits valid-kits])
        block-runners  @(ut/tracked-sub ::bricks/block-runners {})
        client-name    @(ut/tracked-sub ::bricks/client-name {})]
    (vec (for [e valid-kits
               :let [icon (get-in block-runners [(first e) :kits (last e) :icon])
                     tooltip (str (get-in block-runners [(first e) :kits (last e) :tooltip] "(missing tooltip)"))
                     kit-runner-key (str "kit-runner" (hash (str client-name panel-key query-key (first e) (last e))))
                     running-key  (keyword (str "kit-status/" kit-runner-key ">running?"))
                     output-key   (keyword (str "kit/" kit-runner-key ">incremental"))
                     running?     @(ut/tracked-sub ::conn/clicked-parameter-key-alpha {:keypath [running-key]})
                     curr-tab      @(ut/tracked-sub ::bricks/selected-tab {})
                     trig!        [@bricks/waiting?]
                     class        (cond
                                    running? "rotate linear infinite"
                                    (get @bricks/waiting? kit-runner-key) "rotate-reverse linear infinite"
                                    :else "")]]
           [custom-icon-button
            {:icon-name icon ;;(if lines? "zmdi-layers" "zmdi-layers-off")
             :tooltip tooltip
             :class class
             :on-click (fn []
                         (when (not running?)
                           (swap! db/kit-run-ids assoc (keyword kit-runner-key) (ut/generate-uuid))
                           (swap! bricks/waiting? assoc kit-runner-key true)
                           (swap! bricks/temp-extra-subs conj running-key)
                           (swap! bricks/temp-extra-subs conj output-key)
                           (swap! db/kit-fn-lookup assoc [panel-key query-key] running-key)
                           (let [fstr (str "kit-runner " kit-runner-key)
                                 w    (/ (count fstr) 4.1)]
                             (ut/tracked-dispatch
                              [::wfx/push   :default
                               {:kind       :run-kit
                                :kit-keypath e
                                :kit-runner-key kit-runner-key
                                :panel-key   panel-key
                                :data-key    query-key
                                :tab-name    curr-tab
                                :runner      :*
                                :client-name client-name
                                :ui-keypath     [:panels panel-key :queries query-key]}])
                             (ut/dispatch-delay 800 [::http/insert-alert fstr w 1 5])
                             (ut/tapp>> [:clicked-kit running-key])
                             (js/setTimeout #(swap! bricks/waiting? assoc kit-runner-key false) 5000))))
             :active? false}]))))


(defn button-panels [editor? lines? peek? auto-run? external? selected-block? online?
                     websocket-status server-subs things-running flow-watcher-subs-grouped
                     client-name screen-name]
  (let [things-running (vec (distinct (filter #(cstr/includes? (str %) ">running?") things-running)))
        oai-valid?      @(ut/tracked-sub ::openai-key-valid? {})
        left [re-com/h-box
              :style {:position "fixed"
                      :z-index 98
                      :bottom 0
                      :left 0
                      :background-color (when @db/bar-hover-text 
                                          ;;"#00000099"
                                          (theme-pull :theme/base-block-color "#00000099")
                                          )
                      :backdrop-filter (when @db/bar-hover-text "blur(4px) brightness(0.35)")
                     ; :background-color "#00000022"
                      :color "white"}
              ;:gap "5px"
              :height "28px"
              :children (into
                         [[custom-icon-button
                           {:icon-name (if lines? "zmdi-layers" "zmdi-layers-off")
                            :tooltip "show lineage / sub-query lines? (L)"
                            :on-click #(ut/tracked-dispatch [::bricks/toggle-lines])
                            :active? lines?}]

                          [custom-icon-button
                           {:icon-name (if peek? "zmdi-eye" "zmdi-eye-off")
                            :tooltip "toggle peek mode (P)"
                            :on-click #(ut/tracked-dispatch [::bricks/toggle-peek])
                            :active? peek?}]

                          [custom-icon-button
                           {:icon-name (if auto-run? "zmdi-refresh-sync" "zmdi-refresh-sync-off")
                            :tooltip "toggle auto-refresh (O)"
                            :on-click #(ut/tracked-dispatch [::bricks/toggle-auto-run])
                            :active? auto-run?}]

                          [custom-icon-button
                           {:icon-name "zmdi-developer-board"
                            :tooltip "toggle external editing from ./live/* (experimental! 💀)"
                            :on-click #(ut/tracked-dispatch [::bricks/toggle-external])
                            :active? external?
                            :color (if external? "red" "#ffffff")}]

                          (when selected-block?
                            [custom-icon-button
                             {:icon-name "zmdi-close"
                              :tooltip "un-select block (ESC)"
                              :on-click #(ut/tracked-dispatch [::bricks/select-block "none!"])
                              :active? selected-block?}])

                          [custom-text-display
                           {:text (str (get websocket-status :waiting -1))
                            :tooltip "websocket requests waiting (sql queries, etc)"}]

                          [custom-text-display
                           {:text (ut/nf (count server-subs))
                            :tooltip "active reactor subscriptions"}]

                          [custom-text-display
                           {:text (ut/nf (count things-running))
                            :tooltip (str "active running (solvers, flows, etc), " (ut/nf (count things-running)))}]

                          (when (> (count things-running) 0)
                            [custom-icon-button
                             {:icon-name "zmdi-refresh"
                              :tooltip (str "running: " things-running)
                              :active? false
                              :class "rotate linear infinite"
                              :color (theme-pull :theme/editor-outer-rim-color nil)}])

                          (when (not online?)
                            [custom-text-display
                             {:text "RVBBIT server is offline"
                              :tooltip "RVBBIT server is offline"}])

                          (when (ut/ne? flow-watcher-subs-grouped)
                            [re-com/h-box
                             :style {:padding-left "10px" :font-size "16px" :font-weight 700}
                             :gap "8px"
                             :children (for [[kk cnt] flow-watcher-subs-grouped]
                                         [re-com/h-box :style
                                          {:background-color (theme-pull :theme/editor-outer-rim-color nil)
                                           :padding-left     "6px"
                                           :border-radius    "5px 5px 0px 0px"
                                           :padding-right    "6px"
                                           :color            (theme-pull :theme/editor-background-color nil)}
                                          :gap "5px"
                                          :children [[re-com/box :child (str kk)]
                                                     [re-com/box :style {:opacity 0.5} :child (str cnt)]
                                                     [re-com/md-icon-button
                                                      :md-icon-name "zmdi-close"
                                                      :style {:cursor "pointer"
                                                              :color (ut/choose-text-color (theme-pull :theme/editor-outer-rim-color nil))
                                                              :font-size "18px"
                                                             ;:height "10px" 
                                                             ;:margin-top "-3px"
                                                              }:on-click
                                                      #(do (ut/tapp>> [:remove-flow-watchers client-name kk])
                                                           (ut/tracked-dispatch [::wfx/request :default
                                                                                 {:message {:kind :remove-flow-watcher :client-name client-name :flow-id kk}
                                                                                  :timeout 50000}]))]]])])

                          (when oai-valid?
                            [custom-icon-button
                             (let [audio-recording? @(ut/tracked-subscribe_ [::audio/recording?])]
                               {:icon-name (if audio-recording? "ri-mic-line" "ri-mic-fill")
                                :tooltip "transcribe voice to :param/voice"
                                :on-click (if audio-recording?
                                            #(ut/tracked-dispatch [::audio/stop-recording])
                                            #(ut/tracked-dispatch [::audio/start-recording]))
                                :active? audio-recording?
                                :color (if audio-recording? "red" "#ffffff")})])

                          (when @(ut/tracked-subscribe_ [::audio/audio-playing?])
                            [custom-icon-button
                             {:icon-name "zmdi-speaker"
                              :tooltip "stop currently playing audio"
                              :on-click #(audio/stop-audio)
                              :active? true
                              :color "red"}])

                          ;; (when @(ut/tracked-subscribe_ [::audio/recording?])
                          ;;   [custom-icon-button
                          ;;    {:icon-name "zmdi-mic"
                          ;;     :tooltip "recording audio in progress"
                          ;;     :on-click nil
                          ;;     :active? true
                          ;;     :color "red"}])
                          ]

                         (kit-canvas-icons :* :*))]

        memory (let [mem @(ut/tracked-subscribe_ [::bricks/memory])]
                 (when (> (get mem 1) 0) ;; firefox, etc doesn't support this, so not point to render 0MB
                   (str (ut/bytes-to-mb (get mem 1)))))

        right [re-com/h-box
               :height "28px"
               :style {:position "fixed"
                       :z-index 98
                       :bottom 0
                       :right 0
                       :border-radius "15px 0px 0px 0px"
                       :background-color (when @db/bar-hover-text 
                                           ;;"#00000099"
                                           (theme-pull :theme/base-block-color "#00000099")
                                           )
                       :backdrop-filter (when @db/bar-hover-text "blur(4px) brightness(0.35)")
                       :color "white"}
               :children [[custom-icon-button
                           (let [alert-mute? @(ut/tracked-sub ::bricks/alert-mute? {})]
                             {:icon-name (if alert-mute? "zmdi-notifications-none" "zmdi-notifications")
                              :tooltip "mute alert pop-ups / status notifications (M)"
                              :on-click #(ut/tracked-dispatch [::bricks/toggle-alert-mute])
                              :active? alert-mute?})]

                          [custom-icon-button
                           {:icon-name "zmdi-labels"
                            :tooltip "toggle display mode (reduced ui for presenting)"
                            :on-click #(ut/tracked-dispatch [::bricks/toggle-no-ui])
                            :active? @(ut/tracked-sub ::bricks/full-no-ui? {})}]

                          [custom-icon-button
                           {:icon-name "zmdi-refresh"
                            :tooltip "refresh all visible SQL queries"
                            :on-click #(ut/tracked-dispatch [::bricks/refresh-all])}]

                          [custom-icon-button
                           {:icon-name "zmdi-save"
                            :tooltip "save board (Ctrl-S)"
                            :on-click #(ut/tracked-dispatch [::http/save :skinny screen-name])}]


                          ;; [re-com/md-icon-button :src (at) :md-icon-name "zmdi-labels" :tooltip "toggle display mode" :on-click
                          ;;  #(ut/tracked-dispatch [::bricks/toggle-no-ui]) :style
                          ;;  {:color        "#ffffff"
                          ;;   :cursor       "pointer"
                          ;;   :opacity      (if @(ut/tracked-sub ::bricks/full-no-ui? {}) 1.0 0.3)
                          ;;   :margin-top   "-2px"
                          ;;   :padding-left "2px"
                          ;;   :font-size    "15px"}]

                          ;; [re-com/md-icon-button
                          ;;  :src (at)
                          ;;  :md-icon-name "zmdi-refresh"
                          ;;  :tooltip "refresh all"
                          ;;  :on-click #(do (ut/tracked-dispatch [::bricks/refresh-all]))
                          ;;  :style {:color "#ffffff"
                          ;;          :cursor "pointer"
                          ;;          :margin-top "-2px"
                          ;;          :padding-left "2px"
                          ;;          :font-size "15px"}]

                          ;; [re-com/md-icon-button :src (at) :md-icon-name "zmdi-save" :tooltip "save board (Ctrl-S)" :on-click
                          ;;  #(do (ut/tracked-dispatch [::http/save :skinny screen-name])) :style
                          ;;  {:color "#ffffff" :cursor "pointer" :margin-top "-2px" :padding-left "2px" :font-size "15px"}]
                          ]
                           
                           ]]

    [re-com/v-box
     :children [left
                [re-com/box
                 :size "none"
                 :style {:position "fixed"
                         :padding (if @db/bar-hover-text "10px 10px 10px 20px" "5px")
                         :border-radius "0px 20px 20px 0px"
                         :left 0
                         :bottom 28
                         :font-weight 700
                         :z-index (when @db/bar-hover-text 99999999)
                         :background-color (when @db/bar-hover-text 
                                             ;;"#00000099"
                                             (theme-pull :theme/base-block-color "#00000099")
                                             )
                         :backdrop-filter (when @db/bar-hover-text "blur(4px) brightness(0.35)")
                         :font-size (when @db/bar-hover-text "27px")
                         :transition "all 0.3s ease"
                         :text-shadow "2px 1px 3px #000000"
                         :color "#ffffff99"}
                 :child (str (if @db/bar-hover-text 
                          @db/bar-hover-text memory))]
                right]]))





(defn main-panel []
  (let [editor? (and @(ut/tracked-subscribe_ [::bricks/editor?]) 
                     (not @bricks/dragging?))
        alert-mute? @(ut/tracked-sub ::bricks/alert-mute? {})
        buffy? @(ut/tracked-subscribe_ [::bricks/buffy?])
        console? @(ut/tracked-subscribe_ [::bricks/quake-console?])
        flows? @(ut/tracked-subscribe_ [::bricks/flow?])
        external? @(ut/tracked-subscribe_ [::bricks/external?])
        session? @(ut/tracked-subscribe_ [::bricks/session-modal?])
        doom? @(ut/tracked-subscribe_ [::bricks/doom-modal?])
        lines? @(ut/tracked-subscribe_ [::bricks/lines?])
        peek? @(ut/tracked-subscribe_ [::bricks/peek?])
        auto-run? @(ut/tracked-subscribe_ [::bricks/auto-run?])
        rekt [@db/kick-alert @editor-size] ;; a top binding kick for atom reactions 
        wssk @(ut/tracked-subscribe_ [::http/websocket-status])
        websocket-status (select-keys wssk [:status :datasets :panels :waiting])
        online? (true? (= (get websocket-status :status) :connected))
        mouse-active?  (or @(ut/tracked-sub ::bricks/is-mouse-active-alpha? {:seconds 60}) (not online?))
        hh @(ut/tracked-subscribe_ [::subs/h])
        ww @(ut/tracked-subscribe_ [::subs/w])
        selected-block @(ut/tracked-subscribe_ [::bricks/selected-block])
        selected-tab @(ut/tracked-subscribe_ [::bricks/selected-tab])
        selected-block? (true? (not (or (nil? selected-block) (= selected-block "none!"))))
        screen-name (ut/safe-name @(ut/tracked-subscribe_ [::bricks/screen-name]))
        client-name @(ut/tracked-subscribe_ [::bricks/client-name])
        flow-watcher-subs-grouped @(ut/tracked-subscribe_ [::bricks/flow-watcher-subs-grouped])
        server-subs @(ut/tracked-subscribe_ [::bricks/all-server-subs])
        things-running @(ut/tracked-sub ::bricks/things-running {})
        coords (if lines? ;; wicked expensive otherwise
                 (let [;;_ (ut/tapp>> [:lines!])
                       ;;subq-mapping @(ut/tracked-sub ::bricks/subq-mapping-alpha {})
                       ;;_ (ut/tapp>> [:subq-mapping (str subq-mapping)])
                       ;;dwn-from-here (vec (ut/cached-downstream-search subq-mapping selected-block))
                       ;;up-from-here (vec (ut/cached-upstream-search subq-mapping selected-block))
                       ;;involved (vec (distinct (into dwn-from-here up-from-here))) ;; anything on the path
                       ;;subq-blocks @(ut/tracked-sub ::bricks/subq-panels-alpha {:panel-id selected-block}) ;; direct parent? 
                       ;;subq-blocks @(ut/tracked-subscribe [::bricks/subq-panels selected-block])
                       panel-relations @(ut/tracked-sub ::bricks/panel-relations {})
                       smap (get panel-relations :b2b)
                       involved (vec (conj (into
                                            (get-in panel-relations [:full-relations selected-block :downstream] [])
                                            (get-in panel-relations [:full-relations selected-block :upstream] [])) selected-block))
                       direct-parents (get-in panel-relations [:direct-parents selected-block] [])
                      ;;  smap (into {} (for [b (keys subq-mapping)] 
                      ;;                  {b (ut/cached-downstream-search subq-mapping b)}))
                      ;;  smap @(ut/tracked-sub ::bricks/panel-relations {})
                      ;;  _ (ut/tapp>> [:smap smap :down dwn-from-here :up up-from-here :subq-blocks subq-blocks]) 
                      ;; _ (ut/tapp>> [:subq-mapping subq-mapping])
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
                                                  t2        @(ut/tracked-subscribe [::bricks/what-tab dd])
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
                                                       (cond (= dd selected-block) ;; am I me?
                                                       ;;"#9973e0"
                                                             (theme-pull :theme/universal-pop-color "#9973e0")

                                                            ;; (some #(= % dd) subq-blocks) "#e6ed21" ;; direct parent-of-selected

                                                            ;;  (some #(= % dd) (ut/cached-upstream-search subq-mapping selected-block))
                                                            ;;  "#7be073" ;; upstream?
                                                            ;;  (some #(= % dd) (ut/cached-downstream-search subq-mapping selected-block))
                                                            ;;  "#05dfff" ;; downstream?

                                                             (some #(= % dd) direct-parents) "#e6ed21" ;; direct parent-of-selected

                                                             (some #(= % dd) (get-in panel-relations [:full-relations selected-block :upstream] []))
                                                             "#7be073" ;; upstream?
                                                             (some #(= % dd) (get-in panel-relations [:full-relations selected-block :downstream] []))
                                                             "#05dfff" ;; downstream?

                                                             :else "orange")
                                                       k d nil]))))))))))]
                   ;;(ut/tapp>> [:lines!   lmap])
                   lmap)
                 [])]
    (bricks/droppable
      (if @rvbbit-frontend.flows/dragging-port? ["meta-menu" :flow-port] ["meta-menu" ]) ;; eyes
      [(js/Math.floor (/ (ifnil (first @db/context-modal-pos) 0) db/brick-size))
       (js/Math.floor (/ (ifnil (last @db/context-modal-pos) 0) db/brick-size))]
      [re-com/box
       :style {:background-color "black"}
       :attr {:id              "base-canvas"
              :on-drag-over    #(bricks/tag-screen-position %)
              :on-context-menu (re-com/handler-fn #_{:clj-kondo/ignore [:unresolved-symbol]}
                                (bricks/tag-screen-position event) ;; event is magic in handler-fn
                                                  (when (and (not @bricks/over-block?) (not @bricks/over-flow?))
                                                    (bricks/insert-new-block
                                                     [(js/Math.floor (/ (first @db/context-modal-pos) db/brick-size))
                                                      (js/Math.floor (/ (last @db/context-modal-pos) db/brick-size))]
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
                                          (bricks/insert-new-block [(js/Math.floor (/ (first @db/context-modal-pos) db/brick-size))
                                                                    (js/Math.floor (/ (last @db/context-modal-pos) db/brick-size))]
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
                                      (.preventDefault %)))}
       :size "1"
       :width (px ww)
       :height (px hh)
       :child [re-com/v-box
               :size "1"
               :style (let [custom-map (theme-pull :theme/canvas-background-css nil)
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
                                                         (get custom-map :background-size))})))
               :children [(when mouse-active? [bricks/reecatch [tab-menu]])

                          (when mouse-active? [bricks/reecatch [snapshot-menu]])

                          (when @bricks/dragging-editor?
                            [bricks/reecatch [docker-edges (Math/floor (/ ww db/brick-size)) (Math/floor (/ hh db/brick-size))]])

                          (when session? [session-modal])

                          (when doom? [doom-modal])

                          (when (and editor? (not @bricks/mouse-dragging-panel?))
                            [bricks/reecatch [editor-panel (get @editor-size 0) (get @editor-size 1)]])

                          (when (or (and @bricks/dragging? (not @bricks/on-block?) (not @bricks/over-flow?)) @bricks/swap-layers?)
                            [re-com/box :child " " :style
                             {:background-color (str (theme-pull :theme/editor-background-color nil) 22) ;; "#00000022"
                              :filter           "drop-shadow(16px 16px 20px red) invert(75%)"
                              :position         "fixed"
                              :left             (* (js/Math.floor (/ (first @db/context-modal-pos) db/brick-size)) db/brick-size)
                              :top              (* (js/Math.floor (/ (last @db/context-modal-pos) db/brick-size)) db/brick-size)} :width
                             (px (* (get @bricks/dragging-body :w) db/brick-size)) :height
                             (px (* (get @bricks/dragging-body :h) db/brick-size))])

                          (when (and buffy? (not @bricks/mouse-dragging-panel?)) [bricks/reecatch [buffy/chat-panel]])

                          (when flows? ;(and flows? (not @bricks/mouse-dragging-panel?))
                            [bricks/reecatch [flows/flow-panel]])

                          [bricks/reecatch [bricks/grid]]

                          (when mouse-active?
                            [bricks/reecatch [button-panels editor? lines? peek? auto-run? external? selected-block? online?
                                              websocket-status server-subs things-running flow-watcher-subs-grouped
                                              client-name screen-name]])

                          (when mouse-active? [bricks/reecatch [task-bar]])

                          (when console? [bricks/reecatch [quake-console ww]])

                          (when (not alert-mute?) [bricks/reecatch [flows/alert-box]])

                          (when (ut/ne? coords)
                            [:svg
                             {:style {:width          (px ww) ;"6200px" ;; big ass render nothing gets cut off
                                      :height         (px hh) ;"6200px"
                     ;:pointer-events "none"
                                      :z-index        8}}
                             (bricks/draw-lines coords)])]]])))


