(ns rvbbit-frontend.mad-libs
  (:require
   [clojure.string :as cstr]
   [re-com.core :as    re-com
    :refer [at]]
   [re-com.util :refer [px px-n]]
   [rvbbit-frontend.connections :as conn]
   [clojure.edn :as edn]
   [rvbbit-frontend.db :as db]
   [rvbbit-frontend.resolver :as resolver]
   [rvbbit-frontend.utility :as ut :refer [tapp>> pp]]
   [re-frame.core :as re-frame]
   [reagent.core :as reagent]))

(defonce mad-libs-view (reagent/atom nil))
(defonce mad-libs-top? (reagent/atom true))

(defn theme-pull-fn
  [cmp-key fallback & test-fn]
  (let [v                   @(ut/tracked-sub ::conn/clicked-parameter-key-alpha {:keypath [cmp-key]})
        t0                  (ut/splitter (str (ut/safe-name cmp-key)) #"/")
        t1                  (keyword (first t0))
        t2                  (keyword (last t0))
        self-ref-keys       (into #{} (filter namespace) (ut/deep-flatten db/base-theme))
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
        fallback0           (if theme-key? (get resolved-base-theme t2) fallback)
        rs                  (fn [edn] (resolver/logic-and-params edn :theme-pull))]
    (rs (if (not (nil? v)) v fallback0))))

(re-frame/reg-sub ::theme-pull-sub (fn [_ {:keys [cmp-key fallback test-fn]}] (theme-pull-fn cmp-key fallback test-fn)))

(defn theme-pull [cmp-key fallback & test-fn] @(ut/tracked-sub ::theme-pull-sub {:cmp-key cmp-key :fallback fallback :test-fn test-fn}))

(defn mad-libs-shapes
  [query-id width-int height-int]
  (let [;all-sql-call-keys @(ut/tracked-subscribe [::all-sql-call-keys])
        client-name    @(ut/tracked-sub ::client-name {})
        parent-panel-key @(ut/tracked-subscribe [::lookup-panel-key-by-query-key query-id])
        [h w] @(ut/tracked-subscribe [::size parent-panel-key])
        height-int (- height-int 42)
        width-int (- width-int 10)
        recos-page @(ut/tracked-subscribe [::recos-page2])
        query-id (ut/replacer (ut/replacer (str query-id) #":" "") "-" "_")
        sql-params (merge (into {}
                                (for [k [:viz-shapes0-sys2/shape :viz-shapes-sys2/combo_edn :user-dropdown-sys2/req-field]]
                                  {k @(ut/tracked-sub ::conn/clicked-parameter-key-alpha {:keypath [k]})}))
                          {:viz-tables-sys2/table_name query-id})
        sql-params-minus (dissoc sql-params :viz-tables-sys2/table_name) ;; (ut/tracked-dispatch
        combo-picked? (not (nil? (get sql-params :viz-shapes-sys2/combo_edn)))
        shape-picked? (not (nil? (get sql-params :viz-shapes0-sys2/shape)))
        req-field-picked? (not (nil? (get sql-params :user-dropdown-sys/req-field)))
        sql-calls {:viz-tables-sys2  {:select   [:table_name [[:count 1] :recos]]
                                      :from     [:viz_recos_vw]
                                      :where    [:and [:not [:like :table_name "query_preview%"]] [:= :table_name query-id]]
                                      :order-by [:table_name]
                                      :group-by [:table_name]}
                   :viz-shapes0-sys2 {:select   [[:shape_name :shape] [[:count 1] :recos]]
                                      :from     [[:viz_recos_vw :vvt]]
                                      :where    [:and [:= :table_name :viz-tables-sys2/table_name]]
                                      :group-by [:shape_name]}
                   :viz-shapes-sys2  {:select   [:combo_edn [[:count 1] :recos]]
                                      :from     [[:viz_recos_vw :vvt]]
                                      :where    [:and
                                                 (when (not (nil? (get sql-params :viz-shapes0-sys/shape)))
                                                   [:= :shape_name :viz-shapes0-sys/shape])
                                                 (when req-field-picked?
                                                   [:like :combo_edn (str "%" (get sql-params :user-dropdown-sys/req-field) "%")])
                                                 [:= :table_name :viz-tables-sys2/table_name]] ;; :viz-tables-sys/table_name-name
                                      :group-by [:combo_edn]}
                   :recos-sys2       (if (not @mad-libs-top?)
                                       {:select   [:*]
                                        :from     [:viz_recos_vw]
                                        :order-by [[:score :desc]]
                                        :where    [:and [:= :table_name query-id]
                                                   (when combo-picked? [:= :combo_edn :viz-shapes-sys2/combo_edn])
                                                   (when shape-picked? [:= :shape_name :viz-shapes0-sys2/shape])]}
                                       {:select   [:*]
                                        :from     (if (or combo-picked? shape-picked?) [:viz_recos_vw] [:viz_recos_vw2])
                                        :order-by [[:score :desc]]
                                        :where    [:and [:= :table_name query-id]
                                                   (when combo-picked? [:= :combo_edn :viz-shapes-sys2/combo_edn])
                                                   (when shape-picked? [:= :shape_name :viz-shapes0-sys2/shape])]})}
        full-recos @(ut/tracked-sub ::conn/sql-data-alpha {:keypath [:recos-sys2]})
        recos-count (count full-recos)
        pkeys @(ut/tracked-subscribe [::preview-keys2])
        preview-keys (vec (for [k pkeys] (keyword (str "reco-preview" k))))
        preview-maps (into {}
                           (for [{:keys [combo_hash shape_name combo_edn query_map score w h selected_view viz_map condis]}
                                 full-recos]
                             {(keyword (str "reco-preview" combo_hash)) {:shape_name    shape_name
                                                                         :query_map     query_map
                                                                         :combo_hash    combo_hash
                                                                         :selected_view selected_view
                                                                         :w             w
                                                                         :h             h
                                                                         :score         score
                                                                         :viz_map       viz_map
                                                                         :condis        condis
                                                                         :combo_edn     combo_edn}}))
        charts-wide (js/Math.floor (/ w 7))
        charts-high (js/Math.floor (/ h 4.5))
        h-multi 2.2 ;; offsets for css zoom math fuckery
        w-multi 2.3
        preview-container
        (fn [preview-maps pk pnum]
          (try
            (let [panel-key (nth pk pnum)
                  ce (get-in preview-maps [panel-key :combo_edn])
                  color-keys [:YlGn :desc]
                  color-keys-which (last (reverse (sort-by name (keys (get ut/colorbrewer (first color-keys))))))
                  colors1 (vec (if (= (last color-keys) :desc)
                                 (reverse (get-in ut/colorbrewer [(first color-keys) color-keys-which]))
                                 (get-in ut/colorbrewer [(first color-keys) color-keys-which])))
                  zero-color (if (= (last color-keys) :desc) (first colors1) (last colors1))
                  value-to-color (fn [value-vector color-vector value]
                                   (let [min-val    (apply min value-vector)
                                         max-val    (apply max value-vector)
                                         normalized (when (> (- max-val min-val) 0) ; Avoid
                                                      (/ (- value min-val) (- max-val min-val)))
                                         idx        (int (Math/floor (* normalized (dec (count color-vector)))))]
                                     (if (and normalized (>= idx 0) (< idx (count color-vector))) (color-vector idx) nil)))
                  selected-view (try (edn/read-string (get-in preview-maps [panel-key :selected_view])) (catch :default _ nil))
                  rounder (fn [num] (/ (js/Math.round (* num 100)) 100))
                  combo_edn (try (when (not (nil? ce)) (ut/replacer (str ce) #"\"" "")) (catch :default _ "")) ;; TODO, why
                                                                                                                 ;; bombing?
                  shape_name (get-in preview-maps [panel-key :shape_name])
                  score (get-in preview-maps [panel-key :score]) ;(js/Math.round (get-in
                  all-scores (map :score (map val preview-maps))
                  heat-color (value-to-color all-scores colors1 score)
                  heat-color (if (nil? heat-color) zero-color heat-color)
                  hh (js/Math.floor (* (/ (/ height-int charts-high) db/brick-size) h-multi))
                  ww (js/Math.floor (* (/ (/ width-int charts-wide) db/brick-size) w-multi))
                  body
                  [re-com/v-box
                   :size "none"
                   :width (px (* ww db/brick-size))
                   :height (px (* hh db/brick-size))
                   :style
                   {:zoom         0.44
                    :padding-left "10px"
                    :transform    "translate(0)" ;; a known CSS hack for fixed position
                    :cursor       "grab"
                    :border       (str "1px dashed " (theme-pull :theme/editor-outer-rim-color nil) 44)
                    :overflow     "auto"} :justify :between :children
                   [[re-com/h-box :justify :between :children
                     [[re-com/h-box :gap "5px" :style {:padding-left "3px"} :children
                       [[re-com/box :size "none" :child " " :style
                         {:background-color heat-color :width "26px" :height "26px" :border-radius "12px" :margin-top "13px"}]
                        [re-com/box :size "auto" :height "40px" :style
                         {:padding-left  "10px"
                          :padding-top   "4px"
                          :font-family   (theme-pull :theme/base-font nil)
                          :padding-right "14px"
                          :z-index       105
                          :filter        "drop-shadow(0 0 0.9rem black)"
                          :font-weight   400
                          :font-size     "30px"} :attr
                         {:on-double-click #(do (ut/tracked-dispatch [::set-recos-page2 0])
                                                (ut/tracked-dispatch [::conn/click-parameter [:viz-shapes-sys2 :combo_edn]
                                                                      ce]))} :child
                         (str ;panel-key " " pnum
                          combo_edn)]]]
                      [re-com/box :padding "6px" :style {:font-size "20px" :opacity 0.3 :font-weight 700} :child
                       (str (rounder score))]]] [clover panel-key (or selected-view :oz) ww hh]
                    [re-com/box :size "auto" :height "40px" :justify :end :align :end :style
                     {:padding-left   "14px"
                      :padding-right  "10px"
                      :padding-bottom "4px"
                      :font-family    (theme-pull :theme/base-font nil)
                      :filter         "drop-shadow(0 0 0.9rem black)"
                      :z-index        105
                      :font-weight    400
                      :font-size      "29px"} :attr
                     {:on-double-click #(do (ut/tracked-dispatch [::set-recos-page2 0])
                                            (ut/tracked-dispatch [::conn/click-parameter [:viz-shapes0-sys2 :shape]
                                                                  shape_name]))} :child (str shape_name)]]]]
              [re-com/box :size "auto" :child (draggable (sql-spawner-meta panel-key) "meta-menu" body)])
            (catch :default _
              [re-com/box :height (px (* (js/Math.floor (* (/ (/ height-int charts-high) db/brick-size) h-multi)) db/brick-size))
               :width (px (* (js/Math.floor (* (/ (/ width-int charts-wide) db/brick-size) w-multi)) db/brick-size)) :size "auto"
               :align :center :justify :center :style
               {:color     (str (theme-pull :theme/editor-font-color nil) 22) ; "#ffffff22"
                :zoom      0.44
                :font-size "40px"} :child "n/a"]))) ;(str e)
        per-page (* charts-wide charts-high)
        pages (/ recos-count per-page)]
    (ut/tapp>> [:full-recos recos-page preview-maps preview-keys query-id full-recos (select-keys preview-maps preview-keys)])
    (dorun (let [prev-preview-keys (for [k @(ut/tracked-subscribe [::preview-keys2])] (keyword (str "reco-preview" k)))
                 grid-page         @(ut/tracked-subscribe [::recos-page2])
                 grid-page         (if (> (- grid-page 1) pages) 0 grid-page)
                 recos             (take per-page
                                         (drop (* grid-page per-page) @(ut/tracked-subscribe [::conn/sql-data [:recos-sys2]])))
                 recos-keys        (vec (for [{:keys [combo_hash]} recos] combo_hash))]
             (doseq [k prev-preview-keys] (ut/tracked-dispatch [::quick-delete-panel k]))
             (ut/tracked-dispatch [::set-preview-keys2 recos-keys])
             (doseq [{:keys [combo_hash query_map viz_map condis combo_edn shape_name]} recos]
               (insert-hidden-reco-preview combo_hash viz_map query_map condis combo_edn shape_name false))))
    (dorun (for [[k v] sql-calls]
             (let [query        (ut/postwalk-replacer sql-params v)
                   ;data-exists? @(ut/tracked-subscribe [::conn/sql-data-exists? [k]])
                   ;unrun-sql?   @(ut/tracked-subscribe [::conn/sql-query-not-run? [k] query])
                   data-exists?   @(ut/tracked-sub ::conn/sql-data-exists-alpha? {:keypath [k]})
                   unrun-sql?     @(ut/tracked-sub ::conn/sql-query-not-run-alpha? {:keypath [k] :query query})]
               (when (or (not data-exists?) unrun-sql?) (conn/sql-data [k] query client-name)))))
    [re-com/v-box :style {:margin-right "8px"} :height (px height-int) :width (px width-int) :children
     [[re-com/h-box :size "auto" :style {:color (theme-pull :theme/editor-font-color nil)} :children
       [(let []                                                ;; get the next 6 graphs and render?
          [re-com/h-box :children
           [[re-com/v-box :size "auto" :height (px height-int) ;"380px"
             :width (px width-int)                             ;"770px"
             :children
             (for [h (range charts-high)]
               [re-com/h-box :size "none" :children
                (for [w (range charts-wide)] [reecatch [preview-container preview-maps preview-keys (+ (* h charts-wide) w)]])])]
            [re-com/v-box :size "auto" :height (px height-int) ;"380px"
             :style {:border-top (str "1px solid " (theme-pull :theme/universal-pop-color "#9973e0") "66") :overflow "hidden"} :children
             [[re-com/box :child
               [re-com/md-icon-button :md-icon-name "zmdi-trending-up" :on-click
                #(do (clear-preview2-recos) (reset! mad-libs-top? (not @mad-libs-top?))) :style
                {:font-size "22px" :cursor "pointer" :opacity 0.5 :padding "0px" :margin-top "-1px"}] :height "20px" :style
               {:font-size        "12px"
                :color            (if @mad-libs-top?
                                    (theme-pull :theme/editor-background-color nil) ;;"#000000"
                                    (str (theme-pull :theme/editor-font-color nil)))
                :background-color (if @mad-libs-top?
                                    ;"#9973e0" ;"darkcyan"
                                    (theme-pull :theme/universal-pop-color "#9973e0")
                                    "inherit")} :align :center :justify :center]
              [re-com/v-box :children
               (for [[k v] sql-params-minus
                     :when (not (nil? v))
                     :let  [zmdi      (if (= k :viz-shapes0-sys2/shape) "zmdi-chart" "zmdi-map")
                            splts     (map keyword (ut/splitter (ut/replacer (str k) #":" "") #"/"))
                            splts-vec [(first splts) (last splts)]]]
                 [re-com/box :child
                  [re-com/md-icon-button :tooltip (str k) :md-icon-name zmdi :on-click
                   #(do (ut/tracked-dispatch [::set-recos-page2 0]) (ut/tracked-dispatch [::conn/click-parameter splts-vec nil])) ;; clear
                   :style {:font-size "22px" :cursor "pointer" :opacity 0.5 :padding "0px" :margin-top "-1px"}] :height "20px"
                  :style
                  {:font-size        "12px"
                   :background-color (if false ;@mad-libs-top?
                                       ;"#9973e0" ;"darkcyan"
                                       (theme-pull :theme/universal-pop-color "#9973e0")
                                       "inherit")} :align :center :justify :center])] [re-com/gap :size "2px"]
              [re-com/v-box :children
               (for [c (range pages)]
                 [re-com/box :child (str (+ 1 c)) ; (if (= c recos-page) (str (+ 1 c)) "..")
                  :height "19px" :align :end :justify :center :attr {:on-click #(ut/tracked-dispatch [::set-recos-page2 c])}
                  :style
                  {:padding-right    "4px"
                   :cursor           "pointer"
                   :font-size        "10px"
                   :color            (if (= c recos-page)
                                       (theme-pull :theme/editor-background-color nil) ;;"#000000"
                                       (str (theme-pull :theme/editor-font-color nil) 77))
                   :background-color (if (= c recos-page)
                                       ;"#9973e0" ;"darkcyan"
                                       (theme-pull :theme/universal-pop-color "#9973e0")
                                       "inherit")
                   :border-bottom    (str "1px dashed " (theme-pull :theme/universal-pop-color "#9973e0") "66")}])]] :width "30px"]]])]]]]))














