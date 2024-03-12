(ns rvbbit-frontend.mad-libs
  (:require
   [clojure.string :as cstr]))

(defn reco-type-grid [query-id]
  (let []))

;; (defn preview-browser [query-id]
;;   (let [all-sql-call-keys @(re-frame/subscribe [::all-sql-call-keys])
;;         grid-reco? @(re-frame/subscribe [::grid-reco?])
;;         recos-page @(re-frame/subscribe [::recos-page])
;;         all-sql-call-keys-str (for [e all-sql-call-keys] (cstr/replace (ut/unkeyword e) "-" "_"))
;;         sql-params (into {} (for [k [:viz-tables-sys/table_name :viz-shapes0-sys/shape :viz-tables-sys/table_name
;;                                      :viz-shapes0-sys/shape :viz-shapes-sys/combo_edn :user-dropdown-sys/req-field]]
;;                               {k @(re-frame/subscribe [::conn/clicked-parameter-key [k]])}))
;;         combo-picked? (not (nil? (get sql-params :viz-shapes-sys/combo_edn)))
;;         shape-picked? (not (nil? (get sql-params :viz-shapes0-sys/shape)))
;;         clear-filters-fn (fn [] (do (re-frame/dispatch [::conn/click-parameter [:viz-shapes0-sys :shape] nil])
;;                                     (re-frame/dispatch [::conn/click-parameter [:user-dropdown-sys :req-field] nil])
;;                                     (re-frame/dispatch [::conn/click-parameter [:viz-shapes-sys :combo_edn] nil])))
;;         req-field-picked? (not (nil? (get sql-params :user-dropdown-sys/req-field)))
;;         sql-calls {:viz-tables-sys {:select [:table_name [[:count 1] :recos]]
;;                                     :from [:viz_recos_vw]
;;                                     :where [:and [:not [:like :table_name "query_preview%"]]
;;                                             (let [lst (vec (cons :or (vec (for [t all-sql-call-keys-str] [:= :table_name t]))))]
;;                                               (if (> (count (flatten lst)) 1) lst false))]
;;                                     :order-by [:table_name]
;;                                     :group-by [:table_name]}
;;                    :viz-shapes0-sys {:select [[:shape_name :shape] [[:count 1] :recos]]
;;                                      :from [[:viz_recos_vw :vvt]]
;;                                      :where [:and [:= :table_name :viz-tables-sys/table_name]
;;                                              (when req-field-picked?
;;                                                [:like :combo_edn (str "%" (get sql-params :user-dropdown-sys/req-field) "%")])
;;                                              (when combo-picked?
;;                                                [:= :combo_edn :viz-shapes-sys/combo_edn])] ;; :viz-tables-sys/table_name-name
;;                                      :group-by [:shape_name]}
;;                    :viz-shapes-sys {:select [:combo_edn [[:count 1] :recos]]
;;                                     :from [[:viz_recos_vw :vvt]]
;;                                     :order-by [[:score :desc]]
;;                                     :where [:and
;;                                             (when (not (nil? (get sql-params :viz-shapes0-sys/shape)))
;;                                               [:= :shape_name :viz-shapes0-sys/shape])
;;                                             (when req-field-picked?
;;                                               [:like :combo_edn (str "%" (get sql-params :user-dropdown-sys/req-field) "%")])
;;                                             [:= :table_name :viz-tables-sys/table_name]] ;; :viz-tables-sys/table_name-name
;;                                     :group-by [:combo_edn]}
;;                    :recos-sys {:select [:*]
;;                                :from [:viz_recos_vw]
;;                                :order-by [[:score :desc]]
;;                                :where [:and
;;                                        [:= :table_name :viz-tables-sys/table_name]
;;                                        (when combo-picked?
;;                                          [:= :combo_edn :viz-shapes-sys/combo_edn])
;;                                        (when req-field-picked?
;;                                          [:like :combo_edn (str "%" (get sql-params :user-dropdown-sys/req-field) "%")])
;;                                        (when shape-picked?
;;                                          [:= :shape_name :viz-shapes0-sys/shape])]}}
;;         block-list @(re-frame.core/subscribe [::conn/sql-data [:viz-tables-sys]])
;;         combo-list @(re-frame.core/subscribe [::conn/sql-data [:viz-shapes-sys]])
;;         full-recos @(re-frame.core/subscribe [::conn/sql-data [:recos-sys]])
;;         ;current-tab @(re-frame/subscribe [::selected-tab])
;;         current-tab-queries  (try (map #(-> % ut/sql-keyword name) @(re-frame/subscribe [::current-tab-queries]))
;;                                   (catch :default _ []))
;;         block-list (vec (filter (fn [x] (some #(= % (get x :table_name)) current-tab-queries)) block-list)) ;; overwrite above with curr tab only
;;         recos-count (count full-recos)
;;         block-list-boxes (for [{:keys [table_name recos]} block-list
;;                                :let [sel (get sql-params :viz-tables-sys/table_name)
;;                                      sel? (= table_name sel)]]
;;                            [re-com/box
;;                             :attr {:on-click #(do
;;                                                 (re-frame/dispatch [::conn/click-parameter [:viz-tables-sys :table_name] table_name])
;;                                                 (clear-filters-fn))
;;                                    ;#(reset! viz-block table_name)
;;                                    }
;;                             :align :center :justify :center
;;                             :style {:border-top "1px solid #80008045"
;;                                     :border-left "1px solid #80008080"
;;                                     :font-weight 700
;;                                     :font-size "13px"
;;                                     :cursor "pointer"
;;                                     :color (if sel? (theme-pull :theme/editor-background-color nil) ;;"#000000" 
;;                                                ;;"#ffffff77"
;;                                                (str (theme-pull :theme/editor-font-color nil) 77))
;;                                     :background-color (if sel?
;;                                                         "#9973e0"
;;                                                         ;;"#000000"
;;                                                         (theme-pull :theme/editor-background-color nil))
;;                                     :padding-left "5px" :padding-right "5px"}
;;                             :child (str table_name " (" recos ")")])
;;         combo-singles (vec (distinct (flatten (doall (for [{:keys [combo_edn recos]} combo-list]
;;                                                        (cstr/split (cstr/replace combo_edn #"\"" "") ", "))))))
;;         pkeys @(re-frame/subscribe [::preview-keys])
;;         preview-keys (vec (for [k pkeys] (keyword (str "reco-preview" k))))
;;         preview-maps (into {} (for [{:keys [combo_hash shape_name combo_edn query_map viz_map condis]} full-recos]
;;                                 {(keyword (str "reco-preview" combo_hash))
;;                                  {:shape_name shape_name
;;                                   :query_map query_map
;;                                   :combo_hash combo_hash
;;                                   :viz_map viz_map
;;                                   :condis condis
;;                                   :combo_edn combo_edn}}))
;;         reco-selected (keyword (str "reco-preview" @(re-frame/subscribe [::conn/clicked-parameter-key [:recos-sys/combo_hash]])))
;;         preview-container (fn [preview-maps pk pnum]
;;                             (try (let [panel-key (nth pk pnum)
;;                                        ce (get-in preview-maps [panel-key :combo_edn])
;;                                        query_map (get-in preview-maps [panel-key :query_map])
;;                                        viz_map (get-in preview-maps [panel-key :viz_map])
;;                                        condis (get-in preview-maps [panel-key :condis])
;;                                        combo_hash (get-in preview-maps [panel-key :combo_hash])
;;                                        combo_edn (try (when (not (nil? ce)) (cstr/replace ce #"\"" "")) (catch :default _ "")) ;; TODO, why bombing?
;;                                        shape_name (get-in preview-maps [panel-key :shape_name])
;;                                        sel? (= reco-selected panel-key)
;;                                        body [re-com/v-box
;;                                              :size "auto"
;;                                              :attr {:on-click #(do (re-frame/dispatch [::conn/click-parameter [:recos-sys :combo_hash] combo_hash])
;;                                                                    (re-frame/dispatch [::conn/click-parameter [:recos-sys :combo_edn] combo_edn])
;;                                                                    (re-frame/dispatch [::conn/click-parameter [:recos-sys :shape_name] shape_name])
;;                                                                    (re-frame/dispatch [::conn/click-parameter [:recos-sys :query_map] query_map])
;;                                                                    (re-frame/dispatch [::conn/click-parameter [:recos-sys :viz_map] viz_map])
;;                                                                    (re-frame/dispatch [::conn/click-parameter [:recos-sys :condis] condis]))}
;;                                              :width "240px"
;;                                              :style {:zoom 0.44
;;                                                      :border "1px solid #ffffff22"
;;                                                      :overflow "auto"}
;;                                              :justify :between
;;                                              :children
;;                                              [[re-com/box
;;                                                :size "auto"
;;                                                :height "40px"
;;                                                :style {:padding-left "14px"
;;                                                        :cursor "pointer"
;;                                                        :padding-right "14px"
;;                                                        :font-weight 400
;;                                                        :font-size "22px"}
;;                                                :child (str combo_edn)]
;;                                               [honeycomb
;;                                                panel-key
;;                                                :oz]
;;                                               [re-com/box
;;                                                :size "auto"
;;                                                :height "40px"
;;                                                :justify :end :align :end
;;                                                :style {:padding-left "14px"
;;                                                        :padding-right "14px"
;;                                                        :font-weight 400
;;                                                        :font-size "22px"}
;;                                                :child (str shape_name)]]]]

;;                                    (if sel? (draggable
;;                                              (sql-spawner-meta :viz-reco) "meta-menu"
;;                                              [re-com/box
;;                                               :style {:border "2px dashed #9973e0"
;;                                                       :cursor "grab"}
;;                                               :width "258px"
;;                                               :height "191px"
;;                                               :child body])
;;                                        body))

;;                                  (catch :default _
;;                                    [re-com/box
;;                                     :width "240px"
;;                                     :size "auto"
;;                                     :height "400px"
;;                                     :align :center
;;                                     :justify :center
;;                                     :style {:color (str (theme-pull :theme/editor-font-color nil) 22) ; "#ffffff22" 
;;                                             :zoom 0.44
;;                                             :font-size "40px"}
;;                                     :child "n/a" ;(str e)
;;                                     ])))
;;         pages (/ recos-count 6)]
;; ;(tap> [:full-recos (map :table_name block-list) 
;; ;       (filter (fn [x] (some #(= % (get x :table_name)) current-tab-queries)) block-list)
;; ;       ;(filter #(= (get % :table_name) ) block-list)
;; ;       current-tab-queries ]) 

;;     (dorun (let [prev-preview-keys (for [k @(re-frame/subscribe [::preview-keys])] (keyword (str "reco-preview" k)))
;;                  per-page 6 ;; if we ever get more room...
;;                  grid-page @(re-frame/subscribe [::recos-page])
;;                  grid-page (if (> (- grid-page 1) pages) 0 grid-page)
;;                  recos (take per-page (drop (* grid-page per-page) @(re-frame.core/subscribe [::conn/sql-data [:recos-sys]])))
;;                  recos-keys (vec (for [{:keys [combo_hash]} recos] combo_hash))]
;;              (doseq [k prev-preview-keys] (re-frame/dispatch [::quick-delete-panel k]))
;;              (re-frame/dispatch [::set-preview-keys recos-keys])
;;              (doseq [{:keys [combo_hash query_map viz_map condis combo_edn shape_name]} recos]
;;                (insert-hidden-reco-preview combo_hash viz_map query_map condis combo_edn shape_name false))))


;;     ;(tap> (vec (cons :or (vec (for [t all-sql-call-keys-str] [:= :table_name t])))))
;;     ;(tap> sql-params)
;;     ;(tap> combo-singles)
;;     (dorun (for [[k v] sql-calls]
;;              (let [query (walk/postwalk-replace sql-params v)
;;                    data-exists? @(re-frame/subscribe [::conn/sql-data-exists? [k]])
;;                    unrun-sql? @(re-frame/subscribe [::conn/sql-query-not-run? [k] query])]
;;                (when (or (not data-exists?) unrun-sql?)
;;                  (conn/sql-data [k] query)))))

;;     [re-com/v-box
;;      :height "433px"
;;      :children [[re-com/h-box
;;                  :height "39px"
;;                  :gap "4px"
;;                  :style {:overflow "auto"
;;                          :padding-bottom "5px"
;;                          :padding-left "7px"}
;;                  :children block-list-boxes]

;;                 [re-com/h-box
;;                  :height "40px"
;;                  :gap "12px"
;;                  :align :center :justify :start
;;                  :style {;:background-color "maroon"
;;                          :padding-left "6px"}
;;                  :children [[re-com/single-dropdown
;;                              :choices (conj (for [c combo-singles] {:id c :label c})
;;                                             {:id nil :label "(all possible fields)"})
;;                              :max-height "320px"
;;                              :placeholder "(all possible fields)"

;;                              :style {:font-size "13px"
;;                                      ;:height "22px"
;;                                      ;:border-top (str "1px solid " (theme-pull :theme/editor-font-color nil) 11)
;;                                      ;:border-left (str "1px solid " (theme-pull :theme/editor-font-color nil) 11)
;;                                      ;:border-radius "8px"
;;                                      ;:color (theme-pull :theme/editor-font-color nil)
;;                                      }
;;                              ;:parts {:tooltip {:style {:background-color "blue"}}}
;;                                           ;:can-drop-above? true
;;                                           ;:est-item-height 230
;;                              :model (get sql-params :user-dropdown-sys/req-field)
;;                              :on-change #(re-frame/dispatch [::conn/click-parameter [:user-dropdown-sys :req-field] %])
;;                              :width "285px"]
;;                             ;[re-com/box :child "hey"]
;;                             [re-com/single-dropdown
;;                              :choices (conj (for [{:keys [combo_edn recos]} combo-list]
;;                                               {:id combo_edn :label (str combo_edn " (" recos ")")})
;;                                             {:id nil :label "(all possible field combos)"})
;;                              :max-height "320px"
;;                              :placeholder "(all possible field combos)"
;;                              ;:height "29px"
;;                              :style {:font-size "13px"}
;;                                           ;:can-drop-above? true
;;                                           ;:est-item-height 230
;;                              :model (get sql-params :viz-shapes-sys/combo_edn)
;;                              :on-change #(re-frame/dispatch [::conn/click-parameter [:viz-shapes-sys :combo_edn] %])
;;                              :width "660px"]

;;                             [re-com/box :child "grid"
;;                              :attr {:on-click #(re-frame/dispatch [::set-grid-reco? true])}
;;                              :style {:color (if grid-reco? (theme-pull :theme/editor-font-color nil) "inherit")
;;                                      :cursor "pointer"
;;                                      :margin-top "-9px"
;;                                      :font-weight 700}]

;;                             [re-com/box :child "previews"
;;                              :attr {:on-click #(re-frame/dispatch [::set-grid-reco? false])}
;;                              :style {:color (if (not grid-reco?) (theme-pull :theme/editor-font-color nil) "inherit")
;;                                      :cursor "pointer"
;;                                      :margin-top "-9px"
;;                                      :font-weight 700}]]]

;;                 [re-com/h-box
;;                  :size "auto"
;;                  :style {:color (theme-pull :theme/editor-font-color nil)
;;                          ;:border "2px solid yellow"
;;                          }
;;                  :children [[re-com/box
;;                              :size "none"
;;                              :width "300px"
;;                              :child [magic-table :viz-shapes0-list* [:viz-shapes0-sys] 7 8 []]]
;;                                         ;;  [re-com/box
;;                                         ;;   :size "auto"
;;                                         ;;   :child [magic-table :viz-shapes-list* [:viz-shapes-sys] 7 9 []]]

;;                             (if grid-reco?
;;                               [re-com/box
;;                                :size "none"
;;                                :width "880px"
;;                              ;:style {:padding-left "80px"}
;;                                :child [magic-table :recos-list* [:recos-sys] 19 8
;;                                        [;(when shape-picked? :shape_name) 
;;                                         :query_map_str
;;                                         :table_name :context_hash :connection_id :h :w :condis :viz_map
;;                                                    ;(when combo-picked? :combo_edn)
;;                                         :combo_hash]]]

;;                               (let [] ;; get the next 6 graphs and render?
;;                                 [re-com/h-box :children
;;                                  [[re-com/v-box
;;                                    :size "auto"
;;                                    :height "380px"
;;                                    :width "770px"
;;                                    :children [[re-com/h-box
;;                                                :size "auto"
;;                                                :children [[preview-container preview-maps preview-keys 0]
;;                                                           [preview-container preview-maps preview-keys 1]
;;                                                           [preview-container preview-maps preview-keys 2]]]
;;                                               [re-com/h-box
;;                                                :size "auto"
;;                                                :children [[preview-container preview-maps preview-keys 3]
;;                                                           [preview-container preview-maps preview-keys 4]
;;                                                           [preview-container preview-maps preview-keys 5]]]]]
;;                                   [re-com/v-box
;;                                    :size "auto"
;;                                    :height "380px"
;;                                    :style {:border-top "1px solid #9973e066"
;;                                            :overflow "hidden"}
;;                                    :children (let []
;;                                                (for [c (range pages)]
;;                                                  [re-com/box
;;                                                   :child (str c)
;;                                                   :height "19px"
;;                                                   :align :end
;;                                                   :justify :center
;;                                                   :attr {:on-click #(re-frame/dispatch [::set-recos-page c])}
;;                                                   :style {:padding-right "4px"
;;                                                           :cursor "pointer"
;;                                                           :font-size "12px"
;;                                                           :color (if (= c recos-page)
;;                                                                    (theme-pull :theme/editor-background-color nil) ;;"#000000" 
;;                                                                    (theme-pull :theme/editor-font-color nil))
;;                                                           :background-color (if (= c recos-page)
;;                                                                               "#9973e0" ;"darkcyan" 
;;                                                                               "inherit")
;;                                                           :border-bottom "1px solid #9973e066"
;;                                                           :border-right "1px solid #9973e066"}]))
;;                                    :width "30px"]]]))]]]]))