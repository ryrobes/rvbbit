(ns rvbbit-frontend.mapboxes
  (:require
    [clojure.string          :as cstr]
    [clojure.walk            :as walk]
    [re-com.core             :as    re-com
                             :refer [at]]
    [re-com.util             :refer [px]]
    [re-frame.core           :as re-frame]
    [reagent.core            :as reagent]
    [rvbbit-frontend.connections :as conn]
    [rvbbit-frontend.db      :as db]
    [rvbbit-frontend.resolver :as resolver]
    [rvbbit-frontend.utility :as ut]))

(defn theme-pull-fn
  [cmp-key fallback & test-fn] ;; dupe from bricks.cljs refactor into its own ns?
  (let [v                   @(ut/tracked-subscribe [::conn/clicked-parameter-key [cmp-key]])
        t0                  (ut/splitter (str (ut/safe-name cmp-key)) #"/")
        t1                  (keyword (first t0))
        t2                  (keyword (last t0))
        self-ref-keys       (distinct (filter #(and (keyword? %) (namespace %))
                                        (ut/deep-flatten db/base-theme)))
        self-ref-pairs      (into {}
                                  (for [k    self-ref-keys ;; todo add a reurziver version of
                                        :let [bk (keyword (ut/replacer (str k) ":theme/" ""))]]
                                    {k (get db/base-theme bk)}))
        resolved-base-theme (ut/postwalk-replacer self-ref-pairs db/base-theme)
        base-theme-keys     (keys resolved-base-theme)
        theme-key?          (true? (and (= t1 :theme) (some #(= % t2) base-theme-keys)))
        fallback0           (if theme-key? (get resolved-base-theme t2) fallback)
        rs                  (fn [edn] (resolver/logic-and-params edn nil))]
    (rs (if (not (nil? v)) v fallback0))))

(re-frame/reg-sub  ;; dupe from bricks.cljs refactor into its own ns?
  ::theme-pull-sub ;; more cache hits?
  (fn [_ [_ cmp-key fallback & test-fn]] (theme-pull-fn cmp-key fallback test-fn)))

(defn theme-pull
  [cmp-key fallback & test-fn]
  @(ut/tracked-subscribe [::theme-pull-sub cmp-key fallback test-fn])) ;; dupe from bricks.cljs refactor into its own ns?

(defn sql-explanations-kp
  []
  {[:from 0 0] "table-name" [:from 0 1] "table-alias" [:from] "the table we are selecting from"})

(defn map-value-box
  [s k-val-type] ;; dupe of a fn inside flows, but w/o dragging - TODO refactor to single fn -
  (let [render-values? true
        function?      (or (fn? s) (try (fn? s) (catch :default _ false)))]
    (cond (ut/hex-color? s)                        [re-com/h-box :gap "3px" :children
                                                    [[re-com/box :child (str s)]
                                                     [re-com/box :child " " :width "13px" :height
                                                      "13px" :style
                                                      {:background-color (str s)
                                                       :border-radius    "2px"}]]]
          (ut/is-base64? s)                        [re-com/v-box :size "auto" :gap "10px" :children
                                                    [[re-com/box :align :end :justify :end :style
                                                      {:opacity 0.5} :child
                                                      (str "**huge base64 string: ~"
                                                           (.toFixed (/ (* (count s) 6) 8 1024 1024)
                                                                     2)
                                                           " MB")]
                                                     [re-com/box :size "auto" :child
                                                      [:img
                                                       {:src (str "data:image/png;base64," s)}]]]]
          (or function? (= k-val-type "function")) (str (.-name s) "!") ;; temp
          (string? s)                              [re-com/box :size "auto" :align :end :justify
                                                    :end :style {:word-break "break-all"} :child
                                                    (str "\"" s "\"")]
          :else                                    (ut/replacer (str s) #"clojure.core/" ""))))

(defn map-boxes2
  [data block-id flow-name keypath kki init-data-type & [draggable?]] ;; dupe of a fn inside
  (let [sql-explanations (sql-explanations-kp)
        flow-name (ut/data-typer data)
        data (if (or (string? data) (number? data) (boolean? data)) [data] data)
        base-type-vec? (or (vector? data) (list? data))
        iter (if base-type-vec? (range (count data)) (keys data))
        font-size "inherit" ;;"11px"
        draggable? false ;; override bs TODO
        add-kp? (try (keyword? block-id) (catch :default _ false)) ;;true ;(not
        cells? (= block-id :cells)
        main-boxes
          [re-com/v-box ;(if cells? re-com/h-box re-com/v-box)
           :size "auto" :padding "5px" :gap "2px" :style
           {:color       "black"
            :font-family (theme-pull :theme/base-font nil)
            :font-size   font-size ; "11px"
            :font-weight 500} :children
           (for [kk iter] ;; (keys data)
             (let [k-val      (get-in data [kk])
                   k-val-type (ut/data-typer k-val)
                   in-body?   true ;(ut/contains-data? only-body k-val)
                   hovered?   false ;(ut/contains-data? mat-hovered-input k-val)
                   border-ind (if in-body? "solid" "dashed")
                   val-color  (get @(ut/tracked-subscribe [::conn/data-colors]) k-val-type)
                   keypath-in (conj keypath kk)
                   keystyle   {:background-color (if hovered? (str val-color 66) "#00000000")
                               :color            val-color
                               :border-radius    "12px"
                               :border           (str "3px " border-ind " " val-color)}
                   valstyle   {:background-color (if hovered? (str val-color 22) "#00000000") ;"#00000000"
                               :color            val-color
                               :border-radius    "12px"
                               :border           (str "3px " border-ind " " val-color 40)}]
               ^{:key (str block-id keypath kki kk)}
               [re-com/box :child
                (cond
                  (= k-val-type "map")
                    ^{:key (str block-id keypath kki kk k-val-type)}
                    [re-com/h-box :children
                     [[draggable-pill
                       {:from       block-id
                        :new-block  [:artifacts "text"]
                        :idx        0 ;idx
                        :keypath-in keypath-in
                        :flow-id    flow-name
                        :keypath    [:map (if add-kp? (vec (cons :v keypath-in)) keypath-in)]}
                       block-id ;; (when (not (= block-id :inline-render)) block-id)
                       ^{:key (str block-id keypath kki kk k-val-type 1)}
                       [re-com/v-box :min-width (px (* (count (str kk)) 11)) ;"110px"
                        :style {:cursor (when draggable? "grab")} :children
                        [^{:key (str block-id keypath kki kk k-val-type 124)}
                         [re-com/box :child (str kk)]
                         ^{:key (str block-id keypath kki kk k-val-type 134)}
                         [re-com/box :child (str k-val-type) :style
                          {:opacity     0.45
                           :font-size   font-size ; "9px"
                           :padding-top "7px"}]
                         (when (> (count k-val) 1)
                           ^{:key (str block-id keypath kki kk k-val-type 156)}
                           [re-com/box :style {:opacity 0.45} :child (str "(" (count k-val) ")")])]
                        :padding "8px"]]
                      (map-boxes2 k-val block-id flow-name keypath-in kk nil draggable?)] :style
                     keystyle]
                  (or (= k-val-type "vector")
                      (= k-val-type "list") ; (= k-val-type "function")
                      (= k-val-type "rowset")
                      (= k-val-type "jdbc-conn")
                      (= k-val-type "render-object"))
                    ^{:key (str block-id keypath kki kk k-val-type 2)}
                    [re-com/h-box :style {:border-radius "12px" :border "3px solid black"} :children
                     [[draggable-pill
                       {:from       block-id
                        :new-block  [:artifacts "text"]
                        :idx        0 ;idx
                        :keypath-in keypath-in
                        :flow-id    flow-name
                        :keypath    [:map (if add-kp? (vec (cons :v keypath-in)) keypath-in)]}
                       block-id
                       ^{:key (str block-id keypath kki kk k-val-type 3)}
                       [re-com/v-box :min-width (px (* (count (str kk)) 11)) ;"110px"
                        :style
                        {:cursor (when draggable? "grab") ;;; skeptical, ryan 5/24/33
                        } :children
                        (if (and (= k-val-type "list") (= (ut/data-typer (first k-val)) "function"))
                          [^{:key (str block-id keypath kki kk k-val-type 4)}
                           [re-com/h-box :style {:margin-top "4px"} :gap "1px" :children
                            [;[re-com/box :child (str kk) :style {:opacity 0.25}]
                             [re-com/box :child "(" :style
                              {;:opacity 0.5
                               :color       "orange"
                               :margin-top  "-2px"
                               :font-size   "14px"
                               :font-weight 700}]
                             ^{:key (str block-id keypath kki kk k-val-type 5)}
                             [re-com/box :child (str (first k-val)) ; (str  "(")
                              :style
                              {:color     "#ffffff"
                               :font-size font-size ;"17px"
                              }]]]
                           ^{:key (str block-id keypath kki kk k-val-type 6)}
                           [re-com/box :child (str k-val-type) :style
                            {:opacity     0.45
                             :font-size   font-size ; "9px"
                             :padding-top "16px"}]
                           (when (> (count k-val) 1)
                             ^{:key (str block-id keypath kki kk k-val-type 827)}
                             [re-com/box :style {:opacity 0.45} :child
                              (str "(" (count k-val) ")")])]
                          [(when true ;(not (get sql-explanations keypath ""))
                             ^{:key (str block-id keypath kki kk k-val-type 7)}
                             [re-com/box :child (str kk)])
                           (when true ; (not (get sql-explanations keypath ""))
                             ^{:key (str block-id keypath kki kk k-val-type 8)}
                             [re-com/box :child (str (when (= (count k-val) 0) "empty ") k-val-type)
                              :style
                              {:opacity     0.45
                               :font-size   font-size ; "9px"
                               :padding-top "7px"}])
                           (when (not (empty? (get sql-explanations keypath "")))
                             ^{:key (str block-id keypath kki kk k-val-type 82)}
                             [re-com/md-icon-button :md-icon-name "zmdi-info" :tooltip
                              (str "FOOO" (get sql-explanations keypath "")) :style
                              {:font-size  "16px"
                               :cursor     "pointer"
                               :opacity    0.5
                               :padding    "0px"
                               :margin-top "-1px"}])]) :padding "8px"]]
                      [map-boxes2
                       (if (= k-val-type "rowset")
                         (zipmap (iterate inc 0) (take 10 k-val))
                         (zipmap (iterate inc 0) k-val)) block-id flow-name keypath-in kk nil
                       draggable?]] :style keystyle]
                  :else
                    ^{:key (str block-id keypath kki kk k-val-type 9)}
                    [re-com/h-box :children
                     [^{:key (str block-id keypath kki kk k-val-type 10)}
                      [re-com/h-box :gap "6px" :children
                       [[draggable-pill
                         {:from       block-id
                          :new-block  [:artifacts "text"]
                          :idx        0 ;idx
                          :keypath-in keypath-in
                          :flow-id    flow-name
                          :keypath    [:map (if add-kp? (vec (cons :v keypath-in)) keypath-in)]}
                         block-id
                         ^{:key (str block-id keypath kki kk k-val-type 11)}
                         [re-com/box :child (str kk) :style {:cursor (when draggable? "grab")}]]
                        (when true ;(not (get sql-explanations (vec (conj keypath kk)) ""))
                          ^{:key (str block-id keypath kki kk k-val-type 12)}
                          [re-com/box :child (str k-val-type) :style
                           {:opacity    0.45
                            :font-size  font-size ;"9px"
                            :font-style (if (= k-val-type "function") "italic" "normal")}])
                        (when (get sql-explanations (vec (conj keypath kk)) "")
                          ^{:key (str block-id keypath kki kk k-val-type 822)}
                          [re-com/box :style {:opacity 0.45} :child
                           (str (get sql-explanations (vec (conj keypath kk)) ""))])]]
                      ^{:key (str block-id keypath kki kk k-val-type 13)}
                      [re-com/box :size "auto" :align :end :justify :end :child
                       [map-value-box k-val] :style
                       {;:font-weight 500
                        :line-height  "1.2em"
                        :padding-left "5px"}]] :justify :between :padding "5px" :style
                     valstyle])]))]]
    (if (= keypath [])
      (let [k-val-type (ut/data-typer data)
            nin?       (not (= block-id :inline-render))
            val-color  (get @(ut/tracked-subscribe [::conn/data-colors]) k-val-type)]
        [re-com/v-box :children
         [[re-com/v-box :style {:word-wrap "break-word" :overflow-wrap "break-word"} :children
           [(when nin?
              [re-com/v-box :padding "6px" :children
               [^{:key (str block-id keypath kki 00 k-val-type 00002)}
                [re-com/box :child (str flow-name) ;(str k-val-type)
                 :align :end :style
                 {:opacity       0.45
                  :font-size     font-size ;"9px"
                  :margin-top    "-7px"
                  :margin-bottom "-5px"}]]]) main-boxes] :padding "0px"]] :style
         {:font-family   (theme-pull :theme/base-font nil)
          :color         val-color
          :margin-top    (if nin? "0px" "-10px")
          :border-radius "12px"}])
      main-boxes)))