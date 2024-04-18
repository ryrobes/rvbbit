(ns rvbbit-frontend.rules
  (:require
   [reagent.core :as reagent]
   [re-frame.core :as re-frame]
   [re-com.core :as re-com :refer [at]]
   [re-com.util :refer [px]]
   [rvbbit-frontend.connections :as conn]
   [re-catch.core :as rc]
   [rvbbit-frontend.utility :as ut]
   [rvbbit-frontend.buffy :as buffy]
   [clojure.data :as cdata]
   ;[re-pressed.core :as rp]
   [rvbbit-frontend.audio :as audio]
   [cljs.tools.reader :refer [read-string]]
   ;[rvbbit-frontend.shapes2 :as shape2]
   ;[rvbbit-frontend.shapes :as scrubbers]
   [garden.color :as c :refer [hex->hsl hsl->hex hsl hsla color? hex? invert mix
                               complement shades triad tetrad split-complement
                               analogous rgb->hsl rgb->hex]]
   [talltale.core :as tales]
   [day8.re-frame.undo :as undo :refer [undoable]]
   [cljs.core.async :as async :refer [<! >! chan]]
   ;["react" :as react]
   [rvbbit-frontend.resolver :as resolver]
   [clojure.edn :as edn]
   [rvbbit-frontend.db :as db]
   [rvbbit-frontend.subs :as subs]
   [rvbbit-frontend.http :as http]
   [goog.dom :as gdom]
   [rvbbit-frontend.bricks :as bricks :refer [theme-pull]]
   [goog.events :as gevents]
   [clojure.walk :as walk]
   [clojure.string :as cstr]
   [clojure.set :as cset]
   ["react-drag-and-drop" :as rdnd]
   ["react-codemirror2" :as cm]
   ["codemirror/mode/sql/sql.js"]
   ["codemirror/addon/edit/matchbrackets.js"]
   ["codemirror/addon/edit/closebrackets.js"]
   ["codemirror/mode/clojure/clojure.js"]
   ["codemirror/mode/python/python.js"]
   ["codemirror/mode/r/r.js"]
   ["codemirror/mode/julia/julia.js"]
   [rvbbit-frontend.connections :refer [sql-data]]
   ["react-zoom-pan-pinch" :as zpan]
   [websocket-fx.core :as wfx]
   ;[cljs.tools.reader :refer [read-string]]
   ;[oz.core :as oz]
   ;[reagent.dom :as rdom]
   ;[websocket-fx.core :as wfx]
   [cljs-drag-n-drop.core :as dnd2]
   [clojure.walk :as w])
  (:import [goog.events EventType]
           [goog.async Debouncer]))

(re-frame/reg-event-db
 ::edit-rule-drop
 (undoable)
 (fn [db [_ content adding]]
   (tap> [:edit? content adding])
   (let [curr (get-in db [:rules-map (get db :selected-rule) :rule])]
     (assoc-in db [:rules-map (get db :selected-rule)] 
               {:rule (walk/postwalk-replace 
                       {content 
                        (vec (conj content (if (vector? adding) adding [adding])))
                        ;(vec (cons adding content))
                        ;[adding content]
                        } curr)}))))

(re-frame/reg-event-db
 ::edit-rule-drop-kill
 (undoable)
 (fn [db [_ content]]
   (let [curr (get-in db [:rules-map (get db :selected-rule) :rule])]
     (tap> [:kill-nested? content :from curr])
     (assoc-in db [:rules-map (get db :selected-rule)]
               {:rule (ut/remove-subvec curr content)}))))

(re-frame/reg-event-db
 ::edit-rule-drop2
 (undoable)
 (fn [db [_ content adding]]
   (tap> [:edit2? content adding])
   (let [curr (get-in db [:rules-map (get db :selected-rule) :rule])]
     (assoc-in db [:rules-map (get db :selected-rule)]
               {:rule (walk/postwalk-replace 
                       {content (vec (conj content adding))} curr)}))))

(defn draggable-item [element type data]
  [(reagent/adapt-react-class rdnd/Draggable)
   (let [  ]
     {:type          type
      :data          (pr-str data)})
   [re-com/box 
    :style {:cursor "grab"}
    :child element]])

(defn droppable-area [element types-vec content operator]
  [(reagent/adapt-react-class rdnd/Droppable)
   {:types   types-vec
    :on-drop #(let [incoming   (js->clj %)
                    incoming (into [] (for [[k v] incoming] {k (edn/read-string v)}))
                    ii (last (last (first incoming)))
                    ]
                (re-frame/dispatch [::edit-rule-drop (vec (cons operator content)) ii])
                (tap> [:drop-rule-item ii incoming content]))}
   element])

(defn droppable-area2 [element types-vec content]
  [(reagent/adapt-react-class rdnd/Droppable)
   {:types   types-vec
    :on-drop #(let [incoming   (js->clj %)
                    incoming (into [] (for [[k v] incoming] {k (edn/read-string v)}))
                    ii (last (last (first incoming)))]
                (re-frame/dispatch [::edit-rule-drop2 content ii])
                (tap> [:drop-rule-item2 ii incoming content]))}
   element])

(defn kill-nest [op]
  [re-com/box :child "x" :padding "6px"
   :style {:cursor "pointer"}
   :attr {:on-click #(do
                       (tap> [:kill-nested op])
                       (re-frame/dispatch [::edit-rule-drop-kill op]))}])

(def hover-tools-atom (reagent/atom nil))
   
(defn hover-tools [coll]
  (let [hovered? (= @hover-tools-atom coll)]
    [re-com/h-box
     :attr {:on-mouse-enter #(reset! hover-tools-atom coll)
            :on-mouse-over #(when (not= @hover-tools-atom coll)
                              (reset! hover-tools-atom coll))
            :on-mouse-leave #(reset! hover-tools-atom nil)}
    ;;  :style (when hovered? {:background-color "rgba(0, 0, 255, 0.3)"})
     :children [[kill-nest coll]
                [re-com/box :padding "6px" :child "true"]]]))

 
(defn visualize-clause
  [clause level & [contents]]
  (let [is-operator? (fn [c] (and (vector? c) (keyword? (first c))
                                  (contains? #{:and :or :not} (first c))))
        render-operator (fn [operator contents lvl]
                          (let [body (vec (cons operator contents))
                                hovered? (= @hover-tools-atom body)]
                            [re-com/h-box
                             :style (when hovered? {:background-color "rgba(0, 0, 255, 0.3)"})
                           ;:justify :between
                             :align :center 
                             :padding "6px"
                             :children [[re-com/v-box
                                         :size "auto" :width (px (- 540 (* lvl 35)))
                                         :children (cons [droppable-area [re-com/box :child (str operator)] [:operator] contents operator]
                                                         (map #(visualize-clause % (inc lvl) contents) contents))
                                         :gap "10px"
                                         :style {:border "1px solid #ccc" :padding "5px"
                                                 :font-size "17px"
                                                 :margin-left (str (* lvl 10) "px")}]

                                        [hover-tools body]]]))
        render-condition (fn [condition level contents]
                           [droppable-area2
                            [re-com/h-box
                             :children [[re-com/box :child (str condition) :size "auto"]]
                             :style {:border "1px solid blue"
                                     :padding "5px"}]
                            [:operator] contents])]
    (if (vector? clause)
      (if (is-operator? clause)
        (render-operator (first clause) (rest clause) level)
        (let [;words (ut/deep-flatten clause)
              ;lw (last words)
              hovered? (= @hover-tools-atom clause)]
          [re-com/h-box
           ;:padding "6px"
           :style (merge
                   (when hovered? {:background-color "rgba(0, 0, 255, 0.3)"})
                   {:font-size "17px"
                    :margin-left (str (* level 10) "px")})
         ;:justify :between  
           :align :center ;:padding "6px"
           :children
           [[re-com/h-box
           ;:size "auto"
             :children (map #(visualize-clause % level clause) clause)
             :gap "10px"
             :style {:border "1px solid #ccc" :padding "5px"}]
            ;;[re-com/box :padding "6px" :child (if (= lw 456) "false" "true")]
            [hover-tools clause]
            ]])) ; Keep pairings on the same row
      (render-condition clause level contents))))


(defn code-box [width-int height-int value]
  (let []
    [re-com/box
     :size "none"
     ;:width (px (- width-int 24))
     :height (px (- height-int 24))
     :style {:font-family   (theme-pull :theme/monospaced-font nil) ; "Chivo Mono" ;"Fira Code"
             :font-size     "17px"
             :overflow      "auto"
             :border-radius "12px"
             ;:border "1px solid yellow"
             :font-weight   700}
     :child [(reagent/adapt-react-class cm/UnControlled)
             {:value   (ut/format-map (- width-int 95)
                                      (str value))
              ;;:onBlur  #(re-frame/dispatch [::edit-rule (read-string (cstr/join " " (ut/cm-deep-values %)))])
              :onBlur  #(let [parse        (try (read-string
                                                 (cstr/join " " (ut/cm-deep-values %)))
                                                (catch :default e [:cannot-parse (str (.-message e))]))
                              unparseable? (= (first parse) :cannot-parse)]
                          (cond unparseable?
                            ; (js/alert "BAD FORM MATE!!")
                                (do (reset! db/bad-form-rules? true)
                                    (reset! db/bad-form-msg-rules (str (last parse))))

                                (empty? parse)
                                (do (reset! db/bad-form-rules? true)
                                    (reset! db/bad-form-msg-rules "Empty rule"))

                                (not (vector? parse))
                                (do (reset! db/bad-form-rules? true)
                                    (reset! db/bad-form-msg-rules "Needs to be a vector / honey-sql where clause format"))

                                :else (do (reset! db/bad-form-rules? false)
                                          (re-frame/dispatch [::edit-rule parse]))))
              :options {:mode              "clojure"
                        :lineWrapping      true
                        :lineNumbers       false ;true
                        :matchBrackets     true
                        :autoCloseBrackets true
                        :autofocus         false
                        :autoScroll        false
                        :detach            true
                        :readOnly          false
                        :theme             (theme-pull :theme/codemirror-theme nil)}}]]))

(re-frame/reg-sub
 ::rules-map
 (fn [db _]
   (merge
    {"rule name 1" {:rule [:and [:= 1 1] [:= "hot dogs" "hot dogs"]  [:or [:<> "fish" "brocolli"] [:= 2 2] [:not [:= 123 456]]]]}
     :rulke2 {:rule [:or [:not [:> 1 2]] [:and [:= 1 1] [:= 2 2]]]}
     "rule name 3" {:rule [:and [:= 1 1] [:= 2 2]]}
     "rule name 4" {:rule [:or [:> 1 2] [:and [:= 1 1] [:= 2 2]]]}
     "rule name 5" {:rule [:= 2 2]}
     "rule name 6" {:rule [:not [:= 2 2]]}
     "rule name 7" {:rule [:not [:= 2 2]]}
     "rule name 8" {:rule [:not [:= 2 2]]}
     "rule name 9" {:rule [:not [:= 2 2]]}
     "rule name 10" {:rule [:and [:= :day 1] [:= :hour 9]]}}
    (get db :rules-map {}))))

(re-frame/reg-event-db
 ::select-rule
 (fn [db [_ rule-name]]
   (assoc db :selected-rule rule-name)))

(re-frame/reg-sub
 ::operators
 (fn [db _]
   (get-in db [:rule-items :operators]
           [:= :<> :> :< :>= :<= :and :or :not :like])))

(re-frame/reg-sub
 ::conditions
 (fn [db _]
   (get-in db [:rule-items :conditions]
           [:and :or :not])))

(re-frame/reg-sub
 ::time-items
 (fn [db _]
   (get-in db [:rule-items :time-items]
           [[:= :day 1]
            [:= :hour 13]
            [:= :month 1]
            [:= :year 2021]
            ])))

(re-frame/reg-sub
 ::selected-rule
 (fn [db _]
   (get db :selected-rule)))

(re-frame/reg-event-db
 ::edit-rule
 (undoable)
 (fn [db [_ rule]]
   (assoc-in db [:rules-map (get db :selected-rule)] {:rule rule})))

(re-frame/reg-event-db
 ::delete-rule
 (undoable)
 (fn [db [_ rule-name]]
   (-> db
       (ut/dissoc-in [:rules-map rule-name])
       (assoc :selected-rule nil))))

(re-frame/reg-event-db
 ::add-rule
 (undoable)
 (fn [db [_]]
   (let [rr (ut/gen-rule-name)
         rule-name (ut/safe-key rr (vec (keys (get db :rules-map {}))))]
     (-> db
         (assoc-in [:rules-map rule-name :rule] [:and [:= :day 1] [:= :hour 9]])
         (assoc :selected-rule rule-name)))))

(defn rules-list [ph rules selected-rule]
  (let [;rules @(re-frame/subscribe [::rules-map])
        ;selected-rule @(re-frame/subscribe [::selected-rule])
        ]
    [re-com/v-box
     :children
     [[re-com/h-box
       :height "30px" :padding "6px"
       :align :center :justify :between
       :children [[re-com/box
                   :style {:cursor "pointer"}
                   :attr {:on-click #(re-frame/dispatch [::add-rule "new rule"])}
                   :child "+ new rule"]
                  (if selected-rule
                    [re-com/box
                     :style {:cursor "pointer"}
                     :attr {:on-click #(re-frame/dispatch [::delete-rule selected-rule])}
                     :child (str "- delete " selected-rule)]
                    [re-com/gap :size "5px"])]]
      [re-com/v-box
       :padding "6px"
       :style {:border "1px solid red"
               :overflow "auto"}
       :gap "6px"
       :size "none"
       :height (px (- ph 94))
       :children (for [[name {:keys [rule]}] rules
                       :let [selected? (= name selected-rule)]]
                   [re-com/v-box
                    :padding "6px"
                    :size "none"
                    :width "100%"
                    :attr {:on-click #(re-frame/dispatch [::select-rule (if selected? nil name)])}
                    :style {:border (if selected?
                                      "1px dashed cyan"
                                      "1px solid blue")
                            :background-color (if selected?
                                                "rgba(0, 0, 255, 0.3)"
                                                "rgba(0, 0, 0, 0.1)")
                            :cursor "pointer"}
                    :children [[re-com/h-box


                                :style {:font-size "19px"}
                                :justify :between
                                :height "25px"
                                :children [[re-com/box :child (str name)]
                                           [re-com/box :child "true" :style {:font-weight 700}]]]
                               [re-com/box :child
                                 ;;   (if selected?
                                 ;;     (visualize-clause rule 0)
                                 ;;     (str rule))
                                (str rule)]]])]]]))






(defonce selectors-open (reagent/atom []))

(defn selector-panel [name items & [style wide?]]
  (let [open? (some #(= name %) @selectors-open)
        partition? (integer? wide?)]
    [re-com/v-box
     :padding "6px"
     :style (merge {:border "1px solid yellow"} style)
     :children [[re-com/box
                 :height "28px"
                 :style {:cursor "pointer"}
                 :attr {:on-click #(if open?
                                     (reset! selectors-open (remove (fn [x] (= x name)) @selectors-open))
                                     (swap! selectors-open conj name))}
                 :child name]
                (when open?
                  (if partition? 
                    [re-com/v-box
                     :children (for [seg (partition-all wide? items)]
                                 [re-com/h-box ;;:size "auto"
                                  :children (for [item seg
                                                  :let [pw (* (- (last @db/flow-editor-system-mode) 70) 0.35)
                                                        width (/ pw wide?)]]
                                              [draggable-item
                                               [re-com/box
                                                :width (px width)
                                                ;;:size "auto"
                                                :padding "6px"
                                                :style {:border "1px solid purple"
                                                        :font-family   (theme-pull :theme/monospaced-font nil)}
                                                :child (str item)] :operator item])])]
                    [(if wide?
                     re-com/h-box
                     re-com/v-box)
                   :children (for [item items]
                               [draggable-item
                                [re-com/box
                                 :padding "6px"
                                 :style {:border "1px solid purple"
                                         :font-family   (theme-pull :theme/monospaced-font nil)}
                                 :child (str item)] :operator item])]))]]))


(defn left-col [ph]
  (let [operators @(re-frame/subscribe [::operators])
        conditions @(re-frame/subscribe [::conditions])
        time-items @(re-frame/subscribe [::time-items])
        react! [@selectors-open]]
    [re-com/v-box
     :padding "6px"
     :width "35%"
     :height (px (- ph 50))
     :style {:border "1px solid yellow"
             :overflow "auto"}
     :gap "6px"
     :children [[selector-panel "operators" operators {} 5]
                [selector-panel "conditions" conditions {} 3]
                [selector-panel "time items" time-items {}]]]))

(re-frame/reg-event-db
 ::rename-rule
 (undoable)
 (fn [db [_ old new]]
   ;(assoc db :rules-map (cset/rename-keys {old new} (get db :rules-map {})))
   (-> db
       (assoc :rules-map (walk/postwalk-replace {old new} (get db :rules-map {})))
       (assoc :selected-rule new))))

(defonce title-edit-idx (reagent/atom nil))

(defn edit-rule-name [rule-name w]
  (let [;read-only-flow? (true? (cstr/includes? flow-id "/"))
        ;flow-id-regex #"^[a-zA-Z0-9_-]+$"
        ] ;; alpha, underscores, hypens, numbers
    (if (not @title-edit-idx)
      [re-com/box
       :size "none"
       :height  "45px"
       :width (px w)
       :align :center
       :justify :end
       :attr {:on-double-click #(reset! title-edit-idx (str rule-name))}
       :style {:cursor "pointer"
               :padding-right "12px"
               :padding-top "1px"
               :border "2px solid transparent"
               :font-size "26px"}
       :child (str rule-name)]
      [re-com/input-text
       :src (at)
       :model             (str rule-name)
       :width             (px (- w 6))
       :height            "45px"
       :on-change         #(do (re-frame/dispatch [::rename-rule rule-name %])
                               (reset! title-edit-idx nil))
       ;:validation-regex  flow-id-regex
       :change-on-blur?   true
       :style  {:border (str "2px dashed " (theme-pull :theme/editor-outer-rim-color nil))
                :font-size "26px"
                :text-decoration "underline"
                :color (theme-pull :theme/editor-outer-rim-color nil)
                :font-style "underline"
                :text-align "center"
                :background-color "#00000000"}])))

(defn right-col [ph]
  (let [rules @(re-frame/subscribe [::rules-map])
        selected-rule @(re-frame/subscribe [::selected-rule])
        rule-vec (get-in rules [selected-rule :rule])]
    [re-com/v-box
     :padding "6px"
     :width "65%"
     :style {:border "1px solid lime"}
     :children [(when selected-rule
                  [re-com/v-box
                   :style {:font-family   (theme-pull :theme/monospaced-font nil)
                           :border "1px solid pink"}
                   :height (px (* ph 0.7))
                   :padding "6px"
                   :children
                   [
                    ;; [re-com/box :child (str selected-rule) :padding "6px"
                    ;;  :align :center
                    ;;  :justify :center
                    ;;  :style {:border "1px dotted orange" :font-size "20px"}
                    ;;  :height "50px"]
                    [edit-rule-name selected-rule (* (- (last @db/flow-editor-system-mode) 70) 0.65)]
                    [re-com/box
                     :height (px (- (* (* ph 0.7) 0.75) 50)) :size "none"
                     :style {:border "1px solid orange" :overflow "auto"}
                     :child (if @db/bad-form-rules?
                              [re-com/box
                               :size "auto"
                               :style {:background-color "rgba(255, 0, 0, 0.3)"
                                       :border "1px solid red"
                                       :padding "6px"
                                       :font-size "20px"}
                               :child (str @db/bad-form-msg-rules)]
                              [rc/catch
                               (visualize-clause rule-vec 0)])]
                    [re-com/box
                     :style {:border "1px solid orange"}
                     :height (px (- (* (* ph 0.7) 0.25) 12)) ;; "24%" 
                     :padding "6px"
                     :child [code-box 
                             (* (- (last @db/flow-editor-system-mode) 14) 0.65) ;; width
                             (- (* (* ph 0.7) 0.25) 5) ;; (* ph 0.25) 
                             (str rule-vec)]]]])
                [rules-list (if selected-rule
                              (* ph 0.3)
                              ph) rules selected-rule]
              ;[re-com/h-box :children []]
                ]]))

(defn rules-panel []
  (let [[_ hpct] db/flow-panel-pcts ;; [0.85 0.50]
        hh @(re-frame/subscribe [::subs/h]) ;; we want the reaction 
        panel-height (* hh hpct)
        ;; _ (tap> [:yo hh (.-innerHeight js/window)])
        ;ph (- panel-height 10)
        details-panel-height (/ panel-height 1.25) ;; batshit math from flow.cljs panel parent
        ppanel-height (+ panel-height details-panel-height)
        ph (- ppanel-height 124)]
    [re-com/v-box
     :padding "6px"
   ;:size "auto" 
     :width (px (- (last @db/flow-editor-system-mode) 14))
     :size "none"
     :height (px ph)
     :style {:border "1px solid cyan"}
     :children [[re-com/h-box
                 :padding "6px"
                 :justify :between :align :center 
                 :children [[re-com/box :child "newer than the server"]
                            [re-com/box :child "push this rule set to server?"]]]
                [re-com/gap :size "5px"]
                [re-com/h-box :children [[left-col ph]
                                         [right-col ph]]]]]))