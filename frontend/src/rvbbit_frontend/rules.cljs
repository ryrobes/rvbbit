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

;; (defn visualize-clause
;;   [clause level]
;;   (let [is-operator? (fn [c] (and (vector? c) (keyword? (first c))
;;                                   (contains? #{:and :or :not} (first c))))
;;         render-operator (fn [operator contents lvl]
;;                           (let [box-fn (if
;;                                         (some #(= operator %) [:and :or :not])
;;                                          re-com/h-box re-com/v-box)]
;;                             (box-fn :children (cons [re-com/box :child (str operator)] 
;;                                                     (map #(visualize-clause % (inc lvl)) contents))
;;                                     :gap "10px"
;;                                     :style {:border "1px solid #ccc" :padding "5px"})))
;;         render-condition (fn [condition lvl]
;;                            [(if (even? lvl)
;;                               re-com/h-box re-com/v-box)
;;                             :children [condition]
;;                             :style {:border "1px solid blue" :padding "5px"}])]
;;     (if (vector? clause)
;;       (if (is-operator? clause)
;;         (render-operator (first clause) (rest clause) level)
;;         (map #(visualize-clause % level) clause)) ; This assumes non-operator vectors are groups of conditions
;;       (render-condition clause level))))

(defn visualize-clause
  [clause level]
  (let [is-operator? (fn [c] (and (vector? c) (keyword? (first c))
                                  (contains? #{:and :or :not} (first c))))
        render-operator (fn [operator contents lvl]
                          [re-com/h-box
                           ;:justify :between
                           :align :center :padding "6px"
                           :children [[re-com/v-box
                                       :size "auto"
                                       :children (cons (re-com/box :child (str operator))
                                                       (map #(visualize-clause % (inc lvl)) contents))
                                       :gap "10px"
                                       :style {:border "1px solid #ccc" :padding "5px"
                                               :margin-left (str (* lvl 10) "px")}]

                                      [re-com/box :padding "6px" :child "true"]]])
        render-condition (fn [condition lvl]
                           [re-com/h-box 
                            :children [[re-com/box :child (str condition ) :size "auto"]]
                            :style {:border "1px solid blue" :padding "5px"}])]
    (if (vector? clause)
      (if (is-operator? clause)
        (render-operator (first clause) (rest clause) level)
        (let [words (ut/deep-flatten clause)
              lw (last words)]
          [re-com/h-box
         ;:justify :between  
           :align :center ;:padding "6px"
           :children
           [[re-com/h-box
           ;:size "auto"
             :children (map #(visualize-clause % level) clause)
             :gap "10px"
             :style {:border "1px solid #ccc" :padding "5px"}]
            [re-com/box :padding "6px" :child (if (= lw 456) "false" "true")]]])) ; Keep pairings on the same row
      (render-condition clause level))))



(re-frame/reg-sub
 ::rules-map
 (fn [db _]
   (get db :rules-map 
        {"rule name 1" {:rule [:and [:= 1 1] [:= "hot dogs" "hot dogs"]  [:or [:<> "fish" "brocolli"] [:= 2 2] [:not [:= 123 456]]]]}
         ;"rule name 2" {:rule [:or [:not [:> 1 2]] [:and [:= 1 1] [:= 2 2]]]}
         "rule name 3" {:rule [:and [:= 1 1] [:= 2 2]]}
         "rule name 4" {:rule [:or [:> 1 2] [:and [:= 1 1] [:= 2 2]]]}
         "rule name 5" {:rule [:= 2 2]}
         "rule name 6" {:rule [:not [:= 2 2]]}
         "rule name 7" {:rule []}}
        )))

(re-frame/reg-event-db
 ::select-rule
 (fn [db [_ rule-name]]
   (assoc db :selected-rule rule-name)))

(re-frame/reg-sub
 ::selected-rule
 (fn [db _]
   (get db :selected-rule)))

(defn rules-list [ph]
  (let [rules @(re-frame/subscribe [::rules-map])
        selected-rule @(re-frame/subscribe [::selected-rule])]
    (re-com/v-box
     :padding "6px"
     :style {:border "1px solid red"
             :overflow "auto"}
     :gap "6px"
     :size "none"
     :height (px (- ph 64))
     :children (for [[name {:keys [rule]}] rules
                     :let [selected? (= name selected-rule)]]
                 [re-com/v-box
                  :padding "6px"
                  :size "none"
                  :style (merge {:border "1px solid blue"}
                                (when selected?
                                  {;:background-color "lightblue"
                                   :min-height "100px"}))
                  :children [[re-com/h-box
                              
                              :attr {:on-click #(re-frame/dispatch [::select-rule (if selected? nil name)])}
                              :style {:cursor "pointer" :font-size "19px"}
                              :justify :between
                              :height "25px"
                              :children [[re-com/box :child (str name)]
                                         [re-com/box :child "true" :style {:font-weight 700}]]]
                             [re-com/box :child
                              (if selected?
                                (visualize-clause rule 0)
                                (str rule))]]]))))
                 

(defn left-col [ph]
  [re-com/v-box
   :padding "6px"
   :width "35%"
   :style {:border "1px solid yellow"}
   :gap "6px"
   :children [[re-com/box 
               :padding "6px"
               :style {:border "1px solid yellow"}
               :child "operators"]
              [re-com/box
               :padding "6px"
               :style {:border "1px solid green"}
               :child "values"]
              [re-com/box 
               :padding "6px"
               :style {:border "1px solid grey"}
               :child "parameters"]
              ;[re-com/h-box :children []]
              ]])

(defn right-col [ph]
  [re-com/v-box
   :padding "6px"
   :width "65%"
   :style {:border "1px solid lime"}
   :children [
            ;;   [re-com/box 
            ;;    :padding "6px"
            ;;    :child "some stuff up here"]
              [rules-list ph]
              ;[re-com/h-box :children []]
              ]])

(defn rules-panel []
  (let [[_ hpct] db/flow-panel-pcts ;; [0.85 0.50]
        panel-height (* (.-innerHeight js/window) hpct)
        ;ph (- panel-height 10)
        details-panel-height (/ panel-height 1.25)
        ppanel-height (+ panel-height details-panel-height)
        ph (- ppanel-height 124)]
    [re-com/v-box
     :padding "6px"
   ;:size "auto" 
     :width (px (- (last @db/flow-editor-system-mode) 14))
     :size "none"
     :height (px ph)
     :style {:border "1px solid cyan"}
     :children [[re-com/box
                 :padding "6px"
                 :child "some stuff up here"]

                [re-com/gap :size "5px"]
                [re-com/h-box :children [[left-col ph]
                                         [right-col ph]]]]]))