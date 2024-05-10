(ns rvbbit-frontend.signals 
  (:require
   [reagent.core :as reagent]
   [re-frame.core :as re-frame]
   [re-frame.alpha :as rfa]
   [re-com.core :as re-com :refer [at]]
   [re-com.util :refer [px]]
   [rvbbit-frontend.connections :as conn]
   [re-catch.core :as rc]
   [rvbbit-frontend.utility :as ut]
   ;[rvbbit-frontend.buffy :as buffy]
   ;[clojure.data :as cdata]
   ;[re-pressed.core :as rp]
   ;[rvbbit-frontend.audio :as audio]
   [cljs.tools.reader :refer [read-string]]
   ;[rvbbit-frontend.shapes2 :as shape2]
   ;[rvbbit-frontend.shapes :as scrubbers]
  ;;  [garden.color :as c :refer [hex->hsl hsl->hex hsl hsla color? hex? invert mix
  ;;                              complement shades triad tetrad split-complement
  ;;                              analogous rgb->hsl rgb->hex]]
  ;;  [talltale.core :as tales]
   [day8.re-frame.undo :as undo :refer [undoable]]
   [cljs.core.async :as async :refer [<! >! chan]]
   ;["react" :as react]
   ;[rvbbit-frontend.resolver :as resolver]
   [clojure.edn :as edn]
   [rvbbit-frontend.db :as db]
   [rvbbit-frontend.subs :as subs]
   [rvbbit-frontend.http :as http]
   ;[goog.dom :as gdom]
   [rvbbit-frontend.bricks :as bricks :refer [theme-pull]]
   ;[goog.events :as gevents]
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
   ;["react-zoom-pan-pinch" :as zpan]
   [websocket-fx.core :as wfx]
   ;[cljs.tools.reader :refer [read-string]]
   ;[oz.core :as oz]
   ;[reagent.dom :as rdom]
   ;[websocket-fx.core :as wfx]
   ;[cljs-drag-n-drop.core :as dnd2]
   )
  (:import [goog.events EventType]
           [goog.async Debouncer]))

(re-frame/reg-event-db
 ::edit-signal-drop
 (undoable)
 (fn [db [_ content adding]]
   (tap> [:edit? content adding])
   (let [curr (get-in db [:signals-map (get db :selected-warren-item) :signal])]
     (assoc-in db [:signals-map (get db :selected-warren-item) :signal] 
               (walk/postwalk-replace
                {content
                 (vec (conj content (if (vector? adding) adding [adding])))
                                       ;(vec (cons adding content))
                                       ;[adding content]
                 } curr)))))

(re-frame/reg-event-db
 ::edit-signal-drop-kill
 (undoable)
 (fn [db [_ content]]
   (let [curr (get-in db [:signals-map (get db :selected-warren-item) :signal])]
     (tap> [:kill-nested? content :from curr])
     (assoc-in db [:signals-map (get db :selected-warren-item) :signal]
               (ut/remove-subvec curr content)))))

(re-frame/reg-event-db
 ::edit-signal-drop2
 (undoable)
 (fn [db [_ content adding]]
   (tap> [:edit2? content adding])
   (let [curr (get-in db [:signals-map (get db :selected-warren-item) :signal])]
     (assoc-in db [:signals-map (get db :selected-warren-item) :signal]
               (walk/postwalk-replace
                {content (vec (conj content adding))} curr)))))

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
                (ut/tracked-dispatch [::edit-signal-drop (vec (cons operator content)) ii])
                (tap> [:drop-signal-item ii incoming content]))}
   element])

(defn droppable-area2 [element types-vec content]
  [(reagent/adapt-react-class rdnd/Droppable)
   {:types   types-vec
    :on-drop #(let [incoming   (js->clj %)
                    incoming (into [] (for [[k v] incoming] {k (edn/read-string v)}))
                    ii (last (last (first incoming)))]
                (ut/tracked-dispatch [::edit-signal-drop2 content ii])
                (tap> [:drop-signal-item2 ii incoming content]))}
   element])

(defn kill-nest [op]
  [re-com/box :child 
   [re-com/md-icon-button :src (at)
    :md-icon-name "zmdi-close"
    :style {:font-size "14px" 
            :opacity 0.3
            :padding-right "5px"}]
   ;:padding "6px"
   :style {:cursor "pointer"}
   :attr {:on-click #(do
                       (tap> [:kill-nested op])
                       (ut/tracked-dispatch [::edit-signal-drop-kill op]))}])

(def hover-tools-atom (reagent/atom nil))

(defn is-true? [coll full-coll] 
  (let [idx (.indexOf (vec (ut/where-dissect full-coll)) coll)
        selected-warren-item @(ut/tracked-subscribe [::selected-warren-item])
        sigkw (keyword (str "signal/part-" (cstr/replace (str selected-warren-item) ":" "") "-" idx))
        vv @(ut/tracked-subscribe [::conn/clicked-parameter-key [sigkw]])
        ;;vv (if (nil? vv) "err!" vv)
        ] vv))

(declare highlight-code)
(declare unhighlight-code)
   
(defn hover-tools [coll full-coll]
  (let [hovered? (= @hover-tools-atom coll)
        idx (.indexOf (vec (ut/where-dissect full-coll)) coll)
        selected-warren-item @(ut/tracked-subscribe [::selected-warren-item])
        sigkw (keyword (str "signal/part-" (cstr/replace (str selected-warren-item) ":" "") "-" idx))
        vv @(ut/tracked-subscribe [::conn/clicked-parameter-key [sigkw]])
        vv (if (nil? vv) "err!" vv)]
    ;(tap> [:coll-hover coll])
    [re-com/v-box
     :attr {:on-mouse-enter #(reset! hover-tools-atom coll)
            ;; :on-mouse-over #(when (not= @hover-tools-atom coll)
            ;;                   (highlight-code (str coll))
            ;;                   (reset! hover-tools-atom coll))
            :on-mouse-over #(do
                              ;(tap> "on-mouse-over")
                              (when true ;(not= @hover-tools-atom coll)
                                ;(tap> "before highlight-code")
                                (highlight-code (str coll))
                                ;(tap> "after highlight-code")
                                (reset! hover-tools-atom coll)))
            :on-mouse-leave #(do 
                               (unhighlight-code)
                               (reset! hover-tools-atom nil))}
    ;;  :style (when hovered? {:background-color "rgba(0, 0, 255, 0.3)"})
     ;:style {:border "1px solid black"}
     :align :center :justify :center 
     :width "40px"
     ;:padding "10px"
     :children [[re-com/box 
                 ;:padding "6px"
                 :style (merge
                         {;:border "1px solid yellow" 
                          :font-size "12px"}
                         (when (not (true? vv)) {:opacity 0.2})
                         ;(when (true? vv) {:font-weight 700})
                         )
                 ;:width "33px"
                 :child (str vv)]
                [kill-nest coll]
                ]]))

 
(defn visualize-clause
  [clause level & [contents full-clause]]
  (let [is-operator? (fn [c] (and (vector? c) (keyword? (first c))
                                  (contains? #{:and :or :not} (first c))))
        ;_ (tap> [:clause contents])
        render-operator (fn [operator contents lvl]
                          (let [body (vec (cons operator contents))
                                hovered? (= @hover-tools-atom body)]
                            [re-com/h-box
                             :style (merge
                                     {;:transition "all 0.2s"
                                      ;:border "1px solid transparent"
                                      }
                                     (when hovered?
                                       {;:border "1px solid red"
                                        :background-color (str (theme-pull :theme/editor-outer-rim-color nil) 44)
                                        :filter "invert(133%)"}
                                      ;{:background-color "rgba(0, 0, 255, 0.3)"}
                                       )
                                     (when (is-true? body full-clause) 
                                       ;{:filter "brightness(233%)"}
                                       {;:background-color (theme-pull :theme/editor-outer-rim-color nil)
                                        ;:transform "scale(1.03)"
                                        }
                                       )
                                     )
                           ;:justify :between
                             :align :center 
                             ;:padding "6px"
                             :justify :between 
                             :children [[re-com/v-box
                                         :size "auto" :width (px (- 540 (* lvl 35)))
                                         :children (cons [droppable-area [re-com/box :child (str operator)] [:operator] contents operator]
                                                         (map #(visualize-clause % (inc lvl) contents full-clause) contents))
                                         :gap "8px"
                                         :style (merge 
                                                 {:border (str "3px solid " (theme-pull :theme/editor-outer-rim-color nil) 45) 
                                                  :padding "5px"
                                                  :border-radius "8px"
                                                  :background-color (str (theme-pull :theme/editor-outer-rim-color nil) "08")
                                                 ;:font-size "17px"
                                                 :font-weight 700
                                                 :margin-left (if (is-true? body full-clause)
                                                                (str (* lvl 11) "px")
                                                                (str (* lvl 10) "px"))
                                                  }
                                                 (when (is-true? body full-clause)
                                                   {:border (str "3px solid " (theme-pull :theme/editor-outer-rim-color nil) 99)
                                                    :background-color (str (theme-pull :theme/editor-outer-rim-color nil) 44)}))]

                                        [hover-tools body full-clause]]]))
        render-condition (fn [condition level contents]
                           (let [bbox [re-com/h-box
                                       :children [[re-com/box :child (str condition) :size "auto"]]
                                       :style {;:border "1px solid blue"
                                               :border (str "3px solid " (theme-pull :theme/editor-outer-rim-color nil) 45)
                                               :border-radius "8px"
                                               ;:overflow "hidden"
                                               :max-width 230
                                               :padding "5px"}]
                                 bbox (if (= level 0) [re-com/box ;; TODO if is only one, looks weird 
                                                       ;:margin "6px"
                                                       :child bbox] bbox)]
                             [droppable-area2 bbox [:operator] contents]))]
    (if (vector? clause)
      (if (is-operator? clause)
        (render-operator (first clause) (rest clause) level)
        (let [;words (ut/deep-flatten clause)
              ;lw (last words)
              hovered? (= @hover-tools-atom clause)]
          [re-com/h-box
           ;:padding "6px"
           :style (merge
                   {;:transition "all 0.2s"
                    ;:border-radius "8px"
                    :border "3px solid transparent"
                    ;:border (str "3px solid " (theme-pull :theme/editor-outer-rim-color nil) 45)
                    }
                   (when hovered? 
                     ;;{:background-color "rgba(0, 0, 255, 0.3)"}
                     {;;:border "1px solid red"
                      :background-color (theme-pull :theme/editor-outer-rim-color nil)
                      :filter "invert(133%)"
                      }
                         )
                   (when (is-true? clause full-clause)
                                                          ;{:filter "brightness(233%)"}
                     {
                      ;:transform "scale(1.03)"
                      :border-radius "8px"
                      :border (str "3px solid " (theme-pull :theme/editor-outer-rim-color nil) 99)
                      :background-color (str (theme-pull :theme/editor-outer-rim-color nil) 44)
                      })
                   {;:font-size "17px"
                    ;:overflow "hidden"
                    ;:max-width 200
                    :font-weight 300
                    :margin-left (if (is-true? clause full-clause)
                                  (str (* level 11) "px")
                                  (str (* level 10) "px"))})
           :justify :between  
           :align :center ;:padding "6px"
           :children
           [[re-com/h-box
           ;:size "auto"
             
             :children (map #(visualize-clause % level clause full-clause) clause)
             :gap "10px"
             :style {;:border "1px solid #ccc" 
                     :border-radius "8px"
                     :border (str "3px solid " (theme-pull :theme/editor-outer-rim-color nil) 25)
                     :background-color (str (theme-pull :theme/editor-outer-rim-color nil) "08")
                     :padding "5px"}]
            ;;[re-com/box :padding "6px" :child (if (= lw 456) "false" "true")]
            [hover-tools clause full-clause]
            ]])) ; Keep pairings on the same row
      (render-condition clause level contents))))

(def cm-instance (atom nil))
(def markers (atom []))

(declare highlight-code)

(defn code-box [width-int height-int value ttype]
  (let [signal? (= ttype :signal)] 
    [re-com/box
     :size "none"
     ;:width (px (- width-int 24))
     :height (px (- height-int 24))
     :style {:font-family   (theme-pull :theme/monospaced-font nil) ; "Chivo Mono" ;"Fira Code"
             :font-size     "17px"
             :overflow      "auto"
             :border-radius "12px"
             :background-color "#000000"
             ;:border "1px solid yellow"
             :font-weight   700}
     :child [(reagent/adapt-react-class cm/UnControlled)
             {:value   (ut/format-map 640 ;; (- width-int 95)
                                      (str value))
              ;:value  (str value)
              :onBeforeChange (fn [editor data value]
                                (reset! cm-instance editor))
              :onBlur  #(let [parse        (try (read-string
                                                 (cstr/join " " (ut/cm-deep-values %)))
                                                (catch :default e [:cannot-parse (str (.-message e))]))
                              unparseable? (= (first parse) :cannot-parse)]
                          (cond unparseable?
                            ; (js/alert "BAD FORM MATE!!")
                                (do (reset! db/bad-form-signals? true)
                                    (reset! db/bad-form-msg-signals (str (last parse))))

                                (empty? parse)
                                (do (reset! db/bad-form-signals? true)
                                    (reset! db/bad-form-msg-signals "Empty signal"))

                                (and (not (vector? parse)) signal?)
                                (do (reset! db/bad-form-signals? true)
                                    (reset! db/bad-form-msg-signals "Needs to be a vector / honey-sql where clause format"))
 
                                :else (if signal?
                                        
                                       (do (reset! db/bad-form-signals? false)
                                          (ut/tracked-dispatch [::edit-signal parse]))
                                        
                                        (do (reset! db/bad-form-signals? false)
                                            (ut/tracked-dispatch [::edit-basic parse ttype])))
                                ))
              :options {:mode              "clojure"
                        ;:lineWrapping      true
                        :lineNumbers       false ;true
                        :matchBrackets     true
                        :autoCloseBrackets true
                        :autofocus         false
                        :autoScroll        false
                        :detach            true
                        :readOnly          false
                        :theme             (theme-pull :theme/codemirror-theme nil)}}]]))


(defn highlight-code [code]
  (when-let [editor @cm-instance]
    (let [doc (.getDoc editor)
          code (ut/format-map 640 ;; 539 ;;(- 634.4 95) 
                              (str code))
          _ (tap> [:highlighted-code code])]
      ;; Clear existing markers
      (doseq [marker @markers]
        (.clear marker))
      (reset! markers [])
      (let [code-lines (clojure.string/split-lines code)
            start-line (loop [line 0]
                         (when (< line (.lineCount doc))
                           (if (clojure.string/includes? (.getLine doc line) (first code-lines))
                             line
                             (recur (inc line)))))
            end-line (loop [line start-line]
                       (when (< line (.lineCount doc))
                         (if (clojure.string/includes? (.getLine doc line) (last code-lines))
                           line
                           (recur (inc line)))))
            start-ch (clojure.string/index-of (.getLine doc start-line) (first code-lines))
            end-ch (+ (clojure.string/index-of (.getLine doc end-line) (last code-lines)) (count (last code-lines)))
            marker (try
                     (.markText doc
                                #js {:line start-line, :ch start-ch}
                                #js {:line end-line, :ch end-ch}
                                #js {:css (str "filter: invert(233%); color: white; font-weight: 700; background-color: teal; padding: 4px;" )})
                     (catch :default e (tap> [:marker-error (str e)])))]
        (swap! markers conj marker)))))

(defn unhighlight-code []
  (when-let [editor @cm-instance]
    (let [doc (.getDoc editor)]
      (doseq [marker @markers]
        (when marker
          (.clear marker)))
      (reset! markers []))))

(re-frame/reg-sub
 ::signals-map
 (fn [db _]
   (get db :signals-map {})))

(re-frame/reg-sub
 ::solvers-map
 (fn [db _]
   (get db :solvers-map {})))

(re-frame/reg-sub
 ::rules-map
 (fn [db _]
   (get db :rules-map {})))

(re-frame/reg-event-db
 ::select-signal
 (fn [db [_ signal-name]]
   (assoc db :selected-warren-item signal-name)))

(re-frame/reg-sub
 ::operators
 (fn [db _]
   (get-in db [:signal-items :operators]
           [:= :<> :> :< :>= :<= :and :or :not :like :changed?])))

(re-frame/reg-sub
 ::conditions
 (fn [db _]
   (get-in db [:signal-items :conditions]
           [:and :or :not])))

(re-frame/reg-sub
 ::time-items
 (fn [db _]
   (get-in db [:signal-items :time-items]
           [[:= :day 1]
            [:= :hour 13]
            [:= :month 1]
            [:= :year 2021]
            ])))

(re-frame/reg-sub
 ::selected-warren-item
 (fn [db _]
   (get db :selected-warren-item)))

(re-frame/reg-event-db
 ::edit-signal
 (undoable)
 (fn [db [_ signal]]
   (assoc-in db [:signals-map (get db :selected-warren-item) :signal] signal)))

(re-frame/reg-event-db
 ::edit-basic
 (undoable)
 (fn [db [_ parsed ttype]] 
   (let [sver (fn [x] (cstr/replace (str x) ":" ""))
         item-type-str (sver ttype)
         item-type-key (keyword (str item-type-str "s-map"))]
     (assoc-in db [item-type-key (get db :selected-warren-item)] parsed))))

(re-frame/reg-event-db
 ::delete-signal
 (undoable)
 (fn [db [_ signal-name item-type]]
   (let [sver (fn [x] (cstr/replace (str x) ":" ""))
         item-type-str (sver item-type)
         item-type-key (keyword (str item-type-str "s-map"))] ;; :signals-map , etc
     (-> db
         (ut/dissoc-in [item-type-key signal-name])
         (assoc :selected-warren-item nil)))))

(re-frame/reg-event-db
 ::add-signal
 (undoable)
 (fn [db [_ item-type]]
   (let [item-type (keyword (ut/drop-last-char item-type))
         rr (keyword (ut/gen-signal-name item-type))
         existing-keys (vec
                        (into (into (keys (get db :signals-map {}))
                                    (keys (get db :solvers-map {})))
                              (keys (get db :rules-map {}))))
         signal-name (ut/safe-key rr existing-keys)
         sver (fn [x] (cstr/replace (str x) ":" ""))
         item-type-str (sver item-type)
         item-type-key (keyword (str item-type-str "s-map"))
         starting-data (cond (= item-type :signal) [:and [:= :day 1] [:= :hour 9]]
                             (= item-type :solver) {:signal nil :body nil}
                             :else {:flow-id nil :signal nil :overrides {}})
         kp (if (= item-type :signal)
              [:signals-map signal-name :signal]
              [item-type-key signal-name])]
     (tap> [:add-item? item-type signal-name kp rr])
     (-> db
         (assoc-in kp starting-data)
         (assoc :selected-warren-item signal-name)))))

(defn code-box-smol [width-int height-int value & [syntax]]
  [re-com/box
   :size "auto"
     ;:width "100%" ;(px (- width-int 24))
     ;:height (px (- height-int 24))
   :style {:font-family   (theme-pull :theme/monospaced-font nil) ; "Chivo Mono" ;"Fira Code"
           :font-size     "11px"
           :overflow      "auto"
           :border-radius "12px"
           :font-weight   700}
   :child [(reagent/adapt-react-class cm/UnControlled)
           {:value   (ut/format-map 350 ;; (- width-int 95)
                                    (str value))
            :options {:mode              "clojure"
                      :lineWrapping      true
                      :lineNumbers       false ; true
                      :matchBrackets     true
                      :autoCloseBrackets true
                      :autofocus         false
                      :autoScroll        false
                      :detach            true
                      :readOnly          true            ;true
                      :theme             (theme-pull :theme/codemirror-theme nil)}}]]) ;"ayu-mirage" ;"hopscotch"


(defn items-list [ph signals selected-warren-item warren-type-string]
  (let [;signals @(ut/tracked-subscribe [::signals-map])
        ;selected-warren-item @(ut/tracked-subscribe [::selected-warren-item])
        warren-item-type @(ut/tracked-sub ::warren-item-type {})
        ssr (cstr/replace (str @(ut/tracked-sub ::warren-item-type {})) ":" "")
        selected-type (str ssr "s")
        results (into {} (for [[name _] signals]
                           (let [sigkw (keyword (str "signal/" (cstr/replace (str name) ":" "")))
                                 vv (if (= warren-type-string "signals")
                                      @(ut/tracked-subscribe [::conn/clicked-parameter-key [sigkw]])
                                      "")]
                             {name vv})))
        ;; warren-type-string-label (if (cstr/ends-with? warren-type-string "s")
        ;;                            (ut/drop-last-char warren-type-string)
        ;;                            warren-type-string)
        ]
    ;; (tap> [:warren selected-type warren-type-string])
    [re-com/v-box
     :children
     [[re-com/h-box
       :height "30px" :padding "6px"
       :align :center :justify :between
       :style {:font-size "12px"}
       :children [[re-com/box
                   :style {:cursor "pointer"}
                   :attr {:on-click #(ut/tracked-dispatch [::add-signal warren-type-string])}
                   :child (str "+ new " warren-type-string)]
                  (if (and selected-warren-item
                           (= warren-type-string selected-type))
                    [re-com/box
                     :style {:cursor "pointer"}
                     :attr {:on-click #(ut/tracked-dispatch [::delete-signal selected-warren-item warren-item-type])}
                     :child (str "- delete this " warren-type-string "?")]
                    [re-com/gap :size "5px"])]]
      [re-com/v-box
       :padding "6px"
       :style {;:border "1px solid red"
               :overflow "auto"}
       :gap "6px"
       :size "none"
       ;:height (px (- ph 94))
       :children (for [[name {:keys [signal]}] signals
                       :let [selected? (= name selected-warren-item)
                             sigkw (keyword (str "signal/" (cstr/replace (str name) ":" "")))
                             vv (get results name)]]
                   
                   [draggable-item
                    [re-com/v-box
                     :padding "6px"
                     :size "none"
                     :width "100%"
                     :attr {:on-double-click #(ut/tracked-dispatch [::select-signal (if selected? nil name)])}
                     :style {:border (if selected?
                                       (str "3px dashed " (theme-pull :theme/editor-outer-rim-color nil) 99)
                                       (str "1px solid " (theme-pull :theme/editor-outer-rim-color nil) "28")
                                       )
                             :background-color (cond selected? ;;(or selected? (true? vv))
                                                     (str (theme-pull :theme/editor-outer-rim-color nil) 65) ;; "rgba(0, 0, 255, 0.3)"

                                                     (true? vv)
                                                     (str (theme-pull :theme/editor-outer-rim-color nil) 30) ;; "rgba(0, 0, 255, 0.3)"

                                                     :else "rgba(0, 0, 0, 0.1)")
                             :cursor "pointer"}
                     :children [[re-com/h-box
                                 :style {:font-size "19px"}
                                 :justify :between
                                 ;:height "25px"

                                 :children [[re-com/box 
                                             :style {:font-size "15px"
                                                     ;:border "1px solid yellow"
                                                     :font-weight 700}
                                             ;:width "200px"
                                             :padding "6px"
                                             :size "auto"
                                             :child (str name)]
                                            [re-com/box
                                             :align :center :justify :center
                                             :child (str (if (nil? vv) "" vv))
                                             :style {:font-weight 700
                                                     :font-size "15px"
                                                     :opacity (if (true? vv) 1.0 0.2)}]]]
                                ;; (when true ;;(not selected?)
                                ;;   [re-com/box
                                ;;    :padding "4px"
                                ;;    :style {;:opacity 0.33
                                ;;            :font-size "11px"
                                ;;            :border-radius "7px"
                                ;;            :background-color "#00000099"
                                ;;            :font-family  (theme-pull :theme/monospaced-font nil)}
                                ;;    :child [code-box-smol 60 60 (str signal) "clojure"]])
                                ]]
                                 :operator sigkw
                                 ])]]]))








(defn selector-panel [name items icon & [style wide?]]
  ;; (tap> [:db/selectors-open @db/selectors-open])
  (let [open? (some #(= name %) @db/selectors-open)
        items-count (count items)
        partition? (integer? wide?)]
    [re-com/v-box
     :padding "6px"
     :style (merge {:border (str "2px solid " (theme-pull :theme/editor-outer-rim-color nil) 55)
                    :border-radius "4px"}
                   style)
     :children [[re-com/h-box
                 :height "28px"
                 :style {:cursor "pointer" 
                         :user-select "none"
                         :color (theme-pull :theme/editor-outer-rim-color nil)}
                 :justify :between :align :center
                 :attr {:on-click #(if open?
                                     (reset! db/selectors-open (remove (fn [x] (= x name)) @db/selectors-open))
                                     (swap! db/selectors-open conj name))}
                 :children [[re-com/h-box
                             :style {:font-size "22px"}
                             :gap "8px" :align :center :justify :center 
                             :children [[re-com/box :child name]
                                        [re-com/box
                                         :style {:opacity 0.55 :font-size "15px"}
                                         :child (str "(" items-count ")")]]]
                            ;;[re-com/box :child (str items-count)]
                            [re-com/md-icon-button :src (at)
                             :md-icon-name icon
                             :style {;:color (if open?
                                     ;         (str (theme-pull :theme/editor-outer-rim-color nil) 99)
                                     ;         (str (theme-pull :theme/editor-outer-rim-color nil) 45))
                                     :color (theme-pull :theme/editor-outer-rim-color nil)
                                     :font-size "14px"}]]]
                (when open?
                  
                  (if (or (= name "solvers") (= name "rules") (= name "signals"))

                    (let [;signals @(ut/tracked-subscribe [::signals-map]) ;; they get passed from the filter col
                          selected-warren-item @(ut/tracked-sub ::selected-warren-item {})
                          ;signal-vec (get-in signals [selected-warren-item :signal])
                          ph 200]

                      [items-list (if selected-warren-item (* ph 0.3) ph) items selected-warren-item name])

                    (if partition?
                      [re-com/v-box
                       :style {:margin-top "6px"}
                       :children (for [seg (partition-all wide? items)]
                                   [re-com/h-box ;;:size "auto"
                                    :align :center :justify :center
                                    :children (for [item seg
                                                    :let [pw (* (- (last @db/flow-editor-system-mode) 70) 0.35)
                                                          width (- (/ pw wide?) 10)]]
                                                [draggable-item
                                                 [re-com/box
                                                  :width (px width)
                                                ;;:size "auto"
                                                  :padding "6px"
                                                  :style {:border "1px solid purple"
                                                          :background-color "#DA70D624"
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
                                     :child (str item)] :operator item])])))]]))

(re-frame/reg-sub
 ::signal-flow-id
 (fn [db _]
   (get-in db [:signals-map (get db :selected-warren-item) :flow-id])))

(re-frame/reg-sub
 ::signal-open-inputs
 (fn [db _]
   (get-in db [:signals-map (get db :selected-warren-item) :open-inputs])))

(re-frame/reg-event-db
 ::edit-signal-flow-id
 (undoable)
 (fn [db [_ flow-id]]
   (assoc-in db [:signals-map (get db :selected-warren-item) :flow-id] flow-id)))

(re-frame/reg-event-db
 ::open-blocks
 ;;(undoable)
 (fn [db [_ result]]
   (-> db
       (assoc-in [:signals-map (get db :selected-warren-item) :open-inputs] (get result :open-inputs))
       (assoc-in [:signals-map (get db :selected-warren-item) :blocks] (get result :blocks)))))

;; (re-frame/reg-event-db
;;  ::refresh-open-blocks
;;  ;;(undoable)
;;  (fn [db [_ flow-id]]
;;    ;(tap> [:refresh-runstream-panel (keys (get db :runstreams))])
;;    (ut/tracked-dispatch [::wfx/request :default
;;                        {:message    {:kind :get-flow-open-ports
;;                                      :flow-id flow-id
;;                                      :flowmap flow-id
;;                                      :client-name (get db :client-name)}
;;                         :on-response [::open-blocks]
;;                         :timeout    500000}])
;;    db))

(re-frame/reg-event-fx
 ::refresh-open-blocks
 (fn [{:keys [db]} [_ flow-id]]
   {:db db
    :dispatch [::wfx/request :default
               {:message    {:kind :get-flow-open-ports
                             :flow-id flow-id
                             :flowmap flow-id
                             :client-name (get db :client-name)}
                :on-response [::open-blocks]
                :timeout    500000}]}))

(defonce mode-atom (reagent/atom "when logic"))

(defn flow-box [hh]
  (let [server-flows (map :flow_id @(ut/tracked-subscribe [::conn/sql-data [:flows-sys]]))
        sql-calls {:flows-sys {:select [:flow_id :file_path :last_modified]
                               :from [:flows]
                               :connection-id "flows-db"
                               :order-by [[3 :desc]]}}
        open-inputs @(ut/tracked-subscribe [::signal-open-inputs])
        react! [@mode-atom]
        ;signals @(ut/tracked-subscribe [::signals-map])
        ;selected-warren-item @(ut/tracked-subscribe [::selected-warren-item])
        ;signal-vec (get-in signals [selected-warren-item :signal])
        ;flow-id (get-in signals [selected-warren-item :flow-id])
        ww (* (- (last @db/flow-editor-system-mode) 14) 0.318)
        fid @(ut/tracked-subscribe [::signal-flow-id])
        ]

    (dorun (for [[k query] sql-calls]
             (let [data-exists? @(ut/tracked-subscribe [::conn/sql-data-exists? [k]])
                   unrun-sql? @(ut/tracked-subscribe [::conn/sql-query-not-run? [k] query])]
               (when (or (not data-exists?) unrun-sql?)
                 (if (get query :connection-id)
                   (conn/sql-data [k] query (get query :connection-id))
                   (conn/sql-data [k] query))))))

    [re-com/v-box
     ;:style {:font-family   (theme-pull :theme/monospaced-font nil)}
     :padding "6px"
     :children [[re-com/box
                 :padding "6px"
                 ;:style {:font-family   (theme-pull :theme/monospaced-font nil)}
                 :child "flow to run when signals are met:"]
                (if fid
                  [re-com/h-box
                   :align :center
                   :justify :between
                   :padding "6px"
                   :width "320px"
                   :height "35px"
                   :style {;:border "1px solid #ffffff33"
                           :font-family   (theme-pull :theme/monospaced-font nil)}
                   :children [[re-com/box 
                               :style {:overflow "hidden"
                                       ;:border "1px solid yellow"
                                       :font-size "17px"
                                       :white-space "nowrap"
                                       :max-width "292px"}
                               :child (str fid)]
                              [re-com/box
                               :style {:cursor "pointer"}
                               :attr {:on-click #(ut/tracked-dispatch [::edit-signal-flow-id nil])}
                               :child "x"]]]
                  [re-com/typeahead
                   :suggestion-to-string (fn [item]
                                           (str (get item :label)))
                   :render-suggestion (fn [ss _] ;; render in dropdown
                                        [re-com/box
                                         :style {:color "#000000"}
                                         :child (str (get ss :label))])
                   :on-change #(do
                                 (ut/tracked-dispatch [::refresh-open-blocks (get % :id)])
                                 (ut/tracked-dispatch [::edit-signal-flow-id (get % :id)]))
                   :rigid? true
                   :style {:font-family   (theme-pull :theme/monospaced-font nil)}
                   :width "320px"
                   :change-on-blur? true
                   :placeholder (or fid "search for a server flow to run...")
                                   ;:style {:color "#000000"}
                   :data-source (fn [x]
                                  (let [flow-parts (vec (map (fn [n] {:id n :label n}) server-flows))
                                        words (cstr/split (cstr/lower-case (cstr/trim x)) #" ")
                                        matches-word (fn [field word] (cstr/includes? (cstr/lower-case (str field)) word))]
                                    (if (or (nil? x) (empty? x)) flow-parts
                                        (filter (fn [item]
                                                  (let [label (get item :label)]
                                                    (every? (fn [word]
                                                              (matches-word label word))
                                                            words)))
                                                flow-parts))))])
                [re-com/gap :size "5px"]
                [re-com/h-box
                 ;:size "auto" 
                 :justify :between :align :center
                 :children (for [btn ["input values" "when logic"]]
                             [re-com/v-box
                              :height "50px"
                              :align :center :justify :center 
                              :size "auto"
                              :style (merge
                                      {;:border "1px solid pink"
                                       :cursor "pointer"}
                                      (if (= btn @mode-atom)
                                        {:background-color "rgba(0, 0, 255, 0.3)"}
                                        {:background-color "rgba(0, 0, 0, 0.1)"}))
                              :attr {:on-click #(reset! mode-atom btn)}
                              :children [[re-com/box :child "configure"] 
                                         [re-com/box :child (str btn)]]])]
                ;; [re-com/box
                ;;  :size "none"
                ;;  :height (px (- hh 88))
                ;;  :style {:overflow "auto"
                ;;          :border "1px solid pink"}
                ;;  :child [re-com/v-box
                ;;          :children (for [seg (partition-all 2 open-inputs)]
                ;;                      [re-com/h-box
                ;;                       ;:padding "6px"
                ;;                       :justify :between :align :center
                ;;                       :children (for [[k v] seg]
                ;;                                   [re-com/v-box
                ;;                                    ;:size "auto"
                ;;                                    :width (px (* ww 0.5))
                ;;                                    :style {:border "1px solid orange"
                ;;                                            :padding "6px"}
                ;;                                    :children [[re-com/box :child (str k)]
                ;;                                               [re-com/box :child (str (get v :type))]
                ;;                                               ]])])]]
                ]
     ;:style {:border "2px solid maroon"}
     :height (px hh)]))

(defonce searcher-atom (reagent/atom nil))

(defn left-col [ph]
  (let [operators @(ut/tracked-sub ::operators {})
        conditions @(ut/tracked-sub ::conditions {})
        time-items @(ut/tracked-sub ::time-items {})

        signals @(ut/tracked-sub ::signals-map {})
        solvers @(ut/tracked-sub ::solvers-map {})
        rules @(ut/tracked-sub ::rules-map {})

        selected-warren-item @(ut/tracked-sub ::selected-warren-item {})

        signal-vec (get-in signals [selected-warren-item :signal])
        react! [@db/selectors-open]
        filter-results (fn [x y]
                         (if (or (nil? x) (empty? x))
                           y
                           (if (map? y)
                             (into {} (filter (fn [[k v]]
                                                (or (cstr/includes? (cstr/lower-case (str k)) (cstr/lower-case x))
                                                    (cstr/includes? (cstr/lower-case (str v)) (cstr/lower-case x))))
                                              y))
                             (vec (filter #(cstr/includes? (cstr/lower-case (str %)) (cstr/lower-case x)) y)))))
        ;fid @(ut/tracked-subscribe [::signal-flow-id])
        flow-box-hh 148]
    [re-com/v-box
     ;:padding "6px"
     :gap "6px"
     :width "35%"
     :children
     [;(when selected-warren-item
      ;  [flow-box (- flow-box-hh 10)])
      [re-com/h-box
       :padding "6px"
       :height "50px"
       :align :center :justify :between
       ;:style {:border "1px solid orange"}
       :children [[re-com/input-text
                   :src (at)
                   :model             searcher-atom
                   :width             "93%"
                   :on-change #(reset! searcher-atom (let [vv (str %) ;; (cstr/trim (str %))
                                                           ]
                                                       (if (empty? vv) nil vv)))
               ;:validation-regex  flow-id-regex
                   :placeholder "(search filter)"
                   :change-on-blur?   false
                   :style  {:text-decoration (when (ut/ne? @searcher-atom) "underline")
                            :color "inherit"
                        ;:margin-top "3px"
                        ;:margin-left "-4px"
                            :outline "none"
                            :text-align "center"
                            :background-color "#00000000"}]
                  [re-com/box
                   :style {;:border "1px solid maroon"
                           :opacity (if @searcher-atom 1.0 0.45)
                           :cursor "pointer"}
                   :width "20px" :align :center :justify :center
                   :attr {:on-click #(reset! searcher-atom nil)}
                   :child "x"]]]
      [re-com/v-box
       :padding "6px"
       ;:width "35%"
       :height (px (- ph 100 ;; 50
                      ;(if selected-warren-item (+ flow-box-hh 50) 50)
                      ))
       :style {;:border "1px solid yellow"
               :overflow "auto"}
       :gap "6px"
       :children [

                  [selector-panel "operators" (filter-results @searcher-atom operators) "zmdi-puzzle-piece" {} 3]
                  [selector-panel "conditions" (filter-results @searcher-atom conditions) "zmdi-puzzle-piece" {} 3]
                  [selector-panel "time items" (filter-results @searcher-atom time-items) "zmdi-calendar-alt" {} 2]

                  [selector-panel "parameters" (filter-results @searcher-atom time-items) "zmdi-shape" {} 2]
                  [selector-panel "flow values" (filter-results @searcher-atom time-items) "zmdi-shape" {} 2]
                  [selector-panel "clients" (filter-results @searcher-atom time-items) "zmdi-desktop-mac" {} 2]

                  [selector-panel "metrics" (filter-results @searcher-atom time-items) "zmdi-equalizer" {} 2]
                  [selector-panel "KPIs" (filter-results @searcher-atom time-items) "zmdi-traffic" {} 2]
                  
                  ;[selector-panel "instanced signals" (filter-results @searcher-atom signals) "zmdi-flash" {} 5]
                  [re-com/gap :size "20px"]

                  [selector-panel "signals" (filter-results @searcher-atom signals) "zmdi-flash" {} 5]
                  [selector-panel "rules" (filter-results @searcher-atom rules) "zmdi-flash" {} 5]
                  [selector-panel "solvers" (filter-results @searcher-atom solvers) "zmdi-flash" {} 5]

                  [re-com/gap :size "10px"]

                  [re-com/h-box
                   :size "auto"
                   :gap "8px"
                   :children (for [e ["solver" "rule" "signal"]]
                               [re-com/v-box 
                                :padding "4px"
                                :style {:border (str "1px solid " (theme-pull :theme/editor-outer-rim-color nil) 55) 
                                        :font-size "14px"
                                        :color (theme-pull :theme/editor-outer-rim-color nil)
                                        :border-radius "4px"}
                                :size "auto" 
                                :height "50px"
                                :align :center 
                                :justify :center
                                :children [[re-com/box :child (str "+ new " e )]
                                           [re-com/box :child (str "group")]]])]
                  
                  [re-com/gap :size "20px"]
                  
                  ]]]]))


(re-frame/reg-event-db
 ::rename-item
 (undoable)
 (fn [db [_ old new item-type]]
   (let [sver (fn [x] (cstr/replace (str x) ":" ""))
         item-type-str (sver item-type)
         item-type-key (keyword (str item-type-str "s-map"))
         
         refs {(keyword (str item-type-str "/" (sver old)))
               (keyword (str item-type-str "/" (sver new)))}
         ;;old-item (get-in db [item-type-key old])
         ]
     
     (tap> [:rename-item! old new item-type item-type-str item-type-key refs])

     (-> db

         (assoc :signals-map  (walk/postwalk-replace (if (= item-type-key :signals-map) (merge {old new} refs) refs) (get db :signals-map {}))) ;; change all fqns refs
         (assoc :rules-map    (walk/postwalk-replace (if (= item-type-key :rules-map) (merge {old new} refs) refs) (get db :rules-map {})))
         (assoc :solvers-map  (walk/postwalk-replace (if (= item-type-key :solvers-map) (merge {old new} refs) refs) (get db :solvers-map {})))

         (assoc :selected-warren-item new))))) ;; set as selected item



(defonce title-edit-idx (reagent/atom nil))

(defn edit-item-name [item-name item-type w]
  (let [;read-only-flow? (true? (cstr/includes? flow-id "/"))
        ;flow-id-regex #"^[a-zA-Z0-9_-]+$"  ;; alpha, underscores, hypens, numbers
        flow-id-regex #"^[a-zA-Z0-9_?\-]+$"  ;; alpha, underscores, hypens, numbers, question marks
        ]
    (if (not @title-edit-idx)
      [re-com/box
       :size "none"
       :height  "45px"
       :width (px w)
       :align :center
       :justify :start
       :attr {:on-double-click #(reset! title-edit-idx (str item-name))}
       :style {:cursor "pointer"
               :padding-right "12px"
               :padding-top "1px"
               :border "2px solid transparent"
               ;:border "2px solid yellow"
               :font-size "26px"}
       :child (str item-name)]
      [re-com/input-text
       :src (at)
       :model             (cstr/replace (str item-name) ":" "")
       :width             (px (- w 6))
       :height            "45px"
       :on-change         #(do (ut/tracked-dispatch
                                [::rename-item
                                 item-name
                                 (try (keyword (str %)) (catch :default _ (str %)))
                                 item-type])
                               (reset! title-edit-idx nil))
       :validation-regex  flow-id-regex
       :change-on-blur?   true
       :style  {:border (str "2px dashed " (theme-pull :theme/editor-outer-rim-color nil))
                :font-size "26px"
                :text-decoration "underline"
                :color (theme-pull :theme/editor-outer-rim-color nil)
                :font-style "underline"
                :text-align "left"
                :background-color "#00000000"}])))

(re-frame/reg-event-db
 ::run-signals-history
 (fn [db _]
  ;;  (tap> [:signals-history!])
   (ut/tracked-dispatch
    [::wfx/request :default
     {:message    {:kind :signals-history
                   :signal-name (get db :selected-warren-item)
                   :client-name @(ut/tracked-subscribe [::bricks/client-name])}
      :on-response [::signals-history-response]
      :timeout    15000000}])
   db))

(re-frame/reg-sub
 ::run-signals-history?
 (fn [db _]
   (let [signals (vec (keys (get db :signals-map)))
         sel (get db :selected-warren-item)]
     (and (get db :flow?)
          (not (nil? sel))
          (some #(= sel %) signals)
          (= (get @db/flow-editor-system-mode 0) "signals")))))

(re-frame/reg-sub
 ::warren-item-type
 (fn [db _]
   (let [name (get db :selected-warren-item)
         signals (keys (get db :signals-map))
         rules (keys (get db :rules-map))
         solvers (keys (get db :solvers-map))]
     (cond (some #(= name %) signals) :signal
           (some #(= name %) rules) :rule
           (some #(= name %) solvers) :solver
           :else :unknown))))

(defn history-mouse-enter [e]
  (reset! hover-tools-atom e))

(defn history-mouse-over [e]
  (when true ;; todo to short-circuit
    (highlight-code (str e))
    (reset! hover-tools-atom e)))

(defn history-mouse-leave []
  (unhighlight-code)
  (reset! hover-tools-atom nil))


(defn right-col [ph]
  (let [signals @(ut/tracked-sub ::signals-map {})
        selected-warren-item @(ut/tracked-sub ::selected-warren-item {})
        warren-item-type @(ut/tracked-sub ::warren-item-type {})

        sver (fn [x] (cstr/replace (str x) ":" ""))
        item-type-str (sver warren-item-type)
        item-type-key (keyword (str item-type-str "s-map"))

        signal? (true? (= warren-item-type :signal))

        signal-vec (get-in signals [selected-warren-item :signal])
        signal-vec-parts (vec (ut/where-dissect signal-vec))

        warren-item-type @(ut/tracked-sub ::warren-item-type {})
        ;; signals-history (select-keys 
        ;;                  ;;@(ut/tracked-subscribe [::signals-history])
        ;;                  @(rfa/sub ::signals-history {})
        ;;                  signal-vec-parts) ;; sometimes we get weird other items if we change fast

        signals-history (when signal? @(rfa/sub ::signals-history {}))

        other (cond (= warren-item-type :solver) (get @(ut/tracked-sub ::solvers-map {}) selected-warren-item)
                    (= warren-item-type :rule) (get @(ut/tracked-sub ::rules-map {}) selected-warren-item)
                    :else {})

        results (if signal?
                  (into {} (for [idx (range (count signal-vec-parts))]
                             (let [sigkw (keyword (str "signal/part-" (cstr/replace (str name) ":" "") "-" idx))
                                   name (get signal-vec-parts idx)
                                   vv @(ut/tracked-subscribe [::conn/clicked-parameter-key [sigkw]])]
                               {name vv})))
                  {})]

        ;; ^^^ this is a weird hack we need to revisit - by invoking the param-keys we are summoning them from the server to be read and reactive
        ;; even though it is dereffed in a "safe" place...
    
    ;; (tap> [:signals-history signals-history])
    
    ;; (ut/tracked-dispatch
    ;;  [::wfx/request :default
    ;;   {:message    {:kind :signals-history
    ;;                 :signal-name selected-warren-item
    ;;                 :client-name @(ut/tracked-subscribe [::bricks/client-name])}
    ;;    :on-response [::signals-history-response]
    ;;    :timeout    15000000}])
    

    ;; (tap> [:right-col ph signals selected-warren-item signal-vec @db/flow-editor-system-mode])
    [re-com/v-box
     :width "65%"
     :children [(when selected-warren-item
                  [re-com/v-box
                   :style {;:font-family   (theme-pull :theme/monospaced-font nil)
                           ;:border "1px solid pink"
                           }
                   :height (px (* ph 0.7))
                   :padding "6px"
                   :children
                   [(let [ww (* (- (last @db/flow-editor-system-mode) 70) 0.65)]
                      [re-com/h-box
                       :justify :between :align :center
                       :children
                       [[edit-item-name selected-warren-item warren-item-type (* ww 0.9)]
                        (if signal?
                          [re-com/box
                           :align :center :justify :center
                           :style {;:border "1px solid yellow"
                                   :font-size "22px"
                                   :font-weight 700}
                           :height "100%"
                           :width (px (* ww 0.1))
                           :child (str @(ut/tracked-subscribe [::conn/clicked-parameter-key
                                                               [(keyword (str "signal/" (cstr/replace (str selected-warren-item) ":" "")))]]))]
                          [re-com/gap :size "10px"] ;; placeholder
                          )]])

                    [re-com/box
                     :style {;:border "1px solid orange"
                             }
                     :height (px (- (* (* ph 0.7) 0.25) 12)) ;; "24%" 
                     :padding "6px"
                     :child [code-box
                             (* (- (last @db/flow-editor-system-mode) 14) 0.65) ;; width
                             (if signal? 
                               (- (* (* ph 0.7) 0.25) 5)
                               (- (* (* ph 0.7) 0.75) 5))
                             
                             (if (not signal?) 
                               (str other)
                               (str signal-vec))
                             warren-item-type
                             ]]

                    (when signal?
                      [re-com/box
                       :height (px (- (* (* ph 0.7) 0.75) 50)) :size "none"
                       :padding "6px"
                       :style {;:border "1px solid pink" 
                               :overflow "auto"}
                       :child (if @db/bad-form-signals?
                                [re-com/box
                                 :size "auto"
                                 :style {:background-color "rgba(255, 0, 0, 0.3)"
                                         :border "1px solid red"
                                         :padding "6px"
                                         :font-size "20px"}
                                 :child (str @db/bad-form-msg-signals)]
                                [rc/catch (visualize-clause signal-vec 0 nil signal-vec)]
                              ;[re-com/box :child "yo"]
                                )])
                    ]])
                
                ;; [items-list (if selected-warren-item
                ;;               (* ph 0.3)
                ;;               ph) signals selected-warren-item]

                [re-com/box
                 :padding "6px"
                 :align :center :justify :center
                 ;:style {:border "1px solid lime"}
                 :height (px (- (* ph 0.3) 48))
                 :child (cond (and signal? (not (nil? selected-warren-item)))

                          [re-com/v-box
                           :size "auto"
                           :children (for [[e vv] ;(sort-by #(count (pr-str (last %))) signals-history)
                                                    signals-history
                                                    ;:let (for [[ee vvv] signals-history] [(count (str))])
                                           ]
                                       ^{:key (str selected-warren-item e vv "h-box")}
                                       [re-com/h-box
                                        :style (when (= e @hover-tools-atom)
                                                 {:filter "invert(233%)"})
                                        :attr {:on-mouse-enter #(history-mouse-enter e)
                                               :on-mouse-over #(history-mouse-over e)
                                               :on-mouse-leave history-mouse-leave}
                                        :size "auto"
                                        :width "600px"
                                        :children (for [[ee tt] vv
                                                        :let [sec (-> tt js/Date. .toISOString (subs 17 19))]]
                                                    ^{:key (str selected-warren-item "child-boxes" e ee tt)}
                                                    [re-com/box
                                                     :size "none"
                                                     :width "44px"
                                                     :align :center :justify :center
                                                     :style (merge {:border (str "1px solid " (str (theme-pull :theme/editor-outer-rim-color nil) 45))
                                                                    :font-family  (theme-pull :theme/monospaced-font nil)
                                                                    :color "#000000"
                                                                    :font-size "10px"}
                                                                   (when (true? ee)
                                                                     {:background-color (str (theme-pull :theme/editor-outer-rim-color nil) 65)}))
                                                     :child (str sec)])])]
                          
                          (not (nil? selected-warren-item)) [re-com/box :child (str warren-item-type)]

                          :else "(no signal / rule / solver selected)"
                          )]
                
                ]]))

(re-frame/reg-event-db
 ::timeout-response
 (fn [db [_ result what-req]]
   (let [client-name (get db :client-name)]
     (tap> [:websocket-timeout! client-name result what-req])
     db)))

(re-frame/reg-event-db
 ::signals-map-response
 (fn [db [_ result]]
   (let []
     (tap> [:signals-map-in result])
     (assoc db :signals-map result))))

(re-frame/reg-event-db
 ::rules-map-response
 (fn [db [_ result]]
   (let []
     (tap> [:rules-map-in result])
     (assoc db :rules-map result))))

(re-frame/reg-event-db
 ::solvers-map-response
 (fn [db [_ result]]
   (let []
     (tap> [:solvers-map-in result])
     (assoc db :solvers-map result))))

(re-frame/reg-event-db
 ::signals-history-response
 (fn [db [_ result]]
   (if (not (= result :no-updates))
     (let []
       ;;(tap> [:signals-history-in result])
       (assoc db :signals-history result))
     (let []
       ;;(tap> [:nothing-new-in-signal-history :skipping])
       db))))

(re-frame/reg-sub
 ::signals-history
 (fn [db _]
   (get db :signals-history)))

(defn panel-options [get-map-evt-vec selected-item item-type]
  (let [item-type-str (cstr/replace (str item-type) ":" "" )
        server-fn (keyword (str "save-" item-type-str "s-map"))
        data-map (cond (= item-type :solver)  @(ut/tracked-sub ::solvers-map {})
                       (= item-type :rule)    @(ut/tracked-sub ::rules-map {})
                       :else @(ut/tracked-sub ::signals-map {}))]
    [re-com/h-box
     :padding "6px"
     :justify :between :align :center
     :children [[re-com/h-box
                 :align :center :justify :center
                 :gap "8px"
                 :style {:opacity 0.15}
                 :children [[re-com/h-box
                             :attr {:on-click #(ut/tracked-dispatch get-map-evt-vec)}
                             :style {:cursor "pointer"}
                             :gap "4px"
                             :children [[re-com/md-icon-button :src (at)
                                         :md-icon-name "zmdi-download"
                                         :style {:font-size "17px"}]
                                        [re-com/box :child (str "pull " item-type-str "s from server?")]]]]]

                ;; [re-com/box
                ;;  :attr {:on-click #(ut/tracked-dispatch
                ;;                     [::wfx/request :default
                ;;                      {:message    {:kind :signals-history
                ;;                                    :signal-name selected-item
                ;;                                    :client-name @(ut/tracked-sub ::bricks/client-name {})}
                ;;                       :on-response [::signals-history-response]
                ;;                       :timeout    15000000}])}
                ;;  :style {:cursor "pointer" :opacity 0.15}
                ;;  :child "get history (auto)"]

                [re-com/h-box
                 :align :center :justify :center
                 :attr {:on-click #(ut/tracked-dispatch
                                    [::wfx/request :default
                                     {:message    {:kind server-fn ;;:save-signals-map
                                                   :data-map data-map
                                                   :client-name @(ut/tracked-sub ::bricks/client-name {})}
                                                          ;:on-response [::signals-map-response]
                                                          ;:on-timeout  [::timeout-response :get-signals]
                                      :timeout    15000000}])}
                 :style {:cursor "pointer" :opacity 0.15}
                 :gap "4px"
                 :children [[re-com/box :child (str "push this " item-type-str "s set to server?")]
                            [re-com/md-icon-button :src (at)
                             :md-icon-name "zmdi-upload"
                             :style {:font-size "17px"}]]]]])
  )

(defn signals-panel []
  (let [[_ hpct] db/flow-panel-pcts ;; [0.85 0.50]
        hh @(ut/tracked-subscribe [::subs/h]) ;; we want the reaction 
        panel-height (* hh hpct)
        details-panel-height (/ panel-height 1.25) ;; batshit math from flow.cljs panel parent
        ppanel-height (+ panel-height details-panel-height)

        selected-warren-item @(ut/tracked-sub ::selected-warren-item {})
        warren-item-type @(ut/tracked-sub ::warren-item-type {})
        ;; signals-map @(ut/tracked-sub ::signals-map {})

        sver (fn [x] (cstr/replace (str x) ":" ""))
         item-type-str (sver warren-item-type)
         item-type-key (keyword (str item-type-str "s-map"))

        get-map-evt-vec [::wfx/request :default
                         {:message    {:kind item-type-key ;;:signals-map
                                       :client-name @(ut/tracked-sub ::bricks/client-name {})}
                          :on-response (cond
                                         (= item-type-key :rules-map) [::rules-map-response]
                                         (= item-type-key :solvers-map) [::solvers-map-response]
                                         :else [::signals-map-response])
                          :on-timeout  [::timeout-response [:get item-type-key]]
                          :timeout    15000000}]
        ph (- ppanel-height 124)]

    ;; (when (nil? signals-map)
    ;;   (ut/tracked-dispatch get-map-evt-vec)) ;; if fresh, pull it 

    [re-com/v-box
     :padding "6px"
     :width (px (- (last @db/flow-editor-system-mode) 14))
     :size "none"
     :height (px ph)
     :style {;;:border "1px solid cyan"
             :border-radius "8px"
             :background-color (str (theme-pull :theme/editor-rim-color nil) "18")}
     :children [(if (= warren-item-type :unknown)
                  [re-com/gap :size "36px"]
                  [panel-options get-map-evt-vec selected-warren-item warren-item-type])
                [re-com/gap :size "5px"]
                [re-com/h-box :children [[left-col ph]
                                         [right-col ph]]]]]))