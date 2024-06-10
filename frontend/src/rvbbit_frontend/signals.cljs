(ns rvbbit-frontend.signals
  (:require
    ["codemirror/addon/edit/closebrackets.js"]
    ["codemirror/addon/edit/matchbrackets.js"]
    ["codemirror/mode/clojure/clojure.js"]
    ["codemirror/mode/julia/julia.js"]
    ["codemirror/mode/python/python.js"]
    ["codemirror/mode/r/r.js"]
    ["codemirror/mode/sql/sql.js"]
    ["react-codemirror2"     :as cm]
    ["react-drag-and-drop"   :as rdnd]
    [cljs.core.async         :as    async
                             :refer [<! >! chan]]
    [cljs.tools.reader       :refer [read-string]]
    [clojure.edn             :as edn]
    [clojure.set             :as cset]
    [clojure.string          :as cstr]
    [clojure.walk            :as walk]
    [day8.re-frame.undo      :as    undo
                             :refer [undoable]]
    [re-catch.core           :as rc]
    [re-com.core             :as    re-com
                             :refer [at]]
    [re-com.util             :refer [px]]
    [re-frame.alpha          :as rfa]
    [re-frame.core           :as re-frame]
    [reagent.core            :as reagent]
    [rvbbit-frontend.bricks  :as    bricks
                             :refer [theme-pull]]
    [rvbbit-frontend.connections :as conn]
    [rvbbit-frontend.connections :refer [sql-data]]
    [rvbbit-frontend.db      :as db]
    [rvbbit-frontend.http    :as http]
    [rvbbit-frontend.subs    :as subs]
    [rvbbit-frontend.utility :as ut]
    [websocket-fx.core       :as wfx])
  (:import
    [goog.events EventType]
    [goog.async  Debouncer]))

(re-frame/reg-event-db
  ::edit-signal-drop
  (undoable)
  (fn [db [_ content adding]]
    (ut/tapp>> [:edit? content adding])
    (let [curr (get-in db [:signals-map (get db :selected-warren-item) :signal])]
      (assoc-in db
        [:signals-map (get db :selected-warren-item) :signal]
        (ut/postwalk-replacer {content (vec (conj content (if (vector? adding) adding [adding])))}
                              curr)))))

(re-frame/reg-event-db ::edit-signal-drop-kill
                       (undoable)
                       (fn [db [_ content]]
                         (let [curr (get-in db
                                            [:signals-map (get db :selected-warren-item) :signal])]
                           (ut/tapp>> [:kill-nested? content :from curr])
                           (assoc-in db
                             [:signals-map (get db :selected-warren-item) :signal]
                             (ut/remove-subvec curr content)))))

(re-frame/reg-event-db ::edit-signal-drop2
                       (undoable)
                       (fn [db [_ content adding]]
                         (ut/tapp>> [:edit2? content adding])
                         (let [curr (get-in db
                                            [:signals-map (get db :selected-warren-item) :signal])]
                           (assoc-in db
                             [:signals-map (get db :selected-warren-item) :signal]
                             (ut/postwalk-replacer {content (vec (conj content adding))} curr)))))

(defn draggable-item
  [element type data]
  [(reagent/adapt-react-class rdnd/Draggable) (let [] {:type type :data (pr-str data)})
   [re-com/box :style {:cursor "grab"} :child element]])

(defn droppable-area
  [element types-vec content operator]
  [(reagent/adapt-react-class rdnd/Droppable)
   {:types   types-vec
    :on-drop #(let [incoming (js->clj %)
                    incoming (into [] (for [[k v] incoming] {k (edn/read-string v)}))
                    ii       (last (last (first incoming)))]
                (ut/tracked-dispatch [::edit-signal-drop (vec (cons operator content)) ii])
                (ut/tapp>> [:drop-signal-item ii incoming content]))} element])

(defn droppable-area2
  [element types-vec content]
  [(reagent/adapt-react-class rdnd/Droppable)
   {:types   types-vec
    :on-drop #(let [incoming (js->clj %)
                    incoming (into [] (for [[k v] incoming] {k (edn/read-string v)}))
                    ii       (last (last (first incoming)))]
                (ut/tracked-dispatch [::edit-signal-drop2 content ii])
                (ut/tapp>> [:drop-signal-item2 ii incoming content]))} element])

(defn kill-nest
  [op]
  [re-com/box :child
   [re-com/md-icon-button :src (at) :md-icon-name "zmdi-close" :style
    {:font-size "14px" :opacity 0.3 :padding-right "5px"}] :style {:cursor "pointer"} :attr
   {:on-click #(do (ut/tapp>> [:kill-nested op])
                   (ut/tracked-dispatch [::edit-signal-drop-kill op]))}])

(def hover-tools-atom (reagent/atom nil))

(defn is-true?
  [coll full-coll]
  (let [idx                  (.indexOf (vec (ut/where-dissect full-coll)) coll)
        selected-warren-item @(ut/tracked-sub ::selected-warren-item {})
        sigkw                (keyword
                               (str "signal/part-" (ut/replacer (str selected-warren-item) ":" "")
                                    "-"            idx))
        vv                   @(ut/tracked-sub ::conn/clicked-parameter-key-alpha
                                              {:keypath [sigkw]})]
    vv))

(declare highlight-code)
(declare unhighlight-code)

(defn hover-tools
  [coll full-coll]
  (let [hovered?             (= @hover-tools-atom coll)
        idx                  (.indexOf (vec (ut/where-dissect full-coll)) coll)
        selected-warren-item @(ut/tracked-sub ::selected-warren-item {})
        sigkw                (keyword
                               (str "signal/part-" (ut/replacer (str selected-warren-item) ":" "")
                                    "-"            idx))
        vv                   @(ut/tracked-sub ::conn/clicked-parameter-key-alpha {:keypath [sigkw]})
        vv                   (if (nil? vv) "err!" vv)]
    [re-com/v-box :attr
     {:on-mouse-enter #(reset! hover-tools-atom coll)
      :on-mouse-over  #(do (when true ;(not= @hover-tools-atom coll)
                             (highlight-code (str coll))
                             (reset! hover-tools-atom coll)))
      :on-mouse-leave #(do (unhighlight-code) (reset! hover-tools-atom nil))} :align :center
     :justify :center :width "40px" :children
     [[re-com/box :style
       (merge {;:border "1px solid yellow"
               :font-size "12px"}
              (when (not (true? vv)) {:opacity 0.2})) :child (str vv)] [kill-nest coll]]]))


(defn visualize-clause
  [clause level & [contents full-clause]]
  (let [is-operator?
          (fn [c] (and (vector? c) (keyword? (first c)) (contains? #{:and :or :not} (first c))))
        render-operator
          (fn [operator contents lvl]
            (let [body     (vec (cons operator contents))
                  hovered? (= @hover-tools-atom body)]
              [re-com/h-box :style
               (merge {;:transition "all 0.2s" :border "1px solid transparent"
                      }
                      (when hovered?
                        {;:border "1px solid red"
                         :background-color (str (theme-pull :theme/editor-outer-rim-color nil) 44)
                         :filter           "invert(133%)"})
                      (when (is-true? body full-clause)
                        {;:background-color (theme-pull :theme/editor-outer-rim-color nil)
                        })) :align :center :justify :between :children
               [[re-com/v-box :size "auto" :width (px (- 540 (* lvl 35))) :children
                 (cons [droppable-area [re-com/box :child (str operator)] [:operator] contents
                        operator]
                       (map #(visualize-clause % (inc lvl) contents full-clause) contents)) :gap
                 "8px" :style
                 (merge {:border           (str "3px solid "
                                                (theme-pull :theme/editor-outer-rim-color nil)
                                                45)
                         :padding          "5px"
                         :border-radius    "8px"
                         :background-color (str (theme-pull :theme/editor-outer-rim-color nil) "08")
                         :font-weight      700
                         :margin-left      (if (is-true? body full-clause)
                                             (str (* lvl 11) "px")
                                             (str (* lvl 10) "px"))}
                        (when (is-true? body full-clause)
                          {:border           (str "3px solid "
                                                  (theme-pull :theme/editor-outer-rim-color nil)
                                                  99)
                           :background-color (str (theme-pull :theme/editor-outer-rim-color nil)
                                                  44)}))] [hover-tools body full-clause]]]))
        render-condition
          (fn [condition level contents]
            (let [bbox [re-com/h-box :children [[re-com/box :child (str condition) :size "auto"]]
                        :style
                        {;:border "1px solid blue"
                         :border        (str "3px solid "
                                             (theme-pull :theme/editor-outer-rim-color nil)
                                             45)
                         :border-radius "8px"
                         :max-width     230
                         :padding       "5px"}]
                  bbox (if (= level 0)
                         [re-com/box ;; TODO if is only one, looks weird
                          :child bbox]
                         bbox)]
              [droppable-area2 bbox [:operator] contents]))]
    (if (vector? clause)
      (if (is-operator? clause)
        (render-operator (first clause) (rest clause) level)
        (let [;words (ut/deep-flatten clause)
              hovered? (= @hover-tools-atom clause)]
          [re-com/h-box :style
           (merge {;:transition "all 0.2s" :border-radius "8px"
                   :border "3px solid transparent"}
                  (when hovered?
                    {;;:border "1px solid red"
                     :background-color (theme-pull :theme/editor-outer-rim-color nil)
                     :filter           "invert(133%)"})
                  (when (is-true? clause full-clause)
                    {;:transform "scale(1.03)"
                     :border-radius    "8px"
                     :border           (str "3px solid "
                                            (theme-pull :theme/editor-outer-rim-color nil)
                                            99)
                     :background-color (str (theme-pull :theme/editor-outer-rim-color nil) 44)})
                  {;:font-size "17px" :overflow "hidden" :max-width 200
                   :font-weight 300
                   :margin-left (if (is-true? clause full-clause)
                                  (str (* level 11) "px")
                                  (str (* level 10) "px"))}) :justify :between :align :center ;:padding
                                                                                              ;"6px"
           :children
           [[re-com/h-box :children (map #(visualize-clause % level clause full-clause) clause) :gap
             "10px" :style
             {;:border "1px solid #ccc"
              :border-radius    "8px"
              :border           (str "3px solid " (theme-pull :theme/editor-outer-rim-color nil) 25)
              :background-color (str (theme-pull :theme/editor-outer-rim-color nil) "08")
              :padding          "5px"}] [hover-tools clause full-clause]]])) ; Keep pairings on the
                                                                             ; same row
      (render-condition clause level contents))))

(def cm-instance (atom nil))
(def markers (atom []))

(declare highlight-code)

(defn code-box
  [width-int height-int value]
  (doall
    (let [warren-type-string   @(ut/tracked-sub ::warren-item-type {})
          selected-warren-item @(ut/tracked-sub ::selected-warren-item {})
          ttype                (keyword (ut/replacer warren-type-string ":" ""))
          signal?              (= ttype :signal)]
      [re-com/box :size "none" :height (px (- height-int 24)) :style
       {:font-family      (theme-pull :theme/monospaced-font nil) ; "Chivo Mono" ;"Fira Code"
        :font-size        "15px"
        :overflow         "auto"
        :border-radius    "12px"
        :background-color "#000000"
        :font-weight      700} :child
       [(reagent/adapt-react-class cm/UnControlled)
        {:value          (ut/format-map 550 ;; (- width-int 95)
                                        (str value))
         :onBeforeChange (fn [editor data value] (reset! cm-instance editor))
         :onBlur         (fn [e]
                           (let [parse                (try (read-string
                                                             (cstr/join " " (ut/cm-deep-values e)))
                                                           (catch :default e
                                                             [:cannot-parse (str (.-message e))]))
                                 warren-type-string   @(ut/tracked-sub ::warren-item-type {}) ;; they
                                 selected-warren-item @(ut/tracked-sub ::selected-warren-item {})
                                 ttype                (keyword
                                                        (ut/replacer warren-type-string ":" ""))
                                 signal?              (= ttype :signal)
                                 unparseable?         (= (first parse) :cannot-parse)]
                             (cond
                               unparseable?   (do (reset! db/bad-form-signals? true)
                                                  (reset! db/bad-form-msg-signals (str (last
                                                                                         parse))))
                               (empty? parse) (do (reset! db/bad-form-signals? true)
                                                  (reset! db/bad-form-msg-signals "Empty signal"))
                               :else          (do (reset! db/bad-form-signals? false)
                                                  (ut/tracked-dispatch [::edit-basic parse ttype
                                                                        selected-warren-item])))))
         :options        {:mode              "clojure"
                          :lineNumbers       false ;true
                          :matchBrackets     true
                          :autoCloseBrackets true
                          :autofocus         false
                          :autoScroll        false
                          :detach            true
                          :readOnly          false
                          :theme             (theme-pull :theme/codemirror-theme nil)}}]])))

(defn highlight-code
  [code]
  (when-let [editor @cm-instance]
    (let [doc  (.getDoc editor)
          code (ut/format-map 640 ;; 539 ;;(- 634.4 95)
                              (str code))
          _ (ut/tapp>> [:highlighted-code code])]
      (doseq [marker @markers] (.clear marker))
      (reset! markers [])
      (let
        [code-lines (clojure.string/split-lines code)
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
         end-ch (+ (clojure.string/index-of (.getLine doc end-line) (last code-lines))
                   (count (last code-lines)))
         marker
           (try
             (.markText
               doc
               #js {:line start-line :ch start-ch}
               #js {:line end-line :ch end-ch}
               #js
                {:css
                   (str
                     "filter: invert(233%); color: white; font-weight: 700; background-color: teal; font-size: 15px;")})
             (catch :default e (ut/tapp>> [:marker-error (str e)])))]
        (swap! markers conj marker)))))

(defn unhighlight-code
  []
  (when-let [editor @cm-instance]
    (let [doc (.getDoc editor)]
      (doseq [marker @markers] (when marker (.clear marker)))
      (reset! markers []))))

(re-frame/reg-sub ::signals-map (fn [db _] (get db :signals-map {})))

(re-frame/reg-sub ::solvers-map (fn [db _] (get db :solvers-map {})))

(re-frame/reg-sub ::rules-map (fn [db _] (get db :rules-map {})))

(re-frame/reg-event-db ::select-signal
                       (fn [db [_ signal-name]] (assoc db :selected-warren-item signal-name)))

(re-frame/reg-sub
  ::operators
  (fn [db _]
    (get-in db [:signal-items :operators] [:= :<> :> :< :>= :<= :and :or :not :like :changed?])))

(re-frame/reg-sub ::conditions (fn [db _] (get-in db [:signal-items :conditions] [:and :or :not])))

(re-frame/reg-sub ::time-items
                  (fn [db _]
                    (get-in db
                            [:signal-items :time-items]
                            [[:= :day 1] [:= :hour 13] [:= :month 1] [:= :year 2021]])))

(re-frame/reg-sub ::selected-warren-item (fn [db _] (get db :selected-warren-item)))

(re-frame/reg-event-db ::edit-signal
                       (undoable)
                       (fn [db [_ signal name]] (assoc-in db [:signals-map name :signal] signal)))

(re-frame/reg-event-db
  ::edit-basic
  (undoable)
  (fn [db [_ parsed ttype name]]
    (ut/tapp>> [:edit-basic-called-with parsed ttype name])
    (let [sver          (fn [x] (ut/replacer (str x) ":" ""))
          item-type-str (sver ttype)
          item-type-key (keyword (str item-type-str "s-map"))
          kp            (if (= ttype :signal) [item-type-key name :signal] [item-type-key name])]
      (assoc-in db kp parsed))))

(re-frame/reg-event-db ::delete-signal
                       (undoable)
                       (fn [db [_ signal-name item-type]]
                         (let [sver          (fn [x] (ut/replacer (str x) ":" ""))
                               item-type-str (sver item-type)
                               item-type-key (keyword (str item-type-str "s-map"))] ;; :signals-map
                                                                                    ;; , etc
                           (-> db
                               (ut/dissoc-in [item-type-key signal-name])
                               (assoc :selected-warren-item nil)))))

(re-frame/reg-event-db
  ::add-signal
  (undoable)
  (fn [db [_ item-type]]
    (let [item-type     (keyword (ut/drop-last-char item-type))
          rr            (keyword (ut/gen-signal-name item-type))
          existing-keys (vec (into (into (keys (get db :signals-map {}))
                                         (keys (get db :solvers-map {})))
                                   (keys (get db :rules-map {}))))
          signal-name   (ut/safe-key rr existing-keys)
          sver          (fn [x] (ut/replacer (str x) ":" ""))
          item-type-str (sver item-type)
          item-type-key (keyword (str item-type-str "s-map"))
          starting-data (cond (= item-type :signal) [:and [:= :day 1] [:= :hour 9]]
                              (= item-type :solver) {:signal   false
                                                     :persist? false
                                                     :type     :clojure
                                                     :default  0
                                                     :data     (+ 1 2 3)}
                              :else                 {:flow-id nil :signal nil :overrides {}})
          kp            (if (= item-type :signal)
                          [:signals-map signal-name :signal]
                          [item-type-key signal-name])]
      (ut/tapp>> [:add-item? item-type signal-name kp rr])
      (-> db
          (assoc-in kp starting-data)
          (assoc :selected-warren-item signal-name)))))

(defn code-box-smol
  [width-int height-int value & [ww]]
  [re-com/box :size "auto" :style
   {:font-family   (theme-pull :theme/monospaced-font nil) ; "Chivo Mono" ;"Fira Code"
    :font-size     "11px"
    :overflow      "auto"
    :border-radius "12px"
    :font-weight   700} :child
   [(reagent/adapt-react-class cm/UnControlled)
    {:value   (try (ut/format-map (or ww 350) ;; (- width-int 95)
                                  (str value))
                   (catch :default _ value))
     :options {:mode              "clojure"
               :lineWrapping      true
               :lineNumbers       false ; true
               :matchBrackets     true
               :autoCloseBrackets true
               :autofocus         false
               :autoScroll        false
               :detach            true
               :readOnly          true ;true
               :theme             (theme-pull :theme/codemirror-theme nil)}}]]) ;"ayu-mirage" ;"hopscotch"


(defn items-list
  [ph signals selected-warren-item warren-type-string]
  (let [;signals @(ut/tracked-subscribe [::signals-map])
        solver?          (= warren-type-string "solvers")
        signal?          (= warren-type-string "signals")
        warren-item-type @(ut/tracked-sub ::warren-item-type {})
        ssr              (ut/replacer (str @(ut/tracked-sub ::warren-item-type {})) ":" "")
        selected-type    (str ssr "s")
        results          (cond signal? (into {}
                                             (for [[name _] signals]
                                               (let [sigkw (keyword
                                                             (str "signal/"
                                                                  (ut/replacer (str name) ":" "")))
                                                     vv    (if (= warren-type-string "signals")
                                                             @(ut/tracked-sub
                                                                ::conn/clicked-parameter-key-alpha
                                                                {:keypath [sigkw]})
                                                             "")]
                                                 {name vv})))
                               solver? (into {}
                                             (for [[name _] signals]
                                               (let [sigkw (keyword
                                                             (str "solver-meta/"
                                                                  (ut/replacer (str name) ":" "")
                                                                  ">extra"))
                                                     vv    (if (= warren-type-string "solvers")
                                                             @(ut/tracked-sub
                                                                ::conn/clicked-parameter-key-alpha
                                                                {:keypath [sigkw]})
                                                             "")]
                                                 {name vv})))
                               :else   {})]
    [re-com/v-box :children
     [[re-com/h-box :height "30px" :padding "6px" :align :center :justify :between :style
       {:font-size "12px"} :children
       [[re-com/box :style {:cursor "pointer"} :attr
         {:on-click #(ut/tracked-dispatch [::add-signal warren-type-string])} :child
         (str "+ new " warren-type-string)]
        (if (and selected-warren-item (= warren-type-string selected-type))
          [re-com/box :style {:cursor "pointer"} :attr
           {:on-click #(ut/tracked-dispatch [::delete-signal selected-warren-item
                                             warren-item-type])} :child
           (str "- delete this " warren-type-string "?")]
          [re-com/gap :size "5px"])]]
      [re-com/v-box :padding "6px" :style
       {;:border "1px solid red"
        :overflow "auto"} :gap "6px" :size "none" :children
       (for [[name {:keys [signal]}] signals
             :let                    [selected? (= name selected-warren-item)
                                      sigkw     (keyword (str "signal/"
                                                              (ut/replacer (str name) ":" "")))
                                      vv        (when signal? (get results name))
                                      ext-map   (when solver?
                                                  @(ut/tracked-sub
                                                     ::conn/clicked-parameter-key-alpha
                                                     {:keypath [(keyword
                                                                  (str
                                                                    "solver-meta/"
                                                                    (ut/replacer (str name) ":" "")
                                                                    ">extra"))]}))]]
         [draggable-item
          [re-com/v-box :padding "6px" :size "none" :width "100%" :attr
           {:on-double-click #(ut/tracked-dispatch [::select-signal (if selected? nil name)])}
           :style
           {:border           (if selected?
                                (str "3px dashed "
                                     (theme-pull :theme/editor-outer-rim-color nil)
                                     99)
                                (str "1px solid "
                                     (theme-pull :theme/editor-outer-rim-color nil)
                                     "28"))
            :background-color (cond selected? ;;(or selected? (true? vv))
                                      (str (theme-pull :theme/editor-outer-rim-color nil) 65) ;; "rgba(0,
                                    (true? vv) (str (theme-pull :theme/editor-outer-rim-color nil)
                                                    30)                                       ;; "rgba(0,
                                    :else      "rgba(0, 0, 0, 0.1)")
            :cursor           "pointer"} :children
           [[re-com/h-box :style {:font-size "19px"} :justify :between :children
             [[re-com/box :style {:font-size "15px" :font-weight 700} :padding "6px" :size "auto"
               :child (str name)]
              [re-com/box :align :center :justify :center :child
               (if solver? (ut/nf (get ext-map :runs)) (str (if (nil? vv) "" vv))) :style
               {:font-weight 700 :font-size "15px" :opacity (if (true? vv) 1.0 0.2)}]]]
            (when solver?
              [re-com/h-box :padding "6px" :justify :between :style
               {:font-size "13px" :opacity 0.56} :children
               [[re-com/box :child (str (get ext-map :last-processed))]
                [re-com/box :child (str (get ext-map :elapsed-ms) "ms")]]])]] :operator sigkw])]]]))








(defn selector-panel
  [name items icon & [style wide?]]
  (let [open?       (some #(= name %) @db/selectors-open)
        items-count (count items)
        partition?  (integer? wide?)]
    [re-com/v-box :padding "6px" :style
     (merge {:border        (str "2px solid " (theme-pull :theme/editor-outer-rim-color nil) 55)
             :border-radius "4px"}
            style) :children
     [[re-com/h-box :height "28px" :style
       {:cursor "pointer" :user-select "none" :color (theme-pull :theme/editor-outer-rim-color nil)}
       :justify :between :align :center :attr
       {:on-click #(if open?
                     (reset! db/selectors-open (remove (fn [x] (= x name)) @db/selectors-open))
                     (swap! db/selectors-open conj name))} :children
       [[re-com/h-box :style {:font-size "22px"} :gap "8px" :align :center :justify :center
         :children
         [[re-com/box :child name]
          [re-com/box :style {:opacity 0.55 :font-size "15px"} :child (str "(" items-count ")")]]]
        [re-com/md-icon-button :src (at) :md-icon-name icon :style
         {;:color (if open?
          :color     (theme-pull :theme/editor-outer-rim-color nil)
          :font-size "14px"}]]]
      (when open?
        (if (or (= name "solvers") (= name "rules") (= name "signals"))
          (let [;signals @(ut/tracked-subscribe [::signals-map]) ;; they get passed from the
                selected-warren-item @(ut/tracked-sub ::selected-warren-item {})
                ph                   200]
            [items-list (if selected-warren-item (* ph 0.3) ph) items selected-warren-item name])
          (if partition?
            [re-com/v-box :style {:margin-top "6px"} :children
             (for [seg (partition-all wide? items)]
               [re-com/h-box ;;:size "auto"
                :align :center :justify :center :children
                (for [item seg
                      :let [pw    (* (- (last @db/flow-editor-system-mode) 70) 0.35)
                            width (- (/ pw wide?) 10)]]
                  [draggable-item
                   [re-com/box :width (px width) :padding "6px" :style
                    {:border           "1px solid purple"
                     :background-color "#DA70D624"
                     :font-family      (theme-pull :theme/monospaced-font nil)} :child (str item)]
                   :operator item])])]
            [(if wide? re-com/h-box re-com/v-box) :children
             (for [item items]
               [draggable-item
                [re-com/box :padding "6px" :style
                 {:border "1px solid purple" :font-family (theme-pull :theme/monospaced-font nil)}
                 :child (str item)] :operator item])])))]]))

(re-frame/reg-sub ::signal-flow-id
                  (fn [db _] (get-in db [:signals-map (get db :selected-warren-item) :flow-id])))

(re-frame/reg-sub ::signal-open-inputs
                  (fn [db _]
                    (get-in db [:signals-map (get db :selected-warren-item) :open-inputs])))

(re-frame/reg-event-db
  ::edit-signal-flow-id
  (undoable)
  (fn [db [_ flow-id]]
    (assoc-in db [:signals-map (get db :selected-warren-item) :flow-id] flow-id)))

(re-frame/reg-event-db ::open-blocks
                       (fn [db [_ result]]
                         (-> db
                             (assoc-in [:signals-map (get db :selected-warren-item) :open-inputs]
                                       (get result :open-inputs))
                             (assoc-in [:signals-map (get db :selected-warren-item) :blocks]
                                       (get result :blocks)))))


(re-frame/reg-event-fx ::refresh-open-blocks
                       (fn [{:keys [db]} [_ flow-id]]
                         {:db       db
                          :dispatch [::wfx/request :default
                                     {:message     {:kind        :get-flow-open-ports
                                                    :flow-id     flow-id
                                                    :flowmap     flow-id
                                                    :client-name (get db :client-name)}
                                      :on-response [::open-blocks]
                                      :timeout     500000}]}))

(defonce mode-atom (reagent/atom "when logic"))

(defn flow-box
  [hh]
  (let [server-flows (map :flow_id @(ut/tracked-subscribe [::conn/sql-data [:flows-sys]]))
        sql-calls    {:flows-sys {:select        [:flow_id :file_path :last_modified]
                                  :from          [:flows]
                                  :connection-id "flows-db"
                                  :order-by      [[3 :desc]]}}
        open-inputs  @(ut/tracked-subscribe [::signal-open-inputs])
        react!       [@mode-atom]
        ww           (* (- (last @db/flow-editor-system-mode) 14) 0.318)
        fid          @(ut/tracked-subscribe [::signal-flow-id])]
    (dorun (for [[k query] sql-calls]
             (let [data-exists? @(ut/tracked-subscribe [::conn/sql-data-exists? [k]])
                   unrun-sql?   @(ut/tracked-subscribe [::conn/sql-query-not-run? [k] query])]
               (when (or (not data-exists?) unrun-sql?)
                 (if (get query :connection-id)
                   (conn/sql-data [k] query (get query :connection-id))
                   (conn/sql-data [k] query))))))
    [re-com/v-box :padding "6px" :children
     [[re-com/box :padding "6px" :child "flow to run when signals are met:"]
      (if fid
        [re-com/h-box :align :center :justify :between :padding "6px" :width "320px" :height "35px"
         :style
         {;:border "1px solid #ffffff33"
          :font-family (theme-pull :theme/monospaced-font nil)} :children
         [[re-com/box :style
           {:overflow "hidden" :font-size "17px" :white-space "nowrap" :max-width "292px"} :child
           (str fid)]
          [re-com/box :style {:cursor "pointer"} :attr
           {:on-click #(ut/tracked-dispatch [::edit-signal-flow-id nil])} :child "x"]]]
        [re-com/typeahead :suggestion-to-string (fn [item] (str (get item :label)))
         :render-suggestion
         (fn [ss _] ;; render in dropdown
           [re-com/box :style {:color "#000000"} :child (str (get ss :label))]) :on-change
         #(do (ut/tracked-dispatch [::refresh-open-blocks (get % :id)])
              (ut/tracked-dispatch [::edit-signal-flow-id (get % :id)])) :rigid? true :style
         {:font-family (theme-pull :theme/monospaced-font nil)} :width "320px" :change-on-blur? true
         :placeholder (or fid "search for a server flow to run...") :data-source
         (fn [x]
           (let [flow-parts   (vec (map (fn [n] {:id n :label n}) server-flows))
                 words        (ut/splitter (cstr/lower-case (cstr/trim x)) #" ")
                 matches-word (fn [field word] (cstr/includes? (cstr/lower-case (str field)) word))]
             (if (or (nil? x) (empty? x))
               flow-parts
               (filter (fn [item]
                         (let [label (get item :label)]
                           (every? (fn [word] (matches-word label word)) words)))
                 flow-parts))))]) [re-com/gap :size "5px"]
      [re-com/h-box :justify :between :align :center :children
       (for [btn ["input values" "when logic"]]
         [re-com/v-box :height "50px" :align :center :justify :center :size "auto" :style
          (merge {;:border "1px solid pink"
                  :cursor "pointer"}
                 (if (= btn @mode-atom)
                   {:background-color "rgba(0, 0, 255, 0.3)"}
                   {:background-color "rgba(0, 0, 0, 0.1)"})) :attr
          {:on-click #(reset! mode-atom btn)} :children
          [[re-com/box :child "configure"] [re-com/box :child (str btn)]]])]] :height (px hh)]))

(defonce searcher-atom (reagent/atom nil))

(defn left-col
  [ph]
  (let [operators            @(ut/tracked-sub ::operators {})
        conditions           @(ut/tracked-sub ::conditions {})
        time-items           @(ut/tracked-sub ::time-items {})
        signals              @(ut/tracked-sub ::signals-map {})
        solvers              @(ut/tracked-sub ::solvers-map {})
        rules                @(ut/tracked-sub ::rules-map {})
        selected-warren-item @(ut/tracked-sub ::selected-warren-item {})
        signal-vec           (get-in signals [selected-warren-item :signal])
        react!               [@db/selectors-open]
        filter-results       (fn [x y]
                               (if (or (nil? x) (empty? x))
                                 y
                                 (if (map? y)
                                   (into {}
                                         (filter (fn [[k v]]
                                                   (or (cstr/includes? (cstr/lower-case (str k))
                                                                       (cstr/lower-case x))
                                                       (cstr/includes? (cstr/lower-case (str v))
                                                                       (cstr/lower-case x))))
                                           y))
                                   (vec (filter #(cstr/includes? (cstr/lower-case (str %))
                                                                 (cstr/lower-case x))
                                          y)))))
        flow-box-hh          148]
    [re-com/v-box :gap "6px" :width "35%" :children
     [;(when selected-warren-item
      [re-com/h-box :padding "6px" :height "50px" :align :center :justify :between :children
       [[re-com/input-text :src (at) :model searcher-atom :width "93%" :on-change
         #(reset! searcher-atom (let [vv (str %) ;; (cstr/trim (str %))
                                     ]
                                  (if (empty? vv) nil vv))) :placeholder "(search filter)"
         :change-on-blur? false :style
         {:text-decoration  (when (ut/ne? @searcher-atom) "underline")
          :color            "inherit"
          :outline          "none"
          :text-align       "center"
          :background-color "#00000000"}]
        [re-com/box :style
         {;:border "1px solid maroon"
          :opacity (if @searcher-atom 1.0 0.45)
          :cursor  "pointer"} :width "20px" :align :center :justify :center :attr
         {:on-click #(reset! searcher-atom nil)} :child "x"]]]
      [re-com/v-box :padding "6px" :height
       (px (- ph
              100 ;; 50
           )) :style
       {;:border "1px solid yellow"
        :overflow "auto"} :gap "6px" :children
       [[selector-panel "operators" (filter-results @searcher-atom operators) "zmdi-puzzle-piece" {}
         3]
        [selector-panel "conditions" (filter-results @searcher-atom conditions) "zmdi-puzzle-piece"
         {} 3]
        [selector-panel "time items" (filter-results @searcher-atom time-items) "zmdi-calendar-alt"
         {} 2]
        [selector-panel "parameters" (filter-results @searcher-atom time-items) "zmdi-shape" {} 2]
        [selector-panel "flow values" (filter-results @searcher-atom time-items) "zmdi-shape" {} 2]
        [selector-panel "clients" (filter-results @searcher-atom time-items) "zmdi-desktop-mac" {}
         2]
        [selector-panel "metrics" (filter-results @searcher-atom time-items) "zmdi-equalizer" {} 2]
        [selector-panel "KPIs" (filter-results @searcher-atom time-items) "zmdi-traffic" {} 2]
        [re-com/gap :size "20px"]
        [selector-panel "signals" (filter-results @searcher-atom signals) "zmdi-flash" {} 5]
        [selector-panel "rules" (filter-results @searcher-atom rules) "zmdi-flash" {} 5]
        [selector-panel "solvers" (filter-results @searcher-atom solvers) "zmdi-flash" {} 5]
        [re-com/gap :size "10px"]
        [re-com/h-box :size "auto" :gap "8px" :children
         (for [e ["solver" "rule" "signal"]]
           [re-com/v-box :padding "4px" :style
            {:border        (str "1px solid " (theme-pull :theme/editor-outer-rim-color nil) 55)
             :font-size     "14px"
             :color         (theme-pull :theme/editor-outer-rim-color nil)
             :border-radius "4px"} :size "auto" :height "50px" :align :center :justify :center
            :children [[re-com/box :child (str "+ new " e)] [re-com/box :child (str "group")]]])]
        [re-com/gap :size "20px"]]]]]))


(re-frame/reg-event-db
  ::rename-item
  (undoable)
  (fn [db [_ old new item-type]]
    (let [sver          (fn [x] (ut/replacer (str x) ":" ""))
          item-type-str (sver item-type)
          item-type-key (keyword (str item-type-str "s-map"))
          refs          {(keyword (str item-type-str "/" (sver old)))
                           (keyword (str item-type-str "/" (sver new)))}]
      (ut/tapp>> [:rename-item! old new item-type item-type-str item-type-key refs])
      (-> db
          (assoc :signals-map (ut/postwalk-replacer
                                (if (= item-type-key :signals-map) (merge {old new} refs) refs)
                                (get db :signals-map {}))) ;; change all fqns refs
          (assoc :rules-map (ut/postwalk-replacer
                              (if (= item-type-key :rules-map) (merge {old new} refs) refs)
                              (get db :rules-map {})))
          (assoc :solvers-map (ut/postwalk-replacer
                                (if (= item-type-key :solvers-map) (merge {old new} refs) refs)
                                (get db :solvers-map {})))
          (assoc :selected-warren-item new))))) ;; set as selected item



(defonce title-edit-idx (reagent/atom nil))

(defn edit-item-name
  [item-name item-type w]
  (let [;read-only-flow? (true? (cstr/includes? flow-id "/"))
        flow-id-regex #"^[a-zA-Z0-9_?\-]+$" ;; alpha, underscores, hypens, numbers, question
       ]
    (if (not @title-edit-idx)
      [re-com/box :size "none" :height "45px" :width (px w) :align :center :justify :start :attr
       {:on-double-click #(reset! title-edit-idx (str item-name))} :style
       {:cursor        "pointer"
        :padding-right "12px"
        :padding-top   "1px"
        :border        "2px solid transparent"
        :font-size     "26px"} :child (str item-name)]
      [re-com/input-text :src (at) :model (ut/replacer (str item-name) ":" "") :width (px (- w 6))
       :height "45px" :on-change
       #(do (ut/tracked-dispatch [::rename-item item-name
                                  (try (keyword (str %)) (catch :default _ (str %))) item-type])
            (reset! title-edit-idx nil)) :validation-regex flow-id-regex :change-on-blur? true
       :style
       {:border           (str "2px dashed " (theme-pull :theme/editor-outer-rim-color nil))
        :font-size        "26px"
        :text-decoration  "underline"
        :color            (theme-pull :theme/editor-outer-rim-color nil)
        :font-style       "underline"
        :text-align       "left"
        :background-color "#00000000"}])))

(re-frame/reg-event-db ::run-signals-history
                       (fn [db _]
                         (ut/tracked-dispatch
                           [::wfx/request :default
                            {:message     {:kind        :signals-history
                                           :signal-name (get db :selected-warren-item)
                                           :client-name @(ut/tracked-sub ::bricks/client-name {})}
                             :on-response [::signals-history-response]
                             :timeout     15000000}])
                         db))

(re-frame/reg-sub ::run-signals-history?
                  (fn [db _]
                    (let [signals (vec (keys (get db :signals-map)))
                          sel     (get db :selected-warren-item)]
                      (and (get db :flow?)
                           (not (nil? sel))
                           (some #(= sel %) signals)
                           (= (get @db/flow-editor-system-mode 0) "signals")))))

(re-frame/reg-sub ::warren-item-type
                  (fn [db _]
                    (let [name    (get db :selected-warren-item)
                          signals (keys (get db :signals-map))
                          rules   (keys (get db :rules-map))
                          solvers (keys (get db :solvers-map))]
                      (cond (some #(= name %) signals) :signal
                            (some #(= name %) rules)   :rule
                            (some #(= name %) solvers) :solver
                            :else                      :unknown))))

(defn history-mouse-enter [e] (reset! hover-tools-atom e))

(defn history-mouse-over
  [e]
  (when true ;; todo to short-circuit
    (highlight-code (str e))
    (reset! hover-tools-atom e)))

(defn history-mouse-leave [] (unhighlight-code) (reset! hover-tools-atom nil))


(defn right-col
  [ph]
  (let [signals              @(ut/tracked-sub ::signals-map {})
        selected-warren-item @(ut/tracked-sub ::selected-warren-item {})
        warren-item-type     @(ut/tracked-sub ::warren-item-type {})
        signal?              (true? (= warren-item-type :signal))
        solver?              (true? (= warren-item-type :solver))
        signal-vec           (when signal? (get-in signals [selected-warren-item :signal]))
        signal-vec-parts     (when signal? (vec (ut/where-dissect signal-vec)))
        warren-item-type     @(ut/tracked-sub ::warren-item-type {})
        walk-map             (cond signal? (into {}
                                                 (for [idx (range (count signal-vec-parts))]
                                                   (let [sigkw (keyword
                                                                 (str "part-"
                                                                        (ut/replacer
                                                                          (str selected-warren-item)
                                                                          ":"
                                                                          "")
                                                                      "-" idx))
                                                         name  (get signal-vec-parts idx)]
                                                     {sigkw name})))
                                   :else   {})
        signals-history      (when signal?
                               @(ut/tracked-sub
                                  ::conn/clicked-parameter-key-alpha
                                  {:keypath [(keyword (str "signal-history/"
                                                           (ut/unkeyword selected-warren-item)))]}))
        signals-history      (when signal?
                               (select-keys (walk/postwalk-replace walk-map signals-history)
                                            signal-vec-parts))
        other                (cond (= warren-item-type :solver) (get @(ut/tracked-sub ::solvers-map
                                                                                      {})
                                                                     selected-warren-item)
                                   (= warren-item-type :rule)   (get @(ut/tracked-sub ::rules-map
                                                                                      {})
                                                                     selected-warren-item)
                                   :else                        {})
        wwidth               (* (- (last @db/flow-editor-system-mode) 14) 0.65) ;; pixel calc
       ]
    [re-com/v-box :width "65%" ;; :gap "3px"
     :children
     [(when selected-warren-item
        [re-com/v-box :style
         {;:font-family   (theme-pull :theme/monospaced-font nil)
         } :height (px (* ph 0.7)) :padding "6px" :children
         [(let [ww (* (- (last @db/flow-editor-system-mode) 70) 0.65)]
            [re-com/h-box :justify :between :align :center :children
             [[edit-item-name selected-warren-item warren-item-type (* ww 0.9)]
              (cond signal? [re-com/box :align :center :justify :center :style
                             {;:border "1px solid yellow"
                              :font-size   "22px"
                              :font-weight 700} :height "100%" :width (px (* ww 0.1)) :child
                             (str @(ut/tracked-sub ::conn/clicked-parameter-key-alpha
                                                   {:keypath [(keyword
                                                                (str "signal/"
                                                                     (ut/replacer
                                                                       (str selected-warren-item)
                                                                       ":"
                                                                       "")))]}))]
                    solver? [re-com/md-icon-button :src (at) :md-icon-name "zmdi-play" :on-click
                             #(ut/tracked-dispatch
                                [::wfx/request :default
                                 {:message {:kind         :run-solver
                                            :solver-name  selected-warren-item
                                            :override-map (get @(ut/tracked-sub ::solvers-map {})
                                                               selected-warren-item)
                                            :client-name  @(ut/tracked-sub ::bricks/client-name {})}
                                  :timeout 15000000}]) :style
                             {:font-size "17px"
                              :cursor    "pointer"
                              :color     (theme-pull :theme/editor-outer-rim-color nil)}]
                    :else   [re-com/gap :size "10px"] ;; placeholder
              )]])
          [re-com/box :style
           {:border        (str "2px solid " (theme-pull :theme/editor-outer-rim-color nil) 45)
            :border-radius "4px"} :height
           (if signal? (px (- (* (* ph 0.7) 0.25) 12)) (px (- (* (* ph 0.7) 0.55) 12))) :padding
           "6px" :child
           [code-box nil ;(* (- (last @db/flow-editor-system-mode) 14) 0.65) ;; width
            (if signal? (- (* (* ph 0.7) 0.25) 5) (- (* (* ph 0.7) 0.55) 5))
            (if (not signal?) (str other) (str signal-vec))]]
          (when (or solver? signal?)
            [re-com/box :height
             (px (if signal? (- (* (* ph 0.7) 0.75) 50) (- (* (* ph 0.7) 0.45) 50))) :size "none"
             :padding "6px" :style
             (merge
               {:border        (str "2px solid " (theme-pull :theme/editor-outer-rim-color nil) 45)
                :border-radius "4px"
                :overflow      "auto"}
               (when solver?
                 {:border-top    (str "2px solid "
                                      (theme-pull :theme/editor-outer-rim-color nil)
                                      88)
                  :border-radius "12px"})) :child
             (if @db/bad-form-signals?
               [re-com/box :size "auto" :style
                {:background-color "rgba(255, 0, 0, 0.3)"
                 :border           "1px solid red"
                 :padding          "6px"
                 :font-size        "20px"} :child (str @db/bad-form-msg-signals)]
               (if signal?
                 [bricks/reecatch (visualize-clause signal-vec 0 nil signal-vec)]
                 [re-com/v-box :padding "6px" :children
                  [[re-com/box :style {:font-size "19px" :padding-top "6px" :font-weight 700} :child
                    "latest returned value"]
                   (let [vv          @(ut/tracked-sub ::conn/clicked-parameter-key-alpha
                                                      {:keypath [(keyword
                                                                   (str "solver/"
                                                                        (ut/replacer
                                                                          (str selected-warren-item)
                                                                          ":"
                                                                          "")))]})
                         sql-solver? (try (some #(= % :_cache-query) (keys vv))
                                          (catch :default _ false))]
                     (if sql-solver?
                       [re-com/box :align :center :justify :center :child
                        [bricks/honeycomb-fragments vv 11 5]]
                       [re-com/box :child
                        [bricks/map-boxes2 vv nil :solver-meta [] :output nil false]]))
                   [re-com/gap :size "8px"]
                   [re-com/box :style {:font-size "19px" :padding-top "6px" :font-weight 700} :child
                    "latest full output"]
                   (let [full-output @(ut/tracked-sub
                                        ::conn/clicked-parameter-key-alpha
                                        {:keypath
                                           [(keyword
                                              (str "solver-meta/"
                                                   (ut/replacer (str selected-warren-item) ":" "")
                                                   ">output"))]})]
                     [re-com/box :child ;(str "more shit here... error.. println, etc " (str
                                        ;"solver/"
                      [code-box-smol nil nil (pr-str full-output) (- wwidth 100)]])]]))])]])
      (when signal?
        (let [reaktsz [@db/signal-history-ticker?]]
          [re-com/h-box :gap "10px" :align :center :style {:font-size "13px" :padding-left "21px"}
           :children
           [[re-com/box :child (str (if @db/signal-history-ticker? "stop" "play")) :style
             {:color       (if @db/signal-history-ticker? "red" "green")
              :cursor      "pointer"
              :font-weight 700} :attr
             {:on-click #(reset! db/signal-history-ticker? (not @db/signal-history-ticker?))}]
            [re-com/box :child " live history ticker"]
            [re-com/md-icon-button :src (at) :md-icon-name "zmdi-time-countdown" :style
             {:font-size "17px"}]]]))
      (let [signals-history2 (when signal?
                               (into {}
                                     (for [tt (range (count (get signals-history
                                                                 (first (keys signals-history)))))]
                                       {tt (vec (for [kk (keys signals-history)]
                                                  [kk (get-in signals-history [kk tt])]))})))]
        [re-com/box :padding "6px" :align :center :justify :center :child
         (cond (and signal? (not (nil? selected-warren-item)))
                 [re-com/h-box :width "600px" :children
                  (for [[e vv] signals-history2
                        :let   [main       (reduce (fn [a b] (if (> (count a) (count b)) a b))
                                             (map first vv)) ;; largest vector is likely the main
                                ff         (first (filter #(= main (first %)) vv))
                                main-true? (true? (first (last ff)))]]
                    ^{:key (str selected-warren-item e vv "h-box")}
                    [re-com/v-box :height (px (- (* ph 0.3) 85)) :size "auto" :children
                     (for [[kk [ee tt]] vv
                           :let         [sec (-> tt
                                                 js/Date.
                                                 .toISOString
                                                 (subs 17 19))]]
                       ^{:key (str selected-warren-item "child-boxes" e ee tt)}
                       [re-com/box :attr
                        {:on-mouse-enter #(history-mouse-enter kk)
                         :on-mouse-over  #(history-mouse-over kk)
                         :on-mouse-leave history-mouse-leave} :size "auto" :align :center :justify
                        :center :style
                        (merge
                          (when (= kk @hover-tools-atom) {:filter "invert(233%)"})
                          (when (not main-true?)
                            {:border (str "1px solid "
                                          (str (theme-pull :theme/editor-outer-rim-color nil) 45))})
                          {:font-family (theme-pull :theme/monospaced-font nil)
                           :color       "#000000"
                           :font-size   "10px"}
                          (when main-true? ;;(and main-true? (true? ee))
                            {:border (str "1px solid "
                                          (str (theme-pull :theme/editor-outer-rim-color nil)))})
                          (when (true? ee)
                            {:background-color (str (theme-pull :theme/editor-outer-rim-color nil)
                                                    65)})) :child (str sec)])])]
               (not (nil? selected-warren-item))
                 [re-com/box :size "none" :width "600px" :padding "6px" :height
                  (px (- (* ph 0.3) 60)) :style
                  {;:border "1px solid pink"
                   :border-top    (str "2px solid "
                                       (theme-pull :theme/editor-outer-rim-color nil)
                                       88)
                   :border-radius "12px"
                   :overflow      "auto"} :child
                  [re-com/v-box :width "582px" :children
                   [[re-com/box :style {:font-size "19px" :padding-top "6px" :font-weight 700}
                     :child "latest returned value metadata"]
                    [re-com/box :size "auto" :child
                     [code-box-smol nil nil
                      (pr-str @(ut/tracked-sub
                                 ::conn/clicked-parameter-key-alpha
                                 {:keypath [(keyword
                                              (str "solver-meta/"
                                                   (ut/replacer (str selected-warren-item) ":" "")
                                                   ">extra"))]})) (- wwidth 100)]]
                    [re-com/gap :size "8px"]
                    [re-com/box :style {:font-size "19px" :padding-top "6px" :font-weight 700}
                     :child "recent run history"]
                    [re-com/box :size "auto" :child
                     [code-box-smol nil nil
                      (pr-str @(ut/tracked-sub
                                 ::conn/clicked-parameter-key-alpha
                                 {:keypath [(keyword
                                              (str "solver-meta/"
                                                   (ut/replacer (str selected-warren-item) ":" "")
                                                   ">history"))]})) (- wwidth 100)]]]]]
               :else "(no signal / rule / solver selected)")])]]))

(re-frame/reg-event-db ::timeout-response
                       (fn [db [_ result what-req]]
                         (let [client-name (get db :client-name)]
                           (ut/tapp>> [:websocket-timeout! client-name result what-req])
                           db)))

(re-frame/reg-event-db ::signals-map-response
                       (fn [db [_ result]] (let [] (assoc db :signals-map result))))

(re-frame/reg-event-db ::rules-map-response
                       (fn [db [_ result]] (let [] (assoc db :rules-map result))))

(re-frame/reg-event-db ::solvers-map-response
                       (fn [db [_ result]] (let [] (assoc db :solvers-map result))))

(re-frame/reg-event-db
  ::signals-history-response
  (fn [db [_ result]]
    (if (not (= result :no-updates)) (let [] (assoc db :signals-history result)) (let [] db))))

(re-frame/reg-sub ::signals-history (fn [db _] (get db :signals-history)))

(defn panel-options
  [get-map-evt-vec selected-item item-type]
  (let [item-type-str (ut/replacer (str item-type) ":" "")
        server-fn     (keyword (str "save-" item-type-str "s-map"))
        data-map      (cond (= item-type :solver) @(ut/tracked-sub ::solvers-map {})
                            (= item-type :rule)   @(ut/tracked-sub ::rules-map {})
                            :else                 @(ut/tracked-sub ::signals-map {}))]
    [re-com/h-box :padding "6px" :justify :between :align :center :children
     [[re-com/h-box :align :center :justify :center :gap "8px" :style {:opacity 0.15} :children
       [[re-com/h-box :attr {:on-click #(ut/tracked-dispatch get-map-evt-vec)} :style
         {:cursor "pointer"} :gap "4px" :children
         [[re-com/md-icon-button :src (at) :md-icon-name "zmdi-download" :style {:font-size "17px"}]
          [re-com/box :child (str "pull " item-type-str "s from server?")]]]]]
      [re-com/h-box :align :center :justify :center :attr
       {:on-click #(ut/tracked-dispatch
                     [::wfx/request :default
                      {:message {:kind        server-fn ;;:save-signals-map
                                 :data-map    data-map
                                 :client-name @(ut/tracked-sub ::bricks/client-name {})}
                       :timeout 15000000}])} :style {:cursor "pointer" :opacity 0.15} :gap "4px"
       :children
       [[re-com/box :child (str "push this " item-type-str "s set to server?")]
        [re-com/md-icon-button :src (at) :md-icon-name "zmdi-upload" :style
         {:font-size "17px"}]]]]]))

(defn signals-panel
  []
  (let [[_ hpct]             db/flow-panel-pcts ;; [0.85 0.50]
        hh                   @(ut/tracked-sub ::subs/h {}) ;; we want the reaction
        panel-height         (* hh hpct)
        details-panel-height (/ panel-height 1.25) ;; batshit math from flow.cljs panel parent
        ppanel-height        (+ panel-height details-panel-height)
        selected-warren-item @(ut/tracked-sub ::selected-warren-item {})
        warren-item-type     @(ut/tracked-sub ::warren-item-type {})
        sver                 (fn [x] (ut/replacer (str x) ":" ""))
        item-type-str        (sver warren-item-type)
        item-type-key        (keyword (str item-type-str "s-map"))
        get-map-evt-vec      [::wfx/request :default
                              {:message     {:kind        item-type-key ;;:signals-map
                                             :client-name @(ut/tracked-sub ::bricks/client-name {})}
                               :on-response (cond (= item-type-key :rules-map)
                                                    [::rules-map-response]
                                                  (= item-type-key :solvers-map)
                                                    [::solvers-map-response]
                                                  :else [::signals-map-response])
                               :on-timeout  [::timeout-response [:get item-type-key]]
                               :timeout     15000000}]
        ph                   (- ppanel-height 124)]
    [re-com/v-box :padding "6px" :width (px (- (last @db/flow-editor-system-mode) 14)) :size "none"
     :height (px ph) :style
     {;;:border "1px solid cyan"
      :border-radius    "8px"
      :background-color (let [tc           (theme-pull :theme/editor-rim-color nil)
                              len          (count tc)
                              has-opacity? (= len 9)
                              cc           (if has-opacity? (subs tc 0 7) tc)]
                          (str cc "18"))} :children
     [(if (= warren-item-type :unknown)
        [re-com/gap :size "36px"]
        [panel-options get-map-evt-vec selected-warren-item warren-item-type])
      [re-com/gap :size "5px"] (doall [re-com/h-box :children [[left-col ph] [right-col ph]]])]]))