(ns rvbbit-frontend.signals 
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
 ::edit-signal-drop
 (undoable)
 (fn [db [_ content adding]]
   (tap> [:edit? content adding])
   (let [curr (get-in db [:signals-map (get db :selected-signal) :signal])]
     (assoc-in db [:signals-map (get db :selected-signal) :signal] 
               (walk/postwalk-replace
                {content
                 (vec (conj content (if (vector? adding) adding [adding])))
                                       ;(vec (cons adding content))
                                       ;[adding content]
                 }curr)))))

(re-frame/reg-event-db
 ::edit-signal-drop-kill
 (undoable)
 (fn [db [_ content]]
   (let [curr (get-in db [:signals-map (get db :selected-signal) :signal])]
     (tap> [:kill-nested? content :from curr])
     (assoc-in db [:signals-map (get db :selected-signal) :signal]
               (ut/remove-subvec curr content)))))

(re-frame/reg-event-db
 ::edit-signal-drop2
 (undoable)
 (fn [db [_ content adding]]
   (tap> [:edit2? content adding])
   (let [curr (get-in db [:signals-map (get db :selected-signal) :signal])]
     (assoc-in db [:signals-map (get db :selected-signal) :signal]
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
                (re-frame/dispatch [::edit-signal-drop (vec (cons operator content)) ii])
                (tap> [:drop-signal-item ii incoming content]))}
   element])

(defn droppable-area2 [element types-vec content]
  [(reagent/adapt-react-class rdnd/Droppable)
   {:types   types-vec
    :on-drop #(let [incoming   (js->clj %)
                    incoming (into [] (for [[k v] incoming] {k (edn/read-string v)}))
                    ii (last (last (first incoming)))]
                (re-frame/dispatch [::edit-signal-drop2 content ii])
                (tap> [:drop-signal-item2 ii incoming content]))}
   element])

(defn kill-nest [op]
  [re-com/box :child "x" :padding "6px"
   :style {:cursor "pointer"}
   :attr {:on-click #(do
                       (tap> [:kill-nested op])
                       (re-frame/dispatch [::edit-signal-drop-kill op]))}])

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
                                                 ;:font-size "17px"
                                                 :font-weight 700
                                                 :margin-left (str (* lvl 10) "px")}]

                                        [hover-tools body]]]))
        render-condition (fn [condition level contents]
                           (let [bbox [re-com/h-box
                                       :children [[re-com/box :child (str condition) :size "auto"]]
                                       :style {:border "1px solid blue"
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
                   (when hovered? {:background-color "rgba(0, 0, 255, 0.3)"})
                   {;:font-size "17px"
                    :font-weight 300
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
             :background-color "#000000"
             ;:border "1px solid yellow"
             :font-weight   700}
     :child [(reagent/adapt-react-class cm/UnControlled)
             {:value   (ut/format-map (- width-int 95)
                                      (str value))
              ;;:onBlur  #(re-frame/dispatch [::edit-signal (read-string (cstr/join " " (ut/cm-deep-values %)))])
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

                                (not (vector? parse))
                                (do (reset! db/bad-form-signals? true)
                                    (reset! db/bad-form-msg-signals "Needs to be a vector / honey-sql where clause format"))

                                :else (do (reset! db/bad-form-signals? false)
                                          (re-frame/dispatch [::edit-signal parse]))))
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
 ::signals-map
 (fn [db _]
   (get db :signals-map {})))

(re-frame/reg-event-db
 ::select-signal
 (fn [db [_ signal-name]]
   (assoc db :selected-signal signal-name)))

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
 ::selected-signal
 (fn [db _]
   (get db :selected-signal)))

(re-frame/reg-event-db
 ::edit-signal
 (undoable)
 (fn [db [_ signal]]
   (assoc-in db [:signals-map (get db :selected-signal) :signal] signal)))

(re-frame/reg-event-db
 ::delete-signal
 (undoable)
 (fn [db [_ signal-name]]
   (-> db
       (ut/dissoc-in [:signals-map signal-name])
       (assoc :selected-signal nil))))

(re-frame/reg-event-db
 ::add-signal
 (undoable)
 (fn [db [_]]
   (let [rr (keyword (ut/gen-signal-name))
         signal-name (ut/safe-key rr (vec (keys (get db :signals-map {}))))]
     (tap> [:add-signal? signal-name])
     (-> db
         (assoc-in [:signals-map signal-name :signal] [:and [:= :day 1] [:= :hour 9]])
         (assoc :selected-signal signal-name)))))

(defn signals-list [ph signals selected-signal]
  (let [;signals @(re-frame/subscribe [::signals-map])
        ;selected-signal @(re-frame/subscribe [::selected-signal])
        ]
    [re-com/v-box
     :children
     [[re-com/h-box
       :height "30px" :padding "6px"
       :align :center :justify :between
       :children [[re-com/box
                   :style {:cursor "pointer"}
                   :attr {:on-click #(re-frame/dispatch [::add-signal "new signal"])}
                   :child "+ new signal"]
                  (if selected-signal
                    [re-com/box
                     :style {:cursor "pointer"}
                     :attr {:on-click #(re-frame/dispatch [::delete-signal selected-signal])}
                     :child (str "- delete this signal?")]
                    [re-com/gap :size "5px"])]]
      [re-com/v-box
       :padding "6px"
       :style {:border "1px solid red"
               :overflow "auto"}
       :gap "6px"
       :size "none"
       ;:height (px (- ph 94))
       :children (for [[name {:keys [signal]}] signals
                       :let [selected? (= name selected-signal)]]
                   
                   [draggable-item
                    [re-com/v-box
                     :padding "6px"
                     :size "none"
                     :width "100%"
                     :attr {:on-double-click #(re-frame/dispatch [::select-signal (if selected? nil name)])}
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
                                 ;;     (visualize-clause signal 0)
                                 ;;     (str signal))
                                 (str signal)]]]
                                 :operator (keyword (str "signal/" (cstr/replace (str name) ":" "")))
                                 ])]]]))






(defonce selectors-open (reagent/atom []))

(defn selector-panel [name items icon & [style wide?]]
  (let [open? (some #(= name %) @selectors-open)
        items-count (count items)
        partition? (integer? wide?)]
    [re-com/v-box
     :padding "6px"
     :style (merge {:border "1px solid yellow"} style)
     :children [[re-com/h-box
                 :height "28px"
                 :style {:cursor "pointer"}
                 :justify :between :align :center
                 :attr {:on-click #(if open?
                                     (reset! selectors-open (remove (fn [x] (= x name)) @selectors-open))
                                     (swap! selectors-open conj name))}
                 :children [[re-com/h-box
                             :style {:font-size "19px"}
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
                                     :font-size "14px"}]]]
                (when open?
                  (if (= name "signals")

                    (let [;signals @(re-frame/subscribe [::signals-map]) ;; they get passed from the filter col
                          selected-signal @(re-frame/subscribe [::selected-signal])
                          ;signal-vec (get-in signals [selected-signal :signal])
                          ph 200]
                      [signals-list (if selected-signal
                                    (* ph 0.3)
                                    ph) items selected-signal])
                    
                    (if partition?
                      [re-com/v-box
                       :style {:margin-top "6px"}
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
                                     :child (str item)] :operator item])])))]]))

(re-frame/reg-sub
 ::signal-flow-id
 (fn [db _]
   (get-in db [:signals-map (get db :selected-signal) :flow-id])))

(re-frame/reg-sub
 ::signal-open-inputs
 (fn [db _]
   (get-in db [:signals-map (get db :selected-signal) :open-inputs])))

(re-frame/reg-event-db
 ::edit-signal-flow-id
 (undoable)
 (fn [db [_ flow-id]]
   (assoc-in db [:signals-map (get db :selected-signal) :flow-id] flow-id)))

(re-frame/reg-event-db
 ::open-blocks
 ;;(undoable)
 (fn [db [_ result]]
   (-> db
       (assoc-in [:signals-map (get db :selected-signal) :open-inputs] (get result :open-inputs))
       (assoc-in [:signals-map (get db :selected-signal) :blocks] (get result :blocks)))))

;; (re-frame/reg-event-db
;;  ::refresh-open-blocks
;;  ;;(undoable)
;;  (fn [db [_ flow-id]]
;;    ;(tap> [:refresh-runstream-panel (keys (get db :runstreams))])
;;    (re-frame/dispatch [::wfx/request :default
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
  (let [server-flows (map :flow_id @(re-frame/subscribe [::conn/sql-data [:flows-sys]]))
        sql-calls {:flows-sys {:select [:flow_id :file_path :last_modified]
                               :from [:flows]
                               :connection-id "flows-db"
                               :order-by [[3 :desc]]}}
        open-inputs @(re-frame/subscribe [::signal-open-inputs])
        react! [@mode-atom]
        ;signals @(re-frame/subscribe [::signals-map])
        ;selected-signal @(re-frame/subscribe [::selected-signal])
        ;signal-vec (get-in signals [selected-signal :signal])
        ;flow-id (get-in signals [selected-signal :flow-id])
        ww (* (- (last @db/flow-editor-system-mode) 14) 0.318)
        fid @(re-frame/subscribe [::signal-flow-id])
        ]

    (dorun (for [[k query] sql-calls]
             (let [data-exists? @(re-frame/subscribe [::conn/sql-data-exists? [k]])
                   unrun-sql? @(re-frame/subscribe [::conn/sql-query-not-run? [k] query])]
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
                   :style {:border "1px solid #ffffff33"
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
                               :attr {:on-click #(re-frame/dispatch [::edit-signal-flow-id nil])}
                               :child "x"]]]
                  [re-com/typeahead
                   :suggestion-to-string (fn [item]
                                           (str (get item :label)))
                   :render-suggestion (fn [ss _] ;; render in dropdown
                                        [re-com/box
                                         :style {:color "#000000"}
                                         :child (str (get ss :label))])
                   :on-change #(do
                                 (re-frame/dispatch [::refresh-open-blocks (get % :id)])
                                 (re-frame/dispatch [::edit-signal-flow-id (get % :id)]))
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
                                      {:border "1px solid pink"
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
     :style {:border "2px solid maroon"}
     :height (px hh)]))

(defonce searcher-atom (reagent/atom nil))

(defn left-col [ph]
  (let [operators @(re-frame/subscribe [::operators])
        conditions @(re-frame/subscribe [::conditions])
        time-items @(re-frame/subscribe [::time-items])
        signals @(re-frame/subscribe [::signals-map])
        selected-signal @(re-frame/subscribe [::selected-signal])
        signal-vec (get-in signals [selected-signal :signal])
        react! [@selectors-open]
        filter-results (fn [x y]
                         (if (or (nil? x) (empty? x))
                           y
                           (if (map? y)
                             (into {} (filter (fn [[k v]]
                                                (or (cstr/includes? (cstr/lower-case (str k)) (cstr/lower-case x))
                                                    (cstr/includes? (cstr/lower-case (str v)) (cstr/lower-case x))))
                                              y))
                             (vec (filter #(cstr/includes? (cstr/lower-case (str %)) (cstr/lower-case x)) y)))))
        ;fid @(re-frame/subscribe [::signal-flow-id])
        flow-box-hh 148]
    [re-com/v-box
     ;:padding "6px"
     :gap "6px"
     :width "35%"
     :children
     [;(when selected-signal
      ;  [flow-box (- flow-box-hh 10)])
      [re-com/h-box
       :padding "6px"
       :height "50px"
       :align :center :justify :between
       :style {:border "1px solid orange"}
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
                      ;(if selected-signal (+ flow-box-hh 50) 50)
                      ))
       :style {:border "1px solid yellow"
               :overflow "auto"}
       :gap "6px"
       :children [[selector-panel "signals" (filter-results @searcher-atom signals) "zmdi-flash" {} 5]
                  
                  [selector-panel "operators" (filter-results @searcher-atom operators) "zmdi-puzzle-piece" {} 5]
                  [selector-panel "conditions" (filter-results @searcher-atom conditions) "zmdi-puzzle-piece" {} 3]
                  [selector-panel "time items" (filter-results @searcher-atom time-items) "zmdi-calendar-alt" {} 2]
                  
                  [selector-panel "parameters" (filter-results @searcher-atom time-items) "zmdi-shape" {} 2]
                  [selector-panel "flow values" (filter-results @searcher-atom time-items) "zmdi-shape" {} 2]
                  [selector-panel "clients" (filter-results @searcher-atom time-items) "zmdi-desktop-mac" {} 2]

                  [selector-panel "metrics" (filter-results @searcher-atom time-items) "zmdi-equalizer" {} 2]
                  [selector-panel "KPIs" (filter-results @searcher-atom time-items) "zmdi-traffic" {} 2]
                  
                  ]]]]))


(re-frame/reg-event-db
 ::rename-signal
 (undoable)
 (fn [db [_ old new]]
   ;(assoc db :signals-map (cset/rename-keys {old new} (get db :signals-map {})))
   (let [sver (fn [x] (cstr/replace (str x) ":" ""))
         refs {(keyword (str "signal/" (sver old)))
               (keyword (str "signal/" (sver new)))}]
     (-> db
         (assoc :signals-map (walk/postwalk-replace (merge {old new} refs) (get db :signals-map {})))
         (assoc :selected-signal new)))))

(defonce title-edit-idx (reagent/atom nil))

(defn edit-signal-name [signal-name w]
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
       :justify :end
       :attr {:on-double-click #(reset! title-edit-idx (str signal-name))}
       :style {:cursor "pointer"
               :padding-right "12px"
               :padding-top "1px"
               :border "2px solid transparent"
               :font-size "26px"}
       :child (str signal-name)]
      [re-com/input-text
       :src (at)
       :model             (cstr/replace (str signal-name) ":" "")
       :width             (px (- w 6))
       :height            "45px"
       :on-change         #(do (re-frame/dispatch [::rename-signal signal-name
                                                   (try
                                                     (keyword (str %))
                                                     (catch :default _ (str %)))])
                               (reset! title-edit-idx nil))
       :validation-regex  flow-id-regex
       :change-on-blur?   true
       :style  {:border (str "2px dashed " (theme-pull :theme/editor-outer-rim-color nil))
                :font-size "26px"
                :text-decoration "underline"
                :color (theme-pull :theme/editor-outer-rim-color nil)
                :font-style "underline"
                :text-align "center"
                :background-color "#00000000"}])))

(defn right-col [ph]
  (let [signals @(re-frame/subscribe [::signals-map])
        selected-signal @(re-frame/subscribe [::selected-signal])
        signal-vec (get-in signals [selected-signal :signal])]
    ;; (tap> [:right-col ph signals selected-signal signal-vec @db/flow-editor-system-mode])
    [re-com/v-box
     :width "65%"
     :children [(when selected-signal
                  [re-com/v-box
                   :style {:font-family   (theme-pull :theme/monospaced-font nil)
                           :border "1px solid pink"}
                   :height (px (* ph 0.7))
                   :padding "6px"
                   :children
                   [[edit-signal-name selected-signal (* (- (last @db/flow-editor-system-mode) 70) 0.65)]
                    [re-com/box
                     :height (px (- (* (* ph 0.7) 0.75) 50)) :size "none"
                     :padding "6px"
                     :style {:border "1px solid pink" :overflow "auto"}
                     :child (if @db/bad-form-signals?
                              [re-com/box
                               :size "auto"
                               :style {:background-color "rgba(255, 0, 0, 0.3)"
                                       :border "1px solid red"
                                       :padding "6px"
                                       :font-size "20px"}
                               :child (str @db/bad-form-msg-signals)]
                              [rc/catch (visualize-clause signal-vec 0)]
                              ;[re-com/box :child "yo"]
                              )]
                    [re-com/box
                     :style {:border "1px solid orange"}
                     :height (px (- (* (* ph 0.7) 0.25) 12)) ;; "24%" 
                     :padding "6px"
                     :child [code-box
                             (* (- (last @db/flow-editor-system-mode) 14) 0.65) ;; width
                             (- (* (* ph 0.7) 0.25) 5) ;; (* ph 0.25) 
                             (str signal-vec)]]]])
                
                ;; [signals-list (if selected-signal
                ;;               (* ph 0.3)
                ;;               ph) signals selected-signal]

                [re-com/box 
                 :padding "6px"
                 :align :center :justify :center
                 :style {:border "1px solid lime"}
                 ;:height (px (- (* ph 0.3) 20))
                 :child (str (or @hover-tools-atom 
                                 (if (not (nil? selected-signal))
                                   "signal-graph"
                                   "last x signals true")))]
                
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

(defn signals-panel []
  (let [[_ hpct] db/flow-panel-pcts ;; [0.85 0.50]
        hh @(re-frame/subscribe [::subs/h]) ;; we want the reaction 
        panel-height (* hh hpct)
        details-panel-height (/ panel-height 1.25) ;; batshit math from flow.cljs panel parent
        ppanel-height (+ panel-height details-panel-height)
        signals-map @(re-frame/subscribe [::signals-map])
        get-signals-map-evt-vec [::wfx/request :default
                                 {:message    {:kind :signals-map
                                               :client-name @(re-frame/subscribe [::bricks/client-name])}
                                  :on-response [::signals-map-response]
                                  :on-timeout  [::timeout-response :get-signals]
                                  :timeout    15000000}]
        ph (- ppanel-height 124)]
    
    (when (nil? signals-map) 
      (re-frame/dispatch get-signals-map-evt-vec)) ;; if fresh, pull it 

    [re-com/v-box
     :padding "6px"
   ;:size "auto" 
     :width (px (- (last @db/flow-editor-system-mode) 14))
     :size "none"
     :height (px ph)
     :style {:border "1px solid cyan"
             :background-color (str (theme-pull :theme/editor-rim-color nil) "18")}
     :children [[re-com/h-box
                 :padding "6px"
                 :justify :between :align :center 
                 :children [[re-com/h-box 
                             :gap "8px"
                             :children [[re-com/box 
                                         :attr {:on-click #(re-frame/dispatch get-signals-map-evt-vec)}
                                         :style {:cursor "pointer"}
                                         :child "pull from server?"]
                                        ;[re-com/box :child "(diff than the server!)"]
                                        ]]
                            [re-com/box
                             :attr {:on-click #(re-frame/dispatch
                                                [::wfx/request :default
                                                 {:message    {:kind :save-signals-map
                                                               :signals-map @(re-frame/subscribe [::signals-map])
                                                               :client-name @(re-frame/subscribe [::bricks/client-name])}
                                                  ;:on-response [::signals-map-response]
                                                  ;:on-timeout  [::timeout-response :get-signals]
                                                  :timeout    15000000}])}
                             :style {:cursor "pointer"}
                             :child "push this signal set to server?"]]]
                [re-com/gap :size "5px"]
                [re-com/h-box :children [[left-col ph]
                                         [right-col ph]]]]]))