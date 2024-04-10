(ns rvbbit-frontend.buffy
  (:require
   [reagent.core :as reagent]
   [re-frame.core :as re-frame]
   [re-com.core :as re-com :refer [at]]
   [re-com.util :refer [px]]
   [rvbbit-frontend.connections :as conn]
   [re-catch.core :as rc]
   [rvbbit-frontend.utility :as ut]
   [clojure.data :as cdata]
   ;[re-pressed.core :as rp]
   [rvbbit-frontend.audio :as audio]
   [cljs.tools.reader :refer [read-string]]
   [day8.re-frame.undo :as undo :refer [undoable]]
   [clojure.edn :as edn]
   [rvbbit-frontend.db :as db]
   [rvbbit-frontend.subs :as subs]
   ;[rvbbit-frontend.http :as http]
   [rvbbit-frontend.resolver :as resolver]
   [goog.dom :as gdom]
   [rvbbit-frontend.bricks :as bricks :refer [theme-pull]]
   [goog.events :as gevents]
   [clojure.walk :as walk]
   [clojure.string :as cstr]
   [clojure.set :as cset]
   ["react-codemirror2" :as cm]
   ["codemirror/mode/sql/sql.js"]
   ["codemirror/addon/edit/matchbrackets.js"]
   ["codemirror/addon/edit/closebrackets.js"]
   ["codemirror/mode/clojure/clojure.js"]
   [rvbbit-frontend.connections :refer [sql-data]]
   ["react-drag-and-drop" :as rdnd]
   ;[websocket-fx.core :as wfx]
   ;[cljs.tools.reader :refer [read-string]]
   ;[oz.core :as oz]
   ;[reagent.dom :as rdom]
   [websocket-fx.core :as wfx]
   [cljs-drag-n-drop.core :as dnd2]
   ;[rvbbit-frontend.buffy :as buffy]
   )
  (:import [goog.events EventType]
           [goog.async Debouncer]))

;(defonce db/chat-mode (reagent/atom {}))
;(defonce kit-mode (reagent/atom {}))
;(defonce kit-keys (reagent/atom {}))
(defonce kit-pages (reagent/atom {}))
(defonce kit-mutations (reagent/atom {}))

(defonce detached-coords
  (reagent/atom (let [hh (.-innerHeight js/window) ;; starting size set on load
                      ww (.-innerWidth js/window) ;; starting size set on load
                      ;bricks-high (+ (js/Math.floor (/ hh bricks/brick-size)) 1)
                      bricks-wide (+ (js/Math.floor (/ ww bricks/brick-size)) 1)
                      topper (* 2 bricks/brick-size)
                      lefty (* (- bricks-wide 15) bricks/brick-size)]
                  ;(tap> [:coords-editor [lefty topper] bricks-high hh ww])
                  [lefty topper])))

(defn mouse-move-handler [offset]
  (fn [evt]
    (let [start-x (.-clientX evt)
          start-y (.-clientY evt)
          off-x (:x offset)
          off-y (:y offset)
          x      (- start-x off-x)
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

(re-frame/reg-sub
 ::chats
 (fn [db [_ kp]]
   (get-in db [:chats kp]
        ;; [{:text "this is a chat string placeholder this is a chat string placeholder this is a chat string placeholder this is a chat string placeholder this is a chat string placeholder this is a chat string placeholder this is a chat string placeholder"
        ;;             :author "buffy"
        ;;             :timestamp "Tue Nov 07 2023 14 :36:45 GMT-0500 (Eastern Standard Time)"}
           [])))

(re-frame/reg-sub
 ::valid-kits
 (fn [db _]
   (let [kits (get-in db [:server :settings :kits])
        ;_ (tap> [:kits kits])
         ;selected-block (get db :selected-block)
         curr @(re-frame/subscribe [::conn/clicked-parameter-key [:kits-sys/enabled]])
         [item-type item] @(re-frame/subscribe [::bricks/editor-panel-selected-view])
         valid-where? true ;; TODO
        ; _ (tap> [:kits kits item-type item])
         filtered-map (into {}
                            (for [[k v] kits
                                  :when (and  (some #(= [(get v :package-name) (get v :kit-name)] %) curr) ;; is enabled in params?
                                              valid-where? (= (get v :run-on) item-type))]
                              {k v}))]
     ;(tap> [:filtered-map filtered-map])
     filtered-map)))

(defonce hide-diffs? (reagent/atom false))
(defonce hide-block-diffs? (reagent/atom false))

(defn scroll-div-to-bottom [div]
  (let [scrollHeight (.-scrollHeight div)
        offsetHeight (.-offsetHeight div)]
    (set! (.-scrollTop div) (- scrollHeight offsetHeight))))

(defn scroll-to-bottom [id]
  (let [element (gdom/getElement id)]
    (scroll-div-to-bottom element)))

;; (defn scroll-to-element [container-id element-id]
;;   (let [container (gdom/getElement container-id)
;;         element (gdom/getElement element-id)
;;         container-top (.-scrollTop container)
;;         element-top (.-offsetTop element)
;;         container-height (.-offsetHeight container)]
;;     (set! (.-scrollTop container) (- (- element-top container-top) container-height))))

(defn scroll-to-element [container-id element-id]
  (let [container (gdom/getElement container-id)
        element (gdom/getElement element-id)
        container-top (.-scrollTop container)
        element-top (.-offsetTop element)]
    (set! (.-scrollTop container) (- element-top container-top))))

(defn smooth-scroll-to-element [container-id element-id]
  (try
    (let [container (gdom/getElement container-id)
          element (or (gdom/getElement element-id) "") ;; blocks some weird js crash with nil elemnt hiccup?
          container-top (.-scrollTop container)
          element-top (- (.-offsetTop element) 40) ;; 40 is the height of the header, so offset
          start (atom nil)
          duration 500]
      (letfn [(step [timestamp]
                (when (nil? @start) (reset! start timestamp))
                (let [progress (/ (- timestamp @start) duration)
                      new-scroll-top (+ container-top (* progress (- element-top container-top)))]
                  (set! (.-scrollTop container) new-scroll-top)
                  (when (< progress 1)
                    (.requestAnimationFrame js/window step))))]
        (.requestAnimationFrame js/window step)))
    (catch :default _ nil)))

(defn smooth-scroll-to-bottom [container-id element-id]
  (let [container (gdom/getElement container-id)
        element (gdom/getElement element-id)
        container-top (.-scrollTop container)
        element-top (+ (.-offsetTop element) (.-offsetHeight element)) ;; scroll to bottom
        start (atom nil)
        duration 500]
    (letfn [(step [timestamp]
              (when (nil? @start) (reset! start timestamp))
              (let [progress (/ (- timestamp @start) duration)
                    new-scroll-top (+ container-top (* progress (- element-top container-top)))]
                (set! (.-scrollTop container) new-scroll-top)
                (when (< progress 1)
                  (.requestAnimationFrame js/window step))))]
      (.requestAnimationFrame js/window step))))

;; (defn scroll-to-bottom [element]
;;   (set! (.-scrollTop element) (.-scrollHeight element)))

;; (defn start-observing [element]
;;   (let [observer (js/MutationObserver. (fn [mutations observer]
;;                                          (scroll-to-bottom element)))]
;;     (.observe observer element (clj->js {:childList true :subtree true}))))


(re-frame/reg-sub
 ::table-meta-chat
 (fn [db [_]]
   (let [qid (first (keys (get-in db [:panels (get db :selected-block) :queries])))]
     (str "I have a SQL system that takes Clojure HoneySQL (map style) queries as input. Do not use functions, only data-structure based queries.
           Do not use sql/call everything is a map and a vector. Instead of :count :*, please use :count 1 - and the proper aliasing for aggregations is [[:count 1] :alias]
           Given this table metadata and referring to this table as :query/" (ut/replacer qid #":" "") " what are some interesting queries that I could run?  "
          (str "```clojure " (get-in db [:meta qid :fields] {}) "``` ")))))

(re-frame/reg-sub
 ::recos-meta-chat
 (fn [db [_]]
   (let [qid (first (keys (get-in db [:panels (get db :selected-block) :queries])))
         recos (vec (take 10000 ;;; 1k max?
                          (for [row (get-in db [:data :recos-sys2] [])]
                            (vec (vals (select-keys row [:combo_hash :shape_name :combo_edn]))))))]
     (str "For this table, I have a list generated visualizations - could you recommend a few based on their descriptions here please?
           Just give me the combo_hash ids and a quick description for why you chose that one or ones please.
           The format is a vector of vectors with keys :combo_hash (the id we need to render) :shape_name (the viz desc name) :combo_edn (the fields involved).
           List: ```clojure"
          recos " ```  Lets do top 10 please. For each recommendation please include a bit of code so I can render it as ```[:reco-preview *combo-hash-id*]``` . Put this between your title and reason explanation please."))))

(re-frame/reg-sub
 ::object-meta-chat
 (fn [db [_ kp]]
   (let [;qid (first (keys (get-in db [:panels (get db :selected-block) :queries])))
         view-code (get-in db (cons :panels kp))
         view-code-kps (first (filter #(= (last %) :data)
                                      (ut/kvpaths view-code)))
        ; _ (tap> [:view-code-kps kp (get-in view-code view-code-kps)])
         ;qid (or ())
         vt-flat (ut/deep-flatten view-code)
         view-type (cond (some #(= % :ResponsiveContainer) vt-flat) :recharts
                         (some #(cstr/starts-with? (str %) ":nivo-") vt-flat) :nivo
                         (some #(= % :vega) vt-flat) :vega
                         (some #(= % :vega-lite) vt-flat) :vega-lite
                         :else :hiccup-re-com)
         type-explain (condp = view-type
                        :nivo "Nivo React charting library with the base functions having keyword counterparts as follows: :nivo-bar-chart = nivo-bar/BarCanvas,
:nivo-line-chart= nivo-line/LineCanvas,
:nivo-calendar  = nivo-calendar/Calendar,
:nivo-pie-chart = nivo-pie/PieCanvas,
:nivo-waffle-chart = nivo-waffle/WaffleCanvas,
:nivo-scatterplot = nivo-scatterplot/ScatterPlotCanvas,
:nivo-swarmplot = nivo-swarmplot/SwarmPlot,
:nivo-treemap   = nivo-treemap/TreeMap, etc"
                        ;:recharts "Recharts React library with keywordized function call names (i.e. :Bar, :BarChart, etc)"
                        :recharts "Recharts React library"
                        :vega "Vega charting library as EDN using the ClojureScript Oz library"
                        :vega-lite "Vega-Lite charting library as EDN using the ClojureScript Oz library"
                        :hiccup-re-com "Clojure Hiccup and re-com libraries, with re-com functions being keywordized (i.e. :re-com/box, :re-com/v-box, etc)")
         qid (or (get-in view-code view-code-kps) (first (keys (get-in db [:panels (get db :selected-block) :queries]))))
         obj (get-in db (cons :panels kp) {})
         type (get kp 1) ; :views :queries
         qid (if (= type :queries) (first (keys (get-in db [:panels (get db :selected-block) :queries]))) qid)
         meta-fields (into {} (for [[k v] (get-in db [:meta qid :fields] {})] {k (merge v {:commons (vec (keys (get v :commons)))})}))]

     (cond (= type :views)
           (str "Hey Rabbit! I have a Clojure Vector based DSL for data visualization which uses an EDN format. I am using reagent components based off of the " type-explain ". Can you show me some viz variations I can try?
           Please preface the examples with rabbit-view before you enclose the, in triple backticks for code. Please make sure that the vector brackets and curly brackets are balanced and closed and it is a
           valid Clojure datastructure. Do not use functions, just the keywords that correspond to recharts functions as you'll see in my example code. Use only the data fields I have used here since they are specifc to
           the query you are working with. Feel free to be creative within these confines with various rechart options and configs. Do not modify the dimensions of the chart since they are specific to
           it's container, and experimenting with different sizes has no value to the user. Reminder EDN keywords have the color up front, not on the back. Do not modify the options map in :ResponsiveContainer at all. Please try to provide value. Don't concern yourself with modifying the CartesianGrid component,
           since it only draws lines, focus on different ways to visualize the data so a human can interpret it.
           Here is my code and also an example of how I want your variations presented:"
                "rabbit-view```"
                (str obj)
                "```

           Also, you cannot change the query - but for reference, here is the metadata for the query that underlies this view, which might be useful: "
                "```" meta-fields "```"
                "Commons are common value examples, distinct is the number of distinct values in the dataset, cardinality is what percent of the total rows are unique, data-type is self explanatory,
           and group-by tells if a value is an aggregate or a group-by. Use this information to help you decide how to best visualize the data within the confines of the request."
             ; ". And here is a max 20 row sample of the data itself: "
             ; "```"(vec (take 20 (get-in db [:data qid]))) "```"
                " The full dataset is "
                (count (get-in db [:data qid]))
                " rows. When choosing colors keep in mind that this visualization will be displayed on a " (theme-pull :theme/editor-background-color nil)
                " background. Let's try to present at least 5 options of various reasonable types please, but also include at least 2 fairly 'far out' creative examples to help inspire and iterate.")

           (= type :queries)
           (str "Hey Rabbit! I have a SQL interface that takes Clojure HoneySQL (map style) queries as input. Do not use functions, only data-structure based queries.
           Do not use sql/call everything is a map and a vector. Instead of :count :*, please use :count 1 - and the proper aliasing for aggregations is [[:count 1] :alias],
                 you can't order by aggregation calcs, please use their position number instead. Proper order by syntax is [[:field-name :asc] ...].
           Given this table metadata and referring to this table as :query/" (ut/replacer qid #":" "") " Based on this what are some interesting queries that I could run?  "
                (str "```" meta-fields "``` ")
                "Commons are common value examples, distinct is the number of distinct values in the dataset, cardinality is what percent of the total rows are unique, data-type is self explanatory,
           and group-by tells if a value is an aggregate or a group-by. Use this information to help you decide how to best visualize the data within the confines of the request."
                "If you need to go deeper, the parent table " (get-in obj [:from 0]) " has this metadata: "
                "```" (into {} (for [[k v] (get-in db [:meta (keyword (ut/replacer (get-in obj [:from 0]) ":query/" "")) :fields] {})] {k (merge v {:commons (vec (keys (get v :commons)))})})) "```")

           :else "Howdy."))))

;;;(re-frame/dispatch [::conn/click-parameter [panel-key] {raw-param-key pval}])
(re-frame/reg-event-db
 ::click-parameter
 (fn [db [_ keypath value]]
   (tap> [:setting-kit-sourced-click-params keypath value])
   (let [curr (get-in db (cons :click-param keypath))]
     (assoc-in db (cons :click-param keypath) (merge curr value)))))
;;; DUPE OF CONN/CLICK-PARAMETER due to diff logic
;; its a simple fn, but still


(defn code-box [width-int height-int value]
  (let [;sql-hint? (cstr/includes? (str value) ":::sql-string")
        ;kp        [:panels panel-id :queries query-key :select idx]
        ]
    [re-com/box
     :size "auto"
     :width (px (- width-int 24))
     :height (px (- height-int 24))
     :style {:font-family   (theme-pull :theme/monospaced-font nil) ; "Chivo Mono" ;"Fira Code"
             :font-size     "14px"
             :overflow      "auto"
             :border-radius "12px"
             :font-weight   700}
     :child [(reagent/adapt-react-class cm/UnControlled)
             {:value   (ut/format-map (- width-int 24)
                                      (str value))
             ; :onBlur  #(re-frame/dispatch-sync [::update-selected-field kp (read-string (cstr/join " " (ut/cm-deep-values %)))])
              :options {:mode              "clojure"
                        :lineWrapping      true
                        :lineNumbers       true
                        :matchBrackets     true
                        :autoCloseBrackets true
                        :autofocus         false
                        :autoScroll        false
                        :detach            true
                        :readOnly          true            ;true
                        :theme             (theme-pull :theme/codemirror-theme nil) ;"ayu-mirage" ;"hopscotch"
                        }}]]))

(re-frame/reg-event-db
 ::save-rs-value
 (fn [db [_ flow-id kkey source value]]
   (-> db
       (assoc-in [:runstreams flow-id :values kkey :value] value)
       (assoc-in [:runstreams flow-id :values kkey :source] source))))

(re-frame/reg-sub
 ::rs-value ;; duped in AUDIO TOO, OMG please condense !!!!!
 (fn [db [_ flow-id kkey]]  ;;; has a dupe in bricks!!! TODO, please unify LOGIC!!!!
   (let [src (get-in db [:runstreams flow-id :values kkey :source])]
     (if (= src :param)
       ;(get-in db [:runstreams flow-id :values kkey :value])
       (first @(re-frame/subscribe [::resolver/logic-and-params [(get-in db [:runstreams flow-id :values kkey :value])]]))
       (get-in db [:runstreams flow-id :values kkey :value])))))

(defn code-box-rs-value [width-int height-int flow-id kkey value & [param?]]
  (let [param? (if (nil? param?) false true)
        stringify? (and (vector? value) (every? string? value))]
    ;;(tap> [:vv value stringify?])
    [re-com/box
     :size "auto"
     :width (px (- width-int 24))
     :height (px (- height-int 24))
     :style {:font-family   (theme-pull :theme/monospaced-font nil)
             :font-size     "14px"
             :overflow      "auto"
             :border-radius "12px"
             :font-weight   700}
     :child [(reagent/adapt-react-class cm/UnControlled)
             {:value   (if stringify?
                         (str (cstr/join "\n" value))
                         (ut/format-map (- width-int 24)
                                        (str value)))
              :onBlur  #(when (not param?)
                          (re-frame/dispatch [::save-rs-value
                                              flow-id kkey :input
                                              (try
                                                (read-string (cstr/join " " (ut/cm-deep-values %)))
                                                (catch :default _ (pr-str (cstr/join "\n" (ut/cm-deep-values %)))))])) ;; python?
              :options {:mode              (when (not stringify?) "clojure") ;(if stringify? "text" "clojure")
                        :lineWrapping      true
                        :lineNumbers       true
                        :matchBrackets     true
                        :autoCloseBrackets true
                        :autofocus         false
                        :autoScroll        false
                        :detach            true
                        :readOnly          param?
                        :theme             (theme-pull :theme/codemirror-theme nil)}}]]))


;; :sidekicks { :narratives { :narrative-1 [THIS] }} [:kits :naraatives :base :narrative-1 :data]


(defn read-if-keyworded-str [x] (if (cstr/starts-with? (str x) ":") (try (edn/read-string x) (catch :default _ :error!)) (keyword x)))
(defn kit-rows-to-map [rowset]
  (try
    (let [mapped (into {} (for [[k v] (group-by :kit_name rowset)]
                            {(read-if-keyworded-str k)
                             {(read-if-keyworded-str (get (first v) :item_name))
                              (into {} (for [[k v] (group-by :item_key v)]
                                         {(edn/read-string k)
                                          (merge
                                           (edn/read-string (get (first v) :item_options))
                                           {;:id (get (first v) :id)
                                            :data (vec (for [d v]
                                                         (merge {:id (get d :id)}
                                                                (edn/read-string (get d :item_data)))))})}))}}))]
      {:kits mapped})
    (catch :default _ {})))

;; (defonce kit-rows-to-map-atom (reagent/atom {}))

;; (defn kit-rows-to-map77 [rowset]
;;   (let [rshash (hash rowset)
;;         cache (get @kit-rows-to-map-atom rshash)]
;;     (if cache
;;       cache
;;       (let [m (kit-rows-to-map-fn rowset)]
;;         (swap! kit-rows-to-map-atom assoc rshash m)
;;         m))))

(declare render-honey-comb-fragments)

(defn option-buttons [kp mode]
  (let [;meta-fields-msg @(re-frame/subscribe [::table-meta-chat])
        ;recos-fields-msg @(re-frame/subscribe [::recos-meta-chat])
        ;curr-narrative (get @db/kit-keys kp) ;"narrative-1"
       ; [r1 r2] @(re-frame/subscribe [::bricks/query-waitings :kit-results-sys])
       ; running? (or r1 r2)
        data-kp (vec (cons :kit-results-sys kp))

        kit-results      @(re-frame.core/subscribe [::conn/sql-data [:kit-results-sys]])
        kits             (kit-rows-to-map kit-results)

        kit-name         (get @db/kit-mode kp)
        kick-kit-name    (last (keys (get kits :kits)))
        kit-name         (if (or (= kit-name :kick) (= kit-name :ai/calliope)) kick-kit-name kit-name)
        ;_ (tap> [:kit-name kit-name])
        ;kit-base-name    kit-name
        kit-context-name (last kp) ;; :base
        kit-context-name (if (nil? kit-context-name) :base kit-context-name)
        ik (get-in kit-results [0 :item_key])
        item-key (if (cstr/starts-with? (str ik) ":") (try (edn/read-string ik) (catch :default _ "error!")) (keyword ik))
        curr-narrative (get @db/kit-keys kp item-key)

        kit-mutations? (get @kit-mutations kp false)
        narrative-desc (when (or (= mode :narratives) (or (= mode :kick) (= mode :buffy)))
                         (get-in kits [:kits kit-name kit-context-name curr-narrative :description]))
        narratives (when (or (= mode :narratives) (or (= mode :kick) (= mode :buffy)))
                     (keys (get-in kits [:kits kit-name kit-context-name])))
        narrative-items (when (or (= mode :narratives) (or (= mode :kick) (= mode :buffy)))
                          (count (get-in kits [:kits kit-name kit-context-name curr-narrative :data])))
        narrative-item (get @kit-pages kp 0)
        ai-assignments {:select [:*]
                        :order-by [[:need-feedback :desc]]
                        :style-rules {[:* :highlight-3109s]
                                      {:logic [:= :need-feedback "true"]
                                       :style {:background-color "#fdfd9611"
                                               :border "1px solid #fdfd9688"}}}
                        :from   [{:data [{:task-type ":watch"    :item-type ":table" :item ":system-db/errors"  :reqs 0 :need-feedback "false" :wip "false"}
                                         {:task-type ":maintain" :item-type ":board" :item "error-monitor" :reqs 0 :need-feedback "true" :wip "false"}
                                         {:task-type ":maintain" :item-type ":flow" :item "calliope-loop1" :reqs 0 :need-feedback "false" :wip "false"}
                                         {:task-type ":maintain" :item-type ":flow" :item "calliope-loop2" :reqs 0 :need-feedback "false" :wip "false"}
                                         {:task-type ":create"   :item-type ":board" :item "ufo-basic-countries" :reqs 0 :need-feedback "true" :wip "true"}
                                         {:task-type ":watch" :item-type ":metric"  :item ":system-db/errors.count" :reqs 0 :need-feedback "false" :wip "false"}]}]}
        ;_ (tap> [:options-buttons kit-name kit-context-name curr-narrative ik item-key kits])
        react-hack @hide-diffs?
        meta-object-msg (when (= mode :buffy2) @(re-frame/subscribe [::object-meta-chat kp]))]

    (when (or (nil? (get @db/kit-keys kp)) (not (some #(= (get @db/kit-keys kp) %) narratives)))
      (swap! db/kit-keys assoc kp item-key))
    ;(tap> [:options-buttons kp  (get @db/kit-keys kp) (get @kit-pages kp)])
    ;(tap> [:meta-fields meta-fields-msg :recos-fields-msg recos-fields-msg :obj meta-object-msg])
    [(if (or (= mode :narratives)
             (or (= mode :kick) (= mode :buffy)))
       re-com/v-box re-com/h-box)
     :gap "9px"
     :align :center :justify :center
     :children (cond (= mode :snapshots)
                     [[re-com/box
                       :padding "8px"
                       :style {;:background-color "orange"
                               :color (theme-pull :theme/editor-outer-rim-color nil) ;"black"
                               :cursor "pointer"}
                       :attr {:on-click #(re-frame/dispatch [::create-snapshot])}
                       :child "create new snapshot"]

                      [re-com/box
                       :padding "8px"
                       :style {;:background-color "orange"
                               :color (theme-pull :theme/editor-outer-rim-color nil) ;"black"
                               :opacity 0.5
                               :text-decoration (when @hide-diffs? "line-through")
                               :cursor "pointer"}
                       :attr {:on-click #(reset! hide-diffs? (not @hide-diffs?))}
                       :child "diffs?"]

                      [re-com/box
                       :padding "8px"
                       :style {;:background-color "orange"
                               :color (theme-pull :theme/editor-outer-rim-color nil) ;"black"
                               :opacity 0.5
                               :text-decoration (when @hide-block-diffs? "line-through")
                               :cursor "pointer"}
                       :attr {:on-click #(reset! hide-block-diffs? (not @hide-block-diffs?))}
                       :child "block diffs?"]

            ;; [re-com/box
            ;;  :padding "8px"
            ;;  :style {;:background-color "orange"
            ;;          :color (theme-pull :theme/editor-outer-rim-color nil) ;"black"
            ;;          :opacity 0.5
            ;;          :cursor "pointer"}
            ;;  :attr {:on-click #(re-frame/dispatch [::audio/clear-chat kp])}
            ;;  :child "(clear)"]
                      ]

                     (or (= mode :narratives)
                         (or (= mode :kick)
                             ;(= mode :buffy)
                             ))
                     [[re-com/box
                       :padding "4px"
                       :size "auto"
                       ;:align :center
                       :justify :center
                       :style {;:background-color "orange"
                               ;:border "1px solid yellow"
                               :color (theme-pull :theme/editor-outer-rim-color nil) ;"black"
                               :overflow "auto"
                               :font-weight 700
                               ;:cursor "pointer"
                               }
                       :height "44px"
                        ;:attr {:on-click #(re-frame/dispatch [::audio/add-chat meta-object-msg kp])}
                       :child (str narrative-desc)]

                      [re-com/h-box
                       :gap "10px"
                       :justify :between
                       :children [[re-com/h-box

                                   :children [[re-com/single-dropdown
                                               :model curr-narrative
                                               :width "135px"
                                               :style {:background-color "#00000045"
                                                       :border-radius "10px"
                                                       :border "1px solid #ffffff18"}
                                               :on-change #(swap! db/kit-keys assoc kp %)
                                               :choices (vec (map (fn [n] {:id n :label n}) narratives))]]]
                                  [re-com/v-box
                                   :align :center :justify :center
                                   :height "35px"
                                   :padding "4px"
                                   :attr {:on-click #(swap! kit-mutations assoc kp (not (get @kit-mutations kp false)))}
                                   :style {:font-size "10px"
                                           :text-decoration (when (not kit-mutations?) "line-through")
                                           :color (when (not kit-mutations?) "#ffffff50")
                                           :cursor "pointer"
                                           :background-color "#00000045"
                                           :border-radius "10px"
                                           :border "1px solid #ffffff18"}
                                   :children [[re-com/box :child "allow step"]
                                              [re-com/box :child "mutations?"]]]
                                  [re-com/h-box ;; moode :buffy implied
                                   :padding "8px"
                                   :style {;:background-color "orange"
                                           :color (theme-pull :theme/editor-outer-rim-color nil) ;"black"
                               ;:cursor "pointer"
                                           ;:border "1px solid pink"
                                           :border-radius "6px"
                                           :border "1px solid #ffffff19"}
                                   ;:width "500px"
                                   :height "35px"
                                   :size "auto"
                                   :justify :between :align :center
                                   :gap "6px"
                        ;:attr {:on-click #(re-frame/dispatch [::audio/add-chat meta-object-msg kp])}
                                   :children (let [mmin 0
                                                   mmax (- narrative-items 1)
                                                   is-last? (= narrative-item mmax)
                                                   is-first? (= narrative-item mmin)]
                                               [[re-com/md-icon-button :src (at)
                                                 :md-icon-name "zmdi-chevron-left"
                                                 :on-click #(when (not is-first?) (swap! kit-pages assoc kp (dec narrative-item)))
                                                 :style {:color (theme-pull :theme/editor-outer-rim-color nil)
                                                         :cursor "pointer"
                                                         :font-size "23px"}]
                                                [re-com/slider
                                                 :model narrative-item
                                                 :on-change #(swap! kit-pages assoc kp %)
                                                 :min 0 :max mmax
                                                 :width "245px"]
                                                [re-com/md-icon-button :src (at)
                                                 :md-icon-name "zmdi-chevron-right"
                                                 :on-click #(when (not is-last?) (swap! kit-pages assoc kp (inc narrative-item)))
                                                 :style {:color (theme-pull :theme/editor-outer-rim-color nil)
                                                         :cursor "pointer"
                                                         :font-size "23px"}]])]]]]


                    ;;  :else [[re-com/box ;; moode :buffy implied - OLD buffy panel - pre calliope demo
                    ;;          :padding "8px"
                    ;;          :size "auto"
                    ;;          :style {;:background-color "orange"
                    ;;                  :color (theme-pull :theme/editor-outer-rim-color nil) ;"black"
                    ;;                  :cursor "pointer"}
                    ;;         ; :attr {:on-click #(re-frame/dispatch [::audio/add-chat meta-object-msg kp])}
                    ;;          :child "request variations"]

                    ;;         [re-com/box
                    ;;          :padding "8px"
                    ;;          :size "auto"
                    ;;          :style {;:background-color "orange"
                    ;;                  :color (theme-pull :theme/editor-outer-rim-color nil) ;"black"
                    ;;                  :opacity 0.5
                    ;;                  :cursor "pointer"}
                    ;;          ;:attr {:on-click #(re-frame/dispatch [::audio/clear-chat kp])}
                    ;;          :child "(clear)"]]


                     :else [[re-com/h-box
                             :size "none"
                             :height "33px"
                             :width "580px"
                             :padding "6px"
                             ;:style {:border "1px solid white"}
                             :justify :between :align :center
                             :children
                             [;[re-com/box :child "rabbit worker"]
                              [re-com/h-box :gap "4px"
                               :children
                               [[re-com/md-icon-button :src (at)
                                 :md-icon-name "zmdi-chevron-up"
                                 :style {:color (theme-pull :theme/editor-outer-rim-color nil)
                                         :cursor "pointer"
                                         :margin-top "-1px"
                                         :font-size "23px"}]
                                [re-com/box :child " :ai-worker/ " :style {}]
                                [re-com/box :child " Calliope"  ; " Buffy"
                                 :style {:font-family "Homemade Apple" :color "orange" :margin-top "2px"}]]]

                              [re-com/h-box ;:align :center
                               :justify :center
                               :size "none" :width "210px" :height "21px" ;:style {:border "1px solid white" }
                               :gap "8px"
                               :children [[re-com/md-icon-button :src (at)
                                           :md-icon-name "zmdi-circle"
                                           :style {:color "#dddd77" ;(theme-pull :theme/editor-outer-rim-color nil)
                                                   :cursor "pointer"
                                                   :margin-top "-2px"
                                                   :font-size "16px"}]
                                          [re-com/box :child "allocated"]
                                          [re-com/progress-bar
                                           ;:style {:fill "green"}
                                           :model 70 :width "100px"]]]]]

                            [re-com/box :child [render-honey-comb-fragments ai-assignments 11.6 6] :style {:padding-left "5px"}]

                            [re-com/box
                             :style {:margin-top "-60px" :margin-right "-342px"}
                             :child [re-com/single-dropdown
                                     :model curr-narrative
                                     :width "235px"
                                     :style {:background-color "#00000045"
                                             :border-radius "10px"
                                             :border "1px solid #ffffff18"}
                                     :on-change #(swap! db/kit-keys assoc kp %)
                                     :choices (vec (map (fn [n] {:id n :label n}) narratives))]]

                            ;[re-com/gap :size "5px"]
                            ])]))

(defn parse-output [text kp role]
  (let [pre-text text
        text (cstr/replace (str text) #"\n" "")
        splt (vec (remove #(= % "rabbit-view") (doall (cstr/split text "```"))))
        ;splt (map cstr/trim-newline (cstr/split text "```"))
        type (second kp)
        pre-splt (vec (remove #(= % "rabbit-view") (doall (cstr/split pre-text "```"))))]
    (tap> [:tt text splt])
    ;text
    [rc/catch
     [re-com/v-box
      :gap "10px"
      :width "580px"
      :children (doall (for [idx (range (count splt))
                             :let [s (get splt idx)
                                   st (get pre-splt idx)]]
                         (if (or (cstr/starts-with? (cstr/trim s) "[:")
                                 (cstr/starts-with? (cstr/trim s) "[ :")
                                 (cstr/starts-with? (cstr/trim s) "[  :")
                                 (cstr/starts-with? (cstr/trim s) "clojure")
                                 (cstr/starts-with? (cstr/trim s) "clj")
                                 (cstr/starts-with? (cstr/trim s) "{:")
                                 (cstr/starts-with? (cstr/trim s) "rabbit-view")
                                 (cstr/starts-with? (cstr/trim s) "Clojure"))
                  ;[re-com/box :child (str "!"s)]
                  ;[bricks/honeycomb (first kp) key 11 9 (last kp) nil]
                           [re-com/v-box
                            :gap "12px"
                            :children (let [s (-> (str s)
                                                  (cstr/replace "rabbit-view" "")
                                                  (cstr/replace "Clojure" "")
                                                  (cstr/replace "clojure" "")
                                                  (cstr/replace "clj" "")
                                                  (cstr/replace "(" "[")
                                                  (cstr/replace ")" "]")
                                                   ;(cstr/replace "'" "\"")
                                         ;(cstr/replace "rabbit-view" "")
                                                  )
                                            is-meta? (and (cstr/includes? s ":commons")
                                                          (cstr/includes? s ":group-by?")
                                                          (cstr/includes? s ":cardinality"))]
                                        [;[re-com/box :child (str s)]
                                         [re-com/v-box
                                          :padding "8px"
                                          :width "560px"
                                          :size "auto"
                                          :style {:border (when (not is-meta?) "2px dashed orange")}
                                          :gap "8px"
                                          :children [(try [rc/catch
                                                           [re-com/v-box
                                                            :padding "6px"
                                                            :children [(when (not is-meta?)
                                                                         [re-com/h-box
                                                                        ;:padding "6px"
                                                                          :justify :between :align :center
                                                                          :children
                                                                          [(bricks/draggable
                                                                            ((if (= type :queries)
                                                                               bricks/sql-spawner-chat
                                                                               bricks/view-spawner-chat) (if (= type :queries) (last kp) kp)
                                                                                                         (edn/read-string (str s))) "meta-menu"
                                                                            [re-com/md-icon-button :src (at)
                                                                             :md-icon-name "zmdi-code-setting"
                                                                             :style {;:color (theme-pull :theme/editor-font-color nil)
                                                                                     :cursor "grab"
                                                                                     :color "orange"
                                                                                     :height "15px"
                                                                                     :margin-top "-2px"
                                                                                     :font-size "19px"}])
                                                                           [re-com/box
                                                                            :style {:color "orange" :cursor "pointer"}
                                                                            :child "swap to?"
                                                                            :attr {:on-click #(re-frame/dispatch [::update-item kp (edn/read-string (str s))])}
                                                                            :align :end :justify :end :size "auto"]]])
                                                                       [re-com/gap :size "6px"]
                                                                       [code-box 580 nil (str s)]]]] (catch :default e [re-com/box :child (str e)]))

                                                     (try [re-com/box
                                               ;:style {:background-color "#33333322"}
                                                           :size "none"
                                                           :child (if (cstr/includes? s ":select")
                                                                    (let [tmp-key (keyword (str (ut/replacer (last kp) #":" "") "-hist-" (hash s)))]
                                                                      [bricks/honeycomb (first kp) tmp-key 6 11 nil {tmp-key (edn/read-string (str s))}])
                                                                    [bricks/honeycomb (first kp) (last kp) 6 9 {(last kp) (edn/read-string (str s))} nil])]

                                                          (catch :default e [re-com/v-box
                                                                             :gap "10px"
                                                                             :children [[re-com/box
                                                                                         :size "auto"
                                                                            ; :width "560px"
                                                                                         :padding "10px"
                                                                                         :style {:border "1px solid red"
                                                                                                 :color "red"}
                                                                                         :child (str "gpt bad code: " (cljs.core/ex-message e))]
                                                                                        [re-com/box :child (str s)]]])
                                ;;[re-com/box :child (str s)]
                                                          )
                                                     [re-com/gap :size "10px"]]]])]

                           (let [ss (-> (str st)
                                        (cstr/replace "rabbit-view" ""))
                                 user? (not (= role "assistant"))]
                             [re-com/box
                              :style {:padding-left "15px" :margin-right "15px"
                                      :opacity (if user? 0.6 1)
                                      :font-style (if user? "italic" "normal")}
                              :child [re-com/v-box :children (for [line (cstr/split ss #"\n")] [re-com/box :child (str line)])]]))
                ;;  (cond (cstr/starts-with? s "clojure")
                ;;    ;[re-com/box :child (ut/replacer (str s) "clojure" "") :style {:font-family "Consolas"}]
                ;;        (let [ss (ut/replacer (str s) "clojure" "")
                ;;              sdata (try (edn/read-string ss) (catch :default _ {}))
                ;;              tbl-src (keyword (last (cstr/split (str (first (flatten (get sdata :from)))) "/")))]
                ;;      ;(tap> [:ttest tbl-src sdata (flatten (get sdata :from))])
                ;;          (bricks/draggable
                ;;           (bricks/sql-spawner-chat tbl-src sdata) "meta-menu"
                ;;           [code-box 580 nil ss]))

                ;;        (or (cstr/starts-with? s "rabbit-view") (cstr/starts-with? s "[:"))
                ;;    ;[re-com/box :child (ut/replacer (str s) "clojure" "") :style {:font-family "Consolas"}]
                ;;        (let [ss (ut/replacer (str s) "rabbit-view" "")
                ;;              sdata (try (edn/read-string ss) (catch :default _ {}))
                ;;              tbl-src (keyword (last (cstr/split (str (first (flatten (get sdata :from)))) "/")))]
                ;;      ;(tap> [:ttest tbl-src sdata (flatten (get sdata :from))])
                ;;          (bricks/draggable
                ;;           (bricks/sql-spawner-chat tbl-src sdata) "meta-menu"
                ;;           [code-box 580 nil ss]))

                ;;    ;[re-com/box :child [:pre s]]
                ;;        :else [re-com/box :child (str s)
                ;;               ;[re-com/v-box :children (for [r (cstr/split s #"\n")] [re-com/box :child (str r)])]
                ;;               ])
                         ))]]))

(defn waiting-box []
  (let [assistant? true
        role "assistant"]
    [re-com/v-box
     :padding "10px" :gap "5px" :size "none" :width "575px"
     :style {:border-bottom (if assistant? "2px dashed #ffffff17" "inherit")
             :background-color (if assistant? "inherit" (str (theme-pull :theme/editor-rim-color nil) 89))}
     :children [[re-com/h-box
                 :justify :between :align :center
                 :height "33px"
                 :children ((if assistant? reverse vec)
                            [[re-com/box
                              :padding "4px"
                              :style {:font-weight 700 :font-size "15px"}
                              :child (str "waiting on " role)]
                             [re-com/box
                              :style {:font-size "10px"
                                      :color (str (theme-pull :theme/editor-font-color nil) 78)}
                              :child [re-com/md-icon-button :src (at)
                                      :md-icon-name "zmdi-rotate-right"
                                      :class "rotate linear infinite"
                                      :style {:color (theme-pull :theme/editor-font-color nil)
                                              :transform-origin "10px 11px"
                                              :font-size "20px"}]]])]]]))

(defn chat-box [panel-height panel-width chats-vec kp]
  (let [text-box-height 110
        text-box? true]

    (reagent.core/next-tick #(scroll-to-bottom "chat-v-box"))

    [re-com/box
     :padding "5px"
     :size "none"
     :height (px (- panel-height 12 25 (when text-box? text-box-height))) ;; minus size of border top and bottom
     :width (px (- panel-width 12)) ;; minus size of border left and right and header
     :attr {:id "chat-v-box"}
     :style {:overflow "auto"
             :border-radius "16px"}
     :child [re-com/v-box
             :gap "7px"
             :width "575px"
             :children (doall (for [{:keys [role content timestamp body]}
                                    (if (some #(= % kp) @audio/waiting-for-response)
                                      (vec (conj chats-vec {:body [waiting-box]}))
                                      ;(vec (conj chats-vec {:body multiple-choice}))
                                      chats-vec)]

                                (let [assistant? (= role "assistant")]

                                  (if body
                                    body
                                    [re-com/v-box
                                     :padding "10px" :gap "5px" :size "none" :width "575px"
                                     :style {;:border "1px solid orange"
                                             :border-bottom (if assistant? "2px dashed #ffffff17" "inherit")
                                                         ;:border-right (if assistant? "2px dashed #ffffff17" "inherit")
                                             :background-color (if assistant? "inherit" (str (theme-pull :theme/editor-rim-color nil) 89))}
                                     :children [[re-com/h-box
                                                 :justify :between :align :center
                                                 :height "33px"
                                                 ;:style {:border "1px solid yellow"}
                                                 :children ((if assistant? reverse vec)
                                                            [[re-com/box
                                                              :padding "4px"
                                                              :style {:font-weight 700 :font-size "15px"}
                                                              :child (str role)]
                                                             [re-com/box
                                                              :style {:font-size "10px"
                                                                      :color (str (theme-pull :theme/editor-font-color nil) 78)}
                                                              :child (str timestamp)]])]
                                                [re-com/box
                                                 :width "570px"
                                                 :justify (if assistant? :start :end)
                                                 :align (if assistant? :start :end)
                                                 :style {:color (str (theme-pull :theme/editor-font-color nil) 97)}
                                                             ;:child (str content)
                                                 :child (parse-output content kp role)]]]))))]]))

(re-frame/reg-event-db
 ::runstream-item
 ;(undoable)
 (fn [db [_ result]]
   ;(assoc-in db [:data :kit-results-sys] (vec (filter #(not (= (get % :id) id)) (get-in db [:data :kit-results-sys]))))
   ;(tap> [:runstream-item result])
   (-> db
       (assoc-in [:runstreams-lookups (get result :flow-id) :open-inputs] (get result :open-inputs))
       (assoc-in [:runstreams-lookups (get result :flow-id) :blocks] (get result :blocks)))))

(re-frame/reg-event-db
 ::get-runstream-ports
 ;(undoable)
 (fn [db [_ flow-id]]
   ;;(tap> [::get-runstream-ports flow-id])
   (re-frame/dispatch [::wfx/request :default
                       {:message    {:kind :get-flow-open-ports
                                     :flow-id flow-id
                                     :flowmap flow-id
                                     :client-name (get db :client-name)}
                        :on-response [::runstream-item]
                        ;:on-timeout [::http/timeout-response]
                        :timeout    500000}])
   db))

(re-frame/reg-event-db
 ::toggle-runstream-item
 (undoable)
 (fn [db [_ flow-id]]
   (assoc-in db [:runstreams flow-id :open?]
             (not (get-in db [:runstreams flow-id :open?] false)))))

(re-frame/reg-event-db
 ::toggle-runstream-outputs
 (undoable)
 (fn [db [_ flow-id]]
   (assoc-in db [:runstreams flow-id :open-outputs?]
             (not (get-in db [:runstreams flow-id :open-outputs?] false)))))

(re-frame/reg-event-db
 ::toggle-runstream-value
 (undoable)
 (fn [db [_ flow-id kkey]]
   (assoc-in db [:runstreams flow-id :values kkey :open?]
             (not (get-in db [:runstreams flow-id :values kkey :open?] false)))))

(re-frame/reg-event-db
 ::toggle-runstream-fire
 (undoable)
 (fn [db [_ flow-id]]
   (assoc-in db [:runstreams flow-id :fire?]
             (not (get-in db [:runstreams flow-id :fire?] false)))))

(re-frame/reg-event-db
 ::toggle-runstream-drops
 (undoable)
 (fn [db [_ flow-id]]
   (assoc-in db [:runstreams flow-id :drops?]
             (not (get-in db [:runstreams flow-id :drops?] false)))))

(defn gn [x] (try (name x) (catch :default _ x)))
(defn gns [x] (try (namespace x) (catch :default _ x)))
(defn gns? [x] (not (nil? (try (namespace x) (catch :default _ false)))))

(defonce add-action-shelf? (reagent/atom {}))
(defonce add-flow-shelf? (reagent/atom false))
(defonce add-flow-shelf (reagent/atom nil))

(defn prepare-output-param-drag [k flow-id dtype]
  ;(tap> [:out-drag k flow-id dtype])
  (let [is-image?   false
        is-video?   false ;; maybe later re-enable, but would require me to sample the entire output, which in this case is bad news bears
        pwidth      5 ;(js/Math.floor (/ (count (str param-value)) 1.7))
        pwidth      (cond (> pwidth 30) 30
                          (< pwidth 6) 6
                          :else pwidth)
        pheight     (js/Math.floor (/ pwidth 30))
        pheight     (cond (> pheight 3) 3
                          (< pheight 1) 1
                          :else pheight)
        viewable?   (or (= dtype "map") (= dtype "vector"))
        kkp         (keyword (str "flow/" flow-id ">" k))]
    {:h         (cond is-image? 6 is-video? 9 :else (+ 2 pheight))
     :w         (cond is-image? 6 is-video? 13 :else pwidth)
     :root      [0 0]
     :drag-meta {:type        (if viewable?
                                :viewer-pull :param)
                 :param-full (if viewable?
                               [:box :style {} :child [:data-viewer kkp]]
                               kkp)
                 ;:param-type  dtype
                 :param-table flow-id
                 :param-field k}}))

(defn runstream-output-boxes [flow-id blocks-map panel-width open-outputs? open-inputs]
  (let [open-input-keys (vec (keys open-inputs))]
    ;(tap> [:open-inputs open-inputs])
    [re-com/v-box
     :width (px (- panel-width 35))
     :children
     [[re-com/h-box
       :height "30px"
       :align :center :justify :center
       :gap "5px"
       :children [[re-com/box
                   :attr {:on-click #(re-frame/dispatch [::toggle-runstream-outputs flow-id])}
                   :style {:cursor "pointer"}
                   :child (str (count (keys blocks-map)) " outputs")]
                  [re-com/md-icon-button :src (at)
                   :md-icon-name (if open-outputs? "zmdi-chevron-down" "zmdi-chevron-up")
                   :on-click #(re-frame/dispatch [::toggle-runstream-outputs flow-id])
                   :style {:color (str (theme-pull :theme/editor-outer-rim-color nil))
                           :cursor "pointer"
                           :padding "2px"
                           ;:height "22px"
                           ;:margin-top "-3px"
                           ;:margin-right "6px"
                           :font-size "22px"}]]
       :style {:font-size "14px" :opacity 0.6}]

      (when open-outputs?
        [re-com/v-box
         :children (for [chunk (partition-all 3 blocks-map)
                         :let [nums (count chunk)
                               ext  (cond (= nums 3) 28
                                          (= nums 2) 38
                                          :else 48)]]
                     [re-com/h-box
                      :gap "5px"
                      :padding "4px"
                      :children (for [[k {:keys [type last? sample]}] chunk
                                      :let [type (if (cstr/ends-with? (str k) "-vw") "rabbit-code" (gn type))
                                            view? (= type "rabbit-code")
                                            ccolor (get (theme-pull :theme/data-colors db/data-colors) type)
                                            is-input? (true? (some #(= % k) open-input-keys))
                                            ;_ (tap> [:inn k type sample])
                                            ]]
                                  (bricks/draggable
                                   (prepare-output-param-drag k flow-id type)
                                   "meta-menu"
                                   [re-com/v-box
                                    :height "77px"
                                    :style {:cursor "grab"
                                            :opacity (if is-input? 0.45 1)
                                            :border (cond last? (str "3px solid " ccolor)
                                                          is-input? (str "1px solid #00000000")
                                                          :else (str "1px solid " ccolor))
                                            ;:text-decoration (when last? "underline")
                                            :border-radius "10px"
                                            :background-color (str ccolor (if last? 40 22))}
                                    :gap "5px"
                                    :padding "4px"
                                    :size "auto"
                                    :children [[re-com/h-box
                                                :padding "4px"
                                                :size "none"
                                                :width (px (- (/ panel-width nums) ext))
                                                :justify :between
                                                :children [[re-com/box
                                                            :style {:font-size "12px"
                                                                    :font-weight (when last? 700)
                                                                    :text-decoration (when last? "underline")}
                                                            :child (if (> (count (str k)) 30)
                                                                     (try (str (subs (str k) 0 30) "...") (catch :default _ (str k)))
                                                                     (str k))]
                                                           (when (<= (count (str k)) 23)
                                                             [re-com/box
                                                              :style {:font-size "10px" :opacity 0.55}
                                                              :child (str (if (map? type) (get type :out) type))])]]
                                               (if view?
                                                 [re-com/box
                                                  :size "none" :padding "4px"
                                                  :height "35px" :align :center :justify :center
                                                  :style {:font-size "17px"
                                                          :overflow "hidden"
                                                          :opacity 0.55}
                                                  :width (px (- (/ panel-width nums) ext))
                                                  :child "(renderable view)"]
                                                 [re-com/box
                                                  :size "none" :padding "4px"
                                                  :height "35px"
                                                  :style {:font-size "10px"
                                                          :overflow "hidden"
                                                          :opacity 0.55}
                                                  :width (px (- (/ panel-width nums) ext))
                                                  :child (str sample)])]]))])])]]))

(re-frame/reg-sub
 ::shouts
 (fn [db [_ flow-id]]
   (get-in db [:shouts flow-id])))

(re-frame/reg-sub
 ::fire?
 (fn [db [_ flow-id]]
   (get-in db [:runstreams flow-id :fire?] false)))

(re-frame/reg-sub
 ::drops?
 (fn [db [_ flow-id]]
   (get-in db [:runstreams flow-id :drops?] false)))

(re-frame/reg-event-db
 ::set-shout
 (fn [db [_ flow-id shout]]
   (assoc-in db [:shouts flow-id] shout)))

(re-frame/reg-event-db
 ::remove-shout
 (fn [db [_ flow-id]]
   (ut/dissoc-in db [:shouts flow-id])))

(defonce shout-panel? (reagent/atom {}))

(re-frame/reg-sub
 ::get-meta
 (fn [db [_ flow-id bid]] ;; same as in flows.cljs, but with added flow-id param
   (get-in db [:flows flow-id :map bid :data :flow-item :meta])))

;(def add-action-shelf (reagent/atom {}))
(def add-action-shelf-name (reagent/atom nil))
(def add-action-shelf-inputs (reagent/atom nil))
(def add-action-shelf-outputs (reagent/atom nil))
(def add-action-shelf-type (reagent/atom nil))

(defn simple-typeahead [items kkey placeholder]
  [re-com/typeahead
   :width "200px"
   :suggestion-to-string (fn [item]
                           (str (get item :label)))
    ;; render in input box (must be string, no hiccup :())
   :render-suggestion (fn [ss _] ;; render in dropdown
                        [re-com/box
                         :style {:color "#000000"
                                 ;:z-index 1999
                                 }
                         :child (str (get ss :label))])
   ;:on-change #(swap! add-action-shelf assoc kkey (get % :id))
   :on-change #(if (= kkey :inputs)
                 (reset! add-action-shelf-inputs (get % :id))
                 (reset! add-action-shelf-outputs  (get % :id)))
   :rigid? true
   :placeholder (str placeholder)
   :style {:z-index 995
           :background-color "#00000022"
           :padding "4px"
           :font-size "13px"
           :border (str "1px solid " (str (theme-pull :theme/editor-outer-rim-color nil) 44))
           :padding-left "0px"
           :color (theme-pull :theme/editor-outer-rim-color nil)}
   :data-source (fn [x]
                  (let [flow-parts (vec (map (fn [n] {:id n :label n}) items))
                        words (cstr/split (cstr/lower-case (cstr/trim x)) #" ")
                        matches-word (fn [field word] (cstr/includes? (cstr/lower-case (str field)) word))]
                    (if (or (nil? x) (empty? x)) flow-parts
                        (filter (fn [item]
                                  (let [label (get item :label)]
                                    (every? (fn [word]
                                              (matches-word label word))
                                            words)))
                                flow-parts))))])

(re-frame/reg-sub
 ::drops
 (fn [db [_ flow-id]]
   (get-in db [:runstream-drops flow-id] {})))

(re-frame/reg-event-db
 ::add-action
 (undoable)
 (fn [db [_ flow-id name in out type]]
   (if (and (not (empty? (cstr/trim (str name))))
            (not (empty? (cstr/trim (str in))))
            (not (empty? (cstr/trim (str out)))))
     (let [other-names @(re-frame/subscribe [::bricks/all-drops-of :*])
           name (keyword name)
           name (ut/safe-key name other-names)
           _ (tap> [:other-names other-names name])]
       (assoc-in db [:runstream-drops flow-id name] {:in in :out out :type (or type (set [:string]))}))
     db)))

(re-frame/reg-event-db
 ::remove-action
 (undoable)
 (fn [db [_ flow-id name]]
   (ut/dissoc-in db [:runstream-drops flow-id name])))

(defn runstream-box [panel-height panel-width kp]
  (let [text-box-height 110
        text-box? false
        runstreams @(re-frame/subscribe [::bricks/runstreams])
        runstream-keys (vec (map :flow-id runstreams))
        runstreams-lookups @(re-frame/subscribe [::bricks/runstreams-lookups])
       ; rs-hash @(re-frame/subscribe [::runstream-override-map])
        redrops! [@bricks/dragging-body @bricks/dragging?]]

    ;; (tap> [:rs-hash rs-hash])

    ;(reagent.core/next-tick #(scroll-to-bottom "chat-v-box"))

    [re-com/box
     :padding "5px"
     :size "none"
     :height (px (- panel-height 12 25 (when text-box? text-box-height))) ;; minus size of border top and bottom
     :width (px (- panel-width 12)) ;; minus size of border left and right and header
     ;:attr {:id "chat-v-box"}
     :attr {:on-drag-over  #(when (and (not @bricks/over-block?)
                                       (not @bricks/over-flow?))
                              (do (reset! bricks/over-flow? true)
                                  (reset! bricks/over-block? true)))
            :on-drag-leave #(do (reset! bricks/over-block? false)
                                (reset! bricks/over-flow? false))}
     :style {:overflow "auto"
             :border-radius "16px"}
     :child [re-com/v-box
             :gap "9px"
               ;:width "575px"
             :style {:font-size "14px"}
             :children (conj (vec
                              (for [{:keys [flow-id open? open-outputs? values]} runstreams]

                                (let [open-inputs (get-in runstreams-lookups [flow-id :open-inputs])
                                      blocks-map (get-in runstreams-lookups [flow-id :blocks])
                                      ;; _ (tap> [:blocks-map blocks-map])
                                      no-open-inputs? (true? (empty? open-inputs))
                                      shouts @(re-frame/subscribe [::shouts flow-id])
                                      shouts? (not (empty? (cstr/trim (str shouts))))
                                      fire?  @(re-frame/subscribe [::fire? flow-id])
                                      drops?  @(re-frame/subscribe [::drops? flow-id])
                                      sshout-panel? (get @shout-panel? flow-id false)
                                      [xx yy] @detached-coords
                                        ;_ (tap> [:open-inputs flow-id open-inputs])
                                      override-map (into {} (for [[k {:keys [value source]}] values
                                                                  :when (not (nil? value))]
                                                              {k (if (= source :input)
                                                                   value
                                                              ;value
                                                                   (let [vv @(re-frame/subscribe [::rs-value flow-id k])]
                                                                     (if (and (vector? vv) (every? string? vv))
                                                                       (cstr/join "\n" vv) vv)))}))
                                      overrides-map? (not (empty? override-map))
                                      override-map (if (empty? override-map) nil override-map)
                                      no-data? (not open-inputs)
                                      ;; drops
                                      input-ports (vec (keys open-inputs))
                                      output-ports (vec (cset/difference (set (keys blocks-map)) (set input-ports)))
                                      drops @(re-frame/subscribe [::drops flow-id])]

                             ;(tap> [:override-map flow-id override-map])

                                  (when no-data? ;; fetch updated port info from server
                                    (re-frame/dispatch [::get-runstream-ports flow-id]))

                                  [re-com/v-box
                                   :padding "5px"
                                   :style {:border (when false ;open?
                                                     (str "2px solid " (str (theme-pull :theme/editor-outer-rim-color nil) 44)))
                                           :border-radius "11px"
                                           :background-color (str (theme-pull :theme/editor-outer-rim-color nil) 22)}
                                   :children [[re-com/h-box
                                               :height "45px"
                                          ;:padding "9px"
                                               :style {:font-size "20px" :padding-bottom "4px"}
                                               :justify :between :align :center
                                               :children [[re-com/h-box
                                                           :children [[re-com/md-icon-button :src (at)
                                                                       :md-icon-name "zmdi-close"
                                                                       :on-click #(re-frame/dispatch [::bricks/remove-runstream flow-id])
                                                                       :style {:color (str (theme-pull :theme/editor-outer-rim-color nil))
                                                                               :cursor "pointer"
                                                                               :height "15px"
                                                                               :margin-top "3px"
                                                                               :margin-right "6px"
                                                                               :font-size "19px"}]

                                                                      [re-com/box
                                                                       :style {:cursor "pointer"
                                                                               :user-select "none"}
                                                                       :attr {:on-click #(re-frame/dispatch [::toggle-runstream-item flow-id])}
                                                                       :child (str flow-id)]

                                                                      (bricks/draggable
                                                                       (let [s-key (str "runstream-chart-" flow-id)
                                                                             q-key (ut/safe-key (keyword s-key))]
                                                                         (bricks/sql-spawner-chat
                                                                          nil {:views
                                                                               {:reech [:> :ResponsiveContainer
                                                                                        {:width "100%" :height :panel-height+50}
                                                                                        [:> :BarChart
                                                                                         {:data q-key
                                                                                          :margin {:top 5 :bottom 5 :right 30 :left 20}}
                                                                                         [:> :CartesianGrid
                                                                                          {:strokeDasharray "1 4" :opacity 0.33}]
                                                                                         [:> :Tooltip] [:> :XAxis {:dataKey :started}]
                                                                                         [:> :Bar
                                                                                          {:dataKey :elapsed
                                                                                           :stroke :theme/editor-outer-rim-color
                                                                                           :fill :theme/editor-outer-rim-color}]]]}
                                                                               :queries {q-key
                                                                                         {;:select [[[[:sum :elapsed_seconds]] :elapsed] :ts]
                                                                                          :select [[[[:min :elapsed]] :elapsed] :started]
                                                                                          :refresh-every 10
                                                                                          :cache? false
                                                                                          :connection-id "flows-db"
                                                                                          :from
                                                                                          [{:select [:client_name :elapsed :ended :flow_id
                                                                                                     :human_elapsed :in_error :started :ts]
                                                                                            :from [[:flow_history :mm134]]
                                                                                            :where [:= :flow_id (str flow-id)]}]
                                                                                          :group-by [:started]}}} s-key 5 7))
                                                                       "meta-menu"
                                                                       [re-com/md-icon-button :src (at)
                                                                        :md-icon-name "zmdi-chart"
                                                                        :style {:cursor "grab"
                                                                                :color (str (theme-pull :theme/editor-outer-rim-color nil))
                                                                                :height "15px"
                                                                                :margin-left "6px"
                                                                                :margin-top "2px"
                                                                                :font-size "19px"}])

                                                                      [re-com/md-icon-button :src (at)
                                                                       :md-icon-name "zmdi-hearing"
                                                                       :on-click #(swap! shout-panel? assoc flow-id (not sshout-panel?))
                                                                       :style {:cursor "pointer"
                                                                               :color (str (theme-pull :theme/editor-outer-rim-color nil))
                                                                               :height "15px"
                                                                               :transform (when sshout-panel? "scaleX(-1)")
                                                                               :opacity (if shouts? 1.0 0.33)
                                                                               :margin-left "6px"
                                                                               :margin-top "2px"
                                                                               :font-size "19px"}]

                                                                      (when shouts? [re-com/box
                                                                                     :height "28px" :align :center :justify :start
                                                                                     :style {:font-size "11px"
                                                                                             :margin-left "6px"
                                                                                             :color (str (theme-pull :theme/editor-outer-rim-color nil) 65)}
                                                                                     :child (str "\"" shouts "...\"")])

                                                                      [re-com/md-icon-button :src (at)
                                                                       :md-icon-name "zmdi-fire"
                                                                       :on-click #(re-frame/dispatch [::toggle-runstream-fire flow-id])
                                                                       :style {:cursor "pointer"
                                                                               :color (if fire?
                                                                                        (ut/invert-hex-color (str (theme-pull :theme/editor-outer-rim-color nil)))
                                                                                        (str (theme-pull :theme/editor-outer-rim-color nil)))
                                                                               :height "15px"
                                                                               ;:transform (when fire? "scaleX(-1)")
                                                                               :opacity (if fire? 1.0 0.33)
                                                                               :margin-left "6px"
                                                                               :margin-top "2px"
                                                                               :font-size "19px"}]

                                                                      [re-com/h-box
                                                                       :children
                                                                       [[re-com/md-icon-button :src (at)
                                                                         :md-icon-name "fa-solid fa-droplet"
                                                                         :on-click #(re-frame/dispatch [::toggle-runstream-drops flow-id])
                                                                         :style {:cursor "pointer"
                                                                                 :color (str (theme-pull :theme/editor-outer-rim-color nil))
                                                                                 :height "15px"
                                                                                 :transform (when drops? "rotate(-45deg)")
                                                                               ;:opacity (if drops? 1.0 0.33)
                                                                                 :opacity 0.33
                                                                                 :margin-left (if drops? "5px" "6px")
                                                                                 :margin-top "2px"
                                                                                 :font-size "19px"}]
                                                                        [re-com/box
                                                                         :style {:color (str (theme-pull :theme/editor-outer-rim-color nil))
                                                                                 :opacity 0.33
                                                                                 :font-size "9px"
                                                                                 :height "15px"}
                                                                         :child (str (let [cc (count (keys drops))]
                                                                                       (if (= cc 0) "" cc)))]]]

                                                                      (when no-open-inputs?
                                                                        [re-com/box
                                                                         :align :center
                                                                         :style {:margin-left "10px"
                                                                                 :opacity 0.45
                                                                                 :font-weight 500
                                                                                 :font-size "13px"}
                                                                         :child "(no open inputs found)"])]]

                                                          [re-com/h-box
                                                           :children
                                                           [(when overrides-map?
                                                        ;;  [re-com/box
                                                        ;;   :style {:margin-top "11px" :margin-right "-10px"}
                                                        ;;   :child "*"]
                                                              [re-com/md-icon-button :src (at)
                                                               :md-icon-name "zmdi-transform"
                                                               :style {:color (str (theme-pull :theme/editor-outer-rim-color nil))
                                                                       :cursor "pointer"
                                                                       :height "15px"
                                                                       :margin-top "11px" :margin-right "-17px"
                                                                       :font-size "19px"}])

                                                            (bricks/draggable
                                                             {:h 2
                                                              :w 7
                                                              :drag-meta
                                                              {:source-table :hi,
                                                               :table-fields [:*],
                                                               :connection-id nil,
                                                               :source-panel-key :block-7034,
                                                               :type :view},
                                                              :views
                                                              {:flow-play-runstreamed
                                                               [:box
                                                                :align
                                                                :center
                                                                :justify
                                                                :center
                                                                :style
                                                                {:font-size "25px",
                                                                 :font-weight 700,
                                                                 :padding-top "6px",
                                                                 :padding-left "14px",
                                                                 :margin-top "-8px",
                                                                 :color :theme/editor-outer-rim-color,
                                                                 :font-family :theme/base-font}
                                                                :child [:run-flow [(str flow-id) (str "run " flow-id) :runstream-overrides]]]},
                                                              :name "flow-play-from-runstreams"}
                                                             "meta-menu"
                                                             [render-honey-comb-fragments
                                                              [:box
                                                               :justify :center
                                                               :size "none" :width "35px" :height "45px"
                                                               :style {:font-size    "25px"
                                                                       :padding-top  "4px"
                                                                       :margin-left "13px"}
                                                               :child [:run-flow [flow-id "" override-map]]] 1 1]
                                                               ;flow-id
                                                             )]]]]

                                              (when fire? ;(and fire? (not sshout-panel?))
                                                [re-com/v-box
                                                 :padding "4px"
                                                 :height "30px"
                                                 :width (px (- panel-width 38)) :size "none" ;:height "50px"
                                                 :align :center :justify :center
                                                 :children [[re-com/box
                                                             :style {:color (theme-pull :theme/editor-outer-rim-color nil)
                                                                     :font-size "10px"}
                                                             :child "(*flow will be triggered when any of it's inputs are changed)"]]])

                                              (when sshout-panel?
                                                [re-com/v-box
                                                 :padding "4px"
                                                 :width (px (- panel-width 38)) :size "none" ;:height "50px"
                                                 :style {:border (str "1px solid " (str (theme-pull :theme/editor-outer-rim-color nil) 44))
                                                         :border-radius "11px"
                                                         :padding-bottom "10px"
                                                         :background-color "#00000025"}
                                                 :align :center :justify :center
                                                 :children [;; [re-com/box
                                                            ;;  :padding "9px"
                                                            ;;  :style {:color (theme-pull :theme/editor-outer-rim-color nil)}
                                                            ;;  :child "flow will be triggered when it 'hears' a phrase starting with..."]

                                                            [re-com/h-box
                                                             :gap "10px"
                                                             :height "40px"
                                                             :align :center :justify :center
                                                             :style {:font-size "16px"
                                                                     :color (str (theme-pull :theme/editor-outer-rim-color nil))}
                                                             :children [[re-com/box :child "shouts"]
                                                                        [re-com/box
                                                                         :style {:font-size "10px"}
                                                                         :child "(flow will be triggered when it 'hears' a phrase starting with...)"]]]

                                                            [re-com/input-text
                                                             :model (str shouts)
                                                             :on-change #(re-frame/dispatch [::set-shout flow-id (str %)])
                                                             :change-on-blur? true
                                                             :width (px (- panel-width 70))
                                                             :style {:background-color "#00000022"
                                                                     ;:text-decoration "underline"
                                                                     ;:padding "4px"
                                                                     ;:font-weight 700
                                                                     :font-size "16px"
                                                                     :border (str "1px solid " (str (theme-pull :theme/editor-outer-rim-color nil) 44))
                                                                     ;:padding-left "0px"
                                                                     :color "#ffffff88"
                                                                     ;:color (theme-pull :theme/editor-outer-rim-color nil)
                                                                     }]]
                                                 ;:style {:border "1px solid white"}
                                                 ])

                                              (when drops?
                                                (let [add-row (fn [label placeholder kkey choices]
                                                                [re-com/h-box
                                                                  ;:width "50%"
                                                                 :align :start
                                                                 :justify :between
                                                                 :height "40px"
                                                                  ;:style {:border "1px solid white"}
                                                                 :children [[re-com/box
                                                                             :width "80px"
                                                                             :padding "8px" :align :center
                                                                             :style {:font-size "10px"
                                                                                     ;:margin-top (if (= kkey :inputs) "3px" "-4px")
                                                                                     }
                                                                             :child label]
                                                                            [simple-typeahead choices kkey placeholder]
                                                                            [re-com/box
                                                                             :padding "8px" :align :center :justify :end
                                                                             :width "260px"
                                                                             :style {:font-size "10px"
                                                                                     :opacity 0.4
                                                                                     :margin-top "3px"}
                                                                             :child (if (= kkey :inputs)
                                                                                      "stuf stuf suts inny"
                                                                                      "stuf stuf suts outtie")]]])
                                                      t-dropdown (fn [m]
                                                                   ;(tap> [:m m])
                                                                   [re-com/tag-dropdown
                                                                    :model (if m m (set (or @add-action-shelf-type [:string])))
                                                                    :parts {:main {:style {:background-color "#00000000" ;(str (theme-pull :theme/editor-rim-color nil) "11")
                                                                                           :color (theme-pull :theme/editor-font-color nil)
                                                                                           :border (str "0px solid " (theme-pull :theme/editor-outer-rim-color nil))
                                                                                           :outline "none"}}
                                                                            :list-group-item {:style {:background-color (str (theme-pull :theme/editor-rim-color nil) "99")
                                                                                                      :backdrop-filter "blur(4px)"
                                                                                                      :border (str "1px solid " (theme-pull :theme/editor-outer-rim-color nil) "44")
                                                                                                      :color (theme-pull :theme/editor-font-color nil)}} ;; popover
                                                                            :list-group {:style {:margin-top (px (+ (* -1 yy) -45))
                                                                                                 :margin-left (px (+ (* -1 xx) -20))
                                                                                                 :background-color "#00000000"}}}
                                                                    :abbrev-fn (fn [m] [re-com/box
                                                                                        :child (subs (str (get m :label)) 1 2)
                                                                                        :style {:font-weight 700
                                                                                                :color (get m :color)}])
                                                                    :required? true
                                                                            ;:max-width "130px"
                                                                    :disabled? (not (nil? m))
                                                                    :label-fn (fn [m] [re-com/box
                                                                                       :child (str (get m :label))
                                                                                       :style {:font-weight 700
                                                                                               :color (get m :color)}])
                                                                    :style {:font-size   "12px"
                                                                            :font-family (theme-pull :theme/base-font nil)
                                                                            :color       (theme-pull :theme/editor-font-color nil)}
                                                                    :abbrev-threshold 15
                                                                    :choices (vec (for [[k v] (ut/sort-map-by-key
                                                                                               (merge {"any" "#c32148"
                                                                                                       "*table" "#aeaeae"
                                                                                                       "*view" "#aeaeae"
                                                                                                       "#field" "#aeaeae"}
                                                                                                      (theme-pull :theme/data-colors db/data-colors)))]
                                                                                    {:id (keyword k)
                                                                                     :label (str (keyword k))
                                                                                     ;:color (ut/invert-hex-color v)
                                                                                     :color (ut/choose-text-color v)
                                                                                     :background-color v}))
                                                                    :on-change #(reset! add-action-shelf-type %)])]
                                                  [re-com/v-box
                                                   :padding "9px"
                                                   :width (px (- panel-width 38))
                                                   :size "none" ;:height "50px"
                                                   :style {:border (str "1px solid " (str (theme-pull :theme/editor-outer-rim-color nil) 44))
                                                           :border-radius "11px"
                                                           :background-color "#00000025"
                                                           :margin-top "11px"
                                                           :margin-bottom "11px"
                                                           ;:margin-left "5px"
                                                           }
                                                   :align :center :justify :center
                                                   :children [[re-com/h-box
                                                               :style {:color (str (theme-pull :theme/editor-outer-rim-color nil) 75)}
                                                               ;:height "40px"
                                                               :align :center :justify :center
                                                               :gap "14px"
                                                               :children [[re-com/md-icon-button :src (at)
                                                                           :md-icon-name "zmdi-plus"
                                                                           :style {:color (str (theme-pull :theme/editor-outer-rim-color nil))
                                                                                   :cursor "pointer"
                                                                                   :font-size "26px"}
                                                                           :on-click #(swap! add-action-shelf? assoc flow-id (not (get @add-action-shelf? flow-id)))]
                                                                          [re-com/h-box
                                                                           :height "40px"
                                                                           :gap "10px"
                                                                           :align :center :justify :center
                                                                           :style {:font-size "16px"
                                                                                   :color (str (theme-pull :theme/editor-outer-rim-color nil))}
                                                                           :children [[re-com/box :child "drops"]
                                                                                      [re-com/box
                                                                                       :style {:font-size "10px"}
                                                                                       :child "(functions defined as values 'dropped' on to a flow)"]]]]]

                                                              (when (get @add-action-shelf? flow-id)
                                                                [re-com/v-box
                                                                 :children [[re-com/v-box
                                                                             :size "auto" :width "100%"
                                                                             :justify :between :align :center

                                                                             :padding "8px"
                                                                             :children
                                                                             [[add-row "'drop' into:"  " (search for input point)"  :inputs  input-ports]
                                                                              [add-row "pick up from:" " (search for output point)" :outputs output-ports]]]

                                                                            [re-com/h-box
                                                                             :align :center :justify :between
                                                                             :width "100%"
                                                                             :padding "8px"
                                                                             :height "40px"
                                                                             :style {:font-size "10px"
                                                                       ;:border "1px solid pink"
                                                                                     }
                                                                             :children [;[re-com/box :child "some more stuff down here"]

                                                                                        [re-com/input-text
                                                                                         :model add-action-shelf-name ;"tet" ; (get @add-action-shelf :inputs)
                                                                                         :on-change #(reset! add-action-shelf-name %)
                                                                                         :placeholder " (give it a short name)"
                                                                                         :validation-regex #"^[a-zA-Z0-9_-]+$" ;; alpha, underscores, hypens, numbers
                                                                                         :width "272px"
                                                                                         :style {:background-color "#00000022"
                                                                                                 :z-index 888
                                                                                   ;:text-decoration "underline"
                                                                                                 :padding "4px"
                                                                                   ;:font-weight 700
                                                                                                 :font-size "13px"
                                                                                                 :border (str "1px solid " (str (theme-pull :theme/editor-outer-rim-color nil) 44))
                                                                                                 :padding-left "0px"
                                                                                                 :color (theme-pull :theme/editor-outer-rim-color nil)}]

                                                                                        [t-dropdown]

                                                                                        (let [valid?  true
                                                                                ;;  (and (not (nil? @add-action-shelf-name))
                                                                                ;;              (not (nil? @add-action-shelf-inputs))
                                                                                ;;              (not (nil? @add-action-shelf-outputs))
                                                                                ;;              (not (nil? @add-action-shelf-type)))
                                                                                              ]
                                                                                          [re-com/box
                                                                                           :style {:border (str "1px solid " (str (theme-pull :theme/editor-outer-rim-color nil) 89))
                                                                                                   :background-color (str (theme-pull :theme/editor-outer-rim-color nil) 45)
                                                                                                   :cursor "pointer"
                                                                                                   :border-radius "8px"
                                                                                                   :padding-top "5px"
                                                                                                   :padding-bottom "5px"
                                                                                                   :padding-right "14px"
                                                                                                   ;:opacity (if valid? 1.0 0.33)
                                                                                                   ;:text-decoration (if valid? "none" "line-through")
                                                                                                   :padding-left "14px"}
                                                                                           :align :center :justify :center
                                                                                           :attr {:on-click #(do
                                                                                                               (re-frame/dispatch [::add-action
                                                                                                                                   flow-id @add-action-shelf-name
                                                                                                                                   @add-action-shelf-inputs
                                                                                                                                   @add-action-shelf-outputs
                                                                                                                                   @add-action-shelf-type])
                                                                                                               (reset! add-action-shelf-name nil)
                                                                                                               (reset! add-action-shelf-inputs nil)
                                                                                                               (reset! add-action-shelf-outputs nil)
                                                                                                               (reset! add-action-shelf-type nil)
                                                                                                               (swap! add-action-shelf? assoc flow-id false))}
                                                                                           :child "create drop"])]]]])

                                                              [re-com/gap :size "10px"]

                                                              [re-com/v-box
                                                               :width (px (- panel-width 60))
                                                               :gap "5px"
                                                               :children (for [[a {:keys [in out type]}] drops]
                                                                           [re-com/v-box
                                                                            :padding "8px"
                                                                            :gap "12px"
                                                                            :style {:border (str "1px solid " (theme-pull :theme/editor-outer-rim-color nil) 66)
                                                                                    :border-radius "8px"
                                                                                    :background-color (str (theme-pull :theme/editor-outer-rim-color nil) 22)}
                                                                            :children
                                                                            [[re-com/h-box

                                                                              :justify :between :align :center
                                                                              :children [[re-com/box :child (str a)]

                                                                                         [re-com/h-box
                                                                                          :gap "12px"
                                                                                          :children [;[re-com/box :child (str type)]

                                                                                                                   ;[re-com/box :child (str "-")]
                                                                                                     [re-com/md-icon-button :src (at)
                                                                                                      :md-icon-name "zmdi-close"
                                                                                                      :on-click #(re-frame/dispatch [::remove-action flow-id a])
                                                                                                      :style {:color (str (theme-pull :theme/editor-outer-rim-color nil))
                                                                                                              :cursor "pointer"
                                                                                                              :height "15px"
                                                                                                              :font-size "19px"}]]]]]

                                                                             (let [pill-box (fn [x]
                                                                                              (let [ttype (get-in blocks-map [x :type] :?)
                                                                                                    kvord? (fn [x] (try (or (vector? x) (keyword? x)) (catch :default _ false)))
                                                                                                    ttype (cond (kvord? ttype) ttype
                                                                                                                (kvord? (get ttype :out)) (get ttype :out)
                                                                                                                (kvord? (get ttype :*)) (get ttype :*)
                                                                                                                :else (or (first (vals (keys ttype))) :?))
                                                                                                    ccolor (get (theme-pull :theme/data-colors db/data-colors) (gn ttype))]
                                                                                                [re-com/h-box
                                                                                                 :justify :between
                                                                                                 :padding "6px" :width "150px" ;;:justify :center
                                                                                                 :style {:border (str "1px solid " ccolor)
                                                                                                         :font-size "10px"
                                                                                                         :background-color (str ccolor 22)
                                                                                                         :border-radius "5px"
                                                                                                         :overflow "hidden"}
                                                                                                 :children [[re-com/box
                                                                                                             :style {:font-weight 700}
                                                                                                             :child (str x)]
                                                                                                            [re-com/box
                                                                                                             :style {:color ccolor}
                                                                                                             :child (str ttype)]]]))]
                                                                               [re-com/h-box
                                                                                :width "100%"
                                                                                :justify :between
                                                                              ;:gap "10px"
                                                                                :align :center
                                                                                :children [[t-dropdown type]

                                                                                           [re-com/md-icon-button :src (at)
                                                                                            :md-icon-name "fa-solid fa-right-to-bracket"
                                                                                            :style {:color (str (theme-pull :theme/editor-outer-rim-color nil))
                                                                                                  ;:cursor "pointer"
                                                                                                    :opacity 0.33
                                                                                                  ;:height "15px"
                                                                                                    :font-size "24px"}]

                                                                                           [pill-box in]

                                                                                           [re-com/md-icon-button :src (at)
                                                                                            :md-icon-name "fa-solid fa-timeline"
                                                                                            :style {:color (str (theme-pull :theme/editor-outer-rim-color nil))
                                                                                                  ;:cursor "pointer"
                                                                                                    :opacity 0.33
                                                                                                  ;:height "15px"
                                                                                                    :font-size "24px"}]

                                                                                           [pill-box out]

                                                                                           [re-com/md-icon-button :src (at)
                                                                                            :md-icon-name "fa-solid fa-right-from-bracket"
                                                                                            :style {:color (str (theme-pull :theme/editor-outer-rim-color nil))
                                                                                                  ;:cursor "pointer"
                                                                                                    :opacity 0.33
                                                                                                  ;:height "15px"
                                                                                                    :font-size "24px"}]]])]])]]
                                                 ;:style {:border "1px solid white"}
                                                   ]))

                                              (when open?

                                                [re-com/v-box
                                                 :children (let [parted (partition-all 2 (vec (for [[k v] open-inputs] [k v])))]
                                                             (for [batch parted]
                                                               (let [is-first? (= batch (first parted))
                                                                     is-last? (= batch (last parted))
                                                                     is-both? (and is-first? is-last?)
                                                                     ;;_ (tap> [:batch batch])
                                                                     b1 (first batch)
                                                                     b2 (last batch)
                                                                     b1-meta (get-in blocks-map [(first b1) :meta :* :scrubber])
                                                                     b2-meta (get-in blocks-map [(first b2) :meta :* :scrubber])
                                                                     ;;single-scrubber? (or (= ttype :open-block) (= ttype :open-input)) ;; implied by 'open-blocks'?
                                                                     ;_ (when b1-meta (tap> [:b1-meta b1-meta b1]))
                                                                     ;_ (when b2-meta (tap> [:b2-meta b2-meta b1]))
                                                                     fix-type (fn [x] (get x :out x)) ;; TODO remove when input fixed upstream
                                                                     ttype1 (fix-type (get (last b1) :type))
                                                                     ttype2 (fix-type (get (last b2) :type))
                                                                     ;;_ (tap> [:inn b1 b2 ttype1 ttype2 ])
                                                                     droppable? (fn [x] ;(tap> [:gg (gn x) (get-in @bricks/dragging-body [:drag-meta :param-type])])
                                                                                  (try (or
                                                                                        (and @bricks/dragging? ;; all parameters that are valid
                                                                                             (or (= (gn x) (get-in @bricks/dragging-body [:drag-meta :param-type]))
                                                                                                 (= x :any))
                                                                                             (= :param (get-in @bricks/dragging-body [:drag-meta :type])))
                                                                                        (and @bricks/dragging? ;; all cells that are valid
                                                                                             (or (= (gn x) (get-in @bricks/dragging-body [:drag-meta :data-type]))
                                                                                                 (= x :any))
                                                                                             (>= (get-in @bricks/dragging-body [:drag-meta :row-num]) 0)
                                                                                             (= :where (get-in @bricks/dragging-body [:drag-meta :type]))))
                                                                                       (catch :default _ false)))
                                                                     b1-droppable? (droppable? ttype1)
                                                                     b2-droppable? (droppable? ttype2)
                                                                ;_ (tap> [:tgt @bricks/dragging? b1-droppable? b2-droppable? batch ])
                                                                     outcolor1 (try
                                                                                 (get (theme-pull :theme/data-colors db/data-colors)
                                                                                      (gn ttype1))
                                                                                 (catch :default _ "orange"))
                                                                     outcolor2 (try
                                                                                 (get (theme-pull :theme/data-colors db/data-colors)
                                                                                      (gn ttype2))
                                                                                 (catch :default _ "orange"))
                                                                     cell (fn [cc bbb meta drop?]
                                                                            (let [kkey (first bbb)
                                                                             ;drop? true
                                                                                  override @(re-frame/subscribe [::rs-value flow-id kkey])
                                                                                  defaults (get-in (last bbb) [:defaults kkey])
                                                                                  ttype (get (last bbb) :type)
                                                                                  ttype (get (last bbb) :ttype)
                                                                                  ;; meta? (or (map? meta) (vector? meta))
                                                                                  ;;_ (tap> [:bbb bbb meta])
                                                                                  user-input-val (get (last bbb) :user-input defaults)
                                                                                  overridden? (and (not (nil? override)) (not= user-input-val override))
                                                                                  curr-val (if overridden? override user-input-val)
                                                                            ;;  curr-val (try
                                                                            ;;             (if (and (vector? curr-val) (every? string? curr-val))
                                                                            ;;               (cstr/join "\n" curr-val) curr-val)
                                                                            ;;             (catch :default _ curr-val))
                                                                                  trunc 39
                                                                                  param? (= (get-in values [kkey :source]) :param)
                                                                                  param-value (get-in values [kkey :value])
                                                                                  val-open? (and (get-in values [kkey :open?] false) (not drop?))
                                                                                  subbed-val (try (str (subs (str curr-val) 0 trunc)
                                                                                                       (when (> (count (str curr-val)) trunc) "..."))
                                                                                                  (catch :default _ (str curr-val)))
                                                                                  param-pill [re-com/box
                                                                                              :padding "7px"
                                                                                              :min-height "42px"
                                                                                              :width (px (- (/ panel-width 2) 30))
                                                                                              :align :center :justify :center
                                                                                              :attr {:on-click #(re-frame/dispatch [::toggle-runstream-value flow-id kkey])}
                                                                                              :style {:opacity 0.8
                                                                                                      :overflow "hidden"
                                                                                                      :cursor "pointer"
                                                                                                      :font-size "16px"}
                                                                                              :child [re-com/box
                                                                                                      :style {:border (str "1px solid " cc)
                                                                                                              :background-color (str cc 55)
                                                                                                                ;:color cc
                                                                                                              :padding-top "3px"
                                                                                                              :padding-bottom "3px"
                                                                                                              :padding-left "12px"
                                                                                                              :padding-right "12px"}
                                                                                                      :child (str param-value)]]
                                                                             ;_ (tap> [:curr-val curr-val])
                                                                                  the-box [re-com/box
                                                                                           :height "100%"
                                                                                           :style {:border-radius (cond is-both? "11px"
                                                                                                                        is-first? "11px 11px 0px 0px"
                                                                                                                        is-last? "0px 0px 11px 11px"
                                                                                                                        :else "0px")
                                                                                                   :filter (when drop? "brightness(300%)")
                                                                                                   :border (if drop?
                                                                                                             (str "1px solid " cc)
                                                                                                             (str "1px solid #00000045"))
                                                                                                   :background-color (str cc (if drop? 55 20))}
                                                                                           :width (px (- (/ panel-width 2) 18))
                                                                                           :child [re-com/v-box
                                                                                                   :children
                                                                                                   [[re-com/h-box
                                                                                                     :size "none"
                                                                                                     :width (px (- (/ panel-width 2) 30))
                                                                                                     :style {:margin-left "5px" :cursor "pointer"}
                                                                                                     :padding "6px"
                                                                                                     :justify :between
                                                                                                     :children [[re-com/h-box
                                                                                                                 :children [(bricks/draggable
                                                                                                                             (prepare-output-param-drag (first bbb) flow-id ttype)
                                                                                                                             "meta-menu" [re-com/box
                                                                                                                                          :attr {:on-click #(re-frame/dispatch [::toggle-runstream-value flow-id kkey])}
                                                                                                                                          :style {:font-size "17px"}
                                                                                                                                          :child (str (first bbb))])

                                                                                                                            (when overridden?
                                                                                                                              [re-com/md-icon-button :src (at)
                                                                                                                               :md-icon-name "zmdi-undo"
                                                                                                                               :on-click #(re-frame/dispatch [::save-rs-value flow-id kkey nil nil])
                                                                                                                               :style {:cursor "pointer"
                                                                                                                                       :color (str (theme-pull :theme/editor-outer-rim-color nil))
                                                                                                                                       :height "15px"
                                                                                                                                       :margin-left "4px"
                                                                                                                                       :font-size "19px"}])
                                                                                                                            (when (and meta (not val-open?))
                                                                                                                              [re-com/md-icon-button :src (at)
                                                                                                                               :md-icon-name "zmdi-more"
                                                                                                                               :on-click #(re-frame/dispatch [::toggle-runstream-value flow-id kkey])
                                                                                                                               ;:on-click #(re-frame/dispatch [::save-rs-value flow-id kkey nil nil])
                                                                                                                               :style {;:cursor "pointer"
                                                                                                                                       :color (str (theme-pull :theme/editor-outer-rim-color nil))
                                                                                                                                       :height "15px"
                                                                                                                                       :margin-left "4px"
                                                                                                                                       :font-size "19px"}])]]
                                                                                                                [re-com/box
                                                                                                                 :style {:color cc :opacity 0.9}
                                                                                                                 :child (str ttype)]]]

                                                                                                    (when (not val-open?)
                                                                                                      (if drop?

                                                                                                        [re-com/box
                                                                                                         :padding "7px"
                                                                                                         :height "42px"
                                                                                                         :align :center :justify :center
                                                                                                         :style {:cursor "pointer"
                                                                                                                 :font-size "16px"}
                                                                                                         :child (str "drop parameter here to override")]

                                                                                                        (if param?
                                                                                                          param-pill
                                                                                                          [re-com/box
                                                                                                           :padding "7px"
                                                                                                           :height "42px"
                                                                                                           :size "none"
                                                                                                           :width (px (- (/ panel-width 2) 30))
                                                                                                           :attr {:on-click #(re-frame/dispatch [::toggle-runstream-value flow-id kkey])}
                                                                                                           :style {:opacity 0.35
                                                                                                                   :margin-left "7px"
                                                                                                                   :cursor "pointer"
                                                                                                                   :overflow "hidden"
                                                                                                                   :font-size "14px"}
                                                                                                           :child (str subbed-val)])))

                                                                                                    (when (and val-open? param?)
                                                                                                      param-pill)

                                                                                                    (when (and val-open? meta (not param?))
                                                                                                      [re-com/box
                                                                                                       :size "auto"
                                                                                                       ;:style {:width "200px"}
                                                                                                       :child [bricks/scrubber-panel true
                                                                                                               @(re-frame/subscribe [::bricks/keypaths-in-rs-values flow-id (first bbb)])
                                                                                                               [:runstreams flow-id (first bbb)] (first bbb) ;:* ;nil ;(first bbb) ;:*
                                                                                                               {:fm true :canvas? true :flow? true :runstreams? true}]])

                                                                                                    (when val-open?
                                                                                                      [re-com/box
                                                                                                       :size "none"
                                                                                                       :padding "7px"
                                                                                                       :style {:overflow "auto"
                                                                                                               :opacity 0.65
                                                                                                               :font-size "14px"
                                                                                                               :margin "3px"
                                                                                                                 ;:margin-right "-6px"
                                                                                                               :border-radius "8px"
                                                                                                               :background-color "#00000065"
                                                                                                          ;:margin-left "7px"
                                                                                                               }
                                                                                                       :width (px (- (/ panel-width 2) 26))
                                                                                                       :child [code-box-rs-value
                                                                                                               (- (/ panel-width 2) 16)
                                                                                                               nil
                                                                                                               flow-id kkey
                                                                                                               (if (and (vector? curr-val) (every? string? curr-val))
                                                                                                                 curr-val
                                                                                                                 (pr-str curr-val))  ;(pr-str curr-val)
                                                                                                               param?]])]]]]
                                                                              (if drop?
                                                                                (bricks/droppable-pills
                                                                                 ["meta-menu"] {:clause :runstream-overrides
                                                                                                :drop-data {:source (if (= :where (get-in @bricks/dragging-body [:drag-meta :type]))
                                                                                                                      (let [row-num      (get-in @bricks/dragging-body [:drag-meta :row-num])
                                                                                                                            src-table    (get-in @bricks/dragging-body [:drag-meta :source-query])
                                                                                                                            param-field  (get-in @bricks/dragging-body [:drag-meta :param-field])
                                                                                                                            param-full    (keyword (str (ut/unkey src-table) "/" (ut/unkey param-field) "." row-num))]
                                                                                                                        (assoc (get @bricks/dragging-body :drag-meta) :param-full param-full))
                                                                                                                      (get @bricks/dragging-body :drag-meta))
                                                                                                            :target {:flow-id flow-id
                                                                                                                     :type ttype
                                                                                                                     :key kkey}}} the-box)
                                                                                the-box)))]
                                                                 [re-com/h-box
                                                                    ;:size "1"
                                                                  ;:size "auto"
                                                                  ;:height "120px"
                                                                  :justify :between :align :start
                                                                  :children [[cell outcolor1 b1 b1-meta b1-droppable?]

                                                                             (when (> (count batch) 1)
                                                                               [cell outcolor2 b2 b2-meta b2-droppable?])]])))])

                                              (when open?
                                                [runstream-output-boxes flow-id blocks-map panel-width open-outputs? open-inputs])]])))

                             [re-com/v-box
                              :align :center :justify :center
                              :size "none"
                              :padding "6px"
                              :width (px (- panel-width 22))
                                ;:style {:border "1px solid white"}
                              :gap "10px"
                              :children (let [server-flows (map :flow_id @(re-frame/subscribe [::conn/sql-data [:flows-sys]]))
                                              server-flows (vec (sort (cset/difference (set server-flows) (set runstream-keys))))
                                              sql-calls {:flows-sys {:select [:flow_id :file_path :last_modified]
                                                                     :from [:flows]
                                                                     :connection-id "flows-db"
                                                                     :order-by [[3 :desc]]}}]

                                          ;;(tap> [:server-flows server-flows])

                                          (dorun (for [[k query] sql-calls]
                                                   (let [data-exists? @(re-frame/subscribe [::conn/sql-data-exists? [k]])
                                                         unrun-sql? @(re-frame/subscribe [::conn/sql-query-not-run? [k] query])]
                                                     (when (or (not data-exists?) unrun-sql?)
                                                       (if (get query :connection-id)
                                                         (conn/sql-data [k] query (get query :connection-id))
                                                         (conn/sql-data [k] query))))))

                                          (into
                                           [(when (not @add-flow-shelf?)
                                              [re-com/md-icon-button :src (at)
                                               :md-icon-name "zmdi-plus"
                                               :style {:color (str (theme-pull :theme/editor-outer-rim-color nil))
                                                       :cursor "pointer"
                                                       :font-size "26px"}
                                               :on-click #(reset! add-flow-shelf? (not @add-flow-shelf?))])]

                                           (if @add-flow-shelf?
                                             [[re-com/h-box
                                               :height "37px"
                                               :gap "7px"
                                               ;:style {:border "1px solid white"}
                                               :align :center :justify :center
                                               :children [[re-com/md-icon-button :src (at)
                                                           :md-icon-name "zmdi-plus"
                                                           :style {:color (str (theme-pull :theme/editor-outer-rim-color nil))
                                                                   :cursor "pointer"
                                                                   :font-size "26px"}
                                                           :on-click #(reset! add-flow-shelf? (not @add-flow-shelf?))]
                                                          [re-com/md-icon-button :src (at)
                                                           :md-icon-name "zmdi-refresh"
                                                           :on-click #(re-frame/dispatch [::conn/clear-query-history :flows-sys])
                                                           :style {:color (str (theme-pull :theme/editor-outer-rim-color nil))
                                                                   :cursor "pointer"
                                                                   :font-size "26px"}]

                                                          ;; [re-com/single-dropdown
                                                          ;;  :model add-flow-shelf
                                                          ;;  :placeholder "(select a flow from the server)"
                                                          ;;  :width (px (* panel-width 0.55))
                                                          ;;  :style {:background-color "#00000045"
                                                          ;;            ;:color "#ffffff"
                                                          ;;          :border-radius "10px"
                                                          ;;          :border "1px solid #ffffff18"}
                                                          ;;  :on-change #(reset! add-flow-shelf %)
                                                          ;;  :choices (vec (map (fn [n] {:id n :label n}) server-flows))]

                                                          [re-com/typeahead
                                                           ;:width "250px"
                                                           :suggestion-to-string (fn [item]
                                                                                   (str (get item :label)))
                                                                                   ;; render in input box (must be string, no hiccup :())
                                                           :render-suggestion (fn [ss _] ;; render in dropdown
                                                                                [re-com/box
                                                                                 :style {:color "#000000"}
                                                                                 :child (str (get ss :label))])
                                                           :on-change #(reset! add-flow-shelf (get % :id))
                                                           :rigid? true
                                                           :placeholder "Add which server flow?"
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
                                                                                        flow-parts))))]

                                                          [re-com/box
                                                           :style {:border (str "1px solid " (str (theme-pull :theme/editor-outer-rim-color nil) 89))
                                                                   :background-color (str (theme-pull :theme/editor-outer-rim-color nil) 45)
                                                                   :cursor "pointer"
                                                                   :border-radius "8px"
                                                                   :padding-top "5px"
                                                                   :padding-bottom "5px"
                                                                   :padding-right "14px"
                                                                   :padding-left "14px"}
                                                           :align :center :justify :center
                                                           :attr {:on-click #(do
                                                                               (re-frame/dispatch [::bricks/add-runstream @add-flow-shelf])
                                                                               (reset! add-flow-shelf? false))}
                                                           :child "add"]]]]
                                             [])))])]]))

(re-frame/reg-event-db
 ::update-item
 (undoable)
 (fn [db [_ kp v]]
   (assoc-in db (cons :panels kp) v)))

(re-frame/reg-event-db
 ::create-snapshot
 (undoable)
 (fn [db [_ & [key]]]
   (let [snapshot-map (get-in db [:snapshots :params])
         snap-cnt (+ 1 (count (keys snapshot-map)))
         click-param (get-in db [:click-param])
         snap-name (or key
                       ;(str "snapshot-" snap-cnt)
                       (ut/safe-key "snapshot"))
         panels (get db :panels)
         selected-tab (get db :selected-tab)
         block-states (vec (map #(vec (cons :panels %))
                                (filter #(or (= :w (last %))
                                             (= :h (last %))
                                             (= :z (last %))
                                             (= :hidden? (last %))
                                             (= :ghosted? (last %))
                                             (= :root (last %))
                                             (= :selected-view (last %)))
                                        (ut/kvpaths panels))))
         block-states-vals (into {}
                                 (for [b block-states]
                                   {b (get-in db b)}))
         snap {:ts (str (js/Date.))
               :menu? (get-in snapshot-map [key :menu?] false)
               :extra? (get-in snapshot-map [key :extra?] false)
               :selected-tab selected-tab
               :block-states block-states-vals
               :params click-param}]
     (assoc-in db [:snapshots :params snap-name] snap))))

(re-frame/reg-event-db
 ::rename-snapshot
 (undoable)
 (fn [db [_ old new]]
   (let [old-snap (get-in db [:snapshots :params old])]
     (-> db
         (assoc-in [:snapshots :params new] old-snap)
         (ut/dissoc-in [:snapshots :params old])))))

(re-frame/reg-event-db
 ::delete-snapshot
 (undoable)
 (fn [db [_ key]]
   (ut/dissoc-in db [:snapshots :params key])))

(re-frame/reg-event-db
 ::snapshot-menu-toggle
 (undoable)
 (fn [db [_ key]]
   (assoc-in db [:snapshots :params key :menu?]
             (not (get-in db [:snapshots :params key :menu?])))))

(re-frame/reg-event-db
 ::snapshot-extra-toggle
 (undoable)
 (fn [db [_ key]]
   (assoc-in db [:snapshots :params key :extra?]
             (not (get-in db [:snapshots :params key :extra?])))))

(re-frame/reg-sub
 ::is-current-params?
 (fn [db [_ params]]
   (true? (= params (get db :click-param {})))))

(defn render-honey-comb-fragments [c & [w h sys-name]] ;; TODO REPLACE WITH bricks/honeycomb-fragments
  (let [panel-key :virtual-panel ;:block-4752 ;:hello-there-brother
        key :virtual-view ;:view ;:ufo-country ;:heya!
        type (cond (vector? c) :view
                   (string? c) :view
                   (and (map? c)
                        (nil? (get c :view))) :query
                   :else :both)
        data_d c]
    (cond (= type :view)   (let [view {key data_d}
                                 w (or w 11)
                                 h (or h 9)
                                 ;w 11
                                 ;h 9
                                 ]
                             [bricks/honeycomb panel-key :virtual-view w h view nil])
          (= type :query) (let [temp-key (get data_d :_query-id (keyword 
                                                                 (if sys-name (str sys-name)
                                                                     (str "kick-" (hash c)))))
                                query {temp-key (-> data_d ;(get data_d :queries)
                                                    (dissoc :cache?)
                                                    (dissoc :refresh-every))}
                                h (get data_d :_h (or h 6))
                                w (get data_d :_w (or w 10))
                                ;h (get data_d :_h 6)
                                ;w (get data_d :_w 10)
                                ]
                            [re-com/box
                             :size "none"
                             :width (px (* w bricks/brick-size))
                             :height (px (- (* h bricks/brick-size) 30))
                             :child [bricks/honeycomb panel-key
                                     temp-key
                                     h w
                                     nil
                                     query]])
          (= type :both)    (let [queries (get data_d :queries)
                                  ;temp-key (keyword (str "kick-" (hash c)))
                                  qkeys (into {} (for [q (keys queries)]
                                                   {q (keyword (if sys-name 
                                                                 (str (ut/replacer (str q) #":" "") "-" sys-name)
                                                                 (str (ut/replacer (str q) #":" "") "-kick-" (hash data_d))))}))
                                  ndata (walk/postwalk-replace qkeys data_d)
                                  h (get data_d :_h (or h 11))
                                  w (get data_d :_w (or w 9))
                                  ;h (get data_d :_h 11)
                                  ;w (get data_d :_w 9)
                                  ]
                                                                                  ;(tap> [:qkeys (get data_d :selected-view) qkeys views (walk/postwalk-replace qkeys views)])
                              [bricks/honeycomb panel-key
                                                                                   ;(or (first (keys views)) (first (keys queries)))
                               key ;:view ;(get data_d :selected-view)
                               h w
                               {key (get ndata :view)} ;views ;(walk/postwalk-replace qkeys views)
                               (get ndata :queries) ;(walk/postwalk-replace qkeys queries)
                               ])
          :else [bricks/honeycomb panel-key key 11 9])))

(defonce ask-mutates-hover (reagent/atom nil))

(defn ask-mutates-render [ask-mutates uid]
  (let [rerun [@ask-mutates-hover]] ;; hack
    [re-com/v-box
     :gap "12px"
     :children (for [[k v] ask-mutates]
                 [re-com/box
                  :padding "8px"
                  :size "auto" :width "530px"
                  :attr {:on-mouse-enter #(reset! ask-mutates-hover [uid k])
                         :on-mouse-leave #(reset! ask-mutates-hover nil)
                         :on-click #(doseq [[kk vv] v]
                                      (re-frame/dispatch [::bricks/update-workspace-raw kk vv]))}
                  :style {:background-color (if (= @ask-mutates-hover [uid k])
                                              (str (theme-pull :theme/editor-rim-color nil) 89)
                                              "#00000055")
                          :border-radius "10px"
                          :margin-right "4px"
                          :cursor "pointer"
                          :border (str "2px solid " (theme-pull :theme/editor-outer-rim-color nil))}
                  :child [re-com/v-box
                          :children [(if (or (string? k) (nil? k))
                                       [re-com/box
                                        :size "auto"
                                        :padding "6px"
                                        :align :center :justify :center
                                        :style {:font-size "17px"}
                                        :child (str k)]
                                       [render-honey-comb-fragments k])
                                     [re-com/box :size "auto"
                                      :align :center :justify :center
                                      :style {:opacity 0.45
                                              :color (theme-pull :theme/editor-outer-rim-color nil)}
                                      :child (str "changes: " (keys v))]]]])]))

;; binding logic from option-buttons
;; kit-name         (get @db/kit-mode kp)
;; kick-kit-name    (first (keys (get kits :kits)))
;; kit-name         (if (or (= kit-name :kick) (= kit-name :ai/calliope)) kick-kit-name kit-name)
;;         ;kit-base-name    kit-name
;; kit-context-name (last kp) ;; :base
;; kit-context-name (if (nil? kit-context-name) :kick kit-context-name)
;; ik (get-in kit-results [0 :item_key])
;; item-key (if (cstr/starts-with? (str ik) ":") (try (edn/read-string ik) (catch :default _ "error!")) (keyword ik))


(re-frame/reg-event-db
 ::delete-kit-item
 (undoable)
 (fn [db [_ where id]]
   (re-frame/dispatch [::wfx/request :default
                       {:message    {:kind :delete-kit-rows
                                     :where where
                                     :client-name (get db :client-name)}
                        ;:on-response [::refresh-kits] ;; unneeded if we delete local and remote. "feels" faster this way
                        ;:on-timeout [::http/timeout-response]
                        :timeout    500000}])
   (assoc-in db [:data :kit-results-sys] (vec (filter #(not (= (get % :id) id)) (get-in db [:data :kit-results-sys]))))))

(def mutation-log (reagent/atom []))

(defn narrative-box [panel-height panel-width kp]
  ;(tap> [:narrative-up kp (get @db/kit-mode kp) @db/kit-mode])
  (let [;hist-key  (keyword (str "tmp-" (hash (first kp)) "-hist-sys")) ;;  :history-log-sys
        ;reaction-hack [@hide-diffs? @hide-block-diffs?  ];; important to force re-render
        ;snapshots @(re-frame.core/subscribe [::bricks/snapshots])
        ;narratives []
        [r1 r2] @(re-frame/subscribe [::bricks/query-waitings :kit-results-sys])
        running? (or r1 r2)
        ;curr-params @(re-frame.core/subscribe [::bricks/current-params])
        client-name @(re-frame.core/subscribe [::bricks/client-name])
        kit-name         (get @db/kit-mode kp)
        callie?  (= kit-name :ai/calliope)
        text-box?     true ;  (if (not callie?) true)
        text-box-height (if callie? 310 110)
        kit-name (if (= kit-name :ai/calliope) :kick kit-name) ;;; tempo demo
        iname (str (last kp))
        iname (if (empty? iname) ":base" iname)
        where-filter [:and
                      (if (or (= kit-name :kick) (= kit-name :ai/calliope))
                        [:= :client_name (str client-name)] [:= 1 1])
                      [:= :item_name iname]
                      [:= :kit_name (str kit-name)]]
        kit-results      @(re-frame.core/subscribe [::conn/sql-data [:kit-results-sys]])
        kits             (kit-rows-to-map kit-results)
        kick-kit-name    (last (keys (get kits :kits)))
        kit-name         (if (or (= kit-name :kick) (= kit-name :ai/calliope)) kick-kit-name kit-name)
        ;kit-base-name    kit-name
        kit-context-name (last kp) ;; :base
        kit-context-name (if (nil? kit-context-name) :base kit-context-name)

        curr-narrative   (get @db/kit-keys kp (get-in kit-results [0 :item_key]))

        narratives       (get-in kits [:kits kit-name kit-context-name curr-narrative :data])
        ;narrative-desc   (get-in kits [:kits  kit-base-name kit-context-name curr-narrative :description])
        as-pages?        (get-in kits [:kits  kit-name kit-context-name curr-narrative :options :pages?] false)
        narrative-item   (get @kit-pages kp 0)
        narrative-item   (if (and (> narrative-item (- (count narratives) 1)) (not running?) (not (= (count narratives) 0)))
                           (do (swap! kit-pages assoc kp (- (count narratives) 1))
                               (- (count narratives) 1))
                           narrative-item)
        wait?  @(re-frame/subscribe [::bricks/reco-running? kit-context-name kit-name])
        queued?  @(re-frame/subscribe [::bricks/reco-queued? kit-context-name kit-name])

        allow-mutate? (get @kit-mutations kp false)

        sql-calls {:kit-results-sys {:select [:*]
                                     :from [:kits]
                                     :where where-filter}}]
     ;(tap> [:kitz where-filter [kit-name kit-context-name curr-narrative] kits narratives])
    ;;   (tap> [:safe-key (ut/safe-key "snapshot")])
    ;;    (tap> [:safe-key @(re-frame/subscribe [::ut/safe-key "snapshot"])])
    ;; (tap> [:history-log history-log kp "!"])
    ;; (reset! db/active-tmp-history hist-key)

    ;(when (or wait? queued?) (re-frame/dispatch [::conn/clear-query-history [:kit-results-sys]]))

    (doseq [[k query] sql-calls]
      (let [;query (walk/postwalk-replace sql-params v)
            data-exists? @(re-frame/subscribe [::conn/sql-data-exists? [k]])
            unrun-sql? @(re-frame/subscribe [::conn/sql-query-not-run? [k] query])]
        (when (and (or (not data-exists?) unrun-sql?) (not (or wait? queued?)))
          (sql-data [k] query))))

    ;;(tap> [:narratives-gets kp kit-name running? (count kit-results) kits])

    (tap> [:narratives kit-name kits @db/chat-mode @db/kit-mode])

    (if false ; true
      (reagent.core/next-tick #(scroll-to-bottom "chat-v-box")) ;; temp calliope demo
      (reagent.core/next-tick #(smooth-scroll-to-element "chat-v-box-parent" "chat-v-box")))

    (if (or wait? running? queued?)

      [re-com/box
       :padding "5px"
       :size "none"
       :height (px (- panel-height 12 25 (when text-box? text-box-height))) ;; minus size of border top and bottom
       :width (px (- panel-width 12)) ;; minus size of border left and right and header
       :align :center :justify :center
       :child [re-com/md-icon-button
               :md-icon-name "zmdi-refresh"
               ;;:on-click #(re-frame/dispatch [::conn/clear-query-history query-key])
                    ;;  :attr (when hover-field-enable?
                    ;;          {:on-mouse-enter #(swap! db/context-box assoc query-key "refresh this row-set from the database")
                    ;;           :on-mouse-leave #(swap! db/context-box dissoc query-key)})
                    ;;  :class (when single-wait?
                    ;;           "rotate linear infinite")
               :class (if (or wait? queued?)
                        "rotate-reverse linear infinite"
                        "rotate linear infinite")
               :style {:font-size  "45px" ;; "15px"
                       :opacity 0.45
                       :color (theme-pull :theme/editor-outer-rim-color nil)
                       ;:cursor     "pointer"
                       ;:transform-origin "7.5px 11px" ;; "7.5px 11px"
                       :transform-origin "22.5px 22px" ;; "7.5px 11px"
                       ;:padding    "0px"
                       ;:margin-top "-3px"
                       }]]

      [re-com/box
       :padding "5px"
       :size "none"
       :height (px (- panel-height 12 25 (when text-box? text-box-height))) ;; minus size of border top and bottom
       :width (px (- panel-width 12)) ;; minus size of border left and right and header
       :attr {:id "chat-v-box-parent"}
       :style {:overflow "auto"
             ;:background-color (when running? "red" )
               :border-radius "16px"}
       :child [re-com/v-box
               :padding "4px"
               :gap "11px"
               :children (for [{:keys [name content order parameters id step-mutates ask-mutates] :as page} narratives
                               :let [idx (try (.indexOf narratives page) (catch :default _ 0))
                                     selected? (if callie? (= page (last narratives)) (= idx narrative-item)) ;; callie hack for demo
                                     hpage (hash page)
                                     viewable? (or (not as-pages?) selected?)
                                     reparameters (into {} ;; for param spawning
                                                        (for [[k v] parameters]
                                                          {(keyword
                                                            (-> (str
                                                                 (ut/unkeyword kit-context-name) ">"
                                                                 (ut/unkeyword curr-narrative) ">"
                                                                 (ut/unkeyword k))
                                                                (ut/replacer "/" "-")
                                                                (ut/replacer "." "-"))) v}))
                                     rereparameters (into {} ;; for ui
                                                          (for [[k v] parameters]
                                                            {(keyword
                                                              (str
                                                               (ut/unkeyword kit-name) "/"
                                                               (-> (str
                                                                    (ut/unkeyword kit-context-name) ">"
                                                                    (ut/unkeyword curr-narrative) ">"
                                                                    (ut/unkeyword k))
                                                                   (ut/replacer "/" "-")
                                                                   (ut/replacer "." "-")))) v}))
                                     _ (when (and (not (empty? step-mutates)) selected? allow-mutate?)
                                         (doseq [[kk vv] step-mutates]
                                           (re-frame/dispatch [::bricks/update-workspace-raw kk vv])))
                                     _ (when (and (not (empty? parameters)) viewable?)
                                         (dorun (tap> [:reparameters reparameters])
                                                (re-frame/dispatch [::click-parameter [kit-name] reparameters])))]
                               :when (if as-pages? selected? true)]

                           [re-com/v-box
                            :size "auto"
                            :padding "9px"
                            :attr {:id (if selected? "chat-v-box" (str "chat-v-box-" order "-" hpage))}
                            :style {:margin-left "5px"
                                  ;:border-top "2px solid #ffffff17"
                                  ;:border (when (and (not as-pages?) selected?) "3px solid black")
                                  ;:filter (when (and (not as-pages?) selected?) "brightness(1.9)")
                                    :box-shadow (when (and (not as-pages?) selected?)
                                                  (str "inset 0px 0px 2px 2px " (theme-pull :theme/editor-outer-rim-color nil) 33))
                                    :border-radius "12px"  ;; (str (theme-pull :theme/editor-outer-rim-color nil) 15)
                                    :background-color (if (and (not as-pages?) selected?) "#00000065" "#00000045")}
                            :children [[re-com/v-box
                                        :size "auto"
                                        :gap "11px"
                                        :justify :between :align :center
                                        :children [[re-com/h-box
                                                    :size "none"
                                                  ;:height "33px"
                                                    :width (px (- panel-width 40))
                                                    :justify :between :align :center
                                                    :children [[re-com/box
                                                              ;:align :start :justify :start
                                                                :size "auto"
                                                              ;:width (px (- panel-width 190))
                                                                :style {;:border "1px solid red"
                                                                        :padding-left "10px"
                                                                        :cursor "pointer"
                                                                        :user-select "none"}
                                                                :attr {:on-click #(swap! kit-pages assoc kp idx)}
                                                                :child  (if (not callie?)
                                                                          [re-com/box
                                                                           :style {:color (theme-pull :theme/editor-outer-rim-color nil)
                                                                                   :font-size "16px"
                                                                               ;:border "1px solid white"
                                                                                   :font-weight 500}
                                                                       ;;:width (px (- panel-width 60))
                                                                           :size "auto"
                                                                           :child (str name)]
                                                                          "")]
                                                               (when (or (= kit-name :kick) (= kit-name :ai/calliope))
                                                                 [re-com/md-icon-button :src (at)
                                                                  :md-icon-name "zmdi-close"
                                                                  :on-click #(re-frame/dispatch [::delete-kit-item [:and [:= :id id] where-filter] id])
                                                                 ;:on-click #(js/alert (str kp idx id client-name ))
                                                                  :style {:color (str (theme-pull :theme/editor-outer-rim-color nil) (if selected? 33 16))
                                                                          :cursor "pointer"
                                                                        ;:color "orange"
                                                                          :height "15px"
                                                                          :margin-right (if callie? "10px" "6px")
                                                                          :margin-top (if callie? "0px" "-15px")
                                                                        ;:margin-bottom "20px"
                                                                          :font-size "19px"}])]]
                                                   [re-com/v-box
                                                    :size "none"
                                                    :padding "6px"
                                                    :width (px (- panel-width 40))
                                                    :align :center :justify :center
                                                    :style {;:opacity 0.4
                                                            :font-size "12px"
                                                          ;:border "1px solid pink"
                                                            ;:background-color "#00000048"  ;;; looks nice, but is visually confusing IMHO?
                                                            :border-radius "13px"
                                                            :transform "translate(0)"
                                                            :font-weight 500}
                                                    :gap "14px"
                                                    :children (for [c content
                                                                    :let [;_ (tap> [:render type data_d key temp-key])
                                                                          type (cond (vector? c) :view
                                                                                     (string? c) :view
                                                                                     (and (map? c)
                                                                                          (nil? (get c :view))) :query
                                                                                     :else :both)
                                                                          query? (= type :query)
                                                                          both? (= type :both)
                                                                          signal? (or (try (empty? c) (catch :default _ false)) (keyword? c))
                                                                          c2 (if (and both? (not (nil? (get c :view))))
                                                                               (-> c ;; special mod for drag spawn
                                                                                   (assoc :views {:view (get c :view)})
                                                                                   (dissoc :view))
                                                                               c)
                                                                          execute? (and (vector? c) (= :execute (first c)))
                                                                          ;_ (tap> [:content-render execute? (when execute? (get c 1))])
                                                                          mods (when execute? (count (keys (get c 1))))
                                                                          _ (when execute?
                                                                              ;(re-frame/dispatch [::bricks/execute-kit-item [kit-name] c])
                                                                              (let [pp (get c 1)
                                                                                    ;_ (tap> [:pp id idx pp])
                                                                                    ;kk (first pp)
                                                                                    ;vv (last pp)
                                                                                    ]
                                                                                (doseq [[kk vv] pp]
                                                                                  (let [hid (hash [kk vv])
                                                                                        already? (some #(= hid (hash %)) @mutation-log)]
                                                                                    (when (not already?)
                                                                                      (do (tap> [:forced-execution-from-kick kk vv])
                                                                                          (swap! mutation-log conj hid)
                                                                                          (re-frame/dispatch [::bricks/update-workspace-raw kk vv])))))))
                                                                          draggable-spawn-fn (if (or both? query?)
                                                                                               bricks/sql-spawner-chat
                                                                                               bricks/view-spawner-chat)
                                                                          key-gen (let [kk (if query? (last kp) kp)]
                                                                                    (if (nil? kk) (ut/safe-key "kick") kk))]]
                                                        ;;       [re-com/box
                                                        ;;        :style {:border "1px solid green"}
                                                        ;;        :child [bricks/honeycomb
                                                        ;;                panel-key
                                                        ;;                :heya! 11 9
                                                        ;;                {:heya! c}
                                                        ;;                nil]] ;; needs to reference an existing block/view TODO

                                                                (cond

                                                                  signal? nil ;; do nothing, not actual content, a flow signal sneaking in

                                                                  execute? [re-com/box
                                                                            :width "580px" :align :end :justify :end
                                                                            :style {:margin-right "30px"}
                                                                            :child [re-com/box
                                                                            ;:align :end :justify :end
                                                                                    :size "none"
                                                                                    :child (str "*ran " mods " board modification" (when (> mods 1) "s"))
                                                                                    :style {;:background-color (str (theme-pull :theme/editor-outer-rim-color nil) 45)
                                                                                            :border (str "1px solid " (str (theme-pull :theme/editor-outer-rim-color nil) 45))
                                                                                            :border-radius "10px"
                                                                                            :color (str (theme-pull :theme/editor-outer-rim-color nil) 45) ;"#000000"
                                                                                            :padding-left "12px"
                                                                                            :padding-right "12px"}]]

                                                                  (or both? query?)

                                                                  [re-com/v-box
                                                                   :children [(bricks/draggable
                                                                               (draggable-spawn-fn key-gen c2) "meta-menu"
                                                                               [re-com/box
                                                                              ;:width "600px" ;:size "auto"
                                                                                :align :start :justify :start
                                                                                :child [re-com/md-icon-button :src (at)
                                                                                        :md-icon-name "zmdi-code-setting"
                                                                                        :style {;:color (theme-pull :theme/editor-font-color nil)
                                                                                                :cursor "grab"
                                                                                                :color "orange"
                                                                                                :height "15px"
                                                                                                :margin-top "-2px"
                                                                                                :margin-bottom "20px"
                                                                                                :font-size "19px"}]])
                                                                              [re-com/box :child [render-honey-comb-fragments c]]]]

                                                                  :else (bricks/draggable
                                                                         (draggable-spawn-fn key-gen c) "meta-menu"
                                                                         [re-com/box
                                                                          :padding "4px"
                                                                    ;:style {:background-color "green"}
                                                                          :child [render-honey-comb-fragments c 10]])))]

                                                   (when (not (empty? parameters))
                                                     [re-com/box
                                                      :style {:font-size "17px"}
                                                      :child "relevant parameters"])
                                                   (when (not (empty? parameters))
                                                   ;[re-com/box :child "paramtersr!"]
                                                     [bricks/click-param-browser [rereparameters] 540 nil])

                                                   (when (and (not (empty? parameters))
                                                              (not (empty? ask-mutates)))
                                                     [re-com/gap :size "10px"])

                                                   (when (not (empty? ask-mutates))
                                                     [re-com/box
                                                      :style {:font-size "17px"}
                                                      :child "possible board changes"])
                                                   (when (not (empty? ask-mutates))
                                                     [ask-mutates-render ask-mutates hpage])


                                                ;;  [re-com/box
                                                ;;   :size "auto"
                                                ;;   :style {:color (theme-pull :theme/editor-outer-rim-color nil)
                                                ;;           ;:opacity 1
                                                ;;           ;:cursor "pointer"
                                                ;;           ;:padding-right "10px"
                                                ;;           }
                                                ;;   :child (str order)]
                                                   ]]

                                       [re-com/gap :size "9px"]]])]])))

(defonce title-edit-idx (reagent/atom nil))

(defn snapshot-box [panel-height panel-width kp]
  (let [text-box-height 110
        text-box? true
        ;hist-key  (keyword (str "tmp-" (hash (first kp)) "-hist-sys")) ;;  :history-log-sys
        reaction-hack [@hide-diffs? @hide-block-diffs? @title-edit-idx];; important to force re-render
        snapshots @(re-frame.core/subscribe [::bricks/snapshots])
        ordered-snapshots (vec (sort-by :ts (vec (for [[k v] snapshots] (assoc v :key k)))))
        curr-params @(re-frame.core/subscribe [::bricks/current-params])
        ;; sql-calls {hist-key {:select [:client_name
        ;;                               :data :diff
        ;;                               :diff_kp :key
        ;;                               :kp :panel_key
        ;;                               :pre_data :type
        ;;                               :updated]
        ;;                      :from [:panel_history]
        ;;                      :limit 15
        ;;                      :order-by [[:updated :desc]]
        ;;                      :where [:= :kp (str kp)]}}
        ;; history-log @(re-frame.core/subscribe [::conn/sql-data [hist-key]])
        ]
    ;;   (tap> [:safe-key (ut/safe-key "snapshot")])
    ;;    (tap> [:safe-key @(re-frame/subscribe [::ut/safe-key "snapshot"])])
    ;; (tap> [:history-log history-log kp "!"])
    ;; (reset! db/active-tmp-history hist-key)
    ;; (dorun (for [[k query] sql-calls]
    ;;          (let [;query (walk/postwalk-replace sql-params v)
    ;;                data-exists? @(re-frame/subscribe [::conn/sql-data-exists? [k]])
    ;;                unrun-sql? @(re-frame/subscribe [::conn/sql-query-not-run? [k] query])]
    ;;            (when (or (not data-exists?) unrun-sql?)
    ;;              (conn/sql-data [k] query)))))

   ;; (reagent.core/next-tick #(scroll-to-bottom "chat-v-box")) ;; not needed here

    [re-com/box
     :padding "5px"
     :size "none"
     :height (px (- panel-height 12 25 (when text-box? text-box-height))) ;; minus size of border top and bottom
     :width (px (- panel-width 12)) ;; minus size of border left and right and header
     :attr {:id "chat-v-box"}
     :style {:overflow "auto"
             :border-radius "16px"
             ;:border "2px solid red"
             }
     :child [re-com/v-box
             :padding "4px"
             :gap "11px"
             :children (for [{:keys [key ts menu? extra? params block-states selected-tab]} ordered-snapshots
                             :let [valid-state-keys (vec (cset/intersection ;; since we are diffing, we only want state keys that exist in both
                                                          (set (keys block-states)) ;; otherwise we get a diff for every key - missing blocks, new blocks, etc
                                                          (set (keys (get curr-params :block-states))))) ;; which are irrelevant to the user
                                   mparams {:params params
                                            :block-states (select-keys block-states valid-state-keys)
                                            :selected-tab selected-tab}
                                   mparams (if extra? mparams (get mparams :params))
                                   curr-params (if extra? (assoc curr-params :block-states
                                                                 (select-keys (get curr-params :block-states) valid-state-keys))
                                                   (get curr-params :params))
                                   d1 (first (cdata/diff curr-params mparams))
                                   d2 (first (cdata/diff mparams curr-params))
                                   d1 (if @hide-block-diffs? (dissoc d1 :block-states) d1)
                                   d2 (if @hide-block-diffs? (dissoc d2 :block-states) d2)
                                   d1 (if (= (vec (keys d1)) [:params]) (get d1 :params) d1) ;; if there is only params, remove the key. more readable
                                   d2 (if (= (vec (keys d2)) [:params]) (get d2 :params) d2) ;; if there is only params, remove the key. more readable
                                   added (dissoc (ut/dissoc-in d1 [:params :user-sys]) :user-sys)
                                   removed (dissoc (ut/dissoc-in d2 [:params :user-sys]) :user-sys)
                                   added (if (= added {:params nil}) nil added)
                                   removed (if (= removed {:params nil}) nil removed)
                                   diffy (walk/postwalk-replace
                                          {:block-states :blocks :panels :_} ;; just for readability again, we have limited space
                                          {:added added
                                           :removed removed})
                                   ;_  (tap> [:diffy diffy])
                                   ]]

                         [re-com/v-box
                          :size "auto"
                          :padding "9px"
                          :style {;:border-top (if (= curr-params params) "3px dashed orange" "3px solid #ffffff17")
                                  ;:border-bottom (if (= curr-params params) "3px dashed orange" "inherit")
                                  :margin-left "5px"
                                  :border-top (if (= curr-params mparams) "2px solid #00000000" "2px solid #ffffff17")
                                  :border-radius (when (= curr-params mparams) "12px")
                                  :background-color (if (= curr-params mparams) (str (theme-pull :theme/editor-outer-rim-color nil) 15) "inherit")}
                          :children [[re-com/v-box
                                      :size "auto"
                                      :gap "11px"

                                      :justify :between :align :center
                                      :children [[re-com/h-box
                                                  :size "none"
                                                  ;:style {:border "1px solid white"}
                                                  :height "33px"
                                                  :width (px (- panel-width 40))
                                                  :justify :between :align :center
                                                  :children [[re-com/box
                                                              :align :start :justify :start
                                                              :size "none"
                                                              :width (px (- panel-width 190))
                                                              :style {;:border "1px solid red"
                                                                      :padding-left "10px"
                                                                      :cursor "pointer"
                                                                      :user-select "none"}
                                                              :attr {:on-double-click #(reset! title-edit-idx key)
                                                                     :on-click #(re-frame/dispatch [::bricks/swap-snapshot key])}
                                                              :child ;(str key)
                                                              (if (= @title-edit-idx key)
                                                                [re-com/h-box
                                                                 :children
                                                                 [[re-com/input-text
                                                                   :model (str key)
                                                                   :on-change #(do (tap> [:changed-snapshot-name (str key) :to (str %)])
                                                                                   (when (and (not (empty? (cstr/trim (str %))))
                                                                                              (not (some (fn [x] (= x %)) (keys snapshots))))
                                                                                     (re-frame/dispatch [::rename-snapshot key (str %)]))
                                                                                   (reset! title-edit-idx nil))
                                                                   :change-on-blur? true
                                                                   :width (px (- panel-width 230)) ;(px (+ 15 (* (count (str key)) 11))) ;"inherit" ; "200px"
                                                                   :style {:background-color "#00000000"
                                                                           :text-decoration "underline"
                                                                           :font-weight 700
                                                                           :font-size "16px"
                                                                           :border "none"
                                                                           :padding-left "0px"
                                                                           :color (theme-pull :theme/editor-outer-rim-color nil)}]

                                                                  [re-com/md-icon-button :src (at)
                                                                   :md-icon-name "zmdi-delete"
                                                                   ;:tooltip "delete this tab"
                                                                   :style {;:transform (when @db/show-tabs? "rotate(90deg)")
                                                                                          ;:background-color (theme-pull :theme/base-block-color-selected nil)
                                                                           :color (theme-pull :theme/editor-outer-rim-color nil)
                                                                                          ;:padding-left "4px"
                                                                                        ;:padding-right "10px"
                                                                                          ;:margin-left "-3px"
                                                                           :padding-left "10px"
                                                                           :font-size "14px"
                                                                           :padding-top "5px"
                                                                           :cursor "pointer"
                                                                           :width "10px"
                                                                           :height "20px" ;:width (px (+ 0 (* (count (str t)) 11)))

                                                                                                               ;:border "1px solid orange"
                                                                           }
                                                                   :attr {:on-click #(re-frame/dispatch [::delete-snapshot key])}]]]

                                                                [re-com/box
                                                                 ;:size "none"
                                                                 ;:width (px (- panel-width 140))
                                                                 :style {:color (theme-pull :theme/editor-outer-rim-color nil)
                                                                         :font-size "16px"
                                                                         :font-weight 700}
                                                                 :child (str key)
                                                                 ;:attr {:on-double-click #(reset! title-edit-idx (str key))}
                                                                 ])]

                                                             [re-com/box
                                                              :align :end :justify :end
                                                              :size "auto"
                                                              :attr {:on-click #(re-frame/dispatch [::snapshot-extra-toggle key])}
                                                              :style {:color (theme-pull :theme/editor-outer-rim-color nil)
                                                                      :opacity (if extra? 1 0.3)
                                                                      :cursor "pointer"
                                                                      :padding-right "10px"
                                                                      :text-decoration (if extra? "inherit" "line-through")}
                                                              :child "+ extra?"]

                                                             [re-com/box
                                                              :align :end :justify :end
                                                              :size "auto"
                                                              :attr {:on-click #(re-frame/dispatch [::snapshot-menu-toggle key])}
                                                              :style {:color (theme-pull :theme/editor-outer-rim-color nil)
                                                                      :opacity (if menu? 1 0.3)
                                                                      :cursor "pointer"
                                                                      :padding-right "10px"
                                                                      :text-decoration (if menu? "inherit" "line-through")}
                                                              :child "show?"]]]
                                                 [re-com/box
                                                  :size "none"
                                                  ;:height "33px"
                                                  ;:padding "6px"
                                                  :width (px (- panel-width 40))
                                                  :align :center :justify :center
                                                  :style {:opacity 0.4 :font-size "12px" :font-weight 500}
                                                  :child (str "updated: " ts)]]]
                                     [re-com/gap :size "9px"]
                                    ;;  [re-com/v-box
                                    ;;   :children (for [kk (partition-all 3 params)]
                                    ;;               [re-com/h-box
                                    ;;                :size "auto"
                                    ;;                :justify :between :align :center
                                    ;;                :gap "7px"
                                    ;;                :children (for [k (keys kk)
                                    ;;                                :let [vc (count (keys (get params k)))]]
                                    ;;                            [re-com/box :child (str k " (" vc ")")])])]

                                     (when (and (not @hide-diffs?)
                                                (not (empty? (get diffy :added))))
                                       [re-com/box
                                        :padding "6px"
                                        :style {:border "2px solid #32de84"
                                                :background-color "#32de8409"
                                                :margin-bottom "9px"
                                                ;:border-radius "12px"
                                                }
                                        :child [code-box 565 nil (str (get diffy :added))]])

                                     (when (and (not @hide-diffs?)
                                                (not (empty? (get diffy :removed))))
                                       [re-com/box
                                        :padding "6px"
                                        :style {:border "2px solid #fd5c63"
                                                :background-color "#fd5c6309"
                                                :margin-bottom "9px"
                                                ;:border-radius "12px"
                                                }
                                        :child [code-box 565 nil (str (get diffy :removed))]])

                                     [re-com/h-box
                                      :gap "7px"
                                      :justify :between :align :center
                                      :children [[re-com/box
                                                  :style {:cursor "pointer" :color (theme-pull :theme/editor-outer-rim-color nil)}
                                                  :attr {:on-click #(re-frame/dispatch [::create-snapshot key])}
                                                  :child "overwrite"]

                                                 (when (= curr-params mparams)
                                                   [re-com/box
                                                    :style {:color (str (theme-pull :theme/editor-outer-rim-color nil) 22)}
                                                    :child "(matches current)"])

                                                ;;  [re-com/box
                                                ;;   :style {:cursor "pointer" :color (theme-pull :theme/editor-outer-rim-color nil)}
                                                ;;   :attr {:on-click #(re-frame/dispatch [::bricks/swap-snapshot key])}
                                                ;;   :child "open"]
                                                 [re-com/box
                                                  :style {:cursor "pointer" :color (theme-pull :theme/editor-outer-rim-color nil)}
                                                  :attr {:on-click #(re-frame/dispatch [::delete-snapshot key])}
                                                  :child "delete"]]]]])]]))

(defn history-box [panel-height panel-width kp]
  (let [text-box-height 110
        text-box? false ; true
        hist-key  (keyword (str "tmp-" (hash (first kp)) "-hist-sys")) ;;  :history-log-sys
        ;kp (vec (ut/replacer (remove nil? kp) #" :* " " :base "))
        kp (if (some #(or (= % :viz-gen) (= % :*)) kp)
             (vec (walk/postwalk-replace {:* :base :viz-gen :base} (remove nil? kp)))
             kp)
        sql-calls {hist-key {:select [:client_name
                                      :data :diff
                                      :diff_kp :key
                                      :kp :panel_key
                                      :pre_data :type
                                      :updated]
                             :from [:panel_history]
                             :limit 15
                             :order-by [[:updated :desc]]
                             :where [:= :kp (str kp)]}}
        history-log @(re-frame.core/subscribe [::conn/sql-data [hist-key]])]
    (tap> [:history-log history-log kp "!"])
    (reset! db/active-tmp-history hist-key)
    (dorun (for [[k query] sql-calls]
             (let [;query (walk/postwalk-replace sql-params v)
                   data-exists? @(re-frame/subscribe [::conn/sql-data-exists? [k]])
                   unrun-sql? @(re-frame/subscribe [::conn/sql-query-not-run? [k] query])]
               (when (or (not data-exists?) unrun-sql?)
                 (conn/sql-data [k] query)))))

    (reagent.core/next-tick #(smooth-scroll-to-bottom "chat-v-box-parent" "chat-v-box"))

    [re-com/box
     :padding "5px"
     :size "none"
     :height (px (- panel-height 12 25 (when text-box? text-box-height))) ;; minus size of border top and bottom
     :width (px (- panel-width 12)) ;; minus size of border left and right and header
     :attr {:id "chat-v-box-parent"}
     :style {:overflow "auto"
             :border-radius "16px"
             ;:border "2px solid red"
             }
     :child [re-com/v-box
             :gap "11px"
             :children (doall (for [{:keys [client_name data diff diff_kp key kp panel_key pre_data type updated] :as full}
                                    (reverse history-log)]
                                (let [diff (try (edn/read-string diff) (catch :default _ [:cannot-read-val!]))
                                      panel_key (try (edn/read-string panel_key) (catch :default _ :nope-cant-work-panel_key))
                                      diff-str (str (remove nil? diff))
                                      is-last? (= (last (reverse history-log)) full)
                                      diff-str (if (> (count diff-str) 45)
                                                 (str (subs diff-str 0 45) "...")
                                                 (subs diff-str 0 45))

                                      data_d (try (edn/read-string data) (catch :default _ :nope-cant-work-data_d))
                                      kp_d (try (edn/read-string kp) (catch :default _ :nope-cant-work-kp_d))
                                      kp_d (if (some #(or (= % :viz-gen) (= % :*) (= % :base)) kp_d)
                                             [(first kp_d)] ;(vec (walk/postwalk-replace {:* :base :viz-gen :base} (remove nil? kp)))
                                             kp_d)
                                      has-flow-drop? @(re-frame/subscribe [::bricks/has-a-flow-view? panel_key (last kp_d)])
                                      key (try (edn/read-string key) (catch :default _ :nope-cant-work-key))
                                      temp-key (keyword (str (ut/replacer (str key) #":" "") "-hist-" (rand-int 123) (hash data)))]
                                 ; (tap> [:prev-q temp-key data])

                                  [re-com/v-box
                                   ;:padding "10px"
                                   :gap "5px" :size "none"
                                   :style {;:border (str "2px solid " (theme-pull :theme/editor-rim-color nil))
                                           ;:border-bottom (if assistant? "2px dashed #ffffff17" "inherit")
                                           ;              ;:border-right (if assistant? "2px dashed #ffffff17" "inherit")
                                           ;:background-color (if assistant? "inherit" (str (theme-pull :theme/editor-rim-color nil) 89))
                                           }
                                   :attr {:id (if is-last? "chat-v-box" (str "chat-v-box-" (hash full)))}
                                   :children [;[re-com/box :child (str kp)]
                                              [re-com/h-box
                                               :justify :between :align :center
                                               :height "33px" :padding "9px"
                                               :style {:background-color (str (theme-pull :theme/editor-rim-color nil) 99)
                                                       :border-radius "12px"
                                                       :cursor "pointer"}
                                               :attr {:on-click #(re-frame/dispatch [::update-item kp_d data_d])}
                                               :children [[re-com/box
                                                           :padding "4px"
                                                           :style {:font-weight 700 :font-size "15px"}
                                                           :child "" ; diff-str
                                                           ]
                                                          [re-com/box
                                                           :style {:font-size "16px"
                                                                   :opacity 0.5
                                                                   :color (str (theme-pull :theme/editor-font-color nil) 78)}
                                                           :child (str updated)]]]
                                              [re-com/box
                                               :style {:zoom 0.6 :transform "translate(0)"}
                                               :child (cond has-flow-drop?       ""
                                                            ;; [re-com/box
                                                            ;;                      :style {:font-size "16px"}
                                                            ;;                      :child "(contains flow drop code, no preview)"]
                                                            (= type ":views")   (let [view {key data_d}]
                                                                                  [bricks/honeycomb panel_key key 11 9 view nil])
                                                            (= type ":queries") (let [query {temp-key (-> data_d ;(get data_d :queries)
                                                                                                          (dissoc :cache?)
                                                                                                          (dissoc :refresh-every))}]
                                                                                  [bricks/honeycomb panel_key
                                                                                   temp-key
                                                                                   11 9
                                                                                   nil ;; no replacement view
                                                                                   query])
                                                            (= type ":base")    (let [queries (get data_d :queries)
                                                                                      views (get data_d :views)
                                                                                      qkeys (into {} (for [q (keys queries)]
                                                                                                       {q (keyword (str (ut/replacer (str q) #":" "") "-hist-" (rand-int 123) (hash data_d)))}))
                                                                                      ndata (walk/postwalk-replace qkeys data_d)]
                                                                                  ;(tap> [:qkeys (get data_d :selected-view) qkeys views (walk/postwalk-replace qkeys views)])
                                                                                  [bricks/honeycomb panel_key
                                                                                   ;(or (first (keys views)) (first (keys queries)))
                                                                                   (get data_d :selected-view)
                                                                                   11 9
                                                                                   (get ndata :views) ;views ;(walk/postwalk-replace qkeys views)
                                                                                   (get ndata :queries) ;(walk/postwalk-replace qkeys queries)
                                                                                   ])
                                                            :else [bricks/honeycomb panel_key key 11 9])]
                                              [code-box 580 nil diff]
                                              ;[code-box 580 nil diff_kp]
                                             ; [code-box 580 nil (str data)]
                                              [re-com/gap :size "10px"]
                                            ;;   [re-com/box
                                            ;;    ;:justify (if assistant? :start :end)
                                            ;;    ;:align (if assistant? :start :end)
                                            ;;    :style {:color (str (theme-pull :theme/editor-font-color nil) 97)}
                                            ;;                  ;:child (str content)
                                            ;;    :child (str data)]
                                              ]])))]]))

(re-frame/reg-event-db
 ::refresh-kits
 (fn [db [_]]
   (ut/dissoc-in db [:query-history :kit-results-sys])))

(re-frame/reg-event-db
 ::refresh-runstreams
 (fn [db [_]]
   (dissoc db :runstreams-lookups)))

(defn chat-panel []
  (let [x-px (px (first @detached-coords)) ;(px (* x bricks/brick-size))
        y-px (px (last @detached-coords)) ;(px (* y bricks/brick-size))
        panel-width 600
        text-box-height 110
        selected-block @(re-frame/subscribe [::bricks/selected-block])
        selected-view @(re-frame/subscribe [::bricks/editor-panel-selected-view])
        kp (vec (flatten [selected-block selected-view]))

        chats-vec @(re-frame/subscribe [::chats kp])
        audio-playing? @(re-frame/subscribe [::audio/audio-playing?])
        hh @(re-frame/subscribe [::subs/h]) ;; to ensure we get refreshed when the browser changes
        ww @(re-frame/subscribe [::subs/w])
        panel-height (* (.-innerHeight js/window) 0.8)
        ;multiple-choice [option-buttons kp]
        mode (get @db/chat-mode kp (if
                                    ;(nil? (second kp))
                                    (= "none!" (first kp))
                                     ;:snapshots
                                     :runstreams
                                     :history
                                     ;:runstreams
)) ;; default to snap if unselected
        kmode (get @db/kit-mode kp)
        ;;_ (tap> [:kp kp mode kmode])
        text-box? (or (= mode :snapshots) (= mode :buffy) (= mode :narratives) (= mode :kick))
        valid-kits @(re-frame/subscribe [::valid-kits])
        narrative-mode? (some #(= % (last kp)) (keys valid-kits))
        ;_ (tap> [:valid-kits2 (vec (keys valid-kits))])
        choices (into
                 (if (and (not
                           ;(nil? (second kp))
                           (= "none!" (first kp))) (not (empty? valid-kits)))
                   (vec (map (fn [k] [:narratives k]) (keys valid-kits))) [])
                 (vec (remove nil? [;;[:buffy :ai/calliope]

                                    (when (= "none!" (first kp))
                                      [:runstreams :runstreams])
                                    ;; runways, blueprints, burrows, pookas, harestream, runstream, rabbitry (where farmed rabbit are kept, kinda dark)

                                    (if (= "none!" (first kp)) ; (nil? (second kp))
                                      [:snapshots :snapshots]
                                      [:history :history])

                                    [:kick :kick]])))]

    ;; (tap> [:shit mode (vals @db/kit-mode) @db/kit-keys @db/kit-mode])
    ;(tap> [:valid-kits valid-kits selected-block selected-view kp])
    ;(tap> [:choices choices mode kmode])
    ;(tap> [:buffy-modes mode kmode])

    [rc/catch
     [re-com/box
      :size "none"
      :width (px panel-width)
      :height (px panel-height)
      :attr {:on-mouse-enter #(reset! bricks/over-block? true)
             :on-mouse-leave #(reset! bricks/over-block? false)}
      :style {:position "fixed"
              :top y-px
              :left x-px
              :border-radius "16px"
              :z-index 100
              :font-family (theme-pull :theme/base-font nil)
              :color (theme-pull :theme/editor-font-color nil)
              :background-color (str (theme-pull :theme/editor-background-color nil) 99) ; "#000000"
              :backdrop-filter "blur(5px)"
              :border (str "6px solid " (theme-pull :theme/editor-outer-rim-color nil)) ; #b7e27c"
              :filter "drop-shadow(0.35rem 0.35rem 0.4rem rgba(0, 0, 0, 0.8))"

            ;;   :transition (str "opacity 0.2s ease-in-out, background-color 0.5s ease-in-out, border 0.3s ease-in-out"
            ;;                    (if (not (audio-playing?))
            ;;                      ", box-shadow 0.33s ease-in-out"
            ;;                      ""))
              :box-shadow (let [block-id :audio
                               ; block-type :speak
                                talking-block? true]
                            (cond (and audio-playing? talking-block?)
                                  (str "1px 1px " (px (* 80 (+ 0.1
                                                               (get @db/audio-data block-id))))  " "
                                       (theme-pull :theme/editor-outer-rim-color nil))
                                                           ;(str "1px 1px 70px red")
                                  ;(or (and hovered? @on-block) kit-drop?)
                                  ;(str "1px 1px 30px " (get db/block-codes block-type) 87)
                                  :else "none"))}
      :child (if false ;@bricks/dragging-editor?
               [re-com/box :child "paused"]
               [re-com/v-box
                :width "575px"
              ;:style {:margin-top "5px"}
                :children [[re-com/h-box
                            :justify :between :align :center
                            :padding "6px"
                            :children [[re-com/md-icon-button :src (at)
                                        :md-icon-name "zmdi-arrows"
                                        :style {;:background-color (theme-pull :theme/editor-rim-color nil) ;"#00000000"
                                                :color (theme-pull :theme/editor-font-color nil)
                                                :cursor "grab"
                                              ;:margin-left "3px"
                                                :height "15px"
                                                :margin-top "-9px"
                                                :font-size "19px"}
                                        :attr {:on-mouse-down mouse-down-handler}]

                                       [re-com/h-box
                                        :children
                                        [[re-com/h-box
                                          :children (cons
                                                     [re-com/md-icon-button :src (at)
                                                      :md-icon-name "zmdi-refresh"
                                                      :style {;:background-color (theme-pull :theme/editor-rim-color nil) ;"#00000000"
                                                            ;:color (theme-pull :theme/editor-font-color nil)
                                                              :cursor "pointer"
                                                              :color "inherit"
                                                              :margin-right "5px"
                                                              :opacity 0.33
                                                              :height "15px"
                                                              :margin-top "3px"
                                                              :font-size "19px"}
                                                      :attr {:on-click #(re-frame/dispatch (if (and (= kmode :runstreams)
                                                                                                    (= mode :runstreams))
                                                                                             [::refresh-runstreams]
                                                                                             [::refresh-kits]))}]

                                                     (for [[c v] choices]
                                                       [re-com/box :child (if (= c :buffy) ;(str v "!")
                                                                            [re-com/h-box :children
                                                                             [[re-com/box :child " :ai-worker/ " :style {}]
                                                                              [re-com/box :child " Calliope"  ; " Buffy"
                                                                               :style {:font-family "Homemade Apple" :color "orange" :margin-top "2px"}]]]
                                                                            (str v))
                                                        :attr {:on-click #(do (swap! db/chat-mode assoc kp c)
                                                                              (swap! db/kit-mode assoc kp v))}
                                                        :style (if (and (= mode c) (= kmode v))
                                                                 {:text-decoration "underline"}
                                                                 {:cursor "pointer" :opacity 0.6})
                                                        :padding "4px"]))]
                                         [re-com/md-icon-button
                                          :md-icon-name "zmdi-window-minimize"
                                          :on-click #(re-frame/dispatch [::bricks/toggle-buffy])
                                          :style {:font-size "15px"
                                                  :opacity   0.33
                                                  :cursor    "pointer"}]]]]

                            :size "none"
                            :width "588px"
                            :height "25px"
                            :style {;:background-color (theme-pull :theme/editor-rim-color nil)
                                    :background (str "linear-gradient("
                                                     (theme-pull :theme/editor-rim-color nil)
                                                     ", transparent)")
                                    :border-radius "10px 10px 0px 0px"
                                    :color (theme-pull :theme/editor-outer-rim-color nil)}]

                           (cond ;(= mode :buffy2)     [chat-box panel-height panel-width chats-vec kp]
                             (= mode :runstreams) [runstream-box panel-height panel-width kp]
                             (= mode :buffy)      [narrative-box panel-height panel-width kp]
                             (= mode :snapshots)  [snapshot-box panel-height panel-width kp]
                             narrative-mode?      [narrative-box panel-height panel-width kp]
                             (= mode :narratives) [narrative-box panel-height panel-width kp]
                             (or (= mode :kick) (= mode :buffy))      [narrative-box panel-height panel-width kp]
                             :else [history-box panel-height panel-width kp])

                           (when text-box?
                             [re-com/v-box
                              :children [[option-buttons kp mode]
                                         (when (and (not (some #(= % kp) @audio/waiting-for-response))
                                                    (not (= mode :narratives))
                                                    (not (or (= mode :kick) (= mode :buffy)))
                                                    (not (= mode :snapshots)))
                                           [re-com/input-textarea
                                            :width (px (- panel-width 20))
                                            :model ""
                                            :rows 2
                                            :placeholder "_"
                                            :style {:border "1px solid #00000000"
                                                    :background-color "inherit"
                                                    :font-size "16px"
                                                    :font-weight 700}
                                            :on-change #(re-frame/dispatch [::audio/add-chat % kp])])]
                              :padding "5px"
                              :height (px text-box-height)
                              :width (px (- panel-width 12))
                              :style {:border-top (str "2px solid " (theme-pull :theme/editor-outer-rim-color nil))}])]])]]))
