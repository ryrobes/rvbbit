(ns rvbbit-frontend.bricks
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require
   [reagent.core :as reagent]
   [re-frame.core :as re-frame]
   [re-frame.alpha :as rfa]
   ;[re-frame.trace :as rft]
   [re-com.core :as re-com :refer [at]]
   [re-com.util :refer [px]]
   [rvbbit-frontend.recharts :as reech]
   [rvbbit-frontend.audio :as audio]
   ["react-map-gl" :default Map
    :refer [Source Layer Marker]]
    ;[re-pressed.core :as rp]
   [day8.re-frame.undo :as undo :refer [undoable]]
   [clojure.edn :as edn]
   ;[rvbbit-frontend.mapboxes :as mpb]
   [rvbbit-frontend.scrubbers :as scrub]
   [rvbbit-frontend.utility :as ut]
   [rvbbit-frontend.resolver :as resolver]
   [rvbbit-frontend.subs :as subs]
   [rvbbit-frontend.db :as db]
   [rvbbit-frontend.layout :as lay]
   [rvbbit-frontend.select-transform :as st]
   [rvbbit-frontend.http :as http]
   [rvbbit-frontend.connections :as conn]
   [re-catch.core :as rc]
   ;[honey.sql :as honey]
    ;[websocket-fx.core :as wfx]
   [cljs.tools.reader :refer [read-string]]
   [oz.core :as oz]
   [clojure.walk :as walk]
   ;[zprint.core :as zp]
   [clojure.string :as cstr]
   [clojure.set :as cset]
   [goog.events :as gevents]
   ["@nivo/bar" :as nivo-bar]
   ["@nivo/line" :as nivo-line]
   ["@nivo/pie" :as nivo-pie]
   ["@nivo/scatterplot" :as nivo-scatterplot]
   ["@nivo/swarmplot" :as nivo-swarmplot]
   ["@nivo/treemap" :as nivo-treemap]
    ;["@nivo/geo" :as nivo-geo]
   ;["@nivo/heatmap" :as nivo-heatmap]
   ["@nivo/waffle" :as nivo-waffle]
   ["@nivo/calendar" :as nivo-calendar]
   ["react-drag-and-drop" :as rdnd]
   ["react-codemirror2" :as cm]
   ["codemirror/mode/sql/sql.js"]
   ["codemirror/addon/edit/matchbrackets.js"]
   ["codemirror/addon/edit/closebrackets.js"]
   ["codemirror/addon/hint/show-hint.js"]
   ;["codemirror/addon/hint/javascript-hint.js"]
   ;["codemirror/addon/selection/active-line.js"]
   ["codemirror/mode/clojure/clojure.js"]
   ["codemirror/mode/python/python.js"]
   ["codemirror/mode/r/r.js"]
   ["codemirror/mode/julia/julia.js"]
   ["codemirror/mode/markdown/markdown.js"]
   ["html2canvas" :as html2canvas]
   ;;["html2canvas" :as html2canvas]
   ;[html2canvas :default html2canvas]
   [goog.i18n.NumberFormat.Format]
   ;["react-date-range" :as date-picker]
    [reagent.dom :as rdom]
    ;[goog.events.KeyCodes :as keycodes]
    ;[goog.events :as gev]
   [cljs.core.async :as async :refer [<! >! chan]]
   [rvbbit-frontend.shapes :as shape]
   [websocket-fx.core :as wfx])
  ;;(:require-macros [rvbbit-frontend.macros :refer [time-expr]])
  (:import
   [goog.i18n NumberFormat]
   [goog.i18n.NumberFormat Format]
   [goog.events EventType]))

(def brick-size 50)

(def dragging? (reagent/atom false))
(def dragging-editor? (reagent/atom false))
(def dragging-block (reagent/atom nil))
(def dragging-size (reagent/atom []))
(def dragging-body (reagent/atom []))
;;(def dragging-body2 (reagent/atom [])) ;; flow pills
(def dyn-dropper-hover (reagent/atom nil))
(def on-scrubber? (reagent/atom false))
(defonce mad-libs-view (reagent/atom nil))                  ;; will be query ids
(defonce mad-libs-top? (reagent/atom true))
(def swap-layers? (reagent/atom false))

;; (re-frame/reg-event-db
;;  ::ship-atom
;;  (fn [db [_ atom-name aval]]
;;    (ut/tracked-dispatch [::wfx/request :default
;;                        {:message    {:kind :client-ui
;;                                      :atom-name atom-name
;;                                      :value aval
;;                                      :client-name (get db :client-name)}
;;                         :timeout    500000}]) db))

(re-frame/reg-event-fx
 ::ship-atom
 (fn [{:keys [db]} [_ atom-name aval]]
   (ut/tapp>> [:setting :atom-name (get db :client-name) atom-name aval])
   {:dispatch [::wfx/request :default
               {:message    {:kind :client-ui
                             :atom-name atom-name
                             :value aval ;; (if (false? aval) nil aval)
                             :client-name (get db :client-name)}
                :timeout    500000}]}))

(defn watch-fn [key ref old-state new-state]
  (when (not= old-state new-state)
    (ut/tracked-dispatch [::ship-atom key new-state])))

(add-watch dragging? "dragging?" watch-fn)
(add-watch dragging-body "dragging-body" watch-fn)

(defonce drop-last-tracker (reagent/atom {}))
(defonce drop-last-tracker-refs (reagent/atom {}))

;; (ut/tapp>> [(st/transform {:transform-select [[[:sum :sales] :total-sales]
;;                                             ;[[:sum :items-sold] :total-items]
;;                                          :product :category :rep]
;;                       :from [:data]
;;                       :pivot-by [:category]} st/data)])

(defn tag-screen-position [evt]                             ;; for drag and drop components
  (reset! db/context-modal-pos [(.-clientX evt)
                                (.-clientY evt)]))

;; (re-frame/reg-sub
;;  ::resolved-theme
;;  (fn [db [_]]
;;    (let [theme (get-in db [:click-param :theme])
;;          self-ref-keys (distinct (filter #(and (keyword? %) (namespace %)) (ut/deep-flatten db/base-theme)))
;;          self-ref-pairs (into {}
;;                               (for [k self-ref-keys ;; todo add a reurziver version of this
;;                                     :let [bk (keyword (ut/replacer (str k) ":theme/" ""))]]
;;                                 {k (get db/base-theme bk)}))
;;          resolved-base-theme (ut/postwalk-replacer self-ref-pairs db/base-theme)]
;;      resolved-base-theme)))

;; (defn resolved-theme []
;;   (let [self-ref-keys (distinct (filter #(and (keyword? %) (namespace %)) (ut/deep-flatten db/base-theme)))
;;         self-ref-pairs (into {}
;;                              (for [k self-ref-keys ;; todo add a reurziver version of this
;;                                    :let [bk (keyword (ut/replacer (str k) ":theme/" ""))]]
;;                                {k (get db/base-theme bk)}))
;;         resolved-base-theme (ut/postwalk-replacer self-ref-pairs db/base-theme)]
;;     resolved-base-theme))

;(def screenshot (reagent/atom nil))

(re-frame/reg-sub
 ::sessions
 (fn [db _]
   (get db :sessions)))

(re-frame/reg-event-db
 ::save-sessions
 (fn [db [_ res]]
   ;(ut/tapp>> [:res (get db :client-name) res])
   (assoc db :sessions res)))

(re-frame/reg-event-db
 ::take-screenshot
 (fn [db [_ & [save?]]]
   (let [element (js/document.getElementById "base-canvas")
         ;session-hash (hash [(get db :panels)
         ;                    (get db :click-param)])
         fs (vec (for [kk (get db :flow-subs)
                       :let [[f1 f2] (ut/splitter (ut/replacer (str kk) ":" "") "/")]]
                   [(keyword f1) (keyword f2)]))
         session-hash (hash [(ut/remove-underscored (get db :panels))
                             ;(get db :click-param)
                             (ut/remove-keys (get db :click-param) (into (map first fs) [:flow :time :server :flows-sys :client :solver :signal-history :data :solver-meta nil]))])
         ]
     ;(ut/tapp>> [:pushed-snap? (get db :client-name)])
     (.then (html2canvas element)
            (fn [canvas]
              (let [dataUrl (.toDataURL canvas "image/jpeg" 0.75)]
              ;(reset! screenshot dataUrl)
                (when (not (nil? dataUrl))
                  ;(ut/tapp>> [:push-snap? (get db :client-name)])
                  (ut/tracked-dispatch [(if save?
                                        ::http/save-screen-snap
                                        ::http/save-snap) dataUrl]))))
            (fn [error]
              (ut/tapp>> ["Error taking screenshot:" error])))

     (when (not save?)
       (ut/tracked-dispatch [::wfx/request :default
                           {:message    {:kind :session-snaps
                                         :client-name (get db :client-name)}
                            :on-response [::save-sessions]
                            :timeout    15000}]))

     (assoc db :session-hash session-hash))))

(defn mouse-active-recently? [seconds]
  (let [now (js/Date.)
        last-activity @db/last-mouse-activity
        difference (- now last-activity)
        threshold (* seconds 1000)] ;; Convert seconds to milliseconds
    (< difference threshold)))

(re-frame/reg-sub
 ::is-mouse-active?
 (fn [db _]
   (let [fs (vec (for [kk (get db :flow-subs)
                       :let [[f1 f2] (ut/splitter (ut/replacer (str kk) ":" "") "/")]]
                   [(keyword f1) (keyword f2)]))
         session-hash (hash [(ut/remove-underscored (get db :panels))
                             ;(get db :click-param)
                             (ut/remove-keys (get db :click-param) (into (map first fs) [:flow :time :server :flows-sys :client :solver :signal-history :data :solver-meta nil]))
                             ])]
     (and (not= session-hash (get db :session-hash))
          (not (true? (mouse-active-recently? 5)))))))

;;(ut/tapp>> [:screenshot @screenshot])

;; (re-frame/reg-event-db
;;  ::save-screenshot
;;  (fn [db [_ dataUrl]]
;;    ;; Save the screenshot data URL in the db
;;    (assoc db :screenshot dataUrl)
;;    ))


(re-frame/reg-sub
 ::materialized-theme
 (fn [db _]
   (let [tt (get-in db [:click-param :theme])
         tt (ut/postwalk-replacer {:text :_text} tt)
         tt (resolver/logic-and-params tt nil)]
     (ut/tapp>> [:materialized-theme
            (get db :client-name)
            (ut/postwalk-replacer {:_text :text} tt)]))
   nil))


;(theme-pull :theme/editor-rim-color "#a3a3a3")


(defn theme-pull-fn [cmp-key fallback & test-fn]
  (let [;v                   @(ut/tracked-subscribe [::conn/clicked-parameter-key [cmp-key]])
        v                   @(rfa/sub ::conn/clicked-parameter-key-alpha {:keypath [cmp-key]})
        ;v                   (resolver/logic-and-params v nil)
        t0                  (ut/splitter (str (ut/safe-name cmp-key)) #"/")
        t1                  (keyword (first t0))
        t2                  (keyword (last t0))
        self-ref-keys       (distinct (filter #(and (keyword? %) (namespace %)) (ut/deep-flatten db/base-theme)))
        self-ref-pairs      (into {}
                                  (for [k self-ref-keys     ;; todo add a reurziver version of this
                                        :let [bk (keyword (ut/replacer (str k) ":theme/" ""))]]
                                    {k (get db/base-theme bk)}))
        resolved-base-theme (ut/postwalk-replacer self-ref-pairs db/base-theme)
        base-theme-keys     (keys resolved-base-theme)
        theme-key?          (true? (and (= t1 :theme) (some #(= % t2) base-theme-keys)))
        fallback0           (if theme-key? (get resolved-base-theme t2) fallback)
        rs (fn [edn] (resolver/logic-and-params edn nil))]
    (rs (if (not (nil? v)) v fallback0))))

;; (re-frame/reg-sub
;;  ::theme-pull-sub ;; more cache hits?
;;  (fn [_ [_ cmp-key fallback & test-fn]]
;;    (theme-pull-fn cmp-key fallback test-fn)))

(re-frame/reg-sub
 ::theme-pull-sub
 (fn [_ {:keys [cmp-key fallback test-fn]}]
   (theme-pull-fn cmp-key fallback test-fn)))

(defn theme-pull [cmp-key fallback & test-fn]
  ;@(ut/tracked-subscribe [::theme-pull-sub cmp-key fallback test-fn])
  @(rfa/sub ::theme-pull-sub {:cmp-key cmp-key :fallback fallback :test-fn test-fn})
  ;(theme-pull-fn cmp-key fallback test-fn) ;; just run directly? faster? wont get any caching, but maybe thats okay? NO, the sub is def faster..
  )


;; (re-frame/reg-sub
;;  ::theme-pull-sub
;;  (fn [_ [_ cmp-key fallback test-fn]]
;;    (theme-pull-fn cmp-key fallback test-fn)))

;; (defn theme-pull [cmp-key fallback & test-fn]
;;   @(rfa/sub ::theme-pull-sub [cmp-key fallback test-fn]))


(defn code-mirror-theme [] ;; generate a dynamic theme to inject into the DOM
  (let [ccolor (fn [x] (get (theme-pull :theme/data-colors db/data-colors) x))]
    {;; Editor styles
     ".cm-s-rvbbit-dynamic.CodeMirror" {:background "inherit",
                                        :color "#b3b1ad",
                                        :font-size "inherit",
                                        :line-height "inherit"}
     ".cm-s-rvbbit-dynamic .CodeMirror-selected" {:background (theme-pull :theme/editor-outer-rim-color nil)} ;; "#ff4aff"
                                                  ;:color (ut/invert-hex-color (theme-pull :theme/editor-outer-rim-color nil))

     ".cm-s-rvbbit-dynamic .CodeMirror-line::selection, .cm-s-rvbbit-dynamic .CodeMirror-line > span::selection, .cm-s-rvbbit-dynamic .CodeMirror-line > span > span::selection"
     {:background "#27374763"} ;; {:background "rgba(39, 55, 71, 99)"}
     ".cm-s-rvbbit-dynamic .CodeMirror-line::-moz-selection, .cm-s-rvbbit-dynamic .CodeMirror-line > span::-moz-selection, .cm-s-rvbbit-dynamic .CodeMirror-line > span > span::-moz-selection"
     {:background "#27374763"} ;; {:background "rgba(39, 55, 71, 99)"}
     ".cm-s-rvbbit-dynamic .CodeMirror-gutters" {:background "inherit"
                                                 :border-right "0px"
                                                 :z-index "-1"
                                                 :opacity "0.9"}
     ".cm-s-rvbbit-dynamic .CodeMirror-guttermarker" {:color "white"
                                                      :z-index "-1"
                                                      :opacity "0.9"}
     ".cm-s-rvbbit-dynamic .CodeMirror-guttermarker-subtle" {:color "#3d424d"}
     ".cm-s-rvbbit-dynamic .CodeMirror-linenumber" {:color "#3d424d"
                                                    :z-index "-1"
                                                    :opacity "0.9"}
     ".cm-s-rvbbit-dynamic .CodeMirror-cursor" {:border-left (str "2px solid "
                                                                  ;(theme-pull :theme/editor-outer-rim-color nil)
                                                                  (ut/invert-hex-color (theme-pull :theme/editor-outer-rim-color nil)))} ;; {:border-left "1px solid #f386bf"}
         ;; Syntax highlighting
     ".cm-s-rvbbit-dynamic span.cm-comment" {:color "#626a73"}
     ".cm-s-rvbbit-dynamic span.cm-atom" {:color (ccolor "keyword")} ;; {:color "#9a79ec"}
     ".cm-s-rvbbit-dynamic span.cm-number" {:color (ccolor "integer")} ;; {:color "#50a978"}
     ".cm-s-rvbbit-dynamic span.cm-float" {:color (ccolor "float")} ;;{:color "#ffffff"}
     ".cm-s-rvbbit-dynamic span.cm-comment.cm-attribute" {:color "#f386bf"}
     ".cm-s-rvbbit-dynamic span.cm-comment.cm-def" {:color "#39bae650"} ;; #39bae650 ?
     ".cm-s-rvbbit-dynamic span.cm-comment.cm-tag" {:color "#39bae6"}
     ".cm-s-rvbbit-dynamic span.cm-comment.cm-type" {:color "#5998a6"}
     ".cm-s-rvbbit-dynamic span.cm-property, .cm-s-rvbbit-dynamic span.cm-attribute" {:color "#f386bf"}
     ".cm-s-rvbbit-dynamic span.cm-keyword" {:color (ccolor "keyword")} ;;{:color "#33ccda"}
     ".cm-s-rvbbit-dynamic span.cm-builtin" {:color (ccolor "keyword")} ;;{:color "#33ccda"}
     ".cm-s-rvbbit-dynamic span.cm-string" {:color (ccolor "string")} ;;{:color "#feb733"}
     ".cm-s-rvbbit-dynamic span.cm-variable" {:color "#b3b1ad"}
     ".cm-s-rvbbit-dynamic span.cm-variable-2" {:color "#f07178"}
     ".cm-s-rvbbit-dynamic span.cm-variable-3" {:color "#39bae6"}
     ".cm-s-rvbbit-dynamic span.cm-type" {:color "#33ccda"}
     ".cm-s-rvbbit-dynamic span.cm-def" {:color "#ffee99"}
     ".cm-s-rvbbit-dynamic span.cm-bracket" {:color "#d2d8ff"}
     ".cm-s-rvbbit-dynamic span.cm-tag" {:color "#39bae650"} ;; #39bae650 ? was "rgba(57, 186, 230, 80)"
     ".cm-s-rvbbit-dynamic span.cm-header" {:color "#c2d94c"}
     ".cm-s-rvbbit-dynamic span.cm-link" {:color "#39bae6"}
     ".cm-s-rvbbit-dynamic span.cm-error" {:color "#ff3333"}
     ".cm-s-rvbbit-dynamic .CodeMirror-activeline-background" {:background "#01060e"}
     ".cm-s-rvbbit-dynamic .CodeMirror-matchingbracket" {:text-decoration "underline",
                                                         :color (str (theme-pull :theme/editor-outer-rim-color nil) " !important")} ;; "#ff4aff !important"

         ;; Scrollbar corner
     ".cm-s-rvbbit-dynamic ::-webkit-scrollbar-corner" {:background "rgba(0,0,0,0) !important", "background-color" "rgba(0,0,0,0) !important"}
         ;; Matching bracket styles
     "div.CodeMirror span.CodeMirror-matchingbracket" {:color (str (theme-pull :theme/editor-outer-rim-color nil) " !important") ;; "#ff4aff !important"
                                                       :font-weight "bold"}

      ".CodeMirror-hints" {:position "absolute"
                           :z-index "1000000"
                           :overflow "hidden"
                           :list-style "none"
                           :margin "0"
                           :padding "2px"
                           ;:"-webkit-box-shadow" "2px 3px 5px rgba(0, 0, 0, 0.2)"
                           ;:"-moz-box-shadow" "2px 3px 5px rgba(0, 0, 0, 0.2)"
                           :box-shadow "2px 3px 5px rgba(0, 0, 0, 0.2)"
                           :background "white"
                           :font-family (theme-pull :theme/monospaced-font nil) ;;"monospace"
                           ;:font-size "0.9em"
                           ;:max-height "20em"
                           :overflow-y "auto"}

      ".CodeMirror-hint" {:padding "0 4px"
                          :white-space "pre"
                          :overflow "hidden"
                          ;:color "red"
                          :cursor "pointer"
                          ;:background "black"
                          }

      ".CodeMirror-hint-active" {:background-color (theme-pull :theme/editor-outer-rim-color nil)
                                 :color (ut/invert-hex-color (theme-pull :theme/editor-outer-rim-color nil))}


     }))

;;(ut/apply-theme (code-mirror-theme))

(def nff
  (NumberFormat. Format/DECIMAL))

(defn nf
  [num]
  (.format nff (str num)))

(defn num-format-int [num] (.format (NumberFormat. Format/INTEGER) (str num)))

(re-frame/reg-event-db ::shift-down (fn [db [_]] (if (not (get db :shift?)) (assoc db :shift? true) db)))
(re-frame/reg-event-db ::shift-up (fn [db [_]] (assoc db :shift? true)))
(re-frame/reg-sub ::shift? (fn [db [_]] (get db :shift? false)))

(defn flatten-values [data]
  (cond
    (map? data) (mapcat flatten-values (vals data))
    (vector? data) (mapcat flatten-values data)
    :else [data]))

(defn get-all-values [data]
  (vec (flatten-values data)))

;; (re-frame/reg-sub
;;  ::flow-refs
;;  (fn [db {:keys [tab]}] 
;;    (let [pmaps (into {}
;;                      (for [[k v] (get db :panels)
;;                            :when (if tab 
;;                                    (= (get v :tab) tab)
;;                                    true)] {k v}))
         
;;          ])))

(re-frame/reg-sub
 ::get-flow-subs
 (fn [db _]
   (let [create-runner-listeners            (fn [obody] (let [kps       (ut/extract-patterns obody :run-flow 2)
                                                              logic-kps (into {} (for [v kps]
                                                                                   (let [[_ that] v]
                                                                                     {v (keyword (str "flow/" (first that) ">*running?"))})))]
                                                          (ut/postwalk-replacer logic-kps obody)))
         runstream-refs (vec (distinct (filter #(cstr/starts-with? (str %) ":flow/") (ut/deep-flatten (get db :runstreams)))))
         runstreams (vec (for [{:keys [flow-id]}
                               ;;@(ut/tracked-subscribe [::runstreams])
                               @(rfa/sub ::runstreams)] (keyword (str "flow/" flow-id ">*running?"))))
        ;;  tab (get db :selected-tab)
        ;;  panels-map (into {}
        ;;                   (for [[k v] (get db :panels)
        ;;                         :when (= (get v :tab) tab)] {k v}))
         panels-map (get db :panels) ;; this could get funky... TODO since we don't want to declick other tab
         panels (create-runner-listeners panels-map)
         drop-refs (vec (distinct (vals @drop-last-tracker-refs)))
         sflow (get db :selected-flow)
         current-flow-open (when (ut/ne? sflow) (keyword (str "flow/" sflow ">*running?")))
         flow-refs (vec (distinct
                         (conj (filter #(or (cstr/starts-with? (str %) ":flow/")
                                            (cstr/starts-with? (str %) ":screen/")
                                            (cstr/starts-with? (str %) ":time/")
                                            (cstr/starts-with? (str %) ":signal/")
                                            (cstr/starts-with? (str %) ":server/")
                                            (cstr/starts-with? (str %) ":ext-param/")
                                            (cstr/starts-with? (str %) ":solver/")
                                            (cstr/starts-with? (str %) ":solver-meta/")
                                            (cstr/starts-with? (str %) ":data/")
                                            (cstr/starts-with? (str %) ":signal-history/")
                                            (cstr/starts-with? (str %) ":panel/")
                                            (cstr/starts-with? (str %) ":client/"))
                                       (filter keyword?
                                               (into
                                                (ut/deep-flatten panels)
                                                (ut/deep-flatten (get-all-values (get db :click-param))) ;; do we have params in params?
                                                )))
                               current-flow-open)))
         warren-item (get db :selected-warren-item)
         solver-open? (and (= (get @db/flow-editor-system-mode 0) "signals") ;; signals tab selected
                           (get db :flow?) ;; flow panel open
                           )
         solver (if (and (get-in db [:solvers-map warren-item :data]) solver-open?)
                  [(keyword (str "solver/" (ut/replacer (str warren-item) ":" "")))
                   (keyword (str "solver-meta/" (ut/replacer (str warren-item) ":" "") ">extra"))
                   (keyword (str "solver-meta/" (ut/replacer (str warren-item) ":" "") ">output"))
                   (keyword (str "solver-meta/" (ut/replacer (str warren-item) ":" "") ">error"))
                   (keyword (str "solver-meta/" (ut/replacer (str warren-item) ":" "") ">history"))] [])
         solvers (if (and solver-open? (some #(= % "solvers") @db/selectors-open))
                   (vec (for [ss (keys (get db :solvers-map))] (keyword (str "solver-meta/" (ut/replacer (str ss) ":" "") ">extra")))) [])
         signals-mode?  (and (= (get @db/flow-editor-system-mode 0) "signals") ;; signals tab selected
                             (get db :flow?) ;; flow panel open
                             ;(some #(= % "signals") @db/selectors-open)
                             ) ;; signal subbox open
         signals-box-open? (some #(= % "signals") @db/selectors-open)
         signal-ui-refs (when signals-box-open? (vec (for [ss (keys (get db :signals-map))] (keyword (str "signal/" (ut/replacer (str ss) ":" ""))))))
         signal-ui-part-refs (when (and warren-item signals-mode?)
                               (let [pps (ut/where-dissect (get-in db [:signals-map warren-item :signal]))]
                                 (vec (for [pp pps]
                                        (keyword (str "signal/part-"
                                                      (ut/replacer (str warren-item) ":" "") "-"
                                                      (.indexOf pps pp)))))))
         signal-hist (if (and
                          (get-in db [:signals-map warren-item :signal])
                          @db/signal-history-ticker?  ;; unless the ticker is turned off by the user...
                          signals-mode?)
                       [(keyword (str "signal-history/" (ut/replacer (str warren-item) ":" "")))] [])

         signal-subs (if signals-mode? (vec (into signal-hist (into signal-ui-refs signal-ui-part-refs))) [])
         ;;_ (when (ut/ne? signal-subs) (ut/tapp>> [:signal-subs  signal-subs]))
        ;; _ (ut/tapp>> [:signal-subs signal-subs])

         theme-refs (vec (distinct (filter #(cstr/starts-with? (str %) ":flow/") (filter keyword? (ut/deep-flatten (get-in db [:click-param :theme]))))))]
     ;(ut/tapp>> [:flow-refs flow-refs])
     ;;(vec (distinct (into signal-subs (into drop-refs (into runstream-refs (into runstreams (into theme-refs flow-refs)))))))
     (filterv
      #(not (cstr/includes? (str %) "||")) ;; live running subs ignore
      (vec (distinct (concat signal-subs solver solvers drop-refs runstream-refs runstreams theme-refs flow-refs))))
     )))


(re-frame/reg-sub
 ::stale-flow-subs?
 (fn [db _]
   (let [;;new @(ut/tracked-subscribe [::get-flow-subs])
         new @(rfa/sub ::get-flow-subs {})
         old (get db :flow-subs [])
         deads (vec (cset/difference (set old) (set new)))
         deads (filterv #(not (cstr/includes? (str %) "||")) deads)] ;; dont mess with running-sub pseudo flows

    ;;  (when (ut/ne? deads)
    ;;    (ut/tapp>> [:unsub-check (get db :client-name) {:new new :old old :deads deads}]))
     (ut/ne? deads))))

(re-frame/reg-event-db
 ::unsub-to-flows
 (fn [db _]
   (let [;;new @(ut/tracked-subscribe [::get-flow-subs])
         new @(rfa/sub ::get-flow-subs {})
         old (get db :flow-subs [])
         old (filterv #(not (cstr/includes? (str %) "||")) old)
         deads (vec (cset/difference (set old) (set new)))
         deads (filterv #(not (cstr/includes? (str %) "||")) deads)  ;; dont mess with running-sub pseudo flows
         _ (ut/tapp>> [:flow-unsub-change! (get db :client-name) {:old old :new new :removing deads}])
         ;;_ (ut/tapp>> [:deads-exec! deads (get db :client-name)])
         ]
     (doall
      (doseq [n deads]
        (ut/tracked-dispatch [::http/unsub-to-flow-value n])
        (ut/tracked-dispatch [::conn/declick-parameter (vec (map keyword (ut/splitter (ut/replacer (str n) ":" "") #"/")))]) ;; remove from click-param altogether
        ))
     (assoc db :flow-subs new))))


(re-frame/reg-sub
 ::get-new-flow-subs
 (fn [db _]
   (let [;;flow-refs @(ut/tracked-subscribe [::get-flow-subs])
         flow-refs @(rfa/sub ::get-flow-subs {})
         subbed (get db :flow-subs [])
         subbed (filterv #(not (cstr/includes? (str %) "||")) subbed)]  ;; dont mess with running-sub pseudo flows
    ;;  (when (or (= (get db :client-name) :independent-linear-swallow-39)
    ;;            ;(= (get db :client-name) :restored-gray-dove-32)
    ;;            )
    ;;    (ut/tapp>> [:new-flow-subs (ut/ne? (vec (cset/difference (set flow-refs) (set subbed)))) flow-refs
    ;;           (get db :flow-subs [])
    ;;           subbed
    ;;           (vec (cset/difference (set flow-refs) (set subbed)))]))
     (vec (cset/difference (set flow-refs) (set subbed))))))

(re-frame/reg-sub
 ::new-flow-subs?
 (fn [_ _]
   (ut/ne?
    ;;@(ut/tracked-subscribe [::get-new-flow-subs])
    @(rfa/sub ::get-new-flow-subs {})
    )
  ;;  (not= @(ut/tracked-subscribe [::get-new-flow-subs])
  ;;        (get db :flow-subs))
   ))

(re-frame/reg-sub
 ::alerts
 (fn [db _]
   (get db :alerts)))

;(ut/tapp>> [:flow-refs @(ut/tracked-subscribe [::get-flow-subs])])
;(ut/tapp>> [:new-flow-refs @(ut/tracked-subscribe [::get-new-flow-subs])])

(re-frame/reg-event-db
 ::sub-to-flows
 (fn [db _]
   (let [;;new @(ut/tracked-subscribe [::get-new-flow-subs])
         new @(rfa/sub ::get-new-flow-subs {})
         old (get db :flow-subs [])
         old (filterv #(not (cstr/includes? (str %) "||")) old)
        ;;  _ (ut/tapp>> [:flow-sub-change! (get db :client-name) {:old old :new new}])
         subbed (vec (for [n new]
                       (do (ut/tracked-dispatch [::http/sub-to-flow-value n])
                           n)))]
     (assoc db :flow-subs (into subbed old)))))

(re-frame/reg-event-db
 ::sub-to-flows-all ;; temp debugging
 (fn [db _]
   (let [;new @(ut/tracked-subscribe [::get-new-flow-subs])
         all (get db :flow-subs [])
        ;;  subbed (vec (for [n all]
        ;;                (do (ut/tracked-dispatch [::http/sub-to-flow-value n])
        ;;                    n)))
         ]
     (doseq [n all] (ut/tracked-dispatch [::http/sub-to-flow-value n]))
     ;(assoc db :flow-subs (into subbed (get db :flow-subs [])))
     db
     )))

;(ut/tracked-dispatch [::sub-to-flows])


(re-frame/reg-sub
 ::runstream-running?
 (fn [db _]
   (let [selected-block @(ut/tracked-subscribe [::selected-block])
         selected-view @(ut/tracked-subscribe [::editor-panel-selected-view])
         kp (vec (flatten [selected-block selected-view]))
         mode (get @db/chat-mode kp (if
                                     (= "none!" (first kp))
                                     :runstreams
                                     :history))]
     (and (get db :buffy?)
          (= mode :runstreams)
          (true? (some true? (for [[k v] (get-in db [:click-param :flow] {})
                                   :when (cstr/ends-with? (str k) ">*running?")]
                               v)))))))

(re-frame/reg-sub
 ::runstreams-running
 (fn [db _]
   (count (filter true? (for [[k v] (get-in db [:click-param :flow] {})
                              :when (cstr/ends-with? (str k) ">*running?")]
                          v)))))

(re-frame/reg-sub
 ::runstreams-running-list
 (fn [db _]
   (for [[k v] (get-in db [:click-param :flow] {})
         :when (and (true? v) (cstr/ends-with? (str k) ">*running?"))]
     (keyword (-> (str k) (ut/replacer ">*running?" "") (ut/replacer ":" ""))))))

;; (re-frame/reg-event-db
;;  ::refresh-runstreams
;;  (fn [db [_]]
;;    (ut/tapp>> [:tt :refresh-runstreams])
;;    (dissoc db :runstreams-lookups)))

(re-frame/reg-event-db
 ::runstream-item
 ;;(undoable)
 (fn [db [_ result]]
   (-> db
       (assoc-in [:runstreams-lookups (get result :flow-id) :open-inputs] (get result :open-inputs))
       (assoc-in [:runstreams-lookups (get result :flow-id) :blocks] (get result :blocks)))))

(re-frame/reg-event-db
 ::refresh-runstreams
 ;;(undoable)
 (fn [db _]
   ;(ut/tapp>> [:refresh-runstream-panel (keys (get db :runstreams))])
   (when  (ut/ne? (keys (get db :runstreams)))
     (doseq [flow-id (keys (get db :runstreams))]
       (ut/tracked-dispatch [::wfx/request :default
                           {:message    {:kind :get-flow-open-ports
                                         :flow-id flow-id
                                         :flowmap flow-id
                                         :client-name (get db :client-name)}
                            :on-response [::runstream-item]
                            ;:on-timeout [::http/timeout-response]
                            :timeout    500000}])))
   db))

(re-frame/reg-sub
 ::workspace
 (fn [db [_ keypath]]
   (let [merged-keypath (cons :panels keypath)]
     (get-in db merged-keypath))))

(re-frame/reg-sub
 ::workspace-alpha
 (fn [db {:keys [keypath]}]
   (let [merged-keypath (cons :panels keypath)]
     (get-in db merged-keypath))))

(re-frame/reg-sub
 ::what-tab
 (fn [db [_ panel-key]]
   (get-in db [:panels panel-key :tab] "")))

(re-frame/reg-sub
 ::reco-running?
 (fn [db [_ query-key task-id]]
   ;(= :started (get-in db [:reco-status query-key 0]))
   (= :started (get-in db [:status task-id query-key]))))

(re-frame/reg-sub
 ::reco-queued?
 (fn [db [_ query-key task-id]]
   ;(= :queued (get-in db [:reco-status query-key 0]))
   (= :queued (get-in db [:status task-id query-key]))))

(re-frame/reg-event-db
 ::set-reco-queued
 (fn [db [_ query-key task-id]]
   ;(assoc-in db [:reco-status query-key 0] :queued)
   (assoc-in db [:status task-id query-key] :queued)))

(re-frame/reg-sub
 ::reco-count
 (fn [db [_ query-key task-id]]
   ;(get-in db [:reco-status query-key 1])
   (get-in db [:status-data task-id query-key :reco-count] nil)))
   ;-1


(re-frame/reg-sub
 ::is-layout?
 (fn [db [_ panel-key view-key]]
   (true? (some #(= % :layout)
                (ut/deep-flatten (get-in db [:panels panel-key :views view-key] []))))))

(re-frame/reg-sub
 ::is-layout-selected-and-scrubber?                        ;; editor
 (fn [db _]
   (let [panel-key    (get db :selected-block)
         view-key     (get @db/data-browser-query panel-key)
         scrubber-on? true]                                 ; (get-in @db/scrubbers [panel-key view-key] false)

      ;(ut/tapp>> [:db panel-key view-key scrubber-on?])

     (and (true? scrubber-on?)
          (true? (some #(= % :layout)
                       (ut/deep-flatten (get-in db [:panels panel-key :views view-key] []))))))))

(re-frame/reg-sub
 ::snapshots
 (fn [db _]
   (get-in db [:snapshots :params] {})))

;; (re-frame/reg-event-db
;;  ::resub!
;;  (fn [db _]
;;    (ut/tapp>> [:initiating-resub-for (get db :client-name)])
;;    (let [client-name (get db :client-name)
;;          new-client-name (ut/gen-client-name) ;; (keyword (str (ut/replacer (str client-name) ":" "") "-resub*-" (rand-int 12345)))
;;          ]

;;      (ut/tracked-dispatch [::wfx/unsubscribe http/socket-id :server-push2])
;;      (ut/tracked-dispatch [::wfx/disconnect http/socket-id])

;;      (ut/tracked-dispatch [::set-client-name new-client-name]) ;; helps?
;;      (ut/tapp>> [:initiating-resub-for-new new-client-name])

;;      (ut/tracked-dispatch [::wfx/connect http/socket-id (http/options client-name)])
;;      (ut/tracked-dispatch [::wfx/subscribe   http/socket-id :server-push2 (http/subscription client-name)])
;;      (ut/tapp>> [:initiating-resub-for-new new-client-name :done?])
;;      db)))

;; (re-frame/reg-sub
;;  ::lost-server-connection?
;;  (fn [db _]
;;    (let [last-heartbeat (get-in db [:status-data :heartbeat :kick :data 0 :at] "none!") ;;; returns "2024-04-14 05:46:43"
;;          last-heartbeat-time (js/Date. last-heartbeat)
;;          three-minutes-ago (js/Date. (- (js/Date.now) (* 3 60 1000)))]
;;      (< last-heartbeat-time three-minutes-ago))))

;; (re-frame.core/reg-event-fx
;;  ::resub!-fx ;; test fx version 2/4/24
;;  (fn [{:keys [db]} _]
;;    (ut/tapp>> [:initiating-resub-for (get db :client-name)])
;;    (let [client-name (get db :client-name)]
;;      {:db db
;;       :dispatch-n [[::wfx/unsubscribe http/socket-id :server-push2]
;;                    [::wfx/subscribe   http/socket-id :server-push2 (http/subscription client-name)]]})))

(defn sql-alias-replace-sub [query]
  ;;@(ut/tracked-subscribe [::sql-alias-replace-sub query])
  @(rfa/sub ::sql-alias-replace-sub {:query query})
  ;;@(ut/tracked-sub [::sql-alias-replace-sub {:query query}])
  )

(re-frame.core/reg-event-fx
 ::save
 (fn [{:keys [db]} _]
   ;(ut/tapp>> [:saving? 1])
   (let [flow-id @(ut/tracked-subscribe [:rvbbit-frontend.flows/selected-flow])
         flowmaps @(ut/tracked-subscribe [:rvbbit-frontend.flows/flowmap-raw])
         flowmaps-connections @(ut/tracked-subscribe [:rvbbit-frontend.flows/flowmap-connections])
         screen-name (get db :screen-name)
         resolved-queries (into {} (for [[qk q] (apply merge (for [[_ p] (get db :panels)] (get p :queries)))] {qk (sql-alias-replace-sub q)}))
        ;;  resolved-queries (for [[qk q] (apply merge (for [[_ p] (get db :panels)] (get p :queries)))] {qk q})
         flow? (get db :flow? false)
         ttype (if flow? :flow :screen)]
     (ut/tapp>> [:saving ttype])
     (ut/tracked-dispatch [::http/insert-alert [:box :child (str "sent " (ut/replacer (str ttype) ":" "") " save request to server... Please wait.") :style {:font-size "12px" :opacity 0.66}] 11 0.5 6])
     (if flow?
       {;;:db db ;; if ommitted, it is IMPLIED that we are not modding the app-db at all (but could always mutate it here)
        :dispatch [::http/save-flow
                   {:flowmaps flowmaps
                    :zoom @db/pan-zoom-offsets
                    :opts (get-in db [:flows (get db :selected-flow) :opts]
                                  {:retry-on-error? false :retries 5 :close-on-done? true})
                    :flow-id flow-id
                    :flowmaps-connections flowmaps-connections}
                   flow-id]
        :dispatch-later [{:ms 3000, :dispatch [::conn/clear-query-history :flows-sys]}]}

       {:dispatch [::http/save :skinny screen-name resolved-queries]
        :dispatch-later [{:ms 100 :dispatch [::take-screenshot true]}]
        }
       ))))

(re-frame/reg-sub
 ::current-params ;; for snapshot comparisons
 (fn [db _]
   (let [panels (get db :panels)
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
                                   {b (get-in db b)}))]
     {:params (get db :click-param {})
      :block-states block-states-vals
      :selected-tab selected-tab})))

(re-frame/reg-event-db
 ::swap-snapshot
 (undoable)
 (fn [db [_ key]]
   (let [new-params (get-in db [:snapshots :params key :params])
         block-states (get-in db [:snapshots :params key :block-states])
         curr-tab (get-in db [:snapshots :params key :selected-tab])
         extra? (get-in db [:snapshots :params key :extra?])]
     (if extra? ;; apply saved block state of bool
       (-> db
           (assoc :click-param new-params)
           (assoc :selected-tab curr-tab)
           (ut/update-multiple-if-exists block-states))
       (-> db
           (assoc :click-param new-params)
           (assoc :selected-tab curr-tab))))))

(re-frame/reg-sub
 ::matching-snapshots
 (fn [db _]
   (let [snaps (get-in db [:snapshots :params] {})
         ;curr @(ut/tracked-subscribe [::current-params])
         curr @(rfa/sub ::current-params {})
         ]
     (for [[k v] snaps
           :let [block-states (get v :block-states)
                 valid-state-keys (vec (cset/intersection ;; since we are diffing, we only want state keys that exist in both
                                        (set (keys block-states)) ;; otherwise we get a diff for every key - missing blocks, new blocks, etc
                                        (set (keys (get curr :block-states)))))] ;; which are irrelevant to the user]
           :when (= (assoc (select-keys v [:params]) :block-states (select-keys (get v :block-states) valid-state-keys))
                    (dissoc (assoc curr :block-states (select-keys (get curr :block-states) valid-state-keys)) :selected-tab))]
                    ;curr
          k))))

(re-frame/reg-sub
 ::current-tab-queries
 (fn [db _]
   (let [blocks (try
                  (map name
                       (keys (into {} (for [[_ v]
                                            (into {} (filter #(= (get db :selected-tab) (get (val %) :tab "")) (get db :panels)))]
                                        (get v :queries)))))
                  (catch :default _ ["error current tab queries"]))]
     (or blocks []))))

(re-frame/reg-sub
 ::current-tab-condis
 (fn [db _]
   (let [blocks (try
                  (map name
                       (keys (into {} (for [[_ v]
                                            (into {} (filter #(= (get db :selected-tab) (get (val %) :tab "")) (get db :panels)))]
                                        (get v :conditionals)))))
                  (catch :default _ ["error current tab condis"]))]
     (or blocks []))))

(re-frame/reg-sub
 ::current-tab-blocks
 (fn [db _]
   (let [blocks (try (map name (keys (into {} (filter #(= (get db :selected-tab) (get (val %) :tab "")) (get db :panels)))))
                     (catch :default _ []))]
     (or blocks []))))

(re-frame/reg-sub
 ::same-tab?
 (fn [db [_ panel-key1 panel-key2]]
   (= (get-in db [:panels panel-key1 :tab] "")
      (get-in db [:panels panel-key2 :tab] ""))))

(defn partition-keywords [keywords]
  (let [str-keywords (map str keywords)]
    (group-by #(cstr/includes? % "||") str-keywords)))

(re-frame/reg-sub
 ::partitioned-subs ;; in order to abuse re-frame subscription caching
 (fn [db _]
   (partition-keywords (get db :flow-subs []))))

(re-frame/reg-sub
 ::flow-watcher-subs
 :<- [::partitioned-subs]
 (fn [subs _]
   (get subs true [])))

(re-frame/reg-sub
 ::server-subs
 :<- [::partitioned-subs]
 (fn [subs _]
   (get subs false [])))

(re-frame/reg-sub
 ::flow-watcher-subs-grouped
 :<- [::flow-watcher-subs]
 (fn [subs _]
   (if (empty? subs)
     {}
     (->> subs
          (map #(second (ut/splitter (str %) "||")))
          frequencies))))

(re-frame/reg-sub
 ::tabs
 (fn [db]
   (let [seen-tabs (distinct (for [[_ pv] (get db :panels)] (get pv :tab))) ;; prevents orpahned blocks
         explicit-tabs (get db :tabs [])
         tabs (vec (remove empty? (distinct (into explicit-tabs seen-tabs))))]
     tabs)))

(re-frame/reg-sub
 ::visible-tabs
 (fn [db]
   (let [;tabs @(ut/tracked-subscribe [::tabs])
         tabs @(rfa/sub ::tabs {})
         hidden-tabs (get db :hidden-tabs [])]
     (vec (cset/difference (set tabs) (set hidden-tabs))))))

(re-frame/reg-sub
 ::hidden-tabs
 (fn [db]
   (vec (get db :hidden-tabs []))))

(re-frame/reg-event-db
 ::toggle-tab-visibility
 (undoable)
 (fn [db [_ tab-name]]
   (let [tabs (get db :hidden-tabs []) ;@(ut/tracked-subscribe [::tabs])
         exists? (some #(= % tab-name) tabs)]
     (if exists?
       (assoc db :hidden-tabs (vec (remove #(= % tab-name) tabs)))
       (assoc db :hidden-tabs (vec (conj tabs tab-name)))))))

(re-frame/reg-sub
 ::panel-keys
 (fn [db]
   (vec (keys (get db :panels {})))))

(re-frame/reg-sub
 ::selected-tab
 (fn [db]
   (get db :selected-tab (first (get db :tabs)))))

(re-frame/reg-event-db
 ::delete-tab
 (undoable)
 (fn [db [_ tab-name]]
   (let [tabs     (get db :tabs)
         curr-idx (.indexOf tabs tab-name)
         new-tabs (vec (remove #(= % tab-name) (get db :tabs)))
         new-curr (nth (cycle new-tabs) curr-idx)]
     (ut/tapp>> [:boo curr-idx new-curr])
     (-> db
         (assoc :tabs (vec (remove #(= % tab-name) (get db :tabs))))
         (assoc :panels (into {} (filter #(not (= tab-name (get (val %) :tab))) (get db :panels))))))))

(re-frame/reg-event-db
 ::rename-tab
 (fn [db [_ old new]]
    ;(ut/tapp>> [:panels (update-nested-tabs (get db :panels) old new)
    ;       (get db :tabs)
    ;       (map (fn [item] (if (= item old) new item)) (get db :tabs))])
   (-> db
       (assoc :panels (ut/update-nested-tabs (get db :panels) old new))
       (assoc :tabs (vec (map (fn [item] (if (= item old) new item)) (get db :tabs))))
       (assoc :selected-tab new))))

(re-frame/reg-event-db
 ::select-tab
 (fn [db [_ tab]]
   (-> db
       (assoc :selected-tab tab)
       (assoc-in [:click-param :sys :selected-tab] tab)
       (assoc-in [:click-param :sys :selected-tab-idx]
                 (try (.indexOf (get db :tabs) tab) (catch :default _ -1))))))

(re-frame/reg-event-db
 ::add-tab
 (undoable)
 (fn [db [_ new-tab]]
   (assoc db :tabs (conj (vec (get db :tabs)) new-tab))))

(re-frame/reg-sub
 ::editor?
 (fn [db]
   (get db :editor?)))

(re-frame/reg-sub
 ::buffy?
 (fn [db]
   (get db :buffy?)))

(re-frame/reg-sub
 ::flow?
 (fn [db]
   (get db :flow?)))

(re-frame/reg-sub
 ::external?
 (fn [db]
   (get db :external? false)))

(re-frame/reg-sub
 ::lines?
 (fn [db]
   (get db :lines? false)))

(re-frame/reg-sub
 ::full-no-ui?
 (fn [db]
   (get db :no-ui? false)))

(re-frame/reg-sub
 ::peek?
 (fn [db]
   (get db :peek? false)))

(re-frame/reg-sub
 ::flow-editor?
 (fn [db]
   (get db :flow-editor? false)))

(re-frame/reg-sub
 ::auto-run?
 (fn [db]
   (get db :auto-run? false)))

(re-frame/reg-sub
 ::auto-run-and-connected?
 (fn [db]
   (and (= (get-in db [:websocket-fx.core/sockets :default :status]) :connected)
        (get db :auto-run? false))))

(re-frame/reg-sub
 ::bg-status?
 (fn [_]
   (and @db/auto-refresh?                                  ;; auto-refresh killswitch bool
        (or (= @db/editor-mode :status)
            (= @db/editor-mode :vvv)
            (= @db/editor-mode :meta)))))

(re-frame/reg-sub
 ::update-flow-statuses?
 (fn [db]
   (and (get db :flow? false)
        (= (get @db/flow-editor-system-mode 0) "flows running")
        (get db :flow-editor? false))))

(re-frame/reg-event-db
 ::flow-statuses-response
 (fn [db [_ ss]]
   ;(ut/tapp>> [:flow-statuses-in ss])
   (assoc db :flow-statuses ss)))

(re-frame/reg-sub
 ::flow-statuses
 (fn [db]
   (get db :flow-statuses {})))

(re-frame/reg-sub
 ::condi-valves
 (fn [db [_ flow-id]] ;; just for reactionary purposes
   (get-in db [:flow-results :condis flow-id] {})))

(re-frame/reg-sub
 ::condi-valve-open?  ;; TODO needs bid
 (fn [db [_ flow-id bid condi-key]]
   (let [fck (ut/replacer (str condi-key) ":" "")
         bid (ut/replacer (str bid) ":" "")
         conns (get-in db [:flows flow-id :connections])
         conn (for [[c1 c2] conns
                    :when (and ;(cstr/ends-with? (str c1) (str "/" fck))
                           (= c1 (keyword (str bid "/" fck))))]
                               ;(cstr/starts-with? (str c2) (str condi-key))

                (try (keyword (ut/replacer (str (first (ut/splitter (str c2) #"/"))) ":" ""))
                     (catch :default _ :error!)))
         kkey (first conn)]
    ;;  (ut/tapp>> [:condi-valve-open? flow-id condi-key kkey
    ;;         (get-in db [:flow-results :condis flow-id kkey] false)
    ;;         ])
     (get-in db [:flow-results :condis flow-id kkey] false))))

(re-frame/reg-sub
 ::flow-channels-open?
 (fn [db [_ flow-id]]
   (get-in db [:flow-statuses flow-id :channels-open?] false)))

(re-frame/reg-event-db
 ::update-flow-statuses
 (fn [db [_]]
   ;(ut/tapp>> [:flow-statuses-ask (get db :client-name)])
   (ut/tracked-dispatch [::wfx/request :default
                       {:message {:kind         :get-flow-statuses
                                  :client-name  (get db :client-name)}
                        :timeout 50000
                        :on-response [::flow-statuses-response]}])
   db))

(re-frame/reg-sub
 ::has-query?
 (fn [db [_ panel-key query-key]]
   (not (nil? (get-in db [:panels panel-key :queries query-key])))))

(re-frame/reg-sub
 ::col-names                                               ;; temp
 (fn [db [_]]
   (get db :col-names)))

(re-frame/reg-event-db
 ::toggle-external
 (fn [db [_]]
   (assoc-in db [:external?] (not (get-in db [:external?] false)))))

(re-frame/reg-event-db
 ::toggle-flow
 (fn [db [_]]
   (assoc-in db [:flow?] (not (get-in db [:flow?] false)))))

(re-frame/reg-event-db
 ::toggle-flow-editor
 (fn [db [_]]
   (assoc-in db [:flow-editor?] (not (get-in db [:flow-editor?] false)))))

(re-frame/reg-event-db
 ::toggle-session-modal
 (fn [db [_]]
   (assoc db :session-modal? (not (get db :session-modal? false)))))

(re-frame/reg-event-db
 ::disable-session-modal
 (fn [db [_]]
   (assoc db :session-modal? false)))

(re-frame/reg-sub
 ::session-modal?
 (fn [db]
   (get db :session-modal? false)))

(re-frame/reg-event-db
 ::toggle-flow-gantt
 (fn [db [_]]
   (assoc-in db [:flow-gantt?] (not (get-in db [:flow-gantt?] false)))))

(re-frame/reg-sub
 ::flow-gantt?
 (fn [db _]
   (get db :flow-gantt? false)))

(re-frame/reg-event-db
 ::toggle-editor
 (fn [db [_]]
   (if (get db :flow?)
     (assoc-in db [:flow-editor?] (not (get-in db [:flow-editor?] false)))
     (assoc-in db [:editor?] (not (get-in db [:editor?] false))))))

(re-frame/reg-event-db
 ::toggle-buffy
 (fn [db [_]]
   (if (get db :flow?)
     (assoc-in db [:flow-gantt?] (not (get-in db [:flow-gantt?] false)))
     (assoc-in db [:buffy?] (not (get-in db [:buffy?] false))))))

(re-frame/reg-event-db
 ::toggle-lines
 (fn [db [_]]
   (assoc-in db [:lines?] (not (get-in db [:lines?] false)))))

(re-frame/reg-event-db
 ::toggle-peek
 (fn [db [_]]
   (assoc-in db [:peek?] (not (get-in db [:peek?] false)))))

(re-frame/reg-event-db
 ::toggle-kick-alert
 (fn [db [_]]
   (reset! db/kick-alert (not @db/kick-alert))
   db))

(re-frame/reg-event-db
 ::toggle-auto-run
 (fn [db [_]]
   (assoc-in db [:auto-run?] (not (get-in db [:auto-run?] false)))))

(re-frame/reg-event-db
 ::toggle-no-ui
 (fn [db [_]]
   (assoc-in db [:no-ui?] (not (get-in db [:no-ui?] false)))))

(re-frame/reg-event-db
 ::overwrite-theme
 (undoable)
 (fn [db [_ theme-map]]
   (assoc-in db [:click-param :theme]
             (merge
              (apply dissoc (get-in db [:click-param :theme])
                     (vec (keys db/base-theme)))
              theme-map))))

(re-frame/reg-event-db
 ::add-board
 (undoable)
 (fn [db [_ board-map]]
   (ut/tapp>> [:board-map board-map])
   (-> db
       (assoc-in [:click-param :param]
                 (merge
                  (get-in db [:click-param :param])
                  (get-in board-map [:click-param :param])))
       (assoc :panels
              (merge
               (get db :panels)
               (get board-map :panels))))))
  ;;  db


(re-frame/reg-event-db
 ::refresh-meta-tables
 (fn [db [_ type]]
   (condp = type
     :vvv (-> db
              (ut/dissoc-in [:query-history :recos-sys])
              (ut/dissoc-in [:query-history :viz-shapes-sys])
              (ut/dissoc-in [:query-history :viz-shapes0-sys])
              (ut/dissoc-in [:query-history :viz-tables-sys]))
     :files (-> db
                (ut/dissoc-in [:query-history :files-sys])
                (ut/dissoc-in [:query-history :blocks-sys]))
     :meta (-> db
               (ut/dissoc-in [:query-history :tables-sys])
               (ut/dissoc-in [:query-history :fields-sys])
               (ut/dissoc-in [:query-history :connections-sys]))
     :status (-> db (ut/dissoc-in [:query-history :status-sys])))))

(re-frame/reg-event-db
 ::refresh-all
 (fn [db _]
   (-> db
       (dissoc :query-history)
       (dissoc :query-history-meta))))

(re-frame/reg-event-db
 ::set-grid-reco?
 (fn [db [_ b]]
   (assoc-in db [:grid-reco?] b)))

(re-frame/reg-sub
 ::grid-reco?
 (fn [db]
   (get db :grid-reco?)))

(re-frame/reg-event-db
 ::set-preview-keys
 (fn [db [_ v]]
   (assoc-in db [:preview-keys] v)))

(re-frame/reg-event-db
 ::clear-query
 (fn [db [_ k]]
   (ut/dissoc-in db [:data k])))

(re-frame/reg-event-db
 ::set-preview-keys2
 (fn [db [_ v]]
   (assoc-in db [:preview-keys2] v)))

(re-frame/reg-sub
 ::preview-keys
 (fn [db]
   (get db :preview-keys)))

(re-frame/reg-sub
 ::preview-keys2
 (fn [db]
   (get db :preview-keys2)))

(re-frame/reg-event-db
 ::set-recos-page
 (fn [db [_ b]]
   (assoc-in db [:recos-page] b)))

(re-frame/reg-event-db
 ::set-recos-page2
 (fn [db [_ b]]
   (assoc-in db [:recos-page2] b)))

(re-frame/reg-sub
 ::recos-page
 (fn [db]
   (get db :recos-page 0)))

(re-frame/reg-sub
 ::recos-page2
 (fn [db]
   (get db :recos-page2 20)))

(re-frame/reg-sub
 ::user-parameters
 (fn [db [_ k]]
   (get-in db [:click-param k])))

(re-frame/reg-sub
 ::parameters-used-from
 (fn [db {:keys [query-key]}]
   (let [asl (get db :panels)
         aslflat (filter #(cstr/includes? (str %) "/") (ut/deep-flatten asl))
         valid-body-params (filter #(not (and (cstr/ends-with? (str %) "-sys")
                                              (cstr/starts-with? (str %) ":theme")))
                                   (ut/deep-flatten
                                    ;;@(ut/tracked-subscribe [::valid-body-params-in asl])
                                    @(rfa/sub ::valid-body-params-in {:body asl})
                                    ))
         vbp-bools (try  (into {}
                               (for [e valid-body-params] (let [spl (ut/splitter (str e) #"/")]
                                                            {[(-> (first spl) str (ut/replacer #":" "") keyword)
                                                              (-> (last spl) str (ut/replacer #":" "") keyword)]
                                                             (true? (some #(= % e) aslflat))}))) (catch :default _ {}))
         cell-query-key (keyword (str (ut/replacer (str query-key) #":" "") ".*"))
         params (into {} (apply concat
                                (for [[p v] (get-in db [:click-param])
                                      :when (and (not (= p :theme)) (not (cstr/ends-with? (str p) "-sys")))]
                                  (for [e (keys v)]
                                    (let [;asl @(ut/tracked-subscribe [::all-sql-calls])
                                          comb-key (keyword (str (ut/safe-name p) "/" (ut/safe-name e)))]
                                      {[p e] (true? (some #(= % comb-key) aslflat))})))))
         both-maps (into {} (for [[[k1 k2] b] (merge params vbp-bools)
                                  :let [k2 (if (cstr/ends-with? (str k1) ".*") [k2 (get-in db [:click-param cell-query-key k2])] k2)]]
                              {[k1 k2] b}))
        ;;  matchers2 (try
        ;;             (vec (map last (keys (filter (fn [[[k1 _] v]]
        ;;                                            (and v (or (= k1 query-key) ;; base clicks and refs
        ;;                                                       (= k1 cell-query-key)))) ;; multiple cell clicks
        ;;                                          both-maps))))
        ;;             (catch :default _ []))
         matchers (vec (map last (keys (filter #(and (or (last %) (vector? (last (first %)))) (cstr/starts-with? (str (first (first %))) (str query-key))) both-maps))))]
     ;;;(ut/tapp>> [:vbp cell-query-key query-key both-maps matchers])
     matchers)))

(re-frame/reg-event-db
 ::set-user-parameters
 (fn [db [_ k v]]
   (assoc-in db [:click-param k] v)))

(re-frame/reg-event-db
 ::set-client-name
 (fn [db [_ client-name]]
   (set! (.-title js/document) (str "Rabbit (" client-name ")"))
   (assoc-in db [:client-name] client-name)))

(re-frame/reg-sub
 ::client-name
 (fn [db]
   (get db :client-name)))

(re-frame/reg-sub
 ::last-heartbeat
 (fn [db]
   (get-in db [:status-data :heartbeat :kick :data 0 :at] "none!")))

(re-frame/reg-event-db
 ::set-screen-name
 (fn [db [_ screen-name]]
   (http/change-url (str "/" screen-name))
   (assoc-in db [:screen-name] (str screen-name))))

(re-frame/reg-sub
 ::screen-name
 (fn [db]
   (get db :screen-name (ut/safe-name (get db :client-name)))))

; sql-data keypath honey-sql

(re-frame/reg-event-db
 ::dispatch-keepalive
 (fn [db [_]]
   (do (conn/sql-data [:keepalives] {:select [(rand-int 123433333335)] :limit [(rand-int 123333333345)]})
       (assoc-in db [:keepalives] (+ (get-in db [:keepalives] 0) 1)))))

(re-frame/reg-event-db
 ::refresh-status
 (fn [db [_]]
    ;(-> db (ut/dissoc-in [:query-history :status-sys]))
   (let [type @db/editor-mode]
     (condp = type
       :vvv (-> db
                (ut/dissoc-in [:query-history :recos-sys])
                (ut/dissoc-in [:query-history :viz-shapes-sys])
                (ut/dissoc-in [:query-history :viz-shapes0-sys])
                (ut/dissoc-in [:query-history :viz-tables-sys]))
       :files (-> db
                  (ut/dissoc-in [:query-history :files-sys])
                  (ut/dissoc-in [:query-history :blocks-sys]))
       :meta (-> db
                 (ut/dissoc-in [:query-history :tables-sys])
                 (ut/dissoc-in [:query-history :fields-sys])
                 (ut/dissoc-in [:query-history :connections-sys]))
       :status (-> db (ut/dissoc-in [:query-history :status-sys]))))))

(re-frame/reg-event-db
 ::update-workspace
 (undoable)
 (fn [db [_ keypath value]]
   ;(ut/tapp>> [:updating (cons :panels keypath) :with value])
  ;;  (-> db
  ;;      (ut/dissoc-in [:data :history-log])
  ;;      (assoc-in (cons :panels keypath) value))
   (assoc-in db (cons :panels keypath) value)))

(re-frame/reg-event-db
 ::update-workspace-raw
 (undoable)
 (fn [db [_ keypath value]]
   (if (not (= (get-in db keypath) value))
     (do ;(ut/tapp>> [:updating keypath :with value])
         (assoc-in db keypath value))
     db)))

(re-frame/reg-event-db
 ::add-query
 (undoable)
 (fn [db [_ panel-key query-key query]]
   (ut/tapp>> [:adding-query query-key :to panel-key])
   (assoc-in db [:panels panel-key :queries query-key] query)))

(re-frame/reg-event-db
 ::add-new
 (undoable)
 (fn [db [_ panel-key type body]]
   (let [name (if (= type :queries) "new-query" "new-view")
         name (ut/safe-key (keyword name))]
     (ut/tapp>> [:adding-new type :to panel-key])
     (assoc-in db [:panels panel-key type name] body))))

(re-frame/reg-event-db
 ::update-workspace-noundo
 (fn [db [_ keypath value]]
    ;(ut/tapp>> [:updating-noundo (cons :panels keypath) :with value])
  ;;  (-> db
  ;;      (ut/dissoc-in [:data :history-log])
  ;;      (assoc-in (cons :panels keypath) value))
   (assoc-in db (cons :panels keypath) value)))

(re-frame/reg-event-db
 ::update-selected
 (undoable)
 (fn [db [_ value]]
   (ut/tapp>> [:updating-selected [:panels (get db :selected-block)] :with value])
  ;;  (-> db
  ;;      (ut/dissoc-in [:data :history-log])
  ;;      (assoc-in [:panels (get db :selected-block)] value)
  ;;      )
   (assoc-in db [:panels (get db :selected-block)] value)))

(re-frame/reg-event-db
 ::update-selected-key
 (undoable)
 (fn [db [_ key value]]
   (ut/tapp>> [:updating-selected [:panels (get db :selected-block) key] :with value])
   (assoc-in db [:panels (get db :selected-block) key] value)))

(re-frame/reg-event-db
 ::update-selected-key-cons
 (undoable)
 (fn [db [_ key value]]
   (let [kp (vec (into [:panels (get db :selected-block)] key))]
     (ut/tapp>> [:updating-selected2 kp :with value])
     (assoc-in db kp value))))

(re-frame/reg-event-db
 ::update-selected-field
 (undoable)
 (fn [db [_ kp value]]
   (ut/tapp>> [:updating-selected-field kp :with value])
   (assoc-in db kp value)))

(re-frame/reg-event-db
 ::clean-up-previews
 (fn [db [_]]
   (let [pp (doall (for [r (filter #(cstr/starts-with? (str %) ":reco-preview") (keys (get-in db [:panels])))] r))
         pl (keys (get db :panels))
         p0 (cset/difference (set pl) (set pp))]
      ;(ut/tapp>> p0)
      ;(doseq [p pp] (ut/dissoc-in db p))
     (assoc db :panels (select-keys (get db :panels) p0)))))

(re-frame/reg-event-db
 ::delete-selected-panel
 (undoable)
 (fn [db _]
   (ut/tracked-dispatch [::delete-panel (get db :selected-block)])
   db))

(re-frame/reg-event-db
 ::delete-panel
 (undoable)
 (fn [db [_ & [panel-key]]]
   (let [panel-key (if (nil? panel-key) (get db :selected-block) panel-key) ;; if nil delete selected (for shift-D)
         dying-queries  (get-in db [:panels panel-key :queries])
         salute-sailors (into {} (for [[k v] dying-queries]
                                   {(keyword (str "query/" (ut/safe-name k)))
                                    ;; (-> v
                                    ;;     (dissoc :page)
                                    ;;     (dissoc :col-widths)
                                    ;;     (dissoc :row-height)
                                    ;;     (dissoc :render-all?)
                                    ;;     (dissoc :refresh-every)
                                    ;;     (dissoc :deep-meta?)
                                    ;;     (dissoc :clicked-row-height)
                                    ;;     (dissoc :cache?))
                                    (ut/clean-sql-from-ui-keys v)}))]
      ;; replace all :query aliases in other panels with ones being deleted before dropping
     (if (= panel-key (get db :selected-block))

       (ut/postwalk-replacer
        salute-sailors
        (-> db
            (ut/dissoc-in [:panels panel-key])
            (assoc :selected-block "none!")))

       (ut/postwalk-replacer
        salute-sailors
        (ut/dissoc-in db [:panels panel-key]))))))

(re-frame/reg-event-db
 ::quick-delete-panel
 (fn [db [_ panel-key]]
   (ut/dissoc-in db [:panels panel-key])))

(re-frame/reg-sub
 ::lookup-panel-key-by-query-key
 (fn [db [_ query-key]]
   (first (remove nil? (for [[k v] (get db :panels)]
                         (when (some #(= query-key %)
                                     (keys (get v :queries))) k))))))

(re-frame/reg-sub
 ::repl-output
 (fn [db [_ query-key]]
   (get-in db [:repl-output query-key] nil)))

(re-frame/reg-sub
 ::lookup-connection-id-by-query-key
 (fn [db [_ query-key]]
   (get-in db [:panels (first (remove nil? (for [[k v] (get db :panels)]
                                             (when (some #(= query-key %)
                                                         (keys (get v :queries))) k)))) :connection-id])))

(re-frame/reg-event-db
 ::unalias-query
 (undoable)
 (fn [db [_ query-key]]
   (let [panel-key      (first (remove nil? (for [[k v] (get db :panels)]
                                              (when (some #(= query-key %)
                                                          (keys (get v :queries))) k))))
         dying-queries  {query-key (get-in db [:panels panel-key :queries query-key])}
         salute-sailors (into {} (for [[k v] dying-queries]
                                   {(keyword (str "query/" (ut/safe-name k)))
                                    ;; (-> v
                                    ;;     (dissoc :page)
                                    ;;     (dissoc :col-widths)
                                    ;;     (dissoc :row-height)
                                    ;;     (dissoc :render-all?)
                                    ;;     (dissoc :refresh-every)
                                    ;;     (dissoc :deep-meta?)
                                    ;;     (dissoc :clicked-row-height)
                                    ;;     (dissoc :cache?))
                                    (ut/clean-sql-from-ui-keys v)}))]
      ;; replace all :query aliases in other panels with ones being deleted before dropping
      ; (if (= panel-key (get db :selected-block))

      ;   (ut/postwalk-replacer
      ;    salute-sailors
      ;    (-> db
      ;        (ut/dissoc-in [:panels panel-key])
      ;        (assoc :selected-block "none!")))

     (ut/postwalk-replacer
      salute-sailors
        ;(ut/dissoc-in db [:panels panel-key])
      db))))                                                   ;)

(re-frame/reg-sub
 ::selected-block
 (fn [db]
   (get-in db [:selected-block] "none!")))

(re-frame/reg-sub
 ::page-num
 (fn [db [_ panel-key query-key]]
   (get-in db [:panels panel-key :queries query-key :page] 1)))

(re-frame/reg-event-db
 ::change-page
 (undoable)
 (fn [db [_ panel-key query-key new-page-num ttl-pages]]
   (let [;curr-page (get-in db [:panels panel-key :queries query-key :page] 1)
         new-page-num (cond (< new-page-num 1) 1
                            (> new-page-num ttl-pages) ttl-pages
                            :else new-page-num)]
     (assoc-in db [:panels panel-key :queries query-key :page] new-page-num))))

(re-frame/reg-sub
 ::selected-block?
 (fn [db {:keys [panel-key]}]
   (true? (= panel-key (get-in db [:selected-block] "none!")))))

(re-frame/reg-sub
 ::root
 (fn [db [_ panel-key]]
   (get-in db [:panels panel-key :root])))

(re-frame/reg-sub
 ::zindex
 (fn [db [_ panel-key]]
   (get-in db [:panels panel-key :z] 0)))

(re-frame/reg-sub
 ::size
 (fn [db [_ panel-key]]
   [(get-in db [:panels panel-key :h])
    (get-in db [:panels panel-key :w])]))

(re-frame/reg-sub
 ::selected-block-map
 (fn [db]
   (get-in db [:panels (get db :selected-block)])))

(re-frame/reg-sub ::selected-root (fn [db] (get-in db [:panels (get db :selected-block) :root])))
(re-frame/reg-sub ::selected-h (fn [db] (get-in db [:panels (get db :selected-block) :h])))
(re-frame/reg-sub ::selected-w (fn [db] (get-in db [:panels (get db :selected-block) :w])))

(re-frame/reg-event-db
 ::change-drop-type                                        ;; experiment
  ;(undoable)
 (fn [db]
   (ut/tapp>> [::change-drop-type])
   (assoc db :drop-type (rand-int 99))))

(re-frame/reg-sub
 ::drop-type                                               ;; experiment
 (fn [db]
   (get db :drop-type)))

;; (re-frame/reg-event-db
;;  ::get-memory-usage
;;  (fn [db]
;;    (let [mem (when (exists? js/window.performance.memory)
;;                [(.-totalJSHeapSize js/window.performance.memory)
;;                 (.-usedJSHeapSize js/window.performance.memory)
;;                 (.-jsHeapSizeLimit js/window.performance.memory)])
;;          mem-row {:time (str (.toISOString (js/Date.)))
;;                   :total (first mem)
;;                   :used (second mem)
;;                   :limit (last mem)}]
;;      (-> db
;;          (assoc-in [:data :memory-sys] (vec (conj (get-in db [:data :memory-sys] []) mem-row )))
;;          (assoc :memory mem)))))

(re-frame/reg-event-db
 ::get-memory-usage
 (fn [db]
   (let [mem (when (exists? js/window.performance.memory)
               [(.-totalJSHeapSize js/window.performance.memory)
                (.-usedJSHeapSize js/window.performance.memory)
                (.-jsHeapSizeLimit js/window.performance.memory)])
        ;;  mem-row {:time (str (.toISOString (js/Date.)))
        ;;           :total (first mem)
        ;;           :used (second mem)
        ;;           :limit (last mem)}
         ;memory-sys (vec (take-last 200 (conj (get-in db [:data :memory-sys] []) mem-row)))
         ]
    ;;  (-> db
    ;;      ;(assoc-in [:data :memory-sys] memory-sys)
    ;;      (assoc :memory mem))
     (assoc db :memory mem)
     )))

(re-frame/reg-sub
 ::memory
 (fn [db _]
   (get db :memory)))

(re-frame/reg-sub
 ::runstreams
 (fn [db _]
   (let [runstreams-map (get db :runstreams)
         runstreams-map1 {}
        ;;  runstreams-map1 {"dalle3-create-image-newengland" {:open? true}
        ;;                   "gpt35-color-parser" {:open? true}
        ;;                   "dalle3-create-image" {:open? true}}
         runstreams-map (merge runstreams-map1 runstreams-map)]
     (into [] (for [[k v] runstreams-map] (merge {:flow-id k} v))))))

(re-frame/reg-event-db
 ::add-runstream
 (undoable)
 (fn [db [_ flow-id]]
   (ut/tapp>> [:adding-flow-id-to-runstream flow-id (get db :client-name)])
   (assoc-in db [:runstreams flow-id] {:open? true})))

(re-frame/reg-event-db
 ::remove-runstream
 (undoable)
 (fn [db [_ flow-id]]
   (ut/tapp>> [:removing-flow-id-from-runstream flow-id (get db :client-name)])
   (ut/dissoc-in db [:runstreams flow-id])))

(re-frame/reg-sub
 ::runstreams-lookups
 (fn [db _]
   ;(get db :runstreams)
   (get db :runstreams-lookups)))

(re-frame/reg-sub
 ::runstream-override-map
 (fn [_ _]
   (let [;runstreams @(ut/tracked-subscribe [::runstreams])
         runstreams @(rfa/sub ::runstreams {})]
         ;runstreams (vec (filter #(get % :run-on-change?) runstreams))

     ;(ut/tapp>> [:rs-list? runstreams])
         ;runstream-keys (vec (map :flow-id runstreams))
         ;runstreams-lookups @(ut/tracked-subscribe [::bricks/runstreams-lookups])
     (into {}
           (for [{:keys [flow-id values]} runstreams
                 :let [override-map (into {}
                                          (for [[k {:keys [value source]}] values
                                                :when (not (nil? value))]
                                            {k (if (= source :input)
                                                 value
                                                 (let [;;vv @(ut/tracked-subscribe [::rs-value flow-id k])
                                                       vv @(rfa/sub ::rs-value {:flow-id flow-id :kkey k})
                                                       ]
                                                   (if (and (vector? vv) (every? string? vv))
                                                     (cstr/join "\n" vv) vv)))}))
                     ;overrides-map? (not (empty? override-map))
                       override-map (if (empty? override-map) nil override-map)]]
             {flow-id override-map})))))

(re-frame/reg-sub
 ::query
 (fn [db [_ panel-key query-key]]
   (get-in db [:panels panel-key :queries query-key])))

(re-frame/reg-sub
 ::find-query
 (fn [db [_ query-key]]
   (let [kpth (or (vec (first (filter #(= (last %) query-key) (ut/kvpaths (get db :panels))))) [:nope])]
     (get-in db (cons :panels kpth)))))

(defn safe-nth [c i] (try (nth c i) (catch :default _ nil)))

(re-frame/reg-event-db
 ::nudge-panel
 (undoable)
 (fn [db [_ dir]]
   (if (get db :flow?)
     (let [bid (get db :selected-flow-block)
           x (get-in db [:flows (get db :selected-flow) :map bid :x])
           y (get-in db [:flows (get db :selected-flow) :map bid :y])
           chunk 25
           [nx ny] (condp = dir
                     :left [(- x chunk) y]
                     :right [(+ chunk x) y]
                     :up [x (- y chunk)]
                     :down [x (+ chunk y)])]
       (-> db
           (assoc-in [:flows (get db :selected-flow) :map bid :x] nx)
           (assoc-in [:flows (get db :selected-flow) :map bid :y] ny)))

     (let [panel-id     (get db :selected-block)
           selected-tab (get-in db [:panels panel-id :selected-view])
           qkeys        (keys (get-in db [:panels panel-id :queries]))
           query-key    (cond (and (nil? selected-tab) (not (empty? qkeys))) (first qkeys)
                              (and (not (nil? selected-tab)) (some #(= % selected-tab) qkeys)) selected-tab ;(first qkeys)
                              :else nil)
           sel-col      (get db :selected-cols)
           curr-field   (when (and (= (safe-nth sel-col 0) panel-id)
                                   (= (safe-nth sel-col 1) query-key))
                          (safe-nth sel-col 2))
           col-mode?    (true? (not (nil? curr-field)))
           x            (get-in db [:panels panel-id :root 0])
           y            (get-in db [:panels panel-id :root 1])]
      ;(ut/tapp>> [:move col-mode? panel-id curr-field sel-col (= (safe-nth sel-col 0) panel-id)  (safe-nth sel-col 1) query-key])
       (if col-mode?
         (let [modfields          (get db :col-names)
               modfields?         (not (nil? modfields))
               fields             (if modfields? modfields
                                      (vec (keys (get-in db [:meta query-key :fields]))))
               field-idx          (.indexOf fields curr-field)
               selectc            (get-in db [:panels panel-id :queries query-key :select])
               fc                 (count fields)
               move-right-selectc (if (not (> (+ field-idx 1) (- fc 1)))
                                    (assoc selectc (+ field-idx 1) (get selectc field-idx) field-idx (get selectc (+ field-idx 1)))
                                    selectc)
               move-left-selectc  (if (>= (- field-idx 1) 0)
                                    (assoc selectc (- field-idx 1) (get selectc field-idx) field-idx (get selectc (- field-idx 1)))
                                    selectc)
               fields-right       (if (not (> (+ field-idx 1) (- fc 1)))
                                    (assoc fields (+ field-idx 1) (get fields field-idx) field-idx (get fields (+ field-idx 1)))
                                    fields)
               fields-left        (if (>= (- field-idx 1) 0)
                                    (assoc fields (- field-idx 1) (get fields field-idx) field-idx (get fields (- field-idx 1)))
                                    fields)]
           (cond (= dir :left)
                 (-> db
                     (assoc-in [:panels panel-id :queries query-key :select] move-left-selectc)
                     (assoc :col-names fields-left))
                 (= dir :right)
                 (-> db
                     (assoc-in [:panels panel-id :queries query-key :select] move-right-selectc)
                     (assoc :col-names fields-right))))

         (assoc-in db [:panels (get db :selected-block) :root] (condp = dir
                                                                 :left [(- x 1) y]
                                                                 :right [(+ 1 x) y]
                                                                 :up [x (- y 1)]
                                                                 :down [x (+ 1 y)])))))))

(re-frame/reg-event-db
 ::stretch-panel
 (undoable)
 (fn [db [_ dir]]
   (if (get db :flow?)
     (let [bid (get db :selected-flow-block)
           h (get-in db [:flows (get db :selected-flow) :map bid :h])
           w (get-in db [:flows (get db :selected-flow) :map bid :w])
           chunk 25]
       (condp = dir
         :wider    (assoc-in db [:flows (get db :selected-flow) :map bid :w] (+ w chunk))
         :narrower (when (>= (- w chunk) (* 4 chunk)) (assoc-in db [:flows (get db :selected-flow) :map bid :w] (- w chunk)))
         :longer   (assoc-in db [:flows (get db :selected-flow) :map bid :h] (+ h chunk))
         :shorter  (if (>= (- h chunk) (* 3 chunk))
                     (assoc-in db [:flows (get db :selected-flow) :map bid :h] (- h chunk))
                     (assoc-in db [:flows (get db :selected-flow) :map bid :h] 60))))
     (let [panel-id      (get db :selected-block)
           selected-tab  (get-in db [:panels panel-id :selected-view])
           qkeys         (keys (get-in db [:panels panel-id :queries]))
           query-key     (cond (and (nil? selected-tab) (not (empty? qkeys))) (first qkeys)
                               (and (not (nil? selected-tab)) (some #(= % selected-tab) qkeys)) selected-tab ;(first qkeys)
                               :else nil)
           sel-col       (get db :selected-cols)
           curr-field    (when (and (= (safe-nth sel-col 0) panel-id) (= (safe-nth sel-col 1) query-key))
                           (safe-nth sel-col 2))
          ;curr-field (get db :selected-cols panel-id query-key])
           def-col-width (get-in db [:default-col-widths panel-id query-key] 80)
           curr-width    (get-in db [:panels (get db :selected-block) :queries query-key :col-widths curr-field] def-col-width)
           col-mode?     (true? (not (nil? curr-field)))
          ;fields (keys (get-in db [:meta query-key :fields]))
          ;field-idx (.indexOf fields curr-field)
           w             (get-in db [:panels panel-id :w])
           h             (get-in db [:panels panel-id :h])]
       (if col-mode?
         (cond (= dir :wider) (assoc-in db [:panels (get db :selected-block) :queries query-key :col-widths curr-field]
                                        (+ curr-width 10))
               (= dir :narrower) (assoc-in db [:panels (get db :selected-block) :queries query-key :col-widths curr-field]
                                           (- curr-width 10)))
         (condp = dir
           :wider    (assoc-in db [:panels (get db :selected-block) :w] (+ w 1))
           :narrower (when (>= (- w 1) 2) (assoc-in db [:panels (get db :selected-block) :w] (- w 1)))
           :longer   (assoc-in db [:panels (get db :selected-block) :h] (+ h 1))
           :shorter  (when (>= (- h 1) 2) (assoc-in db [:panels (get db :selected-block) :h] (- h 1)))))))))

(re-frame/reg-event-db
 ::select-block
  ;(undoable)
 (fn [db [_ block-key]]
   (ut/tapp>> [:selecting-block block-key])
   (-> db                                                  ;; plus reset selected cols, just in case... (would cause user confusion possibly)
       (assoc :selected-block block-key)
       (assoc-in [:click-param :param :selected-block] block-key)
       (assoc :selected-cols nil))))

(re-frame/reg-event-db
 ::esc-unselect-current
  ;(undoable)
 (fn [db [_]]                                              ;; if col is selected, first unselect that and then on subsequent, unselect block
   (reset! mad-libs-view nil)
   (reset! db/editor-mode :meta)
   (cond
     (and (not (nil? (get db :selected-flow-block)))
          (get db :flow?))
     (assoc db :selected-flow-block nil)
     (nil? (get db :selected-cols))
     (-> db
         (assoc :selected-block "none!")
         (assoc :col-names nil)
         (assoc :selected-cols nil))
     :else (-> db
               (assoc :selected-cols nil)
               (assoc :col-names nil)))))

(re-frame/reg-event-db
 ::next-panel
 (fn [db]
   (let [flow? (get db :flow?)]
     (if flow?
       (let [panel-keys     (vec (for [[k v] (get-in db [:flows (get db :selected-flow) :map])
                                       :when (get v :x)] k))
             selected-panel (get db :selected-flow-block)
             panel-idx      (.indexOf panel-keys selected-panel)
             next-panel     (nth (cycle panel-keys) (+ panel-idx 1))]
         (ut/tapp>> [:next-flow-block :from selected-panel :to next-panel])
        ;;  (-> db
        ;;      ;(assoc-in [:click-param :param :selected-block] next-panel)
        ;;      (assoc :selected-block next-panel))
         (assoc db :selected-flow-block next-panel))
       (let [panel-keys     (filter #(and (not (cstr/starts-with? (str %) ":reco-preview"))
                                          (not (cstr/starts-with? (str %) ":query-preview")))
                                    (keys (doall (sort-by (fn [r] (let [rr (get r :root)
                                                                        [int2 int1] rr]
                                                                    (+ int1 int2)))
                                                          (into {} (filter #(and
                                                                             (not (get (val %) :minimized? false))
                                                                             (= (get db :selected-tab)
                                                                                (get (val %) :tab "")))
                                                                           (get db :panels)))))))                          ;(cycle (keys (get db :panels)))

             selected-panel (get db :selected-block)
             panel-idx      (.indexOf panel-keys selected-panel)
             next-panel     (nth (cycle panel-keys) (+ panel-idx 1))]
        ;;  (ut/tapp>> [:next-panel :from selected-panel :to next-panel])
         (-> db
             (assoc-in [:click-param :param :selected-block] next-panel)
             (assoc :selected-block next-panel)))))))

(re-frame/reg-event-fx
 ::undo-one
 (fn [_ _] (ut/tracked-dispatch [:undo])))

(re-frame/reg-event-fx
 ::redo-one
 (fn [_ _] (ut/tracked-dispatch [:redo])))

(re-frame/reg-sub
 ::build-grid
 (fn [_ {:keys [bh bw start-h start-w]}]
   (vec (for [x (range start-w bw)
              y (range start-h bh)]
          [x y]))))

(re-frame/reg-sub
 ::used-space
 (fn [_ [_ workspace bricks-wide bricks-high]]
   (vec (apply concat
               (for [[panel-key panel] workspace]
                 (let [root-x (first (:root panel))        ;(first root-vec)
                       root-y (last (:root panel))         ;(last root-vec)
                       w      (if (= (:w panel) 0) (- bricks-wide 1) (:w panel))
                       h      (if (= (:h panel) 0) (- bricks-high 1) (:h panel))]
                   (for [x (range root-x (+ root-x w))
                         y (range root-y (+ root-y h))]
                     [x y])))))))

(re-frame/reg-sub
 ::used-space2
 (fn [db [_ bricks-high bricks-wide]]
   (vec (apply concat
               (for [[panel-key panel] (get db :panels)]
                 (let [root-x (first (:root panel))        ;(first root-vec)
                       root-y (last (:root panel))         ;(last root-vec)
                       w      (if (= (:w panel) 0) (- bricks-wide 1) (:w panel))
                       h      (if (= (:h panel) 0) (- bricks-high 1) (:h panel))]
                   (for [x (range root-x (+ root-x w))
                         y (range root-y (+ root-y h))]
                     [x y])))))))

(re-frame/reg-sub
 ::used-space3
 (fn [db [_ bricks-high bricks-wide start-y end-y start-x end-x]]
   (vec (remove nil? (apply concat
                            (for [[panel-key panel] (get db :panels)]
                              (let [root-x (first (:root panel)) ;(first root-vec)
                                    root-y (last (:root panel)) ;(last root-vec)
                                    w      (if (= (:w panel) 0) (- bricks-wide 1) (:w panel))
                                    h      (if (= (:h panel) 0) (- bricks-high 1) (:h panel))]
                                (for [x (range root-x (+ root-x w))
                                      y (range root-y (+ root-y h))]
                                   ;[x y]
                                  (when (and (and (>= x start-x) (<= x end-x))
                                             (and (>= y start-y) (<= y end-y))) [x y])))))))))

(defn get-client-rect [evt]
  (let [r (.getBoundingClientRect (.-target evt))]
    {:left   (.-left r), :top (.-top r)
     :width  (.-width r) :height (.-height r)
     :bottom (.-bottom r) :right (.-right r)}))

(defn is-inside? [coord rects]
  (some (fn [[x y w h key]]
          (when (and (and (>= (first coord) x) (< (first coord) (+ x w)))
                     (and (>= (last coord) y) (< (last coord) (+ y h))))
            key))
        (vec rects)))

;; ;; Example usage
;; (is-inside? [3 3] [[2 2 3 4 :key1] [2 4 6 6 :key2]])

(re-frame/reg-sub
 ::containers
 (fn [db _]
   (vec (for [[k v] (get db :panels)
              :let [kks (ut/deep-flatten (get v :views))]
              :when (and (= (get v :tab) (get db :selected-tab))
                         (not (get v :minimized? false))
                         (not (get v :pinned? false))
                         (not (get v :icon? false)) ;; legacy test blocks
                         (not (get v :iconized? false))
                         (some #(= % :grid) kks))]
          (let [tt (cset/intersection (set (get db :tabs)) (set kks))]
            (vec (into (get v :root) [(get v :w) (get v :h) tt k])))))))

(re-frame/reg-sub
 ::is-grid?222
 (fn [db [_ panel-key view]]
   (true? (some #(= % :grid) (ut/deep-flatten (get-in db [:panels panel-key :views view]))))))

(defn round-to-nearest-quarter [num]
  (* 0.25 (js/Math.round (/ num 0.25))))

(defn mouse-move-handler [offset block tab-offset & [icon?]]
  (fn [evt]
    (let [start-x       (.-clientX evt)
          start-y       (.-clientY evt)
          ;_ (ut/tapp>> [:move block tab-offset])
          ;block         @(ut/tracked-subscribe [::selected-block])
          selected-root @(ut/tracked-subscribe [::root block]) ;;[-1 -1] ; @(ut/tracked-subscribe [::selected-root])
          off-x         (:x offset)
          off-y         (:y offset)
          ;shift? (.-shiftKey evt)
          x1 (if icon?
               (+ (round-to-nearest-quarter (/ (- start-x off-x) brick-size)) 0)
               (+ (js/Math.floor (/ (- start-x off-x) brick-size)) 1))
          y1 (if icon?
               (+ (round-to-nearest-quarter (/ (- start-y off-y) brick-size)) 0)
               (+ (js/Math.floor (/ (- start-y off-y) brick-size)) 0))
          x             (- x1 (first tab-offset))
          y             (- y1 (last tab-offset))
          translate (when false ;icon?
                      (let [containers @(ut/tracked-subscribe [::containers])
                            started-in-container? (not (= tab-offset [0 0]))
                            curr-tab @(ut/tracked-subscribe [::selected-tab])
                            tab-over (or (first (is-inside? [x1 y1] containers)) curr-tab)
                            base-tab? (= curr-tab tab-over)]
                        ;;(ut/tapp>> [:containers  started-in-container? base-tab? tab-over x y x1 y1])
                        (cond
                          (and started-in-container? (not base-tab?))  [x1 y1 tab-over]
                          (and started-in-container? base-tab?) (let [[offx offy] (for [[x y w h [t]] containers
                                                                                        :when (= t tab-over)] [x y])]
                                                                  [(- x1 offx) (- y1 offy) tab-over])
                          ;; (not base-tab?)  [x1 y1 tab-over]
                          ;; base-tab? (let [[offx offy] (for [[x y w h [t]] containers
                          ;;                                         :when (= t tab-over)] [x y])]
                          ;;                                         [(- x1 offx) (- y1 offy) tab-over])
                          :else [x y tab-over])))]
        ;; _ (ut/tapp>> [:in-container translate])

      ;(ut/tapp>> [shift? x y])
      (when (not (= selected-root [x y]))
        (if (and icon? translate)
          (do (when (not (= @dragging-block block)) (reset! dragging-block block))
              (ut/tracked-dispatch-sync [::update-workspace-noundo [block :root] [(first translate) (second translate)]])
              (ut/tracked-dispatch-sync [::update-workspace-noundo [block :tab] (last translate)]))
          (do (when (not (= @dragging-block block)) (reset! dragging-block block))
              (ut/tracked-dispatch-sync [::update-workspace-noundo [block :root] [x y]])))))))

(def mouse-dragging-panel? (reagent/atom false))

(defn mouse-up-handler [on-move]
  (fn me [evt]
    (do
      (reset! mouse-dragging-panel? false)
      (reset! dragging? false)
      (reset! dragging-body {})
      (reset! dragging-block nil)
      (reset! dyn-dropper-hover nil))
    (gevents/unlisten js/window EventType.MOUSEMOVE on-move)))

(defn mouse-down-handler [e block tab-offset & [icon?]]
  (let [{:keys [left top]} (get-client-rect e)
        offset  {:x (- (.-clientX e) left)
                 :y (- (.-clientY e) top)}
        on-move (mouse-move-handler offset block tab-offset icon?)]
    (do (reset! mouse-dragging-panel? true)
;        (reset! dragging-block block)
        (gevents/listen js/window EventType.MOUSEMOVE on-move))
    (gevents/listen js/window EventType.MOUSEUP
                    (mouse-up-handler on-move))))

(def hover-square (reagent/atom nil))
(def on-block? (reagent/atom false))
(def over-block? (reagent/atom false))
(def over-block (reagent/atom nil))
(def over-flow? (reagent/atom false))

(defn is-float?
  [n]   ;; cljs uses js "number" so core "float?" cant tell between int and float
  (and (number? n)
       (not= n (js/Math.floor n))))

(defn insert-new-block [root width height & body]
 ; (ut/tapp>> [:insert?])
  (when (not @over-flow?)
    (let [new-key        (str "block-" (rand-int 12345))
          req-block-name (get-in (first body) [:drag-meta :block-name])
          new-keyw       (if (nil? req-block-name) (keyword new-key) (keyword req-block-name))
          new-keyw       (ut/safe-key new-keyw)
          tab            @(ut/tracked-subscribe [::selected-tab])
          param-value    (str @(ut/tracked-subscribe [::conn/clicked-parameter [(get-in (first body) [:drag-meta :param-table]) (get-in (first body) [:drag-meta :param-field])]]))
          is-image?      (and
                          (or (cstr/ends-with? (cstr/lower-case (str param-value)) ".png")
                              (cstr/ends-with? (cstr/lower-case (str param-value)) ".jpg")
                              (cstr/ends-with? (cstr/lower-case (str param-value)) ".jpeg")
                              (cstr/ends-with? (cstr/lower-case (str param-value)) ".gif")
                              (cstr/includes? (cstr/lower-case (str param-value)) "/images/")
                              (cstr/includes? (cstr/lower-case (str param-value)) "/image/"))
                          (cstr/starts-with? (cstr/lower-case (str param-value)) "http"))
          is-video?      (or (cstr/ends-with? (cstr/lower-case (str param-value)) ".mp4")
                             (cstr/ends-with? (cstr/lower-case (str param-value)) ".mov"))
          param-key      (get-in (first body) [:drag-meta :param-full])
          is-view?       (and (cstr/ends-with? (str param-key) "-vw") (cstr/starts-with? (str param-key) ":flow"))
          multi-param?   (true? (cstr/includes? (str param-key) ".*/"))
          base-map       (if body                             ;; do we have incoming panel data or not?
                           (cond (= (get-in (first body) [:drag-meta :type]) :param)
                                 {:h       (get-in (first body) [:h])
                                  :w       (get-in (first body) [:w])
                                  :tab     tab
                                  :root    root
                                  :name    new-key
                                  :views   (if multi-param?
                                             {:multi-param-vals [:box
                                                                 :align :center
                                                                 :justify :center
                                                                 :padding "13px"
                                                                 :style {:font-size "30px"}
                                                                 :child [:h-box :gap "9px" :children param-key]]}
                                             {:param-val [:box
                                                          :align :center
                                                          :justify :center
                                                          :padding "13px"
                                                          :style {:font-size "45px"}
                                                          :child (cond is-view? [:honeycomb param-key] ;; TODO, we should recur resolve instead of honey-frag
                                                                       is-image? [:img {:src param-key :width "100%"}]
                                                                       is-video? [:iframe ;; :video ? html5 shit instead?
                                                                                  {:src :movies_deets/_1080p_video
                                                                                   :style {:border "none"
                                                                                           :width :panel-width+80-px
                                                                                           :height :panel-height+80-px}}]
                                                                       :else [:string param-key])]})
                                  :queries {}}

                                 (= (get-in (first body) [:drag-meta :type]) :viewer-pull)
                                 {:h       (get-in (first body) [:h])
                                  :w       (get-in (first body) [:w])
                                  :tab     tab
                                  :root    root
                                  :name    new-key
                                  :views   {:pulled-val (get-in (first body) [:drag-meta :param-full])}
                                  :queries {}}

                                 (= (get-in (first body) [:drag-meta :type]) :open-input) ;; middle click open-input
                                 (merge (first body) {:tab     tab
                                                      :root    root
                                                      :name    new-key})

                                 :else (assoc (dissoc (dissoc (first body) :source-panel) :drag-meta) :tab tab))
                           {:h       height :w width
                            :root    root
                            :tab     tab
                            :selected-view :hi
                            :name    new-key
                            :views   {:hi [:box :align :center :justify :center
                                           :attr {:id (str new-keyw "." :hi)}
                                           :style
                                           {:font-size    "106px"
                                            :font-weight  700
                                            :padding-top  "6px"
                                            :padding-left "14px"
                                            :margin-top   "-8px"
                                            :color        :theme/editor-outer-rim-color ;"#f9b9f9"
                                            :font-family  :theme/base-font} :child "hi!"]
                                      ;:stack [:layout {:panels {:hello-there {:h 0.99 :w 0.99 :root [0 0]}}}]
                                      }
                            :queries {}})]
    ;(ut/tapp>> [:param-drop @(ut/tracked-subscribe [::conn/clicked-parameter [(get-in (first body) [:drag-meta :param-table]) (get-in (first body) [:drag-meta :param-field])]])])
    ;(ut/tapp>> [:param-drop (first body)])
    ;(when (and (not (nil? root)) (not (nil? height)) (nil? @dyn-dropper-hover)
    ;           (not (nil? width)) (>= height 2) (>= width 2))
      ;;(ut/tapp>> [:insert? [new-keyw] base-map])
      (ut/tracked-dispatch [::update-workspace [new-keyw] base-map])
    ;  )
      (reset! hover-square nil))))

(re-frame/reg-event-db
 ::new-reco-preview
 (fn [db [_ hash]]
   (assoc-in db [:reco-preview] hash)))

(re-frame/reg-sub
 ::reco-preview-new?
 (fn [db [_ hash]]
   (not (= (get db :reco-preview) hash))))

(re-frame/reg-sub
 ::reco-preview
 (fn [db [_]]
   (get db :reco-preview)))

(re-frame/reg-sub
 ::lookup-mad-libs-row
 (fn [db [_ combohash]]
   (first (filter #(= (get % :combo_hash) combohash) (get-in db [:data :recos-sys2])))))

(defn insert-rec-preview-block [viz query condis connection-id combohash combo-name shape-name single? panel?]
  (let [new-key     (str "reco-preview" (if (not single?) combohash ""))
        new-keyw    (keyword new-key)
        ;combo-name @(ut/tracked-subscribe [::conn/clicked-parameter-key [:recos-sys/combo_edn]])
        ;shape-name @(ut/tracked-subscribe [::conn/clicked-parameter-key (if panel?
        ;                                                                [:viz-shapes0-sys/shape]
        ;                                                                [:viz-shapes0-sys2/shape])])
        ;; table-name  @(ut/tracked-subscribe [::conn/clicked-parameter-key (if panel?
        ;;                                                                  [:viz-tables-sys/table_name]
        ;;                                                                  [:viz-tables-sys2/table_name])])
        table-name @(ut/tracked-sub ::conn/clicked-parameter-key-alpha {:keypath (if panel?
                                                                                   [:viz-tables-sys/table_name]
                                                                                   [:viz-tables-sys2/table_name])})
        
        mad-libs-row (if (not panel?) @(ut/tracked-subscribe [::lookup-mad-libs-row combohash]) {})
        table-name  (if (keyword? table-name) (ut/replacer (ut/replacer (str table-name) ":" "") "-" "_") table-name)
        condi-keys  (try (keys (read-string condis)) (catch :default _ []))
        condi-walks (into {}
                          (for [c condi-keys]
                            (let [randy (rand-int 999)]
                              {c (keyword (str "c" randy "-" (ut/safe-name c)))
                               (keyword (str "condi/" (ut/safe-name c)))
                               (keyword (str "condi/c" randy "-" (ut/safe-name c)))})))
        queries     (into {} (for [q (range (count query))]
                               {(keyword (if (= q 0) (if (not single?)
                                                       (str "query-preview" combohash) "query-preview")
                                             (str (if (not single?)
                                                    (str "query-preview" combohash "-") "query-preview-")
                                                  (+ 1 q))))
                                (nth query q)}))
        queriesrep  (into {} (for [q (range (count query))]
                               {(keyword (if (= q 0) "query-preview"
                                             (str "query-preview-"
                                                  (+ 1 q))))
                                (keyword (if (= q 0)
                                           (str "query-preview" combohash)
                                           (str (str "query-preview" combohash "-") (+ 1 q))))}))
        ;queries1 (into {} (for [q (range (count query))]
        ;                    {(keyword (if (= q 0) "sql" (str "sql-" (+ 1 q))))
        ;                     (nth query q)}))
        ;queries (merge queries0 queries1)
        h             (if panel?
                        ;@(ut/tracked-subscribe [::conn/clicked-parameter-key [:recos-sys/h]])
                        @(ut/tracked-sub ::conn/clicked-parameter-key-alpha {:keypath [:recos-sys/h]})
                        (get mad-libs-row :h nil))
        w             (if panel?
                        ;@(ut/tracked-subscribe [::conn/clicked-parameter-key [:recos-sys/w]])
                        @(ut/tracked-sub ::conn/clicked-parameter-key-alpha {:keypath [:recos-sys/w]})
                        (get mad-libs-row :w nil))
        selected-view (if panel?
                        (try ;; weirdness here.. why was blowing up as seq? def just a keyword
                          (let [;sv @(ut/tracked-subscribe [::conn/clicked-parameter-key [:recos-sys/selected_view]])
                                sv @(ut/tracked-sub ::conn/clicked-parameter-key-alpha {:keypath [:recos-sys/selected_view]})] 
                            (when (ut/ne? sv) sv))
                          (catch :default _ nil))
                        (when (ut/ne? (get mad-libs-row :selected_view)) (get mad-libs-row :selected_view)))
        selected-view (when selected-view (try (keyword (ut/replacer selected-view #":" "")) (catch :default _ selected-view)))
        selected-view (if (= selected-view "") nil selected-view) ;; wtf
        base-map    (merge {:h (if h h 9)
                            :w (if w w 9)
                            ;:h 9 :w 9
                            :mad-libs-combo-hash [combohash table-name]
                            :root                [200 200]         ;; [0 0]
                            :name                (str shape-name " - " combo-name " - " (rand-int 1234))
                            :connection-id       connection-id
                            :views               (if (map? viz) viz {:oz viz})
                            ;:queries {:query-preview query}
                            :queries             queries}
                           (if selected-view {:selected-view selected-view} {}))
        base-map    (if (nil? condis) base-map
                        (assoc base-map :conditionals (read-string condis)))
        base-map    (ut/postwalk-replacer condi-walks base-map)
        base-map    (if (not single?) (ut/postwalk-replacer queriesrep base-map) base-map)]
    ;(ut/tapp>> [:insert-test panel? combohash selected-view h w viz query queries (when (not single?) queriesrep)])
    ;(ut/tapp>> [:insert-condis condis :condi-keys condi-keys :condi-walks condi-walks])
    ;(when (and (not (nil? root)) (not (nil? height)) (nil? @dyn-dropper-hover)
    ;           (not (nil? width)) (>= height 2) (>= width 2))
    (ut/tracked-dispatch [::update-workspace-noundo [new-keyw] base-map])))
    ;  )


(defn draggable [data type element]
  [(reagent/adapt-react-class rdnd/Draggable)
   {:type          type
    ;:on-drag #(tag-screen-position %)
    ;:on-drag #(ut/tapp>> [(.-clientX %) (.-clientY %)])
    :on-drag-end   #(do (reset! dragging? false)
                        (reset! dyn-dropper-hover false)
                        (reset! dragging-size [0 0])
                        ;(reset! on-block? false)
                        ;(ut/tapp>> [(.-clientX %) (.-clientY %)])
                        ; (tag-screen-position %)
                        (reset! dragging-body false))
    :on-drag-start #(do (reset! dragging? true)
                        (reset! on-block? false)
                        (reset! dragging-body data)
                        (reset! dragging-size [(get data :w) (get data :h)]))
    :data          (pr-str data)}
   [re-com/box :size "auto" :child element :style {:cursor "grab"}]])

(defn draggable-stub [data type element]
  [re-com/box :size "auto" :child element ])

(defn droppable [types-vec root element]
  ;(ut/tapp>> [:dropped? types-vec @dragging-body root])
  (if true ;(not @over-flow?) ;; dont trigger if we are cross breeding from flow canvas to prezi canvas
    [(reagent/adapt-react-class rdnd/Droppable)
     {:types   types-vec
      ;:on-drag #(tag-screen-position %)
      ;:on-drag-enter #(ut/tapp>> [:entered-outside?])
      ;:on-drag-over #(ut/tapp>> [:enter-base-canvas-droppable @over-flow?])
      :on-drop #(let [incoming    (js->clj %)
                      _ (ut/tapp>> [:incoming incoming @dragging-body])
                      ;_ (ut/tapp>> [:start? types-vec (last (last incoming)) (map? (last (last incoming))) (try (read-string (last (last incoming))) (catch :default e (str e)))])
                      data        (read-string (last (last incoming)))
                      data        (ut/postwalk-replacer {:reagent-gt :>} data) ;; unfuck "invalid EDN" reagent keywords....
                      type        (first (first incoming))
                      rooted-data (assoc data :root root)
                      dm-type     (get-in rooted-data [:drag-meta :type])
                      ;;_ (ut/tapp>> [:drop-data rooted-data])
                      ;new-key (str "block-" (rand-int 12345))
                      ;new-keyw (keyword new-key)
                      ok-new?     (or
                                   (and (not @on-block?)
                                        (or (false? @dyn-dropper-hover)
                                            (nil? @dyn-dropper-hover)))
                                   (= dm-type :viz-reco))]
                  ;(ut/tapp>> :sofar?)

                  (when (= dm-type :viz-reco)
                    (let [cc          @(ut/tracked-subscribe [::conn/clicked-parameter [:recos-sys]])
                          combo-keys  (select-keys cc [:context_hash :combo_hash])
                          client-name @(ut/tracked-subscribe [::conn/client-name])]
                      (ut/tapp>> [:dropped-viz-reco! data combo-keys cc])
                      (ut/tracked-dispatch [::wfx/request :default
                                          {:message {:kind         :selected-reco
                                                     :dm-type      :viz-reco
                                                     :combo_hash   (get combo-keys :combo_hash)
                                                     :context_hash (get combo-keys :context_hash)
                                                     :client-name  client-name}
                                           :timeout 50000}])))

                  (when (= dm-type :meta-fields)
                    (let [;cc @(ut/tracked-subscribe [::conn/clicked-parameter [:recos-sys]])
                          ;combo-keys (select-keys cc [:context_hash :combo_hash])
                          cc          (get rooted-data :drag-meta)
                          client-name @(ut/tracked-subscribe [::conn/client-name])]
                      (ut/tapp>> [:dropped-field-usage data cc cc])
                      (ut/tracked-dispatch [::wfx/request :default
                                          {:message {:kind        :selected-reco
                                                     :dm-type     :meta-fields
                                                     :drag-meta   cc
                                                     ;:combo_hash (get combo-keys :combo_hash)
                                                     ;:context_hash (get combo-keys :context_hash)
                                                     :client-name client-name}
                                           :timeout 50000}])))

                  (ut/tapp>> [:dropped type :ok-new? :drag-meta-type dm-type [@on-block? @dyn-dropper-hover] ok-new? rooted-data root])
                  (reset! dragging? false)
                  (cond
                    (= dm-type :meta-screens)
                    (ut/tracked-dispatch [::http/load (get rooted-data :file-path)])

                    (= dm-type :meta-theme)
                    (ut/tracked-dispatch [::overwrite-theme
                                        (apply dissoc rooted-data
                                               [:block-key :drag-meta :root :file-path])])

                    (= dm-type :meta-board)
                    (ut/tracked-dispatch [::add-board
                                        (apply dissoc rooted-data
                                               [:block-key :drag-meta :root :file-path])])

                    ; (get rooted-data :file_path) ;; (get @(ut/tracked-subscribe [::conn/clicked-parameter [:files-sys]]) :file_path)
                    ok-new?                                 ;(or (nil? @dyn-dropper-hover) (not @on-block?)) ;; dont create new block for context drops
                    (insert-new-block root 4 4 rooted-data)))}
     [re-com/box :child element]]
    (do ;(reset! dragging? false)
      element)))

(def new-block-atom (reagent/atom nil))

(defn mouse-up-handler-new [on-move]
  (fn me [evt]
    (let [root   (get @new-block-atom :root)
          width  (get @new-block-atom :w)
          height (get @new-block-atom :h)]
      (when (and (not (nil? root)) (not (nil? height)) (not (nil? width)))
        (insert-new-block root width height))
      (reset! new-block-atom nil))
    (gevents/unlisten js/window EventType.MOUSEMOVE on-move)))

(defn mouse-move-handler-new [offset orig-x orig-y]
  (fn [evt]
    (let [start-x    (.-clientX evt)
          start-y    (.-clientY evt)
          off-x      (:x offset)
          off-y      (:y offset)
          x          (js/Math.floor (/ (- start-x off-x) brick-size)) ;
          y          (js/Math.floor (/ (- start-y off-y) brick-size))
          start-root [(js/Math.floor (/ orig-x brick-size))
                      (js/Math.floor (/ orig-y brick-size))]
          drag-w     (- x (js/Math.floor (/ orig-x brick-size)))
          drag-h     (- y (js/Math.floor (/ orig-y brick-size))) ;; bh bw start-h start-w
          drag-wake  @(ut/tracked-sub ::build-grid
                                           {:bh (+ drag-h (last start-root) 1)
                                           :bw (+ drag-w (first start-root) 1)
                                           :start-h (last start-root)
                                           :start-w (first start-root)})]
      (reset! new-block-atom {:root [(first start-root) (last start-root)] :w (+ drag-w 1) :h (+ drag-h 1)})
      (reset! hover-square drag-wake))))

(defn mouse-down-handler-new [e]
  (let [{:keys [left top]} (get-client-rect e)
        offset  {:x (- (.-clientX e) left)
                 :y (- (.-clientY e) top)}
        orig-x  (.-clientX e)
        orig-y  (.-clientY e)
        on-move (mouse-move-handler-new offset orig-x orig-y)]
    (gevents/listen js/window EventType.MOUSEMOVE on-move)
    (gevents/listen js/window EventType.MOUSEUP
                    (mouse-up-handler-new on-move))))

(defn resize-mouse-move-handler [offset width height]
  (fn [evt]
    (let [block      @(ut/tracked-sub ::selected-block {})
          start-x    (.-clientX evt)
          start-y    (.-clientY evt)
          off-x      (:x offset)
          off-y      (:y offset)
          w          (js/Math.floor (/ (- start-x off-x) brick-size))
          h          (js/Math.floor (/ (- start-y off-y) brick-size))
          wf         (+ w width 0)
          hf         (+ h height 0)
          selected-h @(ut/tracked-sub ::selected-h {})
          selected-w @(ut/tracked-sub ::selected-w {})
          wmax       (if (< wf 2) 2 wf)
          hmax       (if (< hf 2) 2 hf)]
      (when (not (= hmax selected-h)) (ut/tracked-dispatch [::update-workspace-noundo [block :h] hmax]))
      (when (not (= wmax selected-w)) (ut/tracked-dispatch [::update-workspace-noundo [block :w] wmax])))))

(defn resize-mouse-down-handler [e]
  ;(ut/tracked-dispatch [::select-block panel-id])
  (let [{:keys [left top]} (get-client-rect e)
        block   @(ut/tracked-sub ::selected-block {})
        ;width   @(ut/tracked-subscribe [::workspace [block :w]])
        ;height  @(ut/tracked-subscribe [::workspace [block :h]])
        width   @(ut/tracked-sub ::workspace-alpha {:keypath [block :w]})
        height  @(ut/tracked-sub ::workspace-alpha {:keypath [block :h]})
        offset  {:x (- (.-clientX e) width)
                 :y (- (.-clientY e) height)}
        on-move (resize-mouse-move-handler offset width height)]
    (do (reset! mouse-dragging-panel? true)
        (gevents/listen js/window EventType.MOUSEMOVE on-move))
    (gevents/listen js/window EventType.MOUSEUP
                    (mouse-up-handler on-move))))

;; (defn format-map [w s]
;;   (zp/zprint-str s (js/Math.floor (/ w 9))
;;                  {:parse-string-all? true
;;                   :style :justified
;;                   ;:fn-map {:double1 :just-float}
;;                   :vector {:respect-nl? true}
;;                   :map {:comma? false :sort? false}
;;                   :parse {:interpose "\n\n"}}))

;; (defn upstream-search [subq-map panel-id]
;;   (let [producers     (into {} (for [[k v] subq-map] {k (get v :produces)}))
;;         all-producers (into {} (for [[k v] producers]
;;                                  (into {} (for [p v]
;;                                             {p k}))))
;;         mm            (ut/postwalk-replacer all-producers subq-map)]
;;     (defn ww [id x]
;;       (let [iid (try (first (get-in mm [id :uses])) (catch :default e nil))]
;;         (if (nil? iid) (conj x id)
;;             (ww iid (conj x id)))))
;;     (ww panel-id [])))




;; (defonce db/cm-instance-panel-code-box (atom nil)) ;; for highlighting
;; (defonce db/markers-panel-code-box (atom []))

(defn highlight-code [code]
  (when-let [editor @db/cm-instance-panel-code-box]
    (let [doc (.getDoc editor)
          ;; code (ut/format-map 640 ;; 539 ;;(- 634.4 95)
          ;;                     (str code))
          code (str code)
          _ (ut/tapp>> [:highlighted-code-panel-code-box code])]
      ;; Clear existing markers
      (doseq [marker @db/markers-panel-code-box]
        (.clear marker))
      (reset! db/markers-panel-code-box [])
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
                                #js {:css (str "filter: invert(233%); color: white; font-weight: 700; background-color: teal; font-size: 15px;")})
                     (catch :default e (ut/tapp>> [:marker-error (str e)])))]
        (swap! db/markers-panel-code-box conj marker)))))

;; (defn highlight-codes [codes css]
;;   (let [css-string (ut/hiccup-css-to-string css)]
;;     (when-let [editor @db/cm-instance-panel-code-box]
;;       (let [doc (.getDoc editor)]
;;         ;; Clear existing markers
;;         (doseq [marker @db/markers-panel-code-box]
;;           (.clear marker))
;;         (reset! db/markers-panel-code-box [])
;;         (doseq [code codes]
;;           (let [code (str code)
;;                 _ (ut/tapp>> [:highlighted-code-panel-code-box code])
;;                 code-lines (clojure.string/split-lines code)
;;                 start-line (loop [line 0]
;;                              (when (< line (.lineCount doc))
;;                                (if (clojure.string/includes? (.getLine doc line) (first code-lines))
;;                                  line
;;                                  (recur (inc line)))))
;;                 end-line (loop [line start-line]
;;                            (when (< line (.lineCount doc))
;;                              (if (clojure.string/includes? (.getLine doc line) (last code-lines))
;;                                line
;;                                (recur (inc line)))))
;;                 start-ch (clojure.string/index-of (.getLine doc start-line) (first code-lines))
;;                 end-ch (+ (clojure.string/index-of (.getLine doc end-line) (last code-lines)) (count (last code-lines)))
;;                 marker (try
;;                          (.markText doc
;;                                     #js {:line start-line, :ch start-ch}
;;                                     #js {:line end-line, :ch end-ch}
;;                                     #js {:css css-string})
;;                          (catch :default e (ut/tapp>> [:marker-error (str e)])))]
;;             (swap! db/markers-panel-code-box conj marker)))))))

;; (defn highlight-codes-only [codes css]
;;   (let [css-string (ut/hiccup-css-to-string css)]
;;     (when-let [editor @db/cm-instance-panel-code-box]
;;       (let [doc (.getDoc editor)]
;;         ;; Clear existing markers
;;         (doseq [marker @db/markers-panel-code-box]
;;           (.clear marker))
;;         (reset! db/markers-panel-code-box [])
;;         (doseq [code codes]
;;           (let [code (str code)
;;                 _ (ut/tapp>> [:highlighted-code-panel-code-box code])
;;                 code-lines (clojure.string/split-lines code)
;;                 start-line (loop [line 0]
;;                              (when (< line (.lineCount doc))
;;                                (if (clojure.string/includes? (.getLine doc line) (first code-lines))
;;                                  line
;;                                  (recur (inc line)))))
;;                 end-line (loop [line start-line]
;;                            (when (< line (.lineCount doc))
;;                              (if (clojure.string/includes? (.getLine doc line) (last code-lines))
;;                                line
;;                                (recur (inc line)))))
;;                 start-ch (clojure.string/index-of (.getLine doc start-line) (first code-lines))
;;                 end-ch (+ (clojure.string/index-of (.getLine doc end-line) (last code-lines)) (count (last code-lines)))
;;                 marker (try
;;                          (let [node (js/document.createElement "span")
;;                                _ (.setAttribute node "style" css-string)
;;                                _ (.appendChild node (js/document.createTextNode code))]
;;                            (.markText doc
;;                                       #js {:line start-line, :ch start-ch}
;;                                       #js {:line end-line, :ch end-ch}
;;                                       #js {:replacedWith node}))
;;                          (catch :default e (ut/tapp>> [:marker-error (str e)])))]
;;             (swap! db/markers-panel-code-box conj marker)))))))



;; (defn highlight-codes-only [codes css hiccup-fn]
;;   (ut/tapp>> [:highlight-codes-only [codes css hiccup-fn]])
;;   (let [css-string (ut/hiccup-css-to-string css)]
;;     (when-let [editor @db/cm-instance-panel-code-box]
;;       (let [doc (.getDoc editor)]
;;         ;; Clear existing markers
;;         (doseq [marker @db/markers-panel-code-box]
;;           (.clear marker))
;;         (reset! db/markers-panel-code-box [])
;;         (doseq [code codes]
;;           (let [code (str code)
;;                 ;_ (ut/tapp>> [:highlighted-code-panel-code-box code])
;;                 code-lines (clojure.string/split-lines code)
;;                 start-line (loop [line 0]
;;                              (when (< line (.lineCount doc))
;;                                (if (clojure.string/includes? (.getLine doc line) (first code-lines))
;;                                  line
;;                                  (recur (inc line)))))
;;                 end-line (loop [line start-line]
;;                            (when (< line (.lineCount doc))
;;                              (if (clojure.string/includes? (.getLine doc line) (last code-lines))
;;                                line
;;                                (recur (inc line)))))
;;                 start-ch (clojure.string/index-of (.getLine doc start-line) (first code-lines))
;;                 end-ch (+ (clojure.string/index-of (.getLine doc end-line) (last code-lines)) (count (last code-lines)))
;;                 marker (try
;;                          (let [node (js/document.createElement "span")
;;                                _ (.setAttribute node "style" css-string)
;;                                _ (.appendChild node (js/document.createTextNode code))
;;                                widget-node (js/document.createElement "div")]
;;                            (.addEventListener node "mouseover" (fn []
;;                                                                  (when-not @widget
;;                                                                    (let [react-element (reagent/as-element (hiccup-fn code))]
;;                                                                      (rdom/render react-element widget-node)
;;                                                                      (reset! widget (.addLineWidget doc end-line widget-node))))))
;;                            (.addEventListener node "mouseout" (fn []
;;                                                                 (when @widget
;;                                                                   (.clear @widget)
;;                                                                   (reset! widget nil))))
;;                            (.addEventListener node "mouseleave" (fn []
;;                                                                   (when @widget
;;                                                                     (.clear @widget)
;;                                                                     (reset! widget nil))))
;;                            (.markText doc
;;                                       #js {:line start-line, :ch start-ch}
;;                                       #js {:line end-line, :ch end-ch}
;;                                       #js {:replacedWith node}))
;;                          (catch :default e (ut/tapp>> [:marker-error (str e)])))]
;;             (swap! db/markers-panel-code-box conj marker)))))))

(def widget (atom nil))

;; (defn highlight-codes-only [codes css hiccup-fn] ;; full note
;;   ;; (ut/tapp>> [:highlight-codes-only [codes css hiccup-fn]])
;;   (let [css-string (ut/hiccup-css-to-string css)]
;;     (when-let [editor @db/cm-instance-panel-code-box]
;;       (let [doc (.getDoc editor)]
;;         ;; Clear existing markers
;;         (doseq [marker @db/markers-panel-code-box]
;;           (.clear marker))
;;         (reset! db/markers-panel-code-box [])
;;         (doseq [code codes]
;;           (let [code (str code)
;;                 code-lines (clojure.string/split-lines code)]
;;             (loop [line 0]
;;               (when (< line (.lineCount doc))
;;                 (when (clojure.string/includes? (.getLine doc line) (first code-lines))
;;                   (let [start-line line
;;                         end-line (loop [line start-line]
;;                                    (when (< line (.lineCount doc))
;;                                      (if (clojure.string/includes? (.getLine doc line) (last code-lines))
;;                                        line
;;                                        (recur (inc line)))))
;;                         start-ch (clojure.string/index-of (.getLine doc start-line) (first code-lines))
;;                         end-ch (+ (clojure.string/index-of (.getLine doc end-line) (last code-lines)) (count (last code-lines)))
;;                         marker (try
;;                                  (let [node (js/document.createElement "span")
;;                                        _ (.setAttribute node "style" css-string)
;;                                        _ (.appendChild node (js/document.createTextNode code))
;;                                        widget-node (js/document.createElement "div")]
;;                                    (.addEventListener node "mouseover" (fn []
;;                                                                          (when-not @widget
;;                                                                            (let [react-element (reagent/as-element (hiccup-fn code))]
;;                                                                              (rdom/render react-element widget-node)
;;                                                                              (reset! widget (.addLineWidget doc end-line widget-node #js {:above (= end-line (dec (.lineCount doc)))}))))))  ;; Render widget above if on last line
;;                                    (.addEventListener node "mouseout" (fn []
;;                                                                         (when @widget
;;                                                                           (.clear @widget)
;;                                                                           (reset! widget nil))))
;;                                    (.addEventListener node "mouseleave" (fn []
;;                                                                           (when @widget
;;                                                                             (.clear @widget)
;;                                                                             (reset! widget nil))))
;;                                    (.markText doc
;;                                               #js {:line start-line, :ch start-ch}
;;                                               #js {:line end-line, :ch end-ch}
;;                                               #js {:replacedWith node}))
;;                                  (catch :default e (ut/tapp>> [:marker-error (str e)])))]
;;                     (swap! db/markers-panel-code-box conj marker)))
;;                 (recur (inc line))))))))))

;; (defn highlight-codes-only [codes css hiccup-fn] ;;; pre autocomplete clicks ?
;;   (let [css-string (ut/hiccup-css-to-string css)]
;;     (when-let [editor @db/cm-instance-panel-code-box]
;;       (let [doc (.getDoc editor)]
;;         ;; Clear existing markers
;;         (doseq [marker @db/markers-panel-code-box]
;;           (.clear marker))
;;         (reset! db/markers-panel-code-box [])
;;         (doseq [code codes]
;;           (let [code (str code)
;;                 code-lines (clojure.string/split-lines code)]
;;             (loop [line 0]
;;               (when (< line (.lineCount doc))
;;                 (when (clojure.string/includes? (.getLine doc line) (first code-lines))
;;                   (let [start-line line
;;                         end-line (loop [line start-line]
;;                                    (when (< line (.lineCount doc))
;;                                      (if (clojure.string/includes? (.getLine doc line) (last code-lines))
;;                                        line
;;                                        (recur (inc line)))))
;;                         code-parts (let [ff (vec (clojure.string/split code #"/"))
;;                                          vv (vec (clojure.string/split (get ff 1) #">"))
;;                                          cc (vec (into [(first ff)] vv))] cc)
;;                   ;;  _ (ut/tapp>> [:code code :code-parts code-parts])
;;                         ]
;;                     (loop [code-parts code-parts
;;                            start-ch 0]
;;                       (when (seq code-parts)
;;                         (let [code-part (first code-parts)
;;                               start-ch (clojure.string/index-of (.getLine doc start-line) code-part start-ch)
;;                               end-ch (+ start-ch (count code-part))
;;                               marker (try
;;                                        (let [node (js/document.createElement "span")
;;                                              _ (.setAttribute node "style" css-string)
;;                                              _ (.appendChild node (js/document.createTextNode code-part))
;;                                              widget-node (js/document.createElement "div")]
;;                                          (.addEventListener node "mouseover" (fn []
;;                                                                                (when-not @widget
;;                                                                                  (let [react-element (reagent/as-element (hiccup-fn code))] ;; Pass the original code to hiccup-fn
;;                                                                                    (ut/tapp>> [:hover code code-part])
;;                                                                                    (rdom/render react-element widget-node)
;;                                                                                    (reset! widget (.addLineWidget doc end-line widget-node #js {:above (= end-line (dec (.lineCount doc)))}))))))  ;; Render widget above if on last line
;;                                          (.addEventListener node "mouseout" (fn []
;;                                                                               (when @widget
;;                                                                                 (.clear @widget)
;;                                                                                 (reset! widget nil))))
;;                                          (.addEventListener node "mouseleave" (fn []
;;                                                                                 (when @widget
;;                                                                                   (.clear @widget)
;;                                                                                   (reset! widget nil))))
;;                                          (.markText doc
;;                                                     #js {:line start-line, :ch start-ch}
;;                                                     #js {:line end-line, :ch end-ch}
;;                                                     #js {:replacedWith node}))
;;                                        (catch :default e (ut/tapp>> [:marker-error (str e)])))]
;;                           (when (clojure.string/includes? (.getLine doc start-line) code-part) ;; Check if the entire code-part is present in the line
;;                             (swap! db/markers-panel-code-box conj marker))
;;                           (recur (rest code-parts) end-ch)))))) ;; Recur with the rest of the code parts and the end character of the current part as the start character for the next part
;;                 (recur (inc line))))))))))

(declare custom-hint-fn)

;; (defn highlight-codes-only [codes css hiccup-fn]
;;   (let [css-string (ut/hiccup-css-to-string css)]
;;     (when-let [editor @db/cm-instance-panel-code-box]
;;       (let [doc (.getDoc editor)]
;;         ;; Clear existing markers
;;         (doseq [marker @db/markers-panel-code-box]
;;           (.clear marker))
;;         (reset! db/markers-panel-code-box [])
;;         (doseq [code codes]
;;           (let [code (str code)
;;                 code-lines (clojure.string/split-lines code)]
;;             (loop [line 0]
;;               (when (< line (.lineCount doc))
;;                 (when (clojure.string/includes? (.getLine doc line) (first code-lines))
;;                   (let [start-line line
;;                         end-line (loop [line start-line]
;;                                    (when (< line (.lineCount doc))
;;                                      (if (clojure.string/includes? (.getLine doc line) (last code-lines))
;;                                        line
;;                                        (recur (inc line)))))
;;                         code-parts (let [ff (vec (clojure.string/split code #"/"))
;;                                          vv (vec (clojure.string/split (get ff 1) #">"))
;;                                          cc (vec (into [(first ff)] vv))] cc)
;;                         full-code-parts code-parts
;;                   ;;  _ (ut/tapp>> [:code code :code-parts code-parts])
;;                         ]
;; (loop [code-parts code-parts
;;        start-ch 0
;;        code-part-start-ch 0
;;        code-part-end-ch 0] ;; Add a new variable to track the end position of the last segment
;;   (when (seq code-parts)
;;     (let [code-part (first code-parts)
;;           start-ch (clojure.string/index-of (.getLine doc start-line) code-part start-ch)
;;           end-ch (+ start-ch (count code-part))
;;           code-part-start-ch (if (= code-part (first full-code-parts)) start-ch code-part-start-ch) ;; Update code-part-start-ch only if it's the first segment
;;           code-part-end-ch end-ch ;; Update code-part-end-ch at the end of each iteration
;;           marker (try
;;                    (let [node (js/document.createElement "span")
;;                          _ (.setAttribute node "style" css-string)
;;                          _ (.appendChild node (js/document.createTextNode code-part))
;;                          widget-node (js/document.createElement "div")]
;;                      (.addEventListener node "click" (fn [event]
;;                                                        (let [this-token-idx (.indexOf full-code-parts code-part)
;;                                                              token-parts (subvec full-code-parts 0 this-token-idx)
;;                                                              seeded-token (str (first token-parts) "/" (cstr/join ">" (rest token-parts)))]
;;                                                          (.showHint editor #js {:completeSingle false :hint (fn [] (custom-hint-fn editor seeded-token start-line code-part-start-ch code-part-end-ch))})))) ;; Use code-part-start-ch and code-part-end-ch here
;;                      (.addEventListener node "mouseover" (fn []
;;                                                            (when-not @widget
;;                                                              (let [react-element (reagent/as-element (hiccup-fn code))] ;; Pass the original code to hiccup-fn
;;                                                                ;;(ut/tapp>> [:hover code code-part])
;;                                                                (rdom/render react-element widget-node)
;;                                                                (reset! widget (.addLineWidget doc end-line widget-node #js {:above (= end-line (dec (.lineCount doc)))}))))))  ;; Render widget above if on last line
;;                      (.addEventListener node "mouseout" (fn []
;;                                                           (when @widget
;;                                                             (.clear @widget)
;;                                                             (reset! widget nil))))
;;                      (.addEventListener node "mouseleave" (fn []
;;                                                             (when @widget
;;                                                               (.clear @widget)
;;                                                               (reset! widget nil))))
;;                      (.markText doc
;;                                 #js {:line start-line, :ch start-ch}
;;                                 #js {:line end-line, :ch end-ch}
;;                                 #js {:replacedWith node}))
;;                    (catch :default e (ut/tapp>> [:marker-error (str e)])))]
;;       (when (clojure.string/includes? (.getLine doc start-line) code-part) ;; Check if the entire code-part is present in the line
;;         (swap! db/markers-panel-code-box conj marker))
;;       (recur (rest code-parts) end-ch code-part-start-ch code-part-end-ch)))) ;; Recur with the rest of the code parts, the end character of the current part as the start character for the next part, and the start and end characters of the first and last segments

;;                           )) ;; Recur with the rest of the code parts and the end character of the current part as the start character for the next part
;;                 (recur (inc line))))))))))


;; (defn highlight-codes-only [codes css hiccup-fn]
;;   (let [css-string (ut/hiccup-css-to-string css)]
;;     (when-let [editor @db/cm-instance-panel-code-box]
;;       (let [doc (.getDoc editor)]
;;         ;; Clear existing markers
;;         (doseq [marker @db/markers-panel-code-box]
;;           (.clear marker))
;;         (reset! db/markers-panel-code-box [])
;;         (doseq [code codes]
;;           (let [code (str code)
;;                 code-lines (clojure.string/split-lines code)]
;;             (loop [line 0]
;;               (when (< line (.lineCount doc))
;;                 (when (clojure.string/includes? (.getLine doc line) (first code-lines))
;;                   (let [start-line line
;;                         end-line (loop [line start-line]
;;                                    (when (< line (.lineCount doc))
;;                                      (if (clojure.string/includes? (.getLine doc line) (last code-lines))
;;                                        line
;;                                        (recur (inc line)))))
;;                         code-parts (let [ff (vec (clojure.string/split code #"/"))
;;                                          vv (vec (clojure.string/split (get ff 1) #">"))
;;                                          cc (vec (into [(first ff)] vv))] cc)
;;                         full-code-parts code-parts
;;                         ;; Calculate the start and end positions of the entire match before the loop
;;                         code-part-start-ch (clojure.string/index-of (.getLine doc start-line) (first full-code-parts))
;;                         code-part-end-ch (+ (clojure.string/index-of (.getLine doc start-line) (last full-code-parts)) (count (last full-code-parts)))]
;;                     (loop [code-parts code-parts
;;                            start-ch 0]
;;                       (when (seq code-parts)
;;                         (let [code-part (first code-parts)
;;                               start-ch (clojure.string/index-of (.getLine doc start-line) code-part start-ch)
;;                               end-ch (+ start-ch (count code-part))
;;                               marker (try
;;                                        (let [node (js/document.createElement "span")
;;                                              _ (.setAttribute node "style" css-string)
;;                                              _ (.appendChild node (js/document.createTextNode code-part))
;;                                              widget-node (js/document.createElement "div")]
;;                                          (.addEventListener node "click" (fn [event]
;;                                                                            (let [this-token-idx (.indexOf full-code-parts code-part)
;;                                                                                  token-parts (subvec full-code-parts 0 this-token-idx)
;;                                                                                  seeded-token (str (first token-parts) "/" (cstr/join ">" (rest token-parts)))]
;;                                                                              (.showHint editor #js {:completeSingle false :hint (fn [] (custom-hint-fn editor seeded-token start-line code-part-start-ch code-part-end-ch))}))))
;;                                          (.addEventListener node "mouseover" (fn []
;;                                                                                (when-not @widget
;;                                                                                  (let [react-element (reagent/as-element (hiccup-fn code))]
;;                                                                                    (rdom/render react-element widget-node)
;;                                                                                    (reset! widget (.addLineWidget doc end-line widget-node #js {:above (= end-line (dec (.lineCount doc)))}))))))
;;                                          (.addEventListener node "mouseout" (fn []
;;                                                                               (when @widget
;;                                                                                 (.clear @widget)
;;                                                                                 (reset! widget nil))))
;;                                          (.addEventListener node "mouseleave" (fn []
;;                                                                                 (when @widget
;;                                                                                   (.clear @widget)
;;                                                                                   (reset! widget nil))))
;;                                          (.markText doc
;;                                                     #js {:line start-line, :ch start-ch}
;;                                                     #js {:line end-line, :ch end-ch}
;;                                                     #js {:replacedWith node}))
;;                                        (catch :default e (ut/tapp>> [:marker-error (str e)])))]
;;                           (when (clojure.string/includes? (.getLine doc start-line) code-part)
;;                             (swap! db/markers-panel-code-box conj marker))
;;                           (recur (rest code-parts) end-ch)))))) ;; Recur with the rest of the code parts and the end character of the current part as the start character for the next part
;;                     (recur (inc line))))))))))


(defonce param-hover (reagent/atom nil))

(defn highlight-codes-only [codes css]
  (let [react! [@db/param-code-hover]]
    (when-let [editor @db/cm-instance-panel-code-box]
      (let [doc (.getDoc editor)]
        ;; Clear existing markers
        (doseq [marker @db/markers-panel-code-box]
          (.clear marker))
        (reset! db/markers-panel-code-box [])
        (doseq [code codes]
          (let [code (str code)
                code-lines (clojure.string/split-lines code)]
            (loop [line 0]
              (when (< line (.lineCount doc))
                (when (clojure.string/includes? (.getLine doc line) (first code-lines))
                  (let [start-line line
                        end-line (loop [line start-line]
                                   (when (< line (.lineCount doc))
                                     (if (clojure.string/includes? (.getLine doc line) (last code-lines))
                                       line
                                       (recur (inc line)))))
                        code-parts (let [ff (vec (clojure.string/split code #"/"))
                                         vv (vec (clojure.string/split (get ff 1) #">"))
                                         cc (vec (into [(first ff)] vv))] cc)
                        full-code-parts code-parts
                        ;; Calculate the start and end positions of the entire match before the loop
                        code-part-start-ch (clojure.string/index-of (.getLine doc start-line) (first full-code-parts))
                        code-part-end-ch (+ (clojure.string/index-of (.getLine doc start-line) (last full-code-parts)) (count (last full-code-parts)))]
                    (loop [code-parts code-parts
                           start-ch 0]
                      (when (seq code-parts)
                        (let [code-part (first code-parts)
                              start-ch (clojure.string/index-of (.getLine doc start-line) code-part start-ch)
                              end-ch (+ start-ch (count code-part))
                              marker (try
                                       (let [node (js/document.createElement "span")
                                             _ (.setAttribute node "style" (ut/hiccup-css-to-string
                                                                            (merge css (when (= (str code) (str @db/param-code-hover))
                                                                                         ;{:filter (str (get css :filter) " scale(1.2)")}
                                                                                         {;:transform "scale(1.2)"
                                                                                          :box-shadow (str "0 0 10px 5px " (ut/invert-hex-color (theme-pull :theme/editor-outer-rim-color nil)))}))))
                                             _ (.appendChild node (js/document.createTextNode code-part))
                                             ;widget-node (js/document.createElement "div")
                                             ]
                                         (.addEventListener node "click" (fn []
                                                                           (let [this-token-idx (.indexOf full-code-parts code-part)
                                                                                 token-parts (subvec full-code-parts 0 this-token-idx)
                                                                                 seeded-token (str (first token-parts) "/" (cstr/join ">" (rest token-parts)))]
                                                                             (.setCursor editor #js {:line start-line, :ch code-part-start-ch})
                                                                             (.showHint editor #js {:completeSingle false :hint (fn [] (custom-hint-fn editor seeded-token start-line code-part-start-ch code-part-end-ch))}))))
                                         (.addEventListener node "mouseover" (fn []
                                                                               (let [code (try (edn/read-string code) (catch :default _  code))
                                                                                     psplit      (ut/splitter (ut/safe-name (str code)) "/")
                                                                                     table       (-> (first psplit) (ut/replacer  #":" "") (ut/replacer  ".*" "") keyword)
                                                                                     field       (keyword (last psplit))]
                                                                                 (reset! param-hover [code table field]))))
                                         (.addEventListener node "mouseenter" (fn []
                                                                                (let [code (try (edn/read-string code) (catch :default _  code))
                                                                                      psplit      (ut/splitter (ut/safe-name (str code)) "/")
                                                                                      table       (-> (first psplit) (ut/replacer  #":" "") (ut/replacer  ".*" "") keyword)
                                                                                      field       (keyword (last psplit))]
                                                                                  ;; (ut/tapp>> [:hh2 [full-code-parts code table field]])
                                                                                  (reset! param-hover [code table field]))))
                                         (.addEventListener node "mouseout" (fn []
                                                                              (reset! param-hover nil)))
                                         (.addEventListener node "mouseleave" (fn []
                                                                                (reset! param-hover nil)))
                                         (.markText doc
                                                    #js {:line start-line, :ch start-ch}
                                                    #js {:line end-line, :ch end-ch}
                                                    #js {:replacedWith node}))
                                       (catch :default e (ut/tapp>> [:marker-error (str e)])))]
                          (when (clojure.string/includes? (.getLine doc start-line) code-part)
                            (swap! db/markers-panel-code-box conj marker))
                          (recur (rest code-parts) end-ch))))))
                (recur (inc line))))))))))


(defn highlight-codes-values [codes css]
  (let [react! [@db/param-code-hover]]
    (when-let [editor @db/cm-instance-panel-code-box]
      (let [doc (.getDoc editor)]
        ;; Clear existing markers
        (doseq [marker @db/markers-panel-code-box]
          (.clear marker))
        (reset! db/markers-panel-code-box [])
        (doseq [[code value] codes]
          (let [code (str code)
                value (if (string? value) (pr-str value) value)
                code-lines (clojure.string/split-lines code)]
            (loop [line 0]
              (when (< line (.lineCount doc))
                (when (clojure.string/includes? (.getLine doc line) (first code-lines))
                  (let [start-line line
                        end-line (loop [line start-line]
                                   (when (< line (.lineCount doc))
                                     (if (clojure.string/includes? (.getLine doc line) (last code-lines))
                                       line
                                       (recur (inc line)))))
                        code-part-start-ch (clojure.string/index-of (.getLine doc start-line) (first code-lines))
                        code-part-end-ch (+ (clojure.string/index-of (.getLine doc start-line) (last code-lines)) (count (last code-lines)))
                        node (js/document.createElement "span")
                        _ (.setAttribute node "style" (ut/hiccup-css-to-string css))
                        _ (.appendChild node (js/document.createTextNode value))
                        marker (.markText doc
                                          #js {:line start-line, :ch code-part-start-ch}
                                          #js {:line end-line, :ch code-part-end-ch}
                                          #js {:replacedWith node})]
                    (swap! db/markers-panel-code-box conj marker)))
                (recur (inc line))))))))))



(defn unhighlight-code []
  (when-let [editor @db/cm-instance-panel-code-box]
    (let [doc (.getDoc editor)]
      (doseq [marker @db/markers-panel-code-box]
        (when marker
          (.clear marker)))
      (reset! db/markers-panel-code-box []))))


;; (defn custom-completions [cm]
;;   (let [cursor (.getCursor cm)
;;         token (.getTokenAt cm cursor {:precise true})]
;;     ;; Generate your custom completion list here
;;     (let [list (map name [:keyword1 :k! :keyword2 ":keyword44"])] ; Your custom keywords
;;       ;; Show custom completions
;;       (.showHint cm (clj->js {:list list
;;                               :from {:line (.-line cursor) :ch (.-start token)}
;;                               :to {:fdline (.-line cursor) :ch (.-end token)}})))))

;; (defn custom-hint-fn [cm options]
;;   (let [cursor (.-doc cm) ;; Get the current cursor position
;;         token (.getRange cursor (.getCursor cursor) (.getCursor cursor "end"))] ;; Get the current token
;;     (ut/tapp>> ["Current token:" token]) ;; Optional: for debugging
;;     ;; Define your list of suggestions here
;;     (let [list [":keyword1" ":k!" ":keyword2" ":keyword44"]]
;;       ;; Return a hint object expected by CodeMirror
;;       (clj->js {:list list
;;                 :from (.-cursor cursor)
;;                 :to (.-cursor cursor)}))))

;; (defn custom-hint-fn [cm options]
;;   (let [;cursor (.-doc cm)
;;         token (.getTokenAt cm (.getCursor cm) "end")]
;;     (ut/tapp>> ["Current token:" (.-string token)]) ;; Debugging
;;     ;; Define your list of suggestions here
;;     (let [input (.-string token)
;;           list [":keyword1" ":k!" ":keyword2" ":keyword44" ":kiwi/farts" ":liwi/limes"]
;;           filtered-list (filter #(clojure.string/starts-with? % input) list)] ;; Dynamic filtering
;;       ;; Return a hint object expected by CodeMirror
;;       (clj->js {:list filtered-list
;;                 :from (.-start token)
;;                 :to (.-end token)}))))

(re-frame/reg-sub
 ::parameters-available
 (fn [db _]
   (let [;;codes [:theme/base-font :font-family :height]
         server-params (get-in db [:autocomplete :clover-params] [])
         view-codes (get-in db [:autocomplete :view-keywords] [])
         flow-subs (get db :flow-subs)
         click-params (vec (for [e (keys (get-in db [:click-param :param]))]
                             (keyword (str "param/" (ut/replacer (str e) ":" "")))))
         themes (vec (for [e (keys (get-in db [:click-param :theme]))]
                       (keyword (str "theme/" (ut/replacer (str e) ":" "")))))
         ;;codes (vec (into server-params (into themes (into flow-subs click-params))))
         codes (vec (apply concat [server-params view-codes themes flow-subs click-params])) ;; not faster, but just less retarded than above
         ]
     codes)))

(defn get-surrounding-tokens [cm cursor token]
  (try
    (let [line (.-line cursor)
          token (try (.-string token) (catch :default _ (str token)))
          line-string (.getLine cm line)
          prev-line-string (if (> line 0) (.getLine cm (dec line)) "")
          next-line-string (if (< line (dec (.lineCount cm))) (.getLine cm (inc line)) "")
          big-line-string (str prev-line-string " " line-string " " next-line-string)]
      [token big-line-string])
    (catch :default e (ut/tapp>> [:err (str e)]))))

;; (defn autocomplete-suggestions
;;   [input all-keywords]
;;   (let [end-with-gt? (cstr/ends-with? input ">")
;;         input-clean (if end-with-gt? (subs input 0 (dec (count input))) input)
;;         input-parts (ut/splitter input-clean #">")
;;         input-depth (count input-parts)]
;;     (->> all-keywords
;;          ;; Filter keywords to start with the cleaned input (excluding trailing '>').
;;          (filter #(cstr/starts-with? % input-clean))
;;          ;; Map each keyword to limit its depth appropriately.
;;          (map (fn [keyword]
;;                 (let [k-parts (ut/splitter keyword #">")
;;                       shown-depth (if end-with-gt? (inc input-depth) input-depth)]
;;                   (cstr/join ">" (take shown-depth k-parts)))))
;;          (distinct) ;; Remove duplicates after processing.
;;          ;; Append '>' to indicate further sublevels only when appropriate.
;;          (map #(let [splits (ut/splitter % #">")]
;;                  (if (and end-with-gt? (< input-depth (count splits)))
;;                    (str % ">")
;;                    %)))
;;          ;; Convert to vector for consistent output.
;;          (vec))))


(defn autocomplete-suggestions-fn
  [input all-keywords]
  (let [end-with-gt? (cstr/ends-with? input ">")
        input-clean (if end-with-gt? (subs input 0 (dec (count input))) input)
        input-parts (ut/splitter input-clean #">")
        input-depth (count input-parts)]
    (->> all-keywords
         ;; Filter keywords to start with the cleaned input (excluding trailing '>').
         (filter #(cstr/starts-with? % input-clean))
         ;((fn [x] (take 300 x))) ;;; ?
         ;; Collect keywords up to the intended depth or one more level.
         (map (fn [keyword]
                (let [k-parts (ut/splitter keyword #">")
                      shown-depth (if end-with-gt? (inc input-depth) input-depth)]
                  [(cstr/join ">" (take shown-depth k-parts))
                   ;; Check if there's a next level beyond the current showing depth.
                   (> (count k-parts) shown-depth)])))
         ;; Remove duplicates after processing.
         ;(distinct)
         ((fn [x] (set x)))
         ;; Map to append '>' if there are further levels indicated by the boolean flag.
         (map (fn [[path has-more]]
                (if has-more
                  (str path ">")
                  path)))
        ;;  ((fn [x] (take 100 x)))
         (vec))))

(defn autocomplete-suggestions [input all-keywords]
  (if (not (cstr/ends-with? input ">"))
    (let [regs (first (filter #(and (not= % input)
                                    (clojure.string/starts-with? % input)) all-keywords))
          match? (cstr/starts-with? (ut/replacer regs input "") ">")]
      (if match?
        (autocomplete-suggestions-fn (str input ">") all-keywords)
        (autocomplete-suggestions-fn input all-keywords)))
    (autocomplete-suggestions-fn input all-keywords)))



;(defonce atom-suggestions (atom []))
;;(reset! db/autocomplete-keywords (vec (sort (map str @(rfa/sub ::parameters-available {})))))

;; (def test-cases
;;   (atom
;;    [":flow/colors-of-the-moment-solver-flow--44>temp-name"
;;     ":flow/colors-of-the-moment-solver-flow--44>waiter-fn>34>565"
;;     ":flow/colors-of-the-moment-solver-flow--44>shell-command-in>34>84"
;;     ":flow/colors"
;;     ":signal/"
;;     ":flow/open"
;;     ":flow/colors-of-the-moment-solver-flow--36>"
;;     ":flow/colors-of-the-moment-solver-flow--44>shell-command-in"
;;     ":flow/colors-of-the-moment-solver-flow--44>shell-command-in>"
;;     ":flow/colors-of-the-moment-solver-flow--36"
;;     ":flow/colors-of-the-moment-solver-flow--4"
;;     ":flow/colors-of-the-moment-solver-flow--44>"
;;     ":flow/colors-of-the-moment-solver-flow--44>s"
;;     ":flow/colors-of-the-moment-solver-flow"
;;     ":flow/colors-of-the-moment-solver-"
;;     ":flow/colors-of-the-moment-solver-flow--4"
;;     ":flow/colors-of-the-moment-solver-flow--46>join-output-as-string"
;;     ":flow/colors-of-the-moment-solver-"
;;     ":flow/colors-of-the-moment"
;;     ":flow/colors-of-the-moment-solver-flow--46>clojure-string-join-1"
;;     ":flow/colors-of-the-moment-solver-flow--46>convert-to-png"
;;     ":flow/colors-of-the-moment-solver-flow--46>done"
;;     ":flow/colors-of-the-moment-solver-flow--36>temp-name"
;;     ":flow/co"
;;     ":fon"
;;     ":flow/colors-of-the-moment-solver-flow--36>shell-command-in"
;;     ":flow/colors-of-the-moment-solver-flow--36>run-id"
;;     ":flow/colors-of-the-moment-solver-flow--36>join-output-as-string"
;;     ":flow/colors-of-the-moment-solver-flow--36>clojure-str"
;;     ":flow/colors-of-the-moment-solver-flow--36>ima"
;;     ":flow/colors-of-the-moment-solvee-string-join-1"
;;     ":flow/colors-of-the-moment-solver-flow--36>convert-to-png"
;;     ":flow/colors-of-the-moment-solver-flow--36"]))

;; (defn benchmark [f arg-pairs]
;;   (let [start (js/performance.now)
;;         n 105]
;;     (doseq [[arg1 arg2] (take n (cycle arg-pairs))] (f arg1 arg2))
;;     (- (js/performance.now) start)))

;; (let [client-name @(re-frame/subscribe [::client-name])
;;       all-keywords (vec (map str @(rfa/sub ::parameters-available {})))
;;       arg-pairs (vec (for [e @test-cases] [e all-keywords]))
;;       arg-pairs2 (vec (for [e @test-cases] [e @db/autocomplete-keywords]))
;;       results {"autocomplete-suggestions" (benchmark autocomplete-suggestions arg-pairs)
;;                "autocomplete-suggestions-atom" (benchmark autocomplete-suggestions arg-pairs2)
;;                ;;"autocomplete-suggestions-atomb" (benchmark autocomplete-suggestionsb arg-pairs2)
;;                }]
;;   (ut/tapp>> [client-name (vec (sort-by val results))]))




;; (defn autocomplete-suggestions
;;   [input all-keywords]
;;   (let [end-with-gt? (cstr/ends-with? input ">")
;;         input-clean (if end-with-gt? (subs input 0 (dec (count input))) input)
;;         input-parts (ut/splitter input-clean #"/") ; Splitting at '/' for namespace consideration.
;;         base-namespace (first input-parts)
;;         base-keypath (second input-parts)]
;;     (->> all-keywords
;;          ;; Map to break down each keyword into [namespace keypath].
;;          (map (fn [kw] (let [parts (ut/splitter kw #"/")]
;;                          [(first parts) (when (> (count parts) 1) (second parts))])))
;;          ;; Filter based on namespace and start of keypath if specified.
;;          (filter (fn [[ns kp]]
;;                    (and (cstr/starts-with? ns base-namespace)
;;                         (or (not base-keypath) ; No keypath specified, match namespace.
;;                             (and kp (cstr/starts-with? kp base-keypath))))))
;;          ;; Build back the full path for keywords with further structure.
;;          (mapcat (fn [[ns kp]]
;;                    (if kp
;;                      (let [full-kpath (str ns "/" kp)
;;                            parts (ut/splitter full-kpath #">")
;;                            shown-depth (if end-with-gt? (inc (count (ut/splitter input-clean #">"))) (count (ut/splitter input-clean #">")))]
;;                        [(str (cstr/join ">" (take shown-depth parts))
;;                              (when (< shown-depth (count parts)) ">"))])
;;                      [(str ns "/")]))) ; Append '/' if namespace has deeper keypaths.
;;          (distinct) ; Remove duplicates.
;;          (remove nil?)
;;          (vec))))



;; (defn autocomplete-suggestions-fn
;;   [input all-keywords]
;;   (let [end-with-gt? (cstr/ends-with? input ">")
;;         input-clean (if end-with-gt? (subs input 0 (dec (count input))) input)
;;         input-parts (ut/splitter input-clean #"/") ; Splitting at '/' for namespace consideration.
;;         base-namespace (first input-parts)
;;         base-keypath (second input-parts)]
;;     (->> all-keywords
;;          ;; Map to break down each keyword into [namespace keypath].
;;          (map (fn [kw] (let [parts (ut/splitter kw #"/")]
;;                          [(first parts) (when (> (count parts) 1) (second parts))])))
;;          ;; Filter based on namespace and start of keypath if specified.
;;          (filter (fn [[ns kp]]
;;                    (and (cstr/starts-with? ns base-namespace)
;;                         (or (not base-keypath) ; No keypath specified, match namespace.
;;                             (and kp (cstr/starts-with? kp base-keypath))))))
;;          ;; Check if any children exist for exact matches and modify behavior accordingly.
;;          (mapcat (fn [[ns kp]]
;;                    (if kp
;;                      (let [full-kpath (str ns "/" kp)
;;                            parts (ut/splitter full-kpath #">")
;;                            has-children (some #(cstr/starts-with? % (str full-kpath ">")) all-keywords)
;;                            shown-depth (if (or end-with-gt? has-children)
;;                                          (inc (count (ut/splitter input-clean #">")))
;;                                          (count (ut/splitter input-clean #">")))]
;;                        [(str (cstr/join ">" (take shown-depth parts))
;;                              (when (< shown-depth (count parts)) ">"))])
;;                      [(str ns "/")]))) ; Only append '/' if namespace has deeper keypaths.
;;          (distinct) ; Remove duplicates.
;;          (remove nil?)
;;          (vec))))

;; (defn autocomplete-suggestions  [input all-keywords]
;;   (let [aa (autocomplete-suggestions-fn input all-keywords)]
;;     (if (and (some #(= % (str input ">")) aa) (<= (count aa) 2))
;;       (autocomplete-suggestions-fn (str input ">") all-keywords)
;;       aa)))













;; (def keywordss
;;   [":flow/colors-of-the-moment-solver-flow--44>temp-name"
;;    ":flow/colors-of-the-moment-solver-flow--44>waiter-fn>34>565"
;;    ":flow/colors-of-the-moment-solver-flow--44>shell-command-in>34>84"
;;    ":flow/colors-of-the-moment-solver-flow--44>shell-command-in>testse>testsets454"
;;    ":flow/colors-of-the-moment-solver-flow--44"
;;    ":flow/colors-of-the-moment-solver-flow--46>run-id"
;;    ":flow/colors-of-the-moment-solver-flow--46>join-output-as-string"
;;    ":flow/colors-of-the-moment-solver-flow--46>clojure-string-join-1delimiter"
;;    ":flow/colors-of-the-moment-solver-flow--46>image-path"
;;    ":flow/colors-of-the-moment-solver-flow--46>clojure-string-join-1"
;;    ":flow/colors-of-the-moment-solver-flow--46>convert-to-png"
;;    ":flow/colors-of-the-moment-solver-flow--46>done"
;;    ":flow/colors-of-the-moment-solver-flow--36>temp-name"
;;    ":flow/colors-of-the-moment-solver-flow--36>waiter-fn"
;;    ":flow/colors-of-the-moment-solver-flow--36>shell-command-in"
;;    ":flow/colors-of-the-moment-solver-flow--36>run-id"
;;    ":flow/colors-of-the-moment-solver-flow--36>join-output-as-string"
;;    ":flow/colors-of-the-moment-solver-flow--36>clojure-string-join-1delimiter"
;;    ":flow/colors-of-the-moment-solver-flow--36>image-path"
;;    ":flow/colors-of-the-moment-solver-flow--36>clojure-string-join-1"
;;    ":flow/colors-of-the-moment-solver-flow--36>convert-to-png"
;;    ":flow/colors-of-the-moment-solver-flow--36>done"])

;; (ut/tapp>> [:autocomplete-test! (into {}
;;                                 (for [ss [":flow/colors-of-the-moment-solver-flow--36>"
;;                                           ":flow/colors-of-the-moment-solver-flow--44>shell-command-in"
;;                                           ":flow/colors-of-the-moment-solver-flow--44>shell-command-in>"
;;                                           ":flow/colors-of-the-moment-solver-flow--36"
;;                                           ":flow/colors-of-the-moment-solver-flow--44"
;;                                           ":flow/colors-of-the-moment-solver-flow--44>"
;;                                           ":flow/colors-of-the-moment-solver-flow--44>s"
;;                                           ":flow/colors-of-the-moment-solver-flow"]]
;;                                   {ss (autocomplete-suggestions ss keywordss)}))])

(defonce last-view-highlighted-hash (atom "lets-fucking-go!"))

(re-frame/reg-sub
 ::panel-code-up?
 (fn [db _]
   (let [editor? (get db :editor? false)
         part-kp @(ut/tracked-sub ::editor-panel-selected-view {})
         view-code-hash (hash [(get-in db [:panels (get db :selected-block)])  part-kp])
         old-view-code-hash @last-view-highlighted-hash]
    ;;  (ut/tapp>> [:runhighlioght? (and editor? (not= view-code-hash old-view-code-hash))])
     (and editor? (not= view-code-hash old-view-code-hash))
     ;;true
     )))


(defn custom-hint-simple-fn [cm input-token]
  (let [cursor (.getCursor cm)
        token (.getTokenAt cm cursor)]
    ;(ut/tapp>> ["Current token:" (.-string token) ":input token:" input-token]) ;; Debugging
    ;; Define your list of suggestions here
    (let [input (.-string token) ;;(or input-token (.-string token))
          input (if (string? input-token) input-token input)
          ;surrounds (get-surrounding-tokens cm cursor token)
          ;_ (ut/tapp>> [:surrounds-simp surrounds])
          list @db/autocomplete-keywords ;;(vec (map str @(rfa/sub ::parameters-available {})))
          ;list [":keyword1" ":k!" ":keyword2" ":keyword44" ":kiwi/farts" ":liwi/limes"]
          ;filtered-list (sort (filter #(clojure.string/starts-with? % input) list)) ;; Dynamic filtering
          filtered-list (sort (autocomplete-suggestions input list))
    ]
      ;; Return a hint object expected by CodeMirror
      (clj->js {:list filtered-list
                :from (clj->js {:line (.-line cursor) :ch (.-start token)})
                :to (clj->js {:line (.-line cursor) :ch (.-end token)})}))))

(defn custom-hint-fn [cm input-token & [start-line start-ch end-ch]]
  (let [cursor (clj->js {:line start-line :ch start-ch}) ;; Create a cursor object from start-line and start-ch
        token (.getTokenAt cm cursor)
        input (if (string? input-token) input-token (.-string token)) ;; Use input-token if it's a string, otherwise use the current token
        ;surrounds (get-surrounding-tokens cm cursor input-token)
        ;_ (ut/tapp>> [:surrounds surrounds])
        list @db/autocomplete-keywords ;; (vec (map str @(rfa/sub ::parameters-available {})))
        ;;filtered-list (sort (filter #(clojure.string/starts-with? % input) list))] ;; Filter the list with the input
        filtered-list (sort (autocomplete-suggestions input list))
  ]
         ;(ut/tapp>> [:custom-hint-called input-token start-line start-ch end-ch])
    ;; Return a hint object expected by CodeMirror
    (clj->js {:list filtered-list
              :from cursor
              :to (clj->js {:line start-line :ch end-ch}) ;; Use start-line and end-ch to create the to cursor
              :pick (fn [cm hint]
                      (let [doc (.getDoc cm)
                            ;; Calculate the end position of the replacement
                            replacement-end-ch (+ start-ch (count hint))]
                        (.replaceRange doc hint cursor (clj->js {:line start-line :ch end-ch}))
                        ;; Set the selection to the end of the replacement without scrolling
                        (js/setTimeout #(do (.setSelection doc (clj->js {:line start-line :ch replacement-end-ch}) (clj->js {:line start-line :ch replacement-end-ch}) (clj->js {:scroll false}))
                                             ;; Force a blur on the CodeMirror instance
                                             ;; (.blur cm)
                                            ) 10)))
              :close (fn [cm]
                       (js/setTimeout #(do (.focus cm)) 100))})))

(defn can-be-autocompleted? [token-string]
  (let [list @db/autocomplete-keywords ;;(vec (map str @(rfa/sub ::parameters-available {})))
        can? (ut/ne? (filter #(clojure.string/starts-with? % token-string) list))
        _ (ut/tapp>> [:can-be-autocompleted? can? token-string])]
    ;;(contains? list token-string)
    can?
    ))

;; (defn remove-highlight-watcher []
;;   (remove-watch db/cm-instance-panel-code-box :highlight-panel-code))

;; (defn add-highlight-watcher []
;;   ;(remove-highlight-watcher)
;;   (add-watch db/cm-instance-panel-code-box :highlight-panel-code
;;              (fn [_ _ _ _]
;;                (ut/tracked-dispatch [::highlight-panel-code]))))

;; (add-highlight-watcher)



(defn panel-code-box [_ _ width-int height-int value]
  (let [sql-hint? (cstr/includes? (str value) ":::sql-string")
        ;selected-block @(ut/tracked-sub ::selected-block {})
        selected-kp @(ut/tracked-sub ::editor-panel-selected-view {})
        ;extra-kp (into [selected-block] selected-kp)
        selected-kp (if (nil? (first selected-kp)) nil selected-kp)
        key selected-kp
        font-size (if (nil? key) 13 17)
        code-width (if (nil? key) (- width-int 24) (- width-int 130))]
    ;(ut/tapp>> [:code-panel key selected-kp])
    [re-com/box
     :size "none"
     :width (px (- width-int 24))
     :height (px (- height-int 24))
     :style {:font-family      (theme-pull :theme/monospaced-font nil) ; "Chivo Mono" ;"Fira Code"
             :font-size        (px font-size)
             :overflow         "auto"
             :background-color (if @db/bad-form? "#8b000075" "inherit")
             ;:transform "scale(0.5)"
             :border-radius    "12px"
             :font-weight      700}
     :child [(reagent/adapt-react-class cm/UnControlled)
             {:value (ut/format-map code-width (str value)) ;; value will be pre filtered by caller
              :onBeforeChange (fn [editor _ _] ;; data value]
                                (reset! db/cm-instance-panel-code-box editor)
                                ;(ut/tracked-dispatch [::highlight-panel-code])
                                )
              :onFocus (fn [_] (reset! db/cm-focused? true))
              ;; :onMount (fn [_] (ut/tracked-dispatch [::highlight-panel-code]))
              :onBlur  #(let [_            (reset! db/cm-focused? false)
                              parse        (try (read-string
                                                  ;(ut/rs-save-floats ;-preserve-floats
                                                 (cstr/join " " (ut/cm-deep-values %)))
                                                (catch :default e [:cannot-parse (str (.-message e))]))
                              selected-kp @(ut/tracked-subscribe [::editor-panel-selected-view])
                              selected-kp (if (nil? (first selected-kp)) nil selected-kp)
                              key selected-kp
                              unparseable? (= (first parse) :cannot-parse)]
                          (if unparseable?
                            ;                          (js/alert "BAD FORM!!")
                            (do (reset! db/bad-form? true)
                                (reset! db/bad-form-msg (str (last parse))))
                                ;(js/alert (str (last parse)))

                            (do (reset! db/bad-form? false)
                                (if (nil? key)
                                  (ut/tracked-dispatch-sync [::update-selected parse])
                                  (ut/tracked-dispatch-sync [::update-selected-key-cons key parse]))
                                (ut/dispatch-delay [::highlight-panel-code] 200))))
              :onInputRead (fn [cm]
                             ;(ut/tapp>> "onInputRead called")
                             (let [cursor (.getCursor cm)
                                   token (.getTokenAt cm cursor)
                                   token-string (.-string token)
                                   token-end (= (.-ch cursor) (.-end token))]
                               ;(ut/tapp>> [:cc token-string])
                               (when (or (= token-string ":")
                                         (and token-end
                                              (can-be-autocompleted? token-string)
                                              ;true
                                              ))
                                 ;(ut/tapp>> [:hit-auto?])
                                 (js/setTimeout (fn []
                                                  (.execCommand cm "autocomplete")) 0))))
              :options {:mode              (if sql-hint? "sql" "clojure")
                        :hintOptions {:hint custom-hint-simple-fn}
                        :lineWrapping      true
                        :lineNumbers       true
                        :matchBrackets     true
                        :autoCloseBrackets true
                        :autofocus         false
                        :autoScroll        false
                        :detach            true
                        :readOnly          false
                        :extraKeys (clj->js {"Ctrl-Right" (fn [^js cm]
                                                            (let [cursor (.getCursor cm)
                                                                  match (.findMatchingBracket cm cursor true)]
                                                              (if (and match (.-match match))
                                                                (.setCursor cm (clj->js {:line (.-line (.-to match)) :ch (+ (.-ch (.-to match)) 1)}))
                                                                (.execCommand cm "goGroupRight"))))
                                             "Shift-Ctrl-Right" (fn [^js cm]
                                                                  (let [cursor (.getCursor cm)
                                                                        match (.findMatchingBracket cm cursor true)]
                                                                    (if (and match (.-match match))
                                                                      (.setSelection cm cursor (clj->js {:line (.-line (.-to match)) :ch (+ (.-ch (.-to match)) 1)}))
                                                                      (.execCommand cm "goGroupRight"))))
                                             "Ctrl-Left"  (fn [^js cm]
                                                            (let [cursor (.getCursor cm)
                                                                  match (.findMatchingBracket cm cursor true)]
                                                              (if (and match (.-match match))
                                                                (.setCursor cm (.-to match))
                                                                (.execCommand cm "goGroupLeft"))))
                                             "Shift-Ctrl-Left"  (fn [^js cm]
                                                                  (let [cursor (.getCursor cm)
                                                                        match (.findMatchingBracket cm cursor true)]
                                                                    (if (and match (.-match match))
                                                                      (.setSelection cm cursor (.-to match))
                                                                      (.execCommand cm "goGroupLeft"))))})
                        :theme             (theme-pull :theme/codemirror-theme nil)}}]]))



(declare map-boxes2)




(re-frame/reg-event-db
 ::highlight-panel-code
 (fn [db _]
   (let [flow-subs (get db :flow-subs)
         [_ data-key] @(ut/tracked-sub ::editor-panel-selected-view {})
         selected-block (get db :selected-block)
         value-spy?      (get-in @db/value-spy [selected-block data-key] false)
         click-params (vec (for [e (keys (get-in db [:click-param :param]))]
                             (keyword (str "param/" (ut/replacer (str e) ":" "")))))
         themes (vec (for [e (keys (get-in db [:click-param :theme]))]
                       (keyword (str "theme/" (ut/replacer (str e) ":" "")))))
         codes (vec (into themes (into flow-subs click-params)))
         part-kp @(ut/tracked-sub ::editor-panel-selected-view {})
         view-code-hash (hash [(get-in db [:panels (get db :selected-block)])  part-kp])]

     (reset! last-view-highlighted-hash view-code-hash)

    ;;  (when (cstr/includes? (str (get db :client-name)) "-short-")
    ;;    (ut/tapp>> [:highlight-panel-code!! (get db :client-name) view-code-hash]))

     (if (not value-spy?)
       (highlight-codes-only codes {;:color "white"
                                    :background-color (str (ut/invert-hex-color (get (theme-pull :theme/data-colors nil) "keyword")))
                                    :filter "invert(1.2)"
                                    :cursor "pointer"
                                  ;:filter (str "invert(1.2)" (when @db/param-code-hover " outer-glow(0px 0px 10px white)"))
                                  ;:font-weight 700
                                    :border-radius "5px"})

       (highlight-codes-values (into {}
                                     (for [c codes] {c @(ut/tracked-sub ::conn/clicked-parameter-key-alpha
                                                                        {:keypath [(try (edn/read-string c) (catch :default _ c))]})}))
                               {;:color "white"
                                :background-color (str (ut/invert-hex-color (get (theme-pull :theme/data-colors nil) "keyword")))
                                :filter "invert(1.2)"
                                :cursor "pointer"
                              ;:filter (str "invert(1.2)" (when @db/param-code-hover " outer-glow(0px 0px 10px white)"))
                              ;:font-weight 700
                                :border-radius "5px"})))
   db))


(defn open-input-code-box [kp width-int height-int value syntax & [style]]
  (let [;sql-hint? (cstr/includes? (str value) ":::sql-string")
        syntax (or (if (= syntax "raw (clojure)") "clojure" syntax) "clojure")
        stringify? (not (= syntax "clojure"))]
        ;kp        [:panels panel-id :queries query-key :select idx]

    [re-com/box
     :size "auto"
     :width (px (- width-int 24))
     :height (px (- height-int 24))
     :style (merge
             {:font-family   (theme-pull :theme/monospaced-font nil) ; "Chivo Mono" ;"Fira Code"
              :font-size     "16px"
              :overflow      "auto"
             ;:border-radius "12px"
              :background-color "#00000000"
              :font-weight   700} style)
     :child [(reagent/adapt-react-class cm/UnControlled)
             {;; :value   (ut/format-map (- width-int 24)
              ;;                         (str value))
              :value   (if stringify?
                         (str (cstr/join "\n" value))
                         (ut/format-map (- width-int 24)
                                        (str value)))
              :onBlur #(let [rr (ut/cm-deep-values %)
                             rrd (try (read-string (cstr/join " " rr))
                                      (catch :default _ (pr-str (cstr/join "\n" rr))))]
                         (ut/tapp>> [:open-input stringify? kp rr rrd value])
                         (if stringify?
                           (when (not= value rr) (ut/tracked-dispatch [::conn/click-parameter kp rr]))
                           (ut/tracked-dispatch [::conn/click-parameter kp rrd])))
              :options {:mode              syntax
                        :lineWrapping      true
                        :lineNumbers       false ;true
                        :matchBrackets     true
                        :autoCloseBrackets true
                        :autofocus         false
                        :autoScroll        false
                        :detach            true
                        :readOnly          false            ;true
                        :theme             (theme-pull :theme/codemirror-theme nil)}}]])) ;"ayu-mirage" ;"hopscotch"


(defn field-code-box [panel-id query-key idx width-int height-int value]
  (let [sql-hint? (cstr/includes? (str value) ":::sql-string")
        kp        [:panels panel-id :queries query-key :select idx]]
    [re-com/box
     :size "auto"
     :width (px (- width-int 24))
     :height (px (- height-int 24))
     :style {:font-family   (theme-pull :theme/monospaced-font nil) ; "Chivo Mono" ;"Fira Code"
             :font-size     "16px"
             :overflow      "auto"
             :border-radius "12px"
             :font-weight   700}
     :child [(reagent/adapt-react-class cm/UnControlled)
             {:value   (ut/format-map (- width-int 24)
                                      (str value))
              :onBlur  #(ut/tracked-dispatch-sync [::update-selected-field kp (read-string (cstr/join " " (ut/cm-deep-values %)))])
              :options {:mode              (if sql-hint? "sql" "clojure")
                        :lineWrapping      true
                        :lineNumbers       true
                        :matchBrackets     true
                        :autoCloseBrackets true
                        :autofocus         false
                        :autoScroll        false
                        :detach            true
                        :readOnly          false            ;true
                        :theme             (theme-pull :theme/codemirror-theme nil)}}]])) ;"ayu-mirage" ;"hopscotch"


(defn panel-param-box [type-key key width-int height-int value]
  (let [sql-hint? (cstr/includes? (str value) ":::sql-string")]
    [re-com/box
     :size "none"
     :width (px (- width-int 24))
     :height (px (- height-int 24))
     :style {:font-family   (theme-pull :theme/monospaced-font nil) ;"Fira Code" ; "Chivo Mono" ;"Fira Code"
             :font-size     "13px"
             :overflow      "auto"
             :border-radius "12px"
             ;:background-color "darkcyan" ;; THEME likely
             :font-weight   700}
     :child [(reagent/adapt-react-class cm/UnControlled)
             {:value   (ut/format-map (- width-int 24)
                                      (str (if (nil? key) value
                                               (get value key))))
              ;:value (str (if (nil? key) value
              ;                (get value key)))
              :onBlur  #(ut/tracked-dispatch [::set-user-parameters type-key
                                            (read-string (cstr/join " " (ut/cm-deep-values %)))])
              ;:onBlur #(doseq [[k v] (read-string (cstr/join " " (ut/cm-deep-values %)))]
              ;           (ut/tracked-dispatch [::conn/click-parameter [type-key k] v]))
              ;:onBlur #(if (nil? key)
              ;           (ut/tracked-dispatch-sync [::update-selected (read-string (cstr/join " " (ut/cm-deep-values %)))])
              ;           (ut/tracked-dispatch-sync [::update-selected-key key (read-string (cstr/join " " (ut/cm-deep-values %)))]))
              :options {:mode              (if sql-hint? "sql" "clojure")
                        :lineWrapping      true
                        :lineNumbers       true
                        :matchBrackets     true
                        :autoCloseBrackets true
                        :autofocus         false
                        :autoScroll        false
                        :detach            true
                        :readOnly          false            ;true
                        :theme             (theme-pull :theme/codemirror-theme nil)}}]])) ;"ayu-mirage" ;"hopscotch"


(defn read-only-sql-box [width-int height-int value]
  [re-com/box
   :size "auto"
   :width (px (- width-int 24))
   :max-height (px (- height-int 24))
   :style {;:background-color "#00000085"
           :font-family   (theme-pull :theme/monospaced-font nil) ; "Fira Code" ;"Chivo Mono" ;"Ubuntu" ;"Fira Code"
           ;:opacity 0.7
           :margin-left   "9px"
           :font-size     "13px"
           :overflow      "hidden"
           :border-radius "12px"
           :font-weight   400}
   :child [(reagent/adapt-react-class cm/UnControlled)
           {:value value
            ;;:value   (ut/replacer value #"\\n" "\n") ;(cstr/join "\n" (ut/splitter value #"\\n")) ;(str value)
            :options {:mode              "sql"
                      :lineWrapping      true               ;false
                      :lineNumbers       false
                      :matchBrackets     true
                      :autoCloseBrackets true
                      :autofocus         false
                      :autoScroll        false
                      :detach            true
                      :readOnly          true
                      :theme             (theme-pull :theme/codemirror-theme nil)}}]]) ;"ayu-mirage"; "hopscotch"


(defn read-only-clojure-box [width-int height-int value]
  [re-com/box
   :size "auto"
   :padding "3px"
   ;:width (px (- width-int 24))
   ;:height (px (- height-int 24))
   :style {:background-color "#008B8B22"
           ;:overflow-x "hidden"
           ;:margin-top "13px"
           ;:margin-left "20px"
           :border           "1px solid #008B8B55"
           :border-radius    "16px"
           :font-family      (theme-pull :theme/monospaced-font nil) ; "Fira Code" ;"Chivo Mono" ;"Ubuntu" ;"Fira Code"
           :font-size        "13px"
           :overflow-y       "auto"
           :overflow-x       "hidden"
           :font-weight      400}
   :child [(reagent/adapt-react-class cm/UnControlled)
           {:value   (ut/format-map (- width-int 0) (str value))
            :options {:mode              "clojure"
                      :lineWrapping      true
                      :lineNumbers       false
                      :matchBrackets     true
                      :autoCloseBrackets true
                      :autofocus         false
                      :autoScroll        false
                      :detach            true
                      :readOnly          true
                      :theme             (theme-pull :theme/codemirror-theme nil)}}]]) ;"ayu-mirage"; "hopscotch"


(defn read-only-clojure-box-un [value]
  [re-com/box
   :size "none"
   :style {:font-family   (theme-pull :theme/monospaced-font nil) ; "Chivo Mono" ;"Ubuntu" ;"Fira Code"
           :font-size     "14px"
           :overflow-y    "hidden"                          ;"auto"
           :border-radius "12px"
           :overflow-x    "hidden"
           :font-weight   700}
   :child [(reagent/adapt-react-class cm/UnControlled)
           {:value   (str value)                            ;(format-map (- width-int 0) (str value))
            :options {:mode              "clojure"
                      :lineWrapping      true
                      :lineNumbers       false
                      :matchBrackets     true
                      :autoCloseBrackets true
                      :autofocus         false
                      :autoScroll        false
                      :detach            true
                      :readOnly          true
                      :theme             (theme-pull :theme/codemirror-theme nil)}}]]) ;"ayu-mirage"; "hopscotch"


(defn panel-code-box-single [panel-id key width-int height-int]
  (let [value     @(ut/tracked-subscribe [::workspace [panel-id key]])
        sql-hint? (cstr/includes? (str value) ":::sql-string")]
    [re-com/box
     :size "none"
     :width (px (- width-int 24))
     :height (px (- height-int 24))
     :style {:background-color "#00000085"
             :font-family      (theme-pull :theme/monospaced-font nil) ; "Fira Code" ;"Chivo Mono" ;"Ubuntu" ;"Fira Code"
             :font-size        "13px"
             :border-radius    "12px"
             :overflow         "auto"
             :font-weight      700}
     :child [(reagent/adapt-react-class cm/UnControlled)
             {:value   (ut/format-map (- width-int 24) (str value)) ;value
              :onBlur  #(let [parse        (try (read-string (cstr/join " " (ut/cm-deep-values %)))
                                                (catch :default _ :cannot-parse))
                              unparseable? (= parse :cannot-parse)]
                          (if unparseable? (js/alert "BAD FORMS!")
                              (ut/tracked-dispatch-sync [::update-workspace [panel-id key] parse])))
              :options {:mode              (if sql-hint? "sql" "clojure")
                        :lineWrapping      true
                        :lineNumbers       true
                        :matchBrackets     true
                        :autoCloseBrackets true
                        :autofocus         false
                        :autoScroll        false
                        :detach            true
                        :readOnly          false            ;true
                        :theme             "hopscotch"}}]]))

;; (defn sql-data-one-off [keypath honey-sql]                  ;; yeah, no good. lol
;;   (ut/tracked-dispatch [::wfx/request :default
;;                       {:message     {:kind       :honey-call
;;                                      :ui-keypath keypath
;;                                      :honey-sql  honey-sql
;;                                      :extras     {}}
;;                        :on-response [::http/socket-response]
;;                        ;:on-timeout [::failed-get-users]
;;                        :timeout     500000}])
;;   (ut/tracked-dispatch [::add-to-sql-history keypath honey-sql])
;;   @(ut/tracked-subscribe [::conn/sql-data keypath]))

(re-frame/reg-sub
 ::meta-from-param-name
 (fn [db {:keys [param-name]}]
   (let [psplit    (ut/splitter (ut/safe-name (str param-name)) "/")
         table     (keyword (ut/replacer (first psplit) #":" ""))
         field     (keyword (last psplit))
         fmetadata (get-in db [:meta table :fields field])]
      ;(ut/tapp>> [:meta-lkup table field fmetadata])
     (cond
       (= table :condi)                                    ;; condis are special
       {:data-type (ut/data-typer                          ;; todo: change this hardcoding when we move to global calcs
                    (condp = (get-in (get-in db [:post-condi field]) [0 :v])
                      1 true
                      0 false
                      :else (get-in (get-in db [:post-condi field]) [0 :v])))}
       (nil? fmetadata)                                    ;; else assume user-space param
       {:data-type (ut/data-typer (get-in db [:click-param table field]))}
       :else fmetadata))))                                    ;; else get the post meta


(re-frame/reg-sub
 ::meta-from-query-name
 (fn [db [_ query-name]]
   (get-in db [:meta query-name])))

(re-frame/reg-sub
 ::panel-from-query-name
 (fn [db [_ query-name]]
   (try (first (remove nil? (for [[k v] (get db :panels)]
                              (when (not (nil? (get-in v [:queries query-name]))) k))))
        (catch :default e nil))))

(re-frame/reg-sub
 ::clover-params-in-view
 (fn [db _]
   (let [part-kp @(ut/tracked-sub ::editor-panel-selected-view {})
         panel (get-in db (vec (into [:panels (get db :selected-block)] (vec part-kp))))
        ;;  parts (vec (filter
        ;;              ;#(and (keyword? %) (cstr/includes? (str %) "/"))
        ;;              ut/process-key
        ;;              (ut/deep-flatten panel)))
         parts (vec (ut/get-compound-keys panel))
         ;;_ (ut/tapp>> [:pp parts])
         ]
     parts)))



(defonce searcher-atom (reagent/atom nil))

(defn filter-search [x y] ;; search-str all-items
  (if (or (nil? x) (empty? x))
    y
    (if (map? y)
      (into {} (filter (fn [[k v]]
                         (or (cstr/includes? (cstr/lower-case (str k)) (cstr/lower-case x))
                             (cstr/includes? (cstr/lower-case (str v)) (cstr/lower-case x))))
                       y))
      (vec (filter #(cstr/includes? (cstr/lower-case (str %)) (cstr/lower-case x)) y)))))

(defn click-param-browser [click-params width-int height-int]
  (let [ph                  @param-hover                    ;; reaction hack
        pf                  @db/param-filter
        selected-block      @(ut/tracked-subscribe [::selected-block])
        current-tab-queries (try                            ;(map #(-> % ut/sql-keyword str) @(ut/tracked-subscribe [::current-tab-queries]))
                              (into (into (vec @(ut/tracked-subscribe [::current-tab-queries]))
                                          (vec @(ut/tracked-subscribe [::current-tab-blocks])))
                                    (vec @(ut/tracked-subscribe [::current-tab-condis])))
                              (catch :default _ []))
        clover-params        (if @db/cm-focused? @(ut/tracked-sub ::clover-params-in-view {}) {})
        ;panel-queries       @(ut/tracked-subscribe [::panel-sql-call-keys selected-block])
        ;panel-q-str         (str (first panel-queries))
        ]



   ;; (ut/tapp>> [current-tab-queries click-params])

    (if false ;@db/param-code-hover

      [re-com/v-box
       :padding "6px"
       :width (px (- width-int 33))
       ;:style {:border "1px solid green"}
       :align :center :justify :between
       :gap "10px"
       :children [[re-com/box
                   :style {:font-size "14px"
                           :font-weight 700
                           :border-radius "5px"
                           :color (ut/invert-hex-color (get (theme-pull :theme/data-colors nil) "keyword")) ;; (theme-pull :theme/editor-font-color nil)
                           :padding "4px"
                           :background-color (get (theme-pull :theme/data-colors nil) "keyword")
                           }
                   :align :center :justify :center
                   :child (str @db/param-code-hover)]

                  (let [code @db/param-code-hover
                        code (try (edn/read-string code) (catch :default _ code))
                        vv @(rfa/sub ::conn/clicked-parameter-key-alpha {:keypath [code]})]
                    [re-com/box
                     :size "auto"
                     :width "100%"
                     ;:width (px (- width-int 33))
                     ;:align :center :justify :center
                     :style {:font-size "15px"}
                                                                ;;:child (pr-str vv)
                     :child [map-boxes2 vv nil "" [] nil nil]
                     ;[shape/map-boxes2 vv nil "" [] nil "map"]
                                                                ;;:child [map-boxes2 vv nil nil [] :output nil]
                     :style {;:color "white"
                                                                        ;:color (str (ut/invert-hex-color (get (theme-pull :theme/data-colors nil) "keyword")))
                                                                        ;:background-color (get (theme-pull :theme/data-colors nil) "keyword")
                             :border-radius "4px"}])
                  ;[map-boxes2 {:test @db/param-code-hover} nil "" [] nil nil]
                  ]]

     [re-com/v-box
     :children
     [


      ;; [re-com/box
      ;;  :child "search me daddy"
      ;;  :height "33px"
      ;;  :width (px (- width-int 33))
      ;;  :style {:border "1px solid green"}]

      [re-com/h-box
       :padding "6px"
       :height "33px"
       :width (px (- width-int 33))
       :align :center :justify :between
       ;:style {:border "1px solid orange" }
       :children (if @db/cm-focused?
                   [[re-com/box
                     :size "auto"
                     :align :center :justify :center
                     :style {:color "#ffffff99" :font-size "14px" :font-weight 500}
                     :child "clover params used in this view"]]
                   [[re-com/input-text
                     :src (at)
                     :model             searcher-atom
                     :width             "93%"
                     :on-change #(reset! searcher-atom (let [vv (str %) ;; (cstr/trim (str %))
                                                             ]
                                                         (if (empty? vv) nil vv)))
                     ;:validation-regex  flow-id-regex
                     :placeholder "(search parameters)"
                     :change-on-blur?   false
                     :style  {:text-decoration (when (ut/ne? @searcher-atom) "underline")
                              :color "inherit"
                              :border "none"
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
                     :child "x"]])]

      [re-com/box
       :size "none"
       :width (px (- width-int 24))
       :height (px (- height-int 103))
       :style {:overflow-y  "auto"
               :padding-top "4px"
             ;:border "1px solid yellow"
               :overflow-x  "hidden"}
       :child [re-com/v-box
               :gap "10px"
               :children (for [grp click-params] ;; [grp (if @db/param-code-hover [{}] click-params)]
                           (let [filtered-params (into {} (filter #(and ;; filter out from the top bar toggle first....
                                                                    (not (= (str (first %)) ":/")) ;; sometimes a garbo param sneaks in, harmless, but lets avoid rendering it
                                                                    (not (cstr/starts-with? (str (first %)) ":conn-list/"))
                                                                    (not (cstr/starts-with? (str (first %)) (if (get pf :theme) ":theme/***" ":theme/")))
                                                                    (not (cstr/starts-with? (str (first %)) (if (get pf :user) ":param/***" ":param/")))
                                                                    (not (cstr/starts-with? (str (first %)) (if (get pf :condis) ":condi/***" ":condi/")))
                                                                    (if (get pf :this-tab? true)
                                                                      (some (fn [x] (cstr/starts-with? (str (first %)) (str ":" x))) current-tab-queries)
                                                                      true)

                                                                   ;   (not (cstr/starts-with? (str (first %)) ":tables-sys/"))
                                                                    (not (cstr/includes? (str (first %)) "-sys/")))
                                                                   ; (not (cstr/starts-with? (str (first %)) (if (= panel-q-str "")
                                                                   ;                                           "none!" panel-q-str)))
                                                                   ;   (not (cstr/starts-with? (str (first %)) ":connections-sys/"))
                                                                   ;   (not (cstr/starts-with? (str (first %)) ":fields-sys/"))

                                                                  grp))
                                 filter-keys (vec (filter-search @searcher-atom (keys filtered-params)))
                                 filtered-params (select-keys filtered-params filter-keys)

                                ;;  filtered-params (if @db/param-code-hover
                                ;;                    (let [code @db/param-code-hover
                                ;;                          code (try (edn/read-string code) (catch :default _ code))
                                ;;                          vv @(rfa/sub ::conn/clicked-parameter-key-alpha {:keypath [code]})]
                                ;;                      {code vv})
                                ;;                    filtered-params)

                                 filtered-params (if @db/cm-focused?
                                                   (select-keys grp clover-params)
                                                   filtered-params)
                                ;;  _ (ut/tapp>> [:wtf (if @db/cm-focused?
                                ;;                  (select-keys (apply concat click-params) (keys clover-params))
                                ;;                  filtered-params)])
                                ;;  _ (ut/tapp>> [:grp clover-params grp])



                                 is-not-empty?   (ut/ne? (remove empty? filtered-params))]
                           ;(ut/tapp>> [filtered-params (empty? (remove empty? filtered-params)) (into {} filtered-params)])
                             (when is-not-empty?
                               [re-com/v-box
                              ;:gap "1px"
                                :style {:cursor           "grab"
                                      ;:background-color "#0b1122"
                                        :background-color (theme-pull :theme/editor-param-background-color nil)}
                                :children (for [[k v] filtered-params]
                                            (let
                                             [meta        @(ut/tracked-sub ::meta-from-param-name {:param-name k})
                                              dtype       (get meta :data-type)
                                              dtype       (try (if (and (= dtype "vector") (every? string? v)) "string" dtype)
                                                               (catch :default _ dtype)) ;; since stringified code is technically not a string yet...
                                                               ;; ^^ its a vector of strings so it can be code edited properly...
                                                               ;; kinda fucky, but makes sense (also we do this in grid.cljs in the view tab drag out - line ~2200)
                                           ;; dtype       (if (nil? dtype) (get (ut/data-typer v) :data-type) dtype) ;; direct lookup?
                                              dcolor      (get @(ut/tracked-sub ::conn/data-colors {}) dtype)
                                              psplit      (ut/splitter (ut/safe-name (str k)) "/")
                                            ;table       (keyword (ut/replacer (ut/replacer (first psplit) #":" "") ".*" ""))
                                              table       (-> (first psplit) (ut/replacer  #":" "") (ut/replacer  ".*" "") keyword)
                                              field       (keyword (last psplit))
                                              param-value (str @(ut/tracked-subscribe [::conn/clicked-parameter [table field]]))
                                              is-image?   (and
                                                           (or (cstr/ends-with? (cstr/lower-case (str param-value)) ".png")
                                                               (cstr/ends-with? (cstr/lower-case (str param-value)) ".jpg")
                                                               (cstr/ends-with? (cstr/lower-case (str param-value)) ".jpeg")
                                                               (cstr/ends-with? (cstr/lower-case (str param-value)) ".gif")
                                                               (cstr/includes? (cstr/lower-case (str param-value)) "/images/")
                                                               (cstr/includes? (cstr/lower-case (str param-value)) "/image/"))
                                                           (cstr/starts-with? (cstr/lower-case (str param-value)) "http"))
                                              is-b64?      (ut/is-base64? (str param-value))
                                              is-video?     (or (cstr/ends-with? (cstr/lower-case (str param-value)) ".mp4")
                                                                (cstr/ends-with? (cstr/lower-case (str param-value)) ".mov"))
                                              selected?   @(ut/tracked-subscribe [::has-query? selected-block table])
                                              pwidth      (js/Math.floor (/ (count (str param-value)) 1.7))
                                              pwidth      (cond (> pwidth 30) 30
                                                                (< pwidth 6) 6
                                                                :else pwidth)
                                              pheight     (js/Math.floor (/ pwidth 30))
                                              pheight     (cond (> pheight 3) 3
                                                                (< pheight 1) 1
                                                                :else pheight)
                                              hovered?    (= k (first @param-hover))]
                                            ;; param-value (if is-b64?
                                            ;;               "**huge base64 string**"
                                            ;;               param-value)

                                            ;(ut/tapp>> [:param-table table])
                                            ;(ut/tapp>> @param-hover)

;[re-com/box :size "auto"  ;;; 26
                                            ;:child
                                              (draggable      ;(sql-spawner-filter :param [k table field])
                                               {:h         (cond is-image? 6 is-video? 9 :else (+ 2 pheight))
                                                :w         (cond is-image? 6 is-video? 13 :else pwidth)
                                                :root      [0 0]
                                                :drag-meta {:type        :param
                                                            :param-full  k
                                                            :param-type  dtype
                                                            :param-table table
                                                            :param-field field}}
                                               "meta-menu"

                                               [re-com/v-box
                                                :size "auto"
                                              ;:width (px (- width-int 33))
                                                :attr {:on-mouse-enter #(reset! param-hover [k table field])
                                                    ;:on-mouse-down #(when (= (.-button %) 1) (ut/tracked-dispatch [::conn/declick-parameter [table field]]))
                                                       :on-mouse-leave #(reset! param-hover nil)}
                                              ;:padding "3px"
                                                :style {:border           (str "1px solid " dcolor)
                                                        :background-color (if hovered? (str dcolor 55)
                                                                              (if selected? (str dcolor 20) "inherit"))}
                                                :children
                                                [[re-com/h-box
                                                ;:padding "3px"
                                                  :justify :between
                                                  :children
                                                  [[re-com/box
                                                    :child (if hovered?
                                                             (let [chars (count (str k))
                                                                   len 40
                                                                   wide? (> chars len)]
                                                               (if wide? (str (subs (str k) 0 len) "...") (str k)))
                                                             (str k))
                                                    :style {:color        dcolor :font-weight 700 :font-size "13px"
                                                            :padding-left "4px" :padding-right "4px"}]
                                                   (if hovered?
                                                     [re-com/md-icon-button
                                                      :md-icon-name "fa-regular fa-trash-can" ;; "zmdi-close" ;; <i class="fa-regular fa-trash-can"></i>
                                                      :style {:color dcolor :padding "0px" :margin-top "-2px" :margin-right "3px" :font-size "14px" :height "15px"}
                                                      :on-click #(ut/tracked-dispatch [::conn/declick-parameter [table field]])]
                                                     [re-com/box
                                                      :child (str dtype) :style {:color       dcolor :font-size "10px"
                                                                                 :padding-top "3px" :padding-left "4px" :padding-right "4px"}])]]


                                                 [re-com/box
                                                  :child (if (scrub/hex-color? v)
                                                           [re-com/h-box
                                                            :gap "7px"
                                                            :children [(str v)
                                                                       [re-com/box
                                                                        :src (at)
                                                                        :child " " :size "auto" :width "15px" :height "15px"
                                                                        :style {:background-color (str v)
                                                                                :margin-top       "2px"
                                                                                :padding-left     "3px"
                                                                                :padding-right    "3px"}]]]
                                                           (if (nil? v) "NULL"
                                                               (cond (= dtype "map") "{ ... map hidden ... }"
                                                                     is-b64?         "**huge base64 string**"
                                                                  ;(= dtype "vector") "[ ... vector hidden ... ]"
                                                                  ;(= dtype "vector") [shape/map-boxes2 v :cells nil [] :cells "vector"]
                                                                     :else (str v))))
                                                  :style (merge {:color          (str (theme-pull :theme/editor-font-color nil) 99) ; "#ffffff99"
                                                                 :font-size      "13px"
                                                                 :font-weight    700
                                                                 :padding-left   "4px"
                                                                 :padding-right  "4px"
                                                                 :padding-bottom "2px"}
                                                                (if (nil? v) {:font-style "italic"
                                                                              :opacity    0.3} {}))
                                                  :align :end]]])))])))]]]]))) ;]


(defonce query-hover (reagent/atom nil))

(re-frame/reg-sub
 ::queries-in-this-tab
 (fn [db _]
   (try (vec (apply concat
                    (for [[_ v] (get db :panels)
                          :let [tab (get db :selected-tab)]
                          :when (=  tab (get v :tab))]
                      (for [[qk _] (get v :queries)]
                        qk))))
        (catch :default _ nil))))

(defn screen-query-browser [width-int height-int]
  (let [ph             @query-hover ;; reaction hack
        queries-in-this-tab @(ut/tracked-subscribe [::queries-in-this-tab])
        selected-block @(ut/tracked-subscribe [::selected-block])
        ;queries (vec (cset/intersection (set queries) (set queries-in-this-tab)))
        queries queries-in-this-tab] ;; TODO clean up, incoming queries is unneeded here, we got a local sub

    [re-com/box
     :size "none"
     ;:width (px (- width-int 24))
     :height (px (- height-int 70))
     :style {:overflow-y  "auto"
             :overflow-x  "hidden"
             :margin-left "-22px"}
     :child [re-com/v-box
             :style {:background-color (theme-pull :theme/editor-param-background-color nil) ;; "#0b1122"
                     :margin-top       "4px"
                     :padding-right    "2px"}
             ;:gap "10px"
             :children (for [k (filter #(not (cstr/starts-with? (str %) ":query-preview")) queries)]
                         ; (filter #(and (not (cstr/starts-with? (str (first %)) ":conn-list/"))
                         ;          (not (cstr/starts-with? (str (first %)) ":tables-sys/"))
                         ;          (not (cstr/starts-with? (str (first %)) ":connections-sys/"))
                         ;          (not (cstr/starts-with? (str (first %)) ":fields-sys/")))
                         ;    grp)]
                         (let [;meta @(ut/tracked-subscribe [::meta-from-query-name k])
                               meta                @(ut/tracked-subscribe [::conn/sql-merged-metadata [k]])
                               rows                (nf (get meta :rowcount))
                               fields              (count (get meta :fields))
                               query-panel         @(ut/tracked-subscribe [::panel-from-query-name k])
                              ;;  subq-blocks         @(ut/tracked-subscribe [::subq-panels selected-block])
                               subq-blocks         @(ut/tracked-sub ::subq-panels-alpha {:panel-id selected-block})
                               subq-mapping        @(ut/tracked-sub ::subq-mapping-alpha {})
                               ;;; subq-mapping        @(ut/tracked-subscribe [::subq-mapping])
                               parent-of-selected? (some #(= % query-panel) subq-blocks)
                               upstream?           (some #(= % query-panel) (ut/cached-upstream-search subq-mapping selected-block))
                               downstream?         (some #(= % query-panel) (ut/cached-downstream-search subq-mapping selected-block))
                               selected?           @(ut/tracked-subscribe [::has-query? selected-block k])]
                           ;(ut/tapp>> @param-hover)
                           [re-com/v-box
                            :attr {:on-mouse-enter #(reset! query-hover k)
                                   :on-mouse-leave #(reset! query-hover nil)
                                   :on-click       #(ut/tracked-dispatch [::select-block query-panel])}
                            :style {:border           (cond
                                                        (= selected-block query-panel) "2px solid #9973e0"
                                                        parent-of-selected? "2px solid #e6ed21"
                                                        upstream? "2px solid #7be073"
                                                        downstream? "2px dashed #05dfff"
                                                        :else (str "1px solid " (theme-pull :theme/editor-font-color nil) 33))
                                    :color            (if (or selected? (= k @query-hover))
                                                        (theme-pull :theme/grid-selected-font-color nil)
                                                        (str (theme-pull :theme/editor-font-color nil) 99))
                                    :cursor           "pointer"
                                    :background-color (if (= k @query-hover)
                                                        (str (theme-pull :theme/grid-selected-background-color nil) 55)
                                                        (if selected?
                                                          ;"#0b031b"
                                                          (theme-pull :theme/grid-selected-background-color nil)
                                                          "inherit"))}
                            :children
                            [[re-com/h-box
                              ;:padding "3px"
                              :justify :between
                              :children
                              [[re-com/box
                                :child (str k) :style {;:color        (str (theme-pull :theme/editor-font-color nil) 99) ;"#ffffff99"
                                                       :font-weight  700 :font-size "13px"
                                                       :padding-left "4px" :padding-right "4px"}]]]
                               ;[re-com/box
                               ; :child (str dtype) :style {:color dcolor :font-size "10px"
                               ;                            :padding-top "3px"  :padding-left "4px" :padding-right "4px"}]


                             [re-com/h-box
                              :justify :between
                              :children
                              [[re-com/box :child (str fields " fields")
                                :style {;:color "#ffffff33"
                                        ;:color       (str (theme-pull :theme/editor-font-color nil) 55)
                                        :font-size   "13px"
                                        :font-weight 500 :padding-left "4px" :padding-right "4px" :padding-bottom "2px"}
                                :align :end]

                               [re-com/box :child (str rows " rows")
                                :style {;:color "#ffffff33"
                                        ;:color       (str (theme-pull :theme/editor-font-color nil) 55)
                                        :font-size   "13px"
                                        :font-weight 500 :padding-left "4px" :padding-right "4px" :padding-bottom "2px"}
                                :align :end]]]]]))]]
                                ))

(re-frame/reg-sub
 ::panel-counts
 (fn [db [_ panel-key]]
   (let [views (count (keys (get-in db [:panels panel-key :views])))
         queries (count (keys (get-in db [:panels panel-key :queries])))]
     [views queries])))

(re-frame/reg-sub
 ::panel-name-only
 (fn [db {:keys [panel-key]}]
   (get-in db [:panels panel-key :name] (str panel-key))))


(defn screen-block-browser [width-int height-int]
  (let [ph             @query-hover ;; reaction hack
       ; queries-in-this-tab @(ut/tracked-subscribe [::queries-in-this-tab])
        selected-block @(ut/tracked-sub ::selected-block {})
        ;selected-block @(ut/tracked-subscribe [::selected-block])
        ;queries (vec (cset/intersection (set queries) (set queries-in-this-tab)))
       ; queries queries-in-this-tab ;; TODO clean up, incoming queries is unneeded here, we got a local sub
        blocks (map keyword @(ut/tracked-sub ::current-tab-blocks {}))]

    ;(ut/tapp>> ph)
    [re-com/box
     :size "none"
     ;:width (px (- width-int 24))
     :height (px (- height-int 70))
     :style {:overflow-y  "auto"
             :overflow-x  "hidden"
             :margin-left "-22px"}
     :child [re-com/v-box
             :style {:background-color (theme-pull :theme/editor-param-background-color nil) ;; "#0b1122"
                     :margin-top       "4px"
                     :padding-right    "2px"}
             ;:gap "10px"
             :children (for [k (filter #(not (cstr/includes? (str %) "query-preview")) blocks)]
                         ; (filter #(and (not (cstr/starts-with? (str (first %)) ":conn-list/"))
                         ;          (not (cstr/starts-with? (str (first %)) ":tables-sys/"))
                         ;          (not (cstr/starts-with? (str (first %)) ":connections-sys/"))
                         ;          (not (cstr/starts-with? (str (first %)) ":fields-sys/")))
                         ;    grp)]
                         (let [;meta @(ut/tracked-subscribe [::meta-from-query-name k])
                               ;meta                {} ;@(ut/tracked-subscribe [::conn/sql-merged-metadata [k]])
                               ;rows                "0" ;(nf (get meta :rowcount))
                               ;fields              0 ;(count (get meta :fields))
                               nname               @(ut/tracked-sub ::panel-name-only {:panel-key k})
                               [rows fields]       @(ut/tracked-subscribe [::panel-counts k])
                               query-panel         k ;@(ut/tracked-subscribe [::panel-from-query-name k])
                               ;subq-blocks         @(ut/tracked-subscribe [::subq-panels selected-block])
                               subq-blocks         @(ut/tracked-sub ::subq-panels-alpha {:panel-id selected-block})
                              ;;  subq-mapping        @(ut/tracked-subscribe [::subq-mapping])
                               subq-mapping        @(ut/tracked-sub ::subq-mapping-alpha {})
                               ;subq-blocks         @(ut/tracked-sub ::subq-panels-alpha {:panel-id selected-block})
                               ;subq-mapping        @(ut/tracked-sub ::subq-mapping {})
                               parent-of-selected? (some #(= % query-panel) subq-blocks)
                               upstream?           (some #(= % query-panel) (ut/cached-upstream-search subq-mapping selected-block))
                               downstream?         (some #(= % query-panel) (ut/cached-downstream-search subq-mapping selected-block))
                               selected?           @(ut/tracked-subscribe [::has-query? selected-block k])]
                           ;(ut/tapp>> @param-hover)
                           [re-com/v-box
                            :attr {:on-mouse-enter #(reset! query-hover k)
                                   :on-mouse-leave #(reset! query-hover nil)
                                   :on-click       #(ut/tracked-dispatch [::select-block query-panel])}
                            :style {:border           (cond
                                                        (= selected-block query-panel) "2px solid #9973e0"
                                                        parent-of-selected? "2px solid #e6ed21"
                                                        upstream? "2px solid #7be073"
                                                        downstream? "2px dashed #05dfff"
                                                        :else (str "1px solid " (theme-pull :theme/editor-font-color nil) 33))
                                    :color            (if (or selected? (= k @query-hover))
                                                        (theme-pull :theme/grid-selected-font-color nil)
                                                        (str (theme-pull :theme/editor-font-color nil) 99))
                                    :cursor           "pointer"
                                    :background-color (if (= k @query-hover)
                                                        (str (theme-pull :theme/grid-selected-background-color nil) 55)
                                                        (if selected?
                                                          (theme-pull :theme/grid-selected-background-color nil)
                                                          "inherit"))}
                            :children
                            [[re-com/h-box
                              :justify :between
                              :children
                              [[re-com/box
                                :child (str nname)
                                :size "auto"
                                :style {:font-weight  700 :font-size "13px"
                                        :padding-left "4px" :padding-right "4px"}]]]

                             [re-com/h-box
                              :justify :between
                              :children
                              [[re-com/box :child (str fields (if (= fields 1) " query" " queries"))
                                :style {:font-size   "13px"
                                        :font-weight 500 :padding-left "4px" :padding-right "4px" :padding-bottom "2px"}
                                :align :end]

                               [re-com/box :child (str rows (if (= rows 1) " view" " views"))
                                :style {:font-size   "13px"
                                        :font-weight 500 :padding-left "4px" :padding-right "4px" :padding-bottom "2px"}
                                :align :end]]]]]))]]))

(defn sql-spawner [type panel-map data-keypath target-id source-panel-key]
  (let [sql-name-base        (ut/replacer (str (ut/safe-name target-id) "-drag-" (rand-int 45)) #"_" "-")
        ;parent-sql (get-in panel-map [:queries (first data-keypath)])
        query-meta           @(ut/tracked-subscribe [::conn/sql-metadata data-keypath])
        field-data-type      (get-in query-meta [:fields target-id :data-type])
        parent-sql-alias     (keyword (str "query/" (ut/safe-name (first data-keypath))))
        ;parent-sql-sql-alias (keyword (ut/replacer (str "q_" (ut/safe-name (first data-keypath))) "*" ""))
        parent-sql-sql-alias (ut/gen-sql-sql-alias)
        ; old-sql-alias (keyword (str "subqa_" (rand-int 123456)))
        agg?                                                ;(and (= type :meta-fields)
        ;(or (= "integer" field-data-type)
        ;    (= "float" field-data-type));)

        (and (or (= field-data-type "integer") (= field-data-type "float"))
             (not (cstr/includes? (cstr/lower-case (str target-id)) "year"))
             (not (cstr/includes? (cstr/lower-case (str target-id)) "month"))
             (not (cstr/includes? (cstr/lower-case (str target-id)) "week")))

        sql-name             (keyword sql-name-base)
        connection-id        (get panel-map :connection-id)
        general-field        {:h             (if agg? 4 5)
                              :w             (if agg? 4 8)
                              :drag-meta     {;:query query-meta
                                              :type             type
                                              :target           target-id
                                              :data-type        field-data-type
                                              :connection-id    connection-id
                                              :source-table     parent-sql-alias
                                              :source-query     (first data-keypath)
                                              :source-panel-key source-panel-key}
                              :source-panel  source-panel-key
                              :connection-id connection-id
                              :name          (str "drag-from-" (get panel-map :name))
                              :queries       (if agg?
                                               {sql-name
                                                {:select
                                                 [[[:sum target-id]
                                                   (keyword (str (ut/safe-name target-id) "_sum"))]]
                                                  ;[[:avg target-id]
                                                  ; (keyword (str (ut/safe-name target-id) "_avg"))]

                                                 :from [[parent-sql-alias parent-sql-sql-alias]]}}

                                               {sql-name
                                                {:select   [target-id [[:count 1] :rowcnt]]
                                                 :from     [[parent-sql-alias parent-sql-sql-alias]]
                                                 :group-by [target-id]
                                                 :order-by [[:rowcnt :desc]]}})}]
    (condp = type
      :field general-field)))

(defn sql-spawner-where [type panel-map data-keypath field-name source-panel-key & [row-num]]
  (let [sql-name-base        (ut/replacer (str (ut/safe-name field-name) "-drag-" (rand-int 45)) #"_" "-")
        ;parent-sql (get-in panel-map [:queries (first data-keypath)])
        selected-field       @(ut/tracked-subscribe [::conn/clicked-parameter data-keypath])
        query-meta           @(ut/tracked-subscribe [::conn/sql-metadata data-keypath])
        field-data-type      (get-in query-meta [:fields field-name :data-type])
        parent-sql-alias     (keyword (str "query/" (ut/safe-name (first data-keypath))))
        ;parent-sql-sql-alias (keyword (ut/replacer (str "q_" (ut/safe-name (first data-keypath))) "*" ""))
        parent-sql-sql-alias (ut/gen-sql-sql-alias)
        selected-fields      (vec (keys (get query-meta :fields)))
        ;old-sql-alias (keyword (str "subqa_" (rand-int 123456)))
        ;agg? ;(and (= type :meta-fields)
        ;(or (= "integer" field-data-type)
        ;    (= "float" field-data-type));)
        flow-item? (= (first data-keypath) :flow-fn-sys)
        new-flow-map (when flow-item? (let [pval @(ut/tracked-subscribe [::conn/clicked-parameter [:flow-fn-sys]])
                                            modded (conn/spawn-open-input-block pval)
                                            body (get modded 2)]
                                        ;(ut/tapp>> [:new-flow-map-on-old modded])
                                        body))
        flow-item            (when flow-item? (try (merge
                                                                               (-> (edn/read-string
                                                                                    (get selected-field :full_map)))
                                                                                         ;(dissoc :view)
                                                                                         ;(dissoc :fn)

                                                                               selected-field)
                                                                              (catch :default _ {})))
       ; flow-item (if (= (get flow-item :type) :open-fn) (assoc flow-item :raw-fn (get flow-item :fn)) flow-item)
       ; _ (ut/tapp>> [:building-flow-item selected-field (-> (edn/read-string
       ;                                                  (get selected-field :full_map))
       ;                                                 (dissoc :view) (dissoc :fn)) flow-item])
        sql-name             (keyword sql-name-base)
        general-field        {:h             6
                              :w             5
                              :flow-item     (select-keys flow-item [:category :name :type :icon :defaults :types :style :selected-style :inputs :expandable? :required])
                              :drag-meta     (if flow-item
                                               {:type (try (edn/read-string (get flow-item :name)) (catch :default _ (get flow-item :name)))}
                                               {:type             :where
                                                :param-full       (get selected-field field-name) ;{field-name (get selected-field field-name)}
                                                :param-table      (first data-keypath) ; table
                                                :param-field      field-name
                                                :target           field-name
                                                :row-num          row-num
                                                :connection-id    (get panel-map :connection-id)
                                                :data-type        field-data-type
                                                :source-table     parent-sql-alias
                                                :source-query     (first data-keypath)
                                                :source-panel-key source-panel-key})
                              :source-panel  source-panel-key
                              :connection-id (get panel-map :connection-id)
                              :name          (str "drag-from-" (get panel-map :name))
                              :queries       {sql-name
                                              {:select selected-fields ;[:*]
                                               :from   [[parent-sql-alias parent-sql-sql-alias]]
                                               :where  [:= field-name (get selected-field field-name)]}}}
        general-field (if flow-item?
                        ;;(select-keys general-field [:h :w :flow-item :drag-meta   ])
                        (let [inputs (vec (keys (get-in new-flow-map [:ports :in])))
                              flow-item (assoc (get-in new-flow-map [:data :flow-item]) :inputs inputs)
                              drag-meta (get-in new-flow-map [:data :drag-meta])] ;; re-re-package for the canvas inserter TODO: ALL THIS NONSENSE
                          (-> new-flow-map (dissoc :data) (assoc :flow-item flow-item) (assoc :drag-meta drag-meta)))
                        general-field)]
                                               ;:group-by [field-name]
                                               ;:order-by [[:rowcnt :desc]]

    general-field))

(defn sql-spawner-chat [query-src query-tgt & [name h w]]
  (let [sql-name-base        (ut/replacer (str "chat-drag-" (rand-int 45)) #"_" "-")
        panel-id @(ut/tracked-subscribe [::panel-from-query-name query-src])
        panel-map @(ut/tracked-subscribe [::panel-map panel-id])
        ;parent-sql (get-in panel-map [:queries (first data-keypath)])
        ;selected-field       @(ut/tracked-subscribe [::conn/clicked-parameter data-keypath])
        ;query-meta           @(ut/tracked-subscribe [::conn/sql-metadata data-keypath])
        ;field-data-type      (get-in query-meta [:fields field-name :data-type])
        ;parent-sql-alias     (keyword (str "query/" (ut/safe-name (first data-keypath))))
        ;parent-sql-sql-alias (keyword (ut/replacer (str "q_" (ut/safe-name (first data-keypath))) "*" ""))
        ;parent-sql-sql-alias (ut/gen-sql-sql-alias)
        ;selected-fields      (vec (keys (get query-meta :fields)))
        ;old-sql-alias (keyword (str "subqa_" (rand-int 123456)))
        ;agg? ;(and (= type :meta-fields)
        ;(or (= "integer" field-data-type)
        ;    (= "float" field-data-type));)
        both-keys? (and (not (nil? (get query-tgt :views)))
                        (not (nil? (get query-tgt :queries))))
        sql-name             (keyword sql-name-base)
        general-field        {:h             (or h 6)
                              :w             (or w 5)
                              :drag-meta     {:type             :chat
                                              ;:param-full       (get selected-field field-name) ;{field-name (get selected-field field-name)}
                                              ;:param-table      (first data-keypath) ; table
                                              ;:param-field      field-name
                                              ;:target           field-name
                                              ;:row-num          row-num
                                              :connection-id    (get panel-map :connection-id)}
                                              ;:data-type        field-data-type
                                              ;:source-table     parent-sql-alias
                                              ;:source-query     (first data-keypath)
                                              ;:source-panel-key source-panel-key

                              ;:source-panel  source-panel-key
                              :connection-id (get panel-map :connection-id)
                              :name          (or name (str "chat-sql-" (get panel-map :name)))
                              :queries       (if both-keys? (get query-tgt :queries) {sql-name query-tgt})
                              :views         (when both-keys? (get query-tgt :views))}]
    general-field))

(defn view-spawner-chat [kp query-tgt]
  (let [;sql-name-base        (ut/replacer (str "chat-drag-" (rand-int 45)) #"_" "-")
        panel-id (first kp) ;@(ut/tracked-subscribe [::panel-from-query-name query-src])
        panel-map @(ut/tracked-subscribe [::panel-map panel-id])
        view-name (last kp)
        view-name (if (nil? view-name) (ut/safe-key (keyword "new")) view-name)
        ;parent-sql (get-in panel-map [:queries (first data-keypath)])
        ;selected-field       @(ut/tracked-subscribe [::conn/clicked-parameter data-keypath])
        ;query-meta           @(ut/tracked-subscribe [::conn/sql-metadata data-keypath])
        ;field-data-type      (get-in query-meta [:fields field-name :data-type])
        ;parent-sql-alias     (keyword (str "query/" (ut/safe-name (first data-keypath))))
        ;parent-sql-sql-alias (keyword (ut/replacer (str "q_" (ut/safe-name (first data-keypath))) "*" ""))
        ;parent-sql-sql-alias (ut/gen-sql-sql-alias)
        ;selected-fields      (vec (keys (get query-meta :fields)))
        ;old-sql-alias (keyword (str "subqa_" (rand-int 123456)))
        ;agg? ;(and (= type :meta-fields)
        ;(or (= "integer" field-data-type)
        ;    (= "float" field-data-type));)
        ;sql-name             (keyword sql-name-base)
        general-field        {:h             4
                              :w            6
                              :drag-meta     {:type             :chat}
                                              ;:param-full       (get selected-field field-name) ;{field-name (get selected-field field-name)}
                                              ;:param-table      (first data-keypath) ; table
                                              ;:param-field      field-name
                                              ;:target           field-name
                                              ;:row-num          row-num
                                              ;:connection-id    (get panel-map :connection-id)
                                              ;:data-type        field-data-type
                                              ;:source-table     parent-sql-alias
                                              ;:source-query     (first data-keypath)
                                              ;:source-panel-key source-panel-key

                              ;:source-panel  source-panel-key
                              ;:connection-id (get panel-map :connection-id)
                              :name          (str "chat-view-" (get panel-map :name))
                              ;;:queries       {sql-name query-tgt}
                              :views         {view-name query-tgt}}]
    general-field))

;; (defn flow-fn-spawner []
;;   (let [;; same as viz-reco but with dynamic combo id via panel lookup
;;       req-name-str     (str "block-" (rand-int 999))
;;       ddata            @(ut/tracked-subscribe [::panel-map type])
;;       poss-value-walks (filter #(or (cstr/starts-with? (str %) ":query-preview")
;;                                     (cstr/starts-with? (str %) ":panel-key"))
;;                                (ut/deep-flatten ddata))
;;       key-walk1        (filter #(not (cstr/includes? (str %) "/")) poss-value-walks)
;;       key-walk         (merge (into {} (for [k key-walk1] {k (keyword (str "gen-viz-" (rand-int 1234)))}))
;;                               {:panel-key (keyword req-name-str)})
;;                   ;value-walk1 (filter #(cstr/includes? (str %) "/") poss-value-walks)
;;       value-walk1      (filter #(cstr/includes? (str %) "/") poss-value-walks)
;;       value-walk       (into {} (for [v value-walk1] (let [vv   (ut/safe-name v)
;;                                                            vs   (ut/splitter vv "/")
;;                                                            qstr (ut/safe-name (first (ut/postwalk-replacer key-walk [(keyword (first vs))])))
;;                                                            vstr (last vs)]
;;                                                        (ut/tapp>> [:value-walk vv vs qstr vstr v (keyword (str qstr "/" vstr))])
;;                                                        {v (keyword (str qstr "/" vstr))})))]
;;               ;(ut/tapp>> [:poss-value-walks poss-value-walks :key-walk key-walk :value-walk value-walk])
;;   (merge (ut/postwalk-replacer (merge key-walk value-walk)
;;                                 ddata)
;;          {;:mad-libs-viz []
;;           :drag-meta {:type       :viz-reco     ;type
;;                       :block-name (keyword req-name-str)}})))

(defn sql-spawner-meta [type]
  (let [selected-meta-db      @(ut/tracked-subscribe [::conn/clicked-parameter [:connections-sys]]) ;;
        selected-meta-table   @(ut/tracked-subscribe [::conn/clicked-parameter [:tables-sys]])
        selected-meta-field   @(ut/tracked-subscribe [::conn/clicked-parameter [:fields-sys]])
        selected-fields       (vec (map ut/sql-keyword (map :field_name
                                                            ;@(ut/tracked-subscribe [::conn/sql-data [:fields-sys]])
                                                            @(rfa/sub ::conn/sql-data-alpha {:keypath [:fields-sys]})
                                                            )))
        parent-sql-sql-alias  (ut/gen-sql-sql-alias)
        field-name            (get selected-meta-field :field_name)
        table-name            (get selected-meta-table :table_name)
        rando                 (rand-int 45)
        sql-name-base         (cond (= type :meta-fields)
                                    (ut/replacer
                                     (str (ut/safe-name table-name) "-drag-" (ut/safe-name field-name) "-" rando)
                                     #"_" "-")
                                    :else (ut/replacer
                                           (str (ut/safe-name table-name) "-drag-" rando)
                                           #"_" "-"))
        sql-name              (keyword sql-name-base)
        connection-id         (get selected-meta-table :connection_id)
        field-data-type       (get selected-meta-field :data_type)
        drag-meta             {:target           (keyword field-name)
                               :source-table     (keyword table-name)
                               :source-query     (keyword table-name)
                               :table-fields     (if (= type :meta-tables) selected-fields [(keyword field-name)])
                               :connection-id    connection-id
                               :source-panel-key "none!"
                               :data-type        field-data-type
                               :type             type}
        db-type               (cstr/lower-case (str (get selected-meta-db :database_name)))
        qual-table-name       (cond (cstr/includes? db-type "vertica") (str (get selected-meta-table :db_schema) "." table-name)
                                    :else table-name)
        agg?                  (and (= type :meta-fields)
                                   ;(or (= "integer" field-data-type)
                                   ;    (= "float" field-data-type))

                                   (or (= field-data-type "integer") (= field-data-type "float"))
                                   (not (cstr/includes? (cstr/lower-case (str field-name)) "year"))
                                   (not (cstr/includes? (cstr/lower-case (str field-name)) "month"))
                                   (not (cstr/includes? (cstr/lower-case (str field-name)) "week")))

        dyn-width             (let [x (js/Math.ceil (* (count selected-fields) 2.2))]
                                (if (>= x 30) 30 x))
        tables-panel          {:h             7
                               :w             dyn-width
                               :drag-meta     drag-meta
                               :connection-id connection-id
                               :name          (str "select-all-" table-name)
                               :queries       {sql-name
                                               {:select selected-fields ;[:*] ;;explicit field names for subsequent removal
                                                :from   [[(keyword qual-table-name) parent-sql-sql-alias]]}}}
        meta-fields-panel     {:h             5
                               :w             5
                               :drag-meta     drag-meta
                               :connection-id connection-id
                               :name          (str "select-" field-name "-" table-name)
                               :queries       {sql-name
                                               {:select   [(keyword field-name) [[:count 1] :rowcnt]]
                                                :from     [[(keyword qual-table-name) parent-sql-sql-alias]]
                                                :group-by [(keyword field-name)]
                                                :order-by [[:rowcnt :desc]]}}}
        meta-screen-load      {:file-path (get @(ut/tracked-subscribe [::conn/clicked-parameter [:files-sys]]) :file_path)
                               :drag-meta {:type type}}

        meta-block-load       (merge
                               (read-string 
                                ;;@(ut/tracked-subscribe [::conn/clicked-parameter-key [:blocks-sys/block_data]])
                                @(ut/tracked-sub ::conn/clicked-parameter-key-alpha {:keypath [:blocks-sys/block_data]})
                                )
                               (let [bk (get @(ut/tracked-subscribe [::conn/clicked-parameter [:blocks-sys]]) :block_key)
                                     bn (get @(ut/tracked-subscribe [::conn/clicked-parameter [:blocks-sys]]) :block_name)]
                                 {:file-path (get @(ut/tracked-subscribe [::conn/clicked-parameter [:files-sys]]) :file_path)
                                  :block-key bk
                                  :drag-meta {:type (cond (= bk ":*theme*") :meta-theme
                                                          (cstr/starts-with? (str bn) "board: ") :meta-board
                                                          :else type)}}))

        meta-fields-agg-panel {:h             5
                               :w             5
                               :drag-meta     drag-meta
                               :connection-id connection-id
                               :name          (str "select-" field-name "-" table-name)
                               :queries       {sql-name
                                               {:select [[[:sum (keyword field-name)]
                                                          (keyword (str field-name "_sum"))]]
                                                         ;[[:avg (keyword field-name)]
                                                         ; (keyword (str field-name "_avg"))]

                                                :from   [(keyword qual-table-name)]}}}]
    ;(ut/tapp>> [:builder-fields selected-fields])
    (cond
      (= type :viz-reco) (let [;; we need to change all the generic aliases into real rando keys
                               ;; inlcuding potential value aliases :query-key/field.row combo keys
                               req-name-str     (str "block-" (rand-int 999))
                               ddata            @(ut/tracked-subscribe [::panel-map :reco-preview])
                               poss-value-walks (filter #(or (cstr/starts-with? (str %) ":query-preview")
                                                             (cstr/starts-with? (str %) ":panel-key"))
                                                        (ut/deep-flatten ddata))
                               key-walk1        (filter #(not (cstr/includes? (str %) "/")) poss-value-walks)
                               key-walk         (merge (into {} (for [k key-walk1] {k (keyword (str "gen-viz-" (rand-int 1234)))}))
                                                       {:panel-key (keyword req-name-str)})
                               ;value-walk1 (filter #(cstr/includes? (str %) "/") poss-value-walks)
                               value-walk1      (filter #(cstr/includes? (str %) "/") poss-value-walks)
                               value-walk       (into {} (for [v value-walk1] (let [vv   (ut/safe-name v)
                                                                                    vs   (ut/splitter vv "/")
                                                                                    qstr (ut/safe-name (first (ut/postwalk-replacer key-walk [(keyword (first vs))])))
                                                                                    vstr (last vs)]
                                                                                (ut/tapp>> [:value-walk vv vs qstr vstr v (keyword (str qstr "/" vstr))])
                                                                                {v (keyword (str qstr "/" vstr))})))]
                           ;(ut/tapp>> [:poss-value-walks poss-value-walks :key-walk key-walk :value-walk value-walk])
                           (merge (ut/postwalk-replacer (merge key-walk value-walk)
                                                         ddata)
                                  {;:mad-libs-viz []
                                   :drag-meta {:type       type
                                               :block-name (keyword req-name-str)}}))
      (= type :meta-tables) tables-panel
      (= type :meta-fields) (if agg?
                              meta-fields-agg-panel
                              meta-fields-panel)
      (= type :meta-screens) meta-screen-load
      (= type :meta-blocks) meta-block-load
      :else (let [;; same as viz-reco but with dynamic combo id via panel lookup
                  req-name-str     (str "block-" (rand-int 999))
                  ddata            @(ut/tracked-subscribe [::panel-map type])
                  poss-value-walks (filter #(or (cstr/starts-with? (str %) ":query-preview")
                                                (cstr/starts-with? (str %) ":panel-key"))
                                           (ut/deep-flatten ddata))
                  key-walk1        (filter #(not (cstr/includes? (str %) "/")) poss-value-walks)
                  key-walk         (merge (into {} (for [k key-walk1] {k (keyword (str "gen-viz-" (rand-int 1234)))}))
                                          {:panel-key (keyword req-name-str)})
                  ;value-walk1 (filter #(cstr/includes? (str %) "/") poss-value-walks)
                  value-walk1      (filter #(cstr/includes? (str %) "/") poss-value-walks)
                  value-walk       (into {} (for [v value-walk1] (let [vv   (ut/safe-name v)
                                                                       vs   (ut/splitter vv "/")
                                                                       qstr (ut/safe-name (first (ut/postwalk-replacer key-walk [(keyword (first vs))])))
                                                                       vstr (last vs)]
                                                                   (ut/tapp>> [:value-walk vv vs qstr vstr v (keyword (str qstr "/" vstr))])
                                                                   {v (keyword (str qstr "/" vstr))})))]
              ;(ut/tapp>> [:poss-value-walks poss-value-walks :key-walk key-walk :value-walk value-walk])
              (merge (ut/postwalk-replacer (merge key-walk value-walk)
                                            ddata)
                     {;:mad-libs-viz []
                      :drag-meta {:type       :viz-reco     ;type
                                  :block-name (keyword req-name-str)}})))))


(defn item-subscription-spawner-meta []
  (let [main-row-selected      @(ut/tracked-subscribe [::conn/clicked-parameter [:searches-rows-sys]]) ;;
        ;subtype-row-selected   @(ut/tracked-subscribe [::conn/clicked-parameter [:searches-sub-type-sys]])
        ;type-row-selected      @(ut/tracked-subscribe [::conn/clicked-parameter [:searches-type-sys]])
        ;selected-fields        (vec (map ut/sql-keyword (map :field_name @(ut/tracked-subscribe [::conn/sql-data [:fields-sys]]))))
        ttype                  (get main-row-selected :item_type)
        shortcode              (try (edn/read-string (get main-row-selected :value)) (catch :default _ "shortcode-error!"))
        rando                (rand-int 45)
        {:keys [item_type item_sub_type
                item_key display_name
                sample is_live block_meta]} main-row-selected
        panel-map             (try (edn/read-string block_meta) (catch :default _ {}))
        natural-key           (ut/safe-key (get panel-map :natural-key (keyword (str "sub-" rando))))


        ;;panel-map            {} ;; @(ut/tracked-subscribe [::workspace [source-panel-id]])
        selected-fields      (vec (keys (get @(ut/tracked-subscribe [::conn/sql-metadata [name]]) :fields)))
        selected-fields      (if (empty? selected-fields) [:*] selected-fields)
        table-name           name
        view-data            (if (= name :view) (get panel-map :views) (get-in panel-map [:views name]))

        parent-sql-sql-alias (ut/gen-sql-sql-alias)
        sql-name-base        (cond
                               :else (ut/replacer
                                      (str (ut/safe-name table-name) "-clone-" rando)
                                      #"_" "-"))
        sql-name             (keyword sql-name-base)
        connection-id        (get panel-map :connection-id)

        drag-meta            {;:source-table     table-name
                              ;:table-fields     selected-fields
                              ;:connection-id    connection-id
                              ;:source-panel-key source-panel-id
                              :type             ttype}
        new-panel            (cond
                               (cstr/ends-with? (str ttype) "-views")
                               {:h         (get panel-map :h 5)
                                :w         (get panel-map :w 7)
                                :drag-meta drag-meta
                                :views     {:vw1 [:box
                                                  :size "auto"
                                                  :child shortcode]}
                                :name      (str "subscription " item_key display_name rando)}

                               (cstr/ends-with? (str ttype) "-queries")
                               {:h         (get panel-map :h 5)
                                :w         (get panel-map :w 7)
                                :drag-meta drag-meta
                                :connection-id (get panel-map :connection-id)
                                :queries     {natural-key {:select [:*]
                                                           :from [shortcode]}}
                                :name      (str "subscription " item_key display_name rando)}

                               :else {:h         (get panel-map :h 5)
                                      :w         (get panel-map :w 7)
                                      :drag-meta drag-meta
                                      :views     {natural-key [:box
                                                               :size "auto"
                                                               :child [:string shortcode]]}
                                      :name      (str "subscription " item_key display_name rando)})]
    ;(ut/tapp>> [:hmmm panel-map])
    new-panel))


(defn sql-spawner-cloner [type source-panel-id name]
  (let [panel-map            @(ut/tracked-sub ::workspace-alpha {:keypath [source-panel-id]})
        selected-fields      (vec (keys (get @(ut/tracked-sub ::conn/sql-metadata-alpha {:keypath [name]}) :fields)))
        selected-fields      (if (empty? selected-fields) [:*] selected-fields)
        table-name           name
        view-data            (if (= name :view) (get panel-map :views) (get-in panel-map [:views name]))
        rando                (rand-int 45)
        parent-sql-sql-alias (ut/gen-sql-sql-alias)
        sql-name-base        (cond                          ;(= type :meta-fields)
                               :else (ut/replacer
                                      (str (ut/safe-name table-name) "-clone-" rando)
                                      #"_" "-"))
        sql-name             (keyword sql-name-base)
        connection-id        (get panel-map :connection-id)
        drag-meta            {:source-table     table-name
                              :table-fields     selected-fields
                              :connection-id    connection-id
                              :source-panel-key source-panel-id
                              :type             type}
        new-panel            (condp = type
                               :query {:h             (get panel-map :h) ;5
                                       :w             (get panel-map :w) ;9
                                       :drag-meta     drag-meta
                                       :connection-id connection-id
                                       :name          (str "clone-" (ut/safe-name table-name) rando)
                                       :queries       {sql-name
                                                       {:select selected-fields ;[:*] ;;explicit field names for subsequent removal
                                                        :from   [[(keyword (str "query/" (ut/safe-name table-name)))
                                                                  ;(keyword (str "clone-" (ut/safe-name table-name) rando))
                                                                  parent-sql-sql-alias]]}}}
                               :view {:h         (get panel-map :h)
                                      :w         (get panel-map :w)
                                      :drag-meta drag-meta
                                      ;:connection-id connection-id
                                      :views     {:view-clone view-data} ;[:box :child [:string table-name]]
                                      :name      (str "clone-" (ut/safe-name table-name) rando)})]
                                      ;:queries {sql-name
                                      ;          {:select selected-fields ;[:*] ;;explicit field names for subsequent removal
                                      ;           :from [[(keyword (str "query/" (ut/safe-name table-name)))
                                      ;                   (keyword (str "clone-" (ut/safe-name table-name) rando))]]}}

    ;(ut/tapp>> [:hmmm panel-map])
    new-panel))

;; (re-frame/reg-sub
;;  ::subq-panels
;;  (fn [db [_ panel-id]]                                     ;; filter has got to be faster than for loops. TODO for other subs
;;    (let [queries-used    (filter #(cstr/includes? (str %) ":query/")
;;                                  (ut/deep-flatten (get-in db [:panels panel-id :queries])))
;;          root-query-keys (for [k queries-used] (keyword (last (ut/splitter (ut/safe-name k) "/"))))
;;          query-sources   (into {} (remove empty? (flatten (for [[k v] (get db :panels)]
;;                                                             (flatten (for [kk (keys (get v :queries))]
;;                                                                        {kk k}))))))]
;;       ;(ut/tapp>> [:root-query-keys root-query-keys :query-sources query-sources])
;;      (vec (remove nil? (for [q root-query-keys] (get query-sources q)))))))

(re-frame/reg-sub
 ::subq-used
 (fn [db [_ panel-id]]                                     ;; filter has got to be faster than for loops. TODO for other subs im using FOR for
   (let [queries-used    (filter #(cstr/includes? (str %) ":query/")
                                 (ut/deep-flatten (get-in db [:panels panel-id :queries])))
         root-query-keys (for [k queries-used] (keyword (last (ut/splitter (ut/safe-name k) "/"))))]
     root-query-keys)))

(re-frame/reg-sub
 ::subq-produced
 (fn [db [_ panel-id]]                                     ;; filter has got to be faster than for loops. TODO for other subs im using FOR for
   (keys (get-in db [:panels panel-id :queries]))))

(re-frame/reg-sub
 ::subq-mapping-orig
 (fn [db [_]]                                              ;; filter has got to be faster than for loops. TODO for other subs im using FOR for
   (into {} (for [[k v] (get db :panels)]
              {k (merge {:uses (apply concat (for [[k1 v1] (get v :queries)]
                                               (for [qq (filter #(cstr/includes? (str %) ":query/")
                                                                (ut/deep-flatten v1))]
                                                 (keyword (last (ut/splitter (ut/safe-name qq) "/"))))))}
                        {:produces (keys (get-in db [:panels k :queries]))})}))))

(re-frame/reg-sub
 ::subq-mapping
 (fn [db [_]]
   (let [panels            (get db :panels)
         all-sql-call-keys (keys (into {} (for [[_ v] panels]
                                            (into {} (for [[kk vv] (get v :queries)]
                                                       (when (nil? (find vv :vselect)) {kk vv}))))))]
      ;(ut/tapp>> [:ttt all-sql-call-keys])
     (into {} (for [[k v] panels]
                {k (merge {:uses (apply concat (for [[_ v1] (merge (get v :queries)
                                                                    (get v :views))]
                                                 (for [qq (filter #(or (cstr/includes? (str %) ":query/")
                                                                       (some #{%} all-sql-call-keys))
                                                                  (flatten (conj (ut/deep-flatten v1)
                                                                                 (apply (fn [x] ;; get partial matches from param use (filters, etc)...
                                                                                          (keyword (first (ut/splitter (str (ut/safe-name x)) #"/"))))
                                                                                        (filter #(cstr/includes? (str %) "/")
                                                                                                (ut/deep-flatten v1))))))]
                                                   (keyword (last (ut/splitter (ut/safe-name qq) "/"))))))}
                          {:produces (keys (get-in db [:panels k :queries]))})})))))



(re-frame/reg-sub
 ::subq-mapping-alpha
 (fn [db {}]
   (let [px (hash (ut/remove-underscored (get db :panels)))
         cc (get @ut/subq-mapping-alpha px)]
     (if cc cc
         (let [panels            (get db :panels)
               all-sql-call-keys (keys (into {} (for [[_ v] panels]
                                                  (into {} (for [[kk vv] (get v :queries)]
                                                             (when (nil? (find vv :vselect)) {kk vv}))))))
               res (into {} (for [[k v] panels]
                              {k (merge {:uses (apply concat (for [[_ v1] (merge (get v :queries)
                                                                                 (get v :views))]
                                                               (for [qq (filter #(or (cstr/includes? (str %) ":query/")
                                                                                     (some #{%} all-sql-call-keys))
                                                                                (flatten (conj (ut/deep-flatten v1)
                                                                                               (apply (fn [x] ;; get partial matches from param use (filters, etc)...
                                                                                                        (keyword (first (ut/splitter (str (ut/safe-name x)) #"/"))))
                                                                                                      (filter #(cstr/includes? (str %) "/")
                                                                                                              (ut/deep-flatten v1))))))]
                                                                 (keyword (last (ut/splitter (ut/safe-name qq) "/"))))))}
                                        {:produces (keys (get-in db [:panels k :queries]))})}))]
           (swap! ut/subq-mapping-alpha assoc px res)
           res
           )))))

(re-frame/reg-sub
 ::subq-panels
 (fn [db [_ panel-id]]                                     ;; filter has got to be faster than for loops. TODO for other subs im using FOR for
   (let [panels            (into {} (filter #(= (get db :selected-tab) (get (val %) :tab "")) (get db :panels))) ;(get db :panels)
         all-sql-call-keys (keys (into {} (for [[_ v] panels]
                                            (into {} (for [[kk vv] (get v :queries)]
                                                       (when (nil? (find vv :vselect)) {kk vv}))))))
         queries-used      (filter #(or (cstr/includes? (str %) ":query/")
                                        (some #{%} all-sql-call-keys))
                                   (ut/deep-flatten (merge (get-in db [:panels panel-id :queries])
                                                           (get-in db [:panels panel-id :views]))))
         root-query-keys   (for [k queries-used] (keyword (last (ut/splitter (ut/safe-name k) "/"))))
         query-sources     (into {} (remove empty? (flatten (for [[k v] panels]
                                                              (flatten (for [kk (keys (get v :queries))]
                                                                         {kk k}))))))]
      ;(ut/tapp>> [:root-query-keys root-query-keys :query-sources query-sources])
     (vec (remove #(or (nil? %) (= (get db :selected-block) %)) (for [q root-query-keys] (get query-sources q)))))))


(re-frame/reg-sub
 ::subq-panels-alpha
 (fn [db {:keys [panel-id]}]                                     ;; filter has got to be faster than for loops. TODO for other subs im using FOR for
   (let [px (hash [panel-id (get db :selected-tab) (ut/remove-underscored (get db :panels))])
         cc (get @ut/subq-panels-alpha px)]

     (if cc cc
         (let [panels            (into {} (filter #(= (get db :selected-tab) (get (val %) :tab "")) (get db :panels))) ;(get db :panels)
               all-sql-call-keys (keys (into {} (for [[_ v] panels]
                                                  (into {} (for [[kk vv] (get v :queries)]
                                                             (when (nil? (find vv :vselect)) {kk vv}))))))
               queries-used      (filter #(or (cstr/includes? (str %) ":query/")
                                              (some #{%} all-sql-call-keys))
                                         (ut/deep-flatten (merge (get-in db [:panels panel-id :queries])
                                                                 (get-in db [:panels panel-id :views]))))
               root-query-keys   (for [k queries-used] (keyword (last (ut/splitter (ut/safe-name k) "/"))))
               query-sources     (into {} (remove empty? (flatten (for [[k v] panels]
                                                                    (flatten (for [kk (keys (get v :queries))]
                                                                               {kk k}))))))
               res (vec (remove #(or (nil? %) (= (get db :selected-block) %)) (for [q root-query-keys] (get query-sources q))))]
      ;(ut/tapp>> [:root-query-keys root-query-keys :query-sources query-sources])
           (swap! ut/subq-panels-alpha assoc px res)
           res)))
   ))


(re-frame/reg-sub
 ::subq-panels-all
 (fn [db [_ panel-id]]                                     ;; filter has got to be faster than for loops. TODO for other subs im using FOR for
   (let [queries-used    (filter #(cstr/includes? (str %) ":query/")
                                 (ut/deep-flatten (into {} (for [[k v] (get db :panels)] (get v :queries)))))
         root-query-keys (for [k queries-used] (keyword (last (ut/splitter (ut/safe-name k) "/"))))
         query-sources   (into {} (remove empty? (flatten (for [[k v] (get db :panels)]
                                                            (flatten (for [kk (keys (get v :queries))]
                                                                       {kk k}))))))])))

(re-frame/reg-sub
 ::subq-panels2
 (fn [db [_]]
   (let [all-qs (into {}
                      (for [[pk pv] (get db :panels)]
                        (let [queries-used    (filter #(cstr/includes? (str %) ":query/")
                                                      (ut/deep-flatten (get pv :queries)))
                              root-query-keys (for [k queries-used] (keyword (last (ut/splitter (ut/safe-name k) "/"))))]
                          {pk root-query-keys})))]
     all-qs)))

(defn scrubber-panel [view? keypath-map view-key & [wildcard full-map]]
  ;(ut/tapp>> [:scrubber-panel :keypath-map view-key keypath-map])
  [rc/catch
   (let [type-key            (if view? :views :queries)
         canvas?             (get full-map :canvas? false)
         flow?               (get full-map :flow? false)
         opts?               (get full-map :opts? false)
         runstreams?         (get full-map :runstreams? false)
         panel-key           @(ut/tracked-subscribe [::selected-block])
         block-map           @(ut/tracked-subscribe [::selected-block-map])
         wild?               (true? (and (not (= (cstr/trim (str wildcard)) "")) (not (nil? wildcard))))
        ; _ (ut/tapp>> [:keypath=map keypath-map])
         scrubber-boxes      (into (sorted-map) (for [[kp [v [scrub-type [ffn ffnx]]]] keypath-map]
                                                      ;:let [_ (ut/tapp>> [scrub-type kp])]

                                                  {kp (when (and (and (not (nil? scrub-type))
                                                                      (not (nil? v))
                                                                      (not (and (keyword? v)
                                                                                (try (integer? (first kp)) (catch :default _ false))
                                                                                (= 1 (count kp))))
                                                                     ;; single vector re-com elements most likely... TODO

                                                                     ;; VV 9/24/23 - to filter out queries except with styling rules...
                                                                      (if view? true
                                                                          (some #(or (= % :page) (= % :style-rules)) kp))

                                                                      (if (or (try (= (first view-key) :flow) (catch :default _ false))
                                                                              (try (= (first view-key) :runstreams) (catch :default _ false)))
                                                                        true
                                                                        (not (some #(cstr/starts-with? (str %) ":*") kp))))

                                                                      ;; system param
                                                                     ;true


                                                                 (if wild? (or (cstr/includes? (str kp) (str wildcard))
                                                                               (cstr/includes? (str v) (str wildcard)))
                                                                     true))
                                                                ;(some #(= % :select) kp) ;; temp sql testing for shapes
                                                                ;(= kp [:vselect]) ;; temp sql testing for shapes
                                                                ;(= kp [:from]) ;; temp sql testing for shapes
                                                                ;(= kp [:where]) ;; temp sql testing for shapes
                                                                ;(= kp [:logic]) ;; temp sql testing for shapes
                                                                ;(= kp [:group-by]) ;; temp sql testing for shapes
 ;; might want these...
                                                        (let [;; _ (ut/tapp>> [:here? kp v])
                                                              ft            (first (remove nil? (for [[k v] (scrub/friendly-text)]
                                                                                                  (when (scrub/ordered-keys-in-kp-multi? k kp) v))))
                                                              message       (get ft :message nil)
                                                              url           (get ft :url)
                                                             ;v (if (nil? v) "nil" v)
                                                             ;values (get ft :values [])
                                                             ;friendly-text (when (scrub/ordered-keys-in-kp k kp))
                                                              system-param? (if opts?
                                                                              (some #(= % (first kp)) [:timeout :close-on-done? :retries :retry-on-error? :debug?])
                                                                              (some #(= % (first kp)) (keys db/base-theme)))
                                                              display-kp    (str (-> (str kp)
                                                                                     (ut/replacer "[" "")
                                                                                     (ut/replacer "]" "")))
                                                              display-kp    (if system-param? (str display-kp " *sys*") display-kp)]
                                                          ;(ut/tapp>> [:ft kp ft])
                                                          [re-com/v-box
                                                           ;:width "100px"
                                                           :padding "4px"
                                                           :gap "3px"
                                                           :attr {:on-mouse-enter #(do ;(ut/tapp>> [:enter-scrub-area :pause-replication])
                                                                                     (reset! on-scrubber? true))
                                                                  :on-mouse-leave #(do ;(ut/tapp>> [:exit-scrub-area :resume-replication])
                                                                                     (reset! on-scrubber? false))}
                                                           :style (if canvas? {:margin-top "-12px"}
                                                                      {:border-top (if (or (= kp [:select]) (= kp [:from]) (= kp [:where]) (= kp [:group-by]))
                                                                                     "0px solid #ffffff18" "1px solid #ffffff18")})
                                                           :children [;[re-com/box :child (str kp " :: " v)]
                                                                      [re-com/h-box
                                                                       :justify :between
                                                                       :children
                                                                      ;[[read-only-clojure-box-un (str kp)]
                                                                      ; [read-only-clojure-box-un (if (string? v) (str "\"" v "\"") (str v))]]
                                                                       [(when (not flow?)
                                                                          [re-com/box
                                                                           :src (at)
                                                                           :child display-kp
                                                                           :style {:color       (if system-param? "#f386bf" "#ae81ff")
                                                                                   :font-weight 700}])

                                                                        (when (not flow?)
                                                                          [re-com/h-box
                                                                           :gap "5px"
                                                                        ;:justify :between
                                                                           :style {:font-size "11px" :padding-top "3px"}
                                                                           :children [(when message [re-com/box :child (str message)])
                                                                                      (when url [:a {:href   url
                                                                                                     :target "_blank"} "link"])]])

                                                                        [re-com/h-box
                                                                         :gap "5px"
                                                                        ;:justify :between
                                                                         :children [(when (scrub/hex-color? v)
                                                                                      [re-com/box
                                                                                       :src (at)
                                                                                       :child " " :size "none" :width "18px" :height "18px"
                                                                                       :attr {:on-click #(reset! scrub/color-picker-enabled?
                                                                                                                 (if (= kp @scrub/color-picker-enabled?) nil kp))}
                                                                                       :style {:background-color (str v)
                                                                                               :cursor           "pointer"
                                                                                               :border           "1px solid #ffffff15"}])
                                                                                    (when (and (not flow?)
                                                                                               (not (vector? v)))
                                                                                     ;; temp to hide large sql vectors
                                                                                      [re-com/box
                                                                                       :src (at)
                                                                                       :child (if (string? v) (str "\"" v "\"") (str v))
                                                                                       :style {:font-weight 700
                                                                                               :color       (cond (string? v) "#b9e47d"
                                                                                                                  (integer? v) "#f2c263"
                                                                                                                  (float? v) "#f2c263"
                                                                                                                  (keyword? v) "#ae81ff"
                                                                                                                  (boolean? v) "#ae81ff"
                                                                                                                  :else "#b9e47d")}])]]]]
                                                                      [rc/catch (if ffnx
                                                                                  [re-com/box
                                                                                  ; :width "100px"
                                                                                   :src (at)
                                                                                   :child [ffn kp v view-key type-key ffnx]]
                                                                                  [re-com/box
                                                                                   ;:width "100px"
                                                                                   :src (at)
                                                                                   :child [ffn kp v view-key type-key]])]]]))}))
         flat-scrubber-boxes (vals scrubber-boxes)
         queries             (get block-map :queries {})
         has-query?          (ut/ne? queries)
         final-children      (if (and has-query? (not view?))
                               (vec (conj flat-scrubber-boxes
                            ;[re-com/box :child (str (keys queries))]
                                          [re-com/v-box
                                           :children (for [[k v] queries]
                                                       [re-com/v-box
                                                        :children [;[re-com/box :child (str k)]
                                                                   [shape/map-boxes2 v panel-key (str k) [] panel-key "map"]]])]))
                               flat-scrubber-boxes)]
     ;;(ut/tapp>> [:SSscrubber  keypath-map block-map panel-key keypath-map (and has-query? (not view?)) flat-scrubber-boxes final-children scrubber-boxes])
     (cond
      ;;  runstreams? [re-com/v-box
      ;;  ;:align :center ;:justify :center
      ;;                   ;:size "auto"
      ;;                   :width "82%"
      ;;                   ;:height "100%"
      ;;                   ;:gap "3px"
      ;;  ;:padding "5px"
      ;;                   :children final-children]
       canvas? [re-com/v-box
       ;:align :center ;:justify :center
                :size "auto"
                :width "100%"
                :height "100%"
                :gap "3px"
       ;:padding "5px"
                :children final-children]
       :else [re-com/v-box
              :gap "3px"
              :padding "5px"
              :children final-children]))])

(def vega-lite-shell
  [:vega-lite
   {:layer      [{:encoding {;:href {:field "cdata" :type "nominal"}
                             :x      {:field nil
                                      :type  "ordinal"
                                      ;:title "x-axis-title"
                                      :sort  "-y"}
                             :y      {:aggregate "sum"
                                      :field     nil
                                      ;:title "y-axis-title"
                                      ;:sort "descending"
                                      :type      "quantitative"}
                             :row    {:field  nil
                                      :legend nil}
                             :size   {:legend nil}
                             :shape  {:legend nil}
                             :column {:field  nil
                                      :legend nil}
                             ; :color {:condition {:param "pts"
                             ;                     :scale {:scheme
                             ;                             "bluepurple"}
                             ;                     :legend nil
                             ;                     :field :rowcnt
                             ;                     :type "ordinal"}
                             ;         :value "grey"}
                             :color  {:scale  :theme/vega-default-color-scheme ; {:scheme "inferno"}
                                      :legend nil
                                      :field  1
                                      :type   "ordinal"}}
                  :mark     {:type "rect" :tooltip {:content "encoding"}}}]
    ;:transform [{:calculate "'#/set-param:fixed_year=' + datum.fixed_year" :as "cdata"}]
    :config     :theme/vega-defaults
    ;;  (merge {:view {:stroke "transparent"}
    ;;                  :axis {:labelColor "#ffffff88"
    ;;                         :tickColor "#ffffff22"
    ;;                         :grid true
    ;;                         :domain false
    ;;                         :titleColor "#ffffff99"
    ;;                         :domainColor "#ffffff22"
    ;;                         :gridColor "#ffffff22"
    ;;                         :labelFont "Lato", :titleFont "Lato"}
    ;;                  :legend {:labelFont "Lato" :titleFont "Lato"}
    ;;                  :header {:labelFont "Lato" :titleFont "Lato"}
    ;;                  :mark {:font "Lato"}
    ;;                  :title {:font "Lato" :subtitleFont "Lato"}})

    :width      :panel-width
    :background "transparent"
    ;:params [{:name "pts" :select "point" :value nil}]
    ;:font "Lato"
    :padding    4
    :height     :panel-height
    :data       {:values nil}}
   {:actions false}])

(re-frame/reg-sub
 ::resolve-sql-alias
 (fn [db {:keys [sql-key]}]
   (let [q (get
            (into {} (for [[_ v] (get db :panels)]
                       (into {} (get v :queries))))
            sql-key)]
    ;;  (-> q
    ;;      (dissoc :limit)
    ;;      (dissoc :page)
    ;;      (dissoc :col-widths)
    ;;      (dissoc :row-height)
    ;;      (dissoc :render-all?)
    ;;      (dissoc :cache?)
    ;;      (dissoc :refresh-every)
    ;;      (dissoc :deep-meta?)
    ;;      (dissoc :clicked-row-height)
    ;;      (dissoc :order-by))
     (ut/clean-sql-from-ui-keys q))))

(defn starts-with-fragment? [fragment vec]
  (and (<= (count fragment) (count vec))
       (every? true?
               (map = fragment (subvec vec 0 (count fragment))))))

(defn increment-last [vec]
  (try (conj (vec (butlast vec)) (inc (last vec))) (catch :Default _ vec)))

(defn sql-alias-replace [query]
  (let [deep-flat-query     (ut/deep-flatten query)

        ;;; temp - 11/28/23
        vpaths (try (let [kpaths              (ut/kvpaths query)
                          vpaths              (get (into {} (for [k kpaths] {(get-in query k) k})) :*code*)
                          vpaths              (assoc vpaths (- (count vpaths) 1) 1)
                          vpaths              (for [kp (filter #(starts-with-fragment? vpaths %) kpaths)] (get-in query kp))]  vpaths)
                    (catch :default _ []))

        all-clicked-walks   (into {}
                                  (map (fn [x] {x
                                                @(ut/tracked-subscribe [::generate-all-clicked
                                                                        {:query-key (-> x str (ut/replacer #":" "")
                                                                                        (ut/replacer "/*.clicked" "") keyword)}])
                                                ;(rfa/sub ::generate-all-clicked {:query-key (-> x str (ut/replacer #":" "") (ut/replacer "/*.clicked" "") keyword)})
                                                })
                                       (filter #(cstr/includes? (str %) "/*.clicked") deep-flat-query)))
        valid-params        (into (distinct
                                   ;@(ut/tracked-subscribe [::valid-body-params-all-condis])
                                   @(rfa/sub ::valid-body-params-all-condis)
                                   )
                                  (vec (remove nil? (for [k deep-flat-query]
                                                      (when (and (keyword? k)
                                                                 (cstr/includes? (str k) "/")
                                                                 (not (ut/is-sql-sql-alias? k))) k)))))
        valid-params        (cset/difference (set valid-params) (set (filter
                                                                      ;;#(and (keyword? %) (cstr/includes? (str %) "/"))
                                                                      ut/process-key
                                                                      vpaths)))
        valid-params-left   (cset/intersection (set deep-flat-query) (set valid-params))
        ;_ (when (some #(= :sseason %) deep-flat-query) (ut/tapp>> [:sql-alias-replace (count valid-params-left) (filter #(and (keyword? %) (cstr/includes? (str %) "/")) vpaths)]))
        value-walks-targets (filter #(and (cstr/includes? (str %) ".")
                                          (not (cstr/includes? (str %) ".*")) ;; dont want to treat multi params like this, they are 'sql-params' below instead
                                          (not (cstr/includes? (str %) "*."))) valid-params)
        value-walks         (into {} (for [k value-walks-targets] ;all-sql-call-keys]
                                       (let [fs    (ut/splitter (ut/safe-name k) "/")
                                             gs    (ut/splitter (last fs) ".")
                                             ds    (keyword (first fs))
                                             row   (int (last gs))
                                             field (keyword (first gs))]
                                         {k (get-in
                                             ;@(ut/tracked-subscribe [::conn/sql-data [ds]])
                                             @(rfa/sub ::conn/sql-data-alpha {:keypath [ds]})
                                             [row field])})))
        condi-walks-targets (filter #(cstr/includes? (str %) "condi/") valid-params)
        condi-walks         (into {} (for [k condi-walks-targets]
                                       {k ;;@(ut/tracked-subscribe [::conn/condi-value (keyword (last (ut/splitter (ut/safe-name k) "/")))])
                                        @(ut/tracked-sub ::conn/condi-value {:condi-key (keyword (last (ut/splitter (ut/safe-name k) "/")))})
                                        }))
        meta-walks          (into {} (for [k valid-params
                                           :when (cstr/includes? (str k) ":meta/")]
                                       {k (let [table-key (ut/replacer (str k) ":meta/" "")
                                                sql-safe (-> table-key str (ut/replacer #":" "") (ut/replacer #"-" "_"))]
                                            {:select [:*]
                                             :from   [[:fields :query-meta-subq]]
                                             :where  [:= :table_name sql-safe]})}))
        sql-params          (into {} (for [k valid-params]
                                       (if (cstr/includes? (str k) ":query/")
                                         {k (dissoc
                                             ;;@(ut/tracked-subscribe [::resolve-sql-alias (keyword (ut/replacer (str k) ":query/" ""))])
                                             @(rfa/sub ::resolve-sql-alias {:sql-key (keyword (ut/replacer (str k) ":query/" ""))})
                                             :style-rules)}
                                         {k
                                          ;@(ut/tracked-subscribe [::conn/clicked-parameter-key [k]])
                                          @(rfa/sub ::conn/clicked-parameter-key-alpha {:keypath [k]})
                                          })))
        replaced            (->> query
                                 (ut/postwalk-replacer condi-walks)
                                 (ut/postwalk-replacer all-clicked-walks)
                                 (ut/postwalk-replacer value-walks)
                                 (ut/postwalk-replacer meta-walks)
                                 (ut/postwalk-replacer sql-params))]
        ;_ (ut/tapp>> [:master-replace query valid-params all-clicked-walks value-walks meta-walks])

    (if
     (ut/ne? valid-params-left)
     (sql-alias-replace replaced)
     replaced)))

(re-frame/reg-sub
 ::sql-alias-replace-sub ;; caching abuse?
 (fn [_ {:keys [query]}]
   (sql-alias-replace query)))

(re-frame/reg-sub
 ::view-clicked-fields
 (fn [db [_ panel-key view-key]]
   (let [vw (get-in db [:panels panel-key :views view-key])
         vw-k (ut/kvpaths vw)
         vw-kf (first (filter #(= (last %) :click) vw-k))
         res (get-in db (vec (into [:panels panel-key :views view-key] vw-kf)) {})]
     (if (map? res) res {}))))



(re-frame/reg-event-db
 ::execute-pill-drop
 (undoable)
 (fn [db [_ panel-key query-key clause drop-data]]
   (let [clause-conform       (cond (or (= clause :where<>)
                                        (= clause :where=)) :where
                                    (or (= clause :is-in)
                                        (= clause :not-in)) :in
                                    (or (= clause :order-by-asc)
                                        (= clause :order-by-desc)) :order-by
                                    :else clause)
         drops                @(ut/tracked-sub ::all-drops-of-alpha {:ttype :*})
         new-q-key            (keyword (str "query-" (rand-int 12345)))
         parent-sql-sql-alias (ut/gen-sql-sql-alias)
         dvec                 (fn [v] (vec (set v)))
         kpath                [:panels panel-key :queries query-key clause-conform]
         existing-query-part  (get-in db kpath)]
     (reset! over-flow? false) ;; just in case we don't want to get stuck in an undraggable state
     (reset! over-block? false)
     (reset! dyn-dropper-hover nil) ;; weird bug where hover stays on after drop - late 2023

     (ut/tapp>> [:execute-pill-drop! clause-conform drops panel-key query-key clause drop-data])

     (cond                                                 ;p = clause-conform

       (some #(= clause-conform %) drops)
       (let [;dd @(ut/tracked-subscribe [::drop-details clause-conform])
             row-num      (get-in drop-data [:drag-body :row-num])
             src-table    (get-in drop-data [:drag-body :source-query])
             param-field  (get-in drop-data [:drag-body :param-field])
             param-key    (keyword (str (ut/unkey src-table) "/" (ut/unkey param-field) "." row-num))
             param-value  (get-in drop-data [:drag-body :param-full])
             target-block (get-in drop-data [:target-panel])
             cell-drop    (ut/safe-key (keyword (str "cell-drop-" (ut/replacer (str clause-conform) ":" ""))))
             ;label         (nth fields 0)
             md            (get-in drop-data [:drag-body :data-type])
              ;;  num?          (or (= md "integer") (= md "float"))
              ;;  formatted-val (if num? (str (nf param-value)) (str param-value))
              ;;  len           (count (str formatted-val))
              ;;  h             (get-in db [:panels target-block :h])
              ;;  w             (get-in db [:panels target-block :w])
             ;charw         (js/Math.ceil (/ (* (js/Math.ceil (/ (+ h w 1) 2)) brick-size) len))
             ;pxsize        (px charw)
             ;labelsz       (* (* w brick-size) 0.1)
             ;pxlabel       (px (if (< labelsz 20) 20 labelsz))
             new-view      [:box :size "auto"
                              ;; :align :center
                              ;; :justify :center
                             ;:padding "13px"
                            :style {:font-size [:auto-size-px param-key]
                                    :transition "all 0.6s ease-in-out"}
                            :child [clause-conform param-key]]]

         (if (not (and row-num src-table param-field))
           (do (ut/tapp>> [:cant-drop-text-view :missing-parts  src-table "/" param-field "." row-num])
               db)
           (-> db
               (assoc-in [:panels target-block :views cell-drop] new-view)
               (assoc-in [:panels target-block :selected-view] cell-drop))))

         ;(ut/tapp>> [:DROP! dd])
         ;db


       (= clause-conform :runstream-overrides)
       (let [{:keys [source target]} (get drop-data :drop-data)
             {:keys [type param-full param-type param-table param-field]} source
             {:keys [flow-id type key]} target]
         (-> db
             (assoc-in [:runstreams flow-id :values key :value] param-full)
             (assoc-in [:runstreams flow-id :values key :source] :param)))

       (cstr/starts-with? (str clause-conform) ":weave/")
       (let [weave-by (keyword (ut/replacer (str clause-conform) ":weave/" ""))
             source-panel-key (get-in drop-data [:drag-body :source-panel-key])
             source-view (get-in drop-data [:drag-body :source-table])
             view-body (get-in db [:panels source-panel-key :views source-view] [:div "where's the beef!"])
             ;view-query (get-in db [:panels source-panel-key :views source-view] [:div "where's the beef!"])

             fxky                (fn [k] (if (cstr/starts-with? (str k) ":query/") (keyword (ut/replacer k #":query/" "")) k))
             data-key-path       (or (first (filter #(= (last %) :data) (ut/kvpaths view-body))) [:nope])
             view-height         (or (first (filter #(= (last %) :height) (ut/kvpaths view-body))) [:nope])
             view-width          (or (first (filter #(= (last %) :width) (ut/kvpaths view-body))) [:nope])
             data-key            (or (first (filter #(= (last %) :dataKey) (ut/kvpaths view-body))) [:nope]) ;; temp
             view-data-ref       (get-in view-body data-key-path)
             base-query          @(ut/tracked-subscribe [::find-query (fxky view-data-ref)])
             weave-by-placeholder (keyword (str "_" (ut/replacer (str weave-by) #":" "")))
             inner-q-name        :gen-viz-609
             view-body           (-> view-body
                                     (assoc-in view-height 150)
                                     (assoc-in (conj (vec (drop-last data-key)) :isAnimationActive) false)
                                     (assoc-in view-width 300)
                                     (assoc-in data-key-path inner-q-name))
             modded-query        (if (get base-query :where)
                                   (assoc base-query :where [:and (get base-query :where) [:= weave-by weave-by-placeholder]])
                                   (assoc base-query :where [:= weave-by weave-by-placeholder]))
             host-query          (get-in db [:panels panel-key :queries query-key])
             field-name          (keyword (str "inviz_" (rand-int 1234)))
             render-field        [[:*render*
                                   {:queries
                                    {inner-q-name
                                     modded-query}
                                    :view
                                    view-body}] field-name]
             host-query-modded   (-> host-query
                                     (assoc :row-height 100)
                                     (assoc-in [:col-widths field-name] 200)
                                     (assoc :select (vec (conj (ut/postwalk-replacer {} ;{weave-by [weave-by weave-by-placeholder]} ;; alias it
                                                                                      (get host-query :select))    ;; need to this to account for things already aliased
                                                               render-field)))
                                     (assoc :group-by (vec (conj (get host-query :group-by) (+ (count (get host-query :select)) 1)))))]
            ;;  fxky                (fn [k] (if (cstr/starts-with? (str k) ":query/") (keyword (ut/replacer k #":query/" "")) k))
            ;;  base-query          @(ut/tracked-subscribe [::find-query (fxky view-data-ref)])
            ;;  base-query-from     (first (get base-query :from))
            ;;  base-query-fields   (cond (map? base-query-from) (get base-query-from :select)
            ;;                            (keyword? base-query-from)
            ;;                            (get @(ut/tracked-subscribe [::find-query (fxky base-query-from)]) :select [])
            ;;                            :else [])
            ;;  local-fields        (get @(ut/tracked-subscribe [::find-query query-key]) :select [])
            ;;  local-meta          (get @(ut/tracked-subscribe [::conn/sql-metadata [query-key]]) :fields)
            ;;                            ;local-parent-fields (get-in table-source [0 0 :select])
            ;;  possibles           (vec (cset/intersection (set local-fields) (set base-query-fields)))
            ;;  possibles-vec       (vec (for [p possibles
            ;;                                 :when (<= (get-in local-meta [p :distinct]) 20)] p))


         (ut/tapp>> [:view-body-drop ; weave-by view-data-ref view-body modded-query
                host-query-modded])
         (assoc-in db [:panels panel-key :queries query-key] host-query-modded))
         ;db


       (not (nil? (get clause-conform :layout-drop)))
       (let [target             (get clause-conform :layout-drop)
             incoming-view-src  (get-in drop-data [:drag-body :source-table])
             incoming-block-src (get-in drop-data [:drag-body :source-panel-key] :query)
             incoming-block-src (if (= (get-in drop-data [:drag-body :type]) :query) :query incoming-block-src) ;; override with query keyword prefix
             compound-key       (try (keyword (str "view/" (name incoming-block-src) "." (name incoming-view-src))) (catch :default e (do (ut/tapp>> [:error e]) nil)))
             target-kp          (vec (flatten (get clause-conform :kp)))
             exists?            (not (nil? (get-in db (conj target-kp compound-key))))]

              ;_ (ut/tapp>> [:adding-view-to-layout target-kp compound-key])


         (ut/tapp>> [:adding-view-to-layout target-kp compound-key (conj target-kp compound-key) exists? (conj target-kp target) (get-in db (conj target-kp target))])
         (if exists?
           (-> db                                          ;; save the old pos with a new name, remove the old compound key version, insert new one
               (assoc-in (conj target-kp (keyword (str "empty-panel" (rand-int 123)))) (get-in db (conj target-kp compound-key)))
               (ut/dissoc-in (conj target-kp target))
               (assoc-in (conj target-kp compound-key) (get-in db (conj target-kp target))))
           (-> db                                          ;; add new pos with old layout, remove old pos
               (assoc-in (conj target-kp compound-key) (get-in db (conj target-kp target)))
               (ut/dissoc-in (conj target-kp target)))))

       (= clause-conform :group-by)
       (-> db
           (assoc-in [:panels panel-key :queries query-key :group-by]
                     (dvec (conj (get-in db [:panels panel-key :queries query-key :group-by])
                                 (get-in drop-data [:drag-body :target]))))
           (assoc-in [:panels panel-key :queries query-key :select]
                     (dvec (conj (get-in db [:panels panel-key :queries query-key :select])
                                 (get-in drop-data [:drag-body :target])))))

       (and (= clause-conform :text)
            (= (get-in drop-data [:drag-body :type]) :where)) ;; basic single cell drag
       (let [;_ (ut/tapp>> [:started?])
             row-num      (get-in drop-data [:drag-body :row-num])
             src-table    (get-in drop-data [:drag-body :source-query])
             param-field  (get-in drop-data [:drag-body :param-field])
             param-key    (keyword (str (ut/unkey src-table) "/" (ut/unkey param-field) "." row-num))
             param-value  (get-in drop-data [:drag-body :param-full])
             target-block (get-in drop-data [:target-panel])
             cell-drop    (keyword (str "cell-drop-" (rand-int 12345)))
             is-image?    (and
                           (or (cstr/ends-with? (cstr/lower-case (str param-value)) ".png")
                               (cstr/ends-with? (cstr/lower-case (str param-value)) ".jpg")
                               (cstr/ends-with? (cstr/lower-case (str param-value)) ".jpeg")
                               (cstr/ends-with? (cstr/lower-case (str param-value)) ".gif")
                               (cstr/includes? (cstr/lower-case (str param-value)) "/images/")
                               (cstr/includes? (cstr/lower-case (str param-value)) "/image/"))
                           (cstr/starts-with? (cstr/lower-case (str param-value)) "http"))
             is-video?     (or (cstr/ends-with? (cstr/lower-case (str param-value)) ".mp4")
                               (cstr/ends-with? (cstr/lower-case (str param-value)) ".mov"))
             ;label         (nth fields 0)
             md            (get-in drop-data [:drag-body :data-type])
             num?          (or (= md "integer") (= md "float"))
             formatted-val (if num? (str (nf param-value)) (str param-value))
             len           (count (str formatted-val))
             h             (get-in db [:panels target-block :h])
             w             (get-in db [:panels target-block :w])
             ;charw         (js/Math.ceil (/ (* (js/Math.ceil (/ (+ h w 1) 2)) brick-size) len))
             ;pxsize        (px charw)
             ;labelsz       (* (* w brick-size) 0.1)
             ;pxlabel       (px (if (< labelsz 20) 20 labelsz))
             new-view      [:box :size "auto"
                            :align :center
                            :justify :center
                            ;:padding "13px"
                            :style {:font-size
                                    [:auto-size-px param-key]
                                    :transition "all 0.6s ease-in-out"}
                            :child (cond is-image? [:img {:src param-key :width "100%"}]
                                         is-video? [:iframe ;; :video ? html5 shit instead?
                                                    {:src :movies_deets/_1080p_video
                                                     :style {:border "none"
                                                             :width :panel-width+80-px
                                                             :height :panel-height+80-px}}]
                                         :else [:string param-key])]]
         (ut/tapp>> [:text-drop param-key drop-data])
         (if (not (and row-num src-table param-field))
           (do (ut/tapp>> [:cant-drop-text-view :missing-parts  src-table "/" param-field "." row-num])
               db)
           (-> db
               (assoc-in [:panels target-block :views cell-drop] new-view)
               (assoc-in [:panels target-block :selected-view] cell-drop))))

       (= clause-conform :where)
       (let [exist?      (not (empty? existing-query-part))
             exclude?    (= clause :where<>)
             dest-field  (get-in drop-data [:drag-body :param-field])
             kw-alias    (get-in drop-data [:drag-body :param-full])
             param?      (keyword? kw-alias)
             stmt        (if param? [:*when kw-alias ;; no need to wrap literals in :*when
                                     [(if exclude? :<> :=) dest-field
                                      kw-alias]]
                             [(if exclude? :<> :=) dest-field kw-alias])
             new         (if exist?
                           [:and existing-query-part stmt]
                           stmt)]
         (assoc-in db kpath new))

       (= clause-conform :in)
       (let [where-exist (get-in db [:panels panel-key :queries query-key :where])
             exist?   (not (empty? where-exist))
             exclude? (= clause :not-in)
             dest-field  (get-in drop-data [:drag-body :param-field])
             kw-alias    (get-in drop-data [:drag-body :param-full])
             stmt     [:*when kw-alias
                       [:in dest-field kw-alias]]
             stmt     (if exclude? [:not stmt] stmt)
             new      (if exist?
                        [:and where-exist stmt]
                        stmt)
             kpath   (vec (conj (vec (drop-last kpath)) :where))]
         ;(ut/tapp>> [:in where-exist exist? new kpath])
         (assoc-in db kpath new))

       (= clause-conform :filter-shadow)
       (let [where-exist     (get-in db [:panels panel-key :queries query-key :where])
             source-query    (get-in drop-data [:drag-body :source-table])
             target-query    (get-in drop-data [:target-query])
             gb1             @(ut/tracked-subscribe [::group-bys target-query])
             gb2             @(ut/tracked-subscribe [::group-bys source-query])
             group-bys       (vec (cset/intersection (set gb1) (set gb2)))
             clicked-keyword (keyword (str (ut/replacer source-query #":" "") "/*.clicked"))
             exist?          (not (empty? where-exist))
             stmt            [:*if clicked-keyword [:*all= clicked-keyword group-bys] [:= 1 1]]
             new             (if exist?
                               [:and where-exist stmt]
                               stmt)
             kpath           (vec (conj (vec (drop-last kpath)) :where))]
         (assoc-in db kpath new))

       (= clause-conform :highlight-rows)
       (let [exist     (get-in db [:panels panel-key :queries query-key :style-rules])
             source-query    (get-in drop-data [:drag-body :source-table])
             target-query    (get-in drop-data [:target-query])
             gb1             @(ut/tracked-subscribe [::group-bys target-query])
             gb2             @(ut/tracked-subscribe [::group-bys source-query])
             group-bys       (vec (cset/intersection (set gb1) (set gb2)))
             rule-key        [:* (keyword (str "highlight-row-" (rand-int 12345)))]
             clicked-keyword (keyword (str (ut/replacer source-query #":" "") "/*.clicked"))
             exist?          (not (empty? exist))
             stmt            {:logic [:*if clicked-keyword [:*all= clicked-keyword group-bys] false]
                              :style {:background-color (str (theme-pull :theme/editor-outer-rim-color nil) 45) ;;"#008b8b66"
                                      :border (str "1px solid " (theme-pull :theme/editor-outer-rim-color nil))}}
             new             (if exist?
                               (assoc exist rule-key stmt)
                               {rule-key stmt})
             kpath           (vec (conj (vec (drop-last kpath)) :style-rules))]
         (assoc-in db kpath new))

       (= clause-conform :click-filter)
       (let [where-exist     (get-in db [:panels panel-key :queries query-key :where])
             source-query    (get-in drop-data [:drag-body :source-table])
             src-panel-key   (get-in drop-data [:drag-body :source-panel-key])
             target-query    (get-in drop-data [:target-query])
             gb1             @(ut/tracked-subscribe [::group-bys target-query])
             gb2             (vec (vals @(ut/tracked-subscribe [::view-clicked-fields src-panel-key source-query])))
             group-bys       (vec (cset/intersection (set gb1) (set gb2)))
             clicked-keyword (keyword (str (ut/replacer src-panel-key #":" "") "/*.clicked"))
             exist?          (not (empty? where-exist))
             stmt            [:*if clicked-keyword [:*all= clicked-keyword group-bys] [:= 1 1]]
             new             (if exist?
                               [:and where-exist stmt]
                               stmt)
             kpath           (vec (conj (vec (drop-last kpath)) :where))]
         ;(ut/tapp>> [:click-filter target-query gb1 gb2 group-bys drop-data])
         (assoc-in db kpath new))

        ;; "🌶️pivot-on/"
       (or (cstr/starts-with? (str clause-conform) ":pivot-on/")
           (cstr/starts-with? (str clause-conform) ":🌶️pivot-on/"))
       (let [pivot-field   (keyword (last (ut/splitter (str clause-conform) #"/")))
             agg-field     (get-in drop-data [:drag-body :target])
             spicy?        (cstr/starts-with? (str clause-conform) ":🌶️pivot-on/")
             existing-q    (get-in db [:panels panel-key :queries query-key])
             aggy (if (= agg-field :rowcnt) :count [:sum agg-field]) ;; rowcnt is not real
             pivot-by-map  {pivot-field [aggy {:select-distinct [pivot-field] :from [[existing-q (keyword (str "subqpiv" (rand-int 1234)))]]}]}
             new-query (-> existing-q
                           (assoc :pivot-by pivot-by-map)
                           (assoc :select (ut/remove-key (get existing-q :select) pivot-field))
                           (assoc :group-by (ut/remove-key (get existing-q :group-by) pivot-field)))
             new-query (if spicy?
                         (-> new-query)
                              ;(assoc :style-rules (merge (get existing-q :style-rules) ;;; ugh, need pre-pivot values... :/
                              ;                           (keys (get-in db [:meta query-key pivot-field :commons]))
                              ;                           ))

                         new-query)]
         (ut/tapp>> [:pivot pivot-field existing-q drop-data new-query])
         (assoc-in db [:panels panel-key :queries query-key] new-query))

       (cstr/starts-with? (str clause-conform) ":join-on/")
       (let [add-alias                     (fn [alias field] (keyword (str (ut/safe-name alias) "/" (ut/safe-name field))))
             join-field                    (keyword (last (ut/splitter (str clause-conform) #"/")))
             existing-q                    (get-in db [:panels panel-key :queries query-key])
             existing-selects              (get existing-q :select)
             existing-table-alias          (get-in existing-q [:from 0 1]) ;; eyes emoji
             existing-table-fields         (keys (get-in db [:meta query-key :fields]))
             existing-alias-walk           (into {} (for [e existing-table-fields] {e (add-alias existing-table-alias e)}))
             existing-joins                (get-in existing-q [:join])
             existing-joins?               (not (empty? existing-joins))
             aliased-q                     (ut/postwalk-replacer existing-alias-walk existing-q)

             incoming-table                (get-in drop-data [:drag-body :source-table])
             incoming-table-alias          parent-sql-sql-alias
             incoming-table-fields         (keys (get-in db [:meta incoming-table :fields]))
             incoming-table-fields-dupes   (cset/intersection (set incoming-table-fields) (set existing-table-fields))
             incoming-table-fields-final   (cset/difference (set incoming-table-fields) incoming-table-fields-dupes)
             incoming-table-fields-aliased (vec (for [e incoming-table-fields-final]
                                                  (add-alias incoming-table-alias e)))

             subquery?                     (not (= (get-in drop-data [:drag-body :source-panel-key]) "none!"))
             incoming-table                (if subquery? (keyword (str "query/" (ut/safe-name incoming-table))) incoming-table)

             aliased-q                     (assoc aliased-q :join (if existing-joins?
                                                                    (vec (concat existing-joins
                                                                                 [[incoming-table incoming-table-alias]
                                                                                  [:= (add-alias incoming-table-alias join-field)
                                                                                    ;(add-alias existing-table-alias join-field) ;; this is for BASE TABLE alias, if previously joined field, use that one
                                                                                   (first (filter #(cstr/includes? (str %) (str "/" (name join-field))) existing-selects))]])) ;; maybe we should ALWAYS use this one? below also

                                                                    [[incoming-table incoming-table-alias]
                                                                     [:= (add-alias incoming-table-alias join-field)
                                                                      (add-alias existing-table-alias join-field)]]))
             aliased-q                     (assoc aliased-q :select (vec (apply merge (get aliased-q :select) (vec incoming-table-fields-aliased))))]
         (ut/tapp>> [:join-debug existing-selects (add-alias incoming-table-alias join-field) (name join-field) (first (filter #(cstr/includes? (str %) (str "/" (name join-field))) existing-selects))])
         (-> db                                            ;; step 1 - alias all table fields in existing query (postwalk on whole map using source fields?)
              ;; step 2 - add :join clause with ON
              ;; step 3 - add joined select fields with aliases (unless they clash with existing ones)
             (assoc-in [:panels panel-key :queries query-key] aliased-q)))

       (some #(= clause-conform %) [:highlight= :highlight> :highlight< :heatmap])
       (let [existing (get-in db [:panels panel-key :queries query-key :style-rules] {})
              ;rname (keyword (str (ut/safe-name clause-conform) (rand-int 12345)))
             heatmap? (true? (= clause-conform :heatmap))
             rname    (keyword (str (if heatmap? "heatmap-" "highlight-") (rand-int 12345)))]
         (assoc-in db [:panels panel-key :queries query-key :style-rules]
                   (merge existing {(if (= clause-conform :heatmap)
                                      [(get-in drop-data [:drag-body :param-field]) rname]
                                      [:* rname])
                                    {:logic (if (= clause-conform :heatmap)
                                              true
                                              [(condp = clause-conform
                                                 :highlight= :=
                                                 :highlight> :>
                                                 :highlight< :<)
                                               (get-in drop-data [:drag-body :param-field])
                                               (get-in drop-data [:drag-body :param-full])])
                                     :style (merge {:background-color (if (= clause-conform :heatmap)
                                                                        [:heatmap] ;; :PuOr :11 :asc
                                                                        "#008b8b66")
                                                    :border           "1px solid #00000000"}
                                                   (if (= clause-conform :heatmap) {:color "#000000"} {}))}})))
        ;(assoc-in db [:panels panel-key :queries query-key :order-by]
        ;          (dvec (conj (get-in db [:panels panel-key :queries query-key :order-by] [])
        ;                      [(get-in drop-data [:drag-body :target]) (if (= clause :order-by-desc) :desc :asc)])))

        ;(= clause-conform :sum)
       (some #(= clause-conform %) [:sum :avg :min :max :count])
       (let [falias (keyword (str (ut/safe-name (get-in drop-data [:drag-body :target])) "_" (ut/safe-name clause-conform) "_" (rand-int 212)))]
         (assoc-in db [:panels panel-key :queries query-key :select]
                   (dvec (conj (get-in db [:panels panel-key :queries query-key :select])
                               [[clause-conform
                                 (if (= clause-conform :count) 1
                                     (get-in drop-data [:drag-body :target]))] falias]))))

       (= clause-conform :count-distinct)
       (let [falias (keyword (str (ut/safe-name (get-in drop-data [:drag-body :target])) "_" "countd" "_" (rand-int 212)))]
         (assoc-in db [:panels panel-key :queries query-key :select]
                   (dvec (conj (get-in db [:panels panel-key :queries query-key :select])
                               [[:count [:distinct (get-in drop-data [:drag-body :target])]] falias]))))

       (= clause-conform :table-filter)
       (let [src-table      (get-in drop-data [:drag-body :source-table])
             target-field   (get-in drop-data [:drag-body :target])
             conn-id        (get-in drop-data [:drag-body :connection-id])
             src-panel-key  (get-in drop-data [:drag-body :source-panel-key])
             src-query      (get-in drop-data [:drag-body :source-query])
             subquery?      (not (= src-panel-key "none!"))
             already-query? (true? (cstr/includes? (str src-table) "query/"))
             src-table      (if (and subquery? (not already-query?))
                              (keyword (str "query/" (ut/safe-name src-table)))
                              src-table)
             src-where      (get-in db [:panels src-panel-key :queries src-query :where])
             exists?        (not (nil? src-where))
             ;new-param      (keyword (str (ut/safe-name new-q-key) "/" (ut/safe-name target-field)))
             new-param      (keyword (str (ut/safe-name new-q-key) "/*.clicked"))
             cla            [:*when new-param
                             [:*all= new-param [target-field]]]
             new-where      (if exists? [:and src-where cla] cla)]
        ; (ut/tapp>> [:dropper! panel-key drop-data])
         (-> db
             (assoc-in [:panels panel-key :selected-view] new-q-key)
             (assoc-in [:panels panel-key :queries new-q-key]
                       (sql-alias-replace-sub {:select   [target-field [[:count 1] :rowcnt]] ;; sql-alias-replace to materialized it from parent... "promoted"
                                               :from     [[src-table parent-sql-sql-alias]]
                                               :group-by [target-field]
                                               :order-by [[target-field :asc]]}))
             (assoc-in [:panels panel-key :connection-id] conn-id)
             (assoc-in [:panels src-panel-key :queries src-query :where] new-where)))

       (= clause-conform :order-by)
       (assoc-in db [:panels panel-key :queries query-key :order-by]
                 (dvec (conj (get-in db [:panels panel-key :queries query-key :order-by] [])
                             [(get-in drop-data [:drag-body :target]) (if (= clause :order-by-desc) :desc :asc)])))

       (= clause-conform :row-count)
       (let [src-table (get-in drop-data [:drag-body :source-table])
             subquery? (not (= (get-in drop-data [:drag-body :source-panel-key]) "none!"))
             src-table (if subquery? (keyword (str "query/" (ut/safe-name src-table))) src-table)]
         (-> db
             (assoc-in [:panels panel-key :queries new-q-key]
                       {:select [[[:count 1] :rowcnt]]
                        :from   [[src-table parent-sql-sql-alias]]})
             (assoc-in [:panels panel-key :connection-id]
                       (get-in drop-data [:drag-body :connection-id]))
             (assoc-in [:panels panel-key :selected-view] new-q-key)))

       (= clause-conform :remove)
       (let [fname      (get-in drop-data [:drag-body :target])
             select     (get-in db [:panels panel-key :queries query-key :select] [])
             order-by   (get-in db [:panels panel-key :queries query-key :order-by] [])
             group-by   (get-in db [:panels panel-key :queries query-key :group-by] [])
             exists-in? (fn [name entry] (or (= name entry) (some #(= name %) (ut/deep-flatten entry))))
             new-select (vec (remove nil? (for [e select] (when (not (exists-in? fname e)) e))))
             new-order  (vec (remove nil? (for [e order-by] (when (not (exists-in? fname e)) e))))
             new-group  (vec (remove nil? (for [e group-by] (when (not (exists-in? fname e)) e))))]
         (-> db
             (assoc-in [:panels panel-key :queries query-key :select] new-select)
             (assoc-in [:panels panel-key :queries query-key :order-by] new-order)
             (assoc-in [:panels panel-key :queries query-key :group-by] new-group)
             (ut/dissoc-in [:panels panel-key :queries query-key (if (empty? new-order) :order-by :nevermind!)]) ;; dirty hack (honey-sql fails w empty clauses)
             (ut/dissoc-in [:panels panel-key :queries query-key (if (empty? new-group) :group-by :nevermind!)]))) ;; dirty hack (honey-sql fails w empty clauses)


       (= clause-conform :promote-query)
       (assoc-in db [:panels panel-key :queries query-key] ;; materialize and overwrite
                 (sql-alias-replace-sub (get-in db [:panels panel-key :queries query-key])))

       (or (= clause-conform :delete-query) (= clause-conform :delete-view))
       (let [source-block      (get-in drop-data [:drag-body :source-panel-key])
             view-cnt          (count (keys (get-in db [:panels source-block :views])))
             q-cnt             (count (keys (get-in db [:panels source-block :queries])))
             source-block-tabs (+ view-cnt q-cnt)
             last-tab?         (= source-block-tabs 1)
             source-tab        (get-in drop-data [:drag-body :source-table])
             selected-block    (get db :selected-block)
             is-selected?      (= source-block selected-block)
              ; dest-block (get drop-data :target-panel)
             source-type       (get-in drop-data [:drag-body :type])
             source-key        (if (= source-type :query) :queries :views)]
              ; dest-tabs (keys (get-in db [:panels dest-block source-key]))
              ; connection-id (get-in db [:panels source-block :connection-id])
              ; new-tab-data (get-in db [:panels source-block source-key source-tab])
              ; new-tab-name (if (= clause-conform :copy-into)
              ;                (if (some #(= % source-tab) dest-tabs) ;; confliting names?
              ;                  (keyword (str (ut/safe-name source-tab) "-" (rand-int 99)))
              ;                  source-tab) ;; else leave the name alone!
              ;                (keyword (str (ut/safe-name source-tab) "-copy-" (rand-int 99))))

         (when (= clause-conform :delete-query) (ut/tracked-dispatch [::unalias-query source-tab])) ;; dont orphan refs!
         (-> db
             (ut/dissoc-in [:panels source-block source-key source-tab])
             (assoc :selected-block (if (and last-tab? is-selected?) nil selected-block)) ;; unselect block if we are deleting it....
             (ut/dissoc-in (if (and (= clause-conform :delete-view) (= view-cnt 1))
                             [:panels source-block source-key]
                             [:panels source-block source-key :nevermind!])) ;; cheese man. cheese. its late.
             (ut/dissoc-in (if last-tab?                   ;; if we are moving out of the last tab, just kill the block too (as long as we "unaliased" it first) ^^^
                             [:panels source-block]        ;; another dirty hack
                             [:panels source-block :nevermind!]))))

       (or (= clause-conform :copy-into) (= clause-conform :move-into) (= clause-conform :reference))
       (let [source-block      (get-in drop-data [:drag-body :source-panel-key])
             source-block-tabs (+ (count (keys (get-in db [:panels source-block :views])))
                                  (count (keys (get-in db [:panels source-block :queries]))))
             last-tab?         (= source-block-tabs 1)
             source-tab        (get-in drop-data [:drag-body :source-table])
             dest-block        (get drop-data :target-panel)
             source-type       (get-in drop-data [:drag-body :type])
             source-key        (if (= source-type :query) :queries :views)
             query?            (= source-type :query)
             view?             (not query?)
             dest-tabs         (keys (get-in db [:panels dest-block source-key]))
             connection-id     (get-in db [:panels source-block :connection-id])
             new-tab-data      (get-in db [:panels source-block source-key source-tab])
             new-tab-name      (if (= clause-conform :move-into)
                                 (if (some #(= % source-tab) dest-tabs) ;; confliting names?
                                   (keyword (str (ut/safe-name source-tab) "-" (rand-int 99)))
                                   source-tab)             ;; else leave the name alone!
                                 (keyword (str (ut/safe-name source-tab) "-copy-" (rand-int 99))))]

         (cond (and view? (= clause-conform :reference))
               (-> db
                   (assoc-in [:panels dest-block source-key new-tab-name]
                             (keyword (str "view/" (ut/safe-name source-block) "." (ut/safe-name source-tab)))))

               query?
               (if (= clause-conform :copy-into)
                 (-> db
                     (assoc-in [:panels dest-block source-key new-tab-name] new-tab-data)
                     (assoc-in [:panels dest-block :connection-id] connection-id))
                 (-> db
                     (assoc-in [:panels dest-block source-key new-tab-name] new-tab-data)
                     (assoc-in [:panels dest-block :connection-id] connection-id)
                     (ut/dissoc-in [:panels source-block source-key source-tab])
                     (ut/dissoc-in (if last-tab?           ;; if we are moving out of the last tab, just kill the block too
                                     [:panels source-block] ;; another dirty hack
                                     [:panels source-block :nevermind!]))))

               :else
               (if (= clause-conform :copy-into)           ;; for views DONT overwrite the connection-id
                 (-> db
                     (assoc-in [:panels dest-block source-key new-tab-name] new-tab-data))
                      ;(assoc-in [:panels dest-block :connection-id] connection-id)

                 (-> db
                     (assoc-in [:panels dest-block source-key new-tab-name] new-tab-data)
                      ;(assoc-in [:panels dest-block :connection-id] connection-id)
                     (ut/dissoc-in [:panels source-block source-key source-tab])
                     (ut/dissoc-in (if last-tab?           ;; if we are moving out of the last tab, just kill the block too
                                     [:panels source-block] ;; another dirty hack
                                     [:panels source-block :nevermind!]))))))

       (some #(= clause-conform %) [:x :y :color :row :column :size :shape]) ;;  :row :column :size :shape | hardcoded & named :oz view
       (let [fname          (get-in drop-data [:drag-body :target])
             source-table   (ut/unre-qword (get-in drop-data [:drag-body :source-table]))
             views          (get-in db [:panels panel-key :views])
             selected-view  (get-in db [:panels panel-key :selected-view])
             has-queries?   (not (nil? (get-in db [:panels panel-key :queries])))
             no-view?       (nil? views)
             single-view?   (vector? views)
             view-map?      (map? views)
             selected-view? (cond no-view? false
                                  view-map? (true? (some #(= % selected-view) views))
                                  (not has-queries?) true
                                  :else false)
             view-body      (ut/postwalk-replacer {}      ;{:vega-default-color-scheme (theme-pull :theme/vega-default-color-scheme {:scheme "inferno"})}
                                                   (cond (= :vega-lite (first (get views :oz))) ;; this is the view, and STOCK vega code exists
                                                         (get views :oz)
                                                         :else vega-lite-shell))
             new-view-body  (-> view-body
                                (assoc-in [1 :layer 0 :encoding clause-conform :field] fname)
                                (assoc-in [1 :data :values] source-table))]
              ;keypath (cond )

         (assoc-in db [:panels panel-key :views :oz] new-view-body))))))

(defn droppable-pills [types-vec drop-data element]
  ;(if @dragging?
  [(reagent/adapt-react-class rdnd/Droppable)
   {:types   types-vec
    :on-drop #(let [incoming (js->clj %)
                    data     (read-string (last (last incoming)))
                    type     (first (first incoming))]
                (ut/tapp>> [:dropped-pill type data
                       [::execute-pill-drop
                        (get drop-data :target-panel)
                        (get drop-data :target-query)
                        (get drop-data :clause) drop-data]])
                (reset! dragging? false)
                ;(reset! dyn-dropper-hover nil)
                (reset! on-block? false)
                (reset! dragging-size [])
                ;(insert-new-block root 3 3 rooted-data)
                (ut/tracked-dispatch [::execute-pill-drop (get drop-data :target-panel)
                                    (get drop-data :target-query)
                                    (get drop-data :clause) drop-data]))}
   [re-com/box :child element]])
   ;[:div element]
   ;element
                                                       ;element))

;; (defn font-size-px [s h w]
;;   (let [len    (count (str s))
;;         charw  (js/Math.ceil (/ (* (js/Math.ceil (/ (+ h w 1) 2)) brick-size) len))
;;         pxsize (px charw)]
;;     pxsize))

(defn font-size-px [s h w] 17)

(re-frame/reg-sub
 ::all-drops-of
 (fn [db [_ ttype]]
   (let [rss (get-in db [:runstream-drops] {})]
     (vec
      (apply concat
             (for [[_ v] rss]
               (for [[kk vv] v
                     :when (try (or (= ttype :*)
                                    (some #(= % ttype) (get vv :type))
                                    (= ttype (get vv :type))) (catch :default _ false))]
                 kk)))))))

(re-frame/reg-sub
 ::all-drops-of-alpha
 (fn [db {:keys [ttype]}]
   (let [rss (get-in db [:runstream-drops] {})]
     (vec
      (apply concat
             (for [[_ v] rss]
               (for [[kk vv] v
                     :when (try (or (= ttype :*)
                                    (some #(= % ttype) (get vv :type))
                                    (= ttype (get vv :type))) (catch :default _ false))]
                 kk)))))))

(defn dynamic-spawner-targets [table-source dbody panel-key query-key width-int height-int]
  ;(when true ;(= panel-key :block-4778)
  ;(ut/tapp>> [:dynamic-spawner-targets panel-key query-key [table-source dbody width-int height-int]])
  (let [drag-body              (get dbody :drag-meta)
        selected-view          @(ut/tracked-subscribe [::selected-view-w-queries panel-key])
        is-layout?             @(ut/tracked-subscribe [::is-layout? panel-key selected-view])

        layout-map             (when is-layout? (let [view @(ut/tracked-subscribe [::view panel-key selected-view])
                                                      kvs  (first (filter #(= (get-in view %) :layout) (ut/kvpaths view)))
                                                      kvs1 (assoc kvs (dec (count kvs)) 1)
                                                      v    (get-in view kvs1)]
                                                  [v [:panels panel-key :views selected-view kvs1 :panels]]))
        view-data-ref         (get-in dbody (or (first (filter #(= (last %) :data) (ut/kvpaths dbody))) [:nope]))
       ; _                      (when (= panel-key :block-4778)
       ;                          (ut/tapp>> [:view-data-ref view-data-ref]))
        viz-for-each-fields     (when (and (keyword view-data-ref) (not (nil? query-key)))
                                  (let [fxky                (fn [k] (if (cstr/starts-with? (str k) ":query/") (keyword (ut/replacer k #":query/" "")) k))
                                        base-query          @(ut/tracked-subscribe [::find-query (fxky view-data-ref)])
                                        base-query-from     (first (get base-query :from))
                                        base-query-fields   (cond (map? base-query-from) (get base-query-from :select)
                                                                  (keyword? base-query-from)
                                                                  (get @(ut/tracked-subscribe [::find-query (fxky base-query-from)]) :select [])
                                                                  :else [])
                                        local-fields        (get @(ut/tracked-subscribe [::find-query query-key]) :select [])
                                        local-meta          (get @(ut/tracked-subscribe [::conn/sql-metadata [query-key]]) :fields)
                                       ;local-parent-fields (get-in table-source [0 0 :select])
                                        possibles           (vec (cset/intersection (set local-fields) (set base-query-fields)))
                                        possibles-vec       (vec (for [p possibles
                                                                       :when (<= (get-in local-meta [p :distinct]) 50)] p))]
                                   ;(ut/tapp>> [:for-each-viz-fn? possibles possibles-vec base-query-fields local-fields])
                                    possibles-vec))

        ;source-table (get drag-body :source-table)
        ;is-query-tab?  (>= (count (get dbody :queries)) 1)
        source-panel-key       (get drag-body :source-panel-key)
        source-table           (get drag-body :source-table)
        data-type              (get drag-body :data-type)
        target                 (get drag-body :target)
        table-source-k         (if (vector? (first table-source)) (first (first table-source)) (first table-source))
        ;test @dyn-dropper-hover
        not-sys?               (not (or (= query-key :tables-sys)
                                        (= query-key :fields-sys)
                                        (= query-key :attribs-sys)
                                        (= query-key :combos-sys)
                                        (= query-key :connections-sys)))
        already-agg?           nil                          ;; for pivot
        agg?                   (and (or (= data-type "integer") (= data-type "float"))
                                    (not (cstr/includes? (cstr/lower-case (str target)) "year"))
                                    (not (cstr/includes? (cstr/lower-case (str target)) "month"))
                                    (not (cstr/includes? (cstr/lower-case (str target)) "week")))
        dim?                   (not agg?)
        connection-id          @(ut/tracked-subscribe [::connection-id panel-key])
        drag-type              (get drag-body :type)
        param?                 (and (or (= drag-type :param)
                                        (= drag-type :where))
                                    (not (cstr/includes? (str (get drag-body :param-full)) ".*/"))) ;; ignore multi-params, they need special handling
        query?                 (= drag-type :query)
        table?                 (or (= drag-type :meta-tables) (= drag-type :meta-fields) query?)
        field?                 (or (= drag-type :meta-fields) (= drag-type :field))
        viz-reco?              (= drag-type :viz-reco)
        not-viz-reco?          (not viz-reco?)
        view-drag?             (= drag-type :view)
        block-panel?           (true? (or view-drag? query?))
        not-view?              (not view-drag?)
        same-db?               (= connection-id (get drag-body :connection-id))
        same-table?            (or (= source-table (first table-source))
                                   ;(= (ut/unre-qword source-table) (ut/unre-qword (first table-source)))
                                   (= table-source-k source-table))
        clicked-fields         (if view-drag? @(ut/tracked-subscribe [::view-clicked-fields source-panel-key source-table]) [])
        clicked?               (ut/ne? clicked-fields)
        query-fields           (if (or clicked? table?)
                                 (vec (keys (get @(ut/tracked-subscribe [::conn/sql-metadata [query-key]]) :fields))) [])
        ;dim-query-fields (into {} (filter (fn [[_ v]] (= (get v :data-type) "string")) (get @(ut/tracked-subscribe [::conn/sql-metadata [query-key]]) :fields)))
        pivot-dim-query-fields (keys (into {} (filter (fn [[_ v]] (and (= (get v :data-type) "string") (< (get v :distinct) 50)))
                                                      (get @(ut/tracked-subscribe [::conn/sql-merged-metadata [query-key]]) :fields))))
        common-fields          (cond table? (vec (cset/intersection (set query-fields) (set (get drag-body :table-fields))))
                                     clicked? (vec (cset/intersection (set (vals clicked-fields)) (set query-fields)))
                                     :else [])
        union?                 (and (= (sort query-fields) (sort (get drag-body :table-fields))) (ut/ne? query-fields))
        partial-union?         (and (>= (count (cset/intersection (set query-fields)
                                                                  (set (get drag-body :table-fields)))) 1) ;; do they share at least 2 field names?
                                    (ut/ne? query-fields))
        common-fields?         (>= (count common-fields) 1)

        ;font-size-int (int (/ width-int 17))
        ;font-size (px (if (> font-size-int 35) 35 font-size-int))


        self?                  (= panel-key source-panel-key)
        view-based?            (nil? table-source)
        w                      @(ut/tracked-subscribe [::panel-width panel-key])
        h                      @(ut/tracked-subscribe [::panel-height panel-key])

        multi-param?           (and (or (= drag-type :param)
                                        (= drag-type :where))
                                    (cstr/includes? (str (get drag-body :param-full)) ".*/"))
        is-child?              (cstr/includes? (str table-source) ":query/")
        sql-options            (vec
                                (apply concat
                                       (remove nil?
                                               (conj []

                                                     (when (and not-view? agg? self? (> (count pivot-dim-query-fields) 0))
                                                       (partition-all 2 (for [f pivot-dim-query-fields] (keyword (str "pivot-on/" (ut/safe-name f))))))

                                                    ;;  (when (and not-view? agg? self? (> (count pivot-dim-query-fields) 0))
                                                    ;;    (partition-all 2 (for [f pivot-dim-query-fields] (keyword (str "🌶️pivot-on/" (ut/safe-name f))))))

                                                     (when (and not-view? not-sys? view-based?) [[]])

                                                     (when (and not-view? not-sys? self? (not table?)) [[:remove] [:order-by-asc :order-by-desc]])
                                                     (when (and not-view? not-sys? (not self?) agg? field? same-table?)
                                                       [[:sum :avg :min :max]
                                                        [:group-by]
                                                        [:order-by-asc :order-by-desc]])

                                                     (when (and not-view? not-sys? param? self? (not table?)) [[:remove] [:where= :where<>]
                                                                                                               [:highlight= :highlight> :highlight< :heatmap]])

                                                     (when (and not-view? not-sys? param? (not self?) (not table?)) [[:where= :where<>]
                                                                                                                     [:highlight= :highlight> :highlight< :heatmap]])

                                                     (when (and not-view? not-sys? multi-param? (not self?) (not table?)) [[:is-in :not-in]])

                                                     (when (and not-view? not-sys? table? common-fields? (not self?) same-db? (not union?))
                                                       (let [poss-joins (vec (take 10 (for [f common-fields] (keyword (str "join-on/" (ut/safe-name f))))))
                                                             poss-cnt   (count poss-joins)]
                                                          ;(ut/tapp>> [:poss poss-joins])
                                                         (if (> poss-cnt 3)
                                                           (partition-all 2 poss-joins)
                                                           (partition-all 1 poss-joins))))

                                                     (when (and view-drag? clicked? common-fields?) [[:click-filter]])

                                                     (when (and not-view? not-sys? dim? (not self?) field? same-table?)
                                                       [[:count-distinct] [:group-by] [:order-by-asc :order-by-desc]])

                                                     (when (and not-view? not-sys? union? (not self?) table? same-db?)
                                                       (if (<= w 5) [[:union] [:union-all]] [[:union :union-all]]))

                                                     (when (and not-view? not-sys? partial-union? (not self?)) ;table? same-db? ;; no need
                                                       [[:filter-shadow]])

                                                     (when (and not-view? not-sys? partial-union? (not self?)) ;table? same-db? ;; no need
                                                       [[:highlight-rows]])))))

        viz-options            [[:x :y :color] [:size :row :column] [:shapes :label]]
        action-typer           (try (keyword (get-in dbody [:drag-meta :data-type])) (catch :default _ :error!)) ;; (get-in dbody [:drag-meta :param-full]) == actual value (or fqdn param I suppose)
        drops                  @(ut/tracked-subscribe [::all-drops-of action-typer])
        ;;drops                  @(ut/tracked-sub ::all-drops-of {:ttype action-typer})
        ;;_ (ut/tapp>> [:spawner-drops action-typer panel-key drops])
        view-options           (remove nil? [[:text]
                                             (when (ut/ne? drops) drops)
                                             ;(when dim? [:dropdown])
                                             (when dim? [:table-filter])])

        color-map              (merge {:sum            "#9973e0"
                                       :avg            "#9973e0"
                                       :min            "#9973e0"
                                       :max            "#9973e0"
                                       :count          "#9973e0"
                                       :row-count      "#9973e0"
                                       :group-by       "#7be073"
                                       :order-by-asc   "#00674b" ;"#7be073"
                                       :order-by-desc  "#00674b" ;"#7be073"
                                       :remove         "#c7005d"
                                       :where=         "#c7005d"
                                       :where<>        "#c7005d"
                                       :filter-shadow  "#FFE5B4"
                                       :highlight-rows "#FFE5B4"
                                       :click-filter   "#FFE5B4"
                                       :is-in          "#9973e0"
                                       :not-in         "#9973e0"
                                       :count-distinct "#9973e0"
                                       :text           "#FFA500"
                                       :table-filter   "#00fff7"
                                       :x              "#FFA500" ;"#66FF00"
                                       :y              "#FFA500" ;"#66FF00"
                                       :color          "#008B8B"
                                       :size           "#00FF40"
                                       :row            "#8cc751"
                                       :column         "#8cc751" ;"#2E8B57"
                                       :shapes         "#00674b"
                                       :label          "#39FF14"
                                       :union          "#b7e27c"
                                       :union-all      "#b7e27c"
                                       :move-into      "#f9b9f9"
                                       :copy-into      "#f9b9f9"
                                       :reference      "#f9b9f9"
                                       :delete-query   "#ff3900"
                                       :promote-query  "#ffa500" ;;"orange"
                                       :delete-view    "#ff3900"
                                       :highlight=     "#7eeeee"
                                       :highlight>     "#7eeeee"
                                       :highlight<     "#7eeeee"
                                       :heatmap        "#7eeeee"
                                       :join           "#8c78b7"}
                                      (into {} (vec (for [f viz-for-each-fields]
                                                      {(keyword (str "weave/" (ut/safe-name f))) "#7FFFD4"})))
                                      (into {} (for [f pivot-dim-query-fields]
                                                 {(keyword (str "pivot-on/" (ut/safe-name f))) "#FFA500"}))
                                      (into {} (for [f pivot-dim-query-fields]
                                                 {(keyword (str "🌶️pivot-on/" (ut/safe-name f))) "#ff5c3a"}))
                                      (into {} (for [f common-fields]
                                                 {(keyword (str "join-on/" (ut/safe-name f))) "#8c78b7"})))
        block-ops              (into (if (>= w 10)
                                       (if view-drag? [[:move-into :copy-into :reference]] [[:move-into :copy-into :row-count]])
                                       (if view-drag? [[:move-into] [:copy-into] [:reference]] [[:move-into] [:copy-into] [:row-count]]))
                                     (when (ut/ne? viz-for-each-fields)
                                       (if (>= w 10)
                                         (vec (for [ff (partition-all 3 viz-for-each-fields)]
                                                (vec (for [f ff] (keyword (str "weave/" (ut/safe-name f)))))))
                                         (vec (for [f viz-for-each-fields]
                                                [(keyword (str "weave/" (ut/safe-name f)))])))))

        general-color          #(get color-map (first (first %)))
        has-view-options?      (ut/ne? (flatten view-options))
        has-viz-options?       (ut/ne? (flatten viz-options))
        has-sql-options?       (ut/ne? (flatten sql-options))
        big-enough-for-viz?    (and (>= w 6) (>= h 5))
        all-options            (cond

                                 is-layout? {:layout-map (vec (for [[k v] (get-in layout-map [0 :panels])] [k]))
                                             :view-opts  (if (and view-drag? self?)
                                                           [[:move-into] [:delete-view] [:copy-into]]
                                                           [[:move-into] [:copy-into]])}

                                 viz-reco? []

                                 (and                       ;(>= w 11) (>= h 6)
                                  view-based? not-sys? (not block-panel?))
                                 (merge
                                   ;{:new-viz1 viz-options
                                   ; :new-view view-options}
                                  (if (and not-view? not-sys? (not self?) agg? field?) {:new-query (if (<= w 9)
                                                                                                     [[:sum] [:avg] [:min] [:max]]
                                                                                                     [[:sum :avg] [:min :max]])} {})
                                  (if (and not-view? not-sys? (not self?) table?) {:new-query [[:row-count]]} {})
                                  (if (and (not table?) has-viz-options? big-enough-for-viz?) {:new-viz viz-options} {})
                                  (if (and (not table?) has-view-options?) {:new-view view-options} {})
                                  (if (and (not self?) block-panel?) {:block-ops block-ops} {}))

                                 (and not-sys? view-based? (not block-panel?))
                                 (merge
                                  (if (and (not self?) block-panel?) {:block-ops block-ops}
                                      (if big-enough-for-viz?
                                        {:new-viz viz-options}
                                        {}))
                                  (if (and self? block-panel?) {:block-ops [(if (= drag-type :query)
                                                                              (if is-child?
                                                                                [:delete-query :promote-query] [:delete-query])
                                                                              [:delete-view])]} {}))

                                 (and (>= w 11) (>= h 6) not-sys?)
                                 (merge
                                  (if has-sql-options? {selected-view sql-options} {})
                                  (if (and (not self?) block-panel?) {:block-ops block-ops}
                                      (if (and (not table?) has-viz-options? big-enough-for-viz?)
                                        {:new-viz viz-options} {}))
                                  (if (and self? block-panel?) {:block-ops [(if (= drag-type :query)
                                                                              (if is-child?
                                                                                [:delete-query :promote-query] [:delete-query])
                                                                              [:delete-view])]} {}))

                                 not-sys? (merge
                                           (if has-sql-options? {selected-view sql-options} {})
                                           (if (and (not self?) block-panel?) {:block-ops block-ops} {})
                                           (if (and self? block-panel?) {:block-ops [(if (= drag-type :query)
                                                                                       (if is-child?
                                                                                         [:delete-query :promote-query] [:delete-query])
                                                                                       [:delete-view])]} {})))
        option-count           (count (flatten (apply concat (vals all-options))))]

    ;; (ut/tapp>> [:dyn-spawn :dragged-obj view-drag?  source-panel-key :src-view source-table
    ;;        :clicked-fields clicked-fields :commons common-fields
    ;;       ;(vec (vals clicked-fields))
    ;;       ; :db drag-body
    ;;        ;:query-fields query-fields :trbl-fields (get drag-body :table-fields)
    ;;        ])



    ;(when partial-union? (ut/tapp>> [query-key panel-key :shadows? partial-union? not-view? not-sys? partial-union? (not self?)]))
    ;(ut/tapp>> [:drag-body drag-body])
    ;;(ut/tapp>> [:pivot-meta? query-key pivot-dim-query-fields @(ut/tracked-subscribe [::conn/sql-merged-metadata [query-key]])])

    ;(when is-layout?
    ;  (ut/tapp>> [:find-layout-map layout-map]))

    ;selected? "2px solid #9973e0"
    ;hover-q? "2px solid #c7005d"
    ;parent-of-selected? "2px solid #9973e0"
    ;upstream? "2px solid #7be073"
    ;(ut/tapp>> [:dyn-targets agg? (keys pivot-dim-query-fields)])
    ;(ut/tapp>> [:tt @(ut/tracked-subscribe [::conn/sql-metadata [query-key]])
    ;           @(ut/tracked-subscribe [::conn/sql-merged-metadata [query-key]])])
    ;(ut/tapp>> [{(str selected-view "1") sql-options}])
    ;(ut/tapp>> [:drag panel-key source-panel-key table-source table-source-k source-table drag-body panel-key])
    ;(ut/tapp>> [:targets-run-in panel-key :from table-source :w query-key :err? drag-body])

    ;;(let [reactivity-hack @dyn-dropper-hover] ;; moved down the chain for less expensive reactions


    (when (> option-count 0)
      (let [hov @dyn-dropper-hover]
        [re-com/h-box
         :attr {:on-drag-over  #(when (not @on-block?) (reset! on-block? true))
                :on-drag-leave #(reset! on-block? false)}
         :gap "5px"
         :size "auto"
         :width (if view-based?
                  (px (- width-int 10))
                  (px (+ 20 width-int)))
         :height (px (- height-int 50))
         :style {:position "fixed"
                 :font-family (theme-pull :theme/base-font nil)
                 :z-index  100}
         :children (for [[tname toptions] all-options]
                     [re-com/v-box
                      :gap "5px"
                      :size "auto"
                      ;:width (px (+ 20 width-int))
                      ;:height (px (- height-int 50))
                      ;:style {:position "fixed" :z-index 50}
                      :children
                      [;[re-com/box :child (str "you are: " source-table " I am: " table-source)]
                       ;[re-com/box :child (str drag-body) :style {:font-size "8px"}]
                       (when (and (not (= tname :layout-map)) (not (= tname :view-opts)))
                         [re-com/box :child (str tname)
                          :align :center :justify :center
                          :style {:background (str (general-color toptions)) :border-radius "10px"
                                  :font-family (theme-pull :theme/base-font nil)
                                  :color      "#000000" :font-weight 700 :font-size "15px"}])

                       [(if (= tname :view-opts) re-com/h-box re-com/v-box)
                        :gap "5px"
                        :style (if (= tname :view-opts) {:position "fixed" :bottom -22 :left 0} {})
                        :width (when (= tname :view-opts) (px (+ width-int 50)))
                        :size (if (= tname :view-opts) "none" "auto")
                        :children (for [opts toptions]

                                    (cond (= tname :layout-map) ;; fancy drop targets for layoutmap
                                          (let [pw-int           (/ width-int brick-size)
                                                ph-int           (/ height-int brick-size)
                                                processed-layout (lay/process-layout (get (first layout-map) :panels) pw-int ph-int)
                                                brix             (fn [x & [mod]] (px (if mod
                                                                                       (+ (* brick-size x) mod)
                                                                                       (* brick-size x))))
                                                boxes            (vec (for [[view-ref p] processed-layout]
                                                                        (let [h             (get p :h 4)
                                                                              w             (get p :w 4)
                                                                              ;z (get p :z 0)
                                                                              [x y] (get p :root [0 0])

                                                                              ;;  x (if (and (< x 1) (is-float? x))
                                                                              ;;      (js/Math.floor (* x pw-int)) x)
                                                                              ;;  y (if (and (< y 1) (is-float? y))
                                                                              ;;      (js/Math.floor (* y ph-int)) y)
                                                                              ;;  h (if (and (< h 1) (is-float? h))
                                                                              ;;      (js/Math.floor (* h ph-int)) h)
                                                                              ;;  w (if (and (< w 1) (is-float? w))
                                                                              ;;      (js/Math.floor (* w pw-int)) w)

                                                                              ;;  h (if (= h 0) (- ph-int y 1) h) ;; stretch val
                                                                              ;;  w (if (= w 0) (- pw-int x) w) ;; stretch val

                                                                              pstyle        (get p :style {})
                                                                              left          (+ -1 (* x brick-size))
                                                                              top           (+ 25 (* y brick-size))
                                                                              root-visible? (not (or (> x (- pw-int 1)) (> y (- ph-int 2))))
                                                                              box-map       {:child  (str view-ref)
                                                                                             :size   "none"
                                                                                             :width  (brix w)
                                                                                             :height (brix h)
                                                                                             :align  :center :justify :center
                                                                                             :attr   {;:on-drag-enter #(reset! dyn-dropper-hover [panel-key o])
                                                                                                      :on-drag-over  #(when (not (= @dyn-dropper-hover [panel-key view-ref]))
                                                                                                                        (reset! dyn-dropper-hover [panel-key view-ref]))
                                                                                                      :on-click      #(reset! dragging? false)
                                                                                                      :on-drag-leave #(reset! dyn-dropper-hover nil)}
                                                                                             :style  (merge {;:border "2px dashed yellow"
                                                                                                             :border           (str "2px " (if (= tname :new-viz22) "dashed" "solid") " " (get color-map view-ref)
                                                                                                                                    (if (= tname :new-viz22) "77" ""))
                                                                                                             :font-family (theme-pull :theme/base-font nil)
                                                                                                             :position         "fixed" ; "relative"
                                                                                                             :overflow         "hidden"
                                                                                                             :color            (if (= [panel-key view-ref] hov) "red" "yellow")
                                                                                                             :background-color (if (= [panel-key view-ref] hov) (str (get color-map view-ref) 55) "#00000011")
                                                                                                             :filter           (str "drop-shadow(0.35rem 0.35rem 0.4rem " (get color-map view-ref) 55 " )")
                                                                                                             ;:z-index (+ p-z z)
                                                                                                             :left             left
                                                                                                             :top              top} pstyle)}
                                                                              ;leftovers (-> p (dissoc :style) (dissoc :h) (dissoc :w) (dissoc :root))
                                                                              leftovers     (dissoc p :style :h :w :root :k :z :view-ref :hidden? :id)]
                                                                          ;(ut/tapp>> [:test x (is-float? x)])
                                                                          ;(ut/tapp>> [:layoutz view-ref leftovers layout-map])
                                                                          (when root-visible?
                                                                            (droppable-pills
                                                                             ["meta-menu"] {:clause       (if (= tname :layout-map) {:layout-drop view-ref :kp (last layout-map)} view-ref)
                                                                                            :drag-body    drag-body
                                                                                            :target-panel panel-key
                                                                                            :target-query query-key}
                                                                             (ut/mapply re-com/box (merge leftovers box-map)))))))]

                                            ;(ut/tapp>> [:boxes? opts layout-map (get (first layout-map) :panels) boxes])

                                            [re-com/h-box
                                             :size "none"
                                             :width (brix pw-int -10)
                                             :height (brix ph-int -47) ;; footer, will be dynamic TODO
                                             :style {;:border "1px solid lime" ;; debug edges
                                                     ;:position "absolute"
                                                     :overflow "hidden"
                                                     :font-family (theme-pull :theme/base-font nil)}
                                                     ;; :background-size (when scrub-borders?
                                                     ;;                    "50px 50px")
                                                     ;; :background-image (when scrub-borders?
                                                     ;;                     "linear-gradient(0deg,   transparent 48px, #ffffff27 48px, #ffffff27 50px, transparent 50px),
                                                     ;;                         linear-gradient(90deg, transparent 48px, #ffffff27 48px, #ffffff27 50px, transparent 50px)")

                                             :children boxes])

                                            ;; [re-com/h-box
                                            ;;  :gap "5px"
                                            ;;  :size "auto"
                                            ;;  :children (for [o opts]
                                            ;;              [re-com/box
                                            ;;               :child  (droppable-pills
                                            ;;                        ["meta-menu"] {:clause (if (= tname :layout-map) {:layout-drop o :kp (last layout-map)} o)
                                            ;;                                       :drag-body drag-body
                                            ;;                                       :target-panel panel-key
                                            ;;                                       :target-query query-key}
                                            ;;                        [re-com/box :size "auto"
                                            ;;                         :attr {;:on-drag-enter #(reset! dyn-dropper-hover [panel-key o])
                                            ;;                                :on-drag-over #(reset! dyn-dropper-hover [panel-key o])
                                            ;;                                :on-click #(reset! dragging? false)
                                            ;;                                :on-drag-leave #(reset! dyn-dropper-hover nil)}
                                            ;;                         :child (str o "!")])
                                            ;;               :align :center
                                            ;;               :justify :center
                                            ;;               :style {:border (str "2px " (if (= tname :new-viz22) "dashed" "solid")  " " (get color-map o)
                                            ;;                                    (if (= tname :new-viz22) "77" ""))
                                            ;;                                                         ;:font-size font-size
                                            ;;                       :font-weight 700
                                            ;;                       :font-size (font-size-px (str o " ") (/ h (+ 1 (count toptions))) (/ w 3))
                                            ;;                       :filter (str "drop-shadow(0.35rem 0.35rem 0.4rem " (get color-map o) 55 " )")
                                            ;;                       :color (if (= [panel-key o] hov) "#000000" (get color-map o))
                                            ;;                       :background-color (if (= [panel-key o] hov) (str (get color-map o) 55) "#00000011")
                                            ;;                       :font-family "Poppins"}
                                            ;;               :size "auto"])]

                                          (= tname :view-opts)
                                          (do (ut/tapp>> [:view-opts-footer opts toptions])
                                              [re-com/h-box
                                               :gap "5px"
                                               :style {;:padding-left "0px"
                                                       :margin-left "-1px"
                                                       :overlay-filter "blur(4px)"}
                                               ;:width (px (- width-int 10))
                                               :justify :between
                                               :size "none"
                                               :children (for [o opts]
                                                           [re-com/box
                                                            ;:width (px (/ (- width-int 60) 2))
                                                            :child (droppable-pills
                                                                    ["meta-menu"] {:clause       (if (= tname :layout-map) {:layout-drop o :kp (last layout-map)} o)
                                                                                   :drag-body    drag-body
                                                                                   :target-panel panel-key
                                                                                   :target-query query-key}
                                                                    [re-com/box :size "none"
                                                                     :width (px (/ (- width-int 13) (count toptions)))
                                                                     :align :center
                                                                     :justify :center
                                                                     :attr {;:on-drag-enter #(reset! dyn-dropper-hover [panel-key o])
                                                                            :on-drag-over  #(when (not (= @dyn-dropper-hover [panel-key o]))
                                                                                              (reset! dyn-dropper-hover [panel-key o]))
                                                                            :on-click      #(reset! dragging? false)
                                                                            :on-drag-leave #(reset! dyn-dropper-hover nil)}
                                                                     :child (str o)])
                                                            :align :center
                                                            :justify :center
                                                            :style {:border           (str "2px " (if (= tname :new-viz22) "dashed" "solid") " " (get color-map o)
                                                                                           (if (= tname :new-viz22) "77" ""))
                                                                    :font-weight      700
                                                                    :font-size        (font-size-px (str o " ") (/ h (+ 1 (count toptions))) (/ w 3))
                                                                    :filter           (str "drop-shadow(0.35rem 0.35rem 0.4rem " (get color-map o) 55 " )")
                                                                    :color            (if (= [panel-key o] hov) "#000000" (get color-map o))
                                                                    :background-color (if (= [panel-key o] hov) (str (get color-map o) 55) "#00000055")
                                                                    :font-family (theme-pull :theme/base-font nil)}
                                                            :size "auto"])])

                                          :else [re-com/h-box
                                                 :gap "5px"
                                                 :size "auto"
                                                 :children (for [o opts]
                                                             [re-com/box
                                                              :child (droppable-pills
                                                                      ["meta-menu"] {:clause       (if (= tname :layout-map) {:layout-drop o :kp (last layout-map)} o)
                                                                                     :drag-body    drag-body
                                                                                     :target-panel panel-key
                                                                                     :target-query query-key}
                                                                      [re-com/box :size "auto"
                                                                       :attr {;:on-drag-enter #(reset! dyn-dropper-hover [panel-key o])
                                                                              :on-drag-over  #(when (not (= @dyn-dropper-hover [panel-key o]))
                                                                                                (reset! dyn-dropper-hover [panel-key o]))
                                                                              :on-click      #(reset! dragging? false)
                                                                              :on-drag-leave #(reset! dyn-dropper-hover nil)}
                                                                       :child (str o)])
                                                              :align :center
                                                              :justify :center
                                                              :style {:border           (str "2px " (if (= tname :new-viz22) "dashed" "solid") " " (get color-map o)
                                                                                             (if (= tname :new-viz22) "77" ""))
                                                                      ;:font-size font-size
                                                                      :font-weight      700
                                                                      :font-size        (font-size-px (str o " ") (/ h (+ 1 (count toptions))) (/ w 3))
                                                                      :filter           (str "drop-shadow(0.35rem 0.35rem 0.4rem " (get color-map o) 55 " )")
                                                                      :color            (if (= [panel-key o] hov) "#000000" (get color-map o))
                                                                      :background-color (if (= [panel-key o] hov) (str (get color-map o) 55) "#00000011")
                                                                      :font-family (theme-pull :theme/base-font nil)}
                                                              :size "auto"])]))]]])]))))

(re-frame/reg-sub                                           ;; sub is cached so faster than doing this expensive run each time? will sync properly?
 ::dynamic-drop-targets                                    ;; @(ut/tracked-subscribe [::dynamic-drop-targets table-source dbody panel-key query-key width-int height-int])
 (fn [_ [_ table-source dbody panel-key query-key width-int height-int]]
   [dynamic-spawner-targets table-source dbody panel-key query-key width-int height-int]))

;; (defn sql-alias-replace2 [query]
;;   (let [;query (dissoc query :style-rules)
;;         valid-params (vec (remove nil? (for [k (ut/deep-flatten query)]
;;                                          (when (and (keyword? k) (cstr/includes? (str k) "/")) k))))
;;         sql-params   (into {} (for [k valid-params]         ;; deref here?
;;                                 (if (cstr/includes? (str k) ":query/")
;;                                   {k (dissoc @(ut/tracked-subscribe [::resolve-sql-alias (keyword (ut/replacer (str k) ":query/" ""))]) :style-rules)}
;;                                   {k @(ut/tracked-subscribe [::conn/clicked-parameter-key [k]])})))
;;         replaced     (->> query
;;                           ;(ut/postwalk-replacer condi-walks)
;;                           ;(ut/postwalk-replacer value-walks)
;;                           (ut/postwalk-replacer sql-params))]
;;     (if                                                     ;(cstr/includes? (str replaced) "/")
;;       (cstr/includes? (str replaced) ":query/")
;;       (sql-alias-replace-sub replaced)
;;       replaced)
;;     ))

(re-frame/reg-sub
 ::get-sql-server-string
 (fn [db [_ k]]
   (let [s (get-in db [:sql-str k])]
     (try (let [s (ut/replacer s #"\\n" "\n")]
            (subs s 1 (- (count s) 1)))
          (catch :default _ s)))))

;; (defn materialize-all-sql [panel-id]
;;   (let [block-map @(ut/tracked-subscribe [::workspace [panel-id]])
;;         sql-calls (get block-map :queries)
;;         ; sql-params (into {} (for [k (ut/deep-flatten sql-calls)]
;;         ;                       (cond
;;         ;                         (cstr/includes? (str k) ":query/")
;;         ;                         {k @(ut/tracked-subscribe [::resolve-sql-alias (keyword (ut/replacer (str k) ":query/" ""))])}
;;         ;                         (cstr/includes? (str k) "/")
;;         ;                         {k @(ut/tracked-subscribe [::conn/clicked-parameter-key [k]])}
;;         ;                         :else {})))
;;         ]
;;     (into {} (doall (for [[k v] sql-calls]
;;                       (let [;query (ut/postwalk-replacer sql-params v)
;;                             query     (-> (sql-alias-replace-sub v)
;;                                           (dissoc :page)
;;                                           (dissoc :col-widths)
;;                                           (dissoc :row-height)
;;                                           (dissoc :render-all?)
;;                                           (dissoc :cache?)
;;                                           (dissoc :refresh-every)
;;                                           (dissoc :deep-meta?)
;;                                           (dissoc :clicked-row-height)
;;                                           (dissoc :style-rules))
;;                             str-query (-> query (honey/format {:pretty true :inline true}))]
;;                         {k str-query}))))))

(defn materialize-one-sql [panel-id sql-key]
  (let [block-map @(ut/tracked-subscribe [::workspace [panel-id]])
        sql-calls {sql-key (get-in block-map [:queries sql-key])}]
        ; sql-params (into {} (for [k (ut/deep-flatten sql-calls)]
        ;                       (cond
        ;                         (cstr/includes? (str k) ":query/")
        ;                         {k @(ut/tracked-subscribe [::resolve-sql-alias (keyword (ut/replacer (str k) ":query/" ""))])}
        ;                         (cstr/includes? (str k) "/")
        ;                         {k @(ut/tracked-subscribe [::conn/clicked-parameter-key [k]])}
        ;                         :else {})))

    (first (for [[k v] sql-calls]
             (let [query     (-> v
                                 ut/clean-sql-from-ui-keys
                                ;;  (dissoc :page)
                                ;;  (dissoc :col-widths)
                                ;;  (dissoc :row-height)
                                ;;  (dissoc :render-all?)
                                ;;  (dissoc :cache?)
                                ;;  (dissoc :refresh-every)
                                ;;  (dissoc :deep-meta?)
                                ;;  (dissoc :clicked-row-height)
                                ;;  (dissoc :style-rules)
                                 sql-alias-replace-sub)
                   ;query (ut/clean-sql-from-ui-keys v)
                   data-key  (get-in query (first (filter #(= (last %) :data) (ut/kvpaths query))))
                   query     (if (not (empty? data-key)) (ut/postwalk-replacer {{:data data-key}
                                                                                 {:select [:*] :from [(str k "/data-entry")]}} query) query)
                  ; str-query (try (-> query
                  ;                    (honey/format {:pretty true :inline true}))
                  ;                (catch js/Error e [(str "\n -- honey-sql error - cannot parse: \n /* " e " */ ")]))
                   str-query [@(ut/tracked-subscribe [::get-sql-server-string k])]]
               ;(ut/tapp>> [:sql-parse query data-key k v])
               (str "-- " k "\n" (first str-query)))))))

(defn formula-code-box [panel-id key width-int height-int type]
  (let [;sql-hint? (cstr/includes? (str value) ":::sql-string")
        honey-sql  @(ut/tracked-subscribe [::query panel-id key])
        sql-string (materialize-one-sql panel-id key)]
    ;(ut/tapp>> (ut/splitter (str sql-string) #"\n"))
    [re-com/box
     :size "auto"
     :width (px (- width-int 24))
     :height (px (- height-int 24))
     :style {:font-family (theme-pull :theme/monospaced-font nil) ; "Fira Code" ; "Chivo Mono" ;"Ubuntu" ;"Fira Code"
             :font-size   "12px"
             ;:background-color "#00000044"
             :overflow    "auto"
             :font-weight 700}
     :child [(reagent/adapt-react-class cm/UnControlled)
             {:value   (if (= type :honey)
                         (ut/format-map (- width-int -40)
                                        (str honey-sql))
                         (ut/splitter (str sql-string) #"\n"))
                         ;["line 1" "line 2"]

              ;:value (str (if (nil? key) value
              ;                (get value key)))
              ;:onBlur #(if (nil? key)
              ;           (ut/tracked-dispatch-sync [::update-selected (read-string (cstr/join " " (ut/cm-deep-values %)))])
              ;           (ut/tracked-dispatch-sync [::update-selected-key key (read-string (cstr/join " " (ut/cm-deep-values %)))]))
              :options {:mode              (if (= type :string) "sql" "clojure")
                        :lineWrapping      true
                        :lineNumbers       false
                        :matchBrackets     true
                        :autoCloseBrackets true
                        :autofocus         false
                        :autoScroll        false
                        :detach            true
                        :readOnly          false            ;true
                        :theme             (theme-pull :theme/codemirror-theme nil)}}]])) ; "ayu-mirage-formula" ;"hopscotch"

(defonce formula-bar (reagent/atom nil))

(defonce value-hover (reagent/atom nil))

(re-frame/reg-sub
 ::stylers
 (fn [db [_ panel-key query-key]]
   (into {}
         (for [[[cols name] logic-map]
               ;(get-in db [:panels panel-key :queries query-key :style-rules])
               (get-in db [:sql-source query-key :style-rules])] ;; moved to fit non-panel queries also

           {name (merge {:cols cols} logic-map)}))))

(defn get-color-temp [weight scheme depth]
  (let [val1       (/ weight 100)
        hue        (* (- 1 val1) 120)
        temp       (str "hsl(" hue ",100%,50%)")
        scheme     (get-in ut/colorbrewer [scheme depth] [])
        ;schene (if (nil? scheme) )
        num-scheme (count scheme)
        bins       (/ 100 num-scheme)
        wbin       (js/Math.ceil (/ weight bins))
        scale      (get scheme wbin)]
    ;(ut/tapp>> [:color-val weight num-scheme bins wbin])
    ;(str "hsl(" hue ",100%,50%)")
    ;temp
    (if (= 0 num-scheme) temp scale)))

(re-frame/reg-sub
 ::heatmap-color-val
 (fn [db [_ panel-key query-key field val styler-keys scheme depth direction]]
   (let [;other-vals (vec (distinct (for [r (get-in db [:data query-key])] (get r field))))
         styler-field (keyword (ut/replacer (str "styler_" (ut/safe-name styler-keys)) #"-" "_"))
         local-vals   (remove nil? (for [r (get-in db [:data query-key])]
                                     (when (= (get r styler-field) 1)
                                       (get r field))))
         other-vals   (cond
                        (= direction :asc) (vec (sort local-vals))
                        (= direction :desc) (vec (reverse (sort local-vals)))
                        :else (vec (sort local-vals)))
         vals-cnt     (count other-vals)
         idx          (.indexOf other-vals val)
         weight       (Math/round (* (/ idx vals-cnt) 100))
         wmix         (get-color-temp weight scheme depth)]
      ;(ut/tapp>> [:heatmap-color-val panel-key query-key field val (keyword (str "styler_" (ut/safe-name styler-keys)))])
     wmix)))

(re-frame/reg-sub
 ::heatmap-color
 (fn [db [_ panel-key query-key field]]
   (into {}
         (for [[[cols name] logic-map]
               (get-in db [:panels panel-key :queries query-key :style-rules])]
           {name (merge {:cols cols} logic-map)}))))

;(defonce column-selected (reagent/atom {}))

(re-frame/reg-sub
 ::column-selected?
 (fn [db [_ panel-key query-key field]]
    ;(true? (= (get-in db [:selected-cols panel-key query-key ]) field))
   (true?
    (when (not (nil? (get db :selected-cols)))
      (let [c             (get db :selected-cols)
            sel-panel-key (safe-nth c 0)
            sel-query-key (safe-nth c 1)
            sel-field     (safe-nth c 2)]
        (and (= panel-key sel-panel-key)
             (= sel-query-key query-key)
             (= sel-field field)))))))

(re-frame/reg-sub
 ::column-selected-any-field?
 (fn [db [_ panel-key query-key]]
    ;(true? (= (get-in db [:selected-cols panel-key query-key ]) field))
   (true?
    (when (not (nil? (get db :selected-cols)))
      (let [c             (get db :selected-cols)
            sel-panel-key (safe-nth c 0)
            sel-query-key (safe-nth c 1)]
              ;sel-field (safe-nth c 2)

        (and (= panel-key sel-panel-key)
             (= sel-query-key query-key)))))))
               ;;(= sel-field field)


(re-frame/reg-sub
 ::column-selected
 (fn [db [_ panel-key query-key]]
   (when (not (nil? (get db :selected-cols)))
     (let [c             (get db :selected-cols)
           sel-panel-key (safe-nth c 0)
           sel-query-key (safe-nth c 1)
           sel-field     (safe-nth c 2)]
       (when (and (= panel-key sel-panel-key)
                  (= sel-query-key query-key))
         sel-field)))))

(re-frame/reg-event-db
 ::column-select
 (undoable)
 (fn [db [_ panel-key query-key field]]
   (if (not (nil? field))
     (assoc db :selected-cols [panel-key query-key field])
     (-> db
         (assoc :selected-cols nil)
         (assoc :col-names nil)))))
    ; (assoc db :selected-cols (when (not (nil? field))
    ;                            [panel-key query-key field]))

(re-frame/reg-event-db
 ::cycle-column-select
 (undoable)
 (fn [db [_ forward?]]
   (if (not (empty? (get db :selected-cols)))

     (let [selected-panel (get db :selected-block)
           selected-tab   (get-in db [:panels selected-panel :selected-view])
           qkeys          (keys (get-in db [:panels selected-panel :queries]))
           query-key      (cond (and (nil? selected-tab) (not (empty? qkeys))) (first qkeys)
                                (and (not (nil? selected-tab)) (some #(= % selected-tab) qkeys)) selected-tab ;(first qkeys)
                                :else nil)
           curr-field     (get-in db [:selected-cols 2])
           fields         (keys (get-in db [:meta query-key :fields]))
           field-idx      (.indexOf fields curr-field)
           new-field      (nth (cycle fields)
                               (if forward? (+ field-idx 1) (- field-idx 1)))]
       (when (not (nil? query-key))
         (assoc db :selected-cols [selected-panel query-key new-field])))

     (let [curr-field (get db :selected-tab)
           ;fields    (get db :tabs)
           fields     @(ut/tracked-subscribe [::visible-tabs]) ;; sneaky sneaky
           field-idx  (.indexOf fields curr-field)
           new-field  (nth (cycle fields)
                           (if forward? (+ field-idx 1) (- field-idx 1)))]
       ;(assoc db :selected-tab new-field)
       (-> db
           (assoc :selected-tab new-field)
           (assoc-in [:click-param :sys :selected-tab] new-field)
           (assoc-in [:click-param :sys :selected-tab-idx]
                     (try (.indexOf (get db :tabs) new-field) (catch :default _ -1))))))))

(re-frame/reg-sub
 ::custom-col-widths
 (fn [db [_ panel-key query-key]]
   (get-in db [:panels panel-key :queries query-key :col-widths]
           (get-in db [:sql-source query-key :col-widths] {})) ;; custom queries that have no home in the panel...
   ))

(re-frame/reg-sub
 ::custom-rows
 (fn [db [_ panel-key query-key]]
   (select-keys (get-in db [:panels panel-key :queries query-key] {}) [:row-height :render-all? :clicked-row-height])))

(re-frame/reg-event-db
 ::set-column-default-widths
 (fn [db [_ panel-key query-key w]]
   (assoc-in db [:default-col-widths panel-key query-key] w)))

(re-frame/reg-sub
 ::column-default-widths
 (fn [db {:keys [panel-key query-key]}]
   (get-in db [:default-col-widths panel-key query-key])))

(declare honeycomb)                                         ;; eyes emoji - would be better to restructure, eh?

(defn insert-hidden-reco-preview [reco-selected reco-viz reco-query reco-condis combo-name shape-name single?]
  (let []
    ;(ut/tapp>> [:insert-hidden-reco-preview reco-selected reco-viz reco-query reco-condis single?])
    (dorun
     (when single? (ut/tracked-dispatch [::new-reco-preview reco-selected]))

     (cond (= (first (read-string reco-viz)) :vega-lite)

           (let [incomingv     (read-string reco-viz)      ;; read-string will ruin my internal namespaced keywords
                 ffromv        (-> (get-in incomingv [1 :data :values])
                                   (ut/replacer "_" "-")
                                   (ut/replacer "query/" ""))
                 original-conn @(ut/tracked-subscribe [::lookup-connection-id-by-query-key (keyword ffromv)])
                 view          (-> incomingv
                                   (assoc-in [1 :data :values] :query-preview)
                                   (assoc-in [1 :config] :theme/vega-defaults)
                                   (assoc-in [1 :width] "container") ;:panel-width)
                                   (assoc-in [1 :height] :panel-height)
                                   (assoc-in [1 :padding] 4)
                                   (assoc-in [1 :background] "transparent"))

                 q-data        (read-string reco-query)
                 incoming      (first q-data)              ;; read-string will ruin my internal namespaced keywords
                 ffrom         (ut/replacer (first (get incoming :from)) "_" "-")
                 query         (vec (for [q q-data] (assoc q :from [(keyword ffrom)])))
                 query         (ut/postwalk-replacer {[[:sum :rows]] [:count 1]} query)]

             (insert-rec-preview-block
              view
              query                                       ;reco-h reco-w
              reco-condis
              original-conn                               ;reco-conn
              reco-selected combo-name shape-name single? false))

           :else (let [view          (read-string reco-viz) ;; read-string will ruin my internal namespaced keywords
                       q-data        (read-string reco-query)
                        ;q-cnt (count q-data)
                       incoming      (first q-data)        ;; read-string will ruin my internal namespaced keywords
                       ffrom         (ut/replacer (first (get incoming :from)) "_" "-")
                       original-conn @(ut/tracked-subscribe [::lookup-connection-id-by-query-key (keyword (last (ut/splitter ffrom "/")))])
                        ;query (assoc incoming :from [(keyword ffrom)])
                       query         (vec (for [q q-data]
                                            (if (nil? (find q :vselect))
                                              (assoc q :from [(keyword ffrom)])
                                              q)))
                                               ;(assoc q :from (keyword ffrom))

                       query         (ut/postwalk-replacer {[:sum :rows] [:count 1]} query)]

                    ;(ut/tapp>> [:rec-preview-block 2 view query reco-condis original-conn reco-selected])
                   (insert-rec-preview-block
                    view
                    query                                 ; reco-h reco-w
                    reco-condis
                    original-conn                         ;reco-conn
                    reco-selected combo-name shape-name single? false))))))

(defn clear-preview2-recos []
  (dorun (let [prev-preview-keys (for [k @(ut/tracked-subscribe [::preview-keys2])] (keyword (str "reco-preview" k)))]
               ;per-page per-page ;; if we ever get more room...
               ;grid-page @(ut/tracked-subscribe [::recos-page2])
               ;grid-page (if (> (- grid-page 1) pages) 0 grid-page)
               ;recos (take per-page (drop (* grid-page per-page) @(re-frame.core/subscribe [::conn/sql-data [:recos-sys2]])))
               ;panel-keys @(ut/tracked-subscribe [::panel-keys])
               ;recos @(re-frame.core/subscribe [::conn/sql-data [:recos-sys2]])
               ;recos-keys (vec (for [{:keys [combo_hash]} recos] combo_hash))

           ;(ut/tapp>> [:clear-mad-libs-recos prev-preview-keys])
           (doseq [k prev-preview-keys] (ut/tracked-dispatch [::quick-delete-panel k]))
           (ut/tracked-dispatch [::set-preview-keys2 []])
           (ut/tracked-dispatch [::clear-query :recos-sys2]))))
           ;(doseq [{:keys [combo_hash query_map viz_map condis combo_edn shape_name]} recos]
           ;  (insert-hidden-reco-preview combo_hash viz_map query_map condis combo_edn shape_name false))


(defn mad-libs-shapes [query-id width-int height-int]
  (let [;all-sql-call-keys @(ut/tracked-subscribe [::all-sql-call-keys])
        ;grid-reco? @(ut/tracked-subscribe [::grid-reco?])
        parent-panel-key  @(ut/tracked-subscribe [::lookup-panel-key-by-query-key query-id])
        [h w] @(ut/tracked-subscribe [::size parent-panel-key])
        height-int        (- height-int 42)
        width-int         (- width-int 10)
        recos-page        @(ut/tracked-subscribe [::recos-page2])
        query-id          (ut/replacer (ut/replacer (str query-id) #":" "") "-" "_")
        ;all-sql-call-keys-str [query-id] ;(for [e all-sql-call-keys] (ut/replacer (ut/safe-name e) "-" "_"))
        sql-params        (merge (into {} (for [k [:viz-shapes0-sys2/shape :viz-shapes-sys2/combo_edn :user-dropdown-sys2/req-field]]
                                            {k 
                                             ;@(ut/tracked-subscribe [::conn/clicked-parameter-key [k]])
                                             @(ut/tracked-sub ::conn/clicked-parameter-key-alpha {:keypath [k]})
                                             }))
                                 {:viz-tables-sys2/table_name query-id})
        sql-params-minus  (dissoc sql-params :viz-tables-sys2/table_name) ;; (ut/tracked-dispatch [::conn/click-parameter [:viz-tables-sys2 :table_name] nil])
        ;sql-params {:viz-tables-sys2/table_name query-id}
        combo-picked?     (not (nil? (get sql-params :viz-shapes-sys2/combo_edn)))
        shape-picked?     (not (nil? (get sql-params :viz-shapes0-sys2/shape)))
        ;; clear-filters-fn  (fn [] (do (ut/tracked-dispatch [::conn/click-parameter [:viz-shapes0-sys2 :shape] nil])
        ;;                              (ut/tracked-dispatch [::conn/click-parameter [:user-dropdown-sys2 :req-field] nil])
        ;;                              (ut/tracked-dispatch [::conn/click-parameter [:viz-shapes-sys2 :combo_edn] nil])))
        req-field-picked? (not (nil? (get sql-params :user-dropdown-sys/req-field)))
        ;query-id-sql (ut/replacer query-id)
        sql-calls         {:viz-tables-sys2  {:select   [:table_name [[:count 1] :recos]]
                                              :from     [:viz_recos_vw]
                                              :where    [:and [:not [:like :table_name "query_preview%"]]
                                                         [:= :table_name query-id]] ;; changed to single from current?
                                              :order-by [:table_name]
                                              :group-by [:table_name]}
                           :viz-shapes0-sys2 {:select   [[:shape_name :shape] [[:count 1] :recos]]
                                              :from     [[:viz_recos_vw :vvt]]
                                              :where    [:and [:= :table_name :viz-tables-sys2/table_name]]
                                                         ;(when req-field-picked?
                                                         ;  [:like :combo_edn (str "%" (get sql-params :user-dropdown-sys/req-field) "%")])
                                                         ;(when combo-picked?
                                                         ;  [:= :combo_edn :viz-shapes-sys/combo_edn])
  ;; :viz-tables-sys/table_name-name
                                              :group-by [:shape_name]}
                           :viz-shapes-sys2  {:select   [:combo_edn [[:count 1] :recos]]
                                              :from     [[:viz_recos_vw :vvt]]
                                              ;:order-by [[:score :desc]]
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
                                                :where    [:and
                                                           [:= :table_name query-id]
                                                           (when combo-picked?
                                                             [:= :combo_edn :viz-shapes-sys2/combo_edn])
                                                           ;(when req-field-picked?
                                                           ;  [:like :combo_edn (str "%" (get sql-params :user-dropdown-sys/req-field) "%")])
                                                           (when shape-picked?
                                                             [:= :shape_name :viz-shapes0-sys2/shape])]}
                                               {:select   [:*]
                                                :from     (if (or combo-picked? shape-picked?) [:viz_recos_vw] [:viz_recos_vw2])
                                                :order-by [[:score :desc]]
                                                :where    [:and
                                                           [:= :table_name query-id]
                                                           (when combo-picked?
                                                             [:= :combo_edn :viz-shapes-sys2/combo_edn])
                                                           ;(when req-field-picked?
                                                           ;  [:like :combo_edn (str "%" (get sql-params :user-dropdown-sys/req-field) "%")])
                                                           (when shape-picked?
                                                             [:= :shape_name :viz-shapes0-sys2/shape])]})}
        ;block-list @(re-frame.core/subscribe [::conn/sql-data [:viz-tables-sys]])
        ;combo-list @(re-frame.core/subscribe [::conn/sql-data [:viz-shapes-sys]])
        ;; full-recos        @(re-frame.core/subscribe [::conn/sql-data [:recos-sys2]])
        full-recos        @(rfa/sub ::conn/sql-data-alpha {:keypath [:recos-sys2]})
        ;current-tab @(ut/tracked-subscribe [::selected-tab])
        ;current-tab-queries  (try (map #(-> % ut/sql-keyword name) @(ut/tracked-subscribe [::current-tab-queries])) (catch :default _ []))
        recos-count       (count full-recos)
        pkeys             @(ut/tracked-subscribe [::preview-keys2])
        preview-keys      (vec (for [k pkeys] (keyword (str "reco-preview" k))))
        preview-maps      (into {} (for [{:keys [combo_hash shape_name combo_edn query_map score w h selected_view viz_map condis]} full-recos]
                                         ;:when (not (= combo_hash "1622273823"))

                                     {(keyword (str "reco-preview" combo_hash))
                                      {:shape_name shape_name
                                       :query_map  query_map
                                       :combo_hash combo_hash
                                       :selected_view selected_view
                                       :w w :h h
                                       :score      score
                                       :viz_map    viz_map
                                       :condis     condis
                                       :combo_edn  combo_edn}}))

        ;reco-selected (keyword (str "reco-preview" @(ut/tracked-subscribe [::conn/clicked-parameter-key [:recos-sys2/combo_hash]])))
        ;; 9x9    2 x 2
        ;; 11x29  3 x 2
        ;charts-wide 3
        ;charts-high 2
        charts-wide       (js/Math.floor (/ w 7))
        charts-high       (js/Math.floor (/ h 4.5))
        h-multi           2.2                               ;; offsets for css zoom math fuckery
        w-multi           2.3
        preview-container (fn [preview-maps pk pnum]
                            (try (let [panel-key        (nth pk pnum)
                                       ce               (get-in preview-maps [panel-key :combo_edn])
                                       ;query_map (get-in preview-maps [panel-key :query_map])
                                       ;viz_map (get-in preview-maps [panel-key :viz_map])
                                       color-keys       [:YlGn :desc]
                                       color-keys-which (last (reverse (sort-by name (keys (get ut/colorbrewer (first color-keys))))))
                                       colors1          (vec (if (= (last color-keys) :desc)
                                                               (reverse
                                                                (get-in ut/colorbrewer [(first color-keys) color-keys-which]))
                                                               (get-in ut/colorbrewer [(first color-keys) color-keys-which])))
                                       zero-color       (if (= (last color-keys) :desc) (first colors1) (last colors1))
                                       value-to-color   (fn [value-vector color-vector value]
                                                          (let [min-val    (apply min value-vector)
                                                                max-val    (apply max value-vector)
                                                                normalized (when (> (- max-val min-val) 0) ; Avoid division by zero
                                                                             (/ (- value min-val) (- max-val min-val)))
                                                                idx        (int (Math/floor (* normalized (dec (count color-vector)))))]
                                                            (if (and normalized (>= idx 0) (< idx (count color-vector)))
                                                              (color-vector idx)
                                                              nil)))
                                       selected-view (try (edn/read-string (get-in preview-maps [panel-key :selected_view])) (catch :default _ nil))
                                       ;;_ (ut/tapp>> [selected-view pk pnum])
                                       ;condis (get-in preview-maps [panel-key :condis])
                                       ;combo_hash (get-in preview-maps [panel-key :combo_hash])
                                       ;query (first @(ut/tracked-subscribe [::panel-sql-call-keys panel-key]))
                                       ;running? (some #(= % query)
                                       ;      (for [e @(ut/tracked-subscribe [::wfx/pending-requests http/socket-id])]
                                       ;        (let [p (get-in e [:message :ui-keypath])]
                                       ;          (when (= (count p) 1) (first p)))))
                                       rounder          (fn [num] (/ (js/Math.round (* num 100)) 100))
                                       combo_edn        (try (when (not (nil? ce)) (ut/replacer (str ce) #"\"" "")) (catch :default _ "")) ;; TODO, why bombing?
                                       shape_name       (get-in preview-maps [panel-key :shape_name])
                                       score            (get-in preview-maps [panel-key :score]) ;(js/Math.round (get-in preview-maps [panel-key :score]))
                                       all-scores       (map :score (map val preview-maps))
                                       heat-color       (value-to-color all-scores colors1 score)
                                       heat-color       (if (nil? heat-color) zero-color heat-color)
                                       ;sel? (= reco-selected panel-key)
                                       hh               (js/Math.floor (* (/ (/ height-int charts-high) brick-size) h-multi))
                                       ww               (js/Math.floor (* (/ (/ width-int charts-wide) brick-size) w-multi))
                                       body             [re-com/v-box
                                                         :size "none"
                                                         ; :attr {:on-click #(do (ut/tracked-dispatch [::conn/click-parameter [:recos-sys2 :combo_hash] combo_hash])
                                                         ;                       (ut/tracked-dispatch [::conn/click-parameter [:recos-sys2 :combo_edn] combo_edn])
                                                         ;                       (ut/tracked-dispatch [::conn/click-parameter [:recos-sys2 :shape_name] shape_name])
                                                         ;                       (ut/tracked-dispatch [::conn/click-parameter [:recos-sys2 :query_map] query_map])
                                                         ;                       (ut/tracked-dispatch [::conn/click-parameter [:recos-sys2 :viz_map] viz_map])
                                                         ;                       (ut/tracked-dispatch [::conn/click-parameter [:recos-sys2 :condis] condis]))}
                                                         ;:width (px (/ width-int 3)) ;:width "240px"
                                                         :width (px (* ww brick-size))
                                                         :height (px (* hh brick-size))
                                                         :style {:zoom         0.44
                                                                 :padding-left "10px"
                                                                 :transform "translate(0)"  ;; a known CSS hack for fixed position in non fixed parents!!! TODO futureproof it?
                                                                 :cursor       "grab"
                                                                 :border       (str "1px dashed " (theme-pull :theme/editor-outer-rim-color nil) 44)
                                                                 :overflow     "auto"}
                                                                 ;:border-bottom (str "1px solid " (theme-pull :theme/editor-outer-rim-color nil) 88)

                                                         :justify :between
                                                         :children
                                                         [[re-com/h-box
                                                           :justify :between
                                                           :children
                                                           [[re-com/h-box
                                                             :gap "5px"
                                                             :style {:padding-left "3px"}
                                                             :children [[re-com/box :size "none" :child " "
                                                                         :style {:background-color heat-color
                                                                                 :width            "26px" :height "26px"
                                                                                 :border-radius    "12px" :margin-top "13px"}]
                                                                        [re-com/box
                                                                         :size "auto"
                                                                         :height "40px"
                                                                         :style {:padding-left  "10px"
                                                                                 :padding-top   "4px"
                                                                                 ;:background-color "orange"
                                                                                 ;:border-radius "14px"
                                                                                 ;:cursor "grab"
                                                                                 :font-family   (theme-pull :theme/base-font nil)
                                                                                 :padding-right "14px"
                                                                                 :z-index       105
                                                                                 :filter        "drop-shadow(0 0 0.9rem black)"
                                                                                 :font-weight   400
                                                                                 :font-size     "30px"}
                                                                         :attr {:on-double-click #(do
                                                                                                    (ut/tracked-dispatch [::set-recos-page2 0])
                                                                                                    (ut/tracked-dispatch [::conn/click-parameter [:viz-shapes-sys2 :combo_edn] ce]))}
                                                                         :child (str ;panel-key " " pnum
                                                                                 combo_edn)]]]
                                                                                  ;;" " (value-to-color all-scores colors1 score)

                                                            [re-com/box
                                                             :padding "6px"
                                                             :style {:font-size "20px" :opacity 0.3 :font-weight 700}
                                                             :child (str (rounder score))]]]

                                                          [honeycomb panel-key (or selected-view :oz) ww hh]
                                                          [re-com/box
                                                           :size "auto"
                                                           :height "40px"
                                                           :justify :end :align :end
                                                           :style {:padding-left   "14px"
                                                                   :padding-right  "10px"
                                                                   :padding-bottom "4px"
                                                                   :font-family    (theme-pull :theme/base-font nil)
                                                                   :filter         "drop-shadow(0 0 0.9rem black)"
                                                                   :z-index        105
                                                                   :font-weight    400
                                                                   :font-size      "29px"}
                                                           :attr {:on-double-click #(do
                                                                                      (ut/tracked-dispatch [::set-recos-page2 0])
                                                                                      (ut/tracked-dispatch [::conn/click-parameter [:viz-shapes0-sys2 :shape] shape_name]))}
                                                           :child (str shape_name)]]]]

                                   ;;  (if sel? (draggable
                                   ;;            (sql-spawner-meta :viz-reco) "meta-menu"
                                   ;;            [re-com/box
                                   ;;             :style {:border "2px dashed #9973e0"
                                   ;;                     :cursor "grab"}
                                   ;;             :width "258px"
                                   ;;             :height "191px"
                                   ;;             :child body])
                                   ;;      body)

                                   [re-com/box
                                    :size "auto"
                                    ;:width (px (/ width-int 3))
                                    ;:height (px (/ height-int 2))
                                    ;:style {:border "2px dashed #9973e0" }
                                    :child (draggable
                                            (sql-spawner-meta panel-key) "meta-menu"
                                             ;; [re-com/box
                                             ;;  :style {:border "2px dashed #9973e0"
                                             ;;          :cursor "grab"}
                                             ;;  ;:width "258px"
                                             ;;  ;:height "191px"
                                             ;;  :child body]
                                            body)])

                                 (catch :default _
                                   [re-com/box
                                    :height (px (* (js/Math.floor (* (/ (/ height-int charts-high) brick-size) h-multi)) brick-size))
                                    :width (px (* (js/Math.floor (* (/ (/ width-int charts-wide) brick-size) w-multi)) brick-size))
                                    :size "auto"
                                    :align :center
                                    :justify :center
                                    :style {:color     (str (theme-pull :theme/editor-font-color nil) 22) ; "#ffffff22"
                                            :zoom      0.44
                                            :font-size "40px"}
                                    :child "n/a"])))            ;(str e)

        per-page          (* charts-wide charts-high)
        pages             (/ recos-count per-page)]

    (ut/tapp>> [:full-recos recos-page preview-maps preview-keys query-id full-recos (select-keys preview-maps preview-keys)])

    (dorun (let [prev-preview-keys (for [k @(ut/tracked-subscribe [::preview-keys2])] (keyword (str "reco-preview" k)))
                 ;per-page per-page ;; if we ever get more room...
                 grid-page         @(ut/tracked-subscribe [::recos-page2])
                 grid-page         (if (> (- grid-page 1) pages) 0 grid-page)
                 recos             (take per-page (drop (* grid-page per-page) @(ut/tracked-subscribe [::conn/sql-data [:recos-sys2]])))
                 recos-keys        (vec (for [{:keys [combo_hash]} recos] combo_hash))]
             (doseq [k prev-preview-keys] (ut/tracked-dispatch [::quick-delete-panel k]))
             (ut/tracked-dispatch [::set-preview-keys2 recos-keys])
             (doseq [{:keys [combo_hash query_map viz_map condis combo_edn shape_name]} recos]
               (insert-hidden-reco-preview combo_hash viz_map query_map condis combo_edn shape_name false))))

    (dorun (for [[k v] sql-calls]
             (let [query        (ut/postwalk-replacer sql-params v)
                   data-exists? @(ut/tracked-subscribe [::conn/sql-data-exists? [k]])
                   unrun-sql?   @(ut/tracked-subscribe [::conn/sql-query-not-run? [k] query])]
               ;(ut/tapp>> [:mad-libs-sql k query])
               (when (or (not data-exists?) unrun-sql?)
                 (conn/sql-data [k] query)))))

    [re-com/v-box
     ;:height "433px"
     :style {:margin-right "8px"}
     :height (px height-int)
     :width (px width-int)
     :children [[re-com/h-box
                 :size "auto"
                 :style {:color (theme-pull :theme/editor-font-color nil)}
                         ;:border "2px solid yellow"

                 :children [(let []                         ;; get the next 6 graphs and render?
                              [re-com/h-box :children
                               [[re-com/v-box
                                 :size "auto"
                                 :height (px height-int)    ;"380px"
                                 :width (px width-int)      ;"770px"

                                 ;;  :children [[re-com/h-box
                                 ;;              :size "none"
                                 ;;              :children [[preview-container preview-maps preview-keys 0]
                                 ;;                         [preview-container preview-maps preview-keys 1]
                                 ;;                         [preview-container preview-maps preview-keys 2]]]
                                 ;;             [re-com/h-box
                                 ;;              :size "none"
                                 ;;              :children [[preview-container preview-maps preview-keys 3]
                                 ;;                         [preview-container preview-maps preview-keys 4]
                                 ;;                         [preview-container preview-maps preview-keys 5]]]]

                                 ;;  :children (for [h (range charts-high)]
                                 ;;              [re-com/h-box :size "none"
                                 ;;               :children (for [w (range charts-wide)]
                                 ;;                           [preview-container preview-maps preview-keys (* (+ 1 h) (+ w 1))])])

                                 :children (for [h (range charts-high)]
                                             [re-com/h-box :size "none"
                                              :children (for [w (range charts-wide)]
                                                          [rc/catch [preview-container preview-maps preview-keys (+ (* h charts-wide) w)]])])]
                                [re-com/v-box
                                 :size "auto"
                                 :height (px height-int)    ;"380px"
                                 :style {:border-top "1px solid #9973e066"
                                         :overflow   "hidden"}
                                 :children [[re-com/box
                                             :child [re-com/md-icon-button
                                                     :md-icon-name "zmdi-trending-up"
                                                     :on-click #(do (clear-preview2-recos)
                                                                    (reset! mad-libs-top? (not @mad-libs-top?)))
                                                     :style {:font-size  "22px"
                                                             :cursor     "pointer"
                                                             :opacity    0.5
                                                             :padding    "0px"
                                                             :margin-top "-1px"}]
                                             :height "20px"
                                             :style {:font-size        "12px"
                                                     :color            (if @mad-libs-top?
                                                                         (theme-pull :theme/editor-background-color nil) ;;"#000000"
                                                                         (str (theme-pull :theme/editor-font-color nil)))
                                                     :background-color (if @mad-libs-top?
                                                                         "#9973e0" ;"darkcyan"
                                                                         "inherit")}
                                             :align :center :justify :center]

                                            [re-com/v-box
                                             :children (for [[k v] sql-params-minus
                                                             :when (not (nil? v))
                                                             :let [zmdi      (if (= k :viz-shapes0-sys2/shape) "zmdi-chart" "zmdi-map")
                                                                   splts     (map keyword (ut/splitter (ut/replacer (str k) #":" "") #"/"))
                                                                   splts-vec [(first splts) (last splts)]]]
                                                         [re-com/box
                                                          :child [re-com/md-icon-button
                                                                  :tooltip (str k)
                                                                  :md-icon-name zmdi
                                                                  ;:on-click #(clear-filters-fn)
                                                                  :on-click #(do (ut/tracked-dispatch [::set-recos-page2 0])
                                                                                 (ut/tracked-dispatch [::conn/click-parameter splts-vec nil])) ;; clear THIS param
                                                                  :style {:font-size  "22px"
                                                                          :cursor     "pointer"
                                                                          :opacity    0.5
                                                                          :padding    "0px"
                                                                          :margin-top "-1px"}]
                                                          :height "20px"
                                                          :style {:font-size        "12px"
                                                                  ;:color (if false ;@mad-libs-top?
                                                                  ;         (theme-pull :theme/editor-background-color nil) ;;"#000000"
                                                                  ;         (str (theme-pull :theme/editor-font-color nil)))
                                                                  :background-color (if false ;@mad-libs-top?
                                                                                      "#9973e0" ;"darkcyan"
                                                                                      "inherit")}
                                                          :align :center :justify :center])]

                                            [re-com/gap :size "2px"]

                                            [re-com/v-box
                                             :children (for [c (range pages)]
                                                         [re-com/box
                                                          :child (str (+ 1 c)) ; (if (= c recos-page) (str (+ 1 c)) "..")
                                                          :height "19px"
                                                          :align :end
                                                          :justify :center
                                                          :attr {:on-click #(ut/tracked-dispatch [::set-recos-page2 c])}
                                                          :style {:padding-right    "4px"
                                                                  :cursor           "pointer"
                                                                  :font-size        "10px"
                                                                  :color            (if (= c recos-page)
                                                                                      (theme-pull :theme/editor-background-color nil) ;;"#000000"
                                                                                      (str (theme-pull :theme/editor-font-color nil) 77))
                                                                  :background-color (if (= c recos-page)
                                                                                      "#9973e0" ;"darkcyan"
                                                                                      "inherit")
                                                                  :border-bottom    "1px dashed #9973e066"}])]]
                                                                  ;:border-right "1px solid #9973e066"

                                 :width "30px"]]])]]]]))

(def timer-id (reagent/atom nil))
(def hover-field (reagent/atom nil))
(def animate? (reagent/atom false))

(re-frame/reg-sub
 ::group-by-intersection
 (fn [db [_ query-key1 query-key2]]
   (let [;queries (into {} (for [p (get-in db [:panels])] (get p :queries)))
         qgb1 (vec (for [[f fv] (get-in db [:meta query-key1 :fields])
                         :when (get fv :group-by?)] f))
         qgb2 (vec (for [[f fv] (get-in db [:meta query-key2 :fields])
                         :when (get fv :group-by?)] f))]
     (ut/tapp>> [qgb1 qgb2])
     [])))

(re-frame/reg-sub
 ::group-bys
 (fn [db [_ query-key1]]
   (let [;queries (into {} (for [p (get-in db [:panels])] (get p :queries)))
         qgb1 (vec (for [[f fv] (get-in db [:meta query-key1 :fields])
                         :when (get fv :group-by?)] f))]
         ;qgb2 (vec (for [[f fv] (get-in db [:meta query-key2 :fields])
         ;           :when (get fv :group-by?)] f))

     qgb1)))

(re-frame/reg-sub
 ::query-waitings
 (fn [_ [_ query-key]]
   (let [;; messages (map :message @(ut/tracked-subscribe [::wfx/pending-requests http/socket-id]))
        ;;  waitings (count (filter #(and (> (count (get % :ui-keypath)) 1)
        ;;                                (cstr/includes? (str (get % :ui-keypath)) (str query-key)))
        ;;                          messages))
         waitings (true? (some #(= % query-key)
                               (for [e
;                                     @(ut/tracked-subscribe [::wfx/pending-requests http/socket-id])
                                     @(rfa/sub ::http/pending-requests {:socket-id http/socket-id})
                                     ]
                                 (let [p (get-in e [:message :ui-keypath])]
                                   (when (> (count p) 1) (first p))))))
         single-wait (true? (some #(= % query-key)
                                  (for [e
                                        ;;@(ut/tracked-subscribe [::wfx/pending-requests http/socket-id])
                                        @(rfa/sub ::http/pending-requests {:socket-id http/socket-id})
                                        ]
                                    (let [p (get-in e [:message :ui-keypath])]
                                      (when (= (count p) 1) (first p))))))]
         ;single-wait (if single-wait 1 0)
        ;;  single-wait (count (filter #(and (> (count (get % :ui-keypath)) 1)
        ;;                                   (cstr/includes? (str (get % :ui-keypath)) (str query-key)))
        ;;                             messages))

     ;(ut/tapp>> [:yo ])
     [waitings single-wait])))

(re-frame/reg-event-db
 ::toggle-heatmap
 (fn [db [_ panel-key query-key column]]
   (let [query (get-in db [:panels panel-key :queries query-key])
         style-rules (get query :style-rules)
         heat-keys (into {} (first (for [[[k1 k2] v] style-rules
                                         :when (and (cstr/starts-with? (str k2) ":heatmap-")
                                                    (= k1 column))]
                                     {[k1 k2] v})))
         has-one? (not (empty? heat-keys))
         base-key [column (keyword (str "heatmap-" (rand-int 45)))]
         base-style {:logic true
                     :style {:background-color [:heatmap]
                             :border
                             "1px solid #00000000"
                             :color "#000000"}}
         new-style-rules (if has-one?
                           (dissoc style-rules (first (keys heat-keys)))
                           (assoc style-rules base-key base-style))]
     (if (empty? new-style-rules)
       (ut/dissoc-in db [:panels panel-key :queries query-key :style-rules]) ;; no need for empty map
       (assoc-in db [:panels panel-key :queries query-key :style-rules] new-style-rules)))))

(re-frame/reg-sub
 ::server-kits
 (fn [db [_ type]] ;; TODO add "who is asking" deets like panel, query, so we can do kit WHEN filtering on fields, data, etc
   (let [all (get-in db [:server :settings :kits])
         ;_ (ut/tapp>> all)
         ;;curr @(ut/tracked-subscribe [::conn/clicked-parameter-key [:kits-sys/enabled]])
         curr @(ut/tracked-sub ::conn/clicked-parameter-key-alpha {:keypath [:kits-sys/enabled]})
         kits (vec (sort-by :name (for [[k v] all
                                        ;:let [_ (ut/tapp>> [:0 [(get v :package-name) (get v :kit-name)] curr])]
                                        :when (and (some #(= [(get v :package-name) (get v :kit-name)] %) curr) ;; is enabled in params?
                                                   (= (get v :run-on) type))]
                                    (merge {:kit-name k} v))))]
     kits)))

(re-frame/reg-sub
 ::installed-kits
 (fn [db _] ;; TODO add "who is asking" deets like panel, query, so we can do kit WHEN filtering on fields, data, etc
   (let [all (get-in db [:server :settings :kits])
        ; _ (ut/tapp>> [:all-kits (get db :client-name) all (get db :server)])
         kits (vec (sort-by :name (for [[k v] all]
                                    (merge {:installed? true
                                            :kit-name k} v))))]
     kits)))

;(ut/tapp>> [:installed-kits @(ut/tracked-subscribe [::installed-kits])])


(re-frame/reg-sub
 ::market-kits
 (fn [_ _] ;; TODO add "who is asking" deets like panel, query, so we can do kit WHEN filtering on fields, data, etc
   [{:kit-name :cool-thing1 :name "Todd Howard" :description "community repo placeholder1"  :package-name :cool-package1}
    {:kit-name :sandwiches! :name "Sandwiches!" :description "sandwiches!"                  :package-name :skyrim-sandwiches}
    {:kit-name :cool-thing3 :name "Horse Armour" :description "community repo placeholder3" :package-name :cool-package1}
    {:kit-name :cool-thing4 :name "Gnomes" :description "community repo placeholder4"       :package-name :cool-package1}]))

;; [{:kit-name :boogers :name "boogs" :description "market placeholder" :package-name :boo}
;;  {:kit-name :boogers2 :name "boogs2" :description "market placeholder2" :package-name :boo}]


(defn magic-table [panel-key data-keypath & [ww hh hide-fields]]
  ;[rc/catch ;(try
  ;(ut/tapp>> [:magic-table [panel-key data-keypath [ww hh hide-fields] ]])
  (let [viewer-sourced?           (try (and (cstr/starts-with? (first data-keypath) "::") (string? (first data-keypath))) (catch :default _ false))
        data-keypath              (if viewer-sourced? (try [(edn/read-string (ut/replacer (first data-keypath) "::" ":"))] (catch :default _ nil)) data-keypath) ;; unpack the viewer string
        panel-map                 @(ut/tracked-subscribe [::workspace [panel-key]])
        query-key                 (first data-keypath)
        w                         (get panel-map :w 11)
        h                         (get panel-map :h 10)
        column-selected           @(ut/tracked-subscribe [::column-selected panel-key query-key]) ;; reaction hack TODO (important!)
        custom-col-widths         @(ut/tracked-subscribe [::custom-col-widths panel-key query-key])
        {:keys
         [row-height
          render-all?
          clicked-row-height]}    @(ut/tracked-subscribe [::custom-rows panel-key query-key])
        width-int                 (- (* (if ww ww
                                            w) brick-size) 31)
        modded-width   (- width-int 8)
        height-int                (* (if hh hh
                                         h) brick-size)
        height-int                (if viewer-sourced? (+ height-int 38) height-int)
        selected?                 @(ut/tracked-sub ::selected-block? {:panel-key panel-key})
        page-num                  @(ut/tracked-subscribe [::page-num panel-key query-key])
        parameters-used-in        @(ut/tracked-sub ::parameters-used-from {:query-key query-key})
        parameters-used-in-sql    (vec (filter #(not (vector? %)) parameters-used-in))
        parameters-used-in-cell   (vec (filter #(vector? %) parameters-used-in))
        ;; expensive logic here.... must re-visit  COMBINE ALL 3 INTO A SINGLE SUB FOR SUB-CACHE PERF ?
        selected-block            @(ut/tracked-sub ::selected-block {})
        subq-blocks               @(ut/tracked-sub ::subq-panels-alpha {:panel-id selected-block})
        parent-of-selected?       (some #(= % panel-key) subq-blocks)
        server-kits               @(ut/tracked-subscribe [::server-kits :queries])
        ;_ (ut/tapp>> server-kits)
        ;child-parts (if parent-of-selected? (vec (ut/deep-flatten @(ut/tracked-subscribe [::panel-sql-calls selected-block]))) [])
        ;subq-mapping @(ut/tracked-subscribe [::subq-mapping])
        ;; running?                  (some #(= % query-key)
        ;;                                 (for [e @(ut/tracked-subscribe [::wfx/pending-requests http/socket-id])]
        ;;                                   (let [p (get-in e [:message :ui-keypath])]
        ;;                                     (when (= (count p) 1) (first p)))))
        ;db/context-box (reagent/atom nil)
        ;; wasteful for simple col decorations... :/
        ;   sql-keys @(ut/tracked-subscribe [::panel-sql-call-keys panel-key])
        ;   reco-selected (let [rr @(ut/tracked-subscribe [::conn/clicked-parameter-key [:viz-tables-sys/table]])
        ;                       rr (if (not (nil? rr)) (keyword (ut/replacer rr "_" "-")) nil)] rr)
        ;   viz-reco? (and (or (= selected-block "none!") (nil? selected-block))
        ;                  (some #(= % reco-selected) sql-keys)
        ;                  (= @db/editor-mode :viz))

        deeper-upstream?          false                     ; (some #(= % panel-key) (ut/cached-upstream-search subq-mapping selected-block))
        kit-callout-fields        []
        child-parts               (into kit-callout-fields
                                        (cond parent-of-selected?
                                              (vec                ;;
                                               (ut/deep-flatten (merge
                                                                 ;;@(ut/tracked-subscribe [::panel-sql-calls selected-block])
                                                                 @(rfa/sub ::panel-sql-calls {:panel-key selected-block})
                                                                 ;;@(ut/tracked-subscribe [::views selected-block])
                                                                 @(rfa/sub ::views {:panel-key selected-block}))))
                                        ;  viz-reco? (vec (for [f (ut/splitter
                                        ;                    (ut/replacer @(ut/tracked-subscribe [::conn/clicked-parameter-key [:recos-sys/combo_edn]]) #"\"" "") ", ")]
                                        ;              (keyword f)))
                                              :else []))
        ;deeper-parts (if upstream? (vec (ut/deep-flatten @(ut/tracked-subscribe [::panel-sql-calls selected-block]))) [])
        ;; expensive logic here.... ^^^^ alert!

        ;hide-fields (try (nth block-sizes 2) (catch :default _ []))
        hide? #_{:clj-kondo/ignore [:not-empty?]}
        (not (empty? hide-fields)) ;(not (empty? hide-fields))
        ;height-int (* (:h panel-map) brick-size)
        ;;rowset                    (re-frame.core/subscribe [::conn/sql-data data-keypath])
        rowset                    (rfa/sub ::conn/sql-data-alpha {:keypath data-keypath})
        non-panel?                (false? (or (nil? hh) (nil? ww)))

        ;is-rowset? (true? (= (db/data-typer @rowset) "rowset"))
        ff                        (filter #(not (cstr/starts-with? (str %) ":styler_")) (keys (first @rowset))) ;; fields with styles thrown out

        selected-field-idx        (.indexOf ff column-selected)
        col-names                 (reverse (into () @(ut/tracked-subscribe [::col-names])))
        col-selected?             (> selected-field-idx -1)
        ff                        (if (and col-selected? (and
                                                          (not (nil? col-names))
                                                          (ut/ne? col-names)))
                                    col-names
                                    ff)
        ;; (try (vec (for [r (get result :result)] (ut/asort r map-order))) (catch :default _ (get result :result)))
        selected-field-code       (get-in
                                   ;;@(ut/tracked-subscribe [::panel-sql-calls selected-block])
                                   @(rfa/sub ::panel-sql-calls {:panel-key selected-block})
                                   [query-key :select selected-field-idx])
        fields                    (if hide? (cset/difference
                                             (set ff)
                                             (set hide-fields))
                                      ff)
        child-cols                (cset/intersection (set child-parts) (set fields))
        fields-cnt                (count fields)
        clicked-row               @(ut/tracked-subscribe [::conn/clicked-parameter data-keypath])
        ;clicked? (not (nil? clicked-row))
        equal-width               (js/Math.floor (/ modded-width fields-cnt))
        ;parent-sql (get-in panel-map [:views query-key])
        equal-width-min           100
        row-height                (if (not (nil? row-height)) row-height 25) ;50 ;100 ;25 ;25

        ;metadata                  @(ut/tracked-subscribe [::conn/sql-metadata data-keypath])
        ;post-metadata             @(ut/tracked-subscribe [::conn/sql-post-metadata data-keypath])

        metadata                  @(ut/tracked-sub ::conn/sql-metadata-alpha {:keypath data-keypath})
        post-metadata             @(ut/tracked-sub ::conn/sql-post-metadata-alpha {:keypath data-keypath})

        ;post-styles @(ut/tracked-subscribe [::conn/sql-post-styles data-keypath])
        ;post-styles-rows @(ut/tracked-subscribe [::conn/sql-post-styles-row-lookups data-keypath])
        ;post-styles-cells @(ut/tracked-subscribe [::conn/sql-post-styles-cell-lookups data-keypath])
        stylers                   @(ut/tracked-subscribe [::stylers panel-key query-key])
        ;post-styles-rows
        ;style-rules (get-in panel-map [:queries query-key :style-rules])
        ;has-row-styles? (not (empty? (flatten post-styles-rows)))
        ;has-cell-styles? (not (empty? (flatten post-styles-cells)))
        rowcount                  (get metadata :rowcount)
        full-rowcount             (get-in post-metadata [:* :rowcount])
        rows-per-page             (if (= page-num -1) full-rowcount 200)
        curr-row-start            (+ (- (* page-num rows-per-page) rows-per-page) 1)
        curr-row-end              (+ rows-per-page curr-row-start)
        curr-row-str              (str curr-row-start " - "
                                       (if (> full-rowcount curr-row-end) curr-row-end full-rowcount))
                                       ;curr-row-end

        ttl-pages                 (nf (js/Math.ceil (/ (or full-rowcount rowcount) rows-per-page)))
        rowcount-string           (cond (= rowcount 1) "1 row"
                                        (= page-num -1) (str (nf rowcount) " rows (limit off!)")
                                        (and (or (> full-rowcount rows-per-page) (> rowcount rows-per-page)) (nil? full-rowcount))
                                        (str curr-row-str " of " (nf rowcount) "+ rows")
                                        (and (or (> full-rowcount rows-per-page) (< rowcount rows-per-page)) (nil? full-rowcount))
                                        (str (nf rowcount) " rows")
                                        :else (str (if (> full-rowcount rows-per-page)
                                                     (str curr-row-str " of ") "") (nf full-rowcount) " rows"))
        modded-height             (- height-int 155)
        max-rows                  (+ (js/Math.floor (/ modded-height row-height))
                                     ;0.75
                                     (if (and (= @formula-bar data-keypath) selected? (not non-panel?)) 1 2))
        ;max-rows (+ 1 (/ modded-height row-height))
        equal-width-final         (if (<= equal-width-min equal-width) equal-width equal-width-min)
        ;equal-width-final equal-width
        table-source              (get-in panel-map [:queries query-key :from])
        ;tt query-key
        ;table-source-root [(keyword (last (ut/splitter (ut/safe-name (first (flatten table-source))) #"/")))]
        drag-meta                 (get @dragging-body :drag-meta)
        connection-id             @(ut/tracked-subscribe [::connection-id panel-key])
        relevant-for-dyn-drop?    (if @dragging?
                                    (and
                                     (or (= (get drag-meta :connection-id) connection-id)
                                         (= (get drag-meta :type) :param)
                                         (= (get drag-meta :type) :query)
                                         (= (get drag-meta :type) :view)
                                         (= (get drag-meta :type) :where))
                                     (not @over-flow?)
                                     (not (= :flow-fn-sys (get drag-meta :source-query)))
                                     (not (= :meta-screens (get drag-meta :type)))
                                     (not (= :meta-theme (get drag-meta :type)))
                                     (not (= :meta-board (get drag-meta :type)))
                                     (not (= :meta-blocks (get drag-meta :type))))
                                    false)
        ;(not (or (= tt :tables-sys)
        ;                                (= tt :fields-sys)
        ;                                (= tt :attribs-sys)
        ;                                (= tt :combos-sys)
        ;                                (= tt :connections-sys)))
        ;; relevant-for-dyn-drop? (or (= (get-in @dragging-body [:drag-meta :source-table]) (first (flatten table-source)))
        ;;                            (= (get-in @dragging-body [:drag-meta :source-panel-key]) panel-key)

        ;;                            (and (or (= (get-in @dragging-body [:drag-meta :type]) :meta-tables)
        ;;                                     (= (get-in @dragging-body [:drag-meta :type]) :meta-fields)
        ;;                                     (= (get-in @dragging-body [:drag-meta :type]) :query))

        ;;                                 (>= (count (cset/intersection (set (keys (get @(ut/tracked-subscribe [::conn/sql-metadata table-source-root]) :fields)))
        ;;                                                        (set (get-in @dragging-body [:drag-meta :table-fields])))) 1))

        ;;                           ; (and (not (= (get-in @dragging-body [:drag-meta :source-table]) (first (flatten table-source))))
        ;;                           ;      (some #(= % (get-in @dragging-body [:drag-meta :param-field]))
        ;;                           ;            (keys (get @(ut/tracked-subscribe [::conn/sql-metadata table-source-root]) :fields)))
        ;;                           ;      )
        ;;                            ; when tables are NOT the same, but are from the same db (?) and have columns with the same name and TYPE...
        ;;                            ;                 (suggest a join)
        ;;                            ;true

        ;;                            ;(some #(= % (get-in @dragging-body [:drag-meta :param-field])) fields)
        ;;                            (some #(= % (get-in @dragging-body [:drag-meta :param-field]))
        ;;                                  (keys (get @(ut/tracked-subscribe [::conn/sql-metadata table-source-root]) :fields))))
        callout-instead?          (and (= rowcount 1) (= fields-cnt 1))
        mad-libs?                 (= query-key @mad-libs-view)
        overlay?                  (or mad-libs?
                                      (and (not non-panel?) ;; no render on editor panels?
                                           @dragging? relevant-for-dyn-drop?))
        ; first-matching-style (fn [r] (apply merge (remove nil? (for [[_ {:keys [rows styles]}] post-styles-rows]
        ;                                                          (when (some #(= % r) rows) styles)))))
        ; first-matching-style-cell (fn [r f] (apply merge (remove nil? (for [[_ {:keys [rows styles]}] (get post-styles-cells f)]
        ;                                                                 (when (some #(= % r) rows) styles)))))
        query                     (get-in panel-map [:queries query-key])
        panel-conn-id             @(ut/tracked-subscribe [::panel-connection-id panel-key])
        has-rabbit-code?          (true? (some (fn [x] (= x "rabbit-code")) (ut/deep-flatten (get metadata :fields))))
        clicked-rabbit-code-multiplier 4
        clicked-row-height        (if (not (nil? clicked-row-height)) clicked-row-height (* row-height clicked-rabbit-code-multiplier))
        sys-panel?                (cstr/ends-with? (str query-key) "-sys")
        text-color                (if non-panel?
                                    (theme-pull :theme/editor-grid-font-color nil)
                                    (theme-pull :theme/grid-font-color nil))
        selected-text-color       (if non-panel?
                                    (theme-pull :theme/editor-grid-selected-font-color nil)
                                    (theme-pull :theme/grid-selected-font-color nil))
        selected-background-color (if non-panel?
                                    (theme-pull :theme/editor-grid-selected-background-color nil)
                                    (theme-pull :theme/grid-selected-background-color nil))
        hover-field-enable?        (true? (and (not @dragging?)
                                               (not @over-flow?)
                                               (not @db/dragging-flow-editor?)
                                               (not @dragging-editor?)
                                               (not @mouse-dragging-panel?)))
        hovered-field-name         [@hover-field @animate? @db/context-box] ;; react-hack! leave in!
        has-pages?                 (and  (>= full-rowcount rows-per-page)
                                         (or (> page-num 0) (nil? page-num))
                                         (not (= page-num -1)))
        last-page?                  (and has-pages? (= page-num (js/Math.ceil (/ full-rowcount rows-per-page))))
        first-page?                 (and has-pages? (or (nil? page-num) (= page-num 1)))
        [waits? single-wait?] @(ut/tracked-subscribe [::query-waitings query-key])
        default-col-widths @(rfa/sub ::column-default-widths {:panel-key panel-key :query-key query-key})
        running? single-wait?
        double-click-timeout 400]

    (when (not= default-col-widths equal-width-final)
      ;;(ut/tapp>> [:default-col-widths panel-key query-key default-col-widths equal-width-final])
      (ut/tracked-dispatch [::set-column-default-widths panel-key query-key equal-width-final])) ;; bad side effect TODO: pure fn to calc equal widths from anywhere

    ;; (ut/tapp>> [:ff col-selected? col-names ff])
    ;(ut/tapp>> [:hohoho @hover-field])
    ;(ut/tapp>> [:max-rows max-rows])
    ;(ut/tapp>> [:magic-table-meta metadata])
    ;(ut/tapp>> [:grid-param-highlight data-keypath parameters-used-in-cell parameters-used-in-sql parameters-used-in])

    ;(ut/tapp>> [:post-styles-cells post-styles-cells])
    ;(ut/tapp>> [table-source table-source-root])
    ;(ut/tapp>> [panel-key data-keypath modded-width fields (count fields) equal-width equal-width-final :hide? hide? hide-fields])
    ;(when @dragging? (ut/tapp>> @dragging-body))
    ;;(ut/tapp>> @rowset)
    ;;(ut/tapp>> [:parts child-parts])

    (if (seq @rowset)                                       ;(and (seq @rowset) (not running?))

      ; (if (and @dragging? relevant-for-dyn-drop?)
      ;  [dynamic-spawner-targets table-source @dragging-body panel-key query-key width-int height-int]

      (if (and callout-instead? (nil? hh))

        (let [label         (nth fields 0)
              md            (get-in metadata [:fields label :data-type])
              num?          (or (= md "integer") (= md "float"))
              value         (get-in @rowset [0 label])
              formatted-val (if num? (str (nf value)) (str value))
              len           (count (str formatted-val))
              charw         (js/Math.ceil (/ (* (js/Math.ceil (/ (+ h w 1) 2)) brick-size) len))
              pxsize        (px charw)
              labelsz       (* height-int 0.1)
              agg?          (not (get-in metadata [:fields label :group-by?]))
              pxlabel       (px (if (< labelsz 20) 20 labelsz))]
          ;(ut/tapp>> [charw])
          ^{:key (str "magic-table-callout-parent" query-key)}
          [re-com/v-box
           :size "auto"
           :style {:overflow "hidden"}
           :align :center
           :justify :center
           :class (when (and (= panel-key (last @hover-field)) @animate?) "zoom-and-fade2")
           :children
           [(when (and overlay? (not mad-libs?))
              ;[dynamic-spawner-targets table-source @dragging-body panel-key query-key width-int height-int]
              @(ut/tracked-subscribe [::dynamic-drop-targets table-source @dragging-body panel-key query-key width-int height-int]))

            [re-com/box :child (str label)
             :size "none" :height (px (* height-int 0.1))
             :style {:font-weight 700
                     :transition "all 0.6s ease-in-out"
                     :filter      (if overlay? "blur(3px) opacity(88)" "none")
                     :opacity     0.6 :font-size pxlabel}]

            [re-com/box
             :align :center :justify :center
             :child formatted-val
             :attr (if agg?
                     {:on-double-click (fn [_]
                                         (let [query-key (keyword
                                                          (str (ut/replacer (str query-key) #":" "")
                                                               "->-"
                                                               (ut/replacer (str (first @hover-field)) #":" "")))
                                               base-table (get-in table-source [0 0])
                                               is-sub? (true? (cstr/starts-with? (str base-table) ":query/"))
                                               new-query (cond is-sub?
                                                               (try (sql-alias-replace-sub base-table) (catch :default _ {:select [["error in sql-alias-replace-sub1" :err]]}))
                                                               (map? base-table) (try (sql-alias-replace base-table) (catch :default _ {:select [["error in sql-alias-replace-sub2" :err]]}))
                                                               :else {:select [:*] :from table-source})]
                                           (reset! animate? true)
                                           (ut/tracked-dispatch [::add-query panel-key query-key new-query])
                                           (js/setTimeout #(do ;; change view tab
                                                             (ut/tracked-dispatch [::select-view panel-key query-key])
                                                             (reset! animate? false)) 1000)))} {})
             :size "auto"
             :style {:font-size   pxsize
                     :cursor (when agg? "pointer")
                     :transition "all 0.6s ease-in-out"
                     :text-decoration (when agg? "underline")
                     :filter      (if overlay? "blur(3px) opacity(88)" "none")
                     :font-weight 700}]]])

        ^{:key (str "magic-table-base-wrapper" query-key)}
        [re-com/v-box
         :style {:margin-top "-8px"}
         :children [(when (and overlay? (not mad-libs?))
                      ;[dynamic-spawner-targets table-source @dragging-body panel-key query-key width-int height-int]
                      @(ut/tracked-subscribe [::dynamic-drop-targets table-source @dragging-body panel-key query-key width-int height-int]))

                    (when (and overlay? mad-libs? (not non-panel?)) ;;; MAD LIBS
                      [re-com/box
                       :child [mad-libs-shapes query-key width-int height-int]
                       :size "none"
                       :align :center :justify :center :height (px (- height-int 40)) :width (px width-int)
                       :style {:position "fixed" :z-index 100}])

                    ^{:key (str "magic-table-base-" data-keypath)}
                    [re-com.core/simple-v-table
                     :columns (vec (for [c fields]
                                     (let [cwidth (cond (cstr/ends-with? (str query-key) "-sys") ;; TODO, convert to "native" rabbitsql :col-width keys system, no more 2 systems
                                                        (cond (some #(= % c) [:queries :fields :views :blocks :recos :combo_hash :schema_cat]) 66
                                                              (= c :database_name) 115
                                                              (= c :description) 550
                                                              (and (= c :name) (cstr/includes? (str query-key) "-fn-")) 145
                                                              (= c :shape_name) 180
                                                              (= c :block_key) 88
                                                              (= c :items) 70
                                                              (= c :item_key) 300
                                                              ;(= c :item_type) 85
                                                              (= c :table_name) 204
                                                              (or (= c :shape) (= c :table)) 204
                                                              (= c :query_map) 282
                                                              (and (some #(= % c) [:screen_name :block_name])
                                                                   (= data-keypath [:blocks-sys])) 298
                                                              (and (some #(= % c) [:screen_name :block_name])
                                                                   (not (= data-keypath [:blocks-sys]))) 385
                                                              :else equal-width-final)
                                                        (and (some #(= % c) (keys custom-col-widths))
                                                             ;(not non-panel?)
                                                             )
                                                        (get custom-col-widths c) ;; :col-wdiths keys exists in honey
                                                        :else equal-width-final)
                                           row-num-fn (fn [row] (try (.indexOf @rowset row)
                                                                     (catch :default _ -1)))]
                                       {:id           c
                                        :header-label (str c)
                                        :distinct     (let [q (get-in metadata [:fields c :distinct])
                                                            f (get-in post-metadata [c :distinct])]
                                                        (if f [true f]
                                                            [false q]))
                                      ;:distinct-full (get-in post-metadata [c :distinct])
                                        :upstream?    (some #(= % c) child-cols)
                                        :data-type    (get-in metadata [:fields c :data-type])
                                        :color        (get @(ut/tracked-subscribe [::conn/data-colors]) (get-in metadata [:fields c :data-type]))
                                        :row-label-fn #(if (= % clicked-row)

                                                         (let [row-hash (str (hash %) c)
                                                               row-height (if has-rabbit-code?
                                                                            clicked-row-height row-height)
                                                               row-num (row-num-fn %)
                                                               hh (js/Math.floor
                                                                   (/ row-height brick-size))
                                                               ww (js/Math.floor
                                                                   (/ cwidth brick-size))]
                                                           (cond (= panel-key :system-tables-list*)
                                                                 (draggable (sql-spawner-meta :meta-tables)
                                                                            "meta-menu"
                                                                            [re-com/box
                                                                             :height (px row-height) ;"22px"
                                                                             :style {:color "#000000"}
                                                                             :child (str (get % c))])
                                                                 (= panel-key :searches-rows-sys-list*)
                                                                 (draggable (item-subscription-spawner-meta)
                                                                            "meta-menu"
                                                                            [re-com/box
                                                                             :height (px row-height) ;"22px"
                                                                             :style {:color "#000000"}
                                                                             :child (str (get % c))])
                                                                 (= panel-key :system-fields-list*)
                                                                 (draggable (sql-spawner-meta :meta-fields)
                                                                            "meta-menu"
                                                                            [re-com/box
                                                                             :height (px row-height) ;"22px"
                                                                             :style {:color "#000000"}
                                                                             :child (str (get % c))])
                                                                 (= panel-key :recos-list*)
                                                                 (draggable (sql-spawner-meta :viz-reco)
                                                                            "meta-menu"
                                                                            [re-com/box
                                                                             :height (px row-height) ;"22px"
                                                                             :style {:color "#000000"}
                                                                             :child (str (get % c))])
                                                                 (= panel-key :files-list*)
                                                                 (draggable (sql-spawner-meta :meta-screens)
                                                                            "meta-menu"
                                                                            [re-com/box
                                                                             :height (px row-height) ;"22px"
                                                                             :style {:color "#000000"}
                                                                             :child (str (get % c))])
                                                                 (= panel-key :blocks-list*)
                                                                 (draggable (sql-spawner-meta :meta-blocks)
                                                                            "meta-menu"
                                                                            [re-com/box
                                                                             :height (px row-height) ;"22px"
                                                                             :style {:color "#000000"}
                                                                             :child (str (get % c))])
                                                                ;;  (= panel-key :flow-fn-list*)
                                                                ;;  (draggable (let [lookup-val []
                                                                ;;                   pval (conn/spawn-open-input-block lookup-val)]
                                                                ;;               (apply conn/add-flow-block pval))
                                                                ;;             "meta-menu"
                                                                ;;             [re-com/box
                                                                ;;              :height (px row-height) ;"22px"
                                                                ;;              :style {:color "#000000"}
                                                                ;;              :child (str (get % c))])

                                                                 (and (= (get-in metadata [:fields c :data-type]) "rabbit-code") true)
                                                                 (draggable (sql-spawner-where :where panel-map data-keypath c panel-key row-num)
                                                                            "meta-menu"
                                                                            (let [rowname (str "query-preview-block" row-hash)
                                                                                  qname (str "query-preview-inviz" row-hash)]
                                                                              (when (and (= (get-in % [c 0]) :*render*)
                                                                                         (empty? @(ut/tracked-subscribe [::panel-map (keyword rowname)])))

                                                                                (ut/tracked-dispatch [::update-workspace [(keyword rowname)]
                                                                                                    (ut/postwalk-replacer
                                                                                                     {:gen-viz-609 (keyword qname)}
                                                                                                     (let [render-code (get-in % [c 1])
                                                                                                           ;only-view? (vector? render-code)
                                                                                                           is-map? (and (map? render-code) (contains? render-code :view))
                                                                                                           base {:h       hh
                                                                                                                 :w       ww
                                                                                                                 :connection-id panel-conn-id
                                                                                                                 :root    [38 1] ;; needs unique?
                                                                                                                 :tab     "strategic grill locations"
                                                                                                                 :name    rowname
                                                                                                                 :views   {:a-row render-code}
                                                                                                                 :queries {}}
                                                                                                           body (if is-map?
                                                                                                                  (-> base
                                                                                                                      (assoc-in [:views :a-row] (get render-code :view))
                                                                                                                      (assoc-in [:queries] (get render-code :queries)))
                                                                                                                  base)]
                                                                                                       body))]))
                                                                            ;; (ut/tracked-dispatch [::update-workspace [new-keyw] base-map])
                                                                              [re-com/box :size "auto"
                                                                               :height (px (+ row-height 20)) ;"33px" ;"30px"
                                                                               :align :start :justify :start
                                                                               :style {;:margin-top "-8px"
                                                                                 ;:margin-top "0px"
                                                                                       :zoom 0.65
                                                                                 ;:overflow "visible"
                                                                                       :padding-top "0px"}
                                                                               :child (if (= (get-in % [c 0]) :*render*)
                                                                                        [re-com/box
                                                                                         :size "none" :width (px cwidth) :height (px row-height)
                                                                                         :child [honeycomb (keyword rowname) :a-row]]
                                                                                        ; hh ;hh
                                                                                        ; ww

                                                                                        [shape/map-boxes2
                                                                                         (get-in % [c 1])
                                                                                         :inline-render :inline-render [] nil
                                                                                         (if (vector? (get-in % [c 1])) "vector" "map")])]))

                                                                 :else

                                                                 (draggable (sql-spawner-where :where panel-map data-keypath c panel-key row-num)
                                                                            "meta-menu"
                                                                            [re-com/box :size "none"
                                                                             :attr {;:on-mouse-enter (fn [_] (when (and (not (= @hover-field [c panel-key])) hover-field-enable?)
                                                                                    ;                          (reset! hover-field [c panel-key])))
                                                                                    :on-mouse-over (fn [_] (when (and (not (= @hover-field [c panel-key])) hover-field-enable?)
                                                                                                             (reset! hover-field [c panel-key])))
                                                                                    :on-mouse-leave (fn [_] (when (and (not (= @hover-field [c panel-key])) hover-field-enable?)
                                                                                                              (reset! hover-field nil)))}
                                                                         ;:height (px row-height) ;"22px"
                                                                             :class  (when (and (= panel-key (last @hover-field)) @animate?) "zoom-and-fade")
                                                                             :style {:color "inherit"}
                                                                             :child ;(str (get % c))
                                                                             (if (and (integer? (get % c))
                                                                                      (not (cstr/includes? (cstr/lower-case (str c)) "year")))
                                                                               (str (nf (get % c))) (str (get % c)))])))

                                                       ;[re-com/box
                                                       ; :style {:background-color (if (some (fn [x] (= x c)) child-cols)
                                                       ;                             (str (get data-colors (get-in metadata [:fields c :data-type])) 25)
                                                       ;                             "inherit")
                                                       ;         }
                                                       ; :child
                                                         (cond
                                                        ;;  (and (= c :country) (= (get % c) "us"))
                                                        ;;  (do ;(ut/tapp>> [:usa!])
                                                        ;;    [re-com/box :size "auto" :justify :center :align :center ;:height "33px"
                                                        ;;     :child (str (get % c)) :style {:background-color "white"
                                                        ;;                                    :font-family "Source Code Pro"
                                                        ;;                                    :font-weight 700
                                                        ;;                                    :border-radius "15px"
                                                        ;;                                    :border "5px solid red"
                                                        ;;                                    :color "blue" :font-size "27px"}])

                                                           (and (= (get-in metadata [:fields c :data-type]) "rabbit-code") true)
                                                           (let [row-hash (str (hash %) c)
                                                                 hh (js/Math.floor
                                                                     (/ row-height brick-size))
                                                                 ww (js/Math.floor
                                                                     (/ cwidth brick-size))
                                                                 rowname (str "query-preview-block" row-hash)
                                                                 qname (str "query-preview-inviz" row-hash)]
                                                             (when (and (= (get-in % [c 0]) :*render*)
                                                                        (empty? @(ut/tracked-subscribe [::panel-map (keyword rowname)])))

                                                               (ut/tracked-dispatch [::update-workspace [(keyword rowname)]
                                                                                   (ut/postwalk-replacer
                                                                                    {:gen-viz-609 (keyword qname)}
                                                                                    (let [render-code (get-in % [c 1])
                                                                                          ;only-view? (vector? render-code)
                                                                                          is-map? (and (map? render-code) (contains? render-code :view))
                                                                                          base {:h       hh
                                                                                                :w       ww
                                                                                                :connection-id panel-conn-id
                                                                                                :root    [38 1] ;; needs unique?
                                                                                                :tab     "strategic grill locations"
                                                                                                :name    rowname
                                                                                                :views   {:a-row render-code}
                                                                                                :queries {}}
                                                                                          body (if is-map?
                                                                                                 (-> base
                                                                                                     (assoc-in [:views :a-row] (get render-code :view))
                                                                                                     (assoc-in [:queries] (get render-code :queries)))
                                                                                                 base)]
                                                                                      body))]))
                                                             [re-com/box :size "auto"
                                                              :height (px (+ row-height 20)) ;"33px" ;"30px"
                                                              :align :start :justify :start
                                                              :style {:zoom 0.65
                                                                      :padding-top "0px"}
                                                              :child (if (= (get-in % [c 0]) :*render*)
                                                                       [re-com/box
                                                                        :size "none" :width (px cwidth) :height (px row-height)
                                                                        :child [honeycomb (keyword rowname) :a-row]]
                                                                       [shape/map-boxes2
                                                                        (get-in % [c 1])
                                                                        :inline-render :inline-render [] nil
                                                                        (if (vector? (get-in % [c 1])) "vector" "map")])])

                                                           (nil? (get % c))
                                                           [re-com/box
                                                            :child "NULL"
                                                            :attr {:on-context-menu
                                                                   (fn [_] (ut/tracked-dispatch [::conn/cell-click-parameter
                                                                                               [(keyword (str (ut/replacer (str query-key) #":" "") ".*"))
                                                                                                (keyword (ut/replacer (str c) #":" ""))]
                                                                                               (get % c)]))}
                                                                  ;;  :on-mouse-enter (fn [_] (when (and (not (= @hover-field [c panel-key])) hover-field-enable?)
                                                                  ;;                            (reset! hover-field [c panel-key])))
                                                                  ;;  :on-mouse-over (fn [_] (when (and (not (= @hover-field [c panel-key])) hover-field-enable?)
                                                                  ;;                           (reset! hover-field [c panel-key])))
                                                                  ;;  :on-mouse-leave (fn [_] (when (and (not (= @hover-field [c panel-key])) hover-field-enable?)
                                                                  ;;                            (reset! hover-field nil)))

                                                            :style {:opacity 0.35 :font-style "italic"}]

                                                           (and (string? (get % c)) (ut/is-hex-color? (str (get % c))))
                                                           [re-com/box
                                                            :align :center :justify :center
                                                            :child (str (get % c))
                                                            ;; :attr {:on-mouse-enter (fn [_] (when (and (not (= @hover-field [c panel-key])) hover-field-enable?)
                                                            ;;                                  (reset! hover-field [c panel-key])))
                                                            ;;        :on-mouse-over (fn [_] (when (and (not (= @hover-field [c panel-key])) hover-field-enable?)
                                                            ;;                                 (reset! hover-field [c panel-key])))
                                                            ;;        :on-mouse-leave (fn [_] (when (and (not (= @hover-field [c panel-key])) hover-field-enable?)
                                                            ;;                                  (reset! hover-field nil)))
                                                            ;;        }
                                                            :style {:background-color (str (get % c))
                                                                    :color (ut/invert-hex-color (str (get % c)))
                                                                    :font-weight 400}]
                                                                    ;:padding-left     "4px"


                                                           (= false (get % c))
                                                           [re-com/box
                                                            :child (str (get % c))
                                                            ;; :attr {:on-mouse-enter (fn [_] (when (and (not (= @hover-field [c panel-key])) hover-field-enable?)
                                                            ;;                                  (reset! hover-field [c panel-key])))
                                                            ;;        :on-mouse-over (fn [_] (when (and (not (= @hover-field [c panel-key])) hover-field-enable?)
                                                            ;;                                 (reset! hover-field [c panel-key])))
                                                            ;;        :on-mouse-leave (fn [_] (when (and (not (= @hover-field [c panel-key])) hover-field-enable?)
                                                            ;;                                  (reset! hover-field nil)))
                                                            ;;        }
                                                            :style {:background-color "#abc4de44"
                                                                    :padding-left     "4px"}]

                                                           (= true (get % c))
                                                           [re-com/box
                                                            :child (str (get % c))
                                                            ;; :attr {:on-mouse-enter (fn [_] (when (and (not (= @hover-field [c panel-key])) hover-field-enable?)
                                                            ;;                                  (reset! hover-field [c panel-key])))
                                                            ;;        :on-mouse-over (fn [_] (when (and (not (= @hover-field [c panel-key])) hover-field-enable?)
                                                            ;;                                 (reset! hover-field [c panel-key])))
                                                            ;;        :on-mouse-leave (fn [_] (when (and (not (= @hover-field [c panel-key])) hover-field-enable?)
                                                            ;;                                  (reset! hover-field nil)))}
                                                            :style {:background-color "#79167955"
                                                                    :padding-left     "4px"}]

                                                           (and (= c :query_map) (cstr/includes? (get % c) "{:select"))
                                                           (try (str (get-in (read-string (get % c)) [0 :select]))
                                                                (catch :default _ "error w query_map render"))

                                                           (and (integer? (get % c)) (not (cstr/includes? (cstr/lower-case (str c)) "year")))
                                                           [re-com/box
                                                            :attr {:on-context-menu
                                                                   (fn [_] (ut/tracked-dispatch [::conn/cell-click-parameter
                                                                                               [(keyword (str (ut/replacer (str query-key) #":" "") ".*"))
                                                                                                (keyword (ut/replacer (str c) #":" ""))]
                                                                                               (get % c)]))}
                                                                  ;;  :on-mouse-enter (fn [_] (when (and (not (= @hover-field [c panel-key])) hover-field-enable?)
                                                                  ;;                            (reset! hover-field [c panel-key])))
                                                                  ;;  :on-mouse-over (fn [_] (when (and (not (= @hover-field [c panel-key])) hover-field-enable?)
                                                                  ;;                           (reset! hover-field [c panel-key])))
                                                                  ;;  :on-mouse-leave (fn [_] (when (and (not (= @hover-field [c panel-key])) hover-field-enable?)
                                                                  ;;                            (reset! hover-field nil)))

                                                            :child (str (nf (get % c)))]

                                                           :else [re-com/box :child (str (get % c))
                                                                ;:align :start :justify :start
                                                                  :attr {:on-context-menu
                                                                         (fn [_] (ut/tracked-dispatch [::conn/cell-click-parameter
                                                                                                     [(keyword (str (ut/replacer (str query-key) #":" "") ".*"))
                                                                                                      (keyword (ut/replacer (str c) #":" ""))]
                                                                                                     (get % c)]))
                                                                         ;:on-mouse-enter (fn [_] (when (and (not (= @hover-field [c panel-key])) hover-field-enable?)
                                                                         ;                          (reset! hover-field [c panel-key])))
                                                                         :on-mouse-over (fn [_] (when (and (not (= @hover-field [c panel-key])) hover-field-enable?)
                                                                                                  (reset! hover-field [c panel-key])))
                                                                         :on-mouse-leave (fn [_] (when (and (not (= @hover-field [c panel-key])) hover-field-enable?)
                                                                                                   (reset! hover-field nil)))}
                                                                  :size "none"]))
                                                                  ;:style {;:margin-top "-50px"
                                                                  ;      ;:overflow "visible"
                                                                  ;        }

                                                       ; ]

                                        :width        cwidth}))) ;187
                                      ;:width-old (if clicked?
                                      ;         (let [char-width (* (count (str (get clicked-row c))) 12)
                                      ;               width (if (< equal-width-min char-width) char-width equal-width-min)]
                                      ;           width)
                                      ;         equal-width-final)

                    ;;  :on-click-row  #(if @timer-id
                    ;;                    (do
                    ;;                      (js/clearTimeout @timer-id)
                    ;;                      (reset! timer-id nil)
                    ;;                      (ut/tapp>> (str "Double clicked row!"  )))
                    ;;                    (do
                    ;;                      (if (= clicked-row (get @rowset %))
                    ;;                        (ut/tracked-dispatch [::conn/click-parameter data-keypath nil])
                    ;;                        (ut/tracked-dispatch [::conn/click-parameter data-keypath (get @rowset %)]))
                    ;;                      (reset! timer-id (js/setTimeout
                    ;;                                        (fn []
                    ;;                                          (reset! timer-id nil))
                    ;;                                        double-click-timeout))))
                    ;;  :on-click-row (fn [x]
                    ;;                  ;(ut/tapp>> [:click!])
                    ;;                  (if @timer-id ;(and @timer-id @hover-field)
                    ;;                    (let [query-key    (keyword
                    ;;                                        (str (ut/replacer (str query-key) #":" "")
                    ;;                                             "->-"
                    ;;                                             (ut/replacer (str (first @hover-field)) #":" "")))
                    ;;                          base-table    (get-in table-source [0 0])
                    ;;                          is-sub?       (true? (cstr/starts-with? (str base-table) ":query/"))
                    ;;                          field-id      (first @hover-field)
                    ;;                          clicked-panel-key     (last @hover-field)
                    ;;                          agg?          (not (get-in metadata [:fields field-id :group-by?]))
                    ;;                          new-query (cond is-sub?
                    ;;                                          (try (sql-alias-replace-sub base-table) (catch :default _ {:select [["error in sql-alias-replace-sub1" :err]]}))
                    ;;                                          (map? base-table) (try (sql-alias-replace base-table) (catch :default _ {:select [["error in sql-alias-replace-sub2" :err]]}))
                    ;;                                          :else {:select [:*] :from table-source})]
                    ;;                        ;(ut/tapp>> [:double!])
                    ;;                      (ut/tracked-dispatch-sync [::conn/click-parameter data-keypath (get @rowset x)])

                    ;;                       ; (ut/tapp>> [:q-insert table-source (when is-sub? (sql-alias-replace-sub base-table))
                    ;;                       ;        (get-in table-source [0 0])
                    ;;                       ;        ])
                    ;;                      (when agg? ;(and agg? (= panel-key clicked-panel-key))  ;;  (= panel-key (last @hover-field))
                    ;;                        (do
                    ;;                          (js/clearTimeout @timer-id)
                    ;;                          (reset! timer-id nil)
                    ;;                          ;(ut/tapp>> (str "Double click! " @hover-field " " (get-in @rowset [x @hover-field]) " " @hover-field))
                    ;;                          (reset! animate? true)
                    ;;                          (ut/tracked-dispatch [::add-query panel-key query-key new-query])
                    ;;                          (js/setTimeout #(do ;; change view tab
                    ;;                                          ;;(ut/tracked-dispatch [::update-workspace [panel-key :selected-view query-key]])
                    ;;                                            (ut/tracked-dispatch [::select-view panel-key query-key])
                    ;;                                            (reset! animate? false)) 1000)))
                    ;;                      ;(ut/tracked-dispatch [::conn/click-parameter data-keypath (get @rowset x)])
                    ;;                      )

                    ;;                    (do ;; single click
                    ;;                      ;(ut/tapp>> (str "Single click!" x))
                    ;;                        ;(when (nil? @timer-id)
                    ;;                      (ut/tracked-dispatch-sync [::conn/click-parameter data-keypath (get @rowset x)])
                    ;;                      (reset! timer-id (js/setTimeout
                    ;;                                        (fn []
                    ;;                                          (reset! timer-id nil))
                    ;;                                        double-click-timeout)))))

                     :on-click-row (fn [x]
                                     ;(if @timer-id ;(and @timer-id @hover-field)

                                         ;(ut/tracked-dispatch-sync [::conn/click-parameter data-keypath (get @rowset x)])

                                     (if (and (not (get-in metadata [:fields (first @hover-field) :group-by?])) ;agg?
                                              (not (nil? (first @hover-field)))
                                              (= clicked-row (get @rowset x))) ;; already selected

                                       (let [query-key    (keyword
                                                           (str (ut/replacer (str query-key) #":" "")
                                                                "->-"
                                                                (ut/replacer (str (first @hover-field)) #":" "")))
                                             base-table    (get-in table-source [0 0])
                                             is-sub?       (true? (cstr/starts-with? (str base-table) ":query/"))
                                             group-bys  @(ut/tracked-subscribe [::group-bys query-key])
                                             clicked-keyword (keyword (str (ut/replacer query-key #":" "") "/*.clicked"))
                                            ; field-id      (first @hover-field)
                                            ; clicked-panel-key     (last @hover-field)
                                            ; agg?          (not (get-in metadata [:fields field-id :group-by?]))
                                             new-query (cond is-sub?
                                                             (try (sql-alias-replace-sub base-table) (catch :default _ {:select [["error in sql-alias-replace-sub1" :err]]}))
                                                             (map? base-table) (try (sql-alias-replace base-table) (catch :default _ {:select [["error in sql-alias-replace-sub2" :err]]}))
                                                             :else {:select [:*] :from table-source})
                                             new-query (assoc new-query :where [:*when clicked-keyword
                                                                                [:*all= clicked-keyword group-bys]])]
                                             ;;(ut/tapp>> [:safe @(ut/tracked-subscribe [::group-by-intersection query-key query-key])])
                                         (reset! animate? true)
                                         (ut/tracked-dispatch [::add-query panel-key query-key new-query])
                                         (js/setTimeout #(do ;; change view tab
                                                             ;;(ut/tracked-dispatch [::update-workspace [panel-key :selected-view query-key]])
                                                           (ut/tracked-dispatch [::select-view panel-key query-key])
                                                           (reset! animate? false)) 1000))

                                       (ut/tracked-dispatch-sync [::conn/click-parameter data-keypath (get @rowset x)])))

;;  :on-click-row #(if (= clicked-row (get @rowset %))
                    ;;                   (ut/tracked-dispatch [::conn/click-parameter data-keypath nil])
                    ;;                   (ut/tracked-dispatch [::conn/click-parameter data-keypath (get @rowset %)]))
                     ;:fixed-column-border-color "#c53ac5"
                     ; :cell-style {:background-color "#00000011"
                     ;              :padding "0px" :margin "0px"
                     ;              :color "#ffffff72"}
                     :cell-style (fn [row {:keys [id] :as column}]
                                   (let [;this-row (.indexOf @rowset row)
                                         ;cstyle (first-matching-style-cell this-row id)
                                         ;multi-col? (seq (get-in stylers [s :cols]))
                                         ;sel? (true? (and selected? (= id (get @column-selected panel-key))))
                                         row-num (try (.indexOf @rowset row)
                                                      (catch :default _ -1))
                                         cell-alias (keyword (str (-> (str id) (ut/replacer #":" "")) "." row-num))
                                         ;;_ (ut/tapp>> [:id cell-alias parameters-used-in-sql])
                                         cell-used?       (true? (some #(= % cell-alias) parameters-used-in-sql))
                                         multi-cell-used? (true? (some #(and (= (first %) id)
                                                                             (some (fn [x] (= x (get row id))) (last %)))
                                                                       parameters-used-in-cell)) ;; parameters-used-in-cell
                                         sel?           (and selected? @(ut/tracked-subscribe [::column-selected? panel-key query-key id]))
                                         poss-stylers   (filter #(cstr/starts-with? (str %) ":styler_") (keys row))
                                         styler-keys    (sort (remove nil? (for [s poss-stylers] (when (= (get row s) 1)
                                                                                                   (keyword (-> (ut/safe-name s)
                                                                                                                (ut/replacer #"_" "-")
                                                                                                                (ut/replacer #"styler-" "")))))))
                                         heatmap-styles (first (remove nil?
                                                                       (for [s styler-keys]
                                                                         (when (and (some #(= :heatmap %)
                                                                                          (ut/deep-flatten (get-in stylers [s :style])))
                                                                                    (or (= (get-in stylers [s :cols]) id)
                                                                                        (try (some #(= id %) (get-in stylers [s :cols])) (catch :default e false))))
                                                                           s))))
                                         heat-walk-map  (fn [obody] (let [kps       (into {} (for [p (ut/kvpaths
                                                                                                      obody)] {p (get-in obody p)}))
                                                                          logic-kps (into {} (for [[_ v]
                                                                                                   (into {} (filter #(cstr/starts-with? (str (last %)) "[:heatmap") kps))]
                                                                                               (let [[_ scheme depth direction] v]
                                                                                                 {v @(ut/tracked-subscribe [::heatmap-color-val panel-key query-key
                                                                                                                          id (get row id) heatmap-styles
                                                                                                                          scheme depth direction])})))]
                                                                      ;(ut/tapp>> [:=-walk/logic-kps logic-kps kps workspace-params])
                                                                      (ut/postwalk-replacer logic-kps obody)))
                                         styles         (apply merge (for [s styler-keys] (if (or (= (get-in stylers [s :cols]) id)
                                                                                                  (try (some #(= id %) (get-in stylers [s :cols])) (catch :default _ false)))
                                                                                            (get-in stylers [s :style]) {})))
                                         ; styles-walk (if heatmap-styles ;(some #(= :heatmap %) (ut/deep-flatten styles))
                                         ;               {:heatmap @(ut/tracked-subscribe [::heatmap-color-val panel-key query-key
                                         ;                                               id (get row id) heatmap-styles])}
                                         ;               {})
                                         ; styles (ut/postwalk-replacer styles-walk styles)
                                         styles         (if heatmap-styles (heat-walk-map styles) styles)
                                         ; row-styles   (apply merge (for [s styler-keys] (if (or (= (get-in stylers [s :cols]) :*)
                                         ;                                                      (= (get-in stylers [s :cols]) [:*]))
                                         ;                                                (get-in stylers [s :style]) {})))
                                         data-type-color (get @(ut/tracked-subscribe [::conn/data-colors]) (get-in metadata [:fields id :data-type]))
                                         agg? (and (not non-panel?) (not (get-in metadata [:fields id :group-by?])))
                                         used-in-a-query? (true? (some #(= id %) parameters-used-in-sql))
                                         clicked-row? (= row clicked-row)]                  ;; color on row doesnt work, overriding here...
                                   ;;  (when sel? (ut/tapp>> [:color id data-type-color (get-in metadata [:fields id :data-type])  @(ut/tracked-subscribe [::conn/data-colors]) ]))
                                     ;;(ut/tapp>> [:row-id id parameters-used-in-cell row])
                                    ;;  (when (and (= id :country) (= (get row id) "us"))
                                    ;;    (ut/tapp>> [:call id row  ]))
                                    ; (if
                                     (merge

                                      ;(when agg? {:box-shadow "inset 0px 0px 2px 2px #ffffff22"})
                                      ;(when agg? {:box-shadow (str "inset 0 0 4px 2px " (get @(ut/tracked-subscribe [::conn/data-colors]) (get-in metadata [:fields id :data-type])))})

                                      (cond
                                        (or (= row clicked-row)
                                            (and (= panel-key :recos-list*) ;; special case with viz browser
                                                 (= (get row :combo_hash)
                                                    ;@(ut/tracked-subscribe [::conn/clicked-parameter-key [:recos-sys/combo_hash]])
                                                    @(rfa/sub ::conn/clicked-parameter-key-alpha {:keypath [:recos-sys/combo_hash]})
                                                    )))

                                        (if has-rabbit-code?
                                          {:border-top (str "2px solid " selected-background-color) ; "#9973e0"
                                           :border-bottom (str "2px solid " selected-background-color) ; "#9973e0"
                                           :padding          "3px"
                                           :margin           "0px"
                                           :background-color (str selected-background-color 11)}
                                            ;:color            selected-background-color

                                          {:background-color selected-background-color ; "#9973e0"
                                           :padding          "3px"
                                           :margin           "0px"
                                           :color            selected-text-color})

                                             ;cstyle (merge {:background-color "#00000011"
                                             ;               :padding "3px"
                                             ;               :margin "0px"
                                             ;               :color "#ffffff72"} cstyle)
                                        styles (merge  ;row-styles
                                                {;:background-color "#00000011"
                                                 :padding "3px"
                                                 :margin  "0px"
                                                 :color   (str text-color 72)} ;"#ffffff72"
                                                styles)
                                        :else
                                        (let [upstream? (some (fn [x] (= x id)) child-cols)
                                              fcolor    (get @(ut/tracked-subscribe [::conn/data-colors]) (get-in metadata [:fields id :data-type]))]
                                          (merge
                                                 ;row-styles
                                           {:background-color (if upstream?
                                                                (str fcolor (if deeper-upstream? 10 20)) "#00000000")
                                            :padding          "3px"
                                            :margin           "0px"}
                                           (if upstream?
                                             {;:border (str "1px solid " (str fcolor (if deeper-upstream? 15 39)))
                                                    ;:filter "contrast(200%)"
                                              :color        (str text-color)
                                              :filter       "brightness(200%)"
                                              :border-left  (str "1px solid " (str fcolor (if deeper-upstream? 15 39)))
                                              :border-right (str "1px solid " (str fcolor (if deeper-upstream? 15 39)))
                                              :border-top   (str "1px solid " (str fcolor (if deeper-upstream? 15 39)))}
                                             {:border-left  (str "1px solid " text-color "08") ;"1px solid #ffffff08"
                                              :border-right (str "1px solid " text-color "08") ;"1px solid #ffffff08"
                                              :color        (str text-color 72)}))))

                                      (when (and clicked-row? used-in-a-query?)
                                        {:filter "hue-rotate(90deg)"})

                                      (if cell-used?
                                        ;{:filter "saturate(200%)"}
                                        {:color selected-text-color
                                         :border (str "1px solid " (theme-pull :theme/editor-outer-rim-color nil))
                                         :border-bottom (str "2px solid " (theme-pull :theme/editor-outer-rim-color nil))
                                         :background-color (str selected-background-color 52)}
                                        {})
                                      (if multi-cell-used?
                                        {:color selected-text-color
                                         ;:border (str "1px dashed " selected-text-color)
                                         ;:border-bottom (str "2px dashed " selected-text-color)
                                         ;:background-color (str selected-background-color 52)
                                         :background (str "repeating-linear-gradient(45deg, " selected-background-color "55, " selected-background-color "55 10px, " selected-background-color "33 10px, " selected-background-color "33 20px)")}
                                        {})

                                      (if sel?             ;; {:background "repeating-linear-gradient(45deg, #606dbc88, #606dbc88 10px, #46529888 10px, #46529888 20px"
                                         ;;           ;:background (str "repeating-linear-gradient(45deg, " (theme-pull :theme/base-block-color-selected nil)
                                         ;;           ;                 ", " (theme-pull :theme/base-block-color-selected nil) " 20px, #00000099 20px, #00000099 40px")
                                         ;;           ;:background-color
                                         ;;           ;"#000000"
                                         ;;           ;(theme-pull :theme/base-block-color-selected nil)
                                         ;;           }
                                        (merge (theme-pull :theme/grid-selected-column-css nil)
                                               {:border-left (str "2px solid "  data-type-color)
                                                :border-right (str "2px solid "  data-type-color)
                                                :background-color (str data-type-color 15)
                                                :height (px (if (and clicked-row? has-rabbit-code?)
                                                              (- clicked-row-height 5)
                                                              row-height))})

                                        {:height (px (if (and clicked-row? has-rabbit-code?)
                                                       (- clicked-row-height 5)
                                                       row-height))})

                                      ;(when agg? {:color (get @(ut/tracked-subscribe [::conn/data-colors]) (get-in metadata [:fields id :data-type]))})
                                      (when agg? {:text-decoration "underline"}))))               ;; temp

                     :row-height row-height ; 444 ;row-height
                     :max-rows max-rows
                     :max-width (px (+ 25 width-int))
                     :table-row-line-color (str text-color 11) ;;"#ffffff11"
                     ;:row-style {:padding "0px"}
                     :row-style (fn [row {:keys [id] :as column}]
                                  (let [;this-row (.indexOf @rowset row)
                                        ;cstyle (first-matching-style this-row)
                                        poss-stylers (filter #(cstr/starts-with? (str %) ":styler_") (keys row))
                                        styler-keys  (sort (remove nil? (for [s poss-stylers] (when (= (get row s) 1)
                                                                                                (keyword (-> (ut/safe-name s)
                                                                                                             (ut/replacer #"_" "-")
                                                                                                             (ut/replacer #"styler-" "")))))))
                                        styles       (apply merge (for [s styler-keys] (if (or (= (get-in stylers [s :cols]) :*)
                                                                                               (= (get-in stylers [s :cols]) [:*]))
                                                                                         (get-in stylers [s :style]) {})))
                                        base {}] ;; merge for all

                                    ;;(ut/tapp>> [:poss poss-stylers styler-keys stylers styles])
                                    ;(ut/tapp>> [:row id column row])
                                    (cond (and (= row clicked-row) has-rabbit-code?)
                                             ;  (= row clicked-row)
 ; (= (get-in metadata [:fields id :data-type]) "rabbit-code") ;(= (get row :country) "us")
                                          {;:height "299px"
                                           ;:overflow "auto"
                                           ;:color selected-background-color
                                           ;:vertical-align "top"
                                           ;:width "200px"
                                           :height (px clicked-row-height)}
                                          (or (= row clicked-row)
                                              (and (= panel-key :recos-list*) ;; special case with viz browser
                                                   (= (get row :combo_hash)
                                                      ;@(ut/tracked-subscribe [::conn/clicked-parameter-key [:recos-sys/combo_hash]])
                                                      @(rfa/sub ::conn/clicked-parameter-key-alpha {:keypath [:recos-sys/combo_hash]})
                                                      )))
                                          {:background-color selected-background-color ;"#bc798c" ; "#ec8250" grid-selected-background-color
                                           :color            selected-text-color ;"#000000"
                                           :padding          "0px"}
                                          styles (merge {:padding "0px"} styles)
                                          ;cstyle (merge {:padding "0px"} cstyle)
                                          ; {:background-color "darkcyan" :color "black" :padding "0px"}
                                          ;(merge {:padding "0px"} (get-in post-styles [:* :toots2 :styles]))
                                          ; (and (= data-keypath [:blocks-sys]) (cstr/includes? @(ut/tracked-subscribe [::conn/clicked-parameter-key [:blocks-sys/view_names]]) (get-in row [:block_key])))
                                          :else {:padding "0px"})))
                     :table-padding 5
                     ;:fixed-column-count (count fields)
                     :fixed-column-border-color "#00000011"
                     ;:column-border-color "#00000011"
                     :column-header-height 39
                     :column-header-renderer (fn [cols parts sort-by-col]
                                               ;(ut/tapp>> [{:cols cols} {:parts parts} {:sort-by-col sort-by-col}])
                                               [re-com/h-box
                                                ;:height "26px"
                                                :children (for [c cols]
                                                            (let [w          (get c :width)
                                                                  id         (get c :id)
                                                                  dst        (nf (nth (get c :distinct) 1))
                                                                  full-meta? (nth (get c :distinct) 0)
                                                                  inv?       (get c :upstream?)
                                                                  color      (get c :color)
                                                                  typ        (get c :data-type)
                                                                  ;sel? (true? (and selected? (= id (get @column-selected panel-key))))
                                                                  sel?       (and selected? @(ut/tracked-subscribe [::column-selected? panel-key query-key id]))
                                                                  label      (get c :header-label)]
                                                              [re-com/box
                                                               ;:height "22px"
                                                               :attr (if selected?
                                                                       {:on-click #(ut/tracked-dispatch [::column-select panel-key query-key (if sel? nil id)])}
                                                                       {})
                                                               :child (draggable (sql-spawner :field panel-map data-keypath id panel-key)
                                                                                 "meta-menu"
                                                                                 [re-com/v-box
                                                                                  :justify :center
                                                                                  :height "39px"
                                                                                  :style (merge {;:color "#000000" ;(str color 99)
                                                                                                 :color            (str color)
                                                                                                 ;:background-color (str color "10")
                                                                                                 ;:background-color (str color)
                                                                                                 :filter           (when inv? "brightness(200%)")
                                                                                                 :background-color (cond
                                                                                                                     ;sel? (str "#000000")
                                                                                                                     inv? (str color (if deeper-upstream? 15 25))
                                                                                                                     :else "inherit")
                                                                                                 :border           (str "2px solid " color (when (not sel?) "39"))
                                                                                                 :font-weight      700}
                                                                                                (if sel?
                                                                                                  ;;{:background "repeating-linear-gradient(45deg, #606dbc88, #606dbc88 10px, #46529888 10px, #46529888 20px"}
                                                                                                  (theme-pull :theme/grid-selected-column-css nil)
                                                                                                  {}))
                                                                                  :padding "5px"
                                                                                  :children
                                                                                  [[re-com/box
                                                                                    :child (let [uppers?   (ut/all-uppers? (ut/safe-name label))
                                                                                                 spc-per   (if uppers? 11 9) ;; 9 7
                                                                                                 crs       (* spc-per (count label))
                                                                                                 overflow? (> crs w)
                                                                                                 space     (int (/ w spc-per))]
                                                                                             (if overflow? (str (subs (str label) 1 space) "..") (str label)))]
                                                                                   [re-com/h-box
                                                                                    :style {:font-size "9px" :font-weight 700 :opacity 0.9}
                                                                                    :gap "5px"
                                                                                    :children [[re-com/box :child (str typ " - ")]
                                                                                               [re-com/box
                                                                                                :style (if (not full-meta?) {:font-style "italic"} {})
                                                                                                :child (str dst (when (not full-meta?) "*"))]]]]
                                                                                  :width (px w)])]))])
                     :parts {:simple-wrapper
                             {:style {:padding-bottom   "0px"
                                      :height (px (- height-int 60))
                                      :margin-bottom    "-5px"
                                      :filter           (if (and overlay?
                                                                 (and ;(not mad-libs?)
                                                                  (not non-panel?))) ;; non-panel mad-libs we want to keep clean
                                                          "blur(3px) opacity(60%)" "none")
                                      :font-weight      700
                                      ;:vertical-align "top"
                                      :background-color "#00000000"}} ;;; questions!

                             :v-scroll {:style       {:background-color "#00000000"}
                                        :thumb-style {:background-color (str text-color 11) ;; "#ffffff11"
                                                      ;:border "1px solid #ffffff18"
                                                      :drag-color       "#791679"
                                                      :hover-color      "#8c78b789"}}
                             :h-scroll {:style       {:background-color "#00000000" :color "green"}
                                        :thumb-style {:background-color (str text-color 11) ;;"#ffffff11"
                                                      ;:border "1px solid #ffffff18"
                                                      :drag-color       "#791679"
                                                      :hover-color      "#8c78b789"}}}
                     :model rowset]

                    [re-com/h-box
                     :padding "4px"
                     :justify :between                      ;:align :center
                     :children
                     [;; panel-code-box [panel-id key width-int height-int value]
                      (if (and (= @formula-bar data-keypath) selected? (not non-panel?))
                        (let [block-h        (or hh h)
                              block-w        (or ww w)
                              ;label-px-width (* brick-size (js/Math.floor (/ block-w 2.4)))
                              label-px-width (js/Math.floor (* brick-size (- block-w 0.45)))]
                          (ut/tapp>> [:formula-bar-open? panel-key query-key block-h block-w column-selected selected-field-idx ff])
                          [re-com/h-box
                           ;:gap "4px"
                           :size "auto"
                           ;:height "100px"
                           :style {:filter     (if overlay? "blur(3px) opacity(88)" "none")
                                   :margin-top "12px"}
                           :justify :between
                           :children
                           [[re-com/v-box
                             :size "auto"
                             ;:style {:border (str "1px solid " selected-background-color 55)}
                             :children [;; [re-com/box :height "15px"
                                        ;;  :child "field honey-sql" ;:padding "1px"
                                        ;;  :style {:font-size "10px"
                                        ;;          :padding-left "2px"
                                        ;;          :background-color (str selected-background-color 50) ;;"#9973e055"
                                        ;;          :color (str text-color 99) ;;"#ffffff99"
                                        ;;        ;:font-weight 700
                                        ;;          }]
                                        ;[formula-code-box panel-key query-key label-px-width 131 :honey]
                                        [field-code-box panel-key query-key selected-field-idx label-px-width 55 selected-field-code]]]]])
                                        ;;[re-com/box :child "hey!"]
                                        ;; [panel-id query-key idx width-int height-int value]

                            ;; [re-com/v-box
                            ;;  :size "auto"
                            ;;  :style {:border (str "1px solid " selected-background-color 55)}
                            ;;  :children [[re-com/box :height "15px"
                            ;;              :child "field honey-sql" ;:padding "1px"
                            ;;              :style {:font-size "10px"
                            ;;                      :padding-left "2px"
                            ;;                      :background-color (str selected-background-color 50) ;;"#9973e055"
                            ;;                      :color (str text-color 99) ;;"#ffffff99"
                            ;;                    ;:font-weight 700
                            ;;                      }]
                            ;;             ;[formula-code-box panel-key query-key label-px-width 131 :honey]
                            ;;             [field-code-box panel-key query-key selected-field-idx label-px-width 75 selected-field-code]
                            ;;             ;;[re-com/box :child "hey!"]
                            ;;             ;; [panel-id query-key idx width-int height-int value]
                            ;;             ]]

                            ;;   [re-com/v-box
                            ;;    :size "auto"
                            ;;  ;:align :center
                            ;;    :style {:border (str "1px solid " selected-background-color 55) ;; "1px solid #9973e055"
                            ;;            }
                            ;;    :children [[re-com/box :height "11px"
                            ;;                :child "honey-sql map" ;:padding "1px"
                            ;;                :style {:font-size "8px"
                            ;;                        :padding-left "2px"
                            ;;                        :background-color (str selected-background-color 55) ;;"#9973e055"
                            ;;                        :color (str text-color 99) ;;"#ffffff99"
                            ;;                      ;:font-weight 700
                            ;;                        }]
                            ;;               [formula-code-box panel-key query-key label-px-width 131 :honey]]]

                            ;;   [re-com/v-box
                            ;;    :size "auto"
                            ;;  ;:align :center
                            ;;    :style {:border (str "1px solid " selected-background-color 55) ;; "1px solid #9973e055"
                            ;;            }
                            ;;    :children [[re-com/box :height "11px" :child "sql string" ;:padding "1px"
                            ;;                :style {:font-size "8px" :padding-left "2px" ;:margin-top "-2px"
                            ;;                        :background-color (str selected-background-color 55) ;;"#9973e055"
                            ;;                        :color (str text-color 99) ;;"#ffffff99"
                            ;;                      ;:font-weight 700
                            ;;                        }]
                            ;;               [formula-code-box panel-key query-key (- width-int label-px-width) 131 :string]]]

                        (let [;; waitings (filter #(and (> (count (get % :ui-keypath)) 1)
                              ;;                        (cstr/includes? (str (get % :ui-keypath)) (str query-key)))
                              ;;                  (map :message @(ut/tracked-subscribe [::wfx/pending-requests http/socket-id])))

                              ;; single-wait (count (filter #(and (> (count (get % :ui-keypath)) 1)
                              ;;                                  (cstr/includes? (str (get % :ui-keypath)) (str query-key)))
                              ;;                            (map :message @(ut/tracked-subscribe [::wfx/pending-requests http/socket-id]))))
                              reco-wait?  @(ut/tracked-subscribe [::reco-running? query-key :reco])
                              reco-queued?  @(ut/tracked-subscribe [::reco-queued? query-key :reco])]

                              ;; batch-status (into {}
                              ;;                    (for [{:keys [description kit-name name icon]} server-kits
                              ;;                          :let [wait?  @(ut/tracked-subscribe [::reco-running? query-key kit-name])
                              ;;                                queued?  @(ut/tracked-subscribe [::reco-queued? query-key kit-name])]]
                              ;;                      {kit-name {:wait? wait? :queued? queued?}}))

                              ;waits (count waitings)

                          ;(ut/tapp>> [:tt waitings])


                          [re-com/h-box
                           :size "none"
                           :height "10px"
                           :padding "3px"
                           :style {:font-size "12px"
                                 ;:font-weight 700 ;:padding-top "6px"
                                   :filter    (if overlay? "blur(3px) opacity(88)" "none")}
                           :children [[re-com/h-box
                                       :gap "5px"
                                       :children [[re-com/box :child rowcount-string]
                                                  [re-com/gap :size "0px"] ;; the 2 gaps are enough
                                                  [re-com/md-icon-button
                                                   :md-icon-name "zmdi-refresh"
                                                   :on-click #(ut/tracked-dispatch [::conn/clear-query-history query-key])
                                                   :attr (when hover-field-enable?
                                                           {:on-mouse-enter #(swap! db/context-box assoc query-key "execute: (re)run query")
                                                            :on-mouse-leave #(swap! db/context-box dissoc query-key)})
                                                   :class (when single-wait?
                                                            "rotate linear infinite")
                                                   :style {:font-size  "15px"
                                                           :cursor     "pointer"
                                                           :transform-origin "7.5px 11px"
                                                           :opacity    0.5
                                                           :padding    "0px"
                                                           :margin-top "-3px"}]

                                                  (when (and (> full-rowcount 5)  ;; TODO, gets messed up sometimes?
                                                             (not non-panel?))
                                                    [re-com/md-icon-button
                                                     :md-icon-name "zmdi-toll"
                                                 ;;:disabled? true
                                                     :on-click #(ut/tracked-dispatch [::conn/run-sql-deep-meta-for panel-key query-key (sql-alias-replace-sub query)])
                                                     :attr (when hover-field-enable?
                                                             {:on-mouse-enter #(swap! db/context-box assoc query-key "meta: (re)run full counts of unique values for each column")
                                                              :on-mouse-leave #(swap! db/context-box dissoc query-key)})
                                                     :class (when waits?
                                                              "rotate linear infinite")
                                                 ;:class "rotate linear infinite"
                                                     :style {:font-size  "15px"
                                                             :cursor     "pointer"
                                                             :transform-origin "7.5px 11px"
                                                             :opacity    0.5
                                                             :padding    "0px"
                                                             :margin-top "-3px"}])

                                                  (when (not non-panel?)
                                                    [re-com/md-icon-button
                                                     :md-icon-name "zmdi-shape"
                                                     :on-click #(do (swap! db/sniff-deck assoc query-key :reco)
                                                                    (ut/tracked-dispatch [::set-reco-queued query-key :reco])
                                                                    (ut/tracked-dispatch [::conn/clear-query-history query-key]))
                                                     :attr (when hover-field-enable?
                                                             {:on-mouse-enter #(swap! db/context-box assoc query-key "shapes: (re)generate all possible viz combos for this query")
                                                              :on-mouse-leave #(swap! db/context-box dissoc query-key)})
                                                     :class (cond reco-wait? "rotate linear infinite"
                                                                  reco-queued? "rotate-reverse linear infinite"
                                                                  :else nil)
                                                     :style {:font-size  "15px"
                                                             :cursor     "pointer"
                                                             :transform-origin "7.5px 11px"
                                                             :opacity    0.5
                                                           ;:color      "#ffffff"
                                                             :padding    "0px"
                                                             :margin-top "-3px"}])]]

                                      (when (and server-kits (not non-panel?)) ;; (not non-panel?)
                                        [re-com/h-box
                                         :gap "5px"
                                         :children (vec (cons [re-com/gap :size "6px"]
                                                              (vec (for [{:keys [description kit-name name icon]} server-kits
                                                                         :let [wait?  @(ut/tracked-subscribe [::reco-running? query-key kit-name])
                                                                                 ;_ (ut/tapp>> [:kit-name kit-name])
                                                                               queued?  @(ut/tracked-subscribe [::reco-queued? query-key kit-name])]]

                                                                     [re-com/md-icon-button
                                                                      :md-icon-name icon
                                                                        ;:on-click #(ut/tracked-dispatch [::conn/clear-query-history query-key])
                                                                      :on-click #(do (swap! db/sniff-deck assoc query-key kit-name)
                                                                                     (ut/tracked-dispatch [::set-reco-queued query-key kit-name])
                                                                                     (ut/tracked-dispatch [::conn/clear-query-history query-key]))
                                                                      :attr (when hover-field-enable?
                                                                              {:on-mouse-enter #(swap! db/context-box assoc query-key (str "kit-fn: " name " - " description))
                                                                               :on-mouse-leave #(swap! db/context-box dissoc query-key)})
                                                                      :class (cond wait? "rotate linear infinite"
                                                                                   queued? "rotate-reverse linear infinite"
                                                                                   :else nil)
                                                                      :style {:font-size  "15px"
                                                                              :cursor     "pointer"
                                                                              :transform-origin "7.5px 11px"
                                                                              :opacity    0.5
                                                                              :padding    "0px"
                                                                              :margin-top "-3px"}]))))])

                                      (when (not non-panel?)
                                        [re-com/box :size "auto"
                                         :style {:opacity 0.5 :padding-left "20px"}
                                         :child (str ;(count waitings) " "
                                                 (get @db/context-box query-key " "))])]]))
                                                        ; (if (= (last @hover-field) panel-key) (str " " (first @hover-field)) "")

;; (when (and running? (not sys-panel?))
                      ;;   [re-com/box :size "none" :height "9px"
                      ;;    :align :center :justify :center
                      ;;    :width "50px"
                      ;;    :style {:font-weight 700
                      ;;            :opacity     0.4
                      ;;            :padding-top "10px"
                      ;;            :font-size   "10px"}
                      ;;    :child
                      ;;    [:img {:src "images/loading-cropped.gif" :width "19px" :height "19px"}]
                      ;;    ;(str "running")
                      ;;    ])

                      [re-com/h-box
                       :children [(when col-selected?
                                    (let [;heat-simple (into {} (for [[[k1 _] v] (get-in query [:style-rules])] {k1 v}))
                              ;is-heat? (get heat-simple column-selected)
                                          heat-keys (into {} (first (for [[[k1 k2] v]
                                                                          (get-in query [:style-rules])
                                                                          :when (and (cstr/starts-with? (str k2) ":heatmap-")
                                                                                     (= k1 column-selected))]
                                                                      {[k1 k2] v})))
                                          has-one? (not (empty? heat-keys))]
                          ;(ut/tapp>> [:heat-keys heat-keys])
                                      [re-com/h-box
                                       :size "none"
                                       :height "10px"
                                       :padding "3px"
                                       :style {:margin-right "10px" :font-size "12px"}
                                       :gap "5px"
                                       :children [[re-com/md-icon-button
                                                   :md-icon-name "zmdi-fire"
                                                   :on-click #(do (ut/tracked-dispatch [::toggle-heatmap panel-key query-key column-selected])
                                                                  (ut/tracked-dispatch [::column-select panel-key query-key nil]))
                                                   :attr (when hover-field-enable?
                                                           {:on-mouse-enter #(swap! db/context-box assoc query-key "toggle heatmap on this column")
                                                            :on-mouse-leave #(swap! db/context-box dissoc query-key)})
                                                   :style {:font-size  "15px"
                                                           :color (if has-one? "orange" "inherit")
                                                           :cursor     "pointer"
                                                           :transform-origin "7.5px 11px"
                                                           :opacity    0.5
                                                           :padding    "0px"
                                                           :margin-top "-3px"}]

                                                  [re-com/md-icon-button
                                                   :md-icon-name "zmdi-delete" ;; panel-key query-key clause drop-data
                                                   :on-click #(do (ut/tracked-dispatch [::execute-pill-drop panel-key query-key :remove {:drag-body {:target column-selected}}])
                                                                  (ut/tracked-dispatch [::column-select panel-key query-key nil]))
                                                   :attr (when hover-field-enable?
                                                           {:on-mouse-enter #(swap! db/context-box assoc query-key "remove this column from the query")
                                                            :on-mouse-leave #(swap! db/context-box dissoc query-key)})
                                                   :style {:font-size  "15px"
                                               ;:color (if has-one? "orange" "inherit")
                                                           :cursor     "pointer"
                                                           :transform-origin "7.5px 11px"
                                                           :opacity    0.5
                                                           :padding    "0px"
                                                           :margin-top "-3px"}]]]))

;; [re-com/box
                                      ;;  :style {;:margin-top "3px"
                                      ;;          :font-size "15px"}
                                      ;;  :child [:span {:style {:font-family "Pastor of Muppets"}} "Metadat"
                                      ;;          [:span {:style {:font-family "Pastor of Muppets Flipped"}} "A"]]]

;[re-com/box :child "hey"]


                                  (when (or non-panel?
                                            (and (nil? (get @db/context-box query-key))
                                                 (not col-selected?)))

                                    [re-com/h-box
                       ;:gap "3px"
                                     :style {:filter (if overlay? "blur(3px) opacity(88)" "none")}
                                     :children [(when (and has-pages? (not first-page?))

                                                  [re-com/md-icon-button
                                                   :md-icon-name "zmdi-caret-left"
                                                   :on-click #(do (ut/tracked-dispatch [::change-page panel-key query-key (- page-num 1) ttl-pages])
                                                                  (ut/tracked-dispatch [::conn/clear-query-history query-key]))
                                                   :style {:font-size  "22px"
                                                           :cursor     "pointer"
                                                           :opacity    0.5
                                                           :padding    "0px"
                                                           :margin-top "-1px"}])

                                                (when has-pages?
                                                  [re-com/box
                                                   :align :center :justify :center
                                                   :style {:font-size "10px"}
                                                   :child (str page-num " / " ttl-pages)])

                                                (if (and has-pages? (not last-page?))

                                                  [re-com/md-icon-button
                                                   :md-icon-name "zmdi-caret-right"
                                                   :on-click #(do (ut/tracked-dispatch [::change-page panel-key query-key (+ page-num 1) ttl-pages])
                                                                  (ut/tracked-dispatch [::conn/clear-query-history query-key]))
                                                   :style {:font-size  "22px"
                                                           :cursor     "pointer"
                                                           :opacity    0.5
                                                           :padding    "0px"
                                                           :margin-top "-1px"}]
                                                  [re-com/box :child " " :width "22px"])]])

                                    ; [re-com/box
                                    ;  :align :center :justify :center
                                    ;  :child ">"]


                                  ;; (when selected?
                                  ;;   [re-com/box
                                  ;;    :align :center :justify :center
                                  ;;    :style {:opacity 0.6 :padding-left "5px" :padding-right "5px"}
                                  ;;    ;:attr {:on-click #(reset! formula-bar (if (= @formula-bar data-keypath) nil data-keypath))}
                                  ;;    ;:child (str (if (= @formula-bar data-keypath) "ff" "gg"))
                                  ;;    :child (if (= @formula-bar data-keypath)

                                  ;;     [re-com/md-icon-button
                                  ;;      :md-icon-name "zmdi-pizza"
                                  ;;      :on-click #(reset! formula-bar (if (= @formula-bar data-keypath) nil data-keypath))
                                  ;;      :style {:font-size "18px"
                                  ;;              :cursor "pointer"
                                  ;;              :margin-top "12px"
                                  ;;              :opacity 0.5
                                  ;;              :padding "0px"
                                  ;;              ;:margin-top "-1px"
                                  ;;              }]
                                  ;;     [re-com/md-icon-button
                                  ;;      :md-icon-name "zmdi-view-headline"
                                  ;;      :on-click #(reset! formula-bar (if (= @formula-bar data-keypath) nil data-keypath))
                                  ;;      :style {:font-size "22px"
                                  ;;              :cursor "pointer"
                                  ;;              :opacity 0.5
                                  ;;              :padding "0px"
                                  ;;              :margin-top "-1px"
                                  ;;              }]

                                  ;;             )
                                  ;;    ]

                                  ;; ;  (if (= @formula-bar data-keypath)
                                  ;; ;    [re-com/md-icon-button
                                  ;; ;     :md-icon-name "zmdi-pizza"
                                  ;; ;     :on-click #(reset! formula-bar (if (= @formula-bar data-keypath) nil data-keypath))
                                  ;; ;     :style {:font-size "15px"
                                  ;; ;             :cursor "pointer"
                                  ;; ;             :opacity 0.5
                                  ;; ;             :padding "0px"
                                  ;; ;             ;:margin-top "-1px"
                                  ;; ;             }]
                                  ;; ;    [re-com/md-icon-button
                                  ;; ;     :md-icon-name "zmdi-view-headline"
                                  ;; ;     :on-click #(reset! formula-bar (if (= @formula-bar data-keypath) nil data-keypath))
                                  ;; ;     :style {:font-size "15px"
                                  ;; ;             :cursor "pointer"
                                  ;; ;             :opacity 0.5
                                  ;; ;             :padding "0px"
                                  ;; ;             ;:margin-top "-1px"
                                  ;; ;             }])

                                  ;;   )


                                  [re-com/gap :size "0px"]]]]]]]) ;; just in case so we don't render empty child vec
 ;; end of h-box

      [re-com/v-box
       :padding "20px"
       :align :center :justify :center
       :style {:font-size   "18px"
               :opacity     0.2
               :color       (str text-color 77)
               :font-weight 700}
       :children [[re-com/box
                   :child (if (and running? (not sys-panel?))
                            "waiting"
                            "no data")]
                  (when (not (and running? (not sys-panel?)))
                    [re-com/md-icon-button
                     :md-icon-name "zmdi-refresh"
                     :on-click #(ut/tracked-dispatch [::conn/clear-query-history query-key])
                    ;;  :attr (when hover-field-enable?
                    ;;          {:on-mouse-enter #(swap! db/context-box assoc query-key "refresh this row-set from the database")
                    ;;           :on-mouse-leave #(swap! db/context-box dissoc query-key)})
                    ;;  :class (when single-wait?
                    ;;           "rotate linear infinite")
                     :style {:font-size  "15px"
                             :cursor     "pointer"
                             :transform-origin "7.5px 11px"
                             ;:opacity    0.5
                             :padding    "0px"
                             :margin-top "-3px"}])]])))

       ;(str "waiting..." "empty?" (empty? @rowset) "nil?" (nil? @rowset))

      ; [re-com/box :child [:img {:src "images/loading.gif"}]]
      ;[re-com/throbber :size :large]

  ;; (catch js/Object e [re-com.core/v-box
  ;;                     :margin "5px"
  ;;                     :style {:font-color "#c9baba" :border "2px solid orange"}
  ;;                     :gap "12px"
  ;;                     :children
  ;;                     [[re-com/box
  ;;                       :align :center :justify :center
  ;;                       :padding "10px"
  ;;                       :style {:color "#000000" :background-color "orange"
  ;;                               :font-weight 700 :font-size "24px"
  ;;                               ;:font-family "Fira Code"
  ;;                               }
  ;;                       :child "Error rendering v-table grid"]
  ;;                      [re-com/box :child (str e)
  ;;                       :align :center :justify :center
  ;;                       :padding "6px"
  ;;                       :style {;:color "orange"
  ;;                               :font-weight 700 :font-size "20px"
  ;;                               ;:font-family "Fira Code"
  ;;                               :padding-bottom "15px"}]]])


(re-frame/reg-sub
 ::sql-data-table
 (fn [_ [_ panel-key keypath]]
   [magic-table panel-key keypath]))

(re-frame/reg-sub
 ::sql-data-table-magic
 (fn [_ [_ panel-key keypath]]
   [magic-table panel-key keypath]))

(re-frame/reg-sub
 ::sql-data-table-magic2
 (fn [_ [_ panel-key keypath]]
   [re-com/box :child [magic-table panel-key keypath]]))

(re-frame/reg-sub
 ::sql-data-table-editor
 (fn [_ [_ panel-key keypath block-sizes block-width]]
   [magic-table panel-key keypath block-sizes block-width]))

(re-frame/reg-sub
 ::is-single-view?
 (fn [db [_ panel-key]]
   (let [body (get-in db [:panels panel-key :views])]
     (true? (and (not (nil? body))
                 (or (not (map? body))
                     (and (map? body) (some #(= % :select) (keys body)))))))))

(re-frame/reg-sub
 ::multiple-oz-views?
 (fn [db [_ panel-key]]
   (let [views    (get-in db [:panels panel-key :views] [])
          ;v-cnt (count views)
         oz-views (remove nil? (for [[k v] views] (when
                                                   (try (= :vega-lite (first v))
                                                        (catch :default e false)) ;; for legacy "single" views
                                                     ; true
                                                   k)))
         oz-cnt   (count (keys oz-views))]
     (true? (>= oz-cnt 2)))))

(re-frame/reg-sub
 ::layered-oz-views
 (fn [db [_ panel-key]]
   (let [views     (get-in db [:panels panel-key :views] [])
         oz-layers (vec
                    (remove nil? (for [[_ v] views]
                                   (when
                                    (try (= :vega-lite (first v))
                                         (catch :default e false))
                                    {:encoding (get-in v [1 :layer 0 :encoding])
                                     :mark     (get-in v [1 :layer 0 :mark])}))))
         vl-shell  (assoc-in vega-lite-shell [1 :layer] oz-layers)]
     vl-shell)))

(re-frame/reg-sub
 ::has-no-view?
 (fn [db [_ panel-key]]
   (nil? (get-in db [:panels panel-key :views]))))

(re-frame/reg-sub
 ::dynamic-tab-selected
 (fn [db [_ panel-key]]
   (try (let [tab-res    (get-in db [:post-tab panel-key])
              vws        (keys (get-in db [:panels panel-key :views]))
              qys        (keys (get-in db [:panels panel-key :queries]))
              all-tabs   (set (flatten (conj vws qys)))
              rule-tabs  (set (for [e (keys (get-in db [:panels panel-key :tab-rules]))] (first e)))
              tabs-exist (cset/intersection (set (keys tab-res)) all-tabs rule-tabs)
              valids     (into {} (for [t tabs-exist]
                                    {t (apply max (flatten (for [r (get-in tab-res [t])]
                                                             (for [b (last r)] ;; WTF happened with this? no likey
                                                               (last (first b))))))}))
              dsc        (into (sorted-map-by (fn [key1 key2] (compare (key2 valids) (key1 valids)))) valids)
              winnah     (first (first dsc))
              query?     (some #(= winnah %) qys)
              full-alias (keyword (str (if query? "grid/" "view/") (ut/safe-name panel-key) "." (ut/safe-name winnah)))]
           ;valids
          (if query?
            [rc/catch [magic-table panel-key [winnah]]]
             ;; todo, this needs to be more systematic. feels very hacky as a shortcircuit
             ;; should be going through honeycomb. grid should be it's own fn pipeline for query aliases (even though this is drastically less code...)
            full-alias))

        (catch :default e nil))))                          ;; just in case... TODO

(re-frame/reg-sub
 ::view
 (fn [db [_ panel-key view-key]]
   (get-in db [:panels panel-key :views view-key])))

(re-frame/reg-sub
 ::views
 (fn [db {:keys [panel-key]}]
   (let [views      (get-in db [:panels panel-key :views] {})
         ;queries    (get-in db [:panels panel-key :queries] {})
         oz-layers  (vec
                     (remove empty?
                             (remove nil? (for [[_ v] views]
                                            (when
                                             (try (= :vega-lite (first v))
                                                  (catch :default _ false))
                                             {:encoding (get-in v [1 :layer 0 :encoding])
                                              :data     (get-in v [1 :data])
                                              :mark     (get-in v [1 :layer 0 :mark])})))))
         oz-layers? (>= (count oz-layers) 2)
         ;tab-rules? false ;; very old feature... need to revisit or remove completely
         ;(and (not (nil? (get-in db [:panels panel-key :tab-rules])))
         ;                (>= (+ (count queries) (count views)) 2))
         ]
      ;(ut/tapp>> [:pp panel-key oz-layers? (count oz-layers) (assoc-in vega-lite-shell [1 :layer] oz-layers) oz-layers])
     (if (map? views)
       (merge views                                        ;(get-in db [:panels panel-key :views])
              (if oz-layers?
                {:layered-viz
                 (-> vega-lite-shell
                     (assoc-in [1 :layer] oz-layers)
                     (assoc-in [1 :resolve] {:scale {:y "independent"}}))} ;; ex: "resolve": {"scale": {"y": "independent"}}
                {})
              ;; (if tab-rules? {:dyn-tab @(ut/tracked-subscribe [::dynamic-tab-selected panel-key])}
              ;;                  ;[:box :child (str ":" @(ut/tracked-subscribe [::dynamic-tab-selected panel-key]))]
              ;;                {})
              {}
              )
       views))))

(re-frame/reg-sub
 ::queries-reco-status
 (fn [db [_ panel-key]]
   (into {} (for [[p _] (get-in db [:panels panel-key :queries] {})]
              {p
               ;(get-in db [:reco-status p 0])
               (get-in db [:status :reco p])}))))

(re-frame/reg-sub
 ::panel-views
 (fn [db {:keys [panel-key]}]
   (let [v (get-in db [:panels panel-key :views])]
     (if (vector? v) {:base v} v))))

(re-frame/reg-sub
 ::panel-view-keys
 (fn [db {:keys [panel-key]}]
   (let [v (get-in db [:panels panel-key :views])]
     (vec (keys v)))))

(re-frame/reg-sub
 ::selected-view
 (fn [db [_ panel-key]]
   (let [body    (get-in db [:panels panel-key :views])
         queries (get-in db [:panels panel-key :queries])
         view    (get-in db [:panels panel-key :selected-view])
         vq      (conj (conj (into (keys body) (keys queries)) :layered-viz) :dyn-tab)] ;; pseudo tabs
      ;  (if view ;(and view ;; if a selected view exists
      ;           ;(some #(= view %) (into (keys queries) (keys body)))) ;; and that selected view is valid
      ;    view ;; cool, use it - otherwise give us the first view key
      ;    (when (map? body) (nth (keys body) 0)))
     (cond (and view (some #(= % view) vq))
                 ;      ;(merge
                 ;       (conj (keys body) :layered-viz)
                 ;      ;       (keys queries))
                 ;      )
           view
           (map? body) (nth (keys body) 0)
           :else nil))))                                      ;(first queries))

(re-frame/reg-sub
 ::selected-view-alpha
 (fn [db {:keys [panel-key]}]
   (let [body    (get-in db [:panels panel-key :views])
         queries (get-in db [:panels panel-key :queries])
         view    (get-in db [:panels panel-key :selected-view])
         vq      (conj (conj (into (keys body) (keys queries)) :layered-viz) :dyn-tab)] ;; pseudo tabs
     (cond (and view (some #(= % view) vq))
           view
           (map? body) (nth (keys body) 0)
           :else nil))))


(re-frame/reg-sub
 ::selected-view-w-queries
 (fn [db [_ panel-key]]
   (let [body    (get-in db [:panels panel-key :views])
         queries (keys (get-in db [:panels panel-key :queries]))
         view    (get-in db [:panels panel-key :selected-view])]
     (cond view view
           (map? body) (nth (keys body) 0)
           :else (first queries)))))

(re-frame/reg-sub
 ::editor-panel-selected-view
 (fn [db _]
   (let [selected-block (get db :selected-block)
         ;;sql-calls @(ut/tracked-subscribe [::panel-sql-calls selected-block])
         sql-calls @(rfa/sub ::panel-sql-calls {:panel-key selected-block})
         ;views @(ut/tracked-subscribe [::panel-views selected-block])
         views @(rfa/sub ::panel-views {:panel-key selected-block})
         first-data-key (first (keys sql-calls))
         first-view-key (first (keys views))
         data-key (if
                   (get @db/data-browser-query selected-block)
                   (get @db/data-browser-query selected-block)
                   first-data-key)
         data-key (if (nil? data-key) first-view-key data-key)
         what (or (when (some #(= % data-key) (keys sql-calls)) :queries)
                  (when (some #(= % data-key) (keys views)) :views))]
     [what data-key])))

(re-frame/reg-event-db
 ::select-view
  ;(undoable)
 (fn [db [_ panel-key view-key]]
   (assoc-in db [:panels panel-key :selected-view] view-key)))

(re-frame/reg-sub
 ::panel-map
 (fn [db [_ panel-key]]
   (get-in db [:panels panel-key])))

(re-frame/reg-sub
 ::panel-connection-id
 (fn [db [_ panel-key]]
   (get-in db [:panels panel-key :connection-id])))

(re-frame/reg-sub
 ::panel-sql-calls
 (fn [db {:keys [panel-key]}]
   (into {} (for [[k v] (get-in db [:panels panel-key :queries])]
              (when (nil? (find v :vselect)) {k v})))))

(re-frame/reg-sub
 ::panel-sql-call-keys
 (fn [db {:keys [panel-key]}]
   (keys (into {} (for [[k v] (get-in db [:panels panel-key :queries])]
                    (when (nil? (find v :vselect)) {k v}))))))

(re-frame/reg-sub
 ::panel-vsql-calls
 (fn [db [_ panel-key]]
   (into {} (for [[k v] (get-in db [:panels panel-key :queries])]
              (when (not (nil? (find v :vselect))) {k v})))))

(re-frame/reg-sub
 ::panel-vsql-call-keys
 (fn [db [_ panel-key]]
   (keys (into {} (for [[k v] (get-in db [:panels panel-key :queries])]
                    (when (not (nil? (find v :vselect))) {k v}))))))

(re-frame/reg-sub
 ::panel-sql-aliases-in-views
 (fn [db [_ panel-key]]
   (let [all-sql-call-keys (set (keys (into {} (for [[_ v] (get db :panels)]
                                                 (into {} (for [[kk vv] (get v :queries)]
                                                            (when (nil? (find vv :vselect)) {kk vv})))))))
         used-keywords     (set (ut/deep-flatten (get-in db [:panels panel-key :views])))]
     (cset/intersection all-sql-call-keys used-keywords))))

(re-frame/reg-sub
 ::panel-sql-aliases-in-views-body
 (fn [db {:keys [panel-key body]}]
   (let [all-sql-call-keys (set (keys (into {} (for [[_ v] (get db :panels)]
                                                 (into {} (for [[kk vv] (get v :queries)]
                                                            (when (nil? (find vv :vselect)) {kk vv})))))))
         raw-body          (set (ut/deep-flatten body))
         used-keywords     (set (ut/deep-flatten (get-in db [:panels panel-key :views])))
         used              (cset/union raw-body used-keywords)]
     (cset/intersection all-sql-call-keys used))))

(re-frame/reg-sub
 ::panel-parameters-in-views
 (fn [db [_ panel-key]]
   (let [all-click-params   (set (keys (remove empty?
                                               (into {} (for [[k v] (get-in db [:click-param])]
                                                          (into {} (for [[kk _] v]
                                                                     {(keyword (str (ut/safe-name k)))                                 nil ;; reffing the whole click-map
                                                                      (keyword (str (ut/safe-name k) "-parameter"))                    nil ;; reffing the whole click-map
                                                                      (keyword (str (ut/safe-name k) "/" (ut/safe-name kk)))           nil
                                                                      (keyword (str (ut/safe-name k) "-parameter/" (ut/safe-name kk))) nil})))))))
        ;;  used-click-params2 (set (vec (remove nil? (for [k (ut/deep-flatten (get-in db [:panels panel-key :views]))]
        ;;                                              (when
        ;;                                               ;;(and (keyword? k) (cstr/includes? (str k) "/"))
        ;;                                               (ut/process-key k)
        ;;                                                k)))))
         used-click-params  (set (ut/deep-flatten (get-in db [:panels panel-key :views])))]
      ;(ut/tapp>> [panel-key used-click-params used-click-params2])
     (cset/intersection all-click-params used-click-params))))

(re-frame/reg-sub
 ::all-sql-calls
 (fn [db]
   (into {} (for [[_ v] (get db :panels)]
              (into {} (for [[kk vv] (get v :queries)]
                         (when (nil? (find vv :vselect)) {kk vv})))))))

(re-frame/reg-sub
 ::all-sql-call-keys
 (fn [db]
   (keys (into {} (for [[_ v] (get db :panels)]
                    (into {} (for [[kk vv] (get v :queries)]
                               (when (nil? (find vv :vselect)) {kk vv}))))))))

(re-frame/reg-sub
 ::all-vsql-call-keys
 (fn [db]
   (keys (into {} (for [[_ v] (get db :panels)]
                    (into {} (for [[kk vv] (get v :queries)]
                               (when (not (nil? (find vv :vselect))) {kk vv}))))))))

(re-frame/reg-sub
 ::all-vsql-calls
 (fn [db]
   (into {} (for [[_ v] (get db :panels)]
              (into {} (for [[kk vv] (get v :queries)]
                         (when (and (not (cstr/starts-with? (str kk) ":query-preview"))
                                    (not (nil? (find vv :vselect)))) {kk vv})))))))

(re-frame/reg-sub
 ::all-click-params2
 (fn [db]
   (vec (remove empty?
                (for [[k v] (get-in db [:click-param])]
                  (into {} (for [[kk vv] v]
                             {(keyword (str (ut/safe-name k) "/" (ut/safe-name kk))) vv})))))))

(re-frame/reg-sub
 ::all-click-params
 (fn [db]
   (let [all-condis {:condi (into {} (for [[_ v] (get db :panels)]
                                       (into {} (for [[kk vv] (get v :conditionals)]
                                                  {kk vv}))))}
         clicks     (get-in db [:click-param])
         cc         (merge clicks all-condis)]
      ; (ut/tapp>> [:clicks clicks :condis all-condis])
     (vec (remove empty?
                  (for [[k v] cc]
                    (into {} (for [[kk vv] v]
                               {(keyword (str (ut/safe-name k) "/" (ut/safe-name kk))) vv}))))))))

(defn sql-data-boxes-values [keypath clickable? style]
  (let [styles        (if (and (map? style) (not (nil? style))) style {})
        data          @(ut/tracked-subscribe [::conn/sql-data keypath])
        clicked-param @(ut/tracked-subscribe [::conn/clicked-parameter keypath])]
    (vec (for [r data]
           [re-com/box
            :size "auto" :padding "4px"
            :attr (if clickable?
                    ; {}
                    {:on-click #(ut/tracked-dispatch [::click-parameter keypath r])}
                    ; {:on-click (re-com/handler-fn (ut/tracked-dispatch [::conn/click-parameter keypath r]))}
                    {})
            :child (str (vals r))
            :style (merge styles {:cursor           (if clickable? "pointer" "inherit")
                                  :background-color (if (= r clicked-param)
                                                      "grey"
                                                      "inherit")})]))))

(defn sql-data-boxes-values2 [keypath style]
  (let [styles        (if (and (map? style) (not (nil? style))) style {})
        data          @(ut/tracked-subscribe [::conn/sql-data keypath])
        clicked-param @(ut/tracked-subscribe [::conn/clicked-parameter keypath])]
    (vec (for [r data]
           [re-com/box
            :size "auto" :padding "4px"
            :attr {:on-click #(ut/tracked-dispatch [::conn/click-parameter keypath r])}
            :child (str (vals r))
            :style (merge styles {:cursor           "pointer"
                                  :background-color (if (= r clicked-param)
                                                      "grey"
                                                      "inherit")})]))))

(re-frame/reg-sub
 ::keypaths-in-view
 (fn [db [_ panel-key view-key]]
   (let [view                 (if (or (= view-key :base) (nil? view-key))
                                (get-in db [:panels panel-key :views])
                                (get-in db [:panels panel-key :views view-key]))
          ;kvpaths (ut/kvpaths view)
         kvpaths              (ut/keypaths view)           ;(sort (ut/keypaths view))
          ;;  layout-map-scrubbers (vec (map (comp vec drop-last)
          ;;                                 (distinct (map (comp vec drop-last)
          ;;                                                (filter #(and (= (last (drop-last %)) :root)
          ;;                                                              (cstr/includes? (str %) " :panels ")) kvpaths)))))
         layout-map-scrubbers @(ut/tracked-subscribe [::scrub/keypaths-of-layout-panels panel-key view-key])
         kvpaths-vals         (into {} (for [p (sort (into layout-map-scrubbers kvpaths))]
                                         {p (let [v (get-in view p)]
                                              [v (try (scrub/get-type p v) (catch :default e (str e)))])}))]
      ;(ut/tapp>> [:kvpaths kvpaths :kvpaths-vals kvpaths-vals :adds layout-map-scrubbers])
      ;(ut/tapp>> [:keypaths-of-layout-panels @(ut/tracked-subscribe [::keypaths-of-layout-panels panel-key view-key])])
      ;(into (sorted-map) kvpaths-vals)
      ;(sort-by key kvpaths-vals)
      ;(reverse kvpaths-vals)
     kvpaths-vals)))

(re-frame/reg-sub
 ::query-schedule
 (fn [db _]
   (let [sched (get db :sched {})]
     (into {} (for [[k v] sched]
                {(str [:run k]) (- v (get-in db [:re-pollsive.core/polling :counter]))})))))

(re-frame/reg-sub
 ::keypaths-in-rs-values
 (fn [db [_ flow-id bid]]
   (try
     (let [pp (get-in db [:runstreams-lookups flow-id :open-inputs bid :user-input])
           params       (get-in db [:runstreams flow-id :values bid :value]  ; try runstream override
                                (get-in db [:runstreams-lookups flow-id :open-inputs bid :user-input])) ; if not, get user data from server
                                        ;(get-in db [:runstreams-lookups flow-id :open-inputs bid :defaults :*])
 ;; if not, get the default value of the field
           ;params       {:* (or params pp)}
           params       {bid (or params pp)}
           kvpaths      (ut/keypaths params)
           kvpaths-vals (into (sorted-map) (for [p kvpaths]
                                             {p (let [v (get-in params p)]
                                                  [v (try (scrub/get-type p v) (catch :default e (str e)))])}))]
           ;_ (ut/tapp>> [:keypaths-in-rs-values flow-id bid pp params kvpaths kvpaths-vals])

       kvpaths-vals)
     (catch :default e
       (do (ut/tapp>> [:error-in :keypaths-in-rs-values e :passed bid]) {})))))

(re-frame/reg-sub
 ::keypaths-in-flow
 (fn [db [_ bid & [single-input?]]]
   (try
     (let [single-input? (if (nil? single-input?) false true)
           params       (get-in db (if single-input?
                                     [:flows (get db :selected-flow) :map bid :data :user-input]
                                     [:flows (get db :selected-flow) :map bid :data :flow-item :defaults])
                                {})
           params       (if single-input? {:* params} params)
           kvpaths      (ut/keypaths params)
           kvpaths-vals (into (sorted-map) (for [p kvpaths]
                                             {p (let [v (get-in params p)]
                                                  [v (try (scrub/get-type p v) (catch :default e (str e)))])}))]
           ;_ (ut/tapp>> [:keypaths-in-flow  bid  params kvpaths kvpaths-vals])

       kvpaths-vals)
     (catch :default e
       (do (ut/tapp>> [:error-in :keypaths-in-flow e :passed bid]) {})))))

(re-frame/reg-sub
 ::keypaths-in-flow-opts
 (fn [db _]
   (try
     (let [params       (get-in db [:flows (get db :selected-flow) :opts])
           kvpaths      (ut/keypaths params)
           kvpaths-vals (into (sorted-map) (for [p kvpaths]
                                             {p (let [v (get-in params p)]
                                                  [v (try (scrub/get-type p v) (catch :default e (str e)))])}))]
       kvpaths-vals)
     (catch :default e
       (do (ut/tapp>> [:error-in :keypaths-in-flow-opts e]) {})))))

(re-frame/reg-sub
 ::keypaths-in-params
 (fn [db {:keys [key-type]}]
   (try
     (let [params       (get-in db [:click-param key-type])
           kvpaths      (ut/keypaths params)
           kvpaths-vals (into (sorted-map) (for [p kvpaths]
                                             {p (let [v (get-in params p)]
                                                  [v (try (scrub/get-type p v) (catch :default e (str e)))])}))]
       kvpaths-vals)
     (catch :default e
       (do (ut/tapp>> [:error-in :keypaths-in-params e :passed key-type]) {})))))

(re-frame/reg-sub
 ::keypaths-in-query
 (fn [db [_ panel-key query-key]]
   (let [query        (get-in db [:panels panel-key :queries query-key])
          ;kvpaths (ut/kvpaths view)
         kvpaths      (ut/kvpaths query)
         kvpaths-vals (into (sorted-map) (for [p kvpaths]
                                           {p (let [v (get-in query p)]
                                                [v (try (scrub/get-type p v) (catch :default e (str e)))])}))]
      ;(into (sorted-map) kvpaths-vals)
     kvpaths-vals)))

;; (re-frame/reg-sub
;;  ::valid-body-params
;;  (fn [db {:keys [panel-key]}]
;;    (let [dat   (get-in db [:panels panel-key :views])
;;          flats (ut/deep-flatten dat)]
;;       ;(ut/tapp>> [:valids panel-key flats dat])
;;      (vec (remove nil? (for [k flats]
;;                          (when (and (keyword? k) (cstr/includes? (str k) "/")
;;                                     (not (cstr/includes? (str k) ":view/"))
;;                            (not (ut/is-sql-sql-alias? k)))
;;                            k)))))))

;; (re-frame/reg-sub
;;  ::valid-body-params-all-condis
;;  (fn [db _]
;;    (let [dat   (for [[_ v] (get-in db [:panels])] (get v :views))
;;          flats (ut/deep-flatten dat)]
;;       ;(ut/tapp>> [:valids panel-key flats dat])
;;      (vec (remove nil? (for [k flats]
;;                          (when (and (keyword? k) (cstr/includes? (str k) "/")
;;                                     (cstr/starts-with? (str k) ":condi/"))
;;                             ;(not (ut/is-sql-sql-alias? k)
;;                            k)))))))

;; (re-frame/reg-sub
;;  ::valid-body-params-in
;;  (fn [_ {:keys [body]}]
;;    (vec (remove nil? (for [k (ut/deep-flatten body)]
;;                        (when (and (keyword? k) (cstr/includes? (str k) "/")
;;                                   (not (cstr/includes? (str k) ":view/"))) k))))))

;; (re-frame/reg-sub
;;  ::valid-sql-params
;;  (fn [db [_ panel-key]]
;;    (vec (remove nil? (for [k (ut/deep-flatten (get-in db [:panels panel-key :queries]))]
;;                        (when (and (keyword? k) (cstr/includes? (str k) "/")) k))))))



;; (defn process-key [k]
;;   (when (and (keyword? k) (cstr/includes? (str k) "/"))
;;     k))

;; (def process-key-cached (memoize process-key))

(re-frame/reg-sub
 ::valid-body-params
 (fn [db {:keys [panel-key]}]
   (let [dat   (get-in db [:panels panel-key :views])
         flats (ut/deep-flatten dat)]
     (vec (remove nil? (map ut/process-key flats))))))

(re-frame/reg-sub
 ::valid-body-params-all-condis
 (fn [db _]
   (let [dat   (for [[_ v] (get-in db [:panels])] (get v :views))
         flats (ut/deep-flatten dat)]
     (vec (remove nil? (map ut/process-key flats))))))

(re-frame/reg-sub
 ::valid-body-params-in
 (fn [_ {:keys [body]}]
   (vec (remove nil? (map ut/process-key (ut/deep-flatten body))))))

(re-frame/reg-sub
 ::valid-sql-params
 (fn [db [_ panel-key]]
   (vec (remove nil? (map ut/process-key (ut/deep-flatten (get-in db [:panels panel-key :queries])))))))





(defn vsql-map [hm]
  ;(ut/tapp>> [:vselect hm])
  (let [select     (get hm :vselect)                        ;; check if vector
        from       (get hm :from)
        where      (get hm :where)
        group-bys  (get hm :group-by)                       ;; nest-by ?
        ;nest-by (get hm :group-by) ;; nest-by ?
        ;fields (keys (first from))
        rowset     (if (vector? from) from [from])
        ;rowset (if group-bys (group-by group-bys rowset) rowset)
        rowset     (if (and (ut/ne? group-bys) (not (nil? group-bys)))
                     (let [group-by-field (first group-bys)]
                       (doall (for [[k v] (group-by group-by-field rowset)]
                                {group-by-field k :rows v})))
                     rowset)
        inside-job (fn [obody]
                     (let [kps       (into {} (for [p (ut/kvpaths obody)] {p (get-in obody p)}))
                           logic-kps (into {} (for [[_ v]
                                                    (into {} (filter #(cstr/starts-with? (str (last %)) "{:vselect") kps))]
                                                {v (vsql-map v)}))]
                       ;(ut/tapp>> [:inny logic-kps kps])
                       (ut/postwalk-replacer logic-kps obody)))]
    (vec (for [row rowset]
           (do                                              ;(ut/tapp>> [:vsql-loop-row row])
             (vec (for [r (ut/postwalk-replacer row select)]
                    (do                                     ;(when (not (or (string? r) (keyword? r))) (ut/tapp>> [:vsql-loop-w r]))
                      (cond (and (map? r) (:vselect r))
                            (do                             ;(ut/tapp>> [:vsql-recur-fn r])
                              (vsql-map r))
                            (some #(= % :vselect) (ut/deep-flatten r))
                            (do                             ;(ut/tapp>> [:run-inside r])
                              (inside-job r))
                            :else r)))))))))

;(pp/pprint rowset)

;(pp/pprint (let [group-by-field :field1]
;             (for [[k v] (group-by group-by-field rowset)]
;               {group-by-field k :rows v})))

;(pp/pprint (map-honey {:select [:box :child :field2 :style {} :test :field3
;                                :nested2 {:select [:t 123 [:hey "fart" :ttt]] :from :nested}]
;                       :from rowset}))

(defn honeycomb-table [panel-key]
  (fn [x] [magic-table panel-key [x]]))

(defn walk-map-sizes [px-width-int px-height-int pw ph x y & [lay? vega-lite?]]
  (let [px-height-int (if (and (< px-height-int 1) (is-float? px-height-int) (not (nil? ph)))
                        (js/Math.floor (* px-height-int ph)) px-height-int)
        px-width-int  (if (and (< px-width-int 1) (is-float? px-width-int))
                        (js/Math.floor (* px-width-int pw)) px-width-int)

        px-height-int (if (and (= px-height-int 0) (not (nil? ph))) (- ph y 1) px-height-int) ;; stretch val
        px-width-int  (if (= px-width-int 0) (- pw x) px-width-int) ;; stretch val

        px-width-int  (* brick-size px-width-int)           ;; switch back to pixel sizes
        px-height-int (* brick-size px-height-int)
        px-width-int  (if lay? (- px-width-int 65) px-width-int)
        px-height-int (if lay? (- px-height-int 65) px-height-int)]

    {:width-int                px-width-int
     :height-int               px-height-int
     :width-px                 (px px-width-int)
     :height-px                (px px-height-int)
     :panel-width              px-width-int
     :panel-height             (cond (and lay? vega-lite?) "container"
                                     vega-lite? (- px-height-int 20)
                                     :else px-height-int)   ;px-height-int

     ;; TODO this as a deconstructed dynamic key with user-space widths :panel-width+XX(-px) etc
     :panel-height+10          (+ 10 px-height-int)
     :panel-height+20          (+ 20 px-height-int)
     :panel-height+30          (+ 30 px-height-int)
     :panel-height+40          (+ 40 px-height-int)
     :panel-height+50          (+ 50 px-height-int)
     :panel-height+60          (+ 60 px-height-int)
     :panel-height+70          (+ 70 px-height-int)
     :panel-height+80          (+ 80 px-height-int)

     :panel-width+10           (+ 10 px-width-int)
     :panel-width+20           (+ 20 px-width-int)
     :panel-width+30           (+ 30 px-width-int)
     :panel-width+40           (+ 40 px-width-int)
     :panel-width+50           (+ 50 px-width-int)
     :panel-width+60           (+ 60 px-width-int)
     :panel-width+70           (+ 70 px-width-int)
     :panel-width+80           (+ 80 px-width-int)
     :panel-width+90           (+ 90 px-width-int)
     :panel-width+100           (+ 100 px-width-int)

     :panel-height+10-px       (px (+ 10 px-height-int))
     :panel-height+20-px       (px (+ 20 px-height-int))
     :panel-height+30-px       (px (+ 30 px-height-int))
     :panel-height+40-px       (px (+ 40 px-height-int))
     :panel-height+50-px       (px (+ 50 px-height-int))
     :panel-height+60-px       (px (+ 60 px-height-int))
     :panel-height+70-px       (px (+ 70 px-height-int))
     :panel-height+80-px       (px (+ 80 px-height-int))

     :panel-width+10-px        (px (+ 10 px-width-int))
     :panel-width+20-px        (px (+ 20 px-width-int))
     :panel-width+30-px        (px (+ 30 px-width-int))
     :panel-width+40-px        (px (+ 40 px-width-int))
     :panel-width+50-px        (px (+ 50 px-width-int))
     :panel-width+60-px        (px (+ 60 px-width-int))
     :panel-width+70-px        (px (+ 70 px-width-int))
     :panel-width+80-px        (px (+ 80 px-width-int))
     :panel-width+90-px        (px (+ 90 px-width-int))
     :panel-width+100-px       (px (+ 100 px-width-int))

     :panel-width-px           (px px-width-int)
     :panel-height-px          (px px-height-int)

     :half-panel-width         (/ px-width-int 2)
     :half-panel-height        (/ px-height-int 2)

     :quarter-panel-width      (/ px-width-int 4)
     :quarter-panel-height     (/ px-height-int 4)
     :quarter-panel-width-px   (px (/ px-width-int 4))
     :quarter-panel-height-px  (px (/ px-height-int 4))

     :3quarter-panel-width     (* px-width-int 0.75)
     :3quarter-panel-height    (* px-height-int 0.75)
     :3quarter-panel-width-px  (px (* px-width-int 0.75))
     :3quarter-panel-height-px (px (* px-height-int 0.75))

     :half-panel-width-px      (px (/ px-width-int 2))
     :half-panel-height-px     (px (/ px-height-int 2))}))

(re-frame/reg-sub
 ::resolve-view-alias
 (fn [db [_ ref-key & [lw lh]]]                            ;; layout sizes, only used for query "views"
   (let [unref-key-vec (ut/splitter (ut/replacer (ut/safe-name ref-key) #"view/" "") ".")
         block-id      (keyword (first unref-key-vec))
         view-name     (keyword (last unref-key-vec))
         is-query?     (true? (= block-id :query))
         qs            (if is-query? (apply merge (flatten (remove empty? (into [] (for [[pk pv] (get db :panels)] ;; expensive? cache as sub?
                                                                                      ;(for [k (keys (get pv :queries))] {k pk})
                                                                                     (for [k (keys (get pv :queries))] {k pk})))))) {})
         panel-key     (when is-query? (get qs view-name))
         view-body     (if is-query?
                          ;[honeycomb panel-key view-name ]
                         [rc/catch [magic-table panel-key [(str ":" view-name)] ;; to avoid RE-resolving before magic-table gets it "::my-query-name"
                                                          lw lh]]
                         (get-in db [:panels block-id :views view-name]))]
          ;in-layout? ()

      ;(ut/tapp>> [:resolve-view-alias ref-key  block-id is-query? panel-key view-name lw lh])
     view-body)))

(re-frame/reg-sub
 ::layouts-for
 (fn [db [_ panel-key view k]]
   (let [;kvs (ut/kvpaths (get-in db [:panels panel-key :views view])) ;; get nested layouts
          ;layout? (true? (= (get-in db [:panels panel-key :views view 0]) :layout))
         layout-map-pre (get-in db [:panels panel-key :views view 1 :panels])
         pw-int         (get-in db [:panels panel-key :w])
         ph-int         (get-in db [:panels panel-key :h])
         layout-map     (lay/process-layout layout-map-pre pw-int ph-int)]
      ;(ut/tapp>> [:layouts-for panel-key view k layout-map])
     (get layout-map k))))

(defn view-alias-replace [view panel-key selected-view]
  (let [valid-params (vec (remove nil? (for [k (ut/deep-flatten view)]
                                         (when (and (keyword? k) (cstr/starts-with? (str k) ":view/")) k))))
        [ph pw] @(ut/tracked-subscribe [::size panel-key])
        sql-params   (into {} (for [k valid-params]         ;; deref here?
                                (let [deets      @(ut/tracked-subscribe [::layouts-for panel-key selected-view k])
                                      body       @(ut/tracked-subscribe [::resolve-view-alias k (get deets :w) (get deets :h)])
                                      body       (if (nil? body) [re-com/box
                                                                  :align :center :justify :center :size "auto"
                                                                  :child [:div [:span {:style {:color       "pink" :font-size "22px"
                                                                                               :font-family (theme-pull :theme/base-font nil)}}
                                                                                (str selected-view " from " panel-key "?")]
                                                                          [:br] (str " I can't find this view. Halp.")]
                                                                  :width :width-px :height :height-px] body)
                                      vega-lite? (true? (some #(= % :vega-lite) (ut/deep-flatten body)))
                                      size-map   (when (and (ut/ne? deets)
                                                            (not (nil? (get deets :w)))
                                                            (not (nil? (get deets :h))))
                                                   (walk-map-sizes (get deets :w) ;(* brick-size (get deets :w))
                                                                   (get deets :h) ;(* brick-size (get deets :h))
                                                                   pw ph (get deets :x) (get deets :y)
                                                                   true vega-lite?))]
                                  {k (if size-map (ut/postwalk-replacer size-map body) body)})))
                                   ;[honeycomb panel-key selected-view (get deets :h) (get deets :w)] ;; panel-key & [override-view fh fw]
                                   ;[:box :child "turd-nuggets"]

        replaced     (ut/postwalk-replacer sql-params view)]
    ;(ut/tapp>> [:view-alias? panel-key replaced view sql-params ])
    (if (cstr/includes? (str replaced) ":view/")            ;(cstr/includes? (str replaced) ":query/")
      (view-alias-replace replaced panel-key selected-view)
      replaced)))

(re-frame/reg-sub
 ::lookup-panel-key-by-query-key
 (fn [db [_ query-key]]
   (first (remove nil? (for [[k v] (get db :panels)]
                         (when (some #(= query-key %)
                                     (keys (get v :queries))) k))))))

(re-frame/reg-event-db
 ::remove-schedule
 (fn [db [_ query-key]]
   (-> db
       ;(assoc-in [:panels @(ut/tracked-subscribe [::lookup-panel-key-by-query-key query-key]) :name]
       ;          ;(get-in db [:re-pollsive.core/polling :counter])
       ;          (str "turd farm" (rand-int 123))
       ;          )
       ;(ut/dissoc-in [:meta query-key])
       ;(ut/dissoc-in [:post-meta query-key])
       (ut/dissoc-in [:sched query-key]))))

(re-frame/reg-event-db ;; original. works fine. BUT dumps all the query execs into one operation
 ::dispatch-auto-queries
 (fn [db [_]]
   (let [scheds (get db :sched)
         scheds-reverse (doall (into {}
                                     (for [vv (vals scheds)]
                                       {vv (vec (for [[k v] scheds
                                                      :when (= v vv)] k))})))
         ;scheds-reverse {10 [:ufo-sightings-drag-country-415]}
         ticktock (get-in db [:re-pollsive.core/polling :counter])]

     (dorun (doseq [[timer queries] scheds-reverse
                    :when (>= ticktock timer)]
              (doseq [query queries]
                (dorun (let []
                         ;(ut/tapp>> [:running-sched :tick ticktock :timer timer query])
                         (ut/tracked-dispatch [::remove-schedule query])
                         (ut/tracked-dispatch [::conn/clear-query-history query]))))))
    ;;  (ut/tapp>> [:scheds-cron-debug :tick ticktock scheds scheds-reverse])
     db)))


;; 5/8/24 trying to offset the execution a little bit to give the screen time to repaint, etc
;; (re-frame/reg-event-db
;;  ::dispatch-auto-queries
;;  (fn [db [_]]
;;    (let [scheds (get db :sched)
;;          scheds-reverse (doall (into {}
;;                                      (for [vv (vals scheds)]
;;                                        {vv (vec (for [[k v] scheds
;;                                                       :when (= v vv)] k))})))
;;          ticktock (get-in db [:re-pollsive.core/polling :counter])]

;;      (letfn [(process-query [query]
;;                (go
;;                  (ut/tracked-dispatch [::remove-schedule query])
;;                  (ut/tracked-dispatch [::conn/clear-query-history query])
;;                  (<! (async/timeout 1000))))]
;;        (dorun (doseq [[timer queries] scheds-reverse
;;                       :when (>= ticktock timer)]
;;                 (doseq [query queries]
;;                   (process-query query)))))
;;      db)))

(re-frame/reg-event-db
 ::insert-sql-source
 (fn [db [_ query-id sql-source]]
   (assoc-in db [:sql-source query-id] sql-source)))

(re-frame/reg-event-db
 ::check-status
 (fn [db _]
   ;(assoc-in db [:sql-source query-id] sql-source)
   (let [client-name (get db :client-name)]
     ;(ut/tapp>> [:checking-server-status... client-name])
     (ut/tracked-dispatch [::wfx/request :default
                         {:message {:kind         :get-status
                                    :client-name  client-name}
                          :on-response [::http/status-response]
                          ;:on-timeout [::failed-get-users]
                          :timeout     500000}])
     db)))

(re-frame/reg-sub
 ::something-running?
 (fn [db _]
   (let [status (get db :status)]
     ;(ut/tapp>> [:status status])
    ;;  (not (empty?
    ;;        (for [[k [v1 v2]] status
    ;;              :when (not (= v2 :done))] {k {(first v1) v2}})))
     (true? (some #(or (= % :started) (= % :queued)) (ut/deep-flatten status))))))

(re-frame/reg-event-db
 ::prune-alerts
 (fn [db [_ & [all?]]]
   ;(ut/tapp>> [:all? all?])
   (if (not @db/pause-alerts)
     (let [alerts (get db :alerts)
           ticktock (get-in db [:re-pollsive.core/polling :counter])]
       (if all?
         (assoc db :alerts [])
         (assoc db :alerts (vec (filter #(> (get % 3) ticktock) alerts)))))
     db)))

(re-frame/reg-event-db
 ::prune-alert
 (fn [db [_ alert-id]]
   (ut/tapp>> [:prune-alert alert-id])
   (if alert-id
     (let [alerts (get db :alerts)]
       (assoc db :alerts (vec (remove #(= (last %) alert-id) alerts))))
     db)))

(re-frame/reg-event-db
 ::clear-cache-atoms
 (fn [db _]
   (ut/tapp>> [:clearing-cache-atoms! (get db :client-name)])
   (doseq [a [ut/replacer-data ut/replacer-cache ut/deep-flatten-data ut/deep-flatten-cache
              ut/clover-walk-singles-map ut/process-key-cache ut/process-key-tracker ut/compound-keys-cache ut/compound-keys-tracker
              ut/upstream-cache ut/upstream-cache-tracker ut/downstream-cache ut/downstream-cache-tracker
              ut/split-cache ut/split-cache-data ut/extract-patterns-data ut/extract-patterns-cache ut/clean-sql-atom ut/auto-font-atom
              ut/postwalk-replace-data-cache ut/postwalk-replace-cache ut/is-base64-atom ut/is-large-base64-atom
              ut/safe-name-cache ut/format-map-atom ut/body-set-atom ut/data-typer-atom ut/coord-cache]]
     (reset! a {}))
   db))

(re-frame/reg-event-db
 ::purge-cache-atoms
 (fn [db _]
   (let [mem (get db :memory)
         js-heap (str (ut/bytes-to-mb (get mem 1)))
         ;js-heap-int (ut/bytes-to-mb-int (get mem 1))
         ;client-name (get db :client-name)
        ;;  _ (ut/tapp>> [:purge-cache-atoms client-name])
         atom-size-map (ut/calculate-atom-sizes {:ut/replacer-data ut/replacer-data
                                                 :ut/replacer-cache ut/replacer-cache

                                                 :ut/deep-flatten-data ut/deep-flatten-data
                                                 :ut/deep-flatten-cache ut/deep-flatten-cache

                                                 :ut/split-cache ut/split-cache
                                                 :ut/split-cache-data ut/split-cache-data

                                                 :ut/clover-walk-singles-map ut/clover-walk-singles-map

                                                 :ut/process-key-cache ut/process-key-cache
                                                 :ut/process-key-tracker ut/process-key-tracker

                                                 :ut/compound-keys-cache ut/compound-keys-cache
                                                 :ut/compound-keys-tracker ut/compound-keys-tracker

                                                 :ut/extract-patterns-data ut/extract-patterns-data
                                                 :ut/extract-patterns-cache ut/extract-patterns-cache

                                                 :ut/postwalk-replace-data-cache ut/postwalk-replace-data-cache
                                                 :ut/postwalk-replace-cache ut/postwalk-replace-cache

                                                 :ut/upstream-cache ut/upstream-cache
                                                 :ut/upstream-cache-tracker ut/upstream-cache-tracker

                                                 :ut/downstream-cache ut/downstream-cache
                                                 :ut/downstream-cache-tracker ut/downstream-cache-tracker

                                                 :ut/auto-font-atom ut/auto-font-atom

                                                 :ut/subq-panels-alpha ut/subq-panels-alpha
                                                 :ut/subq-mapping-alpha ut/subq-mapping-alpha

                                                 :db/flow-results db/flow-results
                                                 :db/scrubbers db/scrubbers
                                                 :ut/is-base64-atom ut/is-base64-atom
                                                 :ut/is-large-base64-atom ut/is-large-base64-atom
                                                 ;:websocket-fx.core/CONNECTIONS websocket-fx.core/CONNECTIONS
                                                 :ut/safe-name-cache ut/safe-name-cache
                                                 :re-frame.db/app-db re-frame.db/app-db
                                                 :ut/clean-sql-atom ut/clean-sql-atom
                                                 ;:ut/deep-flatten-atom ut/deep-flatten-atom
                                                 :ut/format-map-atom ut/format-map-atom
                                                 :ut/body-set-atom ut/body-set-atom
                                                 :ut/data-typer-atom ut/data-typer-atom
                                                 :ut/coord-cache ut/coord-cache})
                                   ;;ttl (apply + (for [[_ v] atom-size-map] (get v :mb)))
         ttl (-> (apply + (for [[_ v] atom-size-map] (get v :mb)))
                 js/Number.
                 (.toFixed 3)
                 js/parseFloat)]
      (ut/tapp>> [:atom-sizes :js-heap js-heap :atoms-raw-ttl (str ttl "MB")  atom-size-map])
      ;;(js/console.log [:atom-sizes client-name :js-heap js-heap js-heap-int  :atoms-raw-ttl (str ttl "MB")  atom-size-map])
          (do
           ;;     (reset! ut/subscription-counts {}) ;; temp
           ;;     (reset! ut/dispatch-counts {}) ;; temp
           ;;     (reset! ut/simple-subscription-counts [])
           ;;     (reset! ut/simple-dispatch-counts [])
     ;;(ut/tapp>> [client-name :purge-postwalk? (get-in atom-size-map [:ut/postwalk-replace-data-cache :mb]) (> (get-in atom-size-map [:ut/postwalk-replace-data-cache :mb]) 15)])
            (when (> (get-in atom-size-map [:ut/postwalk-replace-data-cache :mb]) 40)
              (ut/purge-postwalk-cache 0.99 200))  ;; keep top 70% of freq cache hits distro

            (when (> (get-in atom-size-map [:ut/split-cache-data :mb]) 40)
              (ut/purge-splitter-cache 0.99 100))

            (when (> (get-in atom-size-map [:ut/replacer-data :mb]) 40)
              (ut/purge-replacer-cache 0.99  50))

            (when (> (get-in atom-size-map [:ut/deep-flatten-data :mb]) 40)
              (ut/purge-deep-flatten-cache 0.99 100))

            (when (> (get-in atom-size-map [:ut/extract-patterns-data :mb]) 40)
              (ut/purge-extract-patterns-cache 0.99 100))

            ;(ut/tapp>> [:hello? "you fucking idiots"])

           ;;     ;(reset! ut/safe-name-cache [])
           ;;     ;(reset! ut/split-cache [])
           ;;     ;(reset! ut/replacer-atom {})
           ;;     ;(reset! ut/clean-sql-atom {})
           ;;     ;(reset! ut/deep-flatten-atom {})
           ;;     ;(reset! ut/format-map-atom {})
           ;;     ;(reset! ut/data-typer-atom {})
           ;;     ;(reset! ut/coord-cache {})
            )
     db)))

(re-frame/reg-event-db
 ::clean-up-reco-previews
 (fn [db [_]]
   (let [ks1      (vec (apply concat (for [k (keys (get-in db [:panels]))] (keys (get-in db [:panels k :queries])))))
         sys-ks   (vec (filter #(or (cstr/ends-with? (str %) "-sys")
                                    (cstr/ends-with? (str %) "-sys2")
                                    (cstr/ends-with? (str %) "-sys*")) (keys (get db :data))))
         ;;buffy-fragments (vec (filter #(cstr/ends-with? (str %) "-sys*") (keys (get db :data))))
         base-ks  (into (vec (keys (get db :base-sniff-queries))) [:reco-counts]) ;; why is reco-counts in here?
         ks       (into base-ks (into ks1 sys-ks))
         ks-all   (keys (get db :data)) ;;; hmmm, curious why I did this initially...
         keepers  (vec (filter #(not (cstr/starts-with? (str %) ":query-preview")) ks))

         ;bye      (filter #(cstr/starts-with? (str %) ":query-preview") ks)
        ;;  bye      (vec (cset/difference (set ks) (set ks-all)))
         run-it?  (or (and (nil? @mad-libs-view) (not (= @db/editor-mode :vvv)) (not (= (count ks-all) (count ks))))
                      ;(> (count (filter #(cstr/ends-with? (str %) "-sys*") keepers)) 0)
                      (and (get db :flow?) (get db :flow-editor?)))

         ]

    ;; (ut/tapp>> [:clean-up-on-isle-4 client-name run-it? :before (count ks-all) :after (count ks) :remove bye  :keep keepers])
    ;;  (ut/tapp>> [:ut-atom-sizes
    ;;         (count (keys @ut/replacer-atom)) (count (keys @ut/clean-sql-atom))
    ;;         (count (keys @ut/deep-flatten-atom)) (count (keys @ut/format-map-atom))
    ;;         (count (keys @ut/data-typer-atom))
    ;;         (count (keys @ut/coord-cache))
    ;;         ;(take 40 (sort (keys @ut/replacer-atom)))
    ;;         ;(into (sorted-map) @ut/replacer-atom)
    ;;         client-name
    ;;         ])

       ;; expensive, should do this less often or only on demand like in debug viewer

;;; also remembner we are using memoize on extract-patterns in ut, which might get large and is unclearable !! 5/7/24

    ;;  (reset! ut/format-map-atom {})
    ;;  (ut/tapp>> [:reset-format-map-atom.])


     (if run-it?
       (-> db
           (ut/dissoc-in [:panels nil]) ;; garbage keys created by external edit with bad files
           (assoc :panels (select-keys (get db :panels) (filter #(not (cstr/starts-with? (str %) ":query-preview")) (keys (get db :panels)))))
           (assoc :data (select-keys (get db :data)
                                     ;;(filter #(not (cstr/ends-with? (str %) "-sys*")) keepers)
                                     keepers))
           (assoc :meta (select-keys (get db :meta) keepers))
           (assoc :orders (select-keys (get db :orders) keepers))
           (assoc :post-meta (select-keys (get db :post-meta) keepers))
           (assoc :sql-str (select-keys (get db :sql-str) keepers))
           (assoc :sql-source (select-keys (get db :sql-source) keepers))
           (assoc :query-history (select-keys (get db :query-history)
                                              (filter #(not (cstr/ends-with? (str %) "-sys*")) keepers)))
                                              ;; we DO want to rerun the buffy fragments used in flow UI
           (assoc :click-param (into {} (for [[k v] (get db :click-param)
                                              :when (and (not (cstr/starts-with? (str k) ":kit-"))
                                                         (not (nil? k))
                                                         (not (nil? v)))] {k v})))
           (ut/dissoc-in [:panels "none!"])
           (ut/dissoc-in [:panels :queries])
           (ut/dissoc-in [:panels :views])
           (ut/dissoc-in [:panels nil]))
       ;db
       (-> db
           (ut/dissoc-in [:panels nil])
           (ut/dissoc-in [:panels "none!"])
           (ut/dissoc-in [:panels :queries])
           (ut/dissoc-in [:panels :views])
           ;(ut/dissoc-in [:click-param nil])
           (assoc :click-param (into {} (for [[k v] (get db :click-param)
                                              :when (and (not (cstr/starts-with? (str k) ":kit-"))
                                                         (not (nil? k))
                                                         (not (nil? v)))] {k v}))))))))  ;; garbage keys created by external edit with bad files


(re-frame/reg-sub
 ::get-combo-rows
 (fn [db [_ panel-id [combo-hash table-name]]]
   (let [existing    (get-in db [:data :mad-libs-viz panel-id table-name])
         running?    (fn [kp] (some                        ;#(= (first %) panel-id)
                               #(= % kp)
                               (for [e
                                     ;;@(ut/tracked-subscribe [::wfx/pending-requests http/socket-id])
                                     @(rfa/sub ::http/pending-requests {:socket-id http/socket-id})
                                     ]
                                 (let [p (get-in e [:message :ui-keypath])]
                                   p))))
         reco-counts @(ut/tracked-subscribe [::query-reco-counts table-name])]
      ;(ut/tapp>> [:mad-libs [combo-hash table-name] reco-counts])
      ;;  (ut/tapp>> [:yy (for [e @(ut/tracked-subscribe [::wfx/pending-requests http/socket-id])]
      ;;               (let [p (get-in e [:message :ui-keypath])]
      ;;                 p))])
     ;(ut/tapp>> [:heehaw (empty? (get-in db [:data :viz-shapes])) (not (running? [:viz-shapes])) ])
     (when (and (empty? (get-in db [:data :viz-shapes])) (not (running? [:viz-shapes])))
       ;(do (ut/tapp>> "hey?")
       (conn/sql-data [:viz-shapes]
                      {:select [:axes_logic :base_score
                                :connection_id :library_shapes :selected_view
                                :run_id :shape_name :sql_maps]
                       :from   [[:rule_maps_viz_shapes :uu490]]})) ;)

     (when (and (empty? existing) (> reco-counts 0) (not (running? [:mad-libs-viz panel-id table-name])))
       (conn/sql-data [:mad-libs-viz panel-id table-name]
                      {:select [:*]
                        ;:select-distinct [:talias :field_name :walk1 :walk2 :walk3 :walka :shape_name :combo_hash :table_name :score0 :score1 :score2]
                       :where  [:and
                                [:in :context_hash         ;; self sub-q
                                 {:select [:context_hash]
                                  :from   [[:combo_rows :uu2434aa]]
                                  :where  [:and [:= :combo_hash combo-hash]]}
                                            ;[:= :shape_name shape-name]
                                 :tete]
                                 ;;  [:in :context_hash ;; self sub-q
                                 ;;   {:select [:context_hash]
                                 ;;    :from [[:combo_rows :uu2434aa]]
                                 ;;    :where [:and [:= :combo_hash combo-hash combo-hash]
                                 ;;                                          ;[:= :shape_name shape-name]
                                 ;;            ]}:tete1]
                                [:in :shape_name           ;; self sub-q
                                 {:select [:shape_name]
                                  :from   [[:combo_rows :uu2434aa]]
                                  :where  [:and [:= :combo_hash combo-hash]]}
                                            ;[:= :shape_name shape-name]
                                 :tete2]]
                       :from   [[:combo_rows :uu2434aa]]}))
     (get-in db [:data :mad-libs-viz panel-id table-name]))))

(re-frame/reg-sub
 ::get-mad-libs-options
 (fn [db [_ panel-id src-table-id-str combo-hash]]
    ;(ut/tapp>> [:get-mad-libs-options panel-id src-table-id-str])
   (let [rows          (get-in db [:data :mad-libs-viz panel-id src-table-id-str])
         shape-name    (get (first rows) :shape_name)
         current-combo (filter #(= (get % :combo_hash) combo-hash) rows)]
      ;(ut/tapp>> [:rows (first rows)])
     {:shape_name    shape-name
      :current-combo (into {} (for [[k v] (group-by :axes current-combo)] {k (first v)}))
      :data          (group-by (juxt :talias :field_name :walk1 :walk2 :walk3 :walka :axes) rows)})))
       ;:data rows

(re-frame/reg-sub
 ::lookup-combo-hash2                                      ;; old - was converted into a reg-event
 (fn [db [_ panel-id src-table-id-str shape-name axes-map]]
   (let [;hm (str (hash axes-map))
         running? (fn [kp] (some                           ;#(= (first %) panel-id)
                            #(= % kp)
                            (for [e
                                  ;;@(ut/tracked-subscribe [::wfx/pending-requests http/socket-id])
                                  @(rfa/sub ::http/pending-requests {:socket-id http/socket-id})
                                  ]
                              (let [p (get-in e [:message :ui-keypath])]
                                p))))
         kp1      [:data :mad-libs-viz2 panel-id src-table-id-str shape-name]
         kp2      (vec (remove #(= % :data) kp1))          ;; [:mad-libs-viz2 panel-id src-table-id-str shape-name hm] ;; kp w/o :data prefix
         have-it? (= (get (first (get-in db kp1)) :key_hashes) (pr-str axes-map))
         lk-sql   {:select [:*] :from [:combos]
                   :where  [:and
                            [:= :key_hashes (pr-str axes-map)]
                            [:= :table_name src-table-id-str]
                            [:= :shape_name shape-name]]}]
                    ;:where [:= :key_hashes_hash hm]
                                      ; [rows (get-in db [:data :mad-libs-viz panel-id src-table-id-str])]
      ;(ut/tapp>> [:lookup kp2 :running? (not have-it?) (get (first (get-in db kp1)) :key_hashes) (pr-str axes-map)])
      ;(filter #(= (sort (edn/read-string (get % :key_hashes))) (sort axes-map)) rows)
     (when (and (not have-it?) (not (running? kp2)))
       (conn/sql-data kp2 lk-sql))

     (get-in db kp1))))

(re-frame/reg-sub
 ::lookup-combo-hash
 (fn [db [_ panel-id src-table-id-str shape-name axes-map]]
   (let [kp1 [:data :mad-libs-viz2 panel-id src-table-id-str shape-name]]
     (get-in db kp1))))

(defn prepare-combo-viz [reco-viz reco-query reco-condis]   ;; dupe of part of x TODO
  (cond (= (first (read-string reco-viz)) :vega-lite)

        (let [incomingv     (read-string reco-viz)          ;; read-string will ruin my internal namespaced keywords
              ffromv        (-> (get-in incomingv [1 :data :values])
                                (ut/replacer "_" "-")
                                (ut/replacer "query/" ""))
              original-conn @(ut/tracked-subscribe [::lookup-connection-id-by-query-key (keyword ffromv)])
              view          (-> incomingv
                                (assoc-in [1 :data :values] :query-preview)
                                (assoc-in [1 :config] :theme/vega-defaults)
                                (assoc-in [1 :width] "container") ;:panel-width)
                                (assoc-in [1 :height] :panel-height)
                                (assoc-in [1 :padding] 4)
                                (assoc-in [1 :background] "transparent"))
              q-data        (read-string reco-query)
              incoming      (first q-data)                  ;; read-string will ruin my internal namespaced keywords
              ffrom         (ut/replacer (first (get incoming :from)) "_" "-")
              query         (vec (for [q q-data] (assoc q :from [(keyword ffrom)])))
              query         (ut/postwalk-replacer {[[:sum :rows]] [:count 1]} query)]

          {:view   view
           :query  query
           :condis reco-condis
           :conn   original-conn})

        :else (let [view          (read-string reco-viz)    ;; read-string will ruin my internal namespaced keywords
                    q-data        (read-string reco-query)
                    incoming      (first q-data)            ;; read-string will ruin my internal namespaced keywords
                    ffrom         (ut/replacer (first (get incoming :from)) "_" "-")
                    original-conn @(ut/tracked-subscribe [::lookup-connection-id-by-query-key (keyword (last (ut/splitter ffrom "/")))])
                    query         (vec (for [q q-data]
                                         (if (nil? (find q :vselect))
                                           (assoc q :from [(keyword ffrom)]) q)))
                    query         (ut/postwalk-replacer {[:sum :rows] [:count 1]} query)]

                ;(ut/tapp>> [:rec-preview-block 2 view query reco-condis original-conn reco-selected])

                {:view   view
                 :query  query
                 :condis reco-condis
                 :conn   original-conn})))

(defn modify-combo-viz [viz query condis connection-id combohash combo-name shape-name single? table-name-str] ;;; dupe of insert code!!!
  (let [;table-name @(ut/tracked-subscribe [::conn/clicked-parameter-key (if panel?
        ;                                                                [:viz-tables-sys/table_name]
        ;                                                                [:viz-tables-sys2/table_name])])
        ;table-name (if (keyword? table-name) (ut/replacer (ut/replacer (str table-name) ":" "") "-" "_") table-name)
        condi-keys  (try (keys (read-string condis)) (catch :default _ []))
        condi-walks (into {}
                          (for [c condi-keys]
                            (let [randy (rand-int 999)]
                              {c (keyword (str "c" randy "-" (ut/safe-name c)))
                               (keyword (str "condi/" (ut/safe-name c)))
                               (keyword (str "condi/c" randy "-" (ut/safe-name c)))})))
        queries     (into {} (for [q (range (count query))]
                               {(keyword (if (= q 0) (if (not single?)
                                                       (str "query-preview" combohash) "query-preview")
                                             (str (if (not single?)
                                                    (str "query-preview" combohash "-") "query-preview-")
                                                  (+ 1 q))))
                                (nth query q)}))
        queriesrep  (into {} (for [q (range (count query))]
                               {(keyword (if (= q 0) "query-preview"
                                             (str "query-preview-"
                                                  (+ 1 q))))
                                (keyword (if (= q 0)
                                           (str "query-preview" combohash)
                                           (str (str "query-preview" combohash "-") (+ 1 q))))}))
        base-map    {:mad-libs-combo-hash [combohash table-name-str]
                     :name                (str shape-name " - " combo-name " - " (rand-int 1234))
                     :connection-id       connection-id
                     :views               (if (map? viz) viz {:oz viz})
                     :queries             queries}
        base-map    (if (nil? condis) base-map
                        (assoc base-map :conditionals (read-string condis)))
        base-map    (ut/postwalk-replacer condi-walks base-map)
        base-map    (if (not single?) (ut/postwalk-replacer queriesrep base-map) base-map)]
    base-map))

(defn final-combo-viz [ddata panel-key query-keys]
  (let [;; we need to change all the generic aliases into real rando keys
        ;; inlcuding potential value aliases :query-key/field.row combo keys
        req-name-str     (ut/replacer (str panel-key) #":" "") ;OLD req-name-str (str "block-" (rand-int 999))
        ;ddata @(ut/tracked-subscribe [::panel-map :reco-preview])
        poss-value-walks (distinct (filter #(or (cstr/starts-with? (str %) ":query-preview")
                                                (cstr/starts-with? (str %) ":panel-key"))
                                           (ut/deep-flatten ddata)))
        key-walk1        (filter #(not (cstr/includes? (str %) "/")) poss-value-walks)
        ;key-walk (merge (into {} (for [k key-walk1]
        ;                           {k (keyword (str "gen-viz-" (rand-int 1234)))}))
        ;                {:panel-key (keyword req-name-str)})
        key-walk         (merge                             ;; use old keys (since we should always already have the exact number)
                          (into {} (for [idx (range (count key-walk1))
                                         :let [k (get (vec key-walk1) idx)]] {k (get (vec query-keys) idx)}))
                          {:panel-key (keyword req-name-str)})
        value-walk1      (filter #(cstr/includes? (str %) "/") poss-value-walks)
        value-walk       (into {} (for [v value-walk1] (let [vv   (ut/safe-name v)
                                                             vs   (ut/splitter vv "/")
                                                             qstr (ut/safe-name (first (ut/postwalk-replacer key-walk [(keyword (first vs))])))
                                                             vstr (last vs)]
                                                         (ut/tapp>> [:value-walk vv vs qstr vstr v (keyword (str qstr "/" vstr))])
                                                         {v (keyword (str qstr "/" vstr))})))]
    (ut/tapp>> [:poss-value-walks poss-value-walks :key-walk key-walk :value-walk value-walk :query-keys query-keys])
    (merge (ut/postwalk-replacer (merge key-walk value-walk) ddata))))

(re-frame/reg-event-db
 ::get-combo-hash
 (fn [db [_ panel-id src-table-id-str shape-name axes-map]]
   (let [hm       (str (hash axes-map))
         running? (fn [kp] (some                           ;#(= (first %) panel-id)
                            #(= % kp)
                            (for [e
                                  ;;@(ut/tracked-subscribe [::wfx/pending-requests http/socket-id])
                                  @(rfa/sub ::http/pending-requests {:socket-id http/socket-id})
                                  ]
                              (let [p (get-in e [:message :ui-keypath])]
                                p))))
         kp1      [:data :mad-libs-viz2 panel-id src-table-id-str shape-name hm]
         kp2      (vec (remove #(= % :data) kp1))          ;; [:mad-libs-viz2 panel-id src-table-id-str shape-name hm] ;; kp w/o :data prefix
         exists?  (= (get (first (get-in db kp1)) :key_hashes) (pr-str axes-map))
         lk-sql   {:select [:*] :from [:combos]
                   :where  [:and
                            [:= :key_hashes (pr-str axes-map)]
                            [:= :table_name src-table-id-str]
                            [:= :shape_name shape-name]]}]
      ;(ut/tapp>> [:lookup kp2 :running? (not exists?) (get (first (get-in db kp1)) :key_hashes) (pr-str axes-map)])
      ;(filter #(= (sort (edn/read-string (get % :key_hashes))) (sort axes-map)) rows)
     (when (and (not exists?) (not (running? kp2)))
       (conn/sql-data kp2 lk-sql))

     (if (not exists?)
       (do (reset! db/mad-libs-waiting-room axes-map)
           (go
             (<! (async/timeout 1000))
             (do
               (ut/tracked-dispatch [::post-process-combo-hash panel-id src-table-id-str shape-name kp1])
               (reset! db/mad-libs-waiting-room nil))))
       (ut/tracked-dispatch [::post-process-combo-hash panel-id src-table-id-str shape-name kp1]))

     db)))

(re-frame/reg-event-db
 ::post-process-combo-hash
 (undoable)
 (fn [db [_ panel-id src-table-id-str shape-name kp1]]
   (let [new-combo-row (get-in db kp1)
          ;row (first (get-in db kp1))
         combo-name    (get (first new-combo-row) :combo_edn)
         combo-hash    (get (first new-combo-row) :combo_hash)
         {:keys [view query condis conn error]} (try
                                                  (prepare-combo-viz (get (first new-combo-row) :viz_map "[]")
                                                                     (get (first new-combo-row) :query_map "[]")
                                                                     (get (first new-combo-row) :conditionals ""))
                                                  (catch :default e {:error (str "error: " e)}))
         new-base-map  (modify-combo-viz view query condis conn combo-hash combo-name shape-name false src-table-id-str)
         query-keys    (keys (get-in db [:panels panel-id :queries])) ;; existing keyword aliases
         final-map     (final-combo-viz new-base-map panel-id query-keys)]
     (ut/tapp>> [:new-base-map new-base-map
            :final-combo-viz final-map
            :new-combo-block [view query condis conn error]
            :new-combo-row (first new-combo-row)])
     (if (not error)
        ;(assoc-in db [:panels panel-id :name] (str (get (first (get-in db kp1)) :combo_edn)))
       (assoc-in db [:panels panel-id] (merge (get-in db [:panels panel-id]) final-map))
       db))))
      ;;db

;; (re-frame/reg-event-db
;;  ::get-combo-hash2
;;  (fn [db [_ panel-id src-table-id-str shape-name axes-map]]
;;    (let [hm (str (hash axes-map))
;;          running? (fn [kp] (some ;#(= (first %) panel-id)
;;                             #(= % kp)
;;                             (for [e @(ut/tracked-subscribe [::wfx/pending-requests http/socket-id])]
;;                               (let [p (get-in e [:message :ui-keypath])]
;;                                 p))))
;;          kp1 [:data :mad-libs-viz2 panel-id src-table-id-str shape-name hm]
;;          kp2 (vec (remove #(= % :data) kp1)) ;; [:mad-libs-viz2 panel-id src-table-id-str shape-name hm] ;; kp w/o :data prefix
;;          exists? (= (get (first (get-in db kp1)) :key_hashes) (pr-str axes-map))
;;          lk-sql {:select [:*] :from [:combos]
;;                  :where [:and
;;                          [:= :key_hashes (pr-str axes-map)]
;;                          [:= :table_name src-table-id-str]
;;                          [:= :shape_name shape-name]]}]
;;      (when (and (not exists?) (not (running? kp2)))
;;        (conn/sql-data kp2 lk-sql))

;;      (let []
;;      (some-other-post-processing-BEFORE-assoc)

;;      (assoc-in db [:panels panel-id :name] (str (get (first (get-in db kp1)) :combo_edn))
;;                ))))

(re-frame/reg-sub
 ::get-viz-shape
 (fn [db [_ shape-name]]
    ;(ut/tapp>> [:get-mad-libs-options panel-id src-table-id-str])
   (first (get (group-by :shape_name (get-in db [:data :viz-shapes])) shape-name))))

(re-frame/reg-sub
 ::query-reco-counts
 (fn [db [_ query-id]]
   (let [query-id-str (-> (str query-id)
                          (ut/replacer "-" "_")
                          (ut/replacer ":" ""))]
     (get (first (filter #(= (get % :table_name) query-id-str) (get-in db [:data :reco-counts]))) :cnt 0))))

(re-frame/reg-event-db
 ::update-metadata
 (fn [db [_]]
   (let [panels      (into {} (for [[k v] (get db :panels)
                                    :when (and (not (cstr/includes? (str k) "query-preview"))
                                               (not (cstr/includes? (str k) "reco-preview-")))] {k v}))
         lkp         (fn [query-key]
                       (first (remove nil? (for [[k v] panels]
                                             (when (some #(= query-key %)
                                                         (keys (get v :queries))) k)))))
          ;selected-block (get db :selected-block)
          ;sel-queries (get-in db [:panels selected-block :queries])
         all-queries (into {} (for [[_ v] panels]
                                (into {} (for [[kk vv] (get v :queries)]
                                           (when
                                            (and (nil? (find vv :vselect))
                                                 (nil? (find vv :transform-select)))
                                            {kk vv})))))
         data-hash   (hash (get db :data))]
          ;connection-id (get-in db [:panels selected-block :connection-id])

     (when (not (= data-hash (get db :data-hash)))
       (conn/sql-data [:reco-counts] {:select   [:table_name ;; too expensive to run every 5 seconds?????
                                                 [[:count [:distinct :combo_hash]] :cnt]]
                                      :from     [[:combo_rows :uu2434aa]]
                                      :group-by [:table_name]}))
     (doseq [[k v] all-queries]
       (conn/sql-deep-meta [k] (sql-alias-replace-sub v)
                           (get-in db [:panels (lkp k) :connection-id])))
     (assoc db :data-hash data-hash))))


;; (re-frame/reg-event-db
;;  ::update-metadata
;;  (fn [db [_]]
;;    (let [panels      (into {} (for [[k v] (get db :panels)
;;                                     :when (and (not (cstr/includes? (str k) "query-preview"))
;;                                                (not (cstr/includes? (str k) "reco-preview-")))] {k v}))
;;          lkp         (fn [query-key]
;;                        (first (remove nil? (for [[k v] panels]
;;                                              (when (some #(= query-key %)
;;                                                          (keys (get v :queries))) k)))))
;;          all-queries (into {} (for [[_ v] panels]
;;                                 (into {} (for [[kk vv] (get v :queries)]
;;                                            (when
;;                                             (and (nil? (find vv :vselect))
;;                                                  (nil? (find vv :transform-select)))
;;                                              {kk vv})))))
;;          all-queries (merge all-queries (get db :sql-source))
;;          data-hash   (hash (get db :data))
;;           ;connection-id (get-in db [:panels selected-block :connection-id])
;;          ]
;;      (when (not (= data-hash (get db :data-hash)))
;;        (conn/sql-data [:reco-counts] {:select   [:table_name ;; too expensive to run every 5 seconds?????
;;                                                  [[:count [:distinct :combo_hash]] :cnt]]
;;                                       :from     [[:combo_rows :uu2434aa]]
;;                                       :group-by [:table_name]}))
;;      (doseq [[k v] all-queries]
;;        (conn/sql-deep-meta [k] (sql-alias-replace-sub v) (or (get-in db [:panels (lkp k) :connection-id]) (get v :connection-id)))
;;        )
;;      (assoc db :data-hash data-hash))))


(re-frame/reg-event-db
 ::update-metadata-styles
 (fn [db [_]]
   (let [lkp         (fn [query-key]
                       (first (remove nil? (for [[k v] (get db :panels)]
                                             (when (some #(= query-key %)
                                                         (keys (get v :queries))) k)))))
          ;selected-block (get db :selected-block)
          ;sel-queries (get-in db [:panels selected-block :queries])
         all-queries (into {} (for [[_ v] (get db :panels)]
                                (into {} (for [[kk vv] (get v :queries)]
                                           (when (and (nil? (find vv :vselect))
                                                      (find vv :style-rules)) {kk vv})))))]
          ;connection-id (get-in db [:panels selected-block :connection-id])

     (doseq [[k v] all-queries]
       (conn/sql-style-meta [k] (sql-alias-replace-sub v)
                            (get-in db [:panels (lkp k) :connection-id]))))))

(re-frame/reg-event-db
 ::update-metadata-tabs
 (fn [db [_]]
   (let [tab-rules (into {} (for [[k v] (get db :panels)]
                              (into {} (for [[kk vv] (get v :tab-rules)]
                                         {kk {:logic     vv
                                              :panel-key k}}))))]
          ;kp (keyword (str (ut/safe-name panel-key) "-tab-rules"))

      ;(ut/tapp>> [:tab-rules tab-rules])
      ;(ut/tapp>> [:run-tab-fn-w tab-rules])
     (doseq [[[tab-name rule-name] {:keys [logic panel-key]}] tab-rules]
       (conn/sql-tab-meta [panel-key] [[tab-name rule-name]
                                        ;logic
                                       (sql-alias-replace-sub logic)] panel-key)))))

(re-frame/reg-event-db
 ::update-conditionals
 (fn [db [_]]
   (let [all-condis (into {} (for [[_ v] (get db :panels)]
                               (into {} (for [[kk vv] (get v :conditionals)]
                                          {kk vv}))))]
          ;connection-id (get-in db [:panels selected-block :connection-id])

      ;(ut/tapp>> [:all-condis all-condis])
     (doseq [[k v] all-condis]
       (conn/sql-condi-meta [k] (sql-alias-replace-sub v))))))
      ;; do nothing
      ;nil

(defn iif [v this that] (if (true? v) this that))
(defn eql [this that] (= this that))

;; ;;; test
;; (defn honeycomb-stripped [panel-key & [override-view]]      ;; can sub lots of this TODO. no need to pass it all
;;   (let [;block-map panel-map ;@(ut/tracked-subscribe [::panel-map panel-key]) ;(get workspace panel-key) ;; ?
;;         connection-id     @(ut/tracked-subscribe [::panel-connection-id panel-key]) ;(get block-map :connection-id)
;;         sql-calls         @(ut/tracked-subscribe [::panel-sql-calls panel-key]) ; (get block-map :queries)
;;         base-tables       (flatten (remove nil? (for [[_ v] sql-calls] (when (not (cstr/includes? (str (get v :from)) ":query/")) (get v :from)))))
;;         base-table-sniffs (into {} (for [t base-tables] {t {:select [:*] :from [t] :limit 100}})) ;; need to generate base metadata for use
;;         ]
;;     ;(dorun
;;     (doseq [[k v] (merge sql-calls base-table-sniffs)]      ;; base-table-sniffs allow us to get metadata for the root tables quietly
;;       (let [query        (sql-alias-replace2 v)
;;             data-exists? @(ut/tracked-subscribe [::conn/sql-data-exists? [k]])
;;             unrun-sql?   @(ut/tracked-subscribe [::conn/sql-query-not-run? [k] query])]

;;         ;(dorun
;;         (when (or (not data-exists?) unrun-sql?)
;;           ;(do (ut/tapp>> [:query-inner query])
;;           (if (or (empty? connection-id) (nil? connection-id))
;;             (conn/sql-data [k] query)
;;             (conn/sql-data [k] query connection-id)))       ;)
;;         ; (conn/sql-deep-meta [k] query connection-id)
;;         ))                                                  ;);)
;;     [rc/catch
;;      [magic-table panel-key [(first (keys sql-calls))]]
;;      ]

;;     ))

;; (defn date-ranger-picker []
;;   [(reagent/adapt-react-class date-picker/DateRangePicker)
;;    {:showSelectionPreview      true
;;     :moveRangeOnFirstSelection false
;;     :months                    2
;;     :ranges                    ["2023-02-02" "2023-03-02"]  ;["2021-02-01 - 2021-02-01"]
;;     ;:ranges {"0" "2023-02-02", "1" "2023-03-02" }
;;     :direction                 "horizontal"
;;     :preventSnapRefocus        true
;;     :calendarFocus             "backwards"
;;     ;:onChange #(ut/tapp>> (js->clj %))
;;     :onChange                  #()}])

(defn sql-explanations-kp []
  {[:from 0 0] "table-name"
   [:from 0 1] "table-alias"
   [:from] "the table we are selecting from"})

(defn map-value-box [s k-val-type]  ;; dupe of a fn inside flows, but w/o dragging - TODO refactor to single fn - 3/15/24
  (let [render-values? true
        function? (or (fn? s) (try (fn? s) (catch :default _ false)))]
    (cond (ut/hex-color? s) [re-com/h-box
                             :gap "3px"
                             :children [[re-com/box :child (str s)]
                                        [re-com/box :child " "
                                         :width "13px"
                                         :height "13px"
                                         :style {:background-color (str s)
                                                 :border-radius "2px"}]]]
          (ut/is-base64? s)
          [re-com/v-box
           :size "auto"
           :gap "10px"
           :children
           [[re-com/box
             :align :end :justify :end
             :style {:opacity 0.5}
             :child (str "**huge base64 string: ~" (.toFixed (/ (* (count s) 6) 8 1024 1024) 2) " MB")]
            [re-com/box
             :size "auto"
             :child [:img {:src (str "data:image/png;base64," s)}]]]]

          (or function?
              (= k-val-type "function")) (str (.-name s) "!") ;; temp

          (string? s) [re-com/box
                       :size "auto"
                       :align :end :justify :end
                       :style {:word-break "break-all"}
                       :child (str "\"" s "\"")]
          :else (ut/replacer (str s) #"clojure.core/" ""))))

(defn data-viewer-source [obody]
  (let [kps       (ut/extract-patterns obody :data-viewer 2)
        logic-kps (first (for [v kps]
                           (let [[_ src] v]
                             src)))]
    logic-kps))

(re-frame/reg-sub
 ::data-viewer-source
 (fn [db {:keys [block-id selected-view]}]
   (let [view (get-in db [:panels block-id :views selected-view])
         ;view-vec (ut/deep-flatten view)
         ]
     ;;(ut/tapp>> [:vv block-id selected-view (ut/find-next view-vec :data-viewer) (data-viewer-source view)])
     ;;(ut/find-next view-vec :data-viewer)
     (data-viewer-source view))))

(defn map-boxes2 [data block-id selected-view keypath kki init-data-type & [draggable?]]  ;; dupe of a fn inside flows, but w/o dragging - TODO refactor to single fn - 3/15/24
  ;(ut/tapp>> [:pre-data data])
  (let [sql-explanations (sql-explanations-kp)
        flow-name (ut/data-typer data)
        data (if (or (string? data) (number? data) (boolean? data)) [data] data)
        base-type-vec? (or (vector? data) (list? data))
        iter (if base-type-vec? (range (count data)) (keys data))
        source @(ut/tracked-sub ::data-viewer-source {:block-id block-id :selected-view selected-view})
        font-size "inherit" ;;"11px"
        draggable? false ;; override bs TODO
        ;add-kp? (try (keyword? block-id) (catch :default _ false)) ;;true ;(not show-code-map-opt?) ;; for drag outs only
        ;cells? (= block-id :cells)
        dcolors @(ut/tracked-sub ::conn/data-colors {})
        non-canvas? (nil? block-id)
        dggbl (if non-canvas? draggable-stub draggable)
        main-boxes [re-com/v-box ;(if cells? re-com/h-box re-com/v-box)
                    :size "auto"
                    :padding "5px"
                    :gap "2px"
                    :style {:color "black"
                            :font-family (theme-pull :theme/base-font nil)
                            :font-size font-size; "11px"
                            :font-weight 500}
                    :children
                    (for [kk iter] ;; (keys data)
                      (let [k-val (get-in data [kk])
                            k-val-type (ut/data-typer k-val)
                            in-body? true ;(ut/contains-data? only-body k-val)
                            hovered? false ;(ut/contains-data? mat-hovered-input k-val)
                            border-ind (if in-body? "solid" "dashed")
                            val-color (get dcolors k-val-type)
                            keypath-in (conj keypath kk)
                            keystyle {:background-color (if hovered? (str val-color 66) "#00000000")
                                      ;:font-weight 700
                                      :color val-color
                                      :border-radius "12px"
                                      :border (str "3px " border-ind " " val-color)}
                            valstyle {:background-color (if hovered? (str val-color 22) "#00000000") ;"#00000000"
                                      :color val-color
                                      :border-radius "12px"
                                      :border (str "3px " border-ind " " val-color 40)}]
                        ^{:key (str block-id keypath kki kk)}
                        [re-com/box
                         :child

                         (cond (= k-val-type "map")
                               ^{:key (str block-id keypath kki kk k-val-type)}
                               [re-com/h-box
                                :children
                                [[dggbl ;;draggable
                                  ;; {:from block-id
                                  ;;  :new-block [:artifacts "text"]
                                  ;;  :idx 0 ;idx
                                  ;;  :keypath-in keypath-in
                                  ;;  :flow-id flow-name
                                  ;;  ;:keypath (if is-map? [:map [idx]] ref)
                                  ;;  :keypath [:map (if add-kp?
                                  ;;                   (vec (cons :v keypath-in))
                                  ;;                   keypath-in)]}
                                  {:h         4
                                   :w         6
                                   :root      [0 0]
                                   :drag-meta {:type        :viewer-pull
                                               :param-full  [:box :style {:font-size "17px"} :child [:data-viewer [:get-in [source keypath-in]]]]
                                               ;:param-full  [:box :child (str keypath-in)] ; [:data-viewer source keypath-in]]
                                               :param-type  k-val-type
                                               :param-table :what
                                               :param-field :what}}

                                  "meta-menu" ;block-id ;; (when (not (= block-id :inline-render)) block-id)

                                  ^{:key (str block-id keypath kki kk k-val-type 1)}
                                  [re-com/v-box
                                   :min-width (px (*
                                                   (count (str kk))
                                                   11)) ;"110px"
                                   ;:size "auto"
                                   :style {;:cursor (when draggable? "grab")
                                           ;:border "1px solid white"
                                           }
                                   :children [^{:key (str block-id keypath kki kk k-val-type 124)}
                                              [re-com/box :child (str kk)]
                                              ^{:key (str block-id keypath kki kk k-val-type 134)}
                                              [re-com/box :child (str k-val-type)
                                               :style {:opacity 0.45
                                                       :font-size font-size; "9px"
                                                       ;:font-weight 400
                                                       :padding-top "7px"}]
                                              (when (> (count k-val) 1)
                                                ^{:key (str block-id keypath kki kk k-val-type 156)}
                                                [re-com/box
                                                 :style {:opacity 0.45}
                                                 :child (str "(" (count k-val) ")")])]
                                   :padding "8px"]]
                                 (map-boxes2 k-val block-id selected-view  keypath-in kk nil draggable?)]
                                :style keystyle]

                               (or (= k-val-type "vector") (= k-val-type "list") ; (= k-val-type "function")
                                   (= k-val-type "rowset")
                                   (= k-val-type "jdbc-conn") (= k-val-type "render-object"))

                               ^{:key (str block-id keypath kki kk k-val-type 2)}
                               [re-com/h-box
                                :style {:border-radius "12px"
                                        :border "3px solid black"}
                                :children
                                [[dggbl ;;draggable
                                  ;; {:from block-id
                                  ;;  :new-block [:artifacts "text"]
                                  ;;  :idx 0 ;idx
                                  ;;  :keypath-in keypath-in
                                  ;;  :flow-id flow-name
                                  ;;  :keypath [:map (if add-kp?
                                  ;;                   (vec (cons :v keypath-in))
                                  ;;                   keypath-in)]}
                                  {:h         4
                                   :w         6
                                   :root      [0 0]
                                   :drag-meta {:type        :viewer-pull
                                               :param-full  [:box :style {:font-size "17px"} :child [:data-viewer [:get-in [source keypath-in]]]]
                                               :param-type  k-val-type
                                               :param-table :what
                                               :param-field :what}}
                                  "meta-menu" ;block-id
                                  ^{:key (str block-id keypath kki kk k-val-type 3)}
                                  [re-com/v-box
                                   ;:size "auto"
                                   :min-width (px (*
                                                   (count (str kk))
                                                   11)) ;"110px"
                                   :style {;:cursor (when draggable? "grab") ;;; skeptical, ryan 5/24/33
                                           }
                                   :children (if (and (= k-val-type "list")
                                                      (= (ut/data-typer (first k-val)) "function"))

                                               [^{:key (str block-id keypath kki kk k-val-type 4)}
                                                [re-com/h-box
                                                 :style {:margin-top "4px"}
                                                 :gap "1px"
                                                 :children [;[re-com/box :child (str kk) :style {:opacity 0.25}]
                                                            [re-com/box :child "("
                                                             :style {;:opacity 0.5
                                                                     :color "orange"
                                                                     :margin-top "-2px"
                                                                     :font-size "14px"
                                                                     :font-weight 700}]
                                                            ^{:key (str block-id keypath kki kk k-val-type 5)}
                                                            [re-com/box :child (str (first k-val)) ; (str  "(")
                                                             :style {:color "#ffffff"
                                                                     ;:margin-left "-5px"
                                                                     :font-size font-size ;"17px"
                                                                     ;:opacity 0.7
                                                                     }]]]
                                                 ;[re-com/box :child (str kk)]
                                                ^{:key (str block-id keypath kki kk k-val-type 6)}
                                                [re-com/box :child (str k-val-type)
                                                 :style {:opacity 0.45
                                                         :font-size font-size ; "9px"
                                                         ;:font-weight 400
                                                         :padding-top "16px"}]
                                                (when (> (count k-val) 1)
                                                  ^{:key (str block-id keypath kki kk k-val-type 827)}
                                                  [re-com/box
                                                   :style {:opacity 0.45}
                                                   :child (str "(" (count k-val) ")")])]

                                               [(when true ;(not (get sql-explanations keypath ""))
                                                  ^{:key (str block-id keypath kki kk k-val-type 7)}
                                                  [re-com/box :child (str kk)])

                                                (when true ; (not (get sql-explanations keypath ""))
                                                  ^{:key (str block-id keypath kki kk k-val-type 8)}
                                                  [re-com/box :child (str (when (= (count k-val) 0) "empty ") k-val-type)
                                                   :style {:opacity 0.45
                                                           :font-size font-size ; "9px"
                                                         ;:font-weight 400
                                                           :padding-top "7px"}])

                                                (when (ut/ne? (get sql-explanations keypath ""))
                                                  ^{:key (str block-id keypath kki kk k-val-type 82)}

                                                  [re-com/md-icon-button
                                                   :md-icon-name "zmdi-info"
                                                   :tooltip (str "FOOO" (get sql-explanations keypath ""))
                                                   ;:on-click #(ut/tracked-dispatch [::change-page panel-key (first data-keypath) (- page-num 1) ttl-pages])
                                                   :style {:font-size "16px"
                                                           :cursor "pointer"
                                                           :opacity 0.5
                                                           :padding "0px"
                                                           :margin-top "-1px"}])])

                                   :padding "8px"]]
                                 [map-boxes2
                                  (if (= k-val-type "rowset")
                                    (zipmap (iterate inc 0) (take 10 k-val))
                                    (zipmap (iterate inc 0) k-val))
                                  ; (zipmap (iterate inc 0) k-val)
                                  block-id selected-view keypath-in kk nil draggable?]]
                                :style keystyle]

                               :else
                               ^{:key (str block-id keypath kki kk k-val-type 9)}
                               [re-com/h-box
                                :children [^{:key (str block-id keypath kki kk k-val-type 10)}
                                           [re-com/h-box
                                            :gap "6px"
                                            :children [[dggbl ;;draggable
                                  ;;                       {:from block-id
                                  ;;                        :new-block [:artifacts "text"]
                                  ;;  ;:override (when is-map? src-block-id)
                                  ;;                        :idx 0 ;idx
                                  ;;  ;:keypath (if is-map? [:map [idx]] ref)
                                  ;;                        :keypath-in keypath-in
                                  ;;                        :flow-id flow-name
                                  ;;                        :keypath [:map (if add-kp?
                                  ;;                                         (vec (cons :v keypath-in))
                                  ;;                                         keypath-in)]}
                                                        {:h         4
                                                         :w         6
                                                         :root      [0 0]
                                                         :drag-meta {:type        :viewer-pull
                                                                     :param-full  [:box
                                                                                   :size "auto" :align :center :justify :center
                                                                                   :style {:font-size [:auto-size-px [:get-in [source keypath-in]]]}
                                                                                   :child [:string [:get-in [source keypath-in]]]]
                                                                     :param-type  k-val-type
                                                                     :param-table :what
                                                                     :param-field :what}}
                                                        "meta-menu" ;block-id
                                                        ^{:key (str block-id keypath kki kk k-val-type 11)}
                                                        [re-com/box :child (str kk)
                                                         :style {;:cursor (when draggable? "grab")
                                                                ;:border "1px solid white"
                                                                 }]]

                                                       (when true ;(not (get sql-explanations (vec (conj keypath kk)) ""))
                                                         ^{:key (str block-id keypath kki kk k-val-type 12)}
                                                         [re-com/box :child (str k-val-type)
                                                          :style {:opacity 0.45
                                                                  :font-size font-size  ;"9px"
                                                                  :font-style (if (=  k-val-type "function")
                                                                                "italic" "normal")
                                                                ;:font-weight 400
                                                                  }])
                                                       (when (get sql-explanations (vec (conj keypath kk)) "")
                                                         ^{:key (str block-id keypath kki kk k-val-type 822)}
                                                         [re-com/box
                                                          :style {:opacity 0.45}
                                                          :child (str  (get sql-explanations (vec (conj keypath kk)) ""))])]]
                                           ^{:key (str block-id keypath kki kk k-val-type 13)}
                                           [re-com/box

                                            :size "auto"
                                            :align :end :justify :end
                                            :child

                                            [map-value-box k-val]
                                            :style {;:font-weight 500
                                                    :line-height "1.2em"
                                                    :padding-left "5px"}]]
                                :justify :between
                                :padding "5px"
                                :style valstyle])]))]]
        ;main-boxes
    (if (= keypath [])

      (let [k-val-type (ut/data-typer data)
            nin? (not (= block-id :inline-render))
            val-color (get dcolors k-val-type)]
        [re-com/v-box
         ;:size "auto"
         :children
         [[re-com/v-box
           :style {:word-wrap "break-word"
                   ;:overflow "auto"
                   :overflow-wrap "break-word"}
           :children [(when nin?
                        [re-com/v-box
                       ;:justify :end
                         :padding "6px"
                     ;:size "300px"
                         :children [^{:key (str block-id keypath kki 00 k-val-type 00002)}
                                    [re-com/box :child (str flow-name) ;(str k-val-type)
                                     :align :end
                                     :style {:opacity 0.45
                                           ;:font-weight 400
                                           ;:font-size "14px"
                                             :font-size font-size  ;"9px"
                                             :margin-top "-7px"
                                             :margin-bottom "-5px"
                                           ;:padding-top "7px"
                                             }]]])
                    ;[re-com/box :child  :size "auto"]
                      main-boxes]
           :padding "0px"]]
         :style {:font-family (theme-pull :theme/base-font nil)
                 :color val-color
                 :margin-top (if nin? "0px" "-10px")
                 :border-radius "12px"}])
      main-boxes)))


(defn transform-nested                                      ;; do this on the server?
  [{:keys [data group-key x-key y-key]}]
  (let [groups (group-by group-key data)]
    (vec
     (map (fn [[group vals]]
            {:id   group
             :data (vec
                    (map (fn [val]
                           {:x (get val x-key)
                            :y (get val y-key)}) vals))})
          groups))))

(defn transform-stacked                                     ;; do this on the server?
  [{:keys [data group-key category-key value-key]}]
  (let [groups (group-by group-key data)]
    (vec
     (map (fn [[group vals]]
            (merge
             {group-key group}
             (apply merge-with
                    +
                    (vec
                     (map (fn [val]
                            {(get val category-key) (get val value-key)}) vals))))) groups))))

(defn debounce [f delay]
  (let [input  (chan)
        output (chan)]
    (go (loop []
          (let [args (<! input)]
            (>! output args)
            (<! (async/timeout delay))
            (recur)))
        (fn [& args]
          (go (>! input args))
          (go (let [args (<! output)]
                (apply f args)))))))

(defn nivo-data-check [edn-mass try-block]
  ;(ut/tapp>> [:nivo-data-check edn-mass])
  (let [data     (get edn-mass :data)
        h        (get edn-mass :height "100%")
        w        (get edn-mass :width "100%")
        no-data? (nil? data)]
    (if no-data? [re-com/box
                  :child "no data"
                  :height (if (or (float? h) (integer? h)) (px (js/Math.floor h)) h)
                  :width (if (or (float? w) (integer? w)) (px (js/Math.floor w)) w)
                  :style {:font-family (theme-pull :theme/base-font nil)}
                  :size "auto"
                  :align :center :justify :center]
        try-block)))

(defn nivo-render-straight [edn-mass nivo-fn]               ;; nivo-bar/Bar
  (nivo-data-check
   edn-mass
   (try
     [(reagent/adapt-react-class
       nivo-fn) edn-mass]
     (catch :default e [re-com/box :child (str "reagent/adapt-react-class nivo-render-straight err: " e)]))))

(defn nivo-render [edn-mass nivo-fn panel-key]              ;; nivo-line/Line
  ;(ut/tapp>> [:nivo (get edn-mass :data)])
  (let [data               (get edn-mass :data)
        transformation-map (get edn-mass :transformation-map)
        t-map?             (ut/ne? transformation-map)
        post-data          (if t-map? (st/transform transformation-map data) data)
        has-keys?          (not (nil? (get edn-mass :keys)))
        edn-mass           (assoc edn-mass :data post-data)
        edn-mass           (if (and (not has-keys?) t-map? (get transformation-map :pivot-by))
                             (assoc edn-mass :keys (vec (cset/difference
                                                         (set (keys (first post-data)))
                                                         (set (ut/deep-flatten transformation-map)))))
                             edn-mass)
        edn-mass           (assoc edn-mass :on-click
                                  #(let [;m-pair {(keyword (str (name panel-key) "/" k)) v}
                                         x-key   (get-in edn-mass [:click :x] :x)
                                         y-key   (get-in edn-mass [:click :y] :y)
                                         c-key   (get-in edn-mass [:click :c] :c)
                                         m-pair1 (into {} (for [[k v] (get (js->clj %) "data")] {(keyword k) v}))
                                         m-pair2 {x-key (if (= nivo-fn nivo-calendar/Calendar)
                                                          (get (js->clj %) "day")
                                                          (get (js->clj %) "indexValue"))
                                                  c-key (get (js->clj %) "id")
                                                  y-key (get (js->clj %) "value")}
                                                  ;m-pair (merge m-pair1 m-pair2)
                                         m-pair  (into {} (for [[k v] m-pair2]
                                                            (when (not (nil? v)) {k v})))]
                                     (ut/tapp>> [:nivo-click-params (js->clj %) m-pair])
                                     (doseq [[m1 m2] m-pair]
                                       (ut/tracked-dispatch [::conn/click-parameter [panel-key m1] m2]))
                                     ))
        edn-mass           (if (and (= nivo-fn nivo-calendar/Calendar)
                                    ;(get-in edn-mass [:data 0 :day])
                                    (not (get edn-mass :to)) (not (get edn-mass :from)))
                             (-> edn-mass
                                 (assoc :to (apply max (map :day (get edn-mass :data))))
                                 (assoc :from (apply min (map :day (get edn-mass :data)))))
                             edn-mass)
        node-size?         (keyword? (get edn-mass :nodeSize))
        edn-mass           (if node-size?
                             (assoc edn-mass :nodeSize #(do
                                                          ;(ut/tapp>> (js->clj %))
                                                          (get (walk/keywordize-keys (js->clj %))
                                                               (get edn-mass :nodeSize) 14)))
                             edn-mass)
        node-size2?        (and (keyword? (get edn-mass :size)) (= nivo-fn nivo-swarmplot/SwarmPlot))
        edn-mass           (if node-size2?
                             (assoc edn-mass :size #(do
                                                      ;(ut/tapp>> (js->clj %))
                                                      (get (walk/keywordize-keys (js->clj %))
                                                           (get edn-mass :size) 14)))
                             edn-mass)
        ;edn-mass (assoc-in edn-mass [:theme :grid :line :stroke] "green")
        ;edn-mass (assoc-in edn-mass [:theme] (get data :theme))
        ]
   ;(ut/tapp>> [:edn-mas edn-mass])
    ;; (when (and (not has-keys?) t-map? (not (empty? (get transformation-map :pivot-by))))
    ;;   (ut/tapp>> [:keys (vec (cset/difference
    ;;                      (set (keys (first post-data)))
    ;;                      (set (ut/deep-flatten transformation-map))))]))
    ;(ut/tapp>> [:nivo-nesting :transformed? t-map? data post-data])
    [re-com/box
     :style {:overflow "hidden"}
     :child (nivo-data-check
             edn-mass
             [(reagent/adapt-react-class nivo-fn) edn-mass])]))

;; (defn nivo-render-pivoted [edn-mass nivo-fn] ;; nivo-bar/Bar
;;   (nivo-data-check
;;    edn-mass
;;    (try
;;      [(reagent/adapt-react-class
;;        nivo-fn) edn-mass]
;;      (catch :default e [re-com/box :child (str "reagent/adapt-react-class nivo-render-pivoted err: " e)]))))

;; (defn nivo-choropleth [edn-mass]
;;   (try
;;     [(reagent/adapt-react-class
;;       nivo-geo/Choropleth) edn-mass]
;;     (catch :default e [re-com/box :child (str "reagent/adapt-react-class err: " e)])))

;; (defn nivo-calendar [edn-mass]
;;   (try
;;     [(reagent/adapt-react-class
;;       nivo-calendar/Calendar) edn-mass]
;;     (catch :default e [re-com/box :child (str "reagent/adapt-react-class err: " e)])))

;; (defn nivo-timerange [edn-mass]
;;   (try
;;     [(reagent/adapt-react-class
;;       nivo-calendar/TimeRange) edn-mass]
;;     (catch :default e [re-com/box :child (str "reagent/adapt-react-class err: " e)])))

;; (defn nivo-bump-chart [edn-mass]
;;   (try
;;     [(reagent/adapt-react-class
;;       nivo-bump/AreaBump) edn-mass]
;;     (catch :default e [re-com/box :child (str "reagent/adapt-react-class err: " e)])))

;; (defn nivo-heatmap-chart [edn-mass]
;;   (try
;;     [(reagent/adapt-react-class
;;       nivo-heatmap/HeatMap) edn-mass]
;;     (catch :default e [re-com/box :child (str "reagent/adapt-react-class err: " e)])))

;; (defn nivo-scatterplot-chart [edn-mass]
;;   (try
;;     [(reagent/adapt-react-class
;;       nivo-scatterplot/ScatterPlot) edn-mass]
;;     (catch :default e [re-com/box :child (str "reagent/adapt-react-class err: " e)])))

;; (defn nivo-waffle-chart [edn-mass]
;;   (try
;;     [(reagent/adapt-react-class
;;       nivo-waffle/Waffle) (or edn-mass {})]
;;     (catch :default e [re-com/box :child (str "reagent/adapt-react-class err: " e)])))

;; {:panels {:view-name1 {:h 7 :w 7 :root [0 0] :style {}}
;;           :view-name2 {:h 7 :w 7 :root [0 0] :style {}}}
;;  :style {}}

(re-frame/reg-sub
 ::get-layout-in
 (fn [_ [_ panel-key view-key]]
   (let [view @(ut/tracked-subscribe [::view panel-key view-key])
         kvs  (first (filter #(= (get-in view %) :layout) (ut/kvpaths view)))
         kvs1 (assoc kvs (dec (count kvs)) 1)
         v    (get-in view kvs1)]
     [(vec (flatten (into [:panels panel-key :views view-key] kvs1))) v]))) ;; returns vec [keypath, layout-map]

(re-frame/reg-event-db
 ::delete-layout-panel
 (undoable)
 (fn [db [_ panel-key view-key layout-key root]]
   (let [[layout-kp layout-map] @(ut/tracked-subscribe [::get-layout-in panel-key view-key])
         target-kp (into layout-kp [:panels layout-key])]
     (ut/tapp>> [:deleting-layout-shape target-kp layout-key])
     (ut/dissoc-in db target-kp))))

(re-frame/reg-event-db
 ::clear-layout-panel
 (undoable)
 (fn [db [_ panel-key view-key layout-key root]]
   (let [[layout-kp layout-map] @(ut/tracked-subscribe [::get-layout-in panel-key view-key])
         target-kp (into layout-kp [:panels layout-key])
         new-kp (into layout-kp [:panels (keyword (str "cleared-" (rand-int 1234)))])]
     (ut/tapp>> [:clear-layout-assignment target-kp layout-key])
     (-> db
         (assoc-in new-kp (get-in db target-kp)) ;; insert new blank panel with all settings
         (ut/dissoc-in target-kp))))) ;; remove old one


(re-frame/reg-event-db
 ::split-v-layout-panel
 (undoable)
 (fn [db [_ panel-key view-key layout-key root processed-layout]]
   (let [[layout-kp layout-map] @(ut/tracked-subscribe [::get-layout-in panel-key view-key])
         target-kp (into layout-kp [:panels layout-key])
         old       (get-in db target-kp)
          ;rup (fn [n] n)
          ; (if (and (not (= n 0.25)) (not (= n 0.5)) (not (= n 0.125)))
          ;   (/ (js/Math.ceil (* n 10)) 10) n))
          ;half-of (fn [x] (cond (and (> 0 x) (is-float? x)) (/ x 2)) )
         fck       (fn [x] (if (= x 0.99) 1.0 x))
         rup       (fn [x] (if (= x 1) 0.99 x))]
     (ut/tapp>> [:split-v-layout-shape target-kp layout-key])
     (-> db
         (ut/dissoc-in target-kp)
         (assoc-in (into layout-kp [:panels (keyword (str "empty-v-" (rand-int 1234)))])
                   {:h (rup (/ (fck (get old :h)) 2))
                    :w (rup (fck (get old :w))) :root (get old :root)})
         (assoc-in (into layout-kp [:panels (keyword (str "empty-v-" (rand-int 1234)))])
                   {:h (rup (/ (fck (get old :h)) 2))
                    :w (rup (fck (get old :w))) :root [(first (get old :root))
                                                       (rup (+ (/ (fck (get old :h)) 2) (second (get old :root))))]})))))

(re-frame/reg-event-db
 ::split-h-layout-panel
 (undoable)
 (fn [db [_ panel-key view-key layout-key root processed-layout]]
   (let [[layout-kp layout-map] @(ut/tracked-subscribe [::get-layout-in panel-key view-key])
         target-kp (into layout-kp [:panels layout-key])
         old       (get-in db target-kp)
          ;rup (fn [n] n )
          ; (if (and (not (= n 0.25)) (not (= n 0.5)) (not (= n 0.125)))
          ;  (/ (js/Math.ceil (* n 10)) 10) n))
          ;half-of (fn [x] (cond (and (> 0 x) (is-float? x)) (/ x 2)) )
         fck       (fn [x] (if (= x 0.99) 1.0 x))
         rup       (fn [x] (if (= x 1) 0.99 x))]           ;; "unfuck"
     (ut/tapp>> [:split-h-layout-shape target-kp layout-key])
     (-> db
         (ut/dissoc-in target-kp)
         (assoc-in (into layout-kp [:panels (keyword (str "empty-h-" (rand-int 1234)))])
                   {:h    (rup (fck (get old :h)))         ;(/ (get old :h) 2)
                    :w    (rup (/ (fck (get old :w)) 2))
                    :root (get old :root)})
         (assoc-in (into layout-kp [:panels (keyword (str "empty-h-" (rand-int 1234)))])
                   {:h    (rup (fck (get old :h)))
                    :w    (rup (/ (fck (get old :w)) 2))
                    :root [;(first (get old :root))
                           (rup (+ (/ (fck (get old :w)) 2) (first (get old :root))))
                           (second (get old :root))]})))))

(defn layout [panel-key view-key layout-map pw-int ph-int]
  (let [panels           (get layout-map :panels {})
        selected-block   @(ut/tracked-subscribe [::selected-block])
        scrub-borders?   (and (= panel-key selected-block)
                              @(ut/tracked-subscribe [::is-layout-selected-and-scrubber?]))
        editor?          @(ut/tracked-subscribe [::editor?])

       ; [p-x p-y]        @(ut/tracked-subscribe [::root panel-key])
        p-z              @(ut/tracked-subscribe [::zindex panel-key]) ;; 0 ?
        gli              @(ut/tracked-subscribe [::get-layout-in panel-key view-key])
        scrubber-on?     (get-in @db/scrubbers [panel-key view-key] false)
        brix             (fn [x & [mod]] (px (if mod
                                               (+ (* brick-size x) mod)
                                               (* brick-size x))))
        layout-style     (get layout-map :style {})
        pp               (lay/process-layout (get-in gli [1 :panels]) pw-int ph-int)
        hover-key1       @db/pixel-pusher-key
        ;pp (lay/process-layout gli pw-int ph-int)
        ;panels (process-all-blocks panels pw-int ph-int)
        processed-layout (lay/process-layout panels pw-int ph-int)
        boxes            (into (if scrub-borders?
                                 [[re-com/h-box
                                   :style {:position "fixed" :left 0 :top 25}
                                   :children (for [i (range pw-int)] [re-com/box :child (str i) :align :center :justify :center
                                                                      :style {:font-family (theme-pull :theme/base-font nil) :font-weight 200 :opacity 0.6}
                                                                      :width "50px" :height "50px"])]
                                  [re-com/v-box
                                   :style {:position "fixed" :left 0 :top 25}
                                   :children (for [i (range (- ph-int 1))] [re-com/box :child (if (= i 0) " " (str i)) :align :center :justify :center
                                                                            :style {:font-family (theme-pull :theme/base-font nil) :font-weight 200 :opacity 0.6}
                                                                            :width "50px" :height "50px"])]]
                                 [])
                               (vec (for [[view-ref p] processed-layout]
                                      (let [h             (get p :h 4) ;; if omitted, default to 4
                                            w             (get p :w 4) ;; if omitted, default to 4
                                            z             (get p :z 0)
                                            hidden?       (get p :hidden? false)
                                            ;o-ref (get p :k)
                                            [x y] (get p :root [0 0]) ;; if omitted, default to root pos upper left corner

                                            ;;  x (if (and (< x 1) (is-float? x)) ;; treat as percent of total
                                            ;;      (js/Math.floor (* x pw-int)) x)
                                            ;;  y (if (and (< y 1) (is-float? y)) ;; treat as percent of total
                                            ;;      (js/Math.floor (* y ph-int)) y)
                                            ;;  h (if (and (< h 1) (is-float? h)) ;; treat as percent of total
                                            ;;      (js/Math.floor (* h ph-int)) h)
                                            ;;  w (if (and (< w 1) (is-float? w)) ;; treat as percent of total
                                            ;;      (js/Math.floor (* w pw-int)) w)

;; adjusted-panel "" ; [:div (str p) [:br] (str (get panels view-ref))]
                                            ;; adjusted-panel (compute-expanded-size view-ref p panels pw-int ph-int)
                                            ;;  adjusted-panel (let [w-fixed (compute-expanded-size p panels :w pw-int ph-int)
                                            ;;                       panels1 (assoc panels view-ref w-fixed) ;(ut/postwalk-replacer {p w-fixed})
                                            ;;                       h-fixed (compute-expanded-size w-fixed panels1 :h pw-int ph-int)]
                                            ;;                   h-fixed) ;; pw-int ph-int
                                            ;orig-key (for [[k v] processed-layout] (when (= (get v :root) (get p :root)) k))
                                            orig-key      (first (for [[k v] pp
                                                                       :when (= (get v :root) (get p :root))] k))
                                            ;h (if (= h 0) (- ph-int y 1) h) ;; stretch val to edge of canvas
                                            ;w (if (= w 0) (- pw-int x) w) ;; stretch val to edge of canvas

                                            ;h (get adjusted-panel :h)
                                            ;w (get adjusted-panel :w)

                                            hover-key     @db/pixel-pusher-key
                                            hovered?      (= (last hover-key) orig-key)

                                            pstyle        (get p :style {}) ;; has overridden style map?
                                            left          (+ -1 (* x brick-size)) ;; compensate for parents borders
                                            top           (+ 25 (* y brick-size)) ;; compensate for parents borders
                                            root-visible? (not (or (> x (- pw-int 1)) (> y (- ph-int 2)))) ;; dont render fixed panels that are outside parent boundaries
                                            blank-map     {:child  [re-com/v-box
                                                                    :size "auto" :justify :center :align :center
                                                                    :children (if scrub-borders?
                                                                                [[re-com/v-box
                                                                                  :align :center
                                                                                  :justify :center
                                                                                  :children (for [e (ut/splitter (ut/replacer (str orig-key) #"view/" "") ".")]
                                                                                              [re-com/box
                                                                                               :style {:color "pink" :font-family (theme-pull :theme/base-font nil)}
                                                                                               :child (str (if (cstr/starts-with? e ":") e (str ":" e)))])
                                                                                  :style {:font-size "11px"}]
                                                                               ;;  [re-com/box
                                                                               ;;   :child  (str (dissoc (dissoc (get processed-layout view-ref) :id) :k)) ;adjusted-panel ;(str adjusted-panel) ; (str " (empty panel)")
                                                                               ;;   :style {:font-size "10px"}]
                                                                                 [re-com/h-box
                                                                                  :gap "10px"
                                                                                ;:width "50%"
                                                                                  :padding "5px" :justify :between
                                                                                  :children [[re-com/md-icon-button
                                                                                              :md-icon-name "zmdi-view-agenda"
                                                                                              :on-click #(ut/tracked-dispatch [::split-v-layout-panel panel-key view-key orig-key (get p :root) processed-layout])
                                                                                              :style {:font-size  "17px"
                                                                                                      :cursor     "pointer"
                                                                                                      :color "pink"
                                                                                                      :padding    "0px"
                                                                                                      :margin-top "-1px"}]

                                                                                             [re-com/md-icon-button
                                                                                              :md-icon-name "zmdi-view-agenda"
                                                                                              :on-click #(ut/tracked-dispatch [::split-h-layout-panel panel-key view-key orig-key (get p :root) processed-layout])
                                                                                              :style {:font-size  "17px"
                                                                                                      :cursor     "pointer"
                                                                                                      :transform  "rotate(90deg)"
                                                                                                      :color "pink"
                                                                                                      :padding    "0px"
                                                                                                      :margin-top "-2px"}]]]

                                                                                 [re-com/h-box
                                                                                  :gap "10px"
                                                                                ;:width "50%"
                                                                                  :padding "5px" :justify :between
                                                                                  :children [[re-com/md-icon-button
                                                                                              :md-icon-name "zmdi-grid-off"
                                                                                              :on-click #(ut/tracked-dispatch [::clear-layout-panel panel-key view-key orig-key (get p :root)])
                                                                                              :style {:font-size  "17px"
                                                                                                      :cursor     "pointer"
                                                                                                      :color "pink"
                                                                                                      :padding    "0px"
                                                                                                      :margin-top "-1px"}]
                                                                                             [re-com/md-icon-button
                                                                                              :md-icon-name "zmdi-close"
                                                                                              :on-click #(ut/tracked-dispatch [::delete-layout-panel panel-key view-key orig-key (get p :root)])
                                                                                              :style {:font-size  "17px"
                                                                                                      :cursor     "pointer"
                                                                                                      :color "pink"
                                                                                                      :padding    "0px"
                                                                                                      :margin-top "-1px"}]]]] [])]
                                                           :size   "none"
                                                           :width  (brix w)
                                                           :height (brix h)
                                                           :align  :center :justify :center
                                                           :style  (merge {:border           (if scrub-borders?
                                                                                               "2px dashed pink"
                                                                                               "inherit")
                                                                           :background-color (if hovered? "purple" "inherit")
                                                                           :filter           (when hovered? "drop-shadow(0 0 0.75rem crimson)")
                                                                           :position         "fixed" ; "relative"
                                                                           :overflow         "hidden"
                                                                           ;:margin-right     "-8px"
                                                                           :z-index          (+ p-z z)
                                                                           :left             left
                                                                           :top              top} pstyle)}
                                            box-map       {:child  view-ref ;[re-com/box :child view-ref :style {:transform "translate(0)"}]
                                                           :size   "none"
                                                           :width  (brix w)
                                                           :height (brix h)
                                                           :style  (merge {:border   (if scrub-borders?
                                                                                       "2px dashed #FFC0CB44" "inherit")
                                                                           ;:transform "translate(0)"
                                                                           :position "fixed" ; "relative"
                                                                           :overflow "hidden"
                                                                           :z-index  (+ p-z z)
                                                                           :left     left
                                                                           :top      top} pstyle)}
                                            ;leftovers (-> p (dissoc :style) (dissoc :h) (dissoc :w) (dissoc :root) (dissoc :k) (dissoc :id) (dissoc :view-ref) (dissoc :hidden?))
                                            leftovers     (dissoc p :style :h :w :z :root :k :view-ref :hidden? :id)]
                                        ;(ut/tapp>> [:occupied (all-occupied-coordinates (vals panels))])
                                        ;(ut/tapp>> [:test x (is-float? x)])
                                        ;(ut/tapp>> [:layoutz view-ref leftovers layout-map])
                                        (when (and root-visible? (not hidden?))
                                          (ut/mapply re-com/box (merge leftovers (if (or (keyword? view-ref) (and scrubber-on? (= panel-key selected-block) editor?))
                                                                                   blank-map box-map))))))))]

    ;(ut/tapp>> [:panels :proc hover-key1 processed-layout panels gli pp  ])
    ;(ut/tapp>> [:layout-maps panel-key layout-map pw-int ph-int p-x p-y boxes])
    ;(ut/tapp>> [:scrub-borders? scrub-borders?])
    [re-com/h-box
     :size "none"
     :width (brix pw-int -10)
     :height (brix ph-int -47)                              ;; footer, will be dynamic TODO
     :style (merge layout-style
                   {;:border "1px solid lime" ;; debug edges
                    ;:position "absolute"
                    :overflow         "hidden"
                    :background-size  (when scrub-borders?
                                        "50px 50px")
                    :background-image (when scrub-borders?
                                        "linear-gradient(0deg,   transparent 48px, #ffffff27 48px, #ffffff27 50px, transparent 50px),
                                        linear-gradient(90deg, transparent 48px, #ffffff27 48px, #ffffff27 50px, transparent 50px)")})

     :children boxes]))

(defn dropdown [{:keys [choices field panel-key width button-style style placeholder]}]
  ;(ut/tapp>> [:dropdown? [choices @(ut/tracked-subscribe [::conn/clicked-parameter-key [:block-36/season]]) field]])

  (let [;;choice @(ut/tracked-subscribe [::conn/clicked-parameter-key [(try (keyword (str (name panel-key) "/" (name field))) (catch :default _ :error/error))]])
        choice @(ut/tracked-sub ::conn/clicked-parameter-key-alpha {:keypath [(try (keyword (str (name panel-key) "/" (name field))) (catch :default _ :error/error))]})
        choice (if (string? (get-in choices [0 :id])) (str choice) choice)
        choice (if (and (string? choice) (empty? (cstr/trim choice))) nil choice) ;; in case we get a blank string it wont register as not selected for the placeholder
        ;; choices (if all (conj all choices) choices)
        ;choices (vec (conj {:id nil :label (str placeholder)} choices))
        ;; _ (ut/tapp>> [:dropdown? panel-key choice  choices field  width style field])
        width (if (string? width) (try (edn/read-string (ut/replacer width "px" "")) (catch :default _ 100)) width)
        ]
    [re-com/h-box
     :size "auto" :justify :between :align :center
     :children [[re-com/md-icon-button
                 ;:on-click #(ut/tracked-dispatch [::conn/click-parameter [panel-key] {field nil}])
                 :on-click #(ut/tracked-dispatch [::conn/click-parameter [panel-key field] nil])
                 :style (merge {:font-size "19px" :width "30px" :margin-top "8px" :opacity 0.33} button-style)
                 :md-icon-name "zmdi-close"]
                [re-com/single-dropdown
                 :style (if (nil? style) {} style)
                 :parts {:chosen-drop {:style {:overflow "visible"}}
                         ;:chosen-results {:style {:color "#ffffff"
                         ;                         :background-color "#eeeeee"}}
                         }
                 ;:max-height "700px"
                 :placeholder (str placeholder)
                 :choices choices
                 :model choice ;(ut/tracked-subscribe [::conn/clicked-parameter-key [(try (keyword (str (name panel-key) "/" (name field))) (catch :default _ :error/error))]]) ;(reagent/atom model)
                 :width (px (- width 30))
                 :on-change ;#(ut/tracked-dispatch [::conn/click-parameter [panel-key] {field %}])
                 #(ut/tracked-dispatch [::conn/click-parameter [panel-key field] %])
                 ]]]))

;; notes for inline render.
;; insert a fake block just like preview, expire them quickly

(re-frame/reg-sub
 ::generate-all-clicked
 (fn [db {:keys [query-key]}]
   (let [click-params (get-in db [:click-param query-key] {})
         multi-query-key (-> query-key str (ut/replacer #":" "") (str ".*") keyword)
         click-multi-params (get-in db [:click-param multi-query-key] {})
         merged-multis (into {}
                             (for [[k v] click-multi-params]
                               (if (get click-params k)
                                 {k (merge v (get click-params k))}
                                 {k v})))]
     ;(ut/tapp>> [query-key click-params merged-multis])
     (merge click-params merged-multis))))

(re-frame/reg-sub
 ::rs-value
 (fn [db {:keys [flow-id kkey]}]  ;;; dupe from buffy
   (let [src (get-in db [:runstreams flow-id :values kkey :source])]
     (if (= src :param)
       ;(get-in db [:runstreams flow-id :values kkey :value])
       (let [vvv @(rfa/sub ::resolver/logic-and-params {:m [(get-in db [:runstreams flow-id :values kkey :value])]})
             vv (try
                  (first
                        ;;@(ut/tracked-subscribe [::resolver/logic-and-params [(get-in db [:runstreams flow-id :values kkey :value])]])
                   vvv ;;@(rfa/sub ::resolver/logic-and-params {:m [(get-in db [:runstreams flow-id :values kkey :value])]})
                        ;;; ^^^^ :brave-red-butterfly is not sequable? what? so I wrapped in a vec. started after caching madness....
                   ) (catch :default e (do (ut/tapp>> [:rs-value-fuck-up-bricks vvv flow-id kkey src e]) vvv)))]
         ;(ut/tapp>> [:sketchy-rs-value? flow-id kkey vvv vv])
         vv)
       (get-in db [:runstreams flow-id :values kkey :value])))))

(re-frame/reg-sub
 ::runstream-overrides ;;; duped in AUDIO, TODO!!!
 (fn [db [_ flow-id]]
   (let [values (get-in db [:runstreams flow-id :values])
         override-map (into {} (for [[k {:keys [value source]}] values
                                     :when (not (nil? value))]
                                 {k (if (= source :input)
                                      value
                                      (let [;;vv @(ut/tracked-subscribe [::rs-value flow-id k])
                                            vv @(rfa/sub ::rs-value {:flow-id flow-id :kkey k})
                                            ] ;;; dupe from buffy
                                        (if (and (vector? vv) (every? string? vv))
                                          (cstr/join "\n" vv) vv)))}))]
     ;(when (cstr/includes? (str (get db :client-name)) "copper") (ut/tapp>> [:override-map values override-map]))
     override-map)))

(re-frame/reg-sub
 ::da-sched
 (fn [db _]
   (get db :sched)))

(re-frame/reg-sub
 ::webcam-feed
 (fn [db _]
   (get-in db [:webcam-feed])))

(re-frame/reg-sub
 ::webcam?
 (fn [db _]
   (get-in db [:webcam-feed])))

(defn honeycomb-fragments [c & [w h]]
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
                                 h (or h 9)]
                                 ;w 11
                                 ;h 9

                             [honeycomb panel-key :virtual-view w h view nil])
          (= type :query) (let [temp-key (get data_d :_query-id (keyword (str "kick-" (hash c))))
                                query {temp-key (-> data_d ;(get data_d :queries)
                                                    (dissoc :cache?)
                                                    (dissoc :refresh-every))}
                                h (get data_d :_h (or h 6))
                                w (get data_d :_w (or w 10))]
                                ;h (get data_d :_h 6)
                                ;w (get data_d :_w 10)

                            [re-com/box
                             :size "none"
                             :width (px (* w brick-size))
                             :height (px (- (* h brick-size) 30))
                             :child [honeycomb panel-key
                                     temp-key
                                     h w
                                     nil
                                     query]])
          (= type :both)    (let [queries (get data_d :queries)
                                  ;temp-key (keyword (str "kick-" (hash c)))
                                  qkeys (into {} (for [q (keys queries)]
                                                   {q (keyword (str (ut/replacer (str q) #":" "") "-kick-" (hash data_d)))}))
                                  ndata (ut/postwalk-replacer qkeys data_d)
                                  h (get data_d :_h (or h 11))
                                  w (get data_d :_w (or w 9))]
                                  ;h (get data_d :_h 11)
                                  ;w (get data_d :_w 9)

                                                                                  ;(ut/tapp>> [:qkeys (get data_d :selected-view) qkeys views (ut/postwalk-replacer qkeys views)])
                              [honeycomb panel-key
                                                                                   ;(or (first (keys views)) (first (keys queries)))
                               key ;:view ;(get data_d :selected-view)
                               h w
                               {key (get ndata :view)} ;views ;(ut/postwalk-replacer qkeys views)
                               (get ndata :queries)]) ;(ut/postwalk-replacer qkeys queries)

          :else [honeycomb panel-key key 11 9])))


;;(ut/tracked-dispatch [::start-webcam])


(def mutation-log (reagent/atom []))

;; [bricks/honeycomb :virtual-panel :view w h view nil]

(re-frame/reg-sub
 ::drop-details
 (fn [db [_ drop-id]]
   (let [drops (get-in db [:runstream-drops])]
     (get
      (into {} (apply concat
                      (for [[k v] drops]
                        (for [[kk vv] v]
                          {kk (merge vv
                                     {:flow-id k
                                      :name kk})})))) drop-id {}))))



;; (re-frame/reg-sub
;;  ::lookup-alert-id
;;  (fn [db [_ type flow-id bid value]]
;;    (let [alerts (get db :alerts)]
;;      (filter #(let [un (vec (distinct (ut/deep-flatten %)))]
;;                 (ut/tapp>> [:alerts-flat un])
;;                 ;false
;;                 )
;;              alerts)
;;      )))


(re-frame/reg-sub
 ::lookup-alert-id
 (fn [db [_ type flow-id bid value]]
   (let [alerts (get db :alerts)]
     (last (first
            (filter (fn [x] (let [un (vec (distinct (ut/deep-flatten x)))]
                       ;;(cstr/includes? un (str type " " flow-id " " bid " " value))
                        ;(some #(and (= % value) (= % bid) (= % flow-id) (= % type)) un)
                              (and (some #(= % value) un)
                             ;(some #(= % bid) un)
                                   (some #(= % flow-id) un)
                                   (some #(= % type) un))))
                    alerts))))))

(re-frame/reg-event-db
 ::push-value
 (fn [db [_ flow-id bid value & [alert?]]]
   (let [client-name (get db :client-name)]
     (ut/tapp>> [:push-value flow-id bid value client-name])
     ;(ut/dispatch-delay 100 [::http/insert-alert [:box :child (str [:push-value-to flow-id bid value]) :style {:font-size "14px"}] 11 0.5 5])
     (ut/tracked-dispatch [::wfx/request :default
                         {:message    (merge
                                       {:kind :push-value
                                        :flow-id flow-id
                                        :bid bid
                                        :value value
                                        :client-name client-name}
                                       (when alert? {:alert? alert?}))
                         ;:on-response [::simple-response]
                         ;:on-timeout  [::timeout-response :run-flow flowmap] ;; requeue?
                          :timeout    15000000}])
     db)))

(defn code-box [width-int height-int value & [syntax]]
  [re-com/box
   :size "auto"
     ;:width "100%" ;(px (- width-int 24))
     ;:height (px (- height-int 24))
   :style {:font-family   (theme-pull :theme/monospaced-font nil) ; "Chivo Mono" ;"Fira Code"
           :font-size     "12px"
           :overflow      "auto"
           :border-radius "12px"
           :font-weight   700}
   :child [(reagent/adapt-react-class cm/UnControlled)
           {:value   (str value)
            :options {:mode              (or syntax "text")
                      :lineWrapping      true
                      :lineNumbers       false ; true
                      :matchBrackets     true
                      :autoCloseBrackets true
                      :autofocus         false
                      :autoScroll        false
                      :detach            true
                      :readOnly          true            ;true
                      :theme             (theme-pull :theme/codemirror-theme nil)}}]]) ;"ayu-mirage" ;"hopscotch"

(defn shortcode-box [width-int height-int value & [syntax]]
  [re-com/box
   :size "auto"
   :width  (px (- width-int 24))
   :height (px (- height-int 24))
   :style {:font-family   (theme-pull :theme/monospaced-font nil) ; "Chivo Mono" ;"Fira Code"
           :font-size     "18px"
           :border (str "1px solid " (theme-pull :theme/editor-outer-rim-color nil) 55)
           :background-color "#00000099"
           :overflow      "auto"
           :padding "3px"
           :border-radius "12px"
           :font-weight   700}
   :child [(reagent/adapt-react-class cm/UnControlled)
           {:value   (str value)
            :options {:mode              (or syntax "text")
                      :lineWrapping      true
                      :lineNumbers       false ; true
                      :matchBrackets     true
                      :autoCloseBrackets true
                      :autofocus         false
                      :autoScroll        false
                      :detach            true
                      :readOnly          true            ;true
                      :theme             (theme-pull :theme/codemirror-theme nil)}}]])


(defn edn-code-box [width-int height-int value & [syntax]]
  [re-com/box
   :size "auto"
     ;:width "100%" ;(px (- width-int 24))
     ;:height (px (- height-int 24))
   :style {:font-family   (theme-pull :theme/monospaced-font nil) ; "Chivo Mono" ;"Fira Code"
           ;:font-size     "12px"
           :overflow      "auto"
           :border-radius "12px"
           :font-weight   700}
   :child [(reagent/adapt-react-class cm/UnControlled)
           {;:value   (str value)
            :value (ut/format-map width-int (str value))
            :options {:mode              "clojure" ;(or syntax "text")
                      :lineWrapping      true
                      :lineNumbers       false ; true
                      :matchBrackets     true
                      :autoCloseBrackets true
                      :autofocus         false
                      :autoScroll        false
                      :detach            true
                      :readOnly          true            ;true
                      :theme             (theme-pull :theme/codemirror-theme nil)}}]]) ;"ayu-mirage" ;"hopscotch"

;; (re-frame/reg-sub
;;  ::flowmap-raw ;; dupe from flows.cljs - combine with new lower namespace  TODO
;;  (fn [db]
;;    (let [fmaps (get-in db [:flows (get db :selected-flow) :map] {})
;;          fmaps (into {} (for [[k v] fmaps
;;                               :when (get v :w)]
;;                           {k v}))]
;;      fmaps)))

;; (re-frame/reg-sub
;;  ::opts-map ;; dupe from flows.cljs - combine with new lower namespace  TODO
;;  (fn [db _]
;;    (get-in db [:flows (get db :selected-flow) :opts]
;;            {:retry-on-error? false :retries 5 :close-on-done? true})))

;; (re-frame/reg-sub
;;  ::flowmap-connections ;; dupe from flows.cljs - combine with new lower namespace  TODO
;;  (fn [db]
;;    (vec (get-in db [:flows (get db :selected-flow) :connections] []))))


(defonce progress-bars (reagent.core/atom {}))
(defonce progress-loops (reagent.core/atom {}))

(defn stop-progress-loop [uid]
  (when-let [loop (get @progress-loops uid)]
    (async/close! loop)
    (swap! progress-loops dissoc uid)))

(declare grid)

(defn clover-walk-singles [panel-key client-name px-width-int px-height-int ww hh w h selected-view override-view]
  (let [kk (pr-str [panel-key client-name px-width-int px-height-int ww hh w h selected-view override-view])
        cc (get @ut/clover-walk-singles-map kk)]
    (if (ut/ne? cc) cc
        (let [walk-map (merge
                                  ;drop-walks
                        {;:re-com/box re-com/box ;; basics
                         :box                   re-com/box
                                  ;;  :auto                  (fn [x] (ut/tapp>> [:auto (ut/data-typer x)])
                                  ;;                                     [re-com/box :child x])
                                    ;:re-com/h-box re-com/h-box
                                    ;:re-com/v-box re-com/v-box
                                    ;:== (fn [a b] (= a b))
                                    ;:equal (fn [a b] (str (= a b)))
                                    ;:testme #(= %1 %2)
                                    ;:gap re-com/gap
                         :layout                (fn [x] [layout panel-key (or override-view selected-view) x w h])
                         :px                    re-com.util/px
                         :icon                  re-com/md-icon-button
                         :md-icon               re-com/md-icon-button
                         :h-box                 re-com/h-box
                         :hbox                  re-com/h-box
                         :vbox                  re-com/v-box
                         :rechart               reech/chart
                         :input-text            re-com/input-text
                         :input-textarea        re-com/input-textarea
                         :honeycomb             honeycomb-fragments
                                    ;:label re-com/label
                         :single-dropdown       re-com/single-dropdown
                         :dropdown              #(dropdown (try (assoc % :panel-key panel-key) (catch :default _ %)))
                                    ;;; use square brackets to avoid rerenders?  ^^
                         :atom                  #(reagent/atom %)
                         :get                   get
                         :panel-code-box        panel-code-box
                         :code-box              panel-code-box
                         :panel-code-box-single panel-code-box-single
                         :code-box-single       panel-code-box-single
                         :vega-lite             oz.core/vega-lite
                         :nivo-bar-chart        #(nivo-render % nivo-bar/BarCanvas panel-key)
                                    ;:nivo-bar-chart #(nivo-render-straight % nivo-bar/Bar)
                         :nivo-line-chart       #(nivo-render % nivo-line/LineCanvas panel-key)
                         :nivo-calendar         #(nivo-render % nivo-calendar/Calendar panel-key)
                         :nivo-pie-chart        #(nivo-render % nivo-pie/PieCanvas panel-key)
                         :nivo-waffle-chart     #(nivo-render % nivo-waffle/WaffleCanvas panel-key)
                         :nivo-scatterplot      #(nivo-render % nivo-scatterplot/ScatterPlotCanvas panel-key)
                         :nivo-swarmplot        #(nivo-render % nivo-swarmplot/SwarmPlot panel-key)
                         :nivo-treemap          #(nivo-render % nivo-treemap/TreeMap panel-key)
                         :ro-code-box           #(code-box nil nil %)
                         :edn-code-box          #(edn-code-box ww nil %)
                                   ;:date-range-picker     date-ranger-picker
                                   ;:auto-font-size-px ut/auto-font-size-px
                         :LineChart reech/LineChart
                         :ResponsiveContainer reech/ResponsiveContainer
                         :CartesianGrid reech/CartesianGrid
                         :Line reech/Line
                         :Legend reech/Legend
                         :XAxis reech/XAxis
                         :YAxis reech/YAxis
                         :RadialBar reech/RadialBar
                         :RadialBarChart reech/RadialBarChart
                         :RadarChart reech/RadarChart
                         :ComposedChart reech/ComposedChart
                         :Radar reech/Radar
                         :Scatter reech/Scatter
                         :Bar reech/Bar
                         :BarChart reech/BarChart
                         :Brush reech/Brush
                         :Treemap reech/Treemap
                         :Rectangle reech/Rectangle
                         :Tooltip reech/Tooltip
                         :Area reech/Area
                         :Areas reech/Area
                         :Pie reech/Pie
                         :PieChart reech/PieChart
                         :ScatterChart reech/ScatterChart
                         :ReferenceLine reech/ReferenceLine
                         :AreaChart reech/AreaChart
                         :LabelList reech/LabelList
                         :PolarAngleAxis reech/PolarRadiusAxisTick
                         :PolarGrid reech/PolarGrid
                         :PolarChart reech/PolarChart
                         :Hint reech/Hint
                         :Funnel reech/Funnel
                         :FunnelChart reech/FunnelChart

                         :*client-name client-name
                         :*client-name-str (pr-str client-name)

                         :str (fn [args]
                                         ;(cstr/join (cstr/join "" args) "")
                                (apply str args))

                                  ;;  :get-in (fn [[m kp]]
                                  ;;            (ut/tapp>> [:get-in panel-key m kp])
                                  ;;            (get-in m kp))
                         :left-pad (fn [[num len]]
                                     (let [num-str (str num)
                                           padded-num (.padStart num-str len "0")]
                                       padded-num))

                         :>> (fn [[x y]] (true? (> x y)))
                         :<< (fn [[x y]] (true? (< x y)))

                         :string (fn [args]
                                   (if (vector? args)
                                     (cstr/join "" args) ;;(apply str args))
                                     (str args)))

                                  ;;  :string (fn [args]
                                  ;;            (if (vector? args)
                                  ;;              (cstr/join "" (map str args))
                                  ;;              (str args)))


                                  ;;  :pre-string (fn [args]
                                  ;;            (if (vector? args)
                                  ;;              (cstr/join "" (apply str args))
                                  ;;              (str args)))


                         :data-viewer (fn [x] [re-com/box
                                               :width (px (- ww 10))
                                               :size "none"
                                               :height (px (- hh 60)) ;;"300px" ;(px hh)
                                               :style {:overflow "auto"}
                                               :child [map-boxes2 x panel-key selected-view [] :output nil]])

                         :progress-bar (fn [[ww seconds uid]]
                                         (let [progress (or (get @progress-bars uid) (reagent.core/atom 0))
                                               transition-duration (/ seconds 40.0)]
                                           (when (and (zero? @progress) (not (get @progress-loops uid)))
                                             (let [loop (async/go-loop []
                                                          (<! (async/timeout 1000))
                                                          (swap! progress + (quot 100 seconds))
                                                          (when (< @progress 100)
                                                            (recur)))]
                                               (swap! progress-loops assoc uid loop)))
                                           (swap! progress-bars assoc uid progress)
                                           (fn []
                                             [re-com/progress-bar
                                              :model (if (> @progress 100) 100 @progress)
                                              :striped? true
                                              :bar-style {:color (str (ut/choose-text-color (theme-pull :theme/editor-outer-rim-color nil)) 67)
                                                          :outline "none"
                                                          :border "none"
                                                          :transition (str "all " transition-duration "s ease-in-out")
                                                                    ;:border-radius "9px"
                                                          :background-color (theme-pull :theme/editor-outer-rim-color nil)}
                                              :style {:font-family (theme-pull :theme/base-font nil)
                                                      :background-color "#00000000"
                                                      :border (str "1px solid " (theme-pull :theme/editor-outer-rim-color nil) 55)
                                                      :outline "none"
                                                      :width ww}])))

                                  ;;  :progress-bar (fn [[ww seconds uid]]
                                  ;;                  (let [progress (or (get @progress-bars uid) (reagent.core/atom 0))]
                                  ;;                    (when (and (zero? @progress) (not (get @progress-loops uid)))
                                  ;;                      (let [loop (async/go-loop []
                                  ;;                                   (<! (async/timeout 1000))
                                  ;;                                   (swap! progress + (quot 100 seconds))
                                  ;;                                   (when (< @progress 100)
                                  ;;                                     (recur)))]
                                  ;;                        (swap! progress-loops assoc uid loop)))
                                  ;;                    (swap! progress-bars assoc uid progress)
                                  ;;                    (fn []
                                  ;;                      [re-com/progress-bar
                                  ;;                       :model (if (> @progress 100) 100 @progress)
                                  ;;                       :bar-style {:color (ut/choose-text-color (theme-pull :theme/editor-outer-rim-color nil))
                                  ;;                                   :outline "none"
                                  ;;                                   :border "none"
                                  ;;                                   :transition "all 0.2s ease-in-out"
                                  ;;                                   ;:border-radius "9px"
                                  ;;                                   :background-color (theme-pull :theme/editor-outer-rim-color nil)}
                                  ;;                       :style {:font-family (theme-pull :theme/base-font nil)
                                  ;;                               :background-color "#00000000"
                                  ;;                               :border (str "1px solid " (theme-pull :theme/editor-outer-rim-color nil) 55)
                                  ;;                               :outline "none"
                                  ;;                               :width ww}])))

                         :speak (fn [text] (let []
                                             (when (not (some #(= % text) @db/speech-log))
                                               (swap! db/speech-log conj text)
                                               (ut/dispatch-delay 800 [::http/insert-alert (str text) 13 1 5])
                                               (ut/tracked-dispatch [::audio/text-to-speech11
                                                                     panel-key ;:audio
                                                                     :speak (str text)]))
                                             (str text)))

                         :speak-always (fn [text] (let []
                                                    (when true ; (not (some #(= % text) @db/speech-log))
                                                           ;(swap! db/speech-log conj text)
                                                      (ut/tracked-dispatch [::audio/text-to-speech11
                                                                            panel-key ;:audio
                                                                            :speak (str text)]))
                                                    (str text)))

                                   ;;:number (fn [x] (ut/nf x))

                         :speak-click (fn [text]
                                        (let []
                                          [re-com/v-box
                                           :size "auto"
                                           :children [[re-com/box :child (str text)]
                                                      [re-com/h-box :size "auto"
                                                       :justify :between
                                                       :height "10px"
                                                       :style {};:border "1px solid white"
                                                                         ;:margin-top "-6px"

                                                       :children [[re-com/md-icon-button
                                                                   :md-icon-name "zmdi-play"
                                                                   :on-click (fn [] (when true ;(not (some (fn [x] (= x text)) @db/speech-log))
                                                                                      (do
                                                                                                  ;(swap! db/speech-log conj text)
                                                                                        (ut/dispatch-delay 800 [::http/insert-alert (str "\"" text "\"") 13 2 8])
                                                                                        (ut/tracked-dispatch [::audio/text-to-speech11
                                                                                                                      ;:audio
                                                                                                              panel-key
                                                                                                              :speak (str text)]))))
                                                                   :style {:font-size "15px"
                                                                           :opacity   0.25
                                                                           :cursor    "pointer"}]
                                                                  [re-com/box :child ""]]]]]))

                                  ;;  :run-flow (fn [flow-id] (let [client-name @(ut/tracked-subscribe [::client-name])]
                                  ;;                            (when true ;(not (some #(= % text) @db/speech-log))
                                  ;;                              (do ;; some kind of debounce, prevent multiple, etc
                                  ;;                                (ut/tracked-dispatch [::wfx/request :default
                                  ;;                                                    {:message    {:kind :run-server-flow
                                  ;;                                                                  :flow-id flow-id
                                  ;;                                                                  :client-name client-name
                                  ;;                                                                  :keypath [:panels panel-key :views selected-view]}
                                  ;;                                                     :on-response [::http/socket-response]
                                  ;;                                                     :on-timeout [::http/timeout-response]
                                  ;;                                                     :timeout    500000}])))
                                  ;;                            (str flow-id)))
                         :grid (fn [tab] [rc/catch [grid tab]]) ;;; TODO :grid was clashing with nivo configs

                                  ;;  :unixtime (fn [] '(fn [unixTime]
                                  ;;              (let [date (js/Date. unixTime)]
                                  ;;                (.toLocaleString date))))

                         :read-edn (fn [x] (try (edn/read-string x) (catch :default _ x)))

                         :open-input (fn [args]
                                       [re-com/box
                                        :width (px (- ww 10))
                                        :size "none"
                                        :height (px (- hh 60)) ;;"300px" ;(px hh)
                                        :style {:overflow "auto"} ;; (merge {:overflow "auto"} (get args :style {}))
                                        :child (let [;[kp width-int height-int syntax] args
                                                     {:keys [kp width-int height-int style syntax]} args
                                                     vv @(ut/tracked-subscribe [::conn/clicked-parameter kp])
                                                     vv (if (string? vv) (pr-str vv) vv)]
                                                 [open-input-code-box kp width-int height-int vv syntax style])])

                         :play (fn [text & [uid]]
                                 (let [uid (or uid (str "tt" (rand-int 1234) (rand-int 1234)))]
                                   (when (not (some #(= % uid) @db/speech-log))
                                     (swap! db/speech-log conj uid)
                                     (ut/tracked-dispatch [::audio/text-to-speech11 :audio :speak (str text) true]))
                                   ""))

                         :play1 (fn [[text & [uid]]]
                                  (let [uid (or uid (str "tt" (rand-int 1234) (rand-int 1234)))]
                                    (when (not (some #(= % uid) @db/speech-log))
                                      (swap! db/speech-log conj uid)
                                      (ut/tracked-dispatch [::audio/text-to-speech11 :audio :speak (str text) true]))
                                    ""))

                         :play-click (fn [text]
                                       (let []
                                         [re-com/v-box
                                          :size "auto"
                                          :children [[re-com/box :child (str text)]
                                                     [re-com/h-box :size "auto"
                                                      :justify :between
                                                      :height "10px"
                                                      :style {};:border "1px solid white"
                                                                         ;:margin-top "-6px"

                                                      :children [[re-com/md-icon-button
                                                                  :md-icon-name "zmdi-play"
                                                                  :on-click (fn [] (when (not (some (fn [x] (= x text)) @db/speech-log))
                                                                                     (do
                                                                                                  ;(swap! db/speech-log conj text)
                                                                                       (ut/tracked-dispatch [::audio/text-to-speech11
                                                                                                                     ;:audio
                                                                                                             panel-key
                                                                                                             :speak (str text) true]))))
                                                                  :style {:font-size "15px"
                                                                          :opacity   0.25
                                                                          :cursor    "pointer"}]
                                                                 [re-com/box :child ""]]]]]))

                         :play-click-target (fn [[text panel-key]]
                                              (let []
                                                [re-com/v-box
                                                 :size "auto"
                                                 :children [[re-com/box :child (str text)]
                                                            [re-com/h-box :size "auto"
                                                             :justify :between
                                                             :height "10px"
                                                             :style {};:border "1px solid white"
                                                                                                          ;:margin-top "-6px"

                                                             :children [[re-com/md-icon-button
                                                                         :md-icon-name "zmdi-play"
                                                                         :on-click (fn [] (when (not (some (fn [x] (= x text)) @db/speech-log))
                                                                                            (do
                                                                                                                                   ;(swap! db/speech-log conj text)
                                                                                              (ut/tracked-dispatch [::audio/text-to-speech11
                                                                                                                       ;:audio
                                                                                                                    panel-key
                                                                                                                    :speak (str text) true]))))
                                                                         :style {:font-size "15px"
                                                                                 :opacity   0.25
                                                                                 :cursor    "pointer"}]
                                                                        [re-com/box :child ""]]]]]))

                                  ;;  :push> (fn [[flow-id bid value]]
                                  ;;           ;(ut/tracked-dispatch [::push-value flow-id bid value])
                                  ;;           '(fn [] '(js/alert "hey")))

                                  ;;  :push> #(ut/tracked-dispatch [::push-value (get % 0) (get % 1) (get % 2)])


                         :push (fn [[flow-id bid value & [view]]]
                                 (let [bid (try (if (cstr/starts-with? bid ":")
                                                  (edn/read-string bid)
                                                  (edn/read-string (str ":" bid))) (catch :default _ nil))]
                                   [re-com/v-box
                                    :size "auto"
                                    :children [[re-com/box :child (if view view (str [flow-id bid value]))]
                                               [re-com/h-box :size "auto"
                                                :justify :between
                                                :height "10px"
                                                :style {};:border "1px solid white"
                                                                         ;:margin-top "-6px"

                                                :children [[re-com/md-icon-button
                                                            :md-icon-name "fa-solid fa-location-crosshairs" ;; <i class= "fa-solid fa-location-crosshairs" ></i>
                                                            :on-click (fn [] (when bid
                                                                               (ut/tracked-dispatch [::push-value flow-id bid value])))
                                                            :style {:font-size "15px"
                                                                    :opacity   0.25
                                                                    :cursor    "pointer"}]
                                                           [re-com/box :child ""]]]]]))

                         :dialog-push (fn [[flow-id bid value & [view]]]
                                        (let [bid (try (if (cstr/starts-with? bid ":")
                                                         (edn/read-string bid)
                                                         (edn/read-string (str ":" bid))) (catch :default _ nil))
                                              alert-id @(ut/tracked-subscribe [::lookup-alert-id :dialog-push flow-id bid value])]
                                                    ;;(ut/tapp>> [:alert-id alert-id])
                                          [re-com/box
                                           :style {:cursor "pointer"}
                                                             ;D:font-size "11px"

                                           :attr {:on-click (fn [] (when bid
                                                                     (ut/tracked-dispatch [::push-value flow-id bid value])
                                                                     (ut/dispatch-delay 800 [::prune-alert alert-id])))}
                                           :size "auto"
                                           :child (if view view (str "push " value " to " flow-id "/" bid))]))

                         :web-cam #(let [video-ref (reagent/atom nil)
                                         running? @(ut/tracked-subscribe [::webcam?])
                                         webcam-stream @(ut/tracked-subscribe [::webcam-feed])]
                                     (reagent/create-class
                                      {:component-did-mount
                                       (fn []
                                         (when (and webcam-stream @video-ref)
                                           (set! (.-srcObject @video-ref) webcam-stream)))
                                       :reagent-render
                                       (fn []
                                         [re-com/box
                                                    ;:size "none"
                                                    ;:width (px w)
                                                    ;:height (px (* w 1.45))
                                          :style (if (not running?)
                                                   {:border "1px solid #ffffff87"} {})
                                          :child [:video {:on-click (fn [_] (if running?
                                                                              (ut/tracked-dispatch [::audio/stop-webcam])
                                                                              (ut/tracked-dispatch [::audio/start-webcam])))
                                                            ;:style {:border "1px solid white"}
                                                          :style {:objectFit "cover"
                                                                    ;:position "absolute"
                                                                  :top "0"
                                                                  :left "0"
                                                                  :width "100%"
                                                                  :height "100%"}
                                                                    ;:zIndex -1

                                                          :autoPlay true
                                                          :playsInline true
                                                          :ref (fn [x] (reset! video-ref x))}]])}))

                         :execute (fn [pp] (when (map? pp)
                                             (let [mods (count (keys pp))]
                                               (doseq [[kk vv] pp]
                                                 (let [hid (hash [kk vv])
                                                       already? (some #(= hid (hash %)) @mutation-log)]
                                                   (when (not already?)
                                                     (do (ut/tapp>> [:forced-execution-from-honey-execute kk vv])
                                                         (swap! mutation-log conj hid)
                                                         (ut/tracked-dispatch [::update-workspace-raw kk vv])))))
                                               [re-com/box
                                                                  ;:width "580px" :align :end :justify :end
                                                                  ;:style {:margin-right "30px"}
                                                :child [re-com/box
                                                                              ;:align :end :justify :end
                                                        :size "none"
                                                        :child (str "*ran " mods " board modification" (when (> mods 1) "s"))
                                                        :style {;:background-color (str (theme-pull :theme/editor-outer-rim-color nil) 45)
                                                                :border (str "1px solid " (str (theme-pull :theme/editor-outer-rim-color nil) 45))
                                                                :border-radius "10px"
                                                                :color (str (theme-pull :theme/editor-outer-rim-color nil) 45) ;"#000000"
                                                                :padding-left "12px"
                                                                :padding-right "12px"}]])))

                         :insert-alert (fn [[c w h d]]
                                         (ut/tracked-dispatch [::http/insert-alert c w h d]))

                         :run-flow (fn [[flow-id tt & [overrides]]]
                                     (let [client-name @(ut/tracked-subscribe [::client-name])
                                           base-opts {:increment-id? false}
                                           running-key (keyword (str "flow/" flow-id ">*running?"))
                                           running?
                                                     ;@(ut/tracked-subscribe [::conn/clicked-parameter-key [running-key]])
                                           @(rfa/sub ::conn/clicked-parameter-key-alpha {:keypath [running-key]})

                                                     ;overrides (if (nil? overrides) {} overrides)
                                           runstreamed? (= overrides :runstream-overrides)
                                           overrides (if runstreamed?
                                                       @(ut/tracked-subscribe [::runstream-overrides flow-id])
                                                       overrides)
                                           overrides? (ut/ne? overrides)]

                                                ;;  (ut/tapp>> [:overrides overrides])

                                       [re-com/h-box
                                        :size "auto"
                                        :children [[re-com/box :child (str tt)]
                                                   [re-com/h-box :size "auto"
                                                                 ;:justify :between :align :center
                                                    :height "10px"
                                                    :children [;(str running?)
                                                               (when runstreamed?
                                                                 [re-com/md-icon-button
                                                                  :md-icon-name "zmdi-tune"
                                                                  :style {:font-size "inherit"
                                                                          :padding "5px"
                                                                          :margin-right "-14px"
                                                                          :transform-origin "18px 17px"
                                                                          :margin-top "2px"}])
                                                               [re-com/md-icon-button
                                                                :md-icon-name (if running? "zmdi-refresh" "zmdi-play")
                                                                :class (when running? "rotate linear infinite")
                                                                :on-click (fn [] (when (not running?) ;(not (some (fn [x] (= x text)) @db/speech-log))
                                                                                   (let [fstr (str "running flow " flow-id (when overrides? " (with overrides)"))
                                                                                         w (/ (count fstr) 4.1)]
                                                                                               ;(ut/tapp>> [:fuck-me flow-id client-name overrides])
                                                                                     (ut/tracked-dispatch
                                                                                      [::wfx/request :default
                                                                                       {:message    {:kind :run-flow
                                                                                                     :flow-id flow-id
                                                                                                     :flowmap flow-id
                                                                                                     :no-return? true
                                                                                                     :opts (if (map? overrides)
                                                                                                             (merge base-opts
                                                                                                                    {:overrides overrides})
                                                                                                             base-opts)
                                                                                                                                ;;  :file-image {:flowmaps @(ut/tracked-subscribe [::flowmap-raw])
                                                                                                                                ;;               :opts @(ut/tracked-subscribe [::opts-map])
                                                                                                                                ;;               :zoom @db/pan-zoom-offsets
                                                                                                                                ;;               :flow-id flow-id
                                                                                                                                ;;               :flowmaps-connections @(ut/tracked-subscribe [::flowmap-connections])}
                                                                                                     :client-name client-name
                                                                                                     :keypath [:panels panel-key :views selected-view]}
                                                                                        :on-response [::http/socket-response]
                                                                                        :on-timeout [::http/timeout-response :run-flow-clover]
                                                                                        :timeout    50000000}])
                                                                                     (ut/dispatch-delay 800 [::http/insert-alert fstr w 1 5]))))
                                                                                              ;;  (ut/tracked-dispatch [::conn/click-parameter ;; kinda cheating, but feels better
                                                                                              ;;                      [:flow (keyword (str flow-id ">*running?"))] true])

                                                                :style {:font-size "inherit"
                                                                                  ;:opacity   0.25
                                                                        :padding "5px"
                                                                        :transform-origin "18px 17px"
                                                                        :margin-top "2px"
                                                                        :cursor    "pointer"}]
                                                               [re-com/box :child ""]]]]]))

                                  ;;  :run-flow-fn2 #(ut/tracked-dispatch [::wfx/request :default
                                  ;;                                     {:message    {:kind :run-flow
                                  ;;                                                   :flow-id %
                                  ;;                                                   :flowmap %
                                  ;;                                                   :opts {:increment-id? false}
                                  ;;                                                   :client-name @(ut/tracked-subscribe [::client-name])
                                  ;;                                                   :keypath [:panels panel-key :views selected-view]}
                                  ;;                                      :on-response [::http/socket-response]
                                  ;;                                      :on-timeout [::http/timeout-response]
                                  ;;                                      :timeout    500000}])

                                  ;;  :run-flow-fn #(ut/tapp>> [:cc (js->clj %)])

                         :case (fn [x] (ut/vectorized-case x))

                                  ;;  :scrubber22 (fn [[kk pm & [opts]]]
                                  ;;                [scrubber-panel true
                                  ;;                 @(ut/tracked-subscribe [::keypaths-in-params :param]) ;; leakable?
                                  ;;                 kk pm
                                  ;;                 (if opts
                                  ;;                   {:fm true :canvas? true :opts opts}
                                  ;;                   {:fm true :canvas? true})])

                         :scrubber (fn [[kk pm & [opts]]]
                                     [scrubber-panel true
                                                ;;@(ut/tracked-subscribe [::keypaths-in-params :param]) ;; leakable?
                                      @(rfa/sub ::keypaths-in-params  {:key-type :param})
                                      kk pm
                                      (if opts
                                        {:fm true :canvas? true :opts opts}
                                        {:fm true :canvas? true})])


                                  ; :data-color #(get @(ut/tracked-subscribe [::conn/data-colors]) (ut/data-typer %))

                                  ; :condi-fill-fn #(let [dd (js->clj % :keywordize-keys true)]
                                  ;                         (ut/tapp>> [:dd dd])
                                  ;                         (if (= (get dd :season) "Summer")
                                  ;                           "#ffffff" "#8884d8"))

                                  ; :on-block? (true? @over-block?)

                                  ; :vvvalues (vec (for [r @(ut/tracked-subscribe [::conn/sql-data [:locations-venue-drag-37-clone-40]])] (last (get r :ft))))


                         :Map Map
                         :Source Source
                         :Layer Layer
                         :Marker Marker

                         :Map/Map Map
                         :Map/Source Source
                         :Map/Layer Layer
                         :Map/Marker Marker

                                  ; :ReactMapGL Map/ReactMapGL
                                  ; :MapSource Map/Source
                                  ; :MapLayer Map/Layer
                                  ; :MapMarker Map/Marker
                         :viewport-params-fn #(ut/tracked-dispatch [::conn/click-parameter [panel-key]
                                                                    (select-keys
                                                                     (walk/keywordize-keys (js->clj (.-viewState %)))
                                                                     [:longitude :latitude :zoom])])
                         :test-params-fn #(ut/tracked-dispatch [::conn/click-parameter [panel-key] (str %)]) ;; (walk/keywordize-keys (js->clj (.-features %)))
                         :params> #(ut/tracked-dispatch [::conn/click-parameter [panel-key] (js->clj % :keywordize-keys true)])

                                   ;:viewport-params-fn #(ut/tapp>> (str %))
                                   ;;(fn [new-viewport] (reset! viewport new-viewport))

                         :as-e (fn [x] [reagent/as-element x])
                                   ;:add-commas (fn [x] (interpose "," x))
                                    ;:as-e reagent.core/as-element
                                    ;:as-e into
                                    ;:into (fn [x y] (vec
                                    ;                 (into x y)))
                                    ;:into (fn [x y] into)
                                    ;:into into
                                    ;:into into
                                    ;:grid 0
                                    ;:table honeycomb-table
                                    ;:dispatch ut/tracked-dispatch
                                    ;:click-param (fn [] (ut/tracked-dispatch [::conn/click-parameter [:user-sys] "test"]))
                         :text                  str
                                    ;:string str
                                  ;;  :string                (fn [x] (try
                                  ;;                                   (let [x (for [e x] (ut/safe-name e))] (apply str x))
                                  ;;                                   (catch :default _ (str x))))
                         :number                (fn [x] (str (nf x)))
                         :percent               (fn [x] (str (nf x) "%"))
                         :v-box                 re-com/v-box}
                        (walk-map-sizes
                         (/ px-width-int brick-size)
                         (/ px-height-int brick-size)
                         nil nil nil nil))]
          (swap! ut/clover-walk-singles-map assoc kk walk-map)
          walk-map))))

;; (js/console.info (time-expr (+ 1 8 1 9 9 9 9 9 9 9 1 9 9 9 9 9 ) "note" ))

;;(ut/tapp>> (ut/stats @ut/timing-data))
;;(ut/tapp>> @ut/timing-data)

(defn honeycomb [panel-key & [override-view fh fw replacement-view replacement-query]]         ;; can sub lots of this TODO. no need to pass it all
  ;;(ut/tapp>> [:repo panel-key override-view fh fw replacement-view replacement-query])
  (let [;block-map panel-map ;@(ut/tracked-subscribe [::panel-map panel-key]) ;(get workspace panel-key) ;; ?
        ;da-sched @(ut/tracked-subscribe [::da-sched])
        ;;all-sql-call-keys      @(ut/tracked-subscribe [::all-sql-call-keys]) ;(into {} (for [[k v] workspace] (get v :queries)))
        all-sql-call-keys      @(rfa/sub ::all-sql-call-keys)
        ;;all-view-keys          @(ut/tracked-subscribe [::panel-view-keys panel-key])
        all-view-keys          @(rfa/sub ::panel-view-keys {:panel-key panel-key})
        ;; sql-calls              (if replacement-view
        ;;                          (into {} (for [qid (cset/intersection (set all-sql-call-keys) (set (ut/deep-flatten replacement-view)))]
        ;;                            {qid @(ut/tracked-subscribe [::find-query qid])}))
        ;;                          @(ut/tracked-subscribe [::panel-sql-calls panel-key]))
        ;;sql-calls                @(ut/tracked-subscribe [::panel-sql-calls panel-key])
        sql-calls                @(rfa/sub ::panel-sql-calls {:panel-key panel-key})

        ;replacement-query-only?  (and (nil? replacement-view) replacement-query)
        ;replacement-view-only?   (and (nil? replacement-query) replacement-view)

        replacement-all?         (and (not (nil? replacement-query)) (not (nil? replacement-view)))
        sql-calls             (if replacement-query
                                ;(try (merge {} {override-view (-> (edn/read-string replacement-query)
                                ;                                  (dissoc :cache?) ;; always cache these
                                ;                                  (dissoc :refresh-every))}) ;; dont ever schedule them
                                ;     (catch :default _ sql-calls))
                                (merge {} replacement-query)
                                sql-calls)
        ;all-sql-call-keys      (if replacement-query (merge (keys sql-calls) all-sql-call-keys) all-sql-call-keys)
        all-keys               (vec (into (vec (sort (into (vec (keys sql-calls)) all-view-keys))) [:virtual-view]))

        ;orig-override-view     override-view

        selected-view          @(ut/tracked-sub ::selected-view-alpha {:panel-key panel-key}) ;(or  :view)
        override-view          (cond (= override-view :*) selected-view ;; default editor "everything" view
                                     (and (not (nil? override-view)) (some #(= % override-view) all-keys)) override-view ;; not nil and exists, do it
                                     (and (not (nil? override-view)) (not (some #(= % override-view) all-keys))) ; not nil, and doesnt exist - get first
                                     (first all-keys)
                                     :else nil)             ; else just do wahtever
        ; _ (ut/tapp>> [:sneaky panel-key orig-override-view override-view all-view-keys all-keys (and (not (nil? override-view)) (some #(= % override-view) all-keys))])
        ;; _ (ut/tapp>> [:kwrep (ut/namespaced-swapper "this-block" (ut/replacer (str panel-key) #":" "") {:a :*this-block*/key-a
        ;;                                                                                             :b {:c [:*this-block*/key-b
        ;;                                                                                                     :another/key "some value!!" 1 2 3 [:bananas/this.123]
        ;;                                                                                                     :*this-block*/key-c]
        ;;                                                                                                 [:key :view/*this-block*.my-view :*this-block*/key-b] {:view/*this-block*.my-view 123}
        ;;                                                                                                 :beans/beabs [3 4 5]
        ;;                                                                                                 :d [:another/key :*this-block*/key-d]
        ;;                                                                                                 :e {:f :*this-block*/key-e
        ;;                                                                                                     :g {:h :*this-block*/key-f}}}})])
        w                      (if (not (nil? override-view)) 11 @(ut/tracked-subscribe [::panel-width panel-key]))
        h                      (if (not (nil? override-view)) 8.7 @(ut/tracked-subscribe [::panel-height panel-key]))
        w                      (if fh fh w)
        h                      (if fw fw h)
        ww                     (* w brick-size)
        hh                     (* h brick-size)
        client-name @(ut/tracked-sub ::client-name {})
        ;editor-panel? false ;; legacy, remove
        ;name @(ut/tracked-subscribe [::panel-name brick-vec-key])
        px-width-int           (- ww 100)
        px-height-int          (- hh 105)
       ; _ (ut/tapp>> [:pp panel-key override-view sql-calls])
        ;px-widthf-int (- ww 20)  ;(- ww 100)
        ;px-heightf-int (- hh 55) ;(- hh 105)


        selected-view          (if override-view override-view selected-view)
        ;; all-drops              @(ut/tracked-subscribe [::all-drops-of :*])
        all-drops              @(ut/tracked-sub ::all-drops-of-alpha {:ttype :*})

        drop-walks             (into {} (for [d all-drops]
                                          {d (fn [x] ;(run-drop x)
                                               [:box :child [:string x "hey"]
                                                :style {:color "red"}])}))
                                          ;; drop map will have unresolved stuff, so we want to resolve it early-ish
        ;_ (when (not (empty? drop-walks)) (ut/tapp>> [:drop-walks drop-walks]))
        walk-map              (clover-walk-singles panel-key client-name px-width-int px-height-int ww hh w h selected-view override-view)

        ;sql-aliases-used @(ut/tracked-subscribe [::panel-sql-aliases-in-views panel-key]) ;; pre :layout, needs to scrape postwalked bodies as well

        connection-id          @(ut/tracked-subscribe [::panel-connection-id panel-key]) ;(get block-map :connection-id)
        ; (get block-map :queries)
        ;;body                   @(ut/tracked-subscribe [::views panel-key])
        body                   @(rfa/sub ::views {:panel-key panel-key})
       ; _ (ut/tapp>> [:body replacement-view body])
        body (if replacement-view
               replacement-view ;{selected-view replacement-view}
              ; (try {selected-view (edn/read-string replacement-view)}
              ;      (catch :default _ body))
               body)



        orig-body              body

        single-view?           @(ut/tracked-subscribe [::is-single-view? panel-key])
        no-view?               @(ut/tracked-subscribe [::has-no-view? panel-key])

        is-layout?             @(ut/tracked-subscribe [::is-layout? panel-key selected-view])

        body                   (ut/namespaced-swapper "this-block" (ut/replacer (str panel-key) #":" "") body) ;; eyes emoji - 10/17/23 (earliest swap needed?)

        ;;;removing for now;;;body                   (view-alias-replace body panel-key selected-view) ; ; ; VIEW ALIAS. DRAGONS.

        sql-aliases-used       @(ut/tracked-sub ::panel-sql-aliases-in-views-body {:panel-key panel-key :body body})

        ;all-vsql-call-keys @(ut/tracked-subscribe [::all-vsql-call-keys]) ;LATER
        vsql-calls             (if is-layout?
                                 @(ut/tracked-subscribe [::all-vsql-calls]) ;; get all just in case they don't get resolved out in time (spoiler: they dont)
                                 @(ut/tracked-subscribe [::panel-vsql-calls panel-key]))
        vsql-calls             (ut/namespaced-swapper "this-block" (ut/replacer (str panel-key) #":" "") vsql-calls)
        ;used-vsql-calls (cset/intersection (set (keys vsql-calls)) ) ;LATER
        vsql-replace-map       (into {} (for [[k v] vsql-calls]
                                          ;; search the map for datasets
                                          (let [look-for-datasets   (cset/intersection (set (ut/deep-flatten v)) (set all-sql-call-keys))
                                                data-subbed-rep-map (into {} (for [ds look-for-datasets]
                                                                               {ds
                                                                                ;;@(ut/tracked-subscribe [::conn/sql-data [ds]])
                                                                                @(rfa/sub ::conn/sql-data-alpha {:keypath [ds]})}))
                                                data-subbed-src     (ut/postwalk-replacer data-subbed-rep-map v)]
                                                ;data-subbed-modded (read-string (ut/replacer (str data-subbed-src) #":box "
                                                ;                                 (str ":box :attr {:on-click #(re-frame.core/dispatch [::rvbbit-frontend.connections/click-parameter [" k "] \"test\"])} ")))

                                            ; (ut/tapp>> [:vsql k (vsql-map v) data-subbed-src])
                                            {k (vsql-map data-subbed-src)})))
        ;body orig-body ;@(ut/tracked-subscribe [::views panel-key]) ;(get block-map :views)
        ;layered-body @(ut/tracked-subscribe [::layered-oz-views panel-key])

        ; selected-view-is-ref? (and
        ;                       ; false
        ;                        ;(keyword? (first (get body selected-view)))
        ;                        ;(= 1 (count (get body selected-view)))
        ;                        (cstr/includes? (str (get body selected-view)) ":view/")
        ;                        )
        selected-view-is-sql?  (true? (some #(= selected-view %) (keys sql-calls)))
        override-view-is-sql?  (true? (some #(= override-view %) (keys sql-calls)))

        ;params-used @(ut/tracked-subscribe [::panel-parameters-in-views panel-key])
        valid-body-params      (ut/deep-flatten (merge ;;@(ut/tracked-subscribe [::valid-body-params panel-key])
                                                 @(rfa/sub ::valid-body-params {:panel-key panel-key})
                                                       ;;@(ut/tracked-subscribe [::valid-body-params-all-condis])
                                                 @(rfa/sub ::valid-body-params-all-condis)
                                                       ;; ^^ all condis, just in case? TODO got back to ONLY condis detected in this view if expensive...
                                                       ;; ^^ perhaps they are being resolved out in original "get condis from params"?
                                                       ;;@(ut/tracked-subscribe [::valid-body-params-in body]) ;; extra? and expensive? by ONLY for view aliased views...
                                                 @(rfa/sub ::valid-body-params-in {:body body})
                                                       ;;@(ut/tracked-subscribe [::valid-body-params-in vsql-calls])
                                                 @(rfa/sub ::valid-body-params-in {:body vsql-calls})))
        possible-datasets-used (set (for [e valid-body-params] (keyword (nth (ut/splitter (ut/safe-name e) #"/") 0))))
        used-datasets          (cset/union (set sql-aliases-used) (cset/intersection possible-datasets-used (set all-sql-call-keys)))
        used-datasets          (if replacement-all? (into used-datasets (keys replacement-query)) used-datasets)
        ;valid-sql-params @(ut/tracked-subscribe [::valid-sql-params panel-key])
        data-walks             (into {} (for [k used-datasets] ;all-sql-call-keys]
                                          {k
                                           ;;@(ut/tracked-subscribe [::conn/sql-data [k]])
                                           @(rfa/sub ::conn/sql-data-alpha {:keypath [k]})}))
                                           ;(keyword (str (ut/safe-name k) "-text")) @(ut/tracked-subscribe [::conn/sql-data-text [k]])
                                           ;(keyword (str (ut/safe-name k) "->text")) @(ut/tracked-subscribe [::conn/sql-data-text [k]])
                                           ;            (keyword (str (ut/safe-name k) "/as-text")) @(ut/tracked-subscribe [::conn/sql-data-text [k]])
                                           ;(keyword (str (ut/safe-name k) "-table")) @(ut/tracked-subscribe [::sql-data-table-magic panel-key [k]])
                                           ;(keyword (str (ut/safe-name k) "..table")) @(ut/tracked-subscribe [::sql-data-table-magic panel-key [k]])
                                           ;(keyword (str (ut/safe-name k) "->table")) @(ut/tracked-subscribe [::sql-data-table-magic panel-key [k]])
                                           ;(keyword (str (ut/safe-name k) "-magic")) @(ut/tracked-subscribe [::sql-data-table-magic panel-key [k]])
                                           ;          (keyword (str (ut/safe-name k) "/as-table")) @(ut/tracked-subscribe [::sql-data-table-magic panel-key [k]])
                                           ;(keyword (str (ut/safe-name k) "-boxes")) @(ut/tracked-subscribe [::conn/sql-data-boxes [k] true nil])
                                           ;(keyword (str (ut/safe-name k) "-boxes-values")) @(ut/tracked-subscribe [::conn/sql-data-boxes-values [k] clickable? (get block-map :boxes-values-style)])
                                           ;(keyword (str (ut/safe-name k) "-boxes-values")) (sql-data-boxes-values [k] clickable? (get block-map :boxes-values-style))
                                           ;         (keyword (str (ut/safe-name k) "/as-boxes-values")) (sql-data-boxes-values2 [k] nil)
                                           ;        (keyword (str (ut/safe-name k) "/as-boxes")) (sql-data-boxes-values2 [k] nil)
                                           ;         (keyword (str (ut/safe-name k) "/parameter")) @(ut/tracked-subscribe [::conn/clicked-parameter [k]])
                                           ;         (keyword (str (ut/safe-name k) "/*")) @(ut/tracked-subscribe [::conn/clicked-parameter [k]])
                                           ;(keyword (str (ut/safe-name k) "-parameter")) @(ut/tracked-subscribe [::conn/clicked-parameter [k]])
                                           ;(keyword (str (ut/safe-name k) "->parameter")) @(ut/tracked-subscribe [::conn/clicked-parameter [k]])

        ;value-walks

        ;workspace-params (into {} (for [k (ut/deep-flatten body)] ;; slowish since all bodys with params need to be evaled when any click params are activated
        ;                            (if (cstr/includes? (str k) ".")
        ;                              {k @(ut/tracked-subscribe [::conn/clicked-parameter-key [k]])} {})))
        workspace-params       (into {} (for [k valid-body-params] ;; deref here?
                                          {k
                                           ;@(ut/tracked-subscribe [::conn/clicked-parameter-key [k]])
                                           @(rfa/sub ::conn/clicked-parameter-key-alpha {:keypath [k]})}))
        ;;_ (when (= panel-key :block-55) (ut/tapp>> [:workspace-params workspace-params]))
        value-walks-targets    (filter #(and (cstr/includes? (str %) ".") (not (cstr/includes? (str %) ".*"))) valid-body-params)
        value-walks            (into {} (for [k value-walks-targets] ;all-sql-call-keys]
                                          (let [fs    (ut/splitter (ut/safe-name k) "/")
                                                gs    (ut/splitter (last fs) ".")
                                                ds    (keyword (first fs))
                                                row   (try (int (last gs)) (catch :default _ "label"))
                                                ;row (int (last gs))
                                                field (keyword (first gs))]
                                            {k (if (not (integer? row))
                                                 ;(get-in @(ut/tracked-subscribe [::conn/sql-data [ds]]) [row field])
                                                 (str field)
                                                 (get-in
                                                  ;;@(ut/tracked-subscribe [::conn/sql-data [ds]])
                                                  @(rfa/sub ::conn/sql-data-alpha {:keypath [ds]})
                                                  [row field]))})))

        ;;condi-walks-targets    (distinct (filter #(cstr/includes? (str %) "condi/") valid-body-params))
        condi-walks-targets    (filter #(cstr/includes? (str %) "condi/") valid-body-params) ;; no need for distinct now since deep-flatten returns a set
        condi-walks            (into {} (for [k condi-walks-targets]
                                          {k ;;@(ut/tracked-subscribe [::conn/condi-value (keyword (last (ut/splitter (ut/safe-name k) "/")))])
                                           @(ut/tracked-sub ::conn/condi-value {:condi-key (keyword (last (ut/splitter (ut/safe-name k) "/")))})
                                           }))
        ;sql-params (into {} (for [k (ut/deep-flatten sql-calls)] ;; slowish since all bodys with params need to be evaled when any click params are activated
        ;                      (if (cstr/includes? (str k) ".")
        ;                        {k @(ut/tracked-subscribe [::conn/clicked-parameter-key [k]])} {})))
        ;sql-params (into {} (for [k valid-sql-params] ;; deref here?
        ;                      ;(if (cstr/includes? (str k) ":query/")
        ;                        ;{k @(ut/tracked-subscribe [::resolve-sql-alias (keyword (ut/replacer (str k) ":query/" ""))])}
        ;                      {k @(ut/tracked-subscribe [::conn/clicked-parameter-key [k]])}));)
        ;gen-sql-params (fn [q] (let [valid-sql-params (vec (remove nil? (for [k (ut/deep-flatten q)]
        ;                                                                  (when (and (keyword? k) (cstr/includes? (str k) "/")) k))))]
        ;                         (into {} (for [k valid-sql-params]
        ;                                    {k @(ut/tracked-subscribe [::conn/clicked-parameter-key [k]])}))))
        ;sql-params-replace-recur
        ; AFTER DINNER!
        ;// - remove all editor logic from honeycomb-reader!
        ; - create simple wrapper fns for body view calls, remove the old subs - remove old "boxes" hack calls
        ; - take body - replace used data calls with SQL data subs - tables only
        ;             - replace used data parmeters with clicked param subs - part is fine as is, I think?
        ;             - replace :vsql calls with vsql fn that has it's own sql-data subs inside it
        ; - take sqls - replace table aliases with sql def subqueries (w/o limit or order, just like drag)
        ;             - run them (might be a good time to combine the honey-call and xhoney call on server, since we are in there)
        ;pre-walk-targets []

        ;explode-kvps (fn [x] (for [e kv] ))
        ;; get-kv-map             (fn [obody] (into {} (for [p (ut/kvpaths obody)] {p (get-in obody p)})))
        ;; get-kv-map-nested-keys (fn [obody] (into {} (let [obd (ut/unpack-keys obody)]
        ;;                                               (for [p (ut/kvpaths obd)] {p (get-in obd p)}))))
        ;; kv-map-fn              (if false                    ; is-layout?
        ;;                          get-kv-map-nested-keys get-kv-map) ;; nested is more expensive(ish?), so will try to avoid
        ;; if-walk-map            (fn [obody] (let [;obody (ut/postwalk-replacer condi-walks orig-body)
        ;;                                          kps       (kv-map-fn obody) ;(into {} (for [p (ut/kvpaths obody)] {p (get-in obody p)}))
        ;;                                          logic-kps (into {} (for [[_ v]
        ;;                                                                   (into {} (filter #(cstr/starts-with? (str (last %)) "[:if") kps))]
        ;;                                                               (let [[_ l this that] v]
        ;;                                                                 ;(ut/tapp>> [:if-walk panel-key kps l this that])
        ;;                                                                 {v (if l this that)})))]
        ;;                                      (ut/postwalk-replacer logic-kps obody)))
        into-walk-map2           (fn [obody] (let [;obody (walk/postwalk-replace condi-walks orig-body)
                                                   kps       (ut/extract-patterns obody :into 3) ;(kv-map-fn obody) ;(into {} (for [p (ut/kvpaths obody)] {p (get-in obody p)}))
                                                   logic-kps (into {} (for [v kps]
                                                                        (let [[_ this that] v]
                                                                                ;(ut/tapp>> [:if-walk panel-key kps l this that])
                                                                          {v (into this that)})))]
                                               (walk/postwalk-replace logic-kps obody)))
        if-walk-map2           (fn [obody] (let [;obody (walk/postwalk-replace condi-walks orig-body)
                                                 kps       (ut/extract-patterns obody :if 4) ;(kv-map-fn obody) ;(into {} (for [p (ut/kvpaths obody)] {p (get-in obody p)}))
                                                 logic-kps (into {} (for [v kps]
                                                                      (let [[_ l this that] v]
                                                                        ;(ut/tapp>> [:if-walk panel-key kps l this that])
                                                                        {v (if
                                                                            (if (vector? l)
                                                                              (resolver/logic-and-params l nil)
                                                                              l)
                                                                             this that)})))]
                                             (walk/postwalk-replace logic-kps obody)))
        ;; when-walk-map          (fn [obody] (let [kps       (kv-map-fn obody) ;(into {} (for [p (ut/kvpaths obody)] {p (get-in obody p)}))
        ;;                                          logic-kps (into {} (for [[_ v]
        ;;                                                                   (into {} (filter #(cstr/starts-with? (str (last %)) "[:when") kps))]
        ;;                                                               (let [[_ l this] v]
        ;;                                                                 {v (when l this)})))]
        ;;                                      (walk/postwalk-replace logic-kps obody)))
        when-walk-map2         (fn [obody] (let [kps       (ut/extract-patterns obody :when 3)
                                                 logic-kps (into {} (for [v kps]
                                                                      (let [[_ l this] v]
                                                                        {v (when l this)})))]
                                             (walk/postwalk-replace logic-kps obody)))
        ;; =-walk-map             (fn [obody] (let [kps       (kv-map-fn obody) ;(into {} (for [p (ut/kvpaths obody)] {p (get-in obody p)}))
        ;;                                          logic-kps (into {} (for [[_ v]
        ;;                                                                   (into {} (filter #(cstr/starts-with? (str (last %)) "[:=") kps))]
        ;;                                                               (let [[_ that this] v]
        ;;                                                                 ;(ut/tapp>> [:=-walk panel-key kps this that])
        ;;                                                                 {v (= (str that) (str this))})))]
        ;;                                      ;(ut/tapp>> [:=-walk/logic-kps logic-kps kps workspace-params])
        ;;                                      (walk/postwalk-replace logic-kps obody)))
        =-walk-map2            (fn [obody] (let [kps       (ut/extract-patterns obody := 3)
                                                 logic-kps (into {} (for [v kps]
                                                                      (let [[_ that this] v]
                                                                        ;(ut/tapp>> [:=-walk panel-key kps this that])
                                                                        {v (= (str that) (str this))})))]
                                             ;(ut/tapp>> [:=-walk/logic-kps logic-kps kps workspace-params])
                                             (walk/postwalk-replace logic-kps obody)))
        auto-size-walk-map2            (fn [obody] (let [kps       (ut/extract-patterns obody :auto-size-px 2)
                                                         logic-kps (into {} (for [v kps]
                                                                              (let [[_ l] v]
                                                                        ;(ut/tapp>> [:=-walk panel-key kps this that])
                                                                                {v (ut/auto-font-size-px l h w)})))] ;(= (str that) (str this))

                                             ;(ut/tapp>> [:=-walk/logic-kps logic-kps kps workspace-params])
                                                     (walk/postwalk-replace logic-kps obody)))
        ;; onclick-walk-map       (fn [obody] (let [kps       (kv-map-fn obody) ;(into {} (for [p (ut/kvpaths obody)] {p (get-in obody p)}))
        ;;                                          logic-kps (into {} (for [[_ v]
        ;;                                                                   (into {} (filter #(cstr/starts-with? (str (last %)) "[:set-parameter") kps))]
        ;;                                                               (let [[_ pkey pval] v
        ;;                                                                     raw-param-key (get-in vsql-calls (conj (vec (first (filter #(= (last %) :on-click)
        ;;                                                                                                                                (ut/kvpaths vsql-calls)))) 1) pkey)]
        ;;                                                                 ;(ut/tapp>> [:on-click-hack panel-key v raw-param-key])
        ;;                                                                 {v (fn [] (ut/tracked-dispatch [::conn/click-parameter [panel-key] {raw-param-key pval}]))})))]
        ;;                                      ;(ut/tapp>> [:set-param/logic-kps logic-kps kps])
        ;;                                      (walk/postwalk-replace logic-kps obody)))
        onclick-walk-map2      (fn [obody] (let [kps       (ut/extract-patterns obody :set-parameter 3)
                                                 logic-kps (into {} (for [v kps]
                                                                      (let [[_ pkey pval] v
                                                                            raw-param-key (get-in vsql-calls (conj (vec (first (filter #(= (last %) :on-click)
                                                                                                                                       (ut/kvpaths vsql-calls)))) 1) pkey)]
                                                                        ;(ut/tapp>> [:on-click-hack panel-key v raw-param-key])
                                                                        {v (fn [] (ut/tracked-dispatch [::conn/click-parameter [panel-key] {raw-param-key pval}]))})))]
                                             ;(ut/tapp>> [:set-param/logic-kps logic-kps kps])
                                             (walk/postwalk-replace logic-kps obody)))

        map-walk-map2            (fn [obody] (let [kps       (ut/extract-patterns obody :map 3)
                                                   logic-kps (into {} (for [v kps]
                                                                        (let [[_ that this] v]
                                                                        ;(ut/tapp>> [:=-walk panel-key kps this that])
                                                                          {v ;(resolver/logic-and-params
                                                                           (resolver/logic-and-params (if (vector? this)
                                                                                                        (vec (for [r this]
                                                                                                               (last (get r that))))
                                                                                                               ;(get r that)

                                                                                                        (vec (for [r
                                                                                                                  ;;  @(ut/tracked-subscribe [::conn/sql-data [this]])
                                                                                                                   @(rfa/sub ::conn/sql-data-alpha {:keypath [this]})] (last (get r that))))) panel-key)})))]
                                                                             ; panel-key)
                                                                         ;(= (str that) (str this))

                                              ; (ut/tapp>> [:=map/logic-kps logic-kps kps  ])
                                               (walk/postwalk-replace logic-kps obody)))

        ;; map-walk-map3            (fn [obody] (let [kps       (ut/extract-patterns obody :map2 3) ;;; TEMP!!
        ;;                                            logic-kps (into {} (for [v kps]
        ;;                                                                 (let [[_ that this] v]
        ;;                                                                 ;(ut/tapp>> [:=-walk panel-key kps this that])
        ;;                                                                   {v ;(resolver/logic-and-params
        ;;                                                                    (if (vector? this)
        ;;                                                                      (vec (for [r this]
        ;;                                                                            ;(last (get r that))
        ;;                                                                             (get r that)))
        ;;                                                                      (vec (for [r @(ut/tracked-subscribe [::conn/sql-data [this]])] (last (get r that)))))})))]
        ;;                                                                      ; panel-key)
        ;;                                                                  ;(= (str that) (str this))

        ;;                                        ;(ut/tapp>> [:=map2/logic-kps logic-kps kps  ])
        ;;                                        (walk/postwalk-replace logic-kps obody)))

        ;; scrubber-walk-map2            (fn [obody] (try
        ;;                                             (let [kps      (ut/extract-patterns obody :scrubber2 2)
        ;;                                                   kps2       (ut/extract-patterns obody :scrubber3 3)
        ;;                                                   logic-kps (into {} (for [v kps]
        ;;                                                                        (let [[_ this] v]
        ;;                                                                        ;(ut/tapp>> [:scrubber2 panel-key kps this])
        ;;                                                                          {v [scrubber-panel true
        ;;                                                                              @(ut/tracked-subscribe [::keypaths-in-params :param])
        ;;                                                                              :param
        ;;                                                                              (str (last (ut/splitter (str this) #"/")))
        ;;                                                                              {:fm true :canvas? true}]})))
        ;;                                                   logic-kps2 (into {} (for [v kps2]
        ;;                                                                         (let [[_ this opts] v]
        ;;                                                                         ;(ut/tapp>> [:scrubber3 panel-key kps this])
        ;;                                                                           {v [scrubber-panel true
        ;;                                                                               @(ut/tracked-subscribe [::keypaths-in-params :param])
        ;;                                                                               :param
        ;;                                                                               (str (last (ut/splitter (str this) #"/")))
        ;;                                                                               {:fm true :canvas? true :opts opts}]})))
        ;;                                                   ;; logic-kps2 {}
        ;;                                                   ]
        ;;                                               (walk/postwalk-replace (merge logic-kps2 logic-kps) obody))
        ;;                                             (catch :default e (do (ut/tapp>> [:scrubber-honey-comb-error (str e)]) obody))))

        string-walk            (fn [num obody] (let [kps       (ut/extract-patterns obody :string3 num)
                                                     logic-kps (into {} (for [v kps]
                                                                          (let [[_ & this] v]
                                                                            {v (apply str this)})))]
                                                 (walk/postwalk-replace logic-kps obody)))

        push-walk            (fn [obody] (let [kps       (ut/extract-patterns obody :push> 2) ;; is there a less expensive way to do this? unsure. 3/2/24
                                               logic-kps (into {} (for [v kps]
                                                                    (let [[_ & this] v
                                                                          [[flow-id bid value alert?]] this]
                                                                      {v  #(ut/tracked-dispatch [::push-value flow-id bid value alert?])})))]
                                           (walk/postwalk-replace logic-kps obody)))

        push-walk-fn            (fn [obody] (let [kps       (ut/extract-patterns obody :push>> 2) ;; is there a less expensive way to do this? unsure. 3/2/24
                                                  logic-kps (into {} (for [v kps]
                                                                       (let [[_ & this] v
                                                                             [[flow-id bid value alert?]] this]
                                                                         {v  #(ut/tracked-dispatch [::push-value flow-id bid % alert?])})))]
                                              (walk/postwalk-replace logic-kps obody)))

        get-in-walk            (fn [obody] (let [kps       (ut/extract-patterns obody :get-in 2)
                                                 logic-kps (into {} (for [v kps]
                                                                      (let [[_ [data kp]] v]
                                                                        {v (get-in data kp)})))]
                                             (walk/postwalk-replace logic-kps obody)))

        ;; keymerge-walk            (fn [obody] (let [kps       (ut/extract-patterns obody :keymerge 3)
        ;;                                            logic-kps (into {} (for [v kps]
        ;;                                                                 (let [[_ that this] v]
        ;;                                                                   {v (keyword (ut/replacer (str that this) #":" ""))})))]
        ;;                                        (walk/postwalk-replace logic-kps obody)))

        ;; honey-walk            (fn [obody] (let [kps       (ut/extract-patterns obody :*render* 2)
        ;;                                         logic-kps (into {} (for [v kps]
        ;;                                                              (let [[_ data_d] v]
        ;;                                                                (ut/tapp>> [:data_d data_d])
        ;;                                                                {v ;(keyword (ut/replacer (str that this) #":" ""))
        ;;                                                                 (let [queries (get data_d :queries)
        ;;                                                                  ;views {:view (get data_d :view)}
        ;;                                                                       qkeys (into {} (for [q (keys queries)]
        ;;                                                                                        {q (keyword (str (ut/replacer (str q) #":" "") "-hist-" (rand-int 123) (hash data_d)))}))
        ;;                                                                       ndata (walk/postwalk-replace qkeys data_d)]
        ;;                                                                      ; ndata (assoc-in ndata [:views :view1] (get ndata :view))
        ;;                                                                     ;  ndata (walk/postwalk-replace {{:state ["de" "dc" "fl" "ca" "ct" "bc" "az" "co" "ar"]} ["de" "dc" "fl" "ca" "co" "ct"]
        ;;                                                                     ;                                [:= nil :country] [:= 1 1]} ndata)
        ;;                                                                      ; ndata data_d

        ;;                                                                   (ut/tapp>> [:qkeys panel-key qkeys ndata])
        ;;                                                                   [honeycomb :block-213 ;panel-key
        ;;                                                                            ;(or (first (keys views)) (first (keys queries)))
        ;;                                                                    :new-view ;(get data_d :selected-view)
        ;;                                                                    nil nil
        ;;                                                                    {:new-view (get ndata :view)} ;{:views {:view (get ndata :view)}} ;views ;(walk/postwalk-replace qkeys views)
        ;;                                                                    (get ndata :queries)])})))] ;(walk/postwalk-replace qkeys queries)

        ;;                                     (walk/postwalk-replace logic-kps obody)))

        ;; drop-walk-replace            (fn [obody keyname] (let [kps       (ut/extract-patterns obody keyname 2)
        ;;                                                        logic-kps (into {} (for [v kps]
        ;;                                                                             (let [[_ that this] v]
        ;;                                                                               {v (= (str that) (str this))})))]
        ;;                                                    (walk/postwalk-replace logic-kps obody)))

        run-rs-flow (fn [flow-id flow-id-inst panel-key override-merge-map]
                      (let [client-name @(ut/tracked-subscribe [::client-name])
                            base-opts {:increment-id? false :instance-id flow-id-inst}
                            ;running-key (keyword (str "flow/" flow-id ">*running?"))
                            running-key (keyword (str "flow/" flow-id-inst ">*running?"))
                            running?
                            ;;@(ut/tracked-subscribe [::conn/clicked-parameter-key [running-key]])
                            @(rfa/sub ::conn/clicked-parameter-key-alpha {:keypath [running-key]})
                            overrides (merge @(ut/tracked-subscribe [::runstream-overrides flow-id]) override-merge-map)]
                            ;overrides? (not (empty? overrides))

                        (when (not running?) ;(not (some (fn [x] (= x text)) @db/speech-log))
                          (let [fstr (str panel-key " DROP running flow " flow-id " (with double overrides)")
                                w (/ (count fstr) 4.1)]
                            ;(ut/tapp>> [:run? flow-id-inst])
                            (ut/tracked-dispatch [::wfx/request :default
                                                  {:message    {:kind :run-flow
                                                                :flow-id flow-id
                                                                :flowmap flow-id
                                                                :no-return? true
                                                                :opts (merge base-opts {:overrides overrides})
                                                              ;; :file-image {:flowmaps @(ut/tracked-subscribe [::flowmap-raw])
                                                              ;;              :opts @(ut/tracked-subscribe [::opts-map])
                                                              ;;              :zoom @db/pan-zoom-offsets
                                                              ;;              :flow-id flow-id
                                                              ;;              :flowmaps-connections @(ut/tracked-subscribe [::flowmap-connections])}
                                                                :client-name client-name
                                                                :keypath [:panels panel-key :views selected-view]}
                                                   :on-response [::http/socket-response]
                                                   :on-timeout [::http/timeout-response :run-rs-flow]
                                                   :timeout    50000000}])
                            (ut/dispatch-delay 800 [::http/insert-alert [:box :child fstr :style {:font-size "14px"}] w 0.5 5])))))
                            ;; (ut/tracked-dispatch [::conn/click-parameter ;; kinda cheating, but feels better
                            ;;                     [:flow (keyword (str flow-id ">*running?"))] true])


        run-drop (fn [[drop-id _] val]
                   (let [drop-deets @(ut/tracked-subscribe [::drop-details drop-id])
                         flow-id (get drop-deets :flow-id)
                         ;;flow-id-inst (str flow-id "__" (hash val)) ;; unique flow run instance so we can do multiple, etc
                         flow-id-inst (str flow-id "__" (hash panel-key)) ;; unique flow run instance so we can do multiple, etc
                         drop-overrides {(get drop-deets :in) val}
                         out (get drop-deets :out)
                         out-param (keyword (str "flow/" flow-id-inst ">" out))]
                      ;; :flow/choppa-my-addy>:addy-baddie
                    ;; (ut/tapp>> [:drop-deets out-param val drop-deets @drop-last-tracker-refs @drop-last-tracker])
                     ;;(ut/tapp>> [:dd drop-id val out-param flow-id flow-id-inst w h])
                    ;;  [:box
                    ;;   :style {:color "red"}
                    ;;   :child [:strings "we dropped! " drop-id " " val]]

                     (when (and
                            ;;@(ut/tracked-subscribe [::auto-run-and-connected?])
                            @(rfa/sub ::auto-run-and-connected? {})
                            ;(not= (hash [drop-id val]) (get-in @drop-last-tracker [panel-key drop-id val]))
                            (not= (hash val) (get-in @drop-last-tracker [panel-key drop-id])))
                       ;(swap! drop-last-tracker assoc-in [panel-key drop-id val] (hash [drop-id val]))
                       ;(ut/tapp>> [:drop-running? drop-id val out-param flow-id flow-id-inst])
                       (swap! drop-last-tracker assoc-in [panel-key drop-id] (hash val))
                       (swap! drop-last-tracker-refs assoc [panel-key drop-id] out-param)
                       (run-rs-flow flow-id flow-id-inst panel-key drop-overrides))

                     [honeycomb-fragments
                      ;; [:box
                      ;;  :size "auto" :align :center :justify :center
                      ;;  :style {:color "red" :border "1px solid white"}
                      ;;  :child [:string :param/butter-tits?]]
                      ;[:if out-param out-param "nil?"]
                      [:string out-param]
                      w h]))
                      ;h w


        drop-walk-replace (fn [keynames obody] ;; swapped order since we need to pass a list of keynames
                            (reduce (fn [body keyname]
                                      (let [kps       (ut/extract-patterns body keyname 2)
                                            logic-kps (into {} (for [v kps]
                                                                 (let [[_ val] v]
                                                                   {v (run-drop v val)})))]
                                        (walk/postwalk-replace logic-kps body)))
                                    obody
                                    keynames))












        ;[bricks/scrubber-panel true @(ut/tracked-subscribe [::bricks/keypaths-in-params key-type]) key-type (get @param-search key-type) {:fm true}]


        ;body (ut/postwalk-replacer walk-map body)
        ;body (ut/postwalk-replacer data-walks body)
        ;body (ut/postwalk-replacer workspace-params body)
        ;body (if (string? body) [re-com/box :child "YOOOOO"] body)
        ;_ (when (= panel-key :block-5474) (ut/tapp>> [:prebody body]))
        ;obody-key-set (set (filter keyword? (ut/deep-flatten body))) ;; cache in atom if unchanged? cheaper? TODO


        obody-key-set (ut/body-set body)
        ;;has-fn? (fn [k] (some #(= % k) obody-key-set))
        has-fn? (fn [k] (contains? obody-key-set k)) ;; faster than some on a set since it will stop when found first instance
        ;;has-drops? (not (empty? (cset/intersection (set all-drops) obody-key-set)))
        has-drops? (boolean (some obody-key-set all-drops)) ;; faster?
        ;;_ (ut/tapp>> [:all-drops all-drops has-drops? panel-key])
        ;pre-body               body
        ;; _ (when (= panel-key :block-1774) (ut/tapp>> [:prebody selected-view body]))
        body                    ;;(ut/timed
                                 (cond->> body
                                 true (ut/namespaced-swapper "this-block" (ut/replacer (str panel-key) #":" ""))
                                    ;;(walk/postwalk-replace walk-map) ;;; moved up from after like 6470 - 10/17/23
                                 (has-fn? :*this-block*) (ut/postwalk-replacer {:*this-block* panel-key})
                                 (ut/ne? value-walks) (ut/postwalk-replacer value-walks)
                                 (ut/ne? condi-walks) (ut/postwalk-replacer condi-walks)
                                 (ut/ne? data-walks) (ut/postwalk-replacer data-walks)
                                 (ut/ne? vsql-replace-map) (ut/postwalk-replacer vsql-replace-map) ;;(walk/postwalk-replace walk-map vsql-replace-map))
                                    ;scrubber-walk-map2
                                    ;keymerge-walk


                                 (has-fn? :map) map-walk-map2

                                    ;(walk/postwalk-replace {:=block-10980/season :block-10980/season})

                                 (ut/ne? workspace-params) (ut/postwalk-replacer workspace-params)

                                    ;(walk/postwalk-replace if-walk-map)

                                   ; map-walk-map3
                                   ; honey-walk

                                 (has-fn? :get-in) get-in-walk

                                 (has-fn? :=) =-walk-map2             ;; test, needs to be first - "and" after... THEN if...
                                 (has-fn? :if) if-walk-map2            ;; ifs needs special treatment - must replace BEFORE reg view vecs
                                 (has-fn? :when) when-walk-map2          ;; test!
                                 (has-fn? :set-parameter) onclick-walk-map2       ;; test!
                                 (has-fn? :into) into-walk-map2
                                 (has-fn? :auto-size-px) auto-size-walk-map2

                                 (has-fn? :string3) (string-walk 2) ;; TODO, remove all these extra string replacements and make a [:string & x] ver
                                 (has-fn? :string3) (string-walk 3)
                                 (has-fn? :string3) (string-walk 4)
                                 (has-fn? :string3) (string-walk 5)
                                 (has-fn? :string3) (string-walk 6) ;; TODO REMOVE ALL THIS FUCKERY - we already have a better way w &[args] mapping!

                                 (has-fn? :push>) push-walk
                                 (has-fn? :push>>) push-walk-fn


                                 has-drops? (drop-walk-replace (vec (keys drop-walks))))
                                 ;;(str panel-key ".all-caching.1-walk")
                               ;;  )
                                  ;; TODO make this conditional
                                    ;(walk/postwalk-replace workspace-params)    ;; in case drop-walk run - TODO make this conditional

                                    ;(walk/postwalk-replace drop-walks) ;; 2/15/24

                                    ;; ^ we could prolly do vsql this way? with a :button fn or something
                                    ;(walk/postwalk-replace walk-map) ;; last for stmt eval?


        ;body      (drop-walk-replace body (vec (keys drop-walks))) ;; sketch? but only way we can retain data struct and not functionize like the others
                                                                   ;; since we STILL want to be resolved like it was regular honeycomb
       ; _ (when (= panel-key :block-5474) (ut/tapp>> [:midbody body]))
        ; body (let []
        ;        (ut/tapp>> [:body-pre body])
        ;        (onclick-walk-map body))
        ;body (walk/postwalk-replace drop-walks body)
        ;;walk-map (select-keys walk-map obody-key-set) ;;; reduce size of replacement walk map? any faster? unsure. 
        ;;  ^^ no, slower. unnecessary since postwalk-replace is mostly target structure bound and map GETS are constant time operations (O(1)),
        body                   ;;(ut/timed
                                (ut/postwalk-replacer walk-map body)
                               ;; (str panel-key ".all-caching.2-walk"))

        ;; _ (when (= panel-key :block-1774) (ut/tapp>> [:postbody selected-view body]))
       ; _ (when (= panel-key :block-5474) (ut/tapp>> [:postbody body]))
        ;; body (-> body
        ;;          =-walk-map ;; test, needs to be first - "and" after... THEN if...
        ;;          if-walk-map ;; ifs needs special treatment - must replace BEFORE reg view vecs
        ;;          when-walk-map ;; test!
        ;;          onclick-walk-map ;; test!
        ;;          )
        ;full-rep-map (merge data-walks workspace-params)
        ;body (ut/postwalk-replacer full-rep-map body)
        ;not-editor-and-not-empty? (and (not editor-panel?) (seq body))
        ;editor-and-not-empty? (and editor-panel? (seq body)
        dbody-type (get-in @dragging-body [:drag-meta :type])
        relevant-for-dyn-drop? (or (and is-layout? (or (= :query dbody-type)
                                                       (= :view dbody-type)))
                                   (and 
                                    ;(nil? (get @dragging-body :cloned-from))
                                    (not (contains? @dragging-body :cloned-from))
                                    (not is-layout?)
                                        (not (= :meta-screens dbody-type)) ;; temp
                                        (not (= :meta-theme dbody-type)) ;; temp
                                        (not (= :meta-board dbody-type)) ;; temp
                                        (not (= :meta-blocks dbody-type)))) ;; temp
        ; overlay? (or (and (= selected-view @mad-libs-view) (or selected-view-is-sql? override-view-is-sql?))
        ;           (and @dragging? relevant-for-dyn-drop?))
        overlay?               (and @dragging? relevant-for-dyn-drop?)
        base-tables            (vec (filter #(and (not (ut/is-sql-sql-alias? %)) (keyword? %))
                                            (remove nil? (flatten (for [[_ v] sql-calls]
                                                                    (when
                                                                     (not (cstr/includes? (str (get v :from)) ":query/"))
                                                                      (get-in v [:from 0])))))))
        base-table-sniffs      (into {} (for [t base-tables]
                                          ;(when (keyword? t)
                                          {t {:select [:*]
                                              :connection-id (if (some #(= t %) ["channel_history" "fn_history" "flows_history"]) "flows-db" "system-db")
                                              :from [t] :limit 111}}))

        templated-strings-vals (ut/deep-template-find body)
        templates?              (ut/ne? templated-strings-vals)
        templated-strings-walk (if templates?
                                 (ut/postwalk-replacer {nil ""}
                                                        (into {} (for [k templated-strings-vals]
                                                                   {k
                                                                    ;;@(ut/tracked-subscribe [::conn/clicked-parameter-key [k]])
                                                                    @(rfa/sub ::conn/clicked-parameter-key-alpha {:keypath [k]})}))) {})

        ;) ;; need to generate base metadata for use
        ;query-view-alias? (cstr/starts-with? (str orig-body) ":grid/")

        body (if templates? (ut/deep-template-replace templated-strings-walk body) body)]

    ;; (when templates?
    ;;   (let []
    ;;     (ut/tapp>> [:honeyresolver templated-strings-vals
    ;;            templated-strings-walk
    ;;            (ut/deep-template-replace templated-strings-walk body)
    ;;            body panel-key valid-body-params]))
    ;;   )


      ;;(ut/tapp>> [:pok! ww hh panel-key selected-view orig-override-view all-keys override-view sql-calls override-view fh fw replacement-view replacement-query body])

   ; (when (= panel-key :block-10980)
   ;   (ut/tapp>> [:honeycomb-params panel-key workspace-params value-walks body]))

    ;(ut/tapp>> [:react-me-baby @on-block?]) ;; TODO this is horseshit, testing something

    ;(ut/tapp>> [:base-tables base-tables])
    ;(when (= panel-key :block-10550) ;(cstr/includes? (str body) "fark")
    ;;  (ut/tapp>> [:honeycomb panel-key
    ;;         ;;(get-in @dragging-body [:drag-meta :type])
    ;;         ;(ut/extract-patterns pre-body :if 4) (if-walk-map2 pre-body)
    ;;         body
    ;;         selected-view condi-walks sql-aliases-used used-datasets possible-datasets-used

    ;;         ;;(ut/namespaced-swapper "this-block" (ut/replacer (str panel-key) #":" "") [[[ :*this-block*/test234]] :*this-block*/test23dddd4])
    ;;  ])
            ;; vsql-calls
            ;(get-kv-map body) (get-kv-map-nested-keys body)
            ;(ut/extract-patterns body)
    ;;       ;;  (ut/kvpaths body) (ut/kvpaths3 body) ; (flatten (ut/kvpaths body))
    ;;       ;;  (let [d {:a {:b {:c 1 :d {:e 2}}
    ;;       ;;               :f 3}
    ;;       ;;           :g 4
    ;;       ;;           [1 [:boogs 2] 3] {:h 5
    ;;       ;;                    [4 5 6] 7}}]
    ;;       ;;    (into {} (for [v (ut/kvpaths d)] {v (get-in d v)})))
    ;;       ;;  (let [d {:a {:b {:c 1 :d {:e 2}}
    ;;       ;;               :f 3}
    ;;       ;;           :g 4
    ;;       ;;           [1 [:boogs 2] 3] {:h 5
    ;;       ;;                    [4 5 6] 7}}
    ;;       ;;        dd (ut/unpack-keys d)]
    ;;       ;;    (into {} (for [v (ut/kvpaths dd)] {v (get-in dd v)})))
    ;;        ])
    ; )
    ;(ut/tapp>> {:mapply (ut/mapply re-com/box {:align :center :child "" :size "auto"})})
    ;(ut/tapp>> [:tt overlay? @mad-libs-view selected-view selected-view-is-sql? override-view-is-sql? (and (= selected-view @mad-libs-view) (or selected-view-is-sql? override-view-is-sql?))])
    ; (ut/tapp>> [:base-tables base-tables :base-table-sniffs base-table-sniffs])
    ; (ut/tapp>> [:body-post body])
    ; (ut/tapp>> [:condi condi-walks
    ;        :body (let [obody (ut/postwalk-replacer condi-walks orig-body)
    ;                    kps (into {} (for [p (ut/kvpaths
    ;                                          obody)] {p (get-in obody p)}))
    ;                    logic-kps (into {} (for [[k v]
    ;                                (into {} (filter #(cstr/starts-with? (str (last %)) "[:if") kps))]
    ;                                    (let [[_ l this that] v]
    ;                                      {v (if l this that)})))]
    ;                ;(walk/prewalk-replace logic-kps obody)
    ;                logic-kps
    ;                ;(ut/postwalk-replacer logic-kps obody)
    ;                )])
    ;(ut/tapp>> [:condi-walks condi-walks])
    ;(ut/tapp>> [:honeycomb panel-key valid-body-params value-walks])
    ;(when selected-view-is-ref? (ut/tapp>> [:ref selected-view (get body selected-view)
    ;                                   (view-alias-replace (get body selected-view))]))
    ;(ut/tapp>> [:honeycomb panel-key single-view? selected-view selected-view-is-sql?])
    ;(ut/tapp>> [:doruns (merge sql-calls base-table-sniffs)])
    ;(ut/tapp>> [:base-tables base-tables])
    (doseq [[k v] (merge sql-calls base-table-sniffs)] ;; base-table-sniffs allow us to get metadata for the root tables quietly
      (let [
            query        (sql-alias-replace-sub v)
            ;last-known-fields (vec (keys (get @(ut/tracked-subscribe [::conn/sql-metadata [k]]) :fields))) ;; only used for :*all= ATM
            editor?        @(ut/tracked-sub ::editor? {})
            selected-block @(ut/tracked-sub ::selected-block {})
            being-edited? (and @db/cm-focused? editor? (= selected-block panel-key)) ;; dont run while the query is being edited...
            ;; _ (when being-edited? (ut/tapp>> [:being-edited? @db/cm-focused? "NOT RUNNING" panel-key selected-block]))
            data-exists? @(ut/tracked-subscribe [::conn/sql-data-exists? [k]])
            unrun-sql?   @(ut/tracked-subscribe [::conn/sql-query-not-run? [k] query])
            connection-id (get query :connection-id connection-id)
            ;; query (ut/postwalk-replacer {:read-edn (fn [x] (try (edn/read-string x) (catch :default _ x)))} query)
            ] ;; override w query connection-id

                   ;query (assoc query :last-know-fields last-known-fields)


              ;;  (ut/tapp>> [:query-all-clicked k data-exists? unrun-sql? query])

        ;; (ut/tapp>>  [:unrun? (or (not data-exists?) unrun-sql?) k data-exists? unrun-sql? ])
        (when (and (or (not data-exists?) unrun-sql?) (not being-edited?))
          (let [src @(rfa/sub ::conn/sql-source {:kkey k})
                srcnew? (not= src query)]
            (if (or (empty? connection-id) (nil? connection-id))
              (conn/sql-data [k] query)
              (conn/sql-data [k] query connection-id))
            (when srcnew? ;;; no need to dispatch to update the same shit over and over... (with autorefresh queries)
              (ut/tracked-dispatch [::insert-sql-source k query]))))))

    ;(when (= panel-key :block-289)
    ;  (ut/tapp>> [:honeycomb panel-key (keys sql-calls) (true? (seq body)) override-view selected-view body (get body selected-view) (first (keys body)) sql-aliases-used used-datasets]))

    (cond ;;replacement-all? [rc/catch [re-com/box :child (get body selected-view)]] ;[re-com/box :child (get body override-view)]

      (seq body)
      (cond (not (nil? override-view))                  ;; for editor usage

            (if (= override-view :base)

              (let []
                    ;(ut/tapp>> [:here?])
                [re-com/box ;:size "auto"
                 :style {:filter (if overlay? "blur(3px) opacity(60%)" "none")}
                 :child body])

              (if override-view-is-sql?

                [rc/catch
                 [magic-table panel-key [override-view]]]

                (let []
                      ;(ut/tapp>> [:here? override-view (get body override-view)])
                  [rc/catch
                   [re-com/box
                    :width (px ww)
                        ;:height (px hh) ;; buffy fix here?!?! Jan 3rd 2024. what did it break?
                    :size "none"
                    :style {:filter (if overlay? "blur(3px) opacity(60%)" "none")}
                                ;:border "1px solid white"
                                ;:width "200px"

                    :child (get body override-view)]])))

;query-view-alias? ;;; [eyes emoji]

                ;(let [spl (last (ut/splitter (ut/safe-name orig-body) "/"))
                ;      spl2 (ut/splitter spl ".")
                ;      p-key (keyword (first spl2))
                ;      vw (keyword (last spl2))]
                ;  [rc/catch
                ; [magic-table p-key [vw]]])

            selected-view-is-sql?

            [rc/catch
             [magic-table panel-key [selected-view]]]   ;; magic-table [panel-key data-keypath

;  selected-view-is-ref?

                ;  [rc/catch
                ;   [re-com/v-box
                ;    :children
                ;    [(when overlay?
                ;       [dynamic-spawner-targets nil @dragging-body
                ;        panel-key nil ww hh])
                ;     [re-com/box
                ;      :style {:filter (if overlay? "blur(3px) opacity(60%)" "none")}
                ;     :child view-alias-body]]]]

            single-view?

            [rc/catch
             [re-com/v-box
              :children
              [(when overlay?
                     ;[dynamic-spawner-targets nil @dragging-body panel-key nil ww hh]
                 @(ut/tracked-subscribe [::dynamic-drop-targets nil @dragging-body panel-key nil ww hh]))
               [re-com/box
                :style {:filter (if overlay? "blur(3px) opacity(60%)" "none")}
                :child body]]]]

            :else

                ; [rc/catch
                ;  (get body selected-view)]

            [rc/catch
             [re-com/v-box
                  ;:style {:filter (if overlay? "blur(3px) opacity(60%)" "none")}
              :children
              [(when overlay?
                     ;[dynamic-spawner-targets nil @dragging-body panel-key nil ww hh]
                 @(ut/tracked-subscribe [::dynamic-drop-targets nil @dragging-body panel-key nil ww hh]))
               [re-com/box
                :style {:filter      (if overlay? "blur(3px) opacity(60%)" "none")
                        :margin-top  (if (and overlay? is-layout?)
                                       "-25px" "inherit") ;; not sure why the pos gets fucked up on overlay
                            ;; (filter side effect?) but still, this fixes it. BANDAID!
                        :margin-left (if (and overlay? is-layout?) "-3px" "inherit")}
                :child                                  ;(if (= selected-view :layered-viz)
                    ;(get body selected-view)
                    ;[re-com/box :child "LAYERSZSs"]
                    ;@(ut/tracked-subscribe [::layered-oz-views panel-key])
                    ;layered-body
                (get body selected-view)]]]])                ;)


      (and no-view? selected-view-is-sql?)
      (if (and fw fh) ;(js/alert (str selected-view))
        [rc/catch [magic-table panel-key [selected-view] fw fh]]
        [rc/catch [magic-table panel-key [selected-view]]]) ;; block-sizes block-width

      (and no-view? (nil? selected-view) (seq sql-calls))
      [rc/catch [magic-table panel-key [(first (keys sql-calls))]]]

          ;editor-and-not-empty?
          ;[rc/catch @(ut/tracked-subscribe [::panel-views panel-key])] ;; uinmodded body

      :else [re-com/box :child "nothing here to do."]))
      )

;; (re-frame/reg-sub
;;  ::honeycomb ;; cache cheating?
;;  (fn [_ [_ panel-key & [override-view fh fw replacement-view replacement-query]]]
;;    (honeycomb-fn panel-key override-view fh fw replacement-view replacement-query)))

;; (defn honeycomb [panel-key & [override-view fh fw replacement-view replacement-query]]
;;   @(ut/tracked-subscribe [::honeycomb  panel-key override-view fh fw replacement-view replacement-query]))


(defn brick-key-lookup [brick-vec brick-map]
  (first (remove nil? (for [[k v] brick-map] (when (= brick-vec (:root v)) k)))))

(defonce edit-mode? (reagent/atom {}))

(re-frame/reg-sub
 ::lookup-root-key
 (fn [db [_ brick-vec]]
   (let [lookup-map (into {}
                          (for [[k v]
                                 ;(get db :panels)
                                (into {} (filter #(= (get db :selected-tab) (get (val %) :tab "")) (get db :panels)))]
                            {(get v :root) k}))]
     (get lookup-map brick-vec))))

(re-frame/reg-sub
 ::lookup-root-key-tab
 (fn [db [_ brick-vec tab]]
   (let [lookup-map (into {}
                          (for [[k v]
                                (into {} (filter #(= tab (get (val %) :tab "")) (get db :panels)))]
                            {(get v :root) k}))]
     (get lookup-map brick-vec))))

(re-frame/reg-sub
 ::all-panels-minimized
 (fn [db]
   (vec (for [[k v]
              (into {} (filter #(or (get (val %) :pinned? false)
                                    (= (get db :selected-tab) (get (val %) :tab ""))) (get db :panels)))
              :when (get v :minimized? false)]
          [k (get v :name)]))))

(re-frame/reg-sub
 ::all-roots
 (fn [db]
   (vec (for [[k v]                                        ;; added tab filter - 9/25/23
              (into {} (filter #(or (get (val %) :pinned? false)
                                    (= (get db :selected-tab) (get (val %) :tab ""))) (get db :panels)))]
          (vec (conj (get v :root) k))))))
          ;(get v :root)


(re-frame/reg-sub
 ::all-roots-tab
 (fn [db {:keys [tab]}]
   (vec (for [[k v]
              (into {} (filter #(= tab (get (val %) :tab "")) (get db :panels)))]
          (vec (conj (get v :root) k))))))
          ;(get v :root)


;; (re-frame/reg-sub
;;  ::all-roots-tab
;;  (fn [db [_ tab]]
;;    (let [current-tab (get db :current-tab)]
;;      (when (not= tab current-tab)
;;        (vec (for [[_ v] (into {} (filter #(= tab (get (val %) :tab "")) (get db :panels)))]
;;               (get v :root)))))))


(re-frame/reg-sub
 ::all-roots-tab-sizes
 (fn [db {:keys [tab]}]
   (vec (for [[_ v]
              (into {} (filter #(= tab (get (val %) :tab "")) (get db :panels)))]
          (vec (into (get v :root) [(get v :h) (get v :w)]))))))

(re-frame/reg-sub
 ::all-roots2
 (fn [db [_ start-y end-y start-x end-x]]
   (vec (for [[k v] (get db :panels)]

          (let [r (get v :root)
                x (first r)
                y (last r)]
            (when (and (and (>= x start-x) (<= x end-x))
                       (and (>= y start-y) (<= y end-y))) [x y]))))))

(re-frame/reg-sub
 ::panel-width
 (fn [db [_ panel-key]]
   (get-in db [:panels panel-key :w])))

(re-frame/reg-sub
 ::panel-px-width
 (fn [db [_ panel-key]]
   (* brick-size (get-in db [:panels panel-key :w]))))

(re-frame/reg-sub
 ::panel-px-height
 (fn [db [_ panel-key]]
   (* brick-size (get-in db [:panels panel-key :h]))))

(re-frame/reg-sub
 ::panel-px-root
 (fn [db [_ panel-key]]
   (let [r (get-in db [:panels panel-key :root])
         x (* brick-size (first r))
         y (* brick-size (last r))]
     [x y])))

(re-frame/reg-sub
 ::connection-id
 (fn [db [_ panel-key]]
   (get-in db [:panels panel-key :connection-id])))

(re-frame/reg-sub
 ::panel-height
 (fn [db [_ panel-key]]
   (get-in db [:panels panel-key :h])))

(re-frame/reg-sub
 ::panel-depth
 (fn [db [_ panel-key]]
   (get-in db [:panels panel-key :z] 0)))

(re-frame/reg-event-db
 ::panel-depth-up
 (fn [db [_]]
   (let [panel-key (get db :selected-block)
         old-z     (get-in db [:panels panel-key :z] 0)]
     (assoc-in db [:panels panel-key :z] (+ old-z 1)))))

(re-frame/reg-event-db
 ::panel-depth-down
 (fn [db [_]]
   (let [panel-key (get db :selected-block)
         old-z     (get-in db [:panels panel-key :z] 0)]
     (if (> old-z 0)
       (assoc-in db [:panels panel-key :z] (- old-z 1))
       db))))

(re-frame/reg-sub
 ::panel-name
 (fn [db [_ panel-key]]
   (get-in db [:panels panel-key :name])))

(re-frame/reg-sub
 ::ghosted?
 (fn [db [_ panel-key]]
   (if (get db :peek?)
     false
     (get-in db [:panels panel-key :ghosted?] false))))

(re-frame/reg-sub
 ::un-fired-cross-breed?
 (fn [db [_ panel-key]]
   (true?
    (and (ut/ne? (get-in db [:panels panel-key :cross-breed]))
         (empty? (get-in db [:panels panel-key :cross-breed :children]))))))

(re-frame/reg-sub
 ::no-ui?
 (fn [db [_ panel-key]]
   (if (get db :peek?)
     false
     (or
      (get db :no-ui? false)
      (get-in db [:panels panel-key :no-ui?] false)))
   ;true
   ))

(re-frame/reg-sub
 ::panel-style
 (fn [db [_ panel-key]]
   (if (get db :peek?)
     {}
     (get-in db [:panels panel-key :style] {}))))

(re-frame/reg-sub
 ::hidden?
 (fn [db [_ panel-key]]
   (if (get db :peek?)
     false
     (get-in db [:panels panel-key :hidden?] false))))

(re-frame/reg-sub
 ::minimized?
 (fn [db [_ panel-key]]
   (if (get db :peek?)
     false
     (get-in db [:panels panel-key :minimized?] false))))

(defn downstream-map [panel-id qid subq-map idx final]
  (apply concat

         (for [u (get-in subq-map [panel-id :uses])]
           (apply concat (remove empty?

                                 (for [[k v] (dissoc subq-map panel-id)]
                                   (apply concat (remove empty?

                                                         (for [kk (get v :produces)]
                                                           (if (= u kk)

                                                             (downstream-map k kk subq-map (+ 1 idx) (conj final {:d idx :p k}))
                                                             (conj final {:d idx :p k})))))))))))
                                                             ;final

                                                             ;final

(def warp-hole (reagent/atom false))

(defn squareql-logo [x y]
  (let [left (* x brick-size)
        top  (* y brick-size)]
    [re-com/h-box

     :children [[re-com/box
                 :child [:img {:src "images/squareql-logo-100a.png"}]
                 :height "102px"
                 :width "102px"
                 :size "none"
                 :attr {;:on-mouse-enter #(reset! warp-hole true)
                        :on-mouse-leave #(reset! warp-hole false)
                        :on-click       #(reset! warp-hole true)}
                 :style {;:-webkit-backface-visibility "hidden"
                         :transition_NOT       "transform 0.8s"
                         :transform-style  "preserve-3d"
                         :perspective      "1000px"
                         ;:backface-visibility "hidden"

                         :transform        (if @warp-hole "rotateY(360deg)" "")

                         :background-color "#000000"
                         :filter           "drop-shadow(0.35rem 0.35rem 0.4rem rgba(0, 0, 0, 0.5))"
                         :border           (str "3px solid " (theme-pull :theme/editor-outer-rim-color "#FF06B5")) ;; "#FF06B5"
                         :position         "fixed"
                         :left             left
                         :top              top
                         :z-index          10}]

                [re-com/box
                 :child " "                                 ;[:img {:src "images/squareql-logo-100a.png"}]
                 :height "102px"
                 :width "102px"
                 :size "none"
                 ;:attr {;:on-mouse-enter #(reset! warp-hole true)
                 ;       :on-mouse-leave #(reset! warp-hole false)
                 ;       :on-click #(reset! warp-hole true)}
                 :style {:background-color "#000000"
                         :box-shadow       "inset 0px 0px 20px #FF06B566"
                         ;:filter "drop-shadow(0.35rem 0.35rem 0.4rem rgba(0, 0, 0, 0.5))"
                         ;:border "3px solid #FF06B5"
                         :position         "fixed"
                         :left             left
                         :top              top
                         :z-index          9}]]]))

(defn param-usage [panel-key]
  (let [all-sql-call-keys      @(ut/tracked-subscribe [::all-sql-call-keys]) ;(into {} (for [[k v] workspace] (get v :queries)))
        sql-aliases-used       @(ut/tracked-subscribe [::panel-sql-aliases-in-views panel-key])
        ;params-used @(ut/tracked-subscribe [::panel-parameters-in-views panel-key])
        ;;sql-calls              @(ut/tracked-subscribe [::panel-sql-calls panel-key])
        sql-calls              @(rfa/sub ::panel-sql-calls {:panel-key panel-key})
        ;;valid-body-params      @(ut/tracked-subscribe [::valid-body-params panel-key])
        valid-body-params      @(rfa/sub ::valid-body-params {:panel-key panel-key})
        possible-datasets-used (set (for [e (merge sql-calls valid-body-params)] (keyword (nth (ut/splitter (ut/safe-name e) #"/") 0))))
        used-datasets          (cset/union (set sql-aliases-used) (cset/intersection possible-datasets-used (set all-sql-call-keys)))]
    (ut/tapp>> [:pos panel-key possible-datasets-used])
    (vec (for [d used-datasets] @(ut/tracked-subscribe [::lookup-panel-key-by-query-key d])))))

(defn draw-lines [coords]
  (doall (for [[x1 y1 x2 y2 involved? color z1 z2 same-tab?] coords]
           ^{:key (hash (str x1 y1 x2 y2 "lines"))}
           (let [];selected-dash "" ;@(ut/tracked-subscribe [::selected-dash])
                 ; relations "" ;@(ut/tracked-subscribe [::relations selected-dash])
                 ; selected  "" ;@(ut/tracked-subscribe [::selected])
                 ; involved (vec (remove nil? (distinct (apply concat (filter #(some (fn [x] (= x selected)) %) relations)))))
                 ; nothing-selected? (nil? selected)
                 ;  involved1? (some #(= n1 %) involved)
                 ; involved2? (some #(= n2 %) involved)
                 ; involved? (and involved1? involved2?)
                 ; peek? @(ut/tracked-subscribe [::peek?])

            ;; (ut/tapp>> [:coords coords])
             ; (ut/tapp>> [:lines z1 z2 same-tab? @(ut/tracked-subscribe [::what-tab z1]) @(ut/tracked-subscribe [::what-tab z2])])
             [:path {:stroke-width (if involved? 16 13)
                     :stroke       (if involved? color "#ffffff22") ;(if (or involved? nothing-selected?)
                     ;(if (nil? color) "pink" color) "#E6E6FA")
                     :fill         "none"
                     :filter       "drop-shadow(0.25rem 0.35rem 0.4rem rgba(0, 0, 0, 0.44))"
                     :d            (ut/curved-path-h x1 y1 x2 y2)}]))))

;(def panels-hash (reagent.core/atom nil))

(re-frame/reg-event-db
 ::refresh-history-log
 (fn [db _]
   (if (not (nil? @db/active-tmp-history)) ;(get db :buffy?)
     (ut/dissoc-in db [:data @db/active-tmp-history])
     ;;(ut/dissoc-in db [:data :history-log-sys])
     db)))

(re-frame/reg-event-db
 ::update-panels-hash
 (fn [db _]
   (if (not @on-scrubber?) ;; dont want to push updates during scrubbing
     (let [pp (get db :panels)
           ;ppr (into {} (for [[k v] pp] ;; this is incredibly slow and lags the shit outta everything...
           ;      {k (assoc v :queries (into {} (for [[kk vv] (get v :queries)] {kk (sql-alias-replace-sub vv)})))}))
           ppr {} ;;; TEMPO!
           new-h (hash (ut/remove-underscored pp))
           client-name (get db :client-name)]
      ;;  (ut/tapp>> [:!!!!!!!panels-pushed!!!!!! client-name (get db :panels-hash) ppr new-h])
       (conn/push-panels-to-server pp ppr client-name)
       (when (get db :buffy?) (ut/dispatch-delay 2000 [::refresh-history-log]))
       (assoc db :panels-hash new-h)) db)))




(re-frame/reg-sub
 ::panels-hash
 (fn [db _]
   ;(hash (get db :panels))
   (get db :panels-hash "not-yet!")))

(re-frame/reg-sub
 ::panels
 (fn [db _]
   (get db :panels)))

(re-frame/reg-event-db
 ::toggle-minimize-block
 (undoable)
 (fn [db [_ panel-id]]
   (assoc-in db [:panels panel-id :minimized?]
             (not (get-in db [:panels panel-id :minimized?] false)))))

(re-frame/reg-event-db
 ::toggle-pin-block
 (undoable)
 (fn [db [_ panel-id]]
   (assoc-in db [:panels panel-id :pinned?]
             (not (get-in db [:panels panel-id :pinned?] false)))))

(re-frame/reg-event-db
 ::toggle-icon-block
 (undoable)
 (fn [db [_ panel-id]]
   (assoc-in db [:panels panel-id :iconized?]
             (not (get-in db [:panels panel-id :iconized?] false)))))

(re-frame/reg-event-db
 ::launch-clone
 (undoable)
 (fn [db [_ panel-id]]
   (let [pid (ut/safe-key (keyword (str (ut/safe-name panel-id) "-window")))
         [nx ny] (vec (map #(js/Math.floor (/ % brick-size)) @db/context-modal-pos))
         orig (get-in db [:panels panel-id])
         new (-> orig
                 (assoc :tab (get db :selected-tab))
                 (assoc :root [nx ny])
                 (assoc :z (+ 10 (get-in db [:panels @over-block :z] 11)))
                 (assoc :window (str "spawned-from-icon-of_" panel-id))
                 (assoc :pinned? false)
                 (assoc :iconized? false))]
     (assoc-in db [:panels pid] new))))

(re-frame/reg-event-db
 ::move-to-current-tab
 (undoable)
 (fn [db [_ panel-id]]
   (assoc-in db [:panels panel-id :tab]
             (get db :selected-tab))))

(re-frame/reg-sub
 ::tab-recenter
 (fn [db [_ tab]]
   (let [coords (vec (for [[x y _]
                           ;;@(ut/tracked-subscribe [::all-roots-tab tab])
                           @(rfa/sub ::all-roots-tab {:tab tab})
                           ] [x y]))
         ;;coords+ @(ut/tracked-subscribe [::all-roots-tab-sizes tab])
         coords+ @(rfa/sub ::all-roots-tab-sizes {:tab tab})
         icons? (ut/not-empty? (for [[k v] (get db :panels)
                                     :when (and (= (get v :tab "") tab) (get v :iconized? false))]
                                 k)) ;; if icons, do not recenter
         corner (if icons? [] (try (ut/min-at-least-pos coords) (catch :default _ [0 0])))
         corner (if (empty? corner) [0 0] corner)
         [hh ww]  (ut/canvas-size coords+)
         corner [(* -1 (first corner)) (* -1 (last corner))]
         [hh ww] [(+ hh (first corner)) (+ ww (last corner))]]
     ;(ut/tapp>> [:tab-recenter corner [hh ww] coords])
     (vec (into corner [hh ww])))))

(re-frame/reg-sub
 ::tab-offset
 (fn [_ [_ tab]]
   (let [coords (vec (for [[x y _]
                           ;;@(ut/tracked-subscribe [::all-roots-tab tab])
                           @(rfa/sub ::all-roots-tab {:tab tab})
                           ] [x y]))
         corner (try (ut/min-at-least-pos coords) (catch :default _ [0 0]))]
     ;(ut/tapp>> [:tab-recenter corner [hh ww] coords])
     corner)))

(re-frame/reg-sub
 ::tab-offset2 ;; parent offset
 (fn [db [_ tab]]
   (let [stab (get db :selected-tab)]
     (first (for [[_ v] (get db :panels)
                  :when (and (= (get v :tab) stab) (some #(= % tab) (ut/deep-flatten (get v :views))))]
              (get v :root))))))

(re-frame/reg-sub
 ::is-grid?
 (fn [db [_ panel-key view]]
   (true? (some #(= % :grid) (ut/deep-flatten (get-in db [:panels panel-key :views view]))))))

(re-frame/reg-sub
 ::is-pinned?
 (fn [db [_ panel-key]]
   (get-in db [:panels panel-key :pinned?] false)))

(defn render-icon [icon num]
  (if (and (not (empty? icon))
           (not (nil? icon)))
    (if (cstr/includes? icon "zmdi")
      [re-com/md-icon-button :src (at)
       :md-icon-name icon
       :style {;:color bcolor
               ;:cursor "grab"
               :font-size "15px"}
       :attr {}]
      [re-com/box
       ;:style {:margin-left "-10px"}
       :size "none"
       ;:width "30px"
       :height (px (* num brick-size))
       :child [:img {:src icon
             ;:height "50px"
             ;:height "auto"
             ;:width "50px"
                     :width "100%"}]])
    " "))

(re-frame/reg-sub
 ::iconized
 (fn [db [_ panel-key]]
   (when (get-in db [:panels panel-key :iconized?] false)
     (let [;iconized? (get-in db [:panels panel-key :iconized?] false)
           name (get-in db [:panels panel-key :name] "")
           icon (get-in db [:panels panel-key :icon])
           default-icon-map {:h 1 :w 1
                             :view [:box :align :center :justify :center
                                    :width (px (* brick-size 1))
                                    :height (px (* brick-size 1))
                                    :padding "5px"
                                    :size "auto"
                                    :style {:font-size "10px"
                                            :border (str "1px solid " (theme-pull :theme/editor-outer-rim-color nil) 33)
                                            :filter (str "drop-shadow(0 0 0.75rem " (theme-pull :theme/editor-outer-rim-color nil) ")")}
                                    ;:style {:border "2px solid orange"}
                                    :child (if icon
                                             [render-icon icon 1]
                                             (str name))]}
           icon-view (get-in db [:panels panel-key :icon-view] default-icon-map)]
       icon-view))))

(re-frame/reg-sub
 ::panel-icon
 (fn [db [_ panel-key]]
   (let [selected-view (get-in db [:panels panel-key :selected-view])
         is-grid?     @(ut/tracked-subscribe [::is-grid? panel-key selected-view])
         selected-query? (some #(= % selected-view) (keys (get-in db [:panels panel-key :queries])))
         default (cond selected-query? "zmdi-grid"
                       is-grid? "zmdi-view-dashboard"
                       :else "zmdi-widgets") ;; depends on what view selected? TODO. expensive and semi-pointless?
         i (get-in db [:panels panel-key :icon] default)]
     i)))

(re-frame/reg-sub
 ::has-a-running-flow?
 (fn [db [_ panel-key]]
   (let [panel-map (get-in db [:panels panel-key])
         pile (filter #(cstr/starts-with? (str %) ":flow/") (ut/deep-flatten panel-map))]
     (some true?
           (for [e pile] (let [aa (ut/replacer e ":flow/" "")
                               spl (ut/splitter aa ">")
                               runkey (keyword (str (first spl) ">*running?"))]
                           (get-in db [:click-param :flow runkey])))))))

(re-frame/reg-sub
 ::has-a-flow-view?
 (fn [db [_ panel-key view]]
   (let [panel-map (get-in db [:panels panel-key :views view])
         ppile (ut/deep-flatten panel-map)
         finds (get (group-by #(first (first %)) @drop-last-tracker-refs) panel-key)
         finds (for [[d p] finds
                     :when (some #(= % (last d)) ppile)]
                 p)
         any? (not (empty? finds))]
    ;;  (when (= panel-key :block-5474)
    ;;    (ut/tapp>> [:flow-view panel-key view any?   finds]))
     any?)))

(re-frame/reg-sub
 ::has-a-running-flow-view?
 (fn [db [_ panel-key view]]
   ;(ut/tapp>> [panel-key])
   (let [panel-map (get-in db [:panels panel-key :views view])
         ppile (ut/deep-flatten panel-map)
         ;pile (filter #(cstr/starts-with? (str %) ":flow/") (ut/deep-flatten panel-map)) ;; lol, this is a rabbit-code, not the resulting params
         ;pile (filter #(cstr/starts-with? (str %) ":flow/") (ut/deep-flatten (get (group-by #(first (first %)) @drop-last-tracker-refs) panel-key)))
         finds (get (group-by #(first (first %)) @drop-last-tracker-refs) panel-key)
         finds (for [[d p] finds
                     :when (some #(= % (last d)) ppile)]
                 p)
         running? (some true?
                        (for [e finds] (let [aa (ut/replacer e ":flow/" "")
                                             spl (ut/splitter aa ">")
                                             runkey (keyword (str (first spl) ">*running?"))]
                                         (get-in db [:click-param :flow runkey]))))]
    ;;  (when (= panel-key :block-5474)
    ;;    (ut/tapp>> [:running-flow-view panel-key view   running?   finds]))
     running?)))


(re-frame/reg-event-db
 ::cross-breed
 (undoable)
 (fn [db [_ panel-id]]
   (let [panel-body (get-in db [:panels panel-id])
         cross-data (get panel-body :cross-breed)
         times (get cross-data :num 10)
         new-tab-name (ut/gen-tab-name)
         skeleton (fn [bid new-tab-name idx]
                    (let [pidx (+ 2 idx)
                          h 2]
                      {:h             h
                       :w             7
                       :root          [1 (* pidx h)]
                       :tab           new-tab-name
                       :selected-view :hii
                       :name          (str bid)
                       :views         {:hii [:box :align :center :justify :center
                                             :attr {:id (str bid ":crossed")} :style
                                             {:font-size "22px"
                                              :font-weight 700
                                              :padding-top "6px"
                                              :padding-left "14px"
                                              :margin-top "-8px"
                                              :color :theme/editor-outer-rim-color
                                              :font-family :theme/base-font} :child
                                             (str bid "!")]}
                       :queries       {}}))
         panel-map (into {}
                         (for [idx (range times)]
                           (let [bid (keyword (str "block-cross-" idx))
                                 bid (ut/safe-key bid)]
                             {bid (skeleton bid new-tab-name idx)})))
         new-panel-map (assoc-in panel-body [:cross-breed :children] (vec (keys panel-map)))
         panel-map (assoc panel-map panel-id new-panel-map)
         panels (merge (get db :panels) panel-map)]

         (assoc db :panels panels)

     )))

(def rs-debug (atom []))

;; (defn rs [edn brick-vec-key]
;;   (let [rr (resolver/logic-and-params edn brick-vec-key)]
;;     ;(ut/tapp>> [:rs-d rr])
;;     (swap! rs-debug conj [rr edn brick-vec-key])
;;     rr))

(defn rs [edn brick-vec-key]
  (let [
        ;;rr (resolver/logic-and-params edn brick-vec-key)
        ]
    ;(ut/tapp>> [:rs-d rr])
    (swap! rs-debug conj [edn brick-vec-key])
    edn)) ;; something very fucked up here, need to revise this whole appraoch. slow and weird.


(re-frame/reg-sub
 ::resolved-block-body-shell
 (fn [db [_ {:keys [panel-key]}]]
   (let [body (get-in db [:panels panel-key])
          ;;_ (ut/tapp>> [:oo panel-key body])
         body (-> body (dissoc :views) (dissoc :queries))]
     (resolver/logic-and-params-fn body panel-key)
     ;@(rfa/sub ::logic-and-params {:m body :p panel-key})
     ;body
     )))

(defn maybedoall []
  (let [hover-highlight? (or @param-hover @query-hover)]
    (if hover-highlight?
      doall seq))
  ;;  doall
  )

(defn grid [& [tab]]
  ;;(try

     (let [;reaction-hack! @hover-square ;; seems less expensive than doall-for ?
        ;reaction-hack2! @edit-mode?
        ;editor? @(ut/tracked-subscribe [::get-in [:editor?]])
        ;;external? @(ut/tracked-subscribe [::external?]) ;;; imp?
        ;reaction-hack! (when @dragging? @hover-square) ;; seems less expensive than doall-for ?
        panels-hash1    @(ut/tracked-sub ::panels-hash {})
        ;;panels-hash1    @(rfa/sub ::panels-hash {}) ;;; alpha sub that is SUPPOSED to be memory safe at the possible cost of CPU...
        ;; ^^ https://day8.github.io/re-frame/Flows/#subscribing-to-flows
        ;; meant for subbing to flows which seem to have their own memory and caching semantics
        panels-hash2    (hash (ut/remove-underscored @(ut/tracked-sub ::panels {})))
        [tab-x tab-y]   (if tab
                          (let [tt @(ut/tracked-subscribe [::tab-recenter tab])] 
                            [(get tt 0 0) (get tt 1 0)])
                          [0 0])
        start-y        (if tab tab-y 0)
        start-x        (if tab tab-x 0)
        bricks-high    (+ (js/Math.floor (/ @(ut/tracked-subscribe [::subs/h]) brick-size)) 1)
        bricks-wide    (+ (js/Math.floor (/ @(ut/tracked-subscribe [::subs/w]) brick-size)) 1)
        ; bricks-wide (- end-x start-x)
        ; bricks-high (- end-y start-y)
        ; current-grid @(ut/tracked-subscribe [::build-grid bricks-high bricks-wide start-y start-x])
        ;current-grid @(ut/tracked-subscribe [::build-grid end-y end-x start-y start-x])
        ;wspace @(ut/tracked-subscribe [::workspace []])
        selected-block @(ut/tracked-sub ::selected-block {})
        ;;selected-block @(ut/tracked-subscribe [::selected-block])
        lines?         @(ut/tracked-sub ::lines? {})
        peek?          @(ut/tracked-sub ::peek? {})
        full-no-ui?    @(ut/tracked-sub ::full-no-ui? {})
        ;editor-panels-map (editor-panels bricks-high bricks-wide)
        ;meta-menu (meta-menu-panel bricks-high bricks-wide)
        ;editor-keys (keys editor-panels-map)
        ;workspace (merge wspace
        ;                 (if (and editor? (not (= selected-block "none!")))
        ;                  editor-panels-map {}))
        ; logo-bricks [[(- bricks-wide 4) (- bricks-high 4)]
        ;              [(- bricks-wide 4) (- bricks-high 3)]
        ;              [(- bricks-wide 3) (- bricks-high 3)]
        ;              [(- bricks-wide 3) (- bricks-high 4)]]
        ;used-bricks (used-space workspace bricks-high bricks-wide)
        ;used-bricks @(ut/tracked-subscribe [::used-space workspace bricks-high bricks-wide])
        ;used-bricks (cset/union (set @(ut/tracked-subscribe [::used-space2 bricks-high bricks-wide])) (set logo-bricks))
        ;used-bricks @(ut/tracked-subscribe [::used-space3 bricks-high bricks-wide start-y end-y start-x end-x])
        ;brick-roots (vec (for [[k v] brick-map] (:root v))) ;(vec (keys brick-map))
        brick-roots    (if tab
                         ;@(ut/tracked-subscribe [::all-roots-tab tab])
                         @(rfa/sub ::all-roots-tab {:tab tab})
                         ;@(ut/tracked-subscribe [::all-roots])
                         @(rfa/sub ::all-roots {})
                         )
        audio-playing? @(ut/tracked-sub ::audio/audio-playing? {})
        ;brick-roots @(ut/tracked-subscribe [::all-roots2 start-y end-y start-x end-x])
        ;diff-grid1 (cset/union (set brick-roots) (cset/difference (set current-grid) (set used-bricks)))
        ;diff-grid2 (cset/union (set brick-roots) (remove (set used-bricks) (set current-grid)))
        ;diff-grid (merge (remove used-bricks current-grid) brick-roots)
        top-start      (* start-y brick-size)               ;-100 ;; if shifted some bricks away...
        left-start     (* start-x brick-size)]
    ;(ut/tapp>> @panels-hash)
    ;(ut/tapp>> [:something-running? @(ut/tracked-subscribe [::something-running?])])

    (when false ;;(not @on-scrubber?) ;true ; external? UPDATE-PANELS-HASH DISABLED TMP!! WHEN NON SCRUBBER AND ONLY WHNE MOUSE IS INACTIVE?
      (when (not (= panels-hash2 panels-hash1))  ;; core update for blind backend updating / external editing 2-way
        (ut/tracked-dispatch [::update-panels-hash])))

    ;; ^^  TODO should this be at the honeycomb level instead? grid is one hell of a tick to key off of... feels overly sledgehammer-y.


    ;; (ut/tapp>> [:sub-tracker @(rfa/sub ::client-name) (vec (take 40 (reverse (map (fn [x] [(last x) (first x)]) (sort-by last @ut/subscription-counts)))))])
    ;; (ut/tapp>> [:dispatch-tracker @(rfa/sub ::client-name) (vec (take 40 (reverse (map (fn [x] [(last x) (first x)]) (sort-by last @ut/dispatch-counts)))))])
    ;; (ut/tapp>> [:dispatch-peek! (vec (map first @ut/dispatch-counts))])
    ;; (ut/tapp>> [:sub-counter @(rfa/sub ::client-name) @ut/parameter-keys-hit])

    (when false ;; (cstr/includes? (str @(rfa/sub ::client-name)) "emerald")
      (ut/tapp>> [:dispatch-peek! @(rfa/sub ::client-name)  (vec (reverse (sort-by val (frequencies @ut/simple-dispatch-counts))))])
      (ut/tapp>> [:sub-peek! @(rfa/sub ::client-name)  (vec (reverse (sort-by val (frequencies @ut/simple-subscription-counts))))]))

    ;(ut/tapp>> [:grid-render])
    ;(when @over-block? (ut/tapp>> [:over]))
    ;(ut/tapp>> [:grid bricks-high bricks-wide brick-offset-y brick-offset-x current-grid]) ;current-grid])


    ^{:key (str "base-brick-grid")}
    [re-com/h-box
     ;;:style {:zoom 0.7} ;; this works, but drag and drop gets all fucked up (for ""size to fit"" presentations)
     ;; with some fixing, this MIGHT be a legit strategy for zooming to fit on some axis as a special board property for viewing
     ;; on instead we use snapshots to create several configurations and automatically swap between them. :auto-swap? true
     ; :style {:font-size "10px"
     ;         :background-color "#47555e"
     ;         ;:font-family "Lato"
     ;         }
     ;:style {:transform "scale(0.75)"}
     ;:attr {:on-drag-enter #(ut/tapp>> [:dragging-over [ tab]])}
     :children [((maybedoall)
                 (for [[bw bh brick-vec-key] brick-roots]                ;diff-grid1] ;(if @dragging? current-grid brick-roots)] @(ut/tracked-subscribe [::bricks/subq-panels selected-block])
                   (let [bricksw             (* bw brick-size)
                         bricksh             (* bh brick-size)
                         top                 (+ top-start bricksh)
                         left                (+ left-start bricksw)
                         brick-vec           [bw bh]
                          ;edge? (or (= bw (- bricks-wide 1)) (= bh (- bricks-high 1)))
                         root?               true          ; (some #(= brick-vec %) brick-roots)
                        ;;  brick-vec-key       (if tab
                        ;;                        @(ut/tracked-subscribe [::lookup-root-key-tab brick-vec tab])
                        ;;                        ;@(ut/tracked-subscribe [::lookup-root-key brick-vec])
                        ;;                        brick-vec-key
                        ;;                        )
                         ;;_ (ut/tapp>> [:tab tab brick-vec-key])
                         ;;body-shell           @(rfa/sub ::resolved-block-body-shell {:panel-key brick-vec-key})
                         body-shell           @(re-frame/subscribe [::resolved-block-body-shell {:panel-key brick-vec-key}])
                         ;;rs                  (fn [edn] (resolver/logic-and-params edn brick-vec-key))
                         ;w                   (rs @(ut/tracked-subscribe [::panel-width brick-vec-key]) brick-vec-key)
                         ;h                   (rs @(ut/tracked-subscribe [::panel-height brick-vec-key]) brick-vec-key)
                         ;name                (rs @(ut/tracked-subscribe [::panel-name brick-vec-key]) brick-vec-key)
                         ;z                   (rs @(ut/tracked-subscribe [::panel-depth brick-vec-key]) brick-vec-key)
                         {:keys [w h name z]} body-shell
                        ;;  _ (ut/tapp>>  [:pp brick-vec-key body-shell w h name z])

                         trunc-name          (when (not (nil? name))
                                               (let [charpx      5.75
                                                     pixel-width (* (count (str name)) charpx)
                                                     panel-width (- (* w brick-size) 50)]
                                                 (if (> pixel-width panel-width) (str (subs name 0 (/ panel-width charpx)) "...") name)))
                          ;panel-clone-map   (ut/tracked-subscribe [::panel-map brick-vec-key])  ;; questionable
                        ; cross-breed?        @(ut/tracked-subscribe [::un-fired-cross-breed? brick-vec-key])
                        ; _                   (when cross-breed? ;; TODO , move this side-effect elsewhere - its relatively safe here, but feels gross none-the-less
                        ;                       (ut/tracked-dispatch [::cross-breed brick-vec-key]))
                         block-width         (if root? (+ 1 (* brick-size (if (= w 0) (- bricks-wide 1) w))) brick-size)
                         block-height        (if root? (+ 1 (* brick-size (if (= h 0) (- bricks-high 1) h))) brick-size)
                         selected?           (= brick-vec-key selected-block)
                         block-selected?     selected?
                         ;;subq-blocks         (if root? @(ut/tracked-subscribe [::subq-panels selected-block]) [])
                         subq-blocks         (if root? @(ut/tracked-sub ::subq-panels-alpha {:panel-id selected-block}) [])
                         parent-of-selected? (some #(= % brick-vec-key) subq-blocks)
                          ;param-hover-query (try (nth @param-hover 1) (catch :default e nil))
                         ;;editor?             @(ut/tracked-subscribe [::editor?])
                         editor?             @(rfa/sub ::editor? {})
                         hover-q?            (if (and editor? root?)
                                               (or @(ut/tracked-subscribe [::has-query? brick-vec-key
                                                                           (try (nth @param-hover 1) (catch :default _ nil))])
                                                   @(ut/tracked-subscribe [::has-query? brick-vec-key @query-hover])
                                                   (and (= @query-hover brick-vec-key) (= @db/item-browser-mode :blocks))) false)
                          ;;editor-panel? false ; (true? (some #(= brick-vec-key %) editor-keys))
                          ;subq-used (if root? @(ut/tracked-subscribe [::subq-used selected-block]) [])
                          ;subq-produced (if root? @(ut/tracked-subscribe [::subq-produced selected-block]) [])
                          ;subq-used2 (if root? @(ut/tracked-subscribe [::subq-used brick-vec-key]) [])
                         subq-mapping        (if root? 
                                               ;;@(ut/tracked-subscribe [::subq-mapping])
                                               @(ut/tracked-sub ::subq-mapping-alpha {})
                                               [])
                         upstream?           (some #(= % brick-vec-key) (ut/cached-upstream-search subq-mapping selected-block))
                         downstream?         (some #(= % brick-vec-key) (ut/cached-downstream-search subq-mapping selected-block))
                          ;hovered? false
                         sql-keys            @(ut/tracked-sub ::panel-sql-call-keys {:panel-key brick-vec-key})
                         reco-selected       (let [;;rr @(ut/tracked-subscribe [::conn/clicked-parameter-key [:viz-tables-sys/table_name]])
                                                   rr @(rfa/sub ::conn/clicked-parameter-key-alpha {:keypath [:viz-tables-sys/table_name]})
                                                   rr (if (not (nil? rr)) (keyword (ut/replacer rr "_" "-")) nil)] rr)
                         viz-reco?           (and (or (= selected-block "none!") (nil? selected-block))
                                                  (some #(= % reco-selected) sql-keys)
                                                  editor?
                                                  (= @db/editor-mode :vvv))
                         ;current-tab         @(ut/tracked-subscribe [::selected-tab])
                         ;ghosted?            (rs @(ut/tracked-subscribe [::ghosted? brick-vec-key]) brick-vec-key)
                         ;no-ui?              (or (rs @(ut/tracked-subscribe [::no-ui? brick-vec-key]) brick-vec-key) (not (nil? tab)))
                         ;hidden?             (rs @(ut/tracked-subscribe [::hidden? brick-vec-key]) brick-vec-key)
                         ;minimized?          (rs @(ut/tracked-subscribe [::minimized? brick-vec-key]) brick-vec-key)
                         {:keys [ghosted?
                                 no-ui?
                                 hidden?
                                 minimized?]} body-shell
                        ;;  _ (ut/tapp>> [:body-shell body-shell])
                         no-ui?              (or no-ui? (not (nil? tab)) full-no-ui?)
                         ;panel-style         ;(rs
                         ;                     @(ut/tracked-subscribe [::panel-style brick-vec-key])
                         ;                    ; brick-vec-key)
                         panel-style         (get body-shell  :style)
                         tab-color           (cond
                                               selected? "#9973e0"
                                               viz-reco? "#ffb400"
                                               hover-q? "#c7005d"
                                               parent-of-selected? "#e6ed21"
                                               upstream? "#7be073"
                                               downstream? "#05dfff"
                                               :else "#ffffff10")
                         tab-text-color      (cond
                                               selected? "#000000"
                                               viz-reco? "#000000"
                                               hover-q? "#000000"
                                               parent-of-selected? "#000000"
                                               upstream? "#000000"
                                               downstream? "#000000"
                                               :else (theme-pull :theme/block-tab-selected-font-color "#ffffff55"))
                         iconization         @(ut/tracked-subscribe [::iconized brick-vec-key])
                         being-dragged?      (= brick-vec-key @dragging-block)
                         drag-action?        (and @mouse-dragging-panel?
                                                  ;selected?
                                                  being-dragged?)
                          ;;; all below are for bottom tab bar
                         single-view?        @(ut/tracked-subscribe [::is-single-view? brick-vec-key])
                         no-view?            @(ut/tracked-subscribe [::has-no-view? brick-vec-key])
                         ;;selected-view       (rs @(ut/tracked-subscribe [::selected-view brick-vec-key]) brick-vec-key) ;;; SUSPICOUS? todo - 6/1/24, cRASHING TO JS SOMEWHERE
                         ;;selected-view       @(ut/tracked-subscribe [::selected-view brick-vec-key])
                         selected-view       @(ut/tracked-sub ::selected-view-alpha {:panel-key brick-vec-key})
                         selected-view       (cond viz-reco? reco-selected
                                                   (and (nil? selected-view) no-view? (seq sql-keys))
                                                   (first sql-keys)
                                                   :else selected-view)
                         is-grid?            @(ut/tracked-subscribe [::is-grid? brick-vec-key selected-view])
                         is-pinned?          @(ut/tracked-subscribe [::is-pinned? brick-vec-key])
                         col-selected?       @(ut/tracked-subscribe [::column-selected-any-field? brick-vec-key selected-view])
                         ;;views               @(ut/tracked-subscribe [::views brick-vec-key])
                         views               @(rfa/sub ::views {:panel-key brick-vec-key})

                         ;views               (if (keyword? views) (rs views brick-vec-key) views) ;; for full view aliase... sketch REVISIT

                        ; reco-statuses       @(ut/tracked-subscribe [::queries-reco-status brick-vec-key]) ;; in for reactions
                         all-views           (if single-view? [] (vec (keys views)))
                          ;multiple-oz-views? @(ut/tracked-subscribe [::multiple-oz-views? brick-vec-key])
                          ;all-views (if multiple-oz-views? (conj all-views :layered-viz) all-views)
                         base-view-name      :view         ;; basically a default for single views
                          ;sql-keys0 (if (nil? sql-keys) [] sql-keys)
                         mouse-down?         (atom false)
                         ;tab-parent-offset   (if tab @(ut/tracked-subscribe [::tab-offset tab]) [0 0])
                         tab-offset          (if tab @(ut/tracked-subscribe [::tab-offset2 tab]) [0 0])
                         click-delay         100
                         mixed-keys          ;;(try
                                               (cond (and single-view? (seq sql-keys)) (conj sql-keys base-view-name)
                                                   (and no-view? (seq sql-keys)) sql-keys
                                                   :else (into all-views sql-keys))
                                              ;;  (catch :default e
                                              ;;    (do (ut/tapp>> [:MIXED-KEYS-ERROR! (str e) (.-message e) :stack (.-stack e)])
                                              ;;        (js/console.log (str :MIXED-KEYS-ERROR! (str e) (.-message e) :stack (.-stack e)))
                                              ;;        [])))
                         zz                  (if (or selected? hover-q?)
                                               (+ z 50) (+ z 10))
                         theme-base-block-style (theme-pull :theme/base-block-style {})
                         theme-base-block-style-map (when (map? theme-base-block-style) theme-base-block-style)]
                      ; (ut/tapp>> [:subq-blocks subq-blocks])
                      ; (ut/tapp>> [:subq-mapping subq-mapping :ups (ut/cached-upstream-search subq-mapping selected-block) :downs (ut/cached-downstream-search subq-mapping selected-block)])
                      ; (when root? (ut/tapp>> [:subq-blocks brick-vec-key ;subq-blocks
                      ;                    ;(map :p (filter #(>= (get % :d) 1) (downstream-map selected-block nil subq-mapping 0 [])))
                      ;                    ;(ut/cached-upstream-search subq-mapping brick-vec-key)
                      ;                    ;(downstream-lookups subq-mapping brick-vec-key)
                      ;                    ;subq-mapping
                      ;                    ]))
                      ;; (ut/tapp>> [:rr reco-selected sql-keys viz-reco? selected-block])
                      ;; (ut/tapp>> [brick-vec-key :params-used (param-usage brick-vec-key)])
                      ;; (ut/tapp>> [brick-vec-key subq-mapping subq-blocks])
                      ;; (ut/tapp>> [:up-from-sel (ut/cached-upstream-search subq-mapping selected-block)
                      ;       :down-from-sel (ut/cached-downstream-search subq-mapping selected-block)])

                     (if (and (not (nil? iconization)) (ut/not-empty? iconization)) ;; remove all this for now? TOO MUCH STUFF. make it WORK to be put back...

                       (let [icon-h (get iconization :h 1)
                             icon-w (get iconization :w 1)
                             vv (get iconization :view [:box :child "icon?"])]
                         [re-com/box
                          :size "auto"
                          :width (px (* brick-size icon-w))
                          :height (px (* brick-size icon-h))
                          :align :center :justify :center
                          :attr {;:on-click #(js/alert (if tab "in container" "not in container"))
                                 :on-context-menu #(when (not tab) (ut/tracked-dispatch [::toggle-icon-block brick-vec-key]))
                                 ;:on-mouse-enter #(ut/tapp>> [:dragging-over [current-tab tab]])
                                 :on-mouse-down #(mouse-down-handler % brick-vec-key tab-offset true)
                                 ;:on-mouse-up (fn [_] (reset! mouse-down? false))
                                 :on-mouse-over  #(when (and (not @over-block?) (not (= brick-vec-key @over-block)))
                                                    (reset! over-block brick-vec-key)
                                                    (reset! over-block? true)) ;; took out enter for watched over...? 12/14/23
                                 :on-mouse-leave  #(do (reset! over-block? false)
                                                       (reset! over-block nil))
                                 :on-double-click #(do
                                                     (tag-screen-position %)
                                                     ;(ut/get-parent-z-index %)
                                                     ;(ut/tracked-dispatch [::move-to-current-tab brick-vec-key])
                                                     ;(ut/tracked-dispatch [::toggle-icon-block brick-vec-key])
                                                     (ut/tracked-dispatch [::launch-clone brick-vec-key]))}
                          :style (merge
                                  {:position         "fixed"
                                   :user-select      "none"
                                   :border           (cond
                                                       selected? "2px solid #9973e0"
                                                       :else "2px solid #ffffff05")
                                   ;:transition       "all 100ms ease-in-out"
                                   :color            (theme-pull :theme/block-title-font-color nil)

                                   :cursor           "pointer"
                                   :z-index          zz
                                   :background-color (theme-pull :theme/base-block-color nil)
                                   :top              (px top)
                                   :left             (px left)}
                                  theme-base-block-style-map ;; just in case
                                  panel-style)
                          :child [honeycomb-fragments vv icon-w icon-h]])

                       (when (not (cstr/starts-with? (str brick-vec-key) ":query-preview"))
                         ^{:key (str "brick-" brick-vec-key)}
                         [re-com/box
                          :width (px block-width)
                          :height (px block-height)

                        ;;; need a block key for :hover-watch? or something, not need to do this for all blocks? - see user-sys param setting below. lots of unneeded thrash?
                          :attr ;(merge
                               ;{} ; (if (not root?) {:on-mouse-down mouse-down-handler-new} {})
                          {;:on-mouse-enter  #(do (reset! over-block? true)
                         ;                      ;(ut/tracked-dispatch [::conn/click-parameter [:user-sys] {:over-block brick-vec-key}])
                         ;                      )

                           :on-mouse-over  #(when (not @over-block?)
                                              (reset! over-block brick-vec-key)
                                              (reset! over-block? true)) ;; took out enter for watched over...? 12/14/23
                           :on-mouse-leave  #(do (reset! over-block? false)
                                                 (reset! over-block nil))}
                                               ;(ut/tracked-dispatch [::conn/click-parameter [:user-sys] nil])

                               ; )
                       ;:padding "3px"
                          :style (merge
                                  {:position         "fixed"    ;"absolute" ;"fixed"
                                   :font-size        "13px"
                               ;:-webkit-transform "translateZ(0)"
                                   :z-index          zz
                               ;:display (if (not used?) "none" "inherit")
                               ; :filter           (cond
                               ;                     (and selected? col-selected?)
                               ;                     "drop-shadow(0.35rem 0.35rem 0.4rem #9973e0) drop-shadow(-0.35rem -0.35rem 0.4rem #9973e0)"
                               ;                     selected? ;; "#9973e0"
                               ;                     "drop-shadow(0.35rem 0.35rem 0.4rem #9973e0) drop-shadow(-0.35rem -0.35rem 0.4rem #9973e0)"
                               ;                    ;"drop-shadow(0.35rem 0.35rem 0.4rem rgba(0, 0, 0, 0.5))"
                               ;                     :else "")
                                   :filter (when selected?
                                             (theme-pull :theme/base-block-filter-selected
                                                         "drop-shadow(0.35rem 0.35rem 0.4rem #9973e0) drop-shadow(-0.35rem -0.35rem 0.4rem #9973e0)"))

                               ;(cond
                               ;          ;selected? "drop-shadow(0.35rem 0.35rem 0.4rem #9973e066)"
                               ;          ;drag-action? "opacity(0.4)"
                               ;          root? "" ;"drop-shadow(0.35rem 0.35rem 0.4rem rgba(0, 0, 0, 0.5))" ;"" ; "drop-shadow(0.35rem 0.35rem 0.4rem rgba(0, 0, 0, 0.5))"
                               ;          :else "drop-shadow(0.35rem 0.35rem 0.4rem rgba(0, 0, 0, 0.5))")
                                   :user-select      "none"
                                   :outline          "0px"
                                   ;:overflow         "visible" ;(if is-grid? "auto" "visible")
                                   :overflow (if is-grid?
                                               "hidden" ;"auto"
                                               "visible")
                               ;:opacity (if  drag-action? 0.3 1)
                               ;; https://www.w3schools.com/howto/howto_css_flip_card.asp
                               ;; https://3dtransforms.desandro.com/cube
                               ;; https://3dtransforms.desandro.com/card-flip
                               ;:transform (if selected? "rotateY(180deg)" "")
                                   :display (if (or (and hidden? (not selected?))
                                                    minimized?
                                                    (cstr/starts-with? (str brick-vec-key) ":query-preview"))
                                              "none"
                                              "inherit")
                               ;:-webkit-backface-visibility "hidden"
                               ; :transition "transform 0.8s"

                               ;   :transform (if selected? "scale(1.1)" "")
                               ;   :transition "all 0.3s ease-in-out"
                               ;   :transform-style "preserve-3d"

                                   :transform        (when peek?
                                                       (if selected? "scale(0.7)" ; nil ;"scale(0.85)"
                                                           "scale(0.7)"))
                               ; :transition       "all 0.2s ease-in-out"
                                   :transform-style  "preserve-3d" ;; important for tab embedding!

                                   :box-shadow (when (= brick-vec-key @db/speaking)
                                                 (let [block-id brick-vec-key ;:audio
                                                       talking-block? true]
                                                   (cond (and audio-playing? talking-block?)
                                                         (str "1px 1px " (px (* 90 (+ 0.1
                                                                                      (get @db/audio-data block-id))))  " "
                                                              (theme-pull :theme/editor-outer-rim-color nil))
                                                           ;(str "1px 1px 70px red")
                                  ;(or (and hovered? @on-block) kit-drop?)
                                  ;(str "1px 1px 30px " (get db/block-codes block-type) 87)
                                                         :else "none")))

;:perspective "1000px"
                               ;:backface-visibility "hidden"
                                   :border           (cond      ;editor-panel? "0px solid #bc798c"
                                                   ;selected? "3px solid #58A279"
                                                   ;edge? "1px solid #00000066"
                                                   ;parent-of-selected? "2px dashed #9973e0"

                                                       selected? "2px solid #9973e0"
                                                       viz-reco? "2px dashed #ffb400"
                                                       hover-q? "2px solid #c7005d"
                                                   ;upstream? "2px dashed #7be07377"
                                                   ;root? "2px solid #00000000" ; "3px solid #00000000"
                                                       parent-of-selected? "2px solid #e6ed21" ;"#9973e0" ;"#9973e0"
                                                       upstream? "2px solid #7be073" ;"#09050d" ;"#7be073"
                                                       downstream? "2px dashed #05dfff" ;"#09050d" ;"#7be073"

                                                       ghosted? "2px solid #00000000"

                                                   ;root? "2px solid #ffffff10"

                                                       :else "2px solid #ffffff05") ; "1px solid #00000028"

                               ; :background-image (if selected? "linear-gradient(330deg, rgba(225, 225, 225, 0.05) 0%,
                               ;                                  rgba(225, 225, 225, 0.05) 33.333%,
                               ;                                  rgba(114, 114, 114, 0.05) 33.333%,
                               ;                                  rgba(114, 114, 114, 0.05) 66.666%,
                               ;                                  rgba(52, 52, 52, 0.05) 66.666%,
                               ;                                  rgba(52, 52, 52, 0.05) 99.999%),
                               ;                                  linear-gradient(66deg,
                               ;                                  rgba(181, 181, 181, 0.05) 0%,
                               ;                                  rgba(181, 181, 181, 0.05) 33.333%,rgba(27, 27, 27, 0.05) 33.333%,
                               ;                                  rgba(27, 27, 27, 0.05) 66.666%,rgba(251, 251, 251, 0.05) 66.666%,
                               ;                                  rgba(251, 251, 251, 0.05) 99.999%),linear-gradient(225deg,
                               ;                                  rgba(98, 98, 98, 0.05) 0%, rgba(98, 98, 98, 0.05) 33.333%,
                               ;                                  rgba(222, 222, 222, 0.05) 33.333%, rgba(222, 222, 222, 0.05) 66.666%,
                               ;                                  rgba(228, 228, 228, 0.05) 66.666%, rgba(228, 228, 228, 0.05) 99.999%),
                               ;                                  linear-gradient(90deg,
                               ;                                  rgb(28, 20, 63, 0.11),
                               ;                                  rgb(40, 160, 253, 0.11))"
                               ;                       ;parent-of-selected? "#9973e033"
                               ;                       ;upstream? "#7be07333"
                               ;                       ""
                               ;                       )
                                   :opacity          (cond (and lines?
                                                                (or downstream? upstream? parent-of-selected? hover-q? viz-reco? selected? (= "none!" selected-block))) 1.0
                                                           lines? 0.5
                                                           :else 1.0)
                                   :background-color (cond      ;drag-action? "#00000000"
                                                       ghosted? "#00000000"
                                                       col-selected? "#111214" ; (str (theme-pull :theme/base-block-color-selected nil) 77) ; "#222024"
                                                       selected? (theme-pull :theme/base-block-color-selected nil) ;"#0b031b"
                                                   ;parent-of-selected? "#1c0c2a" ;"#9973e0" ;"#9973e0"
                                                   ;upstream? "#0d0613" ;"#09050d" ;"#7be073"
                                                       root? (theme-pull :theme/base-block-color nil) ;  "#0b1122" ; "#304148"
                                                   ;hovered? "#9973e088" ;"#c591c188"

                                                   ;editor-panel? "#142a33"

                                                   ;edge? "#0b112266" ;;"#00000022"

                                                       :else "#55afb344")
                                   :top              (px top)
                                   :left             (px left)}
                                  theme-base-block-style-map
                                  panel-style)
                          :child                               ;(if root?
                          ^{:key (str "brick-" brick-vec "-root")}
                          [re-com/v-box
                           :gap "1px"
                           :size "1" :justify :between
                           ;:attr {:on-drag-over   #(ut/tapp>> [:dragging-over [current-tab tab]])}
                        ;:style {:filter "drop-shadow(0.35rem 0.35rem 0.4rem rgba(0, 0, 0, 0.5))" } ;; really cool actually. INNER CONTENT drop shadow
                           :children [;(when (not editor-panel?)

                                      (if ;(and (not ghosted?)
                                        ;     (not selected?)
                                        ;     (not no-ui?))
                                       (or (and (not ghosted?) (not no-ui?)) selected?)
                                        ^{:key (str "brick-" brick-vec "-header1")}
                                        [re-com/h-box
                                      ;;  :attr
                                      ;;  (when selected?
                                      ;;    {:on-mouse-down mouse-down-handler
                                      ;;     :on-double-click #(ut/tracked-dispatch [::toggle-minimize-block brick-vec-key])})
                                         :height "20px"
                                         :padding "3px"
                                         :justify :between :align :center
                                         :style {:background-color (cond ;drag-action? "#58A27977"
                                                                  ;parent-of-selected? "#9973e022"
                                                                     selected? "#9973e022" ; "#3b528b" ; "#58A279"
                                                                     parent-of-selected? "#9973e022"
                                                                     upstream? "#9973e022"
                                                                     downstream? "#05dfff22"
                                                                  ;; root?  "#0b1122" ; "#304148"
                                                                  ;:else "#DC143C18"
                                                                     :else "inherit")
                                              ;:overflow "visible"
                                              ; :transition "all 0.2s ease-in-out"
                                              ; :transform-style "preserve-3d"
                                              ;:border-top "1px solid #ffffff11"
                                              ;:border-left "1px solid #ffffff11"
                                              ;:border-right "1px solid #ffffff11"
                                              ;;?;;    :border "1px solid #00000066"
                                              ;:font-weight 400
                                              ;:color "#ffffff" ;(if selected? "#000000" "#ffffff")

                                                 :z-index          (if (or is-grid? selected?) (+ zz 10) zz)
                                                 :color            (cond selected? (theme-pull :theme/block-title-selected-font-color nil) ;"#ffffff"
                                                                      ;parent-of-selected? "#9973e0"
                                                                      ;upstream? "#7be073"
                                                                         :else (theme-pull :theme/block-title-font-color nil)) ;"#ffffff"

                                                 :cursor           (if selected? "grab" "pointer")}
                                         :children [(when (<= w 5)
                                                      ^{:key (str "brick-" brick-vec "-header2")}
                                                      [re-com/box
                                                       :size "1"
                                                       :height "18px"
                                                       :style {;:border  "1px solid white"
                                                               :margin-top "-4px" ;; weird boxing so user doesnt click on the border and not get desired result
                                                               :padding-top  "4px" ;; weird boxing so user doesnt click on the border and not get desired result
                                                               :margin-left "-4px"} ;; weird boxing so user doesnt click on the border and not get desired result
                                                       :attr {:on-click #(ut/tracked-dispatch [::select-block brick-vec-key])
                                                              :on-mouse-down (fn [e]
                                                                               (reset! mouse-down? true)
                                                                               (js/setTimeout
                                                                                #(when @mouse-down?
                                                                                   (mouse-down-handler e brick-vec-key tab-offset))
                                                                                click-delay))
                                                              :on-mouse-up (fn [_] (reset! mouse-down? false))}
                                                       :child " "])
                                                    (when (> w 5)
                                                      ^{:key (str "brick-" brick-vec "-header3")}
                                                      [re-com/box
                                                       :size "1"
                                                       :height "22px"
                                                       :attr {:on-click #(ut/tracked-dispatch [::select-block brick-vec-key])
                                                              :on-mouse-down (fn [e]
                                                                               (reset! mouse-down? true)
                                                                               (js/setTimeout
                                                                                #(when @mouse-down?
                                                                                   (mouse-down-handler e brick-vec-key tab-offset))
                                                                                click-delay))
                                                              :on-mouse-up (fn [_] (reset! mouse-down? false))}
                                                    ;:align :center :justify :center
                                                       :style {:opacity      (if selected? 1.0 0.4)
                                                               :margin-top "-4px" ;; weird boxing so user doesnt click on the border and not get desired result
                                                               :padding-top  "4px" ;; weird boxing so user doesnt click on the border and not get desired result
                                                               :margin-left "-4px" ;; weird boxing so user doesnt click on the border and not get desired result
                                                               :font-size    "10px"
                                                             ;:border  "1px solid white"
                                                               :font-weight  (if selected? 700 300)
                                                               :padding-left "6px"}
                                                       :child (str trunc-name)]) ;(str brick-vec-key " " trunc-name) ;(str brick-vec-key)
                                                    ;:child " "
                                                    ;:width "5px"

                                                  ;; (when false ;(> w 7)
                                                  ;;   ^{:key (str "brick-" brick-vec "-header4")}
                                                  ;;   [re-com/box :size "1"
                                                  ;;    :attr {:on-click #(ut/tracked-dispatch [::select-block brick-vec-key])}
                                                  ;;    :style {:font-weight 300}
                                                  ;;    :child (str name)])
                                                    ^{:key (str "brick-" brick-vec "-header5")}
                                                    (let [hovered-on? (or selected? (= brick-vec-key @over-block))]
                                                      [re-com/h-box
                                                       :gap "5px"
                                                       :children [;;  (when (not selected?)
                                                             ;;    (draggable (merge @panel-clone-map {:cloned-from brick-vec-key})
                                                             ;;               "meta-menu"
                                                             ;;               ^{:key (str "brick-" brick-vec "-clone-dragger")}
                                                             ;;               [re-com/md-icon-button
                                                             ;;                :md-icon-name "zmdi-plus-circle-o-duplicate"
                                                             ;;                :style {:font-size "12px"
                                                             ;;                        :cursor "grab"
                                                             ;;                        :opacity 0.5
                                                             ;;                        :margin-top "-1px"}]

                                                             ;;              ; [re-com/box :child "clone"
                                                             ;;              ;  :padding "3px"
                                                             ;;              ;  :style {:color "#ffffff90" :font-size "10px"
                                                             ;;              ;          :font-weight 500 }
                                                             ;;              ;  :height "12px"]
                                                             ;;               ))

                                                                  (when col-selected?
                                                                    ^{:key (str "brick-" brick-vec "-header-pp")}
                                                                    [re-com/md-icon-button
                                                                     :md-icon-name (if col-selected? "zmdi-pause" "zmdi-play")
                                                                ;:on-click #(ut/tracked-dispatch [::delete-panel brick-vec-key])
                                                                     :style {:font-size "15px"
                                                                             :opacity   0.25
                                                                             :cursor    "pointer"}])
                                                                        ;:color "#00000000"
                                                                        ;:filter "drop-shadow(1px 1px white)"
                                                                        ;:margin-top "-2px"


                                                                  (when hovered-on?
                                                                    ^{:key (str "brick-" brick-vec "-header-pin")}
                                                                    [re-com/md-icon-button
                                                                     :md-icon-name (if is-pinned? "zmdi-pin-off" "zmdi-pin")
                                                                     :on-click #(do (ut/tracked-dispatch [::toggle-pin-block brick-vec-key])
                                                                                  ;(ut/tracked-dispatch [::select-block "none!"])
                                                                                    ;(do)
) ;(reset! over-block? false)
                                                                                      ;(ut/tracked-dispatch [::conn/click-parameter [:user-sys] nil])

                                                                     :style {:font-size "15px"
                                                                             :opacity   (if is-pinned? 0.8 0.1)
                                                                       ;:display (if (or selected? (= brick-vec-key @over-block))
                                                                       ;           "inherit" "none")
                                                                       ;:z-index (if selected? (+ zz 10) zz)
                                                                             :cursor    "pointer"}])
                                                                      ;:margin-top "-1px"


                                                                  (when hovered-on?
                                                                    ^{:key (str "brick-" brick-vec "-header-min")}
                                                                    [re-com/md-icon-button
                                                                     :md-icon-name "zmdi-window-minimize"
                                                                     :on-click #(do (ut/tracked-dispatch [::toggle-minimize-block brick-vec-key])
                                                                                    (when selected? (ut/tracked-dispatch [::select-block "none!"]))
                                                                                    (do (reset! over-block? false)))
                                                                                      ;(ut/tracked-dispatch [::conn/click-parameter [:user-sys] nil])

                                                                     :style {:font-size "15px"
                                                                             :opacity   0.33
                                                                       ;:display (if (or selected? (= brick-vec-key @over-block))
                                                                       ;           "inherit" "none")
                                                                       ;:z-index (if selected? (+ zz 10) zz)
                                                                             :cursor    "pointer"}])
                                                                      ;:margin-top "-1px"


                                                                  (when hovered-on?
                                                                    ^{:key (str "brick-" brick-vec "-header-icon")}
                                                                    [re-com/md-icon-button
                                                                     :md-icon-name "zmdi-photo-size-select-small"
                                                                     :on-click #(do (ut/tracked-dispatch [::toggle-icon-block brick-vec-key])
                                                                                    (when selected? (ut/tracked-dispatch [::select-block "none!"]))
                                                                                    (do (reset! over-block? false)))
                                                                                      ;(ut/tracked-dispatch [::conn/click-parameter [:user-sys] nil])

                                                                     :style {:font-size "15px"
                                                                             :opacity   0.33
                                                                       ;:display (if (or selected? (= brick-vec-key @over-block))
                                                                       ;           "inherit" "none")
                                                                       ;:z-index (if selected? (+ zz 10) zz)
                                                                             :cursor    "pointer"}])
                                                                      ;:margin-top "-1px"


                                                                  (when (or selected? (= brick-vec-key @over-block))
                                                                    ^{:key (str "brick-" brick-vec "-header-close")}
                                                                    [re-com/md-icon-button
                                                                     :md-icon-name "zmdi-close"
                                                                     :on-click #(do (ut/tracked-dispatch [::delete-panel brick-vec-key])
                                                                                    (do (reset! over-block? false)))
                                                                                      ;(ut/tracked-dispatch [::conn/click-parameter [:user-sys] nil])

                                                                     :style {:font-size "15px"
                                                                             :opacity   0.33
                                                                       ;:display (if (or selected? (= brick-vec-key @over-block))
                                                                       ;           "inherit" "none")
                                                                       ;:z-index (if selected? (+ zz 10) zz)
                                                                             :cursor    "pointer"}])]])]]
                                                                      ;:margin-top "-1px"

                                        [re-com/gap :size "20px"]) ;)

                                   ;; main content box
                                      ^{:key (str "brick-" brick-vec "-content-box")}
                                      [re-com/box
                                       :padding "4px"
                                       :size "none"
                                       :height (px (- block-height 40))
                                       :style {;:border "1px solid #ffffff11"
                                            ;:border-right "1px solid #ffffff11"
                                            ;:border-left "1px solid #ffffff11"
                                            ;:border "2px solid white"
                                            ;:overflow "auto"
                                            ;:overflow "visible" ;;; TODO FIXED THE DROPDOWN BUG.. did it fuck other things? YES!
                                               :overflow "hidden" ;;; TODO YUUUUUP ^^^ ;; inherit and move prog-controlled visible/hidden to honeycomb based on if :dropdown is in that view - will it work properly in layouts?
                                               :color    (str (theme-pull :theme/grid-font-color nil) 88)}
                                       :child
                                       (if drag-action?
                                         ^{:key (str "brick-" brick-vec-key "-dragger")}
                                         [re-com/box :child " " :style {:background-color "#58A27933"}]
                                         ^{:key (str "brick-" brick-vec-key "-honeycomb-box")}
                                         (if @dragging-block ;; @dragging? ;; dragging-block
                                           [re-com/box :child " "]
                                           [rc/catch (if viz-reco?
                                                       [honeycomb brick-vec-key selected-view]
                                                       [honeycomb brick-vec-key])]))]
                                                   ;(honeycomb brick-vec-key)


                                      (if (or (and (not ghosted?) (not no-ui?)) selected?)
                                        (let [];;  single-view? @(ut/tracked-subscribe [::is-single-view? brick-vec-key])
                                           ;;  no-view? @(ut/tracked-subscribe [::has-no-view? brick-vec-key])
                                           ;;  selected-view @(ut/tracked-subscribe [::selected-view brick-vec-key])
                                           ;;  selected-view (cond viz-reco? reco-selected
                                           ;;                      (and (nil? selected-view) no-view? (seq sql-keys))
                                           ;;                      (first sql-keys)
                                           ;;                      :else selected-view)
                                           ;;  views @(ut/tracked-subscribe [::views brick-vec-key])
                                           ;;  all-views (if single-view? [] (vec (keys views)))
                                           ;;  ;multiple-oz-views? @(ut/tracked-subscribe [::multiple-oz-views? brick-vec-key])
                                           ;;  ;all-views (if multiple-oz-views? (conj all-views :layered-viz) all-views)
                                           ;;  base-view-name :view ;; basically a default for single views
                                           ;;     ;sql-keys0 (if (nil? sql-keys) [] sql-keys)
                                           ;;  mixed-keys (cond (and single-view? (seq sql-keys)) (conj sql-keys base-view-name)
                                           ;;                   (and no-view? (seq sql-keys)) sql-keys
                                           ;;                   :else (into all-views sql-keys))
                                           ;;  ;single-tab? (= 1 (count mixed-keys))
                                            ;has-open-input? (some #(= % :open-input) (ut/deep-flatten selected-view))

                                          ^{:key (str "brick-" brick-vec "-footer")}
                                          [re-com/box
                                           :size "none"
                                           :width (px (- block-width 4))
                                           :height "15px"
                                           :style {:overflow "hidden"
                                                 ;:border "1px solid yellow"
                                                   :z-index (if selected? (+ zz 10) zz)}
                                           :child [re-com/h-box
                                                   :size "auto"
                                                   :justify :between
                                                   :children [;[re-com/gap :size "12px"]
                                                              (doall (if (empty? mixed-keys)
                                                                       ^{:key (str "brick-" brick-vec "-footer-gap")}
                                                                       [re-com/gap :size "12px"]
                                                                       ^{:key (str "brick-" brick-vec "-footer-sql-keys")}
                                                                       [re-com/h-box
                                                                        :gap "0px"
                                                                        :children (doall
                                                                                   (for [s mixed-keys]

                                                                                     (let [selected?       (= (if (= s base-view-name) nil s) selected-view)
                                                                                        ;;  bselected?      (= brick-vec-key selected-block)
                                                                                           not-view?       (not (some #(= % s) (keys views)))
                                                                                           reco-count      (when not-view? @(ut/tracked-subscribe [::reco-count s :reco]))
                                                                                 ; reco-running?   (and not-view? @(ut/tracked-subscribe [::reco-running? s]))
                                                                                           [_
                                                                                            single-wait?]   (if not-view? @(ut/tracked-subscribe [::query-waitings s]) [0 0])
                                                                                           reco-ready?     (and not-view? (> reco-count 0)) ; (not @(ut/tracked-subscribe [::reco-running? s])))
                                                                                           query-running?  single-wait?
                                                                                           flow-running?    @(ut/tracked-subscribe [::has-a-running-flow-view? brick-vec-key s])
                                                                                 ; deep-running?   (> waitings 0)

                                                                                           param-keyword [(keyword (str "param/" (ut/safe-name s)))]
                                                                                           is-param?      ;@(ut/tracked-subscribe [::conn/clicked-parameter-key param-keyword])
                                                                                           @(rfa/sub ::conn/clicked-parameter-key-alpha {:keypath param-keyword})
                                                                                           param-dtype (when is-param?
                                                                                                         (ut/data-typer
                                                                                                        ;@(ut/tracked-subscribe [::conn/clicked-parameter-key param-keyword])
                                                                                                          is-param?))
                                                                                           param-dtype  (try (if (and (= param-dtype "vector") (every? string? is-param?)) "string" param-dtype)
                                                                                                             (catch :default _ param-dtype)) ;; since stringified code is technically not a string yet...
                                                                                                           ;; ^^ its a vector of strings so it can be code edited properly...
                                                                                                           ;; kinda fucky, but makes sense
                                                                                           param-color (get (theme-pull :theme/data-colors db/data-colors) param-dtype "orange")
                                                                                           fcolor          (cond
                                                                                                             is-param? param-color
                                                                                                             selected? tab-text-color
                                                                                                             (= s :layered-viz) "#FFA50087" ;(theme-pull :theme/block-tab-selected-font-color "#FFA50087") ;"#FFA50087"
                                                                                                             (= s :dyn-tab) "#FFA50087" ;(theme-pull :theme/block-tab-selected-font-color "#FFA50087") ;"#FFA50087"
                                                                                                             :else (theme-pull :theme/block-title-font-color "#ffffff50"))
                                                                                           bcolor          (if is-param? (str fcolor "22") (if selected? tab-color "inherit"))]
                                                                             ;(ut/tapp>> [:tt @(ut/tracked-subscribe [::http/websocket-status]) ])
                                                                             ;(ut/tapp>> [:s s is-param? param-keyword @(ut/tracked-subscribe [::conn/clicked-parameter-key param-keyword])])
                                                                                       (draggable
                                                                                        (if is-param? ;; drop as a resgular param, not some view object
                                                                                          (let [k           (first param-keyword)
                                                                                                psplit      (ut/splitter (ut/safe-name (str k)) "/")
                                                                                                table       (-> (first psplit) (ut/replacer  #":" "") (ut/replacer  ".*" "") keyword)
                                                                                                field       (keyword (last psplit))]
                                                                                            {:h         2 ;(cond is-image? 6 is-video? 9 :else (+ 2 pheight))
                                                                                             :w         5 ;(cond is-image? 6 is-video? 13 :else pwidth)
                                                                                             :root      [0 0]
                                                                                             :drag-meta {:type        :param
                                                                                                         :param-full  (first param-keyword)
                                                                                                         :param-type  param-dtype
                                                                                                         :param-table table
                                                                                                         :param-field field}})
                                                                                          (sql-spawner-cloner (if (some #(= % s) sql-keys) :query :view) brick-vec-key s))
                                                                                        "meta-menu"
                                                                                        ^{:key (str "brick-" brick-vec "-footer-sql-key-" s)}
                                                                                        [re-com/h-box
                                                                               ;:style {:background-color bcolor}
                                                                                ;:class (when (and (not selected?) query-running?) "zoom-and-fade2")
                                                                                         :children [[re-com/box
                                                                                                     :child (str s
                                                                                                               ;(when is-param? " 👻")
                                                                                                                 (when (and not-view? reco-count)
                                                                                                                   (str " " (nf reco-count))))

                                                                                                     :attr {:on-click #(do (reset! mad-libs-view nil)
                                                                                                                           (clear-preview2-recos)
                                                                                                                           (ut/tracked-dispatch [::select-view brick-vec-key (if (= s base-view-name) nil s)]))}

                                                                                                     :style {:font-size        "12px"
                                                                                                             :cursor           (if selected? "inherit" "pointer")
                                                                                                             :font-weight      (cond ;single-tab? 500
                                                                                                                                 selected? 700
                                                                                                                                 :else 500)
                                                                                                           ;:border-right     (when is-param? (str "1px solid " param-color))
                                                                                                           ;:border-top       (when is-param? (str "4px solid " param-color))
                                                                                                             :background-color bcolor
                                                                                                             :color            fcolor
                                                                                                             :margin-top       "-1px" ;(if is-param? "-3px" "-1px")
                                                                                                             :padding-left     "4px"
                                                                                                             :padding-right    "4px"}
                                                                                                     :height "18px"]

                                                                                                  ;; (when (and selected? is-param?) ;" 👻"
                                                                                                  ;;   [re-com/single-dropdown
                                                                                                  ;;    :style {}
                                                                                                  ;;    ;:parts {:chosen-drop {:style {:overflow "visible"}}}
                                                                                                  ;;    ;:height "45px"
                                                                                                  ;;    :choices [{:id "yo" :label "yo"} {:id "yo1" :label "yo1"}]
                                                                                                  ;;    :model "yo"
                                                                                                  ;;    ;:width "100px"
                                                                                                  ;;    :on-change #()])

                                                                                                  ;; (when (and selected? is-param?)
                                                                                                  ;;   [re-com/box
                                                                                                  ;;    :style {:color fcolor
                                                                                                  ;;            :font-size "11px" :opacity 0.85
                                                                                                  ;;            :padding-left "4px" :margin-top "0px"}
                                                                                                  ;;    :child "clojure data"])


                                                                                                    (when (or flow-running?
                                                                                                              (and (not selected?) query-running? not-view?))
                                                                                                      [re-com/md-icon-button
                                                                                                       :md-icon-name "zmdi-refresh"
                                                                                                       :class "rotate linear infinite"
                                                                                                       :style {:font-size  "20px"
                                                                                                               :transform-origin "10px 11px"
                                                                                                               :padding    "0px"
                                                                                                               :margin-top "-5px"}])

                                                                                                    (when reco-ready?
                                                                                                      [re-com/box
                                                                                                       :style {:margin-top "-6px"}
                                                                                                       :child [re-com/md-icon-button
                                                                                                               :md-icon-name "zmdi-view-dashboard"
                                                                                                               :on-click #(do
                                                                                                                            (clear-preview2-recos)
                                                                                                                            (ut/tracked-dispatch [::select-view brick-vec-key (if (= s base-view-name) nil s)])
                                                                                                                            (reset! mad-libs-view (if (= s @mad-libs-view) nil s))
                                                                                                                            (when (not (= s
                                                                                                                                          ;;@(ut/tracked-subscribe [::conn/clicked-parameter-key [:viz-tables-sys2/table_name]])
                                                                                                                                          @(rfa/sub ::conn/clicked-parameter-key-alpha {:keypath [:viz-tables-sys2/table_name]})))
                                                                                                                              (ut/tracked-dispatch [::conn/click-parameter [:viz-tables-sys2 :table_name] s])))
                                                                                                          ;(when (= s @mad-libs-view)
                                                                                                          ;  (clear-preview2-recos))

                                                                                                               :style {:font-size        "14px"
                                                                                                                       :cursor           "pointer"
                                                                                                                       :color            (if (not (or block-selected?
                                                                                                                                                      viz-reco?
                                                                                                                                                      hover-q? parent-of-selected?
                                                                                                                                                      upstream?
                                                                                                                                                      downstream?)) fcolor bcolor)
                                                                                                                       :height           "17px"
                                                                                                                       :background-color "#00000000" ;bcolor
                                                                                                     ;:opacity 0.5
                                                                                                                       :padding-left     "2px"
                                                                                                                       :padding-right    "2px"}]])]]))))]))
                                                                                                     ;:margin-top "-1px"


                                                   ; (draggable @panel-clone-map ;(get brick-map brick-vec-key)
                                                   ;            "meta-menu"
                                                   ;            ^{:key (str "brick-" brick-vec "-clone-dragger")}
                                                   ;            [re-com/box :child "clone"
                                                   ;             :style {:color "#ffffff90" :font-size "10px"
                                                   ;                     :font-weight 500 :padding-left "3px"
                                                   ;                     ;:margin-top "-2px"
                                                   ;                     ;:padding-left "3px"
                                                   ;                     :padding-right "3px"}
                                                   ;             :height "12px"])

                                                              (cond selected?
                                                                    ^{:key (str "brick-" brick-vec "-resize-handle")}
                                                                    [re-com/box
                                                                     :size "none"
                                                                     :justify :end
                                                                     :align :end
                                                                     :width "18px"
                                                                     :height "18px"
                                                                     :attr (if selected?
                                                                             {:on-mouse-down resize-mouse-down-handler}
                                                                             {:on-click #(ut/tracked-dispatch [::select-block brick-vec-key])})
                                                                     :style {:margin-top    "-2px"
                                                                             :margin-right  "-2px"
                                                                             :position "fixed"
                                                                             :left (- block-width 22) :top (- block-height 18)
                                                                             :border-right  "6px solid #9973e0"
                                                                             :border-bottom "6px solid #9973e0"
                                                                             :cursor        "se-resize"}
                                                                     :child " "]

                                                                    (and (or upstream? parent-of-selected? downstream?) (>= w 5))
                                                                    [re-com/box
                                                                     :style {:font-size     "10px"
                                                                             :color         tab-color
                                                                             :margin-top    "-1px"
                                                                             :padding-right "5px"}
                                                                     :child (cond parent-of-selected? "parent"
                                                                                  downstream? "downstream"
                                                                                  :else "upstream")]

                                                                    :else
                                                                    ^{:key (str "brick-" brick-vec "-resize-handle-gap")}
                                                                    [re-com/gap :size "12px"])]]])
                                        [re-com/gap :size "18px"])]]]))))
                                        )
                                        ]])

                                          ;; (catch :default e
                                          ;;   (do (ut/tapp>> [:GRID-ERROR! (str e) (.-message e) :stack (.-stack e) e])
                                          ;;       (js/console.log (str :GRID-ERROR! (str e) (.-message e) :stack (.-stack e) e)))))
                                        )

                       ;; unreachable - if false, remove later after refactor
                        ;; (droppable ["meta-menu"] brick-vec
                        ;;            ^{:key (str "brick-" brick-vec "-inner")}
                        ;;            [re-com/box
                        ;;             :attr (when @dragging?
                        ;;                     (let [drag-wake @(ut/tracked-subscribe [::build-grid
                        ;;                                                           (+ (last @dragging-size) (last brick-vec) 0)
                        ;;                                                           (+ (first @dragging-size) (first brick-vec) 0)
                        ;;                                                           (last brick-vec)
                        ;;                                                           (first brick-vec)])]
                        ;;                       {:on-drag-enter #(reset! hover-square drag-wake)})
                        ;;                     )
                        ;;             :child
                        ;;             ^{:key (str "brick-" brick-vec "-empty-box")}
                        ;;             [re-com/box
                        ;;              :style {:background-color "#161c2c99"}
                        ;;              :padding "3px"
                        ;;              :size "1"
                        ;;              :width (px brick-size)
                        ;;              :height (px brick-size)
                        ;;              :align :center :justify :center
                        ;;              :child " "]])

                       ;)

;; (defn multi-grid []
;;   (let [bricks-high (+ (js/Math.floor (/ @(ut/tracked-subscribe [::subs/h]) brick-size)) 1)
;;         bricks-wide (+ (js/Math.floor (/ @(ut/tracked-subscribe [::subs/w]) brick-size)) 1)]
;;     ;(ut/tapp>> [bricks-high bricks-wide])
;;     ;; brick-offset-y brick-offset-x
;;     ;[grid bricks-high bricks-wide 0 0]
;;     ;(grid bricks-high bricks-wide 0 0) ;; start-y end-y start-x end-x
;;     [re-com/v-box :size "1" :children [(grid 0 25 0 20)
;;                                        (grid 0 25 20 40)
;;                                        ;(grid 0 51 26 51)
;;                                        ]]
;;     ;(grid 27 25 0 0)
;;     ;(grid 27 26 0 25)
;;     ))
