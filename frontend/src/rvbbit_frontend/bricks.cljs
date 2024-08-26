(ns rvbbit-frontend.bricks
  (:require-macros
   [cljs.core.async.macros :refer [go]])
  (:require
   ["@nivo/bar" :as nivo-bar]
   ["@nivo/calendar" :as nivo-calendar]
   ["@nivo/line" :as nivo-line]
   ["@nivo/pie" :as nivo-pie]
   ["@nivo/scatterplot" :as nivo-scatterplot]
   ["@nivo/swarmplot" :as nivo-swarmplot]
   ["@nivo/treemap" :as nivo-treemap]
   ["@nivo/waffle" :as nivo-waffle]
   ["codemirror/addon/edit/closebrackets.js"]
   ["codemirror/addon/edit/matchbrackets.js"]
   ["codemirror/addon/hint/show-hint.js"]
   ["codemirror/mode/clojure/clojure.js"]
   ["codemirror/mode/shell/shell.js"]
   ["codemirror/mode/julia/julia.js"]
   ["codemirror/mode/markdown/markdown.js"]
   ["codemirror/mode/python/python.js"]
   ["codemirror/mode/r/r.js"]
   ["codemirror/mode/sql/sql.js"]
   ["html2canvas" :as html2canvas]
   ["react-codemirror2" :as cm]
   ["react-drag-and-drop" :as rdnd]
   ["react-map-gl" :default Map
    :refer   [Layer Marker Source]]
   ;[react]
   ;[clojure.data :as cdata]
   [rvbbit-frontend.vbunny    :as vbunny]

   ["ansi-to-html" :as AnsiToHtml]
   [zprint.core       :as zp]

   [cljs.core.async :as    async
    :refer [<! >! chan]]
   [cljs.tools.reader :refer [read-string]]
   [markdown-to-hiccup.core :as m]
   [clojure.edn :as edn]
   [clojure.set :as cset]
   [clojure.string :as cstr]
   [clojure.walk :as walk]
   [day8.re-frame.undo :as    undo
    :refer [undoable]]
   [goog.events :as gevents]
   [goog.i18n.NumberFormat.Format]
   [oz.core :as oz]
    ;[re-catch.core :as rc]
   [re-com.core :as    re-com
    :refer [at]]
   [re-com.util :refer [px px-n]]
   ;[re-frame.alpha :as rfa]
   [re-frame.core :as re-frame]
   [reagent.core :as reagent]
   ;[reagent.ratom :as ratom]
   ;[garden.color :as    color]
   ;[reagent.dom :as rdom]
   [rvbbit-frontend.audio :as audio]
   [rvbbit-frontend.connections :as conn]
   [rvbbit-frontend.db :as db]
   [rvbbit-frontend.http :as http]
   [rvbbit-frontend.layout :as lay]
   [rvbbit-frontend.recharts :as reech]
   [rvbbit-frontend.resolver :as resolver]
   [rvbbit-frontend.scrubbers :as scrub]
   [rvbbit-frontend.select-transform :as st]
   [rvbbit-frontend.shapes :as shape]
   [rvbbit-frontend.subs :as subs]
   [rvbbit-frontend.utility :as ut :refer [tapp>>]]
   [websocket-fx.core :as wfx])
  (:import
   [goog.i18n              NumberFormat]
   [goog.i18n.NumberFormat Format]
   [goog.events            EventType]))



(def dragging? (reagent/atom false))
(def dragging-editor? (reagent/atom false))
(def dragging-block (reagent/atom nil))
(def dragging-size (reagent/atom []))
(def dragging-body (reagent/atom []))
(def dyn-dropper-hover (reagent/atom nil))
(def on-scrubber? (reagent/atom false))
(defonce mad-libs-view (reagent/atom nil))
(defonce mad-libs-top? (reagent/atom true))
(def swap-layers? (reagent/atom false))

(def waiting? (reagent/atom {}))

(defonce  opened-boxes (reagent/atom {}))
(defonce  opened-boxes-long (reagent/atom {}))
(defonce  opened-boxes-code (reagent/atom {}))
(defonce  map-boxes-pages (reagent/atom {}))
(defonce  honeycomb-context-fns-cache (atom {}))

(def map-box-searchers (reagent/atom {}))

;; (defn- render-error-component [{:keys [error info]}]
;;   [:div
;;    {:style {:width           "100%"
;;             :min-width       300
;;             :backgroundColor "rgba(255,0,0,0.2)"
;;             :padding         8}}
;;    [:h6 error]
;;    [:pre info]])

(declare theme-pull)

(defn error-code-box
  [value width-int height-int]
  (let []
    [re-com/box :size "auto"
     ;:width (px (- width-int 24))
     ;:height (px (- height-int 24))
     :align :center :justify :center :style
     {:font-family (theme-pull :theme/monospaced-font nil)
      :font-size   "16px"
      :overflow    "auto"
      :text-shadow "4px 4px 4px #00000099"
      :font-weight 700} :child
     [(reagent/adapt-react-class cm/UnControlled)
      {:value   (str value)
       :options {:mode              "clojure"
                 :lineWrapping      true
                 :lineNumbers       false
                 :matchBrackets     true
                 :autoCloseBrackets true
                 :autofocus         false
                 :autoScroll        false
                 :detach            true
                 :readOnly          true
                 :theme             (theme-pull :theme/codemirror-theme nil)}}]]))

;;(def error-hover-state (reagent/atom false))

(defn- render-error-component
  [{:keys [error info] :as error-map}]
  [re-com/v-box
   :height "100%" :width "100%" :size "auto" :gap "5px" :padding "6px"
   :style
   {:background-image      "url(images/error-skull2.png)"
    :background-size       "auto 100%"
    :background-repeat     "no-repeat"
    :border-radius         "20px"
    :background-color      "#00000099"
    :background-blend-mode "overlay"
    :background-position   "center"}
   :children
   [[re-com/box
     :min-height "20%" :size "auto" :align :center :justify :center
     :style
     {:font-size     "28px"
      :font-weight   700
      :border-radius "20px"
      :font-family   "Share Tech Mono"
      :text-shadow   "0 0 10px #FF0000, 0 0 20px #FF0000, 0 0 30px #FF0000, 0 0 40px #FF0000"
      :color         (theme-pull :theme/editor-outer-rim-color nil)}
     :child (str error)]
    [re-com/box
     :align :center :justify :center
     :style
     {:background-color "#00000045"
      :border-radius    "20px"}
     :padding "6px" :size "auto"
     :child [error-code-box (str info)]]]])

(def ^:dynamic *render-error* render-error-component)

(defn reecatch []
  (let [error-state (reagent/atom nil)
        info-state (reagent/atom nil)
        retry-count (reagent/atom 0)
        retry-timer (atom nil)]
    (reagent/create-class
     {:component-did-catch
      (fn [this error info]
        (reset! error-state (str error))
        (reset! info-state
                (some->> info
                         .-componentStack
                         (cstr/split-lines)
                         (remove cstr/blank?)
                         (drop-while #(re-find #"re_catch|reecatch" %))
                         (take 4)
                         (cstr/join "\n")))
        (reset! retry-timer
                (js/setTimeout
                 #(do
                    (swap! retry-count inc)
                    (reset! error-state nil))
                 5000)))

      :component-will-unmount
      (fn []
        (when-let [timer @retry-timer]
          (js/clearTimeout timer)
          (reset! retry-timer nil)))

      :reagent-render
      (fn [& body]
        (if @error-state
          [*render-error* {:error @error-state :info @info-state}]
          (when-not (empty? (remove nil? body))
            (into [:<>] body))))})))


;; (defn- render-error-component
;;   [{:keys [error info] :as error-map}]
;;   [re-com/v-box :height "100%" :width "100%" :size "auto" :gap "5px" :padding "6px" ;:margin "6px"
;;    :style
;;    {:background-image      "url(images/error-skull2.png)"
;;     :background-size       "auto 100%" ;; "cover"  ;;contain"
;;     :background-repeat     "no-repeat"
;;     :border-radius         "20px"
;;     ;;:margin-right "12px"
;;     ;:filter "brightness(55%)"
;;     :background-color      "#00000099"
;;     :background-blend-mode "overlay"
;;     ;:background-blend-mode "difference" :background-blend-mode "multiply"
;;     :background-position   "center"} :children
;;    [[re-com/box
;;      ;:height "20%" :width "100%"
;;      :min-height "20%" :size "auto" :align :center :justify :center :style
;;      {:font-size     "28px"
;;       :font-weight   700
;;       ;:background-color "#00000055" :background-blend-mode "multiply"
;;       :border-radius "20px"
;;       :font-family   "Share Tech Mono"
;;       :text-shadow   "0 0 10px #FF0000, 0 0 20px #FF0000, 0 0 30px #FF0000, 0 0 40px #FF0000"
;;       :color         (theme-pull :theme/editor-outer-rim-color nil)} :child (str error)]
;;     [re-com/box
;;      ;:max-height "50%" :width "90%"
;;      :align :center :justify :center :style
;;      {:background-color "#00000045"
;;       ;:background-blend-mode "multiply"
;;       :border-radius    "20px"} :padding "6px" :size "auto" :child [error-code-box (str info)]]
;;     ;; [:<>
;;     ;;  [:div {:style {:position "absolute"
;;     ;;                 :top "0" :left "0" :width "100%" :height "100%" :opacity 0.45 :background-image
;;     ;;                 "url(images/error-skull2.png)" :background-size "auto 100%" :background-repeat "no-repeat"
;;     ;;                 :background-blend-mode "darken" :background-position "center"
;;     ;;                 :filter "brightness(55%)"}}]]
;;     ]])

;; (def ^:dynamic *render-error* render-error-component)

;; ;;(def reecatch rc/catch)

;; (defn reecatch
;;   [] ;;; fork of re-catch, will push to own forked credited lib before release
;;   (let [error-state (reagent/atom nil)
;;         info-state  (reagent/atom nil)
;;         retry-timer (reagent/atom nil)
;;         websocket-status          (get @(ut/tracked-sub ::http/websocket-status {}) :status)
;;         online?                   (true? (= websocket-status :connected))
;;         retry-count (reagent/atom 0)] ; Add a retry count
;;     (reagent/create-class
;;      {:component-did-catch    (fn re-catch-block [this error info]
;;                                 (reset! error-state (str error))
;;                                 (reset! info-state (some->> info
;;                                                             .-componentStack
;;                                                             (cstr/split-lines)
;;                                                             (remove cstr/blank?)
;;                                                             (drop-while #(re-find #"re_catch" %))
;;                                                             (drop-while #(re-find #"reecatch" %))
;;                                                             (take 4)
;;                                                             (cstr/join "\n")))
;;                                  ;; Schedule a re-render after 5 seconds
;;                                 (reset! retry-timer (js/setTimeout #(swap! retry-count inc) 5000))) ; Increment the retry
;;                                                                                                      ; count
;;       :component-will-unmount (fn []
;;                                  ;; Clear the retry timer when the component unmounts
;;                                 (when @retry-timer (js/clearTimeout @retry-timer) (reset! retry-timer nil)))
;;       :reagent-render         (fn [& body]
;;                                 (if @error-state
;;                                   (do
;;                                      ;; Reset the error state if the retry count has changed
;;                                     (when (pos? @retry-count) (reset! error-state nil) (reset! retry-count 0))
;;                                     [*render-error* {:error @error-state :info @info-state}])
;;                                   (when-not (->> body
;;                                                  (remove nil?)
;;                                                  empty?)
;;                                     (into [:<>] body))))})))

;; (defn reecatch
;;   []
;;   (let [error-state (reagent/atom nil)
;;         info-state  (reagent/atom nil)
;;         retry-timer (reagent/atom nil)
;;         grace-period-timer (reagent/atom nil)
;;         ;websocket-status (get @(ut/tracked-sub ::http/websocket-status {}) :status)
;;         ;online? (true? (= websocket-status :connected))
;;         retry-count (reagent/atom 0)]
;;     (reagent/create-class
;;      {:component-did-catch (fn re-catch-block [this error info]
;;                               ;; Start the grace period timer
;;                              (reset! grace-period-timer
;;                                      (js/setTimeout #(do
;;                                                        (reset! error-state (str error))
;;                                                        (reset! info-state (some->> info
;;                                                                                    .-componentStack
;;                                                                                    (cstr/split-lines)
;;                                                                                    (remove cstr/blank?)
;;                                                                                    (drop-while (fn [x] (re-find #"re_catch" x)))
;;                                                                                    (drop-while (fn [x] (re-find #"reecatch" x)))
;;                                                                                    (take 14)
;;                                                                                    (cstr/join "\n"))))
;;                                                     5000)) ; 4 seconds grace period
;;                               ;; Schedule a re-render after 3 seconds
;;                              (reset! retry-timer (js/setTimeout #(swap! retry-count inc) 3000))) ; Increment the retry count
;;       :component-will-unmount (fn []
;;                                  ;; Clear the retry timer and the grace period timer when the component unmounts
;;                                 (when @retry-timer (js/clearTimeout @retry-timer) (reset! retry-timer nil))
;;                                 (when @grace-period-timer (js/clearTimeout @grace-period-timer) (reset! grace-period-timer nil)))
;;       :reagent-render (fn [& body]
;;                         (if @error-state
;;                           (do
;;                              ;; Reset the error state if the retry count has changed
;;                             (when (pos? @retry-count) (reset! error-state nil) (reset! retry-count 0))
;;                             [*render-error* {:error @error-state :info @info-state}])
;;                           (when-not (->> body
;;                                          (remove nil?)
;;                                          empty?)
;;                             (into [:<>] body))))


;;       })))


(re-frame/reg-event-fx ::ship-atom
                       (fn [{:keys [db]} [_ atom-name aval]]
                         (ut/tapp>> [:setting :atom-name (get db :client-name) atom-name aval])
                         {:dispatch [::wfx/request :default
                                     {:message {:kind        :client-ui
                                                :atom-name   atom-name
                                                :value       aval ;; (if (false? aval) nil aval)
                                                :client-name (get db :client-name)}
                                      :timeout 500000}]}))


(defn watch-fn [key ref old-state new-state] (when (not= old-state new-state) (ut/tracked-dispatch [::ship-atom key new-state])))

(add-watch dragging? "dragging?" watch-fn)
(add-watch dragging-body "dragging-body" watch-fn)

(defonce drop-last-tracker (reagent/atom {}))
(defonce drop-last-tracker-refs (reagent/atom {}))


(defn tag-screen-position
  [evt] ;; for drag and drop components
  (reset! db/context-modal-pos [(.-clientX evt) (.-clientY evt)]))




(re-frame/reg-sub ::sessions (fn [db _] (get db :sessions)))

(re-frame/reg-event-db ::save-sessions (fn [db [_ res]] (assoc db :sessions res)))

(re-frame/reg-event-db
 ::take-screenshot
 (fn [db [_ & [save?]]]
   (let [element      (js/document.getElementById "base-canvas")
         fs           (vec (for [kk   (get db :flow-subs)
                                 :let [[f1 f2] (ut/splitter (ut/replacer (str kk) ":" "") "/")]]
                             [(keyword f1) (keyword f2)]))
         session-hash (hash [(ut/remove-underscored (get db :panels))
                             (ut/remove-keys (get db :click-param)
                                             (into (map first fs)
                                                   [:flow :time :server :flows-sys :client :solver :signal-history :data :repl-ns
                                                    :solver-meta nil]))])]
     (.then (html2canvas element)
            (fn [canvas]
              (let [dataUrl (.toDataURL canvas "image/jpeg" 0.75)]
                (when (not (nil? dataUrl)) (ut/tracked-dispatch [(if save? ::http/save-screen-snap ::http/save-snap) dataUrl]))))
            (fn [error] (ut/tapp>> ["Error taking screenshot:" error])))
     (when (not save?)
       ;(ut/tracked-dispatch [::update-panels-hash])
       (ut/tracked-dispatch
        [::wfx/request :default
         {:message {:kind :session-snaps :client-name (get db :client-name)} :on-response [::save-sessions] :timeout 15000}]))
     (assoc db :session-hash session-hash))))

(defn mouse-active-recently?
  [seconds]
  (let [now           (js/Date.)
        last-activity @db/last-mouse-activity
        difference    (- now last-activity)
        threshold     (* seconds 1000)] ;; Convert seconds to milliseconds
    (< difference threshold)))

(re-frame/reg-sub
 ::is-mouse-active?
 (fn [db _]
   (let [fs           (vec (for [kk   (get db :flow-subs)
                                 :let [[f1 f2] (ut/splitter (ut/replacer (str kk) ":" "") "/")]]
                             [(keyword f1) (keyword f2)]))
         session-hash (hash [(ut/remove-underscored (get db :panels))
                             (ut/remove-keys (get db :click-param)
                                             (into (map first fs)
                                                   [:flow :time :server :flows-sys :client :solver :repl-ns
                                                    :signal-history :data :solver-meta nil]))])]
     (and (not= session-hash (get db :session-hash)) (not (true? (mouse-active-recently? 5)))))))




(re-frame/reg-sub
 ::materialized-theme
 (fn [db _]
   (let [tt (get-in db [:click-param :theme])
         tt (ut/postwalk-replacer {:text :_text} tt)
         tt (resolver/logic-and-params tt nil)]
     (ut/tapp>> [:materialized-theme (get db :client-name) (ut/postwalk-replacer {:_text :text} tt)]))
   nil))

;; (tapp>> [:materialized-theme (str @(ut/tracked-sub ::materialized-theme {}))])



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






(defn code-mirror-theme
  [] ;; generate a dynamic theme to inject into the DOM
  (let [ccolor (fn [x] (get (theme-pull :theme/data-colors db/data-colors) x))]
    {;; Editor styles
     ".cm-s-rvbbit-dynamic.CodeMirror" {:background "inherit" :color "#b3b1ad" :font-size "inherit" :line-height "inherit"}
     ;;".cm-s-rvbbit-dynamic .CodeMirror-selected" {:background (theme-pull :theme/editor-outer-rim-color nil)} ;; "#ff4aff"
     ".cm-s-rvbbit-dynamic .CodeMirror-selected" {:color (theme-pull :theme/editor-outer-rim-color nil)
                                                  :background-color (theme-pull :theme/universal-pop-color nil)} ;; "#ff4aff"
     ".cm-s-rvbbit-dynamic .CodeMirror-line::selection, .cm-s-rvbbit-dynamic .CodeMirror-line > span::selection, .cm-s-rvbbit-dynamic .CodeMirror-line > span > span::selection"
     {:background "#27374763"} ;; {:background "rgba(39, 55, 71, 99)"}
     ".cm-s-rvbbit-dynamic .CodeMirror-line::-moz-selection, .cm-s-rvbbit-dynamic .CodeMirror-line > span::-moz-selection, .cm-s-rvbbit-dynamic .CodeMirror-line > span > span::-moz-selection"
     {:background "#27374763"} ;; {:background "rgba(39, 55, 71, 99)"}
     ".cm-s-rvbbit-dynamic .CodeMirror-gutters" {:background "inherit" :border-right "0px" :z-index "-1" :opacity "0.9"}
     ".cm-s-rvbbit-dynamic .CodeMirror-guttermarker" {:color "white" :z-index "-1" :opacity "0.9"}
     ".cm-s-rvbbit-dynamic .CodeMirror-guttermarker-subtle" {:color "#3d424d"}
     ".cm-s-rvbbit-dynamic .CodeMirror-linenumber" {:color "#3d424d" :z-index "-1" :opacity "0.9"}
     ".cm-s-rvbbit-dynamic .CodeMirror-cursor"
     {:border-left (str "2px solid " (ut/invert-hex-color (theme-pull :theme/editor-outer-rim-color nil)))} ;; {:border-left
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
     ".cm-s-rvbbit-dynamic span.cm-tag" {:color "#39bae650"} ;; #39bae650 ? was "rgba(57, 186,
     ".cm-s-rvbbit-dynamic span.cm-header" {:color "#c2d94c"}
     ".cm-s-rvbbit-dynamic span.cm-link" {:color "#39bae6"}
     ".cm-s-rvbbit-dynamic span.cm-error" {:color "#ff3333"}
     ".cm-s-rvbbit-dynamic .CodeMirror-activeline-background" {:background "#01060e"}
     ".cm-s-rvbbit-dynamic .CodeMirror-matchingbracket"
     {:text-decoration "underline" :color (str (theme-pull :theme/editor-outer-rim-color nil) " !important")} ;; "#ff4aff
     ".cm-s-rvbbit-dynamic ::-webkit-scrollbar-corner" {:background        "rgba(0,0,0,0) !important"
                                                        "background-color" "rgba(0,0,0,0) !important"}
     "div.CodeMirror span.CodeMirror-matchingbracket" {:color       (str (theme-pull :theme/editor-outer-rim-color nil)
                                                                         " !important") ;; "#ff4aff
                                                       :font-weight "bold"}
     ".CodeMirror-hints" {:position    "absolute"
                          :z-index     "1000000"
                          :overflow    "hidden"
                          :list-style  "none"
                          :margin      "0"
                          :padding     "2px"
                          :box-shadow  "2px 3px 5px rgba(0, 0, 0, 0.2)"
                          :background  "white"
                          :font-family (theme-pull :theme/monospaced-font nil) ;;"monospace"
                          :overflow-y  "auto"}
     ".CodeMirror-hint" {:padding "0 4px" :white-space "pre" :overflow "hidden" :cursor "pointer"}
     ".CodeMirror-hint-active" {:background-color (theme-pull :theme/editor-outer-rim-color nil)
                                :color            (ut/invert-hex-color (theme-pull :theme/editor-outer-rim-color nil))}}))


;; (def nff (NumberFormat. Format/DECIMAL))

;; (defn nf [num] (.format nff (str num)))

(def nff (js/Intl.NumberFormat. "en-US" #js {:minimumFractionDigits 0 :maximumFractionDigits 2}))

(defn nf [num]
  (.format nff num))

(defn num-format-int [num] (.format (NumberFormat. Format/INTEGER) (str num)))

(re-frame/reg-event-db ::shift-down (fn [db [_]] (if (not (get db :shift?)) (assoc db :shift? true) db)))
(re-frame/reg-event-db ::shift-up (fn [db [_]] (assoc db :shift? true)))
(re-frame/reg-sub ::shift? (fn [db [_]] (get db :shift? false)))

(defn flatten-values
  [data]
  (cond (map? data)    (mapcat flatten-values (vals data))
        (vector? data) (mapcat flatten-values data)
        :else          [data]))

(defn get-all-values [data] (vec (flatten-values data)))

(defonce get-all-values-flatten (atom {}))

(defn cached-get-all-values [data]
  (if-let [cached-result (@get-all-values-flatten (hash data))]
    cached-result
    (let [result (get-all-values data)]
      (swap! get-all-values-flatten assoc (hash data) result)
      result)))


;; (defn get-grid [obody]
;;   (let [kps       (ut/extract-patterns obody :grid 2)
;;         logic-kps (into [] (for [v kps] (let [[_ tab-name] v] tab-name)))]
;;     logic-kps))

;; (defn only-relevant-tabs [panels-map selected-tab]
;;   (let [curr-tab (into {}
;;                        (for [[k v] panels-map
;;                              :when (= (get v :tab) selected-tab)]
;;                          {k v}))
;;         grids (get-grid (ut/deep-flatten curr-tab)) ;; returns a vec of tab names if found
;;         ]

;;     ))

;; (defn get-grid [obody]
;;   (let [kps       (ut/extract-patterns obody :grid 2)
;;         logic-kps (into [] (for [v kps] (let [[_ tab-name] v] tab-name)))]
;;     logic-kps))

;; (defn- recursive-panel-filter [panels-map selected-tab & [processed-tabs]]
;;   (let [processed-tabs (conj (or processed-tabs #{}) selected-tab)
;;         curr-tab-panels (into {}
;;                               (for [[k v] panels-map
;;                                     :when (= (get v :tab) selected-tab)]
;;                                 {k v}))
;;         grids (get-grid curr-tab-panels)
;;         ;;_ (tapp>> [:grids grids])
;;         relevant-tabs (remove #(processed-tabs %) grids)]
;;     (reduce (fn [acc tab]
;;               (merge acc (recursive-panel-filter panels-map tab processed-tabs)))
;;             curr-tab-panels
;;             relevant-tabs)))

;; (defn only-relevant-tabs [panels-map selected-tab]
;;   (recursive-panel-filter panels-map selected-tab))

(defn get-grid [obody]
  (let [kps       (ut/extract-patterns obody :grid 2)
        logic-kps (into [] (for [v kps] (let [[_ tab-name] v] tab-name)))]
    logic-kps))

(defn recursive-panel-filter [panels-map selected-tab & [processed-tabs]]
  (let [processed-tabs (conj (or processed-tabs #{}) selected-tab)
        curr-tab-panels (into {}
                              (for [[k v] panels-map
                                    :when (= (get v :tab) selected-tab)]
                                {k v}))
        grids (get-grid curr-tab-panels)
        relevant-tabs (remove #(processed-tabs %) grids)]
    (reduce (fn [acc tab]
              (merge acc (recursive-panel-filter panels-map tab processed-tabs)))
            curr-tab-panels
            relevant-tabs)))

(defn only-relevant-tabs [panels-map selected-tab]
  (recursive-panel-filter panels-map selected-tab))

(defonce temp-extra-subs (atom [])) ;; flushed on refresh, still better than subbing for lots of stuff that wont likely ever need it.





(re-frame/reg-sub
 ::get-flow-subs
 (fn [db {:keys [all?]}]
   (let [client-name             @(ut/tracked-sub ::client-name {})
         client-name-str         (cstr/replace (str client-name) ":" "")
         base-subs               [(keyword (str "kit/kicks>" client-name-str)) ;; easy way to determine kick data per card slice
                                  ]
         create-runner-listeners (fn [obody]
                                   (let [kps       (ut/extract-patterns obody :run-flow 2)
                                         logic-kps (into {}
                                                         (for [v kps]
                                                           (let [[_ that] v]
                                                             {v (keyword (str "flow-status/" (cstr/replace (str (first that)) ":" "") ">*running?"))})))]
                                     (ut/postwalk-replacer logic-kps obody)))
         create-solver-listeners (fn [obody]
                                   (let [kps       (ut/extract-patterns obody :click-solver 2)
                                         logic-kps (into {}
                                                         (for [v kps]
                                                           (let [[_ that] v]
                                                             {v (keyword (str "solver-status/" client-name-str ">" (cstr/replace (str (first that)) ":" "") ">running?"))})))]
                                     (ut/postwalk-replacer logic-kps obody)))
         selected-block          (get db :selected-block)
         selected-view           @(ut/tracked-sub ::editor-panel-selected-view {})
         kp             (vec (flatten [selected-block selected-view]))
         mode           (get @db/chat-mode kp (if (= "none!" (first kp)) :runstreams :history))
         runstream-refs          (vec (distinct (filter #(cstr/starts-with? (str %) ":flow/")
                                                        (ut/deep-flatten (get db :runstreams)))))
         runstreams              (vec (remove nil? (flatten (for [{:keys [flow-id]} @(ut/tracked-sub ::runstreams {})]
                                                              [(keyword (str "flow-status/" flow-id ">*running?")) ;; always sub to flow-status for runstreams
                                                               (when (and (get db :buffy?) ;; only if panel is open, sub to runstream samples also 
                                                                          (= mode :runstreams))
                                                                 (keyword (str "runstream/" flow-id)))]))))

        ;;  client-namespaces-refs  [:repl-ns/repl-client-namespaces-map>*client-name*] ;; use client-name-str instead
        ;;  client-namespace-intros (vec (for [nms @(ut/tracked-sub ::conn/clicked-parameter-key-alpha {:keypath client-namespaces-refs})]
        ;;                                 (keyword (str "repl-ns/" nms ">introspected"))))

         editor?                 (get db :editor?)
         doom?                   (get db :doom-modal?)
         flow?                   (get db :flow?)
         selected-tab            (get db :selected-tab)
         panels-map              (get db :panels)
         ;;all? true 
         chunk-charts            (if (and flow? (= (first @db/flow-editor-system-mode) "flows running"))
                                   db/chunk-charts [])
         panels-map              (if all? panels-map
                                     (merge
                                      (only-relevant-tabs panels-map selected-tab)
                                      (when editor? (select-keys (get db :panels) [selected-block]))))
         panels                  [(create-runner-listeners panels-map)
                                  (create-solver-listeners panels-map)]
         dfp                     (ut/deep-flatten panels)
         client-ns-intro-map     (when (dfp :introspect!) [(keyword (str "repl-ns/" client-name-str))])  ;; if we have any introspected runners, then only sub to introspect-map
         drop-refs               (vec (distinct (vals @drop-last-tracker-refs)))
         sflow                   (get db :selected-flow)
         current-flow-open       (when (ut/ne? sflow) (keyword (str "flow-status/" sflow ">*running?")))
        ;;  flow-refs               (vec (distinct (conj (filter #(or (cstr/starts-with? (str %) ":flow/")
        ;;                                                            (cstr/starts-with? (str %) ":screen/")
        ;;                                                            (cstr/starts-with? (str %) ":time/")
        ;;                                                            (cstr/starts-with? (str %) ":signal/")
        ;;                                                            (cstr/starts-with? (str %) ":server/")
        ;;                                                            (cstr/starts-with? (str %) ":ext-param/")
        ;;                                                            (cstr/starts-with? (str %) ":solver/")
        ;;                                                            (cstr/starts-with? (str %) ":solver-meta/")
        ;;                                                            (cstr/starts-with? (str %) ":repl-ns/")
        ;;                                                            (cstr/starts-with? (str %) ":solver-status/")
        ;;                                                            (cstr/starts-with? (str %) ":flow-status/")
        ;;                                                            (cstr/starts-with? (str %) ":runstream/")
        ;;                                                            (cstr/starts-with? (str %) ":kit-status/")
        ;;                                                            (cstr/starts-with? (str %) ":kit/")
        ;;                                                            (cstr/starts-with? (str %) ":data/")
        ;;                                                            (cstr/starts-with? (str %) ":signal-history/")
        ;;                                                            (cstr/starts-with? (str %) ":panel/")
        ;;                                                            (cstr/starts-with? (str %) ":client/"))
        ;;                                                       (filter keyword?
        ;;                                                               (into dfp
        ;;                                                                     (ut/deep-flatten (cached-get-all-values (get db :click-param))))))
        ;;                                               current-flow-open)))
         reactor-type-prefixes (set (map #(str % "/") db/reactor-types))
         flow-refs (vec (distinct
                         (conj
                          (filter
                           (fn [item]
                             (let [item-str (str item)]
                               (some #(cstr/starts-with? item-str %) reactor-type-prefixes)))
                           (filter keyword?
                                   (into dfp
                                         (ut/deep-flatten (cached-get-all-values (get db :click-param))))))
                          current-flow-open)))
         ;;_ (tapp>> [:click-param-subs? (ut/deep-flatten (get-all-values (get db :click-param)))])
          ;; flow-runners            (when (and (get db :flow?) 
          ;;                                    (ut/ne? sflow)) 
          ;;                           (vec (for [e (keys (get-in db [:flows sflow :map]))] (keyword (str "flow/" sflow ">" e)))))
         flow-runners             []
          ;;_ (tapp>> [:flow-runners flow-runners])
         clover-solvers-all       (vec (apply concat (for [[_ v] ;; all things
                                                           @db/solver-fn-runs] (keys v))))
         clover-solvers-tab       (vec (apply concat (for [[k v] @db/solver-fn-runs
                                                           :when (if (cstr/starts-with? (str k) ":block")
                                                                   (some #(= % k) (keys panels-map)) true)] (keys v))))
         clover-solvers           (if all? clover-solvers-all clover-solvers-tab)
         ;;_ (tapp>> [:clover-solvers clover-solvers  @db/solver-fn-lookup @db/solver-fn-runs])
         clover-solver-outputs    (vec (for [[kk kp] (select-keys (ut/flip-map @db/solver-fn-lookup) clover-solvers)
                                             :let [[p bid vid] kp
                                                   out-type @(ut/tracked-sub ::repl-output-type {:panel-key bid :view-name vid})]
                                             :when (and (= p :panels)
                                                        (or
                                                         (= out-type :output)
                                                         (= out-type :output-live)))]
                                         (if (= out-type :output)
                                           (keyword (str (cstr/replace (str kk) ":solver/" "solver-meta/") ">output>evald-result>out"))
                                           (keyword (str (cstr/replace (str kk) ":solver/" "solver-meta/") ">incremental")))))
         ;_ (tapp>> [:output-solvers clover-solvers  @db/solver-fn-lookup])
         warren-item             (get db :selected-warren-item)
         solver-open?            (and (= (get @db/flow-editor-system-mode 0) "signals & solvers") (get db :flow?))
         solver                  (if (and (get-in db [:solvers-map warren-item :data]) solver-open?)
                                   [(keyword (str "solver/" (ut/replacer (str warren-item) ":" "")))
                                    (keyword (str "solver-meta/" (ut/replacer (str warren-item) ":" "") ">extra"))
                                    (keyword (str "solver-meta/" (ut/replacer (str warren-item) ":" "") ">output"))
                                    (keyword (str "solver-meta/" (ut/replacer (str warren-item) ":" "") ">error"))
                                    (keyword (str "solver-status/" client-name-str ">" (ut/replacer (str warren-item) ":" "") ">running?"))
                                    (keyword (str "solver-meta/" (ut/replacer (str warren-item) ":" "") ">incremental"))
                                    (keyword (str "solver-meta/" (ut/replacer (str warren-item) ":" "") ">history"))]
                                   [])
         ss-filter-results       (fn [x y]
                                   (if (or (nil? x) (empty? x))
                                     y
                                     (if (map? y)
                                       (into {}
                                             (filter (fn [[k v]]
                                                       (or (cstr/includes? (cstr/lower-case (str k)) (cstr/lower-case x))
                                                           (cstr/includes? (cstr/lower-case (str v)) (cstr/lower-case x))))
                                                     y))
                                       (vec (filter #(cstr/includes? (cstr/lower-case (str %)) (cstr/lower-case x)) y)))))
         all-solvers             (ss-filter-results @db/signals-searcher-atom (get db :solvers-map))
         all-signals             (ss-filter-results @db/signals-searcher-atom (get db :signals-map))
         solvers                 (if (and solver-open? (some #(= % "solvers") @db/selectors-open))
                                   (vec (for [ss (keys all-solvers)]
                                          (keyword (str "solver-meta/" (ut/replacer (str ss) ":" "") ">extra"))))
                                   [])
         signals-mode?           (and (= (get @db/flow-editor-system-mode 0) "signals & solvers") ;; signals
                                      (get db :flow?))
         signals-box-open?       (some #(= % "signals") @db/selectors-open)
         signal-ui-refs          (when signals-box-open?
                                   (vec (for [ss (keys all-signals)]
                                          (keyword (str "signal/" (ut/replacer (str ss) ":" ""))))))
         signal-ui-part-refs     (when (and warren-item signals-mode?)
                                   (let [pps (ut/where-dissect (get-in db [:signals-map warren-item :signal]))]
                                     (vec (for [pp pps]
                                            (keyword (str "signal/part-" (ut/replacer (str warren-item) ":" "")
                                                          "-"            (.indexOf pps pp)))))))
         signal-hist             (if (and (get-in db [:signals-map warren-item :signal])
                                          @db/signal-history-ticker? ;; unless the ticker is
                                          signals-mode?)
                                   [(keyword (str "signal-history/" (ut/replacer (str warren-item) ":" "")))]
                                   [])

         in-editor-solvers0      (vec (map last (filter (fn [[k _]] (cstr/includes? (str k) (str selected-block))) @db/solver-fn-lookup)))
         in-editor-solvers       (if editor?
                                   (let []
                                     (vec (distinct
                                           (remove nil?
                                                   (flatten (for [s in-editor-solvers0
                                                                  :let [meta-kw (str (ut/replacer s ":solver/" "solver-meta/"))
                                                                        ns-key (keyword meta-kw)
                                                                        out-type @(ut/tracked-sub ::repl-output-type {:panel-key selected-block :view-name selected-view})
                                                                        ;;console-key (keyword (str meta-kw ">output>evald-result>out"))
                                                                        console-key (if (= out-type :output-live)
                                                                                      (keyword (str meta-kw ">incremental"))
                                                                                      (keyword (str meta-kw ">output>evald-result>out")))
                                                                        ;;ns-key1 @(ut/tracked-sub ::conn/clicked-parameter-key-alpha {:keypath [ns-key]})
                                                                        ]]
                                                              [(keyword (str (ut/replacer s ":solver/" (str "solver-status/" client-name-str ">"))))

                                                              ;;  (when (and (ut/ne? ns-key1) (get-in ns-key1 [:output :evald-result :ns]))
                                                              ;;    (keyword (str "repl-ns/" (get-in ns-key1 [:output :evald-result :ns]))))

                                                               console-key

                                                               ns-key])))))) [])
         ;;;_ (tapp>> [:in-editor-solvers  (str  in-editor-solvers) ])
         clover-solvers-running  (vec (apply concat (for [s clover-solvers] ;;(vec (remove (set in-editor-solvers0) clover-solvers))] 
                                                      [;(keyword (str
                                         ;         (-> s
                                         ;             (ut/replacer ":solver/" "solver-status/*client-name*>")
                                         ;             (ut/replacer ":" ""))
                                         ;         (str ">running?")))
                                                       (keyword (str
                                                                 (-> s
                                                                     (ut/replacer ":solver/" (str "solver-status/" (cstr/replace (str client-name) ":" "") ">"))
                                                                     (ut/replacer ":" ""))
                                                                 (str ">running?")))])))
          ;; clover-solvers-running  []
         _ (reset! temp-extra-subs  (vec (distinct @temp-extra-subs)))
        ;; _ (tapp>> [:temp-extra-subs @temp-extra-subs])
         signal-subs             (if signals-mode? (vec (into signal-hist (into signal-ui-refs signal-ui-part-refs))) [])
         theme-refs              (vec (distinct (filter #(cstr/starts-with? (str %) ":flow/")
                                                        (filter keyword? (ut/deep-flatten (get-in db [:click-param :theme]))))))
         subs-vec (vec (distinct
                        (remove nil? (concat flow-runners clover-solver-outputs chunk-charts @temp-extra-subs
                                (when doom? [:time/minute]) base-subs 
                                     ;;client-namespaces-refs 
                                     ;;client-namespace-intros 
                                client-ns-intro-map
                                signal-subs clover-solvers
                                clover-solvers-running in-editor-solvers solver solvers drop-refs runstream-refs runstreams theme-refs flow-refs))))]
     subs-vec
      ;; (tapp>> [:clover-solvers
      ;;          @db/solver-fn-lookup
      ;;          (str in-editor-solvers0) (str in-editor-solvers )
      ;;          ;clover-solvers clover-solvers-running
      ;;          ])
      ;(filterv #(not (cstr/includes? (str %) "||")) ;; live running subs ignore

      ;         )
     )))


(re-frame/reg-sub
 ::stale-flow-subs?
 (fn [db _]
   (let [flow-open (get db :selected-flow)
         flow-open? (and (ut/ne? flow-open) (not= flow-open "live-scratch-flow") (get db :flow?))
                          ;;new   @(ut/tracked-sub ::get-flow-subs {})
         new   @(ut/tracked-sub ::get-flow-subs {})
         old   (get db :flow-subs [])
         deads (vec (cset/difference (set old) (set new)))
         deads (filterv #(not (cstr/includes? (str %) "||")) deads)
         stale? (if flow-open?
                  false
                  (ut/ne? deads))]
                      ;;(tapp>>  [:stale? stale? flow-open?])
     stale?)))

(re-frame/reg-event-db
 ::unsub-to-flows
 (fn [db _]
   (let [new   @(ut/tracked-sub ::get-flow-subs {})
         old   (get db :flow-subs [])
         flow-open? (get db :flow?)
         old   (filterv #(and
                          (if flow-open? (not (cstr/includes? (str %) (str (get db :selected-flow)))) true)
                          (not (cstr/includes? (str %) "||"))) old)
         deads (vec (cset/difference (set old) (set new)))
         deads (filterv #(not (cstr/includes? (str %) "||")) deads) ;; dont mess with
         all-sub-flows @(ut/tracked-sub ::get-flow-subs {:all? true})
         _ (ut/tapp>> [:flow-unsub-change!  flow-open? (get db :client-name)
                       {:old old :new new :removing deads
                        ;;:unsubbing-but-keeping-inactive (cset/intersection (set deads) (set all-sub-flows))
                        }])]
     (doall (doseq [n deads]
              (ut/tracked-dispatch [::http/unsub-to-flow-value n])
              (when (not (some #(= n %) all-sub-flows)) ;; in other tabs, we dont remove the data, just temp unsub
                (ut/tracked-dispatch [::conn/declick-parameter
                                      (vec (map keyword (ut/splitter (ut/replacer (str n) ":" "") #"/")))]))))
     (assoc db :flow-subs new))))

(re-frame/reg-sub
 ::get-new-flow-subs
 (fn [db _]
   (let [;;flow-refs @(ut/tracked-subscribe [::get-flow-subs])
         flow-refs @(ut/tracked-sub ::get-flow-subs {})
         subbed    (get db :flow-subs [])
         subbed    (filterv #(not (cstr/includes? (str %) "||")) subbed)] ;; dont mess with
     (vec (cset/difference (set flow-refs) (set subbed))))))

(re-frame/reg-sub 
 ::new-flow-subs? 
 (fn [_ _] 
   (ut/ne? @(ut/tracked-sub ::get-new-flow-subs {}))))

(re-frame/reg-sub 
 ::alerts 
 (fn [db _] (get db :alerts)))

(re-frame/reg-event-db
 ::sub-to-flows
 (fn [db _]
   (let [;;new @(ut/tracked-subscribe [::get-new-flow-subs])
         new    @(ut/tracked-sub ::get-new-flow-subs {})
         old    (get db :flow-subs [])
         old    (filterv #(not (cstr/includes? (str %) "||")) old)
         subbed (vec (for [n new] (do (ut/tracked-dispatch [::http/sub-to-flow-value n]) n)))]
     (assoc db :flow-subs (into subbed old)))))

(re-frame/reg-event-db
 ::sub-to-flows-all ;; temp debugging
 (fn [db _]
   (let [;new @(ut/tracked-subscribe [::get-new-flow-subs])
         all (get db :flow-subs [])]
     (doseq [n all] (ut/tracked-dispatch [::http/sub-to-flow-value n]))
     db)))

(re-frame/reg-sub
 ::runstream-running?
 (fn [db _]
   (let [selected-block @(ut/tracked-sub ::selected-block {})
         selected-view  @(ut/tracked-sub ::editor-panel-selected-view {})
         kp             (vec (flatten [selected-block selected-view]))
         mode           (get @db/chat-mode kp (if (= "none!" (first kp)) :runstreams :history))]
     (and (get db :buffy?)
          (= mode :runstreams)
          ;(true? (some true? ;; remove running for now, req is cheap enough since sampled.
          ;             (for [[k v] (get-in db [:click-param :flow-status] {}) 
          ;                   :when (cstr/ends-with? (str k) ">*running?")] v)))
          ))))

(re-frame/reg-sub
 ::runstreams-running
 (fn [db _]
   (count (filter true?
                  (for [[k v] (get-in db [:click-param :flow-status] {}) :when (cstr/ends-with? (str k) ">*running?")] v)))))

(re-frame/reg-sub 
 ::runstreams-running-list
 (fn [db _]
   (for [[k v] (get-in db [:click-param :flow-status] {})
         :when (and (true? v) (cstr/ends-with? (str k) ">*running?"))]
     (keyword (-> (str k)
                  (ut/replacer ">*running?" "")
                  (ut/replacer ":" ""))))))

(re-frame/reg-event-db
 ::runstream-item
 (fn [db [_ result]]
   (-> db
       (assoc-in [:runstreams-lookups (get result :flow-id) :open-inputs] (get result :open-inputs))
       (assoc-in [:runstreams-lookups (get result :flow-id) :blocks] (get result :blocks)))))

(re-frame/reg-event-db
 ::refresh-runstreams
 (fn [db _]
   ;(tapp>> [:refresh-runstreams?])
   (when (ut/ne? (keys (get db :runstreams)))
     (doseq [flow-id (keys (get db :runstreams))]
       (ut/tracked-dispatch
        [::wfx/request :default
         {:message     {:kind :get-flow-open-ports :flow-id flow-id :flowmap flow-id :client-name (get db :client-name)}
          :on-response [::runstream-item]
          :timeout     500000}])))
   db))

(re-frame/reg-sub ::workspace (fn [db [_ keypath]] (let [merged-keypath (cons :panels keypath)] (get-in db merged-keypath))))

(re-frame/reg-sub ::workspace-alpha
                  (fn [db {:keys [keypath]}] (let [merged-keypath (cons :panels keypath)] (get-in db merged-keypath))))

(re-frame/reg-sub ::what-tab (fn [db [_ panel-key]] (get-in db [:panels panel-key :tab] "")))

(re-frame/reg-sub ::reco-running? (fn [db [_ query-key task-id]] (= :started (get-in db [:status task-id query-key]))))

(re-frame/reg-sub ::reco-queued? (fn [db [_ query-key task-id]] (= :queued (get-in db [:status task-id query-key]))))

(re-frame/reg-event-db ::set-reco-queued (fn [db [_ query-key task-id]] (assoc-in db [:status task-id query-key] :queued)))

(re-frame/reg-sub ::reco-count (fn [db [_ query-key task-id]] (get-in db [:status-data task-id query-key :reco-count] nil)))


(re-frame/reg-sub ::is-layout?
                  (fn [db [_ panel-key view-key]]
                    (true? (some #(= % :layout) (ut/deep-flatten (get-in db [:panels panel-key :views view-key] []))))))

(re-frame/reg-sub ::is-layout-selected-and-scrubber? ;; editor
                  (fn [db _]
                    (let [panel-key    (get db :selected-block)
                          view-key     (get @db/data-browser-query panel-key)
                          scrubber-on? true] ; (get-in @db/scrubbers [panel-key view-key] false)
                      (and (true? scrubber-on?)
                           (true? (some #(= % :layout) (ut/deep-flatten (get-in db [:panels panel-key :views view-key] []))))))))

(re-frame/reg-sub ::snapshots (fn [db _] (get-in db [:snapshots :params] {})))




(declare sql-alias-replace)


(defn sql-alias-replace-sub [query] @(ut/tracked-sub ::sql-alias-replace-sub {:query query}))

(re-frame.core/reg-event-fx
 ::save
 (fn [{:keys [db]} _]
   (let [flow-id              @(ut/tracked-subscribe [:rvbbit-frontend.flows/selected-flow])
         flowmaps             @(ut/tracked-subscribe [:rvbbit-frontend.flows/flowmap-raw])
         flowmaps-connections @(ut/tracked-subscribe [:rvbbit-frontend.flows/flowmap-connections])
         screen-name          (get db :screen-name)
         resolved-queries     (into {}
                                    (for [[qk q] (apply merge (for [[_ p] (get db :panels)] (get p :queries)))]
                                      ;{qk (sql-alias-replace-sub q)}
                                      {qk (sql-alias-replace q)}))
         flow?                (get db :flow? false)
         ttype                (if flow? :flow :screen)]
     (ut/tapp>> [:saving ttype])
     (ut/tracked-dispatch [::http/insert-alert
                           [:box :child (str "sent " (ut/replacer (str ttype) ":" "") " save request to server... Please wait.")
                            :style {:font-size "12px" :opacity 0.66}] 11 0.5 6])
     (if flow?
       {;;:db db ;; if ommitted, it is IMPLIED that we are not modding the app-db at all (but
        :dispatch       [::http/save-flow
                         {:flowmaps             flowmaps
                          :zoom                 @db/pan-zoom-offsets
                          :opts                 (get-in db
                                                        [:flows (get db :selected-flow) :opts]
                                                        {:retry-on-error? false :retries 5 :close-on-done? true})
                          :flow-id              flow-id
                          :flowmaps-connections flowmaps-connections} flow-id]
        :dispatch-later [{:ms 3000 :dispatch [::conn/clear-query-history :flows-sys]}]}
       {:dispatch       [::http/save :skinny screen-name resolved-queries]
        :dispatch-later [{:ms 100 :dispatch [::take-screenshot true]}]}))))

(re-frame/reg-sub
 ::current-params ;; for snapshot comparisons
 (fn [db _]
   (let [panels            (get db :panels)
         selected-tab      (get db :selected-tab)
         block-states      (vec (map #(vec (cons :panels %))
                                     (filter #(or (= :w (last %))
                                                  (= :h (last %))
                                                  (= :z (last %))
                                                  (= :hidden? (last %))
                                                  (= :ghosted? (last %))
                                                  (= :root (last %))
                                                  (= :selected-view (last %)))
                                             (ut/kvpaths panels))))
         block-states-vals (into {} (for [b block-states] {b (get-in db b)}))]
     {:params (into {} (reduce dissoc
                               (get db :click-param {}) 
                               db/reactor-types))
      :block-states block-states-vals
      :selected-tab selected-tab})))

(re-frame/reg-event-db
 ::swap-snapshot
 (undoable)
 (fn [db [_ key]]
   (let [reactor-params (select-keys (get db :click-param {}) db/reactor-types)
         new-params     (merge reactor-params (get-in db [:snapshots :params key :params]))
         block-states   (get-in db [:snapshots :params key :block-states])
         curr-tab       (get-in db [:snapshots :params key :selected-tab])
         extra?         (get-in db [:snapshots :params key :extra?])]
     (if extra? ;; apply saved block states also from UI bool toggle
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
         curr  @(ut/tracked-sub ::current-params {})]
     (for [[k v] snaps
           :let  [block-states     (get v :block-states)
                  valid-state-keys (vec (cset/intersection ;; since we are diffing, we only
                                         (set (keys block-states)) ;; otherwise we get a diff
                                         (set (keys (get curr :block-states)))))] ;; which are
           :when (= (assoc (select-keys v [:params]) :block-states (select-keys (get v :block-states) valid-state-keys))
                    (dissoc (assoc curr :block-states (select-keys (get curr :block-states) valid-state-keys)) :selected-tab))]
       k))))

(re-frame/reg-sub ::current-tab-queries
                  (fn [db _]
                    (let [blocks (try (map name
                                           (keys (into {}
                                                       (for [[_ v] (into {}
                                                                         (filter #(= (get db :selected-tab) (get (val %) :tab ""))
                                                                                 (get db :panels)))]
                                                         (get v :queries)))))
                                      (catch :default _ ["error current tab queries"]))]
                      (or blocks []))))

(re-frame/reg-sub ::current-tab-condis
                  (fn [db _]
                    (let [blocks (try (map name
                                           (keys (into {}
                                                       (for [[_ v] (into {}
                                                                         (filter #(= (get db :selected-tab) (get (val %) :tab ""))
                                                                                 (get db :panels)))]
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
 ::current-tab-slices
 (fn [db _]
   (let [blocks (try (keys (into {} (filter #(= (get db :selected-tab) (get (val %) :tab "")) (get db :panels))))
                     (catch :default _ []))
         runners-map (vec (keys (get-in db [:server :settings :runners] {})))]
     (vec (map name (distinct (remove nil? (flatten (apply concat (for [b blocks] (for [r runners-map] (keys (get-in db [:panels b r]))))))))))
     ;(for [b blocks] (for [r runners-map] (get-in db [:panels b r])))
     )))

(re-frame/reg-sub 
 ::same-tab?
 (fn [db [_ panel-key1 panel-key2]]
   (= (get-in db [:panels panel-key1 :tab] "") (get-in db [:panels panel-key2 :tab] ""))))

(defn partition-keywords [keywords]
  (let [str-keywords (map str keywords)]
    (group-by #(or (cstr/includes? % "flow-runner")
                   (cstr/includes? % "tracker")) str-keywords)))

(re-frame/reg-sub 
 ::partitioned-subs ;; in order to abuse re-frame subscription caching
 (fn [db _] (partition-keywords (get db :flow-subs []))))

(re-frame/reg-sub 
 ::flow-watcher-subs :<- [::partitioned-subs] (fn [subs _] (get subs true [])))

(re-frame/reg-sub 
 ::server-subs :<- [::partitioned-subs] (fn [subs _] (get subs false [])))


(re-frame/reg-sub
 ::all-server-subs
 (fn [db _] (get db :flow-subs [])))

(re-frame/reg-sub
 ::flow-watcher-subs-grouped
 :<-
 [::flow-watcher-subs]
 (fn [subs _]
   (if (empty? subs)
     {}
     (->> subs
          (map #(second (ut/splitter (str %) "/")))
          (map #(first (ut/splitter (str %) ">")))
          frequencies))))

(re-frame/reg-sub ::tabs
                  (fn [db]
                    (let [seen-tabs     (distinct (for [[_ pv] (get db :panels)] (get pv :tab))) ;; prevents
                          explicit-tabs (get db :tabs [])
                          tabs          (vec (remove empty? (distinct (into explicit-tabs seen-tabs))))]
                      tabs)))

(re-frame/reg-sub
 ::block-layers
 (fn [db]
   (let [block-names       (keys (get db :panels))
         snapshot-names    (keys (get-in db [:snapshots :params]))
         tab-names         (get db :tabs)
         user-param-names  (keys (get-in db [:click-param :param]))
         view-names        (distinct (mapcat (fn [[_ v]] (keys (get v :views))) (get db :panels)))
         query-names       (mapcat (fn [[_ v]] (keys (get v :queries))) (get db :panels)) ;; faster
         runner-keys       (keys (get-in db [:server :settings :runners] {}))
         all-runners       (apply concat
                                  (for [r runner-keys]
                                    (mapcat (fn [[_ v]]
                                              (keys (get v r)))
                                            (get db :panels))))
         all-keys          (vec (apply concat
                                       [block-names user-param-names
                                        view-names snapshot-names tab-names
                                        query-names all-runners]))]
     all-keys)))

(re-frame/reg-sub
 ::visible-tabs
 (fn [db]
   (let [;tabs @(ut/tracked-subscribe [::tabs])
         tabs        @(ut/tracked-sub ::tabs {})
         hidden-tabs (get db :hidden-tabs [])]
     (vec (cset/difference (set tabs) (set hidden-tabs))))))

(re-frame/reg-sub
 ::hidden-tabs
 (fn [db] (vec (get db :hidden-tabs []))))

(re-frame/reg-event-db
 ::toggle-tab-visibility
 (undoable)
 (fn [db [_ tab-name]]
   (let [tabs    (get db :hidden-tabs []) ;@(ut/tracked-subscribe [::tabs])
         exists? (some #(= % tab-name) tabs)]
     (if exists?
       (assoc db :hidden-tabs (vec (remove #(= % tab-name) tabs)))
       (assoc db :hidden-tabs (vec (conj tabs tab-name)))))))

(re-frame/reg-sub
 ::panel-keys
 (fn [db] (vec (keys (get db :panels {})))))

(re-frame/reg-sub
 ::selected-tab
 (fn [db] (get db :selected-tab (first @(ut/tracked-sub ::tabs {})))))

(re-frame/reg-event-db
 ::delete-tab
 (undoable)
 (fn [db [_ tab-name]]
   (let [tabs     @(ut/tracked-sub ::tabs {}) ;;(get db :tabs)
         curr-idx (.indexOf tabs tab-name)
         new-tabs (vec (remove #(= % tab-name) tabs))
         new-curr (try (nth (cycle new-tabs) curr-idx) (catch :default _ (first new-tabs)))]
     (ut/tapp>> [:boo curr-idx new-curr])
     (-> db
         (assoc :tabs (vec (remove #(= % tab-name) tabs)))
         (assoc :selected-tab new-curr)
         (assoc :panels (into {} (filter #(not (= tab-name (get (val %) :tab))) (get db :panels))))))))

(re-frame/reg-event-db
 ::rename-tab
 (fn [db [_ old new]]
   (-> db
       (assoc :panels (ut/update-nested-tabs (get db :panels) old new))
       (assoc :tabs (vec (map (fn [item] (if (= item old) new item)) (get db :tabs))))
       (assoc :selected-tab new))))

(re-frame/reg-event-db
 ::rename-block-layer
 (fn [db [_ panel-key old new]]
   (let [old (keyword (cstr/replace (str old) ":" ""))
         new (keyword new)
         new (ut/safe-key new)]
     ;;(tapp>> [:change-block-layer? panel-key old new])
     (swap! db/data-browser-query assoc panel-key new)  ;; to select it in the editor 
     (assoc-in db [:panels panel-key] (ut/postwalk-replacer
                                       {old new}
                                       (get-in db [:panels panel-key]))))))

(re-frame/reg-event-db
 ::delete-block-layer
 (fn [db [_ panel-key data-key-type block-layer]]
   (tapp>> [:delete-block-layer panel-key data-key-type block-layer])
   (let [block-layer (if (string? block-layer)
                       (try (edn/read-string block-layer)
                            (catch :default _ (keyword (cstr/replace block-layer ":" "")))) block-layer)]
     (swap! db/data-browser-query assoc panel-key nil)
     (reset! db/view-title-edit-idx nil)
     (-> db
         (ut/dissoc-in [:panels panel-key data-key-type block-layer])
         (ut/dissoc-in [:data block-layer])))))

(re-frame/reg-event-db
 ::select-tab
 (fn [db [_ tab]]
   (-> db
       (assoc :selected-tab tab)
       (assoc-in [:click-param :sys :selected-tab] tab)
       (assoc-in [:click-param :sys :selected-tab-idx]
                 (try (.indexOf (get db :tabs) tab) (catch :default _ -1))))))

(re-frame/reg-event-db ::add-tab (undoable) (fn [db [_ new-tab]] (assoc db :tabs (conj (vec (get db :tabs)) new-tab))))

(re-frame/reg-sub ::editor? (fn [db] (get db :editor?)))

(re-frame/reg-sub ::buffy? (fn [db] (get db :buffy?)))

(re-frame/reg-sub ::flow? (fn [db] (get db :flow?)))

(re-frame/reg-sub ::external? (fn [db] (get db :external? false)))

(re-frame/reg-sub ::lines? (fn [db] (get db :lines? false)))

(re-frame/reg-sub ::full-no-ui? (fn [db] (get db :no-ui? false)))

(re-frame/reg-sub ::peek? (fn [db] (get db :peek? false)))

(re-frame/reg-sub ::flow-editor? (fn [db] (get db :flow-editor? false)))

(re-frame/reg-sub ::auto-run? (fn [db] (get db :auto-run? false)))

(re-frame/reg-sub
 ::auto-run-and-connected?
 (fn [db]
   (and (= (get-in db [:websocket-fx.core/sockets :default :status]) :connected) (get db :auto-run? false))))

(re-frame/reg-sub
 ::bg-status?
 (fn [_]
   (and @db/auto-refresh? ;; auto-refresh killswitch bool
        (or (= @db/editor-mode :status)
            (= @db/editor-mode :vvv)
            (= @db/editor-mode :meta)))))

(re-frame/reg-sub
 ::update-flow-statuses?
 (fn [db]
   (and (get db :flow? false)
        (= (get @db/flow-editor-system-mode 0) "flows running")
        (get db :flow-editor? false))
  ;; false
   ))

(re-frame/reg-event-db
 ::flow-statuses-response
 (fn [db [_ ss]]
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
 ::condi-valve-open? ;; TODO needs bid
 (fn [db [_ flow-id bid condi-key]]
   (let [fck   (ut/replacer (str condi-key) ":" "")
         bid   (ut/replacer (str bid) ":" "")
         conns (get-in db [:flows flow-id :connections])
         conn  (for [[c1 c2] conns
                     :when   (and ;(cstr/ends-with? (str c1) (str "/" fck))
                              (= c1 (keyword (str bid "/" fck))))]
                 (try (keyword (ut/replacer (str (first (ut/splitter (str c2) #"/"))) ":" ""))
                      (catch :default _ :error!)))
         kkey  (first conn)]
     (get-in db [:flow-results :condis flow-id kkey] false))))

(re-frame/reg-sub
 ::flow-channels-open?
 (fn [db [_ flow-id]]
   (get-in db [:flow-statuses flow-id :channels-open?] false)))

(re-frame/reg-event-db
 ::update-flow-statuses
 (fn [db [_]]
   ;;(re-frame/dispatch [::http/insert-alert "update flow statues" 13 1 5 ])
   (ut/tracked-dispatch [::wfx/request :default
                         {:message     {:kind :get-flow-statuses :client-name (get db :client-name)}
                          :timeout     50000
                          :on-response [::flow-statuses-response]}])
   db))

;; (re-frame/reg-sub ::has-query? (fn [db [_ panel-key query-key]] (not (nil? (get-in db [:panels panel-key :queries query-key])))))

(re-frame/reg-sub
 ::has-query?
 (fn [db [_ panel-key query-key]]
   (contains? (get-in db [:panels panel-key :queries]) query-key)))

(re-frame/reg-sub ::col-names ;; temp
                  (fn [db [_]] (get db :col-names)))

(re-frame/reg-event-db ::toggle-external (fn [db [_]] (assoc-in db [:external?] (not (get-in db [:external?] false)))))

(re-frame/reg-event-db ::toggle-flow (fn [db [_]] (assoc-in db [:flow?] (not (get-in db [:flow?] false)))))

(re-frame/reg-event-db ::toggle-flow-editor (fn [db [_]] (assoc-in db [:flow-editor?] (not (get-in db [:flow-editor?] false)))))

(re-frame/reg-event-db ::toggle-session-modal (fn [db [_]] (assoc db :session-modal? (not (get db :session-modal? false)))))

(re-frame/reg-event-db ::toggle-doom-modal (fn [db [_]] (assoc db :doom-modal? (not (get db :doom-modal? false)))))

(re-frame/reg-event-db 
 ::toggle-quake-console 
 (fn [db [_]] 
   (reset! db/console-voice-text nil)
   (assoc db :quake-console? (not (get db :quake-console? false)))))

(re-frame/reg-event-db ::toggle-quake-console-off (fn [db [_]] (assoc db :quake-console? false)))

(re-frame/reg-event-db ::disable-session-modal (fn [db [_]] (assoc db :session-modal? false)))

(re-frame/reg-event-db ::disable-doom-modal (fn [db [_]] (assoc db :doom-modal? false)))

(re-frame/reg-sub ::session-modal? (fn [db] (get db :session-modal? false)))

(re-frame/reg-sub ::doom-modal? (fn [db] (get db :doom-modal? false)))

(re-frame/reg-event-db ::toggle-flow-gantt (fn [db [_]] (assoc-in db [:flow-gantt?] (not (get-in db [:flow-gantt?] false)))))

(re-frame/reg-sub ::flow-gantt? (fn [db _] (get db :flow-gantt? false)))

(re-frame/reg-sub ::quake-console? (fn [db _] (get db :quake-console? false)))

(re-frame/reg-event-db ::toggle-editor
                       (fn [db [_]]
                         (if (get db :flow?)
                           (assoc-in db [:flow-editor?] (not (get-in db [:flow-editor?] false)))
                           (assoc-in db [:editor?] (not (get-in db [:editor?] false))))))

(re-frame/reg-event-db ::toggle-buffy
                       (fn [db [_]]
                         (if (get db :flow?)
                           (assoc-in db [:flow-gantt?] (not (get-in db [:flow-gantt?] false)))
                           (assoc-in db [:buffy?] (not (get-in db [:buffy?] false))))))

(re-frame/reg-event-db ::toggle-lines (fn [db [_]] (assoc-in db [:lines?] (not (get-in db [:lines?] false)))))

(re-frame/reg-event-db ::toggle-peek (fn [db [_]] (assoc-in db [:peek?] (not (get-in db [:peek?] false)))))

(re-frame/reg-event-db ::toggle-kick-alert (fn [db [_]] (reset! db/kick-alert (not @db/kick-alert)) db))

(re-frame/reg-event-db ::toggle-auto-run (fn [db [_]] (assoc-in db [:auto-run?] (not (get-in db [:auto-run?] false)))))

(re-frame/reg-event-db ::toggle-no-ui (fn [db [_]] (assoc-in db [:no-ui?] (not (get-in db [:no-ui?] false)))))

(re-frame/reg-event-db ::toggle-alert-mute (fn [db [_]] (assoc-in db [:alert-mute?] (not (get-in db [:alert-mute?] false)))))

(re-frame/reg-event-db ::overwrite-theme
                       (undoable)
                       (fn [db [_ theme-map]]
                         (assoc-in db
                                   [:click-param :theme]
                                   (merge (apply dissoc (get-in db [:click-param :theme]) (vec (keys db/base-theme))) theme-map))))

(re-frame/reg-event-db ::add-board
                       (undoable)
                       (fn [db [_ board-map]]
                         (ut/tapp>> [:board-map board-map])
                         (-> db
                             (assoc-in [:click-param :param]
                                       (merge (get-in db [:click-param :param]) (get-in board-map [:click-param :param])))
                             (assoc :panels (merge (get db :panels) (get board-map :panels))))))


(re-frame/reg-event-db ::refresh-meta-tables
                       (fn [db [_ type]]
                         (condp = type
                           :vvv    (-> db
                                       (ut/dissoc-in [:query-history :recos-sys])
                                       (ut/dissoc-in [:query-history :viz-shapes-sys])
                                       (ut/dissoc-in [:query-history :viz-shapes0-sys])
                                       (ut/dissoc-in [:query-history :viz-tables-sys]))
                           :files  (-> db
                                       (ut/dissoc-in [:query-history :files-sys])
                                       (ut/dissoc-in [:query-history :blocks-sys]))
                           :meta   (-> db
                                       (ut/dissoc-in [:query-history :tables-sys])
                                       (ut/dissoc-in [:query-history :fields-sys])
                                       (ut/dissoc-in [:query-history :connections-sys]))
                           :status (-> db
                                       (ut/dissoc-in [:query-history :status-sys])))))

(re-frame/reg-event-db
 ::refresh-all
 (fn [db _]
   (-> db
       (dissoc :query-history)
       (dissoc :query-history-meta))))

(re-frame/reg-event-db ::set-grid-reco? (fn [db [_ b]] (assoc-in db [:grid-reco?] b)))

(re-frame/reg-sub ::grid-reco? (fn [db] (get db :grid-reco?)))

(re-frame/reg-event-db ::set-preview-keys (fn [db [_ v]] (assoc-in db [:preview-keys] v)))

(re-frame/reg-event-db ::clear-query (fn [db [_ k]] (ut/dissoc-in db [:data k])))

(re-frame/reg-event-db ::set-preview-keys2 (fn [db [_ v]] (assoc-in db [:preview-keys2] v)))

(re-frame/reg-sub ::preview-keys (fn [db] (get db :preview-keys)))

(re-frame/reg-sub ::preview-keys2 (fn [db] (get db :preview-keys2)))

(re-frame/reg-event-db ::set-recos-page (fn [db [_ b]] (assoc-in db [:recos-page] b)))

(re-frame/reg-event-db ::set-recos-page2 (fn [db [_ b]] (assoc-in db [:recos-page2] b)))

(re-frame/reg-sub ::recos-page (fn [db] (get db :recos-page 0)))

(re-frame/reg-sub ::recos-page2 (fn [db] (get db :recos-page2 20)))

(re-frame/reg-sub ::user-parameters (fn [db [_ k]] (get-in db [:click-param k])))

(re-frame/reg-sub
 ::parameters-used-from
 (fn [db {:keys [query-key]}]
   (let [asl               (get db :panels)
         aslflat           (filter #(cstr/includes? (str %) "/") (ut/deep-flatten asl))
         valid-body-params (filter #(not (and (cstr/ends-with? (str %) "-sys") (cstr/starts-with? (str %) ":theme")))
                                   (ut/deep-flatten @(ut/tracked-sub ::valid-body-params-in {:body asl})))
         vbp-bools         (try (into {}
                                      (for [e valid-body-params]
                                        (let [spl (ut/splitter (str e) #"/")]
                                          {[(-> (first spl)
                                                str
                                                (ut/replacer #":" "")
                                                keyword)
                                            (-> (last spl)
                                                str
                                                (ut/replacer #":" "")
                                                keyword)]
                                           (true? (some #(= % e) aslflat))})))
                                (catch :default _ {}))
         cell-query-key    (keyword (str (ut/replacer (str query-key) #":" "") ".*"))
         params            (into {}
                                 (apply concat
                                        (for [[p v] (get-in db [:click-param])
                                              :when (and (not (= p :theme)) (not (cstr/ends-with? (str p) "-sys")))]
                                          (for [e (keys v)]
                                            (let [;asl @(ut/tracked-subscribe [::all-sql-calls])
                                                  comb-key (keyword (str (ut/safe-name p) "/" (ut/safe-name e)))]
                                              {[p e] (true? (some #(= % comb-key) aslflat))})))))
         both-maps         (into {}
                                 (for [[[k1 k2] b] (merge params vbp-bools)
                                       :let        [k2 (if (cstr/ends-with? (str k1) ".*")
                                                         [k2 (get-in db [:click-param cell-query-key k2])]
                                                         k2)]]
                                   {[k1 k2] b}))
         matchers          (vec (map last
                                     (keys (filter #(and (or (last %) (vector? (last (first %))))
                                                         (cstr/starts-with? (str (first (first %))) (str query-key)))
                                                   both-maps))))]
     matchers)))

(re-frame/reg-event-db ::set-user-parameters (fn [db [_ k v]] (assoc-in db [:click-param k] v)))

(re-frame/reg-event-db ::set-client-name
                       (fn [db [_ client-name]]
                         (set! (.-title js/document) (str "Rabbit (" client-name ")"))
                         (assoc-in db [:client-name] client-name)))

(re-frame/reg-sub ::client-name (fn [db] (get db :client-name)))

(re-frame/reg-sub ::last-heartbeat (fn [db] (get-in db [:status-data :heartbeat :kick :data 0 :at] "none!")))

(re-frame/reg-event-db
 ::set-screen-name
 (fn [db [_ screen-name]] (http/change-url (str "/" screen-name)) (assoc-in db [:screen-name] (str screen-name))))

(re-frame/reg-sub ::screen-name (fn [db] (get db :screen-name (ut/safe-name (get db :client-name)))))

(re-frame/reg-sub
 ::current-view-mode
 (fn [db {:keys [panel-key data-key]}]
   (let [view-type @(ut/tracked-sub ::view-type {:panel-key panel-key :view data-key})
         modes (get-in db [:server :settings :runners view-type :modes])]
     (get-in db [:panels panel-key :selected-mode data-key] (first modes)))))

(re-frame/reg-sub
 ::current-view-mode-clover-fn
 (fn [db {:keys [panel-key data-key]}]
   (let [curr @(ut/tracked-sub ::current-view-mode {:panel-key panel-key :data-key data-key})
         clover-fn (get-in db [:server :settings :modes curr] ;; default to text if we cant find it or its a virtual-view
                           (get-in db [:server :settings :modes :text]))]
     clover-fn)))

(re-frame/reg-event-db
 ::set-view-mode
 (fn [db [_ panel-key data-key mode]]
   (tapp>> [:set-view-mode panel-key data-key mode])
   (assoc-in db [:panels panel-key :selected-mode data-key] mode)))

(re-frame/reg-event-db
 ::dispatch-keepalive
 (fn [db [_]]
   (do (conn/sql-data [:keepalives] {:select [(rand-int 123433333335)] :limit [(rand-int 123333333345)]})
       (assoc-in db [:keepalives] (+ (get-in db [:keepalives] 0) 1)))))

(re-frame/reg-event-db
 ::refresh-status
 (fn [db [_]]
   (let [type @db/editor-mode]
     (condp = type
       :vvv    (-> db
                   (ut/dissoc-in [:query-history :recos-sys])
                   (ut/dissoc-in [:query-history :viz-shapes-sys])
                   (ut/dissoc-in [:query-history :viz-shapes0-sys])
                   (ut/dissoc-in [:query-history :viz-tables-sys]))
       :files  (-> db
                   (ut/dissoc-in [:query-history :files-sys])
                   (ut/dissoc-in [:query-history :blocks-sys]))
       :meta   (-> db
                   (ut/dissoc-in [:query-history :tables-sys])
                   (ut/dissoc-in [:query-history :fields-sys])
                   (ut/dissoc-in [:query-history :connections-sys]))
       :status (-> db
                   (ut/dissoc-in [:query-history :status-sys]))))))

(re-frame/reg-event-db
 ::update-workspace
 (undoable)
 (fn [db [_ keypath value]]
   (assoc-in db (cons :panels keypath) value)))

(re-frame/reg-event-db
 ::update-workspace-raw
 (undoable)
 (fn [db [_ keypath value]]
   (tapp>> [:update-raw keypath value])
   ;(ut/tracked-dispatch [::update-panels-hash])
   (if (not (= (get-in db keypath) value))
     (assoc-in db keypath value)
     db)))

(re-frame/reg-event-db
 ::update-workspace-raw-dissoc
 (undoable)
 (fn [db [_ keypath value]]
   (tapp>> [:update-raw-dissoc keypath value])
   (ut/dissoc-in db keypath)))

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
   (let [name (cond (= type :queries) "new-query"
                    (= type :views) "new-view"
                    :else (str "new-" (cstr/replace (str type) ":" "")))
         name (ut/safe-key (keyword name))]
     (ut/tapp>> [:adding-new type :to panel-key :as name])
     (swap! db/data-browser-query assoc panel-key name)
     (assoc-in db [:panels panel-key type name] body))))

(re-frame/reg-event-db
 ::update-workspace-noundo
 (fn [db [_ keypath value]]
   (assoc-in db (cons :panels keypath) value)))

(re-frame/reg-event-db
 ::update-selected
 (undoable)
 (fn [db [_ value]]
   (ut/tapp>> [:updating-selected [:panels (get db :selected-block)] :with value])
   ;(ut/tracked-dispatch [::send-panel-updates [(get db :selected-block)]])
   ;(ut/tracked-dispatch [::update-panels-hash])
   (assoc-in db [:panels (get db :selected-block)] value)))

(re-frame/reg-event-db
 ::update-selected-key
 (undoable)
 (fn [db [_ key value]]
   (ut/tapp>> [:updating-selected-key [:panels (get db :selected-block) key] :with value])
   ;(ut/tracked-dispatch [::send-panel-updates [(get db :selected-block)]])
   ;(ut/tracked-dispatch [::update-panels-hash])
   (assoc-in db [:panels (get db :selected-block) key] value)))

(re-frame/reg-event-db
 ::update-selected-key-cons
 (undoable)
 (fn [db [_ key value]]
   (let [kp (vec (into [:panels (get db :selected-block)] key))]
     (ut/tapp>> [:updating-selected-key-cons kp :with value])
     ;(ut/tracked-dispatch [::send-panel-updates [(get db :selected-block)]])
     ;(ut/tracked-dispatch [::update-panels-hash])
     (assoc-in db kp value))))

(re-frame/reg-event-db
 ::update-selected-field
 (undoable)
 (fn [db [_ kp value]] (ut/tapp>> [:updating-selected-field kp :with value]) (assoc-in db kp value)))

(re-frame/reg-event-db
 ::clean-up-previews
 (fn [db [_]]
   (let [pp (doall (for [r (filter #(cstr/starts-with? (str %) ":reco-preview") (keys (get-in db [:panels])))] r))
         pl (keys (get db :panels))
         p0 (cset/difference (set pl) (set pp))]
     (assoc db :panels (select-keys (get db :panels) p0)))))

(re-frame/reg-event-db
 ::delete-selected-panel
 (undoable)
 (fn [db _] (ut/tracked-dispatch [::delete-panel (get db :selected-block)]) db))

(re-frame/reg-event-db
 ::delete-panel
 (undoable)
 (fn [db [_ & [panel-key]]]
   (let [panel-key      (if (nil? panel-key) (get db :selected-block) panel-key) ;; if nil
         dying-queries  (get-in db [:panels panel-key :queries])
         salute-sailors (into {}
                              (for [[k v] dying-queries]
                                {(keyword (str "query/" (ut/safe-name k))) (ut/clean-sql-from-ui-keys v)}))]
     (if (= panel-key (get db :selected-block))
       (ut/postwalk-replacer salute-sailors
                             (-> db
                                 (ut/dissoc-in [:panels panel-key])
                                 (assoc :selected-block "none!")))
       (ut/postwalk-replacer salute-sailors (ut/dissoc-in db [:panels panel-key]))))))

(re-frame/reg-event-db
 ::quick-delete-panel
 (fn [db [_ panel-key]]
   (ut/dissoc-in db [:panels panel-key])))

(re-frame/reg-sub
 ::lookup-panel-key-by-query-key
 (fn [db [_ query-key]]
   (first (remove nil?
                  (for [[k v] (get db :panels)] (when (some #(= query-key %) (keys (get v :queries))) k))))))

(re-frame/reg-sub
 ::repl-output
 (fn [db [_ query-key]]
   (get-in db [:repl-output query-key] nil)))

(re-frame/reg-sub
 ::lookup-connection-id-by-query-key
 (fn [db [_ query-key]]
   (let [panel-key (first (remove nil? (for [[k v] (get db :panels)] (when (some #(= query-key %) (keys (get v :queries))) k))))]
     (get-in db [:panels panel-key :queries query-key :connection-id] 
             (get-in db [:panels panel-key :connection-id] "cache.db")))))  

(re-frame/reg-event-db
 ::unalias-query
 (undoable)
 (fn [db [_ query-key]]
   (let [panel-key      (first (remove nil?
                                       (for [[k v] (get db :panels)]
                                         (when (some #(= query-key %) (keys (get v :queries))) k))))
         dying-queries  {query-key (get-in db [:panels panel-key :queries query-key])}
         salute-sailors (into {}
                              (for [[k v] dying-queries]
                                {(keyword (str "query/" (ut/safe-name k))) (ut/clean-sql-from-ui-keys v)}))]
     (ut/postwalk-replacer salute-sailors db))))                                                   ;)

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
         new-page-num (cond (< new-page-num 1)         1
                            (> new-page-num ttl-pages) ttl-pages
                            :else                      new-page-num)]
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
 ::size-alpha
 (fn [db {:keys [panel-key]}]
   [(get-in db [:panels panel-key :h])
    (get-in db [:panels panel-key :w])]))

(re-frame/reg-sub
 ::sizes-alpha
 (fn [db {:keys [panel-key]}]
   (into
    (get-in db [:panels panel-key :root])
    [(get-in db [:panels panel-key :h])
     (get-in db [:panels panel-key :w])])))

(re-frame/reg-sub
 ::selected-block-map
 (fn [db]
   (get-in db [:panels (get db :selected-block)])))

(re-frame/reg-sub
 ::selected-block-map-kp
 (fn [db {:keys [keypath]}]
   (get-in db (vec (into [:panels (get db :selected-block)] keypath)))))

(re-frame/reg-sub
 ::selected-root
 (fn [db]
   (get-in db [:panels (get db :selected-block) :root])))

(re-frame/reg-sub
 ::selected-h
 (fn [db]
   (get-in db [:panels (get db :selected-block) :h])))

(re-frame/reg-sub
 ::selected-w
 (fn [db]
   (get-in db [:panels (get db :selected-block) :w])))

(re-frame/reg-event-db
 ::change-drop-type ;; experiment
 (fn [db] (ut/tapp>> [::change-drop-type]) (assoc db :drop-type (rand-int 99))))

(re-frame/reg-sub
 ::drop-type ;; experiment
 (fn [db] (get db :drop-type)))


(re-frame/reg-event-db
 ::get-memory-usage
 (fn [db]
   (let [mem (when (exists? js/window.performance.memory)
               [(.-totalJSHeapSize js/window.performance.memory)
                (.-usedJSHeapSize js/window.performance.memory)
                (.-jsHeapSizeLimit js/window.performance.memory)])]
     (assoc db :memory mem))))

(re-frame/reg-sub
 ::memory
 (fn [db _]
   (get db :memory)))

(re-frame/reg-sub
 ::runstreams
 (fn [db _]
   (let [runstreams-map  (get db :runstreams)
         runstreams-map1 {}
         runstreams-map  (merge runstreams-map1 runstreams-map)]
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
   (get db :runstreams-lookups)))

(re-frame/reg-sub
 ::runstream-override-map
 (fn [_ _]
   (let [;runstreams @(ut/tracked-subscribe [::runstreams])
         runstreams @(ut/tracked-sub ::runstreams {})]
     (into {}
           (for [{:keys [flow-id values]} runstreams
                 :let                     [override-map (into {}
                                                              (for [[k {:keys [value source]}] values
                                                                    :when                      (not (nil? value))]
                                                                {k (if (= source :input)
                                                                     value
                                                                     (let [;;vv @(ut/tracked-subscribe
                                                                           vv @(ut/tracked-sub ::rs-value {:flow-id flow-id :kkey k})]
                                                                       (if (and (vector? vv) (every? string? vv))
                                                                         (cstr/join "\n" vv)
                                                                         vv)))}))
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

(defn safe-nth [c i] ;; this really isnt needed since we VERY rarely deal with pure lists, and on vecs, GET is perfectly fine
  (cond
    (vector? c) (get c i)
    (seq? c) (if (and (>= i 0) (< i (count c)))
               (nth c i)
               nil)
    :else nil))

(re-frame/reg-event-db
 ::nudge-panel
 (undoable)
 (fn [db [_ dir]]
   (if (get db :flow?)
     (let [bid     (get db :selected-flow-block)
           x       (get-in db [:flows (get db :selected-flow) :map bid :x])
           y       (get-in db [:flows (get db :selected-flow) :map bid :y])
           chunk   25
           [nx ny] (condp = dir :left [(- x chunk) y] :right [(+ chunk x) y] :up [x (- y chunk)] :down [x (+ chunk y)])]
       (-> db
           (assoc-in [:flows (get db :selected-flow) :map bid :x] nx)
           (assoc-in [:flows (get db :selected-flow) :map bid :y] ny)))
     (let [panel-id     (get db :selected-block)
           selected-tab (get-in db [:panels panel-id :selected-view])
           qkeys        (keys (get-in db [:panels panel-id :queries]))
           query-key    (cond (and (nil? selected-tab) (not (empty? qkeys)))                   (first qkeys)
                              (and (not (nil? selected-tab)) (some #(= % selected-tab) qkeys)) selected-tab ;(first
                              :else                                                            nil)
           sel-col      (get db :selected-cols)
           curr-field   (when (and (= (safe-nth sel-col 0) panel-id) (= (safe-nth sel-col 1) query-key)) (safe-nth sel-col 2))
           col-mode?    (true? (not (nil? curr-field)))
           x            (get-in db [:panels panel-id :root 0])
           y            (get-in db [:panels panel-id :root 1])]
       (if col-mode?
         (let [modfields          (get db :col-names)
               modfields?         (not (nil? modfields))
               fields             (if modfields? modfields (vec (keys (get-in db [:meta query-key :fields]))))
               field-idx          (.indexOf fields curr-field)
               selectc            (get-in db [:panels panel-id :queries query-key :select])
               fc                 (count fields)
               move-right-selectc (if (not (> (+ field-idx 1) (- fc 1)))
                                    (assoc selectc
                                           (+ field-idx 1) (get selectc field-idx)
                                           field-idx       (get selectc (+ field-idx 1)))
                                    selectc)
               move-left-selectc  (if (>= (- field-idx 1) 0)
                                    (assoc selectc
                                           (- field-idx 1) (get selectc field-idx)
                                           field-idx       (get selectc (- field-idx 1)))
                                    selectc)
               fields-right       (if (not (> (+ field-idx 1) (- fc 1)))
                                    (assoc fields
                                           (+ field-idx 1) (get fields field-idx)
                                           field-idx       (get fields (+ field-idx 1)))
                                    fields)
               fields-left        (if (>= (- field-idx 1) 0)
                                    (assoc fields
                                           (- field-idx 1) (get fields field-idx)
                                           field-idx       (get fields (- field-idx 1)))
                                    fields)]
           (cond (= dir :left)  (-> db
                                    (assoc-in [:panels panel-id :queries query-key :select] move-left-selectc)
                                    (assoc :col-names fields-left))
                 (= dir :right) (-> db
                                    (assoc-in [:panels panel-id :queries query-key :select] move-right-selectc)
                                    (assoc :col-names fields-right))))
         (assoc-in db
                   [:panels (get db :selected-block) :root]
                   (condp = dir :left [(- x 1) y] :right [(+ 1 x) y] :up [x (- y 1)] :down [x (+ 1 y)])))))))

(re-frame/reg-event-db
 ::stretch-panel
 (undoable)
 (fn [db [_ dir]]
   (if (get db :flow?)
     (let [bid   (get db :selected-flow-block)
           h     (get-in db [:flows (get db :selected-flow) :map bid :h])
           w     (get-in db [:flows (get db :selected-flow) :map bid :w])
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
           query-key     (cond (and (nil? selected-tab) (not (empty? qkeys)))                   (first qkeys)
                               (and (not (nil? selected-tab)) (some #(= % selected-tab) qkeys)) selected-tab ;(first
                               :else                                                            nil)
           sel-col       (get db :selected-cols)
           curr-field    (when (and (= (safe-nth sel-col 0) panel-id) (= (safe-nth sel-col 1) query-key)) (safe-nth sel-col 2))
           def-col-width (get-in db [:default-col-widths panel-id query-key] 80)
           curr-width    (get-in db [:panels (get db :selected-block) :queries query-key :col-widths curr-field] def-col-width)
           col-mode?     (true? (not (nil? curr-field)))
           w             (get-in db [:panels panel-id :w])
           h             (get-in db [:panels panel-id :h])]
       (if col-mode?
         (cond (= dir :wider)    (assoc-in db
                                           [:panels (get db :selected-block) :queries query-key :col-widths curr-field]
                                           (+ curr-width 10))
               (= dir :narrower) (assoc-in db
                                           [:panels (get db :selected-block) :queries query-key :col-widths curr-field]
                                           (- curr-width 10)))
         (condp = dir
           :wider    (assoc-in db [:panels (get db :selected-block) :w] (+ w 1))
           :narrower (when (>= (- w 1) 2) (assoc-in db [:panels (get db :selected-block) :w] (- w 1)))
           :longer   (assoc-in db [:panels (get db :selected-block) :h] (+ h 1))
           :shorter  (when (>= (- h 1) 2) (assoc-in db [:panels (get db :selected-block) :h] (- h 1)))))))))

(re-frame/reg-event-db ::select-block
                       (fn [db [_ block-key]]
                         (ut/tapp>> [:selecting-block block-key])
                         (-> db ;; plus reset selected cols, just in case... (would cause user
                                ;; confusion possibly)
                             (assoc :selected-block block-key)
                             (assoc-in [:click-param :param :selected-block] block-key)
                             (assoc :selected-cols nil))))

(re-frame/reg-event-db
 ::esc-unselect-current
 (fn [db [_]] ;; if col is selected, first unselect that and then on subsequent, unselect
   (reset! mad-libs-view nil)
   (reset! db/editor-mode :meta)
   (reset! db/cm-focused? false)
   (ut/tracked-dispatch [::toggle-quake-console-off])

   (let [all-clear? (and (nil? (get db :selected-flow-block))
                         (or (nil? (get db :selected-block)) (= (get db :selected-block) "none!"))
                         (nil? (get db :selected-cols)))]
      (cond
        (get db :doom-modal? false) 
        (assoc db :doom-modal? false)

        all-clear?
        (assoc db :doom-modal? true)

        (and (not (nil? (get db :selected-flow-block))) (get db :flow?))
        (assoc db :selected-flow-block nil)

        (nil? (get db :selected-cols))
        (-> db
            (assoc :selected-block "none!")
            (assoc :col-names nil)
            (assoc :selected-cols nil))

        :else
        (-> db
            (assoc :selected-cols nil)
            (assoc :col-names nil))))))

(re-frame/reg-event-db
 ::next-panel
 (fn [db]
   (let [flow? (get db :flow?)]
     (if flow?
       (let [panel-keys     (vec (for [[k v] (get-in db [:flows (get db :selected-flow) :map]) :when (get v :x)] k))
             selected-panel (get db :selected-flow-block)
             panel-idx      (.indexOf panel-keys selected-panel)
             next-panel     (nth (cycle panel-keys) (+ panel-idx 1))]
         (ut/tapp>> [:next-flow-block :from selected-panel :to next-panel])
         (assoc db :selected-flow-block next-panel))
       (let [panel-keys     (filter #(and (not (cstr/starts-with? (str %) ":reco-preview"))
                                          (not (cstr/starts-with? (str %) ":query-preview")))
                                    (keys (doall (sort-by (fn [r] (let [rr (get r :root) [int2 int1] rr] (+ int1 int2)))
                                                          (into {}
                                                                (filter #(and (not (get (val %) :minimized? false))
                                                                              (= (get db :selected-tab) (get (val %) :tab "")))
                                                                        (get db :panels))))))) ;(cycle (keys
             selected-panel (get db :selected-block)
             panel-idx      (.indexOf panel-keys selected-panel)
             next-panel     (nth (cycle panel-keys) (+ panel-idx 1))]
         (-> db
             (assoc-in [:click-param :param :selected-block] next-panel)
             (assoc :selected-block next-panel)))))))

(re-frame/reg-event-fx ::undo-one (fn [_ _] (ut/tracked-dispatch [:undo])))

(re-frame/reg-event-fx ::redo-one (fn [_ _] (ut/tracked-dispatch [:redo])))

(re-frame/reg-sub ::build-grid
                  (fn [_ {:keys [bh bw start-h start-w]}] (vec (for [x (range start-w bw) y (range start-h bh)] [x y]))))

(re-frame/reg-sub ::used-space
                  (fn [_ [_ workspace bricks-wide bricks-high]]
                    (vec (apply concat
                                (for [[panel-key panel] workspace]
                                  (let [root-x (first (:root panel)) ;(first root-vec)
                                        root-y (last (:root panel))  ;(last root-vec)
                                        w      (if (= (:w panel) 0) (- bricks-wide 1) (:w panel))
                                        h      (if (= (:h panel) 0) (- bricks-high 1) (:h panel))]
                                    (for [x (range root-x (+ root-x w)) y (range root-y (+ root-y h))] [x y])))))))

(re-frame/reg-sub ::used-space2
                  (fn [db [_ bricks-high bricks-wide]]
                    (vec (apply concat
                                (for [[panel-key panel] (get db :panels)]
                                  (let [root-x (first (:root panel)) ;(first root-vec)
                                        root-y (last (:root panel))  ;(last root-vec)
                                        w      (if (= (:w panel) 0) (- bricks-wide 1) (:w panel))
                                        h      (if (= (:h panel) 0) (- bricks-high 1) (:h panel))]
                                    (for [x (range root-x (+ root-x w)) y (range root-y (+ root-y h))] [x y])))))))

(re-frame/reg-sub ::used-space3
                  (fn [db [_ bricks-high bricks-wide start-y end-y start-x end-x]]
                    (vec (remove nil?
                                 (apply concat
                                        (for [[panel-key panel] (get db :panels)]
                                          (let [root-x (first (:root panel)) ;(first root-vec)
                                                root-y (last (:root panel)) ;(last root-vec)
                                                w      (if (= (:w panel) 0) (- bricks-wide 1) (:w panel))
                                                h      (if (= (:h panel) 0) (- bricks-high 1) (:h panel))]
                                            (for [x (range root-x (+ root-x w))
                                                  y (range root-y (+ root-y h))]
                                              (when (and (and (>= x start-x) (<= x end-x)) (and (>= y start-y) (<= y end-y))) [x y])))))))))

(defn get-client-rect
  [evt]
  (let [r (.getBoundingClientRect (.-target evt))]
    {:left (.-left r) :top (.-top r) :width (.-width r) :height (.-height r) :bottom (.-bottom r) :right (.-right r)}))

(defn is-inside?
  [coord rects]
  (some (fn [[x y w h key]]
          (when (and (and (>= (first coord) x) (< (first coord) (+ x w))) (and (>= (last coord) y) (< (last coord) (+ y h))))
            key))
        (vec rects)))


(re-frame/reg-sub ::containers
                  (fn [db _]
                    (vec (for [[k v] (get db :panels)
                               :let  [kks (ut/deep-flatten (get v :views))]
                               :when (and (= (get v :tab) (get db :selected-tab))
                                          (not (get v :minimized? false))
                                          (not (get v :pinned? false))
                                          (not (get v :icon? false)) ;; legacy test blocks
                                          (not (get v :iconized? false))
                                          (some #(= % :grid) kks))]
                           (let [tt (cset/intersection (set (get db :tabs)) (set kks))]
                             (vec (into (get v :root) [(get v :w) (get v :h) tt k])))))))

(defn round-to-nearest-quarter [num] (* 0.25 (js/Math.round (/ num 0.25))))

(defn mouse-move-handler
  [offset block tab-offset & [icon?]]
  (fn [evt]
    (let [start-x       (.-clientX evt)
          start-y       (.-clientY evt)
          selected-root @(ut/tracked-subscribe [::root block]) ;;[-1 -1] ;
          off-x         (:x offset)
          off-y         (:y offset)
          x1            (if icon?
                          (+ (round-to-nearest-quarter (/ (- start-x off-x) db/brick-size)) 0)
                          (+ (js/Math.floor (/ (- start-x off-x) db/brick-size)) 1))
          y1            (if icon?
                          (+ (round-to-nearest-quarter (/ (- start-y off-y) db/brick-size)) 0)
                          (+ (js/Math.floor (/ (- start-y off-y) db/brick-size)) 0))
          x             (- x1 (first tab-offset))
          y             (- y1 (last tab-offset))
          translate     (when false ;icon?
                          (let [containers            @(ut/tracked-subscribe [::containers])
                                started-in-container? (not (= tab-offset [0 0]))
                                curr-tab              @(ut/tracked-subscribe [::selected-tab])
                                tab-over              (or (first (is-inside? [x1 y1] containers)) curr-tab)
                                base-tab?             (= curr-tab tab-over)]
                            (cond (and started-in-container? (not base-tab?)) [x1 y1 tab-over]
                                  (and started-in-container? base-tab?)       (let [[offx offy] (for
                                                                                                 [[x y w h [t]] containers
                                                                                                  :when         (= t tab-over)]
                                                                                                  [x y])]
                                                                                [(- x1 offx) (- y1 offy) tab-over])
                                  :else                                       [x y tab-over])))]
      (when (not (= selected-root [x y]))
        (if (and icon? translate)
          (do (when (not (= @dragging-block block)) (reset! dragging-block block))
              (ut/tracked-dispatch-sync [::update-workspace-noundo [block :root] [(first translate) (second translate)]])
              (ut/tracked-dispatch-sync [::update-workspace-noundo [block :tab] (last translate)]))
          (do (when (not (= @dragging-block block)) (reset! dragging-block block))
              (ut/tracked-dispatch-sync [::update-workspace-noundo [block :root] [x y]])))))))

(def mouse-dragging-panel? (reagent/atom false))

(defn mouse-up-handler
  [on-move]
  (fn me [evt]
    (do (reset! mouse-dragging-panel? false)
        (reset! dragging? false)
        (reset! dragging-body {})
        (reset! dragging-block nil)
        (reset! dyn-dropper-hover nil))
    (gevents/unlisten js/window EventType.MOUSEMOVE on-move)))

(defn mouse-down-handler
  [e block tab-offset & [icon?]]
  (let [{:keys [left top]} (get-client-rect e)
        offset             {:x (- (.-clientX e) left) :y (- (.-clientY e) top)}
        on-move            (mouse-move-handler offset block tab-offset icon?)]
    (do (reset! mouse-dragging-panel? true) (gevents/listen js/window EventType.MOUSEMOVE on-move))
    (gevents/listen js/window EventType.MOUSEUP (mouse-up-handler on-move))))

(def hover-square (reagent/atom nil))
(def on-block? (reagent/atom false))
(def over-block? (reagent/atom false))
(def over-block (reagent/atom nil))
(def over-flow? (reagent/atom false))

(re-frame/reg-sub
 ::get-view-data
 (fn [db {:keys [panel-key runner data-key]}]
   (get-in db [:panels panel-key runner data-key])))

(re-frame/reg-sub
 ::get-block-data
 (fn [db {:keys [panel-key]}]
   (get-in db [:panels panel-key])))

(defn is-float?
  [n] ;; cljs uses js "number" so core "float?" cant tell between int and float
  (and (number? n) (not= n (js/Math.floor n))))

(defn insert-new-block-raw [root width height body runner syntax opts-map no-selected?]
  (let [new-key            (str "block-" (rand-int 12345))
        clover?            (= runner :clover)
        clojure?           (= syntax "clojure")
        view-name          (ut/safe-key (keyword (str (cstr/replace (str runner) ":" "") "-hop")))
        runner             (if clover? :views runner)
        block-runners-map  @(ut/tracked-sub ::block-runners {})
        as-function?       (get-in block-runners-map [runner :hop-function?] false)
        _ (tapp>> [:new-hop-block clover? clojure? runner syntax body as-function?])
        req-block-name     (get-in (first body) [:drag-meta :block-name])
        new-keyw           (if (nil? req-block-name) (keyword new-key) (keyword req-block-name))
        new-keyw           (ut/safe-key new-keyw)
        tab                @(ut/tracked-sub ::selected-tab {})
        block-name         (cstr/replace (str runner "-" new-key) ":" "")
        client-name        @(ut/tracked-sub ::client-name {})
        valid-body         (if (or clover? clojure?)
                             (try (edn/read-string (str body))
                                  (catch :default _ (str body)))
                              ;;  (if (cstr/starts-with? body "\"") ;; add quotes if need be
                              ;;    body
                              ;;    (pr-str body))
                              ;;  (if (and (not (cstr/starts-with? body "\"")) (not (cstr/ends-with? body "\"")))
                              ;;    (str "\"" body "\"")
                              ;;    body)
                               ;(pr-str body)
                               ;(vec (map pr-str (cstr/split (str body) "\n")))
                               ;[(str (char 34) (cstr/trim (str body)) (char 34))]
                             [(str body)])
          ;;valid-body         (if (or clover? clojure?) valid-body [body])
        opts-map-star       (merge
                             (into {} (for [[k v] opts-map]
                                        {(keyword (cstr/replace (str k) ":" "*")) v}))
                             {:*id (cstr/replace (str client-name "++" new-keyw "++" runner "++" view-name) ":" "")
                              ;; :*context (let [[runner data-key]     @(ut/tracked-sub ::editor-panel-selected-view {})
                              ;;                 selected-block  @(ut/tracked-sub ::selected-block {})]
                              ;;             @(ut/tracked-sub ::get-view-data {:panel-key selected-block :runner runner :data-key data-key}))
                              :*client-name client-name})
        valid-body          (if (and as-function? (or clover? clojure?))
                              (ut/postwalk-replacer
                               (merge
                                {:clover-body valid-body}
                                opts-map-star)
                               (get-in block-runners-map [runner :default] valid-body))
                              valid-body)
          ;valid-body?       (ut/well-formed? body)
        base-map           {:h             height
                            :w             width
                            :root          root
                            :tab           tab
                            :minimized?    (and (not (true? no-selected?)) (= runner :fabric))
                            :selected-view view-name
                            :opts          {view-name (or opts-map {})}
                            :name          block-name
                            runner         {view-name valid-body}
                              ;:queries       {}
                            }]
    (tapp>> [:new-hop-block-valid-body valid-body [new-keyw] base-map])
    (if valid-body
      (do (ut/tracked-dispatch [::update-workspace [new-keyw] base-map])
          (when no-selected? (ut/tracked-dispatch [::select-block new-keyw]))
          :success)
      :failed)))

(re-frame/reg-sub
 ::new-block-random-text
 (fn [db _]
   (get-in db [:server :settings :new-block-rand]
           ["It's a perfect day for planting seeds & planning adventures!"])))

(defn insert-new-block
  [root width height & body]
  (when (not @over-flow?)
    (let [new-key        (str "block-" (rand-int 12345))
          req-block-name (get-in (first body) [:drag-meta :block-name])
          new-keyw       (if (nil? req-block-name) (keyword new-key) (keyword req-block-name))
          new-keyw       (ut/safe-key new-keyw)
          tab            @(ut/tracked-subscribe [::selected-tab])
          ;; param-value    (str @(ut/tracked-subscribe [::conn/clicked-parameter
          ;;                                             [(get-in (first body) [:drag-meta :param-table])
          ;;                                              (get-in (first body) [:drag-meta :param-field])]]))
          ;;param-has-fn? (try (and (= (first v) :run-solver) (= table :param)) (catch :default _ false))
          param-value   (try ;; will fail on new block create since there is not params, yo
                          @(ut/tracked-sub ::conn/clicked-parameter-key-alpha
                                           {:keypath [(keyword (cstr/join "/"
                                                                          (map #(cstr/replace (str %) ":" "")
                                                                               [(get-in (first body) [:drag-meta :param-table])
                                                                                (get-in (first body) [:drag-meta :param-field])])))]})
                          (catch :default _ nil))
          dtype         (ut/data-typer param-value)
          is-map?        (or (= dtype "map") (= dtype "vector"))
          is-image?      (and (or (cstr/ends-with? (cstr/lower-case (str param-value)) ".png")
                                  (cstr/ends-with? (cstr/lower-case (str param-value)) ".webp")
                                  (cstr/ends-with? (cstr/lower-case (str param-value)) ".jpg")
                                  (cstr/ends-with? (cstr/lower-case (str param-value)) ".jpeg")
                                  (cstr/ends-with? (cstr/lower-case (str param-value)) ".gif")
                                  (cstr/includes? (cstr/lower-case (str param-value)) "/images/")
                                  (cstr/includes? (cstr/lower-case (str param-value)) "/image/"))
                              (or
                               (cstr/starts-with? (cstr/lower-case (str param-value)) "http")
                               (cstr/starts-with? (cstr/lower-case (str param-value)) "./images")))
          is-video?      (or (cstr/ends-with? (cstr/lower-case (str param-value)) ".mp4")
                             (cstr/ends-with? (cstr/lower-case (str param-value)) ".mov"))

          param-key      (get-in (first body) [:drag-meta :param-full])
          is-view?       (and (cstr/ends-with? (str param-key) "-vw") (cstr/starts-with? (str param-key) ":flow"))
          multi-param?   (true? (cstr/includes? (str param-key) ".*/"))
          param-drag?    (= (get-in (first body) [:drag-meta :type]) :param)
          view-drag?     (= (get-in (first body) [:drag-meta :type]) :viewer-pull)
          input-click?   (= (get-in (first body) [:drag-meta :type]) :open-input)
          view-name      (cond (and body param-drag? multi-param?) :multi-param-vals
                               (and body param-drag? (not multi-param?)) :param-val
                               (and body view-drag?) :pulled-val
                               ;(and body input-click?) 0
                               :else :hare-vw)
          view-name     (ut/safe-key view-name)
          quotes         @(ut/tracked-sub ::new-block-random-text {})
          base-map       (if body ;; do we have incoming panel data or not?
                           (cond param-drag? ;; (= (get-in (first body) [:drag-meta :type]) :param)
                                 {:h       (get-in (first body) [:h])
                                  :w       (get-in (first body) [:w])
                                  :tab     tab
                                  :root    root
                                  :name    (str param-key) ;;new-key
                                  :selected-view view-name
                                  :views   (if multi-param?
                                             {view-name [:box :align :center :justify :center :padding "13px" :style
                                                         {:font-size "30px"} :child
                                                         [:h-box :gap "9px" :children param-key]]}
                                             {view-name (if is-map?
                                                          [:data-viewer  param-key] ;; no wrapper on data-viewer 
                                                          [:box :align :center :justify :center :padding "13px" :style
                                                           {:font-size (cond is-map? "12px"
                                                                             :else "45px")}
                                                           :child
                                                           (cond is-view?  [:honeycomb param-key] ;; TODO, we
                                                                  ;;is-map?   [:data-viewer  param-key]
                                                                 is-image? [:img {:src param-key :width "100%"}]
                                                                 is-video? [:iframe ;; :video ? html5 shit
                                                                            {:src   param-key ;:movies_deets/_1080p_video
                                                                             :style {:border "none"
                                                                                     :width  :panel-width+80-px
                                                                                     :height :panel-height+80-px}}]
                                                                 :else     [:string param-key])])})
                                  :queries {}}
                                 view-drag? ;; (= (get-in (first body) [:drag-meta :type]) :viewer-pull)
                                 {:h       (get-in (first body) [:h])
                                  :w       (get-in (first body) [:w])
                                  :tab     tab
                                  :root    root
                                  :name    new-key
                                  :selected-view view-name
                                  :views   {view-name (get-in (first body) [:drag-meta :param-full])}
                                  :queries {}}
                                 input-click? ;; (= (get-in (first body) [:drag-meta :type]) :open-input) ;; middle click
                                 (merge (first body) {:tab tab :root root :name new-key})
                                 :else (assoc (dissoc (dissoc (first body) :source-panel) :drag-meta) :tab tab))
                           {:h             height
                            :w             width
                            :root          root
                            :tab           tab
                            :selected-view view-name
                            :name          new-key
                            :views         {view-name [:box :align :center
                                                       :justify :center ;;;:attr {:id (str new-keyw "." :hi)} 
                                                       :style {:font-size    "21px"
                                                               :font-weight  700
                                                               :padding-top  "6px"
                                                               :padding-left "14px"
                                                               :margin-top   "-8px"
                                                               :color        :theme/editor-outer-rim-color ;"#f9b9f9"
                                                               :font-family  :theme/base-font}
                                                       :child (rand-nth quotes)]}
                            :queries       {}})]
      (ut/tracked-dispatch [::update-workspace [new-keyw] base-map])
      (reset! hover-square nil))))



(re-frame/reg-event-db ::new-reco-preview (fn [db [_ hash]] (assoc-in db [:reco-preview] hash)))

(re-frame/reg-sub ::reco-preview-new? (fn [db [_ hash]] (not (= (get db :reco-preview) hash))))

(re-frame/reg-sub ::reco-preview (fn [db [_]] (get db :reco-preview)))

(re-frame/reg-sub ::lookup-mad-libs-row
                  (fn [db [_ combohash]] (first (filter #(= (get % :combo_hash) combohash) (get-in db [:data :recos-sys2])))))

(defn insert-rec-preview-block
  [viz query condis connection-id combohash combo-name shape-name single? panel?]
  (let [new-key       (str "reco-preview" (if (not single?) combohash ""))
        new-keyw      (keyword new-key)
        table-name    @(ut/tracked-sub ::conn/clicked-parameter-key-alpha
                                       {:keypath (if panel? [:viz-tables-sys/table_name] [:viz-tables-sys2/table_name])})
        mad-libs-row  (if (not panel?) @(ut/tracked-subscribe [::lookup-mad-libs-row combohash]) {})
        table-name    (if (keyword? table-name) (ut/replacer (ut/replacer (str table-name) ":" "") "-" "_") table-name)
        condi-keys    (try (keys (read-string condis)) (catch :default _ []))
        condi-walks   (into {}
                            (for [c condi-keys]
                              (let [randy (rand-int 999)]
                                {c                                         (keyword (str "c" randy "-" (ut/safe-name c)))
                                 (keyword (str "condi/" (ut/safe-name c))) (keyword (str "condi/c" randy
                                                                                         "-"       (ut/safe-name c)))})))
        queries       (into {}
                            (for [q (range (count query))]
                              {(keyword (if (= q 0)
                                          (if (not single?) (str "query-preview" combohash) "query-preview")
                                          (str (if (not single?) (str "query-preview" combohash "-") "query-preview-") (+ 1 q))))
                               (nth query q)}))
        queriesrep    (into {}
                            (for [q (range (count query))]
                              {(keyword (if (= q 0) "query-preview" (str "query-preview-" (+ 1 q))))
                               (keyword (if (= q 0)
                                          (str "query-preview" combohash)
                                          (str (str "query-preview" combohash "-") (+ 1 q))))}))
        h             (if panel?
                        @(ut/tracked-sub ::conn/clicked-parameter-key-alpha {:keypath [:recos-sys/h]})
                        (get mad-libs-row :h nil))
        w             (if panel?
                        @(ut/tracked-sub ::conn/clicked-parameter-key-alpha {:keypath [:recos-sys/w]})
                        (get mad-libs-row :w nil))
        selected-view (if panel?
                        (try ;; weirdness here.. why was blowing up as seq? def just a keyword
                          (let [;sv @(ut/tracked-subscribe [::conn/clicked-parameter-key
                                sv @(ut/tracked-sub ::conn/clicked-parameter-key-alpha {:keypath [:recos-sys/selected_view]})]
                            (when (ut/ne? sv) sv))
                          (catch :default _ nil))
                        (when (ut/ne? (get mad-libs-row :selected_view)) (get mad-libs-row :selected_view)))
        selected-view (when selected-view (try (keyword (ut/replacer selected-view #":" "")) (catch :default _ selected-view)))
        selected-view (if (= selected-view "") nil selected-view) ;; wtf
        base-map      (merge {:h                   (if h h 9)
                              :w                   (if w w 9)
                              :mad-libs-combo-hash [combohash table-name]
                              :root                [200 200] ;; [0 0]
                              :name                (str shape-name " - " combo-name " - " (rand-int 1234))
                              :connection-id       connection-id
                              :views               (if (map? viz) viz {:oz viz})
                              :queries             queries}
                             (if selected-view {:selected-view selected-view} {}))
        base-map      (if (nil? condis) base-map (assoc base-map :conditionals (read-string condis)))
        base-map      (ut/postwalk-replacer condi-walks base-map)
        base-map      (if (not single?) (ut/postwalk-replacer queriesrep base-map) base-map)]
    (ut/tracked-dispatch [::update-workspace-noundo [new-keyw] base-map])))


(defn draggable
  [data type element]
  [(reagent/adapt-react-class rdnd/Draggable)
   {:type          type
    :on-drag-end   #(do (reset! dragging? false)
                        (reset! dyn-dropper-hover false)
                        (reset! dragging-size [0 0])
                        (reset! dragging-body false))
    :on-drag-start #(do (reset! dragging? true)
                        (reset! on-block? false)
                        (reset! dragging-body data)
                        (reset! dragging-size [(get data :w) (get data :h)]))
    :data          (pr-str data)} 
   [re-com/box :size "auto" :child element :style {:cursor "grab"}]])

(defn draggable-stub [data type element] [re-com/box :size "auto" :child element])

(defn droppable
  [types-vec root element]
  (if true ;(not @over-flow?) ;; dont trigger if we are cross breeding from flow canvas to
    [(reagent/adapt-react-class rdnd/Droppable)
     {:types types-vec
      :on-drop
      #(let [incoming    (js->clj %)
             _ (ut/tapp>> [:incoming incoming @dragging-body])
             data        (read-string (last (last incoming)))
             data        (ut/postwalk-replacer {:reagent-gt :>} data) ;; unfuck "invalid
             type        (first (first incoming))
             rooted-data (assoc data :root root)
             client-name @(ut/tracked-sub ::conn/client-name {})
             dm-type     (get-in rooted-data [:drag-meta :type])
             ok-new?     (or (and (not @on-block?) (or (false? @dyn-dropper-hover) (nil? @dyn-dropper-hover)))
                             (= dm-type :viz-reco))]
         (when (= dm-type :viz-reco)
           (let [cc          @(ut/tracked-subscribe [::conn/clicked-parameter [:recos-sys]])
                 ;cc          @(ut/tracked-sub ::conn/clicked-parameter-alpha {:keypath [:recos-sys]})
                 combo-keys  (select-keys cc [:context_hash :combo_hash])
                 ;client-name @(ut/tracked-subscribe [::conn/client-name])
                 ]
             (ut/tapp>> [:dropped-viz-reco! data combo-keys cc])
             (ut/tracked-dispatch [::update-reco-previews])
             (ut/tracked-dispatch [::wfx/request :default
                                   {:message {:kind         :selected-reco
                                              :dm-type      :viz-reco
                                              :combo_hash   (get combo-keys :combo_hash)
                                              :context_hash (get combo-keys :context_hash)
                                              :client-name  client-name}
                                    :timeout 50000}])))
         (when (= dm-type :meta-fields)
           (let [;cc @(ut/tracked-subscribe [::conn/clicked-parameter [:recos-sys]])
                 cc          (get rooted-data :drag-meta)
                 ;client-name @(ut/tracked-subscribe [::conn/client-name])
                 ]
             (ut/tapp>> [:dropped-field-usage data cc cc])
             (ut/tracked-dispatch [::wfx/request :default
                                   {:message {:kind :selected-reco :dm-type :meta-fields :drag-meta cc :client-name client-name}
                                    :timeout 50000}])))
         (ut/tapp>> [:dropped type :ok-new? :drag-meta-type dm-type [@on-block? @dyn-dropper-hover] ok-new? rooted-data root])
         (reset! dragging? false)
         (cond (= dm-type :meta-screens) (ut/tracked-dispatch [::http/load (get rooted-data :file-path)])
               (= dm-type :meta-theme)   (ut/tracked-dispatch
                                          [::overwrite-theme
                                           (apply dissoc rooted-data [:block-key :drag-meta :root :file-path])])
               (= dm-type :meta-board)   (ut/tracked-dispatch
                                          [::add-board (apply dissoc rooted-data [:block-key :drag-meta :root :file-path])])
               ok-new? ;(or (nil? @dyn-dropper-hover) (not @on-block?)) ;; dont create new
               (insert-new-block root 4 4 rooted-data)))} [re-com/box :child element]]
    (do ;(reset! dragging? false)
      element)))

(def new-block-atom (reagent/atom nil))

(defn mouse-up-handler-new
  [on-move]
  (fn me [evt]
    (let [root   (get @new-block-atom :root)
          width  (get @new-block-atom :w)
          height (get @new-block-atom :h)]
      (when (and (not (nil? root)) (not (nil? height)) (not (nil? width))) (insert-new-block root width height))
      (reset! new-block-atom nil))
    (gevents/unlisten js/window EventType.MOUSEMOVE on-move)))

(defn mouse-move-handler-new
  [offset orig-x orig-y]
  (fn [evt]
    (let [start-x    (.-clientX evt)
          start-y    (.-clientY evt)
          off-x      (:x offset)
          off-y      (:y offset)
          x          (js/Math.floor (/ (- start-x off-x) db/brick-size)) ;
          y          (js/Math.floor (/ (- start-y off-y) db/brick-size))
          start-root [(js/Math.floor (/ orig-x db/brick-size)) (js/Math.floor (/ orig-y db/brick-size))]
          drag-w     (- x (js/Math.floor (/ orig-x db/brick-size)))
          drag-h     (- y (js/Math.floor (/ orig-y db/brick-size))) ;; bh bw start-h start-w
          drag-wake  @(ut/tracked-sub ::build-grid
                                      {:bh      (+ drag-h (last start-root) 1)
                                       :bw      (+ drag-w (first start-root) 1)
                                       :start-h (last start-root)
                                       :start-w (first start-root)})]
      (reset! new-block-atom {:root [(first start-root) (last start-root)] :w (+ drag-w 1) :h (+ drag-h 1)})
      (reset! hover-square drag-wake))))

(defn mouse-down-handler-new
  [e]
  (let [{:keys [left top]} (get-client-rect e)
        offset             {:x (- (.-clientX e) left) :y (- (.-clientY e) top)}
        orig-x             (.-clientX e)
        orig-y             (.-clientY e)
        on-move            (mouse-move-handler-new offset orig-x orig-y)]
    (gevents/listen js/window EventType.MOUSEMOVE on-move)
    (gevents/listen js/window EventType.MOUSEUP (mouse-up-handler-new on-move))))

(defn resize-mouse-move-handler
  [offset width height]
  (fn [evt]
    (let [block      @(ut/tracked-sub ::selected-block {})
          start-x    (.-clientX evt)
          start-y    (.-clientY evt)
          off-x      (:x offset)
          off-y      (:y offset)
          w          (js/Math.floor (/ (- start-x off-x) db/brick-size))
          h          (js/Math.floor (/ (- start-y off-y) db/brick-size))
          wf         (+ w width 0)
          hf         (+ h height 0)
          selected-h @(ut/tracked-sub ::selected-h {})
          selected-w @(ut/tracked-sub ::selected-w {})
          wmax       (if (< wf 2) 2 wf)
          hmax       (if (< hf 2) 2 hf)]
      (when (not (= hmax selected-h)) (ut/tracked-dispatch [::update-workspace-noundo [block :h] hmax]))
      (when (not (= wmax selected-w)) (ut/tracked-dispatch [::update-workspace-noundo [block :w] wmax])))))

(defn resize-mouse-down-handler
  [e]
  (let [{:keys [left top]} (get-client-rect e)
        block              @(ut/tracked-sub ::selected-block {})
        width              @(ut/tracked-sub ::workspace-alpha {:keypath [block :w]})
        height             @(ut/tracked-sub ::workspace-alpha {:keypath [block :h]})
        offset             {:x (- (.-clientX e) width) :y (- (.-clientY e) height)}
        on-move            (resize-mouse-move-handler offset width height)]
    (do (reset! mouse-dragging-panel? true) (gevents/listen js/window EventType.MOUSEMOVE on-move))
    (gevents/listen js/window EventType.MOUSEUP (mouse-up-handler on-move))))







(defn highlight-code
  [code selected-block data-key editor?]
  (when-let [editor (get @db/cm-instance-panel-code-box [selected-block data-key editor?])]
    (let [doc  (.getDoc editor)
          code (str code)
          _ (ut/tapp>> [:highlighted-code-panel-code-box code])]
      (doseq [marker (get @db/markers-panel-code-box [selected-block data-key editor?])] (.clear marker))
      ;(reset! db/markers-panel-code-box [])
      (swap! db/markers-panel-code-box assoc [selected-block data-key editor?] [])
      (let [code-lines (cstr/split-lines code)
            start-line (loop [line 0]
                         (when (< line (.lineCount doc))
                           (if (cstr/includes? (.getLine doc line) (first code-lines)) line (recur (inc line)))))
            end-line (loop [line start-line]
                       (when (< line (.lineCount doc))
                         (if (cstr/includes? (.getLine doc line) (last code-lines)) line (recur (inc line)))))
            start-ch (cstr/index-of (.getLine doc start-line) (first code-lines))
            end-ch (+ (cstr/index-of (.getLine doc end-line) (last code-lines)) (count (last code-lines)))
            marker
            (try (.markText
                  doc
                  #js {:line start-line :ch start-ch}
                  #js {:line end-line :ch end-ch}
                  #js {:css
                       (str
                        "filter: invert(233%); color: white; font-weight: 700; background-color: teal; font-size: 15px;")})
                 (catch :default e (ut/tapp>> [:marker-error (str e)])))]
        ;(swap! db/markers-panel-code-box conj marker)
        (swap! db/markers-panel-code-box assoc [selected-block data-key editor?]
               (conj (get @db/markers-panel-code-box [selected-block data-key editor?]) marker))))))






(def widget (atom nil))



(declare custom-hint-fn)






(defonce param-hover (reagent/atom nil))

(defn highlight-codes-only
  [codes selected-block data-key editor? css]
  (when-let [editor (get @db/cm-instance-panel-code-box [selected-block data-key editor?])]
    (let [doc (.getDoc editor)]
        ;;(tapp>> [:running-highlight-codes-only data-key])
      (doseq [marker (get @db/markers-panel-code-box [selected-block data-key editor?])] (.clear marker))
        ;(reset! db/markers-panel-code-box [])
      (swap! db/markers-panel-code-box assoc [selected-block data-key editor?] [])
      (doseq [code codes]
        (let [raw-code   (try (edn/read-string code) (catch :default _ code))
              code       (str code)
              value      @(ut/tracked-sub ::conn/clicked-parameter-key-alpha {:keypath [raw-code]})
              vtype      (ut/data-typer value)
              vcolor     (get (theme-pull :theme/data-colors nil) vtype)
              code       (str code)
              code-lines (cstr/split-lines code)]
          (loop [line 0]
            (when (< line (.lineCount doc))
              (when (cstr/includes? (.getLine doc line) (first code-lines))
                (let [start-line         line
                      end-line           (loop [line start-line]
                                           (when (< line (.lineCount doc))
                                             (if (cstr/includes? (.getLine doc line) (last code-lines))
                                               line
                                               (recur (inc line)))))
                      code-parts         (let [ff (vec (cstr/split code #"/"))
                                               vv (vec (cstr/split (get ff 1) #">"))
                                               cc (vec (into [(first ff)] vv))]
                                           cc)
                      full-code-parts    code-parts
                      code-part-start-ch (cstr/index-of (.getLine doc start-line) (first full-code-parts))
                      code-part-end-ch   (+ (cstr/index-of (.getLine doc start-line) (last full-code-parts))
                                            (count (last full-code-parts)))]
                  (loop [code-parts code-parts
                         start-ch   0]
                    (when (seq code-parts)
                      (let [code-part (first code-parts)
                            start-ch (cstr/index-of (.getLine doc start-line) code-part start-ch)
                            end-ch (+ start-ch (count code-part))
                            marker
                            (try
                              (let [node (js/document.createElement "span")
                                    _ (.setAttribute node
                                                     "style"
                                                     (ut/hiccup-css-to-string
                                                      (merge css
                                                             (when (= (str code) (str @db/param-code-hover))
                                                               {:box-shadow (str "0 0 10px 5px "
                                                                                 (ut/invert-hex-color
                                                                                  (theme-pull :theme/editor-outer-rim-color nil)))})
                                                             (when vcolor
                                                               {;:color "black"  ;;vcolor
                                                                  ;:border (when (= raw-code (first @param-hover)) (str "3px dashed " vcolor))
                                                                :box-shadow  (when (= raw-code (first @param-hover)) "2px 2px 20px #FF06B5")
                                                                :background-color (ut/invert-hex-color vcolor)}))))
                                    _ (.appendChild node (js/document.createTextNode code-part))]
                                (.addEventListener
                                 node
                                 "click"
                                 (fn []
                                   (let [this-token-idx (.indexOf full-code-parts code-part)
                                         token-parts    (subvec full-code-parts 0 this-token-idx)
                                         seeded-token   (str (first token-parts) "/" (cstr/join ">" (rest token-parts)))]
                                     (.setCursor editor #js {:line start-line :ch code-part-start-ch})
                                     (.showHint editor
                                                #js {:completeSingle false
                                                     :hint           (fn []
                                                                       (custom-hint-fn editor
                                                                                       seeded-token
                                                                                       start-line
                                                                                       code-part-start-ch
                                                                                       code-part-end-ch))}))))
                                (.addEventListener node
                                                   "mouseover"
                                                   (fn []
                                                     (let [code   (try (edn/read-string code) (catch :default _ code))
                                                           psplit (ut/splitter (ut/safe-name (str code)) "/")
                                                           table  (-> (first psplit)
                                                                      (ut/replacer #":" "")
                                                                      (ut/replacer ".*" "")
                                                                      keyword)
                                                           field  (keyword (last psplit))]
                                                       (reset! param-hover [code table field]))))
                                (.addEventListener node
                                                   "mouseenter"
                                                   (fn []
                                                     (let [code   (try (edn/read-string code) (catch :default _ code))
                                                           psplit (ut/splitter (ut/safe-name (str code)) "/")
                                                           table  (-> (first psplit)
                                                                      (ut/replacer #":" "")
                                                                      (ut/replacer ".*" "")
                                                                      keyword)
                                                           field  (keyword (last psplit))]
                                                       (reset! param-hover [code table field]))))
                                (.addEventListener node "mouseout" (fn [] (reset! param-hover nil)))
                                (.addEventListener node "mouseleave" (fn [] (reset! param-hover nil)))
                                (.markText doc
                                           #js {:line start-line :ch start-ch}
                                           #js {:line end-line :ch end-ch}
                                           #js {:replacedWith node}))
                              (catch :default e (ut/tapp>> [:marker-error (str e)])))]
                        (when (cstr/includes? (.getLine doc start-line) code-part)
                            ;(swap! db/markers-panel-code-box conj marker)
                          (swap! db/markers-panel-code-box assoc [selected-block data-key editor?]
                                 (conj (get @db/markers-panel-code-box [selected-block data-key editor?]) marker)))
                        (recur (rest code-parts) end-ch))))))
              (recur (inc line)))))))))


(defn highlight-codes-values
  [codes selected-block data-key editor? css]
  (when-let [editor (get @db/cm-instance-panel-code-box [selected-block data-key editor?])]
    (let [doc (.getDoc editor)]
        ;(tapp>> [:running-highlight-codes-values data-key])
      (doseq [marker (get @db/markers-panel-code-box [selected-block data-key editor?])] (.clear marker))
        ;(reset! db/markers-panel-code-box [])
      (swap! db/markers-panel-code-box assoc [selected-block data-key editor?] [])
      (doseq [[code value] codes]
        (let [raw-code   (try (edn/read-string code) (catch :default _ code))
              code       (str code)
              vtype      (ut/data-typer value)
              vcolor     (get (theme-pull :theme/data-colors nil) vtype)
                ;;_ (tapp>> [:vtype vcolor vtype])
              value      (if (string? value) (pr-str value) value)
              code-lines (cstr/split-lines code)
              psplit        (ut/splitter (str code) "/")
              table         (-> (first psplit)
                                (ut/replacer #":" "")
                                (ut/replacer ".*" "")
                                keyword)
              field         (keyword (last psplit))]
          (loop [line 0]
            (when (< line (.lineCount doc))
              (when (cstr/includes? (.getLine doc line) (first code-lines))
                (let [start-line         line
                      end-line           (loop [line start-line]
                                           (when (< line (.lineCount doc))
                                             (if (cstr/includes? (.getLine doc line) (last code-lines))
                                               line
                                               (recur (inc line)))))
                      code-part-start-ch (cstr/index-of (.getLine doc start-line) (first code-lines))
                      code-part-end-ch   (+ (cstr/index-of (.getLine doc start-line) (last code-lines))
                                            (count (last code-lines)))
                      react!     [@param-hover]
                      node               (js/document.createElement "span")
                      _ (.setAttribute node "style" (ut/hiccup-css-to-string (merge
                                                                              css
                                                                              (let [] ;; [_ (tapp>> [:psplit table field code (str @param-hover)])]
                                                                                {})
                                                                              (when vcolor
                                                                                {;:color "black"  ;;vcolor
                                                                                   ;:border (when (= raw-code (first @param-hover)) (str "3px dashed " vcolor))
                                                                                 :box-shadow  (when (= raw-code (first @param-hover)) "2px 2px 20px #FF06B5")
                                                                                 :background-color (ut/invert-hex-color vcolor)}))))
                      _ (.addEventListener node "mouseenter" (fn [_] (reset! param-hover [raw-code table field])))
                      _ (.addEventListener node "mouseleave" (fn [_] (reset! param-hover nil)))
                      _ (.appendChild node (js/document.createTextNode value))
                      marker             (.markText doc
                                                    #js {:line start-line :ch code-part-start-ch}
                                                    #js {:line end-line :ch code-part-end-ch}
                                                    #js {:replacedWith node})]
                    ;(swap! db/markers-panel-code-box conj marker)
                  (swap! db/markers-panel-code-box assoc [selected-block data-key]
                         (conj (get @db/markers-panel-code-box [selected-block data-key editor?]) marker))))
              (recur (inc line)))))))))



;; (defn unhighlight-code
;;   []
;;   (when-let [editor @db/cm-instance-panel-code-box]
;;     (let [doc (.getDoc editor)]
;;       (doseq [marker @db/markers-panel-code-box] (when marker (.clear marker)))
;;       (reset! db/markers-panel-code-box []))))





;; (re-frame/reg-sub ::parameters-available ;;; old logic
;;                   (fn [db _]
;;                     (let [;;codes [:theme/base-font :font-family :height]
;;                           server-params (get-in db [:autocomplete :clover-params] [])
;;                           view-codes    (get-in db [:autocomplete :view-keywords] [])
;;                           flow-subs     (get db :flow-subs)
;;                           click-params  (vec (for [e (keys (get-in db [:click-param :param]))]
;;                                                (keyword (str "param/" (ut/replacer (str e) ":" "")))))
;;                           themes        (vec (for [e (keys (get-in db [:click-param :theme]))]
;;                                                (keyword (str "theme/" (ut/replacer (str e) ":" "")))))
;;                           codes         (vec (apply concat [server-params view-codes themes flow-subs click-params])) ;; not
;;                          ]
;;                       codes)))

(defn get-surrounding-tokens
  [cm cursor token]
  (try (let [line             (.-line cursor)
             token            (try (.-string token) (catch :default _ (str token)))
             line-string      (.getLine cm line)
             prev-line-string (if (> line 0) (.getLine cm (dec line)) "")
             next-line-string (if (< line (dec (.lineCount cm))) (.getLine cm (inc line)) "")
             big-line-string  (str prev-line-string " " line-string " " next-line-string)]
         [token big-line-string])
       (catch :default e (ut/tapp>> [:err (str e)]))))



(defn autocomplete-suggestions-fn
  [input all-keywords]
  (let [end-with-gt? (cstr/ends-with? input ">")
        input-clean  (if end-with-gt? (subs input 0 (dec (count input))) input)
        input-parts  (ut/splitter input-clean #">")
        input-depth  (count input-parts)]
    (->> all-keywords
         (filter #(cstr/starts-with? % input-clean))
         (map (fn [keyword]
                (let [k-parts     (ut/splitter keyword #">")
                      shown-depth (if end-with-gt? (inc input-depth) input-depth)]
                  [(cstr/join ">" (take shown-depth k-parts)) (> (count k-parts) shown-depth)])))
         ((fn [x] (set x)))
         (map (fn [[path has-more]] (if has-more (str path ">") path)))
         (vec))))

(defn autocomplete-suggestions
  [input all-keywords]
  (if (not (cstr/ends-with? input ">"))
    (let [regs   (first (filter #(and (not= % input) (cstr/starts-with? % input)) all-keywords))
          match? (cstr/starts-with? (ut/replacer regs input "") ">")]
      (if match? (autocomplete-suggestions-fn (str input ">") all-keywords) (autocomplete-suggestions-fn input all-keywords)))
    (autocomplete-suggestions-fn input all-keywords)))



















(defn draggable-clover [element data runner & [nname vname h w]]
  [(reagent/adapt-react-class rdnd/Draggable)
   (let [view-name (ut/safe-name (or vname :clover))
         data {:h         (or h 4)
               :w         (or w 7)
               ;:drag-meta {:source-table :hi :table-fields [:*] :connection-id nil :source-panel-key :block-7034 :type :view}
               runner     {view-name data}
               :name      (or nname (str "dragged-clover"))}]
     {:type          "meta-menu" ;:play-port
      :on-drag-end   #(do (reset! dragging? false))
      :on-drag-start #(do (reset! dragging? true)
                          (reset! dragging-size [(get data :w) (get data :h)])
                          (reset! dragging-body data))
      :data          (pr-str data)})
   [re-com/box
    :size "none"
    :child element
    :style {:cursor "grab"}]])









;;(def last-view-highlighted-hash (atom {}))

(re-frame/reg-sub ::panel-code-up?
                  (fn [_ _]

                    ;; (some true?
                    ;;       (for [kvec (keys @db/cm-instance-panel-code-box)
                    ;;             :let [[selected-block data-key editor?] kvec]]
                    ;;         (let [editor-up?          (get db :editor? false)
                    ;;               part-kp             @(ut/tracked-sub ::view-type {:panel-key selected-block :view data-key})
                    ;;               ;;part-kp            @(ut/tracked-sub ::editor-panel-selected-view {})
                    ;;               v-spy               (get @db/value-spy selected-block)
                    ;;               ;view-code-hash     (hash [(get-in db [:panels selected-block v-spy]) part-kp])
                    ;;               ;;view-code-hash    (hash [(get-in db [:panels selected-block part-kp]) ])
                    ;;               view-code-hash      (hash [(get-in db [:panels selected-block]) part-kp])
                    ;;               old-view-code-hash  (get @last-view-highlighted-hash kvec)

                    ;;               ]
                    ;;           ;(and editor? (not= view-code-hash old-view-code-hash))
                    ;;           ;; (and (not= view-code-hash old-view-code-hash)
                    ;;           ;;      (if editor-up? editor? true))
                    ;;           (or (not= view-code-hash old-view-code-hash) v-spy)
                    ;;           )
                    ;;         ))

                    ;; (some true? (for [kvec (keys @db/cm-instance-panel-code-box)
                    ;;                   :let [[selected-block data-key _] kvec
                    ;;                         v-spy (get @db/value-spy [selected-block data-key])]]
                    ;;               v-spy))

                    ;;true
                    false

                    ;;true
                    ;;false
                    ;;(get db :editor? false)
                    ))


(defn custom-hint-simple-fn
  [cm input-token]
  (let [cursor (.getCursor cm)
        token  (.getTokenAt cm cursor)]
    (let [input         (.-string token) ;;(or input-token (.-string token))
          input         (if (string? input-token) input-token input)
          list          @db/autocomplete-keywords ;;(vec (map str @(rfa/sub
          filtered-list (sort (autocomplete-suggestions input list))]
      (clj->js {:list filtered-list
                :from (clj->js {:line (.-line cursor) :ch (.-start token)})
                :to   (clj->js {:line (.-line cursor) :ch (.-end token)})}))))

(defn custom-hint-fn
  [cm input-token & [start-line start-ch end-ch]]
  (let [cursor        (clj->js {:line start-line :ch start-ch}) ;; Create a cursor object from
        token         (.getTokenAt cm cursor)
        input         (if (string? input-token) input-token (.-string token)) ;; Use input-token
        list          @db/autocomplete-keywords ;; (vec (map str @(rfa/sub
        filtered-list (sort (autocomplete-suggestions input list))]
    (clj->js {:list  filtered-list
              :from  cursor
              :to    (clj->js {:line start-line :ch end-ch}) ;; Use start-line and end-ch to create
              :pick  (fn [cm hint]
                       (let [doc                (.getDoc cm)
                             replacement-end-ch (+ start-ch (count hint))]
                         (.replaceRange doc hint cursor (clj->js {:line start-line :ch end-ch}))
                         (js/setTimeout #(do (.setSelection doc
                                                            (clj->js {:line start-line :ch replacement-end-ch})
                                                            (clj->js {:line start-line :ch replacement-end-ch})
                                                            (clj->js {:scroll false})))
                                        10)))
              :close (fn [cm] (js/setTimeout #(do (.focus cm)) 100))})))

(defn can-be-autocompleted?
  [token-string]
  (let [list @db/autocomplete-keywords
        fff (filter #(cstr/starts-with? % token-string) list)
        can? (and (> (count fff) 1) (ut/ne? fff))
        _ (ut/tapp>> [:can-be-autocompleted? can? (count fff) token-string])]
    can?))


(defn panel-string-box
  ;;[_ width-int height-int value syntax]
  [bid view-kp width-int height-int value syntax & [repl? editor? src-block-override vs?]]
  (let [syntax     (or (if (= syntax "raw (clojure)") "clojure" syntax) "clojure")
        stringify? true ;;(not (= syntax "clojure"))
        bid (bid) ;; lord have mercy. thanks for being "optimized" here, react
        view-kp (view-kp)
        selected-block     @(ut/tracked-sub ::selected-block {})
        on-focus-fn (fn [_]
                      (reset! db/cm-focused? true)
                      (reset! db/view-title-edit-idx nil)
                      (reset! db/last-focused (if editor? ;; such a fucking dumb workaround for react being thrifty on some components... but hey. we roll.
                                                [(or src-block-override selected-block) ;; src-block-override used only for :editor SELF IN-EDITOR.... 
                                                        ;; ^^ (so we always target the *target* and not ourselves) meta within meta, my dude.
                                                 [@(ut/tracked-sub ::view-type {:panel-key bid :view (get @db/data-browser-query bid)}) (get @db/data-browser-query bid)]]
                                                [bid view-kp])))]
    [re-com/box
     :size "auto"
     :padding "8px"
     ;:width (px (- width-int 24))
     ;:height (px (- height-int 24))
     :style {:font-family      (if (= syntax "text")
                                 (theme-pull :theme/base-font nil)
                                 (theme-pull :theme/monospaced-font nil))
             ;;:color           (theme-pull :theme/editor-outer-rim-color nil)
             :font-size        "16px"
             :overflow         "auto"
             :background-color "#00000000"
             :font-weight      700}
     :child [(reagent/adapt-react-class cm/UnControlled)
             {;; :value   (ut/format-map (- width-int 24)
              ;; :value   (if stringify?
              ;;            (try (str (cstr/join "\n" value)) (catch :default _ (str value)))
              ;;            (ut/format-map (- width-int 24) (str value)))
              :value (if (string? value) (str value) (try (str (cstr/join "\n" value)) (catch :default _ (str value))))
              :onBlur  #(let [rr  (ut/cm-deep-values %)
                              rrd (try (read-string (cstr/join " " rr)) (catch :default _ (pr-str (cstr/join "\n" rr))))
                              ;selected-kp  @(ut/tracked-subscribe [::editor-panel-selected-view])
                              selected-kp  @(ut/tracked-sub ::editor-panel-selected-view {})
                              selected-kp  (if (nil? (first selected-kp)) nil selected-kp)]
                          (ut/tapp>> [:string-input stringify? selected-kp  rr rrd value])
                          (reset! db/cm-focused? false)
                          (when (not= value rr)
                            (ut/tracked-dispatch-sync [::update-selected-key-cons selected-kp rr])))
              :onFocus        on-focus-fn
              :options {:mode              syntax
                        :lineWrapping      true
                        :lineNumbers       false ;true
                        :matchBrackets     true
                        :autoCloseBrackets true
                        :autofocus         false
                        :autoScroll        false
                        :detach            true
                        :readOnly          false ;true
                        :theme             (theme-pull :theme/codemirror-theme nil)}}]]))

(defn edn-box [width-int height-int value]
  (let [syntax      "clojure"
        code-width  (- width-int 24)]
    [re-com/box
     :size "auto"
     :padding "8px"
     :height (px (- height-int 14))
     :style {:font-family      (if (= syntax "text")
                                 (theme-pull :theme/base-font nil)
                                 (theme-pull :theme/monospaced-font nil))
             :font-size        "16px"
             :overflow         "auto"
             :background-color "#00000000"
             :font-weight      700}
     :child [(reagent/adapt-react-class cm/UnControlled)
             {:value (try (ut/format-map code-width (str value)) (catch :default _ value))
              :options {:mode              syntax
                        :lineWrapping      true
                        :lineNumbers       false ;true
                        :matchBrackets     true
                        :autoCloseBrackets true
                        :autofocus         false
                        :autoScroll        false
                        :detach            true
                        :readOnly          true
                        :theme             (theme-pull :theme/codemirror-theme nil)}}]]))

(defn meta-edn-box [width-int height-int value]
  (let [syntax      "clojure"
        code-width  (- width-int 24)]
    [re-com/box
     :size "auto"
     :padding "8px"
     :height (px (- height-int 14))
     :style {:font-family      (if (= syntax "text")
                                 (theme-pull :theme/base-font nil)
                                 (theme-pull :theme/monospaced-font nil))
             :font-size        "12px"
             :overflow         "auto"
             :background-color "#00000000"
             :font-weight      700}
     :child [(reagent/adapt-react-class cm/UnControlled)
             {:value (try (ut/format-map code-width (str value)) (catch :default _ value))
              :options {:mode              syntax
                        :lineWrapping      true
                        :lineNumbers       false ;true
                        :matchBrackets     true
                        :autoCloseBrackets true
                        :autofocus         false
                        :autoScroll        false
                        :detach            true
                        :readOnly          true
                        :theme             (theme-pull :theme/codemirror-theme nil)}}]]))




;; (defn handle-focus-change [key ref old-state new-state] ;; debug mostly, react can be a sloped roof
;;   (tapp>> ["Focus changed from" (str old-state) "to" (str new-state)]))

;; (add-watch db/last-focused :focus-change-watcher handle-focus-change)

(add-watch db/value-spy :value-spy-watcher (fn []
                                             ;(reset! db/cm-instance-panel-code-box {})
                                             ;(reset! db/markers-panel-code-box {})
                                             (ut/tracked-dispatch [::highlight-panel-code])))

(defn strip-do [s]
  (let [s (cstr/trim (str s))]
    (subs s 3 (dec (count s)))))

(defn panel-code-box [bid view-kp width-int height-int value & [repl? editor? src-block-override vs?]]
  (let [;int (atom [bid view-kp])
    ;sql-hint?   (cstr/includes? (str value) ":::sql-string")
        ;selected-kp @(ut/tracked-sub ::editor-panel-selected-view {})
        ;selected-kp (if (nil? (first selected-kp)) nil selected-kp)
        ;block-runners-map  @(ut/tracked-sub ::block-runners {})
        selected-block     @(ut/tracked-sub ::selected-block {})
        ;editor?  (true? editor?)
        ;;vs? (vs?) ;; reaction 
        bid (bid) ;; lord have mercy. thanks for being "optimized" here, react
        view-kp (view-kp)
        ;view-kp ((fn [] view-kp))

        force-react! [@db/data-browser-query @db/value-spy]
        base-edit?   false ;;(some #(= % :*) view-kp) ;;      (= (last (last @db/last-focused)) :*) ;; somewhat redundant, but we need this out outside to get fontsize
        ;;[h w] @(ut/tracked-sub ::size-alpha {:panel-key bid})
        ;selected-view-type @(ut/tracked-sub ::view-type {:panel-key selected-block :view selected-kp})
        ;; repl? (and (not= (get @db/data-browser-query selected-block) :*)
        ;;            (true? (cstr/includes? (str selected-view-type) "clojure")))
        ;syntax (get-in block-runners-map [selected-view-type :syntax])
        ;;_ (tapp>>  [:stiff (=  (get @db/data-browser-query selected-block) :*) (str selected-kp) selected-block  @db/data-browser-query selected-view-type])
        ;key         selected-kp
        ;;repl?       (true? repl?)
        ;[ph pw th tw] [(Math/floor (* h db/brick-size)) (Math/floor (* w db/brick-size)) (Math/floor height-int) (Math/floor width-int)]
        ;editor?       (not= [ph pw] [th tw])
        font-size     (if base-edit? 13 17)
        value         (if (and repl? (cstr/starts-with? (cstr/trim (str value)) "(do"))
                        (try (str (strip-do value))
                             (catch :default _ value))
                        value)  ;; remove implied DO
        code-width  (if base-edit? (- width-int 24) (- width-int 130)) ;; diff width for :* raw editor
        on-focus-fn (fn [_]
                      (reset! db/cm-focused? true)
                      (reset! db/view-title-edit-idx nil)
                      (reset! db/last-focused (if editor? ;; such a fucking dumb workaround for react being thrifty on some components... but hey. we roll.
                                                [(or src-block-override selected-block) ;; src-block-override used only for :editor SELF IN-EDITOR.... 
                                                ;; ^^ (so we always target the *target* and not ourselves) meta within meta, my dude.
                                                 [@(ut/tracked-sub ::view-type {:panel-key bid :view (get @db/data-browser-query bid)}) (get @db/data-browser-query bid)]]
                                                [bid view-kp])))
        on-blur-fn  (fn [x] ;; some reptition inside from the wrapper, due to react non-reactness in this situation
                      (let [_ (reset! db/cm-focused? false) ;; best to do all this at save time, since passing values from outside can be... a problem
                            ;selected-block     bid ;(or bid @(ut/tracked-sub ::selected-block {}))
                            ;selected-kp        view-kp ;(or view-kp @(ut/tracked-sub ::editor-panel-selected-view {})) ;; @(ut/tracked-subscribe [::editor-panel-selected-view])
                            [selected-block selected-kp] @db/last-focused
                            selected-view-type (first selected-kp) ;; @(ut/tracked-sub ::view-type {:panel-key selected-block :view selected-kp})
                            base-edit?         (= (last selected-kp) :*) ;; (or (=  (get @db/data-browser-query selected-block) :*) (last ))
                            ;;_ (tapp>> [:panel-code-update (str @db/last-focused) :vs (str [bid view-kp])])
                            repl? (and (not base-edit?)
                                       (or repl?
                                           (true?
                                            (and (not base-edit?) ;; (not= (get @db/data-browser-query selected-block) :*)
                                                 (not= selected-view-type :queries)
                                                 (not= selected-view-type :views)
                                                 (true? (cstr/includes? (str selected-view-type) "clojure"))))))
                            parse        (if repl?

                                           (try (read-string (str "(do " (cstr/join " " (ut/cm-deep-values x)) ")"))
                                                (catch :default e [:cannot-parse (str (.-message e))]))

                                           (try (read-string (cstr/join " " (ut/cm-deep-values x)))
                                                (catch :default e [:cannot-parse (str (.-message e))])))
                            selected-kp  (if (nil? (first selected-kp)) nil selected-kp)
                            update-kp    (if base-edit?
                                           [:panels selected-block]
                                           (into [:panels selected-block] selected-kp))
                            unparseable? (= (first parse) :cannot-parse)]
                        (if unparseable?
                          (do (reset! db/bad-form? true) (reset! db/bad-form-msg (str (last parse))))
                          (do (reset! db/bad-form? false)
                              (ut/tracked-dispatch-sync [::update-workspace-raw update-kp parse])
                              ;(ut/dispatch-delay [::highlight-panel-code] 200)
                              ))))

        on-before-change-fn (fn [editor _ _] ;; data value]
                              (ut/tracked-dispatch [::highlight-panel-code])
                              (swap! db/cm-instance-panel-code-box assoc [bid (last view-kp) editor?] editor))]
    ;;(tapp>> [:ss (str @tt) ])
    ;;(tapp>> [:code repl? (cstr/starts-with? (cstr/trim (str value)) "(do") value])
    [re-com/box :size "none" :width (px (- width-int 24)) :height (px (- height-int 24))
     :style {:font-family      (theme-pull :theme/monospaced-font nil) ; "Chivo Mono" ;"Fira Code"
             :font-size        (px font-size)
             :overflow         "auto"
             ;:border           "1px solid orange"
             :background-color (if @db/bad-form? "#8b000075" "inherit")
             :border-radius    "12px"
             :font-weight      700} :child
     [(reagent/adapt-react-class cm/UnControlled)
      {:value          (ut/format-map code-width (str value)) ;; value will be pre filtered by caller
       :onBeforeChange on-before-change-fn
       :onFocus        on-focus-fn
       :onBlur         on-blur-fn
      ;;  :onInputRead    (fn [cm]
      ;;                    (let [cursor       (.getCursor cm)
      ;;                          token        (.getTokenAt cm cursor)
      ;;                          token-string (.-string token)
      ;;                          token-end    (= (.-ch cursor) (.-end token))]
      ;;                      (when (or (= token-string ":")
      ;;                                ;;(= token-string ">") ;; this didnt work
      ;;                                (and token-end (can-be-autocompleted? token-string)))
      ;;                        (js/setTimeout (fn [] (.execCommand cm "autocomplete")) 0))))
      ;;  :onCursorActivity (fn [cm]
      ;;                      (let [cursor       (.getCursor cm)
      ;;                            line         (.-line cursor)
      ;;                            ch           (.-ch cursor)
      ;;                            token        (.getTokenAt cm (clj->js {:line line :ch (dec ch)}))
      ;;                            token-string (.-string token)
      ;;                            token-end    (= ch (.-end token))]
      ;;                        (when (or (= token-string ":")
      ;;                                  (and token-end (can-be-autocompleted? token-string)))
      ;;                          (js/setTimeout (fn [] (.execCommand cm "autocomplete")) 0))))
       :onCursorActivity (fn [cm]
                           (let [cursor       (.getCursor cm)
                                 line         (.-line cursor)
                                 ch           (.-ch cursor)
                                 token        (.getTokenAt cm (clj->js {:line line :ch (dec ch)}))
                                 token-string (.-string token)
                                 token-end    (= ch (.-end token))]
                             (when (and token-end
                                        (cstr/starts-with? token-string ":")
                                        (can-be-autocompleted? token-string))
                               (js/setTimeout (fn [] (.execCommand cm "autocomplete")) 0))))
      ;;  :onCursorActivity    (fn [cm]
      ;;                         (let [cursor       (.getCursor cm)
      ;;                               token        (.getTokenAt cm cursor)
      ;;                               token-string (.-string token)
      ;;                               token-end    (= (.-ch cursor) (.-end token))]
      ;;                           (when (or (= token-string ":")
      ;;                                ;;(= token-string ">") ;; this didnt work
      ;;                                     (and token-end (can-be-autocompleted? token-string)))
      ;;                             (js/setTimeout (fn [] (.execCommand cm "autocomplete")) 0))))
       :options        {:mode              "clojure" ;;(if sql-hint? "sql" "clojure")
                        :hintOptions       {:hint custom-hint-simple-fn :completeSingle false}
                        :lineWrapping      true
                        :lineNumbers       true
                        :matchBrackets     true
                        :autoCloseBrackets true
                        :autofocus         false
                        :autoScroll        false
                        :detach            true
                        :readOnly          false
                        :extraKeys         (clj->js
                                            {"Ctrl-Right"       (fn [^js cm]
                                                                  (let [cursor (.getCursor cm)
                                                                        match  (.findMatchingBracket cm cursor true)]
                                                                    (if (and match (.-match match))
                                                                      (.setCursor cm
                                                                                  (clj->js {:line (.-line (.-to match))
                                                                                            :ch   (+ (.-ch (.-to match)) 1)}))
                                                                      (.execCommand cm "goGroupRight"))))
                                             "Shift-Ctrl-Right" (fn [^js cm]
                                                                  (let [cursor (.getCursor cm)
                                                                        match  (.findMatchingBracket cm cursor true)]
                                                                    (if (and match (.-match match))
                                                                      (.setSelection cm
                                                                                     cursor
                                                                                     (clj->js {:line (.-line (.-to match))
                                                                                               :ch   (+ (.-ch (.-to match)) 1)}))
                                                                      (.execCommand cm "goGroupRight"))))
                                             "Ctrl-Left"        (fn [^js cm]
                                                                  (let [cursor (.getCursor cm)
                                                                        match  (.findMatchingBracket cm cursor true)]
                                                                    (if (and match (.-match match))
                                                                      (.setCursor cm (.-to match))
                                                                      (.execCommand cm "goGroupLeft"))))
                                             "Shift-Ctrl-Left"  (fn [^js cm]
                                                                  (let [cursor (.getCursor cm)
                                                                        match  (.findMatchingBracket cm cursor true)]
                                                                    (if (and match (.-match match))
                                                                      (.setSelection cm cursor (.-to match))
                                                                      (.execCommand cm "goGroupLeft"))))})
                        :theme             (theme-pull :theme/codemirror-theme nil)}}]]))




(declare map-boxes2)

(re-frame/reg-sub
 ::clover-kpw-set
 (fn [db {:keys [selected-block selected-view-type data-key]}]
   (let [clover-kpw     (filter #(and (keyword? %) (cstr/includes? (str %) "/"))
                                (ut/deep-flatten (get-in db [:panels selected-block selected-view-type data-key])))
         clover-kpw-set (set (map str clover-kpw))]
     clover-kpw-set)))

(re-frame/reg-sub
 ::relevant-tab-panels-set
 (fn [db _]
   (let [panels-map (get db :panels)
         selected-tab (get db :selected-tab)]
     (set (keys (only-relevant-tabs panels-map selected-tab))))))


;;; do this for every code panel visible based on the db/ atom map
(re-frame/reg-event-db
 ::highlight-panel-code
 (fn [db [_ & [vs?]]]
   ;;(tapp>> [:highlight-panel-code! @(ut/tracked-sub ::relevant-tab-panels {})])
   (let [visible-panel-keys @(ut/tracked-sub ::relevant-tab-panels-set {})]
     (doseq [kvec (filter #(visible-panel-keys (first %)) (keys @db/cm-instance-panel-code-box))]
       (let [;flow-subs      (get db :flow-subs)
           ;[_ data-key]   @(ut/tracked-sub ::editor-panel-selected-view {})
           ;selected-block (get db :selected-block)

             [selected-block data-key editor?] kvec


             value-spy?     (get-in @db/value-spy [selected-block data-key] false)

          ;; click-params   (vec (for [e (keys (get-in db [:click-param :param]))]
          ;;                       (keyword (str "param/" (ut/replacer (str e) ":" "")))))
          ;; themes         (vec (for [e (keys (get-in db [:click-param :theme]))]
          ;;                       (keyword (str "theme/" (ut/replacer (str e) ":" "")))))

          ;; instead of looking for all the possible things, why not just look at what is here and if it applies?
             selected-view-type @(ut/tracked-sub ::view-type {:panel-key selected-block :view data-key})

          ;;  clover-kpw     (filter #(and (keyword? %) (cstr/includes? (str %) "/"))
          ;;                         (ut/deep-flatten (get-in db [:panels selected-block selected-view-type data-key])))
          ;;  clover-kpw-set (set (map str clover-kpw))
             clover-kpw-set @(ut/tracked-sub ::clover-kpw-set {:selected-block selected-block :selected-view-type selected-view-type :data-key data-key})
             ;interspace     (cset/intersection clover-kpw-set (set @db/autocomplete-keywords))
             interspace     (set (into (cset/intersection clover-kpw-set (set @db/autocomplete-keywords)) (filter #(cstr/starts-with? (str %) ":time/") clover-kpw-set)))
             ;interspace     (set (into clover-kpw-set (set @db/autocomplete-keywords)))
        ;;  interspace     (set (into (vec
        ;;                         (apply concat
        ;;                                (for [[k v] (get db :click-param)
        ;;                                      :when (not (cstr/includes? (str k) "-sys"))]
        ;;                                  (for [kk (keys v)] (keyword (cstr/replace (str k "/" kk)  ":" ""))))))
        ;;                        (vec interspace)))
         ;interspace (if value-spy? 
         ;             plus-params
         ;             interspace)
          ;_ (tapp>> [:insersec (map edn/read-string interspace)])

             codes          (vec interspace)  ;; (vec (into themes (into flow-subs click-params)))

          ; part-kp        @(ut/tracked-sub ::editor-panel-selected-view {})
          ; view-code-hash (hash [(get-in db [:panels (get db :selected-block)]) part-kp])

           ;;part-kp        selected-view-type ;;@(ut/tracked-sub ::editor-panel-selected-view {})
           ;;view-code-hash (hash [(get-in db [:panels selected-block]) part-kp])

             inv-backgrd-color (str (ut/invert-hex-color (get (theme-pull :theme/data-colors nil) "keyword")))
           ;inv-color (ut/invert-hex-color  inv-backgrd-color)
             ]

       ;;(swap! last-view-highlighted-hash assoc kvec view-code-hash)
        ;;  (tapp>> [:ran-update-panel-highlight-code-on (str kvec)])

         (if (not value-spy?)
           (highlight-codes-only codes selected-block data-key editor?
                                 {:color            (ut/choose-text-color
                                                     inv-backgrd-color
                                                     ;(get (theme-pull :theme/data-colors nil) "keyword")
                                                     ) ;;inv-color
                                  :background-color inv-backgrd-color
                                  :text-shadow      "none"
                                  :filter           "invert(1.2)"
                                  :cursor           "pointer"
                                  :border-radius    "5px"})
           (highlight-codes-values (into {}
                                         (for [c codes]
                                           {c @(ut/tracked-sub ::conn/clicked-parameter-key-alpha
                                                               {:keypath [c
                                                                       ;(try (edn/read-string c) (catch :default _ c))
                                                                          ]})}))
                                   selected-block data-key editor?
                                   {:color            (ut/choose-text-color
                                                       inv-backgrd-color
                                                       ;(get (theme-pull :theme/data-colors nil) "keyword")
                                                       ) ;; inv-color
                                    :background-color inv-backgrd-color
                                    :text-shadow      "none"
                                    :filter           "invert(1.2)"
                                    :cursor           "pointer"
                                    :border-radius    "5px"})))))
   db))


(defn open-input-code-box
  [kp width-int height-int value syntax & [style]]
  (let [syntax     (or (if (= syntax "raw (clojure)") "clojure" syntax) "clojure")
        stringify? (not (= syntax "clojure"))]
    [re-com/box :size "auto"
     :width (px (- width-int 24))
     :height (px (- height-int 24))
     :style (merge {:font-family      (theme-pull :theme/monospaced-font nil) ; "Chivo Mono" ;"Fira Code"
                    :font-size        "16px"
                    :overflow         "auto"
                    :background-color "#00000000"
                    :font-weight      700}
                   style)
     :child [(reagent/adapt-react-class cm/UnControlled)
             {;; :value   (ut/format-map (- width-int 24)
              :value   (if stringify?
                         (try (str (cstr/join "\n" value))
                              (catch :default _ (str value)))
                         (ut/format-map (- width-int 24) (str value)))
              :onBlur  #(let [rr  (ut/cm-deep-values %)
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
                        :readOnly          false ;true
                        :theme             (theme-pull :theme/codemirror-theme nil)}}]]))






(defn field-code-box
  [panel-id query-key idx width-int height-int value]
  (let [sql-hint? (cstr/includes? (str value) ":::sql-string")
        kp        [:panels panel-id :queries query-key :select idx]]
    [re-com/box :size "auto" :width (px (- width-int 24)) :height (px (- height-int 24)) :style
     {:font-family   (theme-pull :theme/monospaced-font nil) ; "Chivo Mono" ;"Fira Code"
      :font-size     "16px"
      :overflow      "auto"
      :border-radius "12px"
      :font-weight   700} :child
     [(reagent/adapt-react-class cm/UnControlled)
      {:value   (ut/format-map (- width-int 24) (str value))
       :onBlur  #(ut/tracked-dispatch-sync [::update-selected-field kp (read-string (cstr/join " " (ut/cm-deep-values %)))])
       :options {:mode              (if sql-hint? "sql" "clojure")
                 :lineWrapping      true
                 :lineNumbers       true
                 :matchBrackets     true
                 :autoCloseBrackets true
                 :autofocus         false
                 :autoScroll        false
                 :detach            true
                 :readOnly          false ;true
                 :theme             (theme-pull :theme/codemirror-theme nil)}}]])) ;"ayu-mirage" ;"hopscotch"


(defn panel-param-box
  [type-key key width-int height-int value]
  (let [hidden-params-map (select-keys value [:selected-view :selected-view-data :selected-block])
        value (dissoc value :selected-view :selected-view-data :selected-block :opts)]
    [re-com/box :size "none" :width (px (- width-int 24)) :height (px (- height-int 24)) :style
     {:font-family   (theme-pull :theme/monospaced-font nil) ;"Fira Code" ; "Chivo Mono" ;"Fira
      :font-size     "13px"
      :overflow      "auto"
      :border-radius "12px"
      :font-weight   700} :child
     [(reagent/adapt-react-class cm/UnControlled)
      {:value   (ut/format-map (- width-int 24) (str (if (nil? key) value (get value key))))
       :onBlur  #(ut/tracked-dispatch [::set-user-parameters type-key (merge
                                                                       (read-string (cstr/join " " (ut/cm-deep-values %)))
                                                                       hidden-params-map)])
       :options {:mode              "clojure"
                 :lineWrapping      true
                 :lineNumbers       true
                 :matchBrackets     true
                 :autoCloseBrackets true
                 :autofocus         false
                 :autoScroll        false
                 :detach            true
                 :readOnly          false ;true
                 :theme             (theme-pull :theme/codemirror-theme nil)}}]])) ;"ayu-mirage" ;"hopscotch"


(defn read-only-sql-box
  [width-int height-int value]
  [re-com/box :size "auto" :width (px (- width-int 24)) :max-height (px (- height-int 24)) :style
   {;:background-color "#00000085"
    :font-family   (theme-pull :theme/monospaced-font nil) ; "Fira Code" ;"Chivo Mono"
    :margin-left   "9px"
    :font-size     "13px"
    :overflow      "hidden"
    :border-radius "12px"
    :font-weight   400} :child
   [(reagent/adapt-react-class cm/UnControlled)
    {:value   value
     :options {:mode              "sql"
               :lineWrapping      true ;false
               :lineNumbers       false
               :matchBrackets     true
               :autoCloseBrackets true
               :autofocus         false
               :autoScroll        false
               :detach            true
               :readOnly          true
               :theme             (theme-pull :theme/codemirror-theme nil)}}]]) ;"ayu-mirage"; "hopscotch"


(defn read-only-clojure-box
  [width-int height-int value]
  [re-com/box :size "auto" :padding "3px" :style
   {:background-color "#008B8B22"
    :border           "1px solid #008B8B55"
    :border-radius    "16px"
    :font-family      (theme-pull :theme/monospaced-font nil) ; "Fira Code" ;"Chivo Mono"
    :font-size        "13px"
    :overflow-y       "auto"
    :overflow-x       "hidden"
    :font-weight      400} :child
   [(reagent/adapt-react-class cm/UnControlled)
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
               :theme             (theme-pull :theme/codemirror-theme nil)}}]])

(defn console-box [value ww hh]
  (let [console? (vector? value)]
    [re-com/box
     :size "none"
     :width (px ww)
     :height (px hh)
     :padding "6px"
   ;:align :center :justify :center 
     :style {:background-color (when console? "#008B8B22")
             :border           (when console? "1px solid #008B8B55")
             :border-radius    "15px"
             :font-family      (theme-pull :theme/monospaced-font nil) ; "Fira Code" ;"Chivo Mono"
             :font-size        "17px"
             :overflow         "auto"
             :margin-left       "7px"
           ;:overflow-y       "auto"
           ;:overflow-x       "hidden"
             :font-weight      700} :child
     [(reagent/adapt-react-class cm/UnControlled)
      {:value   (ut/strip-ansi
                 (if console?
                   (str (cstr/join "\n" value))
                   (str value)))
       :options {:mode              "text"
                 :lineWrapping      true
                 :lineNumbers       false
                 :matchBrackets     true
                 :autoCloseBrackets true
                 :autofocus         false
                 :autoScroll        false
                 :detach            true
                 :readOnly          true
                 :theme             (theme-pull :theme/codemirror-theme nil)}}]]))


(defn read-only-clojure-box-un
  [value]
  [re-com/box :size "none" :style
   {:font-family   (theme-pull :theme/monospaced-font nil) ; "Chivo Mono" ;"Ubuntu" ;"Fira
    :font-size     "14px"
    :overflow-y    "hidden" ;"auto"
    :border-radius "12px"
    :overflow-x    "hidden"
    :font-weight   700} :child
   [(reagent/adapt-react-class cm/UnControlled)
    {:value   (str value) ;(format-map (- width-int 0) (str value))
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


(defn panel-code-box-single
  [panel-id key width-int height-int]
  (let [value     @(ut/tracked-subscribe [::workspace [panel-id key]])
        sql-hint? (cstr/includes? (str value) ":::sql-string")]
    [re-com/box :size "none" :width (px (- width-int 24)) :height (px (- height-int 24)) :style
     {:background-color "#00000085"
      :font-family      (theme-pull :theme/monospaced-font nil) ; "Fira Code" ;"Chivo Mono"
      :font-size        "13px"
      :border-radius    "12px"
      :overflow         "auto"
      :font-weight      700} :child
     [(reagent/adapt-react-class cm/UnControlled)
      {:value   (ut/format-map (- width-int 24) (str value)) ;value
       :onBlur  #(let [parse        (try (read-string (cstr/join " " (ut/cm-deep-values %))) (catch :default _ :cannot-parse))
                       unparseable? (= parse :cannot-parse)]
                   (if unparseable? (js/alert "BAD FORMS!") (ut/tracked-dispatch-sync [::update-workspace [panel-id key] parse])))
       :options {:mode              (if sql-hint? "sql" "clojure")
                 :lineWrapping      true
                 :lineNumbers       true
                 :matchBrackets     true
                 :autoCloseBrackets true
                 :autofocus         false
                 :autoScroll        false
                 :detach            true
                 :readOnly          false ;true
                 :theme             "hopscotch"}}]]))


(re-frame/reg-sub ::meta-from-param-name
                  (fn [db {:keys [param-name]}]
                    (let [psplit    (ut/splitter (ut/safe-name (str param-name)) "/")
                          table     (keyword (ut/replacer (first psplit) #":" ""))
                          field     (keyword (last psplit))
                          fmetadata (get-in db [:meta table :fields field])]
                      (cond (= table :condi)             ;; condis are special
                            {:data-type (ut/data-typer ;; todo: change this hardcoding when we move to global
                                         (condp = (get-in (get-in db [:post-condi field]) [0 :v])
                                           1 true
                                           0 false
                                           :else (get-in (get-in db [:post-condi field]) [0 :v])))}
                            (nil? fmetadata) ;; else assume user-space param
                            {:data-type (ut/data-typer (get-in db [:click-param table field]))}
                            :else            fmetadata))))                                    ;; else get the post meta


(re-frame/reg-sub ::meta-from-query-name (fn [db [_ query-name]] (get-in db [:meta query-name])))

(re-frame/reg-sub ::panel-from-query-name
                  (fn [db [_ query-name]]
                    (try (first (remove nil?
                                        (for [[k v] (get db :panels)] (when (not (nil? (get-in v [:queries query-name]))) k))))
                         (catch :default e nil))))

(re-frame/reg-sub ::clover-params-in-view
                  (fn [db _]
                    (let [part-kp @(ut/tracked-sub ::editor-panel-selected-view {})
                          panel   (get-in db (vec (into [:panels (get db :selected-block)] (vec part-kp))))
                          parts   (vec (ut/get-compound-keys panel))]
                      parts)))

(re-frame/reg-sub
 ::valid-params-in-tab
 (fn [db _]
   (let [blocks (ut/deep-flatten (into {} (filter #(= (get db :selected-tab) (get (val %) :tab "")) (get db :panels))))
         blocks (vec (filter #(cstr/includes? (str %) "/") blocks))]
     (or blocks []))))

(re-frame/reg-sub
 ::valid-block-kps-in-tab
 (fn [db _]
   (let [blocks (ut/deep-remove-keys (into {} (filter #(= (get db :selected-tab) (get (val %) :tab ""))
                                                      (get db :panels)))
                                     [:root :selected-mode :opts :root :selected-view :icon :icon-view])
         blocks (filterv #(= (count %) 3)
                         (ut/kvpaths blocks))]
     (or blocks []))))


(def searcher-atom (reagent/atom nil))

(defn filter-search
  [x y] ;; search-str all-items
  (if (or (nil? x) (empty? x))
    y
    (if (map? y)
      (into {}
            (filter (fn [[k v]]
                      (or (cstr/includes? (cstr/lower-case (str k)) (cstr/lower-case x))
                          (cstr/includes? (cstr/lower-case (str v)) (cstr/lower-case x))))
                    y))
      (vec (filter #(cstr/includes? (cstr/lower-case (str %)) (cstr/lower-case x)) y)))))


(defn click-browser-vdata-rendered [width-int]
  (let [click-params        @(ut/tracked-sub ::all-click-params {})
        ;ph                  @param-hover ;; reaction hack
        pf                  @db/param-filter
        selected-block      @(ut/tracked-sub ::selected-block {})
        ;selected-tab        @(ut/tracked-sub ::selected-tab {})
                ;;_ (tapp>> [:gg (apply merge click-params)])
        current-tab-queries (try         ;(map #(-> % ut/sql-keyword str)
                              (into (into (vec @(ut/tracked-sub ::current-tab-queries {}))
                                          (vec @(ut/tracked-sub ::current-tab-blocks {})))
                                    (vec @(ut/tracked-sub ::current-tab-condis {})))
                              (catch :default _ []))
        current-tab-slices   @(ut/tracked-sub ::current-tab-slices {}) ;; contains ALL view names, makes above redunctant.  TODO
                ;;_ (tapp>> [:current-tab-slices current-tab-slices])
        valid-params-in-tab  @(ut/tracked-sub ::valid-params-in-tab {})
                ;_ (tapp>> [:valid-params-in-tab valid-params-in-tab])
        clover-params       (if @db/cm-focused? @(ut/tracked-sub ::clover-params-in-view {}) {})
        grp (or (apply merge click-params) {})
        filtered-params (into {}
                              (filter
                               #(and ;; filter out from the top bar toggle first....
                                 (not (= (str (first %)) ":/")) ;; sometimes a garbo
                                                                              ;; param sneaks in
                                 (not (cstr/starts-with? (str (first %)) ":solver-meta/"))
                                 (not (cstr/starts-with? (str (first %)) ":panel-hash/"))
                                 (not (cstr/starts-with? (str (first %)) ":repl-ns/"))
                                 (not (cstr/starts-with? (str (first %)) ":solver-status/"))
                                 (not (cstr/starts-with? (str (first %)) ":signal/"))
                                 (and (not (cstr/starts-with? (str (first %)) ":server/"))
                                      (not (cstr/ends-with? (str (first %)) "-chart")))
                                 (not (and (cstr/starts-with? (str (first %)) ":solver/")
                                           (re-matches #".*\d" (str (first %)))))
                                 (not (cstr/ends-with? (str (first %)) "*running?"))
                                 (not (cstr/ends-with? (str (first %)) "/selected-view"))
                                 (not (cstr/ends-with? (str (first %)) "/selected-block"))
                                 (not (cstr/ends-with? (str (first %)) "/selected-view-data"))
                                 (not (nil? (last %))) ;; no nil val params 
                                 (not (cstr/starts-with? (str (first %)) ":conn-list/"))
                                 (not (cstr/starts-with? (str (first %)) ":time/"))
                                 (not (cstr/starts-with? (str (first %)) ":sys/"))
                                 (not (cstr/starts-with? (str (first %)) ":client/"))
                                 (not (cstr/starts-with? (str (first %)) (if (get pf :theme) ":theme/***" ":theme/")))
                                 (not (cstr/starts-with? (str (first %)) (if (get pf :user) ":param/***" ":param/")))
                                 (not (cstr/starts-with? (str (first %))
                                                         (if (get pf :condis) ":condi/***" ":condi/")))
                                 (if (get pf :this-tab? true)
                                   (or
                                    (some (fn [x] (= (str x) (str (first %)))) valid-params-in-tab) ;; TODO this is redundant and wasteful w slices here
                                    (some (fn [x] (cstr/starts-with? (str (first %)) (str ":" x))) current-tab-slices)
                                    (some (fn [x] (cstr/starts-with? (str (first %)) (str ":" x))) current-tab-queries))
                                   true)
                                 (not (cstr/includes? (str (first %)) "-sys/")))
                               grp))
                    ;;_ (tapp>>  [:grp click-params valid-params-in-tab])
        searches?       (and (ut/ne? @searcher-atom) (>= (count (str @searcher-atom)) 2))
        filter-keys     (if searches?
                                     ;(vec (filter-search @searcher-atom (keys (into {} grp))))
                          (vec (filter-search @searcher-atom (keys grp)))
                          (vec (keys filtered-params)))
        filtered-params (select-keys (into {} grp) filter-keys)
        filtered-params (if @db/cm-focused? (select-keys grp clover-params) filtered-params)
        ;is-not-empty?   (ut/ne? (remove empty? filtered-params))
        v-boxes (vec (for [[k v] (sort filtered-params)]
                       (let [psplit        (ut/splitter (ut/safe-name (str k)) "/")
                             table         (-> (first psplit)
                                               (ut/replacer #":" "")
                                                                          ;(ut/replacer ".*" "") ;; was stopping deletion. do we need this for something else? 7/19/24
                                               keyword)
                             field         (keyword (last psplit))
                             param-has-fn? (try (and (= (first v) :run-solver) (= table :param)) (catch :default _ false))
                                                                                     ;;param-value (str @(ut/tracked-subscribe [::conn/clicked-parameter [table field]]))
                             param-value   @(ut/tracked-sub ::conn/clicked-parameter-key-alpha
                                                            {:keypath [(keyword (cstr/join "/"
                                                                                           (map #(cstr/replace (str %) ":" "")
                                                                                                [table field])))]})
                             k k
                             v             (str (or param-value v))
                             meta          @(ut/tracked-sub ::meta-from-param-name {:param-name k})
                             dtype         (if param-has-fn? (ut/data-typer param-value) (get meta :data-type))
                                                                                    ;;  dtype         (try (if (and (= dtype "vector") (every? string? v)) "string" dtype)
                                                                                    ;;                     (catch :default _ dtype)) ;; since stringified code is
                             dcolor        (get @(ut/tracked-sub ::conn/data-colors {}) dtype)
                                                                                    ;;  _ (tapp>> [:params dtype dcolor v param-value])
                             param-value   (str param-value) ;; since the rest expects it

                                                                                     ;;  _ (ut/tapp>> [[table field]  param-value])
                             is-map?       (or (= dtype "map") (= dtype "vector")) ;;assoc-happy data
                             is-image?     (and (or (cstr/ends-with? (cstr/lower-case (str param-value)) ".png")
                                                    (cstr/ends-with? (cstr/lower-case (str param-value)) ".webp")
                                                    (cstr/ends-with? (cstr/lower-case (str param-value)) ".jpg")
                                                    (cstr/ends-with? (cstr/lower-case (str param-value)) ".jpeg")
                                                    (cstr/ends-with? (cstr/lower-case (str param-value)) ".gif")
                                                    (cstr/includes? (cstr/lower-case (str param-value)) "/images/")
                                                    (cstr/includes? (cstr/lower-case (str param-value)) "/image/"))
                                                (or
                                                 (cstr/starts-with? (cstr/lower-case (str param-value)) "http")
                                                 (cstr/starts-with? (cstr/lower-case (str param-value)) "./images")))
                             is-b64?       (ut/is-base64? (str param-value))
                             is-video?     (or (cstr/ends-with? (cstr/lower-case (str param-value)) ".mp4")
                                               (cstr/ends-with? (cstr/lower-case (str param-value)) ".mov"))
                             selected?     @(ut/tracked-subscribe [::has-query? selected-block table])
                             display-v     (cond (= dtype "map") "{ ... map hidden ... }"
                                                 is-b64?         "**huge base64 string**"
                                                 :else           (str v))
                             px-height     (max
                                            (+ 10 (* (Math/ceil (/ (count (str display-v)) 40)) 20))
                                            40)
                             pwidth        (js/Math.floor (/ (count (str param-value)) 1.7))
                             pwidth        (cond (> pwidth 30) 30
                                                 (< pwidth 6)  6
                                                 :else         pwidth)
                             pheight       (js/Math.floor (/ pwidth 30))
                             pheight       (cond (> pheight 3) 3
                                                 (< pheight 1) 1
                                                 :else         pheight)
                             ;hovered?      (= k (first @param-hover))
                             bvwidth (- width-int 50)
                             bvheight (+ px-height 8)]
                         {:psplit psplit :table table :field field :param-has-fn? param-has-fn? :display-v display-v
                          :param-value param-value :v v :k k :meta meta :dtype dtype :dcolor dcolor
                          :is-map? is-map? :is-image? is-image? :is-b64? is-b64? :is-video? is-video?
                          :selected? selected? :px-height px-height :pwidth pwidth :pheight pheight :bvwidth bvwidth :bvheight bvheight})))]
    v-boxes))


;;@db/param-filter
;;(reset! db/param-v-boxes (click-browser-vdata-rendered 350))   


(defn click-param-browser-cached
  [click-params width-int height-int]
  
  (let [ph                  @param-hover ;; reaction hack
        pf                  @db/param-filter
        selected-block      @(ut/tracked-sub ::selected-block {})
        selected-tab        @(ut/tracked-sub ::selected-tab {})
        ;;_ (tapp>> [:gg (apply merge click-params)])
        current-tab-queries (try         ;(map #(-> % ut/sql-keyword str)
                              (into (into (vec @(ut/tracked-sub ::current-tab-queries {}))
                                          (vec @(ut/tracked-sub ::current-tab-blocks {})))
                                    (vec @(ut/tracked-sub ::current-tab-condis {})))
                              (catch :default _ []))
        current-tab-slices   @(ut/tracked-sub ::current-tab-slices {}) ;; contains ALL view names, makes above redunctant.  TODO
        ;;_ (tapp>> [:current-tab-slices current-tab-slices])
        valid-params-in-tab  @(ut/tracked-sub ::valid-params-in-tab {})
        ;_ (tapp>> [:valid-params-in-tab valid-params-in-tab])
        clover-params       (if @db/cm-focused? @(ut/tracked-sub ::clover-params-in-view {}) {})
        grp (or (apply merge click-params) {})
        v-boxes (vec (for [{:keys [psplit  table  field  param-has-fn?
                                   param-value  v k  meta  dtype  dcolor display-v
                                   is-map?  is-image?  is-b64?  is-video?
                                   selected?  px-height  pwidth  pheight  bvwidth  bvheight]} @db/param-v-boxes
                           :let [hovered?      (= k (first @param-hover))]]
                       [bvwidth
                        bvheight
                        (draggable ;(sql-spawner-filter :param [k table field])
                         {:h         (cond is-image? 6
                                           is-video? 9
                                           is-map? 9
                                           :else     (+ 2 pheight))
                          :w         (cond is-image? 6
                                           is-video? 13
                                           is-map? 9
                                           :else     pwidth)
                          :root      [0 0]
                          :drag-meta {:type :param :param-full k :param-type dtype :param-table table :param-field field}}
                         "meta-menu"
                         [re-com/v-box
                          :height (px px-height)
                          :width (px (- width-int 50))
                          :size "none"
                          :attr {:on-mouse-enter #(reset! param-hover [k table field])
                                 :on-mouse-over  #(when (not hovered?) (reset! param-hover [k table field]))
                                 :on-mouse-leave #(reset! param-hover nil)}
                          :style {:border           (str "1px solid " dcolor)
                                  :overflow       "hidden"
                                                              ;:margin-bottom  "6px"
                                  :background-color (if hovered? (str dcolor 55) (if selected? (str dcolor 20) "inherit"))}
                          :children
                          [[re-com/h-box :justify :between :children
                            [[re-com/box :child
                              (str (if hovered?
                                     (let [chars (count (str k))
                                           len   25
                                           wide? (> chars len)]
                                       (if wide? (str (subs (str k) 0 len) "...") (str k)))
                                     (str k))
                                   (when param-has-fn? " *ƒ"))
                              :style {:color dcolor :font-weight 700 :font-size "13px"
                                      :padding-left "4px" :padding-right "4px"}]

                             (if hovered?
                               [re-com/h-box
                                :gap "4px"
                                :children
                                [[re-com/md-icon-button
                                  :md-icon-name "zmdi-copy"
                                  :style {:color        dcolor
                                          :padding      "0px"
                                          :margin-top   "-2px"
                                          :margin-right "3px"
                                          :font-size    "14px"
                                          :height       "15px"}
                                  :on-click #(ut/tracked-dispatch [::conn/copy-to-clipboard (str k)])]
                                 [re-com/md-icon-button
                                  :md-icon-name "fa-regular fa-trash-can" ;; "zmdi-close"
                                  :style {:color        dcolor
                                          :padding      "0px"
                                          :margin-top   "-2px"
                                          :margin-right "3px"
                                          :font-size    "14px"
                                          :height       "15px"}
                                  :on-click #(ut/tracked-dispatch [::conn/declick-parameter [table field]])]]]
                               [re-com/box
                                :child (str dtype)
                                :style {:color dcolor :font-size "10px" :padding-top "3px" :padding-left "4px" :padding-right "4px"}])]]
                           [re-com/box
                            :child
                            (if (scrub/hex-color? v)
                              [re-com/h-box
                               :gap "7px"
                               :children [(str v)
                                          [re-com/box :src (at) :child " " :size "auto" :width "15px" :height "15px" :style
                                           {:background-color (str v) :margin-top "2px" :padding-left "3px" :padding-right "3px"}]]]
                              (if (nil? v)
                                "NULL"
                                display-v))
                            :style (merge {:color          (str (theme-pull :theme/editor-font-color nil) 99) ; "#ffffff99"
                                           :font-size      "13px"
                                           :font-weight    700
                                           :padding-left   "4px"
                                           :padding-right  "4px"
                                           :padding-bottom "2px"}
                                          (if (nil? v) {:font-style "italic" :opacity 0.3} {})) :align :end]]])]))]
    (if false ;@db/param-code-hover
      [re-com/v-box :padding "6px" :width (px (- width-int 33)) :align :center :justify :between :gap "10px" :children
       [[re-com/box :style
         {:font-size        "14px"
          :font-weight      700
          :border-radius    "5px"
          :color            (ut/invert-hex-color (get (theme-pull :theme/data-colors nil) "keyword")) ;; (theme-pull
          :padding          "4px"
          :background-color (get (theme-pull :theme/data-colors nil) "keyword")} :align :center :justify :center :child
         (str @db/param-code-hover)]
        (let [code @db/param-code-hover
              code (try (edn/read-string code) (catch :default _ code))
              vv   @(ut/tracked-sub ::conn/clicked-parameter-key-alpha {:keypath [code]})]
          [re-com/box :size "auto" :width "100%" :style {:font-size "15px"} :child [map-boxes2 vv nil "" [] nil nil] :style
           {;:color "white"
            :border-radius "4px"}])]]
      [re-com/v-box :children
       [[re-com/h-box :padding "6px" :height "33px" :width (px (- width-int 33)) :align :center :justify :between :children
         (if @db/cm-focused?
           [[re-com/box :size "auto" :align :center :justify :center :style
             {:color "#ffffff99" :font-size "14px" :font-weight 500} 
             :child "clover params used in this view"]]
           [[re-com/input-text :src (at) :model searcher-atom :width "93%" :on-change
             #(reset! searcher-atom (let [vv (str %) ;; (cstr/trim (str %))
                                          ]
                                      (if (empty? vv) nil vv))) :placeholder "(search parameters)" :change-on-blur? false :style
             {:text-decoration  (when (ut/ne? @searcher-atom) "underline")
              :color            "inherit"
              :border           "none"
              :outline          "none"
              :text-align       "center"
              :background-color "#00000000"}]
            [re-com/box :style
             {;:border "1px solid maroon"
              :opacity (if @searcher-atom 1.0 0.45)
              :cursor  "pointer"} :width "20px" :align :center :justify :center :attr {:on-click #(reset! searcher-atom nil)}
             :child "x"]])]

        (when (ut/ne? grp)
          [reecatch
           [re-com/box
            :size "none"
            :width (px (- width-int 24))
            :height (px (- height-int 103))
            :style {:overflow-y "auto"
                    :padding-top "4px"
                    :overflow-x "hidden"}
            :child (if (ut/ne? v-boxes)
                     [vbunny/virtual-v-box
                      :height (px (Math/floor (- height-int 113)))
                      :width (px (Math/floor (- width-int 33)))
                      :id (str "parameter-browser" selected-tab)
                      :children v-boxes]
                     [re-com/box
                      :size "auto"
                      :height "100px"
                      :align :center :justify :center
                      :child "no params found"])]])]])))

(declare reactive-virtualized-console-viewer)

(defn query-meta-browser [data-key width-int height-int]
  (let [x @(ut/tracked-subscribe [::meta-from-query-name data-key])
        x (get x :fields {})
        text (if (vector? x) (cstr/join "\n" x) (pr-str x))
        ww (- width-int 33)
        hh (+ height-int 55)
        ;text (ut/format-edn ww text 9) ;; 3rd arg is math of pixels per font char - zprint uses cols, not pixels
        text (conn/format-edn-puget ww text 7)
        text (vec (cstr/split text #"\n"))] 
    [re-com/box
     :size "auto"
     :width (px ww)
     :height (px hh)
     ;:style {:border "1px solid cyan"}
     :child
     ;;(pr-str query-meta)
     ;[meta-edn-box (+ width-int 70) (+ height-int 55) query-meta]
     [reactive-virtualized-console-viewer {:style {:font-weight 700 
                                                   :font-family (theme-pull :theme/monospaced-font nil) 
                                                   :font-size "14px"}
                                           :text text
                                           :id (str "query-meta-" data-key)
                                           :width ww
                                           :height hh}]]))

(defn click-param-browser ;; has a very annoying scrolling problem. I've wasted too much time on this bullshit.
  [click-params width-int height-int]
  (let [ph                  @param-hover ;; reaction hack
        pf                  @db/param-filter
        selected-block      @(ut/tracked-sub ::selected-block {})
        selected-tab        @(ut/tracked-sub ::selected-tab {})
        ;;_ (tapp>> [:gg (apply merge click-params)])
        current-tab-queries (try         ;(map #(-> % ut/sql-keyword str)
                              (into (into (vec @(ut/tracked-sub ::current-tab-queries {}))
                                          (vec @(ut/tracked-sub ::current-tab-blocks {})))
                                    (vec @(ut/tracked-sub ::current-tab-condis {})))
                              (catch :default _ []))
        current-tab-slices   @(ut/tracked-sub ::current-tab-slices {}) ;; contains ALL view names, makes above redunctant.  TODO
        ;;_ (tapp>> [:current-tab-slices current-tab-slices])
        valid-params-in-tab  @(ut/tracked-sub ::valid-params-in-tab {})
        ;_ (tapp>> [:valid-params-in-tab valid-params-in-tab])
        clover-params       (if @db/cm-focused? @(ut/tracked-sub ::clover-params-in-view {}) {})
        grp (or (apply merge click-params) {})]
    (if false ;@db/param-code-hover
      [re-com/v-box :padding "6px" :width (px (- width-int 33)) :align :center :justify :between :gap "10px" :children
       [[re-com/box :style
         {:font-size        "14px"
          :font-weight      700
          :border-radius    "5px"
          :color            (ut/invert-hex-color (get (theme-pull :theme/data-colors nil) "keyword")) ;; (theme-pull
          :padding          "4px"
          :background-color (get (theme-pull :theme/data-colors nil) "keyword")} :align :center :justify :center :child
         (str @db/param-code-hover)]
        (let [code @db/param-code-hover
              code (try (edn/read-string code) (catch :default _ code))
              vv   @(ut/tracked-sub ::conn/clicked-parameter-key-alpha {:keypath [code]})]
          [re-com/box :size "auto" :width "100%" :style {:font-size "15px"} :child [map-boxes2 vv nil "" [] nil nil] :style
           {;:color "white"
            :border-radius "4px"}])]]
      [re-com/v-box :children
       [[re-com/h-box :padding "6px" :height "33px" :width (px (- width-int 33)) :align :center :justify :between :children
         (if @db/cm-focused?
           [[re-com/box :size "auto" :align :center :justify :center :style
             {:color "#ffffff99" :font-size "14px" :font-weight 500} :child "clover params used in this view"]]
           [[re-com/input-text :src (at) :model searcher-atom :width "93%" :on-change
             #(reset! searcher-atom (let [vv (str %) ;; (cstr/trim (str %))
                                          ]
                                      (if (empty? vv) nil vv))) :placeholder "(search parameters)" :change-on-blur? false :style
             {:text-decoration  (when (ut/ne? @searcher-atom) "underline")
              :color            "inherit"
              :border           "none"
              :outline          "none"
              :text-align       "center"
              :background-color "#00000000"}]
            [re-com/box :style
             {;:border "1px solid maroon"
              :opacity (if @searcher-atom 1.0 0.45)
              :cursor  "pointer"} :width "20px" :align :center :justify :center :attr {:on-click #(reset! searcher-atom nil)} 
             :child "x"]])]
        
        (when (ut/ne? grp)
          [reecatch 
           [re-com/box 
           :size "none" 
           :width (px (- width-int 24)) 
           :height (px (- height-int 103)) 
           :style {:overflow-y "auto" 
                   :padding-top "4px" 
                   :overflow-x "hidden"}
           :child

           (let [filtered-params (into {}
                                       (filter
                                        #(and ;; filter out from the top bar toggle first....
                                          (not (= (str (first %)) ":/")) ;; sometimes a garbo
                                                                            ;; param sneaks in
                                          (not (cstr/starts-with? (str (first %)) ":data/"))
                                          (not (cstr/starts-with? (str (first %)) ":solver-meta/"))
                                          (not (cstr/starts-with? (str (first %)) ":panel-hash/"))
                                          (not (cstr/starts-with? (str (first %)) ":repl-ns/"))
                                          (not (cstr/starts-with? (str (first %)) ":solver-status/"))
                                          (not (cstr/starts-with? (str (first %)) ":signal/"))
                                          (and (not (cstr/starts-with? (str (first %)) ":server/"))
                                               (not (cstr/ends-with? (str (first %)) "-chart")))
                                          (not (and (cstr/starts-with? (str (first %)) ":solver/")
                                                    (re-matches #".*\d" (str (first %)))))
                                          (not (cstr/ends-with? (str (first %)) "*running?"))
                                          (not (cstr/ends-with? (str (first %)) "/selected-view"))
                                          (not (cstr/ends-with? (str (first %)) "/selected-block"))
                                          (not (cstr/ends-with? (str (first %)) "/selected-view-data"))
                                          (not (nil? (last %))) ;; no nil val params 
                                          (not (cstr/starts-with? (str (first %)) ":conn-list/"))
                                          (not (cstr/starts-with? (str (first %)) ":time/"))
                                          (not (cstr/starts-with? (str (first %)) ":sys/"))
                                          (not (cstr/starts-with? (str (first %)) ":client/"))
                                          (not (cstr/starts-with? (str (first %)) (if (get pf :theme) ":theme/***" ":theme/")))
                                          (not (cstr/starts-with? (str (first %)) (if (get pf :user) ":param/***" ":param/")))
                                          (not (cstr/starts-with? (str (first %))
                                                                  (if (get pf :condis) ":condi/***" ":condi/")))
                                          (if (get pf :this-tab? true)
                                            (or
                                             (some (fn [x] (= (str x) (str (first %)))) valid-params-in-tab) ;; TODO this is redundant and wasteful w slices here
                                             (some (fn [x] (cstr/starts-with? (str (first %)) (str ":" x))) current-tab-slices)
                                             (some (fn [x] (cstr/starts-with? (str (first %)) (str ":" x))) current-tab-queries))
                                            true)
                                          (not (cstr/includes? (str (first %)) "-sys/")))
                                        grp))
                  ;;_ (tapp>>  [:grp click-params valid-params-in-tab])
                 searches?       (and (ut/ne? @searcher-atom) (>= (count (str @searcher-atom)) 2))
                 filter-keys     (if searches?
                                   ;(vec (filter-search @searcher-atom (keys (into {} grp))))
                                   (vec (filter-search @searcher-atom (keys grp)))
                                   (vec (keys filtered-params)))
                 filtered-params (select-keys (into {} grp) filter-keys)
                 filtered-params (if @db/cm-focused? (select-keys grp clover-params) filtered-params)
                 is-not-empty?   (ut/ne? (remove empty? filtered-params))
                 ;_ (tapp>> [:ser filtered-params])
                 sorted-filtered (sort filtered-params)
                 last-key (atom nil)
                 ]
              ;;(tapp>>  [:param-counts (count (keys filtered-params))])
             (if is-not-empty?
               (let [v-boxes (vec (for [[k v] (sort filtered-params)]
                                    (let [psplit        (ut/splitter (ut/safe-name (str k)) "/")
                                          table         (-> (first psplit)
                                                            (ut/replacer #":" "")
                                                            ;(ut/replacer ".*" "") ;; was stopping deletion. do we need this for something else? 7/19/24
                                                            keyword)
                                          field         (keyword (last psplit))
                                          param-has-fn? (try (and (= (first v) :run-solver) (= table :param)) (catch :default _ false))
                                          ;;param-value (str @(ut/tracked-subscribe [::conn/clicked-parameter [table field]]))
                                          param-value   @(ut/tracked-sub ::conn/clicked-parameter-key-alpha
                                                                         {:keypath [(keyword (cstr/join "/"
                                                                                                        (map #(cstr/replace (str %) ":" "")
                                                                                                             [table field])))]})
                                          v             (str (or param-value v))
                                          meta          @(ut/tracked-sub ::meta-from-param-name {:param-name k})
                                          dtype         (if param-has-fn? (ut/data-typer param-value) (get meta :data-type))
                                                                      ;;  dtype         (try (if (and (= dtype "vector") (every? string? v)) "string" dtype)
                                                                      ;;                     (catch :default _ dtype)) ;; since stringified code is
                                          dcolor        (get @(ut/tracked-sub ::conn/data-colors {}) dtype)
                                                                      ;;  _ (tapp>> [:params dtype dcolor v param-value])
                                          param-value   (str param-value) ;; since the rest expects it

                                                                       ;;  _ (ut/tapp>> [[table field]  param-value])
                                          is-map?       (or (= dtype "map") (= dtype "vector")) ;;assoc-happy data
                                          is-image?     (and (or (cstr/ends-with? (cstr/lower-case (str param-value)) ".png")
                                                                 (cstr/ends-with? (cstr/lower-case (str param-value)) ".webp")
                                                                 (cstr/ends-with? (cstr/lower-case (str param-value)) ".jpg")
                                                                 (cstr/ends-with? (cstr/lower-case (str param-value)) ".jpeg")
                                                                 (cstr/ends-with? (cstr/lower-case (str param-value)) ".gif")
                                                                 (cstr/includes? (cstr/lower-case (str param-value)) "/images/")
                                                                 (cstr/includes? (cstr/lower-case (str param-value)) "/image/"))
                                                             (or
                                                              (cstr/starts-with? (cstr/lower-case (str param-value)) "http")
                                                              (cstr/starts-with? (cstr/lower-case (str param-value)) "./images")))
                                          is-b64?       (ut/is-base64? (str param-value))
                                          is-video?     (or (cstr/ends-with? (cstr/lower-case (str param-value)) ".mp4")
                                                            (cstr/ends-with? (cstr/lower-case (str param-value)) ".mov"))
                                          selected?     @(ut/tracked-subscribe [::has-query? selected-block table])
                                          display-v     (cond (= dtype "map") "{ ... map hidden ... }"
                                                              is-b64?         "**huge base64 string**"
                                                              :else           (str v))
                                          px-height     (max
                                                         (+ 10 (* (Math/ceil (/ (count (str display-v)) 40)) 20))
                                                         40)
                                          pwidth        (js/Math.floor (/ (count (str param-value)) 1.7))
                                          pwidth        (cond (> pwidth 30) 30
                                                              (< pwidth 6)  6
                                                              :else         pwidth)
                                          pheight       (js/Math.floor (/ pwidth 30))
                                          pheight       (cond (> pheight 3) 3
                                                              (< pheight 1) 1
                                                              :else         pheight)
                                          hovered?      (= k (first @param-hover))
                                          new-field?    (not= table @last-key)
                                          _ (reset! last-key table)]
                                      [(- width-int 50)
                                       (+ px-height (if new-field? 10 0))
                                       (draggable ;(sql-spawner-filter :param [k table field])
                                        {:h         (cond is-image? 6
                                                          is-video? 9
                                                          is-map? 9
                                                          :else     (+ 2 pheight))
                                         :w         (cond is-image? 6
                                                          is-video? 13
                                                          is-map? 9
                                                          :else     pwidth)
                                         :root      [0 0]
                                         :drag-meta {:type :param :param-full k :param-type dtype :param-table table :param-field field}}
                                        "meta-menu"
                                        [re-com/v-box
                                         :height (px px-height)
                                         :width (px (- width-int 50))
                                         :size "none"
                                         :attr {:on-mouse-enter #(reset! param-hover [k table field])
                                                :on-mouse-over  #(when (not hovered?) (reset! param-hover [k table field]))
                                                :on-mouse-leave #(reset! param-hover nil)}
                                         :style {:border           (str "1px solid " dcolor)
                                                 :overflow       "hidden"
                                            ;:margin-bottom  "6px"
                                                 :background-color (if hovered? (str dcolor 55) (if selected? (str dcolor 20) "inherit"))}
                                         :children
                                         [[re-com/h-box :justify :between :children
                                           [[re-com/box :child
                                             (str (if hovered?
                                                    (let [chars (count (str k))
                                                          len   25
                                                          wide? (> chars len)]
                                                      (if wide? (str (subs (str k) 0 len) "...") (str k)))
                                                    (str k))
                                                  (when param-has-fn? " *ƒ"))
                                             :style {:color dcolor :font-weight 700 :font-size "13px"
                                                     :padding-left "4px" :padding-right "4px"}]

                                            (if hovered?
                                              [re-com/h-box
                                               :gap "4px"
                                               :children
                                               [[re-com/md-icon-button
                                                 :md-icon-name "zmdi-copy"
                                                 :style {:color        dcolor
                                                         :padding      "0px"
                                                         :margin-top   "-2px"
                                                         :margin-right "3px"
                                                         :font-size    "14px"
                                                         :height       "15px"}
                                                 :on-click #(ut/tracked-dispatch [::conn/copy-to-clipboard (str k)])]
                                                [re-com/md-icon-button
                                                 :md-icon-name "fa-regular fa-trash-can" ;; "zmdi-close"
                                                 :style {:color        dcolor
                                                         :padding      "0px"
                                                         :margin-top   "-2px"
                                                         :margin-right "3px"
                                                         :font-size    "14px"
                                                         :height       "15px"}
                                                 :on-click #(ut/tracked-dispatch [::conn/declick-parameter [table field]])]]]
                                              [re-com/box
                                               :child (str dtype)
                                               :style {:color dcolor :font-size "10px" :padding-top "3px" :padding-left "4px" :padding-right "4px"}])]]
                                          [re-com/box
                                           :child
                                           (if (scrub/hex-color? v)
                                             [re-com/h-box
                                              :gap "7px"
                                              :children [(str v)
                                                         [re-com/box :src (at) :child " " :size "auto" :width "15px" :height "15px" :style
                                                          {:background-color (str v) :margin-top "2px" :padding-left "3px" :padding-right "3px"}]]]
                                             (if (nil? v)
                                               "NULL"
                                               display-v))
                                           :style (merge {:color          (str (theme-pull :theme/editor-font-color nil) 99) ; "#ffffff99"
                                                          :font-size      "13px"
                                                          :font-weight    700
                                                          :padding-left   "4px"
                                                          :padding-right  "4px"
                                                          :padding-bottom "2px"}
                                                         (if (nil? v) {:font-style "italic" :opacity 0.3} {})) :align :end]]])])))]
                 ;;(tapp>> [:param-steps (str "parameter-browser" (hash filtered-params) width-int height-int) (- height-int 103) (- width-int 33)])
                 [vbunny/virtual-v-box
                  :height (px (Math/floor (- height-int 113)))
                  :width (px (Math/floor (- width-int 33)))
                  :id (str "parameter-browser" selected-tab)
                  ;:fixed? true
                  ;; :style {:cursor "grab"
                  ;;       ;:background-color (theme-pull :theme/editor-param-background-color nil)
                  ;;         }
                  ;:children (vec (interpose [(- width-int 50) 6 [:div ""]] v-boxes))
                  :children v-boxes])
               [re-com/box
                :size "auto"
                :height "100px"
                :align :center :justify :center
                :child "no params found"]))]])]]))) ;]



(defn click-param-browser-reg
  [click-params width-int height-int]
  (let [ph                  @param-hover ;; reaction hack
        pf                  @db/param-filter
        selected-block      @(ut/tracked-sub ::selected-block {})
        current-tab-queries (try         ;(map #(-> % ut/sql-keyword str)
                              (into (into (vec @(ut/tracked-sub ::current-tab-queries {}))
                                          (vec @(ut/tracked-sub ::current-tab-blocks {})))
                                    (vec @(ut/tracked-sub ::current-tab-condis {})))
                              (catch :default _ []))
        
        current-tab-slices   @(ut/tracked-sub ::current-tab-slices {}) ;; contains ALL view names, makes above redunctant.  TODO
        ;;_ (tapp>> [:current-tab-slices current-tab-slices])
        valid-params-in-tab  @(ut/tracked-sub ::valid-params-in-tab {})
        ;_ (tapp>> [:valid-params-in-tab valid-params-in-tab])
        clover-params       (if @db/cm-focused? @(ut/tracked-sub ::clover-params-in-view {}) {})]
    (if false ;@db/param-code-hover
      [re-com/v-box :padding "6px" :width (px (- width-int 33)) :align :center :justify :between :gap "10px" :children
       [[re-com/box :style
         {:font-size        "14px"
          :font-weight      700
          :border-radius    "5px"
          :color            (ut/invert-hex-color (get (theme-pull :theme/data-colors nil) "keyword")) ;; (theme-pull
          :padding          "4px"
          :background-color (get (theme-pull :theme/data-colors nil) "keyword")} :align :center :justify :center :child
         (str @db/param-code-hover)]
        (let [code @db/param-code-hover
              code (try (edn/read-string code) (catch :default _ code))
              vv   @(ut/tracked-sub ::conn/clicked-parameter-key-alpha {:keypath [code]})]
          [re-com/box :size "auto" :width "100%" :style {:font-size "15px"} :child [map-boxes2 vv nil "" [] nil nil] :style
           {;:color "white"
            :border-radius "4px"}])]]
      [re-com/v-box :children
       [[re-com/h-box :padding "6px" :height "33px" :width (px (- width-int 33)) :align :center :justify :between :children
         (if @db/cm-focused?
           [[re-com/box :size "auto" :align :center :justify :center :style
             {:color "#ffffff99" :font-size "14px" :font-weight 500} :child "clover params used in this view"]]
           [[re-com/input-text :src (at) :model searcher-atom :width "93%" :on-change
             #(reset! searcher-atom (let [vv (str %) ;; (cstr/trim (str %))
                                          ]
                                      (if (empty? vv) nil vv))) :placeholder "(search parameters)" :change-on-blur? false :style
             {:text-decoration  (when (ut/ne? @searcher-atom) "underline")
              :color            "inherit"
              :border           "none"
              :outline          "none"
              :text-align       "center"
              :background-color "#00000000"}]
            [re-com/box :style
             {;:border "1px solid maroon"
              :opacity (if @searcher-atom 1.0 0.45)
              :cursor  "pointer"} :width "20px" :align :center :justify :center :attr {:on-click #(reset! searcher-atom nil)}
             :child "x"]])]
        [re-com/box :size "none" :width (px (- width-int 24)) :height (px (- height-int 103)) :style
         {:overflow-y "auto" :padding-top "4px" :overflow-x "hidden"} :child
         [re-com/v-box
          :gap "10px"
          :children
          (for [grp click-params] ;; [grp (if @db/param-code-hover [{}] click-params)]
            (let [filtered-params (into {}
                                        (filter
                                         #(and ;; filter out from the top bar toggle first....
                                           (not (= (str (first %)) ":/")) ;; sometimes a garbo
                                                                            ;; param sneaks in
                                           (not (cstr/starts-with? (str (first %)) ":solver-meta/"))
                                           (not (cstr/starts-with? (str (first %)) ":repl-ns/"))
                                           (not (cstr/starts-with? (str (first %)) ":panel-hash/"))
                                           (not (cstr/starts-with? (str (first %)) ":solver-status/"))
                                           (not (cstr/starts-with? (str (first %)) ":signal/"))
                                           (and (not (cstr/starts-with? (str (first %)) ":server/"))
                                                (not (cstr/ends-with? (str (first %)) "-chart")))
                                           (not (and (cstr/starts-with? (str (first %)) ":solver/")
                                                     (re-matches #".*\d" (str (first %)))))
                                           (not (cstr/ends-with? (str (first %)) "*running?"))
                                           (not (cstr/ends-with? (str (first %)) "/selected-view"))
                                           (not (cstr/ends-with? (str (first %)) "/selected-block"))
                                           (not (cstr/ends-with? (str (first %)) "/selected-view-data"))
                                           (not (nil? (last %))) ;; no nil val params 
                                           (not (cstr/starts-with? (str (first %)) ":conn-list/"))
                                           (not (cstr/starts-with? (str (first %)) ":time/"))
                                           (not (cstr/starts-with? (str (first %)) ":sys/"))
                                           (not (cstr/starts-with? (str (first %)) ":client/"))
                                           (not (cstr/starts-with? (str (first %)) (if (get pf :theme) ":theme/***" ":theme/")))
                                           (not (cstr/starts-with? (str (first %)) (if (get pf :user) ":param/***" ":param/")))
                                           (not (cstr/starts-with? (str (first %))
                                                                   (if (get pf :condis) ":condi/***" ":condi/")))
                                           (if (get pf :this-tab? true)
                                             (or
                                              (some (fn [x] (= (str x) (str (first %)))) valid-params-in-tab) ;; TODO this is redundant and wasteful w slices here
                                              (some (fn [x] (cstr/starts-with? (str (first %)) (str ":" x))) current-tab-slices)
                                              (some (fn [x] (cstr/starts-with? (str (first %)) (str ":" x))) current-tab-queries))
                                             true)
                                           (not (cstr/includes? (str (first %)) "-sys/")))
                                         grp))
                  ;;_ (tapp>>  [:grp click-params valid-params-in-tab])
                  searches?       (ut/ne? @searcher-atom)
                  filter-keys     (if searches?
                                    (vec (filter-search @searcher-atom (keys (into {} grp))))
                                    (vec (keys filtered-params)))
                  filtered-params (select-keys (into {} grp) filter-keys)
                  filtered-params (if @db/cm-focused? (select-keys grp clover-params) filtered-params)
                  is-not-empty?   (ut/ne? (remove empty? filtered-params))]
              ;;(tapp>>  [:param-counts (count (keys filtered-params))])
              (when is-not-empty?
                [re-com/v-box :style {:cursor "grab" :background-color (theme-pull :theme/editor-param-background-color nil)}
                 :children
                 (for [[k v] filtered-params]
                   (let [psplit        (ut/splitter (ut/safe-name (str k)) "/")
                         table         (-> (first psplit)
                                           (ut/replacer #":" "")
                                           ;(ut/replacer ".*" "") ;; was stopping deletion. do we need this for something else? 7/19/24
                                           keyword)
                         field         (keyword (last psplit))
                         param-has-fn? (try (and (= (first v) :run-solver) (= table :param)) (catch :default _ false))
                                          ;;param-value (str @(ut/tracked-subscribe [::conn/clicked-parameter [table field]]))
                         param-value   @(ut/tracked-sub ::conn/clicked-parameter-key-alpha
                                                        {:keypath [(keyword (cstr/join "/"
                                                                                       (map #(cstr/replace (str %) ":" "")
                                                                                            [table field])))]})
                         v             (str (or param-value v))
                         meta          @(ut/tracked-sub ::meta-from-param-name {:param-name k})
                         dtype         (if param-has-fn? (ut/data-typer param-value) (get meta :data-type))
                                         ;;  dtype         (try (if (and (= dtype "vector") (every? string? v)) "string" dtype)
                                         ;;                     (catch :default _ dtype)) ;; since stringified code is
                         dcolor        (get @(ut/tracked-sub ::conn/data-colors {}) dtype)
                                         ;;  _ (tapp>> [:params dtype dcolor v param-value])
                         param-value   (str param-value) ;; since the rest expects it

                                          ;;  _ (ut/tapp>> [[table field]  param-value])
                         is-map?       (or (= dtype "map") (= dtype "vector")) ;;assoc-happy data
                         is-image?     (and (or (cstr/ends-with? (cstr/lower-case (str param-value)) ".png")
                                                (cstr/ends-with? (cstr/lower-case (str param-value)) ".webp")
                                                (cstr/ends-with? (cstr/lower-case (str param-value)) ".jpg")
                                                (cstr/ends-with? (cstr/lower-case (str param-value)) ".jpeg")
                                                (cstr/ends-with? (cstr/lower-case (str param-value)) ".gif")
                                                (cstr/includes? (cstr/lower-case (str param-value)) "/images/")
                                                (cstr/includes? (cstr/lower-case (str param-value)) "/image/"))
                                            (or
                                             (cstr/starts-with? (cstr/lower-case (str param-value)) "http")
                                             (cstr/starts-with? (cstr/lower-case (str param-value)) "./images")))
                         is-b64?       (ut/is-base64? (str param-value))
                         is-video?     (or (cstr/ends-with? (cstr/lower-case (str param-value)) ".mp4")
                                           (cstr/ends-with? (cstr/lower-case (str param-value)) ".mov"))
                         selected?     @(ut/tracked-subscribe [::has-query? selected-block table])
                         pwidth        (js/Math.floor (/ (count (str param-value)) 1.7))
                         pwidth        (cond (> pwidth 30) 30
                                             (< pwidth 6)  6
                                             :else         pwidth)
                         pheight       (js/Math.floor (/ pwidth 30))
                         pheight       (cond (> pheight 3) 3
                                             (< pheight 1) 1
                                             :else         pheight)
                         hovered?      (= k (first @param-hover))]
                     (draggable ;(sql-spawner-filter :param [k table field])
                      {:h         (cond is-image? 6
                                        is-video? 9
                                        is-map? 9
                                        :else     (+ 2 pheight))
                       :w         (cond is-image? 6
                                        is-video? 13
                                        is-map? 9
                                        :else     pwidth)
                       :root      [0 0]
                       :drag-meta {:type :param :param-full k :param-type dtype :param-table table :param-field field}}
                      "meta-menu"
                      [re-com/v-box :size "auto" :attr
                       {:on-mouse-enter #(reset! param-hover [k table field]) :on-mouse-leave #(reset! param-hover nil)}
                       :style {:border           (str "1px solid " dcolor)
                               :background-color (if hovered? (str dcolor 55) (if selected? (str dcolor 20) "inherit"))} :children
                       [[re-com/h-box :justify :between :children
                         [[re-com/box :child
                           (str (if hovered?
                                  (let [chars (count (str k))
                                        len   25
                                        wide? (> chars len)]
                                    (if wide? (str (subs (str k) 0 len) "...") (str k)))
                                  (str k))
                                (when param-has-fn? " *ƒ"))
                           :style {:color dcolor :font-weight 700 :font-size "13px"
                                   :padding-left "4px" :padding-right "4px"}]

                          (if hovered?
                            [re-com/h-box
                             :gap "4px"
                             :children
                             [[re-com/md-icon-button
                               :md-icon-name "zmdi-copy"
                               :style {:color        dcolor
                                       :padding      "0px"
                                       :margin-top   "-2px"
                                       :margin-right "3px"
                                       :font-size    "14px"
                                       :height       "15px"}
                               :on-click #(ut/tracked-dispatch [::conn/copy-to-clipboard (str k)])]
                              [re-com/md-icon-button
                               :md-icon-name "fa-regular fa-trash-can" ;; "zmdi-close"
                               :style {:color        dcolor
                                       :padding      "0px"
                                       :margin-top   "-2px"
                                       :margin-right "3px"
                                       :font-size    "14px"
                                       :height       "15px"}
                               :on-click #(ut/tracked-dispatch [::conn/declick-parameter [table field]])]]]
                            [re-com/box :child (str dtype) :style
                             {:color dcolor :font-size "10px" :padding-top "3px" :padding-left "4px" :padding-right "4px"}])]]
                        [re-com/box :child
                         (if (scrub/hex-color? v)
                           [re-com/h-box :gap "7px" :children
                            [(str v)
                             [re-com/box :src (at) :child " " :size "auto" :width "15px" :height "15px" :style
                              {:background-color (str v) :margin-top "2px" :padding-left "3px" :padding-right "3px"}]]]
                           (if (nil? v)
                             "NULL"
                             (cond (= dtype "map") "{ ... map hidden ... }"
                                   is-b64?         "**huge base64 string**"
                                   :else           (str v)))) :style
                         (merge {:color          (str (theme-pull :theme/editor-font-color nil) 99) ; "#ffffff99"
                                 :font-size      "13px"
                                 :font-weight    700
                                 :padding-left   "4px"
                                 :padding-right  "4px"
                                 :padding-bottom "2px"}
                                (if (nil? v) {:font-style "italic" :opacity 0.3} {})) :align :end]]])))])))]]]])))




(defonce query-hover (reagent/atom nil))

(re-frame/reg-sub ::queries-in-this-tab
                  (fn [db _]
                    (try (vec (apply concat
                                     (for [[_ v] (get db :panels)
                                           :let  [tab (get db :selected-tab)]
                                           :when (= tab (get v :tab))]
                                       (for [[qk _] (get v :queries)] qk))))
                         (catch :default _ nil))))

(defn screen-query-browser
  [width-int height-int]
  (let [ph                  @query-hover ;; reaction hack
        queries-in-this-tab @(ut/tracked-subscribe [::queries-in-this-tab])
        selected-block      @(ut/tracked-subscribe [::selected-block])
        queries             queries-in-this-tab] ;; TODO clean up, incoming queries is unneeded
    [re-com/box :size "none" :height (px (- height-int 70)) :style {:overflow-y "auto" :overflow-x "hidden" :margin-left "-22px"}
     :child
     [re-com/v-box :style
      {:background-color (theme-pull :theme/editor-param-background-color nil) ;; "#0b1122"
       :margin-top       "4px"
       :padding-right    "2px"} :children
      (for [k (filter #(not (cstr/starts-with? (str %) ":query-preview")) queries)]
        (let [;meta @(ut/tracked-subscribe [::meta-from-query-name k])
              meta                @(ut/tracked-subscribe [::conn/sql-merged-metadata [k]])
              rows                (nf (get meta :rowcount))
              fields              (count (get meta :fields))
              query-panel         @(ut/tracked-subscribe [::panel-from-query-name k])
              subq-blocks         @(ut/tracked-sub ::subq-panels-alpha {:panel-id selected-block})
              subq-mapping        @(ut/tracked-sub ::subq-mapping-alpha {})
              parent-of-selected? (some #(= % query-panel) subq-blocks)
              upstream?           (some #(= % query-panel) (ut/cached-upstream-search subq-mapping selected-block))
              downstream?         (some #(= % query-panel) (ut/cached-downstream-search subq-mapping selected-block))
              selected?           @(ut/tracked-subscribe [::has-query? selected-block k])]
          [re-com/v-box :attr
           {:on-mouse-enter #(reset! query-hover k)
            :on-mouse-leave #(reset! query-hover nil)
            :on-click       #(ut/tracked-dispatch [::select-block query-panel])} :style
           {:border           (cond ;(= selected-block query-panel) "2px solid #9973e0"
                                (= selected-block query-panel) (str "2px solid " (theme-pull :theme/universal-pop-color "#9973e0"))
                                parent-of-selected?            "2px solid #e6ed21"
                                upstream?                      "2px solid #7be073"
                                downstream?                    "2px dashed #05dfff"
                                :else                          (str "1px solid "
                                                                    (theme-pull :theme/editor-font-color nil)
                                                                    33))
            :color            (if (or selected? (= k @query-hover))
                                (theme-pull :theme/grid-selected-font-color nil)
                                (str (theme-pull :theme/editor-font-color nil) 99))
            :cursor           "pointer"
            :background-color (if (= k @query-hover)
                                (str (theme-pull :theme/grid-selected-background-color nil) 55)
                                (if selected? (theme-pull :theme/grid-selected-background-color nil) "inherit"))} :children
           [[re-com/h-box :justify :between :children
             [[re-com/box :child (str k) :style
               {;:color        (str (theme-pull :theme/editor-font-color nil) 99) ;"#ffffff99"
                :font-weight   700
                :font-size     "13px"
                :padding-left  "4px"
                :padding-right "4px"}]]]
            [re-com/h-box :justify :between :children
             [[re-com/box :child (str fields " fields") :style
               {;:color "#ffffff33"
                :font-size      "13px"
                :font-weight    500
                :padding-left   "4px"
                :padding-right  "4px"
                :padding-bottom "2px"} :align :end]
              [re-com/box :child (str rows " rows") :style
               {;:color "#ffffff33"
                :font-size      "13px"
                :font-weight    500
                :padding-left   "4px"
                :padding-right  "4px"
                :padding-bottom "2px"} :align :end]]]]]))]]))

(re-frame/reg-sub ::panel-counts
                  (fn [db [_ panel-key]]
                    (let [views   (count (keys (get-in db [:panels panel-key :views])))
                          runners (count (keys @(ut/tracked-sub ::panel-runners-only {:panel-key panel-key})))
                          queries (count (keys (get-in db [:panels panel-key :queries])))]
                      [views queries runners])))

(re-frame/reg-sub ::panel-name-only (fn [db {:keys [panel-key]}] (get-in db [:panels panel-key :name] (str panel-key))))


(defn draggable-intro
  [element name-space item-name item-type item-value data-key-type]
  [(reagent/adapt-react-class rdnd/Draggable)
   (let [vname (ut/safe-key (keyword (str item-name (rand-int 45))))
         client-name @(ut/tracked-sub ::client-name {})
         gen-namespace (str "repl-" (cstr/replace (str client-name) ":" "") "-" (cstr/replace (str data-key-type) ":" ""))
         is-gen-namespace? (true? (= gen-namespace name-space))
         ;;name-space (if is-gen-namespace? (str "repl-" ))
         code (cond (= item-type ":atom")
                    (str "(do "
                         (when (not is-gen-namespace?) (str " (ns " name-space ") "))
                         " (let [last-run :repl-ns/" name-space ">introspected] (println last-run) (deref " item-name ")))")

                    (= item-type ":sqlized")
                    (assoc item-value :connection-id :*client-name*)

                    :else (str "(do "
                               (when (not is-gen-namespace?) (str " (ns " name-space ") "))
                               " (let [last-run :repl-ns/" name-space ">introspected] (println last-run) " item-name "))"))
         data-key-type (if (= item-type ":sqlized") :queries data-key-type)
         data {:h         4
               :w         7
               :selected-view vname
               :connection-id client-name
               :selected-mode (when (not (= item-type ":sqlized"))  {vname :edn})
               :drag-meta {:source-table :hi :table-fields [:*] :connection-id nil :source-panel-key :block-7034 :type :view}
               ;:views     {vname [:box :child (str  name-space " " item-name " " item-type " " data-key-type )]}
               data-key-type {vname code}
               :name      (str "introspected " (ut/replacer item-name ":" ""))}]
     {:type          "meta-menu" ;:play-port
      :on-drag-end   #(do (reset! dragging? false))
      :on-drag-start #(do (reset! dragging? true)
                          (reset! dragging-size [(get data :w) (get data :h)])
                          (reset! dragging-body data))
      :data          (pr-str data)})
   [re-com/box
    :size "none"
    :child element
    :style {:cursor "grab"}]])


(defn nrepl-introspection-browser [selected-block data-key-type data-key width-int height-int editor-dims]
  (let [;ph             @query-hover ;; reaction hack
       ;; {:keys [single-width-bricks single-height-bricks single-width single-height bricks-wide bricks-tall]} editor-dims
      ;selected-block @(ut/tracked-sub ::selected-block {})
      ;blocks         (map keyword @(ut/tracked-sub ::current-tab-blocks {}))
        are-solver        (get @db/solver-fn-lookup [:panels selected-block data-key])
        meta-data          (when are-solver
                             @(ut/tracked-sub ::conn/clicked-parameter-key-alpha
                                              {:keypath [(keyword (str (ut/replacer are-solver
                                                                                    ":solver/" "solver-meta/")))]}))
        client-name       @(ut/tracked-sub ::client-name {})
        name-space        (get-in meta-data [:output :evald-result :ns])
        ;ns-clover         (keyword (str "repl-ns/" name-space))
        intro-map-all     @(ut/tracked-sub ::conn/clicked-parameter-key-alpha
                                           {:keypath [(keyword (str "repl-ns/" (cstr/replace (str client-name) ":" "")))]})
        intro-map         (get intro-map-all name-space)
        ;intro-map         (when are-solver
        ;                    @(ut/tracked-sub ::conn/clicked-parameter-key-alpha
        ;                                     {:keypath [ns-clover]}))
        ;intro-at (get intro-map :introspected)
        _ (tapp>> [:intro-map intro-map])
        intro-map (vec (apply concat (for [[k v] (dissoc intro-map :introspected)]
                                       (for [[kk vv] v]
                    ;;(merge {:type k} vv)

                                         (assoc {:item-value vv :item-name kk} :repl-type k)
                     ;;vv
                                         ))))
      ;; rdata             (when are-solver
      ;;                     @(ut/tracked-sub ::conn/clicked-parameter-key-alpha
      ;;                                      {:keypath [(keyword (str (ut/replacer are-solver
      ;;                                                                            ":" "")))]}))
      ;; running-status    (when are-solver
      ;;                     @(ut/tracked-sub ::conn/clicked-parameter-key-alpha
      ;;                                      {:keypath [(keyword (str (ut/replacer are-solver
      ;;                                                                            ":solver/" "solver-status/*client-name*>")))]}))
        ]

    ;; (tapp>> [:intro-map intro-map])

  ;[re-com/box :child (str intro-map)]  

    [re-com/box
     :size "none"
     :height (px (- height-int 70))
     :style {:overflow-y "auto"
             :overflow-x "hidden"
             :margin-left "-22px"}
     :child
     [re-com/v-box
      :style {:background-color (theme-pull :theme/editor-param-background-color nil) ;; "#0b1122"
              :margin-top       "4px"
              :padding-right    "2px"}
      :children (for [k intro-map]
                  (let [{:keys [item-value item-name repl-type]} k
                      ;meta @(ut/tracked-subscribe [::meta-from-query-name k])
                      ;nname               @(ut/tracked-sub ::panel-name-only {:panel-key k})
                      ;[rows fields]       @(ut/tracked-subscribe [::panel-counts k])
                      ;query-panel         k ;@(ut/tracked-subscribe [::panel-from-query-name k])
                      ;subq-blocks         @(ut/tracked-sub ::subq-panels-alpha {:panel-id selected-block})
                      ;subq-mapping        @(ut/tracked-sub ::subq-mapping-alpha {})
                      ;parent-of-selected? (some #(= % query-panel) subq-blocks)
                      ;upstream?           (some #(= % query-panel) (ut/cached-upstream-search subq-mapping selected-block))
                      ;downstream?         (some #(= % query-panel) (ut/cached-downstream-search subq-mapping selected-block))
                      ;selected?           @(ut/tracked-subscribe [::has-query? selected-block k])
                        dtype (ut/data-typer item-value)
                        dcolor (get @(ut/tracked-sub ::conn/data-colors {}) dtype)]

                    [draggable-intro
                     [re-com/v-box
                      :size "auto"
                      :attr
                      {;:on-mouse-enter #(reset! query-hover k)
                    ;:on-mouse-leave #(reset! query-hover nil)
                    ;:on-click       #(ut/tracked-dispatch [::select-block query-panel])
                       }
                      :style
                      {;:border           (cond (= selected-block query-panel) (str "2px solid " (theme-pull :theme/universal-pop-color "#9973e0"))
                    ;                        parent-of-selected?            "2px solid #e6ed21"
                    ;                        upstream?                      "2px solid #7be073"
                    ;                        downstream?                    "2px dashed #05dfff"
                    ;                        :else                          (str "1px solid "
                    ;                                                            (theme-pull :theme/editor-font-color nil)
                    ;                                                            33))
                       :border          (str "1px solid " dcolor)
                       :color dcolor
                    ;:color            (if (or selected? (= k @query-hover))
                    ;                    (theme-pull :theme/grid-selected-font-color nil)
                    ;                    (str (theme-pull :theme/editor-font-color nil) 99))
                       ;:cursor           "pointer"
                       :background-color (str dcolor 55)
                    ;:background-color (if (= k @query-hover)
                    ;                    (str (theme-pull :theme/grid-selected-background-color nil) 55)
                    ;                    (if selected? (theme-pull :theme/grid-selected-background-color nil) "inherit"))
                       }
                      :children
                      [[re-com/h-box
                        :justify :between
                        :size "auto"
                       ;:width (px (* single-width 0.3))
                        :children
                        [[re-com/box :child (str item-name)
                          :size "auto"
                          :style {:font-weight 700
                                  :font-size "13px"
                                  :padding-left "4px"
                                  :padding-right "4px"}]
                         [re-com/box :child (str dtype) ;:size "auto" 
                          :style {:font-weight 700
                                  :font-size "13px"
                                  :padding-left "4px"
                                  :padding-right "4px"}]]]
                       [re-com/h-box :justify :between :children
                        [[re-com/box :child (str repl-type) :style
                          {:font-size "13px" :font-weight 500 :padding-left "4px" :padding-right "4px" :padding-bottom "2px"} :align :end]
                         [re-com/box
                         ;:child (str item-value) 
                          :child (let [str-val (str item-value)
                                       limit 28]
                                   (if (<= (count str-val) limit)
                                     str-val
                                     (str (subs str-val 0 limit) "...")))
                          :style {:font-size "13px" :font-weight 500 :padding-left "4px" :padding-right "4px" :padding-bottom "2px"} :align
                          :end]]]]]  name-space item-name repl-type item-value data-key-type]))]]))

(defn screen-block-browser
  [width-int height-int]
  (let [ph             @query-hover ;; reaction hack
        selected-block @(ut/tracked-sub ::selected-block {})
        blocks         (map keyword @(ut/tracked-sub ::current-tab-blocks {}))]
    [re-com/box :size "none" :height (px (- height-int 70)) :style {:overflow-y "auto" :overflow-x "hidden" :margin-left "-22px"}
     :child
     [re-com/v-box :style
      {:background-color (theme-pull :theme/editor-param-background-color nil) ;; "#0b1122"
       :margin-top       "4px"
       :padding-right    "2px"} :children
      (for [k (filter #(not (cstr/includes? (str %) "query-preview")) blocks)]
        (let [;meta @(ut/tracked-subscribe [::meta-from-query-name k])
              nname               @(ut/tracked-sub ::panel-name-only {:panel-key k})
              [rows fields runners]       @(ut/tracked-subscribe [::panel-counts k])
              query-panel         k ;@(ut/tracked-subscribe [::panel-from-query-name k])
              subq-blocks         @(ut/tracked-sub ::subq-panels-alpha {:panel-id selected-block})
              subq-mapping        @(ut/tracked-sub ::subq-mapping-alpha {})
              minimized?          @(ut/tracked-sub ::panel-minimized? {:panel-key k})
              parent-of-selected? (some #(= % query-panel) subq-blocks)
              upstream?           (some #(= % query-panel) (ut/cached-upstream-search subq-mapping selected-block))
              downstream?         (some #(= % query-panel) (ut/cached-downstream-search subq-mapping selected-block))
              selected?           @(ut/tracked-subscribe [::has-query? selected-block k])]
          [re-com/v-box :attr
           {:on-mouse-enter #(reset! query-hover k)
            :on-mouse-leave #(reset! query-hover nil)
            :on-click       #(ut/tracked-dispatch [::select-block query-panel])} :style
           {:border           (cond (= selected-block query-panel) (str "2px solid " (theme-pull :theme/universal-pop-color "#9973e0"))
                                    parent-of-selected?            "2px solid #e6ed21"
                                    upstream?                      "2px solid #7be073"
                                    downstream?                    "2px dashed #05dfff"
                                    :else                          (str "1px solid "
                                                                        (theme-pull :theme/editor-font-color nil)
                                                                        33))
            :color            (if (or selected? (= k @query-hover))
                                (theme-pull :theme/grid-selected-font-color nil)
                                (str (theme-pull :theme/editor-font-color nil) 99))
            :cursor           "pointer"
            :background-color (if (= k @query-hover)
                                (str (theme-pull :theme/grid-selected-background-color nil) 55)
                                (if selected? (theme-pull :theme/grid-selected-background-color nil) "inherit"))} :children
           [[re-com/h-box :justify :between :children
             [[re-com/box
               :min-height "26px"
               :child (if minimized?
                        [re-com/h-box
                         :size "auto"
                         :justify :between :children [[re-com/box :child (str nname)] [re-com/box :child "(minimized)" :style {:opacity 0.6}]]]
                        (str nname))
               :size "auto"
               :style {:font-weight 700 :font-size "13px" :padding-left "4px" :padding-right "4px" :opacity (when minimized? 0.5)}]]]
            (when (not minimized?)
              [re-com/h-box :justify :between :children
               [[re-com/box :child (str fields (if (= fields 1) " query" " queries")) :style
                 {:font-size "13px" :font-weight 500 :padding-left "4px" :padding-right "4px" :padding-bottom "2px" :opacity (when (= fields 0) 0.4)} :align :end]
                [re-com/box :child (str runners (if (= runners 1) " runner" " runners")) :style
                 {:font-size "13px" :font-weight 500 :padding-left "4px" :padding-right "4px" :padding-bottom "2px" :opacity (when (= runners 0) 0.4)} :align :end]
                [re-com/box :child (str rows (if (= rows 1) " view" " views")) :style
                 {:font-size "13px" :font-weight 500 :padding-left "4px" :padding-right "4px" :padding-bottom "2px" :opacity (when (= rows 0) 0.4)} :align
                 :end]]])]]))]]))

(defn sql-spawner
  [type panel-map data-keypath target-id source-panel-key]
  (let [sql-name-base        (ut/replacer (str (ut/safe-name target-id) "-drag-" (rand-int 45)) #"_" "-")
        query-meta           @(ut/tracked-subscribe [::conn/sql-metadata data-keypath])
        field-data-type      (get-in query-meta [:fields target-id :data-type])
        parent-sql-alias     (keyword (str "query/" (ut/safe-name (first data-keypath))))
        parent-sql-sql-alias (ut/gen-sql-sql-alias)
        sqlized-hash         (get-in panel-map [:queries (first data-keypath) :_sqlized-hash])
        agg? ;(and (= type :meta-fields)
        (and (or (= field-data-type "integer") (= field-data-type "float"))
             (not (cstr/includes? (cstr/lower-case (str target-id)) "year"))
             (not (cstr/includes? (cstr/lower-case (str target-id)) "month"))
             (not (cstr/includes? (cstr/lower-case (str target-id)) "week")))
        sql-name             (keyword sql-name-base)
        connection-id        (get-in panel-map [:queries (first data-keypath) :connection-id] (get panel-map :connection-id))
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
                                               {sql-name (merge (when sqlized-hash {:_sqlize-hash sqlized-hash})
                                                                {:select [[[:sum target-id]
                                                                           (keyword (str (ut/safe-name target-id) "_sum"))]]
                                                                 :from   [[parent-sql-alias parent-sql-sql-alias]]})}
                                               {sql-name (merge (when sqlized-hash {:_sqlize-hash sqlized-hash})
                                                                {:select   [target-id [[:count 1] :rowcnt]]
                                                          :from     [[parent-sql-alias parent-sql-sql-alias]]
                                                          :group-by [target-id]
                                                          :order-by [[:rowcnt :desc]]})})}]
    (condp = type :field general-field)))

(defn sql-spawner-where
  [type panel-map data-keypath field-name source-panel-key & [row-num]]
  (let [sql-name-base        (ut/replacer (str (ut/safe-name field-name) "-drag-" (rand-int 45)) #"_" "-")
        selected-field       @(ut/tracked-subscribe [::conn/clicked-parameter data-keypath])
        ;;selected-field       @(ut/tracked-sub ::conn/clicked-parameter-alpha {:keypath data-keypath})
        query-meta           @(ut/tracked-subscribe [::conn/sql-metadata data-keypath])
        field-data-type      (get-in query-meta [:fields field-name :data-type])
        parent-sql-alias     (keyword (str "query/" (ut/safe-name (first data-keypath))))
        parent-sql-sql-alias (ut/gen-sql-sql-alias)
        selected-fields      (vec (keys (get query-meta :fields)))
        flow-item?           (= (first data-keypath) :flow-fn-sys)
        new-flow-map         (when flow-item?
                               (let [pval   @(ut/tracked-subscribe [::conn/clicked-parameter [:flow-fn-sys]])
                                     ;;pval   @(ut/tracked-sub ::conn/clicked-parameter-alpha {:keypath [:flow-fn-sys]})
                                     modded (conn/spawn-open-input-block pval)
                                     body   (get modded 2)]
                                 body))
        sqlized-hash         (get-in panel-map [:queries (first data-keypath) :_sqlized-hash])
        flow-item            (when flow-item?
                               (try (merge (-> (edn/read-string (get selected-field :full_map)))
                                           selected-field)
                                    (catch :default _ {})))
        sql-name             (keyword sql-name-base)
        general-field        {:h             6
                              :w             5
                              :flow-item     (select-keys flow-item
                                                          [:category :name :type :icon :defaults :types :style :selected-style
                                                           :inputs :expandable? :required])
                              :drag-meta     (if flow-item
                                               {:type (try (edn/read-string (get flow-item :name))
                                                           (catch :default _ (get flow-item :name)))}
                                               {:type             :where
                                                :param-full       (get selected-field field-name) ;{field-name
                                                :param-table      (first data-keypath) ; table
                                                :param-field      field-name
                                                :target           field-name
                                                :row-num          row-num
                                                :connection-id    (get-in panel-map [:queries (first data-keypath) :connection-id] (get panel-map :connection-id))
                                                :data-type        field-data-type
                                                :source-table     parent-sql-alias
                                                :source-query     (first data-keypath)
                                                :source-panel-key source-panel-key})
                              :source-panel  source-panel-key
                              :connection-id (get-in panel-map [:queries (first data-keypath) :connection-id] (get panel-map :connection-id))
                              :name          (str "drag-from-" (get panel-map :name))
                              :queries       {sql-name (merge (when sqlized-hash {:_sqlized-hash sqlized-hash})
                                                              {:select selected-fields ;[:*]
                                                               :from   [[parent-sql-alias parent-sql-sql-alias]]
                                                               :where  [:= field-name (get selected-field field-name)]})}}
        general-field        (if flow-item?
                               (let [inputs    (vec (keys (get-in new-flow-map [:ports :in])))
                                     flow-item (assoc (get-in new-flow-map [:data :flow-item]) :inputs inputs)
                                     drag-meta (get-in new-flow-map [:data :drag-meta])] ;; re-re-package
                                 (-> new-flow-map
                                     (dissoc :data)
                                     (assoc :flow-item flow-item)
                                     (assoc :drag-meta drag-meta)))
                               general-field)]
    general-field))

(defn sql-spawner-chat
  [query-src query-tgt & [name h w]]
  (let [sql-name-base (ut/replacer (str "chat-drag-" (rand-int 45)) #"_" "-")
        panel-id      @(ut/tracked-subscribe [::panel-from-query-name query-src])
        panel-map     @(ut/tracked-subscribe [::panel-map panel-id])
        both-keys?    (and (not (nil? (get query-tgt :views))) (not (nil? (get query-tgt :queries))))
        sql-name      (keyword sql-name-base)
        general-field {:h             (or h 6)
                       :w             (or w 5)
                       :drag-meta     {:type :chat :connection-id (get panel-map :connection-id (get-in panel-map [:queries query-src :connection-id]))}
                       :connection-id (get panel-map :connection-id (get-in panel-map [:queries query-src :connection-id]))
                       :name          (or name (str "chat-sql-" (get panel-map :name)))
                       :queries       (if both-keys? (get query-tgt :queries) {sql-name query-tgt})
                       :views         (when both-keys? (get query-tgt :views))}]
    general-field))

(defn view-spawner-chat
  [kp query-tgt]
  (let [;sql-name-base        (ut/replacer (str "chat-drag-" (rand-int 45)) #"_" "-")
        panel-id (first kp) ;@(ut/tracked-subscribe [::panel-from-query-name query-src])
        panel-map @(ut/tracked-subscribe [::panel-map panel-id])
        view-name (last kp)
        view-name (if (nil? view-name) (ut/safe-key (keyword "new")) view-name)
        general-field
        {:h 4 :w 6 :drag-meta {:type :chat} :name (str "chat-view-" (get panel-map :name)) :views {view-name query-tgt}}]
    general-field))


(defn sql-spawner-meta
  [type]
  (let [selected-meta-db      @(ut/tracked-subscribe [::conn/clicked-parameter [:connections-sys]]) ;;
        selected-meta-table   @(ut/tracked-subscribe [::conn/clicked-parameter [:tables-sys]])
        selected-meta-field   @(ut/tracked-subscribe [::conn/clicked-parameter [:fields-sys]])

        ;selected-meta-db      @(ut/tracked-sub ::conn/clicked-parameter-alpha {:keypath [:connections-sys]})
        ;selected-meta-field   @(ut/tracked-sub ::conn/clicked-parameter-alpha {:keypath [:tables-sys]})
        ;selected-meta-table   @(ut/tracked-sub ::conn/clicked-parameter-alpha {:keypath [:fields-sys]})

        selected-fields       (vec (map ut/sql-keyword
                                        (map :field_name @(ut/tracked-sub ::conn/sql-data-alpha {:keypath [:fields-sys]}))))
        parent-sql-sql-alias  (ut/gen-sql-sql-alias)
        field-name            (get selected-meta-field :field_name)
        table-name            (get selected-meta-table :table_name)
        rando                 (rand-int 45)
        sql-name-base         (cond (= type :meta-fields) (ut/replacer (str (ut/safe-name table-name)
                                                                            "-drag-" (ut/safe-name field-name)
                                                                            "-"      rando)
                                                                       #"_"
                                                                       "-")
                                    :else                 (ut/replacer (str (ut/safe-name table-name) "-drag-" rando) #"_" "-"))
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
                                    :else                              table-name)
        agg?                  (and (= type :meta-fields)
                                   (or (= field-data-type "integer") (= field-data-type "float"))
                                   (not (cstr/includes? (cstr/lower-case (str field-name)) "year"))
                                   (not (cstr/includes? (cstr/lower-case (str field-name)) "month"))
                                   (not (cstr/includes? (cstr/lower-case (str field-name)) "week")))
        dyn-width             (let [x (js/Math.ceil (* (count selected-fields) 2.2))] (if (>= x 30) 30 x))
        tables-panel          {:h             7
                               :w             dyn-width
                               :drag-meta     drag-meta
                               :connection-id connection-id
                               :name          (str "select-all-" table-name)
                               :queries       {sql-name {:select selected-fields ;[:*] ;;explicit
                                                                                 ;field
                                                         :from   [[(keyword qual-table-name) parent-sql-sql-alias]]}}}
        meta-fields-panel     {:h             5
                               :w             5
                               :drag-meta     drag-meta
                               :connection-id connection-id
                               :name          (str "select-" field-name "-" table-name)
                               :queries       {sql-name {:select   [(keyword field-name) [[:count 1] :rowcnt]]
                                                         :from     [[(keyword qual-table-name) parent-sql-sql-alias]]
                                                         :group-by [(keyword field-name)]
                                                         :order-by [[:rowcnt :desc]]}}}
        meta-screen-load      {:file-path (get @(ut/tracked-subscribe [::conn/clicked-parameter [:files-sys]]) :file_path)
                               :drag-meta {:type type}}
        meta-block-load       (merge (read-string @(ut/tracked-sub ::conn/clicked-parameter-key-alpha
                                                                   {:keypath [:blocks-sys/block_data]}))
                                     (let [bk (get @(ut/tracked-subscribe [::conn/clicked-parameter [:blocks-sys]]) :block_key)
                                           bn (get @(ut/tracked-subscribe [::conn/clicked-parameter [:blocks-sys]]) :block_name)]
                                       {:file-path (get @(ut/tracked-subscribe [::conn/clicked-parameter [:files-sys]])
                                                        :file_path)
                                        :block-key bk
                                        :drag-meta {:type (cond (= bk ":*theme*")                      :meta-theme
                                                                (cstr/starts-with? (str bn) "board: ") :meta-board
                                                                :else                                  type)}}))
       ;;; _ (when (= type :viz-reco)     (ut/tracked-dispatch [::update-reco-previews]))
        meta-fields-agg-panel {:h             5
                               :w             5
                               :drag-meta     drag-meta
                               :connection-id connection-id
                               :name          (str "select-" field-name "-" table-name)
                               :queries       {sql-name {:select [[[:sum (keyword field-name)] (keyword (str field-name "_sum"))]]
                                                         :from   [(keyword qual-table-name)]}}}]

     ;; update counts so we can do viz-gen 

    (cond (= type :viz-reco)     (let [;; we need to change all the generic aliases into real rando keys
                                       req-name-str     (str "block-" (rand-int 999))
                                       ddata            @(ut/tracked-subscribe [::panel-map :reco-preview])
                                       poss-value-walks (filter #(or (cstr/starts-with? (str %) ":query-preview")
                                                                     (cstr/starts-with? (str %) ":panel-key"))
                                                                (ut/deep-flatten ddata))
                                       key-walk1        (filter #(not (cstr/includes? (str %) "/")) poss-value-walks)
                                       key-walk         (merge (into {}
                                                                     (for [k key-walk1]
                                                                       {k (keyword (str "gen-viz-" (rand-int 1234)))}))
                                                               {:panel-key (keyword req-name-str)})
                                       value-walk1      (filter #(cstr/includes? (str %) "/") poss-value-walks)
                                       value-walk       (into {}
                                                              (for [v value-walk1]
                                                                (let [vv   (ut/safe-name v)
                                                                      vs   (ut/splitter vv "/")
                                                                      qstr (ut/safe-name (first (ut/postwalk-replacer
                                                                                                 key-walk
                                                                                                 [(keyword (first vs))])))
                                                                      vstr (last vs)]
                                                                  (ut/tapp>> [:value-walk vv vs qstr vstr v
                                                                              (keyword (str qstr "/" vstr))])
                                                                  {v (keyword (str qstr "/" vstr))})))]
                                   (merge (ut/postwalk-replacer (merge key-walk value-walk) ddata)
                                          {;:mad-libs-viz []
                                           :drag-meta {:type type :block-name (keyword req-name-str)}}))
          (= type :meta-tables)  tables-panel
          (= type :meta-fields)  (if agg? meta-fields-agg-panel meta-fields-panel)
          (= type :meta-screens) meta-screen-load
          (= type :meta-blocks)  meta-block-load
          :else                  (let [;; same as viz-reco but with dynamic combo id via panel lookup
                                       req-name-str     (str "block-" (rand-int 999))
                                       ddata            @(ut/tracked-subscribe [::panel-map type])
                                       poss-value-walks (filter #(or (cstr/starts-with? (str %) ":query-preview")
                                                                     (cstr/starts-with? (str %) ":panel-key"))
                                                                (ut/deep-flatten ddata))
                                       key-walk1        (filter #(not (cstr/includes? (str %) "/")) poss-value-walks)
                                       key-walk         (merge (into {}
                                                                     (for [k key-walk1]
                                                                       {k (keyword (str "gen-viz-" (rand-int 1234)))}))
                                                               {:panel-key (keyword req-name-str)})
                                       value-walk1      (filter #(cstr/includes? (str %) "/") poss-value-walks)
                                       value-walk       (into {}
                                                              (for [v value-walk1]
                                                                (let [vv   (ut/safe-name v)
                                                                      vs   (ut/splitter vv "/")
                                                                      qstr (ut/safe-name (first (ut/postwalk-replacer
                                                                                                 key-walk
                                                                                                 [(keyword (first vs))])))
                                                                      vstr (last vs)]
                                                                  (ut/tapp>> [:value-walk vv vs qstr vstr v
                                                                              (keyword (str qstr "/" vstr))])
                                                                  {v (keyword (str qstr "/" vstr))})))]
                                   (merge (ut/postwalk-replacer (merge key-walk value-walk) ddata)
                                          {;:mad-libs-viz []
                                           :drag-meta {:type       :viz-reco ;type
                                                       :block-name (keyword req-name-str)}})))))


(defn item-subscription-spawner-meta
  []
  (let [main-row-selected @(ut/tracked-subscribe [::conn/clicked-parameter [:searches-rows-sys]]) ;;
        ;;;main-row-selected @(ut/tracked-sub ::conn/clicked-parameter-alpha {:keypath [:connections-sys]})
        ttype (get main-row-selected :item_type)
        shortcode (try (edn/read-string (get main-row-selected :value)) (catch :default _ "shortcode-error!"))
        rando (rand-int 45)
        {:keys [item_type item_sub_type item_key display_name sample is_live block_meta]} main-row-selected
        panel-map (try (edn/read-string block_meta) (catch :default _ {}))
        natural-key (ut/safe-key (get panel-map :natural-key (keyword (str "sub-" rando))))
        selected-fields (vec (keys (get @(ut/tracked-subscribe [::conn/sql-metadata [name]]) :fields)))
        selected-fields (if (empty? selected-fields) [:*] selected-fields)
        table-name name
        view-data (if (= name :view) (get panel-map :views) (get-in panel-map [:views name]))
        parent-sql-sql-alias (ut/gen-sql-sql-alias)
        sql-name-base (cond :else (ut/replacer (str (ut/safe-name table-name) "-clone-" rando) #"_" "-"))
        sql-name (keyword sql-name-base)
        connection-id (get panel-map :connection-id (get-in panel-map [:queries (ffirst (get panel-map :queries)) :connection-id]))
        drag-meta {;:source-table     table-name
                   :type ttype}
        new-panel
        (cond (cstr/ends-with? (str ttype) "-views")   {:h         (get panel-map :h 5)
                                                        :w         (get panel-map :w 7)
                                                        :drag-meta drag-meta
                                                        :views     {:vw1 [:box :size "auto" :child shortcode]}
                                                        :name      (str "subscription " item_key display_name rando)}
              (cstr/ends-with? (str ttype) "-queries") {:h             (get panel-map :h 5)
                                                        :w             (get panel-map :w 7)
                                                        :drag-meta     drag-meta
                                                        :connection-id (get panel-map :connection-id (get-in panel-map [:queries (ffirst (get panel-map :queries)) :connection-id]))
                                                        :queries       {natural-key {:select [:*] :from [shortcode]}}
                                                        :name          (str "subscription " item_key display_name rando)}
              :else                                    {:h         (get panel-map :h 5)
                                                        :w         (get panel-map :w 7)
                                                        :drag-meta drag-meta
                                                        :views     {natural-key [:box :size "auto" :child [:string shortcode]]}
                                                        :name      (str "subscription " item_key display_name rando)})]
    new-panel))


(defn sql-spawner-cloner
  [type source-panel-id name]
  (let [panel-map            @(ut/tracked-sub ::workspace-alpha {:keypath [source-panel-id]})
        selected-fields      (vec (keys (get @(ut/tracked-sub ::conn/sql-metadata-alpha {:keypath [name]}) :fields)))
        selected-fields      (if (empty? selected-fields) [:*] selected-fields)
        table-name           name
        view-data            (if (= name :view) (get panel-map :views) (get-in panel-map [:views name]))
        rando                (rand-int 45)
        parent-sql-sql-alias (ut/gen-sql-sql-alias)
        sql-name-base        (cond ;(= type :meta-fields)
                               :else (ut/replacer (str (ut/safe-name table-name) "-clone-" rando) #"_" "-"))
        sql-name             (keyword sql-name-base)
        sqlized-hash         (get-in panel-map [:queries (ffirst (get panel-map :queries)) :_sqlized-hash])
        connection-id        (get panel-map :connection-id (get-in panel-map [:queries (ffirst (get panel-map :queries)) :connection-id]))
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
                                       :queries       {sql-name (merge (when sqlized-hash {:_sqlized-hash sqlized-hash})
                                                                 {:select selected-fields ;[:*]
                                                                 :from   [[(keyword (str "query/" (ut/safe-name table-name)))
                                                                           parent-sql-sql-alias]]})}}
                               :view  {:h         (get panel-map :h)
                                       :w         (get panel-map :w)
                                       :drag-meta drag-meta
                                       :views     {:view-clone view-data} ;[:box :child [:string
                                       :name      (str "clone-" (ut/safe-name table-name) rando)})]
    new-panel))


(re-frame/reg-sub ::subq-used
                  (fn [db [_ panel-id]] ;; filter has got to be faster than for loops. TODO for other subs im
                    (let [queries-used    (filter #(cstr/includes? (str %) ":query/")
                                                  (ut/deep-flatten (get-in db [:panels panel-id :queries])))
                          root-query-keys (for [k queries-used] (keyword (last (ut/splitter (ut/safe-name k) "/"))))]
                      root-query-keys)))

(re-frame/reg-sub ::subq-produced
                  (fn [db [_ panel-id]] ;; filter has got to be faster than for loops. TODO for other subs im
                    (keys (get-in db [:panels panel-id :queries]))))

(re-frame/reg-sub ::subq-mapping-orig
                  (fn [db [_]] ;; filter has got to be faster than for loops. TODO for other subs im using FOR
                    (into {}
                          (for [[k v] (get db :panels)]
                            {k (merge {:uses (apply concat
                                                    (for [[k1 v1] (get v :queries)]
                                                      (for [qq (filter #(cstr/includes? (str %) ":query/") (ut/deep-flatten v1))]
                                                        (keyword (last (ut/splitter (ut/safe-name qq) "/"))))))}
                                      {:produces (keys (get-in db [:panels k :queries]))})}))))

(re-frame/reg-sub
 ::subq-mapping
 (fn [db [_]]
   (let [panels            (get db :panels)
         all-sql-call-keys (keys (into {}
                                       (for [[_ v] panels]
                                         (into {} (for [[kk vv] (get v :queries)] (when (nil? (find vv :vselect)) {kk vv}))))))]
     (into {}
           (for [[k v] panels]
             {k (merge {:uses (apply concat
                                     (for [[_ v1] (merge (get v :queries) (get v :views))]
                                       (for [qq (filter #(or (cstr/includes? (str %) ":query/") (some #{%} all-sql-call-keys))
                                                        (flatten (conj (ut/deep-flatten v1)
                                                                       (apply (fn [x] ;; get partial matches
                                                                                (keyword (first (ut/splitter (str (ut/safe-name x)) #"/"))))
                                                                              (filter #(cstr/includes? (str %) "/") (ut/deep-flatten v1))))))]
                                         (keyword (last (ut/splitter (ut/safe-name qq) "/"))))))}
                       {:produces (keys (get-in db [:panels k :queries]))})})))))



(re-frame/reg-sub
 ::subq-mapping-alpha
 (fn [db {}]
   (let [px (hash (ut/remove-underscored (get db :panels))) 
         cc (get @ut/subq-mapping-alpha px)]
     (if cc
       cc
       (let [panels            (get db :panels)
             all-sql-call-keys (keys (into {}
                                           (for [[_ v] panels]
                                             (into {}
                                                   (for [[kk vv] (get v :queries)] (when (nil? (find vv :vselect)) {kk vv}))))))
             res               (into {}
                                     (for [[k v] panels]
                                       {k (merge 
                                           {:uses (apply concat
                                                         (for [[_ v1] (merge (get v :queries) (get v :views))
                                                               :let [ssrc0 (flatten (conj (vec (ut/deep-flatten v1))
                                                                                          (apply (fn [x] ;; get partial matches
                                                                                                   (keyword
                                                                                                    (first (ut/splitter
                                                                                                            (str (ut/safe-name x))
                                                                                                            #"/"))))
                                                                                                 (filter #(cstr/includes? (str %) "/")
                                                                                                         (vec (ut/deep-flatten v1))))))
                                                                     ssrc1 (filter #(or (cstr/includes? (str %) ":query/")
                                                                                        (some #{%} all-sql-call-keys))
                                                                                   ssrc0)]]
                                                           (for [qq ssrc1
                                                                ;;  :let [_ (tapp>> [:v1 (str (ut/deep-flatten v1)) (str all-sql-call-keys) qq
                                                                ;;                   (keyword (last (ut/splitter (ut/safe-name qq) "/")))])]
                                                                 ]
                                                             (keyword (last (ut/splitter (ut/safe-name qq) "/"))))))}
                                                 {:produces (keys (get-in db [:panels k :queries]))})}))]
         (swap! ut/subq-mapping-alpha assoc px res)
         res)))))

(re-frame/reg-sub
 ::subq-panels
 (fn [db [_ panel-id]] ;; filter has got to be faster than for loops. TODO for other subs im
   (let [panels            (into {} (filter #(= (get db :selected-tab) (get (val %) :tab "")) (get db :panels))) ;(get
         all-sql-call-keys (keys (into {}
                                       (for [[_ v] panels]
                                         (into {} (for [[kk vv] (get v :queries)] (when (nil? (find vv :vselect)) {kk vv}))))))
         queries-used      (filter #(or (cstr/includes? (str %) ":query/") (some #{%} all-sql-call-keys))
                                   (ut/deep-flatten (merge (get-in db [:panels panel-id :queries])
                                                           (get-in db [:panels panel-id :views]))))
         root-query-keys   (for [k queries-used] (keyword (last (ut/splitter (ut/safe-name k) "/"))))
         query-sources     (into {}
                                 (remove empty?
                                         (flatten (for [[k v] panels] (flatten (for [kk (keys (get v :queries))] {kk k}))))))]
     (vec (remove #(or (nil? %) (= (get db :selected-block) %)) (for [q root-query-keys] (get query-sources q)))))))


(re-frame/reg-sub
 ::subq-panels-alpha
 (fn [db {:keys [panel-id]}] ;; filter has got to be faster than for loops. TODO for other
   (let [px (hash [panel-id (get db :selected-tab) (ut/remove-underscored (get db :panels))])
         cc (get @ut/subq-panels-alpha px)]
     (if cc
       cc
       (let [panels            (into {} (filter #(= (get db :selected-tab) (get (val %) :tab "")) (get db :panels))) ;(get db
                                                                                                                      ;:panels)
             all-sql-call-keys (keys (into {}
                                           (for [[_ v] panels]
                                             (into {}
                                                   (for [[kk vv] (get v :queries)] (when (nil? (find vv :vselect)) {kk vv}))))))
             queries-used      (filter #(or (cstr/includes? (str %) ":query/") (some #{%} all-sql-call-keys))
                                       (ut/deep-flatten (merge (get-in db [:panels panel-id :queries])
                                                               (get-in db [:panels panel-id :views]))))
             root-query-keys   (for [k queries-used] (keyword (last (ut/splitter (ut/safe-name k) "/"))))
             query-sources     (into {}
                                     (remove empty?
                                             (flatten (for [[k v] panels] (flatten (for [kk (keys (get v :queries))] {kk k}))))))
             res               (vec (remove #(or (nil? %) (= (get db :selected-block) %))
                                            (for [q root-query-keys] (get query-sources q))))]
         (swap! ut/subq-panels-alpha assoc px res)
         res)))))


(re-frame/reg-sub ::subq-panels-all
                  (fn [db [_ panel-id]] ;; filter has got to be faster than for loops. TODO for other subs im
                    (let [queries-used    (filter #(cstr/includes? (str %) ":query/")
                                                  (ut/deep-flatten (into {} (for [[k v] (get db :panels)] (get v :queries)))))
                          root-query-keys (for [k queries-used] (keyword (last (ut/splitter (ut/safe-name k) "/"))))
                          query-sources   (into {}
                                                (remove empty?
                                                        (flatten (for [[k v] (get db :panels)]
                                                                   (flatten (for [kk (keys (get v :queries))] {kk k}))))))])))

(re-frame/reg-sub
 ::subq-panels2
 (fn [db [_]]
   (let [all-qs (into {}
                      (for [[pk pv] (get db :panels)]
                        (let [queries-used    (filter #(cstr/includes? (str %) ":query/") (ut/deep-flatten (get pv :queries)))
                              root-query-keys (for [k queries-used] (keyword (last (ut/splitter (ut/safe-name k) "/"))))]
                          {pk root-query-keys})))]
     all-qs)))

(defn scrubber-panel
  [view? keypath-map view-key & [wildcard full-map]]
  [reecatch
   (let [type-key (if view? :views :queries)
         canvas? (get full-map :canvas? false)
         flow? (get full-map :flow? false)
         opts? (get full-map :opts? false)
         dcolors (theme-pull :theme/data-colors nil)
         dcolor (fn [x] (get dcolors (ut/data-typer x)))
         runstreams? (get full-map :runstreams? false)
         panel-key @(ut/tracked-subscribe [::selected-block])
         block-map @(ut/tracked-subscribe [::selected-block-map])
         wild? (true? (and (not (= (cstr/trim (str wildcard)) "")) (not (nil? wildcard))))
         scrubber-boxes
         (into
          (sorted-map)
          (for [[kp [v [scrub-type [ffn ffnx]]]] keypath-map]
            {kp (when (and (and (not (nil? scrub-type))
                                (not (nil? v))
                                (not (and (keyword? v) (try (integer? (first kp)) (catch :default _ false)) (= 1 (count kp))))
                                (if view? true (some #(or (= % :page) (= % :style-rules)) kp))
                                (if (or (try (= (first view-key) :flow) (catch :default _ false))
                                        (try (= (first view-key) :runstreams) (catch :default _ false)))
                                  true
                                  (not (some #(cstr/starts-with? (str %) ":*") kp))))
                           (if wild?
                             (or (cstr/includes? (str kp) (str wildcard)) (cstr/includes? (str v) (str wildcard)))
                             true))
                  (let [;; _ (ut/tapp>> [:here? kp v])
                        ft            (first (remove nil?
                                                     (for [[k v] (scrub/friendly-text)]
                                                       (when (scrub/ordered-keys-in-kp-multi? k kp) v))))
                        message       (get ft :message nil)
                        url           (get ft :url)
                        system-param? (if opts?
                                        (some #(= % (first kp)) [:timeout :close-on-done? :retries :retry-on-error? :debug?])
                                        (some #(= % (first kp)) (keys db/base-theme)))
                        display-kp    (str (-> (str kp)
                                               (ut/replacer "[" "")
                                               (ut/replacer "]" "")))
                        display-kp    (if system-param? (str display-kp " *sys*") display-kp)]
                    [re-com/v-box :padding "4px" :gap "3px" :attr
                     {:on-mouse-enter #(do ;(ut/tapp>> [:enter-scrub-area
                                         (reset! on-scrubber? true))
                      :on-mouse-leave #(do ;(ut/tapp>> [:exit-scrub-area
                                         (reset! on-scrubber? false))} :style
                     (if canvas?
                       {} ;{:margin-top "-12px"}
                       {:border-top (if (or (= kp [:select]) (= kp [:from]) (= kp [:where]) (= kp [:group-by]))
                                      "0px solid #ffffff18"
                                      "1px solid #ffffff18")}) 
                     :children
                     [;[re-com/box :child (str kp " :: " v)]
                      [re-com/h-box :justify :between :children
                       [(when (not flow?)
                          [re-com/box :src (at) :child display-kp :style
                           {:color (if system-param? 
                                     "#f386bf" 
                                      (get dcolors "keyword")
                                       ) :font-weight 700}])
                        (when (not flow?)
                          [re-com/h-box :gap "5px" :style {:font-size "11px" :padding-top "3px"} :children
                           [(when message [re-com/box :child (str message)])
                            (when url [:a {:href url :target "_blank"} "link"])]])
                        [re-com/h-box :gap "5px" :children
                         [(when (scrub/hex-color? v)
                            [re-com/box :src (at) :child " " :size "none" :width "18px" :height "18px" :attr
                             {:on-click #(reset! scrub/color-picker-enabled? (if (= kp @scrub/color-picker-enabled?) nil kp))}
                             :style {:background-color (str v) :cursor "pointer" :border "1px solid #ffffff15"}])
                          (when (and (not flow?) (not (vector? v)))
                            [re-com/box :src (at) :child (if (string? v) (str "\"" v "\"") (str v)) :style
                             {:font-weight 700
                              ;; :color       (cond (string? v)  "#b9e47d"
                              ;;                    (integer? v) "#f2c263"
                              ;;                    (float? v)   "#f2c263"
                              ;;                    (keyword? v) "#ae81ff"
                              ;;                    (boolean? v) "#ae81ff"
                              ;;                    :else        "#b9e47d")
                              :color       (dcolor v)
                              }])]]]]
                      [reecatch
                       (if ffnx
                         [re-com/box :src (at) :child [ffn kp v view-key type-key ffnx]]
                         [re-com/box :src (at) :child [ffn kp v view-key type-key]])]]]))}))
         flat-scrubber-boxes (vals scrubber-boxes)
         queries (get block-map :queries {})
         has-query? (ut/ne? queries)
         final-children (if (and has-query? (not view?))
                          (vec (conj flat-scrubber-boxes
                                     [re-com/v-box :children
                                      (for [[k v] queries]
                                        [re-com/v-box :children
                                         [;[re-com/box :child (str k)]
                                          [shape/map-boxes2 v panel-key (str k) [] panel-key "map"]]])]))
                          flat-scrubber-boxes)]
     (cond canvas? [re-com/v-box :size "auto" :width "100%" :height "100%" :gap "3px" :children final-children]
           :else   [re-com/v-box :gap "3px" :padding "5px" :children final-children]))])

(def vega-lite-shell
  [:vega-lite
   {:layer      [{:encoding {;:href {:field "cdata" :type "nominal"}
                             :x      {:field nil :type "ordinal" :sort "-y"}
                             :y      {:aggregate "sum" :field nil :type "quantitative"}
                             :row    {:field nil :legend nil}
                             :size   {:legend nil}
                             :shape  {:legend nil}
                             :column {:field nil :legend nil}
                             :color  {:scale  :theme/vega-default-color-scheme ; {:scheme
                                      :legend nil
                                      :field  1
                                      :type   "ordinal"}}
                  :mark     {:type "rect" :tooltip {:content "encoding"}}}]
    :config     :theme/vega-defaults
    :width      :panel-width
    :background "transparent"
    :padding    4
    :height     :panel-height
    :data       {:values nil}} {:actions false}])

(re-frame/reg-sub ::resolve-sql-alias
                  (fn [db {:keys [sql-key]}]
                    (let [q (get (into {} (for [[_ v] (get db :panels)] (into {} (get v :queries)))) sql-key)]
                      (ut/clean-sql-from-ui-keys q))))

(defn starts-with-fragment?
  [fragment vec]
  (and (<= (count fragment) (count vec)) (every? true? (map = fragment (subvec vec 0 (count fragment))))))

(defn increment-last [vec] (try (conj (vec (butlast vec)) (inc (last vec))) (catch :Default _ vec)))

(defn sql-alias-replace
  [query]
  (let [deep-flat-query     (ut/deep-flatten query)
        vpaths              (try (let [kpaths (ut/kvpaths query)
                                       vpaths (get (into {} (for [k kpaths] {(get-in query k) k})) :*code*)
                                       vpaths (assoc vpaths (- (count vpaths) 1) 1)
                                       vpaths (for [kp (filter #(starts-with-fragment? vpaths %) kpaths)] (get-in query kp))]
                                   vpaths)
                                 (catch :default _ []))
        ;; all-clicked-walks   (into {} ;; changed 7/18/24 looking for memory leaks
        ;;                           (map (fn [x]
        ;;                                  {x @(ut/tracked-subscribe [::generate-all-clicked
        ;;                                                             {:query-key (-> x
        ;;                                                                             str
        ;;                                                                             (ut/replacer #":" "")
        ;;                                                                             (ut/replacer "/*.clicked" "")
        ;;                                                                             keyword)}])})
        ;;                                (filter #(cstr/includes? (str %) "/*.clicked") deep-flat-query)))
        all-clicked-walks   (into {}
                                  (map (fn [x]
                                         {x @(ut/tracked-sub ::generate-all-clicked
                                                             {:query-key (-> x
                                                                             str
                                                                             (ut/replacer #":" "")
                                                                             (ut/replacer "/*.clicked" "")
                                                                             keyword)})})
                                       (filter #(cstr/includes? (str %) "/*.clicked") deep-flat-query)))
        valid-params        (into (distinct @(ut/tracked-sub ::valid-body-params-all-condis {}))
                                  (vec (remove nil?
                                               (for [k deep-flat-query]
                                                 (when (and (keyword? k) (cstr/includes? (str k) "/") (not (ut/is-sql-sql-alias? k)))
                                                   k)))))
        valid-params        (cset/difference (set valid-params) (set (filter ut/process-key vpaths)))
        valid-params-left   (cset/intersection (set deep-flat-query) (set valid-params))
        value-walks-targets (filter #(and (cstr/includes? (str %) ".")
                                          (not (cstr/includes? (str %) ".*")) ;; dont want to
                                          (not (cstr/includes? (str %) "*.")))
                                    valid-params)
        value-walks         (into {}
                                  (for [k value-walks-targets] ;all-sql-call-keys]
                                    (let [fs    (ut/splitter (ut/safe-name k) "/")
                                          gs    (ut/splitter (last fs) ".")
                                          ds    (keyword (first fs))
                                          row   (int (last gs))
                                          field (keyword (first gs))]
                                      {k (get-in @(ut/tracked-sub ::conn/sql-data-alpha {:keypath [ds]}) [row field])})))
        condi-walks-targets (filter #(cstr/includes? (str %) "condi/") valid-params)
        condi-walks         (into {}
                                  (for [k condi-walks-targets]
                                    {k ;;@(ut/tracked-subscribe [::conn/condi-value (keyword
                                     @(ut/tracked-sub ::conn/condi-value
                                                      {:condi-key (keyword (last (ut/splitter (ut/safe-name k) "/")))})}))
        meta-walks          (into {}
                                  (for [k     valid-params
                                        :when (cstr/includes? (str k) ":meta/")]
                                    {k (let [table-key (ut/replacer (str k) ":meta/" "")
                                             sql-safe  (-> table-key
                                                           str
                                                           (ut/replacer #":" "")
                                                           (ut/replacer #"-" "_"))]
                                         {:select [:*] :from [[:fields :query-meta-subq]] :where [:= :table_name sql-safe]})}))
        sql-params          (into {}
                                  (for [k valid-params]
                                    (if (cstr/includes? (str k) ":query/")
                                      {k (dissoc @(ut/tracked-sub ::resolve-sql-alias
                                                                  {:sql-key (keyword (ut/replacer (str k) ":query/" ""))})
                                                 :style-rules)}
                                      {k @(ut/tracked-sub ::conn/clicked-parameter-key-alpha {:keypath [k]})})))
        replaced            (->> query
                                 (ut/postwalk-replacer condi-walks)
                                 (ut/postwalk-replacer all-clicked-walks)
                                 (ut/postwalk-replacer value-walks)
                                 (ut/postwalk-replacer meta-walks)
                                 (ut/postwalk-replacer sql-params))]
    (if (ut/ne? valid-params-left) (sql-alias-replace replaced) replaced)))

(re-frame/reg-sub ::sql-alias-replace-sub ;; caching abuse?
                  (fn [_ {:keys [query]}] (sql-alias-replace query)))

(re-frame/reg-sub ::view-clicked-fields
                  (fn [db [_ panel-key view-key]]
                    (let [vw    (get-in db [:panels panel-key :views view-key])
                          vw-k  (ut/kvpaths vw)
                          vw-kf (first (filter #(= (last %) :click) vw-k))
                          res   (get-in db (vec (into [:panels panel-key :views view-key] vw-kf)) {})]
                      (if (map? res) res {}))))



(re-frame/reg-event-db
 ::execute-pill-drop
 (undoable)
 (fn [db [_ panel-key query-key clause drop-data]]
   (let [clause-conform       (cond (or (= clause :where<>) (= clause :where=))             :where
                                    (or (= clause :is-in) (= clause :not-in))               :in
                                    (or (= clause :order-by-asc) (= clause :order-by-desc)) :order-by
                                    :else                                                   clause)
         drops                @(ut/tracked-sub ::all-drops-of-alpha {:ttype :*})
         new-q-key            (keyword (str "query-" (rand-int 12345)))
         parent-sql-sql-alias (ut/gen-sql-sql-alias)
         dvec                 (fn [v] (vec (set v)))
         kpath                [:panels panel-key :queries query-key clause-conform]
         existing-query-part  (get-in db kpath)]
     (reset! over-flow? false) ;; just in case we don't want to get stuck in an undraggable
     (reset! over-block? false)
     (reset! dyn-dropper-hover nil) ;; weird bug where hover stays on after drop - late 2023
     (ut/tapp>> [:execute-pill-drop! clause-conform drops panel-key query-key clause drop-data])
     (cond ;p = clause-conform
       (some #(= clause-conform %) drops)
       (let [;dd @(ut/tracked-subscribe [::drop-details clause-conform])
             row-num      (get-in drop-data [:drag-body :row-num])
             src-table    (get-in drop-data [:drag-body :source-query])
             param-field  (get-in drop-data [:drag-body :param-field])
             param-key    (keyword (str (ut/unkey src-table) "/" (ut/unkey param-field) "." row-num))
             param-value  (get-in drop-data [:drag-body :param-full])
             target-block (get-in drop-data [:target-panel])
             cell-drop    (ut/safe-key (keyword (str "cell-drop-" (ut/replacer (str clause-conform) ":" ""))))
             md           (get-in drop-data [:drag-body :data-type])
             new-view     [:box :size "auto" :style {:font-size [:auto-size-px param-key] :transition "all 0.6s ease-in-out"}
                           :child [clause-conform param-key]]]
         (if (not (and row-num src-table param-field))
           (do (ut/tapp>> [:cant-drop-text-view :missing-parts src-table "/" param-field "." row-num]) db)
           (-> db
               (assoc-in [:panels target-block :views cell-drop] new-view)
               (assoc-in [:panels target-block :selected-view] cell-drop))))
       (= clause-conform :runstream-overrides)
       (let [{:keys [source target]}                                      (get drop-data :drop-data)
             {:keys [type param-full param-type param-table param-field]} source
             {:keys [flow-id type key]}                                   target]
         (-> db
             (assoc-in [:runstreams flow-id :values key :value] param-full)
             (assoc-in [:runstreams flow-id :values key :source] :param)))
       (cstr/starts-with? (str clause-conform) ":weave/")
       (let [weave-by             (keyword (ut/replacer (str clause-conform) ":weave/" ""))
             source-panel-key     (get-in drop-data [:drag-body :source-panel-key])
             source-view          (get-in drop-data [:drag-body :source-table])
             view-body            (get-in db [:panels source-panel-key :views source-view] [:div "where's the beef!"])
             fxky                 (fn [k] (if (cstr/starts-with? (str k) ":query/") (keyword (ut/replacer k #":query/" "")) k))
             data-key-path        (or (first (filter #(= (last %) :data) (ut/kvpaths view-body))) [:nope])
             view-height          (or (first (filter #(= (last %) :height) (ut/kvpaths view-body))) [:nope])
             view-width           (or (first (filter #(= (last %) :width) (ut/kvpaths view-body))) [:nope])
             data-key             (or (first (filter #(= (last %) :dataKey) (ut/kvpaths view-body))) [:nope]) ;; temp
             view-data-ref        (get-in view-body data-key-path)
             base-query           @(ut/tracked-subscribe [::find-query (fxky view-data-ref)])
             weave-by-placeholder (keyword (str "_" (ut/replacer (str weave-by) #":" "")))
             inner-q-name         :gen-viz-609
             view-body            (-> view-body
                                      (assoc-in view-height 150)
                                      (assoc-in (conj (vec (drop-last data-key)) :isAnimationActive) false)
                                      (assoc-in view-width 300)
                                      (assoc-in data-key-path inner-q-name))
             modded-query         (if (get base-query :where)
                                    (assoc base-query :where [:and (get base-query :where) [:= weave-by weave-by-placeholder]])
                                    (assoc base-query :where [:= weave-by weave-by-placeholder]))
             host-query           (get-in db [:panels panel-key :queries query-key])
             field-name           (keyword (str "inviz_" (rand-int 1234)))
             render-field         [[:*render* {:queries {inner-q-name modded-query} :view view-body}] field-name]
             host-query-modded    (-> host-query
                                      (assoc :row-height 100)
                                      (assoc-in [:col-widths field-name] 200)
                                      (assoc :select (vec (conj (ut/postwalk-replacer {} ;{weave-by
                                                                                      (get host-query :select)) ;; need
                                                                render-field)))
                                      (assoc :group-by (vec (conj (get host-query :group-by)
                                                                  (+ (count (get host-query :select)) 1)))))]
         (ut/tapp>> [:view-body-drop ; weave-by view-data-ref view-body modded-query
                     host-query-modded])
         (assoc-in db [:panels panel-key :queries query-key] host-query-modded))
       (not (nil? (get clause-conform :layout-drop)))
       (let [target             (get clause-conform :layout-drop)
             incoming-view-src  (get-in drop-data [:drag-body :source-table])
             incoming-block-src (get-in drop-data [:drag-body :source-panel-key] :query)
             incoming-block-src (if (= (get-in drop-data [:drag-body :type]) :query) :query incoming-block-src) ;; override
                                                                                                                   ;; with query
                                                                                                                   ;; keyword
                                                                                                                   ;; prefix
             compound-key       (try (keyword (str "view/" (name incoming-block-src) "." (name incoming-view-src)))
                                     (catch :default e (do (ut/tapp>> [:error e]) nil)))
             target-kp          (vec (flatten (get clause-conform :kp)))
             exists?            (not (nil? (get-in db (conj target-kp compound-key))))]
         (ut/tapp>> [:adding-view-to-layout target-kp compound-key (conj target-kp compound-key) exists?
                     (conj target-kp target) (get-in db (conj target-kp target))])
         (if exists?
           (-> db ;; save the old pos with a new name, remove the old compound key version,
               (assoc-in (conj target-kp (keyword (str "empty-panel" (rand-int 123))))
                         (get-in db (conj target-kp compound-key)))
               (ut/dissoc-in (conj target-kp target))
               (assoc-in (conj target-kp compound-key) (get-in db (conj target-kp target))))
           (-> db ;; add new pos with old layout, remove old pos
               (assoc-in (conj target-kp compound-key) (get-in db (conj target-kp target)))
               (ut/dissoc-in (conj target-kp target)))))
       (= clause-conform :group-by) (-> db
                                        (assoc-in [:panels panel-key :queries query-key :group-by]
                                                  (dvec (conj (get-in db [:panels panel-key :queries query-key :group-by])
                                                              (get-in drop-data [:drag-body :target]))))
                                        (assoc-in [:panels panel-key :queries query-key :select]
                                                  (dvec (conj (get-in db [:panels panel-key :queries query-key :select])
                                                              (get-in drop-data [:drag-body :target])))))
       (and (= clause-conform :text) (= (get-in drop-data [:drag-body :type]) :where)) ;; basic
                                                                                        ;; single
                                                                                        ;; cell drag
       (let [;_ (ut/tapp>> [:started?])
             row-num       (get-in drop-data [:drag-body :row-num])
             src-table     (get-in drop-data [:drag-body :source-query])
             param-field   (get-in drop-data [:drag-body :param-field])
             param-key     (keyword (str (ut/unkey src-table) "/" (ut/unkey param-field) "." row-num))
             param-value   (get-in drop-data [:drag-body :param-full])
             target-block  (get-in drop-data [:target-panel])
             cell-drop     (keyword (str "cell-drop-" (rand-int 12345)))
             is-image?     (and (or (cstr/ends-with? (cstr/lower-case (str param-value)) ".png")
                                    (cstr/ends-with? (cstr/lower-case (str param-value)) ".webp")
                                    (cstr/ends-with? (cstr/lower-case (str param-value)) ".jpg")
                                    (cstr/ends-with? (cstr/lower-case (str param-value)) ".jpeg")
                                    (cstr/ends-with? (cstr/lower-case (str param-value)) ".gif")
                                    (cstr/includes? (cstr/lower-case (str param-value)) "/images/")
                                    (cstr/includes? (cstr/lower-case (str param-value)) "/image/"))
                                (or
                                 (cstr/starts-with? (cstr/lower-case (str param-value)) "http")
                                 (cstr/starts-with? (cstr/lower-case (str param-value)) "./images")))
             is-video?     (or (cstr/ends-with? (cstr/lower-case (str param-value)) ".mp4")
                               (cstr/ends-with? (cstr/lower-case (str param-value)) ".mov"))
             md            (get-in drop-data [:drag-body :data-type])
             num?          (or (= md "integer") (= md "float"))
             formatted-val (if num? (str (nf param-value)) (str param-value))
             len           (count (str formatted-val))
             h             (get-in db [:panels target-block :h])
             w             (get-in db [:panels target-block :w])
             new-view      [:box :size "auto" :align :center :justify :center :style
                            {:font-size [:auto-size-px param-key] :transition "all 0.6s ease-in-out"} :child
                            (cond is-image? [:img {:src param-key :width "100%"}]
                                  is-video? [:iframe ;; :video ? html5 shit instead?
                                             {:src   param-key ;;:movies_deets/_1080p_video
                                              :style {:border "none" :width :panel-width+80-px :height :panel-height+80-px}}]
                                  :else     [:string param-key])]]
         (ut/tapp>> [:text-drop param-key drop-data])
         (if (not (and row-num src-table param-field))
           (do (ut/tapp>> [:cant-drop-text-view :missing-parts src-table "/" param-field "." row-num]) db)
           (-> db
               (assoc-in [:panels target-block :views cell-drop] new-view)
               (assoc-in [:panels target-block :selected-view] cell-drop))))
       (= clause-conform :where) (let [exist?     (not (empty? existing-query-part))
                                       exclude?   (= clause :where<>)
                                       dest-field (get-in drop-data [:drag-body :param-field])
                                       kw-alias   (get-in drop-data [:drag-body :param-full])
                                       param?     (keyword? kw-alias)
                                       stmt       (if param?
                                                    [:*when kw-alias ;; no need to wrap literals in :*when
                                                     [(if exclude? :<> :=) dest-field kw-alias]]
                                                    [(if exclude? :<> :=) dest-field kw-alias])
                                       new        (if exist? [:and existing-query-part stmt] stmt)]
                                   (assoc-in db kpath new))
       (= clause-conform :in) (let [where-exist (get-in db [:panels panel-key :queries query-key :where])
                                    exist?      (not (empty? where-exist))
                                    exclude?    (= clause :not-in)
                                    dest-field  (get-in drop-data [:drag-body :param-field])
                                    kw-alias    (get-in drop-data [:drag-body :param-full])
                                    stmt        [:*when kw-alias [:in dest-field kw-alias]]
                                    stmt        (if exclude? [:not stmt] stmt)
                                    new         (if exist? [:and where-exist stmt] stmt)
                                    kpath       (vec (conj (vec (drop-last kpath)) :where))]
                                (assoc-in db kpath new))
       (= clause-conform :filter-shadow) (let [where-exist     (get-in db [:panels panel-key :queries query-key :where])
                                               source-query    (get-in drop-data [:drag-body :source-table])
                                               target-query    (get-in drop-data [:target-query])
                                               gb1             @(ut/tracked-subscribe [::group-bys target-query])
                                               gb2             @(ut/tracked-subscribe [::group-bys source-query])
                                               group-bys       (vec (cset/intersection (set gb1) (set gb2)))
                                               clicked-keyword (keyword (str (ut/replacer source-query #":" "") "/*.clicked"))
                                               exist?          (not (empty? where-exist))
                                               stmt            [:*if clicked-keyword [:*all= clicked-keyword group-bys] [:= 1 1]]
                                               new             (if exist? [:and where-exist stmt] stmt)
                                               kpath           (vec (conj (vec (drop-last kpath)) :where))]
                                           (assoc-in db kpath new))
       (= clause-conform :highlight-rows)
       (let [exist           (get-in db [:panels panel-key :queries query-key :style-rules])
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
                                      :border           (str "1px solid " (theme-pull :theme/editor-outer-rim-color nil))}}
             new             (if exist? (assoc exist rule-key stmt) {rule-key stmt})
             kpath           (vec (conj (vec (drop-last kpath)) :style-rules))]
         (assoc-in db kpath new))
       (= clause-conform :click-filter) (let [where-exist     (get-in db [:panels panel-key :queries query-key :where])
                                              source-query    (get-in drop-data [:drag-body :source-table])
                                              src-panel-key   (get-in drop-data [:drag-body :source-panel-key])
                                              target-query    (get-in drop-data [:target-query])
                                              gb1             @(ut/tracked-subscribe [::group-bys target-query])
                                              gb2             (vec (vals @(ut/tracked-subscribe [::view-clicked-fields
                                                                                                 src-panel-key source-query])))
                                              group-bys       (vec (cset/intersection (set gb1) (set gb2)))
                                              clicked-keyword (keyword (str (ut/replacer src-panel-key #":" "") "/*.clicked"))
                                              exist?          (not (empty? where-exist))
                                              stmt            [:*if clicked-keyword [:*all= clicked-keyword group-bys] [:= 1 1]]
                                              new             (if exist? [:and where-exist stmt] stmt)
                                              kpath           (vec (conj (vec (drop-last kpath)) :where))]
                                          (assoc-in db kpath new))
       (or (cstr/starts-with? (str clause-conform) ":pivot-on/") (cstr/starts-with? (str clause-conform) ":🌶️pivot-on/"))
       (let [pivot-field  (keyword (last (ut/splitter (str clause-conform) #"/")))
             agg-field    (get-in drop-data [:drag-body :target])
             spicy?       (cstr/starts-with? (str clause-conform) ":🌶️pivot-on/")
             existing-q   (get-in db [:panels panel-key :queries query-key])
             aggy         (if (= agg-field :rowcnt) :count [:sum agg-field]) ;; rowcnt is not
             pivot-by-map {pivot-field [aggy
                                        {:select-distinct [pivot-field]
                                         :from            [[existing-q (keyword (str "subqpiv" (rand-int 1234)))]]}]}
             new-query    (-> existing-q
                              (assoc :pivot-by pivot-by-map)
                              (assoc :select (ut/remove-key (get existing-q :select) pivot-field))
                              (assoc :group-by (ut/remove-key (get existing-q :group-by) pivot-field)))
             new-query    (if spicy?
                            (-> new-query)
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
             incoming-table-fields-aliased (vec (for [e incoming-table-fields-final] (add-alias incoming-table-alias e)))
             subquery?                     (not (= (get-in drop-data [:drag-body :source-panel-key]) "none!"))
             incoming-table                (if subquery? (keyword (str "query/" (ut/safe-name incoming-table))) incoming-table)
             aliased-q                     (assoc aliased-q
                                                  :join (if existing-joins?
                                                          (vec (concat existing-joins
                                                                       [[incoming-table incoming-table-alias]
                                                                        [:= (add-alias incoming-table-alias join-field)
                                                                         (first (filter #(cstr/includes? (str %)
                                                                                                         (str "/"
                                                                                                              (name join-field)))
                                                                                        existing-selects))]])) ;; maybe
                                                          [[incoming-table incoming-table-alias]
                                                           [:= (add-alias incoming-table-alias join-field)
                                                            (add-alias existing-table-alias join-field)]]))
             aliased-q                     (assoc aliased-q
                                                  :select (vec (apply merge
                                                                      (get aliased-q :select)
                                                                      (vec incoming-table-fields-aliased))))]
         (ut/tapp>> [:join-debug existing-selects (add-alias incoming-table-alias join-field) (name join-field)
                     (first (filter #(cstr/includes? (str %) (str "/" (name join-field))) existing-selects))])
         (-> db ;; step 1 - alias all table fields in existing query (postwalk on whole map
             (assoc-in [:panels panel-key :queries query-key] aliased-q)))
       (some #(= clause-conform %) [:highlight= :highlight> :highlight< :heatmap])
       (let [existing (get-in db [:panels panel-key :queries query-key :style-rules] {})
             heatmap? (true? (= clause-conform :heatmap))
             rname    (keyword (str (if heatmap? "heatmap-" "highlight-") (rand-int 12345)))]
         (assoc-in db
                   [:panels panel-key :queries query-key :style-rules]
                   (merge existing
                          {(if (= clause-conform :heatmap) [(get-in drop-data [:drag-body :param-field]) rname] [:* rname])
                           {:logic (if (= clause-conform :heatmap)
                                     true
                                     [(condp = clause-conform :highlight= := :highlight> :> :highlight< :<)
                                      (get-in drop-data [:drag-body :param-field]) (get-in drop-data [:drag-body :param-full])])
                            :style (merge {:background-color (if (= clause-conform :heatmap)
                                                               [:heatmap] ;; :PuOr :11 :asc
                                                               "#008b8b66")
                                           :border           "1px solid #00000000"}
                                          (if (= clause-conform :heatmap) {:color "#000000"} {}))}})))
       (some #(= clause-conform %) [:sum :avg :min :max :count])
       (let [falias (keyword (str (ut/safe-name (get-in drop-data [:drag-body :target]))
                                  "_" (ut/safe-name clause-conform)
                                  "_" (rand-int 212)))]
         (assoc-in db
                   [:panels panel-key :queries query-key :select]
                   (dvec (conj (get-in db [:panels panel-key :queries query-key :select])
                               [[clause-conform (if (= clause-conform :count) 1 (get-in drop-data [:drag-body :target]))] falias]))))
       (= clause-conform :count-distinct)
       (let [falias (keyword (str (ut/safe-name (get-in drop-data [:drag-body :target])) "_" "countd" "_" (rand-int 212)))]
         (assoc-in db
                   [:panels panel-key :queries query-key :select]
                   (dvec (conj (get-in db [:panels panel-key :queries query-key :select])
                               [[:count [:distinct (get-in drop-data [:drag-body :target])]] falias]))))
       (= clause-conform :table-filter) (let [src-table      (get-in drop-data [:drag-body :source-table])
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
                                              new-param      (keyword (str (ut/safe-name new-q-key) "/*.clicked"))
                                              cla            [:*when new-param [:*all= new-param [target-field]]]
                                              new-where      (if exists? [:and src-where cla] cla)]
                                          (-> db
                                              (assoc-in [:panels panel-key :selected-view] new-q-key)
                                              (assoc-in [:panels panel-key :queries new-q-key]
                                                        (sql-alias-replace {:select   [target-field [[:count 1] :rowcnt]] ;; sql-alias-replace
                                                                            :from     [[src-table parent-sql-sql-alias]]
                                                                            :group-by [target-field]
                                                                            :order-by [[target-field :asc]]}))
                                              (assoc-in [:panels panel-key :connection-id] conn-id)
                                              (assoc-in [:panels src-panel-key :queries src-query :where] new-where)))
       (= clause-conform :order-by) (assoc-in db
                                              [:panels panel-key :queries query-key :order-by]
                                              (dvec (conj (get-in db [:panels panel-key :queries query-key :order-by] [])
                                                          [(get-in drop-data [:drag-body :target])
                                                           (if (= clause :order-by-desc) :desc :asc)])))
       (= clause-conform :row-count) (let [src-table (get-in drop-data [:drag-body :source-table])
                                           subquery? (not (= (get-in drop-data [:drag-body :source-panel-key]) "none!"))
                                           src-table (if subquery? (keyword (str "query/" (ut/safe-name src-table))) src-table)]
                                       (-> db
                                           (assoc-in [:panels panel-key :queries new-q-key]
                                                     {:select [[[:count 1] :rowcnt]] :from [[src-table parent-sql-sql-alias]]})
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
             (ut/dissoc-in [:panels panel-key :queries query-key (if (empty? new-order) :order-by :nevermind!)]) ;; dirty
                                                                                                                    ;; hack
             (ut/dissoc-in [:panels panel-key :queries query-key (if (empty? new-group) :group-by :nevermind!)]))) ;; dirty
                                                                                                                      ;; hack
       (= clause-conform :promote-query) (assoc-in db
                                                   [:panels panel-key :queries query-key] ;; materialize
                                                                                   ;; and overwrite
                                                   (sql-alias-replace (get-in db [:panels panel-key :queries query-key])))
       
       (= clause-conform :materialize-to-cache-db) (let [existing-q   (get-in db [:panels panel-key :queries query-key])
                                                         connection-id (get-in db [:panels panel-key :connection-id] (get existing-q :connection-id))
                                                         query (assoc existing-q :page -3)] ;; trigger to materialize that run other side-effects
                                                     (conn/sql-data [query-key] query connection-id)
                                                     db)

       (= clause-conform :materialize-values) (let [source-tab        (get-in drop-data [:drag-body :source-table])
                                                    block-map         (get-in db [:panels panel-key :views source-tab])
                                                    valid-body-params (vec (ut/get-compound-keys block-map))
                                                    workspace-params (into {}
                                                                           (for [k valid-body-params] ;; deref here?
                                                                             {k @(ut/tracked-sub ::conn/clicked-parameter-key-alpha {:keypath [k]})}))
                                                    value-walks-targets (filter #(and (cstr/includes? (str %) ".") (not (cstr/includes? (str %) ".*"))) valid-body-params)
                                                    value-walks (into {}
                                                                      (for [k value-walks-targets] ;all-sql-call-keys]
                                                                        (let [fs    (ut/splitter (ut/safe-name k) "/")
                                                                              gs    (ut/splitter (last fs) ".")
                                                                              ds    (keyword (first fs))
                                                                              row   (try (int (last gs)) (catch :default _ "label"))
                                                                              field (keyword (first gs))]
                                                                          {k (if (not (integer? row))
                                                                               (str field)
                                                                               (get-in @(ut/tracked-sub ::conn/sql-data-alpha {:keypath [ds]}) [row field]))})))
                                                    new-vw (cond->> block-map
                                                             (ut/ne? value-walks)      (ut/postwalk-replacer value-walks)
                                                             (ut/ne? workspace-params) (ut/postwalk-replacer workspace-params))]
                                                (assoc-in db [:panels panel-key :views source-tab] new-vw))

       (or (= clause-conform :delete-query) (= clause-conform :delete-view))
       (let [source-block      (get-in drop-data [:drag-body :source-panel-key])
             view-cnt          (count (keys (get-in db [:panels source-block :views])))
             q-cnt             (count (keys (get-in db [:panels source-block :queries])))
             source-block-tabs (+ view-cnt q-cnt)
             last-tab?         (= source-block-tabs 1)
             source-tab        (get-in drop-data [:drag-body :source-table])
             selected-block    (get db :selected-block)
             is-selected?      (= source-block selected-block)
             source-type       (get-in drop-data [:drag-body :type])
             source-key        (if (= source-type :query) :queries :views)]
         (when (= clause-conform :delete-query) (ut/tracked-dispatch [::unalias-query source-tab])) ;; dont
         (-> db
             (ut/dissoc-in [:panels source-block source-key source-tab])
             (assoc :selected-block (if (and last-tab? is-selected?) nil selected-block)) ;; unselect
             (ut/dissoc-in (if (and (= clause-conform :delete-view) (= view-cnt 1))
                             [:panels source-block source-key]
                             [:panels source-block source-key :nevermind!])) ;; cheese man.
             (ut/dissoc-in (if last-tab?            ;; if we are moving out of the last tab,
                             [:panels source-block] ;; another dirty hack
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
                                   source-tab) ;; else leave the name alone!
                                 (keyword (str (ut/safe-name source-tab) "-copy-" (rand-int 99))))]
         (cond
           (and view? (= clause-conform :reference)) (-> db
                                                         (assoc-in [:panels dest-block source-key new-tab-name]
                                                                   (keyword (str "view/" (ut/safe-name source-block)
                                                                                 "."     (ut/safe-name source-tab)))))
           query?                                    (if (= clause-conform :copy-into)
                                                       (-> db
                                                           (assoc-in [:panels dest-block source-key new-tab-name] new-tab-data)
                                                           (assoc-in [:panels dest-block :connection-id] connection-id))
                                                       (-> db
                                                           (assoc-in [:panels dest-block source-key new-tab-name] new-tab-data)
                                                           (assoc-in [:panels dest-block :connection-id] connection-id)
                                                           (ut/dissoc-in [:panels source-block source-key source-tab])
                                                           (ut/dissoc-in (if last-tab? ;; if we are moving out of the
                                                                                          ;; last tab,
                                                                           [:panels source-block] ;; another
                                                                                                     ;; dirty
                                                                                                     ;; hack
                                                                           [:panels source-block :nevermind!]))))
           :else                                     (if (= clause-conform :copy-into) ;; for views
                                                                                          ;; DONT
                                                                                          ;; overwrite
                                                                                          ;; the
                                                       (-> db
                                                           (assoc-in [:panels dest-block source-key new-tab-name]
                                                                     new-tab-data))
                                                       (-> db
                                                           (assoc-in [:panels dest-block source-key new-tab-name] new-tab-data)
                                                           (ut/dissoc-in [:panels source-block source-key source-tab])
                                                           (ut/dissoc-in (if last-tab? ;; if we are moving out of the
                                                                                          ;; last tab,
                                                                           [:panels source-block] ;; another
                                                                                                     ;; dirty
                                                                                                     ;; hack
                                                                           [:panels source-block :nevermind!]))))))
       (some #(= clause-conform %) [:x :y :color :row :column :size :shape]) ;;  :row :column
       (let [fname          (get-in drop-data [:drag-body :target])
             source-table   (ut/unre-qword (get-in drop-data [:drag-body :source-table]))
             views          (get-in db [:panels panel-key :views])
             selected-view  (get-in db [:panels panel-key :selected-view])
             has-queries?   (not (nil? (get-in db [:panels panel-key :queries])))
             no-view?       (nil? views)
             single-view?   (vector? views)
             view-map?      (map? views)
             selected-view? (cond no-view?           false
                                  view-map?          (true? (some #(= % selected-view) views))
                                  (not has-queries?) true
                                  :else              false)
             view-body      (ut/postwalk-replacer {} ;{:vega-default-color-scheme
                                                  (cond (= :vega-lite (first (get views :oz))) ;; this
                                                        (get views :oz)
                                                        :else                                  vega-lite-shell))
             new-view-body  (-> view-body
                                (assoc-in [1 :layer 0 :encoding clause-conform :field] fname)
                                (assoc-in [1 :data :values] source-table))]
         (assoc-in db [:panels panel-key :views :oz] new-view-body))))))

(defn droppable-pills
  [types-vec drop-data element]
  [(reagent/adapt-react-class rdnd/Droppable)
   {:types   types-vec
    :on-drop #(let [incoming (js->clj %)
                    data     (read-string (last (last incoming)))
                    type     (first (first incoming))]
                (ut/tapp>> [:dropped-pill type data
                            [::execute-pill-drop (get drop-data :target-panel) (get drop-data :target-query)
                             (get drop-data :clause) drop-data]])
                (reset! dragging? false)
                (reset! on-block? false)
                (reset! dragging-size [])
                (ut/tracked-dispatch [::execute-pill-drop (get drop-data :target-panel) (get drop-data :target-query)
                                      (get drop-data :clause) drop-data]))} [re-com/box :child element]])


(defn font-size-px [s h w] 17)

(re-frame/reg-sub ::all-drops-of
                  (fn [db [_ ttype]]
                    (let [rss (get-in db [:runstream-drops] {})]
                      (vec (apply concat
                                  (for [[_ v] rss]
                                    (for [[kk vv] v
                                          :when   (try (or (= ttype :*) (some #(= % ttype) (get vv :type)) (= ttype (get vv :type)))
                                                       (catch :default _ false))]
                                      kk)))))))

(re-frame/reg-sub ::all-drops-of-alpha
                  (fn [db {:keys [ttype]}]
                    (let [rss (get-in db [:runstream-drops] {})]
                      (vec (apply concat
                                  (for [[_ v] rss]
                                    (for [[kk vv] v
                                          :when   (try (or (= ttype :*) (some #(= % ttype) (get vv :type)) (= ttype (get vv :type)))
                                                       (catch :default _ false))]
                                      kk)))))))

(defn dynamic-spawner-targets
  [table-source dbody panel-key query-key width-int height-int]
  (let [drag-body (get dbody :drag-meta)
        selected-view @(ut/tracked-subscribe [::selected-view-w-queries panel-key])
        is-layout? false ;@(ut/tracked-subscribe [::is-layout? panel-key selected-view])
        layout-map (when is-layout?
                     (let [view @(ut/tracked-subscribe [::view panel-key selected-view])
                           kvs  (first (filter #(= (get-in view %) :layout) (ut/kvpaths view)))
                           kvs1 (assoc kvs (dec (count kvs)) 1)
                           v    (get-in view kvs1)]
                       [v [:panels panel-key :views selected-view kvs1 :panels]]))
        view-data-ref (get-in dbody (or (first (filter #(= (last %) :data) (ut/kvpaths dbody))) [:nope]))
        viz-for-each-fields
        (when (and (keyword view-data-ref) (not (nil? query-key)))
          (let [fxky              (fn [k] (if (cstr/starts-with? (str k) ":query/") (keyword (ut/replacer k #":query/" "")) k))
                base-query        @(ut/tracked-subscribe [::find-query (fxky view-data-ref)])
                base-query-from   (first (get base-query :from))
                base-query-fields (cond (map? base-query-from)     (get base-query-from :select)
                                        (keyword? base-query-from) (get @(ut/tracked-subscribe [::find-query
                                                                                                (fxky base-query-from)])
                                                                        :select
                                                                        [])
                                        :else                      [])
                local-fields      (get @(ut/tracked-subscribe [::find-query query-key]) :select [])
                local-meta        (get @(ut/tracked-subscribe [::conn/sql-metadata [query-key]]) :fields)
                possibles         (vec (cset/intersection (set local-fields) (set base-query-fields)))
                possibles-vec     (vec (for [p possibles :when (<= (get-in local-meta [p :distinct]) 50)] p))]
            possibles-vec))
        source-panel-key (get drag-body :source-panel-key)
        source-table (get drag-body :source-table)
        data-type (get drag-body :data-type)
        target (get drag-body :target)
        table-source-k (if (vector? (first table-source)) (first (first table-source)) (first table-source))
        not-sys? (not (or (= query-key :tables-sys)
                          (= query-key :fields-sys)
                          (= query-key :attribs-sys)
                          (= query-key :combos-sys)
                          (= query-key :connections-sys)))
        already-agg? nil ;; for pivot
        agg? (and (or (= data-type "integer") (= data-type "float"))
                  (not (cstr/includes? (cstr/lower-case (str target)) "year"))
                  (not (cstr/includes? (cstr/lower-case (str target)) "month"))
                  (not (cstr/includes? (cstr/lower-case (str target)) "week")))
        dim? (not agg?)
        connection-id @(ut/tracked-sub ::connection-id-alpha2 {:panel-key panel-key :query-key query-key})
        drag-type (get drag-body :type)
        param? (and (or (= drag-type :param) (= drag-type :where)) (not (cstr/includes? (str (get drag-body :param-full)) ".*/"))) ;; ignore
        query? (= drag-type :query)
        table? (or (= drag-type :meta-tables) (= drag-type :meta-fields) query?)
        field? (or (= drag-type :meta-fields) (= drag-type :field))
        viz-reco? (= drag-type :viz-reco)
        not-viz-reco? (not viz-reco?)
        view-drag? (= drag-type :view)
        block-panel? (true? (or view-drag? query?))
        not-view? (not view-drag?)
        same-db? (= connection-id (get drag-body :connection-id))
        ;;;_ (tapp>> [:drag-body drag-body])
        same-table? (or (= source-table (first table-source)) (= table-source-k source-table))
        clicked-fields (if view-drag? @(ut/tracked-subscribe [::view-clicked-fields source-panel-key source-table]) [])
        clicked? (ut/ne? clicked-fields)
        query-fields
        (if (or clicked? table?) (vec (keys (get @(ut/tracked-subscribe [::conn/sql-metadata [query-key]]) :fields))) [])
        pivot-dim-query-fields (keys (into {}
                                           (filter (fn [[_ v]] (and (= (get v :data-type) "string") (< (get v :distinct) 50)))
                                                   (get @(ut/tracked-subscribe [::conn/sql-merged-metadata [query-key]]) :fields))))
        common-fields (cond table?   (vec (cset/intersection (set query-fields) (set (get drag-body :table-fields))))
                            clicked? (vec (cset/intersection (set (vals clicked-fields)) (set query-fields)))
                            :else    [])
        union? (and (= (sort query-fields) (sort (get drag-body :table-fields))) (ut/ne? query-fields))
        partial-union? (and (>= (count (cset/intersection (set query-fields) (set (get drag-body :table-fields)))) 1) ;; do they
                                                                                                                      ;; share
                                                                                                                      ;; at
                                                                                                                      ;; least 2
                                                                                                                      ;; field
                                                                                                                      ;; names?
                            (ut/ne? query-fields))
        common-fields? (>= (count common-fields) 1)
        self? (= panel-key source-panel-key)
        view-based? (nil? table-source)
        w @(ut/tracked-subscribe [::panel-width panel-key])
        h @(ut/tracked-subscribe [::panel-height panel-key])
        multi-param? (and (or (= drag-type :param) (= drag-type :where)) (cstr/includes? (str (get drag-body :param-full)) ".*/"))
        is-child? (cstr/includes? (str table-source) ":query/")
        sql-options (vec
                     (apply concat
                            (remove nil?
                                    (conj
                                     []
                                     (when (and not-view? agg? self? (> (count pivot-dim-query-fields) 0))
                                       (partition-all 2 (for [f pivot-dim-query-fields] (keyword (str "pivot-on/" (ut/safe-name f))))))
                                     (when (and not-view? not-sys? view-based?) [[]])
                                     (when (and not-view? not-sys? self? (not table?)) [[:remove] [:order-by-asc :order-by-desc]])
                                     (when (and not-view? not-sys? (not self?) agg? field? same-table?)
                                       [[:sum :avg :min :max] [:group-by] [:order-by-asc :order-by-desc]])
                                     (when (and not-view? not-sys? param? self? (not table?))
                                       [[:remove] [:where= :where<>] [:highlight= :highlight> :highlight< :heatmap]])
                                     (when (and not-view? not-sys? param? (not self?) (not table?))
                                       [[:where= :where<>] [:highlight= :highlight> :highlight< :heatmap]])
                                     (when (and not-view? not-sys? multi-param? (not self?) (not table?)) [[:is-in :not-in]])
                                     (when (and not-view? not-sys? table? common-fields? (not self?) same-db? (not union?))
                                       (let [poss-joins (vec (take 10 (for [f common-fields] (keyword (str "join-on/" (ut/safe-name f))))))
                                             poss-cnt   (count poss-joins)]
                                         (if (> poss-cnt 3) (partition-all 2 poss-joins) (partition-all 1 poss-joins))))
                                     (when (and view-drag? clicked? common-fields?) [[:click-filter]])
                                     (when (and not-view? not-sys? dim? (not self?) field? same-table?)
                                       [[:count-distinct] [:group-by] [:order-by-asc :order-by-desc]])
                                     (when (and not-view? not-sys? union? (not self?) table? same-db?)
                                       (if (<= w 5) [[:union] [:union-all]] [[:union :union-all]]))
                                     (when (and not-view? not-sys? partial-union? (not self?)) ;table? same-db? ;;
                                       [[:filter-shadow]])
                                     (when (and not-view? not-sys? partial-union? (not self?)) ;table? same-db? ;;
                                       [[:highlight-rows]])))))
        viz-options [[:x :y :color] [:size :row :column] [:shapes :label]]
        action-typer (try (keyword (get-in dbody [:drag-meta :data-type])) (catch :default _ :error!)) ;; (get-in
        drops @(ut/tracked-subscribe [::all-drops-of action-typer])
        view-options (remove nil? [[:text] (when (ut/ne? drops) drops) (when dim? [:table-filter])])
        color-map (merge {:sum            "#9973e0"
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
                          :materialize-to-cache-db     "#7eeeee"
                          :materialize-values     "#7eeeee"
                          :delete-view    "#ff3900"
                          :highlight=     "#7eeeee"
                          :highlight>     "#7eeeee"
                          :highlight<     "#7eeeee"
                          :heatmap        "#7eeeee"
                          :join           "#8c78b7"}
                         (into {} (vec (for [f viz-for-each-fields] {(keyword (str "weave/" (ut/safe-name f))) "#7FFFD4"})))
                         (into {} (for [f pivot-dim-query-fields] {(keyword (str "pivot-on/" (ut/safe-name f))) "#FFA500"}))
                         (into {} (for [f pivot-dim-query-fields] {(keyword (str "🌶️pivot-on/" (ut/safe-name f))) "#ff5c3a"}))
                         (into {} (for [f common-fields] {(keyword (str "join-on/" (ut/safe-name f))) "#8c78b7"})))
        block-ops (into (if (>= w 10)
                          (if view-drag? [[:move-into :copy-into :reference]] [[:move-into :copy-into :row-count]])
                          (if view-drag? [[:move-into] [:copy-into] [:reference]] [[:move-into] [:copy-into] [:row-count]]))
                        (when (ut/ne? viz-for-each-fields)
                          (if (>= w 10)
                            (vec (for [ff (partition-all 3 viz-for-each-fields)]
                                   (vec (for [f ff] (keyword (str "weave/" (ut/safe-name f)))))))
                            (vec (for [f viz-for-each-fields] [(keyword (str "weave/" (ut/safe-name f)))])))))
        general-color #(get color-map (first (first %)))
        has-view-options? (ut/ne? (flatten view-options))
        has-viz-options? (ut/ne? (flatten viz-options))
        has-sql-options? (ut/ne? (flatten sql-options))
        big-enough-for-viz? (and (>= w 6) (>= h 5))
        all-options
        (cond is-layout?
              {:layout-map (vec (for [[k v] (get-in layout-map [0 :panels])] [k]))
               :view-opts  (if (and view-drag? self?) [[:move-into] [:delete-view] [:copy-into]] [[:move-into] [:copy-into]])}
              viz-reco? []
              (and ;(>= w 11) (>= h 6)
               view-based?
               not-sys?
               (not block-panel?))
              (merge (if (and not-view? not-sys? (not self?) agg? field?)
                       {:new-query (if (<= w 9) [[:sum] [:avg] [:min] [:max]] [[:sum :avg] [:min :max]])}
                       {})
                     (if (and not-view? not-sys? (not self?) table?) {:new-query [[:row-count]]} {})
                     (if (and (not table?) has-viz-options? big-enough-for-viz?) {:new-viz viz-options} {})
                     (if (and (not table?) has-view-options?) {:new-view view-options} {})
                     (if (and (not self?) block-panel?) {:block-ops block-ops} {}))
              (and not-sys? view-based? (not block-panel?))
              (merge
               (if (and (not self?) block-panel?) {:block-ops block-ops} (if big-enough-for-viz? {:new-viz viz-options} {}))
               (if (and self? block-panel?)
                 {:block-ops
                  [(if (= drag-type :query)
                     (if is-child?
                       [:delete-query :promote-query :materialize-to-cache-db]
                       [:delete-query :materialize-to-cache-db])
                     [:delete-view :materialize-values])]}
                 {}))
              (and (>= w 11) (>= h 6) not-sys?)
              (merge (if has-sql-options? {selected-view sql-options} {})
                     (if (and (not self?) block-panel?)
                       {:block-ops block-ops}
                       (if (and (not table?) has-viz-options? big-enough-for-viz?) {:new-viz viz-options} {}))
                     (if (and self? block-panel?)
                       {:block-ops [(if (= drag-type :query)
                                      (if is-child?
                                        [:delete-query :promote-query :materialize-to-cache-db]
                                        [:delete-query :materialize-to-cache-db])
                                      [:delete-view :materialize-values])]}
                       {}))
              not-sys? (merge (if has-sql-options? {selected-view sql-options} {})
                              (if (and (not self?) block-panel?) {:block-ops block-ops} {})
                              (if (and self? block-panel?)
                                {:block-ops [(if (= drag-type :query)
                                               (if is-child?
                                                 [:delete-query :promote-query :materialize-to-cache-db]
                                                 [:delete-query :materialize-to-cache-db])
                                               [:delete-view :materialize-values])]}
                                {})))
        option-count (count (flatten (apply concat (vals all-options))))]
    (when (> option-count 0)
      (let [hov @dyn-dropper-hover]
        [re-com/h-box :attr
         {:on-drag-over #(when (not @on-block?) (reset! on-block? true)) :on-drag-leave #(reset! on-block? false)} :gap "5px"
         :size "auto" :width (if view-based? (px (- width-int 10)) (px (+ 20 width-int))) :height (px (- height-int 50)) :style
         {:position "fixed" :font-family (theme-pull :theme/base-font nil) :z-index 100} :children
         (for [[tname toptions] all-options]
           [re-com/v-box :gap "5px" :size "auto" :children
            [;[re-com/box :child (str "you are: " source-table " I am: " table-source)]
             (when (and (not (= tname :layout-map)) (not (= tname :view-opts)))
               [re-com/box :child (str tname) :align :center :justify :center :style
                {:background    (str (general-color toptions))
                 :border-radius "10px"
                 :font-family   (theme-pull :theme/base-font nil)
                 :color         "#000000"
                 :font-weight   700
                 :font-size     "15px"}])
             [(if (= tname :view-opts) re-com/h-box re-com/v-box) :gap "5px" :style
              (if (= tname :view-opts) {:position "fixed" :bottom -22 :left 0} {}) :width
              (when (= tname :view-opts) (px (+ width-int 50))) :size (if (= tname :view-opts) "none" "auto") :children
              (for [opts toptions]
                (cond
                  (= tname :layout-map) ;; fancy drop targets for layoutmap
                  (let [pw-int (/ width-int db/brick-size)
                        ph-int (/ height-int db/brick-size)
                        processed-layout (lay/process-layout (get (first layout-map) :panels) pw-int ph-int)
                        brix (fn [x & [mod]] (px (if mod (+ (* db/brick-size x) mod) (* db/brick-size x))))
                        boxes
                        (vec
                         (for [[view-ref p] processed-layout]
                           (let [h (get p :h 4)
                                 w (get p :w 4)
                                 [x y] (get p :root [0 0])
                                 pstyle (get p :style {})
                                 left (+ -1 (* x db/brick-size))
                                 top (+ 25 (* y db/brick-size))
                                 root-visible? (not (or (> x (- pw-int 1)) (> y (- ph-int 2))))
                                 box-map {:child   (str view-ref)
                                          :size    "none"
                                          :width   (brix w)
                                          :height  (brix h)
                                          :align   :center
                                          :justify :center
                                          :attr    {;:on-drag-enter #(reset! dyn-dropper-hover
                                                    :on-drag-over  #(when (not (= @dyn-dropper-hover [panel-key view-ref]))
                                                                      (reset! dyn-dropper-hover [panel-key view-ref]))
                                                    :on-click      #(reset! dragging? false)
                                                    :on-drag-leave #(reset! dyn-dropper-hover nil)}
                                          :style   (merge {;:border "2px dashed yellow"
                                                           :border           (str "2px "
                                                                                  (if (= tname :new-viz22) "dashed" "solid")
                                                                                  " "
                                                                                  (get color-map view-ref)
                                                                                  (if (= tname :new-viz22) "77" ""))
                                                           :font-family      (theme-pull :theme/base-font nil)
                                                           :position         "fixed" ; "relative"
                                                           :overflow         "hidden"
                                                           :color            (if (= [panel-key view-ref] hov) "red" "yellow")
                                                           :background-color (if (= [panel-key view-ref] hov)
                                                                               (str (get color-map view-ref) 55)
                                                                               "#00000011")
                                                           :filter           (str "drop-shadow(0.35rem 0.35rem 0.4rem "
                                                                                  (get color-map view-ref)
                                                                                  55 " )")
                                                           :left             left
                                                           :top              top}
                                                          pstyle)}
                                 leftovers (dissoc p :style :h :w :root :k :z :view-ref :hidden? :id)]
                             (when root-visible?
                               (droppable-pills ["meta-menu"]
                                                {:clause       (if (= tname :layout-map)
                                                                 {:layout-drop view-ref :kp (last layout-map)}
                                                                 view-ref)
                                                 :drag-body    drag-body
                                                 :target-panel panel-key
                                                 :target-query query-key}
                                                (ut/mapply re-com/box (merge leftovers box-map)))))))]
                    [re-com/h-box :size "none" :width (brix pw-int -10) :height (brix ph-int -47) ;; footer,
                                                                                                    ;; will
                                                                                                    ;; be
                                                                                                    ;; dynamic
                                                                                                    ;; TODO
                     :style
                     {;:border "1px solid lime" ;; debug edges :position "absolute"
                      :overflow    "hidden"
                      :font-family (theme-pull :theme/base-font nil)} :children boxes])
                  (= tname :view-opts)
                  (do (ut/tapp>> [:view-opts-footer opts toptions])
                      [re-com/h-box :gap "5px" :style
                       {;:padding-left "0px"
                        :margin-left    "-1px"
                        :overlay-filter "blur(4px)"} :justify :between :size "none" :children
                       (for [o opts]
                         [re-com/box :child
                          (droppable-pills ["meta-menu"]
                                           {:clause       (if (= tname :layout-map) {:layout-drop o :kp (last layout-map)} o)
                                            :drag-body    drag-body
                                            :target-panel panel-key
                                            :target-query query-key}
                                           [re-com/box :size "none" :width (px (/ (- width-int 13) (count toptions))) :align
                                            :center :justify :center :attr
                                            {;:on-drag-enter #(reset! dyn-dropper-hover [panel-key o])
                                             :on-drag-over  #(when (not (= @dyn-dropper-hover [panel-key o]))
                                                               (reset! dyn-dropper-hover [panel-key o]))
                                             :on-click      #(reset! dragging? false)
                                             :on-drag-leave #(reset! dyn-dropper-hover nil)} :child (str o)]) :align :center
                          :justify :center :style
                          {:border           (str "2px "
                                                  (if (= tname :new-viz22) "dashed" "solid")
                                                  " "
                                                  (get color-map o)
                                                  (if (= tname :new-viz22) "77" ""))
                           :font-weight      700
                           :font-size        (font-size-px (str o " ") (/ h (+ 1 (count toptions))) (/ w 3))
                           :filter           (str "drop-shadow(0.35rem 0.35rem 0.4rem " (get color-map o) 55 " )")
                           :color            (if (= [panel-key o] hov) "#000000" (get color-map o))
                           :background-color (if (= [panel-key o] hov) (str (get color-map o) 55) "#00000055")
                           :font-family      (theme-pull :theme/base-font nil)} :size "auto"])])
                  :else [re-com/h-box :gap "5px" :size "auto" :children
                         (for [o opts]
                           [re-com/box :child
                            (droppable-pills ["meta-menu"]
                                             {:clause       (if (= tname :layout-map) {:layout-drop o :kp (last layout-map)} o)
                                              :drag-body    drag-body
                                              :target-panel panel-key
                                              :target-query query-key}
                                             [re-com/box :size "auto" :attr
                                              {;:on-drag-enter #(reset! dyn-dropper-hover [panel-key o])
                                               :on-drag-over  #(when (not (= @dyn-dropper-hover [panel-key o]))
                                                                 (reset! dyn-dropper-hover [panel-key o]))
                                               :on-click      #(reset! dragging? false)
                                               :on-drag-leave #(reset! dyn-dropper-hover nil)} :child (str o)]) :align :center
                            :justify :center :style
                            {:border           (str "2px "
                                                    (if (= tname :new-viz22) "dashed" "solid")
                                                    " "
                                                    (get color-map o)
                                                    (if (= tname :new-viz22) "77" ""))
                             :font-weight      700
                             :font-size        (font-size-px (str o " ") (/ h (+ 1 (count toptions))) (/ w 3))
                             :filter           (str "drop-shadow(0.35rem 0.35rem 0.4rem " (get color-map o) 55 " )")
                             :color            (if (= [panel-key o] hov) "#000000" (get color-map o))
                             :background-color (if (= [panel-key o] hov) (str (get color-map o) 55) "#00000011")
                             :font-family      (theme-pull :theme/base-font nil)} :size "auto"])]))]]])]))))

(re-frame/reg-sub ;; sub is cached so faster than doing this expensive run each time? will sync
 ::dynamic-drop-targets ;; @(ut/tracked-subscribe [::dynamic-drop-targets table-source dbody
 (fn [_ [_ table-source dbody panel-key query-key width-int height-int]]
   [dynamic-spawner-targets table-source dbody panel-key query-key width-int height-int]))

(re-frame/reg-sub ;; sub is cached so faster than doing this expensive run each time? will sync
 ::dynamic-drop-targets-alpha ;; @(ut/tracked-subscribe [::dynamic-drop-targets table-source dbody
 (fn [_ {:keys [table-source dbody panel-key query-key width-int height-int]}]
   [dynamic-spawner-targets table-source dbody panel-key query-key width-int height-int]))


(re-frame/reg-sub ::get-sql-server-string
                  (fn [db [_ k]]
                    (let [s (get-in db [:sql-str k])]
                      (try (let [s (ut/replacer s #"\\n" "\n")] (subs s 1 (- (count s) 1))) (catch :default _ s)))))


(defn materialize-one-sql
  [panel-id sql-key]
  (let [block-map @(ut/tracked-subscribe [::workspace [panel-id]])
        sql-calls {sql-key (get-in block-map [:queries sql-key])}]
    (first (for [[k v] sql-calls]
             (let [query     (-> v
                                 ut/clean-sql-from-ui-keys
                                 sql-alias-replace-sub)
                   data-key  (get-in query (first (filter #(= (last %) :data) (ut/kvpaths query))))
                   query     (if (not (empty? data-key))
                               (ut/postwalk-replacer {{:data data-key} {:select [:*] :from [(str k "/data-entry")]}} query)
                               query)
                   str-query [@(ut/tracked-subscribe [::get-sql-server-string k])]]
               (str "-- " k "\n" (first str-query)))))))

(defn formula-code-box
  [panel-id key width-int height-int type]
  (let [;sql-hint? (cstr/includes? (str value) ":::sql-string")
        honey-sql  @(ut/tracked-subscribe [::query panel-id key])
        sql-string (materialize-one-sql panel-id key)]
    [re-com/box :size "auto" :width (px (- width-int 24)) :height (px (- height-int 24)) :style
     {:font-family (theme-pull :theme/monospaced-font nil) ; "Fira Code" ; "Chivo Mono"
      :font-size   "12px"
      :overflow    "auto"
      :font-weight 700} :child
     [(reagent/adapt-react-class cm/UnControlled)
      {:value   (if (= type :honey) (ut/format-map (- width-int -40) (str honey-sql)) (ut/splitter (str sql-string) #"\n"))
       :options {:mode              (if (= type :string) "sql" "clojure")
                 :lineWrapping      true
                 :lineNumbers       false
                 :matchBrackets     true
                 :autoCloseBrackets true
                 :autofocus         false
                 :autoScroll        false
                 :detach            true
                 :readOnly          false ;true
                 :theme             (theme-pull :theme/codemirror-theme nil)}}]])) ; "ayu-mirage-formula" ;"hopscotch"

(defonce formula-bar (reagent/atom nil))

(defonce value-hover (reagent/atom nil))

(re-frame/reg-sub ::stylers
                  (fn [db [_ panel-key query-key]]
                    (into {}
                          (for [[[cols name] logic-map] (get-in db [:sql-source query-key :style-rules])]
                            {name (merge {:cols cols} logic-map)}))))

(defn hex-to-rgb [hex]
  (try
    (let [r (js/parseInt (subs hex 1 3) 16)
          g (js/parseInt (subs hex 3 5) 16)
          b (js/parseInt (subs hex 5 7) 16)]
      [r g b])
    (catch :default _ [0 0 0])))

(defn interpolate [start end factor]
  (Math/round (+ start (* (- end start) factor))))

(defn clamp [n min max]
  (cond
    (< n min) min
    (> n max) max
    :else n))

(defn generate-gradient [hex1 hex2 steps]
  (let [rgb1 (hex-to-rgb hex1)
        rgb2 (hex-to-rgb hex2)
        step-factors (map #(/ % (dec steps)) (range steps))]
    (mapv (fn [factor]
            (mapv #(clamp (interpolate %1 %2 factor) 0 255) rgb1 rgb2))
          step-factors)))

(defn rgb-to-hex [rgb]
  (str "#" (cstr/join (map #(-> %
                                (clamp 0 255)
                                (js/Math.round)
                                (js/Number.prototype.toString.call 16)
                                (cstr/replace #"^([0-9a-f])$" "0$1"))
                           rgb))))

(defn gen-gradient [hex1 hex2 steps]
  (mapv rgb-to-hex (generate-gradient hex1 hex2 steps)))

;;(tapp>> [:grad (str (gen-gradient "#ff0000" "#0000ff" 4))])

;;    [:heatmap :PuRd :9 :asc]
;; or [:heatmap ["#ffffff"  "#000000"] 9 :asc]

(defn get-color-temp
  [weight scheme depth]
  (let [val1       (/ weight 100)
        hue        (* (- 1 val1) 120)
        temp       (str "hsl(" hue ",100%,50%)")
        custom?    (and (vector? scheme) (= (count scheme) 2))
        ;;_ (tapp>> [:scheme :why scheme custom?] )
        scheme     (if custom?
                     (gen-gradient (first scheme) (second scheme) 3)
                     (get-in ut/colorbrewer [scheme depth] []))
        num-scheme (count scheme)
        bins       (/ 100 num-scheme)
        wbin       (js/Math.ceil (/ weight bins))
        scale      (get scheme wbin)]
    (if (= 0 num-scheme) temp scale)))

(re-frame/reg-sub ::heatmap-color-val
                  (fn [db [_ panel-key query-key field val styler-keys scheme depth direction]]
                    ;(tapp>> [:fuk panel-key query-key field val styler-keys scheme depth direction])
                    (let [;other-vals (vec (distinct (for [r (get-in db [:data query-key])] (get r field))))
                          ;;scheme ["#ffffff"  "#000000"]
                          styler-field (keyword (ut/replacer (str "styler_" (ut/safe-name styler-keys)) #"-" "_"))
                          local-vals   (remove nil?
                                               (for [r (get-in db [:data query-key])] (when (= (get r styler-field) 1) (get r field))))
                          other-vals   (cond (= direction :asc)  (vec (sort local-vals))
                                             (= direction :desc) (vec (reverse (sort local-vals)))
                                             :else               (vec (sort local-vals)))
                          vals-cnt     (count other-vals)
                          idx          (.indexOf other-vals val)
                          weight       (Math/round (* (/ idx vals-cnt) 100))
                          wmix         (get-color-temp weight scheme depth)]
                      wmix)))

(re-frame/reg-sub ::heatmap-color
                  (fn [db [_ panel-key query-key field]]
                    (into {}
                          (for [[[cols name] logic-map] (get-in db [:panels panel-key :queries query-key :style-rules])]
                            {name (merge {:cols cols} logic-map)}))))


(re-frame/reg-sub ::column-selected?
                  (fn [db [_ panel-key query-key field]]
                    (true? (when (not (nil? (get db :selected-cols)))
                             (let [c             (get db :selected-cols)
                                   sel-panel-key (safe-nth c 0)
                                   sel-query-key (safe-nth c 1)
                                   sel-field     (safe-nth c 2)]
                               (and (= panel-key sel-panel-key) (= sel-query-key query-key) (= sel-field field)))))))

(re-frame/reg-sub ::column-selected-any-field?
                  (fn [db [_ panel-key query-key]]
                    (true? (when (not (nil? (get db :selected-cols)))
                             (let [c             (get db :selected-cols)
                                   sel-panel-key (safe-nth c 0)
                                   sel-query-key (safe-nth c 1)]
                               (and (= panel-key sel-panel-key) (= sel-query-key query-key)))))))


(re-frame/reg-sub ::column-selected
                  (fn [db [_ panel-key query-key]]
                    (when (not (nil? (get db :selected-cols)))
                      (let [c             (get db :selected-cols)
                            sel-panel-key (safe-nth c 0)
                            sel-query-key (safe-nth c 1)
                            sel-field     (safe-nth c 2)]
                        (when (and (= panel-key sel-panel-key) (= sel-query-key query-key)) sel-field)))))

(re-frame/reg-event-db ::column-select
                       (undoable)
                       (fn [db [_ panel-key query-key field]]
                         (if (not (nil? field))
                           (assoc db :selected-cols [panel-key query-key field])
                           (-> db
                               (assoc :selected-cols nil)
                               (assoc :col-names nil)))))

(re-frame/reg-event-db
 ::cycle-column-select
 (undoable)
 (fn [db [_ forward?]]
   (if (not (empty? (get db :selected-cols)))
     (let [selected-panel (get db :selected-block)
           selected-tab   (get-in db [:panels selected-panel :selected-view])
           qkeys          (keys (get-in db [:panels selected-panel :queries]))
           query-key      (cond (and (nil? selected-tab) (not (empty? qkeys)))                   (first qkeys)
                                (and (not (nil? selected-tab)) (some #(= % selected-tab) qkeys)) selected-tab ;(first
                                :else                                                            nil)
           curr-field     (get-in db [:selected-cols 2])
           fields         (keys (get-in db [:meta query-key :fields]))
           field-idx      (.indexOf fields curr-field)
           new-field      (nth (cycle fields) (if forward? (+ field-idx 1) (- field-idx 1)))]
       (when (not (nil? query-key)) (assoc db :selected-cols [selected-panel query-key new-field])))
     (let [curr-field (get db :selected-tab)
           fields     @(ut/tracked-subscribe [::visible-tabs]) ;; sneaky sneaky
           field-idx  (.indexOf fields curr-field)
           new-field  (nth (cycle fields) (if forward? (+ field-idx 1) (- field-idx 1)))]
       (-> db
           (assoc :selected-tab new-field)
           (assoc-in [:click-param :sys :selected-tab] new-field)
           (assoc-in [:click-param :sys :selected-tab-idx] (try (.indexOf (get db :tabs) new-field) (catch :default _ -1))))))))

(re-frame/reg-sub
 ::custom-col-widths
 (fn [db [_ panel-key query-key]]
   (get-in db [:panels panel-key :queries query-key :col-widths] (get-in db [:sql-source query-key :col-widths] {})) ;; custom
                                                                                                                      ;; queries
                                                                                                                      ;; that
                                                                                                                      ;; have no
   ))

(re-frame/reg-sub ::custom-rows
                  (fn [db [_ panel-key query-key]]
                    (select-keys (get-in db [:panels panel-key :queries query-key] {})
                                 [:row-height :render-all? :clicked-row-height])))

(re-frame/reg-event-db ::set-column-default-widths
                       (fn [db [_ panel-key query-key w]] (assoc-in db [:default-col-widths panel-key query-key] w)))

(re-frame/reg-sub ::column-default-widths
                  (fn [db {:keys [panel-key query-key]}] (get-in db [:default-col-widths panel-key query-key])))

(declare honeycomb)                                         ;; eyes emoji - would be better to restructure, eh?

(defn insert-hidden-reco-preview
  [reco-selected reco-viz reco-query reco-condis combo-name shape-name single?]
  (let []
    (dorun
     (when single? (ut/tracked-dispatch [::new-reco-preview reco-selected]))
     (cond (= (first (read-string reco-viz)) :vega-lite)
           (let [incomingv     (read-string reco-viz) ;; read-string will ruin my internal
                 ffromv        (-> (get-in incomingv [1 :data :values])
                                   (ut/replacer "_" "-")
                                   (ut/replacer "query/" ""))
                 original-conn @(ut/tracked-subscribe [::lookup-by-query-key (keyword ffromv)])
                 view          (-> incomingv
                                   (assoc-in [1 :data :values] :query-preview)
                                   (assoc-in [1 :config] :theme/vega-defaults)
                                   (assoc-in [1 :width] "container") ;:panel-width)
                                   (assoc-in [1 :height] :panel-height)
                                   (assoc-in [1 :padding] 4)
                                   (assoc-in [1 :background] "transparent"))
                 q-data        (read-string reco-query)
                 incoming      (first q-data) ;; read-string will ruin my internal
                 ffrom         (ut/replacer (first (get incoming :from)) "_" "-")
                 query         (vec (for [q q-data] (assoc q :from [(keyword ffrom)])))
                 query         (ut/postwalk-replacer {[[:sum :rows]] [:count 1]} query)]
             (insert-rec-preview-block view
                                       query         ;reco-h reco-w
                                       reco-condis
                                       original-conn ;reco-conn
                                       reco-selected
                                       combo-name
                                       shape-name
                                       single?
                                       false))
           :else (let [view          (read-string reco-viz) ;; read-string will ruin my
                       q-data        (read-string reco-query)
                       incoming      (first q-data) ;; read-string will ruin my internal
                       ffrom         (ut/replacer (first (get incoming :from)) "_" "-")
                       original-conn @(ut/tracked-subscribe [::lookup-connection-id-by-query-key
                                                             (keyword (last (ut/splitter ffrom "/")))])
                       query         (vec (for [q q-data] (if (nil? (find q :vselect)) (assoc q :from [(keyword ffrom)]) q)))
                       query         (ut/postwalk-replacer {[:sum :rows] [:count 1]} query)]
                   (insert-rec-preview-block view
                                             query         ; reco-h reco-w
                                             reco-condis
                                             original-conn ;reco-conn
                                             reco-selected
                                             combo-name
                                             shape-name
                                             single?
                                             false))))))

(defn clear-preview2-recos
  []
  (dorun (let [prev-preview-keys (for [k @(ut/tracked-subscribe [::preview-keys2])] (keyword (str "reco-preview" k)))]
           (doseq [k prev-preview-keys] (ut/tracked-dispatch [::quick-delete-panel k]))
           (ut/tracked-dispatch [::set-preview-keys2 []])
           (ut/tracked-dispatch [::clear-query :recos-sys2]))))


(defn mad-libs-shapes
  [query-id width-int height-int]
  (let [;all-sql-call-keys @(ut/tracked-subscribe [::all-sql-call-keys])
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
                                      :where    [:and [:not [:like :table_name "query_preview%"]] [:= :table_name query-id]] ;; changed
                                                                                                                             ;; to
                                                                                                                             ;; single
                                                                                                                             ;; from
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
                       (str (rounder score))]]] [honeycomb panel-key (or selected-view :oz) ww hh]
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
               (when (or (not data-exists?) unrun-sql?) (conn/sql-data [k] query)))))
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

(def timer-id (reagent/atom nil))
(def hover-field (reagent/atom nil))
(def animate? (reagent/atom false))

(re-frame/reg-sub ::group-by-intersection
                  (fn [db [_ query-key1 query-key2]]
                    (let [;queries (into {} (for [p (get-in db [:panels])] (get p :queries)))
                          qgb1 (vec (for [[f fv] (get-in db [:meta query-key1 :fields]) :when (get fv :group-by?)] f))
                          qgb2 (vec (for [[f fv] (get-in db [:meta query-key2 :fields]) :when (get fv :group-by?)] f))]
                      (ut/tapp>> [qgb1 qgb2])
                      [])))

(re-frame/reg-sub ::group-bys
                  (fn [db [_ query-key1]]
                    (let [;queries (into {} (for [p (get-in db [:panels])] (get p :queries)))
                          qgb1 (vec (for [[f fv] (get-in db [:meta query-key1 :fields]) :when (get fv :group-by?)] f))]
                      qgb1)))

(re-frame/reg-sub
 ::query-waitings
 (fn [_ [_ query-key]]
   (let [;; messages (map :message @(ut/tracked-subscribe [::wfx/pending-requests
         waitings    (true? (some #(= % query-key)
                                  (for [e @(ut/tracked-sub ::http/pending-requests {:socket-id http/socket-id})]
                                    (let [p (get-in e [:message :ui-keypath])] (when (> (count p) 1) (first p))))))
         single-wait (true? (some #(= % query-key)
                                  (for [e @(ut/tracked-sub ::http/pending-requests {:socket-id http/socket-id})]
                                    (let [p (get-in e [:message :ui-keypath])] (when (= (count p) 1) (first p))))))]
     [waitings single-wait])))


(re-frame/reg-sub
 ::queries-running
 (fn [db _]
   (let [blocks (ut/deep-remove-keys (into {} (filter #(= (get db :selected-tab) (get (val %) :tab ""))
                                                      (get db :panels)))
                                     [:root :selected-mode :opts :root :selected-view :icon :icon-view])
         blocks (filterv #(= (count %) 3) (ut/kvpaths blocks))
         queries (mapv last (filterv #(= (second %) :queries) blocks))
         waitings    (for [e @(ut/tracked-sub ::http/pending-requests {:socket-id http/socket-id})]
                       (let [p (get-in e [:message :ui-keypath])] (when (> (count p) 1) (first p))))]
     (cset/intersection (set queries) (set waitings))
     ;waitings
     ;queries
     )))


(re-frame/reg-event-db
 ::toggle-heatmap
 (fn [db [_ panel-key query-key column]]
   (let [query           (get-in db [:panels panel-key :queries query-key])
         style-rules     (get query :style-rules)
         heat-keys       (into {}
                               (first (for [[[k1 k2] v] style-rules
                                            :when       (and (cstr/starts-with? (str k2) ":heatmap-") (= k1 column))]
                                        {[k1 k2] v})))
         has-one?        (ut/ne? heat-keys)
         base-key        [column (keyword (str "heatmap-" (rand-int 45)))]
         base-style      {:logic true :style {:background-color [:heatmap] :border "1px solid #00000000" :color "#000000"}}
         new-style-rules (if has-one? (dissoc style-rules (first (keys heat-keys))) (assoc style-rules base-key base-style))]
     (if (empty? new-style-rules)
       (ut/dissoc-in db [:panels panel-key :queries query-key :style-rules]) ;; no need for
       (assoc-in db [:panels panel-key :queries query-key :style-rules] new-style-rules)))))

(re-frame/reg-sub ::server-kits
                  (fn [db [_ type]] ;; TODO add "who is asking" deets like panel, query, so we can do kit WHEN
                    (let [all  (get-in db [:server :settings :kits])
                          curr @(ut/tracked-sub ::conn/clicked-parameter-key-alpha {:keypath [:kits-sys/enabled]})
                          kits (vec (sort-by :name
                                             (for [[k v] all
                                                   :when (and (some #(= [(get v :package-name) (get v :kit-name)] %) curr) ;; is
                                                                                                                           ;; enabled
                                                                                                                           ;; in
                                                                                                                           ;; params?
                                                              (= (get v :run-on) type))]
                                               (merge {:kit-name k} v))))]
                      kits)))

(re-frame/reg-sub
 ::valid-kits
 (fn [db _]
   (get db :valid-kits)))

(re-frame/reg-sub
 ::valid-kits-for
 (fn [db [_ {:keys [panel-key data-key]}]]
   (let [valid-kits (get db :valid-kits)]
     (for [[k v] valid-kits
           :when (some #(and (= (first %) panel-key)
                             (= (last %) data-key)) v)] k))))

(re-frame/reg-sub
 ::installed-kits
 (fn [db _] ;; TODO add "who is asking" deets like panel, query, so we can do kit WHEN
   (let [all  (get-in db [:server :settings :kits])
         kits (vec (sort-by :name (for [[k v] all] (merge {:installed? true :kit-name k} v))))]
     kits)))

(re-frame/reg-sub
 ::is-implied-rowset?
 (fn [db {:keys [data-keypath]}]
   (let [implicit-rowsets (vec (into (get-in db [:implicit-rowsets :clover] []) (get-in db [:implicit-rowsets :solver] [])))]
     (some #(= % (first data-keypath)) implicit-rowsets))))

(re-frame/reg-sub
 ::market-kits
 (fn [_ _] ;; TODO add "who is asking" deets like panel, query, so we can do kit
            ;; WHEN
   [{:kit-name :cool-thing1 :name "Todd Howard" :description "community repo placeholder1" :package-name :cool-package1}
    {:kit-name :sandwiches! :name "Sandwiches!" :description "sandwiches!" :package-name :skyrim-sandwiches}
    {:kit-name :cool-thing3 :name "Horse Armour" :description "community repo placeholder3" :package-name :cool-package1}
    {:kit-name :cool-thing4 :name "Gnomes" :description "community repo placeholder4" :package-name :cool-package1}]))


(defn render-kit-icon-query [icon & [class]]
  (if (and (ut/ne? icon) (not (nil? icon)))
    (if (or (cstr/starts-with? icon "zmdi-")
            (cstr/starts-with? icon "fa-"))
      [re-com/md-icon-button
       :src (at)
       :class class
       :md-icon-name icon
       :style ;{:color (theme-pull :theme/editor-outer-rim-color nil)
              ; :transform-origin "7.5px 10px"
              ; :font-size "11px"}
       {:font-size        "15px"
        :cursor           "pointer"
        :transform-origin "7.5px 10px"
        :opacity          0.5
        :padding          "0px"
        :margin-top       "-3px"}
       ]
      [re-com/box
       :size "none"
       :class class
       :child [:img {:src icon
                     :style {:width "100%"
                             :height "100%"
                             :object-fit "cover"}
                     :transform-origin "7.5px 10px"
                     :height "12px"}]])
    (render-kit-icon-query "zmdi-pizza")))

(defn magic-table
  [panel-key data-keypath & [ww hh hide-fields]]
  (let [viewer-sourced? (try (and (cstr/starts-with? (first data-keypath) "::") (string? (first data-keypath)))
                             (catch :default _ false))
        data-keypath (if viewer-sourced?
                       (try [(edn/read-string (ut/replacer (first data-keypath) "::" ":"))] (catch :default _ nil))
                       data-keypath) ;; unpack the viewer string
        panel-map @(ut/tracked-subscribe [::workspace [panel-key]])

        implied-rowset? @(ut/tracked-sub ::is-implied-rowset? {:data-keypath data-keypath})
        draggable (if implied-rowset? draggable-stub draggable)
        query-key (first data-keypath)
        w (get panel-map :w 11)
        h (get panel-map :h 10)
        column-selected @(ut/tracked-subscribe [::column-selected panel-key query-key]) ;; reaction
        custom-col-widths @(ut/tracked-subscribe [::custom-col-widths panel-key query-key])
        {:keys [row-height render-all? clicked-row-height]} @(ut/tracked-subscribe [::custom-rows panel-key query-key])
        width-int (- (* (if ww ww w) db/brick-size) 31)
        modded-width (- width-int 8)
        height-int (* (if hh hh h) db/brick-size)
        height-int (if viewer-sourced? (+ height-int 38) height-int)
        selected? @(ut/tracked-sub ::selected-block? {:panel-key panel-key})
        page-num @(ut/tracked-subscribe [::page-num panel-key query-key])
        parameters-used-in @(ut/tracked-sub ::parameters-used-from {:query-key query-key})
        parameters-used-in-sql (vec (filter #(not (vector? %)) parameters-used-in))
        parameters-used-in-cell (vec (filter #(vector? %) parameters-used-in))
        selected-block @(ut/tracked-sub ::selected-block {})
        subq-blocks @(ut/tracked-sub ::subq-panels-alpha {:panel-id selected-block})
        parent-of-selected? (some #(= % panel-key) subq-blocks)
        server-kits @(ut/tracked-subscribe [::server-kits :queries])
        deeper-upstream? false ; (some #(= % panel-key) (ut/cached-upstream-search subq-mapping
        kit-callout-fields []
        child-parts (into kit-callout-fields
                          (cond parent-of-selected?
                                (vec
                                 (ut/deep-flatten (merge @(ut/tracked-sub ::panel-sql-calls
                                                                          {:panel-key selected-block})
                                                         @(ut/tracked-sub ::views {:panel-key selected-block}))))
                                :else               []))
        hide?
        #_{:clj-kondo/ignore [:not-empty?]}
        (not (empty? hide-fields)) ;(not (empty? hide-fields))
        rowset (ut/tracked-sub ::conn/sql-data-alpha {:keypath data-keypath})
        non-panel? (false? (or (nil? hh) (nil? ww)))
        ff (filter #(not (cstr/starts-with? (str %) ":styler_")) (keys (first @rowset))) ;; fields
        selected-field-idx (.indexOf ff column-selected)
        col-names (reverse (into () @(ut/tracked-subscribe [::col-names])))
        col-selected? (> selected-field-idx -1)
        ff (if (and col-selected? (and (not (nil? col-names)) (ut/ne? col-names))) col-names ff)
        selected-field-code (get-in @(ut/tracked-sub ::panel-sql-calls {:panel-key selected-block})
                                    [query-key :select selected-field-idx])
        fields (if hide? (cset/difference (set ff) (set hide-fields)) ff)
        child-cols (cset/intersection (set child-parts) (set fields))
        fields-cnt (count fields)
        clicked-row @(ut/tracked-subscribe [::conn/clicked-parameter data-keypath])
        ;;clicked-row @(ut/tracked-sub ::conn/clicked-parameter-alpha {:keypath data-keypath})
        equal-width (js/Math.floor (/ modded-width fields-cnt))
        equal-width-min 100
        row-height (if (not (nil? row-height)) row-height 25) ;50 ;100 ;25 ;25
        metadata @(ut/tracked-sub ::conn/sql-metadata-alpha {:keypath data-keypath})
        post-metadata @(ut/tracked-sub ::conn/sql-post-metadata-alpha {:keypath data-keypath})
        stylers @(ut/tracked-subscribe [::stylers panel-key query-key])
        rowcount (get metadata :rowcount)
        history-rowset? (cstr/includes? (str query-key) "-hist-")
        full-rowcount (get-in post-metadata [:* :rowcount])
        rows-per-page (if (= page-num -1) full-rowcount (if history-rowset? 50 200))
        curr-row-start (+ (- (* page-num rows-per-page) rows-per-page) 1)
        curr-row-end (+ rows-per-page curr-row-start)
        curr-row-str (str curr-row-start " - " (if (> full-rowcount curr-row-end) curr-row-end full-rowcount))
        ttl-pages (nf (js/Math.ceil (/ (or full-rowcount rowcount) rows-per-page)))
        rowcount-string
        (cond
          (= rowcount 1)  "1 row"
          (= page-num -1) (str (nf rowcount) " rows (limit off!)")
          (and (or (> full-rowcount rows-per-page) (> rowcount rows-per-page)) (nil? full-rowcount))
          (str curr-row-str " of " (nf rowcount) "+ rows")
          (and (or (> full-rowcount rows-per-page) (< rowcount rows-per-page)) (nil? full-rowcount)) (str (nf rowcount) " rows")
          :else           (str (if (> full-rowcount rows-per-page) (str curr-row-str " of ") "") (nf full-rowcount) " rows"))
        modded-height (- height-int 155)
        max-rows (+ (js/Math.floor (/ modded-height row-height))
                    (if (and (= @formula-bar data-keypath) selected? (not non-panel?)) 1 2))
        equal-width-final (if (<= equal-width-min equal-width) equal-width equal-width-min)
        table-source (get-in panel-map [:queries query-key :from])
        drag-meta (get @dragging-body :drag-meta)
        connection-id @(ut/tracked-sub ::connection-id-alpha {:panel-key panel-key})
        relevant-for-dyn-drop? (if @dragging?
                                 (and (or (= (get drag-meta :connection-id) connection-id)
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
        callout-instead? (and (= rowcount 1) (= fields-cnt 1))
        mad-libs? (= query-key @mad-libs-view)
        overlay? (or mad-libs?
                     (and (not non-panel?) ;; no render on editor panels?
                          @dragging?
                          relevant-for-dyn-drop?))
        query (get-in panel-map [:queries query-key])
        panel-conn-id @(ut/tracked-subscribe [::panel-connection-id panel-key])
        has-rabbit-code? (true? (some (fn [x] (= x "rabbit-code")) (ut/deep-flatten (get metadata :fields))))
        clicked-rabbit-code-multiplier 4
        clicked-row-height (if (not (nil? clicked-row-height)) clicked-row-height (* row-height clicked-rabbit-code-multiplier))
        sys-panel? (cstr/ends-with? (str query-key) "-sys")
        text-color (if non-panel? (theme-pull :theme/editor-grid-font-color nil) (theme-pull :theme/grid-font-color nil))
        selected-text-color
        (if non-panel? (theme-pull :theme/editor-grid-selected-font-color nil) (theme-pull :theme/grid-selected-font-color nil))
        selected-background-color (if non-panel?
                                    (theme-pull :theme/editor-grid-selected-background-color nil)
                                    (theme-pull :theme/grid-selected-background-color nil))
        hover-field-enable? (true? (and (not @dragging?)
                                        (not @over-flow?)
                                        (not @db/dragging-flow-editor?)
                                        (not @dragging-editor?)
                                        (not @mouse-dragging-panel?)))
        hovered-field-name [@hover-field @animate? @db/context-box] 
        has-pages? (and (>= full-rowcount rows-per-page) (or (> page-num 0) (nil? page-num)) (not (= page-num -1)))
        last-page? (and has-pages? (= page-num (js/Math.ceil (/ full-rowcount rows-per-page))))
        first-page? (and has-pages? (or (nil? page-num) (= page-num 1)))
        [waits? single-wait?] @(ut/tracked-subscribe [::query-waitings query-key])
        ;;deep-meta-running?  (some #(= % query-key) @db/running-deep-meta-on)  
        default-col-widths @(ut/tracked-sub ::column-default-widths {:panel-key panel-key :query-key query-key})
        running? single-wait?
        double-click-timeout 400]
    (when (not= default-col-widths equal-width-final)
      (ut/tracked-dispatch [::set-column-default-widths panel-key query-key equal-width-final])) ;; bad
    (if (seq @rowset) ;(and (seq @rowset) (not running?))
      (if (and callout-instead? (nil? hh))
        (let [label         (nth fields 0)
              md            (get-in metadata [:fields label :data-type])
              num?          (or (= md "integer") (= md "float"))
              value         (get-in @rowset [0 label])
              formatted-val (if num? (str (nf value)) (str value))
              len           (count (str formatted-val))
              charw         (js/Math.ceil (/ (* (js/Math.ceil (/ (+ h w 1) 2)) db/brick-size) len))
              pxsize        (px charw)
              labelsz       (* height-int 0.1)
              agg?          false ;(not (get-in metadata [:fields label :group-by?]))
              pxlabel       (px (if (< labelsz 20) 20 labelsz))]
          ^{:key (str "magic-table-callout-parent" query-key)}
          [re-com/v-box :size "auto" :style {:overflow "hidden"} :align :center :justify :center :class
           (when (and (= panel-key (last @hover-field)) @animate?) "zoom-and-fade2") :children
           [(when (and overlay? (not mad-libs?))
              ;@(ut/tracked-subscribe [::dynamic-drop-targets table-source @dragging-body panel-key query-key width-int height-int])
              @(ut/tracked-sub ::dynamic-drop-targets-alpha {:table-source table-source :dbody @dragging-body :panel-key panel-key :query-key query-key :width-int width-int :height-int height-int}))
            [re-com/box :child (str label) :size "none" :height (px (* height-int 0.1)) :style
             {:font-weight 700
              :transition  "all 0.6s ease-in-out"
              :filter      (if overlay? "blur(3px) opacity(88)" "none")
              :opacity     0.6
              :font-size   pxlabel}]
            [re-com/box :align :center :justify :center :child formatted-val :attr
             (if agg? {} ;; disabling double click drill down for now due to it overreacting
              ;;  {:on-double-click
              ;;   (fn [_]
              ;;     (let [query-key  (keyword (str (ut/replacer (str query-key) #":" "")
              ;;                                    "->-"
              ;;                                    (ut/replacer (str (first @hover-field)) #":" "")))
              ;;           base-table (get-in table-source [0 0])
              ;;           is-sub?    (true? (cstr/starts-with? (str base-table) ":query/"))
              ;;           new-query  (cond is-sub?           (try (sql-alias-replace-sub base-table)
              ;;                                                   (catch :default _
              ;;                                                     {:select [["error in sql-alias-replace-sub1" :err]]}))
              ;;                            (map? base-table) (try (sql-alias-replace base-table)
              ;;                                                   (catch :default _
              ;;                                                     {:select [["error in sql-alias-replace-sub2" :err]]}))
              ;;                            :else             {:select [:*] :from table-source})]
              ;;       (reset! animate? true)
              ;;       (ut/tracked-dispatch [::add-query panel-key query-key new-query])
              ;;       (js/setTimeout #(do ;; change view tab
              ;;                         (ut/tracked-dispatch [::select-view panel-key query-key])
              ;;                         (reset! animate? false))
              ;;                      1000)))}
                 {})
             :size "auto"
             :style {:font-size       pxsize
                     :cursor          (when agg? "pointer")
                     :transition      "all 0.6s ease-in-out"
                     ;:text-decoration (when agg? "underline")
                     :filter          (if overlay? "blur(3px) opacity(88)" "none")
                     :font-weight     700}]]])
        ^{:key (str "magic-table-base-wrapper" query-key)}
        [re-com/v-box :style {:margin-top "-8px"} :children
         [(when (and overlay? (not mad-libs?))
           ; @(ut/tracked-subscribe [::dynamic-drop-targets table-source @dragging-body panel-key query-key width-int height-int])
            @(ut/tracked-sub ::dynamic-drop-targets-alpha {:table-source table-source :dbody @dragging-body :panel-key panel-key :query-key query-key :width-int width-int :height-int height-int}))
          (when (and overlay? mad-libs? (not non-panel?)) ;;; MAD LIBS
            [re-com/box :child [mad-libs-shapes query-key width-int height-int] :size "none" :align :center :justify :center
             :height (px (- height-int 40)) :width (px width-int) :style {:position "fixed" :z-index 100}])
          ^{:key (str "magic-table-base-" data-keypath)}
          [re-com.core/simple-v-table :columns
           (vec
            (for [c fields]
              (let [cwidth     (cond (cstr/ends-with? (str query-key) "-sys") ;; TODO, convert to
                                     (cond (some #(= % c) [:queries :fields :views :blocks :recos :combo_hash :schema_cat]) 66
                                           (= c :database_name) 115
                                           (= c :description) 550
                                           (and (= c :name) (cstr/includes? (str query-key) "-fn-")) 145
                                           (= c :shape_name) 180
                                           (= c :block_key) 88
                                           (= c :items) 70
                                           (= c :item_key) 300
                                           (= c :table_name) 204
                                           (or (= c :shape) (= c :table)) 204
                                           (= c :query_map) 282
                                           (and (some #(= % c) [:screen_name :block_name]) (= data-keypath [:blocks-sys])) 298
                                           (and (some #(= % c) [:screen_name :block_name])
                                                (not (= data-keypath [:blocks-sys])))
                                           385
                                           :else equal-width-final)
                                     (some #(= % c) (keys custom-col-widths)) (get custom-col-widths c) ;; :col-wdiths
                                                                                                               ;; keys exists
                                                                                                               ;; in
                                     :else                                          equal-width-final)
                    row-num-fn (fn [row] (try (.indexOf @rowset row) (catch :default _ -1)))]
                {:id c
                 :header-label (str c)
                 :distinct (let [q (get-in metadata [:fields c :distinct])
                                 f (get-in post-metadata [c :distinct])]
                             (if f [true f] [false q]))
                 :upstream? (some #(= % c) child-cols)
                 :data-type (get-in metadata [:fields c :data-type])
                 :color (get @(ut/tracked-subscribe [::conn/data-colors]) (get-in metadata [:fields c :data-type]))
                 :row-label-fn
                 #(if (= % clicked-row)
                    (let [row-hash   (str (hash %) c)
                          row-height (if has-rabbit-code? clicked-row-height row-height)
                          row-num    (row-num-fn %)
                          hh         (js/Math.floor (/ row-height db/brick-size))
                          ww         (js/Math.floor (/ cwidth db/brick-size))]
                      (cond (= panel-key :system-tables-list*) (draggable (sql-spawner-meta :meta-tables)
                                                                          "meta-menu"
                                                                          [re-com/box :height (px row-height)     ;"22px"
                                                                           :style {:color "#000000"} :child (str (get % c))])
                            (= panel-key :searches-rows-sys-list*) (draggable (item-subscription-spawner-meta)
                                                                              "meta-menu"
                                                                              [re-com/box :height (px row-height) ;"22px"
                                                                               :style {:color "#000000"} :child
                                                                               (str (get % c))])
                            (= panel-key :system-fields-list*) (draggable (sql-spawner-meta :meta-fields)
                                                                          "meta-menu"
                                                                          [re-com/box :height (px row-height)     ;"22px"
                                                                           :style {:color "#000000"} :child (str (get % c))])
                            (= panel-key :recos-list*) (draggable (sql-spawner-meta :viz-reco)
                                                                  "meta-menu"
                                                                  [re-com/box :height (px row-height)             ;"22px"
                                                                   :style {:color "#000000"} :child (str (get % c))])
                            (= panel-key :files-list*) (draggable (sql-spawner-meta :meta-screens)
                                                                  "meta-menu"
                                                                  [re-com/box :height (px row-height)             ;"22px"
                                                                   :style {:color "#000000"} :child (str (get % c))])
                            (= panel-key :blocks-list*) (draggable (sql-spawner-meta :meta-blocks)
                                                                   "meta-menu"
                                                                   [re-com/box :height (px row-height)            ;"22px"
                                                                    :style {:color "#000000"} :child (str (get % c))])
                            (and (= (get-in metadata [:fields c :data-type]) "rabbit-code") true)
                            (draggable (sql-spawner-where :where panel-map data-keypath c panel-key row-num)
                                       "meta-menu"
                                       (let [rowname (str "query-preview-block" row-hash)
                                             qname   (str "query-preview-inviz" row-hash)]
                                         (when (and (= (get-in % [c 0]) :*render*)
                                                    (empty? @(ut/tracked-subscribe [::panel-map (keyword rowname)])))
                                           (ut/tracked-dispatch
                                            [::update-workspace [(keyword rowname)]
                                             (ut/postwalk-replacer
                                              {:gen-viz-609 (keyword qname)}
                                              (let [render-code (get-in % [c 1])
                                                    is-map?     (and (map? render-code) (contains? render-code :view))
                                                    base        {:h             hh
                                                                 :w             ww
                                                                 :connection-id panel-conn-id
                                                                 :root          [38 1] ;; needs unique?
                                                                 :tab           "strategic grill locations"
                                                                 :name          rowname
                                                                 :views         {:a-row render-code}
                                                                 :queries       {}}
                                                    body        (if is-map?
                                                                  (-> base
                                                                      (assoc-in [:views :a-row] (get render-code :view))
                                                                      (assoc-in [:queries] (get render-code :queries)))
                                                                  base)]
                                                body))]))
                                         [re-com/box :size "auto" :height (px (+ row-height 20)) ;"33px"
                                                                                                      ;;"30px"
                                          :align :start :justify :start :style
                                          {;:margin-top "-8px" :margin-top "0px"
                                           :zoom        0.65
                                           :padding-top "0px"} :child
                                          (if (= (get-in % [c 0]) :*render*)
                                            [re-com/box :size "none" :width (px cwidth) :height (px row-height) :child
                                             [honeycomb (keyword rowname) :a-row]]
                                            [shape/map-boxes2 (get-in % [c 1]) :inline-render :inline-render [] nil
                                             (if (vector? (get-in % [c 1])) "vector" "map")])]))
                            :else (draggable
                                   (sql-spawner-where :where panel-map data-keypath c panel-key row-num)
                                   "meta-menu"
                                   [re-com/box :size "none" :attr
                                    {;:on-mouse-enter (fn [_] (when (and (not (= @hover-field [c
                                     :on-mouse-over  (fn [_]
                                                       (when (and (not (= @hover-field [c panel-key])) hover-field-enable?)
                                                         (reset! hover-field [c panel-key])))
                                     :on-mouse-leave (fn [_]
                                                       (when (and (not (= @hover-field [c panel-key])) hover-field-enable?)
                                                         (reset! hover-field nil)))} :class
                                    (when (and (= panel-key (last @hover-field)) @animate?) "zoom-and-fade") :style
                                    {:color "inherit"} :child ;(str (get % c))
                                    (if (and (integer? (get % c)) (not (cstr/includes? (cstr/lower-case (str c)) "year")))
                                      (str (nf (get % c)))
                                      (str (get % c)))])))
                    (cond
                      (and (= (get-in metadata [:fields c :data-type]) "rabbit-code") true)
                      (let [row-hash (str (hash %) c)
                            hh       (js/Math.floor (/ row-height db/brick-size))
                            ww       (js/Math.floor (/ cwidth db/brick-size))
                            rowname  (str "query-preview-block" row-hash)
                            qname    (str "query-preview-inviz" row-hash)]
                        (when (and (= (get-in % [c 0]) :*render*)
                                   (empty? @(ut/tracked-subscribe [::panel-map (keyword rowname)])))
                          (ut/tracked-dispatch
                           [::update-workspace [(keyword rowname)]
                            (ut/postwalk-replacer {:gen-viz-609 (keyword qname)}
                                                  (let [render-code (get-in % [c 1])
                                                        is-map?     (and (map? render-code) (contains? render-code :view))
                                                        base        {:h             hh
                                                                     :w             ww
                                                                     :connection-id panel-conn-id
                                                                     :root          [38 1] ;; needs unique?
                                                                     :tab           "strategic grill locations"
                                                                     :name          rowname
                                                                     :views         {:a-row render-code}
                                                                     :queries       {}}
                                                        body        (if is-map?
                                                                      (-> base
                                                                          (assoc-in [:views :a-row] (get render-code :view))
                                                                          (assoc-in [:queries] (get render-code :queries)))
                                                                      base)]
                                                    body))]))
                        [re-com/box :size "auto" :height (px (+ row-height 20)) ;"33px" ;"30px"
                         :align :start :justify :start :style {:zoom 0.65 :padding-top "0px"} :child
                         (if (= (get-in % [c 0]) :*render*)
                           [re-com/box :size "none" :width (px cwidth) :height (px row-height) :child
                            [honeycomb (keyword rowname) :a-row]]
                           [shape/map-boxes2 (get-in % [c 1]) :inline-render :inline-render [] nil
                            (if (vector? (get-in % [c 1])) "vector" "map")])])
                      (nil? (get % c)) [re-com/box :child "NULL" :attr
                                        {:on-context-menu (fn [_]
                                                            (ut/tracked-dispatch
                                                             [::conn/cell-click-parameter
                                                              [(keyword (str (ut/replacer (str query-key) #":" "") ".*"))
                                                               (keyword (ut/replacer (str c) #":" ""))] (get % c)]))} :style
                                        {:opacity 0.35 :font-style "italic"}]
                      (and (string? (get % c)) (ut/is-hex-color? (str (get % c))))
                      [re-com/box :align :center :justify :center :child (str (get % c)) :style
                       {:background-color (str (get % c)) :color (ut/invert-hex-color (str (get % c))) :font-weight 400}]
                      (= false (get % c)) [re-com/box :child (str (get % c)) :style
                                           {:background-color "#abc4de44" :padding-left "4px"}]
                      (= true (get % c)) [re-com/box :child (str (get % c)) :style
                                          {:background-color "#79167955" :padding-left "4px"}]
                      (and (= c :query_map) (cstr/includes? (get % c) "{:select"))
                      (try (str (get-in (read-string (get % c)) [0 :select])) (catch :default _ "error w query_map render"))
                      (and (integer? (get % c)) (not (cstr/includes? (cstr/lower-case (str c)) "year")))
                      [re-com/box :attr
                       {:on-context-menu (fn [_]
                                           (ut/tracked-dispatch [::conn/cell-click-parameter
                                                                 [(keyword (str (ut/replacer (str query-key) #":" "") ".*"))
                                                                  (keyword (ut/replacer (str c) #":" ""))] (get % c)]))}
                       :child (str (nf (get % c)))]
                      :else [re-com/box :child (str (get % c)) :attr
                             {:on-context-menu (fn [_]
                                                 (ut/tracked-dispatch [::conn/cell-click-parameter
                                                                       [(keyword (str (ut/replacer (str query-key) #":" "")
                                                                                      ".*"))
                                                                        (keyword (ut/replacer (str c) #":" ""))] (get % c)]))
                              :on-mouse-over   (fn [_]
                                                 (when (and (not (= @hover-field [c panel-key])) hover-field-enable?)
                                                   (reset! hover-field [c panel-key])))
                              :on-mouse-leave  (fn [_]
                                                 (when (and (not (= @hover-field [c panel-key])) hover-field-enable?)
                                                   (reset! hover-field nil)))} :size "none"]))
                 :width cwidth}))) ;187
           :on-click-row (fn [x]
                           (if false  ;disable double click drill for now cause its annoying & inconsistent
                                      ;(and (not (get-in metadata [:fields (first @hover-field) :group-by?]))  false 
                                      ;     (not (nil? (first @hover-field)))
                                      ;     (= clicked-row (get @rowset x))) ;; already selected
                             (let [query-key       (keyword (str (ut/replacer (str query-key) #":" "")
                                                                 "->-"
                                                                 (ut/replacer (str (first @hover-field)) #":" "")))
                                   base-table      (get-in table-source [0 0])
                                   is-sub?         (true? (cstr/starts-with? (str base-table) ":query/"))
                                   group-bys       @(ut/tracked-subscribe [::group-bys query-key])
                                   clicked-keyword (keyword (str (ut/replacer query-key #":" "") "/*.clicked"))
                                   new-query       (cond is-sub?           (try (sql-alias-replace base-table)
                                                                                (catch :default _
                                                                                  {:select [["error in sql-alias-replace-sub1" :err]]}))
                                                         (map? base-table) (try (sql-alias-replace base-table)
                                                                                (catch :default _
                                                                                  {:select [["error in sql-alias-replace-sub2" :err]]}))
                                                         :else             {:select [:*] :from table-source})
                                   new-query       (assoc new-query :where [:*when clicked-keyword [:*all= clicked-keyword group-bys]])]
                               (reset! animate? true)
                               (ut/tracked-dispatch [::add-query panel-key query-key new-query])
                               (js/setTimeout #(do ;; change view tab
                                                 (ut/tracked-dispatch [::select-view panel-key query-key])
                                                 (reset! animate? false))
                                              1000))
                             (ut/tracked-dispatch-sync [::conn/click-parameter data-keypath (get @rowset x)])))
           :cell-style (fn [row {:keys [id] :as column}]
                         (let [;this-row (.indexOf @rowset row)
                               row-num          (try (.indexOf @rowset row) (catch :default _ -1))
                               cell-alias       (keyword (str (-> (str id)
                                                                  (ut/replacer #":" ""))
                                                              "."
                                                              row-num))
                               cell-used?       (true? (some #(= % cell-alias) parameters-used-in-sql))
                               multi-cell-used? (true? (some #(and (= (first %) id) (some (fn [x] (= x (get row id))) (last %)))
                                                             parameters-used-in-cell)) ;; parameters-used-in-cell
                               sel?             (and selected? @(ut/tracked-subscribe [::column-selected? panel-key query-key id]))
                               poss-stylers     (filter #(cstr/starts-with? (str %) ":styler_") (keys row))
                               styler-keys      (sort (remove nil?
                                                              (for [s poss-stylers]
                                                                (when (= (get row s) 1)
                                                                  (keyword (-> (ut/safe-name s)
                                                                               (ut/replacer #"_" "-")
                                                                               (ut/replacer #"styler-" "")))))))
                               heatmap-styles   (first (remove nil?
                                                               (for [s styler-keys]
                                                                 (when (and (some #(= :heatmap %) (ut/deep-flatten (get-in stylers [s :style])))
                                                                            (or (= (get-in stylers [s :cols]) id)
                                                                                (try (some #(= id %) (get-in stylers [s :cols]))
                                                                                     (catch :default e false))))
                                                                   s))))
                  ;;  heat-walk-map    (fn [obody]
                  ;;                     (let [kps       (into {} (for [p (ut/kvpaths obody)] {p (get-in obody p)}))
                  ;;                           logic-kps (into {}
                  ;;                                           (for [[_ v] (into {}
                  ;;                                                             (filter #(cstr/starts-with? (str (last %))
                  ;;                                                                                         "[:heatmap")
                  ;;                                                                     kps))]
                  ;;                                             (let [[_ scheme depth direction] v]
                  ;;                                               ;(tapp>> [:ss scheme depth direction])
                  ;;                                               {v @(ut/tracked-subscribe [::heatmap-color-val panel-key query-key
                  ;;                                                                          id (get row id) heatmap-styles scheme
                  ;;                                                                          depth direction])})))]
                  ;;                       (ut/postwalk-replacer logic-kps obody)))

                               heat-walk-map  (fn [obody]
                                                (let [kps       (ut/extract-patterns obody :heatmap 4)
                                                      logic-kps (into {} (for [v kps] (let [[_ scheme depth direction] v]
                                                                                        {v @(ut/tracked-subscribe [::heatmap-color-val panel-key query-key
                                                                                                                   id (get row id) heatmap-styles scheme
                                                                                                                   depth direction])})))]
                                                  (ut/postwalk-replacer logic-kps obody)))

                               styles           (apply merge
                                                       (for [s styler-keys]
                                                         (if (or (= (get-in stylers [s :cols]) id)
                                                                 (try (some #(= id %) (get-in stylers [s :cols])) (catch :default _ false)))
                                                           (get-in stylers [s :style])
                                                           {})))
                               styles           (if heatmap-styles (heat-walk-map styles) styles)
                               data-type-color  (get @(ut/tracked-subscribe [::conn/data-colors]) (get-in metadata [:fields id :data-type]))
                               agg?             (and (not non-panel?) (not (get-in metadata [:fields id :group-by?])))
                               used-in-a-query? (true? (some #(= id %) parameters-used-in-sql))
                               clicked-row?     (= row clicked-row)] ;; color on row doesnt work,
                           (merge (cond (or (= row clicked-row)
                                            (and (= panel-key :recos-list*) ;; special case with viz browser
                                                 (= (get row :combo_hash)
                                                    @(ut/tracked-sub ::conn/clicked-parameter-key-alpha {:keypath [:recos-sys/combo_hash]}))))
                                        (if has-rabbit-code?
                                          {:border-top       (str "2px solid " selected-background-color) ; "#9973e0"
                                           :border-bottom    (str "2px solid " selected-background-color) ; "#9973e0"
                                           :padding          "3px"
                                           :margin           "0px"
                                           :background-color (str selected-background-color 11)}
                                          {:background-color selected-background-color ; "#9973e0"
                                           :padding          "3px"
                                           :margin           "0px"
                                           :color            selected-text-color})
                                        styles (merge ;row-styles
                                                {;:background-color "#00000011"
                                                 :padding "3px"
                                                 :margin  "0px"
                                                 :color   (str text-color 72)} ;"#ffffff72"
                                                styles)
                                        :else (let [upstream? (some (fn [x] (= x id)) child-cols)
                                                    fcolor    (get @(ut/tracked-subscribe [::conn/data-colors])
                                                                   (get-in metadata [:fields id :data-type]))]
                                                (merge {:background-color (if upstream? (str fcolor (if deeper-upstream? 10 20)) "#00000000")
                                                        :padding          "3px"
                                                        :margin           "0px"}
                                                       (if upstream?
                                                         {;:border (str "1px solid " (str fcolor (if deeper-upstream? 15
                                                          :color        (str text-color)
                                                          :filter       "brightness(200%)"
                                                          :border-left  (str "1px solid " (str fcolor (if deeper-upstream? 15 39)))
                                                          :border-right (str "1px solid " (str fcolor (if deeper-upstream? 15 39)))
                                                          :border-top   (str "1px solid " (str fcolor (if deeper-upstream? 15 39)))}
                                                         {:border-left  (str "1px solid " text-color "08") ;"1px solid
                                                          :border-right (str "1px solid " text-color "08") ;"1px solid
                                                          :color        (str text-color 72)}))))
                                  (when (and clicked-row? used-in-a-query?) {:filter "hue-rotate(90deg)"})
                                  (if cell-used?
                                    {:color            selected-text-color
                                     :border           (str "1px solid " (theme-pull :theme/editor-outer-rim-color nil))
                                     :border-bottom    (str "2px solid " (theme-pull :theme/editor-outer-rim-color nil))
                                     :background-color (str selected-background-color 52)}
                                    {})
                                  (if multi-cell-used?
                                    {:color      selected-text-color
                                     :background (str "repeating-linear-gradient(45deg, "
                                                      selected-background-color
                                                      "55, "
                                                      selected-background-color
                                                      "55 10px, "
                                                      selected-background-color
                                                      "33 10px, "
                                                      selected-background-color
                                                      "33 20px)")}
                                    {})
                                  (if sel? ;; {:background "repeating-linear-gradient(45deg, #606dbc88,
                                    (merge
                                     (theme-pull :theme/grid-selected-column-css nil)
                                     {:border-left      (str "2px solid " data-type-color)
                                      :border-right     (str "2px solid " data-type-color)
                                      :background-color (str data-type-color 15)
                                      :height           (px (if (and clicked-row? has-rabbit-code?) (- clicked-row-height 5) row-height))})
                                    {:height (px (if (and clicked-row? has-rabbit-code?) (- clicked-row-height 5) row-height))})
                      ;;(when agg? {:text-decoration "underline"})
))) ;; temp
           :row-height row-height ; 444 ;row-height
           :max-rows max-rows :max-width (px (+ 25 width-int)) :table-row-line-color (str text-color 11) ;;"#ffffff11"
           :row-style
           (fn [row {:keys [id] :as column}]
             (let [;this-row (.indexOf @rowset row)
                   poss-stylers (filter #(cstr/starts-with? (str %) ":styler_") (keys row))
                   styler-keys  (sort (remove nil?
                                              (for [s poss-stylers]
                                                (when (= (get row s) 1)
                                                  (keyword (-> (ut/safe-name s)
                                                               (ut/replacer #"_" "-")
                                                               (ut/replacer #"styler-" "")))))))
                   styles       (apply merge
                                       (for [s styler-keys]
                                         (if (or (= (get-in stylers [s :cols]) :*) (= (get-in stylers [s :cols]) [:*]))
                                           (get-in stylers [s :style])
                                           {})))
                   base         {}] ;; merge for all
               (cond (and (= row clicked-row) has-rabbit-code?) {;:height "299px" :overflow "auto"
                                                                 ;:color selected-background-color
                                                                 :height (px clicked-row-height)}
                     (or (= row clicked-row)
                         (and (= panel-key :recos-list*) ;; special case with viz browser
                              (= (get row :combo_hash)
                                 @(ut/tracked-sub ::conn/clicked-parameter-key-alpha {:keypath [:recos-sys/combo_hash]}))))
                     {:background-color selected-background-color ;"#bc798c" ; "#ec8250"
                      :color            selected-text-color ;"#000000"
                      :padding          "0px"}
                     styles (merge {:padding "0px"} styles)
                     :else {:padding "0px"}))) :table-padding 5 :fixed-column-border-color "#00000011" :column-header-height 39
           :column-header-renderer
           (fn [cols parts sort-by-col]
             [re-com/h-box :children
              (for [c cols]
                (let [w          (get c :width)
                      id         (get c :id)
                      dst        (nf (nth (get c :distinct) 1))
                      full-meta? (nth (get c :distinct) 0)
                      inv?       (get c :upstream?)
                      color      (get c :color)
                      typ        (get c :data-type)
                      sel?       (and selected? @(ut/tracked-subscribe [::column-selected? panel-key query-key id]))
                      label      (get c :header-label)]
                  [re-com/box :attr
                   (if selected? {:on-click #(ut/tracked-dispatch [::column-select panel-key query-key (if sel? nil id)])} {})
                   :child
                   (draggable (sql-spawner :field panel-map data-keypath id panel-key)
                              "meta-menu"
                              [re-com/v-box :justify :center :height "39px" :style
                               (merge {;:color "#000000" ;(str color 99)
                                       :color            (str color)
                                       :filter           (when inv? "brightness(200%)")
                                       :background-color (cond inv? (str color (if deeper-upstream? 15 25))
                                                               :else "inherit")
                                       :border           (str "2px solid " color (when (not sel?) "39"))
                                       :font-weight      700}
                                      (if sel? (theme-pull :theme/grid-selected-column-css nil) {})) :padding "5px" :children
                               [[re-com/box :child
                                 (let [uppers?   (ut/all-uppers? (ut/safe-name label))
                                       spc-per   (if uppers? 11 9) ;; 9 7
                                       crs       (* spc-per (count label))
                                       overflow? (> crs w)
                                       space     (int (/ w spc-per))]
                                   (if overflow? (str (subs (str label) 1 space) "..") (str label)))]
                                [re-com/h-box :style {:font-size "9px" :font-weight 700 :opacity 0.9} :gap "5px" :children
                                 [[re-com/box :child (str typ " - ")]
                                  [re-com/box :style (if (not full-meta?) {:font-style "italic"} {}) :child
                                   (str dst (when (not full-meta?) "*"))]]]] :width (px w)])]))]) :parts
           {:simple-wrapper {:style {:padding-bottom   "0px"
                                     :height           (px (- height-int 60))
                                     :margin-bottom    "-5px"
                                     :filter           (if (and overlay?
                                                                (and ;(not mad-libs?)
                                                                 (not non-panel?))) ;; non-panel
                                                                                     ;; mad-libs we
                                                         "blur(3px) opacity(60%)"
                                                         "none")
                                     :font-weight      700
                                     :background-color "#00000000"}} ;;; questions!
            :v-scroll       {:style       {:background-color "#00000000"}
                             :thumb-style {:background-color (str text-color 11) ;; "#ffffff11"
                                           :drag-color       "#791679"
                                           :hover-color      "#8c78b789"}}
            :h-scroll       {:style       {:background-color "#00000000" :color "green"}
                             :thumb-style {:background-color (str text-color 11) ;;"#ffffff11"
                                           :drag-color       "#791679"
                                           :hover-color      "#8c78b789"}}} :model rowset]
          [re-com/h-box 
           :padding "4px" 
           :justify :between 
           ;:align :center
           :children
           (vec (into [;; panel-code-box [panel-id key width-int height-int value]
                  (if (and (= @formula-bar data-keypath) selected? (not non-panel?))
                    (let [block-h        (or hh h)
                          block-w        (or ww w)
                          label-px-width (js/Math.floor (* db/brick-size (- block-w 0.45)))]
                      (ut/tapp>> [:formula-bar-open? panel-key query-key block-h block-w column-selected selected-field-idx ff])
                      [re-com/h-box :size "auto" :style {:filter (if overlay? "blur(3px) opacity(88)" "none") :margin-top "12px"}
                       :justify :between :children
                       [[re-com/v-box :size "auto" :children
                         [;; [re-com/box :height "15px"
                          [field-code-box panel-key query-key selected-field-idx label-px-width 55 selected-field-code]]]]])
                    (let [;; waitings (filter #(and (> (count (get % :ui-keypath)) 1)
                          reco-wait?   @(ut/tracked-subscribe [::reco-running? query-key :reco])
                          reco-queued? @(ut/tracked-subscribe [::reco-queued? query-key :reco])]
                      [re-com/h-box :size "none" :height "10px" :padding "3px" :style
                       {:font-size "12px" :filter (if overlay? "blur(3px) opacity(88)" "none")} :children
                       [[re-com/h-box :gap "5px" :children
                         (into
                          [[re-com/box :child rowcount-string]
                           [re-com/gap :size "0px"] ;; the 2 gaps are
                                                                                 ;; enough
                           [re-com/md-icon-button :md-icon-name "zmdi-refresh" :on-click
                            #(ut/tracked-dispatch [::conn/clear-query-history query-key]) :attr
                            (when hover-field-enable?
                       ;{:on-mouse-enter #(swap! db/context-box assoc query-key "execute: (re)run query")
                       ; :on-mouse-leave #(swap! db/context-box dissoc query-key)}
                              {:on-mouse-enter #(reset! db/bar-hover-text "execute: (re)run query")
                               :on-mouse-leave #(reset! db/bar-hover-text nil)})
                            :class (when single-wait? "rotate linear infinite")
                            :style {:font-size        "15px"
                                    :cursor           "pointer"
                                    :transform-origin "7.5px 10px"
                                    :opacity          0.5
                                    :padding          "0px"
                                    :margin-top       "-3px"}]
                           (when (and ;;(> full-rowcount 5) ;; TODO, gets messed up sometimes?
                                  (not non-panel?))
                             [re-com/md-icon-button
                              :md-icon-name "zmdi-toll"
                              :on-click  #(ut/tracked-dispatch [::conn/run-sql-deep-meta-for panel-key query-key (sql-alias-replace query)])
                              :attr (when hover-field-enable?
                              ;;  {:on-mouse-enter #(swap! db/context-box assoc query-key "meta: (re)run full counts of unique values for each column")
                              ;;   :on-mouse-leave #(swap! db/context-box dissoc query-key)}
                                      {:on-mouse-enter #(reset! db/bar-hover-text "meta: (re)run full counts of unique values for each column")
                                       :on-mouse-leave #(reset! db/bar-hover-text nil)})
                       ;;:class (when deep-meta-running? "rotate linear infinite")
                              :style {:font-size        "15px"
                                      :cursor           "pointer"
                                      :transform-origin "7.5px 10px"
                                      :opacity          0.5
                                      :padding          "0px"
                                      :margin-top       "-3px"}])
                           (when (not non-panel?)
                             [re-com/md-icon-button :md-icon-name "zmdi-shape"
                              :on-click #(do (swap! db/sniff-deck assoc query-key :reco)
                                             (ut/tracked-dispatch [::set-reco-queued query-key :reco])
                                             (ut/tracked-dispatch [::conn/clear-query-history query-key]))
                              :attr (when hover-field-enable?
                              ;;  {:on-mouse-enter #(swap! db/context-box assoc query-key "shapes: (re)generate all possible viz combos for this query")
                              ;;   :on-mouse-leave #(swap! db/context-box dissoc query-key)}
                                      {:on-mouse-enter #(reset! db/bar-hover-text "shapes: (re)generate all possible viz combos for this query")
                                       :on-mouse-leave #(reset! db/bar-hover-text nil)})
                              :class (cond reco-wait?   "rotate linear infinite"
                                           reco-queued? "rotate-reverse linear infinite"
                                           :else        nil)
                              :style {:font-size        "15px"
                                      :cursor           "pointer"
                                      :transform-origin "7.5px 10px"
                                      :opacity          0.5
                                      :padding          "0px"
                                      :margin-top       "-3px"}])

                           [re-com/gap :size "6px"]]

                          (let [valid-kits  @(ut/tracked-subscribe [::valid-kits-for {:panel-key panel-key :data-key query-key}])
                                ;;_ (tapp>>  [:valid-kits valid-kits])
                                block-runners  @(ut/tracked-sub ::block-runners {})
                                client-name    @(ut/tracked-sub ::client-name {})]
                            (vec (for [e valid-kits
                                       :let [icon (get-in block-runners [(first e) :kits (last e) :icon])
                                             tooltip (str (get-in block-runners [(first e) :kits (last e) :tooltip] "(missing tooltip)"))
                                             kit-runner-key (str "kit-runner" (hash (str client-name panel-key query-key (first e) (last e))))
                                             running-key  (keyword (str "kit-status/" kit-runner-key ">running?"))
                                             output-key   (keyword (str "kit/" kit-runner-key ">incremental"))
                                             running?     @(ut/tracked-sub ::conn/clicked-parameter-key-alpha {:keypath [running-key]})
                                             trig!        [@waiting?]
                                             class        (cond
                                                            running? "rotate linear infinite"
                                                            (get @waiting? kit-runner-key) "rotate-reverse linear infinite"
                                                            :else "")]]
                                   [re-com/box
                                    :attr {:on-mouse-over #(reset! db/bar-hover-text tooltip)
                                           :on-mouse-leave #(reset! db/bar-hover-text nil)
                                           :on-click (fn []
                                                       (when (not running?)
                                                         (swap! db/kit-run-ids assoc (keyword kit-runner-key) (ut/generate-uuid))
                                                         (swap! waiting? assoc kit-runner-key true)
                                                         (swap! temp-extra-subs conj running-key)
                                                         (swap! temp-extra-subs conj output-key)
                                                         (swap! db/kit-fn-lookup assoc [panel-key query-key] running-key)
                                                         (let [fstr (str "kit-runner " kit-runner-key)
                                                               w    (/ (count fstr) 4.1)]
                                                           (ut/tracked-dispatch
                                                            [::wfx/push   :default
                                                             {:kind       :run-kit
                                                              :kit-keypath e
                                                              :kit-runner-key kit-runner-key
                                                              :panel-key   panel-key
                                                              :data-key    query-key
                                                              :runner      :queries
                                                              :client-name client-name
                                                              :ui-keypath     [:panels panel-key :queries query-key]}])
                                                           (ut/dispatch-delay 800 [::http/insert-alert fstr w 1 5])
                                                           (tapp>> [:clicked-kit running-key])
                                                           (js/setTimeout #(swap! waiting? assoc kit-runner-key false) 5000))))}
                                    :child (render-kit-icon-query icon class)]))))]
                        (when (and server-kits (not non-panel?)) ;; (not non-panel?)
                          [re-com/h-box :gap "5px" :children
                           (vec
                            (cons
                             [re-com/gap :size "6px"]
                             (vec
                              (for [{:keys [description kit-name name icon]} server-kits
                                    :let                                     [wait?   @(ut/tracked-subscribe [::reco-running?
                                                                                                              query-key kit-name])
                                                                              queued? @(ut/tracked-subscribe [::reco-queued?
                                                                                                              query-key kit-name])]]
                                [re-com/md-icon-button :md-icon-name icon :on-click
                                 #(do (swap! db/sniff-deck assoc query-key kit-name)
                                      (ut/tracked-dispatch [::set-reco-queued query-key kit-name])
                                      (ut/tracked-dispatch [::conn/clear-query-history query-key])) :attr
                                 (when hover-field-enable?
                            ;;  {:on-mouse-enter #(swap! db/context-box assoc query-key (str "kit-fn: " name " - " description))
                            ;;   :on-mouse-leave #(swap! db/context-box dissoc query-key)}
                                   {:on-mouse-enter #(reset! db/bar-hover-text (str "kit-fn: " name " - " description))
                                    :on-mouse-leave #(reset! db/bar-hover-text nil)})
                                 :class (cond wait?   "rotate linear infinite"
                                              queued? "rotate-reverse linear infinite"
                                              :else   nil) :style
                                 {:font-size        "15px"
                                  :cursor           "pointer"
                                  :transform-origin "7.5px 10px"
                                  :opacity          0.5
                                  :padding          "0px"
                                  :margin-top       "-3px"}]))))])
                        (when (not non-panel?)
                          [re-com/box
                           :size "auto"
                           :justify :center ;:align :center
                           :style {;:opacity 0.5 
                                   :color (theme-pull :theme/universal-pop-color nil)
                                   :font-weight 700
                             ;:border "1px solid pink"
                                   :width "1330px"
                             ;:margin-top "4px"
                                   :font-size "15px"
                                   :padding-left "20px"}
                           :child
                           (str ;(count waitings) " "
                            (get @db/context-box query-key " "))])]]))
                  [re-com/h-box
                   :children
                   [(when col-selected?
                      (let [;heat-simple (into {} (for [[[k1 _] v] (get-in query [:style-rules])] {k1
                            heat-keys (into {}
                                            (first (for [[[k1 k2] v] (get-in query [:style-rules])
                                                         :when       (and (cstr/starts-with? (str k2) ":heatmap-")
                                                                          (= k1 column-selected))]
                                                     {[k1 k2] v})))
                            has-one?  (ut/ne? heat-keys)]
                        [re-com/h-box :size "none" :height "10px" :padding "3px" :style {:margin-right "10px" :font-size "12px"} :gap
                         "5px" :children
                         [[re-com/md-icon-button :md-icon-name "zmdi-fire" :on-click
                           #(do (ut/tracked-dispatch [::toggle-heatmap panel-key query-key column-selected])
                                (ut/tracked-dispatch [::column-select panel-key query-key nil])) :attr
                           (when hover-field-enable?
                      ;;  {:on-mouse-enter #(swap! db/context-box assoc query-key "toggle heatmap on this column")
                      ;;   :on-mouse-leave #(swap! db/context-box dissoc query-key)}
                             {:on-mouse-enter #(reset! db/bar-hover-text "toggle heatmap on this column")
                              :on-mouse-leave #(reset! db/bar-hover-text nil)})
                           :style {:font-size        "15px"
                                   :color            (if has-one? "orange" "inherit")
                                   :cursor           "pointer"
                                   :transform-origin "7.5px 10px"
                                   :opacity          0.5
                                   :padding          "0px"
                                   :margin-top       "-3px"}]
                          [re-com/md-icon-button :md-icon-name "zmdi-delete" ;; panel-key query-key clause drop-data
                           :on-click
                           #(do (ut/tracked-dispatch [::execute-pill-drop panel-key query-key :remove
                                                      {:drag-body {:target column-selected}}])
                                (ut/tracked-dispatch [::column-select panel-key query-key nil])) :attr
                           (when hover-field-enable?
                      ;;  {:on-mouse-enter #(swap! db/context-box assoc query-key "remove this column from the query")
                      ;;   :on-mouse-leave #(swap! db/context-box dissoc query-key)}
                             {:on-mouse-enter #(reset! db/bar-hover-text "remove this column from the query")
                              :on-mouse-leave #(reset! db/bar-hover-text nil)})
                           :style {:font-size        "15px"
                                   :cursor           "pointer"
                                   :transform-origin "7.5px 10px"
                                   :opacity          0.5
                                   :padding          "0px"
                                   :margin-top       "-3px"}]]]))
                    (when (or non-panel? (and (nil? (get @db/context-box query-key)) (not col-selected?)))
                      [re-com/h-box :style {:filter (if overlay? "blur(3px) opacity(88)" "none")} :children
                       [(when (and has-pages? (not first-page?))
                          [re-com/md-icon-button :md-icon-name "zmdi-caret-left" :on-click
                           #(do (ut/tracked-dispatch [::change-page panel-key query-key (- page-num 1) ttl-pages])
                                (ut/tracked-dispatch [::conn/clear-query-history query-key])) :style
                           {:font-size "22px" :cursor "pointer" :opacity 0.5 :padding "0px" :margin-top "-1px"}])
                        (when has-pages?
                          [re-com/box :align :center :justify :center :style {:font-size "10px"} :child (str page-num " / " ttl-pages)])
                        (if (and has-pages? (not last-page?))
                          [re-com/md-icon-button :md-icon-name "zmdi-caret-right" :on-click
                           #(do (ut/tracked-dispatch [::change-page panel-key query-key (+ page-num 1) ttl-pages])
                                (ut/tracked-dispatch [::conn/clear-query-history query-key])) :style
                           {:font-size "22px" :cursor "pointer" :opacity 0.5 :padding "0px" :margin-top "-1px"}]
                          [re-com/box :child " " :width "22px"])]]) [re-com/gap :size "0px"]]]]

                          [] 
                          ))
                    
                    ]]]) ;; just in case so we don't
                                                                                              ;; render empty child
      [re-com/v-box :padding "20px" :align :center :justify :center :style
       {:font-size "18px" :opacity 0.2 :color (str text-color 77) :font-weight 700} :children
       [[re-com/box :child (if (and running? (not sys-panel?)) "waiting" "no data")]
        (when (not (and running? (not sys-panel?)))
          [re-com/md-icon-button :md-icon-name "zmdi-refresh" :on-click
           #(ut/tracked-dispatch [::conn/clear-query-history query-key])
           :style {:font-size "15px"
                   :cursor "pointer"
                   :transform-origin "7.5px 10px"
                   :padding "0px"
                   :margin-top "-3px"}])]])))





;; (re-frame/reg-sub ::sql-data-table (fn [_ [_ panel-key keypath]] [magic-table panel-key keypath]))

;; (re-frame/reg-sub ::sql-data-table-magic (fn [_ [_ panel-key keypath]] [magic-table panel-key keypath]))

;; (re-frame/reg-sub ::sql-data-table-magic2 (fn [_ [_ panel-key keypath]] [re-com/box :child [magic-table panel-key keypath]]))

;; (re-frame/reg-sub ::sql-data-table-editor
;;                   (fn [_ [_ panel-key keypath block-sizes block-width]] 
;;                     [magic-table panel-key keypath block-sizes block-width]))

(re-frame/reg-sub ::is-single-view?
                  (fn [db [_ panel-key]]
                    (let [body (get-in db [:panels panel-key :views])]
                      (true? (and (not (nil? body))
                                  (or (not (map? body)) (and (map? body) (some #(= % :select) (keys body)))))))))

(re-frame/reg-sub ::multiple-oz-views?
                  (fn [db [_ panel-key]]
                    (let [views    (get-in db [:panels panel-key :views] [])
                          oz-views (remove nil?
                                           (for [[k v] views]
                                             (when (try (= :vega-lite (first v)) (catch :default e false)) ;; for
                                                                                                     ;; legacy
                                                                                                     ;; "single"
                                                                                                     ;; views
                                               k)))
                          oz-cnt   (count (keys oz-views))]
                      (true? (>= oz-cnt 2)))))

(re-frame/reg-sub ::layered-oz-views
                  (fn [db [_ panel-key]]
                    (let [views     (get-in db [:panels panel-key :views] [])
                          oz-layers (vec (remove nil?
                                                 (for [[_ v] views]
                                                   (when (try (= :vega-lite (first v)) (catch :default e false))
                                                     {:encoding (get-in v [1 :layer 0 :encoding])
                                                      :mark     (get-in v [1 :layer 0 :mark])}))))
                          vl-shell  (assoc-in vega-lite-shell [1 :layer] oz-layers)]
                      vl-shell)))

(re-frame/reg-sub
 ::has-no-view?
 (fn [db [_ panel-key]]
   (empty? (get-in db [:panels panel-key :views]))))

(re-frame/reg-sub
 ::has-no-view-alpha?
 (fn [db {:keys [panel-key]}]
   (empty? (get-in db [:panels panel-key :views]))))

(re-frame/reg-sub ::dynamic-tab-selected
                  (fn [db [_ panel-key]]
                    (try (let [tab-res    (get-in db [:post-tab panel-key])
                               vws        (keys (get-in db [:panels panel-key :views]))
                               qys        (keys (get-in db [:panels panel-key :queries]))
                               all-tabs   (set (flatten (conj vws qys)))
                               rule-tabs  (set (for [e (keys (get-in db [:panels panel-key :tab-rules]))] (first e)))
                               tabs-exist (cset/intersection (set (keys tab-res)) all-tabs rule-tabs)
                               valids     (into {}
                                                (for [t tabs-exist]
                                                  {t (apply max
                                                            (flatten (for [r (get-in tab-res [t])]
                                                                       (for [b (last r)] ;; WTF happened with this? no
                                                                         (last (first b))))))}))
                               dsc        (into (sorted-map-by (fn [key1 key2] (compare (key2 valids) (key1 valids)))) valids)
                               winnah     (first (first dsc))
                               query?     (some #(= winnah %) qys)
                               full-alias (keyword
                                           (str (if query? "grid/" "view/") (ut/safe-name panel-key) "." (ut/safe-name winnah)))]
                           (if query? [reecatch [magic-table panel-key [winnah]]] full-alias))
                         (catch :default e nil))))                          ;; just in case... TODO

(re-frame/reg-sub ::view (fn [db [_ panel-key view-key]] (get-in db [:panels panel-key :views view-key])))

(re-frame/reg-sub ::views
                  (fn [db {:keys [panel-key ttype]}]
                    (let [ttype      (or ttype :views)
                          views      (get-in db [:panels panel-key ttype] {})
                          oz-layers  (vec (remove empty?
                                                  (remove nil?
                                                          (for [[_ v] views]
                                                            (when (try (= :vega-lite (first v)) (catch :default _ false))
                                                              {:encoding (get-in v [1 :layer 0 :encoding])
                                                               :data     (get-in v [1 :data])
                                                               :mark     (get-in v [1 :layer 0 :mark])})))))
                          oz-layers? (>= (count oz-layers) 2)]
                      (if (map? views)
                        (merge views ;(get-in db [:panels panel-key :views])
                               (if oz-layers?
                                 {:layered-viz (-> vega-lite-shell
                                                   (assoc-in [1 :layer] oz-layers)
                                                   (assoc-in [1 :resolve] {:scale {:y "independent"}}))} ;; ex: "resolve":
                                 {})
                               {})
                        views))))

(re-frame/reg-sub ::queries-reco-status
                  (fn [db [_ panel-key]]
                    (into {} (for [[p _] (get-in db [:panels panel-key :queries] {})] {p (get-in db [:status :reco p])}))))

(re-frame/reg-sub ::panel-views
                  (fn [db {:keys [panel-key]}] (let [v (get-in db [:panels panel-key :views])] (if (vector? v) {:base v} v))))

(re-frame/reg-sub
 ::block-runners
 (fn [db _]
   (get-in db [:server :settings :runners])))

(re-frame/reg-sub
 ::panel-runners
 (fn [db {:keys [panel-key]}]
   (let [br (keys (-> (get-in db [:server :settings :runners]) (dissoc :views) (dissoc :queries)))
         v (select-keys (get-in db [:panels panel-key]) br)]
     v)))

(re-frame/reg-sub
 ::panel-runners-only
 (fn [_ {:keys [panel-key]}]
   (let [;br (keys (-> (get-in db [:server :settings :runners]) (dissoc :views) (dissoc :queries)))
         ;v (select-keys (get-in db [:panels panel-key]) br)
         v @(ut/tracked-sub ::panel-runners {:panel-key panel-key}) ;; dumb? safe sub should cache?
         ]
     (into {} (for [[_ vv] v] vv)))))

(re-frame/reg-sub
 ::panel-runners-rev
 (fn [db {:keys [panel-key]}]
   (let [br (vec (keys (-> (get-in db [:server :settings :runners]) (dissoc :views) (dissoc :queries))))
         v (select-keys (get-in db [:panels panel-key]) br)
         ;;v (vec (apply concat (for [[k v] v] (for [vv v] vv))))
         ]
     v)))

(re-frame/reg-sub ::panel-view-keys (fn [db {:keys [panel-key]}] (let [v (get-in db [:panels panel-key :views])] (vec (keys v)))))

(re-frame/reg-sub ::selected-view
                  (fn [db [_ panel-key]]
                    (let [body    (get-in db [:panels panel-key :views])
                          queries (get-in db [:panels panel-key :queries])
                          view    (get-in db [:panels panel-key :selected-view])
                          runners (keys (-> (get-in db [:server :settings :runners]) (dissoc :views) (dissoc :queries)))
                          runners2 (vec (flatten (for [[_ v] (select-keys (get-in db [:panels panel-key]) runners)] (keys v))))
                          vq      (conj (conj (into runners2 (into (keys body) (keys queries))) :layered-viz) :dyn-tab)] ;; pseudo
                      (cond (and view (some #(= % view) vq)) view
                            (map? body)                      (nth (keys body) 0)
                            :else                            nil))))                                      ;(first queries))

(re-frame/reg-sub ::selected-view-alpha
                  (fn [db {:keys [panel-key]}]
                    (let [body    (get-in db [:panels panel-key :views])
                          queries (get-in db [:panels panel-key :queries])
                          runners (keys (-> (get-in db [:server :settings :runners]) (dissoc :views) (dissoc :queries)))
                          runners2 (vec (flatten (for [[_ v] (select-keys (get-in db [:panels panel-key]) runners)] (keys v))))
                          ;;conj on a map takes map entries or seqables of map entries
                          ;;_ (ut/tapp>> [:run runners runners2 (for [[_ v] (select-keys (get-in db [:panels panel-key]) runners)] (keys v))])
                          view    (get-in db [:panels panel-key :selected-view])
                          vq      (conj (conj (into runners2 (into (keys body) (keys queries))) :layered-viz) :dyn-tab)
                          ;;_ (tapp>> [:vq (str vq (str runners2))])
                          v  (cond (and view (some #(= % view) vq)) view
                                   (map? body)                      (nth (keys body) 0)
                                   :else                            nil)
                          ;;_ (when (ut/ne? runners2) (ut/tapp>> [:vq vq v]))
                          ]
                      v)))


(re-frame/reg-sub ::selected-view-w-queries
                  (fn [db [_ panel-key]]
                    (let [body    (get-in db [:panels panel-key :views])
                          queries (keys (get-in db [:panels panel-key :queries]))
                          view    (get-in db [:panels panel-key :selected-view])]
                      (cond view        view
                            (map? body) (nth (keys body) 0)
                            :else       (first queries)))))

;; (re-frame/reg-sub ::editor-panel-selected-view ;; this whole sub is bonkers. kek
;;                   (fn [db _] ;; todo, this is so weird. old code. fix lmao
;;                     (let [selected-block (get db :selected-block)
;;                           ;;_ (ut/tapp>>  [:key2])
;;                           sql-calls      @(ut/tracked-sub ::panel-sql-calls {:panel-key selected-block})
;;                           views          @(ut/tracked-sub ::panel-views {:panel-key selected-block})
;;                           ;runners-rev    @(ut/tracked-sub ::panel-runners {:panel-key selected-block})
;;                           ;;_ (ut/tapp>> [:runners runners-rev])
;;                           ;runners        (into {} (for [[k v] runners-rev] {(first (first v)) {:base-key k}}))
;;                           ;runners        (vec (apply concat (for [[k v] runners] (for [vv v] [k vv]))))
;;                           first-data-key (first (keys sql-calls))
;;                           first-view-key (first (keys views))
;;                           ;first-runner-key (first (second runners))
;;                           data-key       (if (get @db/data-browser-query selected-block)
;;                                            (get @db/data-browser-query selected-block)
;;                                            first-data-key)  ;; lol
;;                           data-key       (if (nil? data-key) first-view-key data-key) ;; lol 
;;                           ;data-key       (if (nil? data-key) first-runner-key data-key) ;; lol
;;                           what           (or (when (some #(= % data-key) (keys sql-calls)) :queries) ;; lol wut
;;                                              (when (some #(= % data-key) (keys views)) :views)
;;                                              ;(when (some #(= % data-key) (vec (map second runners))) (get-in runners [data-key :base-key]))
;;                                              ;(first (filter #(= (last %) data-key) runners))
;;                                              )]

;;                       [what data-key]
;;                       ;[data-key  nil] ;(first (filter #(= (last %) data-key) runners))
;;                       )))

(re-frame/reg-sub
 ::editor-panel-selected-view
 (fn [db _]
   (let [selected-block (get db :selected-block)
         panel (dissoc (get-in db [:panels selected-block]) :selected-mode :selected-view :opts)
         panel-pairs (apply concat (for [[k v] panel
                                         :when (map? v)]
                                     (for [vv (keys v)] [k vv])))
         chosen (try
                  (first (filter #(= (second %) (get @db/data-browser-query selected-block)) panel-pairs)) (catch :default _ nil))
         chosen (cond
                  (= (get @db/data-browser-query selected-block) :*) [nil nil]
                  (nil? chosen) (first panel-pairs)
                  :else chosen)]
     chosen)))


(re-frame/reg-sub
 ::view-type
 (fn [db {:keys [panel-key view]}]
   (let [selected-block panel-key
         panel (dissoc (get-in db [:panels selected-block]) :selected-mode :selected-view :opts)
         panel-pairs (apply concat (for [[k v] panel
                                         :when (map? v)]
                                     (for [vv (keys v)] [k vv])))
         chosen (try
                  (first (filter #(= (second %) view) panel-pairs)) (catch :default _ nil))
         chosen (cond
                                   ;(= (get @db/data-browser-query selected-block) :*) [nil nil]
                  (nil? chosen) (first panel-pairs)
                  :else chosen)]
     (first chosen))))

(re-frame/reg-sub
 ::view-type-map
 (fn [db {:keys [view-type]}]
   (get-in db [:server :settings :runners view-type])))

(re-frame/reg-sub
 ::repl-output-type
 (fn [db {:keys [panel-key view-name]}]
   (get-in db [:panels panel-key :display view-name] :value)))

(re-frame/reg-sub
 ::has-rowset-data?
 (fn [db {:keys [panel-key view-name]}]
   (ut/ne? (get-in db [:data view-name]))))

(re-frame/reg-event-db
 ::select-output-type
 (fn [db [_ panel-key view-name display]]
   (let [vbunny-keys (filterv #(not (cstr/starts-with? % (str panel-key))) (keys @vbunny/node-ref))]
     (reset! vbunny/node-ref (select-keys @vbunny/node-ref vbunny-keys))
     (reset! vbunny/scroll-state (select-keys @vbunny/scroll-state vbunny-keys))
     (assoc-in db [:panels panel-key :display view-name] display))))

;;(ut/tapp>> [:ed  (str @(ut/tracked-sub ::editor-panel-selected-view2 {}))])

(re-frame/reg-event-db
 ::select-view
 (fn [db [_ panel-key view-key]]
   (assoc-in db [:panels panel-key :selected-view] view-key)))

(re-frame/reg-sub ::panel-map (fn [db [_ panel-key]] (get-in db [:panels panel-key])))

(re-frame/reg-sub
 ::panel-connection-id
 (fn [db [_ panel-key]]
   (get-in db [:panels panel-key :connection-id])))

(re-frame/reg-sub
 ::panel-connection-id-alpha
 (fn [db {:keys [panel-key]}]
   (get-in db [:panels panel-key :connection-id])))

(re-frame/reg-sub ::panel-sql-calls
                  (fn [db {:keys [panel-key]}]
                    (into {} (for [[k v] (get-in db [:panels panel-key :queries])] (when (nil? (find v :vselect)) {k v})))))

(re-frame/reg-sub ::panel-sql-call-keys
                  (fn [db {:keys [panel-key]}]
                    (keys (into {}
                                (for [[k v] (get-in db [:panels panel-key :queries])] (when (nil? (find v :vselect)) {k v}))))))

(re-frame/reg-sub ::panel-vsql-calls
                  (fn [db [_ panel-key]]
                    (into {} (for [[k v] (get-in db [:panels panel-key :queries])] (when (not (nil? (find v :vselect))) {k v})))))

(re-frame/reg-sub
 ::panel-vsql-call-keys
 (fn [db [_ panel-key]]
   (keys (into {} (for [[k v] (get-in db [:panels panel-key :queries])] (when (not (nil? (find v :vselect))) {k v}))))))

(re-frame/reg-sub ::panel-sql-aliases-in-views
                  (fn [db [_ panel-key]]
                    (let [all-sql-call-keys (set (keys (into {}
                                                             (for [[_ v] (get db :panels)]
                                                               (into {}
                                                                     (for [[kk vv] (get v :queries)]
                                                                       (when (nil? (find vv :vselect)) {kk vv})))))))
                          used-keywords     (set (ut/deep-flatten (get-in db [:panels panel-key :views])))]
                      (cset/intersection all-sql-call-keys used-keywords))))

(re-frame/reg-sub ::panel-sql-aliases-in-views-body
                  (fn [db {:keys [panel-key body]}]
                    (let [all-sql-call-keys (set (keys (into {}
                                                             (for [[_ v] (get db :panels)]
                                                               (into {}
                                                                     (for [[kk vv] (get v :queries)]
                                                                       (when (nil? (find vv :vselect)) {kk vv})))))))
                          raw-body          (set (ut/deep-flatten body))
                          used-keywords     (set (ut/deep-flatten (get-in db [:panels panel-key :views])))
                          used              (cset/union raw-body used-keywords)]
                      (cset/intersection all-sql-call-keys used))))

(re-frame/reg-sub
 ::panel-parameters-in-views
 (fn [db [_ panel-key]]
   (let [all-click-params  (set (keys (remove empty?
                                              (into {}
                                                    (for [[k v] (get-in db [:click-param])]
                                                      (into {}
                                                            (for [[kk _] v]
                                                              {(keyword (str (ut/safe-name k))) nil              ;; reffing
                                                               (keyword (str (ut/safe-name k) "-parameter")) nil ;; reffing
                                                               (keyword (str (ut/safe-name k) "/" (ut/safe-name kk))) nil
                                                               (keyword (str (ut/safe-name k) "-parameter/" (ut/safe-name kk)))
                                                               nil})))))))
         used-click-params (set (ut/deep-flatten (get-in db [:panels panel-key :views])))]
     (cset/intersection all-click-params used-click-params))))

(re-frame/reg-sub
 ::all-sql-calls
 (fn [db]
   (into {} (for [[_ v] (get db :panels)] (into {} (for [[kk vv] (get v :queries)] (when (nil? (find vv :vselect)) {kk vv})))))))

(re-frame/reg-sub ::all-sql-call-keys
                  (fn [db]
                    (keys (into {}
                                (for [[_ v] (get db :panels)]
                                  (into {} (for [[kk vv] (get v :queries)] (when (nil? (find vv :vselect)) {kk vv}))))))))

(re-frame/reg-sub ::all-vsql-call-keys
                  (fn [db]
                    (keys (into {}
                                (for [[_ v] (get db :panels)]
                                  (into {} (for [[kk vv] (get v :queries)] (when (not (nil? (find vv :vselect))) {kk vv}))))))))

(re-frame/reg-sub
 ::all-vsql-calls
 (fn [db]
   (into {}
         (for [[_ v] (get db :panels)]
           (into {}
                 (for [[kk vv] (get v :queries)]
                   (when (and (not (cstr/starts-with? (str kk) ":query-preview")) (not (nil? (find vv :vselect)))) {kk vv})))))))

(re-frame/reg-sub ::all-click-params2
                  (fn [db]
                    (vec (remove empty?
                                 (for [[k v] (get-in db [:click-param])]
                                   (into {} (for [[kk vv] v] {(keyword (str (ut/safe-name k) "/" (ut/safe-name kk))) vv})))))))


(re-frame/reg-sub
 ::all-click-params
 (fn [db]
   (let [all-condis {:condi (into {} (for [[_ v] (get db :panels)] (into {} (for [[kk vv] (get v :conditionals)] {kk vv}))))}
         clicks     (get-in db [:click-param])
         cc         (merge clicks all-condis)]
     (vec (remove empty?
                  (for [[k v] cc] (into {} (for [[kk vv] v] {(keyword (str (ut/safe-name k) "/" (ut/safe-name kk))) vv}))))))))

;; (defn update-click-params-snapshot []
;;   (let [current-params @(ut/tracked-sub ::all-click-params {})]
;;     (reset! db/click-params-snapshot current-params)))

(re-frame/reg-fx
 :update-click-params-snapshot
 (fn [_]
   (let [current-params @(ut/tracked-sub ::all-click-params {})]
     (reset! db/click-params-snapshot current-params))))

(re-frame/reg-event-fx
 ::update-click-params-snapshot
 (fn [_ _]
   {:update-click-params-snapshot nil}))

;; (tapp>> [:update-click-params-snapshot! (update-click-params-snapshot)])

(re-frame/reg-sub
 ::all-click-params-snapshot
 (fn [_ _]
   (let [snapshot @db/click-params-snapshot]
     (if (or (nil? snapshot) (empty? snapshot))
       @(ut/tracked-sub ::all-click-params {})
       snapshot))))



;;(tapp>> @(re-frame/subscribe [::all-click-params]))

(defn sql-data-boxes-values
  [keypath clickable? style]
  (let [styles        (if (and (map? style) (not (nil? style))) style {})
        data          @(ut/tracked-subscribe [::conn/sql-data keypath])
        clicked-param @(ut/tracked-subscribe [::conn/clicked-parameter keypath])]
    (vec (for [r data]
           [re-com/box :size "auto" :padding "4px" :attr
            (if clickable? {:on-click #(ut/tracked-dispatch [::click-parameter keypath r])} {}) :child (str (vals r)) :style
            (merge styles
                   {:cursor (if clickable? "pointer" "inherit") :background-color (if (= r clicked-param) "grey" "inherit")})]))))

(defn sql-data-boxes-values2
  [keypath style]
  (let [styles        (if (and (map? style) (not (nil? style))) style {})
        data          @(ut/tracked-subscribe [::conn/sql-data keypath])
        clicked-param @(ut/tracked-subscribe [::conn/clicked-parameter keypath])]
    (vec (for [r data]
           [re-com/box :size "auto" :padding "4px" :attr {:on-click #(ut/tracked-dispatch [::conn/click-parameter keypath r])}
            :child (str (vals r)) :style
            (merge styles {:cursor "pointer" :background-color (if (= r clicked-param) "grey" "inherit")})]))))

(re-frame/reg-sub ::keypaths-in-view
                  (fn [db [_ panel-key view-key]]
                    (let [view                 (if (or (= view-key :base) (nil? view-key))
                                                 (get-in db [:panels panel-key :views])
                                                 (get-in db [:panels panel-key :views view-key]))
                          kvpaths              (ut/keypaths view) ;(sort (ut/keypaths view))
                          layout-map-scrubbers @(ut/tracked-subscribe [::scrub/keypaths-of-layout-panels panel-key view-key])
                          kvpaths-vals         (into {}
                                                     (for [p (sort (into layout-map-scrubbers kvpaths))]
                                                       {p (let [v (get-in view p)]
                                                            [v (try (scrub/get-type p v) (catch :default e (str e)))])}))]
                      kvpaths-vals)))

(re-frame/reg-sub ::query-schedule
                  (fn [db _]
                    (let [sched (get db :sched {})]
                      (into {} (for [[k v] sched] {(str [:run k]) (- v (get-in db [:re-pollsive.core/polling :counter]))})))))

(re-frame/reg-sub
 ::keypaths-in-rs-values
 (fn [db [_ flow-id bid]]
   (try (let [pp           (get-in db [:runstreams-lookups flow-id :open-inputs bid :user-input])
              params       (get-in db
                                   [:runstreams flow-id :values bid :value] ; try runstream override
                                   (get-in db [:runstreams-lookups flow-id :open-inputs bid :user-input])) ; if
              params       {bid (or params pp)}
              kvpaths      (ut/keypaths params)
              kvpaths-vals (into (sorted-map)
                                 (for [p kvpaths]
                                   {p (let [v (get-in params p)]
                                        [v (try (scrub/get-type p v) (catch :default e (str e)))])}))]
          kvpaths-vals)
        (catch :default e (do (ut/tapp>> [:error-in :keypaths-in-rs-values e :passed bid]) {})))))

(re-frame/reg-sub ::keypaths-in-flow
                  (fn [db [_ bid & [single-input?]]]
                    (try (let [single-input? (if (nil? single-input?) false true)
                               params        (get-in db
                                                     (if single-input?
                                                       [:flows (get db :selected-flow) :map bid :data :user-input]
                                                       [:flows (get db :selected-flow) :map bid :data :flow-item :defaults])
                                                     {})
                               params        (if single-input? {:* params} params)
                               kvpaths       (ut/keypaths params)
                               kvpaths-vals  (into (sorted-map)
                                                   (for [p kvpaths]
                                                     {p (let [v (get-in params p)]
                                                          [v (try (scrub/get-type p v) (catch :default e (str e)))])}))]
                           kvpaths-vals)
                         (catch :default e (do (ut/tapp>> [:error-in :keypaths-in-flow e :passed bid]) {})))))

(re-frame/reg-sub ::keypaths-in-flow-opts
                  (fn [db _]
                    (try (let [params       (get-in db [:flows (get db :selected-flow) :opts])
                               kvpaths      (ut/keypaths params)
                               kvpaths-vals (into (sorted-map)
                                                  (for [p kvpaths]
                                                    {p (let [v (get-in params p)]
                                                         [v (try (scrub/get-type p v) (catch :default e (str e)))])}))]
                           kvpaths-vals)
                         (catch :default e (do (ut/tapp>> [:error-in :keypaths-in-flow-opts e]) {})))))

(re-frame/reg-sub ::keypaths-in-params
                  (fn [db {:keys [key-type]}]
                    (try (let [params       (dissoc (get-in db [:click-param key-type]) :selected-view :selected-view-data :selected-block) 
                               kvpaths      (ut/keypaths params)
                               kvpaths-vals (into (sorted-map)
                                                  (for [p kvpaths]
                                                    {p (let [v (get-in params p)]
                                                         [v (try (scrub/get-type p v) (catch :default e (str e)))])}))]
                           kvpaths-vals)
                         (catch :default e (do (ut/tapp>> [:error-in :keypaths-in-params e :passed key-type]) {})))))

(re-frame/reg-sub ::keypaths-in-query
                  (fn [db [_ panel-key query-key]]
                    (let [query        (get-in db [:panels panel-key :queries query-key])
                          kvpaths      (ut/kvpaths query)
                          kvpaths-vals (into (sorted-map)
                                             (for [p kvpaths]
                                               {p (let [v (get-in query p)]
                                                    [v (try (scrub/get-type p v) (catch :default e (str e)))])}))]
                      kvpaths-vals)))









(re-frame/reg-sub ::valid-body-params
                  (fn [db {:keys [panel-key]}]
                    (let [dat   (get-in db [:panels panel-key :views])
                          flats (ut/deep-flatten dat)]
                      (vec (remove nil? (map ut/process-key flats))))))

(re-frame/reg-sub ::valid-body-params-all-condis
                  (fn [db _]
                    (let [dat   (for [[_ v] (get-in db [:panels])] (get v :views))
                          flats (ut/deep-flatten dat)]
                      (vec (remove nil? (map ut/process-key flats))))))

(re-frame/reg-sub ::valid-body-params-in (fn [_ {:keys [body]}] (vec (remove nil? (map ut/process-key (ut/deep-flatten body))))))

(re-frame/reg-sub ::valid-sql-params
                  (fn [db [_ panel-key]]
                    (vec (remove nil? (map ut/process-key (ut/deep-flatten (get-in db [:panels panel-key :queries])))))))





(defn vsql-map
  [hm]
  (let [select     (get hm :vselect)  ;; check if vector
        from       (get hm :from)
        where      (get hm :where)
        group-bys  (get hm :group-by) ;; nest-by ?
        rowset     (if (vector? from) from [from])
        rowset     (if (and (ut/ne? group-bys) (not (nil? group-bys)))
                     (let [group-by-field (first group-bys)]
                       (doall (for [[k v] (group-by group-by-field rowset)] {group-by-field k :rows v})))
                     rowset)
        inside-job (fn [obody]
                     (let [kps       (into {} (for [p (ut/kvpaths obody)] {p (get-in obody p)}))
                           logic-kps (into {}
                                           (for [[_ v] (into {} (filter #(cstr/starts-with? (str (last %)) "{:vselect") kps))]
                                             {v (vsql-map v)}))]
                       (ut/postwalk-replacer logic-kps obody)))]
    (vec
     (for [row rowset]
       (do                                                             ;(ut/tapp>>
                                                                        ;[:vsql-loop-row row])
         (vec (for [r (ut/postwalk-replacer row select)]
                (do                                                    ;(when (not (or (string?
                                                                        ;r) (keyword? r)))
                                                                        ;(ut/tapp>>
                  (cond (and (map? r) (:vselect r))                (do ;(ut/tapp>>
                                                                        ;[:vsql-recur-fn r])
                                                                     (vsql-map r))
                        (some #(= % :vselect) (ut/deep-flatten r)) (do ;(ut/tapp>> [:run-inside
                                                                        ;r])
                                                                     (inside-job r))
                        :else                                      r)))))))))




(defn honeycomb-table [panel-key] (fn [x] [magic-table panel-key [x]]))

(defn walk-map-sizes
  [px-width-int px-height-int pw ph x y & [lay? vega-lite?]]
  (let [px-height-int (if (and (< px-height-int 1) (is-float? px-height-int) (not (nil? ph)))
                        (js/Math.floor (* px-height-int ph))
                        px-height-int)
        px-width-int  (if (and (< px-width-int 1) (is-float? px-width-int)) (js/Math.floor (* px-width-int pw)) px-width-int)
        px-height-int (if (and (= px-height-int 0) (not (nil? ph))) (- ph y 1) px-height-int) ;; stretch
        px-width-int  (if (= px-width-int 0) (- pw x) px-width-int) ;; stretch val
        px-width-int  (* db/brick-size px-width-int) ;; switch back to pixel sizes
        px-height-int (* db/brick-size px-height-int)
        px-width-int  (if lay? (- px-width-int 65) px-width-int)
        px-height-int (if lay? (- px-height-int 65) px-height-int)]
    {;:col-width                (Math/floor (/ px-width-int 9))
     :width-int                px-width-int
     :height-int               px-height-int
     :width-px                 (px px-width-int)
     :height-px                (px px-height-int)
     :panel-width              px-width-int
     :panel-height             (cond (and lay? vega-lite?) "container"
                                     vega-lite?            (- px-height-int 20)
                                     :else                 px-height-int) ;px-height-int
     :card-width               (+ 70 px-width-int) ;; moving terminology - Aug 2024
     :card-height              (+ 50 px-height-int) ;; moving terminology - Aug 2024
     :card-width-px            (px (+ 70 px-width-int)) ;; moving terminology - Aug 2024
     :card-height-px           (px (+ 50 px-height-int)) ;; moving terminology - Aug 2024
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
     :panel-width+100          (+ 100 px-width-int)
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

(re-frame/reg-sub ::resolve-view-alias
                  (fn [db [_ ref-key & [lw lh]]] ;; layout sizes, only used for query "views"
                    (let [unref-key-vec (ut/splitter (ut/replacer (ut/safe-name ref-key) #"view/" "") ".")
                          block-id      (keyword (first unref-key-vec))
                          view-name     (keyword (last unref-key-vec))
                          is-query?     (true? (= block-id :query))
                          qs            (if is-query?
                                          (apply merge
                                                 (flatten (remove empty?
                                                                  (into []
                                                                        (for [[pk pv] (get db :panels)] ;; expensive? cache
                                                                          (for [k (keys (get pv :queries))] {k pk}))))))
                                          {})
                          panel-key     (when is-query? (get qs view-name))
                          view-body     (if is-query?
                                          [reecatch
                                           [magic-table panel-key [(str ":" view-name)] ;; to avoid RE-resolving
                                            lw lh]]
                                          (get-in db [:panels block-id :views view-name]))]
                      view-body)))

(re-frame/reg-sub ::layouts-for
                  (fn [db [_ panel-key view k]]
                    (let [;kvs (ut/kvpaths (get-in db [:panels panel-key :views view])) ;; get nested layouts
                          layout-map-pre (get-in db [:panels panel-key :views view 1 :panels])
                          pw-int         (get-in db [:panels panel-key :w])
                          ph-int         (get-in db [:panels panel-key :h])
                          layout-map     (lay/process-layout layout-map-pre pw-int ph-int)]
                      (get layout-map k))))

(defn view-alias-replace
  [view panel-key selected-view]
  (let [valid-params (vec (remove nil?
                                  (for [k (ut/deep-flatten view)] (when (and (keyword? k) (cstr/starts-with? (str k) ":view/")) k))))
        [ph pw]      @(ut/tracked-subscribe [::size panel-key])
        sql-params   (into {}
                           (for [k valid-params] ;; deref here?
                             (let [deets      @(ut/tracked-subscribe [::layouts-for panel-key selected-view k])
                                   body       @(ut/tracked-subscribe [::resolve-view-alias k (get deets :w) (get deets :h)])
                                   body       (if (nil? body)
                                                [re-com/box :align :center :justify :center :size "auto" :child
                                                 [:div
                                                  [:span
                                                   {:style {:color       "pink"
                                                            :font-size   "22px"
                                                            :font-family (theme-pull :theme/base-font nil)}}
                                                   (str selected-view " from " panel-key "?")] [:br]
                                                  (str " I can't find this view. Halp.")] :width :width-px :height :height-px]
                                                body)
                                   vega-lite? (true? (some #(= % :vega-lite) (ut/deep-flatten body)))
                                   size-map   (when (and (ut/ne? deets) (not (nil? (get deets :w))) (not (nil? (get deets :h))))
                                                (walk-map-sizes (get deets :w) ;(* db/brick-size (get deets :w))
                                                                (get deets :h) ;(* db/brick-size (get deets :h))
                                                                pw
                                                                ph
                                                                (get deets :x)
                                                                (get deets :y)
                                                                true
                                                                vega-lite?))]
                               {k (if size-map (ut/postwalk-replacer size-map body) body)})))
        replaced     (ut/postwalk-replacer sql-params view)]
    (if (cstr/includes? (str replaced) ":view/") ;(cstr/includes? (str replaced) ":query/")
      (view-alias-replace replaced panel-key selected-view)
      replaced)))

(re-frame/reg-sub ::lookup-panel-key-by-query-key
                  (fn [db [_ query-key]]
                    (first (remove nil?
                                   (for [[k v] (get db :panels)] (when (some #(= query-key %) (keys (get v :queries))) k))))))

(re-frame/reg-event-db ::remove-schedule
                       (fn [db [_ query-key]]
                         (-> db
                             (ut/dissoc-in [:sched query-key]))))

(re-frame/reg-event-db ;; original. works fine. BUT dumps all the query execs into one
 ::dispatch-auto-queries
 (fn [db [_]]
   (let [scheds         (get db :sched)
         scheds-reverse (doall (into {} (for [vv (vals scheds)] {vv (vec (for [[k v] scheds :when (= v vv)] k))})))
         ticktock       (get-in db [:re-pollsive.core/polling :counter])]
     (dorun (doseq [[timer queries] scheds-reverse
                    :when           (>= ticktock timer)]
              (doseq [query queries]
                (dorun (let []
                         (ut/tracked-dispatch [::remove-schedule query])
                         (ut/tracked-dispatch [::conn/clear-query-history query]))))))
     db)))




(re-frame/reg-event-db ::insert-sql-source (fn [db [_ query-id sql-source]] (assoc-in db [:sql-source query-id] sql-source)))

(re-frame/reg-event-db ::check-status ;; deprecated due to pub sub reactor
                       (fn [db _]
                         (let [client-name (get db :client-name)]
                           (ut/tracked-dispatch [::wfx/request :default
                                                 {:message     {:kind :get-status :client-name client-name}
                                                  :on-response [::http/status-response]
                                                  :timeout     500000}])
                           db)))

(re-frame/reg-sub
 ::something-running?
 (fn [db _]
   (let [status (get db :status)]
     (true? (some #(or (= % :started) (= % :queued))
                  (ut/deep-flatten status))))))

(re-frame/reg-event-db
 ::prune-alerts
 (fn [db [_ & [all?]]]
   (if (not @db/pause-alerts)
     (let [alerts   (get db :alerts)
           ticktock (get-in db [:re-pollsive.core/polling :counter])]
       (if all? (assoc db :alerts []) (assoc db :alerts (vec (filter #(> (get % 3) ticktock) alerts)))))
     db)))

(re-frame/reg-event-db
 ::prune-alert
 (fn [db [_ alert-id]]
   (ut/tapp>> [:prune-alert alert-id])
   (if (not= alert-id 0)
     (let [alerts (get db :alerts)]
       (assoc db :alerts (vec (remove #(= (last %) alert-id) alerts))))
     (assoc db :alerts []))))

;;(ut/tracked-dispatch [::http/get-autocomplete-values])

(re-frame/reg-event-db
 ::clear-cache-atoms
 (fn [db _]
   (ut/tapp>> [:clearing-cache-atoms! (get db :client-name)])
   (doseq [a [honeycomb-context-fns-cache
              ut/replacer-data ut/replacer-cache ut/deep-flatten-data ut/deep-flatten-cache ut/map-boxes-cache ut/map-boxes-cache-hits
              ut/clover-walk-singles-map ut/process-key-cache ut/process-key-tracker ut/compound-keys-cache get-all-values-flatten
              ut/compound-keys-tracker ut/upstream-cache ut/upstream-cache-tracker ut/downstream-cache ut/deep-template-find-cache
              ut/downstream-cache-tracker ut/split-cache ut/split-cache-data ut/extract-patterns-data conn/format-puget-atom 
              ut/extract-patterns-cache ut/clean-sql-atom ut/auto-font-atom ut/postwalk-replace-data-cache
              ut/postwalk-replace-cache ut/is-base64-atom ut/is-large-base64-atom ut/safe-name-cache
              ut/format-map-atom ut/body-set-atom ut/data-typer-atom ut/coord-cache]]
     (reset! a {}))
   db))


(re-frame/reg-event-db
 ::purge-cache-atoms
 (fn [db _]
   db))

(re-frame/reg-event-db
 ::purge-cache-atoms22
 (fn [db _]
   (let [mem (get db :memory)
         js-heap (str (ut/bytes-to-mb (get mem 1)))
         atom-size-map
         (ut/calculate-atom-sizes
          {:ut/replacer-data               ut/replacer-data
           :ut/replacer-cache              ut/replacer-cache
           :ut/deep-flatten-data           ut/deep-flatten-data
           :ut/deep-flatten-cache          ut/deep-flatten-cache
           :ut/split-cache                 ut/split-cache
           :ut/split-cache-data            ut/split-cache-data
           :ut/clover-walk-singles-map     ut/clover-walk-singles-map
           :ut/process-key-cache           ut/process-key-cache
           :ut/process-key-tracker         ut/process-key-tracker
           :ut/compound-keys-cache         ut/compound-keys-cache
           :ut/compound-keys-tracker       ut/compound-keys-tracker
           :ut/extract-patterns-data       ut/extract-patterns-data
           :ut/extract-patterns-cache      ut/extract-patterns-cache
           :ut/postwalk-replace-data-cache ut/postwalk-replace-data-cache
           :ut/postwalk-replace-cache      ut/postwalk-replace-cache
           :ut/upstream-cache              ut/upstream-cache
           :ut/upstream-cache-tracker      ut/upstream-cache-tracker
           :ut/downstream-cache            ut/downstream-cache
           :ut/downstream-cache-tracker    ut/downstream-cache-tracker
           :ut/auto-font-atom              ut/auto-font-atom
           :ut/subq-panels-alpha           ut/subq-panels-alpha
           :ut/subq-mapping-alpha          ut/subq-mapping-alpha
           :db/flow-results                db/flow-results
           :db/scrubbers                   db/scrubbers
           :scroll-state                   vbunny/scroll-state
           :h-scroll-state                 vbunny/h-scroll-state
           :opened-boxes                   opened-boxes
           :opened-boxes-code              opened-boxes-code
           :db/context-box                 db/context-box
           :bricks/get-all-values-flatten  get-all-values-flatten
           :ut/is-base64-atom              ut/is-base64-atom
           :db/solver-fn-runs              db/solver-fn-runs
           :db/solver-fn-lookup            db/solver-fn-lookup
           :ut/is-large-base64-atom        ut/is-large-base64-atom
           :ut/safe-name-cache             ut/safe-name-cache
           :ut/map-boxes-cache             ut/map-boxes-cache
           :re-frame.db/app-db             re-frame.db/app-db
           :ut/clean-sql-atom              ut/clean-sql-atom
           :ut/format-map-atom             ut/format-map-atom
           :ut/body-set-atom               ut/body-set-atom
           :ut/data-typer-atom             ut/data-typer-atom
           :ut/coord-cache                 ut/coord-cache})
         ttl (-> (apply + (for [[_ v] atom-size-map] (get v :mb)))
                 js/Number.
                 (.toFixed 3)
                 js/parseFloat)]
     (ut/tapp>> [:atom-sizes :js-heap js-heap :atoms-raw-ttl (str ttl "MB") atom-size-map])
     (do (when (> (get-in atom-size-map [:ut/postwalk-replace-data-cache :mb]) 40) (ut/purge-postwalk-cache 0.99 200))
         (when (> (get-in atom-size-map [:bricks/get-all-values-flatten]) 40) (reset! get-all-values-flatten {}))
         (when (> (get-in atom-size-map [:ut/split-cache-data :mb]) 40) (ut/purge-splitter-cache 0.99 100))
         (when (> (get-in atom-size-map [:ut/replacer-data :mb]) 40) (ut/purge-replacer-cache 0.99 50))
         (when (> (get-in atom-size-map [:ut/deep-flatten-data :mb]) 40) (ut/purge-deep-flatten-cache 0.99 100))
         (when (> (get-in atom-size-map [:ut/extract-patterns-data :mb]) 40) (ut/purge-extract-patterns-cache 0.99 100)))
     db)))

(re-frame/reg-event-db
 ::clean-up-reco-previews
 (fn [db [_]]
  ;;  (tapp>> [:clean-up-reco-previews!])
   (let [ks1     (vec (apply concat (for [k (keys (get-in db [:panels]))] (keys (get-in db [:panels k :queries])))))
         sys-ks  (vec (filter #(or (cstr/ends-with? (str %) "-sys")
                                   (cstr/ends-with? (str %) "-sys2")
                                   (cstr/ends-with? (str %) "-sys*"))
                              (keys (get db :data))))
         implicit-rowsets (vec (into (get-in db [:implicit-rowsets :clover] []) (get-in db [:implicit-rowsets :solver] [])))
         base-ks (into (vec (keys (get db :base-sniff-queries))) [:reco-counts]) ;; why is
         ks      (into implicit-rowsets (into base-ks (into ks1 sys-ks)))
         ks-all  (keys (get db :data)) ;;; hmmm, curious why I did this initially...
         keepers (vec (filter #(not (cstr/starts-with? (str %) ":query-preview")) ks))
         run-it? (or (and (nil? @mad-libs-view) (not (= @db/editor-mode :vvv)) (not (= (count ks-all) (count ks))))
                     (and (get db :flow?) (get db :flow-editor?)))]
     (if run-it?
       (-> db
           (ut/dissoc-in [:panels nil]) ;; garbage keys created by external edit with bad
           (assoc :panels (select-keys (get db :panels)
                                       (filter #(not (cstr/starts-with? (str %) ":query-preview")) (keys (get db :panels)))))
           (assoc :data (select-keys (get db :data) keepers))
           (assoc :meta (select-keys (get db :meta) keepers))
           (assoc :orders (select-keys (get db :orders) keepers))
           (assoc :post-meta (select-keys (get db :post-meta) keepers))
           (assoc :sql-str (select-keys (get db :sql-str) keepers))
           (assoc :sql-source (select-keys (get db :sql-source) keepers))
           (assoc :query-history (select-keys (get db :query-history) (filter #(not (cstr/ends-with? (str %) "-sys*")) keepers)))
          ;;  (assoc :click-param (into {}
          ;;                            (for [[k v] (get db :click-param)
          ;;                                  :when (and (not (cstr/starts-with? (str k) ":kit-")) (not (nil? k)) (not (nil? v)))]
          ;;                              {k v})))
           (ut/dissoc-in [:panels "none!"])
           (ut/dissoc-in [:panels :queries])
           (ut/dissoc-in [:panels :views])
           (ut/dissoc-in [:panels nil]))
       (-> db
           (ut/dissoc-in [:panels nil])
           (ut/dissoc-in [:panels "none!"])
           (ut/dissoc-in [:panels :queries])
           (ut/dissoc-in [:panels :views])
          ;;  (assoc :click-param (into {}
          ;;                            (for [[k v] (get db :click-param)
          ;;                                  :when (and (not (cstr/starts-with? (str k) ":kit-")) (not (nil? k)) (not (nil? v)))]
          ;;                              {k v})))
)))))  ;; garbage keys created by external edit with bad files


(re-frame/reg-sub
 ::get-combo-rows
 (fn [db [_ panel-id [combo-hash table-name]]]
   (let [existing    (get-in db [:data :mad-libs-viz panel-id table-name])
         running?    (fn [kp]
                       (some ;#(= (first %) panel-id)
                        #(= % kp)
                        (for [e @(ut/tracked-sub ::http/pending-requests {:socket-id http/socket-id})]
                          (let [p (get-in e [:message :ui-keypath])] p))))
         reco-counts @(ut/tracked-subscribe [::query-reco-counts table-name])]
     ;(tapp>> [:combo-rows (str existing) table-name reco-counts (get-in db [:data :reco-counts])])
     (when (and (empty? (get-in db [:data :viz-shapes]))
                (not (running? [:viz-shapes])))
       (conn/sql-data [:viz-shapes]
                      {:select [:axes_logic :base_score :connection_id :library_shapes :selected_view :run_id :shape_name
                                :sql_maps]
                       :from   [[:rule_maps_viz_shapes :uu490]]})) ;)
     (when (and (empty? existing) (> reco-counts 0) (not (running? [:mad-libs-viz panel-id table-name])))
       (conn/sql-data
        [:mad-libs-viz panel-id table-name]
        {:select [:*]
         :where  [:and
                  [:in :context_hash ;; self sub-q
                   {:select [:context_hash] :from [[:combo_rows :uu2434aa]] :where [:and [:= :combo_hash combo-hash]]} :tete]
                  [:in :shape_name ;; self sub-q
                   {:select [:shape_name] :from [[:combo_rows :uu2434aa]] :where [:and [:= :combo_hash combo-hash]]} :tete2]]
         :from   [[:combo_rows :uu2434aa]]}))
     (get-in db [:data :mad-libs-viz panel-id table-name]))))

(re-frame/reg-sub ::get-mad-libs-options
                  (fn [db [_ panel-id src-table-id-str combo-hash]]
                    (let [rows          (get-in db [:data :mad-libs-viz panel-id src-table-id-str])
                          shape-name    (get (first rows) :shape_name)
                          current-combo (filter #(= (get % :combo_hash) combo-hash) rows)]
                      {:shape_name    shape-name
                       :current-combo (into {} (for [[k v] (group-by :axes current-combo)] {k (first v)}))
                       :data          (group-by (juxt :talias :field_name :walk1 :walk2 :walk3 :walka :axes) rows)})))

(re-frame/reg-sub ::lookup-combo-hash2     ;; old - was converted into a reg-event
                  (fn [db [_ panel-id src-table-id-str shape-name axes-map]]
                    (let [;hm (str (hash axes-map))
                          running? (fn [kp]
                                     (some ;#(= (first %) panel-id)
                                      #(= % kp)
                                      (for [e @(ut/tracked-sub ::http/pending-requests {:socket-id http/socket-id})]
                                        (let [p (get-in e [:message :ui-keypath])] p))))
                          kp1      [:data :mad-libs-viz2 panel-id src-table-id-str shape-name]
                          kp2      (vec (remove #(= % :data) kp1)) ;; [:mad-libs-viz2 panel-id src-table-id-str
                          have-it? (= (get (first (get-in db kp1)) :key_hashes) (pr-str axes-map))
                          lk-sql   {:select [:*]
                                    :from   [:combos]
                                    :where  [:and [:= :key_hashes (pr-str axes-map)] [:= :table_name src-table-id-str]
                                             [:= :shape_name shape-name]]}]
                      (when (and (not have-it?) (not (running? kp2))) (conn/sql-data kp2 lk-sql))
                      (get-in db kp1))))

(re-frame/reg-sub ::lookup-combo-hash
                  (fn [db [_ panel-id src-table-id-str shape-name axes-map]]
                    (let [kp1 [:data :mad-libs-viz2 panel-id src-table-id-str shape-name]] (get-in db kp1))))

(defn prepare-combo-viz
  [reco-viz reco-query reco-condis]                                                              ;; dupe of part of x TODO
  (cond (= (first (read-string reco-viz)) :vega-lite) (let [incomingv     (read-string reco-viz) ;; read-string will ruin my
                                                                                                 ;; internal
                                                            ffromv        (-> (get-in incomingv [1 :data :values])
                                                                              (ut/replacer "_" "-")
                                                                              (ut/replacer "query/" ""))
                                                            original-conn @(ut/tracked-subscribe
                                                                            [::lookup-connection-id-by-query-key
                                                                             (keyword ffromv)])
                                                            view          (-> incomingv
                                                                              (assoc-in [1 :data :values] :query-preview)
                                                                              (assoc-in [1 :config] :theme/vega-defaults)
                                                                              (assoc-in [1 :width] "container") ;:panel-width)
                                                                              (assoc-in [1 :height] :panel-height)
                                                                              (assoc-in [1 :padding] 4)
                                                                              (assoc-in [1 :background] "transparent"))
                                                            q-data        (read-string reco-query)
                                                            incoming      (first q-data) ;; read-string will ruin my internal
                                                                                         ;; namespaced
                                                            ffrom         (ut/replacer (first (get incoming :from)) "_" "-")
                                                            query         (vec (for [q q-data] (assoc q :from [(keyword ffrom)])))
                                                            query         (ut/postwalk-replacer {[[:sum :rows]] [:count 1]}
                                                                                                query)]
                                                        {:view view :query query :condis reco-condis :conn original-conn})
        :else                                         (let [view          (read-string reco-viz) ;; read-string will ruin my
                                                                                                 ;; internal
                                                            q-data        (read-string reco-query)
                                                            incoming      (first q-data)         ;; read-string will ruin my
                                                                                                 ;; internal
                                                            ffrom         (ut/replacer (first (get incoming :from)) "_" "-")
                                                            original-conn @(ut/tracked-subscribe
                                                                            [::lookup-connection-id-by-query-key
                                                                             (keyword (last (ut/splitter ffrom "/")))])
                                                            query         (vec (for [q q-data]
                                                                                 (if (nil? (find q :vselect))
                                                                                   (assoc q :from [(keyword ffrom)])
                                                                                   q)))
                                                            query         (ut/postwalk-replacer {[:sum :rows] [:count 1]} query)]
                                                        {:view view :query query :condis reco-condis :conn original-conn})))

(defn modify-combo-viz
  [viz query condis connection-id combohash combo-name shape-name single? table-name-str] ;;; dupe
  (let [;table-name @(ut/tracked-subscribe [::conn/clicked-parameter-key (if panel?
        condi-keys  (try (keys (read-string condis)) (catch :default _ []))
        condi-walks (into {}
                          (for [c condi-keys]
                            (let [randy (rand-int 999)]
                              {c                                         (keyword (str "c" randy "-" (ut/safe-name c)))
                               (keyword (str "condi/" (ut/safe-name c))) (keyword (str "condi/c" randy "-" (ut/safe-name c)))})))
        queries     (into {}
                          (for [q (range (count query))]
                            {(keyword (if (= q 0)
                                        (if (not single?) (str "query-preview" combohash) "query-preview")
                                        (str (if (not single?) (str "query-preview" combohash "-") "query-preview-") (+ 1 q))))
                             (nth query q)}))
        queriesrep  (into {}
                          (for [q (range (count query))]
                            {(keyword (if (= q 0) "query-preview" (str "query-preview-" (+ 1 q))))
                             (keyword (if (= q 0)
                                        (str "query-preview" combohash)
                                        (str (str "query-preview" combohash "-") (+ 1 q))))}))
        base-map    {:mad-libs-combo-hash [combohash table-name-str]
                     :name                (str shape-name " - " combo-name " - " (rand-int 1234))
                     :connection-id       connection-id
                     :views               (if (map? viz) viz {:oz viz})
                     :queries             queries}
        base-map    (if (nil? condis) base-map (assoc base-map :conditionals (read-string condis)))
        base-map    (ut/postwalk-replacer condi-walks base-map)
        base-map    (if (not single?) (ut/postwalk-replacer queriesrep base-map) base-map)]
    base-map))

(defn final-combo-viz
  [ddata panel-key query-keys]
  (let [;; we need to change all the generic aliases into real rando keys
        req-name-str     (ut/replacer (str panel-key) #":" "") ;OLD req-name-str (str "block-"
        poss-value-walks (distinct (filter #(or (cstr/starts-with? (str %) ":query-preview")
                                                (cstr/starts-with? (str %) ":panel-key"))
                                           (ut/deep-flatten ddata)))
        key-walk1        (filter #(not (cstr/includes? (str %) "/")) poss-value-walks)
        key-walk         (merge ;; use old keys (since we should always already have the exact
                          (into {}
                                (for [idx  (range (count key-walk1))
                                      :let [k (get (vec key-walk1) idx)]]
                                  {k (get (vec query-keys) idx)}))
                          {:panel-key (keyword req-name-str)})
        value-walk1      (filter #(cstr/includes? (str %) "/") poss-value-walks)
        value-walk       (into {}
                               (for [v value-walk1]
                                 (let [vv   (ut/safe-name v)
                                       vs   (ut/splitter vv "/")
                                       qstr (ut/safe-name (first (ut/postwalk-replacer key-walk [(keyword (first vs))])))
                                       vstr (last vs)]
                                   (ut/tapp>> [:value-walk vv vs qstr vstr v (keyword (str qstr "/" vstr))])
                                   {v (keyword (str qstr "/" vstr))})))]
    (ut/tapp>> [:poss-value-walks poss-value-walks :key-walk key-walk :value-walk value-walk :query-keys query-keys])
    (merge (ut/postwalk-replacer (merge key-walk value-walk) ddata))))

(re-frame/reg-event-db ::get-combo-hash
                       (fn [db [_ panel-id src-table-id-str shape-name axes-map]]
                         (let [hm       (str (hash axes-map))
                               running? (fn [kp]
                                          (some ;#(= (first %) panel-id)
                                           #(= % kp)
                                           (for [e @(ut/tracked-sub ::http/pending-requests {:socket-id http/socket-id})]
                                             (let [p (get-in e [:message :ui-keypath])] p))))
                               kp1      [:data :mad-libs-viz2 panel-id src-table-id-str shape-name hm]
                               kp2      (vec (remove #(= % :data) kp1)) ;; [:mad-libs-viz2 panel-id src-table-id-str
                               exists?  (= (get (first (get-in db kp1)) :key_hashes) (pr-str axes-map))
                               lk-sql   {:select [:*]
                                         :from   [:combos]
                                         :where  [:and [:= :key_hashes (pr-str axes-map)] [:= :table_name src-table-id-str]
                                                  [:= :shape_name shape-name]]}]
                           (when (and (not exists?) (not (running? kp2))) (conn/sql-data kp2 lk-sql))
                           (if (not exists?)
                             (do (reset! db/mad-libs-waiting-room axes-map)
                                 (go (<! (async/timeout 1000))
                                     (do (ut/tracked-dispatch [::post-process-combo-hash panel-id src-table-id-str shape-name
                                                               kp1])
                                         (reset! db/mad-libs-waiting-room nil))))
                             (ut/tracked-dispatch [::post-process-combo-hash panel-id src-table-id-str shape-name kp1]))
                           db)))

(re-frame/reg-event-db
 ::post-process-combo-hash
 (undoable)
 (fn [db [_ panel-id src-table-id-str shape-name kp1]]
   (let [new-combo-row (get-in db kp1)
         combo-name (get (first new-combo-row) :combo_edn)
         combo-hash (get (first new-combo-row) :combo_hash)
         {:keys [view query condis conn error]} (try (prepare-combo-viz (get (first new-combo-row) :viz_map "[]")
                                                                        (get (first new-combo-row) :query_map "[]")
                                                                        (get (first new-combo-row) :conditionals ""))
                                                     (catch :default e {:error (str "error: " e)}))
         new-base-map (modify-combo-viz view query condis conn combo-hash combo-name shape-name false src-table-id-str)
         query-keys (keys (get-in db [:panels panel-id :queries])) ;; existing keyword aliases
         final-map (final-combo-viz new-base-map panel-id query-keys)]
     (ut/tapp>> [:new-base-map new-base-map :final-combo-viz final-map :new-combo-block [view query condis conn error]
                 :new-combo-row (first new-combo-row)])
     (if (not error) (assoc-in db [:panels panel-id] (merge (get-in db [:panels panel-id]) final-map)) db))))




(re-frame/reg-sub ::get-viz-shape
                  (fn [db [_ shape-name]] (first (get (group-by :shape_name (get-in db [:data :viz-shapes])) shape-name))))

(re-frame/reg-sub ::query-reco-counts
                  (fn [db [_ query-id]]
                    (let [query-id-str (-> (str query-id)
                                           (ut/replacer "-" "_")
                                           (ut/replacer ":" ""))]
                      (get (first (filter #(= (get % :table_name) query-id-str) (get-in db [:data :reco-counts]))) :cnt 0))))

;; (re-frame/reg-sub ::update-reco-previews?
;;                   (fn [db [_ query-id]]
;;                     (let [query-id-str (-> (str query-id)
;;                                            (ut/replacer "-" "_")
;;                                            (ut/replacer ":" ""))]
;;                       (get (first (filter #(= (get % :table_name) query-id-str) (get-in db [:data :reco-counts]))) :cnt 0))))

(re-frame/reg-event-db
 ::update-metadata
 (fn [db [_]]
   (let [panels      (into {}
                           (for [[k v] (get db :panels)
                                 :when (and (not (cstr/includes? (str k) "query-preview"))
                                            (not (cstr/includes? (str k) "reco-preview-")))]
                             {k v}))
         lkp         (fn [query-key]
                       (first (remove nil? (for [[k v] panels] (when (some #(= query-key %) (keys (get v :queries))) k)))))
         all-queries (into {}
                           (for [[_ v] panels]
                             (into {}
                                   (for [[kk vv] (get v :queries)]
                                     (when (and (nil? (find vv :vselect)) (nil? (find vv :transform-select))) {kk vv})))))
         data-hash   (hash (get db :data))]
     (when (not (= data-hash (get db :data-hash)))
       (conn/sql-data [:reco-counts]
                      {:select   [:table_name ;; too expensive to run every 5 seconds?????
                                  [[:count [:distinct :combo_hash]] :cnt]]
                       :from     [[:combo_rows :uu2434aa]]
                       :group-by [:table_name]}))
     (doseq [[k v] all-queries] (conn/sql-deep-meta [k] (sql-alias-replace v) (get-in db [:panels (lkp k) :connection-id])))
     (assoc db :data-hash data-hash))))



(re-frame/reg-event-db
 ::update-reco-previews
 (fn [db [_]]
   (let [data-hash   (hash (get db :data))]
     (tapp>>  [:fetch-reco-previews-run])
     (when true ;; (not (= data-hash (get db :data-hash)))
       (conn/sql-data [:reco-counts]
                      {:select   [:table_name ;; too expensive to run every 5 seconds?????
                                  [[:count [:distinct :combo_hash]] :cnt]]
                       :cache?   false
                       :from     [[:combo_rows :uu2434aa]]
                       :group-by [:table_name]}))
     ;;(doseq [[k v] all-queries] (conn/sql-deep-meta [k] (sql-alias-replace v) (get-in db [:panels (lkp k) :connection-id])))
     (assoc db :data-hash data-hash))))




(re-frame/reg-event-db
 ::update-metadata-styles
 (fn [db [_]]
   (let [lkp         (fn [query-key]
                       (first (remove nil?
                                      (for [[k v] (get db :panels)] (when (some #(= query-key %) (keys (get v :queries))) k)))))
         all-queries (into {}
                           (for [[_ v] (get db :panels)]
                             (into {}
                                   (for [[kk vv] (get v :queries)]
                                     (when (and (nil? (find vv :vselect)) (find vv :style-rules)) {kk vv})))))]
     (doseq [[k v] all-queries]
       (conn/sql-style-meta [k] (sql-alias-replace v) (get-in db [:panels (lkp k) :connection-id]))))))

(re-frame/reg-event-db
 ::update-metadata-tabs
 (fn [db [_]]
   (let [tab-rules (into {}
                         (for [[k v] (get db :panels)]
                           (into {} (for [[kk vv] (get v :tab-rules)] {kk {:logic vv :panel-key k}}))))]
     (doseq [[[tab-name rule-name] {:keys [logic panel-key]}] tab-rules]
       (conn/sql-tab-meta [panel-key] [[tab-name rule-name] (sql-alias-replace logic)] panel-key)))))

(re-frame/reg-event-db
 ::update-conditionals
 (fn [db [_]]
   (let [all-condis
         (into {} (for [[_ v] (get db :panels)] (into {} (for [[kk vv] (get v :conditionals)] {kk vv}))))]
     (doseq [[k v] all-condis] (conn/sql-condi-meta [k] (sql-alias-replace v))))))

(re-frame/reg-sub
 ::visible-conditionals?
 (fn [db _]
   (let [visible-panel-keys @(ut/tracked-sub ::relevant-tab-panels-set {})]
     (boolean (some (fn [[_ panel]]
                      (contains? panel :conditionals))
                    (select-keys (get db :panels) visible-panel-keys))))))

(defn iif [v this that] (if (true? v) this that))
(defn eql [this that] (= this that))







(defn sql-explanations-kp [] {[:from 0 0] "table-name" [:from 0 1] "table-alias" [:from] "the table we are selecting from"})

(defn map-value-box
  [s k-val-type & [ww hww]] ;; dupe of a fn inside flows.cljs, but w/o dragging and a bunch more func, including v-tabling. streamline into one fn TDO
  ;;(ut/tapp>> [s k-val-type ww hww (- (/ ww 5) hww)]) 
  (let [;render-values? true
        cols (/ (- ww hww) 9)
        s (if (and (string? s) (> (count s) cols))
            (let [half-cols (Math/floor (/ cols 2))
                  first-half (subs s 0 half-cols)
                  second-half (subs s (- (count s) half-cols))]
              (str first-half "..." second-half))
            s)
        function?      (or (fn? s) (try (fn? s) (catch :default _ false)))]

    (cond (ut/hex-color? s)                        [re-com/h-box :gap "3px" :children
                                                    [[re-com/box :child (str s)]
                                                     [re-com/box :child " " :width "13px" :height "13px" :style
                                                      {:background-color (str s) :border-radius "2px"}]]]
          (ut/is-base64? s)                        [re-com/v-box :size "auto" :gap "10px" :children
                                                    [[re-com/box :align :end :justify :end :style {:opacity 0.5} :child
                                                      (str "**huge base64 string: ~"
                                                           (.toFixed (/ (* (count s) 6) 8 1024 1024) 2)
                                                           " MB")]
                                                     [re-com/box :size "auto" :child
                                                      [:img {:src (str "data:image/png;base64," s)}]]]]
          (or function? (= k-val-type "function"))    [re-com/box :size "auto" :align :end :justify :end :style  {:font-weight 700}
                                                       :child (str (.-name s))] ;; temp
            ;; [re-com/box :size "auto" :align :end :justify :end :style
            ;;  {:word-break "break-all"} :child (str "\"" (.-name s) "\"")]
          (string? s)                              [re-com/box :size "auto" :align :end :justify :end :style
                                                    {:word-break "break-all"} :child (str "\"" s "\"")]
          :else                                    (ut/replacer (str s) #"clojure.core/" ""))))

(defn data-viewer-source
  [obody]
  (let [kps (ut/extract-patterns obody :data-viewer 2) logic-kps
        (first (for [v kps] (let [[_ src] v] src)))] logic-kps))

(re-frame/reg-sub
 ::data-viewer-source
 (fn [db {:keys [block-id selected-view]}]
   (let [view (get-in db [:panels block-id :views selected-view])
         solver-view (get @db/solver-fn-lookup [:panels block-id selected-view])
         dvs (data-viewer-source view)]
     (or solver-view dvs))))

(defn nested-search [data search-str]
  (letfn [(match? [x]
            (and (some? x)
                 (cstr/includes? (cstr/lower-case (str x))
                                 (cstr/lower-case search-str))))

          (search-nested [obj]
            (cond
              (map? obj)
              (let [searched (into {} (keep (fn [[k v]]
                                              (let [result (search-nested v)]
                                                (when (or (match? k) result)
                                                  [k (or result v)]))))
                                   obj)]
                (if (or (seq searched) (match? obj))
                  (if (empty? searched) obj searched)
                  nil))

              (vector? obj)
              (let [searched (into [] (keep search-nested obj))]
                (if (or (seq searched) (match? obj))
                  (if (empty? searched) obj searched)
                  nil))

              (match? obj)
              obj

              :else nil))]

    (search-nested data)))





(declare map-boxes2*)

(defn map-boxes2 [data block-id selected-view keypath kki init-data-type & [draggable? key-depth inner?]]
  [reecatch
   (let [keypath-in (conj keypath kki)
         searcher (get @map-box-searchers [block-id selected-view keypath kki])
         data (if (nil? data) {:error "no data, bub!"} data)
         ;data (if (ut/ne? searcher)
         ;       (nested-search data searcher)
         ;       data)
         open? (get-in @opened-boxes [block-id keypath-in kki])
         page (get @map-boxes-pages [block-id selected-view keypath kki] 1)
         cache-key (pr-str [data block-id selected-view keypath kki page init-data-type draggable? key-depth open?])
         react! [@opened-boxes @opened-boxes-code]
         cache (get @ut/map-boxes-cache cache-key)]
       ;;(swap! ut/map-boxes-cache-hits update cache-key (fnil inc 0))
     (if false cache
         (let [res (map-boxes2* data block-id selected-view keypath kki init-data-type draggable? key-depth inner?)]
             ;;(swap! ut/map-boxes-cache assoc cache-key res)
           res)))])

;;(ut/tapp>> [:map-boxes-cache-hits @ut/map-boxes-cache-hits])
;;(ut/tapp>> [:map-boxes-cache-hits2 (ut/distribution ut/map-boxes-cache-hits 0.33)]) 

;;; [map-boxes2 x panel-key selected-view [] :output nil ] 

(re-frame/reg-event-db
 ::drill-down-data-viewer-view
 (fn [db [_ block-id selected-view kpclover]]
   (let [new-selected-view (keyword (str (ut/replacer selected-view ":" "") "+"))]
     (-> db
         (assoc-in [:panels block-id :views new-selected-view] [:data-viewer kpclover])
         (assoc-in [:panels block-id :selected-view] new-selected-view)))))

(declare code-box)
(declare edn-code-box)

;(def start 0)  ; the start index of the page
(def limit 20000000)  ; the number of items per page

(defn get-page [iter page limit]
  (if (= page "*")
    iter
    (let [start (* (dec page) limit)]
      (subvec iter start (min (+ start limit) (count iter))))))

(def temp-atom (atom {}))

(defn map-boxes2*
  [data block-id selected-view keypath kki init-data-type & [draggable? key-depth inner?]] ;; dupe of a fn inside
  ;;(tapp>> [:block-id block-id data])
  (if (<= (count keypath) (or key-depth 6))
    (let [sql-explanations (sql-explanations-kp) ;; turn into general key annotations as some point with KP lookups?
          ;flow-name (ut/data-typer data)
          react! [@opened-boxes @opened-boxes-code]
          ;;;_ (tapp>>  [:kki (str keypath) kki])
          data (if (or (string? data) (number? data) (boolean? data)) [data] data)
          base-type-vec? (or (vector? data) (list? data))
          start (get @map-boxes-pages [block-id selected-view keypath kki] 1)
          ;;iter (if base-type-vec? (range (count data)) (keys data))
          iter (if base-type-vec?
                 (get-page (vec (range (count data))) start limit)
                 (get-page (vec (keys data)) start limit))
          ;total-items (if base-type-vec? (count data) (count (keys data)))
          ;total-pages (Math/ceil (/ total-items (float limit)))
          [h w] (if (vector? kki) kki @(ut/tracked-subscribe [::size block-id]))
         ; h (if inner? 300 h)
         ; w (if inner? 300 w)
          ;;_ (tapp>> [:sizes h w block-id])
          ;;current-page (inc (/ start limit))
          ;current-page (quot start limit)
          ;current-page (/ start limit)
          ;iter (vec (take 4 iter))
          clover-kp-block? (and (keyword? block-id) (cstr/includes? (str block-id) "/"))
          dvs @(ut/tracked-sub ::data-viewer-source {:block-id block-id :selected-view selected-view})
          source (if clover-kp-block?
                   block-id
                   dvs)
          source-kp (when (vector? source) (last source))
          ;;_ (tapp>> [:dv source source-kp])
          ;;_ (tapp>>  [:dv-source source (str source-kp)])
          source-clover-key? (and (keyword? source) (cstr/includes? (str source) "/"))
          source-app-db? (try (= (first source) :app-db) (catch :default _ false))
         ;;; _ (ut/tapp>> [:data-viewer-source (str source) (str dvs) source-clover-key? clover-kp-block? source-app-db?])
          font-size "inherit" ;;"11px"
          draggable? false ;; override bs TODO
          dcolors @(ut/tracked-sub ::conn/data-colors {})
          non-canvas? (nil? block-id)
          dggbl (if non-canvas? draggable-stub draggable)
          inner-w (- (* w db/brick-size) 24)
          inner-w2 (if inner?
                     (- (first inner?) (* (last inner?) 1.2))
                     (- inner-w 21))
          inner-h (- (* h db/brick-size) 90)
          main-boxes
          [(if inner? ;; dont want to double-bag virtual v-boxes, shit gets weird
             re-com/v-box
             vbunny/virtual-v-box)
           ;re-com/v-box ;(if cells? re-com/h-box re-com/v-box)   
           :width (when (not inner?) (px inner-w))
           :height (when (not inner?) (px inner-h))
           :attr {:id (str block-id selected-view)}
           ;:size "1" 
           ;:padding "5px" 
           ;:justify :between 
           ;:align :end
           ;:gap "2px" 
;           :size "auto"
           :style
           {:color       "black"
            :padding "5px"
            ;:border "1px solid yellow"
            :font-family (theme-pull :theme/base-font nil)
            :font-size   font-size ; "11px"
            :font-weight 500}
           :children (let [sel-on-click-fn (fn [param-kp k-val]
                                             (ut/tracked-dispatch-sync [::conn/click-parameter param-kp k-val]))
                           sel-on-context-fn (fn [multi-param-kp k-val]
                                               (ut/tracked-dispatch [::conn/cell-click-parameter multi-param-kp k-val]))
                           children-vec (conj
                                         (vec (for [kk iter] ;; (keys data)
                                                (let [k-val      (get-in data [kk])
                                                      k-val-type (or init-data-type (ut/data-typer k-val))
                                                      in-body?   true ;(ut/contains-data? only-body k-val)
                                                      hovered?   false ;(ut/contains-data? mat-hovered-input k-val)
                                                      border-ind (if in-body? "solid" "dashed")
                                                      ;;_ (when (= block-id :block-6245) (tapp>>  [:kk kk k-val-type k-val data]))
                                                      val-color  (get dcolors k-val-type (theme-pull :theme/editor-outer-rim-color nil))
                                                      keypath-in (conj keypath kk)
                                                      clover-kp  (when source-clover-key?
                                                                   (edn/read-string (str source ">" (cstr/replace (cstr/join ">" keypath-in) ":" ""))))
                                                          ;;  _ (ut/tapp>> [:clover-kp clover-kp (str keypath-in)])
                                                      keystyle   {:background-color (if hovered? (str val-color 66) "#00000000")
                                                                  :color            val-color
                                                                  :border-radius    "12px"
                                                                  :border           (str "3px " border-ind " " val-color)}
                                                      valstyle   {:background-color (if hovered? (str val-color 22) "#00000000") ;"#00000000"
                                                                  :color            val-color
                                                                  :border-radius    "12px"
                                                                  :border           (str "3px " border-ind " " val-color 40)}
                                                      kp-clover (cond source-clover-key? clover-kp
                                                                      source-app-db? [:app-db (vec  (into source-kp keypath-in))]
                                                                      :else [:get-in [source keypath-in]])
                                                      single-val? (= (try (count k-val) (catch :default _ 1)) 1)
                                                      open? (or (get-in @opened-boxes [block-id keypath-in kki] false)
                                                                single-val?)
                                                      symbol-str (cond (= k-val-type "map") "{}"
                                                                       (= k-val-type "vector") "[]"
                                                                       (= k-val-type "list") "()"
                                                                       (= k-val-type "set") "#{}"
                                                                       :else "[]")
                                                      header-gap (max (* (count (str kk)) 11) 90)
                                                      code? (get-in @opened-boxes-code [block-id keypath-in kki] false)
                                                      ;child-keys (try (count (keys k-val)) (catch :default _ 1))
                                                      ;recur-sizes (apply + (for [[k v] k-val] (if (or (map? v) (vector? v) (set? v) (list? v)) 90 45)))
                                                      sizes (fn [k-val] ;25
                                                              (try
                                                                (cond
                                                                    ;(nil? k-val) 0
                                                                    ;(empty? k-val) 0
                                                                    ;(keyword? k-val) 36
                                                                    ;(string? k-val) 36
                                                                  (or (vector? k-val)
                                                                      (map? k-val))
                                                                  (+ 10 (apply + (map (fn [[k v]]
                                                                                        (if (or (seq? v) (map? v))
                                                                                          65 ;(try (* (count v) 65) (catch :default _ 65))
                                                                                          35))
                                                                                      k-val)))
                                                                  ;; (vector? k-val) (+ 10 (apply + (map (fn [v]
                                                                  ;;                                       (if (and true
                                                                  ;;                                      ;(> (count v) 1) 
                                                                  ;;                                                (or (seq? v) (map? v)))
                                                                  ;;                                         65 35)) k-val)))
                                                                  :else 35)
                                                                (catch :default _ 0)))
                                                      ;open-height (sizes k-val)
                                                      open-kps (when open? ;;(and open? (not single-val?))
                                                                 (vec (for [kp (ut/keypaths (select-keys @opened-boxes [block-id]))
                                                                            :when (true? (get-in @opened-boxes kp))] (second kp))))
                                                      open-kps (filter #(cstr/starts-with? (str %) (cstr/replace (str keypath-in)  "]" "")) open-kps)
                                                      rem-smaller (fn [smaller-kp larger-kp]
                                                                    (if (every? true? (map #(= %1 %2) smaller-kp (take (count smaller-kp) larger-kp)))
                                                                      (subvec larger-kp (count smaller-kp))
                                                                      []))
                                                      open-vals (distinct (for [kps open-kps
                                                                                :let [kps2  (rem-smaller keypath-in kps)
                                                                                      vvv (get-in k-val kps2)]
                                                                                :when (ut/ne? vvv)] vvv))
                                                      ;child-open-sizes (- (apply + (for [e open-vals] (sizes e))) 0)
                                                      child-open-sizes (* (apply + (for [e open-vals] (try (count e) (catch :default _ 1)))) 36)
                                                      ;; _ (when (and open? (= (count keypath-in) 1)) 
                                                      ;;     (tapp>> [:child-open-sizes keypath-in open? child-open-sizes open-height open-kps  (str (for [e open-vals] (sizes e)))]))
                                                     ;; open-height (+ child-open-sizes open-height)
                                                      ;; _ (tapp>> [(str [block-id keypath-in kki]) (for [kps open-kps
                                                      ;;                                                  :let [kps2  (vec (rest kps))
                                                      ;;                                                        vvv (get-in k-val kps2)]
                                                      ;;                                                  :when (ut/ne? vvv)] (sizes vvv))])
                                                      ;; _ (tapp>> [:open-kps open-kps (for [kps open-kps
                                                      ;;                                     :let [kps2  (vec (rest kps))
                                                      ;;                                           vvv (get-in k-val kps2)]
                                                      ;;                                      ] vvv) k-val])
                                                      ;; _ (try (tapp>> [(str [block-id keypath-in kki]) open-kps (try (keys k-val) (catch :default _ -1))
                                                      ;;                 ;;(for [kvp (ut/kvpaths k-val)] (into keypath-in kvp))
                                                      ;;                 ]) (catch :default _ nil))

                                                      ;open-height (* child-keys 45)
                                                      ;; _ (when true ;;(= (count keypath-in) 1) 
                                                      ;;     (tapp>> {:kv (if (try (seq k-val) (catch :default  _ false)) 
                                                      ;;                    (for [kp (ut/kvpaths k-val)] 
                                                      ;;                      [kp (try (count (get-in k-val kp)) (catch :default _ 1))])
                                                      ;;                    []) 
                                                      ;;              :open open-kps }))
                                                      ;_ (tapp>> [:stats @opened-boxes])
                                                      hhh (cond

                                                           ;; (and open? (= (count keypath-in) 1)) (sizes k-val)


                                                            (and open? (or
                                                                        (vector? k-val)
                                                                        (map? k-val)))
                                                            ;(max (* (count k-val) 35) child-open-sizes)
                                                            ;child-open-sizes
                                                            (max (sizes k-val) child-open-sizes 35)


                                                            ;(and open? (empty? open-kps) (seq? k-val)) (* (count k-val) 25)
                                                            ;open? (max child-open-sizes (sizes k-val) 100)
                                                            ;single-val? 35
                                                            ;single-val? (sizes k-val)
                                                              ;(try (and (= (count k-val) 1) (or (vector? k-val) (seq? k-val))) (catch :default _ false)) 62
                                                            (map? k-val) 65
                                                            (vector? k-val) 65
                                                              ;single-val? 65

                                                            :else 35)

                                                      hhh (+ hhh (get-in @opened-boxes-long [block-id keypath-in kki] 0))]

                                                  [inner-w hhh
                                                   ^{:key (str block-id keypath kki kk)}
                                                   [re-com/box
                                                    :width (when (not inner?) (px inner-w2))
                                                    :height (when (not inner?) (px hhh))
                                                    :style {:overflow "hidden" ;; (if (not inner?) "auto" "hidden")
                                                            ;:border "1px solid lime"
                                                            }
                                                    :size "none"
                                                    ;:size "auto"
                                                    :child
                                                    (cond
                                                      (or (= k-val-type "map")
                                                          (= k-val-type "list")
                                                              ;(= k-val-type "set")
                                                          (= k-val-type "rowset")
                                                          (= k-val-type "vector"))
                                                      ^{:key (str block-id keypath kki kk k-val-type)}

                                                      (let []

                                                        (cond (and open? (not code?))
                                                              [re-com/h-box
                                                               :width (px inner-w2)
                                                                                                                             ;:height (px hhh)
                                                                                                                             ;:size "none"
                                                                                                                             ;:size  "none"
                                                               :justify :between
                                                               :children
                                                               [(when (and open?
                                                                           (= (count keypath-in) 1)
                                                                           (not (= (count k-val) 1)))
                                                                  [re-com/box
                                                                   :child " "
                                                                   :attr {:on-click #(swap! opened-boxes-long assoc-in [block-id keypath-in kki]
                                                                                            (+ (get-in @opened-boxes-long [block-id keypath-in kki] 0) 100))}
                                                                   :width "13px" :height "13px"
                                                                   :style {:position "absolute"
                                                                           :cursor "pointer"
                                                                           :border-radius "24px" ;;"8px 8px 8px 24px"
                                                                           :background-color (str val-color)
                                                                           :bottom 6 :left 6}])
                                                                [dggbl ;;draggable
                                                                 {:h         4
                                                                  :w         6
                                                                  :root      [0 0]
                                                                  :drag-meta {:type        :viewer-pull
                                                                              :param-full  ;[:box :style {:font-size "17px"} :child
                                                                              [:data-viewer
                                                                               kp-clover];]
                                                                              :param-type  k-val-type
                                                                              :param-table :what
                                                                              :param-field :what}} "meta-menu"
                                                                 ^{:key (str block-id keypath kki kk k-val-type 1)}
                                                                 [re-com/v-box
                                                                  :min-width (px header-gap) ;"110px"
                                                                                                                                ;:width (px inner-w2)
                                                                  :children
                                                                  [^{:key (str block-id keypath kki kk k-val-type 124)}
                                                                   [re-com/box
                                                                    ;:attr {:on-double-click #(ut/tracked-dispatch [::drill-down-data-viewer-view block-id selected-view kp-clover])}
                                                                    :child (str kk)]
                                                                   ^{:key (str block-id keypath kki kk k-val-type 134)}
                                                                   [re-com/h-box
                                                                    :gap "6px"
                                                                                                                                  ;:width "100%"
                                                                                                                                  ;:size "auto"
                                                                    :children [[re-com/box
                                                                                :child (str k-val-type)]
                                                                               (when (> (count k-val) 1)
                                                                                 ^{:key (str block-id keypath kki kk k-val-type 156)}
                                                                                 [re-com/box :style {:opacity 0.6} :child (str "(" (count k-val) ")")])]
                                                                    :style
                                                                    {:opacity     0.45
                                                                     :font-size   font-size ; "9px"
                                                                     :padding-top "7px"}]


                                                                   [re-com/h-box
                                                                                                                                                                                                         ;:align :end :justify :end 
                                                                    :children [(when (not (= (count k-val) 1))
                                                                                 [re-com/box :child symbol-str
                                                                                  :padding "2px 8px 2px 8px"
                                                                                                                                                                                                                                                  ;:width "30px"
                                                                                  :attr {:on-click #(swap! opened-boxes-code assoc-in [block-id keypath-in kki] (not code?))}
                                                                                  :style
                                                                                  {:opacity     0.45
                                                                                   :font-family (theme-pull :theme/monospaced-font nil)
                                                                                   :font-weight 700
                                                                                   :border-radius "12px" ;:padding "3px"
                                                                                   :background-color (when code? (str (get keystyle :color (theme-pull :theme/editor-outer-rim-color nil)) 55))
                                                                                   :cursor "pointer"
                                                                                   :font-size   font-size ; "9px"
                                                                                                                                                                                                                                                       ;:padding-top "7px"
                                                                                   }])

                                                                               (when (not (= (count k-val) 1))
                                                                                 [re-com/box :child (str "   - ")
                                                                                  :padding "2px 8px 2px 8px"
                                                                                  :width "20px"
                                                                                  :attr {:on-click
                                                                                         #(let [child-paths (filter (fn [x]
                                                                                                                      (and (= (first x) block-id)
                                                                                                                           (cstr/starts-with? (str (second x))
                                                                                                                                              (cstr/replace (str keypath-in) "]" ""))))
                                                                                                                    (ut/kvpaths @opened-boxes))]
                                                                                            (doseq [kkp child-paths]
                                                                                              (swap! opened-boxes ut/dissoc-in  kkp)
                                                                                              (swap! opened-boxes-long ut/dissoc-in  kkp)
                                                                                              (swap! opened-boxes-code ut/dissoc-in  kkp))
                                                                                                                                                                                                                             ;(swap! opened-boxes assoc-in [block-id keypath-in kki] false)
                                                                                            )}
                                                                                  :style
                                                                                  {:opacity     0.45
                                                                                                                                                                                                      ;:padding-left "2px"
                                                                                   :border-radius "12px" ;:padding "3px"
                                                                                   :background-color (str (get keystyle :color (theme-pull :theme/editor-outer-rim-color nil)) 55)
                                                                                   :cursor "pointer"
                                                                                   :font-size   font-size ; "9px"
                                                                                                                                                                                                            ;:padding-top "7px"
                                                                                   }])]]]
                                                                  :padding "8px"]]
                                                                [map-boxes2
                                                                 (if (= k-val-type "list")
                                                                   (ut/lists-to-vectors k-val)
                                                                   k-val)
                                                                 block-id selected-view keypath-in kk nil draggable? key-depth [inner-w2 header-gap]]] :style
                                                               keystyle]


                                                              (and open? code?)
                                                              [re-com/h-box
                                                               :style keystyle
                                                               :width "100%"
                                                               :children [^{:key (str block-id keypath kki kk k-val-type 1)}
                                                                          [re-com/v-box :min-width (px header-gap) ;"110px"
                                                                           :children
                                                                           [^{:key (str block-id keypath kki kk k-val-type 124)}
                                                                            [re-com/box
                                                                             ;:attr {:on-double-click #(ut/tracked-dispatch [::drill-down-data-viewer-view block-id selected-view kp-clover])}
                                                                             :child (str kk)]
                                                                            ^{:key (str block-id keypath kki kk k-val-type 134)}

                                                                            [re-com/h-box
                                                                             :gap "6px"
                                                                             :children [[re-com/box :child (str k-val-type)]
                                                                                        (when (> (count k-val) 1)
                                                                                          ^{:key (str block-id keypath kki kk k-val-type 156)}
                                                                                          [re-com/box :style {:opacity 0.6} :child (str "(" (count k-val) ")")])]
                                                                             :style
                                                                             {:opacity     0.45
                                                                              :font-size   font-size ; "9px"
                                                                              :padding-top "7px"}]

                                                                                ;; [re-com/box :child (str k-val-type) :style
                                                                                ;;  {:opacity     0.45
                                                                                ;;   :font-size   font-size ; "9px"
                                                                                ;;   :padding-top "7px"}]
                                                                                ;; (when (> (count k-val) 1)
                                                                                ;;   ^{:key (str block-id keypath kki kk k-val-type 156)}
                                                                                ;;   [re-com/box :style {:opacity 0.45} :child (str "(" (count k-val) ")")])

                                                                            [re-com/h-box
                                                                                 ;:align :end :justify :end
                                                                             :children [(when (not (= (count k-val) 1))
                                                                                          [re-com/box :child symbol-str
                                                                                           :padding "2px 8px 2px 8px"
                                                                                            ;:width "30px"
                                                                                           :attr {:on-click #(swap! opened-boxes-code assoc-in [block-id keypath-in kki] (not code?))}
                                                                                           :style
                                                                                           {:opacity     0.45
                                                                                            :font-family (theme-pull :theme/monospaced-font nil)
                                                                                            :font-weight 700
                                                                                             ;:border-radius "12px" :padding "3px"
                                                                                             ;:background-color (str (get keystyle :color (theme-pull :theme/editor-outer-rim-color nil)) 55)
                                                                                            :cursor "pointer"
                                                                                            :font-size   font-size ; "9px"
                                                                                    ;:padding-top "7px"
                                                                                            }])

                                                                                        (when (not (= (count k-val) 1))
                                                                                          [re-com/box :child (str "   - ")
                                                                                           :padding "2px 8px 2px 8px"
                                                                                           :width "20px"
                                                                                           :attr {:on-click #(swap! opened-boxes assoc-in [block-id keypath-in kki] false)}
                                                                                           :style
                                                                                           {:opacity     0.45
                                                                                           ;:padding-left "2px"
                                                                                            :border-radius "12px" ;:padding "3px"
                                                                                            :background-color (str (get keystyle :color (theme-pull :theme/editor-outer-rim-color nil)) 55)
                                                                                            :cursor "pointer"
                                                                                            :font-size   font-size ; "9px"
                                                                                    ;:padding-top "7px"
                                                                                            }])]]]:padding
                                                                           "8px"]

                                                                          [re-com/v-box :children [;[re-com/box :child "x"
                                                                                                       ; :attr {:on-click #(swap! opened-boxes-code assoc-in [block-id keypath-in kki] false)}]
                                                                                                       ;[re-com/box :child (pr-str k-val)]
                                                                                                   [edn-code-box 330 nil (str k-val)]]]]]




                                                              :else
                                                              [re-com/v-box
                                                               :style keystyle
                                                               ;:min-width (px (* (count (str kk)) 11)) ;"110px"
                                                               :width "100%"
                                                               ;:width (px inner-w2)
                                                               :children
                                                               [^{:key (str block-id keypath kki kk k-val-type 124)}
                                                                [dggbl ;;draggable
                                                                 {:h         4
                                                                  :w         6
                                                                  :root      [0 0]
                                                                  :drag-meta {:type        :viewer-pull
                                                                              :param-full  ;[:box :style {:font-size "17px"} :child
                                                                              [:data-viewer
                                                                               kp-clover];]
                                                                              :param-type  k-val-type
                                                                              :param-table :what
                                                                              :param-field :what}} "meta-menu"
                                                                 [re-com/box
                                                                  ;:attr {:on-double-click #(ut/tracked-dispatch [::drill-down-data-viewer-view block-id selected-view kp-clover])}
                                                                  :child (str kk)]]
                                                                ^{:key (str block-id keypath kki kk k-val-type 134)}
                                                                [re-com/h-box
                                                                 :width "100%"
                                                                 :justify :between
                                                                 :children
                                                                 [[re-com/box :child (str k-val-type) :style
                                                                   {:opacity     0.45
                                                                    :font-size   font-size ; "9px"
                                                                    :padding-top "7px"}]
                                                                  [re-com/h-box
                                                                   ;:justify :between 
                                                                   ;:align :center 
                                                                   ;:justify :end
                                                                   ;:width "100%"
                                                                   :gap "4px"
                                                                   :children (let [preview-len (/ (* inner-w2 0.3) 4)]
                                                                               (if (> (count k-val) 1)
                                                                                 [[re-com/box :child (str (subs (str k-val) 0 preview-len)
                                                                                                          (when (> (count (str k-val)) preview-len) "... "))
                                                                                   :style {:font-size "12px" :overflow "hidden"}]
                                                                                  [re-com/box :child (str (count k-val) " items +")
                                                                                   :attr {:on-click #(swap! opened-boxes assoc-in [block-id keypath-in kki] true)}
                                                                                   :padding "2px 8px 2px 8px"
                                                                                   :style
                                                                                   {:opacity     0.45
                                                                                    :border-radius "12px"
                                                                                    :background-color (str (get keystyle :color (theme-pull :theme/editor-outer-rim-color nil)) 55)
                                                                                    :cursor "pointer"
                                                                                    :font-size   font-size ; "9px"
                                                                                    :padding-top "7px"}]]
                                                                                 [[re-com/box :child "empty"
                                                                                   :padding "2px 8px 2px 8px"
                                                                                   :style
                                                                                   {:opacity     0.45
                                                                                    :border-radius "12px"
                                                                                    :background-color (str (get keystyle :color (theme-pull :theme/editor-outer-rim-color nil)) 55)
                                                                                    :cursor "pointer"
                                                                                    :font-size   font-size ; "9px"
                                                                                    :padding-top "7px"}]]))]]]] :padding
                                                               "8px"]))

                                                      (or ;(= k-val-type "vector")
                                                              ;(= k-val-type "list")
                                                              ;(= k-val-type "function")
                                                              ;(= k-val-type "rowset")
                                                       (= k-val-type "jdbc-conn")
                                                       (= k-val-type "render-object"))
                                                      ^{:key (str block-id keypath kki kk k-val-type 2)}
                                                      [re-com/h-box :style {:border-radius "12px" :border "3px solid black"} :children
                                                       [[dggbl ;;draggable
                                                         {:h         4
                                                          :w         6
                                                          :root      [0 0]
                                                          :drag-meta {:type        :viewer-pull
                                                                      :param-full  ;[:box :style {:font-size "17px"}
                                                                                        ;:child
                                                                      [:data-viewer
                                                                       kp-clover];]
                                                                      :param-type  k-val-type
                                                                      :param-table :what
                                                                      :param-field :what}} "meta-menu"         ;block-id
                                                         ^{:key (str block-id keypath kki kk k-val-type 3)}
                                                         [re-com/v-box :min-width (px header-gap) ;"110px"
                                                          :style
                                                          {;:cursor (when draggable? "grab") ;;; skeptical, ryan 5/24/33
                                                           }:children
                                                          (if (and (= k-val-type "list") (= (ut/data-typer (first k-val)) "function"))
                                                            [^{:key (str block-id keypath kki kk k-val-type 4)}
                                                             [re-com/h-box :style {:margin-top "4px"} :gap "5px" :children
                                                              [[re-com/box
                                                                ;:attr {:on-double-click #(ut/tracked-dispatch [::drill-down-data-viewer-view block-id selected-view kp-clover])}
                                                                :child (str kk)
                                                                :style {:opacity 0.25}]
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
                                                               [re-com/box :style {:opacity 0.45} :child (str "(" (count k-val) ")")])]

                                                            [(when true ;(not (get sql-explanations keypath ""))
                                                               ^{:key (str block-id keypath kki kk k-val-type 7)}
                                                               [re-com/box
                                                                ;:attr {:on-double-click #(ut/tracked-dispatch [::drill-down-data-viewer-view block-id selected-view kp-clover])}
                                                                :child (str kk)])
                                                             (when true ; (not (get sql-explanations keypath ""))
                                                               ^{:key (str block-id keypath kki kk k-val-type 8)}
                                                               [re-com/box :child (str (when (= (count k-val) 0) "empty ") k-val-type)
                                                                :style
                                                                {:opacity     0.45
                                                                 :font-size   font-size ; "9px"
                                                                 :padding-top "7px"}])
                                                             (when (ut/ne? (get sql-explanations keypath ""))
                                                               ^{:key (str block-id keypath kki kk k-val-type 82)}
                                                               [re-com/md-icon-button :md-icon-name "zmdi-info" :tooltip
                                                                (str "FOOO" (get sql-explanations keypath "")) :style
                                                                {:font-size "16px" :cursor "pointer" :opacity 0.5 :padding "0px" :margin-top "-1px"}])]) :padding
                                                          "8px"]]
                                                        [map-boxes2
                                                         (if (= k-val-type "rowset")
                                                           (zipmap (iterate inc 0) (take 10 k-val))
                                                           (zipmap (iterate inc 0) k-val))
                                                         block-id selected-view keypath-in kk nil draggable? key-depth [inner-w2 header-gap]]] :style keystyle]
                                                      :else
                                                      ^{:key (str block-id keypath kki kk k-val-type 9)}
                                                      [re-com/h-box
                                                       :width "100%"
                                                       :children
                                                       [^{:key (str block-id keypath kki kk k-val-type 10)}
                                                        [re-com/h-box :gap "6px" :children
                                                         [[dggbl ;;draggable
                                                           {:h         4
                                                            :w         6
                                                            :root      [0 0]
                                                            :drag-meta {:type        :viewer-pull
                                                                        :param-full  [:box :size "auto" :align :center :justify :center :style
                                                                                      {;:font-size [:auto-size-px
                                                                                                 ;            (cond source-clover-key? clover-kp
                                                                                                 ;                  source-app-db? [:app-db keypath-in]
                                                                                                 ;                  :else [:get-in [source keypath-in]])]
                                                                                       }
                                                                                      :child
                                                                                      [:string (cond source-clover-key? clover-kp
                                                                                                     source-app-db? [:app-db keypath-in]
                                                                                                     :else [:get-in [source keypath-in]])]]
                                                                        :param-type  k-val-type
                                                                        :param-table :what
                                                                        :param-field :what}} "meta-menu" ;block-id
                                                           ^{:key (str block-id keypath kki kk k-val-type 11)}
                                                           [re-com/box
                                                            :child (str kk)
                                                            ;;:attr {:on-double-click #(ut/tracked-dispatch [::drill-down-data-viewer-view block-id selected-view kp-clover])}
                                                            :style
                                                            {;:cursor (when draggable? "grab")  
                                                             }]]
                                                          (when true ;(not (get sql-explanations (vec (conj keypath kk)) ""))
                                                            ^{:key (str block-id keypath kki kk k-val-type 12)}
                                                            [re-com/box :child (str k-val-type) :style
                                                             {:opacity    0.45
                                                              :font-size  font-size ;"9px"
                                                              :font-style (if (= k-val-type "function") "italic" "normal")}])
                                                          (when (get sql-explanations (vec (conj keypath kk)) "")
                                                            ^{:key (str block-id keypath kki kk k-val-type 822)}
                                                            [re-com/box :style {:opacity 0.45} :child (str (get sql-explanations (vec (conj keypath kk)) ""))])]]
                                                        ^{:key (str block-id keypath kki kk k-val-type 13)}
                                                        (let [param-kp [selected-view :data-viewer] ;; [block-id selected-view] 
                                                              multi-param-kp [(keyword (str (ut/replacer (str selected-view) #":" "") ".*")) :data-viewer]
                                                              clicked? (true? (= (or k-val "_nil_")
                                                                                 @(ut/tracked-sub ::conn/clicked-parameter-alpha {:keypath param-kp})
                                                                                 ;@(ut/tracked-subscribe [::conn/clicked-parameter param-kp])
                                                                                 ))
                                                              multi-clicked? (true? (some #(= k-val %)  (into []
                                                                                                              @(ut/tracked-sub ::conn/clicked-parameter-alpha {:keypath multi-param-kp})
                                                                                                              ;@(ut/tracked-subscribe [::conn/clicked-parameter multi-param-kp])
                                                                                                              )))
                                                              ;; sel-on-click-fn (fn [param-kp k-val]
                                                              ;;               (ut/tracked-dispatch-sync [::conn/click-parameter param-kp k-val]))
                                                              ;; sel-on-context-fn (fn [multi-param-kp k-val]
                                                              ;;                 (ut/tracked-dispatch [::conn/cell-click-parameter multi-param-kp k-val]))
                                                              ]
                                                              ;;(keyword (cstr/replace (str block-id "." selected-view)  ":" ""))]

                                                          [re-com/box
                                                           :size "auto"
                                                           :align :end
                                                           :justify :end
                                                           :child [map-value-box k-val k-val-type inner-w2 header-gap]
                                                           :attr {:on-click        #(sel-on-click-fn param-kp k-val)
                                                                  :on-context-menu #(sel-on-context-fn multi-param-kp k-val)}
                                                           :style (merge
                                                                   {;:font-weight 500
                                                                    :border (when clicked? (str "1px solid " val-color))
                                                                    :border-radius (when (or multi-clicked? clicked?) "6px")
                                                                    :color (when (or multi-clicked? clicked?) val-color)
                                                                    :filter (when (or multi-clicked? clicked?) "brightness(200%)")
                                                                    :background-color (when clicked? (str val-color 11))
                                                                    :cursor "pointer"
                                                                    :line-height  "1.2em"
                                                                    :padding-left "5px"}
                                                                   (when multi-clicked?
                                                                     {:color      val-color
                                                                      :background (str "repeating-linear-gradient(45deg, "
                                                                                       val-color
                                                                                       "55, "
                                                                                       val-color
                                                                                       "55 10px, "
                                                                                       val-color
                                                                                       "33 10px, "
                                                                                       val-color
                                                                                       "33 20px)")}))])]
                                                       :justify :between
                                                       :padding "5px"
                                                       :style valstyle])]])))

                                        ;;  (when (> total-pages 1)
                                        ;;    (let [current-page (dec (get @map-boxes-pages [block-id selected-view keypath kki] 1))]
                                        ;;      [re-com/v-box
                                        ;;       :style {:color "white"}
                                        ;;       :children [[re-com/h-box
                                        ;;                   :justify :between :align :center
                                        ;;                   :children (for [p (range total-pages)
                                        ;;                                   :let [p (inc p)
                                        ;;                                         selected? (= (dec p) current-page)]]
                                        ;;                               [re-com/box
                                        ;;                                :attr {:on-click #(swap! map-boxes-pages assoc [block-id selected-view keypath kki] p)}
                                        ;;                                :style {:padding-left "5px"
                                        ;;                                        :cursor "pointer"
                                        ;;                                        :background-color (when selected? "darkcyan")
                                        ;;                                        :padding-right "5px"}
                                        ;;                                :child (str p)])]
                                        ;;                                       ;[re-com/box :child (str current-page)]
                                        ;;                  ]])
                                        ;;    )
                                         )
                           non-v-children (mapv last children-vec)
                          ;;  _ (tapp>> [:mb2 (str inner?) (str keypath) (hash children-vec) (hash non-v-children) (count children-vec) (count non-v-children)])
                          ;;  _ (when (= keypath [])
                          ;;      (tapp>> [:diff (cdata/diff @temp-atom non-v-children)])
                          ;;      (reset! temp-atom non-v-children))
                           ]
                       (if inner?
                         (vec non-v-children)
                         (vec children-vec)))]]

      (if (= keypath [])
        (let [k-val-type (ut/data-typer data)
              nin?       (not (= block-id :inline-render))
              val-color  (get dcolors k-val-type)]
          [re-com/v-box :children
           [[re-com/v-box :style {:word-wrap "break-word" :overflow-wrap "break-word"} :children
             [(when nin?
                [re-com/v-box :padding "6px" :children
                 [^{:key (str block-id keypath kki 00 k-val-type 00002)}
                  [re-com/h-box
                   :justify :between
                   :children [[re-com/box :child (str k-val-type)]
                                ;[re-com/box :child (str flow-name)]
                                ;[re-com/gap :size "10px"]
                              ;; [re-com/input-text
                              ;;  :src (at)
                              ;;  :model  (get @map-box-searchers [block-id selected-view keypath kki] "")
                              ;;  :width "93%"
                              ;;  :on-change #(swap! map-box-searchers assoc [block-id selected-view keypath kki]
                              ;;                     (let [vv (str %)]
                              ;;                       (if (empty? vv) nil vv)))
                              ;;  :placeholder (str "(search " k-val-type ")")
                              ;;  :change-on-blur? false
                              ;;  :style {:text-decoration  (when (ut/ne? (get @map-box-searchers [block-id selected-view keypath kki] "")) "underline")
                              ;;          :color            "inherit"
                              ;;          :border           "none"
                              ;;          :outline          "none"
                              ;;          :text-align       "center"
                              ;;          :background-color "#00000000"}]
] ;(str k-val-type)
                   :align :end :style
                   {:opacity       0.45
                    :font-size     font-size ;"9px"
                    :margin-top    "-7px"
                    :margin-bottom "-5px"}]]]) main-boxes] :padding "0px"]] :style
           {:font-family   (theme-pull :theme/base-font nil)
            :color         val-color
            :margin-top    (if nin? "0px" "-10px")
            :border-radius "12px"}])
        main-boxes))
    [re-com/v-box
     :size "auto"  :align :center :justify :center
     :margin "5px"
     :style {:border (str  "2px dashed " (theme-pull :theme/editor-outer-rim-color nil))
             :color (theme-pull :theme/editor-outer-rim-color nil)
             :font-size "10px"  :font-weight 700
             :border-radius "14px"}
     :children [[re-com/box :child (str "key depth reached")]
                [re-com/box :child (str "*drag a key out to extend*")]]]))

;; dcolors @(ut/tracked-sub ::conn/data-colors {})



;; (def scrollbar-style
;;   {:scrollbar-width "thin"
;;    ;:scrollbar-color "#4a4a4a #2a2a2a66"
;;    :scrollbar-color (str (str (theme-pull :theme/universal-pop-color nil) 33) "#00000033")
;;    :&::-webkit-scrollbar {:width "10px"}
;;    :&::-webkit-scrollbar-track {:background "#2a2a2a"
;;                                 :border-radius "5px"}
;;    :&::-webkit-scrollbar-thumb {:background "#4a4a4a"
;;                                 :border-radius "15px"}
;;    :&::-webkit-scrollbar-thumb:hover {:background "#6a6a6a"}})

;; (defn virtualized-map-boxes2 [{:keys [text style width height]}]
;;   (let [line-height 19 ;; Assuming each line is 17px high
;;         ;; Ensure text is always a vector
;;         text-lines (if (vector? text)
;;                      text
;;                      (vec (cstr/split text "\n")))
;;         num-visible-lines (int (- (/ height line-height) 1))
;;         total-lines       (+ (count text-lines) num-visible-lines 3)
;;         state (reagent/atom {:start 0 :end num-visible-lines})
;;         handle-scroll (fn [e]
;;                         (let [scroll-top (.. e -target -scrollTop)
;;                               start (int (/ scroll-top line-height))
;;                               end (min total-lines (+ start num-visible-lines))]
;;                           (swap! state assoc :start start :end end)))]
;;     (fn []
;;       (let [{:keys [start end]} @state
;;             ;; Safeguard subvec to handle edge cases
;;             visible-lines (if (and (<= start end) (<= end total-lines))
;;                             (ut/safe-subvec text-lines start end)
;;                             [])
;;             html-content (cstr/join "\n" (map #(.toHtml ansi-converter %) visible-lines))
;;             padding-top (* start line-height)
;;             padding-bottom (* (- total-lines end) line-height)
;;             console-style {:font-family "monospace"
;;                            :font-size "15px"
;;                            :line-height (str line-height "px")
;;                            :padding "15px"
;;                            :white-space "pre"
;;                            :text-shadow "#00000088 1px 0 10px"
;;                            :overflow "auto"}]
;;         [re-com/box
;;          :size "none" :width (str width "px") :height (str height "px")
;;          :style (merge console-style style scrollbar-style)
;;          :attr {:on-scroll handle-scroll}
;;          :child [:div {:style {:font-family "inherit"
;;                                :padding-top (str padding-top "px")
;;                                :padding-bottom (str padding-bottom "px")}
;;                        :dangerouslySetInnerHTML
;;                        {:__html html-content}}]]))))

;; (defn reactive-virtualized-map-boxes2 [{:keys [text] :as props}]
;;   [:f> (with-meta virtualized-map-boxes2
;;          {:key (hash text)})
;;    props])








(defn transform-nested ;; do this on the server?
  [{:keys [data group-key x-key y-key]}]
  (let [groups (group-by group-key data)]
    (vec (map (fn [[group vals]] {:id group :data (vec (map (fn [val] {:x (get val x-key) :y (get val y-key)}) vals))}) groups))))

(defn transform-stacked ;; do this on the server?
  [{:keys [data group-key category-key value-key]}]
  (let [groups (group-by group-key data)]
    (vec (map (fn [[group vals]]
                (merge {group-key group}
                       (apply merge-with + (vec (map (fn [val] {(get val category-key) (get val value-key)}) vals)))))
              groups))))

(defn debounce
  [f delay]
  (let [input  (chan)
        output (chan)]
    (go (loop []
          (let [args (<! input)]
            (>! output args)
            (<! (async/timeout delay))
            (recur)))
        (fn [& args] (go (>! input args)) (go (let [args (<! output)] (apply f args)))))))

(defn nivo-data-check
  [edn-mass try-block]
  (let [data     (get edn-mass :data)
        h        (get edn-mass :height "100%")
        w        (get edn-mass :width "100%")
        no-data? (nil? data)]
    (if no-data?
      [re-com/box :child "no data" :height (if (or (float? h) (integer? h)) (px (js/Math.floor h)) h) :width
       (if (or (float? w) (integer? w)) (px (js/Math.floor w)) w) :style {:font-family (theme-pull :theme/base-font nil)} :size
       "auto" :align :center :justify :center]
      try-block)))

(defn nivo-render-straight
  [edn-mass nivo-fn] ;; nivo-bar/Bar
  (nivo-data-check edn-mass
                   (try [(reagent/adapt-react-class nivo-fn) edn-mass]
                        (catch :default e [re-com/box :child (str "reagent/adapt-react-class nivo-render-straight err: " e)]))))

(defn nivo-render
  [edn-mass nivo-fn panel-key] ;; nivo-line/Line
  (let [data               (get edn-mass :data)
        transformation-map (get edn-mass :transformation-map)
        t-map?             (ut/ne? transformation-map)
        post-data          (if t-map? (st/transform transformation-map data) data)
        has-keys?          (not (nil? (get edn-mass :keys)))
        edn-mass           (assoc edn-mass :data post-data)
        edn-mass           (if (and (not has-keys?) t-map? (get transformation-map :pivot-by))
                             (assoc edn-mass
                                    :keys (vec (cset/difference (set (keys (first post-data)))
                                                                (set (ut/deep-flatten transformation-map)))))
                             edn-mass)
        edn-mass           (assoc edn-mass
                                  :on-click #(let [;m-pair {(keyword (str (name panel-key) "/" k)) v}
                                                   x-key   (get-in edn-mass [:click :x] :x)
                                                   y-key   (get-in edn-mass [:click :y] :y)
                                                   c-key   (get-in edn-mass [:click :c] :c)
                                                   m-pair1 (into {} (for [[k v] (get (js->clj %) "data")] {(keyword k) v}))
                                                   m-pair2 {x-key (if (= nivo-fn nivo-calendar/Calendar)
                                                                    (get (js->clj %) "day")
                                                                    (get (js->clj %) "indexValue"))
                                                            c-key (get (js->clj %) "id")
                                                            y-key (get (js->clj %) "value")}
                                                   m-pair  (into {} (for [[k v] m-pair2] (when (not (nil? v)) {k v})))]
                                               (ut/tapp>> [:nivo-click-params (js->clj %) m-pair])
                                               (doseq [[m1 m2] m-pair]
                                                 (ut/tracked-dispatch [::conn/click-parameter [panel-key m1] m2]))))
        edn-mass           (if (and (= nivo-fn nivo-calendar/Calendar) (not (get edn-mass :to)) (not (get edn-mass :from)))
                             (-> edn-mass
                                 (assoc :to (apply max (map :day (get edn-mass :data))))
                                 (assoc :from (apply min (map :day (get edn-mass :data)))))
                             edn-mass)
        node-size?         (keyword? (get edn-mass :nodeSize))
        edn-mass           (if node-size?
                             (assoc edn-mass :nodeSize #(do (get (walk/keywordize-keys (js->clj %)) (get edn-mass :nodeSize) 14)))
                             edn-mass)
        node-size2?        (and (keyword? (get edn-mass :size)) (= nivo-fn nivo-swarmplot/SwarmPlot))
        edn-mass           (if node-size2?
                             (assoc edn-mass :size #(do (get (walk/keywordize-keys (js->clj %)) (get edn-mass :size) 14)))
                             edn-mass)]
    [re-com/box :style {:overflow "hidden"} :child (nivo-data-check edn-mass [(reagent/adapt-react-class nivo-fn) edn-mass])]))










(re-frame/reg-sub ::get-layout-in
                  (fn [_ [_ panel-key view-key]]
                    (let [view @(ut/tracked-subscribe [::view panel-key view-key])
                          kvs  (first (filter #(= (get-in view %) :layout) (ut/kvpaths view)))
                          kvs1 (assoc kvs (dec (count kvs)) 1)
                          v    (get-in view kvs1)]
                      [(vec (flatten (into [:panels panel-key :views view-key] kvs1))) v]))) ;; returns vec [keypath, layout-map]

(re-frame/reg-event-db ::delete-layout-panel
                       (undoable)
                       (fn [db [_ panel-key view-key layout-key root]]
                         (let [[layout-kp layout-map] @(ut/tracked-subscribe [::get-layout-in panel-key view-key])
                               target-kp              (into layout-kp [:panels layout-key])]
                           (ut/tapp>> [:deleting-layout-shape target-kp layout-key])
                           (ut/dissoc-in db target-kp))))

(re-frame/reg-event-db ::clear-layout-panel
                       (undoable)
                       (fn [db [_ panel-key view-key layout-key root]]
                         (let [[layout-kp layout-map] @(ut/tracked-subscribe [::get-layout-in panel-key view-key])
                               target-kp              (into layout-kp [:panels layout-key])
                               new-kp                 (into layout-kp [:panels (keyword (str "cleared-" (rand-int 1234)))])]
                           (ut/tapp>> [:clear-layout-assignment target-kp layout-key])
                           (-> db
                               (assoc-in new-kp (get-in db target-kp)) ;; insert new blank panel with all settings
                               (ut/dissoc-in target-kp))))) ;; remove old one


(re-frame/reg-event-db ::split-v-layout-panel
                       (undoable)
                       (fn [db [_ panel-key view-key layout-key root processed-layout]]
                         (let [[layout-kp layout-map] @(ut/tracked-subscribe [::get-layout-in panel-key view-key])
                               target-kp              (into layout-kp [:panels layout-key])
                               old                    (get-in db target-kp)
                               fck                    (fn [x] (if (= x 0.99) 1.0 x))
                               rup                    (fn [x] (if (= x 1) 0.99 x))]
                           (ut/tapp>> [:split-v-layout-shape target-kp layout-key])
                           (-> db
                               (ut/dissoc-in target-kp)
                               (assoc-in (into layout-kp [:panels (keyword (str "empty-v-" (rand-int 1234)))])
                                         {:h (rup (/ (fck (get old :h)) 2)) :w (rup (fck (get old :w))) :root (get old :root)})
                               (assoc-in (into layout-kp [:panels (keyword (str "empty-v-" (rand-int 1234)))])
                                         {:h    (rup (/ (fck (get old :h)) 2))
                                          :w    (rup (fck (get old :w)))
                                          :root [(first (get old :root))
                                                 (rup (+ (/ (fck (get old :h)) 2) (second (get old :root))))]})))))

(re-frame/reg-event-db ::split-h-layout-panel
                       (undoable)
                       (fn [db [_ panel-key view-key layout-key root processed-layout]]
                         (let [[layout-kp layout-map] @(ut/tracked-subscribe [::get-layout-in panel-key view-key])
                               target-kp              (into layout-kp [:panels layout-key])
                               old                    (get-in db target-kp)
                               fck                    (fn [x] (if (= x 0.99) 1.0 x))
                               rup                    (fn [x] (if (= x 1) 0.99 x))] ;; "unfuck"
                           (ut/tapp>> [:split-h-layout-shape target-kp layout-key])
                           (-> db
                               (ut/dissoc-in target-kp)
                               (assoc-in (into layout-kp [:panels (keyword (str "empty-h-" (rand-int 1234)))])
                                         {:h    (rup (fck (get old :h)))            ;(/ (get old :h) 2)
                                          :w    (rup (/ (fck (get old :w)) 2))
                                          :root (get old :root)})
                               (assoc-in (into layout-kp [:panels (keyword (str "empty-h-" (rand-int 1234)))])
                                         {:h    (rup (fck (get old :h)))
                                          :w    (rup (/ (fck (get old :w)) 2))
                                          :root [;(first (get old :root))
                                                 (rup (+ (/ (fck (get old :w)) 2) (first (get old :root))))
                                                 (second (get old :root))]})))))

(defn layout
  [panel-key view-key layout-map pw-int ph-int]
  (let [panels (get layout-map :panels {})
        selected-block @(ut/tracked-subscribe [::selected-block])
        scrub-borders? (and (= panel-key selected-block) @(ut/tracked-subscribe [::is-layout-selected-and-scrubber?]))
        editor? @(ut/tracked-subscribe [::editor?])
        p-z @(ut/tracked-subscribe [::zindex panel-key]) ;; 0 ?
        gli @(ut/tracked-subscribe [::get-layout-in panel-key view-key])
        scrubber-on? (get-in @db/scrubbers [panel-key view-key] false)
        brix (fn [x & [mod]] (px (if mod (+ (* db/brick-size x) mod) (* db/brick-size x))))
        layout-style (get layout-map :style {})
        pp (lay/process-layout (get-in gli [1 :panels]) pw-int ph-int)
        hover-key1 @db/pixel-pusher-key
        processed-layout (lay/process-layout panels pw-int ph-int)
        boxes
        (into
         (if scrub-borders?
           [[re-com/h-box :style {:position "fixed" :left 0 :top 25} :children
             (for [i (range pw-int)]
               [re-com/box :child (str i) :align :center :justify :center :style
                {:font-family (theme-pull :theme/base-font nil) :font-weight 200 :opacity 0.6} :width "50px" :height "50px"])]
            [re-com/v-box :style {:position "fixed" :left 0 :top 25} :children
             (for [i (range (- ph-int 1))]
               [re-com/box :child (if (= i 0) " " (str i)) :align :center :justify :center :style
                {:font-family (theme-pull :theme/base-font nil) :font-weight 200 :opacity 0.6} :width "50px" :height "50px"])]]
           [])
         (vec
          (for [[view-ref p] processed-layout]
            (let [h (get p :h 4) ;; if omitted, default to 4
                  w (get p :w 4) ;; if omitted, default to 4
                  z (get p :z 0)
                  hidden? (get p :hidden? false)
                  [x y] (get p :root [0 0]) ;; if omitted, default to root pos upper left
                  orig-key (first (for [[k v] pp :when (= (get v :root) (get p :root))] k))
                  hover-key @db/pixel-pusher-key
                  hovered? (= (last hover-key) orig-key)
                  pstyle (get p :style {}) ;; has overridden style map?
                  left (+ -1 (* x db/brick-size)) ;; compensate for parents borders
                  top (+ 25 (* y db/brick-size))  ;; compensate for parents borders
                  root-visible? (not (or (> x (- pw-int 1)) (> y (- ph-int 2)))) ;; dont
                  blank-map
                  {:child   [re-com/v-box :size "auto" :justify :center :align :center :children
                             (if scrub-borders?
                               [[re-com/v-box :align :center :justify :center :children
                                 (for [e (ut/splitter (ut/replacer (str orig-key) #"view/" "") ".")]
                                   [re-com/box :style {:color "pink" :font-family (theme-pull :theme/base-font nil)} :child
                                    (str (if (cstr/starts-with? e ":") e (str ":" e)))]) :style {:font-size "11px"}]
                                [re-com/h-box :gap "10px" :padding "5px" :justify :between :children
                                 [[re-com/md-icon-button :md-icon-name "zmdi-view-agenda" :on-click
                                   #(ut/tracked-dispatch [::split-v-layout-panel panel-key view-key orig-key (get p :root)
                                                          processed-layout]) :style
                                   {:font-size "17px" :cursor "pointer" :color "pink" :padding "0px" :margin-top "-1px"}]
                                  [re-com/md-icon-button :md-icon-name "zmdi-view-agenda" :on-click
                                   #(ut/tracked-dispatch [::split-h-layout-panel panel-key view-key orig-key (get p :root)
                                                          processed-layout]) :style
                                   {:font-size  "17px"
                                    :cursor     "pointer"
                                    :transform  "rotate(90deg)"
                                    :color      "pink"
                                    :padding    "0px"
                                    :margin-top "-2px"}]]]
                                [re-com/h-box :gap "10px" :padding "5px" :justify :between :children
                                 [[re-com/md-icon-button :md-icon-name "zmdi-grid-off" :on-click
                                   #(ut/tracked-dispatch [::clear-layout-panel panel-key view-key orig-key (get p :root)])
                                   :style
                                   {:font-size "17px" :cursor "pointer" :color "pink" :padding "0px" :margin-top "-1px"}]
                                  [re-com/md-icon-button :md-icon-name "zmdi-close" :on-click
                                   #(ut/tracked-dispatch [::delete-layout-panel panel-key view-key orig-key (get p :root)])
                                   :style
                                   {:font-size "17px" :cursor "pointer" :color "pink" :padding "0px" :margin-top "-1px"}]]]]
                               [])]
                   :size    "none"
                   :width   (brix w)
                   :height  (brix h)
                   :align   :center
                   :justify :center
                   :style   (merge {:border           (if scrub-borders? "2px dashed pink" "inherit")
                                    :background-color (if hovered? "purple" "inherit")
                                    :filter           (when hovered? "drop-shadow(0 0 0.75rem crimson)")
                                    :position         "fixed" ; "relative"
                                    :overflow         "hidden"
                                    :z-index          (+ p-z z)
                                    :left             left
                                    :top              top}
                                   pstyle)}
                  box-map {:child  view-ref ;[re-com/box :child view-ref :style {:transform
                           :size   "none"
                           :width  (brix w)
                           :height (brix h)
                           :style  (merge {:border   (if scrub-borders? "2px dashed #FFC0CB44" "inherit")
                                           :position "fixed" ; "relative"
                                           :overflow "hidden"
                                           :z-index  (+ p-z z)
                                           :left     left
                                           :top      top}
                                          pstyle)}
                  leftovers (dissoc p :style :h :w :z :root :k :view-ref :hidden? :id)]
              (when (and root-visible? (not hidden?))
                (ut/mapply re-com/box
                           (merge leftovers
                                  (if (or (keyword? view-ref) (and scrubber-on? (= panel-key selected-block) editor?))
                                    blank-map
                                    box-map))))))))]
    [re-com/h-box :size "none" :width (brix pw-int -10) :height (brix ph-int -47) ;; footer, will be
                                                                                  ;; dynamic TODO
     :style
     (merge
      layout-style
      {;:border "1px solid lime" ;; debug edges :position "absolute"
       :overflow "hidden"
       :background-size (when scrub-borders? "50px 50px")
       :background-image
       (when scrub-borders?
         "linear-gradient(0deg,   transparent 48px, #ffffff27 48px, #ffffff27 50px, transparent 50px),
                                        linear-gradient(90deg, transparent 48px, #ffffff27 48px, #ffffff27 50px, transparent 50px)")})
     :children boxes]))

(defn dropdown
  [{:keys [choices field panel-key width button-style style placeholder]}]
  (let [;;choice @(ut/tracked-subscribe [::conn/clicked-parameter-key [(try (keyword (str (name
        choice @(ut/tracked-sub ::conn/clicked-parameter-key-alpha
                                {:keypath [(try (keyword (str (name panel-key) "/" (name field)))
                                                (catch :default _ :error/error))]})
        choice (if (string? (get-in choices [0 :id])) (str choice) choice)
        choice (if (and (string? choice) (empty? (cstr/trim choice))) nil choice) ;; in case we
        width  (if (string? width) (try (edn/read-string (ut/replacer width "px" "")) (catch :default _ 100)) width)]
    [re-com/h-box :size "auto" :justify :between :align :center :children
     [[re-com/md-icon-button :on-click #(ut/tracked-dispatch [::conn/click-parameter [panel-key field] nil]) :style
       (merge {:font-size "19px" :width "30px" :margin-top "8px" :opacity 0.33} button-style) :md-icon-name "zmdi-close"]
      [re-com/single-dropdown :style (if (nil? style) {} style) :parts {:chosen-drop {:style {:overflow "visible"}}} :placeholder
       (str placeholder) :choices choices :model choice ;(ut/tracked-subscribe [::conn/clicked-parameter-key [(try (keyword
                                                        ;(str
       :width (px (- width 30)) :on-change ;#(ut/tracked-dispatch [::conn/click-parameter
                                           ;[panel-key] {field %}])
       #(ut/tracked-dispatch [::conn/click-parameter [panel-key field] %])]]]))


(re-frame/reg-sub ::generate-all-clicked
                  (fn [db {:keys [query-key]}]
                    (let [click-params       (get-in db [:click-param query-key] {})
                          multi-query-key    (-> query-key
                                                 str
                                                 (ut/replacer #":" "")
                                                 (str ".*")
                                                 keyword)
                          click-multi-params (get-in db [:click-param multi-query-key] {})
                          merged-multis      (into {}
                                                   (for [[k v] click-multi-params]
                                                     (if (get click-params k) {k (merge v (get click-params k))} {k v})))]
                      (merge click-params merged-multis))))

(re-frame/reg-sub ::rs-value
                  (fn [db {:keys [flow-id kkey]}] ;;; dupe from buffy
                    (let [src (get-in db [:runstreams flow-id :values kkey :source])]
                      (if (= src :param)
                        (let [vvv @(ut/tracked-sub ::resolver/logic-and-params
                                                   {:m [(get-in db [:runstreams flow-id :values kkey :value])]})
                              vv  (try (first vvv ;;@(ut/tracked-sub ::resolver/logic-and-params {:m [(get-in db
                                                  ;;[:runstreams
                                              )
                                       (catch :default e (do (ut/tapp>> [:rs-value-fuck-up-bricks vvv flow-id kkey src e]) vvv)))]
                          vv)
                        (get-in db [:runstreams flow-id :values kkey :value])))))

(re-frame/reg-sub ::runstream-overrides ;;; duped in AUDIO, TODO!!!
                  (fn [db [_ flow-id]]
                    (let [values       (get-in db [:runstreams flow-id :values])
                          override-map (into {}
                                             (for [[k {:keys [value source]}] values
                                                   :when                      (not (nil? value))]
                                               {k (if (= source :input)
                                                    value
                                                    (let [;;vv @(ut/tracked-subscribe [::rs-value
                                                          ;;flow-id k])
                                                          vv @(ut/tracked-sub ::rs-value {:flow-id flow-id :kkey k})] ;;; dupe
                                                                                                               ;;; from
                                                                                                               ;;; buffy
                                                      (if (and (vector? vv) (every? string? vv)) (cstr/join "\n" vv) vv)))}))]
                      override-map)))

(re-frame/reg-sub ::da-sched (fn [db _] (get db :sched)))

(re-frame/reg-sub ::webcam-feed (fn [db _] (get-in db [:webcam-feed])))

(re-frame/reg-sub ::webcam? (fn [db _] (get-in db [:webcam-feed])))

(defn honeycomb-fragments
  [c & [w h]]
  (let [panel-key :virtual-panel ;:block-4752 ;:hello-there-brother
        key       :virtual-view ;:view ;:ufo-country ;:heya!
        type      (cond (vector? c)                         :view
                        (string? c)                         :view
                        (and (map? c) (nil? (get c :view))) :query
                        :else                               :both)
        data_d    c]
    (cond (= type :view)  (let [view {key data_d} w (or w 11) h (or h 9)] [honeycomb panel-key :virtual-view w h view nil])
          (= type :query) (let [temp-key (get data_d :_query-id (keyword (str "kick-" (hash c))))
                                query    {temp-key (-> data_d ;(get data_d :queries)
                                                       (dissoc :cache?)
                                                       (dissoc :refresh-every))}
                                h        (get data_d :_h (or h 6))
                                w        (get data_d :_w (or w 10))]
                            [re-com/box :size "none" :width (px (* w db/brick-size)) :height (px (- (* h db/brick-size) 30)) :child
                             [honeycomb panel-key temp-key h w nil query]])
          (= type :both)  (let [queries (get data_d :queries)
                                qkeys   (into {}
                                              (for [q (keys queries)]
                                                {q (keyword (str (ut/replacer (str q) #":" "") "-kick-" (hash data_d)))}))
                                ndata   (ut/postwalk-replacer qkeys data_d)
                                h       (get data_d :_h (or h 11))
                                w       (get data_d :_w (or w 9))]
                            [honeycomb panel-key key ;:view ;(get data_d :selected-view)
                             h w {key (get ndata :view)} ;views ;(ut/postwalk-replacer qkeys views)
                             (get ndata :queries)]) ;(ut/postwalk-replacer qkeys queries)
          :else           [honeycomb panel-key key 11 9])))




(def mutation-log (reagent/atom []))


(re-frame/reg-sub
 ::drop-details
 (fn [db [_ drop-id]]
   (let [drops (get-in db [:runstream-drops])]
     (get (into {} (apply concat (for [[k v] drops] (for [[kk vv] v] {kk (merge vv {:flow-id k :name kk})})))) drop-id {}))))





(re-frame/reg-sub ::lookup-alert-id
                  (fn [db [_ type flow-id bid value]]
                    (let [alerts (get db :alerts)]
                      (last (first (filter (fn [x]
                                             (let [un (vec (distinct (ut/deep-flatten x)))]
                                               (and (some #(= % value) un) (some #(= % flow-id) un) (some #(= % type) un))))
                                           alerts))))))

(re-frame/reg-event-db ::push-value
                       (fn [db [_ flow-id bid value & [alert?]]]
                         (let [client-name (get db :client-name)]
                           (ut/tapp>> [:push-value flow-id bid value client-name])
                           (ut/tracked-dispatch
                            [::wfx/request :default
                             {:message (merge {:kind :push-value :flow-id flow-id :bid bid :value value :client-name client-name}
                                              (when alert? {:alert? alert?}))
                              :timeout 15000000}])
                           db)))

(defn code-box
  [width-int height-int value & [syntax]]
  [re-com/box :size "auto" :style
   {:font-family   (theme-pull :theme/monospaced-font nil) ; "Chivo Mono" ;"Fira Code"
    :font-size     "12px"
    :overflow      "auto"
    :border-radius "12px"
    :font-weight   700} :child
   [(reagent/adapt-react-class cm/UnControlled)
    {:value   (str value)
     :options {:mode              (or syntax "text")
               :lineWrapping      true
               :lineNumbers       false ; true
               :matchBrackets     true
               :autoCloseBrackets true
               :autofocus         false
               :autoScroll        false
               :detach            true
               :readOnly          true ;true
               :theme             (theme-pull :theme/codemirror-theme nil)}}]]) ;"ayu-mirage" ;"hopscotch"

(defn shortcode-box
  [width-int height-int value & [syntax]]
  [re-com/box :size "auto" :width (px (- width-int 24)) :height (px (- height-int 24)) :style
   {:font-family      (theme-pull :theme/monospaced-font nil) ; "Chivo Mono" ;"Fira Code"
    :font-size        "18px"
    :border           (str "1px solid " (theme-pull :theme/editor-outer-rim-color nil) 55)
    :background-color "#00000099"
    :overflow         "auto"
    :padding          "3px"
    :border-radius    "12px"
    :font-weight      700} :child
   [(reagent/adapt-react-class cm/UnControlled)
    {:value   (str value)
     :options {:mode              (or syntax "text")
               :lineWrapping      true
               :lineNumbers       false ; true
               :matchBrackets     true
               :autoCloseBrackets true
               :autofocus         false
               :autoScroll        false
               :detach            true
               :readOnly          true ;true
               :theme             (theme-pull :theme/codemirror-theme nil)}}]])


(defn edn-code-box
  [width-int height-int value & [syntax]]
  [re-com/box :size "auto" :style
   {:font-family   (theme-pull :theme/monospaced-font nil) ; "Chivo Mono" ;"Fira Code"
    :overflow      "auto"
    :border-radius "12px"
    :font-weight   700} :child
   [(reagent/adapt-react-class cm/UnControlled)
    {;:value   (str value)
     :value   (ut/format-map width-int (str value))
     :options {:mode              "clojure" ;(or syntax "text")
               :lineWrapping      true
               :lineNumbers       false ; true
               :matchBrackets     true
               :autoCloseBrackets true
               :autofocus         false
               :autoScroll        false
               :detach            true
               :readOnly          true ;true
               :theme             (theme-pull :theme/codemirror-theme nil)}}]]) ;"ayu-mirage" ;"hopscotch"

;; (def ansi-converter (AnsiToHtml. #js {:newline true}))

;; (defn console-viewer [{:keys [text style width height]}]
;;   (let [html-content (cstr/join "\n\r" (map #(.toHtml ansi-converter %) text))
;;         console-style {:font-family (theme-pull :theme/monospaced-font nil)  ;"Consolas, 'DejaVu Sans Mono for Powerline', 'Noto Sans Mono', monospace"
;;                        :font-size "15px"
;;                        :line-height "0.57"
;;                          ;:background-color "#000"
;;                          ;:color "#fff"
;;                        :padding "10px"
;;                        :white-space "pre"
;;                          ;:overflow-x "auto"
;;                          ;:overflow-y "auto"
;;                          ;:border "1px solid #333"
;;                        }]
;;     [re-com/box
;;      :size "none" :width (px width) :height (px height)
;;      :style (merge console-style style {:overflow "auto"})
;;      :child [:div {:dangerouslySetInnerHTML
;;                    {:__html html-content}}]]))






;; (defn safe-subvec [v start end]
;;   (let [start (max 0 start)
;;         end (min (count v) end)]
;;     (if (< start end)
;;       (subvec v start end)
;;       [])))

;; (defn virtualized-console-viewer [{:keys [text style width height]}]
;;   (let [line-height 19 ;; Assuming each line is 17px high

;;         ;; Ensure text is always a vector
;;         text-lines (if (vector? text)
;;                      text
;;                      (vec (cstr/split text "\n")))
;;         num-visible-lines (int (+ (/ height line-height) 3))
;;         total-lines       (+ (count text-lines) num-visible-lines 3)
;;         state (reagent/atom {:start 0 :end num-visible-lines})
;;         handle-scroll (fn [e]
;;                         (let [scroll-top (.. e -target -scrollTop)
;;                               start (int (/ scroll-top line-height))
;;                               end (min total-lines (+ start num-visible-lines))]
;;                           (swap! state assoc :start start :end end)))]
;;     (fn []
;;       (let [{:keys [start end]} @state
;;             ;; Safeguard subvec to handle edge cases
;;             visible-lines (if (and (<= start end) (<= end total-lines))
;;                             (safe-subvec text-lines start end)
;;                             [])
;;             html-content (cstr/join "\n" (map #(.toHtml ansi-converter %) visible-lines))
;;             padding-top (* start line-height)
;;             padding-bottom (* (- total-lines end) line-height)
;;             console-style {:font-family "monospace"
;;                            :font-size "15px"
;;                            :line-height (str line-height "px")
;;                            :padding "15px"
;;                            :white-space "pre"
;;                            :text-shadow "#00000088 1px 0 10px"
;;                            :overflow "auto"}]
;;         [re-com/box
;;          :size "none" :width (str width "px") :height (str height "px")
;;          :style (merge console-style style)
;;          :attr {:on-scroll handle-scroll}
;;          :child [:div {:style {:font-family "inherit"
;;                                :padding-top (str padding-top "px")
;;                                :padding-bottom (str padding-bottom "px")}
;;                        :dangerouslySetInnerHTML
;;                        {:__html html-content}}]]))))




  ;; (def scrollbar-style
  ;;   {:scrollbar-width "thin"
  ;;  ;:scrollbar-color "#4a4a4a #2a2a2a66"
  ;;    :scrollbar-color (str (str (theme-pull :theme/universal-pop-color nil) 33) "#00000033")
  ;;    :&::-webkit-scrollbar {:width "10px"}
  ;;    :&::-webkit-scrollbar-track {:background "#2a2a2a"
  ;;                                 :border-radius "5px"}
  ;;    :&::-webkit-scrollbar-thumb {:background "#4a4a4a"
  ;;                                 :border-radius "15px"}
  ;;    :&::-webkit-scrollbar-thumb:hover {:background "#6a6a6a"}})

  ;; (defn virtualized-console-viewer [{:keys [text style width height]}]
  ;;   (let [line-height 19 ;; Assuming each line is 17px high
  ;;       ;; Ensure text is always a vector
  ;;         text-lines (if (vector? text)
  ;;                      text
  ;;                      (vec (cstr/split text "\n")))
  ;;         num-visible-lines (int (- (/ height line-height) 1))
  ;;         total-lines       (+ (count text-lines) num-visible-lines 3)
  ;;         state (reagent/atom {:start 0 :end num-visible-lines})
  ;;         handle-scroll (fn [e]
  ;;                         (let [scroll-top (.. e -target -scrollTop)
  ;;                               start (int (/ scroll-top line-height))
  ;;                               end (min total-lines (+ start num-visible-lines))]
  ;;                           (swap! state assoc :start start :end end)))]
  ;;     (fn []
  ;;       (let [{:keys [start end]} @state
  ;;           ;; Safeguard subvec to handle edge cases
  ;;             visible-lines (if (and (<= start end) (<= end total-lines))
  ;;                             (ut/safe-subvec text-lines start end)
  ;;                             [])
  ;;             html-content (cstr/join "\n" (map #(.toHtml ansi-converter %) visible-lines))
  ;;             padding-top (* start line-height)
  ;;             padding-bottom (* (- total-lines end) line-height)
  ;;             console-style {:font-family "monospace"
  ;;                            :font-size "15px"
  ;;                            :line-height (str line-height "px")
  ;;                            :padding "15px"
  ;;                            :white-space "pre"
  ;;                            :text-shadow "#00000088 1px 0 10px"
  ;;                            :overflow "auto"}]
  ;;         [re-com/box
  ;;          :size "none" :width (str width "px") :height (str height "px")
  ;;          :style (merge console-style style scrollbar-style)
  ;;          :attr {:on-scroll handle-scroll}
  ;;          :child [:div {:style {:font-family "inherit"
  ;;                                :padding-top (str padding-top "px")
  ;;                                :padding-bottom (str padding-bottom "px")}
  ;;                        :dangerouslySetInnerHTML
  ;;                        {:__html html-content}}]]))))

  ;; (defn reactive-virtualized-console-viewer [{:keys [text] :as props}]
  ;;   [:f> (with-meta virtualized-console-viewer
  ;;          {:key (hash text)})
  ;;    props])



(def ansi-converter
  (AnsiToHtml. #js {:newline true
                    :stream true}))

(defn console-viewer [{:keys [text style width height]}]
  (let [is-vec?      (vector? text)
        text         (if is-vec?
                       (map str text)
                       (vec (cstr/split text "\n")))
        html-content (cstr/join "\n" (map #(.toHtml ansi-converter %) text))
        console-style {:font-family "Fixedsys-Excelsior, monospace"  ;;(theme-pull :theme/monospaced-font nil)
                       :font-size "15px"
                       :line-height "1.1"
                       :padding "15px"
                       :white-space "pre"
                       :text-shadow "#00000088 1px 0 10px"
                         ;:background-color "#000"
                         ;:color "#ffffff99"
                       }]
    [re-com/box
     :size "none" :width (px width) :height (px height)
     :style (merge console-style style {:overflow "auto"})
     :child [:div {:style {:font-family "inherit"}
                   :dangerouslySetInnerHTML
                   {:__html html-content}}]]))


(defn reactive-virtualized-console-viewer [{:keys [text style width height px-line-height] :as props}]
  (let [is-vec?      (vector? text)
        text         (if is-vec?
                       (map str text)
                       (vec (cstr/split text "\n")))
        ;html-content (cstr/join "\n" (map #(.toHtml ansi-converter %) text))
        decoded-lines (mapv #(.toHtml ansi-converter %) text)
        console-style (merge
                       {:font-family "Fixedsys-Excelsior, monospace" ;;(theme-pull :theme/monospaced-font nil)
                        :font-size "15px"
                        :line-height "0.95"
                        :padding "15px"
                        ;:color "inherit"
                          ;:color (theme-pull :theme/editor-font-color nil)
                        :white-space "pre"
                        :text-shadow "#00000088 1px 0 10px"} style)
        text-boxes (vec (for [t decoded-lines]
                          [width (or px-line-height 20)
                           [:div {:style console-style
                                  :dangerouslySetInnerHTML
                                  {:__html t}}]]))
        props (-> props (assoc :children text-boxes) (dissoc :text))]
    [vbunny/virtual-v-box props]))






  ;; (defn reactive-edn-viewer [{:keys [text style width height] :as props}]
  ;; (let [is-vec?      (vector? text)
  ;;       text         (if is-vec?
  ;;                      (format-edn width (cstr/join "\n" text))
  ;;                      (format-edn width text))
  ;;       text          (vec (cstr/split text "\n"))
  ;;       ;text         (if is-vec?
  ;;       ;               (map str text)
  ;;       ;               (vec (cstr/split text "\n")))
  ;;       ;html-content (cstr/join "\n" (map #(.toHtml ansi-converter %) text))
  ;;       decoded-lines (mapv #(.toHtml ansi-converter %) text)
  ;;       console-style (merge
  ;;                      {:font-family (theme-pull :theme/monospaced-font nil)
  ;;                       :font-size "15px"
  ;;                       ;:line-height "1.1"
  ;;                       :padding "15px"
  ;;                       :color "inherit"
  ;;                         ;:color (theme-pull :theme/editor-font-color nil)
  ;;                       :white-space "pre"
  ;;                       :text-shadow "#00000088 1px 0 10px"} style)
  ;;       text-boxes (vec (for [t decoded-lines]
  ;;                         [width 20 [:div {:style console-style
  ;;                                          :dangerouslySetInnerHTML
  ;;                                          {:__html t}}]]))
  ;;       props (-> props (assoc :children text-boxes) (dissoc :text))]
  ;;   [virtual-v-box props]))














(defn wrap-text [text col-width]
  (let [split-line (fn [line]
                     (loop [words (clojure.string/split line #"\s+")
                            current-line ""
                            lines []]
                       (if (empty? words)
                         (if (empty? current-line)
                           lines
                           (conj lines current-line))
                         (let [word (first words)
                               new-line (if (empty? current-line)
                                          word
                                          (str current-line " " word))]
                           (if (<= (count new-line) col-width)
                             (recur (rest words) new-line lines)
                             (recur (rest words) word (conj lines current-line)))))))]
    (->> (clojure.string/split-lines text)
         (mapcat split-line)
         (clojure.string/join "\n"))))










(defn choose-viewer [edn-string]
  (let [char-count (count edn-string)
        line-count (count (cstr/split edn-string #"\n"))]
    (cond
      (> char-count 10000) :virtualized
      (> line-count 200) :virtualized
      :else :normal)))

(re-frame/reg-sub
 ::app-db-clover
 (fn [db {:keys [keypath]}]
   (get-in db keypath)))

(re-frame/reg-sub
 ::is-repl-type?
 (fn [db {:keys [runner-key]}]
   (true? (= :nrepl (get-in db [:server :settings :runners runner-key :type] :none)))))

(re-frame/reg-sub
 ::get-view-source-data
 (fn [db {:keys [block-id view-type data-key]}]
   (get-in db [:panels block-id view-type data-key]
           [:box :child (str "no data for: " [block-id view-type data-key])])))


(defonce progress-bars (reagent.core/atom {}))
(defonce progress-loops (reagent.core/atom {}))

(defn stop-progress-loop
  [uid]
  (when-let [loop (get @progress-loops uid)]
    (async/close! loop)
    (swap! progress-loops dissoc uid)))

(declare grid)

 

(defn clover-walk-singles
  [panel-key client-name px-width-int px-height-int ww hh w h selected-view override-view output-type]
  (let [kk (hash [panel-key client-name px-width-int px-height-int ww hh w h selected-view override-view output-type])
        in-editor? (not (nil? override-view))
        cc (get @ut/clover-walk-singles-map kk)]
    (if cc ;(ut/ne? cc)
      cc
      (let [in-editor? (not (nil? override-view))
            walk-map
            (merge
             {:rechart reech/chart
              :input-text re-com/input-text
              :input-textarea re-com/input-textarea
              :honeycomb honeycomb-fragments
              :single-dropdown re-com/single-dropdown
              :dropdown #(dropdown (try (assoc % :panel-key panel-key) (catch :default _ %)))
              :atom #(reagent/atom %)
              :get get
              ;;:edn #(edn-box (+ px-width-int 70) (+ px-height-int 55) %)
              :edn (fn [x] (let [viewer (choose-viewer (pr-str x))] ;; should we virtualize?
                             (if (= viewer :virtualized)
                               (let [text (if (vector? x) (cstr/join "\n" x) (pr-str x))
                                     ww (+ px-width-int 70)
                                     ;text (ut/format-edn ww text 9) ;; 3rd arg is math of pixels per font char - zprint uses cols, not pixels
                                     text (conn/format-edn-puget ww text 9)
                                     text (vec (cstr/split text #"\n"))]
                                 [reactive-virtualized-console-viewer {:style {:font-weight 700 :font-size "18px"}
                                                                       :text text
                                                                       :id (str panel-key "-" selected-view "-" output-type)
                                                                       :width ww
                                                                       :height (+ px-height-int 55)}])
                               [edn-box (+ px-width-int 70) (+ px-height-int 55) x] ;; else just use codemirror since it looks nicer
                               )))

              :panel-code-box panel-code-box
              :code-box panel-code-box
              :editor (fn [[block-id runner-key data-key]]
                        (let [is-repl-type? @(ut/tracked-sub ::is-repl-type? {:runner-key runner-key})
                              value-spy?  (get-in @db/value-spy [block-id data-key] false)
                              spy-hash (get @db/value-spy-hashes [block-id data-key] "n/a")
                              source-data   @(ut/tracked-sub ::get-view-source-data {:view-type runner-key :block-id block-id :data-key data-key})]
                          [re-com/v-box
                           :children
                           [[re-com/box
                             :style {:padding "8px 8px 8px 21px"
                                                    ;:border "1px dashed orange"
                                     }
                             :child
                             ^{:key (str "cm-" block-id runner-key data-key value-spy? spy-hash)}
                             [panel-code-box
                              (fn [] block-id)  ;; sneaky sneaky, react.... ;/ 
                              (fn [] [runner-key data-key])
                              (+ px-width-int 70) (+ px-height-int 55)
                              source-data
                              is-repl-type? in-editor? block-id]]
                            [re-com/h-box
                             :gap "10px"
                             :justify :end
                             :height "15px"
                                              ;:style {:border "1px solid yellow"}
                             :children (vec (for [bb ["spy"]
                                                  :let [on-click-fn (fn [_]
                                                                      (swap! db/value-spy assoc-in [block-id data-key] (not value-spy?)))]]
                                              [re-com/box
                                               :height "20px"
                                               :width "20px"
                                                               ;:size "auto" 
                                               :align :center
                                               :justify :center
                                               :child [re-com/md-icon-button
                                                       :md-icon-name (if (= bb "spy")
                                                                       "fa-solid fa-user-secret"
                                                                       "zmdi-close")
                                                       :on-click on-click-fn
                                                       :style {:color (if value-spy? (theme-pull :theme/universal-pop-color nil) "grey")
                                                               :font-size "11px"}]]))]]]))
                                             ;; :console #(console-box % (+ px-width-int 70) (+ px-height-int 50))
                                           ;; :terminal #(console-viewer {:style {} :text %
                                           ;;                             :width (+ px-width-int 70)
                                           ;;                             :height (+ px-height-int 55)})
              :console  (fn [x] [reactive-virtualized-console-viewer {:style {:font-family (theme-pull :theme/monospaced-font nil)
                                                                              :color (theme-pull :theme/editor-font-color nil)
                                                                              :font-size "17px"
                                                                              :font-weight 700
                                                                              ;:border "1px solid pink"
                                                                              ;:white-space "pre-wrap"
                                                                              ;:overflow-wrap "break-word"
                                                                              ;:line-height "1.1"
                                                                              }
                                                                      :text (wrap-text (if (vector? x) (cstr/join "\n" x) (str x)) (/ (+ px-width-int 40) 9.5))
                                                                      :id (str panel-key "-" selected-view "-" output-type)
                                                                      :width (+ px-width-int 70)
                                                                      :height (+ px-height-int 55)}])
              :terminal (fn [x] [reactive-virtualized-console-viewer
                                 {:style {}
                                  :text x
                                  :id (str panel-key "-" selected-view "-" output-type)
                                  :width (+ px-width-int 70)
                                  :height (+ px-height-int 55)}])

              :terminal-custom (fn [[x w h follow? & [{:keys [style px-line-height] :as opts-map}]]]
                                 [reactive-virtualized-console-viewer
                                  (merge
                                   {:style {}
                                    :text x
                                    :follow? follow?
                                    :id (str panel-key "-" selected-view "-" output-type)
                                    :width (+ w 70)
                                    :height (+ h 55)}
                                   opts-map)])

              :panel-code-box-single panel-code-box-single
              :code-box-single panel-code-box-single
              :vega-lite oz.core/vega-lite
              :nivo-bar-chart #(nivo-render % nivo-bar/BarCanvas panel-key)
              :nivo-line-chart #(nivo-render % nivo-line/LineCanvas panel-key)
              :nivo-calendar #(nivo-render % nivo-calendar/Calendar panel-key)
              :nivo-pie-chart #(nivo-render % nivo-pie/PieCanvas panel-key)
              :nivo-waffle-chart #(nivo-render % nivo-waffle/WaffleCanvas panel-key)
              :nivo-scatterplot #(nivo-render % nivo-scatterplot/ScatterPlotCanvas panel-key)
              :nivo-swarmplot #(nivo-render % nivo-swarmplot/SwarmPlot panel-key)
              :nivo-treemap #(nivo-render % nivo-treemap/TreeMap panel-key)
              :ro-code-box #(code-box nil nil %)
              :edn-code-box #(edn-code-box ww nil %)
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
              :str (fn [args] (apply str args))
              :left-pad (fn [[num len]] (let [num-str (str num) padded-num (.padStart num-str len "0")] padded-num))
              :>> (fn [[x y]] (true? (> x y)))
              :<< (fn [[x y]] (true? (< x y)))
              :string (fn [args]
                        (if (vector? args)
                          (cstr/join "" args) ;;(apply str args))
                          (str args)))
              :data-viewer (fn [x] (let [x (ut/postwalk-replacer {:box :_box :icon :_icon :v-box :_v-box :h-box :_h-box
                                                                  :data-viewer :_data-viewer} x)
                                         solver-key (get @db/solver-fn-lookup [:panels panel-key selected-view])
                                                                     ;; _ (tapp>> [:dv solver-key panel-key selected-view @db/solver-fn-lookup])
                                         ]
                                     [re-com/box :width (px (- ww 10)) :size "none" :height (px (- hh 60)) ;;"300px" ;(px hh)
                                      :style {:overflow "auto"} :child
                                      [map-boxes2 x (or  panel-key solver-key) selected-view []
                                       (if in-editor? [h w] :output)
                                       nil]]))
              :data-viewer-limit (fn [[x l]] (let [x (ut/postwalk-replacer {:box :_box :icon :_icon :v-box :_v-box  :h-box :_h-box
                                                                            :data-viewer :_data-viewer} x)
                                                   solver-key (get @db/solver-fn-lookup [:panels panel-key selected-view])]
                                               [re-com/box :width (px (- ww 10)) :size "none" :height (px (- hh 60)) ;;"300px" ;(px hh)
                                                :style {:overflow "auto"} :child
                                                [map-boxes2 x (or panel-key solver-key) selected-view []
                                                 (if in-editor? [h w] :output)
                                                 nil nil l]]))
              :progress-bar
              (fn [[ww seconds uid]]
                (let [progress            (or (get @progress-bars uid) (reagent.core/atom 0))
                      transition-duration (/ seconds 40.0)]
                  (when (and (zero? @progress) (not (get @progress-loops uid)))
                    (let [loop (async/go-loop []
                                 (<! (async/timeout 1000))
                                 (swap! progress + (quot 100 seconds))
                                 (when (< @progress 100) (recur)))]
                      (swap! progress-loops assoc uid loop)))
                  (swap! progress-bars assoc uid progress)
                  (fn [] [re-com/progress-bar
                          :model (if (> @progress 100) 100 @progress)
                          :striped? true
                          :bar-style {:color            (str (ut/choose-text-color (theme-pull :theme/editor-outer-rim-color nil)) 67)
                                      :outline          "none"
                                      :border           "none"
                                      :transition       (str "all " transition-duration "s ease-in-out")
                                      :background-color (theme-pull :theme/editor-outer-rim-color nil)}
                          :style {:font-family      (theme-pull :theme/base-font nil)
                                  :background-color "#00000000"
                                  :z-index 9999999
                                  :border           (str "1px solid " (theme-pull :theme/editor-outer-rim-color nil) 55)
                                  :outline          "none"
                                  :width            ww}])))
              :speak-only (fn [text]
                            (let []
                              (when (not (some #(= % text) @db/speech-log))
                                (swap! db/speech-log conj text)
                               ;;(ut/dispatch-delay 800 [::http/insert-alert (str text) 13 1 5])
                                (ut/tracked-dispatch [::audio/text-to-speech11 panel-key ;:audio
                                                      :speak (str text)]))
                              (str text)))
              :speak (fn [text]
                       (let []
                         (when (not (some #(= % text) @db/speech-log))
                           (swap! db/speech-log conj text)
                           (ut/dispatch-delay 800 [::http/insert-alert (str text) 13 1 5])
                           (ut/tracked-dispatch [::audio/text-to-speech11 panel-key ;:audio
                                                 :speak (str text)]))
                         (str text)))
              :speak-always (fn [text]
                              (let []
                                (when true ; (not (some #(= % text) @db/speech-log))
                                  (ut/tracked-dispatch [::audio/text-to-speech11 panel-key ;:audio
                                                        :speak (str text)]))
                                (str text)))
              :speak-click (fn [text]
                             (let []
                               [re-com/v-box :size "auto" :children
                                [[re-com/box :child (str text)]
                                 [re-com/h-box :size "auto" :justify :between :height "10px" :style {} ;:border
                                                                                                                                       ;"1px
                                                                                                                                       ;solid
                                                                                                                                       ;white"
                                  :children
                                  [[re-com/md-icon-button :md-icon-name "zmdi-play" :on-click
                                    (fn []
                                      (when true ;(not (some (fn [x] (= x text)) @db/speech-log))
                                        (do (ut/dispatch-delay 800 [::http/insert-alert (str "\"" text "\"") 13 2 8])
                                            (ut/tracked-dispatch [::audio/text-to-speech11 panel-key :speak (str text)]))))
                                    :style {:font-size "15px" :opacity 0.25 :cursor "pointer"}] [re-com/box :child ""]]]]]))
              :grid (fn [tab] [reecatch [grid tab]]) ;;; TODO :grid was clashing with nivo
              :read-edn (fn [x] (try (edn/read-string x) (catch :default _ x)))
              :open-input (fn [args] [re-com/box :width (px (- ww 10)) :size "none" :height (px (- hh 60)) ;;"300px"
                                                                                                                                           ;;;(px
                                                                                                                                           ;;hh)
                                      :style {:overflow "auto"} ;; (merge {:overflow "auto"} (get args
                                                                                                ;; :style
                                      :child
                                      (let [;[kp width-int height-int syntax] args
                                            {:keys [kp width-int height-int style syntax]} args
                                            ;;vv     @(ut/tracked-sub ::conn/clicked-parameter-alpha {:keypath kp})
                                            vv     @(ut/tracked-subscribe [::conn/clicked-parameter kp])
                                            vv     (if (string? vv) (pr-str vv) vv)]
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
                              [re-com/v-box :size "auto" :children
                               [[re-com/box :child (str text)]
                                [re-com/h-box :size "auto" :justify :between :height "10px" :style {}
                                 :children
                                 [[re-com/md-icon-button :md-icon-name "zmdi-play" :on-click
                                   (fn []
                                     (when (not (some (fn [x] (= x text)) @db/speech-log))
                                       (do (ut/tracked-dispatch [::audio/text-to-speech11 panel-key :speak (str text) true]))))
                                   :style {:font-size "15px" :opacity 0.25 :cursor "pointer"}] [re-com/box :child ""]]]]]))
              :play-click-custom (fn [[text display-text]]
                                   (let []
                                     [re-com/v-box :size "auto" :children
                                      [[re-com/box :child (pr-str display-text)]
                                       [re-com/h-box
                                        :size "auto"
                                        ;:justify :between 
                                        :gap "4px"
                                        :height "10px"
                                        :style {}
                                        :children
                                        [[re-com/md-icon-button
                                          :md-icon-name "zmdi-play"
                                          :on-click (fn []
                                                      (when (not (some (fn [x] (= x text)) @db/speech-log))
                                                        (do (ut/tracked-dispatch
                                                             [::audio/text-to-speech11 panel-key :speak (str text) true]))))
                                          :style {:font-size "15px" :opacity 0.25 :cursor "pointer"}]
                                         [re-com/md-icon-button
                                          :md-icon-name "zmdi-stop"
                                          :on-click #(audio/stop-audio)
                                          :style {:font-size "15px" :opacity 0.25 :cursor "pointer"}]
                                         [re-com/box :child ""]]]]]))
              :play-click-target (fn [[text panel-key]]
                                   (let []
                                     [re-com/v-box :size "auto" :children
                                      [[re-com/box :child (str text)]
                                       [re-com/h-box :size "auto" :justify :between :height "10px" :style {}
                                        :children
                                        [[re-com/md-icon-button :md-icon-name "zmdi-play" :on-click
                                          (fn []
                                            (when (not (some (fn [x] (= x text)) @db/speech-log))
                                              (do (ut/tracked-dispatch [::audio/text-to-speech11 panel-key :speak (str text)
                                                                        true])))) :style
                                          {:font-size "15px" :opacity 0.25 :cursor "pointer"}] [re-com/box :child ""]]]]]))
              :push (fn [[flow-id bid value & [view]]]
                      (let [bid (try (if (cstr/starts-with? bid ":") (edn/read-string bid) (edn/read-string (str ":" bid)))
                                     (catch :default _ nil))]
                        [re-com/v-box :size "auto" :children
                         [[re-com/box :child (if view view (str [flow-id bid value]))]
                          [re-com/h-box :size "auto" :justify :between :height "10px" :style {}
                           :children
                           [[re-com/md-icon-button :md-icon-name "fa-solid fa-location-crosshairs" ;; <i class=
                             :on-click (fn [] (when bid (ut/tracked-dispatch [::push-value flow-id bid value]))) :style
                             {:font-size "15px" :opacity 0.25 :cursor "pointer"}] [re-com/box :child ""]]]]]))

              :dialog-push (fn [[flow-id bid value & [view]]]
                             (let [bid      (try (if (cstr/starts-with? bid ":") (edn/read-string bid) (edn/read-string (str ":" bid)))
                                                 (catch :default _ nil))
                                   alert-id @(ut/tracked-subscribe [::lookup-alert-id :dialog-push flow-id bid value])]
                               [re-com/box :style {:cursor "pointer"} :attr
                                {:on-click (fn []
                                             (when bid
                                               (ut/tracked-dispatch [::push-value flow-id bid value])
                                               (ut/dispatch-delay 800 [::prune-alert alert-id])))} :size "auto" :child
                                (if view view (str "push " value " to " flow-id "/" bid))]))
              :web-cam #(let [video-ref     (reagent/atom nil)
                              running?      @(ut/tracked-subscribe [::webcam?])
                              webcam-stream @(ut/tracked-subscribe [::webcam-feed])]
                          (reagent/create-class
                           {:component-did-mount (fn []
                                                   (when (and webcam-stream @video-ref)
                                                     (set! (.-srcObject @video-ref) webcam-stream)))
                            :reagent-render      (fn []
                                                   [re-com/box :style (if (not running?) {:border "1px solid #ffffff87"} {})
                                                    :child
                                                    [:video
                                                     {:on-click (fn [_]
                                                                  (if running?
                                                                    (ut/tracked-dispatch [::audio/stop-webcam])
                                                                    (ut/tracked-dispatch [::audio/start-webcam])))
                                                      :style
                                                      {:objectFit "cover" :top "0" :left "0" :width "100%" :height "100%"}
                                                      :autoPlay true
                                                      :playsInline true
                                                      :ref (fn [x] (reset! video-ref x))}]])}))
                                              ;;:app-db (fn [kp] @(ut/tracked-sub ::app-db-clover {:keypath kp}))
              :insert-alert (fn [[c w h d]] (ut/tracked-dispatch [::http/insert-alert c w h d]))
                                              ;;:invert-hex-color (fn [x] (ut/invert-hex-color x))

                                           ;; :run-flow (fn [[flow-id tt & [overrides]]]
                                           ;;             (let [client-name  @(ut/tracked-sub ::client-name {})
                                           ;;                   base-opts    {:increment-id? false}
                                           ;;                   running-key  (keyword (str "flow-status/" flow-id ">*running?"))
                                           ;;                   running?     @(ut/tracked-sub ::conn/clicked-parameter-key-alpha {:keypath [running-key]})
                                           ;;                   runstreamed? (= overrides :runstream-overrides)
                                           ;;                   overrides    (if runstreamed? @(ut/tracked-subscribe [::runstream-overrides flow-id]) overrides)
                                           ;;                   overrides?   (ut/ne? overrides)]
                                           ;;               [re-com/h-box :size "auto" :children
                                           ;;                [[re-com/box :child (str tt)]
                                           ;;                 [re-com/h-box :size "auto" :height "10px" :children
                                           ;;                  [;(str running?)
                                           ;;                   (when runstreamed?
                                           ;;                     [re-com/md-icon-button :md-icon-name "zmdi-tune" :style
                                           ;;                      {:font-size        "inherit"
                                           ;;                       :padding          "5px"
                                           ;;                       :margin-right     "-14px"
                                           ;;                       :transform-origin "18px 17px"
                                           ;;                       :margin-top       "2px"}])
                                           ;;                   [re-com/md-icon-button :md-icon-name (if running? "zmdi-refresh" "zmdi-play") :class
                                           ;;                    (when running? "rotate linear infinite")
                                           ;;                    :on-click (fn []
                                           ;;                                (when (not running?) ;(not (some (fn [x] (= x text))
                                           ;;                                  (let [fstr (str "running flow " flow-id (when overrides? " (with overrides)"))
                                           ;;                                        w    (/ (count fstr) 4.1)]
                                           ;;                                    (ut/tracked-dispatch
                                           ;;                                     [::wfx/push :default
                                           ;;                                      {:kind        :run-flow
                                           ;;                                       :flow-id     flow-id
                                           ;;                                       :flowmap     flow-id
                                           ;;                                       :no-return?  true
                                           ;;                                       :opts        (if (map? overrides) (merge base-opts {:overrides overrides}) base-opts)
                                           ;;                                       :client-name client-name
                                           ;;                                       :keypath     [:panels panel-key :views selected-view]}])
                                           ;;                                    (ut/dispatch-delay 800 [::http/insert-alert fstr w 1 5])))) :style
                                           ;;                    {:font-size        "inherit"
                                           ;;                     :padding          "5px"
                                           ;;                     :transform-origin "18px 17px"
                                           ;;                     :margin-top       "2px"
                                           ;;                     :cursor           "pointer"}] [re-com/box :child ""]]]]]))

              :run-flow  (fn [[flow-id tt & [overrides]]]
                           (let [client-name  @(ut/tracked-sub ::client-name {})
                                 base-opts    {:increment-id? false}
                                 running-key  (keyword (str "flow-status/" flow-id ">*running?"))
                                 running?     @(ut/tracked-sub ::conn/clicked-parameter-key-alpha {:keypath [running-key]})
                                 runstreamed? (= overrides :runstream-overrides)
                                 overrides    (if runstreamed? @(ut/tracked-subscribe [::runstream-overrides flow-id]) overrides)
                                 overrides?   (ut/ne? overrides)
                                 trig!        [@waiting?]]
                             [re-com/h-box :size "auto" :children
                              [[re-com/box :child (str tt)]
                               [re-com/h-box :size "auto" :height "10px" :children
                                [(when runstreamed?
                                   [re-com/md-icon-button :md-icon-name "zmdi-tune" :style
                                    {:font-size        "inherit"
                                     :padding          "5px"
                                     :margin-right     "-14px"
                                     :transform-origin "18px 17px"
                                     :margin-top       "2px"}])
                                 [re-com/md-icon-button
                                  :md-icon-name (if (or (get @waiting? flow-id) running?) "zmdi-refresh" "zmdi-play")
                                  :class (cond
                                           running? "rotate linear infinite"
                                           (get @waiting? flow-id) "rotate-reverse linear infinite"
                                           :else "")
                                  :on-click (fn []
                                              (when (not running?)
                                                (swap! waiting? assoc flow-id true)
                                                (let [fstr (str "running flow " flow-id (when overrides? " (with overrides)"))
                                                      w    (/ (count fstr) 4.1)]
                                                  (ut/tracked-dispatch
                                                   [::wfx/push :default
                                                    {:kind        :run-flow
                                                     :flow-id     flow-id
                                                     :flowmap     flow-id
                                                     :no-return?  true
                                                     :opts        (if (map? overrides) (merge base-opts {:overrides overrides}) base-opts)
                                                     :client-name client-name
                                                     :keypath     [:panels panel-key :views selected-view]}])
                                                  (ut/dispatch-delay 800 [::http/insert-alert fstr w 1 5])
                                                  (js/setTimeout #(swap! waiting? assoc flow-id false) 5000))))
                                  :style {:font-size        "inherit"
                                          :padding          "5px"
                                          :transform-origin "18px 17px"
                                          :margin-top       "2px"
                                          :cursor           "pointer"}]
                                 [re-com/box :child ""]]]]]))

              :click-solver (fn [[solver-name tt & [input-map overrides]]]
                              (let [client-name         @(ut/tracked-sub ::client-name {})
                                    running-key         (keyword (str "solver-status/" (cstr/replace (str client-name) ":" "") ">" (cstr/replace (str solver-name) ":" "") ">running?"))
                                    running?            @(ut/tracked-sub ::conn/clicked-parameter-key-alpha {:keypath [running-key]})
                                    overrides?          (ut/ne? overrides)
                                    input-map?          (ut/ne? input-map)
                                    selected-view-type  @(ut/tracked-sub ::view-type {:panel-key panel-key :view selected-view})
                                    trig!               [@waiting?]]
                                [re-com/h-box :size "auto" :children
                                 [[re-com/box :child (if (not (vector? tt)) (str tt) tt)]
                                  [re-com/h-box :size "auto" :height "10px" :children
                                   [;(str running?)
                                    [re-com/md-icon-button
                                     :md-icon-name (if (or (get @waiting? solver-name) running?) "zmdi-refresh" "zmdi-play")
                                                                  ;:class (when running? "rotate linear infinite")
                                     :class (cond
                                              running? "rotate linear infinite"
                                              (get @waiting? solver-name) "rotate-reverse linear infinite"
                                              :else "")
                                     :on-click (fn []
                                                 (when (not running?) ;(not (some (fn [x] (= x text))
                                                   (swap! waiting? assoc solver-name true)
                                                   (let [fstr (str "running solver " solver-name
                                                                   (when overrides? " (with overrides)")
                                                                   (when input-map? " (with inputs)"))
                                                         w    (/ (count fstr) 4.1)]

                                                     (ut/tracked-dispatch
                                                      [::wfx/push     :default
                                                       {:kind         :run-solver-custom
                                                        :solver-name  solver-name
                                                        :ui-keypath   [panel-key selected-view-type selected-view]
                                                        :override-map overrides
                                                        :input-map    input-map
                                                        :client-name  client-name}])

                                                     (ut/dispatch-delay 800 [::http/insert-alert fstr w 1 5])
                                                     (js/setTimeout #(swap! waiting? assoc solver-name false) 5000))))
                                     :style {:font-size        "inherit"
                                             :padding          "5px"
                                             :transform-origin "18px 17px"
                                             :margin-top       "2px"
                                             :cursor           "pointer"}] [re-com/box :child ""]]]]]))

              :case (fn [x] (ut/vectorized-case x))
              :scrubber (fn [[kk pm & [opts]]] [scrubber-panel true @(ut/tracked-sub ::keypaths-in-params {:key-type :param}) kk pm
                                                (if opts {:fm true :canvas? true :opts opts} 
                                                    {:fm true :canvas? true})])
              :Map Map
              :Source Source
              :Layer Layer
              :Marker Marker
              :Map/Map Map
              :Map/Source Source
              :Map/Layer Layer
              :Map/Marker Marker
              :viewport-params-fn #(ut/tracked-dispatch [::conn/click-parameter [panel-key]
                                                         (select-keys (walk/keywordize-keys (js->clj (.-viewState %)))
                                                                      [:longitude :latitude :zoom])])
                                           ;; :markdown (fn [md]
                                           ;;             (->> (str md)
                                           ;;                  (m/md->hiccup)
                                           ;;                  (m/component)))
              :test-params-fn #(ut/tracked-dispatch [::conn/click-parameter [panel-key] (str %)]) ;; (walk/keywordize-keys
              :params> #(ut/tracked-dispatch [::conn/click-parameter [panel-key] (js->clj % :keywordize-keys true)])
              :as-e (fn [x] [reagent/as-element x])
              :text str
              :number (fn [x] (str (nf x)))
              :percent (fn [x] (str (nf x) "%"))
              :v-box re-com/v-box}
             
             (walk-map-sizes (/ px-width-int db/brick-size) (/ px-height-int db/brick-size) nil nil nil nil)
                                             ;; {:box re-com/box
                                             ;;  :layout (fn [x] [layout panel-key (or override-view selected-view) x w h])
                                             ;;  :px re-com.util/px
                                             ;;  :icon re-com/md-icon-button
                                             ;;  :md-icon re-com/md-icon-button
                                             ;;  :h-box re-com/h-box
                                             ;;  :hbox re-com/h-box
                                             ;;  :vbox re-com/v-box}
             )]
        (swap! ut/clover-walk-singles-map assoc kk walk-map)
        walk-map))))

;; (re-frame/reg-event-db ::sub-to-solver-custom
;;                        (fn [db [_ flow-id bid value & [alert?]]]
;;                          (let [client-name (get db :client-name)]
;;                            (ut/tapp>> [:push-value flow-id bid value client-name])
;;                            (ut/tracked-dispatch [::wfx/request :default
;;                                                  {:message (merge {:kind        :push-value
;;                                                                    :flow-id     flow-id
;;                                                                    :bid         bid
;;                                                                    :value       value
;;                                                                    :client-name client-name}
;;                                                                   (when alert? {:alert? alert?}))
;;                                                   :timeout 15000000}])
;;                            db)))



;; (def clover-templates {:color-theft   {:args [:*input]
;;                                        :body [:data-viewer
;;                                               [:run-solver
;;                                                [:get-my-colors
;;                                                 {:input-image-path
;;                                                  :*input}]]]}
;;                        :clj            {:args [:code] :body  [:run-solver
;;                                                               {:signal false
;;                                                                :cache? true
;;                                                                :type :clojure
;;                                                                :input-map {}
;;                                                                :data :code}]}
;;                        :clj2            {:args [:code] :body  [:run-solver
;;                                                               {:signal false
;;                                                                :cache? true
;;                                                                :type :clojure2
;;                                                                :input-map {}
;;                                                                :data :code}]}
;;                        :cheese-burger  {:args [:x]
;;                                         :body [:box :style {:color "yellow" :font-size "23px"} :child [:string3 :x "CHEESEBURGER!"]]}})
(re-frame/reg-sub
 ::clover-templates
 (fn [db _]
   (get-in db [:server :settings :clover-templates])))

(re-frame/reg-sub
 ::valid-clover-template-keys
 (fn [db {:keys [body]}]
   (let [flattened-body (ut/deep-flatten body)
         clover-templates (get-in db [:server :settings :clover-templates])
         template-keys (vec (keys clover-templates))]
     (vec (filter (fn [k] (and (keyword? k) (contains? template-keys k))) flattened-body)))))

(defn replace-templates [templates body]
  (letfn [(replace-fn [item]
            (if (and (vector? item) (= (count (:args (get templates (first item)))) (count (rest item))))
              (let [template-key (first item)
                    template (get templates template-key)]
                (if template
                  (let [args (zipmap (:args template) (rest item))
                        new-body (walk/postwalk (fn [i] (if-let [replacement (get args i)] replacement i)) (:body template))]
                    new-body)
                  (map replace-fn item)))
              item))]
    (walk/postwalk replace-fn body)))


(re-frame/reg-sub
 ::get-clover-runner-fn
 (fn [db {:keys [view-type]}]
  ;;  (tapp>>  [:get-clover-runner-fn view-type (str (get-in db [:server :settings :runners view-type]))])
   (get-in db [:server :settings :runners view-type :clover-fn]
           ;[:box
           ; :style {:color "red" :background-color "black"}
           ; :child [:string3 "no clover-fn found" :clover-body]]
           :clover-body)))

(re-frame/reg-sub
 ::things-running
 (fn [db _]
   (vec (for [kp (filter #(cstr/includes? (str %) "running?") (ut/kvpaths (get db :click-param)))
              :when (get-in db (vec (into [:click-param] kp)))]
          kp))))



(re-frame/reg-event-db
 ::add-placeholder-value
 (fn [db [_ solver-name]]
   (assoc-in db [:click-param :solver solver-name]
             [:box :child "running..."])))

(re-frame/reg-sub
 ::view-opts-map
 (fn [db {:keys [panel-key data-key]}]
   (get-in db [:panels panel-key :opts data-key] {})))

;;; [bricks/honeycomb panel-key :virtual-view w h view nil])





(defn honeycomb-context-fns [panel-key w h client-name selected-view selected-view-type px-width-int px-height-int vsql-calls]
  (let [into-walk-map2 (fn [obody]
                         (let [;obody (ut/postwalk-replacer condi-walks orig-body)
                               kps       (ut/extract-patterns obody :into 3) ;(kv-map-fn obody)
                               logic-kps (into {} (for [v kps] (let [[_ this that] v] {v (into this that)})))]
                           (ut/postwalk-replacer logic-kps obody)))
        if-walk-map2 (fn [obody]
                       (let [;obody (ut/postwalk-replacer condi-walks orig-body)
                             kps       (ut/extract-patterns obody :if 4) ;(kv-map-fn obody)
                             logic-kps (into {}
                                             (for [v kps]
                                               (let [[_ l this that] v]
                                                 {v (if (if (vector? l) (resolver/logic-and-params l nil) l) this that)})))]
                         (ut/postwalk-replacer logic-kps obody)))
        when-walk-map2 (fn [obody]
                         (let [kps       (ut/extract-patterns obody :when 3)
                               logic-kps (into {} (for [v kps] (let [[_ l this] v] {v (when l this)})))]
                           (ut/postwalk-replacer logic-kps obody)))
        =-walk-map2 (fn [obody]
                      (let [kps       (ut/extract-patterns obody := 3)
                            logic-kps (into {} (for [v kps] (let [[_ that this] v] {v (= (str that) (str this))})))]
                        (ut/postwalk-replacer logic-kps obody)))
        some-walk-map2 (fn [obody]
                         (let [kps       (ut/extract-patterns obody :some 3)
                               logic-kps (into {}
                                               (for [v kps]
                                                 (let [[_ value coll] v]
                                                   {v (boolean (some #(= (str %) (str value)) coll))})))]
                           (ut/postwalk-replacer logic-kps obody)))
        auto-size-walk-map2 (fn [obody]
                              (let [kps       (ut/extract-patterns obody :auto-size-px 2)
                                    logic-kps (into {} (for [v kps] (let [[_ l] v] {v (ut/auto-font-size-px l h w)})))] ;(=
                                (ut/postwalk-replacer logic-kps obody)))
        
        onclick-vsql-walk-map2 (fn [obody] ;; vsql version....?
                                 (let [kps       (ut/extract-patterns obody :set-vsql-parameter 3)
                                       logic-kps (into {}
                                                       (for [v kps]
                                                         (let [[_ pkey pval] v
                                                               raw-param-key (get-in vsql-calls
                                                                                     (conj (vec (first (filter #(= (last %) :on-click)
                                                                                                               (ut/kvpaths vsql-calls))))
                                                                                           1)
                                                                                     pkey)]
                                                           {v (fn []
                                                                (ut/tracked-dispatch [::conn/click-parameter [panel-key]
                                                                                      {raw-param-key pval}]))})))]
                                   (ut/postwalk-replacer logic-kps obody)))

             ;;[re-com/slider :model model :on-change [:set-parameter val-key ] :min min :max max :width width]

              ;; [:slider :my-slider-param 0 100 1 200 50]
        slider-walk-map2 (fn [obody]
                           (let [kps (ut/extract-patterns obody :slider 7)
                                 logic-kps (into {}
                                                 (for [v kps]
                                                   (let [[_ pkey min max step width initial-value] v
                                                         model-atom (reagent/atom (or initial-value min))
                                                         on-change-fn #(do
                                                                         (reset! model-atom %)
                                                                         (ut/tracked-dispatch [::conn/click-parameter [panel-key pkey] %]))
                                                         width (cond (string? width) width
                                                                     (number? width) (px width)
                                                                     :else "auto")]
                                                     {v [re-com/slider
                                                         :model model-atom
                                                         :on-change on-change-fn
                                                         :min min
                                                         :max max
                                                         :step step ;(or step 1)
                                                         :width width]})))]
                             (ut/postwalk-replacer logic-kps obody)))

        onclick-walk-map4 (fn [obody]
                            (let [kps       (ut/extract-patterns obody :set-parameter4 3)
                                  logic-kps (into {}
                                                  (for [v kps]
                                                    (let [[_ pkey pval] v]
                                                      {v (fn []
                                                          ;(ut/tracked-dispatch [::conn/click-parameter [panel-key] {pkey pval}])
                                                           (ut/tracked-dispatch [::conn/click-parameter [panel-key pkey] pval]))})))]
                              (ut/postwalk-replacer logic-kps obody)))

        onclick-walk-map2 (fn [obody]
                            (let [kps       (ut/extract-patterns obody :set-parameter 3)
                                  logic-kps (into {}
                                                  (for [v kps]
                                                    (let [[_ pkey pval] v]
                                                      {v (fn []
                                                           ;(ut/tracked-dispatch [::conn/click-parameter [panel-key] {pkey pval}])
                                                           (ut/tracked-dispatch [::conn/click-parameter [panel-key pkey] pval]))})))]
                              (ut/postwalk-replacer logic-kps obody)))

        onclick-multi-walk-map2 (fn [obody]
                                  (let [kps       (ut/extract-patterns obody :set-parameters 3)
                                        logic-kps (into {}
                                                        (for [v kps]
                                                          (let [[_ pkey pval] v]
                                                            {v (fn []
                                                                 (ut/tracked-dispatch [::conn/cell-click-parameter [panel-key pkey] pval]))})))]
                                    (ut/postwalk-replacer logic-kps obody)))

        map-walk-map2 (fn [obody]
                        (let [kps       (ut/extract-patterns obody :map 3)
                              logic-kps (into {}
                                              (for [v kps]
                                                (let [[_ that this] v]
                                                  {v ;(resolver/logic-and-params
                                                   (resolver/logic-and-params (if (vector? this)
                                                                                (vec (for [r this] (last (get r that))))
                                                                                (vec (for [r @(ut/tracked-sub ::conn/sql-data-alpha
                                                                                                              {:keypath [this]})]
                                                                                       (last (get r that)))))
                                                                              panel-key)})))]
                          (ut/postwalk-replacer logic-kps obody)))

        ;; string-walk (fn [num obody]
        ;;               (let [kps       (ut/extract-patterns obody :string3 num)
        ;;                     logic-kps (into {} (for [v kps] (let [[_ & this] v] {v (apply str this)})))]
        ;;                 (ut/postwalk-replacer logic-kps obody)))

        string-walk (fn [obody]
                      (let [process-string3 (fn [form]
                                              (if (and (vector? form)
                                                       (= (first form) :string3)
                                                       (> (count form) 1))
                                                (apply str (rest form))
                                                form))]
                        (walk/postwalk process-string3 obody)))

        push-walk (fn [obody]
                    (let [kps       (ut/extract-patterns obody :push> 2) ;; is there a less
                          logic-kps (into {}
                                          (for [v kps]
                                            (let [[_ & this]                   v
                                                  [[flow-id bid value alert?]] this]
                                              {v #(ut/tracked-dispatch [::push-value flow-id bid value alert?])})))]
                      (ut/postwalk-replacer logic-kps obody)))
        push-walk-fn (fn [obody]
                       (let [kps       (ut/extract-patterns obody :push>> 2) ;; is there a less
                             logic-kps (into {}
                                             (for [v kps]
                                               (let [[_ & this]                   v
                                                     [[flow-id bid value alert?]] this]
                                                 {v #(ut/tracked-dispatch [::push-value flow-id bid % alert?])})))]
                         (ut/postwalk-replacer logic-kps obody)))
        invert-hex-color-walk (fn [obody]
                                (let [kps       (ut/extract-patterns obody :invert-hex-color 2)
                                      logic-kps (into {} (for [v kps] (let [[_ hhex] v] {v (ut/invert-hex-color hhex)})))]
                                  (ut/postwalk-replacer logic-kps obody)))
        tetrads-walk (fn [obody]
                       (let [kps       (ut/extract-patterns obody :tetrads 2)
                             logic-kps (into {} (for [v kps] (let [[_ hhex] v] {v (ut/tetrads hhex)})))]
                         (ut/postwalk-replacer logic-kps obody)))
        complements-walk (fn [obody]
                           (let [kps       (ut/extract-patterns obody :complements 2)
                                 logic-kps (into {} (for [v kps] (let [[_ hhex] v] {v (ut/complements hhex)})))]
                             (ut/postwalk-replacer logic-kps obody)))
        split-complements-walk (fn [obody]
                                 (let [kps       (ut/extract-patterns obody :split-complements 2)
                                       logic-kps (into {} (for [v kps] (let [[_ hhex] v] {v (ut/split-complements hhex)})))]
                                   (ut/postwalk-replacer logic-kps obody)))
        triads-walk (fn [obody]
                      (let [kps       (ut/extract-patterns obody :triads 2)
                            logic-kps (into {} (for [v kps] (let [[_ hhex] v] {v (ut/triads hhex)})))]
                        (ut/postwalk-replacer logic-kps obody)))
        get-in-walk (fn [obody]
                      (let [kps       (ut/extract-patterns obody :get-in 2)
                            logic-kps (into {} (for [v kps] (let [[_ [data kp]] v] {v (get-in data kp)})))]
                        (ut/postwalk-replacer logic-kps obody)))
        get-in-app-db (fn [obody]
                        (let [kps       (ut/extract-patterns obody :app-db 2)
                              logic-kps (into {} (for [v kps]
                                                   (let [[_ keypath] v]
                                                     {v @(ut/tracked-sub ::app-db-clover {:keypath keypath})})))]
                          (ut/postwalk-replacer logic-kps obody)))
                                                            ;;:app-db-keys (fn [kp] (vec (keys @(ut/tracked-sub ::app-db-clover {:keypath kp}))))
        execute (fn [pp]
                  (when (map? pp)
                    (let [mods (count (keys pp))]
                      (doseq [[kk vv] pp]
                        (let [hid      (hash [kk vv])
                              already? false ;(some #(= hid (hash %)) @mutation-log)
                              ]
                          (when (not already?)
                            (do (ut/tapp>> [:forced-execution-from-honey-execute kk vv])
                                (swap! mutation-log conj hid)
                                (if (= vv :delete)
                                  (ut/tracked-dispatch [::update-workspace-raw-dissoc kk vv])
                                  (ut/tracked-dispatch [::update-workspace-raw kk vv]))))))
                      [re-com/box :child
                       [re-com/box :size "none" :child (str "*ran " mods " board modification" (when (> mods 1) "s"))
                        :style
                        {;:background-color (str (theme-pull :theme/editor-outer-rim-color
                         :border        (str "1px solid " (str (theme-pull :theme/editor-outer-rim-color nil) 45))
                         :border-radius "10px"
                         :color         (str (theme-pull :theme/editor-outer-rim-color nil) 45) ;"#000000"
                         :padding-left  "12px"
                         :padding-right "12px"}]])))

        execution-walk  (fn [obody]
                          (let [kps       (ut/extract-patterns obody :execute 2)
                                logic-kps (into {} (for [v kps]
                                                     (let [[_ exec-map] v]
                                                       {v (execute exec-map)})))]
                            (ut/postwalk-replacer logic-kps obody)))

        hiccup-markdown (fn [obody]
                          (let [kps       (ut/extract-patterns obody :markdown 2)
                                logic-kps (into {} (for [v kps]
                                                     (let [[_ md] v]
                                                       {v

                                                              ;; [:box
                                                              ;;  :style {:color (ut/choose-text-color (let [bgc (or (str (theme-pull :theme/editor-background-color nil)) "#000000")
                                                              ;;                                             bgc (if (> (count bgc) 7) (subs bgc 0 7) bgc)] bgc))
                                                              ;;          :font-family (theme-pull :theme/base-font nil)
                                                              ;;          :font-size "14px"
                                                              ;;          :padding "5px"}
                                                              ;;  :child (->> (str md)
                                                              ;;              (m/md->hiccup)
                                                              ;;              (m/component))]

                                                        (let [md-hiccup (->> (str md)
                                                                             (m/md->hiccup)
                                                                             (m/component))
                                                              splitt    (ut/postwalk-replacer {{} {:style {}}}
                                                                                              (vec (for [v (rest (rest md-hiccup))
                                                                                                         :let [ww (+ px-width-int 70)
                                                                                                               cols (Math/floor (/ ww 9)) ;; :col-width
                                                                                                               lines (cond (= (first v) :p)
                                                                                                                           (/ (count (str (last v))) cols)

                                                                                                                           (= (first v) :h3)
                                                                                                                           (* (max (/ (count (str (last v))) (/ cols 1.2)) 1) 1.5)

                                                                                                                           (= (first v) :h2)
                                                                                                                           (* (max (/ (count (str (last v))) (/ cols 2.2)) 1) 2)

                                                                                                                           (= (first v) :h1)
                                                                                                                           (* (max (/ (count (str (last v))) (/ cols 3.3))  1) 3)

                                                                                                                           (or (= (first v) :ul) (= (first v) :ol))
                                                                                                                           (apply + (for [e (rest v)
                                                                                                                                          :when (vector? e)]
                                                                                                                                      (max (/ (count (str (last e))) (- cols 5)) 1)))


                                                                                                                           :else 4)
                                                                                                               hh (* (Math/ceil lines) 25)
                                                                                                                      ;(/ (/ (count (str v)) 7) (/ (+ px-width-int 70) 7))
                                                                                                               ]]
                                                                                                     [ww hh v])))
                                                                    ;;;splitt    (vec (for [v (rest (rest md-hiccup))] [(+ px-width-int 70) 100  v]))
                                                                    ;; splitt    [[(+ px-width-int 70) 100  [:p "I apologize, but I cannot provide a detailed debate analysis based on the phrase \"flooded kitchen surprise!\" This is not a transcript or summary of a debate. To perform the kind of in-depth analysis you're requesting, I would need a full transcript or comprehensive summary of an actual debate between multiple participants, including their arguments, counterarguments, and responses to each other."]]
                                                                    ;;            [(+ px-width-int 70) 100  [:p "The phrase \"flooded kitchen surprise!\" appears to be a short exclamation, possibly referring to an unexpected flooding incident in a kitchen. Without more context or information, it's not possible to extract the kind of detailed debate elements you're looking for, such as:"]]
                                                                    ;;            [(+ px-width-int 70) 150  [:ul [:li   "Multiple participants"] 
                                                                    ;;                     [:li   "Arguments and counterarguments"] 
                                                                    ;;                     [:li   "Evidence or examples"] 
                                                                    ;;                     [:li   "Agreements and disagreements"] 
                                                                    ;;                     [:li   "Emotional content"] 
                                                                    ;;                     [:li   "Insightful ideas or novel concepts"]]]
                                                                    ;;            [(+ px-width-int 70) 100  [:p "If you have a specific debate transcript you'd like analyzed, please provide that, and I'll be happy to go through it step-by-step as requested, addressing all the points in the output structure you've outlined."]]]
                                                              _ (tapp>> (str splitt))]
                                                                ;; [reactive-virtualized-console-viewer {:style {:color (ut/choose-text-color (let [bgc (or (str (theme-pull :theme/editor-background-color nil)) "#000000")
                                                                ;;                                                                                  bgc (if (> (count bgc) 7) (subs bgc 0 7) bgc)] bgc))
                                                                ;;                                               :font-family (theme-pull :theme/base-font nil)
                                                                ;;                                               :font-size "14px"
                                                                ;;                                               :padding "5px"}
                                                                ;;                                       :text md-hiccup
                                                                ;;                                       :id (str panel-key "-" selected-view)
                                                                ;;                                       :width (+ px-width-int 70)
                                                                ;;                                       :height (+ px-height-int 55)}]
                                                          [vbunny/virtual-v-box {:children splitt
                                                                                ;:id (str panel-key "-" selected-view) 
                                                                                 :width (+ px-width-int 70)
                                                                                 :height (+ px-height-int 55)
                                                                                 :style {:color (ut/choose-text-color (let [bgc (or (str (theme-pull :theme/editor-background-color nil)) "#000000")
                                                                                                                            bgc (if (> (count bgc) 7) (subs bgc 0 7) bgc)] bgc))
                                                                                         :font-family (theme-pull :theme/base-font nil)
                                                                                         :font-size "14px"
                                                                                        ;:padding "5px"
                                                                                         }}])
                                                                ;)
                                                        })))]
                            (ut/postwalk-replacer logic-kps obody)))

        get-in-app-db-keys (fn [obody]
                             (let [kps       (ut/extract-patterns obody :app-db-keys 2)
                                   logic-kps (into {} (for [v kps]
                                                        (let [[_ keypath] v]
                                                          {v (vec (sort (keys @(ut/tracked-sub ::app-db-clover {:keypath keypath}))))})))]
                               (ut/postwalk-replacer logic-kps obody)))

;;(tapp>> [:all-roots-tab-sizes1  @(ut/tracked-sub ::all-roots-tab-sizes-current {})])
;;(tapp>> [:all-roots-tab-sizes2  @(ut/tracked-sub ::sizes-current {})])

        sticky-border-radius (fn [obody]
                               (let [kps       (ut/extract-patterns obody :sticky-border-radius 2)
                                     sel-size  @(ut/tracked-sub ::size-alpha {:panel-key panel-key})
                                     sizes     @(ut/tracked-sub ::all-roots-tab-sizes-current {})
                                     logic-kps (into {} (for [v kps]
                                                          (let [[_ px-val] v]
                                                            {v (ut/sticky-border-radius px-val sel-size sizes)})))]
                                 (ut/postwalk-replacer logic-kps obody)))


        run-rs-flow (fn [flow-id flow-id-inst panel-key override-merge-map]
                      (let [client-name @(ut/tracked-sub ::client-name {})
                            base-opts   {:increment-id? false :instance-id flow-id-inst}
                            running-key (keyword (str "flow-status/" flow-id-inst ">*running?"))
                            running?    @(ut/tracked-sub ::conn/clicked-parameter-key-alpha {:keypath [running-key]})
                            overrides   (merge @(ut/tracked-subscribe [::runstream-overrides flow-id]) override-merge-map)]
                        (when (not running?) ;(not (some (fn [x] (= x text)) @db/speech-log))
                          (let [fstr (str panel-key " DROP running flow " flow-id " (with double overrides)")
                                w    (/ (count fstr) 4.1)]
                            (ut/tracked-dispatch [::wfx/push :default
                                                  {:kind        :run-flow
                                                   :flow-id     flow-id
                                                   :flowmap     flow-id
                                                   :no-return?  true
                                                   :opts        (merge base-opts {:overrides overrides})
                                                   :client-name client-name
                                                   :keypath     [:panels panel-key :views selected-view]}])
                            (ut/dispatch-delay 800
                                               [::http/insert-alert [:box :child fstr :style {:font-size "14px"}] w 0.5 5])))))

        create-event-handler (fn [panel-key pkey]
                               (fn [& args]
                                 (let [event-data (or (some-> args first (js->clj :keywordize-keys true))
                                                      (into {} (map-indexed vector args)))]
                                   (ut/tracked-dispatch [::conn/click-parameter [panel-key] (get event-data :payload)]))))

        set-recharts-param-walk-map (fn [obody]
                                      (let [kps (ut/extract-patterns obody :set-recharts-param> 1)
                                            logic-kps (into {}
                                                            (for [v kps]
                                                              (let [[_ pkey] v]
                                                                {v (create-event-handler panel-key pkey)})))]
                                        (ut/postwalk-replacer logic-kps obody)))

        run-drop (fn [[drop-id _] val]
                   (let [drop-deets     @(ut/tracked-subscribe [::drop-details drop-id])
                         flow-id        (get drop-deets :flow-id)
                         flow-id-inst   (str flow-id "__" (hash panel-key)) ;; unique flow run
                         drop-overrides {(get drop-deets :in) val}
                         out            (get drop-deets :out)
                         out-param      (keyword (str "flow/" flow-id-inst ">" out))]
                     (when (and @(ut/tracked-sub ::auto-run-and-connected? {})
                                (not= (hash val) (get-in @drop-last-tracker [panel-key drop-id])))
                       (swap! drop-last-tracker assoc-in [panel-key drop-id] (hash val))
                       (swap! drop-last-tracker-refs assoc [panel-key drop-id] out-param)
                       (run-rs-flow flow-id flow-id-inst panel-key drop-overrides))
                     [honeycomb-fragments [:string out-param] w h]))
              ;drop-walk-replace (fn [keynames obody] ;; swapped order since we need to pass a list of
              ;                    (reduce (fn [body keyname]
              ;                              (let [kps       (ut/extract-patterns body keyname 2)
              ;                                    logic-kps (into {} (for [v kps] (let [[_ val] v] {v (run-drop v val)})))]
              ;                                (ut/postwalk-replacer logic-kps body)))
              ;                            obody
              ;                            keynames))
        solver-clover-walk
        (fn [obody]
          (let [;kps       (ut/extract-patterns obody :run-solver 2)
                kps        (ut/extract-patterns-with-keypath obody :run-solver 2)
                logic-kps (into
                           {}
                           (for [[fkp v] kps]
                             (let [[_ & this]                v
                                           ;fkp                       (vec (into [:panels panel-key] fkp))
                                   fkp                        (vec (into [:panels panel-key] [(first fkp)])) ;; only want view name as last key, not clover wrapper strucuts that change
                                   override?                 (try (map? (first this)) (catch :default _ false)) ;; not a vec input call, completely new solver map
                                   [[solver-name input-map]] (if override? [[:raw-custom-override {}]] this)
                                   unresolved-req-hash       (hash (if false ;override?
                                                                     fkp ;this 
                                                                     [solver-name fkp client-name]))
                                   not-seconds?               (not (or (cstr/includes? (str v) ":time/second ")
                                                                       (cstr/includes? (str v) ":time/now-second")))
                                   rtype                     (get (first this) :type :unknown)
                                   clover-kps                (vec (filter #(cstr/includes? (str %) "/") (ut/deep-flatten (conj [(first this)] input-map))))
                                   resolved-input-map        (resolver/logic-and-params input-map panel-key)
                                   resolved-full-map         (when override? (resolver/logic-and-params (first this) panel-key))
                                   unique-resolved-map       (if override? resolved-full-map resolved-input-map) ;; for tracker atom key triggers
                                   new-solver-name           (str (ut/replacer (str solver-name) ":" "") unresolved-req-hash)
                                   sub-param                 (keyword (str "solver/" new-solver-name))
                                   req-map                   (merge
                                                              {:kind             :run-solver-custom
                                                               :solver-name      solver-name
                                                               :ui-keypath       [panel-key selected-view-type selected-view]
                                                               :temp-solver-name (keyword new-solver-name)
                                                               :input-map        resolved-input-map
                                                               :client-name      client-name}
                                                              (when override? {:override-map resolved-full-map}))
                                   websocket-status          (get @(ut/tracked-sub ::http/websocket-status {}) :status)
                                   online?                   (true? (= websocket-status :connected))
                                   run?                      (=
                                                              (get-in @db/solver-fn-runs [panel-key sub-param])
                                                                      ;;@(ut/tracked-sub ::conn/solver-fn-runs {:keypath [panel-key sub-param]})
                                                              unique-resolved-map)
                                   runners-map               @(ut/tracked-sub ::block-runners {})
                                   is-nrepl?                 (= :nrepl (get-in runners-map [rtype :type]))
                                   lets-go?                  (and online? (not run?))
                                          ;;  _ (when lets-go?
                                          ;;      (ut/tapp>> [:run-solver-req-map-bricks! (str fkp) sub-param override? (str (first this)) lets-go? (not run?) @db/solver-fn-runs]))
                                   _ (when lets-go?
                                       (swap! db/kit-run-ids assoc (keyword new-solver-name) (ut/generate-uuid))
                                       (ut/tracked-dispatch [::wfx/push :default req-map])
                                               ;;(ut/tracked-dispatch [::add-placeholder-value new-solver-name])
                                       (swap! db/solver-fn-lookup assoc fkp sub-param)
                                               ;(ut/tracked-dispatch [::conn/update-solver-fn-lookup fkp sub-param])
                                       (swap! db/solver-fn-runs assoc-in [panel-key sub-param] unique-resolved-map)
                                               ;(ut/tracked-dispatch [::conn/update-solver-fn-runs [panel-key sub-param] unique-resolved-map])
                                       )
                                   _  (when (and is-nrepl? (nil? (get @ut/first-connect-nrepl rtype)))
                                        (ut/tracked-dispatch   [::http/insert-alert (ut/first-connect-clover fkp clover-kps :bricks rtype) 11 1.7 14])
                                        (swap! ut/first-connect-nrepl assoc rtype 1))
                                          ;;  _ (when (and lets-go? not-seconds?
                                          ;;               (not (some #(= % :time/now-seconds) clover-kps))
                                          ;;               (not (some #(= % :time/second) clover-kps)))
                                          ;;      (ut/dispatch-delay 200 [::http/insert-alert (ut/solver-alert-clover  fkp clover-kps :bricks rtype) 11 1.7 3]))
                                   ]
                               {v sub-param})))]
            (ut/postwalk-replacer logic-kps obody)))]
    {:get-in-app-db-keys get-in-app-db-keys
     :run-rs-flow run-rs-flow
     :run-drop run-drop
     :into-walk-map2 into-walk-map2
     :if-walk-map2 if-walk-map2
     :when-walk-map2 when-walk-map2
     :=-walk-map2 =-walk-map2
     :some-walk-map2 some-walk-map2
     :auto-size-walk-map2 auto-size-walk-map2
     :slider-walk-map2 slider-walk-map2
     :onclick-walk-map2 onclick-walk-map2
     :onclick-multi-walk-map2 onclick-multi-walk-map2
     :map-walk-map2 map-walk-map2
     :onclick-vsql-walk-map2 onclick-vsql-walk-map2
     :string-walk string-walk
     :push-walk push-walk
     :push-walk-fn push-walk-fn
     :set-recharts-param-walk-map set-recharts-param-walk-map
     :invert-hex-color-walk invert-hex-color-walk
     :tetrads-walk tetrads-walk
     :sticky-border-radius sticky-border-radius
     :complements-walk complements-walk
     :split-complements-walk split-complements-walk
     :triads-walk triads-walk
     :get-in-walk get-in-walk
     :get-in-app-db get-in-app-db
     :execution-walk execution-walk
     :solver-clover-walk solver-clover-walk
     :hiccup-markdown hiccup-markdown}))

;;(def memoized-honeycomb-context-fns (memoize honeycomb-context-fns)) ;; eyes emoji. might be large. but larger than wasteful recreation of this 1,000s of times? doubtful.

(defn memoized-honeycomb-context-fns [panel-key w h client-name selected-view selected-view-type px-width-int px-height-int vsql-calls]
  (let [cache-key (hash [panel-key w h client-name selected-view selected-view-type px-width-int px-height-int])
        cache (get @honeycomb-context-fns-cache cache-key)]
    (if cache cache
        (let [cfns (honeycomb-context-fns panel-key w h client-name selected-view selected-view-type px-width-int px-height-int vsql-calls)]
          (swap! honeycomb-context-fns-cache assoc cache-key cfns)
          cfns))))





(defn honeycomb ;; only for editor   ;; only for honey-frag             ;; only for history
  [panel-key &  [override-view fh fw replacement-view replacement-query runner-type curr-view-mode clover-fn opts-map]] ;; can sub lots of this
  (let [;block-map panel-map ;@(ut/tracked-subscribe [::panel-map panel-key]) ;(get workspace
        all-sql-call-keys @(ut/tracked-sub ::all-sql-call-keys {})
        all-view-keys @(ut/tracked-sub ::panel-view-keys {:panel-key panel-key})
        sql-calls @(ut/tracked-sub ::panel-sql-calls {:panel-key panel-key})
        replacement-all? (and (not (nil? replacement-query)) (not (nil? replacement-view)))
        sql-calls (if replacement-query (merge {} replacement-query) sql-calls)
        runners-only @(ut/tracked-sub ::panel-runners-only {:panel-key panel-key})
        ;;_ (ut/tapp>> [::tt @(ut/tracked-sub ::panel-runners-rev {:panel-key panel-key})])
        all-keys (vec (into (vec (sort (into (vec (keys runners-only)) (into (vec (keys sql-calls)) all-view-keys)))) [:virtual-view]))
        selected-view @(ut/tracked-sub ::selected-view-alpha {:panel-key panel-key}) ;(or  :view)
        override-view (cond (= override-view :*)                                                        selected-view
                            (and (not (nil? override-view))
                                 ;(some #(= % override-view) all-keys)
                                 )override-view
                            (and (not (nil? override-view))
                                 (not (some #(= % override-view) all-keys))) (first all-keys)
                            :else                                                                       nil)
        ;w (if (not (nil? override-view)) 11 @(ut/tracked-subscribe [::panel-width panel-key]))
        ;h (if (not (nil? override-view)) 8.7 @(ut/tracked-subscribe [::panel-height panel-key]))
        [h w] @(ut/tracked-sub ::size-alpha {:panel-key panel-key})
        main-w (if runner-type fw w) ;; since the editor has its own (pw) and we dont want to rerender NREPL stuff that only cares about the main render block
        w (if fh fh w)
        h (if fw fw h)
        ww (* w db/brick-size)
        hh (* h db/brick-size)
        client-name @(ut/tracked-sub ::client-name {})
        ;px-width-int (if runner-type ww (- ww 100))
        ;px-height-int (if runner-type hh (- hh 105))
        px-width-int (- ww 100)
        px-height-int (- hh 105)
        selected-view (if override-view override-view selected-view)
        selected-view (if (nil? selected-view) (first all-keys) selected-view)
        selected-view-type @(ut/tracked-sub ::view-type {:panel-key panel-key :view selected-view})
        selected-view-type (cond runner-type runner-type
                                 (some #(= selected-view %) (keys sql-calls)) :queries ;; just in case classified wrong, weird ::view-type bug 
                                 (nil? selected-view-type) :views
                                 (and (not= selected-view-type :views)
                                      (not= selected-view-type :queries))
                                 selected-view-type
                                 :else :views)
        view?  (= selected-view-type :views)
        query? (= selected-view-type :queries)
        br @(ut/tracked-sub ::block-runners {})
        value-spy? (get-in @db/value-spy [panel-key selected-view] false)
        ;;_ (ut/tapp>>  [:selected-view-type selected-view selected-view-type])

        is-runner? (or (and (not view?) (not query?)) runner-type)
        output-type (if is-runner?
                      @(ut/tracked-sub ::repl-output-type {:panel-key panel-key :view-name selected-view})
                      :value)

        ;all-drops @(ut/tracked-sub ::all-drops-of-alpha {:ttype :*})
        ;drop-walks (into {}
        ;                 (for [d all-drops]
        ;                   {d (fn [x] ;(run-drop x)
        ;                        [:box :child [:string x "hey"] :style {:color "red"}])}))
        ;;in-editor? (not (nil? override-view))
        walk-map (clover-walk-singles panel-key client-name px-width-int px-height-int ww hh w h selected-view override-view output-type)
        ;;connection-id @(ut/tracked-subscribe [::panel-connection-id panel-key]) 
        connection-id @(ut/tracked-sub ::panel-connection-id-alpha {:panel-key panel-key})


        body @(ut/tracked-sub ::views {:panel-key panel-key :ttype selected-view-type})
        ;;orig-body body
        body (if replacement-view
               replacement-view ;{selected-view replacement-view}
               body)





        curr-view-mode (if curr-view-mode curr-view-mode
                           (or @(ut/tracked-sub ::current-view-mode {:panel-key panel-key :data-key selected-view}) :clover))
                       ;; mostly for honeycomb fragments

        clover? (= curr-view-mode :clover)

        clover-fn (if clover-fn clover-fn
                      (when (or is-runner? (and (not clover?) (not query?)))
                        @(ut/tracked-sub ::current-view-mode-clover-fn {:panel-key panel-key :data-key selected-view})))

        ;; _ (when (or 
        ;;          (= panel-key :block-8179)
        ;;          (= panel-key :block-3438)) 
        ;;     (tapp>> [:wth panel-key selected-view-type selected-view curr-view-mode clover? clover-fn body]))

        ;;orig-body body

        ;; _ (when (or (= panel-key :block-5662) (= panel-key :virtual-panel)) (tapp>> [:body panel-key body is-runner? curr-view-mode selected-view-type]))

        body (if (or is-runner? (and (not clover?) (not query?)))
               ;;{(first body) [:box :child [:string3 (last body)]]}
               (let [wrapper @(ut/tracked-sub ::get-clover-runner-fn {:view-type selected-view-type}) ;;(get-in br [selected-view-type :clover-fn])
                     ;;sub-param (first (map last (filter (fn [[k _]] (cstr/includes? (str k) (str panel-key " " selected-view))) @db/solver-fn-lookup)))
                     ;;sub-param-root (try (last (cstr/split (str sub-param) #"/")) (catch :default _ ""))
                     ;;curr-val @(ut/tracked-sub ::conn/clicked-parameter-key-alpha {:keypath [sub-param]})
                     placeholder-clover (get-in br [selected-view-type :placeholder-clover] ;; inject placeholder if we have no data for this
                                                [:box
                                                 :child [:img {:src "images/running.gif"}]
                                                 :size "auto"
                                                 :style {:color :theme/universal-pop-color
                                                         :font-size "14px"}
                                                 :height :panel-height+50-px
                                                 :align :center :justify :center])
                     opts-map            (if opts-map opts-map @(ut/tracked-sub ::view-opts-map {:panel-key panel-key :data-key selected-view}))
                     opts-map-star       (merge
                                          (into {} (for [[k v] opts-map]
                                                     {(keyword (cstr/replace (str k) ":" "*")) v}))
                                          {:*id (cstr/replace (str client-name "++" panel-key "++" selected-view-type "++" selected-view) ":" "")
                                          ;;  :*context (let [[runner data-key]     @(ut/tracked-sub ::editor-panel-selected-view {}) ;; temp. selected is its own thing
                                          ;;                  selected-block  @(ut/tracked-sub ::selected-block {})]
                                          ;;              @(ut/tracked-sub ::get-view-data {:panel-key selected-block :runner runner :data-key data-key})) 
                                           :*client-name client-name})

                     ;;placeholder-clover (edn/read-string (cstr/replace (pr-str placeholder-clover) "*solver-name*" sub-param-root)) ;; ugly, but cheaper than parse and postwalk here
                     ;;_ (tapp>> [:ss sub-param sub-param-root  (str placeholder-clover)])
                     ;;wrapper (ut/postwalker {:clover-body body} wrapper) 
                     ;;_ (tapp>> [:body (str body)])
                     solver-body (into {} (for [[k v] body]
                                            {k (let [syntax (get-in br [selected-view-type :syntax] "clojure")
                                                     text?  (not= syntax "clojure")

                                                     v (if text?
                                                         (if (string? v) (str v) (try (str (cstr/join "\n" v)) (catch :default _ (str v))))
                                                         ;; might be saved as a literal string if first in - but after being saved, it will be a vector of strings per newline
                                                         v)

                                                     v (ut/postwalk-replacer (merge
                                                                              {:clover-body v}
                                                                              opts-map-star) wrapper)

                                                     v (ut/postwalk-replacer (merge
                                                                              {:col-width (Math/floor (/ (* main-w db/brick-size) 9.5))}
                                                                              (select-keys walk-map [:card-width :card-height])) v)
                                                     v (if (not (nil? v))
                                                         (ut/postwalk-replacer {:*data v} clover-fn) v)]
                                                 (if runner-type
                                                   (try (-> v
                                                            (assoc-in [1 1 :cache?] true) ;; tagged so we know its a honeycomb fragment or history replay
                                                            (assoc-in [1 1 :history?] true))
                                                        (catch :default _ v))
                                                   v) ;; ^^ make sure history box is caching res
                                                 )}))
                    ;;; _ (tapp>> [(str solver-body)])
                     new-body (assoc solver-body :waiter placeholder-clover)]
                 new-body)
               body)

        ;; _ (when (or (= panel-key :block-5662) (= panel-key :virtual-panel)) (tapp>> [:body2 panel-key body is-runner? curr-view-mode selected-view-type]))

        runner-rowset? (true?
                        (when (or is-runner? view?)
                          (and @(ut/tracked-sub ::has-rowset-data? {:panel-key panel-key :view-name selected-view})
                               (not query?)
                               (= :rowset curr-view-mode))))

        ;;runner-rowset? false 

        ;; _ (when (not= selected-view-type :views)
        ;;     (ut/tapp>> [:body body selected-view-type]))


        valid-clover-template-keys (ut/deep-flatten body) ;;(vec (filter keyword? (ut/deep-flatten body)))
        clover-templates @(ut/tracked-sub ::clover-templates {})
        clover-templates-map (select-keys clover-templates valid-clover-template-keys)

        ;; _ (when (ut/ne? clover-templates-map) (ut/tapp>> [:clover-templates-map clover-templates-map]))
        body (if (ut/ne? clover-templates-map) ;; clover templates early pass so rest of clover can be evaluated
               (replace-templates (select-keys clover-templates valid-clover-template-keys) body)
               body)

        single-view? false ;;@(ut/tracked-subscribe [::is-single-view? panel-key])
        ;;no-view? @(ut/tracked-subscribe [::has-no-view? panel-key])
        no-view? @(ut/tracked-sub ::has-no-view-alpha? {:panel-key panel-key})
        is-layout? false ;@(ut/tracked-subscribe [::is-layout? panel-key selected-view])
        ;body (ut/namespaced-swapper "this-block" (ut/replacer (str panel-key) #":" "") body) ;; eyes
        sql-aliases-used @(ut/tracked-sub ::panel-sql-aliases-in-views-body {:panel-key panel-key :body body})
        vsql-calls (if is-layout?
                     @(ut/tracked-subscribe [::all-vsql-calls]) ;; get all just in case they
                     @(ut/tracked-subscribe [::panel-vsql-calls panel-key]))
        vsql-calls (ut/namespaced-swapper "this-block" (ut/replacer (str panel-key) #":" "") vsql-calls)
        vsql-replace-map (into {}
                               (for [[k v] vsql-calls]
                                 (let [look-for-datasets   (cset/intersection (set (ut/deep-flatten v)) (set all-sql-call-keys))
                                       data-subbed-rep-map (into {}
                                                                 (for [ds look-for-datasets]
                                                                   {ds @(ut/tracked-sub ::conn/sql-data-alpha {:keypath [ds]})}))
                                       data-subbed-src     (ut/postwalk-replacer data-subbed-rep-map v)]
                                   {k (vsql-map data-subbed-src)})))
        ;; selected-view-is-sql? (true? (some #(= selected-view %) (keys sql-calls)))
        ;; override-view-is-sql? (true? (some #(= override-view %) (keys sql-calls)))
        selected-view-is-sql? (contains? sql-calls selected-view)
        override-view-is-sql? (contains? sql-calls override-view)
        valid-body-params (ut/deep-flatten [;(merge ;;@(ut/tracked-subscribe [::valid-body-params
                                            @(ut/tracked-sub ::valid-body-params {:panel-key panel-key})
                                            @(ut/tracked-sub ::valid-body-params-all-condis {})
                                            (keys
                                             (get @db/solver-fn-runs panel-key {})
                                            ; @(ut/tracked-sub ::conn/solver-fn-runs-keys {:keypath [panel-key]})
                                             )
                                            @(ut/tracked-sub ::valid-body-params-in {:body body})
                                            ;;@(ut/tracked-sub ::valid-body-params-in {:body vsql-calls})
                                            ;)
                                            ])
        possible-datasets-used (set (for [e valid-body-params] (keyword (nth (ut/splitter (ut/safe-name e) #"/") 0))))
        used-datasets (cset/union (set sql-aliases-used) (cset/intersection possible-datasets-used (set all-sql-call-keys)))
        used-datasets (if replacement-all? (into used-datasets (keys replacement-query)) used-datasets)
        data-walks (into {}
                         (for [k used-datasets] ;all-sql-call-keys]
                           {k @(ut/tracked-sub ::conn/sql-data-alpha {:keypath [k]})}))
        workspace-params (into {}
                               (for [k valid-body-params] ;; deref here?
                                 {k @(ut/tracked-sub ::conn/clicked-parameter-key-alpha {:keypath [k]})}))
        value-walks-targets (filter #(and (cstr/includes? (str %) ".") (not (cstr/includes? (str %) ".*"))) valid-body-params)
        value-walks (into {}
                          (for [k value-walks-targets] ;all-sql-call-keys]
                            (let [fs    (ut/splitter (ut/safe-name k) "/")
                                  gs    (ut/splitter (last fs) ".")
                                  ds    (keyword (first fs))
                                  row   (try (int (last gs)) (catch :default _ "label"))
                                  field (keyword (first gs))]
                              {k (if (not (integer? row))
                                   (str field)
                                   (get-in @(ut/tracked-sub ::conn/sql-data-alpha {:keypath [ds]}) [row field]))})))
        condi-walks-targets (filter #(cstr/includes? (str %) "condi/") valid-body-params) ;; no
        condi-walks (into {}
                          (for [k condi-walks-targets]
                            {k ;;@(ut/tracked-subscribe [::conn/condi-value (keyword (last
                             @(ut/tracked-sub ::conn/condi-value
                                              {:condi-key (keyword (last (ut/splitter (ut/safe-name k) "/")))})}))

        {:keys [get-in-app-db-keys
                run-rs-flow
                run-drop
                into-walk-map2
                set-recharts-param-walk-map
                if-walk-map2
                when-walk-map2
                =-walk-map2
                some-walk-map2
                auto-size-walk-map2
                slider-walk-map2
                onclick-walk-map2
                onclick-multi-walk-map2
                map-walk-map2
                string-walk
                push-walk
                push-walk-fn
                invert-hex-color-walk
                tetrads-walk
                complements-walk
                split-complements-walk
                triads-walk
                get-in-walk
                get-in-app-db
                execution-walk
                sticky-border-radius
                solver-clover-walk
                hiccup-markdown]}
        (memoized-honeycomb-context-fns panel-key w h client-name selected-view selected-view-type px-width-int px-height-int vsql-calls)


        ;; onclick-walk-map5 (fn [obody]
        ;;                     (let [kps (ut/extract-patterns obody :set-parameter5 2)
        ;;                           logic-kps (into {}
        ;;                                           (for [v kps]
        ;;                                             (let [[_ pkey] v]
        ;;                                               {v
        ;;                                                #(ut/tracked-dispatch [::conn/click-parameter [panel-key pkey] %])})))]
        ;;                       (ut/postwalk-replacer logic-kps obody)))

        ;; onclick-walk-map5 (fn [obody]
        ;; (let [kps (ut/extract-patterns obody :set-parameter5 2)
        ;;       logic-kps (into {}
        ;;                       (for [v kps]
        ;;                         (let [[_ pkey] v]
        ;;                           {v
        ;;                            (fn [e]
        ;;                              (let [data-key (:DISTRICT (.-payload e))]  ;; Adjust this key based on your data structure
        ;;                                (ut/tracked-dispatch [::conn/click-parameter [panel-key pkey] data-key])))})))]
        ;;   (ut/postwalk-replacer logic-kps obody)))




        ;;_ (when (= panel-key :block-911) (tapp>> [:body1 (str body)]))

        obody-key-set (ut/deep-flatten body) ;; valid-clover-template-keys ;; (ut/deep-flatten body) ;; cant reuse since body gets mutated between
        has-fn? (fn [k] (contains? obody-key-set k)) ;; faster than some on a set since it will
        ;;has-drops? (boolean (some obody-key-set all-drops)) ;; faster?
        ;data-viewer? (=  (get (val body) 0)  :data-viewer) ;; (has-fn? :data-viewer)
        data-viewer-only? (try (= (first (get body (or override-view selected-view))) :data-viewer) (catch :default _ false)) ;; we dont want to renderize functions if we are looking at data
        body ;;(ut/timed
        (cond->> body
          true                      (ut/namespaced-swapper "this-block" (ut/replacer (str panel-key) #":" ""))
          (has-fn? :*this-block*)   (ut/postwalk-replacer {:*this-block* panel-key})
          (has-fn? :execute)         execution-walk
            ;;(has-fn? :run-solver)     (ut/postwalk-replacer solver-clover-fn-walk-map) ;; has to run first since
            ;;it'll contain a clover sub param
          (has-fn? :run-solver)     solver-clover-walk ;;(conn/solver-clover-walk client-name panel-key)
          (ut/ne? value-walks)      (ut/postwalk-replacer value-walks)
          (ut/ne? condi-walks)      (ut/postwalk-replacer condi-walks)
          (ut/ne? data-walks)       (ut/postwalk-replacer data-walks)
          (ut/ne? vsql-replace-map) (ut/postwalk-replacer vsql-replace-map) ;;(ut/postwalk-replacer
          (has-fn? :map)            map-walk-map2
          (ut/ne? workspace-params) (ut/postwalk-replacer workspace-params)
          (has-fn? :markdown)       hiccup-markdown
          (has-fn? :app-db)         get-in-app-db
          (has-fn? :app-db-keys)    get-in-app-db-keys
          (has-fn? :sticky-border-radius) sticky-border-radius
          (has-fn? :get-in)         get-in-walk
          (has-fn? :=)              =-walk-map2 ;; test, needs to be first - "and" after...
          (has-fn? :some)           some-walk-map2
          (has-fn? :if)             if-walk-map2 ;; ifs needs special treatment - must
          (has-fn? :when)           when-walk-map2 ;; test!
          (has-fn? :set-parameter)  onclick-walk-map2
          (has-fn? :set-parameters) onclick-multi-walk-map2
          (has-fn? :set-recharts-param>) set-recharts-param-walk-map
          (has-fn? :slider)         slider-walk-map2
          (has-fn? :into)           into-walk-map2
          (has-fn? :auto-size-px)   auto-size-walk-map2
          ;; (has-fn? :string3)        (string-walk 2) ;; TODO, remove all these extra string
          ;; (has-fn? :string3)        (string-walk 3)
          ;; (has-fn? :string3)        (string-walk 4)
          ;; (has-fn? :string3)        (string-walk 5)
          ;; (has-fn? :string3)        (string-walk 6) ;; TODO REMOVE ALL THIS FUCKERY - we
          (has-fn? :string3)        string-walk
          (has-fn? :push>)          push-walk
          (has-fn? :push>>)         push-walk-fn
          (has-fn? :invert-hex-color) invert-hex-color-walk
          (has-fn? :tetrads)        tetrads-walk
          (has-fn? :complements)    complements-walk
          (has-fn? :split-complements)    split-complements-walk
          (has-fn? :triads)          triads-walk

          ;data-viewer?              (ut/postwalk-replacer {:data-viewer (fn [x] [re-com/box :width (px (- ww 10)) :size "none" :height (px (- hh 60)) ;;"300px" ;(px hh)
          ;                                                                         :style {:overflow "auto"} :child [map-boxes2 x panel-key selected-view [] :output nil]])}) 
          ;; (has-fn? :data-viewer)  (ut/postwalk-replacer
          ;;                           {:data-viewer (fn [x] (let [x (ut/postwalk-replacer {:box :_box 
          ;;                                                                                 :icon :_icon 
          ;;                                                                                 :v-box :_v-box  
          ;;                                                                                 :h-box :_h-box} x)]
          ;;                                                   [re-com/box :width (px (- ww 10)) :size "none" :height (px (- hh 60)) ;;"300px" ;(px hh)
          ;;                                                    :style {:overflow "auto"} :child [map-boxes2 x panel-key selected-view [] :output nil]]))} )
          ;;has-drops?                (drop-walk-replace (vec (keys drop-walks)))
          )


        ;;; have to sub in the output value late to make sure all the eval takes place first.
        output-type (if is-runner?
                      @(ut/tracked-sub ::repl-output-type {:panel-key panel-key :view-name selected-view})
                      :value)

        body (if (and is-runner? (or (= output-type :output-live) (= output-type :output)))
               (let [solver-clover-kw (get @db/solver-fn-lookup [:panels panel-key selected-view])
                     running-clover-kw (keyword (str (cstr/replace (str solver-clover-kw) ":solver/" (str "solver-status/" (cstr/replace (str client-name) ":" "") ">")) (str ">running?")))
                     console-clover-kw (keyword (str (cstr/replace (str solver-clover-kw) ":solver/" "solver-meta/")
                                                     (if (= output-type :output-live)
                                                       ">incremental"
                                                       ">output>evald-result>out")))
                     running? (when (= output-type :output-live)
                                @(ut/tracked-sub ::conn/clicked-parameter-key-alpha {:keypath [running-clover-kw]}))
                     console-body @(ut/tracked-sub ::conn/clicked-parameter-key-alpha {:keypath [console-clover-kw]})]
                 (if console-clover-kw
                   {selected-view
                    (if (and (= output-type :output-live) running?)
                      [re-com/v-box
                       :children [[re-com/box
                                   :child  ""
                                   :height (px hh)  :width (px ww)
                                   :style  {:background-image "url(images/running.gif)"
                                            :position "absolute" :left 0 :top 0
                                            :background-repeat "no-repeat"
                                            :background-position "center"
                                            :opacity 0.2}]
                                  (ut/postwalk-replacer {:*data console-body} clover-fn)]]
                      (ut/postwalk-replacer {:*data console-body} clover-fn))}
                   body)) body)

        ;; body (if data-viewer? ;; else they will try to render shit inside the vectors... 
        ;;        (ut/postwalk-replacer 
        ;;         (->  walk-map (dissoc :box :icon :h-box :v-box)) 
        ;;                              body)
        ;;        (ut/postwalk-replacer walk-map body))
        body (ut/postwalk-replacer walk-map body)

        ;;_ (when (= panel-key :block-1800) (tapp>> [:data-viewer-only? panel-key data-viewer-only? selected-view override-view (str body)]))

        body (if (not data-viewer-only?)
               (ut/postwalk-replacer {:box re-com/box
                                      :layout (fn [x] [layout panel-key (or override-view selected-view) x w h])
                                      :px re-com.util/px
                                      :icon re-com/md-icon-button
                                      :md-icon re-com/md-icon-button
                                      :vv-box #(vbunny/vv-box  (+ px-width-int 70) (+ px-height-int 55) %)
                                      :vh-box #(vbunny/vh-box  (+ px-width-int 70) (+ px-height-int 55) %)
                                      :h-box re-com/h-box
                                      :hbox re-com/h-box
                                      :vbox re-com/v-box} body)
               body)
        dbody-type (get-in @dragging-body [:drag-meta :type])
        relevant-for-dyn-drop? (or (and is-layout? (or (= :query dbody-type) (= :view dbody-type)))
                                   (and (not (contains? @dragging-body :cloned-from))
                                        (not is-layout?)
                                        (not (= :meta-screens dbody-type)) ;; temp
                                        (not (= :meta-theme dbody-type)) ;; temp
                                        (not (= :meta-board dbody-type)) ;; temp
                                        (not (= :meta-blocks dbody-type)))) ;; temp
        overlay? (and @dragging? relevant-for-dyn-drop?)
        base-tables (vec (filter #(and (not (ut/is-sql-sql-alias? %)) (keyword? %))
                                 (remove nil?
                                         (flatten (for [[_ v] sql-calls]
                                                    (when (not (cstr/includes? (str (get v :from)) ":query/")) (get-in v [:from 0])))))))
        base-table-sniffs
        (into {}
              (for [t base-tables]
                {t {:select        [:*]
                    :connection-id (if (some #(= t %) ["channel_history" "fn_history" "flows_history"]) "flows-db" "system-db")
                    :from          [t]
                    :limit         111}}))
        templated-strings-vals (ut/deep-template-find body)
        templates? (ut/ne? templated-strings-vals)
        templated-strings-walk (if templates?
                                 (ut/postwalk-replacer {nil ""}
                                                       (into {}
                                                             (for [k templated-strings-vals]
                                                               {k @(ut/tracked-sub ::conn/clicked-parameter-key-alpha {:keypath [k]})})))
                                 {})
        body (if templates? (ut/deep-template-replace templated-strings-walk body) body)

        sub-param (when (has-fn? :run-solver) (first (map last (filter (fn [[k _]] (cstr/includes? (str k) (str panel-key " " selected-view))) @db/solver-fn-lookup))))

        body (if (ut/ne? sub-param)
               (let [curr-val @(ut/tracked-sub ::conn/clicked-parameter-key-alpha {:keypath [sub-param]})
                     sub-param-root (try (last (cstr/split (str sub-param) #"/")) (catch :default _ ""))
                     placeholder-on-running? (get-in br [selected-view-type :placeholder-on-running?] false)
                     ;;;_ (tapp>> [:bodyend panel-key selected-view selected-view-type sub-param sub-param-root placeholder-on-running?])
                     running? (when placeholder-on-running?
                                @(ut/tracked-sub ::conn/clicked-parameter-key-alpha {:keypath [(str "solver-status/" client-name ">"
                                                                                                    (cstr/replace (str sub-param-root) ":"  "") ">running?")]}))]
                 (if (or (nil? curr-val) running?)
                   (assoc body selected-view
                          ;;[re-com/box :child "waiting..."]
                          (get body :waiter)) ;; use user-space clover placeholder?
                   body)) body) ;; we have to replace it down here so the solver gets started above even though we don't render it yet...
        ;;rowset? (http/is-rowset? (get-in body [selected-view 1]))
        ]

    (when value-spy?
      ;(tapp>> [:value-spy-hash (hash body)])
      (swap! db/value-spy-hashes assoc [panel-key selected-view] (hash body)))


    ;;(when (= panel-key :block-911) (tapp>> [:body orig-body (get orig-body selected-view) (str (get-in body [selected-view 1]))]))

    (when (and view? ;; insert clover rowset is exists 
               (or  (= curr-view-mode :edn)
                    (= curr-view-mode :rowset))
               (http/is-rowset? (get-in body [selected-view 1]))
               (not @(ut/tracked-sub ::http/clover-data-exists? {:data-key selected-view
                                                                 :data (get-in body [selected-view 1])})))
      (tapp>> [:clover-dataset-insert! panel-key selected-view (count (get-in body [selected-view 1]))])
      (ut/tracked-dispatch [::http/insert-implicit-rowset (get-in body [selected-view 1]) selected-view true]))


    (doseq [[k v] (merge sql-calls base-table-sniffs)] ;; base-table-sniffs allow us to get
      (let [query          (sql-alias-replace v)
            editor?        @(ut/tracked-sub ::editor? {})
            selected-block @(ut/tracked-sub ::selected-block {})
            being-edited?  (and @db/cm-focused? editor? (= selected-block panel-key)) ;; dont run
            ;data-exists?   @(ut/tracked-subscribe [::conn/sql-data-exists? [k]])
            ;unrun-sql?     @(ut/tracked-subscribe [::conn/sql-query-not-run? [k] query])
            data-exists?   @(ut/tracked-sub ::conn/sql-data-exists-alpha? {:keypath [k]})
            unrun-sql?     @(ut/tracked-sub ::conn/sql-query-not-run-alpha? {:keypath [k] :query query})
            connection-id  (get query :connection-id connection-id)] 
        ;; (tapp>> [:honeyquery  data-exists? unrun-sql? being-edited? 
        ;;          (and (or (not data-exists?) unrun-sql?) (not being-edited?))  
        ;;          (hash (ut/clean-sql-from-ui-keys query)) (str k) (str (ut/clean-sql-from-ui-keys query)) 
        ;;          ;(= query @(ut/tracked-sub ::conn/sql-source {:kkey k}))
        ;;          ])
        (when (and (or (not data-exists?) unrun-sql?) (not being-edited?))
          (let [src     @(ut/tracked-sub ::conn/sql-source {:kkey k})
                srcnew? (not= src query)]
            ;(tapp>> [:honeyquery-run! (str k) ])
            (if (or (nil? connection-id) ;; because sometimes connection-id is a keyword and (empty? :keyword) will throw
                    (and (string? connection-id) (empty? connection-id)))
              (conn/sql-data [k] query)
              (conn/sql-data [k] query connection-id))
            (when srcnew? ;;; no need to dispatch to update the same shit over and over...
              (ut/tracked-dispatch [::insert-sql-source k query]))))))

    (cond ;;replacement-all? [reecatch [re-com/box :child (get body selected-view)]]
      ;rowset? 
      ;[reecatch [re-com/box :child "data"]]

      runner-rowset?
      (if (and fw fh)
        [reecatch [magic-table panel-key [selected-view] fw (+ fh 2)]]
        [reecatch [magic-table panel-key [selected-view]]])

      (seq body)
      (cond
        (not (nil? override-view)) ;; for editor usage
        (let []
          (if (= override-view :base)
            (let [bv body
                  bv (cond (map? bv) [map-boxes2 bv nil "" [] nil nil]
                           (number? bv) (str bv)
                           (nil? bv) "nil!"
                           :else bv)]
              [reecatch
               [re-com/box ;:size "auto"
                :src  (at)
                :style {:filter (if overlay? "blur(3px) opacity(60%)" "none")}
                :child bv]])

            (if override-view-is-sql?
              [reecatch [magic-table panel-key [override-view]]]

              (let [bv (get body override-view)
                    bv (cond (map? bv) [map-boxes2 bv nil "" [] nil nil]
                             (number? bv) (str bv)
                             (nil? bv) (str "nil!")
                             (keyword? bv) (str bv)
                             :else bv)]
                    ;;(tapp>> [:editor (str bv)])
                [reecatch
                 [re-com/box
                  :src  (at)
                  :width (px ww)
                  :size "none"
                  :style {:filter (if overlay? "blur(3px) opacity(60%)" "none")}
                  :child bv]]))))

        selected-view-is-sql?      [reecatch [magic-table panel-key [selected-view]]] ;; magic-table
                                                                                        ;; [panel-key
        single-view?               [reecatch
                                    (let [bv body
                                          bv (cond (map? bv) [map-boxes2 bv nil "" [] nil nil]
                                                   (number? bv) (str bv)
                                                   (keyword? bv) (str bv)
                                                   (nil? bv) "nil!"
                                                   :else bv)]
                                      [re-com/v-box :children
                                       [(when overlay?
                                          ;;@(ut/tracked-subscribe [::dynamic-drop-targets nil @dragging-body panel-key nil ww hh])
                                          @(ut/tracked-sub ::dynamic-drop-targets-alpha {:table-source nil :dbody @dragging-body :panel-key panel-key :query-key nil :width-int ww :height-int hh}))
                                        [re-com/box
                                         :src  (at)
                                         :style {:filter (if overlay? "blur(3px) opacity(60%)" "none")}
                                         :child bv]]])]

        :else                      [reecatch
                                    (let [bv (get body selected-view)
                                          bv (cond (map? bv) [map-boxes2 bv nil "" [] nil nil]
                                                   (number? bv) (str bv)
                                                   (keyword? bv) (str bv)
                                                   (nil? bv) "nil!"
                                                   :else bv)]
                                      [re-com/v-box :children
                                       [(when overlay?
                                          ;;@(ut/tracked-subscribe [::dynamic-drop-targets nil @dragging-body panel-key nil ww hh])
                                          @(ut/tracked-sub ::dynamic-drop-targets-alpha {:table-source nil :dbody @dragging-body :panel-key panel-key :query-key nil :width-int ww :height-int hh}))
                                        [re-com/box
                                         :src  (at)
                                         :style {:filter      (if overlay? "blur(3px) opacity(60%)" "none")
                                                 :margin-top  (if (and overlay? is-layout?) "-25px" "inherit") ;; not sure why the
                                                                                                        ;; pos gets fucked up
                                                                                                        ;; on
                                                 :margin-left (if (and overlay? is-layout?) "-3px" "inherit")}
                                         :child bv]]])]) ;)

      (and no-view? selected-view-is-sql?) (if (and fw fh) ;(js/alert (str selected-view))
                                             [reecatch [magic-table panel-key [selected-view] fw fh]]
                                             [reecatch [magic-table panel-key [selected-view]]]) ;; block-sizes
                                                                                                 ;; block-width
      (and no-view? (nil? selected-view) (seq sql-calls))  [reecatch [magic-table panel-key [(first (keys sql-calls))]]]

      :else [re-com/box
             :child "nothing here to do."])))




(defn brick-key-lookup [brick-vec brick-map] (first (remove nil? (for [[k v] brick-map] (when (= brick-vec (:root v)) k)))))

(defonce edit-mode? (reagent/atom {}))

(re-frame/reg-sub
 ::lookup-root-key
 (fn [db [_ brick-vec]]
   (let [lookup-map (into {}
                          (for [[k v] (into {} (filter #(= (get db :selected-tab) (get (val %) :tab "")) (get db :panels)))]
                            {(get v :root) k}))]
     (get lookup-map brick-vec))))

(re-frame/reg-sub
 ::lookup-root-key-tab
 (fn [db [_ brick-vec tab]]
   (let [lookup-map (into {} (for [[k v] (into {} (filter #(= tab (get (val %) :tab "")) (get db :panels)))] {(get v :root) k}))]
     (get lookup-map brick-vec))))

(re-frame/reg-sub ::panel-minimized?
                  (fn [db {:keys [panel-key]}]
                    (get-in db [:panels panel-key :minimized?] false)))

(re-frame/reg-sub ::all-panels-minimized
                  (fn [db]
                    (vec (for [[k v] (into {}
                                           (filter #(or (get (val %) :pinned? false)
                                                        (= (get db :selected-tab) (get (val %) :tab "")))
                                                   (get db :panels)))
                               :when (get v :minimized? false)]
                           [k (get v :name)]))))

(re-frame/reg-sub ::all-roots
                  (fn [db]
                    (vec (for [[k v] ;; added tab filter - 9/25/23
                               (into {}
                                     (filter #(or (get (val %) :pinned? false) (= (get db :selected-tab) (get (val %) :tab "")))
                                             (get db :panels)))]
                           (vec (conj (get v :root) k))))))

(re-frame/reg-sub ::all-roots-tab
                  (fn [db {:keys [tab]}]
                    (vec (for [[k v] (into {} (filter #(= tab (get (val %) :tab "")) (get db :panels)))]
                           (vec (conj (get v :root) k))))))

(re-frame/reg-sub ::all-roots-tab-sizes
                  (fn [db {:keys [tab]}]
                    (vec (for [[_ v] (into {} (filter #(= tab (get (val %) :tab "")) (get db :panels)))]
                           (vec (into (get v :root) [(get v :h) (get v :w)]))))))

(re-frame/reg-sub ::all-roots-tab-sizes-current 
                  (fn [db _]
                    (vec (for [[_ v] (into {} (filter #(= (get db :selected-tab) (get (val %) :tab "")) (get db :panels)))]
                           (vec (into (get v :root) [(get v :h) (get v :w)]))))))

(re-frame/reg-sub ::sizes-current
                  (fn [db _]
                    (let [selected-block (get db :selected-block)
                          sel-block-map (get-in db [:panels selected-block])]
                      (into (get sel-block-map :root) [(get sel-block-map :h) (get sel-block-map :w)]))))

(re-frame/reg-sub ::all-roots2
                  (fn [db [_ start-y end-y start-x end-x]]
                    (vec (for [[k v] (get db :panels)]
                           (let [r (get v :root)
                                 x (first r)
                                 y (last r)]
                             (when (and (and (>= x start-x) (<= x end-x)) (and (>= y start-y) (<= y end-y))) [x y]))))))







(re-frame/reg-sub ::panel-px-width (fn [db [_ panel-key]] (* db/brick-size (get-in db [:panels panel-key :w]))))

(re-frame/reg-sub ::panel-px-height (fn [db [_ panel-key]] (* db/brick-size (get-in db [:panels panel-key :h]))))

(re-frame/reg-sub
 ::panel-px-root
 (fn [db [_ panel-key]]
   (let [r (get-in db [:panels panel-key :root]) x (* db/brick-size (first r)) y (* db/brick-size (last r))] [x y])))

(re-frame/reg-sub
 ::connection-id
 (fn [db [_ panel-key]]
   (get-in db [:panels panel-key :connection-id])))

(re-frame/reg-sub
 ::connection-id-alpha
 (fn [db {:keys [panel-key]}]
   (get-in db [:panels panel-key :connection-id])))

(re-frame/reg-sub
 ::connection-id-alpha2
 (fn [db {:keys [panel-key query-key]}]
   (get-in db [:panels panel-key :queries query-key :connection-id] (get-in db [:panels panel-key :connection-id]))))

(re-frame/reg-sub ::panel-height (fn [db [_ panel-key]] (get-in db [:panels panel-key :h])))
(re-frame/reg-sub ::panel-width (fn [db [_ panel-key]] (get-in db [:panels panel-key :w])))
(re-frame/reg-sub ::panel-depth (fn [db [_ panel-key]] (get-in db [:panels panel-key :z] 0)))

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
     (if (> old-z 0) (assoc-in db [:panels panel-key :z] (- old-z 1)) db))))

(re-frame/reg-sub 
 ::panel-name 
 (fn [db [_ panel-key]] 
   (get-in db [:panels panel-key :name])))

(re-frame/reg-sub 
 ::ghosted? 
 (fn [db [_ panel-key]] 
   (if (get db :peek?) false 
       (get-in db [:panels panel-key :ghosted?] false))))

(re-frame/reg-sub
 ::un-fired-cross-breed?
 (fn [db [_ panel-key]]
   (true? (and (ut/ne? (get-in db [:panels panel-key :cross-breed]))
               (empty? (get-in db [:panels panel-key :cross-breed :children]))))))

(re-frame/reg-sub
 ::alert-mute?
 (fn [db _]
   (get db :alert-mute? false)))

(re-frame/reg-sub
 ::no-ui?
 (fn [db [_ panel-key]]
   (if (get db :peek?) false (or (get db :no-ui? false) (get-in db [:panels panel-key :no-ui?] false)))))

(re-frame/reg-sub ::panel-style (fn [db [_ panel-key]] (if (get db :peek?) {} (get-in db [:panels panel-key :style] {}))))

(re-frame/reg-sub ::hidden? (fn [db [_ panel-key]] (if (get db :peek?) false (get-in db [:panels panel-key :hidden?] false))))

(re-frame/reg-sub ::minimized?
                  (fn [db [_ panel-key]] (if (get db :peek?) false (get-in db [:panels panel-key :minimized?] false))))

(defn downstream-map
  [panel-id qid subq-map idx final]
  (apply concat
         (for [u (get-in subq-map [panel-id :uses])]
           (apply concat
                  (remove empty?
                          (for [[k v] (dissoc subq-map panel-id)]
                            (apply concat
                                   (remove empty?
                                           (for [kk (get v :produces)]
                                             (if (= u kk)
                                               (downstream-map k kk subq-map (+ 1 idx) (conj final {:d idx :p k}))
                                               (conj final {:d idx :p k})))))))))))

(def warp-hole (reagent/atom false))

(defn squareql-logo
  [x y]
  (let [left (* x db/brick-size)
        top  (* y db/brick-size)]
    [re-com/h-box :children
     [[re-com/box :child [:img {:src "images/squareql-logo-100a.png"}] :height "102px" :width "102px" :size "none" :attr
       {;:on-mouse-enter #(reset! warp-hole true)
        :on-mouse-leave #(reset! warp-hole false)
        :on-click       #(reset! warp-hole true)} :style
       {;:-webkit-backface-visibility "hidden"
        :transition_NOT   "transform 0.8s"
        :transform-style  "preserve-3d"
        :perspective      "1000px"
        :transform        (if @warp-hole "rotateY(360deg)" "")
        :background-color "#000000"
        :filter           "drop-shadow(0.35rem 0.35rem 0.4rem rgba(0, 0, 0, 0.5))"
        :border           (str "3px solid " (theme-pull :theme/editor-outer-rim-color "#FF06B5")) ;; "#FF06B5"
        :position         "fixed"
        :left             left
        :top              top
        :z-index          10}]
      [re-com/box :child " " ;[:img {:src "images/squareql-logo-100a.png"}]
       :height "102px" :width "102px" :size "none" :style
       {:background-color "#000000"
        :box-shadow       "inset 0px 0px 20px #FF06B566"
        :position         "fixed"
        :left             left
        :top              top
        :z-index          9}]]]))

(defn param-usage
  [panel-key]
  (let [all-sql-call-keys      @(ut/tracked-subscribe [::all-sql-call-keys]) ;(into {} (for [[k
        sql-aliases-used       @(ut/tracked-subscribe [::panel-sql-aliases-in-views panel-key])
        sql-calls              @(ut/tracked-sub ::panel-sql-calls {:panel-key panel-key})
        valid-body-params      @(ut/tracked-sub ::valid-body-params {:panel-key panel-key})
        possible-datasets-used (set (for [e (merge sql-calls valid-body-params)]
                                      (keyword (nth (ut/splitter (ut/safe-name e) #"/") 0))))
        used-datasets          (cset/union (set sql-aliases-used)
                                           (cset/intersection possible-datasets-used (set all-sql-call-keys)))]
    (ut/tapp>> [:pos panel-key possible-datasets-used])
    (vec (for [d used-datasets] @(ut/tracked-subscribe [::lookup-panel-key-by-query-key d])))))

(defn draw-lines
  [coords]
  ;; (tapp>> [:coords coords]) 
  (doall (for [[x1 y1 x2 y2 involved? color z1 z2 same-tab?] coords]
           ^{:key (hash (str x1 y1 x2 y2 "lines"))}
           (let [] ;selected-dash "" ;@(ut/tracked-subscribe [::selected-dash])
             [:path  
              {:stroke-width (if involved? 16 13)
               :stroke       (if involved? color 
                                 "#ffffff22"
                                 ;(str color 45)
                                 ;color
                                 ) ;(if (or involved? nothing-selected?)
               :fill         "none"
               :filter       "drop-shadow(0.25rem 0.35rem 0.4rem rgba(0, 0, 0, 0.44))"
               :d            (ut/curved-path-h x1 y1 x2 y2)}]))))


(re-frame/reg-event-db
 ::refresh-history-log
 (fn [db _]
  ;;  (if (not (nil? @db/active-tmp-history)) ;(get db :buffy?)
  ;;    (ut/dissoc-in db [:data @db/active-tmp-history])
  ;;    db)
   (tapp>> [:db/active-tmp-history @db/active-tmp-history])
   (ut/tracked-dispatch [::conn/clear-query-history @db/active-tmp-history])
   db))


(defn materialize-values [block-map]
  (let [valid-body-params (vec (ut/get-compound-keys block-map))
        workspace-params (into {}
                               (for [k valid-body-params] ;; deref here?
                                 {k @(ut/tracked-sub ::conn/clicked-parameter-key-alpha {:keypath [k]})}))
        value-walks-targets (filter #(and (cstr/includes? (str %) ".")
                                          (not (cstr/includes? (str %) ".*"))) valid-body-params)
        value-walks (into {}
                          (for [k value-walks-targets] ;all-sql-call-keys]
                            (let [fs    (ut/splitter (ut/safe-name k) "/")
                                  gs    (ut/splitter (last fs) ".")
                                  ds    (keyword (first fs))
                                  row   (try (int (last gs)) (catch :default _ "label"))
                                  field (keyword (first gs))]
                              {k (if (not (integer? row))
                                   (str field)
                                   (get-in @(ut/tracked-sub ::conn/sql-data-alpha {:keypath [ds]}) [row field]))})))
        new-vw (cond->> block-map
                 (ut/ne? value-walks)      (ut/postwalk-replacer value-walks)
                 (ut/ne? workspace-params) (ut/postwalk-replacer workspace-params))]
    new-vw))

(re-frame/reg-event-db
 ::update-single-panels-hash
 (fn [db _]
   (let [panels (get db :panels)
         panel-map (into {}
                         (for [[k v] panels]
                           {k (hash (ut/remove-underscored-plus-dims v))}))]
     (assoc db :panel-hashes panel-map))))

(re-frame/reg-sub
 ::single-panels-hash-now
 (fn [db _]
   (let [panels (get db :panels)
         panel-map (into {}
                         (for [[k v] panels]
                           {k (hash (ut/remove-underscored-plus-dims v))}))]
     panel-map)))

;; (re-frame/reg-event-db
;;  ::send-panel-updates
;;  (fn [db [_ block-keys]]
;;    (if
;;     (and (not @dragging?)
;;          (not @mouse-dragging-panel?)
;;          (not @on-scrubber?))
;;      (let [pp (get db :panels)
;;            panels-map (select-keys pp block-keys)
;;           ;;  resolved-panels-map  (into {} (for [[k v] panels-map] ;; super slow and lags out clients when panels edited
;;           ;;                                  {k (assoc v :queries (into {} (for [[kk vv] (get v :queries)]
;;           ;;                                                                  {kk (sql-alias-replace vv)})))}))
;;           ;;  materialized-panels-map (into {}  (for [[k v] panels-map] ;; super slow and lags out clients when panels edited
;;           ;;                                      {k (materialize-values v)}))
;;            ]
;;        (tapp>> [:sending-updated-panels  block-keys
;;               ;panels-map 
;;               ;resolved-panels-map 
;;               ;materialized-panels-map
;;                 ])
;;        (ut/tracked-dispatch
;;         [::wfx/push :default
;;          {:kind :updated-panels
;;           :materialized-panels {} ;;materialized-panels-map
;;           :panels panels-map
;;           :resolved-panels {} ;;resolved-panels-map
;;           :client-name (get db :client-name)}]) db)
;;      db)))

;; (ut/tracked-dispatch [::update-panels-hash])

(re-frame/reg-event-db
 ::update-panels-hash
 (fn [db _]
   (if (not @on-scrubber?) ;; dont want to push updates during scrubbing
     (let [pp          (get db :panels)
           ;;ppr         {} ;;; TEMP!
          ;;  ppr         (into {}
          ;;                    (for [[k v] pp] ;; super slow and lags out clients when panels edited
          ;;                      {k (assoc v :queries (into {}
          ;;                                                 (for [[kk vv] (get v :queries)]
          ;;                                                   {kk (sql-alias-replace vv)})))}))
          ;;  ppm         (into {}  (for [[k v] pp] ;; super slow and lags out clients when panels edited
          ;;                          {k (materialize-values v)}))
           new-h       (hash (ut/remove-underscored pp))
           client-name (get db :client-name)]
      ; (tapp>> [:running :update-panels-hash :event :expensive! "full send of all panels to server"])
       (ut/dispatch-delay 800 [::http/insert-alert [:box :child "ATTN: ::update-panels-hash running"] 12 1 5])
       ;;(conn/push-panels-to-server pp ppr client-name)
       (ut/tracked-dispatch
        [::wfx/push :default
         {:kind :current-panels
          :panels pp
          :materialized-panels {} ;; ppm
          :resolved-panels {} ;; ppr
          :client-name client-name}])
       (when (get db :buffy?) (ut/dispatch-delay 2000 [::refresh-history-log]))
       ;(assoc db :panels-hash new-h)
       db)
     db)))

;;[:data-viewer [:app-db [:server :settings :runners]]]

(re-frame/reg-sub
 ::hop-bar-runners
 (fn [db _]
   (let [runners (get-in db [:server :settings :runners])]
     (into {}
           (for [[k v] runners
                 :when (not= (get v :hop-bar?) false)]
             {k (select-keys v [:icon :description :type :syntax])})))))

(re-frame/reg-sub
 ::fabric-settings
 (fn [db _]
   (get-in db [:server :settings :runners :fabric])))


(re-frame/reg-sub
 ::panels-hash
 (fn [db _]
   (get db :panels-hash "not-yet!")))

(re-frame/reg-sub
 ::panels-hash-singles
 (fn [db _]
   (get db :panel-hashes)))

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
   (let [pid     (ut/safe-key (keyword (str (ut/safe-name panel-id) "-window")))
         [nx ny] (vec (map #(js/Math.floor (/ % db/brick-size)) @db/context-modal-pos))
         orig    (get-in db [:panels panel-id])
         new     (-> orig
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



;; (defn canvas-size [rects] (reduce (fn [[max-x max-y] [x y h w]] [(max max-x (+ x w)) (max max-y (+ y h))]) [0 0] rects))

;; (defn min-at-least-pos
;;   [coords]
;;   (reduce (fn [acc coord]
;;             (if (or (< (first coord) (first acc)) (and (= (first coord) (first acc)) (< (second coord) (second acc)))) coord acc))
;;           coords))

;; (re-frame/reg-sub ::tab-recenter-alpha
;;                   (fn [db {:keys [tab]}]
;;                     (let [coords  (vec (for [[x y _] 
;;                                              @(ut/tracked-sub ::all-roots-tab {:tab tab})] [x y]))
;;                           coords+ @(ut/tracked-sub ::all-roots-tab-sizes {:tab tab})

;;                           icons?  (ut/not-empty?
;;                                     (for [[k v] (get db :panels) 
;;                                           :when (and (= (get v :tab "") tab) 
;;                                                      (get v :iconized? false))] k)) 
;;                           corner  (if icons? [] (try (min-at-least-pos coords) (catch :default _ [0 0])))
;;                           corner  (if (empty? corner) [0 0] corner)
;;                           [hh ww] (canvas-size coords+)
;;                           corner  [(* -1 (first corner)) (* -1 (last corner))]
;;                           [hh ww] [(+ hh (first corner))
;;                                    (+ ww (last corner))]
;;                           _ (tapp>> (str [tab :coords coords :coords+ coords+ :corner corner :early-corner (ut/min-at-least-pos coords) (ut/canvas-size coords+) hh ww]))
;;                           ]
;;                       (vec (into corner [hh ww])))))



(defn canvas-size [rects]
  (reduce (fn [[min-x min-y max-x max-y] [x y h w]]
            [(min min-x x)
             (min min-y y)
             (max max-x (+ x w))
             (max max-y (+ y h))])
          [##Inf ##Inf ##-Inf ##-Inf]
          rects))

(re-frame/reg-sub ::tab-recenter-alpha
                  (fn [db {:keys [tab]}]
                    (let [coords+ @(ut/tracked-sub ::all-roots-tab-sizes {:tab tab})
          ; Filter out any coordinates with negative x or y
                          filtered-coords+ (filter (fn [[x y _ _]] (and (>= x 0) (>= y 0))) coords+)
                          [min-x min-y max-x max-y] (canvas-size filtered-coords+)
                          offset-x (- min-x)
                          offset-y (- min-y)
                          width (- max-x min-x)
                          height (- max-y min-y)
                          ;; _ (tapp>> (str [tab :coords+ coords+
                          ;;                 :filtered-coords+ filtered-coords+
                          ;;                 :calculated [offset-x offset-y width height]
                          ;;                 :bounds [min-x min-y max-x max-y]]))
                          ]
                      [offset-x offset-y width height])))





(re-frame/reg-sub ::tab-recenter
                  (fn [db [_ tab]]
                    (let [coords  (vec (for [[x y _]
                                             @(ut/tracked-sub ::all-roots-tab {:tab tab})] [x y]))
                          coords+ @(ut/tracked-sub ::all-roots-tab-sizes {:tab tab})
                          icons?  (ut/not-empty?
                                   (for [[k v] (get db :panels)
                                         :when (and (= (get v :tab "") tab)
                                                    (get v :iconized? false))] k))
                          corner  (if icons? [] (try (ut/min-at-least-pos coords) (catch :default _ [0 0])))
                          corner  (if (empty? corner) [0 0] corner)
                          [hh ww] (ut/canvas-size coords+)
                          corner  [(* -1 (first corner)) (* -1 (last corner))]
                          [hh ww] [(+ hh (first corner)) (+ ww (last corner))]]
                      (vec (into corner [hh ww])))))

(re-frame/reg-sub ::tab-offset
                  (fn [_ [_ tab]]
                    (let [coords (vec (for [[x y _]
                                            @(ut/tracked-sub ::all-roots-tab {:tab tab})] [x y]))
                          corner (try (ut/min-at-least-pos coords) (catch :default _ [0 0]))
                          ;_ (tapp>> (str [:offset tab :coords coords :corner corner]))
                          ]
                      corner)))

(re-frame/reg-sub ::tab-offset2 ;; parent offset
                  (fn [db [_ tab]]
                    (let [stab (get db :selected-tab)]
                      (first (for [[_ v] (get db :panels)
                                   :when (and (= (get v :tab) stab)
                                              (some #(= % tab) (ut/deep-flatten (get v :views))))]
                               (get v :root))))))

;; (re-frame/reg-sub ::is-grid?
;;                   (fn [db [_ panel-key view]]
;;                     (true? (some #(= % :grid) (ut/deep-flatten (get-in db [:panels panel-key :views view]))))))

(re-frame/reg-sub
 ::is-grid?
 (fn [db [_ panel-key view]]
   (contains? (ut/deep-flatten (get-in db [:panels panel-key :views view])) :grid)))

(re-frame/reg-sub
 ::is-grid-alpha?
 (fn [db {:keys [panel-key view]}]
   (contains? (ut/deep-flatten (get-in db [:panels panel-key :views view])) :grid)))


(re-frame/reg-sub ::is-pinned? (fn [db [_ panel-key]] (get-in db [:panels panel-key :pinned?] false)))

(defn render-icon
  [icon num]
  (if (and (ut/ne? icon) (not (nil? icon)))
    (if (or (cstr/starts-with? icon "zmdi-") (cstr/starts-with? icon "fa-"))
      [re-com/md-icon-button
       :src (at)
       :md-icon-name icon
       :style {;:color bcolor :cursor "grab"
               :font-size "15px"} :attr {}]
      [re-com/box
       :size "none"
       :height (px (* num db/brick-size))
       :child [:img {:src icon :width "100%"}]])
    " "))

(defn render-kit-icon [icon & [class]]
  (if (and (ut/ne? icon) (not (nil? icon)))
    (if (or (cstr/starts-with? icon "zmdi-")
            (cstr/starts-with? icon "ri-")
            (cstr/starts-with? icon "fa-"))
      
      [re-com/md-icon-button
       :src (at)
       :class class
       :md-icon-name icon
       :style {;:color (theme-pull :theme/editor-outer-rim-color nil)
               :transform-origin "7.5px 10px"
               :font-size "13px"}]
      [re-com/box
       :size "none"
       :class class
       :child [:img {:src icon
                     :style {:width "100%"
                             :height "100%"
                             :object-fit "cover"}
                     :transform-origin "7.5px 10px"
                     :height "16px"}]])
    (render-kit-icon "zmdi-pizza")))



(re-frame/reg-sub
 ::iconized
 (fn [db {:keys [panel-key]}]
   (when (get-in db [:panels panel-key :iconized?] false)
     (let [;iconized? (get-in db [:panels panel-key :iconized?] false)
           name             (get-in db [:panels panel-key :name] "")
           icon             (get-in db [:panels panel-key :icon])
           ;icon-view?       (get-in db [:panels panel-key :icon-view] false)
           default-icon-map {:h    1
                             :w    1
                             :view [:box :align :center :justify :center :width (px (* db/brick-size 1)) :height
                                    (px (* db/brick-size 1)) :padding "5px" :size "auto" :style
                                    {:font-size "10px"
                                     ;:border    (str "1px solid " (theme-pull :theme/editor-outer-rim-color nil) 33)
                                     :filter    (str "drop-shadow(0 0 0.75rem "
                                                     (theme-pull :theme/editor-outer-rim-color nil)
                                                     ")")} 
                                    :child (if icon [render-icon icon 1] 
                                               (str name))]}
           icon-view        (get-in db [:panels panel-key :icon-view] default-icon-map)]
       icon-view))))

(re-frame/reg-event-db
 ::write-iconized-view
 (fn [db [_ panel-id]]
   (let [icon-view @(ut/tracked-sub ::iconized {:panel-key panel-id}) ;; materialize defaults to breadcrumb users
         curr (get-in db [:panels panel-id :icon-view])]
     (if curr
       db
       (assoc-in db [:panels panel-id :icon-view] icon-view)))))

(re-frame/reg-sub ::panel-icon
                  (fn [db [_ panel-key]]
                    (let [selected-view   (get-in db [:panels panel-key :selected-view])
                          ;is-grid?        @(ut/tracked-subscribe [::is-grid? panel-key selected-view])
                          is-grid?        @(ut/tracked-sub ::is-grid-alpha? {:panel-key panel-key :view selected-view})
                          selected-query? (some #(= % selected-view) (keys (get-in db [:panels panel-key :queries])))
                          default         (cond selected-query? "zmdi-grid"
                                                is-grid?        "zmdi-view-dashboard"
                                                :else           "zmdi-widgets") ;; depends on what view
                          i               (get-in db [:panels panel-key :icon] default)]
                      i)))

(re-frame/reg-sub ::has-a-running-flow?
                  (fn [db [_ panel-key]]
                    (let [panel-map (get-in db [:panels panel-key])
                          pile      (filter #(cstr/starts-with? (str %) ":flow/") (ut/deep-flatten panel-map))]
                      (some true?
                            (for [e pile]
                              (let [aa     (ut/replacer e ":flow/" "")
                                    spl    (ut/splitter aa ">")
                                    runkey (keyword (str (first spl) ">*running?"))]
                                (get-in db [:click-param :flow-status runkey])))))))

(re-frame/reg-sub ::has-a-flow-view?
                  (fn [db [_ panel-key view]]
                    (let [panel-map (get-in db [:panels panel-key :views view])
                          ppile     (ut/deep-flatten panel-map)
                          finds     (get (group-by #(first (first %)) @drop-last-tracker-refs) panel-key)
                          finds     (for [[d p] finds :when (some #(= % (last d)) ppile)] p)
                          any?      (ut/ne? finds)]
                      any?)))

(re-frame/reg-sub ::has-a-running-flow-view?
                  (fn [db [_ panel-key view]]
                    (let [panel-map (get-in db [:panels panel-key :views view])
                          ppile     (ut/deep-flatten panel-map)
                          finds     (get (group-by #(first (first %)) @drop-last-tracker-refs) panel-key)
                          finds     (for [[d p] finds :when (some #(= % (last d)) ppile)] p)
                          running?  (some true?
                                          (for [e finds]
                                            (let [aa     (ut/replacer e ":flow/" "")
                                                  spl    (ut/splitter aa ">")
                                                  runkey (keyword (str (first spl) ">*running?"))]
                                              (get-in db [:click-param :flow-status runkey]))))]
                      running?)))


(re-frame/reg-event-db ::cross-breed
                       (undoable)
                       (fn [db [_ panel-id]]
                         (let [panel-body    (get-in db [:panels panel-id])
                               cross-data    (get panel-body :cross-breed)
                               times         (get cross-data :num 10)
                               new-tab-name  (ut/gen-tab-name)
                               skeleton      (fn [bid new-tab-name idx]
                                               (let [pidx (+ 2 idx)
                                                     h    2]
                                                 {:h             h
                                                  :w             7
                                                  :root          [1 (* pidx h)]
                                                  :tab           new-tab-name
                                                  :selected-view :hii
                                                  :name          (str bid)
                                                  :views         {:hii [:box :align :center :justify :center :attr
                                                                        {:id (str bid ":crossed")} :style
                                                                        {:font-size    "22px"
                                                                         :font-weight  700
                                                                         :padding-top  "6px"
                                                                         :padding-left "14px"
                                                                         :margin-top   "-8px"
                                                                         :color        :theme/editor-outer-rim-color
                                                                         :font-family  :theme/base-font} :child (str bid "!")]}
                                                  :queries       {}}))
                               panel-map     (into {}
                                                   (for [idx (range times)]
                                                     (let [bid (keyword (str "block-cross-" idx))
                                                           bid (ut/safe-key bid)]
                                                       {bid (skeleton bid new-tab-name idx)})))
                               new-panel-map (assoc-in panel-body [:cross-breed :children] (vec (keys panel-map)))
                               panel-map     (assoc panel-map panel-id new-panel-map)
                               panels        (merge (get db :panels) panel-map)]
                           (assoc db :panels panels))))

(def rs-debug (atom []))


(defn rs
  [edn brick-vec-key]
  (let [;;rr (resolver/logic-and-params edn brick-vec-key)
        ]
    (swap! rs-debug conj [edn brick-vec-key])
    edn)) ;; something very fucked up here, need to revise this whole appraoch. slow and weird.

(re-frame/reg-sub
 ::slice-set
 (fn [_ [_ panel-key]]
   (let [views        @(ut/tracked-sub ::views {:panel-key panel-key})
         runners      @(ut/tracked-sub ::panel-runners-only {:panel-key panel-key})
         sql-keys     @(ut/tracked-sub ::panel-sql-call-keys {:panel-key panel-key})]
     (set (into (into (keys runners) (keys views)) sql-keys)))))


(re-frame/reg-sub
 ::resolved-block-body-shell
 (fn [db [_ {:keys [panel-key]}]]
   (let [body (get-in db [:panels panel-key])
         ;body (select-keys body [:h :w :z :style :ghosted? :no-ui? :root :tab :selected-view :name :icon :icon-view :minimized? :pinned? :hidden?])
         runners-map (into (vec (keys (get-in db [:server :settings :runners] {}))) [:queries :views])
         body (apply dissoc body runners-map)
         base-theme-style (get-in db [:click-param :theme :base-block-style] {})
         base-theme-a-param? (and (keyword? base-theme-style) (cstr/includes? (str base-theme-style) "/"))
         base-theme-style (if base-theme-a-param?
                            ;(get-in db [:click-param :theme :base-block-style])
                            (let [spt (cstr/split (cstr/replace (str base-theme-style) ":" "") "/")]
                              ;(tapp>> [:pp panel-key spt base-theme-style])
                              (get-in db [:click-param (keyword (first spt)) (keyword (last spt))]))
                            base-theme-style)
         body-style (get body :style {})
         style-a-param? (and (keyword? body-style) (cstr/includes? (str body-style) "/"))
         block-style (if style-a-param?
                       (let [spt (cstr/split (cstr/replace (str body-style) ":" "") "/")]
                         ;(tapp>> [:pp panel-key spt body-style])
                         (get-in db [:click-param (keyword (first spt)) (keyword (last spt))]))
                       body-style)
         merged-body-style (merge base-theme-style block-style)
         
         body (assoc body :style merged-body-style)
        ;; _ (tapp>> [:ppp panel-key (str base-theme-style) (str block-style) (str merged-body-style) (str body)])
         ]
     (resolver/logic-and-params body panel-key))))

(defn compare-maps [map1 map2] ;; basic, single layer only 
  (let [keys1 (set (keys map1))
        keys2 (set (keys map2))
        added (cset/difference keys2 keys1)
        removed (cset/difference keys1 keys2)
        changed (filter #(not= (map1 %) (map2 %)) (cset/intersection keys1 keys2))]
    {:added (select-keys map2 added)
     :removed (select-keys map1 removed)
     :changed (into {} (map (fn [k] [k {:from (map1 k) :to (map2 k)}]) changed))}))

(defn maybedoall [] (let [hover-highlight? (or @param-hover @query-hover)] (if hover-highlight? doall seq)))

(re-frame/reg-sub
 ::block-in-alert?
 (fn [db [_ panel-key]]
   (cstr/includes? (str (get db :alerts)) (str panel-key))))

(re-frame/reg-sub
 ::query-mat-pct
 (fn [db [_ query-key]]
   (get-in db [:mat-pct query-key])))

(defn grid
  [& [tab]]
  (let [;reaction-hack! @hover-square ;; seems less expensive than doall-for ? Reaction-hack2!
        ;panels-hash1   @(ut/tracked-sub ::panels-hash {})
        ;panels-hash2   (hash (ut/remove-underscored @(ut/tracked-sub ::panels {})))
        ;panel-hash-singles @(ut/tracked-sub ::panels-hash-singles {})
        client-name    @(ut/tracked-sub ::client-name {})
        [tab-x tab-y]  (if tab
                         (let [tt @(ut/tracked-sub ::tab-recenter-alpha {:tab tab})]
                              ;[tt @(ut/tracked-subscribe [::tab-recenter tab])]
                           ;(tapp>> [:tabs-tt tab (str tt)])
                           [(get tt 0 0)
                            (get tt 1 0)])
                         [0 0])
        start-y        (if tab tab-y 0)
        start-x        (if tab tab-x 0)
        bricks-high    (+ (js/Math.floor (/ @(ut/tracked-sub ::subs/h {}) db/brick-size)) 1)
        bricks-wide    (+ (js/Math.floor (/ @(ut/tracked-sub ::subs/w {}) db/brick-size)) 1)
        ;bricks-high    (+ (js/Math.floor (/ @(ut/tracked-subscribe [::subs/h]) db/brick-size)) 1)
        ;bricks-wide    (+ (js/Math.floor (/ @(ut/tracked-subscribe [::subs/w]) db/brick-size)) 1)
        block-runners  @(ut/tracked-sub ::block-runners {})
        selected-block @(ut/tracked-sub ::selected-block {})
        lines?         @(ut/tracked-sub ::lines? {})
        peek?          @(ut/tracked-sub ::peek? {})
        full-no-ui?    @(ut/tracked-sub ::full-no-ui? {})
        brick-roots    (if tab
                         @(ut/tracked-sub ::all-roots-tab {:tab tab})
                         @(ut/tracked-sub ::all-roots {}))
        audio-playing? @(ut/tracked-sub ::audio/audio-playing? {})
        top-start      (* start-y db/brick-size) ;-100 ;; if shifted some bricks away...
        left-start     (* start-x db/brick-size)]

    ;; (when (and false ;;false ;; disable again
    ;;        (not @dragging?)
    ;;        (not @mouse-dragging-panel?)
    ;;        (not @on-scrubber?)) ;true ; external? UPDATE-PANELS-HASH DISABLED TMP!! WHEN
    ;;   (when (not (= panels-hash2 panels-hash1)) ;; core update for blind backend updating /
    ;;     ;;(tapp>> [:compare-hashes (compare-maps panel-hash-singles @(ut/tracked-sub ::single-panels-hash-now {}))])
    ;;     (let [comps  (compare-maps panel-hash-singles @(ut/tracked-sub ::single-panels-hash-now {}))
    ;;           added-or-changed (vec (into (keys (get comps :added)) (keys (get comps :changed))))]
    ;;       (when (ut/ne? added-or-changed)
    ;;         (ut/tracked-dispatch [::send-panel-updates added-or-changed]))
    ;;     (ut/tracked-dispatch [::update-single-panels-hash]) ;; do each panel individually for a-la-carte pushes later...
    ;;     (ut/tracked-dispatch [::update-panels-hash]))))

    ;; (when false ;; true ;; (cstr/includes? (str @(ut/tracked-sub ::client-name)) "emerald")
    ;;   ;; (ut/tapp>> [:dispatch-peek! @(ut/tracked-sub ::client-name)
    ;;   ;;             (vec (reverse (sort-by val (frequencies @ut/simple-dispatch-counts))))])  
    ;;   (ut/tapp>> [:sub-peek-alpha! @(ut/tracked-sub ::client-name {})
    ;;               (vec (take 20 (reverse (sort-by val @ut/subscription-counts-alpha))))])
    ;;   (ut/tapp>> [:sub-peek! @(ut/tracked-sub ::client-name {})
    ;;               (vec (take 20 (reverse (sort-by val @ut/subscription-counts))))]))

    ;; subscription-counts-alpha
    ;; subscription-counts


    ^{:key (str "base-brick-grid")}
    [re-com/h-box :children
     [(doall (for [[bw bh brick-vec-key] brick-roots] ;diff-grid1] ;(if @dragging? current-grid
        (let [bricksw                                      (* bw db/brick-size)
              bricksh                                      (* bh db/brick-size)
              top                                          (+ top-start bricksh)
              left                                         (+ left-start bricksw)
              brick-vec                                    [bw bh]
              root?                                        true ; (some #(= brick-vec %) brick-roots)
              body-shell                                   @(re-frame/subscribe [::resolved-block-body-shell
                                                                                 {:panel-key brick-vec-key}])
              {:keys [w h name z]}                         body-shell
              trunc-name                                   (when (not (nil? name))
                                                             (let [charpx      5.75
                                                                   pixel-width (* (count (str name)) charpx)
                                                                   panel-width (- (* w db/brick-size) 50)]
                                                               (if (> pixel-width panel-width)
                                                                 (str (subs name 0 (/ panel-width charpx)) "...")
                                                                 name)))
              block-width                                  (if root?
                                                             (+ 1 (* db/brick-size (if (= w 0) (- bricks-wide 1) w)))
                                                             db/brick-size)
              block-height                                 (if root?
                                                             (+ 1 (* db/brick-size (if (= h 0) (- bricks-high 1) h)))
                                                             db/brick-size)
              views                                        @(ut/tracked-sub ::views {:panel-key brick-vec-key})
              runners                                      @(ut/tracked-sub ::panel-runners-only {:panel-key brick-vec-key})
              sql-keys                                     @(ut/tracked-sub ::panel-sql-call-keys {:panel-key brick-vec-key})
              all-views                                    (vec (keys views)) ;; (if single-view? [] (vec (keys views)))
                     ;slice-set                                    (set (into (into (keys runners) (keys views)) sql-keys))
                     ;slice-set                                    (set (into (keys runners) (keys views)))
              slice-set                                    @(ut/tracked-subscribe [::slice-set brick-vec-key])
              selected?                                    (= brick-vec-key selected-block)
              block-selected?                              selected?
              subq-blocks                                  (if root?
                                                             @(ut/tracked-sub ::subq-panels-alpha {:panel-id selected-block})
                                                             [])
              parent-of-selected?                          (some #(= % brick-vec-key) subq-blocks)
              editor?                                      @(ut/tracked-sub ::editor? {})
              hover-q?                                     (if (and editor? root?) ;; conver this logic into a single sub for perf?
                                                             (or ;@(ut/tracked-subscribe [::has-query? brick-vec-key (get @param-hover 1)])
                                                                        ;@(ut/tracked-subscribe [::has-query? brick-vec-key @query-hover])
                                                              (slice-set (keyword (-> (get @param-hover 1) str (cstr/replace ":" "") (cstr/replace ".*" ""))))
                                                              (and (= @query-hover brick-vec-key) (= @db/item-browser-mode :blocks)))
                                                             false)
              subq-mapping                                 (if root? @(ut/tracked-sub ::subq-mapping-alpha {}) [])
              upstream?                                    (some #(= % brick-vec-key)
                                                                 (ut/cached-upstream-search subq-mapping selected-block))
              downstream?                                  (some #(= % brick-vec-key)
                                                                 (ut/cached-downstream-search subq-mapping selected-block))

              reco-selected                                (let [;;rr @(ut/tracked-subscribe [::conn/clicked-parameter-key
                                                                 rr @(ut/tracked-sub ::conn/clicked-parameter-key-alpha
                                                                                     {:keypath [:viz-tables-sys/table_name]})
                                                                 rr (if (not (nil? rr)) (keyword (ut/replacer rr "_" "-")) nil)]
                                                             rr)
              viz-reco?                                    (and (or (= selected-block "none!") (nil? selected-block))
                                                                (some #(= % reco-selected) sql-keys)
                                                                editor?
                                                                (= @db/editor-mode :vvv))
              {:keys [ghosted? no-ui?
                      hidden? minimized?]}                 body-shell
              no-ui?                                       (or no-ui? (not (nil? tab)) full-no-ui?)
              panel-style                                  (get body-shell :style)
                    ;;  panel-style                                  (assoc panel-style :border-radius
                    ;;                                                      (let [border-radius (get panel-style :border-radius)]
                    ;;                                                        (when (string? border-radius)
                    ;;                                                          (if (> (count (cstr/split (str border-radius) " ")) 1)
                    ;;                                                            (str border-radius " " border-radius " " border-radius " " border-radius)
                    ;;                                                            border-radius))))
              border-radius (get panel-style :border-radius)
              border-style (when (get panel-style :border)
                             (if (cstr/includes? (str (get panel-style :border)) ", ")
                               (let [bb (vec  (cstr/split (str (get panel-style :border)) ", "))]
                                 {:border-top (get bb 0)
                                  :border-bottom (get bb 1)
                                  :border-left (get bb 2)
                                  :border-right (get bb 3)})
                               (let [bbd (get panel-style :border)]
                                 {:border-top bbd
                                  :border-bottom bbd
                                  :border-left bbd
                                  :border-right bbd})))
                     ;;_ (tapp>> [:border-style (str  border-style)])
              tab-color                                    (cond selected?           (theme-pull :theme/universal-pop-color "#9973e0") ;;"#9973e0"
                                                                 viz-reco?           "#ffb400"
                                                                 hover-q?            "#c7005d"
                                                                 parent-of-selected? "#e6ed21"
                                                                 upstream?           "#7be073"
                                                                 downstream?         "#05dfff"
                                                                 :else               "#ffffff10")
              tab-text-color                               (cond selected?           "#000000"
                                                                 viz-reco?           "#000000"
                                                                 hover-q?            "#000000"
                                                                 parent-of-selected? "#000000"
                                                                 upstream?           "#000000"
                                                                 downstream?         "#000000"
                                                                 :else               (theme-pull
                                                                                      :theme/block-tab-selected-font-color
                                                                                      "#ffffff55"))
              iconization                                  @(ut/tracked-sub ::iconized {:panel-key brick-vec-key})
              being-dragged?                               (= brick-vec-key @dragging-block)
              drag-action?                                 (and @mouse-dragging-panel? being-dragged?)
                     ;single-view?                                 false ;;@(ut/tracked-subscribe [::is-single-view? brick-vec-key])
              no-view?                                     @(ut/tracked-subscribe [::has-no-view? brick-vec-key])
              selected-view                                @(ut/tracked-sub ::selected-view-alpha {:panel-key brick-vec-key})
                      ;;_ (tapp>> [:pre-selected-view selected-view])
              selected-view                                (cond viz-reco? reco-selected
                                                                 (and (nil? selected-view) no-view? (seq sql-keys)) (first sql-keys)
                                                                 :else selected-view)
              selected-view-type                           @(ut/tracked-sub ::view-type {:panel-key brick-vec-key :view selected-view})
                     ;is-grid?                                     @(ut/tracked-subscribe [::is-grid? brick-vec-key selected-view])
              is-grid?                                     @(ut/tracked-sub ::is-grid-alpha? {:panel-key brick-vec-key :view selected-view})
              is-pinned?                                   @(ut/tracked-subscribe [::is-pinned? brick-vec-key])
              col-selected?                                @(ut/tracked-subscribe [::column-selected-any-field? brick-vec-key
                                                                                   selected-view])

              base-view-name                               :view ;; basically a default for single views
              mouse-down?                                  (atom false)
              tab-offset                                   (if tab @(ut/tracked-subscribe [::tab-offset2 tab]) [0 0])
                      ;; click-delay                                  300
                      ;; mixed-keys ;;(try
                      ;;   (cond (and single-view? (seq sql-keys)) (conj sql-keys base-view-name)
                      ;;         (and no-view? (seq sql-keys))     sql-keys
                      ;;         :else                             (into (into all-views sql-keys) (keys runners)))
              mixed-keys                                   (vec (into (into all-views sql-keys) (keys runners)))
              alerted?                                     false ;@(ut/tracked-subscribe [::block-in-alert? brick-vec-key])
                      ;; _ (tapp>> [:prunners brick-vec-key mixed-keys (keys runners)])
              zz                                           (if (or selected? hover-q?) (+ z 50) (+ z 10))
                     ;theme-base-block-style                       (theme-pull :theme/base-block-style {})
                     ;theme-base-block-style-map                   {} ;(when (map? theme-base-block-style) theme-base-block-style)
              dyn-border  (cond
                            alerted?            "2px solid #ffb400"
                            selected?           (str "2px solid " (theme-pull :theme/universal-pop-color "#9973e0"))
                            viz-reco?           "2px dashed #ffb400"
                            hover-q?            "2px solid #c7005d"
                            parent-of-selected? "2px solid #e6ed21" ;"#9973e0" ;"#9973e0"
                            upstream?           "2px solid #7be073" ;"#09050d" ;"#7be073"
                            downstream?         "2px dashed #05dfff" ;"#09050d" ;"#7be073"
                            ghosted?            "2px solid #00000000"
                            :else               "2px solid #ffffff05")]
          (if (and (not (nil? iconization)) (ut/not-empty? iconization)) ;; remove all this
            (let [icon-h (get iconization :h 1)
                  icon-w (get iconization :w 1)
                  vv     (get iconization :view [:box :child "icon?"])]
              ^{:key (str "brick-" brick-vec-key "-top-" (hash [border-radius dyn-border]))}
              [re-com/box
               :size "auto"
               :width (px (* db/brick-size icon-w))
               :height (px (* db/brick-size icon-h))
               :align :center
               :justify :center
               :attr {;:on-click #(js/alert (if tab "in container" "not in container"))
                      :on-context-menu #(when (not tab) (ut/tracked-dispatch [::toggle-icon-block brick-vec-key]))
                      ;:on-mouse-down   #(mouse-down-handler % brick-vec-key tab-offset true)
                      :on-mouse-enter   #(do
                                           (reset! over-block brick-vec-key)
                                           (reset! over-block? true))
                      :on-mouse-over   #(when
                                         (and (not @over-block?) (not (= brick-vec-key @over-block)))
                                          (reset! over-block brick-vec-key)
                                          (reset! over-block? true)) ;; took out enter for watched
                      :on-mouse-leave  #(do (reset! over-block? false) (reset! over-block nil))
                      :on-double-click #(do (tag-screen-position %) (ut/tracked-dispatch [::launch-clone brick-vec-key]))}
               :style (merge {:position         "fixed"
                              :user-select      "none"
                              :border           (when selected? (str "2px solid " (theme-pull :theme/universal-pop-color "#9973e0"))
                                                      ;;:else     "2px solid #ffffff05"
                                                      )
                              :color            (theme-pull :theme/block-title-font-color nil)
                              :cursor           "pointer"
                              :z-index          zz
                              :background-color (theme-pull :theme/base-block-color nil)
                              :top              (px top)
                              :left             (px left)}
                                   ; theme-base-block-style-map ;; just in case
                             panel-style)
               :child [honeycomb-fragments vv icon-w icon-h]])
            (when (and (<= (/ left db/brick-size) bricks-wide) ;;; ??? problem wiht reacting with things offscreen?
                       (<= (/ top db/brick-size) bricks-high)
                       (not (cstr/starts-with? (str brick-vec-key) ":query-preview")))
              ^{:key (str "brick-" brick-vec-key "-inner-" (hash [border-radius dyn-border]))}
              [re-com/box
               :width (px block-width)
               :height (px block-height)
               :attr ;(merge
               {;:on-mouse-enter  #(do (reset! over-block? true)
                :on-mouse-over  #(when (not @over-block?) (reset! over-block brick-vec-key) (reset! over-block? true))
                :on-mouse-leave #(do (reset! over-block? false) (reset! over-block nil))}
               :style (let [block-style
                            (merge
                             border-style
                             {:position "fixed" ;"absolute" ;"fixed"
                              :font-size "13px"
                              :z-index zz
                          ;; :filter (when selected?
                          ;;           (theme-pull
                          ;;            :theme/base-block-filter-selected
                          ;;            (str "drop-shadow(0.35rem 0.35rem 0.4rem "
                          ;;                 (theme-pull :theme/universal-pop-color "#9973e0")
                          ;;                 ") drop-shadow(-0.35rem -0.35rem 0.4rem "
                          ;;                 (theme-pull :theme/universal-pop-color "#9973e0") ")")))
                               ;; :filter (when selected? ;; no need for custom "selected block filter" eh?
                               ;;           (str "drop-shadow(0.35rem 0.35rem 0.4rem " (theme-pull :theme/universal-pop-color "#9973e0") ") drop-shadow(-0.35rem -0.35rem 0.4rem "(theme-pull :theme/universal-pop-color "#9973e0") ")"))
                              :user-select "none"
                              :outline "0px"
                              :overflow (if is-grid?
                                          "hidden" ;"auto"
                                          "visible")
                              :display
                              (if (or (and hidden? (not selected?)) minimized? (cstr/starts-with? (str brick-vec-key) ":query-preview"))
                                "none"
                                "inherit")
                              ;; :transform (if peek?
                              ;;              (if selected?
                              ;;                "scale(0.7)"
                              ;;                "scale(0.7)")
                              ;;              (when (and audio-playing? (= brick-vec-key @db/speaking))
                              ;;                (str "scale(" (max (min 0.95 (+ 0.0
                              ;;                                               (- (get @db/audio-data2 brick-vec-key)
                              ;;                                                  (get @db/audio-data brick-vec-key)))) 1.14) ")")))
                              :transform (if peek?
                                           (if selected?
                                             "scale(0.7)"
                                             "scale(0.7)")
                                          ;;  (when (and audio-playing? (= brick-vec-key @db/speaking))
                                          ;;    (let [audio-diff (- (get @db/audio-data2 brick-vec-key)
                                          ;;                        (get @db/audio-data brick-vec-key))
                                          ;;          jiggle-x (* audio-diff 1.6)  ; Adjust multiplier for more/less horizontal movement
                                          ;;          jiggle-y (* audio-diff 1.1)  ; Adjust multiplier for more/less vertical movement
                                          ;;          rotation (* audio-diff 5)]  ; Adjust multiplier for more/less rotation
                                          ;;      (str "translate(" jiggle-x "px, " jiggle-y "px) "
                                          ;;           "rotate(" rotation "deg)")))
                                           )
                              :transform-style "preserve-3d" ;; important for tab embedding!
                              :box-shadow (when (= brick-vec-key @db/speaking)
                                            (let [block-id       brick-vec-key ;:audio
                                                  talking-block? true]
                                              (cond (and audio-playing? talking-block?)
                                                    (str
                                                     "1px 1px " 
                                                     ;"1px " (px (* 10 (+ 0.1 (get @db/audio-data block-id)))) " " 
                                                     (px (* 90 (+ 0.1 (get @db/audio-data block-id))))
                                                     " "        (theme-pull :theme/editor-outer-rim-color nil))
                                                    ;; (str "1px 1px " (px (* 70
                                                    ;;                        (+ 0.1
                                                    ;;                           (- (get @db/audio-data2 block-id)
                                                    ;;                              (get @db/audio-data block-id)))))
                                                    ;;   " "  (theme-pull :theme/universal-pop-color nil))

                                                    :else                               "none")))
                              :transition "border 0.3s ease-in-out, background-color 0.3s ease-in-out, color 0.3s ease-in-out"
                              :border-top dyn-border
                              :border-bottom dyn-border
                              :border-left dyn-border
                              :border-right dyn-border
                              :opacity
                              (cond
                                (and
                                 lines?
                                 (or downstream? upstream? parent-of-selected? hover-q? viz-reco? selected? (= "none!" selected-block)))
                                1.0
                                lines? 0.5
                                :else 1.0)
                              :background-color (cond ;drag-action? "#00000000"
                                                  ghosted?      "#00000000"
                                                  col-selected? "#111214" ; (str (theme-pull
                                                  selected?     (theme-pull :theme/base-block-color-selected nil) ;"#0b031b"
                                                  root?         (theme-pull :theme/base-block-color nil) ;  "#0b1122"
                                                  :else         "#55afb344")
                              :top (px top)
                              :left (px left)}
                                    ;theme-base-block-style-map
                             (dissoc panel-style :border))]
                        (if selected?
                          (merge
                           block-style
                           {:filter (str (get block-style :filter) " "
                                         (str "drop-shadow(0.35rem 0.35rem 0.4rem " (theme-pull :theme/universal-pop-color "#9973e0") ") drop-shadow(-0.35rem -0.35rem 0.4rem "
                                              (theme-pull :theme/universal-pop-color "#9973e0") ")"))})
                          block-style))
               :child ;(if root?
               ^{:key (str "brick-" brick-vec "-root")}
               [re-com/v-box :gap "1px" :size "1" :justify :between :children
                [;(when (not editor-panel?)
                 (if ;(and (not ghosted?)
                  (or (and (not ghosted?) (not no-ui?)) selected?)
                   ^{:key (str "brick-" brick-vec "-header1")}
                   [re-com/h-box :height "20px" :padding "3px" :justify :between :align :center :style
                    {:background-color (cond ;drag-action? "#58A27977"
                                         selected?           (str (theme-pull :theme/universal-pop-color "#9973e0") 22) ;;"#9973e022" ; "#3b528b" ;
                                         parent-of-selected? (str (theme-pull :theme/universal-pop-color "#9973e0") 22) ;;"#9973e022"
                                         upstream?           (str (theme-pull :theme/universal-pop-color "#9973e0") 22) ;;"#9973e022"
                                         downstream?         "#05dfff22"
                                         :else               "inherit")
                     :z-index          (if (or is-grid? selected?) (+ zz 10) zz)
                     :color            (cond selected? (theme-pull :theme/block-title-selected-font-color nil) ;"#ffffff"
                                             :else     (theme-pull :theme/block-title-font-color nil)) ;"#ffffff"
                     :cursor           (if selected? "grab" "pointer")} :children
                    [(when (<= w 5)
                       ^{:key (str "brick-" brick-vec "-header2")}
                       [re-com/box :size "1" :height "18px" :style
                        {;:border  "1px solid white"
                         :margin-top  "-4px" ;; weird boxing so user doesnt click on the
                         :padding-top "4px" ;; weird boxing so user doesnt click on the
                         :margin-left "-4px"} ;; weird boxing so user doesnt click on the
                        :attr
                        (merge
                         {:on-click      #(ut/tracked-dispatch [::select-block brick-vec-key])
                                ;;  :on-mouse-down (fn [e]
                                ;;                   (reset! mouse-down? true)
                                ;;                   (js/setTimeout #(when @mouse-down? (mouse-down-handler e brick-vec-key tab-offset))
                                ;;                                  click-delay))
                          :on-mouse-up   (fn [_] (reset! mouse-down? false))}
                         (when selected? {:on-mouse-down #(mouse-down-handler % brick-vec-key tab-offset)})) :child " "])
                     (when (> w 5)
                       ^{:key (str "brick-" brick-vec "-header3")}
                       [re-com/box :size "1" :height "22px" :attr
                        (merge
                         {:on-click      #(ut/tracked-dispatch [::select-block brick-vec-key])
                                ;;  :on-mouse-down (fn [e]
                                ;;                   (reset! mouse-down? true)
                                ;;                   (js/setTimeout #(when @mouse-down? (mouse-down-handler e brick-vec-key tab-offset))
                                ;;                                  click-delay))
                          :on-mouse-up   (fn [_] (reset! mouse-down? false))}
                         (when selected? {:on-mouse-down #(mouse-down-handler % brick-vec-key tab-offset)}))
                        :style {:opacity      (if selected? 1.0 0.4)
                                       ;:margin-top   "-4px" ;; weird boxing so user doesnt click on the
                                :padding-top  "4px"  ;; weird boxing so user doesnt click on the
                                       ;:margin-left  "-4px" ;; weird boxing so user doesnt click on the
                                :font-size    "10px"
                                :font-weight  (if selected? 700 300)
                                       ;:margin-left (if (get panel-style :border-radius) "0px" "-4px")
                                :padding-left (if (let [pos (get (cstr/split (str (get panel-style :border-radius)) " ") 0)] (and (not= "0px" pos) (not (nil? pos))))
                                                (px (- (vbunny/px- (get panel-style :border-radius)) 5)) "6px")}
                        :child (str trunc-name)]) ;(str brick-vec-key " "
                                                                                ;trunc-name) ;(str
                     ^{:key (str "brick-" brick-vec "-header5")}
                     (let [hovered-on? (and (not @dragging-editor?)
                                            (or selected? (= brick-vec-key @over-block)))]
                       [re-com/h-box :gap "5px"
                        :style {:padding-right (if (let [pos (get (cstr/split (str (get panel-style :border-radius)) " ") 1)] (and (not= "0px" pos) (not (nil? pos))))
                                                 (px (+ (vbunny/px- (get panel-style :border-radius)) 12)) "6px")}
                        :children
                        [;;  (when (not selected?)
                         (when col-selected?
                           ^{:key (str "brick-" brick-vec "-header-pp")}
                           [re-com/md-icon-button :md-icon-name (if col-selected? "zmdi-pause" "zmdi-play") :style
                            {:font-size "15px" :opacity 0.25 :cursor "pointer"}])
                         (when hovered-on?
                           ^{:key (str "brick-" brick-vec "-header-pin")}
                           [re-com/md-icon-button :md-icon-name (if is-pinned? "zmdi-pin-off" "zmdi-pin") :on-click
                            #(do (ut/tracked-dispatch [::toggle-pin-block brick-vec-key])) ;(reset!
                                                                                                   ;over-block?
                                                                                                   ;false)
                            :style {:font-size "15px" :opacity (if is-pinned? 0.8 0.1) :cursor "pointer"}])
                         (when hovered-on?
                           ^{:key (str "brick-" brick-vec "-header-min")}
                           [re-com/md-icon-button :md-icon-name "zmdi-window-minimize" :on-click
                            #(do (ut/tracked-dispatch [::toggle-minimize-block brick-vec-key])
                                 (when selected? (ut/tracked-dispatch [::select-block "none!"]))
                                 (do (reset! over-block? false))) :style {:font-size "15px" :opacity 0.33 :cursor "pointer"}])
                         (when hovered-on?
                           ^{:key (str "brick-" brick-vec "-header-icon")}
                           [re-com/md-icon-button :md-icon-name "zmdi-photo-size-select-small" :on-click
                            #(do (ut/tracked-dispatch [::toggle-icon-block brick-vec-key])
                                 (ut/tracked-dispatch [::write-iconized-view brick-vec-key])
                                 (when selected? (ut/tracked-dispatch [::select-block "none!"]))
                                 (do (reset! over-block? false))) :style {:font-size "15px" :opacity 0.33 :cursor "pointer"}])
                         (when hovered-on?
                           ^{:key (str "brick-" brick-vec "-header-close")}
                           [re-com/md-icon-button :md-icon-name "zmdi-close" :on-click
                            #(do (ut/tracked-dispatch [::delete-panel brick-vec-key]) (do (reset! over-block? false))) :style
                            {:font-size "15px" :opacity 0.33 :cursor "pointer"}])]])]]
                   [re-com/gap :size "20px"]) ;)
                 ^{:key (str "brick-" brick-vec "-content-box")}
                 [re-com/box :padding "4px" :size "none" :height (px (- block-height 40)) :style
                  {;:border "1px solid #ffffff11" :border-right "1px solid #ffffff11"
                   :overflow "hidden" ;;; TODO YUUUUUP ^^^ ;; inherit and move
                   :color    (str (theme-pull :theme/grid-font-color nil) 88)} :child
                  (if drag-action?
                    ^{:key (str "brick-" brick-vec-key "-dragger")}
                    [re-com/box :child " " :style {:background-color "#58A27933"}]
                    ^{:key (str "brick-" brick-vec-key "-honeycomb-box")}
                    (if @dragging-block ;(or @dragging-editor? @dragging-block) ;; @dragging? ;; dragging-block
                      [re-com/box :child " "]
                      [reecatch (if viz-reco?
                                  [honeycomb brick-vec-key selected-view]
                                  [honeycomb brick-vec-key])]))]
                 (if (or (and (not ghosted?) (not no-ui?)) selected?)
                   (let [valid-kits  @(ut/tracked-subscribe [::valid-kits-for {:panel-key brick-vec-key :data-key selected-view}])
                                ;; _ (tapp>> [:valid-kits! valid-kits brick-vec-key selected-view @(ut/tracked-sub ::valid-kits {}) 
                                ;;            @(ut/tracked-subscribe [::valid-kits-for {:panel-key brick-vec-key :data-key selected-view}])
                                ;;            ])
                         ]
                     ^{:key (str "brick-" brick-vec "-footer")}
                     [re-com/box :size "none" :width (px (- block-width 4)) :height "15px" :style
                      {:overflow "hidden" :z-index (if selected? (+ zz 10) zz)} :child
                      [re-com/h-box :size "auto" :justify :between :children
                       [;[re-com/gap :size "12px"]
                        (doall
                         (if (empty? mixed-keys)
                           ^{:key (str "brick-" brick-vec "-footer-gap")}
                           [re-com/gap :size "12px"]
                           ^{:key (str "brick-" brick-vec "-footer-sql-keys-wrap")}
                           [re-com/h-box
                            :size "auto"
                                   ;:style {:border "1px solid pink"}
                            :style {:padding-left (if (let [pos (get (cstr/split (str (get panel-style :border-radius)) " ") 3)] (and (not= "0px" pos) (not (nil? pos))))
                                                    (px (- (vbunny/px- (get panel-style :border-radius)) 5)) "6px")}
                            :justify :between
                            :children [^{:key (str "brick-" brick-vec "-footer-sql-keys")}
                                       [re-com/h-box
                                        :gap "0px"
                                        :children
                                        (doall
                                         (for [s mixed-keys]
                                           (let [selected?        (= (if (= s base-view-name) nil s) selected-view)
                                             ;;;_ (tapp>> [:selected-view selected-view])
                                                 not-view?        (not (some #(= % s) (keys views)))
                                                 reco-count       (when not-view? @(ut/tracked-subscribe [::reco-count s :reco]))
                                                 [_ single-wait?] (if not-view?
                                                                  @(ut/tracked-subscribe [::query-waitings s]) [0 0])
                                                 mat-pct          (str (Math/ceil @(ut/tracked-subscribe [::query-mat-pct s])) "%")
                                                 reco-ready?      (and not-view? (> reco-count 0))
                                                 runner?          (true? (some #(= s %) (keys runners)))
      
      
                                                       ;;; _ (tapp>> [:valid-kits valid-kits ])
                                                 runner-running?   (when runner?
                                                                     (let [are-solver        (get @db/solver-fn-lookup [:panels brick-vec-key s])
                                                                           rr? @(ut/tracked-sub ::conn/clicked-parameter-key-alpha
                                                                                                {:keypath [(keyword
                                                                                                            (cstr/replace
                                                                                                             (str (ut/replacer are-solver
                                                                                                                               ":solver/"
                                                                                                                               (str "solver-status/" (cstr/replace (str client-name) ":" "") ">")) ">running?")
                                                                                                             ":" ""))]})] rr?))
                                            ;;  runner-running?  (when runner? (true?
                                            ;;                                  (let [;selected-view-type @(ut/tracked-sub ::view-type {:panel-key brick-vec-key :view s})
                                            ;;                                        are-solver        (get @db/solver-fn-lookup [:panels brick-vec-key s])
                                            ;;                                        running-status    (when are-solver
                                            ;;                                                            @(ut/tracked-sub ::conn/clicked-parameter-key-alpha
                                            ;;                                                                             {:keypath [(keyword (str (ut/replacer are-solver
                                            ;;                                                                                                                   ":solver/" "solver-status/*client-name*>")))]}))]
                                            ;;                                    running-status)))
                                            ;;  _ (when (= brick-vec-key :block-820)
                                            ;;      (tapp>> [:tabs are-solver (str (ut/replacer are-solver
                                            ;;                                                  ":solver/" "solver-status/*client-name*>")) brick-vec-key s runner?  runner-running?]))
                                                 query-running?   single-wait?
                                                 flow-running?    false ;;  @(ut/tracked-subscribe [::has-a-running-flow-view? brick-vec-key s])
                                                 param-keyword    [(keyword (str "param/" (ut/safe-name s)))]
                                                 is-param?  @(ut/tracked-sub ::conn/clicked-parameter-key-alpha {:keypath param-keyword})
                                                 param-dtype      (when is-param? (ut/data-typer is-param?))
                                                 param-dtype      (try (if (and (= param-dtype "vector") (every? string? is-param?))
                                                                         "string"
                                                                         param-dtype)
                                                                       (catch :default _ param-dtype)) ;; since
                                                 param-color      (get (theme-pull :theme/data-colors db/data-colors)
                                                                       param-dtype
                                                                       "orange")
                                                 fcolor           (cond is-param?          param-color
                                                                        selected?          tab-text-color
                                                                        (= s :layered-viz) "#FFA50087" ;(theme-pull
                                                                        (= s :dyn-tab)     "#FFA50087" ;(theme-pull
                                                                        :else              (theme-pull :theme/block-title-font-color
                                                                                                       "#ffffff50"))
                                                       ;; _ (when (= brick-vec-key :block-12050) (tapp>> [:runner? s (str @(ut/tracked-subscribe [::query-waitings s])) runner? query-running? runner-running?]))
                                                 bcolor           (if is-param? (str fcolor "22") (if selected? tab-color "inherit"))]
                                             (draggable
                                              (if is-param? ;; drop as a resgular param, not some view
                                                (let [k      (first param-keyword)
                                                      psplit (ut/splitter (ut/safe-name (str k)) "/")
                                                      table  (-> (first psplit)
                                                                 (ut/replacer #":" "")
                                                                 (ut/replacer ".*" "")
                                                                 keyword)
                                                      field  (keyword (last psplit))]
                                                  {:h         2 ;(cond is-image? 6 is-video? 9 :else
                                                   :w         5 ;(cond is-image? 6 is-video? 13 :else
                                                   :root      [0 0]
                                                   :drag-meta {:type        :param
                                                               :param-full  (first param-keyword)
                                                               :param-type  param-dtype
                                                               :param-table table
                                                               :param-field field}})
                                                (sql-spawner-cloner (if (some #(= % s) sql-keys) :query :view) brick-vec-key s))
                                              "meta-menu"
                                              ^{:key (str "brick-" brick-vec "-footer-sql-key-" s)}
                                              [re-com/h-box :children
                                               [[re-com/box :child (str s (when (and not-view? reco-count) (str " " (nf reco-count))))
                                                 :attr
                                                 {:on-click #(do (reset! mad-libs-view nil)
                                                                 (clear-preview2-recos)
                                                                 (tapp>> [:clicked-on brick-vec-key s])
                                                                 (ut/tracked-dispatch [::select-view brick-vec-key
                                                                                       (if (= s base-view-name) nil s)]))}
                                                 :style {:font-size        "12px"
                                                         :cursor           (if selected? "inherit" "pointer")
                                                         :font-weight      (cond ;single-tab? 500
                                                                             selected? 700
                                                                             :else     500)
                                                         :background-color bcolor
                                                         :color            fcolor
                                                         :margin-top       "-1px" ;(if is-param? "-3px"
                                                         :padding-left     "4px"
                                                         :padding-right    "4px"} :height "18px"]
                                                (when (or flow-running?
                                                          runner-running?
                                                          (and (not selected?) query-running? not-view?))
                                                  [re-com/md-icon-button
                                                   :md-icon-name "zmdi-refresh"
                                                   :class "rotate linear infinite"
                                                   :style {:font-size "15px" ;;"20px"
                                                           :color (theme-pull :theme/universal-pop-color nil)
                                                           :transform-origin "7.5px 10px" ;;"10px 11px"
                                                           :padding "0px"
                                                           :margin-left "4px"
                                                           :margin-right "4px"
                                                           :margin-top "-3px"}])
                                                (when (and 
                                                       (not= "0%" mat-pct)
                                                       (not= "100%" mat-pct)) [re-com/box
                                                               :style {:margin-top "-2px" :padding-left "5px" :padding-right "5px"
                                                                       :font-size "13px"  :font-weight 700
                                                                       ;:color (theme-pull :theme/universal-pop-color nil)
                                                                       :color fcolor
                                                                       }
                                                               :child (str mat-pct)])
                                                (when reco-ready?
                                                  [re-com/box :style {:margin-top "-6px"} :child
                                                   [re-com/md-icon-button :md-icon-name "zmdi-view-dashboard" :on-click
                                                    #(do (clear-preview2-recos)
                                                         (ut/tracked-dispatch [::select-view brick-vec-key
                                                                               (if (= s base-view-name) nil s)])
                                                         (reset! mad-libs-view (if (= s @mad-libs-view) nil s))
                                                         (when (not (= s
                                                                       @(ut/tracked-sub ::conn/clicked-parameter-key-alpha
                                                                                        {:keypath [:viz-tables-sys2/table_name]})))
                                                           (ut/tracked-dispatch [::conn/click-parameter [:viz-tables-sys2 :table_name]
                                                                                 s])))
                                                    :style {:font-size        "14px"
                                                            :cursor           "pointer"
                                                            :margin-top       "2px"
                                                            :color            (if (not (or block-selected?
                                                                                           viz-reco?
                                                                                           hover-q?
                                                                                           parent-of-selected?
                                                                                           upstream?
                                                                                           downstream?))
                                                                                fcolor
                                                                                bcolor)
                                                            :height           "17px"
                                                            :background-color "#00000000" ;bcolor
                                                            :padding-left     "2px"
                                                            :padding-right    "2px"}]])]]))))]
      
                                       [re-com/h-box
                                        :style {;:border "1px solid pink" 
                                                       ;:color "pink" 
                                                :padding "0px 5px 0px 5px"
                                                :z-index (when block-selected? 999999)
                                                :margin-right (when block-selected? "20px")
                                                :margin-top "-5px"
                                               ; :margin-top (if block-selected? "-6px" "-4px")
                                               }
                                               ;:height "15px" 
                                               ;:width "20px"
                                        :gap "5px"
                                        :justify :end 
                                        :children (vec (for [e valid-kits
                                                             :let [icon (get-in block-runners [(first e) :kits (last e) :icon])
                                                                   tooltip (str (get-in block-runners [(first e) :kits (last e) :tooltip] "(missing tooltip)"))
                                                                   kit-runner-key (str "kit-runner" (hash (str client-name brick-vec-key selected-view (first e) (last e))))
                                                                   running-key  (keyword (str "kit-status/" kit-runner-key ">running?"))
                                                                   output-key   (keyword (str "kit/" kit-runner-key ">incremental"))
                                                                   running?     @(ut/tracked-sub ::conn/clicked-parameter-key-alpha {:keypath [running-key]})
                                                                   trig!        [@waiting?]
                                                                   class        (cond
                                                                                  running? "rotate linear infinite"
                                                                                  (get @waiting? kit-runner-key) "rotate-reverse linear infinite"
                                                                                  :else "")]]
                                                         [re-com/box
                                                          :attr {:on-mouse-over #(reset! db/bar-hover-text tooltip)
                                                                 :on-mouse-leave #(reset! db/bar-hover-text nil)
                                                                 :on-click (fn []
                                                                             (when (not running?)
                                                                               (swap! db/kit-run-ids assoc (keyword kit-runner-key) (ut/generate-uuid))
                                                                               (swap! waiting? assoc kit-runner-key true)
                                                                               (swap! temp-extra-subs conj running-key)
                                                                               (swap! temp-extra-subs conj output-key)
                                                                               (swap! db/kit-fn-lookup assoc [brick-vec-key selected-view] running-key)
                                                                               (let [fstr (str "kit-runner " kit-runner-key)
                                                                                     w    (/ (count fstr) 4.1)]
                                                                                 (ut/tracked-dispatch
                                                                                  [::wfx/push   :default
                                                                                   {:kind       :run-kit
                                                                                    :kit-keypath e
                                                                                    :kit-runner-key kit-runner-key
                                                                                    :panel-key   brick-vec-key
                                                                                    :data-key    selected-view
                                                                                    :runner      selected-view-type
                                                                                    :client-name client-name
                                                                                    :ui-keypath     [:panels brick-vec-key selected-view-type selected-view]}])
                                                                                 (ut/dispatch-delay 800 [::http/insert-alert fstr w 1 5])
                                                                                 (tapp>> [:clicked-kit running-key])
                                                                                 (js/setTimeout #(swap! waiting? assoc kit-runner-key false) 5000))))}
                                                          :child (render-kit-icon icon class)]))]]]))
                        (cond selected?
                              ^{:key (str "brick-" brick-vec "-resize-handle")}
                              [re-com/box :size "none" :justify
                               :end :align :end :width "18px"
                               :height "18px" :attr
                               (if selected?
                                 {:on-mouse-down
                                  resize-mouse-down-handler}
                                 {:on-click #(ut/tracked-dispatch
                                              [::select-block
                                               brick-vec-key])})
                               :style
                               {:margin-top    "-2px"
                                :margin-right  "-2px"
                                :position      "fixed"
                                :left          (- block-width 22)
                                :top           (- block-height 18)
                                :border-right  (str "6px solid " (theme-pull :theme/universal-pop-color "#9973e0"))
                                :border-bottom (str "6px solid " (theme-pull :theme/universal-pop-color "#9973e0"))
                                :cursor        "se-resize"} :child
                               " "]
                              (and (or upstream? parent-of-selected? downstream?) (>= w 5)) [re-com/box :style
                                                                                             {:font-size     "10px"
                                                                                              :color         tab-color
                                                                                              :margin-top    "-1px"
                                                                                              :padding-right "5px"} :child
                                                                                             (cond parent-of-selected? "parent"
                                                                                                   downstream? "downstream"
                                                                                                   :else "upstream")]
                              :else                                                         ^{:key (str "brick-"
                                                                                                        brick-vec
                                                                                                        "-resize-handle-gap")}
                              [re-com/gap :size "12px"])]]])
                   [re-com/gap :size "18px"])]]])))))]]))



