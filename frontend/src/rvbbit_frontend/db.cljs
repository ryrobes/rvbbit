(ns rvbbit-frontend.db
  (:require
   [reagent.core :as reagent]
   [talltale.core :as tales]
   [clojure.string :as cstr]))

(defn gen-client-name []
  (let [names [(tales/quality) (rand-nth [(tales/shape) (tales/color)]) (tales/animal)]]
    (keyword (str (cstr/replace (cstr/join "-" names) " " "-") "-" (rand-int 45)))))

(def brick-size 50)
(def version "0.2.0-alpha")
(def version-date "10/2024")

(defonce client-name (gen-client-name))

(def rabbit-search-input (reagent/atom nil))
(def clover-leaf-previews (reagent/atom {}))
(def sockets [:query1  :query2  :query3])
(def reactor-types #{:flow :screen :time :signal :server :ext-param :solver :*data :incoming :solver-status :ai-worker :clover-gen :leaf :actions
                     :solver-meta :kit-status :kit :repl-ns :flow-status :signal-history :panel :client :settings :panel-hash})

(def drag-body-map (atom {}))

(def honey-comb-cache (atom {}))
(def clicked-parameter-key-cache (atom {}))
(def theme-pull-cache (atom {}))
(def data-colors-cache (atom {}))
(def runner-crosswalk-map-cache (atom {}))
(def view-type-cache (atom {}))

(def temp-viz-reco-panel-keys (reagent/atom {}))

(def bar-hover-text (reagent/atom nil))
(def console-voice-text (reagent/atom nil))
(def macro-undo? (reagent/atom false))
(defonce seq-atom (atom 0))
(def show-overlay (reagent/atom true))
(defonce dragged-kp (atom [:canvas :canvas :canvas]))
(defonce last-modal-viz-field (reagent/atom nil))

(defonce fresh-spawn-modal? (reagent/atom false))
(defonce drop-spawn-modal? (reagent/atom false))
(defonce drop-spawn-package (reagent/atom {}))

(defonce clover-cache-atom (reagent/atom {}))
(defonce rel-kw-cache-atom (atom {}))

(def canvas-scale (reagent/atom 0.7))
;; (defn svg-scale [] (str "scale(" @canvas-scale ") translate(0%, " (* (* @canvas-scale 10) -1) ")"))
;; "scale(0.5) translate(0%, -50%)"

(def hide-annotate? (reagent/atom false))

(def running-queries (atom #{}))

(defonce chat-audio-text (reagent/atom nil))
(defonce chat-image (reagent/atom nil))
(defonce custom-clover-drawers (reagent/atom {}))

(def click-params-snapshot (atom nil))
(def param-v-boxes (reagent/atom nil))
(def solver-view-mode (reagent/atom "return value"))
(def signals-searcher-atom (reagent/atom nil))
(def params-or-meta-atom (reagent/atom :params))
;; (def running-deep-meta-on (reagent/atom []))

(defonce cm-instance-runstream-code-box (atom {}))
(defonce search-box (reagent/atom nil))
(defonce context-modal-pos (reagent/atom [0 0]))
(defonce editor-mode (reagent/atom :meta))
(defonce item-browser-mode (reagent/atom :blocks))
(defonce auto-refresh? (reagent/atom false))
(defonce param-filter
  (reagent/atom {;:clicked true
                 :this-tab? true ;false
                 :user      true
                 :theme     false
                 :condis    false
                 ;:used?     false
                 }))
(def pause-alerts (reagent/atom false))
(defonce speech-log (reagent/atom []))
(def last-mouse-activity (atom nil))
(def snap-to-grid 25)

(defonce cm-focused? (reagent/atom false))
(defonce last-focused (reagent/atom nil))
(defonce  kit-run-ids (atom {}))

(defonce unsafe-keys (atom #{}))

(defonce view-title-edit-idx (reagent/atom nil))
(defonce chunk-chart (reagent/atom :server/cpu-chart))
(def chunk-charts [:server/cpu-chart :server/mem-chart :server/threads-chart :server/nrepl-chart :server/solvers-chart :server/flows-chart])

(defonce incidental-rowsets (atom []))

(def virtualized-debug-msg (reagent/atom {}))

(defonce shape-rotator (reagent/atom {}))

(defonce value-spy (reagent/atom {}))
(defonce value-spy-hashes (reagent/atom {}))
(defonce solver-meta-spy (reagent/atom {}))
(defonce signal-history-ticker? (reagent/atom true))

(defonce cm-instance-panel-code-box (atom {})) ;; for highlighting
(defonce markers-panel-code-box (atom {}))

(defonce autocomplete-keywords (atom []))

(def param-code-hover (atom nil))

(defonce solver-fn-runs (atom {}))
(defonce solver-fn-lookup (atom {}))
(defonce kit-fn-lookup (atom {}))
;(def solver-fn-lookup-vec (atom {}))

(def running-blocks (reagent/atom {}))
(def real-running-blocks (reagent/atom {}))

(def pixel-pusher-hover (reagent/atom nil))
(def pixel-pusher-key (reagent/atom nil))
(def sniff-deck (reagent/atom {}))
(def context-box (reagent/atom {}))

(defonce bad-form? (reagent/atom false))
(defonce bad-form-msg (reagent/atom nil))

(def bad-form-signals? (reagent/atom false))
(def bad-form-msg-signals (reagent/atom nil))

(defonce selectors-open (reagent/atom []))
(defonce selectors-items (reagent/atom {}))

(defonce kick-alert (reagent/atom false))

(defonce last-gantt (reagent/atom {}))
(def last-update (reagent/atom 0))

(def zoomer (reagent/atom nil))

(defonce flow-browser-panel? (reagent/atom true))

(def dragging-flow-editor? (reagent/atom false))
(def bumper (reagent/atom nil))

(defonce show-tabs? (reagent/atom true))
(defonce show-hidden-tabs? (reagent/atom false))
(defonce show-snaps? (reagent/atom true))
(defonce vertical-snaps? (reagent/atom false))
(defonce mad-libs-waiting-room (reagent/atom nil))
(defonce mad-libs-box (reagent/atom {}))

(defonce active-flow-scrubbers (reagent/atom {}))
(defonce scrubbers (reagent/atom {}))
(defonce honeycomb-builder (reagent/atom {}))
(defonce data-browser-query (reagent/atom {}))
(defonce data-browser-query-con (reagent/atom {}))
(defonce kit-browser (reagent/atom {}))

(def speaking (reagent/atom nil))
(def audio-data (reagent/atom {}))
(def audio-data2 (reagent/atom {}))
(def audio-blob-queue (atom {}))

(defonce kit-mode (reagent/atom {["none!" nil nil] :runstreams}))
(defonce kit-keys (reagent/atom {}))
(defonce chat-mode (reagent/atom {["none!" nil nil] :runstreams}))
(defonce flow-editor-system-mode (reagent/atom ["flows running" 800]))

(defonce flow-results (reagent/atom {}))

(def active-tmp-history (reagent/atom nil))
(def flow-panel-pcts [0.85 0.50])

(defonce based [-785 -862 0.925])
(defonce pan-zoom-offsets (reagent/atom based))
(def panning? (reagent/atom false))
(def zooming? (reagent/atom false))
(def zoomer-state (reagent/atom nil))

(defonce flow-detached-coords
  (reagent/atom (let [hh     (.-innerHeight js/window) ;; starting size set on load
                      ww     (.-innerWidth js/window) ;; starting size set on load
                      topper (* hh 0.06)
                      lefty  (* ww 0.06)]
                  [lefty topper])))

(def data-colors
  {"string"      "#5dc963" ; "#abdc32" ; "#feb733"
   "boolean"     "#f386bf"
   "integer"     "#d6e367" ; "#28ae80" ;"#50a978"
   "nil"         "#ff2147"
   "rabbit-code" "#FC0FC0"
   "rabbitsql"   "#FC0FC0"
   "unknown"     "#c7005d"
   "float"       "#2c728e" ;"#77ffd1"
   "date"        "#3b528b" ; "#66607d"
   "datetime"    "#472d7b" ; "#66607d"
   "map"         "#00FFFF"
   "keyword"     "#add8e6"
   "any"         "#ffffff"
   "list"        "#c32148"
   "vector"      "#008080"})

(def old-theme
  {:editor-rim-color                      "#41307f"
   :editor-font-color                     "#ffffff"
   :editor-background-color               "#000000"
   :editor-param-background-color         "#0b1122"
   :editor-grid-font-color                "#ffffff"
   :editor-grid-selected-background-color "#8879bc" ;"#bc798c"
   :block-title-font-color                "#ffffff"
   :block-title-selected-font-color       "#ffffff"
   :block-tab-selected-font-color         "#FFA50087"
   :monospaced-font                       "Fira Code"
   :grid-selected-column-css              {:background-color "#00000088" :filter "brightness(200%)"}
   :grid-selected-background-color        "#8879bc" ;"#bc798c"
   :editor-grid-selected-font-color       "#000000"
   :grid-selected-font-color              "#000000"
   :grid-font-color                       "#ffffff"
   :base-block-color                      "#0b1122"
   :codemirror-theme                      "ayu-mirage"
   :base-font                             "Alata"
   :canvas-background-css                 {:background-image  "url(images/fake-brick.png)"
                                           :background-repeat "repeat"
                                           :background-color  "#47555e"}
   :base-block-color-selected             "#0b031b"
   :editor-outer-rim-color                "#a891ff"
   :vega-default-color-scheme             {:scheme "magma"}
   :vega-defaults                         {:view   {:stroke "transparent"}
                                           :axis   {:domainColor "#ffffff22"
                                                    :grid        true
                                                    :labelColor  "#ffffff88"
                                                    :titleFont   "Lato"
                                                    :axisFont    "Lato"
                                                    :font        "Lato"
                                                    :titleColor  "#ffffff99"
                                                    :labelFont   "Lato"
                                                    :domain      false
                                                    :gridColor   "#ffffff22"
                                                    :tickColor   "#ffffff22"}
                                           :legend {:labelFont  "Lato"
                                                    :legendFont "Lato"
                                                    :labelColor "#ffffff99"
                                                    :titleColor "#ffffff99"
                                                    :stroke     "#ffffff99"
                                                    :titleFont  "Lato"}
                                           :header {:labelFont "Lato" :titleFont "Lato" :titleColor "#ffffff99"}
                                           :mark   {:font "Lato"}
                                           :title  {:font         "Lato"
                                                    :subtitleFont "Lato"
                                                    :labelFont    "Lato"
                                                    :titleFont    "Lato"
                                                    :titleColor   "#ffffff99"}}})

(def base-theme
  (merge
    old-theme ;; temp
    {:theme-name                            "who ya gonna call?"
     :codemirror-theme                      "ayu-mirage"
     :editor-param-background-color         "#0b1122"
     :universal-pop-color                   "#9973e0"
     :vega-defaults                         {:view   {:stroke "transparent"}
                                             :axis   {:domainColor "#ffffff22"
                                                      :grid        true
                                                      :font        "Lato"
                                                      :labelColor  "#ffffff88"
                                                      :titleFont   "Lato"
                                                      :titleColor  "#ffffff99"
                                                      :labelFont   "Lato"
                                                      :domain      false
                                                      :gridColor   "#ffffff22"
                                                      :tickColor   "#ffffff22"
                                                      :axisFont    "Lato"}
                                             :legend {:labelFont  "Lato"
                                                      :legendFont "Lato"
                                                      :labelColor "#ffffff99"
                                                      :titleColor "#ffffff99"
                                                      :stroke     "#ffffff99"
                                                      :titleFont  "Lato"}
                                             :header {:labelFont "Lato" :titleFont "Lato" :titleColor "#ffffff99"}
                                             :mark   {:font "Lato"}
                                             :title  {:font         "Lato"
                                                      :subtitleFont "Lato"
                                                      :labelFont    "Lato"
                                                      :titleFont    "Lato"
                                                      :titleColor   "#ffffff99"}}
     :grid-selected-column-css              {:background-color "#00000088" :filter "brightness(200%)"}
     :base-block-color-selected             "#0b031b"
     :block-title-font-color                "#ffffff"
     :base-block-color                      "#0b1122"
     :editor-rim-color                      "#0b1122"
     :vega-default-color-scheme             {:scheme "accent"}
     :editor-grid-selected-font-color       "#ffffff"
     :monospaced-font                       "IBM Plex Mono"
     :editor-grid-selected-background-color "#FFA50087"
     :data-colors                           data-colors
     :base-block-style                      {}
     :nivo-defaults                         {:font-family :theme/base-font
                                             :labels      {:text {:fill        "#ffffff"
                                                                  :font-size   "16px"
                                                                  :font-family :theme/base-font
                                                                  :font-weight 700}}
                                             :tooltip     {:container {:background   "#000"
                                                                       :color        "#ffffff"
                                                                       :text         {:fill "#eeeeee"}
                                                                       :fontSize     "18px"
                                                                       :borderRadius "4px"
                                                                       :boxShadow    "0 1px 2px rgba(0, 0, 0, 0.55)"
                                                                       :padding      "5px 9px"}
                                                           :basic     {:whiteSpace "pre" :display "flex" :alignItems "center"}
                                                           :tableCell {:padding "3px 5px"}}
                                             :axis        {:legend {:text {:fill        "#ffffff"
                                                                           :font-size   "14px"
                                                                           :font-family :theme/base-font
                                                                           :font-weight 700}}
                                                           :ticks  {:line {:stroke "#ffffff60"}
                                                                    :text {:fill "#ffffff60" :font-weight 700}}}
                                             :grid        {:line {:stroke "#ffffff" :strokeWidth 0 :strokeDasharray "0 0"}}}
     :editor-outer-rim-color                "#33ffb7"
     :grid-selected-font-color              "#ffffff"
     :block-title-selected-font-color       "#ffffff"
     :grid-selected-background-color        "#8879bc"
     :grid-font-color                       "#ffffff"
     :canvas-background-css                 {:background-image  "url(images/fake-brick5.png)"
                                             :transition        "all 0.6s ease-in-out"
                                             :background-color  "#47555e"
                                             :background-repeat "repeat"}
     :editor-font-color                     "#ffffff"
     :base-font                             "Oxygen Mono"
     :block-tab-selected-font-color         "#FFA500"
     :editor-background-color               "#000000"
     :editor-grid-font-color                "#ffffff"}))

(def default-db
  {:reco-preview       nil
   :editor?            false
   :buffy?             false
   :ai-worker?         false
   :client-name        client-name
   :flow-editor?       true
   :annotate?          false
   :grid-recos?        true
   :selected-flow      (str "my-new-flow" (rand-int 7564))
   :selected-cols      {}
   ;:annotations        {}
   :recos-page         0
   :recos-page2        0
   :selected-tab       "snowshoe hare"
   :tabs               ["snowshoe hare"]
   :click-param        {:theme base-theme}
   :panels             {}
   :window             {:w (.-innerWidth js/window) :h (.-innerHeight js/window)}})
