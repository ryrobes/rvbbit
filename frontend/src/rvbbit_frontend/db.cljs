(ns rvbbit-frontend.db
  (:require
   [reagent.core :as reagent]
   [talltale.core :as tales]
   [clojure.string :as cstr]))

(defn gen-client-name []
  (let [names [(tales/quality) (rand-nth [(tales/shape) (tales/color)]) (tales/animal)]]
    (keyword (str (cstr/replace (cstr/join "-" names) " " "-") "-" (rand-int 45)))))

(def brick-size 50)
(def version "snowshoe-hare.0.2.0")
(def version-date "december-28-2024")

(defonce loaded-screen? (reagent/atom false))
(defonce post-boot? (reagent/atom false))
(def loading-message (reagent/atom nil))
(def final-loading-message (reagent/atom nil))

(defonce client-name (gen-client-name))

(def formula-search-input (reagent/atom nil))
(def rabbit-search-input (reagent/atom nil))
(def clover-leaf-previews (reagent/atom nil))
(def sockets [:query1  :query2  :query3])
(def reactor-types #{:flow :screen :time :signal :server :ext-param :solver :*data
                     :incoming :solver-status :ai-worker :clover-gen :leaf :actions
                     :solver-meta :kit-status :kit :repl-ns :flow-status :signal-history :panel :client :settings
                     ;:panel-hash :data-hash
                     })

(def emoji-collection
  ["🦊" "🌸" "🍜"
   "🐇" "🥕" "🌱"
   "🦀" "🌊" "🏖️"
   "🦉" "🌙" "⭐"
   "🐸" "🍄" "🌿"
   "🦝" "🗑️" "✨"
   "🐢" "🌴" "🥥"
   "🦥" "🌳" "🍃"
   "🦩" "🌺" "💗"
   "🐠" "🐋" "🌊"
   "🦜" "🍉"
   "🦁" "🌞" "🦒"
   "🐼" "🎋" "🥢"
   "🦄" "🌟" "🍭"
   "🐉" "🏯" "🎋"
   "🦦" "🐟" "💦"
   "🦔" "🍎" "🍂"
   "🐌" "🍄" "🌧️"
   "🦚" "💎" "🌺"
   "🐝" "🌼" "🍯"
   "🦋" "🌸"
   "🐙" "🌊" "💫"
   "🦕" "🌿" "🗿"
   "🦘" "🌿" "🦘"
   "🐪" "🏜️" "⭐"
   "🦭" "🐟" "❄️"
   "🦩" "💕" "🌸"
   "🐳" "🌊" "🐋"
   "🦖" "🌋" "🔥"
   "🦡" "🌲" "🌛"])

(def messages
  [;; Functional programming jokes
   :avoiding-side-effects?
   :currying-favor
   :maintaining-immutability
   :pursuing-purity
   :recursing-recursively
   :reducing-complexity
   :mapping-over-manifolds
   :folding-space-time
   :evaluating-lazily

   :spawning-atoms
   :dereferencing-universe
   :proving-functions-are-just-expensive-closures
   :untangling-recursive-dreams-within-dreams
   :teaching-pure-functions-to-lie
   :quantum-currying-in-parallel-universes
   :folding-origami-in-fourth-dimensional-space
   :discovering-monad-shaped-holes-in-reality
   :feeding-infinity-to-lazy-evaluators

   ;; Database cosmic horror
   :teaching-postgres-forbidden-knowledge
   :normalizing-relationships-with-elder-gods
   :indexing-memories-that-never-existed
   :querying-the-void-between-joins
   :sharding-the-multiverse-for-better-throughput

   ;; Cyberpunk data viz
   :hijacking-neural-datapaths
   :synthesizing-illegal-color-spaces
   :rendering-impossible-geometries
   :calibrating-retinal-bandwidth
   :optimizing-wetware-interfaces

   ;; Tech/sci-fi mashups
   :reticulating-quantum-splines
   :defragmenting-temporal-cache
   :patching-universe-simulation
   :compiling-artificial-dreams
   :downloading-consciousness-updates
   :overclocking-reality-engine

   ;; Wonderland tech
   :debugging-cheshire-cat-smiles
   :calculating-exact-degree-of-madness
   :redefining-unbirthday-protocols
   :synchronizing-rabbit-hole-endpoints
   :optimizing-tea-party-entropy

   ;; Clojure cosmic horror
   :awakening-ancient-parentheses
   :summoning-elder-macros
   :threading-needles-through-spacetime
   :eval-ing-expressions-that-should-not-be

   ;; Delightfully absurd
   :convincing-pixels-to-dance
   :negotiating-with-rogue-algorithms
   :measuring-speed-of-dark
   :calculating-probability-of-improbability
   :teaching-silence-to-sing
   :unboxing-schrödingers-cache
   :dividing-by-zero-for-science

   ;; Meta weirdness
   :generating-more-genuine-artificial-progress
   :simulating-simulation-simulators
   :accelerating-time-perception-matrices
   :loading-alternative-loading-realities
   :upgrading-upgrade-upgraders
   :generating-status-updates
   :inventing-progress-bars
   :simulating-work
   :pretending-to-be-busy
   :adding-artificial-delay
   :finding-more-loading-messages])

(defn rabbit-svg [color & [stroke]]
  [:svg
   {:version "1.0"
    :xmlns "http://www.w3.org/2000/svg"
    :width "1453.000000pt"
    :height "794.000000pt"
    :viewBox "0 0 1453.000000 794.000000"
    :preserveAspectRatio "xMidYMid meet"}
   [:g
    {:transform "translate(0.000000,794.000000) scale(0.100000,-0.100000)"
     :fill color
    ; :style {:filter "drop-shadow(600px 600px 600px #000000)"}
     :stroke-width "100px"
     :vector-effect "non-scaling-stroke"
     :stroke (or stroke "none")}
    [:path
     {:d "M3934 7694 c-248 -41 -474 -157 -660 -336 -188 -181 -305 -396 -361
  -658 -24 -117 -24 -372 1 -488 84 -391 326 -707 674 -881 90 -45 253 -98 348
  -112 l81 -12 -18 -71 c-17 -67 -39 -191 -54 -298 l-6 -48 -629 0 -628 0 -5
  -302 c-5 -328 -11 -376 -74 -543 -135 -363 -432 -643 -801 -759 -145 -45 -243
  -56 -524 -56 l-258 0 0 -1256 0 -1256 373 5 c443 6 602 23 917 98 877 208
  1662 738 2186 1476 l66 92 976 3 c1076 4 1013 1 1279 69 224 57 461 162 659
  291 144 93 226 159 359 290 l111 108 61 -83 c245 -332 583 -550 1005 -649 93
  -22 95 -22 1528 -28 l1435 -5 474 -238 c260 -131 474 -237 476 -235 9 10 1115
  2230 1115 2238 0 5 -243 131 -539 280 l-539 269 44 105 c85 200 110 347 101
  601 -19 522 -186 991 -506 1420 -96 129 -347 380 -476 476 -382 285 -812 453
  -1274 499 -66 6 -791 10 -2012 10 l-1909 0 0 -228 c0 -275 13 -450 45 -608 13
  -65 23 -119 22 -121 -2 -1 -39 9 -82 22 -227 70 -427 99 -685 99 -278 0 -484
  -32 -734 -115 -65 -22 -119 -39 -120 -37 -1 2 -12 39 -25 83 -34 119 -76 214
  -139 315 -198 312 -505 517 -865 575 -112 18 -303 18 -413 -1z m285 -834 c164
  -31 298 -167 331 -337 33 -167 -66 -356 -229 -436 -62 -30 -72 -32 -176 -32
  -101 0 -115 2 -170 29 -121 59 -214 184 -234 314 -32 202 109 408 314 458 63
  16 99 16 164 4z m6542 -1 c261 -26 495 -105 717 -241 100 -61 176 -122 281
  -222 298 -290 465 -642 502 -1062 18 -199 -10 -301 -112 -409 -116 -123 -203
  -140 -659 -132 -254 5 -320 9 -392 26 -370 85 -658 349 -772 708 -19 63 -86
  450 -86 502 0 5 -285 11 -677 14 -562 4 -692 7 -758 20 -392 78 -720 324 -897
  676 -20 40 -41 86 -47 102 l-10 29 1403 0 c914 0 1440 -4 1507 -11z m-4206
  -862 c135 -34 232 -70 338 -125 377 -193 655 -552 754 -971 36 -157 43 -392
  15 -556 -68 -398 -294 -747 -627 -969 -150 -100 -301 -165 -495 -214 l-105
  -26 -1178 -3 -1178 -4 -25 -42 c-199 -338 -268 -439 -418 -614 -428 -498
  -1038 -848 -1678 -963 l-98 -17 0 417 0 417 58 13 c182 40 445 150 620 258
  251 156 482 381 647 632 119 180 234 444 281 643 l18 77 678 0 678 0 -5 23
  c-2 12 -16 135 -31 272 -48 456 -21 677 121 970 70 146 158 268 280 390 196
  196 409 317 675 385 134 34 186 39 385 35 163 -3 204 -7 290 -28z m2991 -1237
  c150 -35 278 -108 391 -220 115 -113 163 -201 254 -462 l44 -128 1067 0 1068
  0 270 -135 270 -135 -181 -363 c-100 -199 -182 -363 -184 -365 -2 -2 -84 38
  -184 87 l-181 91 -1447 0 c-963 0 -1470 4 -1513 11 -333 53 -607 321 -676 658
  -22 113 -15 287 16 386 39 126 105 239 192 331 123 130 247 203 413 245 99 25
  276 25 381 -1z"}]
    [:path
     {:d "M210 1875 l0 -1255 185 0 185 0 0 1255 0 1255 -185 0 -185 0 0 -1255z"}]]])

(def drag-body-map (atom {}))
(def shape-rotation-panel (atom nil))
(defonce viz-modes-atom (reagent/atom "quick viz"))
(defonce data-layers-open (reagent/atom {}))
(def render-pop? (reagent/atom {}))
(def cache-pop? (reagent/atom #{}))

(def panel-code-box-cache (reagent/atom {}))
(def panel-string-box-cache (reagent/atom {}))
(def panel-sql-box-cache (reagent/atom {}))

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

(def clover-cache-atom (atom {}))
(def rel-kw-cache-atom (atom {}))

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

(defonce reserved-block-keywords (atom #{}))
(defonce reserved-view-keywords (atom #{}))

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

;; (def old-theme
;;   {:editor-rim-color                      "#41307f"
;;    :editor-font-color                     "#ffffff"
;;    :editor-background-color               "#000000"
;;    :editor-param-background-color         "#0b1122"
;;    :editor-grid-font-color                "#ffffff"
;;    :editor-grid-selected-background-color "#8879bc" ;"#bc798c"
;;    :block-title-font-color                "#ffffff"
;;    :block-title-selected-font-color       "#ffffff"
;;    :block-tab-selected-font-color         "#FFA50087"
;;    :monospaced-font                       "Fira Code"
;;    :grid-selected-column-css              {:background-color "#00000088" :filter "brightness(200%)"}
;;    :grid-selected-background-color        "#8879bc" ;"#bc798c"
;;    :editor-grid-selected-font-color       "#000000"
;;    :grid-selected-font-color              "#000000"
;;    :grid-font-color                       "#ffffff"
;;    :base-block-color                      "#0b1122"
;;    :codemirror-theme                      "ayu-mirage"
;;    :base-font                             "Alata"
;;    :canvas-background-css                 {:background-image  "url(images/fake-brick.png)"
;;                                            :background-repeat "repeat"
;;                                            :background-color  "#47555e"}
;;    :base-block-color-selected             "#0b031b"
;;    :editor-outer-rim-color                "#a891ff"
;;    :vega-default-color-scheme             {:scheme "magma"}
;;    :vega-defaults                         {:view   {:stroke "transparent"}
;;                                            :axis   {:domainColor "#ffffff22"
;;                                                     :grid        true
;;                                                     :labelColor  "#ffffff88"
;;                                                     :titleFont   "Lato"
;;                                                     :axisFont    "Lato"
;;                                                     :font        "Lato"
;;                                                     :titleColor  "#ffffff99"
;;                                                     :labelFont   "Lato"
;;                                                     :domain      false
;;                                                     :gridColor   "#ffffff22"
;;                                                     :tickColor   "#ffffff22"}
;;                                            :legend {:labelFont  "Lato"
;;                                                     :legendFont "Lato"
;;                                                     :labelColor "#ffffff99"
;;                                                     :titleColor "#ffffff99"
;;                                                     :stroke     "#ffffff99"
;;                                                     :titleFont  "Lato"}
;;                                            :header {:labelFont "Lato" :titleFont "Lato" :titleColor "#ffffff99"}
;;                                            :mark   {:font "Lato"}
;;                                            :title  {:font         "Lato"
;;                                                     :subtitleFont "Lato"
;;                                                     :labelFont    "Lato"
;;                                                     :titleFont    "Lato"
;;                                                     :titleColor   "#ffffff99"}}})

;; (def base-theme22
;;   (merge
;;     old-theme ;; temp
;;     {:theme-name                            "who ya gonna call?"
;;      :codemirror-theme                      "ayu-mirage"
;;      :editor-param-background-color         "#0b1122"
;;      ;;;:universal-pop-color                   "#9973e0" ;;; NO
;;      :vega-defaults                         {:view   {:stroke "transparent"}
;;                                              :axis   {:domainColor "#ffffff22"
;;                                                       :grid        true
;;                                                       :font        "Lato"
;;                                                       :labelColor  "#ffffff88"
;;                                                       :titleFont   "Lato"
;;                                                       :titleColor  "#ffffff99"
;;                                                       :labelFont   "Lato"
;;                                                       :domain      false
;;                                                       :gridColor   "#ffffff22"
;;                                                       :tickColor   "#ffffff22"
;;                                                       :axisFont    "Lato"}
;;                                              :legend {:labelFont  "Lato"
;;                                                       :legendFont "Lato"
;;                                                       :labelColor "#ffffff99"
;;                                                       :titleColor "#ffffff99"
;;                                                       :stroke     "#ffffff99"
;;                                                       :titleFont  "Lato"}
;;                                              :header {:labelFont "Lato" :titleFont "Lato" :titleColor "#ffffff99"}
;;                                              :mark   {:font "Lato"}
;;                                              :title  {:font         "Lato"
;;                                                       :subtitleFont "Lato"
;;                                                       :labelFont    "Lato"
;;                                                       :titleFont    "Lato"
;;                                                       :titleColor   "#ffffff99"}}
;;      :grid-selected-column-css              {:background-color "#00000088" :filter "brightness(200%)"}
;;      :base-block-color-selected             "#0b031b"
;;      :block-title-font-color                "#ffffff"
;;      :base-block-color                      "#0b1122"
;;      :editor-rim-color                      "#0b1122"
;;      :vega-default-color-scheme             {:scheme "accent"}
;;      :editor-grid-selected-font-color       "#ffffff"
;;      :monospaced-font                       "IBM Plex Mono"
;;      :editor-grid-selected-background-color "#FFA50087"
;;      :data-colors                           data-colors
;;      :base-block-style                      {}
;;      :nivo-defaults                         {:font-family :theme/base-font
;;                                              :labels      {:text {:fill        "#ffffff"
;;                                                                   :font-size   "16px"
;;                                                                   :font-family :theme/base-font
;;                                                                   :font-weight 700}}
;;                                              :tooltip     {:container {:background   "#000"
;;                                                                        :color        "#ffffff"
;;                                                                        :text         {:fill "#eeeeee"}
;;                                                                        :fontSize     "18px"
;;                                                                        :borderRadius "4px"
;;                                                                        :boxShadow    "0 1px 2px rgba(0, 0, 0, 0.55)"
;;                                                                        :padding      "5px 9px"}
;;                                                            :basic     {:whiteSpace "pre" :display "flex" :alignItems "center"}
;;                                                            :tableCell {:padding "3px 5px"}}
;;                                              :axis        {:legend {:text {:fill        "#ffffff"
;;                                                                            :font-size   "14px"
;;                                                                            :font-family :theme/base-font
;;                                                                            :font-weight 700}}
;;                                                            :ticks  {:line {:stroke "#ffffff60"}
;;                                                                     :text {:fill "#ffffff60" :font-weight 700}}}
;;                                              :grid        {:line {:stroke "#ffffff" :strokeWidth 0 :strokeDasharray "0 0"}}}
;;      :editor-outer-rim-color                "#33ffb7"
;;      :grid-selected-font-color              "#ffffff"
;;      :block-title-selected-font-color       "#ffffff"
;;      :grid-selected-background-color        "#8879bc"
;;      :grid-font-color                       "#ffffff"
;;      :canvas-background-css                 {:background-image  "url(images/fake-brick5.png)"
;;                                              :transition        "all 0.6s ease-in-out"
;;                                              :background-color  "#47555e"
;;                                              :background-repeat "repeat"}
;;      :editor-font-color                     "#ffffff"
;;      :base-font                             "Oxygen Mono"
;;      :block-tab-selected-font-color         "#FFA500"
;;      :editor-background-color               "#000000"
;;      :editor-grid-font-color                "#ffffff"}))

(def base-theme {:theme-name "simple-pastel-dark",
                 :line-style "curved-path-h"
                 :running-gif "images/running.gif"
                 :codemirror-theme "rvbbit-dynamic",
                 :editor-param-background-color "#000000",
                 :pop-2 "#99ccff",
                 :base-block-style
                 {:border-radius "3px",
                  :border "1px solid #1a1a1a",
                  :background "#0a0a0a",
                  :letter-spacing "0.01em"},
                 :vega-defaults
                 {:mark {:font "Inter", :stroke "#99ccff", :color "#99ccff65"},
                  :legend
                  {:labelFont "Inter",
                   :legendFont "Inter",
                   :labelColor "#b4b4c8",
                   :titleColor "#b4b4c8",
                   :stroke "#99ccff",
                   :titleFont "Inter"},
                  :header {:labelFont "Inter", :titleFont "Inter", :titleColor "#b4b4c8"},
                  :title
                  {:font "Inter",
                   :subtitleFont "Inter",
                   :labelFont "Inter",
                   :titleFont "Inter",
                   :titleColor "#b4b4c8"},
                  :point {:font "Inter", :stroke "#99ccff", :color "#99ccff65"},
                  :area {:font "Inter", :stroke "#99ccff", :color "#99ccff65"},
                  :axis
                  {:domainColor "#202020",
                   :grid true,
                   :font "Inter",
                   :labelColor "#b4b4c8",
                   :titleFont "Inter",
                   :titleColor "#b4b4c8",
                   :labelFont "Inter",
                   :domain false,
                   :gridColor "#161616",
                   :tickColor "#202020",
                   :axisFont "Inter"},
                  :tooltip {:fill "#0a0a0a", :stroke "#1a1a1a", :strokeWidth 1, :color "#b4b4c8"},
                  :view {:stroke "transparent"}},
                 :grid-selected-column-css
                 {:background-color "#161616", :box-shadow "inset 0 0 0 1px #99ccff"},
                 :base-block-color-selected "#0f0f0f",
                 :block-title-font-color "#b4b4c8",
                 :base-block-color "#0a0a0a",
                 :pop-3 "#ffcccc",
                 :editor-rim-color "#1a2229",
                 :data-colors
                 {"float" "#99ccff",
                  "boolean" "#ffcccc",
                  "map" "#ccffcc",
                  "list" "#ffffcc",
                  "string" "#ffccff",
                  "any" "#99ccff",
                  "vector" "#ffcccc",
                  "keyword" "#ccffcc",
                  "rabbit-code" "#ffffcc",
                  "datetime" "#ffccff",
                  "integer" "#ccffcc",
                  "unknown" "#99ccff",
                  "date" "#ffcccc"},
                 :editor-grid-selected-font-color "#0a0a0a",
                 :monospaced-font "Fira Code",
                 :editor-grid-selected-background-color "#99ccff",
                 :nivo-defaults
                 {:font-family "Inter",
                  :labels
                  {:text
                   {:fill "#b4b4c8", :font-size "13px", :font-family "Inter", :font-weight 400}},
                  :tooltip
                  {:container
                   {:background "#0a0a0a",
                    :color "#b4b4c8",
                    :text {:fill "#b4b4c8"},
                    :fontSize "13px",
                    :borderRadius "2px",
                    :border "1px solid #1a1a1a",
                    :box-shadow "0 2px 4px rgba(0,0,0,0.2)",
                    :padding "8px 12px"},
                   :basic {:whiteSpace "pre", :display "flex", :alignItems "center"},
                   :tableCell {:padding "4px 8px"}},
                  :axis
                  {:legend
                   {:text
                    {:fill "#b4b4c8", :font-size "13px", :font-family "Inter", :font-weight 400}},
                   :ticks {:line {:stroke "#202020"}, :text {:fill "#b4b4c8", :font-weight 400}}},
                  :grid {:line {:stroke "#161616", :strokeWidth 1}}},
                 :editor-outer-rim-color "#99ccff",
                 :grid-selected-font-color "#0a0a0a",
                 :block-title-selected-font-color "#0a0a0a",
                 :grid-selected-background-color "#99ccff",
                 :grid-font-color "#b4b4c8",
                 :canvas-background-css {:background-color "#000000"}
                ;;  :canvas-background-css {:background-color "#00000045"
                ;;                          :background "url(images/large-rabbit.svg)" },
                ;;  :canvas-background-css {;:background-color "#00000045"
                ;;                          :background "url(images/large-rabbit.svg) center center no-repeat"
                ;;                          :background-size "800px auto"},
                 :editor-font-color "#b4b4c8",
                 :base-font "Inter",
                 :block-tab-selected-font-color "#61a377",
                 :editor-background-color "#0a0a0a",
                 :editor-grid-font-color "#b4b4c8",
                 :pop-1 "#e372f9"})

(def nivo-colors [{:id "nivo" :label "nivo" :group "Categorical colors"}
                  {:id "category10" :label "category10" :group "Categorical colors"}
                  {:id "accent" :label "accent" :group "Categorical colors"}
                  {:id "dark2" :label "dark2" :group "Categorical colors"}
                  {:id "paired" :label "paired" :group "Categorical colors"}
                  {:id "pastel1" :label "pastel1" :group "Categorical colors"}
                  {:id "pastel2" :label "pastel2" :group "Categorical colors"}
                  {:id "set1" :label "set1" :group "Categorical colors"}
                  {:id "set2" :label "set2" :group "Categorical colors"}
                  {:id "set3" :label "set3" :group "Categorical colors"}
                  {:id "tableau10" :label "tableau10" :group "Categorical colors"}
                  {:id "brown_blueGreen" :label "brown_blueGreen" :group "Diverging colors"}
                  {:id "purpleRed_green" :label "purpleRed_green" :group "Diverging colors"}
                  {:id "pink_yellowGreen" :label "pink_yellowGreen" :group "Diverging colors"}
                  {:id "purple_orange" :label "purple_orange" :group "Diverging colors"}
                  {:id "red_blue" :label "red_blue" :group "Diverging colors"}
                  {:id "red_grey" :label "red_grey" :group "Diverging colors"}
                  {:id "red_yellow_blue" :label "red_yellow_blue" :group "Diverging colors"}
                  {:id "red_yellow_green" :label "red_yellow_green" :group "Diverging colors"}
                  {:id "spectral" :label "spectral" :group "Diverging colors"}
                  {:id "blues" :label "blues" :group "Sequential colors"}
                  {:id "greens" :label "greens" :group "Sequential colors"}
                  {:id "greys" :label "greys" :group "Sequential colors"}
                  {:id "oranges" :label "oranges" :group "Sequential colors"}
                  {:id "purples" :label "purples" :group "Sequential colors"}
                  {:id "reds" :label "reds" :group "Sequential colors"}
                  {:id "blue_green" :label "blue_green" :group "Sequential colors"}
                  {:id "blue_purple" :label "blue_purple" :group "Sequential colors"}
                  {:id "green_blue" :label "green_blue" :group "Sequential colors"}
                  {:id "orange_red" :label "orange_red" :group "Sequential colors"}
                  {:id "purple_blue_green" :label "purple_blue_green" :group "Sequential colors"}
                  {:id "purple_blue" :label "purple_blue" :group "Sequential colors"}
                  {:id "purple_red" :label "purple_red" :group "Sequential colors"}
                  {:id "red_purple" :label "red_purple" :group "Sequential colors"}
                  {:id "yellow_green_blue" :label "yellow_green_blue" :group "Sequential colors"}
                  {:id "yellow_green" :label "yellow_green" :group "Sequential colors"}
                  {:id "yellow_orange_brown" :label "yellow_orange_brown" :group "Sequential colors"}
                  {:id "yellow_orange_red" :label "yellow_orange_red" :group "Sequential colors"}])

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
