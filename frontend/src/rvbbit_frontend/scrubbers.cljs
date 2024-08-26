(ns rvbbit-frontend.scrubbers
  (:require
    ["react-codemirror2"     :as cm]
    ["react-colorful"        :as react-colorful]
    ["react-drag-and-drop"   :as rdnd]
    [cljs.tools.reader       :refer [read-string]]
    [clojure.string          :as cstr]
    [clojure.walk            :as walk]
    [day8.re-frame.undo      :as    undo
                             :refer [undoable]]
    [garden.color            :as    c
                             :refer [analogous color? complement hex->hsl hex? hsl hsl->hex hsla invert mix rgb->hex rgb->hsl
                                     shades split-complement tetrad triad]]
    [re-com.core             :as    re-com
                             :refer [at]]
    [re-com.util             :refer [px]]
    [re-frame.core           :as re-frame]
    [reagent.core            :as reagent]
    [rvbbit-frontend.connections :as conn]
    [rvbbit-frontend.db      :as db]
    [rvbbit-frontend.shapes  :as shape]
    [rvbbit-frontend.utility :as ut]))


(defn theme-pull
  [cmp-key fallback & test-fn]
  @(ut/tracked-subscribe [:rvbbit-frontend.bricks/theme-pull-sub cmp-key fallback test-fn]))

(re-frame/reg-sub ::user-scrubber-options
                  (fn [db [_ _]]
                    (let [rs-scrubber-meta   (when (and (get db :buffy?) (= (get @db/chat-mode ["none!" nil nil]) :runstreams))
                                               (apply merge
                                                 (for [f (keys (get-in db [:runstreams]))]
                                                   (into {}
                                                         (for [[k v] (get-in db [:runstreams-lookups f :blocks])
                                                               :when (or (vector? (get-in v [:meta :* :scrubber]))
                                                                         (map? (get-in v [:meta :* :scrubber])))]
                                                           {[k] (get-in v [:meta :* :scrubber])})))))
                          flow-scrubber-meta (when (get db :flow?)
                                               (into {}
                                                     (for [[k v] (get-in db
                                                                         [:flows (get db :selected-flow) :map
                                                                          (get db :selected-flow-block) :data :flow-item :meta])
                                                           :when (or (vector? (get v :scrubber)) (map? (get v :scrubber)))]
                                                       {[k] (get v :scrubber)})))]
                      (merge flow-scrubber-meta
                             rs-scrubber-meta
                             (get-in db [:click-param :theme :*scrubber-options])
                             (get-in db [:click-param :param :*scrubber-options])))))

(re-frame/reg-sub ::user-scrubbers
                  (fn [db [_ _]]
                    (merge (get-in db [:click-param :theme :*scrubbers]) (get-in db [:click-param :param :*scrubbers]))))

(re-frame/reg-sub ::canvas-overrides
                  (fn [db _]
                    (let [obody      (get db :panels)
                          kps2       (ut/extract-patterns obody :scrubber 3)
                          logic-kps2 (into {}
                                           (for [v kps2]
                                             (let [[_ param opts] v
                                                   pkey-vec       [(keyword (last (ut/splitter (ut/replacer (str param) #":" "")
                                                                                               #"/")))]]
                                               {pkey-vec opts})))]
                      logic-kps2)))

(re-frame/reg-sub ::keypaths-of-layout-panels
                  (fn [db [_ & [panel-key view-key]]]
                    (let [panel-key (if (nil? panel-key) (get db :selected-block) panel-key) ;; if nil get
                          view-key  (if (nil? view-key) (get @db/data-browser-query panel-key) view-key)
                          panels    (get-in db [:panels panel-key :views view-key])
                          kvpaths   (ut/kvpaths panels)]
                      (vec (map (comp vec drop-last)
                             (distinct (map (comp vec drop-last)
                                         (filter #(and (= (last (drop-last %)) :root) (cstr/includes? (str %) " :panels "))
                                           kvpaths))))))))




(defn friendly-text
  []
  (let [theme-parameters @(ut/tracked-subscribe [::user-scrubber-options])
        canvas-overrides @(ut/tracked-subscribe [::canvas-overrides])]
    (merge
      {[:mark :type] {:message "vega-lite viz mark type"
                      :url     "https://vega.github.io/vega-lite/docs/mark.html"
                      :values  ["area" "bar" "circle" "line" "point" "rect" "rule" "square" "text" "tick"]}
       [1 :syntax] {:message "codemirror syntax highlighting - also changes how the data is stored(!)"
                    :values  ["clojure" "r" "javascript" "python" "julia" "text" "markdown"]}
       [[:encoding :x :type] [:encoding :y :type] [:encoding :color :type]]
         {:message "" :url "" :values ["quantitative" "temporal" "ordinal" "nominal"]}
       [:scheme]
         {:message "vega-lite color schemes / palettes"
          :url "https://vega.github.io/vega/docs/schemes/#reference"
          :values
            [{:id "accent" :label "accent" :group "Categorical Schemes"}
             {:id "category10" :label "category10" :group "Categorical Schemes"}
             {:id "category20" :label "category20" :group "Categorical Schemes"}
             {:id "category20b" :label "category20b" :group "Categorical Schemes"}
             {:id "category20c" :label "category20c" :group "Categorical Schemes"}
             {:id "dark2" :label "dark2" :group "Categorical Schemes"} {:id "paired" :label "paired" :group "Categorical Schemes"}
             {:id "pastel1" :label "pastel1" :group "Categorical Schemes"}
             {:id "pastel2" :label "pastel2" :group "Categorical Schemes"} {:id "set1" :label "set1" :group "Categorical Schemes"}
             {:id "set2" :label "set2" :group "Categorical Schemes"} {:id "set3" :label "set3" :group "Categorical Schemes"}
             {:id "tableau10" :label "tableau10" :group "Categorical Schemes"}
             {:id "tableau20" :label "tableau20" :group "Categorical Schemes"}
             {:id "blues" :label "blues" :group "Sequential Single-Hue Schemes"}
             {:id "tealblues" :label "tealblues" :group "Sequential Single-Hue Schemes"}
             {:id "teals" :label "teals" :group "Sequential Single-Hue Schemes"}
             {:id "greens" :label "greens" :group "Sequential Single-Hue Schemes"}
             {:id "browns" :label "browns" :group "Sequential Single-Hue Schemes"}
             {:id "oranges" :label "oranges" :group "Sequential Single-Hue Schemes"}
             {:id "reds" :label "reds" :group "Sequential Single-Hue Schemes"}
             {:id "purples" :label "purples" :group "Sequential Single-Hue Schemes"}
             {:id "warmgreys" :label "warmgreys" :group "Sequential Single-Hue Schemes"}
             {:id "greys" :label "greys" :group "Sequential Single-Hue Schemes"}
             {:id "viridis" :label "viridis" :group "Sequential Multi-Hue Schemes"}
             {:id "magma" :label "magma" :group "Sequential Multi-Hue Schemes"}
             {:id "inferno" :label "inferno" :group "Sequential Multi-Hue Schemes"}
             {:id "plasma" :label "plasma" :group "Sequential Multi-Hue Schemes"}
             {:id "cividis" :label "cividis" :group "Sequential Multi-Hue Schemes"}
             {:id "turbo" :label "turbo" :group "Sequential Multi-Hue Schemes"}
             {:id "bluegreen" :label "bluegreen" :group "Sequential Multi-Hue Schemes"}
             {:id "bluepurple" :label "bluepurple" :group "Sequential Multi-Hue Schemes"}
             {:id "goldgreen" :label "goldgreen" :group "Sequential Multi-Hue Schemes"}
             {:id "goldorange" :label "goldorange" :group "Sequential Multi-Hue Schemes"}
             {:id "goldred" :label "goldred" :group "Sequential Multi-Hue Schemes"}
             {:id "greenblue" :label "greenblue" :group "Sequential Multi-Hue Schemes"}
             {:id "orangered" :label "orangered" :group "Sequential Multi-Hue Schemes"}
             {:id "purplebluegreen" :label "purplebluegreen" :group "Sequential Multi-Hue Schemes"}
             {:id "purpleblue" :label "purpleblue" :group "Sequential Multi-Hue Schemes"}
             {:id "purplered" :label "purplered" :group "Sequential Multi-Hue Schemes"}
             {:id "redpurple" :label "redpurple" :group "Sequential Multi-Hue Schemes"}
             {:id "yellowgreenblue" :label "yellowgreenblue" :group "Sequential Multi-Hue Schemes"}
             {:id "yellowgreen" :label "yellowgreen" :group "Sequential Multi-Hue Schemes"}
             {:id "yelloworangebrown" :label "yelloworangebrown" :group "Sequential Multi-Hue Schemes"}
             {:id "yelloworangered" :label "yelloworangered" :group "Sequential Multi-Hue Schemes"}
             {:id "darkblue" :label "darkblue" :group "For Dark Backgrounds"}
             {:id "darkgold" :label "darkgold" :group "For Dark Backgrounds"}
             {:id "darkgreen" :label "darkgreen" :group "For Dark Backgrounds"}
             {:id "darkmulti" :label "darkmulti" :group "For Dark Backgrounds"}
             {:id "darkred" :label "darkred" :group "For Dark Backgrounds"}
             {:id "lightgreyred" :label "lightgreyred" :group "For Light Backgrounds"}
             {:id "lightgreyteal" :label "lightgreyteal" :group "For Light Backgrounds"}
             {:id "lightmulti" :label "lightmulti" :group "For Light Backgrounds"}
             {:id "lightorange" :label "lightorange" :group "For Light Backgrounds"}
             {:id "lighttealblue" :label "lighttealblue" :group "For Light Backgrounds"}
             {:id "blueorange" :label "blueorange" :group "Diverging Schemes"}
             {:id "brownbluegreen" :label "brownbluegreen" :group "Diverging Schemes"}
             {:id "purplegreen" :label "purplegreen" :group "Diverging Schemes"}
             {:id "pinkyellowgreen" :label "pinkyellowgreen" :group "Diverging Schemes"}
             {:id "purpleorange" :label "purpleorange" :group "Diverging Schemes"}
             {:id "redblue" :label "redblue" :group "Diverging Schemes"}
             {:id "redgrey" :label "redgrey" :group "Diverging Schemes"}
             {:id "redyellowblue" :label "redyellowblue" :group "Diverging Schemes"}
             {:id "redyellowgreen" :label "redyellowgreen" :group "Diverging Schemes"}
             {:id "spectral" :label "spectral" :group "Diverging Schemes"}
             {:id "rainbow" :label "rainbow" :group "Cyclical Schemes"}
             {:id "sinebow" :label "sinebow" :group "Cyclical Schemes"}]}
       [:aggregate] {:message "vega-lite axis aggregation fn used"
                     :url     "https://vega.github.io/vega-lite/docs/aggregate.html"
                     :values  ["count" "distinct" "sum" ;"product" "mean"
                               "average" "variance" "variancep" "stdev" "median" "q1" "q3" "ci0" "ci1" "min" "max"]}
       [:sort] {:message "vega-lite axis sorting attrib"
                :url     "https://vega.github.io/vega-lite/docs/sort.html"
                :values  ["x" "-x" "y" "-y" "ascending" "descending" "color" "-color"]}
       [:font-weight] {:message "CSS font-weight attribute (not all are supported in all fonts)"
                       :values  [100 200 300 400 500 600 700]}
       [:opacity] {:message "CSS opacity attribute" :values [0.0 0.05 1.0]}
       [:font-size] {:message "CSS font-size attribute" :values [0 1 350]}
       [:select] {:message "HoneySQL SELECT clause"}
       [:from] {:message "HoneySQL FROM clause"}
       [[:mix-blend-mode] [:background-blend-mode]] {:message "CSS color filtering"
                                                     :values  ["normal" "multiply" "screen" "overlay" "darken" "lighten"
                                                               "color-dodge" "color-burn" "hard-light" "soft-light" "difference"
                                                               "exclusion" "hue" "saturation" "color" "luminosity"]}
       [[:font-family] [:titleFont] [:monospaced-font] [:base-font] [:labelFont] [:legendFont]]
         {:message "font face!"
          :values  (distinct (sort ["VikingNormal" "WolfpackHalloweed" "Roboto" "Roboto Condensed" "Ubuntu" "Alata" "Oxygen Mono"
                                    "Lato" "Poppins" "Montserrat" "Open Sans" "Victor Mono" "Sudo Var" "Overpass Mono" "ZT Gatha"
                                    "Instagram Sans Condensed" "Instagram Sans" "Nova Square" "Noto Sans Linear A" "Nova Mono"
                                    "Share Tech Mono" "Oswald" "Raleway" "Homemade Apple" "Kulim Park" "JetBrains Mono"  "DejaVu Sans Mono for Powerline"
                                    "IBM Plex Mono" "Roboto Slab" "Source Code Pro" "Pastor of Muppets" "Odor Mean Chey"
                                    "Pastor of Muppets Flipped" "Roboto Condensed" "Fira Sans" "Fira Code" "Sono" "Chivo Mono"]))}
       [[:codemirror-theme]]
         {:message "codemirror theme"
          :values  (distinct (sort ["3024-day" "bespin" "gruvbox-dark" "material" "panda-syntax" "tomorrow-night-bright"
                                    "3024-night" "blackboard" "hopscotch" "mbo" "paraiso-dark" "tomorrow-night-eighties" "abcdef"
                                    "cobalt" "icecoder" "mdn-like" "paraiso-light" "ttcn" "ambiance-mobile" "colorforth" "idea"
                                    "midnight" "pastel-on-dark" "twilight" "ambiance" "darcula" "isotope" "monokai" "railscasts"
                                    "vibrant-ink" "ayu-dark-smaller" "dracula" "lesser-dark" "moxer" "rubyblue" "xq-dark"
                                    "rvbbit-dynamic" "ayu-dark" "duotone-dark" "liquibyte" "neat" "seti" "xq-light" "ayu-mirage"
                                    "duotone-light" "lucario" "neo" "shadowfox" "yeti" "ayu-yellow" "eclipse" "material-darker"
                                    "night" "solarized" "yonce" "base16-dark" "elegant" "material-ocean" "nord" "ssms" "zenburn"
                                    "base16-light" "erlang-dark" "material-palenight" "oceanic-next" "the-matrix"]))}}
      canvas-overrides
      theme-parameters)))

(re-frame/reg-event-db
  ::scrubber-view-update
  (undoable)
  (fn [db [_ kp v view-key type-key]]
    (let [selected-block (get db :selected-block)
          rs?            (and (vector? view-key) (= (first view-key) :runstreams))
          _ (ut/tapp>> [:scrubber-view-update kp v view-key type-key])
          full-kp        (cond (and (vector? view-key) (= (first view-key) :opts)) [:flows (get db :selected-flow) :opts]
                               (or (nil? view-key) (= view-key :base))             [:panels selected-block type-key]
                               (or (= view-key :theme) (= view-key :param))        [:click-param view-key]
                               rs?                                                 [:runstreams (second view-key) :values
                                                                                    (last view-key) :value]
                               (and (vector? view-key) (= (first view-key) :flow)) (if (= kp [:*])
                                                                                     [:flows (get db :selected-flow) :map
                                                                                      (last view-key) :data :user-input]
                                                                                     [:flows (get db :selected-flow) :map
                                                                                      (last view-key) :data :flow-item :defaults])
                               :else                                               [:panels selected-block type-key view-key])
          kkp            (if rs? full-kp (apply merge full-kp kp))
          kkp            (if (= kp [:*]) (vec (drop-last kkp)) kkp)]
      (ut/tapp>> [:scrubber-update kkp :with v view-key type-key kp full-kp])
      (assoc-in db kkp v))))

(defn is-hex-color? [s] (and (string? s) (cstr/starts-with? s "#") (or (= (count s) 4) (= (count s) 7) (= (count s) 9))))

(re-frame/reg-sub ::colors-used
                  (fn [db [_ view-key]]
                    (if (or (= view-key :theme) (= view-key :param))
                      (vec (distinct (filter is-hex-color? (ut/deep-flatten (get-in db [:click-param view-key])))))
                      (vec (distinct (filter is-hex-color?
                                       (ut/deep-flatten (dissoc (get db :panels) (get db :selected-block)))))))))

(defn ordered-keys-in-kp?
  [keypath-contains kp]
  (let [kstr (str keypath-contains) ;; cant use 'subset? since we care about the key order.
        len  (count kstr)
        ss   (subs kstr 1 (- len 1))]
    (cstr/includes? (str kp) ss)))

(defn ordered-keys-in-kp-multi?
  [keypath-contains kp]
  (if (vector? (first keypath-contains))
    (true? (some true? (for [kpc keypath-contains] (ordered-keys-in-kp-multi? kpc kp))))
    (ordered-keys-in-kp? keypath-contains kp)))

;; (defn custom-slider-style [track-color thumb-color]
;;   {:appearance "none"
;;    :height "10px"
;;    :background track-color
;;    :outline "none"
;;    :border-radius "8px"
;;    :opacity "0.7"
;;    :transition "opacity .2s"
;;    :-webkit-appearance "none"
;;    :cursor "pointer"

;;    ;; Webkit (Chrome, Safari, newer versions of Opera)
;;    "--webkit-slider-thumb" {:-webkit-appearance "none"
;;                              :appearance "none"
;;                              :width "20px"
;;                              :height "20px"
;;                              :background thumb-color
;;                              :cursor "pointer"
;;                              :border-radius "50%"}

;;    ;; Mozilla Firefox
;;    "--moz-range-thumb" {:width "20px"
;;                          :height "20px"
;;                          :background thumb-color
;;                          :cursor "pointer"
;;                          :border-radius "50%"
;;                          :border "none"}

;;    ;; Microsoft Edge and IE
;;    "--ms-thumb" {:width "20px"
;;                   :height "20px"
;;                   :background thumb-color
;;                   :cursor "pointer"
;;                   :border-radius "50%"}

;;    "--moz-range-track" {:background track-color}
;;    "--ms-track" {:background "transparent"
;;                   :border-color "transparent"
;;                   :color "transparent"}})


;; (defn custom-slider-style [track-color thumb-color]
;;   {:appearance "none"
;;    :-webkit-appearance "none"
;;    :-moz-appearance "none"
;;    ;:width "100%"
;;    :height "10px"
;;    :background track-color
;;    ;:background "transparent" ;;track-color
;;    :border (str "1px solid " thumb-color)
;;    :border-radius "5px"
;;    :outline "none"
;;    :opacity "0.7"
;;    :transition "opacity .2s"
;;    :cursor "pointer"

;;    ;; Webkit (Chrome, Safari, newer versions of Opera)
;;    "::-webkit-slider-runnable-track" {:width "100%"
;;                                       :height "10px"
;;                                       :background track-color
;;                                       :border-radius "5px"
;;                                       :border "none"}
;;    "::-webkit-slider-thumb" {:-webkit-appearance "none"
;;                              :appearance "none"
;;                              :width "20px"
;;                              :height "20px"
;;                              :background thumb-color
;;                              :cursor "pointer"
;;                              :border "none"
;;                              :border-radius "50%"
;;                              :margin-top "-5px"}

;;    ;; Mozilla Firefox
;;    "::-moz-range-track" {:width "100%"
;;                          :height "10px"
;;                          :background track-color
;;                          :border-radius "5px"
;;                          :border "none"}
;;    "::-moz-range-thumb" {:width "20px"
;;                          :height "20px"
;;                          :background thumb-color
;;                          :cursor "pointer"
;;                          :border-radius "50%"
;;                          :border "none"}

;;    ;; Microsoft Edge and IE
;;    "::-ms-track" {:width "100%"
;;                   :height "10px"
;;                   :background "transparent"
;;                   :border-color "transparent"
;;                   :color "transparent"}
;;    "::-ms-fill-lower" {:background track-color
;;                        :border-radius "5px"}
;;    "::-ms-fill-upper" {:background track-color
;;                        :border-radius "5px"}
;;    "::-ms-thumb" {:width "20px"
;;                   :height "20px"
;;                   :background thumb-color
;;                   :cursor "pointer"
;;                   :border-radius "50%"
;;                   :border "none"
;;                   :margin-top "0px"}})

(defn inject-slider-styles [track-color thumb-color]
  (let [style (str "input[type='range']::-webkit-slider-runnable-track {"
                   "width: 100%;"
                   "height: 10px;"
                   "background: " track-color ";"
                   "border-radius: 5px;"
                   ;"opacity: 0.7;"
                   ;"border: 1px solid " thumb-color ";"
                   "border: none;"
                   "}"
                   "input[type='range']::-webkit-slider-thumb {"
                   "-webkit-appearance: none;"
                   "appearance: none;"
                   "width: 20px;"
                   "height: 20px;"
                   "background: " thumb-color ";"
                   "cursor: pointer;"
                   "border: none;"
                   "border-radius: 10%;"
                   "margin-top: -5px;"
                   "}"
                   "input[type='range']::-moz-range-track {"
                   "width: 100%;"
                   "height: 10px;"
                   "background: " track-color ";"
                   "border-radius: 5px;"
                   "border: none;"
                   "}"
                   "input[type='range']::-moz-range-thumb {"
                   "width: 20px;"
                   "height: 20px;"
                   "background: " thumb-color ";"
                   "cursor: pointer;"
                   "border-radius: 10%;"
                   "border: none;"
                   "}"
                   "input[type='range']::-ms-track {"
                   "width: 100%;"
                   "height: 10px;"
                   "background: transparent;"
                   "border-color: transparent;"
                   "color: transparent;"
                   "}"
                   "input[type='range']::-ms-fill-lower {"
                   "background: " track-color ";"
                   "border-radius: 5px;"
                   "}"
                   "input[type='range']::-ms-fill-upper {"
                   "background: " track-color ";"
                   "border-radius: 5px;"
                   "}"
                   "input[type='range']::-ms-thumb {"
                   "width: 20px;"
                   "height: 20px;"
                   "background: " thumb-color ";"
                   "cursor: pointer;"
                   "border-radius: 10%;"
                   "border: none;"
                   "margin-top: 0px;"
                   "}")]
    (js/document.head.appendChild (let [style-el (.createElement js/document "style")]
                                    (set! (.-type style-el) "text/css")
                                    (set! (.-innerHTML style-el) style)
                                    style-el))))

;; (inject-slider-styles "#ffffff" (theme-pull :theme/editor-outer-rim-color nil))


;; Call `apply-custom-slider-style` wherever your themes are set or updated


(defn custom-slider-style [track-color thumb-color]
  {:appearance "none"
   :-webkit-appearance "none"
   :-moz-appearance "none"
   :height "10px"
   :background track-color
   :border (str "1px solid " thumb-color)
   :border-radius "5px"
   :outline "none"
   :opacity "0.7"
   :transition "opacity .2s"
   :cursor "pointer"

   ;; Webkit (Chrome, Safari, newer versions of Opera)
   "input[type='range']::-webkit-slider-runnable-track" {:width "100%"
                                                         :height "10px"
                                                         :background track-color
                                                         :border-radius "5px"
                                                         :border "none"}
   "input[type='range']::-webkit-slider-thumb" {:-webkit-appearance "none"
                                                :appearance "none"
                                                :width "20px"
                                                :height "20px"
                                                :background thumb-color
                                                :cursor "pointer"
                                                :border "none"
                                                :border-radius "50%"
                                                :margin-top "-5px"}

   ;; Mozilla Firefox
   "input[type='range']::-moz-range-track" {:width "100%"
                                            :height "10px"
                                            :background track-color
                                            :border-radius "5px"
                                            :border "none"}
   "input[type='range']::-moz-range-thumb" {:width "20px"
                                            :height "20px"
                                            :background thumb-color
                                            :cursor "pointer"
                                            :border-radius "50%"
                                            :border "none"}

   ;; Microsoft Edge and IE
   "input[type='range']::-ms-track" {:width "100%"
                                     :height "10px"
                                     :background "transparent"
                                     :border-color "transparent"
                                     :color "transparent"}
   "input[type='range']::-ms-fill-lower" {:background track-color
                                          :border-radius "5px"}
   "input[type='range']::-ms-fill-upper" {:background track-color
                                          :border-radius "5px"}
   "input[type='range']::-ms-thumb" {:width "20px"
                                     :height "20px"
                                     :background thumb-color
                                     :cursor "pointer"
                                     :border-radius "50%"
                                     :border "none"
                                     :margin-top "0px"}})


(defn int-slider
  [kp v view-key type-key & [[ext]]]
  [re-com/box :src (at) :child
   (let [v           (if (= ext :px) (int (first (cstr/split v "px"))) v)
         values      (get (first (remove nil? ;;; slider values should be [:min :step :max]
                                   (for [[k v] (friendly-text)] (when (ordered-keys-in-kp? k kp) v))))
                          :values)
         has-values? (= 3 (count values))
         min-val     (if has-values? (nth values 0) -20)
         step-val    (if has-values? (nth values 1) 1)
         max-val     (if has-values? (nth values 2) 200)]
     [re-com/h-box :src (at) :size "auto" :justify :between ;:size "1"
      :children
      [[re-com/md-icon-button :md-icon-name "zmdi-caret-left" :on-click
        #(ut/tracked-dispatch [::scrubber-view-update kp (if (= ext :px) (str (- v 1) "px") (- v 1)) view-key type-key]) 
        :style {:font-size "34px" :cursor "pointer" :opacity 0.3 :color "#ffffff" 
                :padding "0px" :margin-top "-11px"}]
       
       [re-com/slider :src (at) :model v :min min-val :step step-val :max max-val 
        :style (custom-slider-style "#ffffff" "#ffffff")
        :width (when (try (= (first view-key) :runstreams) (catch :default _ false)) "210px") :on-change
        #(ut/tracked-dispatch [::scrubber-view-update kp (if (= ext :px) (str % "px") %) view-key type-key]) :disabled? false]
       
       [re-com/md-icon-button :md-icon-name "zmdi-caret-right" :on-click
        #(ut/tracked-dispatch [::scrubber-view-update kp (if (= ext :px) (str (+ v 1) "px") (+ v 1)) view-key type-key]) 
        :style {:font-size "34px" :cursor "pointer" :opacity 0.3 :color "#ffffff" 
                :padding "0px" :margin-top "-11px"
                }]]])])


(defn options-col
  [colors curr-color kp view-key type-key label]
  [re-com/v-box :gap "7px" :padding "3px" :children
   (conj (vec (for [c colors]
                [re-com/box :child " " ; (str c)
                 :width "43px" :height "41px" :justify :center :align :center :attr
                 {:on-click #(ut/tracked-dispatch [::scrubber-view-update kp c view-key type-key])} :style
                 {:background-color c
                  :font-size        "9px"
                  :font-weight      500
                  :cursor           "pointer"
                  :border           (if (= c curr-color) "3px solid #c61e5d" "0px solid #00000000")
                  :border-radius    "5px"}]))
         [re-com/box :child label :width "29px" :height "41px" :padding "2px" :style
          {:transform "rotate(90deg)" :color "#ffffff44" :font-size "15px" :font-weight 400}])])

(defn color-farmer
  [kp v view-key type-key]
  (let [colors-used      @(ut/tracked-subscribe [::colors-used view-key])
        cccolor          (if (= (count v) 9) (subs v 0 7) v)
        colors-used      (vec (distinct (conj colors-used cccolor))) ;(if (empty? colors-used)
        complement-color (hsl->hex (complement cccolor))
        inverse-color    (hsl->hex (invert cccolor))
        shades-color     (vec (for [s (shades cccolor)] (hsl->hex s)))
        triad-color      (vec (for [s (triad cccolor)] (hsl->hex s)))
        tetrad-color     (vec (for [s (tetrad cccolor)] (hsl->hex s)))]
    [re-com/h-box :gap "3px" :style {:padding-top "3px"} :children
     [[options-col colors-used cccolor kp view-key type-key "used"]
      [options-col shades-color cccolor kp view-key type-key "shades"]
      [options-col triad-color cccolor kp view-key type-key "triad"]
      [options-col tetrad-color cccolor kp view-key type-key "tetrad"]
      [options-col [complement-color] cccolor kp view-key type-key "compliment"]
      [options-col [inverse-color] cccolor kp view-key type-key "inverse"]]]))

(defonce color-picker-enabled? (reagent/atom nil))

(defn color-picker
  [kp v view-key type-key]
  (when (= kp @color-picker-enabled?)
    [re-com/h-box :children
     [[re-com/box :src (at) :height "500px" :padding "5px" :child
       [(reagent/adapt-react-class react-colorful/HexColorPicker)
        {:color (str v) :onChange #(ut/tracked-dispatch [::scrubber-view-update kp % view-key type-key])}]]
      (try [color-farmer kp v view-key type-key] (catch js/Object e [re-com/box :child (str "Error: " e)]))]]))

(defn float-slider [kp v view-key type-key] [re-com/box :src (at) :child (str "float-slider " v)])

(defn text-box [kp v view-key type-key] [re-com/box :src (at) :child (str "text-box " v)])

(re-frame/reg-sub ::query-by-query-key
                  (fn [db [_ query-key]]
                    (first (remove nil?
                             (for [[k v] (get db :panels)]
                               (when (some #(= query-key %) (keys (get v :queries)))
                                 (get-in db [:panels k :queries query-key])))))))

(def lookup-atom (reagent/atom {}))
(def lookup-atom-f (reagent/atom {}))

(def shape-nubs ;[data-type]
  {"integer" [4 5 2 2] "string" [2 1 0 5] "float" [5 5 0 2]})

(defn shapes2
  [body color data-type v]
  (let [[a1 a2 a3 a4] (get shape-nubs data-type [0 0 0 0])
        new?          (= v :*new)
        color         (if new? "#ffffff" color)]
    [re-com/h-box :style
     {:filter (if new?
                "none"
                (str "drop-shadow(0px 0px 1px "
                     color
                     ") drop-shadow(0px 0px 1px "
                     color
                     ") drop-shadow(0px 0px 1px "
                     color
                     ") drop-shadow(0px 0px 1px "
                     color
                     ")"))} :padding "10px" :min-height "160px" :children
     [[re-com/v-box :children
       (for [r (range 3)]
         [re-com/box :child " " :style
          (merge {:background-color (if (= r a1) "#090f1e" "transparent")}
                 (if (and (= r a1) new?)
                   {:border-left   (str "3px dashed " color 36)
                    :border-top    (str "3px dashed " color 36)
                    :border-bottom (str "3px dashed " color 36)}
                   {})) :width "21px" :height "33px" :size "none"])]
      [re-com/v-box :size "auto" :children
       [[re-com/h-box :children
         (for [r (range 3)]
           [re-com/box :child " " :style
            (merge {:background-color (if (= r a2) "#090f1e" "transparent")}
                   (if (and (= r a2) new?)
                     {:border-left  (str "3px dashed " color 36)
                      :border-top   (str "3px dashed " color 36)
                      :border-right (str "3px dashed " color 36)}
                     {})) :width "33px" :height "21px" :size "none"])]
        [re-com/box :child body :size "auto" :style
         {:background-color "#090f1e" ; (str color 25)
          :border           (if new? (str "3px dashed " color 36) "none")}]
        [re-com/h-box :children
         (for [r (range 3)]
           [re-com/box :child " " :style
            (merge {:background-color (if (= r a3) "#090f1e" "transparent")}
                   (if (and (= r a3) new?)
                     {:border-left   (str "3px dashed " color 36)
                      :border-right  (str "3px dashed " color 36)
                      :border-bottom (str "3px dashed " color 36)}
                     {})) :width "33px" :height "21px" :size "none"])]]]
      [re-com/v-box :children
       (for [r (range 3)]
         [re-com/box :child " " :style
          (merge {:background-color (if (= r a4) "#090f1e" "transparent")}
                 (if (and (= r a4) new?)
                   {:border-right  (str "3px dashed " color 36)
                    :border-top    (str "3px dashed " color 36)
                    :border-bottom (str "3px dashed " color 36)}
                   {})) :width "21px" :height "33px" :size "none"])]]]));)

(defn code-box
  [panel-id key width-int height-int value kp view-key type-key]
  (let [sql-hint? (cstr/includes? (str value) ":::sql-string")]
    [re-com/box :size "none" :width (px (- width-int 24)) :height (px (- height-int 24)) :style
     {:font-family "Chivo Mono" ;"Fira Code"
      :font-size   "13px"
      :overflow    "auto"
      :border      "1px solid #ffffff22"
      :font-weight 700} :child
     [(reagent/adapt-react-class cm/UnControlled)
      {:value   (ut/format-map (- width-int 24) (str value))
       :onBlur  #(ut/tracked-dispatch-sync [::scrubber-view-update kp (read-string (cstr/join " " (ut/cm-deep-values %))) view-key
                                            type-key])
       :options {:mode              (if sql-hint? "sql" "clojure")
                 :lineWrapping      true
                 :lineNumbers       false
                 :matchBrackets     true
                 :autoCloseBrackets true
                 :autofocus         false
                 :autoScroll        false
                 :detach            true
                 :readOnly          false ;true
                 :theme             "ayu-mirage" ;"hopscotch"
                }}]]))

(defn shapes
  [body color data-type v]
  (let [zmdi-icons ["zmdi-triangle-down" "zmdi-triangle-up" "zmdi-square-o" "zmdi-circle-o" "zmdi-star-outline" "zmdi-star"
                    "zmdi-dot-circle" "zmdi-circle" "zmdi-network" "zmdi-network-outline" "zmdi-brightness-2" "zmdi-brightness-5"]
        ico        (nth zmdi-icons 1)
        new?       (= v :*new)
        color      (if new? "#ffffff" color)]
    [re-com/h-box :size "auto" :style
     {:background-color (str color 11) :border (if new? (str "1px dashed " color) (str "1px solid " color))} :justify :between
     :children
     [body
      [re-com/box :child
       [re-com/md-icon-button :md-icon-name ico :style
        {:font-size "15px" :cursor "pointer" :color (str color 66) :padding "3px" :margin-top "-5px"}]]]]))

(defn clause-vec-loop
  [kp v view-key fp idx]
  (if (= fp 1)
    (let [;result-fields @(ut/tracked-subscribe [::conn/sql-merged-metadata [view-key]])
         ] ;; need to get resulting data types BEFORE the investigation loop...
      [re-com/v-box :size "auto" :children
       [;[re-com/box :child (str (get-in @lookup-atom-f [view-key idx]))]
        [shapes [clause-vec-loop kp v view-key 0 idx]
         (get @(ut/tracked-subscribe [::conn/data-colors]) (get-in @lookup-atom [view-key idx]) "#ffffff22")
         (get-in @lookup-atom [view-key idx]) v]]])
    (if (try (> (count v) 1) (catch :default e false))
      [re-com/v-box :justify :between :size "auto" :padding "1px" :style {:padding-left "5px" :padding-right "5px"} :children
       (for [e (range (count v))] [clause-vec-loop (conj kp e) (get v e) view-key 0 idx])]
      (let [pos            (- (count kp) 1)
            lpos           (last kp)
            src-table      (first (flatten (get @(ut/tracked-subscribe [::query-by-query-key view-key]) :from)))
            src-table-base (if (cstr/starts-with? (str src-table) ":query/")
                             (keyword (last (ut/splitter (str src-table) "/")))
                             src-table)
            metad          @(ut/tracked-subscribe [::conn/sql-merged-metadata [view-key]])
            src-metad      @(ut/tracked-subscribe [::conn/sql-merged-metadata [src-table-base]])
            pos-meaning    (cond (or (and (= pos 0) (or (or (= lpos :group-by) (= lpos :order-by) (= lpos :select)) (= lpos 0)))
                                     (and (= pos 1) (= lpos 0)))
                                   :base-field
                                 (and (= lpos 1) (= pos 1)) :alias
                                 (and (= pos 2) (= lpos 0)) :fn-name
                                 (and (= pos 2) (= lpos 1)) :fn-val
                                 :else :?)
            mdata?         (true? (or (= pos-meaning :base-field) (= pos-meaning :alias) (= pos-meaning :fn-val)))
            data-type      (cond (and mdata? (keyword? v) (= pos-meaning :fn-val))       (get-in src-metad [:fields v :data-type])
                                 (and mdata? (not (keyword? v)) (= pos-meaning :fn-val)) (ut/data-typer v)
                                 mdata?                                                  (get-in metad [:fields v :data-type]))
            data-ex        (cond (and mdata? (keyword? v) (= pos-meaning :fn-val))       (cstr/join ", "
                                                                                                    (keys (get-in src-metad
                                                                                                                  [:fields v
                                                                                                                   :commons])))
                                 (and mdata? (not (keyword? v)) (= pos-meaning :fn-val)) (str v)
                                 mdata?                                                  (cstr/join ", "
                                                                                                    (keys (get-in metad
                                                                                                                  [:fields v
                                                                                                                   :commons]))))
            base-box       [re-com/v-box :justify :between :size "auto" :children
                            [[re-com/h-box :justify :between :children
                              [[re-com/box :child (str v) ;(if (= pos-meaning :alias) (str v "*") (str v ))
                                :style
                                {:font-style     (if (= pos-meaning :alias) "italic" "none")
                                 :color          (if (= v :*new) "#ffffff" "inherit")
                                 :padding-left   "3px"
                                 :padding-bottom "3px"
                                 :font-weight    700
                                 :opacity        (if (= pos-meaning :alias) 0.3 1.0)
                                 :font-size      "14px"}]
                               (when mdata?
                                 [re-com/box :padding "4px" :child (str data-type) :style
                                  {:opacity 0.4 :font-weight 200 :font-size "10px"}])]]
                             (when mdata?
                               [re-com/box :align :end :child
                                (let [slen   (count (str data-ex))
                                      ex-str (if (> slen 100) (str (subs (str data-ex) 0 20) "...") data-ex)]
                                  (str ex-str)) :style
                                {:opacity 0.6 :padding-right "4px" :padding-bottom "2px" :font-weight 200 :font-size "12px"}])]
                            :style
                            {;:border-top (str "1px solid " (get @(ut/tracked-subscribe
                             :color       (get @(ut/tracked-subscribe [::conn/data-colors]) data-type "#ffffff33")
                             :font-size   "11px"
                             :font-weight 200}]]
        (if (and (not (nil? data-type)) (= pos-meaning :fn-val))
          [shapes
           [re-com/v-box :children
            [;[re-com/box :child (str (if (not (keyword? v)) "fn-val" "field-val"))]
             base-box] :size "auto"] (get @(ut/tracked-subscribe [::conn/data-colors]) data-type "#ffffff33")
           (get-in @lookup-atom [view-key idx]) v]
          base-box)))))

(defn select-clause
  [kp v view-key type-key]
  (let [parted? (> (count v) 1)
        pstyle  {;:zoom 0.65
                 :padding "8px"}]
    (if parted?
      [re-com/v-box :justify :between :src (at) :size "auto" :style pstyle :gap "5px" :children
       (for [p (partition-all 1 v)]
         [re-com/h-box :padding "1px" :size "auto" :gap "3px" :children
          (for [vv p]
            [re-com/box :size "auto" :padding "2px" :width "400px" :child
             [re-com/v-box :size "auto" :gap "3px" :children
              [[clause-vec-loop kp vv view-key 1 (.indexOf v vv)]
               [code-box "panel-key" view-key 524 55 (str vv) (conj kp (.indexOf v vv)) view-key type-key]]]])])]
      [re-com/box :src (at) :size "auto" :child
       [re-com/h-box :style pstyle :padding "1px" :size "auto" :gap "3px" :children
        (for [vv v]
          [re-com/box :size "auto" :padding "2px" :width "400px" :child [clause-vec-loop kp vv view-key 1 (.indexOf v vv)]])]])))

(defn button-panel
  [kp v view-key type-key]
  (let [values (get (first (remove nil? (for [[k v] (friendly-text)] (when (ordered-keys-in-kp-multi? k kp) v)))) :values)]
    [re-com/h-box :src (at) :gap "3px" :size "auto" :children
     (for [vv values]
       [re-com/box :src (at) :align :center :justify :center :size "auto" :padding "3px" :child (str vv) :attr
        {:on-click #(ut/tracked-dispatch [::scrubber-view-update kp vv view-key type-key])} :style
        {:color            (if (= vv v) "#000000" "#ffffff77")
         :cursor           "pointer"
         :font-size        "12px"
         :font-weight      700
         :background-color (if (= vv v)
                             (theme-pull :theme/editor-outer-rim-color nil)
                             (str (theme-pull :theme/editor-outer-rim-color nil) 33))}])]))

(defn pixel-controller
  [id ico kp]
  (let [hovered? (= @db/pixel-pusher-hover [id ico kp])]
    [re-com/md-icon-button :src (at) :attr
     {:on-mouse-enter #(reset! db/pixel-pusher-hover [id ico kp]) :on-mouse-leave #(reset! db/pixel-pusher-hover nil)}
     :md-icon-name ico ; "zmdi-chevron-down"
     :style {:color (if hovered? "#ffffff" "#ffffff55") :border (when hovered? "3px solid #201d2a")}]))

(defn clojure-box
  [width-int height-int value kp v view-key type-key]
  ^{:key (str "clojure-box-" kp)}
  [re-com/box :attr {:id (str "clojure-box-" kp)} :size "auto" :padding "3px" :style
   {:background-color "#008B8B22"
    :border           "1px solid #008B8B55" ;(if @db/bad-form? "2px solid red" "1px solid
    :border-radius    "16px"
    :font-family      (theme-pull :theme/monospaced-font nil) ; "Fira Code" ;"Chivo Mono"
    :font-size        "13px"
    :overflow-y       "auto"
    :overflow-x       "hidden"
    :font-weight      400} :child
   ^{:key (str "clojure-box-" kp "/cm")}
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
               :readOnly          true ;false
               :theme             (theme-pull :theme/codemirror-theme nil) ;"ayu-mirage";
              }}]])

(defn trunc [n] (/ (js/Math.floor (* n 100)) 100))

(defonce use-percents? (reagent/atom true))

(defn layout-box
  [kp v view-key type-key]
  (let [;reacthack @pixel-pusher-hover ;; imp! Use-percents? @use-percents? ;; keep the
        x                  (get-in v [:root 0])
        y                  (get-in v [:root 1])
        w                  (get v :w)
        h                  (get v :h)
        z                  (get v :z 0)
        base-pct-val       0.1
        base-val           0.5
        inc-val            (if @use-percents? base-pct-val base-val)
        x+                 #(ut/tracked-dispatch [::scrubber-view-update kp (assoc v :root [(trunc (+ x inc-val)) y]) view-key
                                                  type-key])
        x-                 #(ut/tracked-dispatch [::scrubber-view-update kp (assoc v :root [(trunc (- x inc-val)) y]) view-key
                                                  type-key])
        y+                 #(ut/tracked-dispatch [::scrubber-view-update kp (assoc v :root [x (trunc (+ y inc-val))]) view-key
                                                  type-key])
        y-                 #(ut/tracked-dispatch [::scrubber-view-update kp (assoc v :root [x (trunc (- y inc-val))]) view-key
                                                  type-key])
        h+                 #(ut/tracked-dispatch [::scrubber-view-update kp (assoc v :h (trunc (+ h inc-val))) view-key type-key])
        h-                 #(ut/tracked-dispatch [::scrubber-view-update kp (assoc v :h (trunc (- h inc-val))) view-key type-key])
        w+                 #(ut/tracked-dispatch [::scrubber-view-update kp (assoc v :w (trunc (+ w inc-val))) view-key type-key])
        w-                 #(ut/tracked-dispatch [::scrubber-view-update kp (assoc v :w (trunc (- w inc-val))) view-key type-key])
        z+                 #(ut/tracked-dispatch [::scrubber-view-update kp (assoc v :z (trunc (+ z 1))) view-key type-key])
        z-                 #(ut/tracked-dispatch [::scrubber-view-update kp (assoc v :z (trunc (- z 1))) view-key type-key])
        editor-box         [re-com/box :padding "10px" :size "auto" :child [clojure-box 220 100 v kp v view-key type-key]]
        percent-toggle     [re-com/v-box :style {:margin-top "-8px"} :children
                            [[re-com/box :height "21px" :align :center :justify :center :child
                              [re-com/checkbox :model use-percents? ;false
                               :on-change #(reset! use-percents? (not @use-percents?)) :label
                               [re-com/box :child "by 1 or 0.1" :style {:padding-top "4px"}]] :style
                              {:font-family  "Kulim Park"
                               :font-weight  500
                               :font-size    "10px"
                               :color        "#ffffff66"
                               :padding-left "40px"}]
                             [re-com/box :height "21px" :style {:font-weight 200 :margin-left "41px" :opacity 0.7} :align :center
                              :justify :center :child "(< 1.0 = %)"]]]
        button-block-style {:border "1px solid #201d2a" :background-color "#201d2a" :cursor "pointer"}
        blank-block-style  {:border "0px solid #aaaaaa"}
        filled-block-style {:border "0px solid green" :background-color "#201d2a"}]
    [re-com/h-box :attr {:on-mouse-enter #(reset! db/pixel-pusher-key kp) :on-mouse-leave #(reset! db/pixel-pusher-key nil)}
     :children
     [[re-com/box :padding "15px" :width "300px" :size "none" :align :center :justify :center :child
       [re-com/h-box :gap "10px" :justify :between :children
        [[re-com/h-box :justify :between :size "auto" :children
          [[re-com/box ;:size "160px"
            :size "auto" :child
            [re-com/v-box ;;:align :center  :justify :center
             :style {:font-family "Roboto Mono" :font-size "10px" :font-weight 500 :color "#ffffff77"} :padding "5px" ;:size
                                                                                                                      ;"auto"
             :children
             [[re-com/h-box :children
               [[re-com/box :child
                 [re-com/v-box :align :center :justify :center :children
                  [[re-com/box :child (str x)] [re-com/box :child "x" :style {:font-size "8px" :font-weight 400}]]] ;(str x)
                 :align :center :width "33px" :height "31px" :justify :center                         ; :size
                                                                                                      ; "30px"
                 :style blank-block-style]
                [re-com/box :child [pixel-controller :y "zmdi-chevron-up" kp] :align :center :width "33px" :height "31px" :justify
                 :center                                                                              ; :size
                                                                                                      ; "30px"
                 :attr {:on-click y-} :style button-block-style]
                [re-com/box :child " " :align :center :width "33px" :height "31px" :justify :center   ; :size
                                                                                                      ; "30px"
                 :style blank-block-style]
                [re-com/box :child " " :align :center :width "33px" :height "31px" :justify :center   ; :size
                                                                                                      ; "30px"
                 :style blank-block-style]
                [re-com/box :child [pixel-controller :h "zmdi-chevron-up" kp] :align :center :width "33px" :height "31px" :justify
                 :center                                                                              ; :size
                                                                                                      ; "30px"
                 :attr {:on-click h+} :style button-block-style]
                [re-com/box :child " " :align :center :width "33px" :height "31px" :justify :center   ; :size
                                                                                                      ; "30px"
                 :style blank-block-style]
                [re-com/box :child [pixel-controller :z "zmdi-plus" kp] :align :center :width "33px" :height "31px" :justify
                 :center                                                                              ; :size
                                                                                                      ; "30px"
                 :attr {:on-click z+} :style filled-block-style]]]
              [re-com/h-box :children
               [[re-com/box :child [pixel-controller :x "zmdi-chevron-left" kp] :align :center :width "33px" :height "31px"
                 :justify :center                                                                     ; :size
                                                                                                      ; "30px"
                 :attr {:on-click x-} :style button-block-style]
                [re-com/box :child " o " :align :center :width "33px" :height "31px" :justify :center ; :size
                                                                                                      ; "30px"
                 :style filled-block-style]
                [re-com/box :child [pixel-controller :x "zmdi-chevron-right" kp] :align :center :width "33px" :height "31px"
                 :justify :center                                                                     ; :size
                                                                                                      ; "30px"
                 :attr {:on-click x+} :style button-block-style]
                [re-com/box :child " " :align :center :width "33px" :height "31px" :justify :center   ; :size
                                                                                                      ; "30px"
                 :style blank-block-style]
                [re-com/box :child
                 [re-com/v-box :align :center :justify :center :children
                  [[re-com/box :child (str h)] [re-com/box :child "height" :style {:font-size "8px" :font-weight 400}]]] ;(str
                                                                                                                         ;h)
                 :align :center :width "33px" :height "31px" :justify :center                       ; :size
                                                                                                    ; "30px"
                 :style blank-block-style]
                [re-com/box :child " " :align :center :width "33px" :height "31px" :justify :center ; :size
                                                                                                    ; "30px"
                 :style blank-block-style]
                [re-com/box :child
                 [re-com/v-box :align :center :justify :center :children
                  [[re-com/box :child (str z)] [re-com/box :child "order" :style {:font-size "8px" :font-weight 400}]]] ;(str o)
                 :align :center :width "33px" :height "31px" :justify :center                       ; :size
                                                                                                    ; "30px"
                 :style blank-block-style]]]
              [re-com/h-box :children
               [[re-com/box :child " " :align :center :width "33px" :height "31px" :justify :center ; :size
                                                                                                    ; "30px"
                 :style blank-block-style]
                [re-com/box :child [pixel-controller :y "zmdi-chevron-down" kp] :align :center :width "33px" :height "31px"
                 :justify :center                                                                   ; :size
                                                                                                    ; "30px"
                 :attr {:on-click y+} :style button-block-style]
                [re-com/box :child
                 [re-com/v-box :align :center :justify :center :children
                  [[re-com/box :child (str y)] [re-com/box :child "y" :style {:font-size "8px" :font-weight 400}]]] ;(str y)
                 :align :center :width "33px" :height "31px" :justify :center                       ; :size
                                                                                                    ; "30px"
                 :style blank-block-style]
                [re-com/box :child " " :align :center :width "33px" :height "31px" :justify :center ; :size
                                                                                                    ; "30px"
                 :style blank-block-style]
                [re-com/box :child [pixel-controller :h "zmdi-chevron-down" kp] :align :center :width "33px" :height "31px"
                 :justify :center                                                                   ; :size
                                                                                                    ; "30px"
                 :attr {:on-click h-} :style button-block-style]
                [re-com/box :child " " :align :center :width "33px" :height "31px" :justify :center ; :size
                                                                                                    ; "30px"
                 :style blank-block-style]
                [re-com/box :child [pixel-controller :z "zmdi-minus" kp] :align :center :width "33px" :height "31px" :justify
                 :center                                                                            ; :size
                                                                                                    ; "30px"
                 :attr {:on-click z-} :style filled-block-style]]]
              [re-com/h-box :children
               [[re-com/box :child " " :align :center :width "33px" :height "31px" :justify :center ; :size
                                                                                                    ; "30px"
                 :style blank-block-style]
                [re-com/box :child " " :align :center :width "33px" :height "31px" :justify :center ; :size
                                                                                                    ; "30px"
                 :style blank-block-style]
                [re-com/box :child " " :align :center :width "33px" :height "31px" :justify :center ; :size
                                                                                                    ; "30px"
                 :style blank-block-style]
                [re-com/box :child " " :align :center :width "33px" :height "31px" :justify :center ; :size
                                                                                                    ; "30px"
                 :style blank-block-style]
                [re-com/box :child " " :align :center :width "33px" :height "31px" :justify :center ; :size
                                                                                                    ; "30px"
                 :style blank-block-style]
                [re-com/box :child " " :align :center :width "33px" :height "31px" :justify :center ; :size
                                                                                                    ; "30px"
                 :style blank-block-style]
                [re-com/box :child " " :align :center :width "33px" :height "31px" :justify :center ; :size
                                                                                                    ; "30px"
                 :style blank-block-style]]]
              [re-com/h-box :children
               [[re-com/box :child [pixel-controller :w "zmdi-chevron-left" kp] :align :center :width "33px" :height "31px"
                 :justify :center                                                                   ; :size
                                                                                                    ; "30px"
                 :attr {:on-click w-} :style button-block-style]
                [re-com/box :child
                 [re-com/v-box :align :center :justify :center :children
                  [[re-com/box :child (str w)] [re-com/box :child "width" :style {:font-size "8px" :font-weight 400}]]] ;(str w)
                 :align :center :width "33px" :height "31px" :justify :center ; :size "30px"
                 :style blank-block-style]
                [re-com/box :child [pixel-controller :w "zmdi-chevron-right" kp] :align :center :width "33px" :height "31px"
                 :justify :center                                             ; :size "30px"
                 :attr {:on-click w+} :style button-block-style] percent-toggle]]]]]]]]]] editor-box]]))

(defn dropdown-panel
  [kp v view-key type-key]
  (let [values (get (first (remove nil? (for [[k v] (friendly-text)] (when (ordered-keys-in-kp-multi? k kp) v)))) :values)]
    [re-com/box :style {:padding-left "11px"} :align :center :justify :center :child
     [re-com/single-dropdown :choices
      (if (and (vector? values) (map? (first values))) ;; simple vector or a vec map with
        values
        (vec (for [o values] {:id o :label o}))) :model (reagent/atom v) :width "360px" :on-change
      #(ut/tracked-dispatch [::scrubber-view-update kp % view-key type-key])]]))

(defn boolean-panel
  [kp v view-key type-key]
  (let [values [true false]]
    [re-com/h-box :src (at) :gap "3px" :size "auto" :children
     (for [vv values]
       [re-com/box :src (at) :align :center :justify :center :size "auto" :padding "3px" :child (str vv) :attr
        {:on-click #(ut/tracked-dispatch [::scrubber-view-update kp vv view-key type-key])} :style
        {:color            (if (= vv v) "#000000" "#ffffff77")
         :cursor           "pointer"
         :font-size        "12px"
         :font-weight      700
         :background-color (if (= vv v)
                             (theme-pull :theme/editor-outer-rim-color nil)
                             (str (theme-pull :theme/editor-outer-rim-color nil) 33))}])]))

(defn hex-color? [s] (true? (and (string? s) (cstr/starts-with? (str s) "#"))))






(defn scrubbers
  []
  (let [layout-box-maps @(ut/tracked-subscribe [::keypaths-of-layout-panels])
        user-scrubbers  @(ut/tracked-subscribe [::user-scrubbers])
        user-scrubbers  (ut/postwalk-replacer {:button-panel   button-panel
                                               :string?        string?
                                               :vector?        vector?
                                               :integer?       integer?
                                               :dropdown-panel dropdown-panel
                                               :boolean?       boolean?
                                               :boolean-panel  boolean-panel
                                               :int-slider     int-slider
                                               :float?         float?
                                               :text-box       text-box
                                               :keyword?       keyword?
                                               :color-picker   color-picker
                                               :px-int?        #(cstr/ends-with? % "px")
                                               :hex-color?     #(true? (and (string? %) (cstr/starts-with? (str %) "#")))}
                                              user-scrubbers)]
    (merge
      {:select-clause      {:test-fn          vector?
                            :keypath-contains [[:select] [:from] [:where] [:group-by] [:logic]]
                            :renderer-fn      select-clause
                            :renderer-fn-ext  []}
       :layout-box         {:test-fn map? :keypath-contains layout-box-maps :renderer-fn layout-box :renderer-fn-ext []}
       :font-weight-picker {:test-fn integer? :keypath-contains [:font-weight] :renderer-fn button-panel :renderer-fn-ext [100]}
       :button-panel       {:test-fn          string? ;#(or (integer? %) (cstr/ends-with? %
                            :keypath-contains (into (vec (for [[k v] (friendly-text) :when (string? (get-in v [:values 0]))] k))
                                                    [[:font-weight] [:mark :type] [:encoding :x :type] [:encoding :y :type]
                                                     [:aggregate] [:sort] [:encoding :color :type]])
                            :renderer-fn      button-panel
                            :renderer-fn-ext  []}
       :dropdown           {:test-fn          string? ;#(or (integer? %) (cstr/ends-with? %
                            :keypath-contains [[:font-family] [:scheme] [:titleFont] [:monospaced-font] [:base-font]
                                               [:codemirror-theme] [:labelFont] [:legendFont]]
                            :renderer-fn      dropdown-panel
                            :renderer-fn-ext  []}
       :boolean-panel      {:test-fn          boolean? ;#(or (integer? %) (cstr/ends-with? %
                            :keypath-contains []
                            :renderer-fn      boolean-panel
                            :renderer-fn-ext  []}
       :int-slider         {:test-fn integer? :keypath-contains [] :renderer-fn int-slider :renderer-fn-ext []}
       :px-slider          {:test-fn          #(cstr/ends-with? % "px")
                            :keypath-contains []
                            :renderer-fn      int-slider
                            :renderer-fn-ext  [:px]}
       :zfloat-slider      {:test-fn float? :keypath-contains [] :renderer-fn int-slider :renderer-fn-ext [:float]}
       :color-picker       {:test-fn          #(true? (and (string? %) (cstr/starts-with? (str %) "#")))
                            :keypath-contains []
                            :renderer-fn      color-picker
                            :renderer-fn-ext  []}
       :kstr-text-box      {:test-fn keyword? :keypath-contains [] :renderer-fn text-box :renderer-fn-ext []}}
      user-scrubbers)))

(defn get-type
  [kp v]
  (first (sort (remove nil?
                 (for [[k {:keys [test-fn keypath-contains renderer-fn renderer-fn-ext]}] (scrubbers)]
                   (when (and (test-fn v) ;; run the checker fn
                              (if (or (nil? keypath-contains) ;; see if the keys given are an ordered
                                      (not (empty? keypath-contains)))
                                (ordered-keys-in-kp-multi? keypath-contains kp)
                                true)) ;; if empty or nil, ignore this test
                     [k [renderer-fn renderer-fn-ext]]))))))



