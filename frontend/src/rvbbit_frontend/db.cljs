(ns rvbbit-frontend.db
  (:require ;[rvbbit-frontend.http :as http]
   [reagent.core :as reagent]
   ;[websocket-fx.core :as wfx]
   ))

(defonce search-box (reagent/atom nil))
(defonce context-modal-pos (reagent/atom [0 0]))
(defonce editor-mode (reagent/atom :meta))
(defonce item-browser-mode (reagent/atom :queries))
(defonce auto-refresh? (reagent/atom false))
(defonce param-filter (reagent/atom {;:clicked true
                                     :this-tab? false
                                     :user true
                                     :theme false
                                     :condis false}))
(def pause-alerts (reagent/atom false))
(def speech-log (reagent/atom []))
(def last-mouse-activity (atom nil))
(def snap-to-grid 25)

(def running-blocks (reagent/atom {}))
(def real-running-blocks (reagent/atom {}))

(def pixel-pusher-hover (reagent/atom nil))
(def pixel-pusher-key (reagent/atom nil))
(def sniff-deck (reagent/atom {}))
(def context-box (reagent/atom {}))

(defonce bad-form? (reagent/atom false))
(defonce bad-form-msg (reagent/atom nil))

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

(defonce flow-results (reagent/atom {}))

(def active-tmp-history (reagent/atom nil))

(defonce based [-785 -862 0.925])
(defonce pan-zoom-offsets (reagent/atom based))
(def panning? (reagent/atom false))
(def zooming? (reagent/atom false))
(def zoomer-state (reagent/atom nil))
;(defonce context-drop-pos (reagent/atom [0 0]))

(defonce flow-detached-coords
  (reagent/atom (let [hh (.-innerHeight js/window) ;; starting size set on load
                      ww (.-innerWidth js/window) ;; starting size set on load
                      topper (* hh 0.06)
                      lefty (* ww 0.06)]
                  [lefty topper])))


(def data-colors {"string"        "#5dc963" ; "#abdc32" ; "#feb733"
                  "boolean"       "#f386bf"
                  "integer"       "#d6e367" ; "#28ae80" ;"#50a978"
                  "nil"           "#ff2147"
                  "rabbit-code"   "#FC0FC0"
                  "rabbitsql"     "#FC0FC0"
                  "unknown"       "#c7005d"
                  "float"         "#2c728e" ;"#77ffd1"
                  "date"          "#3b528b" ; "#66607d" 
                  "datetime"      "#472d7b" ; "#66607d"
                  "map"           "#00FFFF"
                  "keyword"       "#add8e6"
                  "any"           "#ffffff"
                  "list"          "#c32148"
                  "vector"        "#008080"})

(def old-theme {:editor-rim-color "#41307f"
                :editor-font-color "#ffffff"
                :editor-background-color "#000000"
                :editor-param-background-color "#0b1122"
                :editor-grid-font-color "#ffffff"
                :editor-grid-selected-background-color "#8879bc" ;"#bc798c"
                :block-title-font-color "#ffffff"
                :block-title-selected-font-color "#ffffff"
                :block-tab-selected-font-color "#FFA50087"
                 ;:block-tab-font-color "#ffffff50" ;; we use title color instead, lets not be too redundant
                :monospaced-font "Fira Code"
                 ;:grid-selected-column-css {:background "repeating-linear-gradient(45deg, #606dbc55, #606dbc55 10px, #46529855 10px, #46529855 20px"}
                :grid-selected-column-css {:background-color "#00000088" :filter "brightness(200%)"}
                :grid-selected-background-color "#8879bc" ;"#bc798c"
                :editor-grid-selected-font-color "#000000"
                :grid-selected-font-color "#000000"
                :grid-font-color "#ffffff"
                :base-block-color "#0b1122"
                :codemirror-theme "ayu-mirage"
                :base-font "Alata"
                :canvas-background-css {:background-image "url(images/fake-brick.png)"
                                        :background-repeat "repeat"
                                        :background-color "#47555e"}
                :base-block-color-selected "#0b031b"
                :editor-outer-rim-color "#a891ff"
                 ;:vega-default-color-scheme  {:scheme "inferno"}
                :vega-default-color-scheme {:scheme "magma"}
                :vega-defaults {:view {:stroke "transparent"}
                                :axis {:domainColor "#ffffff22"
                                       :grid true
                                       :labelColor "#ffffff88"
                                       :titleFont "Lato"
                                       :axisFont "Lato"
                                       :font "Lato"
                                       :titleColor "#ffffff99"
                                       :labelFont "Lato"
                                       :domain false
                                       :gridColor "#ffffff22"
                                       :tickColor "#ffffff22"}
                                :legend {:labelFont "Lato" :legendFont "Lato" :labelColor "#ffffff99"
                                         :titleColor "#ffffff99" :stroke "#ffffff99" :titleFont "Lato"}
                                :header {:labelFont "Lato" :titleFont "Lato" :titleColor "#ffffff99"}
                                :mark {:font "Lato"}
                                :title {:font "Lato" :subtitleFont "Lato" :labelFont "Lato"
                                        :titleFont "Lato" :titleColor "#ffffff99"}}})

(def base-theme (merge old-theme ;; temp
                       {:theme-name "who ya gonna call?"
                        :codemirror-theme "ayu-mirage"
                        :editor-param-background-color "#0b1122"
                        :vega-defaults {:view {:stroke "transparent"}
                                        :axis {:domainColor "#ffffff22"
                                               :grid true
                                               :font "Lato"
                                               :labelColor "#ffffff88"
                                               :titleFont "Lato"
                                               :titleColor "#ffffff99"
                                               :labelFont "Lato"
                                               :domain false
                                               :gridColor "#ffffff22"
                                               :tickColor "#ffffff22"
                                               :axisFont "Lato"}
                                        :legend {:labelFont "Lato"
                                                 :legendFont "Lato"
                                                 :labelColor "#ffffff99"
                                                 :titleColor "#ffffff99"
                                                 :stroke "#ffffff99"
                                                 :titleFont "Lato"}
                                        :header {:labelFont "Lato"
                                                 :titleFont "Lato"
                                                 :titleColor "#ffffff99"}
                                        :mark {:font "Lato"}
                                        :title {:font "Lato"
                                                :subtitleFont "Lato"
                                                :labelFont "Lato"
                                                :titleFont "Lato"
                                                :titleColor "#ffffff99"}}
                        :grid-selected-column-css {:background-color "#00000088"
                                                   :filter "brightness(200%)"}
                        :base-block-color-selected "#0b031b"
                        :block-title-font-color "#ffffff"
                        :base-block-color "#0b1122"
                        :editor-rim-color "#0b1122"
                        :vega-default-color-scheme {:scheme "accent"}
                        :editor-grid-selected-font-color "#ffffff"
                        :monospaced-font "IBM Plex Mono"
                        :editor-grid-selected-background-color "#FFA50087"
                        :data-colors data-colors
                        :base-block-style {}
                        :nivo-defaults
                        {:font-family :theme/base-font
                         :labels {:text {:fill "#ffffff"
                                         :font-size "16px"
                                         :font-family :theme/base-font
                                         :font-weight 700}}
                         :tooltip {:container {:background "#000"
                                               :color "#ffffff"
                                               :text {:fill "#eeeeee"}
                                               :fontSize "18px"
                                               :borderRadius "4px"
                                               :boxShadow
                                               "0 1px 2px rgba(0, 0, 0, 0.55)"
                                               :padding "5px 9px"}
                                   :basic {:whiteSpace "pre"
                                           :display "flex"
                                           :alignItems "center"}
                                   :tableCell {:padding "3px 5px"}}
                         :axis {:legend {:text {:fill "#ffffff"
                                                :font-size "14px"
                                                :font-family :theme/base-font
                                                :font-weight 700}}
                                :ticks {:line {:stroke "#ffffff60"}
                                        :text {:fill "#ffffff60"
                                               :font-weight 700}}}
                         :grid {:line {:stroke "#ffffff"
                                       :strokeWidth 0
                                       :strokeDasharray "0 0"}}}
                        :editor-outer-rim-color "#33ffb7"
                        :grid-selected-font-color "#ffffff"
                        :block-title-selected-font-color "#ffffff"
                        :grid-selected-background-color "#8879bc"
                        :grid-font-color "#ffffff"
                        :canvas-background-css {:background-image "url(images/fake-brick5.png)"
                                                :transition "all 0.6s ease-in-out"
                                                :background-color "#47555e"
                                                :background-repeat "repeat"}
                        :editor-font-color "#ffffff"
                        :base-font "Oxygen Mono"
                        :block-tab-selected-font-color "#FFA500"
                        :editor-background-color "#000000"
                        :editor-grid-font-color "#ffffff"}))

(def default-db
  {:reco-preview nil
   :editor? false
   :buffy? false
   :flow-editor? true
   :grid-recos? true
   :openai-api-key     "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"
   :openai-org-id      "org-YLngjih2M4oju0tnnZtKCAYg"
   :elevenlabs-api-key "f74e20dec69741c2d51663cbd8cd4cf6"
   :selected-flow "live-scratch-flow"
   :selected-cols {}
   :recos-page 0
   :recos-page2 0
   :selected-tab "strategic grill locations"
   :tabs ["strategic grill locations"]
   :click-param {;:conn-list {:connection_id "-1245280001", :database_name "Microsoft SQL Server", :database_version "15.00.4261"}
                 :theme base-theme ;; start off default with base values 
                 ;:user-params {:user-param1 "Heya!" :truth? true :orange "#FFA500"} ;; ?
} ;; temp
   :panels {;;     :tester123 {:h 2 
        ;;                 :w 40
        ;;                 :root [0 0]
        ;;                 :name "database connections analyzed"
        ;;                 :views [:v-box
        ;;                         ;:padding "3px"
        ;;                        ;:style {:background-color "#ffffff77"}
        ;;                         :children
        ;;                         [[:h-box
        ;;                           :gap "6px"
        ;;                           ;:height "20px"
        ;;                           :size "auto"
        ;;                           :children :conn-list-boxes-values]
        ;;                          [:box
        ;;                           :align :center :justify :center
        ;;                           :child :conn-list-parameter]]]
        ;;                 ;:boxes-values-style {:border "1px solid #ffffff55" :border-radius "10px" :background-color "#ffffff55" :font-size "11px"}
        ;;                         ;:boxes-values-style-selected {}
        ;;                         ;:boxes-style :text-style :values-style :table-style 
        ;;                 :clickable? true
        ;;                 :queries {:conn-list {:select-distinct [:connection_id :database_name :database_version]
        ;;                                       :from [:connections]}}
        ;;                 ;:connection-id []
        ;;                 }
        ;;     :ufo-test {:h 8
        ;;                :w 6
        ;;                :connection-id "429999852"
        ;;                :name "drag-from-blick-blok"
        ;;                :views [:box :child :shape-drag0-magic]
        ;;                :queries
        ;;                {:shape-drag0
        ;;                 {:select [:shape [[:count 1] :rowcnt]]
        ;;                  :from [{:select :* :from [:ufo_sightings]}]
        ;;                  :group-by [:shape]
        ;;                  :order-by [[:rowcnt :desc]]}}
        ;;                :root [44 3]}
        ;;     :test-bigfoot {:h 6
        ;;                    :w 11
        ;;                    :connection-id "429999852"
        ;;                    :name "drag-from-blick-blok"
        ;;                    :views [:box :child :city-drag28-magic]
        ;;                    :queries {:city-drag28
        ;;                              {:select :*
        ;;                               :from [:bigfoot_sightings_locations]}}
        ;;                    :root [39 17]}
        ;;     :vega-lite-test2 {:h 8
        ;;                       :w 14
        ;;                       :root [25 3]
        ;;                       :name "vega-lite-test 2"
        ;;                       :views
        ;;                       [:vega-lite
        ;;                        {:encoding
        ;;                         {:y {:field :table_name :type "ordinal"}
        ;;                          :x {:aggregate "sum"
        ;;                              :field :entries
        ;;                              :title
        ;;                              :conn-list-parameter/database_name
        ;;                              :type "quantitative"}
        ;;                          :color {:scale {:scheme "viridis"}
        ;;                                  :field :table_name
        ;;                                  :type "ordinal"
        ;;                                  :legend {:titleColor "#ffffff88"
        ;;                                           :labelColor "#ffffff88"}}}
        ;;                         :config {:axis {:labelColor "#ffffff88"
        ;;                                         :tickColor "#ffffff22"
        ;;                                         :titleColor "#ffffff998"
        ;;                                         :domainColor "#ffffff22"
        ;;                                         :gridColor "#ffffff22"}}
        ;;                         :mark "bar"
        ;;                         :width "container"
        ;;                         :background "#ffffff00"
        ;;                         :font "Lato"
        ;;                         :height :height-int
        ;;                         :data {:values :sample-viz3}}]
        ;;                       ;:connection-id []
        ;;                       :queries
        ;;                       {:sample-viz3
        ;;                        {:select [[[:count 1] :entries] :table_name]
        ;;                         :from [:tests]
        ;;                         :where [:= :connection_id
        ;;                                 :conn-list-parameter/connection_id]
        ;;                         :group-by [:table_name]}}}
        ;;     :vega-lite-test {:h 8
        ;;                      :w 10
        ;;                      :root [14 3]
        ;;                      :name "vega-lite-test"
        ;;                      :views2 [:box :child :sample-viz-table]
        ;;                      :views
        ;;                      [:vega-lite
        ;;                       {:encoding
        ;;                        {:y {:field :data_type
        ;;                             :type "ordinal"
        ;;                             :title "row count"}
        ;;                         :x {:aggregate "sum"
        ;;                             :field :entries
        ;;                             :title
        ;;                             :conn-list-parameter/database_name
        ;;                             :type "quantitative"}
        ;;                         :color {:scale {:scheme "viridis"}
        ;;                                 :legend {:titleColor "#ffffff88"
        ;;                                          :labelColor "#ffffff88"}
        ;;                                 :field :data_type
        ;;                                 :type "ordinal"}}
        ;;                        :config {:axis {:labelColor "#ffffff88"
        ;;                                        :tickColor "#ffffff22"
        ;;                                        :titleColor "#ffffff998"
        ;;                                        :domainColor "#ffffff22"
        ;;                                        :gridColor "#ffffff22"}}
        ;;                        :mark "bar"
        ;;                        :background "#ffffff00"
        ;;                        :font "Lato"
        ;;                        :height :height-int
        ;;                        :width "container"
        ;;                        :data {:values :sample-viz2}}]
        ;;                      :connection-id []
        ;;                      :queries
        ;;                      {:sample-viz2
        ;;                       {:select [[[:count 1] :entries] :data_type]
        ;;                        :from [:tests]
        ;;                        :where [:= :connection_id
        ;;                                :conn-list-parameter/connection_id]
        ;;                        :group-by [:data_type]}}}
        ;;     :tester1232222 {:h 0 :w 4
        ;;                     :root [0 2]
        ;;                     :name "tables"
        ;;                     :clickable? true
        ;;                     :views [:v-box :children :tables-boxes :gap "8px" :style {:padding-top "10px"}]
        ;;                     :boxes-style {:border "1px solid #ffffff55" :border-radius "10px" :background-color "#ffffff55" :font-size "11px"}
        ;;                     :queries {:tables {:select-distinct [:table_name :context_hash :table_type]
        ;;                                        :from [:fields]
        ;;                                        :where [:= :connection_id :conn-list-parameter/connection_id]
        ;;                                        :order-by [:table_name]
        ;;                                        :limit 6}}
        ;;                     ;:connection-id []
        ;;                     }

        ;;     :tester123333 {:h 4
        ;;                    :w 8
        ;;                    :root [5 3]
        ;;                    :name "ui1"
        ;;                    :views [:v-box :children
        ;;                            [[:box :style
        ;;                              {:font-size "33px"
        ;;                               :color "#ffffff"
        ;;                               :font-weight 700} :align :center :justify
        ;;                              :center :child
        ;;                              :conn-list-parameter/database_name]
        ;;                             [:box :style
        ;;                              {:font-size "28px"
        ;;                               :color "#ffffff"
        ;;                               :font-weight 700} :align :center :justify
        ;;                              :center :child
        ;;                              :conn-list-parameter/database_version]
        ;;                             [:box :style
        ;;                              {:font-size "28px"
        ;;                               :color "#ffffff"
        ;;                               :font-weight 700} :align :center :justify
        ;;                              :center :child
        ;;                              :conn-list-parameter/connection_id]]]
        ;;                    :queries {}
        ;;                    ;:connection-id []
        ;;                    }

        ;;     :hello-there-brother {:h 3
        ;;                           :w 12
        ;;                           :root [2 2]
        ;;                           :name "hello there!"
        ;;                           :tab "strategic grill locations"
        ;;                           :views {:heya! [:box :align :center :justify :center :style
        ;;                                           {:font-size "50px"
        ;;                                            :font-weight 700
        ;;                                            :color :theme/editor-outer-rim-color
        ;;                                            :padding-top "14px"
        ;;                                            :opacity 1
        ;;                                            :font-family :theme/base-font} :child
        ;;                                           "hello! 🐇 🍂 🦃"]}
        ;;                           :queries {}}

        ;;     :field-list {:h 18 :w 9
        ;;                  :root [28 6]
        ;;                  :name "field list"
        ;;                  :views [:v-box :children :fields-list-boxes :gap "8px" :style {:padding-top "10px"}]
        ;;                  :boxes-style {:border "1px solid #ffffff55" :border-radius "10px" :background-color "#ffffff55" :font-size "11px"}
        ;;                  :queries {:fields-list {:select-distinct [:field_name :data_type]
        ;;                                          :from [:fields]
        ;;                                          :where [:= :connection_id :conn-list-parameter/connection_id]
        ;;                                          :order-by [:table_name]
        ;;                                          :limit 36}}
        ;;                  ;:connection-id []
        ;;                  }

        ;;     :tester12355 {:h 11
        ;;                   :w 35
        ;;                   :root [5 14]
        ;;                   :name "blick-blok"
        ;;                   :views [:box :child :test-grid-magic]
        ;;                   :queries {:test-grid {:select :* :from [:fields] :where [:= :connection_id :conn-list-parameter/connection_id] :order-by [:table_name] :limit 100}}
        ;;                   ;:connection-id []
        ;;                   }

        ;;     :tester12366 {:h 3
        ;;                   :w 24
        ;;                   :root [5 10]
        ;;                   :name "block-6757"
        ;;                   :views [:box :child :test-grid-parameter]
        ;;                   :queries {}
        ;;                   ;:connection-id []
        ;;                   }
            }
   :window {:w (.-innerWidth js/window)
            :h (.-innerHeight js/window)}})
