(ns rvbbit-frontend.ai-workers
  (:require
   ["codemirror/addon/edit/closebrackets.js"]
   ["codemirror/addon/edit/matchbrackets.js"]
   ["codemirror/mode/clojure/clojure.js"]
   ["codemirror/mode/sql/sql.js"]
   ["react-codemirror2"     :as cm]
   ["react-drag-and-drop"   :as rdnd]
   [cljs-drag-n-drop.core   :as dnd2]
   [cljs.tools.reader       :refer [read-string]]
   [cljs.core.async :refer [go timeout <!]]
   [clojure.data            :as cdata]
   [clojure.edn             :as edn]
   [clojure.set             :as cset]
   [clojure.string          :as cstr]
   [clojure.walk            :as walk]
   [day8.re-frame.undo      :as    undo
    :refer [undoable]]
   [goog.dom                :as gdom]
   [goog.events             :as gevents]
    ;[re-catch.core           :as rc]
   [re-com.core             :as    re-com
    :refer [at]]
   [re-com.util             :refer [px]]
   [re-frame.core           :as re-frame]
   [reagent.core            :as reagent]
   [rvbbit-frontend.audio   :as audio]
   [rvbbit-frontend.vbunny   :as vbunny]
   [rvbbit-frontend.bricks  :as    bricks
    :refer [theme-pull]]
   [rvbbit-frontend.connections :as conn]
   [rvbbit-frontend.connections :refer [sql-data]]
   [rvbbit-frontend.db      :as db]
   [rvbbit-frontend.resolver :as resolver]
   [rvbbit-frontend.subs    :as subs]
   [rvbbit-frontend.http    :as http]
   [rvbbit-frontend.utility :as ut]
   [websocket-fx.core       :as wfx])
  (:import
   [goog.events EventType]
   [goog.async  Debouncer]))

(defonce kit-pages (reagent/atom {}))
(defonce kit-mutations (reagent/atom {}))

(defonce detached-coords
  (reagent/atom (let [hh          (.-innerHeight js/window) ;; starting size set on load
                      ww          (.-innerWidth js/window) ;; starting size set on load
                      bricks-wide (+ (js/Math.floor (/ ww db/brick-size)) 1)
                      topper      (* 2 db/brick-size)
                      lefty       (* (- bricks-wide 15) db/brick-size)]
                  [lefty topper])))

(defn mouse-move-handler
  [offset]
  (fn [evt]
    (let [start-x (.-clientX evt)
          start-y (.-clientY evt)
          off-x   (:x offset)
          off-y   (:y offset)
          x       (- start-x off-x)
          y       (- start-y off-y)]
      (reset! detached-coords [x y]))))

(defn mouse-up-handler
  [on-move]
  (fn me [evt]
    (reset! bricks/dragging-editor? false)
    (do (gevents/unlisten js/window EventType.MOUSEMOVE on-move))))

(defn mouse-down-handler
  [e]
  (let [{:keys [left top]} (bricks/get-client-rect e)
        offset             {:x (- (.-clientX e) left) :y (- (.-clientY e) top)}
        on-move            (mouse-move-handler offset)]
    (reset! bricks/dragging-editor? true)
    (do (gevents/listen js/window EventType.MOUSEMOVE on-move))
    (gevents/listen js/window EventType.MOUSEUP (mouse-up-handler on-move))))

(defn scroll-div-to-bottom
  [div]
  (let [scrollHeight (.-scrollHeight div)
        offsetHeight (.-offsetHeight div)]
    (set! (.-scrollTop div) (- scrollHeight offsetHeight))))

(defn scroll-to-bottom [id] (let [element (gdom/getElement id)] (scroll-div-to-bottom element)))


(defn scroll-to-element
  [container-id element-id]
  (let [container     (gdom/getElement container-id)
        element       (gdom/getElement element-id)
        container-top (.-scrollTop container)
        element-top   (.-offsetTop element)]
    (set! (.-scrollTop container) (- element-top container-top))))

(defn smooth-scroll-to-element
  [container-id element-id]
  (try (let [container     (gdom/getElement container-id)
             element       (or (gdom/getElement element-id) "") ;; blocks some weird js crash with
             container-top (.-scrollTop container)
             element-top   (- (.-offsetTop element) 40) ;; 40 is the height of the header, so
             start         (atom nil)
             duration      500]
         (letfn [(step [timestamp]
                   (when (nil? @start) (reset! start timestamp))
                   (let [progress       (/ (- timestamp @start) duration)
                         new-scroll-top (+ container-top (* progress (- element-top container-top)))]
                     (set! (.-scrollTop container) new-scroll-top)
                     (when (< progress 1) (.requestAnimationFrame js/window step))))]
           (.requestAnimationFrame js/window step)))
       (catch :default _ nil)))

(defn smooth-scroll-to-bottom
  [container-id element-id]
  (let [container     (gdom/getElement container-id)
        element       (gdom/getElement element-id)
        container-top (.-scrollTop container)
        element-top   (+ (.-offsetTop element) (.-offsetHeight element)) ;; scroll to bottom
        start         (atom nil)
        duration      500]
    (letfn [(step [timestamp]
              (when (nil? @start) (reset! start timestamp))
              (let [progress       (/ (- timestamp @start) duration)
                    new-scroll-top (+ container-top (* progress (- element-top container-top)))]
                (set! (.-scrollTop container) new-scroll-top)
                (when (< progress 1) (.requestAnimationFrame js/window step))))]
      (.requestAnimationFrame js/window step))))



(defn render-honey-comb-fragments
  [c & [w h sys-name]] ;; TODO REPLACE WITH bricks/clover-fragments
  (let [;panel-key :virtual-panel ;:block-4752 ;:hello-there-brother
        [panel-key key]       [(keyword (str "prando" (rand-int 123))) (keyword (str "vrando" (rand-int 123)))]
        panel-key (if (cstr/ends-with? (str sys-name) "-sys*") :virtual-panel panel-key)
        ;key       :virtual-view ;:view ;:ufo-country ;:heya!
        type      (cond (vector? c)                         :view
                        (string? c)                         :view
                        (and (map? c) (nil? (get c :view))) :query
                        :else                               :both)
        data_d    c]
    ;;(ut/tapp>> [:frag type data_d sys-name w h])
    (cond (= type :view)  (let [view {key data_d}
                                w (or w 11)
                                h (or h 9)]
                            [bricks/clover panel-key key w h view nil])
          (= type :query) (let [temp-key (get data_d :_query-id (keyword (if sys-name (str sys-name) (str "kick-" (hash c)))))
                                query    {temp-key (-> data_d ;(get data_d :queries)
                                                       (dissoc :cache?)
                                                       (dissoc :refresh-every))}
                                h        (get data_d :_h (or h 6))
                                w        (get data_d :_w (or w 10))]
                            [re-com/box :size "none" :width (px (* w db/brick-size))
                             :height (px (- (* h db/brick-size) 30))
                             :child [bricks/clover panel-key temp-key h w nil query]])
          (= type :both)  (let [queries (get data_d :queries)
                                qkeys   (into {}
                                              (for [q (keys queries)]
                                                {q (keyword (if sys-name
                                                              (str (ut/replacer (str q) #":" "") "-" sys-name)
                                                              (str (ut/replacer (str q) #":" "") "-kick-" (hash data_d))))}))
                                ndata   (ut/postwalk-replacer qkeys data_d)
                                h       (get data_d :_h (or h 11))
                                w       (get data_d :_w (or w 9))]
                            [bricks/clover panel-key key ;:view ;(get data_d :selected-view)
                             h w {key (get ndata :view)} ;views ;(ut/postwalk-replacer qkeys views)
                             (get ndata :queries) ;(ut/postwalk-replacer qkeys queries)
                             ])
          :else           [bricks/clover panel-key key 11 9])))

(re-frame/reg-sub
 ::kit-run-waiting?
 (fn [_ {:keys [panel-key data-key]}]
   (let [running-key (get @db/kit-fn-lookup [panel-key data-key])
         running? @(ut/tracked-sub ::conn/clicked-parameter-key-alpha {:keypath [running-key]})]
     running?)))

(re-frame/reg-sub
 ::kit-console-incremental
 (fn [_ {:keys [panel-key data-key]}]
   (let [running-key (get @db/kit-fn-lookup [panel-key data-key])
         console-key (keyword (-> (str running-key) (cstr/replace ">running?" ">incremental") (cstr/replace "-status" "") (cstr/replace ":" "")))
         output @(ut/tracked-sub ::conn/clicked-parameter-key-alpha {:keypath [console-key]})]
     output)))

(defn start-conversation [client-name assistant-name]
  (ut/tracked-dispatch [::wfx/push   :default
                        {:kind       :start-conversation
                         :assistant-name assistant-name
                         :client-name client-name}])
  (ut/dispatch-delay 800 [::http/insert-alert (str "Starting conversation with " assistant-name "...") 12 1 5]))

(defn sonnet-cost [input output]
  ;$3.00 per 1M input tokens
  ;$15.00 per 1M output tokens
  (let [input-cost (* 3.00 (/ input 1000000))
        output-cost (* 15.00 (/ output 1000000))]
    (+ input-cost output-cost)))

(defn get-threads-for-assistant [assistant]
  (let [client-name @(ut/tracked-sub ::bricks/client-name {})
        client-name-str (cstr/replace (str client-name) ":" "")
        threads-key (keyword (str "ai-worker/threads-for>" client-name-str))
        threads @(ut/tracked-sub ::conn/clicked-parameter-key-alpha {:keypath [threads-key]})]
    (filterv #(= (first %) assistant) threads)))

(defn get-thread-name [assistant thread-id]
  (let [threads (get-threads-for-assistant assistant)]
    (get-in (filterv #(= (last %) thread-id) threads) [0 1] nil)))

(defn context-metadata []
  (let [client-name @(ut/tracked-sub ::bricks/client-name {})
        curr-tab @(ut/tracked-sub ::bricks/selected-tab {})
        [runner-src data-key] @(ut/tracked-sub ::bricks/editor-panel-selected-view {})
        coords @(ut/tracked-sub ::bricks/all-roots-tab-sizes {:tab curr-tab})
        selected-block    @(ut/tracked-sub ::bricks/selected-block {})
        no-selected?      (or (nil? selected-block) (= selected-block "none!"))
        context-body      (pr-str @(ut/tracked-sub ::bricks/get-block-data {:panel-key selected-block}))
        hh @(ut/tracked-subscribe_ [::subs/h])
        ww @(ut/tracked-subscribe_ [::subs/w])
        mdata @(ut/tracked-sub ::conn/sql-metadata-alpha {:keypath [data-key]})
        canvas-w (/ ww db/brick-size)
        canvas-h (/ hh db/brick-size)]
    (merge
     {:client-name client-name
      ;:runner (if no-selected? :clojure (or runner-src :views))
      :metadata (if (= runner-src :queries)
                  {:connection-id @(ut/tracked-subscribe [::bricks/lookup-connection-id-by-query-key data-key])
                   :database-type (get mdata :database-type "")
                   :fields (get mdata :fields)} {})
      :canvas-coords coords
      :canvas-size [canvas-w canvas-h]}
      ;:id (cstr/replace (str client-name "++" selected-block "++" runner-src "++" data-key) ":" "")
     (when (not no-selected?)
       {:context {[:panels selected-block] context-body}}))))
  
(re-frame/reg-event-db
 ::delete-thread-local
 (fn [db [_ thread-id worker-name client-name]]
   (let [client-name-str (cstr/replace (str client-name) ":" "")
         threads-for-key [:click-param :ai-worker (keyword (str "threads-for>" client-name-str))]
         threads-for (get-in db threads-for-key)
         threads-for (filterv #(not= (last %) thread-id) threads-for)
         thread-key [:click-param :ai-worker (keyword (str worker-name ">" thread-id ">last"))]
         thread-key-running [:click-param :ai-worker (keyword (str worker-name ">" thread-id ">running?"))]]
     (-> db
         (ut/dissoc-in thread-key)
         (ut/dissoc-in thread-key-running)
         (assoc-in threads-for-key threads-for)))))

(defn worker-start-panel [panel-height]
  (let [workers-map @(ut/tracked-sub ::conn/clicked-parameter-key-alpha {:keypath [:ai-worker/config>client]})
        limit 15
        usage 4.3777
        client-name @(ut/tracked-sub ::bricks/client-name {})
        client-name-str (cstr/replace (str client-name) ":" "")
        threads-key (keyword (str "ai-worker/threads-for>" client-name-str))
        all-threads @(ut/tracked-sub ::conn/clicked-parameter-key-alpha {:keypath [threads-key]}) ;; for reaction purposes only. data is small.
        costs @(ut/tracked-sub ::conn/clicked-parameter-key-alpha {:keypath [:ai-worker/costs]})
        ttl-costs (apply + (for [v costs] (get v :total_cost 0)))
        ttl-costs (-> (or ttl-costs 0)
                      (js/Number.)
                      (.toFixed 5)
                      (js/parseFloat))
        costs-ass (group-by :assistant_name costs) 
        panel-height (- panel-height 70)
       ;; _ (ut/tapp>> [:workers-map workers-map])
        ;usage-pct (Math/ceil (* (/ usage limit) 100))  
        ]
    [re-com/v-box
     ;:justify :between
     :children
     [[re-com/box
       ;:size "auto" 
       :height "33px"
       :padding "5px"
       ;:style {:border "1px solid red"}
       :child [re-com/h-box
               :size "auto"
               :justify :between
               
               ;:gap "10px"
               :children
               [
                ;; [re-com/md-icon-button :src (at)
                ;;  :md-icon-name "zmdi-circle"
                ;;  :style
                ;;  {:color      "#dddd77" ;(theme-pull :theme/editor-outer-rim-color nil)
                ;;   :cursor     "pointer"
                ;;   :margin-top "-2px"
                ;;   :font-size  "16px"}]
                [re-com/box 
                 :style {:font-size "18px" :margin-top "3px" :margin-left "10px" :opacity 0.33}
                 :child (str "cumulative total: $" ttl-costs)]
                ;; [re-com/progress-bar
                ;;  :striped? true
                ;;  :bar-style {:color            (str (ut/choose-text-color (theme-pull :theme/editor-outer-rim-color nil)) 67)
                ;;              :outline          "none"
                ;;              :border           "none"
                ;;              :background-color (theme-pull :theme/editor-outer-rim-color nil)}
                ;;  :model usage-pct
                ;;  :width "230px"]
                ;[re-com/gap :size "5px"]
                ]]]
      [re-com/box
       :size "auto"
       :height (px panel-height)
       :style {:overflow-y "auto"
               ;:border "1px solid green"
               }
       :child [re-com/v-box
               :padding "8px"
               :size "auto" ;:height "100%"
               :children
               [[re-com/gap :size "10px"]
                [re-com/v-box
                 :gap "15px"
                 :children (for [[worker-name {:keys [name-style description image elevenlabs-voice model platform]}] workers-map
                                 :let [threads (get-threads-for-assistant worker-name)
                                       worker-cost (-> (get-in costs-ass [worker-name 0 :total_cost] 0)
                                                       (js/Number.)
                                                       (.toFixed 5)
                                                       (js/parseFloat))]]
                             [re-com/v-box 
                              :padding "6px"
                              :style {:border-radius "12px"
                                      :backdrop-filter "blur(12px)"
                                      :background-color (str (get name-style :color "#000000") 20)
                                      :border (str "2px solid " (get name-style :color (theme-pull :theme/editor-outer-rim-color nil)))}
                              :children 
                              [[re-com/h-box
                                ;:style {:border "1px solid cyan"}
                                :padding "6px"
                                :gap "2px"
                                :justify :between
                                :children [[re-com/v-box
                                            :children
                                            [[re-com/h-box 
                                              ;:justify :between
                                              :gap "10px"
                                              :children [[re-com/box :child (str worker-name)]
                                                         [re-com/md-icon-button 
                                                          :md-icon-name "ri-chat-thread-fill"
                                                          :on-click #(start-conversation client-name worker-name)
                                                          :style {:font-size "25px" :opacity 0.25 :cursor "pointer"
                                                                  :margin-top "5px"}]
                                                         ;;[re-com/box :child (str worker-name)]
                                                         ] 
                                              :style (merge 
                                                      (assoc name-style :font-size "28px")
                                                      {:text-shadow "4px 4px 4px #00000099"
                                                       :font-weight 700})]
                                             [re-com/box 
                                              :width "410px"
                                              :style {;:border "1px solid lime" 
                                                      :opacity 0.9
                                                      ;:font-weight 700
                                                      :font-size "15px"}
                                              :child (str description)]]]
                                           (when image
                                             [:img {:src image
                                                    :style {:border-radius "12px"}
                                                    :height "100px"
                                                    :width "100px"}])]]
                               
                               (when (ut/ne? threads)
                                 [re-com/v-box 
                                  :padding "6px"
                                  :style {:padding-left "20px"}
                                  :children (for [[_ summy t] threads
                                                  :let [thread-key (keyword (str "ai-worker/" worker-name ">" t ">last"))
                                                        char-limit 66
                                                        summy (when summy (if (<= (count summy) char-limit) summy (str (subs summy 0 char-limit) "...")))
                                                        thread-key-running (keyword (str "ai-worker/" worker-name ">" t ">running?"))
                                                        running? @(ut/tracked-sub ::conn/clicked-parameter-key-alpha {:keypath [thread-key-running]})
                                                        thread @(ut/tracked-sub ::conn/clicked-parameter-key-alpha {:keypath [thread-key]})
                                                        msg-count (count thread)]] 
                                              [re-com/h-box 
                                               :style {:border "1px solid red"
                                                       :border-radius "6px"}
                                               :padding "4px"
                                               :justify :between
                                               :children [[re-com/md-icon-button :md-icon-name "zmdi-close"
                                                           :on-click #(do 
                                                                        (ut/tracked-dispatch [::wfx/push   :default
                                                                                            {:kind       :delete-thread
                                                                                             :thread-id  t
                                                                                             :assistant-name worker-name
                                                                                             :client-name client-name}])
                                                                        (ut/tracked-dispatch [::delete-thread-local t worker-name client-name]))
                                                           :style {:font-size "15px" :opacity 0.33 :cursor "pointer"}]

                                                          [re-com/box
                                                           :style {:cursor "pointer"}
                                                           :attr {:on-click #(do
                                                                               (ut/tracked-dispatch [::select-ai-worker worker-name t]))}
                                                           :child (if running? "**running**" (str (or summy t)))]
                                                          [re-com/box
                                                           :child (str msg-count)]]])])
                               
                               [re-com/h-box
                                :justify :between
                                :style {:font-size "13px" :opacity 0.6}
                                :padding "6px"
                                :children
                                [[re-com/box :child (str platform " / " model " ($" (or worker-cost 0.0) ")" )]
                                 [re-com/md-icon-button :md-icon-name "zmdi-play"
                                  :on-click (fn [] (ut/tracked-dispatch [::audio/text-to-speech11 :audio :speak "Yo. Ready to get this party started?" false elevenlabs-voice]))
                                  :style {:font-size "20px" :opacity 0.25 :cursor "pointer" :margin-top "-5px"}]
                                 ]]]])]]]]]]))

;; (defn speak-seq [text]
;;   (when (not (some #(= % text) @db/speech-log))
;;     (swap! db/speech-log conj text)
;;     ;;(ut/dispatch-delay 800 [::http/insert-alert (str text) 13 1 5])
;;      ;;  (ut/tracked-dispatch [::audio/text-to-speech11 panel-key ;:audio
;;      ;;                        :speak (str text)])
;;     (audio/dispatch-serial [::audio/text-to-speech11 :audio
;;                             :speak (str text) false nil (swap! db/seq-atom inc)])))

(defn speak-seq [text]
  (when (not (some #(= % text) @db/speech-log))
    (swap! db/speech-log conj text)
    (go
      (<! (timeout 800))  ; Non-blocking delay of 800ms
      (audio/dispatch-serial [::audio/text-to-speech11 :audio
                              :speak (str text) false nil (swap! db/seq-atom inc)]))))

;; (defn speak-seq [line]
;;   (js/Promise.
;;    (fn [resolve _reject]
;;      (when (not (some #(= % line) @db/speech-log))
;;        (swap! db/speech-log conj line)
;;        (audio/dispatch-serial [::audio/text-to-speech11 :audio
;;                                :speak (str line) false nil (swap! db/seq-atom inc)]))
;;      (resolve nil))))

(defn speak-box-text [line]
  (let [rr! [@audio/current-playing-text]
        speaking? (and (ut/ne? (get @audio/current-playing-text :text))
                       (= (cstr/trim (str line)) (cstr/trim (str (get @audio/current-playing-text :text)))))]
    [re-com/box
     ;;:attr (when speaking? {:id "chat-v-box1"})
     :style (if speaking?
              ;;{:border "1px solid red"} 
              {:background-color (str (theme-pull :theme/editor-outer-rim-color nil) 23)
               :color (theme-pull :theme/editor-outer-rim-color nil)
               :text-shadow "2px 1px 3px #000000"
               :border-radius "12px"}
              {})
     :child (str line)]))

;; (ut/tracked-dispatch [::wfx/push   :default
;;                       {:kind       :send-basic-message
;;                        :message "What is the stock price of Facebook?"
;;                        :thread-id  selected-thread
;;                        :assistant-name selected-worker
;;                        :client-name client-name}])


(defn custom-icon-button [{:keys [icon-name tooltip on-click active? color class]}]
  (let [hovered? (reagent/atom false)]
    (fn [{:keys [icon-name tooltip on-click active? class color]}]
      [re-com/md-icon-button
       :src (at)
       :md-icon-name icon-name
       :class class
       :on-click (fn [e]
                   (on-click e)
                   (reset! hovered? false))  ; Reset hover state after click
       :style {:color (if @hovered?
                        (theme-pull :theme/editor-outer-rim-color nil)
                        (or color "#ffffff"))
               :width "50px"
               :height "40px"
               ;:border "1px solid cyan"
               :cursor "pointer"
               :z-index (when @db/bar-hover-text 99999999)
               :text-shadow "2px 1px 3px #000000"
               :filter "drop-shadow(2px 1px 3px #000000)"
               :opacity (if active? 1.0 (if @hovered? 0.8 0.45))
               :margin-top (if (not @hovered?) "12px" "4px")
               :padding-left "2px"
               :font-size (if @hovered? "42px" "25px")
               :transition "all 0.3s ease"}
       :attr {:on-mouse-over (fn []
                               (reset! hovered? true)
                               (reset! db/bar-hover-text tooltip))
              :on-mouse-out (fn []
                              (reset! hovered? false)
                              (reset! db/bar-hover-text nil))}])))

(defn custom-text-display [{:keys [text tooltip color on-click]}]
  (let [hovered? (reagent/atom false)]
    (fn [{:keys [text tooltip color on-click]}]
      [re-com/box
       :attr {:on-mouse-over (fn []
                               (reset! hovered? true)
                               (reset! db/bar-hover-text tooltip))
              :on-mouse-out (fn []
                              (reset! hovered? false)
                              (reset! db/bar-hover-text nil))
              :on-click (when on-click
                          (fn [e]
                            (on-click e)
                            (reset! hovered? false)))}
       :style {:color (if @hovered?
                        (theme-pull :theme/editor-outer-rim-color nil)
                        (or color "#ffffff99"))
               :cursor (if on-click "pointer" "default")
               :min-width "35px"
               :height "28px"
               :font-weight 700
               :z-index (when @db/bar-hover-text 99999999)
               ;:display "inline-flex"
               ;:align-items "center"
               ;:justify-content "center"
               :text-shadow "2px 1px 3px #00000099"
               :filter "drop-shadow(2px 1px 3px #00000099)"
               :opacity (if @hovered? 1.0 0.8)
               ;:padding-top (when (not @hovered?) "4px")
               :margin-top (if (not @hovered?) "-2px" "-9px")
               :font-size (if @hovered? "30px" "18px")
               :transition "all 0.3s ease"
               :padding "5px"}
       :child [:span text]])))

(defonce client-metadata? (reagent/atom true))
(defonce speak? (reagent/atom true))
(defonce timestamps? (reagent/atom true))

(defn chat-button-panel [selected-worker selected-thread client-name]
  (let [oai-valid?    @(ut/tracked-sub ::bricks/openai-key-valid? {})
        annotate?     @(ut/tracked-sub ::bricks/annotate? {})
        selected-block    @(ut/tracked-sub ::bricks/selected-block {})
        no-selected?      (or (nil? selected-block) (= selected-block "none!"))
        voices-enabled? true ;;@(ut/tracked-sub ::audio/voices-enabled? {})
        metadata      (context-metadata)]
    [re-com/h-box
     :children
     [[re-com/h-box
       :width "260px"
       :style {;:border "1px dashed white"
               :padding-left "20px"
               :padding-right "20px"}
       :height "50px"
       :justify :between
       :children [(when oai-valid?
                    [custom-icon-button
                     (let [audio-recording? @(ut/tracked-subscribe_ [::audio/recording?])]
                       {:icon-name (if audio-recording? "ri-mic-line" "ri-mic-fill")
                        :tooltip "voice to chat text"
                        :on-click (if audio-recording?
                                    #(ut/tracked-dispatch [::audio/stop-recording])
                                    #(ut/tracked-dispatch [::audio/start-recording]))
                        :active? audio-recording?
                        :color (if audio-recording? "red" "#ffffff")})])

                  [custom-icon-button
                   {:icon-name "ri-camera-lens-line"
                    :tooltip "add a screen snap to chat"
                    :on-click #(do
                                 (reset! db/hide-annotate? true)
                                 (ut/tracked-dispatch [::bricks/save-snap-forced])
                                 ;(reset! disabled? true)
                                 (js/setTimeout (fn []
                                                  ;(reset! disabled? false)
                                                  (reset! db/hide-annotate? false)
                                                  (let [;;ww @(ut/tracked-subscribe_ [::subs/w])
                                                        client-name @(ut/tracked-sub ::bricks/client-name {})
                                                                          ;;macro-undo-limit (Math/floor (/ ww 180))
                                                        client-name-str (cstr/replace (str client-name) ":" "")
                                                        macro-undos    @(ut/tracked-sub ::conn/clicked-parameter-key-alpha {:keypath [(keyword (str "client/macro-undo-map>" client-name-str))]})
                                                        ll (last (sort macro-undos))]
                                                    (reset! db/chat-image ll))) 6000))
                    :active? false ;macro-undo?
                    :color "#ffffff"}]
                  
                  (when (not no-selected?)
                    [custom-icon-button
                     {:icon-name "ri-shapes-line"
                      :tooltip "add selected block snap to chat"
                      :on-click #(do
                                   (reset! db/hide-annotate? true)
                                   (ut/tracked-dispatch [::bricks/save-snap-block selected-block])
                                   ;(reset! disabled? true)
                                   (js/setTimeout (fn []
                                                     ;(reset! disabled? false)
                                                    (reset! db/hide-annotate? false)
                                                    (let [;;ww @(ut/tracked-subscribe_ [::subs/w])
                                                          client-name @(ut/tracked-sub ::bricks/client-name {})
                                                          ;;macro-undo-limit (Math/floor (/ ww 180))
                                                          client-name-str (cstr/replace (str client-name) ":" "")
                                                          panel-key-str (cstr/replace (str selected-block) ":" "")
                                                          ;macro-undos    @(ut/tracked-sub ::conn/clicked-parameter-key-alpha {:keypath [(keyword (str "client/macro-undo-map>" client-name-str))]})
                                                          ;ll (last (sort macro-undos))
                                                          ]
                                                      (reset! db/chat-image (str client-name-str "/" panel-key-str)))) 6000))
                      :active? false ;macro-undo?
                      :color "#ffffff"}])

                  [custom-icon-button
                   {:icon-name "ri-sketching"
                    :tooltip "annotate the screen"
                    :on-click #(ut/tracked-dispatch [::bricks/toggle-annotate])
                    :active? annotate?
                    :color (if annotate? (theme-pull :theme/editor-outer-rim-color nil) "#ffffff")}]]]

      [re-com/h-box
       :width "260px"
       ;:style {:border "1px dashed white"}
       ;:padding "6px"
       :height "50px"
       :size "none"
       :justify :center ;:between
       :align :center 
       :children [
                  
                  [custom-icon-button
                   {:icon-name "ri-pie-chart-box-line"
                    :tooltip "send canvas and block metadata"
                    :on-click #(swap! client-metadata? not)
                    :active? @client-metadata?
                    :color (if @client-metadata? (theme-pull :theme/editor-outer-rim-color nil) "#ffffff")}]
                  
                  (when voices-enabled?
                    [custom-icon-button
                     {:icon-name "ri-speak-line"
                      :tooltip "toggle assistant speech (if available)"
                      :on-click #(swap! speak? not)
                      :active? @speak?
                      :color (if @speak? (theme-pull :theme/editor-outer-rim-color nil) "#ffffff")}])
                  
                  [custom-icon-button
                   {:icon-name "ri-timeline-view"
                    :tooltip "display timestamps"
                    :on-click #(swap! timestamps? not)
                    :active? @timestamps?
                    :color (if @timestamps? (theme-pull :theme/editor-outer-rim-color nil) "#ffffff")}]
                  
                  
                  ]]

      ;; [re-com/box
      ;;  :width "260px"
      ;;  :style {:border "1px dashed white"}
      ;;  :padding "6px"
      ;;  :height "50px"
      ;;  :size "none"
      ;;  :justify :between
      ;;  :child (cstr/join "\n" (keys metadata))]

      (when @(ut/tracked-subscribe_ [::audio/audio-playing?])
        [custom-icon-button
         {:icon-name "zmdi-speaker"
          :tooltip "stop currently playing audio / skip"
          :on-click #(audio/stop-audio)
          :active? true
          :color "red"}])]]
    )
  )

(def hover-on-chat (reagent/atom false))
(defonce tool-details-hide (reagent/atom {}))

(defn top-chat-panel [panel-height selected-worker]
  (when-let [subs @(ut/tracked-sub ::get-thread-subs {})]
    (try
      (let [;subs @(ut/tracked-sub ::get-thread-subs {})
            workers-map @(ut/tracked-sub ::conn/clicked-parameter-key-alpha {:keypath [:ai-worker/config>client]})
            selected-worker-map (get workers-map selected-worker)
            client-name @(ut/tracked-sub ::bricks/client-name {})
            selected-thread @(ut/tracked-sub ::selected-ai-worker-thread {})
            panel-height (- panel-height 175) ;;; subtract header height
            ;;_ (ut/tapp>> [:subs selected-thread subs])
            thread-key (keyword (str "ai-worker/" selected-worker ">" selected-thread ">last"))
            thread-key-running (keyword (str "ai-worker/" selected-worker ">" selected-thread ">running?"))
            selected-block    @(ut/tracked-sub ::bricks/selected-block {})
            no-selected?      (or (nil? selected-block) (= selected-block "none!"))
            ;react! [@audio/current-playing-text]
            react! [@tool-details-hide @timestamps?]
            running? (or @db/hide-annotate?
                         @(ut/tracked-sub ::conn/clicked-parameter-key-alpha {:keypath [thread-key-running]}))
            metadata    (when @client-metadata? (pr-str " CLIENT-METADATA: " (context-metadata)))
            data @(ut/tracked-sub ::conn/clicked-parameter-key-alpha {:keypath [thread-key]})
            data (vec (remove nil?
                              (conj (vec data)
                                    (when (ut/ne? @db/chat-audio-text)
                                      {:role "user"
                                       :content {:text (str @db/chat-audio-text)}
                                       :event-type "user-text"})
                                    (when (ut/ne? metadata)
                                      {:role "user"
                                       :content {:text (if no-selected?
                                                         (str  "(canvas meta-data will be sent to " selected-worker " on next msg)")
                                                         (str "(canvas & " selected-block " meta-data will be sent to " selected-worker " on next msg)"))}
                                       :event-type "user-meta"})
                                    (when (ut/ne? @db/chat-image)
                                      {:role "user"
                                       :content {:text (str "assets/snaps/" (str @db/chat-image) ".jpg")}
                                       :event-type "user-image"}))))
            thread-name (get-thread-name selected-worker selected-thread)
            ;; _ (ut/tapp>> [:thread data])
            ;; running? @(ut/tracked-sub ::conn/clicked-parameter-key-alpha {:keypath (filterv #(cstr/ends-with? (str %) "running?") subs)})
            ;; data @(ut/tracked-sub ::conn/clicked-parameter-key-alpha {:keypath (filterv #(cstr/ends-with? (str %) "last") subs)})
            token-usage (distinct (for [d data
                                        :when (get-in d [:_metadata :token-usage :input_tokens])]
                                    [(get-in d [:_metadata :msg-id])
                                     (get-in d [:_metadata :token-usage :input_tokens])
                                     (get-in d [:_metadata :token-usage :output_tokens])]))
            input-tokens (reduce + (map #(get % 1) token-usage))
            output-tokens (reduce + (map #(get % 2) token-usage))
            ;total-tokens (+ input-tokens output-tokens)
            ]

        (when (not @hover-on-chat)
          (reagent.core/next-tick #(smooth-scroll-to-element "chat-v-box-parent1" "chat-v-box1")))

        [re-com/v-box
         :justify :between
         :padding "3px"
         :attr {:on-mouse-enter #(reset! hover-on-chat true)
                :on-mouse-over #(when (not @hover-on-chat) (reset! hover-on-chat true))
                :on-mouse-leave #(reset! hover-on-chat false)}
         :children
         [[re-com/v-box
           :size "auto" :padding "5px"
           :height "79px"
           :style {;:border "1px solid red" 
                   ;:background "#00000011"
                   }
           :justify :center
           :children [[re-com/h-box
                       :justify :between
                       :children
                       [;[re-com/box :child (str "tokens: " (ut/nf total-tokens))]
                    ;; [re-com/box :child (str "running? " running?)]
                        [re-com/md-icon-button
                         :style
                         {:font-size "25px"
                      ;:margin-top "2px"
                      ;:width "35px"
                          }
                         :on-click #(ut/tracked-dispatch [::select-ai-worker nil])
                         :md-icon-name "zmdi-chevron-left"]
                        [re-com/box :child (str (ut/nf input-tokens) " input, " (ut/nf output-tokens) " output")]
                        [re-com/box :child (str "$" (sonnet-cost input-tokens output-tokens))]]]
                      [re-com/box
                       :padding "6px"
                       :height "43px"
                       :style {:font-size "14px"
                               :font-weight 700
                               ;:border "1px solid yellow" 
                               ;:border-bottom "1px solid #00000055"
                               :overflow "hidden"}
                       :align (if (nil? thread-name) :center  :start) 
                       :justify :center
                       :child (str (or thread-name "New Thread"))]]]
          [re-com/box
           :size "auto"
           :attr {:id "chat-v-box-parent1"}
           :height (px panel-height)
           :style {:overflow-y "auto"
                   ;:border "1px solid green"
                   }
           :child [re-com/v-box
                   :padding "8px"
                   :size "auto" ;:height "100%"
                   :children
                   [[re-com/gap :size "10px"]
                    [re-com/v-box
                     :gap "8px"
                     :style {:padding-bottom "50px"}
                     :children (cons

                                (for [idx (range (count data))
                                      :let [ddata (get data idx)
                                            is-last? (= idx (- (count data) 1))
                                            {:keys [content role event-type _metadata]} ddata
                                            send-message (fn [] (let [image? (ut/ne? @db/chat-image)]
                                                                  (ut/tracked-dispatch
                                                                   [::wfx/push   :default
                                                                    (merge
                                                                     {:kind    :send-basic-message
                                                                      :message (str (get content :text) metadata)
                                                                      :thread-id  selected-thread
                                                                      :assistant-name selected-worker
                                                                      :client-name client-name}
                                                                     (when image? {:image-path (str "assets/snaps/" (str @db/chat-image) ".jpg")}))])
                                                                  (reset! db/chat-audio-text nil)
                                                                  (reset! db/chat-image nil)))]]
                                  [re-com/v-box
                                   :attr (when is-last? {:id "chat-v-box1"})
                                   :style (merge
                                           (if (and
                                                (not= event-type "user-image")
                                                (not= event-type "user-meta")
                                                (not= event-type "user-text"))
                                             {} ;; {:border "1px solid cyan"}
                                             {:border (str "3px dashed " (or (theme-pull :theme/universal-pop-color "#00FFFF") "#00FFFF"))
                                              :border-radius "12px"
                                              :margin-top "20px"
                                              :opacity 0.7})
                                           (when (odd? idx)  ;;(= role "assistant") 
                                             {:border-radius "12px"
                                              :background-color (str (ut/invert-hex-color (theme-pull :theme/editor-outer-rim-color nil)) 15)})
                                           (when (= event-type "start-thread")
                                             {:border-radius "12px"
                                              :background-color (get-in content [:name-style :color] (theme-pull :theme/editor-outer-rim-color nil))}))
                                   :padding "6px"
                                   :gap "2px"
                                   :children [[re-com/h-box
                                               :justify :between
                                               :padding "5px"
                                               :style {:font-weight 700}
                                               :children
                                               [[re-com/box :child (let [role (cond (= event-type "tool_result") "rabbit" 
                                                                                    (= role "assistant") selected-worker
                                                                                :else role)]
                                                                     (-> (str (if (cstr/starts-with? (str event-type) "user-")
                                                                                (str role " (unsent)")
                                                                                (str role)))
                                                                         (cstr/replace "-" " ")
                                                                         ut/proper-case))]
                                                (let [sfix (fn [x] (-> x
                                                                       str
                                                                       (cstr/replace "_" " ")
                                                                       (cstr/replace "-" " ")
                                                                       ut/proper-case))]
                                                  (cond (= event-type "user-text") [re-com/h-box :gap "3px"
                                                                                    :children [[re-com/box :child (sfix event-type)]
                                                                                               [re-com/md-icon-button :md-icon-name "zmdi-close"
                                                                                                :on-click #(reset! db/chat-audio-text nil)
                                                                                                :style {:font-size "15px" :opacity 0.33
                                                                                                        :cursor "pointer" :margin-top "-2px"}]]]
                                                        (= event-type "user-image") [re-com/h-box :gap "3px"
                                                                                     :children [[re-com/box :child (sfix event-type)]
                                                                                                [re-com/md-icon-button :md-icon-name "zmdi-close"
                                                                                                 :on-click #(reset! db/chat-image nil)
                                                                                                 :style {:font-size "15px" :opacity 0.33
                                                                                                         :cursor "pointer" :margin-top "-2px"}]]]
                                                        (= event-type "user-meta") [re-com/h-box :gap "3px"
                                                                                    :children [[re-com/box :child (sfix event-type)]
                                                                                               [re-com/md-icon-button :md-icon-name "zmdi-close"
                                                                                                :on-click #(reset! client-metadata? false)
                                                                                                :style {:font-size "15px" :opacity 0.33
                                                                                                        :cursor "pointer" :margin-top "-2px"}]]]
                                                        (= event-type "tool_use") [re-com/h-box :gap "3px"
                                                                                   :children [[re-com/md-icon-button
                                                                                               :md-icon-name (if @tool-details-hide "zmdi-chevron-down" "zmdi-chevron-up")
                                                                                               :on-click #(swap! tool-details-hide assoc (get _metadata :msg-id true)
                                                                                                                 (not (get @tool-details-hide (get _metadata :msg-id true))))
                                                                                               :style {:font-size "15px" :opacity 0.33
                                                                                                       :cursor "pointer" :margin-top "-2px"}]
                                                                                              [re-com/box
                                                                                               :child (str "called "
                                                                                                           (->
                                                                                                            (get content :name)
                                                                                                            str
                                                                                                            (cstr/replace  "__"  ".")
                                                                                                            (cstr/replace  "_"  "/")))]]]
                                                        (and (nil? event-type)
                                                             (not= role "user")) [re-com/box :child "API Error"]

                                                        :else [re-com/box :child (sfix event-type)]))]]
                                              (cond

                                                (and (not= role "user")
                                                     (= event-type "text"))
                                                [re-com/v-box
                                                 :gap "6px"
                                                 :padding "5px"
                                                 :children (for [line (cstr/split-lines (str (get content :text)))
                                                                 :let [_ (when @speak? (speak-seq line))]]
                                                             (if (= role "assistant")
                                                               [speak-box-text line]
                                                               [re-com/box :child (str line)]))]

                                                (and (= role "user")
                                                     (= event-type "text"))
                                                [re-com/v-box
                                                 :gap "6px"
                                                 :padding "5px"
                                                 :children (for [line (cstr/split-lines
                                                                       (try
                                                                         (first (cstr/split (str (get content :text)) "CLIENT-METADATA"))
                                                                         (catch :default _ (str (get content :text)))))]
                                                             [re-com/box :child (str line)])]

                                                (= event-type "user-text")
                                                [re-com/v-box
                                                 :gap "6px"
                                                 :padding "5px"
                                                 :style {:cursor "pointer"}
                                                 :attr {:on-click send-message}
                                                 :children (for [line (cstr/split-lines (str (get content :text)))]
                                                             ^{:key line}
                                                             [re-com/box :child (str line)])]

                                                (= event-type "user-meta")
                                                [re-com/v-box
                                                 :gap "6px"
                                                 :padding "5px"
                                                 :align :center :justify :center
                                                 :style {:cursor "pointer" :opacity 0.6}
                                                 :attr {:on-click send-message}
                                                 :children [[re-com/box :child (str (get content :text))]]]

                                                (= event-type "user-image")
                                                [re-com/box
                                                 :style {:cursor "pointer"}
                                                 :attr {:on-click send-message}
                                                 :child [:img {:width "100%"
                                                               :src (str (get content :text))}]]

                                                (= event-type "start-thread")
                                                [re-com/h-box
                                                 :size "auto"
                                                 :align :center
                                                 :justify :between
                                                 :style {:padding-right "3px"}
                                                 :children [[re-com/box 
                                                             :width "400px"
                                                             :padding "8px"
                                                             :child (str (get content :description))]
                                                            [:img {:width "100px"
                                                               :style {:border-radius "50px"}
                                                               :src (str (get content :image))}]]]

                                                (= event-type "image")
                                                (let [rel-path (get-in _metadata [:image-path :relative-path])]
                                                  (if rel-path
                                                    [re-com/box :child [:img {:src (get-in _metadata [:image-path :relative-path])}]]
                                                    [re-com/box
                                                     :padding "10px"
                                                     :align :center :justify :center
                                                     :style {:opacity 0.65}
                                                     :child "(cannot display image - path is not relative to web server root)"]))

                                                (and (nil? event-type)
                                                     (not= role "user"))
                                                [re-com/box :child (try
                                                                     (let [err-str-json-mixed (last (cstr/split (str content) "Calling API Error: "))
                                                                           err-mixed-map (edn/read-string err-str-json-mixed)
                                                                           json-error (ut/parse-json-string (get err-mixed-map :body))
                                                                           err-map (assoc err-mixed-map :body json-error)]
                                                                       [bricks/edn-box 500 nil err-map "11px"])
                                                                     (catch :default _ (str content)))]

                                                (= event-type "tool_use")
                                                [re-com/box :child (if (not (get @tool-details-hide (get _metadata :msg-id) true))
                                                                     [bricks/edn-box 500 nil (str content) "11px"]
                                                                     [re-com/gap :size "10px"])]

                                                (= event-type "tool_result")
                                                [re-com/box
                                                 :padding "5px"
                                                 :child (try
                                                          (str (ut/parse-json-string (get content :content)))
                                                          (catch :default _ (str "Error Decoding JSON in :content key" content)))]

                                                :else [re-com/box :child (try
                                                                           (first (cstr/split (str content) "CLIENT-METADATA"))
                                                                           (catch :default _ (str content)))])

                                              (when (and 
                                                     @timestamps?
                                                     (and
                                                     (not= event-type "user-image")
                                                     (not= event-type "user-meta")
                                                     (not= event-type "user-text")))
                                                [re-com/h-box
                                                 :justify :between
                                                 :padding "5px"
                                                 :style {:font-size "10px" :opacity 0.45}
                                                 :children
                                                 [[re-com/box :child (str (ut/format-timestamp (get _metadata :created-at)))]
                                                  [re-com/box :child (str (get _metadata :msg-id))]]])
                                              [re-com/gap :size "3px"]]])

                                [(when running?
                                   [re-com/box
                                    :align :center
                                    :justify :center
                                    :child [re-com/md-icon-button
                                            :md-icon-name "zmdi-refresh"
                                            :class "rotate linear infinite"
                                            :style {:font-size "15px"
                                                    :transform-origin "7.5px 10px" ;; perfect spin!
                                                    :margin-top "-3px"}]])])]]]]

          [chat-button-panel selected-worker selected-thread client-name]]])
      (catch :default e
        (do (ut/tapp>> [:top-chat-panel-error e])
            [re-com/box
             :align :center
             :justify :center
             :child "No threads."])))))

(re-frame/reg-sub
 ::get-thread-subs
 (fn [db _] 
   (let [flow-subs (get db :flow-subs)
         selected-worker (get db :selected-ai-worker)
         ai-worker-subs (filterv #(cstr/starts-with? (str %) (str ":ai-worker/" selected-worker)) flow-subs)]
     ai-worker-subs)))

(re-frame/reg-sub
 ::worker-drop-down?
 (fn [db _]
   (get db :worker-drop-down? false)))

(re-frame/reg-event-db 
 ::toggle-worker-drop-down 
 (fn [db _] 
   (assoc db :worker-drop-down? (not (get db :worker-drop-down? false))))) 

(re-frame/reg-sub
 ::ai-workers
 (fn [db _]
   (get-in db [:server :settings :ai-workers])))

(re-frame/reg-sub
 ::selected-ai-worker
 (fn [db _]
   (get db :selected-ai-worker)))

(re-frame/reg-sub
 ::selected-ai-worker-thread
 (fn [db _]
   (get db :selected-ai-worker-thread)))

(re-frame/reg-event-db
 ::select-ai-worker
 (fn [db [_ worker-name & [thread-id]]]
   (if thread-id 
     (-> db
         (assoc :selected-ai-worker-thread thread-id)
         (assoc :selected-ai-worker worker-name))
     (assoc db :selected-ai-worker worker-name))))

(defn worker-drop-down []
  (let [workers-map @(ut/tracked-sub ::conn/clicked-parameter-key-alpha {:keypath [:ai-worker/config>client]})
        selected-worker @(ut/tracked-sub ::selected-ai-worker {})]
    [re-com/v-box :padding "8px" :size "auto" ;:width "400px"
     :style
     {:background-color "#000000"
      :color            (theme-pull :theme/editor-outer-rim-color nil)
      :position         "fixed"
      :z-index          900
      :border-radius    "0px 0px 0px 12px"
      :border-left      (str "3px solid " (theme-pull :theme/editor-outer-rim-color nil))
      :border-bottom    (str "3px solid " (theme-pull :theme/editor-outer-rim-color nil))
      :top              24 ;(px y) ;(+ y 70))
      :font-size        "19px"
      :right            0} :children
     (for [[k v] workers-map
           :let [selected? (= k selected-worker)]]
       [re-com/h-box :gap "10px" :justify :between :children
        [[re-com/box
          :attr {:on-click #(do (ut/tracked-dispatch [::select-ai-worker k])
                                (ut/tracked-dispatch [::toggle-worker-drop-down]))}
          :child (str k)
          :style (get v :name-style {})]]
        :style
        (if selected?
          {:text-decoration  "underline"
           :font-weight      700
           :border-radius    "8px"
           :background-color (str (theme-pull :theme/editor-outer-rim-color nil) 40)}
          {:cursor "pointer" :opacity 0.6}) :padding "4px"])]))

(defn chat-panel []
  (let [x-px            (px (first @detached-coords)) ;(px (* x db/brick-size))
        y-px            (px (last @detached-coords)) ;(px (* y db/brick-size))
        panel-width     600
        selected-block  @(ut/tracked-subscribe [::bricks/selected-block])
        selected-view   @(ut/tracked-subscribe [::bricks/editor-panel-selected-view])
        client-name     @(ut/tracked-sub ::bricks/client-name {})
        audio-playing?  @(ut/tracked-subscribe [::audio/audio-playing?])
        hh              @(ut/tracked-subscribe [::subs/h]) ;; to ensure we get refreshed when
        ww              @(ut/tracked-subscribe [::subs/w])
        workers-map @(ut/tracked-sub ::conn/clicked-parameter-key-alpha {:keypath [:ai-worker/config>client]})
        selected-worker @(ut/tracked-sub ::selected-ai-worker {})
        selected-worker-map (get workers-map selected-worker)
        worker-drop-down? @(ut/tracked-sub ::worker-drop-down? {})
        panel-height    (* (.-innerHeight js/window) 0.9)]
    [bricks/reecatch
     [re-com/box :size "none" :width (px panel-width) :height (px panel-height) :attr
      {:on-mouse-enter #(reset! bricks/over-block? true) :on-mouse-leave #(reset! bricks/over-block? false)} :style
      {:position         "fixed"
       :top              y-px
       :left             x-px
       :border-radius    "16px"
       :z-index          100
       :font-family      (theme-pull :theme/base-font nil)
       :color            (theme-pull :theme/editor-font-color nil)
       :background-color (str (theme-pull :theme/editor-background-color nil) 99) ; "#000000"
       :backdrop-filter  "blur(5px)"
       :border           (str "6px solid " (theme-pull :theme/editor-outer-rim-color nil)) ; #b7e27c"
       :filter           "drop-shadow(0.35rem 0.35rem 0.4rem rgba(0, 0, 0, 0.8))"
       :box-shadow       (let [block-id       :audio
                               talking-block? true]
                           (cond (and audio-playing? talking-block?)
                                 (str
                                  "1px 1px " (px (* 80
                                                    (+ 0.1 (get @db/audio-data block-id))))
                                  " "        (theme-pull :theme/editor-outer-rim-color nil))
                                 :else                               "none"))}
      :child (if @bricks/dragging-editor?
               [re-com/box
                :size "auto"
                :align :center :justify :center
                :child "paused"
                :style (let [cc (theme-pull :theme/editor-outer-rim-color nil)
                             ccc (count cc)
                             cc (str (if (> ccc 7) (subs cc 0 7) cc) 45)]
                         {:user-select "none"
                          :color (theme-pull :theme/editor-outer-rim-color nil)
                          :font-size "18px"
                          :background-image (str "linear-gradient(0deg, "
                                                 cc
                                                 " 2px, transparent 8px), linear-gradient(90deg, "
                                                 cc
                                                 " 2px, transparent 8px)")
                          :background-size  (str "50px 50px, 50px 50px")})]
               [re-com/v-box :width "575px" :children
                [[re-com/h-box :justify :between :align :center :padding "6px" :children
                  [[re-com/md-icon-button :src (at) :md-icon-name "zmdi-arrows" :style
                    {;:background-color (theme-pull :theme/editor-rim-color nil) ;"#00000000"
                     :color      (theme-pull :theme/editor-font-color nil)
                     :cursor     "grab"
                     :height     "15px"
                     :margin-top "-9px"
                     :font-size  "19px"}
                    :attr {:on-mouse-down mouse-down-handler}]
                   [re-com/h-box :children
                    [[re-com/h-box :children
                      [
                       [re-com/md-icon-button :style
                        {:font-size "15px" 
                         :margin-top "2px"
                         :width "35px"} 
                        :on-click #(ut/tracked-dispatch [::select-ai-worker nil])
                        :md-icon-name "ri-team-line"]

                       [re-com/box :child
                        [re-com/h-box 
                         :style {:font-weight 700}
                         :children
                         [[re-com/box :child " :ai-worker/ " :style {}]
                          [re-com/box :child (str " " (or selected-worker "?"))
                           :style (get selected-worker-map :name-style
                                       {:font-family (theme-pull :theme/base-monospace-font nil)
                                        :color (theme-pull :theme/universal-pop-color "#FC0FC0")}
                                       )]]]
                        :padding "4px"]
                       [re-com/md-icon-button :style
                        {;:font-size "13px" :margin-top "-16px"
                         :width "28px"} :on-click #(ut/tracked-dispatch [::toggle-worker-drop-down]) 
                        :md-icon-name (if worker-drop-down? "zmdi-chevron-up" "zmdi-chevron-down")]
                       ]]
                     [re-com/md-icon-button 
                      :md-icon-name "zmdi-window-minimize" 
                      :on-click #(ut/tracked-dispatch [::bricks/toggle-ai-worker])
                      :style {:font-size "15px" :opacity 0.33 :cursor "pointer"}]
                     (when worker-drop-down?
                       [worker-drop-down])  
                     
                     ]]]
                  :size "none"
                  :width "588px"
                  :height "25px"
                  :style {;:background-color (theme-pull :theme/editor-rim-color nil)
                          :background    (str "linear-gradient(" (theme-pull :theme/editor-rim-color nil) ", transparent)")
                          :border-radius "10px 10px 0px 0px"
                          :color         (theme-pull :theme/editor-outer-rim-color nil)}]
                 
                 (if selected-worker
                   [top-chat-panel panel-height selected-worker]
                   [worker-start-panel panel-height])
                 
                 ;[re-com/box :child "Chat Window"]
                 ;[re-com/box :child "Bottom Window Maybe"]
                 ]])]]))
