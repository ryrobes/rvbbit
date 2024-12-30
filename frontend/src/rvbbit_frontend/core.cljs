(ns rvbbit-frontend.core
  (:require
   [cljs.core.async         :refer [go chan <! put! timeout]]
   [clojure.string          :as cstr]
   [day8.re-frame.undo      :as undo]
   [re-frame.core           :as re-frame]
   [re-pollsive.core        :as poll]
   [re-pressed.core         :as rp]
   [reagent.core            :as reagent]
   [reagent.dom             :as rdom]
   [rvbbit-frontend.audio   :as audio]
   [rvbbit-frontend.bricks  :as bricks]
   [rvbbit-frontend.config  :as config]
   [rvbbit-frontend.db      :as db]
   [rvbbit-frontend.events  :as events]
   [rvbbit-frontend.block-patrol  :as bp]
   [rvbbit-frontend.flows   :as flows]
   [rvbbit-frontend.http    :as http]
   [rvbbit-frontend.signals :as signals]
   [rvbbit-frontend.subs    :as subs]
   [rvbbit-frontend.utility :as ut]
   [rvbbit-frontend.views   :as views]
   [websocket-fx.core       :as wfx]))

(defn dev-setup [] (when config/debug? (println "dev mode")))

(defn debounce
  [f wait]
  (let [timeout-id (atom nil)]
    (fn [& args]
      (when @timeout-id (js/clearTimeout @timeout-id))
      (reset! timeout-id (js/setTimeout (fn [] (apply f args)) wait)))))

(defn update-mouse-activity [] (reset! db/last-mouse-activity (js/Date.)))

(defn track-mouse-activity []
  (let [debounced-update (debounce update-mouse-activity 1000)] ;; 1000 milliseconds = 1 second
    (.addEventListener js/window "mousemove" (fn [_] (debounced-update)))))

(re-frame/reg-fx :reset-atom!
                 (fn [[atom value]]
                   (reset! atom value)))

(re-frame/reg-event-db ::alt-key-down (fn [db _] (assoc db :alt-key-held? true)))

(re-frame/reg-event-db ::alt-key-up (fn [db _] (assoc db :alt-key-held? false)))

(re-frame/reg-event-db ::alt-key-toggle (fn [db _] (assoc db :alt-key-held? (not (get db :alt-key-held? false)))))

;; (re-frame/reg-event-db ::console-log (fn [_ _]
;;                                        (reset! db/loaded-screen? false)
;;                                        (reset! db/final-loading-message nil) db))

(re-frame/reg-event-fx ::console-log
                       (fn [_ _]
                         {:fx [[:reset-atom! [db/loaded-screen? false]]
                               [:reset-atom! [db/post-boot? true]]
                               [:reset-atom! [db/final-loading-message nil]]]}))

(defn dispatch-keydown-rules []
  (ut/tracked-dispatch-sync
    [::rp/set-keydown-rules
     {:event-keys           [;[[::bricks/shift-down] [{:keyCode 16 :shiftKey true}]]
                             [[::bricks/esc-unselect-current] [{:keyCode 27}]] ; ESC
                             [[::bricks/redo-one] [{:keyCode 90 :ctrlKey true :shiftKey true}]] ; Z
                             [[::bricks/undo-one] [{:keyCode 90 :ctrlKey true :shiftKey false}]]
                             [[::bricks/toggle-buffy] [{:keyCode 32 :shiftKey true :ctrlKey false}]] ; shift-space
                             [[::bricks/toggle-ai-worker] [{:keyCode 67 :shiftKey true :ctrlKey false}]] ; shift-c
                             [[::bricks/toggle-editor] [{:keyCode 32 :shiftKey false :ctrlKey false}]] ; space
                             [[::bricks/toggle-flow] [{:keyCode 32 :shiftKey false :ctrlKey true}]] ; ctrl-space
                             [[::bricks/next-panel] [{:keyCode 9}]] ; tab
                             [[::bricks/toggle-kick-alert] [{:keyCode 75}]] ; k
                             [[::bricks/toggle-alert-mute] [{:keyCode 77}]] ; m
                             [[::bricks/toggle-peek] [{:keyCode 80}]] ; p
                             [[::bricks/toggle-lines] [{:keyCode 76}]] ; l
                             [[::bricks/toggle-auto-run] [{:keyCode 79}]] ; O
                             [[::bricks/delete-selected-panel] [{:keyCode 46 :shiftKey true}]] ; shift-delete
                             [[::bricks/cycle-column-select false] [{:keyCode 81}]] ; q
                             [[::bricks/cycle-column-select true] [{:keyCode 69}]] ; e
                             [[::audio/start-recording] [{:keyCode 84}]] ; t
                             [[::audio/stop-recording] [{:keyCode 89}]] ; y
                             [[::bricks/stretch-panel :wider] [{:keyCode 68 :shiftKey true}]] ; shift-d
                             [[::bricks/stretch-panel :longer] [{:keyCode 83 :shiftKey true}]] ; shift-s
                             [[::bricks/stretch-panel :narrower] [{:keyCode 65 :shiftKey true}]] ; shift-a
                             [[::bricks/stretch-panel :shorter] [{:keyCode 87 :shiftKey true}]] ; shift-w
                             [[::bricks/panel-depth-up] [{:keyCode 82 :shiftKey true}]] ; shift-r
                             [[::bricks/panel-depth-down] [{:keyCode 70 :shiftKey true}]] ; shift-f
                             [[::bricks/random-theme] [{:keyCode 82 :shiftKey false :ctrlKey true}]] ; ctrl-r
                             [[::flows/run-current-flowmap] [{:keyCode 70} {:keyCode 70}]] ; f f
                             ;[[::bricks/toggle-session-modal] [{:keyCode 192}]] ; ` tilde ;; old echoes TODO bring back
                             [[::console-log] [{:keyCode 192}]] ; ` tilde
                             [[::bricks/save] [{:keyCode 83 :shiftKey false :ctrlKey true}]] ;; ctrl-s
                             [[::bricks/nudge-panel :up] [{:keyCode 87}]] ; w
                             [[::bricks/nudge-panel :down] [{:keyCode 83}]] ; s
                             [[::bricks/nudge-panel :left] [{:keyCode 65}]] ; a
                             [[::bricks/nudge-panel :right] [{:keyCode 68}]] ; d
                             [[::alt-key-toggle] [{:keyCode 86}]] ;; v
                             [[::alt-key-down] [{:keyCode 67}]]] ;; alt key down
      :prevent-default-keys [{:keyCode 32} {:keyCode 86} {:keyCode 46 :shiftKey true} {:keyCode 83 :ctrlKey true}
                             {:keyCode 82 :shiftKey false :ctrlKey true}
                             {:keyCode 83 :shiftKey false :ctrlKey true} {:keyCode 9} {:keyCode 70 :ctrlKey true}]}]))



(defn dispatch-keyup-rules []
  (ut/tracked-dispatch-sync [::rp/set-keyup-rules {:event-keys [[[::alt-key-up] [{:keyCode 67}]]]}]))

;; (re-frame/reg-sub
;;  ::memory-usage-breached-threshold?
;;  (fn [db _]
;;    (let [{:keys [_ used ttl-heap]} (get db :memory)]
;;      (> (/ used ttl-heap) 0.75))))

(defonce root-key
  (reagent/atom (cljs.core/random-uuid)))

(defn root []
  (fn [] [[:div {:key @root-key}
           [views/main-panel]]]))

(defn clear-cache-and-reload! []
  (re-frame/clear-subscription-cache!)
  (swap! root-key (cljs.core/random-uuid))
  (let [root-el (.getElementById js/document "app")]
    (rdom/unmount-component-at-node root-el)
    (rdom/render [root] root-el)))

;; (re-frame/reg-event-db
;;  ::purge-sub-cache!
;;  (fn [db]
;;    (let [;client-name       (get db :client-name)
;;          [total used heap] (get db :memory)]
;;      (ut/tapp>> [:debug "total memory:" (ut/bytes-to-mb total) "used memory:" (ut/bytes-to-mb used) "heap:"
;;                  (ut/bytes-to-mb heap)])
;;      (let [pct-used     (/ used total)
;;            pct-used-str (str (.. pct-used (toFixed 1)) "%")]
;;        (ut/tapp>> [:purging-sub-cache-for! db/client-name :pct-used pct-used-str])
;;        (clear-cache-and-reload!)
;;        db))))

(defn dispatch-poller-rules []
  (ut/tracked-dispatch
    [::poll/set-rules
     [{:interval                 10
       :event                    [::bricks/dispatch-auto-queries]
       :poll-when                [::bricks/auto-run-and-connected?]
       :dispatch-event-on-start? false}

      {:interval                 5
       :event                    [::bricks/update-flow-statuses]
       :poll-when                [::bricks/update-flow-statuses?]
       :dispatch-event-on-start? true}

      {:interval                 600 ;; was 3600
       :event                    [::bricks/clean-up-reco-previews]
       :poll-when                [::bricks/clean-up-reco-previews?]
       :dispatch-event-on-start? false}

      {:interval                 5
       :event                    [::bricks/prune-alerts]
       :dispatch-event-on-start? false}

      {:interval                 7
       :event                    [::bricks/get-memory-usage]
       :dispatch-event-on-start? false}

      ;; {:interval                 1 ;;; test
      ;;  :event                    [::bricks/highlight-panel-code]
      ;;  :poll-when                [::bricks/panel-code-up?]
      ;;  :dispatch-event-on-start? false}

      ;; {:interval                 2 ;; push sample data to runstream when running ?
      ;;  :event                    [::bricks/refresh-runstreams]
      ;;  :poll-when                [::bricks/runstream-running?]
      ;;  :dispatch-event-on-start? false}

      {:interval                 1 ;; subscribe to server data from flows if we see it
       :event                    [::bricks/sub-to-flows]
       :poll-when                [::bricks/new-flow-subs?]
       :dispatch-event-on-start? false}

      {:interval                 30 ;; 5 ;; unsubscribe to server data
       :event                    [::bricks/unsub-to-flows]
       :poll-when                [::bricks/stale-flow-subs?]
       :dispatch-event-on-start? false}

      ;; {:interval                 3600 ;; ten mins. less? more?
      ;;  :event                    [::bricks/purge-cache-atoms]
      ;;  :dispatch-event-on-start? false}

      {:interval                 120 ;;300
       :event                    [::bricks/save-snap-periodically]
       :dispatch-event-on-start? false}

      {:interval                 1800 ;; 30 mins. more?
       :event                    [::bricks/clear-cache-atoms]
       :dispatch-event-on-start? false}

      {:interval                 3600 ;; expensive, testing
       :event                    [::http/get-autocomplete-values]
       :dispatch-event-on-start? true}

      {:interval                 3 ;8 ;5
       :event                    [::bp/deal-with-changed-panels]
       :poll-when                [::bp/panels-changed?]
       :dispatch-event-on-start? false}

      {:interval                 15
       :event                    [::bricks/update-user-params-hash]
       :poll-when                [::bricks/user-params-hash-changed?]
       :dispatch-event-on-start? false}

      ;; {:interval 1000
      ;;  :event [::bricks/update-metadata-tabs]
      ;;  :dispatch-event-on-start? false}

       {:interval                 300
        :event                    [::ut/random-fun-log-message]
        :dispatch-event-on-start? false}

      ;; {:interval                 5 ;; too much when recos gets big? filter?
      ;;  :event                    [::bricks/update-reco-previews]
      ;;  :dispatch-event-on-start? false}

      ;; {:interval                 600
      ;;  :event                    [::bricks/update-conditionals]
      ;;  :poll-when                [::bricks/visible-conditionals?]
      ;;  :dispatch-event-on-start? false}

      {:interval                 4
       :event                    [::bricks/refresh-status]
       :poll-when                [::bricks/bg-status?] ;; @db/editor-mode
       :dispatch-event-on-start? false}]]))

(defn wait-for-wss-config []
  (let [c (chan)]
    (re-frame/dispatch-sync [::http/wss-config])
    (go
      (loop []
        (let [config (get @re-frame.db/app-db :wss-config)]
          (cond
            (vector? config) (do (put! c true)
                                 (ut/pp ["🗝" :websocket-config-aquired! (str config)])
                                 ;(.close c)
                                 )
            (= config "failed") (do (put! c false)
                                    ;(.close c)
                                    )
            :else (do (<! (timeout 100))
                      (recur))))))
    c))

(defn ^:dev/after-load mount-root []
  (re-frame/clear-subscription-cache!)
  (reset! db/clover-cache-atom {})
  (let [root-el (.getElementById js/document "app")]
    (rdom/unmount-component-at-node root-el)
    (rdom/render [views/main-panel] root-el)))

(def g-key-down? (atom false))

(defn init []
  (set! (.-title js/document) (str "🐇 Rabbit is dreaming... 🌙"))
  (ut/tracked-dispatch-sync [::events/initialize-db])
  (ut/tracked-dispatch [::bricks/set-client-name db/client-name])

  (go
    (let [_ (<! (wait-for-wss-config))]  ;; need websocket data before we connect and sub to all the things
      (ut/tracked-dispatch [::wfx/connect http/socket-id (http/options db/client-name)])
      ;(ut/tracked-dispatch [::wfx/connect :secondary (http/options-secondary :secondary)])
      ;(ut/tracked-dispatch [::wfx/connect :leaves (http/options-secondary :leaves)])
      (ut/tracked-dispatch [::wfx/connect :query1 (http/options-secondary :query1)])
      (ut/tracked-dispatch [::wfx/connect :query2 (http/options-secondary :query2)])
      (ut/tracked-dispatch [::wfx/connect :query3 (http/options-secondary :query3)])
      (ut/tracked-dispatch-sync [::wfx/request :default
                                 {:message     {:kind :get-settings
                                                :client-name db/client-name}
                                  :on-response [::http/simple-response-boot-no-load] ;; just get settings
                                  :on-timeout  [::http/timeout-response [:boot :get-settings]]
                                  :timeout     15000}])
      (ut/tracked-dispatch [::wfx/request :default
                            {:message     {:kind :signals-map
                                           :client-name db/client-name}
                             :on-response [::signals/signals-map-response]
                             :timeout     15000000}])
      (ut/tracked-dispatch [::wfx/request :default
                            {:message     {:kind :solvers-map
                                           :client-name db/client-name}
                             :on-response [::signals/solvers-map-response]
                             :timeout     15000000}])
      (undo/undo-config! {:harvest-fn   (fn [ratom] (select-keys @ratom [:panels :signals-map :flows :click-param]))
                          :reinstate-fn (fn [ratom value] (swap! ratom merge value))})
      (track-mouse-activity)
    ;; (let [press-fn   (fn [event] ;; test, keeping out of re-pressed / app-db due to causing event
    ;;                    (when (and (= (.-keyCode event) 71) (not @g-key-down?))
    ;;                      (reset! g-key-down? true)
    ;;                      (reset! flows/drop-toggle? (not @flows/drop-toggle?))))
    ;;       release-fn (fn [event] (when (= (.-keyCode event) 71)
    ;;                                (reset! g-key-down? false)
    ;;                                (reset! flows/drop-toggle? false)))]
    ;;   (.addEventListener js/window "keydown" press-fn)
    ;;   (.addEventListener js/window "keyup" release-fn))
      (ut/tracked-dispatch
       [::wfx/request :default
        {:message {:kind :session-snaps
                   :client-name db/client-name}
         :on-response [::bricks/save-sessions]
         :timeout 15000}])

      (js/setTimeout
       (fn []
         (let [url-vec  @(ut/tracked-subscribe [::http/url-vec])
               base-dir "./screens/"]
           (if (>= (count url-vec) 1) ;; if we have a url with a screen, load that, oitherwise load default screen
             (ut/tracked-dispatch-sync [::http/load (str base-dir (js/decodeURIComponent (first url-vec)) ".edn")])
             (ut/tracked-dispatch [::wfx/request :default ;:secondary ;; load default boot flowset
                                   {:message     {:kind :get-settings :client-name db/client-name}
                                    :on-response [::http/simple-response-boot]
                                    :on-timeout  [::http/timeout-response [:boot :get-settings]]
                                    :timeout     60000}])))) 5000)

      (http/start-listening-to-url-changes)
      (ut/tracked-dispatch-sync [::rp/add-keyboard-event-listener "keydown"])
      (ut/tracked-dispatch-sync [::rp/add-keyboard-event-listener "keyup"])
      (ut/tracked-dispatch [::poll/init])
      (ut/tracked-dispatch [::subs/window-fx-watcher])
      (ut/dispatch-delay 7000 [::audio/text-to-speech11 :audio :elevenlabs nil])
    ;; ^^ get voices if avail, but wait to make sure we have the api key from the server first
      (ut/dispatch-delay 4000 [::bricks/leaf-push [:canvas :canvas :canvas] {}])
      (dispatch-poller-rules)
      (dispatch-keyup-rules)
      (dispatch-keydown-rules)
      (dev-setup)
      (mount-root))))
