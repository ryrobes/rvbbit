(ns rvbbit-frontend.core
  (:require
   [reagent.dom :as rdom]
   [re-frame.core :as re-frame]
   [re-pressed.core :as rp]
   [rvbbit-frontend.events :as events]
   [rvbbit-frontend.views :as views]
   [rvbbit-frontend.bricks :as bricks]
   [rvbbit-frontend.audio :as audio]
   [rvbbit-frontend.flows :as flows]
   [rvbbit-frontend.config :as config]
   [rvbbit-frontend.subs :as subs]
   [rvbbit-frontend.http :as http]
   [rvbbit-frontend.db :as db]
   [rvbbit-frontend.signals :as signals]
   [rvbbit-frontend.utility :as ut]
   [clojure.string :as cstr]
   [day8.re-frame.undo :as undo]
   ;[cljs-time.core]
   [websocket-fx.core :as wfx]
   [re-pollsive.core :as poll]))

(defn dev-setup []
  (when config/debug?
    (println "dev mode")))

(def client-name (ut/gen-client-name))

(defn debounce [f wait]
  (let [timeout-id (atom nil)]
    (fn [& args]
      (when @timeout-id
        (js/clearTimeout @timeout-id))
      (reset! timeout-id
              (js/setTimeout (fn [] (apply f args)) wait)))))

(defn update-mouse-activity []
  (reset! db/last-mouse-activity (js/Date.)))

(defn track-mouse-activity []
  (let [debounced-update (debounce update-mouse-activity 1000)] ;; 1000 milliseconds = 1 second
    (.addEventListener js/window "mousemove"
                       (fn [_] (debounced-update)))))

(re-frame/reg-event-db
 ::alt-key-down
 (fn [db _]
   (assoc db :alt-key-held? true)))

(re-frame/reg-event-db
 ::alt-key-up
 (fn [db _]
   (assoc db :alt-key-held? false)))

(re-frame/reg-event-db
 ::alt-key-toggle
 (fn [db _]
   (assoc db :alt-key-held? (not (get db :alt-key-held? false)))))

(defn dispatch-keydown-rules []
  (ut/tracked-dispatch-sync
   [::rp/set-keydown-rules
    {:event-keys [;[[::bricks/shift-down] [{:keyCode 16 :shiftKey true}]]
                  ;[[::bricks/select-block "none!"] [{:keyCode 27}]] ; ESC 
                  [[::bricks/esc-unselect-current] [{:keyCode 27}]] ; ESC 
                  [[::bricks/redo-one] [{:keyCode 90 :ctrlKey true :shiftKey true}]] ; Z
                  [[::bricks/undo-one] [{:keyCode 90 :ctrlKey true :shiftKey false}]]
                  [[::bricks/toggle-buffy] [{:keyCode 32 :shiftKey true :ctrlKey false}]] ; space 
                  [[::bricks/toggle-editor] [{:keyCode 32 :shiftKey false :ctrlKey false}]] ; space 
                  [[::bricks/toggle-flow] [{:keyCode 32 :shiftKey false :ctrlKey true}]] ; space 

                  [[::bricks/next-panel] [{:keyCode 9}]] ; tab

                  [[::bricks/toggle-kick-alert] [{:keyCode 75}]] ; k

                  [[::bricks/toggle-peek] [{:keyCode 80}]] ; p 
                  [[::bricks/toggle-lines] [{:keyCode 76}]] ; l
                  [[::bricks/toggle-auto-run] [{:keyCode 79}]] ; O

                  [[::bricks/delete-selected-panel] [{:keyCode 46 :shiftKey true}]] ; shift-delete

                  [[::bricks/cycle-column-select false] [{:keyCode 81}]] ; q
                  [[::bricks/cycle-column-select true] [{:keyCode 69}]] ; e

                  [[::audio/start-recording]       [{:keyCode 84}]] ; t
                  [[::audio/stop-recording]       [{:keyCode 89}]] ; y

                  [[::bricks/stretch-panel :wider] [{:keyCode 68 :shiftKey true}]] ; shift-d 
                  [[::bricks/stretch-panel :longer] [{:keyCode 83 :shiftKey true}]] ; shift-s 
                  [[::bricks/stretch-panel :narrower] [{:keyCode 65 :shiftKey true}]] ; shift-a 
                  [[::bricks/stretch-panel :shorter] [{:keyCode 87 :shiftKey true}]] ; shift-w

                  [[::bricks/panel-depth-up] [{:keyCode 82 :shiftKey true}]] ; shift-r
                  [[::bricks/panel-depth-down] [{:keyCode 70 :shiftKey true}]] ; shift-f

                  [[::flows/run-current-flowmap] [{:keyCode 70} {:keyCode 70}]] ; f f 

                  [[::bricks/toggle-session-modal] [{:keyCode 192}]] ; ` tilde

                 ; [[::flows/flip-drop-toggle] [{:keyCode 71}]] ; g

                  ;[[::bricks/change-drop-type] [{:keyCode 81}]] ; q
                  ;[[::bricks/change-drop-type] [{:keyCode 69}]] ; e

                  [[::bricks/save] [{:keyCode 83 :shiftKey false :ctrlKey true}]]

                  [[::bricks/nudge-panel :up] [{:keyCode 87}]] ; w
                  [[::bricks/nudge-panel :down] [{:keyCode 83}]] ; s
                  [[::bricks/nudge-panel :left] [{:keyCode 65}]] ; a
                  [[::bricks/nudge-panel :right] [{:keyCode 68}]] ; d

                  [[::alt-key-toggle] [{:keyCode 86}]] ;; v 

                  [[::alt-key-down] [{:keyCode 67}]]] ;; alt key down

     :prevent-default-keys  [{:keyCode 32}
                             {:keyCode 86}
                             ;{:keyCode 18}
                             {:keyCode 46 :shiftKey true}
                             {:keyCode 83 :ctrlKey true}
                             {:keyCode 83 :shiftKey false :ctrlKey true}
                             {:keyCode 9}
                             {:keyCode 70 :ctrlKey true}]}]))



(defn dispatch-keyup-rules []
  (ut/tracked-dispatch-sync
   [::rp/set-keyup-rules
    {:event-keys
     ;;[[[::bricks/shift-up] [{:keyCode 16 :shiftKey true}]]]
     [[[::alt-key-up] [{:keyCode 67}]]]}]))
     ;:prevent-default-keys  [{:keyCode 32} {:keyCode 83 :ctrlKey true} {:keyCode 9} {:keyCode 70 :ctrlKey true}]  

(re-frame/reg-sub
 ::memory-usage-breached-threshold?
 (fn [db _]
   (let [{:keys [_ used ttl-heap]} (get db :memory)]
     (> (/ used ttl-heap) 0.75))))

(defonce root-key (reagent.core/atom (cljs.core/random-uuid)))

(defn root []
  (fn []
    [[:div {:key @root-key} [views/main-panel]]]))

(defn clear-cache-and-reload! [] ;;; TEST!
  (re-frame/clear-subscription-cache!)
  (swap! root-key (cljs.core/random-uuid))
  (let [root-el (.getElementById js/document "app")]
    (rdom/unmount-component-at-node root-el)
    (rdom/render [root] root-el)))

;; (defn clear-cache-and-reload! [] ;;; TEST!
;;   (re-frame/clear-subscription-cache!)
;;   (let [root-el (.getElementById js/document "app")]
;;     (rdom/unmount-component-at-node root-el)
;;     (rdom/render [views/main-panel] root-el)))

;; (re-frame/reg-event-db
;;  ::purge-sub-cache!
;;  (fn [db]
;;    (let [client-name (get db :client-name)
;;          {:keys [total used _]} (get db :memory)
;;          pct-used (/ used total)
;;          pct-used-str (str (.. pct-used (toFixed 1)) "%")]
;;      (tap> [:purging-sub-cache-for! client-name :pct-used pct-used-str])
;;      (clear-cache-and-reload!)
;;      db)))

(re-frame/reg-event-db
 ::purge-sub-cache!
 (fn [db]
   (let [client-name (get db :client-name)
         [total used heap] (get db :memory)]
     (tap> [:debug "total memory:" (ut/bytes-to-mb total) "used memory:" (ut/bytes-to-mb used) "heap:" (ut/bytes-to-mb heap)])
     (let [pct-used (/ used total)
           pct-used-str (str (.. pct-used (toFixed 1)) "%")]
       (tap> [:purging-sub-cache-for! client-name :pct-used pct-used-str])
       (clear-cache-and-reload!)
       db))))


(defn dispatch-poller-rules []
  (ut/tracked-dispatch
   [::poll/set-rules
    [{:interval                 10 ;; 1 terrible idea. test
      :event                    [::bricks/dispatch-auto-queries]
      :poll-when                [::bricks/auto-run-and-connected?] 
      :dispatch-event-on-start? false}

     {:interval                 5
      :event                    [::bricks/update-metadata]
      ;:poll-when                [::subs/get-the :auto-run-enabled?]
      :dispatch-event-on-start? false}

    ;;  {:interval                 30
    ;;   :event                    [::bricks/take-screenshot]
    ;;   :poll-when                [::bricks/is-mouse-active?]
    ;;   :dispatch-event-on-start? false}

     {:interval                 5
      :event                    [::bricks/update-flow-statuses]
      :poll-when                [::bricks/update-flow-statuses?]
      :dispatch-event-on-start? true}

     {:interval                 120
      :event                    [::bricks/clean-up-reco-previews]
      ;:poll-when                [::subs/get-the :auto-run-enabled?]
      :dispatch-event-on-start? false}

     {:interval                 2
      :event                    [::bricks/prune-alerts]
      ;:poll-when                [::subs/get-the :auto-run-enabled?]
      :dispatch-event-on-start? false}

     {:interval                 5
      :event                    [::bricks/get-memory-usage]
      ;:poll-when                [::subs/get-the :auto-run-enabled?]
      :dispatch-event-on-start? false}
     
     {:interval                 1
      :event                    [::signals/run-signals-history]
      :poll-when                [::signals/run-signals-history?]
      :dispatch-event-on-start? false}
     
    ;;  {:interval                 240
    ;;   :event                    [::purge-sub-cache!]
    ;;   ;:poll-when                [::memory-usage-breached-threshold?]
    ;;   :dispatch-event-on-start? false}     

    ;;  {:interval                 15 ;; more?
    ;;   :event                    [::bricks/resub!] ;[::wfx/subscribe http/socket-id :server-push2 (http/subscription client-name)]
    ;;   :poll-when                [::bricks/lost-server-connection?]
    ;;   :dispatch-event-on-start? false}

    ;;  {:interval                 2
    ;;   :event                    [::bricks/check-status]
    ;;   :poll-when                [::bricks/something-running?]
    ;;   :dispatch-event-on-start? false}

     {:interval                 2 ;; push sample data to runstream when running ?
      :event                    [::bricks/refresh-runstreams]
      :poll-when                [::bricks/runstream-running?]
      :dispatch-event-on-start? false}

     {:interval                 1 ;; subscribe to server data from flows if we see it
      :event                    [::bricks/sub-to-flows]
      :poll-when                [::bricks/new-flow-subs?]
      :dispatch-event-on-start? false}
     
     {:interval                 5 ;; subscribe to server data from flows if we see it
      :event                    [::bricks/unsub-to-flows]
      :poll-when                [::bricks/stale-flow-subs?]
      :dispatch-event-on-start? false}     
     
    ;;  {:interval                 30
    ;;   :event                    [::bricks/sub-to-flows-all]
    ;;   ;:poll-when                [::bricks/new-flow-subs?]
    ;;   :dispatch-event-on-start? true}     

   ;  {:interval                 1
   ;   :event                    [::bricks/update-metadata-styles]
   ;   ;:poll-when                [::subs/get-the :auto-run-enabled?]
   ;   :dispatch-event-on-start? false}

     {:interval                 1000
      :event                    [::bricks/update-metadata-tabs]
      ;:poll-when                [::subs/get-the :auto-run-enabled?]
      :dispatch-event-on-start? false}

     {:interval                 1
      :event                    [::bricks/update-conditionals]
      ;:poll-when                [::subs/get-the :auto-run-enabled?]
      :dispatch-event-on-start? false}

     {:interval                 4
      :event                    [::bricks/refresh-status]
      :poll-when                [::bricks/bg-status?] ;; @db/editor-mode
      :dispatch-event-on-start? false}]]))

(defn ^:dev/after-load mount-root []
  (re-frame/clear-subscription-cache!)
  ;(ut/tracked-dispatch [::wfx/connect http/socket-id db/options])
  ;(ut/tracked-dispatch [::wfx/subscribe http/socket-id :server-push db/subscription]) ;; resub on dev page reload
  (let [root-el (.getElementById js/document "app")]
    (rdom/unmount-component-at-node root-el)
    (rdom/render [views/main-panel] root-el)))

(def g-key-down? (atom false))

(defn init []
  ;; (set! (.-title js/document) (str "Rabbit (" client-name ")"))
  (set! (.-title js/document) (str "Rabbit is dreaming..."))
  (ut/tracked-dispatch-sync [::events/initialize-db])
  (ut/tracked-dispatch [::bricks/set-client-name client-name])
  (ut/tracked-dispatch [::wfx/connect http/socket-id (http/options client-name)])
  (ut/tracked-dispatch [::wfx/request :default
                      {:message    {:kind :get-settings
                                    :client-name client-name}
                       :on-response [::http/simple-response-boot-no-load] ;; just get settings
                       :on-timeout [::http/timeout-response [:boot :get-settings]]
                       :timeout    15000}])
  (ut/tracked-dispatch [::wfx/request :default
                      {:message    {:kind :signals-map
                                    :client-name @(ut/tracked-subscribe [::bricks/client-name])}
                       :on-response [::signals/signals-map-response]
                       ;;:on-timeout  [::timeout-response :get-signals]
                       :timeout    15000000}])
  (undo/undo-config!
   {:harvest-fn  (fn [ratom] (select-keys @ratom [:panels :signals-map :flows]))
    :reinstate-fn (fn [ratom value] (swap! ratom merge value))})
  (track-mouse-activity)
  (let [press-fn (fn [event] ;; test, keeping out of re-pressed / app-db due to causing event thrash
                   ;; still kind of fucky though
                   (when (and (= (.-keyCode event) 71) (not @g-key-down?))
                     (do (reset! g-key-down? true)
                         (reset! flows/drop-toggle? (not @flows/drop-toggle?)))))
        release-fn (fn [event]
                     (when (= (.-keyCode event) 71)
                       (do (reset! g-key-down? false)
                           (reset! flows/drop-toggle? false))))]
    (.addEventListener js/window "keydown" press-fn)
    (.addEventListener js/window "keyup" release-fn))
  (ut/tracked-dispatch [::wfx/request :default
                      {:message    {:kind :session-snaps
                                    :client-name client-name}
                       :on-response [::bricks/save-sessions]
                       :timeout    15000}])

  (let [url-vec  @(ut/tracked-subscribe [::http/url-vec])
        base-dir "./screens/"]
    (if (>= (count url-vec) 1) ;; if we have a url with a flowset, load that, oitherwise load the server default
      (ut/tracked-dispatch-sync [::http/load (str base-dir (js/decodeURIComponent (first url-vec)) ".edn")])
      (ut/tracked-dispatch [::wfx/request :default ;; load default boot flowset
                          {:message    {:kind :get-settings
                                        :client-name client-name}
                           :on-response [::http/simple-response-boot]
                           :on-timeout [::http/timeout-response [:boot :get-settings]]
                           :timeout    15000}])))
  (http/start-listening-to-url-changes)
  (ut/tracked-dispatch-sync [::rp/add-keyboard-event-listener "keydown"])
  (ut/tracked-dispatch-sync [::rp/add-keyboard-event-listener "keyup"])
  (ut/tracked-dispatch-sync [::poll/init])
  (ut/tracked-dispatch [::subs/window-fx-watcher])
 ; (ut/tracked-dispatch [::subs/window-resized]) ;; redundant? 
  (ut/tracked-dispatch [::audio/text-to-speech11 :audio :elevenlabs nil]) ;; get voices if avail
  (dispatch-poller-rules)
  (dispatch-keyup-rules)
  (dispatch-keydown-rules)

  (dev-setup)
  (mount-root))
