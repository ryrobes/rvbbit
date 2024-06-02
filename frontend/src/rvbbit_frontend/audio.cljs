(ns rvbbit-frontend.audio
  (:require [re-frame.core :as re-frame]
            [day8.re-frame.http-fx]
            [ajax.core :as ajax]
            [clojure.string :as cstr]
            ["blob-util" :as blob-util]
            [day8.re-frame.undo :as undo :refer [undoable]]
            [ajax.protocols :as pr]
            [rvbbit-frontend.db :as db]
            [rvbbit-frontend.http :as http]
            [rvbbit-frontend.resolver :as resolver]
            [rvbbit-frontend.connections :as conn]
            [rvbbit-frontend.utility :as ut]
            [websocket-fx.core :as wfx]
            [goog.dom :as dom]
            [clojure.walk :as walk]))

(def google-token "")
(defonce listening-block (reagent.core/atom nil))

(def waiting-for-response (reagent.core/atom []))

(re-frame/reg-sub
 ::transcriptions
 (fn [db _]
   (vec
    (for [e (get db :audio-transcribed)]
      (get-in e [:alternatives])))))

(re-frame/reg-sub
 ::recording?
 (fn [db _]
   (not (nil? (get db :recorder)))))

(re-frame/reg-sub
 ::latest-transcription
 (fn [db _]
   ; (str (cstr/join ". " (last (get db :audio-transcribed-vec))) ".")
   (str (last (get db :audio-transcribed-vec)))))

(re-frame/reg-event-db
 ::chat-back ;; basically ::append-block-body w/o blocking
 (fn [db [_ block-id text]]
   (if (some #{@listening-block} (keys (get db :blocks)))
     (assoc-in db [:blocks block-id :body]
               (str (get-in db [:blocks block-id :body]) "\n" text))
     db)))

(re-frame/reg-sub
 ::self-voice-name
 (fn [db _]
   (get db :self-voice-name "(this you?)")))

(re-frame/reg-event-db
 ::set-self-voice-name
 (undoable)
 (fn [db [_ new-name]]
   (assoc db :self-voice-name new-name)))

(re-frame/reg-sub
 ::auto-train-voice?
 (fn [db _]
   (get db :auto-train-voice? false)))

(re-frame/reg-event-db
 ::set-auto-train-voice
 (fn [db [_ bool]]
   (assoc db :auto-train-voice? bool)))
          ;(not (get db :auto-train-voice? false))


;; (re-frame/reg-event-db
;;  ::failure-speech-to-text
;;  (fn [db [_ result]]
;;    (let [old-status (get-in db [:http-reqs :speech-to-text])]
;;      (assoc-in db [:http-reqs :speech-to-text]
;;                (merge old-status
;;                       {:status "failed"
;;                        :ended (ut/get-time-format-str)
;;                        :ended-unix (.getTime (js/Date.))
;;                        :message result})))))

;; (re-frame/reg-event-db
;;  ::success-speech-to-text
;;  (fn [db [_  result]]
;;    (let [old-status (get-in db [:http-reqs :speech-to-text])
;;          phrases (count (get result :results))
;;          lines (vec (for [e (range phrases)]
;;                       (get-in result [:results e :alternatives 0 :transcript])))]
;;      (ut/tracked-dispatch [::chat-back @listening-block (cstr/join "\n" lines)])
;;      (-> db
;;          (assoc :audio-transcribed (conj (get db :audio-transcribed)
;;                                          (get result :results)))
;;          (assoc :audio-transcribed-vec (vec (conj (vec (get db :audio-transcribed-vec []))
;;                                                   lines)))
;;          (assoc-in [:http-reqs :speech-to-text]
;;                    (merge old-status
;;                           {:status "success"
;;                            :ended (ut/get-time-format-str)
;;                            :ended-unix (.getTime (js/Date.))
;;                            :message result}))))))

;; (re-frame/reg-event-fx
;;  ::speech-to-text
;;  (fn [{:keys [db]} _]
;;    (let [url "https://speech.googleapis.com/v1/speech:recognize"
;;          method :POST]
;;      {:db   (assoc-in db [:http-reqs :speech-to-text]
;;                       {:status "running"
;;                        :url url
;;                        :started (ut/get-time-format-str)
;;                        :start-unix (.getTime (js/Date.))})
;;       :http-xhrio {:method          method
;;                    :uri             url
;;                    :params {:config {:languageCode "en-US"}
;;                             :audio {:content (str (get-in db [:audio-data-recorded :base64]))}}
;;                    :timeout  280000   ;; should move to websockets or message queue
;;                    :headers {"Authorization" (str "Bearer " google-token)}
;;                    :format          (ajax/json-request-format {:keywords? true})
;;                    :response-format (ajax/json-response-format {:keywords? true})
;;                    :on-success      [::success-speech-to-text]
;;                    :on-failure      [::failure-speech-to-text]}})))

;; (defn start-webcam-stream [target-element-id]
;;   (let [video-element (dom/getElement target-element-id)]
;;     (.getUserMedia (.-mediaDevices js/navigator) #js {:video true :audio false}
;;                    (fn [stream]
;;                      (set! (.-srcObject video-element) stream))
;;                    (fn [err]
;;                      (js/console.log (str "Error accessing the webcam:" err))))))

;; (defn stop-webcam-stream [target-element-id]
;;   (let [video-element (dom/getElement target-element-id)
;;         stream (.-srcObject video-element)]
;;     (when stream
;;       (doseq [track (.-getTracks stream)]
;;         (.stop track))
;;       (set! (.-srcObject video-element) nil))))

(re-frame/reg-event-db
 ::save-webcam-stream
 (fn [db [_ stream]]
   (ut/tapp>> [:stream-started!])
   (assoc db :webcam-feed stream)))

(defn get-webcam-stream []
  (-> (.-mediaDevices js/navigator)
      (.getUserMedia #js {:video true :audio false})))

(re-frame/reg-fx
 ::start-webcam
 (fn []
   (let [stream-promise (get-webcam-stream)]
     (ut/tapp>> [:starting-webcam])
     (.then stream-promise
            (fn [stream]
              ;; Process the stream here. For example, you can dispatch an event to save it in the app-db.
              (ut/tracked-dispatch [::save-webcam-stream stream]))
            (fn [err]
              (js/console.log "Error accessing the webcam:" err))))))

;; (re-frame/reg-event-db
;;  ::stop-webcam
;;  (fn [db [_]]
;;    (let [stream (get-in db [:webcam-feed])]
;;      (when stream
;;        (doseq [track (.-getTracks stream)]
;;          (.stop track)))
;;      (assoc-in db [:webcam-feed] nil))))

(re-frame/reg-event-db
 ::stop-webcam
 (fn [db [_]]
   (let [stream (get-in db [:webcam-feed])]
     (when stream
       (doseq [track (array-seq (.getTracks stream))]
         (.stop track)))
     (assoc-in db [:webcam-feed] nil))))



;; (defn stop-webcam [recorder]
;;   (ut/tapp>> ["stop-recording-fn - state:" (.-state recorder)])
;;   (let [stream (.-stream recorder)]
;;     (doseq [track (.getTracks stream)]
;;       (.stop track)))
;;   (.stop recorder))

;; (re-frame/reg-event-db
;;  ::stop-webcam
;;  (fn [db [_]]
;;    (let [stream (get-in db [:webcam-feed])]
;;      (ut/tapp>> [:stopping-webcam])
;;      (when stream
;;        (doseq [track (.-getTracks stream)]
;;          (.stop track)))
;;      (dissoc db :webcam-feed))))


(re-frame/reg-event-fx
 ::start-webcam
 (fn [_ _]
   {::start-webcam nil}))






;; (defn start-webcam-stream []
;;   (.getUserMedia (.-mediaDevices js/navigator) #js {:video true :audio false}
;;                  (fn [stream]
;;                    (ut/tracked-dispatch [:save-webcam-stream stream]))
;;                  (fn [err]
;;                    (js/console.error "Error accessing the webcam:" err))))

;; (start-webcam-stream )

;; (defn get-audio-stream []
;;   (-> (.-mediaDevices js/navigator)
;;       (.getUserMedia #js {:audio true})))

;; (re-frame/reg-fx
;;  ::start-recording
;;  (fn []
;;    (let [stream-promise (get-audio-stream)
;;          face-block-id "face"] ;; @(ut/tracked-subscribe [::face-block @db/hovered-block])

;;      (reset! listening-block face-block-id)
;;      (.then stream-promise
;;             (fn [stream]
;;               (let [recorder (create-media-recorder stream)]
;;                 (on-data-available recorder
;;                                    (fn [audio-data]
;;                                      (let [form-data (create-form-data audio-data)]
;;                                        (ut/tapp>> [:call-back-data-inner (count (str form-data))]))))
;;                       ;; you now have a FormData object that you can send to the Google Speech-to-Text API
;;                       ;; use your existing HTTP request code to send this data

;;                 (ut/tracked-dispatch [::recorder-created recorder])
;;                 (start-recording recorder)))))))


(re-frame/reg-event-db
 ::chat-response
 (fn [db [_ kp result]]
   (reset! waiting-for-response (vec (remove #(= % kp) @waiting-for-response)))
   (let [chat-text (get-in result [:convo :choices 0 :message :content])
         new-chats (conj (get-in db [:chats kp] []) {:content chat-text :role "assistant" :timestamp (ut/get-time-format-str)})]
   ;;  (ut/tracked-dispatch [::text-to-speech11 :audio :speak chat-text])
     ;(assoc-in db [:post-meta ui-keypath field name] new-map)
     ;(ut/dissoc-in db [:query-history])
     (ut/tapp>> [:chat-response result])
     (assoc-in db [:chats kp] new-chats))))

(re-frame/reg-event-db
 ::chat-timeout-response
 (fn [db [_  kp result]]
   (let []
     (reset! waiting-for-response (vec (remove #(= % kp) @waiting-for-response)))
     (ut/tapp>> [:chat-timeout! result kp])
     db)))

(defn push-oai-message [convo-vec client-name panels panel-key kp]
  ;(reset! waiting-for-response true)
  (swap! waiting-for-response conj kp)
  (ut/tracked-dispatch [::wfx/request :default
                      {:message    {:kind :open-ai-push
                                    :client-name client-name
                                    :panel-key panel-key
                                    :query-key 0
                                    :panels panels
                                    :convo convo-vec}
                       :on-response [::chat-response kp]
                       :on-timeout  [::chat-timeout-response kp]
                       :timeout    500000}]))

(re-frame/reg-event-db
 ::add-chat
 (fn [db [_ chat-text kp]]
   (let [new-chats (conj (get-in db [:chats kp] []) {:content chat-text :role "user" :timestamp (ut/get-time-format-str)})]
     (push-oai-message new-chats (get db :client-name) (get db :panels) (get db :selected-block) kp)
     (assoc-in db [:chats kp] new-chats))))

(re-frame/reg-event-db
 ::clear-chat
 (fn [db [_ kp]]
   (assoc-in db [:chats kp] [])))

(re-frame/reg-event-db
 ::failure-speech-to-text
 (fn [db [_ result]]
   (let [old-status (get-in db [:http-reqs :speech-to-text])]
     (assoc-in db [:http-reqs :speech-to-text]
               (merge old-status
                      {:status "failed"
                       :ended (ut/get-time-format-str)
                       :ended-unix (.getTime (js/Date.))
                       :message result})))))

(defn invert-map [m]
  (reduce (fn [acc [k v]]
            (update acc v
                    (fn [existing]
                      (if existing
                        (conj existing k)
                        [k]))))
          {} m))

;; (re-frame/reg-sub
;;  ::rs-value ;; duped in AUDIO TOO, OMG please condense !!!!!
;;  (fn [db [_ flow-id kkey]]  ;;; has a dupe in bricks!!! TODO, please unify LOGIC!!!!
;;    (let [src (get-in db [:runstreams flow-id :values kkey :source])]
;;      (if (= src :param)
;;        ;(get-in db [:runstreams flow-id :values kkey :value])
;;        (first @(ut/tracked-subscribe [::resolver/logic-and-params [(get-in db [:runstreams flow-id :values kkey :value])]]))
;;        (get-in db [:runstreams flow-id :values kkey :value])))))

(re-frame/reg-sub
 ::rs-value
 (fn [db {:keys [flow-id kkey]}]  ;;; dupe from buffy
   (let [src (get-in db [:runstreams flow-id :values kkey :source])]
     (if (= src :param)
       ;(get-in db [:runstreams flow-id :values kkey :value])
       (let [vvv @(ut/tracked-sub ::resolver/logic-and-params {:m [(get-in db [:runstreams flow-id :values kkey :value])]})
             vv (try
                  (first
                        ;;@(ut/tracked-subscribe [::resolver/logic-and-params [(get-in db [:runstreams flow-id :values kkey :value])]])
                   vvv ;;@(rfa/sub ::resolver/logic-and-params {:m [(get-in db [:runstreams flow-id :values kkey :value])]})
                        ;;; ^^^^ :brave-red-butterfly is not sequable? what? so I wrapped in a vec. started after caching madness....
                   ) (catch :default e (do (ut/tapp>> [:rs-value-fuck-up-audio vvv flow-id kkey src e]) vvv)))]
         ;(ut/tapp>> [:sketchy-rs-value? flow-id kkey vvv vv])
         vv)
       (get-in db [:runstreams flow-id :values kkey :value])))))

(re-frame/reg-sub
 ::runstream-overrides
 (fn [db [_ flow-id]]
   (let [values (get-in db [:runstreams flow-id :values])
         override-map (into {} (for [[k {:keys [value source]}] values
                                     :when (not (nil? value))]
                                 {k (if (= source :input)
                                      value
                                      (let [vv @(ut/tracked-subscribe [::rs-value flow-id k])] ;;; dupe from buffy
                                        (if (and (vector? vv) (every? string? vv))
                                          (cstr/join "\n" vv) vv)))}))]
     ;(ut/tapp>> [:override-map override-map])
     override-map)))

(re-frame/reg-sub
 ::client-name
 (fn [db _]
   (get db :client-name)))

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

(defn start-flow [flow-id]
  (let [client-name @(ut/tracked-subscribe [::client-name])
        base-opts {:increment-id? false}
        overrides @(ut/tracked-subscribe [::runstream-overrides flow-id])
        overrides? (not (empty? overrides))
        fstr (str "running flow " flow-id (when overrides? " (with overrides)"))
        w (/ (count fstr) 4.1)]
    (ut/tracked-dispatch [::wfx/request :default
                        {:message    {:kind :run-flow
                                      :flow-id flow-id
                                      :flowmap flow-id
                                      :opts (if (map? overrides)
                                              (merge base-opts
                                                     {:overrides overrides})
                                              base-opts)
                                      ;; :file-image {:flowmaps @(ut/tracked-subscribe [::flowmap-raw])
                                      ;;              :opts @(ut/tracked-subscribe [::opts-map])
                                      ;;              :zoom @db/pan-zoom-offsets
                                      ;;              :flow-id flow-id
                                      ;;              :flowmaps-connections @(ut/tracked-subscribe [::flowmap-connections])}
                                      :client-name client-name}
                         :on-response [::http/socket-response]
                         :on-timeout [::http/timeout-response]
                         :timeout    500000}])
    (ut/dispatch-delay 800 [::http/insert-alert fstr w 1 5])
    ;; (ut/tracked-dispatch [::conn/click-parameter ;; kinda cheating, but feels better
    ;;                     [:flow (keyword (str flow-id ">*running?"))] true])
    ))

(re-frame/reg-event-db
 ::success-speech-to-text
 (fn [db [_  result]]
   (let [old-status (get-in db [:http-reqs :speech-to-text])
         ;phrases (count (get result :results))
         client-name (get db :client-name)
         lines (get-in result [:text])
        ; res (conj (get db :audio-transcribed) lines)
        ; resv (vec (remove empty? (conj (vec (get db :audio-transcribed-vec [])) lines)))
         sshouts (get db :shouts)
         shouts (invert-map sshouts)
         shout-flows-to-run (vec (flatten (for [k (keys shouts)
                                                :let [k (ut/remove-punctuation (cstr/lower-case k))
                                                      lines (ut/remove-punctuation (cstr/lower-case lines))
                                                           ;_ (ut/tapp>> [:k k lines])
                                                      ]
                                                :when (and (and (not (empty? (cstr/trim k))) (not (empty? (cstr/trim lines))))
                                                           (cstr/starts-with? lines (str k)))]
                                            (get shouts k))))]
     (ut/tapp>> [:voice-trigger lines shout-flows-to-run])
     (doseq [flow-id shout-flows-to-run]
       (let [txt (str "shout trigger '" (get sshouts flow-id) "': " flow-id)
             cnt (js/Math.ceil (/ (count txt) 5))]
         (try (start-flow flow-id) (catch :default e (ut/tapp>> [:error flow-id e])))
         (ut/dispatch-delay 400 [::http/insert-alert [:box :child txt] cnt 1 6])))

     ;;(ut/tracked-dispatch [::add-chat (str lines)]) ;; sends to oai as a chat content resp
     (ut/tracked-dispatch [::wfx/request :default
                         {:message    {:kind :voice-trigger
                                       :client-name client-name
                                       :voice-text lines}
                          ;:on-response [::chat-response kp]
                          ;:on-timeout  [::chat-timeout-response kp]
                          :timeout    500000}])
     (-> db
         (assoc-in [:incoming-voice] lines)
         (assoc-in [:click-param :param :voice] lines)
         (assoc-in [:http-reqs :speech-to-text]
                   (merge old-status
                          {:status "success"
                           :ended (ut/get-time-format-str)
                           :ended-unix (.getTime (js/Date.))
                           :message result}))))))

(re-frame/reg-event-fx
 ::speech-to-text
 (fn [{:keys [db]} _]
   (let [url "https://api.openai.com/v1/audio/transcriptions"
         openapi-key (get db :openai-api-key)
         openapi-org-id (get db :openai-org-id)
         headers (merge {"Authorization" (str "Bearer " openapi-key)}
                        (if (not (nil? openapi-org-id))
                          {"OpenAI-Organization" openapi-org-id} {}))
         blob (get-in db [:audio-data-recorded :blob])
         audio-file (js/File. [(clj->js blob)] "audio.wav" {"type" "audio/wav"})
         form-data (let [data (js/FormData.)]
                     (.append data "file" audio-file)
                     (.append data "model" "whisper-1")
                     data)
         auto-train-voice? (get db :auto-train-voice? false)
         method :POST]

     (when auto-train-voice?
       (ut/tracked-dispatch [::edit-voice "me" "me"]))

     {:db   (assoc-in db [:http-reqs :speech-to-text]
                      {:status "running"
                       :url url
                       :started (ut/get-time-format-str)
                       :start-unix (.getTime (js/Date.))})
      :http-xhrio {:method          method
                   :uri             url
                   :body form-data
                   ;:params {:file (str (get-in db [:audio-data-recorded :blob]))
                   ;         ;:prompt "" ;; "style guide"
                   ;         :model "whisper-1"}
                   :timeout  280000   ;; should move to websockets or message queue
                   :headers headers
                   ;:format          (ajax/json-request-format {:keywords? true})
                   :response-format (ajax/json-response-format {:keywords? true})
                   :on-success      [::success-speech-to-text]
                   :on-failure      [::failure-speech-to-text]}})))

(defn get-audio-stream []
  (-> (.-mediaDevices js/navigator)
      (.getUserMedia #js {:audio true})))

(defn create-media-recorder [stream]
  (js/MediaRecorder. stream))

(defn start-recording [recorder]
  (ut/tapp>> ["start-recording-fn - state:" (.-state recorder)])
  (.start recorder))

;; (defn stop-recording [recorder]
;;   (ut/tapp>> ["stop-recording-fn - state:" (.-state recorder)])
;;   (.stop recorder))

(defn stop-recording [recorder]
  (ut/tapp>> ["stop-recording-fn - state:" (.-state recorder)])
  (let [stream (.-stream recorder)]
    (doseq [track (.getTracks stream)]
      (.stop track)))
  (.stop recorder))

(defn create-form-data [blob]
  (let [form-data (js/FormData.)]
    (.append form-data "audio" blob)
    form-data))

(re-frame/reg-event-db
 ::audio-started
 (fn [db _]
   (assoc db :audio-playing? true)))

(re-frame/reg-event-db
 ::audio-ended
 (fn [db _]
   (assoc db :audio-playing? false)))

(re-frame/reg-sub
 ::audio-playing?
 (fn [db _]
   (get db :audio-playing? false)))

(re-frame/reg-sub
 ::get-voice-id
 (fn [db [_ voice-name]]
   (let [voice-map (into {}
                         (for [{:keys [name voice_id]} (get-in db [:http-reqs "elevenlabs-test" "elevenlabs-test" :message :voices])]
                           {name voice_id}))]
     (get voice-map voice-name))))

;; (defn play-audio-blob [audio-data]
;;   (ut/tracked-dispatch [::audio-started])
;;   (let [url (.createObjectURL js/URL audio-data)
;;         audio (js/document.createElement "audio")]
;;     (set! (.-src audio) url)
;;     (.addEventListener audio "ended"
;;                        (fn [e] (ut/tracked-dispatch [::audio-ended])))
;;     (.play audio)))

(def playing? (reagent.core/atom false)) ;; only used here for perf reasons instead of subs

;(defonce audio-context (js/AudioContext.))
;; @(re-frame/subscription [::audio-playing?])

(defn draw [analyser block-id]
  (let [buffer-length (.-frequencyBinCount analyser)
        data-array (js/Uint8Array. buffer-length)]
    ;(ut/tapp>> [:rms? @playing?])
    (letfn [(frame []
              (do
                ;(ut/tapp>> [:audio-playing? @playing?])
                (when @playing?
                  (.getByteTimeDomainData analyser data-array)
                  (let [rms (->> (range buffer-length)
                                 (map #(- (aget data-array %) 128))
                                 (map #(* % %))
                                 (reduce +)
                                 (/ buffer-length)
                                 (js/Math.sqrt))
                                 ;(fn [x] (if (js/isFinite x) x 0))

                        rms (if (js/isFinite rms) rms 0)
                       ; zero-crossings (->> (range (dec buffer-length))
                       ;                     (filter #(let [cur (aget data-array %)
                       ;                                    next (aget data-array (inc %))]
                       ;                                (and (> cur 128) (<= next 128))))
                       ;                     (count))
                        peak (->> (range buffer-length)
                                  (map #(js/Math.abs (- (aget data-array %) 128)))
                                  (reduce max))]
                    ;(ut/tapp>> [:RMS-data rms])
                    ;(when (js/isNaN rms)
                    ;  (ut/tapp>> [:problematic-data-array data-array]))
                    (swap! db/audio-data2 assoc block-id peak)
                    (swap! db/audio-data assoc block-id rms))
                  (js/requestAnimationFrame frame))))]
      ;(ut/tapp>> [:frame-called])
      (frame))))

(defn play-audio-blob [audio-data block-id]
  (reset! playing? true)
  (ut/tracked-dispatch [::audio-started])
  (reset! db/speaking block-id)
  ;(ut/tapp>> audio-data)
  (let [audio-context (js/AudioContext.)
        url (js/URL.createObjectURL audio-data)
        audio-element (js/document.createElement "audio")
        source-node (.createMediaElementSource audio-context audio-element)
        analyser (.createAnalyser audio-context)]
    (set! (.-src audio-element) url)
    (.addEventListener audio-element "ended" (fn [e] (do (ut/tracked-dispatch [::audio-ended])
                                                         (swap! db/audio-data dissoc block-id)
                                                         (swap! db/audio-data2 dissoc block-id)
                                                         (reset! playing? false))))
    (set! (.-fftSize analyser) 2048)
    (.connect source-node analyser)
    (.connect analyser (.-destination audio-context))
    (.play audio-element)
    (draw analyser block-id)))

(re-frame/reg-event-db
 ::save-recording-data
 (fn [db [_ audio-map]]
   (assoc db :audio-data-recorded audio-map)))

(defn blob-to-base64 [blob]
  (-> (blob-util/blobToBase64String blob)
      (.then (fn [base64-string]
               (ut/tracked-dispatch [::save-recording-data {:base64 base64-string :blob blob}])
               (ut/tracked-dispatch [::speech-to-text])))))

(defn on-data-available [recorder callback]
  (.addEventListener recorder "dataavailable"
                     (fn [event]
                       (let [audio-data (.-data event)
                             form-data (create-form-data audio-data)]
                         ;;(play-audio-blob audio-data "echo") ;; uncomment to hear the echo
                         (ut/tapp>> [:audio-data-received-callback (count (str form-data))])
                         (blob-to-base64 audio-data)
                         (ut/tracked-dispatch [::save-recording-data {:form form-data :raw audio-data}])))))
        ;; you now have a FormData object that you can send to the Google Speech-to-Text API
        ;; use your existing HTTP request code to send this data

(re-frame/reg-event-db
 ::recorder-created
 (fn [db [_ recorder]]
   (assoc db :recorder recorder)))

(re-frame/reg-event-fx
 ::start-recording
 (fn [_ _]
   {::start-recording nil}))

(re-frame/reg-sub
 ::kits ;; dupe from canvas
 (fn [db [_ block-id]]
   (vec (remove nil?
                (for [[k v] (get db :blocks)]
                  (when (= (get v :kit-parent) block-id)
                    k))))))

(re-frame/reg-sub
 ::face-block ;; modded dupe from canvas
 (fn [db [_ block-id]]
   (if (= (get-in db [:blocks block-id :block-type]) "kit")
     (let [kits @(ut/tracked-subscribe [::kits block-id])
           fb-id (get-in db [:blocks block-id :face-block-id] (first kits))]
       fb-id)
     block-id)))

(re-frame/reg-fx
 ::start-recording
 (fn []
   (let [stream-promise (get-audio-stream)
         face-block-id "face"] ;; @(ut/tracked-subscribe [::face-block @db/hovered-block])

     (reset! listening-block face-block-id)
     (.then stream-promise
            (fn [stream]
              (let [recorder (create-media-recorder stream)]
                (on-data-available recorder
                                   (fn [audio-data]
                                     (let [form-data (create-form-data audio-data)]
                                       (ut/tapp>> [:call-back-data-inner (count (str form-data))]))))
                      ;; you now have a FormData object that you can send to the Google Speech-to-Text API
                      ;; use your existing HTTP request code to send this data

                (ut/tracked-dispatch [::recorder-created recorder])
                (start-recording recorder)))))))

;; (re-frame/reg-event-db
;;  ::start-recording
;;  (fn [db _]
;;    (let [recorder (start-recording-stream)]
;;      (ut/tapp>> ["Attempting to start recording..."])
;;      (ut/tapp>> ["Current state:" (.-state recorder)])
;;      (assoc db :recorder recorder))))

(re-frame/reg-event-db
 ::stop-recording
 (fn [db _]
   (let [recorder (:recorder db)] ;(or (:recorder db) (:webcam-feed db))

     (ut/tapp>> ["Attempting to stop recording..."])
     (ut/tapp>> ["state:" (.-state recorder)])
     (.stop recorder)
     (stop-recording recorder)
     (-> db
         ;(dissoc :webcam-feed)
         (dissoc :recorder)))))

(re-frame/reg-sub
 ::latest-audio
 (fn [db _]
   (get db :thumper-speaks)))

(defn speak-last []
  (let [audio-encoded @(ut/tracked-subscribe [::latest-audio])
        audio-blob (ut/base64-to-blob audio-encoded "audio/mpeg")]
    (play-audio-blob audio-blob "?")))

;; (re-frame/reg-event-db
;;  ::failure-text-to-speech
;;  (fn [db [_ block-type block-id result]]
;;    (let [old-status (get-in db [:http-reqs block-type block-id])]
;;      (assoc-in db [:http-reqs block-type block-id]
;;                (merge old-status
;;                       {:status "failed"
;;                        :ended (ut/get-time-format-str)
;;                        :ended-unix (.getTime (js/Date.))
;;                        :message result})))))

;; (re-frame/reg-event-db
;;  ::success-text-to-speech
;;  (fn [db [_ block-type block-id result]]
;;    (let [old-status (get-in db [:http-reqs block-type block-id])
;;          ;phrases (count (get result :results))
;;          ]
;;      (play-audio-blob (ut/base64-to-blob (get result :audioContent) "audio/mpeg") block-id)
;;      (reset! db/speaking block-id)
;;      (-> db
;;          (assoc :thumper-speaks (get result :audioContent))
;;          (assoc-in [:http-reqs block-type block-id]
;;                    (merge old-status
;;                           {:status "success"
;;                            :ended (ut/get-time-format-str)
;;                            :ended-unix (.getTime (js/Date.))
;;                            :message result}))))))

;; (re-frame/reg-event-fx
;;  ::text-to-speech
;;  (fn [{:keys [db]} [_ block-id block-type text-to-speak]] ;;; get new code: https://developers.google.com/oauthplayground/?code=4/0AbUR2VMRzydaQdfs1EtgukPwIItaoOyZyYHRSReQ0HuqlJj-UsSnTgzTXyXwhGhe-cbR-A&scope=https://www.googleapis.com/auth/cloud-platform
;;    (let [;google-key google-token
;;          url "https://texttospeech.googleapis.com/v1/text:synthesize"
;;          method :POST
;;          voice-name (get-in db [:blocks block-id :req :voice :name] "en-US-Neural2-F")
;;          studio? (true? (cstr/includes? voice-name "Studio"))
;;          voice-sex (get-in db [:blocks block-id :req :voice :ssmlGender] "FEMALE")
;;          ssml-text (if studio? text-to-speak
;;                      (str "<speak>" text-to-speak "</speak>"))]
;;      {:db   (assoc-in db [:http-reqs block-type block-id]
;;                       {:status "running"
;;                        :url url
;;                        :started (ut/get-time-format-str)
;;                        :start-unix (.getTime (js/Date.))})
;;       :http-xhrio {:method          method
;;                    :uri             url
;;                    :params {:input (if studio? {:text ssml-text}
;;                                        {:ssml ssml-text }) ; "Happy birthday, Mr. president! I'm so happy I could attend."}
;;                             :voice {:languageCode "en-US"
;;                                     :name voice-name ;"en-US-Neural2-F" ;; "en-US-Wavenet-H" ; "en-US-Studio-O" ; "en-US-Standard-A" ;; en-US-Neural2-F    en-US-Studio-O    en-US-Wavenet-H
;;                                     ;:name "en-US-Studio-O"
;;                                     :ssmlGender voice-sex ;"FEMALE"
;;                                     }
;;                             :audioConfig {:audioEncoding "MP3"}}
;;                    :timeout  280000   ;; should move to websockets or message queue
;;                    :headers {"Authorization" (str "Bearer " google-token)
;;                              ;"Accept" "application/vnd.github+json"
;;                              ;"X-GitHub-Api-Version" "2022-11-28"
;;                              }
;;                    :format          (ajax/json-request-format {:keywords? true})
;;                    :response-format (ajax/json-response-format {:keywords? true})
;;                    :on-success      [::success-text-to-speech block-type block-id]
;;                    :on-failure      [::failure-text-to-speech block-type block-id]}})))

(re-frame/reg-event-db
 ::failure-text-to-speech11
 (fn [db [_ block-type block-id result]]
   (let [old-status (get-in db [:http-reqs block-type block-id])]
     (assoc-in db [:http-reqs block-type block-id]
               (merge old-status
                      {:status "failed"
                       :ended (ut/get-time-format-str)
                       :ended-unix (.getTime (js/Date.))
                       :message result})))))

(re-frame/reg-event-db
 ::success-text-to-speech11
 (fn [db [_ block-type block-id voices? result]]
   (ut/tapp>> [:speak? block-type block-id voices?])
   (if voices?
     (let [old-status (get-in db [:http-reqs block-type block-id])]
       (-> db
           (assoc-in [:http-reqs block-type block-id]
                     (merge old-status
                            {:status "success"
                             :ended (ut/get-time-format-str)
                             :ended-unix (.getTime (js/Date.))
                             :message result}))))
     (let [old-status (get-in db [:http-reqs block-type block-id])
           byte-nums  result  ;; This is already a vector of byte values
           byte-array (js/Uint8Array. byte-nums)
           blob       (js/Blob. #js [byte-array] #js {:type "audio/mpeg"})]
         ;url        (js/URL.createObjectURL blob)

     ;(ut/tapp>> (type result))
       (play-audio-blob blob block-id)
     ;(swap! db/audio-blob-queue assoc block-id (vec (conj (get @db/audio-blob-queue block-id []) blob)))
       (-> db
         ;(assoc :current-audio url)
           (assoc-in [:http-reqs block-type block-id]
                     (merge old-status
                            {:status "success"
                             :ended (ut/get-time-format-str)
                             :ended-unix (.getTime (js/Date.))
                             :message {:audio-blob blob}})))))))

(re-frame/reg-event-fx
 ::text-to-speech11
 (fn [{:keys [db]} [_ block-id block-type text-to-speak & [audio-file?]]]
   ;; only run if API key? (if (get db :elevenlabs-api-key) ... {:db db})
   (let [voices? (nil? text-to-speak)
         xi-api-key (get db :elevenlabs-api-key)
         method (if voices? :GET :POST)
         voice-key (get-in db [:server :settings :eleven-labs-default-voice-name] "Not OG Buffy") ;; "Not OG Buffy"
         vname (get-in db [:blocks block-id :req :voice_name] voice-key)
         vid (first (remove nil? (for [v (get-in db [:http-reqs :elevenlabs :audio :message :voices])]
                                   (when (= (get v :name) vname) (get v :voice_id)))))
         ;vid "ecjGKdrYybHS1XMO2PWR"
         url (if voices?
               "https://api.elevenlabs.io/v1/voices" ;; just get voice map
               (str "https://api.elevenlabs.io/v1/text-to-speech/" vid)) ;; "/stream"
         aturl (let [protocol (.-protocol js/window.location)
                     host (.-host js/window.location)
                     host-without-port (clojure.string/replace host #":\d+$" "")
                     new-port "8888"]
                 (str protocol "//" host-without-port ":" new-port "/audio"))
         url (if audio-file?
               aturl ;;"http://localhost:8888/audio"
               url)
         header {"xi-api-key" xi-api-key}
         data (if audio-file?
                {:path text-to-speak}
                (if voices? {}
                    (dissoc (assoc (get-in db [:blocks block-id :req]) :text text-to-speak) :speak)))]
     (ut/tapp>> [method url data voices? vname vid])
     {:db   (assoc-in db [:http-reqs block-type block-id]
                      {:status "running"
                       :url url
                       :started (ut/get-time-format-str)
                       :start-unix (.getTime (js/Date.))})
      :http-xhrio {:method          method
                   :uri             url
                   :params          data
                   :timeout         280000
                   :headers         header
                   :format          (ajax/json-request-format {:keywords? true})
                   :response-format (if voices?
                                      (ajax/json-response-format {:keywords? true})
                                      {:read pr/-body :description "rawww" :type :arraybuffer :content-type "audio/mpeg"})
                   :on-success      [::success-text-to-speech11 block-type block-id voices?]
                   :on-failure      [::failure-text-to-speech11 block-type block-id]}})))

;(ut/tracked-dispatch [::text-to-speech11 :audio :elevenlabs nil]) ;; get voices if avail
;(ut/tracked-dispatch [::text-to-speech11 :audio :speak "lets gets some data, y'all!! "])

;; (re-frame/reg-event-db
;;  ::failure-get-voices11
;;  (fn [db [_ block-type block-id result]]
;;    (let [old-status (get-in db [:http-reqs block-type block-id])]
;;      (assoc-in db [:http-reqs block-type block-id]
;;                (merge old-status
;;                       {:status "failed"
;;                        :ended (ut/get-time-format-str)
;;                        :ended-unix (.getTime (js/Date.))
;;                        :message result})))))

;; (re-frame/reg-event-db
;;  ::success-get-voices11
;;  (fn [db [_ block-type block-id result]]
;;    (let [old-status (get-in db [:http-reqs block-type block-id])
;;          ;phrases (count (get result :results))
;;          ]
;;      (play-audio-blob (ut/base64-to-blob (get result :audioContent) "audio/mpeg") block-id)
;;      (reset! db/speaking block-id)
;;      (-> db
;;          (assoc :thumper-speaks (get result :audioContent))
;;          (assoc-in [:http-reqs block-type block-id]
;;                    (merge old-status
;;                           {:status "success"
;;                            :ended (ut/get-time-format-str)
;;                            :ended-unix (.getTime (js/Date.))
;;                            :message result}))))))

;; (re-frame/reg-event-fx
;;  ::get-voices11
;;  (fn [{:keys [db]} [_ block-id block-type text-to-speak]] ;;; get new code: https://developers.google.com/oauthplayground/?code=4/0AbUR2VMRzydaQdfs1EtgukPwIItaoOyZyYHRSReQ0HuqlJj-UsSnTgzTXyXwhGhe-cbR-A&scope=https://www.googleapis.com/auth/cloud-platform
;;    (let [;google-key google-token
;;          url "https://texttospeech.googleapis.com/v1/text:synthesize"
;;          method :POST
;;          voice-name (get-in db [:blocks block-id :req :voice :name] "en-US-Neural2-F")
;;          studio? (true? (cstr/includes? voice-name "Studio"))
;;          voice-sex (get-in db [:blocks block-id :req :voice :ssmlGender] "FEMALE")
;;          ssml-text (if studio? text-to-speak
;;                        (str "<speak>" text-to-speak "</speak>"))]
;;      {:db   (assoc-in db [:http-reqs block-type block-id]
;;                       {:status "running"
;;                        :url url
;;                        :started (ut/get-time-format-str)
;;                        :start-unix (.getTime (js/Date.))})
;;       :http-xhrio {:method          method
;;                    :uri             url
;;                    :params {:input (if studio? {:text ssml-text}
;;                                        {:ssml ssml-text}) ; "Happy birthday, Mr. president! I'm so happy I could attend."}
;;                             :voice {:languageCode "en-US"
;;                                     :name voice-name ;"en-US-Neural2-F" ;; "en-US-Wavenet-H" ; "en-US-Studio-O" ; "en-US-Standard-A" ;; en-US-Neural2-F    en-US-Studio-O    en-US-Wavenet-H
;;                                     ;:name "en-US-Studio-O"
;;                                     :ssmlGender voice-sex ;"FEMALE"
;;                                     }
;;                             :audioConfig {:audioEncoding "MP3"}}
;;                    :timeout  280000   ;; should move to websockets or message queue
;;                    :headers {"Authorization" (str "Bearer " google-token)
;;                              ;"Accept" "application/vnd.github+json"
;;                              ;"X-GitHub-Api-Version" "2022-11-28"
;;                              }
;;                    :format          (ajax/json-request-format {:keywords? true})
;;                    :response-format (ajax/json-response-format {:keywords? true})
;;                    :on-success      [::success-get-voices11 block-type block-id]
;;                    :on-failure      [::failure-get-voices11 block-type block-id]}})))

(re-frame/reg-event-db
 ::failure-edit-voice
 (fn [db [_ block-type block-id result]]
   (let [old-status (get-in db [:http-reqs block-type block-id])]
     (assoc-in db [:http-reqs block-type block-id]
               (merge old-status
                      {:status "failed"
                       :ended (ut/get-time-format-str)
                       :ended-unix (.getTime (js/Date.))
                       :message result})))))

(re-frame/reg-event-db
 ::success-edit-voice
 (fn [db [_ block-type block-id result]]
   (let [old-status (get-in db [:http-reqs block-type block-id])]
     (-> db
         (assoc-in [:http-reqs block-type block-id]
                   (merge old-status
                          {:status "success"
                           :ended (ut/get-time-format-str)
                           :ended-unix (.getTime (js/Date.))
                           :message result}))))))

(re-frame/reg-event-fx
 ::edit-voice
 (fn [{:keys [db]} [_ block-id block-type]]
   (let [;vid "AxgZF8qyZHiePwKPgREq"
         voice-name (get db :self-voice-name)
         vid @(ut/tracked-subscribe [::get-voice-id voice-name])
         new? (nil? vid)
         url (if new?
               (str "https://api.elevenlabs.io/v1/voices/add")
               (str "https://api.elevenlabs.io/v1/voices/" vid "/edit"))
         ;url (str "https://api.elevenlabs.io/v1/voices/add")
         xi-api-key (get db :elevenlabs-api-key)
         method :POST
         header {"xi-api-key" xi-api-key}
         blob (get-in db [:audio-data-recorded :blob])
         audio-file (js/File. [(clj->js blob)] "audio.wav" {"type" "audio/wav"}) ;; Name your file as per requirement
         form-data (if new?
                     (let [data (js/FormData.)]
                       (.append data "files" audio-file)
                       ;(.append data "voice_id" vid)
                       (.append data "name" voice-name)
                       data)
                     (let [data (js/FormData.)]
                       (.append data "files" audio-file)
                       (.append data "voice_id" vid)
                       (.append data "name" voice-name)
                       data))]
     (ut/tapp>> [:training-voice voice-name :is-new? new?])
     {:db   (assoc-in db [:http-reqs block-type block-id]
                      {:status "running"
                       :url url
                       :started (ut/get-time-format-str)
                       :start-unix (.getTime (js/Date.))})
      :http-xhrio {:method          method
                   :uri             url
                   :body            form-data
                   :timeout         280000
                   :headers         header
                   :format          (ajax/json-request-format {:keywords? true})
                   :response-format (ajax/json-response-format {:keywords? true})
                   :on-success      [::success-edit-voice block-type block-id]
                   :on-failure      [::failure-edit-voice block-type block-id]}})))