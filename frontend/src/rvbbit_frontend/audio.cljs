(ns rvbbit-frontend.audio
  (:require
    ["blob-util"             :as blob-util]
    [ajax.core               :as ajax]
    [ajax.protocols          :as pr]
    [clojure.string          :as cstr]
    [clojure.walk            :as walk]
    [day8.re-frame.http-fx]
    [day8.re-frame.undo      :as    undo
                             :refer [undoable]]
    [goog.dom                :as dom]
    [re-frame.core           :as re-frame]
    [rvbbit-frontend.connections :as conn]
    [rvbbit-frontend.db      :as db]
    [rvbbit-frontend.http    :as http]
    [rvbbit-frontend.resolver :as resolver]
    [rvbbit-frontend.utility :as ut]
    [websocket-fx.core       :as wfx]))

(def google-token "")
(defonce listening-block (reagent.core/atom nil))

(def waiting-for-response (reagent.core/atom []))

(re-frame/reg-sub ::transcriptions
                  (fn [db _]
                    (vec (for [e (get db :audio-transcribed)] (get-in e [:alternatives])))))

(re-frame/reg-sub ::recording? (fn [db _] (not (nil? (get db :recorder)))))

(re-frame/reg-sub ::latest-transcription (fn [db _] (str (last (get db :audio-transcribed-vec)))))

(re-frame/reg-event-db
  ::chat-back ;; basically ::append-block-body w/o blocking
  (fn [db [_ block-id text]]
    (if (some #{@listening-block} (keys (get db :blocks)))
      (assoc-in db [:blocks block-id :body] (str (get-in db [:blocks block-id :body]) "\n" text))
      db)))

(re-frame/reg-sub ::self-voice-name (fn [db _] (get db :self-voice-name "(this you?)")))

(re-frame/reg-event-db ::set-self-voice-name
                       (undoable)
                       (fn [db [_ new-name]] (assoc db :self-voice-name new-name)))

(re-frame/reg-sub ::auto-train-voice? (fn [db _] (get db :auto-train-voice? false)))

(re-frame/reg-event-db ::set-auto-train-voice (fn [db [_ bool]] (assoc db :auto-train-voice? bool)))







(re-frame/reg-event-db
  ::save-webcam-stream
  (fn [db [_ stream]] (ut/tapp>> [:stream-started!]) (assoc db :webcam-feed stream)))

(defn get-webcam-stream
  []
  (-> (.-mediaDevices js/navigator)
      (.getUserMedia #js {:video true :audio false})))

(re-frame/reg-fx ::start-webcam
                 (fn []
                   (let [stream-promise (get-webcam-stream)]
                     (ut/tapp>> [:starting-webcam])
                     (.then stream-promise
                            (fn [stream] (ut/tracked-dispatch [::save-webcam-stream stream]))
                            (fn [err] (js/console.log "Error accessing the webcam:" err))))))


(re-frame/reg-event-db ::stop-webcam
                       (fn [db [_]]
                         (let [stream (get-in db [:webcam-feed])]
                           (when stream
                             (doseq [track (array-seq (.getTracks stream))] (.stop track)))
                           (assoc-in db [:webcam-feed] nil))))






(re-frame/reg-event-fx ::start-webcam (fn [_ _] {::start-webcam nil}))













(re-frame/reg-event-db
  ::chat-response
  (fn [db [_ kp result]]
    (reset! waiting-for-response (vec (remove #(= % kp) @waiting-for-response)))
    (let [chat-text (get-in result [:convo :choices 0 :message :content])
          new-chats (conj
                      (get-in db [:chats kp] [])
                      {:content chat-text :role "assistant" :timestamp (ut/get-time-format-str)})]
      (ut/tapp>> [:chat-response result])
      (assoc-in db [:chats kp] new-chats))))

(re-frame/reg-event-db ::chat-timeout-response
                       (fn [db [_ kp result]]
                         (let []
                           (reset! waiting-for-response (vec (remove #(= % kp)
                                                               @waiting-for-response)))
                           (ut/tapp>> [:chat-timeout! result kp])
                           db)))

(defn push-oai-message
  [convo-vec client-name panels panel-key kp]
  (swap! waiting-for-response conj kp)
  (ut/tracked-dispatch [::wfx/request :default
                        {:message     {:kind        :open-ai-push
                                       :client-name client-name
                                       :panel-key   panel-key
                                       :query-key   0
                                       :panels      panels
                                       :convo       convo-vec}
                         :on-response [::chat-response kp]
                         :on-timeout  [::chat-timeout-response kp]
                         :timeout     500000}]))

(re-frame/reg-event-db
  ::add-chat
  (fn [db [_ chat-text kp]]
    (let [new-chats (conj (get-in db [:chats kp] [])
                          {:content chat-text :role "user" :timestamp (ut/get-time-format-str)})]
      (push-oai-message new-chats
                        (get db :client-name)
                        (get db :panels)
                        (get db :selected-block)
                        kp)
      (assoc-in db [:chats kp] new-chats))))

(re-frame/reg-event-db ::clear-chat (fn [db [_ kp]] (assoc-in db [:chats kp] [])))

(re-frame/reg-event-db ::failure-speech-to-text
                       (fn [db [_ result]]
                         (let [old-status (get-in db [:http-reqs :speech-to-text])]
                           (assoc-in db
                             [:http-reqs :speech-to-text]
                             (merge old-status
                                    {:status     "failed"
                                     :ended      (ut/get-time-format-str)
                                     :ended-unix (.getTime (js/Date.))
                                     :message    result})))))

(defn invert-map
  [m]
  (reduce (fn [acc [k v]] (update acc v (fn [existing] (if existing (conj existing k) [k])))) {} m))


(re-frame/reg-sub
  ::rs-value
  (fn [db {:keys [flow-id kkey]}] ;;; dupe from buffy
    (let [src (get-in db [:runstreams flow-id :values kkey :source])]
      (if (= src :param)
        (let [vvv @(ut/tracked-sub ::resolver/logic-and-params
                                   {:m [(get-in db [:runstreams flow-id :values kkey :value])]})
              vv  (try (first vvv ;;@(rfa/sub ::resolver/logic-and-params {:m [(get-in db
                                  ;;[:runstreams
                       )
                       (catch :default e
                         (do (ut/tapp>> [:rs-value-fuck-up-audio vvv flow-id kkey src e]) vvv)))]
          vv)
        (get-in db [:runstreams flow-id :values kkey :value])))))

(re-frame/reg-sub ::runstream-overrides
                  (fn [db [_ flow-id]]
                    (let [values       (get-in db [:runstreams flow-id :values])
                          override-map (into {}
                                             (for [[k {:keys [value source]}] values
                                                   :when                      (not (nil? value))]
                                               {k (if (= source :input)
                                                    value
                                                    (let [vv @(ut/tracked-subscribe [::rs-value
                                                                                     flow-id k])] ;;; dupe
                                                      (if (and (vector? vv) (every? string? vv))
                                                        (cstr/join "\n" vv)
                                                        vv)))}))]
                      override-map)))

(re-frame/reg-sub ::client-name (fn [db _] (get db :client-name)))




(defn start-flow
  [flow-id]
  (let [client-name @(ut/tracked-subscribe [::client-name])
        base-opts   {:increment-id? false}
        overrides   @(ut/tracked-subscribe [::runstream-overrides flow-id])
        overrides?  (ut/ne? overrides)
        fstr        (str "running flow " flow-id (when overrides? " (with overrides)"))
        w           (/ (count fstr) 4.1)]
    (ut/tracked-dispatch [::wfx/request :default
                          {:message     {:kind        :run-flow
                                         :flow-id     flow-id
                                         :flowmap     flow-id
                                         :opts        (if (map? overrides)
                                                        (merge base-opts {:overrides overrides})
                                                        base-opts)
                                         :client-name client-name}
                           :on-response [::http/socket-response]
                           :on-timeout  [::http/timeout-response]
                           :timeout     50000000}])
    (ut/dispatch-delay 800 [::http/insert-alert fstr w 1 5])))

(re-frame/reg-event-db
  ::success-speech-to-text
  (fn [db [_ result]]
    (let [old-status         (get-in db [:http-reqs :speech-to-text])
          client-name        (get db :client-name)
          lines              (get-in result [:text])
          sshouts            (get db :shouts)
          shouts             (invert-map sshouts)
          shout-flows-to-run (vec (flatten (for [k     (keys shouts)
                                                 :let  [k     (ut/remove-punctuation
                                                                (cstr/lower-case k))
                                                        lines (ut/remove-punctuation
                                                                (cstr/lower-case lines))]
                                                 :when (and (and (ut/ne? (cstr/trim k))
                                                                 (ut/ne? (cstr/trim lines)))
                                                            (cstr/starts-with? lines (str k)))]
                                             (get shouts k))))]
      (ut/tapp>> [:voice-trigger lines shout-flows-to-run])
      (doseq [flow-id shout-flows-to-run]
        (let [txt (str "shout trigger '" (get sshouts flow-id) "': " flow-id)
              cnt (js/Math.ceil (/ (count txt) 5))]
          (try (start-flow flow-id) (catch :default e (ut/tapp>> [:error flow-id e])))
          (ut/dispatch-delay 400 [::http/insert-alert [:box :child txt] cnt 1 6])))
      (ut/tracked-dispatch
        [::wfx/request :default
         {:message {:kind :voice-trigger :client-name client-name :voice-text lines}
          :timeout 500000}])
      (-> db
          (assoc-in [:incoming-voice] lines)
          (assoc-in [:click-param :param :voice] lines)
          (assoc-in [:http-reqs :speech-to-text]
                    (merge old-status
                           {:status     "success"
                            :ended      (ut/get-time-format-str)
                            :ended-unix (.getTime (js/Date.))
                            :message    result}))))))

(re-frame/reg-event-fx
  ::speech-to-text
  (fn [{:keys [db]} _]
    (let [url               "https://api.openai.com/v1/audio/transcriptions"
          openapi-key       (get db :openai-api-key)
          openapi-org-id    (get db :openai-org-id)
          headers           (merge {"Authorization" (str "Bearer " openapi-key)}
                                   (if (not (nil? openapi-org-id))
                                     {"OpenAI-Organization" openapi-org-id}
                                     {}))
          blob              (get-in db [:audio-data-recorded :blob])
          audio-file        (js/File. [(clj->js blob)] "audio.wav" {"type" "audio/wav"})
          form-data         (let [data (js/FormData.)]
                              (.append data "file" audio-file)
                              (.append data "model" "whisper-1")
                              data)
          auto-train-voice? (get db :auto-train-voice? false)
          method            :POST]
      (when auto-train-voice? (ut/tracked-dispatch [::edit-voice "me" "me"]))
      {:db         (assoc-in db
                     [:http-reqs :speech-to-text]
                     {:status     "running"
                      :url        url
                      :started    (ut/get-time-format-str)
                      :start-unix (.getTime (js/Date.))})
       :http-xhrio {:method          method
                    :uri             url
                    :body            form-data
                    :timeout         280000 ;; should move to websockets or message queue
                    :headers         headers
                    :response-format (ajax/json-response-format {:keywords? true})
                    :on-success      [::success-speech-to-text]
                    :on-failure      [::failure-speech-to-text]}})))

(defn get-audio-stream
  []
  (-> (.-mediaDevices js/navigator)
      (.getUserMedia #js {:audio true})))

(defn create-media-recorder [stream] (js/MediaRecorder. stream))

(defn start-recording
  [recorder]
  (ut/tapp>> ["start-recording-fn - state:" (.-state recorder)])
  (.start recorder))


(defn stop-recording
  [recorder]
  (ut/tapp>> ["stop-recording-fn - state:" (.-state recorder)])
  (let [stream (.-stream recorder)] (doseq [track (.getTracks stream)] (.stop track)))
  (.stop recorder))

(defn create-form-data
  [blob]
  (let [form-data (js/FormData.)]
    (.append form-data "audio" blob)
    form-data))

(re-frame/reg-event-db ::audio-started (fn [db _] (assoc db :audio-playing? true)))

(re-frame/reg-event-db ::audio-ended (fn [db _] (assoc db :audio-playing? false)))

(re-frame/reg-sub ::audio-playing? (fn [db _] (get db :audio-playing? false)))

(re-frame/reg-sub ::get-voice-id
                  (fn [db [_ voice-name]]
                    (let [voice-map (into {}
                                          (for [{:keys [name voice_id]}
                                                  (get-in db
                                                          [:http-reqs "elevenlabs-test"
                                                           "elevenlabs-test" :message :voices])]
                                            {name voice_id}))]
                      (get voice-map voice-name))))


(def playing? (reagent.core/atom false)) ;; only used here for perf reasons instead of subs


(defn draw
  [analyser block-id]
  (let [buffer-length (.-frequencyBinCount analyser)
        data-array    (js/Uint8Array. buffer-length)]
    (letfn [(frame []
              (do (when @playing?
                    (.getByteTimeDomainData analyser data-array)
                    (let [rms  (->> (range buffer-length)
                                    (map #(- (aget data-array %) 128))
                                    (map #(* % %))
                                    (reduce +)
                                    (/ buffer-length)
                                    (js/Math.sqrt))
                          rms  (if (js/isFinite rms) rms 0)
                          peak (->> (range buffer-length)
                                    (map #(js/Math.abs (- (aget data-array %) 128)))
                                    (reduce max))]
                      (swap! db/audio-data2 assoc block-id peak)
                      (swap! db/audio-data assoc block-id rms))
                    (js/requestAnimationFrame frame))))]
      (frame))))

(defn play-audio-blob
  [audio-data block-id]
  (reset! playing? true)
  (ut/tracked-dispatch [::audio-started])
  (reset! db/speaking block-id)
  (let [audio-context (js/AudioContext.)
        url           (js/URL.createObjectURL audio-data)
        audio-element (js/document.createElement "audio")
        source-node   (.createMediaElementSource audio-context audio-element)
        analyser      (.createAnalyser audio-context)]
    (set! (.-src audio-element) url)
    (.addEventListener audio-element
                       "ended"
                       (fn [e]
                         (do (ut/tracked-dispatch [::audio-ended])
                             (swap! db/audio-data dissoc block-id)
                             (swap! db/audio-data2 dissoc block-id)
                             (reset! playing? false))))
    (set! (.-fftSize analyser) 2048)
    (.connect source-node analyser)
    (.connect analyser (.-destination audio-context))
    (.play audio-element)
    (draw analyser block-id)))

(re-frame/reg-event-db ::save-recording-data
                       (fn [db [_ audio-map]] (assoc db :audio-data-recorded audio-map)))

(defn blob-to-base64
  [blob]
  (-> (blob-util/blobToBase64String blob)
      (.then (fn [base64-string]
               (ut/tracked-dispatch [::save-recording-data {:base64 base64-string :blob blob}])
               (ut/tracked-dispatch [::speech-to-text])))))

(defn on-data-available
  [recorder callback]
  (.addEventListener recorder
                     "dataavailable"
                     (fn [event]
                       (let [audio-data (.-data event)
                             form-data  (create-form-data audio-data)]
                         (ut/tapp>> [:audio-data-received-callback (count (str form-data))])
                         (blob-to-base64 audio-data)
                         (ut/tracked-dispatch [::save-recording-data
                                               {:form form-data :raw audio-data}])))))

(re-frame/reg-event-db ::recorder-created (fn [db [_ recorder]] (assoc db :recorder recorder)))

(re-frame/reg-event-fx ::start-recording (fn [_ _] {::start-recording nil}))

(re-frame/reg-sub
  ::kits ;; dupe from canvas
  (fn [db [_ block-id]]
    (vec (remove nil? (for [[k v] (get db :blocks)] (when (= (get v :kit-parent) block-id) k))))))

(re-frame/reg-sub ::face-block ;; modded dupe from canvas
                  (fn [db [_ block-id]]
                    (if (= (get-in db [:blocks block-id :block-type]) "kit")
                      (let [kits  @(ut/tracked-subscribe [::kits block-id])
                            fb-id (get-in db [:blocks block-id :face-block-id] (first kits))]
                        fb-id)
                      block-id)))

(re-frame/reg-fx ::start-recording
                 (fn []
                   (let [stream-promise (get-audio-stream)
                         face-block-id  "face"] ;; @(ut/tracked-subscribe [::face-block
                                                ;; @db/hovered-block])
                     (reset! listening-block face-block-id)
                     (.then stream-promise
                            (fn [stream]
                              (let [recorder (create-media-recorder stream)]
                                (on-data-available recorder
                                                   (fn [audio-data]
                                                     (let [form-data (create-form-data audio-data)]
                                                       (ut/tapp>> [:call-back-data-inner
                                                                   (count (str form-data))]))))
                                (ut/tracked-dispatch [::recorder-created recorder])
                                (start-recording recorder)))))))


(re-frame/reg-event-db ::stop-recording
                       (fn [db _]
                         (let [recorder (:recorder db)] ;(or (:recorder db) (:webcam-feed db))
                           (ut/tapp>> ["Attempting to stop recording..."])
                           (ut/tapp>> ["state:" (.-state recorder)])
                           (.stop recorder)
                           (stop-recording recorder)
                           (-> db
                               (dissoc :recorder)))))

(re-frame/reg-sub ::latest-audio (fn [db _] (get db :thumper-speaks)))

(defn speak-last
  []
  (let [audio-encoded @(ut/tracked-subscribe [::latest-audio])
        audio-blob    (ut/base64-to-blob audio-encoded "audio/mpeg")]
    (play-audio-blob audio-blob "?")))




(re-frame/reg-event-db ::failure-text-to-speech11
                       (fn [db [_ block-type block-id result]]
                         (let [old-status (get-in db [:http-reqs block-type block-id])]
                           (assoc-in db
                             [:http-reqs block-type block-id]
                             (merge old-status
                                    {:status     "failed"
                                     :ended      (ut/get-time-format-str)
                                     :ended-unix (.getTime (js/Date.))
                                     :message    result})))))

(re-frame/reg-event-db ::success-text-to-speech11
                       (fn [db [_ block-type block-id voices? result]]
                         (ut/tapp>> [:speak? block-type block-id voices?])
                         (if voices?
                           (let [old-status (get-in db [:http-reqs block-type block-id])]
                             (-> db
                                 (assoc-in [:http-reqs block-type block-id]
                                           (merge old-status
                                                  {:status     "success"
                                                   :ended      (ut/get-time-format-str)
                                                   :ended-unix (.getTime (js/Date.))
                                                   :message    result}))))
                           (let [old-status (get-in db [:http-reqs block-type block-id])
                                 byte-nums  result ;; This is already a vector of byte values
                                 byte-array (js/Uint8Array. byte-nums)
                                 blob       (js/Blob. #js [byte-array] #js {:type "audio/mpeg"})]
                             (play-audio-blob blob block-id)
                             (-> db
                                 (assoc-in [:http-reqs block-type block-id]
                                           (merge old-status
                                                  {:status     "success"
                                                   :ended      (ut/get-time-format-str)
                                                   :ended-unix (.getTime (js/Date.))
                                                   :message    {:audio-blob blob}})))))))

(re-frame/reg-event-fx
  ::text-to-speech11
  (fn [{:keys [db]} [_ block-id block-type text-to-speak & [audio-file?]]]
    (let [voices?    (nil? text-to-speak)
          xi-api-key (get db :elevenlabs-api-key)
          method     (if voices? :GET :POST)
          voice-key  (get-in db [:server :settings :eleven-labs-default-voice-name] "Not OG Buffy") ;; "Not
          vname      (get-in db [:blocks block-id :req :voice_name] voice-key)
          vid        (first (remove nil?
                              (for [v (get-in db [:http-reqs :elevenlabs :audio :message :voices])]
                                (when (= (get v :name) vname) (get v :voice_id)))))
          url        (if voices?
                       "https://api.elevenlabs.io/v1/voices" ;; just get voice map
                       (str "https://api.elevenlabs.io/v1/text-to-speech/" vid)) ;; "/stream"
          aturl      (let [protocol          (.-protocol js/window.location)
                           host              (.-host js/window.location)
                           host-without-port (clojure.string/replace host #":\d+$" "")
                           new-port          "8888"]
                       (str protocol "//" host-without-port ":" new-port "/audio"))
          url        (if audio-file?
                       aturl ;;"http://localhost:8888/audio"
                       url)
          header     {"xi-api-key" xi-api-key}
          data       (if audio-file?
                       {:path text-to-speak}
                       (if voices?
                         {}
                         (dissoc (assoc (get-in db [:blocks block-id :req]) :text text-to-speak)
                           :speak)))]
      (ut/tapp>> [method url data voices? vname vid])
      {:db         (assoc-in db
                     [:http-reqs block-type block-id]
                     {:status     "running"
                      :url        url
                      :started    (ut/get-time-format-str)
                      :start-unix (.getTime (js/Date.))})
       :http-xhrio {:method          method
                    :uri             url
                    :params          data
                    :timeout         280000
                    :headers         header
                    :format          (ajax/json-request-format {:keywords? true})
                    :response-format (if voices?
                                       (ajax/json-response-format {:keywords? true})
                                       {:read         pr/-body
                                        :description  "rawww"
                                        :type         :arraybuffer
                                        :content-type "audio/mpeg"})
                    :on-success      [::success-text-to-speech11 block-type block-id voices?]
                    :on-failure      [::failure-text-to-speech11 block-type block-id]}})))





(re-frame/reg-event-db ::failure-edit-voice
                       (fn [db [_ block-type block-id result]]
                         (let [old-status (get-in db [:http-reqs block-type block-id])]
                           (assoc-in db
                             [:http-reqs block-type block-id]
                             (merge old-status
                                    {:status     "failed"
                                     :ended      (ut/get-time-format-str)
                                     :ended-unix (.getTime (js/Date.))
                                     :message    result})))))

(re-frame/reg-event-db ::success-edit-voice
                       (fn [db [_ block-type block-id result]]
                         (let [old-status (get-in db [:http-reqs block-type block-id])]
                           (-> db
                               (assoc-in [:http-reqs block-type block-id]
                                         (merge old-status
                                                {:status     "success"
                                                 :ended      (ut/get-time-format-str)
                                                 :ended-unix (.getTime (js/Date.))
                                                 :message    result}))))))

(re-frame/reg-event-fx
  ::edit-voice
  (fn [{:keys [db]} [_ block-id block-type]]
    (let [;vid "AxgZF8qyZHiePwKPgREq"
          voice-name (get db :self-voice-name)
          vid        @(ut/tracked-subscribe [::get-voice-id voice-name])
          new?       (nil? vid)
          url        (if new?
                       (str "https://api.elevenlabs.io/v1/voices/add")
                       (str "https://api.elevenlabs.io/v1/voices/" vid "/edit"))
          xi-api-key (get db :elevenlabs-api-key)
          method     :POST
          header     {"xi-api-key" xi-api-key}
          blob       (get-in db [:audio-data-recorded :blob])
          audio-file (js/File. [(clj->js blob)] "audio.wav" {"type" "audio/wav"}) ;; Name your
          form-data  (if new?
                       (let [data (js/FormData.)]
                         (.append data "files" audio-file)
                         (.append data "name" voice-name)
                         data)
                       (let [data (js/FormData.)]
                         (.append data "files" audio-file)
                         (.append data "voice_id" vid)
                         (.append data "name" voice-name)
                         data))]
      (ut/tapp>> [:training-voice voice-name :is-new? new?])
      {:db         (assoc-in db
                     [:http-reqs block-type block-id]
                     {:status     "running"
                      :url        url
                      :started    (ut/get-time-format-str)
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