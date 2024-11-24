(ns rvbbit-backend.assistants
  (:require
   [cheshire.core           :as json]
   [clj-http.client         :as client]
   [clojure.edn             :as edn]
   [clojure.java.io         :as io]
   [clojure.java.shell      :as shell]
   [clojure.pprint          :as pp]
   [clojure.string          :as cstr]
   [clojure.walk            :as walk]
   [clojure.core.async      :refer [go chan <! >! <!! >!! timeout]]
   [clojure.java.shell      :refer [sh]]
   [rvbbit-backend.freezepop :as fpop]
   [clojure.core.memoize :as memo]
   [rvbbit-backend.config   :as config]
   [rvbbit-backend.util     :as ut]
   [websocket-layer.core    :as wl]
   [rvbbit-backend.pool-party :as ppy]
   [clojure.pprint :refer [pprint]]
   [rvbbit-backend.sql :as    sql
    :refer [insert-error-row! pool-create sql-exec sql-query metrics-kpi-db
            sql-query-meta sql-query-one system-db cache-db cache-db-memory  system-reporting-db
            to-sql]]
   [rvbbit-backend.db         :as db])
  (:import
   (java.util Base64 UUID)
   (java.nio.file Paths)
   (java.io File)))



(defn oai-call [req openai-api-key openai-org-id]
  (try
    (let [{:keys [url headers query-params method body file] :or {method :get}} req
          http-method                                                           (case method
                                                                                  :get     client/get
                                                                                  :post    client/post
                                                                                  :put     client/put
                                                                                  :delete  client/delete
                                                                                  :head    client/head
                                                                                  :options client/options
                                                                                  :patch   client/patch
                                                                                  :GET     client/get
                                                                                  :POST    client/post
                                                                                  :PUT     client/put
                                                                                  :DELETE  client/delete
                                                                                  :HEAD    client/head
                                                                                  :OPTIONS client/options
                                                                                  :PATCH   client/patch
                                                                                  (throw (IllegalArgumentException.
                                                                                           {:error (str "Unknown http method: "
                                                                                                        method)})))
          headers                                                               (merge {"Authorization" (str "Bearer "
                                                                                                             openai-api-key)
                                                                                        "Content-Type"  "application/json"
                                                                                        "OpenAI-Beta"   "assistants=v1"}
                                                                                       (if (not (nil? openai-org-id))
                                                                                         {"OpenAI-Organization" openai-org-id}
                                                                                         {})
                                                                                       headers)
          body2                                                                 (if (nil? body)
                                                                                  {:query-params query-params}
                                                                                  (if (nil? file)
                                                                                    {:body (json/generate-string body)}
                                                                                    {:multipart [{:name "file"
                                                                                                  :content (slurp file)
                                                                                                  :filename
                                                                                                    (last (clojure.string/split
                                                                                                            file
                                                                                                            #"/"))}
                                                                                                 {:name    "purpose"
                                                                                                  :content "assistants"}]}))
          response                                                              (try (http-method url
                                                                                                  (merge {:as      :json
                                                                                                          :headers headers
                                                                                                          :debug   false}
                                                                                                         body2))
                                                                                     (catch Exception e
                                                                                       {:error {:message (.getMessage e)
                                                                                                :msg     (str e)
                                                                                                :class   (str (type e))}}))]
      (if (:error response) (do (println [:error response :body-sent body2]) response) (get response :body)))
    (catch Exception e {:error-outer {:message (.getMessage e) :msg (str e) :class (str (type e))}})))

;; (defn list-assistants [] (oai-call {:url "https://api.openai.com/v1/assistants" :method :get :query-params {:limit 20}}))

;; (defn list-models [] (oai-call {:url "https://api.openai.com/v1/models" :method :get}))

;; ;; (ut/pp (get (list-models) :data))

;; (defn create-assistant
;;   []
;;   (oai-call
;;     {:url "https://api.openai.com/v1/assistants"
;;      :method :post
;;      :body
;;        {:model "gpt-4-1106-preview"
;;         :name "general-coordinator"
;;         :instructions
;;           "You are 'Rabbit', a helpful assistant that helps people create useful data boards from their SQL databases.
;;                                                These boards consist of query resultsets and data visualizations. They are laid out on a grid canvas based on 50 pixels per square.

;;                                                Each thread will contain 3 files:

;;                                                The first file (filename ending with 'recos.csv') will be all the generated visualizations for the users queries - you can use this data to suggest
;;                                                individual combo_ids that the user can choose, these will then be rendered for the users inspection and selection.
;;                                                There are a number of fields in this file to help you select the appropriate one.

;;                                                The second file (filename ending with 'meta.csv') contains the metadata for the queries that the user has on their board -
;;                                                these are the ones we should focus on. You will find lots of data about each field in each query/table here to help you with you descision making.

;;                                                The third file (filename ending with 'canvas.json') contains the current state of the users board - the coordinates of all the objects, how big
;;                                                they are as well as how much space the user has. This will be imporatant for suggesting placements for these new items on a simple x, y, w, h grid system.

;;                                                Follow the users instructions and always be suggesting new things for them to try, iterating on them. If you are given no instructions,
;;                                                then you should look at the files and the state of the board and recommend things that seems useful or interesting, this could
;;                                                start a conversation with the user to iterate on."
;;         :tools [{:type "retrieval"}]}}))


;; (defn upload-file [file] (api/create-file {:purpose "assistants" :file (java.io.File. file)} {:api-key openapi-key}))

;; (defn chat-lame
;;   [convo]
;;   (try (api/create-chat-completion {:model    "gpt-4" ;"gpt-4-1106-preview"
;;                                     :messages convo}
;;                                    {:api-key openapi-key})
;;        (catch Exception e (ut/pp [:api/create-chat-completion :error (str e)]))))

;; (defn chat
;;   [convo]
;;   (oai-call {:url    "https://api.openai.com/v1/chat/completions"
;;              :body   {:messages (vec (for [{:keys [role content]} convo] {:role role :content content})) :model "gpt-4"}
;;              :method :post}))

;; (defn one-shot-chat
;;   [message system-msg]
;;   (oai-call {:url    "https://api.openai.com/v1/chat/completions"
;;              :body   {:messages [{:role "system" :content system-msg} {:role "user" :content message}]
;;                       :model    "gpt-4-1106-preview"}
;;              :method :post}))

;; (defn buffy
;;   [query-name]
;;   (let [base-folder    "/home/ryanr/rvbbit-backend/kit-rowsets/"
;;         _ (println query-name)
;;         table-name     (keyword (str "table" (rand-int 12345)))
;;         data-file-path (str base-folder (ut/unkeyword query-name) ".edn")
;;         gdata          (take 100 (edn/read-string (slurp data-file-path))) ;(durable-atom "./datom.edn")
;;         story          (one-shot-chat (str "read this data, analyze it, and tell me a short story about it please."
;;                                            "it is in EDN format, very similar to JSON. Here it is: ``` "
;;                                            (pr-str gdata)
;;                                            " ```")
;;                                       "You are a helpful assistant - an upbeat valley girl who loves reading tabular data.")
;;         _ (ut/pp story)]
;;     {:storytime {:data [{:ask-mutates       {}
;;                          :content           (vec (for [line (vec (remove empty?
;;                                                                    (cstr/split-lines (get-in story
;;                                                                                              [:choices 0 :message :content]))))]
;;                                                    [:box :padding "10px" :width "500px" :size "auto" :style {:font-size "17px"}
;;                                                     :child [:speak-click (str line)]]))
;;                          :highlight-columns {}
;;                          :name              "Well, you asked for it."
;;                          :order             0
;;                          :parameters        {}
;;                          :step-mutates      {}}]}}))



(def thread-lookups (atom {}))

(def base-url "https://api.openai.com/v1")
(def api-key (get (config/settings) :openai-api-key))

;; General API request function
(defn api-request
  ([method endpoint] (api-request method endpoint nil))
  ([method endpoint body]
   (try
     (let [url (str base-url endpoint)
           headers {"Authorization" (str "Bearer " api-key)
                    "OpenAI-Beta" "assistants=v2"
                    "Content-Type" "application/json"}
           request (cond-> {:method method
                            :url url
                            :headers headers
                            :as :json}
                     body (assoc :body (json/generate-string body)))]
       (:body (client/request request)))
     (catch Exception e
       (let [response (ex-data e)
             status (:status response)
             body (try (json/parse-string (:body response) true)
                       (catch Exception _ (:body response)))]
         (throw (ex-info (str "API request failed: " (or (:message body) (.getMessage e)))
                         {:status status
                          :method method
                          :endpoint endpoint
                          :response body})))))))

;; Assistant creation and management

;; (def assistant-instructions
;;   (slurp "defs/fabric/clover-canvas/context.md"))

(defn create-assistant [assistant-name assistant-instructions assistant-functions]
  (api-request :post "/assistants"
               {:model "gpt-4o" ;; "gpt-4-turbo" "o1-preview" ;; "gpt-4o" ; "o1-mini"
                :name assistant-name ;;"Calliope"
                ;:top_p 1.0
                ;:temperature 1.0
                ;:response_format {:type "json_object"}
                :instructions assistant-instructions
                :tools (vec (remove nil?
                                    [{:type "code_interpreter"}
                                     (when assistant-functions
                                       (for [[k {:keys [description parameters]}] assistant-functions]
                                         {:type "function"
                                          :function {:name k
                                                     :description description
                                                     :parameters parameters}}))]))}))

(defn get-assistant
  "Retrieves the details of an existing assistant."
  [assistant-id]
  (api-request :get (str "/assistants/" assistant-id)))

(defn update-assistant
  "Updates an existing assistant with new parameters."
  [assistant-id updates]
  (api-request :post (str "/assistants/" assistant-id) updates))

;; Thread management

(defn create-thread
  "Creates a new conversation thread."
  []
  (api-request :post "/threads"))

(defn get-thread
  "Retrieves the details of an existing thread."
  [thread-id]
  (api-request :get (str "/threads/" thread-id)))

(defn delete-thread
  "Deletes a conversation thread."
  [thread-id]
  (api-request :delete (str "/threads/" thread-id)))

(defn delete-assistant
  "Deletes an assistant."
  [assistant-id]
  (api-request :delete (str "/assistants/" assistant-id)))

;; Message management

(defn add-message
  "Adds a new message to a thread."
  [thread-id content client-name ai-worker-name]
  (let [content {:role "user"
                 :content content}]
    (swap! db/ai-worker-atom update-in [(keyword ai-worker-name) (keyword thread-id) :all]
           (fn [existing]
             (if (nil? existing)
               [content]
               (conj existing content))))
    (api-request :post (str "/threads/" thread-id "/messages") content)))

(defn get-messages
  "Retrieves all messages from a thread."
  [thread-id]
  (api-request :get (str "/threads/" thread-id "/messages")))

;; Run management

(defn create-run
  "Initiates a new run of the assistant on a thread."
  [thread-id assistant-id]
  (api-request :post (str "/threads/" thread-id "/runs")
               {:assistant_id assistant-id}))

(defn get-run
  "Retrieves the status of a run."
  [thread-id run-id]
  (api-request :get (str "/threads/" thread-id "/runs/" run-id)))

(defn cancel-run
  "Cancels an in-progress run."
  [thread-id run-id]
  (api-request :post (str "/threads/" thread-id "/runs/" run-id "/cancel")))

;; File management

;; (defn upload-file
;;   "Uploads a file to be used with the assistant."
;;   [file-path]
;;   (client/post (str base-url "/files")
;;              {:headers {"Authorization" (str "Bearer " api-key)}
;;               :multipart [{:name "purpose" :content "assistants"}
;;                           {:name "file" :content (clojure.java.io/file file-path)}]
;;               :as :json}))

;; (defn upload-image
;;   "Uploads an image file to be used with the assistant."
;;   [image-path]
;;   (let [response (client/post (str base-url "/files")
;;                             {:headers {"Authorization" (str "Bearer " api-key)}
;;                              :multipart [{:name "purpose" :content "assistants"}
;;                                          {:name "file"
;;                                           :content (clojure.java.io/file image-path)
;;                                           :mime-type "image/png"}] ; Adjust mime-type as needed
;;                              :as :json})]
;;     (:body response)))

(defn upload-and-add-file
  "Uploads a file and adds it to the specified thread."
  [thread-id file-path client-name ai-worker-name]
  (let [upload-response (client/post (str base-url "/files")
                                   {:headers {"Authorization" (str "Bearer " api-key)}
                                    :multipart [{:name "purpose" :content "assistants"}
                                                {:name "file"
                                                 :content (clojure.java.io/file file-path)}]
                                    :as :json})
        file-id (-> upload-response :body :id)]
    (when file-id
      (let [content {:role "user"
                     :content [{:type "file_attachment"
                                :file_id file-id}]}]
        (api-request :post (str "/threads/" thread-id "/messages") content)
        (swap! db/ai-worker-atom update-in [(keyword ai-worker-name) (keyword thread-id) :all]
               (fn [existing]
                 (if (nil? existing)
                   [content]
                   (conj existing content))))
        {:status :success
         :message "File uploaded and added to thread"
         :file-id file-id}))))

(defn upload-and-add-image
  "Uploads an image file and adds it to the specified thread."
  [thread-id image-path client-name ai-worker-name]
  (let [mime-type (case (clojure.string/lower-case (re-find #"\.\w+$" image-path))
                    ".png"  "image/png"
                    ".jpg"  "image/jpeg"
                    ".jpeg" "image/jpeg"
                    ".gif"  "image/gif"
                    "application/octet-stream") ; default mime-type
        upload-response (client/post (str base-url "/files")
                                   {:headers {"Authorization" (str "Bearer " api-key)}
                                    :multipart [{:name "purpose" :content "assistants"}
                                                {:name "file"
                                                 :content (clojure.java.io/file image-path)
                                                 :mime-type mime-type}]
                                    :as :json})
        file-id (-> upload-response :body :id)]
    (when file-id
      (let [content {:role "user"
                     :content [{:type "image_file"
                                :image_file {:file_id file-id}}]}]
        (api-request :post (str "/threads/" thread-id "/messages") content)
        (swap! db/ai-worker-atom update-in [(keyword ai-worker-name) (keyword thread-id) :all]
               (fn [existing]
                 (if (nil? existing)
                   [content]
                   (conj existing content))))
        {:status :success
         :message "Image uploaded and added to thread"
         :file-id file-id}))))

(defn add-image-to-thread
  "Adds an uploaded image to the thread."
  [thread-id file-id client-name ai-worker-name]
  (add-message thread-id
               [{:type "image_file"
                 :image_file {:file_id file-id}}]
               client-name ai-worker-name))

;; Context and execution management

(declare poll-run-until-complete handle-required-actions)

(defn add-context
  "Adds context to the thread without expecting a response."
  [thread-id content client-name ai-worker-name]
  (add-message thread-id content client-name ai-worker-name))

(defn add-execution-result-silent
  "Adds an execution result to the thread as a system message."
  [thread-id result]
  (api-request :post (str "/threads/" thread-id "/messages")
               {:role "system"
                :content (str "Execution result (silent): " (pr-str result))}))

;; DSL and response handling

(defn extract-edn-code
  "Extracts EDN code from a response string, handling various formats including unwrapped EDN."
  [response]
  (when (map? response)
    (when-let [content (-> response :data first :content first :text :value)]
      (let [code-block-pattern #"(?s)```(?:clojure|clojurescript|edn)?\s*(.*?)\s*```"]
        (if-let [matches (re-find code-block-pattern content)]
          (second matches)
          ;; If no code block is found, return the entire content
          content)))))

;; (defn extract-edn-code
;;   "Extracts EDN code from a response string, handling various formats including unwrapped EDN."
;;   [response]
;;   (when (map? response)
;;     (when-let [content (-> response :data first :content first :text :value)]
;;       (let [code-block-pattern #"(?s)```(?:clojure|clojurescript|edn|)?\s*(.*?)\s*```"]
;;         (if-let [matches (re-find code-block-pattern content)]
;;           (second matches)
;;           ;; If no code block is found, return the entire content
;;           content)))))

(defn clean-and-parse-edn
  "Cleans the EDN string and attempts to parse it."
  [edn-string]
  (try
    (let [cleaned-code (-> edn-string
                           ;(clojure.string/replace #"^\s*;.*$" "" :multiline)  ; Remove comment lines
                           ;(clojure.string/replace #"#_\(.*?\)" "")            ; Remove reader comments
                           (clojure.string/trim)
                           (cstr/trim-newline)
                           ;(cstr/replace "\n" " ")
                           (clojure.string/trim))]
      (if (clojure.string/starts-with? cleaned-code "{")
        (edn/read-string cleaned-code)
        (throw (ex-info "Invalid EDN: Does not start with a map" {}))))
    (catch Exception e
      (println "EDN parsing error:" (.getMessage e))
      nil)))

(defn find-outermost-map
  "Finds the outermost map in the string, including all nested content."
  [s]
  (let [trimmed (cstr/trim s)
        start (cstr/index-of trimmed "{")
        end (cstr/last-index-of trimmed "}")]
    (when (and start end (> end start))
      (subs trimmed start (inc end)))))

(defn safe-read-string
  "Safely reads a string as EDN, returning nil if it fails."
  [s]
  (try
    (edn/read-string s)
    (catch Exception e
      (println "EDN parsing error:" (.getMessage e))
      nil)))

(defn clean-keywords
  "Recursively clean keywords in a data structure, removing colons from keys."
  [data]
  (walk/postwalk
   (fn [x]
     (if (keyword? x)
       (keyword (clojure.string/replace (name x) #"^:" ""))
       x))
   data))

(defn extract-valid-edn
  "Extracts valid EDN from the response, focusing on the outermost map."
  [content]
  (when-let [outer-map (find-outermost-map content)]
    (safe-read-string (clean-keywords outer-map))))

(defn format-timestamp
  "Converts a Unix timestamp to a human-readable format."
  [unix-timestamp]
  (let [date (java.util.Date. (* 1000 unix-timestamp))
        formatter (java.text.SimpleDateFormat. "EEEE, MMMM d'th' h:mm a")]
    (.format formatter date)))

(defn request-from-assistant
  "Sends a request to the assistant and handles both DSL code and chat responses,
   with improved error handling and content parsing."
  [thread-id assistant-id prompt client-name ai-worker-name]
  (println "Sending prompt to assistant:" prompt)
  (add-message thread-id prompt client-name ai-worker-name)
  (let [run (create-run thread-id assistant-id)
        _ (println "Run created:" (:id run) " " ai-worker-name " " thread-id)
        _ (swap! db/ai-worker-atom assoc-in [(keyword ai-worker-name) (keyword thread-id) :running?] true)
        response (<!! (poll-run-until-complete thread-id (:id run)))
        _ (swap! db/ai-worker-atom assoc-in [(keyword ai-worker-name) (keyword thread-id) :running?] false)
        _ (swap! db/ai-worker-atom update-in [(keyword ai-worker-name) (keyword thread-id) :all]
                 (fn [existing]
                   (if (nil? existing)
                     [response]
                     (conj existing response))))
        _ (swap! db/ai-worker-atom assoc-in [(keyword ai-worker-name) (keyword thread-id) :last]
                 (vec (reverse (map (fn [x]
                                      (-> x
                                          (dissoc :id :object :metadata :run_id :thread_id :assistant_id)
                                          (assoc :created_at_str (format-timestamp (:created_at x)))))
                                    (get response :data)))))
        ;_ (println "Raw response:" response)
        extracted-content (extract-edn-code response)
        ;_ (println "Extracted content:" extracted-content)
        ]
    (if-let [parsed-edn (clean-and-parse-edn extracted-content)]
      {:type :dsl-code
       :content parsed-edn}
      {:type :chat-response
       :content (or extracted-content "No response content")})))

;; (defn request-from-assistant
;;   "Sends a request to the assistant and handles both DSL code and chat responses,
;;    with improved error handling and content parsing."
;;   [thread-id assistant-id prompt]
;;   (println "Sending prompt to assistant:" prompt)
;;   (add-message thread-id prompt)
;;   (let [run (create-run thread-id assistant-id)
;;         _ (println "Run created:" (:id run))
;;         response (<!! (poll-run-until-complete thread-id (:id run)))
;;         _ (println "Raw response:" response)
;;         content (-> response :data first :content first :text :value)
;;         extracted-edn (extract-valid-edn content)]
;;     (println "Extracted EDN:" (pr-str extracted-edn))
;;     (if extracted-edn
;;       {:type :dsl-code
;;        :content extracted-edn}
;;       {:type :chat-response
;;        :content (or content "No response content")})))

(defn execute-dsl
  "Executes the DSL code (placeholder function)."
  [dsl-data]
  (if (map? dsl-data)
    (do
      (ut/pp ["Executing DSL:" dsl-data])
      ;;(wss/push-to-client [:ui-keypath :not-needed-here] [] :acclaimed-burgundy-cattle-33 1 :push-assocs dsl-data)
      {:result "DSL execution result placeholder"})
    {:error "Invalid DSL data" :data dsl-data}))

;; Main workflow function

(defn assistant-workflow
  "Handles the complete workflow of interacting with the assistant,
   including DSL code generation, execution, and chat responses."
  [assistant-id thread-id prompt client-name ai-worker-name]
  ;(println "Starting assistant workflow with prompt:" prompt)
  (let [response (request-from-assistant thread-id assistant-id prompt client-name ai-worker-name)]
    (println "Response from assistant:" response)
    (case (:type response)
      :dsl-code
      (let [execution-result (execute-dsl (:content response))]
        ;(add-execution-result-silent thread-id execution-result)
        {:type :dsl-execution
         :dsl-code (:content response)
         :execution-result execution-result})

      :chat-response
      {:type :chat-response
       :content (:content response)}

      :error
      {:type :error
       :error (:error response)
       :details (:raw-content response)}

      ;; Default case
      {:type :unknown-response
       :content response})))

;; Utility functions

(defn poll-run-until-complete
  "Polls a run until it's completed, handling any required actions."
  [thread-id run-id]
  (go
    (loop [attempts 0]
      (let [run (get-run thread-id run-id)]
        (println "Run status:" (:status run)) ; Log the run status
        (case (:status run)
          "completed" (let [messages (get-messages thread-id)]
                        (println "Completed. Messages:" (count messages))
                        messages)
          "failed" (do
                     (println "Run failed. Details:" run)
                     (throw (ex-info "Run failed" run)))
          "requires_action" (do
                              (println "Run requires action")
                              (handle-required-actions thread-id run-id)
                              (recur (inc attempts)))
          (if (< attempts 30) ; Limit the number of polling attempts
            (do
              (<! (timeout 2000)) ; Wait for 2 seconds before polling again
              (recur (inc attempts)))
            (throw (ex-info "Polling timed out" {:thread-id thread-id :run-id run-id}))))))))


(defn handle-required-actions
  "Handles any actions required by the assistant during a run."
  [thread-id run-id]
  ;; Implement handling of required actions here
  ;; This might involve calling functions, retrieving data, etc.
  (println (str "Handling required actions for run:" thread-id " " run-id)))

(defn get-ai-workers []
  (let [workers-dir (io/file "./ai-workers")
        subfolders  (filter #(.isDirectory %) (.listFiles workers-dir))]
    (into {}
          (for [folder subfolders
                :let [folder-name (.getName folder)
                      config-file (io/file folder "config.edn")
                      solvers-file (io/file folder "solvers.edn")
                      solvers (edn/read-string (slurp solvers-file))
                      functions-map nil ;; convert to description, name, parameters from solvers
                      ]]
            (when (.exists config-file)
              {folder-name {:personality (edn/read-string (slurp config-file))
                            :functions functions-map}})))))

(defn respond-and-run [message client-name ai-worker-name]
  (let [assistant-id (get-in @thread-lookups [client-name ai-worker-name :assistant-id])
        thread-id (get-in @thread-lookups [client-name ai-worker-name :thread-id])]
    (assistant-workflow assistant-id thread-id message client-name ai-worker-name)))

(defn runstream-boot-agent [ai-worker-name client-name & [message]]
  (try
    (let [workers-map (get-ai-workers)
          config-map (get workers-map ai-worker-name)
          ;;_ (ut/pp [:config-map config-map])
          assistant (create-assistant ai-worker-name
                                      (get-in config-map [:personality :instructions] "You are a helpful assistant!")
                                      (get config-map :functions nil))
          _ (println "OpenAI Assistant created:" (:id assistant) " for " ai-worker-name)
          assistant-id (:id assistant)
          thread (create-thread)
          _ (println "Thread created:" (:id thread))
          thread-id (:id thread)
          _ (swap! thread-lookups assoc-in [client-name ai-worker-name]
                   {:thread-id thread-id
                    :assistant-id assistant-id
                    :thread thread
                    :assistant assistant})
          ;; _ (swap! db/ai-worker-atom assoc-in [(keyword ai-worker-name) (keyword thread-id)]
          ;;          [ai-worker-name assistant-id thread-id])
          _ (swap! db/ai-worker-atom update-in [:threads-for client-name] ;; :ai-worker/threads-for>*client-name*
                   (fn [existing]
                     (if (nil? existing)
                       [[ai-worker-name assistant-id thread-id]]
                       (conj existing [ai-worker-name assistant-id thread-id]))))
          result (assistant-workflow assistant-id thread-id
                                     (or message "Hello how are you?") client-name ai-worker-name)]
      result)
    (catch Exception e
      (let [data (ex-data e)]
        (println "An error occurred:")
        (println "Message:" (.getMessage e) (str e))
        (when data
          (println "Additional error data:")
          (ut/pp data))
        (.printStackTrace e)))))

;; (ut/pp [:assistants "runstream-boot-agent"])

(defn clean-up-threads-and-assistants []
  (doseq [client-name (keys @thread-lookups)]
    (doseq [ai-worker-name (keys (get-in @thread-lookups [client-name]))]
      (let [thread-id (get-in @thread-lookups [client-name ai-worker-name :thread-id])
            assistant-id (get-in @thread-lookups [client-name ai-worker-name :assistant-id])]
        (ut/pp [:delete-OpenAI-assistant-threads thread-id])
        (delete-thread thread-id)
        (ut/pp [:delete-OpenAI-assistant assistant-id])
        (delete-assistant assistant-id)))))

;; (runstream-boot-agent "Calliope" :lucid-blue-kangaroo-0)

;;  (respond-and-run "Good. Thank you! How old is Rich Hickey, creator of Clojure?" :lucid-blue-kangaroo-0 "Calliope")

;;  (respond-and-run "How old was he when he took a year off to create Clojure?" :lucid-blue-kangaroo-0 "Calliope")

;;  (respond-and-run "Thoughts on Clojure and it's future?" :lucid-blue-kangaroo-0 "Calliope")

;; (ut/pp [:thread-lookups @thread-lookups])
;; (ut/pp [:ai-worker-messages @db/ai-worker-atom])










(defn call-function-by-name [function-name args client-name]
  (let [function-name (-> (str function-name)
                          (cstr/replace  "__"  ".")
                          (cstr/replace  "_"  "/"))
        parts (clojure.string/split function-name #"/")
        [ns-name fn-name] (if (= (count parts) 2)
                            parts
                            [nil (first parts)])
        target-ns (if ns-name
                    (find-ns (symbol ns-name))
                    *ns*)
        function (ns-resolve target-ns (symbol fn-name))
        args (assoc args :client-name client-name)]
    (if function
      (function args)
      (throw (ex-info (str "Function not found: " function-name)
                      {:function-name function-name})))))

(def alpha-vantage-api-key (get (config/settings) :alpha-vantage-api-key))

(defn get-stock-price [{:keys [ticker]}]  ;; LLM tool calling test fn
  (try
    (doall
     (let [url (str "https://www.alphavantage.co/query"
                    "?function=GLOBAL_QUOTE"
                    "&symbol=" (clojure.string/upper-case ticker)
                    "&apikey=" alpha-vantage-api-key)
           response (client/get url {:as :json})
           ;;_ (ut/pp [:response response])
           data (get-in response [:body (keyword "Global Quote")])
           price (get data (keyword "05. price"))]
       (if price
         {:price (Double/parseDouble price)
          :symbol ticker
          :last-updated (get data (keyword "07. latest trading day"))}
         {:error (str "No price data found for ticker: " ticker)})))
    (catch Exception e
      {:error (str "Error fetching stock price: " (.getMessage e))})))

(defn quantum-simulation [{:keys [seed]}] ;; LLM tool calling test fn
  (println "Running quantum simulation with seed:" seed)
  {:result "Simulation complete" :data [0.5 0.3 0.2]})

;; (ut/pp (get-stock-price {:ticker "AAPL"}))
;; (ut/pp (keys @db/model-costs))
;; (ut/pp (get-in @db/model-costs ["Anthropic" "claude-3-5-sonnet" :per-mtok-input]))
;; (ut/pp (keys (get-in @db/model-costs ["Anthropic" ])))

(declare run-sub-conversation)

(def anthropic-base-url "https://api.anthropic.com/v1/")
(def anthropic-api-key (get (config/settings) :anthropic-api-key))

;; Atom to store responses for each conversation
(def responses (fpop/thaw-atom {} "./data/atoms/responses-atom.msgpack.transit"))

;; Atom to store running status for each conversation
(def running-status (atom {}))

(def summy-threads (fpop/thaw-atom {} "./data/atoms/summy-threads-atom.msgpack.transit"))

;; Atom to store conversation hierarchy
(def conversation-hierarchy (fpop/thaw-atom {} "./data/atoms/conversation-hierarchy-atom.msgpack.transit"))

;; (ut/pp [:conversation-hierarchy @conversation-hierarchy])
;; (ut/pp [:responses @responses])
;; (ut/pp [:running-status @running-status])

;; Assistant configurations
;; (def assistant-configs
;;   {"Calliope"
;;    {:model "claude-3-5-sonnet-20240620"
;;     :system-message "You are a helpful AI assistant with access to various tools.
;;                      Your personality is 90% business and 10% valley girl
;;                      (think Buffy the Vampire Slayer, not Hannah Montana), your name is Calliope."
;;     :max-tokens 8192
;;     :tools [{:name "rvbbit-backend__assistants_get-stock-price"
;;              :description "Get the current stock price for a given ticker symbol."
;;              :input_schema {:type "object"
;;                             :properties {:ticker {:type "string"
;;                                                   :description "The stock ticker symbol, e.g. AAPL for Apple Inc."}}
;;                             :required ["ticker"]}}
;;             {:name "rvbbit-backend__assistants_quantum-simulation"
;;              :description "Run the quantum simulation, baby!"
;;              :input_schema {:type "object"
;;                             :properties {:seed {:type "integer"
;;                                                 :description "Chose a starting random number between 0 and 100, dont ask the user, just do it."}}
;;                             :required ["seed"]}}]}
;;    "Summy"
;;    {:model "claude-3-haiku-20240307"
;;     :system-message "You are a thread summarizer. You will be given a thread of messages and you will summarize it all into one short sentence.
;;                      Output ONLY that sentence. Don't start with 'The Summary' just say the summary in a single SHORT sentence.
;;                      Less than 10 words, brevity is more important than accuracy."
;;     :max-tokens 1024}

;;    "specialized-assistant"
;;    {:model "claude-3-5-sonnet-20240307"
;;     :system-message "You are a specialized AI assistant for quantum computing tasks."
;;     :max-tokens 8192
;;     :tools {"quantum-simulation"
;;             {:func (fn [params]
;;                      (println "Running quantum simulation with params:" params)
;;                      {:result "Simulation complete" :data [0.5 0.3 0.2]})
;;              :description "Run a quantum simulation"
;;              :inputs {:params {:type "string"
;;                                :description "Parameters for the quantum simulation"}}}}}})

(defn assistant-configs []
  (assoc
   (get-in @db/ai-worker-atom [:config :server])
   ;; hardcoded "system" assistants for various tasks
   "Summy"
   {:model "claude-3-haiku-20240307"
    :system "You are a thread summarizer. You will be given a thread of messages and you will summarize it all into one short sentence.
             Output ONLY that sentence. Don't start with 'The Summary' just say the summary in a single SHORT sentence.
             Less than 10 words, brevity is more important than accuracy."
    :max-tokens 1024}
   "Clammy"
   {:model "claude-3-haiku-20240307"
    :system [{:text (str "You are a SQL translator. Your job is to convert string SQL into Honey-SQL Clojure vectors and maps format.
                                      This is a dialect of Honey-SQL called Rabbit Clover and there is added functionality beyond the Honey-SQL spec, the examples will make this clear.
                                      You will be given the string - output ONLY the EDN of the converted query. Here are some examples of this type of conversion plus some additional metadata to help understand."
                         (with-out-str
                           (clojure.pprint/pprint @db/clover-sql-training-atom)))
              :type "text"
              :cache_control {:type "ephemeral"}}]
    :max-tokens 1024}))

;; (ut/pp  (assistant-configs))
;; (ut/pp (get-in @db/ai-worker-atom [:config :server]))

(declare start-conversation send-message get-responses)

(def summy-cache (atom {}))

(defn- hash-msgs [msgs]
  (hash (pr-str msgs)))

(def ttl-assistants {:select [:assistant_name
                              [[:+ :input_cost_sum :output_cost_sum]
                               :total_cost]]
                     :from
                     [[{:select
                        [:assistant_name [[:count 1] :rowcnt]
                         [[:sum :input_cost] :input_cost_sum]
                         [[:sum :output_cost] :output_cost_sum]]
                        :from
                        [[{:select [:assistant_name :client_name
                                    :input_cost :input_tokens
                                    :model_id :output_cost
                                    :output_tokens :platform
                                    :thread_id :ts]
                           :from   [[:llm_log :ss561]]} :kk153]]
                        :group-by [:assistant_name]
                        :order-by [[:rowcnt :desc]]} :hh66]]})


;; (def ttt "[\"Hey, why don't you draw me something fun on the canvas, if you could, Calliope, please.\\\"\\\\n\\\\n###CLIENT-METADATA###\\\\n\\\\n\\\" {:client-name :cute-ovoid-swallow-11, :metadata {}, :canvas-coords [], :canvas-size [41.88 21.66]}\" {:type \"text\", :text \"Absolutely! I'd love to draw something fun on the canvas for you. Let's create a playful little scene with some colorful elements. I'll use the Rabbit canvas to create this for you, using the Clover DSL. \\n\\nGiven the canvas size of [41.88 21.66], we have a good amount of space to work with. I'll create a few blocks to compose a cheerful scene. Let's go with a sunny day theme with a sun, a tree, and a cute little rabbit (in honor of our Rabbit canvas system)!\\n\\nI'll create four blocks: one for the sun, one for the tree, one for the rabbit, and one for a title. Here we go!\"} {:type \"tool_use\", :id \"toolu_01Re1ZaXVr3U47EEcgreWjsS\", :name \"rvbbit-backend__websockets_rabbit-edit\", :input {:block-keypath \"[:panels :block-sun]\", :block-body \"{\\n  :name \\\"Sunny Sun\\\",\\n  :w 8,\\n  :h 8,\\n  :root [2 2],\\n  :tab \\\"Fun Drawing\\\",\\n  :views {\\n    :sun-view [:box\\n ")
;; (ut/pp (first (cstr/split (str ttt) #"CLIENT-METADATA")))

(defn call-clammy [sql-string]
  (try
    (let [_ (ut/pp [:clammy-sql-string sql-string])
          aid "Clammy"
          cid :rvbbit
          tid (start-conversation cid aid)
          _ (send-message cid aid tid (str sql-string))
          clammy (get-in (last (get-responses cid aid tid)) [:content 0 :text] "No summary available")
          ;cost-per-assistant (sql-query system-reporting-db (to-sql ttl-assistants))
          ]
      ;(swap! summy-threads assoc thread-id clammy)
      ;(swap! db/ai-worker-atom assoc-in [:costs] cost-per-assistant)
      ;; (swap! db/ai-worker-atom assoc-in [:threads-for client-name]
      ;;        (vec (for [[a s t] (get-in @db/ai-worker-atom [:threads-for client-name])]
      ;;               (if (= t thread-id)
      ;;                 [a summy t]
      ;;                 [a s t]))))
      (ut/pp [:clammy-done {sql-string clammy}])
      clammy)
    (catch Exception e (ut/pp [:call-clammy-failed! (str e) e :sql-string sql-string]))))

(def memoized-call-summy
  (memo/ttl
   (fn [msgs thread-id client-name]
     (try
       (let [;;msgs (mapv #(first (cstr/split (str %) #"CLIENT-METADATA")) (for [m msgs] (get m :content)))
             msgs (vec
                   (remove nil?
                           (for [m msgs
                                 :let [content (get m :content)
                                       ttype (get content :type)]]
                             (when (= ttype "text")
                               (first (cstr/split (str (get content :text)) #"CLIENT-METADATA"))))))
             _ (ut/pp [:summy-msgs  msgs])
             aid "Summy"
             cid :rvbbit
             tid (start-conversation cid aid)
             _ (send-message cid aid tid (str msgs))
             summy (get-in (last (get-responses cid aid tid)) [:content 0 :text] "No summary available")
             cost-per-assistant (sql-query system-reporting-db (to-sql ttl-assistants))]
         (swap! summy-threads assoc thread-id summy)
         (swap! db/ai-worker-atom assoc-in [:costs] cost-per-assistant)
         (swap! db/ai-worker-atom assoc-in [:threads-for client-name]
                (vec (for [[a s t] (get-in @db/ai-worker-atom [:threads-for client-name])]
                       (if (= t thread-id)
                         [a summy t]
                         [a s t]))))
         (ut/pp [:summy-done {thread-id summy}])
         summy)
       (catch Exception e (ut/pp [:call-summy-failed! (str e) e :msgs msgs]))))
   :ttl/threshold 3600000))

(defn call-summy [msgs thread-id client-name]
  (let [msg-hash (hash-msgs msgs)]
    (if-let [cached-result (get @summy-cache msg-hash)]
      cached-result
      (let [result (memoized-call-summy msgs thread-id client-name)]
        (swap! summy-cache assoc msg-hash result)
        result))))

;; (defn call-summy [msgs]
;;   (let [msgs (vec (for [m msgs] (get m :content)))
;;         aid "Summy"
;;         cid :rvbbit
;;         tid (start-conversation cid aid)
;;         _ (send-message cid aid tid (pr-str msgs))]
;;     (get-in (last (get-responses cid aid tid)) [:content 0 :text] "No summary available")))

;; (ut/pp (start-conversation :rvbbit "Summy"))

;; (ut/pp (call-summy (pr-str {:role "assistant",
;;                             :content
;;                             [{:text "The founder effect led to the Robitaille name becoming more common in Quebec compared to France over time.", :type "text"}]})))

(defn unroll-messages [messages]
  (apply concat (for [msg messages
                      :let [content (get msg :content)]]
                  (if (vector? content)
                    (for [c content
                          :let [ttype (get c :type)
                                ;role (get msg :role)
                                ]]
                      (assoc (assoc (dissoc msg :content) :content c) :event-type ttype))
                    [msg]))))

(defn interpret-and-clean-messages [messages thread-id client-name]
  (let [cleaned (-> messages
                    ut/replace-large-base64
                    unroll-messages)
        _ (when (= (get (last messages) :role) "assistant")
            (ppy/execute-in-thread-pools :summy (call-summy cleaned thread-id client-name)))]
    cleaned))

;; Function to generate a unique session ID
(defn generate-thread-id []
  (str (java.util.UUID/randomUUID)))

(defn get-responses [client-name assistant-name thread-id]
  (println "Getting responses for:" client-name assistant-name thread-id)
  (get-in @responses [client-name assistant-name thread-id] []))

(defn set-running-status [client-name assistant-name thread-id status]
  (println "Setting running status for:" client-name assistant-name thread-id "to" status)
  (swap! db/ai-worker-atom assoc-in [(keyword assistant-name) (keyword thread-id) :running?] status)
  (swap! running-status assoc-in [client-name assistant-name thread-id] status))

(defn get-running-status [client-name assistant-name thread-id]
  ;; (println "Getting running status for:" client-name assistant-name thread-id)
  (get-in @running-status [client-name assistant-name thread-id] false))

;; Helper functions for atom manipulation
(defn update-responses [client-name assistant-name thread-id new-message & [extra]]
  (let [extra (merge
               (get new-message :_metadata)
               {:created-at (System/currentTimeMillis)
                :msg-id (java.util.UUID/randomUUID)} extra)]
    (println "Updating responses for:" client-name assistant-name thread-id)
    (pprint new-message)
    (swap! responses assoc-in [client-name assistant-name thread-id]
           (conj (get-in @responses [client-name assistant-name thread-id] []) (merge new-message {:_metadata extra})))
  ;; limit to a single atom later, but for now... we can control the reactions with an airlock atom
  ;; since we have to strip base64 files from the messages anyways - it's too much memory stress on the browser/websocket reader
    (swap! db/ai-worker-atom assoc-in [(keyword assistant-name) (keyword thread-id) :last]
           (if (not= assistant-name "Summy") ;; else we will loop forever, lol
             (interpret-and-clean-messages (get-responses client-name assistant-name thread-id) thread-id client-name)
             (get-responses client-name assistant-name thread-id)))))

(defn update-conversation-hierarchy [client-name assistant-name thread-id & [parent-info]]
  (swap! conversation-hierarchy update-in [client-name]
         (fn [client-convos]
           (let [updated-convos (assoc-in client-convos [assistant-name thread-id]
                                          {:status (get-running-status client-name assistant-name thread-id)
                                           :parent parent-info
                                           :children []})]
             (if parent-info
               (update-in updated-convos
                          [(:assistant parent-info) (:session parent-info) :children]
                          conj [assistant-name thread-id])
               updated-convos)))))

(defn get-client-conversation-hierarchy [client-name]
  (get @conversation-hierarchy client-name))

;; (defn get-mime-type [file-path]
;;   (let [extension (cstr/lower-case (re-find #"\.\w+$" file-path))]
;;     (case extension
;;       ".png"  "image/png"
;;       ".jpg"  "image/jpeg"
;;       ".jpeg" "image/jpeg"
;;       ".gif"  "image/gif"
;;       ".webp" "image/webp"
;;       "application/octet-stream"))) ; default mime-type

;; (defn encode-image [file-path]
;;   (println "Encoding image:" file-path)
;;   (let [file (io/file file-path)
;;         bytes (byte-array (.length file))]
;;     (with-open [in (io/input-stream file)]
;;       (.read in bytes))
;;     (.encodeToString (Base64/getEncoder) bytes)))

(defn convert-available? []
  (try
    (let [{:keys [exit]} (sh "which" "convert")]
      (zero? exit))
    (catch Exception _
      false)))

(defn optimize-image [file-path]
  (if (convert-available?)
    (let [;extension (cstr/lower-case (re-find #"\.\w+$" file-path))
          temp-file (str "/tmp/" (UUID/randomUUID) ".jpg")]
      (sh "convert" file-path "-quality" "85%" temp-file)
      temp-file)
    file-path))

(defn get-mime-type [file-path]
  (let [extension (cstr/lower-case (re-find #"\.\w+$" file-path))]
    (if (convert-available?) "image/jpeg" ;; since we assume it will be processed
      (case extension
      ".png"  "image/png"
      ".jpg"  "image/jpeg"
      ".jpeg" "image/jpeg"
      ".gif"  "image/gif"
      ".webp" "image/webp"
      "application/octet-stream"))))

(defn encode-image [file-path]
  (println "Encoding image:" file-path)
  (let [optimized-path (optimize-image file-path)
        file (io/file optimized-path)
        bytes (byte-array (.length file))]
    (with-open [in (io/input-stream file)]
      (.read in bytes))
    (when (not= optimized-path file-path)
      (io/delete-file optimized-path))
    (.encodeToString (Base64/getEncoder) bytes)))

;; (defn get-available-models []
;;   (println "Fetching available models from Anthropic API")
;;   (try
;;     (let [response (client/get (str anthropic-base-url "models")
;;                                {:headers {"Content-Type" "application/json"
;;                                           "X-API-Key" anthropic-api-key
;;                                           "anthropic-version" "2023-06-01"}
;;                                 :throw-exceptions false
;;                                 :as :json})]
;;       (if (< (:status response) 400)
;;         (do
;;           (println "Successfully retrieved models:")
;;           (get-in response [:body :models]))
;;         (do
;;           (println "Error response from Anthropic API:")
;;           (println (:body response))
;;           (throw (ex-info "Anthropic API error" {:status (:status response)
;;                                                  :body (:body response)})))))
;;     (catch Exception e
;;       (println "Exception in get-available-models:")
;;       (println (.getMessage e))
;;       (throw (ex-info "Error fetching available models"
;;                       {:error (.getMessage e)
;;                        :data (ex-data e)})))))

;; (ut/pp [:models (get-available-models)])

(defn call-claude-api [messages assistant-name client-name thread-id]
  (println "Calling Claude API with messages:")
  (let [messages (ut/deep-remove-underscore-keys messages)
        aconfigs (assistant-configs)
        {:keys [model tools system max-tokens]} (get aconfigs assistant-name)]
    (ut/pp [:messages (ut/replace-large-base64 messages)])
    (try
      (let [response (client/post (str anthropic-base-url "messages")
                                  {:headers {"Content-Type" "application/json"
                                             "X-API-Key" anthropic-api-key
                                             "anthropic-beta" "prompt-caching-2024-07-31" ;; testing
                                             "anthropic-version" "2023-06-01"}
                                   :body (json/generate-string {:messages messages
                                                                :model model
                                                                :tools (or tools [])
                                                                :system system
                                                                :max_tokens (or max-tokens 8192)})
                                   :throw-exceptions false
                                   :as :json})]
        (if (>= (:status response) 400)
          (do
            (println "Error response from Claude API:")
            (pprint response)
            (set-running-status client-name assistant-name thread-id false)
            (swap! db/ai-worker-atom assoc-in [(keyword assistant-name) (keyword thread-id) :last]
                   (conj (vec (if (not= assistant-name "Summy")
                                (interpret-and-clean-messages (get-responses client-name assistant-name thread-id) thread-id client-name)
                                (get-responses client-name assistant-name thread-id)))
                         {:role "assistant"
                          :content (str "API Error: " (get response :body))
                          :_metadata {:created-at (System/currentTimeMillis)
                                      :msg-id (java.util.UUID/randomUUID)}}))
            (throw (ex-info "Claude API error" {:status (:status response)
                                                :body (:body response)})))
          (let [model-trunc (if (> (count model) 9)
                              (subs model 0 (- (count model) 9))
                              model)
                input-cost-per-tok (try (/ (get-in @db/model-costs ["Anthropic" model-trunc :per-mtok-input]) 1000000.0) (catch Exception e (do (ut/pp [:str (str e)]) 0)))
                output-cost-per-tok (try (/ (get-in @db/model-costs ["Anthropic" model-trunc :per-mtok-output]) 1000000.0) (catch Exception e (do (ut/pp [:str (str e)]) 0)))
                ;;_ (ut/pp [:model-trunc model-trunc (get-in @db/model-costs ["Anthropic" model-trunc :per-mtok-input])])
                in-tok (get-in response [:body :usage :input_tokens])
                out-tok (get-in response [:body :usage :output_tokens])
                sql-row {:client_name (str client-name)
                         :assistant_name assistant-name
                         :thread_id thread-id
                         :model_id model
                         :platform "Anthropic"
                         :input_tokens in-tok
                         :output_tokens out-tok
                         :input_cost (* in-tok input-cost-per-tok)
                         :output_cost (* out-tok output-cost-per-tok)}]
            ;;(ut/pp [:llm-log-sql-row sql-row])
            (sql-exec system-reporting-db (to-sql {:insert-into [:llm_log] :values [sql-row]}))
            (println "Claude API response:")
            (ut/pp [:response (ut/replace-large-base64 response)])
            response)
          ))
      (catch Exception e
        (println "Exception in call-claude-api:")
        (pprint (ex-data e))
        (set-running-status client-name assistant-name thread-id false)
        (swap! db/ai-worker-atom assoc-in [(keyword assistant-name) (keyword thread-id) :last]
               (conj (vec (if (not= assistant-name "Summy")
                            (interpret-and-clean-messages (get-responses client-name assistant-name thread-id) thread-id client-name)
                            (get-responses client-name assistant-name thread-id)))
                     {:role "assistant"
                      :content (str "Calling API Error: " (ex-data e))
                      :_metadata {:created-at (System/currentTimeMillis)
                                  :msg-id (java.util.UUID/randomUUID)}}))
        (throw (ex-info "Error calling Claude API"
                        {:error (.getMessage e)
                         :data (ex-data e)}))))))

;; Core functions
(defn start-conversation [client-name assistant-name]
  (println "Starting conversation for client:" client-name "with assistant:" assistant-name)
  (let [assistant-config (get (assistant-configs) assistant-name)
        assistant-id nil ;; for now
        _ (when-not assistant-config
            (throw (ex-info (str "Assistant config not found for: " assistant-name)
                            {:assistant-name assistant-name})))
        thread-id (generate-thread-id)
        ;;tools-map (into {} (map (fn [[k v]] [k (dissoc v :func)]) (:tools assistant-config)))
        ;; system-message {:role "user" ;; "system"
        ;;                 :content (:system-message assistant-config)}
        _ (swap! db/ai-worker-atom update-in [:threads-for client-name] ;; :ai-worker/threads-for>*client-name*
                 (fn [existing]
                   (if (nil? existing)
                     [[assistant-name assistant-id thread-id]]
                     (conj existing [assistant-name assistant-id thread-id]))))]
    (swap! db/ai-worker-atom assoc-in [(keyword assistant-name) (keyword thread-id) :last]
           [{:role (str "summoning-" assistant-name)
             :content {:summoning assistant-name
                       :description (get assistant-config :description)
                       :image (get assistant-config :image)
                       :name-style (get assistant-config :name-style)}
             :event-type "start-thread"
             :_metadata {:created-at (System/currentTimeMillis)
                         :msg-id (java.util.UUID/randomUUID)}}])
    ;(update-responses client-name assistant-name thread-id system-message)
    (update-conversation-hierarchy client-name assistant-name thread-id)
    (println "Generated session ID:" thread-id)
    thread-id))

(defn analyze-image-path [path]
  (let [file (io/file path)
        absolute-path (.getAbsolutePath file)
        current-dir (System/getProperty "user.dir")
        relative-path (if (.startsWith absolute-path current-dir)
                        (-> (Paths/get current-dir (into-array String []))
                            (.relativize (Paths/get absolute-path (into-array String [])))
                            str)
                        path)
        relative-path (if (= absolute-path relative-path) nil relative-path)] ;; if relative is not reachable from the web server, return nil
    {:absolute-path absolute-path
     :relative-path relative-path}))

(defn send-message [client-name assistant-name thread-id content & [image-path]]
  (println "Sending message for:" client-name assistant-name thread-id)
  (println "Content:" content)
  (when image-path (println "Image path:" image-path))
  (let [assistant-config (get (assistant-configs) assistant-name)
        _ (when-not assistant-config
            (throw (ex-info (str "Assistant config not found for: " assistant-name)
                            {:assistant-name assistant-name})))
        ;tools (:tools assistant-config)
        message (if image-path
                  (let [mime-type (get-mime-type image-path)]
                    {:role "user"
                     :_metadata {:image-path (analyze-image-path image-path)}
                     :content [{:type "text" :text content}
                               {:type "image"
                                :source {:type "base64"
                                         :media_type mime-type
                                         :data (encode-image image-path)}}]})
                  {:role "user" :content content})
        _ (update-responses client-name assistant-name thread-id message)
        messages (get-responses client-name assistant-name thread-id)]
    (set-running-status client-name assistant-name thread-id true)
    (update-conversation-hierarchy client-name assistant-name thread-id)
    (println "Starting message loop")
    (loop [current-messages messages
           loop-count 0]
      (println "Message loop iteration:" loop-count)
      (let [api-response (call-claude-api current-messages assistant-name client-name thread-id)
            extras {:request-time (get-in api-response [:headers "request-time"])
                    :token-usage (get-in api-response [:body :usage])
                    :rate-limit {:requests-limit (get-in api-response [:headers "anthropic-ratelimit-requests-limit"])
                                 :requests-remaining (get-in api-response [:headers "anthropic-ratelimit-requests-remaining"])
                                 :tokens-limit (get-in api-response [:headers "anthropic-ratelimit-tokens-limit"])
                                 :tokens-remaining (get-in api-response [:headers "anthropic-ratelimit-tokens-remaining"])
                                 :tokens-reset (get-in api-response [:headers "anthropic-ratelimit-tokens-reset"])}}
            body-content (get-in api-response [:body :content])
            responses (filterv #(= (:type %) "text") body-content)
            tool-requests (filterv #(= (:type %) "tool_use") body-content)
            ;parsed-response (json/parse-string api-response true)
            ;{:keys [response tool-response]} (handle-tool-usage parsed-response tools)
            ;response api-response
            text-responses (cstr/join "\n" (for [x responses] (get x :text "")))
            _ (ut/pp [:text-responses text-responses])
            _ (ut/pp [:tool-request tool-requests])]
        (update-responses client-name assistant-name thread-id {:role "assistant" :content body-content} extras)

        (if (ut/ne? tool-requests)
          (let [tool-responses (vec
                                (for [{:keys [id name input]} tool-requests
                                      :let [_ (set-running-status client-name assistant-name thread-id true)
                                            _ (ut/pp [:running-tool-request id name input])
                                            results (try
                                                      (call-function-by-name name input client-name)
                                                      (catch Throwable e {:tool-calling-error (str e)}))
                                            results (json/generate-string results)
                                            _ (ut/pp [:tool-result id results])]]
                                  {:type "tool_result"
                                   :tool_use_id id
                                   :content results}))
                _ (ut/pp [:tool-responses tool-responses])]
            (send-message client-name assistant-name thread-id tool-responses)
            (set-running-status client-name assistant-name thread-id false)
            {:text-responses text-responses
             :tool-requests tool-requests
             :tool-responses tool-responses})

          (do (set-running-status client-name assistant-name thread-id false)
            {:text-responses text-responses}))))))

(defn run-sub-conversation
  "Runs a sub-conversation using the provided assistant name and returns the result along with tracking info."
  [parent-client-name parent-assistant-name parent-thread-id sub-assistant-name initial-message]
  (println "Starting sub-conversation")
  (println "Parent info:" parent-client-name parent-assistant-name parent-thread-id)
  (println "Sub-assistant:" sub-assistant-name)
  (println "Initial message:" initial-message)

  (let [sub-client-name (str parent-client-name "-sub")
        sub-thread-id (start-conversation sub-client-name sub-assistant-name)
        parent-info {:assistant parent-assistant-name :session parent-thread-id}
        _ (update-conversation-hierarchy sub-client-name sub-assistant-name sub-thread-id parent-info)
        result (send-message sub-client-name sub-assistant-name sub-thread-id initial-message)
        tracking-info {:parent {:client parent-client-name
                                :assistant parent-assistant-name
                                :session parent-thread-id}
                       :sub {:client sub-client-name
                             :assistant sub-assistant-name
                             :session sub-thread-id}}]

    (println "Sub-conversation completed. Result:")
    (pprint result)
    (println "Tracking info:")
    (pprint tracking-info)

    (update-responses parent-client-name parent-assistant-name parent-thread-id
                      {:role "user"
                       :content (str "Sub-Thread-Request: " initial-message)}
                      {:sub-assistant sub-assistant-name})
    (update-responses parent-client-name parent-assistant-name parent-thread-id
                      {:role "assistant"
                       :content (str "Sub-Thread-Result: " (json/generate-string result))}
                      {:sub-assistant sub-assistant-name})

    {:result result
     :tracking-info tracking-info}))

(defn delete-worker-thread [client-name assistant-name thread-id]
  (swap! db/ai-worker-atom ut/dissoc-in [(keyword assistant-name) (keyword thread-id)])
  (swap! responses ut/dissoc-in [client-name assistant-name thread-id])
  (swap! running-status ut/dissoc-in [client-name assistant-name thread-id])
  (swap! db/ai-worker-atom assoc-in [:threads-for client-name]
         (filterv #(not= (last %) thread-id) (get-in @db/ai-worker-atom [:threads-for client-name])))
  (swap! conversation-hierarchy ut/dissoc-in [client-name assistant-name thread-id]))

;;; testing shit

(def test-client-name :quality-linear-ferret-34)
(def test-assistant-name "Calliope")

(comment
  (ut/pp (ut/kvpaths @db/ai-worker-atom))

  (ut/pp (get-in @db/ai-worker-atom [:threads-for :esteemed-square-pheasant-16]))

  (call-function-by-name "rvbbit-backend.assistants/get-stock-price" {:ticker "AAPL"} "client-name")

  (def main-session (start-conversation test-client-name test-assistant-name))

  ;;(ut/pp (get-in @db/ai-worker-atom [(keyword test-assistant-name) (keyword test-client-name) :last]))

  (ut/pp [:main-session main-session])

  (send-message test-client-name
                test-assistant-name
                main-session
                "What is good, my dude?")

    (send-message test-client-name
                test-assistant-name
                main-session
                "Good evening!")

  (def main-session (start-conversation test-client-name test-assistant-name))

  (send-message test-client-name
                test-assistant-name
                main-session
                "Run the quantum simulation please!")

  (ut/pp [:get-responses (get-responses test-client-name test-assistant-name main-session)])

  (ut/pp [:get-responses-unrolled (unroll-messages (get-responses test-client-name test-assistant-name main-session))])

  (send-message test-client-name
                test-assistant-name
                main-session
                "What is the latest stock price for Meta/Facebook?")

  (send-message test-client-name
                test-assistant-name
                main-session
                "Very nice, What about Tesla?")

  (send-message test-client-name
                test-assistant-name
                main-session
                "How do you feel about the quantum simulation data?")

  (send-message test-client-name
                test-assistant-name
                main-session
                "what is the user appear to be requesting here? And if so, what would your plan be to fulfill that request?"
                  ;;"/home/ryanr/rvbbit/backend/assets/screen-snaps/server-clock.jpg"
                "/home/ryanr/Desktop/Screenshot_20240921_150055.png")

    (send-message test-client-name
                test-assistant-name
                main-session
                "so we ran it twice, with the same seed and got the same results")

      (send-message test-client-name
                test-assistant-name
                main-session
                "go for it, im feeling lucky")

    (send-message test-client-name
                  test-assistant-name
                  main-session
                  "describe the image, what might it be?"
                  "/home/ryanr/rvbbit/backend/assets/screen-snaps/server-clock.jpg")

  (ut/pp [:analyze-image-path (analyze-image-path "/home/ryanr/rvbbit/backend/assets/screen-snaps/server-clock.jpg")])
  (ut/pp [:analyze-image-path (analyze-image-path "assets/screen-snaps/server-clock.jpg")])
  (ut/pp [:analyze-image-path (analyze-image-path "/home/ryanr/Desktop/Screenshot_20240921_150055.png")])


;;  [parent-client-name parent-assistant-name parent-thread-id sub-assistant-name initial-message]
  (run-sub-conversation test-client-name test-assistant-name main-session test-assistant-name "Run the quantum simulation please!")

  (send-message test-client-name
                test-assistant-name
                main-session
                "How old is Rich Hickey, creator of Clojure?")

  (send-message test-client-name
                test-assistant-name
                main-session
                "How old was he when he took a year off to create Clojure?")

  (send-message test-client-name
                test-assistant-name
                main-session
                "I need to perform a specialized task related to quantum computing.
                 Please use the run-sub-conversation tool with the specialized-assistant.")

  ;; Check the responses to see the result of the sub-conversation
  (get-responses test-client-name test-assistant-name main-session)

  ;; Check the conversation hierarchy for the main client
  (get-client-conversation-hierarchy test-client-name)

  ;; Example of using the framework for a different task
  (def research-session (start-conversation "researcher" test-assistant-name))

  (send-message "researcher"
                test-assistant-name
                research-session
                "Can you help me find recent papers on machine learning in quantum computing?")

  ;; Check the conversation hierarchy for the researcher
  (get-client-conversation-hierarchy "researcher")

  ;; Example of using an image in a conversation (assuming you have an image file)
  (def image-session (start-conversation "image-analyst" test-assistant-name))

  (send-message "image-analyst"
                test-assistant-name
                image-session
                "What can you tell me about this image?"
                "/path/to/your/image.jpg")

  ;; Check the conversation hierarchy for the image analyst
  (get-client-conversation-hierarchy "image-analyst")

  ;; Example of a conversation with multiple levels of sub-conversations
  (def complex-session (start-conversation "complex-client" test-assistant-name))

  (send-message "complex-client"
                test-assistant-name
                complex-session
                "I need to perform a series of nested specialized tasks.")

  ;; Simulate multiple levels of sub-conversations
  (let [sub1-result (run-sub-conversation "complex-client" test-assistant-name complex-session "specialized-assistant" "Perform first level task")
        sub1-session (get-in sub1-result [:tracking-info :sub :session])
        sub2-result (run-sub-conversation (str "complex-client" "-sub") "specialized-assistant" sub1-session test-assistant-name "Perform second level task")
        sub2-session (get-in sub2-result [:tracking-info :sub :session])]
    (run-sub-conversation (str "complex-client" "-sub-sub") test-assistant-name sub2-session "specialized-assistant" "Perform third level task"))

  ;; Check the complex conversation hierarchy
  (get-client-conversation-hierarchy "complex-client")
  )
