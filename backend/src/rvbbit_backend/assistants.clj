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
   [rvbbit-backend.config   :as config]
   [rvbbit-backend.util     :as ut]
   [rvbbit-backend.websockets :as wss]
   ; [wkok.openai-clojure.api :as api]
   ))



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

(def assistant-instructions
  (slurp "defs/fabric/clover-canvas/context.md"))

(defn create-assistant
  "Creates a new assistant with the specified instructions and capabilities."
  []
  (api-request :post "/assistants"
               {:model "gpt-4o" ;; "o1-preview" ;; "gpt-4o" ; "o1-mini" ; "gpt-4-1106-preview"
                :name "Calliope"
                ;:top_p 1.0
                ;:temperature 1.0
                ;:response_format {:type "json_object"}
                :instructions assistant-instructions
                :tools [{:type "code_interpreter"} ]}))

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

;; Message management

(defn add-message
  "Adds a new message to a thread."
  [thread-id content]
  (api-request :post (str "/threads/" thread-id "/messages")
               {:role "user"
                :content content}))

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
  [thread-id file-path]
  (let [upload-response (client/post (str base-url "/files")
                                   {:headers {"Authorization" (str "Bearer " api-key)}
                                    :multipart [{:name "purpose" :content "assistants"}
                                                {:name "file"
                                                 :content (clojure.java.io/file file-path)}]
                                    :as :json})
        file-id (-> upload-response :body :id)]
    (when file-id
      (api-request :post (str "/threads/" thread-id "/messages")
                   {:role "user"
                    :content [{:type "file_attachment"
                               :file_id file-id}]})
      {:status :success
       :message "File uploaded and added to thread"
       :file-id file-id})))

(defn upload-and-add-image
  "Uploads an image file and adds it to the specified thread."
  [thread-id image-path]
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
      (api-request :post (str "/threads/" thread-id "/messages")
                   {:role "user"
                    :content [{:type "image_file"
                               :image_file {:file_id file-id}}]})
      {:status :success
       :message "Image uploaded and added to thread"
       :file-id file-id})))

(defn add-image-to-thread
  "Adds an uploaded image to the thread."
  [thread-id file-id]
  (add-message thread-id
               [{:type "image_file"
                 :image_file {:file_id file-id}}]))

;; Context and execution management

(declare poll-run-until-complete handle-required-actions)

(defn add-context
  "Adds context to the thread without expecting a response."
  [thread-id content]
  (add-message thread-id content))

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

(defn request-from-assistant
  "Sends a request to the assistant and handles both DSL code and chat responses,
   with improved error handling and content parsing."
  [thread-id assistant-id prompt]
  (println "Sending prompt to assistant:" prompt)
  (add-message thread-id prompt)
  (let [run (create-run thread-id assistant-id)
        _ (println "Run created:" (:id run))
        response (<!! (poll-run-until-complete thread-id (:id run)))
        _ (println "Raw response:" response)
        extracted-content (extract-edn-code response)
        _ (println "Extracted content:" extracted-content)]
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
      (wss/push-to-client [:ui-keypath :not-needed-here] [] :acclaimed-burgundy-cattle-33 1 :push-assocs dsl-data)
      {:result "DSL execution result placeholder"})
    {:error "Invalid DSL data" :data dsl-data}))

;; Main workflow function

(defn assistant-workflow
  "Handles the complete workflow of interacting with the assistant,
   including DSL code generation, execution, and chat responses."
  [assistant-id thread-id prompt]
  ;(println "Starting assistant workflow with prompt:" prompt)
  (let [response (request-from-assistant thread-id assistant-id prompt)]
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

;; Example usage


(defn runstream-boot-agent
  "Demonstrates the usage of the assistant for both DSL code generation and analysis."
  []
  (try
    (let [assistant (create-assistant)
          _ (println "Assistant created:" (:id assistant))
          assistant-id (:id assistant)
          thread (create-thread)
          _ (println "Thread created:" (:id thread))
          thread-id (:id thread)]

      ;; Request DSL code
      (let [result (assistant-workflow assistant-id thread-id ;; example voice prompt w auto metadata
                                       "Create some new group by blocks from this table of low cardinality dimensions, and then create graphs for each of them. 
                                        I'm trying to see the distribution of crimes in this particular data set, and then also create a block that explains 
                                         your findings, please. Also, use vega-lite, but optimize for a dark background and theme please. 
                                        The fields in the viz need to map with the query fields.
                                        SINCE YOU ARE ONLY RETURNING EDN, NEVER USE BACKTICKS OR LANGUAGE HINTS. JUST THE ACTUAL EDN MAP ITSELF. NO BACKTICKS.
                                        MAKE SURE THE BRACKETS ARE CLOSED AND BALANCED.\n\n\n\n 
                                                                               ##QUERY/TABLE METADATA: \n{:connection-id \"boston-crime\", :database-type \"SQLite\", :fields 
                                                                               {:SHOOTING {:min \"\", :data-type \"unknown\", :commons {nil 500}, :distinct 1, :group-by? true, :median \"\", :max \"\", :avg \" 
                                                                               (non-numeric average)\", :cardinality 0}, :DISTRICT {:min \"\", :data-type \"string\", :commons {\"C11\" 82, \"B2\" 75, \"B3\" 49}, 
                                                                               :distinct 13, :group-by? true, :median \"C11\", :max \"E5\", :avg \"D14 (non-numeric average)\", :cardinality 2}, :INCIDENT_NUMBER 
                                                                               {:min \"I182070372\", :data-type \"string\", :commons {\"I182070397\" 5, \"I182070779\" 4, \"I182070889\" 3}, :distinct 454, 
                                                                               :group-by? true, :median \"I182070648\", :max \"I182070945\", :avg \"I182070945 (non-numeric average)\", :cardinality 90}, 
                                                                               :OFFENSE_DESCRIPTION {:min \"AFFRAY\", :data-type \"string\", :commons {\"SICK/INJURED/MEDICAL - PERSON\" 43, \"VERBAL DISPUTE\" 34,
                                                                               \"INVESTIGATE PERSON\" 33}, :distinct 89, :group-by? true, :median \"M/V - LEAVING SCENE - PROPERTY DAMAGE\", :max \"WEAPON - OTHER - OTHER VIOLATION\", 
                                                                               :avg \"LARCENY ALL OTHERS (non-numeric average)\", :cardinality 17}, :DAY_OF_WEEK {:min \"0001-01-02\", :data-type \"string\", 
                                                                               :commons {\"0001-01-07\" 228, \"0001-01-08\" 178, \"0001-01-06\" 75}, :distinct 7, :group-by? true, :median \"0001-01-07\", 
                                                                               :max \"0001-01-08\", :avg \"0001-01-07 (non-numeric average)\", :cardinality 1}, :MONTH {:min 6, :data-type \"integer\", 
                                                                               :commons {9 477, 8 21, 7 1}, :distinct 4, :group-by? true, :median 9, :max 9, :avg 8.948, :cardinality 0}, :YEAR {:min 2018, 
                                                                               :data-type \"integer\", :commons {2018 500}, :distinct 1, :group-by? true, :median 2018, :max 2018, :avg 2018, :cardinality 0}, 
                                                                               :HOUR {:min 0, :data-type \"integer\", :commons {18 45, 14 33, 20 32}, :distinct 24, :group-by? true, :median 18, :max 23, :avg 13.43, 
                                                                               :cardinality 4}, :STREET {:min \"\", :data-type \"string\", :commons {nil 41, \"WASHINGTON ST\" 20, \"HUNTINGTON AVE\" 12}, :distinct 275, 
                                                                               :group-by? true, :median \"HILLSIDE ST\", :max \"WOODROW AVE\", :avg \"LINCOLN ST (non-numeric average)\", :cardinality 55}, 
                                                                               :OFFENSE_CODE_GROUP {:min \"Aggravated Assault\", :data-type \"string\", :commons {\"Motor Vehicle Accident Response\" 69,
                                                                               \"Medical Assistance\" 50, \"Larceny\" 41}, :distinct 41, :group-by? true, :median \"Motor Vehicle Accident Response\",
                                                                               :max \"Warrant Arrests\", :avg \"Larceny (non-numeric average)\", :cardinality 8}, :UCR_PART {:min \"Other\", 
                                                                               :data-type \"string\", :commons {\"Part Three\" 265, \"Part Two\" 140, \"Part One\" 93}, :distinct 4, :group-by? true, 
                                                                               :median \"Part Three\", :max \"Part Two\", :avg \"Part One (non-numeric average)\", :cardinality 0}, :Long {:min \"\", 
                                                                               :data-type \"float\", :commons {-1 6, -71.1136693 5, nil 5}, :distinct 413, :group-by? true, :median \"-71.07849582\", 
                                                                               :max \"-71.17087959\", :avg \"-71.13937053 (non-numeric average)\", :cardinality 82}, :REPORTING_AREA {:min \"\", 
                                                                               :data-type \"integer\", :commons {nil 42, 519 6, 177 5}, :distinct 282, :group-by? true, :median \"361\", :max \"957\", 
                                                                               :avg \"808 (non-numeric average)\", :cardinality 56}, :Location {:min \"(-1.00000000, -1.00000000)\", :data-type \"string\", 
                                                                               :commons {\"(-1.00000000, -1.00000000)\" 6, \"(0.00000000, 0.00000000)\" 5, \"(42.24635332, -71.11366930)\" 5}, :distinct 413, 
                                                                               :group-by? true, :median \"(42.32177032, -71.09779774)\", :max \"(42.38512136, -71.01150101)\", 
                                                                               :avg \"(42.35779134, -71.13937053) (non-numeric average)\", :cardinality 82}, :OFFENSE_CODE {:min 301, :data-type \"integer\", 
                                                                               :commons {3006 43, 3301 34, 3115 33}, :distinct 89, :group-by? true, :median 3203, :max 3831, :avg 2357.544, :cardinality 17}, 
                                                                               :Lat {:min \"\", :data-type \"float\", :commons {-1 6, 42.24635332 5, nil 5}, :distinct 413, :group-by? true, 
                                                                               :median \"42.32177032\", :max \"42.38512136\", :avg \"42.35779134 (non-numeric average)\", :cardinality 82}, 
                                                                               :OCCURRED_ON_DATE {:min \"2018-06-15 00:00:00.000000\", :data-type \"string\", 
                                                                               :commons {\"2018-09-02 00:00:00.000000\" 5, \"2018-09-03 10:00:00.000000\" 5, \"2018-09-01 17:03:00.000000\" 5}, 
                                                                               :distinct 404, :group-by? true, :median \"2018-09-02 18:20:00.000000\", :max \"2018-09-03 21:25:00.000000\", 
                                                                               :avg \"2018-09-02 13:00:00.000000 (non-numeric average)\", :cardinality 80}}}\n\n 
                                                                               ##CANVAS-COORDS-USED: \n[[3 2 7 30]]\n\n 
                                                                               ##CANVAS-SIZE: \n[57.42 32.76]\n\n 
                                                                               {[:panels :block-10928] \"{:h 7, :w 30, :connection-id \"boston-crime\", :name \"select-all-offenses\", 
                                                                               :queries {:offenses-drag-31 {:select [:DAY_OF_WEEK :DISTRICT :HOUR :INCIDENT_NUMBER :Lat :Location :Long :MONTH 
                                                                               :OCCURRED_ON_DATE :OFFENSE_CODE :OFFENSE_CODE_GROUP :OFFENSE_DESCRIPTION :REPORTING_AREA :SHOOTING :STREET :UCR_PART 
                                                                               :YEAR], :from [[:offenses :cc23]]}}, :root [3 2], :tab \"round leopard\"}\"} \n\n ")]
        (println "Result of DSL request:")
        (ut/pp [:res1 result]))

      ;; Ask for analysis
      ;; (let [result (assistant-workflow assistant-id thread-id
      ;;                                  "Analyze the dashboard we just created. What insights can we draw?")]
      ;;   (println "Result of analysis request:")
      ;;   (ut/pp [:res2 result]))

      ;; Clean up
      ;(delete-thread thread-id)
      )
    (catch Exception e
      (let [data (ex-data e)]
        (println "An error occurred:")
        (println "Message:" (.getMessage e))
        (when data
          (println "Additional error data:")
          (ut/pp data))
        (.printStackTrace e)))))

;; (ut/pp [:assistants "runstream-boot-agent"])

;; (runstream-boot-agent)

