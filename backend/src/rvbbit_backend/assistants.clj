(ns rvbbit-backend.assistants
  (:require
    [cheshire.core           :as json]
    [clj-http.client         :as client]
    [clojure.edn             :as edn]
    [clojure.java.io         :as io]
    [clojure.java.shell      :as shell]
    [clojure.pprint          :as pp]
    [clojure.string          :as cstr]
    ;[rvbbit-backend.config   :as config]
    ;[rvbbit-backend.util     :as ut]
   ; [wkok.openai-clojure.api :as api]
   ))

;; (def openapi-key (get (config/settings) :openai-api-key))
;; (def openapi-org-id (get (config/settings) :openai-org-id))

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





