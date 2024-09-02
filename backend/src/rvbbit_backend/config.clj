(ns rvbbit-backend.config
  (:require
   ;[rvbbit-backend.db :refer [settings-atom]]
   [rvbbit-backend.assistants :as ass]
   [puget.printer           :as puget]
   [clojure.edn     :as edn]
   [clojure.java.io :as io]
   [clojure.string  :as cstr]))

(defonce settings-atom (atom {}))

(defn slurp-edn-files
  [dir]
  (let [files (->> (file-seq (io/file dir))
                   (filter #(.isFile ^java.io.File %)))]
    (reduce (fn [m file]
              (let [filename (.getName file)]
                (try (let [content  (edn/read-string (slurp file))
                           base-key (keyword (-> (str filename)
                                                 (cstr/replace ".edn" "")
                                                 (cstr/replace " " "-")))]
                       (if (map? content) (assoc m base-key content) m))
                     (catch Exception e (println "Error reading file" filename ":" (.getMessage e)) m))))
      {}
      files)))

;; (def kits (slurp-edn-files "./kits")) ;; this pathway is deprecated

;; (def kit-fns
;;   (into {}
;;         (for [[k v] kits]
;;           (into {}
;;                 (for [[kk vv] (get v :packages)] {kk (merge (merge (dissoc v :packages) {:kit-name kk :package-name k}) vv)})))))

;; (defn settings []
;;   (let [config (try (read-string (slurp "./defs/config.edn"))
;;                     (catch Exception e
;;                       (do (println [:ERROR! "defs/config.edn cannot be read!!! This is bad, please address."])
;;                           {:error (str e) :debug-level 1})))
;;         sec-edn "./defs/secrets.edn"
;;         secrets (try (read-string (slurp sec-edn))
;;                      (catch Exception _
;;                        {:no-secrets-file-found-at sec-edn}))
;;         ;; validated {:openai (when (get (list-models) :data))}
;;         openai-models (get (ass/oai-call {:url "https://api.openai.com/v1/models" :method :get} 
;;                                          (get secrets :openai-api-key) (get secrets :openai-org-id)) :data)
;;         models-valid? (true? (and (vector? openai-models) 
;;                                   (not-empty openai-models) 
;;                                   (contains? (first openai-models) :id) 
;;                                   (contains? (first openai-models) :created)))
;;         settings-map (merge config secrets {:openai-valid? models-valid?})]
;;     (reset! settings-atom settings-map)
;;     settings-map))




(def openai-cache (atom {:api-key nil :org-id nil :models nil}))

(defn settings []
  (let [config (try (read-string (slurp "./defs/config.edn"))
                    (catch Exception e
                      (do (println [:ERROR! "defs/config.edn cannot be read!!! This is bad, please address."])
                          {:error (str e) :debug-level 1})))
        sec-edn "./defs/secrets.edn"
        secrets (try (read-string (slurp sec-edn))
                     (catch Exception _
                       {:no-secrets-file-found-at sec-edn}))
        api-key (get secrets :openai-api-key)
        org-id (get secrets :openai-org-id)
        cached-data @openai-cache
        openai-models (if api-key
                        (if (and (= api-key (:api-key cached-data))
                                 (= org-id (:org-id cached-data))
                                 (some? (:models cached-data)))
                          (:models cached-data)
                          (let [new-models (get (ass/oai-call {:url "https://api.openai.com/v1/models" :method :get}
                                                              api-key org-id) :data)]
                            (reset! openai-cache {:api-key api-key :org-id org-id :models new-models})
                            new-models))
                        nil)
        models-valid? (true? (and (vector? openai-models)
                                  (not-empty openai-models)
                                  (contains? (first openai-models) :id)
                                  (contains? (first openai-models) :created)))
        settings-map (merge config secrets {:openai-valid? models-valid?})]
    (reset! settings-atom settings-map)
    settings-map))

;; (defn pp [x] (puget/cprint x))

;; (def openapi-key (get (settings) :openai-api-key))
;; (def openapi-org-id (get (settings) :openai-org-id))

;; (def test-data [{:name "Ryan1" :year 78}
;;                {:name "Ryan2" :year "70s"}
;;                {:name "Ryan3" :year "'78"}
;;                {:name "Ryan4" :year 0}
;;                {:name "Ryan5" :year 1978}
;;                {:name "Ryan6" :year "1977-1982"}
;;                {:name "Ryan7" :year "0078"}
;;                {:name "Ryan8" :year "001978"}])

;; data cleaner, partiion all value , data, and a clean command 
;; (pp (ass/oai-call {:url "https://api.openai.com/v1/chat/completions"
;;                    :method :post
;;                    :body {:model "gpt-4o"
;;                           :messages [{:role "system"
;;                                       :content (str "You are a data cleaner. You are given a dataset and instructions, 
;;                                                      you will not respond to anything, you will just clean the data and return it as valid Clojure EDN. 
;;                                                           Do NOT send backticks or language identifiers, JUST the EDN structure please. 
;;                                                      The instructions are: 'Fix the YEAR col - make a guess if you have to with the information given.
;;                                                      If a range is given, pick something in the middle - just make a reasonable assumption as to what
;;                                                      the user intended. If you CANNOT decide or there is not enough information, 
;;                                                      use -1 as the answer for that column.'"
;;                                                     "here is the data: " (pr-str test-data))}]}}
;;                   (get (settings) :openai-api-key)
;;                   (get (settings) :openai-org-id)))
