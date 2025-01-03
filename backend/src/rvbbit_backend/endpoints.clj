(ns rvbbit-backend.endpoints
  (:require [clojure.string :as cstr]
            [clj-http.client :as client]
            [clojure.data.json :as json]
            [fast-edn.core :as edn]
            [rvbbit-backend.config   :as config]
            [rvbbit-backend.util :as ut]
            [clojure.walk :as walk]))

(defn clean-keyword [s]
  (-> s
      (cstr/lower-case)
      (cstr/replace #"[^\w\s-]" "")
      (cstr/replace #"\s+" "-")
      (keyword)))

(defn keywordize-keys-custom [m]
  (let [f (fn [[k v]]
            [(if (string? k) (clean-keyword k) k) v])]
    (walk/postwalk (fn [x]
                     (if (map? x)
                       (into {} (map f x))
                       x))
                   m)))

(defn ensure-vector-map [data]
  (walk/postwalk (fn [x]
                   (cond
                     (list? x) (vec x)
                     (set? x) (vec x)
                     :else x))
                 data))

(defn format-select
  "Formats the select clause for query params"
  [select]
  (cstr/join "," (map (fn [k] (cstr/replace (name k) #">" ".")) select)))

(defn filter-response [response select]
  (if (seq select)
    (let [paths (map #(cstr/split (name %) #">") select)]
      (reduce (fn [acc path]
                (assoc-in acc (map keyword path) (get-in response (map keyword path))))
              {}
              paths))
    response))

(defn flatten-response [response]
  (letfn [(flatten-map [m prefix]
            (mapcat (fn [[k v]]
                      (let [new-key (if prefix
                                      (keyword (str (name prefix) "_" (name k)))
                                      k)]
                        (if (map? v)
                          (flatten-map v new-key)
                          [[new-key v]])))
                    m))]
    (if (map? response)
      [(into {} (flatten-map response nil))]
      (mapv #(into {} (flatten-map % nil)) response))))

(defn api-query
  [{:keys [endpoint where select method body params headers pretty-spit post-process-fn flatten?] :or {method :get flatten? false}}]
  (try
    (let [base-url (str endpoint)
          query-params (merge
                        (when where
                          (->> where
                               (map (fn [[k v]] [(name k) v]))
                               (into {})))
                        params)
          _ (ut/pp [:api-call-params query-params] {:width 50})
          select-param (when select (format-select select))
          all-params (cond-> query-params
                       select-param (assoc :fields select-param))
          api-fn (case method
                   :get client/get
                   :post client/post
                   :put client/put
                   :delete client/delete)
          request-options (cond-> {:query-params all-params
                                   :headers headers
                                   :throw-exceptions false
                                   :as :json}
                            body (assoc :form-params body))]
      (try
        (let [response (api-fn base-url request-options)
              body (-> (:body response)
                       keywordize-keys-custom
                       ensure-vector-map)
              result (cond-> (filter-response body select)
                       flatten? flatten-response)
              _ (when post-process-fn (ut/pp [:running-post-process-fn (str post-process-fn)] {:width 50}))
              result (if (not (nil? post-process-fn))
                       (try ((eval post-process-fn) result)
                         ;;(post-process-fn result)
                            (catch Throwable e
                              (let [data (ex-data e)
                                    res [{:api-query-error "post-process-fn error" (get data :cause) (str e)}]]
                                (ut/pp [:api-query-post-process-fn-error res] {:width 50})
                                res)))
                       result)]
          (when (and pretty-spit (cstr/ends-with? (str pretty-spit) ".edn"))
            (try
              (do (ut/pp [:saving-api-call-result-as pretty-spit] {:width 50})
                  (ut/pretty-spit pretty-spit result 120))
              (catch Exception e (ut/pp [:save-request-failed (str e)] {:width 50}))))
          ;(ut/pp [:result-sample-5 (ut/limited result 5)])
          (if (< (:status response) 400)
            result
            (throw (ex-info "API request failed"
                            {:status (:status response)
                             :body body
                             :headers (:headers response)
                             :request-url base-url
                             :request-params all-params}))))
        (catch Exception e
          (let [data (ex-data e)
                emap {:cause (.getMessage e)
                      :data data
                      :request-url base-url
                      :request-params all-params}]
            (throw (ex-info "API request error"
                            emap))))))

    (catch Exception e
      (let [data (ex-data e)
            emap {:cause (.getMessage e)
                  ;; :data (assoc-in data [:data :body]
                  ;;                 (-> (get-in data [:data :body])
                  ;;                     json/read-str
                  ;;                     edn/read-string
                  ;;                     walk/keywordize-keys))
                  :data data
                  }]
        (ut/pp {"api-query functions call error" [(-> (get-in emap [:data :data :body])   )
                                                  emap]} {:width 50})
        (throw (ex-info "api-query functions call error"
                        emap))))))

;; (comment

;;   (ut/pp (keys api-data))

;;   (ut/pp endpoints)

;;   (ut/pp (read-api-data api-data))

;;   (ut/pp (api-query {:endpoint "https://owen-wilson-wow-api.onrender.com/wows/random"
;;                        ;:where {:active true}
;;                        ;:params {:limit 10 :offset 0}
;;                        ;:select [:name :email :address>city :address>country]
;;                      :method :get
;;                      :flatten? true}))

;;   (api-query {:endpoint "https://api.example.com/users"
;;               :where {:active true}
;;               :params {:limit 10 :offset 0}
;;               :select [:name :email :address>city :address>country]
;;               :method :get
;;               :flatten? true})

;;   (api-query {:endpoint "https://api.example.com/users"
;;               :body {:name "Alice" :email "alice@example.com" :age 30}
;;               :method :post
;;               :headers {"Authorization" "Bearer mytoken"}})
;;   )

