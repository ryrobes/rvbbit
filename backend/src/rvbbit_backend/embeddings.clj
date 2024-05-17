(ns rvbbit-backend.embeddings
  (:require [wkok.openai-clojure.api :as api]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            ;;[durable-atom.core :refer [durable-atom]]
            [rvbbit-backend.util :as ut]
            [rvbbit-backend.config :as config]))

;; note to future Ryan, duratom is a fucking landmine. was causing 100% cpu usage and weird thread race conditions in websockets
;; if you choose to keep using this... BE WARNED! 2/23/24

;; (def openapi-key (get config/settings :openapi-key))
;; (def openapi-org-id (get config/settings :openapi-org-id))

;; (defonce cosine-cache (durable-atom "./data/cosine-cache.edn"))
;; (defonce embedding-cache (durable-atom "./data/embedding-cache.edn"))
;; (defonce selected-recos-embeddings (durable-atom "./data/selected-recos-embeddings.edn"))
;(defonce selected-recos (durable-atom "./data/selected-recos.edn"))
;;(defonce selected-recos (atom (edn/read-string (slurp "./data/selected-recos.edn"))))
(def selected-recos (ut/thaw-atom {} "./data/selected-recos.edn"))
(reset! selected-recos (distinct @selected-recos)) ;; we should be reducing to a multiplier, but this is fine for now.

;; (defonce found-fields-embeddings-input (durable-atom "./data/found-fields-embeddings.edn"))
;; (defonce found-fields-embeddings-scores (durable-atom "./data/found-fields-embeddings-scores.edn"))

;; (defonce combos-embeddings-input (durable-atom "./data/combos-embeddings.edn"))
;; (defonce combos-embeddings-scores (durable-atom "./data/combos-embeddings-scores.edn"))

;(def a (durable-atom "./data/atomy.edn"))
;(reset! a {:foo "bar"})

;(ut/pp [:size-of-cosine-cache (count @cosine-cache)])
;(ut/pp [:size-of-embedding-cache (count @embedding-cache)])
;; (ut/pp [:reco-history-weight (count @selected-recos)])
;; (ut/pp [:selected-recos-debug (take 10 @selected-recos)]) ;; pre-reduce and then use multiplier in calcs
;; (ut/pp [:selected-recos-debug (take 10 (frequencies @selected-recos))])
;; (ut/pp [:selected-recos-debug (take 10 (vec (for [[k v] (frequencies @selected-recos)] (assoc k :freq v))))]) ;; pre-reduce and then use multiplier in calcs

;; (defn embedding [m]
;;   (if (get @embedding-cache (hash m))
;;     (do ;(ut/pp "CACHE")
;;       (get @embedding-cache (hash m)))
;;     (let [e (get-in (api/create-embedding {:model "text-embedding-ada-002"
;;                                            :input (pr-str m)}
;;                                           {:api-key openapi-key}) [:data 0 :embedding])]
;;       ;(ut/pp "$$$$")
;;       (swap! embedding-cache assoc (hash m) e)
;;       e)))

;; (defn dot-product [v1 v2]
;;   (reduce + (map * v1 v2)))

;; (defn magnitude [v]
;;   (Math/sqrt (dot-product v v)))

;; (defn cosine-similarity [v1 v2]
;;   ;(ut/pp [v1 v2])
;;   (if (get @cosine-cache [v1 v2])
;;     (get @cosine-cache [v1 v2])
;;     (let [cos (/ (dot-product v1 v2)
;;                  (* (magnitude v1) (magnitude v2)))]
;;       (swap! cosine-cache assoc [v1 v2] cos)
;;       cos)))



;; (defn similarities [current-embedding saved-embeddings]
;;   (map #(cosine-similarity current-embedding %) saved-embeddings))

;; (defn most-similar [current-embedding saved-embeddings]
;;   (apply max-key #(cosine-similarity current-embedding %) saved-embeddings))


