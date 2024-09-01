(ns rvbbit-backend.embeddings
  (:require
    [clojure.edn             :as edn]
    [clojure.java.io         :as io]
    [rvbbit-backend.config   :as config]
    [rvbbit-backend.freezepop   :as fpop]
    [rvbbit-backend.util     :as ut]
    [wkok.openai-clojure.api :as api]))

(def selected-recos (atom {})) ;; (fpop/thaw-atom {} "./data/selected-recos.edn"))
;;(reset! selected-recos (distinct @selected-recos)) ;; we should be reducing to a multiplier, but this is fine for now.














