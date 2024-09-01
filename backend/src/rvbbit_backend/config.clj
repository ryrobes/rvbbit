(ns rvbbit-backend.config
  (:require
   ;[rvbbit-backend.db :refer [settings-atom]]
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

(defn settings []
  (let [config (try (read-string (slurp "./defs/config.edn"))
                    (catch Exception e
                      (do (println [:ERROR! "defs/config.edn cannot be read!!! This is bad, please address."])
                          {:error (str e) :debug-level 1})))
        sec-edn "./defs/secrets.edn"
        secrets (try (read-string (slurp sec-edn))
                     (catch Exception _
                       {:no-secrets-file-found-at sec-edn}))
        settings-map (merge config secrets)]
    (reset! settings-atom settings-map)
    settings-map))
    