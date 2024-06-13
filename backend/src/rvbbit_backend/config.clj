(ns rvbbit-backend.config
  (:require
    [clojure.edn     :as edn]
    [clojure.java.io :as io]
    [clojure.string  :as cstr]))

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

(def kits (slurp-edn-files "./kits"))

(def kit-fns
  (into {}
        (for [[k v] kits]
          (into {}
                (for [[kk vv] (get v :packages)] {kk (merge (merge (dissoc v :packages) {:kit-name kk :package-name k}) vv)})))))

(defn settings [] (try (read-string (slurp "./defs/config.edn")) (catch Exception e {:error (str e) :debug-level 1})))