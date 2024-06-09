(ns rvbbit-backend.external
  (:require
    [clojure.core.async   :as async]
    [clojure.data         :as data]
    [clojure.java.io      :as io]
    [clojure.set          :as cset]
    [clojure.string       :as cstr]
    [rvbbit-backend.util  :as ut]
    [websocket-layer.core :as wl]))

(defn pretty-spit
  [file-name collection]
  (spit (java.io.File. file-name)
        (with-out-str (clojure.pprint/write collection :dispatch clojure.pprint/code-dispatch))))


(def client-pushes (atom {}))

(defn fixstr [s] (cstr/replace (str s) #":" ""))

(defn get-all-paths
  [root-path]
  (let [root-file (io/file root-path)
        root-path (-> root-file
                      .getCanonicalFile
                      .getPath)]
    (->> root-file
         file-seq
         (map #(cstr/replace % root-path ""))
         vec)))


(defn get-subdirs
  [dir]
  (let [dir-file (io/file dir)
        files    (or (.listFiles dir-file) [])]
    (->> files
         (filter #(.isDirectory %))
         (map #(.getName %))
         vec)))

(defn delete-if-empty
  [dir]
  (let [dir-file (io/file dir)]
    (when (and (.isDirectory dir-file) (empty? (.list dir-file)))
      (try (do (ut/pp [:**cleanup! :deleting-empty dir]) (io/delete-file dir-file))
           (catch Exception e (ut/pp [:error-deleting-empty dir e]))))))

(defn cleanup
  [extra-files dirs]
  (doseq [file extra-files]
    (try (do (ut/pp [:**cleanup! :deleting file]) (io/delete-file file))
         (catch Exception e (ut/pp [:error-deleting file :err e]))))
  (doseq [dir dirs] (delete-if-empty dir)))

(defn create-dirs [path] (when (not (.exists (io/file path))) (.mkdirs (io/file path))))


(defn read-dir
  [dir]
  (let [files    (get-all-paths dir)
        repacked (into {}
                       (for [file  files
                             :let  [relative-path (cstr/replace file dir "")
                                    keyname       (keyword (cstr/replace (str (last (cstr/split
                                                                                      relative-path
                                                                                      #"/")))
                                                                         ".edn"
                                                                         ""))
                                    file-data     (try (when (.exists (io/file file))
                                                         (read-string (slurp file)))
                                                       (catch Exception e
                                                         (do (ut/pp [:cannot-read-existing file e])
                                                             {})))]
                             :when file-data]
                         [keyname file-data]))]
    repacked))

(defn repack-block
  [client-name tab block-name panel-key]
  (let [client-dir    (str "./live/" (fixstr client-name) "/")
        block-dir     (str client-dir "/" tab "/" block-name "/")
        files         (get-all-paths block-dir)
        root-file     (str "./live/" (fixstr client-name) ".edn")
        original-name (get-in (try (read-string (slurp root-file))
                                   (catch Exception e {:invalid-edn! (str e)}))
                              [panel-key :name]
                              block-name)
        q             (atom {})
        v             (atom {})
        repack        (into {}
                            (for [file  files
                                  :let  [relative-path (cstr/replace file block-dir "")
                                         keyname       (keyword (cstr/replace (str (last
                                                                                     (cstr/split
                                                                                       relative-path
                                                                                       #"/")))
                                                                              ".edn"
                                                                              ""))
                                         subkey        (cond (cstr/includes? (str relative-path)
                                                                             "/queries/")
                                                               q
                                                             (cstr/includes? (str relative-path)
                                                                             "/views/")
                                                               v
                                                             :else nil)
                                         file-data     (try (when (.exists (io/file file))
                                                              (read-string (slurp file)))
                                                            (catch Exception _ nil))]
                                  :when file-data]
                              (if subkey
                                (do (swap! subkey merge {keyname file-data}) {})
                                {keyname file-data})))
        repack        (-> repack
                          (assoc :queries @q)
                          (assoc :views @v))
        base-map      (get repack panel-key)]
    (-> (merge base-map (dissoc repack panel-key))
        (assoc :tab tab)
        (assoc :name original-name))))





(def file-work-agent (agent nil))

(defn write-panels
  [client-name panels]
  (pretty-spit (str "./live/" (fixstr client-name) ".edn") panels) ;; overwrite existing
  (let [name-mapping-raw (into {} (for [[k v] panels] {k (get v :name)}))
        name-mapping     (ut/generate-unique-names name-mapping-raw)
        rev-name-mapping (ut/reverse-map name-mapping)
        client-name      (fixstr client-name)
        panel-cnt        (count (keys panels))
        path-history     (atom [])
        client-dir       (str "./live/" client-name "/")]
    (swap! client-pushes assoc client-name (System/currentTimeMillis))
    (create-dirs client-dir) ;; create base client folder root if not exists
    (doseq [[k v] panels ;; for each panel
            :when (map? v)
            :let  [;_ (ut/pp [:working-on k v])
                   board-name (get v :tab)
                   block-dir (str client-dir "/" board-name "/" (get name-mapping k) "/")
                   block-file-base (str block-dir (fixstr k) ".edn")
                   naked-incoming-block-data (-> v
                                                 (dissoc :views)
                                                 (dissoc :tab)
                                                 (dissoc :name)
                                                 (dissoc :queries))
                   naked-incoming-views (get v :views {})
                   naked-incoming-queries (get v :queries {})
                   existing-file-data (try (when (.exists (io/file block-file-base))
                                             (read-string (slurp block-file-base)))
                                           (catch Exception e
                                             (do (ut/pp [:cannot-read-existing block-file-base e])
                                                 {})))
                   existing-file-data-base (-> existing-file-data
                                               (dissoc :views)
                                               (dissoc :tab)
                                               (dissoc :queries))
                   base-exists? (and (.exists (io/file block-file-base))
                                     (= naked-incoming-block-data existing-file-data-base))]]
      (swap! path-history conj block-file-base)
      (if (not base-exists?)
        (do ;(ut/pp [:file-unpacker! k :base client-name
          (create-dirs block-dir)
          (pretty-spit block-file-base naked-incoming-block-data)))
      (when (not (empty? naked-incoming-views))
        (doseq [[kk vv] naked-incoming-views
                :let    [view-base          (str block-dir "/views/")
                         view-file-base     (str view-base (fixstr kk) ".edn")
                         existing-file-data (try (when (.exists (io/file view-file-base))
                                                   (read-string (slurp view-file-base)))
                                                 (catch Exception e
                                                   (do (ut/pp [:cannot-read-existing view-file-base
                                                               e])
                                                       {})))
                         base-exists?       (and (.exists (io/file view-file-base))
                                                 (= vv existing-file-data))]]
          (swap! path-history conj view-file-base)
          (if (not base-exists?)
            (do ;(ut/pp [:file-unpacker! k kk :views client-name
              (create-dirs view-base)
              (pretty-spit view-file-base vv)))))
      (when (not (empty? naked-incoming-queries))
        (doseq [[kk vv] naked-incoming-queries
                :let    [query-base         (str block-dir "/queries/")
                         query-file-base    (str query-base (fixstr kk) ".edn")
                         existing-file-data (try (when (.exists (io/file query-file-base))
                                                   (read-string (slurp query-file-base)))
                                                 (catch Exception e
                                                   (do (ut/pp [:cannot-read-existing query-file-base
                                                               e])
                                                       {})))
                         base-exists?       (and (.exists (io/file query-file-base))
                                                 (= vv existing-file-data))]]
          (swap! path-history conj query-file-base)
          (if (not base-exists?)
            (do ;(ut/pp [:file-unpacker! k kk :queries client-name
              (create-dirs query-base)
              (pretty-spit query-file-base vv))))))
    (let [extras      (vec (cset/difference (set (get-all-paths client-dir))
                                            (set (mapv #(cstr/replace % "//" "/") @path-history))))
          extra-files (filterv #(cstr/ends-with? % ".edn") extras)
          dirs        (filterv #(not (cstr/ends-with? % ".edn")) extras)]
      (cleanup extra-files dirs))))




