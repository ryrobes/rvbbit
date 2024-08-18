(ns rvbbit-backend.external
  (:require
   [clojure.core.async   :as async]
   [clojure.data         :as data]
   [clojure.edn          :as edn]
   [clojure.java.io      :as io]
   [clojure.set          :as cset]
   [clojure.string       :as cstr]
   [rvbbit-backend.util  :as ut]
   [rvbbit-backend.config  :as cfg]
   [clojure.pprint :as ppr]
   [websocket-layer.core :as wl])
  (:import    [java.security MessageDigest]
              [java.math BigInteger]))

(defonce file-versions (atom {}))



;; (ut/pp [:file-versions @file-versions])


;; (defn calculate-hash [^String data]
;;   (let [md (MessageDigest/getInstance "SHA-256")]
;;     (.update md (.getBytes data "UTF-8"))
;;     (format "%064x" (BigInteger. 1 (.digest md)))))

(defn calculate-file-hash [file]
  (hash (slurp file)))

(defn calculate-data-hash [data]
  (try (hash (pr-str data)) 
       (catch Exception _ -1)))

(defn update-client-version [file-path data]
  (let [new-hash (calculate-data-hash data)]
    (swap! file-versions update file-path
           (fn [versions]
             (assoc versions :client new-hash)))))

(defn update-server-version [file-path data]
  (let [new-hash (calculate-data-hash data)]
    (swap! file-versions update file-path
           (fn [versions]
             (assoc versions :server new-hash)))))

(defn local-edit? [file-path]
  (let [versions (get @file-versions file-path)
        current-hash (calculate-file-hash (io/file file-path))]
    (and (not= current-hash (:client versions))
         (not= current-hash (:server versions)))))

(defn pretty-spit [file-name collection & [src]]
  (let [src (or src :server)
        ;data-hash (calculate-data-hash collection)
        ]
    ;;(swap! file-versions assoc-in [file-name src] data-hash)  
      (spit (java.io.File. file-name)
        (with-out-str (ppr/write collection
                                 :dispatch ppr/code-dispatch)))))



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
      (try (do (ut/pp [:**clean-up! :deleting-empty dir]) 
               (io/delete-file dir-file))
           (catch Exception e (ut/pp [:error-deleting-empty dir e]))))))

(defn cleanup
  [extra-files dirs]
  (doseq [file extra-files]
    (try (do (ut/pp [:**clean-up! :deleting file]) 
             (io/delete-file file))
         (catch Exception e (ut/pp [:error-deleting file :err e]))))
  (doseq [dir dirs] (delete-if-empty dir)))

(defn create-dirs [path] (when (not (.exists (io/file path))) (.mkdirs (io/file path))))


(defn read-dir
  [dir]
  (let [files    (get-all-paths dir)
        repacked (into {}
                       (for [file  files
                             :let  [relative-path (cstr/replace file dir "")
                                    keyname       (keyword (cstr/replace (str (last (cstr/split relative-path #"/"))) ".edn" ""))
                                    file-data     (try (when (.exists (io/file file)) (read-string (slurp file)))
                                                       (catch Exception e (do (ut/pp [:cannot-read-existing file e]) {})))]
                             :when file-data]
                         [keyname file-data]))]
    repacked))

(defn repack-block-old [client-name tab block-name panel-key]
  (let [client-dir    (str "./live/" (fixstr client-name) "/")
        block-dir     (str client-dir "/" tab "/" block-name "/")
        files         (filter #(not (or (cstr/ends-with? % ".swp")
                                        ;(cstr/starts-with? % ".")
                                        ))
                              (get-all-paths block-dir))
        root-file     (str "./live/" (fixstr client-name) ".edn")
        original-name (get-in (try (read-string (slurp root-file)) (catch Exception e {:invalid-edn! (str e)}))
                              [panel-key :name]
                              block-name)
        runners       (into {} (for [[k v] (get (cfg/settings) :runners {})] {k (get v :ext ".edn")})) ;; new mapping {:type ".ext"}
        q             (atom {}) ;; old hardcoded mapping for :views (as it would exist in the new mapping)
        v             (atom {}) ;; old hardcoded mapping for :queries (as it would exist in the new mapping)
        repack        (into {}
                            (for [file  files
                                  :let  [relative-path (cstr/replace file block-dir "")
                                         keyname       (keyword
                                                        (cstr/replace (str (last (cstr/split relative-path #"/"))) ".edn" "")) ;; the file name with will become the item key 
                                         subkey        (cond (cstr/includes? (str relative-path) "/queries/") q ;; hardcoded :queries
                                                             (cstr/includes? (str relative-path) "/views/")   v ;; hardcoded :views 
                                                             :else                                            nil)
                                         file-data     (try (when (.exists (io/file file)) (edn/read-string (slurp file)))
                                                            (catch Exception _ nil))]
                                  :when file-data]
                              (if subkey (do (swap! subkey merge {keyname file-data}) {}) {keyname file-data})))
        repack        (-> repack
                          (assoc :queries @q) ;; hardcoded repacks 
                          (assoc :views @v))
        _ (ut/pp [:repack2 repack])
        base-map      (get repack panel-key)]
    (-> (merge base-map (dissoc repack panel-key))
        (assoc :tab tab)
        (assoc :name original-name))))


(defn repack-block [client-name tab block-name panel-key]
  (let [client-dir    (str "./live/" (fixstr client-name) "/")
        block-dir     (str client-dir "/" tab "/" block-name "/")
        files         (filter #(not (or (cstr/ends-with? % ".swp")
                                        ;(cstr/starts-with? % ".")
                                        ))
                              (get-all-paths block-dir))
        root-file     (str "./live/" (fixstr client-name) ".edn")
        original-name (get-in (try (read-string (slurp root-file)) (catch Exception e {:invalid-edn! (str e)}))
                              [panel-key :name]
                              block-name)
        runners       (into {} (for [[k v] (get (cfg/settings) :runners {})] {k (get v :ext ".edn")}))
        extensions    (set (map :ext (vals runners)))
        remove-extension (fn [filename]
                           (let [ext (first (filter #(cstr/ends-with? filename %) extensions))]
                             (if ext
                               (subs filename 0 (- (count filename) (count ext)))
                               filename)))
        special-keys  (atom {})
        repack        (into {}
                            (for [file  files
                                  :let  [relative-path (cstr/replace file block-dir "")
                                         keyname       (keyword
                                                        (-> (str (last (cstr/split relative-path #"/")))
                                                            (cstr/replace  ".edn" "")
                                                            (cstr/replace  ".clj" "")
                                                            (cstr/replace  ".py" "")
                                                            (cstr/replace  ".txt" ""))
                                                        ;;(first (cstr/split (str (last (cstr/split relative-path #"/"))) #"."))
                                                        )
                                         ;filename      (last (cstr/split relative-path #"/"))
                                         ;keyname       (keyword (remove-extension filename))
                                         special-key   (first (keep (fn [[k v]]
                                                                      (when (cstr/includes? relative-path (str "/" (name k) "/"))
                                                                        k))
                                                                    runners))
                                         file-data     (try (when (.exists (io/file file)) (edn/read-string (slurp file)))
                                                            (catch Exception _ nil))]
                                  :when file-data]
                              (if special-key
                                (do
                                  (swap! special-keys update special-key (fnil conj {}) [keyname file-data])
                                  {})
                                {keyname file-data})))
        repack        (merge repack @special-keys)
       ;; _ (ut/pp [:repack3 repack])
        base-map      (get repack panel-key)]
    (-> (merge base-map (dissoc repack panel-key))
        (assoc :tab tab)
        (assoc :name original-name))))



(def file-work-agent (agent nil))

(defn process-runner-type [items block-dir runner-key runner-ext path-history]
        (when (ut/ne? items)
          (doseq [[kk vv] items
                  :let    [view-base          (str block-dir "/" (cstr/replace (str runner-key) ":" "") "/")
                           view-file-base     (str view-base (fixstr kk) runner-ext)
                           existing-file-data (try (when (.exists (io/file view-file-base)) (edn/read-string (slurp view-file-base)))
                                                   (catch Exception e (do (ut/pp [:cannot-read-existing view-file-base e]) {})))
                           base-exists?       (and (.exists (io/file view-file-base)) (= vv existing-file-data))]]
            (swap! path-history conj view-file-base)
            (when (not base-exists?)
              ;(do ;(ut/pp [:file-unpacker! k kk :views client-name
                (create-dirs view-base)
                (pretty-spit view-file-base vv)))));)

(defn write-panels [client-name panels]
  (ut/pp [:write-panels! client-name (keys panels)])  
  (pretty-spit (str "./live/" (fixstr client-name) ".edn") panels :client) ;; overwrite existing FULL deck image
  (let [name-mapping-raw (into {} (for [[k v] panels] {k (get v :name)}))
        name-mapping     (ut/generate-unique-names name-mapping-raw)
        ;rev-name-mapping (ut/reverse-map name-mapping)
        client-name      (fixstr client-name)
        panel-cnt        (count (keys panels))
        path-history     (atom [])
        runners          (into {} (for [[k v] (get (cfg/settings) :runners {})] {k (get v :ext ".edn")}))
        client-dir       (str "./live/" client-name "/")]
    (swap! client-pushes assoc client-name (System/currentTimeMillis))
    (create-dirs client-dir) ;; create base client folder root if not exists
    (doseq [[k v] panels ;; for each panel
            :when (map? v)
            :let  [;_ (ut/pp [:working-on k v])
                   board-name                (get v :tab)
                   block-dir                 (str client-dir "/" board-name "/" (get name-mapping k) "/")
                   block-file-base           (str block-dir (fixstr k) ".edn")
                  ;;  naked-incoming-block-data (-> v
                  ;;                                (dissoc :views)
                  ;;                                (dissoc :tab)
                  ;;                                (dissoc :name)
                  ;;                                (dissoc :queries))
                   naked-incoming-block-data (apply dissoc v (conj (keys runners) :tab :name))
                   ;naked-incoming-views      (get v :views {})
                   ;naked-incoming-queries    (get v :queries {})
                   incoming-runners          (select-keys v (keys runners))
                   existing-file-data        (try (when (.exists (io/file block-file-base)) (edn/read-string (slurp block-file-base)))
                                                  (catch Exception e (do (ut/pp [:cannot-read-existing block-file-base e]) {})))
                  ;;  existing-file-data-base   (-> existing-file-data
                  ;;                                (dissoc :views)
                  ;;                                (dissoc :tab)
                  ;;                                (dissoc :queries))
                   existing-file-data-base   (apply dissoc existing-file-data (conj (keys runners) :tab))
                   base-exists?              (and (.exists (io/file block-file-base))
                                                  (= naked-incoming-block-data existing-file-data-base))]]
      (swap! path-history conj block-file-base)
      (when (not base-exists?)
        (do ;(ut/pp [:file-unpacker! k :base client-name
          (create-dirs block-dir)
          (pretty-spit block-file-base naked-incoming-block-data :client)))

      (doseq [rr (keys incoming-runners)]
        (process-runner-type (get v rr) block-dir rr (get runners rr) path-history)))
    (let [extras      (vec (cset/difference (set (get-all-paths client-dir))
                                            (set (mapv #(cstr/replace % "//" "/") @path-history))))
          extra-files (filterv #(cstr/ends-with? % ".edn") extras)
          dirs        (filterv #(not (cstr/ends-with? % ".edn")) extras)]
      (cleanup extra-files dirs))))





(defn write-panels-old 
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
                   board-name                (get v :tab)
                   block-dir                 (str client-dir "/" board-name "/" (get name-mapping k) "/")
                   block-file-base           (str block-dir (fixstr k) ".edn")
                   naked-incoming-block-data (-> v
                                                 (dissoc :views)
                                                 (dissoc :tab)
                                                 (dissoc :name)
                                                 (dissoc :queries))
                   naked-incoming-views      (get v :views {})
                   naked-incoming-queries    (get v :queries {})
                   existing-file-data        (try (when (.exists (io/file block-file-base)) (read-string (slurp block-file-base)))
                                                  (catch Exception e (do (ut/pp [:cannot-read-existing block-file-base e]) {})))
                   existing-file-data-base   (-> existing-file-data
                                                 (dissoc :views)
                                                 (dissoc :tab)
                                                 (dissoc :queries))
                   base-exists?              (and (.exists (io/file block-file-base))
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
                         existing-file-data (try (when (.exists (io/file view-file-base)) (read-string (slurp view-file-base)))
                                                 (catch Exception e (do (ut/pp [:cannot-read-existing view-file-base e]) {})))
                         base-exists?       (and (.exists (io/file view-file-base)) (= vv existing-file-data))]]
          (swap! path-history conj view-file-base)
          (if (not base-exists?)
            (do ;(ut/pp [:file-unpacker! k kk :views client-name
              (create-dirs view-base)
              (pretty-spit view-file-base vv)))))
      (when (not (empty? naked-incoming-queries))
        (doseq [[kk vv] naked-incoming-queries
                :let    [query-base         (str block-dir "/queries/")
                         query-file-base    (str query-base (fixstr kk) ".edn")
                         existing-file-data (try (when (.exists (io/file query-file-base)) (read-string (slurp query-file-base)))
                                                 (catch Exception e (do (ut/pp [:cannot-read-existing query-file-base e]) {})))
                         base-exists?       (and (.exists (io/file query-file-base)) (= vv existing-file-data))]]
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


