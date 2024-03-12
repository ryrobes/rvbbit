(ns rvbbit-backend.external
  (:require [clojure.java.io :as io]
            [clojure.string :as cstr]
            [rvbbit-backend.util :as ut]
            [clojure.core.async :as async]
            [clojure.data :as data]
            [clojure.set :as cset]
            [websocket-layer.core :as wl]
            ;[zprint.core :as zp]
            )
 ; (:import [java.time LocalDateTime]
 ;          [java.time.format DateTimeFormatter])
  )

(defn pretty-spit
  [file-name collection]
  (spit (java.io.File. file-name)
        (with-out-str (clojure.pprint/write collection :dispatch clojure.pprint/code-dispatch))))

;; (defn format [s]
;;   (zp/zprint-str s nil ;(js/Math.floor (/ w 9))
;;                {:parse-string-all? true
;;                 :style :justified
;;                   ;:fn-map {:double :just-float}
;;                 :vector {:respect-nl? true}
;;                 :map {:comma? false :sort? false}
;;                 :parse {:interpose "\n\n"}}))

(def client-pushes (atom {}))

(defn fixstr [s]
  (cstr/replace (str s) #":" ""))

(defn get-all-paths [root-path]
  (let [root-file (io/file root-path)
        root-path (-> root-file .getCanonicalFile .getPath)]
    (->> root-file
         file-seq
         ;(map #(.getAbsolutePath %))
         (map #(cstr/replace % root-path ""))
         vec)))

;; (defn get-subdirs [dir]
;;   (let [dir-file (io/file dir)]
;;     (->> (file-seq dir-file)
;;          (filter #(.isDirectory %))
;;          (map #(.getName %))
;;          vec)))

(defn get-subdirs [dir]
  (let [dir-file (io/file dir)
        files (or (.listFiles dir-file) [])]
    (->> files
         (filter #(.isDirectory %))
         (map #(.getName %))
         vec)))

(defn delete-if-empty [dir]
  (let [dir-file (io/file dir)]
    (when (and (.isDirectory dir-file) (empty? (.list dir-file)))
      (try (do (ut/pp [:**cleanup! :deleting-empty dir])
               (io/delete-file dir-file))
           (catch Exception e
             (ut/pp [:error-deleting-empty dir e]))))))

(defn cleanup [extra-files dirs]
  (doseq [file extra-files]
    (try (do (ut/pp [:**cleanup! :deleting file])
             (io/delete-file file))
         (catch Exception e
           (ut/pp [:error-deleting file :err e]))))
  (doseq [dir dirs]
    (delete-if-empty dir)))

(defn create-dirs [path]
  (when (not (.exists (io/file path))) (.mkdirs (io/file path))))

;; (defn read-dir [dir]
;;   (let [files (get-all-paths dir) ;(file-seq (io/file dir))
;;         ]
;;     (println files)
;;     (into {} (for [file files
;;                    :let [relative-path (cstr/replace (.getAbsolutePath file) dir "")
;;                          _ (println relative-path)
;;                          file-data (when (.isFile file)
;;                                      (try
;;                                        (read-string (slurp (.getAbsolutePath file)))
;;                                        (catch Exception e
;;                                          (do (ut/pp [:cannot-read-existing (.getAbsolutePath file) e]) {}))))]
;;                    :when file-data]
;;                [relative-path file-data]))))

(defn read-dir [dir]
  (let [files (get-all-paths dir)
        repacked (into {} (for [file files
                                :let [relative-path (cstr/replace file dir "")
                                      keyname (keyword (cstr/replace (str (last (cstr/split relative-path #"/"))) ".edn" ""))
                                      ;_ (println relative-path)
                                      file-data (try
                                                  (when (.exists (io/file file))
                                                    (read-string (slurp file)))
                                                  (catch Exception e
                                                    (do (ut/pp [:cannot-read-existing file e]) {})))]
                                :when file-data]
                            [keyname file-data]))]
    repacked))

(defn repack-block [client-name tab block-name panel-key]
  (let [client-dir (str "./live/" (fixstr client-name) "/")
        block-dir (str client-dir "/" tab "/" block-name "/")
        files (get-all-paths block-dir)
        root-file (str "./live/" (fixstr client-name) ".edn")
        original-name (get-in (try (read-string (slurp root-file))
                                   (catch Exception e {:invalid-edn! (str e)})) [panel-key :name] block-name)
        q (atom {})
        v (atom {})
        repack (into {} (for [file files
                              :let [relative-path (cstr/replace file block-dir "")
                                    keyname (keyword (cstr/replace (str (last (cstr/split relative-path #"/"))) ".edn" ""))
                         ;_ (println relative-path)
                                    subkey (cond (cstr/includes? (str relative-path) "/queries/") q
                                                 (cstr/includes? (str relative-path) "/views/") v
                                                 :else nil)
                                    file-data (try
                                                (when (.exists (io/file file))
                                                  (read-string (slurp file)))
                                                (catch Exception _
                                       ;(do (ut/pp [:cannot-read-existing file e]) {})
                                                  nil))]
                              :when file-data]
                          (if subkey
                            ;{subkey {keyname file-data}}
                            (do (swap! subkey merge {keyname file-data})
                                {})
                            {keyname file-data})))
        repack (-> repack (assoc :queries @q) (assoc :views @v))
        base-map (get repack panel-key)]
    (-> (merge base-map (dissoc repack panel-key))
        (assoc :tab tab) (assoc :name original-name))))


;; (defn repack-block [client-name tab block-name]
;;   (let [client-dir (str "./live/" client-name "/")
;;         source (read-string (slurp (str "./live/" (fixstr client-name) ".edn")))
;;         files (get-all-paths client-dir)]

;;     )
;;   )


(def file-work-agent (agent nil))

(defn write-panels [client-name panels]
  (pretty-spit (str "./live/" (fixstr client-name) ".edn") panels) ;; overwrite existing "client source" file
  (let [name-mapping-raw (into {} (for [[k v] panels] {k (get v :name)}))
        ;current-names (vec (vals name-mapping-raw))
        ;name-mapping (into {} (for [[k v] name-mapping-raw] {(ut/unique-block-id v (remove #(= % v) current-names) []) k}))
        name-mapping (ut/generate-unique-names name-mapping-raw)
        rev-name-mapping (ut/reverse-map name-mapping)
        client-name (fixstr client-name)
        panel-cnt (count (keys panels))
        path-history (atom [])
        client-dir (str "./live/" client-name "/")]

    ;(ut/pp [:name-mapping rev-name-mapping])

    (swap! client-pushes assoc client-name (System/currentTimeMillis))

    (create-dirs client-dir) ;; create base client folder root if not exists

      ;; create block folders 

        ;(do (println (str "creating " client-dir))
        ;    (.mkdirs (io/file client-dir))))

    ;; add stuff first, remove orphans after 
    (doseq [[k v] panels  ;; for each panel 
            ;:when (let [vv (ut/remove-empty-sub-keys v)
            ;            repack (ut/remove-empty-sub-keys (repack-block client-name (get v :tab) (get v :name) k))]
            ;        (and (map? v) (not (= vv repack))))
            :when (map? v)
            :let [;_ (ut/pp [:working-on k v])
                  ;vv (ut/remove-empty-sub-keys v)
                  ;repack (ut/remove-empty-sub-keys (repack-block client-name (get v :tab) (get v :name) k))
                  board-name (get v :tab)
                  block-dir (str client-dir "/" board-name "/" (get name-mapping k) "/")
                  block-file-base (str block-dir (fixstr k) ".edn")
                  naked-incoming-block-data (-> v (dissoc :views) (dissoc :tab) (dissoc :name) (dissoc :queries))
                  naked-incoming-views (get v :views {})
                  naked-incoming-queries (get v :queries {})
                  existing-file-data (try (when (.exists (io/file block-file-base))
                                            (read-string (slurp block-file-base)))
                                          (catch Exception e (do (ut/pp [:cannot-read-existing block-file-base e]) {})))
                  existing-file-data-base (-> existing-file-data (dissoc :views) (dissoc :tab) (dissoc :queries))
                  base-exists? (and (.exists (io/file block-file-base))
                                    (= naked-incoming-block-data existing-file-data-base))]]
      (swap! path-history conj block-file-base)
      (if (not base-exists?)
        (do ;(ut/pp [:file-unpacker! k :base client-name 
            ;        ;block-file-base
            ;        :out-of-date :writing! :diff (data/diff naked-incoming-block-data existing-file-data)])
          (create-dirs block-dir)
          (pretty-spit block-file-base naked-incoming-block-data))
        ;(ut/pp [:file-unpacker k :base client-name 
        ;        ;block-file-base
        ;        :up-to-date :skipping])
        )

      ;; ok lets do views subfolder
      (when (not (empty? naked-incoming-views))
        (doseq [[kk vv] naked-incoming-views
                :let [view-base (str block-dir "/views/")
                      view-file-base (str view-base (fixstr kk) ".edn")
                      existing-file-data (try (when (.exists (io/file view-file-base))
                                                (read-string (slurp view-file-base)))
                                              (catch Exception e (do (ut/pp [:cannot-read-existing view-file-base e]) {})))
                      base-exists? (and (.exists (io/file view-file-base))
                                        (= vv existing-file-data))]]
          (swap! path-history conj view-file-base)
          (if (not base-exists?)
            (do ;(ut/pp [:file-unpacker! k kk :views client-name 
                ;        ;view-file-base 
                ;        :out-of-date :writing! :diff (data/diff vv existing-file-data)])
              (create-dirs view-base)
              (pretty-spit view-file-base vv))
            ;(ut/pp [:file-unpacker k kk :views client-name view-file-base :up-to-date :skipping])
            )))

      ;; query time
      (when (not (empty? naked-incoming-queries))
        (doseq [[kk vv] naked-incoming-queries
                :let [query-base (str block-dir "/queries/")
                      query-file-base (str query-base (fixstr kk) ".edn")
                      existing-file-data (try (when (.exists (io/file query-file-base))
                                                (read-string (slurp query-file-base)))
                                              (catch Exception e (do (ut/pp [:cannot-read-existing query-file-base e]) {})))
                      base-exists? (and (.exists (io/file query-file-base))
                                        (= vv existing-file-data))]]
          (swap! path-history conj query-file-base)
          (if (not base-exists?)
            (do ;(ut/pp [:file-unpacker! k kk :queries client-name 
                ;        ;query-file-base
                ;        :out-of-date :writing! :diff (data/diff vv existing-file-data)])
              (create-dirs query-base)
              (pretty-spit query-file-base vv))
            ;(ut/pp [:file-unpacker k kk :queries client-name 
            ;        ;query-file-base
            ;        :up-to-date :skipping])
            ))))

      ;(println (str "got it " (count (keys panels))))

    ;; removal phase
    (let [extras (vec (cset/difference (set (get-all-paths client-dir)) (set (mapv #(cstr/replace % "//" "/") @path-history))))
          extra-files (filterv #(cstr/ends-with? % ".edn") extras)
          dirs (filterv #(not (cstr/ends-with? % ".edn")) extras)]
      (cleanup extra-files dirs))

    ;(ut/pp [:processed panel-cnt :panels :for client-name])
    ))


;; (def client-queues (atom {}))

;; (defmethod wl/handle-subscription :server-push2 [{:keys [kind ui-keypath client-name]}]
;;   (let [results (async/chan 100)]
;;     (async/go-loop []
;;       (async/<! (async/timeout 300))
;;       (if-let [queue-atom (get @client-queues client-name)]
;;         (let [item (ut/dequeue! queue-atom)]
;;           (if item
;;             (when (async/>! results item)
;;               (recur))
;;             (recur)))
;;         (recur)))
;;     results))

;; (defn push-to-client [ui-keypath data client-name & [reco-count elapsed-ms]]
;;   (let [client-queue-atom (get @client-queues client-name)]
;;     (if client-queue-atom
;;       (swap! client-queue-atom conj
;;              {:ui-keypath ui-keypath
;;               :elapsed-ms elapsed-ms
;;               :data [data (get (first reco-count) :cnt)]
;;               :client-name client-name})
;;       (let [new-queue-atom (atom (clojure.lang.PersistentQueue/EMPTY))]
;;         (swap! client-queues assoc client-name new-queue-atom)
;;         (push-to-client ui-keypath data client-name reco-count elapsed-ms)))))
