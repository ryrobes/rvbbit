(ns rvbbit-backend.freezepop
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]
            [clojure.string :as cstr]
            [clojure.pprint :as pprint]
            [rvbbit-backend.util :as ut]
            [rvbbit-backend.pool-party :as ppy]
            [flowmaps.db :as flow-db]
            [cognitect.transit :as transit])
  (:import [java.io ByteArrayInputStream ByteArrayOutputStream]
           [java.nio.file Paths]))


(defn write-transit-data
  [data file]
  (with-open [out (io/output-stream file)] (transit/write (transit/writer out :msgpack) data)))

(defn read-transit-data
  [file-path]
  (let [abs-filepath (.toString (.toAbsolutePath (Paths/get file-path (into-array String []))))]
    (when (.exists (io/file abs-filepath))
      (with-open [in (io/input-stream abs-filepath)]
        (transit/read (transit/reader in :msgpack))))))

(def managed-atoms (atom {}))

;; (defn thaw-atom
;;   "Thaws an atom from disk or creates a new one if the file doesn't exist."
;;   [initial-state file-path & [out?]]
;;   (let [file  (io/file file-path)
;;         state (if (.exists file)
;;                 (with-open [rdr (io/reader file)]
;;                   (try (edn/read (java.io.PushbackReader. rdr))
;;                        (catch Exception e (do (ut/pp [:thaw-atom-error!!!! file e]) (System/exit 0)))))
;;                 initial-state)
;;         a     (atom state)]
;;     (when (not out?) ;; we dont want to manage some of these, do it manually
;;       (swap! managed-atoms assoc file-path a))
;;     a))

;; (defn freeze-atoms
;;   "Freezes all managed atoms to disk."
;;   []
;;   (doall
;;    (pmap (fn [[file-path a]]
;;            (let [wtr (io/writer file-path)]
;;              (try (binding [*out* wtr] ;; selective pretty print formatting
;;                     (if (or (cstr/includes? (str file-path) "signals.")
;;                             (cstr/includes? (str file-path) "rules.")
;;                             (cstr/includes? (str file-path) "errors.")
;;                             (cstr/includes? (str file-path) "autocomplete")
;;                             (cstr/includes? (str file-path) "training-atom.")
;;                             (cstr/includes? (str file-path) "sql-cache.")
;;                             (cstr/includes? (str file-path) "solvers."))
;;                       (clojure.pprint/pprint @a)
;;                       (prn @a)))
;;                   (finally (.close wtr))))
;;            (let [size-in-bytes      (java.nio.file.Files/size (java.nio.file.Paths/get file-path (into-array String [])))
;;                  size-in-mb         (/ size-in-bytes 1048576.0)
;;                  size-in-mb-rounded (/ (Math/round (* size-in-mb 100.0)) 100.0)]
;;              (ut/pp ["  " :freezing-atom file-path size-in-mb-rounded :mb])))
;;          @managed-atoms)))

(defn thaw-atom
  "Thaws an atom from disk or creates a new one if the file doesn't exist."
  [initial-state file-path & [out?]]
  (let [file  (io/file file-path)
        state (if (.exists file)
                (if (cstr/ends-with? file-path ".msgpack.transit")
                  (read-transit-data file-path)
                  (with-open [rdr (io/reader file)]
                    (try (edn/read (java.io.PushbackReader. rdr))
                         (catch Exception e (do (ut/pp [:thaw-atom-error!!!! file e]) (System/exit 0))))))
                initial-state)
        a     (atom state)]
    (when (not out?) ;; we dont want to manage some of these, do it manually
      (swap! managed-atoms assoc file-path a))
    a))

(defn freeze-atoms
  "Freezes all managed atoms to disk."
  []
  (doall
   (pmap (fn [[file-path a]]
           (if (cstr/ends-with? file-path ".msgpack.transit")
             (write-transit-data @a file-path)
             (let [wtr (io/writer file-path)]
               (try (binding [*out* wtr] ;; selective pretty print formatting
                      (if (or (cstr/includes? (str file-path) "signals.")
                              (cstr/includes? (str file-path) "rules.")
                              (cstr/includes? (str file-path) "errors.")
                              (cstr/includes? (str file-path) "autocomplete")
                              (cstr/includes? (str file-path) "training-atom.")
                              (cstr/includes? (str file-path) "sql-cache.")
                              (cstr/includes? (str file-path) "solvers."))
                        (clojure.pprint/pprint @a)
                        (prn @a)))
                    (finally (.close wtr)))))
           (let [size-in-bytes      (java.nio.file.Files/size (java.nio.file.Paths/get file-path (into-array String [])))
                 size-in-mb         (/ size-in-bytes 1048576.0)
                 size-in-mb-rounded (/ (Math/round (* size-in-mb 100.0)) 100.0)]
             (ut/pp ["  " :freezing-atom file-path size-in-mb-rounded :mb])))
         (dissoc @managed-atoms "./defs/solvers.edn" "./defs/signals.edn") ;; dont freeze these, since we manage them manually
         )))

(defn freeze-atom
  "Freezes a single atom to disk."
  [file-path]
  (ppy/execute-in-thread-pools
   :freeze-atom-serial
   (fn [] (let [a (get @managed-atoms file-path)]
            (when a (with-open [wtr (io/writer file-path)] (binding [*out* wtr] (prn @a))))))))

;; (freeze-atom "./defs/solvers.edn")
;; (ut/pp  (keys @managed-atoms))






;; (defn write-transit [data]
;;   (let [baos (ByteArrayOutputStream.)
;;         writer (transit/writer baos :msgpack)]
;;     (transit/write writer data)
;;     (.toByteArray baos)))

;; (defn read-transit [in]
;;   (let [reader (transit/reader in :msgpack)]
;;     (transit/read reader)))

(defn thaw-flow-results []
  (ut/pp [:thawing-flow-results-atom...])
  (let [file-path "./data/atoms/flow-db-results-atom.msgpack.transit"
        state (read-transit-data file-path)]
    (reset! flow-db/results-atom state)))

(defn freeze-flow-results []
  (let [file-path "./data/atoms/flow-db-results-atom.msgpack.transit"]
    (write-transit-data @flow-db/results-atom file-path)))


;; (defn thaw-flow-results []
;;   (ut/pp [:thawing-flow-results-atom...])
;;   (let [file-path "./data/atoms/flow-db-results-atom.edn"
;;         file      (io/file file-path)
;;         state     (if (.exists file)
;;                     (with-open [rdr (io/reader file)]
;;                       (try (edn/read (java.io.PushbackReader. rdr))
;;                            (catch Exception e (do (ut/pp [:flow-results-thaw-atom-error!!!! file e]) (System/exit 0)))))
;;                     {})]
;;     (reset! flow-db/results-atom state)))

;; (defn freeze-flow-results []
;;   (with-open [wtr (io/writer "./data/atoms/flow-db-results-atom.edn")]
;;     (binding [*out* wtr] (prn @flow-db/results-atom)))) 