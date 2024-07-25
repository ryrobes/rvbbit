(ns rvbbit-backend.evaluator
  (:require
   [clojure.edn           :as edn]
   [clojure.string        :as cstr]
   [clojure.pprint        :as ppt]
   [nrepl.core            :as nrepl]
   [nrepl.server          :as nrepl-server]
   [nrepl.transport       :as transport]
   [rvbbit-backend.config :as config]
   [clojure.walk :as walk]
   [rvbbit-backend.sql        :as    sql
    :refer [sql-exec sql-query sql-query-one system-db autocomplete-db ghost-db flows-db insert-error-row! to-sql
            pool-create]]
   [rvbbit-backend.ddl        :as sqlite-ddl]
   [rvbbit-backend.external :as ext]
   [rvbbit-backend.queue-party :as qp]
   [rvbbit-backend.util   :as ut])
  (:import [java.util.concurrent TimeoutException TimeUnit]))




(def ^:private initial-max-items 200)
(def ^:private max-wire-size (* 50 1024)) ; 50KB, as per your example
(def ^:private max-string-length 1000)

(defn- format-bytes
  "Format byte size to a human-readable string with appropriate unit."
  [bytes]
  (let [units ["bytes" "KB" "MB" "GB"]
        unit-index (int (Math/floor (/ (Math/log (max bytes 1)) (Math/log 1024))))
        value (/ bytes (Math/pow 1024 unit-index))
        formatted (if (zero? unit-index)
                    (format "%,d" (int value))
                    (format "%.2f" value))
        unit (nth units unit-index)]
    (str formatted " " unit)))

(defn- estimate-wire-size
  "Estimate the wire size of a data structure."
  [data]
  (cond
    (nil? data) 4  ; Assume nil takes 4 bytes
    (string? data) (count (.getBytes ^String data "UTF-8"))
    (number? data) (count (str data))
    (keyword? data) (+ 1 (count (name data)))
    (symbol? data) (count (str data))
    (map? data) (reduce + (map #(+ (estimate-wire-size (key %))
                                   (estimate-wire-size (val %)))
                               data))
    (coll? data) (reduce + (map estimate-wire-size data))
    :else 1))

(defn- truncate-string
  "Truncate a string if it's too long."
  [s]
  (if (and (string? s) (> (count s) max-string-length))
    (str (subs s 0 max-string-length) "...")
    s))

(defn- adaptive-sample-collection
  "Sample a collection, adaptively reducing the number of items until under the size limit."
  [coll current-size]
  (loop [limit (min initial-max-items (count coll))
         sampled-size (estimate-wire-size coll)]
    (if (<= (+ current-size sampled-size) max-wire-size)
      {:data (vec (take limit coll))
       :pruned (> (count coll) limit)
       :size sampled-size}
      (if (<= limit 1)
        {:data []
         :pruned true
         :size 0}
        (let [new-limit (max 1 (int (* limit 0.75)))]  ; Reduce by 25% each iteration, minimum 1
          (recur new-limit
                 (estimate-wire-size (take new-limit coll))))))))

(defn- sample-nested-structure
  "Recursively sample a nested structure, preserving structure while limiting size."
  [data current-size]
  (let [item-size (estimate-wire-size data)]
    (if (<= (+ current-size item-size) max-wire-size)
      {:data data
       :pruned false
       :size item-size}
      (cond
        (string? data)
        (let [truncated (truncate-string data)]
          {:data truncated
           :pruned (not= data truncated)
           :size (estimate-wire-size truncated)})

        (map? data)
        (let [{:keys [data pruned size]} (adaptive-sample-collection
                                          (map (fn [[k v]]
                                                 [k (sample-nested-structure v 0)])
                                               data)
                                          current-size)]
          {:data (into {} (map (fn [[k v]] [k (:data v)]) data))
           :pruned (or pruned (some :pruned (map second data)))
           :size size})

        (coll? data)
        (let [{:keys [data pruned size]} (adaptive-sample-collection
                                          (map #(sample-nested-structure % 0) data)
                                          current-size)]
          {:data (mapv :data data)
           :pruned (or pruned (some :pruned data))
           :size size})

        :else
        {:data data
         :pruned false
         :size item-size}))))

(defn- describe-structure
  "Generate a detailed description of the structure."
  [data]
  (letfn [(count-levels
            ([d] (count-levels d 0 []))
            ([d depth acc]
             (if (and (coll? d) (seq d))
               (let [current-count (count d)
                     next-level (if (map? d) (vals d) d)]
                 (recur
                  (first next-level)
                  (inc depth)
                  (update acc depth (fnil max 0) current-count)))
               acc)))]
    (let [levels (count-levels data)
          depth (count levels)
          total-items (reduce * levels)
          estimated-size (estimate-wire-size data)
          level-description (if (seq levels)
                              (cstr/join "x" levels)
                              "1")]
      {:depth depth
       :dimensions level-description
       :total-items total-items
       :estimated-size estimated-size})))

(defn safe-sample
  "Safely sample a data structure, preserving structure while limiting size."
  [data]
  (try
    (let [initial-description (describe-structure data)
          initial-wire-size (:estimated-size initial-description)
          {:keys [data pruned size] :as sample-result} (sample-nested-structure data 0)]
      (if (nil? sample-result)
        (throw (ex-info "Unexpected nil result from sample-nested-structure"
                        {:initial-wire-size initial-wire-size}))
        (let [sampling-occurred? (or pruned (not= initial-wire-size size))
              final-description (when sampling-occurred? (describe-structure data))]
          {:data data
           :message (if sampling-occurred?
                      (format "Data was sampled to reduce size. Original structure: depth %d, dimensions %s, %s items, size %s. Sampled structure: depth %d, dimensions %s, %s items, size %s."
                              (:depth initial-description)
                              (:dimensions initial-description)
                              (:total-items initial-description)
                              (format-bytes initial-wire-size)
                              (:depth final-description)
                              (:dimensions final-description)
                              (:total-items final-description)
                              (format-bytes size))
                      (format "Data was not sampled. Structure: depth %d, dimensions %s, %s items, size %s."
                              (:depth initial-description)
                              (:dimensions initial-description)
                              (:total-items initial-description)
                              (format-bytes initial-wire-size)))
           :sampling-details (when sampling-occurred?
                               {:original-structure initial-description
                                :sampled-structure final-description
                                :pruning-occurred pruned
                                :size-reduction-percentage (int (* 100 (/ (- initial-wire-size size) initial-wire-size)))})
           :sampling-error? false})))
    (catch Exception e
      {:error (.getMessage e)
       :data data
       :message "An error occurred during sampling. Original, unsampled data is returned."
       :sampling-error? true})))

(defn safe-sample-with-description
  "Safely sample data, preserving structure when possible, and provide a description."
  [data]
  (let [{:keys [data message sampling-details error sampling-error?]} (safe-sample data)]
    (cond-> {:data data
             :message message
             :sampling-error? (boolean sampling-error?)}
      (not sampling-error?) (assoc :sampling-details sampling-details)
      error (assoc :error error))))






;;(defn run [code] (eval code))

(defonce eval-cache (atom {}))
(def rabbit-config (config/settings)) ;; legacy
(def debug? true)

(def repl-server (atom nil))
(def ext-repl-port (get-in rabbit-config [:ext-nrepl :port] 32999))
(def ext-repl-host (get-in rabbit-config [:ext-nrepl :host] "127.0.0.1"))
(def repl-port (get rabbit-config :nrepl-port 8181))

(def nrepls-run (atom 0))
(def nrepls-intros-run (atom 0))

(defn strip-ansi-codes [s] (cstr/replace s #"\u001B\[[0-9;]*m" ""))

(def repl-introspection-atom (ut/thaw-atom {} "./data/atoms/repl-introspection-atom.edn"))
(defonce repl-introspection-child-atoms (atom {}))
;(def repl-client-namespaces-map (atom {}))


(defn insert-rowset
  [rowset table-name keypath client-name & [columns-vec db-conn queue-name]]
  ;; (ut/pp [:insert-into-cache-db!! (first rowset) (count rowset) table-name columns-vec])
  (if (ut/ne? rowset)
    (try (let [rowset-type     (cond (and (map? (first rowset)) (vector? rowset))       :rowset
                                     (and (not (map? (first rowset))) (vector? rowset)) :vectors)
               columns-vec-arg columns-vec
               ;db-conn         (or db-conn cache-db)
               rowset-fixed    (if (= rowset-type :vectors)
                                 (for [r rowset] (zipmap columns-vec-arg r))
                                 rowset)
               columns         (keys (first rowset-fixed))
               table-name-str  (ut/unkeyword table-name)
               ddl-str         (sqlite-ddl/create-attribute-sample table-name-str rowset-fixed)
               extra           {:queue queue-name
                                :extras [ddl-str columns-vec-arg table-name table-name-str]}]
           ;;(enqueue-task5d (fn [] (write-transit-data rowset-fixed keypath client-name table-name-str)))
           ;(swap! last-solvers-data-atom assoc keypath rowset-fixed) ;; full data can be clover
           ;(write-transit-data rowset-fixed keypath client-name table-name-str)
           (sql-exec db-conn (str "drop table if exists " table-name-str " ; ") extra)
           (sql-exec db-conn ddl-str extra)
           (doseq [batch (partition-all 10 rowset-fixed)
                   :let  [values     (vec (for [r batch] (vals r)))
                          insert-sql (to-sql {:insert-into [table-name] :columns columns :values values})]]
             (sql-exec db-conn insert-sql extra))
           (ut/pp [:INSERTED-SUCCESS! :introspected-rowset (count rowset) :into table-name-str  client-name])
           {:sql-cache-table table-name :rows (count rowset)})
         (catch Exception e (ut/pp [:INSERT-ERROR! (str e) table-name])))
    (ut/pp [:cowardly-wont-insert-empty-rowset table-name :puttem-up-puttem-up!])))


(defn logger [name edn]
  (let [dir         (str "./logs/" (str (java.time.LocalDate/now)) "/")
        _           (ext/create-dirs dir)
        fp          (str dir name ".log.edn")
        mst         (System/currentTimeMillis)
        data        [(ut/millis-to-date-string mst) edn]
        pretty-data (with-out-str (ppt/pprint data))]
    (spit fp (str pretty-data "\n") :append false)))

(defn introspect-namespace [conn namespace-name client-name]
  (try
    (let [_ (swap! nrepls-intros-run inc)
          introspection-code (str "
                           (do
                             (println \"Debug: Starting introspection for namespace: " namespace-name "\")
                             (let [ns-obj (find-ns '" namespace-name ")
                                   _      (println \"Debug: Namespace:\" (str ns-obj))
                                   vars   (if ns-obj (ns-interns ns-obj) {})
                                   _      (println \"Debug: Number of vars:\" (count vars))
                                   safe-pr-str (fn [v]
                                                 (try
                                                   (pr-str v)
                                                   (catch Exception e
                                                     (str \"#<Error printing: \" (.getMessage e) \">\"))))
                                   safe-deref (fn [v]
                                                (try
                                                  (cond
                                                    (instance? clojure.lang.Atom v) @v
                                                    (instance? clojure.lang.Ref v) @v
                                                    (instance? clojure.lang.Agent v) @v
                                                    (var? v) (var-get v)
                                                    :else v)
                                                  (catch Exception e
                                                    (str \"#<Error dereferencing: \" (.getMessage e) \">\"))))
                                   result (into {}
                                                (map (fn [[k v]]
                                                       (try
                                                         (let [raw-value (var-get v)
                                                               value (safe-deref raw-value)
                                                               type-kw (cond
                                                                         (fn? raw-value) :function
                                                                         (instance? clojure.lang.Atom raw-value) :atom
                                                                         (instance? clojure.lang.Ref raw-value) :ref
                                                                         (:once (meta v)) :defonce
                                                                         :else :def)
                                                               info (cond
                                                                      (= type-kw :function) {:arg-list (str (first (:arglists (meta v))))
                                                                                             :doc (:doc (meta v))}
                                                                      (= type-kw :atom) {:value (safe-pr-str value)}
                                                                      (= type-kw :ref) {:value (safe-pr-str value)}
                                                                      :else {:value (safe-pr-str value)})]
                                                           (println \"Debug: Processing var:\" k \"of type:\" type-kw)
                                                           [(str k) (merge {:name (str k)
                                                                            :type (str type-kw)}
                                                                           info)])
                                                         (catch Exception e
                                                           (println \"Debug: Error processing var:\" k \"-\" (.getMessage e))
                                                           [(str k) {:name (str k)
                                                                     :type :error
                                                                     :value (str \"Error: \" (.getMessage e))}])))
                                                     vars))]
                               (println \"Debug: Introspection complete. Result count:\" (count result))
                               result))
                           ")
          skt (nrepl/client conn 60000000)
          msg (nrepl/message skt {:op "eval" :code introspection-code})
          rsp-read      (vec ;(remove #(or (nil? %) (cstr/starts-with? (str %) "(var")) ;; no
                         (nrepl/response-values msg));)
          rsp           (nrepl/combine-responses msg)
          ;msg-out       (vec (remove nil? (for [m msg] (get m :out))))
          merged-values (first rsp-read)
          results (-> rsp
                      (assoc :values merged-values)
                    ;(assoc :out (vec (cstr/split (strip-ansi-codes (cstr/join msg-out)) #"\n")))
                    ;(dissoc :id :out :status :session)
                      (assoc :ns namespace-name))
          sampler  (fn [x] (cond
                               ;; Handle strings
                             (string? x) (if (> (count x) 30)
                                           (subs x 0 30)
                                           x)
                               ;; Handle vectors
                             (vector? x) (subvec x 0 (min (count x) 5))
                               ;; Handle lists
                             (list? x) (let [limited (take 5 x)]
                                         (apply list limited))
                               ;; Handle maps
                             (map? x) (into (empty x) (take 5 x))
                               ;; Handle sets
                             (set? x) (into (empty x) (take 5 x))
                               ;; Default case for other types
                             :else x))
          results0 (group-by :type (for [[_ v] (get results :values)] v))
          sqlized  (atom {})
          results0 (into {} (for [[k v] results0]
                              {k (into {} (for [vv v
                                                :let [value-data (try (edn/read-string (get vv :value)) (catch Exception _ (get vv :value)))
                                                      table-name (str
                                                                  (-> (str namespace-name) (cstr/replace  "." "_") (cstr/replace  "-" "_")) "_"
                                                                  (-> (str (get vv :name)) (cstr/replace  "." "_") (cstr/replace  "-" "_")) "_tbl"
                                                                  ;;(cstr/replace (str (get vv :name)) ":" "")
                                                                  )
                                                      _ (when (try
                                                                (and (vector? value-data) (map? (first value-data)))
                                                                (catch Exception _ false))
                                                          (swap! sqlized assoc table-name {:select [:*]
                                                                                           :connection-id client-name
                                                                                           :from [(keyword table-name)]})
                                                          (qp/serial-slot-queue :serial-data-literal-code-intro client-name
                                                                                (fn []
                                                                                  (insert-rowset value-data
                                                                                                 table-name
                                                                                                 nil
                                                                                                 nil
                                                                                                 (keys (first value-data))
                                                                                                 (sql/create-or-get-client-db-pool client-name)
                                                                                                 client-name))))]]
                                            {(get vv :name)
                                             (sampler value-data)}))}))
          results0 (assoc results0 :introspected (ut/millis-to-date-string (System/currentTimeMillis)))
          results0 (assoc results0 ":sqlized" @sqlized)]
      results0)
    (catch Throwable e {:introspection-error (str e)
                        :introspected (ut/millis-to-date-string (System/currentTimeMillis))})))

(defn update-namespace-state-async [host port client-name id code ns-str]
  ;(future
  (try
    (with-open [conn (nrepl/connect :host host :port port)]
      (let [_ (ut/pp [:running-namespace-introspection ns-str host port client-name id])
            introspection (introspect-namespace conn ns-str client-name)
              ;split-ns (vec (map keyword (cstr/split ns-str #".")))
              ;split-ns (vec (map keyword (cstr/split ns-str #"\.")))
            ]
        (ut/pp [:repl {:introspected-repl-session introspection}])
          ;;(swap! repl-introspection-atom assoc-in split-ns introspection) ;; host and port for later?
        (swap! repl-introspection-atom assoc-in [client-name ns-str] introspection) ;; host and port for later?
          ;; (logger (str (ut/keypath-munger [ns-str host port client-name id]) "-intro")
          ;;         {:orig-code code
          ;;          :introspection introspection})
        ))
    (catch Exception e
      (ut/pp [:repl "Error during async introspection:" (ex-message e)]))));)


(defn create-nrepl-server! []
  (ut/pp [:starting-local-nrepl :port repl-port])
  (reset! repl-server (nrepl-server/start-server :port repl-port :bind "127.0.0.1")))

(defn stop-nrepl-server! [timeout-ms]
  (when @repl-server
    (ut/pp [:stopping-local-nrepl])
    (let [shutdown-future (future
                            (doseq [session (:sessions @repl-server)]
                              (try
                                (transport/send (:transport session) {:op "interrupt"})
                                (catch Exception e
                                  (ut/pp [:failed-to-interrupt-session (str e)]))))
                            (nrepl-server/stop-server @repl-server)
                            (reset! repl-server nil)
                            (ut/pp [:performing-additional-cleanup])
                            ;; Add your custom cleanup logic here
                            )]
      (try
        @(deref shutdown-future timeout-ms :timeout)
        (catch TimeoutException _
          (ut/pp [:forceful-shutdown-required])
          (future-cancel shutdown-future))))))

(defn perform-gc []
  (ut/pp [:triggering-garbage-collection])
  (System/gc)
  (Thread/sleep 1000)  ; Give GC some time to work
  (System/runFinalization))

(defn graceful-restart-nrepl-server! []
  (ut/pp [:performing-graceful-restart])
  ;(stop-nrepl-server! 5000)  ; 5 second timeout for graceful shutdown
  (try
    (nrepl-server/stop-server @repl-server) ;; will throw due to closing socket ? TODO
    (catch Throwable _ nil))
  (reset! repl-server nil)
  ;(perform-gc)
  (create-nrepl-server!))

;; (graceful-restart-nrepl-server!)

(defn forceful-restart-nrepl-server! []
  (ut/pp [:performing-forceful-restart])
  (when @repl-server
    (try
      (nrepl-server/stop-server @repl-server)
      (catch Exception e
        (ut/pp [:error-stopping-server (str e)]))))
  (reset! repl-server nil)
  (perform-gc)
  (create-nrepl-server!))




(defn repl-eval
  [code repl-host repl-port client-name id]

  (let [_           (swap! nrepls-run inc)
        s           (str code) ;; ? why convert. TODO: remove
        eval-cache? false ;true ; (cstr/includes? s ";:::cache-me")
        cache-hash  (hash (cstr/trim (cstr/trim-newline (cstr/trim s))))]
    ;(ut/pp [:repl :started (str code) client-name id])
    (if false ;(and (not (nil? (get @eval-cache cache-hash))) eval-cache?)
      (do ;(println "cache-hit" cache-hash)
        (get @eval-cache cache-hash))
      (let [nrepl?           true ;(cstr/includes? s ":::use-nrepl")
            ext-nrepl?       (cstr/includes? s ":::ext-nrepl") ;; from rabbit.edn
            custom-nrepl?    (cstr/includes? s ":::custom-nrepl")
            custom-nrepl-map (cond ext-nrepl?                                          {:host ext-repl-host
                                                                                        :port ext-repl-port}
                                   (and (not (nil? repl-host)) (not (nil? repl-port))) {:host repl-host ;; override repl
                                                                                        :port repl-port}
                                   custom-nrepl?                                       (first ;; parse out conn deets, old data-rabbit fn
                                                                                        (remove nil?
                                                                                                (for [l (cstr/split s #"\n")]
                                                                                                  (when (cstr/includes?
                                                                                                         l
                                                                                                         ":::custom-nrepl")
                                                                                                    (edn/read-string
                                                                                                     (last (cstr/split
                                                                                                            l
                                                                                                            #":::custom-nrepl")))))))
                                   :else                                               {:host "127.0.0.1" :port 8181})
            nrepl-port       (get custom-nrepl-map :port)
            nrepl-host       (get custom-nrepl-map :host)]
        (if (not nrepl?) ;; never going to happen here TODO: remove? straight eval...
          (let [eval-output  (try (if (string? code) (load-string s) (eval code))
                                  (catch Exception e {:server-eval-error [] :error (ex-message e) :data (ex-data e)}))
                output-type  (type eval-output) ;;; old data-rabbit code, will revisit
                eval-output0 (cond (or (cstr/starts-with? (str output-type) "class tech.v3.dataset.impl")
                                       (cstr/starts-with? (str output-type) "class clojure.lang.Var"))
                                   [:pre (str eval-output)]
                                   :else eval-output)]
            (do (when debug?
                  (println ["   !**NOT REPL**!   " (newline) (str output-type) (newline) eval-output]))
                {:evald-result eval-output0}))
          (let
           [e (try
                (with-open [conn (nrepl/connect :host nrepl-host :port nrepl-port)]
                  (let [user-fn-str   (slurp "./user.clj")
                        ;gen-ns        (str "repl-" (-> (ut/keypath-munger [client-name "." id])
                        ;                               (cstr/replace  "_" "-")
                        ;                               (cstr/replace  "--" "-")))
                        gen-ns        (cstr/replace
                                       (str "repl-" (str client-name) "-"
                                            (if (vector? id)
                                              (-> (ut/keypath-munger id)
                                                  (cstr/replace  "_" "-")
                                                  (cstr/replace  "--" "-"))
                                              (-> (str id)
                                                  (cstr/replace  "_" "-")
                                                  (cstr/replace  "--" "-")))) ":" "")
                        user-fn-str   (if (not (cstr/includes? (str s) "(ns "))
                                        (cstr/replace user-fn-str "rvbbit-board.user" (str gen-ns)) user-fn-str)
                        s             (str user-fn-str "\n" s)
                        skt           (nrepl/client conn 60000000)
                        msg           (nrepl/message skt {:op "eval" :code s})
                        rsp-read      (vec (remove #(or (nil? %) (cstr/starts-with? (str %) "(var"))
                                                   (nrepl/response-values msg)))
                        rsp           (nrepl/combine-responses msg)
                        msg-out       (vec (remove nil? (for [m msg] (get m :out))))
                        merged-values rsp-read
                        sampled-values (try
                                         (safe-sample-with-description (first rsp-read))
                                        ;; ^^ revist when we have multi output repl calls - for now, just one output, one result (since front-end embeds DO block)
                                         (catch Exception e (do (ut/pp [:safe-sample-with-description-ERROR e]) {})))
                        output        {:evald-result
                                       (-> rsp
                                           (assoc-in [:meta :nrepl-conn] custom-nrepl-map)
                                           (assoc :value merged-values)
                                           ;(assoc :out (vec (cstr/split (strip-ansi-codes (cstr/join msg-out)) #"\n")))
                                           (assoc :out (vec (cstr/split (cstr/join msg-out) #"\n")))
                                           (dissoc :id)
                                           (dissoc :session))
                                       :sampled sampled-values}
                        ns-str (get-in output [:evald-result :ns] "user")]

                    (swap! repl-introspection-atom assoc-in [:repl-client-namespaces-map client-name]
                           (vec (distinct (conj (get-in @repl-introspection-atom [:repl-client-namespaces-map client-name] []) ns-str))))

                    ;;; :repl-ns/repl-client-namespaces-map>*client-name*

                    ;;(swap! repl-client-namespaces-map update-in [client-name] (fnil conj #{}) gen-ns)

                    ;; when only?
                    (when (and (cstr/includes? (str code) ":introspect!")
                               (not= client-name :rvbbit)
                               (not (nil? client-name))
                               (not (cstr/includes? (str ns-str) "rvbbit-backend.")))
                      (qp/serial-slot-queue :nrepl-introspection client-name
                                            (fn [] (update-namespace-state-async nrepl-host nrepl-port client-name id code ns-str))))




                    output))

                (catch Exception ee
                  (let
                   [error-msg (ex-message ee)
                    added-errors
                    (if (cstr/includes? error-msg "Could not read response value")
                      (str
                       "Looks like a Tag Reader issue: Try printing it w println or wrapping it in a str. 
                                            (Rabbit cares about 'values' first, not printing, so you may have to be 
                                                 explicit about what you want, output-wise)"
                       "\n")
                      nil)]
                    {:evald-result (merge {:nrepl-conn custom-nrepl-map
                                           :cause      (str (ex-cause ee))
                                           :err-data   (str (ex-data ee))
                                           :error      (ex-message ee)}
                                          (when (not (nil? added-errors))
                                            {:rabbit-added added-errors}))})))
            ns-str (get-in e [:evald-result :ns] "user")]
            (do ;(println "cache miss" cache-hash (keys @eval-cache))
              ;(swap! eval-cache assoc cache-hash e)
              (logger (str (ut/keypath-munger [ns-str nrepl-host nrepl-port client-name id]) "-eval") e)
              ;;(ut/pp [:repl (str code) client-name id :result e])
              e) ;; nrepl://127.0.0.1:44865
            ))))))


