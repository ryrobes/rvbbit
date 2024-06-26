(ns rvbbit-backend.evaluator
  (:require
   [clojure.edn           :as edn]
   [clojure.string        :as cstr]
   [clojure.pprint        :as ppt]
   [nrepl.core            :as nrepl]
   [nrepl.server          :as nrepl-server]
   [rvbbit-backend.config :as config]
   [rvbbit-backend.external :as ext]
   [rvbbit-backend.util   :as ut]))

(defn run [code] (eval code))

(defonce eval-cache (atom {}))
(def rabbit-config (config/settings)) ;; legacy
(def debug? true)

(def repl-server (atom nil))
(def ext-repl-port (get-in rabbit-config [:ext-nrepl :port] 32999))
(def ext-repl-host (get-in rabbit-config [:ext-nrepl :host] "127.0.0.1"))
(def repl-port (get rabbit-config :nrepl-port 8181))

(defn strip-ansi-codes [s] (cstr/replace s #"\u001B\[[0-9;]*m" ""))

(def repl-introspection-atom (ut/thaw-atom {} "./data/atoms/repl-introspection-atom.edn"))

(defn logger [name edn]
  (let [dir         (str "./logs/" (str (java.time.LocalDate/now)) "/")
        _           (ext/create-dirs dir)
        fp          (str dir name ".log.edn")
        mst         (System/currentTimeMillis)
        data        [(ut/millis-to-date-string mst) edn]
        pretty-data (with-out-str (ppt/pprint data))]
    (spit fp (str pretty-data "\n") :append false)))

(defn introspect-namespace [conn namespace-name]
  (try
    (let [introspection-code (str "
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
          skt (nrepl/client conn 60000)
          msg (nrepl/message skt {:op "eval" :code introspection-code})
          rsp-read      (vec ;(remove #(or (nil? %) (cstr/starts-with? (str %) "(var")) ;; no
                         (nrepl/response-values msg));)
          rsp           (nrepl/combine-responses msg)
          msg-out       (vec (remove nil? (for [m msg] (get m :out))))
          merged-values (first rsp-read)
          results (-> rsp
                      (assoc :values merged-values)
                    ;(assoc :out (vec (cstr/split (strip-ansi-codes (cstr/join msg-out)) #"\n")))
                    ;(dissoc :id :out :status :session)
                      (assoc :ns namespace-name))
          results0 (group-by :type (for [[_ v] (get results :values)] v))
          results0 (into {} (for [[k v] results0]
                              {k (into {} (for [vv v]
                                            {(get vv :name)
                                             (try (edn/read-string (get vv :value)) (catch Exception _ (get vv :value)))}))}))
          results0 (assoc results0 :introspected (ut/millis-to-date-string (System/currentTimeMillis)))]
      results0)
    (catch Throwable e {:introspection-error (str e)
                        :introspected (ut/millis-to-date-string (System/currentTimeMillis))})))


(defn update-namespace-state-async [host port client-name id code ns-str]
  (future
    (try
      (with-open [conn (nrepl/connect :host host :port port)]
        (let [introspection (introspect-namespace conn ns-str)
              ;split-ns (vec (map keyword (cstr/split ns-str #".")))
              split-ns (vec (map keyword (cstr/split ns-str #"\.")))
              ]
          ;(ut/pp [:repl {:introspected-repl-session introspection}])
          (swap! repl-introspection-atom assoc-in split-ns introspection) ;; host and port for later?
          (logger (str (ut/keypath-munger [ns-str host port client-name id]) "-intro")
                  {:orig-code code
                   :introspection introspection})))
      (catch Exception e
        (ut/pp [:repl "Error during async introspection:" (ex-message e)])))))

(defn create-nrepl-server!
  []
  (ut/pp [:starting-local-nrepl :port repl-port])
  (reset! repl-server (nrepl-server/start-server :port repl-port :bind "127.0.0.1")))





(defn repl-eval
  [code repl-host repl-port client-name id]
  (let [s           (str code) ;; ? why convert. TODO: remove
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
                                   custom-nrepl?                                       (first
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
                output-type  (type eval-output)
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
                        gen-ns        (str "repl-" (-> (ut/keypath-munger [client-name "." id]) 
                                                       (cstr/replace  "_" "-")
                                                       (cstr/replace  "--" "-")))
                        user-fn-str (if (not (cstr/includes? (str s) "(ns ")) 
                                      (cstr/replace user-fn-str "rvbbit-board.user" (str gen-ns)) user-fn-str)
                        s             (str user-fn-str "\n" s)
                        skt           (nrepl/client conn 60000)
                        msg           (nrepl/message skt {:op "eval" :code s})
                        rsp-read      (vec (remove #(or (nil? %) (cstr/starts-with? (str %) "(var")) ;; no
                                                   (nrepl/response-values msg)))
                        rsp           (nrepl/combine-responses msg)
                        msg-out       (vec (remove nil? (for [m msg] (get m :out))))
                        merged-values rsp-read
                        output        {:evald-result
                                       (-> rsp
                                           (assoc-in [:meta :nrepl-conn] custom-nrepl-map)
                                           (assoc :value merged-values) ;; ?
                                           (assoc :out (vec (cstr/split (strip-ansi-codes (cstr/join msg-out)) #"\n")))
                                           (dissoc :id)
                                           (dissoc :session))}
                        ns-str (get-in output [:evald-result :ns] "user")]

                    (update-namespace-state-async nrepl-host nrepl-port client-name id code ns-str)




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