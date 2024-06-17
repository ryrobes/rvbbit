(ns rvbbit-backend.evaluator
  (:require
    [clojure.edn           :as edn]
    [clojure.string        :as cstr]
    [nrepl.core            :as nrepl]
    [nrepl.server          :as nrepl-server]
    [rvbbit-backend.config :as config]
    [rvbbit-backend.util   :as ut]))

(defn run [code] (eval code))

(defonce eval-cache (atom {}))
(def rabbit-config (config/settings)) ;; legacy
(def debug? true)

(def repl-server (atom nil))
(def ext-repl-port (get-in rabbit-config [:ext-nrepl :port] 32999))
(def ext-repl-host (get-in rabbit-config [:ext-nrepl :host] "127.0.0.1"))
(def repl-port (get rabbit-config :nrepl-port 8181))

(defn create-nrepl-server!
  []
  (ut/pp [:starting-local-nrepl :port repl-port])
  (reset! repl-server (nrepl-server/start-server :port repl-port :bind "127.0.0.1")))

(defn strip-ansi-codes [s] (cstr/replace s #"\u001B\[[0-9;]*m" ""))

(defn repl-eval
  [code repl-host repl-port]
  (let [s           (str code) ;; ? why convert. TODO: remove
        eval-cache? false ;true ; (cstr/includes? s ";:::cache-me")
        cache-hash  (hash (cstr/trim (cstr/trim-newline (cstr/trim s))))]
    (if false ;(and (not (nil? (get @eval-cache cache-hash))) eval-cache?)
      (do ;(println "cache-hit" cache-hash)
        (get @eval-cache cache-hash))
      (let [nrepl?           true ;(cstr/includes? s ":::use-nrepl")
            ext-nrepl?       (cstr/includes? s ":::ext-nrepl") ;; from rabbit.edn
            custom-nrepl?    (cstr/includes? s ":::custom-nrepl")
            custom-nrepl-map (cond ext-nrepl?                                          {:host ext-repl-host ;; hardcoded
                                                                                                            ;; instead of
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
        (if (not nrepl?) ;; never going to happen here TODO: remove if
          (let [eval-output  (try (if (string? code) (load-string s) (eval code))
                                  (catch Exception e {:server-eval-error [] :error (ex-message e) :data (ex-data e)}))
                output-type  (type eval-output)
                eval-output0 (cond (or (cstr/starts-with? (str output-type) "class tech.v3.dataset.impl")
                                       (cstr/starts-with? (str output-type) "class clojure.lang.Var"))
                                     [:pre (str eval-output)]
                                   :else eval-output)]
            (do (when debug? (println ["   !**NOT REPL**!   " (newline) (str output-type) (newline) eval-output]))
                {:evald-result eval-output0}))
          (let
            [e (try
                 (with-open [conn (nrepl/connect :host nrepl-host :port nrepl-port)]
                   (let [user-fn-str   (slurp "./user.clj")
                         s             (str user-fn-str "\n" s)
                         skt           (nrepl/client conn 60000)
                         msg           (nrepl/message skt {:op "eval" :code s})
                         rsp-read      (vec (remove #(or (nil? %) (cstr/starts-with? (str %) "(var")) ;; no
                                              (nrepl/response-values msg)))
                         rsp           (nrepl/combine-responses msg)
                         msg-out       (vec (remove nil? (for [m msg] (get m :out))))
                         merged-values rsp-read]
                     {:evald-result (-> rsp
                                        (assoc-in [:meta :nrepl-conn] custom-nrepl-map)
                                        (assoc :value merged-values) ;; ?
                                        (assoc :out (vec (cstr/split (strip-ansi-codes (cstr/join msg-out)) #"\n")))
                                        (dissoc :id)
                                        (dissoc :session))}))
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
                                           (if (not (nil? added-errors)) {:rabbit-added added-errors}))})))]
            (do ;(println "cache miss" cache-hash (keys @eval-cache))
              ;(swap! eval-cache assoc cache-hash e)
              e) ;; nrepl://127.0.0.1:44865
          ))))))