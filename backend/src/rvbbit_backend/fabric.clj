(ns rvbbit-backend.fabric
  (:require [clojure.string          :as cstr]
            [clojure.edn             :as edn]
            [rvbbit-backend.util     :as ut]
            [rvbbit-backend.external :as ext]
            [clojure.java.shell      :as shell]))


(defn run-shell-command
  "execute a generic shell command and return output as a map of timing and seq of str lines (forked fabric version)"
  [command]
  (let [;;shell                    (or (System/getenv "SHELL") "/bin/sh")
        ;;output                   (shell/sh shell "-c" (str "mkdir -p shell-root ; cd shell-root ; " command))
        output                   (shell/sh "/bin/bash" "-c" (str "mkdir -p shell-root ; cd shell-root ; " command))
        split-lines              (vec (remove empty? (cstr/split-lines (get output :out))))
        exit-code                (get output :exit)
        error                    (vec (remove empty? (cstr/split-lines (get output :err))))
        has-timing?              (if (ut/ne? error) (cstr/starts-with? (get error 0) "real") false)
        error-data               (if has-timing? [] error)
        timing-values-to-seconds #(let [split-timing (cstr/split (get (cstr/split % #"\t") 1) #"m")
                                        minutes      (edn/read-string (get split-timing 0))
                                        seconds      (edn/read-string (cstr/join "" (drop-last (get split-timing 1))))]
                                    (+ (* 60 minutes) seconds))
        timing-data              (if has-timing? (into [] (for [x error] (timing-values-to-seconds x))) [])]
    {:output     split-lines
     :exception  error-data
     :seconds    timing-data
     :exit-code  exit-code
     :command    (str command)}))

(defn read-local-file
  "read local file and return its content as a string (forked fabric version)"
  [full-path]
  (let [fqd?      (or (cstr/starts-with? full-path "/") (cstr/starts-with? full-path "~"))
        output    (run-shell-command "pwd")
        pwd       (first (get-in output [:output :output] []))
        full-path (if fqd? full-path (str pwd "/" full-path))]
    (ut/pp [:reading-file full-path])
    (try {:file-data (str (slurp full-path)) :error nil}
         (catch Exception e
           {:file-data (str "\n" (str (.getMessage e)) "\n")
            :error     nil ;(str "read-local-file, caught exception: " (.getMessage e))
            }))))

(defn write-local-file
  "write local file with given data (forked fabric version)"
  [full-path file-data]
  (let [fqd?      (or (cstr/starts-with? full-path "/") (cstr/starts-with? full-path "~"))
        output    (run-shell-command "pwd")
        pwd       (first (get-in output [:output] []))
        full-path (if fqd? full-path (str pwd "/" full-path))]
    (ut/pp [:writing-file full-path])
    (do (try (spit full-path file-data)
             (catch Exception e
               (do (println "err")
                   {;:file-data file-data
                    :status    :error
                    :file-path full-path
                    :error     (str "caught exception: " (.getMessage e))})))
        {:status :ok :file-path full-path})))

(defn models-list-to-map [models-list]
  (let [is-header? #(cstr/ends-with? % ":")]
    (loop [remaining models-list
           current-header nil
           current-items []
           result {}]
      (if-let [item (first remaining)]
        (if (is-header? item)
          (recur (rest remaining)
                 (cstr/replace item ":" "")
                 []
                 (cond-> result
                   current-header (assoc current-header current-items)))
          (recur (rest remaining)
                 current-header
                 (conj current-items item)
                 result))
        ;; last group and empty groups 
        (cond-> result
          current-header (assoc current-header current-items))))))

(defn- kebab-case
  "Convert a string to kebab-case"
  [s]
  (-> s
      (cstr/replace #"([a-z0-9])([A-Z])" "$1-$2")
      (cstr/replace #"([A-Z])([A-Z][a-z])" "$1-$2")
      (cstr/replace #"_" "-")
      (cstr/replace #":" "")
      cstr/lower-case))

(defn- shell-escape
  "Escape a string for safe use in shell commands"
  [s]
  (-> s
      (cstr/replace #"'" "'\\''")  ; Replace ' with '\''
      (->> (format "'%s'"))))    ; Wrap the entire string in single quotes

(defn get-fabric-models []
  (let [output (run-shell-command "fabric --listmodels")]
    (if (= (get output :error-code 0) 0)
      (models-list-to-map (get output :output)) ;; check for malformed first?
      (get output :exception))))

(defn get-fabric-patterns []
  (let [output (run-shell-command "fabric --list")]
    (if (= (get output :error-code 0) 0)
      (get output :output)
      (get output :exception))))

(defn fabric
  "Wrapper for the fabric CLI application.
   Required arguments:
   :input   - The input text (will be passed as --text)
   :pattern - The pattern to use
   
   Optional arguments (all others from the CLI help):
   :copy, :agents, :output, :session, :clear-session, :session-log,
   :list-sessions, :gui, :stream, :list, :temp, :top-p, :frequency-penalty,
   :presence-penalty, :update, :setup, :change-default-model, :model,
   :list-models, :remote-ollama-server, :context"
  [& {:keys [input pattern] :as opts}]
  (when (or (nil? input) (nil? pattern))
    (throw (IllegalArgumentException. "Both :input and :pattern are required arguments.")))

  (let [id (get opts :id)
        opts (dissoc opts :id)
        opts (if id (-> opts
                       ;; (assoc :session (str id))
                        (assoc :output  (str "./fabric-outputs/" id))) opts)
        cli-args (reduce-kv
                  (fn [args k v]
                    (case k
                      :input (conj args "--text" (shell-escape v))
                      (into args
                            (if (boolean? v)
                              [(str "--" (name (kebab-case k)))]
                              [(str "--" (name (kebab-case k))) (str v)]))))
                  ["fabric"]  ; Start with the command name
                  opts)
        command (cstr/join " " cli-args)]
    (write-local-file (str "../fabric-inputs/" id) (str opts "\n \n \n" command))
    (ut/pp [:fabric-cli command])

    ;; Execute the command
    ;(shell/sh "/bin/bash" "-c" (str "mkdir -p shell-root ; cd shell-root ; " command))
    command))

(ext/create-dirs "./fabric-sessions") ;; from base rvbbit folder
(ext/create-dirs "./fabric-outputs")
(ext/create-dirs "./fabric-inputs")

;; (ut/pp [:fabric-models (get-fabric-models)])
;; (ut/pp [:fabric-patterns (get-fabric-patterns)])

;; (let [fb-cli (fabric :input "Funny joke about birds" 
;;                      :pattern "tweet" 
;;                      ;:session "12345a" 
;;                      :output "../fabric-outputs/12345a" 
;;                      :model "gpt-4o")]
;;   (ut/pp [:fabric-cli fb-cli :output (run-shell-command fb-cli)]))

;; (write-local-file (str "../fabric-inputs/hey" ) "he y hey hye")
