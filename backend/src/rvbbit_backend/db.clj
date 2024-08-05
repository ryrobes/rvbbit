(ns rvbbit-backend.db 
  [:require
   [rvbbit-backend.util       :as ut :refer [ne?]]
   [clojure.string            :as cstr]
   [clojure.edn               :as edn]
   [clojure.set               :as cset]
   [clojure.walk              :as walk]
   [rvbbit-backend.pool-party :as ppy]
   [rvbbit-backend.queue-party  :as qp]
   [flowmaps.db               :as flow-db]])

(def key-depth-limit (atom 8)) 

(defonce kit-atom (ut/thaw-atom {} "./data/atoms/kit-atom.edn"))
(defonce father-time (ut/thaw-atom {} "./data/atoms/father-time-atom.edn")) ;; a hedge; since thread starvation has as times been an issue, cascading into the scheduler itself.
(defonce screens-atom (atom {})) ;; (ut/thaw-atom {} "./data/atoms/screens-atom.edn"))
(defonce server-atom (ut/thaw-atom {} "./data/atoms/server-atom.edn"))
(defonce flow-status (atom {}))
(defonce kit-status (atom {}))
(defonce params-atom (atom  {})) ;; stop persisting params, they are dynamic and can be reloaded live (do we *really* care about dead rabbit session params? no)
(defonce panels-atom (ut/thaw-atom {} "./data/atoms/panels-atom.edn"))
(defonce solver-status (atom {}))
(defonce last-signals-history-atom (ut/thaw-atom {} "./data/atoms/last-signals-history-atom.edn"))
(defonce last-signal-value-atom (ut/thaw-atom {} "./data/atoms/last-signal-value-atom.edn"))
(defonce last-signals-atom-stamp (ut/thaw-atom {} "./data/atoms/last-signals-atom-stamp.edn"))
(defonce last-solvers-atom (ut/thaw-atom {} "./data/atoms/last-solvers-atom.edn"))
(defonce last-solvers-data-atom (ut/thaw-atom {} "./data/atoms/last-solvers-data-atom.edn"))
(defonce last-solvers-atom-meta (ut/thaw-atom {} "./data/atoms/last-solvers-atom-meta.edn"))
(defonce last-solvers-history-atom (ut/thaw-atom {} "./data/atoms/last-solvers-history-atom.edn"))
(defonce last-solvers-history-counts-atom (ut/thaw-atom {} "./data/atoms/last-solvers-history-counts-atom.edn"))
(defonce last-signals-atom (ut/thaw-atom {} "./data/atoms/last-signals-atom.edn"))
(defonce repl-introspection-atom (ut/thaw-atom {} "./data/atoms/repl-introspection-atom.edn"))
(defonce splitter-stats (volatile! {}))
(defonce atoms-and-watchers (atom {}))
(defonce watcher-log (atom {}))

(defonce last-values (ut/thaw-atom {} "./data/atoms/last-values.edn"))
(defonce last-values-per (ut/thaw-atom {} "./data/atoms/last-values-per.edn"))

(defonce param-var-mapping (atom {}))
(defonce param-var-crosswalk (atom {}))
(defonce param-var-key-mapping (atom {}))

(def master-atom-map
  [[:master-time-watcher  father-time  :time]
   [:master-screen-watcher  screens-atom  :screen]
   [:master-params-watcher  params-atom  :client]
   [:master-panels-watcher  panels-atom  :panel]
   [:master-flow-watcher flow-db/results-atom  :flow]
   [:master-nrepl-instrospection-watcher repl-introspection-atom  :repl-ns]
   [:master-solver-status-watcher  solver-status  :solver-status]
   [:master-signals-watcher  last-signals-atom  :signal]
   [:master-solver-watcher  last-solvers-atom  :solver]
   [:master-solver-meta-watcher  last-solvers-atom-meta :solver-meta]
   [:master-flow-status-watcher  flow-status  :flow-status]
   [:master-kit-status-watcher  kit-status  :kit-status]
   [:master-kit-watcher  kit-atom  :kit]
   [:master-data-watcher  last-solvers-data-atom  :data]
   [:master-signal-history-watcher  last-signals-history-atom  :signal-history]
   [:master-server-watcher server-atom :server]
   [:master-tracker-watcher flow-db/tracker :tracker]])

(def sharded-atoms
  (atom {:time (atom {})
         :screen (atom {})
         :client (atom {})
         :panel (atom {})
         :flow (atom {})
         :repl-ns (atom {})
         :solver-status (atom {})
         :signal (atom {})
         :kit (atom {})
         :solver (atom {})
         :solver-meta (atom {})
         :flow-status (atom {})
         :kit-status  (atom {})
         :data (atom {})
         :signal-history (atom {})
         :server (atom {})
         :tracker (atom {})}))

;; (ut/pp @kit-atom)

(defn current-watchers [] 
  (set (apply concat (for [[_ v] @sharded-atoms] (keys @v)))))

(defn current-subs [] 
  (set (apply concat (for [[_ v] @atoms-and-watchers] (keys v)))))

(defn current-all-subs [] 
  (set (apply concat (for [[k v] @atoms-and-watchers] 
                       (for [vv (keys v)] [k vv])))))

(def master-reactor-atoms
  (into {} (for [[_ atom base-type] master-atom-map]
             {base-type atom})))

(def reactor-boot-time (atom (System/currentTimeMillis)))


(defn reactor-uptime-seconds []
  (let [uptime-ms      (- (System/currentTimeMillis) @reactor-boot-time)
        uptime-seconds (/ uptime-ms 1000.0)] ;; Note the 1000.0 for floating point division
    (Math/round uptime-seconds)))

(def parse-coded-keypath
  (memoize
   (fn [coded-keypath]
     (let [parts (cstr/split (ut/safe-name coded-keypath) #"[/>]")
           master-type (keyword (first (cstr/split (cstr/replace (str coded-keypath) ":" "") #"/")))
           path (rest parts)
           parsed-path (if (or (= master-type :flow)
                               (= master-type :flow-status))
                         (cons (str (second parts))
                               (map (fn [s] (if (re-matches #"\d+" s) (Integer/parseInt s) (keyword s))) (drop 2 parts)))
                         (map (fn [s] (if (re-matches #"\d+" s) (Integer/parseInt s) (keyword s))) path))
           limited-path (take @key-depth-limit parsed-path)]
       (into [master-type] limited-path)))))

(def create-coded-keypath
  (memoize
   (fn [base-type keypath]
     (let [limited-keypath (take @key-depth-limit keypath)
           keypath-str (cstr/join ">" (map (fn [k]
                                             (cond
                                               (keyword? k) (ut/safe-name k)
                                               (string? k) k
                                               :else (str k)))
                                           limited-keypath))]
       (keyword (str (ut/safe-name base-type) "/" keypath-str))))))

(defn get-atom-splitter-deep
  [coded-keypath parent-atom]
  (let [;coded-keypath (if (and (cstr/starts-with? (str coded-keypath) ":flow/") (cstr/includes? (str coded-keypath) "*running"))
        ;                (edn/read-string (cstr/replace (str e) ":flow/" ":flow-status/"))
        ;                coded-keypath)
        parsed-keypath (parse-coded-keypath coded-keypath)
        [master-type & rest-path] parsed-keypath
        limited-path (take @key-depth-limit rest-path)
        limited-coded-keypath (create-coded-keypath master-type limited-path)
        namespace-key (keyword (cstr/replace (name master-type) ":" ""))
        child-atoms (get @sharded-atoms namespace-key)]
    (if-let [child-atom (get @child-atoms limited-coded-keypath)]
      child-atom
      (let [parent-value (get-in @parent-atom limited-path)
            new-child-atom (atom {(last parsed-keypath) parent-value})]
        ;;(ut/pp [:new-child-atom-created-for! coded-keypath])
        ;;(vswap! splitter-stats assoc-in [namespace-key limited-coded-keypath] 0)
        (swap! child-atoms assoc limited-coded-keypath new-child-atom)
        new-child-atom))))

(defn unsub-atom-splitter-deep [coded-keypath]
  (let [;coded-keypath (if (and (cstr/starts-with? (str coded-keypath) ":flow/") (cstr/includes? (str coded-keypath) "*running"))
        ;                (edn/read-string (cstr/replace (str e) ":flow/" ":flow-status/"))
        ;                coded-keypath)
        namespace-key (keyword (cstr/replace (first (cstr/split (str coded-keypath) #"/")) ":" ""))
        child-atoms (get @sharded-atoms namespace-key)]
    (when child-atoms
      (when-let [child-atom (get @child-atoms coded-keypath)]
        ;; Perform any cleanup on the child-atom if necessary
        (swap! child-atoms dissoc coded-keypath)
        ;; Update statistics
        (vswap! splitter-stats update namespace-key dissoc coded-keypath)
        ;; Log the unsubscription
        (ut/pp [:reactor-stopped-watching coded-keypath])
        ;; Return true if unsubscription was successful
        true))))

(defn clean-up-reactor []
  (let [watchers (set (apply concat (for [[_ v] @sharded-atoms] (keys @v))))
        subs (set (apply concat (for [[_ v] @atoms-and-watchers] (keys v))))
        no-longer-needed (cset/difference watchers subs)
        sub-cnt (count subs)
        watcher-cnt (count watchers)]
    (when (> (count no-longer-needed) 0)
      (ut/pp [:cleaning-up-watchers :needed sub-cnt :have watcher-cnt :removing no-longer-needed])
      ;;(ut/pp [:removing no-longer-needed])
      (doseq [flow-key no-longer-needed]
        (unsub-atom-splitter-deep flow-key)))))

(defn reboot-reactor! []
  (doseq [ckp (vec (apply concat (for [[_ a] @sharded-atoms] (keys @a))))]
    (unsub-atom-splitter-deep ckp))
  (doseq [cn (keys @atoms-and-watchers)]
    (ppy/reset-cached-thread-pools-wildcard (cstr/replace (str cn) ":" "")))
  (reset! atoms-and-watchers {})
  (ppy/reset-cached-thread-pools-wildcard ":subscriptions")
  (ppy/reset-cached-thread-pools-wildcard ":watchers")
  (ppy/reset-cached-thread-pools-wildcard ":nrepl")
  (ppy/reset-cached-thread-pools-wildcard ":flow")
  (ppy/reset-cached-thread-pools-wildcard ":sql")
  (ppy/reset-cached-thread-pools-wildcard ":param")
  (ppy/reset-cached-thread-pools-wildcard ":client")
  (ppy/reset-cached-thread-pools-wildcard ":query")
  (ppy/reset-cached-thread-pools-wildcard ":signal")
  (ppy/reset-cached-thread-pools-wildcard ":captured")
  (reset! reactor-boot-time (System/currentTimeMillis))
  (qp/cleanup-inactive-queues 10)
  (vreset! splitter-stats  {}))

(defn client-sub-latency []
  (let [last-times (into {} (for [[_ v] @splitter-stats]
                              (into {} (for [[kk vv] v] {kk (get vv :last)}))))]
    (dissoc
     (into {} (for [[k v] @watcher-log]
                {k (apply max (for [[kk vv] v]
                                (try
                                  (- (get vv :last-push-ms)
                                     (get last-times kk)) (catch Exception _ 1))))})) :rvbbit)))

(defn client-subs-late-delivery [ms]
  (vec (keys (filter #(> (val %) ms) (client-sub-latency)))))



(defn master-watch-splitter-deep [name-kw parent-atom]
  ;(qp/add-watch+
  (ppy/add-watch+
   parent-atom name-kw
   (fn [_ _ old-state new-state]
     ;(when (not= old-state new-state)
     (let [;[added removed _] (clojure.data/diff old-state new-state)
             ;akp (ut/kvpaths added)
             ;rkp (ut/kvpaths removed)
             ;ckp (distinct (into akp rkp))
           ckp (ut/diff-keypaths old-state new-state)
           child-atoms (get @sharded-atoms name-kw)
           changed-coded (for [c ckp] (create-coded-keypath name-kw (take @key-depth-limit c)))
           keypath-work (select-keys @child-atoms changed-coded)
             ;cnts (count keypath-work)
           ]
       (when (seq keypath-work) ;;(pos? cnts)
         (doseq [[coded-keypath child-atom] keypath-work]
             ;(when (cstr/starts-with? (str coded-keypath) (str name-kw))
           (let [keypath (parse-coded-keypath coded-keypath)
                 [_ & path] keypath
                 new-value (get-in new-state path)
                 old-value (get-in @child-atom path)
                     ;old-value (get-in old-state path)
                 ]
             (when (not= old-value new-value)
               (vswap! splitter-stats assoc-in [name-kw coded-keypath]
                       {:cnt (inc (get-in @splitter-stats [name-kw coded-keypath :cnt] 0))
                        :last (System/currentTimeMillis)})
                           ;;[(fnil inc 0) (System/currentTimeMillis)])
               (swap! child-atom ;; {(last keypath) new-value})
                   ;;(fn [_] {(last keypath) new-value})
                      (fn [old-state]
                        (if (= old-state {})
                          {(last keypath) new-value}
                          (assoc-in old-state path new-value)))))))))););)
   name-kw))

(defn get-atom-splitter-deep-kp [ttype keypath parent-atom]
  (let [coded-keypath (create-coded-keypath ttype keypath)]
    (get-atom-splitter-deep coded-keypath parent-atom)))

(defn replace-flow-key-vars
  [flow-key client-name]
  (let [client-name-str (cstr/replace (str client-name) ":" "")]
    (edn/read-string ;; should always be a keyword coming in
     (ut/replace-multiple (str flow-key) {"*client-name*" client-name-str}))))

(defn break-up-flow-key
  [key]
  (let [ff  (cstr/split (-> (str key)
                            (cstr/replace #":" ""))
                        #"/")
        ff2 (cstr/split (last ff) #">")]
    [(first ff2) (keyword (last ff2))]))

(defn break-up-flow-key-ext ;;; ex :flow/flow-id>flow-block-data>0>1>key34 etc 
  [key]
  (let [ff             (cstr/split (-> (str key)
                                       (cstr/replace #":" ""))
                                   #"/")
        ff2            (cstr/split (last ff) #">")
        keyword-or-int (fn [s] (if (re-matches #"\d+" s) (Integer/parseInt s) (keyword s)))]
    (vec (into [(keyword (first ff)) (first ff2)] (rest (for [e ff2] (keyword-or-int e)))))))

(defn client-kp
  [flow-key keypath base-type sub-path client-param-path]
  (cond (cstr/includes? (str flow-key) "running?")  false
        (= base-type :time)                         client-param-path
        (= base-type :signal)                       client-param-path
        (= base-type :solver)                       (vec (into [(keyword (second sub-path))] (vec (rest (rest sub-path)))))
        (= base-type :kit)                          (vec (into [(keyword (second sub-path))] (vec (rest (rest sub-path)))))
        (= base-type :solver-meta)                  (vec (into [(keyword (second sub-path))] (vec (rest (rest sub-path)))))
        (= base-type :repl-ns)                      (vec (into [(keyword (second sub-path))] (vec (rest (rest sub-path)))))
        (= base-type :solver-status)                (vec (into [(keyword (second sub-path))] (vec (rest (rest sub-path)))))
        (= base-type :flow-status)                  keypath ;; (vec (into [(keyword (second sub-path))] (vec (rest (rest sub-path)))))
        (= base-type :kit-status)                   (vec (into [(keyword (second sub-path))] (vec (rest (rest sub-path)))))
        (= base-type :signal-history)               (vec (into [(keyword (second sub-path))] (vec (rest (rest sub-path))))) ;;(vec
        (= base-type :server)                       client-param-path
        (= base-type :screen)                       (vec (rest sub-path))
        (= base-type :client)                       (vec (into [(keyword (second sub-path))] (vec (rest (rest sub-path)))))
        (= base-type :panel)                        (vec (into [(keyword (second sub-path))] (vec (rest (rest sub-path)))))
        :else                                       keypath ;; assume flow
        ))

(defn get-atom-from-keys [base-type sub-type sub-path keypath]
  (let [;base-type (if (and (cstr/includes? (str keypath) ":*") 
        ;                   (= base-type :flow)) :flow-status base-type)
        ]
    (get-atom-splitter-deep-kp base-type keypath (base-type master-reactor-atoms))))

(defn make-watcher
  [keypath flow-key client-name handler-fn & [no-save]]

  (fn [kkey atom old-state new-state]
    (let [client-name          :all ;; test, no need for individual cache for clients. kinda
          old-value          (get-in old-state keypath)
          new-value          (get-in new-state keypath)
          sub-path           (break-up-flow-key-ext flow-key)
          base-type          (first sub-path)
          last-value         (get-in @last-values-per [client-name keypath])
          all-clients-subbed (for [c     (keys @atoms-and-watchers)
                                   :when (some #(= % (cstr/replace (str flow-key) ":" ""))
                                          ;; normalizes since old clients had keyword colons at weird places 
                                               (vec (for [fkw (keys (get @atoms-and-watchers c))] (cstr/replace (str fkw) ":" ""))))]
                               c)]
      (when ;(or
       (and (not (nil? new-value))
            (not= old-value new-value) ;; or look at last-val?
            )
        (doseq [client-name all-clients-subbed]
            ;(qp/serial-slot-queue :watcher-to-client-serial client-name
            ;(ppy/execute-in-thread-pools (keyword (str "watcher-to-client/" (cstr/replace (str client-name) ":" "")))
          (ppy/execute-in-thread-pools :watcher-to-client
                                       (fn []
                                         (swap! watcher-log
                                                (fn [log]
                                                  (-> log
                                                      (assoc-in [client-name flow-key :pushes]
                                                                (inc (get-in @watcher-log [client-name flow-key :pushes] 0)))
                                                      (assoc-in [client-name flow-key :last-push-ms]
                                                                (System/currentTimeMillis))
                                                      (assoc-in [client-name flow-key :last-push]
                                                                (last (cstr/split (get (ut/current-datetime-parts) :now) #", "))))))
                                          ;;  (when (cstr/includes? (str client-name) "-fat-")  (ut/pp [:running-push! flow-key client-name new-value]))
                                          ;;  (ppy/execute-in-thread-pools (keyword (str "reaction-logger/" (cstr/replace (str client-name) ":" "")))
                                          ;;                               (fn []
                                          ;;                                 (when (or (not= flow-key :time/now)
                                          ;;                                           (not= flow-key :time/second)) ;;(not (cstr/starts-with? (str flow-key) ":time/"))
                                          ;;                                   (spit (str "./reaction-logs/" (str (cstr/replace (str client-name) ":" "")) ".log")
                                          ;;                                         (str (ut/get-current-timestamp) "  " client-name "  " flow-key " " (ut/data-typer new-value) "\n" 
                                          ;;                                              (grab-string-chunk old-value) 
                                          ;;                                              "\n" 
                                          ;;                                              (grab-string-chunk new-value) 
                                          ;;                                              "\n\n\n") :append true)
                                          ;;           ;(ut/pp [:running-push! flow-key client-name])
                                          ;;                                   )))
                                         (handler-fn base-type keypath client-name new-value)))
          (when (and (not no-save) (ut/serializable? new-value)) ;; dont want to cache tracker
            (swap! last-values assoc keypath new-value)
            (swap! last-values-per assoc-in [client-name keypath] new-value)) ;; if a new client
          )))))

(defn add-watcher
  [keypath client-name fn flow-key sub-type & [flow-id]] ;; flow id optional is for unsub stuff
  (let [;;client-name :all ;; test
        sub-path        (break-up-flow-key-ext flow-key)
        watch-key       (str :all "-" (str keypath) "-" sub-type "-" flow-key)
        base-type       (first sub-path)
        tracker?        (= sub-type :tracker)
        flow?           (= base-type :flow) ;; (cstr/starts-with? (str flow-key) ":flow/")
        ;status?         (and (cstr/includes? (str keypath) ":*") flow?)
        client?         (= base-type :client)
        panel?          (= base-type :panel)
        screen?         (= base-type :screen) ;; (cstr/starts-with? (str flow-key) ":screen/")
        time?           (= base-type :time)
        signal?         (= base-type :signal)
        solver?         (= base-type :solver)
        kit?            (= base-type :kit)
        data?           (= base-type :data)
        solver-meta?    (= base-type :solver-meta)
        repl-ns?        (= base-type :repl-ns)
        solver-status?  (= base-type :solver-status)
        flow-status?    (= base-type :flow-status)
        kit-status?     (= base-type :kit-status)
        signal-history? (= base-type :signal-history)
        server?         (= base-type :server)
        ;base-type       (if status? :flow-status base-type)
        ;; base-type       (cond status? :flow-status
        ;;                       tracker? :tracker
        ;;                       :else base-type)
        keypath         (cond ;flow? keypath
                          ;;;true [:v]
                          signal?         (vec (rest keypath))
                          solver?         (vec (into [(keyword (second sub-path))] (vec (rest (rest sub-path)))))
                          kit?            (vec (into [(keyword (second sub-path))] (vec (rest (rest sub-path)))))
                          data?           (vec (into [(keyword (second sub-path))] (vec (rest (rest sub-path)))))
                          solver-meta?    (vec (into [(keyword (second sub-path))] (vec (rest (rest sub-path)))))
                          repl-ns?        (vec (into [(keyword (second sub-path))] (vec (rest (rest sub-path)))))
                          solver-status?  (vec (into [(keyword (second sub-path))] (vec (rest (rest sub-path)))))
                          flow-status?    keypath ;;(vec (into [(keyword (second sub-path))] (vec (rest (rest sub-path)))))
                          kit-status?     (vec (into [(keyword (second sub-path))] (vec (rest (rest sub-path)))))
                          signal-history? (vec (into [(keyword (second sub-path))] (vec (rest (rest sub-path)))))
                          time?           (vec (rest keypath))
                          server?         (vec (rest keypath))
                          screen?         (vec (rest sub-path))
                          panel?          (vec (into [(keyword (second sub-path))] (vec (rest (rest sub-path)))))
                          client?         (vec (into [(keyword (second sub-path))] (vec (rest (rest sub-path)))))
                          :else           keypath)
        ;;keypath             (if flow? keypath (vec (rest keypath)))
        ;flow-key          (if status? (edn/read-string (cstr/replace (str flow-key) ":flow/" ":flow-status/")) flow-key) 
        watcher          (make-watcher keypath flow-key :all fn (= sub-type :tracker))
        atom-to-watch    (get-atom-splitter-deep flow-key (get master-reactor-atoms base-type))
        ;; atom-to-watch2       (cond
        ;;                       ;status?         (get-atom-splitter (first keypath) :flow flow-status-child-atoms flow-status)
        ;;                       status?         (split-2-deep base-type sub-path flow-status-child-atoms flow-status :flow)
        ;;                       ;tracker?         (get-atom-splitter (first keypath) :flow flow-tracker-child-atoms flow-db/tracker)
        ;;                       tracker?        (split-2-deep base-type sub-path flow-tracker-child-atoms flow-db/tracker)
        ;;                       ;signal?         (get-atom-splitter (ut/hash-group (keyword (second sub-path)) num-groups) :signal signal-child-atoms last-signals-atom)
        ;;                       signal?         (split-2-deep base-type sub-path signal-child-atoms last-signals-atom)
        ;;                       ;solver?         (get-atom-splitter (keyword (second sub-path)) :solver solver-child-atoms last-solvers-atom)
        ;;                       solver?         (split-2-deep base-type sub-path solver-child-atoms last-solvers-atom)
        ;;                       ;solver-meta?    (get-atom-splitter-2deep (keyword (second sub-path)) (keyword (get sub-path 2)) :solver-meta solver-meta-child-atoms last-solvers-atom-meta)
        ;;                       solver-meta?    (split-2-deep base-type sub-path solver-meta-child-atoms last-solvers-atom-meta)
        ;;                       ;repl-ns?        (get-atom-splitter (keyword (second sub-path)) :repl-ns evl/repl-introspection-child-atoms evl/repl-introspection-atom)
        ;;                       repl-ns?        (split-2-deep base-type sub-path evl/repl-introspection-child-atoms evl/repl-introspection-atom)
        ;;                       ;solver-status?  (get-atom-splitter-2deep (keyword (second sub-path)) (keyword (get sub-path 2)) :solver-status solver-status-child-atoms solver-status)
        ;;                       solver-status?  (split-2-deep base-type sub-path solver-status-child-atoms solver-status)
        ;;                       data?           last-solvers-data-atom
        ;;                       signal-history? (split-2-deep base-type sub-path last-signals-history-child-atoms last-signals-history-atom) ;;last-signals-history-atom
        ;;                       server?         server-atom
        ;;                       ;;time?           (get-atom-splitter-2deep (keyword (second sub-path)) (keyword (get sub-path 2)) :time time-child-atoms father-time)
        ;;                       time?           (split-2-deep base-type sub-path time-child-atoms father-time)
        ;;                       ;panel?          (get-atom-splitter (keyword (second sub-path)) :panel panel-child-atoms panels-atom)
        ;;                       panel?          (split-2-deep base-type sub-path panel-child-atoms panels-atom)
        ;;                       ;client?         (get-atom-splitter (keyword (second sub-path)) :client param-child-atoms params-atom)
        ;;                       client?         (split-2-deep base-type sub-path param-child-atoms params-atom)
        ;;                   ;flow?           (get-atom-splitter (first keypath) :flow flow-child-atoms flow-db/results-atom)
        ;;                       flow?           (split-2-deep base-type sub-path flow-child-atoms flow-db/results-atom)
        ;;                       ;screen?         (get-atom-splitter (second sub-path) :screen screen-child-atoms screens-atom)
        ;;                       screen?         (split-2-deep base-type sub-path screen-child-atoms screens-atom)
        ;;                       ;:else           (get-atom-splitter (first keypath) :flow flow-child-atoms flow-db/results-atom)
        ;;                       :else           (split-2-deep base-type sub-path flow-child-atoms flow-db/results-atom))
        ]

    ;(remove-watch atom-to-watch watch-key) ;; not needed if we're just going to re-add the same atom and watch-key anyways. but good to keep in mind.

    ;;(swap! atoms-and-watchers ut/dissoc-in [client-name flow-key]) ;; pointless?
    (ppy/add-watch+ atom-to-watch watch-key watcher base-type client-name flow-key)
    (swap! atoms-and-watchers assoc-in
           [client-name flow-key]
           {:created   (ut/get-current-timestamp)
            :sub-type  sub-type
            :sub-path  sub-path
            :base-type base-type
            :flow-key  flow-key
            :watch-key watch-key
            :flow-id   flow-id
            :keypath   keypath})))

(defn remove-watcher
  [keypath client-name sub-type flow-id flow-key]
  (try
    (let [sub-path (break-up-flow-key-ext flow-key)
          keypath-wildcards-map {"*client-name*" client-name ;; ALREADY IN SUB TO VALUE, NOT
                                 :*client-name*  client-name}
          keypath (walk/postwalk-replace keypath-wildcards-map keypath) ;; keypath wildcards...
          sub-path (walk/postwalk-replace keypath-wildcards-map sub-path)
          watch-key (str :all "-" (str keypath) "-" sub-type "-" flow-key)
          base-type (first sub-path)
          tracker? (= sub-type :tracker)
          flow? (= base-type :flow) ;; (cstr/starts-with? (str flow-key) ":flow/")
          status? (and (cstr/includes? (str keypath) ":*") flow?)
          client? (= base-type :client)
          panel? (= base-type :panel)
          screen? (= base-type :screen) ;; (cstr/starts-with? (str flow-key)
                                        ;; ":screen/")
          time? (= base-type :time)
          signal? (= base-type :signal)
          solver? (= base-type :solver)
          kit? (= base-type :kit)
          flow-status? (= base-type :flow-status)
          kit-status? (= base-type :kit-status)
          data? (= base-type :data)
          solver-meta? (= base-type :solver-meta)
          repl-ns?        (= base-type :repl-ns)
          solver-status?  (= base-type :solver-status)
          signal-history? (= base-type :signal-history)
          server?         (= base-type :server)
          ;; base-type       (cond status? :flow-status
          ;;                       tracker? :tracker
          ;;                       :else base-type)
          keypath (cond ;flow? keypath
                    ;;;true [:v]
                    signal?         (vec (rest keypath))
                    data?           (vec (into [(keyword (second sub-path))] (vec (rest (rest sub-path)))))
                    solver?         (vec (into [(keyword (second sub-path))] (vec (rest (rest sub-path)))))
                    kit?            (vec (into [(keyword (second sub-path))] (vec (rest (rest sub-path)))))
                    solver-meta?    (vec (into [(keyword (second sub-path))] (vec (rest (rest sub-path)))))
                    repl-ns?        (vec (into [(keyword (second sub-path))] (vec (rest (rest sub-path)))))
                    solver-status?  (vec (into [(keyword (second sub-path))] (vec (rest (rest sub-path)))))
                    flow-status?    keypath ;; (vec (into [(keyword (second sub-path))] (vec (rest (rest sub-path)))))
                    kit-status?     (vec (into [(keyword (second sub-path))] (vec (rest (rest sub-path)))))
                    signal-history? (vec (into [(keyword (second sub-path))] (vec (rest (rest sub-path)))))
                    time?           (vec (rest keypath))
                    screen?         (vec (rest sub-path))
                    server?         (vec (rest sub-path))
                    panel?          (vec (into [(keyword (second sub-path))] (vec (rest (rest sub-path)))))
                    client?         (vec (into [(keyword (second sub-path))] (vec (rest (rest sub-path)))))
                    :else           keypath)
          ;keypath (vec (rest keypath))
          ;;keypath (if flow? keypath (vec (rest keypath)))

          ;; atom-to-watch     (cond
          ;;                     ;status?         (get-atom-splitter (first keypath) :flow flow-status-child-atoms flow-status)
          ;;                     status?         (split-2-deep base-type sub-path flow-status-child-atoms flow-status :flow)
          ;;                     ;tracker?         (get-atom-splitter (first keypath) :flow flow-tracker-child-atoms flow-db/tracker)
          ;;                     tracker?        (split-2-deep base-type sub-path flow-tracker-child-atoms flow-db/tracker)
          ;;                     ;signal?         (get-atom-splitter (ut/hash-group (keyword (second sub-path)) num-groups) :signal signal-child-atoms last-signals-atom)
          ;;                     signal?         (split-2-deep base-type sub-path signal-child-atoms last-signals-atom)
          ;;                     ;solver?         (get-atom-splitter (keyword (second sub-path)) :solver solver-child-atoms last-solvers-atom)
          ;;                     solver?         (split-2-deep base-type sub-path solver-child-atoms last-solvers-atom)
          ;;                     ;solver-meta?    (get-atom-splitter-2deep (keyword (second sub-path)) (keyword (get sub-path 2)) :solver-meta solver-meta-child-atoms last-solvers-atom-meta)
          ;;                     solver-meta?    (split-2-deep base-type sub-path solver-meta-child-atoms last-solvers-atom-meta)
          ;;                     ;repl-ns?        (get-atom-splitter (keyword (second sub-path)) :repl-ns evl/repl-introspection-child-atoms evl/repl-introspection-atom)
          ;;                     repl-ns?        (split-2-deep base-type sub-path evl/repl-introspection-child-atoms evl/repl-introspection-atom)
          ;;                     ;solver-status?  (get-atom-splitter-2deep (keyword (second sub-path)) (keyword (get sub-path 2)) :solver-status solver-status-child-atoms solver-status)
          ;;                     solver-status?  (split-2-deep base-type sub-path solver-status-child-atoms solver-status)
          ;;                     data?           last-solvers-data-atom
          ;;                     signal-history? (split-2-deep base-type sub-path last-signals-history-child-atoms last-signals-history-atom) ;;last-signals-history-atom
          ;;                     server?         server-atom
          ;;                     ;;time?           (get-atom-splitter-2deep (keyword (second sub-path)) (keyword (get sub-path 2)) :time time-child-atoms father-time)
          ;;                     time?           (split-2-deep base-type sub-path time-child-atoms father-time)
          ;;                     ;panel?          (get-atom-splitter (keyword (second sub-path)) :panel panel-child-atoms panels-atom)
          ;;                     panel?          (split-2-deep base-type sub-path panel-child-atoms panels-atom)
          ;;                     ;client?         (get-atom-splitter (keyword (second sub-path)) :client param-child-atoms params-atom)
          ;;                     client?         (split-2-deep base-type sub-path param-child-atoms params-atom)
          ;;                 ;flow?           (get-atom-splitter (first keypath) :flow flow-child-atoms flow-db/results-atom)
          ;;                     flow?           (split-2-deep base-type sub-path flow-child-atoms flow-db/results-atom)
          ;;                     ;screen?         (get-atom-splitter (second sub-path) :screen screen-child-atoms screens-atom)
          ;;                     screen?         (split-2-deep base-type sub-path screen-child-atoms screens-atom)
          ;;                     ;:else           (get-atom-splitter (first keypath) :flow flow-child-atoms flow-db/results-atom)
          ;;                     :else           (split-2-deep base-type sub-path flow-child-atoms flow-db/results-atom))
          ;;flow-key           (if status? (edn/read-string (cstr/replace (str flow-key) ":flow/" ":flow-status/")) flow-key)
          atom-to-watch      (get-atom-splitter-deep flow-key (get master-reactor-atoms base-type))
          all-other-watchers (dissoc (into {} (for [[k v] @atoms-and-watchers] {k (vec (for [[_ v] v] (get v :watch-key)))})) client-name)
          ;; all-other-watch-keys (set
          ;;                       (for [e (distinct (apply concat (vals all-other-watchers)))]
          ;;                         (if (and (cstr/starts-with? (str e) ":flow/") (cstr/includes? (str e) "*running"))
          ;;                           (edn/read-string (cstr/replace (str e) ":flow/" ":flow-status/")) e)))
          all-other-watch-keys (set (apply concat (vals all-other-watchers)))]

      (when false ;(not (all-other-watch-keys watch-key)) ;; set as a fn? cool?
            ;;;(not (some #(= % watch-key) all-other-watch-keys))
            ;;;; since we have not CLIENT SPECIFIC watchers at this level, we just watch each distinct one and then distribute the reactions secondarily
            ;;;; we do not want to REMOVE any watchers unless NOT ONE is using it. i.e. last one out turns out the lights, kinda thing
        (do
          (ut/pp [:removing-watcher keypath client-name sub-type flow-id watch-key])
          (remove-watch atom-to-watch watch-key) ;; stop watching child atom
          (unsub-atom-splitter-deep flow-key)))  ;; remove child atom from rabbit-reactor loop 

      (swap! atoms-and-watchers ut/dissoc-in [client-name flow-key]))
    (catch Throwable e (ut/pp [:remove-watcher-error e]))))


(defn clover-lookup
  [client-name flow-key & [signal?]]
  (let [flow-key-orig           flow-key
        ;flow-key-orig           (keyword (cstr/replace (str flow-key-orig) ":" "")) ;; normalized the old weird colon inserted clover kws
        flow-key-sub            (replace-flow-key-vars flow-key client-name)
        flow-key-split          (break-up-flow-key flow-key)
        flow-key-split-sub      (break-up-flow-key flow-key-sub)
        vars?                   (not (= flow-key flow-key-sub))
        flow-key                (if vars? flow-key-sub flow-key)
        flow-key-split          (if vars? flow-key-split-sub flow-key-split)
        [flow-id step-id]       flow-key-split ;(break-up-flow-key flow-key)
        keypath                 [flow-id step-id]
        sub-path                (break-up-flow-key-ext flow-key)
        base-type               (first sub-path)
        flow-client-param-path  (keyword (cstr/replace (str (first keypath) (last keypath)) #":" ">"))
        other-client-param-path (keyword (cstr/replace (cstr/join ">" (vec (rest sub-path))) ":" ""))
        client-param-path       (if (= base-type :flow) flow-client-param-path other-client-param-path)
        client-keypath          (client-kp flow-key keypath base-type sub-path client-param-path)
        ssp                     (break-up-flow-key-ext flow-key-orig)
        req-client-kp           (client-kp flow-key-orig
                                           (vec (break-up-flow-key flow-key-orig))
                                           base-type
                                           ssp
                                           (keyword (cstr/replace (cstr/join ">" (vec (rest ssp))) ":" "")))
        ;;_ (ut/pp [:flow-key flow-key vars? flow-key-sub flow-key-split flow-key-split-sub]) ;; this
        _ (when vars? (swap! param-var-mapping assoc [client-name client-keypath] req-client-kp))
        _ (when vars? (swap! param-var-crosswalk assoc-in [client-name flow-key-orig] [flow-key [client-name client-keypath]]))
        _ (when vars?
            (swap! param-var-key-mapping assoc
                   client-name
                   (vec (distinct (conj (get @param-var-key-mapping client-name []) [flow-key-orig flow-key])))))
        lv                      (get @last-values keypath)]
    (ut/pp [:solver-lookup! flow-key base-type client-param-path keypath {:sub-path sub-path}])
    (cond
      (cstr/includes? (str flow-key) "*running?")  false
      (= base-type :time)                         (get @father-time client-param-path)
      ;;(= base-type :time)                         (get @(get-atom-splitter (ut/hash-group (keyword (second sub-path)) num-groups) :time time-child-atoms father-time) client-param-path)
      (= base-type :signal)                       (get @last-signals-atom client-param-path)
      (= base-type :data)                         (get-in @last-solvers-data-atom
                                                          (vec (into [(keyword (second sub-path))] (vec (rest (rest sub-path)))))
                                                          lv)
      (= base-type :solver)                       (get-in @last-solvers-atom
                                                          (vec (into [(keyword (second sub-path))] (vec (rest (rest sub-path)))))
                                                          lv)
      (= base-type :kit)                          (get-in @kit-atom
                                                          (vec (into [(keyword (second sub-path))] (vec (rest (rest sub-path)))))
                                                          lv)
      (= base-type :solver-meta)                  (get-in @last-solvers-atom-meta
                                                          (vec (into [(keyword (second sub-path))] (vec (rest (rest sub-path)))))
                                                          lv)
      (= base-type :repl-ns)                      (get-in @repl-introspection-atom
                                                          
                                                          (vec (into [(keyword (second sub-path))] (vec (rest (rest sub-path)))))
                                                          lv)
      (= base-type :solver-status)                  (get-in @solver-status
                                                            (vec (into [(keyword (second sub-path))] (vec (rest (rest sub-path)))))
                                                            lv)
      (= base-type :flow-status)                  (get-in @flow-status
                                                          (vec (into [(keyword (second sub-path))] (vec (rest (rest sub-path)))))
                                                          lv)
      (= base-type :kit-status)                  (get-in @kit-status
                                                         (vec (into [(keyword (second sub-path))] (vec (rest (rest sub-path)))))
                                                         lv)
      (= base-type :signal-history)               (get-in @last-signals-history-atom
                                                          (vec (into [(keyword (second sub-path))] (vec (rest (rest sub-path)))))
                                                          lv)
      (= base-type :server)                       (get @server-atom client-param-path lv)
      (= base-type :screen)                       (get-in @screens-atom (vec (rest sub-path)) lv)
      (= base-type :client)                       (get-in @params-atom
                                                          (vec (into [(keyword (second sub-path))] (vec (rest (rest sub-path)))))
                                                          lv)
      (= base-type :panel)                        (get-in @panels-atom
                                                          (vec (into [(keyword (second sub-path))] (vec (rest (rest sub-path)))))
                                                          lv)
      :else                                       (get-in @flow-db/results-atom keypath lv) ;; assume
      )))

(defn get-starting-value [base-type client-param-path sub-path keypath lv]
  (cond
                ;;(cstr/includes? (str flow-key) "running?")  false
    (= base-type :time)                         (get @father-time client-param-path)
                    ;;(= base-type :time)                       (get @(get-atom-splitter (ut/hash-group (keyword (second sub-path)) num-groups) :time time-child-atoms father-time) client-param-path)
    (= base-type :signal)                       (get @last-signals-atom client-param-path)
    (= base-type :data)                         (get-in @last-solvers-data-atom
                                                        (vec (into [(keyword (second sub-path))]
                                                                   (vec (rest (rest sub-path)))))
                                                        lv)
    (= base-type :solver)                       (get-in @last-solvers-atom
                                                        (vec (into [(keyword (second sub-path))]
                                                                   (vec (rest (rest sub-path)))))
                                                        lv)
    (= base-type :kit)                          (get-in @kit-atom
                                                        (vec (into [(keyword (second sub-path))]
                                                                   (vec (rest (rest sub-path)))))
                                                        lv)
    (= base-type :solver-meta)                  (get-in @last-solvers-atom-meta
                                                        (vec (into [(keyword (second sub-path))]
                                                                   (vec (rest (rest sub-path)))))
                                                        lv)
    (= base-type :repl-ns)                      (get-in @repl-introspection-atom
                                                        (vec (into [(keyword (second sub-path))]
                                                                   (vec (rest (rest sub-path)))))
                                                        lv)
    (= base-type :solver-status)                  (get-in @solver-status
                                                          (vec (into [(keyword (second sub-path))]
                                                                     (vec (rest (rest sub-path)))))
                                                          lv)
    ;;(= base-type :solver-status)                 {} ; nil ;; the old atom is gone anyways, lets clear the client for new values
    (= base-type :signal-history)               (get-in @last-signals-history-atom
                                                        (vec (into [(keyword (second sub-path))]
                                                                   (vec (rest (rest sub-path)))))
                                                        lv)
    (= base-type :server)                       (get @server-atom client-param-path lv)
    (= base-type :screen)                       (get-in @screens-atom (vec (rest sub-path)) lv)
    (= base-type :client)                       (get-in @params-atom
                                                        (vec (into [(keyword (second sub-path))]
                                                                   (vec (rest (rest sub-path)))))
                                                        lv)
    (= base-type :panel)                        (get-in @panels-atom
                                                        (vec (into [(keyword (second sub-path))]
                                                                   (vec (rest (rest sub-path)))))
                                                        lv)
    (= base-type :flow-status)                  (get-in @flow-status keypath lv)
    (= base-type :kit-status)                   (get-in @kit-status
                                                        (vec (into [(keyword (second sub-path))]
                                                                   (vec (rest (rest sub-path)))))
                                                        lv)
    :else                                       (get-in @flow-db/results-atom keypath lv)))