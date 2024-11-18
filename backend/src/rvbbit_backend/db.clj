(ns rvbbit-backend.db
  [:require
   ;[rvbbit-backend.xtdb-init :as xtdb-init]
   [rvbbit-backend.util       :as ut :refer [ne?]]
   [clojure.string            :as cstr]
   [clojure.edn               :as edn]
   [clojure.data.json         :as json]
   [clojure.set               :as cset]
   [clojure.walk              :as walk]
   [clj-http.client           :as http]
   [io.pedestal.http          :as phttp]
   [datalevin.core            :as d]
   [rvbbit-backend.pool-party :as ppy]
   [rvbbit-backend.freezepop :as fpop]
   [rvbbit-backend.config  :refer [settings-atom]]
   [flowmaps.db               :as flow-db]])

;; datalevin stuff
(defonce ddb (d/open-kv "db/shape-rotations"))
;; (defonce shapes-db "shapes-db")

(defn ddb-put! [coll-name kkey vval]
  (d/transact-kv ddb [[:put coll-name kkey vval]]))

(defn ddb-get [coll-name kkey]
  (d/get-value ddb coll-name kkey))

(defn open-ddb [coll-name]
  (ut/pp ["📊" :opening-datalevin-shape-rotations-collection coll-name])
  (d/open-dbi ddb coll-name))

(defn close-ddb []
  (d/close-kv ddb))


;; (open-ddb "honeyhash-map") ;; start datalevin instance
;; (open-ddb "honeyhash-map") ;; start datalevin instance

;; (d/sync-kv ddb)

;; (ddb-put! "honeyhash-map" 650895555 {:farts 123})
;; (mapv first (d/get-range ddb "honeyhash-map" [:all]))
;; (d/get-value ddb "honeyhash-map" 650895555)

;; (ddb-put! "honeyhash-map" 650895555 {:farts 123})
;; (ddb-get "honeyhash-map" 650895555 )
;; (filterv #(= (first %) 650895555) (d/get-range ddb "honeyhash-map" [:all]))

;; (mapv first (d/get-range ddb "honeyhash-map" [:all]))
;; (first (d/get-range ddb "honeyhash-map" [:all]))
;; (d/get-value ddb "honeyhash-map" 131743921)


;; hikari pool connection map
(defonce conn-map (atom {}))

;; the rabbit "reactor"...
(def key-depth-limit (atom 8))

(defonce mem-usage (atom [])) ;; needs to be accessed from evaluator
(defonce cpu-usage (atom [])) ;; needs to be accessed from evaluator

(defonce shape-rotation-status (atom {}))

(defonce leaf-brute-force-map (atom {}))
(defonce leaf-brute-force-last-panel-hash (atom {}))
(defonce query-runstream-cache (atom {}))
(defonce drag-body-map (fpop/thaw-atom {} "./data/atoms/drag-body-map-atom.msgpack.transit"))

(defonce shapes-result-map (atom {})) ; (fpop/thaw-atom {} "./data/atoms/shapes-result-map-atom.msgpack.transit"))
;; (defonce shapes-result-crosswalk (fpop/thaw-atom {} "./data/atoms/shapes-result-crosswalk-atom.msgpack.transit"))
;; ^^ client-name, panel, view, {:shapes [all reco block maps] -and- :fields [field maps]}
(defonce shapes-result-map-by-honey-hash (atom {})) ; (fpop/thaw-atom {} "./data/atoms/shapes-result-map-by-honey-hash-atom.msgpack.transit"))
;; ^^ honey-hash, {:shapes [all reco block maps] -and- :fields [field maps]}

(defonce realm-atom (atom {}))

(defonce viz-reco-sniffs (atom 0))
(defonce leaf-evals (atom 0))

(defonce leaf-bench (fpop/thaw-atom {} "./data/atoms/leaf-bench-atom.msgpack.transit")) ;; temp for perf work

(def client-panels (atom {})) ;(fpop/thaw-atom {} "./data/atoms/client-panels-atom.msgpack.transit"))
(def client-panels-resolved (atom {}))
(def client-panels-materialized (atom {}))
(def client-panels-data (atom {}))  ;; (fpop/thaw-atom {} "./data/atoms/client-panels-data-atom.msgpack.transit"))
(def client-panels-metadata (atom {})) ;; (fpop/thaw-atom {} "./data/atoms/client-panels-metadata-atom.msgpack.transit"))

(def ack-scoreboard (atom {}))  ;; (fpop/thaw-atom {} "./data/atoms/ack-scoreboard-atom.msgpack.transit"))

(defonce kit-atom (fpop/thaw-atom {} "./data/atoms/kit-atom.msgpack.transit"))
(defonce father-time (fpop/thaw-atom {} "./data/atoms/father-time-atom.edn")) ;; a hedge; since thread starvation has as times been an issue, cascading into the scheduler itself.
(defonce screens-atom (atom {})) ;; (fpop/thaw-atom {} "./data/atoms/screens-atom.edn"))
(defonce server-atom (atom {})) ;; (fpop/thaw-atom {} "./data/atoms/server-atom.edn"))
(defonce ai-worker-atom (fpop/thaw-atom {} "./data/atoms/ai-worker-atom.msgpack.transit"))
(defonce flow-status (atom {}))
(defonce kit-status (atom {}))

(defonce model-costs (fpop/thaw-atom {"Anthropic"
                                      {"claude-3-5-sonnet"  {:per-mtok-input 3.0 :per-mtok-output 15.0}
                                       "claude-3-opus"      {:per-mtok-input 15.0 :per-mtok-output 75.00}
                                       "claude-3-haiku"     {:per-mtok-input 0.25 :per-mtok-output 1.25}}}
                                     "./ai-workers/model-costs.edn"))

(defonce actions-atom (fpop/thaw-atom {} "./data/atoms/actions-atom.msgpack.transit"))
(defonce kpi-atom (fpop/thaw-atom {} "./data/atoms/kpi-atom.msgpack.transit"))
(defonce metric-atom (fpop/thaw-atom {} "./data/atoms/metric-atom.msgpack.transit"))
(defonce runstream-atom (atom {}))
(defonce params-atom (atom  {})) ;; stop persisting params, they are dynamic and can be reloaded live (do we *really* care about dead rabbit session params? no)
(defonce panels-atom (fpop/thaw-atom {} "./data/atoms/panels-atom.msgpack.transit"))
(defonce leaf-atom (atom {})) ;(fpop/thaw-atom {} "./data/atoms/leaf-atom.msgpack.transit"))
(defonce leaf-drags-atom (atom {}))
(defonce solver-status (atom {}))
(defonce query-metadata (atom {})) ;(fpop/thaw-atom {} "./data/atoms/query-metadata.msgpack.transit"))
(defonce last-signals-history-atom (fpop/thaw-atom {} "./data/atoms/last-signals-history-atom.msgpack.transit"))
(defonce last-signal-value-atom (fpop/thaw-atom {} "./data/atoms/last-signal-value-atom.msgpack.transit"))
(defonce last-signals-atom-stamp (fpop/thaw-atom {} "./data/atoms/last-signals-atom-stamp.msgpack.transit"))
(defonce last-solvers-atom (fpop/thaw-atom {} "./data/atoms/last-solvers-atom.msgpack.transit"))
(defonce last-solvers-data-atom (atom {})) ;; (fpop/thaw-atom {} "./data/atoms/last-solvers-data-atom.msgpack.transit"))
(defonce last-solvers-atom-meta (fpop/thaw-atom {} "./data/atoms/last-solvers-atom-meta..msgpack.transit"))
(defonce last-solvers-history-atom (fpop/thaw-atom {} "./data/atoms/last-solvers-history-atom.msgpack.transit"))
(defonce last-solvers-history-counts-atom (fpop/thaw-atom {} "./data/atoms/last-solvers-history-counts-atom.msgpack.transit"))
(defonce last-signals-atom (fpop/thaw-atom {} "./data/atoms/last-signals-atom.msgpack.transit"))
(defonce repl-introspection-atom (fpop/thaw-atom {} "./data/atoms/repl-introspection-atom.msgpack.transit"))
(defonce incoming-atom (fpop/thaw-atom {} "./data/atoms/incoming-atom.msgpack.transit"))
(defonce splitter-stats (volatile! {}))
(defonce atoms-and-watchers (atom {}))
(defonce watcher-log (atom {}))
(defonce client-click-params (atom {}))

;; (defonce last-values (fpop/thaw-atom {} "./data/atoms/last-values.msgpack.transit"))
;; (defonce last-values-per (fpop/thaw-atom {} "./data/atoms/last-values-per.msgpack.transit"))

(defonce param-var-mapping (atom {}))
(defonce param-var-crosswalk (atom {}))
(defonce param-var-key-mapping (atom {}))

(defonce clover-gen (fpop/thaw-atom {} "./data/atoms/clover-gen-atom.msgpack.transit"))

(def clover-sql-training-atom (fpop/thaw-atom {} "./data/training/clover-sql-training-atom.edn"))
(def clover-sql-enriched-training-atom (fpop/thaw-atom {} "./data/training/clover-sql-enriched-training-atom.edn"))

(def master-atom-map
  [[:master-time-watcher  father-time  :time]
   [:master-clover-gen  clover-gen  :clover-gen]
   [:master-leaf-watcher  leaf-atom  :leaf]
   [:master-incoming-watcher  incoming-atom  :incoming]
   [:master-actions-watcher  actions-atom  :actions]
   [:master-settings-watcher  settings-atom  :settings]
   [:master-kpi-watcher  kpi-atom  :kpi]
   [:master-metric-watcher  metric-atom  :metric]
   [:master-screen-watcher  screens-atom  :screen]
   [:master-params-watcher  params-atom  :client]
   [:master-panels-watcher  panels-atom  :panel]
   [:master-assistant-watcher  ai-worker-atom  :ai-worker]
   [:master-runstream-watcher  runstream-atom  :runstream]
   [:master-flow-watcher flow-db/results-atom  :flow]
   [:master-flow-runner-watcher flow-db/results-atom  :flow-runner] ;; * a copy, but it maintainted separately - pruned, etc.
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

(defn get-master-atom [name-keyword]
  (second (first (filter #(= (last %) name-keyword) master-atom-map))))

;; (ut/pp (get @last-solvers-atom :fortunate-cubic-donkey-22))

(def sharded-atoms
  (atom {:time (atom {})
         :settings (atom {})
         :clover-gen (atom {})
         :leaf (atom {})
         :screen (atom {})
         :incoming (atom {})
         :actions (atom {})
         :ai-worker (atom {})
         :client (atom {})
         :kpi (atom {})
         :metric (atom {})
         :panel (atom {})
         :flow (atom {})
         :flow-runner (atom {})
         :runstream (atom {})
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

(def type-keys (conj (keys @sharded-atoms) :*data))

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
                               (= master-type :runstream)
                               (= master-type :flow-runner)
                              ;;  (and (= master-type :ai-worker)
                              ;;       (not (cstr/includes? (str coded-keypath) "threads-for")))
                               (= master-type :tracker)
                               (= master-type :flow-status))
                         (cons (str (second parts))
                               (map (fn [s] (if (re-matches #"\d+" s) (Integer/parseInt s) (keyword s))) (drop 2 parts)))
                         (map (fn [s] (if (re-matches #"\d+" s) (Integer/parseInt s) (keyword s))) path))
           limited-path (take @key-depth-limit parsed-path)]
       (into [master-type] limited-path)))))

;; (def parse-coded-keypath
;;   (memoize
;;    (fn [coded-keypath]
;;      (let [parts (cstr/split (ut/safe-name coded-keypath) #"[/>]")
;;            master-type (keyword (first (cstr/split (cstr/replace (str coded-keypath) ":" "") #"/")))
;;            path (rest parts)
;;            parsed-path (cond
;;                          (and (= master-type :ai-worker)
;;                               (not (cstr/includes? (str coded-keypath) "threads-for")))
;;                          (cons (str (second parts))
;;                                (cons (str (nth parts 2))
;;                                      (map (fn [s] (if (re-matches #"\d+" s) (Integer/parseInt s) (keyword s))) (drop 3 parts))))

;;                          (or (= master-type :flow)
;;                              (= master-type :runstream)
;;                              (= master-type :flow-runner)
;;                              (= master-type :tracker)
;;                              (= master-type :flow-status))
;;                          (cons (str (second parts))
;;                                (map (fn [s] (if (re-matches #"\d+" s) (Integer/parseInt s) (keyword s))) (drop 2 parts)))

;;                          :else
;;                          (map (fn [s] (if (re-matches #"\d+" s) (Integer/parseInt s) (keyword s))) path))
;;            limited-path (take @key-depth-limit parsed-path)]
;;        (into [master-type] limited-path)))))

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
        master-type (if (= master-type :*data) :data master-type)
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

(defn pool-cleaner! []
  (ppy/reset-cached-thread-pools-wildcard ":subscriptions")
  (ppy/reset-cached-thread-pools-wildcard ":watchers")
  (ppy/reset-cached-thread-pools-wildcard ":nrepl")
  (ppy/reset-cached-thread-pools-wildcard ":serial-nrepl")
  (ppy/reset-cached-thread-pools-wildcard ":flow")
  (ppy/reset-cached-thread-pools-wildcard ":sql")
  (ppy/reset-cached-thread-pools-wildcard ":param")
  (ppy/reset-cached-thread-pools-wildcard ":client")
  (ppy/reset-cached-thread-pools-wildcard ":query")
  (ppy/reset-cached-thread-pools-wildcard ":signal")
  (ppy/reset-cached-thread-pools-wildcard ":captured"))

;; (pool-cleaner!)
;; (reboot-reactor!)

(defn reboot-reactor! []
  (doseq [ckp (vec (apply concat (for [[_ a] @sharded-atoms] (keys @a))))]
    (unsub-atom-splitter-deep ckp))
  (doseq [cn (keys @atoms-and-watchers)]
    (ppy/reset-cached-thread-pools-wildcard (cstr/replace (str cn) ":" "")))
  (reset! atoms-and-watchers {})
  (pool-cleaner!)
  (reset! reactor-boot-time (System/currentTimeMillis))
  ;; (qp/cleanup-inactive-queues 10) ;; issues with this, not really important though
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
  (let [client-name-str (cstr/replace (str client-name) ":" "")
        flow-key-str (str flow-key)
        ;; flow-key-str (let [client-name-str (if (not (or (cstr/starts-with? (str flow-key) ":solver/*")
        ;;                                            (cstr/starts-with? (str flow-key) ":solver-meta/*")))
        ;;                                      "rvbbit"
        ;;                                      client-name-str)]
        ;;                   ;; ^^ this is super hacky - we need to unify the flowkey reactor types and behavior. ideally generalized and data drive with config maps.
        ;;                   ;; this means that its an auto generated solver name which needs a client-name prefix, rather than a global solver, which needs rvbbit as the cid
        ;;                (-> flow-key-str
        ;;                    (cstr/replace ":solver/" (str ":solver/" client-name-str ">"))
        ;;                    (cstr/replace ":solver-meta/" (str ":solver-meta/" client-name-str ">"))))
        ]
    (edn/read-string ;; should always be a keyword coming in
     (ut/replace-multiple flow-key-str {"*client-name*" client-name-str}))))

(defn break-up-flow-key
  [key]
  (let [ff  (cstr/split (-> (str key)
                            (cstr/replace #":" ""))
                        #"/")
        ff2 (cstr/split (last ff) #">")]
    [(first ff2) (keyword (last ff2))]))

(defn break-up-flow-key-ext ;;; ex :flow/flow-id>flow-block-data>0>1>key34 etc
  [key]
  (let [ff    (cstr/split (-> (str key) (cstr/replace #":" "")) #"/")
        ff2   (cstr/split (last ff) #">")
        keyword-or-int (fn [s] (if (re-matches #"\d+" s) (Integer/parseInt s) (keyword s)))]
    (vec (into [(keyword (first ff)) (first ff2)] (rest (for [e ff2] (keyword-or-int e)))))))

(defn client-kp
  [flow-key keypath base-type sub-path client-param-path client-name]
    (cond (cstr/includes? (str flow-key) "running?")  false ;; TODO, mess
          (= base-type :time)                         client-param-path
          (= base-type :signal)                       client-param-path
          (= base-type :settings)                     (vec (into [(keyword (second sub-path))] (vec (rest (rest sub-path)))))
          (= base-type :kpi)                          (vec (into [(keyword (second sub-path))] (vec (rest (rest sub-path)))))
          (= base-type :metric)                       (vec (into [(keyword (second sub-path))] (vec (rest (rest sub-path)))))
          (= base-type :solver)                       (vec (into [(keyword (second sub-path))] (vec (rest (rest sub-path)))))
          (= base-type :ai-worker)                    (vec (into [(keyword (second sub-path))] (vec (rest (rest sub-path)))))
          (= base-type :kit)                          (vec (into [(keyword (second sub-path))] (vec (rest (rest sub-path)))))
          (= base-type :solver-meta)                  (vec (into [(keyword (second sub-path))] (vec (rest (rest sub-path)))))
          (= base-type :incoming)                     (vec (into [(keyword (second sub-path))] (vec (rest (rest sub-path)))))
          (= base-type :actions)                      (vec (into [(keyword (second sub-path))] (vec (rest (rest sub-path)))))
          (= base-type :repl-ns)                      (vec (into [(keyword (second sub-path))] (vec (rest (rest sub-path)))))
          (= base-type :solver-status)                (vec (into [(keyword (second sub-path))] (vec (rest (rest sub-path)))))
          (= base-type :clover-gen)                   (vec (into [(keyword (second sub-path))] (vec (rest (rest sub-path)))))
          (= base-type :leaf)                         (vec (into [(keyword (second sub-path))] (vec (rest (rest sub-path)))))
          (= base-type :flow-status)                  keypath ;;(vec (rest sub-path)) ;;keypath ;; (vec (into [(keyword (second sub-path))] (vec (rest (rest sub-path)))))
          (= base-type :kit-status)                   (vec (into [(keyword (second sub-path))] (vec (rest (rest sub-path)))))
          (= base-type :signal-history)               (vec (into [(keyword (second sub-path))] (vec (rest (rest sub-path))))) ;;(vec
          (= base-type :server)                       client-param-path
          (= base-type :screen)                       (vec (rest sub-path))
          (= base-type :client)                       (vec (into [(keyword (second sub-path))] (vec (rest (rest sub-path)))))
          (= base-type :panel)                        (vec (into [(keyword (second sub-path))] (vec (rest (rest sub-path)))))
          :else                                       (vec (rest sub-path)) ;;keypath ;;(vec (rest sub-path)) ;;keypath ;; assume flow
          ))

(defn get-atom-from-keys [base-type sub-type sub-path keypath]
    (let [base-type (if (= base-type :*data) :data base-type)]
      (get-atom-splitter-deep-kp base-type keypath (base-type master-reactor-atoms))))

(declare trigger-hooks)

(defn make-watcher
  [keypath flow-key client-name handler-fn & [no-save]]
  (fn [kkey atom old-state new-state]
    (let [client-name          :all ;; test, no need for individual cache for clients. kinda
          old-value          (get-in old-state keypath)
          new-value          (get-in new-state keypath)
          sub-path           (break-up-flow-key-ext flow-key)
          base-type          (first sub-path)
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

                                         (ppy/execute-in-thread-pools :trigger-hooks (fn [] (trigger-hooks flow-key new-value)))
                                         ;; push to any subbed REST endpoints

                                         (swap! client-click-params assoc-in [client-name flow-key] new-value) ;; for diffing later

                                        ;;  (when (cstr/starts-with? (str client-name) ":gor")
                                        ;;    (ut/pp [:running-push! flow-key client-name keypath new-value]))

                                        ;;  (when (cstr/ends-with? (str flow-key) "*running?") (ut/pp [:running-push! flow-key client-name keypath new-value]))
                                        ;; (when (cstr/includes? (str flow-key) ":solver") (ut/pp [:running-push! flow-key client-name keypath new-value]))

                                        ;;   (when (cstr/includes? (str client-name) "-bronze-")  (ut/pp [:running-push! flow-key client-name keypath new-value]))
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

                                         (when
                                          (not (and (= base-type :leaf) (empty? new-value)))
                                          ;; ^^ leaf reaction hack TODO, find root cause of sporadic empty resolve
                                           (handler-fn base-type keypath client-name new-value))

                                         ))
          (when (and (not no-save) (ut/serializable? new-value)) ;; dont want to cache tracker
            ;(swap! last-values assoc keypath new-value)
            (ddb-put! "last-values" (hash keypath) new-value)
            ;(swap! last-values-per assoc-in [client-name keypath] new-value)
            ) ;; if a new client
          )))))

(defn add-watcher
  [keypath client-name fn flow-key sub-type & [flow-id]] ;; flow id optional is for unsub stuff
  (let [;;client-name :all ;; test
        sub-path        (break-up-flow-key-ext flow-key)
        watch-key       (str :all "-" (str keypath) "-" sub-type "-" flow-key)
        base-type       (first sub-path)
        ;tracker?        (= sub-type :tracker)
        ;flow?           (= base-type :flow) ;; (cstr/starts-with? (str flow-key) ":flow/")
        ;status?         (and (cstr/includes? (str keypath) ":*") flow?)
        client?         (= base-type :client)
        panel?          (= base-type :panel)
        screen?         (= base-type :screen) ;; (cstr/starts-with? (str flow-key) ":screen/")
        time?           (= base-type :time)
        signal?         (= base-type :signal)
        kpi?            (= base-type :kpi)
        metric?         (= base-type :metric)
        incoming?       (= base-type :incoming)
        actions?        (= base-type :actions)
        ai-worker?      (= base-type :ai-worker)
        settings?       (= base-type :settings)
        solver?         (= base-type :solver)
        kit?            (= base-type :kit)
        data?           (or (= base-type :data)
                            (= base-type :*data))
        solver-meta?    (= base-type :solver-meta)
        repl-ns?        (= base-type :repl-ns)
        solver-status?  (= base-type :solver-status)
        clover-gen?     (= base-type :clover-gen)
        leaf?           (= base-type :leaf)
        flow-status?    (= base-type :flow-status)
        kit-status?     (= base-type :kit-status)
        signal-history? (= base-type :signal-history)
        server?         (= base-type :server)
        ;keypath         (if (= (first keypath) (cstr/replace (str (second keypath)) ":" "")) [(first keypath)] keypath)
        ;base-type       (if status? :flow-status base-type)
        ;; base-type       (cond status? :flow-status
        ;;                       tracker? :tracker
        ;;                       :else base-type)
        keypath         (cond ;flow? keypath
                          ;;;true [:v]
                          signal?         (vec (rest keypath))
                          settings?       (vec (into [(keyword (second sub-path))] (vec (rest (rest sub-path)))))
                          solver?         (vec (into [(keyword (second sub-path))] (vec (rest (rest sub-path)))))
                          kpi?            (vec (into [(keyword (second sub-path))] (vec (rest (rest sub-path)))))
                          metric?         (vec (into [(keyword (second sub-path))] (vec (rest (rest sub-path)))))
                          kit?            (vec (into [(keyword (second sub-path))] (vec (rest (rest sub-path)))))
                          actions?        (vec (into [(keyword (second sub-path))] (vec (rest (rest sub-path)))))
                          ai-worker?      (vec (into [(keyword (second sub-path))] (vec (rest (rest sub-path)))))
                          data?           (vec (into [(keyword (second sub-path))] (vec (rest (rest sub-path)))))
                          incoming?       (vec (into [(keyword (second sub-path))] (vec (rest (rest sub-path)))))
                          solver-meta?    (vec (into [(keyword (second sub-path))] (vec (rest (rest sub-path)))))
                          repl-ns?        (vec (into [(keyword (second sub-path))] (vec (rest (rest sub-path)))))
                          solver-status?  (vec (into [(keyword (second sub-path))] (vec (rest (rest sub-path)))))
                          clover-gen?     (vec (into [(keyword (second sub-path))] (vec (rest (rest sub-path)))))
                          leaf?           (vec (into [(keyword (second sub-path))] (vec (rest (rest sub-path)))))
                          flow-status?    (vec (rest sub-path)) ;keypath ;;(vec (into [(keyword (second sub-path))] (vec (rest (rest sub-path)))))
                          kit-status?     (vec (into [(keyword (second sub-path))] (vec (rest (rest sub-path)))))
                          signal-history? (vec (into [(keyword (second sub-path))] (vec (rest (rest sub-path)))))
                          time?           (vec (rest keypath))
                          server?         (vec (rest keypath))
                          screen?         (vec (rest sub-path))
                          panel?          (vec (into [(keyword (second sub-path))] (vec (rest (rest sub-path)))))
                          client?         (vec (into [(keyword (second sub-path))] (vec (rest (rest sub-path)))))
                          :else           (vec (rest sub-path)) ;;keypath ;;(vec (rest sub-path)) ;;keypath
                          )
        ;;keypath             (if flow? keypath (vec (rest keypath)))
        ;flow-key          (if status? (edn/read-string (cstr/replace (str flow-key) ":flow/" ":flow-status/")) flow-key)
        base-type         (if (= base-type :*data) :data base-type)
        watcher          (make-watcher keypath flow-key :all fn (= sub-type :tracker))
        atom-to-watch    (get-atom-splitter-deep flow-key (get master-reactor-atoms base-type))]

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
          ;tracker? (= sub-type :tracker)
          ;flow? (= base-type :flow) ;; (cstr/starts-with? (str flow-key) ":flow/")
          ;status? (and (cstr/includes? (str keypath) ":*") flow?)
          client? (= base-type :client)
          panel? (= base-type :panel)
          screen? (= base-type :screen) ;; (cstr/starts-with? (str flow-key)
                                        ;; ":screen/")
          time? (= base-type :time)
          signal? (= base-type :signal)
          settings? (= base-type :settings)
          solver? (= base-type :solver)
          incoming? (= base-type :incoming)
          actions? (= base-type :actions)
          kpi? (= base-type :kpi)
          metric? (= base-type :metric)
          ai-worker? (= base-type :ai-worker)
          kit? (= base-type :kit)
          flow-status? (= base-type :flow-status)
          kit-status? (= base-type :kit-status)
          data? (or (= base-type :data) (= base-type :*data))
          solver-meta? (= base-type :solver-meta)
          repl-ns?        (= base-type :repl-ns)
          solver-status?  (= base-type :solver-status)
          clover-gen?   (= base-type :clover-gen)
          leaf?         (= base-type :leaf)
          signal-history? (= base-type :signal-history)
          server?         (= base-type :server)
          ;keypath         (if (= (first keypath) (cstr/replace (str (second keypath)) ":" "")) [(first keypath)] keypath)
          ;; base-type       (cond status? :flow-status
          ;;                       tracker? :tracker
          ;;                       :else base-type)
          keypath (cond ;flow? keypath
                    ;;;true [:v]
                    signal?         (vec (rest keypath))
                    data?           (vec (into [(keyword (second sub-path))] (vec (rest (rest sub-path)))))
                    solver?         (vec (into [(keyword (second sub-path))] (vec (rest (rest sub-path)))))
                    settings?       (vec (into [(keyword (second sub-path))] (vec (rest (rest sub-path)))))
                    kit?            (vec (into [(keyword (second sub-path))] (vec (rest (rest sub-path)))))
                    incoming?       (vec (into [(keyword (second sub-path))] (vec (rest (rest sub-path)))))
                    ai-worker?      (vec (into [(keyword (second sub-path))] (vec (rest (rest sub-path)))))
                    kpi?            (vec (into [(keyword (second sub-path))] (vec (rest (rest sub-path)))))
                    actions?        (vec (into [(keyword (second sub-path))] (vec (rest (rest sub-path)))))
                    metric?         (vec (into [(keyword (second sub-path))] (vec (rest (rest sub-path)))))
                    solver-meta?    (vec (into [(keyword (second sub-path))] (vec (rest (rest sub-path)))))
                    repl-ns?        (vec (into [(keyword (second sub-path))] (vec (rest (rest sub-path)))))
                    solver-status?  (vec (into [(keyword (second sub-path))] (vec (rest (rest sub-path)))))
                    clover-gen?     (vec (into [(keyword (second sub-path))] (vec (rest (rest sub-path)))))
                    leaf?           (vec (into [(keyword (second sub-path))] (vec (rest (rest sub-path)))))
                    flow-status?    keypath ;; (vec (into [(keyword (second sub-path))] (vec (rest (rest sub-path)))))
                    kit-status?     (vec (into [(keyword (second sub-path))] (vec (rest (rest sub-path)))))
                    signal-history? (vec (into [(keyword (second sub-path))] (vec (rest (rest sub-path)))))
                    time?           (vec (rest keypath))
                    screen?         (vec (rest sub-path))
                    server?         (vec (rest sub-path))
                    panel?          (vec (into [(keyword (second sub-path))] (vec (rest (rest sub-path)))))
                    client?         (vec (into [(keyword (second sub-path))] (vec (rest (rest sub-path)))))
                    :else           (vec (rest sub-path)) ;;keypath
                    )
          base-type         (if (= base-type :*data) :data base-type)
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
        ;keypath         (if (= (first keypath) (cstr/replace (str (second keypath)) ":" "")) [(first keypath)] keypath)
        sub-path                (break-up-flow-key-ext flow-key)
        base-type               (first sub-path)
        ;;flow-client-param-path  (keyword (cstr/replace (str (first keypath) (last keypath)) #":" ">"))
        other-client-param-path (keyword (cstr/replace (cstr/join ">" (vec (rest sub-path))) ":" ""))
        ;; client-param-path       (if (or (= base-type :flow-status)
        ;;                                  ;;(= base-type :kit-status)
        ;;                                 (= base-type :flow)) flow-client-param-path other-client-param-path)
        client-param-path       other-client-param-path
        client-keypath          (client-kp flow-key keypath base-type sub-path client-param-path client-name)
        ssp                     (break-up-flow-key-ext flow-key-orig)
        req-client-kp           (client-kp flow-key-orig
                                           (vec (break-up-flow-key flow-key-orig))
                                           base-type
                                           ssp
                                           (keyword (cstr/replace (cstr/join ">" (vec (rest ssp))) ":" ""))
                                           client-name)
        ;;_ (ut/pp [:flow-key flow-key vars? flow-key-sub flow-key-split flow-key-split-sub]) ;; this
        _ (when vars? (swap! param-var-mapping assoc [client-name client-keypath] req-client-kp))
        _ (when vars? (swap! param-var-crosswalk assoc-in [client-name flow-key-orig] [flow-key [client-name client-keypath]]))
        _ (when vars?
            (swap! param-var-key-mapping assoc
                   client-name
                   (vec (distinct (conj (get @param-var-key-mapping client-name []) [flow-key-orig flow-key])))))
        lv                      ;(get @last-values keypath)
                                (ddb-get "last-values" (hash keypath))

        ]
    ;; (ut/pp [:solver-lookup! flow-key base-type client-param-path keypath
    ;;         {:sub-path sub-path
    ;;          :new-path (vec (into [client-name] (into [(keyword (second sub-path))] (vec (rest (rest sub-path))))))}])
    (cond
      ;;(cstr/includes? (str flow-key) "*running?")  false
      (= base-type :time)                         (get @father-time client-param-path)
      ;;(= base-type :time)                         (get @(get-atom-splitter (ut/hash-group (keyword (second sub-path)) num-groups) :time time-child-atoms father-time) client-param-path)
      (= base-type :signal)                       (get-in @settings-atom
                                                          (vec (into [(keyword (second sub-path))] (vec (rest (rest sub-path)))))
                                                          lv)
      (= base-type :settings)                     (get @settings-atom client-param-path)
      (or (= base-type :data)
          (= base-type :*data))                         (get-in @last-solvers-data-atom
                                                                (vec (into [(keyword (second sub-path))] (vec (rest (rest sub-path)))))
                                                                lv)
      (= base-type :solver)                       (get-in @last-solvers-atom
                                                          (vec (into [(keyword (second sub-path))] (vec (rest (rest sub-path)))))
                                                          lv)
      (= base-type :incoming)                     (get-in @incoming-atom
                                                          (vec (into [(keyword (second sub-path))] (vec (rest (rest sub-path)))))
                                                          lv)
      (= base-type :actions)                      (get-in @actions-atom
                                                          (vec (into [(keyword (second sub-path))] (vec (rest (rest sub-path)))))
                                                          lv)
      (= base-type :kpi)                          (get-in @kpi-atom
                                                          (vec (into [(keyword (second sub-path))] (vec (rest (rest sub-path)))))
                                                          lv)
      (= base-type :ai-worker)                    (get-in @ai-worker-atom
                                                          (vec (into [(keyword (second sub-path))] (vec (rest (rest sub-path)))))
                                                          lv)
      (= base-type :metric)                       (get-in @metric-atom
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
      (= base-type :solver-status)                (get-in @solver-status
                                                          (vec (into [(keyword (second sub-path))] (vec (rest (rest sub-path)))))
                                                          lv)
      (= base-type :clover-gen)                  (get-in @clover-gen
                                                         (vec (into [(keyword (second sub-path))] (vec (rest (rest sub-path)))))
                                                         lv)
      (= base-type :leaf)                        (get-in @leaf-atom
                                                         (vec (into [(keyword (second sub-path))] (vec (rest (rest sub-path)))))
                                                         lv)
      (= base-type :flow-status)                  (get-in @flow-status (vec (rest sub-path)) lv)
      (= base-type :runstream)                    (get-in @runstream-atom (vec (rest sub-path)) lv)
      (= base-type :kit-status)                   (get-in @kit-status
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
      :else                                       (get-in @flow-db/results-atom (vec (rest sub-path)) lv) ;; assume flow literal
      )))

(defn get-starting-value [base-type client-param-path sub-path keypath lv client-name]
    (cond
                ;;(cstr/includes? (str flow-key) "running?")  false
      (= base-type :time)                         (get @father-time client-param-path)
                    ;;(= base-type :time)                       (get @(get-atom-splitter (ut/hash-group (keyword (second sub-path)) num-groups) :time time-child-atoms father-time) client-param-path)
      (= base-type :signal)                       (get @last-signals-atom client-param-path)
      (or (= base-type :data)
          (= base-type :*data))                   (get-in @last-solvers-data-atom
                                                          (vec (into [(keyword (second sub-path))] (vec (rest (rest sub-path)))))
                                                          lv)
      (= base-type :settings)                     (get-in @settings-atom
                                                          (vec (into [(keyword (second sub-path))]
                                                                     (vec (rest (rest sub-path)))))
                                                          lv)
      (= base-type :solver)                       (get-in @last-solvers-atom
                                                          (vec (into [(keyword (second sub-path))] (vec (rest (rest sub-path)))))
                                                          lv)
      (= base-type :kit)                          (get-in @kit-atom
                                                          (vec (into [(keyword (second sub-path))]
                                                                     (vec (rest (rest sub-path)))))
                                                          lv)
      (= base-type :kpi)                          (get-in @kpi-atom
                                                          (vec (into [(keyword (second sub-path))] (vec (rest (rest sub-path)))))
                                                          lv)
      (= base-type :metric)                       (get-in @metric-atom
                                                          (vec (into [(keyword (second sub-path))] (vec (rest (rest sub-path)))))
                                                          lv)
      (= base-type :incoming)                     (get-in @incoming-atom
                                                          (vec (into [(keyword (second sub-path))] (vec (rest (rest sub-path)))))
                                                          lv)
      (= base-type :actions)                      (get-in @actions-atom
                                                          (vec (into [(keyword (second sub-path))] (vec (rest (rest sub-path)))))
                                                          lv)
      (= base-type :solver-meta)                  (get-in @last-solvers-atom-meta
                                                          (vec (into [(keyword (second sub-path))] (vec (rest (rest sub-path)))))
                                                          lv)
      (= base-type :ai-worker)                    (get-in @ai-worker-atom
                                                          (vec (into [(keyword (second sub-path))] (vec (rest (rest sub-path)))))
                                                          lv)
      (= base-type :repl-ns)                      (get-in @repl-introspection-atom
                                                          (vec (into [(keyword (second sub-path))]
                                                                     (vec (rest (rest sub-path)))))
                                                          lv)
      (= base-type :solver-status)                (get-in @solver-status
                                                          (vec (into [(keyword (second sub-path))]
                                                                     (vec (rest (rest sub-path)))))
                                                          lv)
      (= base-type :clover-gen)                   (get-in @clover-gen
                                                          (vec (into [(keyword (second sub-path))]
                                                                     (vec (rest (rest sub-path)))))
                                                          lv)
      (= base-type :leaf)                         (get-in @leaf-atom
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
      (= base-type :flow-status)                  (get-in @flow-status (vec (rest sub-path)) lv)
      (= base-type :runstream)                    (get-in @runstream-atom (vec (rest sub-path)) lv)
      (= base-type :kit-status)                   (get-in @kit-status
                                                          (vec (into [(keyword (second sub-path))]
                                                                     (vec (rest (rest sub-path)))))
                                                          lv)
      :else                                       (get-in @flow-db/results-atom (vec (rest sub-path)) lv)))



;; Reactor REST hooks, get, set.

(defn send-edn-success [content]
  (assoc (phttp/edn-response content) :headers {"Content-Type" "application/edn"}))  ;"text/plain"

(defn get-value [namespaced-key client-name]
  (let [vv (clover-lookup client-name namespaced-key)]
    (ut/pp [:reactor-REST-get-value! namespaced-key :from client-name :value vv])
    vv))

(defn set-value! [namespaced-key value client-name]
  (try
    (let [nms (parse-coded-keypath namespaced-key)
        ttype (first nms)
        matom (get-master-atom ttype)
        _ (ut/pp [:reactor-REST-put-value! nms namespaced-key value])
        _ (swap! matom assoc-in (vec (rest nms)) value)]
      :done!)
    (catch Exception e
      (ut/pp [:reactor-REST-put-value-error! namespaced-key value e])
      {:error (str "Error setting value: " (.getMessage e))})))


(def allowed-type-keywords (set (for [k type-keys] (name k))))

(defn parse-keyword [s]
  (if (cstr/starts-with? s ":")
    (keyword (subs s 1))
    (keyword s)))

(defn construct-namespaced-keyword [type keypath]
  (keyword (name type) (name keypath)))

(defn parse-body [content-type body]
  (case content-type
    "application/edn"  (edn/read-string body)
    "application/json" (json/read-str body :key-fn keyword)
    body))

(defn get-reactor-value [request]
  (let [type-str   (get-in request [:path-params :type-keyword])
        keypath-str (get-in request [:path-params :keypath-keyword])
        caller-ip    (or (get-in request [:headers "x-forwarded-for"])
                         (:remote-addr request))
        client-name   (if (nil? caller-ip) :rvbbit-rest (keyword (str "rvbbit-rest-"caller-ip)))]
    (if (or (cstr/blank? type-str) (cstr/blank? keypath-str))
      (send-edn-success {:error "Both type and keypath must be provided"})
      (let [type-keyword    (parse-keyword type-str)
            keypath-keyword (parse-keyword keypath-str)
            namespaced-key  (construct-namespaced-keyword type-keyword keypath-keyword)
            value (get-value namespaced-key client-name)]
        ;; We don't validate type-keyword for GET requests. it'd just return nil
        (ut/ppln [:get-reactor-value namespaced-key :from client-name])
        (send-edn-success {:value value})))))

(defn put-reactor-value [request]
  (let [type-str    (get-in request [:path-params :type-keyword])
        keypath-str (get-in request [:path-params :keypath-keyword])
        caller-ip    (or (get-in request [:headers "x-forwarded-for"])
                         (:remote-addr request))
        client-name   (if (nil? caller-ip) :rvbbit-rest (keyword (str "rvbbit-rest-" caller-ip)))]
    (if (or (cstr/blank? type-str) (cstr/blank? keypath-str))
      (send-edn-success {:error "Both type and keypath must be provided"})
      (let [type-keyword (parse-keyword type-str)]
        ;; Validate the type keyword against the allowed set
        ;; This is crucial as it determines the data source
        (if-not (contains? allowed-type-keywords (name type-keyword))
          (send-edn-success {:error (str "Invalid data source: " type-keyword)})
          (let [keypath-keyword (parse-keyword keypath-str)
                namespaced-key  (construct-namespaced-keyword type-keyword keypath-keyword)
                content-type    (get-in request [:headers "content-type"])
                body            (slurp (:body request))
                value           (try
                                  (parse-body content-type body)
                                  (catch Exception e
                                    (send-edn-success {:error (str "Error parsing request body: " (.getMessage e))})))
                _ (set-value! namespaced-key value client-name)]
            (ut/ppln [:put-reactor-value namespaced-key value])
            (send-edn-success {:status "success"})))))))


  ;; Example: Get a value or set a value, causing any reactions assoc'd with the value to fire, just like a normal param change.
  ;; curl -X GET "http://localhost:8888/reactor/time/minute"
  ;; curl -X PUT "http://localhost:8888/reactor/client/surprising-wide-turkey-1>stats>last-seen" -d 'Nice!'
  ;; curl -X PUT "http://localhost:8888/reactor/:client/:surprising-wide-turkey-1" -H "Content-Type: application/json" -d '{"stats": {"last-seen": "Nice!!"}}'
  ;; curl -X PUT "http://localhost:8888/reactor/:client/surprising-wide-turkey-1" -H "Content-Type: application/edn" -d '{:stats {:last-seen "Nice!!!!"}}'


;;; hooks
;; (def subscriptions (atom {}))
;; ;;  (fpop/thaw-atom {} "./defs/rest-subscriptions.edn")

;; (defn subscribe-hook [type-keyword keypath-keyword callback-url]
;;   (let [kkey (keyword (cstr/replace (str type-keyword "/" keypath-keyword) ":" ""))]
;;     (swap! subscriptions update kkey (fnil conj #{}) callback-url)
;;     (ut/ppln [:subscribed kkey callback-url])))

;; (defn unsubscribe-hook [type-keyword keypath-keyword callback-url]
;;   (let [kkey (keyword (cstr/replace (str type-keyword "/" keypath-keyword) ":" ""))]
;;     (swap! subscriptions update kkey disj callback-url)
;;     (ut/ppln [:unsubscribed kkey callback-url])))

(def subscriptions (fpop/thaw-atom {} "./defs/rest-subscriptions.edn"))

(defn trigger-hooks [key new-value]
  (when-let [callbacks (get @subscriptions key)]
    (doseq [callback-url callbacks]
      (future
        (try
          (http/post callback-url
                     {:body (json/write-str {:reactor-key key
                                             :value new-value})
                      :content-type :json})
          (ut/ppln [:hook-triggered callback-url])
          (catch Exception e
            (ut/ppln [:hook-error callback-url (.getMessage e)])))))))

(defn subscribe-hook [type-keyword keypath-keyword callback-url client-name]
  (let [kkey (keyword (cstr/replace (str type-keyword "/" keypath-keyword) ":" ""))]
    (swap! subscriptions update kkey
           (fn [urls]
             (vec (distinct (conj (or urls []) callback-url)))))
    (trigger-hooks kkey (get-value kkey client-name)) ;; kick it for initial value
    (ut/ppln [:REST-subscribed kkey callback-url :from client-name])))

(defn unsubscribe-hook [type-keyword keypath-keyword callback-url client-name]
  (let [kkey (keyword (cstr/replace (str type-keyword "/" keypath-keyword) ":" ""))]
    (swap! subscriptions update kkey
           (fn [urls]
             (vec (remove #(= % callback-url) (or urls [])))))
    (ut/ppln [:REST-unsubscribed kkey callback-url :from client-name])))

(defn manage-hook [request]
  (let [type-str    (get-in request [:path-params :type-keyword])
        keypath-str (get-in request [:path-params :keypath-keyword])
        body        (parse-body (get-in request [:headers "content-type"])
                                (slurp (:body request)))
        caller-ip    (or (get-in request [:headers "x-forwarded-for"])
                         (:remote-addr request))
        client-name   (if (nil? caller-ip) :rvbbit-rest (keyword (str "rvbbit-rest-" caller-ip)))
        callback-url (:callback-url body)
        unsubscribe  (:unsubscribe body)]
    (if (or (cstr/blank? type-str) (cstr/blank? keypath-str) (cstr/blank? callback-url))
      (send-edn-success {:error "Type, keypath, and callback URL must be provided"})
      (let [type-keyword    (parse-keyword type-str)
            keypath-keyword (parse-keyword keypath-str)]
        (if unsubscribe
          (do (unsubscribe-hook type-keyword keypath-keyword callback-url client-name)
              (send-edn-success {:status "unsubscribed"}))
          (do (subscribe-hook type-keyword keypath-keyword callback-url client-name)
              (send-edn-success {:status "subscribed"})))))))

;; (ut/pp [:hooks @subscriptions]) ;; persist?
;; (ut/pp [:hooks @atoms-and-watchers])

  ;; Example: Subscribe to changes
  ;; curl -X POST "http://localhost:8888/reactor-hook/time/second"  -H "Content-Type: application/json" -d '{"callback-url": "http://localhost:8500"}'
  ;; curl -X POST "http://localhost:8888/reactor-hook/time/second"  -H "Content-Type: application/json" -d '{"callback-url": "http://localhost:8500", "unsubscribe": true}'

  ;; curl -X POST "http://localhost:8888/reactor-hook/:client/surprising-wide-turkey-1>stats>last-seen"  -H "Content-Type: application/json" -d '{"callback-url": "http://localhost:8500"}'
  ;; curl -X POST "http://localhost:8888/reactor-hook/:client/surprising-wide-turkey-1>stats>last-seen"  -H "Content-Type: application/json" -d '{"callback-url": "http://localhost:8500", "unsubscribe": true}'

;;  :client/surprising-wide-turkey-1>stats>last-seen




