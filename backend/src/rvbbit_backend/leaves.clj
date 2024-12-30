(ns rvbbit-backend.leaves
  (:require
   ;[rvbbit-backend.xtdb-init :as xtdb-init]
   [clojure.string :as cstr]
   [clojure.walk :as walk]
   [clojure.edn :as edn]
   [clojure.set :as cset]
   [rvbbit-backend.sql        :as    sql
    :refer [sql-exec sql-query ghost-db system-db to-sql]]
   ;[rvbbit-backend.pool-party :as ppy]
   [rvbbit-backend.evaluator :as evl]
   [clojure.core.reducers :as r]
   [rvbbit-backend.shape-rotator :as rota]
   [rvbbit-backend.db :as db]
   [rvbbit-backend.config :as config]
   [rvbbit-backend.util :as ut]))

;; _    ____ ____ ____      /    _    ____ ____ _  _ ____ ____      /    ___  ____ ____ _  _ ____ _  _ ____ ____
;; |    |___ |__| |___     /     |    |___ |__| |  | |___ [__      /     |__] |__/ |__| |\ | |    |__| |___ [__
;; |___ |___ |  | |       /      |___ |___ |  |  \/  |___ ___]    /      |__] |  \ |  | | \| |___ |  | |___ ___]

;; ____ ____ ____ ____ ___ _ _  _ ____    ____ _  _ _    ____    ___  ____ ____ ____ ___     _ _  _ ____ ____ ____ ____ _  _ ____ ____    ____ _  _ ____ _ _  _ ____
;; |__/ |___ |__| |     |  | |  | |___    |__/ |  | |    |___ __ |__] |__| [__  |___ |  \    | |\ | |___ |___ |__/ |___ |\ | |    |___    |___ |\ | | __ | |\ | |___
;; |  \ |___ |  | |___  |  |  \/  |___    |  \ |__| |___ |___    |__] |  | ___] |___ |__/    | | \| |    |___ |  \ |___ | \| |___ |___    |___ | \| |__] | | \| |___  ?

;; need to refresh this on occasion...
(def current-runners (keys (get (config/settings) :runners)))

;(get @db/client-panels cid)
;(get @db/client-panels-data cid)
;(get @db/client-panels-metadata cid)

;; (ut/pp (keys @db/query-metadata) )
;; (ut/pp (get @db/client-panels :genius-prismatic-hound-25))
;; (ut/pp (set (map #(vec (take 3 %)) (filter (fn [x] (some #(= % (second x)) current-runners)) (ut/keypaths (get @db/client-panels :genius-prismatic-hound-25))))))
;; (ut/pp (get @db/client-panels-metadata (ffirst @db/client-panels-metadata)))
;; (ut/pp (keys @db/client-panels-metadata))

(def get-connection-meta
  (memoize
   (fn [connection-id]
     (let [row (sql-query system-db
                          (to-sql {:select [[:connection_str :connection-string] :user_name [:database_name :name] [:database_version :version]]
                                   :from [:connections]
                                   :where [:= :connection_id connection-id]}))
           whole-map {:database (into {} (for [[k v] (first row)]
                                           {(keyword (str (-> (str k)
                                                              (cstr/replace  ":" "")
                                                              (cstr/replace  "_" "-")))) v}))}
           mmap (into {} (for [[k v] (first row)]
                           {(keyword (str "database/"
                                          (-> (str k)
                                              (cstr/replace  ":" "")
                                              (cstr/replace  "_" "-")))) v}))]
       (merge whole-map mmap)))))

;; (defn visible-views [panels-map]
;;   (into {}
;;         (for [[k v] panels-map]
;;           (if-let [sel-view (get v :selected-view)] ;; if has selected view, trim all others, else process all
;;             (let [view-runner (first
;;                                (for [r current-runners
;;                                      :when (get-in v [r sel-view])] r))
;;                   sel-view-body (get-in v [view-runner sel-view])
;;                   v (apply dissoc v current-runners)   ;; remove ALL runners
;;                   v (assoc-in v [view-runner sel-view] ;; add back only selected
;;                               sel-view-body)]
;;               {k v}) {k v}))))



(def visible-views-cache (atom {}))

(defn visible-views [panels-map]
  (let [cache-key (hash panels-map)
        cached-result (get @visible-views-cache cache-key)]
    (if (ut/ne? cached-result)
      cached-result
      (let [result
            (into {}
                  (for [[k v] panels-map]
                    (if-let [sel-view (get v :selected-view)] ;; if has selected view, trim all others, else process all
                      (let [view-runner (first
                                         (for [r current-runners
                                               :when (get-in v [r sel-view])] r))
                            sel-view-body (get-in v [view-runner sel-view])
                            v (apply dissoc v current-runners)   ;; remove ALL runners
                            v (assoc-in v [view-runner sel-view] ;; add back only selected
                                        sel-view-body)]
                        {k v}) {k v})))]
        (swap! visible-views-cache assoc cache-key result)
        result))))

;; Function to clear the cache when needed
(defn clear-visible-views-cache! []
  (reset! visible-views-cache {}))

(def keypaths-cache (atom {}))
;; (reset! keypaths-cache {})

(defn hash-client-state [client-name]
  (let [cpans (get @db/client-panels client-name)
        ;;dbody (get-in @db/params-atom [client-name :dragging-body])
        ;;cpans-data (get @db/client-panels-data client-name) ;;?? unneeded
        cpans-meta (get @db/client-panels-metadata client-name)]
  ;;  (ut/pp ["🌿" :hashed-key client-name (vector (keys cpans)) (vector (keys cpans-meta))])
   (hash (ut/deep-remove-underscore-keys [cpans  cpans-meta]))))

;; (ut/pp (get @db/client-panels :superb-Prussian-blue-gnu-36))

(defn view-keypaths [client-name & [tab]]
  (let [cache-key 0  ;[:view-keypaths client-name (get @db/leaf-brute-force-last-panel-hash client-name)]
        cached-result nil] ;(get @keypaths-cache cache-key)]
    (if (ut/ne? cached-result)
      ;;cached-result
      (do ;(ut/pp ["💰" :cache-hit :view-keypaths client-name tab])
          cached-result)
      (let [cpanels (get @db/client-panels client-name)
            result (vec (sort (conj
                               (filter #(not (cstr/includes? (str (first %)) "preview"))
                                       (distinct (map #(vec (take 3 %))
                                                      (filter (fn [x] (some #(= % (second x)) current-runners))
                                                              (ut/keypaths
                                                               (if (not (nil? tab))
                                                                 (let [all cpanels
                                                                       tabbers (vec (for [[k v] all
                                                                                          :when (= (get v :tab) tab)] k))]
                                                                   (visible-views (select-keys all tabbers)))
                                                                 (visible-views cpanels)))))))
                               [:canvas :canvas :canvas])))]
        ;(ut/pp ["💨" :cache-miss :view-keypaths client-name tab])
        ;(swap! keypaths-cache assoc cache-key result)
        result))))

(defn field-keypaths [client-name & [tab]]
  (let [cache-key [:field-keypaths client-name tab (hash-client-state client-name)]
        cached-result (get @keypaths-cache cache-key)]
    (if (ut/ne? cached-result)
      cached-result
      ;;(do (ut/pp ["💰" :cache-hit :field-keypaths client-name tab]) cached-result)
      (let [all-kp (view-keypaths client-name tab)
            all-qs (filterv #(= (second %) :queries) all-kp)
            result (vec
                    (apply concat
                           (for [kp all-qs
                                 :let [[panel-key runner data-key] kp
                                       metadata (get-in @db/client-panels-metadata [client-name panel-key runner data-key]
                                                        (get-in @db/query-metadata [client-name data-key]))
                                       fields (get metadata :fields [])]]
                             (for [f (keys fields)] (vec (into kp [:field f]))))))]
        ;;(ut/pp ["💨" :cache-miss :field-keypaths client-name tab])
        (swap! keypaths-cache assoc cache-key result)
        result))))

;; Function to clear the keypaths cache when needed
(defn clear-keypaths-cache! []
  (reset! keypaths-cache {}))

;; (ut/pp (get @db/client-panels :unreal-pear-wren-40))
;; (ut/pp (field-keypaths :unreal-pear-wren-40 "delightful echidna"))

;; only alive clients, and only currently viewed tab...
;; (ut/pp @db/ack-scoreboard)
;; (ut/pp (mapv (fn [[cid x]] [:client {:acks (:ack x) :last-seen (:last-seen-seconds-ago x) :mem (:memory x) :client-subs (:client-subs x)} cid ]) @db/ack-scoreboard))
;; (ut/pp (get @db/client-panels :calm-plum-peafowl-31) )
;; (ut/pp (get @db/client-panels :kind-orange-hedgehog-25) )

;; (ut/pp (view-keypaths :unreal-pear-wren-40 "delightful echidna"))

;; (ut/pp (view-keypaths :calm-plum-peafowl-31))

(defn active-clients []
  (vec (for [[k v] @db/ack-scoreboard
             ;:let [lss (get v :last-seen-seconds-ago -1)]
             ;:when (and (>= lss 0) (< lss 1800))
             ] ;; >30 mins ack is very very dead
         [k (get v :selected-tab)])))

;; (ut/pp (active-clients))

;; (ut/pp (get @evl/nrepl-output-atom :unreal-pear-wren-40))
;; (ut/pp (get @db/leaf-atom :truthful-cerulean-eagle-40))

(def context-map-cache (atom {}))

;; (ut/pp [:ff @db/drag-body-map])
;; (context-map :spirited-triangular-serval-31 [:block-10470 :queries :grouped-all-superstore-2 :field :city] [:canvas :canvas :canvas])

(defn get-parent-table [runner body client-name] ;; also gets tables out of views
  (cond
    (= runner :queries)
    (try
      (let [ww (first (flatten (get body :from)))]
        (if (cstr/starts-with? (str ww) ":query/")
          (keyword (cstr/replace (str ww) ":query/" ""))
          ww))
      (catch Exception _ nil))

    (= runner :views)
    (let [query-keys (vec (apply concat (for [[_ v] (get @db/client-panels client-name)
                                              :when (get v :queries)]  (keys (get v :queries)))))
          query-keys (into query-keys
                           (vec (for [q query-keys] (keyword (str "query/" (cstr/replace (str q) ":" ""))))))
          matches (cset/intersection (set query-keys) (set (ut/deep-flatten body)))]
                              ;;  (ut/pp [:vega panel-key data-key matches query-keys])
      (when (ut/ne? matches) (-> (first matches) (cstr/replace ":" "") (cstr/replace "query/" "") keyword)))

    (= runner :pivot) (get-in body [:rowset-keypath 0]) ;; source of the pivot, not the middle flat table
    :else nil))

(defn context-map [client-name leaf-kp & [selected-kp fake-atom dbody]]
  (let [cache-key [(hash [client-name leaf-kp selected-kp fake-atom dbody]) (get @db/leaf-brute-force-last-panel-hash client-name)]
        cached-result (get @context-map-cache cache-key)]
    (if (ut/ne? cached-result)
      cached-result
      ;;(do (ut/pp ["💰" :cache-hit :context-map client-name leaf-kp selected-kp]) cached-result)

      (let [;;;_ (ut/pp  [:fake-atom (keys fake-atom)])
            [panel-key runner data-key _ field-name] leaf-kp
            leaf-kp [panel-key runner data-key]
            canvas-drop? (true? (or (= leaf-kp [:canvas :canvas :canvas]) (empty? leaf-kp)))
            settings (dissoc @db/latest-settings-map :runners :modes)
            tab-name (get-in @db/client-panels [client-name panel-key :tab])
            outputs (into {} (for [[[_ kp] v] (get @evl/nrepl-output-atom client-name)] {kp v}))
            block-body (get-in @db/client-panels [client-name panel-key])
            generated? (not (nil? (get block-body :shape-rotator)))
            body  (get-in @db/client-panels [client-name panel-key runner data-key])
            parent-table (get-parent-table runner body client-name)
            grandparent-table (last (first
                                     (filter #(= parent-table (second %))
                                             (apply concat (for [[kk v] (get @db/client-panels client-name)
                                                                 :when (get v :queries)]
                                                             (for [[k vv] (get v :queries)]
                                                               [kk k (try
                                                                       (let [ww (first (flatten (get vv :from)))]
                                                                         (if (cstr/starts-with? (str ww) ":query/")
                                                                           (keyword (cstr/replace (str ww) ":query/" ""))
                                                                           ww))
                                                                       (catch Exception _ nil))]))))))
            ;; _ (ut/pp [:grandparent-table grandparent-table])
            dragging-kp (or selected-kp (get @db/leaf-drags-atom client-name))
            dragging? (true? (or (= leaf-kp dragging-kp) ; are WE dragging
                                 (= [panel-key runner data-key :field field-name] dragging-kp)))
            no-drag? (or (= dragging-kp [:canvas :canvas :canvas]) (empty? dragging-kp))
            fresh? (and no-drag? canvas-drop?)
            ;something-dragging? (ut/ne? dragging-kp)
            ;; _ (ut/pp [:ttt dragging-kp leaf-kp])
            query-metadata (get-in @db/client-panels-metadata [client-name panel-key runner data-key]
                                   (get-in @db/query-metadata [client-name data-key]))
            parent-query-metadata (get-in @db/query-metadata [client-name parent-table])
            grandparent-query-metadata (get-in @db/query-metadata [client-name grandparent-table])
            connection-id (get-in @db/client-panels (conj (into [client-name] leaf-kp) :connection-id)
                                  (get-in @db/client-panels (into [client-name] (conj [(first leaf-kp)] :connection-id))))
            ;; db-type (rota/db-type-from-connection-id connection-id) ;; use get :database/* info instead
            ;; dragging-body (get-in @db/params-atom [client-name :dragging-body])
            ;; dragging-type (get-in @db/params-atom [client-name :dragging-body :drag-meta :type])

            dragging-body (or dbody (get-in @db/drag-body-map [client-name dragging-kp]))
            dragging-type (get-in dragging-body [:drag-meta :type])

            hh (get-in @db/client-panels [client-name panel-key :h] 0)
            ww (get-in @db/client-panels [client-name panel-key :w] 0)
            drag-meta (get dragging-body :drag-meta)
            drag-meta (assoc drag-meta :pivot? (get drag-meta :pivot? false)) ;; so we always resolve this key
            cell-drag? (try (>= (get drag-meta :row-num) 0) (catch Exception _ false))
            drag-meta (if cell-drag? (assoc drag-meta :type :cell) drag-meta)
            result (merge
                    ;; generic drag-meta
                    (into {} (for [[k v] drag-meta]
                               {(keyword (str "drag-meta/" (cstr/replace (str k) ":" "")))
                                (if (keyword? v) (str v) v)})) ;; since these are clover sql params we can't have keyword values


                    ;; this is all the field level leaves for the field in question - not the dragged field, but an individual
                    ;; (when field-name ;; get easy to access field meta with :field-*
                    ;;   (merge
                    ;;    (into {} (for [[k v]
                    ;;                   ;; (if fake-atom
                    ;;                   ;;   (get-in fake-atom [:fields :by-keypath [panel-key runner data-key :field field-name]])
                    ;;                   ;;   (get-in @db/leaf-atom [client-name :fields :by-keypath [panel-key runner data-key :field field-name]]))
                    ;;                   (get-in @db/leaf-field-meta-map [client-name connection-id data-key field-name])
                    ;;                   ;; ^^ get data from field-attributes calls done earlier in query/table assessment
                    ;;                   ]
                    ;;               {(keyword (str "field/" (cstr/replace (str k) ":" ""))) v}))
                    ;;    {:field (get-in @db/leaf-field-meta-map [client-name connection-id data-key field-name])
                    ;;     ;; (if fake-atom
                    ;;     ;;   (get-in fake-atom [:fields :by-keypath [panel-key runner data-key :field field-name]])
                    ;;     ;;   (get-in @db/leaf-atom [client-name :fields :by-keypath [panel-key runner data-key :field field-name]]))
                    ;;     }))

                    (when (= dragging-type :param)
                      (let [param-map (into {}
                                            (for [[k v] drag-meta ;;(get dragging-body :drag-meta)
                                                  :let [kk (-> k str (cstr/replace "param-" "")
                                                               (cstr/replace ":" ""))]]
                                              {kk v}))
                            param-nm-map (into {} (for [[k v] param-map] {(keyword (str "drag-param/" k)) v}))
                            param-nested-map {:drag-param (into {} (for [[k v] param-map] {(keyword k) v}))}]
                        (merge param-nm-map param-nested-map)))

                    ;; this is for the dragged field, get all of ITS field level leaves accessible
                    ;(when something-dragging? ;; get easy drag access with :drag-field- or :drag-view-
                    (if (= (count dragging-kp) 5) ;; field

                        ;; we need fake namespaced keywords as well as proper maps for both fn and sql-logic usages
                      (merge
                       (into {} (for [[k v]
                                      (if fake-atom
                                        (get-in fake-atom [:fields :by-keypath dragging-kp])
                                        (get-in @db/leaf-atom [client-name :fields :by-keypath dragging-kp]))]
                                  {(keyword (str "drag-field/" (cstr/replace (str k) ":" ""))) v}))
                       {:drag-field (merge {:name (last dragging-kp)
                                            :connection-id (get-in @db/client-panels (conj (vec (into [client-name] (take 3 dragging-kp))) :connection-id)
                                                                   (get-in @db/client-panels (into [client-name] (conj (vec [(first dragging-kp)]) :connection-id))))}
                                           (get-in
                                            (get-in @db/client-panels-metadata (into [client-name] dragging-kp)
                                                    (get-in @db/query-metadata [client-name (get dragging-kp 2)])) [:fields (last dragging-kp)] [])
                                           (if fake-atom
                                             (get-in fake-atom [:fields :by-keypath dragging-kp])
                                             (get-in @db/leaf-atom [client-name :fields :by-keypath dragging-kp])))})

                      (merge
                       (into {} (for [[k v]
                                      (if fake-atom
                                        (get-in fake-atom [:views :by-keypath dragging-kp])
                                        (get-in @db/leaf-atom [client-name :views :by-keypath dragging-kp]))]
                                  {(keyword (str "drag-view/" (cstr/replace (str k) ":" ""))) v}))
                       {:drag-view
                        (if fake-atom
                          (get-in fake-atom [:views :by-keypath dragging-kp])
                          (get-in @db/leaf-atom [client-name :views :by-keypath dragging-kp]))}))
                      ;)

                    ;; some extra metadata for the dragged field
                    ;(when (= (count dragging-kp) 5) ;; field drag
                    ;; we want to keep them in even if null / n/a in this context since they are logical false
                    (merge
                     (into {} (for [[k v] (get-in
                                           (get-in @db/client-panels-metadata (into [client-name] dragging-kp)
                                                   (get-in @db/query-metadata [client-name (get dragging-kp 2)])) [:fields (last dragging-kp)] [])]
                                {(keyword (str "drag-field/" (cstr/replace (str k) ":" ""))) v}))
                     {:drag-field/connection-id (get-in @db/client-panels (conj (vec (into [client-name] (take 3 dragging-kp))) :connection-id)
                                                        (get-in @db/client-panels (into [client-name] (conj (vec [(first dragging-kp)]) :connection-id))))
                      :drag-field/name (last dragging-kp)})
                     ; )

                    ;; database metadata on the query level :database-name = "SQLite" etc
                    (when (or field-name (= runner :queries))
                      (get-connection-meta connection-id)) ;; get :database/* info

                    ;; if this is a field call, here is some unpacked metadata for that field
                    (when field-name (let [field-map {:name field-name
                                                      ;:*is-dragging? dragging?
                                                      :connection-id connection-id
                                                      :avg (get-in query-metadata [:fields field-name :avg])
                                                      :cardinality (get-in query-metadata [:fields field-name :cardinality])
                                                      :top-values (vec (keys (get-in query-metadata [:fields field-name :commons])))
                                                      :data-type (get-in query-metadata [:fields field-name :data-type])
                                                      :distinct-values (get-in query-metadata [:fields field-name :distinct])
                                                      :is-group-by? (get-in query-metadata [:fields field-name :group-by?])
                                                      :max (get-in query-metadata [:fields field-name :max])
                                                      :median (get-in query-metadata [:fields field-name :median])
                                                      :min (get-in query-metadata [:fields field-name :min])}
                                           fake-ns-map (into {} (for [[k v] field-map]
                                                                  {(keyword (str "field/" (cstr/replace (str k) ":" ""))) v}))]
                                       (merge fake-ns-map {:field field-map
                                                           :is-dragging? dragging?})))

                    ;; here are more basic view/query based lookups that apply to all drags/leaf lookups
                    {:body body
                     :tt {:rr {:zz 45}} ;; [:= :tt>rr>zz 45] test for clover kp get-in style
                     :dragging-body dragging-body
                     :dragging-type dragging-type
                     :drag-meta drag-meta
                     :cell-drag? cell-drag?
                     ;:dragging/body dragging-body
                     :dragging/type (str dragging-type) ;; stringified since it can only be used in clover-sql and we can't use keywords as values there
                     :drag-meta/type (str dragging-type) ;; stringified since it can only be used in clover-sql and we can't use keywords as values there
                     :drag-from-self? (true? (= (first dragging-kp) (first leaf-kp)))
                     :canvas-drop? canvas-drop?
                     :no-drag? no-drag?
                     :is-dragging? dragging?
                     :fresh? fresh?
                     :output (get outputs leaf-kp)
                     :clover-kws (filterv #(and (keyword? %) (cstr/includes? (str %) "/")) (ut/deep-flatten body))
                     :connection-id connection-id
                     :metadata query-metadata
                     :generated? (true? generated?)
                     :grandparent-table grandparent-table
                     :grandparent-table-str (str grandparent-table)
                     :grandparent-metadata grandparent-query-metadata
                     :parent-metadata parent-query-metadata
                     :parent-table parent-table
                     :parent-table-str (str parent-table)
                     :height-int (* hh 50)
                     :width-int (* ww 50)
                     :height hh
                     :width ww
                     :settings settings
                     :panel-key panel-key
                     :block-key panel-key
                     :data-key data-key
                     :query-key data-key
                     :view-key data-key
                     :tab tab-name
                     :runner runner
                     :runner-str (str runner)
                     :ui-keypath [:panels panel-key runner data-key]})]
        ;; (ut/pp ["💨" :pp client-name leaf-kp selected-kp settings parent-table])
        ;(ut/pp ["💨" :cache-miss :context-map client-name leaf-kp selected-kp])
        (swap! context-map-cache assoc cache-key result)
        ;; (ut/pp {:context-map (dissoc result :settings)})
        result))))

;; Function to clear the context-map cache when needed
(defn clear-context-map-cache! []
  (reset! context-map-cache {}))

;; (clear-context-map-cache!)

(def leaf-fn-cache (atom {}))

;; (ut/pp (count (keys @leaf-fn-cache)))

(declare leaf-eval-runstream)

(defn leaf-fn-eval [leaf-fn leaf-name client-name leaf-kp & [selected-kp fake-atom dbody]]
  (let [cache-key 0 ;;[(get @db/leaf-brute-force-last-panel-hash client-name) (hash fake-atom) client-name leaf-kp selected-kp dbody] ;0 ;(hash [(context-map client-name leaf-kp selected-kp) selected-kp leaf-fn leaf-name leaf-kp])
        cached-result nil] ;(get @leaf-fn-cache cache-key)]
    (if (ut/ne? cached-result)
      cached-result
      ;;(do (ut/pp ["💰" :cache-hit :leaf-fn-eval client-name leaf-kp leaf-name]) cached-result)
      (try
        (let [clover-kw  (filterv #(and (keyword? %) (cstr/includes? (str %) "/")) (ut/deep-flatten leaf-fn))
              clover-lookup-map (into {} (for [kw clover-kw] {kw (db/clover-lookup client-name kw)}))
              leaf-fn (walk/postwalk-replace clover-lookup-map leaf-fn)
              repl-str   (str "(let [deep-flatten rvbbit-backend.util/deep-flatten
                                     pp rvbbit-backend.util/pp
                                     flatten-to-keywords rvbbit-backend.util/deep-flatten
                               item-context-map (rvbbit-backend.leaves/context-map " client-name leaf-kp selected-kp fake-atom dbody")] (" leaf-fn  " item-context-map))")
              ;;_          (ut/pp [:repl-str repl-str])
              res        (evl/repl-eval repl-str "localhost" 8181 :rvbbit [] [])
              outs (vec (remove empty? (get-in res [:evald-result :out])))
              _ (when    (ut/ne? outs) (ut/pp ["🥩" {:raw-repl-output [leaf-name leaf-kp] :says outs}]))
              vall       (get-in res [:evald-result :value])
              err        (get-in res [:evald-result :err])
              ;; pretty-err (try (str leaf-name ":fn-line:" (last (cstr/split (cstr/join " " outs) "(REPL"))) (catch Exception e (str (str e) "::" err)))
              result     (if err
                           (do
                            ;;  (when-let [f (requiring-resolve 'rvbbit-backend.websockets/notify-general)]
                            ;;    (f client-name (str "leaf action failed: " (str pretty-err))))
                             (ut/pp [:leaf-fn-error outs]) {}) vall)]
          ;(ut/pp [:leaf-fn-cache-miss-LIVE leaf-name client-name leaf-kp])
          ;(ut/pp ["💨" :cache-miss :leaf-fn-eval client-name leaf-kp leaf-name])
          (swap! leaf-fn-cache assoc cache-key result)
          result)
        (catch Exception e
          (let [error-result {:error (str "leaf-fn-failed: " e)}]
            (swap! leaf-fn-cache assoc cache-key error-result)
            error-result))))))

(def fn-context-map-cache (atom {}))

(defn fn-context-map [client-name src-keypath target-keypath & [dbody]] ;; used by repl call above
  (let [cache-key 0 ;(hash [client-name src-keypath target-keypath (hash-client-state client-name)])
        cached-result nil] ;(get @fn-context-map-cache cache-key)]
    (if (ut/ne? cached-result)
      cached-result
      ;;(do (ut/pp ["💰" :cache-hit :fn-context-map client-name src-keypath target-keypath]) cached-result)
      (let [src-keypath (if (cstr/starts-with? (str (get src-keypath 2)) ":query/")
                          (edn/read-string (cstr/replace (str src-keypath) ":query/" ":")) src-keypath)
            settings (dissoc @db/latest-settings-map :runners :modes)
            base-src-view-kp (vec (take 3 src-keypath))
            base-src-block-kp (vec (take 1 src-keypath))
            src-runner (get base-src-view-kp 1)
            base-target-view-kp (vec (take 3 target-keypath))
            target-runner (get base-target-view-kp 1)
            base-target-block-kp (vec (take 1 target-keypath))
            source-view-body (get-in @db/client-panels (into [client-name] base-src-view-kp))
            target-view-body (get-in @db/client-panels (into [client-name] base-target-view-kp))
            source-block (get-in @db/client-panels (into [client-name] base-src-block-kp))
            target-block (get-in @db/client-panels (into [client-name] base-target-block-kp))
            source-connection-id (get source-view-body :connection-id (get source-block :connection-id))
            target-connection-id (get target-view-body :connection-id (get target-block :connection-id))
            source-db-type (rota/db-type-from-connection-id source-connection-id)
            target-db-type (rota/db-type-from-connection-id target-connection-id)
            dragging-kp   (get @db/leaf-drags-atom client-name)
            dragging-body (or dbody (get-in @db/drag-body-map [client-name dragging-kp]))
            parent-table (get-parent-table target-runner target-view-body client-name)
            ;dragging-type (get-in dragging-body [:drag-meta :type])
            ;dragging-body (get-in @db/params-atom [client-name :dragging-body])
            ;; parent-query-metadata (get-in @db/query-metadata [client-name (get src-keypath 2)])
            parent-query-metadata (get-in @db/client-panels-metadata (into [client-name] base-src-view-kp)
                                          (get-in @db/query-metadata [client-name (get src-keypath 2)]))
            ;;_ (ut/pp [:parent-query-metadata parent-query-metadata (into [client-name] base-src-view-kp) [client-name (get src-keypath 2)]])
            drag-meta (get dragging-body :drag-meta)
            result {:source-kp (into [:panels] base-src-view-kp)
                    :source-block-kp (into [:panels] base-src-block-kp)
                    :source-block source-block
                    :source-view source-view-body
                    :source-query source-view-body
                    :source-view-body source-view-body
                    :source-query-body source-view-body
                    :source-runner src-runner
                    :target-runner target-runner
                    :source-connection-id source-connection-id
                    :target-connection-id target-connection-id
                    :source-db-type source-db-type
                    :target-db-type target-db-type
                    :dragging-body dragging-body
                    :drag-body dragging-body
                    :parent-table parent-table
                    :parent-table-str (str parent-table)
                    :settings settings
                    :parent-query-metadata parent-query-metadata
                    :drag-meta drag-meta
                    :target-kp (into [:panels] base-target-view-kp)
                    :target-block-kp (into [:panels] base-target-block-kp)
                    :target-block target-block
                    :target-view target-view-body
                    :target-query target-view-body
                    :source-item (last src-keypath)
                    :source-field (when (= (get src-keypath 3) :field) (last src-keypath))}]
        (swap! fn-context-map-cache assoc cache-key result)
        ;;(ut/pp ["💨" :cache-miss :fn-context-map client-name src-keypath target-keypath])
        result))))

(defn clover-error-wrapper [message-header1 message-header2 message-body & [code]]
  {;:h nil :w 6.5
   :selected-view :error
   :views {:error [:v-box
                   :padding "5px"
                   :gap "6px"
                   ;:size "auto"
                   :align :center :justify :center
                   :style {:font-size "17px"}
                   :children [[:box :child (str message-header1)]
                              [:box :child (str message-header2)]
                              [:box
                               :style {:font-size "13px"}
                               :child (str message-body)]
                              [:box :child [:edn code]]]]}})

;; Function to clear the cache when needed
(defn clear-fn-context-map-cache! []
  (reset! fn-context-map-cache {}))

(defn leaf-action-fn-eval [client-name src-keypath target-keypath leaf-action-cat leaf-action leaf-fn & [dbody]]
  (let [cache-key 0 ;(hash [src-keypath target-keypath leaf-action-cat leaf-action (hash-client-state client-name)])
        cached-result nil] ;(get @leaf-fn-cache cache-key)]
    (if (ut/ne? cached-result)
      cached-result
      ;;(do (ut/pp ["💰" :cache-hit :leaf-action-fn-eval client-name src-keypath target-keypath leaf-action-cat leaf-action]) cached-result)
      (try
        (let [;clover-kw  (filterv #(and (keyword? %) (cstr/includes? (str %) "/")) (ut/deep-flatten leaf-fn))
              ;clover-lookup-map (into {} (for [kw clover-kw] {kw (db/clover-lookup client-name kw)}))
              ;leaf-fn (walk/postwalk-replace clover-lookup-map leaf-fn)
              repl-str   (str "(let [deep-flatten rvbbit-backend.util/deep-flatten
                                     pp rvbbit-backend.util/pp
                                     flatten-to-keywords rvbbit-backend.util/deep-flatten
                                     fn-context-map (rvbbit-backend.leaves/fn-context-map " client-name src-keypath target-keypath dbody ")]
                               (" leaf-fn  " fn-context-map))")
              ;;_          (ut/pp [:repl-str repl-str])
              res        (evl/repl-eval repl-str "localhost" 8181 :rvbbit [] [])
              outs (vec (get-in res [:evald-result :out]))
              _ (when  (ut/ne? outs) (ut/pp ["🥩" {:raw-repl-output [client-name src-keypath target-keypath leaf-action-cat leaf-action] :says outs}]))
              vall       (get-in res [:evald-result :value])
              err        (get-in res [:evald-result :err])
              ;pretty-err (try (str leaf-action-cat "/" (cstr/replace (str leaf-action) ":" "") " " (last outs)) (catch Exception e (str (str e) "::" err)))
              ;;_ (ut/pp [:REPL-outs client-name src-keypath target-keypath outs])
              _ (when (ut/ne? (remove empty? outs))
                  (try
                    (when-let [f (requiring-resolve 'rvbbit-backend.websockets/alert!)]
                      (f client-name [:v-box
                                      :style {:font-size "15px"}
                                      :children [[:box
                                                  :style {:font-size "23px"}
                                                  :child (str leaf-action-cat "/" (cstr/replace (str leaf-action) ":" "") " says...")]
                                                 [:v-box
                                                  :style {:font-family :theme/monospaced-font
                                                          :font-size "10px"}
                                                  :children (for [o outs] [:box :child (str o)])]]]  12 (+ (* (count outs) 1) 4) 10))
                    (catch Exception e (ut/pp [:leaf-action-fn-notify-failed (str e)]))))
              result     (if err (do
                                   (try
                                     (when-let [f (requiring-resolve 'rvbbit-backend.websockets/alert!)]
                                       (f client-name [:v-box
                                                       :style {:font-size "15px"}
                                                       :children [[:box
                                                                   :style {:font-size "23px"}
                                                                   :child (str leaf-action-cat "/" (cstr/replace (str leaf-action) ":" "") " says...")]
                                                                  [:box :child (last outs)]
                                                                  ;; [:box :child (str "(from leaf fn line " (cstr/replace
                                                                  ;;                                          (last (cstr/split (str (first outs)) #"REPL:")) ")." "") " in leaves.edn)")]
                                                                  ]]  12 4 10))
                                     (catch Exception e (ut/pp [:leaf-action-fn-notify-failed (str e)])))
                                   (ut/pp [:leaf-action-fn-error outs])
                                   [(clover-error-wrapper "leaf-action-fn-eval-error!" (str leaf-action-cat " " leaf-action) err leaf-fn)]) vall)]
          ;(ut/pp ["💨" :cache-miss :leaf-action-fn-eval client-name src-keypath target-keypath leaf-action-cat leaf-action])
          (swap! leaf-fn-cache assoc cache-key (first result))
          (first result))
        (catch Exception e
          (let [error-result {:error (str "leaf-action-fn-failed: " e)
                              :args [client-name src-keypath target-keypath leaf-action-cat leaf-action]}]
            ;(swap! leaf-fn-cache assoc cache-key error-result)
            (ut/pp error-result)) {})))))

;; (time (do (leaf-eval-runstream :fields :grin-hexagonal-elephant-11) (leaf-eval-runstream :views :grin-hexagonal-elephant-11)))

;; (ut/pp (view-keypaths :grin-hexagonal-elephant-11))
;; (ut/pp (field-keypaths :grin-hexagonal-elephant-11))

;; (ut/pp (leaf-fn-eval '(fn [{:keys [panel-key view-key tab]}] (str panel-key view-key tab 123 "and" :time/hour :signal/daily?)) :leaf-123 :unreal-pear-wren-40 [:block-8742 :clojure :new-clojure-1]))

(def leaf-fns-cache2 (atom nil))
(def leaf-fns-cache (atom nil))

(defn clear-leaf-fns-cache! []
  (reset! leaf-fns-cache nil))

(defn load-leaf-meta []
  (if (ut/ne? @leaf-fns-cache)
    @leaf-fns-cache
    (let [file-content (slurp "defs/leaves.edn")
          leaf-fns (edn/read-string file-content)]
      (reset! leaf-fns-cache leaf-fns)
      leaf-fns)))

;; (ut/pp (load-leaf-meta))

(defn load-leaf-fns [client-name view-kp dragging-kp]
  (let [cache-key [client-name view-kp dragging-kp (get @db/leaf-brute-force-last-panel-hash client-name)]]
    (if-let [cached-result (get @leaf-fns-cache2 cache-key)]
      cached-result
      (try
        (let [leaf-fns (load-leaf-meta)
              {:keys [views fields actions-fns action-labels]} leaf-fns
              single-layer-expand (fn [vv] (into {} (for [[k v] vv]
                                                      (if (cstr/includes? (str k) ".")
                                                        (let [spl (cstr/split (cstr/replace (str k) ":" "") #"\.")
                                                              base (first spl)
                                                              vvals (rest spl)
                                                              vvals (if (= (count vvals) 1)
                                                                      (let [leaf-fn (get views (keyword (str (first vvals) "*")))
                                                                            leaf-name k
                                                                            res (if leaf-fn
                                                                                  (first (leaf-fn-eval leaf-fn leaf-name client-name view-kp dragging-kp))
                                                                                  vvals)]
                                                                        res)
                                                                      vvals)]
                                                          (into {}
                                                                (for [arg vvals]
                                                                  {(if (keyword? arg)
                                                                     (keyword (cstr/replace (str base "." arg) ":" ""))
                                                                     (keyword (str base "." arg)))
                                                                   (walk/postwalk-replace {:*arg (keyword arg)} v)})))
                                                        {k v}))))
              double-layer-expand (fn [xx] (into {} (for [[k v] xx] {k (single-layer-expand v)})))
              views (single-layer-expand views)
              fields (single-layer-expand fields)
              actions-fns (double-layer-expand actions-fns)
              action-labels (double-layer-expand action-labels)
              leaf-fns (-> leaf-fns
                           (assoc :views views)
                           (assoc :actions-fns actions-fns)
                           (assoc :action-labels action-labels)
                           (assoc :fields fields))]
          (swap! leaf-fns-cache2 assoc cache-key leaf-fns)
          leaf-fns)
        (catch Exception e
          (ut/pp ["Error reading or parsing leaves.edn file:" (.getMessage e)])
          (throw (ex-info "Failed to load leaf-fns" {:cause e})))))))


;; (defn load-leaf-fns [client-name view-kp dragging-kp]
;;   (try
;;     (let [;;file-content (slurp "defs/leaves.edn")
;;           leaf-fns (load-leaf-meta) ;; (edn/read-string file-content)
;;           {:keys [views fields actions-fns action-labels]} leaf-fns
;;           single-layer-expand (fn [vv] (into {} (for [[k v] vv]
;;                                                   (if (cstr/includes? (str k) ".")
;;                                                     (let [spl (cstr/split (cstr/replace (str k) ":" "") #"\.")
;;                                                           base (first spl)
;;                                                           vvals (rest spl)
;;                                                           vvals (if (= (count vvals) 1)
;;                                                                   (let [leaf-fn (get views (keyword (str (first vvals) "*")))
;;                                                                         leaf-name k
;;                                                                         res (if leaf-fn
;;                                                                               (first (leaf-fn-eval leaf-fn leaf-name client-name view-kp dragging-kp))
;;                                                                               vvals)]
;;                                                                     res) ;; dynamic args enumerated
;;                                                                   vvals)
;;                                                           ;; _ (ut/pp [:enumerating-args-for spl k (count vvals)])
;;                                                           ]
;;                                                       (into {}
;;                                                             (for [arg vvals]
;;                                                               {(if (keyword? arg)
;;                                                                  (keyword (cstr/replace (str base "." arg) ":" ""))
;;                                                                  (keyword (str base "." arg)))
;;                                                                (walk/postwalk-replace {:*arg (keyword arg)} v)})))
;;                                                     {k v}))))
;;           double-layer-expand (fn [xx] (into {} (for [[k v] xx] {k (single-layer-expand v)})))
;;           views (single-layer-expand views)
;;           fields (single-layer-expand fields)
;;           actions-fns (double-layer-expand actions-fns)
;;           action-labels (double-layer-expand action-labels)
;;           leaf-fns (-> leaf-fns
;;                        (assoc :views views)
;;                        (assoc :actions-fns actions-fns)
;;                        (assoc :action-labels action-labels)
;;                        (assoc :fields fields))]
;;           ;;(ut/pp [:leaf-views leaf-fns])
;;             ;(reset! leaf-fns-cache leaf-fns)
;;       leaf-fns)

;;     (catch Exception e
;;       (ut/pp ["Error reading or parsing leaves.edn file:" (.getMessage e)])
;;       (throw (ex-info "Failed to load leaf-fns" {:cause e})))))

;; (ut/ppp [:test :stull "yoyoy"])
;; (ut/pp (view-keypaths :unreal-pear-wren-40 "delightful echidna"))

(def memoized-sql-query
  (memoize
   (fn [db sql & [extra]]
     (sql-query db sql extra))))

;; (defn resolve-and-evaluate-leaf-qls [leaf-qls resy resy-grp view-kp max-iterations field-walk-map]
;;   (loop [iteration 0
;;          previous-results {}]
;;     (let [current-results
;;           (reduce
;;            (fn [acc [leaf-name leaf-ql]]
;;              (let [lql {:select [[1 :one]] :where leaf-ql}
;;                    lql-walks (merge (get-in @resy [:by-keypath view-kp]) field-walk-map)
;;                    lql-res (walk/postwalk-replace lql-walks lql)
;;                    ;;_ (ut/pp [:lql-walks leaf-name lql lql-walks lql-res])
;;                    sqlcall (memoized-sql-query ghost-db (to-sql lql-res))
;;                    leaf-val (true? (= sqlcall [{:one 1}]))]
;;                (assoc acc leaf-name leaf-val)))
;;            {}
;;            leaf-qls)]

;;       ;; Update resy and resy-grp with current results
;;       (doseq [[leaf-name leaf-val] current-results]
;;         (swap! resy assoc-in [:by-keypath view-kp leaf-name] leaf-val)
;;         (if (true? leaf-val)
;;           (swap! resy-grp update-in [:by-leaf leaf-name]
;;                  (fnil (fn [existing] (vec (distinct (conj existing view-kp)))) []))
;;           (swap! resy-grp update-in [:by-leaf leaf-name]
;;                  (fnil (fn [existing] (vec (remove #(= % view-kp) existing))) []))))

;;       (cond
;;         ;; stop if we've reached the maximum number of iterations
;;         (>= iteration max-iterations)
;;         (do ;(ut/pp [:stopped-at-iteration view-kp iteration])
;;           [resy resy-grp])

;;         ;; stop if results haven't changed (we've reached a fixed point)
;;         (= previous-results current-results)
;;         (do ;(ut/pp [:stopped-at-iteration view-kp iteration])
;;           [resy resy-grp])

;;         ;; orrrrr, continue with another iteration (these are cheap logic queries)
;;         :else
;;         (recur (inc iteration) current-results)))))

(def leaf-ql-cache (atom {}))

(defn clear-leaf-ql-cache! []
  (reset! leaf-ql-cache {}))

;; (clear-leaf-ql-cache!)

;; ut/parse-clover-keypath

(defn resolve-and-evaluate-leaf-qls [client-name leaf-qls resy resy-grp view-kp max-iterations field-walk-map]
  (let [cache-key 0 ;(hash [client-name leaf-qls @resy view-kp max-iterations field-walk-map])
        cached-result nil] ; (get @leaf-ql-cache cache-key)] ;; (get-in @resy [:by-keypath view-kp])
    (if (ut/ne? cached-result)
      cached-result
      ;;(do (ut/pp ["💰" :cache-hit :resolve-and-evaluate-leaf-qls client-name view-kp]) cached-result)
      (let [result
            (loop [iteration 0
                   previous-results {}]
              (let [current-results
                    (reduce
                     (fn [acc [leaf-name leaf-ql]]
                       (let [lql {:select [[1 :one]] :where leaf-ql}
                             clover-kp-kws (filterv #(and (not (cstr/starts-with? (str %)  ":>"))
                                                          (not (cstr/starts-with? (str %)  ":<>"))
                                                          (cstr/includes? (str %) ">"))
                                                    (ut/deep-flatten leaf-ql))
                             clover-kp-kws-walk (into {} (for [c clover-kp-kws] {c (get-in field-walk-map (ut/parse-clover-keypath c))}))
                             lql-walks (merge (get-in @resy [:by-keypath view-kp]) field-walk-map clover-kp-kws-walk)
                             lql-res (walk/postwalk-replace lql-walks lql)
                             ;;;_ (ut/pp  [:lql-walks lql-res leaf-ql clover-kp-kws-walk])
                             sqlcall (sql-query ghost-db (to-sql lql-res) {:lql-def lql :leaf-name leaf-name :client-name client-name :no-error? true})
                             leaf-val (true? (= sqlcall [{:one 1}]))]
                         (assoc acc leaf-name leaf-val)))
                     {}
                     leaf-qls)]

                ;; Update resy and resy-grp with current results
                (doseq [[leaf-name leaf-val] current-results]
                  (swap! resy assoc-in [:by-keypath view-kp leaf-name] leaf-val)
                  (if (true? leaf-val)
                    (swap! resy-grp update-in [:by-leaf leaf-name]
                           (fnil (fn [existing] (vec (distinct (conj existing view-kp)))) []))
                    (swap! resy-grp update-in [:by-leaf leaf-name]
                           (fnil (fn [existing] (vec (remove #(= % view-kp) existing))) []))))

                (cond
                  (>= iteration max-iterations) [resy resy-grp]
                  (= previous-results current-results) [resy resy-grp]
                  :else (recur (inc iteration) current-results))))]
        ;;(swap! leaf-ql-cache assoc cache-key result)
        ;;(ut/pp ["💨" :cache-miss :resolve-and-evaluate-leaf-qls client-name view-kp])
        result))))

;; (leaf-eval-runstream :views :truthful-cerulean-eagle-40)
;; (leaf-eval-runstream :fields :truthful-cerulean-eagle-40)
;; (leaf-eval-runstream :patches :truthful-cerulean-eagle-40)
;; (leaf-eval-runstream :fields :spirited-triangular-serval-31)

(def filtered-leaf-defs-cache (atom {}))

(defn get-filtered-leaf-defs [client-name view-kp dragging-kp]
  (let [leaf-defs (load-leaf-fns client-name view-kp dragging-kp)
        ;;cache-key (hash leaf-defs)
        ]
    (let [result
          {:leaf-fns-f (into {} (for [[k v] (get leaf-defs :fields)
                                      :when (try
                                              (not (keyword? (first v)))
                                              (catch Exception _ false))] {k v}))
           :leaf-qls-f (into {} (for [[k v] (get leaf-defs :fields)
                                      :when (try
                                              (keyword? (first v))
                                              (catch Exception _ false))] {k v}))
           :leaf-fns-v (into {} (for [[k v] (get leaf-defs :views)
                                      :when (try
                                              (not (keyword? (first v)))
                                              (catch Exception _ false))] {k v}))
           :leaf-qls-v (into {} (for [[k v] (get leaf-defs :views)
                                      :when (try
                                              (keyword? (first v))
                                              (catch Exception _ false))] {k v}))
           :categories (get leaf-defs :categories)
           :action-labels (get leaf-defs :action-labels)}]
      ;;(swap! filtered-leaf-defs-cache assoc cache-key result)
      result)))

(defn clear-filtered-leaf-defs-cache! []
  (reset! filtered-leaf-defs-cache {}))

;; (ut/pp [:acc (active-clients)])

(defn leaf-eval-runstream3 [client-name selected-kp & [dbody dragged-field-map]]
  (let [accl (active-clients)
        res {} ;(or res {})
        ui-action-map-acc (atom {})
        dragging-kp (if (= (first selected-kp) :canvas)
                      [:canvas :canvas :canvas]
                      selected-kp)
        leaf-meta-maps (load-leaf-meta)
        active-clients-and-tab (filterv #(= (first %) client-name) accl)
        ;; {:keys [leaf-fns-f leaf-qls-f leaf-fns-v leaf-qls-v]} (get-filtered-leaf-defs client-name view-kp dragging-kp)

        ;; Process fields first (unchanged)
        fields-result
        (if (ut/ne? dragged-field-map)
          {:by-keypath {dragging-kp dragged-field-map}} ;; just recreate the shape from the shape-rotator meta
          (when (= (count dragging-kp) 5)
            (let [resy (atom {}) ;; otherwise calcuate from :fields leaf rules, but they are likely deprecated now...
                  resy-grp (atom {})
                  resy-meta (atom {})
                  work-paths (when (= (count dragging-kp) 5) [dragging-kp])] ;; only necessary field to be processed is the dragged field

              (doseq [view-kp work-paths]
                (let [{:keys [leaf-fns-f leaf-qls-f leaf-fns-v leaf-qls-v action-labels]} (get-filtered-leaf-defs client-name view-kp dragging-kp)
                      _ (swap! resy-meta assoc-in [:by-keypath-meta view-kp] action-labels)
                      leaf-fn-results
                      (doall
                       (pmap (fn [[leaf-name leaf-fn]]
                               (let [leaf-val (leaf-fn-eval leaf-fn leaf-name client-name view-kp dragging-kp res dbody)
                                     leaf-val (if (vector? leaf-val) (first leaf-val) leaf-val)]
                                 [leaf-name leaf-val view-kp]))
                             leaf-fns-f))]

                  (doseq [[leaf-name leaf-val view-kp] leaf-fn-results]
                    (swap! resy assoc-in [:by-keypath view-kp leaf-name] leaf-val)
                    (if (true? leaf-val)
                      (swap! resy-grp assoc-in [:by-leaf leaf-name]
                             (vec (conj (get-in @resy-grp [:by-leaf leaf-name] []) view-kp)))
                      (swap! resy-grp assoc-in [:by-leaf leaf-name]
                             (vec (get-in @resy-grp [:by-leaf leaf-name] []))))))

                (let [ctx-map (context-map client-name view-kp dragging-kp {:fields (merge @resy @resy-grp)} dbody)
                      {:keys [leaf-fns-f leaf-qls-f leaf-fns-v leaf-qls-v action-labels]} (get-filtered-leaf-defs client-name view-kp dragging-kp)]
                  (resolve-and-evaluate-leaf-qls client-name leaf-qls-f resy resy-grp view-kp 100 ctx-map)))

              (doseq [[k v] (get @resy-grp :by-leaf)
                      :when (cstr/includes? (str k) "/")
                      :let [[atype akey] (mapv keyword (cstr/split (cstr/replace (str k) ":" "") #"/"))]]
                (swap! ui-action-map-acc assoc-in [atype akey] v))

              (merge @resy @resy-grp @resy-meta))))
        ;;;_ (ut/pp [:fields-result! dragging-kp fields-result])

        ;; Modified views processing to ensure complete context
        intermediate-res (assoc res :fields fields-result)
        views-result
        (let [resy (atom {})
              resy-grp (atom {})
              resy-meta (atom {})
              work-paths (view-keypaths client-name (-> active-clients-and-tab first second))
              ;work-paths (view-keypaths client-name nil)
              _ (ut/pp [:leaf-runstream3 :view-work-paths client-name (count work-paths) selected-kp])
              ]

          ;; Process views sequentially to maintain context
          (doseq [view-kp work-paths]
            ;; First evaluate all leaf-fns
            (let [{:keys [leaf-fns-f leaf-qls-f leaf-fns-v leaf-qls-v action-labels]} (get-filtered-leaf-defs client-name view-kp dragging-kp)
                  _ (swap! resy-meta assoc-in [:by-keypath-meta view-kp] action-labels)
                  leaf-fn-results
                  (doall
                   (pmap (fn [[leaf-name leaf-fn]]
                           (let [leaf-val (leaf-fn-eval leaf-fn leaf-name client-name view-kp dragging-kp intermediate-res dbody)
                                 leaf-val (if (vector? leaf-val) (first leaf-val) leaf-val)]
                             [leaf-name leaf-val view-kp]))
                         leaf-fns-v))]

              (doseq [[leaf-name leaf-val view-kp] leaf-fn-results]
                (swap! resy assoc-in [:by-keypath view-kp leaf-name] leaf-val)
                (if (true? leaf-val)
                  (swap! resy-grp assoc-in [:by-leaf leaf-name]
                         (vec (conj (get-in @resy-grp [:by-leaf leaf-name] []) view-kp)))
                  (swap! resy-grp assoc-in [:by-leaf leaf-name]
                         (vec (get-in @resy-grp [:by-leaf leaf-name] []))))))

            ;; Then evaluate QLs with complete context
            (let [current-context (merge intermediate-res
                                         {:views (merge @resy @resy-grp)})
                  {:keys [leaf-fns-f leaf-qls-f leaf-fns-v leaf-qls-v action-labels]} (get-filtered-leaf-defs client-name view-kp dragging-kp)
                  ctx-map (context-map client-name view-kp dragging-kp current-context dbody)]
              (resolve-and-evaluate-leaf-qls client-name leaf-qls-v resy resy-grp view-kp 100 ctx-map)))

          (doseq [[k v] (get @resy-grp :by-leaf)
                  :when (cstr/includes? (str k) "/")
                  :let [[atype akey] (mapv keyword (cstr/split (cstr/replace (str k) ":" "") #"/"))]]
            (swap! ui-action-map-acc assoc-in [atype akey] v))

          (merge @resy @resy-grp @resy-meta))]

    (-> res
        (assoc-in [:metadata :categories] (get leaf-meta-maps :categories))
        (assoc-in [:metadata :action-labels] (get leaf-meta-maps :action-labels))
        (assoc-in [:metadata :by-keypath-meta] (get views-result :by-keypath-meta))
        (assoc-in [:debug] {:dragging-kp dragging-kp})
        ;(assoc-in [:fields] fields-result) ;; mostly decision making meta
        (assoc-in [:actions] (merge {:*dragging-kp dragging-kp} @ui-action-map-acc))
        ;(assoc-in [:views] views-result)   ;; mostly decision making meta - will be useful for debugging later with UI
        )))

;; (ut/pp (active-clients))

;; (ut/pp
;;  ;(ut/pretty-spit "/home/ryanr/new-res.edn"
;;                  (time
;;                  ;(ut/deep-sort
;;                   (select-keys (leaf-eval-runstream3 :yummy-gold-pigeon-2 [:block-3266 :queries :all-bigfoot-sightings :field :bfroid]) [:actions :debug])))
;;  ;);)

;; (ut/pp
;;  ;(ut/pretty-spit "/home/ryanr/old-res.edn"
;;                  (time
;;                  ;(ut/deep-sort
;;                   (select-keys (leaf-eval-runstream-parallel-cached :harmonious-elliptic-bear-30 [:block-10889 :queries :all-offenses :field :DISTRICT]) [:actions :debug])))
;;  ;)

;; (= (ut/deep-sort (select-keys (leaf-eval-runstream3 :smile-rectangular-turtle-38 [:block-10889 :queries :all-offenses :field :DISTRICT]) [:actions :debug]))
;;    (ut/deep-sort (select-keys (leaf-eval-runstream3 :smile-rectangular-turtle-38 [:block-10889 :queries :all-offenses :field :DISTRICT]) [:actions :debug])))


;; (defn leaf-eval-runstream2-parallel [leaf-type & [client-name selected-kp res]]
;;   (let [accl (active-clients)
;;         res (or res {})
;;         ui-action-map-acc (atom {})
;;         resy (atom {})
;;         resy-grp (atom {})
;;         dragging-kp selected-kp
;;         active-clients-and-tab (if client-name
;;                                  (filterv #(= (first %) client-name) accl)
;;                                  accl)
;;         leaf-defs (get (load-leaf-fns) leaf-type)
;;         leaf-fns (into {} (for [[k v] leaf-defs
;;                                 :when (try
;;                                         (not (keyword? (first v)))
;;                                         (catch Exception _ false))] {k v}))
;;         leaf-qls (into {} (for [[k v] leaf-defs
;;                                 :when (try
;;                                         (keyword? (first v))
;;                                         (catch Exception _ false))] {k v}))]

;;     ;; Process clients in parallel
;;     (doall
;;      (pmap
;;       (fn [[client-name tab-name]]
;;         (let [work-paths (if (or (= leaf-type :views)
;;                                  (= leaf-type :patches))
;;                            (view-keypaths client-name tab-name)
;;                            (field-keypaths client-name tab-name))]

;;           ;; Process paths in parallel
;;           (doall
;;            (pmap
;;             (fn [view-kp]
;;               ;; Process leaf-fns in parallel
;;               (doall
;;                (pmap
;;                 (fn [[leaf-name leaf-fn]]
;;                   (let [leaf-val (leaf-fn-eval leaf-fn leaf-name client-name view-kp dragging-kp res)
;;                         leaf-val (if (vector? leaf-val) (first leaf-val) leaf-val)]
;;                     (swap! resy assoc-in [:by-keypath view-kp leaf-name] leaf-val)
;;                     (if (true? leaf-val)
;;                       (swap! resy-grp assoc-in [:by-leaf leaf-name]
;;                              (vec (conj (get-in @resy-grp [:by-leaf leaf-name] []) view-kp)))
;;                       (swap! resy-grp assoc-in [:by-leaf leaf-name]
;;                              (vec (get-in @resy-grp [:by-leaf leaf-name] []))))))
;;                 leaf-fns))

;;               ;; Process leaf-qls
;;               (let [ctx-map (context-map client-name view-kp dragging-kp res)]
;;                 (resolve-and-evaluate-leaf-qls client-name leaf-qls resy resy-grp view-kp 100 ctx-map)))
;;             work-paths))))
;;       active-clients-and-tab))

;;     ;; Process action map updates in parallel
;;     (doall
;;      (pmap
;;       (fn [[k v]]
;;         (when (cstr/includes? (str k) "/")
;;           (let [[atype akey] (mapv keyword (cstr/split (cstr/replace (str k) ":" "") #"/"))]
;;             (swap! ui-action-map-acc assoc-in [atype akey] v))))
;;       (get @resy-grp :by-leaf)))

;;     ;; Return the final accumulated results
;;     (-> res
;;         (assoc-in [:metadata :categories] (get (load-leaf-fns) :categories))
;;         (assoc-in [:metadata :action-labels] (get (load-leaf-fns) :action-labels))
;;         (assoc-in [:actions] @ui-action-map-acc)
;;         (assoc-in [leaf-type] (merge @resy @resy-grp)))))




;; (time  (leaf-eval-runstream2 :views :rewarding-coffee-guinea-pig-13 [:block-7533 :queries :grouped-all-superstore-5]))
;; (time  (leaf-eval-runstream2-parallel :views :rewarding-coffee-guinea-pig-13 [:block-7533 :queries :grouped-all-superstore-5]))

;; (defn leaf-eval-runstream-parallel [leaf-type & [client-name selected-kp]]
;;   (swap! db/leaf-evals inc)
;;   (let [accl (active-clients)
;;         ui-action-map-acc (atom {})
;;         dragging-kp (or selected-kp (get @db/leaf-drags-atom client-name))
;;         leaf-defs (get (load-leaf-fns) leaf-type)

;;         active-clients-and-tab (if client-name (filterv #(= (first %) client-name) accl) accl)

;;         leaf-fns (into {} (for [[k v] leaf-defs
;;                                 :when (try (not (keyword? (first v)))
;;                                            (catch Exception _ false))] {k v}))
;;         leaf-qls (into {} (for [[k v] leaf-defs
;;                                 :when (try (keyword? (first v))
;;                                            (catch Exception _ false))] {k v}))
;;        ;;; _ (ut/pp [:leaf-eval-runstream-parallel-start leaf-type client-name dragging-kp leaf-defs leaf-fns active-clients-and-tab])
;;         ]
;;     (doall
;;      (pmap
;;       (fn [[client-name tab-name]]
;;         (let [resy (atom {})
;;               resy-grp (atom {})
;;               work-paths (if (= leaf-type :views)
;;                            (view-keypaths client-name tab-name)
;;                           ;;  (field-keypaths client-name tab-name)
;;                            (if (= (count dragging-kp) 5) [dragging-kp] [])
;;                            )]
;;           (doall
;;            (pmap
;;             (fn [view-kp]
;;               ;; Process leaf-fns
;;               (doall
;;                (pmap
;;                 (fn [[leaf-name leaf-fn]]
;;                   (let [leaf-val (leaf-fn-eval leaf-fn leaf-name client-name view-kp dragging-kp)
;;                         leaf-val (if (vector? leaf-val) (first leaf-val) leaf-val)]
;;                     (swap! resy assoc-in [:by-keypath view-kp leaf-name] leaf-val)
;;                     (swap! resy-grp update-in [:by-leaf leaf-name]
;;                            (fnil #(vec (if (true? leaf-val)
;;                                          (distinct (conj % view-kp))
;;                                          (remove #{view-kp} %)))
;;                                  []))))
;;                 leaf-fns))

;;               ;; Process leaf-qls
;;               (let [ctx-map (context-map client-name view-kp dragging-kp)]
;;                 (resolve-and-evaluate-leaf-qls client-name leaf-qls resy resy-grp view-kp 100 ctx-map)))
;;             work-paths))

;;           ;; Process ui-action-map-acc
;;           (doseq [[k v] (get @resy-grp :by-leaf)
;;                   :when (cstr/includes? (str k) "/")
;;                   :let [[atype akey] (mapv keyword (cstr/split (cstr/replace (str k) ":" "") #"/"))]]
;;             (swap! ui-action-map-acc assoc-in [atype akey] v))

;;           ;; Update db/leaf-atom
;;           ;; (ut/pp [:drag-data {:action-map @ui-action-map-acc
;;           ;;                     :meta-map (merge @resy @resy-grp)}])
;;           (swap! db/leaf-atom
;;                  (fn [current-state]
;;                    (-> current-state
;;                        (assoc-in [client-name :metadata :categories] (get (load-leaf-fns) :categories))
;;                        (assoc-in [client-name :metadata :action-labels] (get (load-leaf-fns) :action-labels))
;;                        (assoc-in [client-name :debug] {:dragging-kp dragging-kp})
;;                        (assoc-in [client-name :actions] @ui-action-map-acc) ;(if (empty? @ui-action-map-acc) {[:none :none] :none} @ui-action-map-acc))
;;                        (assoc-in [client-name leaf-type] (merge @resy @resy-grp)))))))
;;       active-clients-and-tab))
;;     ;; (ut/pp [:leaf-eval-runstream-parallel-end leaf-type client-name dragging-kp])
;;     {dragging-kp @ui-action-map-acc}))



(def leaf-eval-runstream-parallel-cache (atom {}))

;; (defn leaf-eval-runstream-parallel-cached [client-name & [selected-kp]]
;;   ;(swap! db/leaf-evals inc)
;;   (let [;dragging-kp (or selected-kp (get @db/leaf-drags-atom client-name [:canvas :canvas :canvas]))
;;         cache-key 0 ;[dragging-kp (hash-client-state client-name) (hash (load-leaf-fns))]
;;         cached nil] ;(get @leaf-eval-runstream-parallel-cache cache-key)]
;;     (if cached
;;       (do ;;(ut/pp ["☘️☘️☘️" :cache-hit cache-key])
;;         (swap! db/leaf-atom assoc client-name cached)
;;           cached)
;;       (do ;;(ut/pp ["🍁🍁🍁" :cache-miss cache-key])
;;           (leaf-eval-runstream-parallel :fields client-name)
;;           (leaf-eval-runstream-parallel :views client-name)
;;           (swap! leaf-eval-runstream-parallel-cache assoc cache-key
;;                  (get @db/leaf-atom client-name))
;;           (get @db/leaf-atom client-name)))))



;; (defn leaf-eval-runstream-parallel [leaf-type & [client-name selected-kp]]
;;   (let [dragging-kp (or selected-kp (get @db/leaf-drags-atom client-name))
;;         cache-key [leaf-type dragging-kp (hash-client-state client-name) (hash (get (load-leaf-fns) leaf-type))] ;;[leaf-type client-name selected-kp (hash @db/client-panels)]
;;         cached-result (get @leaf-eval-runstream-parallel-cache cache-key)]
;;     (if cached-result
;;       (do (swap! db/leaf-atom client-name (merge (get @db/leaf-atom client-name) cached-result))
;;           (ut/pp ["☘️☘️☘️" :cache-hit cache-key])
;;           cached-result)
;;       (let [_ (ut/pp ["🍁🍁🍁" :cache-miss cache-key])
;;             accl (active-clients)
;;             ui-action-map-acc (atom {})

;;             active-clients-and-tab (if client-name (filterv #(= (first %) client-name) accl) accl)
;;             leaf-defs (get (load-leaf-fns) leaf-type)
;;             leaf-fns (into {} (for [[k v] leaf-defs
;;                                     :when (try (not (keyword? (first v)))
;;                                                (catch Exception _ false))] {k v}))
;;             leaf-qls (into {} (for [[k v] leaf-defs
;;                                     :when (try (keyword? (first v))
;;                                                (catch Exception _ false))] {k v}))
;;             result (do
;;                      (doall
;;                       (pmap
;;                        (fn [[client-name tab-name]]
;;                          (let [resy (atom {})
;;                                resy-grp (atom {})
;;                                work-paths (if (or (= leaf-type :views) (= leaf-type :patches))
;;                                             (view-keypaths client-name tab-name)
;;                                             (field-keypaths client-name tab-name))]
;;                            (doall
;;                             (pmap
;;                              (fn [view-kp]
;;                               ;; Process leaf-fns
;;                                (doall
;;                                 (pmap
;;                                  (fn [[leaf-name leaf-fn]]
;;                                    (let [leaf-val (leaf-fn-eval leaf-fn leaf-name client-name view-kp dragging-kp)
;;                                          leaf-val (if (vector? leaf-val) (first leaf-val) leaf-val)]
;;                                      (swap! resy assoc-in [:by-keypath view-kp leaf-name] leaf-val)
;;                                      (swap! resy-grp update-in [:by-leaf leaf-name]
;;                                             (fnil #(vec (if (true? leaf-val)
;;                                                           (distinct (conj % view-kp))
;;                                                           (remove #{view-kp} %)))
;;                                                   []))))
;;                                  leaf-fns))

;;                               ;; Process leaf-qls
;;                                (let [ctx-map (context-map client-name view-kp dragging-kp)]
;;                                  (resolve-and-evaluate-leaf-qls client-name leaf-qls resy resy-grp view-kp 100 ctx-map)))
;;                              work-paths))

;;                           ;; Process ui-action-map-acc
;;                            (doseq [[k v] (get @resy-grp :by-leaf)
;;                                    :when (cstr/includes? (str k) "/")
;;                                    :let [[atype akey] (mapv keyword (cstr/split (cstr/replace (str k) ":" "") #"/"))]]
;;                              (swap! ui-action-map-acc assoc-in [atype akey] v))

;;                           ;; Update db/leaf-atom
;;                            (swap! db/leaf-atom
;;                                   (fn [current-state]
;;                                     (-> current-state
;;                                         (assoc-in [client-name :metadata :categories] (get (load-leaf-fns) :categories))
;;                                         (assoc-in [client-name :metadata :action-labels] (get (load-leaf-fns) :action-labels))
;;                                         (assoc-in [client-name :debug] {:dragging-kp dragging-kp})
;;                                         (assoc-in [client-name :actions] @ui-action-map-acc)
;;                                         (assoc-in [client-name leaf-type] (merge @resy @resy-grp)))))))
;;                        active-clients-and-tab))
;;                      {dragging-kp @ui-action-map-acc})]
;;         (swap! leaf-eval-runstream-parallel-cache assoc cache-key (get @db/leaf-atom client-name))
;;         result))))

;; Function to clear the cache when needed
(defn clear-leaf-eval-runstream-parallel-cache! []
  (reset! leaf-eval-runstream-parallel-cache {}))





;; (ut/pp [:leaf-bench (into {} (for [[k v] @db/leaf-bench]
;;                                {k {:parallel-fields (ut/avg (get-in v [:parallel :fields] []))
;;                                    :parallel-views (ut/avg (get-in v [:parallel :views] []))

;;                                    :serial-fields (ut/avg (get-in v [:serial :fields] []))
;;                                    :serial-views (ut/avg (get-in v [:serial :views] []))}}))])

;; (clear-all-leaf-caches "fsdfs")

;; (time (do (leaf-eval-runstream :fields :truthful-cerulean-eagle-40) (leaf-eval-runstream :views :truthful-cerulean-eagle-40)))

;; (leaf-eval-runstream :views)
;; (leaf-eval-runstream :fields)


;; (defn generate-all-drag-possibles-for [panel-hash client-name]
;;   (ut/pp [:srtart]) ;; pre-generate for all possible selects
;;    (let [tt (atom {})]
;;      (doseq [sel (into (field-keypaths client-name) (view-keypaths client-name))]
;;        (let [res (leaf-eval-runstream2 :views client-name sel (leaf-eval-runstream2 :fields client-name sel))]
;;          (swap! tt assoc sel res)))
;;      ;;@tt
;;      (ut/pp [:end])
;;      (swap! db/leaf-brute-force-map assoc-in [client-name panel-hash] @tt)
;;      ))

;; (defn generate-all-drag-possibles-parallel [client-name]
;;   (ut/pp ;; pre-generate for all possible selects
;;    (let [tt (atom {})]
;;      (doseq [sel (into (field-keypaths client-name) (view-keypaths client-name))]
;;        (let [res (do (leaf-eval-runstream-parallel :fields client-name sel)
;;                      (leaf-eval-runstream-parallel :views client-name sel))]
;;          (swap! tt merge res)))
;;      @tt)))


;; (defn generate-all-drag-possibles [panel-hash client-name]
;;   (ut/pp ;; pre-generate for all possible selects
;;    (let [all-keypaths (into (field-keypaths client-name) (view-keypaths client-name))
;;          results (r/fold
;;                   (r/monoid merge (constantly {}))
;;                   (fn [acc sel]
;;                     (let [res (leaf-eval-runstream2 :views client-name sel (leaf-eval-runstream2 :fields client-name sel))]
;;                       {sel (merge acc res)}))
;;                   all-keypaths)]
;;      ;results
;;      (swap! db/leaf-brute-force-map assoc-in [client-name panel-hash] results)
;;      )))

;; (defn generate-all-drag-possibles-chunked [client-name]
;;   (ut/pp
;;    (let [all-keypaths (into (field-keypaths client-name) (view-keypaths client-name))
;;          batch-size 10  ; Adjust this based on your needs
;;          batches (partition-all batch-size all-keypaths)
;;          results (r/fold
;;                   (r/monoid merge (constantly {}))
;;                   (fn [acc batch]
;;                     (reduce (fn [acc sel]
;;                               (merge acc
;;                                      (leaf-eval-runstream-parallel :fields client-name sel)
;;                                      (leaf-eval-runstream-parallel :views client-name sel)))
;;                             acc
;;                             batch))
;;                   batches)]
;;      results)))

;; (defn generate-all-drag-possibles-pmap! [client-name]
;;    (let [all-keypaths (into (field-keypaths client-name) (view-keypaths client-name))
;;          results (->> all-keypaths
;;                       (pmap (fn [sel]
;;                               (merge (leaf-eval-runstream-parallel :fields client-name sel)
;;                                      (leaf-eval-runstream-parallel :views client-name sel))))
;;                       (apply merge))]
;;      (ut/pp [:finished :generate-all-drag-possibles-pmap! client-name])
;;      (swap! db/leaf-atom assoc-in [client-name :all-actions] results)))

;; (defn clean-leaf-pull [client-name sel]
;;   (leaf-eval-runstream2 :views client-name sel
;;                         (leaf-eval-runstream2 :fields client-name sel
;;                                               {})))

;; (defn generate-all-drag-possibles-pmap! [panel-hash client-name]
;;   (let [start-time (System/nanoTime)
;;         all-keypaths (conj (distinct (into (field-keypaths client-name)
;;                                            (view-keypaths client-name))) [:canvas :canvas :canvas])
;;         _ (ut/pp ["🍁" :started :generate-all-drag-possibles-pmap! client-name (count all-keypaths) :keypaths])
;;         results (->> all-keypaths
;;                      (pmap (fn [sel]
;;                             ;;  {sel  {:fields (leaf-eval-runstream2 :fields client-name sel)
;;                             ;;         :views (leaf-eval-runstream2 :views client-name sel)}}
;;                             ;;  {sel (leaf-eval-runstream2 :views client-name sel
;;                             ;;                             (leaf-eval-runstream2 :fields client-name sel
;;                             ;;                                                   (get-in @db/leaf-brute-force-map [client-name sel])))}
;;                              {sel (get (leaf-eval-runstream3 client-name sel) :actions)}
;;                              ;;(swap! db/leaf-brute-force-map assoc-in [client-name sel] (leaf-eval-runstream3 client-name sel))
;;                              ))
;;                      (apply merge))
;;         end-time (System/nanoTime)
;;         execution-time (/ (- end-time start-time) 1e9)]
;;     (ut/pp ["🍁" :finished :generate-all-drag-possibles-pmap! client-name panel-hash
;;             :execution-time (str execution-time " seconds")
;;             ;:total-actions-possibilities (count (keys results))
;;             ])
;;     (swap! db/leaf-atom assoc-in [client-name :all-actions] results)
;;     (swap! db/leaf-brute-force-map assoc-in [client-name] results)
;;     ))

;; (leaf-eval-runstream2 :fields :rewarding-coffee-guinea-pig-13 [:block-7533 :queries :grouped-all-superstore-5])

;; (ut/pp (keys (get-in @db/leaf-brute-force-map [:rewarding-coffee-guinea-pig-13 (get @db/leaf-brute-force-last-panel-hash :rewarding-coffee-guinea-pig-13)])))
;; (get-in @db/leaf-brute-force-map [:rewarding-coffee-guinea-pig-13 (get @db/leaf-brute-force-last-panel-hash :rewarding-coffee-guinea-pig-13) ])
;; ;; (ut/pp @db/leaf-brute-force-last-panel-hash)
;; (ut/pp (get-in @db/leaf-brute-force-map [:rewarding-coffee-guinea-pig-13 (get @db/leaf-brute-force-last-panel-hash :rewarding-coffee-guinea-pig-13)
;;                                          [:block-7533 :queries :grouped-all-superstore-5] ] ))
;; (ut/pp @db/leaf-brute-force-map )
;; (reset! db/leaf-brute-force-map {})

;; (time (generate-all-drag-possibles-for :grin-hexagonal-elephant-11)) ;; 154 / 63 secs
;; (time (generate-all-drag-possibles-parallel :grin-hexagonal-elephant-11)) ;; 148 / 10 sec
;; (time (generate-all-drag-possibles-pmap :grin-hexagonal-elephant-11)) ;; 82 / 13

;; (time (generate-all-drag-possibles-for :imaginative-lavender-mole-43)) ;; 8.5
;; (time (generate-all-drag-possibles-parallel :imaginative-lavender-mole-43)) ;;
;; (time (generate-all-drag-possibles-pmap :imaginative-lavender-mole-43)) ;;

;; (time (generate-all-drag-possibles-for :bountiful-sapphire-leopard-25)) ;; 4 secs
;; (time (generate-all-drag-possibles-parallel :bountiful-sapphire-leopard-25)) ;; 2 sec
;; (time (generate-all-drag-possibles-pmap :bountiful-sapphire-leopard-25)) ;; 22 sec



;; (leaf-eval-runstream :views :truthful-cerulean-eagle-40)
;; (leaf-eval-runstream :fields :truthful-cerulean-eagle-40)

;; (ut/pp @db/leaf-drags-atom)
;; (ut/pp (get-in @db/params-atom [:truthful-cerulean-eagle-40 :dragging-body]) {:width 120})


