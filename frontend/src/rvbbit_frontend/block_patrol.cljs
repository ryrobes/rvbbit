(ns rvbbit-frontend.block-patrol
  (:require [re-frame.core :as re-frame]
            [rvbbit-frontend.utility :as ut]
            [rvbbit-frontend.db :as db]
            [rvbbit-frontend.bricks :as bricks]
            ;[rvbbit-frontend.http :as http]
            [websocket-fx.core :as wfx]
            [clojure.string :as cstr]))

;; Hash function for a single block
(defn hash-block [block]
  (let [cleaned-block (ut/deep-remove-underscore-keys block)]
    (hash (cstr/join (sort (map str (flatten (into [] cleaned-block))))))))

;; Subscription to get the hash of the entire :panels structure
(re-frame/reg-sub
 ::panels-hash
 (fn [db]
   (let [panels (dissoc (:panels db) nil)
         cleaned-panels (ut/deep-remove-underscore-keys panels)]
     (hash (cstr/join (sort (map str (flatten (into [] cleaned-panels)))))))))

;; Subscription to get all panels with their individual hashes
(re-frame/reg-sub
 ::panels-with-hashes
 (fn [db]
   (reduce-kv
    (fn [acc panel-key panel-data]
      (assoc acc panel-key {:data panel-data
                            :hash (hash-block panel-data)}))
    {}
    (dissoc (:panels db) nil))))

(re-frame/reg-event-fx
 ::push-panels-to-server
 (fn [{:keys [db]} [_ panels]]
   (let [client-name (get db :client-name)
         runners   (keys (get-in db [:server :settings :runners]))
        ;;  panels (into {}
        ;;               (for [[k v] panels
        ;;                     :let [ds-keys (vec (apply concat (for [r runners
        ;;                                                            :when (get v r)
        ;;                                                            :let [vkeys (keys (get v r))]]
        ;;                                                        (for [v vkeys] [r v]))))
        ;;                           implicit-rowsets (vec (into (get-in db [:implicit-rowsets :clover] [])
        ;;                                                       (get-in db [:implicit-rowsets :solver] [])))
        ;;                           implied? (fn [data-keypath] (let [data-keypath (last data-keypath)]
        ;;                                                         (some #(= % (first data-keypath)) implicit-rowsets)))
        ;;                           ;implied-ds (filterv (fn [x] @(ut/tracked-sub ::bricks/is-implied-rowset? {:data-keypath [(last x)]})) ds-keys)
        ;;                           implied-ds (filterv implied? ds-keys)
        ;;                           ;; _ (ut/tapp>> [:implied-db k (str ds-keys) (str implied-ds)])
        ;;                           ]]
        ;;                 {k (assoc v :implied-rowsets implied-ds)}))
         panels (reduce-kv
                 (fn [acc k v]
                   (let [ds-keys (into []
                                       (comp
                                        (filter #(get v %))
                                        (mapcat #(map (fn [vk] [% vk]) (keys (get v %)))))
                                       runners)
                         implicit-rowsets (into (get-in db [:implicit-rowsets :clover] [])
                                                (get-in db [:implicit-rowsets :solver] []))
                         implied-ds (filterv #(some (fn [x] (= x (last %))) implicit-rowsets) ds-keys)
                         ;_ (ut/tapp>> [:ds-keys k (str implied-ds) (str ds-keys) (str implicit-rowsets)])
                         ]
                     (assoc acc k (assoc v :implied-rowsets implied-ds))))
                 {}
                 panels)
         ;dbody-keys (filterv (fn [x] (cstr/starts-with? (str (first x)) ":block")) (keys @db/drag-body-map))
         ;dbody-keys-this (filterv (fn [x] (some #(= % (first x)) (keys panels))) dbody-keys)
         ;drag-body-map-this-tab (select-keys @db/drag-body-map dbody-keys-this)
         ;;;_ (ut/pp [:this-keys dbody-keys1 dbody-keys2 (keys @db/drag-body-map)])
         ]
     (ut/tapp>> [:pushing-panels-to-server (count (keys panels)) (str (keys panels)) ])
     {:dispatch-later
      [
      ;;  {:ms 4000
      ;;   :dispatch [::wfx/push :default ;:leaves
      ;;              {:kind :warm-leaf-evals
      ;;               :timeout  500000
      ;;               :work-targets dbody-keys-this
      ;;               :client-name client-name}]}
       {:ms 800
        :dispatch [::bricks/refresh-history-log]}]
      :dispatch
      [::wfx/push :default ;:secondary
       {:kind :current-panels
        :panels   panels
        :timeout  500000
        :materialized-panels {} ;; ppm
        :resolved-panels {} ;; ppr
        :drag-body-map {} ;;drag-body-map-this-tab
        ;; :resolved-panels  (into {} (for [[k v] panels] ;; super slow and lags out clients when panels edited
        ;;                             {k (assoc v :queries (into {} (for [[kk vv] (get v :queries)]
        ;;                                                             {kk (bricks/sql-alias-replace vv)})))}))
        :client-name client-name}]})))

;; Effect to run side effect function for changed panels
(re-frame/reg-fx
 :push-changed-panels
 (fn [changed-panels]
   (let [changed-panels-keys (filterv #(not (cstr/starts-with? (str %) ":reco-preview")) (keys changed-panels))
         changed-panels (select-keys changed-panels changed-panels-keys)]
    ;;  (doseq [panel-key changed-panels-keys]
    ;;    (ut/tracked-dispatch [::bricks/save-snap-block panel-key]))
     (ut/tapp>> [:pushing-changed-panels (str changed-panels-keys)])
     (re-frame/dispatch [::push-panels-to-server changed-panels]))))

(re-frame/reg-event-fx
 ::deal-with-changed-panels
 (fn [{:keys [db]} _]
   (let [old-hashes (get db :panel-hashes)
         new-panels-with-hashes @(ut/tracked-sub ::panels-with-hashes {})
         new-hashes (into {} (map (fn [[k v]] [k (:hash v)]) new-panels-with-hashes))
         changed-panels (reduce-kv
                         (fn [acc panel-key {:keys [data hash]}]
                           (if (not= hash (get old-hashes panel-key))
                             (assoc acc panel-key data)
                             acc))
                         {}
                         new-panels-with-hashes)
         is-initial-boot? (empty? old-hashes)] ;; if nothing has a hash, it's the first time - dont trigger
     (cond-> {:db (-> db
                      (assoc :panel-hashes new-hashes)
                      (assoc-in [:click-param :panel-hash] new-hashes))}
       (and (not is-initial-boot?) (seq changed-panels))
       (assoc :push-changed-panels changed-panels)))))

(re-frame/reg-sub
 ::panels-changed?
 (fn [db]
   (let [old-hash (::overall-panels-hash db)
         new-hash @(ut/tracked-sub ::panels-hash {})]
     (and (not @bricks/on-scrubber?)
          (not= old-hash new-hash)))))

