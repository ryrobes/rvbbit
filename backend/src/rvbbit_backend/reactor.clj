(ns rvbbit-backend.reactor
  (:require [clojure.string :as cstr]
            [rvbbit-backend.pool-party :as ppy]
            [rvbbit-backend.util :as ut])
  (:import (java.util.concurrent Executors)))

(def shard-map-count 4)



;;(rkt/dynamic-rabbit-reactor! shard-maps master-atoms :rvbbit-reactor1 tracking-atom)

(defn parse-rabbit-path
  [coded-keypath]
  (let [ff             (cstr/split (-> (str coded-keypath)
                                       (cstr/replace #":" ""))
                                   #"/")
        ff2            (cstr/split (last ff) #">")
        keyword-or-int (fn [s] (if (re-matches #"\d+" s) (Integer/parseInt s) (keyword s)))
        master-type    (keyword (first ff))]
    (vec (concat [master-type]
                 (if (= master-type :flow)
                   [(second ff2)]
                   [(keyword-or-int (second ff2))])
                 (map keyword-or-int (drop 1 ff2))))))

(defn get-master-type [coded-keypath]
  (keyword (first (cstr/split (subs (str coded-keypath) 1) #"/"))))

(defn get-shard-path [coded-keypath]
  (rest (parse-rabbit-path coded-keypath)))

(defn get-shard-map-index [coded-keypath]
  (mod (hash coded-keypath) shard-map-count))

(defn create-shard-maps []
  (vec (repeatedly shard-map-count #(atom {}))))

(defn create-tracking-atom []
  (atom {}))

(def shard-maps (create-shard-maps))

(def tracking-atom (create-tracking-atom))

(defn update-tracking [tracking-atom coded-keypath]
  (swap! tracking-atom update coded-keypath
         (fn [track]
           (-> (or track {})
               (update :access-count (fnil inc 0))
               (assoc :last-access (System/currentTimeMillis))))))

;; (defn create-thread-pool [thread-count]
;;   (Executors/newFixedThreadPool thread-count))

;;(dynamic-rabbit-reactor! shard-maps master-atoms :my-reactor tracking-atom)

(defn dynamic-rabbit-reactor
  [shard-maps master-atom-map identifier tracking-atom]
  (ut/pp [:booting-atom-shard-reactor identifier])
  (let [watch-fn (fn [master-type]
                   (fn [_ _ old-state new-state]
                     (ppy/execute-in-thread-pools identifier
                                                  (fn []
                                                    (doseq [shard-map shard-maps]
                                                      (doseq [[coded-keypath shard-atom] @shard-map]
                                                        (when (= (get-master-type coded-keypath) master-type)
                                                          (let [shard-path (get-shard-path coded-keypath)
                                                                old-value (get-in old-state shard-path)
                                                                new-value (get-in new-state shard-path)]
                                                            (when (not= old-value new-value)
                                                              (reset! shard-atom new-value)
                                                              (update-tracking tracking-atom coded-keypath))))))))))]

    (doseq [[master-type master-atom] master-atom-map]
      (add-watch master-atom identifier (watch-fn master-type)))))

(defn get-shard-atom
  [shard-maps master-atom-map tracking-atom coded-keypath]
  (let [shard-map-index (get-shard-map-index coded-keypath)
        shard-map (nth shard-maps shard-map-index)]
    (if-let [shard-atom (get @shard-map coded-keypath)]
      (do
        (update-tracking tracking-atom coded-keypath)
        shard-atom)
      (let [master-type (get-master-type coded-keypath)
            shard-path (get-shard-path coded-keypath)
            master-atom (get master-atom-map master-type)
            current-value (get-in @master-atom shard-path)
            new-atom (atom current-value)]
        (swap! shard-map assoc coded-keypath new-atom)
        (update-tracking tracking-atom coded-keypath)
        new-atom))))

(defn cleanup-shards
  [shard-maps tracking-atom timeout-ms]
  (let [current-time (System/currentTimeMillis)]
    (doseq [shard-map shard-maps]
      (swap! shard-map
             (fn [m]
               (->> m
                    (remove (fn [[coded-keypath _]]
                              (let [last-access (get-in @tracking-atom [coded-keypath :last-access])]
                                (and last-access (> (- current-time last-access) timeout-ms)))))
                    (into {})))))))







;; Usage example:
(comment
  (def master-atoms {:flow (atom {"myflow-id" {:thing1 {:thing2 "nested value"}
                                               :other-thing "shallow value"}})
                     :user (atom {:user1 {:name "Alice" :age 30}
                                  :user2 {:name "Bob" :age 25}})})
  (def shard-maps (create-shard-maps))
  (def tracking-atom (create-tracking-atom))

  (dynamic-rabbit-reactor shard-maps master-atoms :my-reactor tracking-atom)

  ;; Get a shard atom (this implicitly creates a subscription)
  (def flow-shard (get-shard-atom shard-maps master-atoms tracking-atom :flow/myflow-id>thing1>thing2))
  @flow-shard  ;; => "nested value"

  ;; Update master atom
  (swap! (:flow master-atoms) assoc-in ["myflow-id" :thing1 :thing2] "updated value")
  ;; The update will be processed asynchronously in the thread pool

  ;; Check tracking info
  @tracking-atom

  ;; Simulate passage of time and cleanup
  (cleanup-shards shard-maps tracking-atom (* 3 60 60 1000))  ;; 3 hour timeout

  ;; Don't forget to shut down the thread pool when you're done
  )