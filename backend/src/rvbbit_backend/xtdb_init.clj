(ns rvbbit-backend.xtdb-init
  (:require    [xtdb.node :as xtn]
               [xtdb.api :as xt]))

(defonce xtdb-node
  (xtn/start-node
   {:log     [:local {:path "db/xtdb-system/tx-log"}]
    :storage [:local {:path "db/xtdb-system/storage"}]}))

(defn get-xtdb-node []
  xtdb-node)

