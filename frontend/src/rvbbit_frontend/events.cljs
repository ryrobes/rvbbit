(ns rvbbit-frontend.events
  (:require
    [district0x.re-frame.window-fx]
    [re-frame.core      :as re-frame]
    [rvbbit-frontend.db :as db]))

(re-frame/reg-event-db ::initialize-db #_{:clj-kondo/ignore [:unresolved-symbol]} (fn [_ _] db/default-db))

