(ns rvbbit-frontend.events
  (:require
   [re-frame.core :as re-frame]
   ;[re-pressed.core :as rp]
   [rvbbit-frontend.db :as db]
   ;;[day8.re-frame.tracing :refer-macros [fn-traced]]
   [district0x.re-frame.window-fx]))

(re-frame/reg-event-db
 ::initialize-db
 #_{:clj-kondo/ignore [:unresolved-symbol]}
 (fn [_ _] db/default-db))

