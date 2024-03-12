(ns rvbbit-frontend.events
  (:require
   [re-frame.core :as re-frame]
   [re-pressed.core :as rp]
   [rvbbit-frontend.db :as db]
   [day8.re-frame.tracing :refer-macros [fn-traced]]
   [district0x.re-frame.window-fx]))

(re-frame/reg-event-db
 ::initialize-db
 #_{:clj-kondo/ignore [:unresolved-symbol]}
 (fn-traced [_ _]
            db/default-db))

(re-frame/reg-event-db
 ::set-re-pressed-example
 (fn [db [_ value]]
   (assoc db :re-pressed-example value)))
