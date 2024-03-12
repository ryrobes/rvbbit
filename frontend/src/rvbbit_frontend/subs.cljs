(ns rvbbit-frontend.subs
  (:require
   [re-frame.core :as re-frame]
   [district0x.re-frame.window-fx]))

(re-frame/reg-sub
 ::name
 (fn [db]
   (:name db)))

(re-frame/reg-sub
 ::re-pressed-example
 (fn [db _]
   (:re-pressed-example db)))

(re-frame/reg-sub
 ::server
 (fn [db]
   (:server db)))

(re-frame/reg-event-db
 ::window-resized
 (fn [db [_ w h]]
   ;(assoc db :window [w h])
   (-> db
       (assoc-in [:window :w] w)
       (assoc-in [:window :h] h))))

(re-frame/reg-sub ::w (fn [db] (get-in db [:window :w])))
(re-frame/reg-sub ::h (fn [db] (get-in db [:window :h])))

(re-frame/reg-event-fx
 ::window-fx-watcher
 (fn []
   {:window/on-resize {:dispatch [::window-resized]
                       :debounce-ms 200
                       ;; :id is optional
                       ;:id ::my-listener
                       }}))