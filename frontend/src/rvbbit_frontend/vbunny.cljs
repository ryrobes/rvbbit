(ns rvbbit-frontend.vbunny
  (:require [reagent.core                   :as reagent]
            [re-frame.core                  :as re-frame]
            [rvbbit-frontend.utility        :as ut]
            ;[rvbbit-frontend.http           :as http]
            [rvbbit-frontend.db             :as db]
            [rvbbit-frontend.connections    :as conn]
            ;[reagent.dom                    :as rdom]
            [re-com.core                    :as re-com]
            [clojure.string                 :as cstr]
            [clojure.edn                    :as edn]
            [rvbbit-frontend.resolver       :as resolver]
            [react]))

(comment "custom virtualized v-box/h-box used for terminal and other things - data-viewer, etc")

;;; so many dupes of this, theme stuff needs to be its own namespace that gets refed by all (currently depends too heavily on conn & resolver)
(defn theme-pull
  [cmp-key fallback & test-fn]
  (let [wh @(ut/tracked-sub ::conn/world-hash {})]
    (if (contains? @db/theme-pull-cache [cmp-key fallback wh])

      (let [cache (get @db/theme-pull-cache [cmp-key fallback wh])]
        ;(ut/pp [:theme-pull-cache (str cmp-key)])
        cache)
      ;; cache
      (let [res (let [v                   @(ut/tracked-sub ::conn/clicked-parameter-key-alpha {:keypath [cmp-key]})
                      t0                  (ut/splitter (str (ut/safe-name cmp-key)) #"/")
                      t1                  (keyword (first t0))
                      t2                  (keyword (last t0))
                      self-ref-keys       (into #{} (filter namespace) (ut/deep-flatten db/base-theme))
                      self-ref-pairs (reduce (fn [acc k]
                                               (let [bk (keyword (cstr/replace (name k) "theme/" ""))]
                                                 (if-let [value (get db/base-theme bk)]
                                                   (assoc acc k value)
                                                   acc)))
                                             {}
                                             self-ref-keys)
                      resolved-base-theme (ut/postwalk-replacer self-ref-pairs db/base-theme)
                      base-theme-keys     (keys resolved-base-theme)
                      theme-key?          (true? (and (= t1 :theme) (some #(= % t2) base-theme-keys)))
                      fallback0           (if theme-key? (get resolved-base-theme t2) fallback)
                      rs                  (fn [edn] (resolver/logic-and-params edn :theme-pull))]
                  (rs (if (not (nil? v)) v fallback0)))]
        (swap! db/theme-pull-cache assoc [cmp-key fallback wh] res)
        ;(ut/pp [:theme-pull-NON-cache (str cmp-key) (str res)])
        res))))


(def scrollbar-stylev
  {:scrollbar-width "thin"
   ;;:scrollbar-color (str (theme-pull :theme/universal-pop-color nil) "#00000033")
   :scrollbar-color (str (theme-pull :theme/editor-outer-rim-color nil) "#00000033")
   "--webkit-scrollbar" {;:height "10px"
                          ;:opacity 0.8
                          :border-radius "5px"}
   "--webkit-scrollbar-track" {:background "#2a2a2a"
                                :border-radius "5px"}
   "--webkit-scrollbar-thumb" {:background "#4a4a4a"
                                :opacity 0.8
                                :border-radius "15px"}
   "--webkit-scrollbar-thumb:hover" {:background "#6a6a6a"}})

(defn safe-subvec [v start end]
  (let [start (max 0 start)
        end (min (count v) end)]
    (if (< start end)
      (subvec v start end)
      [])))

(defonce scroll-state (reagent/atom {}))
(defonce node-ref (reagent/atom {}))

;; (ut/tapp>> [:scroll-state @scroll-state])

(defn px- [x]
  (try (edn/read-string (cstr/replace (str x) "px" ""))
       (catch :default _ 0)))

(defn clean-child-map [children id]
  ;; functions change every time they are rendered, so we can't use them inside a diff or hash
  (let [res (ut/deep-remove-keys
             children
             [:on-click :on-context-menu :on-double-click :on-hover :on-mouse-down :on-mouse-leave
              :on-mouse-up :on-mouse-move :on-mouse-out :on-mouse-over :on-mouse-enter])]
    res))

(defn calculate-v-heights [new-children id & [internal?]]
  (let [new-heights (mapv second new-children)
        new-cumulative-heights (reduce
                                (fn [acc height]
                                  (conj acc (+ (or (last acc) 0) height)))
                                []
                                new-heights)
        children-hash (hash (clean-child-map new-children id))]
    ;;(ut/tapp>> [:calculate-v-heights id new-heights new-cumulative-heights children-hash])
    (swap! scroll-state update id
           (fn [state]
             (let [;old-total-height (:total-height state 0)
                   new-total-height (last new-cumulative-heights)]
               (-> state
                   (assoc :heights new-heights)
                   (assoc :children-hash children-hash)
                   (assoc :cumulative-heights new-cumulative-heights)
                   (update :max-height #(max (or % 0) new-total-height))
                   (assoc :total-height new-total-height)))))))

(defn virtualized-v-box [{:keys [children style width height id follow?] :as props}]
  (let [;node-ref (reagent/atom nil)
        is-at-bottom (reagent/atom true)  ; Initialize as true
        ;wssk @(ut/tracked-subscribe_ [::http/websocket-status])
        ;websocket-status (select-keys wssk [:status :datasets :panels :waiting])
        online? true ;(true? (= (get websocket-status :status) :connected))
        mouse-active?  (or @(ut/tracked-sub ::ut/is-mouse-active-alpha? {:seconds 60}) (not online?))
        update-visible-range (fn [container-height scroll-top]
                               (let [{:keys [cumulative-heights max-height total-height]} (get @scroll-state id)
                                     start (or (some #(when (> (nth cumulative-heights % 0) scroll-top) %)
                                                     (range (count cumulative-heights)))
                                               0)
                                     end (or (some #(when (> (nth cumulative-heights % 0) (+ scroll-top container-height)) %)
                                                   (range (count cumulative-heights)))
                                             (count children))
                                     at-bottom? (>= (+ scroll-top container-height) (- total-height 1))]
                                 (reset! is-at-bottom at-bottom?)
                                 (swap! scroll-state update id assoc
                                        :start start
                                        :end end
                                        :scroll-top scroll-top)))
        handle-scroll (fn [e]
                        (let [node (.-target e)
                              container-height (.-clientHeight node)
                              scroll-top (.-scrollTop node)]
                          (swap! scroll-state update id
                                 (fn [state]
                                   (let [total-height (:total-height state 0)
                                         old-scroll-top (:scroll-top state 0)
                                         scrolling-up? (< scroll-top old-scroll-top)
                                         within-content? (<= (+ scroll-top container-height) total-height)
                                         new-max-height (cond
                                                          (not scrolling-up?) (max (:max-height state 0) (+ scroll-top container-height))
                                                          (and scrolling-up? within-content?) total-height
                                                          :else (:max-height state 0))]
                                     (-> state
                                         (assoc :scroll-top scroll-top)
                                         (assoc :max-height new-max-height)))))
                          (update-visible-range container-height scroll-top)))
        ;; find-start-index (fn [scroll-top]
        ;;                    (let [cumulative-heights (get-in @scroll-state [id :cumulative-heights])]
        ;;                      (or (some #(when (> (nth cumulative-heights % 0) scroll-top) %)
        ;;                                (range (count cumulative-heights)))
        ;;                          0)))
        ;; update-visible-range (fn [container-height scroll-top]
        ;;                        (let [start (find-start-index scroll-top)
        ;;                              initial-end (find-start-index (+ scroll-top container-height))
        ;;                              end (loop [idx initial-end]
        ;;                                    (let [current-height (- (get-in @scroll-state [id :cumulative-heights idx] 0)
        ;;                                                            (get-in @scroll-state [id :cumulative-heights start] 0))]
        ;;                                      (if (or (>= current-height container-height) (>= idx (count children)))
        ;;                                        (min (inc idx) (count children))
        ;;                                        (recur (inc idx)))))
        ;;                              end (max end (min (count children) (+ start 1)))] ; Ensure at least 2 items are rendered
        ;;                          (swap! scroll-state update id assoc
        ;;                                 :scroll-top scroll-top
        ;;                                 :start start
        ;;                                 :end end)))
        ;; handle-scroll (fn [e]
        ;;                 (let [container-height (.. e -target -clientHeight)
        ;;                       scroll-top (.. e -target -scrollTop)]
        ;;                   (update-visible-range container-height scroll-top)))
        set-scroll-position (fn [node]
                              (when node
                                (let [{:keys [scroll-top total-height]} (get @scroll-state id)
                                      container-height (.-clientHeight node)
                                      new-scroll-top (if (and follow? @is-at-bottom)
                                                       (max 0 (- total-height container-height))
                                                       (or scroll-top 0))]
                                  (set! (.-scrollTop node) new-scroll-top)
                                  (update-visible-range container-height new-scroll-top))))]

    (reagent/create-class
     {:component-did-mount
      ;; (fn [this]
      ;;   (let [node (rdom/dom-node this)]
      ;;     (reset! node-ref node)
      ;;     (set-scroll-position node)))

      (fn [this]
        (set-scroll-position (get @node-ref id)))

      ;; :component-did-update
      ;; (fn [this old-argv new-argv]
      ;;   (let [node (get @node-ref id)]
      ;;     (set-scroll-position node)))

      ;; :component-did-update
      ;; (fn [this]
      ;;   (set-scroll-position (get @node-ref id)))

      :component-will-unmount
      (fn []
        (when-let [node (get @node-ref id)]
          (.removeEventListener node "scroll" handle-scroll)))

      :reagent-render
      (fn [{:keys [children style width height id follow?] :as props}]
        (let [{:keys [start end cumulative-heights max-height scroll-top]
               :or {start 0 end 0 scroll-top 0}} (get @scroll-state id)
              end (if ;(or (cstr/includes? id "history-v-box")
                      ;    (cstr/includes? id "parameter-browser"))
                      true
                    (inc end) end) ;; get an extra block for history...
              visible-children (safe-subvec children start end)
              v-box-style (merge {:overflow-y (if mouse-active? "auto" "hidden")
                                  :overflow-x "hidden"}
                                 style
                                 scrollbar-stylev)]
          [:div (merge
                 (dissoc props :children :initial-scroll :follow?)
                 {:style (merge v-box-style
                                {:width (str width "px")
                                 :height (str height "px")})
                  :on-scroll handle-scroll
                  :ref #(when % (swap! node-ref assoc id %))})
           [:div {:style {:height (str max-height "px")
                          :position "relative"}}
            (for [[index [child-width child-height child]] (map-indexed vector visible-children)]
              ^{:key (str id (+ start index))}
              [:div {:style {:position "absolute"
                             :top (str (if (zero? (+ start index))
                                         0
                                         (nth cumulative-heights (dec (+ start index)) 0)) "px")
                             :height (str child-height "px")
                             :width (str child-width "px")}}
               child])

            ;; (let [ttl (- (count children) 1) ;; just debug bottom labels
            ;;       rendered (- (- (- start end)) 1)
            ;;       ss  (+ start 1)
            ;;       ee (min end ttl)
            ;;       all? (= rendered ttl)
            ;;       ee (if all? (dec ee) ee)
            ;;       ttl (if all? (dec ttl) ttl)
            ;;       rendered (if all? (dec rendered) rendered)]
            ;;   [:div {:style {:position "fixed"
            ;;                  :bottom 15
            ;;                  :font-size "11px"
            ;;                  :right 13
            ;;                  :font-family (theme-pull :theme/base-font nil)
            ;;                  :color "#ffffff55"}}
            ;;    (str ss "-" ee " of " ttl " (" rendered  " rendered)")])
            ]]))})))

(defn reactive-virtualized-v-box [props]
  (fn [props]
    (calculate-v-heights (get props :children)
                         (get props :id) true)
    [:f> (with-meta virtualized-v-box
           {:key (hash [(clean-child-map (get props :children) nil) (get props :id)])})
     props]))

(defn vv-box [w h num] ;; test fn
  [reactive-virtualized-v-box
   {:children (vec (for [i (range num)
                         :let [hh 145]]
                     [w hh
                      [re-com/box
                       :size "none"
                       :height (str hh "px") :width (str (- w 20) "px")
                       :style {:border "1px solid white"}
                       :child (str "heys " i)]]))
    :id (str h w)
    :height h
    :width w}])

(defn virtual-v-box [& {:keys [id children height width attr fixed?] :as cfg-map}]
  (let [width  (if (string? width)  (px- width) width)
        height (if (string? height) (px- height) height)
        ;; _ (ut/tapp>> [:id id children])
        ;; children (vec (cons [20 20 [:div {:style {}} " "]]
        ;;                     (conj children [20 20 [:div {:style {}} " "]])))
        children (vec (conj children [20 20 [:div {:style {}} " "]]))
        ;; empty div on bottom and top cleans up some virtual-dom scrolling fuckery, gets more accurate recalc sizes
        id (or id (get attr :id)) ;; in case mixed with re-com, which will throw if given root level :id key
        id (str id "-" height width)
        cfg-map (-> cfg-map
                    (assoc :width width)
                    (assoc :children children)
                    (assoc :height height)
                    (assoc :id id))]
    [reactive-virtualized-v-box cfg-map]))

(def h-scroll-state (reagent/atom {}))

(defn virtualized-h-box [{:keys [children style width height id] :as props}]
  (let [node-ref (reagent/atom nil)
        calculate-widths (fn [new-children]
                           (let [new-widths (mapv first new-children)
                                 new-cumulative-widths (reduce
                                                        (fn [acc width]
                                                          (conj acc (+ (or (last acc) 0) width)))
                                                        []
                                                        new-widths)]
                             (swap! h-scroll-state update id assoc
                                    :widths new-widths
                                    :cumulative-widths new-cumulative-widths
                                    :total-width (last new-cumulative-widths))))

        find-start-index (fn [scroll-left]
                           (let [cumulative-widths (get-in @h-scroll-state [id :cumulative-widths])]
                             (or (some #(when (> (nth cumulative-widths % 0) scroll-left) %)
                                       (range (count cumulative-widths)))
                                 0)))

        update-visible-range (fn [container-width scroll-left]
                               (let [start (find-start-index scroll-left)
                                     end (find-start-index (+ scroll-left container-width))
                                     end (min (count children) (+ end 2))] ; Add 2 for buffer
                                 (swap! h-scroll-state update id assoc
                                        :scroll-left scroll-left
                                        :start start
                                        :end end)))

        handle-scroll (fn [e]
                        (let [container-width (.. e -target -clientWidth)
                              scroll-left (.. e -target -scrollLeft)]
                          (update-visible-range container-width scroll-left)))

        set-scroll-position (fn [node]
                              (when node
                                (let [{:keys [scroll-left total-width]} (get @h-scroll-state id)
                                      max-scroll (- total-width width)
                                      target-scroll (min (or scroll-left 0) max-scroll)]
                                  (set! (.-scrollLeft node) target-scroll)
                                  (update-visible-range width target-scroll))))]

    (reagent/create-class
     {:component-did-mount
      (fn [this]
        (calculate-widths children)
        (set-scroll-position @node-ref))

      :component-did-update
      (fn [this old-argv]
        (let [new-argv (reagent/argv this)
              old-children (get-in old-argv [1 :children])
              new-children (get-in new-argv [1 :children])]
          (when (not= (count old-children) (count new-children))
            (calculate-widths new-children)
            (js/requestAnimationFrame #(set-scroll-position @node-ref)))))

      :reagent-render
      (fn [{:keys [children style width height id] :as props}]
        (let [{:keys [start end cumulative-widths total-width] :or {start 0 end 0}} (get @h-scroll-state id)
              visible-children (safe-subvec children start end)
              h-box-style (merge {:overflow-x "auto"
                                  :overflow-y "hidden"
                                  ;:border "1px solid cyan"
                                  }
                                 style
                                 scrollbar-stylev)]
          [:div (merge
                 (dissoc props :children :initial-scroll)
                 {:style (merge h-box-style
                                {:width (str width "px")
                                 :height (str height "px")})
                  :on-scroll handle-scroll
                  :ref #(reset! node-ref %)})
           [:div {:style {:width (str total-width "px")
                          :height "100%"
                          :position "relative"}}
            (for [[index [child-width child-height child]] (map-indexed vector visible-children)]
              ^{:key (+ start index)}
              [:div {:style {:position "absolute"
                             :left (str (if (zero? (+ start index))
                                          0
                                          (try (nth cumulative-widths (dec (+ start index))) (catch :default _ 0))) "px")
                             :height "100%"
                             :width (str child-width "px")}}
               child])
            [:div {:style {:position "fixed"
                           :top 41
                           :left 22
                           :color "white"}}
             (str "Showing items " start " to " end " of " (count children))]]]))})))

;; (defn virtualized-h-box [{:keys [children style width height id] :as props}]
;;   (let [calculate-widths (fn [new-children]
;;                            (let [new-widths (mapv first new-children)
;;                                  new-cumulative-widths (reduce
;;                                                         (fn [acc width]
;;                                                           (conj acc (+ (or (last acc) 0) width)))
;;                                                         []
;;                                                         new-widths)]
;;                              (swap! h-scroll-state update id assoc
;;                                     :widths new-widths
;;                                     :cumulative-widths new-cumulative-widths
;;                                     :total-width (last new-cumulative-widths))))

;;         find-start-index (fn [scroll-left]
;;                            (let [cumulative-widths (get-in @h-scroll-state [id :cumulative-widths])]
;;                              (or (some #(when (> (nth cumulative-widths % 0) scroll-left) %)
;;                                        (range (count cumulative-widths)))
;;                                  0)))

;;         update-visible-range (fn [container-width scroll-left]
;;                                (let [start (find-start-index scroll-left)
;;                                      end (find-start-index (+ scroll-left container-width))
;;                                      end (min (count children) (+ end 2))] ; Add 2 for buffer
;;                                  (swap! h-scroll-state update id assoc
;;                                         :scroll-left scroll-left
;;                                         :start start
;;                                         :end end)))

;;         handle-scroll (fn [e]
;;                         (let [container-width (.. e -target -clientWidth)
;;                               scroll-left (.. e -target -scrollLeft)]
;;                           (update-visible-range container-width scroll-left)))

;;         set-scroll-position (fn [node]
;;                               (when node
;;                                 (let [{:keys [scroll-left total-width]} (get @h-scroll-state id)
;;                                       max-scroll (- total-width width)
;;                                       target-scroll (min (or scroll-left 0) max-scroll)]
;;                                   (set! (.-scrollLeft node) target-scroll)
;;                                   (update-visible-range width target-scroll))))]

;;     (reagent/create-class
;;      {:component-did-mount
;;       (fn [this]
;;         (calculate-widths children)
;;         (set-scroll-position (rdom/dom-node  this)))

;;       :component-did-update
;;       (fn [this old-argv]
;;         (let [new-argv (reagent/argv this)
;;               old-children (get-in old-argv [1 :children])
;;               new-children (get-in new-argv [1 :children])]
;;           (when (not= (count old-children) (count new-children))
;;             (calculate-widths new-children)
;;             (js/requestAnimationFrame #(set-scroll-position (rdom/dom-node  this))))))

;;       :reagent-render
;;       (fn [{:keys [children style width height id] :as props}]
;;         (let [{:keys [start end cumulative-widths total-width] :or {start 0 end 0}} (get @h-scroll-state id)
;;               visible-children (safe-subvec children start end)
;;               h-box-style (merge {:overflow-x "auto"
;;                                   :overflow-y "hidden"
;;                                   ;:border "1px solid cyan"
;;                                   }
;;                                  style
;;                                  scrollbar-stylev)]
;;           [:div (merge
;;                  (dissoc props :children :initial-scroll)
;;                  {:style (merge h-box-style
;;                                 {:width (str width "px")
;;                                  :height (str height "px")})
;;                   :on-scroll handle-scroll})
;;            [:div {:style {:width (str total-width "px")
;;                           :height "100%"
;;                           :position "relative"}}
;;             (for [[index [child-width child-height child]] (map-indexed vector visible-children)]
;;               ^{:key (+ start index)}
;;               [:div {:style {:position "absolute"
;;                              :left (str (if (zero? (+ start index))
;;                                           0
;;                                           (try (nth cumulative-widths (dec (+ start index))) (catch :default _ 0))) "px")
;;                              :height "100%"
;;                              :width (str child-width "px")}}
;;                child])
;;             [:div {:style {:position "fixed"
;;                            :top 41
;;                            :left 22
;;                            :color "white"}}
;;              (str "Showing items " start " to " end " of " (count children))]]]))})))

(defn reactive-virtualized-h-box [props]
  (fn [props]
    [:f> (with-meta virtualized-h-box
           {:key (hash (:children props))})
     props]))

(defn vh-box [w h num] ;; your test fn
  [reactive-virtualized-h-box {:children (vec (for [i (range num)
                                                    :let [ww 145]]
                                                [ww h
                                                 [re-com/box
                                                  :size "none"
                                                  :width (str ww "px") :height (str (- h 20) "px")
                                                  :style {:border "1px solid white"}
                                                  :child (str "heys " i)]]))
                               :id (str h w)
                               :height h
                               :width w}])

(defn virtual-h-box [& {:keys [id children height width] :as cfg-map}]
  (let []
    [reactive-virtualized-h-box cfg-map]))


