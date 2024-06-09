(ns rvbbit-frontend.layout
  (:require
    [clojure.set    :as cset]
    [clojure.string :as cstr]
    [reagent.core   :as reagent]
    [rvbbit-frontend.utility :as ut]))

(defn point-inside-block?
  [x y block]
  (let [{:keys [root w h]} block
        [block-x block-y]  root]
    (and (>= x block-x) (< x (+ block-x w)) (>= y block-y) (< y (+ block-y h)))))

(defn compute-expanded-size
  [view-ref block all-blocks-map pw-int ph-int]
  (let [{:keys [root w h]} block
        [start-x start-y]  root
        all-blocks         (vec (remove #(= block %) (vals all-blocks-map)))
        start-x            (if (and (< start-x 1) (ut/is-float? start-x)) ;; treat as percent of
                                                                          ;; total
                             (js/Math.floor (* start-x pw-int))
                             start-x)
        start-y            (if (and (< start-y 1) (ut/is-float? start-y)) ;; treat as percent of
                                                                          ;; total
                             (js/Math.floor (* start-y ph-int))
                             start-y)
        h                  (if (and (< h 1) (ut/is-float? h)) ;; treat as percent of total
                             (js/Math.floor (* h ph-int))
                             h)
        w                  (if (and (< w 1) (ut/is-float? w)) ;; treat as percent of total
                             (js/Math.floor (* w pw-int))
                             w)
        all-blocks         (vec
                             (for [p    all-blocks
                                   :let [x (get-in p [:root 0])
                                         y (get-in p [:root 1])
                                         h (get p :h)
                                         w (get p :w)]]
                               (-> p
                                   (assoc :root [(if (and (< x 1) (ut/is-float? x)) ;; treat as
                                                   (js/Math.floor (* x pw-int))
                                                   x)
                                                 (if (and (< y 1) (ut/is-float? y)) ;; treat as
                                                   (js/Math.floor (* y ph-int))
                                                   y)])
                                   (assoc :h (if (and (< h 1) (ut/is-float? h)) ;; treat as
                                               (js/Math.floor (* h ph-int))
                                               h))
                                   (assoc :w (if (and (< w 1) (ut/is-float? w)) ;; treat as
                                               (js/Math.floor (* w pw-int))
                                               w)))))]
    (-> block
        (assoc :w (if (zero? w)
                    (let [h               (if (zero? h) 2 h)
                          possible-widths (for [y (range start-y (+ start-y h))]
                                            (loop [x start-x]
                                              (if (or (some #(point-inside-block? x y %) all-blocks)
                                                      (= x pw-int)) ; assuming 12 as the parent
                                                                    ; boundary
                                                x
                                                (recur (inc x)))))]
                      (- (apply min possible-widths) start-x))
                    w))
        (assoc :h (if (zero? h)
                    (let [w                (if (zero? w) 2 w)
                          possible-heights (for [x (range start-x (+ start-x w))]
                                             (loop [y start-y]
                                               (if (or (some #(point-inside-block? x y %)
                                                             all-blocks)
                                                       (= y ph-int)) ; assuming 12 as the parent
                                                                     ; boundary
                                                 y
                                                 (recur (inc y)))))]
                      (- (apply min possible-heights) start-y 0))
                    h)))))

(defn resolve-percents
  [block-map pw-int ph-int]
  (into
    {}
    (for [[k v] block-map]
      (let [x               (get-in v [:root 0])
            y               (get-in v [:root 1])
            w               (get v :w)
            h               (get v :h)
            round-up-half   (fn [num] (/ (js/Math.ceil (* 2 num)) 2))
            round-down-half (fn [x] x) ; (fn [num] (/ (js/Math.floor (* 2 num)) 2))
            h               (if (and (= h 0.99) (ut/is-float? h)) ;; override as 1.0
                              (js/Math.floor (* 1 ph-int))
                              h)
            w               (if (and (= w 0.99) (ut/is-float? w)) ;; override as 1.0
                              (js/Math.floor (* 1 pw-int))
                              w)
            x               (if (and (< x 1) (ut/is-float? x)) ;; treat as percent of total
                              (round-down-half (* x pw-int))
                              x)
            y               (if (and (< y 1) (ut/is-float? y)) ;; treat as percent of total
                              (round-down-half (* y ph-int))
                              y)
            h               (if (and (< h 1) (ut/is-float? h) (not (zero? h))) ;; treat as
                              (round-down-half (* h ph-int))
                              h)
            w               (if (and (< w 1) (ut/is-float? w) (not (zero? w))) ;; treat as
                              (round-down-half (* w pw-int))
                              w)]
        {k (-> v
               (assoc :root [x y])
               (assoc :h h)
               (assoc :w w))}))))

(defn process-layout
  [blocks-map pw-int ph-int]
  (let [ph-int   (- ph-int 1)
        bks      (reagent/atom {})
        statics  (into {}
                       (for [[k v] blocks-map
                             :when (and (> (get v :h) 1) (> (get v :w) 1))]
                         {k (assoc v :id k)}))
        percents (into {}
                       (for [[k v] blocks-map
                             :when (and (or (< (get v :h) 1) (< (get v :h) 1))
                                        (not (zero? (get v :h)))
                                        (not (zero? (get v :v))))]
                         {k (assoc v :id k)}))
        dyn-keys (vec (cset/difference (set (keys blocks-map))
                                       (set (into (keys statics) (keys percents)))))
        dyns     (select-keys blocks-map dyn-keys)
        res-dyn  (resolve-percents dyns pw-int ph-int)]
    (swap! bks merge (resolve-percents statics pw-int ph-int))
    (swap! bks merge (resolve-percents percents pw-int ph-int))
    (doseq [bv (reverse (sort-by #(apply + (get % :root)) (for [[k v] res-dyn] (merge v {:id k}))))]
      (let [id       (get bv :id)
            clean    bv ;(dissoc bv :id)
            resolved (compute-expanded-size id clean @bks pw-int ph-int)]
        (swap! bks merge {id resolved})))
    (into {}
          (for [[k v] @bks]
            (if (= (+ (get v :h) (get-in v [:root 1])) ph-int)
              {k (assoc v :h (- (get v :h) 0))}
              {k v})))))