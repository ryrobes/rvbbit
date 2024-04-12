(ns rvbbit-frontend.utility
  (:require [clojure.string :as cstr]
            [talltale.core :as tales]
            [clojure.walk :as walk]
            [reagent.core :as reagent]
            [re-frame.core :as re-frame]
            [re-com.util :refer [px]]
            [cljs.core.async :as async :refer [<! timeout]]
            [cljs.tools.reader :refer [read-string]]
            [re-com.validate :refer [string-or-hiccup? alert-type? vector-of-maps?]]
            [zprint.core :as zp])
  (:import
   [goog.i18n NumberFormat]
   [goog.i18n.NumberFormat Format]
   [goog.events EventType]))

(defn format-duration-seconds [seconds]
  (let [hours (int (/ seconds 3600))
        minutes (int (mod (/ seconds 60) 60))
        seconds (int (mod seconds 60))
        out (str (when (pos? hours) (str hours " hour" (when (> hours 1) "s") ", "))
                 (when (pos? minutes) (str minutes " minute" (when (> minutes 1) "s") ", "))
                 seconds " second" (when (> seconds 1) "s"))]
    (if (= out "0 second")
      "less than a second" out)))

(defn truncate-string
  "Truncates a string to a max-length and appends an ellipsis if it exceeds the length."
  [s max-length]
  (if (> (count s) max-length)
    (str (subs s 0 (dec max-length)) "…")
    s))

(defn deselect-keys [m ks]
  (apply dissoc m ks))

(def nff
  (NumberFormat. Format/DECIMAL))

(defn nf
  [num]
  (.format nff (str num)))

(defn is-hiccup? [x]
  (let [x1 (first x)]
    (and (vector? x)
         (or (= x1 :div)
             (= x1 :iframe)
             (= x1 :object)
             (= x1 :img)
             (= x1 :p)
             (= x1 :span)
             (= x1 :h1)
             (= x1 :h2)
             (= x1 :h3)))))

(def coord-cache (reagent/atom {}))

(defn is-float?
  [n] ;; cljs uses js "number" so core "float?" cant tell between int and float
  (and (number? n)
       (not= n (js/Math.floor n))))

(defn dispatch-delay
  [ms event]
  (async/go
    (<! (async/timeout ms))
    (re-frame/dispatch event)))

(defn client-name []
  (let [quals ["of-the" "hailing-from" "banned-from" "of" "exiled-from"]
        names [(tales/quality) (rand-nth [(tales/shape) (tales/color)]) (tales/animal) (rand-nth quals) (tales/landform)]]
    (keyword (clojure.string/replace (clojure.string/join "-" names) " " "-"))))

(defn make-tab-name []
  (str (str (rand-nth [(tales/shape) (tales/quality) (tales/color)]) " " (tales/animal))))

(defn has-nested-map-values? [x]
  (try (let [aa (some true?
                      (vec (if (and (or (list? x) (vector? x)) (not (empty? x)) (vector-of-maps? x))
                             (if (map? (first x))
                               (for [k (keys (first x))]
                                 (or (map?    (get (first x) k))
                                     (vector? (get (first x) k)))) false) false)))]
         (if aa true false)) (catch :default _ true)))

;; (defn unique-block-id-helper [base-name counter all-names]
;;   (let [new-name (if (= counter 0)
;;                    base-name
;;                    (str base-name "-" counter))]
;;     (if (contains? all-names new-name)
;;       (recur base-name (inc counter) all-names)
;;       new-name)))

;; ;; given a proposed id, and a list of existing ids, and a list of reserved names, return a unique id adding -1, -2, etc
;; (defn unique-block-id [proposed existing-keys reserved-names]
;;   (let [all-names (set (concat existing-keys reserved-names))]
;;     (unique-block-id-helper proposed 0 all-names)))

(defn unique-block-id-helper [base-name counter all-names]
  (loop [new-counter counter]
    (let [new-name (if (= new-counter 0)
                     base-name
                     (str base-name "-" new-counter))
          new-name (if (keyword? base-name) (keyword (cstr/replace (str new-name) #":" "")) new-name)]
      (if (contains? all-names new-name)
        (recur (inc new-counter))
        new-name))))

(defn unique-block-id [proposed existing-keys reserved-names]
  (let [all-names (set (concat existing-keys reserved-names))]
    (unique-block-id-helper proposed 0 all-names)))

(defn hex-to-rgb [hex]
  (mapv #(js/parseInt % 16)
        [(subs hex 1 3) (subs hex 3 5) (subs hex 5 7)]))

(defn luminance [rgb]
  (let [[r g b] (mapv #(double (/ % 255.0)) rgb)
        luminance (+ (* 0.2126 r) (* 0.7152 g) (* 0.0722 b))]
    luminance))

(defn choose-text-color [hex]
  (let [rgb (hex-to-rgb hex)
        luma (luminance rgb)]
    (if (> luma 0.5) "#000000" "#ffffff")))

(defn find-next [v k]
  (second (drop-while #(not= % k) v)))

(defn select-keypaths [m keys]
  (into {} (for [k keys] [k (get-in m k)])))

(defn sort-map-by-key [m]
  (sort-by first (into [] m)))

(re-frame/reg-sub
 ::safe-key
 (fn [db [_ proposed & [locals]]]
   (let [block-names (keys (get db :panels))
         locals (or locals [])
         incoming-keyword? (keyword? proposed)
         snapshot-names (keys (get-in db [:snapshots :params]))
         tab-names (get db :tabs)
         user-param-names (keys (get-in db [:click-param :param]))
         view-names (distinct (mapcat (fn [[_ v]] (keys (get v :views))) (get db :panels)))
         ;query-names (apply into (for [[_ v] (get db :panels)] (keys (get v :queries))))
         query-names (mapcat (fn [[_ v]] (keys (get v :queries))) (get db :panels)) ;; faster than apply into?
         all-keys (vec (apply concat [block-names user-param-names locals view-names snapshot-names tab-names query-names]))
         reco (unique-block-id proposed all-keys [])]
     ;;(tap> [:safe-key proposed all-keys :ret reco])
     (cond (and incoming-keyword? (keyword? reco)) reco
           (and incoming-keyword? (not (keyword? reco))) (keyword (cstr/replace (str reco) #":" ""))
           :else reco))))

(defn safe-key [proposed & [locals]] @(re-frame/subscribe [::safe-key proposed locals]))

(defn get-time-format-str []
  (cstr/join " " (drop 4 (drop-last 4 (cstr/split (str (js/Date.)) #" ")))))

(defn base64-to-uint8-array [base64]
  (let [binary-string (.atob js/window base64)
        len (.-length binary-string)
        bytes (js/Uint8Array. len)]
    (dotimes [i len]
      (aset bytes i (.charCodeAt binary-string i)))
    bytes))

(defn ne? [x]
  (if (seqable? x)
    (boolean (seq x))
    true))

(defn base64-to-blob [base64-content content-type]
  (let [byte-array (base64-to-uint8-array base64-content)
        options (clj->js {:type content-type})]
    (js/Blob. #js [byte-array] options)))

;; (defn data-typer [v]
;;   (cond (string? v) "string"
;;         (integer? v) "integer"
;;         (float? v) "float"
;;         (boolean? v) "boolean"
;;         (map? v) "map"
;;         (vector? v) "vector"
;;         (keyword v) "keyword"
;;         (list? v) "list"
;;         :else "?"))

(defn data-typer-fn [x] ;; exists in both block and db TODO - but they are different!!!
  (cond   (or (and (vector? x) (clojure.string/includes? (str x) "#object"))
              (and (vector? x) (fn? (first x))) ;; ?
              (and (vector? x) (cstr/starts-with? (str (first x)) ":re-com"))
              (and (vector? x) (cstr/starts-with? (str (first x)) ":vega"))
              (and (vector? x) (cstr/starts-with? (str (first x)) ":markdown"))
              (and (vector? x) (cstr/starts-with? (str (first x)) ":div"))
              (and (vector? x) (cstr/starts-with? (str (first x)) ":span"))
              (and (vector? x) (is-hiccup? x))) "render-object"
          (string? x) "string"
          (boolean? x) "boolean"
          ;(and (vector? x) (string-or-hiccup? x)) "hiccup"
          (or (and (or (list? x)
                       (vector? x))
                   (not (empty? x)) (every? map? x))
              (and (vector-of-maps? x)
                   (not (has-nested-map-values? x))))
          "rowset"  ;;; breaks shit? TODO
          (vector? x) "vector"
          (or (and (map? x)
                   (contains? x :classname)
                   (contains? x :subprotocol)
                   (contains? x :subname))
              (and (map? x)
                   (contains? x :dbtype)
                   (contains? x :dbname)
                   (contains? x :user))) "jdbc-conn"
          (map? x) "map"
          (list? x) "list"
          (nil? x) "nil"
          (int? x) "integer"
          (set? x) "set"
          (= (str (type x)) "cljs.core/LazySeq") "lazy"
          (= cljs.core/LazySeq (type x)) "lazy"
          (keyword? x) "keyword"
          (float? x) "float"
          ;(and (ifn? x) (not (cstr/includes? x "("))) "unquoted"
          (ifn? x) "function"
          :else "unknown"))

(def data-typer-atom (atom {}))

(defn data-typer [x]
  (let [;x (str [x1 x2 x3])
        cache (get @data-typer-atom x)]
    (if (not (nil? cache)) cache
        (let [deep (data-typer-fn x)]
          (swap! data-typer-atom assoc x deep)
          deep))))

;; (defn not-empty? [x] ;; fucking ppl who shit on (not (empty? x))
;;   (true? (not-empty x)))

(defn contains-data? [data target]
  (cond
    (map? data)
    (or (= data target)
        (some #(contains-data? % target) (vals data)))

    (coll? data)
    (or (= data target)
        (some #(contains-data? % target) data))

    :else
    (= data target)))

(defn asort [m order]
  (let [order-map (apply hash-map (interleave order (range)))]
    (conj
     (sorted-map-by #(compare (order-map %1) (order-map %2))) ; empty map with the desired ordering
     (select-keys m order))))

(defn unkeyword [x]
  (try ;; 'name' always causes me issues...
    (if (keyword? x)
      (-> (str x)
          (cstr/replace #":" "")
        ;(cstr/replace #"-" "_")
          )
      (str x))
    (catch :default _ (do (tap> [:error-in-unkeyword x])
                          (str x)))))

(defn template-replace [replacements s]
  (reduce (fn [str [key value]]
            (let [value (get :code value value)]
              (cstr/replace str (re-pattern (cstr/join ["\\{\\{" key "\\}\\}"])) value)))
          s
          replacements))

(defn hex-color? [s]
  (true? (and (string? s)
              (cstr/starts-with? (str s) "#"))))

;; (defn is-base64? [s]
;;   (if (string? s)
;;     (let [substring (subs s 0 (min 1000 (count s)))]
;;       (boolean (re-matches #"^(?:[A-Za-z0-9+/]{4})*(?:[A-Za-z0-9+/]{2}==|[A-Za-z0-9+/]{3}=)?$" substring)))
;;     false))


(defn bytes-to-mb [bytes]
  (let [mb (/ bytes 1048576.0)
        formatted-mb0 (-> mb
                          (.toFixed 0)
                          js/parseFloat
                          str
                          (.toLocaleString js/Intl.NumberFormat "en-US"))
        ;formatted-mb (* 1.8 formatted-mb0)
        ]
    ;(str (nf formatted-mb) "MB, (" (nf formatted-mb0) "MB)")
    (str (nf formatted-mb0) "MB")))

(defn is-base64-fn? [s]
  (if (and (string? s) (> (count s) 3000))
    (let [substring (subs s 0 3000)]
      (boolean (re-matches #"^(?:[A-Za-z0-9+/]{4})*(?:[A-Za-z0-9+/]{2}==|[A-Za-z0-9+/]{3}=)?$" substring)))
    false))

(def is-base64-atom (atom {}))

(defn is-base64? [s] ;; this is called a lot and semi-expensive
  (let [hx (hash s)
        cache (get @is-base64-atom hx)]
    (if (not (nil? cache)) cache
        (let [deep (is-base64-fn? s)]
          (swap! is-base64-atom assoc hx deep)
          deep))))

(def is-large-base64-atom (atom {}))

;; (defn replace-large-base64-fn [data]
;;   (cond
;;     (string? data) (if (and (is-base64? data) (> (count data) 3000)) "**huge base64 string**" data)
;;     (map? data) (into {} (map (fn [[k v]] [k (replace-large-base64-fn v)]) data))
;;     (coll? data) (mapv replace-large-base64-fn data)
;;     :else data))

(defn replace-large-base64-fn [data]
  (cond
    (string? data) (if (and (is-base64? data) (> (count data) 3000)) "**huge base64 string**" data)
    (map? data) (into {} (map (fn [[k v]] [k (replace-large-base64-fn v)]) data))
    (vector? data) (mapv replace-large-base64-fn data)
    (list? data) (doall (map replace-large-base64-fn data)) ;; dont want to covert lists to vectors like old version above!!
    :else data))

(defn replace-large-base64 [data]
  (let [hx (hash data)
        cache (get @is-large-base64-atom hx)]
    (if (not (nil? cache)) cache
        (let [deep (replace-large-base64-fn data)]
          (swap! is-large-base64-atom assoc hx deep)
          deep))))

(defn calculate-atom-sizes [atom-map]
  (into {}
        (for [[name a] atom-map]
          (try 
            (let [size-bytes (-> @a
                               pr-str
                               .-length)
                size-mb (/ size-bytes 1048576.0)]

            {name [size-bytes :bytes size-mb :mb (try (count (keys @a)) (catch :default _ -1)) :keys]})
            (catch :default e {name [:error (str e)]})))))

(defn template-find [s]
  (let [s (str s)
        pattern (re-pattern "\\{\\{:(\\S+?)\\}\\}")]
    (vec (map keyword (map (fn [match] (second match))
                           (re-seq pattern s))))))

(defn deep-template-find [data]
  (letfn [(recurse [item]
            (cond
              (string? item) (template-find item)
              (coll? item) (mapcat recurse item)
              :else []))]
    (vec (set (recurse data)))))

(defn deep-template-replace [replacements data]
  (letfn [(replace-in-str [s]
            (reduce (fn [sstr [key value]]
                      (let [value-str (str (get :code value value))] ; Convert value to string
                        (cstr/replace sstr (re-pattern (cstr/join ["\\{\\{" key "\\}\\}"])) value-str)))
                    s
                    replacements))]
    (walk/postwalk (fn [x] (if (string? x) (replace-in-str x) x)) data)))


;; (defn unkeyword [x]
;;   (try (str (name x)) (catch :default _
;;                         (cstr/replace (str x) #":" ""))))


(defn replacer-fn [x1 x2 x3]
  (let [res (try (cstr/replace (str x1) x2 x3)
                 (catch :default _ (do (when (not (nil? x1)) ;; no need to alert me of null
                                         (tap> [:string-replace-issue x1 x2 x3]))
                                       x1)))] res))

(def replacer-atom (atom {}))

(defn replacer [x1 x2 x3]
  (let [x (str [x1 x2 x3])
        cache (get @replacer-atom x)]
    (if (not (nil? cache)) cache
        (let [deep (replacer-fn x1 x2 x3)]
          (swap! replacer-atom assoc x deep)
          deep))))

(defn unkey [k]
  (let [s (try (name k)
               (catch :default _ (replacer (str k) #":" "")))]
    (if (keyword? s) ;; fails conversion (namespaced, etc)
      (replacer (str s) #":" "") s)))

(defn unre-qword [x]
  (if (keyword? x)
    (-> (str x)
        (cstr/replace #":" "")
        (cstr/replace #"query/" "")
        (keyword))
    (keyword x)))

(defn sql-keyword [x]
  (if (not (keyword? x))
    (-> (str x)
        (cstr/replace #":" "")
        (cstr/replace #"-" "_")
        keyword)
    x))

(def mod-letters ["aa" "bb" "cc" "dd" "ee" "ff" "gg" "hh" "ii" "jj" "kk" "mm"
                  "nn" "oo" "pp" "qq" "rr" "ss" "tt" "uu" "vv" "ww" "xx" "yy" "zz"])

(defn gen-sql-sql-alias []
  (let [rid (rand-int 999)]
    (keyword (str (rand-nth mod-letters) rid))))

(defn is-sql-sql-alias? [k]
  (true? (try (let [kk (str k)
                    letters (str (subs kk 1 3))
                    rid (subs kk 3 4)]
                (and (some #(= % letters) mod-letters)
                     (integer? (js/parseInt rid))))
              (catch :default _ false))))

;(let [kk (gen-sql-sql-alias)]
;  (tap> [:poop kk (is-sql-sql-alias? kk)]))

(defn read-string-preserve-floats [s]
  (-> s
      (clojure.string/replace #"(\d+\.\d+)" "FLOAT-$1-FLOAT") ; mark floats
      read-string
      (clojure.walk/postwalk
       (fn [item]
         (if (and (string? item) (clojure.string/starts-with? item "FLOAT-"))
           (-> item
               (clojure.string/replace-first "FLOAT-" "")
               (clojure.string/replace-first "-FLOAT" "")
               (js/parseFloat))
           item)))))

(defn rs-save-floats [s]
  (walk/postwalk-replace
   {:ONE-POINT-OH 1.0}
   (-> s
       (cstr/replace #"1.0" ":ONE-POINT-OH")
       read-string)))

;; (defn deep-remove-keys [data keys-to-remove]
;;   (let [key-remove-set (set keys-to-remove)]
;;     (cond
;;       (map? data)
;;       (->> data
;;            (reduce-kv (fn [acc k v]
;;                         (if (key-remove-set k)
;;                           acc
;;                           (assoc acc k (deep-remove-keys v keys-to-remove)))) {})
;;            (into (empty data)))

;;       (vector? data)
;;       (mapv (fn [elem] (deep-remove-keys elem keys-to-remove)) data)

;;       :else data)))

(defn get-parent-z-index [event]
  (let [element (.-target event) ;; Get the target element of the event
        parent-element (.-parentNode element) ;; Get the parent of the target element
        computed-style (js/getComputedStyle parent-element) ;; Get the computed style of the parent element
        z-index (.-zIndex computed-style)] ;; Extract the z-index from the computed style
    (tap> [:parent-z-index z-index]) ;; Print or use the z-index as needed
    z-index)) ;; Return the z-index

(defn deep-remove-keys [data keys-to-remove]
  (let [key-remove-set (set keys-to-remove)]
    (cond
      (map? data)
      (->> data
           (reduce-kv (fn [acc k v]
                        (if (or (key-remove-set k)
                                (and (keyword? k) (cstr/starts-with? (name k) "_")))
                          acc
                          (assoc acc k (deep-remove-keys v keys-to-remove)))) {})
           (into (empty data)))

      (vector? data)
      (mapv (fn [elem] (deep-remove-keys elem keys-to-remove)) data)

      :else data)))

(defn clean-sql-from-ui-keys-fn [query]
  (let [res (deep-remove-keys query [:cache? :col-widths :row-height :render-all? :refresh-every :page :connection-id
                                     :deep-meta? :clicked-row-height :style-rules])]
    res))

(defn update-nested-tabs [m old new]
  (into {}
        (map (fn [[k v]]
               (if (and (map? v) (= (:tab v) old))
                 [k (assoc v :tab new)]
                 [k v]))
             m)))

(defn update-multiple [orig-map updates]
  (reduce (fn [acc-map [keypath value]]
            (assoc-in acc-map keypath value))
          orig-map
          updates))

(defn update-multiple-if-exists [orig-map updates]
  (reduce (fn [acc-map [keypath value]]
            (if (not (nil? (get-in acc-map keypath)))
              (assoc-in acc-map keypath value)
              acc-map))
          orig-map
          updates))

(defn is-hex-color? [s]
  (and (string? s) (cstr/starts-with? s "#")
       (or (= (count s) 4) (= (count s) 7) (= (count s) 9))))

(defn invert-hex-color [hex]
  (let [rgb (js/parseInt (subs hex 1) 16)
        r (bit-and (bit-shift-right rgb 16) 255)
        g (bit-and (bit-shift-right rgb 8) 255)
        b (bit-and rgb 255)
        inverted-r (js/Number.prototype.toString.call (- 255 r) 16)
        inverted-g (js/Number.prototype.toString.call (- 255 g) 16)
        inverted-b (js/Number.prototype.toString.call (- 255 b) 16)
        pad (fn [s] (if (< (count s) 2) (str "0" s) s))]
    (str "#" (pad inverted-r) (pad inverted-g) (pad inverted-b))))




;; runstream below


(def clean-sql-atom (atom {}))

(defn clean-sql-from-ui-keys [x] ; <-- gets called hundreds of times
  (let [hx (hash x)
        cache (get @clean-sql-atom hx)]
    (if cache cache (let [deep (clean-sql-from-ui-keys-fn x)]
                      (swap! clean-sql-atom assoc hx deep)
                      deep))))

(def auto-font-atom (atom {}))

(defn pad-left [s len pad-char]
  (str (apply str (repeat (max 0 (- len (count s))) pad-char)) s))

(defn today []
  (let [d (js/Date.)]
    (str (.getFullYear d) "-"
         (pad-left (str (inc (.getMonth d))) 2 "0") "-"
         (pad-left (str (.getDate d)) 2 "0"))))

(defn auto-font-size-px-fn [value h w]
  (let [brick-size 50
        ;w (- w 2)
        ;h 300 w 400
        ;w (- w 0.3)
        ;label         (nth fields 0)
        md            (data-typer value) ;(get-in metadata [:fields label :data-type])
        num?          (or (= md "integer") (= md "float"))
        ;value         (get-in @rowset [0 label])
        formatted-val (if num? (str (nf value)) (str value))
        len           (count (str formatted-val))
        ;len           (if (= len 1) 1.3 len)
        charw         (js/Math.ceil (/ (* (js/Math.ceil (/ (+ h w 1) 2)) brick-size) len))
        pxsize        (px charw)
        ;labelsz       (* height-int 0.1)
        ;agg?          (not (get-in metadata [:fields label :group-by?]))
        ;pxlabel       (px (if (< labelsz 20) 20 labelsz))
        ]
    ;(tap> [:auto-font pxsize value h w])
    pxsize))

(defn auto-font-size-px [value h w]
  (let [hx [value h w]
        cache (get @auto-font-atom hx)]
    (if cache cache (let [deep (auto-font-size-px-fn value h w)]
                      (swap! auto-font-atom assoc hx deep)
                      deep))))

(defn auto-font-size-fn [value h w]
  (let [brick-size 50
        ;w (- w 2)
        ;h 300 w 400
        ;w (- w 0.3)
        ;label         (nth fields 0)
        md            (data-typer value) ;(get-in metadata [:fields label :data-type])
        num?          (or (= md "integer") (= md "float"))
        ;value         (get-in @rowset [0 label])
        formatted-val (if num? (str (nf value)) (str value))
        len           (count (str formatted-val))
        ;len           (if (= len 1) 1.3 len)
        charw         (js/Math.ceil (/ (* (js/Math.ceil (/ (+ h w 1) 2)) brick-size) len))
        ;pxsize        (px charw)
        ;labelsz       (* height-int 0.1)
        ;agg?          (not (get-in metadata [:fields label :group-by?]))
        ;pxlabel       (px (if (< labelsz 20) 20 labelsz))
        ]
    ;(tap> [:auto-font pxsize value h w])
    charw))

(defn auto-font-size [value h w]
  (let [hx [value h w "int"]
        cache (get @auto-font-atom hx)]
    (if cache cache (let [deep (auto-font-size-fn value h w)]
                      (swap! auto-font-atom assoc hx deep)
                      deep))))

(defn min-at-least-pos [coords]
  (reduce (fn [acc coord] (if (or (< (first coord) (first acc))
                                  (and (= (first coord) (first acc))
                                       (< (second coord) (second acc))))
                            coord
                            acc))
          coords))

(defn not-empty?
  [coll]
  (try (boolean (seq coll)) (catch :default e
                              (do (tap> [:no-empty-failed coll e])
                                  false))))

(defn canvas-size [rects]
  (reduce (fn [[max-x max-y] [x y h w]]
            [(max max-x (+ x w)) (max max-y (+ y h))])
          ;corner
          [0 0]
          rects))

(defn remove-punctuation [s]
  (try
    ;(cstr/replace s #"[^\p{Alpha}\p{Space}]" "")
    ;(cstr/replace s #"[\\p{Punct}]+" "")
    ;s
    (-> s
        (cstr/replace "." "")
        (cstr/replace "," "")
        (cstr/replace "?" "")
        (cstr/replace "!" ""))
    (catch :default _ s)))

(defn map->css [m]
  (cstr/join "\n"
             (map (fn [[selector styles]]
                    (str selector " { "
                         (cstr/join " "
                                    (map (fn [[prop val]]
                                           (str (cstr/replace (name prop) #"_" "-") ": " val ";")) styles))
                         "}"))
                  m)))

(defn update-theme-css [css-string]
  (let [style-id "custom-codemirror-theme"
        style-element (.getElementById js/document style-id)]
    (if style-element
      (set! (.-innerHTML style-element) css-string)
      (let [head (.querySelector js/document "head")
            style-element (js/document.createElement "style")]
        (set! (.-id style-element) style-id)
        (set! (.-type style-element) "text/css")
        (set! (.-innerHTML style-element) css-string)
        (.appendChild head style-element)))))

(defn apply-theme [theme-map]
  (let [css-string (map->css theme-map)]
    (update-theme-css css-string)))

(defn vectorized-case [test-seq]
  (let [expr (first test-seq)
        clauses (butlast (rest test-seq))
        else-result (last test-seq)
        conditions (partition 2 clauses)]
    (or (some (fn [[condition result]]
                (when (= expr condition)
                  result))
              conditions)
        else-result)))

(defn all-uppers? [s]
  (= s (cstr/upper-case s)))

(defn dissoc-in [map-in keypath]
  (let [base-kp (pop keypath)
        last-kp (last keypath)]
    (update-in map-in base-kp dissoc last-kp)))

;; (defn deep-flatten [x]
;;   (if (coll? x)
;;     (mapcat deep-flatten x)
;;     [x]))

(defn deep-flatten-real [x]
  (if (coll? x)
    (mapcat deep-flatten-real x)
    [x]))

(defonce deep-flatten-atom (atom {}))

(defn deep-flatten [x]
  (let [hx (hash x)
        cache (get @deep-flatten-atom hx)]
    (if cache cache (let [deep (deep-flatten-real x)]
                      (swap! deep-flatten-atom assoc hx deep)
                      deep))))

(def format-map-atom (atom {}))

(defn format-map [w s]
  (let [cache (get @format-map-atom [w s])]
    (if (not (nil? cache))
      cache
      (let [;s (cstr/replace s #"1.0" "\"ONE-POINT-ZERO\"")
            type (cond (cstr/includes? s "(") :community
                       (cstr/starts-with? (cstr/trim-newline s) "[:") :hiccup
                       :else :justified)
            o (zp/zprint-str s (js/Math.floor (/ w 9))
                             {:parse-string? true ;; was :parse-string-all?
                             ; :style :justified-original ;; cool but SLOOOOOOWWWWW
                              :style type ;:community ;[:binding-nl :extend-nl] ;:community
                              :pair {:force-nl? true}
                              :pair-fn {:hang? true}
                              :binding {:force-nl? true}
                              ;:style :hiccup
                              ;:style :keyword-respect-nl
                              ;:fn-map {:double :just-float}
                              :vector {:respect-nl? true}
                              :map {:comma? false
                                    :sort? false}
                              :parse {:interpose "\n\n"}})]
        (swap! format-map-atom assoc [w s] o)
        o))))

(def body-set-atom (atom {}))

(defn body-set-fn [body] ;; expensive
  (set (filter keyword? (deep-flatten body))))

(defn body-set [body]
  (let [hx (hash body)
        cache (get @body-set-atom hx)]
    (if cache cache
        (let [deep (body-set-fn body)]
          (swap! body-set-atom assoc hx deep)
          deep))))

(defn deep-remove-nil-values [m]
  (into {}
        (map (fn [[k v]]
               [k (cond
                    (map? v) (deep-remove-nil-values v)
                    :else v)])
             (filter (fn [[_ v]] (not (nil? v))) m))))

(defn cm-deep-values [s]
  (vec (flatten (into []
                      (for [c (range (count (.-children (.-doc s))))]
                        (into []
                              (for [line (.-lines (aget (.-children (.-doc s)) c))]
                                (.-text line))))))))

(defn kvpaths
  ([m] (kvpaths [] m ()))
  ([prev m result]
   (reduce-kv (fn [res k v] (if (associative? v)
                              (let [kp (conj prev k)]
                                (kvpaths kp v (conj res kp)))
                              (conj res (conj prev k))))
              result
              m)))

(defn kvpaths2
  ([m] (kvpaths [] m ()))
  ([prev m result]
   (reduce-kv (fn [res k v] (if (or (map? v) (vector? v) (list? v)) ;(associative? v)
                              (let [kp (conj prev k)]
                                (kvpaths kp v (conj res kp)))
                              (conj res (conj prev k))))
              result
              m)))

(defn kvpaths3
  ([m] (kvpaths3 [] m []))
  ([prev m result]
   (reduce-kv
    (fn [res k v]
      (let [new-prev (conj prev k)]
        (cond
          (and (associative? k) (associative? v)) (concat res (kvpaths3 new-prev k []) (kvpaths3 new-prev v []))
          (associative? k) (concat res (kvpaths3 new-prev k []))
          (associative? v) (concat res (kvpaths3 new-prev v []))
          :else (conj res new-prev))))
    result
    m)))

(defn unpack-keys [m]
  (let [unpack-vec (fn [v] (into {} (map-indexed (fn [idx x] [(str ":key" idx) x]) v)))]
    (reduce-kv
     (fn [acc k v]
       (cond
         (vector? k) (merge acc (unpack-vec k) {[k] (if (associative? v) (unpack-keys v) v)})
         (associative? v) (assoc acc k (unpack-keys v))
         :else (assoc acc k v)))
     {}
     m)))



;; (defn replace-namespaced-key [source-ns target-ns item]
;;   (let [pattern (re-pattern (str ":\\*" source-ns "\\*/.*"))]
;;     (if (and (keyword? item)
;;              (re-matches pattern (pr-str item)))
;;         (keyword target-ns (name item))
;;         item)))


(defn replace-namespaced-keyv [source target item]
  (let [pattern-ns (re-pattern (str ":\\*" source "\\*/.*"))
        pattern-name (re-pattern (str ":view/.*" source ".*"))]
    (cond
      (and (keyword? item) (re-matches pattern-ns (pr-str item)))
      (keyword target (name item))

      (and (keyword? item) (re-matches pattern-name (pr-str item)))
      (let [new-name (str target "." (second (clojure.string/split (name item) #"\.")))]
        (keyword (namespace item) new-name))

      :else
      item)))

(defn namespaced-swapper [source-ns target-ns data] ;; back here for ->> usage in honeycomb
  (walk/postwalk (partial replace-namespaced-keyv source-ns target-ns) data))

(defn keywordify ;; same as server
  [s]
  (let [formatted-value (cond (nil? s) "is_null"  ;; handle nil
                              (number? s) (str s) ;; handle numbers by
                                                  ;; converting them to strings
                              :else s)]  ;; handle all other cases
    (-> formatted-value
        (cstr/lower-case)
        (cstr/replace " " "_")
        (cstr/replace "-" "_")
        keyword)))

(defn remove-key ;; for sql select field and aliased fields
  [coll key]
  (vec
   (mapcat (fn [item]
             (cond (keyword? item)
                   (when (not= key item)
                     [item])
                   (vector? item)
                   (let [last-in-vector (last item)]
                     (if (and (keyword? last-in-vector)
                              (= key last-in-vector))
                       []
                       [item]))
                   :else [item]))
           coll)))

(defn matches-pattern? [item kw num]
  ;(or ;(and (vector? item) (= (count item) 3) (= (first item) :=))
  ;    ;(and (vector? item) (= (count item) 3) (= (first item) :when))
  (and (vector? item) (= (count item) num) (= (first item) kw))
  ;    ;(and (vector? item) (= (count item) 3) (= (first item) :set-parameter))
  ;    )
  )

(defn extract-patterns [data kw num]
  (let [matches (atom [])]
    (walk/prewalk
     (fn [item]
       (when (matches-pattern? item kw num)
         (swap! matches conj item))
       item)
     data)
    @matches))

(defn keypaths
  ([m] (keypaths [] m ()))
  ([prev m result]
   (reduce-kv (fn [res k v] (if (associative? v)
                              (keypaths (conj prev k) v res)
                              (conj res (conj prev k))))
              result
              m)))

(defn sanitize-name [name] ;; keep updated in server also
  (-> name ;; chars that make file and folder operations annoying
      (clojure.string/replace  " " "_")
      (clojure.string/replace  "<" "_")
      (clojure.string/replace  ">" "_")
      (clojure.string/replace  "#" "_")
      (clojure.string/replace  "$" "_")
      (clojure.string/replace  "+" "_")
      (clojure.string/replace  "%" "_")
      (clojure.string/replace  "!" "_")
      (clojure.string/replace  "`" "_")
      (clojure.string/replace  "&" "_")
      (clojure.string/replace  "*" "_")
      (clojure.string/replace  "'" "_")
      (clojure.string/replace  "|" "_")
      (clojure.string/replace  "{" "_")
      (clojure.string/replace  "}" "_")
      (clojure.string/replace  "?" "_")
      (clojure.string/replace  "," "_")
      (clojure.string/replace  (str (char 34)) "_") ;; double quote
      (clojure.string/replace  "=" "_")
      (clojure.string/replace  "/" "_")
      (clojure.string/replace  ":" "_")
      (clojure.string/replace  "@" "_")
      (clojure.string/replace  "[" "_")
      (clojure.string/replace  "]" "_")
      (clojure.string/replace  "\\" "_")))

(defn keypath-munger [kp]
  (let [nkp (sanitize-name
             (cstr/replace (cstr/join "_"
                                      kp) ":" ""))]
    (-> nkp
        (cstr/replace "-" "_")
        (cstr/replace "." "_"))))

(defn set-and-reset! [atom value delay-ms]
  (async/go
    (reset! atom value)
    (<! (async/timeout delay-ms))
    (reset! atom nil)))

(defn curved-path-h [x1 y1 x2 y2]
  (let [mx (+ x1 (/ (- x2 x1) 2))
        line ["M" x1 y1 "C" mx y1 mx y2 x2 y2]]
    (cstr/join " " line)))

(defn curved-path-v0 [x1 y1 x2 y2]
  (let [my (+ y1 (/ (- y2 y1) 2))
        line ["M" x1 y1 "C" x1 my x2 my x2 y2]]
    (cstr/join " " line)))

(defn curved-path-v1 [x1 y1 x2 y2]
  (let [straight-segment-length 8
        ;; Calculate the start and end points for the straight segments
        y1-straight (+ y1 straight-segment-length)
        y2-straight (- y2 straight-segment-length)
        ;; Define the path with the new segments
        line ["M" x1 y1
              "L" x1 y1-straight
              "C" x1 (+ y1-straight (/ (- y2-straight y1-straight) 2))
              x2 (+ y1-straight (/ (- y2-straight y1-straight) 2))
              x2 y2-straight
              "L" x2 y2]]
    (cstr/join " " line)))






;; (defn curved-path-v [x1 y1 x2 y2]
;;   (if (< (- y2 y1) 50)
;;     (curved-path-v0 x1 y1 x2 y2)
;;     (curved-path-v1 x1 y1 x2 y2)))

;; (defn curved-path-v3 [x1 y1 x2 y2]
;;   (let [mid-x (+ x1 (/ (- x2 x1) 2))
;;         control-y1 (- y1 -30)
;;         control-y2 (+ y2 -30)
;;         line ["M" x1 y1
;;               "C" x1 control-y1 mid-x control-y1 mid-x y1
;;               "C" mid-x control-y2 x2 control-y2 x2 y2]]
;;     (cstr/join " " line)))


(defn curved-path-v3a [x1 y1 x2 y2]
  (let [mid-x (+ x1 (/ (- x2 x1) 2))
        control-y1 (+ y1 40) ; Control point for the first curve should be below y1
        control-y2 (- y2 40) ; Control point for the second curve should be above y2
        mid-y1 (+ y1 20)     ; Middle point for the first curve should be below y1
        mid-y2 (- y2 20)     ; Middle point for the second curve should be above y2
        line ["M" x1 y1
              "C" x1 control-y1 mid-x control-y1 mid-x mid-y1
              "S" mid-x control-y2 mid-x mid-y2
              "C" mid-x control-y2 x2 control-y2 x2 y2]]
    (cstr/join " " line)))

(defn curved-path-v3b [x1 y1 x2 y2]
  (let [dx (Math/abs (- x2 x1))
        dy (Math/abs (- y2 y1))
        curve-depth (min dy (/ dx 2)) ; The depth is the smaller of dy and half of dx
        control-y1 (+ y1 curve-depth) ; Control point for the first curve
        control-y2 (- y2 curve-depth) ; Control point for the second curve
        mid-x (+ x1 (/ dx 2)) ; Midpoint on the x-axis
        line ["M" x1 y1
              "C" x1 control-y1 mid-x control-y1 mid-x y1
              "C" mid-x control-y2 x2 control-y2 x2 y2]]
    (cstr/join " " line)))

(defn curved-path-v3 [x1 y1 x2 y2]
  (let [dx (Math/abs (- x2 x1))
        dy (Math/abs (- y2 y1))
        arc-radius (if (< dy 30) (/ dx 4) (/ dx 2)) ; Smaller arc for smaller dy, larger otherwise
        control-y1 (+ y1 arc-radius) ; Control point for the first curve
        control-y2 (- y2 arc-radius) ; Control point for the second curve
        mid-x (+ x1 (/ dx 2)) ; Midpoint on the x-axis
        line ["M" x1 y1
              "C" x1 control-y1 mid-x control-y1 mid-x y1
              "C" mid-x control-y2 x2 control-y2 x2 y2]]
    (cstr/join " " line)))

(defn curved-path-v [x1 y1 x2 y2]
  (cond
    (> y1 y2) (curved-path-v3 x1 y1 x2 y2)
    (< (- y2 y1) 50) (curved-path-v0 x1 y1 x2 y2)
    :else (curved-path-v1 x1 y1 x2 y2)))


;; (defn curved-path-v3 [x1 y1 x2 y2 ]
;;   (let [
;;         straight-segment-length 10
;;         mid-y (+ y1 (/ (- y2 y1) 2))
;;         control-point-offset (Math/min straight-segment-length (/ (Math/abs (- y2 y1)) 2))
;;         control-y1 (+ y1 control-point-offset)
;;         control-y2 (- y2 control-point-offset)
;;         ;; The path will start with a straight line segment
;;         start-straight-line ["M" x1 y1 "L" x1 control-y1]
;;         ;; The cubic Bezier curve for the transition
;;         bezier-curve ["C" x1 (+ control-y1 control-point-offset) x2 (- control-y2 control-point-offset) x2 control-y2]
;;         ;; The final straight line segment
;;         end-straight-line ["L" x2 y2]
;;         ;; Combine all parts of the path
;;         line (concat start-straight-line bezier-curve end-straight-line)]
;;     (cstr/join " " line)))


;; (defn curved-path-v4 [x1 y1 x2 y2 ]
;;   (let [straight-segment-length 10
;;         dy (Math/abs (- y2 y1))
;;         straight-seg-adjust (Math/min straight-segment-length (/ dy 4))
;;         ;; Calculate the positions where the straight segments end and the curves begin
;;         y1-end-straight (+ y1 straight-seg-adjust)
;;         y2-start-straight (- y2 straight-seg-adjust)
;;         ;; Calculate the control points for the Bezier curves
;;         y1-control (+ y1-end-straight straight-seg-adjust)
;;         y2-control (- y2-start-straight straight-seg-adjust)
;;         mid-y1 (+ y1-control (/ (- y2-control y1-control) 3))
;;         mid-y2 (- y2-control (/ (- y2-control y1-control) 3))
;;         ;; Define the path with the straight segments and Bezier curves
;;         line ["M" x1 y1 "L" x1 y1-end-straight
;;               "C" x1 y1-control x1 mid-y1 x1 (+ y1-control (/ (- mid-y1 y1-control) 2))
;;               "C" x1 mid-y1 x1 mid-y2 x1 (+ mid-y1 (/ (- mid-y2 mid-y1) 2))
;;               "C" x1 mid-y2 x1 y2-control x1 (+ mid-y2 (/ (- y2-control mid-y2) 2))
;;               "C" x1 y2-control x2 y2-control x2 y2-start-straight
;;               "L" x2 y2]]
;;     (cstr/join " " line)))


(defn stepped-path-h [x1 y1 x2 y2]
  (let [mx (+ x1 (/ (- x2 x1) 2))
        line ["M" x1 y1 "L" mx y1 "L" mx y2 "L" x2 y2]]
    (cstr/join " " line)))

(defn stepped-path-v [x1 y1 x2 y2]
  (let [;mx (+ x1 (/ (- x2 x1) 2))
        my (+ y1 (/ (- y2 y1) 2))
        line ["M" x1 y1 "L" x1 my "L" x2 my "L" x2 y2]
        ;line0  ["M" x1 y1 "L" mx y1 "L" mx y2 "L" x2 y2]
        ]
    (cstr/join " " line)))

(defn read-file [file callback] ;; annoying because FileReader is async
  (let [js-file-reader (js/FileReader.)]
    (set! (.-onload js-file-reader)
          (fn [evt]
            (let [result (-> evt .-target .-result)]
              (callback result))))
    (.readAsText js-file-reader file)))

(defn mapply [f & args]
  (apply f  (apply concat (butlast args) (last args))))

(def colorbrewer
  {:YlGn
   {:3 ["rgb(247,252,185)" "rgb(173,221,142)" "rgb(49,163,84)"],
    :4
    ["rgb(255,255,204)"
     "rgb(194,230,153)"
     "rgb(120,198,121)"
     "rgb(35,132,67)"],
    :5
    ["rgb(255,255,204)"
     "rgb(194,230,153)"
     "rgb(120,198,121)"
     "rgb(49,163,84)"
     "rgb(0,104,55)"],
    :6
    ["rgb(255,255,204)"
     "rgb(217,240,163)"
     "rgb(173,221,142)"
     "rgb(120,198,121)"
     "rgb(49,163,84)"
     "rgb(0,104,55)"],
    :7
    ["rgb(255,255,204)"
     "rgb(217,240,163)"
     "rgb(173,221,142)"
     "rgb(120,198,121)"
     "rgb(65,171,93)"
     "rgb(35,132,67)"
     "rgb(0,90,50)"],
    :8
    ["rgb(255,255,229)"
     "rgb(247,252,185)"
     "rgb(217,240,163)"
     "rgb(173,221,142)"
     "rgb(120,198,121)"
     "rgb(65,171,93)"
     "rgb(35,132,67)"
     "rgb(0,90,50)"],
    :9
    ["rgb(255,255,229)"
     "rgb(247,252,185)"
     "rgb(217,240,163)"
     "rgb(173,221,142)"
     "rgb(120,198,121)"
     "rgb(65,171,93)"
     "rgb(35,132,67)"
     "rgb(0,104,55)"
     "rgb(0,69,41)"],
    :type "seq"},
   :Spectral
   {:11
    ["rgb(158,1,66)"
     "rgb(213,62,79)"
     "rgb(244,109,67)"
     "rgb(253,174,97)"
     "rgb(254,224,139)"
     "rgb(255,255,191)"
     "rgb(230,245,152)"
     "rgb(171,221,164)"
     "rgb(102,194,165)"
     "rgb(50,136,189)"
     "rgb(94,79,162)"],
    :10
    ["rgb(158,1,66)"
     "rgb(213,62,79)"
     "rgb(244,109,67)"
     "rgb(253,174,97)"
     "rgb(254,224,139)"
     "rgb(230,245,152)"
     "rgb(171,221,164)"
     "rgb(102,194,165)"
     "rgb(50,136,189)"
     "rgb(94,79,162)"],
    :4
    ["rgb(215,25,28)"
     "rgb(253,174,97)"
     "rgb(171,221,164)"
     "rgb(43,131,186)"],
    :type "div",
    :7
    ["rgb(213,62,79)"
     "rgb(252,141,89)"
     "rgb(254,224,139)"
     "rgb(255,255,191)"
     "rgb(230,245,152)"
     "rgb(153,213,148)"
     "rgb(50,136,189)"],
    :8
    ["rgb(213,62,79)"
     "rgb(244,109,67)"
     "rgb(253,174,97)"
     "rgb(254,224,139)"
     "rgb(230,245,152)"
     "rgb(171,221,164)"
     "rgb(102,194,165)"
     "rgb(50,136,189)"],
    :9
    ["rgb(213,62,79)"
     "rgb(244,109,67)"
     "rgb(253,174,97)"
     "rgb(254,224,139)"
     "rgb(255,255,191)"
     "rgb(230,245,152)"
     "rgb(171,221,164)"
     "rgb(102,194,165)"
     "rgb(50,136,189)"],
    :5
    ["rgb(215,25,28)"
     "rgb(253,174,97)"
     "rgb(255,255,191)"
     "rgb(171,221,164)"
     "rgb(43,131,186)"],
    :3 ["rgb(252,141,89)" "rgb(255,255,191)" "rgb(153,213,148)"],
    :6
    ["rgb(213,62,79)"
     "rgb(252,141,89)"
     "rgb(254,224,139)"
     "rgb(230,245,152)"
     "rgb(153,213,148)"
     "rgb(50,136,189)"]},
   :Paired
   {:12
    ["rgb(166,206,227)"
     "rgb(31,120,180)"
     "rgb(178,223,138)"
     "rgb(51,160,44)"
     "rgb(251,154,153)"
     "rgb(227,26,28)"
     "rgb(253,191,111)"
     "rgb(255,127,0)"
     "rgb(202,178,214)"
     "rgb(106,61,154)"
     "rgb(255,255,153)"
     "rgb(177,89,40)"],
    :11
    ["rgb(166,206,227)"
     "rgb(31,120,180)"
     "rgb(178,223,138)"
     "rgb(51,160,44)"
     "rgb(251,154,153)"
     "rgb(227,26,28)"
     "rgb(253,191,111)"
     "rgb(255,127,0)"
     "rgb(202,178,214)"
     "rgb(106,61,154)"
     "rgb(255,255,153)"],
    :10
    ["rgb(166,206,227)"
     "rgb(31,120,180)"
     "rgb(178,223,138)"
     "rgb(51,160,44)"
     "rgb(251,154,153)"
     "rgb(227,26,28)"
     "rgb(253,191,111)"
     "rgb(255,127,0)"
     "rgb(202,178,214)"
     "rgb(106,61,154)"],
    :4
    ["rgb(166,206,227)"
     "rgb(31,120,180)"
     "rgb(178,223,138)"
     "rgb(51,160,44)"],
    :type "qual",
    :7
    ["rgb(166,206,227)"
     "rgb(31,120,180)"
     "rgb(178,223,138)"
     "rgb(51,160,44)"
     "rgb(251,154,153)"
     "rgb(227,26,28)"
     "rgb(253,191,111)"],
    :8
    ["rgb(166,206,227)"
     "rgb(31,120,180)"
     "rgb(178,223,138)"
     "rgb(51,160,44)"
     "rgb(251,154,153)"
     "rgb(227,26,28)"
     "rgb(253,191,111)"
     "rgb(255,127,0)"],
    :9
    ["rgb(166,206,227)"
     "rgb(31,120,180)"
     "rgb(178,223,138)"
     "rgb(51,160,44)"
     "rgb(251,154,153)"
     "rgb(227,26,28)"
     "rgb(253,191,111)"
     "rgb(255,127,0)"
     "rgb(202,178,214)"],
    :5
    ["rgb(166,206,227)"
     "rgb(31,120,180)"
     "rgb(178,223,138)"
     "rgb(51,160,44)"
     "rgb(251,154,153)"],
    :3 ["rgb(166,206,227)" "rgb(31,120,180)" "rgb(178,223,138)"],
    :6
    ["rgb(166,206,227)"
     "rgb(31,120,180)"
     "rgb(178,223,138)"
     "rgb(51,160,44)"
     "rgb(251,154,153)"
     "rgb(227,26,28)"]},
   :Set2
   {:3 ["rgb(102,194,165)" "rgb(252,141,98)" "rgb(141,160,203)"],
    :4
    ["rgb(102,194,165)"
     "rgb(252,141,98)"
     "rgb(141,160,203)"
     "rgb(231,138,195)"],
    :5
    ["rgb(102,194,165)"
     "rgb(252,141,98)"
     "rgb(141,160,203)"
     "rgb(231,138,195)"
     "rgb(166,216,84)"],
    :6
    ["rgb(102,194,165)"
     "rgb(252,141,98)"
     "rgb(141,160,203)"
     "rgb(231,138,195)"
     "rgb(166,216,84)"
     "rgb(255,217,47)"],
    :7
    ["rgb(102,194,165)"
     "rgb(252,141,98)"
     "rgb(141,160,203)"
     "rgb(231,138,195)"
     "rgb(166,216,84)"
     "rgb(255,217,47)"
     "rgb(229,196,148)"],
    :8
    ["rgb(102,194,165)"
     "rgb(252,141,98)"
     "rgb(141,160,203)"
     "rgb(231,138,195)"
     "rgb(166,216,84)"
     "rgb(255,217,47)"
     "rgb(229,196,148)"
     "rgb(179,179,179)"],
    :type "qual"},
   :PuBu
   {:3 ["rgb(236,231,242)" "rgb(166,189,219)" "rgb(43,140,190)"],
    :4
    ["rgb(241,238,246)"
     "rgb(189,201,225)"
     "rgb(116,169,207)"
     "rgb(5,112,176)"],
    :5
    ["rgb(241,238,246)"
     "rgb(189,201,225)"
     "rgb(116,169,207)"
     "rgb(43,140,190)"
     "rgb(4,90,141)"],
    :6
    ["rgb(241,238,246)"
     "rgb(208,209,230)"
     "rgb(166,189,219)"
     "rgb(116,169,207)"
     "rgb(43,140,190)"
     "rgb(4,90,141)"],
    :7
    ["rgb(241,238,246)"
     "rgb(208,209,230)"
     "rgb(166,189,219)"
     "rgb(116,169,207)"
     "rgb(54,144,192)"
     "rgb(5,112,176)"
     "rgb(3,78,123)"],
    :8
    ["rgb(255,247,251)"
     "rgb(236,231,242)"
     "rgb(208,209,230)"
     "rgb(166,189,219)"
     "rgb(116,169,207)"
     "rgb(54,144,192)"
     "rgb(5,112,176)"
     "rgb(3,78,123)"],
    :9
    ["rgb(255,247,251)"
     "rgb(236,231,242)"
     "rgb(208,209,230)"
     "rgb(166,189,219)"
     "rgb(116,169,207)"
     "rgb(54,144,192)"
     "rgb(5,112,176)"
     "rgb(4,90,141)"
     "rgb(2,56,88)"],
    :type "seq"},
   :GnBu
   {:3 ["rgb(224,243,219)" "rgb(168,221,181)" "rgb(67,162,202)"],
    :4
    ["rgb(240,249,232)"
     "rgb(186,228,188)"
     "rgb(123,204,196)"
     "rgb(43,140,190)"],
    :5
    ["rgb(240,249,232)"
     "rgb(186,228,188)"
     "rgb(123,204,196)"
     "rgb(67,162,202)"
     "rgb(8,104,172)"],
    :6
    ["rgb(240,249,232)"
     "rgb(204,235,197)"
     "rgb(168,221,181)"
     "rgb(123,204,196)"
     "rgb(67,162,202)"
     "rgb(8,104,172)"],
    :7
    ["rgb(240,249,232)"
     "rgb(204,235,197)"
     "rgb(168,221,181)"
     "rgb(123,204,196)"
     "rgb(78,179,211)"
     "rgb(43,140,190)"
     "rgb(8,88,158)"],
    :8
    ["rgb(247,252,240)"
     "rgb(224,243,219)"
     "rgb(204,235,197)"
     "rgb(168,221,181)"
     "rgb(123,204,196)"
     "rgb(78,179,211)"
     "rgb(43,140,190)"
     "rgb(8,88,158)"],
    :9
    ["rgb(247,252,240)"
     "rgb(224,243,219)"
     "rgb(204,235,197)"
     "rgb(168,221,181)"
     "rgb(123,204,196)"
     "rgb(78,179,211)"
     "rgb(43,140,190)"
     "rgb(8,104,172)"
     "rgb(8,64,129)"],
    :type "seq"},
   :RdGy
   {:11
    ["rgb(103,0,31)"
     "rgb(178,24,43)"
     "rgb(214,96,77)"
     "rgb(244,165,130)"
     "rgb(253,219,199)"
     "rgb(255,255,255)"
     "rgb(224,224,224)"
     "rgb(186,186,186)"
     "rgb(135,135,135)"
     "rgb(77,77,77)"
     "rgb(26,26,26)"],
    :10
    ["rgb(103,0,31)"
     "rgb(178,24,43)"
     "rgb(214,96,77)"
     "rgb(244,165,130)"
     "rgb(253,219,199)"
     "rgb(224,224,224)"
     "rgb(186,186,186)"
     "rgb(135,135,135)"
     "rgb(77,77,77)"
     "rgb(26,26,26)"],
    :4
    ["rgb(202,0,32)"
     "rgb(244,165,130)"
     "rgb(186,186,186)"
     "rgb(64,64,64)"],
    :type "div",
    :7
    ["rgb(178,24,43)"
     "rgb(239,138,98)"
     "rgb(253,219,199)"
     "rgb(255,255,255)"
     "rgb(224,224,224)"
     "rgb(153,153,153)"
     "rgb(77,77,77)"],
    :8
    ["rgb(178,24,43)"
     "rgb(214,96,77)"
     "rgb(244,165,130)"
     "rgb(253,219,199)"
     "rgb(224,224,224)"
     "rgb(186,186,186)"
     "rgb(135,135,135)"
     "rgb(77,77,77)"],
    :9
    ["rgb(178,24,43)"
     "rgb(214,96,77)"
     "rgb(244,165,130)"
     "rgb(253,219,199)"
     "rgb(255,255,255)"
     "rgb(224,224,224)"
     "rgb(186,186,186)"
     "rgb(135,135,135)"
     "rgb(77,77,77)"],
    :5
    ["rgb(202,0,32)"
     "rgb(244,165,130)"
     "rgb(255,255,255)"
     "rgb(186,186,186)"
     "rgb(64,64,64)"],
    :3 ["rgb(239,138,98)" "rgb(255,255,255)" "rgb(153,153,153)"],
    :6
    ["rgb(178,24,43)"
     "rgb(239,138,98)"
     "rgb(253,219,199)"
     "rgb(224,224,224)"
     "rgb(153,153,153)"
     "rgb(77,77,77)"]},
   :Purples
   {:3 ["rgb(239,237,245)" "rgb(188,189,220)" "rgb(117,107,177)"],
    :4
    ["rgb(242,240,247)"
     "rgb(203,201,226)"
     "rgb(158,154,200)"
     "rgb(106,81,163)"],
    :5
    ["rgb(242,240,247)"
     "rgb(203,201,226)"
     "rgb(158,154,200)"
     "rgb(117,107,177)"
     "rgb(84,39,143)"],
    :6
    ["rgb(242,240,247)"
     "rgb(218,218,235)"
     "rgb(188,189,220)"
     "rgb(158,154,200)"
     "rgb(117,107,177)"
     "rgb(84,39,143)"],
    :7
    ["rgb(242,240,247)"
     "rgb(218,218,235)"
     "rgb(188,189,220)"
     "rgb(158,154,200)"
     "rgb(128,125,186)"
     "rgb(106,81,163)"
     "rgb(74,20,134)"],
    :8
    ["rgb(252,251,253)"
     "rgb(239,237,245)"
     "rgb(218,218,235)"
     "rgb(188,189,220)"
     "rgb(158,154,200)"
     "rgb(128,125,186)"
     "rgb(106,81,163)"
     "rgb(74,20,134)"],
    :9
    ["rgb(252,251,253)"
     "rgb(239,237,245)"
     "rgb(218,218,235)"
     "rgb(188,189,220)"
     "rgb(158,154,200)"
     "rgb(128,125,186)"
     "rgb(106,81,163)"
     "rgb(84,39,143)"
     "rgb(63,0,125)"],
    :type "seq"},
   :YlOrBr
   {:3 ["rgb(255,247,188)" "rgb(254,196,79)" "rgb(217,95,14)"],
    :4
    ["rgb(255,255,212)"
     "rgb(254,217,142)"
     "rgb(254,153,41)"
     "rgb(204,76,2)"],
    :5
    ["rgb(255,255,212)"
     "rgb(254,217,142)"
     "rgb(254,153,41)"
     "rgb(217,95,14)"
     "rgb(153,52,4)"],
    :6
    ["rgb(255,255,212)"
     "rgb(254,227,145)"
     "rgb(254,196,79)"
     "rgb(254,153,41)"
     "rgb(217,95,14)"
     "rgb(153,52,4)"],
    :7
    ["rgb(255,255,212)"
     "rgb(254,227,145)"
     "rgb(254,196,79)"
     "rgb(254,153,41)"
     "rgb(236,112,20)"
     "rgb(204,76,2)"
     "rgb(140,45,4)"],
    :8
    ["rgb(255,255,229)"
     "rgb(255,247,188)"
     "rgb(254,227,145)"
     "rgb(254,196,79)"
     "rgb(254,153,41)"
     "rgb(236,112,20)"
     "rgb(204,76,2)"
     "rgb(140,45,4)"],
    :9
    ["rgb(255,255,229)"
     "rgb(255,247,188)"
     "rgb(254,227,145)"
     "rgb(254,196,79)"
     "rgb(254,153,41)"
     "rgb(236,112,20)"
     "rgb(204,76,2)"
     "rgb(153,52,4)"
     "rgb(102,37,6)"],
    :type "seq"},
   :Pastel2
   {:3 ["rgb(179,226,205)" "rgb(253,205,172)" "rgb(203,213,232)"],
    :4
    ["rgb(179,226,205)"
     "rgb(253,205,172)"
     "rgb(203,213,232)"
     "rgb(244,202,228)"],
    :5
    ["rgb(179,226,205)"
     "rgb(253,205,172)"
     "rgb(203,213,232)"
     "rgb(244,202,228)"
     "rgb(230,245,201)"],
    :6
    ["rgb(179,226,205)"
     "rgb(253,205,172)"
     "rgb(203,213,232)"
     "rgb(244,202,228)"
     "rgb(230,245,201)"
     "rgb(255,242,174)"],
    :7
    ["rgb(179,226,205)"
     "rgb(253,205,172)"
     "rgb(203,213,232)"
     "rgb(244,202,228)"
     "rgb(230,245,201)"
     "rgb(255,242,174)"
     "rgb(241,226,204)"],
    :8
    ["rgb(179,226,205)"
     "rgb(253,205,172)"
     "rgb(203,213,232)"
     "rgb(244,202,228)"
     "rgb(230,245,201)"
     "rgb(255,242,174)"
     "rgb(241,226,204)"
     "rgb(204,204,204)"],
    :type "qual"},
   :Set3
   {:12
    ["rgb(141,211,199)"
     "rgb(255,255,179)"
     "rgb(190,186,218)"
     "rgb(251,128,114)"
     "rgb(128,177,211)"
     "rgb(253,180,98)"
     "rgb(179,222,105)"
     "rgb(252,205,229)"
     "rgb(217,217,217)"
     "rgb(188,128,189)"
     "rgb(204,235,197)"
     "rgb(255,237,111)"],
    :11
    ["rgb(141,211,199)"
     "rgb(255,255,179)"
     "rgb(190,186,218)"
     "rgb(251,128,114)"
     "rgb(128,177,211)"
     "rgb(253,180,98)"
     "rgb(179,222,105)"
     "rgb(252,205,229)"
     "rgb(217,217,217)"
     "rgb(188,128,189)"
     "rgb(204,235,197)"],
    :10
    ["rgb(141,211,199)"
     "rgb(255,255,179)"
     "rgb(190,186,218)"
     "rgb(251,128,114)"
     "rgb(128,177,211)"
     "rgb(253,180,98)"
     "rgb(179,222,105)"
     "rgb(252,205,229)"
     "rgb(217,217,217)"
     "rgb(188,128,189)"],
    :4
    ["rgb(141,211,199)"
     "rgb(255,255,179)"
     "rgb(190,186,218)"
     "rgb(251,128,114)"],
    :type "qual",
    :7
    ["rgb(141,211,199)"
     "rgb(255,255,179)"
     "rgb(190,186,218)"
     "rgb(251,128,114)"
     "rgb(128,177,211)"
     "rgb(253,180,98)"
     "rgb(179,222,105)"],
    :8
    ["rgb(141,211,199)"
     "rgb(255,255,179)"
     "rgb(190,186,218)"
     "rgb(251,128,114)"
     "rgb(128,177,211)"
     "rgb(253,180,98)"
     "rgb(179,222,105)"
     "rgb(252,205,229)"],
    :9
    ["rgb(141,211,199)"
     "rgb(255,255,179)"
     "rgb(190,186,218)"
     "rgb(251,128,114)"
     "rgb(128,177,211)"
     "rgb(253,180,98)"
     "rgb(179,222,105)"
     "rgb(252,205,229)"
     "rgb(217,217,217)"],
    :5
    ["rgb(141,211,199)"
     "rgb(255,255,179)"
     "rgb(190,186,218)"
     "rgb(251,128,114)"
     "rgb(128,177,211)"],
    :3 ["rgb(141,211,199)" "rgb(255,255,179)" "rgb(190,186,218)"],
    :6
    ["rgb(141,211,199)"
     "rgb(255,255,179)"
     "rgb(190,186,218)"
     "rgb(251,128,114)"
     "rgb(128,177,211)"
     "rgb(253,180,98)"]},
   :Greys
   {:3 ["rgb(240,240,240)" "rgb(189,189,189)" "rgb(99,99,99)"],
    :4
    ["rgb(247,247,247)"
     "rgb(204,204,204)"
     "rgb(150,150,150)"
     "rgb(82,82,82)"],
    :5
    ["rgb(247,247,247)"
     "rgb(204,204,204)"
     "rgb(150,150,150)"
     "rgb(99,99,99)"
     "rgb(37,37,37)"],
    :6
    ["rgb(247,247,247)"
     "rgb(217,217,217)"
     "rgb(189,189,189)"
     "rgb(150,150,150)"
     "rgb(99,99,99)"
     "rgb(37,37,37)"],
    :7
    ["rgb(247,247,247)"
     "rgb(217,217,217)"
     "rgb(189,189,189)"
     "rgb(150,150,150)"
     "rgb(115,115,115)"
     "rgb(82,82,82)"
     "rgb(37,37,37)"],
    :8
    ["rgb(255,255,255)"
     "rgb(240,240,240)"
     "rgb(217,217,217)"
     "rgb(189,189,189)"
     "rgb(150,150,150)"
     "rgb(115,115,115)"
     "rgb(82,82,82)"
     "rgb(37,37,37)"],
    :9
    ["rgb(255,255,255)"
     "rgb(240,240,240)"
     "rgb(217,217,217)"
     "rgb(189,189,189)"
     "rgb(150,150,150)"
     "rgb(115,115,115)"
     "rgb(82,82,82)"
     "rgb(37,37,37)"
     "rgb(0,0,0)"],
    :type "seq"},
   :Greens
   {:3 ["rgb(229,245,224)" "rgb(161,217,155)" "rgb(49,163,84)"],
    :4
    ["rgb(237,248,233)"
     "rgb(186,228,179)"
     "rgb(116,196,118)"
     "rgb(35,139,69)"],
    :5
    ["rgb(237,248,233)"
     "rgb(186,228,179)"
     "rgb(116,196,118)"
     "rgb(49,163,84)"
     "rgb(0,109,44)"],
    :6
    ["rgb(237,248,233)"
     "rgb(199,233,192)"
     "rgb(161,217,155)"
     "rgb(116,196,118)"
     "rgb(49,163,84)"
     "rgb(0,109,44)"],
    :7
    ["rgb(237,248,233)"
     "rgb(199,233,192)"
     "rgb(161,217,155)"
     "rgb(116,196,118)"
     "rgb(65,171,93)"
     "rgb(35,139,69)"
     "rgb(0,90,50)"],
    :8
    ["rgb(247,252,245)"
     "rgb(229,245,224)"
     "rgb(199,233,192)"
     "rgb(161,217,155)"
     "rgb(116,196,118)"
     "rgb(65,171,93)"
     "rgb(35,139,69)"
     "rgb(0,90,50)"],
    :9
    ["rgb(247,252,245)"
     "rgb(229,245,224)"
     "rgb(199,233,192)"
     "rgb(161,217,155)"
     "rgb(116,196,118)"
     "rgb(65,171,93)"
     "rgb(35,139,69)"
     "rgb(0,109,44)"
     "rgb(0,68,27)"],
    :type "seq"},
   :BrBG
   {:11
    ["rgb(84,48,5)"
     "rgb(140,81,10)"
     "rgb(191,129,45)"
     "rgb(223,194,125)"
     "rgb(246,232,195)"
     "rgb(245,245,245)"
     "rgb(199,234,229)"
     "rgb(128,205,193)"
     "rgb(53,151,143)"
     "rgb(1,102,94)"
     "rgb(0,60,48)"],
    :10
    ["rgb(84,48,5)"
     "rgb(140,81,10)"
     "rgb(191,129,45)"
     "rgb(223,194,125)"
     "rgb(246,232,195)"
     "rgb(199,234,229)"
     "rgb(128,205,193)"
     "rgb(53,151,143)"
     "rgb(1,102,94)"
     "rgb(0,60,48)"],
    :4
    ["rgb(166,97,26)"
     "rgb(223,194,125)"
     "rgb(128,205,193)"
     "rgb(1,133,113)"],
    :type "div",
    :7
    ["rgb(140,81,10)"
     "rgb(216,179,101)"
     "rgb(246,232,195)"
     "rgb(245,245,245)"
     "rgb(199,234,229)"
     "rgb(90,180,172)"
     "rgb(1,102,94)"],
    :8
    ["rgb(140,81,10)"
     "rgb(191,129,45)"
     "rgb(223,194,125)"
     "rgb(246,232,195)"
     "rgb(199,234,229)"
     "rgb(128,205,193)"
     "rgb(53,151,143)"
     "rgb(1,102,94)"],
    :9
    ["rgb(140,81,10)"
     "rgb(191,129,45)"
     "rgb(223,194,125)"
     "rgb(246,232,195)"
     "rgb(245,245,245)"
     "rgb(199,234,229)"
     "rgb(128,205,193)"
     "rgb(53,151,143)"
     "rgb(1,102,94)"],
    :5
    ["rgb(166,97,26)"
     "rgb(223,194,125)"
     "rgb(245,245,245)"
     "rgb(128,205,193)"
     "rgb(1,133,113)"],
    :3 ["rgb(216,179,101)" "rgb(245,245,245)" "rgb(90,180,172)"],
    :6
    ["rgb(140,81,10)"
     "rgb(216,179,101)"
     "rgb(246,232,195)"
     "rgb(199,234,229)"
     "rgb(90,180,172)"
     "rgb(1,102,94)"]},
   :PuOr
   {:11
    ["rgb(127,59,8)"
     "rgb(179,88,6)"
     "rgb(224,130,20)"
     "rgb(253,184,99)"
     "rgb(254,224,182)"
     "rgb(247,247,247)"
     "rgb(216,218,235)"
     "rgb(178,171,210)"
     "rgb(128,115,172)"
     "rgb(84,39,136)"
     "rgb(45,0,75)"],
    :10
    ["rgb(127,59,8)"
     "rgb(179,88,6)"
     "rgb(224,130,20)"
     "rgb(253,184,99)"
     "rgb(254,224,182)"
     "rgb(216,218,235)"
     "rgb(178,171,210)"
     "rgb(128,115,172)"
     "rgb(84,39,136)"
     "rgb(45,0,75)"],
    :4
    ["rgb(230,97,1)"
     "rgb(253,184,99)"
     "rgb(178,171,210)"
     "rgb(94,60,153)"],
    :type "div",
    :7
    ["rgb(179,88,6)"
     "rgb(241,163,64)"
     "rgb(254,224,182)"
     "rgb(247,247,247)"
     "rgb(216,218,235)"
     "rgb(153,142,195)"
     "rgb(84,39,136)"],
    :8
    ["rgb(179,88,6)"
     "rgb(224,130,20)"
     "rgb(253,184,99)"
     "rgb(254,224,182)"
     "rgb(216,218,235)"
     "rgb(178,171,210)"
     "rgb(128,115,172)"
     "rgb(84,39,136)"],
    :9
    ["rgb(179,88,6)"
     "rgb(224,130,20)"
     "rgb(253,184,99)"
     "rgb(254,224,182)"
     "rgb(247,247,247)"
     "rgb(216,218,235)"
     "rgb(178,171,210)"
     "rgb(128,115,172)"
     "rgb(84,39,136)"],
    :5
    ["rgb(230,97,1)"
     "rgb(253,184,99)"
     "rgb(247,247,247)"
     "rgb(178,171,210)"
     "rgb(94,60,153)"],
    :3 ["rgb(241,163,64)" "rgb(247,247,247)" "rgb(153,142,195)"],
    :6
    ["rgb(179,88,6)"
     "rgb(241,163,64)"
     "rgb(254,224,182)"
     "rgb(216,218,235)"
     "rgb(153,142,195)"
     "rgb(84,39,136)"]},
   :BuPu
   {:3 ["rgb(224,236,244)" "rgb(158,188,218)" "rgb(136,86,167)"],
    :4
    ["rgb(237,248,251)"
     "rgb(179,205,227)"
     "rgb(140,150,198)"
     "rgb(136,65,157)"],
    :5
    ["rgb(237,248,251)"
     "rgb(179,205,227)"
     "rgb(140,150,198)"
     "rgb(136,86,167)"
     "rgb(129,15,124)"],
    :6
    ["rgb(237,248,251)"
     "rgb(191,211,230)"
     "rgb(158,188,218)"
     "rgb(140,150,198)"
     "rgb(136,86,167)"
     "rgb(129,15,124)"],
    :7
    ["rgb(237,248,251)"
     "rgb(191,211,230)"
     "rgb(158,188,218)"
     "rgb(140,150,198)"
     "rgb(140,107,177)"
     "rgb(136,65,157)"
     "rgb(110,1,107)"],
    :8
    ["rgb(247,252,253)"
     "rgb(224,236,244)"
     "rgb(191,211,230)"
     "rgb(158,188,218)"
     "rgb(140,150,198)"
     "rgb(140,107,177)"
     "rgb(136,65,157)"
     "rgb(110,1,107)"],
    :9
    ["rgb(247,252,253)"
     "rgb(224,236,244)"
     "rgb(191,211,230)"
     "rgb(158,188,218)"
     "rgb(140,150,198)"
     "rgb(140,107,177)"
     "rgb(136,65,157)"
     "rgb(129,15,124)"
     "rgb(77,0,75)"],
    :type "seq"},
   :RdYlGn
   {:11
    ["rgb(165,0,38)"
     "rgb(215,48,39)"
     "rgb(244,109,67)"
     "rgb(253,174,97)"
     "rgb(254,224,139)"
     "rgb(255,255,191)"
     "rgb(217,239,139)"
     "rgb(166,217,106)"
     "rgb(102,189,99)"
     "rgb(26,152,80)"
     "rgb(0,104,55)"],
    :10
    ["rgb(165,0,38)"
     "rgb(215,48,39)"
     "rgb(244,109,67)"
     "rgb(253,174,97)"
     "rgb(254,224,139)"
     "rgb(217,239,139)"
     "rgb(166,217,106)"
     "rgb(102,189,99)"
     "rgb(26,152,80)"
     "rgb(0,104,55)"],
    :4
    ["rgb(215,25,28)"
     "rgb(253,174,97)"
     "rgb(166,217,106)"
     "rgb(26,150,65)"],
    :type "div",
    :7
    ["rgb(215,48,39)"
     "rgb(252,141,89)"
     "rgb(254,224,139)"
     "rgb(255,255,191)"
     "rgb(217,239,139)"
     "rgb(145,207,96)"
     "rgb(26,152,80)"],
    :8
    ["rgb(215,48,39)"
     "rgb(244,109,67)"
     "rgb(253,174,97)"
     "rgb(254,224,139)"
     "rgb(217,239,139)"
     "rgb(166,217,106)"
     "rgb(102,189,99)"
     "rgb(26,152,80)"],
    :9
    ["rgb(215,48,39)"
     "rgb(244,109,67)"
     "rgb(253,174,97)"
     "rgb(254,224,139)"
     "rgb(255,255,191)"
     "rgb(217,239,139)"
     "rgb(166,217,106)"
     "rgb(102,189,99)"
     "rgb(26,152,80)"],
    :5
    ["rgb(215,25,28)"
     "rgb(253,174,97)"
     "rgb(255,255,191)"
     "rgb(166,217,106)"
     "rgb(26,150,65)"],
    :3 ["rgb(252,141,89)" "rgb(255,255,191)" "rgb(145,207,96)"],
    :6
    ["rgb(215,48,39)"
     "rgb(252,141,89)"
     "rgb(254,224,139)"
     "rgb(217,239,139)"
     "rgb(145,207,96)"
     "rgb(26,152,80)"]},
   :Reds
   {:3 ["rgb(254,224,210)" "rgb(252,146,114)" "rgb(222,45,38)"],
    :4
    ["rgb(254,229,217)"
     "rgb(252,174,145)"
     "rgb(251,106,74)"
     "rgb(203,24,29)"],
    :5
    ["rgb(254,229,217)"
     "rgb(252,174,145)"
     "rgb(251,106,74)"
     "rgb(222,45,38)"
     "rgb(165,15,21)"],
    :6
    ["rgb(254,229,217)"
     "rgb(252,187,161)"
     "rgb(252,146,114)"
     "rgb(251,106,74)"
     "rgb(222,45,38)"
     "rgb(165,15,21)"],
    :7
    ["rgb(254,229,217)"
     "rgb(252,187,161)"
     "rgb(252,146,114)"
     "rgb(251,106,74)"
     "rgb(239,59,44)"
     "rgb(203,24,29)"
     "rgb(153,0,13)"],
    :8
    ["rgb(255,245,240)"
     "rgb(254,224,210)"
     "rgb(252,187,161)"
     "rgb(252,146,114)"
     "rgb(251,106,74)"
     "rgb(239,59,44)"
     "rgb(203,24,29)"
     "rgb(153,0,13)"],
    :9
    ["rgb(255,245,240)"
     "rgb(254,224,210)"
     "rgb(252,187,161)"
     "rgb(252,146,114)"
     "rgb(251,106,74)"
     "rgb(239,59,44)"
     "rgb(203,24,29)"
     "rgb(165,15,21)"
     "rgb(103,0,13)"],
    :type "seq"},
   :Accent
   {:3 ["rgb(127,201,127)" "rgb(190,174,212)" "rgb(253,192,134)"],
    :4
    ["rgb(127,201,127)"
     "rgb(190,174,212)"
     "rgb(253,192,134)"
     "rgb(255,255,153)"],
    :5
    ["rgb(127,201,127)"
     "rgb(190,174,212)"
     "rgb(253,192,134)"
     "rgb(255,255,153)"
     "rgb(56,108,176)"],
    :6
    ["rgb(127,201,127)"
     "rgb(190,174,212)"
     "rgb(253,192,134)"
     "rgb(255,255,153)"
     "rgb(56,108,176)"
     "rgb(240,2,127)"],
    :7
    ["rgb(127,201,127)"
     "rgb(190,174,212)"
     "rgb(253,192,134)"
     "rgb(255,255,153)"
     "rgb(56,108,176)"
     "rgb(240,2,127)"
     "rgb(191,91,23)"],
    :8
    ["rgb(127,201,127)"
     "rgb(190,174,212)"
     "rgb(253,192,134)"
     "rgb(255,255,153)"
     "rgb(56,108,176)"
     "rgb(240,2,127)"
     "rgb(191,91,23)"
     "rgb(102,102,102)"],
    :type "qual"},
   :PRGn
   {:11
    ["rgb(64,0,75)"
     "rgb(118,42,131)"
     "rgb(153,112,171)"
     "rgb(194,165,207)"
     "rgb(231,212,232)"
     "rgb(247,247,247)"
     "rgb(217,240,211)"
     "rgb(166,219,160)"
     "rgb(90,174,97)"
     "rgb(27,120,55)"
     "rgb(0,68,27)"],
    :10
    ["rgb(64,0,75)"
     "rgb(118,42,131)"
     "rgb(153,112,171)"
     "rgb(194,165,207)"
     "rgb(231,212,232)"
     "rgb(217,240,211)"
     "rgb(166,219,160)"
     "rgb(90,174,97)"
     "rgb(27,120,55)"
     "rgb(0,68,27)"],
    :4
    ["rgb(123,50,148)"
     "rgb(194,165,207)"
     "rgb(166,219,160)"
     "rgb(0,136,55)"],
    :type "div",
    :7
    ["rgb(118,42,131)"
     "rgb(175,141,195)"
     "rgb(231,212,232)"
     "rgb(247,247,247)"
     "rgb(217,240,211)"
     "rgb(127,191,123)"
     "rgb(27,120,55)"],
    :8
    ["rgb(118,42,131)"
     "rgb(153,112,171)"
     "rgb(194,165,207)"
     "rgb(231,212,232)"
     "rgb(217,240,211)"
     "rgb(166,219,160)"
     "rgb(90,174,97)"
     "rgb(27,120,55)"],
    :9
    ["rgb(118,42,131)"
     "rgb(153,112,171)"
     "rgb(194,165,207)"
     "rgb(231,212,232)"
     "rgb(247,247,247)"
     "rgb(217,240,211)"
     "rgb(166,219,160)"
     "rgb(90,174,97)"
     "rgb(27,120,55)"],
    :5
    ["rgb(123,50,148)"
     "rgb(194,165,207)"
     "rgb(247,247,247)"
     "rgb(166,219,160)"
     "rgb(0,136,55)"],
    :3 ["rgb(175,141,195)" "rgb(247,247,247)" "rgb(127,191,123)"],
    :6
    ["rgb(118,42,131)"
     "rgb(175,141,195)"
     "rgb(231,212,232)"
     "rgb(217,240,211)"
     "rgb(127,191,123)"
     "rgb(27,120,55)"]},
   :Dark2
   {:3 ["rgb(27,158,119)" "rgb(217,95,2)" "rgb(117,112,179)"],
    :4
    ["rgb(27,158,119)"
     "rgb(217,95,2)"
     "rgb(117,112,179)"
     "rgb(231,41,138)"],
    :5
    ["rgb(27,158,119)"
     "rgb(217,95,2)"
     "rgb(117,112,179)"
     "rgb(231,41,138)"
     "rgb(102,166,30)"],
    :6
    ["rgb(27,158,119)"
     "rgb(217,95,2)"
     "rgb(117,112,179)"
     "rgb(231,41,138)"
     "rgb(102,166,30)"
     "rgb(230,171,2)"],
    :7
    ["rgb(27,158,119)"
     "rgb(217,95,2)"
     "rgb(117,112,179)"
     "rgb(231,41,138)"
     "rgb(102,166,30)"
     "rgb(230,171,2)"
     "rgb(166,118,29)"],
    :8
    ["rgb(27,158,119)"
     "rgb(217,95,2)"
     "rgb(117,112,179)"
     "rgb(231,41,138)"
     "rgb(102,166,30)"
     "rgb(230,171,2)"
     "rgb(166,118,29)"
     "rgb(102,102,102)"],
    :type "qual"},
   :PiYG
   {:11
    ["rgb(142,1,82)"
     "rgb(197,27,125)"
     "rgb(222,119,174)"
     "rgb(241,182,218)"
     "rgb(253,224,239)"
     "rgb(247,247,247)"
     "rgb(230,245,208)"
     "rgb(184,225,134)"
     "rgb(127,188,65)"
     "rgb(77,146,33)"
     "rgb(39,100,25)"],
    :10
    ["rgb(142,1,82)"
     "rgb(197,27,125)"
     "rgb(222,119,174)"
     "rgb(241,182,218)"
     "rgb(253,224,239)"
     "rgb(230,245,208)"
     "rgb(184,225,134)"
     "rgb(127,188,65)"
     "rgb(77,146,33)"
     "rgb(39,100,25)"],
    :4
    ["rgb(208,28,139)"
     "rgb(241,182,218)"
     "rgb(184,225,134)"
     "rgb(77,172,38)"],
    :type "div",
    :7
    ["rgb(197,27,125)"
     "rgb(233,163,201)"
     "rgb(253,224,239)"
     "rgb(247,247,247)"
     "rgb(230,245,208)"
     "rgb(161,215,106)"
     "rgb(77,146,33)"],
    :8
    ["rgb(197,27,125)"
     "rgb(222,119,174)"
     "rgb(241,182,218)"
     "rgb(253,224,239)"
     "rgb(230,245,208)"
     "rgb(184,225,134)"
     "rgb(127,188,65)"
     "rgb(77,146,33)"],
    :9
    ["rgb(197,27,125)"
     "rgb(222,119,174)"
     "rgb(241,182,218)"
     "rgb(253,224,239)"
     "rgb(247,247,247)"
     "rgb(230,245,208)"
     "rgb(184,225,134)"
     "rgb(127,188,65)"
     "rgb(77,146,33)"],
    :5
    ["rgb(208,28,139)"
     "rgb(241,182,218)"
     "rgb(247,247,247)"
     "rgb(184,225,134)"
     "rgb(77,172,38)"],
    :3 ["rgb(233,163,201)" "rgb(247,247,247)" "rgb(161,215,106)"],
    :6
    ["rgb(197,27,125)"
     "rgb(233,163,201)"
     "rgb(253,224,239)"
     "rgb(230,245,208)"
     "rgb(161,215,106)"
     "rgb(77,146,33)"]},
   :OrRd
   {:3 ["rgb(254,232,200)" "rgb(253,187,132)" "rgb(227,74,51)"],
    :4
    ["rgb(254,240,217)"
     "rgb(253,204,138)"
     "rgb(252,141,89)"
     "rgb(215,48,31)"],
    :5
    ["rgb(254,240,217)"
     "rgb(253,204,138)"
     "rgb(252,141,89)"
     "rgb(227,74,51)"
     "rgb(179,0,0)"],
    :6
    ["rgb(254,240,217)"
     "rgb(253,212,158)"
     "rgb(253,187,132)"
     "rgb(252,141,89)"
     "rgb(227,74,51)"
     "rgb(179,0,0)"],
    :7
    ["rgb(254,240,217)"
     "rgb(253,212,158)"
     "rgb(253,187,132)"
     "rgb(252,141,89)"
     "rgb(239,101,72)"
     "rgb(215,48,31)"
     "rgb(153,0,0)"],
    :8
    ["rgb(255,247,236)"
     "rgb(254,232,200)"
     "rgb(253,212,158)"
     "rgb(253,187,132)"
     "rgb(252,141,89)"
     "rgb(239,101,72)"
     "rgb(215,48,31)"
     "rgb(153,0,0)"],
    :9
    ["rgb(255,247,236)"
     "rgb(254,232,200)"
     "rgb(253,212,158)"
     "rgb(253,187,132)"
     "rgb(252,141,89)"
     "rgb(239,101,72)"
     "rgb(215,48,31)"
     "rgb(179,0,0)"
     "rgb(127,0,0)"],
    :type "seq"},
   :PuBuGn
   {:3 ["rgb(236,226,240)" "rgb(166,189,219)" "rgb(28,144,153)"],
    :4
    ["rgb(246,239,247)"
     "rgb(189,201,225)"
     "rgb(103,169,207)"
     "rgb(2,129,138)"],
    :5
    ["rgb(246,239,247)"
     "rgb(189,201,225)"
     "rgb(103,169,207)"
     "rgb(28,144,153)"
     "rgb(1,108,89)"],
    :6
    ["rgb(246,239,247)"
     "rgb(208,209,230)"
     "rgb(166,189,219)"
     "rgb(103,169,207)"
     "rgb(28,144,153)"
     "rgb(1,108,89)"],
    :7
    ["rgb(246,239,247)"
     "rgb(208,209,230)"
     "rgb(166,189,219)"
     "rgb(103,169,207)"
     "rgb(54,144,192)"
     "rgb(2,129,138)"
     "rgb(1,100,80)"],
    :8
    ["rgb(255,247,251)"
     "rgb(236,226,240)"
     "rgb(208,209,230)"
     "rgb(166,189,219)"
     "rgb(103,169,207)"
     "rgb(54,144,192)"
     "rgb(2,129,138)"
     "rgb(1,100,80)"],
    :9
    ["rgb(255,247,251)"
     "rgb(236,226,240)"
     "rgb(208,209,230)"
     "rgb(166,189,219)"
     "rgb(103,169,207)"
     "rgb(54,144,192)"
     "rgb(2,129,138)"
     "rgb(1,108,89)"
     "rgb(1,70,54)"],
    :type "seq"},
   :YlOrRd
   {:3 ["rgb(255,237,160)" "rgb(254,178,76)" "rgb(240,59,32)"],
    :4
    ["rgb(255,255,178)"
     "rgb(254,204,92)"
     "rgb(253,141,60)"
     "rgb(227,26,28)"],
    :5
    ["rgb(255,255,178)"
     "rgb(254,204,92)"
     "rgb(253,141,60)"
     "rgb(240,59,32)"
     "rgb(189,0,38)"],
    :6
    ["rgb(255,255,178)"
     "rgb(254,217,118)"
     "rgb(254,178,76)"
     "rgb(253,141,60)"
     "rgb(240,59,32)"
     "rgb(189,0,38)"],
    :7
    ["rgb(255,255,178)"
     "rgb(254,217,118)"
     "rgb(254,178,76)"
     "rgb(253,141,60)"
     "rgb(252,78,42)"
     "rgb(227,26,28)"
     "rgb(177,0,38)"],
    :8
    ["rgb(255,255,204)"
     "rgb(255,237,160)"
     "rgb(254,217,118)"
     "rgb(254,178,76)"
     "rgb(253,141,60)"
     "rgb(252,78,42)"
     "rgb(227,26,28)"
     "rgb(177,0,38)"],
    :type "seq"},
   :BuGn
   {:3 ["rgb(229,245,249)" "rgb(153,216,201)" "rgb(44,162,95)"],
    :4
    ["rgb(237,248,251)"
     "rgb(178,226,226)"
     "rgb(102,194,164)"
     "rgb(35,139,69)"],
    :5
    ["rgb(237,248,251)"
     "rgb(178,226,226)"
     "rgb(102,194,164)"
     "rgb(44,162,95)"
     "rgb(0,109,44)"],
    :6
    ["rgb(237,248,251)"
     "rgb(204,236,230)"
     "rgb(153,216,201)"
     "rgb(102,194,164)"
     "rgb(44,162,95)"
     "rgb(0,109,44)"],
    :7
    ["rgb(237,248,251)"
     "rgb(204,236,230)"
     "rgb(153,216,201)"
     "rgb(102,194,164)"
     "rgb(65,174,118)"
     "rgb(35,139,69)"
     "rgb(0,88,36)"],
    :8
    ["rgb(247,252,253)"
     "rgb(229,245,249)"
     "rgb(204,236,230)"
     "rgb(153,216,201)"
     "rgb(102,194,164)"
     "rgb(65,174,118)"
     "rgb(35,139,69)"
     "rgb(0,88,36)"],
    :9
    ["rgb(247,252,253)"
     "rgb(229,245,249)"
     "rgb(204,236,230)"
     "rgb(153,216,201)"
     "rgb(102,194,164)"
     "rgb(65,174,118)"
     "rgb(35,139,69)"
     "rgb(0,109,44)"
     "rgb(0,68,27)"],
    :type "seq"},
   :Oranges
   {:3 ["rgb(254,230,206)" "rgb(253,174,107)" "rgb(230,85,13)"],
    :4
    ["rgb(254,237,222)"
     "rgb(253,190,133)"
     "rgb(253,141,60)"
     "rgb(217,71,1)"],
    :5
    ["rgb(254,237,222)"
     "rgb(253,190,133)"
     "rgb(253,141,60)"
     "rgb(230,85,13)"
     "rgb(166,54,3)"],
    :6
    ["rgb(254,237,222)"
     "rgb(253,208,162)"
     "rgb(253,174,107)"
     "rgb(253,141,60)"
     "rgb(230,85,13)"
     "rgb(166,54,3)"],
    :7
    ["rgb(254,237,222)"
     "rgb(253,208,162)"
     "rgb(253,174,107)"
     "rgb(253,141,60)"
     "rgb(241,105,19)"
     "rgb(217,72,1)"
     "rgb(140,45,4)"],
    :8
    ["rgb(255,245,235)"
     "rgb(254,230,206)"
     "rgb(253,208,162)"
     "rgb(253,174,107)"
     "rgb(253,141,60)"
     "rgb(241,105,19)"
     "rgb(217,72,1)"
     "rgb(140,45,4)"],
    :9
    ["rgb(255,245,235)"
     "rgb(254,230,206)"
     "rgb(253,208,162)"
     "rgb(253,174,107)"
     "rgb(253,141,60)"
     "rgb(241,105,19)"
     "rgb(217,72,1)"
     "rgb(166,54,3)"
     "rgb(127,39,4)"],
    :type "seq"},
   :RdYlBu
   {:11
    ["rgb(165,0,38)"
     "rgb(215,48,39)"
     "rgb(244,109,67)"
     "rgb(253,174,97)"
     "rgb(254,224,144)"
     "rgb(255,255,191)"
     "rgb(224,243,248)"
     "rgb(171,217,233)"
     "rgb(116,173,209)"
     "rgb(69,117,180)"
     "rgb(49,54,149)"],
    :10
    ["rgb(165,0,38)"
     "rgb(215,48,39)"
     "rgb(244,109,67)"
     "rgb(253,174,97)"
     "rgb(254,224,144)"
     "rgb(224,243,248)"
     "rgb(171,217,233)"
     "rgb(116,173,209)"
     "rgb(69,117,180)"
     "rgb(49,54,149)"],
    :4
    ["rgb(215,25,28)"
     "rgb(253,174,97)"
     "rgb(171,217,233)"
     "rgb(44,123,182)"],
    :type "div",
    :7
    ["rgb(215,48,39)"
     "rgb(252,141,89)"
     "rgb(254,224,144)"
     "rgb(255,255,191)"
     "rgb(224,243,248)"
     "rgb(145,191,219)"
     "rgb(69,117,180)"],
    :8
    ["rgb(215,48,39)"
     "rgb(244,109,67)"
     "rgb(253,174,97)"
     "rgb(254,224,144)"
     "rgb(224,243,248)"
     "rgb(171,217,233)"
     "rgb(116,173,209)"
     "rgb(69,117,180)"],
    :9
    ["rgb(215,48,39)"
     "rgb(244,109,67)"
     "rgb(253,174,97)"
     "rgb(254,224,144)"
     "rgb(255,255,191)"
     "rgb(224,243,248)"
     "rgb(171,217,233)"
     "rgb(116,173,209)"
     "rgb(69,117,180)"],
    :5
    ["rgb(215,25,28)"
     "rgb(253,174,97)"
     "rgb(255,255,191)"
     "rgb(171,217,233)"
     "rgb(44,123,182)"],
    :3 ["rgb(252,141,89)" "rgb(255,255,191)" "rgb(145,191,219)"],
    :6
    ["rgb(215,48,39)"
     "rgb(252,141,89)"
     "rgb(254,224,144)"
     "rgb(224,243,248)"
     "rgb(145,191,219)"
     "rgb(69,117,180)"]},
   :Blues
   {:3 ["rgb(222,235,247)" "rgb(158,202,225)" "rgb(49,130,189)"],
    :4
    ["rgb(239,243,255)"
     "rgb(189,215,231)"
     "rgb(107,174,214)"
     "rgb(33,113,181)"],
    :5
    ["rgb(239,243,255)"
     "rgb(189,215,231)"
     "rgb(107,174,214)"
     "rgb(49,130,189)"
     "rgb(8,81,156)"],
    :6
    ["rgb(239,243,255)"
     "rgb(198,219,239)"
     "rgb(158,202,225)"
     "rgb(107,174,214)"
     "rgb(49,130,189)"
     "rgb(8,81,156)"],
    :7
    ["rgb(239,243,255)"
     "rgb(198,219,239)"
     "rgb(158,202,225)"
     "rgb(107,174,214)"
     "rgb(66,146,198)"
     "rgb(33,113,181)"
     "rgb(8,69,148)"],
    :8
    ["rgb(247,251,255)"
     "rgb(222,235,247)"
     "rgb(198,219,239)"
     "rgb(158,202,225)"
     "rgb(107,174,214)"
     "rgb(66,146,198)"
     "rgb(33,113,181)"
     "rgb(8,69,148)"],
    :9
    ["rgb(247,251,255)"
     "rgb(222,235,247)"
     "rgb(198,219,239)"
     "rgb(158,202,225)"
     "rgb(107,174,214)"
     "rgb(66,146,198)"
     "rgb(33,113,181)"
     "rgb(8,81,156)"
     "rgb(8,48,107)"],
    :type "seq"},
   :PuRd
   {:3 ["rgb(231,225,239)" "rgb(201,148,199)" "rgb(221,28,119)"],
    :4
    ["rgb(241,238,246)"
     "rgb(215,181,216)"
     "rgb(223,101,176)"
     "rgb(206,18,86)"],
    :5
    ["rgb(241,238,246)"
     "rgb(215,181,216)"
     "rgb(223,101,176)"
     "rgb(221,28,119)"
     "rgb(152,0,67)"],
    :6
    ["rgb(241,238,246)"
     "rgb(212,185,218)"
     "rgb(201,148,199)"
     "rgb(223,101,176)"
     "rgb(221,28,119)"
     "rgb(152,0,67)"],
    :7
    ["rgb(241,238,246)"
     "rgb(212,185,218)"
     "rgb(201,148,199)"
     "rgb(223,101,176)"
     "rgb(231,41,138)"
     "rgb(206,18,86)"
     "rgb(145,0,63)"],
    :8
    ["rgb(247,244,249)"
     "rgb(231,225,239)"
     "rgb(212,185,218)"
     "rgb(201,148,199)"
     "rgb(223,101,176)"
     "rgb(231,41,138)"
     "rgb(206,18,86)"
     "rgb(145,0,63)"],
    :9
    ["rgb(247,244,249)"
     "rgb(231,225,239)"
     "rgb(212,185,218)"
     "rgb(201,148,199)"
     "rgb(223,101,176)"
     "rgb(231,41,138)"
     "rgb(206,18,86)"
     "rgb(152,0,67)"
     "rgb(103,0,31)"],
    :type "seq"},
   :RdBu
   {:11
    ["rgb(103,0,31)"
     "rgb(178,24,43)"
     "rgb(214,96,77)"
     "rgb(244,165,130)"
     "rgb(253,219,199)"
     "rgb(247,247,247)"
     "rgb(209,229,240)"
     "rgb(146,197,222)"
     "rgb(67,147,195)"
     "rgb(33,102,172)"
     "rgb(5,48,97)"],
    :10
    ["rgb(103,0,31)"
     "rgb(178,24,43)"
     "rgb(214,96,77)"
     "rgb(244,165,130)"
     "rgb(253,219,199)"
     "rgb(209,229,240)"
     "rgb(146,197,222)"
     "rgb(67,147,195)"
     "rgb(33,102,172)"
     "rgb(5,48,97)"],
    :4
    ["rgb(202,0,32)"
     "rgb(244,165,130)"
     "rgb(146,197,222)"
     "rgb(5,113,176)"],
    :type "div",
    :7
    ["rgb(178,24,43)"
     "rgb(239,138,98)"
     "rgb(253,219,199)"
     "rgb(247,247,247)"
     "rgb(209,229,240)"
     "rgb(103,169,207)"
     "rgb(33,102,172)"],
    :8
    ["rgb(178,24,43)"
     "rgb(214,96,77)"
     "rgb(244,165,130)"
     "rgb(253,219,199)"
     "rgb(209,229,240)"
     "rgb(146,197,222)"
     "rgb(67,147,195)"
     "rgb(33,102,172)"],
    :9
    ["rgb(178,24,43)"
     "rgb(214,96,77)"
     "rgb(244,165,130)"
     "rgb(253,219,199)"
     "rgb(247,247,247)"
     "rgb(209,229,240)"
     "rgb(146,197,222)"
     "rgb(67,147,195)"
     "rgb(33,102,172)"],
    :5
    ["rgb(202,0,32)"
     "rgb(244,165,130)"
     "rgb(247,247,247)"
     "rgb(146,197,222)"
     "rgb(5,113,176)"],
    :3 ["rgb(239,138,98)" "rgb(247,247,247)" "rgb(103,169,207)"],
    :6
    ["rgb(178,24,43)"
     "rgb(239,138,98)"
     "rgb(253,219,199)"
     "rgb(209,229,240)"
     "rgb(103,169,207)"
     "rgb(33,102,172)"]},
   :RdPu
   {:3 ["rgb(253,224,221)" "rgb(250,159,181)" "rgb(197,27,138)"],
    :4
    ["rgb(254,235,226)"
     "rgb(251,180,185)"
     "rgb(247,104,161)"
     "rgb(174,1,126)"],
    :5
    ["rgb(254,235,226)"
     "rgb(251,180,185)"
     "rgb(247,104,161)"
     "rgb(197,27,138)"
     "rgb(122,1,119)"],
    :6
    ["rgb(254,235,226)"
     "rgb(252,197,192)"
     "rgb(250,159,181)"
     "rgb(247,104,161)"
     "rgb(197,27,138)"
     "rgb(122,1,119)"],
    :7
    ["rgb(254,235,226)"
     "rgb(252,197,192)"
     "rgb(250,159,181)"
     "rgb(247,104,161)"
     "rgb(221,52,151)"
     "rgb(174,1,126)"
     "rgb(122,1,119)"],
    :8
    ["rgb(255,247,243)"
     "rgb(253,224,221)"
     "rgb(252,197,192)"
     "rgb(250,159,181)"
     "rgb(247,104,161)"
     "rgb(221,52,151)"
     "rgb(174,1,126)"
     "rgb(122,1,119)"],
    :9
    ["rgb(255,247,243)"
     "rgb(253,224,221)"
     "rgb(252,197,192)"
     "rgb(250,159,181)"
     "rgb(247,104,161)"
     "rgb(221,52,151)"
     "rgb(174,1,126)"
     "rgb(122,1,119)"
     "rgb(73,0,106)"],
    :type "seq"},
   :Pastel1
   {:3 ["rgb(251,180,174)" "rgb(179,205,227)" "rgb(204,235,197)"],
    :4
    ["rgb(251,180,174)"
     "rgb(179,205,227)"
     "rgb(204,235,197)"
     "rgb(222,203,228)"],
    :5
    ["rgb(251,180,174)"
     "rgb(179,205,227)"
     "rgb(204,235,197)"
     "rgb(222,203,228)"
     "rgb(254,217,166)"],
    :6
    ["rgb(251,180,174)"
     "rgb(179,205,227)"
     "rgb(204,235,197)"
     "rgb(222,203,228)"
     "rgb(254,217,166)"
     "rgb(255,255,204)"],
    :7
    ["rgb(251,180,174)"
     "rgb(179,205,227)"
     "rgb(204,235,197)"
     "rgb(222,203,228)"
     "rgb(254,217,166)"
     "rgb(255,255,204)"
     "rgb(229,216,189)"],
    :8
    ["rgb(251,180,174)"
     "rgb(179,205,227)"
     "rgb(204,235,197)"
     "rgb(222,203,228)"
     "rgb(254,217,166)"
     "rgb(255,255,204)"
     "rgb(229,216,189)"
     "rgb(253,218,236)"],
    :9
    ["rgb(251,180,174)"
     "rgb(179,205,227)"
     "rgb(204,235,197)"
     "rgb(222,203,228)"
     "rgb(254,217,166)"
     "rgb(255,255,204)"
     "rgb(229,216,189)"
     "rgb(253,218,236)"
     "rgb(242,242,242)"],
    :type "qual"},
   :YlGnBu
   {:3 ["rgb(237,248,177)" "rgb(127,205,187)" "rgb(44,127,184)"],
    :4
    ["rgb(255,255,204)"
     "rgb(161,218,180)"
     "rgb(65,182,196)"
     "rgb(34,94,168)"],
    :5
    ["rgb(255,255,204)"
     "rgb(161,218,180)"
     "rgb(65,182,196)"
     "rgb(44,127,184)"
     "rgb(37,52,148)"],
    :6
    ["rgb(255,255,204)"
     "rgb(199,233,180)"
     "rgb(127,205,187)"
     "rgb(65,182,196)"
     "rgb(44,127,184)"
     "rgb(37,52,148)"],
    :7
    ["rgb(255,255,204)"
     "rgb(199,233,180)"
     "rgb(127,205,187)"
     "rgb(65,182,196)"
     "rgb(29,145,192)"
     "rgb(34,94,168)"
     "rgb(12,44,132)"],
    :8
    ["rgb(255,255,217)"
     "rgb(237,248,177)"
     "rgb(199,233,180)"
     "rgb(127,205,187)"
     "rgb(65,182,196)"
     "rgb(29,145,192)"
     "rgb(34,94,168)"
     "rgb(12,44,132)"],
    :9
    ["rgb(255,255,217)"
     "rgb(237,248,177)"
     "rgb(199,233,180)"
     "rgb(127,205,187)"
     "rgb(65,182,196)"
     "rgb(29,145,192)"
     "rgb(34,94,168)"
     "rgb(37,52,148)"
     "rgb(8,29,88)"],
    :type "seq"},
   :Set1
   {:3 ["rgb(228,26,28)" "rgb(55,126,184)" "rgb(77,175,74)"],
    :4
    ["rgb(228,26,28)"
     "rgb(55,126,184)"
     "rgb(77,175,74)"
     "rgb(152,78,163)"],
    :5
    ["rgb(228,26,28)"
     "rgb(55,126,184)"
     "rgb(77,175,74)"
     "rgb(152,78,163)"
     "rgb(255,127,0)"],
    :6
    ["rgb(228,26,28)"
     "rgb(55,126,184)"
     "rgb(77,175,74)"
     "rgb(152,78,163)"
     "rgb(255,127,0)"
     "rgb(255,255,51)"],
    :7
    ["rgb(228,26,28)"
     "rgb(55,126,184)"
     "rgb(77,175,74)"
     "rgb(152,78,163)"
     "rgb(255,127,0)"
     "rgb(255,255,51)"
     "rgb(166,86,40)"],
    :8
    ["rgb(228,26,28)"
     "rgb(55,126,184)"
     "rgb(77,175,74)"
     "rgb(152,78,163)"
     "rgb(255,127,0)"
     "rgb(255,255,51)"
     "rgb(166,86,40)"
     "rgb(247,129,191)"],
    :9
    ["rgb(228,26,28)"
     "rgb(55,126,184)"
     "rgb(77,175,74)"
     "rgb(152,78,163)"
     "rgb(255,127,0)"
     "rgb(255,255,51)"
     "rgb(166,86,40)"
     "rgb(247,129,191)"
     "rgb(153,153,153)"],
    :type "qual"}})