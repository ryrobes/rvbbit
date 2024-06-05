(ns rvbbit-backend.util
  (:require
   [clojure.string :as cstr]
   [clojure.pprint :as pprint]
   [clojure.walk :as walk]
   [clojure.java.io :as io]
   [clojure.data.csv :as csv]
   [hikari-cp.core :as hik]
   [clojure.core.async :as async]
   [clojure.data.json :as json2]
   [clojure.edn :as edn]
   [cheshire.core :as json]
   [honey.sql :as honey]
   [talltale.core :as tales]
   [chime.core :as chime]
   [clojure.spec.test.alpha :as stest]
   [clojure.spec.alpha :as s]
   [clojure.java.jdbc :as jdbc]
   [rvbbit-backend.config :as config]
   [clojure.java.shell :refer [sh]]
   [puget.printer :as puget]
   ;[datalevin.core :as d]
   )
  (:import [java.lang.management ManagementFactory]
           java.time.Instant
           java.time.LocalTime
           java.time.LocalDate
           java.time.ZoneId
           java.time.format.DateTimeFormatter
           ;[java.nio.charset.Charset Charset]
           [java.time LocalTime Duration Instant ZonedDateTime ZoneId Period DayOfWeek]
           [java.text SimpleDateFormat]
           [java.util Date TimeZone Calendar]
           [java.awt.image BufferedImage]
           [javax.imageio ImageIO]
           [java.io File]
           [java.awt Color]
           [jline TerminalFactory]
           [java.time.format TextStyle]
           [java.util Locale]
           [java.security MessageDigest]
           [java.util Base64]
           ;;[org.apache.commons.codec.binary Base64]
          ;;  (java.awt.image BufferedImage)
          ;;  (javax.imageio ImageIO)
          ;;  (java.io ByteArrayInputStream)
          ;;  (org.apache.commons.codec.binary Base64)

           java.time.format.DateTimeFormatter))

(def rvbbit "
,...............................................................................
................................................................................
...............,........,## ....................................................
...............*#### ....#(#####................................................
................###,#####*##... ####@...........................................
.................##,((#(/####@.....,###@........................................
................./##.   ./((*###@...#//###......................................
..................###. .(   /(/(###..#%(###%....................................
...................,##.      ,(((.##@. ,((.##&..................................
.....................###  ,   ,*//.*##@../(.###.................................
.......................###. .   ,,(  ##%@..*,,##(...............................
................((*/.... ###.. ,   /  .#(@&..,,###..............................
...........................(####        (/#(#(((@@##%%%(........................
.............########...((...../#####.    ((#(((((/(/#(@@@@@@@..................
....................................                .*((##((#@@@................
........................(####(...####(,              ......((((@@@..............
.......##########,..//......................        .,###.../((((@@@............
......................((##(..(########..... (#((((((. . .     (((((@@@..........
......................     ....................(((((((((((((((((((/((@@@........
...........########### .......###....(/................../((((((@@&(............
.................................. ..............    ((,**( ((/(/(#@@@,.#.......
.................((( .*(..(/....(........(/..... ,.,,/,  .#..,,,,,**/***........
....................((&..((. (....(/   .... (............ ..,*//(((# ...........
......................(((../(..(/... (  . . ..*,..........*(*...................
........................ ((&..(*.*(... (/....,..... ...%(##.....................
...........................((( .((. (,. .    . /. *. &(# .......................
..............................((&..(  ((. (  //   *(((..........................
................................#((  ((  (  # ./(((,............................
................................../((% ,(  (/. #................................
.....................................(((  (((( .................................
.......................................(((((....................................
................................................................................
")

(def managed-atoms (atom {}))

(declare pp)

(defn deselect-keys [m ks]
  (apply dissoc m ks))

(defn print-ansi-art [filename]
  (with-open [reader (clojure.java.io/reader filename)]
    (doseq [line (line-seq reader)]
      (println line))))

(defn index-of [coll item]
  (first (keep-indexed #(when (= %2 item) %1) coll)))

;; (defn sha-256 [s]
;;   (let [hash (MessageDigest/getInstance "SHA-256")
;;         encoder (Base64/getEncoder)
;;         bytes (.getBytes s "UTF-16")]
;;     (.update hash bytes)
;;     (-> (.digest hash)
;;         (.encodeToString encoder)
;;         (cstr/replace "/" "_"))))

;; (defn current-datetime-parts []
;;   (try
;;     (let [now (java.time.ZonedDateTime/now)
;;           day-of-week (.getDayOfWeek now)
;;           month (.getMonth now)
;;           formatter (java.time.format.DateTimeFormatter/ofPattern "EEEE, MMMM d h:mma" java.util.Locale/US)
;;           formatter-seconds (java.time.format.DateTimeFormatter/ofPattern "EEEE, MMMM d h:mm:ssa" java.util.Locale/US)
;;           now-str (.format formatter now)
;;           now-seconds-str (.format formatter-seconds now)
;;           day (.getDayOfMonth now)
;;           nth (case (mod day 10)
;;                 1 (if (= day 11) "th" "st")
;;                 2 (if (= day 12) "th" "nd")
;;                 3 (if (= day 13) "th" "rd")
;;                 "th")]
;;       {:year (.getYear now)
;;        :month (.getMonthValue now)
;;        :month-name (.name month)
;;        :day day
;;        :day-of-week-int (.getValue day-of-week)
;;        :day-of-week (.name day-of-week)
;;        :hour (.getHour now)
;;        :minute (.getMinute now)
;;        :second (.getSecond now)
;;        :quarter (inc (quot (.getMonthValue now) 4))
;;        :am-pm (if (< (.getHour now) 12) "AM" "PM")
;;        :now now-str
;;        :now-seconds now-seconds-str
;;        :nth nth})
;;     (catch Throwable e {:time-atom-error (str "Error! " e)})))

(defn current-datetime-parts []
  (try
    (let [now (java.time.ZonedDateTime/now)
          day-of-week (.getDayOfWeek now)
          month (.getMonth now)
          day (.getDayOfMonth now)
          nth (case (mod day 10)
                1 (if (= day 11) "th" "st")
                2 (if (= day 12) "th" "nd")
                3 (if (= day 13) "th" "rd")
                "th")
          formatter (java.time.format.DateTimeFormatter/ofPattern (str "EEEE, MMMM d'" nth "' h:mma") java.util.Locale/US)
          formatter-seconds (java.time.format.DateTimeFormatter/ofPattern (str "EEEE, MMMM d'" nth "' h:mm:ssa") java.util.Locale/US)
          now-str (.format formatter now)
          now-seconds-str (.format formatter-seconds now)]
      {:year (.getYear now)
       :month (.getMonthValue now)
       :month-name (.name month)
       :day day
       :day-of-week-int (.getValue day-of-week)
       :day-of-week (.name day-of-week)
       :hour (.getHour now)
       :minute (.getMinute now)
       :second (.getSecond now)
       :quarter (inc (quot (.getMonthValue now) 4))
       :am-pm (if (< (.getHour now) 12) "AM" "PM")
       :now now-str
       :now-seconds now-seconds-str
       :nth nth})
    (catch Throwable e {:time-atom-error (str "Error! " e)})))

;; (defn break-out-parts [clause]
;;   (cond
;;     (not (coll? clause)) [clause]
;;     (some #{:and :or} (take 1 clause)) (cons clause (mapcat break-out-parts (rest clause)))
;;     :else (cons clause (mapcat break-out-parts (rest clause)))))

;; (defn where-dissect [clause]
;;   (let [parts (break-out-parts clause)]
;;     (cons clause (remove #(= clause %) parts))))

(defn break-out-parts [clause]
  (cond
    (not (vector? clause)) []
    (some #{:and :or} (take 1 clause)) (cons clause (mapcat break-out-parts (rest clause)))
    :else (cons clause (mapcat break-out-parts (rest clause)))))

(defn where-dissect [clause]
  (let [parts (break-out-parts clause)]
    (cons clause (remove #(= clause %) parts))))

(defn thaw-atom
  "Thaws an atom from disk or creates a new one if the file doesn't exist."
  [initial-state file-path & [out?]]
  (let [file (io/file file-path)
        state (if (.exists file)
                (with-open [rdr (io/reader file)]
                  (try (edn/read (java.io.PushbackReader. rdr))
                       (catch Exception e (do (pp [:thaw-atom-error!!!! file e]) (System/exit 0)))))
                initial-state)
        a (atom state)]
    (when (not out?) ;; we dont want to manage some of these, do it manually
      (swap! managed-atoms assoc file-path a))
    a))

(defn serializable? [value]
  (and (not (clojure.string/includes? (pr-str value) "#object"))
       (not (clojure.string/includes? (pr-str value) "#error"))))

;; (defn freeze-atoms
;;   "Freezes all managed atoms to disk."
;;   []
;;   (doseq [[file-path a] @managed-atoms]
;;     (pp ["  " :freezing-atom file-path])
;;     (with-open [wtr (io/writer file-path)]
;;       (binding [*out* wtr]
;;         (if (cstr/includes? (str file-path) "signals")
;;           (clojure.pprint/pprint @a)
;;           (prn @a))))))

;; (defn freeze-atoms
;;   "Freezes all managed atoms to disk."
;;   []
;;   (doall
;;    (pmap (fn [[file-path a]]
;;            (pp ["  " :freezing-atom file-path])
;;            (with-open [wtr (io/writer file-path)]
;;              (binding [*out* wtr] ;; selective pretty print formatting 
;;                (if (or (cstr/includes? (str file-path) "signals.")
;;                        (cstr/includes? (str file-path) "rules.")
;;                        (cstr/includes? (str file-path) "errors.") 
;;                        (cstr/includes? (str file-path) "autocomplete") 
;;                        (cstr/includes? (str file-path) "training-atom.")
;;                        (cstr/includes? (str file-path) "sql-cache.")
;;                        (cstr/includes? (str file-path) "solvers."))
;;                  (clojure.pprint/pprint @a)
;;                  (prn @a)))))
;;          @managed-atoms)))


;; (defn freeze-atoms
;;   "Freezes all managed atoms to disk."
;;   []
;;   (doall
;;    (pmap (fn [[file-path a]]
;;            (with-open [wtr (io/writer file-path)]
;;              (binding [*out* wtr] ;; selective pretty print formatting 
;;                (if (or (cstr/includes? (str file-path) "signals.")
;;                        (cstr/includes? (str file-path) "rules.")
;;                        (cstr/includes? (str file-path) "errors.")
;;                        (cstr/includes? (str file-path) "autocomplete")
;;                        (cstr/includes? (str file-path) "training-atom.")
;;                        (cstr/includes? (str file-path) "sql-cache.")
;;                        (cstr/includes? (str file-path) "solvers."))
;;                  (clojure.pprint/pprint @a)
;;                  (prn @a))))
;;            (let [size-in-bytes (java.nio.file.Files/size (java.nio.file.Paths/get file-path (into-array String [])))
;;                  size-in-mb (/ size-in-bytes 1048576.0)]
;;              (pp ["  " :freezing-atom file-path :is-now (format "%.2f" size-in-mb) "MB"])))
;;          @managed-atoms)))


(defn freeze-atoms
  "Freezes all managed atoms to disk."
  []
  (doall
   (pmap (fn [[file-path a]]
           (let [wtr (io/writer file-path)]
             (try
               (binding [*out* wtr] ;; selective pretty print formatting 
                 (if (or (cstr/includes? (str file-path) "signals.")
                         (cstr/includes? (str file-path) "rules.")
                         (cstr/includes? (str file-path) "errors.")
                         (cstr/includes? (str file-path) "autocomplete")
                         (cstr/includes? (str file-path) "training-atom.")
                         (cstr/includes? (str file-path) "sql-cache.")
                         (cstr/includes? (str file-path) "solvers."))
                   (clojure.pprint/pprint @a)
                   (prn @a)))
               (finally
                 (.close wtr))))
           (let [size-in-bytes (java.nio.file.Files/size (java.nio.file.Paths/get file-path (into-array String [])))
                 size-in-mb (/ size-in-bytes 1048576.0)
                 size-in-mb-rounded (/ (Math/round (* size-in-mb 100.0)) 100.0)]
             (pp ["  " :freezing-atom file-path size-in-mb-rounded :mb])))
         @managed-atoms)))


;; (defn freeze-atoms ;; slow as all hell
;;   "Freezes all managed atoms to disk."
;;   []
;;   (doseq [[file-path a] @managed-atoms]
;;     (with-open [wtr (io/writer file-path)]
;;       (binding [*out* wtr]
;;         (clojure.pprint/pprint @a)))))

(defn freeze-atom
  "Freezes a single atom to disk."
  [file-path]
  (let [a (get @managed-atoms file-path)]
    (when a
      (with-open [wtr (io/writer file-path)]
        (binding [*out* wtr]
          (prn @a))))))

(def terminal (TerminalFactory/get))

(defn get-terminal-width []
  (try
    (.getWidth terminal)
    (catch Throwable _ 85)))

;; (defn get-terminal-width []
;;   (try
;;     (.getWidth (TerminalFactory/get))
;;     (catch Throwable _ 85)))

;; (defn avg [nums]
;;   (when (seq nums)
;;     (/ (reduce + nums) (count nums))))

(defn avg [nums]
  (when (seq nums)
    (Math/round (/ (reduce + nums) (double (count nums))))))

(defn get-system-load-average []
  (let [os-bean (java.lang.management.ManagementFactory/getOperatingSystemMXBean)]
    (.getSystemLoadAverage os-bean)))

(def debug-level (get config/settings :debug-level 0))

;(def debug? false) ;false) ; false)
;(defn ifdebug? [x] (when debug? x))

(defn replace-multiple [s replacements]
  (reduce (fn [s [k v]] (cstr/replace s k v)) s replacements))

(defn json-to-edn [json-str]
  ;;(pp [:json-to-end-debug! (str json-str)])
  (json2/read-str (str json-str) :key-fn keyword))

(defn get-colors [image-path]
  (let [image (ImageIO/read (File. image-path))
        width (.getWidth image)
        height (.getHeight image)
        colors (for [x (range width)
                     y (range height)]
                 (let [rgb (.getRGB image x y)
                       color (Color. rgb)]
                   (format "#%02x%02x%02x"
                           (.getRed color)
                           (.getGreen color)
                           (.getBlue color))))]
    (vec (map first (->> (frequencies colors)
                         (sort-by val)
                         (reverse)
                         (take 50))))))

(defn hue-to-rgb [p q t]
  (let [t (cond
            (< t 0) (+ t 1)
            (> t 1) (- t 1)
            :else t)]
    (cond
      (< (* 6 t) 1) (+ p (* (- q p) 6 t))
      (< (* 2 t) 1) q
      (< (* 3 t) 2) (+ p (* (- q p) 6 (- 0.6666667 t)))
      :else p)))

(defn hsl-to-rgb [h s l]
  (let [s (/ s 100.0)
        l (/ l 100.0)
        q (if (<= l 0.5) (* l (+ 1 s)) (- (+ l s) (* l s)))
        p (* 2 l q)
        h (/ h 360.0)]
    (map (fn [x] (int (* 255 (hue-to-rgb p q x))))
         [(+ h 0.3333333) h (- h 0.3333333)])))

(defn rgb-to-hsl [r g b]
  (let [r (/ r 255.0)
        g (/ g 255.0)
        b (/ b 255.0)
        max (max r g b)
        min (min r g b)
        l (/ (+ max min) 2)
        delta (- max min)
        s (if (== delta 0.0)
            0.0
            (/ delta (if (<= l 0.5) (+ max min) (- 2 max min))))
        h (cond
            (== delta 0) 0
            (== max r) (rem (/ (- g b) delta) 6)
            (== max g) (+ 2 (/ (- b r) delta))
            (== max b) (+ 4 (/ (- r g) delta)))
        hue (* 60 (if (< h 0) (+ h 6) h))]
    [(mod hue 360) (* 100 s) (* 100 l)]))

(defn hex-to-rgb [hex]
  (map #(Integer/parseInt % 16)
       [(subs hex 1 3) (subs hex 3 5) (subs hex 5 7)]))

(defn rgb-to-xyz [r g b]
  (let [r (if (> r 0.04045) (Math/pow ((r + 0.055) / 1.055) 2.4) (/ r 12.92))
        g (if (> g 0.04045) (Math/pow ((g + 0.055) / 1.055) 2.4) (/ g 12.92))
        b (if (> b 0.04045) (Math/pow ((b + 0.055) / 1.055) 2.4) (/ b 12.92))
        r (* r 100)
        g (* g 100)
        b (* b 100)]
    [(+ (* r 0.4124) (* g 0.3576) (* b 0.1805))
     (+ (* r 0.2126) (* g 0.7152) (* b 0.0722))
     (+ (* r 0.0193) (* g 0.1192) (* b 0.9505))]))

(defn xyz-to-cie [x y z]
  (let [total (+ x y z)]
    [(if (zero? total) 0 (/ x total))
     (if (zero? total) 0 (/ y total))]))

(defn hex-to-cie [hex]
  (let [[r g b] (hex-to-rgb hex)
        [x y z] (rgb-to-xyz (/ r 255.0) (/ g 255.0) (/ b 255.0))]
    (xyz-to-cie x y z)))

(defn cie-to-xyz [x y brightness]
  (let [Y brightness
        X (/ (* Y x) y)
        Z (/ (* Y (- 1 x y)) y)]
    [X Y Z]))

(defn xyz-to-rgb [X Y Z]
  (let [r (/ (+ (* X  3.2406) (* Y -1.5372) (* Z -0.4986)) 100)
        g (/ (+ (* X -0.9689) (* Y  1.8758) (* Z  0.0415)) 100)
        b (/ (+ (* X  0.0557) (* Y -0.2040) (* Z  1.0570)) 100)]
    (mapv #(max 0 (min 255 (Math/round (* 255 (if (> % 0.0031308)
                                                (Math/pow (* % 1.055) (/ 1 2.4))
                                                (* % 12.92)))))) [r g b])))

(defn rgb-to-hex2 [r g b]
  (format "#%02x%02x%02x" r g b))

(defn cie-to-hex [x y brightness]
  (pp [:cie-to-hex y brightness])
  (let [[X Y Z] (cie-to-xyz x y brightness)
        [r g b] (xyz-to-rgb X Y Z)]
    (rgb-to-hex2 r g b)))

(defn hue-to-rgb2 [hue]
  (try
    (let [hue' (rem (/ hue 182.04) 360) ; Scale hue to [0, 360] and wrap around
          sector (/ hue' 60)
          c 1 ; Assuming full saturation
          x (* c (- 1 (Math/abs (- (rem sector 2) 1))))
          m 0 ; Assuming brightness at midpoint for simplicity
          rgb' (cond
                 (< sector 1) [c x 0]
                 (< sector 2) [x c 0]
                 (< sector 3) [0 c x]
                 (< sector 4) [0 x c]
                 (< sector 5) [x 0 c]
                 :else        [c 0 x])
          [r g b] (mapv #(+ m (* 255 %)) rgb')]
      (mapv int [r g b]))
    (catch Exception _ "")))

(defn rgb-to-hex2 [r g b]
  (format "#%02x%02x%02x" r g b))

(defn hue-to-hex [hue]
  (let [[r g b] (hue-to-rgb2 hue)
        out (rgb-to-hex2 r g b)]
    (if (= out "#nullnullnull") "#00000000" out)))

;; (defn hex-to-hue-sat [hex]
;;   (let [[r g b] (hex-to-rgb hex)
;;         [h s] (rgb-to-hsl r g b)]
;;     [(int h) (int s)]))

(defn hex-to-hue-sat [hex]
  (let [[r g b] (hex-to-rgb hex)
        [h s l] (rgb-to-hsl r g b)
        hue-scaled (* h (/ 65535 360))] ; Scale hue to Philips Hue range
    [(int hue-scaled) 
     ;(int s)
     254
     ])) ; Scale saturation to [0, 254]

;; (defn bytes-to-mb [bytes]
;;   (let [mb (/ bytes 1048576.0)
;;         formatted-mb0 (-> mb
;;                           (.toFixed 0)
;;                           js/parseFloat
;;                           str
;;                           (.toLocaleString js/Intl.NumberFormat "en-US"))
;;         ;formatted-mb (* 1.8 formatted-mb0)
;;         ]
;;     ;(str (nf formatted-mb) "MB, (" (nf formatted-mb0) "MB)")
;;     (str (nf formatted-mb0) "MB")))


(defn bytes-to-mb [bytes]
  (let [mb (/ bytes 1048576.0)
        formatted-mb0 (format "%.0f" mb)]
    (str formatted-mb0 "MB")))

;; Example usage
;(hex-to-cie "#ff0000")  ;; For red color


(defn interpolate-hsl [hsl1 hsl2 factor]
  (map (fn [v1 v2] (+ v1 (* (- v2 v1) factor))) hsl1 hsl2))

(defn hex-to-rgb [hex]
  (map #(Integer/parseInt (subs hex %1 (+ %1 2)) 16) [1 3 5]))

(defn rgb-to-hex [rgb]
  (format "#%02X%02X%02X" (nth rgb 0) (nth rgb 1) (nth rgb 2)))

(defn generate-gradient [hex1 hex2 steps]
  (let [rgb1 (hex-to-rgb hex1)
        rgb2 (hex-to-rgb hex2)
        hsl1 (rgb-to-hsl (nth rgb1 0) (nth rgb1 1) (nth rgb1 2))
        hsl2 (rgb-to-hsl (nth rgb2 0) (nth rgb2 1) (nth rgb2 2))]
    (map (fn [step]
           (let [factor (/ step (dec steps))
                 hsl (interpolate-hsl hsl1 hsl2 factor)
                 rgb (hsl-to-rgb (nth hsl 0) (nth hsl 1) (nth hsl 2))]
             (rgb-to-hex rgb)))
         (range steps))))


;; (defn save-base64-json-to-png [b64-json file-path]
;;   (let [decoder (java.util.Base64/getDecoder)
;;         data (.decode decoder (get (json2/read-str b64-json :key-fn keyword) :data))
;;         output-stream (io/output-stream file-path)]
;;     (.write output-stream data)
;;     (.flush output-stream)
;;     (.close output-stream)
;;     (println "Saved PNG to" file-path)))

;; (defn save-base64-to-png [b64-string file-path]
;;   (let [decoder (java.util.Base64/getDecoder)
;;         data (.decode decoder b64-string)
;;         output-stream (io/output-stream file-path)]
;;     (.write output-stream data)
;;     (.flush output-stream)
;;     (.close output-stream)
;;     (println "Saved PNG to" file-path)
;;     (spit (str "/home/ryanr/b64-blob.txt") b64-string)))


(defn base64-decode [b64-string]
  (.decode (java.util.Base64/getDecoder) b64-string))

(defn save-base64-to-webp [b64-string file-path]
  (let [;b64-string (str "{\"data\":\"" b64-string "\"}")
        data-bytes (base64-decode b64-string)]
    (with-open [out-stream (io/output-stream (io/file file-path))]
      (.write out-stream data-bytes))
    ;(println "Saved PNG to" file-path)
    (str file-path)
    ;(spit (str "/home/ryanr/b64-blob.txt") b64-string)
    ))

(defn base64-decode1 [b64-string]
  (let [image-data (second (re-find #"data:image/png;base64,(.*)" b64-string))]
    (.decode (java.util.Base64/getDecoder) image-data)))

(defn save-base64-to-png [b64-string file-path]
  (let [data-bytes (base64-decode1 b64-string)]
    (with-open [out-stream (io/output-stream (io/file file-path))]
      (.write out-stream data-bytes))
    (str file-path)))

(defn base64-decode2 [b64-string]
  (let [image-data (second (re-find #"data:image/jpeg;base64,(.*)" b64-string))]
    (.decode (java.util.Base64/getDecoder) image-data)))

(defn save-base64-to-jpeg [b64-string file-path]
  (let [data-bytes (base64-decode2 b64-string)]
    (with-open [out-stream (io/output-stream (io/file file-path))]
      (.write out-stream data-bytes))))

;; (defn write-base64-image [base64-str file-path]
;;   (let [decoder (java.util.Base64/getDecoder)
;;         image-bytes (.decode decoder base64-str)]
;;     (with-open [out (clojure.java.io/output-stream file-path)]
;;       (.write out image-bytes))))

;; (defn save-base64-to-png2 [b64-string file-path]
;;   (let [decoder (java.util.Base64/getDecoder)
;;         data-bytes (.decode decoder b64-string)]
;;     (with-open [out-stream (io/output-stream (io/file file-path))]
;;       (.write out-stream data-bytes))
;;     (println "Saved PNG to" file-path)))


;; (defn base64-to-buffered-image [base64-str]
;;   (let [image-bytes (Base64/decodeBase64 base64-str)
;;         input-stream (ByteArrayInputStream. image-bytes)]
;;     (ImageIO/read input-stream)))

;; (defn save-image-to-file [^BufferedImage img file-path]
;;   (ImageIO/write img "png" (io/file file-path)))

;; (defn process-and-save-image [base64-str file-path]
;;   (let [img (base64-to-buffered-image base64-str)]
;;     (save-image-to-file img file-path)
;;     (println "Saved PNG to" file-path)))


(defn uptime-seconds []
  (let [runtime-bean (java.lang.management.ManagementFactory/getRuntimeMXBean)
        uptime-ms (.getUptime runtime-bean)
        uptime-seconds (/ uptime-ms 1000.0)]  ;; Note the 1000.0 for floating point division
    (Math/round uptime-seconds)))

(defn flatten-one-level [v]
  (vec (mapcat (fn [x] (if (coll? (first x)) x (list x))) v)))

(defn millis-to-date-string [millis]
  (let [instant (Instant/ofEpochMilli millis)
        zonedDateTime (.atZone instant (ZoneId/of "UTC"))
        formatter (DateTimeFormatter/ofPattern "yyyy-MM-dd HH:mm:ss")]
    (.format formatter zonedDateTime)))

(defn bytes-to-mb [bytes]
  (int (/ bytes (Math/pow 1024 2))))

(defn get-current-timestamp []
  (let [zonedDateTime (ZonedDateTime/now (ZoneId/systemDefault))
        formatter (DateTimeFormatter/ofPattern "yyyy-MM-dd HH:mm:ss")]
    (.format formatter zonedDateTime)))

;; simple jvm stats for debug

(defn get-memory-usage []
  (let [memory-bean (ManagementFactory/getMemoryMXBean)
        memory-usage->map (fn [m]
                            {:init (bytes-to-mb (.getInit m))
                             :used (bytes-to-mb (.getUsed m))
                             :committed (bytes-to-mb (.getCommitted m))
                             :max (bytes-to-mb (.getMax m))})]
    {:heap-usage (memory-usage->map (.getHeapMemoryUsage memory-bean))
     :non-heap-usage (memory-usage->map (.getNonHeapMemoryUsage memory-bean))}))

;; (defn template-replace [replacements s]
;;   (reduce (fn [str [key value]]
;;             (let [value (get :code value value)]
;;               (cstr/replace str (re-pattern (cstr/join ["\\{\\{" key "\\}\\}"])) value)))
;;           s
;;           replacements))

(defn template-replace [replacements s]
  (reduce (fn [s [key value]]
            (let [value (get :code value value)
                  value (if value (str value) "")] ;; force a string, since this is a string operation
              (cstr/replace s (re-pattern (cstr/join ["\\{\\{" key "\\}\\}"])) value)))
          s
          replacements))

(defn get-thread-count []
  (let [thread-bean (ManagementFactory/getThreadMXBean)]
    {:total-started (.getTotalStartedThreadCount thread-bean)
     :peak (.getPeakThreadCount thread-bean)
     :current (.getThreadCount thread-bean)
     :daemon (.getDaemonThreadCount thread-bean)}))

(defn abs-file-path [fpath]
  (str ;(.getAbsolutePath (clojure.java.io/file fpath))
   (.getCanonicalPath (clojure.java.io/file fpath))))


;; odds and ends

;; (defn time-seq [v]
;;   (let [{:keys [days minutes seconds weeks months at starts tz]
;;          :or {tz (ZoneId/systemDefault)}} (apply hash-map v)
;;         zone-id (if (instance? String tz) (ZoneId/of tz) tz)
;;         starting-instant (if starts
;;                            (-> (LocalDate/parse starts)
;;                                (.atTime (if at (LocalTime/of (quot at 100) (mod at 100)) (LocalTime/MIDNIGHT)))
;;                                (.atZone zone-id)
;;                                .toInstant)
;;                            (Instant/now))]
;;     (cond
;;       days
;;       (if at
;;         (chime/periodic-seq
;;          (-> starting-instant
;;              (.plus (Period/ofDays days)))
;;          (Period/ofDays days))
;;         (chime/periodic-seq starting-instant (Period/ofDays days)))

;;       minutes
;;       (if at
;;         (chime/periodic-seq
;;          (-> (LocalTime/of (quot at 100) (mod at 100))
;;              (.adjustInto (ZonedDateTime/now zone-id))
;;              .toInstant)
;;          (Duration/ofMinutes minutes))
;;         (chime/periodic-seq (Instant/now) (Duration/ofMinutes minutes)))

;;       seconds
;;       (if at
;;         (chime/periodic-seq
;;          (-> (LocalTime/of (quot at 100) (mod at 100))
;;              (.adjustInto (ZonedDateTime/now zone-id))
;;              .toInstant)
;;          (Duration/ofSeconds seconds))
;;         (chime/periodic-seq (Instant/now) (Duration/ofSeconds seconds)))

;;       weeks
;;       (if at
;;         (chime/periodic-seq
;;          (-> starting-instant
;;              (.plus (Period/ofWeeks weeks)))
;;          (Period/ofWeeks weeks))
;;         (chime/periodic-seq starting-instant (Period/ofWeeks weeks)))

;;       months
;;       (if at
;;         (chime/periodic-seq
;;          (-> (LocalDate/parse starts)
;;              (.atTime (if at (LocalTime/of (quot at 100) (mod at 100)) (LocalTime/MIDNIGHT)))
;;              (.atZone zone-id)
;;              (.plus (Period/ofMonths months))
;;              .toInstant)
;;          (Duration/ofDays 30)) ; approx
;;         (chime/periodic-seq starting-instant (Duration/ofDays 30)))

;;       :else
;;       (throw (IllegalArgumentException. "Unsupported time unit")))))


(defn time-seq [v]
  (let [{:keys [days minutes seconds hours weeks months at starts tz]
         :or {tz (ZoneId/systemDefault)}} (apply hash-map v)
        zone-id (if (instance? String tz) (ZoneId/of tz) tz)
        starting-instant (if starts
                           (-> (LocalDate/parse starts)
                               (.atTime (if at (LocalTime/of (quot at 100) (mod at 100)) (LocalTime/MIDNIGHT)))
                               (.atZone zone-id)
                               .toInstant)
                           (Instant/now))]
    (cond
      days
      (if at
        (chime/periodic-seq
         (-> starting-instant
             (.plus (Period/ofDays days)))
         (Period/ofDays days))
        (chime/periodic-seq starting-instant (Period/ofDays days)))

      hours
      (if at
        (chime/periodic-seq
         (-> (LocalTime/of (quot at 100) (mod at 100))
             (.adjustInto (ZonedDateTime/now zone-id))
             .toInstant)
         (Duration/ofHours hours))
        (chime/periodic-seq (Instant/now) (Duration/ofHours hours)))

      minutes
      (if at
        (chime/periodic-seq
         (-> (LocalTime/of (quot at 100) (mod at 100))
             (.adjustInto (ZonedDateTime/now zone-id))
             .toInstant)
         (Duration/ofMinutes minutes))
        (chime/periodic-seq (Instant/now) (Duration/ofMinutes minutes)))

      seconds
      (if at
        (chime/periodic-seq
         (-> (LocalTime/of (quot at 100) (mod at 100))
             (.adjustInto (ZonedDateTime/now zone-id))
             .toInstant)
         (Duration/ofSeconds seconds))
        (chime/periodic-seq (Instant/now) (Duration/ofSeconds seconds)))

      weeks
      (if at
        (chime/periodic-seq
         (-> starting-instant
             (.plus (Period/ofWeeks weeks)))
         (Period/ofWeeks weeks))
        (chime/periodic-seq starting-instant (Period/ofWeeks weeks)))

      months
      (if at
        (chime/periodic-seq
         (-> (LocalDate/parse starts)
             (.atTime (if at (LocalTime/of (quot at 100) (mod at 100)) (LocalTime/MIDNIGHT)))
             (.atZone zone-id)
             (.plus (Period/ofMonths months))
             .toInstant)
         (Duration/ofDays 30)) ; approx
        (chime/periodic-seq starting-instant (Duration/ofDays 30)))

      :else
      (throw (IllegalArgumentException. "Unsupported time unit")))))

(defn dequeue! [queue]
  (let [item (peek @queue)]
    (swap! queue pop)
    item))

(defn unix-to-date [timestamp]
  (java.util.Date. timestamp))

(defn today-yyyymmdd []
  (let [cal (Calendar/getInstance)
        fmt (SimpleDateFormat. "yyyy-MM-dd")]
    (.format fmt (.getTime cal))))

(defn date-str-to-unix [date-str]
  (let [fmt (SimpleDateFormat. "yyyy-MM-dd")
        date (.parse fmt date-str)]
    (.getTime date)))

(defn date-to-ymd [date] ;; blargh
  (let [cal (Calendar/getInstance)]
    (.setTime cal date)
    (str (.get cal Calendar/YEAR) "-"
         (inc (.get cal Calendar/MONTH)) "-" ; +1 since jan = 0
         (.get cal Calendar/DATE))))

(defn chan? [ch]
  (try (-> ch class .getName (= "clojure.core.async.impl.channels.ManyToManyChannel"))
       (catch Exception _ false)))

;; (defn generate-name []
;;   (let [quals ["of-the" "hailing-from" "banned-from" "of" "exiled-from" "from"]
;;         names [(tales/quality) (rand-nth [(tales/shape) (tales/color)]) (tales/animal) (rand-nth quals) (tales/landform)]]
;;     (cstr/replace (clojure.string/join "-" names) " " "-")))

(defn generate-name []
  (let [;quals ["of-the" "hailing-from" "banned-from" "of" "exiled-from"]
        names [(tales/quality) (rand-nth [(tales/shape) (tales/color)]) (tales/animal) ;(rand-nth quals) (tales/landform)
               ]]
    (str (cstr/replace (cstr/join "-" names) " " "-") "-" (rand-int 45))))

(defn channel-open? [ch]
  (async/offer! ch nil))

(defn accumulate-unique-runs [data]
  (let [merge-runs (fn [runs run]
                     (let [run-start (:start run)
                           run-end (:end run)]
                       (cond
                         ; If run with same start and end exists, return runs as is.
                         (some #(and (= (:start %) run-start)
                                     (= (:end %) run-end)) runs) runs
                         ; If run with same start exists without an end, replace if current has an end.
                         (some #(and (= (:start %) run-start) (nil? (:end %)) run-end) runs)
                         (conj (vec (remove #(and (= (:start %) run-start) (nil? (:end %))) runs)) run)
                         ; Otherwise, add run.
                         :else (conj runs run))))]
    (reduce (fn [acc entry]
              (reduce (fn [inner-acc [block-id run]]
                        (update inner-acc block-id (fn [runs]
                                                     (merge-runs (or runs []) run))))
                      acc (into [] entry)))
            {} data)))


;; (defn limit-elements [x]
;;   (try (cond
;;          (map? x) x ;; dont limit maps elements at all
;;          (instance? clojure.lang.IPending x) (doall (take db/sample-limit x))
;;          :else x)
;;        (catch Exception _ x)))


(defn format-duration [start-ms end-ms]
  (let [duration (java.time.Duration/ofMillis (- end-ms start-ms))
        hours (.toHours duration)
        minutes (.toMinutesPart duration)
        seconds (.toSecondsPart duration)
        out (str (when (pos? hours) (str hours " hour" (when (> hours 1) "s") ", "))
                 (when (pos? minutes) (str minutes " minute" (when (> minutes 1) "s") ", "))
                 seconds " second" (when (> seconds 1) "s"))]
    (if (= out "0 second")
      "less than a second" out)))

(defn format-duration-seconds [seconds]
  (let [duration (java.time.Duration/ofSeconds seconds)
        hours (.toHours duration)
        minutes (.toMinutesPart duration)
        seconds (.toSecondsPart duration)
        out (str (when (pos? hours) (str hours " hour" (when (> hours 1) "s") ", "))
                 (when (pos? minutes) (str minutes " minute" (when (> minutes 1) "s") ", "))
                 seconds " second" (when (> seconds 1) "s"))]
    (if (= out "0 second")
      "less than a second" out)))



;; (defn limited-t [x]
;;   (walk/postwalk limited2 x))


(defn nf [i]
  (pprint/cl-format nil "~:d" i))

;; (defn kvpaths
;;   ([m] (kvpaths [] m ()))
;;   ([prev m result]
;;    (reduce-kv (fn [res k v] (if (associative? v)
;;                               (let [kp (conj prev k)]
;;                                 (kvpaths kp v (conj res kp)))
;;                               (conj res (conj prev k))))
;;               result
;;               m)))

;; (defn keypaths
;;   ([m] (keypaths [] m ()))
;;   ([prev m result]
;;    (reduce-kv (fn [res k v] (if (associative? v)
;;                               (keypaths (conj prev k) v res)
;;                               (conj res (conj prev k))))
;;               result
;;               m)))

(defmacro timed-exec [expr] ;; expression based, not fn based
  `(let [start# (System/currentTimeMillis)
         result# ~expr
         end# (System/currentTimeMillis)]
     {:result result#
      :fn-start start#
      :fn-end end#
      :elapsed-ms (- end# start#)}))

(defn dissoc-in [map-in keypath]
  (let [base-kp (pop keypath)
        last-kp (last keypath)]
    (update-in map-in base-kp dissoc last-kp)))

(defn ppln [x]
  (puget/with-options {:width 330}
    (puget/cprint x)))

(defn pplno [_] nil) ;; TODO, goofy

(defn chan? [ch]
  (try (-> ch class .getName (= "clojure.core.async.impl.channels.ManyToManyChannel"))
       (catch Exception _ false)))

(defn unkeyword [k]
  (cstr/replace (str k) #":" "")
  ;(name k) ;; caused error? w namespaced keywords?
  )

(defn namespaced? [k]
  (and k (namespace k)))

(defn ms-to-iso8601 [ms]
  (let [instant (Instant/ofEpochMilli ms)
        formatter (DateTimeFormatter/ISO_INSTANT)]
    (.format formatter instant)))

(defn is-hiccup? [x] (cstr/includes? (str x) ":div")) ;; TODO, no need anymore?

(defn coll-of-maps? [x]
  (and (or (list? x)
           (vector? x))
       (not (empty? x)) (every? map? x)))

(defn has-nested-map-values? [x]
  (try (let [aa (some true?
                      (vec (if (and (or (list? x) (vector? x)) (not (empty? x)) (coll-of-maps? x))
                             (if (map? (first x))
                               (for [k (keys (first x))]
                                 (or (map?    (get (first x) k))
                                     (vector? (get (first x) k)))) false) false)))]
         (if aa true false)) (catch Exception _ true)))

(defn select-keypaths [m keypaths]
  (letfn [(extract [m path]
            (let [val (get-in m path)]
              (when val
                (assoc-in {} path val))))]
    (apply merge-with merge (map #(extract m %) keypaths))))

(defn data-typer [x] ;; legacy data rabbit code. TODO revisit
  ;(let [x (if (instance? clojure.lang.IPending x) (doall (take 20 x)) x)] ;; if lazy, do a limited realize
  (cond   (or (and (vector? x) (clojure.string/includes? (str x) "#object"))
              (and (vector? x) (fn? (first x))) ;; ?
              (and (vector? x) (cstr/starts-with? (str (first x)) ":re-com"))
              (and (vector? x) (cstr/starts-with? (str (first x)) ":vega"))
              (and (vector? x) (is-hiccup? x))) "render-object"
          (string? x) "string"
          (boolean? x) "boolean"
          ;(and (vector? x) (string-or-hiccup? x)) "hiccup"
          ;(and (coll-of-maps? x)
          ;     (not (has-nested-map-values? x)))
          (coll-of-maps? x)
          "rowset"  ;;; TODO, revisit. convoluted logic
          (vector? x) "vector"
          (or (and (map? x)
                   (contains? x :classname)
                   (contains? x :subprotocol)
                   (contains? x :subname))
              (and (map? x)
                   (contains? x :dbtype)
                   (contains? x :dbname)
                   (contains? x :user))) "jdbc-conn"
          (map? x)  "map"
          (list? x) "list"
          (nil? x)  "nil"
          (int? x)  "integer"
          (set? x)  "set"
          (instance? clojure.lang.IPending x) "lazy"
          (keyword? x) "keyword"
          (float? x) "float"
          (ifn? x) "function"
          :else "unknown"));)

(defn gen-coords [blocks] ;; awful attempt at auto coord / layout placing - revist later
  (let [start-x 100
        start-y 100
        step-x 330  ; 250 + 80
        step-y 330  ; 250 + 80
        placed-blocks (atom {})
        max-y (atom start-y)
        queue (atom blocks)]

    (while (seq @queue)
      (let [[from to] (first @queue)]
        (swap! queue rest)

        (when (not (contains? @placed-blocks from))
          (do
            (swap! placed-blocks assoc from [start-x @max-y])
            (swap! max-y #(+ % step-y))))

        (when (not (contains? @placed-blocks to))
          (let [[from-x from-y] (@placed-blocks from)
                to-x (+ from-x step-x)
                to-y (if (= from-x to-x) (+ from-y step-y) from-y)]
            (do
              (swap! placed-blocks assoc to [to-x to-y])
              (when (> to-y @max-y) (swap! max-y #(+ % step-y))))))))

    (mapv (fn [[block [x y]]] [block x y]) @placed-blocks)))

(defn coords-map [connections]
  (into {}
        (for [[bid x y] (gen-coords connections)]
          {bid {:x x :y y}})))

;; spec explorations TODO, revisit

(def nspc ['flowmaps.core 'clojure.set])

(defn get-fn-specs [ns-sym]
  (->> (s/registry)
       (filter #(= (namespace (key %)) (name ns-sym)))
       (into {})))

(defn instrument-ns [ns-syms & [un?]] ;; nice
  (doseq [ns-sym ns-syms]
    (->> (ns-publics ns-sym)
         keys
         (filter #(s/get-spec `%))
         (apply (if un?
                  stest/unstrument
                  stest/instrument)))))

(defn test-value [ns-syms args]
  (if (vector? args)
    (let [specs (mapcat get-fn-specs ns-syms)]
      (->> specs
           (map (fn [[k v]]
                  (let [args-spec (:args (s/spec v))]
                    [(str (namespace k) "/" (name k)) (not= (s/conform args-spec args) :clojure.spec.alpha/invalid)])))
           (into (sorted-map))))
    (println "The provided arguments should be a vector.")))

(defn write-csv [data filename]
  (let [header (map name (keys (first data)))
        rows (map vals data)]
    (with-open [writer (io/writer filename)]
      (csv/write-csv writer (cons header rows)))))

(defn write-json [data filename]
  (let [json-str (json/generate-string data)]
    (with-open [writer (io/writer filename)]
      (.write writer json-str))))

(defn map-vals [m f]
  (into {} (for [[k v] m] [k (f v)])))

(defn dissoc-recursive [data]
  (cond
    (map? data) (-> data
                    (dissoc :where)
                    (map-vals dissoc-recursive))
    (vector? data) (mapv dissoc-recursive data)
    :else data))

(defn lists-to-vectors [x]
  (cond
    (list? x) (vec (map lists-to-vectors x))
    (map? x) (into {} (map (fn [[k v]] [k (lists-to-vectors v)]) x))
    (coll? x) (mapv lists-to-vectors x)
    :else x))

(defn dissoc-in [map-in keypath]
  (let [base-kp (pop keypath)
        last-kp (last keypath)]
    (update-in map-in base-kp dissoc last-kp)))

;; (defn deep-flatten-fn [x]
;;   (if (coll? x)
;;     (mapcat deep-flatten-fn x)
;;     [x]))

;; (def deep-flatten (memoize deep-flatten-fn))

(defn millis-to-date-string [millis]
  (let [date (java.util.Date. millis)
        format (java.text.SimpleDateFormat. "yyyy-MM-dd HH:mm:ss")]
    (.format format date)))

(defn deep-flatten [x]
  (if (coll? x)
    (mapcat deep-flatten x)
    [x]))

(def boot-timestamp (str (-> (java.text.SimpleDateFormat. "yyyy-MM-dd HH:mm:ss")
                             (.format (java.util.Date.)))))

(defn unique-block-id-helper [base-name counter all-names]
  (let [new-name (if (= counter 0)
                   base-name
                   (str base-name "-" counter))]
    (if (contains? all-names new-name)
      (recur base-name (inc counter) all-names)
      new-name)))

;; given a proposed id, and a list of existing ids, and a list of reserved names, return a unique id adding -1, -2, etc
(defn unique-block-id [proposed existing-keys reserved-names]
  (let [all-names (set (concat existing-keys reserved-names))]
    (unique-block-id-helper proposed 0 all-names)))

;; same as above but takes a map of existing ids and key pairs and understands existing dupes
(defn generate-unique-names
  [name-map]
  (let [update-map-with-unique-name
        (fn [acc [k v]]
          (let [unique-name (unique-block-id-helper v 0 (set (vals acc)))]
            (assoc acc k unique-name)))]
    (reduce update-map-with-unique-name {} name-map)))

(defn reverse-map [m]
  (reduce (fn [acc [k v]] (assoc acc v k)) {} m))

(defn deep-remove-nil-values [item]
  (cond
    (map? item)
    (->> item
         (map (fn [[k v]] [k (deep-remove-nil-values v)]))
         (filter (fn [[k v]] (not (nil? v))))
         (into {}))

    (coll? item)
    (map deep-remove-nil-values item)

    :else
    item))

(defn remove-empty-key [arg-map key-name]
  (if (empty? (get arg-map key-name))
    (dissoc arg-map key-name)
    arg-map))

(defn remove-empty-sub-keys [arg-map]
  (-> arg-map
      (remove-empty-key :queries)
      (remove-empty-key :views)))

(defn remove-namespaced-keys [m]
  (walk/postwalk
   (fn [x]
     (if (map? x)
       (into {} (remove (fn [[k v]] (and (keyword? k) (namespace k))) x))
       x))
   m))

;;; dupe for dumb reasons 11/5/23 TODO
(defn pool-create [jdbc-body name]
  (delay (let [base {;:jdbc-url "jdbc:sqlite:db/system.db"
                     :minimum-idle 10 ;; min iodle connections to keep open?
                     ;:cache "shared"
                     :idle-timeout       600000
                     :max-lifetime       1800000
                     :pool-name name ;"system-db-pool"
                     ;:metric-registry instrument/metric-registry
                     :maximum-pool-size 30}
               bbase (merge base jdbc-body)]
           ;(ut/pp [:creating-conn-pool name])
           (hik/make-datasource bbase))))

(def log-jdbc-map2 {:jdbc-url "jdbc:sqlite:db/system-log.db"
                    :idle-timeout        600000
                    :max-lifetime       1800000
                    :transaction_mode "IMMEDIATE"
                    :journal_mode "WAL"
                    :cache "shared"})

(def log-jdbc-map {:jdbc-url "jdbc:clickhouse://10.174.1.150:8123/default"
                   :username "default"
                   :password "notofox"
                   :driver-class-name "ru.yandex.clickhouse.ClickHouseDriver"
                   :connection-timeout 5000
                   ;:maximum-pool-size 20
                   :max-lifetime 300000})

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


(defn deep-remove-keys2 [data keys-to-remove] ;; doent automatically remove underscore keys 
  (let [key-remove-set (set keys-to-remove)]
    (cond
      (map? data)
      (->> data
           (reduce-kv (fn [acc k v]
                        (if (key-remove-set k)
                          acc
                          (assoc acc k (deep-remove-keys v keys-to-remove)))) {})
           (into (empty data)))

      (vector? data)
      (mapv (fn [elem] (deep-remove-keys elem keys-to-remove)) data)

      :else data)))

(defn is-base64? [s]
  (if (and (string? s) (> (count s) 3000))
    (let [substring (subs s 0 3000)]
      (boolean (re-matches #"^(?:[A-Za-z0-9+/]{4})*(?:[A-Za-z0-9+/]{2}==|[A-Za-z0-9+/]{3}=)?$" substring)))
    false))

(defn replace-large-base64 [data]
  (cond
    (string? data) (if (and (is-base64? data) (> (count data) 3000)) "**huge base64 string**" data)
    (map? data) (into {} (map (fn [[k v]] [k (replace-large-base64 v)]) data))
    (vector? data) (mapv replace-large-base64 data)
    (list? data) (doall (map replace-large-base64 data)) ;; use doall to realize the lazy sequence
    :else data))

(defn limited [x & [flow-limit]]
  (let [flow-limit (if (nil? flow-limit) 50 flow-limit)]
    (letfn [(limited-helper [x]
              (cond
                (map? x) (into (empty x) (map (fn [[k v]] [k (limited-helper v)])) x)
                (coll? x) (into (empty x) (take flow-limit (map limited-helper x)))
                (and (not (string? x)) (not (keyword? x)) (not (number? x)))
                (try (doall (take flow-limit x)) (catch Exception _ x))
                :else x))]
      (limited-helper x))))

(defn greeting [name]
  (let [now (java.time.ZonedDateTime/now)
        hour (.getHour now)
        formatter (java.time.format.DateTimeFormatter/ofPattern "h:mma")]
    (str "Good "
         (cond
           (< hour 12) "morning"
           (< hour 18) "afternoon"
           :else "evening") " " name
         ", it is "
         (.format now formatter))))

;; (defn limited [x]
;;   (let [flow-limit 75]
;;     (letfn [(limited-helper [x]
;;               (cond
;;                 (map? x) (into (empty x) (map (fn [[k v]] [k (limited-helper v)])) x)
;;                 (vector? x) (vec (take flow-limit (map limited-helper x)))
;;                 (seq? x) (take flow-limit (map limited-helper x))
;;                 :else x))]
;;       (limited-helper x))))


;; (defn limited-deep [x]
;;   (walk/postwalk limited x))

;(def system-log-db {:datasource @(pool-create log-jdbc-map "system-log-db-pool")})

;; (defn insert-log-row! [log caller] ;; warning uses shit outside of what it is passed!!!
;;   (jdbc/with-db-connection [sdb system-log-db] ;; ?
;;     (let [llen (count log)
;;           ins (-> {:insert-into [:logs]
;;                    :values [{:booted (str boot-timestamp)
;;                              :ts (/ (.getTime (java.util.Date.)) 1000)
;;                              :hts (str (-> (java.text.SimpleDateFormat. "yyyy-MM-dd HH:mm:ss")
;;                                            (.format (java.util.Date.))))
;;                              :caller (str caller)
;;                              :type_key (str (first log))
;;                              :log_str (if (> llen 25000) (subs (str log) 0 25000) (str log))}]}
;;                   (honey/format {:pretty false :inline true}))]
;;       (try
;;         (jdbc/db-do-commands sdb ins)
;;         (catch Exception _
;;           (println (str :error-inserting-log! :error-inserting-log! log caller)))))))

;; (defn delay-execution [ms f]
;;   (future (Thread/sleep ms) (f)))

(defn delay-execution [ms f]
  (future (do (Thread/sleep ms) (f))))

;; (defn ppln [x]
;;   (if (>= debug-level 2)
;;     (puget/with-options {:width (get-terminal-width)}
;;       (puget/cprint x))
;;     ((fn [& _]) x)))



;; (defn ppa [x] ;; always print
;;   (puget/with-options {:width (get-terminal-width)}
;;     (puget/cprint x)))

(def console-lock (Object.))

(defn safe-cprint [x]
  (locking console-lock
    (puget/with-options {:width (get-terminal-width)}
      (puget/cprint x))))

;; (defn safe-cprint [x]
;;   (locking console-lock
;;     (try
;;       (puget/with-options {:width (get-terminal-width)}
;;         (puget/cprint x))
;;       (finally ;; pass
;;         ))))

;; (defn safe-cprint [x]
;;   (locking console-lock
;;     (try
;;       (puget/with-options {:width (get-terminal-width)}
;;         (let [output (str (java.time.LocalDateTime/now) " " (with-out-str (puget/cprint x)))]
;;           (async/thread (spit "console.log" output :append true)) ; Write to log file asynchronously
;;           (print output))) ; Print to console
;;       (catch Throwable t
;;         (let [error-message (str (java.time.LocalDateTime/now) " Error: " (.getMessage t))]
;;           (async/thread (spit "console.log" error-message :append true)) ; Write error to log file asynchronously
;;           (println error-message)))
;;       (finally ;; pass
;;         ))))


(defn ppln [x]
  (if (>= debug-level 2)
    (safe-cprint x)
    ((fn [& _]) x)))

(defn pp [x]
  (if (>= debug-level 1)
    (safe-cprint x)
    ((fn [& _]) x)))

;; (defn pp [x]
;;   (if (>= debug-level 1)
;;     (puget/with-options {:width (get-terminal-width)}
;;       (puget/cprint x))
;;     ((fn [& _]) x)))

(defn ppa [x] ;; always print
  (safe-cprint x))

;; (defn pp [& _]
;;   ;; Do nothing
;;   )

;; (defn ppln [& _]
;;   ;; Do nothing
;;   )


(defn keywordize-string [s] ;; technically spaces are valid in keywords, but they make it a bitch to reference properly
  (-> (str s)
      (cstr/replace #" " "-")
      (cstr/replace #":" "")
      (cstr/lower-case)           ;; Convert to lower case
      (keyword)))                ;; Convert to keyword

(defn unix-timestamp []
  (-> (Instant/now)
      (.getEpochSecond)))

(defn cast-to-type [value type]
  (try
    (case type
      :text    (str value)
      :string  (str value)  ;; Just in case you want to differentiate between :text and :string
      :integer (Integer. (str value))
      :float   (Float. (str value))
      value)  ;; default case
    (catch Exception _ value)))

(defn unkeyword [x]
  (if (keyword? x)
    (-> (str x)
        (cstr/replace #":" "")
        (cstr/replace #"-" "_"))
    (str x)))

;; (def unkeyword-atom (atom {}))

;; (defn unkeyword-ff [x]
;;   (let [cache (get @unkeyword-atom x)]
;;     (if cache
;;       cache
;;       (let [res (unkeyword-fn x)]
;;         (swap! unkeyword-atom assoc x res)
;;         res))))

(defn pretty-spit
  [file-name collection & [width]]
  (let [width (or width 85)]
    (spit (java.io.File. file-name)
          (with-out-str (clojure.pprint/write collection :dispatch clojure.pprint/code-dispatch :right-margin width)))))

(defn copy-file [src dest]
  (with-open [in-file (io/reader src)
              out-file (io/writer dest)]
    (io/copy in-file out-file)))

(defn get-file-vectors-simple
  "returns a vector of filenames/folders in the X folder relative to the jar (packaged by default)
   as well as the working directory so I can use full path names in subsequent commands"
  [relative-folder file-ext]
  (let [is-ext? (if (= file-ext "directory")
                  #(and (.isDirectory %) (not (cstr/includes? (str %) "/."))) ; remove hidden folders and files
                  #(cstr/ends-with? (str %) file-ext)) ; remove anythint not with this .ext
        file-seq (into []
                       (for [f
                             (filter is-ext?
                                     (.listFiles (io/file relative-folder)))]
                         (str f)))
        files-only (into [] (for [f file-seq] (cstr/replace (str f) relative-folder "")))]
    files-only))

(defn get-file-vectors-with-time-diff
  [relative-folder file-ext]
  (let [is-ext? (if (= file-ext "directory")
                  #(and (.isDirectory %) (not (cstr/includes? (str %) "/."))) ; remove hidden folders and files
                  #(cstr/ends-with? (str %) file-ext)) ; remove anything not with this .ext
        file-seq (into []
                       (for [f
                             (filter is-ext?
                                     (.listFiles (io/file relative-folder)))]
                         [f (.lastModified f)]))
        files-with-time-diff (into [] (for [[f time] file-seq
                                            :let [tt (- (System/currentTimeMillis) time)]]
                                        [(cstr/replace (str f) relative-folder "")
                                         (format-duration-seconds (/ tt 1000)) tt]))]
    files-with-time-diff))

(defn ne? [x]
  (if (seqable? x)
    (boolean (seq x))
    true))

(defn flatten-map [m]
  (for [[category items] m
        [name item] items]
    (assoc item :category category :name name)))

(defn sanitize-name-fn [name] ;; keep updated in client also
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

(def sanitize-name-atom (atom {}))

(defn sanitize-name [name]
  (let [cache (get @sanitize-name-atom name)]
    (if cache
      cache
      (let [res (sanitize-name-fn name)]
        (swap! sanitize-name-atom assoc name res)
        res))))

;; (defn dequeue! [queue]
;;   (let [item (peek @queue)]
;;     (swap! queue pop)
;;     item))

(defn matches-pattern? [item kw num]
  (and (vector? item) (= (count item) num) (= (first item) kw)))

(defn extract-patterns [data kw num]
  (let [matches (atom [])]
    (walk/prewalk
     (fn [item]
       (when (matches-pattern? item kw num)
         (swap! matches conj item))
       item)
     data)
    @matches))

(defn kvpaths
  ([m] (kvpaths [] m ()))
  ([prev m result]
   (reduce-kv (fn [res k v] (if (associative? v)
                              (let [kp (conj prev k)]
                                (kvpaths kp v (conj res kp)))
                              (conj res (conj prev k))))
              result
              m)))

(defn keypath-munger-fn [kp]
  (let [nkp (sanitize-name
             (cstr/replace (cstr/join "_"
                                      kp) ":" ""))]
    (-> nkp
        (cstr/replace "-" "_")
        (cstr/replace "(" "") ;; only for safe fields
        (cstr/replace ")" "") ;; only for safe fields
        (cstr/replace "." "_"))))

(def keypath-munger (memoize keypath-munger-fn))

(defn safe-fields [f]
  (keyword
   (let [b (str (keypath-munger (cstr/split (cstr/trim (str f)) #" ")))]
     (if ;; cant have fields that start with an integer
      (try (integer? (Long/parseLong (str (first b)))) (catch Exception _ false))
       (str "_" b)
       b))))

(defn csv-data->maps [csv-data]
  (map zipmap
       (->> (first csv-data) ;; First row is the header
            (map safe-fields) ;(map keyword) ;; Drop if you want string keys instead
            repeat)
       (rest csv-data)))

(defn nf [num]
  (cstr/trim (str (format "%,12d" num))))

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

(defn keypaths
  ([m] (keypaths [] m ()))
  ([prev m result]
   (reduce-kv (fn [res k v] (if (associative? v)
                              (keypaths (conj prev k) v res)
                              (conj res (conj prev k))))
              result
              m)))

(defn keypaths2
  ([m] (keypaths [] m ()))
  ([prev m result]
   (reduce-kv (fn [res k v] (if (map? v)
                              (keypaths (conj prev k) v res)
                              (conj res (conj prev k))))
              result
              m)))
