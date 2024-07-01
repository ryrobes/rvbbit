(ns rvbbit-backend.util
  (:require
    [cheshire.core           :as json]
    [chime.core              :as chime]
    [clojure.core.async      :as async]
    [clojure.data.csv        :as csv]
    [clojure.data.json       :as json2]
    [clojure.edn             :as edn]
    [clojure.java.io         :as io]
    [clojure.java.jdbc       :as jdbc]
    [clojure.java.shell      :refer [sh]]
    [clojure.pprint          :as pprint]
    [clojure.spec.alpha      :as s]
    [clojure.spec.test.alpha :as stest]
    [clojure.string          :as cstr]
   [zprint.core              :as zp]
    [clojure.walk            :as walk]
    [hikari-cp.core          :as hik]
    [honey.sql               :as honey]
    [puget.printer           :as puget]
    [rvbbit-backend.config   :as config]
    [talltale.core           :as tales])
  (:import
   [java.lang.management ManagementFactory]
   java.time.Instant
   java.time.LocalTime
   java.time.LocalDate
   [java.lang.management ManagementFactory]
   [java.io BufferedReader InputStreamReader]
   java.time.ZoneId
   java.nio.ByteBuffer
   java.time.format.DateTimeFormatter
   [java.time            LocalTime Duration Instant ZonedDateTime ZoneId Period DayOfWeek]
   [java.text            SimpleDateFormat]
   [java.util            Date TimeZone Calendar]
   [java.awt.image       BufferedImage]
   [javax.imageio        ImageIO]
   [java.io              File]
   [java.awt             Color]
   [jline                TerminalFactory]
   [java.time.format     TextStyle]
   [java.util            Locale]
   [java.security        MessageDigest]
   [java.util            Base64]
   [com.sun.management OperatingSystemMXBean]
   [java.lang.management RuntimeMXBean]
   [java.lang.management ManagementFactory]
   [javax.management MBeanServerInvocationHandler]
   java.time.format.DateTimeFormatter))

(def rvbbit
  "
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

(defn deselect-keys [m ks] (apply dissoc m ks))

(def console-lock (Object.))

(defn safe-println [x] (locking console-lock (println x)))

(defn print-ansi-art
  [filename]
  (with-open [reader (clojure.java.io/reader filename)] (doseq [line (line-seq reader)] (safe-println line))))

(defn index-of [coll item] (first (keep-indexed #(when (= %2 item) %1) coll)))

(defn current-datetime-parts
  []
  (try (let [now               (java.time.ZonedDateTime/now)
             day-of-week       (.getDayOfWeek now)
             month             (.getMonth now)
             day               (.getDayOfMonth now)
             nth               (case (mod day 10)
                                 1 (if (= day 11) "th" "st")
                                 2 (if (= day 12) "th" "nd")
                                 3 (if (= day 13) "th" "rd")
                                 "th")
             formatter         (java.time.format.DateTimeFormatter/ofPattern (str "EEEE, MMMM d'" nth "' h:mma")
                                                                             java.util.Locale/US)
             formatter-seconds (java.time.format.DateTimeFormatter/ofPattern (str "EEEE, MMMM d'" nth "' h:mm:ssa")
                                                                             java.util.Locale/US)
             now-str           (.format formatter now)
             now-seconds-str   (.format formatter-seconds now)]
         {:year            (.getYear now)
          :month           (.getMonthValue now)
          :month-name      (.name month)
          :day             day
          :day-of-week-int (.getValue day-of-week)
          :day-of-week     (.name day-of-week)
          :hour            (.getHour now)
          :minute          (.getMinute now)
          :second          (.getSecond now)
          :quarter         (inc (quot (.getMonthValue now) 4))
          :am-pm           (if (< (.getHour now) 12) "AM" "PM")
          :now             now-str
          :now-seconds     now-seconds-str
          :nth             nth})
       (catch Throwable e {:time-atom-error (str "Error! " e)})))



(defn break-out-parts
  [clause]
  (cond (not (vector? clause))             []
        (some #{:and :or} (take 1 clause)) (cons clause (mapcat break-out-parts (rest clause)))
        :else                              (cons clause (mapcat break-out-parts (rest clause)))))

(defn where-dissect [clause] (let [parts (break-out-parts clause)] (cons clause (remove #(= clause %) parts))))

(defn thaw-atom
  "Thaws an atom from disk or creates a new one if the file doesn't exist."
  [initial-state file-path & [out?]]
  (let [file  (io/file file-path)
        state (if (.exists file)
                (with-open [rdr (io/reader file)]
                  (try (edn/read (java.io.PushbackReader. rdr))
                       (catch Exception e (do (pp [:thaw-atom-error!!!! file e]) (System/exit 0)))))
                initial-state)
        a     (atom state)]
    (when (not out?) ;; we dont want to manage some of these, do it manually
      (swap! managed-atoms assoc file-path a))
    a))

(defn serializable?
  [value]
  (and (not (clojure.string/includes? (pr-str value) "#object")) (not (clojure.string/includes? (pr-str value) "#error"))))




(defn freeze-atoms
  "Freezes all managed atoms to disk."
  []
  (doall
    (pmap (fn [[file-path a]]
            (let [wtr (io/writer file-path)]
              (try (binding [*out* wtr] ;; selective pretty print formatting
                     (if (or (cstr/includes? (str file-path) "signals.")
                             (cstr/includes? (str file-path) "rules.")
                             (cstr/includes? (str file-path) "errors.")
                             (cstr/includes? (str file-path) "autocomplete")
                             (cstr/includes? (str file-path) "training-atom.")
                             (cstr/includes? (str file-path) "sql-cache.")
                             (cstr/includes? (str file-path) "solvers."))
                       (clojure.pprint/pprint @a)
                       (prn @a)))
                   (finally (.close wtr))))
            (let [size-in-bytes      (java.nio.file.Files/size (java.nio.file.Paths/get file-path (into-array String [])))
                  size-in-mb         (/ size-in-bytes 1048576.0)
                  size-in-mb-rounded (/ (Math/round (* size-in-mb 100.0)) 100.0)]
              (pp ["  " :freezing-atom file-path size-in-mb-rounded :mb])))
          @managed-atoms)))


(defn freeze-atom
  "Freezes a single atom to disk."
  [file-path]
  (let [a (get @managed-atoms file-path)] (when a (with-open [wtr (io/writer file-path)] (binding [*out* wtr] (prn @a))))))

(def terminal (TerminalFactory/get))
(defn get-terminal-width [] (try (.getWidth terminal) (catch Throwable _ 85)))

;; (defn get-terminal-width []
;;   (let [terminal (TerminalFactory/get)]
;;     (try (.getWidth terminal) (catch Throwable _ 85))))

(defn zp-stats [s]
  (let [s (pr-str s)
        o (zp/zprint-str s
                         (get-terminal-width)
                         {:parse-string-all? true ;; was :parse-string-all?
                          ;:color?        true
                          :style         [:justified-original] ;;type ;:community ;[:binding-nl :extend-nl]
                          :pair          {:force-nl? false}
                          :map {:hang? true :comma? false :sort? false}
                          :pair-fn {:hang? true}
                          :binding       {:force-nl? true}
                          :vector        {:respect-nl? true}
                          :parse         {:interpose "\n\n"}})]
    o))

(defn safe-name [x] (cstr/replace (str x) ":" ""))

(defn avg [nums] (when (seq nums) (Math/round (/ (reduce + nums) (double (count nums))))))

(defn avgf [nums]
  (when (seq nums)
    (Double/parseDouble (format "%.2f" (/ (reduce + nums) (double (count nums)))))))

(defn get-system-load-average
  []
  (let [os-bean (java.lang.management.ManagementFactory/getOperatingSystemMXBean)] (.getSystemLoadAverage os-bean)))

;; (defn get-jvm-cpu-usage []
;;   (let [os-mxbean (ManagementFactory/getOperatingSystemMXBean)
;;         runtime-mxbean (ManagementFactory/getRuntimeMXBean)
;;         uptime (.getUptime runtime-mxbean)
;;         cpu-time (if (instance? OperatingSystemMXBean os-mxbean)
;;                    (let [os-mxbean-ext ^OperatingSystemMXBean os-mxbean]
;;                      (.getProcessCpuTime os-mxbean-ext))
;;                    (throw (UnsupportedOperationException. "CPU time not supported on this JVM")))]
;;     (if (> uptime 0)
;;       (let [cpu-usage (/ (* cpu-time 100.0) (* uptime 1000000.0))]
;;         cpu-usage)
;;       0.0)))

;; (defn get-jvm-cpu-usage []
;;   (let [os-mxbean (ManagementFactory/getOperatingSystemMXBean)
;;         runtime-mxbean (ManagementFactory/getRuntimeMXBean)
;;         uptime (.getUptime runtime-mxbean)
;;         available-processors (.getAvailableProcessors os-mxbean)
;;         cpu-time (if (instance? OperatingSystemMXBean os-mxbean)
;;                    (let [os-mxbean-ext ^OperatingSystemMXBean os-mxbean]
;;                      (.getProcessCpuTime os-mxbean-ext))
;;                    (throw (UnsupportedOperationException. "CPU time not supported on this JVM")))]
;;     (if (> uptime 0)
;;       (let [cpu-usage (/ (* cpu-time 100.0) (* uptime 1000000.0 available-processors))]
;;         cpu-usage)
;;       0.0)))

(defn get-pid []
  (let [runtime-mxbean (ManagementFactory/getRuntimeMXBean)
        jvm-name (.getName runtime-mxbean)]
    (first (clojure.string/split jvm-name #"@"))))

(defn get-cpu-usage-unix [pid]
  (let [process-builder (ProcessBuilder. ["sh" "-c" (str "top -b -n 1 | grep " pid)])
        process (.start process-builder)
        reader (BufferedReader. (InputStreamReader. (.getInputStream process)))]
    (try
      (let [output (reduce str (line-seq reader))]
        (if (clojure.string/blank? output)
          (throw (Exception. "CPU usage not found"))
          ;; Assuming a more generic parsing strategy that doesn't rely on fixed positions
          (let [parts (clojure.string/split output #"\s+")
                cpu-usage-index (->> parts
                                     (map-indexed vector)
                                     (filter #(re-matches #"\d+\.\d+" (second %)))
                                     (first)
                                     (first))]
            (if cpu-usage-index
              (Float/parseFloat (nth parts cpu-usage-index))
              (throw (Exception. "CPU usage not found"))))))
      (finally
        (.close reader)))))

;; "java.util.IllegalFormatConversionException: f != java.lang.Long"
;; (defn cumulative-to-delta [cumulative-values]
;;   (vec
;;    (cons 0  ; First value has no previous value to subtract from
;;          (map -
;;               (rest cumulative-values)
;;               cumulative-values))))

(defn memory-used []
  (int (Math/floor (/ (float (/ (- (-> (java.lang.Runtime/getRuntime)
                                       (.totalMemory))
                                   (-> (java.lang.Runtime/getRuntime)
                                       (.freeMemory)))
                                1024))
                      1024))))

(defn cumulative-to-delta [cumulative-values]
  (if (empty? cumulative-values)
    []
    (vec
     (cons 0.0
           (map (fn [a b]
                  (if (and (number? a) (number? b))
                    (float (- a b))
                    (throw (IllegalArgumentException. "Non-numeric value in input"))))
                (rest cumulative-values)
                cumulative-values)))))

(defn normalize-cpu-usage [cpu-usage num-cores]
  (let [max-theoretical-usage (* 100 num-cores)]
    (/ (max 0 (min cpu-usage max-theoretical-usage)) num-cores)))

(defn get-jvm-cpu-usage [] (let [num-cores (.. Runtime getRuntime availableProcessors)
                                 raw-usage (get-cpu-usage-unix (get-pid))
                                 ;;normalized-usage (/ raw-usage num-cores)
                                 normalized-usage (normalize-cpu-usage raw-usage num-cores)]
                             normalized-usage))

(defn bytes-to-mb [bytes]
  (Math/round (/ bytes 1048576.0)))

(defn get-non-heap-memory-usage []
  (let [memory-mx-bean (ManagementFactory/getMemoryMXBean)
        nonHeapUsage (.getNonHeapMemoryUsage memory-mx-bean)]
    (bytes-to-mb (.getUsed nonHeapUsage))))

(def debug-level (get (config/settings) :debug-level 0))

(defn replace-multiple [s replacements] (reduce (fn [s [k v]] (cstr/replace s k v)) s replacements))

(defn json-to-edn [json-str] (json2/read-str (str json-str) :key-fn keyword))

(defn get-colors
  [image-path]
  (let [image  (ImageIO/read (File. image-path))
        width  (.getWidth image)
        height (.getHeight image)
        colors (for [x (range width)
                     y (range height)]
                 (let [rgb   (.getRGB image x y)
                       color (Color. rgb)]
                   (format "#%02x%02x%02x" (.getRed color) (.getGreen color) (.getBlue color))))]
    (vec (map first
           (->> (frequencies colors)
                (sort-by val)
                (reverse)
                (take 50))))))

(defn hue-to-rgb
  [p q t]
  (let [t (cond (< t 0) (+ t 1)
                (> t 1) (- t 1)
                :else   t)]
    (cond (< (* 6 t) 1) (+ p (* (- q p) 6 t))
          (< (* 2 t) 1) q
          (< (* 3 t) 2) (+ p (* (- q p) 6 (- 0.6666667 t)))
          :else         p)))

(defn hsl-to-rgb
  [h s l]
  (let [s (/ s 100.0)
        l (/ l 100.0)
        q (if (<= l 0.5) (* l (+ 1 s)) (- (+ l s) (* l s)))
        p (* 2 l q)
        h (/ h 360.0)]
    (map (fn [x] (int (* 255 (hue-to-rgb p q x)))) [(+ h 0.3333333) h (- h 0.3333333)])))

(defn rgb-to-hsl
  [r g b]
  (let [r     (/ r 255.0)
        g     (/ g 255.0)
        b     (/ b 255.0)
        max   (max r g b)
        min   (min r g b)
        l     (/ (+ max min) 2)
        delta (- max min)
        s     (if (== delta 0.0) 0.0 (/ delta (if (<= l 0.5) (+ max min) (- 2 max min))))
        h     (cond (== delta 0) 0
                    (== max r)   (rem (/ (- g b) delta) 6)
                    (== max g)   (+ 2 (/ (- b r) delta))
                    (== max b)   (+ 4 (/ (- r g) delta)))
        hue   (* 60 (if (< h 0) (+ h 6) h))]
    [(mod hue 360) (* 100 s) (* 100 l)]))

(defn hex-to-rgb [hex] (map #(Integer/parseInt % 16) [(subs hex 1 3) (subs hex 3 5) (subs hex 5 7)]))

(defn rgb-to-xyz
  [r g b]
  (let [r (if (> r 0.04045) (Math/pow ((r + 0.055) / 1.055) 2.4) (/ r 12.92))
        g (if (> g 0.04045) (Math/pow ((g + 0.055) / 1.055) 2.4) (/ g 12.92))
        b (if (> b 0.04045) (Math/pow ((b + 0.055) / 1.055) 2.4) (/ b 12.92))
        r (* r 100)
        g (* g 100)
        b (* b 100)]
    [(+ (* r 0.4124) (* g 0.3576) (* b 0.1805)) (+ (* r 0.2126) (* g 0.7152) (* b 0.0722))
     (+ (* r 0.0193) (* g 0.1192) (* b 0.9505))]))

(defn xyz-to-cie [x y z] (let [total (+ x y z)] [(if (zero? total) 0 (/ x total)) (if (zero? total) 0 (/ y total))]))

(defn hex-to-cie
  [hex]
  (let [[r g b] (hex-to-rgb hex) [x y z] (rgb-to-xyz (/ r 255.0) (/ g 255.0) (/ b 255.0))] (xyz-to-cie x y z)))

(defn cie-to-xyz [x y brightness] (let [Y brightness X (/ (* Y x) y) Z (/ (* Y (- 1 x y)) y)] [X Y Z]))

(defn xyz-to-rgb
  [X Y Z]
  (let [r (/ (+ (* X 3.2406) (* Y -1.5372) (* Z -0.4986)) 100)
        g (/ (+ (* X -0.9689) (* Y 1.8758) (* Z 0.0415)) 100)
        b (/ (+ (* X 0.0557) (* Y -0.2040) (* Z 1.0570)) 100)]
    (mapv #(max 0 (min 255 (Math/round (* 255 (if (> % 0.0031308) (Math/pow (* % 1.055) (/ 1 2.4)) (* % 12.92)))))) [r g b])))

(defn rgb-to-hex2 [r g b] (format "#%02x%02x%02x" r g b))

(defn cie-to-hex
  [x y brightness]
  (pp [:cie-to-hex y brightness])
  (let [[X Y Z] (cie-to-xyz x y brightness) [r g b] (xyz-to-rgb X Y Z)] (rgb-to-hex2 r g b)))

(defn hue-to-rgb2
  [hue]
  (try (let [hue'    (rem (/ hue 182.04) 360) ; Scale hue to [0, 360] and wrap around
             sector  (/ hue' 60)
             c       1 ; Assuming full saturation
             x       (* c (- 1 (Math/abs (- (rem sector 2) 1))))
             m       0 ; Assuming brightness at midpoint for simplicity
             rgb'    (cond (< sector 1) [c x 0]
                           (< sector 2) [x c 0]
                           (< sector 3) [0 c x]
                           (< sector 4) [0 x c]
                           (< sector 5) [x 0 c]
                           :else        [c 0 x])
             [r g b] (mapv #(+ m (* 255 %)) rgb')]
         (mapv int [r g b]))
       (catch Exception _ "")))

(defn rgb-to-hex2 [r g b] (format "#%02x%02x%02x" r g b))

(defn hue-to-hex [hue] (let [[r g b] (hue-to-rgb2 hue) out (rgb-to-hex2 r g b)] (if (= out "#nullnullnull") "#00000000" out)))


(defn hex-to-hue-sat
  [hex]
  (let [[r g b]    (hex-to-rgb hex)
        [h s l]    (rgb-to-hsl r g b)
        hue-scaled (* h (/ 65535 360))] ; Scale hue to Philips Hue range
    [(int hue-scaled) 254])) ; Scale saturation to [0, 254]


(defn bytes-to-mb [bytes] (let [mb (/ bytes 1048576.0) formatted-mb0 (format "%.0f" mb)] (str formatted-mb0 "MB")))


(defn interpolate-hsl [hsl1 hsl2 factor] (map (fn [v1 v2] (+ v1 (* (- v2 v1) factor))) hsl1 hsl2))

(defn hex-to-rgb [hex] (map #(Integer/parseInt (subs hex %1 (+ %1 2)) 16) [1 3 5]))

(defn rgb-to-hex [rgb] (format "#%02X%02X%02X" (nth rgb 0) (nth rgb 1) (nth rgb 2)))

(defn generate-gradient
  [hex1 hex2 steps]
  (let [rgb1 (hex-to-rgb hex1)
        rgb2 (hex-to-rgb hex2)
        hsl1 (rgb-to-hsl (nth rgb1 0) (nth rgb1 1) (nth rgb1 2))
        hsl2 (rgb-to-hsl (nth rgb2 0) (nth rgb2 1) (nth rgb2 2))]
    (map (fn [step]
           (let [factor (/ step (dec steps))
                 hsl    (interpolate-hsl hsl1 hsl2 factor)
                 rgb    (hsl-to-rgb (nth hsl 0) (nth hsl 1) (nth hsl 2))]
             (rgb-to-hex rgb)))
      (range steps))))



(defn base64-decode [b64-string] (.decode (java.util.Base64/getDecoder) b64-string))

(defn save-base64-to-webp
  [b64-string file-path]
  (let [;b64-string (str "{\"data\":\"" b64-string "\"}")
        data-bytes (base64-decode b64-string)]
    (with-open [out-stream (io/output-stream (io/file file-path))] (.write out-stream data-bytes))
    (str file-path)))

(defn base64-decode1
  [b64-string]
  (let [image-data (second (re-find #"data:image/png;base64,(.*)" b64-string))]
    (.decode (java.util.Base64/getDecoder) image-data)))

(defn save-base64-to-png
  [b64-string file-path]
  (let [data-bytes (base64-decode1 b64-string)]
    (with-open [out-stream (io/output-stream (io/file file-path))] (.write out-stream data-bytes))
    (str file-path)))

(defn base64-decode2
  [b64-string]
  (let [image-data (second (re-find #"data:image/jpeg;base64,(.*)" b64-string))]
    (.decode (java.util.Base64/getDecoder) image-data)))

(defn save-base64-to-jpeg
  [b64-string file-path]
  (let [data-bytes (base64-decode2 b64-string)]
    (with-open [out-stream (io/output-stream (io/file file-path))] (.write out-stream data-bytes))))






(defn uptime-seconds
  []
  (let [runtime-bean   (java.lang.management.ManagementFactory/getRuntimeMXBean)
        uptime-ms      (.getUptime runtime-bean)
        uptime-seconds (/ uptime-ms 1000.0)] ;; Note the 1000.0 for floating point division
    (Math/round uptime-seconds)))

(defn flatten-one-level [v] (vec (mapcat (fn [x] (if (coll? (first x)) x (list x))) v)))

(defn millis-to-date-string
  [millis]
  (let [instant       (Instant/ofEpochMilli millis)
        zonedDateTime (.atZone instant (ZoneId/of "UTC"))
        formatter     (DateTimeFormatter/ofPattern "yyyy-MM-dd HH:mm:ss")]
    (.format formatter zonedDateTime)))

(defn bytes-to-mb [bytes] (int (/ bytes (Math/pow 1024 2))))

(defn get-current-timestamp
  []
  (let [zonedDateTime (ZonedDateTime/now (ZoneId/systemDefault))
        formatter     (DateTimeFormatter/ofPattern "yyyy-MM-dd HH:mm:ss")]
    (.format formatter zonedDateTime)))


(defn get-memory-usage
  []
  (let [memory-bean       (ManagementFactory/getMemoryMXBean)
        memory-usage->map (fn [m]
                            {:init      (bytes-to-mb (.getInit m))
                             :used      (bytes-to-mb (.getUsed m))
                             :committed (bytes-to-mb (.getCommitted m))
                             :max       (bytes-to-mb (.getMax m))})]
    {:heap-usage     (memory-usage->map (.getHeapMemoryUsage memory-bean))
     :non-heap-usage (memory-usage->map (.getNonHeapMemoryUsage memory-bean))}))


(defn template-replace
  [replacements s]
  (reduce (fn [s [key value]]
            (let [value (get :code value value)
                  value (if value (str value) "")] ;; force a string, since this is a string
              (cstr/replace s (re-pattern (cstr/join ["\\{\\{" key "\\}\\}"])) value)))
    s
    replacements))

(defn get-thread-count
  []
  (let [thread-bean (ManagementFactory/getThreadMXBean)]
    {:total-started (.getTotalStartedThreadCount thread-bean)
     :peak          (.getPeakThreadCount thread-bean)
     :current       (.getThreadCount thread-bean)
     :daemon        (.getDaemonThreadCount thread-bean)}))

(defn abs-file-path
  [fpath]
  (str ;(.getAbsolutePath (clojure.java.io/file fpath))
    (.getCanonicalPath (clojure.java.io/file fpath))))








(defn time-seq
  [v]
  (let [{:keys [days minutes seconds hours weeks months at starts tz] :or {tz (ZoneId/systemDefault)}} (apply hash-map v)
        zone-id          (if (instance? String tz) (ZoneId/of tz) tz)
        starting-instant (if starts
                           (-> (LocalDate/parse starts)
                               (.atTime (if at (LocalTime/of (quot at 100) (mod at 100)) (LocalTime/MIDNIGHT)))
                               (.atZone zone-id)
                               .toInstant)
                           (Instant/now))]
    (cond days    (if at
                    (chime/periodic-seq (-> starting-instant
                                            (.plus (Period/ofDays days)))
                                        (Period/ofDays days))
                    (chime/periodic-seq starting-instant (Period/ofDays days)))
          hours   (if at
                    (chime/periodic-seq (-> (LocalTime/of (quot at 100) (mod at 100))
                                            (.adjustInto (ZonedDateTime/now zone-id))
                                            .toInstant)
                                        (Duration/ofHours hours))
                    (chime/periodic-seq (Instant/now) (Duration/ofHours hours)))
          minutes (if at
                    (chime/periodic-seq (-> (LocalTime/of (quot at 100) (mod at 100))
                                            (.adjustInto (ZonedDateTime/now zone-id))
                                            .toInstant)
                                        (Duration/ofMinutes minutes))
                    (chime/periodic-seq (Instant/now) (Duration/ofMinutes minutes)))
          seconds (if at
                    (chime/periodic-seq (-> (LocalTime/of (quot at 100) (mod at 100))
                                            (.adjustInto (ZonedDateTime/now zone-id))
                                            .toInstant)
                                        (Duration/ofSeconds seconds))
                    (chime/periodic-seq (Instant/now) (Duration/ofSeconds seconds)))
          weeks   (if at
                    (chime/periodic-seq (-> starting-instant
                                            (.plus (Period/ofWeeks weeks)))
                                        (Period/ofWeeks weeks))
                    (chime/periodic-seq starting-instant (Period/ofWeeks weeks)))
          months  (if at
                    (chime/periodic-seq (-> (LocalDate/parse starts)
                                            (.atTime (if at (LocalTime/of (quot at 100) (mod at 100)) (LocalTime/MIDNIGHT)))
                                            (.atZone zone-id)
                                            (.plus (Period/ofMonths months))
                                            .toInstant)
                                        (Duration/ofDays 30)) ; approx
                    (chime/periodic-seq starting-instant (Duration/ofDays 30)))
          :else   (throw (IllegalArgumentException. "Unsupported time unit")))))

(defn dequeue!
  [queue]
  (let [item (peek @queue)]
    (swap! queue pop)
    item))

(defn unix-to-date [timestamp] (java.util.Date. timestamp))

(defn today-yyyymmdd [] (let [cal (Calendar/getInstance) fmt (SimpleDateFormat. "yyyy-MM-dd")] (.format fmt (.getTime cal))))

(defn today-yyyymmdd-hhmm
  []
  (let [cal (Calendar/getInstance) fmt (SimpleDateFormat. "yyyy-MM-dd HH:mm")] (.format fmt (.getTime cal))))

(defn date-str-to-unix [date-str] (let [fmt (SimpleDateFormat. "yyyy-MM-dd") date (.parse fmt date-str)] (.getTime date)))

(defn date-to-ymd
  [date] ;; blargh
  (let [cal (Calendar/getInstance)]
    (.setTime cal date)
    (str (.get cal Calendar/YEAR)
         "-" (inc (.get cal Calendar/MONTH))
         "-" ; +1 since jan = 0
           (.get cal Calendar/DATE))))

(defn chan?
  [ch]
  (try (-> ch
           class
           .getName
           (= "clojure.core.async.impl.channels.ManyToManyChannel"))
       (catch Exception _ false)))


(defn generate-name
  []
  (let [;quals ["of-the" "hailing-from" "banned-from" "of" "exiled-from"]
        names [(tales/quality) (rand-nth [(tales/shape) (tales/color)]) (tales/animal) ;(rand-nth
              ]]
    (str (cstr/replace (cstr/join "-" names) " " "-") "-" (rand-int 45))))

(defn channel-open? [ch] (async/offer! ch nil))

(defn accumulate-unique-runs
  [data]
  (let [merge-runs (fn [runs run]
                     (let [run-start (:start run)
                           run-end   (:end run)]
                       (cond (some #(and (= (:start %) run-start) (= (:end %) run-end)) runs) runs
                             (some #(and (= (:start %) run-start) (nil? (:end %)) run-end) runs)
                               (conj (vec (remove #(and (= (:start %) run-start) (nil? (:end %))) runs)) run)
                             :else (conj runs run))))]
    (reduce (fn [acc entry]
              (reduce (fn [inner-acc [block-id run]] (update inner-acc block-id (fn [runs] (merge-runs (or runs []) run))))
                acc
                (into [] entry)))
      {}
      data)))


(defn format-duration
  [start-ms end-ms]
  (let [duration (java.time.Duration/ofMillis (- end-ms start-ms))
        hours    (.toHours duration)
        minutes  (.toMinutesPart duration)
        seconds  (.toSecondsPart duration)
        out      (str (when (pos? hours) (str hours " hour" (when (> hours 1) "s") ", "))
                      (when (pos? minutes) (str minutes " minute" (when (> minutes 1) "s") ", "))
                      seconds
                      " second"
                      (when (> seconds 1) "s"))]
    (if (= out "0 second") "less than a second" out)))

(defn format-duration-seconds
  [seconds]
  (let [duration (java.time.Duration/ofSeconds seconds)
        hours    (.toHours duration)
        minutes  (.toMinutesPart duration)
        seconds  (.toSecondsPart duration)
        out      (str (when (pos? hours) (str hours " hour" (when (> hours 1) "s") ", "))
                      (when (pos? minutes) (str minutes " minute" (when (> minutes 1) "s") ", "))
                      seconds
                      " second"
                      (when (> seconds 1) "s"))]
    (if (= out "0 second") "less than a second" out)))


(defn nf [i] (pprint/cl-format nil "~:d" i))



(defmacro timed-exec
  [expr] ;; expression based, not fn based
  `(let [start#  (System/currentTimeMillis)
         result# ~expr
         end#    (System/currentTimeMillis)]
     {:result result# :fn-start start# :fn-end end# :elapsed-ms (- end# start#)}))

(defn dissoc-in [map-in keypath] (let [base-kp (pop keypath) last-kp (last keypath)] (update-in map-in base-kp dissoc last-kp)))

(defn ppln [x] (puget/with-options {:width 330} (puget/cprint x)))

(defn pplno [_] nil) ;; TODO, goofy

(defn chan?
  [ch]
  (try (-> ch
           class
           .getName
           (= "clojure.core.async.impl.channels.ManyToManyChannel"))
       (catch Exception _ false)))

(defn unkeyword [k] (cstr/replace (str k) #":" ""))

(defn namespaced? [k] (and k (namespace k)))

(defn ms-to-iso8601
  [ms]
  (let [instant (Instant/ofEpochMilli ms) formatter (DateTimeFormatter/ISO_INSTANT)] (.format formatter instant)))

(defn is-hiccup? [x] (cstr/includes? (str x) ":div")) ;; TODO, no need anymore?

(defn coll-of-maps? [x] (and (or (list? x) (vector? x)) (not (empty? x)) (every? map? x)))

(defn has-nested-map-values?
  [x]
  (try (let [aa (some true?
                      (vec (if (and (or (list? x) (vector? x)) (not (empty? x)) (coll-of-maps? x))
                             (if (map? (first x))
                               (for [k (keys (first x))] (or (map? (get (first x) k)) (vector? (get (first x) k))))
                               false)
                             false)))]
         (if aa true false))
       (catch Exception _ true)))

(defn select-keypaths
  [m keypaths]
  (letfn [(extract [m path] (let [val (get-in m path)] (when val (assoc-in {} path val))))]
    (apply merge-with merge (map #(extract m %) keypaths))))

(defn data-typer
  [x] ;; legacy data rabbit code. TODO revisit
  (cond (or (and (vector? x) (clojure.string/includes? (str x) "#object"))
            (and (vector? x) (fn? (first x))) ;; ?
            (and (vector? x) (cstr/starts-with? (str (first x)) ":re-com"))
            (and (vector? x) (cstr/starts-with? (str (first x)) ":vega"))
            (and (vector? x) (is-hiccup? x)))
          "render-object"
        (string? x) "string"
        (boolean? x) "boolean"
        (coll-of-maps? x) "rowset" ;;; TODO, revisit. convoluted logic
        (vector? x) "vector"
        (or (and (map? x) (contains? x :classname) (contains? x :subprotocol) (contains? x :subname))
            (and (map? x) (contains? x :dbtype) (contains? x :dbname) (contains? x :user)))
          "jdbc-conn"
        (map? x) "map"
        (list? x) "list"
        (nil? x) "nil"
        (int? x) "integer"
        (set? x) "set"
        (instance? clojure.lang.IPending x) "lazy"
        (keyword? x) "keyword"
        (float? x) "float"
        (ifn? x) "function"
        :else "unknown"));)

(defn gen-coords
  [blocks] ;; awful attempt at auto coord / layout placing - revist later
  (let [start-x       100
        start-y       100
        step-x        330 ; 250 + 80
        step-y        330 ; 250 + 80
        placed-blocks (atom {})
        max-y         (atom start-y)
        queue         (atom blocks)]
    (while (seq @queue)
      (let [[from to] (first @queue)]
        (swap! queue rest)
        (when (not (contains? @placed-blocks from))
          (do (swap! placed-blocks assoc from [start-x @max-y]) (swap! max-y #(+ % step-y))))
        (when (not (contains? @placed-blocks to))
          (let [[from-x from-y] (@placed-blocks from)
                to-x            (+ from-x step-x)
                to-y            (if (= from-x to-x) (+ from-y step-y) from-y)]
            (do (swap! placed-blocks assoc to [to-x to-y]) (when (> to-y @max-y) (swap! max-y #(+ % step-y))))))))
    (mapv (fn [[block [x y]]] [block x y]) @placed-blocks)))

(defn coords-map [connections] (into {} (for [[bid x y] (gen-coords connections)] {bid {:x x :y y}})))


(def nspc ['flowmaps.core 'clojure.set])

(defn get-fn-specs
  [ns-sym]
  (->> (s/registry)
       (filter #(= (namespace (key %)) (name ns-sym)))
       (into {})))

(defn instrument-ns
  [ns-syms & [un?]] ;; nice
  (doseq [ns-sym ns-syms]
    (->> (ns-publics ns-sym)
         keys
         (filter #(s/get-spec `%))
         (apply (if un? stest/unstrument stest/instrument)))))

(defn test-value
  [ns-syms args]
  (if (vector? args)
    (let [specs (mapcat get-fn-specs ns-syms)]
      (->> specs
           (map (fn [[k v]]
                  (let [args-spec (:args (s/spec v))]
                    [(str (namespace k) "/" (name k)) (not= (s/conform args-spec args) :clojure.spec.alpha/invalid)])))
           (into (sorted-map))))
    (println "The provided arguments should be a vector.")))

(defn write-csv
  [data filename]
  (let [header (map name (keys (first data)))
        rows   (map vals data)]
    (with-open [writer (io/writer filename)] (csv/write-csv writer (cons header rows)))))

(defn write-json
  [data filename]
  (let [json-str (json/generate-string data)] (with-open [writer (io/writer filename)] (.write writer json-str))))

(defn map-vals [m f] (into {} (for [[k v] m] [k (f v)])))

(defn dissoc-recursive
  [data]
  (cond (map? data)    (-> data
                           (dissoc :where)
                           (map-vals dissoc-recursive))
        (vector? data) (mapv dissoc-recursive data)
        :else          data))

(defn lists-to-vectors
  [x]
  (cond (list? x) (vec (map lists-to-vectors x))
        (map? x)  (into {} (map (fn [[k v]] [k (lists-to-vectors v)]) x))
        (coll? x) (mapv lists-to-vectors x)
        :else     x))

(defn dissoc-in [map-in keypath] (let [base-kp (pop keypath) last-kp (last keypath)] (update-in map-in base-kp dissoc last-kp)))



(defn millis-to-date-string
  [millis]
  (let [date (java.util.Date. millis) format (java.text.SimpleDateFormat. "yyyy-MM-dd HH:mm:ss")] (.format format date)))

;;(defn deep-flatten [x] (if (coll? x) (mapcat deep-flatten x) [x]))

;; (defn deep-flatten [x] (if (coll? x) (into #{} (mapcat deep-flatten x)) #{x}))

(def df-cache (atom {}))

(defn deep-flatten [x]
  (if-let [cached-result (@df-cache (pr-str x))]
    cached-result
    (let [result (vec (filter keyword? (if (coll? x) (into #{} (mapcat deep-flatten x)) #{x})))]
      (swap! df-cache assoc (pr-str x) result)
      result)))

(def boot-timestamp
  (str (-> (java.text.SimpleDateFormat. "yyyy-MM-dd HH:mm:ss")
           (.format (java.util.Date.)))))

(defn unique-block-id-helper
  [base-name counter all-names]
  (let [new-name (if (= counter 0) base-name (str base-name "-" counter))]
    (if (contains? all-names new-name) (recur base-name (inc counter) all-names) new-name)))

(defn unique-block-id
  [proposed existing-keys reserved-names]
  (let [all-names (set (concat existing-keys reserved-names))] (unique-block-id-helper proposed 0 all-names)))

(defn generate-unique-names
  [name-map]
  (let [update-map-with-unique-name
          (fn [acc [k v]] (let [unique-name (unique-block-id-helper v 0 (set (vals acc)))] (assoc acc k unique-name)))]
    (reduce update-map-with-unique-name {} name-map)))

(defn reverse-map [m] (reduce (fn [acc [k v]] (assoc acc v k)) {} m))

(defn deep-remove-nil-values
  [item]
  (cond (map? item)  (->> item
                          (map (fn [[k v]] [k (deep-remove-nil-values v)]))
                          (filter (fn [[k v]] (not (nil? v))))
                          (into {}))
        (coll? item) (map deep-remove-nil-values item)
        :else        item))

(defn remove-empty-key [arg-map key-name] (if (empty? (get arg-map key-name)) (dissoc arg-map key-name) arg-map))

(defn remove-empty-sub-keys
  [arg-map]
  (-> arg-map
      (remove-empty-key :queries)
      (remove-empty-key :views)))

(defn remove-namespaced-keys
  [m]
  (walk/postwalk (fn [x] (if (map? x) (into {} (remove (fn [[k v]] (and (keyword? k) (namespace k))) x)) x)) m))

(defn pool-create
  [jdbc-body name]
  (delay (let [base  {;:jdbc-url "jdbc:sqlite:db/system.db"
                      :minimum-idle      10 ;; min iodle connections to keep open?
                      :idle-timeout      600000
                      :max-lifetime      1800000
                      :pool-name         name ;"system-db-pool"
                      :maximum-pool-size 30}
               bbase (merge base jdbc-body)]
           (hik/make-datasource bbase))))

(def log-jdbc-map2
  {:jdbc-url         "jdbc:sqlite:db/system-log.db"
   :idle-timeout     600000
   :max-lifetime     1800000
   :transaction_mode "IMMEDIATE"
   :journal_mode     "WAL"
   :cache            "shared"})

(def log-jdbc-map
  {:jdbc-url           "jdbc:clickhouse://10.174.1.150:8123/default"
   :username           "default"
   :password           "notofox"
   :driver-class-name  "ru.yandex.clickhouse.ClickHouseDriver"
   :connection-timeout 5000
   :max-lifetime       300000})

(defn deep-remove-keys
  [data keys-to-remove]
  (let [key-remove-set (set keys-to-remove)]
    (cond (map? data)    (->> data
                              (reduce-kv (fn [acc k v]
                                           (if (or (key-remove-set k) (and (keyword? k) (cstr/starts-with? (name k) "_")))
                                             acc
                                             (assoc acc k (deep-remove-keys v keys-to-remove))))
                                         {})
                              (into (empty data)))
          (vector? data) (mapv (fn [elem] (deep-remove-keys elem keys-to-remove)) data)
          :else          data)))

(defn clean-sql-from-ui-keys
  [query]
  (let [res (deep-remove-keys query
                              [:cache? :col-widths :row-height :render-all? :refresh-every :page :connection-id :deep-meta?
                               :clicked-row-height :style-rules])]
    res))

(defn deep-remove-keys2
  [data keys-to-remove] ;; doent automatically remove underscore keys
  (let [key-remove-set (set keys-to-remove)]
    (cond (map? data)    (->> data
                              (reduce-kv (fn [acc k v]
                                           (if (key-remove-set k) acc (assoc acc k (deep-remove-keys v keys-to-remove))))
                                         {})
                              (into (empty data)))
          (vector? data) (mapv (fn [elem] (deep-remove-keys elem keys-to-remove)) data)
          :else          data)))

(defn is-base64?
  [s]
  (if (and (string? s) (> (count s) 3000))
    (let [substring (subs s 0 3000)]
      (boolean (re-matches #"^(?:[A-Za-z0-9+/]{4})*(?:[A-Za-z0-9+/]{2}==|[A-Za-z0-9+/]{3}=)?$" substring)))
    false))

(defn replace-large-base64
  [data]
  (cond (string? data) (if (and (is-base64? data) (> (count data) 3000)) "**huge base64 string**" data)
        (map? data)    (into {} (map (fn [[k v]] [k (replace-large-base64 v)]) data))
        (vector? data) (mapv replace-large-base64 data)
        (list? data)   (doall (map replace-large-base64 data)) ;; use doall to realize the lazy
        :else          data))

(defn limited
  [x & [flow-limit]]
  (let [flow-limit (if (nil? flow-limit) 50 flow-limit)]
    (letfn [(limited-helper [x]
              (cond (map? x)                                                     (into (empty x)
                                                                                       (map (fn [[k v]] [k (limited-helper v)]))
                                                                                       x)
                    (coll? x)                                                    (into (empty x)
                                                                                       (take flow-limit (map limited-helper x)))
                    (and (not (string? x)) (not (keyword? x)) (not (number? x))) (try (doall (take flow-limit x))
                                                                                      (catch Exception _ x))
                    :else                                                        x))]
      (limited-helper x))))

(defn greeting
  [name]
  (let [now       (java.time.ZonedDateTime/now)
        hour      (.getHour now)
        formatter (java.time.format.DateTimeFormatter/ofPattern "h:mma")]
    (str "Good "    (cond (< hour 12) "morning"
                          (< hour 18) "afternoon"
                          :else       "evening")
         " "        name
         ", it is " (.format now formatter))))

(defn calculate-atom-size
  [name a]
  (try (let [size-bytes (-> @a
                            pr-str
                            .length) ; use .length as a method
             size-mb    (-> size-bytes
                            (/ 1048576.0)
                            (* 1e6)
                            Math/round
                            (/ 1e6))]
         {name {;:bytes size-bytes
                :mb   size-mb
                :keys (try (count (keys @a)) (catch Exception _ -1))}})
       (catch Exception e {name [:error (str e)]})))

(defn cache-distribution
  [tracker-atom percent]
  (let [values                      (vals @tracker-atom)
        sorted-values               (if (every? number? values) (sort values) [])
        total                       (count sorted-values)
        top-10-percent-threshold    (if (not-empty sorted-values) (nth sorted-values (int (* total 0.9))) 0)
        bottom-10-percent-threshold (if (not-empty sorted-values) (nth sorted-values (int (* total 0.1))) 0)
        counts                      (group-by #(cond (< % bottom-10-percent-threshold) :bottom-10-percent
                                                     (> % top-10-percent-threshold)    :top-10-percent
                                                     :else                             :middle-80-percent)
                                              sorted-values)
        frequency-buckets           (group-by #(cond (and (>= % 1) (< % 5))     "1 - 4"
                                                     (and (>= % 5) (< % 20))    "5 - 19"
                                                     (and (>= % 20) (< % 100))  "20 - 99"
                                                     (and (>= % 100) (< % 500)) "100 - 499"
                                                     (>= % 500)                 "500+")
                                              sorted-values)
        dd                          (distinct values)
        total-hit-count             (reduce + (vals @tracker-atom))
        cutoff-hit-count            (* total-hit-count percent)
        sorted-cache                (->> @tracker-atom
                                         (sort-by val)
                                         reverse)
        accumulated-hits            (reductions + (map second sorted-cache))
        cutoff-index                (->> accumulated-hits
                                         (map-indexed vector)
                                         (drop-while (fn [[idx acc]] (< acc cutoff-hit-count)))
                                         ffirst)
        keys-to-keep                (set (map first (take (inc cutoff-index) sorted-cache)))
        cutoff-frequency            (if (seq sorted-cache) (val (nth sorted-cache cutoff-index)) 0)
        above-threshold             (count keys-to-keep)
        below-threshold             (- (count sorted-cache) above-threshold)]
    {:culling             {:cutoff-frequency cutoff-frequency :above-threshold above-threshold :below-threshold below-threshold}
     :total-entries       total
     :top-10-freq-vals    (vec (take 10 (reverse (sort dd))))
     :bottom-10-freq-vals (vec (take 10 (sort dd)))
     :top-10-percent      (count (get counts :top-10-percent))
     :middle-80-percent   (count (get counts :middle-80-percent))
     :bottom-10-percent   (count (get counts :bottom-10-percent))
     :frequency-buckets   (into {} (map (fn [[k v]] [k (count v)]) frequency-buckets))}))

(defn purge-cache
  [name percent tracker-atom data-atom & [hard-limit]]
  (let [total-hit-count  (reduce + (vals @tracker-atom))
        cutoff-hit-count (* total-hit-count percent)
        sorted-cache     (->> @tracker-atom
                              (sort-by val)
                              reverse)
        accumulated-hits (reductions + (map second sorted-cache))
        cutoff-index     (->> accumulated-hits
                              (map-indexed vector)
                              (drop-while (fn [[idx acc]] (< acc cutoff-hit-count)))
                              ffirst)
        cutoff-frequency (if hard-limit hard-limit (if (seq sorted-cache) (val (nth sorted-cache cutoff-index)) 0))
        keys-to-keep     (if hard-limit
                           (set (map first (filter (fn [[k v]] (>= v hard-limit)) sorted-cache)))
                           (set (map first (take (inc cutoff-index) sorted-cache))))
        above-threshold  (count keys-to-keep)
        below-threshold  (- (count sorted-cache) above-threshold)
        distro           (cache-distribution tracker-atom percent)
        pre-mb           (get-in (calculate-atom-size :temp data-atom) [:temp :mb])]
    (swap! data-atom (fn [old-cache] (into {} (filter (fn [[k _]] (keys-to-keep k)) old-cache))))
    (reset! tracker-atom {})
    (let [post-mb (get-in (calculate-atom-size :temp data-atom) [:temp :mb])]
      (pp [:cache-atom-culling! name (str pre-mb "mb") "->" (str post-mb "mb")
               {:top-pct          percent
                :hard-limit       hard-limit
                :cutoff-frequency cutoff-frequency
                :pre-mb           pre-mb
                :distribution     distro
                :post-mb          post-mb
                :keeping          above-threshold
                :purging          below-threshold}]))))


(defn hash-group [key num-groups]
  (mod (hash key) num-groups))


(defn delay-execution [ms f] (future (do (Thread/sleep ms) (f))))





(defn safe-cprint [x] (locking console-lock (puget/with-options {:width (get-terminal-width)} (puget/cprint x))))



(defn ppln [x] (if (>= debug-level 2) (safe-cprint x) ((fn [& _]) x)))

(defn pp [x] (if (>= debug-level 1) (safe-cprint x) ((fn [& _]) x)))


(defn ppa
  [x] ;; always print
  (safe-cprint x))



(defn keywordize-string
  [s] ;; technically spaces are valid in keywords, but they make it a bitch to reference
  (-> (str s)
      (cstr/replace #" " "-")
      (cstr/replace #":" "")
      (cstr/lower-case) ;; Convert to lower case
      (keyword)))                ;; Convert to keyword

(defn unix-timestamp
  []
  (-> (Instant/now)
      (.getEpochSecond)))

(defn cast-to-type
  [value type]
  (try (case type
         :text    (str value)
         :string  (str value) ;; Just in case you want to differentiate between :text and :string
         :integer (Integer. (str value))
         :float   (Float. (str value))
         value) ;; default case
       (catch Exception _ value)))

(defn unkeyword
  [x]
  (if (keyword? x)
    (-> (str x)
        (cstr/replace #":" "")
        (cstr/replace #"-" "_"))
    (str x)))



(defn pretty-spit
  [file-name collection & [width]]
  (let [width (or width 85)]
    (spit (java.io.File. file-name)
          (with-out-str (clojure.pprint/write collection :dispatch clojure.pprint/code-dispatch :right-margin width)))))

(defn copy-file [src dest] (with-open [in-file (io/reader src) out-file (io/writer dest)] (io/copy in-file out-file)))

(defn get-file-vectors-simple
  "returns a vector of filenames/folders in the X folder relative to the jar (packaged by default)
   as well as the working directory so I can use full path names in subsequent commands"
  [relative-folder file-ext]
  (let [is-ext?    (if (= file-ext "directory")
                     #(and (.isDirectory %) (not (cstr/includes? (str %) "/."))) ; remove hidden
                     #(cstr/ends-with? (str %) file-ext)) ; remove anythint not with this .ext
        file-seq   (into [] (for [f (filter is-ext? (.listFiles (io/file relative-folder)))] (str f)))
        files-only (into [] (for [f file-seq] (cstr/replace (str f) relative-folder "")))]
    files-only))

(defn get-file-vectors-with-time-diff
  [relative-folder file-ext]
  (let [is-ext?              (if (= file-ext "directory")
                               #(and (.isDirectory %) (not (cstr/includes? (str %) "/."))) ; remove
                               #(cstr/ends-with? (str %) file-ext)) ; remove anything not with
        file-seq             (into [] (for [f (filter is-ext? (.listFiles (io/file relative-folder)))] [f (.lastModified f)]))
        files-with-time-diff (into []
                                   (for [[f time] file-seq
                                         :let     [tt (- (System/currentTimeMillis) time)]]
                                     [(cstr/replace (str f) relative-folder "") (format-duration-seconds (/ tt 1000)) tt]))]
    files-with-time-diff))

(defn ne? [x] (if (seqable? x) (boolean (seq x)) true))

(defn flatten-map
  [m]
  (for [[category items] m
        [name item]      items]
    (assoc item
      :category category
      :name     name)))

(defn sanitize-name-fn
  [name]   ;; keep updated in client also
  (-> name ;; chars that make file and folder operations annoying
      (clojure.string/replace " " "_")
      (clojure.string/replace "<" "_")
      (clojure.string/replace ">" "_")
      (clojure.string/replace "#" "_")
      (clojure.string/replace "$" "_")
      (clojure.string/replace "+" "_")
      (clojure.string/replace "%" "_")
      (clojure.string/replace "!" "_")
      (clojure.string/replace "`" "_")
      (clojure.string/replace "&" "_")
      (clojure.string/replace "*" "_")
      (clojure.string/replace "'" "_")
      (clojure.string/replace "|" "_")
      (clojure.string/replace "{" "_")
      (clojure.string/replace "}" "_")
      (clojure.string/replace "?" "_")
      (clojure.string/replace "," "_")
      (clojure.string/replace (str (char 34)) "_") ;; double quote
      (clojure.string/replace "=" "_")
      (clojure.string/replace "/" "_")
      (clojure.string/replace ":" "_")
      (clojure.string/replace "@" "_")
      (clojure.string/replace "[" "_")
      (clojure.string/replace "]" "_")
      (clojure.string/replace "\\" "_")))

(def sanitize-name-atom (atom {}))

(defn sanitize-name
  [name]
  (let [cache (get @sanitize-name-atom name)]
    (if cache
      cache
      (let [res (sanitize-name-fn name)]
        (swap! sanitize-name-atom assoc name res)
        res))))


(defn matches-pattern? [item kw num] (and (vector? item) (= (count item) num) (= (first item) kw)))

(defn extract-patterns
  [data kw num]
  (let [matches (atom [])]
    (walk/prewalk (fn [item] (when (matches-pattern? item kw num) (swap! matches conj item)) item) data)
    @matches))

(defn kvpaths
  ([m] (kvpaths [] m ()))
  ([prev m result]
   (reduce-kv (fn [res k v] (if (associative? v) (let [kp (conj prev k)] (kvpaths kp v (conj res kp))) (conj res (conj prev k))))
              result
              m)))

(defn keypath-munger-fn
  [kp]
  (let [nkp (sanitize-name (cstr/replace (cstr/join "_" kp) ":" ""))]
    (-> nkp
        (cstr/replace "-" "_")
        (cstr/replace "(" "") ;; only for safe fields
        (cstr/replace ")" "") ;; only for safe fields
        (cstr/replace "." "_"))))

(def keypath-munger (memoize keypath-munger-fn))

(defn safe-fields
  [f]
  (keyword (let [b (str (keypath-munger (cstr/split (cstr/trim (str f)) #" ")))]
             (if ;; cant have fields that start with an integer
               (try (integer? (Long/parseLong (str (first b)))) (catch Exception _ false))
               (str "_" b)
               b))))

(defn csv-data->maps
  [csv-data]
  (map zipmap
    (->> (first csv-data) ;; First row is the header
         (map safe-fields) ;(map keyword) ;; Drop if you want string keys instead
         repeat)
    (rest csv-data)))

;;(defn nf [num] (cstr/trim (str (format "%,12d" num))))

(defn nf [num]
  (cstr/trim
   (if (float? num)
     (str (format "%,12.2f" num))
     (str (format "%,12d" num)))))

(defn kvpaths
  ([m] (kvpaths [] m ()))
  ([prev m result]
   (reduce-kv (fn [res k v] (if (associative? v) (let [kp (conj prev k)] (kvpaths kp v (conj res kp))) (conj res (conj prev k))))
              result
              m)))

(defn kvpaths2
  ([m] (kvpaths [] m ()))
  ([prev m result]
   (reduce-kv (fn [res k v]
                (if (or (map? v) (vector? v) (list? v)) ;(associative? v)
                  (let [kp (conj prev k)] (kvpaths kp v (conj res kp)))
                  (conj res (conj prev k))))
              result
              m)))

(defn keypaths
  ([m] (keypaths [] m ()))
  ([prev m result]
   (reduce-kv (fn [res k v] (if (associative? v) (keypaths (conj prev k) v res) (conj res (conj prev k)))) result m)))

(defn keypaths2
  ([m] (keypaths [] m ()))
  ([prev m result] (reduce-kv (fn [res k v] (if (map? v) (keypaths (conj prev k) v res) (conj res (conj prev k)))) result m)))
