(ns rvbbit-backend.flowmaps
  (:refer-clojure :exclude [abs update-vals update-keys])
  (:require
    [chime.core              :as chime]
    [clojure.core.async      :as async]
    [clojure.edn             :as edn]
    [clojure.set             :as cset]
    [clojure.spec.alpha      :as s]
    [clojure.spec.test.alpha :as stest]
    [clojure.string          :as cstr]
    [clojure.walk            :as walk]
    [debux.core              :as dx]
    [rvbbit-backend.util     :as ut])
  (:import
    [java.lang.management ManagementFactory]
    java.time.Instant
    java.time.LocalTime
    java.time.LocalDate
    [java.time            LocalTime Duration Instant ZonedDateTime ZoneId Period DayOfWeek]
    [java.text            SimpleDateFormat]
    [java.util            Date TimeZone Calendar]
    java.time.format.DateTimeFormatter))

(def results-atom (atom {})); (datom {} "results-atom")) ;; basically a log
(def resolved-paths (atom {})); (datom {} "resolved-paths"))
(def channel-history (atom {})); (datom {} "channel-history"))
(def fn-history (atom {})); (datom {} "fn-history"))
(def block-dump (atom {})); (datom {} "block-dump"))
(def chains-completed (atom {})); (datom [] "chains-completed"))
(def block-defs (atom {})); (datom {} "block-defs"))
(def hidden-values (atom {})); (datom {} "hidden-values"))
(def waffle-data (atom {}));  (datom {} "waffle-data"))

(def port-accumulator (atom {}))
(defonce channels-atom (atom {})) ;; ref for putting things on later via REPL or whatever.
(defonce condi-channels-atom (atom {})) ;; ref for putting things on later via REPL or whatever.
(defonce working-data (atom {}))
(defonce live-schedules (atom []))
(defonce sample-limit 20) ;; to keep huge seqs from getting to console print and web-ui
(defonce rewinds 0) ;; limit to history events for UI / logging

(defmacro flow>
  [& forms]
  (let [step-count (atom 0)
        labels     (doall (map (fn [_]
                                 (do (swap! step-count inc) (keyword (str "step" @step-count))))
                            forms))
        conns      (vec (map vector labels (rest labels)))
        components (vec (mapcat vector labels forms))]
    `{:components ~(apply hash-map components) :connections ~conns}))

(defn hide-secret
  [flow-id x] ;; TODO, upcoming secrets, this just hides from the UI for now
  (try (walk/postwalk-replace (get @hidden-values flow-id) x) (catch Exception _ x)))

(defn limited
  [x flow-id]
  (let [flow-limit (get-in @working-data [flow-id :limit] sample-limit)]
    (hide-secret flow-id
                 (letfn [(limited-helper [x]
                           (cond (map? x)
                                   (into (empty x) (map (fn [[k v]] [k (limited-helper v)])) x)
                                 (coll? x) (into (empty x) (take flow-limit (map limited-helper x)))
                                 (and (not (string? x)) (not (keyword? x)) (not (number? x)))
                                   (try (doall (take flow-limit x)) (catch Exception _ x))
                                 :else x))]
                   (limited-helper x)))))

(defn ppf [x] (ut/pp x))




(s/def ::fn? (s/and ifn? #(not (map? %))))

(s/def ::components
  (s/or :function          ::fn?
        :map-with-function (s/and map? (s/keys :req-un [::fn?]))
        :static-value      #(and (not (fn? %)) (not (map? (:fn %))))))

(s/def ::connection-pair
  (s/and vector?
         (s/coll-of (s/or :keyword keyword?
                          :any     any?)
                    :count 2)))

(s/def ::connections (s/and vector? (s/and (s/coll-of ::connection-pair) (complement empty?))))

(s/def ::network-map (s/keys :req-un [::connections ::components]))

(s/def ::opts-map (s/keys :opt-un [::debug? ::debux?]))

(s/def ::done-ch
  #(or (ut/chan? %) (and (seqable? %) (ut/chan? (first %))) (instance? clojure.lang.Atom %)))

(s/def ::subflow-map map?)






(defn push!
  [flow-id block-id keypath values & [ts tse]] ;; for default subscription queue usage (soon
  (let [bd            (try (get-in @block-defs [flow-id (nth keypath 2)]) (catch Exception _ nil))
        rewind        (get-in @working-data [flow-id :rewinds])
        rewind-limit? (not (= rewind 0))
        tse           (when tse (if (= ts tse) (+ tse 2) tse)) ;; add 2 ms for viz purposes!
        tsh-full      (vec (sort (distinct (conj (conj (get-in @block-dump [flow-id :ts-history] [])
                                                       ts)
                                                 tse))))
        tsh           (if rewind-limit? (take rewind (reverse tsh-full)) tsh-full)]
    (swap! block-dump assoc-in keypath values) ;; populate block-dump atom incrementally
    (when (and ts tse) ;; supplemental dump keys - only want history entries, not rando key
      #_{:clj-kondo/ignore [:redundant-do]}
      (do ;; ^ again. *not* redundant here.
        (swap! block-dump assoc-in
          [flow-id :block-history block-id]
          (conj (if rewind-limit?
                  (vec (take-last rewind
                                  (sort-by
                                    :start
                                    (get-in @block-dump [flow-id :block-history block-id] []))))
                  (get-in @block-dump [flow-id :block-history block-id] []))
                {:start ts :end tse :body values}))
        (swap! block-dump assoc-in [flow-id :ts-history] tsh)))
    (when (and ts bd)
      (swap! waffle-data assoc
        flow-id
        (conj (if rewind-limit?
                (vec (take-last rewind (sort-by :start (get @waffle-data flow-id))))
                (get @waffle-data flow-id))
              {:name   (str (nth keypath 2))
               :type   (ut/data-typer (get values :v))
               :start  ts
               :end    tse
               :number (count (filter #(= (get % :name) (str (nth keypath 2)))
                                (get @waffle-data flow-id)))})))))

(defn push-channel-value
  [channels flow-id value]
  (doall
    (doseq [channel-name channels]
      (let [start        (System/currentTimeMillis)
            channel-name (if (string? channel-name) (edn/read-string channel-name) channel-name) ;; front-end
                                                                                                 ;; channel
                                                                                                 ;; names
                                                                                                 ;; are
                                                                                                 ;; strings
            from         (first channel-name)
            v            value]
        (ut/pp [:incoming-channel-push channel-name value])
        (push! flow-id
               from
               [flow-id :blocks from :body]
               {:v value}
               start
               (System/currentTimeMillis))
        (swap! channel-history update
          flow-id
          conj
          {:path      [:pushed :from :web]
           :type      :channel
           :channel   channel-name
           :dest      (last channel-name)
           :start     start
           :end       (System/currentTimeMillis)
           :value     (limited v flow-id)
           :data-type (ut/data-typer v)})
        (swap! fn-history assoc
          flow-id
          (conj (get @fn-history flow-id [])
                {:block      from
                 :from       :static
                 :path       [:from :static from]
                 :value      (limited value flow-id)
                 :type       :function
                 :dest       from
                 :channel    [from]
                 :data-type  (ut/data-typer (limited value flow-id))
                 :start      start
                 :end        (System/currentTimeMillis)
                 :elapsed-ms (- (System/currentTimeMillis) start)}))
        (async/put! (get-in @channels-atom [flow-id channel-name])
                    {:sender (last channel-name) :value value})
        [[:pushed flow-id channel-name] value]))))


(defn wait-for-event
  [channel flow-id start-time timeout-ms]
  (try (let [deadline (+ (System/currentTimeMillis) timeout-ms)]
         (loop []
           (let [events (get @channel-history flow-id)
                 event  (first (filter #(and (= (get % :channel) channel)
                                             (>= (get % :end) start-time))
                                 events))]
             (cond event event ;(get event :value) ; Found the desired event, return it
                   (> (System/currentTimeMillis) deadline)
                     (throw (Exception. (str "Timeout waiting for event to hit channel " channel
                                             " in flow "                                 flow-id)))
                   :else (do (Thread/sleep 300) ; Wait for a short duration before polling again
                             (recur))))))
       (catch Exception e {:error (str e)})))

(defn find-keys-with-path
  [paths-map path-segment]
  (let [segment-length (count path-segment)]
    (reduce (fn [acc [k v]]
              (if (some (fn [path]
                          (some #(= path-segment (subvec path % (+ % segment-length)))
                                (range 0 (+ 1 (- (count path) segment-length)))))
                        v)
                (conj acc k)
                acc))
      []
      paths-map)))

(defn build-all-paths
  [node connections visited]
  (let [next-nodes (map second (filter #(= (first %) node) connections))]
    (if (node (set visited))
      [[[node :loop-to] (first visited)]]
      (if (empty? next-nodes)
        [[node]]
        (for [next next-nodes
              path (build-all-paths next connections (cons node visited))]
          (cons node path))))))

(defn build-paths-from-all-nodes
  [connections]
  (mapcat #(build-all-paths % connections []) (set (map first connections))))

(defn path-map
  [connections]
  (into {}
        (for [v (build-paths-from-all-nodes connections)]
          {(hash v) (list (vec (flatten v)))})))

(defn- end-chain-msg
  [flow-id fp ff result the-path debug?] ;; TODO better calc of pathfinding to derive natural
  (let [fp-vals          (vec (for [f the-path] [f (get-in @results-atom [flow-id f])]))
        res-paths        (distinct (get @resolved-paths flow-id))
        chains           (count res-paths)
        last-in-chain?   (some #(= the-path %) res-paths)
        chains-run       (+ (count (distinct @chains-completed)) 1)
        all-chains-done? (= chains chains-run)]
    (when debug? (ut/pp [the-path :chain-done! ff :done-hop-or-empty :res-paths res-paths]))
    (when last-in-chain?
      (do (when debug?
            (ut/pp [chains-run :chain-done :done-hop ff :steps (count fp-vals) :final-result result
                    :value-path fp-vals chains-run :of chains :flow-chains-completed]))
          (swap! chains-completed conj the-path))
      (when (and all-chains-done? last-in-chain?)
        (when debug?
          (ut/pp [:all-chains-done! :resolved-paths (vec (distinct (get @resolved-paths flow-id)))
                  ff :fp fp :results-atom (get @results-atom flow-id)]))))))

(defn close-channels!
  [flow-id]
  (doseq [[k c] (get @channels-atom flow-id)]
    (try #_{:clj-kondo/ignore [:redundant-do]}
         (do ;(ut/pp [:closing-channel k c])
           (try (when (not (nil? c))
                  (do (swap! channels-atom ut/dissoc-in [flow-id k]) (async/close! c)))
                (catch Throwable e
                  (do (swap! channels-atom assoc-in [flow-id k] c)
                      (ut/pp [:error-closing-channel-inner e k c])))) ;; close channel
         ) ;; remove from "live" channel atom
         (catch Throwable e
           (do (swap! channels-atom assoc-in [flow-id k] c) ;; if failed, keep key val for now
               (ut/pp [:error-closing-channel-outer e k c]))))))

(defn close-channel!
  [flow-id channel-id]
  (let [ch (get-in @channels-atom [flow-id channel-id])]
    (try (when (not (nil? ch))
           (do (swap! channels-atom ut/dissoc-in [flow-id channel-id]) (async/close! ch)))
         (catch Throwable e (ut/pp [:error-closing-channel-inner e flow-id channel-id]))) ;; close
    (swap! channels-atom ut/dissoc-in [flow-id channel-id])))

(declare flow) ;; so we can call it in (process ..) even though it's still undefined at this point

(defn- process
  [flow-id connections input-chs output-chs ff fp block-function opts-map done-ch]
  (let [web-push?                              false ;(true? @web/websocket?)
        {:keys [debug? debux? close-on-done?]} opts-map
        pp                                     (if debug? ut/pp ut/pp) ;; TODO, grody
        channel-results                        (atom (into {} (map #(vector % nil) input-chs)))]
    (pp ["Starting processing " ff " with input channels: " input-chs " and output channels: "
         output-chs])
    (try
      (async/go-loop [pending-chs input-chs]
        (when (seq pending-chs)
          (let [wstart                 (System/currentTimeMillis) ;; start on channel (for
                [data ch]              (async/alts! pending-chs)
                data-from              (get data :sender)
                data-val               (get data :value :done)
                next-hops              (map second (filter #(= ff (first %)) connections))
                triples                [data-from ff (first next-hops)]
                t2                     triples ; (vec (remove #(= % :done) (remove nil?
                poss-path-keys         (find-keys-with-path fp t2)
                is-accumulating-ports? (true? (get data-val :port-in?))
                start                  (System/currentTimeMillis) ;; non wait
                the-path               (try (apply max-key
                                              count
                                              (apply concat (vals (select-keys fp poss-path-keys))))
                                            (catch Exception _ [:error-path-issue!]))
                valid-output-channels? (or (ut/chan? done-ch) ;; single output channel
                                           (try               ;; or several
                                             (ut/chan? (first done-ch))
                                             (catch Exception _ false)))
                done-channel-push?     (and (= ff :done) valid-output-channels?)
                pre-print              (get block-function :pre-print)
                post-print             (get block-function :post-print)]
            (when (= ff :done) ;; process any :done stuff, debug or not. ends this
              (let [done-channel? valid-output-channels? ;(ut/chan? done-ch)
                    multi-ch?     (seqable? done-ch)
                    done-atom?    (instance? clojure.lang.Atom done-ch)
                    channels      (+ (count (keys (get @channels-atom flow-id)))
                                     (count (keys (get @condi-channels-atom flow-id))))]
                (cond done-atom?                    (reset! done-ch data-val)
                      (and done-channel? multi-ch?) (doseq [d done-ch] (async/>! d data-val))
                      done-channel?                 (async/>! done-ch data-val)
                      :else                         nil)
                (when close-on-done? (close-channels! flow-id)) ;; kill all channels for this flow.
                                                                ;; we done.
                (pp [:done-block-reached
                     (cond done-atom?    [:sending-to-atom done-ch]
                           done-channel? [:sending-to-channel done-ch]
                           :else         [:ending-flow-chain
                                          (if debug?
                                            [:debug-keeping channels :channels-open]
                                            [:closing channels :channels])])])))
            (when is-accumulating-ports?
              (swap! port-accumulator assoc ff (merge (get @port-accumulator ff {}) data-val))) ;; ff
            (swap! channel-results assoc ch data-val)
            (swap! channel-history update
              flow-id
              conj
              (let [v (if (and (map? data-val) (get data-val :port-in?))
                        (first (vals (dissoc data-val :port-in?)))
                        data-val)]
                {:path      the-path ;; PRE RECUR HISTORY PUSH.
                 :type      :channel
                 :channel   [data-from ff]
                 :dest      ff
                 :start     start
                 :end       (System/currentTimeMillis)
                 :value     (limited v flow-id)
                 :data-type (ut/data-typer v)}))
            (when (and (or (nil? data-val) (= data-val :done)) close-on-done?)
              (let [chk  (ut/keypaths (get @channels-atom flow-id))
                    chkp (for [c chk] {(get-in @channels-atom [flow-id c]) c})
                    chp  (get chkp ch)]
                (close-channel! flow-id chp)))
            (if (or (and (not (ut/namespaced? ff)) (not (ut/namespaced? data-from)))
                    (not (some nil? (vals @channel-results))))
              (if (or (get data-val :error) (= data-val :done)) ;; receiving :done/error keyword as
                                                                ;; a value is a
                (do (pp [:chain-done! ff :done-signal])
                    (end-chain-msg flow-id fp ff nil the-path debug?))
                (let [block-fn-raw? (fn? block-function)
                      is-subflow? (s/valid? ::network-map block-function)
                      block-is-port?
                        (and (not block-fn-raw?) (get block-function :fn) (ut/namespaced? ff))
                      inputs (get block-function :inputs [])
                      resolved-inputs (walk/postwalk-replace (get @port-accumulator ff) inputs)
                      fully-resolved?
                        (= 0 (count (cset/intersection (set resolved-inputs) (set inputs))))
                      pre-in (cond (and fully-resolved?
                                        (not is-subflow?) ;; subflows namespaced keys are not
                                        is-accumulating-ports?)
                                     resolved-inputs
                                   :else data-val)
                      _ (when pre-print
                          (ut/pp [flow-id ff (str (java.time.LocalDateTime/now)) :pre
                                  (try (if (and fully-resolved? is-accumulating-ports?) ;; TODO
                                         (apply pre-print pre-in) ;; resolved inputs
                                         (pre-print pre-in))
                                       (catch Exception e [:pre-print-error (str e)]))]))
                      pre-when? (try (if (and fully-resolved? is-accumulating-ports?)
                                       (apply (get block-function :pre-when? (fn [_] true)) pre-in) ;; resolved
                                       ((get block-function :pre-when? (fn [_] true)) pre-in))
                                     (catch Exception _ true)) ;; hmm, maybe false instead?
                      subflow-overrides
                        (when is-subflow?
                          (into {}
                                (for [[k v] (dissoc (get @port-accumulator ff) ;data-val
                                              :port-in?)]
                                  {(keyword (last (cstr/split (str (ut/unkeyword k)) #"/"))) v})))
                      expression
                        (delay
                          (try (cond (not pre-when?) [:failed-pre-check :w-input pre-in] ;; failed
                                     (and fully-resolved? is-subflow?) ;(and is-subflow?
                                       (let [a (atom nil)] ;; use temp atom to hold subflow
                                         (flow block-function
                                               {:flow-id (str flow-id "/" (ut/unkeyword ff))
                                                :debug?  debug?}
                                               a ;output-chs
                                               subflow-overrides)
                                         (while (nil? @a) (Thread/sleep 100))
                                         @a)
                                     (and fully-resolved? is-accumulating-ports?)
                                       (apply (get block-function :fn) resolved-inputs)
                                     block-fn-raw? (block-function data-val)
                                     block-is-port? {:port-in? true ff data-val} ;; just
                                     :else ((get block-function :fn) data-val))
                               (catch Exception e
                                 (do (ut/pp [:chain-dead! :exec-error-in ff :processing data-val
                                             :from data-from :block-fns block-function :error e]) ; :done
                                     {:error (str e)}))))
                      expression-dbgn
                        (delay
                          (try ;; testing - to be refactored away shortly w macros
                            (cond (not pre-when?) [:failed-pre-check :w-input pre-in] ;; failed
                                  is-subflow? (let [a (atom nil)] ;; use temp atom to hold
                                                (flow block-function
                                                      {:flow-id (str flow-id "/" (ut/unkeyword ff))
                                                       :debug?  debug?}
                                                      a ;output-chs
                                                      subflow-overrides)
                                                (while (nil? @a) (Thread/sleep 100))
                                                @a)
                                  (and fully-resolved? is-accumulating-ports?)
                                    (dx/dbgt_ (apply (get block-function :fn) resolved-inputs))
                                  block-fn-raw? (dx/dbgt_ (block-function data-val))
                                  block-is-port? (dx/dbgt_ {:port-in? true ff data-val}) ;; just
                                  :else (dx/dbgt_ ((get block-function :fn) data-val)))
                            (catch Exception e
                              (do (ut/pp [:chain-dead! :exec-error-in ff :processing data-val :from
                                          data-from :block-fns block-function :error e]) ; :done
                                  {:error (str e)}))))
                      {:keys [result fn-start fn-end elapsed-ms dbgn-output]}
                        (merge (ut/timed-exec @expression)
                               (if debux? {:dbgn-output (with-out-str @expression-dbgn)} {}))
                      condis (into {}
                                   (for [[path f] (get block-function :cond)]
                                     {path (try (f result)
                                                (catch Exception e
                                                  (do (ut/pp [:block ff :cond path :condi-error e])
                                                      false)))}))
                      condi-path (for [[k v] condis :when v] k)
                      condi-channels (vec (remove nil?
                                            (for [c condi-path]
                                              (get-in @condi-channels-atom [flow-id [ff c]]))))
                      error? (get result :error)
                      post-when? (try ((get block-function :post-when? (fn [_] true)) result)
                                      (catch Exception _ true)) ;; default to true on error?
                      view (if error? ;; if error, we'll force a view to better surface it in
                             (fn [x] [:re-com/v-box :style
                                      {:font-family "Poppins" :font-size "16px"} :padding "4px"
                                      :children
                                      [[:re-com/box :align :center :justify :center :style
                                        {:font-size "30px" :font-weight 700 :color "red"} :child
                                        (str "error:")]
                                       [:re-com/box :padding "6px" :style {:color "orange"} :child
                                        (str (get x :error))]]])
                             (get block-function :view))
                      speak-fn (get block-function :speak) ;; or [:speak :fn] later
                      speak (when (fn? speak-fn) (try (speak-fn result) (catch Exception _ nil)))
                      view-out (when (fn? view)
                                 (try (let [vv (view result)] ;; if view isn't hiccup and just
                                        (if (string? vv)
                                          [:re-com/box :width :width-px :height :height-px :child vv
                                           :align :center :justify :center :padding "10px" :size
                                           "none" :style
                                           {:font-size   "18px"
                                            :margin-top  "30px"
                                            :font-weight 700
                                            :color       :vcolor}]
                                          vv))
                                      (catch Exception e {:cant-eval-view-struct e :block ff})))
                      output-chs (remove nil?
                                   (cond ;; output channel, condis, or regular output switch
                                     (or (not pre-when?) (not post-when?))       [] ;; failed
                                                                                    ;; post/pre-when,
                                                                                    ;; send nowhere.
                                     (and done-channel-push? (seqable? done-ch)) done-ch
                                     done-channel-push?                          [done-ch]
                                     #_{:clj-kondo/ignore [:not-empty?]}
                                     (not (empty? condi-path))                   condi-channels
                                     :else                                       output-chs))
                      web-val-map (merge (cond (and fully-resolved? is-accumulating-ports?)
                                                 {:v     (limited result flow-id) ;; since it's
                                                                                  ;; going
                                                  :input (limited resolved-inputs flow-id)}
                                               (and block-is-port? (ut/namespaced? ff)) ;; only true
                                                 {:v       (limited data-val flow-id) ;; since it's
                                                  :port-of (first next-hops)}
                                               :else {:v     (limited result flow-id) ;; since it's
                                                      :input (limited data-val flow-id)})
                                         (if #_{:clj-kondo/ignore [:not-empty?]}
                                           (not (empty? condis))
                                           {:cond condis}
                                           {})
                                         (if view {:view view-out} {})
                                         (if speak {:speak speak} {}))
                      value-only (fn [x]
                                   (if (and (map? x) (get x :port-in?))
                                     (first (vals (dissoc x :port-in?)))
                                     x))]
                  (when post-print
                    (ut/pp [flow-id ff (str (java.time.LocalDateTime/now)) :post
                            (try (post-print result)
                                 (catch Exception e [:post-print-error (str e)]))]))
                  (when (and web-push? (not (= ff :done))) ;; :done block is hidden, so no pushing
                                                           ;; data
                    (push! flow-id
                           ff
                           [flow-id :blocks ff :body]
                           web-val-map
                           start
                           (System/currentTimeMillis)))
                  (swap! resolved-paths assoc flow-id (conj (get @resolved-paths flow-id) the-path))
                  (swap! results-atom assoc-in [flow-id ff] (limited result flow-id)) ;; final /
                  (swap! fn-history assoc
                    flow-id
                    (conj (get @fn-history flow-id [])
                          {:block      ff
                           :from       data-from
                           :path       the-path
                           :value      (limited (value-only result) flow-id)
                           :type       :function
                           :dest       ff
                           :channel    [ff]
                           :dbgn       (str dbgn-output)
                           :data-type  (ut/data-typer (limited (get web-val-map :v) flow-id))
                           :start      fn-start
                           :end        fn-end
                           :elapsed-ms elapsed-ms}))
                  (try (pp [:block ff [data-from ff] :has-received
                            (limited (value-only data-val) flow-id) :to-web
                            (limited (get web-val-map :v) flow-id) :from data-from :next-channel
                            output-chs :has-output (limited (value-only result) flow-id)])
                       (catch Exception e (ut/pp [:?test? e])))
                  (try (when #_{:clj-kondo/ignore [:not-empty?]}
                         (not (empty? output-chs))
                         (do (if (seq output-chs)
                               (doseq [oo output-chs] ;; forward processed data to the output
                                 (try (async/>! oo
                                                (if done-channel-push?
                                                  result ;; no map wrapper for return channel
                                                  {:sender ff :value result}))
                                      (catch Exception e
                                        (ut/pp [:caught-in-push-loop e :outputs (count output-chs)
                                                :this oo :output-chs output-chs]))))
                               (async/>! output-chs {:sender ff :value result}))))
                       (catch Exception e
                         (ut/pp [:caught-in-doseq-push-loop e :output-chs output-chs])))
                  (if (empty? pending-chs)
                    (end-chain-msg flow-id fp ff result the-path debug?) ;; never runs in
                    (recur pending-chs))))
              (recur pending-chs)))))
      (catch Throwable e (ut/pp [:main-go-loop-in-process-error! e])))))


(defn flow
  [network-map & [opts-map done-ch subflow-map]]
  (try ;; to pretty print the exception data only and not tons of other unhelpful garbage
    (cond ;; check spec, was instrumenting but the error msgs were horrible
      (not (s/valid? ::network-map network-map))
        (throw (ex-info "Invalid network-map"
                        {:exception      :flow-cannot-run!
                         :failed-spec-on :flow-map
                         :issue          (s/explain-str ::network-map network-map)
                         :input          network-map}))
      (and opts-map (not (s/valid? ::opts-map opts-map)))
        (throw (ex-info "Invalid opts-map"
                        {:exception      :flow-cannot-run!
                         :failed-spec-on :flow-opts-map
                         :issue          (s/explain-str ::opts-map opts-map)
                         :input          opts-map}))
      (and done-ch (not (s/valid? ::done-ch done-ch)))
        (throw (ex-info "Invalid done-ch"
                        {:exception      :flow-cannot-run!
                         :failed-spec-on :output-channel-or-atom
                         :issue          (s/explain-str ::done-ch done-ch)}))
      (and subflow-map (not (s/valid? ::subflow-map subflow-map)))
        (throw (ex-info "Invalid subflow-map"
                        {:exception      :flow-cannot-run!
                         :failed-spec-on :subflow-override-map
                         :issue          (s/explain-str ::subflow-map subflow-map)
                         :input          subflow-map}))
      :else
        (let [flow-id (or (get opts-map :flow-id) (ut/generate-name))
              flow-id (if (not (= flow-id "live-scratch-flow"))
                        (str flow-id
                             "-"
                             (count (filter #(cstr/starts-with? % flow-id)
                                      (keys @channel-history))))
                        flow-id)] ;; don't number the scratch-flow iterations...
          (swap! results-atom dissoc flow-id)
          (swap! channel-history dissoc flow-id)
          (swap! fn-history dissoc flow-id)
          (swap! block-dump dissoc flow-id)
          (swap! waffle-data dissoc flow-id)
          (swap! block-defs dissoc flow-id) ;; save and ship these in case the UI somehow
          (when #_{:clj-kondo/ignore [:not-empty?]} ;; clean up old channels on new run, if
            (and (not (empty? (get @channels-atom flow-id))) ;; - we have channels
                 (not (get opts-map :debug? true))) ;; - debug is NOT enabled
            (doseq [[k c] (get @channels-atom flow-id)]
              (try #_{:clj-kondo/ignore [:redundant-do]}
                   (do (ut/pp [:closing-channel k c]) (async/close! c))
                   (catch Exception e (ut/pp [:error-closing-channel e k c])))))
          (try
            (let [{:keys [components connections]} network-map
                  opts-map                         (merge {:debug?         true
                                                           :debux?         false
                                                           :close-on-done? true}
                                                          opts-map) ;; merge w defaults, pass
                  {:keys [debug? debux?]}          opts-map
                  pp                               (if debug? ut/pp ut/pp) ;; TODO, gross.
                  web-push?                        false ;(true? @web/websocket?)
                  gen-connections                  (for [[_ f] connections
                                                         :let  [base (ut/unkeyword
                                                                       (first (cstr/split (str f)
                                                                                          #"/")))]
                                                         :when (cstr/includes? (str f) "/")]
                                                     [f (keyword base)])
                  components                       (into
                                                     {} ;; todo, integrate this into the logic
                                                     (for [[k v] components] ;; <- sub-flow
                                                       {k (if (s/valid? ::network-map v)
                                                            (merge
                                                              v
                                                              {:inputs
                                                                 (vec (for [c (filter
                                                                                #(cstr/includes?
                                                                                   (str %)
                                                                                   (str k "/"))
                                                                                (distinct
                                                                                  (flatten
                                                                                    connections)))]
                                                                        (keyword (last (cstr/split
                                                                                         (str c)
                                                                                         #"/")))))})
                                                            v)}))
                  named-connections                connections
                  connections                      (vec (distinct (into connections
                                                                        gen-connections)))
                  channels                         (into {}
                                                         (for [conn connections] ;; TODO
                                                           {conn (async/chan 1)}))
                  components                       (assoc (merge ;; filter out comps that have no
                                                            (merge
                                                              (into {}
                                                                    (for [[_ v] components
                                                                          :when (get v :cond)]
                                                                      (get v :cond)))
                                                              (into
                                                                {}
                                                                (flatten
                                                                  (for [[k v] components
                                                                        :let  [ins (get v :inputs)]
                                                                        :when ins]
                                                                    (for [i ins]
                                                                      {(keyword (str
                                                                                  (ut/unkeyword k)
                                                                                  "/"
                                                                                  (ut/unkeyword i)))
                                                                         {:fn :port}})))))
                                                            components)
                                                     :done {:fn (fn [x] x)}) ;; add in 'implied'
                  condi-connections                (vec (apply concat
                                                          (for [[k v] components
                                                                :when (get v :cond)]
                                                            (for [c (keys (get v :cond))] [k c]))))
                  condi-channels                   (into {}
                                                         (for [conn condi-connections]
                                                           {conn (async/chan 1)}))
                  connections                      (vec (distinct (into connections
                                                                        condi-connections))) ;; add
                  coords-map                       (ut/coords-map connections)
                  fp                               (path-map connections)
                  sources                          (fn [to]
                                                     (vec (distinct (for [[ffrom tto] connections
                                                                          :when       (= tto to)]
                                                                      ffrom))))
                  components                       (select-keys components
                                                                (vec (distinct (flatten
                                                                                 (apply conj
                                                                                   connections))))) ;; no
                  input-mappings                   (into {} ;; mostly for web
                                                         (for [b (keys components)]
                                                           {b (vec (remove #{b}
                                                                     (flatten (filter #(= (second %)
                                                                                          b)
                                                                                connections))))}))
                  started                          (atom #{})]
              (when web-push? (push! flow-id :none [flow-id :blocks] {}))
              (swap! working-data assoc
                flow-id ;; network-map
                {:connections     named-connections
                 :points          (get network-map :points)
                 :hide            (get network-map :hide)
                 :limit           (get network-map :limit sample-limit)
                 :rewinds         (get network-map :rewinds rewinds)
                 :description     (get network-map :description)
                 :colors          (get network-map :colors :Paired)
                 :docs            (into {}
                                        (remove empty?
                                          (for [[k {:keys [doc]}] (get network-map :components)
                                                :when             (not (nil? doc))]
                                            {k doc})))
                 :gen-connections gen-connections
                 :components-list (keys components)
                 :components      (pr-str components)
                 :condis          condi-connections})
              (pp [:gen-connections gen-connections :connections connections :fp fp :comps
                   components :coords-map coords-map :condi-connections condi-connections])
              (when web-push?
                (push! flow-id :none [flow-id :network-map] (get @working-data flow-id)))
              (when web-push?
                #_{:clj-kondo/ignore [:redundant-do]} ;; lol, it is, in fact, NOT redundant.
                (do
                  (doseq [[bid v] components
                          :when   (not (= bid :done))
                          :let    [from-val (get components bid) ;; incoming fn or static -
                                   static-input? (and (not (fn? from-val)) ;; not a raw fn block
                                                      (nil? (get from-val :fn)))
                                   is-subflow? (s/valid? ::network-map from-val)
                                   view-mode (get-in network-map [:canvas bid :view-mode])
                                   hidden1? (get-in network-map [:canvas bid :hidden?])
                                   hidden? (if (and (nil? hidden1?) (ut/namespaced? bid))
                                             true
                                             hidden1?)
                                   block-def (merge {:y (or (get-in network-map [:canvas bid :y])
                                                            (get v :y (get-in coords-map [bid :y]))) ;; deprecated
                                                     :x (or (get-in network-map [:canvas bid :x])
                                                            (get v :x (get-in coords-map [bid :x]))) ;; deprecated
                                                     :base-type :artifacts
                                                     :width (or (get-in network-map
                                                                        [:canvas bid :w])
                                                                (get v :w 240))  ;; deprecated
                                                     :height (or (get-in network-map
                                                                         [:canvas bid :h])
                                                                 (get v :h 255)) ;; deprecated
                                                     :type :text
                                                     :flowmaps-created? true
                                                     :block-type "map2"
                                                     :hidden? hidden?
                                                     :hidden-by
                                                       (when (ut/namespaced? bid)
                                                         (try (edn/read-string
                                                                (first (cstr/split (str bid) #"/")))
                                                              (catch Exception _ nil)))
                                                     :view-mode (if (get v :view) "view" "data")
                                                     :options [{:prefix ""
                                                                :suffix ""
                                                                :block-keypath [:view-mode]
                                                                :values
                                                                  (vec
                                                                    (remove nil?
                                                                      ["data"
                                                                       (when (get v :view) "view")
                                                                       (when (get v :doc) "doc")
                                                                       "grid" "text"
                                                                       (when (and (not
                                                                                    static-input?)
                                                                                  debux?)
                                                                         "dbgn")
                                                                       (when (and (not is-subflow?)
                                                                                  static-input?)
                                                                         "input")]))}]}
                                                    (when is-subflow? {:subflow? true})
                                                    (when static-input?
                                                      {:text-input? true ;; override the view mode w
                                                                         ;; input
                                                       :view-mode   "input"})
                                                    (when view-mode {:view-mode view-mode}))]]
                    (do (swap! block-defs assoc-in [flow-id bid] block-def)
                        (push! flow-id bid [flow-id :blocks bid] block-def)))
                  (doseq [[bid v] (get network-map :canvas)              ;; decorative
                                                                         ;; front-end-only blocks
                          :when   (string? bid)
                          :let    [block-def {:y          (or
                                                            (get-in network-map [:canvas bid :y])
                                                            (get v :y (get-in coords-map [bid :y]))) ;; deprecated
                                              :x          (or
                                                            (get-in network-map [:canvas bid :x])
                                                            (get v :x (get-in coords-map [bid :x]))) ;; deprecated
                                              :base-type  :artifacts
                                              :width      (or (get-in network-map [:canvas bid :w])
                                                              (get v :w 240)) ;; deprecated
                                              :height     (or (get-in network-map [:canvas bid :h])
                                                              (get v :h 255)) ;; deprecated
                                              :type       :text
                                              :inputs     (get-in network-map [:canvas bid :inputs])
                                              :block-type "text"}]]
                    (do (swap! block-defs assoc-in [flow-id bid] block-def)
                        (push! flow-id bid [flow-id :blocks bid] block-def)))
                  (doseq [[bid inputs] input-mappings
                          :when        (not (= bid :done))
                          :let         [inputs (vec (conj (vec (for [i inputs]
                                                                 [i [:text [:no nil]]]))
                                                          [nil [:text [:paragraph 0]]]))]] ;; not
                    (do (swap! block-defs assoc-in [flow-id bid :inputs] inputs)
                        (push! flow-id bid [flow-id :blocks bid :inputs] inputs)))))
              (swap! channels-atom assoc flow-id channels)
              (swap! condi-channels-atom assoc flow-id condi-channels)
              (doseq [c connections] ;;; "boot" channels into history even if they never get
                (swap! channel-history update
                  flow-id
                  conj
                  {:path      [:creating-channels :*]
                   :channel   c
                   :start     (System/currentTimeMillis)
                   :end       (System/currentTimeMillis)
                   :value     nil
                   :data-type "boot"}))
              (doseq [from (keys components)]
                (let [start             (System/currentTimeMillis)
                      subflow-override? (not (nil? (get subflow-map from)))
                      fn-is-subflow?    (s/valid? ::network-map (get components from))
                      from-val          (if subflow-override?
                                          (get subflow-map from)
                                          (get components from))
                      out-conns         (filter #(= from (first %)) connections)
                      in-conns          (filter #(= from (second %)) connections)
                      in-condi-conns    (filter #(= from (second %)) condi-connections)
                      to-chans          (map channels (filter #(= from (first %)) connections))]
                  (pp (str "  ... Processing: " from))
                  (if (and (not (fn? from-val)) ;; not a raw fn
                           (nil? (get from-val :fn))
                           (not fn-is-subflow?)) ;; make sure its not a sub-flow map / we will
                    (let [text-input? (true? (some #(= :text-input %) (flatten [from-val])))
                          from-val    (if text-input? (last from-val) from-val)]
                      (when (some #(= % from) (get network-map :hide))
                        (swap! hidden-values assoc-in [flow-id from-val] "***HIDDEN-FROM-UI***"))
                      (when web-push?
                        (push! flow-id
                               from
                               [flow-id :blocks from :body]
                               (str {:v (limited from-val flow-id)})))
                      (pp (vec (remove nil?
                                 [:block from :pushing-static-value from-val :to
                                  (vec (map last out-conns))
                                  (when subflow-override? :subflow-value-override!)])))
                      (swap! started conj from)
                      (swap! results-atom assoc-in [flow-id from] from-val)
                      (when web-push? ;; seeding the block history with this static value, just
                        (push! flow-id
                               from
                               [flow-id :blocks from :body]
                               {:v (limited from-val flow-id)}
                               start
                               (System/currentTimeMillis)))
                      (swap! fn-history assoc
                        flow-id
                        (conj (get @fn-history flow-id [])
                              {:block      from
                               :from       :static
                               :path       [:from :static from]
                               :value      (limited from-val flow-id)
                               :type       :function
                               :dest       from
                               :channel    [from]
                               :data-type  (ut/data-typer (limited from-val flow-id))
                               :start      start
                               :end        (System/currentTimeMillis)
                               :elapsed-ms (- (System/currentTimeMillis) start)})) ;; temp modded
                      (doseq [c to-chans] (async/put! c {:sender from :value from-val})))
                    (let [;_ (ut/pp [:pre-subflow-block from from-val])
                          in-chans     (remove nil?
                                         (into (map channels in-conns)
                                               (map condi-channels in-condi-conns)))
                          has-starter? (get from-val :starter)
                          out-chans    (map channels out-conns)
                          from-val     (if (map? from-val) ;(and (map? from-val) (not (s/valid?
                                         (merge from-val
                                                {:inputs ;(if (s/valid? ::network-map from-val)
                                                   (vec (for [i (get from-val :inputs)]
                                                          (keyword (str (ut/unkeyword from)
                                                                        "/"
                                                                        (ut/unkeyword i)))))})
                                         from-val)
                          srcs         (sources from)]
                      (pp ["Creating channels for" from :-> srcs :already-started
                           (count (set @started)) :blocks])
                      (let [] ;do ;when parents-ready? ;; placeholder
                        (pp ["Starting block " from " with channel(s): " in-chans])
                        (swap! started conj from)
                        (async/thread (pp ["In thread for " from " reading from channel " in-chans])
                                      (process flow-id
                                               connections
                                               in-chans
                                               out-chans
                                               from
                                               fp
                                               from-val
                                               opts-map
                                               done-ch)))
                      (when has-starter? ;; 95% dupe code from above - refactor TODO - used to
                        (let [;text-input? false ;(true? (some #(= :text-input %) (flatten
                              from-val (get from-val :starter)] ;; lazy override temp for this
                          (when web-push?
                            (push! flow-id from [flow-id :blocks from :body] (str {:v from-val})))
                          (pp (vec (remove nil?
                                     [:block-starter from :pushing-static-value from-val :to
                                      (vec (map last out-conns))
                                      (when subflow-override? :subflow-value-override!)])))
                          (swap! started conj from)
                          (swap! results-atom assoc-in [flow-id from] from-val)
                          (when web-push? ;; seeding the block history with this static value,
                            (push! flow-id
                                   from
                                   [flow-id :blocks from :body]
                                   {:v from-val}
                                   start
                                   (System/currentTimeMillis)))
                          (swap! fn-history assoc
                            flow-id
                            (conj (get @fn-history flow-id [])
                                  {:block      from
                                   :from       :starter
                                   :path       [:from :starter from]
                                   :value      (limited from-val flow-id)
                                   :type       :function
                                   :dest       from
                                   :channel    [from]
                                   :data-type  (ut/data-typer (limited from-val flow-id))
                                   :start      start
                                   :end        (System/currentTimeMillis)
                                   :elapsed-ms (- (System/currentTimeMillis) start)})) ;; temp
                          (doseq [c to-chans] (async/put! c {:sender from :value from-val})))))))))
            (catch Exception e (ut/pp [:flow-error e])))))
    (catch clojure.lang.ExceptionInfo e (ut/pp (.getData e)))))



(defn flow-results
  [] ;; somewhat pointless, but hey. TODO
  (into {}
        (for [flow-id (keys @resolved-paths)]
          {flow-id (let [res (dissoc (into {}
                                           (for [p (vec (distinct (get @resolved-paths flow-id)))]
                                             {p (for [pp p]
                                                  [pp (get-in @results-atom [flow-id pp])])}))
                               [:error-path-issue!])] ;; TODO
                     res)})))

(defn ask [flow-id point-name value])

(defn schedule!
  [time-seq1 flowmap & args]
  (let [[opts chan-out override] args
        opts                     (if (nil? opts) {} opts)
        times                    (if (and (vector? time-seq1) (keyword? (first time-seq1)))
                                   (doall (take 1000 (ut/time-seq time-seq1)))
                                   time-seq1)] ;; if custom time list, just send it.
    (swap! live-schedules conj
      {:flow-id  (get opts :flow-id "unnamed-flow-sched")
       :override override
       :schedule (if (vector? time-seq1) time-seq1 [:custom-time-fn])})
    (chime/chime-at times ;; [] chime time seq, https://github.com/jarohen/chime#recurring-schedules
                    (fn [time]
                      (let [opts (merge opts {:schedule-started (str time)})]
                        (flow flowmap opts chan-out override)))
                    {:on-finished   (fn [] (ut/pp [:schedule-finished! opts time-seq1]))
                     :error-handler (fn [e] (ut/pp [:scheduler-error e]))})))

(def my-network
  {:description "a simple example flow: addition and integers"
   :components  {:comp1          10
                 :comp2          20
                 :comp3          [133 45]
                 :simple-plus-10 {:fn #(+ 10 %)}
                 :add-one        #(+ 1 %)
                 :adder-one      {:fn #(apply + %) :inputs [:in]}
                 :adder          {:fn + :inputs [:in1 :in2]}}
   :connections [[:comp1 :adder/in1] [:comp2 :adder/in2] [:comp3 :adder-one/in]
                 [:adder-one :add-one] [:adder :simple-plus-10] [:simple-plus-10 :done]]
   :canvas      {:adder-one      {:x 1009 :y 765 :h 207 :w 297 :view-mode "data"}
                 :add-one        {:x 1400 :y 754 :h 247 :w 440 :view-mode "data"}
                 :adder-one/in   {:x 650 :y 746 :h 255 :w 240 :view-mode "data"}
                 :comp2          {:x 249 :y 423 :h 142 :w 155 :view-mode "data"}
                 :comp3          {:x 283 :y 771 :h 203 :w 251 :view-mode "input"}
                 :adder/in1      {:x 486 :y 160 :h 192 :w 242 :view-mode "data"}
                 :comp1          {:x 252 :y 204 :h 127 :w 148 :view-mode "input"}
                 :adder          {:x 875 :y 332 :h 207 :w 198 :view-mode "data"}
                 :simple-plus-10 {:x 1189 :y 307 :h 224 :w 239 :view-mode "data"}
                 :adder/in2      {:x 490 :y 426 :h 201 :w 244 :view-mode "data"}}})

(def test-flow-map-1
  {:components  {:comp1 10
                 :comp2 20
                 :comp3 [133 45]
                 :simple-plus-10 {:fn #(+ 10 %) :x 1222 :y 312}
                 :add-one {:fn #(+ % 1) :x 1574 :y 452 :w 220 :h 173}
                 :add-one2 {:fn #(+ 1 %) :x 367 :y 1492}
                 :add-one3 {:fn #(+ 1 %) :x 839 :y 1481}
                 :counter {:fn   #(count %)
                           :view (fn [x] [:re-com/box :child (str x " loops") :align :center
                                          :justify :center :padding "10px" :style
                                          {:color "yellow" :font-weight 700 :font-size "100px"}])
                           :h    225
                           :w    725
                           :x    1981
                           :y    915}
                 :conjer
                   {:fn   (fn [x] (defonce vvv (atom [])) (do (swap! vvv conj x) @vvv))
                    :view (fn [x] [:vega-lite
                                   {:data       {:values (map-indexed (fn [index value]
                                                                        {:index index :value value})
                                                                      x)}
                                    :mark       {:type "bar" :color "#FC0FC0"}
                                    :encoding   {:x {:field "index"
                                                     :type  "ordinal"
                                                     :title "index of conjs pass"
                                                     :axis  {:labelColor  "#ffffff77"
                                                             :ticks       false
                                                             :titleColor  "#ffffff"
                                                             :gridColor   "#ffffff11"
                                                             :labelFont   "Poppins"
                                                             :titleFont   "Poppins"
                                                             :domainColor "#ffffff11"}}
                                                 :y {:field "value"
                                                     :type  "quantitative"
                                                     :title "random additive values"
                                                     :axis  {:labelColor    "#ffffff77"
                                                             :titleColor    "#ffffff"
                                                             :ticks         false
                                                             :gridColor     "#ffffff11"
                                                             :labelFont     "Poppins"
                                                             :titleFont     "Poppins"
                                                             :labelFontSize 9
                                                             :labelLimit    180
                                                             :domainColor   "#ffffff11"}}}
                                    :padding    {:top 15 :left 15}
                                    :width      "container"
                                    :height     :height-int
                                    :background "transparent"
                                    :config     {:style {"guide-label" {:fill "#ffffff77"}
                                                         "guide-title" {:fill "#ffffff77"}}
                                                 :view  {:stroke "#00000000"}}} {:actions false}])
                    :w    650
                    :h    371
                    :x    1217
                    :y    848}
                 :add-one4 {:fn   #(do (Thread/sleep 120) (+ 45 %))
                            :x    595
                            :y    1087
                            :cond {:condicane2 #(> % 200)}}
                 :whoops {:fn #(str % ". YES.") :x 1627 :y 1337}
                 :condicane {:fn   #(str % " condicane!")
                             :view (fn [x] [:re-com/box :child
                                            (str "conditional path that never gets run or seen" x)
                                            :align :center :justify :center :padding "10px" :style
                                            {:color "darkcyan" :font-weight 700 :font-size "20px"}])
                             :x    2069
                             :y    1235
                             :h    255
                             :w    240}
                 :condicane2 {:fn #(str "FINAL VAL: " % " DONE") :x 1210 :y 1268}
                 :baddie #(str % " is so bad!")
                 :baddie2 {:fn #(+ % 10) :x 1941 :y 589 :w 256 :h 167}
                 :looper (fn [x] x)
                 :adder {:fn + :y 254 :x 839 :inputs [:in1 :in2]}}
   :connections [[:comp1 :adder/in1] ;; channels directly to port
                 [:comp2 :adder/in2] [:adder :simple-plus-10] [:condicane2 :whoops]
                 [:add-one4 :add-one2] [:add-one4 :conjer] [:conjer :counter] [:whoops :done]
                 [:simple-plus-10 :add-one] [:add-one :add-one2] [:add-one2 :add-one3]
                 [:add-one3 :add-one4]]})

(def simple-flow
  {:components  {:comp1          10
                 :comp2          20
                 :comp3          [133 45]
                 :simple-plus-10 #(+ 10 %)
                 :add-one        #(+ 1 %)
                 :adder-one      {:fn #(apply + %) :inputs [:in]}
                 :adder          {:fn + :inputs [:in1 :in2]}}
   :connections [[:comp1 :adder/in1] [:comp2 :adder/in2] [:comp3 :adder-one/in]
                 [:adder-one :add-one] [:adder :simple-plus-10] [:simple-plus-10 :done]]
   :canvas      {:adder-one      {:x 732 :y 764 :h 255 :w 240}
                 :add-one        {:x 1123 :y 785 :h 199 :w 280}
                 :adder-one/in   {:x 430 :y 760 :h 255 :w 240}
                 :comp2          {:x 100 :y 430 :h 255 :w 240}
                 :comp3          {:x 136 :y 791 :h 203 :w 251}
                 :adder/in1      {:x 430 :y 100 :h 255 :w 240}
                 :comp1          {:x 100 :y 100 :h 255 :w 240}
                 :adder          {:x 862 :y 307 :h 225 :w 278}
                 :simple-plus-10 {:x 1255 :y 327 :h 194 :w 307}
                 :adder/in2      {:x 430 :y 430 :h 255 :w 240}}})

(def debug? true)

(defn flow-waiter
  [in-flow]
  (doall (let [a (atom nil)]
           (flow in-flow {:debug? debug?} a)
           (while (nil? @a) (Thread/sleep 100)) ;; check the atom every 100 ms
           (do (ut/pp [:done!]) @a))))







