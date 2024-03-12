(ns rvbbit-backend.instrument
  (:require
   ;[metrics.jvm.core :refer [instrument-jvm]]
   [metrics.core :refer [new-registry]]
   [metrics.gauges :refer [gauge-fn gauge]]
   [metrics.timers :refer [timer]]
   [metrics.reporters.jmx :as jmx]
   [metrics.reporters.console :as console]
   [metrics.reporters.graphite :as graphite]
   [rvbbit-backend.config :as config]
   [metrics.reporters.csv :as csv])
  (:import [java.util.concurrent TimeUnit])
  (:import [com.codahale.metrics MetricFilter]))

(def metric-registry (new-registry)) ;; https://metrics-clojure.readthedocs.io/en/latest/
(def graphite-host (get config/settings :graphite-host "127.0.0.1"))

;(def CR (csv/reporter metric-registry "/tmp/csv_reporter" {}))
;(csv/start CR 5) ;; https://metrics-clojure.readthedocs.io/en/latest/reporting.html#jmx-and-jvisualvm

;(def JR (jmx/reporter metric-registry {}))
;(jmx/start JR)

;(def CR1 (console/reporter metric-registry {}))
;(console/start CR1 10)

(def GR (graphite/reporter metric-registry
                           {:host graphite-host ;"10.174.1.248" ;; "localhost" ;; graphite server
                            :port 2003
                            ;:protocol :udp
                            :prefix "canvasql.backend"
                            :rate-unit TimeUnit/SECONDS
                            :duration-unit TimeUnit/MILLISECONDS
                            :filter MetricFilter/ALL}))
(graphite/start GR 3)