(ns rvbbit-backend.instrument
  (:require
    [metrics.core          :refer [new-registry]]
    [metrics.gauges        :refer [gauge gauge-fn]]
    [metrics.reporters.console :as console]
    [metrics.reporters.csv :as csv]
    [metrics.reporters.graphite :as graphite]
    [metrics.reporters.jmx :as jmx]
    [metrics.timers        :refer [timer]]
    [rvbbit-backend.config :as config])
  (:import
    [java.util.concurrent TimeUnit])
  (:import
    [com.codahale.metrics MetricFilter]))

(def metric-registry (new-registry)) ;; https://metrics-clojure.readthedocs.io/en/latest/
(def graphite-host (get config/settings :graphite-host "127.0.0.1"))




(def GR
  (graphite/reporter metric-registry
                     {:host          graphite-host ;"10.174.1.248" ;; "localhost" ;; graphite
                      :port          2003
                      :prefix        "canvasql.backend"
                      :rate-unit     TimeUnit/SECONDS
                      :duration-unit TimeUnit/MILLISECONDS
                      :filter        MetricFilter/ALL}))
(graphite/start GR 3)