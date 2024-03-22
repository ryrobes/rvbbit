(ns rvbbit-backend.camp-boss
  (:require [datalevin.core :as d]
            [tea-time.core :as tt]
            [taskpool.taskpool :as tp]))


;(println (tt/unix-time-micros))   ; Wall clock in microseconds
;1522776026066000
;(tt/linear-time-micros) ; Monotonic clock in microseconds
;128572305580
;(tt/start!)             ; Start threadpool

;(tt/after! 3 (bound-fn [] (prn :hi)))

;; https://github.com/aphyr/tea-time

;; https://github.com/WickedShell/clj-taskpool