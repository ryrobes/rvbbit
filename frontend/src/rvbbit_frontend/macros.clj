(ns rvbbit-frontend.macros)

;; (defmacro time-expr [expr note]
;;   `(let [start# (.now js/performance)
;;          result# ~expr
;;          end# (.now js/performance)]
;;      (js/console.info (str [(- end# start#)  ~note ]))
;;      result#))

(defmacro time-expr [expr note]
  `(let [start# (.now js/performance)
         result# ~expr
         end# (.now js/performance)
         elapsed# (- end# start#)]
     ;(js/console.info (str [(-> elapsed# (.toFixed 5)) ~note]))
     [result# 
      ;;;elapsed#
      (-> elapsed# (.toFixed 5)) 
      ]))
