(ns rvbbit-backend.resolver)




(defmulti resolve (fn [x] (type x)))

(defmethod resolve [Map]) ;; sql-map resolve down to x

(defmethod resolve [Vector]) ;; sql-map partial, get context resolve down to x

(defmethod resolve [Boolean]) ;; return value