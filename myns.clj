(ns myns)

(defn func1 [a b]
  (+ a b))

(defn func2 [c]
  (+ c (func1 1 2)))