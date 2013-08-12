(ns testns)
  ;(:use [myns :as n :only (func2)]))

(use '(myns :as n :only (func2)))

(defn my-func1 []
  (func1 1 2))

(defn my-func2 []
  (func2))

(defn my-func3 []
  (func1))

(defn my-func4 [a b]
  (func2 1 2 3))