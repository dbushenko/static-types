(ns testns
  (:require [myns :as m]))

(defn my-func1 []
  (m/func1 1 2))

(defn my-func2 []
  (m/func2))

(defn my-func3 []
  (m/func1))

(defn my-func4 [a b]
  (m/func2 1 2 3))