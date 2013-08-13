(ns testns)
  ; (:use myns
  ; 		[clojure.string]))

(use '[clojure [string :as str :only (replace)] pprint] 'myns)

(defn my-func1 []
  (func1 1 2))

(defn my-func2 []
  (func2))

(defn my-func3 []
  (n/func1))

(defn my-func4 [a b]
  (func2 1 2 3))

(defn my-func5 []
  (func1))