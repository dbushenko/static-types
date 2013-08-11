(ns static-types.core
  (:use clojure.walk))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Find function definitions

(defn- function-node? [node]
  (and (list? node)
       (= 'defn (first node))))

(defn- extract-function-definition [node]
  [(nth node 1) (nth node 2)])

(defn find-function-definitions [code]
  (let [functions (atom [])]
    (postwalk (fn [node]
                (if (function-node? node)
                  (swap! functions conj (extract-function-definition node)))
                node)
              code)
    @functions))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Find imports using (require ...) and (use ...) statements

(defn- import-node? [node]
  (and (list? node)
       (or (= 'use (first node))
           (= 'require (first node)))))

(defn- extract-prefix [node]
  (cond
   (= 'use (first node)) ""
   (= 'require (first node)) (if (and (coll? (second node))
                                      (= 3 (count (second node))))
                               [(first (second node)) (nth (second node) 2)]
                               [(second node) (second node)])))

(defn find-prefix-for-ns [code]
  (let [prefix (atom [])]
    (postwalk (fn [node]
                (if (import-node? node)
                  (swap! prefix conj (extract-prefix node)))
                node)
              code)
    @prefix))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Find imports using (:require ...) and (:use ...)
;; statements inside namespace definition

(defn- ns-import-node? [node]
  (and (list? node)
       (or (= :use (first node))
           (= :require (first node)))))

(defn- ns-extract-prefix [node]
  (cond
   (= :use (first node)) ""
   (= :require (first node)) (if (and (coll? (second node))
                                      (= 3 (count (second node))))
                               [(first (second node)) (nth (second node) 2)]
                               [(second node) (second node)])))

(defn find-prefix-for-ns-in-ns [code]
  (let [ns-def (first (filter #(and (list? %)
                                    (= 'ns (first %))) code))]
    (if-not (nil? ns-def)
      (let [prefix (atom [])]
        (postwalk (fn [node]
                    (if (ns-import-node? node)
                      (swap! prefix conj (ns-extract-prefix node)))
                    node)
                  ns-def)
        @prefix))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Find functions of some namespace inside the provided code

(defn get-functions-of-ns [namespace functions imports]
  (let [p (second (first (filter #(= namespace (first %)) imports)))
        prefix (if-not (nil? p) (str p "/") "")]
    (doall (map
            (fn [fdef]
              (let [f (first fdef)
                    params (second fdef)]
                [(symbol (str prefix f)) params]))
            functions))))

(defn get-ns-name [code]
  (second (first (filter #(and (list? %) (= 'ns (first %))) code))))

(defn- find-func-def [name funcs]
  (second (first (filter #(= name (first %)) funcs))))

(defn- check-func-call [fdef fcall cur-func]
  (let [defcount (count fdef)
        defcall (dec (count fcall))]
    (if-not (= defcount defcall)
      (do
        (println "Error in" cur-func "calling" (first fcall))
        (println "You should provide" defcount "parameters!\n")))))

(defn check-functions [code functions]
  (let [cur-func (atom nil)]
    (prewalk (fn [node]
                (if (list? node)
                  (let [fdef (find-func-def (first node) functions)]
                    (cond
                     (= 'defn (first node)) (reset! cur-func (second node))
                     (not (nil? fdef)) (check-func-call fdef node @cur-func))))
                node)
              code))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Testing functions

(defn check-ns1-funcs-in-ns2 [ns1-fname ns2-fname]
  (let [ns1 (read-string (str "[" (slurp ns1-fname) "]"))
        ns2 (read-string (str "[" (slurp ns2-fname) "]"))
        funcs (find-function-definitions ns1)
        p1 (find-prefix-for-ns ns2)
        p2 (find-prefix-for-ns-in-ns ns2)
        imports (concat p1 p2)
        ns-name (get-ns-name ns1)
        prefixed-fs (get-functions-of-ns ns-name funcs imports)]
    (check-functions ns2 prefixed-fs)))