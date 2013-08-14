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

; (defn- remove-params 
;   ([code] (remove-params code false))
;   ([code ignore]
;   (if (empty? code)
;     []
;     (let [current (first code)
;           tail (rest code)]
;       (if ignore
;         (remove-params tail false)
;         (if (keyword? current) 
;           (remove-params tail true)
;           (cons current (remove-params tail false))))))))

(defn- get-param [param code]
  (let [current (first code)
        tail (rest code)]
    (if (not (empty? tail))
      (if (= current param)
        (first tail)
        (get-param param tail)))))

(comment defn- simple-use? [code]
  (if (empty? code)
    false
    (or (keyword? (first code))
        (simple-use? (rest code)))))

(defn- unquote* [code]
  (if (coll? code)
    (if (= (first code) 'quote)
      (second code)
      code)
    code))

(comment defn- parse-use-simple 
  ([node]
    (let [path (first node)
          tail (rest node)
          only-funcs (set (get-param :only tail))
          general-entry [path nil (if (nil? only-funcs) 
                            #{}
                            only-funcs)]
          prefix (get-param :as tail)]
      (if (nil? prefix)
        [general-entry]
        [general-entry [path prefix #{}]])))
  ([path node]
    (let [name (symbol (str path "." (first node)))
          only-funcs (set (get-param :only node))
          general-entry [name nil (if (nil? only-funcs) 
                            #{}
                            only-funcs)]
          prefix (get-param :as node)]
      (if (nil? prefix)
        [general-entry]
        [general-entry [name prefix #{}]]))))

(comment defn- parse-require-simple 
  ([node]
    (let [path (first node)
          tail (rest node)
          only-funcs (set (get-param :only tail))
          general-entry [path nil (if (nil? only-funcs) 
                            #{}
                            only-funcs)]
          prefix (get-param :as tail)]
      [path (if (nil? prefix)
              path
              prefix)
            (if (nil? only-funcs) 
              #{}
              only-funcs)]
  ([path node]
    (let [name (symbol (str path "." (first node)))
          only-funcs (set (get-param :only node))
          general-entry [name nil (if (nil? only-funcs) 
                            #{}
                            only-funcs)]
          prefix (get-param :as node)]
      (if (nil? prefix)
        [general-entry]
        [general-entry [name prefix #{}]]))))

(comment defn- parse-use-2 
  ([node]
    (let [current (first node)
          tail (rest node)]
      (if (empty? tail)
        [[current nil #{}]]
        (parse-use-2 current tail))))
  ([path node]
    (let [current (first node)
          tail (rest node)]
      (concat 
        (if (empty? tail)
          []
          (parse-use-2 path tail))
        (if (coll? current)
          (parse-use-simple path current)
          [[(symbol (str path "." (first node))) nil #{}]])))))

(comment defn- parse-use-1 [node]
  (let [node1 (unquote* node)]
    (if (coll? node1)
      (if (simple-use? node1)
        (parse-use-simple node1)
        (parse-use-2 node1))
      [[node1 nil #{}]])))

(comment defn- parse-use [code]
  (if (simple-use? code)
    (parse-use-simple code)
    (apply concat (map parse-use-1 code))))

(defrecord NsNode [full-ns-name ns-prefix functions])

(defn- make-ns-symbol-with-prefix [prefix ns-name]
  (symbol (str prefix "." ns-name)))

(defn- simple-ns-use? [node]
  (symbol? node)) 

(defn- ns-with-children-use? [node]
  (if (coll? node)
    (empty? (filter keyword? node))))

(defn- ns-with-options-use? [node]
  (if (coll? node)
    (not (empty? (filter keyword? node)))))

(defn- parse-simple-ns-use [node]
  (NsNode. node nil #{}))

(defn- parse-ns-with-children-use [node]
  (let [prefix (first node)
        child (rest node)]
    (if (empty? child) 
      (NsNode. prefix nil #{})
      (map #(if (simple-ns-use? %)
              (NsNode. (make-ns-symbol-with-prefix prefix %)
                        nil
                        #{})
              ;; () 
            child)
    ))))

(defn- parse-ns-with-options-use [node]
  )

(defn- parse-use [code]
  (let [node (unquote* code)]
    (cond 
      (simple-ns-use? node) (parse-simple-ns-use node)                ;; (use 'myns.abc)
      (ns-with-children-use? node) (parse-ns-with-children-use node)  ;; (use '[myns.abc a1 a2])
      (ns-with-options-use? node) (parse-ns-with-options-use node)    ;; (use '[myns.abc.a1 :as a1])
      :default nil)))

(defn- parse-require [code]
  )

(defn- import-node? [node]
  (and (list? node)
       (or (= 'use (first node))
           (= 'require (first node)))))

(defn- extract-prefix [node]
  (cond
   (= 'use (first node)) (apply concat (map parse-use (rest code)))
   (= 'require (first node)) (if (and (coll? (second node))
                                      (= 3 (count (second node))))
                               [[(first (second node)) (nth (second node) 2) #{}]]
                               [[(first (second node)) (first (second node)) #{}]])))

(defn find-prefix-for-ns [code]
  (let [prefix (atom [])]
    (postwalk (fn [node]
                (if (import-node? node)
                  (swap! prefix concat (extract-prefix node)))
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
   (= :use (first node)) (parse-use (rest node))
   (= :require (first node)) (if (and (coll? (second node))
                                      (= 3 (count (second node))))
                               [[(first (second node)) (nth (second node) 2) #{}]]
                               [[(first (second node)) (first (second node)) #{}]])))

(defn find-prefix-for-ns-in-ns [code]
  (let [ns-def (first (filter #(and (list? %)
                                    (= 'ns (first %))) code))]
    (if-not (nil? ns-def)
      (let [prefix (atom [])]
        (postwalk (fn [node]
                    (if (ns-import-node? node)
                      (swap! prefix concat (ns-extract-prefix node)))
                    node)
                  ns-def)
        @prefix))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Find functions of some namespace inside the provided code

(defn- apply-import [functions import]
  (let [prefix (second import)
        funcs (nth import 2)]
    (into [] (doall (map
            (fn [fdef]
              (let [f (first fdef)
                    params (second fdef)]
                [(symbol (str (if (nil? prefix) "" (str prefix "/")) f)) params]))
            (filter #(or (empty? funcs) (contains? funcs (first %))) functions))))))

(defn get-functions-of-ns [namespace functions imports]
  (let [result-funcs (atom [])]
    (doall (map #(swap! result-funcs concat (apply-import functions %)) (filter #(= namespace (first %)) imports)))
    @result-funcs))

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

(def black-list #{'ns 'use 'require})

(defn- check-functions* [node functions cur-func]
  (if (coll? node)
    (let [fdef (find-func-def (first node) functions)]
      (cond 
       (and 
        (= 'defn (first node))
        (not= @cur-func :ignore))
         (reset! cur-func (second node))
       (contains? black-list (first node)) (do (reset! cur-func :ignore))
       (and 
        (not (nil? fdef))
        (not= @cur-func :ignore))
         (check-func-call fdef node @cur-func))
      (doall (map #(check-functions* % functions cur-func) node))
      (if (contains? black-list (first node))
          (reset! cur-func nil)))))

(defn check-functions [code functions]
  (let [cur-func (atom nil)]
    (check-functions* code functions cur-func))
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
    (println funcs)
    (println p1)
    (println p2)
    (println imports)
    (println prefixed-fs)
    (check-functions ns2 prefixed-fs)))


; Examples

; (use '[myns :as m :only (func2)])
; (use 'myns 'clojure.string)
; (use 'myns '[clojure.string])
; (use '[clojure string] 'myns)
; (use '[clojure [string :as str :only (replace)] pprint] 'myns)


;; TODO
;; :use 
;;      :exclude
;;      :rename
;; :require
;;      (require '(clojure zip [set :as s]))
