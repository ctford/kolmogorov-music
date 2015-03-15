(ns kolmogorov-music.kolmogorov
  (:require [clojure.repl :as repl]))

(defn in-ns? [sym ns]
  (contains? (ns-interns ns) sym))

(defn sexpr [sym]
  (-> sym repl/source-fn read-string))

(defn definition [sym]
  (-> sym sexpr last))

(defn complexity-sym [sym ns]
  (if (in-ns? sym ns)
    (->> (definition sym)
         flatten
         (map #(-> % (complexity-sym ns) inc))
         (apply +))
    0))

(defn complexity-sexpr [sexpr ns]
  (if (seq? sexpr)
    (->> sexpr
         (map #(-> % (complexity-fn ns) inc))
         (apply +))
    (complexity-fn sexpr ns)))

(defmacro complexity [sexpr]
  (complexity-sexpr sexpr *ns*))

(complexity (+ 4 4))
