(ns kolmogorov-music.kolmogorov
  (:require [clojure.repl :as repl]))

(defn in-ns? [sym ns]
  (contains? (ns-interns ns) sym))

(defn sexpr [sym]
  (-> sym repl/source-fn read-string))

(defn definition [sym]
  (-> sym sexpr last))

(defn complexity-fn [sym ns]
  (if (in-ns? sym ns)
    (->> (definition sym)
         flatten
         (map #(-> % (complexity-fn ns) inc))
         (apply +))
    0))

(defmacro complexity [sym]
  (complexity-fn sym *ns*))
