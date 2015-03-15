(ns kolmogorov-music.kolmogorov
  (:require [clojure.repl :as repl]))

(defn in-ns? [sym ns]
  (contains? (ns-interns ns) sym))

(defn sexpr [sym]
  (-> sym repl/source-fn read-string))

(defn complexity-fn [sym ns]
  (if (in-ns? sym ns)
    (->> (sexpr sym)
         flatten
         (filter #(not= % sym))
         (map #(complexity-fn % ns))
         (reduce +))
    1))

(defmacro complexity [sym]
  (complexity-fn sym *ns*))
