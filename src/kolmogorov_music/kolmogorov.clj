(ns kolmogorov-music.kolmogorov
  (:require [clojure.repl :as repl]))

(defn in-ns? [sym ns]
  (let [mappings (ns-interns ns) ]
    (contains? mappings sym)))

(defn sexpr [sym]
  (-> sym repl/source-fn read-string))

(defn definition [sym]
  (-> sym sexpr last))

(declare complexity-sexpr)

(defn complexity-sym [sym terminal?]
  (if-not (terminal? sym)
    (->> (definition sym)
         (complexity-sexpr terminal?))
    0))

(defn complexity-sexpr [terminal? nested-sexpr]
  (let [sexpr (flatten nested-sexpr)]
    (->> sexpr
       (map #(complexity-sym % terminal?))
       (reduce + (count sexpr)))))

(defn complexity* [expr ns]
  (let [terminal? #(not (in-ns? % ns))]
    (if (seq? expr)
      (complexity-sexpr terminal? expr)
      (complexity-sym expr terminal?))))

(defmacro complexity [expr]
  (complexity* expr *ns*))

(def ascii
  (->> (range 32 127)
       (map char)))

(defn extend-with [elements strings]
  (for [s strings e elements]
    (conj s e)))

(defn kleene* [elements]
  (->> (lazy-seq (kleene* elements))
       (extend-with elements)
       (cons [])))

(defn lexicon []
  (->> ascii
       kleene*
       (map (partial apply str))))

(defn monocon []
  (->> #{nil}
       kleene*))
