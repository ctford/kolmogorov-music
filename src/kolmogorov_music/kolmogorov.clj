(ns kolmogorov-music.kolmogorov
  (:require [clojure.repl :as repl]))

(defmacro intension [expr]
  (count
    (if (symbol? expr)
      (repl/source-fn expr)
      (str expr))))

(defmacro expression [expr]
  (-> expr eval str count))

(defmacro randomness [expr]
  `(/ (intension ~expr) (expression ~expr)))

(defmacro random? [expr]
  `(<= 1 (randomness ~expr)))

(defn in-ns? [sym ns]
  (let [mappings (ns-interns ns) ]
    (contains? mappings sym)))

(defn sexpr [sym]
  (-> sym repl/source-fn read-string))

(defn definition [sym]
  (-> sym sexpr last))

(declare complexity*)

(defn complexity-sym [sym terminal?]
  (if-not (terminal? sym)
    (complexity* (definition sym) terminal?)
    0))

(defn complexity-sexpr [terminal? sexpr]
  (->> sexpr
       (map #(complexity* % terminal?))
       (reduce + (count sexpr))))

(defn complexity* [expr terminal?]
  (cond
    (seq? expr) (complexity-sexpr terminal? expr)
    (coll? expr) (complexity-sexpr terminal? expr)
    (#{true false} expr) 1
    (string? expr) (* 7 (count expr))
    (char? expr) 7
    (integer? expr) (int (Math/ceil (/ (Math/log expr) (Math/log 2))))
    :otherwise (complexity-sym expr terminal?)))

(defn relative-to [ns]
  #(not (in-ns? % ns)))

(defmacro complexity [expr]
  (complexity* expr (relative-to *ns*)))

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

(defmacro insight [expr]
  (/ (complexity* (eval expr)(relative-to *ns*))
     (complexity* expr (relative-to *ns*))))

