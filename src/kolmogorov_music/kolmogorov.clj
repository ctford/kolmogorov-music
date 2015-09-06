(ns kolmogorov-music.kolmogorov
  (:require [clojure.repl :as repl]))

(defmacro intension [expr]
  (-> expr str count))

(defmacro expression [expr]
  (-> expr eval str count))

(defmacro randomness [expr]
  `(/ (intension ~expr) (expression ~expr)))

(defmacro random? [expr]
  `(<= 1 (randomness ~expr)))

(defmacro definition [sym]
  `(-> ~sym repl/source-fn read-string))

(definition 'definition)

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
