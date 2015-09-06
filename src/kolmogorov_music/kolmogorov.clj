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

(defmacro definitional [f sym]
  (let [sexpr (-> sym repl/source-fn read-string)]
    `(~f ~sexpr)))

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

(defn complexity
  "A hypothetical function that determines the Kolmogorov complexity of a value."
  [expr]
  (-> expr str count))

(defn select [applies? xs]
  (->> xs (drop-while (complement applies?)) first))

(defn more-complex-than? [n limit]
  (< limit (complexity n)))

(defn enterprise []
  (->> (monocon)
       (select #(more-complex-than? % 101))))
