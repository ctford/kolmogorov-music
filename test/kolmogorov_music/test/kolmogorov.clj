(ns kolmogorov-music.test.kolmogorov
  (:require [kolmogorov-music.kolmogorov :refer :all]
            [midje.sweet :refer :all]))

(defn foo [x] (inc x))
(defn bar [x] (+ (inc x) x))
(def baz (comp foo foo))

(def as (repeat 65 \A))

(fact "Kolmogorov intension is how long its string representation is."
  (intension (repeat 65 \A)) => 14)

(fact "Kolmogorov expression is how long the string representation of what it evaluates to is."
  (expression (repeat 65 \A)) => 29)

(fact "Kolmogorov randomness is the compression ratio between the intension and the expression."
  (randomness (repeat 65 \A)) => 14/29)

(fact "A value is random if its intension isn't shorter than its expression."
  (random? (repeat 65 \A)) => false
  (random? (->> 66 char (repeat 14) (take 3) first)) => true)

(defn subsequence [start end s]
  (->> s (drop start) (take (- end start))))

(fact "The Kleene star describes all possible sequences of a set of elements."
  (->> #{} kleene* (subsequence 0 1)) => [[]]
  (->> #{true} kleene* (subsequence 0 5)) => [[] [true] [true true] [true true true] [true true true true]]
  (->> #{true false} kleene* (subsequence 0 5)) => [[] [true] [false] [true true] [true false]])

(fact "We can construct all strings as a lazy sequence."
  (->> (lexicon) (subsequence 0 5)) => ["" " " "!" "\"" "#"]
  (->> (lexicon) (subsequence 95 100)) => ["~" "  " " !" " \"" " #"]
  (nth (lexicon) 364645) => "GEB")

(fact "The enterprise makes everything more complicated."
  (definitional intension enterprise) => 98
  (expression (enterprise)) => 105)
