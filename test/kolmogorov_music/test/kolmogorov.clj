(ns kolmogorov-music.test.kolmogorov
  (:require [kolmogorov-music.kolmogorov :as kolmogorov]
            [midje.sweet :refer :all]))

(defn foo [x] (inc x))
(defn bar [x] (+ (inc x) x))
(def baz (comp foo foo))

(def as (repeat 65 \A))

(fact "Kolmogorov intension is how long its string representation is."
  (kolmogorov/intension (repeat 65 \A)) => 14)

(fact "Kolmogorov expression is how long the string representation of what it evaluates to is."
  (kolmogorov/expression (repeat 65 \A)) => 29)

(fact "Kolmogorov randomness is the compression ratio between the intension and the expression."
  (kolmogorov/randomness (repeat 65 \A)) => 14/29)

(fact "A value is random if its intension isn't shorter than its expression."
  (kolmogorov/random? (repeat 65 \A)) => false
  (kolmogorov/random? (->> 66 char (repeat 14) (take 3) first)) => true)

(defn subsequence [start end s]
  (->> s (drop start) (take (- end start))))

(fact "The Kleene star describes all possible sequences of a set of elements."
  (->> #{} kolmogorov/kleene* (subsequence 0 1)) => [[]]
  (->> #{true} kolmogorov/kleene* (subsequence 0 5)) => [[] [true] [true true] [true true true] [true true true true]]
  (->> #{true false} kolmogorov/kleene* (subsequence 0 5)) => [[] [true] [false] [true true] [true false]])

(fact "We can construct all strings as a lazy sequence."
  (->> (kolmogorov/lexicon) (subsequence 0 5)) => ["" " " "!" "\"" "#"]
  (->> (kolmogorov/lexicon) (subsequence 95 100)) => ["~" "  " " !" " \"" " #"]
  (nth (kolmogorov/lexicon) 364645) => "GEB")

(defn minimal-complexity
  "A hypothetical function that determines the minimal Kolmogorov complexity of a natural number."
  [n]
  (-> n str count))

(defn first-that [applies? xs]
  (->> xs
       (drop-while (complement applies?))
       first))

(defn more-complex-than? [n limit]
  (< limit (minimal-complexity n)))

(defmacro enterprise
  [expr]
  `(->> (kolmogorov/monocon)
        (first-that #(more-complex-than? % (kolmogorov/intension ~expr)))))

(fact "The enterprise makes everything more complicated."
  (kolmogorov/intension (repeat 65 \A)) => 14
  (enterprise (repeat 65 \A)) => (repeat 4 nil)
  (enterprise (kolmogorov/definition 'enterprise)) => (repeat 11 nil))
