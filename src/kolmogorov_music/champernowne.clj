(ns kolmogorov-music.champernowne
  (:require [midje.sweet :refer :all])
  (:import [java.lang Math]))

(defn decompose [n]
  (let [[remainder quotient] ((juxt mod quot) n 10)]
    (if (zero? quotient)
      [remainder]
      (conj (decompose quotient) remainder))))

(defn word
  ([from]
   (->> (range)
        (map (partial + from))
        (mapcat decompose)))
  ([]
   (word 0)))

(fact "The Champernowne word is defined by concatenating the natural numbers base 10."
  (->> (word) (take 16)) => [0 1 2 3 4 5 6 7 8 9 1 0 1 1 1 2])
