(ns kolmogorov-music.test.champernowne
  (:require [kolmogorov-music.champernowne :as champernowne]
            [midje.sweet :refer :all]))

(fact "The Champernowne word is defined by concatenating the natural numbers base 10."
  (->> (champernowne/word) (take 10))
    => [0 1 2 3 4 5 6 7 8 9])

(fact "The Champernowne constant is the fractional expansion of the Champernowne word."
  (champernowne/constant 10) => 0.123456789)

(fact "The Champernowne index is the first occurance of a sequence in the Champernowne word."
  (champernowne/index [4 5]) => 4)
