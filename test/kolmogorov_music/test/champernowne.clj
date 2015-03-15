(ns kolmogorov-music.test.champernowne
  (:require [kolmogorov-music.champernowne :as champernowne]
            [midje.sweet :refer :all]))

(fact "The Champernowne constant is defined by concatenating the natural numbers base 10."
  (->> champernowne/constant (take 12))
    => [0 1 2 3 4 5 6 7 8 9 1 0])
