(ns kolmogorov-music.test.kolmogorov
  (:require [kolmogorov-music.kolmogorov :as kolmogorov]
            [midje.sweet :refer :all]))

(defn foo [x] (inc x))
(def bar (comp foo foo))

(fact "Kolmogorov complexity is how many symbols a definition comprises."
  (kolmogorov/complexity foo) => 2)

(fact "The symbol count is recursive within the current namespace."
  (kolmogorov/complexity bar) => 5)

(fact "Symbols outside the current namespace are considered atoms."
  (kolmogorov/complexity inc) => 1)
