(ns kolmogorov-music.test.kolmogorov
  (:require [kolmogorov-music.kolmogorov :as kolmogorov]
            [midje.sweet :refer :all]))

(defn foo [x] (inc x))
(def bar (comp foo foo foo))

(fact "The Kolmogorov complexity of a symbol is how many symbols its definition comprises."
  (kolmogorov/complexity foo) => 5
  (kolmogorov/complexity bar) => 6
  (kolmogorov/difference bar foo) => 1)

(fact "in-current-ns? is a macro."
  (kolmogorov/in-current-ns? foo) => true
  (kolmogorov/in-current-ns? inc) => false)
