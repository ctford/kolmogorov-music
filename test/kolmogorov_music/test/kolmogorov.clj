(ns kolmogorov-music.test.kolmogorov
  (:require [kolmogorov-music.kolmogorov :as kolmogorov]
            [midje.sweet :refer :all]))

(defn foo [x] (inc x))
(def bar (comp foo foo))

(fact "The Kolmogorov complexity of a symbol its character length"
  (kolmogorov/complexity foo) => 22
  (kolmogorov/complexity bar) => 24
  (kolmogorov/difference bar foo) => 2
  (kolmogorov/less-than bar foo) => false 
  (kolmogorov/less-than foo bar) => true)
