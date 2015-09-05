(ns kolmogorov-music.test.kolmogorov
  (:require [kolmogorov-music.kolmogorov :as kolmogorov]
            [midje.sweet :refer :all]))

(defn foo [x] (inc x))
(defn bar [x] (+ (inc x) x))
(def baz (comp foo foo))

(fact "Kolmogorov complexity is how many symbols a definition comprises."
  (kolmogorov/complexity foo) => 2)

(fact "The symbol count includes nested sexprs."
  (kolmogorov/complexity bar) => 4)

(fact "The symbol count is recursive within the current namespace."
  (kolmogorov/complexity baz) => 7)

(fact "Symbols outside the current namespace are considered atoms."
  (kolmogorov/complexity inc) => 0)

(fact "Sexprs can also be analysed for complexity."
  (kolmogorov/complexity (+ foo (88 "bar" true))) => 7)

(defn minimal-complexity
  "A hypothetical function that determines the minimal Kolmogorov complexity of a natural number."
  [n]
  (inc n))

(defn first-that [applies? xs]
  (->> xs
       (drop-while (complement applies?))
       first))

(defn more-complex-than? [n limit]
  (< limit (minimal-complexity n)))

(defn enterprise*
  "Find the first natural number with a complexity greater than f."
  [expr ns]
  (->> (range)
       (first-that #(more-complex-than? % (kolmogorov/complexity* expr ns)))))

(defmacro enterprise
  [expr]
  (enterprise* expr *ns*))

(fact "The enterprise makes everything more complicated."
  (enterprise inc) => 0
  (enterprise baz) => 7
  (enterprise enterprise) => 25)
