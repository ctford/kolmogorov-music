(ns kolmogorov-music.test.kolmogorov
  (:require [kolmogorov-music.kolmogorov :as kolmogorov]
            [midje.sweet :refer :all]))

(defn foo [x] (inc x))
(defn bar [x] (+ (inc x) x))
(def baz (comp foo foo))

(fact "Kolmogorov complexity is how many symbols a definition comprises."
  (kolmogorov/complexity foo) => 2)

(fact "The symbol count includes nested sexprs."
  (kolmogorov/complexity bar) => 5)

(fact "The symbol count is recursive within the current namespace."
  (kolmogorov/complexity baz) => 7)

(fact "Symbols outside the current namespace are considered atoms."
  (kolmogorov/complexity inc) => 0)

(fact "Sexprs can also be analysed for complexity."
  (kolmogorov/complexity (+ foo (88 "bar" true))) => 37)

(fact "So can collections."
  (kolmogorov/complexity [1 2 [4 5]]) => 11
  (kolmogorov/complexity #{:x :y}) => 2
  (kolmogorov/complexity {:x [3 4] :y [6 2]}) => 18)

(fact "Data types have complexity based on their bits."
  (kolmogorov/complexity true) => 1
  (kolmogorov/complexity \A) => 7
  (kolmogorov/complexity "foo") => 21
  (kolmogorov/complexity 8) => 3)

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
  (inc (count n)))

(defn first-that [applies? xs]
  (->> xs
       (drop-while (complement applies?))
       first))

(defn more-complex-than? [n limit]
  (< limit (minimal-complexity n)))

(defn enterprise*
  "Find the first natural number with a complexity greater than f."
  [expr ns]
  (->> (kolmogorov/monocon)
       (first-that #(more-complex-than? % (kolmogorov/complexity* expr (kolmogorov/relative-to ns))))))

(defmacro enterprise
  [expr]
  (enterprise* expr *ns*))

(defn yo-dawg []
  (enterprise enterprise))

(fact "The enterprise makes everything more complicated."
  (enterprise inc) => (repeat 0 nil)
  (enterprise baz) => (repeat 7 nil)
  (kolmogorov/complexity enterprise) =>  38
  (enterprise enterprise) => (repeat 38 nil)
  (kolmogorov/complexity yo-dawg) => #(< % (kolmogorov/complexity (yo-dawg))))

(fact "The level of insight is the ratio of Kolmogorov complexity to raw complexity"
  (kolmogorov/complexity [\A \A \A \A \A]) => 40
  (kolmogorov/complexity (repeat 5 \A)) => 13
  (kolmogorov/insight (repeat 5 \A)) => 40/13)
