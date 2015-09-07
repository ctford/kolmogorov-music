(ns kolmogorov-music.kolmogorov
  (:require [clojure.repl :as repl]
            [midje.sweet :refer :all]))

;;; A billion 'a's ;;;

(comment
  (repeat 1000000000000 \A)
  )

(defmacro description [expr]
  (-> expr str count))

(fact "Kolmogorov description is how long its string representation is."
  (description (repeat 65 \A)) => 14)


(defn extension [value]
  (->> value (apply str) count))

(fact "Kolmogorov extension is how long the string representation of what it evaluates to is."
  (extension (repeat 65 \A)) => 65)

(defmacro randomness [expr]
  `(/ (description ~expr) (extension ~expr)))

(fact "Kolmogorov randomness is the compression ratio between the description and the extension."
  (randomness (repeat 65 \A)) => 14/65)


(defmacro random? [expr]
  `(<= 1 (randomness ~expr)))

(fact "A value is random if its description isn't shorter than its extension."
  (random? (repeat 65 \A)) => false
  (random? (->> 66 char (repeat 14) (take 3))) => true)


(def ascii
  (->> (range 32 127)
       (map char)))

(defn extend-with [elements strings]
  (for [s strings e elements]
    (conj s e)))

(defn kleene* [elements]
  (->> (lazy-seq (kleene* elements))
       (extend-with elements)
       (cons [])))

(defn lexicon []
  (->> ascii
       kleene*
       (map (partial apply str))))

(defn subsequence [start end s]
  (->> s (drop start) (take (- end start))))

(fact "We can construct all strings as a lazy sequence."
  (->> (lexicon) (subsequence 0 5)) => ["" " " "!" "\"" "#"]
  (->> (lexicon) (subsequence 95 100)) => ["~" "  " " !" " \"" " #"]
  (nth (lexicon) 364645) => "GEB")


(defn complexity
  "A hypothetical function that determines the Kolmogorov complexity of a value."
  [string]
  (->> string (map int) (reduce + 0)))

(defn select [applies? xs]
  (->> xs (drop-while (complement applies?)) first))

(defn more-complex-than? [limit]
  (fn [value] (< limit (complexity value))))

(defn enterprise []
  (select
    (more-complex-than? (-> 'enterprise repl/source-fn extension))
    (lexicon)))

