(ns kolmogorov-music.kolmogorov
  (:require [clojure.repl :as repl]
            [midje.sweet :refer :all]))

;;; A billion 'a's ;;;

(comment
  (repeat 1000000000000 \A)
  )

(defmacro description-length [expr]
  (-> expr print-str count))

(fact "Kolmogorov description length is how long its string representation is."
  (description-length (repeat 65 \A)) => 13)


(defn value-length [value]
  (-> value print-str count))

(fact "Kolmogorov value length is how long the string representation of what it evaluates to is."
  (value-length (repeat 65 \A)) => 131)


(defmacro randomness [expr]
  `(/ (description-length ~expr) (value-length ~expr)))

(fact "Kolmogorov randomness is the compression ratio between the description and the value."
  (randomness (repeat 65 \A)) => 13/131)


(defmacro random? [expr]
  `(>= (randomness ~expr) 1))

(fact "A value is random if its description isn't shorter than its value."
  (random? (repeat 65 \A)) => false
  (random? (->> 66 char (repeat 14) (take 3))) => true)


(def ascii
  (->> (range 32 127)
       (map char)))

(defn kleene* [elements]
  (letfn [(expand [strings] (for [s strings e elements] (conj s e)))]
    (->>
      (lazy-seq (kleene* elements))
      expand
      (cons []))))

(defn lexicon []
  (->> ascii
       kleene*
       (map (partial apply str))))

(fact "We can construct all strings as a lazy sequence."
  (->> (lexicon) (take 5)) => ["" " " "!" "\"" "#"]
  (nth (lexicon) 364645) => "GEB")


(defn complexity
  "A hypothetical function that determines the Kolmogorov complexity of any value."
  [string]
  (->> string (map int) (reduce + 0)))

(defn enterprise
  "Calculate the shortest string that is more complicated than itself."
  []
  (let [its-own-source (repl/source-fn 'enterprise)]
    (->> (lexicon)
         (drop-while (fn [s] (<= (complexity s) (value-length its-own-source))))
         first)))

