(ns kolmogorov-music.talk
  (:require [clojure.repl :as repl]
            [midje.sweet :refer :all :exclude [after]]
            [overtone.live :refer :all :exclude [stop]]
            [leipzig.melody :refer :all]
            [leipzig.canon :refer [canon simple]]
            [leipzig.scale :refer [A B major minor]]
            [leipzig.live :as live]
            [leipzig.live :refer [stop]]
            [kolmogorov-music.geb :as geb]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Air on the \G String ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(comment
  (take 10000
        (repeat (* 1000 1000 1000 1000) \G)))


(defmacro description-length [expression]
  (-> expression print-str count))

(defn result-length [expression]
  (-> expression print-str count))


(fact "The description-length is how long the string representation of the expression is."
  (description-length (repeat 65 \G)) => 13)

(fact "The result-length is how long the string representation of the evaluated result is."
  (result-length (repeat 65 \G)) => 131)


(defmacro explanatory-power [expression]
  `(/ (result-length ~expression) (description-length ~expression)))

(fact "Explanatory power is the reciprocal of the compression ratio."
  (explanatory-power (repeat 65 \G)) => 131/13)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The Library of Babel ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn kleene* [elements]
  (letfn [(append-to [strings] (for [s strings e elements] (conj s e)))]
    (->>
      (lazy-seq (kleene* elements))
      append-to
      (cons []))))

(defn library-of-babel []
  (let [ascii (->> (range 32 127) (map char))]
    (->> ascii
         kleene*
         (map (partial apply str)))))

(fact "We can construct all strings as a lazy sequence."
      (->> (library-of-babel) (take 5)) => ["" " " "!" "\"" "#"]
      (nth (library-of-babel) 364645) => "GEB")

(fact "Lexicons are well-explained."
      (explanatory-power (take 10000 (library-of-babel))) => 993)







;;;;;;;;;;;;;;;;;;;;;
;;; Drawing Hands ;;;
;;;;;;;;;;;;;;;;;;;;;

(defn complexity
  "An impossible function that determines the Kolmogorov complexity of any value."
  [string]
  (->> string (map int) (reduce + 0)))

(defn enterprise
  "Calculate the shortest string that is more complicated than itself."
  []
  (let [source (repl/source-fn 'enterprise)]
    (->> (library-of-babel)
         (drop-while #(<= (complexity %) (result-length source)))
         first)))














;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Analysis by compression ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def row-row
  "A simple melody built from durations and pitches."
              ; Row, row, row  your boat,
  (->> (phrase [3/3  3/3  2/3  1/3  3/3]
               [  0    0    0    1    2])
       (then
                ; Gent-ly   down the  stream,
         (phrase [2/3  1/3  2/3  1/3  6/3]
                 [  2    1    2    3    4]))
       (then    ; Merrily, merrily, merrily, merrily,
         (phrase (repeat 12 1/3)
                 (mapcat (partial repeat 3) [7 4 2 0])))
       (then
                ; Life is   but  a    dream!
         (phrase [2/3  1/3  2/3  1/3  6/3]
                 [  4    3    2    1    0]))
       (canon (simple 4))
       (tempo (bpm 100))
       (where :pitch (comp A major))))

(comment
  (live/play row-row)
  (live/jam (var row-row))
)


(defmacro definitional [macro sym]
  (let [value (-> sym repl/source-fn read-string last)]
    `(~macro ~value)))

(fact "Canons have good compression - they're highly structured."
  (definitional description-length row-row) => 275
  (definitional result-length row-row) => 2081
  (definitional explanatory-power row-row) => 2081/275)





;;;;;;;;;;;;;;;;;;;
;;; Arrangement ;;;
;;;;;;;;;;;;;;;;;;;

(defmethod live/play-note :default
  [{midi :pitch seconds :duration}]
  (some-> midi midi->hz (geb/overchauffeur seconds)))
