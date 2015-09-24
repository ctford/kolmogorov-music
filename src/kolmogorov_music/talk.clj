(ns kolmogorov-music.talk
  (:require [clojure.repl :as repl]
            [midje.sweet :refer :all :exclude [after]]
            [overtone.live :refer :all :exclude [stop]]
            [leipzig.melody :refer :all]
            [leipzig.canon :refer [canon simple]]
            [leipzig.scale :refer [A B major minor]]
            [leipzig.live :as live]
            [leipzig.live :refer [stop]]
            [kolmogorov-music.geb :as geb]
            [kolmogorov-music.coding :as coding]))




























;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Air on the \G String ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(comment (take 10000 (repeat (* 1000 1000 1000 1000) \G)))


(defmacro description-length [expression]
  (-> expression print-str count))

(defn result-length [expression]
  (-> expression print-str count))


(fact "The description-length is how long the string representation of the expression is."
  (description-length (repeat 65 \G)) => 13)

(fact "The result-length is how long the string representation of the evaluated result is."
  (result-length (repeat 65 \G)) => 131)


(defmacro randomness [expression]
  `(/ (description-length ~expression) (result-length ~expression)))

(fact "Kolmogorov randomness is the compression ratio between the description and the result."
  (randomness (repeat 65 \G)) => 13/131
  (randomness (->> 71 char repeat first)) => 26)

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


(defmacro definitionally [macro sym]
  (let [value (-> sym repl/source-fn read-string last)]
    `(~macro ~value)))

(fact "The definitionally macro lets us calculate on the definition of symbols."
  (definitionally description-length row-row) => 287
  (definitionally result-length row-row) => 2081
  (definitionally randomness row-row) => 287/2081)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The Library of Babel ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn kleene* [elements]
  (letfn [(expand [strings] (for [s strings e elements] (conj s e)))]
    (->>
      (lazy-seq (kleene* elements))
      expand
      (cons []))))

(defn library-of-babel []
  (let [ascii (->> (range 32 127) (map char))]
    (->> ascii
       kleene*
       (map (partial apply str)))))

(fact "We can construct all strings as a lazy sequence."
  (->> (library-of-babel) (take 5)) => ["" " " "!" "\"" "#"]
  (nth (library-of-babel) 364645) => "GEB")

(fact "Lexicons aren't very random."
  (randomness (take 10000 (library-of-babel))) => #(< % 1/100))





;;;;;;;;;;;;;;;;
;;; Contact ;;;;
;;;;;;;;;;;;;;;;

(defn decompose [n]
  (let [[remainder quotient] ((juxt mod quot) n 10)]
    (if (zero? quotient)
      [remainder]
      (conj (decompose quotient) remainder))))

(defn champernowne-word
  ([from]
   (->> (range)
        (map (partial + from))
        (mapcat decompose)))
  ([]
   (champernowne-word 0)))

(fact "The Champernowne word is defined by concatenating the natural numbers base 10."
  (->> (champernowne-word) (take 16)) => [0 1 2 3 4 5 6 7 8 9 1 0 1 1 1 2])








;;;;;;;;;;;;
;;; Anti ;;;
;;;;;;;;;;;;

(defmethod live/play-note :default
  [{midi :pitch seconds :duration}]
  (some-> midi midi->hz (geb/overchauffeur seconds)))

(defn copyright-infringement-song
  ([skip-to]
   (->>
     (champernowne-word skip-to)
     (coding/decode 3)
     (tempo (bpm 120))))
   ([] (copyright-infringement-song 0)))

(def blurred-lines 12450012001200311273127612731276127312761273127612731276127312761245001200121245001200120031127312761273127612731276127312761273127612731276124500120012124500120012003112731276127312761273127612731276127312761273127612450012001212450012001200311273127612731276127312761273127612731276127312761245001200121240001200120031126812711268127112681271126812711268127112681271124000120012124000120012003112681271126812711268127112681271126812711268127112400012001212400012001200311268127112681271126812711268127112681271126812711240001200121252004100411264125012621249126112471245)

(comment
  (live/play (copyright-infringement-song blurred-lines))
  (live/play (copyright-infringement-song)))

