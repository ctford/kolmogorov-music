(ns kolmogorov-music.kolmogorov
  (:require [clojure.repl :as repl]
            [midje.sweet :refer :all :exclude [after]]
            [overtone.live :refer :all :exclude [stop]]
            [leipzig.melody :refer :all]
            [leipzig.canon :as canon]
            [leipzig.scale :as scale]
            [leipzig.live :as live]
            [leipzig.live :refer [stop]]
            [leipzig.chord :as chord]
            [leipzig.temperament :as temperament]
            [kolmogorov-music.instrument :as instrument]
            [kolmogorov-music.coding :as coding]))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Air on a \G String ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(comment
  (repeat 1000000000000 \G)
  )

(defmacro description-length [expr]
  (-> expr print-str count))

(fact "The description-length is how long the string representation of the expression is."
  (description-length (repeat 65 \G)) => 13)


(defn result-length [result]
  (-> result print-str count))

(fact "The result-length is how long the string representation of the evaluated result is."
  (result-length (repeat 65 \G)) => 131)


(defmacro randomness [expr]
  `(/ (description-length ~expr) (result-length ~expr)))

(fact "Kolmogorov randomness is the compression ratio between the description and the result."
  (randomness (repeat 65 \G)) => 13/131)

(defmacro random? [expr]
  `(>= (randomness ~expr) 1))

(fact "A value is random if its description isn't shorter than its result."
  (random? (repeat 65 \G)) => false
  (random? (->> 66 char (repeat 14) (take 3))) => true)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Row, row, row your boat ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def row-row
  "A simple melody built from durations and pitches."
  ; Row, row, row  your boat,
  (->> (phrase [3/3  3/3  2/3  1/3  3/3]
               [  0    0    0    1    2])
       (then
         ; Gent-ly  down the  stream,
         (phrase [2/3  1/3  2/3  1/3  6/3]
                 [  2    1    2    3    4]))
       (then
         ; Merrily, merrily, merrily, merrily,
         (phrase (repeat 12 1/3)
                 (mapcat (partial repeat 3) [7 4 2 0])))
       (then
         ; Life  is   but  a    dream!
         (phrase [2/3  1/3  2/3  1/3  6/3]
                 [  4    3    2    1    0]))
       (canon/canon (canon/simple 4))
       (where :pitch (comp scale/A scale/major))))

(comment
  (live/play row-row)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The Library of Babel ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn kleene* [elements]
  (letfn [(expand [strings] (for [s strings e elements] (conj s e)))]
    (->>
      (lazy-seq (kleene* elements))
      expand
      (cons []))))

(defn babel []
  (let [ascii (->> (range 32 127) (map char))]
    (->> ascii
       kleene*
       (map (partial apply str)))))

(fact "We can construct all strings as a lazy sequence."
  (->> (babel) (take 5)) => ["" " " "!" "\"" "#"]
  (nth (babel) 364645) => "GEB")

(defn dna []
  (->> "GATC"
       kleene*
       (map (partial apply str))))

(fact "We can construct all genes as a lazy sequence."
  (->> (dna) (take 5)) => ["" "G" "A" "T" "C"]
  (nth (dna) 7154) => "GATTACA")

(fact "Lexicons aren't very random."
  (randomness (take 1000 (babel))) => #(< % 1/100)
  (randomness (take 1000 (dna))) => #(< % 1/100))


;;;;;;;;;;;;;;;;;;;;;
;;; Drawing Hands ;;;
;;;;;;;;;;;;;;;;;;;;;

(defn complexity
  "A hypothetical function that determines the Kolmogorov complexity of any value."
  [string]
  (->> string (map int) (reduce + 0)))

(defmacro enterprise
  "Calculate the shortest string that is more complicated than the specified sym."
  [sym]
  `(let [source# (-> ~sym quote repl/source-fn)]
     (->> (babel)
       (drop-while #(<= (complexity %) (result-length source#)))
       first)))

(defn yo-dawg
  "I heard you like complexity, so I put some enterprise in your enterprise."
  []
  (enterprise enterprise))


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


;;;;;;;;;;;;;;;;
;;; Anti EP ;;;;
;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;
;;; Blurred Lines ;;;
;;;;;;;;;;;;;;;;;;;;;

; Arrangement
(defmethod live/play-note :default
  [{hertz :pitch seconds :duration}]
  (when hertz (instrument/overchauffeur (midi->hz hertz) seconds 0.02)))

(defn copyright-infringement-song
  ([skip-to]
   (->>
     (champernowne-word skip-to)
     (coding/decode 3)
     (where :time (bpm 120))
     (where :duration (bpm 120))))
   ([] (copyright-infringement-song 0)))

(def blurred-lines 12450012001200311273127612731276127312761273127612731276127312761245001200121245001200120031127312761273127612731276127312761273127612731276124500120012124500120012003112731276127312761273127612731276127312761273127612450012001212450012001200311273127612731276127312761273127612731276127312761245001200121240001200120031126812711268127112681271126812711268127112681271124000120012124000120012003112681271126812711268127112681271126812711268127112400012001212400012001200311268127112681271126812711268127112681271126812711240001200121252004100411264125012621249126112471245)

(comment
  (live/stop)
  (fx-reverb)
  (fx-distortion)
  (live/play (copyright-infringement-song blurred-lines))
  (live/play (copyright-infringement-song))
  )

;;;;;;;;;;;;;;;;;;;
;;; In the Mood ;;;
;;;;;;;;;;;;;;;;;;;
