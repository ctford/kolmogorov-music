(ns kolmogorov-music.kolmogorov
  (:require [clojure.repl :as repl]
            [midje.sweet :refer :all :exclude [after]]
            [overtone.live :refer :all]
            [leipzig.melody :refer :all]
            [leipzig.scale :as scale]
            [leipzig.live :as live]
            [leipzig.chord :as chord]
            [leipzig.temperament :as temperament]))

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
  (nth (dna) 364645) => "GGGCTCGAGG")

(fact "Lexicons aren't very random."
  (randomness (take 1000 (babel))) => #(< % 1/100)
  (randomness (take 1000 (dna))) => #(< % 1/100))


(defn complexity
  "A hypothetical function that determines the Kolmogorov complexity of any value."
  [string]
  (->> string (map int) (reduce + 0)))

(defn enterprise
  "Calculate the shortest string that is more complicated than itself."
  []
  (let [its-own-source (repl/source-fn 'enterprise)]
    (->> (babel)
         (drop-while (fn [s] (<= (complexity s) (value-length its-own-source))))
         first)))


(defn decompose [n]
  (let [[remainder quotient] ((juxt mod quot) n 10)]
    (if (zero? quotient)
      [remainder]
      (conj (decompose quotient) remainder))))

(defn word
  ([from]
   (->> (range)
        (map (partial + from))
        (mapcat decompose)))
  ([]
   (word 0)))

(fact "The Champernowne word is defined by concatenating the natural numbers base 10."
  (->> (word) (take 16)) => [0 1 2 3 4 5 6 7 8 9 1 0 1 1 1 2])


(definst sing [freq 110 dur 1.0 vol 1.0]
  (-> (sin-osc freq)
      (+ (sin-osc (* 3.01 freq)))
      (+ (* 1/3 (sin-osc (* 2.01 freq))))
      (+ (* 1/8 (sin-osc (* 5.01 freq))))
      (+ (* 1/2 (sin-osc (* 1/2 freq))))
      (rlpf (line:kr 3000 0 1) 1/5)
      (clip2 0.7)
      (* (env-gen (adsr 0.001 0.03 0.9 0.2) (line:kr 1 0 dur) :action FREE))
      (* vol)))

; Arrangement
(defmethod live/play-note :default
  [{hertz :pitch seconds :duration}]
  (when hertz (sing hertz seconds 0.02)))

(defn least [[x & xs :as v]]
  (if (= x (apply min v))
    0
    (inc (least xs))))

(defn synchronise [v]
  (vec (repeat (count v) (apply max v))))

(defn increment [v i n]
  (update-in v [i] (partial + n)))

(defn digit-shift [x n]
  (apply * x (repeat n (biginteger 10))))

(defn construct [a b c d]
  (if (zero? (* a b))
    {:duration (/ c (max d 1))}
    {:duration (/ a b) :pitch (+ (digit-shift c 1) d)}))

(defn decode*
  [state [a b c d & digits :as remaining?]]
  (when remaining?
    (let [voice (least state)
          time (get state voice)
          note (assoc (construct a b c d) :time time)]
      (->> digits
           (decode* (-> state (increment voice (:duration note))))
           lazy-seq
           (cons note)))))

(defn flatten-ratio [duration]
  (let [d (if (ratio? duration) (denominator duration) 1)
        n (* d duration)]
    (+ (digit-shift n 1) d)))

(defn code [[{:keys [duration pitch] :as remaining?} & notes]]
  (if remaining?
    (let [encoding (if pitch
                     (+ (digit-shift (flatten-ratio duration) 2) pitch)
                     (+ (flatten-ratio duration)))]
      (+ (digit-shift encoding (* 4 (count notes))) (code notes)))
    0))

(def row
  (->> (phrase [3/3 3/3 2/3 1/3 3/3]
               [0 0 0 1 2])
       (then
         (phrase [2/3 1/3 2/3 1/3 6/3]
                 [2 1 2 3 4]))
       (then
         (phrase (repeat 1/3)
                 [7 7 7 4 4 4 2 2 2 0 0 0]))
       (then
         (phrase [2/3 1/3 2/3 1/3 6/3]
                 [4 3 2 1 0]))
       (with (->> (phrase [8] [9]) (times 2)))
       (with (->> (phrase [1 1 2] [-7 -3 -7]) (times 4)))
       ;(with (->> (phrase (cycle [2/3 1/3]) [8 8 nil 8 nil 8 nil 7]) (times 4)))
       (wherever :pitch, :pitch (comp scale/A scale/major scale/lower))
       code))

(def blurred-lines
  (let [riff (phrase (repeat 8 1/2) (concat [[nil nil]] (repeat 6 [2 4]) [[nil nil]]))
        accompaniment (->> (times 4 riff)
                           (then (times 3 (wherever :pitch, :pitch (scale/from -3) riff)))
                           (then (phrase [4] [[nil nil]])))
        bass
        (->>
          (phrase [1/2 6/2 1/2 1/2 6/2 1/2 1/2 6/2 1/2 1/2 6/2 1/2]
                  (cycle [-14 nil -14]))
          (then
            (phrase [1/2 6/2 1/2 1/2 6/2 1/2 1/2 6/2 1/2 1/2 1/2 1/2 1/2 1/2 1/2 1/2 1/2]
                    [-17 nil -17 -17 nil -17 -17 nil -17 -10 -3 -11 -4 -12 -5 -13 -14])))
        harmony (phrase [32] [14])
        ]
    (->>
      (with bass accompaniment)
      (wherever :pitch, :pitch (comp scale/A scale/major))
      code)))

(defn decode [channels notes]
  (decode* (vec (repeat channels 0)) notes))

(defn track [start]
  (->>
    (word start)
    (decode 3)
    (wherever :pitch, :pitch temperament/equal)
    (where :time (bpm 120))
    (where :duration (bpm 120))))

(comment

   ; Loop the track, allowing live editing.
  (live/stop)
  (live/play (track row))

  (fx-reverb)
  (fx-chorus)
  (fx-distortion)

  (live/play (track blurred-lines))
  )
