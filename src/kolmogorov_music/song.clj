(ns kolmogorov-music.song
  (:require [overtone.live :refer :all]
            [leipzig.melody :refer :all]
            [leipzig.scale :as scale]
            [leipzig.live :as live]
            [leipzig.chord :as chord]
            [leipzig.temperament :as temperament]
            [kolmogorov-music.champernowne :as champernowne]))

(defn digits [n]
  (champernowne/decompose n 10))

(definst sing [freq 110 dur 1.0 vol 1.0]
  (-> (sin-osc freq)
      (+ (sin-osc (* 3.01 freq)))
      (+ (* 1/4 (sin-osc (* 2.05 freq))))
      (+ (* 1/3 (sin-osc (* 1/2 freq))))
      (rlpf (line:kr 5000 0 1.0) 1/50)
      (clip2 0.5)
      (* (env-gen (adsr 0.05 (* dur 1/3) 0.2) (line:kr 1 0 dur) :action FREE))
      (* vol)))

; Arrangement
(defmethod live/play-note :default
  [{hertz :pitch seconds :duration}]
  (when hertz (sing hertz seconds 0.05)))

(defn construct [time duration pitch]
  {:time time 
   :pitch pitch
   :duration duration})

(defn least [[x & xs :as v]]
  (if (= x (apply min v))
    0
    (inc (least xs))))

(defn synchronise [v]
  (vec (repeat (count v) (apply max v))))

(defn increment [v i n]
  (update-in v [i] (partial + n)))

(defn decode*
  ([state [a b c d & digits :as remaining?]]
   (when remaining?
     (let [index (least state)
           rest? (zero? (* a b))
           pitch (when-not rest? (+ c d))
           duration (if (not rest?) (/ a b) (/ c d))
           time (get state index)]
       (cons (construct time duration pitch)
             (lazy-seq (->> digits
                            (decode* (increment state index duration)))))))))

(defn digit-shift [x n]
  (apply * x (repeat n (bigint 10))))

(defn code-pitch [pitch]
  (let [half (quot pitch 2)]
    (+ (digit-shift half 1) (- pitch half))))

(defn code-duration [duration]
  (let [denom (if (ratio? duration) (denominator duration) 1)
        numer (* denom duration)]
    (+ (digit-shift numer 1) denom)))

(defn code [[{:keys [duration pitch] :as remaining?} & notes]]
  (if remaining?
    (let [encoding (+ (digit-shift (code-duration duration) 2)
                      (code-pitch pitch)) ]
      (+ (digit-shift encoding (* 4 (count notes))) (code notes)))
    0))

(def row
  (->> (phrase [3/3 3/3 2/3 1/3 3/3]
               [7 7 7 8 9])
       (then
         (phrase [2/3 1/3 2/3 1/3 6/3]
                 [9 8 9 10 11]))
       (then
         (phrase (repeat 1/3)
                 [14 14 14 11 11 11 9 9 9 7 7 7]))
       (then
         (phrase [2/3 1/3 2/3 1/3 6/3]
                 [11 10 9 8 7]))
       (with (->> (phrase [8] [16]) (times 2)))
       (with (->> (phrase [1 1 2] [0 4 0]) (times 4)))
       code))

(defn decode [channels notes]
  (decode* (vec (repeat channels 0)) notes))

(defn track []
  (->>
    (champernowne/word 10 row)
    (decode 3)
    (wherever :pitch, :pitch (comp temperament/equal scale/A scale/minor scale/lower))
    (where :time (bpm 120))
    (where :duration (bpm 120))))

(comment
            
   ; Loop the track, allowing live editing.
  (live/play (track))
  (fx-reverb)
  (fx-chorus)
  (fx-distortion)
  
  )
