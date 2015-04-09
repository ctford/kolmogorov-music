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

(defn most-behind [[x & xs :as v]]
  (if (= x (apply min v))
    0
    (inc (most-behind xs))))

(defn synchronise [v]
  (vec (repeat (count v) (apply max v))))

(defn decode*
  ([state [a b c d & digits]]
   (when (or a b c d)
     (if (zero? (* a b))
       (decode* (synchronise state) digits)
       (let [part (most-behind state)
             duration (/ a b)
             pitch (+ c d)
             time (get state part)]
         (cons (construct time duration pitch)
               (lazy-seq (->> digits
                              (decode* (update-in state [part] (partial + duration)))))))))))

(defn tens [n]
  (apply * (repeat n (bigint 10))))

(defn code [[{:keys [duration pitch] :as note} & notes]]
  (if (nil? note)
    (bigint 0)
    (let [one (+ (* (tens 3) (if (ratio? duration) (numerator duration) duration))
                 (* (tens 2) (if (ratio? duration) (denominator duration) 1))
                 (* (tens 1) (quot pitch 2))
                 (* (tens 0) (- pitch (quot pitch 2))))]
      (+ (* (bigint one) (tens (* (bigint 4) (count notes)))) (code notes)))))

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
