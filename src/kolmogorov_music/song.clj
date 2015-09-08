(ns kolmogorov-music.song
  (:require [overtone.live :refer :all]
            [leipzig.melody :refer :all]
            [leipzig.scale :as scale]
            [leipzig.live :as live]
            [leipzig.chord :as chord]
            [leipzig.temperament :as temperament]
            [kolmogorov-music.champernowne :as champernowne]))

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
    (champernowne/word start)
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
