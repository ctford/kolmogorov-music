(ns kolmogorov-music.song
  (:require [overtone.live :refer :all]
            [leipzig.melody :refer :all]
            [leipzig.scale :as scale]
            [leipzig.live :as live]
            [leipzig.chord :as chord]
            [leipzig.temperament :as temperament]
            [kolmogorov-music.champernowne :as champernowne]))

(def master-volume 0.1)

; Instruments
(definst buzz [freq 110 vol 1.0]
  (-> (saw freq)
      (+ (sin-osc (* 1/2 freq)))
      (rlpf (* 2 440) 1/9)
      (* (env-gen (perc 0.01 0.3) :action FREE))
      (* vol master-volume)))

(definst sing [freq 110 dur 1.0 vol 1.0]
  (-> (sin-osc freq)
      (+ (sin-osc (* 3 freq)))
      (+ (* 1/3 (sin-osc (* 2 freq))))
      (+ (* 1/3 (sin-osc (* 1/2 freq))))
      (rlpf (line:kr 3000 500 dur) 1/3)
      (clip2 0.3)
      (* (env-gen (adsr 0.05 (* dur 1/3) 0.5) (line:kr 1 0 dur) :action FREE))
      (* vol master-volume)))

(definst organ [freq 440 dur 1 vol 1.0]
  (-> (square freq)
      (+ (sin-osc (* 1/2 freq)))
      (* (sin-osc 3))
      (lpf 3000)
      (* (env-gen (perc (* 1/4 dur) (* 3/4 dur)) :action FREE))
      (* 1/3 vol master-volume)))

(defn evens [[x y & xs]]
  (cons x (lazy-seq (evens xs))))

(defn odds [[x y & xs]]
  (cons y (lazy-seq (odds xs))))

(defn split [xs]
  [(evens xs) (odds xs)])

(comment
(fx-reverb)  
(fx-chorus)
  )

; Arrangement
(defmethod live/play-note :bass [{hertz :pitch}] (buzz hertz))
(defmethod live/play-note :melody [{hertz :pitch seconds :duration}] (sing hertz seconds))
(defmethod live/play-note :accompaniment [{hertz :pitch seconds :duration}] (organ hertz seconds))

; Composition
(def melody
  (let [[durations pitches] (split (champernowne/word 7))]
    (->> 
      (phrase (map #([4 2 1 1/2 1/4] (mod % 5)) durations) pitches)
       (where :part (is :melody)))))

(def chords
  (->> (champernowne/word 7)
       (map #(-> chord/seventh (chord/root %)))))

(def accompaniment
  (->> chords
       (phrase (repeat 4)) 
       (where :part (is :accompaniment))))

(def bass
  (->> chords
       (mapcat vals)
       (phrase (cycle [2 1 1])) 
       (where :pitch (comp scale/lower scale/lower))
       (where :part (is :bass))))

; Track
(def track
  (->>
    melody
    (with bass)
    (with accompaniment)
    (where :pitch (comp temperament/equal scale/A scale/minor))
    (where :time (bpm 100))
    (where :duration (bpm 100))))

(comment
  ; Loop the track, allowing live editing.
  (live/jam (var track))
)

(comment
  two voices?
  difference ranges?
  difference instruments?
  difference pans?

  rest
  change mode 
  change pitch
  change duration
  sharpen
  )
