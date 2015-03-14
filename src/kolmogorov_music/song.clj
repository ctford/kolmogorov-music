(ns kolmogorov-music.song
  (:require [overtone.live :refer :all]
            [leipzig.melody :refer :all]
            [leipzig.scale :as scale]
            [leipzig.live :as live]
            [leipzig.chord :as chord]
            [leipzig.temperament :as temperament]))

; Instruments
(definst bass [freq 110 volume 1.0]
  (-> (saw freq)
      (* (env-gen (perc 0.1 0.4) :action FREE))
      (* volume)))

(definst organ [freq 440 dur 1 volume 1.0]
  (-> (square freq)
      (* (env-gen (adsr 0.01 0.8 0.1) (line:kr 1 0 dur) :action FREE))
      (* 1/4 volume)))

; Arrangement
(defmethod live/play-note :bass [{hertz :pitch}] (bass hertz))
(defmethod live/play-note :accompaniment [{hertz :pitch seconds :duration}] (organ hertz seconds))

; Composition
(def progression [0 0 3 0 4 0])

(defn bassline [root]
  (->> (phrase (cycle [1 1/2 1/2 1 1]) [0 -3 -1 0 2 0 2 3 2 0])
       (where :pitch (scale/from root))
       (where :pitch (comp scale/lower scale/lower))
       (where :part (is :bass))))

(defn accompaniment [root]
  (->>
    (phrase [8] [(-> chord/seventh (chord/root root))])
    (where :part (is :accompaniment))))

; Track
(def track
  (->>
    (mapthen bassline progression)
    (with (mapthen accompaniment progression))
    (where :pitch (comp temperament/equal scale/A scale/minor))
    (where :time (bpm 90))
    (where :duration (bpm 90))))

(defn -main []
  (live/play track))

(comment
  ; Loop the track, allowing live editing.
  (live/jam (var track))
)
