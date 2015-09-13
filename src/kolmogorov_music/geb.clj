(ns kolmogorov-music.geb
  (:require [overtone.live :refer :all :exclude [stop]]
            [leipzig.melody :refer :all]
            [leipzig.canon :as canon]
            [leipzig.scale :as scale]
            [leipzig.live :as live]
            [leipzig.live :refer [stop]]
            [leipzig.chord :as chord]
            [leipzig.temperament :as temperament]
            [kolmogorov-music.instrument :as instrument]))

(defmacro defs [names values]
  `(do
     ~@(map
         (fn [name value] `(def ~name ~value))
         names (eval values))))

(defs [C D E F G A B]
  (map
    (comp scale/C scale/major)
    (range)))


(def geb
  (let [theme (->> "GEB"
                   (map (fn [c] [(int c) (->> c str (symbol "kolmogorov-music.geb") find-var deref)]))
                   (phrase [4 4 8]))
        chords (map #(phrase (repeat 1/2) (juxt %) )     [-2 -1 0])
        ]
    (->> (phrase [4 4 8] [-2 -1 0]) 
         (where :pitch (comp scale/lower scale/lower))
         (where :pitch (comp scale/B scale/minor))
         (where :part (is :accompaniment))
         (with theme)
         (where :time (bpm 90))
         (where :duration (bpm 90)))))

; Arrangement
(defmethod live/play-note :default
  [{midi :pitch seconds :duration}]
  (some-> midi midi->hz (instrument/overchauffeur seconds 0.02)))

(defmethod live/play-note :accompaniment
  [{midi :pitch seconds :duration}]
  (some-> midi (instrument/piano seconds)))

(comment
  (live/stop)
  (fx-chorus)
  (fx-reverb)
  (fx-distortion)
  (live/jam (var geb))
  )
