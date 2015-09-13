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
  (letfn [(sym [s] (symbol "kolmogorov-music.geb" s))]
    (->> "GEB"
      (map (fn [c] [(int c) (-> c char str sym find-var deref)]))
      (phrase [4 4 8])
      (where :time (bpm 90))
      (where :duration (bpm 90)))))

; Arrangement
(defmethod live/play-note :default
  [{hertz :pitch seconds :duration}]
  (when hertz (instrument/overchauffeur (midi->hz hertz) seconds 0.02)))

(comment
  (live/stop)
  (fx-chorus)
  (fx-reverb)
  (fx-distortion)
  (live/jam (var geb))
  )
