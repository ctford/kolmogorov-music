(ns kolmogorov-music.geb
  (:require [overtone.live :refer :all :exclude [stop]]
            [leipzig.melody :refer :all]
            [leipzig.canon :as canon]
            [leipzig.scale :as scale]
            [leipzig.live :as live]
            [leipzig.live :refer [stop]]
            [leipzig.chord :as chord]
            [leipzig.temperament :as temperament]
            [kolmogorov-music.instrument :as instrument]
            [kolmogorov-music.coding :as coding]))

(def geb
  (let [timing [4 4 8]
        theme (->> "GEB"
                   (map coding/char->ascii)
                   (phrase timing)
                   (canon/canon #(where :pitch coding/ascii->midi %)))
        bass (->> (phrase timing [-2 -1 0])
                  (where :pitch (comp scale/lower scale/lower)))
        decoration (phrase (repeat 64 1/4) (cycle [7 8 9 11 7 6]))
        arpeggios (mapthen #(->> (phrase (repeat 1/2) [0 -3 0 2 1 0 -1 -3]) (where :pitch (scale/from %))) [-2 -1 0 0])
        ]
    (->> bass
   ;      (with decoration)
   ;      (with arpeggios)
         (where :pitch (comp scale/B scale/minor))
         (with theme)
         (where :time (bpm 90))
         (where :duration (bpm 90)))))

; Arrangement
(defmethod live/play-note :default
  [{midi :pitch seconds :duration}]
  (some-> midi midi->hz (instrument/overchauffeur seconds 0.02)))

(comment
  (live/stop)
  (fx-chorus)
  (fx-reverb)
  (fx-distortion)
  (fx-bitcrusher)
  (live/jam (var geb))
  )
