(ns kolmogorov-music.geb
  (:require [overtone.live :refer :all :exclude [stop]]
            [leipzig.melody :refer :all]
            [leipzig.canon :as canon]
            [leipzig.scale :as scale]
            [leipzig.live :as live]
            [leipzig.live :refer [stop]]
            [leipzig.chord :as chord]
            [leipzig.temperament :as temperament]
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
        arpeggios (->>
                    [-2 -1 0 0]
                    (mapthen
                    #(->> (phrase (repeat 1/2) [[0 2] -3 [0 2] -3 [0 3] -3 [0 2]]) (where :pitch (scale/from %)))
                    )
                    (then (phrase (repeat 1/2) [[0 3] -3 [0 2] -3]))
                    )
        fiddle (phrase (repeat 32 1/2) (cycle [4 2 2 0 -1]))
        fiddle2 (phrase (repeat 64 1/4) (cycle [4 2 5 4 5 4 7 7]))
        bass2 (phrase (repeat 4) [-11 -14 -11 -14])
        ]
    (->> bass
         ;(with fiddle fiddle2)
         ;(with decoration)
         ;(with arpeggios)
         (where :pitch (comp scale/B scale/minor))
         (with theme)
         (where :time (bpm 90))
         (where :duration (bpm 90)))))

(comment
  (live/stop)
  (fx-chorus)
  (fx-reverb)
  (fx-distortion)
  (live/jam (var geb))
  )

; Instrumentation
(definst overchauffeur [freq 110 dur 1.0 vol 1.0]
  (-> (sin-osc freq)
      (+ (* 1/2 (sin-osc (* 3.01 freq))))
      (+ (* 1/3 (sin-osc (* 2.01 freq))))
      (+ (* 1/8 (sin-osc (* 5.01 freq))))
      (+ (* 2 (sin-osc (* 0.5 freq))))
      (clip2 0.7)
      (lpf 1000)
      (hpf 100)
      (* (env-gen (adsr 0.01 0.3 0.9 0.2) (line:kr 1 0 dur) :action FREE))
      (* vol)))

(defmethod live/play-note :default
  [{midi :pitch seconds :duration}]
  (some-> midi midi->hz (overchauffeur seconds)))
