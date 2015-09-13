(ns kolmogorov-music.blurred-lines
  (:require [leipzig.melody :refer :all]
            [leipzig.scale :as scale]
            [leipzig.chord :as chord]
            [leipzig.temperament :as temperament]
            [kolmogorov-music.coding :as coding]))

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
        harmony (phrase [32] [14])]
    (->>
      (with bass accompaniment)
      (wherever :pitch, :pitch (comp scale/A scale/major))
      coding/encode)))
