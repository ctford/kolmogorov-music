(ns kolmogorov-music.instrument
  (:require [overtone.live :refer :all]))

(definst overchauffeur [freq 110 dur 1.0 vol 1.0]
  (-> (sin-osc freq)
      (+ (* 1/2 (sin-osc (* 3.01 freq))))
      (+ (* 1/3 (sin-osc (* 2.01 freq))))
      (+ (* 1/8 (sin-osc (* 5.01 freq))))
      (+ (* 2 (sin-osc (* 0.5 freq))))
      (clip2 0.5)
      (rlpf (line:kr 2000 (* 2/3 freq) dur) 1/9)
      (+ (* 4 (sin-osc freq) (env-gen (perc))))
      (hpf 300)
      (lpf 1000)
      (* (env-gen (adsr 0.01 0.3 0.9 0.2) (line:kr 1 0 dur) :action FREE))
      (* vol)))

(comment
(fx-reverb)
(fx-distortion)
(fx-chorus)
(fx-bitcrusher)
)
