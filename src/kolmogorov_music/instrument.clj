(ns kolmogorov-music.instrument
  (:require [overtone.live :refer :all]
            [overtone.samples.piano :refer [index-buffer]]))

(definst overchauffeur [freq 110 dur 1.0 vol 1.0]
  (-> (sin-osc freq)
      (+ (* 1/2 (sin-osc (* 3.01 freq))))
      (+ (* 1/3 (sin-osc (* 2.01 freq))))
      (+ (* 1/8 (sin-osc (* 5.01 freq))))
      (+ (* 2 (sin-osc (* 0.5 freq))))
      (clip2 0.4)
      ;(rlpf (line:kr 2000 (* 2/3 freq) dur) 1/2)
      (+ (* 1 (sin-osc freq) (env-gen (perc))))
      (hpf 100)
      (lpf 1000)
      (* (env-gen (adsr 0.01 0.3 0.9 0.2) (line:kr 1 0 dur) :action FREE))
      (* vol)))

(definst piano
  [note 60 duration 10 level 1 rate 1 loop? 0 attack 0 decay 1 sustain 1 release 0.1 curve -4]
  (let  [buf (index:kr (:id index-buffer) note)
         cutoff (line:kr 1 0 duration)
         env (env-gen (adsr attack decay sustain release level curve) cutoff :action FREE)]
    (* env (scaled-play-buf 2 buf :rate rate :level level :loop loop? :action FREE))))

(comment
(fx-reverb)
(fx-distortion)
(fx-chorus)
(fx-bitcrusher)
)
