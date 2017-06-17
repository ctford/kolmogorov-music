(ns kolmogorov-music.geb
  (:require [overtone.live :refer :all :exclude [stop]]
            [leipzig.melody :refer :all]
            [leipzig.canon :refer [canon interval]]
            [leipzig.scale :refer [lower B minor from]]
            [leipzig.live :as live]
            [leipzig.live :refer [stop]]
            [kolmogorov-music.coding :as coding]))

(def geb
  (let [theme (->> "GEB"
                   (map coding/char->ascii)
                   (phrase [4 4 8])
                   (all :theme true))
        ascii #(where :pitch coding/ascii->midi %)
        double-canon (->> theme
                          (canon ascii))
        sample #(->> %
                     (where :pitch char)
                     (all :part :sample))
        triple-canon (->> theme
                          (canon #(with (sample %) (ascii %))))
        robot (->> triple-canon
                   (filter #(-> % :part (= :sample))))
        bass (->> (phrase [4 4 8] [-2 -1 0])
                  (canon (interval -7))
                  (where :pitch (comp lower lower)))
        alt-bass (->> (phrase (repeat 4 4) (cycle [3 0]))
                      (canon (interval -7))
                      (where :pitch (comp lower lower)))
        riff (->> [-2 -1 0 0]
                  (mapthen #(->> (phrase (repeat 7 1/2)
                                         (interleave [[0 2] [0 2] [0 3] [0 2]] (repeat -3)))
                                 (where :pitch (from %))))
                  (then (phrase (repeat 4 1/2) (interleave [[0 3] [0 2]] (repeat -3)))))
        twiddle (with
                  (phrase (repeat 32 1/2) (cycle [4 2 2 0 -1]))
                  (phrase (repeat 64 1/4) (cycle [4 2 5 4 5 4 7 7])))
        decoration (phrase (repeat 64 1/4) (cycle [7 8 9 11 7 6]))]
    (->> (with bass double-canon)
         ;(with alt-bass twiddle decoration)
         ;(with riff)
         ;robot
         (times 2)
         (wherever (comp not :theme) :pitch (comp B minor))
         (tempo (bpm 90)))))

(comment
  (map fx-chorus [0 1])
  (map fx-distortion [0 1] [2 2] [0.18 0.14])
  (volume 0.8)
  (live/jam (var geb))
  (def geb nil)

  (do (recording-start "geb.wav")
      (live/play geb))
  (recording-stop))

; Instrumentation
(definst overchauffeur [freq 110 dur 1.0 top 1500 vol 0.25]
  (-> (sin-osc freq)
      (+ (* 1/3 (sin-osc (* 2.01 freq))))
      (+ (* 1/2 (sin-osc (* 3.01 freq))))
      (+ (* 1/8 (sin-osc (* 5.01 freq))))
      (+ (* 2 (sin-osc (* 0.5 freq))))
      (clip2 0.7)
      (lpf top)
      (hpf 20)
      (* (env-gen (adsr 0.01 0.2 0.8 0.2) (line:kr 1 0 dur) :action FREE))
      (* vol)))

(defmethod live/play-note :default
  [{midi :pitch seconds :duration}]
  (some-> midi midi->hz (overchauffeur seconds 1500)))

(comment

(def godel (sample "samples/godel.wav"))
(def escher (sample "samples/escher.wav"))
(def bach (sample "samples/bach.wav"))

(defn book [initial]
  (({\G godel
     \E escher
     \B bach}
    initial)))

(defmethod live/play-note :sample
  [{initial :pitch}]
  (book initial))
)
