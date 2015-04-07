(ns kolmogorov-music.song
  (:require [overtone.live :refer :all]
            [leipzig.melody :refer :all]
            [leipzig.scale :as scale]
            [leipzig.live :as live]
            [leipzig.chord :as chord]
            [leipzig.temperament :as temperament]
            [kolmogorov-music.champernowne :as champernowne]))

(def master-volume 0.03)

; Instruments
;(defonce sweep-bus (audio-bus))
;(defonce sweepers (group "Synths connected to the sweep."))
;(defonce writers (group "Pushing onto the bus." :head sweepers))
;(defonce readers (group "Pulling from the bus." :after writers))

;(defsynth random-walk [out-bus 0 freq 0.3]
;  (out:kr out-bus (-> (lf-noise1:kr freq) (+ 1) (/ 2))))

;(defonce sweeping (random-walk writers sweep-bus))

(definst buzz [freq 110 vol 1.0]
  (-> (saw freq)
      (+ (sin-osc (* 0.505 freq)))
      (rlpf (* 3 440) 1/9)
      (* (env-gen (perc 0.01 0.3) :action FREE))
      (* vol master-volume)))

(definst sing [freq 110 dur 1.0 vol 1.0]
  (-> (sin-osc freq)
      (+ (sin-osc (* 3.01 freq)))
      (+ (* 1/4 (sin-osc (* 2.01 freq))))
      (+ (* 1/3 (sin-osc (* 1/2 freq))))
      (rlpf (line:kr 3000 500 dur) 1/5)
      (clip2 0.3)
      (* (env-gen (adsr 0.05 (* dur 1/3) 0.2) (line:kr 1 0 dur) :action FREE))
      (* vol master-volume)))

; Arrangement
(defmethod live/play-note :dux [{hertz :pitch}] (buzz hertz))
(defmethod live/play-note :comes [{hertz :pitch seconds :duration}] (sing hertz seconds))

(defn chunked-by [n s]
  (let [ch (take n s)]
    (when (= n (count ch))
      (cons
        (take n s)
        (lazy-seq (chunked-by n (drop n s)))))))

(defn split-by [n s]
  (when (seq s)
    (cons
      (take-while (partial not= n) s)
      (lazy-seq (->> s (drop-while #(not= % n)) rest (split-by n))))))

(defn encode [[duration pitch & others]]
  (when others
    (let [duration (/ (or duration 1) 16)
          pitch (or pitch 0) ]
    (cons {:time 0
           :duration duration 
           :pitch pitch}
          (lazy-seq (->> others encode (where :time (scale/from duration))))))))

(defn evens [xs]
  (->> xs (chunked-by 2) (map first)))

(defn odds [xs]
  (->> xs (chunked-by 2) (map second)))

(defn arrange [strings]
  (map
    #(with
       (->> % evens encode (all :part :dux))
       (->> % odds encode (all :part :comes)))
    strings))

(defn end [notes]
  (let [{:keys [duration time]} (or (last notes) {:time 0 :duration 0})]
    (+ duration time)))

(defn sequentially [[a & bs]]
  (when a
    (concat a
            (lazy-seq (->> bs sequentially (where :time (scale/from (end a))))))))

(def track
  (->>
    (champernowne/word 16 (rand-int 99999))
    (split-by 15)
    arrange
    sequentially
    (where :pitch (comp temperament/equal scale/A scale/minor))
    (where :pitch scale/lower)
    (where :time (bpm 160))
    (where :duration (bpm 160))))

(comment
            
  ; Loop the track, allowing live editing.
  (live/jam (var track))
  (fx-reverb)
  (fx-chorus)
  (fx-distortion))
