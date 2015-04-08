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

(defn n [time duration pitch part]
  {:time time 
   :pitch pitch
   :duration duration
   :part part})

(defn encode [{:keys [dux comes] :as state} [a b c d & digits]]
  (if (zero? (* a b))
    (encode {:dux (max dux comes) :comes (max dux comes)} digits)
    (let [part (if (<= dux comes) :dux :comes)
          duration (/ a b)
          pitch (+ c d)
          time (part state)]
      (cons (n time duration pitch part)
            (lazy-seq (->> digits
                           (encode (update-in state [part] (partial + duration)))))))))

(def track
  (->>
    (champernowne/word 10 410033073307230713083309)
    (encode {:dux 0 :comes 0}) 
    (wherever :pitch, :pitch (comp temperament/equal scale/A scale/minor scale/lower))
    (where :time (bpm 120))
    (where :duration (bpm 120))))

(comment
            
  ; Loop the track, allowing live editing.
  (live/jam (var track))
  (fx-reverb)
  (fx-chorus)
  (fx-distortion))
