(ns kolmogorov-music.coding
  (:require [leipzig.melody :refer :all]
            [leipzig.scale :as scale]
            [leipzig.live :as live]
            [leipzig.chord :as chord]
            [leipzig.temperament :as temperament]))

(defn least [[x & xs :as v]]
  (if (= x (apply min v))
    0
    (inc (least xs))))

(defn increment [v i n]
  (update-in v [i] (partial + n)))

(defn digit-shift [x n]
  (apply * x (repeat n (biginteger 10))))

(defn construct [a b c d]
  (if (zero? (* a b))
    {:duration (/ c (max d 1))}
    {:duration (/ a b) :pitch (+ (digit-shift c 1) d)}))

(defn decode*
  [state [a b c d & digits :as remaining?]]
  (when remaining?
    (let [voice (least state)
          time (get state voice)
          note (assoc (construct a b c d) :time time)]
      (->> digits
           (decode* (-> state (increment voice (:duration note))))
           lazy-seq
           (cons note)))))

(defn flatten-ratio [duration]
  (let [d (if (ratio? duration) (denominator duration) 1)
        n (* d duration)]
    (+ (digit-shift n 1) d)))

(defn encode [[{:keys [duration pitch] :as remaining?} & notes]]
  (if remaining?
    (let [encoding (if pitch
                     (+ (digit-shift (flatten-ratio duration) 2) pitch)
                     (+ (flatten-ratio duration)))]
      (+ (digit-shift encoding (* 4 (count notes))) (encode notes)))
    0))

(defn decode [channels notes]
  (decode* (vec (repeat channels 0)) notes))

(defmacro defs [names values]
  `(do
     ~@(map
         (fn [name value] `(def ~name ~value))
         names (eval values))))

(def char->ascii int)

(defs [A B C D E F G]
  (map
    (comp scale/A scale/low scale/minor)
    (range)))

(defn ascii->midi [n]
  (->> n
       char
       str
       (symbol "kolmogorov-music.coding")
       find-var
       deref))
