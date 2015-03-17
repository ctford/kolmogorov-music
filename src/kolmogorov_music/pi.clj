(ns kolmogorov-music.pi
  (:import [java.lang Math]))

(defn exp [x n]
  (apply * (repeat n (bigint x))))

(defn term [i]
  (/
   (- (/ 4 (+ (* 8 i) 1))
      (/ 2 (+ (* 8 i) 4))
      (/ 1 (+ (* 8 i) 5))
      (/ 1 (+ (* 8 i) 6)))
   (exp 16 i)))

(defn pi [i]
  (->> (range i)
       (map term)
       (apply +)))
