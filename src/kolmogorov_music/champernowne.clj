(ns kolmogorov-music.champernowne
  (:import [java.lang Math]
           [java.util Collections]))

(defn decompose [n]
  (let [[remainder quotient] ((juxt mod quot) n 10)]
    (if (zero? quotient)
      [remainder]
      (conj (decompose quotient) remainder))))

(defn word
  ([from]
   (->> (range)
        (map (partial + from))
        (mapcat decompose)))
  ([]
   (word 0)))

(defn rightshift [n distance base]
  (/ n (int (Math/pow base distance))))

(defn expand [digits]
  (->> digits
       (map #(rightshift %2 %1 10) (range))
       (apply +)))

(defn constant
  [precision]
  (->> (word)
       (take precision)
       expand
       double))

(defn prefix? [sub super]
  (= sub (take (count sub) super)))

(defn windows [super]
  (map (fn [i] [i (drop i super)]) (range)))

(defn index [sub]
  (->> (word)
       windows
       (some (fn [[i super]] (when (prefix? sub super) i)))))
