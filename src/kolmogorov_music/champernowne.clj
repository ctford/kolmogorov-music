(ns kolmogorov-music.champernowne
  (:import [java.lang Math]
           [java.util Collections]))

(defn decompose [n base]
  (let [[remainder quotient] ((juxt mod quot) n base)]
    (if (pos? quotient)
      (conj (decompose quotient base) remainder)
      [remainder])))

(defn word
  ([base]
   (->> (range)
        (mapcat #(decompose % base))))
  ([]
   (word 10)))

(defn rightshift [n distance base]
  (/ n (int (Math/pow base distance))))

(defn expand [base digits]
  (->> digits
       (map #(rightshift %2 %1 base) (range))
       (apply +)))

(defn constant
  ([precision base]
   (->> (word base)
        (take precision)
        (expand base)))
  ([precision]
   (constant precision 10)))

(defn prefix? [sub super]
  (= sub (take (count sub) super)))

(defn windows [super]
  (map (fn [i] [i (drop i super)]) (range)))

(defn index [sub base]
  (->> (word base)
       windows
       (some (fn [[i super]] (when (prefix? sub super) i)))))
