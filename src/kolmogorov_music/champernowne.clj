(ns kolmogorov-music.champernowne)

(defn decompose [n]
  (let [remainder (mod n 10)
        quotient (quot n 10)]
    (if (pos? quotient)
      (conj (decompose quotient) remainder)
      [remainder])))

(def constant
  (->> (range)
       (mapcat decompose)))
