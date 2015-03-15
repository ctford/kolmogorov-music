(ns kolmogorov-music.champernowne)

(defn decompose [n]
  (let [[remainder quotient] ((juxt mod quot) n 10)]
    (if (pos? quotient)
      (conj (decompose quotient) remainder)
      [remainder])))

(def constant
  (->> (range)
       (mapcat decompose)))
