(ns kolmogorov-music.champernowne)

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
