(ns kolmogorov-music.champernowne)

(defn decompose [n base]
  (let [[remainder quotient] ((juxt mod quot) n base)]
    (if (pos? quotient)
      (conj (decompose quotient base) remainder)
      [remainder])))

(defn constant
  ([base]
   (->> (range)
        (mapcat #(decompose % base))))
  ([]
   (constant 10)))
