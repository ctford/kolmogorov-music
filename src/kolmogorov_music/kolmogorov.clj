(ns kolmogorov-music.kolmogorov)

(defn complexity-fn [sym]
  (-> sym clojure.repl/source-fn read-string flatten count))

(defmacro complexity [sym]
  (complexity-fn sym))

(defmacro difference [a b]
  `(- (complexity ~a) (complexity ~b)))
