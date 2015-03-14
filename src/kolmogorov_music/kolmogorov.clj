(ns kolmogorov-music.kolmogorov)

(defmacro complexity [sym]
  `(-> '~sym clojure.repl/source-fn count))

(defmacro lift [op [a b]]
  `(~op (complexity ~a) (complexity ~b)))

(defmacro difference [sym-a sym-b]
  `(lift - [~sym-a ~sym-b]))

(defmacro sum [sym-a sym-b]
  `(lift + [~sym-a ~sym-b]))

(defmacro less-than [sym-a sym-b]
  `(lift < [~sym-a ~sym-b]))

(difference distance distance)
(sum distance distance)

(difference distance difference)
(less-than distance difference)
(less-than difference distance)
