(ns kolmogorov-music.test.champernowne
  (:require [kolmogorov-music.song :as song]
            [midje.sweet :refer :all]
            [leipzig.melody :as leipzig]))

(fact "code encodes each note of a melody into four digits that determine pitch and duration."
  (->> (leipzig/phrase [2/3 1/3] [0 4]) song/code) => 23001322)

(fact "Multiple parts are multiplexed, based on which part is furthest behind."
  (->> (leipzig/phrase [1 1 1 1] [0 1 2 3])
       (with (leipzig/phrase [2 2] [4 5]))
       song/code) => 212211001101212311111112)

(fact "Rests are encoded with zeros where duration normally is."
  (->> (leipzig/phrase [1 1 1 1] [0 nil 2 nil]) song/code) => 1100001111110011)

(fact "(comp code decode) is an identity for a single part."
  (let [melody (leipzig/phrase [3/3 3/3 2/3 1/3 3/3]
                               [  0   0   0   1   2])]
    (->> melody song/code song/digits (song/decode 1)) => melody))

(fact "(comp code decode) is an identity for two parts."
  (let [melody (leipzig/phrase [3/3 3/3 2/3 1/3 3/3]
                               [  0   0   0   1   2])
        bass (leipzig/phrase [1 1 2] [0 4 0])
        harmony (with melody bass)]
    (->> harmony song/code song/digits (song/decode 2)) => harmony))

(fact "(comp code decode) is an identity for three parts."
  (let [melody (leipzig/phrase [3/3 3/3 2/3 1/3 3/3]
                               [  0   0   0   1   2])
        bass (leipzig/phrase [1 1 2] [0 4 0])
        accompaniment (leipzig/phrase [8] [9])
        harmony (with melody accompaniment bass)]
    (->> harmony song/code song/digits (song/decode 3)) => harmony))
