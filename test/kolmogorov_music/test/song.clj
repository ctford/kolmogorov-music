(ns kolmogorov-music.test.champernowne
  (:require [kolmogorov-music.song :as song]
            [midje.sweet :refer :all]
            [leipzig.melody :as leipzig]))

(fact "(comp code decode) is an identity for a single part."
  (let [melody (leipzig/phrase [3/3 3/3 2/3 1/3 3/3]
                               [  0   0   0   1   2])]
    (->> melody song/code song/digits (song/decode [0])) => melody))

(fact "(comp code decode) is an identity for two parts."
  (let [melody (leipzig/phrase [3/3 3/3 2/3 1/3 3/3]
                               [  0   0   0   1   2])
        bass (leipzig/phrase [1 1 2] [0 4 0])
        harmony (with melody bass)]
    (->> harmony song/code song/digits (song/decode [0 0])) => harmony))

(fact "(comp code decode) is an identity for three parts."
  (let [melody (leipzig/phrase [3/3 3/3 2/3 1/3 3/3]
                               [  0   0   0   1   2])
        bass (leipzig/phrase [1 1 2] [0 4 0])
        accompaniment (leipzig/phrase [8] [9])
        harmony (with melody accompaniment bass)]
    (->> harmony song/code song/digits (song/decode [0 0 0])) => harmony))
