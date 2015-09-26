(ns kolmogorov-music.test.song
  (:require [kolmogorov-music.kolmogorov :as song]
            [kolmogorov-music.coding :as coding]
            [midje.sweet :refer :all]
            [leipzig.melody :as leipzig]))

(fact "encode encodes each note of a melody into four digits that determine pitch and duration."
  (->> (leipzig/phrase [2/3 1/3] [0 4]) coding/encode) => 23001304)

(fact "Multiple parts are multiplexed, based on which part is furthest behind."
  (->> (leipzig/phrase [1 1 1 1] [0 1 2 3])
       (leipzig/with (leipzig/phrase [2 2] [4 5]))
       coding/encode) => 210411001101210511021103)

(fact "Rests are encoded with zeros where duration normally is."
  (->> (leipzig/phrase [1 1 1 1] [0 nil 2 nil]) coding/encode) => 1100001111020011)

(fact "(comp code decode-into-parts) is an identity for a single part."
  (let [melody (leipzig/phrase [3/3 3/3 2/3 1/3 3/3]
                               [  0   0   0   1   2])]
    (->> melody coding/encode song/decompose (coding/decode-into-parts 1)) => melody))

(fact "Rests survive the round trip."
  (let [melody (leipzig/phrase [1   1   1]
                               [0   nil 2])]
    (->> melody coding/encode song/decompose (coding/decode-into-parts 1)) => melody))

(fact "(comp code decode-into-parts) is an identity for two parts."
  (let [melody (leipzig/phrase [3/3 3/3 2/3 1/3 3/3]
                               [  0   0   0   1   2])
        bass (leipzig/phrase [1 1 2] [0 4 0])
        harmony (leipzig/with melody bass)]
    (->> harmony coding/encode song/decompose (coding/decode-into-parts 2)) => harmony))

(fact "(comp code decode-into-parts) is an identity for three parts."
  (let [melody (leipzig/phrase [3/3 3/3 2/3 1/3 3/3]
                               [  0   0   0   1   2])
        bass (leipzig/phrase [1 1 2] [0 4 0])
        accompaniment (leipzig/phrase [8] [9])
        harmony (leipzig/with melody accompaniment bass)]
    (->> harmony coding/encode song/decompose (coding/decode-into-parts 3)) => harmony))
