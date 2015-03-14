(defproject kolmogorov-music "0.1.0-SNAPSHOT"
  :main ^{:skip-aot true} kolmogorov-music.song
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [overtone "0.9.1"]
                 [leipzig "0.8.1"]]
  :profiles {:dev
             {:plugins [[lein-midje "3.1.3"]
                        [codox "0.8.8"]]
              :dependencies [[midje "1.6.3"]]}})
