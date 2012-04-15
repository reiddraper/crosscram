(ns crosscram.bots
  (:require [crosscram.core :as crosscram])
  (:require [clojure.core.match :as match])
  (:require [clojure.set :as set])
  (:require clojure.pprint)
  )

(defn brute-force-moves [game]
  (let [rows (:rows game)
        columns (:columns game)]
    (loop [moves (match/match (:next-player game)
                              :horizontal (crosscram/generate-horizontal rows columns)
                              :vertical (crosscram/generate-vertical rows columns))]

      (if (apply crosscram/location-empty? (:board game) (first moves))
        (first moves)
        (recur (rest moves))))))

(defn random-moves [game]
  (let [rows (:rows game)
        columns (:columns game)]
    (loop [moves (match/match (:next-player game)
                              :horizontal (crosscram/generate-horizontal rows columns)
                              :vertical (crosscram/generate-vertical rows columns))]
      (let [m (set/difference (set moves) (set (map :move (:history game))))
            random-move (rand-nth (vec m))]
        (if (apply crosscram/location-empty? (:board game) random-move)
          random-move
          (recur (remove #{random-move} m)))))))
