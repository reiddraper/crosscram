(ns crosscram.bots
  (:require [crosscram.core :as crosscram])
  (:require [crosscram.board :as board])
  (:require [clojure.core.match :as match])
  (:require [clojure.set :as set])
  (:require clojure.pprint)
  )

(defn brute-force-moves [game]
  (let [rows (:rows game)
        columns (:columns game)]
    (loop [moves (match/match (:next-player game)
                              :horizontal (board/generate-horizontal rows columns)
                              :vertical (board/generate-vertical rows columns))]

      (if (apply board/location-empty? (:board game) (first moves))
        (first moves)
        (recur (rest moves))))))

(defn random-moves [game]
  (let [rows (:rows game)
        columns (:columns game)]
    (loop [moves (match/match (:next-player game)
                              :horizontal (board/generate-horizontal rows columns)
                              :vertical (board/generate-vertical rows columns))]
      (let [m (set/difference (set moves) (set (map :move (:history game))))
            random-move (rand-nth (vec m))]
        (if (apply board/location-empty? (:board game) random-move)
          random-move
          (recur (remove #{random-move} m)))))))


(defn -main [& args]
  (println (crosscram.core/play-symmetric
    (apply crosscram.core/new-game args)
    brute-force-moves
    random-moves
    {:bot-a-wins 0 :bot-b-wins 0 :draws 1}
    10)))
