(ns crosscram.bots
  (:require [crosscram.core :as crosscram])
  (:require [crosscram.board :as board])
  (:require [clojure.core.match :as match])
  )

;;
;; Some naive bots
;;

(defn brute-force-moves [game]
  (first (board/available-moves game)))

(defn random-moves [game]
  (rand-nth (board/available-moves game)))

;;
;; My own bot
;;

(defn- borders-an-opponent [[a b] board direction]
  (or
    (not (nil? (board/two-d-get board (if (= direction :horizontal) [a (inc b)] [(inc a) b]))))
    (not (nil? (board/two-d-get board (if (= direction :horizontal) [a (dec b)] [(dec a) b]))))))

(defn- distance-from-opponent [piece game]
  (+
    (borders-an-opponent (first piece) (:board game) (:next-player game))
    (borders-an-opponent (second piece) (:board game) (:next-player game))))

(defn- score-for-avoiding-crowded-spaces [[a b] board direction]
  (let [pos (if (= direction :horizontal) a b)
        arr (if (= direction :horizontal) (map (fn [row] (get row b)) board) (get board a))]
  (+
    (cond
      (not (nil? (nth arr (+ pos 1)))) 0
      (not (nil? (nth arr (+ pos 2)))) 2
      :else 1)
    (cond
      (not (nil? (nth arr (- pos 1)))) 0
      (not (nil? (nth arr (- pos 2)))) 2
      :else 1))))

(defn- distance-from-self [piece game]
  (score-for-avoiding-crowded-spaces piece (:board game) (:next-player game)))

(defn- score [piece game]
  (+ (distance-from-self piece game) (distance-from-opponent piece game)))

(defn better-than-random [game]
  (first (reverse (sort-by
    (fn [arg] (score arg game))
    (board/available-moves game)))))

;;
;; Main
;;

(defn -main [rows columns num-games]
  (println (str "Scores: "
    (crosscram/play-symmetric
      (crosscram/new-game (Integer/parseInt rows) (Integer/parseInt columns) :horizontal )
      better-than-random
      random-moves
      (Integer/parseInt num-games)))))
