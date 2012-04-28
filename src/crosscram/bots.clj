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

(defn- valid-empty-space? [board [r c]]
  (cond
    (not (< -1 r (count board))) false
    (not (< -1 c (count (nth board r)))) false
    :else (nil? (nth (nth board r) c))))

(defn- num-neighbors [[r c] board direction]
  (+
    (if-not (valid-empty-space? board (if (= direction :horizontal) [r (inc c)] [(inc r) c]))
      1 0)
    (if-not (valid-empty-space? board (if (= direction :horizontal) [r (dec c)] [(dec r) c]))
      1 0)))

(defn- distance-from-opponent [[pos-a pos-b] game]
  (+ (num-neighbors pos-a (:board game) (:next-player game))
    (num-neighbors pos-b (:board game) (:next-player game))))

(defn- score-for-avoiding-crowded-spaces [[r c] board direction]
  (let [pos (if (= direction :horizontal) r c)
        arr (if (= direction :horizontal) (map (fn [row] (get row c)) board) (get board r))]
  (+
    (cond
      (= (+ pos 1) (count arr)) 0
      (not (nil? (nth arr (+ pos 1)))) 0
      (= (+ pos 2) (count arr)) 2
      (not (nil? (nth arr (+ pos 2)))) 2
      :else 1)
    (cond
      (= (- pos 1) -1) 0
      (not (nil? (nth arr (- pos 1)))) 0
      (= (- pos 2) -1) 2
      (not (nil? (nth arr (- pos 2)))) 2
      :else 1))))

(defn- distance-from-self [[pos-a pos-b] game]
  (+ (score-for-avoiding-crowded-spaces pos-a (:board game) (:next-player game))
    (score-for-avoiding-crowded-spaces pos-b (:board game) (:next-player game))))

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
