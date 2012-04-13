(ns crosscram.core
  (:require [clojure.core.match :as match]))

(defn- abs [x]
  (if (< x 0)
    (* -1 x)
    x))

(defn valid-pair? [[a b] [c d]] 
  (cond (= a c) (= 1 (abs (- b d)))
        (= b d) (= 1 (abs (- a c)))
        true false))

(defn horizontal-pair? [[a b] [c d]] 
  (if (= a c)
    (= 1 (abs (- b d)))
    false))

(defn vertical-pair? [[a b] [c d]] 
  (if (= b d)
    (= 1 (abs (- a c)))
    false))

(defn- generate-horizontal-for-row [columns row]
  (take (- columns 1)
        (for [x (range columns)]
          [[row x] [row  (+ 1 x)]])))

(defn- generate-vertical-for-column [rows column]
  (take (- rows 1)
        (for [x (range rows)]
          [[x column] [(+ 1 x) column]])))

(defn generate-horizontal [rows columns]
  (apply concat
         (map (partial generate-horizontal-for-row columns)
              (range rows))))

(defn generate-vertical [rows columns]
  (apply concat
         (map (partial generate-vertical-for-column rows)
              (range columns))))

(defn- two-nil? [coll]
  (= true (reduce (fn [acc elem]
                    (match/match [acc elem]
                                 [true _] true
                                 [0 nil] 1
                                 [1 nil] true
                                 [_ _] 0))
                  0 coll)))

(defn can-play-horizontal? [board]
  (boolean
    (some identity (map two-nil? board))))

(defn can-play-vertical? [board]
  (can-play-horizontal? (apply map vector board)))

(defn two-d-get [coll [a b]]
  (get (get coll a) b))

(defn location-empty? [board pos-a pos-b]
  (and (nil? (two-d-get board pos-a))
       (nil? (two-d-get board pos-b))))

(defn board [rows columns]
  (vec (repeat rows
               (vec
                 (take columns (repeat nil))))))

(defn add-piece [board piece a b]
  (-> board
    (assoc-in a piece)
    (assoc-in b piece)))

(defprotocol CrossCramGame
  (over? [this])
  (play-piece [this pos-a pos-b])
  (next-player [this])
  )

(defn opposite [player]
  (match/match player
               :horizontal :vertical
               :vertical :horizontal))

(defrecord Game [board
                 next-player
                 num-plays
                 history]
  CrossCramGame
  (over? [this]
    (match/match next-player
                 :horizontal (not (can-play-horizontal? board))
                 :vertical  (not (can-play-vertical? board))))

  (play-piece [this pos-a pos-b]
    (cond
      ;; is the game already over?
      (over? this) (throw (Exception. "The game is already over"))

      ;; are the two points valid for this player?
      (not (match/match next-player
                        :horizontal (horizontal-pair? pos-a pos-b)
                        :vertical (vertical-pair? pos-a pos-b)))
      (throw (Exception. "Not a valid vertical or horizontal shape"))

      ;; is someone trying to play on a spot that is already
      ;; occupied?
      (not (location-empty? board pos-a pos-b)) (throw (Exception.
                                                         "Can't move here, it's occupied"))

      ;; ok, play the piece!
      true
      (-> this
;;        (assoc :board (add-piece board {:move-number num-plays
;;                                        :location [pos-a pos-b]}
;;                                 pos-a pos-b))
        (assoc :board (add-piece board (inc num-plays) pos-a pos-b))
        (update-in [:num-plays] inc)
        (update-in [:history] #(conj % {:player next-player :move [pos-a pos-b]}))
        (assoc :next-player (opposite next-player)))))

  (next-player [this] next-player))

(defn new-game [rows columns start-player]
  (Game. (board rows columns)
         start-player
         0
         []))
