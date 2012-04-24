(ns crosscram.core
  (:require [crosscram.board :as board]
            [clojure.core.match :as match]))


(defn opposite [player]
  (match/match player
               :horizontal :vertical
               :vertical :horizontal))

(defn over? [game]
  (match/match (:next-player game)
               :horizontal (not (board/can-play-horizontal? (:board game)))
               :vertical  (not (board/can-play-vertical? (:board game)))))

(defn winner [game]
  (when (over? game)
    (:player (last (:history game)))))

(defn play-piece [game pos-a pos-b]
  (cond
    ;; is the game already over?
    (over? game) (throw (Exception. "The game is already over"))

    ;; are the two points valid for this player?
    (not (match/match (:next-player game)
                      :horizontal (board/horizontal-pair? pos-a pos-b)
                      :vertical (board/vertical-pair? pos-a pos-b)))
    (throw (Exception. (str "Not a valid vertical or horizontal shape: " pos-a pos-b)))

    ;; is someone trying to play on a spot that is already
    ;; occupied?
    (not (board/location-empty? (:board game) pos-a pos-b)) (throw (Exception.
                                                                     "Can't move here, it's occupied"))

    ;; ok, play the piece!
    :else
    (-> game
      (assoc :board (board/add-piece (:board game) (inc (count (:history game))) pos-a pos-b))
      (update-in [:history] #(conj % {:player (:next-player game) :move [pos-a pos-b]}))
      (assoc :next-player (opposite (:next-player game))))))

(defn new-game [rows columns start-player]
  {:board (board/board rows columns)
   :rows rows
   :columns columns
   :next-player start-player
   :history []})

(defn play [game bot-a bot-b]
  (loop [g game
         bot-funs (cycle [bot-a bot-b])]
    (if (over? g)
      g
      (let [new-game (apply play-piece g ((first bot-funs) g))]
        (recur new-game (rest bot-funs))))))

(defn score [game1 game2]
  (match/match [game1 game2]
    [:horizontal :vertical ] {:bot-a 1 :bot-b 0 :draws 0}
    [:vertical :horizontal ] {:bot-a 0 :bot-b 1 :draws 0}
    [:horizontal :horizontal ] {:bot-a 0 :bot-b 0 :draws 1}
    [:vertical :vertical ] {:bot-a 0 :bot-b 0 :draws 1}
    ;; TODO: not sure why,
    ;; by eliminating the two lines
    ;; above this comment
    ;; and replacing them with the
    ;; line below throws an exception:
    ;; java.lang.RuntimeException: java.lang.Exception: No match found.
    ;; Followed 1 branches. Breadcrumbs:
    ;;
    ;;[_ _] (recur (dec to-go))
    ))

(defn play-symmetric [game bot-a bot-b scoreboard games-to-play]
  (if (= 0 games-to-play)
    scoreboard
    (let [g1 (winner (play game bot-a bot-b))
          g2 (winner (play game bot-b bot-a))]
      (let [newboard (merge-with + scoreboard (score g1 g2))]
        (play-symmetric game bot-a bot-b newboard (dec games-to-play))))))
