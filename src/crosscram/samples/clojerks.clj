(ns crosscram.samples.clojerks
  "Play any move that guantees you can extra
  move later"
  (:require [crosscram.game :as game]))

(defn evaluator
  [op board]
  (let [avail-moves (game/available-moves board)
        opp-moves (game/available-moves (game/transpose board))]
    (op (count avail-moves) (count opp-moves))))

(defn make-move
  [game]
  (->> game :board
    game/available-moves
    (map (partial game/move game))
    (sort-by (comp (partial evaluator -) :board))
    last
    :history
    last
    ))
