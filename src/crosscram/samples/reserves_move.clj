(ns crosscram.samples.reserves-move
  "Play any move that guantees you can extra
  move later"
  (:require [crosscram.board :as board]))

(defn valid-empty-pair?
  [game [a b]]
  (and (board/valid-empty-space? (:board game) a)
       (board/valid-empty-space? (:board game) b)))

(defn above-saves?
  [game [[r1 c1] [r2 c2]]]
  (let [above-row (- r1 1)
        above-above-row (- r1 2)]
    (and (valid-empty-pair? game [[above-row c1] [above-row c2]])
         (not (valid-empty-pair?
                game [[above-above-row c1] [above-above-row c2]])))))

(defn below-saves?
  [game [[r1 c1] [r2 c2]]]
  (let [below-row (+ r1 1)
        below-below-row (+ r1 2)]
    (and (valid-empty-pair? game [[below-row c1] [below-row c2]])
         (not (valid-empty-pair?
                game [[below-below-row c1] [below-below-row c2]])))))

(defn reserves-move?
  "Return true if this
  (horizontal) move reserves an extra
  guaranteed move later in the game


  Here is an example where the 'x' move
  saves us the 'y' move for the future.

  [1][1][ ][ ]
  [2][3][3][ ]
  [2][ ][x][x]
  [ ][ ][y][y]

  A move can reserve us a spot
  either 'above' or 'below'.
  If either of those spots are
  valid, empty, and either
  on the edge of the board, or
  bordered with an occupied
  spot, this move will save us
  a spot in the future
  "
  [game move]

  (or (above-saves? game move)
      (below-saves? game move)))
(defn make-move [game]
  (or (first (filter (partial reserves-move? game) (board/available-moves game)))
      (first (board/available-moves game))))
