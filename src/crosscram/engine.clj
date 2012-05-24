(ns crosscram.engine
  "Game engine.

A bot is a function of [game -> domino].

The returned domino must be vertical; that is, the row coordinates differ
by 1 but the column coordinates are equal. That is, every player plays
vertical, and sees the other player as playing horizontal. (The game engine
gives the second player a transposed view of the board.)")

;; TODO(timmc:2012-05-23) Decide on tournament rules for bots throwing
;; exceptions, returning nil, returning bad dominoes...
