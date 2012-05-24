(ns crosscram.game
  "Game knowledge")

;; Implementation details

;; In 2 dimensions:
;; - A board is a 2-level nesting of vectors, addressed by row, then column
;;   - Each cell contains the ordinal of the move, or nil
;; - A square is a vector of [r, c], zero-indexed
;; - A domino is a vector of two squares in order: [[r, c], [(inc r), c]]
;;   - That is, a player always sees their own domino as vertical
;;   - The order of the squares is not currently important to the game engine,
;;     but the history will contain the same ordering the bots provide.
;; - A game is a hash of:
;;   :board - just a board
;;   :dims - [rows columns]
;;   :history - vector of dominoes

;; TODO(timmc:2012-05-23) Should we canonicalize the order of the squares in
;; the domino on receipt from a bot?

