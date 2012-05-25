(ns crosscram.test.game
  (:use clojure.test
        crosscram.game))

(deftest dominoes
  ;; validity
  (are [p e] (= (valid-domino? p) (boolean e))
       [[0 0] [1 0]] true
       [[2 9] [1 9]] true
       [[0 0] [0 1]] true
       ;; positional
       [[5 6] [5 6]] false
       [[1 0] [0 1]] false
       ;; values
       [[2 -1] [1 -1]] false
       [[0 0] [0 1.0]] false
       [[0 0] [nil 1]] false
       ;; dim
       [[0 0] [0 1] [0 2]] false
       [[0 0 1] [0 1 1]] false
       ;; coll type
       (list [0 0] [0 1]) false
       '[(0 1) [0 2]] false)
  ;; squares "abstraction"
  (is (= (domino-squares [[0 1] [1 1]]) [[0 1] [1 1]]))
  ;; orientation
  (is (horizontal? [[5 9] [5 10]]))
  (is (horizontal? [[5 10] [5 9]]))
  (is (not (horizontal? [[9 5] [10 5]]))))

(deftest boards-and-moves
  (let [empty (mk-board [2 3])
        tile [[0 2] [1 2]]
        move0 (place-domino empty tile 0)]
    ;; impl
    (is (= empty [[nil nil nil] [nil nil nil]]))
    (is (= move0 [[nil nil 0] [nil nil 0]]))
    ;; impl - non-vertical
    (is (= (place-domino empty [[0 0] [0 1]] 5)
           [[5 5 nil] [nil nil nil]]))
    ;; api
    (is (= (lookup-square empty [0 2]) nil))
    (is (= (lookup-square move0 [0 2]) 0))
    (is (= (lookup-square move0 [1 2]) 0))
    (is (= (lookup-square move0 [1 1]) nil))))

(deftest inspection
  (is (horizontal-space? (mk-board [3 4])))
  (is (not (horizontal-space? (mk-board [1 1]))))
  (is (horizontal-space? (place-domino (mk-board [2 3]) [[0 0] [1 0]] 0)))
  (is (not (horizontal-space? (place-domino (mk-board [2 3]) [[0 1] [1 1]] 0))))
  (is (horizontal-space? (place-domino (mk-board [2 3]) [[0 2] [1 2]] 0)))
  (is (horizontal-space? (place-domino (mk-board [2 3]) [[0 1] [0 2]] 0))))

(deftest rotations
  (let [d [[1 2] [0 2]]]
    (is (= (rotate-domino d 1) [[2 1] [2 0]]))
    (is (= (rotate-domino d 0) d))
    (is (= (rotate-domino d 1) (rotate-domino d -3)))
    (is (= (rotate-domino d 4) (rotate-domino d 18))))
  (let [p [[3 4] [2 4]]
        b57 (mk-board [5 7])
        move0 (place-domino b57 p 0)]
    (is (= (place-domino b57 p 0)
           (rotate-board (place-domino (rotate-board b57 1)
                                       (rotate-domino p 1) 0)
                         1)))
    (is (= (rotate-board move0 0) move0))
    (is (= (rotate-board move0 1) (rotate-board move0 -3)))
    (is (= (rotate-board move0 4) (rotate-board move0 18)))))

(deftest validity-moves
  (let [single (place-domino (mk-board [4 4]) [[1 1] [2 1]] 0)]
    (are [p e] (= (valid-move? single p) (boolean e))
         [[0 0] [0 1]] true
         [[2 2] [2 3]] true
         ;; overlap
         [[1 1] [1 2]] false
         [[2 2] [2 1]] false
         ;; off board
         [[0 4] [0 3]] false
         [[4 1] [3 1]] false)))

(deftest games
  (is (= (mk-game [2 3] 1)
         {:board (mk-board [3 2]), :dims [3 2], :history [], :player-id 1}))
  (let [game-base (mk-game [2 3] 0)
        move-0 [[1 1] [0 1]]
        game-0 (move game-base move-0)
        game-1 (move game-0 :crosscram.game/out)]
    (is (= (:board game-0) (place-domino (mk-board [2 3]) move-0 0)))
    (is (= (:history game-0) [move-0]))
    (is (= (:board game-1) (:board game-0)))
    (is (= (:history game-1) [move-0 :crosscram.game/out]))))
