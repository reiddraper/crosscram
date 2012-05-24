(ns crosscram.test.game
  (:use clojure.test
        crosscram.game))

(deftest board-manip
  (let [empty (board [2 3])
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
