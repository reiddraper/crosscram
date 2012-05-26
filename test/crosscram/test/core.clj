(ns crosscram.test.core
  (:use [crosscram.core])
  (:use [clojure.test]))

(deftest basics
  (is (= (opposite :horizontal) :vertical))
  (is (= (opposite :vertical) :horizontal)))
