(ns crosscram.core
  (:use [clojure.core.match :only [match]]))

(defn abs [x]
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
    false
    ))

(defn vertical-pair? [[a b] [c d]] 
  (if (= b d)
    (= 1 (abs (- a c)))
    false
    ))

(defn- two-nil? [coll]
  (= true (reduce (fn [acc elem]
                    (match [acc elem]
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

(defn add-piece)
