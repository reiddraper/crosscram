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
    (= 1 (abs (- a 0)))
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

(defn board [rows columns]
  (vec (repeat rows
               (vec
                 (take 5 (repeat nil))))))

(defn add-piece)
