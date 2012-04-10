(ns crosscram.two-dim
  (:refer-clojure :exclude [get]))

(defprotocol TwoDim
  (get [this row column])
  (assoc [this row column value]))

(extend-type clojure.lang.PersistentVector
  TwoDim
  (get [this row column]
    (clojure.core/get
      (clojure.core/get
        this row) column))

  (assoc [this row column value]
    (clojure.core/assoc-in this [row colum] value)))
