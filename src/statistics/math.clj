(ns statistics.math)

(defn ceil [n]
  (Math/ceil n))

(defn log2 [n]
  (/ (Math/log n) (Math/log 2)))

(defn minmax [coll]
  [(apply min coll) (apply max coll)])

(defn square [x]
  (* x x))

(defn pow [x y]
  (reduce * (repeat y x)))

(defn sqrt [x]
  (Math/sqrt x))