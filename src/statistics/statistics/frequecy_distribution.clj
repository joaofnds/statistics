(ns statistics.statistics.frequecy-distribution
  (:require [statistics.math :refer [ceil sqrt log2 minmax]]))

(defn sturges [n]
  (int (ceil (inc (log2 n)))))

(defn number-of-classes [n method]
  (case method
    :sqrt (int (ceil (sqrt n)))
    :sturges (int (ceil (sturges n)))))

(defn frequency-distribution [coll bins-method]
  (let [coll (sort coll)
        n (count coll)                      ;; collection size
        k (number-of-classes n bins-method) ;; number of classes
        [li ls] (minmax coll)               ;; limit inferior, limit superior
        iqr (/ (- ls li) k)]                ;; interquantile range
    (map
     (fn [j]
       (let [min (+ li (* j iqr))
             max (+ li (* (inc j) iqr))
             max-comp (if (= (inc j) k) <= <) ;; last class must include max
             numbers (filter #(and (<= min %) (max-comp % max)) coll)
             F (count numbers)
             F-prime (count (filter #(max-comp % max) coll))
             f (float (/ F n))
             f-prime (float (/ F-prime n))
             c (float (/ (+ min max) 2))]
         {:j (inc j) :min min :max max :F F :F-prime F-prime :f f :f-prime f-prime :c c}))
     (range k))))
