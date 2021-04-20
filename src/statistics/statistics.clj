(ns statistics.statistics
  (:require [statistics.math :refer [minmax square pow sqrt]]))

(defn coll-range [coll]
  (let [[li ls] (minmax coll)]
    (- ls li)))

(defn mean [coll]
  (if (empty? coll)
    0
    (/ (apply + coll)
       (count coll))))

(defn median [coll]
  (let [sorted (sort coll)
        cnt (count sorted)
        halfway (quot cnt 2)]
    (if (odd? cnt)
      (nth sorted halfway) ; (1)
      (let [bottom (dec halfway)
            bottom-val (nth sorted bottom)
            top-val (nth sorted halfway)]
        (mean [bottom-val top-val])))))

(defn mode [coll]
  (let [freqs (frequencies coll)
        occurrences (group-by val freqs)
        modes (last (sort occurrences))
        modes (->> modes
                   val
                   (map key))]
    modes))

(defn icdf
  "Inverse Cumulative Distribution Function"
  [coll point]
  (let [n (count coll)
        j (int (* n point))
        f (rem (* n point) 1)
        xj (nth coll (dec j)) ;; j-1 due to 0-based index
        xj+1 (nth coll j)]
    (if (zero? f)
      (/ (+ xj xj+1) 2)
      xj+1)))

(defn deviance [coll]
  (let [mean (mean coll)]
    (map #(- % mean) coll)))

(defn variance [coll]
  (/ (apply + (map square (deviance coll)))
     (dec (count coll))))

(defn std-deviation [coll]
  (sqrt (variance coll)))

(defn coefficient-of-variation [coll]
  (let [std-deviation (std-deviation coll)
        mean (mean coll)]
    (* (/ std-deviation mean) 100)))

(defn moment [coll ord]
  {:pre [(pos? ord)]}
  (/ (apply + (map #(pow % ord) (deviance coll)))
     (count coll)))

(defn a3 [coll]
  (let [m2 (moment coll 2)
        m3 (moment coll 3)]
    (/ m3 (* m2 (sqrt m2)))))

(defn a4 [coll]
  (let [m2 (moment coll 2)
        m4 (moment coll 4)]
    (/ m4 (square m2))))

(defn skewness [coll]
  (condp apply [(a3 coll)]
    neg?  "negative"
    zero? "symmetric"
    pos?  "positive"))

(defn kurtosis [coll]
  (condp apply [(- (a4 coll) 3)]
    neg?  "platykurtic"
    zero? "mesokurtic"
    pos?  "leptokurtic"))

(defn quartiles [coll]
  [(icdf coll 0.25)
   (icdf coll 0.50)
   (icdf coll 0.75)])

(defn interquartile-range [coll]
  (let [[Q1 _ Q3] (quartiles coll)]
    (- Q3 Q1)))

(defn dispersion [coll]
  (let [[li ls] (minmax coll)
        md (median coll)]
    [(- md li)
     (- ls md)]))

(defn fences [coll K]
  (let [[Q1 _ Q3] (quartiles coll)
        IQR (- Q3 Q1)]
    [(- Q1 (* K IQR))
     (+ Q3 (* K IQR))]))

(defn outliers [coll K]
  (let [[fi fs] (fences coll K)]
    (filter
     #(or (< % fi)
          (> % fs))
     coll)))
