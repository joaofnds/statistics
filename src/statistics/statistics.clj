(ns statistics.statistics)

(defn frequency-distribution [coll]
  (let [coll (sort coll)
        n (count coll)
        k (int (Math/ceil (Math/sqrt n)))
        inferior-limit (apply min coll)
        superior-limit (apply max coll)
        i (/ (- superior-limit inferior-limit) k)
        classes (map #(+ inferior-limit (* i %)) (range k))]
    (map-indexed
     (fn [j _class]
       (let [min (+ inferior-limit (* j i))
             max (+ inferior-limit (* (inc j) i))
             max-comp (if (= (inc j) k) <= <)
             numbers (filter #(and (<= min %) (max-comp % max)) coll)
             F (count numbers)
             F-prime (count (filter #(max-comp % max) coll))
             f (float (/ F n))
             f-prime (float (/ F-prime n))
             c (float (/ (+ min max) 2))]
         {:j (inc j) :min min :max max :F F :F-prime F-prime :f f :f-prime f-prime :c c}))
     classes)))

(defn mean [coll]
  (let [sum (apply + coll)
        count (count coll)]
    (if (pos? count)
      (/ sum count)
      0)))

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

(defn variance [coll]
  (let [square #(* % %)
        n (count coll)
        mean (mean coll)
        square-diffs (map #(square (- % mean)) coll)]
    (/ (apply + square-diffs) (dec n))))

(defn std-deviation [coll]
  (Math/sqrt (variance coll)))

(defn coefficient-of-variation [coll]
  (let [std-deviation (std-deviation coll)
        mean (mean coll)]
    (* (/ std-deviation mean) 100)))

(defn range [coll]
  (- (apply max coll) (apply min coll)))

(defn square [x]
  (* x x))

(defn pow [x y]
  (reduce * (repeat y x)))

(defn sqrt [x]
  (Math/sqrt x))

(defn moment [coll ord]
  {:pre [(pos? ord)]}
  (let [mean (mean coll)]
    (/ (apply + (map #(pow (- % mean) ord) coll))
       (count coll))))

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
