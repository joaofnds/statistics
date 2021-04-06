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
    (/ (reduce + square-diffs) (dec n))))

(defn std-deviation [coll]
  (Math/sqrt (variance coll)))

(defn coefficient-of-variation [coll]
  (let [std-deviation (std-deviation coll)
        mean (mean coll)]
    (* (/ std-deviation mean) 100)))
