(ns dataviz.statistics)

(defn frequency-distribution [coll]
  (let [coll (sort coll)
        n (count coll)
        k (int (Math/ceil (Math/sqrt n)))
        es (apply min coll)
        ei (apply max coll)
        i (/ (- ei es) k)
        classes (map #(+ es (* i %)) (range k))]
    (map-indexed
     (fn [j _class]
       (let [min (+ es (* j i))
             max (+ es (* (inc j) i))
             max-comp (if (= (inc j) k) <= <)
             numbers (filter #(and (<= min %) (max-comp % max)) coll)
             F (count numbers)
             F-prime (count (filter #(max-comp % max) coll))
             f (float (/ F n))
             f-prime (float (/ F-prime n))
             c (float (/ (+ min max) 2))]
         {:j (inc j) :min min :max max :F f :F-prime F-prime :f f :f-prime f-prime :c c}))
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
