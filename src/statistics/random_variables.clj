(ns statistics.random-variables
  (:require [statistics.math :refer [square sqrt]]))

(defn mean [values weights]
  (apply + (map * values weights)))

(defn variance
  ([values weights]
   (variance values weights (mean values weights)))

  ([values weights mu]
   (apply + (map
              #(* %2 (square (- %1 mu)))
              values weights))))

(defn std-deviation
  ([values weights]
   (std-deviation values weights (mean values weights)))

  ([values weights mu]
   (sqrt (variance values weights mu))))

(comment
  (let [values [490 -10]
        weights [0.01 0.99]]
    (println (str "mu:  " (mean values weights)))
    (println (str "s^2: " (variance values weights)))
    (println (str "s:   " (std-deviation values weights)))))

