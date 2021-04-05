(ns statistics.core
  (:require [clojure.pprint :as pprint]
            [statistics.statistics :as s]
            [statistics.input :as i]
            [statistics.vega :as vega])
  (:gen-class))

(defn -main [& args]
  (vega/create-page (sort (map #(Float/parseFloat %) args))))

(comment
  (let [coll (sort [46 55 56 57 58 59 61 61 65 66 67 68 68 69 70 70 71 72 74 75 80])
        quantiles (i/prompt-coll "quantile: " i/prompt-float)]
    (s/quantile-values coll quantiles)))
