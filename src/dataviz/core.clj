(ns dataviz.core
  (:require [clojure.pprint :as pprint]
            [dataviz.statistics :as s]
            [dataviz.input :as i]
            [dataviz.vega :as vega])
  (:gen-class))

(defn -main [& args]
  (vega/create-page (sort (map #(Integer/parseInt %) args))))

(comment
  (let [coll (sort [46 55 56 57 58 59 61 61 65 66 67 68 68 69 70 70 71 72 74 75 80])
        quantiles (i/prompt-coll "quantile: " i/prompt-float)]
    (s/quantile-values coll quantiles)))
