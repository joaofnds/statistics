(ns dataviz.core
  (:require [clojure.pprint :as pprint]
            [dataviz.statistics :as s]
            [dataviz.input :as i]
            [dataviz.vega :as vega])
  (:gen-class))

(defn -main [& args]
  (vega/create-page (sort (map #(Integer/parseInt %) args))))
