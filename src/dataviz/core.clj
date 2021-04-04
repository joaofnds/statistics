(ns dataviz.core
  (:require [clojure.pprint :as pprint]
            [oz.core :as oz]
            [dataviz.statistics :as s])
  (:gen-class))

(defn build-chart [data]
  {:data {:values data}
   :mark "bar"
   :encoding {:x {:title "Número de pessoas com diabetes"
                  :field :j
                  :type "ordinal"
                  :axis {:labelAngle 0}}
              :y {:title "Porcentagem de grupos"
                  :field :F
                  :type "quantitative"
                  :axis {:labelExpr "datum.label * 100 + '%'"}}}})

(defn fd-table [fd]
  [:table {:border "1"}
   [:thead
    [:tr
     (for [k (keys (first fd))]
       [:th k])]]
   [:tbody
    (for [row fd]
      [:tr
       (for [v (vals row)]
         [:td v])])]])

(defn mean-median-mode-table [coll]
  [:table {:border 1}
   [:tbody
    [:tr
     [:th "mean"]
     [:td (float (s/mean coll))]]
    [:tr
     [:th "median"]
     [:td (s/median coll)]]
    [:tr
     [:th "mode"]
     [:td (str/join ", " (s/mode coll))]]]])

(defn -main [& args]
  (let [coll (map #(Float/parseFloat %) args)
        fd (s/frequency-distribution coll)]
    (oz/export!
     [:div
      (fd-table fd)
      (mean-median-mode-table coll)
      [:vega-lite (build-chart fd)]]
     "out.html")))
