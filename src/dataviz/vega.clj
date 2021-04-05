(ns statistics.vega
  (:require [clojure.string :as str]
            [oz.core :as oz]
            [statistics.statistics :as s]))

(defn build-chart [data]
  {:mark "bar"
   :autosize {:type "fit" :contains "padding"}
   :data {:values data}
   :encoding {:x {:field :j
                  :type "ordinal"
                  :axis {:labelAngle 0}}
              :y {:field :F
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

(defn quartiles-table [coll]
  (let [[q25 q50 q75] (s/quantile-values coll [0.25 0.5 0.75])]
    [:table {:border 1}
     [:tbody
      [:tr
       [:th "Q.25"]
       [:td q25]]
      [:tr
       [:th "Q.50"]
       [:td q50]]
      [:tr
       [:th "Q.75"]
       [:td q75]]]]))

(defn variance-table [coll]
  [:table {:border 1}
   [:tbody
    [:tr
     [:th "variance"]
     [:td (s/variance coll)]]
    [:tr
     [:th "standard deviation"]
     [:td (s/std-deviation coll)]]
    [:tr
     [:th "CV"]
     [:td (s/coefficient-of-variation coll)]]]])

(defn create-page [coll]
  (let [fd (s/frequency-distribution coll)]
    (oz/export!
     [:div
      (fd-table fd)
      (mean-median-mode-table coll)
      (quartiles-table coll)
      (variance-table coll)
      [:vega-lite (build-chart fd)]]
     "out.html")))
