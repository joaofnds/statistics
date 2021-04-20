(ns statistics.vega
  (:require [clojure.string :as str]
            [oz.core :as oz]
            [statistics.statistics :as s]
            [statistics.statistics.frequecy-distribution :as fd]))

(defn build-chart [data]
  {:mark "bar"
   :autosize {:type "fit" :contains "padding"}
   :data {:values data}
   :encoding {:x {:field :j
                  :type "ordinal"
                  :axis {:labelAngle 0}}
              :y {:field :f
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

(defn properties-table
  ([coll] (properties-table coll [0.25 0.75]))
  ([coll points]
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
      [:td (str/join ", " (s/mode coll))]]
     (for [p points]
       [:tr
        [:th (str "Q" p)]
        [:td (s/icdf coll p)]])
     [:tr
      [:th "Tukey's fences (K=1.5)"]
      [:td (str/join ", " (s/fences coll 1.5))]]
     [:tr
      [:th "Tukey's fences (K=3)"]
      [:td (str/join ", " (s/fences coll 3))]]
     [:tr
      [:th "variance"]
      [:td (s/variance coll)]]
     [:tr
      [:th "standard deviation"]
      [:td (s/std-deviation coll)]]
     [:tr
      [:th "CV"]
      [:td (s/coefficient-of-variation coll)]]
     [:tr
      [:th "range"]
      [:td (s/coll-range coll)]]
     [:tr
      [:th "m2"]
      [:td (s/moment coll 2)]]
     [:tr
      [:th "m3"]
      [:td (s/moment coll 3)]]
     [:tr
      [:th "m4"]
      [:td (s/moment coll 4)]]
     [:tr
      [:th "a3"]
      [:td (s/a3 coll)]]
     [:tr
      [:th "a4"]
      [:td (s/a4 coll)]]
     [:tr
      [:th "skewness"]
      [:td (s/skewness coll)]]
     [:tr
      [:th "kurtosis"]
      [:td (s/kurtosis coll)]]]]))

(defn create-page [coll]
  (let [fd (fd/frequency-distribution coll :sqrt)]
    (oz/export!
     [:div
      (fd-table fd)
      (properties-table coll)
      [:vega-lite (build-chart fd)]]
     "out.html")))
