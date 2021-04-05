(ns statistics.input
  (:require [clojure.string :as str]))

(defn prompt [message]
  (print message)
  (flush)
  (read-line))

(defn prompt-int [message]
  (try
    (Integer/parseInt (str/trim (prompt message)))
    (catch Exception _
      nil)))

(defn prompt-float [message]
  (try
    (Float/parseFloat (str/trim (prompt message)))
    (catch Exception _
      nil)))

(defn prompt-coll [message p-fn]
  (loop [coll (vector)]
    (let [q (p-fn message)]
      (if q
        (recur (conj coll q))
        coll))))
