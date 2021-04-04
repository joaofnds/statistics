(ns dataviz.input
  (:require [clojure.string :as str]))

(defn prompt [message]
  (print message)
  (flush)
  (read-line))

(defn prompt-int [message]
  (Integer/parseInt (str/trim (prompt message))))

(defn prompt-float [message]
  (Float/parseFloat (str/trim (prompt message))))

(defn prompt-coll [message p-fn]
  (loop [coll (vector)]
    (let [q (p-fn message)]
      (if q
        (recur (conj coll q))
        coll))))
