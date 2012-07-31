(ns cloforth.dictionary
  [:require [clojure.string :as s]])

(defn- to-prim-name [sym]
  (s/replace-first (str sym) "primitive-" "" ))

(defn name-for [value dict]
  (some #(if (= (val %) value) (key %)) dict))

(defn- to-dictionary [h]
  (apply
    hash-map
    (flatten
      (map
        (fn [[key value]] [(to-prim-name key) @value])
        h))))

(defn create-dictionary [ns]
  (to-dictionary (ns-publics ns)))
