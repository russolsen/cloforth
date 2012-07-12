(ns cloforth.dictionary
  [:require [clojure.string :as s]])

(defn- to-prim-name [sym]
  (s/replace-first (str sym) "primitive-" "" ))

(defn- map-keys [f h]
  (apply
    hash-map
    (flatten
      (map
        (fn [[k v]] [(f k) v])
        h))))

(defn create-dictionary [ns]
  (map-keys to-prim-name (ns-publics ns)))
