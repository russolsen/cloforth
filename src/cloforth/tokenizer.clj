(ns cloforth.tokenizer
  (:use [clojure.java.io :as jio]))

(defn to-int [string]
  (try
    (Long/parseLong string)
    (catch Exception e nil)))

(defn- to-char [i]
  (if (< i 0) i (char i)))

(defn- get-ch []
  (to-char (.read *in*)))

(defn- ws? [ch]
  (or (= ch \space) (= ch \tab) (= ch \newline)))

(defn- eof? [ch]
  (and (number? ch) (< ch 0)))

(defn- handle-start [ch token]
  (cond
    (eof? ch) {:state :complete :type :eof :text nil}
    (ws? ch)  (recur (get-ch) token)
    (= ch \') {:state :string :type :string :text ""}
    (= ch \[)  {:state :complete :type :l-bracket :text "["}
    (= ch \])  {:state :complete :type :r-bracket :text "]"}
    :default  (assoc token :state :word :type :word :text (str ch))))

(defn- handle-string [ch token]
  (cond
    (eof? ch) (assoc token :state :complete :type :premature-eof)
    (= ch \') (assoc token :state :complete)
    :default  (assoc token :text (str (:text token) ch))))

(defn- handle-word [ch token]
  (cond
    (eof? ch) (assoc token :state :complete)
    (ws? ch)  (assoc token :state :complete)
    :default  (assoc token :text (str (:text token) ch))))


(defn- read-token [token]
  (case (:state token)
    :complete token
    :start (recur (handle-start (get-ch) token))
    :string (recur (handle-string (get-ch) token))
    :word (recur (handle-word (get-ch) token))
          (assoc token :state :error)))

(defn get-token []
  (select-keys (read-token {:state :start}) [:type :text] ))
