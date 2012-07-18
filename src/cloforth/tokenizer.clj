(ns cloforth.tokenizer
  (:import (clojure.lang LineNumberingPushbackReader))
  (:use [clojure.java.io :as jio]))

(defn to-int [string]
  (try
    (Long/parseLong string)
    (catch Exception e nil)))

(defn- to-char [i]
  (if (< i 0) i (char i)))

(defn- get-ch [r]
  (when (and (instance? LineNumberingPushbackReader r) (.atLineStart r))
    (print "c4>> ")
    (flush))
  (to-char (.read r)))

(defn- ws? [ch]
  (or (= ch \space) (= ch \tab) (= ch \newline)))

(defn- eof? [ch]
  (and (number? ch) (< ch 0)))

(defn- handle-start [r ch token]
  (cond
    (eof? ch) {:state :complete :type :eof :text nil}
    (ws? ch)  (recur r (get-ch r) token)
    (= ch \;) {:state :comment }
    (= ch \') {:state :string     :type :string :text ""}
    (= ch \[)  {:state :complete  :type :l-bracket :text "["}
    (= ch \])  {:state :complete  :type :r-bracket :text "]"}
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

(defn- handle-comment [ch token]
  (if (= ch \newline)
    (assoc token :state :start)
    token))

(defn- read-token [r token]
  (case (:state token)
    :complete token
    :comment (recur r (handle-comment (get-ch r) token))
    :start (recur r (handle-start r (get-ch r) token))
    :string (recur r (handle-string (get-ch r) token))
    :word (recur r (handle-word (get-ch r) token))
    (assoc token :state :error)))

(defn get-token [r]
  (select-keys (read-token r {:state :start}) [:type :text] ))