(ns cloforth.interpreter
  [:require [clojure.pprint :as pp]
            [cloforth.environment :as env]
            [cloforth.dictionary :as dict]
            [cloforth.primitives]
            [cloforth.compiler :as comp]
            [cloforth.tokenizer :as tok]])

(defn handle-def [string env]
  (if-let [word-f ((:dictionary env) string)]
    (word-f env)))

(defn handle-number [string env]
  (if-let [n (tok/to-int string)]
    (env/stack-push n env)))

(defn handle-unknown [string env]
  (println "Unknown: don't know what to do with" string)
  env)

(defn handle-word [string env]
  (or (handle-def string env)
      (handle-number string env)
      (handle-unknown string env)))

(defn run [token env]
  (let [type (:type token)
        text (:text token)]
    (case type
      :string (env/stack-push text env)
      :word (handle-word text env)
      :eof (handle-unknown env)
      (handle-unknown env text))))

(defn repl [env]
  (let [t (tok/get-token)]
    (if (= (:type t) :eof)
      env
      (recur (run t env)))))

(defn dictionary []
  (merge
    (dict/create-dictionary 'cloforth.primitives)
    (dict/create-dictionary 'cloforth.compiler)))

(def clean-env {:dictionary (dictionary) :stack [] :return []})

(defn run-string [env s]
  (with-in-str s (repl env)))

(defn run-files [env files]
  (if (empty? files)
    env
    (let [new-env (run-string env (slurp (first files)))]
      (recur new-env (rest files)))))

(defn -main [ & files]
  (let [env {:dictionary (dictionary) :stack []}]
    (if (empty? files)
      (repl env)
      (run-files env files))))
