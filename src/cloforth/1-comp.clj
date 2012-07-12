(ns cloforth.compiler
  [:require [clojure.pprint :as pp]
            [cloforth.environment :as env]
            [cloforth.primitives]
            [cloforth.dictionary :as dict]
            [cloforth.tokenizer :as tok]])

(defn- program-runner [program orig-env]
  (loop [e (env/init-ip orig-env)]
    (let [ip (:ip e)]
      (if (>= ip (count program))
        e
        (let [e ((get program ip) e)]
          (recur (env/inc-ip e)))))))

(defn- compile-word 
  "Compile the given word, returning it's function"
  [dictionary text]
  (if-let [d (dictionary text)]
    d
    (if-let [n (tok/to-int text)]
      (partial env/stack-push n))))

(defn- compile-token 
  "Compile the given token, returning it's function"
  [dictionary token]
  (let [type (:type token)
        text (:text token)]
    (case type
      :string (partial env/stack-push text)
      :word (compile-word dictionary text)
      :eof nil
      (println "don't know what to do with" text))))

(defn- compile-until 
  "Keep compiling words until f-done? returns true"
  [dictionary f-done? result]
  (let [token (tok/get-token)]
    (if (f-done? token)
      result
      (if-let [compiled (compile-token dictionary token)]
        (recur dictionary f-done? (concat result [compiled]))
        result))))

(defn- compile-compound 
  "Compile a compound word (i.e. [ w w w ]), returns vector"
  [dictionary]
  (compile-until
    dictionary
    (fn [token] (or (= (:type token) :eof) (= (:type token) :r-bracket)))
    []))
 
(defn- compile-statement [dictionary]
  (let [token (tok/get-token)]
    (case (:type token)
      :eof []
      :l-bracket (partial program-runner (vec (compile-compound dictionary)))
      (compile-word dictionary (:text token)))))

(defn primitive-define [env]
  (let [name-token (tok/get-token)
        name  (:text name-token)
        dictionary (:dictionary env)
        body (compile-statement dictionary)]
    (update-in env [:dictionary] assoc name body)))

(defn primitive-if [env]
  (let [body (compile-statement (:dictionary env))]
    (partial program-runner  [(partial env/branch 1)  body])))

(defn doit []
  (println (compile-statement (dict/create-dictionary 'cloforth.primitives))))

