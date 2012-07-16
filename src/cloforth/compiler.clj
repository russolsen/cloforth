(ns cloforth.compiler
  [:require [clojure.pprint :as pp]
            [cloforth.environment :as env]
            [cloforth.primitives :as prims]
            [cloforth.dictionary :as dict]
            [cloforth.tokenizer :as tok]])

(defn inner [program orig-env]
  #_(println "inner" program)
  (if (fn? program)
    (program orig-env)
    (loop [e (env/init-ip orig-env)]
      (let [ip (:ip e)]
        (if (>= ip (count program))
          (assoc e :ip (:ip orig-env))
          (let [f (get program ip)
                new-env (f e)]
            (recur (env/inc-ip new-env))))))))

(declare compile-statement)

(defn compile-if [r dictionary]
  (let [body (compile-statement r dictionary)]
    (vec (concat [prims/primitive-not (partial env/branch (count body))] body))))

(defn compile-ifelse [r dictionary]
  (let [true-part  (compile-statement r dictionary)
        false-part (compile-statement r dictionary)]
    (vec
     (concat
      [prims/primitive-not (partial env/branch 2)] true-part [(partial env/jump 1)] false-part))))

(defn compile-word 
  "Compile the given word, returning a vector of functions"
  [r dictionary text]
  (cond
    (= "if" text) (compile-if r dictionary)
    (= "ifelse" text) (compile-ifelse r dictionary)
    (dictionary text) (partial inner (dictionary text))
    (tok/to-int text) (partial env/stack-push (tok/to-int text))
    :default (println "Don't know what to do with" text)))

(defn- compile-token 
  "Compile the given token, returning a vector of functions"
  [r dictionary token]
  (let [type (:type token)
        text (:text token)]
    (case type
      :string [(partial env/stack-push text)]
      :word (compile-word r dictionary text)
      :eof nil
      (println "don't know what to do with" text))))

(defn compile-until 
  "Keep compiling words until f-done? is true, returns modified result vector"
  [r dictionary f-done? result]
  (let [token (tok/get-token r)]
    (if (f-done? token)
      result
      (if-let [compiled (compile-token r dictionary token)]
        (recur r dictionary f-done? (if (coll? compiled) (vec (concat result compiled)) (vec (conj result compiled))))
        result))))

(defn- compile-compound 
  "Compile a compound word (i.e. [ w w w ]), returns vector"
  [r dictionary]
  (compile-until
    r
    dictionary
    (fn [token] (or (= (:type token) :eof) (= (:type token) :r-bracket)))
    []))
 
(defn compile-statement [r dictionary]
  (let [token (tok/get-token r)]
    (case (:type token)
      :eof []
      :string (partial env/stack-push (:text token))
      :l-bracket (compile-compound r dictionary)
      (compile-token r dictionary token))))


(defn primitive-compile [env]
  (let [dictionary (:dictionary env)
        r (:in env)
        body (compile-statement r dictionary)]
    (env/stack-push body env)))

#_(defn primitive-define [{r :in dictionary :dictionary :as env}]
  (let [token (tok/get-token r)
        name (:text token)
        body (compile-statement r dictionary)]
    (env/set! name body env)))

(defn primitive-gettok [{r :in :as env}]
  (let [token (tok/get-token r)]
    (env/stack-push (:text token) env)))