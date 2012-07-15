(ns cloforth.interpreter
  [:require [clojure.pprint :as pp]
            [cloforth.environment :as env]
            [cloforth.dictionary :as dict]
            [cloforth.primitives]
            [cloforth.compiler :as comp]
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

(defn db [msg env] (pp/pprint (str msg (dissoc env :dictionary))))

(defn repl [{dictionary :dictionary :as env}]
  #_(db "repl"  env)
  (if (:quit env)
    env
    (let [r (:in env)
          compiled (comp/compile-statement r dictionary )]
      (if (and (coll? compiled) (empty? compiled)) 
        env
        (recur (inner compiled env))))))

(defn dictionary []
  (merge
    (dict/create-dictionary 'cloforth.primitives)
    (dict/create-dictionary 'cloforth.compiler)))

(defn run-string [env s]
  (let [r (java.io.StringReader. s)
        old_in (:in env)]
    (assoc ( repl (assoc env :in r)) :in old_in)))

(defn clean-env []
  {:in *in* :dictionary (dictionary) :stack [] :return [] :ip 0})

(defn run-file [env file]
  (run-string env (slurp file)))

(defn run-files [env files]
  (if (empty? files)
    env
    (let [new-env (run-file env (first files))]
      (recur new-env (rest files)))))

(defn main [ & files]
  (println "FILES" files)
  (let [env (run-file (clean-env) "init.c4")]
    (if (or (nil? files) (empty? files))
      (repl env)
      (run-files env files))))

(defn -main [ & files]
  (apply main files))
