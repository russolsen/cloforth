(ns cloforth.interpreter
  [:require [cloforth.dictionary :as dict]
            [cloforth.primitives]
            [cloforth.compiler :as comp]])

(defn prn-env [e] (println (dissoc e :dictionary)))

(defn repl [env]
  (let [dictionary (:dictionary env)]
    (if (:quit env)
      env
      (let [r (:in env)
            compiled (comp/compile-statement r dictionary )]
        (if (and (coll? compiled) (empty? compiled)) 
          env
          (let [result (comp/execute-program env compiled)
                _ (doall result)
                new-env (last result)]
            (recur new-env)))))))

(defn run-reader [{old_in :in :as env} r]
  (assoc (repl (assoc env :in r)) :in old_in))

(defn run-string [env s]
  (let [r (java.io.StringReader. s)]
    (run-reader env r)))

(defn run-file [env file]
  (let [r (java.io.FileReader. file)]
    (run-reader env r)))

(defn clean-env []
  (let  [ dict
         (merge
          (dict/create-dictionary 'cloforth.primitives)
          (dict/create-dictionary 'cloforth.compiler))]
    (run-file
     {:in *in* :dictionary dict :stack '() :frame-stack [] :ip 0}
     "init.c4")))

(defn main [ & files]
  (let [env (clean-env)]
    (if (or (nil? files) (empty? files))
      (repl env)
      (reduce run-file env files))))

(defn -main [ & files]
  (apply main files)
  nil)
