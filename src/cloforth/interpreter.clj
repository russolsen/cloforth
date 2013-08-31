(ns cloforth.interpreter
  [:require [cloforth.dictionary :as dict]
            [cloforth.primitives]
            [cloforth.compiler :as comp]])

(defn states [{dictionary :dictionary :as env}]
  (when-not (:quit env)
    (let [r (:in env)
          compiled (comp/compile-statement r dictionary )]
      (when-not (and (coll? compiled) (empty? compiled)) 
        (let [new-env (comp/inner compiled env)]
          (lazy-seq (cons new-env (states new-env))))))))


(defn next-state [{dictionary :dictionary :as env}]
  (when-not (:quit env)
    (let [r (:in env)
          compiled (comp/compile-statement r dictionary )]
      (when-not (and (coll? compiled) (empty? compiled)) 
        (comp/inner compiled env)))))

(defn repl [env]
  (let [states (take-while #(not (nil? %)) (iterate next-state env))]
    #_(doseq [s states] (cloforth.primitives/dump s))
    (last states)))

(defn run-reader [{old_in :in :as env} r]
  (assoc (repl (assoc env :in r)) :in old_in))

(defn run-string [env s] (run-reader env (java.io.StringReader. s)))

(defn run-file [env file]
  (with-open [r (java.io.FileReader. file)]
    (run-reader env r)))

(defn clean-env []
  (let  [ dict
         (merge
          (dict/create-dictionary 'cloforth.primitives)
          (dict/create-dictionary 'cloforth.compiler))]
    (run-file
     {:in *in* :dictionary dict :stack [] :code [] :ip 0}
     "init.c4")))

(defn main [ & files]
  (let [env (clean-env)]
    (if (or (nil? files) (empty? files))
      (repl env)
      (reduce run-file env files))))

(defn
  -main [ & files]
  (apply main files)
  nil)
