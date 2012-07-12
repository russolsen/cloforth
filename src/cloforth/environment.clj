(ns cloforth.environment
  [:require [clojure.pprint :as pp]])

(defn stack-push [v env]
  (assoc env :stack (vec (cons v (:stack env)))))

(defn stack-pop [env]
  (assoc env :stack (subvec (:stack env) 1)))

(defn stack-top [env]
  (first (:stack env)))

(defn ret-push [v env]
  (assoc env :ret (vec (cons v (:ret env)))))

(defn ret-pop [env]
  (assoc env :ret (subvec (:ret env) 1)))

(defn ret-top [env]
  (first (:ret env)))

(defn inc-ip [env]
  (assoc env :ip (inc (:ip env))))

(defn jump [n env]
  (println "jump " n "old ip" (:ip env) "new ip" (+ (:ip env) n))
  (assoc env :ip (+ (:ip env) n)))

(defn recur [n env]
  (println "jump " n "old ip" (:ip env) "new ip" (+ (:ip env) n))
  (assoc env :ip (+ (:ip env) n)))

(defn branch [n env]
  (let [top (stack-top env)
        env (stack-pop env)]
    (println "branch" top (:ip env))
    (if top
      (jump n env)
      env)))

(defn init-ip [env]
  (assoc env :ip 0))
