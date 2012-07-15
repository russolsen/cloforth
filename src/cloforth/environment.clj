(ns cloforth.environment
  [:require [clojure.pprint :as pp]])

(defn stack-push [v env]
  (assoc env :stack (vec (cons v (:stack env)))))

(defn stack-pop [env]
  (if (empty? (:stack env))
       (do (println "Stack underflow!!") env)       
       (assoc env :stack (subvec (:stack env) 1))))

(defn stack-top [env]
  (if (empty? (:stack env))
       (do (println "Stack underflow!!") env)       
       (first (:stack env))))

(defn inc-ip [env]
  (assoc env :ip (inc (:ip env))))

(defn set! [name value env]
  (update-in env [:dictionary] assoc name value))

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
