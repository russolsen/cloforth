(ns cloforth.environment
  [:require [clojure.pprint :as pp]])

(defn stack-push [v env]
  (assoc env :stack (vec (cons v (:stack env)))))

(defn stack-pop
  ([env]
     (stack-pop 1 env))
  ([n env]
     (if (> n (count (:stack env)))
       (do (println "Stack underflow!!") env)       
       (assoc env :stack (subvec (:stack env) n)))))

(defn stack-top [env]
  (if (empty? (:stack env))
       (do (println "Stack underflow!!") env)       
       (first (:stack env))))

(defn inc-ip [env]
  (assoc env :ip (inc (:ip env))))

(defn set! [name value env]
  (update-in env [:dictionary] assoc name value))

(defn goto [n env]
  (assoc env :ip n))

(defn jump [n env]
  (goto (+ (:ip env) n) env))

(defn recur [env]
  (goto 0 env))

(defn branch [n env]
  (let [top (stack-top env)
        env (stack-pop env)]
    (if top
      (jump n env)
      env)))

(defn init-ip [env]
  (assoc env :ip 0))
