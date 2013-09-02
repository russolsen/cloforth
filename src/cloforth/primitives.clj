(ns cloforth.primitives
  [:require [clojure.pprint :as pp]
   [cloforth.environment :as env]])

(defn dump [env]
  (println "======")
  (println "stack:" (:stack env))
  (println "frame statck:" (:frame-stack env))
  (println "dict keys:" (sort (keys (:dictionary env))))
  (println "======")
  env)

(defn- binary-op [env f]
  (let [b (env/stack-nth env 1)
        a (env/stack-nth env 0)]
    (env/stack-push (env/stack-pop 2 env) (f a b) )))

(defn- unary-op [env f]
  (let [x (first (:stack env))]
    (env/stack-push (env/stack-pop env) (f x))))

(defn primitive-print [env] (print (env/stack-peek env)) env)

(defn primitive-char [env] (unary-op env char))

(defn primitive-not [env] (unary-op env not))

(defn primitive-+ [env] (binary-op env +))

(defn primitive-- [env] (binary-op env -))

(defn primitive-* [env] (binary-op env *))

(defn primitive-div [env] (binary-op env /))

(defn primitive-> [env] (binary-op env >))

(defn primitive-<= [env] (binary-op env <=))

(defn primitive->= [env] (binary-op env >=))

(defn primitive-< [env] (binary-op env <))

(defn primitive-= [env] (binary-op env =))

(defn primitive-mod [env] (binary-op mod =))

(defn primitive-drop [env] (env/stack-pop env))

(defn primitive-dup [env] (env/stack-push (env/stack-peek env) env))

(defn push-one [env] (env/stack-push env 1))

(defn push-two [env] (env/stack-push env 2))

(defn rot [env]
  (let [a (env/stack-nth env 0)
        b (env/stack-nth env 1)
        env (env/stack-pop 2 env)]
    (-> env
        (env/stack-push a)
        (env/stack-push b))))

(defn lrot [env]
  (let [c (env/stack-nth env 0)
        b (env/stack-nth env 1)
        a (env/stack-nth env 2)
        env (env/stack-pop 3 env)]
    (-> env
         (env/stack-push b)
         (env/stack-push c)
         (env/stack-push a))))

(defn primitive-set! [env]
  (let [name (env/stack-nth env 0)
        value (env/stack-nth env 1)
        env (env/stack-pop 2 env)]
    (println "set: name:" name "value:" value)
    (update-in env [:dictionary] assoc name value)))

(defn lookup [env]
  (unary-op env (fn [name] (get (:dictionary env) name))))

(defn ip [env] (env/stack-push (:ip env) env))

(defn goto [env]
  (let [address (env/stack-peek env)]
    (env/stack-pop (assoc env :ip address))))

(defn jump [env]
  (let [delta (env/stack-peek env)]
    (println "in jump, delta: " delta)
    (let [new-env (env/jump  (env/stack-pop env) delta)]
      (println "new env:")
      (dump new-env)
      new-env)))

#_(defn recur [env] (assoc env :ip -1))

(defn clear [env] (assoc env :stack []))

(defn quit [env] (assoc env :quit true))

(defn primitive-.dict [env] (pp/pprint (:dictionary env)) env)

(defn primitive-.def [env]
  (let [word (env/stack-nth env 0)]
    (pp/pprint (get (:dictionary env) word))
    (env/stack-pop env)))

(defn primitive-.stack [env] (pp/pprint (:stack env)) env)

(defn hello [env] (println "HELLO") env)
