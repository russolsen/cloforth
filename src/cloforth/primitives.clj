(ns cloforth.primitives
  [:require [clojure.pprint :as pp]
            [cloforth.environment :as env]])

(defn dump [env]
  (pp/pprint env)
  env)

(defn- binary-op [env f]
  (let [b (first (:stack env))
        a (second (:stack env))]
     (env/stack-push (f a b) (env/stack-pop (env/stack-pop env)))))

(defn- unary-op [env f]
  (let [x (first (:stack env))]
    (env/stack-push (f x) (env/stack-pop env))))

(defn primitive-. [env]
  (print (env/stack-top env))
  (env/stack-pop env))

(defn nl [env]
  (println)
  env)

(defn primitive-true [env] (env/stack-push true env))

(defn primitive-false [env] (env/stack-push false env))

(defn primitive-not [env] (unary-op env not))

(defn primitive-inc [env] (unary-op env inc))

(defn primitive-dec [env] (unary-op env dec))

(defn primitive-drop [env] (env/stack-pop env))

(defn primitive-dup [env] (env/stack-push (env/stack-top env) env))

(defn primitive-+ [env] (binary-op env +))

(defn primitive-- [env] (binary-op env -))

(defn primitive-* [env] (binary-op env *))

(defn primitive-div [env] (binary-op env /))

(defn primitive-> [env] (binary-op env >))

(defn primitive-< [env] (binary-op env <))

(defn primitive-= [env] (binary-op env =))

(defn rot [env]
  (let [a (first (:stack env))
        b (second (:stack env))
        env (env/stack-pop (env/stack-pop env))]
    (env/stack-push b (env/stack-push a env))))

(defn primitive-set! [env]
  (let [name (first (:stack env))
        value (second (:stack env))
        env (env/stack-pop (env/stack-pop env))]
    (update-in env [:dictionary] assoc name value)))

(defn lookup [env]
  (unary-op env (fn [name] (get (:dictionary env) name))))

(defn clear [env]
  (assoc env :stack []))

(defn primitive-quit [env]
  (assoc env :quit true))

(defn set-env! [env])

(defn primitive-.dict [env]
  (pp/pprint (:dictionary env))
  env)

(defn primitive-.stack [env]
  (pp/pprint (:stack env))
  env)

(defn hello [env]
  (println "HELLO")
  env)
