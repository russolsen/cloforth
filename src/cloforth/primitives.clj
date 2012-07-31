(ns cloforth.primitives
  [:require [clojure.pprint :as pp]
   [cloforth.environment :as env]])

(defn dump [env]
  (pp/pprint (dissoc env :dictionary :in)))

(defn- binary-op [env f]
  (let [b (first (:stack env))
        a (second (:stack env))]
     (env/stack-push (f a b) (env/stack-pop 2 env))))

(defn- unary-op [env f]
  (let [x (first (:stack env))]
    (env/stack-push (f x) (env/stack-pop env))))

(defn primitive-print [env] (print (env/stack-top env)) env)

(defn primitive-char [env] (unary-op env char))

(defn primitive-not [env] (unary-op env not))

(defn primitive-+ [env] (binary-op env +))

(defn primitive-- [env] (binary-op env -))

(defn primitive-* [env] (binary-op env *))

(defn primitive-div [env] (binary-op env /))

(defn primitive-> [env] (binary-op env >))

(defn primitive-< [env] (binary-op env <))

(defn primitive-= [env] (binary-op env =))

(defn primitive-mod [env] (binary-op mod =))

(defn primitive-drop [env] (env/stack-pop env))

(defn primitive-dup [env] (env/stack-push (env/stack-top env) env))

(defn rot [env]
  (let [a (first (:stack env))
        b (second (:stack env))
        env (env/stack-pop 2 env)]
    (->> env
        (env/stack-push a)
        (env/stack-push b))))

(defn lrot [env]
  (let [c (first (:stack env))
        b (second (:stack env))
        a (nth (:stack env) 2)
        env (env/stack-pop 3 env)]
    (->> env
         (env/stack-push b)
         (env/stack-push c)
         (env/stack-push a))))

(defn primitive-set! [env]
  (let [name (first (:stack env))
        value (second (:stack env))
        env (env/stack-pop 2 env)]
    #_(println "*******prim set" name value)
    (update-in env [:dictionary] assoc name value)))

(defn lookup [env]
  (unary-op env (fn [name] (get (:dictionary env) name))))

(defn ip [env] (env/stack-push (:ip env) env))

(defn goto [env]
  (let [address (env/stack-top env)]
    (env/stack-pop (assoc env :ip address))))

(defn jump [env]
  (let [delta (env/stack-top env)]
    (env/jump delta (env/stack-pop env))))

(defn recur [env] (assoc env :ip -1))

(defn clear [env] (assoc env :stack []))

(defn quit [env] (assoc env :quit true))

(defn primitive-.dict [env] (pp/pprint (apply sorted-map (flatten (seq (:dictionary env))))) env)

(defn primitive-.stack [env] (pp/pprint (:stack env)) env)

(defn hello [env] (println "HELLO") env)
