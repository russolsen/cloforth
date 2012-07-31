(ns cloforth.test.compiler
  (:use [cloforth.compiler :as c])
  (:use [clojure.test]))

(def starting-env {:ip 0 :stack [] :counter 0})

(defn inc-counter [env] (assoc env :counter (inc (:counter env))))

(defn dec-counter [env] (assoc env :counter (dec (:counter env))))

(defn ip-is [expected-value env] (is (= (:ip env) expected-value)))

(deftest execute-one-function
  (let [env (c/inner inc-counter starting-env)]
    (is (map? env))
    (is (= 1 (:counter env)))
    (is (= (:ip env) 0))))

(deftest execute-one-single-element-vector
  (let [env (c/inner [inc-counter] starting-env)]
    (is (map? env))
    (is (= 1 (:counter env)))
    (is (= (:ip env) 0))))

(deftest execute-one-two-element-vector
  (let [env (c/inner [inc-counter inc-counter] starting-env)]
    (is (map? env))
    (is (= 2 (:counter env)))
    (is (= (:ip env) 0))))

(deftest ip-is-respected
  (let [this-env (assoc starting-env :ip 1)
        env (c/inner [dec-counter inc-counter dec-counter] this-env)]
    (is (map? env))
    (is (= -1 (:counter env)))
    (is (= (:ip env) 1))))


(deftest execute-one-nested-vectors
  (let [env (c/inner [inc-counter [dec-counter] [inc-counter] inc-counter] starting-env)]
    (is (= 2 (:counter env))))
  (let [env (c/inner [inc-counter [dec-counter inc-counter] inc-counter] starting-env)]
    (is (= 2 (:counter env))))
  (let [env (c/inner [[inc-counter dec-counter inc-counter] [inc-counter]] starting-env)]
    (is (= 2 (:counter env))))
  #_(let [env (c/inner [[inc-counter [dec-counter [inc-counter]]] [inc-counter]] starting-env)]
    (is (= 2 (:counter env)))))


