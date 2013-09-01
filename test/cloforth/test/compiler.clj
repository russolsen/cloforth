(ns cloforth.test.compiler
  (:use [cloforth.compiler :as c])
  (:use [clojure.test])
  (:require [cloforth.environment :as env]))


(def starting-env (env/make-env))

(defn inc-counter [env] (assoc env :counter (inc (:counter env))))

(defn dec-counter [env] (assoc env :counter (dec (:counter env))))

(defn ip-is [expected-value env] (is (= (:ip env) expected-value)))

(deftest execute-one-function
  (let [env (c/execute-program starting-env inc-counter)]
    (is (map? env))
    (is (= 1 (:counter env)))
    (is (= (:ip env) 0))))

(deftest execute-one-single-element-vector
  (let [env (c/execute-program starting-env [inc-counter])]
    (is (map? env))
    (is (= 1 (:counter env)))
    (is (= (:ip env) 0))))

(deftest execute-one-two-element-vector
  (let [env (c/execute-program starting-env [inc-counter inc-counter])]
    (is (map? env))
    (is (= 2 (:counter env)))
    (is (= (:ip env) 0))))



