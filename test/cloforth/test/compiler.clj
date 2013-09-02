(ns cloforth.test.compiler
  (:use [cloforth.compiler :as c])
  (:use [cloforth.execute :as x])
  (:use [clojure.test])
  (:require [cloforth.environment :as env]))


(def starting-env (assoc (env/make-env) :counter 0))

(defn inc-counter [env] (update-in env [:counter] inc))

(defn dec-counter [env] (update-in env [:counter] dec))

(deftest execute-one-function
  (let [env (last (x/execute-program starting-env inc-counter))]
    (is (map? env))
    (is (= 1 (:counter env)))))

(deftest execute-one-single-element-vector
  (let [env (last (x/execute-program starting-env [inc-counter]))]
    (is (map? env))
    (is (= 1 (:counter env)))))

(deftest execute-one-two-element-vector
  (let [env (last (x/execute-program starting-env [inc-counter inc-counter]))]
    (is (map? env))
    (is (= 2 (:counter env)))))



