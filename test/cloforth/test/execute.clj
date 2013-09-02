(ns cloforth.test.execute
  (:use [cloforth.execute :as x])
  (:use [cloforth.repl :as repl])
  (:use [clojure.test]))

(defn- x
  "Execute a string as a cloforth program"
  [s]
  (repl/run-string (repl/clean-env) s))

(defn- stack-from [s]
  (:stack (x s)))

(defn- output-from [s]
  (with-out-str (x s)))

(deftest nothing-from-nothing
  (is (= (stack-from "") [])))

(deftest simple_push
  (is (= (stack-from "100") [100]))
  (is (= (stack-from "200 100") [100 200])))

(deftest simple_drop
  (is (= (stack-from "100 drop") [])))

(deftest dup
  (is (= (stack-from "100 dup") [100 100])))

(deftest add
  (is (= (stack-from "100 100 +") [200])))

(deftest sub
  (is (= (stack-from "200 100 -") [100])))

(deftest mult
  (is (= (stack-from "5 4 *") [20])))

(deftest div
  (is (= (stack-from "9 3 -") [6])))

(deftest gt
  (is (= (stack-from "9 3 >") [true]))
  (is (= (stack-from "3 9 >") [false])))

(deftest lt
  (is (= (stack-from "9 3 <") [false]))
  (is (= (stack-from "3 9 <") [true])))

(deftest is-equals
  (is (= (stack-from "9 3 =") [false]))
  (is (= (stack-from "3 9 =") [false]))
  (is (= (stack-from "1 1 =") [true]))
  (is (= (stack-from "2 2 =") [true]))
  (is (= (stack-from "3 3 =") [true])))

(deftest string-equals
  (is (= (stack-from "'xx' 'xx' =") [true])))

(deftest simple-not
  (is (= (stack-from "true not") [false]))
  (is (= (stack-from "false not") [true])))

(deftest add-one
  (is (= (stack-from "1 1+") [2]))
  (is (= (stack-from "2 1+") [3])))

(deftest sub-one
  (is (= (stack-from "1 1-") [0]))
  (is (= (stack-from "2 1-") [1])))

(deftest dot
  (is (= (output-from "99 .") "99")))

(deftest nl
  (is (= (output-from "nl") "\n")))

(deftest rot
  (is (= (stack-from "1 2 3 rot") [2 3 1])))

(deftest lrot
  (is (= (stack-from "1 2 3 lrot") [1 3 2])))

(deftest more-complex-expression
  (is (= (stack-from "2 2 + 6 *") [24])))

(deftest colon-define
  (is (= (stack-from ": ++ [ + + ] 2 3 4 ++") [9])))

(deftest nested-calls
  (is (= (stack-from ": a [ 10 ] : b [ dup ] a b") [10 10])))

(def one-if-true  ": w [ if [ 1 ] ]")

(deftest simple-if
  (is (= (stack-from (str one-if-true " true w")) [1]))
  (is (= (stack-from (str one-if-true " false w")) []))
  (is (= (stack-from (str one-if-true " false w 99")) [99])))

(def one-or-two  ": w [ ifelse [ 1 ] [ 2 ] ]")

(deftest ifelse
  (is (= (stack-from (str one-or-two " true w")) [1]))
  (is (= (stack-from (str one-or-two " false w")) [2]))
  (is (= (stack-from (str one-or-two " false w 99" )) [99 2])))

(deftest push-string
  (is (= (stack-from "'foo'") ["foo"])))
