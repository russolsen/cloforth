(ns cloforth.compiler
  [:require [cloforth.environment :as env]
            [cloforth.primitives :as prims]
            [cloforth.tokenizer :as tok]])


(defn pr-program [program]
  (if (coll? program)
    (doseq [p program] (pr-program p))
    (if (:description (meta program))
      (print (:description (meta program)))
      (println (str program " ")))))


(defn execute-primitive [program ip env]
  #(println "exec prim:" program)
  (if-not (map? env)
    (throw (Exception. "not a map")))
  (if (> ip 0)
    (env/pop-frame env)
    (let [new-env (program (env/inc-ip env))]
      new-env)))

(defn execute-one-program-instruction [program ip env]
  #_(println "exec one prog ins:" program ip)
  (if (>= ip (count program))
    (env/pop-frame env)
    (let [f (program ip)]
      (f (env/inc-ip env)))))

(defn execute-one
  "Execute the next program step, return the new env.
   Pops the return stack if there is nothing left to do."
  [env]
  #_(println "execute-one:" (:frame-stack env))
  (let [frame (env/peek-frame env)
        ip (:ip frame)
        program (:program frame)]
    (when frame
      (if (fn? program)
        (execute-primitive program ip env)
        (execute-one-program-instruction program ip env)))))

(defn execute [env]
  #_(println "Execute:" (env/peek-frame env))
  (when (env/peek-frame env)
    (let [result (execute-one env)]
      #_(println "execute: result" (dissoc result :dictionary))
      #_(flush)
      (when result
        (lazy-seq (cons env (execute result)))))))

(defn execute-program [env program]
  #_(println "execute program:")
  #_(pr-program program)
  (flush)
  (cons env (execute (env/push-frame env (env/make-frame program)))))


(declare compile-statement)


(defn compile-word-reference [name value]
  (with-meta
    (fn [env]
      (let [new-env (env/push-frame env (env/make-frame value))]
        #_(println "new env:" (dissoc  new-env :dictionary))
        new-env))
    {:description (str "Call to [" name "]")}))

(defn compile-branch [n]
  (with-meta
    (partial env/branch n)
    {:description (str "Branch " n)}))

(defn compile-jump [n]
  (with-meta
    (partial env/jump n)
    {:description (str "Jump " n)}))

;; experimental
(defn compile-while [r dictionary]
  (let [condition (compile-statement r dictionary)
        body (compile-statement r dictionary)
        len-cond (count condition)
        len-body (count body)]
    (vec (concat
          condition
          [prims/primitive-not (compile-branch (+ len-body 1))]
          body
          [(compile-jump (- 0 len-body 3 len-cond))]))))

(defn- compile-if [r dictionary]
  (let [body (compile-statement r dictionary)]
    (vec (concat [prims/primitive-not (partial env/branch (count body))] body))))

(defn- compile-ifelse [r dictionary]
  (let [true-part  (compile-statement r dictionary)
        false-part (compile-statement r dictionary)]
    (vec
     (concat
      [prims/primitive-not (partial env/branch (inc  (count true-part)))]
      true-part
      [(fn [env] (env/jump env (count false-part)))]
      false-part))))

(defn compile-push [value]
  (with-meta
    (fn [env] (env/stack-push env value))
    {:description (str "Push " value)}))

(defn- compile-word 
  "Compile the given word, returning either a function or a vector of functions"
  [r dictionary text]
  (cond
    (= "if" text) (compile-if r dictionary)
    (= "ifelse" text) (compile-ifelse r dictionary)
    (= "while" text) (compile-while r dictionary)
    (dictionary text) (compile-word-reference text (dictionary text))
    (tok/to-int text) (compile-push (tok/to-int text))
    :else (println "Don't know what to do with" text)))

(defn- compile-token 
  "Compile the given token, returning a vector of functions"
  [r dictionary token]
  (let [type (:type token)
        text (:text token)]
    (case type
      :string [(compile-push text)]
      :word (compile-word r dictionary text)
      :eof nil
      (println "don't know what to do with" text))))

(defn- compile-until 
  "Keep compiling words until f-done? is true, returns modified result vector"
  [r dictionary f-done? result]
  (let [token (tok/get-token r)]
    (if (f-done? token)
      result
      (if-let [compiled (compile-token r dictionary token)]
        (recur
         r
         dictionary
         f-done?
         (if (coll? compiled) (vec (concat result compiled)) (vec (conj result compiled))))
        result))))

(defn- compile-compound 
  "Compile a compound word (i.e. [ w w w ]), returns vector"
  [r dictionary]
  (compile-until
    r
    dictionary
    (fn [token] (or (= (:type token) :eof) (= (:type token) :r-bracket)))
    []))
 
(defn compile-statement [r dictionary]
  #_(println "compile statement: " r)
  (let [token (tok/get-token r)
        text (:text token)]
    (case (:type token)
      :eof []
      :string (fn [env] (env/stack-push env text))
      :l-bracket (compile-compound r dictionary)
      (compile-token r dictionary token))))

(defn primitive-compile [env]
  (let [dictionary (:dictionary env)
        r (:in env)
        body (compile-statement r dictionary)]
    (env/stack-push env body)))

(defn primitive-print-compiled [env]
  (let [p (env/stack-peek env)]
    (pr-program p)
    (env/stack-peek env)))

(defn primitive-gettok [{r :in :as env}]
  (let [token (tok/get-token r)]
    (env/stack-push env (:text token))))