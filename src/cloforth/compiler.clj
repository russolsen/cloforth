(ns cloforth.compiler
  [:require [cloforth.environment :as env]
            [cloforth.primitives :as prims]
            [cloforth.tokenizer :as tok]])


(defn execute-primitive [program ip env]
  (if-not (map? env) (throw (Exception. "not a map")))
  (if (< ip 1)
    (env/pop-frame (program env))))

(defn execute-one-program-instruction [program ip env]
  (if (>= ip (count program))
    (env/pop-frame env)
    (let [f (program ip)]
      (f (env/inc-ip env)))))

(defn execute-one
  "Execute the next program step, return the new env.
   Pops the return stack if there is nothing left to do."
  [env]
  (let [frame (env/peek-frame env)
        ip (:ip frame)
        program (:program frame)]
    (when frame
      (if (fn? program)
        (execute-primitive program ip env)
        (execute-one-program-instruction program ip env)))))

;;(defn execute-one [env] (let [result (xx-execute-one env)] (println "exectute one::::=>" result)))

(defn execute [env]
  (when (env/peek-frame env)
    (let [next-state (execute-one env)]
      (when next-state
        #_(println "===> Next state:" next-state)
        (lazy-seq (cons next-state (execute next-state)))))))

(defn execute-program [env program]
  #_(println "execute program:" program)
  (cons env (execute (env/push-frame env (env/make-frame program)))))


#_(defn execute-collection [program env]
  (if (>= (:ip env) (count program))
    env
    (do
      #_(println "in collection loop ip: " (:ip env))
      (recur program (env/inc-ip (inner (program (:ip env)) env))))))


(defn pr-program [program]
  (if (coll? program)
    (doseq [p program] (pr-program p))
    (if (:description (meta program))
      (print (:description (meta program)))
      (print (str program " ")))))

#_(defn inner [program env]
  #_(pr-program program)
  #_(println)
  (cond
   (fn? program)   (program env)
   (coll? program) (with-reset-ip #(execute-collection program %) env)
   :else           (throw (str "Dont know what to do with" program))))

(declare compile-statement)

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
      [(partial env/jump (count false-part))]
      false-part))))

(defn compile-push [value]
  (with-meta
    (fn [env] (env/stack-push env value))
    {:description (str "Push " value)}))

(defn compile-word-reference [name value]
  (with-meta
    value
    {:description (str "Call to [" name "]")}))

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