(ns cloforth.execute
  [:require
   [cloforth.environment :as env]])

(defn execute-primitive [program ip env]
  (if-not (map? env)
    (throw (Exception. "not a map")))
  (if (> ip 0)
    (env/pop-frame env)
    (let [new-env (program (env/inc-ip env))]
      new-env)))

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

(defn execute [env]
  (when (env/peek-frame env)
    (let [result (execute-one env)]
      #_(println "execute: result" (dissoc result :dictionary))
      #_(flush)
      (when result
        (lazy-seq (cons env (execute result)))))))

(defn execute-program [env program]
  (cons env (execute (env/push-frame env (env/make-frame program)))))

