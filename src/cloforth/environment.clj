(ns cloforth.environment
  [:require [clojure.pprint :as pp]])

;; Debugging

(defn prn-env [e] (println (dissoc e :dictionary)))


;; Dictionary functions

(defn set! [name value env]
  (update-in env [:dictionary] assoc name value))

;; Data stack functions

(defn stack-push [env value]
  (update-in env [:stack] conj value))

(defn stack-pop-1 [env]
  (update-in env [:stack] pop))

(defn stack-pop
  ([env]
     (stack-pop-1 env))
  ([n env]
     (last (take (inc n) (iterate stack-pop-1 env)))))

(defn stack-peek [env]
  (peek (:stack env)))

(defn stack-nth [env n]
  (nth (:stack env) n))

;; Execute frame functions

(defn make-frame [program & [ip]]
  (let [ip (if ip ip 0)]
    {:program program :ip ip}))

(defn push-frame [env frame]
  (update-in env [:frame-stack] conj frame))

(defn x-pop-frame [env]
  (update-in env [:frame-stack] pop))

(defn pop-frame [env]
  (let [result (x-pop-frame env)]
    (when (nil? result)
      (println "******** nil pop frame:" env))
    result))

(defn peek-frame [env]
  (peek (:frame-stack env)))

(defn current-ip [env]
  (:ip (peek-frame env)))

(defn current-program [env]
  (:program (peek-frame env)))

(defn inc-ip [env]
;;  (println "inc ip: env:" env)
  (let [top (peek-frame env)]
    ;;(println "top:" top)
    (push-frame (pop-frame env) (update-in top [:ip] inc))))

(defn set-ip [env new-ip]
  (let [top (peek-frame env)]
    (push-frame (pop-frame env) (assoc-in top [:ip] new-ip))))

;; Jump and branch functions

(defn jump [env n]
  (let [ip (current-ip env)]
    (set-ip env (+ ip n))))

(defn recur [env]
  (set-ip env 0))

(defn branch [n env]
  (let [top (stack-peek env)
        env (stack-pop env)]
    (if top
      (jump env n)
      env)))

;; {:in <stream> :stack [1 2 3] :dictionary {<word> <def>} :frame-stack [{:program <func> :ip}]}

(defn make-env []
  {:in *in* :dictionary {} :stack '() :frame-stack []})

