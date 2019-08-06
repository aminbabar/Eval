;; Amin Babar
;; 10/29/2018
;; Home-work 6 - Clojure
;; evaluator implementing a small subset of Clojure, allowing you to run a basic Clojure REPL without using the built-in eval function
;; Collaborators: Amr, Shane
;; Used an online editor so might have some compiling issues with VM. Not sure. 
;; Pasted the code over here from the online editor, so I am hoping it runs.
(declare helper-func-let)
(ns project.clj)

(defn my-eval
  [state expression]
  (if (not (seq? expression))
    (if (symbol? expression)
      (assoc state :return (get (:environment state) expression))
      (assoc state :return expression))
    (let [env (:environment state)
          f (first expression)]
      (cond

      	;; FUNCTIONS:

        ;; evaluates list, cons, first and rest
        (= f 'list) (assoc state
                        :return
                        (apply list
                               (map #(:return (my-eval state %))
                                    (rest expression))))
        (= f 'cons) (assoc state
                        :return
                        (apply cons
                               (map #(:return (my-eval state %))
                                    (rest expression))))
        (= f 'first) (assoc state
                        :return
                        (apply first
                               (map #(:return (my-eval state %))
                                    (rest expression))))
        (= f 'rest) (assoc state
                        :return
                        (apply rest
                               (map #(:return (my-eval state %))
                                    (rest expression))))


        ;; implementation for functions empty?, = and println
        (= f 'empty?) (assoc state
                        :return
                        (empty? (:return (my-eval state (second expression)))))
        (= f '=) (assoc state
                        :return
                        (apply =
                               (map #(:return (my-eval state %))
                                    (rest expression))))
        (= f 'println) (assoc state
                        :return
                        (println (:return (my-eval state (second expression)))))


        ;; takes care of the +, -, * and divide operations
        (= f '+) (assoc state
                        :return
                        (apply +
                               (map #(:return (my-eval state %))
                                    (rest expression))))
        (= f '-) (assoc state
                        :return
                        (apply -
                               (map #(:return (my-eval state %))
                                    (rest expression))))
        (= f '*) (assoc state
                        :return
                        (apply *
                               (map #(:return (my-eval state %))
                                    (rest expression))))
        (= f '/) (assoc state
                        :return
                        (apply /
                               (map #(:return (my-eval state %))
                                    (rest expression))))

        ;; returns the symbol bindings map
        (= f 'environment-bindings) (assoc state
                        :return env)



        ;; SPECIAL FORMS:

        (= f 'def) {:environment (assoc env
                                        (second expression)
                                        (:return (my-eval state
                                                          (nth expression 2))))
                    :return (second expression)}

        ;; assoc second expression and the env. A new map is created for the inputs and the body
        (= f 'defn) {:environment (assoc env
                                        (second expression)
                                        {:inputs (nth expression 2) :body (last expression)})
                        :return (second expression)}


        (= f 'quote) (assoc state
                        :return
                        (second expression)



        ;; calls on the helper function
        (= f 'let) (assoc state
                        :return
                        (helper-func-let env (second expression) (last expression)))


        ;; Assoc with the 3rd value if the second exp is true, else assoc the return with the 4th
        (= f 'if) (assoc state
                        :return
                        (if (:return (my-eval state (second expression)))
                            (:return (my-eval state (nth expression 2)))
                            (:return (my-eval state (nth expression 3)))))



        ;; CITE: https://zaiste.net/merging_maps_in_clojure/
        ;; CITE: https://stackoverflow.com/questions/6135764/when-to-use-zipmap-and-when-map-vector
        ;; error message is returned if none of the bindings match
        :else (if (f env) (assoc state :return (let [new-state
                                                    {:environment (merge env
                                                    (zipmap (:inputs (f env))
                                                    (map #(:return (my-eval state %))
                                                    (rest expression))))}]
                                                    (:return (my-eval new-state (:body (f env))))))
                 (assoc state :return "Error: That function is not defined."))))))

(defn helper-func-let
;; If binding are there, the expression is evaluated, otherwise, all the bindings are assoc with the return values by calling on the function
;; recursively

 [environments bindings expression]
 (if (empty? bindings)  (:return (my-eval {:environment  environments} expression))
     (helper-func-let (assoc environments (first bindings)  (:return (my-eval {:environment environments} (second bindings)))) (rest (rest bindings)) expression)))


;; CODE FROM CLASS
;; A state is always composed of an :environment
;; and optionally a :return
;; {:environment {},
;;  :return val}

(defn repl
  "A Clojure REPL"
  ([]
   (repl {:environment {}}))
  ([state]
   (print "H]=> ")
   (flush)
   (let [line (read-line)]
     (when (not (empty? line))
       (let [new-state (my-eval state
                                (read-string line))]
         (println "Environment: " (:environment new-state))
         (println (:return new-state))
         (recur new-state))))))


(defn -main
  []
  (repl))