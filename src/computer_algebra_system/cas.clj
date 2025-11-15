(ns cas.core
  (:require [clojure.core.match :refer [match]]))

(defn simplify
  "Recursively simplifies an expression using algebraic rules."
  [expr]
  (match [expr]
    ;; --- Base Cases ---
    [_ :guard number?] expr
    [_ :guard symbol?] expr
    
    ;; --- Simplification Rules ---
    
    ;; Rule: (+ ?x 0) => ?x
    [('clojure.core/+ ?x 0)] (simplify ?x)
    ;; Rule: (+ 0 ?x) => ?x
    [('clojure.core/+ 0 ?x)] (simplify ?x)
    
    ;; Rule: (* ?x 1) => ?x
    [('clojure.core/* ?x 1)] (simplify ?x)
    ;; Rule: (* 1 ?x) => ?x
    [('clojure.core/* 1 ?x)] (simplify ?x)
    
    ;; Rule: (* ?x 0) => 0
    [('clojure.core/* ?x 0)] 0
    ;; Rule: (* 0 ?x) => 0
    [('clojure.core/* 0 ?x)] 0

    ;; --- Default/Recursive Case ---
    ;; If it's a list (an operation), simplify its arguments recursively
    [('?op & ?args)] (let [simplified-args (map simplify ?args)]
                       (apply list ?op simplified-args))

    ;; If no pattern matches
    :else expr))

;; Example Usage:
(def expr-to-simplify '(+ (* 5 x 0) (* 1 y) 0))
;; Expected result: y
(println "Simplified expression:" (simplify expr-to-simplify))

(defn differentiate
  "Computes the derivative of expr with respect to variable v."
  [expr v]
  (match [expr]
    ;; d/dx(c) = 0
    [_ :guard number?] 0

    ;; d/dx(v) = 1
    [_ :guard #(= % v)] 1

    ;; d/dx(u) = 0 if u is a different variable
    [_ :guard symbol?] 0

    ;; d/dx(u + w) = d/dx(u) + d/dx(w) - Sum Rule
    [('clojure.core/+ u w)]
    '(+ ~(differentiate u v) ~(differentiate w v))

    ;; d/dx(u * w) = u * d/dx(w) + w * d/dx(u) - Product Rule
    [('clojure.core/* u w)]
    '(+ (* ~u ~(differentiate w v))
      (* ~w ~(differentiate u v)))
    
    ;; d/dx(sin(u)) = cos(u) * d/dx(u) - Chain Rule for sin
    [('sin u)]
    '(* (cos ~u) ~(differentiate u v))

    :else (throw (Exception. (str "Cannot differentiate " expr)))))


;; Example Usage:
(def func '(+ (* 2 x) (sin x)))
;; Expected raw result: (+ (+ (* 2 1) (* x 0)) (* (cos x) 1))
(println "Derivative (raw):" (differentiate func 'x)) 

;; The beauty of the framework is that we can pipe the result into our simplifier:
;; Expected simplified result: (+ 2 (cos x))
(println "Derivative (simplified):" (simplify (differentiate func 'x)))

(defn ast->str [expr]
  ;; This is a naive implementation; a real one would handle operator precedence.
  (match [expr]
    [_ :guard number?] (str expr)
    [_ :guard symbol?] (name expr)
    [('?op & ?args)] (str "(" (name ?op) " " 
                           (clojure.string/join " " (map ast->str ?args)) 
                           ")")
    :else (str expr)))

;; The final shell could be a simple REPL loop:
(defn run-cas []
  (println "Welcome to My Clojure CAS!")
  (loop []
    (print "> ")
    (flush)
    (let [input (read-line)]
      (when-not (= input "exit")
        (try
          ;; In a real CAS, we would have a full parser here, 
          ;; but for this example, we use `read-string` for simplicity.
          (let [expr (read-string input)
                result (simplify expr)] ; Simplify after evaluation/differentiation
            (println "=>" (ast->str result)))
          (catch Exception e
            (println "Error:" (.getMessage e))))
        (recur)))))

;; To start the system:
;; (run-cas)

