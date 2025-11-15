(ns cas.integration
  (:require [clojure.core.match :refer [match]]
            [cas.core :refer [simplify]])) 

(defn integrate
  "Computes the indefinite integral of expr with respect to variable v."
  [expr v]
  (match [expr]

    ;; --- Base Cases and Simple Rules ---
    
    ;; Rule: Int(0 dx) = C
    [0] '(+ 0 C) ; We use 'C' as a constant of integration symbol

    ;; Rule: Int(k dx) = k*x + C  (where k is a number)
    [_ :guard number?] '(+ (* ~expr ~v) C)

    ;; Rule: Int(v dx) = v^2 / 2 + C 
    ;; Note: this is a specific case of the power rule
    [_ :guard #(= % v)] '(+ (/ (* ~v ~v) 2) C)

    ;; Rule: Int(x^n dx) = x^(n+1) / (n+1) + C
    ;; This requires more complex pattern matching for power expressions:
    [('clojure.core/pow ?var ?n)]
    (if (= ?var v)
      '(+ (/ (pow ~v ~(+ ?n 1)) ~(+ ?n 1)) C)
      (throw (Exception. (str "Cannot integrate power expression with respect to " v))))
    
    ;; --- Linearity Rule ---

    ;; Rule: Int(f(x) + g(x) dx) = Int(f(x) dx) + Int(g(x) dx)
    [('clojure.core/+ ?f ?g)]
    '(+ ~(integrate ?f v) ~(integrate ?g v))

    ;; Rule: Int(k * f(x) dx) = k * Int(f(x) dx) (where k is a constant)
    [('clojure.core/* ?k ?f)]
    (if (number? ?k)
      '(* ~?k ~(integrate ?f v))
      (throw (Exception. (str "Cannot integrate product for " expr))))
    
    ;; --- Standard Function Rules ---
    
    ;; Rule: Int(cos(x) dx) = sin(x) + C
    [('cos ?arg)]
    (if (= ?arg v)
      '(+ (sin ~v) C)
      (throw (Exception. "Chain rule for integration not implemented yet.")))
    
    ;; If no direct rule matches
    :else (throw (Exception. (str "No integration rule found for " expr " with respect to " v))))))

(defn substitute [expr var-to-replace replacement-expr]
  (match [expr]
    [_ :guard number?] expr
    [_ :guard symbol?] (if (= expr var-to-replace) replacement-expr expr)
    [('?op & ?args)] (apply list ?op (map #(substitute % var-to-replace replacement-expr) ?args))
    :else expr))

(defn integrate-by-substitution [expr v]
  ;; A simple example heuristic: look for u=ax+b type substitutions
  (comment 
    ;; This part is highly complex in practice. The logic would involve:
    ;; 1. Picking a subexpression 'u'.
    ;; 2. Calculating du/dx using 'differentiate'.
    ;; 3. Attempting to rewrite the original integral in terms of 'u' and 'du'.
    ;; 4. Recursively calling 'integrate' on the new expression.
    ;; 5. Substituting back the original variable.
    )
  (throw (Exception. "Integration by substitution is a complex AI problem!")))

;; Add more rules to your existing simplify function in `my-cas.core`
(defn simplify [...]
  (match [expr]
    ... ; existing rules

    ;; Rule: (+ ?x C) => (+ C ?x) canonicalization for display
    [('clojure.core/+ ?x 'C)] '(+ 'C ~?x)

    ;; Rule: (/ ?x 1) => ?x
    [('clojure.core// ?x 1)] ?x

    ;; Make sure numerical operations evaluate constants immediately
    [('clojure.core/+ ?a ?b :guard #(and (number? ?a) (number? ?b)))] (+ ?a ?b)
    [('clojure.core/* ?a ?b :guard #(and (number? ?a) (number? ?b)))] (* ?a ?b)
    
    ... ; default case
    ))

;; Example function (f(x) = x^3) as a Clojure function
(defn f [x]
  (* x x x)) 


(defn numerical-integrate-trapezoidal
  "Numerically integrates a function f from a to b using the trapezoidal rule 
   with n subintervals."
  [f a b n]
  (let [h (/ (- b a) n)
        ;; Generate the x points (x0 to xn)
        x-points (map #(+ a (* % h)) (range (inc n))) 
        ;; Calculate the function values f(xi)
        y-points (map f x-points)
        
        ;; Get the first and last values (f(x0) and f(xn))
        first-val (first y-points)
        last-val (last y-points)
        
        ;; Sum the intermediate values (f(x1) to f(xn-1)) and multiply by 2
        ;; Use `drop 1` and `drop-last 1` to exclude first/last points
        intermediate-sum (* 2 (apply + (drop-last 1 (drop 1 y-points))))]
    
    ;; Apply the main formula
    (* (/ h 2) (+ first-val intermediate-sum last-val))))

(defn numerical-integrate-simpson
  "Numerically integrates a function f from a to b using Simpson's rule 
   with n (must be even) subintervals."
  [f a b n]
  (when (odd? n)
    (throw (IllegalArgumentException. "n must be an even number for Simpson's Rule.")))
  
  (let [h (/ (- b a) n)
        x-points (map #(+ a (* % h)) (range (inc n)))
        y-points (map f x-points)
        
        ;; Sum the even-indexed points (excluding first/last) and multiply by 2
        sum-even (* 2 (apply + (take-nth 2 (drop 2 (drop-last 1 y-points))))) ; f(x2), f(x4), ...
        ;; Sum the odd-indexed points and multiply by 4
        sum-odd  (* 4 (apply + (take-nth 2 (drop 1 (drop-last 0 y-points))))) ; f(x1), f(x3), ...
        
        first-val (first y-points)
        last-val (last y-points)]
    
    ;; Apply the main formula
    (* (/ h 3) (+ first-val sum-even sum-odd last-val))))

(defn handle-numerical-integration [f-expr v a b n]
  ;; Need to convert the symbolic expression f-expr into a runnable Clojure function
  ;; This can be complex, involving macro expansion or evaluation in a safe context.
  ;; For simplicity, we assume we can safely `eval` it:
  (let [func (eval `(fn [~v] ~f-expr))]
    (println "Using Trapezoidal Rule:")
    (numerical-integrate-trapezoidal func a b n)
    (println "Using Simpson's Rule:")
    (numerical-integrate-simpson func a b n)))

;; Example of how a user might call this in the extended REPL (conceptually):
;; > (integrate-numerical (* x x x) x 0 1 1000)
;; => 0.249999... (close to the exact 0.25)
