;; Examples of internal representation (AST):

;; Represents: x
(def variable 'x)

;; Represents: 5
(def constant 5)

;; Represents: (+ x 2)
(def addition-expr '(+ x 2))

;; Represents: (* 3 (+ x 2))
(def complex-expr '(* 3 (+ x 2)))

;; Represents: (sin y)
(def function-call '(sin y))
