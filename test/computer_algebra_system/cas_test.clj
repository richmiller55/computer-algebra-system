(ns computer-algebra-system.cas-test
  (:require [clojure.test :refer :all]
            [computer-algebra-system.cas :refer :all]))

(deftest simplify-test
  (testing "Simplification rules"
    (is (= 'x (simplify '(+ x 0))))
    (is (= 'x (simplify '(+ 0 x))))
    (is (= 'x (simplify '(* x 1))))
    (is (= 'x (simplify '(* 1 x))))
    (is (= 0 (simplify '(* x 0))))
    (is (= 0 (simplify '(* 0 x))))
    (is (= 'y (simplify '(+ (* 5 x 0) (* 1 y) 0))))))

(deftest differentiate-test
  (testing "Differentiation rules"
    (is (= 0 (differentiate 5 'x)))
    (is (= 1 (differentiate 'x 'x)))
    (is (= 0 (differentiate 'y 'x)))
    (is (= '(+ 1 0) (simplify (differentiate '(+ x y) 'x))))
    (is (= '(+ (* x 0) (* 1 y)) (simplify (differentiate '(* x y) 'x))))
    (is (= '(* (cos x) 1) (simplify (differentiate '(sin x) 'x))))
    (is (= '(+ 2 (cos x)) (simplify (differentiate '(+ (* 2 x) (sin x)) 'x))))))
