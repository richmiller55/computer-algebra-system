(ns computer-algebra-system.integration-test
  (:require [clojure.test :refer :all]
            [computer-algebra-system.integration :refer :all]))

(defn- approx-equal? [a b tolerance]
  (< (Math/abs (- a b)) tolerance))

(deftest integrate-test
  (testing "Integration rules"
    (is (= '(+ 0 C) (integrate 0 'x)))
    (is (= '(+ (* 5 x) C) (integrate 5 'x)))
    (is (= '(+ (/ (* x x) 2) C) (integrate 'x 'x)))
    (is (= '(+ (sin x) C) (integrate '(cos x) 'x)))))

(deftest numerical-integration-test
  (testing "Numerical integration"
    (let [f (fn [x] (* x x)) ; f(x) = x^2
          a 0.0
          b 1.0
          n 1000
          tolerance 0.001
          exact-integral (/ 1.0 3.0)] ; integral of x^2 from 0 to 1 is 1/3
      (testing "Trapezoidal rule"
        (let [result (numerical-integrate-trapezoidal f a b n)]
          (is (approx-equal? result exact-integral tolerance))))
      (testing "Simpson's rule"
        (let [result (numerical-integrate-simpson f a b n)]
          (is (approx-equal? result exact-integral tolerance)))))))
