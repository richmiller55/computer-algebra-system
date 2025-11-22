(ns algabra-trig
  (:require [clojure.math :as math]))


 (defn generate-sine-data [start end step]
  "Generates a sequence of [x y] points for a sine wave from start to end with a given step."
  (for [x (range start end step)]
    ;; Calculate the sine value using Java's Math/sin method through clojure.math
    ;; Math/PI is used for scaling the input correctly for a full cycle (0 to 2*PI)
    [x (math/sin x)]))

;; Example usage: generate data points for one full cycle (0 to 2*PI)
(def sine-points (generate-sine-data 0 (* 2 math/PI) 0.1))

;; Print some of the generated points (for demonstration)
;; (take 10 sine-points)
;; => ([0 0.0] [0.1 0.09983341664682815] ... )


(defn generate-cos-data [start end step]
  "Generates a sequence of [x y] points for a sine wave from start to end with a given step."
  (for [x (range start end step)]
    ;; Calculate the sine value using Java's Math/sin method through clojure.math
    ;; Math/PI is used for scaling the input correctly for a full cycle (0 to 2*PI)
    [x (math/cos x)]))

;; Example usage: generate data points for one full cycle (0 to 2*PI)
(def cos-points (generate-cos-data 0 (* 2 math/PI) 0.1))

;; Calculate the cosine of 0 radians (should be 1.0)
(comment 
(Math/cos 0)
;; => 1.0

;; Calculate the cosine of Pi radians (should be -1.0)
(Math/cos Math/PI)
;; => -1.0

;; Calculate the cosine of Pi/2 radians (should be close to 0.0)
(Math/cos (/ Math/PI 2))
;; => 6.123233995736766e-17 (very close to zero)
)

(defn generate-tangent-series [start end step]
  "Generates a lazy sequence of [x y] points for a tangent wave."
  (for [x (range start end step)]
    ;; Calculate the tangent value
    [x (math/tan x)]))

;; Example usage: Generate values from -PI/2 to PI/2 (exclusive of the ends due to asymptotes)
;; The tangent function has vertical asymptotes at odd multiples of PI/2.
;; We must avoid these points in our range to prevent errors or unexpected 'infinity' values.
(def tan-points (generate-tangent-series -1.5 1.5 0.1)) ; Use values close to -PI/2 and PI/2

(comment
;; Print the first few data points
(println "First 10 tangent data points:")
(doseq [point (take 10 tan-points)]
  (println point)))
