(ns rt.core-test
  (:require [clojure.test :refer :all]
            [rt.core :refer :all])
  (:import [rt.core Vector3 Point3 Ray Color]))
;; it is lousy that you have to import records

(def v1 (Vector3. 1 2 3))
(def v2 (Vector3. 100 200 300))
(def i (Vector3. 1 0 0))
(def j (Vector3. 0 1 0))
(def k (Vector3. 0 0 1))
(def v-zero (Vector3. 0 0 0))
(def p-zero (Point3. 0 0 0))

(deftest vector-addition
  (let [sum (Vector3. 101 202 303)]
    (is (= (v+ v1 v2) sum))))

(deftest vector-subtraction
  (let [diff (Vector3. 99 198 297)]
    (is (= (v- v2 v1) diff))))

(deftest vector-prod
  (let [prod (Vector3. 200 400 600)]
    (is (= (k*v 2 v2) prod))))

(deftest vector-dot
  (let [dot-prod (+ 100 400 900)]
    (is (= dot-prod (dot v1 v2)))))

(deftest vec-mag-squared
  (let [sq-mag (+ 1 4 9)]
    (is (= (squared-mag v1) sq-mag))))

(deftest vector-mag
  (let [res (mag v1)]
    (is (and (> res 3) (< res 4)))))

(deftest cross-prod
  (is (= (cross j k) i))
  (is (= (cross k i) j))
  (is (= (cross i j) k)))

(deftest vector-norm
  (is (= (normalize v-zero) v-zero))
  (is (= (normalize (Vector3. 2 0 0)) i)))

(deftest neg-test
  (is (= (neg i) (Vector3. -1 0 0))))

(deftest point-addition
  (let [p (Point3. 1 2 3)]
    (is (= p (p+v p-zero v1)))))

(deftest position-test
  (let [t 2
        r (Ray. p-zero v1)]
    (is (= (Point3. 2 4 6)
           (position-at-time r t)))))

(deftest root-test
  (is (= (roots 1 4 3)
         [-1.0 -3.0])))

(deftest brighten-test
  (is (= (k*col 0.5 red)
         (Color. 0.5 0.0 0.0))))

(deftest add-col-test
  (is (= (col+ blue
               (col+ red green))
         white)))

(deftest clamp-test
  (is (= red
         (clamp (Color. 2.0 0.0 0.0)))))

(deftest color-combine-test
  (is (= (col* red green) black)))

(run-tests 'rt.core-test)
