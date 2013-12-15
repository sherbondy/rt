(ns rt.core-test
  (:require [clojure.test :refer :all]
            [rt.core :refer :all])
  (:import [rt.core Vec3]))

(def v1 (Vec3. 1 2 3))
(def v2 (Vec3. 100 200 300))
(def i (Vec3. 1 0 0))
(def j (Vec3. 0 1 0))
(def k (Vec3. 0 0 1))
(def v-zero (Vec3. 0 0 0))

(deftest vector-addition
  (let [sum (Vec3. 101 202 303)]
    (is (= (v+ v1 v2) sum))))

(deftest vector-subtraction
  (let [diff (Vec3. 99 198 297)]
    (is (= (v- v2 v1) diff))))

(deftest vector-prod
  (let [prod (Vec3. 200 400 600)]
    (is (= (c*v 2 v2) prod))))

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
  (is (= (normalize (Vec3. 2 0 0)) i)))

(deftest neg-test
  (is (= (neg i) (Vec3. -1 0 0))))

(run-tests 'rt.core-test)
