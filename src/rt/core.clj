(ns rt.core
  (:require [clojure.core.typed :as typed
             :refer [ann ann-datatype ann-form ann-record check-ns tc-ignore]]
            [clojure.math.numeric-tower :as math]))

"""
This is a ray tracer written in Clojure/Script.
It attempts to be a simple port of [Htrace](http://nobugs.org/developer/htrace/htrace.hs)
My goals in writing it are initially to:
- Explore core.typed
- Write a simple system and test as I go.
Later:
- Write an *explainable*, visual program
- Use a pipeline so people can *explore* and see intermediate outputs...
- Try interesting optimizations, potentially using glsl, web workers, or both.
- Try doing client-side previews with server-side rendering, sharing most of the clojure code.
"""

(ann-record Vec3 [x :- Number y :- Number z :- Number])
(defrecord Vec3 [x y z])

(ann v+ [Vec3 Vec3 -> Vec3])
(defn v+ [{:keys [x y z]} {a :x b :y c :z}]
  (Vec3. (+ x a) (+ y b) (+ z c)))

(ann v- [Vec3 Vec3 -> Vec3])
(defn v- [{:keys [x y z]} {a :x b :y c :z}]
  (Vec3. (- x a) (- y b) (- z c)))

(ann c*v [Number Vec3 -> Vec3])
(defn c*v [c {:keys [x y z]}]
  (Vec3. (* c x) (* c y) (* c z)))

(ann dot [Vec3 Vec3 -> Number])
(defn dot [{:keys [x y z]} {a :x b :y c :z}]
  (+ (* x a) (* y b) (* z c)))

(ann squared-mag [Vec3 -> Number])
(defn squared-mag [v]
  (dot v v))

(ann mag [Vec3 -> Number])
(defn mag [v]
  (math/sqrt (squared-mag v)))

;; is the implementation in Htrace wrong, or are they
;; using a left-handed coordinate system?
(ann cross [Vec3 Vec3 -> Vec3])
(defn cross [{:keys [x y z]} {a :x b :y c :z}]
  (Vec3. (- (* y c)
            (* z b))
         (- (* z a)
            (* x c))
         (- (* x b)
            (* y a))))

(ann normalize [Vec3 -> Vec3])
(defn normalize [v]
  (let [magv (mag v)]
    (if (not= magv 0)
      (c*v (/ 1 magv) v)
      (Vec3. 0 0 0))))

(ann neg [Vec3 -> Vec3])
(defn neg [v]
  (c*v -1 v))

;; how do I define an alias?
;;(def Point3 Vec3)
;;(Point3. 1 2 3)
