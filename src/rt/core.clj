(ns rt.core
  (:require [clojure.core.typed :as typed
             :refer [ann ann-many ann-record]]
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
I wish my text editor rendered markdown in comments!

Every subsection in Htrace could probably be its own namespaced module...
"""

(ann-record Vector3 [x :- Number, y :- Number, z :- Number])
(defrecord Vector3 [x y z])

(ann v+ [Vector3 Vector3 -> Vector3])
(defn v+ [{:keys [x y z]} {a :x b :y c :z}]
  (Vector3. (+ x a) (+ y b) (+ z c)))

(ann v- [Vector3 Vector3 -> Vector3])
(defn v- [{:keys [x y z]} {a :x b :y c :z}]
  (Vector3. (- x a) (- y b) (- z c)))

;; k is for konstant!
(ann k*v [Number Vector3 -> Vector3])
(defn k*v [c {:keys [x y z]}]
  (Vector3. (* c x) (* c y) (* c z)))

(ann dot [Vector3 Vector3 -> Number])
(defn dot [{:keys [x y z]} {a :x b :y c :z}]
  (+ (* x a) (* y b) (* z c)))

(ann squared-mag [Vector3 -> Number])
(defn squared-mag [v]
  (dot v v))

;; careful: sqrt is not consistent across hosts...
;; js = Math/sqrt
(ann mag [Vector3 -> Number])
(defn mag [v]
  (math/sqrt (squared-mag v)))

;; is the implementation in Htrace wrong, or are they
;; using a left-handed coordinate system?
;; I did the math by hand and got - their x and z coords...
(ann cross [Vector3 Vector3 -> Vector3])
(defn cross [{:keys [x y z]} {a :x b :y c :z}]
  (Vector3. (- (* y c)
            (* z b))
         (- (* z a)
            (* x c))
         (- (* x b)
            (* y a))))

(ann normalize [Vector3 -> Vector3])
(defn normalize [v]
  (let [magv (mag v)]
    (if (not= magv 0)
      (k*v (/ 1 magv) v)
      (Vector3. 0 0 0))))

(ann neg [Vector3 -> Vector3])
(defn neg [v]
  (k*v -1 v))

;; how do I define an alias for a record?
;; wish I could just do:
;;(def Point3 Vector3), or something along those lines...
;;(Point3. 1 2 3)
;; I can define a union type for Vector3 and Point3...
(ann-record Point3 [x :- Number, y :- Number, z :- Number])
(defrecord Point3 [x y z])

;; could I just reuse v+ with some core.typed cleverness?
(ann p+v [Point3 Vector3 -> Point3])
(defn p+v [{:keys [x y z]} {a :x b :y c :z}]
  (Point3. (+ x a) (+ y b) (+ z c)))

;; does origin make more sense than base?
(ann-record Ray [base :- Point3  direction :- Vector3])
(defrecord Ray [base direction])

;; now I am feeling the pain, should really be using multimethods...
;; maybe I should make a macro for commutative operations?
(ann position-at-time [Ray Number -> Point3])
(defn position-at-time [{:keys [base direction]} time]
  (p+v base (k*v time direction)))

(ann roots [Number Number Number -> (IPersistentVector Number)])
(defn roots [a b c]
  (let [discriminant (- (* b b) (* 4 a c))]
    (if (< discriminant 0)
      []
      (let [negative-b (* -1 b)
            root (math/sqrt discriminant)]
        [(* 0.5 (+ negative-b root))
         (* 0.5 (- negative-b root))]))))

;; time to bring in the core match?
(ann xor [Bool Bool -> Bool])
(defn xor [a b]
  (condp = a
    true (not b)
    false b))


;; oh, could core/typed do (runtime?) bounds enforcing on numbers?
;; that'd be incredible
(ann-record Color [r :- Number, g :- Number, b :- number])
(defrecord Color [r g b])

(ann-many Color
          red green blue white
          black midgrey nearlywhite)
(def red         (Color. 1.0 0.0 0.0))
(def green       (Color. 0.0 1.0 0.0))
(def blue        (Color. 0.0 0.0 1.0))
(def white       (Color. 1.0 1.0 1.0))
(def black       (Color. 0.0 0.0 0.0))
(def midgrey     (Color. 0.5 0.5 0.5))
(def nearlywhite (Color. 0.8 0.8 0.8))

(ann k*col [Number Color -> Color])
(defn k*col [k {:keys [r g b]}]
  (Color. (* k r) (* k g) (* k b)))

(ann col+ [Color Color -> Color])
(defn col+ [{:keys [r g b]} {r2 :r g2 :g b2 :b}]
  (Color. (+ r r2) (+ g g2) (+ b b2)))

;; couldn't help myself :P
(ann clamp-fn [Number Number -> (Fn [Number -> Number])])
(defn clamp-fn [f-min f-max]
  (fn [f]
    (min (max f f-min) f-max)))

(ann clamp-0-1 [Number -> Number])
(def clamp-0-1 (clamp-fn 0.0 1.0))

(ann clamp [Color -> Color])
(defn clamp [{:keys [r g b]}]
  (Color. (clamp-0-1 r)
          (clamp-0-1 g)
          (clamp-0-1 b)))

;; = combine_col
;; it would be amazing to simply define a binary operation,
;; say that it is associative+commutative, and get an efficent
;; variadic version for free...
(ann col* [Color Color -> Color])
(defn col* [{:keys [r g b]} {r2 :r g2 :g b2 :b}]
  (Color. (* r r2) (* g g2) (* b b2)))
