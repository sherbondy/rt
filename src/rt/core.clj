(ns rt.core
  (:require [clojure.core.typed :as typed
             :refer [ann ann-datatype ann-many ann-protocol ann-record
                     def-alias def> defprotocol>
                     letfn>]]
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

;; now I am feeling the pain, should really be using multimethods to define addition
;; maybe I should make a macro for commutative operations?

;; does origin make more sense than base?
(ann-record Ray [base :- Point3  direction :- Vector3])
(defrecord Ray [base direction])

(def-alias Time Number)
(ann position-at-time [Ray Time -> Point3])
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
(ann xor [Boolean Boolean -> Boolean])
(defn xor [a b]
  (condp = a
    true (not b)
    false b))

(ann negate [Number -> Number])
(defn negate [n]
  (- 0 n))

;; oh, could core/typed do (runtime?) bounds enforcing on numbers?
;; that'd be incredible
(ann-record Color [r :- Number, g :- Number, b :- number])
(defrecord Color [r g b])

(ann-many Color
          red green blue white
          black mid-grey nearly-white)
(def red          (Color. 1.0 0.0 0.0))
(def green        (Color. 0.0 1.0 0.0))
(def blue         (Color. 0.0 0.0 1.0))
(def white        (Color. 1.0 1.0 1.0))
(def black        (Color. 0.0 0.0 0.0))
(def mid-grey     (Color. 0.5 0.5 0.5))
(def nearly-white (Color. 0.8 0.8 0.8))

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
;; specify that it is associative and commutative, and get an efficent
;; variadic version for free...
(ann col* [Color Color -> Color])
(defn col* [{:keys [r g b]} {r2 :r g2 :g b2 :b}]
  (Color. (* r r2) (* g g2) (* b b2)))


;; procedural textures are functions of points that emit materials
(ann-record Material [color :- Color, reflectivity :- Number, diffuseness :- Number])
(defrecord Material [color reflectivity diffuseness])

(ann-many [Point3 -> Material]
          flat-red shiny-red semi-shiny-green shiny-white checked-matt)

(defn flat-red [_] (Material. red 0.0 1.0))
(defn shiny-red [_] (Material. red 0.3 0.9))
(defn semi-shiny-green [_] (Material. green 0.5 0.7))
(defn shiny-white [_] (Material. white 0.3 0.9))

(def> matt-size :- Number 20.0)

(ann matt-dim-even? [Number -> Boolean])
(defn matt-dim-even? [dim]
  (even? (int (/ dim matt-size))))

(defn checked-matt [{:keys [x y z]}]
  (let [x-even? (matt-dim-even? x)
        y-even? (matt-dim-even? y)
        z-even? (matt-dim-even? z)]
    (if (xor (xor x-even? y-even?) z-even?)
      (Material. white 0.0 1.0)
      (Material. black 0.0 1.0))))


;; SHAPES

(ann-record Intersection [normal :- Vector3, point :- Point3, ray :- Ray, material :- Material])
(defrecord Intersection [normal point ray material])

(def> epsilon :- Number 0.001)

(def-alias TimeIntersectVec (IPersistentVector Time Intersection))

(ann-protocol Shape
  intersect [Shape Ray -> TimeIntersectVec])
(defprotocol> Shape
  (intersect [shape ray]))

(def-alias MaterialFn [Point3 -> Material])

(ann-datatype Sphere [center :- Point3, radius :- Number, material-fn :- MaterialFn])
(deftype Sphere [center radius material-fn]
  Shape
  (intersect [this {:keys [base direction] :as ray}]
    (let [a (squared-mag direction)
          b (* 2 (dot direction (- base center)))
          c (- (squared-mag (- base center))
               (* radius radius))
          times (filter #(> % epsilon) (roots a b c))]
      (letfn> [normal-at-time :- [Time -> Vector3]
               (normal-at-time [t]
                 (normalize (- (position-at-time t ray) center)))
               intersection-at-time :- [Time -> Intersection]
               (intersection-at-time [t]
                 (let [pos (position-at-time ray t)]
                  (Intersection. (normal-at-time t) pos ray (material-fn pos))))]
             (map #([% (intersection-at-time %)]) times)))))

;; Plane is defined by a normal (its a 2 sided plane though) and a distance.
;; The plane coincident with y=5 and normal (0,0,1) has distance -5.
(ann-datatype Plane [normal :- Vector3, distance :- Number, material-fn :- MaterialFn])
(deftype Plane [normal distance material-fn]
  Shape
  (intersect [this {:keys [base direction] :as ray}]
    (let [nnorm (normalize normal)
          vd    (dot nnorm direction)
          v0    (negate (+ (dot nnorm base) distance))]
      (if (= vd 0.0)
        []
        (let [t (/ v0 vd)
              hit-point (position-at-time ray t)]
          (if (> t epsilon)
            [t (Intersection.
                (if (> vd 0) (neg normal) normal)
                hit-point ray (material-fn hit-point))]
            []))))))

;; might this get an empty vector as the argument? In which case the result would be nil.
(ann closest [(IPersistentVector TimeIntersectVec) -> (U Intersection nil)])
;; I guess we do this trickery with reduce to support select-nearest when xs is empty
(defn closest [xs]
  (letfn> [select-nearest :- [TimeIntersectVec TimeIntersectVec -> TimeIntersectVec]
           (select-nearest [[t1 i1] [t2 i2]]
             (if (< t1 t2) [t1 i1] [t2 i2]))]
    (second (reduce select-nearest (first xs) (rest xs)))))



;; LIGHTS

(ann point-is-lit? [Point3 Point3 -> Boolean])
(defn point-is-lit? [point light-pos shapes]
  "Helper to calculate the diffuse light at the surface normal, given
   the light direction (from light source to surface)"
  (let [path (- light-pos point)
        time-at-light (mag path)
        ray (Ray. point (normalize path))
        hits (concat (map #(intersect % ray) shapes))
        times (map first hits)]
    (if (empty? times)
      true
      (> (min times) time-at-light))))

(ann diffuse-coeff [Vector3 Vector3 -> Number])
(defn diffuse-coeff [light-dir normal]
  "Helper to calculate the diffuse light at the surface normal, given
   the light direction (from light source to surface)"
  (max 0.0 (negate (dot (normalize light-dir) (normalize normal)))))

(ann-protocol Light
  local-light [Light Intersection -> Color])
(defprotocol> Light
  (local-light [light intersection]))

(ann-datatype Directional [direction :- Vector3, color :- Color])
(deftype Directional [direction light-color]
  Light
  (local-light [light {:keys [normal material] :as intersection}]
    (let [mixed-color (col* (:color material) light-color)
          diffuse     (k*col (* (diffuse-coeff direction normal)
                                (:diffuseness material))
                             mixed-color)])))

(ann-datatype Spotlight [point :- Point3, color :- Color])
(deftype Spotlight [light-pos light-color]
  Light
  (local-light [light {:keys [normal point material] :as intersection}]
    (let [mixed-color (col* (:color material) light-color)
          diffuse     (k*col (* (diffuse-coeff (- point light-pos) normal)
                                (:diffuseness material))
                             mixed-color)]
      (if (point-is-lit? point light-pos)
        diffuse
        black))))
