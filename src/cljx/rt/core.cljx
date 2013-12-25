(ns rt.core
  #+clj(:require [clojure.core.typed :as t
                  :refer [ann ann-datatype ann-form ann-many
                          ann-protocol ann-record
                          def-alias defprotocol>
                          for>
                          AnyInteger
                          NonEmptySeq
                          NonEmptySeqable
                          check-ns]]
                 [clojure.math.numeric-tower :as math])
  #+clj(:import  [clojure.lang IPersistentVector Seqable])

  #+cljs(:require [rt.math :as math])
  #+cljs(:require-macros [rt.typed :as t
                          ;; no ann-many yet :(
                          :refer [ann ann-datatype ann-form ann-many
                                  ann-protocol ann-record
                                  def-alias defprotocol>]]))


"""
This is a ray tracer written in Clojure/Script.
It attempts to be a simple port of
[Htrace](http://nobugs.org/developer/htrace/htrace.hs).

My goals in writing it are initially to:
- Explore core.typed
- Write a simple system and test as I go.
Later:
- Write an *explainable*, visual program
- Use a pipeline so people can *explore* and see intermediate outputs...
- Try interesting optimizations, potentially using glsl,
  web workers, or both.
- Try doing client-side previews with server-side rendering,
  sharing most of the clojure code.
I wish my text editor rendered markdown in comments!

Every subsection in Htrace could probably be its own namespaced module...

Should really be using native arrays instead of records and vectors...
Maybe I should make a cljs backend for core.matrix that uses
typed array buffers as the backing store?
"""

;; maybe use ztellman's primitive-math library for .clj

;; add these annotations...
(ann ^:no-check clojure.core/not-empty
          (All [x] [(U nil (Seqable x)) ->
                    (U nil (NonEmptySeqable x))]))
(ann ^:no-check clojure.core/spit [String String -> nil])
(ann ^:no-check clojure.math.numeric-tower/sqrt [Number -> Number])
(ann ^:no-check clojure.math.numeric-tower/round [Number -> Integer])

;; what are the hazards of defining equals without also defining hashCode?
(ann-datatype Vector3 [x :- Number, y :- Number, z :- Number])
(deftype Vector3 [x y z]
  Object
  (equals [v1 v2] (and (= (.-x v1) (.-x v2))
                       (= (.-y v1) (.-y v2))
                       (= (.-z v1) (.-z v2)))))

(ann-datatype Point3 [x :- Number, y :- Number, z :- Number])
(deftype Point3 [x y z]
  Object
  (equals [p1 p2] (and (= (.-x p1) (.-x p2))
                       (= (.-y p1) (.-y p2))
                       (= (.-z p1) (.-z p2)))))

(def-alias AddPointFn
  (U [Point3  Point3 -> Vector3]
     [Vector3 Point3 -> Point3]))

(ann-protocol AddPoint3
  +p AddPointFn
  -p AddPointFn)
(defprotocol> AddPoint3
  (+p [this p])
  (-p [this p]))

(def-alias AddVectorFn
  (U [Point3  Vector3 -> Point3]
     [Vector3 Vector3 -> Vector3]))

(ann-protocol AddVector3
  +v AddVectorFn
  -v AddVectorFn)
(defprotocol> AddVector3
  (+v [this v])
  (-v [this v]))

(ann-protocol ScalarMult
  *k (All [x] [x Number -> x]))
(defprotocol> ScalarMult
  (*k [this k]))

;; why can't I access x y and z directly in extend-type?
;; this seems like a flaw in the clj language/compiler...
(extend-type Vector3
  AddPoint3
  (+p [v p] (Point3.  (+ (.-x v) (.-x p))
                      (+ (.-y v) (.-y p))
                      (+ (.-z v) (.-z p))))
  (-p [v p] (Point3.  (- (.-x v) (.-x p))
                      (- (.-y v) (.-y p))
                      (- (.-z v) (.-z p))))

  AddVector3
  (+v [v1 v2] (Vector3. (+ (.-x v1) (.-x v2))
                        (+ (.-y v1) (.-y v2))
                        (+ (.-z v1) (.-z v2))))
  (-v [v1 v2] (Vector3. (- (.-x v1) (.-x v2))
                        (- (.-y v1) (.-y v2))
                        (- (.-z v1) (.-z v2))))

  ScalarMult
  (*k [v k] (Vector3. (* (.-x v) k)
                      (* (.-y v) k)
                      (* (.-z v) k))))

(ann dot [Vector3 Vector3 -> Number])
(defn dot [v1 v2]
  (+ (* (.-x v1) (.-x v2))
     (* (.-y v1) (.-y v2))
     (* (.-z v1) (.-z v2))))

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
(defn cross [v1 v2]
  (Vector3. (- (* (.-y v1) (.-z v2))
               (* (.-z v1) (.-y v2)))
            (- (* (.-z v1) (.-x v2))
               (* (.-x v1) (.-z v2)))
            (- (* (.-x v1) (.-y v2))
               (* (.-y v1) (.-x v2)))))

(ann normalize [Vector3 -> Vector3])
(defn normalize [v]
  (let [mag-v (mag v)]
    (if (not= mag-v 0)
      (*k v (/ 1 mag-v))
      (Vector3. 0 0 0))))

(ann neg [Vector3 -> Vector3])
(defn neg [v]
  (*k v -1))

;; how do I define an alias for a record?
;; wish I could just do:
;; (defalias Point3 Vector3), or something along those lines...
;; although it is actually nice to distinguish the two
(extend-type Point3
  AddPoint3
  (+p [p1 p2] (Vector3. (+ (.-x p1) (.-x p2))
                        (+ (.-y p1) (.-y p2))
                        (+ (.-z p1) (.-z p2))))
  (-p [p1 p2] (Vector3. (- (.-x p1) (.-x p2))
                        (- (.-y p1) (.-y p2))
                        (- (.-z p1) (.-z p2))))

  AddVector3
  (+v [p v] (Point3. (+ (.-x p) (.-x v))
                     (+ (.-y p) (.-y v))
                     (+ (.-z p) (.-z v))))
  (-v [p v] (Point3. (- (.-x p) (.-x v))
                     (- (.-y p) (.-y v))
                     (- (.-z p) (.-z v))))

  ScalarMult
  (*k [p k] (Point3. (* (.-x p) k)
                     (* (.-y p) k)
                     (* (.-z p) k))))

(ann origin Point3)
(def origin (Point3. 0 0 0))

;; now I am feeling the pain, should I be using multimethods
;; to define addition?
;; maybe I should make a macro for commutative operations?

;; does origin make more sense than base?
(ann-record Ray [base :- Point3  direction :- Vector3])
(defrecord Ray [base direction])

(def-alias Time Number)
(ann position-at-time [Ray Time -> Point3])
(defn position-at-time [{:keys [base direction]} time]
  (+v base (*k direction time)))

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
    true  (not b)
    false b))

(ann negate [Number -> Number])
(defn negate [n]
  (- 0 n))

;; oh, could core/typed do (runtime?) bounds enforcing on numbers?
;; that'd be incredible
(ann-datatype Color [r :- Number, g :- Number, b :- Number])
(deftype Color [r g b]
  Object
  (equals [_ c2] (and (= r (.-r c2))
                      (= g (.-g c2))
                      (= b (.-b c2))))
  ScalarMult
  (*k [_ k] (Color. (* r k) (* g k) (* b k))))

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

(ann +col [Color Color -> Color])
(defn +col [c1 c2]
  (Color. (+ (.-r c1) (.-r c2))
          (+ (.-g c1) (.-g c2))
          (+ (.-b c1) (.-b c2))))

;; couldn't help myself :P
(ann clamp-fn [Number Number -> (Fn [Number -> Number])])
(defn clamp-fn [f-min f-max]
  (fn [f]
    (min (max f f-min) f-max)))

(ann clamp-0-1 [Number -> Number])
(def clamp-0-1 (clamp-fn 0.0 1.0))

(ann clamp [Color -> Color])
(defn clamp [color]
  (Color. (clamp-0-1 (.-r color))
          (clamp-0-1 (.-g color))
          (clamp-0-1 (.-b color))))

;; = combine_col
;; it would be amazing to simply define a binary operation,
;; specify that it is associative and commutative, and get an efficent
;; variadic version for free...
;; this is really just a dot product... maybe we should have a generic dot?
;; would be easy if we defined vectors and colors as tuples...
(ann *col [Color Color -> Color])
(defn *col [c1 c2]
  (Color. (* (.-r c1) (.-r c2))
          (* (.-g c1) (.-g c2))
          (* (.-b c1) (.-b c2))))

(ann sum-colors [(Seqable Color) -> Color])
(defn sum-colors [colors]
  (reduce +col black colors))

(ann col->vec255 [Color -> (IPersistentVector Integer)])
(defn col->vec255 [color]
  [(math/round (* (.-r color) 255))
   (math/round (* (.-g color) 255))
   (math/round (* (.-b color) 255))])

;; procedural textures are functions of points that emit materials
(ann-record Material [color :- Color, reflectivity :- Number,
                           diffuseness :- Number])
(defrecord Material [color reflectivity diffuseness])

(def-alias MaterialFn [Point3 -> Material])
(ann-many MaterialFn
               flat-red shiny-red semi-shiny-green
               shiny-white checked-matt)

(defn flat-red [_] (Material. red 0.0 1.0))
(defn shiny-red [_] (Material. red 0.3 0.9))
(defn semi-shiny-green [_] (Material. green 0.5 0.7))
(defn shiny-white [_] (Material. white 0.3 0.9))

(ann matt-size Number)
(def matt-size 20.0)

(ann matt-dim-even? [Number -> Boolean])
(defn matt-dim-even? [dim]
  (even? (int (/ dim matt-size))))

(defn checked-matt [point]
  (let [x-even? (matt-dim-even? (.-x point))
        y-even? (matt-dim-even? (.-y point))
        z-even? (matt-dim-even? (.-z point))]
    (if (xor (xor x-even? y-even?) z-even?)
      (Material. white 0.0 1.0)
      (Material. black 0.0 1.0))))


;; SHAPES

(ann-record Intersection [normal :- Vector3, point :- Point3,
                               ray :- Ray, material :- Material])
(defrecord Intersection [normal point ray material])

(ann epsilon Number)
(def epsilon 0.001)

;; this should really be a tuple... maybe use clj-tuple?
(def-alias TimeIntersect '[Time Intersection])
(def-alias TimeIntersectSeq (Seqable TimeIntersect))
(def-alias NonEmptyTISeq (NonEmptySeq TimeIntersect))
;; a seq of TimeIntersect vectors... hope this isn't too confusing...

(ann-protocol Shape
  intersect [Shape Ray -> TimeIntersectSeq])
(defprotocol> Shape
  (intersect [shape ray]))

(ann above-error? [Number -> Boolean])
(defn above-error? [n]
  (> n epsilon))

(ann sphere-time-intersect-fn [Ray Point3 MaterialFn ->
                               (Fn [Time -> TimeIntersect])])
(defn sphere-time-intersect-fn [ray center material-fn]
  (fn [time]
    (let [pos    (position-at-time ray time)
          normal (-p pos center)]
      [time
       (Intersection. normal pos ray (material-fn pos))])))

(ann-datatype Sphere [center :- Point3, radius :- Number,
                      material-fn :- MaterialFn])
(deftype Sphere [center radius material-fn]
  Shape
  (intersect [this {:keys [base direction] :as ray}]
    (let [a (squared-mag direction)
          center-vec (-p base center)
          b (* 2 (dot direction center-vec))
          c (- (squared-mag center-vec)
               (* radius radius))
          times (filter above-error?
                        (roots a b c))]
      (map (sphere-time-intersect-fn ray center material-fn)
           times))))

;; Plane is defined by a normal (its a 2 sided plane though) and a distance
;; The plane coincident with y=5 and normal (0,0,1) has distance -5.
;; Htrace contained a non-sensical dot product between a point and a vector
;; so I used the standard algebraic form t = ((p_0 - l_0) dot n)/(l dot n)
(ann-datatype Plane [normal :- Vector3, distance :- Number,
                     material-fn :- MaterialFn])
(deftype Plane [normal distance material-fn]
  Shape
  (intersect [this {:keys [base direction] :as ray}]
    (let [vd     (dot direction normal)
          neg-p0 (*k (+v origin normal) distance)
          v0     (negate (dot (+p base neg-p0) normal))]
      (if (zero? vd)
        []
        (let [t (/ v0 vd)
              hit-point (position-at-time ray t)]
          (if (> t epsilon)
            [[t (Intersection.
                  (if (> vd 0) (neg normal) normal)
                  hit-point ray (material-fn hit-point))]]
            []))))))

(ann select-nearest [TimeIntersect TimeIntersect -> TimeIntersect])
(defn select-nearest [[t1 i1] [t2 i2]]
  (if (< t1 t2) [t1 i1] [t2 i2]))

;; the input must be non-empty!
(ann closest [NonEmptyTISeq -> Intersection])
(defn closest [xs]
  (second (reduce select-nearest xs)))

;; LIGHTS

(ann intersect-ray-fn [Ray -> (Fn [Shape -> TimeIntersectSeq])])
(defn intersect-ray-fn [ray]
  (fn [shape]
    (intersect shape ray)))

(ann combined-hits [ShapeVector Ray -> TimeIntersectSeq])
(defn combined-hits [shapes ray]
  (let [intersect-fn (intersect-ray-fn ray)]
    (filter not-empty
            (apply concat (map intersect-fn shapes)))))

(ann point-is-lit? [Point3 Point3 ShapeVector -> Boolean])
(defn point-is-lit? [point light-pos shapes]
  "Helper to calculate the diffuse light at the surface normal, given
   the light direction (from light source to surface)"
  (let [path (-p light-pos point)
        time-at-light (mag path)
        ray (Ray. point (normalize path))
        hits (combined-hits shapes ray)
        times (map first hits)]
    (if (empty? times)
      true
      ;; make sure the intersection isn't *behind* the light
      (> (apply min (first times) (rest times))
         time-at-light))))

(ann diffuse-coeff [Vector3 Vector3 -> Number])
(defn diffuse-coeff [light-dir normal]
  "Helper to calculate the diffuse light at the surface normal, given
   the light direction (from light source to surface)"
  (max 0.0 (negate (dot (normalize light-dir)
                        (normalize normal)))))

;; explicitly take shapes as an arg since we don't want globals littering
;; our program
(ann-protocol Light
  local-light [Light ShapeVector Intersection -> Color])
(defprotocol> Light
  (local-light [light shapes intersection]))

(ann-datatype Directional [direction :- Vector3, light-color :- Color])
(deftype Directional [direction light-color]
  Light
  (local-light [light shapes {:keys [normal material] :as intersection}]
    (let [mixed-color (*col (:color material) light-color)
          diffuse     (*k mixed-color
                          (* (diffuse-coeff direction normal)
                                (:diffuseness material)))]
      diffuse)))

(ann-datatype Spotlight [light-pos :- Point3, light-color :- Color])
(deftype Spotlight [light-pos light-color]
  Light
  (local-light [light shapes {:keys [normal point material]
                              :as intersection}]
    (let [mixed-color (*col (:color material) light-color)
          direction   (-p point light-pos)
          diffuse     (*k mixed-color
                          (* (diffuse-coeff direction normal)
                                (:diffuseness material)))]
      (if (point-is-lit? point light-pos shapes)
        diffuse
        black))))


;; REFLECTIONS: global lighting model

(declare raytrace)

(ann reflected-ray [Scene AnyInteger Intersection -> Color])
(defn reflected-ray [scene depth {:keys [normal point ray material]
                                  :as intersection}]
  (if (= (:reflectivity material) 0.0)
    black
    (let [in-dir (:direction ray)
          neg-in-dir (neg in-dir)
          k (* 2 (dot (normalize normal)
                      (normalize neg-in-dir)))
          ;; don't know why Htrace does double negative...
          out-ray-dir (+v (*k (normalize normal) k)
                          in-dir)
          reflected-col (raytrace scene
                                  (inc depth)
                                  (Ray. point out-ray-dir))]
      (*k reflected-col
          (:reflectivity material)))))

;; VIEWING AND SCREEN

;; view up really ought to be orthogonal to the view direction...
;; view direction = (normalize (-p looking-at camera-pos))
(ann-record View [camera-pos :- Point3, view-dist :- Number,
                  looking-at :- Point3, view-up :- Vector3])
(defrecord View [camera-pos view-dist looking-at view-up])

(def-alias LightVector (IPersistentVector Light))
(def-alias ShapeVector (IPersistentVector Shape))

(ann-record Scene [view :- View, shapes :- ShapeVector,
                   background-color :- Color, ambient-light :- Color,
                   lights :- LightVector])
(defrecord Scene [view shapes background-color ambient-light lights])

(ann default-scene Scene)
(def default-scene
  (Scene.
   (View. (Point3. 0 0 -100) 100 (Point3. 0 0 100) (Vector3. 0 1 0))
   [(Plane. (Vector3. 0 -1 0) 50 shiny-red)
    (Sphere. (Point3. 50 10 100) 40 semi-shiny-green)
    (Sphere. (Point3. -80 0 80) 50 shiny-white)]
   black
   (Color. 0.1 0.1 0.1)
   [(Spotlight. (Point3. 100 -30 0) nearly-white)
    (Spotlight. (Point3. -100 -100 150) nearly-white)]))

(ann transform-point [Point3 Vector3 Vector3 Vector3 Point3 -> Point3])
(defn transform-point [screen-center center-offset view-right view-up point]
  (let [offset-point (+v point center-offset)]
    (+v (+v screen-center
            (*k view-right (.-x offset-point)))
         (*k view-up (.-y offset-point)))))

(ann empty-pixel-grid [Integer Integer -> (Seqable Point3)])
(defn empty-pixel-grid [width height]
  #+clj(for> :- Point3
             [y :- Number (range height)
              x :- Number (range width)]
             (Point3. x y 0.0))
  #+cljs(for [y (range height)
              x (range width)]
          (Point3. x y 0.0)))

;; this is exactly the sort of thing that matrix operations shine at.
(ann pixel-grid [View Number Number -> (IPersistentVector Point3)])
(defn pixel-grid [{:keys [camera-pos view-dist looking-at view-up]}
                  width height]
  (let [grid          (empty-pixel-grid width height)
        center-offset (Vector3. (* -0.5 width) (* -0.5 height) 0)
        view-dir      (normalize (-p looking-at camera-pos))
        screen-center (+v camera-pos (*k view-dir view-dist))
        view-right    (cross view-up view-dir)]
    (vec
     (map (partial transform-point screen-center center-offset
                                   view-right view-up)
       grid))))
;; direly need to test

;; it is beautiful how add is implicitly defined for compound types
;; in haskell...

(ann parallel-projection [View Point3 -> Ray])
(defn parallel-projection [{:keys [camera-pos looking-at]} point]
  "Create rays parallel to viewing screen (orthographic)"
  (Ray. point
        (normalize (-p looking-at camera-pos))))

(ann perspective-projection [View Point3 -> Ray])
(defn perspective-projection [{:keys [camera-pos]} point]
  "Perspective projection which creates rays through
  (0,0,-distance) through the point"
  (Ray. point
        (normalize (-p point camera-pos))))


;; MAIN RENDERING FUNCTIONS

(ann max-bounce-depth Integer)
(def max-bounce-depth 2)

(ann total-local-lighting [Intersection ShapeVector
                           Color LightVector -> Color])
(defn total-local-lighting [hit shapes ambient-light lights]
  (+col ambient-light
        (sum-colors
          (map (ann-form
                #(local-light % shapes hit)
                [Light -> Color])
             lights))))

;; my version explicitly takes the scene as an arg
;; boo for global variables...
;; maybe we should take the desired max bounces as an arg...
(ann overall-lighting [Scene AnyInteger Intersection -> Color])
(defn overall-lighting [{:keys [lights ambient-light shapes] :as scene}
                        depth hit]
  "Calculate the overall color of a ray/shape intersection,
   taking into account local lighting (diffuse only) and
   global lighting (reflections only, to depth bounces)"
  (let [local-lighting  (total-local-lighting hit shapes ambient-light lights)
        global-lighting (if (< depth max-bounce-depth)
                          (reflected-ray scene depth hit)
                          black)]
    (clamp (+col local-lighting global-lighting))))

;; explicitly takes the scene as an argument instead of using global vars
(ann raytrace [Scene AnyInteger Ray -> Color])
(defn raytrace [{:keys [shapes background-color] :as scene} depth ray]
  (let [hits (combined-hits shapes ray)]
    (if (empty? hits)
      background-color
      (overall-lighting scene depth (closest (seq hits))))))

;; I separated render from render-to-pgm since I want to have
;; multiple output targets
;; (e.g. the html canvas element)
;; explicitly take a scene as an argument...
;; (contains view, shapes, and lighting...)
;; we deliberately force the output to be a vec to get O(1) indexing
(ann render [Scene Integer Integer -> (IPersistentVector Color)])
(defn render [{:keys [view] :as scene}
              width height]
  (let [ray-collection   (map (partial perspective-projection view)
                              (pixel-grid view width height))
        color-collection (map (partial raytrace scene 0)
                              ray-collection)]
    (vec color-collection)))


;; PPM export functions...

(ann ppm-color [Color -> String])
(defn ppm-color [color]
  (apply str
         (-> (vec (interpose " " (col->vec255 color)))
             (conj " "))))

(ann make-ppm [Integer Integer (Seqable Color) -> String])
(defn make-ppm [width height pixel-colors]
  (str "P3\n" width " " height "\n255\n"
       (apply str
         #+clj(for> :- String
                [pixel :- Color pixel-colors]
                (ppm-color pixel))
         #+cljs(for [pixel pixel-colors]
                 (ppm-color pixel)))))
