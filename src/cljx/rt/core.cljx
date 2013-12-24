(ns rt.core
  #+clj(:require [clojure.core.typed :as typed
                  :refer [ann ann-datatype ann-form ann-many
                          ann-protocol ann-record
                          def-alias def> defprotocol>
                          for> letfn>
                          AnyInteger
                          NonEmptySeqable
                          check-ns]]
                 [clojure.math.numeric-tower :as math])
  #+cljs(:require [rt.math :as math])
  #+clj(:import [clojure.lang IPersistentVector Seqable]))

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

;; add these annotations...
#+clj(ann ^:no-check clojure.core/not-empty [Seqable -> Boolean])
#+clj(ann ^:no-check clojure.core/spit [String String -> nil])
#+clj(ann ^:no-check clojure.math.numeric-tower/sqrt [Number -> Number])
#+clj(ann ^:no-check clojure.math.numeric-tower/round [Number -> Integer])

#+clj(ann-record Vector3 [x :- Number, y :- Number, z :- Number])
(defrecord Vector3 [x y z])

#+clj(ann v+ [Vector3 Vector3 -> Vector3])
(defn v+ [{:keys [x y z]} {a :x b :y c :z}]
  (Vector3. (+ x a) (+ y b) (+ z c)))

#+clj(ann v- [Vector3 Vector3 -> Vector3])
(defn v- [{:keys [x y z]} {a :x b :y c :z}]
  (Vector3. (- x a) (- y b) (- z c)))

;; k is for konstant!
#+clj(ann k*v [Number Vector3 -> Vector3])
(defn k*v [c {:keys [x y z]}]
  (Vector3. (* c x) (* c y) (* c z)))

#+clj(ann dot [Vector3 Vector3 -> Number])
(defn dot [{:keys [x y z]} {a :x b :y c :z}]
  (+ (* x a) (* y b) (* z c)))

#+clj(ann squared-mag [Vector3 -> Number])
(defn squared-mag [v]
  (dot v v))

;; careful: sqrt is not consistent across hosts...
;; js = Math/sqrt
#+clj(ann mag [Vector3 -> Number])
(defn mag [v]
  (math/sqrt (squared-mag v)))

;; is the implementation in Htrace wrong, or are they
;; using a left-handed coordinate system?
;; I did the math by hand and got - their x and z coords...
#+clj(ann cross [Vector3 Vector3 -> Vector3])
(defn cross [{:keys [x y z]} {a :x b :y c :z}]
  (Vector3. (- (* y c)
               (* z b))
            (- (* z a)
               (* x c))
            (- (* x b)
               (* y a))))

#+clj(ann normalize [Vector3 -> Vector3])
(defn normalize [v]
  (let [mag-v (mag v)]
    (if (not= mag-v 0)
      (k*v (/ 1 mag-v) v)
      (Vector3. 0 0 0))))

#+clj(ann neg [Vector3 -> Vector3])
(defn neg [v]
  (k*v -1 v))

;; how do I define an alias for a record?
;; wish I could just do:
;; (defalias Point3 Vector3), or something along those lines...
;; although it is actually nice to distinguish the two
#+clj(ann-record Point3 [x :- Number, y :- Number, z :- Number])
(defrecord Point3 [x y z])

#+clj(def> origin :- Point3 (Point3. 0 0 0))
#+cljs(def origin (Point3. 0 0 0))

;; could I just reuse v+ with some core.typed cleverness?
;; intersection type?
;; have the function dispatch on the arg order...
;; or implement + as a multimethod... (as in algo generic)
#+clj(ann p+ [Point3 Point3 -> Vector3])
(defn p+ [{:keys [x y z]} {a :x b :y c :z}]
  (Vector3. (+ x a) (+ y b) (+ z c)))

#+clj(ann p+v [Point3 Vector3 -> Point3])
(defn p+v [{:keys [x y z]} {a :x b :y c :z}]
  (Point3. (+ x a) (+ y b) (+ z c)))

#+clj(ann k*p [Number Point3 -> Point3])
(defn k*p [c {:keys [x y z]}]
  (Point3. (* c x) (* c y) (* c z)))

;; now I am feeling the pain, should I be using multimethods
;; to define addition?
;; maybe I should make a macro for commutative operations?

;; does origin make more sense than base?
#+clj(ann-record Ray [base :- Point3  direction :- Vector3])
(defrecord Ray [base direction])

#+clj(def-alias Time Number)
#+clj(ann position-at-time [Ray Time -> Point3])
(defn position-at-time [{:keys [base direction]} time]
  (p+v base (k*v time direction)))

#+clj(ann roots [Number Number Number -> (IPersistentVector Number)])
(defn roots [a b c]
  (let [discriminant (- (* b b) (* 4 a c))]
    (if (< discriminant 0)
      []
      (let [negative-b (* -1 b)
            root (math/sqrt discriminant)]
        [(* 0.5 (+ negative-b root))
         (* 0.5 (- negative-b root))]))))

;; time to bring in the core match?
#+clj(ann xor [Boolean Boolean -> Boolean])
(defn xor [a b]
  (condp = a
    true (not b)
    false b))

#+clj(ann negate [Number -> Number])
(defn negate [n]
  (- 0 n))

;; oh, could core/typed do (runtime?) bounds enforcing on numbers?
;; that'd be incredible
#+clj(ann-record Color [r :- Number, g :- Number, b :- Number])
(defrecord Color [r g b])

#+clj(ann-many Color
        red green blue white
        black mid-grey nearly-white)
(def red          (Color. 1.0 0.0 0.0))
(def green        (Color. 0.0 1.0 0.0))
(def blue         (Color. 0.0 0.0 1.0))
(def white        (Color. 1.0 1.0 1.0))
(def black        (Color. 0.0 0.0 0.0))
(def mid-grey     (Color. 0.5 0.5 0.5))
(def nearly-white (Color. 0.8 0.8 0.8))

#+clj(ann k*col [Number Color -> Color])
(defn k*col [k {:keys [r g b]}]
  (Color. (* k r) (* k g) (* k b)))

#+clj(ann col+ [Color Color -> Color])
(defn col+ [{:keys [r g b]} {r2 :r g2 :g b2 :b}]
  (Color. (+ r r2) (+ g g2) (+ b b2)))

;; couldn't help myself :P
#+clj(ann clamp-fn [Number Number -> (Fn [Number -> Number])])
(defn clamp-fn [f-min f-max]
  (fn [f]
    (min (max f f-min) f-max)))

#+clj(ann clamp-0-1 [Number -> Number])
(def clamp-0-1 (clamp-fn 0.0 1.0))

#+clj(ann clamp [Color -> Color])
(defn clamp [{:keys [r g b]}]
  (Color. (clamp-0-1 r)
          (clamp-0-1 g)
          (clamp-0-1 b)))

;; = combine_col
;; it would be amazing to simply define a binary operation,
;; specify that it is associative and commutative, and get an efficent
;; variadic version for free...
#+clj(ann col* [Color Color -> Color])
(defn col* [{:keys [r g b]} {r2 :r g2 :g b2 :b}]
  (Color. (* r r2) (* g g2) (* b b2)))

#+clj(ann sum-colors [(Seqable Color) -> Color])
(defn sum-colors [colors]
  (reduce col+ black colors))

#+clj(ann col->vec255 [Color -> (IPersistentVector Integer)])
(defn col->vec255 [{:keys [r g b] :as color}]
  [(math/round (* r 255))
   (math/round (* g 255))
   (math/round (* b 255))])

;; procedural textures are functions of points that emit materials
#+clj(ann-record Material [color :- Color, reflectivity :- Number,
                           diffuseness :- Number])
(defrecord Material [color reflectivity diffuseness])

#+clj(ann-many [Point3 -> Material]
               flat-red shiny-red semi-shiny-green
               shiny-white checked-matt)

(defn flat-red [_] (Material. red 0.0 1.0))
(defn shiny-red [_] (Material. red 0.3 0.9))
(defn semi-shiny-green [_] (Material. green 0.5 0.7))
(defn shiny-white [_] (Material. white 0.3 0.9))

#+clj(def> matt-size :- Number 20.0)
#+cljs(def matt-size 20.0)

#+clj(ann matt-dim-even? [Number -> Boolean])
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

#+clj(ann-record Intersection [normal :- Vector3, point :- Point3,
                               ray :- Ray, material :- Material])
(defrecord Intersection [normal point ray material])

#+clj(def> epsilon :- Number 0.001)
#+cljs(def epsilon 0.001)

;; this should really be a tuple... maybe use clj-tuple?
#+clj(def-alias TimeIntersect '[Time Intersection])
#+clj(def-alias TimeIntersectSeq (Seqable TimeIntersect))
#+clj(def-alias NonEmptyTISeq (NonEmptySeqable TimeIntersect))
;; a seq of TimeIntersect vectors... hope this isn't too confusing...

#+clj(ann-protocol Shape
       intersect [Shape Ray -> TimeIntersectSeq])
#+clj(defprotocol> Shape
       (intersect [shape ray]))
#+cljs(defprotocol Shape
        (intersect [shape ray]))

#+clj(def-alias MaterialFn [Point3 -> Material])

#+clj(ann above-error? [Number -> Boolean])
(defn above-error? [n]
  (> n epsilon))

#+clj(ann sphere-time-intersect-fn [Ray Point3 Time MaterialFn -> (Fn [Time -> TimeIntersect])])
(defn sphere-time-intersect-fn [ray center material-fn]
  (fn [time]
    (let [pos    (position-at-time ray time)
          normal (p+ pos (k*p -1 center))]
      [time
       (Intersection. normal pos ray (material-fn pos))])))

#+clj(ann-datatype Sphere [center :- Point3, radius :- Number,
                           material-fn :- MaterialFn])
(deftype Sphere [center radius material-fn]
  Shape
  (intersect [this {:keys [base direction] :as ray}]
    (let [a (squared-mag direction)
          center-vec (p+ base (k*p -1 center))
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
#+clj(ann-datatype Plane [normal :- Vector3, distance :- Number,
                     material-fn :- MaterialFn])
(deftype Plane [normal distance material-fn]
  Shape
  (intersect [this {:keys [base direction] :as ray}]
    (let [vd     (dot direction normal)
          neg-p0 (k*p distance (p+v origin normal))
          v0     (negate (dot (p+ base neg-p0) normal))]
      (if (= vd 0.0)
        []
        (let [t (/ v0 vd)
              hit-point (position-at-time ray t)]
          (if (> t epsilon)
            [[t (Intersection.
                  (if (> vd 0) (neg normal) normal)
                  hit-point ray (material-fn hit-point))]]
            []))))))

#+clj(ann select-nearest [TimeIntersect TimeIntersect -> TimeIntersect])
(defn select-nearest [[t1 i1] [t2 i2]]
  (if (< t1 t2) [t1 i1] [t2 i2]))

;; the input must be non-empty!
#+clj(ann closest [NonEmptyTISeq -> Intersection])
(defn closest [xs]
  (second (reduce select-nearest xs)))

;; LIGHTS

#+clj(ann intersect-ray-fn [Ray -> (Fn [Shape -> TimeIntersectSeq])])
(defn intersect-ray-fn [ray]
  (fn [shape]
    (intersect shape ray)))

#+clj(ann combined-hits [ShapeVector Ray -> TimeIntersectSeq])
(defn combined-hits [shapes ray]
  (let [intersect-fn (intersect-ray-fn ray)]
    (filter not-empty
            (apply concat (map intersect-fn shapes)))))

#+clj(ann point-is-lit? [Point3 Point3 ShapeVector -> Boolean])
(defn point-is-lit? [point light-pos shapes]
  "Helper to calculate the diffuse light at the surface normal, given
   the light direction (from light source to surface)"
  (let [path (p+ light-pos (k*p -1 point))
        time-at-light (mag path)
        ray (Ray. point (normalize path))
        hits (combined-hits shapes ray)
        times (map first hits)]
    (if (empty? times)
      true
      ;; make sure the intersection isn't *behind* the light
      (> (apply min times) time-at-light))))

#+clj(ann diffuse-coeff [Vector3 Vector3 -> Number])
(defn diffuse-coeff [light-dir normal]
  "Helper to calculate the diffuse light at the surface normal, given
   the light direction (from light source to surface)"
  (max 0.0 (negate (dot (normalize light-dir) (normalize normal)))))

;; explicitly take shapes as an arg since we don't want globals littering
;; our program
#+clj(ann-protocol Light
  local-light [Light ShapeVector Intersection -> Color])
(#+clj defprotocol> #+cljs defprotocol Light
  (local-light [light shapes intersection]))

#+clj(ann-datatype Directional [direction :- Vector3, light-color :- Color])
(deftype Directional [direction light-color]
  Light
  (local-light [light shapes {:keys [normal material] :as intersection}]
    (let [mixed-color (col* (:color material) light-color)
          diffuse     (k*col (* (diffuse-coeff direction normal)
                                (:diffuseness material))
                             mixed-color)]
      diffuse)))

#+clj(ann-datatype Spotlight [light-pos :- Point3, light-color :- Color])
(deftype Spotlight [light-pos light-color]
  Light
  (local-light [light shapes {:keys [normal point material]
                              :as intersection}]
    (let [mixed-color (col* (:color material) light-color)
          direction   (p+ point (k*p -1 light-pos))
          diffuse     (k*col (* (diffuse-coeff direction normal)
                                (:diffuseness material))
                             mixed-color)]
      (if (point-is-lit? point light-pos shapes)
        diffuse
        black))))


;; REFLECTIONS: global lighting model

#+clj(ann raytrace [Scene AnyInteger Ray -> Color])
(declare raytrace)

#+clj(ann reflected-ray [Scene AnyInteger Intersection -> Color])
(defn reflected-ray [scene depth {:keys [normal point ray material]
                                  :as intersection}]
  (if (= (:reflectivity material) 0.0)
    black
    (let [in-dir (:direction ray)
          neg-in-dir (neg in-dir)
          k (* 2 (dot (normalize normal) (normalize neg-in-dir)))
          ;; don't know why Htrace does double negative...
          out-ray-dir (v+ (k*v k (normalize normal)) in-dir)
          reflected-col (raytrace scene
                                  (inc depth)
                                  (Ray. point out-ray-dir))]
      (k*col (:reflectivity material)
             reflected-col))))

;; VIEWING AND SCREEN

#+clj(ann-record View [camera-pos :- Point3, view-dist :- Number,
                  looking-at :- Point3, view-up :- Vector3])
(defrecord View [camera-pos view-dist looking-at view-up])

#+clj(def-alias LightVector (IPersistentVector Light))
#+clj(def-alias ShapeVector (IPersistentVector Shape))

#+clj(ann-record Scene [view :- View, shapes :- ShapeVector,
                   background-color :- Color, ambient-light :- Color,
                   lights :- LightVector])
(defrecord Scene [view shapes background-color ambient-light lights])

#+clj(ann default-scene Scene)
(def default-scene
  (Scene.
   (View. (Point3. 0 0 -100) 100 (Point3. 0 0 100) (Vector3. 0 1 0))
   [(Plane. (Vector3. 0 -1 0) 50 shiny-red)
    (Sphere. (Point3. 50 10 100) 40 semi-shiny-green)
    (Sphere. (Point3. -80 0 80) 50 checked-matt)]
   black
   (Color. 0.1 0.1 0.1)
   [(Spotlight. (Point3. 100 -30 0) nearly-white)
    (Spotlight. (Point3. -100 -100 150) nearly-white)]))

#+clj(ann transform-point [Point3 Point3 Vector3 Vector3 -> Point3])
(defn transform-point [screen-center view-right view-up
                       {:keys [x y] :as point}]
  (p+v (p+v screen-center (k*v x view-right))
       (k*v y view-up)))

#+clj(ann pixel-grid [View Number Number -> (Seqable Point3)])
(defn pixel-grid [{:keys [camera-pos view-dist looking-at view-up]}
                  width height]
  (let [grid #+clj(for> :- Point3
                    [y :- Number (range height)
                     x :- Number (range width)]
                    (Point3. x y 0.0))
             #+cljs(for [y (range height) x (range width)]
                     (Point3. x y 0.0))

        center-offset (Vector3. (* -0.5 width) (* -0.5 height) 0)
        pixel-offsets (map #+clj(ann-form #(p+v % center-offset)
                                          [Point3 -> Point3])
                           #+cljs #(p+v % center-offset)
                           grid)

        view-dir      (normalize (p+ looking-at (k*p -1 camera-pos)))
        screen-center (p+v camera-pos (k*v view-dist view-dir))
        view-right    (cross view-up view-dir)]
          (map (partial transform-point screen-center view-right view-up)
               pixel-offsets)))
;; direly need to test

;; it is beautiful how add is implicitly defined for compound types
;; in haskell...

#+clj(ann parallel-projection [View Point3 -> Ray])
(defn parallel-projection [{:keys [camera-pos looking-at]} point]
  "Create rays parallel to viewing screen (orthographic)"
  (Ray. point (normalize (p+ looking-at (k*p -1 camera-pos)))))

#+clj(ann perspective-projection [View Point3 -> Ray])
(defn perspective-projection [{:keys [camera-pos]} point]
  "Perspective projection which creates rays through
  (0,0,-distance) through the point"
  (Ray. point (normalize (p+ point (k*p -1 camera-pos)))))


;; MAIN RENDERING FUNCTIONS

#+clj(def> max-bounce-depth :- Integer 2)
#+cljs(def max-bounce-depth 2)

;; my version explicitly takes the scene as an arg
;; boo for global variables...
;; maybe we should take the desired max bounces as an arg...
#+clj(ann overall-lighting [Scene AnyInteger Intersection -> Color])
(defn overall-lighting [{:keys [lights ambient-light shapes] :as scene}
                        depth hit]
  "Calculate the overall color of a ray/shape intersection,
   taking into account local lighting (diffuse only) and
   global lighting (reflections only, to depth bounces)"
  (let [local-lighting (col+ ambient-light
                             (sum-colors
                              (map #+clj(ann-form #(local-light % shapes hit)
                                             [Light -> Color])
                                   #+cljs #(local-light % shapes hit)
                                   lights)))
        global-lighting (if (< depth max-bounce-depth)
                          (reflected-ray scene depth hit)
                          black)]
    (clamp (col+ local-lighting global-lighting))))

;; explicitly takes the scene as an argument instead of using global vars
;; (ann raytrace [Scene Integer Ray -> Color])
(defn raytrace [{:keys [shapes background-color] :as scene} depth ray]
  (let [hits (combined-hits shapes ray)]
    (if (empty? hits)
      background-color
      (overall-lighting scene depth (closest hits)))))

;; I separated render from render-to-pgm since I want to have
;; multiple output targets
;; (e.g. the html canvas element)
;; explicitly take a scene as an argument...
;; (contains view, shapes, and lighting...)
#+clj(ann render [Scene Number Number -> (Seqable Color)])
(defn render [{:keys [view shapes background-color ambient-light lights]
               :as scene}
              width height]
  (let [ray-collection   (map (partial perspective-projection view)
                              (pixel-grid view width height))
        color-collection (map (partial raytrace scene 0)
                              ray-collection)]
    color-collection))

#+clj(ann ppm-color [Color -> String])
(defn ppm-color [color]
  (apply str
         (-> (vec (interpose " " (col->vec255 color)))
             (conj " "))))

#+clj(ann make-ppm [Integer Integer (Seqable Color) -> String])
(defn make-ppm [width height pixel-colors]
  (str "P3\n" width " " height "\n255\n"
       (apply str
         #+clj(for> :- String
                [pixel :- Color pixel-colors]
                (ppm-color pixel))
         #+cljs(for [pixel pixel-colors]
                 (ppm-color pixel)))))
