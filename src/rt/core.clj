(ns rt.core
  (:require [clojure.core.typed :as typed
             :refer [ann ann-datatype ann-form ann-many
                     ann-protocol ann-record
                     def-alias def> defprotocol>
                     for> letfn>
                     AnyInteger]]
            [clojure.math.numeric-tower :as math])
  (:import [clojure.lang IPersistentVector Seqable]))

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
(ann ^:no-check clojure.core/not-empty [Seqable -> Boolean])
(ann ^:no-check clojure.math.numeric-tower/sqrt [Number -> Number])
(ann ^:no-check clojure.math.numeric-tower/round [Number -> Integer])

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
  (let [mag-v (mag v)]
    (if (not= mag-v 0)
      (k*v (/ 1 mag-v) v)
      (Vector3. 0 0 0))))

(ann neg [Vector3 -> Vector3])
(defn neg [v]
  (k*v -1 v))

;; how do I define an alias for a record?
;; wish I could just do:
;;(defalias Point3 Vector3), or something along those lines...
;;(Point3. 1 2 3)
;; I can define a union type for Vector3 and Point3...
(ann-record Point3 [x :- Number, y :- Number, z :- Number])
(defrecord Point3 [x y z])

(def> origin :- Point3 (Point3. 0 0 0))

;; could I just reuse v+ with some core.typed cleverness?
;; make a union type, and have the function dispatch on the arg order...
;; really seems like I should implement + as a multimethod
(ann p+ [Point3 Point3 -> Vector3])
(defn p+ [{:keys [x y z]} {a :x b :y c :z}]
  (Vector3. (+ x a) (+ y b) (+ z c)))

(ann p+v [Point3 Vector3 -> Point3])
(defn p+v [{:keys [x y z]} {a :x b :y c :z}]
  (Point3. (+ x a) (+ y b) (+ z c)))

(ann k*p [Number Point3 -> Point3])
(defn k*p [c {:keys [x y z]}]
  (Point3. (* c x) (* c y) (* c z)))

;; now I am feeling the pain, should really be using multimethods
; to define addition
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
(ann-record Color [r :- Number, g :- Number, b :- Number])
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

(ann sum-colors [(Seqable Color) -> Color])
(defn sum-colors [colors]
  (reduce col+ black colors))

;; procedural textures are functions of points that emit materials
(ann-record Material [color :- Color, reflectivity :- Number,
                      diffuseness :- Number])
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

(ann-record Intersection [normal :- Vector3, point :- Point3,
                          ray :- Ray, material :- Material])
(defrecord Intersection [normal point ray material])

(def> epsilon :- Number 0.001)

;; this should really be a tuple...
(def-alias TimeIntersect '[Time Intersection])
(def-alias TimeIntersectSeq (Seqable TimeIntersect))
;; a vector of TimeIntersectVecs... hope this isn't confusing...

(ann-protocol Shape
  intersect [Shape Ray -> TimeIntersectSeq])
(defprotocol> Shape
  (intersect [shape ray]))

(def-alias MaterialFn [Point3 -> Material])

(ann-datatype Sphere [center :- Point3, radius :- Number,
                      material-fn :- MaterialFn])
(deftype Sphere [center radius material-fn]
  Shape
  (intersect [this {:keys [base direction] :as ray}]
    (let [a (squared-mag direction)
          center-vec (p+ base (k*p -1 center))
          b (* 2 (dot direction center-vec))
          c (- (squared-mag center-vec)
               (* radius radius))
          times (filter (ann-form #(> % epsilon)
                                  [Number -> Boolean])
                        (roots a b c))]
      ;; these should maybe be top-level functions?
      (letfn> [normal-at-time :- [Time -> Vector3]
               (normal-at-time [t]
                 (normalize (p+ (position-at-time ray t)
                                (k*p -1 center))))

               intersection-at-time :- [Time -> Intersection]
               (intersection-at-time [t]
                 (let [pos (position-at-time ray t)]
                  (Intersection. (normal-at-time t)
                                 pos ray (material-fn pos))))]
              (map
               (ann-form #(identity [% (intersection-at-time %)])
                         [Time -> TimeIntersect])
               times)))))

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

;; might this get an empty vector as the argument?
;; In which case the result would be nil.
(ann closest [TimeIntersectSeq -> Intersection])
;; I guess we do this trickery with reduce to support select-nearest
;; when xs is empty
(defn closest [xs]
  (letfn> [select-nearest :- [TimeIntersect TimeIntersect ->
                              TimeIntersect]
           (select-nearest [[t1 i1] [t2 i2]]
             (if (< t1 t2) [t1 i1] [t2 i2]))]
    (second (reduce select-nearest xs))))

;; LIGHTS

;; does this capture the fact that the result may be empty?
;; this is broken!
(ann combined-hits [ShapeVector Ray -> TimeIntersectSeq])
(defn combined-hits [shapes ray]
  (filter not-empty
          (apply concat (map (ann-form #(intersect % ray)
                                 [Shape -> TimeIntersectSeq])
                         shapes))))

;; something is TERRIBLY wrong: hits is returning color instead
;; of intersect time vecs...
(ann point-is-lit? [Point3 Point3 ShapeVector -> Boolean])
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
      (> (apply min times) time-at-light))))

(ann diffuse-coeff [Vector3 Vector3 -> Number])
(defn diffuse-coeff [light-dir normal]
  "Helper to calculate the diffuse light at the surface normal, given
   the light direction (from light source to surface)"
  (max 0.0 (negate (dot (normalize light-dir) (normalize normal)))))

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
    (let [mixed-color (col* (:color material) light-color)
          diffuse     (k*col (* (diffuse-coeff direction normal)
                                (:diffuseness material))
                             mixed-color)]
      diffuse)))

(ann-datatype Spotlight [light-pos :- Point3, light-color :- Color])
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

(ann raytrace [Scene AnyInteger Ray -> Color])
(declare raytrace)

(ann reflected-ray [Scene AnyInteger Intersection -> Color])
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

(ann-record View [camera-pos :- Point3, view-dist :- Number,
                  looking-at :- Point3, view-up :- Vector3])
(defrecord View [camera-pos view-dist looking-at view-up])

(def-alias LightVector (IPersistentVector Light))
(def-alias ShapeVector (IPersistentVector Shape))

(ann-record Scene [view :- View, shapes :- ShapeVector,
                   background-color :- Color, ambient-light :- Color,
                   lights :- LightVector])
(defrecord Scene [view shapes background-color ambient-light lights])

(def> default-scene :- Scene
  (Scene.
   (View. (Point3. 0 0 -100) 100 (Point3. 0 0 100) (Vector3. 0 -1 0))
   [(Plane. (Vector3. 0 -1 0) 50 shiny-red)
    (Sphere. (Point3. 50 10 100) 40 semi-shiny-green)
    (Sphere. (Point3. -80 0 80) 50 checked-matt)]
   black
   (Color. 0.1 0.1 0.1)
   [(Spotlight. (Point3. 100 -30 0) nearly-white)
    (Spotlight. (Point3. -100 -100 150) nearly-white)]))

;; why do they have x and y flipped in Htrace?
;; this fn seems like a hack: should probably just do matrix math?
;; incomplete...
(ann pixel-grid [View Number Number -> (Seqable Point3)])
(defn pixel-grid [{:keys [camera-pos view-dist looking-at view-up]}
                  width height]
  (let [grid (for> :- Point3
                   [x :- Number (range width)
                    y :- Number (range height)]
                   (Point3. x y 0.0))
        center-offset (Vector3. (* -0.5 width) (* -0.5 height) 0)
        pixel-offsets (map (ann-form #(p+v % center-offset)
                                     [Point3 -> Point3])
                           grid)
        ;; view dir should be a vector!
        view-dir (normalize (p+ looking-at (k*p -1 camera-pos)))
        screen-center (p+v camera-pos (k*v view-dist view-dir))
        view-right (cross view-dir view-up)]
        (letfn> [transform :- [Point3 -> Point3]
                (transform [{:keys [x y]}]
                  (p+v (p+v screen-center (k*v x view-right))
                       (k*v y (neg view-up))))]
          (map transform pixel-offsets))))
;; direly need to test

;; it is beautiful how add is implicitly defined for compound types
;; in haskell...

(ann parallel-projection [View Point3 -> Ray])
(defn parallel-projection [{:keys [camera-pos looking-at]} point]
  "Create rays parallel to viewing screen (orthographic)"
  (Ray. point (normalize (p+ looking-at (k*p -1 camera-pos)))))

(ann perspective-projection [View Point3 -> Ray])
(defn perspective-projection [{:keys [camera-pos]} point]
  "Perspective projection which creates rays through
  (0,0,-distance) through the point"
  (Ray. point (normalize (p+ point (k*p -1 camera-pos)))))


;; MAIN RENDERING FUNCTIONS

(def> max-bounce-depth :- Integer 2)

;; my version explicitly takes the scene as an arg
;; boo for global variables...
;; maybe we should take the desired max bounces as an arg...
(ann overall-lighting [Scene AnyInteger Intersection -> Color])
(defn overall-lighting [{:keys [lights ambient-light shapes] :as scene}
                        depth hit]
  "Calculate the overall color of a ray/shape intersection,
   taking into account local lighting (diffuse only) and
   global lighting (reflections only, to depth bounces)"
  (let [local-lighting (col+ ambient-light
                             (sum-colors
                              (map (ann-form #(local-light % shapes hit)
                                             [Light -> Color])
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
(ann render [Scene Number Number -> (Seqable Color)])
(defn render [{:keys [view shapes background-color ambient-light lights]
               :as scene}
              width height]
  (letfn> [projection :- [Point3 -> Ray]
           (projection [point] (perspective-projection view point))]
    (let [ray-collection (map projection (pixel-grid view width height))
          color-collection (map (partial raytrace scene 0) ray-collection)]
      color-collection)))

(ann ppm-color [Color -> String])
(defn ppm-color [{:keys [r g b] :as color}]
  (str (math/round (* r 255)) " "
       (math/round (* g 255)) " "
       (math/round (* b 255)) " "))

(ann make-ppm [Integer Integer (Seqable Color) -> String])
(defn make-ppm [width height pixel-colors]
  (str "P3\n" width " " height "\n255\n"
       (apply str
         (for> :- String
               [pixel :- Color pixel-colors]
           (ppm-color pixel)))))

(ann -main [-> Any])
(defn -main []
  (spit "test.ppm"
    (make-ppm 500 500
              (render default-scene 500 500))))

(-main)
