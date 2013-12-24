(ns rt.main
  (:require [rt.core :refer [col->vec255 render default-scene]])
  (:use-macros [dommy.macros :only [sel1]]))

;; output function based on:
;; http://beej.us/blog/data/html5s-canvas-2-pixel/

(enable-console-print!)
(println "hello there...")

(def canvas (sel1 :#canvas))

(defn set-pixel! [image-data x y [r g b]]
  (let [width (.-width image-data)
        index (* (+ x (* y width)) 4)]
    (aset (.-data image-data) index       r)
    (aset (.-data image-data) (+ index 1) g)
    (aset (.-data image-data) (+ index 2) b)
    (aset (.-data image-data) (+ index 3) 255)))

(defn render-to-canvas [canvas scene]
  (let [ctx    (.getContext canvas "2d")
        width  (.-width canvas)
        height (.-height canvas)
        image-data (.createImageData ctx width height)
        rendering  (render scene width height)]
    (doseq [y (range height)
            x (range width)]
      (let [pixel (nth rendering (+ x (* y width)))
            color (col->vec255 pixel)]
        (set-pixel! image-data x y color)))
    (.putImageData ctx image-data 0 0)))

(render-to-canvas canvas default-scene)
