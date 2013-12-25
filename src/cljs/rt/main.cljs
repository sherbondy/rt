(ns rt.main
  (:require [rt.core :refer [col->vec255 render default-scene
                             perspective-projection pixel-grid
                             raytrace]])
  (:use-macros [dommy.macros :only [sel1]]))

;; output function based on:
;; http://beej.us/blog/data/html5s-canvas-2-pixel/

(enable-console-print!)
(println "hello there...")

(def canvas (sel1 :#canvas))

;; maybe it would be better to make out data Uint8ClampedArray in one go
;; instead of calling a function for each pixel. Could also ditch the
;; intermediate vector step.
(defn set-pixel! [pixel-data index color]
  (let [index (* index 4)]
    (aset pixel-data index       (* 255 (.-r color)))
    (aset pixel-data (+ index 1) (* 255 (.-g color)))
    (aset pixel-data (+ index 2) (* 255 (.-b color)))
    (aset pixel-data (+ index 3) 255)))


;; (defn render [{:keys [view] :as scene}
;;               width height]
;;   (let [ray-collection   (map (partial perspective-projection view)
;;                               (pixel-grid view width height))
;;         color-collection (map (partial raytrace scene 0)
;;                               ray-collection)]
;;     (vec color-collection)))

;; instead of using the render function directly, I should render and display
;; each pixel or row incrementally. Then the user can see the progress!

(defn render-to-canvas [canvas {:keys [view] :as scene}]
  (let [ctx    (.getContext canvas "2d")
        width  (.-width canvas)
        height (.-height canvas)
        max-index  (* (dec width) (dec height))
        image-data (.createImageData ctx width height)
        pixel-data (.-data image-data)
        pixel-points (pixel-grid view width height)]
    (letfn [(render-point [index]
              (let [point (nth pixel-points index)
                    ray   (perspective-projection view point)
                    color (raytrace scene 0 ray)]
                (set-pixel! pixel-data index color)
                (when (< index max-index)
                  (if (= (mod index width) 0)
                    (do
                      (.putImageData ctx image-data 0 0)
                      (js/requestAnimationFrame
                       #(render-point (inc index))))
                    (render-point (inc index))))))]
      (render-point 0))))

(render-to-canvas canvas default-scene)
