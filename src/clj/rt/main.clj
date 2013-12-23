(ns rt.main
  (:require [clojure.core.typed :as typed
             :refer [ann check-ns]]
            [rt.core
             :refer [make-ppm render default-scene]]))

(ann -main [ -> nil])
(defn -main []
  (spit "test.ppm"
    (make-ppm 100 100
              (render default-scene 100 100))))

(-main)
