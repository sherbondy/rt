(defproject rt "0.0.1"
  :description "A typed ray-tracer written in clojure (and targetting cljs too)"
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/core.typed "0.2.19"]
                 [org.clojure/math.numeric-tower "0.0.2"]]
  :core.typed {:check
               [rt.core]})
