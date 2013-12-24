(defproject rt "0.0.1"
  :description "A typed ray-tracer written in clojure (and targetting cljs too)"
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/core.typed "0.2.19"]
                 [org.clojure/math.numeric-tower "0.0.2"]
                 ;; light table only supports up to 2030 for now... :(
                 ;;[org.clojure/clojurescript "0.0-2030"]
                 [prismatic/dommy "0.1.1"]]

  :plugins [[com.keminglabs/cljx "0.3.1"]
            [lein-cljsbuild "1.0.1"]]

  :source-paths ["target/generated/src/clj" "src/clj"]

  :core.typed {:check
               [rt.core]}

  :cljx {
    :builds [
      {:source-paths ["src/cljx"]
       :output-path "target/generated/src/clj"
       :rules :clj}
      {:source-paths ["src/cljx"]
       :output-path "target/generated/src/cljs"
       :rules :cljs}]}
  ;; also need to port tests to cljx... reference:
  ;; https://github.com/Prismatic/schema/blob/master/project.clj

  :cljsbuild {
    :builds [{
      :source-paths ["target/generated/src/cljs" "src/cljs"]
      :compiler {
        :output-dir "out"
        :output-to "rt.js"
        :source-map "rt.js.map"
        :optimizations :whitespace
        :pretty-print false}}]}

  :main rt.main)
