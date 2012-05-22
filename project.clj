(defproject waltz "0.1.1"
  :description "A ClojureScript library to keep your state in step"
  :dependencies [[org.clojure/clojure "1.4.0"]]
  :plugins [[lein-cljsbuild "0.1.9"]]
  :cljsbuild {:builds [{:source-path "src"
                        :compiler {:output-to "out/waltz.js"
                                   :output-dir "out"
                                   :optimizations :whitespace
                                   :pretty-print true}}]})