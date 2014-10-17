(defproject constraint "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/clojurescript "0.0-2311"]]
  :jvm-opts  ["-Xmx1G"]
  :plugins [[lein-cljsbuild "1.0.4-SNAPSHOT"]]

  :cljsbuild {:builds [{:id "dev"
                        :source-paths ["src/constraint/"]
                        :compiler {:output-to "resources/public/constraint.js"
                                   :output-dir "resources/public/out"
                                   :optimizations :none
                                   :source-map true}}]})
