(defproject constraint "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/clojurescript "0.0-2311"]
                 [com.cemerick/piggieback  "0.1.3"]
                 [com.cemerick/austin "0.1.4"]
                 [figwheel  "0.1.3-SNAPSHOT"]
                 [prismatic/dommy "1.0.0"]
                 [crate "0.2.4"]
                 [rm-hull/monet "0.2.1"]]
  :jvm-opts  ["-Xmx1G"]

  :plugins [[lein-cljsbuild "1.0.4-SNAPSHOT"]
            [lein-figwheel "0.1.3-SNAPSHOT"]
            [com.cemerick/piggieback  "0.1.3"]
            [com.cemerick/austin "0.1.4"]]

  :figwheel {:http-server-root "public"
             :port 3449
             :css-dirs ["resources/public/css"]}


  :cljsbuild {:builds [{:id "dev"
                        :source-paths ["src/constraint"
                                       "src/figwheel"
                                       "src/brepl"]

                        :compiler {:output-to "resources/public/constraint.js"
                                   :output-dir "resources/public/out"
                                   :optimizations :none
                                   :source-map true}}]})
