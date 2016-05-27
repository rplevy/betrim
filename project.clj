(defproject rplevy/betrim "4.4.0"
  :author "Robert P. Levy <https://rplevy.info>"
  :description "Simple(r), flexible logging for Clojure/Script. No XML."
  :url "https://github.com/rplevy/betrim"
  :license {:name "Eclipse Public License"
            :url  "http://www.eclipse.org/legal/epl-v10.html"
            :distribution :repo
            :comments "Same as Clojure"}
  :min-lein-version "2.3.3"
  :global-vars {*warn-on-reflection* true
                *assert*             true}

  :dependencies
  [[org.clojure/clojure "1.8.0"]
   [org.clojure/core.memoize "0.5.8"]
   [io.aviso/pretty "0.1.26"]]

  :profiles
  {:test {:dependencies [[org.clojure/tools.logging "0.3.1"]]}
   :dev [:test {:dependencies [[org.clojure/clojurescript "1.8.51"]]
                :plugins [[lein-cljsbuild "1.1.3"]]}]}

  :cljsbuild {:test-commands {"node" ["node" :node-runner "target/tests.js"]
                              "phantom" ["phantomjs" :runner "target/tests.js"]}
              :builds [{:id "main"
                        :source-paths ["src"]
                        :compiler {:output-to "target/main.js"
                                   :optimizations :advanced
                                   :pretty-print false}}]}

  :aliases {"build-once" ["do" "clean," "cljsbuild" "once" "main"]
            "deploy-lib" ["do" "build-once," "deploy" "clojars," "install"]
            "start-dev"  ["with-profile" "+server-jvm" "repl" ":headless"]}

  :repositories {"sonatype-oss-public"
                 "https://oss.sonatype.org/content/groups/public/"})
