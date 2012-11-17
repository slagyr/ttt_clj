(defproject ttt "1.0.0"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [speclj "2.3.1"]]
  :profiles {:dev {:dependencies [[speclj "2.3.1"]]}}
  :plugins [[speclj "2.3.1"]]
  :test-paths ["spec"]
  :main ttt.core)
