(defproject com.seriouslyseylerius/rosterbot "0.1.0-SNAPSHOT"
  :description "A tool for generating duty rosters"
  :url "http://github.com/seylerius/rosterbot"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [loco "0.3.1"]
                 [google-apps-clj "0.5.3"]
                 [clj-pdf "2.2.8"]
                 [clj-time "0.12.2"]
                 [funcool/cuerdas "2.0.1"]]
  :main ^:skip-aot com.seriouslyseylerius.rosterbot
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
