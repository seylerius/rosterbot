(defproject com.seriouslyseylerius/rosterbot "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [loco "0.3.1"]
                 [google-apps-clj "0.5.3"]
                 [clj-pdf "2.2.8"]]
  :main ^:skip-aot com.seriouslyseylerius.rosterbot
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
