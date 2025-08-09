(defproject de.dixieflatline/zeef "0.1.0-SNAPSHOT"
  :description "Filter expression library"
  :url "https://github.com/20centaurifux/zeef"
  :license {:name "AGPLv3"
            :url "https://www.gnu.org/licenses/agpl-3.0"}
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [org.clojure/test.check "1.1.1"]
                 [camel-snake-kebab "0.4.3"]
                 [metosin/malli "0.19.1"]]
  :target-path "target/%s"
  :plugins [[lein-cljfmt "0.9.2"]]
  :cljfmt {:load-config-file? true})