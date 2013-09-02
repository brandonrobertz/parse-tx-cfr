(defproject parse-tx-cfr "0.2.0-SNAPSHOT"
  :description "FIXME: An experimental PDF parser for Texas Ethics Commission Campaign Finance Reports"
  :url "https://github.com/brandonrobertz/Parse-TX-CFR"
  :license {:name "GPLv3+"
            :url "http://www.gnu.org/licenses/gpl-3.0.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [com.snowtide/pdftextstream "2.6.2"]]
  :repositories [["snowtide-releases" "http://maven.snowtide.com/releases"]]
  :main parse-tx-cfr.core
  :repl-options {:port 50000})
