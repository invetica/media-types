(defproject invetica/media-types "0.1.0-SNAPSHOT"
  :description "Media types (https://tools.ietf.org/html/rfc2046)."
  :url "https://github.com/invetica/media-types"
  :license {:name "The MIT License"
            :url "http://opensource.org/licenses/MIT"}
  :dependencies [[org.clojure/clojure "1.9.0-alpha16"]
                 [org.clojure/test.check "0.9.0" :scope "provided"]]
  :aliases {"lint" ["do" ["whitespace-linter"] ["eastwood"]]}
  :profiles
  {:dev {:dependencies [[invetica/spec "0.4.0"]
                        [org.clojure/data.xml "0.2.0-alpha2"]]
         :plugins [[jonase/eastwood "0.2.3"]
                   [listora/whitespace-linter "0.1.0"]]}})
