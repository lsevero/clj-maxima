(defproject org.clojars.lsevero/clj-maxima "0.1.1-SNAPSHOT"
  :description "Maxima as a clojure library"
  :url "https://github.com/lsevero/clj-maxima"
  :license {:name "GPL-2.0"
            }
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [abclj "0.1.6"]
                 [org.clojure/tools.logging "0.6.0"]
                 ]
  :profiles {:dev {:plugins [[cider/cider-nrepl "0.24.0"]]
                   :global-vars {*warn-on-reflection* true}
                   :repl-options {:init-ns clj-maxima.core}}}
  :repl-options {:init-ns clj-maxima.core})
