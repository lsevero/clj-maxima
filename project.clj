(defproject clj-maxima "0.1.0-SNAPSHOT"
  :description "Maxima as a clojure library"
  :url "https://github.com/lsevero/clj-maxima"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [abclj "1.5.0"]]
  :profiles {:dev {:plugins [[cider/cider-nrepl "0.24.0"]]
                   :repl-options {:init-ns clj-maxima.core}}}
  :repl-options {:init-ns clj-maxima.core})
