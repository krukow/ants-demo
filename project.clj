(defproject ants "1.0.0-SNAPSHOT"
  :description "FIXME: write"
  :dependencies [[org.clojure/clojure "1.2.0"]
		 [org.clojure/clojure-contrib "1.2.0"]]
  :dev-dependencies [[swank-clojure "1.3.0-SNAPSHOT"]
		     [cdt "1.0.1-SNAPSHOT"]]

  :jvm-opts
  ["-agentlib:jdwp=transport=dt_socket,server=y,suspend=n,address=8030"])
