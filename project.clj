(defproject iris "1.0.0-SNAPSHOT"
  :description "A Factually-aware Siri-clone"
  :dependencies [
                 ;; [org.clojure/clojure "1.3.0"]
                 [funnyplaces "1.2.1"]
                 [cadr "1.0.1-SNAPSHOT"]
                 [lambda "1.0.2-SNAPSHOT"]
                 [clj-http "0.2.5"]
                 [org.clojure/data.json "0.1.1"]
                 [net.sourceforge.javaflacencoder/java-flac-encoder "0.2.3"]
                 [de.huxhorn.sulky/de.huxhorn.sulky.3rdparty.jlayer "1.0"]
                 [slingshot "0.10.1"]
                 [edu.mit/jwi "2.2.2"]
                 [egamble/let-else "1.0.0"]]
  :dev-dependencies [[debug "1.0.0-SNAPSHOT"]]
  :repositories {"conjars" "http://conjars.org/repo/"}
  :main iris.core)
