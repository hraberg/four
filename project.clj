(defproject four "0.1.0"
  :dependencies [[org.clojure/clojure "1.5.0-alpha3"]]
  :plugins [[lein-swank "1.4.4"]]
  :jar-name "four.jar"
  :jar-exclusions [#"project.clj"]
  :main four)
