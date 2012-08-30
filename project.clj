(defproject four "0.1.0"
  :dependencies [[org.clojure/clojure "1.5.0-alpha4"]
                 [org.clojars.toxi/jogl "2.0.0-rc10"]]
  :profiles {:dev {:dependencies [[net.sf.proguard/proguard-base "4.8"]]}}
  :plugins [[lein-swank "1.4.4"]]
  :jar-name "four.jar"
  :jar-exclusions [#"project.clj" #"maven" #"leiningen" #"META-INF"]
  :jvm-opts ["-Dsun.java2d.noddraw=true" "-Dsun.java2d.opengl=false"]
  :main four)
