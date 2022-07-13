(defproject stonks "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url  "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [org.clojure/core.memoize "1.0.257"]
                 [com.taoensso/nippy "3.1.3"]
                 [metosin/jsonista "0.3.6"]
                 [org.clj-commons/clj-http-lite "0.4.392"]
                 [com.googlecode.lanterna/lanterna "3.1.1"]]
  :main stonks.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot      :all
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"
                                  "-Dclojure.compiler.elide-meta=[:doc :file :line :added]"]}
             :dev     {:plugins [[lein-shell "0.5.0"]]}}
  :aliases {"native"     ["shell"
                          "native-image"
                          "--report-unsupported-elements-at-runtime"
                          "--allow-incomplete-classpath"
                          "--initialize-at-build-time"
                          "--diagnostics-mode"
                          "--no-server"
                          "--no-fallback"
                          ; to use with Docker from scratch, doesn't support MacOS
                          ;"--static"
                          "--install-exit-handlers"
                          "--enable-url-protocols=https"
                          "-jar" "./target/uberjar/${:uberjar-name:-${:name}-${:version}-standalone.jar}"
                          "-H:+ReportExceptionStackTraces"
                          ; to debug image with GraalVM Dashboard
                          ;"-H:+DashboardAll"
                          "-H:Name=./target/${:name}"]
            ;; https://github.com/upx/upx
            ;; compress native image to reduce binary size
            "compress"   ["shell"
                          "upx"
                          "-9"                              ; max compress ratio
                          "./target/${:name}"
                          "-o"
                          "./target/${:name}-cli"]
            "run-native" ["shell" "./target/${:name}"]})
