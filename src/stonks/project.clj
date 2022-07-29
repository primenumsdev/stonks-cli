(defproject stonks "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :repositories [["github" {:url      "https://maven.pkg.github.com/cljcloud/emdb"
                            :username "private-token"
                            :password :env/GITHUB_TOKEN}]]
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [org.clojure/core.memoize "1.0.257"]
                 [cljcloud/emdb "0.1.0"]
                 [cljcloud/clj-term "0.1.0"]
                 [metosin/jsonista "0.3.6"]
                 [org.clj-commons/clj-http-lite "0.4.392"]]
  :main stonks.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot      :all
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"
                                  "-Dclojure.compiler.elide-meta=[:doc :file :line :added]"]}
             :dev     {:plugins [[lein-shell "0.5.0"]]}}
  :aliases {"native"   ["shell"
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
                        "-H:Name=./target/${:name}-bin"]
            ;; https://github.com/upx/upx
            ;; compress native image to reduce binary size
            "compress" ["shell"
                        "upx"
                        ; max compress ratio
                        "-9"
                        "./target/${:name}-bin"
                        "-o"
                        "./target/${:name}-cli"]
            "build"    ["do"
                        ["clean"]
                        ["uberjar"]
                        ["native"]
                        ["compress"]]})
