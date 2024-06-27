(defproject rvbbit-backend "0.1.0-SNAPSHOT"
  :description        "FIXME: write description"
  :url                "https://github.com/ryrobes/rvbbit-backend"
  :license            {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
                       :url  "https://www.eclipse.org/legal/epl-2.0/"}
  :plugins            [[lein-cljfmt "0.6.4"] [lein-ancient "0.6.15"]]
  :dependencies       [;[org.clojure/clojure "1.10.3"]
                       [org.clojure/clojure "1.11.1"]
                       [org.clojure/core.async "1.6.673"]
                       [com.github.seancorfield/honeysql "2.2.868"]
                       [jarohen/chime "0.3.3"]
                       [philoskim/debux "0.8.3"]
                       [org.clojure/java.jdbc "0.7.12"]
                       [clj-taskpool "0.1.0"] ;; https://github.com/WickedShell/clj-taskpool
                       [tea-time "1.0.1"] ;; https://github.com/aphyr/tea-time - task scheduler
                       [mvxcvi/puget "1.3.2"] ;; https://github.com/greglook/puget
                       [clojure.java-time "1.1.0"] ;; https://github.com/dm3/clojure.java-time
                       [com.nextjournal/beholder "1.0.0"]
                       [org.xerial/sqlite-jdbc "3.36.0.3"]
                       [org.postgresql/postgresql "42.3.3"]
                       [ru.yandex.clickhouse/clickhouse-jdbc "0.3.1"] ; "0.3.2"]
                       [org.clojars.prepor/vertica-jdbc "7.0.1-0"]
                       [mysql/mysql-connector-java "8.0.31"]
                       [com.microsoft.sqlserver/mssql-jdbc "11.2.1.jre8"]
                       [com.oracle.database.jdbc/ojdbc8 "19.11.0.0"]
                       [clj-time "0.15.2"]
                       [org.clojure/data.csv "1.0.1"]
                       [org.clj-commons/claypoole "1.2.2"]
                       [csv-map "0.1.2"]
                       [org.duckdb/duckdb_jdbc "0.9.1"]
                       [org.clojure/math.combinatorics "0.1.6"]
                       [hikari-cp "3.0.1"] ;; https://github.com/tomekw/hikari-cp
                       [net.clojars.wkok/openai-clojure "0.11.1"]
                       [clj-http "3.12.3"]
                       [cheshire "5.11.0"]
                       [org.clojure/core.cache "1.0.207"]
                       [talltale "0.5.8"]
                       [clucie "0.4.2"]
                       [jline "2.14.6"] ;; to detect console width
                       [clj-commons/clj-ssh "0.5.15"]
                       [com.ryrobes/flowmaps "0.31-SNAPSHOT"]
                       [nrepl "0.9.0"]
                       [shutdown "0.1.0-SNAPSHOT"]
                       [org.hsqldb/hsqldb "2.7.1"] ;; upper case forcing bullshit. useless as a
                       [com.h2database/h2 "2.1.214"]
                       [org.slf4j/slf4j-nop "1.7.32"]
                       [com.github.vertical-blank/sql-formatter "1.0.3"]
                       [org.clojars.rutledgepaulv/websocket-layer "0.1.11"]
                       [com.fasterxml.jackson.core/jackson-core "2.14.0-rc1"]
                       [io.pedestal/pedestal.service "0.6.4"]
                       [io.pedestal/pedestal.jetty "0.6.4"]]
  :jvm-opts           ["--add-opens=jdk.management/com.sun.management.internal=ALL-UNNAMED" ;; suppresses warnings
                       ;;"-Xmx32g"  ;; "-Xmx64g" ;; testing with 64g on my main dev machine
                       "-Xmx64g" ;; testing with 64g on my main dev machine
                       "-Xms16g" ;; testing
                       "-XX:+UseG1GC"
                       "-XX:MaxGCPauseMillis=200"
                       ;;"-Xss2048k" ;; testing with double the stack space to root out some flow-runner intermittent StackOverflow issue...
                       "-Xss1536k"
                       "--add-opens=java.base/sun.nio.ch=ALL-UNNAMED"] ;; datalevin specific
  :cljfmt             {}
  :source-paths       ["src" "../shared"]
  :repl-options       {:init-ns rvbbit-backend.core}
  :warn-on-reflection false
  :main               ^{:skip-aot true} rvbbit-backend.core ;; not for library use?
)
