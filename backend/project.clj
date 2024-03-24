(defproject rvbbit-backend "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "https://github.com/ryrobes/rvbbit-backend"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
 ; {:name "Eclipse Public License"
 ;  :url "http://www.eclipse.org/legal/epl-v10.html"}
  :plugins [[lein-cljfmt "0.6.4"]
            [lein-ancient "0.6.15"]]
  :dependencies [;[org.clojure/clojure "1.10.3"]
                 [org.clojure/clojure "1.11.1"]
                 ;[org.clojure/clojure "1.8.0"]
                 ;[org.clojure/core.async "0.3.441"]
                 ;[org.clojure/core.async "1.5.648"]
                 [org.clojure/core.async "1.6.673"]
                 ;[datalevin "0.6.22"] ;; https://github.com/juji-io/datalevin
                 ;[fuzzy-string "0.1.3"]
                 [com.github.seancorfield/honeysql "2.2.868"]
                 [jarohen/chime "0.3.3"]
                 [philoskim/debux "0.8.3"]
                 ;;;;;[zprint "1.2.8"] ;;; nope
                 [org.clojure/java.jdbc "0.7.12"]
                 [clj-taskpool "0.1.0"] ;; https://github.com/WickedShell/clj-taskpool
                 [tea-time "1.0.1"] ;; https://github.com/aphyr/tea-time - task scheduler
                 ;[net.sekao/odoyle-rules "1.0.0"] ;; https://github.com/oakes/odoyle-rules
                 [mvxcvi/puget "1.3.2"] ;; https://github.com/greglook/puget
                 [clojure.java-time "1.1.0"] ;; https://github.com/dm3/clojure.java-time
                 [com.nextjournal/beholder "1.0.0"]
                 ;[org.duckdb/duckdb_jdbc "0.5.0"]
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

                 [org.apache.lucene/lucene-core "8.11.3"]
                 [org.apache.lucene/lucene-analyzers-common "8.11.3"]
                 [org.apache.lucene/lucene-queryparser "8.11.3"]

                 [jline "2.14.6"] ;; to detect console width

                 [clj-commons/clj-ssh "0.5.15"]

                ;;  [commons-codec/commons-codec "1.15"]

                 [com.ryrobes/flowmaps "0.31-SNAPSHOT"]
               ;  [techascent/tech.ml.dataset "6.092"] ;; CTE based? move to ext repl?
               ;  [scicloj/scicloj.ml "0.2.1"] ;; CTE based? move to ext repl?
                 [nrepl "0.9.0"]
                 ;[incanter "1.9.3"]
                 ;[duratom "0.5.8"]
                 ;[durable-atom "0.0.3"]
                 ;[org.slf4j/slf4j-simple "1.7.5"]
                 ;[org.slf4j/jcl-over-slf4j "2.0.0-alpha0"]
                ; [io.pedestal/pedestal.jetty "0.5.10"]
                 [shutdown "0.1.0-SNAPSHOT"]
                 [org.hsqldb/hsqldb "2.7.1"] ;; upper case forcing bullshit. useless as a cache db
                 [com.h2database/h2 "2.1.214"]
                 ;[ch.qos.logback/logback-classic "1.2.3" :exclusions [org.slf4j/slf4j-api]]
                 [org.slf4j/slf4j-nop "1.7.32"]
                 ;[org.slf4j/slf4j-api "1.7.26"]
                 ;[org.slf4j/jul-to-slf4j "1.7.25"]
                 ;[org.slf4j/jcl-over-slf4j "1.7.25"]
                 ;[org.slf4j/log4j-over-slf4j "1.7.26"]
                 [metrics-clojure "2.10.0"]
                 [metrics-clojure-jvm "2.10.0"]
                 [metrics-clojure-health "2.10.0"]
                 [metrics-clojure-graphite "2.10.0"]
                 [metrics-clojure-ring "2.10.0"]
                 [com.github.vertical-blank/sql-formatter "1.0.3"]
                 [org.clojars.rutledgepaulv/websocket-layer "0.1.11"]
                 [com.fasterxml.jackson.core/jackson-core "2.14.0-rc1"]
                 [io.pedestal/pedestal.service "0.5.10"]
                 [io.pedestal/pedestal.jetty "0.5.10"]]
  :jvm-opts ["--add-opens=jdk.management/com.sun.management.internal=ALL-UNNAMED" ;; suppresses codahale.metrics msg in Java 11
             "--add-opens=java.base/java.nio=ALL-UNNAMED"
             "-Xmx28g" ;; testing with 48g
             "-Dcom.sun.management.jmxremote=true"
             "-Dcom.sun.management.jmxremote.port=8989"
             "-Dcom.sun.management.jmxremote.host=0.0.0.0"
             "-Dcom.sun.management.jmxremote.rmi.port=8989"
             "-Dcom.sun.management.jmxremote.local.port=8990"
             "-Dcom.sun.management.jmxremote.authenticate=false"
             "-Dcom.sun.management.jmxremote.ssl=false"
             "--add-opens=java.base/sun.nio.ch=ALL-UNNAMED"] ;; datalevin specific for Java 11+ (eliminates warnings)
  :cljfmt {}
  :source-paths ["src" "../shared"]
  :repl-options {:init-ns rvbbit-backend.core}
  :warn-on-reflection false
  :main ^{:skip-aot true} rvbbit-backend.core ;; not for library use?
  )
