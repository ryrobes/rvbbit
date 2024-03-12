(defproject formatter "0.1.0-SNAPSHOT"
  :description "A minimal project to run cljfmt on a Shadow-cljs project"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :plugins [[lein-cljfmt "0.6.8"]
           [lein-ancient "0.6.15"]]

  :cljfmt {
    ;;:indents ^:replace {#".*" [[:inner 0]]}
  }
)

