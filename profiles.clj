;; {:user {:plugins [[refactor-nrepl "1.1.0"]
;;                   [cider/cider-nrepl "0.12.0-SNAPSHOT"]]
;;         :dependencies [[org.clojure/tools.nrepl "0.2.12"]
;;                        [com.cemerick/piggieback "0.2.1"]
;;                        [figwheel-sidecar "0.5.0-1"]]
;;         :repl-options {:nrepl-middleware [cemerick.piggieback/wrap-cljs-repl]}}}

{:user {:plugins [[refactor-nrepl "2.2.0-SNAPSHOT"]
                  [cider/cider-nrepl "0.14.0-SNAPSHOT"]]
        :dependencies [[org.clojure/tools.nrepl "0.2.12"]
                       [com.cemerick/piggieback "0.2.1"]
                       [figwheel-sidecar "0.5.0-1"]]
        :repl-options {:nrepl-middleware [cemerick.piggieback/wrap-cljs-repl]}}}

