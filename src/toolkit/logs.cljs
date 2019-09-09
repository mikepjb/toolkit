(ns toolkit.logs
  "Collects logs using the AWS cli"
  (:require ["child_process" :as child-process] ;; load node library
            [clojure.string :refer [split]])) 

(defn sh
  "Shell wrapper around child-process, belongs in it's own namespace."
  [cmd & args]
  (let [process (child-process/spawnSync cmd (clj->js args))]
    {:exit process.status
     :out  (js->clj (split (.toString process.stdout) #"\n"))
     :err  (js->clj (split (.toString process.stderr) #"\n"))}))

;; (defn proc-test []
;;   (child-process/exec
;;    "cat ~/.bashrc"
;;    (defn [js-err out err]))
;;   {:out (.stdout)})
