(ns toolkit.logs
  "Collects logs using the AWS cli"
  (:require ["child_process" :as child-process] ;; load node library
            [clojure.string :refer [split]])) 

(defn sh
  "Shell wrapper around child-process, belongs in it's own namespace."
  [cmd & args]
  (let [process (child-process/spawnSync cmd (clj->js args))]
    {:exit process.status
     :out  (.toString process.stdout)
     :err  (.toString process.stderr)}))

(defn log-streams
  "Returns a list of log streams in Amazon CloudWatch given a group-name."
  [group-name]
  (js->clj
   (JSON.parse
    (:out
     (sh "aws" "logs" "describe-log-streams" "--log-group-name" group-name "--limit" "5"))) :keywordize-keys true))
