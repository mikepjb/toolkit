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

(defn parse-json [n]
  (js->clj (JSON.parse n) :keywordize-keys true))

(defn describe-log-streams
  "Returns a list of log streams in Amazon CloudWatch given a group-name."
  [group-name]
  (:logStreams
   (parse-json
    (:out
     (sh "aws" "logs" "describe-log-streams"
         "--log-group-name" group-name
         "--order-by" "LastEventTime"
         "--descending"
         "--limit" "25")))))

(defn log-stream
  "Returns the logs, given a group-name and stream-name."
  [group-name stream-name]
  (-> (sh "aws" "logs" "get-log-events" "--log-group-name" group-name "--log-stream-name" stream-name)
      :out
      parse-json
      :events))

(comment
  (def log-streams (describe-log-streams "iceberg"))
  (def example-stream (log-stream "iceberg" (:logStreamName (second results))))
  (doseq [l (map :message example-stream)] (println l))
  (require 'cljs.pprint)
  (cljs.pprint/pprint (map :message (log-stream "iceberg" (:logStreamName (first log-streams))))))
