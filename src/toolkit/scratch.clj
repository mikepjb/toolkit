(ns toolkit.scratch
  (:require [clojure.spec.alpha :as s]
            [clojure.pprint :refer [pprint]]))

(def recipe {:ingredients ["aubergine"]
             :steps       ["half the aubergine"
                           "slice the exposed flesh"
                           "salt the flesh"]})

(s/valid? map? recipe)

(assoc recipe :author "Mike")
(dissoc recipe :steps)

(def people [{:name "Brad"     :salary 27000 :department "hats"}
             {:name "Janet"    :salary 54000 :department "capes"}
             {:name "Eddie"    :salary 19500 :department "hats"}
             {:name "Frank"    :salary 98000 :department "capes"}
             {:name "Rocky"    :salary 18000 :department "shoes"}])

(defn highly-paid? [x]
  (< 30000 (:salary x)))

(pprint
 (group-by (juxt :department highly-paid?)
           people))
