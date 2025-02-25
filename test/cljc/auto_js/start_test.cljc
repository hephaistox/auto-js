(ns auto-js.start-test
  (:require
   [auto-js.start :as sut]
   #?(:clj [clojure.test :refer [deftest is]]
      :cljs [cljs.test :refer [deftest is] :include-macros true])))

(deftest occupation-s-test
  (is (= "50.00%"
         (sut/occupation-s {:cumulated-time 10
                            :machine-start 20}
                           20)))
  (is (= "60.00%"
         (sut/occupation-s {:cumulated-time 10
                            :machine-start 20}
                           25))
      "Time spend busy since last machine-start is counted in the occupation"))
