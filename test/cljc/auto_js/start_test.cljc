(ns auto-js.start-test
  (:require
   [auto-js.start :as sut]
   #?(:clj [clojure.test :refer [deftest is]]
      :cljs [cljs.test :refer [deftest is] :include-macros true])))

(deftest occupation-s-test
  (is (= "(75.00%)" (sut/occupation-s 10 14 16 :simple)))
  (is (= "( 14 -> *, 75.00%)" (sut/occupation-s 10 14 16 :medium)))
  (is (= "( 14 ->  12 , 75.00%)" (sut/occupation-s 10 14 16 :detailed))))
