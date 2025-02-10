(ns auto-js.rep.repetition-test
  (:require
   [auto-js.pb-data.js-pb   :refer [js-pb]]
   [auto-js.pb-data.map-ops :refer [operations]]
   [auto-js.rep.repetition  :as sut]
   #?(:clj [clojure.test :refer [deftest is]]
      :cljs [cljs.test :refer [deftest is] :include-macros true])))

(deftest canonical-test
  (is (= [:J1 :J1 :J1 :J2 :J2 :J2]
         (-> (operations {:J1 [{:p 9
                                :m :M0}
                               {:p 9
                                :m :M11}
                               {:p 4
                                :m :M2}]
                          :J2 [{:p 16
                                :m :M0}
                               {:p 4
                                :m :M1}
                               {:p 15
                                :m :M2}]})
             (js-pb :test-instance)
             (sut/topological-order [:J1 :J1 :J1 :J2 :J2 :J2])
             sut/canonical))
      "Canonical"))

(deftest decode-rep-test
  (is
   (=
    [{:p 9
      :m :M0
      :n :J1}
     {:p 9
      :m :M11
      :n :J1}
     {:p 4
      :m :M2
      :n :J1}
     {:p 16
      :m :M0
      :n :J2}
     {:p 4
      :m :M1
      :n :J2}
     {:p 15
      :m :M2
      :n :J2}]
    (-> (operations {:J1 [{:p 9
                           :m :M0}
                          {:p 9
                           :m :M11}
                          {:p 4
                           :m :M2}]
                     :J2 [{:p 16
                           :m :M0}
                          {:p 4
                           :m :M1}
                          {:p 15
                           :m :M2}]})
        (js-pb :test-instance)
        (sut/topological-order [:J1 :J1 :J1 :J2 :J2 :J2])))))
