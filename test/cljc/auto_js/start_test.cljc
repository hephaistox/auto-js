(ns auto-js.start-test
  (:require
   [auto-js.start :as sut]
   #?(:clj [clojure.test :refer [deftest is]]
      :cljs [cljs.test :refer [deftest is] :include-macros true])))

(deftest entity-test
  (is (nil? (sut/entity {} 0)) "Non existing step is noop")
  (is (nil? (sut/entity {:step :wait-stock
                         :next-bucket 3}
                        2))
      "Is noop if entity next bucket is not at the current bucket")
  (is (= {:step :prod-start
          :current-operation {:m :a
                              :pt 3}
          :next-bucket 2}
         (sut/entity {:step :wait-stock
                      :current-operation {:m :a
                                          :pt 3}
                      :next-bucket 2}
                     2))
      "Wait-stock starts the production right now --TODO will manage resources later on")
  (is (= {:step :prod-end
          :current-operation {:m :a
                              :pt 3}
          :next-bucket 5}
         (sut/entity {:step :prod-start
                      :current-operation {:m :a
                                          :pt 3}
                      :next-bucket 2}
                     2))
      "Prod-start triggers the end in `pt` buckets")
  (is (= {:step :wait-stock
          :next-ops [:rest-of-next-ops]
          :current-operation {:m :b
                              :pt 7}
          :next-bucket 2}
         (sut/entity {:step :prod-end
                      :next-ops [{:m :b
                                  :pt 7}
                                 :rest-of-next-ops]
                      :current-operation {:m :a
                                          :pt 3}
                      :next-bucket 2}
                     2))
      "Update to the next operations, same bucket"))


(deftest next-entity-bucket-test
  (is (= [7 3]
         (sut/next-entity-bucket {"p0" {:next-bucket 7}
                                  "p1" {:next-bucket 3}}))))
