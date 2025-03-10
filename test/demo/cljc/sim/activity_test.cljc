(ns sim.activity-test
  "Test a jobshop defined as an activity simulation"
  (:require
   [auto-core.string.format :refer [f pfln]]
   [auto-js.sim.activity    :as sim-activity]
   #?(:clj [clojure.test :refer [deftest is]]
      :cljs [cljs.test :refer [deftest is] :include-macros true])
   [auto-opti.prng          :as opt-prng]))

(def data
  {:max-nb-entity 100
   :routes {:blue {:operations [{:m :m4
                                 :pt {:dstb-name :normal
                                      :location 20
                                      :scale 0.2}}
                                {:m :m2
                                 :pt {:dstb-name :normal
                                      :location 30
                                      :scale 0.2}}
                                {:m :m1
                                 :pt {:dstb-name :normal
                                      :location 20
                                      :scale 0.2}}]
                   :category-probability 0.2}
            :purple {:operations [{:m :m4
                                   :pt {:dstb-name :normal
                                        :location 20
                                        :scale 0.2}}
                                  {:m :m3
                                   :pt {:dstb-name :normal
                                        :location 30
                                        :scale 0.2}}
                                  {:m :m1
                                   :pt {:dstb-name :normal
                                        :location 10
                                        :scale 0.2}}]
                     :category-probability 0.2}}
   :starting-bucket 0
   :waiting-time {:location 3
                  :scale 0.2
                  :dstb-name :normal}
   :resource-input {:m1 {}
                    :m2 {}
                    :m3 {}
                    :m4 {}}
   :seed #uuid "e85427c1-ed25-4ed4-9b11-52238d268265"})
