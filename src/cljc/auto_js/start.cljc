(ns auto-js.start
  (:require
   [auto-js.sim.activity  :as sim-activity]
   [auto-js.sim.js        :as sim-js]
   [auto-js.sim.js.xchart :as sim-js-xchart]))

;; ********************************************************************************
;; Tests
;; ********************************************************************************

(def data
  {:routes {:blue {:operations [{:m :m4
                                 :pt {:dstb-name :normal-integer
                                      :location 10
                                      :scale 5}}
                                {:m :m2
                                 :pt {:dstb-name :normal-integer
                                      :location 51
                                      :scale 5}}
                                {:m :m1
                                 :pt {:dstb-name :normal-integer
                                      :location 20
                                      :scale 5}}]
                   :probability 0.3}
            :purple {:operations [{:m :m4
                                   :pt {:dstb-name :normal-integer
                                        :location 10
                                        :scale 5}}
                                  {:m :m3
                                   :pt {:dstb-name :normal-integer
                                        :location 18
                                        :scale 5}}
                                  {:m :m1
                                   :pt {:dstb-name :normal-integer
                                        :location 10
                                        :scale 5}}]
                     :probability 0.7}}
   :entity-sources {:product {:nb-max 200
                              :create-fn (fn [_model entity-source]
                                           {:entity-id (str "P" (inc (:nb-entity entity-source)))})
                              :waiting-time {:location 10
                                             :scale 10
                                             :dstb-name :normal-integer}}}
   :route-distribution {:dst-name :categorical}
   :resources {:m1 {}
               :m2 {}
               :m3 {}
               :m4 {}}
   :seed #uuid "e85427c1-ed25-4ed4-9b11-52238d268265"})

(def model (sim-js/run data 30000))

(comment
  (-> (sim-js/run data 55)
      sim-activity/print-output)
  (sim-js/synthesis model)
  (sim-js-xchart/build-charts [sim-js-xchart/products-input-output
                               sim-js-xchart/products-nb
                               sim-js-xchart/machine-occupation
                               sim-js-xchart/input-stock
                               sim-js-xchart/entity-throughput]
                              (sim-js/synthesis model)
                              {})
  ;
)
