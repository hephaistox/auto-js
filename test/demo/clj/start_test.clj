(ns start-test
  (:require
   [auto-js.sim.activity  :as sim-activity]
   [auto-js.sim.js        :as sim-js]
   [auto-js.sim.js.xchart :as sim-js-xchart]
   [auto-js.start         :as js-start]))

(-> (sim-js/run js-start/data 55)
    sim-activity/print-output)
(sim-js/synthesis js-start/model)
(sim-js-xchart/build-charts [sim-js-xchart/products-input-output
                             sim-js-xchart/products-nb
                             sim-js-xchart/machine-occupation
                             sim-js-xchart/input-stock
                             sim-js-xchart/entity-throughput]
                            (sim-js/synthesis js-start/model)
                            {})
