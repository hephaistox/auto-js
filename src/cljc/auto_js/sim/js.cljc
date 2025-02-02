(ns auto-js.sim.js
  "Simple jobshop with input stock"
  (:require
   [auto-js.sim.activity :as sim-activity]
   [auto-opti.prng       :refer [xoroshiro128]]
   [auto-opti.sample     :as opt-sample]
   [auto-opti.tb         :as opt-tb]))

;; ********************************************************************************
;; Simulation runner
;; ********************************************************************************

(defn run
  "Run the jobshop model described in `model` until it `it-stop`"
  [model it-stop]
  (loop [model (-> model
                   (sim-activity/start (xoroshiro128 (:seed model)))
                   sim-activity/add-iteration-past-event
                   sim-activity/errors)]
    (if (seq (:errors model))
      model
      (let [{:keys [it bucket]} model
            new-model (or (sim-activity/on-new-product model)
                          (sim-activity/on-production-end model))
            next-bucket (some->> [(sim-activity/next-event-bucket new-model)
                                  (sim-activity/resource-next-event new-model)]
                                 (filter some?)
                                 seq
                                 (apply min))]
        (cond
          (nil? new-model) (-> model
                               (update :errors
                                       conj
                                       {:error-id :bucket-has-no-activity
                                        :bucket bucket
                                        :it it}))
          (nil? next-bucket) (-> new-model
                                 (assoc :sim-status :no-next-event))
          (>= it it-stop) (-> new-model
                              (assoc :sim-status :last-iteration-reached))
          (< next-bucket bucket) (-> new-model
                                     (update :errors
                                             conj
                                             {:error-id :causality-broken
                                              :bucket bucket
                                              :next-bucket next-bucket}))
          :else (-> new-model
                    sim-activity/add-snapshot
                    (sim-activity/add-iteration next-bucket)
                    recur))))))

(defn- create-sample
  [interval]
  (fn [sample] (opt-tb/make-level (sort-by first (opt-sample/sampling-rounded sample interval)))))

(defn- create-stats
  [tb-var start end]
  (-> (opt-tb/stats tb-var start end)
      (update-keys name)))

(defn synthesis
  "Builds a synthesis of a `model`"
  [{:keys [bucket stats]
    :as model}]
  (let [{:keys [entities-in entities-out entities-nb resources entities routes]} stats
        start (min bucket (* 10 (sim-activity/first-entity-out model)))
        end (min bucket (sim-activity/last-entity-in model))]
    {:entities-in (create-stats entities-in start end)
     :entities-out (create-stats entities-out start end)
     :entities-nb (create-stats entities-nb start end)
     :resources (-> resources
                    (update-keys name)
                    (update-vals (fn [resource]
                                   (-> resource
                                       (assoc :occupation-ma
                                              (-> (:occupation resource)
                                                  opt-tb/moving-average
                                                  (create-stats start end)))
                                       (update :nb-in-stock create-stats start end)
                                       (update :occupation create-stats start end)))))
     :routes (-> routes
                 (update-keys name)
                 (update-vals (fn [route]
                                (-> route
                                    (update :route-nb create-stats start end)
                                    (update :throughputs (create-sample 50))))))
     :entities (-> entities
                   (update :throughputs (create-sample 50)))
     :start start
     :end end}))
