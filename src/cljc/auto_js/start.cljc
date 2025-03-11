(ns auto-js.start
  (:require
   [auto-js.sim.activity :as sim-activity]
   [auto-opti.prng       :refer [xoroshiro128]]))

(def u #uuid "0b0a9886-3530-4c55-9330-e0a49111d45b")

(defn- find-next-bucket
  [model]
  (some->> [(sim-activity/next-event-bucket model) (sim-activity/resource-next-event model)]
           (filter some?)
           seq
           (apply min)))

(defn run
  "Run the jobshop model described in `model` until it `it-stop`"
  [model it-stop uuid]
  (loop [model (-> model
                   (sim-activity/start (xoroshiro128 uuid))
                   sim-activity/add-iteration-past-event
                   sim-activity/errors)]
    (if (seq (:errors model))
      model
      (let [{:keys [it bucket]} model
            new-model (or (sim-activity/on-new-product model)
                          (sim-activity/on-production-end model))
            next-bucket (find-next-bucket new-model)]
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
          :else (-> new-model
                    sim-activity/add-snapshot
                    (sim-activity/add-iteration next-bucket)
                    recur))))))

;; ********************************************************************************
;; Tests
;; ********************************************************************************

(def data
  {:routes {:blue {:operations [{:m :m4
                                 :pt {:dstb-name :normal-integer
                                      :location 18
                                      :scale 0.2}}
                                {:m :m2
                                 :pt {:dstb-name :normal-integer
                                      :location 30
                                      :scale 0.2}}
                                {:m :m1
                                 :pt {:dstb-name :normal-integer
                                      :location 20
                                      :scale 0.2}}]
                   :probability 0.3}
            :purple {:operations [{:m :m4
                                   :pt {:dstb-name :normal-integer
                                        :location 18
                                        :scale 0.2}}
                                  {:m :m3
                                   :pt {:dstb-name :normal-integer
                                        :location 30
                                        :scale 0.2}}
                                  {:m :m1
                                   :pt {:dstb-name :normal-integer
                                        :location 10
                                        :scale 0.2}}]
                     :probability 0.7}}
   :entity-sources {:product {:nb-max 200
                              :create-fn (fn [_model entity-source]
                                           {:entity-id (str "P" (inc (:nb-entity entity-source)))})
                              :waiting-time {:location 10
                                             :scale 0.2
                                             :dstb-name :normal-integer}}}
   :route-distribution {:dst-name :categorical}
   :resources {:m1 {}
               :m2 {}
               :m3 {}
               :m4 {}}
   :seed #uuid "e85427c1-ed25-4ed4-9b11-52238d268265"})

(-> data
    (run 30000 u)
    :stats
    ;sim-activity/print-output
    ;;
)
