(ns auto-js.start
  (:require
   [auto-js.sim.activity :as sim-activity]
   [auto-opti.prng       :refer [xoroshiro128]]))

(def u #uuid "0b0a9886-3530-4c55-9330-e0a49111d45b")

(defn find-next-bucket
  "Next bucket to execute the jobshop model.

  It could be the creation of an entity, or the end of a machine process."
  [model]
  (let [v (->> [(sim-activity/next-event-bucket model) (sim-activity/resource-next-event model)]
               (filter some?))]
    (when-not (empty? v) (apply min v))))

(defn run
  "Run the jobshop model described in `model` until it `it-stop`"
  [model it-stop uuid]
  (loop [model (-> model
                   (sim-activity/start (xoroshiro128 uuid))
                   sim-activity/errors)]
    (if (seq (:errors model))
      model
      (let [{:keys [it]} model
            model (-> model
                      sim-activity/on-new-product)
            next-bucket (find-next-bucket model)]
        (cond
          (nil? next-bucket) (-> model
                                 (assoc :sim-status :no-next-event))
          (>= it it-stop) (-> model
                              (assoc :sim-status :last-iteration-reached))
          :else (recur (-> model
                           (assoc :it (inc it) :bucket next-bucket))))))))

;; ********************************************************************************
;; Tests
;; ********************************************************************************

(def data
  {:routes {:blue {:operations [{:m :m4
                                 :pt {:dstb-name :normal-integer
                                      :location 20
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
                                        :location 20
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
   :entity-sources {:product {:nb-max 100
                              :waiting-time {:location 3
                                             :scale 0.2
                                             :dstb-name :normal-integer}}}
   :route-distribution {:dst-name :categorical}
   :resources {:m1 {}
               :m2 {}
               :m3 {}
               :m4 {}}
   :seed #uuid "e85427c1-ed25-4ed4-9b11-52238d268265"})

(-> data
    (run 1 u)
    ;TODO With a limited number of iterations, write the on-
    sim-activity/print-output
    ;;
)

