(ns auto-js.start
  (:require
   [auto-js.sim.activity    :as sim-activity]
   [auto-opti.prng          :refer [xoroshiro128]]
   [auto-opti.tb            :as opt-tb]
   [com.hypirion.clj-xchart :as c]))

(def u #uuid "0b0a9886-3530-4c55-9330-e0a49111d45b")

(defn- find-next-bucket
  [model]
  (some->> [(sim-activity/next-event-bucket model) (sim-activity/resource-next-event model)]
           (filter some?)
           seq
           (apply min)))

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

;; ********************************************************************************
;; Tests
;; ********************************************************************************

(def data
  {:routes {:blue {:operations [{:m :m4
                                 :pt {:dstb-name :normal-integer
                                      :location 18
                                      :scale 0.5}}
                                {:m :m2
                                 :pt {:dstb-name :normal-integer
                                      :location 19
                                      :scale 0.5}}
                                {:m :m1
                                 :pt {:dstb-name :normal-integer
                                      :location 20
                                      :scale 0.5}}]
                   :probability 0.3}
            :purple {:operations [{:m :m4
                                   :pt {:dstb-name :normal-integer
                                        :location 18
                                        :scale 0.5}}
                                  {:m :m3
                                   :pt {:dstb-name :normal-integer
                                        :location 22
                                        :scale 0.5}}
                                  {:m :m1
                                   :pt {:dstb-name :normal-integer
                                        :location 10
                                        :scale 0.5}}]
                     :probability 0.7}}
   :entity-sources {:product {:nb-max 200
                              :create-fn (fn [_model entity-source]
                                           {:entity-id (str "P" (inc (:nb-entity entity-source)))})
                              :waiting-time {:location 20
                                             :scale 10
                                             :dstb-name :normal-integer}}}
   :route-distribution {:dst-name :categorical}
   :resources {:m1 {}
               :m2 {}
               :m3 {}
               :m4 {}}
   :seed #uuid "e85427c1-ed25-4ed4-9b11-52238d268265"})

(def model (run data 30000))

(defn to-xchart-serie
  [tb-var start end]
  (let [v (->> (opt-tb/clamp tb-var start end)
               :deltas
               vec
               (sort-by first))]
    {:x (map first v)
     :style {:marker-type :none}
     :y (map second v)}))

(defn to-xchart-stats [tb-var start end] (opt-tb/stats tb-var start end))

(def common-opts {:legend {}})
(def start 0)
(def end 1000)

(defn chart
  [model]
  (let [{:keys [bucket stats]} model
        start 0
        end bucket
        common-opts {:legend {}}]
    (c/view
     (c/xy-chart {"Products in" (to-xchart-serie (:entities-in stats) start end)
                  "Products out" (to-xchart-serie (:entities-out stats) start end)}
                 (merge common-opts {:title "Products input output"}))
     (c/xy-chart {"Products nb" (to-xchart-serie (:entities-nb stats) start end)}
                 (merge common-opts {:title "Throughput"}))
     (c/xy-chart (->> model
                      :stats
                      :resources
                      (mapv (fn [[resource-id resource]] [(name resource-id)
                                                          (-> resource
                                                              :occupation
                                                              opt-tb/moving-average
                                                              (opt-tb/clamp (+ 200 start) end)
                                                              (to-xchart-serie start end))]))
                      (into {}))
                 (merge common-opts
                        {:render-style :area
                         :legend {:position :outside-e}
                         :title "Machine occupation"}))
     (c/category-chart (c/transpose-map (->> model
                                             :stats
                                             :resources
                                             (mapv (fn [[machine-id machine]]
                                                     [(name machine-id)
                                                      (update-keys (if (:nb-in-stock machine)
                                                                     (-> machine
                                                                         :nb-in-stock
                                                                         (to-xchart-stats start end)
                                                                         (dissoc :mean))
                                                                     {:min 0
                                                                      :max 0})
                                                                   name)]))
                                             (into {})))
                       (merge common-opts
                              {:title "nb in input stock"
                               :y-axis {:tick-mark-spacing-hint 200}}))
     (c/category-chart (c/transpose-map (->> model
                                             :stats
                                             :resources
                                             (mapv (fn [[machine-id machine]]
                                                     [(name machine-id)
                                                      (if (:nb-in-stock machine)
                                                        {"mean" (-> machine
                                                                    :nb-in-stock
                                                                    (to-xchart-stats start end)
                                                                    :mean)}
                                                        {"mean" 0})]))
                                             (into {})))
                       (merge common-opts
                              {:title "Average in stock"
                               :legend {:visible? false}
                               :y-axis {:tick-mark-spacing-hint 200}})))))

(comment
  (-> (run data 55)
      sim-activity/print-output)
  (-> (run data 30000)
      chart)
  ;
)
