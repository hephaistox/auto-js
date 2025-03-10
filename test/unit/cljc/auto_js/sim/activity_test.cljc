(ns auto-js.sim.activity-test
  (:require
   [auto-js.sim.activity :as sut]
   [auto-opti.prng       :as opt-prng]
   #?(:clj [clojure.test :refer [deftest is]]
      :cljs [cljs.test :refer [deftest is] :include-macros true])))

(def u #uuid "0b0a9886-3530-4c55-9330-e0a49111d45b")

;; ********************************************************************************
;; ** Start
;; ********************************************************************************

(deftest start-test
  (is (= {:bucket 0
          :entities {}
          :entity-sources {}
          :errors []
          :it 0
          :past-events []
          :resources {}
          :route-dstb true
          :routes {}}
         (-> {}
             (sut/start (opt-prng/xoroshiro128 u))
             (update :route-dstb some?)))
      "No entity source to start")
  (is (= {:bucket 0
          :entities {}
          :entity-sources {:product {:next-event 0
                                     :entity-source-id :product
                                     :nb-max 1
                                     :waiting-time 2
                                     :nb-entity 0}}
          :errors []
          :it 0
          :past-events []
          :resources {}
          :route-dstb true
          :routes {}}
         (-> {:entity-sources {:product {:waiting-time 2}}}
             (sut/start nil)
             (update :route-dstb some?)))
      "A simple entity-source"))

(deftest errors-test
  (is (= {:errors [{:error-id :no-source-id}]}
         (-> {}
             sut/errors))
      "No entity source to errors")
  (is (= {:entity-sources {:product {}}}
         (-> {:entity-sources {:product {}}}
             sut/errors))
      "A simple entity-source"))

;; ********************************************************************************
;; ** Entitites
;; ********************************************************************************

(deftest create-new-entity-test
  (is (= {:model {:errors [{:error-id :create-unkown-entity-id
                            :entity-source-id nil
                            :known-ids []}]}}
         (sut/create-new-entity {} nil))
      "Creation of an unknown entity raises an error")
  (is
   (= {:model {:bucket 3
               :entities {"product-0" {:entity-id "product-0"}}
               :entity-sources {:product {:entity-source-id :product
                                          :nb-entity 1
                                          :nb-max 2
                                          :next-event 16
                                          :waiting-time true}}
               :errors []
               :it 0
               :past-events [{:bucket 3
                              :entity-id "product-0"
                              :event-id :create-new-entity
                              :entity-source-id :product
                              :nb-entity 0
                              :nb-max 2
                              :waiting-time 13}]
               :resources {}
               :route-dstb true
               :routes {}}
       :entity {:entity-id "product-0"}}
      (-> {:entity-sources {:product {:next-event 666
                                      :nb-max 2
                                      :waiting-time {:dstb-name :normal-integer
                                                     :scale 2
                                                     :location 13}
                                      :nb-entity 0}}
           :bucket 3}
          (sut/start (opt-prng/xoroshiro128 u))
          (sut/create-new-entity :product)
          (update-in [:model :route-dstb] some?)
          (update-in [:model :entity-sources] update-vals #(update % :waiting-time some?))))
   "Create an entity, some remains")
  (is (= {:model {:bucket 3
                  :entities {"product-0" {:entity-id "product-0"}}
                  :entity-sources {}
                  :errors []
                  :it 0
                  :past-events [{:bucket 3
                                 :entity-id "product-0"
                                 :event-id :create-new-entity
                                 :entity-source-id :product
                                 :last? true
                                 :nb-entity 0
                                 :nb-max 1}]
                  :resources {}
                  :route-dstb true
                  :routes {}}
          :entity {:entity-id "product-0"}}
         (-> {:entity-sources {:product {:next-event 666
                                         :nb-max 1
                                         :waiting-time {:dstb-name :normal-integer
                                                        :scale 2
                                                        :location 13}}}
              :bucket 3}
             (sut/start (opt-prng/xoroshiro128 u))
             (sut/create-new-entity :product)
             (update-in [:model :route-dstb] some?)))
      "Create the last entity"))

(deftest destroy-new-entity-test
  (is (= {:bucket 0
          :entities {:a 1
                     :b 2}
          :errors [{:error-id :destroy-unkown-entity-id
                    :entity-id :not-existing
                    :known-ids [:a :b]}]
          :past-events [{:event-id :destroy-entity
                         :entity-id :not-existing
                         :bucket 0}]}
         (-> {:bucket 0
              :entities {:a 1
                         :b 2}}
             (sut/destroy-entity {:entity-id :not-existing})))
      "Destroy a non existing entity")
  (is (= {:entities {:b 2}}
         (sut/destroy-entity {:entities {:a 1
                                         :b 2}}
                             {:entity-id :a}))
      "Destroy an existing entity"))

(deftest on-new-entity-test
  (is (nil? (sut/on-new-entity {})) "If there is no bucket and date, no entity is created.")
  (is (= {:bucket 10
          :entity-sources {:product {:next-event 12}}}
         (sut/on-new-entity {:bucket 10
                             :entity-sources {:product {:next-event 12}}}))
      "If current bucket is too early for entity creation.")
  (is (= {:bucket 19
          :entity-sources {:product {:next-event 12}}
          :errors [{:error-id :next-event-missed
                    :entity-source-id :product}]}
         (sut/on-new-entity {:bucket 19
                             :entity-sources {:product {:next-event 12}}}))
      "Error if current bucket is too late for entity creation.")
  (is (= {:bucket 3
          :entity-sources {:tst {:waiting-time 3
                                 :next-event 6
                                 :nb-entity 1
                                 :entity-source-id :tst
                                 :nb-max 2}}
          :route-dstb true
          :routes {}
          :it 0
          :resources {}
          :errors []
          :past-events [{:bucket 3
                         :entity-id "tst-0"
                         :event-id :create-new-entity
                         :entity-source-id :tst
                         :nb-entity 0
                         :nb-max 2
                         :waiting-time 3}]
          :entities {"tst-0" {:entity-id "tst-0"}}}
         (-> {:bucket 3
              :entity-sources {:tst {:waiting-time 3
                                     :nb-max 2}}}
             (sut/start (opt-prng/xoroshiro128 u))
             sut/on-new-entity
             (update :route-dstb some?)))
      "Create an entity"))

(deftest next-event-bucket-test
  (is (nil? (sut/next-event-bucket {})) "Empty model has no next bucket")
  (is (= 12
         (sut/next-event-bucket {:entity-sources {:tst {:next-event 12}
                                                  :tst2 {:next-event 13}}}))))

;; ********************************************************************************
;; ** Route
;; ********************************************************************************

(deftest end-route-test
  (is (= {:errors [{:error-id :route-ends-was-not-started
                    :entity {}}]}
         (sut/end-route {} {}))
      "Not started entity raise an error when ended")
  (is (= {:entities {}
          :bucket 112
          :stats {:routes {:a {:throughputs [100]}}
                  :products-out {:deltas {112 1}
                                 :start 112
                                 :end 112}}}
         (-> (sut/end-route {:entities {:e {:foo :bar}}
                             :bucket 112}
                            {:starts 12
                             :entity-id :e
                             :route-id :a})
             (update-in [:stats :routes] #(into {} %))
             (update-in [:stats :products-out] #(into {} %))))
      "Ending a route is deleting the entity, and updating stats"))

(deftest next-op-test
  (is (= {:entities {}
          :bucket 12
          :stats {:routes {:r1 {:throughputs [9]}}
                  :products-out {:deltas {12 1}
                                 :start 12
                                 :end 12}}}
         (-> {:entities {:e {}}
              :bucket 12}
             (sut/next-op {:entity-id :e
                           :route-id :r1
                           :starts 3})
             (update-in [:stats :products-out] #(into {} %))))
      "When no next-ops, it is ending the route")
  (is (= {:entities {:e1 {:entity-id :e1
                          :route-id :r1
                          :next-ops [{:e :f}]
                          :current-operation {:c :d}
                          :starts 3}}}
         (-> {}
             (sut/next-op {:entity-id :e1
                           :route-id :r1
                           :next-ops [{:c :d} {:e :f}]
                           :current-operation {:a :b}
                           :starts 3})))
      "next-op with 2 remaining operations"))

(deftest start-route-test
  (is (= {:bucket 2
          :routes {:route-a {:operations [{:a :b} {:c :d}]}}
          :stats {:products-in {:deltas {2 1}
                                :start 2
                                :end 2}}
          :past-events [{:event-id :start-route
                         :bucket 2
                         :entity-id :a
                         :route-id :route-a}]
          :entities {:a {:route-id :route-a
                         :starts 2
                         :entity-id :a
                         :current-operation {:a :b}
                         :next-ops [{:c :d}]}}}
         (-> {:bucket 2
              :routes {:route-a {:operations [{:a :b} {:c :d}]}}}
             (sut/start-route {:entity-id :a} :route-a)
             (update-in [:stats :products-in] #(into {} %))))
      "A normal operation to start")
  (is (= {:errors [{:error-id :route-not-existing
                    :route-id :non-existing-route-id}]}
         (-> {}
             (sut/start-route {} :non-existing-route-id)))
      "Non existing route creates an error"))

(deftest on-new-product-test
  (is
   (=
    {:bucket 12
     :entities {"p-0" {:entity-id "p-0"
                       :route-id :a
                       :starts 12
                       :next-ops nil
                       :step :in-production
                       :current-operation {:m :m1
                                           :pt 1}}}
     :entity-sources {:p {:next-event 14
                          :entity-source-id :p
                          :waiting-time 2
                          :nb-entity 1
                          :nb-max 2}}
     :errors []
     :it 0
     :past-events [{:bucket 12
                    :entity-id "p-0"
                    :event-id :create-new-entity
                    :entity-source-id :p
                    :nb-entity 0
                    :nb-max 2
                    :waiting-time 2}
                   {:event-id :start-route
                    :bucket 12
                    :entity-id "p-0"
                    :route-id :a}
                   {:event-id :enter-production
                    :bucket 12
                    :entity-id "p-0"
                    :current-operation {:m :m1
                                        :pt 1}
                    :machine {:starts 12
                              :ends 13
                              :next-event 13}}]
     :resources {:m1 {:starts 12
                      :ends 13
                      :next-event 13}}
     :routes {:a {:route-id :a
                  :operations [{:m :m1
                                :pt 1}]}}
     :stats {:products-in true
             :resources {:m1 {:occupation true}}}}
    (-> {:bucket 12
         :resources {:m1 {}}
         :entity-sources {:p {:next-event 12
                              :nb-max 2
                              :waiting-time 2
                              :entity-source-id :p}}
         :routes {:a {:operations [{:m :m1
                                    :pt 1}]}}}
        (sut/start nil)
        sut/on-new-product
        (dissoc :route-dstb)
        (update-in [:stats :products-in] some?)
        (update-in [:stats :resources :m1 :occupation] some?)))
   "Creates a new product. Entity source is not ended.")
  (is (= {:bucket 15
          :entity-sources {:p {:next-event 12}}
          :errors [{:error-id :next-event-missed
                    :entity-source-id nil}]}
         (-> {:bucket 15
              :entity-sources {:p {:next-event 12}}}
             sut/on-new-product))
      "Inconsistency detected")
  (is (= {:bucket 1
          :entity-sources {:p {:next-event 12}}}
         (-> {:bucket 1
              :entity-sources {:p {:next-event 12}}}
             sut/on-new-product))
      "Creation is for later on"))


;; ********************************************************************************
;; ** Machine
;; ********************************************************************************

(deftest enter-production-test
  (is (= {:errors [{:error-id :current-operation-missing
                    :entity {}}]}
         (-> {}
             (sut/enter-production {})))
      "Need a current operation")
  (is (= {:resources {:m1 {:starts 10}}
          :errors [{:error-id :machine-is-busy-already
                    :machine {:starts 10}}]}
         (-> {:resources {:m1 {:starts 10}}}
             (sut/enter-production {:current-operation {:m :m1}})))
      "Can't start if machine is already busy")
  (is (= {:resources {:m1 {:starts 12
                           :foo :bar
                           :ends 13
                           :next-event 13}}
          :bucket 12
          :past-events [{:bucket 12
                         :entity-id :e1
                         :event-id :enter-production
                         :machine {:starts 12
                                   :ends 13
                                   :foo :bar
                                   :next-event 13}
                         :current-operation {:m :m1
                                             :pt 1}}]
          :stats {:resources {:m1 {:occupation {:deltas {12 1}
                                                :start 12
                                                :end 12}}}}
          :entities {:e1 {:step :in-production}}}
         (-> {:resources {:m1 {:foo :bar}}
              :bucket 12}
             (sut/enter-production {:current-operation {:m :m1
                                                        :pt 1}
                                    :entity-id :e1})
             (update-in [:stats :resources :m1 :occupation] #(into {} %))))
      "Starts on an available machine"))

(deftest enter-input-stock-test
  (is (= {:errors [{:error-id :no-current-operation
                    :entity {}}]}
         (-> {}
             (sut/enter-input-stock {})))
      "No current operation is an error")
  (is
   (=
    {:bucket 0
     :entities {:e1 {:step :in-production}}
     :entity-sources {:product {:entity-source-id :product
                                :nb-entity 0
                                :nb-max 1
                                :next-event 0
                                :waiting-time true}}
     :errors []
     :it 0
     :past-events [{:bucket 0
                    :current-operation {:m :m1
                                        :pt 1}
                    :entity-id :e1
                    :event-id :enter-production
                    :machine {:starts 0
                              :ends 1
                              :next-event 1}}]
     :resources {:m1 {:starts 0
                      :ends 1
                      :next-event 1}}
     :routes {}
     :stats {:resources {:m1 {:occupation {:deltas {0 1}
                                           :start 0
                                           :end 0}}}}}
    (-> {:resources {:m1 {}}
         :entity-sources {:product {}}}
        (sut/start nil)
        (sut/enter-input-stock {:current-operation {:m :m1
                                                    :pt 1}
                                :waiting-time true
                                :entity-id :e1})
        (dissoc :route-dstb)
        (update-in [:entity-sources :product :waiting-time] some?)
        (update-in [:stats :resources :m1 :occupation] #(into {} %))))
   "If no production is in progress, starts directly the production")
  (is (= {:bucket 0
          :it 0
          :entity-sources {}
          :entities {:e1 {:step :wait-in-input-stock}}
          :errors []
          :past-events [{:bucket 0
                         :current-operation {:m :m1}
                         :entity-id :e1
                         :event-id :enter-input-stock-wait-in-stock
                         :machine {:starts 1
                                   :waiting-products [:e1]}}]
          :resources {:m1 {:starts 1
                           :waiting-products [:e1]}}
          :routes {}
          :stats {:resources {:m1 {:nb-in-stock {:deltas {0 1}
                                                 :start 0
                                                 :end 0}}}}}
         (-> {:resources {:m1 {:starts 1}}}
             (sut/start nil)
             (sut/enter-input-stock {:current-operation {:m :m1}
                                     :entity-id :e1})
             (dissoc :route-dstb)
             (update-in [:stats :resources :m1 :nb-in-stock] #(into {} %))))
      "Wait in stock if a production is in progress"))

(deftest ends-production-test
  (is (= {:errors [{:error-id :no-current-operation
                    :entity {:entity-id :e1}}]}
         (-> {}
             (sut/ends-production {:entity-id :e1})))
      "No current operation raise an error")
  (is (= {:bucket 1
          :entities {}
          :past-events [{:bucket 1
                         :current-operation {:m :m1}
                         :entity-id :e1
                         :event-id :ends-production
                         :machine {:starts 1
                                   :ends 2
                                   :next-event 2}}]
          :resources {:m1 {}}
          :stats {:routes {:r1 {:throughputs [1]}}
                  :products-out {:deltas {1 1}
                                 :start 1
                                 :end 1}
                  :resources {:m1 {:occupation {:deltas {1 1}
                                                :start 1
                                                :end 1}}}}}
         (-> {:bucket 1
              :entities {:e1 {:step :foo
                              :entity-id :e1}}
              :resources {:m1 {:starts 1
                               :ends 2
                               :next-event 2}}}
             (sut/ends-production {:current-operation {:m :m1}
                                   :entity-id :e1
                                   :route-id :r1
                                   :starts 0})
             (update-in [:stats :resources :m1 :occupation] #(into {} %))
             (update-in [:stats :products-out] #(into {} %))
             (update-in [:stats :routes :r1 :throughputs] #(into [] %))))
      "Stops production of `e1` on `m1`, no product waits in `m1` stock")
  (is
   (=
    {:bucket 4
     :entities {:e2 {:step :in-production
                     :current-operation {:m :m1
                                         :pt 3}
                     :entity-id :e2}}
     :past-events [{:bucket 4
                    :current-operation {:m :m1
                                        :pt 3}
                    :entity-id :e2
                    :event-id :enter-production
                    :machine {:waiting-products [:e3]
                              :starts 4
                              :ends 7
                              :next-event 7}}
                   {:bucket 4
                    :current-operation {:m :m1}
                    :entity-id :e1
                    :event-id :ends-production
                    :machine {:starts 1
                              :ends 2
                              :waiting-products [:e2 :e3]
                              :next-event 2}}]
     :resources {:m1 {:waiting-products [:e3]
                      :starts 4
                      :ends 7
                      :next-event 7}}
     :stats {:routes {:r1 {:throughputs [3]}}
             :products-out {:deltas {4 1}
                            :start 4
                            :end 4}
             :resources {:m1 {:occupation {:deltas {4 2}
                                           :start 4
                                           :end 4}}}}}
    (-> {:bucket 4
         :entities {:e1 {:step :foo}
                    :e2 {:step :bar
                         :current-operation {:m :m1
                                             :pt 3}
                         :entity-id :e2}}
         :resources {:m1 {:starts 1
                          :ends 2
                          :waiting-products [:e2 :e3]
                          :next-event 2}}}
        (sut/ends-production {:entity-id :e1
                              :route-id :r1
                              :starts 1
                              :current-operation {:m :m1}})
        (update-in [:stats :resources :m1 :occupation] #(into {} %))
        (update-in [:stats :products-out] #(into {} %))
        (update-in [:stats :routes :r1 :throughputs] #(into [] %))))
   "Stops production of `e1` on `m1` with product `e2` waiting in the stock of that machine and ready to start"))

(deftest resource-next-event-test
  (is (nil? (-> {}
                (sut/resource-next-event)))
      "No next event")
  (is (= 13
         (-> {:resources {:a {:next-event 13}
                          :b {:next-event 15}}}
             (sut/resource-next-event)))
      "Next resource event is 13, the smallest date"))

;; ********************************************************************************
;; ** jobshop specific
;; ********************************************************************************

#_(deftest find-next-bucket-test
    (is (nil? (sut/find-next-bucket nil)) "An empty model returns no next bucket.")
    (is (= 12
           (sut/find-next-bucket {:entity-sources {:tst {:next-event 12}
                                                   :tst2 {:next-event 13}}}))
        "A new entity will start at 12")
    (is (= 13 (sut/find-next-bucket {:resources {:a {:next-event 13}}})) "One machine resource")
    (is (= 10
           (sut/find-next-bucket {:resources {:a {:next-event 13}
                                              :b {:next-event 10}}}))
        "The minimum is returned")
    (is (= 5
           (sut/find-next-bucket {:resources {:a {:next-event 13}
                                              :b {:next-event 10}}
                                  :entity-sources {:p {:next-event 5}}}))
        "The minimum is `next-product`")
    (is (= 10
           (sut/find-next-bucket {:resources {:a {:next-event 13}
                                              :b {:next-event 10}}
                                  :entity-sources {:p {:next-event 50}}}))
        "The minimum is a `machine-end`"))
