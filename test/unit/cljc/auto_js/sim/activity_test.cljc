(ns auto-js.sim.activity-test
  (:require
   [auto-js.sim.activity :as sut]
   [auto-opti.prng       :as opt-prng]
   #?(:clj [clojure.test :refer [deftest is]]
      :cljs [cljs.test :refer [deftest is] :include-macros true])))

(def u #uuid "0b0a9886-3530-4c55-9330-e0a49111d45b")

(defn clojurify
  "Remove all non clojure datastructure"
  [model]
  (cond-> (dissoc model :route-dstb)
    (:stats model) (update-in [:stats] update-vals #(into {} %))
    (:entity-sources model)
    (update :entity-sources
            update-vals
            (fn [entity-source] (update entity-source :waiting-time #(if (number? %) % (some? %)))))
    (get-in model [:stats :resources])
    (update-in [:stats :resources]
               update-vals
               (fn [resource-stat]
                 (cond-> resource-stat
                   (:nb-in-stock resource-stat) (update :nb-in-stock (partial into {}))
                   (:occupation resource-stat) (update :occupation (partial into {})))))
    (get-in model [:stats :routes]) (update-in [:stats :routes]
                                               update-vals
                                               (fn [route-stat]
                                                 (cond-> route-stat
                                                   (:route-nb route-stat)
                                                   (update :route-nb (partial into {})))))))

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
          :routes {}}
         (-> {:entity-sources {:product {:waiting-time 2}}}
             (sut/start nil)
             clojurify))
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
   (=
    {:model {:bucket 3
             :entities {"product-1" {:entity-id "product-1"
                                     :starts 3}}
             :entity-sources {:product {:entity-source-id :product
                                        :nb-entity 1
                                        :nb-max 2
                                        :next-event 16
                                        :waiting-time true}}
             :errors []
             :it 0
             :past-events [{:bucket 3
                            :entity-id "product-1"
                            :event-id :create-new-entity
                            :entity-source-id :product
                            :nb-entity 1
                            :nb-max 2
                            :waiting-time 13}]
             :resources {}
             :routes {}
             :stats {:entities-in {:deltas {3 1}
                                   :start 3
                                   :end 3}
                     :entities-nb {:deltas {3 1}
                                   :start 3
                                   :end 3}}}
     :entity {:entity-id "product-1"
              :starts 3}}
    (-> {:entity-sources {:product {:next-event 666
                                    :nb-max 2
                                    :waiting-time {:dstb-name :normal-integer
                                                   :scale 2
                                                   :location 13}
                                    :nb-entity 0}}
         :bucket 3}
        (sut/start (opt-prng/xoroshiro128 u))
        (sut/create-new-entity :product)
        (update :model clojurify)))
   "Create an entity, some remains")
  (is
   (=
    {:model {:bucket 3
             :entities {"product-1" {:entity-id "product-1"
                                     :starts 3}}
             :entity-sources {}
             :errors []
             :it 0
             :past-events [{:bucket 3
                            :entity-id "product-1"
                            :event-id :create-new-entity
                            :entity-source-id :product
                            :last? true
                            :nb-entity 1
                            :nb-max 1}]
             :resources {}
             :routes {}
             :stats {:entities-in {:deltas {3 1}
                                   :start 3
                                   :end 3}
                     :entities-nb {:deltas {3 1}
                                   :start 3
                                   :end 3}}}
     :entity {:entity-id "product-1"
              :starts 3}}
    (-> {:entity-sources {:product {:next-event 666
                                    :nb-max 1
                                    :waiting-time {:dstb-name :normal-integer
                                                   :scale 2
                                                   :location 13}}}
         :bucket 3}
        (sut/start (opt-prng/xoroshiro128 u))
        (sut/create-new-entity :product)
        (update :model clojurify)))
   "Create the last entity"))

(deftest destroy-entity-test
  (is (= {:bucket 0
          :entities {:a 1
                     :b 2}
          :errors [{:error-id :unkown-entity-id
                    :during :destroy-entity
                    :entity-id :not-existing
                    :known-ids [:a :b]}]}
         (-> {:bucket 0
              :entities {:a 1
                         :b 2}}
             (sut/destroy-entity {:entity-id :not-existing})))
      "Destroy a non existing entity")
  (is (= {:entities {:b 2}
          :bucket 3
          :stats {:entities {:throughputs [0]}
                  :entities-nb {:deltas {3 -1}
                                :start 3
                                :end 3}
                  :entities-out {:deltas {3 1}
                                 :start 3
                                 :end 3}}
          :past-events [{:event-id :destroy-entity
                         :entity-id :a
                         :bucket 3}]}
         (-> {:entities {:a 1
                         :b 2}
              :bucket 3}
             (sut/destroy-entity {:entity-id :a
                                  :starts 3})
             clojurify))
      "Destroy an existing entity"))

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
          :past-events [{:event-id :destroy-entity
                         :entity-id :e
                         :bucket 112}
                        {:event-id :end-route
                         :entity-id :e
                         :bucket 112}]
          :stats {:routes {:a {:throughputs [100]}}
                  :entities {:throughputs [100]}
                  :entities-nb {:deltas {112 -1}
                                :start 112
                                :end 112}
                  :entities-out {:deltas {112 1}
                                 :start 112
                                 :end 112}}}
         (-> {:entities {:e {:foo :bar}}
              :bucket 112}
             (sut/end-route {:starts 12
                             :entity-id :e
                             :route-id :a})
             clojurify))
      "Ending a route is deleting the entity, and updating stats"))

(deftest next-op-test
  (is (= {:entities {}
          :bucket 12
          :past-events [{:event-id :destroy-entity
                         :entity-id :e
                         :bucket 12}
                        {:event-id :end-route
                         :entity-id :e
                         :bucket 12}]
          :stats {:routes {:r1 {:throughputs [9]}}
                  :entities {:throughputs [9]}
                  :entities-nb {:deltas {12 -1}
                                :start 12
                                :end 12}
                  :entities-out {:deltas {12 1}
                                 :start 12
                                 :end 12}}}
         (-> {:entities {:e {}}
              :bucket 12}
             (sut/next-op {:entity-id :e
                           :route-id :r1
                           :starts 3})
             clojurify))
      "When no next-ops, it is ending the route")
  (is (= {:entities {:e1 {:entity-id :e1
                          :route-id :r1
                          :next-ops [{:e :f}]
                          :current-operation {:c :d}
                          :starts 3}}
          :bucket 44
          :past-events [{:event-id :next-op
                         :current-operation {:c :d}
                         :entity-id :e1
                         :bucket 44}]}
         (-> {:bucket 44}
             (sut/next-op {:entity-id :e1
                           :route-id :r1
                           :next-ops [{:c :d} {:e :f}]
                           :current-operation {:a :b}
                           :starts 3})
             clojurify))
      "next-op with 2 remaining operations"))

(deftest start-route-test
  (is (= {:bucket 2
          :routes {:route-a {:operations [{:a :b} {:c :d}]}}
          :stats {:routes {:route-a {:route-nb {:deltas {2 1}
                                                :start 2
                                                :end 2}}}}
          :past-events [{:event-id :next-op
                         :bucket 2
                         :entity-id :a
                         :current-operation {:a :b}}
                        {:event-id :start-route
                         :bucket 2
                         :entity-id :a
                         :route-id :route-a}]
          :entities {:a {:route-id :route-a
                         :entity-id :a
                         :current-operation {:a :b}
                         :next-ops [{:c :d}]}}}
         (-> {:bucket 2
              :routes {:route-a {:operations [{:a :b} {:c :d}]}}}
             (sut/start-route {:entity-id :a} :route-a)
             clojurify))
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
     :entities {"p-1" {:entity-id "p-1"
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
                    :entity-id "p-1"
                    :event-id :create-new-entity
                    :entity-source-id :p
                    :nb-entity 1
                    :nb-max 2
                    :waiting-time 2}
                   {:event-id :start-route
                    :bucket 12
                    :entity-id "p-1"
                    :route-id :a}
                   {:event-id :next-op
                    :entity-id "p-1"
                    :current-operation {:m :m1
                                        :pt 1}
                    :bucket 12}
                   {:event-id :enter-production
                    :bucket 12
                    :entity-id "p-1"
                    :current-operation {:m :m1
                                        :pt 1}
                    :machine {:starts 12
                              :ends 13
                              :resource-id :m1
                              :entity-id "p-1"
                              :next-event 13}}]
     :resources {:m1 {:starts 12
                      :ends 13
                      :entity-id "p-1"
                      :resource-id :m1
                      :next-event 13}}
     :routes {:a {:route-id :a
                  :operations [{:m :m1
                                :pt 1}]}}
     :stats {:entities-nb {:deltas {12 1}
                           :start 12
                           :end 12}
             :entities-in {:deltas {12 1}
                           :start 12
                           :end 12}
             :routes {:a {:route-nb {:deltas {12 1}
                                     :start 12
                                     :end 12}}}
             :resources {:m1 {:occupation {:deltas {12 1}
                                           :start 12
                                           :end 12}}}}}
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
        clojurify))
   "Creates a new product. Entity source is not ended.")
  (is (= {:bucket 15
          :entity-sources {:p {:next-event 12}}
          :errors [{:error-id :next-event-missed
                    :entity-source-id nil}]}
         (-> {:bucket 15
              :entity-sources {:p {:next-event 12}}}
             sut/on-new-product))
      "Inconsistency detected")
  (is (nil? (-> {:bucket 1
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
                           :entity-id :e1
                           :ends 13
                           :next-event 13}}
          :bucket 12
          :past-events [{:bucket 12
                         :entity-id :e1
                         :event-id :enter-production
                         :machine {:starts 12
                                   :ends 13
                                   :foo :bar
                                   :entity-id :e1
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
                              :resource-id :m1
                              :ends 1
                              :entity-id :e1
                              :next-event 1}}]
     :resources {:m1 {:starts 0
                      :ends 1
                      :resource-id :m1
                      :entity-id :e1
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
                                   :resource-id :m1
                                   :waiting-products [:e1]}}]
          :resources {:m1 {:starts 1
                           :resource-id :m1
                           :waiting-products [:e1]}}
          :routes {}
          :stats {:resources {:m1 {:nb-in-stock {:deltas {0 1}
                                                 :start 0
                                                 :end 0}}}}}
         (-> {:resources {:m1 {:starts 1}}}
             (sut/start nil)
             (sut/enter-input-stock {:current-operation {:m :m1}
                                     :entity-id :e1})
             clojurify))
      "Wait in stock if a production is in progress"))

(deftest ends-production-test
  (is (= {:errors [{:error-id :no-current-operation
                    :entity {:entity-id :e1}}]}
         (-> {}
             (sut/ends-production {:entity-id :e1})))
      "No current operation raise an error")
  (is
   (=
    {:bucket 1
     :entities {}
     :past-events [{:event-id :destroy-entity
                    :entity-id :e1
                    :bucket 1}
                   {:bucket 1
                    :entity-id :e1
                    :event-id :end-route}
                   {:bucket 1
                    :current-operation {:m :m1}
                    :entity-id :e1
                    :event-id :ends-production
                    :machine {}}]
     :resources {:m1 {}}
     :stats {:routes {:r1 {:throughputs [1]}}
             :entities-nb {:deltas {1 -1}
                           :start 1
                           :end 1}
             :entities-out {:deltas {1 1}
                            :start 1
                            :end 1}
             :entities {:throughputs [1]}
             :resources {:m1 {:occupation {:deltas {1 -1}
                                           :start 1
                                           :end 1}
                              :nb-in-stock {:deltas {1 -1}
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
        clojurify))
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
                              :entity-id :e2
                              :ends 7
                              :next-event 7}}
                   {:event-id :destroy-entity
                    :entity-id :e1
                    :bucket 4}
                   {:event-id :end-route
                    :entity-id :e1
                    :bucket 4}
                   {:bucket 4
                    :current-operation {:m :m1}
                    :entity-id :e1
                    :event-id :ends-production
                    :machine {:waiting-products [:e2 :e3]}}]
     :resources {:m1 {:waiting-products [:e3]
                      :starts 4
                      :entity-id :e2
                      :ends 7
                      :next-event 7}}
     :stats {:routes {:r1 {:throughputs [3]}}
             :entities-nb {:deltas {4 -1}
                           :start 4
                           :end 4}
             :entities-out {:deltas {4 1}
                            :start 4
                            :end 4}
             :entities {:throughputs [3]}
             :resources {:m1 {:occupation {:deltas {4 0}
                                           :start 4
                                           :end 4}
                              :nb-in-stock {:deltas {4 -1}
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
        clojurify))
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
