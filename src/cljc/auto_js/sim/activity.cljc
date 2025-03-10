(ns auto-js.sim.activity
  "Simulate a jobshop with a simulation by activity."
  (:require
   [auto-core.os.colorized-text :as col-text]
   [auto-core.string.format     :refer [f pfln]]
   [auto-opti.dstb              :as opt-dstb]
   [auto-opti.routings          :as opt-routings]
   [auto-opti.tb                :as opt-tb]
   [clojure.string              :as str]))

;; ********************************************************************************
;; ** Start the model
;; ********************************************************************************

(defn start
  "Starts the simulation.

  This initialization so you can `start` it again after a previous run has been stopped.

  * `routings` are initialized
  * `entity-sources` are initialized with their default value and probability distribution.
  * default values are assigned to `bucket`, `resources`.
  * `entities` are reinitialized
  * `it` is set to `0`
  "
  [{:keys [resources entity-sources bucket]
    :or {bucket 0}
    :as model}
   prng]
  (-> model
      (auto-opti.routings/start prng)
      (dissoc :sim-status)
      (assoc :it 0
             :entity-sources (update-vals entity-sources
                                          #(-> %
                                               (assoc :next-event bucket :nb-entity 0)
                                               (update :waiting-time (partial opt-dstb/dstb prng))
                                               (update :nb-max (fn [v] (or v 1)))))
             :resources (or resources {})
             :past-events []
             :errors []
             :entities {}
             :bucket bucket)
      (update :entity-sources
              #(into {}
                     (map (fn [[entity-id entity]] [entity-id
                                                    (assoc entity :entity-source-id entity-id)])
                          %)))))

(defn errors
  "Check model consistancy"
  [{:keys [entity-sources]
    :as model}]
  (if (empty? entity-sources)
    (-> model
        (update :errors conj {:error-id :no-source-id}))
    model))

;; ********************************************************************************
;; ** Entitites
;; ********************************************************************************

(defn get-entity
  "Returns a pair of
  * `entity` found ( `nil` if not)
  * `model` updated with an error if not"
  [model entity-id]
  (if-let [entity (get-in model [:entities entity-id])]
    [entity model]
    [nil
     (-> model
         (update :errors
                 conj
                 {:error-id :unkown-entity-id
                  :entity-id entity-id
                  :known-ids (vec (keys (:entities model)))}))]))

(defn create-new-entity
  "Creates a new entity from `entity-source-id`"
  [model entity-source-id]
  (if-let [entity-source (get-in model [:entity-sources entity-source-id])]
    (try
      (let [{:keys [create-fn nb-entity next-event nb-max waiting-time]} entity-source
            {:keys [bucket]} model
            new-entity (if (fn? create-fn)
                         (create-fn model entity-source)
                         {:entity-id (str (name entity-source-id) "-" nb-entity)})
            new-entity-id (:entity-id new-entity)
            model (assoc-in model [:entities new-entity-id] new-entity)
            event {:bucket bucket
                   :event-id :create-new-entity
                   :entity-id new-entity-id
                   :entity-source-id entity-source-id
                   :nb-max nb-max
                   :nb-entity nb-entity}]
        (if (and (some? nb-max) (< (inc nb-entity) nb-max))
          (let [actual-waiting-time (opt-dstb/resolve waiting-time)]
            {:model (-> model
                        (assoc-in [:entity-sources entity-source-id]
                                  (-> entity-source
                                      (update :nb-entity inc)
                                      (assoc :next-event (+ actual-waiting-time next-event))))
                        (update :past-events conj (assoc event :waiting-time actual-waiting-time)))
             :entity new-entity})
          {:model (-> model
                      (update :entity-sources dissoc entity-source-id)
                      (update :past-events conj (assoc event :last? true)))
           :entity new-entity}))
      (catch #?(:clj Exception
                :cljs :default)
        e
        {:model (update model
                        :errors
                        conj
                        {:error-id :new-entity-exception
                         :exception e
                         :entity-source-id entity-source-id})}))
    {:model (update model
                    :errors
                    conj
                    {:error-id :create-unkown-entity-id
                     :entity-source-id entity-source-id
                     :known-ids (vec (keys (:entity-sources model)))})}))

(defn destroy-entity
  "Destroys an entity from `entity-source-id`"
  [model entity]
  (let [{:keys [bucket]} model
        entity-id (:entity-id entity)]
    (if (get-in model [:entities entity-id])
      (-> model
          (update :entities dissoc entity-id))
      (-> model
          (update :past-events
                  conj
                  {:event-id :destroy-entity
                   :entity-id entity-id
                   :bucket bucket})
          (update :errors
                  conj
                  {:error-id :destroy-unkown-entity-id
                   :entity-id entity-id
                   :known-ids (vec (keys (:entities model)))})))))

(defn do-new-entity-from-source-id
  [model entity-source-id]
  (let [{:keys [bucket]} model
        {:keys [next-event]} (get-in model [:entity-sources entity-source-id])]
    (cond
      (> bucket next-event) (update model
                                    :errors
                                    conj
                                    {:error-id :next-event-missed
                                     :entity-source-id entity-source-id})
      (= bucket next-event) (:model (create-new-entity model entity-source-id))
      :else model)))

(defn on-new-entity
  "At date `next-event`, adds a new entity. A maximum of `nb-max` is created, if this value is `nil`, no limit is added."
  [model]
  (let [{:keys [entity-sources bucket]} model]
    (when (some? bucket) (reduce do-new-entity-from-source-id model (keys entity-sources)))))

(defn next-event-bucket
  "Bucket of the next entity to create, `nil` otherwise"
  [model]
  (some->> model
           :entity-sources
           vals
           (map :next-event)
           (apply min)))

;; ********************************************************************************
;; ** Route
;; Manage an entity that lifecycle is based on a route
;; ********************************************************************************

(defn end-route
  "The `entity` ends its route.

  Error is raised if
  * the entity was not started already

  Then
  * Updates stats with route throughputs
  * Counts the number of products out
  * Destroy the entity"
  [model entity]
  (let [{:keys [starts route-id]} entity
        {:keys [bucket]} model]
    (if (number? starts)
      (-> model
          (update-in [:stats :routes route-id :throughputs] conj (- bucket starts))
          (update-in [:stats :products-out] (opt-tb/incnil bucket :level))
          (destroy-entity entity))
      (-> model
          (update :errors
                  conj
                  {:error-id :route-ends-was-not-started
                   :entity entity})))))

(defn next-op
  "`Entity` is moving to its next operation.
  It is ended if the last operation is reached.

  * `entities` is updated route advancement
  * route is ended if no more current-operation are available."
  [model entity]
  (let [{:keys [next-ops entity-id]} entity
        [current-operation & next-ops] next-ops
        current-operation (cond-> current-operation
                            (:pt current-operation) (update :pt opt-dstb/resolve))
        new-entity (assoc entity :current-operation current-operation :next-ops next-ops)]
    (if current-operation
      (assoc-in model [:entities entity-id] new-entity)
      (end-route model new-entity))))

(defn start-route
  "The `entity` starts the route `route-id`.

  * Route is picked in the `routes`, an `errors` is raised if it is not existing
  * Entity is updated with that route-id
  * Stats creates a products-in"
  [model entity route-id]
  (let [entity-id (:entity-id entity)
        {:keys [bucket]} model]
    (if-let [route (get-in model [:routes route-id])]
      (let [operations (:operations route)]
        (if (seq operations)
          (let [new-entity (-> entity
                               (assoc :route-id route-id :starts bucket :next-ops operations))]
            (-> model
                (update-in [:stats :products-in] (opt-tb/incnil bucket :level))
                (assoc-in [:entities entity-id] new-entity)
                (update :past-events
                        conj
                        {:event-id :start-route
                         :bucket bucket
                         :entity-id entity-id
                         :route-id route-id})
                (next-op new-entity)))
          (-> model
              (update :errors
                      conj
                      {:error-id :route-is-empty
                       :route-id :route-id
                       :entity-id entity-id}))))
      (-> model
          (update :errors
                  conj
                  {:error-id :route-not-existing
                   :route-id route-id})))))

;; ********************************************************************************
;; ** Machine
;; ********************************************************************************

(defn enter-production
  "Product starts its production on machine.

  Errors are raised if
     * A current operation is not set
     * The machine doesn't have `starts` value already.

  Stats are updated to show machine occupation

  Otherwise machine is updated with `starts` `ends` and `next-event`.
  And `entities` is set to `in-production`."
  [model entity]
  (let [{:keys [bucket resources]} model
        {:keys [current-operation entity-id]} entity
        {:keys [m pt]} current-operation
        machine (get resources m)
        {:keys [starts]} machine]
    (cond
      (nil? current-operation) (-> model
                                   (update :errors
                                           conj
                                           {:error-id :current-operation-missing
                                            :entity entity}))
      starts (-> model
                 (update :errors
                         conj
                         {:error-id :machine-is-busy-already
                          :machine machine}))
      :else (let [new-machine
                  (-> machine
                      (assoc :starts bucket :ends (+ pt bucket) :next-event (+ pt bucket)))]
              (-> model
                  (assoc-in [:resources m] new-machine)
                  (update-in [:stats :resources m :occupation] (opt-tb/incnil bucket :level))
                  (update :past-events
                          conj
                          {:bucket bucket
                           :entity-id entity-id
                           :event-id :enter-production
                           :current-operation current-operation
                           :machine new-machine})
                  (update-in [:entities entity-id] assoc :step :in-production))))))

(defn enter-input-stock
  "Product enters the input-stock.

  Errors are raised if
     * A current operation is not set

  Enter the production if the machine production is available,
  Is waiting in stock otherwise"
  [model entity]
  (let [{:keys [bucket]} model
        {:keys [entity-id current-operation]} entity
        {:keys [m]} current-operation
        machine (get-in model [:resources m])
        {:keys [starts waiting-products]} machine]
    (cond
      (nil? current-operation) (-> model
                                   (update :errors
                                           conj
                                           {:error-id :no-current-operation
                                            :entity entity}))
      (nil? starts) (enter-production model entity)
      :else (let [new-machine (assoc machine :waiting-products (conj waiting-products entity-id))]
              (-> model
                  (update-in [:stats :resources m :nb-in-stock] (opt-tb/incnil bucket :level))
                  (update :past-events
                          conj
                          {:bucket bucket
                           :current-operation current-operation
                           :entity-id entity-id
                           :event-id :enter-input-stock-wait-in-stock
                           :machine new-machine})
                  (assoc-in [:resources m] new-machine)
                  (update-in [:entities entity-id] assoc :step :wait-in-input-stock))))))

(defn ends-production
  "Production of `entity` is ended.

  For `current-operation`,
  * its machine is updated in `resources`
  * `entities` step is ended.
  * `stats` are updated with `occupation` level decreased.

  If exists, one `waiting-product` is picked and its production started

  Errors if entity has no `current-operation`."
  [model entity]
  (let [{:keys [bucket]} model
        {:keys [current-operation entity-id]} entity]
    (if (nil? current-operation)
      (update model
              :errors
              conj
              {:error-id :no-current-operation
               :entity entity})
      (let [{:keys [m]} current-operation
            machine (-> model
                        (get-in [:resources m]))
            waiting-products (get-in model [:resources m :waiting-products])
            model (-> model
                      (assoc-in [:resources m]
                                (-> machine
                                    (dissoc :starts :ends :next-event)))
                      (update-in [:entities entity-id] assoc :step :move-to-next-machine)
                      (update :past-events
                              conj
                              {:bucket bucket
                               :entity-id entity-id
                               :event-id :ends-production
                               :current-operation current-operation
                               :machine machine})
                      (next-op entity)
                      (update-in [:stats :resources m :occupation] (opt-tb/decnil bucket :level)))]
        (if (seq waiting-products)
          (let [[entity-to-start & rwaiting-products] waiting-products]
            (-> model
                (assoc-in [:resources m :waiting-products] rwaiting-products)
                (enter-production (get-in model [:entities entity-to-start]))))
          model)))))

(defn resource-next-event
  [model]
  (some->> model
           :resources
           vals
           (map :next-event)
           (filter some?)
           seq
           (apply min)))

;; ********************************************************************************
;; ** jobshop specific
;; ********************************************************************************

(defn on-new-product
  "At date `next-event`, adds a new product starting a route. A maximum of `nb-max` is created, if this value is `nil`, no limit is added."
  [model]
  (let [{:keys [entity-sources bucket]} model]
    (reduce (fn [model entity-source]
              (let [{:keys [entity-source-id next-event]} entity-source]
                (cond
                  (> bucket next-event) (update model
                                                :errors
                                                conj
                                                {:error-id :next-event-missed
                                                 :entity-source-id entity-source-id})
                  (= bucket next-event)
                  (let [{:keys [model entity]} (create-new-entity model entity-source-id)
                        entity-id (:entity-id entity)
                        route-id (opt-routings/pick-route-id model)
                        model (start-route model entity route-id)]
                    (enter-input-stock model (get-in model [:entities entity-id])))
                  :else model)))
            model
            (vals entity-sources))))

(defn print-event
  "Print the `event` if it is matching one of this events."
  [event]
  (when-let [{:keys [current-operation
                     entity-id
                     event-id
                     waiting-time
                     entity-source-id
                     last?
                     bucket
                     route-id
                     machine
                     nb-entity
                     nb-max]}
             event]
    (let [{:keys [m pt]} current-operation
          {:keys [waiting-products]} machine
          entity-id-s (f "%s" entity-id)]
      (print (f "b= %03d - " bucket))
      (case event-id
        :create-new-entity
        (if last?
          (pfln "`%s` ->> `%s,` %d is last" (name entity-source-id) entity-id-s nb-max)
          (pfln "`%s` ->> `%s,` next in `%03d` bucket (%d/%d)"
                (name entity-source-id)
                entity-id-s
                waiting-time
                nb-entity
                nb-max))
        :destroy-entity (pfln "Entity `%s` is destroyed" entity-id-s)
        :start-route (pfln " \\_ route `%s`" (name route-id))
        :enter-production (pfln "`%s` starts production on `%s`, during %d" entity-id-s m pt)
        :enter-input-stock-wait-in-stock
        (pfln "`%s` waits in `%s` in stock: %s"
              entity-id-s
              m
              (if (seq waiting-products) (str/join ", " waiting-products) "_"))
        :ends-production (pfln "Product `%s` ends production on machine `%s` (input is %s)"
                               entity-id-s
                               m
                               (if (seq waiting-products) (str/join ", " waiting-products) "_"))
        (println (pr-str event))))))

(defn print-error
  [{:keys [error-id bucket entity]
    :as error}]
  (case error-id
    (println (pr-str error))))

(defn show-past
  [model]
  (run! print-event
        (-> model
            :past-events)))

(defn print-errors
  [{:keys [errors]
    :as _model}]
  (when (seq errors) (run! print-error errors) :error))

(defn print-status
  [{:keys [sim-status]
    :as _model}]
  (when sim-status
    (println "Finished with status: " col-text/font-red (name sim-status) col-text/style-reset-all)
    :error))

(defn print-output [model] (show-past model) (print-status model) (print-errors model))
