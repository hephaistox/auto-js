(ns auto-js.sim.activity
  "Simulate a jobshop with a simulation by activity."
  (:require
   [auto-core.data.map          :as core-map]
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

  Can be used after a previous run has been stopped."
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
      (update :resources #(core-map/add-ids % :resource-id))
      (update :entity-sources #(core-map/add-ids % :entity-source-id))))

(defn errors
  "Check model consistency"
  [{:keys [entity-sources]
    :as model}]
  (cond-> model
    (empty? entity-sources) (update :errors conj {:error-id :no-source-id})))

;; ********************************************************************************
;; **
;; ********************************************************************************

(defn add-iteration
  "Adds an iteration event"
  [model bucket]
  (let [{:keys [it]} model
        it (inc it)]
    (-> model
        (assoc :bucket bucket :it it)
        (update :past-events
                conj
                {:it it
                 :event-id :iteration
                 :bucket bucket}))))

(defn add-snapshot
  "Adds an event with a resource snapshot"
  [model]
  (let [{:keys [bucket it]} model]
    (-> model
        (update :past-events
                conj
                {:bucket bucket
                 :snapshot (:resources model)
                 :it it
                 :event-id :snapshot}))))

;; ********************************************************************************
;; ** Entitites
;; ********************************************************************************

(defn get-entity
  "Returns a pair of
  * `entity` found ( `nil` if not)
  * `model` updated with an error if not

  :errors `unkown-entity-id`
  :past-events none
  :stats none"
  [model entity-id during-kw]
  (if-let [entity (get-in model [:entities entity-id])]
    [entity model]
    [nil
     (-> model
         (update :errors
                 conj
                 {:error-id :unkown-entity-id
                  :during during-kw
                  :entity-id entity-id
                  :known-ids (vec (keys (:entities model)))}))]))

(defn create-new-entity
  "Creates a new entity from `entity-source-id`

  A source should have
   * `create-fn` optional a function
   * `nb-entity`
   * `next-event`
   * `nb-max`
   * `waiting-time`

  :errors `new-entity-exception`, `create-unkown-entity-id`
  :past-events `create-new-entity`
  :stats `entities-in` how many new entities have been created
          and `entities-nb` how many are in the system"
  [model entity-source-id]
  (if-let [entity-source (get-in model [:entity-sources entity-source-id])]
    (try
      (let [{:keys [create-fn nb-entity next-event nb-max waiting-time]} entity-source
            {:keys [bucket]} model
            new-entity (if (fn? create-fn)
                         (create-fn model entity-source)
                         {:entity-id (str (name entity-source-id) "-" (inc nb-entity))
                          :starts bucket})
            new-entity-id (:entity-id new-entity)
            new-model (-> model
                          (assoc-in [:entities new-entity-id] new-entity)
                          (update-in [:stats :entities-in] (opt-tb/incnil bucket :level))
                          (update-in [:stats :entities-nb] (opt-tb/incnil bucket :level)))
            event {:bucket bucket
                   :event-id :create-new-entity
                   :entity-id new-entity-id
                   :entity-source-id entity-source-id
                   :nb-max nb-max
                   :nb-entity (inc nb-entity)}]
        (if (and (some? nb-max) (< (inc nb-entity) nb-max))
          (let [actual-waiting-time (opt-dstb/resolve waiting-time)]
            {:model (-> new-model
                        (assoc-in [:entity-sources entity-source-id]
                                  (-> entity-source
                                      (update :nb-entity inc)
                                      (assoc :next-event (+ actual-waiting-time next-event))))
                        (update :past-events conj (assoc event :waiting-time actual-waiting-time)))
             :entity new-entity})
          (if (< nb-entity nb-max)
            {:model (-> new-model
                        (update :entity-sources dissoc entity-source-id)
                        (update :past-events conj (assoc event :last? true)))
             :entity new-entity}
            {:model new-model})))
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
  "Destroys an entity from `entity-source-id`

  :errors : none
  :stats : `entities throughputs` store the list of duration of the entity in the system
           `entities-out` and `entities-nb`
  :past-events `destroy-entity`"
  [model entity]
  (let [{:keys [bucket]} model
        {:keys [entity-id starts]} entity
        [entity model] (get-entity model entity-id :destroy-entity)]
    (if entity
      (-> model
          (update-in [:stats :entities-out] (opt-tb/incnil bucket :level))
          (update-in [:stats :entities-nb] (opt-tb/decnil bucket :level))
          (update-in [:stats :entities :throughputs] conj (- bucket starts))
          (update :past-events
                  conj
                  {:event-id :destroy-entity
                   :entity-id entity-id
                   :bucket bucket})
          (update :entities dissoc entity-id))
      model)))

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
  "The `entity` ends its route, the entity is destroyed

  :errors : `route-ends-was-not-started`
  :stats : `routes route-id throughputs`
  :past-events `end-route`"
  [model entity]
  (let [{:keys [starts route-id entity-id]} entity
        {:keys [bucket]} model]
    (if (number? starts)
      (-> model
          (update-in [:stats :routes route-id :throughputs] conj (- bucket starts))
          (update :past-events
                  conj
                  {:event-id :end-route
                   :entity-id entity-id
                   :bucket bucket})
          (destroy-entity entity))
      (-> model
          (update :errors
                  conj
                  {:error-id :route-ends-was-not-started
                   :entity entity})))))

(defn next-op
  "`Entity` is moving to its next operation, if there is no next operation, the route is ended.

  Next operation is resolved and set to `current-operation`.

  :stats : When route is ended: `:routes route-id :throughputs`
  :errors : `route-ends-was-not-started`
  :past-events : `:next-op` and `end-route`"
  [model entity]
  (let [{:keys [next-ops entity-id]} entity
        {:keys [bucket]} model
        [current-operation & next-ops] next-ops
        current-operation (cond-> current-operation
                            (:pt current-operation) (update :pt opt-dstb/resolve))
        new-entity (assoc entity :current-operation current-operation :next-ops next-ops)]
    (if current-operation
      (-> model
          (assoc-in [:entities entity-id] new-entity)
          (update :past-events
                  conj
                  {:event-id :next-op
                   :entity-id entity-id
                   :current-operation current-operation
                   :bucket bucket}))
      (end-route model new-entity))))

(defn start-route
  "The `entity` starts the route `route-id`, that should be found in route.

  :errors : `:route-not-existing` and `route-is-empty` and when route is ended: `:routes route-id :throughputs`
  `route-ends-was-not-started`
  :past-events : `:start-route`
  :stats : `:routes route-id :route-nb`"
  [model entity route-id]
  (let [entity-id (:entity-id entity)
        {:keys [bucket]} model]
    (if-let [route (get-in model [:routes route-id])]
      (let [operations (:operations route)]
        (if (seq operations)
          (let [new-entity (-> entity
                               (assoc :route-id route-id :next-ops operations))]
            (-> model
                (assoc-in [:entities entity-id] new-entity)
                (update-in [:stats :routes route-id :route-nb] (opt-tb/incnil bucket :level))
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

  :errors : `current-operation-missing`, `machine-is-busy-already`
  :past-events :  `enter-production`
  :stats `:resources m :occupation`

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
      :else (let [new-machine (-> machine
                                  (assoc :starts bucket
                                         :ends (+ pt bucket)
                                         :next-event (+ pt bucket)
                                         :entity-id entity-id))]
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
  "Product enters the input-stock and directly the production slot if available, or waits in stock instead.

  :errors `no-current-operation`
  :past-events `enter-input-stock-wait-in-stock`
  :stats `:resources m :nb-in-stock`"
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
  "Production of `entity` is ended, if exsits one `waiting-product` is picked and its production started.

  :errors : `:no-current-operation`
  :past-events : `:ends-production`
  :stats `:resources m :nb-in-stock`, `:resources m :occupation`"
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
            new-machine (-> (get-in model [:resources m])
                            (dissoc :starts :ends :next-event :entity-id))
            waiting-products (get-in model [:resources m :waiting-products])
            new-model
            (-> model
                (assoc-in [:resources m] new-machine)
                (update-in [:entities entity-id] assoc :step :move-to-next-machine)
                (update :past-events
                        conj
                        {:bucket bucket
                         :entity-id entity-id
                         :event-id :ends-production
                         :current-operation current-operation
                         :machine new-machine})
                (next-op entity)
                (update-in [:stats :resources m :nb-in-stock] (opt-tb/decnil bucket :level))
                (update-in [:stats :resources m :occupation] (opt-tb/decnil bucket :level)))]
        (if (seq waiting-products)
          (let [[entity-to-start & rwaiting-products] waiting-products]
            (-> new-model
                (assoc-in [:resources m :waiting-products] rwaiting-products)
                (enter-production (get-in new-model [:entities entity-to-start]))))
          new-model)))))

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
;; ** Jobshop specific
;; ********************************************************************************

(defn on-new-product
  "For entity-source that should appear now (`next-event=bucket`), the product is created, the route is started and its first operation executed

  * :errors `:next-event-missed` if an event should have been done in the past
            `:source-empty` if the source exists but has never something to create
  * :past-events: none
  * :stats: none"
  [model]
  (let [{:keys [entity-sources bucket]} model]
    (some->> entity-sources
             vals
             (filter #(>= bucket (:next-event %)))
             seq
             (reduce (fn [model entity-source]
                       (let [{:keys [entity-source-id next-event]} entity-source]
                         (cond
                           (> bucket next-event) (update model
                                                         :errors
                                                         conj
                                                         {:error-id :next-event-missed
                                                          :entity-source-id entity-source-id})
                           (= bucket next-event)
                           (let [{:keys [model entity]} (create-new-entity model entity-source-id)]
                             (if entity
                               (let [entity-id (:entity-id entity)
                                     route-id (opt-routings/pick-route-id model)
                                     model (start-route model entity route-id)]
                                 (enter-input-stock model (get-in model [:entities entity-id])))
                               (update model
                                       :errors
                                       conj
                                       {:error-id :source-empty
                                        :entity-source-id entity-source-id})))
                           :else model)))
                     model))))

(defn on-production-end
  "For all resources with a `next-event` at current `bucket`, ends the production of entities occupying the machine."
  [model]
  (let [{:keys [bucket]} model]
    (some->> model
             :resources
             vals
             (filter #(= bucket (:next-event %)))
             seq
             (reduce (fn [model resource]
                       (let [{:keys [entity-id]} resource]
                         (if-let [entity (get-in model [:entities entity-id])]
                           (let [model (ends-production model entity)
                                 entity (get-in model [:entities entity-id])]
                             (enter-input-stock model entity))
                           (-> model
                               (update :errors
                                       conj
                                       {:error-id :entity-not-found
                                        :entity-id entity-id})))))
                     model))))

(defn- defaulting [s d] (if (empty? s) d s))

(defn print-event
  "Print the `event` if it is matching one of this events."
  [event]
  (when-let [{:keys [current-operation
                     entity-id
                     event-id
                     waiting-time
                     entity-source-id
                     last?
                     snapshot
                     bucket
                     route-id
                     machine
                     it
                     nb-entity
                     nb-max]}
             event]
    (let [{:keys [m pt]} current-operation
          {:keys [waiting-products]} machine
          entity-id-s (f "%s" entity-id)]
      (case event-id
        :create-new-entity (if last?
                             (pfln "%03d `%s` ->> `%s,` next in `%03d` bucket (%d/%d) - last"
                                   bucket
                                   (name entity-source-id)
                                   entity-id-s
                                   waiting-time
                                   nb-entity
                                   nb-max)
                             (pfln "%03d `%s` ->> `%s,` next in `%03d` bucket (%d/%d)"
                                   bucket
                                   (name entity-source-id)
                                   entity-id-s
                                   waiting-time
                                   nb-entity
                                   nb-max))
        :destroy-entity (pfln "Entity `%s` is destroyed" entity-id-s)
        :start-route (pfln " \\_ route `%s`" (name route-id))
        :enter-production (pfln "`%s` starts on `%s`, during %d" entity-id-s m pt)
        :iteration nil
        :snapshot (do (pfln "snapshot, iteration `%d`:" it)
                      (run! (fn [{:keys [resource-id entity-id waiting-products]}]
                              (pfln "%2s (%3s) <- %s"
                                    resource-id
                                    (or entity-id "_")
                                    (defaulting (str/join "," waiting-products) "_")))
                            (vals snapshot)))
        :next-op (pfln "`%s` next operation to `%s`" entity-id-s (name m))
        :enter-input-stock-wait-in-stock
        (pfln "`%03d` `%s` waits in `%s` in stock: %s"
              bucket
              entity-id-s
              m
              (if (seq waiting-products) (str/join ", " waiting-products) "_"))
        :ends-production (pfln "`%03d` `%s` ends on machine `%s`" bucket entity-id-s m)
        (println "Event:" (pr-str event))))))

(defn print-error
  [{:keys [error-id _bucket _entity entity-source-id]
    :as error}]
  (print col-text/font-red)
  (case error-id
    :source-empty (pfln "`%s` has created nothing to create." (name entity-source-id))
    (println "Error:" (pr-str error)))
  (print col-text/style-reset-all))

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
