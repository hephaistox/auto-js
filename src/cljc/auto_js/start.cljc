(ns auto-js.start
  (:require
   [auto-opti.distribution :as opt-distribution]
   [auto-opti.prng         :as opt-prng]
   #?@(:cljs [[goog.string.format] [goog.string :as gstring]])
   [clojure.string         :as str]))

;; ********************************************************************************
;; Helpers
;; ********************************************************************************

(defn occupation-rate [val all] (if (and (number? val) (pos? all)) (* 100.0 (/ val all)) 0.0))

(defn- f
  "To format strings across clojure(script)"
  [s & args]
  #?(:clj (apply format s args)
     :cljs (apply gstring/format s args)))

(defn pf [s & args] (print (apply f s args)))

(defn pfln [s & args] (println (apply f s args)))

(defn add-distribution
  [prng dstb]
  (let [{:keys [location scale dstb-name]} dstb]
    (assoc dstb
           :dstb
           (opt-distribution/distribution {:prng prng
                                           :params {:scale scale
                                                    :location location}
                                           :dstb-name dstb-name}))))

;; ********************************************************************************
;; Route
;; ********************************************************************************

(defn machines
  [{:keys [routes]
    :as _model}]
  (->> routes
       vals
       (apply concat)
       (map :m)
       distinct
       sort))

(defn route-ids
  [{:keys [routes]
    :as _model}]
  (-> routes
      keys
      vec))

;; ********************************************************************************
;; Run
;; ********************************************************************************

(defn resolve-current-operation
  "Turn the operation in actual values and removes decisions to take"
  [current-operation]
  (when current-operation
    (assoc current-operation
           :pt
           (Math/round (opt-distribution/draw (get-in current-operation [:pt :dstb]))))))

(defn create-new-entity
  "Creates an entity if it is `next-creation` and the number created is matching max-nb-entity limit."
  [model route-ids route-distribution]
  (let [{:keys [nb-entity next-creation bucket routes max-nb-entity waiting-time]} model
        entity-id (str "p" nb-entity)
        route-id (nth route-ids (opt-distribution/draw route-distribution))
        route (get routes route-id)
        waiting-time (Math/round (opt-distribution/draw (:dstb waiting-time)))
        current-operation (resolve-current-operation (first route))]
    (when (= bucket next-creation)
      ;;NOTE Creates an entity if it is `next-creation` and the number created is matching max-nb-entity limit.
      (if (< (inc nb-entity) max-nb-entity)
        (-> model
            (assoc :next-creation (+ waiting-time next-creation))
            (update :nb-entity inc)
            (update :it inc)
            (update :entities
                    assoc
                    entity-id
                    {:route-id route-id
                     :step :input-stock
                     :machine-arrival bucket
                     :stock-entry-b bucket
                     :current-operation current-operation
                     :next-ops (rest route)})
            (update :past-events
                    conj
                    {:id :create-new-entity
                     :bucket bucket
                     :entity-id entity-id
                     :waiting-time waiting-time
                     :nb-entity nb-entity}))
        ;;NOTE Create the last one, `nil` next-creation deactivate the feature
        (-> model
            (dissoc :next-creation)
            (update :nb-entity inc)
            (update :it inc)
            (update :entities
                    assoc
                    entity-id
                    {:route-id route-id
                     :step :input-stock
                     :machine-arrival bucket
                     :stock-entry-b bucket
                     :current-operation current-operation
                     :next-ops (rest route)})
            (update :past-events
                    conj
                    {:id :create-last-entity
                     :entity-id entity-id
                     :bucket bucket
                     :nb-entity nb-entity}))))))

(defn print-new-entity
  [{:keys [id waiting-time]} entity-id-s]
  (case id
    :create-new-entity (pfln "Creates product %s, next in `%d` bucket" entity-id-s waiting-time)
    :create-last-entity (pfln "Creates product %s, it was the last product to create" entity-id-s)
    :continue))

(defn on-prod
  "Starts production if the entity is in stock and that machine is not processing already"
  [model]
  (let [{:keys [entities bucket]} model]
    (loop [[[entity-id entity] & rentities] entities]
      (let [{:keys [current-operation step stock-entry-b]} entity
            {:keys [m pt]} current-operation]
        (if (and (= step :input-stock) (not (get-in model [:resources m :machine-start])))
          ;;NOTE Production starts
          (-> model
              (update-in [:resources m]
                         #(-> %
                              (assoc :machine-start bucket)
                              (assoc :machine-end (+ bucket pt))
                              (update :stock-occupation (fnil + 0) (- bucket stock-entry-b))))
              (update :it inc)
              (dissoc :stock-entry-b)
              (update-in [:entities entity-id]
                         assoc
                         :step :prod-started
                         :prod-end (+ bucket pt)
                         :prod-start bucket)
              (update :past-events
                      conj
                      {:id :on-prod
                       :bucket bucket
                       :current-operation current-operation
                       :entity-id entity-id}))
          (when (seq rentities) (recur rentities)))))))

(defn print-on-prod
  [{:keys [id]} entity-id-s current-operation-s]
  (if (= id :on-prod) (pfln "Produces %s on %s" entity-id-s current-operation-s) :continue))

(defn on-output
  [model]
  (let [{:keys [entities stats bucket]} model]
    (loop [[[entity-id entity] & rentities] entities]
      (let [{:keys [next-ops current-operation prod-start prod-end route-id step machine-arrival]}
            entity]
        (if (and (= step :prod-started) (= bucket prod-end))
          (let [{:keys [m]} current-operation
                rnext-ops (rest next-ops)
                throughput (- bucket machine-arrival)
                processing-duration (- bucket prod-start)
                next-op (resolve-current-operation (first next-ops))]
            (if next-op
              ;;NOTE Schedule production and next operation
              (-> model
                  (update-in [:resources m]
                             #(-> %
                                  (dissoc :machine-start)
                                  (dissoc :machine-end)
                                  (update :cumulated-time
                                          (fnil (partial + (- prod-end prod-start)) 0))))
                  (update-in [:entities entity-id]
                             assoc
                             :step :input-stock
                             :stock-entry-b bucket
                             :next-ops rnext-ops
                             :current-operation next-op)
                  (update :it inc)
                  (update :past-events
                          conj
                          {:id :on-output
                           :bucket bucket
                           :current-operation current-operation
                           :next-op next-op
                           :entity-id entity-id}))
              ;;NOTE This was the last operation, cleaning of that entity
              (-> model
                  (update-in [:resources m]
                             #(-> %
                                  (dissoc :machine-start)
                                  (dissoc :machine-end)
                                  (update :cumulated-time
                                          (fnil (partial + (- prod-end prod-start)) 0))))
                  (update :it inc)
                  (assoc :stats
                         (update stats
                                 route-id
                                 conj
                                 {:processing-duration processing-duration
                                  :throughput throughput}))
                  (update :past-events
                          conj
                          {:id :on-output-last-op
                           :current-operation current-operation
                           :bucket bucket
                           :next-op nil
                           :entity-id entity-id})
                  (update :entities dissoc entities entity-id))))
          (when (seq rentities) (recur rentities)))))))

(defn print-on-output
  [{:keys [id]} entity-id-s current-operation-s]
  (case id
    :on-output (pfln "End of production %s on %s" entity-id-s current-operation-s)
    :on-output-last-op
    (pfln "End of production %s on %s, was the last operation" entity-id-s current-operation-s)
    :continue))

(defn print-next-bucket
  [entity]
  (if (= :next-bucket (:id entity)) (pfln "Move to bucket %3d" (:next-bucket entity)) :continue))

(defn find-next-bucket
  [model]
  (apply min
         (:next-creation model)
         (->> model
              :resources
              vals
              (keep :machine-end))))

(defn run
  "Run the jobshop model described in `model` until it `it-stop`"
  [model it-stop]
  (let [{:keys [seed starting-bucket max-nb-entity]} model ;; Constant data destructured once
        route-ids (route-ids model)
        prng (opt-prng/xoroshiro128 seed)
        route-distribution (opt-distribution/distribution {:prng prng
                                                           :params {:a 0
                                                                    :b (count route-ids)}
                                                           :dstb-name :uniform-int})]
    (loop [model (-> model
                     (dissoc :status)
                     (update :routes
                             update-vals
                             (partial
                              mapv
                              #(update % :pt (partial add-distribution (opt-prng/xoroshiro128)))))
                     (update :waiting-time #(add-distribution prng %))
                     (assoc :nb-entity 0
                            :it 0
                            :next-creation starting-bucket
                            :resources {}
                            :entities {}
                            :stats {}
                            :bucket starting-bucket))]
      (let [{:keys [bucket it nb-entity entities]} model
            next-bucket (find-next-bucket model)]
        (cond
          ;;NOTE There are too many its, we stop
          (>= it it-stop) (assoc model :status :stop-at :bucket bucket)
          (and (>= nb-entity max-nb-entity) (empty? entities))
          (assoc model :status :no-workload :bucket bucket)
          :else (recur (or (create-new-entity model route-ids route-distribution)
                           (on-prod model)
                           (on-output model)
                           ;;NOTE When no activity is to be done, go to next iteration
                           (-> model
                               (update :it inc)
                               (update :past-events
                                       conj
                                       {:id :next-bucket
                                        :bucket bucket
                                        :next-bucket (inc bucket)})
                               (assoc :bucket next-bucket)))))))))

;; ********************************************************************************
;; Printers
;; ********************************************************************************

(defn occupation-s
  [machine bucket]
  (let [{:keys [cumulated-time machine-start]
         :or {cumulated-time 0}}
        machine
        updated-cumulated
        (if machine-start (+ cumulated-time (- bucket machine-start)) cumulated-time)]
    (f "%3.2f%%" (occupation-rate updated-cumulated bucket))))

(defn wip-machine-s
  [machine]
  (let [{:keys [machine-start machine-end]} machine]
    (if machine-start (f "(%3s -> %3s)" (or machine-start "") (or machine-end "")) "( ________ )")))

(defn stock-occupation
  [machine bucket]
  (f "%3.2f%%" (occupation-rate (:stock-occupation machine) bucket)))

(defn other-entities
  [other-entities]
  (->> other-entities
       (map (fn [[entity-step entities]] (f "%s(%3d)" (name entity-step) (count entities))))
       (apply str "products ")))

(defn print-event
  [event]
  (when-let [{:keys [current-operation entity-id]} event]
    (let [current-operation-s (f "(%s, %3d)" (:m current-operation) (:pt current-operation))
          entity-id-s (f "%s" entity-id)]
      (and (print-new-entity event entity-id-s)
           (print-on-prod event entity-id-s current-operation-s)
           (print-on-output event entity-id-s current-operation-s)
           (print-next-bucket event)))))

(defn separator [] (println (apply str (repeat 80 "*"))))

(defn print-workshop
  [model]
  (let [{:keys [entities bucket it resources stats past-events]} model]
    (separator)
    (pfln "it=%3d, bucket= %3d, #product finished %-5d"
          it
          bucket
          (or (->> stats
                   (mapcat second)
                   count)
              0))
    (print-event (first past-events))
    (doseq [m (machines model)]
      (let [machine (get resources m)
            {:keys [input-stock prod-started]}
            (->> entities
                 (map (fn [[entity-id entity]] (assoc entity :entity-id entity-id)))
                 (filter (fn [entity] (= m (:m (:current-operation entity)))))
                 (group-by :step))]
        (if prod-started
          (pf "%4s %4s %12s (%7s) | "
              (name m)
              (str/join ", " (mapv #(f "%4s" (:entity-id %)) prod-started))
              (wip-machine-s machine)
              (occupation-s machine bucket))
          (pf "%4s                   (%7s) | " (name m) (occupation-s machine bucket)))
        (pf "%s" (stock-occupation machine bucket))
        (if input-stock
          (pfln "<<- %10s" (str/join ", " (mapv #(f "%4s" (:entity-id %)) input-stock)))
          (println))))))

(defn stats
  [model]
  (let [{:keys [bucket resources stats]} model]
    {:resources (-> resources
                    (update-vals (fn [{:keys [cumulated-time]
                                       :as resource}]
                                   (-> resource
                                       (assoc :occupation-rate
                                              (occupation-rate cumulated-time bucket))))))
     :stats (-> stats
                (update-vals frequencies))
     :bucket bucket}))

(defn run*
  [model it-stop]
  (-> (run model it-stop)
      (dissoc :routes
              :max-nb-entity
              :resource-input
              :seed
              :starting-bucket
              :waiting-time
              :nb-entity)))

;; ********************************************************************************
;; Tests
;; ********************************************************************************

(def data
  {:max-nb-entity 100
   ;; Add route proba
   :routes {:blue [{:m :m4
                    :pt {:dstb-name :normal
                         :location 20
                         :scale 0.2}}
                   {:m :m2
                    :pt {:dstb-name :normal
                         :location 30
                         :scale 0.2}}
                   {:m :m1
                    :pt {:dstb-name :normal
                         :location 20
                         :scale 0.2}}]
            :purple [{:m :m4
                      :pt {:dstb-name :normal
                           :location 20
                           :scale 0.2}}
                     {:m :m3
                      :pt {:dstb-name :normal
                           :location 30
                           :scale 0.2}}
                     {:m :m1
                      :pt {:dstb-name :normal
                           :location 10
                           :scale 0.2}}]}
   :starting-bucket 0
   :waiting-time {:location 10
                  :dstb-name :normal
                  :scale 0.2}
   :resource-input {:m1 {}
                    :m2 {}
                    :m3 {}
                    :m4 {}}
   :seed #uuid "e85427c1-ed25-4ed4-9b11-52238d268265"})

(comment
  (-> (run data 2)
      print-workshop)
  (time (loop [it (range 0 30)]
          (let [model (run data (first it))]
            (cond
              (= (:status model) :no-workload)
              (do (print-workshop model) (println "no more workload") (println ""))
              (seq it) (do (print-workshop model)
                           (let [rit (rest it)]
                             (if (seq rit)
                               (recur (rest it))
                               (println "stopped before the end of execution"))))))))
  (run* data 2)
  (run* data 1996)
  (run* data 100)
  (run* data 7)
  (run* data 11)
  (-> (run data 11)
      on-prod)
  (run* data 13)
  (run* data 15)
  (time (-> (run data 400000)
            stats))
  ;;
)
