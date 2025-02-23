(ns auto-js.start
  (:require
   [auto-opti.distribution :as opt-distribution]
   [auto-opti.prng         :as opt-prng]
   #?@(:cljs [[goog.string.format] [goog.string :as gstring]])
   [clojure.string         :as str]))

;; ********************************************************************************
;; Data
;; ********************************************************************************

(def data
  {:max-nb-entity 2
   :routes {:blue [{:m :m4
                    :pt 1}
                   {:m :m2
                    :pt 3}
                   {:m :m1
                    :pt 2}]
            :purple [{:m :m4
                      :pt 1}
                     {:m :m3
                      :pt 3}
                     {:m :m1
                      :pt 1}]}
   :starting-bucket 0
   :waiting-time 0
   :resource-input {:m1 {}
                    :m2 {}
                    :m3 {}
                    :m4 {}}
   :seed #uuid "e85427c1-ed25-4ed4-9b11-52238d268265"})

;; ********************************************************************************
;; Events
;; ********************************************************************************

(def registry
  {:input-stock (fn [entities entity-id]
                  (let [entity (get entities entity-id)
                        {:keys [current-operation next-bucket]} entity
                        {:keys [m pt]} current-operation
                        new-bucket (+ next-bucket pt)]
                    (-> entities
                        (update entity-id assoc :step :prod-started :next-bucket new-bucket)
                        (update-vals (fn [other-entity]
                                       (cond-> other-entity
                                         (and (= m (get-in other-entity [:current-operation :m]))
                                              (contains? #{:input-stock :prod-start}
                                                         (:step other-entity)))
                                         (assoc :next-bucket new-bucket)))))))
   :prod-started (fn [entities entity-id]
                   (let [entity (get entities entity-id)
                         {:keys [next-ops]} entity
                         rnext-ops (rest next-ops)]
                     (if-let [current-operation (first next-ops)]
                       (update entities
                               entity-id
                               assoc
                               :step :input-stock
                               :next-ops rnext-ops
                               :current-operation current-operation)
                       (update entities
                               entity-id
                               assoc
                               :step :entity-dead
                               :next-ops rnext-ops
                               :current-operation nil))))
   :entity-dead (fn [entities _entity-id] entities)})

(defn next-entity-buckets
  "Returns the buckets where entities where an event occur"
  [entities]
  (some->> entities
           vals
           (keep (fn [entity] (when-not (= :entity-dead (:step entity)) (:next-bucket entity))))
           vec))

(defn- get-earliest-entity-event
  "Returns the first entity that will trigger an event"
  [entities required-bucket]
  (loop [rentities entities]
    (let [[entity-id {:keys [step next-bucket]}] (first rentities)]
      (if (and (= next-bucket required-bucket) (not= step :entity-dead))
        ((get registry step) entities entity-id)
        (recur (rest entities))))))

;; ********************************************************************************
;; Route
;; ********************************************************************************

(defn machines
  [{:keys [routes]
    :as _model-data}]
  (->> routes
       vals
       (apply concat)
       (map :m)
       distinct
       sort))

(defn route-ids
  [{:keys [routes]
    :as _model-data}]
  (-> routes
      keys
      vec))

;; ********************************************************************************
;; Run
;; ********************************************************************************

(defn run
  "Run the jobshop model described in `model-data` until iteration `iteration-stop`"
  [model-data iteration-stop]
  (let [model-data (dissoc model-data :status)
        {:keys [max-nb-entity seed routes waiting-time starting-bucket]} model-data
        route-ids (route-ids model-data)
        prng (opt-prng/xoroshiro128 seed)
        route-distribution (opt-distribution/distribution {:prng prng
                                                           :params {:a 0
                                                                    :b (count route-ids)}
                                                           :dst-name :uniform-int})]
    (loop [nb-entity 0
           bucket starting-bucket
           next-creation starting-bucket
           it 0
           entities {}]
      (let [next-buckets (cond-> (next-entity-buckets entities)
                           ;;NOTE Consider next-creation only if some entities are to be created
                           next-creation (conj next-creation))
            updated-model (fn [status b]
                            (merge model-data
                                   {:nb-entity nb-entity
                                    :bucket b
                                    :next-creation next-creation
                                    :iteration it
                                    :entities entities
                                    :status status}))]
        (cond
          (empty? next-buckets)
          ;;NOTE No new entity to create, nor entity to update
          (updated-model :no-workload bucket)
          ;;NOTE There are too many iterations, we stop
          (>= it iteration-stop) (updated-model :stop-at bucket)
          :else
          ;;NOTE Iteration will advance
          (let [bucket (apply min next-buckets)
                it (inc it)]
            (cond
              ;;NOTE A new entity is created
              (= bucket next-creation)
              (let [entity-id (str "p" nb-entity)
                    route-id (nth route-ids (opt-distribution/draw route-distribution))
                    route (get routes route-id)
                    nb-entity (inc nb-entity)
                    new-entity {:route-id route-id
                                :next-bucket bucket
                                :step :input-stock
                                :current-operation (first route)
                                :next-ops (rest route)}
                    entities (assoc entities entity-id new-entity)]
                (if (< nb-entity max-nb-entity)
                  ;;NOTE Prepare next entity creation
                  (recur nb-entity bucket (+ waiting-time next-creation) it entities)
                  ;;NOTE Create the last one, `nil` next-creation deactivate the feature
                  (recur nb-entity bucket nil it entities)))
              :else
              ;;NOTE An entity is updated
              (recur nb-entity
                     bucket
                     next-creation
                     it
                     (get-earliest-entity-event entities bucket)))))))))

;; ********************************************************************************
;; Printers
;; ********************************************************************************

(defn- f
  "To format strings across clojure(script)"
  [s & args]
  #?(:clj (apply format s args)
     :cljs (apply gstring/format s args)))

(defn print-workshop
  [model]
  (let [{:keys [entities bucket iteration]} model]
    (println (apply str (repeat 80 "*")))
    (println "bucket=" bucket ", iteration=" iteration)
    (println "      | Input      | Process   |")
    (doseq [machine (machines model)]
      (print (f "%4s" (name machine)) " | ")
      (let [{:keys [input-stock prod-started]}
            (->> entities
                 (map (fn [[entity-id entity]] (assoc entity :entity-id entity-id)))
                 (filter (fn [entity] (= machine (:m (:current-operation entity)))))
                 (group-by :step))]
        (print (f "%10s" (str/join ", " (mapv #(f "%4s" (:entity-id %)) input-stock))) "|")
        (print (f "%10s" (str/join ", " (mapv #(f "%4s" (:entity-id %)) prod-started))) "|"))
      (println))
    (let [other-entities (->> entities
                              (map (fn [[entity-id entity]] (assoc entity :entity-id entity-id)))
                              (filter (fn [entity] (nil? (:m (:current-operation entity)))))
                              (group-by :step))]
      (doseq [[entity-step entities] other-entities]
        (println (count entities) "entities in" (name entity-step))))))

(-> (run data 3)
    print-workshop)

(loop [it (range 0 400)]
  (let [model (run data (first it))]
    (cond
      (= (:status model) :no-workload) (println "no more workload")
      (seq it)
      (do (print-workshop model)
          (let [rit (rest it)]
            (if (seq rit) (recur (rest it)) (println "stopped before the end of execution")))))))

(run data 0)
(run data 1)
(run data 2)
(run data 3)
(run data 14)
(run data 15)
(run data 200)
