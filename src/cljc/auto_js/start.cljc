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
  {:input-stock (fn [{:keys [entities resources]} entity-id]
                  (let [entity (get entities entity-id)
                        {:keys [current-operation next-bucket]} entity
                        {:keys [m pt]} current-operation
                        new-bucket (+ next-bucket pt)]
                    {:resources (update resources m assoc :started next-bucket)
                     :entities
                     (-> entities
                         (update entity-id assoc :step :prod-started :next-bucket new-bucket)
                         (update-vals (fn [other-entity]
                                        (cond-> other-entity
                                          (and (= m (get-in other-entity [:current-operation :m]))
                                               (contains? #{:input-stock :prod-start}
                                                          (:step other-entity)))
                                          (assoc :next-bucket new-bucket)))))}))
   :prod-started (fn [{:keys [entities resources]} entity-id]
                   (let [entity (get entities entity-id)
                         {:keys [next-ops current-operation]} entity
                         {:keys [m pt]} current-operation
                         rnext-ops (rest next-ops)]
                     {:resources (let [cumulated-time (get-in resources [m :cumulated-time] 0)]
                                   (-> resources
                                       (update m dissoc :started)
                                       (update m assoc :cumulated-time (+ cumulated-time pt))))
                      :entities (if-let [current-operation (first next-ops)]
                                  (update entities
                                          entity-id
                                          assoc
                                          :step :input-stock
                                          :next-ops rnext-ops
                                          :current-operation current-operation)
                                  (update entities
                                          entity-id
                                          assoc
                                          :step :done
                                          :next-ops rnext-ops
                                          :current-operation nil))}))
   :done (fn [event-input _entity-id] event-input)})

(defn next-entity-buckets
  "Returns the buckets where entities where an event occur"
  [entities]
  (some->> entities
           vals
           (keep (fn [entity] (when-not (= :done (:step entity)) (:next-bucket entity))))
           vec))

(defn- get-earliest-entity-event
  "Returns the first entity that will trigger an event"
  [{:keys [entities]
    :as event-input}
   required-bucket]
  (loop [rentities entities]
    (let [[entity-id {:keys [step next-bucket]}] (first rentities)]
      (if (and (= next-bucket required-bucket) (not= step :done))
        ((get registry step) event-input entity-id)
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
           resources {}
           entities {}]
      (let [next-buckets (cond-> (next-entity-buckets entities)
                           ;;NOTE Consider next-creation only if some entities are to be created
                           next-creation (conj next-creation))
            updated-model (fn [status b]
                            (merge model-data
                                   {:nb-entity nb-entity
                                    :bucket b
                                    :resources resources
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
                  (recur nb-entity bucket (+ waiting-time next-creation) it resources entities)
                  ;;NOTE Create the last one, `nil` next-creation deactivate the feature
                  (recur nb-entity bucket nil it resources entities)))
              :else
              ;;NOTE An entity is updated
              (let [{:keys [resources entities]} (get-earliest-entity-event {:entities entities
                                                                             :resources resources}
                                                                            bucket)]
                (recur nb-entity bucket next-creation it resources entities)))))))))

;; ********************************************************************************
;; Printers
;; ********************************************************************************

(defn- f
  "To format strings across clojure(script)"
  [s & args]
  #?(:clj (apply format s args)
     :cljs (apply gstring/format s args)))

(defn occupation-rate [val all] (if (and (number? val) (pos? all)) (* 100.0 (/ val all)) 100.0))

(defn print-occupation
  [cumulated started bucket debug]
  (case debug
    :simple (when cumulated (print (f "  (%3.2f%%)" (occupation-rate cumulated bucket))))
    :medium (cond
              (and cumulated started)
              (print (f "  (%3s -> *, %3.2f%%)" started (occupation-rate cumulated bucket)))
              cumulated (print (f "  (    -> *, %3.2f%%)" (occupation-rate cumulated bucket)))
              started (print (f "  (%3s -> *)" started))
              :else (print))
    :detailed
    (cond
      (and cumulated started)
      (print (f "  (%3s -> %3s*, %3.2f%%)" started cumulated (occupation-rate cumulated bucket)))
      cumulated (print (f "  (    -> %3s , %3.2f%%)" cumulated (occupation-rate cumulated bucket)))
      started (print (f "  (%3s ->    *,    _ %%)" started))
      :else (print))))

(defn other-entities
  [other-entities]
  (->> other-entities
       (map (fn [[entity-step entities]] (f "%s(%3d)" (name entity-step) (count entities))))
       (apply str "products ")))

(defn print-workshop
  [model]
  (let [{:keys [entities bucket iteration resources]} model]
    ;    (println (apply str (repeat 80 "*")))
    (println "iteration=" iteration
             ", bucket=" bucket
             " " (->> entities
                      (map (fn [[entity-id entity]] (assoc entity :entity-id entity-id)))
                      (filter (fn [entity] (nil? (:m (:current-operation entity)))))
                      (group-by :step)
                      other-entities))
    (doseq [m (machines model)]
      (print (f "%4s" (name m)) " | ")
      (let [{:keys [input-stock prod-started]}
            (->> entities
                 (map (fn [[entity-id entity]] (assoc entity :entity-id entity-id)))
                 (filter (fn [entity] (= m (:m (:current-operation entity)))))
                 (group-by :step))]
        (print (f "%10s" (str/join ", " (mapv #(f "%4s" (:entity-id %)) input-stock))) "|")
        (print (f "%10s" (str/join ", " (mapv #(f "%4s" (:entity-id %)) prod-started))) "|"))
      (print-occupation (get-in resources [m :cumulated-time])
                        (get-in resources [m :started])
                        bucket
                        :simple)
      (println))))

;; ********************************************************************************
;; Tests
;; ********************************************************************************

(-> (run data 14)
    print-workshop)

(loop [it (range 0 400)]
  (let [model (run data (first it))]
    (cond
      (= (:status model) :no-workload) (do (print-workshop model) (println "no more workload"))
      (seq it)
      (do (print-workshop model)
          (let [rit (rest it)]
            (if (seq rit) (recur (rest it)) (println "stopped before the end of execution")))))))

(run data 0)
(run data 1)
(run data 2)
(run data 3)
(run data 4)
(run data 8)
(run data 3)
(run data 14)
(run data 15)
(run data 200)
