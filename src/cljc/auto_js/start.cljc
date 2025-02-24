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
  {:max-nb-entity 400
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
   :waiting-time 10
   :resource-input {:m1 {}
                    :m2 {}
                    :m3 {}
                    :m4 {}}
   :seed #uuid "e85427c1-ed25-4ed4-9b11-52238d268265"})

(defn occupation-rate [val all] (if (and (number? val) (pos? all)) (* 100.0 (/ val all)) 100.0))

;; ********************************************************************************
;; Events
;; ********************************************************************************

(def registry
  {:input-stock (fn [{:keys [entities resources stats]} entity-id]
                  (let [entity (get entities entity-id)
                        {:keys [current-operation next-bucket]} entity
                        {:keys [m pt]} current-operation
                        new-bucket (+ next-bucket pt)]
                    {:resources (update resources m assoc :started next-bucket)
                     :stats stats
                     :entities
                     (-> entities
                         (update entity-id assoc :step :prod-started :next-bucket new-bucket)
                         (update-vals (fn [other-entity]
                                        (cond-> other-entity
                                          (and (= m (get-in other-entity [:current-operation :m]))
                                               (contains? #{:input-stock :prod-start}
                                                          (:step other-entity)))
                                          (assoc :next-bucket new-bucket)))))}))
   :prod-started
   (fn [{:keys [entities stats resources]} entity-id]
     (let [entity (get entities entity-id)
           {:keys [next-ops current-operation start-bucket next-bucket route-id]} entity
           {:keys [m pt]} current-operation
           rnext-ops (rest next-ops)
           throughput (- next-bucket start-bucket)
           updated-resources (let [cumulated-time (get-in resources [m :cumulated-time] 0)]
                               (-> resources
                                   (update m dissoc :started)
                                   (update m assoc :cumulated-time (+ cumulated-time pt))))]
       (if-let [current-operation (first next-ops)]
         ;;NOTE A next operation happens
         {:resources updated-resources
          :stats stats
          :entities (-> entities
                        (update entity-id
                                assoc
                                :step :input-stock
                                :next-ops rnext-ops
                                :current-operation current-operation))}
         ;;NOTE This was the last operation, cleaning of that entity
         {:resources updated-resources
          :stats (update stats route-id conj throughput)
          :entities (dissoc entities entity-id)})))})

(defn next-entity-buckets
  "Returns the buckets where entities where an event occur"
  [entities]
  (some->> entities
           vals
           (keep (fn [entity] (:next-bucket entity)))
           vec))

(defn- get-earliest-entity-event
  "Returns the first entity that will trigger an event"
  [{:keys [entities]
    :as event-input}
   required-bucket]
  (loop [rentities entities]
    (let [[entity-id {:keys [step next-bucket]}] (first rentities)]
      (if (= next-bucket required-bucket)
        ((get registry step) event-input entity-id)
        (let [rrentities (rest rentities)] (when (seq rrentities) (recur (rest rentities))))))))

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
           entities {}
           stats {}]
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
                                    :stats stats
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
                                :start-bucket bucket
                                :next-bucket bucket
                                :step :input-stock
                                :current-operation (first route)
                                :next-ops (rest route)}
                    entities (assoc entities entity-id new-entity)]
                (if (< nb-entity max-nb-entity)
                  ;;NOTE Prepare next entity creation
                  (recur nb-entity
                         bucket
                         (+ waiting-time next-creation)
                         it
                         resources
                         entities
                         stats)
                  ;;NOTE Create the last one, `nil` next-creation deactivate the feature
                  (recur nb-entity bucket nil it resources entities stats)))
              :else
              ;;NOTE An entity is updated
              (let [{:keys [resources entities stats]} (get-earliest-entity-event
                                                        {:entities entities
                                                         :stats stats
                                                         :resources resources}
                                                        bucket)]
                (recur nb-entity bucket next-creation it resources entities stats)))))))))

;; ********************************************************************************
;; Printers
;; ********************************************************************************

(defn- f
  "To format strings across clojure(script)"
  [s & args]
  #?(:clj (apply format s args)
     :cljs (apply gstring/format s args)))



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

;; ********************************************************************************
;; Tests
;; ********************************************************************************

(-> (run data 200)
    stats)

(-> (run data 9)
    print-workshop)

(time (loop [it (range 0 400)]
        (let [model (run data (first it))]
          (cond
            (= (:status model) :no-workload)
            (do (print-workshop model) (println "no more workload") (println ""))
            (seq it) (do (print-workshop model)
                         (let [rit (rest it)]
                           (if (seq rit)
                             (recur (rest it))
                             (println "stopped before the end of execution"))))))))

(run data 0)
(run data 1)
(run data 2)
(run data 3)
(run data 4)
(run data 8)
(run data 9)
(run data 13)
(run data 14)
(run data 15)
(run data 200)
