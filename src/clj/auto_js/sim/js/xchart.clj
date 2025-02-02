(ns auto-js.sim.js.xchart
  (:require
   [com.hypirion.clj-xchart :as c]))

(defn- canonical-xchart
  [tb-var]
  (let [v (->> tb-var
               :deltas
               vec
               (sort-by first))]
    {:x (map first v)
     :style {:marker-type :none}
     :y (map second v)}))

(defn products-input-output
  [model-synthesis _params]
  (c/xy-chart (-> {"Products in" [:entities-in "vals"]
                   "Products out" [:entities-out "vals"]}
                  (update-vals (fn [path]
                                 (-> model-synthesis
                                     (get-in path)
                                     canonical-xchart))))
              {:title "Products input output"}))

(defn products-nb
  [model-synthesis _params]
  (c/xy-chart (-> {"Throughputs" (-> (get-in model-synthesis [:entities-in "vals"])
                                     canonical-xchart)})
              {:title "Products number"}))

(defn machine-occupation
  [model-synthesis _params]
  (c/xy-chart (-> (:resources model-synthesis)
                  (update-vals (fn [v]
                                 (-> v
                                     (get-in [:occupation-ma "vals"])
                                     canonical-xchart))))
              {:title "Machine occupation"
               :render-style :area
               :legend :outside-e}))

(defn input-stock
  [model-synthesis _params]
  (c/category-chart (c/transpose-map (-> (:resources model-synthesis)
                                         (update-vals (fn [v]
                                                        (-> v
                                                            (get-in [:nb-in-stock])
                                                            (select-keys
                                                             ["stddev" "max" "mean"]))))))
                    {:title "Input stock"}))

(defn entity-throughput
  [model-synthesis _params]
  (c/category-chart {"Time in workshop" (-> (get-in model-synthesis [:entities :throughputs])
                                            canonical-xchart)}
                    {:title "Entity time in workshop"
                     :legend {:visible? false}}))

(defn build-charts
  [chart-builders model-synthesis params]
  (->> chart-builders
       (mapv (fn [f] (f model-synthesis params)))
       (apply c/view)))
