(ns auto-js.rendering.printer
  "A printer rendering displaying the list of machines, their stats, which products are in processing or in input stock."
  (:require
   [auto-core.string.format :refer [f pf pfln]]
   [auto-opti.routings      :refer [machines]]
   [clojure.string          :as str]))

(defn- rate [val all] (if (and (number? val) (pos? all)) (* 100.0 (/ val all)) 0.0))

(defn- occupation-s
  [machine bucket]
  (let [{:keys [cumulated-time machine-start]
         :or {cumulated-time 0}}
        machine
        updated-cumulated
        (if machine-start (+ cumulated-time (- bucket machine-start)) cumulated-time)]
    (f "%3.2f%%" (rate updated-cumulated bucket))))

(defn- wip-machine-s
  [machine]
  (let [{:keys [machine-start machine-end]} machine]
    (if machine-start (f "(%3s -> %3s)" (or machine-start "") (or machine-end "")) "( ________ )")))

(defn- stock-occupation [machine bucket] (f "%3.2f%%" (rate (:stock-occupation machine) bucket)))

(defn- other-entities
  [other-entities]
  (->> other-entities
       (map (fn [[entity-step entities]] (f "%s(%3d)" (name entity-step) (count entities))))
       (apply str "products ")))

(defn separator [] (println (apply str (repeat 80 "*"))))

(defn print-workshop
  [model print-event-fn]
  (let [{:keys [entities bucket it resources stats past-events]} model]
    (separator)
    (pfln "it=%3d, bucket= %3d, #product finished %-5d"
          it
          bucket
          (or (->> stats
                   (mapcat second)
                   count)
              0))
    (print-event-fn (first past-events))
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
