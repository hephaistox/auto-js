(ns auto-js.js
  "A jobshop problem is stated with such a map:

  ```
  {:job-nb-ops [3 3]
   :op-machine [0 1 2 0 1 2]
   :op-processtime [9 9 4 16 4 15]}```

  As this namespace is extensively executed, it is optimized. Immutability is turned off and results are updated in place, it means that these functions are updating \"in place\" their arguments with `aset-...`."
  (:refer-clojure :exclude [compile])
  (:require
   [clojure.string :as str]))

(defn compile
  "Turns data of a jobshop instance into arrays, as expected by the rest of the functions below"
  [{:keys [op-processtime job-nb-ops op-machine]
    :as js-pb}]
  (let [op-processtimeI (int-array op-processtime)
        op-nb (alength op-processtimeI)
        job-nb (count job-nb-ops)
        job-ops (->> job-nb-ops
                     (reductions + 0))
        m-nb (count (distinct op-machine))]
    (-> js-pb
        (assoc :job-conjI (int-array (repeat job-nb -1))
               :job-nb job-nb
               :job-opI (int-array job-ops)
               :m-nb m-nb
               :mach-disjI (int-array (repeat m-nb -1))
               :op-jobsI (->> job-nb-ops
                              (interleave (range))
                              (partition 2)
                              (mapcat (fn [[k v]] (repeat v k)))
                              int-array)
               :op-machineI (int-array op-machine)
               :op-nb op-nb
               :op-processtimeI op-processtimeI))))

(defn- is-IntArray-name?
  [kw]
  (= (-> kw
         str
         last)
     \I))

(defn clojurify
  "Turns `js-pb` into idiomatic clojure datastructures"
  [js-pb]
  (->> js-pb
       (mapv (fn [[k v]] [k (if (is-IntArray-name? k) (vec v) v)]))
       (into {})))

(defn create-representation
  "A representation is a mutable set of data"
  [{:keys [op-nb]
    :as _js-pb}]
  {:topoI (int-array (repeat op-nb 0))})

(defn create-solution
  [js-pb]
  (let [{:keys [op-nb]} js-pb]
    {:op-start-timeI (int-array (repeat op-nb 0))
     :op-prevI (int-array (repeat op-nb -1))}))

(defn canonical-repetition
  "Build the canonical repetitionI representation,

  All operations of the first job, then all operations of second job, ..."
  [representation
   {:keys [job-nb-ops]
    :as _js-pb}]
  (-> representation
      (assoc :repetitionI
             (->> job-nb-ops
                  (interleave (range))
                  (partition 2)
                  (mapcat (fn [[n machines]] (repeat machines n)))
                  int-array))))

(defn decode-rep
  "Turns `representation` - a representation with repetitionI - into a topoIlogical order.

  `topoI` mutably updated with that result"
  [representation js-pb]
  (let [{:keys [job-opI op-nb]} js-pb
        {:keys [repetitionI topoI]} representation
        job-firstI (aclone job-opI)]
    (loop [iop 0]
      (when (< iop op-nb)
        (let [job (aget repetitionI iop)
              o (aget job-firstI job)]
          (aset job-firstI job (inc o))
          (aset topoI iop o)
          (recur (inc iop)))))
    representation))

(defn evaluate-schedule
  "Turns a topological order into a schedule"
  [representation solution compiled-js-pb]
  (let [{:keys [job-conjI mach-disjI op-machineI op-nb op-jobsI op-processtimeI]} compiled-js-pb
        {:keys [topoI]} representation
        {:keys [op-start-timeI op-prevI]} solution
        job-conjI (aclone job-conjI)
        mach-disjI (aclone mach-disjI)]
    (loop [n 0]
      (let [np1 (inc n)
            o (aget topoI n)
            m (aget op-machineI o)
            j (aget op-jobsI o)
            os (aget op-start-timeI o)]
        (let [od (aget mach-disjI m)]
          (when (>= od 0)
            (let [de (+ (aget op-start-timeI od) (aget op-processtimeI od))]
              (when (> de os) (aset op-prevI o od) (aset op-start-timeI o de)))))
        (let [oc (aget job-conjI j)]
          (when (>= oc 0)
            (let [je (+ (aget op-start-timeI oc) (aget op-processtimeI oc))]
              (when (> je os) (aset op-prevI o oc) (aset op-start-timeI o je)))))
        (aset mach-disjI m o)
        (aset job-conjI j o)
        (when (< np1 op-nb) (recur np1))))
    solution))

(defn to-mermaid
  "Generates the diagram."
  [{:keys [op-start-timeI]
    :as _solution}
   {:keys [op-processtimeI op-machineI op-jobsI]
    :as _compiled-js-pb}]
  (let [pref (fn [& s] (str "   " (str/join " " s)))]
    (->>
      (for [o (range (count op-start-timeI))]
        (pref (str "o" o "-m" (aget op-machineI o) "-j" (aget op-jobsI o))
              "    :" (aget op-start-timeI o)
              "," (+ (aget op-start-timeI o) (aget op-processtimeI o))))
      (concat
       ["---" "displayMode: compact" "---" "gantt" (pref "title" "jobshop") (pref "dateFormat x")])
      (str/join "\n"))))
