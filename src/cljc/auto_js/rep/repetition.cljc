(ns auto-js.rep.repetition)

(defn canonical
  "Build the canonical repetition representation,

  All operations of first operation, then all operations of second operation, ..."
  [{:keys [jobs]
    :as _js-pb}]
  (->> jobs
       (mapcat (fn [[job-id job-ops]] (repeat (count job-ops) job-id)))))

(defn topological-order
  "Turns the representation with repetition into a topological order"
  [_js-pb rep]
  (reduce (fn [_v _job] nil) {} rep)
  #_(loop [rep rep
           js-pb js-pb
           decoded-rep []]
      (if rep
        (let [job (first rep)]
          (recur (next rep)
                 (assoc js-pb job (next job-sequence))
                 (conj decoded-rep (assoc (first job-sequence) :n job))))
        decoded-rep)))

{:jobs {:J1 [0 1 2]
        :J2 [3 4 5]}
 :machines {:M0 [0 3]
            :M1 [4]
            :M11 [1]
            :M2 [2 5]}
 :name :test-instance
 :operations [{:id 0
               :j :J1
               :m :M0
               :p 9
               :pos 0}
              {:id 1
               :j :J1
               :m :M11
               :p 9
               :pos 1}
              {:id 2
               :j :J1
               :m :M2
               :p 4
               :pos 2}
              {:id 3
               :j :J2
               :m :M0
               :p 16
               :pos 0}
              {:id 4
               :j :J2
               :m :M1
               :p 4
               :pos 1}
              {:id 5
               :j :J2
               :m :M2
               :p 15
               :pos 2}]}
