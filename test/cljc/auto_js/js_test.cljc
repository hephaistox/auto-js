(ns auto-js.js-test
  (:require
   [auto-js.js  :as sut]
   #?(:clj [clojure.test :refer [deftest is]]
      :cljs [cljs.test :refer [deftest is] :include-macros true])))

(def js-pb-example
  "Data from the exercise"
  (-> {:job-nb-ops [3 3]
       :op-machine [0 1 2 0 1 2]
       :op-processtime [5 6 7 8 9 10]}
      sut/compile))

(deftest clojurify-test
  (is (= {:job-conjI [-1 -1]
          :job-nb 2
          :job-nb-ops [3 3]
          :job-opI [0 3 6]
          :m-nb 3
          :mach-disjI [-1 -1 -1]
          :op-jobsI [0 0 0 1 1 1]
          :op-machine [0 1 2 0 1 2]
          :op-machineI [0 1 2 0 1 2]
          :op-nb 6
          :op-processtime [5 6 7 8 9 10]
          :op-processtimeI [5 6 7 8 9 10]}
         (sut/clojurify js-pb-example))))

(deftest create-representation-test
  (is (= {:topoI [0 0 0 0 0 0]}
         (-> js-pb-example
             sut/create-representation
             sut/clojurify))))

(deftest create-solution-test
  (is (= {:op-start-timeI [0 0 0 0 0 0]
          :op-prevI [-1 -1 -1 -1 -1 -1]}
         (-> js-pb-example
             sut/create-solution
             sut/clojurify))))

(deftest canonical-repetition-test
  (is (= {:topoI [0 0 0 0 0 0]
          :repetitionI [0 0 0 1 1 1]}
         (-> (sut/create-representation js-pb-example)
             (sut/canonical-repetition js-pb-example)
             sut/clojurify))))

(deftest decode-rep-test
  (is (= [0 3 1 4 2 5]
         (-> {:topoI [0 0 0 0 0 0]
              :repetitionI [0 1 0 1 0 1]}
             (update :topoI int-array)
             (update :repetitionI int-array)
             (sut/decode-rep js-pb-example)
             :topoI
             vec)))
  (is (= [0 1 2 3 4 5]
         (-> (sut/create-representation js-pb-example)
             (sut/canonical-repetition js-pb-example)
             (sut/decode-rep js-pb-example)
             :topoI
             vec))))

(deftest evaluate-schedule-test
  (is (-> (sut/create-representation js-pb-example)
          (sut/canonical-repetition js-pb-example)
          (sut/decode-rep js-pb-example)
          (sut/evaluate-schedule (sut/create-solution js-pb-example) js-pb-example)
          sut/clojurify)))

#?(:clj (spit "tmp/test.mermaid"
              (-> (sut/create-representation js-pb-example)
                  (sut/canonical-repetition js-pb-example)
                  (sut/decode-rep js-pb-example)
                  (sut/evaluate-schedule (sut/create-solution js-pb-example) js-pb-example)
                  (sut/to-mermaid js-pb-example))))
