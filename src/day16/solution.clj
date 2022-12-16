(ns day16.solution
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.math.combinatorics :as combo]))

;; --- A* algorithm code taken from https://matthewdowney.github.io/astar-in-clojure-find-k-shortest-paths.html

(declare a*, a*-seq, next-a*-path, unseen?, step-factory, rpath, cmp-step)

(defn a*
  "A sequence of paths from `src` to `dest`, shortest first, within the supplied `graph`.
  If the graph is weighted, supply a `distance` function. To make use of A*, supply a 
  heuristic function. Otherwise performs like Dijkstra's algorithm."
  [graph src dest & {:keys [distance heuristic]}]
  (let [init-adjacent (sorted-set-by cmp-step {:node src :cost 0 :entered 0})]
    (a*-seq graph dest init-adjacent
            (or distance (constantly 1))
            (or heuristic (constantly 0)))))

(defn a*-seq
  "Construct a lazy sequence of calls to `next-a*-path`, returning the shortest path first."
  [graph dest adjacent distance heuristic]
  (lazy-seq
   (when-let [[path, adjacent'] (next-a*-path graph dest adjacent distance heuristic)]
     (cons path (a*-seq graph dest adjacent' distance heuristic)))))

(defn next-a*-path [graph dest adjacent f-cost f-heur]
  (when-let [{:keys [node] :as current} (first adjacent)]
    (let [path (rpath current)
          adjacent' (disj adjacent current)] ;; "pop" the current node
      (if (= node dest)
        [(reverse path), adjacent']
        (let [last-idx (or (:entered (last adjacent')) 0)
              factory (step-factory current last-idx f-cost f-heur dest)
              xform (comp (filter (partial unseen? path)) (map-indexed factory))
              adjacent'' (into adjacent' xform (get graph node))]
          (recur graph dest adjacent'' f-cost f-heur))))))

(defn unseen? [path node]
  (not-any? #{node} path))

(defn step-factory [parent last-insertion cost heur dest]
  (fn [insertion-idx node]
    {:parent parent
     :node node
     :entered (+ last-insertion (inc insertion-idx))
     :cost (+ (:cost parent) (cost (:node parent) node) (heur node dest))}))

(defn rpath [{:keys [node parent]}]
  (lazy-seq
   (cons node (when parent (rpath parent)))))

(defn cmp-step [step-a step-b]
  (let [cmp (compare (:cost step-a) (:cost step-b))]
    (if (zero? cmp)
      (compare (:entered step-a) (:entered step-b))
      cmp)))

;; ---

(defn parse-input [s]
  (let [[_ valve flow-rate neighbors]
        (re-find #"Valve ([A-Z]{2}) has flow rate=(\d+); tunnels? leads? to valves? (.+)"
                 s)]
    {:valve valve
     :flow-rate (parse-long (or flow-rate "0"))
     :neighbors (set (string/split neighbors #", "))}))

(defn path-score
  [graph open-valves [action & remaining] location time score]
  (let [time' (dec time)
        release-rate (reduce (fn [acc valve] (+ acc (get-in graph [valve :flow-rate])))
                             0
                             open-valves)
        score' (+ release-rate score)]
    (cond
      (nil? action)
      (+ score (* time release-rate))

      (= :open action)
      (let [open-valves' (conj open-valves location)]
        (recur graph open-valves' remaining location time' score'))

      :else
      (let [location' action]
        (recur graph open-valves remaining location' time' score')))))

;; solution part 1

;; took about 10 minutes... pretty bad for a part 1.
;; I think investing into build a useful-graph where all the 0 values (except for AA)
;; are tossed out and the edges get weighted with the shortest path length.
;; this would necessitate a re-write of the `path-score` function since it expects
;; a path to be described in 1 minute steps.
(time
 (with-open [rdr (io/reader "src/day16/input.txt")]
   (let [start "AA"

         minutes 30

         graph (->> rdr
                    line-seq
                    (transduce
                     (map parse-input)
                     (fn
                       ([m] m)
                       ([m {k :valve :as v}]
                        (assoc m k v)))
                     {}))

         useful-valves (->> graph
                            vals
                            (remove #(zero? (:flow-rate %)))
                            (sort-by :flow-rate >)
                            (map :valve))

         heuristic-visits 7 ;; this is just a guess on my part. I'll work my way up till I find the answer
         permutations (mapcat combo/permutations (combo/combinations useful-valves heuristic-visits))

         simplified-graph (reduce-kv #(assoc %1 %2 (:neighbors %3)) {} graph)

         start-paths  (transduce
                       ;; taking the first but the right path might be one of the other ones! 
                       (map #(first (a* simplified-graph start %)))
                       (fn
                         ([m] m)
                         ([m path]
                          (assoc m [start (last path)] path)))
                       {}
                       useful-valves)

         useful-paths (->> (for [x useful-valves
                                 y useful-valves
                                 :when (not= x y)]
                             [x y])
                           (transduce
                            (map (fn [[x y]]
                                   (first ;; taking the first but the right path might be one of the other ones! 
                                    (a* simplified-graph x y))))
                            (fn
                              ([m] m)
                              ([m path]
                               (assoc m [(first path) (last path)] path)))
                            {}))

         candidate-paths (->> permutations
                              (map (comp
                                    #(mapcat (fn [x]
                                               (if (= x :open)
                                                 '(:open)
                                                 (rest (or (get useful-paths x)
                                                           (get start-paths x)))))
                                             %)
                                    #(interleave % (repeat :open))
                                    #(partition 2 1 %)
                                    #(conj % start)))
                              ;; there cannot be more actions than minutes
                              (remove #(> (count %) minutes)))]
     (->> candidate-paths
          (transduce (map #(path-score graph #{} % start minutes 0))
                     max
                     0)))))

;;--- solution part 2

;; answer is 2496. Ran someone else's part 2 code on my input. Their part 1 answer wasn't right for my input though.
(time ;; taking even longer to execute.
 (with-open [rdr (io/reader "src/day16/input.txt")]
   (let [start "AA"

         minutes 26

         graph (->> rdr
                    line-seq
                    (transduce
                     (map parse-input)
                     (fn
                       ([m] m)
                       ([m {k :valve :as v}]
                        (assoc m k v)))
                     {}))

         useful-valves (->> graph
                            vals
                            (remove #(zero? (:flow-rate %)))
                            (sort-by :flow-rate >)
                            (map :valve))

         heuristic-visits 6 ;; this is just a guess on my part. I'll work my way up till I find the answer
         permutations (mapcat combo/permutations (combo/combinations useful-valves heuristic-visits))

         simplified-graph (reduce-kv #(assoc %1 %2 (:neighbors %3)) {} graph)

         start-paths  (transduce
                       ;; taking the first but the right path might be one of the other ones! 
                       (map #(first (a* simplified-graph start %)))
                       (fn
                         ([m] m)
                         ([m path]
                          (assoc m [start (last path)] path)))
                       {}
                       useful-valves)

         useful-paths (->> (for [x useful-valves
                                 y useful-valves
                                 :when (not= x y)]
                             [x y])
                           (transduce
                            (map (fn [[x y]]
                                   (first ;; taking the first but the right path might be one of the other ones! 
                                    (a* simplified-graph x y))))
                            (fn
                              ([m] m)
                              ([m path]
                               (assoc m [(first path) (last path)] path)))
                            {}))

         share (quot heuristic-visits 2) ;; each you an the elephant will take half the important valves

         {:keys [your-share elephant-share]}
         (transduce (map #(split-at share %))
                    (fn
                      ([m] m)
                      ([{:keys [your-share elephant-share] :as acc}
                        [your-valves elephant-valves]]
                       ;; remove symmetric distributions of the valves
                       ;; doesn't matter which of you carries out which half of the plan
                       (if (or (contains? (set your-share) elephant-valves)
                               (contains? (set elephant-share) your-valves))
                         acc
                         {:your-share (conj your-share your-valves) :elephant-share (conj elephant-share elephant-valves)})))
                    {:your-share '() :elephant-share '()}
                    permutations)

         your-candidate-paths (->> your-share
                                   (map (comp
                                         #(mapcat (fn [x]
                                                    (if (= x :open)
                                                      '(:open)
                                                      (rest (or (get useful-paths x)
                                                                (get start-paths x)))))
                                                  %)
                                         #(interleave % (repeat :open))
                                         #(partition 2 1 %)
                                         #(conj % start)))
                              ;; there cannot be more actions than minutes
                                   (remove #(> (count %) minutes)))

         elephant-candidate-paths (->> elephant-share
                                       (map (comp
                                             #(mapcat (fn [x]
                                                        (if (= x :open)
                                                          '(:open)
                                                          (rest (or (get useful-paths x)
                                                                    (get start-paths x)))))
                                                      %)
                                             #(interleave % (repeat :open))
                                             #(partition 2 1 %)
                                             #(conj % start)))
                              ;; there cannot be more actions than minutes
                                       (remove #(> (count %) minutes)))

         your-scores (map #(path-score graph #{} % start minutes 0) your-candidate-paths)
         elephant-scores (map #(path-score graph #{} % start minutes 0) elephant-candidate-paths)
         total-scores (map + your-scores elephant-scores)]
     (apply max total-scores))))

(comment
  (path-score
   {"JJ" {:valve "JJ", :flow-rate 21, :neighbors #{"II"}},
    "HH" {:valve "HH", :flow-rate 22, :neighbors #{"GG"}},
    "FF" {:valve "FF", :flow-rate 0, :neighbors #{"GG" "EE"}},
    "GG" {:valve "GG", :flow-rate 0, :neighbors #{"HH" "FF"}},
    "DD" {:valve "DD", :flow-rate 20, :neighbors #{"CC" "AA" "EE"}},
    "CC" {:valve "CC", :flow-rate 2, :neighbors #{"DD" "BB"}},
    "II" {:valve "II", :flow-rate 0, :neighbors #{"JJ" "AA"}},
    "BB" {:valve "BB", :flow-rate 13, :neighbors #{"CC" "AA"}},
    "AA" {:valve "AA", :flow-rate 0, :neighbors #{"DD" "II" "BB"}},
    "EE" {:valve "EE", :flow-rate 3, :neighbors #{"FF" "DD"}}}
   #{}
   '("DD" :open "CC" "BB" :open
          "AA" "II" "JJ" :open "II"
          "AA" "DD" "EE" "FF" "GG" "HH"
          :open "GG" "FF" "EE" :open "DD"
          "CC" :open)
   "AA"
   30
   0)

  (def graph
    {"JJ" #{"II"},
     "HH" #{"GG"},
     "FF" #{"GG" "EE"},
     "GG" #{"HH" "FF"},
     "DD" #{"CC" "AA" "EE"},
     "CC" #{"DD" "BB"},
     "II" #{"JJ" "AA"},
     "BB" #{"CC" "AA"},
     "AA" #{"DD" "II" "BB"},
     "EE" #{"FF" "DD"}})

  (a* graph "AA" "JJ"))
