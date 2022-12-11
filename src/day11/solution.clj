(ns day11.solution
  (:require [clojure.string :as string]))

(defn parse-operation [[arg1 operation arg2]]
  (memoize
   (fn [old]
     (eval
      (list
       (read-string operation)
       (if (= arg1 "old") old (read-string arg1))
       (if (= arg2 "old") old (read-string arg2)))))))

(defn parse-monkey-input [monkey-input]
  (let [[monkey-number-input
         starting-items-input
         operation-input
         pred-input
         true-case-input
         false-case-input] (string/split-lines monkey-input)
        monkey-number (parse-long (re-find #"\d+" monkey-number-input))
        starting-items (mapv parse-long (re-seq #"\d+" starting-items-input))
        operation (parse-operation (re-seq #"old|\+|\*|\d+" operation-input))
        divisor (parse-long (re-find #"\d+" pred-input))
        pred (memoize #(zero? (mod % divisor)))
        true-target (parse-long (re-find #"\d+" true-case-input))
        false-target (parse-long (re-find #"\d+" false-case-input))]
    {:monkey-number monkey-number
     :items starting-items
     :operation operation
     :pred pred
     :true-target true-target
     :false-target false-target
     :inspected-count 0
     :divisor divisor ;; just needed for part 2
     }))

(defn run-turn [turn monkey-profiles]
  (let [{:keys [monkey-number items operation
                pred true-target false-target]}
        (get monkey-profiles turn)]
    (reduce
     (fn [profiles item]
       (let [worry-level (quot (operation item) 3)
             target (if (pred worry-level)
                      true-target
                      false-target)]
         (-> profiles
             ;; from
             (update-in [monkey-number :items] (comp vec rest))
             (update-in [monkey-number :inspected-count] inc)
             ;; to
             (update-in [target :items] conj worry-level))))
     monkey-profiles
     items)))

(defn run-round [monkey-profiles]
  (loop [turn 0
         monkey-profiles monkey-profiles]
    (if (>= turn (count monkey-profiles))
      monkey-profiles
      (recur (inc turn)
             (run-turn turn monkey-profiles)))))

(defn largest-two
  "Returns the two largest numbers from a sequence"
  [coll]
  (take 2 (sort > coll)))

;; solution part 1

(time
 (let [monkey-inputs (string/split (slurp "src/day11/input.txt") #"\n\n")
       monkey-profiles (mapv parse-monkey-input monkey-inputs)
       target-round 20
       inspected-counts (->> monkey-profiles
                             (iterate run-round)
                             (take (inc target-round))
                             last
                             (map :inspected-count))]
   (reduce * (largest-two inspected-counts))))

;; solution part 2

(defn run-turn' [lcm turn monkey-profiles]
  (let [{:keys [monkey-number items operation
                pred true-target false-target]}
        (get monkey-profiles turn)]
    (reduce
     (fn [profiles item]
       (let [worry-level (mod (operation item) lcm) ;; can toss away a lot of the number while preserving the divisability of it.
             target (if (pred worry-level)
                      true-target
                      false-target)]
         (-> profiles
             ;; from
             (update-in [monkey-number :items] (comp vec rest))
             (update-in [monkey-number :inspected-count] inc)
             ;; to
             (update-in [target :items] conj worry-level))))
     monkey-profiles
     items)))

(defn run-round' [lcm monkey-profiles]
  (loop [turn 0
         monkey-profiles monkey-profiles]
    (if (>= turn (count monkey-profiles))
      monkey-profiles
      (recur (inc turn)
             (run-turn' lcm turn monkey-profiles)))))

(time
 (let [monkey-inputs (string/split (slurp "src/day11/input.txt") #"\n\n")
       monkey-profiles (mapv parse-monkey-input monkey-inputs)
       lcm (transduce (map :divisor) * monkey-profiles)
       target-round 10000
       inspected-counts (->> monkey-profiles
                             (iterate (partial run-round' lcm))
                             (take (inc target-round))
                             last
                             (map :inspected-count))]
   (reduce * (largest-two inspected-counts))))
