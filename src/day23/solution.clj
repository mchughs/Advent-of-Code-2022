(ns day23.solution
  (:require [clojure.string :as string]
            [clojure.set :as set]))

(def enumerate
  #(map-indexed (fn [idx item] [idx item]) %))

(defn neighbors [[x y]]
  (into #{}
        (for [dx [-1 0 1]
              dy [-1 0 1]
              :when (not= dx dy 0)]
          [(+ x dx) (+ y dy)])))

(def directions (cycle '(:N :S :W :E)))
(def direction->fn
  {:N (fn [[x y]]
        [[(dec x) (dec y)]
         [x       (dec y)]
         [(inc x) (dec y)]])
   :S (fn [[x y]]
        [[(dec x) (inc y)]
         [x       (inc y)]
         [(inc x) (inc y)]])
   :W (fn [[x y]]
        [[(dec x) (dec y)]
         [(dec x)      y]
         [(dec x) (inc y)]])
   :E (fn [[x y]]
        [[(inc x) (dec y)]
         [(inc x)      y]
         [(inc x) (inc y)]])})

(defn gather-proposals [elves round]
  (let [priorities (->> directions
                        (drop round)
                        (take 4))]
    (reduce (fn [acc elf]
              (if-let [_alone? (empty? (set/intersection (neighbors elf) elves))]
                (assoc acc elf elf)
                (let [options (->> priorities
                                   (map direction->fn)
                                   (map #(% elf)))
                      proposal (->> options
                                    (filter #(empty? (set/intersection (set %) elves)))
                                    first
                                    second)]
                  (assoc acc elf (or proposal elf)))))
            {}
            elves)))

(defn move [proposals]
  (let [freq (frequencies (vals proposals))]
    (reduce-kv (fn [acc elf proposal]
                 (when (nil? elf) (println elf proposal))
                 (if (= 1 (freq proposal))
                   (conj acc proposal)
                   (conj acc elf)))
               #{}
               proposals)))

(defn display! [elves]
  (let [right (transduce (map first) max 0 elves)
        left (transduce (map first) min 0 elves)
        bottom (transduce (map last) max 0 elves)
        top (transduce (map last) min 0 elves)]
    (doseq [y (range top (inc bottom))]
      (->> (range left (inc right))
           (map (fn [x] (cond
                          (= [0 0] [x y])           "s"
                          (contains? elves [x y])   "#"
                          :else                     ".")))
           (apply str)
           println))))

(defn count-ground-tiles [elves]
  (let [right (transduce (map first) max 0 elves)
        left (transduce (map first) min 0 elves)
        bottom (transduce (map last) max 0 elves)
        top (transduce (map last) min 0 elves)]
    (->> (for [y (range top (inc bottom))
               x (range left (inc right))]
           (when-not (contains? elves [x y])
             [x y]))
         (remove nil?)
         count)))

;; solution part 1

(time
 (let [elves (->> (for [[y line] (enumerate (string/split-lines (slurp "src/day23/input.txt")))]
                    (for [[x char] (enumerate line)
                          :when (= char \#)]
                      [x y]))
                  (apply concat)
                  (into #{}))
       limit 10]
   (count-ground-tiles
    (loop [round 0
           elves elves]
      (if (= round limit)
        elves
        (let [proposals (gather-proposals elves round)
              elves' (move proposals)]
          (recur (inc round) elves')))))))

;; solution part 2

(defn run-round [[elves round]] 
  [(move (gather-proposals elves round))
   (inc round)])

(time
 (let [elves (->> (for [[y line] (enumerate (string/split-lines (slurp "src/day23/input.txt")))]
                    (for [[x char] (enumerate line)
                          :when (= char \#)]
                      [x y]))
                  (apply concat)
                  (into #{}))]
   (->> [elves 0]
        (iterate run-round)
        (partition 2 1)
        (take-while (fn [[[prev _]
                          [next _]]]
                      (not= prev next)))

        count
        inc)))
