(ns day14.solution
  (:require [clojure.java.io :as io]
            [clojure.set :as set]))

(def hole [500 0])

(declare visualize! visualize!')

(defn parse-path [path-str]
  (->> (re-seq #"\d+" path-str)
       (map parse-long)
       (partition 2)
       (partition 2 1)))

(defn tumble-sand
  "returns sand + one more rested grain"
  [terrain sand]
  (let [points (set/union terrain sand)
        abyss  (apply max (map last terrain))]
    (conj sand
          (loop [[x y] hole]
            (when-not (< abyss y)
              (if-let [next-point
                       (->> [[x  (inc y)]  ;; below
                             [(dec x) (inc y)]  ;; below-left
                             [(inc x) (inc y)]] ;; below-right
                            (filter #(not (contains? points %)))
                            first)]
                (recur next-point)
                [x y]))))))

;; solution part 1

(time
 (with-open [rdr (io/reader "src/day14/input.txt")]
   (let [terrain
         (->> rdr
              line-seq
              (map parse-path)
              (apply concat)
              (reduce
               (fn [acc [[x1 y1] [x2 y2]]]
                 (let [[x1 x2] (sort [x1 x2])
                       [y1 y2] (sort [y1 y2])
                       points (for [x (range x1 (inc x2))
                                    y (range y1 (inc y2))]
                                [x y])]
                   (apply conj acc points)))
               #{}))
         sand (->> #{}
                   (iterate #(tumble-sand terrain %))
                   (take-while #(not (contains? % nil)))
                   last)]
     #_(visualize! terrain sand)
     (count sand))))

;; solution part 2

(defn tumble-sand'
  "returns sand + one more rested grain"
  [terrain sand]
  (let [points (set/union terrain sand)
        floor  (inc (apply max (map last terrain)))]
    (conj sand
          (loop [[x y] hole] ;; NOTE: could significantly speed up by starting the loop at the point just before the last grain settled
            (if-let [next-point
                     (->> [[x       (inc y)]  ;; below
                           [(dec x) (inc y)]  ;; below-left
                           [(inc x) (inc y)]] ;; below-right
                          (filter #(and (not (contains? points %))
                                        (not (< floor (second %)))))
                          first)]
              (recur next-point)
              [x y])))))

(time
 (with-open [rdr (io/reader "src/day14/input.txt")]
   (let [terrain
         (->> rdr
              line-seq
              (map parse-path)
              (apply concat)
              (reduce
               (fn [acc [[x1 y1] [x2 y2]]]
                 (let [[x1 x2] (sort [x1 x2])
                       [y1 y2] (sort [y1 y2])
                       points (for [x (range x1 (inc x2))
                                    y (range y1 (inc y2))]
                                [x y])]
                   (apply conj acc points)))
               #{}))
         sand (->> #{}
                   (iterate #(tumble-sand' terrain %))
                   (take-while #(not (contains? % hole)))
                   last
                   (tumble-sand' terrain) ;; last grain plugs the hole.
                   )]
     #_(visualize!' terrain sand)
     (count sand))))

;; BONUS

(defn visualize!
  [terrain sand]
  (let [[left right]  ((juxt #(apply min %)
                             #(apply max %))
                       (map first terrain))
        [top  bottom] ((juxt #(apply min 0 %)
                             #(apply max %))
                       (map last terrain))]
    (doseq [y (range top (inc bottom))]
      (->> (range left (inc right))
           (map (fn [x] (cond
                          (= hole [x y])            "+"
                          (contains? terrain [x y]) "#"
                          (contains? sand [x y])    "o"
                          :else                     ".")))
           (apply str)
           println))))

(defn visualize!'
  [terrain sand]
  (let [[left right]  ((juxt #(dec (apply min %))
                             #(inc (apply max %)))
                       (map first sand))
        [top  bottom] ((juxt #(apply min 0 %)
                             #(inc (apply max %)))
                       (map last sand))]
    (doseq [y (range top (inc bottom))]
      (->> (range left (inc right))
           (map (fn [x] (cond
                          (= hole [x y])            "+"
                          (contains? terrain [x y]) "#"
                          (= bottom y)              "#"
                          (contains? sand [x y])    "o"
                          :else                     ".")))
           (apply str)
           println))))

