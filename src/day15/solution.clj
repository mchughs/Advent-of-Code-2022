(ns day15.solution
  (:require [clojure.java.io :as io]))

(defn parse-sensor-beacons [s]
  (->> s
       (re-seq #"-?\d+")
       (map parse-long)
       (partition 2)))

(defn manhattan-distance
  [[x1 y1] [x2 y2]]
  (+ (abs (- x2 x1))
     (abs (- y2 y1))))

(defn sensor+range [[sensor beacon]]
  {:sensor sensor
   :distance (manhattan-distance sensor beacon)})

(defn sensor-in-range?
  [y' {:keys [sensor distance]}]
  (let [[_x y] sensor
        dy (abs (- y' y))]
    (<= dy distance)))

(defn horizontal-coverage
  [y' {[x y] :sensor
       distance :distance}]
  ((juxt #(- x %)
         #(+ x %))
   (- distance
      (abs (- y' y)))))

(defn collapse-consecutive
  "Returns a coll of ranges which do not overlap with eachother."
  [ranges]
  (let [[init-range & sorted-ranges] (sort-by first ranges)]
    (->> sorted-ranges
         (reduce (fn [[[start' end'] :as acc] [start end]]
                   (if (<= start end') ;; consecutive/overlapping
                     (conj (pop acc) [(min start start') (max end end')])
                     (conj acc [start  end])))
                 (list init-range)))))

(defn bisect
  "Ex: (bisect [0 10] 3)  => ([0 2] [4 10])
   Ex: (bisect [0 10] 30) => ([0 10])"
  [[start end] n]
  (if (<= start n end)
    (list [start (dec n)] [(inc n) end])
    (list [start end])))

(defn count-unoccupied
  [interesting-beacons-x ranges]
  ;; remove the spots already occupied by beacons
  (let [bisected-ranges (reduce (fn [acc bisector]
                                  (mapcat #(bisect % bisector) acc))
                                ranges
                                interesting-beacons-x)]
    (transduce (map (comp inc abs #(apply - %)))
               +
               0
               bisected-ranges)))

;; solution part 1

(time
 (with-open [rdr (io/reader "src/day15/input.txt")]
   (let [row-of-interest 2000000
         parsed (->> rdr
                     line-seq
                     (map parse-sensor-beacons))
          ;; beacons which lie in our row-of-interest
         interesting-beacons-x (->> parsed
                                    (map second)
                                    (distinct)
                                    (filter #(= row-of-interest (second %)))
                                    (map first))]
     (->> parsed
          (map sensor+range)
          (filter #(sensor-in-range? row-of-interest %))
          (map #(horizontal-coverage row-of-interest %))
          collapse-consecutive
          (count-unoccupied interesting-beacons-x)))))

;; solution part 2

(defn sensors->edges
  [{[x y] :sensor
    distance :distance}]
  ;; the edge lies just beyond the range of the sensor
  (let [d (inc distance)]
    ;; vertices listed in counter-clockwise order
    (->> [[x (+ y d)]
          [(+ x d) y]
          [x (- y d)]
          [(- x d) y]]
         cycle
         (partition 2 1)
         (take 4)
         (map (fn [[[x1 y1] [x2 y2]]]
                (let [m (/ (- y2 y1) (- x2 x1))
                      b (- y1 (* m x1))]
                  {:m m :b b}))))))

(sensors->edges
 {:sensor '(8 7), :distance 9})

(defn intersection
  "Given two lines return their point of intersection."
  [{m1 :m b1 :b} {m2 :m b2 :b}]
  (when (not= m1 m2) ;; lines with the same slope can't intersect 
    (let [x (/ (- b2 b1) (- m1 m2))
          y (+ (* m1 x) b1)]
      (when (and (int? x) (int? y))
        [x y]))))

(defn sensor-not-in-range?
  "Returns true if the point is outside of the sensor's viewing range."
  [point {:keys [sensor distance]}]
  (> (manhattan-distance point sensor) distance))

(time
 (with-open [rdr (io/reader "src/day15/input.txt")]
   (let [max-value 4000000
         tuning-frequency-fn (fn [[x y]] (+ (* 4000000 x) y))

         sensors (->> rdr
                      line-seq
                      (map parse-sensor-beacons)
                      (map sensor+range))

         {pos-slope-lines 1
          neg-slope-lines -1}
         (->> sensors
              (mapcat sensors->edges)
              distinct
              (group-by :m))

         candidates ;; the hidden point is likely on the intersection of two blind edges
         (->> (for [l+ pos-slope-lines
                    l- neg-slope-lines]
                (intersection l+ l-))
              (remove nil?)
              distinct
              (filter (fn [[x y]]
                        (and (<= 0 x max-value)
                             (<= 0 y max-value)))))]
     (->> candidates
          (filter (fn [candidate]
                    (every? #(sensor-not-in-range? candidate %) sensors)))
          first
          tuning-frequency-fn))))

