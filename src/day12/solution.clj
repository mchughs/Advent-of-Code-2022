(ns day12.solution 
  (:require [clojure.string :as string]
            [clojure.set :as set]))

(defn- enumerate [coll]
  (map-indexed (fn [idx item] [idx item]) coll))

(defn- on-the-field? [width height [x' y']]
  (and (< -1 x' width)
       (< -1 y' height)))

(defn- scalable? [v v']
  (cond
    ;; end has an elevation equivalent to \z, 122
    (= :end v')  (#{122 121} v)
    ;; start has an elevation equivalent to \a, 97
    (= :start v) (#{97 98} v')
    :else
    (let [dv (- v' v)]
      (<= dv 1))))

(def plus
  (partial mapv +))

(defn traversable-points [topography width height p visited]
  (->> '([-1 0] [1 0] [0 -1] [0 1])
       (map #(plus p %))
       (remove visited)
       (filter #(on-the-field? width height %))
       (filter #(scalable? (get topography p)
                           (get topography %)))))

(defn next-steps
  [topography width height visited-points]
  (set/union
   visited-points
   (->> visited-points
        (mapcat #(traversable-points topography width height % visited-points))
        set)))

;; solution part 1

(time
 (let [lines (string/split-lines (slurp "src/day12/input.txt"))
       width (count (first lines))
       height (count lines)
       topography (->> (for [[y row] (enumerate lines)]
                         (for [[x v] (enumerate row)]
                           (let [value (case v
                                         \S :start
                                         \E :end
                                         (int v))]
                             {[x y] value})))
                       (apply concat)
                       (apply merge))
       start-point (->> topography
                        (filter (fn [[_k v]] (= :start v)))
                        ffirst)
       end-point (->> topography
                      (filter (fn [[_k v]] (= :end v)))
                      ffirst)]
   (->> #{start-point}
        (iterate #(next-steps topography width height %))
        (take-while #(not (contains? % end-point)))
        count)))

;;solution part 2

(defn traversable-points' [topography width height p visited]
  (->> '([-1 0] [1 0] [0 -1] [0 1])
       (map #(plus p %))
       (remove visited)
       (filter #(on-the-field? width height %))
       (filter #(scalable? (get topography %) ;; flipped p and p' from part 1 since we are traversing from the end in part 2.
                           (get topography p)))))

(defn next-steps'
  [topography width height visited-points]
  (set/union
   visited-points
   (->> visited-points
        (mapcat #(traversable-points' topography width height % visited-points))
        set)))

;; use ray tracing from the end to see what is the first \a hit.
(time
 (let [lines (string/split-lines (slurp "src/day12/input.txt"))
       width (count (first lines))
       height (count lines)
       topography (->> (for [[y row] (enumerate lines)]
                         (for [[x v] (enumerate row)]
                           (let [value (case v
                                         \S 97
                                         \E :end
                                         (int v))]
                             {[x y] value})))
                       (apply concat)
                       (apply merge))
       all-start-points (->> topography
                             (filter (fn [[_k v]] (= 97 v)))
                             (map first)
                             set)
       end-point (->> topography
                      (filter (fn [[_k v]] (= :end v)))
                      ffirst)]
   (->> #{end-point}
        (iterate #(next-steps' topography width height %))
        (take-while #(empty? (set/intersection % all-start-points)))
        count)))

;; BONUS

(defn visualize!  
  [start-point end-point height width visited-points]  
  (doseq [y (range 0 height)]
    (->> (range 0 width)
         (map (fn [x] (cond
                        (= start-point
                           [x y])                 "S"
                        (= end-point
                           [x y])                 "E"
                        (contains? visited-points
                                   [x y])         "#"
                        :else                     ".")))
         (apply str)
         println)))

(defn visualize!'
  [start-points end-point height width visited-points]
  (doseq [y (range 0 height)]
    (->> (range 0 width)
         (map (fn [x] (cond
                        (contains? start-points
                                   [x y])         "a"
                        (= end-point
                           [x y])                 "E"
                        (contains? visited-points
                                   [x y])         "#"
                        :else                     ".")))
         (apply str)
         println)))
