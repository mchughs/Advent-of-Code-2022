(ns day18.solution
  (:require [clojure.java.io :as io]
            [clojure.math.combinatorics :as combo]
            [clojure.set :as set]))

(defn parse-input [input]
  (mapv parse-long (re-seq #"\d+" input)))

(defn adjacent? [[a b]]
  (#{[1 0 0]
     [0 1 0]
     [0 0 1]}
   (mapv (comp abs -) a b)))

;; solution part 1

(time
 (with-open [rdr (io/reader "src/day18/input.txt")]
   (let [cubes (->> rdr line-seq (map parse-input))
         total-faces (* 6 (count cubes))
         touching-faces (* 2 (count (filter adjacent? (combo/combinations cubes 2))))]
     (- total-faces
        touching-faces))))

;; solution part 2

(defn generate-wrapping-mesh
  [minimum maximum]
  (let [r (range minimum (inc maximum))]
    (set (for [x r y r z r]
           [x y z]))))

(defn adjacent-cubes [[x y z]]
  #{[(inc x)      y       z]
    [x       (inc y)      z]
    [x            y  (inc z)]
    [(dec x)      y       z]
    [x       (dec y)      z]
    [x            y  (dec z)]})

(defn dfs-lazy [graph start]
  (let [step (fn step [[curr & remaining] visited]
               (lazy-seq
                (when (seq curr)
                  (let [neighbors (remove visited (graph curr))]
                    (cons curr (step (concat neighbors remaining)
                                     (conj visited curr)))))))]
    (step (list start) #{})))

(time
 (with-open [rdr (io/reader "src/day18/input.txt")]
   (let [cubes (->> rdr line-seq (map parse-input) set)
         minimum (dec (apply min (mapcat identity cubes)))
         maximum (inc (apply max (mapcat identity cubes)))
         mesh (generate-wrapping-mesh minimum maximum)
         graph (reduce (fn [m cube]
                         ;; remove any cubes that are part of the lava-glob and only include cubes which are part of our mesh
                         (let [reachable-neighbors (set/intersection mesh (set/difference (adjacent-cubes cube) cubes))]
                           (assoc m cube reachable-neighbors)))
                       {}
                       (set/difference mesh cubes))
         ;; creates a cubic mould around our lava-glob
         mould (set (dfs-lazy graph [minimum minimum minimum]))
         mould-total-faces (* 6 (count mould))
         mould-touching-faces (* 2 (count (filter adjacent? (combo/combinations mould 2))))
         mould-length (inc (- maximum minimum))
         mould-exterior-surface (* 6 mould-length mould-length) ;; surface area of a cube
         ]
     ;; this gives the surfaces area of the interior of the mould 
     ;; which is equivalent to the surface area of the exterior of the lava-glob
     (- mould-total-faces
        mould-touching-faces
        mould-exterior-surface))))
