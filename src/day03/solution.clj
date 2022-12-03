(ns day03.solution
  (:require [clojure.java.io :as io]
            [clojure.set :as set]))

;; solution part 1

(defn- find-duplicate-item [rucksack]
  (let [size (count rucksack)
        compartment-1 (set (subs rucksack 0 (/ size 2)))
        compartment-2 (set (subs rucksack (/ size 2) size))]
    (first (set/intersection compartment-1 compartment-2))))

(defn- prioritize [item]
  (let [ascii (int item)
        offset (if (< ascii 91)
                 38 ;; uppercase
                 96 ;; lowercase
                 )]
    (- ascii offset)))

(time
 (with-open [rdr (io/reader "src/day03/input.txt")]
   (->> rdr
        line-seq
        (transduce (comp (map find-duplicate-item)
                         (map prioritize))
                   +
                   0))))

;; solution part 2

(defn- find-duplicate-item' [elf-rucksacks]
  (->> elf-rucksacks
       (map set)
       (apply set/intersection)
       first))

(time
 (with-open [rdr (io/reader "src/day03/input.txt")]
   (->> rdr
        line-seq
        (partition 3)
        (transduce (comp (map find-duplicate-item')
                         (map prioritize))
                   +
                   0))))
