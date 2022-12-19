(ns day19.solution 
  (:require [clojure.java.io :as io]))

(defn parse-input [input]
  (let [[blueprint-id
         ore-robot-ore-cost
         clay-robot-ore-cost
         obsidian-robot-ore-cost
         obsidian-robot-clay-cost
         geode-robot-ore-cost
         geode-robot-obsidian-cost]
        (map parse-long (re-seq #"\d+" input))]
    {:id blueprint-id
     :ore-robot {:ore ore-robot-ore-cost
                 :clay 0
                 :obsidian 0}
     :clay-robot {:ore clay-robot-ore-cost
                  :clay 0
                  :obsidian 0}
     :obsidian-robot {:ore obsidian-robot-ore-cost
                      :clay obsidian-robot-clay-cost
                      :obsidian 0}
     :geode-robot {:ore geode-robot-ore-cost
                   :clay 0
                   :obsidian geode-robot-obsidian-cost}}))

(defn affordable? [minerals robot-blueprint]
  (not (some neg? (vals (merge-with - minerals robot-blueprint)))))

(defn flip-coin [] (< (rand) 0.5))

(defn spend ;; TODO can I build more than one robot at a time?
  [minerals {:keys [ore-robot clay-robot obsidian-robot geode-robot]}]
  (let [[affordable-ore-robot?
         affordable-clay-robot?
         affordable-obsidian-robot?
         affordable-geode-robot?]
        (map #(affordable? minerals %)
             [ore-robot
              clay-robot
              obsidian-robot
              geode-robot])]
    (cond
      affordable-geode-robot?
      {:robots {:geode 1}
       :spent (update-vals geode-robot #(* -1 %))}
      (and affordable-obsidian-robot? (flip-coin))
      {:robots {:obsidian 1}
       :spent (update-vals obsidian-robot #(* -1 %))}
      (and affordable-clay-robot? (flip-coin))
      {:robots {:clay 1}
       :spent (update-vals clay-robot #(* -1 %))}
      (and affordable-ore-robot? (flip-coin))
      {:robots {:ore 1}
       :spent (update-vals ore-robot #(* -1 %))}
      :else
      {:robots {}
       :spent {}})))

(defn simulate [time-limit blueprint]
  (loop [countdown time-limit
         robots {:ore 1
                 :clay 0
                 :obsidian 0
                 :geode 0}
         minerals {:ore 0
                   :clay 0
                   :obsidian 0
                   :geode 0}]
    (if (zero? countdown)
      (:geode minerals)
      (let [collected           robots
            {new-robots :robots
             spent      :spent} (spend minerals blueprint)
            total-robots        (merge-with + robots new-robots)]
        (recur (dec countdown)
               total-robots
               (merge-with + minerals collected spent))))))

(defn calculate-quality-level [time-limit {:keys [id] :as blueprint}]
  (let [simulation-count 60000 ;; just an arbitrary number. Balance run-time and chance to find the global maximum.
                              ;; some kind of intelligent gradient descent to move to better and better buying strategies
        production (transduce (map #(simulate time-limit %))
                              max
                              0
                              (repeat simulation-count blueprint))]
    (* id production)))

;; solution part 1

#_(time ;;1418
 (let [time-limit 24]
   (with-open [rdr (io/reader "src/day19/input.txt")]
     (->> rdr
          line-seq
          (transduce (comp (map parse-input)
                           (map #(calculate-quality-level time-limit %)))
                     +
                     0)))))

;; solution part 2

(defn spend'
  [minerals {:keys [ore-robot clay-robot obsidian-robot geode-robot]}]
  (let [[affordable-ore-robot?
         affordable-clay-robot?
         affordable-obsidian-robot?
         affordable-geode-robot?]
        (map #(affordable? minerals %)
             [ore-robot
              clay-robot
              obsidian-robot
              geode-robot])]
    (cond
      ;; an idea would be to scale the chance of building a robot inversely with the amount of that robot you have.
      ;; This roughly cooresponds to minting ore robots early, then clay, then obsidan, and then geo but always allowing for some randomness.
      (and (< (rand) 0.9) ;; For part 2 as it looks like over the course of 32 minutes it's sometimes wiser to build an obsidian robot instead of a geode robot.
           affordable-geode-robot?)
      {:robots {:geode 1}
       :spent (update-vals geode-robot #(* -1 %))}
      (and affordable-obsidian-robot? (flip-coin))
      {:robots {:obsidian 1}
       :spent (update-vals obsidian-robot #(* -1 %))}
      (and affordable-clay-robot? (flip-coin))
      {:robots {:clay 1}
       :spent (update-vals clay-robot #(* -1 %))}
      (and affordable-ore-robot? (flip-coin))
      {:robots {:ore 1}
       :spent (update-vals ore-robot #(* -1 %))}
      :else
      {:robots {}
       :spent {}})))

(defn simulate' [time-limit blueprint]
  (loop [countdown time-limit
         robots {:ore 1
                 :clay 0
                 :obsidian 0
                 :geode 0}
         minerals {:ore 0
                   :clay 0
                   :obsidian 0
                   :geode 0}]
    (if (zero? countdown)
      (:geode minerals)
      (let [collected           robots
            {new-robots :robots
             spent      :spent} (spend' minerals blueprint)
            total-robots        (merge-with + robots new-robots)]
        (recur (dec countdown)
               total-robots
               (merge-with + minerals collected spent))))))

(defn calculate-production [time-limit blueprint]
  (let [simulation-count 600000 ;; based on 32 actions with 4 binary choices you have 3.4e38 possibilities, most of which are duds
        production (transduce (map #(simulate' time-limit %))
                              max
                              0
                              (repeat simulation-count blueprint))]
    (* production)))

;; ran for ~30 minutes with 600,000 simulations and didn't get the right answer still...
(time ;;3570 too low
 (let [time-limit 32]
   (with-open [rdr (io/reader "src/day19/input.txt")]
     (->> rdr
          line-seq
          (take 3)
          (map parse-input)
          (map #(calculate-production time-limit %))
          prn
          #_(transduce (comp (map parse-input)
                             (map #(calculate-production time-limit %)))
                       *
                       1)))))
