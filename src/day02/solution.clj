(ns day02.solution
  (:require [clojure.java.io :as io]
            [clojure.set :as set]))

(def ^:private ->shape
  {\A :rock
   \B :paper
   \C :scissors
   \X :rock
   \Y :paper
   \Z :scissors})

(def ^:private beats
  {:rock :scissors
   :paper :rock
   :scissors :paper})

(def ^:private shape->points
  {:rock 1
   :paper 2
   :scissors 3})

(def ^:private outcome->points
  {:lose 0
   :draw 3
   :win 6})

(defn play-round [moves]
  (let [[opponent-shape _ your-shape] (map ->shape moves)
        your-outcome (cond (= opponent-shape your-shape)         :draw
                           (= opponent-shape (beats your-shape)) :win
                           :else                                 :lose)]
    (+ (outcome->points your-outcome)
       (shape->points your-shape))))


;; solution part 1

(time
 (with-open [rdr (io/reader "src/day02/input.txt")]
   (->> rdr
        line-seq
        (map play-round)
        (apply +))))

;; solution part 2

(def ^:private ->outcome
  {\X :lose
   \Y :draw
   \Z :win})

(def ^:private outcome->shape-fn
  ;; the win and loss is inverted from your expectations
  ;; because an opponent's win is your loss.
  {:lose beats
   :draw identity
   :win (set/map-invert beats)})

(defn play-round* [[opponent-symbol _ your-symbol]]
  (let [[opponent-shape _ your-outcome] [(->shape opponent-symbol) _ (->outcome your-symbol)]
        your-shape ((outcome->shape-fn your-outcome) opponent-shape)]
    (+ (outcome->points your-outcome)
       (shape->points your-shape))))

(time
 (with-open [rdr (io/reader "src/day02/input.txt")]
   (->> rdr
        line-seq
        (map play-round*)
        (apply +))))
