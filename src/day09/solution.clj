(ns day09.solution
  (:require [clojure.java.io :as io]))

(def direction->fn
  {:R #(update % 0 inc)
   :U #(update % 1 inc)
   :L #(update % 0 dec)
   :D #(update % 1 dec)})

(defn nudge [n]
  (cond (zero? n)  0
        (pos?  n)  1
        (neg?  n) -1))

(defn catch-up [head-position tail-position]
  (let [[dx dy] (map - head-position tail-position)]
    ;; if on top of eachother or head immediately adjacent to tail
    ;; then no movement.
    ;; else nudge the position towards the head
    (if (and (< (abs dx) 2)
             (< (abs dy) 2))
      tail-position
      (mapv + tail-position [(nudge dx) (nudge dy)]))))

(defn- follow
  "Produces the trajectory of a tail starting at `tail-position` following head along `head-trajectory`."
  [head-trajectory tail-position]
  (->> head-trajectory
       (reduce (fn [acc head-position]
                 (let [tail-position (peek acc)
                       tail-position' (catch-up head-position tail-position)]
                   (conj acc tail-position')))
               [tail-position])
       rest))

(defn- execute-move
  [{:keys [head-positions
           tail-positions]
    :as _visited}
   [direction n]]
  (let [head-position (peek head-positions)
        tail-position (peek tail-positions)
        head-trajectory (->> head-position
                             (iterate (direction->fn direction))
                             (take (inc n)))
        tail-trajectory (follow head-trajectory tail-position)]
    {:head-positions (apply conj head-positions head-trajectory)
     :tail-positions (apply conj tail-positions tail-trajectory)}))

;; solution part 1

(time
 (with-open [rdr (io/reader "src/day09/input.txt")]
   (->> rdr
        line-seq
        (map (fn [s]
               (let [[_ direction n] (re-find #"([LRUD]) (\d+)" s)]
                 [(keyword direction) (parse-long n)])))
        (reduce execute-move {:head-positions '([0 0])
                              :tail-positions '([0 0])})
        :tail-positions
        set
        count)))

;; solution part 2

(defn- execute-move'
  [[head-positions & tails]
   [direction n]]
  (let [head-position (peek head-positions)
        head-trajectory (->> head-position
                             (iterate (direction->fn direction))
                             (take (inc n)))
        tails-trajectories (->> tails
                                (reduce
                                 (fn [[leader-trajectory :as trajectories]
                                      [follower-position _]]
                                   (let [trajectory' (follow leader-trajectory follower-position)]
                                     (conj trajectories trajectory')))
                                 (list head-trajectory))
                                reverse
                                rest ;; drop the init head-trajectory 
                                )]
    ;; compile all the trajectories into a list in the proper order
    (cons (apply conj head-positions head-trajectory)
          (map (fn [tail-positions tail-trajectory]
                 (apply conj tail-positions tail-trajectory))
               tails
               tails-trajectories))))

(time
 (with-open [rdr (io/reader "src/day09/input.txt")]
   (->> rdr
        line-seq
        (map (fn [s]
               (let [[_ direction n] (re-find #"([LRUD]) (\d+)" s)]
                 [(keyword direction) (parse-long n)])))
        (reduce execute-move' (repeat 10 '([0 0])))
        last ;;last tail
        set
        count)))

;; BONUS

(comment
  (defn visualize
    "Can be used to visualize the path of an segement of the rope or even the position of the rope itself! HINT (map first trajectories)"
    [points]
    (let [right-bound (transduce (map first) max 0 points)
          left-bound (transduce (map first) min 0 points)
          top-bound (transduce (map last) max 0 points)
          bottom-bound (transduce (map last) min 0 points)]
      (doseq [y (reverse (range bottom-bound (inc top-bound)))]
        (->> (range left-bound (inc right-bound))
             (map (fn [x] (cond
                            (= [0 0] [x y])           "s"
                            (contains? points [x y])  "#"
                            :else                     ".")))
             (apply str)
             println))))

  (visualize #{[4 3] [2 2] [0 0]
               [1 0] [3 3] [3 4]
               [4 2] [3 0] [4 1]
               [2 4] [2 0] [1 2]
               [3 2]})

  (visualize #{[4 3] [2 2] [0 0]
               [1 0] [3 3] [3 4]
               [3 0] [2 4] [2 0]
               [1 2] [3 2]}))

