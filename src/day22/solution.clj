(ns day22.solution
  (:require [clojure.string :as string]))

(def facings
  {:east {"L" :north
          "R" :south
          :move (fn [{:keys [width _height]} position]
                  (update position 1
                          (fn [x] (mod (inc x) width))))}
   :south {"L" :east
           "R" :west
           :move (fn [{:keys [_width height]} position]
                   (update position 0
                           (fn [y]
                             (mod (inc y) height))))}
   :west {"L" :south
          "R" :north
          :move (fn [{:keys [width _height]} position]
                  (update position 1
                          (fn [x] (mod (dec x) width))))}
   :north {"L" :west
           "R" :east
           :move (fn [{:keys [_width height]} position]
                   (update position 0
                           (fn [y] (mod (dec y) height))))}})

(defn step
  [{:keys [chart width height]}
   {:keys [position facing]}
   direction]
  (if (#{"L" "R"} direction)
    {:position position
     :facing (get-in facings [facing direction])}
    (let [move-fn (partial (get-in facings [facing :move]) {:width width
                                                            :height height})]
      (loop [steps direction
             resting position
             position' position]
        (if (zero? steps)
          {:position position'
           :facing facing}
          (let [next (move-fn position')
                terrain (str (get-in chart next))]
            (cond
              (= terrain "#") {:position resting
                               :facing facing}
              (= terrain " ") (recur steps resting next)
              (= terrain ".") (recur (dec steps) next next))))))))

;; solution part 1

(time
 (let [[chart* directions*] (string/split (slurp "src/day22/input.txt") #"\n\n")
       directions (map #(if (#{"L" "R"} %) % (parse-long %)) (re-seq #"\d+|L|R" directions*))
       chart (->> chart*
                  string/split-lines
                  (mapv #(let [length (count %)]
                           (apply str % (repeat (- 150 length) " ")))))
       start [0 (ffirst (filter #(= \. (last %)) (map-indexed (fn [idx char] [idx char]) (first chart))))] ;; [y x]
       facing :east
       width (count (first chart))
       height (count chart)

       {[y x] :position facing :facing}
       (reduce
        (fn [acc direction]
          (step {:chart chart :width width :height height}
                acc
                direction))
        {:position start :facing facing}
        directions)

       row (inc y)
       column (inc x)
       facing-score ({:east 0
                      :south 1
                      :west 2
                      :north 3} facing)]

   (+ (* 1000 row)
      (* 4 column)
      facing-score)))

;; solution part 2

(defn adjacency-chart)

(time
 (let [[chart* directions*] (string/split (slurp "src/day22/input.txt") #"\n\n")
       directions (map #(if (#{"L" "R"} %) % (parse-long %)) (re-seq #"\d+|L|R" directions*))
       chart (->> chart*
                  string/split-lines
                  (mapv #(let [length (count %)]
                           (apply str % (repeat (- 150 length) " ")))))
       start [0 (ffirst (filter #(= \. (last %)) (map-indexed (fn [idx char] [idx char]) (first chart))))] ;; [y x]
       facing :east
       width (count (first chart))
       height (count chart)

       {[y x] :position facing :facing}
       {:position [0 0] :facing :east}
       #_(reduce
        (fn [acc direction]
          (step {:chart chart :width width :height height}
                acc
                direction))
        {:position start :facing facing}
        directions)

       row (inc y)
       column (inc x)
       facing-score ({:east 0
                      :south 1
                      :west 2
                      :north 3} facing)]
(doseq [x chart] (println x))
   (+ (* 1000 row)
      (* 4 column)
      facing-score)))
 