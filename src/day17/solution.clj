(ns day17.solution
  (:require [clojure.string :as string]))

(def rocks
  (cycle
   (list
    '("|..@@@@.|")
    '("|...@...|" "|..@@@..|" "|...@...|")
    '("|....@..|" "|....@..|" "|..@@@..|")
    '("|..@....|" "|..@....|" "|..@....|" "|..@....|")
    '("|..@@...|" "|..@@...|"))))

(defn display! [cave]
  (doseq [layer cave]
    (println layer))
  (println))

(def immoveable-piece? #{\# \| \+ \-})
(def moveable-piece? #{\@})
(def air? #{\.})
(def contains-moveable-rock? #(some moveable-piece? %))

(def jet->fn
  {\>
   (fn [layers]
     (let [grouped-layers (partition-by contains-moveable-rock? layers)
           [above-immovable-layers moveable-layers below-immovable-layers]
           (if (= 3 (count grouped-layers))
             grouped-layers
             ;; there might not be any layers above the moving rock
             (into ['()] grouped-layers))]
       (loop [[layer & remaining] moveable-layers
              acc (transient [])]
         (let [[l-edge a b c d e f g r-edge] layer]
           (cond
             ;; reached the last moveable layer
             (nil? layer) (concat above-immovable-layers (persistent! acc) below-immovable-layers)
             ;; hit a wall
             (= g \@) layers
             ;; hit a rock
             (or (and (= f \@) (= g \#))
                 (and (= e \@) (= f \#))
                 (and (= d \@) (= e \#))
                 (and (= c \@) (= d \#))
                 (and (= b \@) (= c \#))
                 (and (= a \@) (= b \#))) layers
             ;; move just the moveable rock
             :else
             (let [moved-layer (str l-edge
                                    (apply str
                                           (map (fn [pre post]
                                                  (cond (immoveable-piece? pre) pre
                                                        (moveable-piece? post)  post
                                                        :else \.))
                                                [a  b c d e f  g]
                                                [\. a b c d e f]))
                                    r-edge)]
               (recur remaining (conj! acc moved-layer))))))))
   \<
   (fn [layers]
     (let [grouped-layers (partition-by contains-moveable-rock? layers)
           [above-immovable-layers moveable-layers below-immovable-layers]
           (if (= 3 (count grouped-layers))
             grouped-layers
             ;; there might not be any layers above the moving rock
             (into ['()] grouped-layers))]
       (loop [[layer & remaining] moveable-layers
              acc (transient [])]
         (let [[l-edge a b c d e f g r-edge] layer]
           (cond
             ;; reached the last moveable layer
             (nil? layer) (concat above-immovable-layers (persistent! acc) below-immovable-layers)
             ;; hit a wall
             (= a \@) layers
             ;; hit a rock
             (or (and (= f \#) (= g \@))
                 (and (= e \#) (= f \@))
                 (and (= d \#) (= e \@))
                 (and (= c \#) (= d \@))
                 (and (= b \#) (= c \@))
                 (and (= a \#) (= b \@))) layers
             ;; move just the moveable rock
             :else
             (let [moved-layer (str l-edge
                                    (apply str
                                           (map (fn [pre post]
                                                  (cond (immoveable-piece? pre) pre
                                                        (moveable-piece? post)  post
                                                        :else \.))
                                                [a b c d e f  g]
                                                [b c d e f g \.]))
                                    r-edge)]
               (recur remaining (conj! acc moved-layer))))))))})

(defn settle [cave]
  (map #(string/replace % "@" "#") cave))

(defn settled? [cave]
  (boolean (not (some contains-moveable-rock? cave))))

(defn fall [cave]
  (if (settled? cave)
    cave
    (let [[above-immovable-layers
           moveable-layers
           below-immovable-layers] (->> (conj cave "|.......|" #_top-layer)
                                      ;; split at index would make more sense here 
                                      ;; since once we don't see any moveable rock we're done checking
                                      ;; but i'm already using a '() which doesn't index so...
                                        (partition-by contains-moveable-rock?))

          moving-layers (partition 2 1 (concat
                                        (list (last above-immovable-layers))
                                        moveable-layers
                                        (list (first below-immovable-layers))))

          moved-layers (loop [[[above-layer below-layer] & layers] moving-layers
                              fallen '()]
                         (let [matching-pieces (map vector above-layer below-layer)
                               next-pieces (->> matching-pieces
                                                (map (fn [[above below]]
                                                       (cond
                                                         (and (moveable-piece? above)
                                                              (immoveable-piece? below))
                                                         :abort
                                                         (and (moveable-piece? above)
                                                              (air? below))
                                                         \@
                                                         (and (air? above)
                                                              (moveable-piece? below))
                                                         \.
                                                         :else
                                                         below))))
                               next-layer (apply str next-pieces)]
                           (cond
                             (some #{:abort} next-pieces) (settle (concat moveable-layers (list (first below-immovable-layers))))
                             (nil? layers) (reverse (conj fallen next-layer))
                             :else (recur layers (conj fallen next-layer)))))]
      (drop-while
       #{"|.......|"}
       (concat above-immovable-layers
               moved-layers
               (rest below-immovable-layers))))))

;; solution part 1

;; 3202 is the correct answer for my input. I have a bug somewhere clearly as I get too large of an answer.
;; some block must not end up in its proper place but it's very difficult to find where

;; a big optimization would be to collapse the cave from the floor up until
;; a layer is actually reachable. if a layer looks like |#######| for example
;; everything below it is unreachable. So you can collapse that down into its height
;; and continue with the rest of the falling blocks

;; another big optimization would be to memoize the push and fall fns given that
;; there are only 7 places with 3 options per place \. \@ \#. Covering every case over the course of the program likely happens very quickly.
(time
 (let [jet-pattern (cycle (string/trim (slurp "src/day17/input.txt")))
       end-count 2022
       padding '("|.......|" "|.......|" "|.......|")
       floor '("+-------+")
       final-cave
       (loop [cave floor
              rock-count 0
              [next-rock & remaining-rocks :as rocks] rocks
              [jet & remaining-jets :as jets] jet-pattern]
         (cond
           (= (inc end-count) rock-count)
           cave

           (settled? cave)
           (do (println (->> cave (drop-while #(not (some #{\#} %))) ;; remove layers until you hit the top of the structure
                             count
                             (+ -1)))
             (recur (concat next-rock padding cave)
                    (inc rock-count)
                    remaining-rocks
                    jets))

           :else
           (let [push (jet->fn jet)]
             (recur (fall (push cave))
                    rock-count
                    rocks
                    remaining-jets))))]
   (->> final-cave
        (drop-while #(not (some #{\#} %))) ;; remove layers until you hit the top of the structure
        count
        (+ -1) ;; remove floor from the count
        #_display!)))

;; solution part 2 NONE :'(
;; answer is 1591977077352

(comment
  (defn n-times [n f]
    (apply comp (repeat n f)))

  (display!
   ((n-times 2 (jet->fn \>))
    '("|.....#.|"
      "|....@..|"
      "|....@..|"
      "|..@@@..|"
      "|.....#.|"
      "|.....#.|")))

  (display!
   ((jet->fn \<)
    '("|....@..|"
      "|....@..|"
      "|..@@@..|"
      "|...#...|"
      "|..###..|"
      "|...#...|"
      "|..####.|"
      "+-------+")))

  (display!
   (settle '("|....@..|"
             "|....@..|"
             "|..@@@..|"
             "|.......|"
             "|.....#.|")))

  (display! #_settled?
   ((n-times 10 fall)
    '("|.......|"
      "|...@#..|"
      "|..@@...|"
      "|.......|"
      "|....#..|"
      "|....#..|"
      "|....##.|"
      "|....##.|"
      "|.#.....|"
      "|.####..|"
      "+-------+")))

  (display!
   ((n-times 13 fall)
    '("|@......|"
      "|@......|"
      "|@......|"
      "|@......|"
      "|.......|"
      "|.......|"
      "|.......|"
      "|..#####|"
      "|.#.....|"
      "|.#.....|"
      "|.#.....|"
      "|.######|"
      "|.######|"
      "|.######|"
      "|.######|"
      "|.######|"
      "+-------+")))
  (display!
   ((n-times 13 fall)
    '("|.......|"
      "|..@....|"
      "|..@....|"
      "|..@....|"
      "|.......|"
      "|.......|"
      "|.......|"
      "|...####|"
      "|##.#...|"
      "|.#.....|"
      "|.#.....|"
      "|.######|"
      "|.######|"
      "|.######|"
      "|.######|"
      "|.######|"
      "+-------+")))

  (def heights
    (map parse-long (string/split-lines (slurp "src/day17/heights.txt"))))
  (def my-heights
    (map parse-long (string/split-lines (slurp "src/day17/my_heights.txt"))))
  
  ;; tried to debug here but my output matches every single
  ;; line of the sample input correctly so it isn't very helpful
  (->> (map = heights my-heights)
       (map-indexed (fn [idx x] [idx x]))
       (remove (fn [[idx x]] (= true x))))
  )
