(ns day10.solution 
  (:require [clojure.java.io :as io]))

(defn parse-instruction [s]
  (if (= "noop" s)
    {:cycles 1
     :n 0}
    {:cycles 2
     :n (parse-long (. s substring 5))}))

(defn synchronize [instructions]
  (mapcat (fn [{:keys [cycles n]}]
            (if (= cycles 1)
              [n]
              [0 n]))
          instructions))

;; solution part 1

(time
 (with-open [rdr (io/reader "src/day10/input.txt")]
   (let [register-history
         (->> rdr
              line-seq
              (map parse-instruction)
              synchronize
              (reduce (fn [history n]
                        (let [register (peek history)]
                          (conj history (+ register n))))
                      [1 1]))
         key-cycles '(20 60 100 140 180 220)]
     (reduce (fn [acc cycle]
               (+ acc (* cycle
                         (get register-history cycle))))
             0
             key-cycles))))

;; solution part 2

(defn sprite-position [register-value]
  (let [pixels ((juxt dec identity inc) register-value)]
    (set pixels)))

(defn display! [stream]
  (doseq [line (partition 40 stream)]
    (println (apply str line))))

(time
 (with-open [rdr (io/reader "src/day10/input.txt")]
   (let [sprite-position-history
         (->> rdr
              line-seq
              (map parse-instruction)
              synchronize
              (reduce (fn [history n]
                        (let [register (peek history)]
                          (conj history (+ register n))))
                      [1 1])
              (mapv sprite-position))

         scan-position-history
         (-> (for [cycle (range 240)
                   :let [scan-position (mod cycle 40)]]
               scan-position)
             (conj 0) ;; init for cycle 0
             vec)

         display-data
         (rest ;; drop cycle 0
          (map (fn [scan sprite]
                 (if (contains? sprite scan)
                   "#"
                   "."))
               scan-position-history
               sprite-position-history))]
     (display! display-data))))
