(require '[clojure.string :as string])

(def place-scores [10 1 2 3 4 5 6 7 8 9])

(def roll-sum-frequencies
  (->> (let [rolls (range 1 4)] (for [a rolls b rolls c rolls] [a b c]))
       (map (partial apply +))
       frequencies))

(defn search [universes places scores player depth]
  (->> roll-sum-frequencies
       ((if (<= depth 2) pmap map)
        (fn [[roll-sum frequency]]
          (let [universes (* universes frequency)
                place (mod (+ (places player) roll-sum) 10)
                score (+ (scores player) (place-scores place))]
            (if (<= 21 score)
              {player universes}
              (search universes
                      (assoc places player place)
                      (assoc scores player score)
                      (- 1 player)
                      (inc depth))))))
       (reduce (partial merge-with +))))

(time (let [places (mapv (fn [line] (Integer. (last (string/split line #" "))))
                         (string/split-lines (slurp "21.txt")))]
        (apply max (vals (search 1 places [0 0] 0 0)))))
