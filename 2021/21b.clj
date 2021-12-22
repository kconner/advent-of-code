(require '[clojure.string :as string])

(def place-scores [10 1 2 3 4 5 6 7 8 9])

(defn take-turn [[score place] roll-sum]
  (let [place (mod (+ place roll-sum) 10)]
    [(+ score (place-scores place)) place]))

(def roll-sum-frequencies
  (->> (let [rolls (range 1 4)]
         (for [a rolls b rolls c rolls] [a b c]))
       (map (partial apply +))
       frequencies))

(defn search [universes game whose-turn depth]
  (->> roll-sum-frequencies
       ((if (<= depth 2) pmap map)
        (fn [[roll-sum frequency]]
          (let [universes (* universes frequency)
                [score place :as state] (take-turn (game whose-turn) roll-sum)]
            (if (<= 21 score)
              (if (zero? whose-turn) [universes 0] [0 universes])
              (search universes
                      (assoc game whose-turn state)
                      (- 1 whose-turn)
                      (inc depth))))))
       (reduce (fn [[sv0 sv1] [v0 v1]] [(+ sv0 v0) (+ sv1 v1)]))))

(time (apply
       max
       (search
        1
        (mapv (fn [line]
                [0 (Integer. (last (string/split line #" ")))])
              (string/split-lines (slurp "21.txt")))
        0
        0)))
