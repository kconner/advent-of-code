(require '[clojure.string :as string])

(def place-scores [10 1 2 3 4 5 6 7 8 9])

(defn take-turn [[universes whose-turn player-states] roll-sum frequency]
  [(* universes frequency)
   (- 1 whose-turn)
   (update player-states whose-turn
           (fn [[score place]]
             (let [place (mod (+ place roll-sum) 10)]
               [(+ score (place-scores place)) place])))])

(def roll-sum-frequencies
  (->> (let [rolls (range 1 4)]
         (for [a rolls b rolls c rolls] [a b c]))
       (map (partial apply +))
       frequencies))

(defn search [game depth]
  (->> roll-sum-frequencies
       ((if (<= depth 2) pmap map)
        (fn [[roll-sum frequency]]
          (let [[universes _ [[score-0 _] [score-1 _]] :as game]
                (take-turn game roll-sum frequency)]
            (cond (<= 21 score-0) [universes 0]
                  (<= 21 score-1) [0 universes]
                  :else (search game (inc depth))))))
       (reduce (fn [[sv0 sv1] [v0 v1]] [(+ sv0 v0) (+ sv1 v1)]))))

(time (apply
       max
       (search
        [1 0 (mapv (fn [line]
                     [0 (Integer. (last (string/split line #" ")))])
                   (string/split-lines (slurp "21.txt")))]
        0)))
