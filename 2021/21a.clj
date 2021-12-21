(require '[clojure.string :as string])

(def place-scores [10 1 2 3 4 5 6 7 8 9])

(defn take-turn [[rolls whose-turn player-states] roll-sum]
  [(+ rolls 3)
   (- 1 whose-turn)
   (update player-states whose-turn
           (fn [[score place]]
             (let [place (mod (+ place roll-sum) 10)]
               [(+ score (place-scores place)) place])))])

(time (some (fn [[rolls _ [[score-0 _] [score-1 _]]]]
              (cond (<= 1000 score-0) (* score-1 rolls)
                    (<= 1000 score-1) (* score-0 rolls)
                    :else nil))
            (reductions
             take-turn
             [0 0 (mapv (fn [line]
                          [0 (Integer. (last (string/split line #" ")))])
                        (string/split-lines (slurp "21.txt")))]
             (map (partial apply +) (partition 3 (cycle (range 1 101)))))))
