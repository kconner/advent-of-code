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

(def win-at 21)

(defn take-turns [[victories-0 victories-1 games]]
  (let [games (for [game games
                    [roll-sum frequency] roll-sum-frequencies]
                (take-turn game roll-sum frequency))
        {new-victories-0 true games false}
        (group-by (fn [[_ _ [[score-0 _] [_ _]]]] (<= win-at score-0)) games)
        {new-victories-1 true games false}
        (group-by (fn [[_ _ [[_ _] [score-1 _]]]] (<= win-at score-1)) games)]
    [(apply + victories-0 (map first new-victories-0))
     (apply + victories-1 (map first new-victories-1))
     games]))

(def -initial-game [1 0 (mapv (fn [line]
                                [0 (Integer. (last (string/split line #" ")))])
                              (string/split-lines (slurp "21.txt")))])
(def -initial-state [0 0 [-initial-game]])

(take-turns -initial-state)

(time (subvec (first (drop 100 (iterate (fn [state] (time (take-turns state))) -initial-state))) 0 2))

; this is taking way too long to run.
; step 9 only took twice as long as step 8, but it used 7GB by then.
; i could pmap on the roll sum frequencies to partition the games.
; i could try DFS with frequencies.
