(def paragraphs
  (clojure.string/split (slurp "4.txt") #"\s*\n\n\s*"))

(def draws
  (map #(Integer. %)
       (clojure.string/split (first paragraphs) #",")))

(defn square-from-string [string]
  {:value (Integer. string)})

(defn row-from-line [line]
  (into [] (map square-from-string)
        (clojure.string/split line #"\s+")))

(defn board-from-paragraph [paragraph]
  (into [] (map row-from-line)
        (clojure.string/split paragraph #"\s*\n\s*")))

(def boards
  (into [] (map board-from-paragraph)
        (rest paragraphs)))

(def indices ((comp range count first) boards))

(defn has-board-won-by-axis [board make-key-path]
  (some (fn [major]
          (every? (fn [minor]
                    (get-in board (conj (make-key-path major minor) :marked)))
                  indices))
        indices))

(defn has-board-won [board]
  (or (has-board-won-by-axis board #(vector %1 %2))
      (has-board-won-by-axis board #(vector %2 %1))))

(defn boards-after-draw [boards draw]
  (clojure.walk/postwalk
   (fn [item]
     (if (= (:value item) draw)
       (assoc item :marked true)
       item))
   boards))

(defn find-winner [boards prior-draw draws]
  (if (empty? draws) :no-winner
      (if-let [winning-board
               (some (fn [board] (and (has-board-won board) board)) boards)]
        {:board winning-board, :final-draw prior-draw}
        (find-winner
         (boards-after-draw boards (first draws))
         (first draws)
         (rest draws)))))

(defn find-last-winner [boards prior-draw draws]
  (cond (empty? draws) :no-winner
        (= 1 (count boards)) (find-winner boards prior-draw draws)
        :else (find-last-winner
               (boards-after-draw (remove has-board-won boards) (first draws))
               (first draws)
               (rest draws))))

(defn score-for-winner [winner]
  (* (:final-draw winner)
     (->> (:board winner)
          (apply concat)
          (remove :marked)
          (map :value)
          (reduce +))))

(score-for-winner (find-last-winner boards nil draws))
