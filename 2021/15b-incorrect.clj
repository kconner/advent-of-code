(require '[clojure.string :as string])

; A cell's score is its value plus the minimum of the scores left and above it. 

(defn cell-score [left [above cell]]
  (+ cell (min left above)))

; A row's scores are a lazy sequence of the scores for each cell computed by
; the score of the prior cell and the score of the same cell in the prior row. 

(defn row-scores [above row]
  (rest (reductions cell-score ##Inf (map list above row))))

; The total path score is the last score in the last row, computed from all
; prior rows, starting with a value above the left corner that neutralizes its
; contribution. 

(defn path-score [rows]
  (last (reduce row-scores
                (conj (repeat ##Inf) (- (ffirst rows)))
                rows)))

; An incremented cell value is the incremented remainder having divided by 9.

(defn increment-cell [cell]
  (inc (mod cell 9)))

; An expanded row is the concatenated first five incrementing copies of the row.

(defn expand-row [row]
  (->> row
       (iterate (partial map increment-cell))
       (take 5)
       (apply concat)))

; An expanded list of rows is the expansion of each row of the concatenated
; first five incrementing copies of all rows.

(defn expand-rows [rows]
  (->> rows
       (iterate (partial map (partial map increment-cell)))
       (take 5)
       (apply concat)
       (map expand-row)))

; The answer is the path score for the lazy expanded sequence of sequences
; of integers parsed from characters of lines of text. 

(->> (slurp "15.txt")
     string/split-lines
     (map (partial map #(Integer. (str %))))
     expand-rows
     path-score)
