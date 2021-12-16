(require '[clojure.string :as string])

; A cell's neighbors are a list of cardinally adjacent coordinates.

(defn neighbors [[r c]]
  (list [r (inc c)] [(inc r) c] [r (dec c)] [(dec r) c]))

; A cell list's unscored neighbors are a list of unique neighbors of the cells
; which are among the set of unscored cells.

(defn unscored-neighbors [unscored-cells cells]
  (filter unscored-cells (set (mapcat neighbors cells))))

; After adding a new cell, the cells by score map is a mutated copy where the
; set of cells keyed by the cell's score, which is the sum of the accumulated
; path score and the cell's value, also includes the cell.

(defn add-cell-by-score [rows score cells-by-score cell]
  (update cells-by-score
          (+ score (get-in rows cell))
          (fnil conj #{})
          cell))

; The path score is the accumulated score when the last cell is found in the
; set of cells for the accumulated score. Until that happens, the map of sets
; of cells by score is evolved by removing the set of cells for the accumulated
; score and adding cells by score for each their cells' adjacent unscored cells.
; This process is effectively a weighted flood fill.

(defn path-score [rows]
  (let [rs (count rows)
        cs (count (first rows))
        last-cell [(dec rs) (dec cs)]]
    (loop [unscored-cells (disj (set (for [r (range rs)
                                           c (range cs)] [r c])) [0 0])
           cells-by-score {0 #{[0 0]}}
           score 0]
      (let [cells (cells-by-score score)]
        (cond (get cells last-cell) score
              (empty? cells) (recur unscored-cells cells-by-score (inc score))
              :else (let [neighbors (unscored-neighbors unscored-cells cells)]
                      (recur (apply disj unscored-cells neighbors)
                             (reduce (partial add-cell-by-score rows score)
                                     (dissoc cells-by-score score)
                                     neighbors)
                             (inc score))))))))

; An incremented cell value is the incremented remainder having divided by 9.

(defn increment-cell [cell]
  (inc (mod cell 9)))

; An expanded row is the vector of the concatenated first five incrementing
; copies of the row.

(defn expand-row [row]
  (->> row
       (iterate (partial map increment-cell))
       (take 5)
       (apply concat)
       vec))

; An expanded list of rows is the vector of the expansion of each row of the
; concatenated first five incrementing copies of all rows.

(defn expand-rows [rows]
  (->> rows
       (iterate (partial map (partial map increment-cell)))
       (take 5)
       (apply concat)
       (mapv expand-row)))

; The answer is the path score for the expanded vector of vectors of integers
; parsed from characters of lines of text. 

(time (->> (slurp "15.txt")
           string/split-lines
           (mapv (partial mapv #(Integer. (str %))))
           expand-rows
           path-score))
