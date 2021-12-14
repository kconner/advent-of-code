(require '[clojure.string :as string])

; A point is the integer values of the text on either side of its line's comma.
(defn point-for-line [line]
  (mapv #(Integer. %) (string/split line #",")))

; A fold function flips points around a given line's vertex on its stated axis.
(defn fold-for-line [line]
  (let [parts (string/split line #" |=")
        axis (case (parts 2) "x" 0 "y" 1)
        vertex (Integer. (parts 3))
        doubled-vertex (+ vertex vertex)]
    (fn [point]
      (update point axis #(if (< % vertex) % (- doubled-vertex %))))))

; The paragraphs are the file's text split by double newlines.
(def paragraphs
  (string/split (slurp "13.txt") #"\n\n"))

; The total fold operation is the composition of the fold functions
; for each line of the second paragraph.
(def folds
  (->> (second paragraphs)
       string/split-lines
       reverse
       (map fold-for-line)
       (apply comp)))

; The folded points are the unique results of the fold operation
; on each point given lines of the first paragraph.
(def folded-points
  (->> (first paragraphs)
       string/split-lines
       (map (comp folds point-for-line))
       set))

; The dimensions are the inclusive ranges from 0 to each
; maximum value among all points, per axis.
(def dimensions
  (mapv (comp range inc)
        (reduce (partial mapv max) folded-points)))

; The lines are joined strings per Y, which are joined characters per X,
; in which the character is visible when the point is among the folded points.
(def lines
  (for [y (second dimensions)]
    (string/join
     (for [x (first dimensions)]
       (if (folded-points [x y]) \# \space)))))

; Print em.
(println (string/join \newline lines))