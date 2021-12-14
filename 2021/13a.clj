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

; The total fold operation is the fold function for the first line
; of the second paragraph.
(def folds
  (->> (second paragraphs)
       string/split-lines
       first
       fold-for-line))

; The folded points are the unique results of the fold operation
; on each point given lines of the first paragraph.
(def folded-points
  (->> (first paragraphs)
       string/split-lines
       (map (comp folds point-for-line))
       set))

; Count em.
(count folded-points)