(require '[clojure.string :as string])

(defn point-for-line [line]
  (mapv #(Integer. %) (string/split line #",")))

(defn fold-for-line [line]
  (let [parts (string/split line #" |=")
        axis (case (parts 2) "x" 0 "y" 1)
        vertex (Integer. (parts 3))
        doubled-vertex (+ vertex vertex)]
    (fn [point]
      (update point axis #(if (< % vertex) % (- doubled-vertex %))))))

(def text
  (string/split (slurp "13.txt") #"\n\n"))

(def folds
  (->> (second text)
       string/split-lines
       reverse
       (map fold-for-line)
       (apply comp)))

(def folded-points
  (->> (first text)
       string/split-lines
       (map (comp folds point-for-line))
       set))

(def dimensions
  (reduce (partial mapv max) folded-points))

(def lines
  (for [r (range (inc (second dimensions)))]
    (string/join
     (for [c (range (inc (first dimensions)))]
       (if (folded-points [c r]) \# \space)))))

(println (string/join \newline lines))