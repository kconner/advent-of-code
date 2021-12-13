(require '[clojure.string :as string])

(defn point-for-line [line] (mapv #(Integer. %) (string/split line #",")))

(defn transform-for-line [line]
  (let [parts (string/split line #" |=")
        axis (case (parts 2) "x" 0 "y" 1)
        value (Integer. (parts 3))]
    (fn [point]
      (if (< (point axis) value)
        point
        (update point axis #(- (+ value value) %))))))

(def paragraphs (string/split (slurp "13.txt") #"\n\n"))

(def transforms (->> (second paragraphs)
                     string/split-lines
                     (map transform-for-line)))

(->> (first paragraphs)
     string/split-lines
     (mapv point-for-line)
     (pmap (first transforms))
     set
     count)
