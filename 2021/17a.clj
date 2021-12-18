(defn target-area [text]
  (->> text
       (re-matches #"target area: x=(-?\d+)..(-?\d+), y=(-?\d+)..(-?\d+)")
       (drop 1)
       (map #(Integer. %))))

(time
 (let [[_ _ minty _] (target-area (slurp "17.txt"))]
   (reduce + (range (- minty)))))
