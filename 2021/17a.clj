(defn target-area [text]
  (->> text
       (re-matches #"target area: x=(-?\d+)..(-?\d+), y=(-?\d+)..(-?\d+)")
       (drop 1)
       (map #(Integer. %))))

(time
 (let [[_ _ minty maxty] (target-area (slurp "17.txt"))]
   (->> (range (- minty) (dec minty) -1)
        (mapcat
         (fn [vy]
           (->> (iterate dec vy)
                (reductions + 0)
                (take-while #(<= minty %))
                (keep (fn [y] (if (<= minty y maxty) vy nil))))))
        first
        inc
        range
        (reduce +))))
