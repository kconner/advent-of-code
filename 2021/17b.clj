(defn target-area [text]
  (->> text
       (re-matches #"target area: x=(-?\d+)..(-?\d+), y=(-?\d+)..(-?\d+)")
       (drop 1)
       (map #(Integer. %))))

(defn hits-target [[mintx maxtx minty maxty] vx vy]
  (loop [x 0 y 0 vx vx vy vy]
    (cond (or (< maxtx x) (< y minty)) false
          (and (<= mintx x maxtx) (<= minty y maxty)) true
          :else (recur (+ x vx) (+ y vy) (max 0 (dec vx)) (dec vy)))))

(time
 (let [[_ maxtx minty _ :as target] (target-area (slurp "17.txt"))]
   (->>
    (pmap (partial apply hits-target target)
          (for [vx (range 1 (inc maxtx))
                vy (range minty (inc (- minty)))]
            [vx vy]))
    (filter identity)
    count)))
