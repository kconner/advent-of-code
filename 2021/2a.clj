(def lines (clojure.string/split
            (slurp "2.txt")
            #"\n"))

(def steps (map
            (fn [line]
              (let [[direction distance] (clojure.string/split line #"\s")]
                [direction (Integer. distance)]))
            lines))

(def end (reduce
          (fn [[h d] [direction distance]]
            (case direction
              "forward" [(+ h distance) d]
              "up" [h (- d distance)]
              "down" [h (+ d distance)]))
          [0 0]
          steps))

(apply * end)