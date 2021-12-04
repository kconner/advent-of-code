(def lines (clojure.string/split
            (slurp "2.txt")
            #"\n"))

(def steps (map
            (fn [line]
              (let [[direction distance] (clojure.string/split line #"\s")]
                [direction (Integer. distance)]))
            lines))

(def end (reduce
          (fn [[h d a] [direction distance]]
            (case direction
              "forward" [(+ h distance) (+ d (* a distance)) a]
              "up" [h d (- a distance)]
              "down" [h d (+ a distance)]))
          [0 0 0]
          steps))

(* (end 0) (end 1))