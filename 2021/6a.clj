(->> (clojure.string/split (slurp "6.txt") #",")
     frequencies
     (reduce-kv (fn [cs i c] (assoc cs (Integer. i) c))
                (apply vector (repeat 9 0)))
     (iterate (fn [[c0 c1 c2 c3 c4 c5 c6 c7 c8]]
                [c1 c2 c3 c4 c5 c6 (+ c0 c7) c8 c0]))
     (drop 80)
     first
     (reduce +))
