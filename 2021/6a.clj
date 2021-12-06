(->> (clojure.string/split (slurp "6.txt") #",")
     frequencies
     (reduce-kv (fn [coll key count]
                  (assoc coll (Integer. key) count))
                (apply vector (repeat 9 0)))
     (iterate (fn [counts]
                (mapv (fn [n]
                        (case n
                          6 (+ (counts 0) (counts 7))
                          8 (counts 0)
                          (counts (inc n))))
                      (range 9))))
     (drop 80)
     first
     (reduce +))
