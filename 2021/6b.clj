(->> (clojure.string/split (slurp "6.txt") #",")
     frequencies
     (reduce-kv (fn [c k v] (assoc c (Integer. k) v))
                (vec (repeat 9 0)))
     (iterate (fn [[a b c d e f g h i]]
                [b c d e f g (+ h a) i a]))
     (drop 256)
     first
     (reduce +))
