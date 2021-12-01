(def lines (clojure.string/split (slurp "1.txt") #"\n"))

(def numbers (map #(Integer. %) lines))

(def increasing-pairs (filter #(apply < %) (map vector numbers (drop 1 numbers))))

(count increasing-pairs)
