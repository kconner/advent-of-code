(def lines (clojure.string/split (slurp "1.txt") #"\n"))

(def numbers (map #(Integer. %) lines))

(def sums (map + numbers (drop 1 numbers) (drop 2 numbers)))

(def increasing-pairs (filter #(apply < %) (map list sums (drop 1 sums))))

(count increasing-pairs)
