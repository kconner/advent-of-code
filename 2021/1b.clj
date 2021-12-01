(def lines (clojure.string/split (slurp "1.txt") #"\n"))

(def numbers (map #(Integer. %) lines))

(def triples (map vector numbers (drop 1 numbers) (drop 2 numbers)))

(def sums (map #(apply + %) triples))

(def pairs-of-sums (map vector sums (drop 1 sums)))

(def increasing-pairs-of-sums (filter #(apply < %) pairs-of-sums))

(count increasing-pairs-of-sums)
