# utility

(defn not-empty? [s]
  (not (empty? s)))

# parsing

(def line-grammar
  (do
    (def location-id ~(some (range "09")))
    (def spaces ~(some " "))
    (def line ~(sequence (capture ,location-id)
                        ,spaces
                        (capture ,location-id)))
    (peg/compile line)))

(defn numbers-in-line [text]
  (->> text
    (peg/match line-grammar)
    (map scan-number)))

(def input-path "1.txt")

# problem 1

(defn problem1 []
  (->> input-path
    (slurp)
    (string/split "\n")
    (filter not-empty?)
    (map numbers-in-line)
    (apply map array)
    (map sort)
    (apply map -)
    (map math/abs)
    (apply +)))
    
(print (problem1))

# problem 2