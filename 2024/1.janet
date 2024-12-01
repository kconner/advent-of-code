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

(defn lists-from-file [path]
  (->> path
    (slurp)
    (string/split "\n")
    (filter not-empty?)
    (map numbers-in-line)
    (apply map array)))

# problem 1

(defn problem1 [path]
  (->> path
    (lists-from-file)
    (map sort)
    (apply map -)
    (map math/abs)
    (apply +)))

# problem 2

(defn kinda-like-a-dot-product [freqs1 freqs2]
  (->> freqs1
    (keys)
    (map
      (fn [k]
        (* k
          (get freqs1 k)
          (get freqs2 k 0))))
    (apply +)
  )
)

(defn problem2 [path]
  (->> path
    (lists-from-file)
    (map frequencies)
    (apply kinda-like-a-dot-product)))

# main 

# (def input-path "1.test.txt")
(def input-path "1.txt")
(print (problem1 input-path))
(print (problem2 input-path))
