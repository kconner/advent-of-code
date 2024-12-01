(import ./tools :only [lines-from-file])

# parsing

(def line-grammar
  (let [id ~(capture (some (range "09")))
        spaces ~(some " ")]
    (peg/compile ~(* ,id ,spaces ,id))))

(defn ids-in-line [line]
  (map scan-number (peg/match line-grammar line)))

(defn lists-from-file [path]
  (map array ;(map ids-in-line (tools/lines-from-file path))))

# problem 1

(defn difference [a b]
  (math/abs (- a b)))

(defn problem1 [lists]
  (+ ;(map difference ;(map sort lists))))

# problem 2

(defn frequency-product-sum [freqs1 freqs2]
  (defn frequency-product [k]
    (* k (get freqs1 k) (get freqs2 k 0)))
  (+ ;(map frequency-product (keys freqs1))))

(defn problem2 [lists]
  (frequency-product-sum ;(map frequencies lists)))

# main

(defn main [&]
  # (def input-path "1.test.txt")
  (def input-path "1.txt")
  (def lists (lists-from-file input-path))
  (print (problem1 lists))
  (print (problem2 lists)))
