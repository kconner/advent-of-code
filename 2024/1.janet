(use ./tools)

(def line-grammar
  (let [id ~(capture (some (range "09")))
        spaces ~(some " ")]
    (peg/compile ~(* ,id ,spaces ,id -1))))

# zip of list of number pairs parsed from lines of the file
(defn lists-from-file [path]
  (map array ;(map |(map scan-number (peg/match line-grammar $))
                   (lines-from-file path))))

# sum of absolute differences of pairs across sorted lists
(defn problem1 [lists]
  (+ ;(map |(math/abs (- $0 $1))
           ;(map sort lists))))

# sum of the products of ids with their frequencies in each list
(defn problem2 [lists]
  (let [[freqs1 freqs2] (map frequencies lists)]
    (+ ;(map |(* $ (freqs1 $) (in freqs2 $ 0))
             (keys freqs1)))))

(defn main [&]
  (spork/test/timeit
    (do
      (def path "1.txt")
      # (def path "1.test.txt")
      (def lists (lists-from-file path))
      (print (problem1 lists))
      (print (problem2 lists)))))
