(use ./tools)

(defn sum-of-muls [text]
  (+ ;(map |(apply * $) (partition 2 text))))

(def int-grammar ~(/ (<- (between 1 3 (range "09"))) ,scan-number))
(def mul-grammar ~(* "mul(" ,int-grammar "," ,int-grammar ")"))
(def dont-range-grammar ~(* "don't()" (any (if-not "do()" 1))))

(defn problem1 [text]
  (->> text
       (peg/match ~(any (choice ,mul-grammar 1)))
       (sum-of-muls)))

(defn problem2 [text]
  (->> text
       (peg/match ~(any (choice ,dont-range-grammar ,mul-grammar 1)))
       (sum-of-muls)))

(defn main [&]
  (spork/test/timeit
    (do
      (def path "3.txt")
      # (def path "3.test.txt")
      # (def path "3.test2.txt")
      (def text (slurp path))
      (print (problem1 text))
      (print (problem2 text)))))
