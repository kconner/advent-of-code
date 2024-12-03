(use ./tools)

(def int-grammar ~(/ (<- (between 1 3 (range "09"))) ,scan-number))
(def mul-grammar ~(/ (* "mul(" ,int-grammar "," ,int-grammar ")") ,*))
(def dont-range-grammar ~(* "don't()" (any (if-not "do()" 1))))

(defn sum-of-muls [text & extra-grammars]
  (+ ;(peg/match ~(any (choice ,;extra-grammars ,mul-grammar 1)) text)))

(defn problem1 [text]
  (sum-of-muls text))

(defn problem2 [text]
  (sum-of-muls text dont-range-grammar))

(defn main [&]
  (spork/test/timeit
    (do
      (def path "3.txt")
      # (def path "3.test.txt")
      # (def path "3.test2.txt")
      (def text (slurp path))
      (print (problem1 text))
      (print (problem2 text)))))
