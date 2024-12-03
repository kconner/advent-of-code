(use ./tools)

(defn sum-of-muls [grammar text]
  (->> text
       (peg/match grammar)
       (map scan-number)
       (partition 2)
       (map |(apply * $))
       (apply +)))

(def mul-grammar ~{:main (* "mul(" :int "," :int ")")
                   :int (<- (between 1 3 (range "09")))})

(defn problem1 [text]
  (sum-of-muls
    (peg/compile ~{:main (any (choice :mul 1))
                   :mul ,mul-grammar})
    text))

(defn problem2 [text]
  (sum-of-muls
    (peg/compile ~{:main (any (choice :dont-range :mul 1))
                   :dont-range (* "don't()" (any (if-not "do()" 1)))
                   :mul ,mul-grammar})
    text))

(defn main [&]
  (spork/test/timeit
    (do
      (def path "3.txt")
      # (def path "3.test.txt")
      # (def path "3.test2.txt")
      (def text (slurp path))
      (print (problem1 text))
      (print (problem2 text)))))
