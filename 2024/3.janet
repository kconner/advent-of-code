(use ./tools)

(defn sum-of-muls [text]
  (+ ;(map |(apply * $) (partition 2 text))))

(def mul-grammar
  ~{:main (* "mul(" :int "," :int ")")
    :int (/ (<- (between 1 3 (range "09"))) ,scan-number)})

(defn problem1 [text]
  (->> text
       (peg/match ~{:main (any (choice ,mul-grammar 1))})
       (sum-of-muls)))

(defn problem2 [text]
  (->> text
       (peg/match ~{:main (any (choice :dont-range ,mul-grammar 1))
                    :dont-range (* "don't()" (any (if-not "do()" 1)))})
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
