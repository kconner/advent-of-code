(use ./tools)

(defn equations-from-file [path]
  (->> (slurp path)
       (peg/match ~{:main (some :line)
                    :line (/ (* :int ":" :operands "\n")
                             ,|{:test $0 :operands $1})
                    :operands (/ (some (* " " :int)) ,tuple)
                    :int (/ (<- (some (range "09"))) ,scan-number)})))

(defn solvable-p [{:test test :operands [o & os]}]
  (defn iter [acc operands]
    (if (< test acc)
      false
      (match operands
        [o & os] (or (iter (* acc o) os)
                     (iter (+ acc o) os))
        [] (= test acc))))
  (iter o os))

(defn problem1 [equations]
  (->> equations
       (filter solvable-p)
       (map |($ :test))
       (apply +)))

(defn digit-count-to-tenth [n]
  (cond
    (< n 10) 10
    (< n 100) 100
    (< n 1000) 1000
    (< n 10000) 10000
    true "assertion failed"))

(defn catenate [a b]
  (+ b (* a (digit-count-to-tenth b))))

(defn solvable-with-catenation-p [{:test test :operands [o & os]}]
  (defn iter [acc operands]
    (if (< test acc)
      false
      (match operands
        [o & os] (or (iter (* acc o) os)
                     (iter (+ acc o) os)
                     (iter (catenate acc o) os))
        [] (= test acc))))
  (iter o os))

(defn problem2 [equations]
  (->> equations
       (filter solvable-with-catenation-p)
       (map |($ :test))
       (apply +)))

(defn main [&]
  (spork/test/timeit
    (do
      (def path "7.txt")
      # (def path "7.test.txt")
      (def equations (equations-from-file path))
      (print (problem1 equations))
      (print (problem2 equations)))))
