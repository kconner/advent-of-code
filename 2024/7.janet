(use ./tools)

(defn equations-from-file [path]
  (->> (slurp path)
       (peg/match ~{:main (some :line)
                    :line (/ (* :int ":" :operands "\n")
                             ,|{:test $0 :operands $1})
                    :operands (/ (some (* " " :int)) ,|(reverse $&))
                    :int (/ (<- (some (range "09"))) ,scan-number)})))

(defn integer? [v] (= (mod v 1) 0))

(defn solvable-p [{:test test :operands operands}]
  (defn iter [acc [o & os]]
    (if (empty? os)
      (= acc o)
      (or (let [accc (/ acc o)]
            (and (integer? accc) (iter accc os)))
          (let [accc (- acc o)]
            (and (pos? accc) (iter accc os))))))
  (iter test operands))

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

(defn decatenate [acc o]
  (/ (- acc o) (digit-count-to-tenth o)))

(defn solvable-with-decatenation-p [{:test test :operands operands}]
  (defn iter [acc [o & os]]
    (if (empty? os)
      (= acc o)
      (or (let [accc (/ acc o)]
            (and (integer? accc) (iter accc os)))
          (let [accc (- acc o)]
            (and (pos? accc) (iter accc os)))
          (let [accc (decatenate acc o)]
            (and (integer? accc) (iter accc os))))))
  (iter test operands))

(defn problem2 [equations]
  (->> equations
       (filter solvable-with-decatenation-p)
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
