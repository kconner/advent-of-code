(use ./tools)

(def line-grammar
  (let [id ~(capture (some (range "09")))
        spaces ~(some " ")]
    (peg/compile ~(* (some (* ,id ,spaces)) ,id -1))))

(defn reports-from-file [path]
  (map |(map scan-number (peg/match line-grammar $))
       (lines-from-file path)))

(def safe-increases {+1 true +2 true +3 true})
(def safe-decreases {-1 true -2 true -3 true})

# safe when no difference between an adjacent level is not in the safe set.
# the safe set depends on the sign of the first.
(defn is-safe [report]
  (let [changes (map - (drop 1 report) report)
        safe-changes (if (< 0 (first changes)) safe-increases safe-decreases)]
    (not (find |(not (safe-changes $)) changes))))

(defn problem1 [reports]
  (count is-safe reports))

# count of the reports for which the report or any of its slicings is safe.
# slicings each omit one item.
(defn problem2 [reports]
  (defn slicings [report]
    (map |(tuple/join (tuple/slice report 0 $)
                      (tuple/slice report (inc $)))
         (range (length report))))
  (count |(or (is-safe $)
              (some is-safe (slicings $)))
         reports))

(defn main [&]
  (spork/test/timeit
    (do
      (def path "2.txt")
      # (def path "2.test.txt")
      (def reports (reports-from-file path))
      (print (problem1 reports))
      (print (problem2 reports)))))
