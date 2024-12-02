(use ./tools)

(def line-grammar
  (let [id ~(capture (some (range "09")))
        spaces ~(some " ")]
    (peg/compile ~(* (some (* ,id ,spaces)) ,id))))

(defn reports-from-file [path]
  (map |(map scan-number (peg/match line-grammar $))
       (lines-from-file path)))

(def safe-increases {+1 true +2 true +3 true})
(def safe-decreases {-1 true -2 true -3 true})

(defn is-safe [report]
  (def changes (map - (drop 1 report) report))
  (def safe-changes (if (< 0 (first changes)) safe-increases safe-decreases))
  (every? (map |(safe-changes $) changes)))

(defn problem1 [reports]
  (count is-safe reports))

(defn problem2 [reports]
  (defn slicings [report]
    (map |(tuple/join (tuple/slice report 0 $)
                      (tuple/slice report (inc $)))
         (range (length report))))
  (count |(some is-safe ~(,$ ,;(slicings $))) reports))

(defn main [&]
  (spork/test/timeit
    (do
      (def path "2.txt")
      # (def path "2.test.txt")
      (def reports (reports-from-file path))
      (print (problem1 reports))
      (print (problem2 reports)))))
