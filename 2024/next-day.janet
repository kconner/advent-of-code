(use ./tools)

(defn model-from-file [path]
  (lines-from-file path))

(defn problem1 [model])

(defn problem2 [model])

(defn main [&]
  (spork/test/timeit
    (do
      # (def path "n.txt")
      (def path "n.test.txt")
      (def reports (reports-from-file path))
      (print (problem1 reports))
      (print (problem2 reports)))))
