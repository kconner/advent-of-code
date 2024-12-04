(use ./tools)

(defn model-from-file [path]
  (lines-from-file path))

(defn problem1 [model])

(defn problem2 [model])

(defn main [&]
  (spork/test/timeit
    (do
      # (def path "5.txt")
      (def path "5.test.txt")
      (def model (model-from-file path))
      (print (problem1 model))
      (print (problem2 model)))))
