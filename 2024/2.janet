(use ./tools)

(defn model-from-file [path]
  (lines-from-file path))

(defn problem1 [model])

(defn problem2 [model])

(defn main [&]
  (spork/test/timeit
    (let [# path "2.txt"
          path "2.test.txt"
          model (model-from-file path)]
      (print (problem1 model))
      (print (problem2 model)))))
