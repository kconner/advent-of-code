(import spork :only [test/timeit] :export true)

(defn lines-from-file [path]
  (filter |(not (empty? $))
          (string/split "\n" (slurp path))))
