(use ./tools)

(defn main [&]
  (spork/test/timeit
    (print "hello, world")))
