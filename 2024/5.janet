(use ./tools)

(defn model-from-file [path]
  (->> (slurp path)
       (peg/match
         ~{:main (/ (* :rules "\n" :lists -1) ,|{:rules $0 :lists $1})
           :rules (/ (some (* :rule (constant true))) ,struct)
           :rule (/ (* :int "|" :int "\n") ,tuple)
           :lists (/ (some :list) ,tuple)
           :list (/ (* (some (* :int ",")) :int "\n") ,tuple)
           :int (/ (<- (between 1 3 (range "09"))) ,scan-number)})
       (first)))

# procedural code is so hot right now
(defn correctly-ordered-p [rules list]
  (prompt 'out
    (eachp (i1 v1) (tuple/slice list 0 (dec (length list)))
      (each v2 (drop (inc i1) list)
        (if (rules ~(,v2 ,v1))
          (return 'out false))))
    true))

(defn middle-item [list]
  (list (/ (dec (length list)) 2)))

(defn problem1 [{:lists lists :rules rules}]
  (->> lists
       (filter |(correctly-ordered-p rules $))
       (map middle-item)
       (apply +)))

(defn problem2 [{:lists lists :rules rules}]
  (->> lists
       (filter |(not (correctly-ordered-p rules $)))
       (map |(sorted $ |(rules $&)))
       (map middle-item)
       (apply +)))

(defn main [&]
  (spork/test/timeit
    (do
      (def path "5.txt")
      # (def path "5.test.txt")
      (def model (model-from-file path))
      (print (problem1 model))
      (print (problem2 model)))))
