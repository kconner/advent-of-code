(use ./tools)

(defn grid-from-file [path]
  (let [input (slurp path)
        dimension (string/find "\n" input)
        wrap (inc dimension)
        char-position (fn [index] ~(,(mod index wrap) ,(div index wrap)))]
    (first (peg/match ~{:main (/ (some (choice (* :position :digit) "\n")) ,struct)
                        :digit (/ (<- (range "09")) ,scan-number)
                        :position (/ ($) ,char-position)} input))))

(def directions '((0 -1) (1 0) (0 1) (-1 0)))

(defn adjacent-positions [[x y]]
  (map (fn [[dx dy]] ~(,(+ x dx) ,(+ y dy))) directions))

(defn merge-sets [into src]
  (default into @{})
  (eachk key src
    (set (into key) true))
  into)

(defn nines-via-ns [grid n]
  (def result @{})
  (if (= n 9)
    (eachp (pos value) grid
      (when (= value n)
        (set (result pos) @{pos true})))
    (eachp (pos nines) (nines-via-ns grid (inc n))
      (each adjacent (adjacent-positions pos)
        (when (= (grid adjacent) n)
          (update result adjacent |(merge-sets $ nines))))))
  result)

(defn problem1 [grid]
  (+ ;(map length (values (nines-via-ns grid 0)))))

(defn trail-count-via-ns [grid n]
  (def result @{})
  (if (= n 9)
    (eachp (pos value) grid
      (when (= value n)
        (set (result pos) 1)))
    (eachp (pos trail-count) (trail-count-via-ns grid (inc n))
      (each adjacent (adjacent-positions pos)
        (when (= (grid adjacent) n)
          (update result adjacent (fn [c] (default c 0) (+ c trail-count)))))))
  result)

(defn problem2 [grid]
  (+ ;(values (trail-count-via-ns grid 0))))

(defn main [&]
  (spork/test/timeit
    (do
      (def path "10.txt")
      # (def path "10.test.txt")
      (def grid (grid-from-file path))
      (print (problem1 grid))
      (print (problem2 grid)))))
