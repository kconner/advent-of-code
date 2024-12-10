(use ./tools)

(defn model-from-file [path]
  (let [input (slurp path)
        dimension (string/find "\n" input)
        wrap (inc dimension)
        char-position (fn [index] ~(,(mod index wrap) ,(div index wrap)))]
    {:dimension dimension
     :grid (first (peg/match ~{:main (/ (some (choice (* :position :digit) "\n")) ,struct)
                               :digit (/ (<- (range "09")) ,scan-number)
                               :position (/ ($) ,char-position)} input))}))

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

(defn problem1 [{:dimension dimension :grid grid}]
  (+ ;(map length (values (nines-via-ns grid 0)))))

(defn problem2 [model])

(defn main [&]
  (spork/test/timeit
    (do
      (def path "10.txt")
      # (def path "10.test.txt")
      (def model (model-from-file path))
      (print (problem1 model))
      (print (problem2 model)))))
