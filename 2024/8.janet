(use ./tools)

(defn model-from-file [path]
  (let [input (slurp path)
        dimension (string/find "\n" input)
        wrap (inc dimension)
        char-position (fn [index] ~(,(mod index wrap) ,(div index wrap)))
        antennae (peg/match
                   ~{:main (some (choice "." "\n" :antenna))
                     :antenna (/ (<- (* ($) :alphanumeral)) ,tuple)
                     :alphanumeral (choice (range "09") (range "AZ") (range "az"))}
                   input)]
    {:dimension dimension
     :freqs (->> antennae
                 (map (fn [[index freq]] ~(,freq ,(char-position index))))
                 (group-by first)
                 (values)
                 (map |(map |($ 1) $)))}))

(defn in-bounds [dimension pos]
  (every? (map |(< -1 $ dimension) pos)))

(defn problem1 [{:freqs freqs :dimension dimension}]
  (def antinodes @{})
  (each antennae freqs
    (each a antennae
      (each b antennae
        (if (not= a b)
          (let [an (tuple ;(map + a (map - a b)))]
            (if (in-bounds dimension an)
              (set (antinodes an) true)))))))
  (length antinodes))

(defn problem2 [{:freqs freqs :dimension dimension}]
  (def antinodes @{})
  (each antennae freqs
    (each a antennae
      (each b antennae
        (when (not= a b)
          (def step (map - a b))
          (var pos a)
          (while (in-bounds dimension pos)
            (set (antinodes pos) true)
            (set pos (tuple ;(map + pos step))))))))
  (length antinodes))

(defn main [&]
  (spork/test/timeit
    (do
      (def path "8.txt")
      # (def path "8.test.txt")
      (def model (model-from-file path))
      (print (problem1 model))
      (print (problem2 model)))))
