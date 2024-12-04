(use ./tools)

(defn rows-from-file [path]
  (map string/bytes (lines-from-file path)))

(defn flip [lines]
  (map reverse lines))

(defn transpose [lines]
  (map tuple ;lines))

(defn shear [dimension nil-runs lines]
  (->> (map |(tuple ;(nil-runs $0)
                    ;$1
                    ;(nil-runs (- dimension 1 $0)))
            (range dimension)
            lines)
       (apply map tuple)
       (map |(filter |(not (nil? $)) $))
       (filter |(<= 4 (length $)))))

(defn search-space [rows]
  (let [dimension (length rows)
        nil-runs (map array/new-filled (range (inc dimension)))
        east rows
        west (flip east)
        south (transpose east)
        north (flip south)
        southwest (shear dimension nil-runs east)
        northeast (flip southwest)
        southeast (shear dimension nil-runs west)
        northwest (flip southeast)]
    (string/join
      (mapcat |(map |(string/from-bytes ;$) $)
              [east west north south northeast northwest southeast southwest])
      " ")))

(defn problem1 [rows]
  (length (string/find-all "XMAS" (search-space rows))))

(defn problem2 [rows])

(defn main [&]
  (spork/test/timeit
    (do
      (def path "4.txt")
      # (def path "4.test.txt")
      (def rows (rows-from-file path))
      (print (problem1 rows))
      (print (problem2 rows)))))
