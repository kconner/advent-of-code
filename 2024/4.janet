(use ./tools)

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

(defn problem1 [lines]
  (->> lines
       (map string/bytes)
       (search-space)
       (string/find-all "XMAS")
       (length)))

(defn problem2 [lines]
  (let [text (string/join lines " ")
        sms (from-pairs (map |(tuple (string/bytes $) true) (tuple "SM" "MS")))
        hop (inc (length (first lines)))]
    (defn x-mas-p [joint]
      (->> ~((,(get text (- joint hop 1)) ,(get text (+ joint hop 1)))
              (,(get text (- joint hop -1)) ,(get text (+ joint hop -1))))
           (map sms)
           (every?)))
    (count x-mas-p (string/find-all "A" text))))

(defn main [&]
  (spork/test/timeit
    (do
      (def path "4.txt")
      # (def path "4.test.txt")
      (def lines (lines-from-file path))
      (print (problem1 lines))
      (print (problem2 lines)))))
