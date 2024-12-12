(use ./tools)

(defn insert [counted-set stone stone-count]
  (update counted-set stone
          (fn [value]
            (default value 0)
            (+ value stone-count))))

(defn stones-from-file [path]
  (->> (slurp path)
       (peg/match ~{:main (some (* :int (choice " " "\n")))
                    :int (/ (<- (some (range "09"))) ,scan-number)})
       (reduce |(insert $0 $1 1) @{})))

(defn half-digit-count-to-tenth [n]
  (cond
    (< n 100) 10
    (< n 10000) 100
    (< n 1000000) 1000
    (< n 100000000) 10000
    (< n 10000000000) 100000
    (< n 1000000000000) 1000000
    true "assertion failed"))

(defn split [stone]
  (let [factor (half-digit-count-to-tenth stone)
        result @{(div stone factor) 1}]
    (insert result (mod stone factor) 1)))

(defn digit-count-even? [n]
  (cond
    (< n 10) false
    (< n 100) true
    (< n 1000) false
    (< n 10000) true
    (< n 100000) false
    (< n 1000000) true
    (< n 10000000) false
    (< n 100000000) true
    (< n 1000000000) false
    true "assertion failed"))

(defn blink [stone]
  (cond
    (zero? stone) @{1 1}
    (digit-count-even? stone) (split stone)
    true @{(* stone 2024) 1}))

(defn scale-counts [counted-set factor]
  (each key (keys counted-set)
    (update counted-set key |(* $ factor)))
  counted-set)

(defn insert-set [counted-set additions]
  (eachp (stone stone-count) additions
    (insert counted-set stone stone-count))
  counted-set)

(defn blink-all [stones]
  (let [result @{}]
    (eachp (stone stone-count) stones
      (insert-set result (scale-counts (blink stone) stone-count)))
    result))

(defn problem1 [stones]
  (var result stones)
  (for i 0 25
    (set result (blink-all result)))
  (+ ;(values result)))

(defn problem2 [stones]
  (var result stones)
  (for i 0 75
    (set result (blink-all result)))
  (+ ;(values result)))

(defn main [&]
  (spork/test/timeit
    (do
      (def path "11.txt")
      # (def path "11.test.txt")
      (def stones (stones-from-file path))
      (print (problem1 stones))
      (print (problem2 stones)))))
