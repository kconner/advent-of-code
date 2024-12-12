(use ./tools)

# 0 -> 1
# even number of digits AAABBB -> AAA BBB,
#   and drop leading 0s so sometimes it's AAA BB.
# otherwise N -> 2024*N

# how many stones after 25 blinks?
# this looks like a job for memoization.

(defn stones-from-file [path]
  (->> (slurp path)
       (peg/match ~{:main (some (* :int (choice " " "\n")))
                    :int (/ (<- (some (range "09"))) ,scan-number)})))

(defn half-digit-count-to-tenth [n]
  (cond
    (< n 100) 10
    (< n 10000) 100
    (< n 1000000) 1000
    (< n 100000000) 10000
    (< n 10000000000) 100000
    (< n 1000000000000) 1000000
    true "assertion failed"))

(half-digit-count-to-tenth 999999)

(defn split [stone]
  (let [factor (half-digit-count-to-tenth stone)]
    [(div stone factor) (mod stone factor)]))

(split 255000)

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
    (< n 10000000000) true
    true "assertion failed"))

(digit-count-even? 1000)

(defn blink [cache stone]
  (let [cached (cache stone)]
    (if (truthy? cached)
      cached
      (let [result (cond
                     (zero? stone) [1]
                     (digit-count-even? stone) (split stone)
                     true [(* stone 2024)])]
        (set (cache stone) result)
        result))))

(defn blink-times [cache stone n]
  (let [stones (blink cache stone)]
    (if (<= n 1)
      stones
      (mapcat |(blink-times cache $ (dec n)) stones))))

stones
(length (blink-times @{} 17 25))

(defn problem1 [stones]
  (def cache @{})
  (->> stones
       (mapcat |(blink-times cache $ 25))
       (length)))

(defn problem2 [stones]
  (def cache @{})
  (->> stones
       (mapcat |(blink-times cache $ 75))
       (length)))


(defn main [&]
  (spork/test/timeit
    (do
      # (def path "11.txt")
      (def path "11.test.txt")
      (def stones (stones-from-file path))
      (print (problem1 stones))
      (print (problem2 stones)))))
