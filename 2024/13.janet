(use ./tools)

(defn games-from-file [path]
  (->> (slurp path)
       (peg/match ~{:main (* :game (any (* "\n" :game)))
                    :game (/ (* :button :button :goal) ,|{:a $0 :b $1 :goal $2})
                    :button (/ (* "Button " (range "AB") ": X+" :int ", Y+" :int "\n") ,tuple)
                    :goal (/ (* "Prize: X=" :int ", Y=" :int "\n") ,tuple)
                    :int (/ (<- (some (range "09"))) ,scan-number)})))

(defn solve-game [{:a a :b b :goal goal} &named limit goal-offset]
  (default limit math/inf)
  (default goal-offset [0 0])
  (def [[ax ay] [bx by] [gx gy]] [a b (map + goal-offset goal)])
  # gx = ax A + bx B
  # gy = ay A + by B
  # (algebra ensues)
  # A = (gx - bx B) / ax = (gy - by B) / ay
  # B = (ax gy - ay gx) / (ax by - ay bx)
  (def b-count (/ (- (* ax gy) (* ay gx))
                  (- (* ax by) (* ay bx))))
  (def a-count (/ (- gx (* bx b-count)) ax))
  (if
    (and (zero? (mod a-count 1))
         (zero? (mod b-count 1)))
    (+ (* 3 a-count) b-count)
    nil))

(defn problem1 [games]
  (->> games
       (map |(solve-game $ :limit 100))
       (filter number?)
       (apply +)))

(defn problem2 [games]
  (->> games
       (map |(solve-game $ :goal-offset [10000000000000 10000000000000]))
       (filter number?)
       (apply +)))

(defn main [&]
  (spork/test/timeit
    (do
      (def path "13.txt")
      # (def path "13.test.txt")
      (def games (games-from-file path))
      (print (problem1 games))
      (print (problem2 games)))))
