(use ./tools)

# 4 games -> 320 games

# push A: 3 tokens
# push B: 1 token

# button press limit: 100 per button

# for each game, can it be solved,
# and if so, what's the fewest tokens you can spend to solve it?

# i picture slowly increasing cost while circling the prize to reach it.
# let's say i start wih 100 Bs since that's cheaper.
# when on the prize I stop and call this the cheapest.
# while both right and forward of the prize i subtract a B.
# while either left or behind the prize i add an A.
# no solution when B falls below 0.

(defn games-from-file [path]
  (->> (slurp path)
       (peg/match ~{:main (* :game (any (* "\n" :game)))
                    :game (/ (* :button :button :goal) ,|{:a $0 :b $1 :goal $2})
                    :button (/ (* "Button " (range "AB") ": X+" :int ", Y+" :int "\n") ,tuple)
                    :goal (/ (* "Prize: X=" :int ", Y=" :int "\n") ,tuple)
                    :int (/ (<- (some (range "09"))) ,scan-number)})))

(defn solve-game [limit {:a a :b b :goal goal}]
  (def [[ax ay] [bx by] [gx gy]] [a b goal])
  (def initial-b (min limit (inc (max (div gx bx) (div gy by)))))
  (var [a-count b-count] [0 initial-b])
  (var [px py] [(+ (* a-count ax) (* b-count bx))
                (+ (* a-count ay) (* b-count by))])
  (prompt 'out
    (while (<= 0 b-count)
      # (pp [[a-count b-count] [px py] [(+ (* a-count ax) (* b-count bx)) (+ (* a-count ay) (* b-count by))] [gx gy]])
      (cond
        (= [px py] goal) (return 'out (+ (* 3 a-count) b-count))
        (and (<= gx px) (<= gy py))
        (do
          (set b-count (dec b-count))
          (set px (- px bx))
          (set py (- py by)))
        true
        (do
          (set a-count (inc a-count))
          (set px (+ px ax))
          (set py (+ py ay)))))))

(defn problem1 [games]
  (->> games
       (map |(solve-game 100 $))
       (filter number?)
       (apply +)))

(defn problem2 [games])

(defn main [&]
  (spork/test/timeit
    (do
      (def path "13.txt")
      # (def path "13.test.txt")
      (def games (games-from-file path))
      (print (problem1 games))
      (print (problem2 games)))))
