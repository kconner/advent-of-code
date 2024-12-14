(use ./tools)

(defn robots-from-file [path]
  (->> (slurp path)
       (peg/match ~{:main (any (* :robot "\n"))
                    :robot (/ (* "p=" :vector " v=" :vector) ,|@{:pos $0 :vel $1})
                    :vector (/ (* :int "," :int) ,tuple)
                    :int (/ (<- (some (choice "-" (range "09")))) ,scan-number)})))

(defn time-step [seconds limit pos vel]
  (mod (+ pos (* seconds vel)) limit))

(defn time-step-vec [seconds [lx ly] {:pos [px py] :vel [vx vy]}]
  (map |(time-step seconds ;$) [[lx px vx] [ly py vy]]))

(defn problem1 [limits robots]
  (def final-positions (map |(time-step-vec 100 limits $) robots))
  (def [cx cy] (map |(div $ 2) limits))
  (*
    (count (fn [[px py]] (and (< px cx) (< py cy))) final-positions)
    (count (fn [[px py]] (and (< px cx) (> py cy))) final-positions)
    (count (fn [[px py]] (and (> px cx) (< py cy))) final-positions)
    (count (fn [[px py]] (and (> px cx) (> py cy))) final-positions)))

(defn problem2 [limits robots])

(defn main [&]
  (spork/test/timeit
    (do
      (def [path limits] ["14.txt" [101 103]])
      # (def [path limits] ["14.test.txt" [11 7]])
      (def robots (robots-from-file path))
      (print (problem1 limits robots))
      (print (problem2 limits robots)))))
