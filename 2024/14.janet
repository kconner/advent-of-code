(use ./tools)

(defn robots-from-file [path]
  (->> (slurp path)
       (peg/match ~{:main (any (* :robot "\n"))
                    :robot (/ (* "p=" :vector " v=" :vector) ,|{:pos $0 :vel $1})
                    :vector (/ (* :int "," :int) ,tuple)
                    :int (/ (<- (some (choice "-" (range "09")))) ,scan-number)})))

(defn center-of [limits]
  (map |(div $ 2) limits))

(defn time-step [seconds limit pos vel]
  (mod (+ pos (* seconds vel)) limit))

(defn time-step-vec [seconds [lx ly] {:pos [px py] :vel [vx vy]}]
  [;(map |(time-step seconds ;$) [[lx px vx] [ly py vy]])])

(defn time-step-positions-of [seconds limits robots]
  (map |(time-step-vec seconds limits $) robots))

(defn problem1 [limits robots]
  (def [cx cy] (center-of limits))
  (def final-positions (time-step-positions-of 100 limits robots))
  (*
    (count (fn [[px py]] (and (< px cx) (< py cy))) final-positions)
    (count (fn [[px py]] (and (< px cx) (> py cy))) final-positions)
    (count (fn [[px py]] (and (> px cx) (< py cy))) final-positions)
    (count (fn [[px py]] (and (> px cx) (> py cy))) final-positions)))

(defn squared-scaled-distance-to-center [[cx cy] [px py]]
  (let [dx (- cx px)
        dy (- cy py)]
    (+ (/ (* dx dx) cx)
       (/ (* dy dy) cy))))

(defn min-and-max-squared-scaled-distances-to-center [center positions]
  (def dists (map |(squared-scaled-distance-to-center center $) positions))
  [(reduce2 min dists)
   (reduce2 max dists)])

(defn table-of [positions]
  (from-pairs (map |[$ true] positions)))

(defn string-of [[lx ly] positions]
  (def pos-table (table-of positions))
  (def dot (first (string/bytes "*")))
  (def space (first (string/bytes ".")))
  (def newline (first (string/bytes "\n")))
  (def bytes @[])
  (each x (range lx)
    (array/push bytes ;(map |(if (pos-table [x $]) dot space) (range ly)) newline))
  (pp bytes)
  (string/from-bytes ;bytes))

(defn problem2 [limits robots]
  (def center (center-of limits))
  (for t 3299 9283
    (def final-positions (time-step-positions-of t limits robots))
    (def [min-egg max-egg] (min-and-max-squared-scaled-distances-to-center
                             center final-positions))
    (when (<= 89 max-egg 91)
      (pp [t min-egg max-egg])
      (print (string-of limits final-positions)))))

(defn main [&]
  (spork/test/timeit
    (do
      (def [path limits] ["14.txt" [101 103]])
      # (def [path limits] ["14.test.txt" [11 7]])
      (def robots (robots-from-file path))
      (print (problem1 limits robots))
      (print (problem2 limits robots)))))

# lower bound (3299 73)

# wrong (6831 81.42
# might be 6936?
# ok anyway found it. no need for beauty today

# upper bound (9282 1.14078431372549)
# upper bound (13702 73)

