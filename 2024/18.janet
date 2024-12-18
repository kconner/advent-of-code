(use ./tools)

(defn walls-from-file [path]
  (->> (slurp path)
       (peg/match
         ~{:main (some :coordinate)
           :coordinate (/ (* :int "," :int "\n") ,tuple)
           :int (/ (<- (some (choice "-" (range "09")))) ,scan-number)})))

(defn in-bounds? [dimension [x y]]
  (and (< -1 x dimension) (< -1 y dimension)))

(defn next-steps [dimension visited pos]
  (->> [[1 0] [0 1] [-1 0] [0 -1]]
       (map |[;(map + pos $)])
       (filter |(and (not (visited $))
                     (in-bounds? dimension $)))))

(defn bfs [dimension walls]
  (def visited (from-pairs (map |[$ true] walls)))
  (def goal [(dec dimension) (dec dimension)])
  (def queue @[])
  (array/push queue {:pos [0 0] :steps 0})
  (prompt 'out
    (while (not (empty? queue))
      (def {:pos pos :steps steps} (queue 0))
      (array/remove queue 0)
      (when (= pos goal)
        (return 'out steps))
      (when (not (visited pos))
        (set (visited pos) true)
        (array/push queue ;(map |{:pos $ :steps (inc steps)}
                                (next-steps dimension visited pos)))))))

(defn problem1 [dimension walls limit]
  (bfs dimension (array/slice walls 0 limit)))

(defn problem2 [dimension walls initial-limit]
  (prompt 'out
    (for limit initial-limit 3450
      (when (nil? (bfs dimension (array/slice walls 0 limit)))
        (return 'out (string/format "%d,%d" ;(walls (dec limit))))))))

(defn main [&]
  (spork/test/timeit
    (do
      (def [path dimension limit] ["18.txt" 71 1024])
      # (def [path dimension limit] ["18.test.txt" 7 12])
      (def walls (walls-from-file path))
      (print (problem1 dimension walls limit))
      (print (problem2 dimension walls limit)))))
