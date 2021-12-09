(require '[clojure.set :as set])
(require '[clojure.string :as string])

(defn value-from-digits [digits]
  (reduce (fn [sum d] (+ (* sum 10) d)) digits))

(defn digits-by-segments [segment-sets]
  (let [by-count (group-by count segment-sets)
        [s25 s025 s1235 s-all] (->> [2 3 4 7]
                                    (map by-count)
                                    (map first))
        s0 (set/difference s025 s25)
        s13 (set/difference s1235 s25)
        s46 (set/difference s-all s0 s1235)
        s0156 (apply set/intersection (by-count 6))
        s1 (set/intersection s13 s0156)
        s5 (set/intersection s25 s0156)
        s2 (set/difference s25 s5)
        s3 (set/difference s13 s1)
        s036 (apply set/intersection (by-count 5))
        s6 (set/difference s036 s0 s3)
        s4 (set/difference s46 s6)]
    {s25 1 s025 7 s1235 4 s-all 8
     (set/difference s-all s3) 0
     (set/difference s-all s2) 6
     (set/difference s-all s4) 9
     (set/difference s-all s1 s5) 2
     (set/difference s-all s1 s4) 3
     (set/difference s-all s2 s4) 5}))

(defn value-from-line [line]
  (let [segments (mapv set (string/split line #" "))]
    (value-from-digits (mapv (digits-by-segments (subvec segments 0 10))
                             (subvec segments 11)))))

(->> (slurp "8.txt")
     string/split-lines
     (pmap value-from-line)
     (reduce +))
