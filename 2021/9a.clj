(require '[clojure.string :as string])
(require '[clojure.set :as set])

(defn lows-in-line [line]
  (let [bounded-line (concat "z" line "z")
        slopes (map compare (drop 1 bounded-line) bounded-line)
        is-low (mapv (fn [a b] (and (neg? a) (pos? b)))
                     slopes (drop 1 slopes))]
    (filter is-low (range (count is-low)))))

(defn lows-in-lines [lines]
  (->> lines
       (map-indexed (fn [r line] (map (partial list r)
                                      (lows-in-line line))))
       (apply concat)))

(defn transpose [lines]
  (for [c (range (count (first lines)))]
    (map (fn [line] (line c)) lines)))

(let [lines (mapv vec (string/split-lines (slurp "9.txt")))]
  (->> (set/intersection
        (set (lows-in-lines lines))
        (set (map reverse (lows-in-lines (transpose lines)))))
       (map (fn [[r c]] (compare ((lines r) c) \/)))
       (reduce +)))
