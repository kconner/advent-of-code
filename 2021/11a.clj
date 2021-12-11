(require '[clojure.string :as string])
(require '[clojure.set :as set])

(def ordinates (range 10))

(def all-cells (for [r ordinates c ordinates] [r c]))

(def neighboring-ordinates
  (memoize
   (fn [i] (set/intersection #{(dec i) i (inc i)} (set ordinates)))))

(def neighbors
  (memoize
   (fn [[r c]] (set (for [ar (neighboring-ordinates r)
                          ac (neighboring-ordinates c)] [ar ac])))))

(defn rays-from-border [flashed-cells border-cells]
  (->> border-cells
       (mapcat (fn [cell] (set/difference (neighbors cell) flashed-cells)))
       frequencies))

(defn absorb [rows rays]
  (reduce-kv (fn [[rows flashing-cells] cell ray-count]
               (let [value (+ (get-in rows cell) ray-count)]
                 [(assoc-in rows cell value)
                  (if (<= value 9) flashing-cells (conj flashing-cells cell))]))
             [rows #{}]
             rays))

(defn flash [rows flashed-cells border-cells]
  (let [rays (rays-from-border flashed-cells border-cells)]
    (if (empty? rays) rows
        (let [[rows frontier] (absorb rows rays)]
          (flash rows (set/union flashed-cells frontier) frontier)))))

(def ambient-rays (frequencies all-cells))

(defn step [rows flash-count]
  (let [[rows flashed-cells] (absorb rows ambient-rays)]
    (reduce (fn [[rows flash-count] cell]
              (if (<= (get-in rows cell) 9)
                [rows flash-count]
                [(assoc-in rows cell 0) (inc flash-count)]))
            [(flash rows flashed-cells flashed-cells) flash-count]
            all-cells)))

(def rows (mapv (partial mapv #(compare % \0))
                (string/split-lines (slurp "11.txt"))))

(->> [rows 0]
     (iterate (partial apply step))
     (drop 100)
     first
     second)
