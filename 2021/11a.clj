(require '[clojure.string :as string])
(require '[clojure.set :as set])

(def neighboring-ordinates
  (memoize
   (fn [i]
     (set/intersection #{(dec i) i (inc i)}
                       (set (range 10))))))

(def neighbors
  (memoize
   (fn [[r c]]
     (set (for [ar (neighboring-ordinates r)
                ac (neighboring-ordinates c)] [ar ac])))))

(defn rays-from-border [flashed-cells border-cells]
  (->> border-cells
       (mapcat (fn [cell] (set/difference (neighbors cell) flashed-cells)))
       frequencies))

(defn flash [rows flashed-cells border-cells]
  (let [rays (rays-from-border flashed-cells border-cells)]
    (if (empty? rays) rows
        (let [[rows frontier-cells]
              (reduce-kv (fn [[rows frontier-cells] cell ray-count]
                           (let [value (+ (get-in rows cell) ray-count)]
                             [(assoc-in rows cell value)
                              (if (<= value 9) frontier-cells
                                  (conj frontier-cells cell))]))
                         [rows #{}]
                         rays)]
          (flash rows
                 (set/union flashed-cells frontier-cells)
                 frontier-cells)))))

(defn step [rows flash-count]
  (let [[rows flashed-cells]
        (reduce (fn [[rows flashed-cells] cell]
                  (let [value (inc (get-in rows cell))]
                    [(assoc-in rows cell value)
                     (if (<= value 9) flashed-cells
                         (conj flashed-cells cell))]))
                [rows #{}]
                (for [r (range 10) c (range 10)] [r c]))]
    (reduce (fn [[rows flash-count] cell]
              (if (<= (get-in rows cell) 9)
                [rows flash-count]
                [(assoc-in rows cell 0) (inc flash-count)]))
            [(flash rows flashed-cells flashed-cells) flash-count]
            (for [r (range 10) c (range 10)] [r c]))))

(def rows (mapv (partial mapv #(compare % \0))
                (string/split-lines (slurp "11.txt"))))

(->> [rows 0]
     (iterate (partial apply step))
     (drop 100)
     first
     second)
