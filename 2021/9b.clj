(require '[clojure.string :as string])
(require '[clojure.set :as set])

(defn cell-value [lines [r c]]
  (get (get lines r) c))

(defn basin-frontier [lines basin border]
  (->> border
       (map (fn [[r c]] #{[(dec r) c] [r (inc c)]
                          [(inc r) c] [r (dec c)]}))
       (apply set/union)
       (remove (partial basin))
       (remove (fn [cell]
                 (let [value (cell-value lines cell)]
                   (or (nil? value)
                       (= \9 value)))))
       set))

(defn basin-from-cell-step [lines basin border]
  (let [frontier (basin-frontier lines basin border)
        basin (set/union basin frontier)]
    (if (empty? frontier) basin
        (basin-from-cell-step lines basin frontier))))

(defn basin-from-cell [lines cell]
  (basin-from-cell-step lines #{cell} #{cell}))

(defn basins-among-lines [lines]
  (reduce (fn [basins cell]
            (if (or (= \9 (cell-value lines cell))
                    (some (fn [basin] (basin cell)) basins))
              basins
              (conj basins (basin-from-cell lines cell))))
          #{}
          (for [r (range (count lines))
                c (range (count (first lines)))] [r c])))

(->> "9.txt"
     slurp
     string/split-lines
     (mapv vec)
     basins-among-lines
     (map count)
     sort
     reverse
     (take 3)
     (reduce *))
