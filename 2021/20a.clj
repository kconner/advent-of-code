(require '[clojure.string :as string])

(defn table-entry [value]
  (->> (range 9)
       (reduce (fn [[value characters] _]
                 [(bit-shift-right value 1)
                  (conj characters (if (odd? value) \# \.))])
               [value, ()])
       second
       (partition 3)))

(defn make-table [input]
  (->> input
       (keep-indexed (fn [index character]
                       (when (= character \#) (table-entry index))))
       set))

(defn padded-image [factor image]
  (let [padding (repeat factor \.)
        padded-lines (map (fn [line] (concat padding line padding)) image)
        padding-lines (repeat factor (repeat (count (first padded-lines)) \.))]
    (concat padding-lines padded-lines padding-lines)))

(defn enhance [table image]
  (let [horizontal-groups (map (fn [line] (map list line (drop 1 line) (drop 2 line))) image)]
    (map (partial map (fn [group] (if (table group) \# \.)))
         (map (partial map list) horizontal-groups (drop 1 horizontal-groups) (drop 2 horizontal-groups)))))

(time (let [lines (string/split-lines (slurp "20.txt"))
            table (make-table (first lines))
            steps 2]
        (->> (drop 2 lines)
             (padded-image (* 2 steps))
             (iterate (partial enhance table))
             (drop steps)
             first
             (apply concat)
             (filter (partial = \#))
             count)))
