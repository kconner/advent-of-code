(require '[clojure.string :as string])

(defn table-entry [value]
  (->> (range 9)
       (reduce (fn [[value characters] _]
                 [(bit-shift-right value 1)
                  (conj characters (if (odd? value) \# \.))])
               [value, ()])
       (second)
       (partition 3)))

(defn table [input]
  (->> input
       (keep-indexed
        (fn [index character]
          (when (= character \#)
            (table-entry index))))
       set))

(defn padded-image [factor image]
  (let [padding (repeat factor \.)
        padded-lines (map #(concat padding % padding) image)
        padding-lines (repeat factor (repeat (count (first padded-lines)) \.))]
    (concat padding-lines padded-lines padding-lines)))

(defn enhance [table image]
  (let [horizontal-groups (map (fn [line] (map list line (drop 1 line) (drop 2 line))) image)]
    (map (partial map (fn [group] (if (table group) \# \.)))
         (map (partial map list) horizontal-groups (drop 1 horizontal-groups) (drop 2 horizontal-groups)))))

(time (let [lines (string/split-lines (slurp "20.txt"))
            a-table (table (first lines))]
        (->> (drop 2 lines)
             (padded-image 4)
             (enhance a-table)
             (enhance a-table)
             (apply concat)
             (filter (partial = \#))
             count)))