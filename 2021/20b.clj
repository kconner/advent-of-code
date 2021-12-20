(require '[clojure.string :as string])

(defn group-if-lit [index character]
  (when (= character \#)
    (->> (range 9)
         (reduce (fn [[value characters] _]
                   [(bit-shift-right value 1)
                    (conj characters (if (odd? value) \# \.))])
                 [index ()])
         second
         (partition 3))))

(defn make-table [input]
  (let [lit-groups (set (keep-indexed group-if-lit input))]
    #(if (lit-groups %) \# \.)))

(defn padded-image [factor image]
  (let [padding (repeat factor \.)
        padded-lines (map (fn [line] (concat padding line padding)) image)
        padding-lines (repeat factor (repeat (count (first padded-lines)) \.))]
    (concat padding-lines padded-lines padding-lines)))

(defn triples [join items]
  (map join items (drop 1 items) (drop 2 items)))

(defn enhance [table image]
  (->> image
       (map (partial triples list))
       (triples (partial map list))
       (map (partial map table))))

(time (let [lines (string/split-lines (slurp "20.txt"))
            table (make-table (first lines))
            steps 50]
        (->> (drop 2 lines)
             (padded-image (* 2 steps))
             (iterate (partial enhance table))
             (drop steps)
             first
             (apply concat)
             (filter (partial = \#))
             count)))
