(->> (clojure.string/split-lines (slurp "5.txt"))
     (map (fn [line] (clojure.string/split line #" -> |,")))
     (map (fn [ordinate-strings] (mapv #(Integer. %) ordinate-strings)))
     (mapcat (fn [[x0 y0 x1 y1]]
               (let [step [(compare x1 x0) (compare y1 y0)]]
                 (reductions (fn [point _] (map + point step))
                             [x0 y0]
                             (->> (map #(Math/abs (- %1 %2)) [x0 y0] [x1 y1])
                                  (apply max)
                                  range)))))
     (reduce (fn [map key]
               (update map key (fn [value]
                                 (inc (or value 0))))) {})
     (filter (comp not zero? dec second))
     count)
