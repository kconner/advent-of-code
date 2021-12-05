(->> (clojure.string/split (slurp "5.txt") #"\n")
     (map (fn [line] (clojure.string/split line #" -> |,")))
     (map (fn [ordinate-strings] (mapv #(Integer. %) ordinate-strings)))
     (filter (fn [[x0 y0 x1 y1]]
               (or (= x0 x1) (= y0 y1))))
     (mapcat (fn [[x0 y0 x1 y1]]
               (let [step [(compare x1 x0) (compare y1 y0)]]
                 (reduce (fn [[point :as all] _] (conj all (map + point step)))
                         (list [x0 y0])
                         (->> (map #(Math/abs (- %1 %2)) [x0 y0] [x1 y1])
                              (apply max)
                              range)))))
     (reduce (fn [map key]
               (update map key (fn [value]
                                 (inc (or value 0))))) {})
     (filter (comp not zero? dec second))
     (count))
