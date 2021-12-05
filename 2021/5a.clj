(->> (clojure.string/split (slurp "5.txt") #"\n")
     (map (fn [line] (clojure.string/split line #" -> ")))
     (map (fn [point-strings]
            (->> point-strings
                 (map (fn [string] (clojure.string/split string #",")))
                 (map (fn [ordinate-strings]
                        (mapv #(Integer. %) ordinate-strings))))))
     (filter (fn [[[x0 y0] [x1 y1]]]
               (or (= x0 x1)
                   (= y0 y1))))
     (map sort)
     (mapcat (fn [[[x0 y0] [x1 y1]]]
               (for [x (range x0 (inc x1))
                     y (range y0 (inc y1))]
                 [x y])))
     (reduce (fn [map key]
               (update map key (fn [value]
                                 (inc (or value 0))))) {})
     (filter (comp not zero? dec second))
     (count))
