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
     (map (fn [[[x0 y0] [x1 y1]]]
            (for [x (range x0 (+ 1 x1))
                  y (range y0 (+ 1 y1))]
              [x y])))
     (apply concat)
     (reduce (fn [map key]
               (update map key (fn [value]
                                 (inc (or value 0))))) {})
     (filter (fn [[_ value]]
               (<= 2 value)))
     (count))
