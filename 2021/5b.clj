(->> (clojure.string/split (slurp "5.txt") #"\n")
     (map (fn [line] (clojure.string/split line #" -> ")))
     (map (fn [point-strings]
            (->> point-strings
                 (map (fn [string] (clojure.string/split string #",")))
                 (map (fn [ordinate-strings]
                        (mapv #(Integer. %) ordinate-strings))))))
     (mapcat (fn [[[x0 y0] [x1 y1]]]
               (let [xstep (compare x1 x0)
                     ystep (compare y1 y0)]
                 (for [step (range (inc (apply max (map #(Math/abs (- %1 %2)) [x0 y0] [x1 y1]))))]
                   [(+ x0 (* step xstep))
                    (+ y0 (* step ystep))]))))
     (reduce (fn [map key]
               (update map key (fn [value]
                                 (inc (or value 0))))) {})
     (filter (fn [[_ value]]
               (<= 2 value)))
     (count))
