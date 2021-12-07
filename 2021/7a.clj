(def locations (->> (clojure.string/split (slurp "7.txt") #",")
                    (mapv #(Integer. %))
                    sort))

(->> (range (inc (last locations)))
     (pmap (fn [to]
             (reduce + (for [from locations]
                         (Math/abs (- from to))))))
     (reduce min))
