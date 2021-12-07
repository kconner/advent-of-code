(def locations (->> (clojure.string/split (slurp "7.txt") #",")
                    (mapv #(Integer. %))
                    sort))

(def cost (memoize (fn [distance]
                     (if (<= distance 1) distance
                         (+ distance (cost (dec distance)))))))

(->> (range (inc (last locations)))
     (pmap (fn [to]
             (reduce + (for [from locations]
                         (cost (Math/abs (- from to)))))))
     (reduce min))
