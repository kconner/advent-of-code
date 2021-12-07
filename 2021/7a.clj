(def locations (->> (clojure.string/split (slurp "7.txt") #",")
                    (mapv #(Integer. %))
                    sort))

(defn cost [from to]
  (Math/abs (- from to)))

(->> (range (inc (last locations)))
     (map (fn [to]
            (for [from locations]
              (cost from to))))
     (map #(reduce + %))
     (reduce min))
