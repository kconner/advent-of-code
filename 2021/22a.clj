(require '[clojure.string :as string])

(def step-regex
  #"(on|off) x=(-?\d+)..(-?\d+),y=(-?\d+)..(-?\d+),z=(-?\d+)..(-?\d+)")

(defn step-from-line [line]
  (let [[_ color & bounds] (re-matches step-regex line)]
    (concat [(when (= color "on") true)]
            (mapv #(Integer. %) bounds))))

(defn filter-limits [lower upper steps]
  (filter (fn [[_ minx maxx miny maxy minz maxz]]
            (and (<= lower maxx) (<= minx upper)
                 (<= lower maxy) (<= miny upper)
                 (<= lower maxz) (<= minz upper))) steps))

(defn filter-x [steps x]
  (filter (fn [[_ min max]] (<= min x max)) steps))

(defn filter-y [steps y]
  (filter (fn [[_ _ _ min max]] (<= min y max)) steps))

(defn filter-z [steps z]
  (filter (fn [[_ _ _ _ _ min max]] (<= min z max)) steps))

(time
 (let [upper 50
       lower (- upper)
       ordinates (range lower (inc upper))
       steps (->> (slurp "22.txt")
                  string/split-lines
                  (map step-from-line)
                  reverse
                  (filter-limits lower upper))]
   (count
    (apply concat
           (pmap (fn [x]
                   (let [steps (filter-x steps x)]
                     (apply concat
                            (pmap (fn [y]
                                    (let [steps (filter-y steps y)]
                                      (keep (fn [z]
                                              (let [steps (filter-z steps z)]
                                                (ffirst steps)))
                                            ordinates)))
                                  ordinates))))
                 ordinates)))))
