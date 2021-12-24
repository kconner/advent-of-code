(require '[clojure.string :as string])

(def step-regex
  #"(on|off) x=(-?\d+)..(-?\d+),y=(-?\d+)..(-?\d+),z=(-?\d+)..(-?\d+)")

(defn step-from-line [line]
  (let [[_ color & bounds] (re-matches step-regex line)]
    {:on (= color "on")
     :tree {:box (mapv vec (partition 2 (map #(Integer. %) bounds)))}}))

(->> (slurp "22.txt")
     string/split-lines
     (map step-from-line)
     (take 2)
     reverse)

(defn volume [tree]
  (if-let [children (:children tree)]
    (apply + (map volume children))
    (let [[[lx ux] [ly uy] [lz uz]] (:box tree)]
      (* (- (inc ux) lx) (- (inc uy) ly) (- (inc uz) lz)))))

(time (volume {:box [[5 5] [6 7] [8 10]]}))
(time (volume {:box [[5 9] [6 14] [8 29]]
               :children [{:box [[5 5] [6 7] [8 10]]}
                          {:box [[5 9] [10 14] [25 29]]}]}))

(defn intersects-x [[[l1 u1]] [[l2 u2]]]
  (and (<= l1 u2) (<= l2 u1)))

(defn intersects-y [[_ [l1 u1]] [_ [l2 u2]]]
  (and (<= l1 u2) (<= l2 u1)))

(defn intersects-z [[_ _ [l1 u1]] [_ _ [l2 u2]]]
  (and (<= l1 u2) (<= l2 u1)))

(defn intersects-yz [box1 box2]
  (and (intersects-x box1 box2)
       (intersects-y box1 box2)
       (intersects-z box1 box2)))

(time (intersects-xyz [[5 9] [14 15] [8 10]]
                      [[5 9] [6 14] [8 29]]))

(defn covered-by-x [[[l1 u1]] [[l2 u2]]]
  (and (<= l2 l1) (<= u1 u2)))

(defn covered-by-y [[_ [l1 u1]] [_ [l2 u2]]]
  (and (<= l2 l1) (<= u1 u2)))

(defn covered-by-z [[_ _ [l1 u1]] [_ _ [l2 u2]]]
  (and (<= l2 l1) (<= u1 u2)))

(defn covered-by-xyz [box1 box2]
  (and (covered-by-x box1 box2)
       (covered-by-y box1 box2)
       (covered-by-z box1 box2)))

(time (covered-by-xyz [[4 10] [14 15] [8 10]]
                      [[4 11] [6 15] [7 9]]))

(defn subtract [{box :box :as leaf}
                {tbox :box tchildren :children :as tree}]
  ; do the bounding boxes overlap? if not, we're done
  (cond (not (intersects-xyz box tbox)) leaf
  ; if bounding boxes overlap and there are no children,
        (nil? tchildren) :todo
        ;
        ))

(time (subtract {:box [[4 5] [6 7] [8 10]]}
                {:box [[5 9] [6 14] [8 29]]}))

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
