(require '[clojure.string :as string])

(def step-regex
  #"(on|off) x=(-?\d+)..(-?\d+),y=(-?\d+)..(-?\d+),z=(-?\d+)..(-?\d+)")

(defn step-from-line [line]
  (let [[_ value minx maxx miny maxy minz maxz] (re-matches step-regex line)]
    {:from (Integer. minx) :to (inc (Integer. maxx))
     :step {:from (Integer. miny) :to (inc (Integer. maxy))
             :step {:from (Integer. minz) :to (inc (Integer. maxz))
                     :value (= "on" value)}}}))

(def -step (step-from-line "on x=-42..2,y=-37..12,z=-34..14"))

(def -tree
  [{:at -42 :child
    [{:at -37 :child
      [{:at -34 :value true}
       {:at 15}]}
     {:at 13}]}
   {:at 3}])

(def -tree2
  [{:at -42 :child
    [{:at -37 :child
      [{:at -34 :value true}
       {:at 0}
       {:at 6 :value false}
       {:at 10 :value true}
       {:at 15}]}
     {:at 13}]}
   {:at 3}])

; could binary search instead.
(defn cut [node edge]
(let [extended (concat [{:at ##-Inf}] node [{:at ##Inf}])]
  (vec
    (drop 1
      (mapcat (fn [{from :at :as interval} {to :at}]
            (if (< from edge to)
              [interval (assoc interval :at edge)]
              [interval]))
          extended (drop 1 extended))))))

(cut (get-in -tree2 [0 :child 0 :child]) 0)
(cut (get-in -tree2 [0 :child 0 :child]) 1)
(cut (get-in -tree2 [0 :child 0 :child]) -50)
(cut (get-in -tree2 [0 :child 0 :child]) 50)

(defn insert-z [node {from :from to :to new-value :value}]
  (map (fn [{at :at value :value :as interval}]
         (if (and (nil? value) (<= from at) (< at to))
           (assoc interval :value new-value)
           interval))
       (cut (cut node from) to)))

(insert-z (get-in -tree2 [0 :child 0 :child]) {:from -50 :to 50 :value false})
(insert-z [] {:from -50 :to 50 :value false})

(defn insert-y [node {from :from to :to step :step}]
  (map (fn [{at :at child :child :as interval}]
         (if (and (<= from at) (< at to))
           (assoc interval :child (insert-z (or child []) step))
           interval))
       (cut (cut node from) to)))

(defn insert-x [node {from :from to :to step :step}]
  (map (fn [{at :at child :child :as interval}]
         (if (and (<= from at) (< at to))
           (assoc interval :child (insert-y (or child []) step))
           interval))
       (cut (cut node from) to)))

(insert-y (get-in -tree2 [0 :child]) {:from -50 :to 50 :step {:from 0 :to 9 :value false}})
(insert-y [] {:from -50 :to 50 :step {:from -10 :to 10 :value false}})

-tree
-tree2
(insert-x -tree -step)
(insert-x -tree2 -step)
(insert-x [] -step)

(defn on-length [node]
  (reduce + (map (fn [{from :at value :value} {to :at}]
                   (if value (- to from) 0))
                 node (drop 1 node))))

(on-length (get-in -tree [0 :child 0 :child]))
(on-length (get-in -tree2 [0 :child 0 :child]))

(defn on-area [node]
  (reduce + (map (fn [{from :at child :child} {to :at}]
                   (if child (* (- to from) (on-length child)) 0))
                 node (drop 1 node))))

(on-area (get-in -tree [0 :child]))
(on-area (get-in -tree2 [0 :child]))

(defn on-volume [node]
  (reduce + (map (fn [{from :at child :child} {to :at}]
                   (if child (* (- to from) (on-area child)) 0))
                 node (drop 1 node))))

(on-volume -tree)
(on-volume -tree2)

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
