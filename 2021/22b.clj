(require '[clojure.string :as string])

(def step-regex
  #"(on|off) x=(-?\d+)..(-?\d+),y=(-?\d+)..(-?\d+),z=(-?\d+)..(-?\d+)")

(defn step-from-line [line]
  (let [[_ value minx maxx miny maxy minz maxz] (re-matches step-regex line)]
    {:from (Integer. minx) :to (inc (Integer. maxx))
     :step {:from (Integer. miny) :to (inc (Integer. maxy))
            :step {:from (Integer. minz) :to (inc (Integer. maxz))
                   :value (= "on" value)}}}))

; could binary search instead.
(defn cut [node edge]
  (let [extended (concat [{:at ##-Inf}] node [{:at ##Inf}])]
    (->> [extended (drop 1 extended)]
         (apply mapcat
                (fn [{from :at :as interval} {to :at}]
                  (if (< from edge to)
                    [interval (assoc interval :at edge)]
                    [interval])))
         (drop 1)
         vec)))

(def insert-z
  (memoize
   (fn [node {from :from to :to new-value :value}]
     (map (fn [{at :at value :value :as interval}]
            (if (and (nil? value) (<= from at) (< at to))
              (assoc interval :value new-value)
              interval))
          (cut (cut node from) to)))))

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

(defn on-length [node]
  (reduce + (map (fn [{from :at value :value} {to :at}]
                   (if value (- to from) 0))
                 node (drop 1 node))))

(defn on-area [node]
  (reduce + (map (fn [{from :at child :child} {to :at}]
                   (if child (* (- to from) (on-length child)) 0))
                 node (drop 1 node))))

(defn on-volume [node]
  (reduce + (pmap (fn [{from :at child :child} {to :at}]
                    (if child (* (- to from) (on-area child)) 0))
                  node (drop 1 node))))

(time (->> (slurp "22.txt")
           string/split-lines
           (map step-from-line)
           (reverse)
           (reduce insert-x [])
           (on-volume)))
