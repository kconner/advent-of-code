(require '[clojure.string :as string])

(defn is-big [node]
  (pos? (compare \a (first node))))

(defn is-big-or-unvisited [visited node]
  (or (is-big node) (not (visited node))))

(defn count-paths [graph visited current]
  (let [visited (conj visited current)]
    (if (= current "end") 1
        (->> (graph current)
             (filter (partial is-big-or-unvisited visited))
             (map (partial count-paths graph visited))
             (reduce +)))))

(def graph
  (->> (slurp "12.txt")
       string/split-lines
       (map #(string/split % #"-"))
       (map (fn [[a b]] {a (list b), b (list a)}))
       (apply merge-with concat)))

(count-paths graph #{} "start")
