(require '[clojure.string :as string])
(require '[clojure.set :as set])

(defn is-big [node]
  (pos? (compare \a (first node))))

(defn is-big-or-unvisited [visited node]
  (or (is-big node) (not (visited node))))

(defn find-paths [graph visited current]
  (let [visited (conj visited current)]
    (if (= current "end")
      (list visited)
      (mapcat (fn [next] (find-paths graph visited next))
              (filter (partial is-big-or-unvisited visited) (graph current))))))

(def graph
  (->> (slurp "12.txt")
       string/split-lines
       (map #(string/split % #"-"))
       (map (fn [[a b]] {a (list b) b (list a)}))
       (apply merge-with concat)))

(count (find-paths graph #{} "start"))
