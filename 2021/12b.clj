(require '[clojure.string :as string])

(defn is-big [node]
  (pos? (compare \a (first node))))

(defn can-visit [visits node]
  (and (not= "start" node)
       (or (is-big node) (not (visits node)) (not (visits :revisit)))))

(defn visit [visits node]
  (if (or (is-big node) (not (visits node))) node :revisit))

(defn count-paths [graph visits current]
  (let [visits (conj visits (visit visits current))]
    (if (= current "end") 1
        (->> (graph current)
             (filter (partial can-visit visits))
             (map (partial count-paths graph visits))
             (reduce +)))))

(def graph
  (->> (slurp "12.txt")
       string/split-lines
       (map #(string/split % #"-"))
       (map (fn [[a b]] {a (list b), b (list a)}))
       (apply merge-with concat)))

(count-paths graph #{} "start")
