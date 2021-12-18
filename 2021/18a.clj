(require '[clojure.string :as string])
(require '[clojure.edn :as edn])
(require '[clojure.walk :as walk])

(defn path-to-explode
  ([form] (path-to-explode form 0))
  ([form depth]
   (cond (number? form) nil
         (= depth 4) ()
         :else (->> form
                    (map-indexed
                     (fn [index item]
                       (if-let [path (path-to-explode item (inc depth))]
                         (conj path index)
                         nil)))
                    (some identity)))))

(defn inc-path
  ([path]
   (if-let [incremented (inc-path (vec path) (dec (count path)))]
     (conj incremented 0)
     nil))
  ([path place]
   (cond (neg? place) nil
         (zero? (path place)) (assoc path place 1)
         :else (inc-path (assoc path place 0) (dec place)))))

(defn dec-path
  ([path]
   (if-let [decremented (dec-path (vec path) (dec (count path)))]
     (conj decremented 1)
     nil))
  ([path place]
   (cond (neg? place) nil
         (pos? (path place)) (assoc path place 0)
         :else (dec-path (assoc path place 1) (dec place)))))

(defn add-on-path [form path value]
  (cond (empty? path) form
        (number? (get-in form path)) (update-in form path (partial + value))
        :else (add-on-path form (drop-last path) value)))

(defn explode [form path]
  (let [[left right] (get-in form path)]
    (-> form
        (assoc-in path 0)
        (add-on-path (dec-path path) left)
        (add-on-path (inc-path path) right))))

(defn path-to-split [form]
  (if (number? form)
    (if (<= 10 form) () nil)
    (->> form
         (map-indexed
          (fn [index item]
            (if-let [path (path-to-split item)]
              (conj path index)
              nil)))
         (some identity))))

(defn split [form path]
  (update-in form path
             (fn [value] (let [left (quot value 2)]
                           [left (- value left)]))))

(defn simplify [form]
  (if-let [path (path-to-explode form)]
    (simplify (explode form path))
    (if-let [path (path-to-split form)]
      (simplify (split form path))
      form)))

(defn add [left right] (simplify [left right]))

(defn magnitude-step [form]
  (if (number? form) form
      (+ (* 3 (first form))
         (* 2 (second form)))))

(def magnitude (partial walk/postwalk magnitude-step))

(time (->> (slurp "18.txt")
           (string/split-lines)
           (map edn/read-string)
           (reduce add)
           magnitude))
