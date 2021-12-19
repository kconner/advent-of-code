(require '[clojure.string :as string])
(require '[clojure.set :as set])

(defn beacon-for-line [line]
  (->> line
       (re-matches #"(-?\d+),(-?\d+),(-?\d+)")
       rest
       (map #(Integer. %))))

(defn scanner-for-paragraph [paragraph]
  (->> paragraph string/split-lines rest (map beacon-for-line)))

(defn scanners-for-text [text]
  (map scanner-for-paragraph (string/split text #"\n\n")))

(defn beacon-rotations [[a b c]]
  (let [na (- a) nb (- b) nc (- c)]
    [[a b c] [a nb nc] [na b nc] [na nb c]
     [b c a] [b nc na] [nb c na] [nb nc a]
     [c a b] [c na nb] [nc a nb] [nc nb a]
     [a c nb] [a nc b] [na c b] [na nc nb]
     [b a nc] [b na c] [nb a c] [nb na nc]
     [c b na] [c nb a] [nc b a] [nc nb na]]))

(defn scanner-rotations [scanner]
  (apply map list (map beacon-rotations scanner)))

(defn oriented-matching-scanner [basis-scanner scanner-rotation]
  (some (fn [os] (when (<= 12 (count (filter basis-scanner os))) os))
        (for [bb basis-scanner rb scanner-rotation :let [offset (map - bb rb)]]
          (map (partial map + offset) scanner-rotation))))

(defn next-match-among-all [oriented-scanners remaining-rotated-scanners]
  (first (for [[index rotated-scanner]
               (map-indexed vector remaining-rotated-scanners)
               basis-scanner oriented-scanners
               :let [basis-set (set basis-scanner)]
               scanner-rotation rotated-scanner
               :let [match (oriented-matching-scanner basis-set scanner-rotation)]
               :when match
               :let [remainder-vector (vec remaining-rotated-scanners)]]
           [match (concat (subvec remainder-vector (inc index))
                          (subvec remainder-vector 0 index))])))

(defn mutually-oriented-scanners [scanners]
  (loop [oriented-scanners [(first scanners)]
         remaining-rotated-scanners (map scanner-rotations (rest scanners))]
    (if (empty? remaining-rotated-scanners) oriented-scanners
        (let [[match remainder]
              (next-match-among-all oriented-scanners remaining-rotated-scanners)]
          (recur (conj oriented-scanners match) remainder)))))

(time (->> (slurp "19.txt")
           scanners-for-text
           mutually-oriented-scanners
           (map set)
           (apply set/union)
           count))
