(require '[clojure.string :as string])
(require '[clojure.set :as set])

(defn beacon-for-line [line]
  (->> line
       (re-matches #"(-?\d+),(-?\d+),(-?\d+)")
       rest
       (mapv #(Integer. %))))

(defn scanner-for-paragraph [paragraph]
  (->> paragraph string/split-lines rest (map beacon-for-line)))

(defn scanners-for-text [text]
  (map scanner-for-paragraph (string/split text #"\n\n")))

(defn beacon-rotations [[a b c]]
  (let [na (- a) nb (- b) nc (- c)]
    [[a b c] [a nb nc] [na b nc] [na nb c]
     [b c a] [b nc na] [nb c na] [nb nc a]
     [c a b] [c na nb] [nc a nb] [nc na b]
     [a c nb] [a nc b] [na c b] [na nc nb]
     [b a nc] [b na c] [nb a c] [nb na nc]
     [c b na] [c nb a] [nc b a] [nc nb na]]))

(defn scanner-rotations [scanner]
  (apply map vector (map beacon-rotations scanner)))

(defn oriented-matching-scanner [basis-scanner scanner-rotation]
  (some (fn [os] (when (<= 12 (count (filter basis-scanner os))) os))
        (for [bb basis-scanner rb scanner-rotation :let [offset (mapv - bb rb)]]
          (mapv (partial map + offset) scanner-rotation))))

(defn oriented-matching-scanner-2 [basis-scanner scanner-rotation]
  (some (fn [[offset count]]
          (when (<= 12 count)
            (mapv (partial map + offset) scanner-rotation)))
        (frequencies (for [bb basis-scanner rb scanner-rotation]
                       (mapv - bb rb)))))

(defn next-match-among-all [oriented-scanners remaining-rotated-scanners]
  (first (for [[index rotated-scanner]
               (map-indexed vector remaining-rotated-scanners)
               basis-scanner oriented-scanners
               :let [basis-set (set basis-scanner)]
               scanner-rotation rotated-scanner
               :let [match (oriented-matching-scanner-2 basis-set scanner-rotation)]
               :when match]
           [match (vec (concat (subvec remaining-rotated-scanners (inc index))
                               (subvec remaining-rotated-scanners 0 index)))])))

(defn mutually-oriented-scanners [scanners]
  (loop [oriented-scanners [(first scanners)]
         remaining-rotated-scanners (mapv scanner-rotations (rest scanners))]
    (if (empty? remaining-rotated-scanners) oriented-scanners
        (let [[match remainder]
              (time (next-match-among-all oriented-scanners remaining-rotated-scanners))]
          (recur (conj oriented-scanners match) remainder)))))

(time (->> (slurp "19.txt")
           scanners-for-text
           mutually-oriented-scanners
           (map set)
           (apply set/union)
           count))
