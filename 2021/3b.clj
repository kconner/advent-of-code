(def lines (clojure.string/split (slurp "3.txt") #"\n"))

(defn bits-for-line [line]
  (into [] (map #(= \1 %)) line))

(def bit-lines (map bits-for-line lines))

(defn find-rating-bits [condition bit-lines places]
  (cond
    (empty? places) bit-lines
    (= 1 (count bit-lines)) bit-lines
    :else (let
           [place (first places)
            grouped-bits (group-by identity (map #(% place) bit-lines))
            required-bit (apply condition
                                (map count [(grouped-bits false) (grouped-bits true)]))]
            (find-rating-bits
             condition
             (filter (fn [line] (= required-bit (line place))) bit-lines)
             (rest places)))))

(def ratings-bits
  (map
   (fn [condition]
     (first (find-rating-bits condition bit-lines
                              ((comp range count first) bit-lines))))
   [<= >]))

(defn number-for-bits [bits]
  (let [bit-vector (apply vector (reverse bits))]
    (first (reduce
            (fn [[sum power], index]
              [(+ sum (if (bit-vector index) power 0)) (* power 2)])
            [0 1]
            (range (count bit-vector))))))

(apply * (map number-for-bits ratings-bits))
