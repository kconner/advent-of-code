(def lines (clojure.string/split (slurp "3.txt") #"\n"))

(defn bits-for-line [line]
    (into [] (map #(= \1 %)) line))

(def bit-lines (map bits-for-line lines))

(defn counts-by-value [values]
    (let [grouped (group-by identity values)]
        (map count [(grouped false) (grouped true)])))

(defn find-rating [condition bit-lines places]
    (cond
        (empty? places) bit-lines
        (= 1 (count bit-lines)) bit-lines
        :else (let
            [place (first places)
             required-bit (apply condition
                (counts-by-value (map #(% place) bit-lines)))]
            (find-rating
                condition
                (filter #(= required-bit (% place)) bit-lines)
                (rest places)))))

(def ratings
    (map
        (fn [condition]
            (first (find-rating condition bit-lines
                ((comp range count first) bit-lines))))
        [<= >]))

(defn number-for-bits [bits]
    (let [bit-vector (apply vector (reverse bits))]
        (first (reduce
            (fn [[sum power], index]
                [(+ sum (if (bit-vector index) power 0)) (* power 2)])
            [0 1]
            (range (count bit-vector))))))

(apply * (map number-for-bits ratings))
