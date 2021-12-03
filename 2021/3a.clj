(def lines (clojure.string/split (slurp "3.txt") #"\n"))

(defn bits-for-line [line]
    (map #(= \1 %) line))

(def bits-by-line (map (comp bits-for-line reverse seq) lines))

(def bits-by-place (apply map list bits-by-line))

(defn counts-by-value [values]
    (let [grouped (group-by identity values)]
        (map count [(grouped false) (grouped true)])))

(def counts-by-bit-by-place (map counts-by-value bits-by-place))

(def gamma-bits
    (map (fn [counts] (apply < counts))
        counts-by-bit-by-place))

(def epsilon-bits (map not gamma-bits))

(defn number-for-bits [bits]
    (let [bit-vector (apply vector bits)]
        (first (reduce
            (fn [[sum power], index]
                [(+ sum (if (bit-vector index) power 0)) (* power 2)])
            [0 1]
            (range (count bit-vector))))))

(apply * (map number-for-bits [gamma-bits epsilon-bits]))
