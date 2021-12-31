(require '[clojure.string :as string])

(defn do-inp [dest state]
  (let [[digit & digits] (state :digits)]
    (-> state
        (assoc :digits digits)
        (assoc dest digit))))

(defn do-add-lit [dest lit state]
  (update state dest (partial + lit)))

(defn do-add-reg [dest src state]
  (update state dest (partial + (state src))))

(defn do-mul-lit [dest lit state]
  (update state dest (partial * lit)))

(defn do-mul-reg [dest src state]
  (update state dest (partial * (state src))))

(defn do-div-lit [dest lit state]
  (update state dest (fn [dividend] (quot dividend lit))))

(defn do-div-reg [dest src state]
  (update state dest (fn [dividend] (quot dividend (state src)))))

(defn do-mod-lit [dest lit state]
  (update state dest (fn [dividend] (mod dividend lit))))

(defn do-mod-reg [dest src state]
  (update state dest
          (fn [dividend]
            (let [divisor (state src)]
              (if (or (< dividend 0) (<= divisor 0))
                (throw (ArithmeticException.))
                (mod dividend divisor))))))

(defn do-eql-lit [dest lit state]
  (update state dest (fn [value] (if (= lit value) 1 0))))

(defn do-eql-reg [dest src state]
  (update state dest (fn [value] (if (= (state src) value) 1 0))))

(do-inp :w {:w 5 :digits '(3 2 1)})
(do-add-lit :w 3 {:w 5})
(do-add-reg :w :x {:w 5 :x 3})
(do-mul-lit :w 3 {:w 5})
(do-mul-reg :w :x {:w 5 :x 3})
(do-div-lit :w 5 {:w 13})
(do-div-reg :w :x {:w 13 :x 5})
(do-mod-lit :w 5 {:w 13})
(do-mod-reg :w :x {:w 13 :x 5})
(do-eql-lit :w 3 {:w 5})
(do-eql-reg :w :x {:w 5 :x 3})

(def instruction-regex #"([a-z]{3}) ([w-z])( (([w-z])|(-?\d+)))?")

(defn parse [line]
  (let [[_ op dest _ _ src lit] (re-matches instruction-regex line)
        dest (keyword dest)
        src (keyword src)
        lit (when lit (Integer. lit))]
    [op dest src lit]))

(defn assemble [[op dest src lit]]
  (case op
    "inp" (partial do-inp dest)
    "add" (if lit (partial do-add-lit dest lit)
              (partial do-add-reg dest src))
    "mul" (if lit (partial do-mul-lit dest lit)
              (partial do-mul-reg dest src))
    "div" (if lit (partial do-div-lit dest lit)
              (partial do-div-reg dest src))
    "mod" (if lit (partial do-mod-lit dest lit)
              (partial do-mod-reg dest src))
    "eql" (if lit (partial do-eql-lit dest lit)
              (partial do-eql-reg dest src))))

(defn digits [depth]
  (if (zero? depth) '(())
      (let [child (digits (dec depth))]
        (for [d (range 9 0 -1)
              c child]
          (conj c d)))))

(defn initial-state [digits]
  {:w 0 :x 0 :y 0 :z 0 :digits digits :initial-digits digits})

(time (let [instructions (->> (slurp "24.txt")
                              string/split-lines
                              (map parse))
            inp-count (count (filter (fn [[op]] (= op "inp")) instructions))
            program (->> instructions
                         (map assemble)
                         reverse
                         (apply comp))
            initial-state {:w 0 :x 0 :y 0 :z 0}]
        (->> (digits inp-count)
             (take 1000000)
             (map initial-state)
             (keep (fn [state]
                     (try (program state)
                          (catch Exception _ nil))))
             (filter (comp zero? :z))
             first)))
