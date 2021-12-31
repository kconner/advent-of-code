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
  (update state dest (partial / lit)))

(defn do-div-reg [dest src state]
  (update state dest (partial / (state src))))

(defn do-mod-lit [dest lit state]
  (update state dest (partial mod lit)))

(defn do-mod-reg [dest src state]
  (update state dest (partial mod (state src))))

(defn do-eql-lit [dest lit state]
  (update state dest (fn [value] (if (= lit value) 1 0))))

(defn do-eql-reg [dest src state]
  (update state dest (fn [value] (if (= (state src) value) 1 0))))

(def instruction-regex #"([a-z]{3}) ([w-z])( (([w-z])|(-?\d+)))?")

(defn instruction [line]
  (let [[_ op dest _ _ src lit] (re-matches instruction-regex line)
        dest (keyword dest)
        src (keyword src)
        lit (when lit (Integer. lit))]
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
                (partial do-eql-reg dest src)))))

(defn digits []
  (let [ds (range 9 0 -1)]
    (for [d0 ds d1 ds d2 ds d3 ds d4 ds
          d5 ds d6 ds d7 ds d8 ds d9 ds
          d10 ds d11 ds d12 ds d13 ds]
      (list d0 d1 d2 d3 d4 d5 d6 d7 d8 d9 d10 d11 d12 d13))))

(defn initial-state [digits]
  {:w 0 :x 0 :y 0 :z 0 :digits digits})

(time (let [program (->> (slurp "24.txt")
                         string/split-lines
                         (map instruction)
                         (apply comp))]
        (->> (digits)
             (map initial-state)
             (keep (fn [state]
                     (try (program state)
                          (catch Exception _ nil))))
             first)))
