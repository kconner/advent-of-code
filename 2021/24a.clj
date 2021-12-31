(require '[clojure.string :as string])

(def instruction-regex #"([a-z]{3}) ([w-z])( (([w-z])|(-?\d+)))?")

(defn parse [line]
  (let [[_ op dest _ _ src lit] (re-matches instruction-regex line)
        dest (keyword dest)
        src (keyword src)
        lit (when lit (Integer. lit))]
    [op dest src lit]))

;; Assumes the very first instruction is inp.
(defn split-steps [[inp & instructions]]
  (if (empty? instructions) ()
      (let [[step more] (split-with (fn [[op]] (not= op "inp")) instructions)]
        (conj (split-steps more)
              (conj step inp)))))

(defn do-inp [dest state digit]
  (assoc state dest digit))

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

(do-inp :w {:w 5} 3)
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

(defn assemble-step [instructions]
  (->> instructions
       (map assemble)
       reverse
       (apply comp)))

;; when out of steps to run, if z is zero, return the number, else nil
;; when there are steps to run, run the next step with every digit
;; if the result was successful, conj digits on the way back up
(defn search [state [step & steps]]
  (if (nil? step) (when (zero? (:z state)) ())
      (some (fn [digit]
              (try (let [state (step state digit)
                         result (search state steps)]
                     (when result (conj result digit)))
                   (catch Exception _ nil)))
            (range 9 0 -1))))

(defn initial-state [digits]
  {:w 0 :x 0 :y 0 :z 0 :digits digits :initial-digits digits})

(time (let [instructions (map parse (string/split-lines (slurp "24.txt")))
            steps (map assemble-step (split-steps instructions))
            initial-state {:w 0 :x 0 :y 0 :z 0}]
        (search initial-state steps)))
