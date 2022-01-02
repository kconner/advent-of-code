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

(defn asm-inp [dest state]
  (assoc state dest 'user/digit))

(defn asm-add-lit [dest lit state]
  (case lit
    0 state
    (update state dest (fn [exp]
                         (if (number? exp) (+ exp lit)
                             `(+ ~exp ~lit))))))

(defn asm-add-reg [dest src state]
  (update state dest (fn [exp]
                       (if (= 0 exp) (state src)
                           `(+ ~exp ~(state src))))))

(defn asm-mul-lit [dest lit state]
  (case lit
    0 (assoc state dest lit)
    1 state
    (update state dest (fn [exp]
                         (if (number? exp) (* exp lit)
                             `(* ~exp ~lit))))))

(defn asm-mul-reg [dest src state]
  (update state dest (fn [exp] `(* ~exp ~(state src)))))

(defn asm-div-lit [dest lit state]
  (case lit
    1 state
    (update state dest (fn [exp]
                         (if (number? exp) (quot exp lit)
                             `(quot ~exp ~lit))))))

(defn asm-div-reg [dest src state]
  (update state dest (fn [exp] `(quot ~exp ~(state src)))))

(defn asm-mod-lit [dest lit state]
  (update state dest (fn [exp]
                       (if (number? exp) (mod exp lit)
                           `(mod ~exp ~lit)))))

(defn asm-mod-reg [dest src state]
  (update state dest
          (fn [exp]
            `(let [dividend ~exp
                   divisor ~(state src)]
               (if (or (< exp 0) (<= divisor 0))
                 (throw (ArithmeticException.))
                 (mod exp divisor))))))

(defn asm-eql-lit [dest lit state]
  (update state dest (fn [exp]
                       (if (and (zero? lit)
                                (coll? exp)
                                (= 'if (first exp)))
                         (let [[op condition then else] exp]
                           `(~op ~condition ~else ~then))
                         `(if (= ~exp ~lit) 1 0)))))

(defn asm-eql-reg [dest src state]
  (update state dest (fn [exp] `(if (= ~exp ~(state src)) 1 0))))

(defn assemble [state [op dest src lit]]
  (case op
    "inp" (asm-inp dest state)
    "add" (if lit (asm-add-lit dest lit state)
              (asm-add-reg dest src state))
    "mul" (if lit (asm-mul-lit dest lit state)
              (asm-mul-reg dest src state))
    "div" (if lit (asm-div-lit dest lit state)
              (asm-div-reg dest src state))
    "mod" (if lit (asm-mod-lit dest lit state)
              (asm-mod-reg dest src state))
    "eql" (if lit (asm-eql-lit dest lit state)
              (asm-eql-reg dest src state))))

(defn assemble-step [instructions]
  `(fn [z digit]
     ~(:z (reduce assemble '{:z user/z} instructions))))

(defn compile-step [instructions]
  (memoize (eval (assemble-step instructions))))

;; when out of steps to run, if z is zero, return the number, else nil
;; when there are steps to run, run the next step with every digit
;; if the result was successful, conj digits on the way back up
(defn search [z [step & steps]]
  (if (nil? step) (when (zero? z) ())
      (some (fn [digit]
              (try (let [z (step z digit)
                         result (search z steps)]
                     (when result (conj result digit)))
                   (catch Exception _ nil)))
            (range 9 0 -1))))

(time (let [instructions (map parse (string/split-lines (slurp "24.txt")))
            steps (map compile-step (split-steps instructions))
            initial-z 0]
        (search 0 steps)))

(comment
  (time (let [instructions (map parse (string/split-lines (slurp "24.txt")))
              steps (map assemble-step (split-steps instructions))
              initial-z 0]
          (first steps))))
