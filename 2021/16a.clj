(require '[clojure.walk :as walk])

(def bits-for-character
  {\0 '(0 0 0 0) \1 '(0 0 0 1) \2 '(0 0 1 0) \3 '(0 0 1 1)
   \4 '(0 1 0 0) \5 '(0 1 0 1) \6 '(0 1 1 0) \7 '(0 1 1 1)
   \8 '(1 0 0 0) \9 '(1 0 0 1) \A '(1 0 1 0) \B '(1 0 1 1)
   \C '(1 1 0 0) \D '(1 1 0 1) \E '(1 1 1 0) \F '(1 1 1 1)})

(defn bits-for-text [text]
  (mapcat bits-for-character text))

(defn read-number
  ([state count]
   (read-number state 0 count))
  ([[offset bits] sum count]
   (if (zero? count) [[offset bits] sum]
       (read-number [(inc offset) (rest bits)]
                    (+ sum sum (first bits))
                    (dec count)))))

(defn read-flag [state]
  (let [[state value] (read-number state 1)]
    [state (= value 1)]))

(defn read-literal-value
  ([state]
   (read-literal-value state 0))
  ([state sum]
   (let [[state continue] (read-flag state)
         [state value] (read-number state 4)
         sum (+ (bit-shift-left sum 4) value)]
     (if-not continue [state sum]
             (read-literal-value state sum)))))

(defn read-literal-packet [state]
  (let [[state value] (read-literal-value state)]
    [state {:value value}]))

(declare read-packet)

(defn read-packets-by-count
  ([state]
   (let [[state count] (read-number state 11)]
     (read-packets-by-count state [] count)))
  ([state packets count]
   (if (zero? count) [state packets]
       (let [[state packet] (read-packet state)]
         (read-packets-by-count state (conj packets packet) (dec count))))))

(defn read-packets-by-length
  ([state]
   (let [[state length] (read-number state 15)]
     (read-packets-by-length state [] (+ (first state) length))))
  ([state packets end]
   (if (<= end (first state)) [state packets]
       (let [[state packet] (read-packet state)]
         (read-packets-by-length state (conj packets packet) end)))))

(defn read-operator-packet [state]
  (let [[state use-count] (read-flag state)
        [state operands] (if use-count
                           (read-packets-by-count state)
                           (read-packets-by-length state))]
    [state {:operands operands}]))

(defn read-packet [state]
  (let [[state version] (read-number state 3)
        [state type] (read-number state 3)
        [state packet] (case type
                         4 (read-literal-packet state)
                         (read-operator-packet state))]
    [state (assoc packet :version version)]))

(defn with-state [reader bits & args]
  (second (apply reader [0 bits] args)))

(defn version-sum [packet]
  (walk/postwalk
   (fn [item]
     (if-let [version (:version item)]
       (apply + version (:operands item))
       item))
   packet))

(time (version-sum (with-state read-packet (bits-for-text (slurp "16.txt")))))
