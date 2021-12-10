(def closers {\( \), \[ \], \{ \}, \< \>})
(def scores {\) 1, \] 2, \} 3, \> 4})

(defn stack-or-invalid-character [stack character]
  (if (char? stack) stack
      (if-let [closer (closers character)]
        (conj stack closer)
        (let [[head & tail] stack]
          (if (= head character) tail character)))))

(defn score-remainder [sum character]
  (+ (* 5 sum) (scores character)))

(->> (slurp "10.txt")
     clojure.string/split-lines
     (map (partial reduce stack-or-invalid-character ()))
     (remove char?)
     (map (partial reduce score-remainder 0))
     sort
     (#(nth % (/ (count %) 2))))
