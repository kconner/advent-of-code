(def closers {\( \), \[ \], \{ \}, \< \>})
(def scores {\) 3, \] 57, \} 1197, \> 25137})

(defn stack-or-invalid-character [stack character]
  (if (char? stack) stack
      (if-let [closer (closers character)]
        (conj stack closer)
        (let [[head & tail] stack]
          (if (= head character) tail character)))))

(->> (slurp "10.txt")
     clojure.string/split-lines
     (map (partial reduce stack-or-invalid-character ()))
     (filter char?)
     (map scores)
     (reduce +))
