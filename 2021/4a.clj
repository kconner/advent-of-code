(def paragraphs
  (clojure.string/split (slurp "4.txt") #"\s*\n\n\s*"))

(def draws
  (into () (map #(Integer. %))
        (clojure.string/split (first paragraphs) #",")))

(defn square-for-string [string]
  {:value (Integer. string), :marked false})

(defn row-from-line [line]
  (into [] (map square-for-string)
        (clojure.string/split line #"\s+")))

(defn board-from-paragraph [paragraph]
  (into [] (map row-from-line)
        (clojure.string/split paragraph #"\s*\n\s*")))

(def boards
  (into [] (map board-from-paragraph)
        (rest paragraphs)))

(def indices ((comp range count first) boards))

(defn has-board-won-by-axis [board make-key-path]
  (some (fn [major]
          (every? (fn [minor]
                    (get-in board (conj (make-key-path major minor) :marked)))
                  indices))
        indices))

(defn has-board-won [board]
  (or (has-board-won-by-axis board (fn [major minor] [major minor]))   ;; row
      (has-board-won-by-axis board (fn [major minor] [minor major])))) ;; column

;; (defn find-winning-board [boards draws]
;;   (if-let (some has-board-won boards)


(def aboard
  (-> boards
      (update-in [0 1 0 :marked] (constantly true))
      (update-in [0 1 2 :marked] (constantly true))
      (update-in [0 1 2 :marked] (constantly true))
      (update-in [0 1 3 :marked] (constantly true))
      (update-in [0 1 4 :marked] (constantly true))))

aboard

(has-board-won (first aboard))

;; (get-in (first aboard) [0 0 :marked])
