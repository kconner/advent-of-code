(def paragraphs
  (clojure.string/split (slurp "4.txt") #"\s*\n\n\s*"))

(def draw-order
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

boards