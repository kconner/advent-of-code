(require '[clojure.string :as string])

; The lines are the file's text split by newline.

(def lines (string/split-lines (slurp "14.txt")))

; The relationships are a map of ordered pairs of parent characters
; to child characters, one relationship per line of text,
; not counting the first two.

(defn map-with-relationship-for-line [map line]
  (assoc map [(get line 0) (get line 1)] (get line 6)))

(def relationships
  (reduce map-with-relationship-for-line {} (drop 2 lines)))

; The child frequencies for a given depth and ordered parent characters are
; the value-summing merge of maps of child frequencies of the two child pairs
; of characters given by the relationship of the parents to the middle child,
; plus one for the visited child, until the depth limit.

; Memoizing reuses work when any parent character pair occurs more than once
; at the same depth.

(def child-frequencies
  (memoize
   (fn [depth left right]
     (if (zero? depth) {}
         (let [child (relationships [left right])
               depth (dec depth)]
           (merge-with +
                       {child 1}
                       (child-frequencies depth left child)
                       (child-frequencies depth child right)))))))

; The total character frequencies are the value-summing merge of maps of
; the frequencies of the input string and the child frequencies of each pair
; of parent characters in the input string.

(def character-frequencies
  (let [input (first lines)]
    (apply merge-with +
           (frequencies input)
           (map (partial child-frequencies 40) input (drop 1 input)))))

; The answer is the difference between the extremes of the map's values.

(let [counts (vals character-frequencies)]
  (- (apply max counts) (apply min counts)))
