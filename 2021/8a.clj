(->> (slurp "8.txt")
     clojure.string/split-lines
     (map (fn [line] (clojure.string/split line #" \| ")))
     (map second)
     (mapcat (fn [line] (for [word (clojure.string/split line #" ")] (count word))))
     (filter #{2 3 4 7})
     count)