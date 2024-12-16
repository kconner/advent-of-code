(use ./tools)

(defn model-from-file [path]
  (let [input (slurp path)
        dimension (string/find "\n" input)
        wrap (inc dimension)
        char-position (fn [index] [(mod index wrap) (div index wrap)])
        start (char-position (string/find "S" input))
        end (char-position (string/find "E" input))
        grid (->> input
                  (peg/match
                    ~{:main (/ (some (choice (* :position :space) "#" "\n")) ,table)
                      :space (* (constant true) (choice "." "S" "E"))
                      :position (/ ($) ,char-position)})
                  (first))]
    {:grid grid
     :start [[start [1 0]] @{start true}]
     :end end}))

(def move-options
  (let [e [1 0] w [-1 0] n [0 -1] s [0 1] stay [0 0]]
    {e [[1 e e] [1000 stay n] [1000 stay s]]
     w [[1 w w] [1000 stay n] [1000 stay s]]
     n [[1 n n] [1000 stay e] [1000 stay w]]
     s [[1 s s] [1000 stay e] [1000 stay w]]}))

(defn next-scored-moves [grid score [[pos dir] squares]]
  (->> (move-options dir)
       (map (fn [[addend step new-dir]]
              [(+ score addend) [[;(map + pos step)] new-dir]]))
       (filter (fn [[_ [p _]]] (grid p)))
       (map (fn [[s [p d]]]
              (def new-squares (table/clone squares))
              (set (new-squares p) true)
              [s [[p d] new-squares]]))))

(defn bfs [{:grid grid :start start :end end}]
  (def score-queue @[]) # score, ordered ascending
  (def squares-by-move-by-score @{}) # score: table of [pos dir]: squares

  (defn dequeue-bucket []
    (def score (score-queue 0))
    (array/remove score-queue 0)
    (def squares-by-move (squares-by-move-by-score score))
    (set (squares-by-move-by-score score) nil) # Unnecessary but helps speed
    [score squares-by-move])

  (defn enqueue-scored-move [score [move squares]]
    (var squares-by-move (squares-by-move-by-score score))
    (if (nil? squares-by-move)
      (do
        (set squares-by-move @{move squares})
        (set (squares-by-move-by-score score) squares-by-move)
        # Could binary search and insert instead to reduce by O(n)
        (array/push score-queue score)
        (sort score-queue))
      (do
        (def existing-squares (squares-by-move move))
        (if (nil? existing-squares)
          (set (squares-by-move move) squares)
          (merge-into existing-squares squares)))))

  (enqueue-scored-move 0 start)

  (def visited @{}) # [pos dir]: true
  (prompt 'out
    (while true
      (def [score squares-by-move] (dequeue-bucket))
      (eachp [move squares] squares-by-move
        (when (not (visited move))
          (set (visited move) true)
          (when (= (first move) end)
            (return 'out [score squares]))
          (each [next-score next-move] (next-scored-moves grid score [move squares])
            (enqueue-scored-move next-score next-move)))))))

(defn problem1 [[score _]]
  score)

(defn problem2 [[_ squares]]
  (length squares))

(defn main [&]
  (spork/test/timeit
    (do
      (def path "16.txt")
      # (def path "16.test.txt")
      # (def path "16.test2.txt")
      (def model (model-from-file path))
      (def result (bfs model))
      (print (problem1 result))
      (print (problem2 result)))))
