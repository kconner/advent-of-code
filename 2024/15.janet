(use ./tools)

(defn model-from-file [path]
  (let [input (slurp path)
        dimension (string/find "\n" input)
        wrap (inc dimension)
        char-position (fn [index] [(mod index wrap) (div index wrap)])
        start (char-position (string/find "@" input))]
    (first (peg/match
             ~{:main (/ (* :grid :steps)
                        ,|{:grid $0 :start start :steps $1})
               :grid (/ (some (choice (* :position :object) "\n")) ,table)
               :object (/ (<- (choice "#" "." "O" "@"))
                          ,|(match $ "#" :wall "O" :box _ nil))
               :position (/ ($) ,char-position)
               :steps (/ (some (choice :step "\n")) ,tuple)
               :step (/ (<- (choice "<" ">" "^" "v"))
                        ,|(match $ "<" [-1 0] ">" [1 0] "^" [0 -1] "v" [0 1]))}
             input))))

(defn find-space-beyond-boxes [grid start step]
  (var square start)
  (while (= (grid square) :box)
    (set square [;(map + square step)]))
  (match (grid square)
    :wall nil
    nil square
    _ (do (pp "assertion failed") "WAT")))

(defn try-step [grid robot step]
  (def robot-after-step [;(map + robot step)])
  (match (grid robot-after-step)
    nil robot-after-step
    :wall robot
    :box (match (find-space-beyond-boxes grid [;(map + robot step)] step)
           nil robot
           square (do
                    (set (grid square) :box)
                    (set (grid robot-after-step) nil)
                    robot-after-step))))

(defn gps-of [[x y]]
  (+ x (* 100 y)))

(defn problem1 [{:grid grid :start start :steps steps}]
  (reduce |(try-step grid $0 $1) start steps)
  (+ ;(map gps-of (filter |(= (grid $) :box) (keys grid)))))

(defn problem2 [model])

(defn main [&]
  (spork/test/timeit
    (do
      (def path "15.txt")
      # (def path "15.test.txt")
      (def model (model-from-file path))
      (print (problem1 model))
      (print (problem2 model)))))
