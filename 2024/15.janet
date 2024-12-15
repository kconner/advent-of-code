(use ./tools)

(defn model-from-file [path]
  (let [input (slurp path)
        dimension (string/find "\n" input)
        wrap (inc dimension)
        char-position (fn [index] [(mod index wrap) (div index wrap)])
        start (char-position (string/find "@" input))]
    (first (peg/match
             ~{:main (/ (* :grid :steps)
                        ,|{:dimension dimension :grid $0 :start start :steps $1})
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

(defn widen [dimension grid]
  (def wide-grid @{})
  (for x 0 dimension
    (def x1 (* 2 x))
    (def x2 (inc x1))
    (for y 0 dimension
      (match (grid [x y])
        :wall (do
                (set (wide-grid [x1 y]) :wall)
                (set (wide-grid [x2 y]) :wall))
        :box (do
               (set (wide-grid [x1 y]) :box-left)
               (set (wide-grid [x2 y]) :box-right))
        _ (do))))
  wide-grid)

(defn string-of [[dim-x dim-y] grid]
  (def characters @[])
  (each y (range dim-y)
    (array/push characters
                ;(map |(match (grid [$ y])
                         :wall "#"
                         :box "O"
                         :box-left "["
                         :box-right "]"
                         nil ".")
                      (range dim-x)) "\n"))
  (string/join characters))

# (print (string-of [(model :dimension) (model :dimension)] (model :grid)))

# (print (string-of [(* 2 (model :dimension)) (model :dimension)]
#                   (widen (model :dimension) (model :grid))))

(defn try-step-2 [grid robot step]
  # ok now what
)

(defn problem2 [{:dimension dimension :grid grid :start [sx sy] :steps steps}]
  (def wide-grid (widen dimension grid))
  (def wide-start [(* 2 sx) sy])
  (reduce |(try-step-2 wide-grid $0 $1) wide-start steps)
  (+ ;(map gps-of (filter |(= (wide-grid $) :box) (keys wide-grid)))))

(defn main [&]
  (spork/test/timeit
    (do
      (def path "15.txt")
      (def path "15.test.txt")
      (def model (model-from-file path))
      (print (problem1 model))
      (print (problem2 model)))))
