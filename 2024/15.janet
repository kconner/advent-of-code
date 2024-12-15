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

(defn try-step [grid robot step kick-box?]
  # if the robot's destination square is free, step there.
  # if it's a wall, stay put.
  # if it's a box side, kick the box.
  # - if the box stack budges, step into its place.
  # - if there's no such hole, stay put.
  (def destination [;(map + robot step)])
  (match (grid destination)
    nil destination
    :wall robot
    :box (if (kick-box? grid destination step)
           destination
           robot)
    :box-left (if (kick-box? grid destination step)
                destination
                robot)
    :box-right (if (kick-box? grid destination step)
                 destination
                 robot)))

(defn find-space-beyond-boxes [grid start step]
  (var square start)
  (while (= (grid square) :box)
    (set square [;(map + square step)]))
  (match (grid square)
    :wall nil
    nil square))

(defn kick-narrow-box? [grid destination step]
  (match (find-space-beyond-boxes grid destination step)
    nil false
    space (do
            # poomp
            (set (grid space) :box)
            (set (grid destination) nil)
            true)))

(defn gps-of [[x y]]
  (+ x (* 100 y)))

(defn problem1 [{:grid grid :start start :steps steps}]
  (reduce |(try-step grid $0 $1 kick-narrow-box?) start steps)
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

(defn kick-wide-box? [grid start step]
  (def box-sides-to-move @[])
  (def box-sides-to-verify-movable @[])
  (def verified @{})
  (def enqueue-pairs (zero? (first step))) # move whole boxes during vertical kicks
  (defn enqueue [square]
    (array/push box-sides-to-verify-movable square)
    (if enqueue-pairs
      (array/push box-sides-to-verify-movable
                  [(+ (square 0) (match (grid square) :box-left 1 :box-right -1))
                   (square 1)])))
  (defn dequeue []
    (let [square (box-sides-to-verify-movable 0)]
      (array/remove box-sides-to-verify-movable 0)
      square))
  (enqueue start)
  (prompt 'out
    (while (not (empty? box-sides-to-verify-movable))
      (def square (dequeue))
      (when (not (verified square))
        (set (verified square) true)
        (array/push box-sides-to-move square)
        (def neighbor [;(map + square step)])
        (match (grid neighbor)
          nil (do)
          :wall (return 'out false)
          :box-left (enqueue neighbor)
          :box-right (enqueue neighbor))))
    (reverse! box-sides-to-move)
    (each square box-sides-to-move
      (set (grid [;(map + square step)]) (grid square))
      (set (grid square) nil))
    true))

(defn problem2 [{:dimension dimension :grid grid :start [sx sy] :steps steps}]
  (print (string-of [dimension dimension] grid))
  (def wide-grid (widen dimension grid))
  (def wide-start [(* 2 sx) sy])
  (print (string-of [(* 2 dimension) dimension] wide-grid))
  (reduce |(let [result (try-step wide-grid $0 $1 kick-wide-box?)]
             (print (string-of [(* 2 dimension) dimension] wide-grid))
             result)
          wide-start steps)
  (print (string-of [(* 2 dimension) dimension] wide-grid))
  (+ ;(map gps-of (filter |(= (wide-grid $) :box-left) (keys wide-grid)))))

(defn main [&]
  (spork/test/timeit
    (do
      (def path "15.txt")
      # (def path "15.test.txt")
      (def model (model-from-file path))
      (print (problem1 model))
      (def model (model-from-file path))
      (print (problem2 model)))))
