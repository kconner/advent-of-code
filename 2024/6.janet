(use ./tools)

(def up '(0 -1))
(def right '(1 0))
(def down '(0 1))
(def left '(-1 0))

(defn next-step [step]
  (case step up right right down down left left up))

(defn position-after-step [(px py) (sx sy)]
  ~(,(+ px sx) ,(+ py sy)))

(defn model-from-file [path]
  (let [input (slurp path)
        dimension (string/find "\n" input)
        wrap (inc dimension)
        char-position (fn [index] ~(,(mod index wrap) ,(div index wrap)))]
    {:dimension dimension
     :obstacles (from-pairs (map |(tuple (char-position $) true)
                                 (string/find-all "#" input)))
     :guard-start {:pos (char-position (string/find "^" input))
                   :step up}}))

(defn in-bounds [dimension pos]
  (every? (map |(< -1 $ dimension) pos)))

(defn default-visited-positions [model]
  (def {:dimension dimension :obstacles obstacles} model)
  (var guard (struct/to-table (model :guard-start)))
  (def visited-positions @{})
  (while (in-bounds dimension (guard :pos))
    (let [position-ahead (position-after-step (guard :pos) (guard :step))]
      (if (obstacles position-ahead)
        (set (guard :step) (next-step (guard :step)))
        (do
          (set (visited-positions (guard :pos)) true)
          (set (guard :pos) position-ahead)))))
  (keys visited-positions))

(defn problem1 [model]
  (length (default-visited-positions model)))

(defn guard-loops [model extra-obstacle]
  (prompt 'out
    (def {:dimension dimension :obstacles permanent-obstacles} model)
    (if (permanent-obstacles extra-obstacle)
      (return 'out false)) # already an obstacle; won't make a difference
    (def obstacles (table/clone permanent-obstacles))
    (set (obstacles extra-obstacle) true)
    (var guard (struct/to-table (model :guard-start)))
    (def turned-orientations @{})
    (while (in-bounds dimension (guard :pos))
      (let [position-ahead (position-after-step (guard :pos) (guard :step))]
        (if (obstacles position-ahead)
          (do
            (set (guard :step) (next-step (guard :step)))
            (let [orientation (table/to-struct guard)]
              (if (turned-orientations orientation)
                (return 'out true)) # detected a loop
              (set (turned-orientations orientation) true)))
          (set (guard :pos) position-ahead))))
    false)) # escaped without looping

(defn problem2 [model]
  (->> (default-visited-positions model)
       (count |(guard-loops model $))))

(defn main [&]
  (spork/test/timeit
    (do
      (def path "6.txt")
      # (def path "6.test.txt")
      (def model (model-from-file path))
      (print (problem1 model))
      (print (problem2 model)))))
