(use ./tools)

# naively:
# parse the input to discover:
# - all obstacle locations
# - the guard's position

# no wait let's start at the end.
# i need to know the number of unique positions visited by the guard.
# so i'll build a set of visited positions, tuples.
# i should consider the starting position to be visited.
# then we need a position and direction for the guard.
# loop: inspect the square forward of the guard
# - if it's out of bounds, quit
# - if it's an obstacle, change the direction
# - otherwise it's an empty space. add the current square to the set, step the guard forward
# count the size of the visited square set.

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

(defn problem1 [model]
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
  (length visited-positions))

(defn guard-loops [model extra-obstacle]
  (prompt 'out
    (def {:dimension dimension :obstacles permanent-obstacles} model)
    (if (permanent-obstacles extra-obstacle)
      (do
        # (pp ['redundant extra-obstacle @{}])
        (return 'out false))) # already an obstacle; won't make a difference
    (def obstacles (table/clone permanent-obstacles))
    (set (obstacles extra-obstacle) true)
    (var guard (struct/to-table (model :guard-start)))
    (def visited-orientations @{})
    (while (in-bounds dimension (guard :pos))
      (let [position-ahead (position-after-step (guard :pos) (guard :step))]
        (if (obstacles position-ahead)
          (do
            (set (guard :step) (next-step (guard :step)))
            (if (visited-orientations (table/to-struct guard))
              (do
                # (pp ['looped-on-turn extra-obstacle visited-orientations])
                (return 'out true)))) # detected a loop
          (let [orientation (table/to-struct guard)]
            (if (visited-orientations orientation)
              (do
                # (pp ['looped extra-obstacle visited-orientations])
                (return 'out true))) # detected a loop
            (do
              (set (visited-orientations orientation) true)
              (set (guard :pos) position-ahead))))))
    # (pp ['escaped extra-obstacle])
    false)) # escaped without looping

(defn all-positions [dimension]
  (mapcat (fn [x] (map |(tuple $ x) (range dimension)))
          (range dimension)))

(defn problem2 [model]
  (->> (model :dimension)
       (all-positions)
       (map |(guard-loops model $))
       (count true?)))

(defn main [&]
  (spork/test/timeit
    (do
      (def path "6.txt")
      (def path "6.test.txt")
      (def model (model-from-file path))
      (print (problem1 model))
      (print (problem2 model))
      #
)))
