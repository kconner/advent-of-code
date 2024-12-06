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

(defn problem1 [path]
  (def input (slurp path))
  (def dimension (string/find "\n" input))
  (def wrap (inc dimension))
  (defn position-for-character [wrap index]
    ~(,(mod index wrap) ,(div index wrap)))
  (var guard-position (position-for-character wrap (string/find "^" input)))
  (var guard-step up)
  (def obstacles (from-pairs (map |(tuple (position-for-character wrap $) true) (string/find-all "#" input))))
  (defn in-bounds [pos]
    (every? (map |(< -1 $ dimension) pos)))
  (defn position-after-step [(px py) (sx sy)]
    ~(,(+ px sx) ,(+ py sy)))
  (def visited-positions @{})
  (while (in-bounds guard-position)
    (let [position-ahead (position-after-step guard-position guard-step)]
      (if (obstacles position-ahead)
        (set guard-step (next-step guard-step))
        (do
          (pp guard-position)
          (set (visited-positions guard-position) true)
          (set guard-position position-ahead)))))
  (length visited-positions))

(defn problem2 [path])

(position-after-step guard-position guard-step)

(defn main [&]
  (spork/test/timeit
    (do
      # (def path "6.txt")
      (def path "6.test.txt")
      # (def model (model-from-file path))
      (print (problem1 path))
      (print (problem2 path)))))
