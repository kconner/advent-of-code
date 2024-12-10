(use ./tools)

# it's a heightmap, 0 is low and 9 is high.
# desirable path starts with a 0, ends with a 9
# only ever changes by 1 step to step,
# and include cardinal adjacent steps only.

# a trailhead is the start of a trail. they have to have a 0.
# a trailhead's score is the number of unique 9s
# that can be reached by paths from there.

# sum all the trailhead scores.

# my data has 194 0s and 138 9s.

# i think i can do this by starting with a list of all 9s,
# in a counted set, so their scores are initially all 1.
# then i'd find all the adjacent 8s as a new counted set.
# that way if two 9s are adjacent to an 8, that 8 has a score of 2.
# eventually you have 1s and you find adjacent 0s.
# the counts of the 0s in the final set are their scores.
# sum them.

(defn model-from-file [path]
  (let [input (slurp path)
        dimension (string/find "\n" input)
        wrap (inc dimension)
        char-position (fn [index] ~(,(mod index wrap) ,(div index wrap)))]
    {:dimension dimension
     :grid (first (peg/match ~{:main (/ (some (choice (* :position :digit) "\n")) ,struct)
                               :digit (/ (<- (range "09")) ,scan-number)
                               :position (/ ($) ,char-position)} input))}))

(def up '(0 -1))
(def right '(1 0))
(def down '(0 1))
(def left '(-1 0))

(defn problem1 [model]

  #
)

(defn problem2 [model])

(defn main [&]
  (spork/test/timeit
    (do
      # (def path "10.txt")
      (def path "10.test.txt")
      (def model (model-from-file path))
      (print (problem1 model))
      (print (problem2 model)))))
