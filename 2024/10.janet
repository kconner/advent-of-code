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
  (lines-from-file path))

(defn problem1 [model])

(defn problem2 [model])

(defn main [&]
  (spork/test/timeit
    (do
      # (def path "10.txt")
      (def path "10.test.txt")
      (def model (model-from-file path))
      (print (problem1 model))
      (print (problem2 model)))))
