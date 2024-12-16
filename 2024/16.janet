(use ./tools)

# 15x15 -> 141x141
# One S for start, one E for end
# Turns cost 1000, moves cost 1
# Initial direction is east
#
# Observations:
# - Multiple possible paths exist
# - S and E are both always at a corner with two ways in

# I can parse a grid to find all the empty squares in a table,
# plus the start and end positions and the implicit starting direction.

# I can BFS,
# but I need to find the minimum cost, and not every move costs the same.
# I could bucket moves by total score to get there.
# I can prevent rework from loops by tracking visited [position direction]s.
# With each dequeued bucket:
# - With each move in the bucket:
#   - If I already checked it, skip it
#   - Mark that I'm checking it now
#   - I already know the score
#   - If it's at E, the score is the answer
#   - Get all the moves I can make next and enqueue them all
# To dequeue a bucket:
# - Get the next score from the score list
# - Delete that score
# - Get the bucket from the buckets by score table
# - Delete that table entry if it's not faster to just keep it
# To enqueue a move at a score,
# - Get the bucket for the score
#   - If it doesn't exist,
#     - Add it, an empty array, to the buckets by score table
#     - Insert the score in the score list
# - Push the move on the array
# Init by enqueueing [start [1 0]] at score 0

# The data model needs to enable discovering next moves from a current one.
# But to build that I only need to look over the whole grid once, and
# due to avoiding duplicate work during the BFS, that wouldn't save much.
# However I could go a step further and compact it all such that the only
# nodes are S, E, and 3-way and 4-way intersections, in open directions.
# For example S north and S east would be bidirectionally connected with an
# arc cost of 1000.
# OK I really like that.

# First we parse the grid.

(defn model-from-file [path]
  (->> (slurp path)
       (peg/match ~{:main 1})))

(defn problem1 [model])

(defn problem2 [model])

(defn main [&]
  (spork/test/timeit
    (do
      # (def path "16.txt")
      (def path "16.test.txt")
      (def model (model-from-file path))
      (print (problem1 model))
      (print (problem2 model)))))
