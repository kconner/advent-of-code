(use ./tools)

# todo: filter out {:count 0}?
(defn model-from-file [path]
  (->> (slurp path)
       (peg/match ~{:main (* (some (* :file :space)) :file)
                    :file (/ (* :id :digit) ,|{:id $0 :count $1})
                    :space (/ :digit ,|{:count $})
                    :id (/ ($) ,|(/ $ 2))
                    :digit (/ (<- (range "09")) ,scan-number)})))

(defn disk-from-model [model]
  (def disk @[]) # (array/ensure @[] 200000 0))
  (each {:id id :count block-count} model
    (repeat block-count
      (array/push disk id)))
  disk)

(defn move-blocks [disk]
  (var space-cursor 0)
  (var tail-cursor (dec (length disk)))
  (while (< space-cursor tail-cursor)
    (if (nil? (disk space-cursor))
      (do
        (set (disk space-cursor) (disk tail-cursor))
        (array/remove disk tail-cursor)
        (set tail-cursor (dec tail-cursor)))
      (set space-cursor (inc space-cursor))))
  disk)

(defn checksum [disk]
  (+ ;(map (fn [index id] (default id 0) (* index id))
           (range (length disk)) disk)))

(defn problem1 [model]
  (->> model
       (disk-from-model)
       (move-blocks)
       (checksum)))

# This time, attempt to move whole files to the leftmost span of free space
# blocks that could fit the file. Attempt to move each file exactly once in
# order of decreasing file ID number starting with the file with the highest
# file ID number. If there is no span of free space to the left of a file that
# is large enough to fit the file, the file does not move.

(defn move-files [model]
  # for each file from right to left (indexed by src-cursor)
  # scan for a free space block of sufficient size, stopping at the cursor index
  # if one is found,
  # - replace the source file with a free space; don't bother joining it
  # - replace the destination space with the file
  # - if there was a remainder of space, then
  #   - if the next item is space, increase it
  #   - otherwise insert a new space object and adjust the cursor index
  # - break from the search
  (var src-cursor (length model))
  # todo loop somehow
  (prompt 'out
    (while true
      (prompt 'next-src
        (set src-cursor (dec src-cursor))
        # (pp [src-cursor model])
        (when (zero? src-cursor) (return 'out))

        (def src-file (model src-cursor))
        (def {:id src-id :count src-count} src-file)
        (when (nil? src-id) (return 'next-src)) # space stays put
        # (var dest-cursor 2)
        # (set dest-cursor (inc dest-cursor))
        (for dest-cursor 0 src-cursor
          (def {:id dest-id :count dest-count} (model dest-cursor))
          (when (and (nil? dest-id) (<= src-count dest-count))
            (set (model src-cursor) {:count src-count}) # leave empty space behind
            (set (model dest-cursor) src-file) # replace the space
            (def remainder (- dest-count src-count)) # what to do with extra space
            (cond
              # neatly filled, no-op
              (zero? remainder) nil
              # next is space, add to it
              (nil? ((model (inc dest-cursor)) :id)) (update model
                                                             (inc dest-cursor)
                                                             |{:count (+ ($ :count)
                                                                         remainder)})
              # next is a file, insert new space
              true (do
                     (array/insert model (inc dest-cursor) {:count remainder})
                     (set src-cursor (inc src-cursor)))) # src cursor was altered
            # stop the search
            (return 'next-src))))))
  model)

# (while (< space-cursor tail-cursor)
#   (cond
#     (nil? (disk space-cursor))
#     (do
#       (set (disk space-cursor) (disk tail-cursor))
#       (array/remove disk tail-cursor)
#       (set tail-cursor (dec tail-cursor)))
#     true (set space-cursor (inc space-cursor))))
# #
# disk)

(defn problem2 [model]
  (->> model
       (move-files)
       (disk-from-model)
       (checksum)))

(defn main [&]
  (spork/test/timeit
    (do
      (def path "9.txt")
      # (def path "9.test.txt")
      (def model (model-from-file path))
      (print (problem1 model))
      (print (problem2 model)))))
