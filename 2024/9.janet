(use ./tools)

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

(defn frag [disk]
  (var space-cursor 0)
  (var tail-cursor (dec (length disk)))
  (while (< space-cursor tail-cursor)
    (cond
      (nil? (disk space-cursor))
      (do
        (set (disk space-cursor) (disk tail-cursor))
        (array/remove disk tail-cursor)
        (set tail-cursor (dec tail-cursor)))
      true (set space-cursor (inc space-cursor))))
  disk)

(defn checksum [disk]
  (+ ;(map * (range (length disk)) disk)))

(defn problem1 [disk]
  (->> disk
       (frag)
       (checksum)))

(defn problem2 [disk])

(defn main [&]
  (spork/test/timeit
    (do
      (def path "9.txt")
      # (def path "9.test.txt")
      (def model (model-from-file path))
      (def disk (disk-from-model model))
      (print (problem1 disk))
      (print (problem2 disk)))))
