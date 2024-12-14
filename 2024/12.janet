(use ./tools)

(defn model-from-file [path]
  (let [input (slurp path)
        dimension (string/find "\n" input)
        wrap (inc dimension)
        char-position (fn [index] [(mod index wrap) (div index wrap)])]
    {:dimension dimension
     :grid (first (peg/match
                    ~{:main (/ (some (choice (* :position :character) "\n")) ,struct)
                      :character (<- (range "AZ"))
                      :position (/ ($) ,char-position)} input))}))

(defn make-region [first-square crop]
  @{:id first-square
    :crop crop
    :squares @{first-square true}
    :perimeter 0})

(defn annex [regions region patch]
  # only if the patch is new land,
  (when (not= region patch)
    # add annexed squares to the old region squares
    (merge-into (region :squares) (patch :squares))
    # add to the perimeter
    (update region :perimeter |(+ $ (patch :perimeter)))
    # delete the annexed region from the region table
    (set (regions (patch :id)) nil))
  region)

(defn region-at [regions square]
  # O(n) so maybe we do an index later?
  (find |(($ :squares) square) (values regions)))

(defn regions-from-model [{:dimension dimension :grid grid}]
  (def max-dimension (dec dimension))
  (def regions @{})
  # how about we look at each square in reading order starting from the corner.
  (for x 0 dimension
    (for y 0 dimension
      (def square [x y])
      (def crop (grid square))
      # first declare it its own region.
      (var patch (make-region square crop))
      # then look at the adjacent square to the left.
      (var other-square [(dec x) y])
      (var other-crop (grid other-square))
      (if (nil? other-crop)
        # if it's out of bounds, add to the perimeter.
        (update patch :perimeter inc)
        (do
          (def other-region (region-at regions other-square))
          (if (= crop other-crop)
            # if it's the same crop, merge regions.
            (set patch (annex regions other-region patch))
            # if it's different, add to both perimeters.
            (do
              (update patch :perimeter inc)
              (update other-region :perimeter inc)))))
      # then look at the adjacent square above; do the same.
      (var other-square [x (dec y)])
      (var other-crop (grid other-square))
      (if (nil? other-crop)
        # if it's out of bounds, add to the perimeter.
        (update patch :perimeter inc)
        (do
          (def other-region (region-at regions other-square))
          (if (= crop other-crop)
            # if it's the same crop, merge regions.
            (set patch (annex regions other-region patch))
            # if it's different, add to both perimeters.
            (do
              (update patch :perimeter inc)
              (update other-region :perimeter inc)))))
      # then if it's on the far right, add to the perimeter.
      (when (= x max-dimension)
        (update patch :perimeter inc))
      # then if it's on the bottom, add to the perimeter.
      (when (= y max-dimension)
        (update patch :perimeter inc))
      # ensure the patch is saved in the region table.
      (set (regions (patch :id)) patch)))
  regions)

(defn region-area [region]
  (length (region :squares)))

(defn region-price [prop region]
  (* (region-area region) (region prop)))

(defn problem1 [model]
  (+ ;(map |(region-price :perimeter $) (regions-from-model model))))

(defn squares-around [[px py]]
  [[px py] [(dec px) py] [px (dec py)] [(dec px) (dec py)]])

(defn count-corners [{:dimension dimension :grid grid} regions]
  (each region regions
    (set (region :corner-count) 0))

  (for y 0 (inc dimension)
    (for x 0 (inc dimension)
      (def squares-by-region (group-by |(region-at regions $)
                                       (squares-around [x y])))
      (eachp (region squares) squares-by-region
        (if (odd? (length squares))
          (update region :corner-count inc)
          # it's 2. add two corners only if they're catty-corner
          (let [[x0 y0] (squares 0)
                [x1 y1] (squares 1)]
            (when (and (not= x0 x1) (not= y0 y1))
              (update region :corner-count |(+ 2 $)))))))))

(defn problem2 [model]
  (def regions (regions-from-model model))
  (count-corners model regions)
  (+ ;(map |(region-price :corner-count $) regions)))

(defn main [&]
  (spork/test/timeit
    (do
      (def path "12.txt")
      # (def path "12.test.txt")
      # (def path "12.test2.txt")
      (def model (model-from-file path))
      (print (problem1 model))
      (print (problem2 model)))))
