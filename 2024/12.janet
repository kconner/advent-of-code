(use ./tools)

# region: contiguous same letters, at least one
# need to know a region's area and perimeter
# area: number of squares
# perimeter: "number of sides of garden plots in the region that do not touch
# another garden plot in the same region", and that means it includes the
# outer border. holes count too.
# price = area * perimeter.

# first we identify all the regions.
# the total area of all regions should equal the total area of the board.
# then we figure out the perimeters. or maybe those can be a side effect of
# the region search.

# how about we look at each square in reading order starting from the corner.
# first declare it its own region.
# then look at the adjacent square to the left.
# - if it's the same, merge regions.
# - if it's out of bounds, add to the perimeter.
# - if it's different, add to both perimeters.
# then look at the adjacent square above; do the same.
# then if it's on the far right, add to the perimeter.
# then if it's on the bottom, add to the perimeter.

# so what is a region?
# - a crop character
# - a region id: the first square identified
# - a set of squares like @{(3, 5) true}
#   - its area is the length of this
# - a perimeter like 12

# what operations are there?
# same crop type as me? check a region's crop character
# region area? (length squares)
# region price? (* area perimeter)
# get every region? let's keep a table by region id
# which region is at [x, y]?
# - i could O(n) search the region table
# - or i could make an index like @{square region}
# annex a new region into an old region
# - add annexed squares to the old region squares
# - add to the perimeter
# - delete the annexed region from the region table
#   - update the region index, if i make that

(defn model-from-file [path]
  (let [input (slurp path)
        dimension (string/find "\n" input)
        wrap (inc dimension)
        char-position (fn [index] ~(,(mod index wrap) ,(div index wrap)))]
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
    (set (regions (patch :id)) nil)
    # TODO: update the region index, if i make that
)
  region)

(defn region-at [regions square]
  # TODO: O(n) so maybe we do an index later?
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

(defn region-price [region]
  (* (region-area region) (region :perimeter)))

(defn problem1 [model]
  (+ ;(map region-price (regions-from-model model))))

(defn problem2 [model])

(defn main [&]
  (spork/test/timeit
    (do
      (def path "12.txt")
      (def path "12.test.txt")
      (def model (model-from-file path))
      (print (problem1 model))
      (print (problem2 model)))))
