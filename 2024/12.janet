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
  (lines-from-file path))

(defn problem1 [model])

(defn problem2 [model])

(defn main [&]
  (spork/test/timeit
    (do
      # (def path "12.txt")
      (def path "12.test.txt")
      (def model (model-from-file path))
      (print (problem1 model))
      (print (problem2 model)))))
