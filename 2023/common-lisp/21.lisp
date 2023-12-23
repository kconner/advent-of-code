;;; Model

; The garden is a 2D array of characters with dimensions x, y.
; . is an empty space, # is an obstacle, and S is the starting position.

; A distance-field is a 2D array of integers with dimensions x, y.

;;; Parsing

(defun transpose (lists)
  (apply #'mapcar #'list lists))

(defun file->lines (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          collect (coerce line 'list) into rows
          finally (return (transpose rows)))))

(defun lines->array (lines)
  (let ((width (length (first lines)))
        (height (length lines)))
    (make-array (list width height)
                :initial-contents lines)))

(defparameter *garden* (lines->array (file->lines "21.txt")))

;;; Formatting

(defun print-garden (garden)
  (loop for y from 0 below (array-dimension garden 1)
        do (loop for x from 0 below (array-dimension garden 0)
                 do (princ (aref garden x y)))
        do (terpri)))

(print-garden *garden*)

(defun print-number-cell (number-or-nil)
  (if (null number-or-nil)
      (princ "   -")
      (format t "~4d" number-or-nil)))

(defun print-distance-field (distance-field)
  (loop for y from 0 below (array-dimension distance-field 1)
        do (loop for x from 0 below (array-dimension distance-field 0)
                 do (print-number-cell (aref distance-field x y)))
        do (terpri)))

;;; Problem 1

(defstruct square x y)

(defun adjacent-squares (square)
  (let ((x (square-x square))
        (y (square-y square)))
    (list (make-square :x (1- x) :y y)
          (make-square :x (1+ x) :y y)
          (make-square :x x :y (1- y))
          (make-square :x x :y (1+ y)))))

(defun square-in-bounds-p (square)
  (let ((x (square-x square))
        (y (square-y square)))
    (and (>= x 0)
         (< x (array-dimension *garden* 0))
         (>= y 0)
         (< y (array-dimension *garden* 1)))))

(defun garden-square-walkable-p (square)
  (let ((x (square-x square))
        (y (square-y square)))
    (char/= (aref *garden* x y) #\#)))

(defun distance-field-square-unmarked-p (square distance-field)
  (let ((x (square-x square))
        (y (square-y square)))
    (null (aref distance-field x y))))

(defun mark-square (square distance distance-field)
  (let ((x (square-x square))
        (y (square-y square)))
    (setf (aref distance-field x y) distance)))

; find the square with the S.
; in the distance field, mark it with 0.
; consider that to be the whole list of squares at distance 0.
; n = 0.
; given the list of squares at distance N-1,
; mark squares at distance N and accumulate their squares:
;   for each square in the list at distance N-1,
;     for each of the four adjacent squares,
;       when it's in bounds, is a . in the garden, and is nil in the distance field,
;         write N in the distance field at that square,
;         and collect the square.
;   append the lists of squares.
; unless the list of squares is empty,
;   recurse with the list of squares at N to mark N+1.
; in this way, populate the whole distance field with distances from N.

(defun fill-distance-field (distance-field distance squares-at-prior-distance)
  (let ((squares-at-distance nil))
    (loop for square in squares-at-prior-distance
          do (loop for adjacent-square in (adjacent-squares square)
                   do (when (and (square-in-bounds-p adjacent-square)
                                 (garden-square-walkable-p adjacent-square)
                                 (distance-field-square-unmarked-p adjacent-square
                                                                   distance-field))
                        (mark-square adjacent-square distance distance-field)
                        (push adjacent-square squares-at-distance))))
    (unless (null squares-at-distance)
      (fill-distance-field distance-field (1+ distance) squares-at-distance))))

(defun make-distance-field (entry-square)
  (let ((distance-field (make-array (array-dimensions *garden*) :initial-element nil)))
    (when (square-in-bounds-p entry-square)
      (mark-square entry-square 0 distance-field))
    (fill-distance-field distance-field 1 (list entry-square))
    distance-field))

(defun start-square ()
  (loop for x from 0 below (array-dimension *garden* 0)
        do (loop for y from 0 below (array-dimension *garden* 1)
                 do (when (eq (aref *garden* x y) #\S)
                      (return-from start-square (make-square :x x :y y))))))

(defun distance-field-count-if (predicate distance-field)
  (loop for x from 0 below (array-dimension distance-field 0)
        sum (loop for y from 0 below (array-dimension distance-field 1)
                  count (funcall predicate (aref distance-field x y)))))

(defun end-steps-in-tile (target-distance entry-point)
  (let ((distance-field (make-distance-field entry-point)))
    ; (print-distance-field distance-field)
    (distance-field-count-if #'(lambda (distance)
                                 (and distance
                                      (evenp (- target-distance distance))
                                      (<= distance target-distance)))
                             distance-field)))

(defun problem1 ()
  (end-steps-in-tile 64 (start-square)))

(print (problem1))

;;; Problem 2

; this time, we can still use the same garden data, but squares have to be able to
; wrap their coordinates in the space of the garden's dimensions, treating it like a
; wrapped texture lookup. every square is in bounds, too.
; the distance field is the same in the abstract, but we can't store it with an array.

; we can try a hash of square to distance. but the sample problem data shows that
; for a distance of N, you expect something approaching N^2 reachable squares, which
; is only every other square that you actually hit.
; and the target distance is 26,501,365, so an upper bound on the hash would be
; (* 26501365 26501365 2) = 1,404,644,693,726,450
; about a quadrillion. i can't store that in 64GB, but before that, i don't think we can
; do 26 million cycles the last of which tops out at a 100 million square horizon.

; so we need to not walk them one by one. what other insights can we find?

; the distance field is not strictly necessary. what's needed is to count the
; even-numbered spaces on the way to the max distance.

; the garden could be freely rotated to place the starting point at a corner,
; or wherever is most convenient. by considering all tilings of the garden to be in
; one of four quadrants, we might find that the work to step over one tiling is repeatable
; for the rest of the tiles in the quadrant.

; the garden has empty spaces in a square around the outside, as well as diagonal paths
; that create a diamond shape with the starting point at the center. when wrapped, the
; garden has two large diamonds that checker on the diagonal.

; because the garden's outer edge is a square of empty spaces, it is the case that the
; adjacent edge on the wrapped repetition of the garden contains all reachable squares,
; which may mean it's the cleanest boundary for dividing repetitive work.
; that's because, knowing the distance to the corner, you know that's the closest
; starting point to find everything else in the garden. for cardinal tiles i'm not sure.

; the garden's dimensions are odd. that means that the number of even-numbered squares
; in the distance field is not the same for every repetition of the garden. if we consider
; the starting garden to be garden distance 0 from the start, then the four adjacent ones
; are distance 1, and so the even-distanced squares in distance 0 would match with those
; in all even-distanced gardens and the odd-distanced squares in the odd-distance gardens.

; if we can deal with the garden as one level of wrapping, we can recurse and deal in
; tiles of tiles. for those whose outer corner touches the horizon, we can reduce zoom
; and recurse again to zoom back in, increasing detail only in the areas that need
; special consideration.

; we'll need to know for each tile the minimum and maximum distances from the starting
; point. hopefully those are in the near and far corners.

; ---

; backing up. let me try and plan this top down.

; the garden tiles infinitely, and we start in a particular spot in one of the tiles.

; the number of squares in any tile that can be reached in 26,501,365 steps is
; the sum, across all tiles, of the number of squares that can be reached from the tile's
; entry point in steps equal to 26,501,365 minus the entry point's distance from the
; starting point, where the entry point is defined as the square adjacent to the minimum
; distance edge square of an evaluated adjacent tile.

; for one tile, the number of squares that can be reached in a local target number of steps,
; from a given spot on the tile considered local distance zero, is supported by creating
; a single-tile distance field from that spot, as in problem 1. with that in hand,
; the number of squares that can be reached in the local target number of steps is the
; number of distance field squares whose value is set, less than or equal to the target,
; and even.

; a distance field from a given starting point and distance zero can be reused for any
; tile that contains that starting point. this work would be reduced from a factor of
; the number of tiles to cover, which i'll guess is 2 * 26,501,365^2 / 131^2, to
; a factor of the number of starting points, which we can guess is 9.
; that reduces total brute force work by a factor of about 9*10^9.

; the number of squares in a tile that can be reached in a given number of steps,
; from a given spot on the tile, is also memoizable.
; terms could be phrased as, for an entry point and a target distance localized by
; having subtracted the distance to the entry point, or,
; for an entry point and its known distance, implicitly using the final target distance.

; let's design the recursion in the abstract.

; "the entry point of a tile is the square adjacent to the minimum distance edge square
; of an evaluated adjacent tile."
; the entry point and distance in the starting tile are known. from that we can decide
; the entry points to the four adjacent tiles by finding the minimum distance on each edge.

; before deriving a rule for recursing to diagonals, let's check for insights about
; the data by seeing where the entry point would be for
; - immediate and successive cardinal direction tiles: do they have a common entry point,
;   or a repeating cycle?
; - immediate and successive in-quadrant tiles: are they all diagonal entering from
;   the corner, or what?

; for a given minimum and maximum x and y, iterate over the squares in a given 2D array
; and call a reducing function, accumulating with a given initial value

(defstruct subarray-bounds min-x max-x min-y max-y)

(defun reduce-subarray (reducing-function initial-value bounds array) 
  (let ((min-x (subarray-bounds-min-x bounds))
        (max-x (subarray-bounds-max-x bounds))
        (min-y (subarray-bounds-min-y bounds))
        (max-y (subarray-bounds-max-y bounds)))
    (loop for x from min-x below max-x
          do (loop for y from min-y below max-y
                   do (when (aref array x y)
                        (setf initial-value
                              (funcall reducing-function
                                       initial-value
                                       (list (aref array x y)
                                             (make-square :x x :y y))))))
          finally (return initial-value))))

(defun left-edge-bounds ()
  (make-subarray-bounds :min-x 0 :max-x 1
                        :min-y 0 :max-y (array-dimension *garden* 1)))

(defun right-edge-bounds ()
  (let ((max-x (array-dimension *garden* 0)))
    (make-subarray-bounds :min-x (- max-x 1) :max-x max-x
                          :min-y 0 :max-y (array-dimension *garden* 1))))

(defun up-edge-bounds ()
  (make-subarray-bounds :min-x 0 :max-x (array-dimension *garden* 0)
                        :min-y 0 :max-y 1))

(defun down-edge-bounds ()
  (let ((max-y (array-dimension *garden* 1)))
    (make-subarray-bounds :min-x 0 :max-x (array-dimension *garden* 0)
                          :min-y (- max-y 1) :max-y max-y)))

(defun whole-bounds ()
  (make-subarray-bounds :min-x 0 :max-x (array-dimension *garden* 0)
                        :min-y 0 :max-y (array-dimension *garden* 1)))

; a reducing function to find the minimum value and its coordinates

(defun nearest-square (entry-point bounds)
  (reduce-subarray #'(lambda (minimum item)
                       (if (< (first item) (first minimum)) item minimum))
                   (list most-positive-fixnum -1 -1)
                   bounds
                   (make-distance-field entry-point)))

(defun farthest-square (entry-point bounds)
  (reduce-subarray #'(lambda (minimum item)
                       (if (> (first item) (first minimum)) item minimum))
                   (list -1 -1 -1)
                   bounds
                   (make-distance-field entry-point)))

(defun wrap-to-left (square)
  (make-square :x (+ (square-x square) (array-dimension *garden* 0))
               :y (square-y square)))

(defun wrap-to-right (square)
  (make-square :x (- (square-x square) (array-dimension *garden* 0))
               :y (square-y square)))

(defun wrap-to-up (square)
  (make-square :x (square-x square)
               :y (+ (square-y square) (array-dimension *garden* 1))))

(defun wrap-to-down (square)
  (make-square :x (square-x square)
               :y (- (square-y square) (array-dimension *garden* 1))))

(defun explore-entry-points ()
  (let* ((start-tile-entry (start-square))
         (start-tile-left-exit (nearest-square start-tile-entry (left-edge-bounds)))
         (left-tile-entry (wrap-to-left (second start-tile-left-exit)))
         (left-tile-left-exit (nearest-square left-tile-entry (left-edge-bounds)))
         (left-left-tile-entry (wrap-to-left (second left-tile-left-exit)))
         (left-left-tile-left-exit (nearest-square left-left-tile-entry (left-edge-bounds))))
    (print start-tile-entry) ; 65, 65
    (print start-tile-left-exit) ; distance 65 to 0, 65
    (print left-tile-entry) ; 131, 65
    (print left-tile-left-exit) ; distance +131 to 0, 65
    (print left-left-tile-entry) ; 131, 65
    (print left-left-tile-left-exit))) ; distance +131 to 0, 65. repeats!

; i didn't expect costs of 131, but looking closer at the input, it's not just that the
; edge squares are empty, but so is the entire row and column containing the start point,
; the center. that means we know for certain that the shortest distance from the entry
; point of any tile to any other tile heading away from the starting point is just a
; straight line to the tile edge.

; we also know for certain that the cost of that wrap is exactly 131, except for the
; center tile, where it is 65.

; how many non-start tiles will we reach in any cardinal direction?
; (/ (- 26501365 65) 131) = exactly 202300.
; let's use a smaller target for intuition.
; (+ 65 (* 202300 131)) => (+ 65 (* 1 131)) = 196.
; if our target was 196 steps, we would need to step through the entire starting tile,
; plus the adjacent cardinal tiles. where could we stop?

(defun explore-maximum-distances-in-tiles ()
  (let* ((start-tile-entry (start-square))
         (start-tile-maximum-value (farthest-square start-tile-entry (whole-bounds)))
         (start-tile-left-exit (nearest-square start-tile-entry (left-edge-bounds)))
         (left-tile-entry (wrap-to-left (second start-tile-left-exit)))
         (left-tile-maximum-value (farthest-square left-tile-entry (whole-bounds)))
         (left-tile-up-exit (nearest-square left-tile-entry (up-edge-bounds)))
         (left-up-tile-entry (wrap-to-up (second left-tile-up-exit)))
         (left-up-tile-maximum-value (farthest-square left-up-tile-entry (whole-bounds)))
         )
    (print start-tile-entry)
    (print start-tile-maximum-value) ; distance 130 to any corner
    (print start-tile-left-exit) ; distance 65 to center left
    (print left-tile-entry) ; center, off the right edge
    (print left-tile-maximum-value) ; distance +196 to the upper or lower left corners
    (print left-tile-up-exit) ; 66, in the upper right corner
    (print left-up-tile-entry) ; lower right, off the lower edge
    (print left-up-tile-maximum-value))) ; distance +261 to the upper left corner

; then, the starting tile contains distances 0 <= d < 131.
; an adjacent cardinal tile contains distances 65 < d <= 196, or 0 < (d - 65) <= 131.
; the nearest diagonal tiles contain distances 131 < d <= 261, or 0 < (d - 131) <= 130.
; the starting tile can be counted as internal.
; the cardinal tile can be counted as a corner, where the boundary enters from the bottom
; edge and exits the top edge, without pushing past either of the other edges.
; the diagonal tile can be counted as a side, where the boundary enters from one edge
; and leaves by the adjacent edge, also without touching the other two.
; due to this pattern where the boundary only touches the tip of the cardinal tiles
; and otherwise splits boundary tile edges down the middle, every other tile is
; fully internal, no matter how many multiples of 131 the target factors in.

; also, we can define these nine tiles, count their values based on 196, and
; multiply them based on how many of their type exist within range of the target.

; let's define the target distance again as:
; tile-wrap-count = 202300, or 1
; target-distance = (+ 65 (* tile-wrap-count 131))

; so in the general case we can sum
; each of the four cardinal tiles once,
; each of four diagonal tiles times tile-wrap-count,
; and the internal tile times, what…
; when tile-wrap-count is 1, there is 1, the center.
; when tile-wrap-count is 2, there are 5 counting the immediate cardinals.

; oh, but there is a new class of side tile, those where the boundary enters and exits
; the two far sides rather than the two near sides. we need to count those differently.

; for each cardinal direction there is always 1 corner tile.
; for each diagonal direction there are tile-wrap-count outer side tiles. (fewer hits)
; for each diagonal direction there are (1- tile-wrap-count) inner side tiles. (more hits)
; the number of internal tiles is 1 for the starting tile, plus a sequence sum.
; the sequence is that in each of four directions there's 1, then a stairstep of 1+2,
; then a stairstep of 1+2+3 and so on. if we pair up two directions worth of stairstep,
; the sequence sum for the pair is 0*1, then 1*2, then 2*3 and so on.
; so with the starting square and two pairs of stairsteps, the number of internal tiles
; is (1+ (* 2 tile-wrap-count (1- tile-wrap-count))).

; oh, let's double check one thing. for two adjacent internal tiles,
; since we are only counting the even steps, the distance difference between them
; is odd, isn't it? yeah, we proved that it's always 131 from entry point to entry point.
; this shouldn't affect the repeating side tiles of either kind, because they are
; diagonally adjacent, nor the corner tiles because they're unique. but we really have
; two kinds of internal nodes, the evens and the odds. we need to count them separately.

; we should also stop using (+ 65 131) as a test value. it doesn't predict the same edge
; as adding an even number of wraps, so let's use (+ 65 131 131) = 327 instead.

; diagramming for myself, i find that the even internal tiles is described by the sequence:
; when tile-wrap-count is 2, there is 1, the center, or 1^2.
; when tile-wrap-count is 4, there are another 4 times 2, a total of 9, or 3^2.
; when tile-wrap-count is 6, there are another 4 times 4, a total of 25, or 5^2.
; when tile-wrap-count is 8, there are another 4 times 6, a total of 49, or 7^2.
; so the number of even internal tiles is explicitly the square of tile-wrap-count - 1.

; the number of odd internal tiles is described by the sequnece:
; when tile-wrap-count is 2, there are 4 times 1, a total of 4, or 2^2.
; when tile-wrap-count is 4, there are another 4 times 3, a total of 16, or 4^2.
; when tile-wrap-count is 6, there are another 4 times 5, a total of 36, or 6^2.
; so the number of odd internal tiles is explicitly the square of tile-wrap-count.

(defun tile-type-counts (target-distance)
  (let* ((tile-wrap-count (/ (- target-distance 65) 131))
         (corner-tile-count-per-direction 1)
         (outer-side-tile-count-per-direction tile-wrap-count) 
         (inner-side-tile-count-per-direction (1- tile-wrap-count)) 
         (even-internal-count (* (1- tile-wrap-count) (1- tile-wrap-count)))
         (odd-internal-count (* tile-wrap-count tile-wrap-count)))
    (list corner-tile-count-per-direction
          outer-side-tile-count-per-direction
          inner-side-tile-count-per-direction
          even-internal-count
          odd-internal-count
          (= (+ even-internal-count odd-internal-count)
             (1+ (* 2 tile-wrap-count (1- tile-wrap-count)))))))

(tile-type-counts (+ 65 (* 2 131)))
(tile-type-counts (+ 65 (* 4 131)))
(tile-type-counts (+ 65 (* 6 131)))
(tile-type-counts 26501365)

(defun problem2-target-distance () 26501365)

(defun substitute-target-distance () (+ 65 (* 2 131)))

; looks good. now let's find, for tile-wrap-count 2, the counts for each distinct type.
(defun end-steps-in-even-internal-tile ()
  (end-steps-in-tile (substitute-target-distance) (start-square)))

; verified this is the same as the even internal tile for distance + 1:
(defun end-steps-in-odd-internal-tile ()
  (let* ((distance-and-entry-point (nearest-square (start-square) (left-edge-bounds)))
         (entry-distance (first distance-and-entry-point))
         (entry-point (wrap-to-left (second distance-and-entry-point))))
    (end-steps-in-tile (- (substitute-target-distance) entry-distance)
                       entry-point)))

