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
    (mark-square entry-square 0 distance-field)
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

(defun problem1 ()
  (let ((distance-field (make-distance-field (start-square)))
        (target-distance 64))
    ; (print-distance-field *distance-field*)
    (distance-field-count-if #'(lambda (distance)
                                 (and distance
                                      (evenp (- target-distance distance))
                                      (<= distance target-distance)))
                             distance-field)))

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

