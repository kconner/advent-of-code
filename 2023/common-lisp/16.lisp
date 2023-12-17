;;; Model

; A grid is a 2D array of characters whose dimensions are x and y.
; A direction is :north, :east, :south, :west.
; A breadcrumb is a list of x, y, and direction.
; A breadcrumb-set is a hashtable of breadcrumb to t.

;;; Parsing

(defun file->lines (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          collect line)))

(defun lines->grid (lines)
  (let ((grid (make-array (list (length (first lines))
                                (length lines)))))
    (loop for line in lines
          for y from 0
          do (loop for character across line
                   for x from 0
                   do (setf (aref grid x y) character)))
    grid))

(defparameter *grid* (lines->grid (file->lines "16.txt")))

;;; Problem 1

; An adjacent breadcrumb takes one step away from the given tile in the given direction.
(defun adjacent-breadcrumb (x y direction)
  (case direction
    (:north (list x (1- y) :north))
    (:east (list (1+ x) y :east))
    (:south (list x (1+ y) :south))
    (:west (list (1- x) y :west))))
  
; The next breadcrumbs to explore are derived from the tile character and breadcrumb.
(defun next-breadcrumbs (tile breadcrumb)
  (let ((x (first breadcrumb))
        (y (second breadcrumb))
        (direction (third breadcrumb)))
    (case tile
      (#\. (list (adjacent-breadcrumb x y direction)))
      (#\| (if (or (eq direction :north) (eq direction :south))
                (list (adjacent-breadcrumb x y direction))
                (list (adjacent-breadcrumb x y :north)
                      (adjacent-breadcrumb x y :south))))
      (#\- (if (or (eq direction :east) (eq direction :west))
                (list (adjacent-breadcrumb x y direction))
                (list (adjacent-breadcrumb x y :east)
                      (adjacent-breadcrumb x y :west))))
      (#\/ (case direction
             (:east (list (adjacent-breadcrumb x y :north)))
             (:north (list (adjacent-breadcrumb x y :east)))
             (:west (list (adjacent-breadcrumb x y :south)))
             (:south (list (adjacent-breadcrumb x y :west)))))
      (#\\ (case direction
             (:east (list (adjacent-breadcrumb x y :south)))
             (:south (list (adjacent-breadcrumb x y :east)))
             (:west (list (adjacent-breadcrumb x y :north)))
             (:north (list (adjacent-breadcrumb x y :west))))))))

; A breadcrumb is in bounds when its x and y are within the grid's dimensions.
(defun in-bounds-p (breadcrumb)
  (let ((x (first breadcrumb))
        (y (second breadcrumb)))
    (and (<= 0 x) (< x (array-dimension *grid* 0))
         (<= 0 y) (< y (array-dimension *grid* 1)))))

; Extend the path of light until it reaches an existing breadcrumb,
; leaving them behind as it goes.
(defun explore (breadcrumb breadcrumb-set)
  (when (and (not (gethash breadcrumb breadcrumb-set))
             (in-bounds-p breadcrumb))
    (setf (gethash breadcrumb breadcrumb-set) t)
    (loop for next in (next-breadcrumbs
                        (aref *grid* (first breadcrumb) (second breadcrumb))
                        breadcrumb)
          do (explore next breadcrumb-set))))

; A tile is energized if any of the four breadcrumbs at that location are in the set.
(defun tile-energized-p (x y breadcrumb-set)
  (or (gethash (list x y :north) breadcrumb-set)
      (gethash (list x y :east) breadcrumb-set)
      (gethash (list x y :south) breadcrumb-set)
      (gethash (list x y :west) breadcrumb-set)))

; The energized tile count is the count of energized tiles within the grid's dimensions
; after exploring from a given starting point.
(defun energized-tile-count (x y direction)
  (let ((breadcrumb-set (make-hash-table :test #'equal)))
    (explore (list x y direction) breadcrumb-set)
    (let ((count 0))
      (dotimes (x (array-dimension *grid* 0))
        (dotimes (y (array-dimension *grid* 1))
          (when (tile-energized-p x y breadcrumb-set)
            (incf count))))
      count)))

(defun problem1 ()
  (energized-tile-count 0 0 :east))

(print (problem1))

;;; Problem 2

(defun problem2 ()
  (let ((mx (array-dimension *grid* 0))
        (my (array-dimension *grid* 1)))
  (max
    (loop for y from 0 below my
          maximize (energized-tile-count 0 y :east))
    (loop for y from 0 below my
          maximize (energized-tile-count (1- mx) y :west))
    (loop for x from 0 below mx
          maximize (energized-tile-count x 0 :south))
    (loop for x from 0 below mx
          maximize (energized-tile-count x (1- my) :north)))))

(print (problem2))
