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

(defparameter *distance-field* (make-array (array-dimensions *garden*)
                                           :initial-element nil))

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

(print-distance-field *distance-field*)

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

(defun garden-square-empty-p (square)
  (let ((x (square-x square))
        (y (square-y square)))
    (eq (aref *garden* x y) #\.)))

(defun distance-field-square-unmarked-p (square)
  (let ((x (square-x square))
        (y (square-y square)))
    (null (aref *distance-field* x y))))

(defun mark-square (square distance)
  (let ((x (square-x square))
        (y (square-y square)))
    (setf (aref *distance-field* x y) distance)))

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

(defun fill-distance-field (distance squares-at-prior-distance)
  (let ((squares-at-distance nil))
    (loop for square in squares-at-prior-distance
          do (loop for adjacent-square in (adjacent-squares square)
                   do (when (and (square-in-bounds-p adjacent-square)
                                 (garden-square-empty-p adjacent-square)
                                 (distance-field-square-unmarked-p adjacent-square))
                        (mark-square adjacent-square distance)
                        (push adjacent-square squares-at-distance))))
    (unless (null squares-at-distance)
      (fill-distance-field (1+ distance) squares-at-distance))))

(defun start-square ()
  (loop for x from 0 below (array-dimension *garden* 0)
        do (loop for y from 0 below (array-dimension *garden* 1)
                 do (when (eq (aref *garden* x y) #\S)
                      (return-from start-square (make-square :x x :y y))))))

(defun distance-field-count-if (predicate)
  (loop for x from 0 below (array-dimension *distance-field* 0)
        sum (loop for y from 0 below (array-dimension *distance-field* 1)
                  count (funcall predicate (aref *distance-field* x y)))))

(defun problem1 ()
  (let ((*distance-field* (make-array (array-dimensions *garden*) :initial-element nil))
        (start-square (start-square))
        (target-distance 64))
    (mark-square start-square 0)
    (fill-distance-field 1 (list start-square))
    ; (print-distance-field *distance-field*)
    (distance-field-count-if #'(lambda (distance)
                                 (and distance
                                      (evenp (- target-distance distance))
                                      (<= distance target-distance))))))

(print (problem1))

;;; Problem 2
