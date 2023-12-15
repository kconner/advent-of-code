;;; Model

; A board is a 2D array of characters. Dimensions are X and Y from the upper left.
(defun make-board (dimensions)
  (make-array dimensions :element-type 'character))

(defun copy-into-board (board copy)
  (let* ((dimensions (array-dimensions board))
         (mx (first dimensions))
         (my (second dimensions)))
    (loop for x from 0 below mx
          do (loop for y from 0 below my
                   do (setf (aref copy x y) (aref board x y))))))

(defun copy-board (board)
  (let ((copy (make-board (array-dimensions board))))
    (copy-into-board board copy)
    copy))

;;; Parsing

(defun file->lines (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          collect line)))

(defun lines->board (lines)
  (let ((array (make-board (list (length (car lines)) (length lines)))))
    (loop for line in lines
          for y from 0
          do (loop for character across line
                   for x from 0
                   do (setf (aref array x y) character)))
    array))

(defparameter *board* (lines->board (file->lines "14.txt")))

;;; Problem 1

(defun board-load (board)
  (let* ((dimensions (array-dimensions board))
         (mx (first dimensions))
         (my (second dimensions)))
    (loop for x from 0 below mx
          with board-load = 0
          do (loop for y from 0 below my
                   for next-cell-load downfrom (1- my)
                   with rock-load = my
                   do (case (aref board x y)
                        (#\O (incf board-load rock-load)
                             (decf rock-load))
                        (#\# (setf rock-load next-cell-load))))
          finally (return board-load))))

(print (board-load *board*))

;;; Problem 2

; Rotate coordinates into a board coordinate space adjustment applied to a board.
(defun rotation (board x y view)
  (let ((x (case view
             (:north x)
             (:west y)
             (:south (- (array-dimension board 0) x 1))
             (:east (- (array-dimension board 1) y 1))))
        (y (case view
             (:north y)
             (:west (- (array-dimension board 0) x 1))
             (:south (- (array-dimension board 1) y 1))
             (:east x))))
    (values x y)))

(defun get-rotated (board x y view)
  (multiple-value-bind (x y) (rotation board x y view)
    (aref board x y)))

(defun set-rotated (board x y view value)
  (multiple-value-bind (x y) (rotation board x y view)
    (setf (aref board x y) value)))

(defsetf get-rotated set-rotated)

(defun rotated-dimensions (board view)
  (let* ((dimensions (array-dimensions board))
         (mx (first dimensions))
         (my (second dimensions)))
    (values (case view
              (:north mx)
              (:south mx)
              (:east my)
              (:west my))
            (case view
              (:north my)
              (:south my)
              (:east mx)
              (:west mx)))))

(defun tilt (board view)
  (multiple-value-bind (mx my) (rotated-dimensions board view)
    (loop for x from 0 below mx
          do (loop for y from 0 below my
                   with run-start = y
                   with pending-rocks = 0
                   do (case (get-rotated board x y view)
                        (#\O
                         (incf pending-rocks)
                         (setf (get-rotated board x y view) #\.))
                        (#\#
                         (loop for i from 1 to pending-rocks
                               for yy from run-start
                               do (setf (get-rotated board x yy view) #\O))
                         (setf pending-rocks 0
                               run-start (1+ y))))
                   finally (loop for i from 1 to pending-rocks
                                 for yy from run-start
                                 do (setf (get-rotated board x yy view) #\O))))))

; Roll north, west, south, and east, and end in the northward orientation again.
(defun cycle (board)
  (tilt board :north)
  (tilt board :west)
  (tilt board :south)
  (tilt board :east))

; call the cycle function in a loop that technically tops out at one billion runs, but should really stop when the prior and current board states are equal.
(defun problem2 ()
  (let ((board (copy-board *board*))
        (copy (make-board (array-dimensions *board*))))
    (loop for i from 1 to 100 ; 0000000
          until (equal board copy)
          do 
          ; (print i)
          (copy-into-board board copy)
          (cycle board))
    (board-load board)))

(problem2)

; …

; when the cycle function is not producing further changes, use the problem 1 function to sum the column scores.

; hypothesis is that it will reach a steady state where there is no change after single cycles anymore.
; if comparing board states is expensive, i could check only every 1000 cycles or something.

(defun print-grid (columns)
  (mapcar #'print columns)
  (print "-"))

; Print 0,0 - 0,mx as a line, then 1-0, 1-mx, etc.
(defun print-board (board)
  (loop for y from 0 below (array-dimension board 1)
        do (loop for x from 0 below (array-dimension board 0)
                 do (format t "~a" (aref board x y)))
        (format t "~%")))

