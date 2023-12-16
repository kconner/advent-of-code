;;; Model

; A board is a 2D array of characters. Dimensions are X and Y from the upper left.
(defun make-board (dimensions)
  (make-array dimensions :element-type 'character))

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

(defun load-board ()
  (lines->board (file->lines "14.txt")))

;;; Problem 1

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

(defun board-load (board)
  (let* ((dimensions (array-dimensions board))
         (mx (first dimensions))
         (my (second dimensions)))
    (loop for x from 0 below mx
          with board-load = 0
          do (loop for y from 0 below my
                   for rock-load downfrom my
                   do (when (eql (aref board x y) #\O)
                        (incf board-load rock-load)))
          finally (return board-load))))

(defun problem1 ()
  (let ((board (load-board)))
    (tilt board :north)
    (print (board-load board))))

(print (problem1))

;;; Problem 2

(defun cycle (board)
  (tilt board :north)
  (tilt board :west)
  (tilt board :south)
  (tilt board :east))

; call the cycle function in a loop that technically tops out at one billion runs,
; but should really stop when we detect a board state cycle. at that point, skip
; full cycles and do the remainder.
(defun problem2 ()
  (let ((board (load-board))
        (board-visit-hash (make-hash-table :test 'equalp)))
    (loop for i from 0 below 1000000000
          until (gethash board board-visit-hash)
          do (progn
               (setf (gethash board board-visit-hash) i)
               (cycle board))
          finally (return 
                    (let ((remainder (mod (- 1000000000 i)
                                          (- i (gethash board board-visit-hash)))))
                      (loop for j from 0 below remainder
                            do (cycle board)
                            finally (return (board-load board))))))))

(print (problem2))
