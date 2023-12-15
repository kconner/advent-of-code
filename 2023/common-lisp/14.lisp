;;; Model

; 

;;; Parsing

(defun file-lines (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          collect line)))

(defun string->list (s)
  (coerce s 'list))

(defun list->string (l)
  (coerce l 'string))

(defun transpose (list-of-strings)
  (let ((list-of-char-lists (mapcar #'string->list list-of-strings)))
    (mapcar #'list->string (apply #'mapcar #'list list-of-char-lists))))

; Grab all the lines.
; Transpose them to get strings representing columns.
(defparameter *columns* 
  (transpose (file-lines "14.txt")))

;;; Problem 1

; Loop
; - For character across column
; - For next-cell-load from length-1, iterating down
; - With rock-load=length
; - With column-load=0
; - Do case character
;   - O: add rock-load to column-load, and decrement rock-load.
;   - #: set rock-load=next-cell-load
(defun column-score (column)
  (loop for character across column
        for next-cell-load downfrom (1- (length column))
        with rock-load = (length column)
        with column-load = 0
        do (case character
             (#\O (incf column-load rock-load)
                  (decf rock-load))
             (#\# (setf rock-load next-cell-load)))
        finally (return column-load)))

; Sum the column scores.
(defun problem1 ()
  (reduce #'+ (mapcar #'column-score *columns*)))

(print (problem1))

;;; Problem 2

; Rewrite the column with Os rolled to the beginning.
(defun tilt-north (column)
  (loop for character across column
        with pending-rocks = 0
        with pending-spaces = 0
        with post-tilt-column = (make-array (length column) :element-type 'character :fill-pointer 0)
        do (case character
             (#\. (incf pending-spaces))
             (#\O (incf pending-rocks))
             (#\#
              (loop for i from 1 to pending-rocks
                    do (vector-push #\O post-tilt-column))
              (loop for i from 1 to pending-spaces
                    do (vector-push #\. post-tilt-column))
              (vector-push #\# post-tilt-column)
              (setf pending-spaces 0
                    pending-rocks 0)))
        finally
              (loop for i from 1 to pending-rocks
                    do (vector-push #\O post-tilt-column))
              (loop for i from 1 to pending-spaces
                    do (vector-push #\. post-tilt-column))
              (return post-tilt-column)))

; Roll north, west, south, and east, and end in the northward orientation again.
(defun cycle (columns)
  (setf columns (mapcar #'tilt-north columns)) ; north
  (setf columns (transpose columns))
  (setf columns (mapcar #'tilt-north columns)) ; west
  (setf columns (transpose (reverse columns)))
  (setf columns (mapcar #'tilt-north columns)) ; south
  (setf columns (transpose (reverse columns)))
  (setf columns (mapcar #'tilt-north columns)) ; east
  (setf columns (reverse (transpose (reverse columns)))))

; call the cycle function in a loop that technically tops out at one billion runs, but should really stop when the prior and current board states are equal.
(defun problem2 ()
  (let ((columns *columns*)
        (prior-columns nil))
    (loop for i from 1 to 1000 ; 000000
          until (equal columns prior-columns)
          do ;(print i)
          (setf prior-columns columns
                columns (cycle columns)))
    (reduce #'+ (mapcar #'column-score columns))))

; (problem2)

; …

; when the cycle function is not producing further changes, use the problem 1 function to sum the column scores.

; hypothesis is that it will reach a steady state where there is no change after single cycles anymore.
; if comparing board states is expensive, i could check only every 1000 cycles or something.

(defun print-grid (columns)
  (mapcar #'print columns)
  (print "-"))

