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
; Pad each with a final # for convenience.
(defparameter *columns* 
  (mapcar #'(lambda (s) (concatenate 'string s "#")) 
    (transpose (file-lines "14.txt"))))

;;; Problem 1

; Loop
; - For character across column-string
; - For next-cell-load from length-1, iterating down
; - With rock-load=length
; - With column-load=0
; - Do case character
;   - O: add rock-load to column-load, and decrement rock-load.
;   - #: set rock-load=next-cell-load
(defun column-score (column)
  (loop for character across column
        for next-cell-load downfrom (- (length column) 2)
        with rock-load = (1- (length column))
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
