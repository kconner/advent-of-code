(ql:quickload :cl-ppcre)

;;; Model

(defstruct run direction length)

(defstruct x-intercept x down up)

;;; Parsing

(defun file->lines (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          collect line)))

(defun line->run (line)
  (multiple-value-bind (match captures)
    (ppcre:scan-to-strings "^([UDLR]) ([\\d]+) \\(#.*\\)$" line)
    (declare (ignore match))
    (make-run :direction (aref (aref captures 0) 0)
              :length (parse-integer (aref captures 1)))))

(defparameter *runs* (mapcar #'line->run (file->lines "18.txt")))

;;; Problem 1

(defun x-intercepts-by-y ()
  (loop with hash = (make-hash-table)
        with x = 0
        with y = 0
        for run in *runs*
        for direction = (run-direction run)
        for len = (run-length run)
        do (when (or (eq direction #\U) (eq direction #\D))
             (loop for i from 0 to len
                   for yy = (case direction
                              (#\D (+ y i))
                              (#\U (- y i)))
                   do (push (make-x-intercept
                              :x x
                              :down (case direction
                                      (#\D (< i len))
                                      (#\U (< 0 i)))
                              :up (case direction
                                    (#\U (< i len))
                                    (#\D (< 0 i))))
                            (gethash yy hash))))
        do (case direction
             (#\R (incf x len))
             (#\L (decf x len))
             (#\D (incf y len))
             (#\U (decf y len)))
        finally (return hash)))

; The loop for a y is:
; For all the x-intercepts sorted,
; with an above-inside and below-inside states initialized false,
; count the run up from the prior x only if either bit is set.
; Count the x-intercept as dug out.
; Then toggle each bit according to whether the vertical line extends each direction.
(defun row-edge-or-inside-count (x-intercepts)
  (loop with above-inside = nil
        with below-inside = nil
        with count = 0
        with prior-x = nil
        for x-intercept in x-intercepts
        for x = (x-intercept-x x-intercept)
        for down = (x-intercept-down x-intercept)
        for up = (x-intercept-up x-intercept)
        do (when (and prior-x (or above-inside below-inside))
             (incf count (- x prior-x 1)))
        do (incf count)
        do (when up (setf above-inside (not above-inside)))
        do (when down (setf below-inside (not below-inside)))
        do (setf prior-x x)
        finally (return count)))

(defun problem1 ()
  (let ((sum 0))
    (maphash (lambda (y x-intercepts)
               (declare (ignore y))
               (incf sum (row-edge-or-inside-count
                           (sort x-intercepts #'< :key #'x-intercept-x))))
             (x-intercepts-by-y))
    sum))

(print (problem1))

;;; Problem 2

; This solution could be more elegant. I had to give it just a little extra heap.
; ros run -l 18.lisp -- --dynamic-space-size 8000

(defun line->long-run (line)
  (multiple-value-bind (match captures)
    (ppcre:scan-to-strings "^[UDLR] [\\d]+ \\(#(.+)(.)\\)$" line)
    (declare (ignore match))
    (make-run :direction (case (aref (aref captures 1) 0)
                           (#\0 #\R)
                           (#\1 #\D)
                           (#\2 #\L)
                           (#\3 #\U))
              :length (parse-integer (aref captures 0) :radix 16))))

(defun problem2 ()
  (let ((*runs* (mapcar #'line->long-run (file->lines "18.txt"))))
    (problem1)))

(print (problem2))
