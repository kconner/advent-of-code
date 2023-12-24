(ql:quickload :cl-ppcre)

;;; Model

(defstruct v3 x y z)

(defstruct brick offset size)

(defun brick-upper-bound (brick)
  (let ((offset (brick-offset brick))
        (size (brick-size brick)))
    (make-v3 :x (+ (v3-x offset) (v3-x size))
             :y (+ (v3-y offset) (v3-y size))
             :z (+ (v3-z offset) (v3-z size)))))

(defun copy-brick-with-offset-z (brick z)
  (let ((offset (brick-offset brick)))
    (make-brick :offset (make-v3 :x (v3-x offset)
                                 :y (v3-y offset)
                                 :z z)
                :size (brick-size brick))))

;;; Parsing

(defun file->lines (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          collect line)))

(defun line->brick (line)
  (multiple-value-bind (match captures)
    (ppcre:scan-to-strings "^(\\d+),(\\d+),(\\d+)~(\\d+),(\\d+),(\\d+)$" line)
    (declare (ignore match))
    (let ((min-x (parse-integer (aref captures 0)))
          (min-y (parse-integer (aref captures 1)))
          (min-z (parse-integer (aref captures 2)))
          (max-x (parse-integer (aref captures 3)))
          (max-y (parse-integer (aref captures 4)))
          (max-z (parse-integer (aref captures 5))))
      (make-brick
        :offset (make-v3 :x min-x :y min-y :z min-z)
        :size (make-v3 :x (- max-x min-x -1)
                       :y (- max-y min-y -1)
                       :z (- max-z min-z -1))))))

(defparameter *bricks-aloft*
  (sort (mapcar #'line->brick
                (file->lines "22.test.txt"))
        #'< :key (lambda (b) (v3-z (brick-offset b)))))

(defparameter *dimensions*
  (let ((max-x 0)
        (max-y 0)
        (max-z 0))
    (dolist (brick *bricks-aloft*)
      (let ((upper-bound (brick-upper-bound brick)))
        (setf max-x (max max-x (v3-x upper-bound)))
        (setf max-y (max max-y (v3-y upper-bound)))
        (setf max-z (max max-z (v3-z upper-bound)))))
    (make-v3 :x max-x :y max-y :z max-z)))
  
;;; Problem 1

; the goal is to find the number of bricks that can be disintegrated safely.

; a brick can be disintegrated safely (without causing others to fall)
; unless a brick it supports is supported by no other bricks.

; due to the shape and alignment of bricks, if A supports B and B supports C,
; A cannot reach C to directly support it. so transitive support is not important.

; a brick supports another when its upper bound z equals the other's
; offset and their x, y rects intersect. it looks like the horizontal dimensions are
; 10x10, and the vertical dimension is in the hundreds.

; now, how to settle the bricks? i think you could sort them by lower z and then
; insert one at a time, placing them as low as they will go.

; we could begin with a 10x10 grid of references to the topmost brick placed
; in that column, initially all blank.

; to insert a brick, use topmost-brick-index to find the topmost bricks
; in each column of the brick's x, y range. among those, determine the maximum
; upper bound z value, or 1 if there are none.
; then make a copy of the brick with the z offset set to sit atop that height.
; finally reference the newly inserted brick with the x, y range of the inserted brick.

(defun insert-settled-brick (brick settled-bricks topmost-brick-index)
  (let ((upper-bound (brick-upper-bound brick))
        (supporting-bricks nil))
    (print (list "brick, aloft" brick))
    (loop for x from (v3-x (brick-offset brick)) below (v3-x upper-bound)
          do (loop for y from (v3-y (brick-offset brick)) below (v3-y upper-bound)
                   do (let ((topmost-brick (aref topmost-brick-index x y)))
                        (when topmost-brick
                          (pushnew topmost-brick supporting-bricks)))))
    (print (list "bricks beneath" supporting-bricks))
    (let ((max-supporting-z (reduce #'max (mapcar #'(lambda (b)
                                                      (v3-z (brick-upper-bound b)))
                                                  supporting-bricks)
                                    :initial-value 1)))
      (print (list "max z" max-supporting-z))
      (setf supporting-bricks
            (remove-if-not #'(lambda (b)
                               (= (v3-z (brick-upper-bound b)) max-supporting-z))
                           supporting-bricks))
      (print (list "immediate supporting bricks" supporting-bricks))
      (let ((settled-brick (copy-brick-with-offset-z brick max-supporting-z)))
        (print (list "settled brick" settled-brick))
        (vector-push-extend settled-brick settled-bricks)
        (loop for x from (v3-x (brick-offset brick)) below (v3-x upper-bound)
              do (loop for y from (v3-y (brick-offset brick)) below (v3-y upper-bound)
                       do (setf (aref topmost-brick-index x y) settled-brick)))))))

(defun make-settled-bricks ()
  (let ((settled-bricks (make-array (length *bricks-aloft*)
                                    :fill-pointer 0))
        (topmost-brick-index (make-array (list (v3-x *dimensions*)
                                               (v3-y *dimensions*))
                                         :initial-element nil)))
    (loop for brick in *bricks-aloft*
          do (insert-settled-brick brick settled-bricks topmost-brick-index)
          do (terpri)
          )
    settled-bricks))

(make-settled-bricks)

