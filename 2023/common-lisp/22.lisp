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
                (file->lines "22.txt"))
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

; how to settle the bricks? i think you could sort them by lower z and then
; insert one at a time, placing them as low as they will go.

; we could begin with a 10x10 grid of references to the topmost brick placed
; in that column, initially all blank.

; to insert a brick, collect the topmost bricks in each column of the brick's x, y range.
; among those, determine the maximum upper bound z value, or 1 if there are none.
; then make a copy of the brick with the z offset set to sit atop that height.
; finally reference the newly inserted brick with the x, y range of the inserted brick.

(defun insert-settled-brick
  (brick
    settled-bricks
    topmost-brick-index-grid
    supported-index-by-index
    supporting-index-by-index)
  (let ((upper-bound (brick-upper-bound brick))
        (supporting-bricks-index-list nil)
        (brick-index (length settled-bricks)))
    ; (print (list "brick, aloft" brick))
    (loop for x from (v3-x (brick-offset brick)) below (v3-x upper-bound)
          do (loop for y from (v3-y (brick-offset brick)) below (v3-y upper-bound)
                   do (let ((index (aref topmost-brick-index-grid x y)))
                        (when index
                          (pushnew index supporting-bricks-index-list)))))
    ; (print (list "bricks beneath" supporting-bricks-index-list))
    (let ((max-supporting-z
            (reduce #'max
                    (mapcar #'(lambda (index)
                                (v3-z (brick-upper-bound (aref settled-bricks index))))
                            supporting-bricks-index-list)
                    :initial-value 1)))
      ; (print (list "max z" max-supporting-z))
      (setf supporting-bricks-index-list
            (remove-if-not #'(lambda (index)
                               (= (v3-z (brick-upper-bound (aref settled-bricks index)))
                                  max-supporting-z))
                           supporting-bricks-index-list))
      ; (print (list "immediate supporting bricks" supporting-bricks-index-list))
      (let ((settled-brick (copy-brick-with-offset-z brick max-supporting-z)))
        ; (print (list "settled brick" settled-brick))
        (vector-push-extend settled-brick settled-bricks)
        (loop for x from (v3-x (brick-offset brick)) below (v3-x upper-bound)
              do (loop for y from (v3-y (brick-offset brick)) below (v3-y upper-bound)
                       do (setf (aref topmost-brick-index-grid x y) brick-index))))
      (setf (gethash brick-index supporting-index-by-index)
            supporting-bricks-index-list)
      (loop for supporting-brick-index in supporting-bricks-index-list
            do (push brick-index (gethash supporting-brick-index
                                          supported-index-by-index))))))

(defun make-settled-bricks ()
  (let ((settled-bricks (make-array (length *bricks-aloft*)
                                    :fill-pointer 0))
        (topmost-brick-index-grid (make-array (list (v3-x *dimensions*)
                                               (v3-y *dimensions*))
                                         :initial-element nil))
        (supported-index-by-index (make-hash-table :size (length *bricks-aloft*)))
        (supporting-index-by-index (make-hash-table :size (length *bricks-aloft*))))
    (loop for brick in *bricks-aloft*
          do (insert-settled-brick brick
                                   settled-bricks
                                   topmost-brick-index-grid
                                   supported-index-by-index
                                   supporting-index-by-index))
    (values settled-bricks
            supported-index-by-index
            supporting-index-by-index)))

; the goal is to find the number of bricks that can be disintegrated safely.

; horizontal dimensions are 10x10, and the vertical dimension is in the hundreds.
; there are 1,388 bricks.

; a brick can be removed safely (without causing others to fall)
; unless a brick it supports is supported by no other bricks.

(defun safe-to-remove-brick-p (brick-index
                                supported-index-by-index
                                supporting-index-by-index)
  (let ((supported-index-list (gethash brick-index supported-index-by-index)))
    (loop for supported-index in supported-index-list
          do (let ((supporting-index-list (gethash supported-index
                                                   supporting-index-by-index)))
               (when (= (length supporting-index-list) 1)
                 (return-from safe-to-remove-brick-p nil))))
    t))

(defun problem1 ()
  (multiple-value-bind
    (settled-bricks supported-index-by-index supporting-index-by-index)
    (make-settled-bricks)
    (loop for brick-index from 0 below (length settled-bricks)
          count (safe-to-remove-brick-p brick-index
                                        supported-index-by-index
                                        supporting-index-by-index))))

(print (problem1))

;;; Problem 2

; now the goal is to find how many bricks would fall if i removed each brick.
; i think that means, for a given brick i need to track it as speculatively removed,
; then enqueue all those it supports which have no support other than those that are
; speculatively removed.

(defun count-of-bricks-caused-to-fall (initial-brick-index
                                        supported-index-by-index
                                        supporting-index-by-index)
  (let ((removed-brick-index-list (make-array (length *bricks-aloft*)
                                              :fill-pointer 0))
        (removed-brick-index-hash (make-hash-table)))
    (vector-push-extend initial-brick-index removed-brick-index-list)
    (setf (gethash initial-brick-index removed-brick-index-hash) t)
    (loop for queue-index from 0
          while (< queue-index (length removed-brick-index-list))
          ; do (print (list "brick index" (aref removed-brick-index-list queue-index)))
          do (loop for supported-index in (gethash (aref removed-brick-index-list
                                                         queue-index)
                                                   supported-index-by-index)
                   ; do (print (list "supported index" supported-index))
                   do (let ((remaining-supports 
                              (remove-if #'(lambda (index)
                                             (gethash index removed-brick-index-hash))
                                         (gethash supported-index
                                                  supporting-index-by-index))))
                        ; (print (list "remaining supports" remaining-supports))
                        (when (and (null remaining-supports)
                                   (not (gethash supported-index
                                                 removed-brick-index-hash)))
                          (vector-push-extend supported-index removed-brick-index-list)
                          (setf (gethash supported-index removed-brick-index-hash) t)))))
    ; (print (length removed-brick-index-list))
    (1- (length removed-brick-index-list))))

(defun problem2 ()
  (multiple-value-bind
    (settled-bricks supported-index-by-index supporting-index-by-index)
    (make-settled-bricks)
    (loop for brick-index from 0 below (length settled-bricks)
          ; do (terpri) 
          ; do (print (list "trial index" brick-index)) 
          sum (count-of-bricks-caused-to-fall brick-index
                                              supported-index-by-index
                                              supporting-index-by-index))))

(print (problem2))
