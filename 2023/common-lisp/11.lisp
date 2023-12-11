;;; Model

(defstruct point x y)
(defstruct size width height)

(defparameter *compression-ratio* 2)

;;; Parsing

(defun file-lines (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          collect line)))

(defparameter *lines* (file-lines "11.txt"))

;;; Problem 1

(defun map-size ()
  (make-size
    :width (length (first *lines*))
    :height (length *lines*)))

; Unit rows are the set of line indices of lines that contain any #s.
(defun unit-rows ()
  (let ((set (make-hash-table :size 200)))
    (loop for line in *lines*
          for row from 0
          when (find #\# line)
          do (setf (gethash row set) t))
    set))

; Unit columns are the set of character indices of each # found in any line.
(defun unit-columns ()
  (let ((set (make-hash-table :size 200)))
    (loop for line in *lines*
          do (loop for column from 0
                   for char across line
                   when (char= #\# char)
                   do (setf (gethash column set) t)))
    
    set))

; The uncompressed width of an interval is 1 if it's in the set of unit intervals, otherwise it's *compression-ratio*.
(defun uncompressed-width (unit-set compressed-index)
  (if (gethash compressed-index unit-set) 1 *compression-ratio*))

; A mapping of compressed index to uncompressed index is the result of iterating over the compressed intervals and incrementing by the uncompressed width.
(defun make-index-map (unit-set compressed-size)
  (let ((uncompressed-index 0)
        (table (make-hash-table :size 200)))
    (loop for compressed-index from 0 below compressed-size
          do (setf (gethash compressed-index table) uncompressed-index)
             (incf uncompressed-index (uncompressed-width unit-set compressed-index)))
    table))

; Galaxy locations are all the pairs of x and y where a # is found in the input. Look up x given the column and y given the row.
(defun galaxy-points ()
  (let* ((map-size (map-size))
         (row-index-map (make-index-map (unit-rows) (size-height map-size)))
         (column-index-map (make-index-map (unit-columns) (size-width map-size))))
    (loop for line in *lines*
          for row from 0
          append (loop for column from 0
                       for char across line
                       when (char= #\# char)
                       collect (make-point
                                 :x (gethash column column-index-map)
                                 :y (gethash row row-index-map))))))

; All pairs of items in a list is each car paired with each item of its own cdr, flattened.
(defun all-pairs (list)
  (loop for sublist on list
        append (loop for cdr-item in (cdr sublist)
                     collect (list (car sublist) cdr-item))))

; Manhattan distance of two points is the sum of absolute values of differences of xs and ys.
(defun manhattan-distance (point1 point2)
  (+ (abs (- (point-x point1) (point-x point2)))
     (abs (- (point-y point1) (point-y point2)))))

; Sum of Manhattan distances between all pairs of galaxy locations.
(defun galaxy-pair-distance-sum (galaxy-points)
  (apply #'+
         (mapcar
           (lambda (pair)
             (apply #'manhattan-distance pair))
           (all-pairs galaxy-points))))

(let ((*compression-ratio* 2))
  (print (galaxy-pair-distance-sum (galaxy-points))))

;;; Problem 2

(let ((*compression-ratio* 1000000))
  (print (galaxy-pair-distance-sum (galaxy-points))))

