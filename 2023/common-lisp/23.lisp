;;; Model

; the map is a 2D array of characters with dimensions x, y.
; . is empty and may be a node or part of a path,
; # is a wall and is never a node or part of a path,
; > v < ^ are each part of a path and indicate one-way travel in the direction they point.
(defparameter *map* nil)

; a direction-char is one of the characters: > v < ^
(defun direction-char-reverse (direction-char)
  (case direction-char
    (#\> #\<)
    (#\< #\>)
    (#\^ #\v)
    (#\v #\^)))

; a square is a position on the map.
(defstruct square
  (x nil :type fixnum)
  (y nil :type fixnum))

(defun square-in-bounds-p (square)
  (let ((x (square-x square))
        (y (square-y square)))
    (and (<= 0 x) (< x (array-dimension *map* 0))
         (<= 0 y) (< y (array-dimension *map* 1)))))

; a square is enterable from a direction if it is in bounds
; and is either a . or a one-way path in the given direction.
(defun square-enterable-from-direction-p (square direction-char)
  (and (square-in-bounds-p square)
       (let* ((x (square-x square))
              (y (square-y square))
              (square-char (aref *map* x y)))
         (or (eq square-char #\.)
             (eq square-char direction-char)))))

(defun adjacent-square (square direction-char)
  (let ((x (square-x square))
        (y (square-y square)))
    (case direction-char
      (#\> (make-square :x (+ x 1) :y y))
      (#\v (make-square :x x :y (+ y 1)))
      (#\< (make-square :x (- x 1) :y y))
      (#\^ (make-square :x x :y (- y 1))))))

; an edge represents a path toward another node whose location it carries.
; its value is the number of steps taken along the path.
(defstruct edge
  (value nil :type fixnum)
  (end-square nil :type square))

; a node represents an endpoint or intersection in the map.
(defstruct node
  (square nil :type square)
  (edges nil :type list))

; the graph is a vector of nodes and a hash of node index by square.
(defstruct graph
  (nodes (make-array 0 :element-type 'node :adjustable t :fill-pointer t))
  (index-by-square (make-hash-table :test #'equalp)))

(defun graph-add-node (graph node)
  (setf (gethash (node-square node) (graph-index-by-square graph))
        (vector-push-extend node (graph-nodes graph))))

(defun graph-has-node-at-square-p (graph square)
  (and (gethash square (graph-index-by-square graph)) t))

(defun graph-node-at-square (graph square)
  (aref (graph-nodes graph)
        (gethash square (graph-index-by-square graph))))

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

(setf *map* (lines->array (file->lines "23.test.txt")))

;;; Problem 1

(defun empty-square-in-row (y)
  (loop for x from 0 below (array-dimension *map* 0)
        when (eq (aref *map* x y) #\.)
        return (make-square :x x :y y)))

; the entry square is the empty space in the map along the top edge.
(defun entry-square ()
  (empty-square-in-row 0))

; the exit square is the empty space in the map along the bottom edge.
(defun exit-square ()
  (empty-square-in-row (- (array-dimension *map* 1) 1)))

(defun enterable-adjacent-squares-and-directions (square prior-direction-char)
  (loop for direction-char in (remove (direction-char-reverse prior-direction-char)
                                      '(#\> #\v #\< #\^))
        for adjacent-square = (adjacent-square square direction-char)
        when (square-enterable-from-direction-p adjacent-square direction-char)
        collect (list adjacent-square direction-char)))

(defun find-edge-end (value square direction-char)
  (let ((next-steps (enterable-adjacent-squares-and-directions square direction-char)))
    (if (/= 1 (length next-steps))
        (make-edge :value value :end-square square)
        (let* ((next-step (first next-steps))
               (next-square (first next-step))
               (next-direction-char (second next-step)))
          (find-edge-end (1+ value) next-square next-direction-char)))))

(defun edges-from-square (square)
  (loop for (adjacent-square direction-char)
        in (enterable-adjacent-squares-and-directions square nil)
        collect (find-edge-end 1 adjacent-square direction-char)))

; starting by treating the entry point as a node, create the graph by attempting
; to walk all paths from all encountered nodes.
(defun make-filled-graph ()
  (let ((graph (make-graph))
        (node-square-queue
          (make-array 0 :element-type 'square :fill-pointer t :adjustable t)))
    (vector-push-extend (entry-square) node-square-queue)
    (loop for i from 0
          while (< i (length node-square-queue))
          for square = (aref node-square-queue i)
          do (print (list "make filled graph" i square))
          unless (graph-has-node-at-square-p graph square)
          do (let ((edges (edges-from-square square)))
               (print (list "edges from square" edges))
               (graph-add-node graph (make-node :square square :edges edges))
               (loop for end-square in (mapcar #'edge-end-square edges)
                     unless (graph-has-node-at-square-p graph end-square)
                     do (vector-push-extend end-square node-square-queue))))
    graph))

; to find the edges from a square, for four directions:
; if the square is enterable, perform a search for an end square in that direction.

; A node is a non-wall square that has adjacent non-wall squares numbering other than 2.

; If a slope against is detected in a direction other than the direction of travel, bail on that path.

; If the directed walk of a path completes by finding a new or known node, create an edge. 

; Record the number of steps as the edge value. Don't repeat the work to search the immediate paths from a given node. 


;;; Formatting

(defun print-map (map)
  (loop for y from 0 below (array-dimension map 1)
        do (loop for x from 0 below (array-dimension map 0)
                 do (princ (aref map x y)))
        do (terpri)))

(print-map *map*)

