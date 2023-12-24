;;; Model

; the map is a 2D array of characters with dimensions x, y.
; . is empty and may be a node or part of a path,
; # is a wall and is never a node or part of a path,
; > v < ^ are each part of a path and indicate one-way travel in the direction they point.
(defparameter *map* nil)

(defun print-map (map)
  (loop for y from 0 below (array-dimension map 1)
        do (loop for x from 0 below (array-dimension map 0)
                 do (princ (aref map x y)))
        do (terpri)))

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

(defun square->string (square)
  (format nil "~a,~a" (square-x square) (square-y square)))

(defun square-char (square)
  (aref *map* (square-x square) (square-y square)))

(defun square-in-bounds-p (square)
  (let ((x (square-x square))
        (y (square-y square)))
    (and (<= 0 x) (< x (array-dimension *map* 0))
         (<= 0 y) (< y (array-dimension *map* 1)))))

; a square is walkable if it is in bounds and is not a #.
(defun square-walkable-p (square)
  (and (square-in-bounds-p square)
       (char/= (square-char square) #\#)))

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

(defun print-graph-as-mermaid (graph)
  (format t "flowchart LR;~%")
  (loop for node across (graph-nodes graph)
        do (let ((node-string (square->string (node-square node))))
             (format t "~a~%" node-string)
             (loop for edge in (node-edges node)
                   do (format t
                              "~a --~a--> ~a~%"
                              node-string
                              (edge-value edge)
                              (square->string (edge-end-square edge)))))))

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

(setf *map* (lines->array (file->lines "23.txt")))

(print-map *map*)

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

; to find the edges from a square, for four directions:
; if the square is walkable, perform a search for an end square in that direction.
; during the search, stop and return no edge if stepping on a reverse one-way travel square.
; consider walkable squares in all directions except the reverse.
; when there is one option, continue the path search.
; otherwise, stop and return an edge. record the number of steps as the edge value.

(defun walkable-adjacent-squares-and-directions (square prior-direction-char)
  (loop for direction-char in (remove (direction-char-reverse prior-direction-char)
                                      '(#\> #\v #\< #\^))
        for adjacent-square = (adjacent-square square direction-char)
        when (square-walkable-p adjacent-square)
        collect (list adjacent-square direction-char)))

(defun find-edge-end (value square direction-char)
  (if (char= (square-char square) (direction-char-reverse direction-char))
      nil
      (let ((next-steps (walkable-adjacent-squares-and-directions square direction-char)))
        (if (/= 1 (length next-steps))
            (make-edge :value value :end-square square)
            (let* ((next-step (first next-steps))
                   (next-square (first next-step))
                   (next-direction-char (second next-step)))
                (find-edge-end (1+ value) next-square next-direction-char))))))

(defun edges-from-square (square)
  (loop for (adjacent-square direction-char)
        in (walkable-adjacent-squares-and-directions square nil)
        for edge = (find-edge-end 1 adjacent-square direction-char)
        when edge
        collect edge))

; starting by treating the entry point as a node, create the graph by attempting
; to walk all paths from all encountered nodes once each.
(defun make-filled-graph ()
  (let ((graph (make-graph))
        (node-square-queue
          (make-array 0 :element-type 'square :fill-pointer t :adjustable t)))
    (vector-push-extend (entry-square) node-square-queue)
    (loop for i from 0
          while (< i (length node-square-queue))
          for square = (aref node-square-queue i)
          unless (graph-has-node-at-square-p graph square)
          do (let ((edges (edges-from-square square)))
               (graph-add-node graph (make-node :square square :edges edges))
               (loop for end-square in (mapcar #'edge-end-square edges)
                     unless (graph-has-node-at-square-p graph end-square)
                     do (vector-push-extend end-square node-square-queue))))
    graph))

(print-graph-as-mermaid (make-filled-graph))

; ok, in mermaid rendering i can see the graph is acyclic, and except for two long arcs
; going around the corners, each node's children are on the immediate next depth level,
; so it's almost perfectly stratified. it's also symmetric in the start-to-end direction
; and across it, so it doesn't matter which end you search from. it does not pinch and
; create subgraphs and there are no irrelevant paths to prune. there are something like
; 2^10 paths so let's walk them all and take the max.

; find the longest path by DFS. accumulate the edge value sum. when recursing to children,
; take the maximum of their result. avoid memoizing unless it turns out to be slow.
(defun longest-path (graph node)
  (let ((edges (node-edges node)))
    (if (null edges)
        0
        (apply #'max
               (mapcar (lambda (edge)
                         (+ (edge-value edge)
                            (longest-path graph
                                          (graph-node-at-square
                                            graph (edge-end-square edge)))))
                       (node-edges node))))))

(defun problem1 ()
  (let ((graph (make-filled-graph)))
    (longest-path graph (graph-node-at-square graph (entry-square)))))

(print (problem1))
