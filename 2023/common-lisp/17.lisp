;;; Model

; A cost is an int.
; A cost-grid is a 2D array of costs whose dimensions are x and y.

; A state is a distinct state the crucible can be in,
; irrespective of the cost to get there.
(defstruct state x y direction moves-between-turns)

(defparameter *minimum-moves-between-turns* 0)
(defparameter *maximum-moves-between-turns* 3)

(defclass bfs ()
  ; hashtable of cost to state list
  ((queue :reader bfs-queue :initform (make-hash-table))
   ; hashtable of state to cost
   (hash :reader bfs-hash :initform (make-hash-table :test 'equalp))))

; Get cost for a state, O(1)
(defmethod get-cost ((bfs bfs) state)
  (gethash state (bfs-hash bfs)))

; Put cost for a state, O(1)
(defmethod put-cost ((bfs bfs) state cost)
  (setf (gethash state (bfs-hash bfs)) cost))

(defsetf get-cost put-cost)

; Remove state for a cost, O(n)
(defmethod remove-state-at-cost ((bfs bfs) cost state)
  (let ((states (gethash cost (bfs-queue bfs))))
    (setf (gethash cost (bfs-queue bfs)) (remove state states :test #'equalp))))

; Insert state for a cost, O(1)
(defmethod insert-state-at-cost ((bfs bfs) cost state)
  (let ((states (gethash cost (bfs-queue bfs))))
    (setf (gethash cost (bfs-queue bfs)) (cons state states))))

; Dequeue next state for a cost, O(1)
(defmethod dequeue-state-at-cost ((bfs bfs) cost)
  (let ((states (gethash cost (bfs-queue bfs))))
    (let ((state (first states)))
      (setf (gethash cost (bfs-queue bfs)) (rest states))
      state)))

; Advance a cost cursor to the next cost with any states, O(n)
(defmethod advance-cost-cursor ((bfs bfs) cost-cursor)
  (loop for cost from cost-cursor
        for states = (gethash cost (bfs-queue bfs))
        while (null states)
        finally (return cost)))

;;; Parsing

(defun file->lines (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          collect line)))

(defun lines->cost-grid (lines)
  (let ((cost-grid (make-array (list (length (first lines))
                                (length lines)))))
    (loop for line in lines
          for y from 0
          do (loop for character across line
                   for x from 0
                   do (setf (aref cost-grid x y)
                            (- (char-code character) (char-code #\0)))))
    cost-grid))

(defparameter *cost-grid* (lines->cost-grid (file->lines "17.txt")))

;;; Problem 1

(defun in-bounds-p (state)
  (let ((x (state-x state))
        (y (state-y state)))
    (and (<= 0 x) (< x (array-dimension *cost-grid* 0))
         (<= 0 y) (< y (array-dimension *cost-grid* 1)))))

; Determine the next possible states:
; - 90º turns with 2 to go,
; - unless 0 to go, straight ahead with 1- to go
; - only positions in bounds
(defun next-states (state)
  (let ((x (state-x state))
        (y (state-y state))
        (direction (state-direction state))
        (moves-between-turns (state-moves-between-turns state))
        (acc nil))
    (when (< *minimum-moves-between-turns* moves-between-turns)
      (cond
        ((or (eq direction :north) (eq direction :south))
         (push (make-state :x (1+ x)
                           :y y
                           :direction :east
                           :moves-between-turns 1)
               acc)
         (push (make-state :x (1- x)
                           :y y
                           :direction :west
                           :moves-between-turns 1)
               acc))
        ((or (eq direction :east) (eq direction :west))
         (push (make-state :x x
                           :y (1+ y)
                           :direction :south
                           :moves-between-turns 1)
               acc)
         (push (make-state :x x
                           :y (1- y)
                           :direction :north
                           :moves-between-turns 1)
               acc))))
    (when (< moves-between-turns *maximum-moves-between-turns*)
      (push (make-state :x (case direction
                             (:north x)
                             (:south x)
                             (:east (1+ x))
                             (:west (1- x)))
                        :y (case direction
                             (:east y)
                             (:west y)
                             (:south (1+ y))
                             (:north (1- y)))
                        :direction direction
                        :moves-between-turns (1+ moves-between-turns))
            acc))
    (remove-if-not #'in-bounds-p acc)))

; BFS.
; Initial step: state (0 0 :east 3), cost 0
; Loop:
; Dequeue state.
; If the state is at the end, return the cost.
; With each next possible state, find its cost, then insert:
; - The queue must contain only one element of each state
; - The lowest known cost for the state should be kept
; - When inserting a new lowest cost, group it with all other items of that cost.
; - Therefore, first check the hash of state for a previously known cost.
;   - If there's no known cost,
;     - Save the new cost to the hash.
;     - Add the state to the queue at its cost.
;   - If the new cost < the known cost,
;     - Replace the cost in the hash.
;     - Remove the state from the queue at the old cost.
;     - Add the step to the queue at its new cost.
;   - If the known cost <= the new cost,
;     - Take no action.
(defun problem1 ()
  (let ((bfs (make-instance 'bfs))
        (end-x (1- (array-dimension *cost-grid* 0)))
        (end-y (1- (array-dimension *cost-grid* 1))))
    (insert-state-at-cost bfs 0 (make-state
                                  :x 0 :y 0
                                  :direction :east
                                  :moves-between-turns 0))
    (loop for cost-cursor = (advance-cost-cursor bfs 0)
          do (loop for state = (dequeue-state-at-cost bfs cost-cursor)
                   while state
                   do (progn
                        (when (and (= (state-x state) end-x)
                                   (= (state-y state) end-y))
                          (return-from problem1 cost-cursor))
                        (loop for next-state in (next-states state)
                              for next-cost = (+ cost-cursor
                                                 (aref *cost-grid*
                                                       (state-x next-state)
                                                       (state-y next-state)))
                              for known-cost = (get-cost bfs next-state)
                              do (cond
                                   ((null known-cost)
                                    (put-cost bfs next-state next-cost)
                                    (insert-state-at-cost bfs next-cost next-state))
                                   ((< next-cost known-cost)
                                    (remove-state-at-cost bfs known-cost next-state)
                                    (put-cost bfs next-state next-cost)
                                    (insert-state-at-cost bfs next-cost next-state)))))))))

(print (problem1))

;;; Problem 2

(defun problem2 ()
  (let ((*minimum-moves-between-turns* 3)
        (*maximum-moves-between-turns* 10))
    (problem1)))

(print (problem2))
