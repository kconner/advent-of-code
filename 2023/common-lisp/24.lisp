(ql:quickload :cl-ppcre)

;;; Model

(defstruct v2 
  (x nil :type number)
  (y nil :type number))

(defun v2-+ (a b)
  (make-v2 :x (+ (v2-x a) (v2-x b))
           :y (+ (v2-y a) (v2-y b))))

(defun v2-* (v c)
  (make-v2 :x (* (v2-x v) c)
           :y (* (v2-y v) c)))

(defun v2-within-bounds-p (v lower-bound upper-bound)
  (and (<= (v2-x lower-bound) (v2-x v) (v2-x upper-bound))
       (<= (v2-y lower-bound) (v2-y v) (v2-y upper-bound))))

(defstruct ray2
  (pos nil :type v2)
  (vel nil :type v2))

(defun ray2-sample (ray interpolant)
  (v2-+ (ray2-pos ray)
        (v2-* (ray2-vel ray)
              interpolant)))

(defstruct v3
  (x nil :type number)
  (y nil :type number)
  (z nil :type number))

(defun v3->v2 (v)
  (make-v2 :x (v3-x v) :y (v3-y v)))

(defstruct ray3
  (pos nil :type v3)
  (vel nil :type v3))

(defun ray3->ray2 (r)
  (make-ray2 :pos (v3->v2 (ray3-pos r))
             :vel (v3->v2 (ray3-vel r))))

;;; Parsing

(defun file->lines (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          collect line)))

(defun line->ray3 (line)
  (multiple-value-bind (match captures)
    (ppcre:scan-to-strings "^(-?\\d+),\\s+(-?\\d+),\\s+(-?\\d+)\\s+@\\s+(-?\\d+),\\s+(-?\\d+),\\s+(-?\\d+)$" line)
    (declare (ignore match))
    (let ((pos-x (parse-integer (aref captures 0)))
          (pos-y (parse-integer (aref captures 1)))
          (pos-z (parse-integer (aref captures 2)))
          (vel-x (parse-integer (aref captures 3)))
          (vel-y (parse-integer (aref captures 4)))
          (vel-z (parse-integer (aref captures 5))))
      (make-ray3
        :pos (make-v3 :x pos-x :y pos-y :z pos-z)
        :vel (make-v3 :x vel-x :y vel-y :z vel-z)))))

(defparameter *ray3s* (mapcar #'line->ray3 (file->lines "24.txt")))

(defparameter *ray2s* (mapcar #'ray3->ray2 *ray3s*))

;;; Problem 1

; for a pair of rays, find the intersection between the lines phrased as
; a-pos + a-vel * a-interpolant = b-pos + b-vel * b-interpolant
; by solving for a-interpolant.
(defun ray2-intersection-interpolant (subject other)
  (handler-case
    ; thanks wikipedia
    ; https://en.wikipedia.org/wiki/Line–line_intersection
    ; what a bro
    (let* ((p1 (ray2-pos subject)) (x1 (v2-x p1)) (y1 (v2-y p1))
           (p2 (v2-+ p1 (ray2-vel subject))) (x2 (v2-x p2)) (y2 (v2-y p2))
           (p3 (ray2-pos other)) (x3 (v2-x p3)) (y3 (v2-y p3))
           (p4 (v2-+ p3 (ray2-vel other))) (x4 (v2-x p4)) (y4 (v2-y p4))
           (interpolant (/ (- (* (- x1 x3) (- y3 y4))
                                (* (- y1 y3) (- x3 x4)))
                             (- (* (- x1 x2) (- y3 y4))
                                (* (- y1 y2) (- x3 x4))))))
      interpolant)
    (arithmetic-error () nil)))

(defun rays-intersect-in-bounds-p (ray-a ray-b lower-bound upper-bound)
  (let ((interpolant-a (ray2-intersection-interpolant ray-a ray-b))
        (interpolant-b (ray2-intersection-interpolant ray-b ray-a)))
    ; (print (list interpolant-a interpolant-b))
    (and interpolant-a
         interpolant-b
         (<= 0 interpolant-a)
         (<= 0 interpolant-b)
         (v2-within-bounds-p (ray2-sample ray-a interpolant-a)
                             lower-bound
                             upper-bound))))

(defun 2-combinations (items)
  (loop for sublist on items
        for item = (first sublist)
        nconc (loop for other in (rest sublist)
                    collect (list item other))))

(defun problem1 ()
  (let ((lower-bound (make-v2 :x 200000000000000 :y 200000000000000))
        (upper-bound (make-v2 :x 400000000000000 :y 400000000000000)))
    (loop for (a b) in (2-combinations *ray2s*)
          when (rays-intersect-in-bounds-p a b lower-bound upper-bound)
          count t)))

(print (problem1))

;;; Problem 2

; first let's look for sets of parallel hailstones. for all combinations of rays,
; compare their velocities. if one is a multiple of the other, they are parallel,
; and may also be collinear.
(defun v3-parallel-p (a b)
  (= (/ (v3-x a) (v3-x b))
     (/ (v3-y a) (v3-y b))
     (/ (v3-z a) (v3-z b))))

(defun look-for-parallel-rays ()
  (remove nil
          (loop for (a b) in (2-combinations *ray3s*)
                collect (v3-parallel-p (ray3-vel a) (ray3-vel b)))))

; there aren't any such pairs. too bad, that would have been sweet.
