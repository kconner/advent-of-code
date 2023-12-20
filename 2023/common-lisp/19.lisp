(ql:quickload :cl-ppcre)

;;; Model

(defvar |x|)
(defvar |m|)
(defvar |a|)
(defvar |s|)

(defun A () t)

(defun R () nil)

(defun workflow-body (rules) 
  (multiple-value-bind (match captures)
    (ppcre:scan-to-strings "^([a-z]+)([<>])([0-9]+):([ARa-z]+),(.*)$" rules)
    (if match
        `(if (,(intern (aref captures 1))
               ,(intern (aref captures 0))
               ,(parse-integer (aref captures 2)))
             (,(intern (aref captures 3)))
             ,(workflow-body (aref captures 4)))
        `(,(intern rules)))))

(defun make-workflow (line)
  (multiple-value-bind (match captures)
    (ppcre:scan-to-strings "^([^{]+){([^}]+)}$" line)
    (declare (ignore match))
    `(defun ,(intern (aref captures 0)) ()
       ,(workflow-body (aref captures 1)))))

(defstruct part x m a s)

;;; Parsing

(defun file->lines (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          collect line)))

(defun lines->paragraphs (lines)
  (loop for line in lines
        with groups = nil
        with group-lines = nil
        do (if (equal line "")
               (progn
                 (push (reverse group-lines) groups)
                 (setf group-lines nil))
               (push line group-lines))
        finally (return (reverse (push (reverse group-lines) groups)))))

(defparameter *paragraphs* (lines->paragraphs (file->lines "19.txt")))

(defun line->part (line)
  (multiple-value-bind (match captures)
    (ppcre:scan-to-strings "^{x=([0-9]+),m=([0-9]+),a=([0-9]+),s=([0-9]+)}$" line)
    (declare (ignore match))
    (make-part :x (parse-integer (aref captures 0))
               :m (parse-integer (aref captures 1))
               :a (parse-integer (aref captures 2))
               :s (parse-integer (aref captures 3)))))

(defparameter *parts* (mapcar #'line->part (second *paragraphs*)))

(declaim (sb-ext:muffle-conditions cl:style-warning))
(dolist (line (first *paragraphs*))
  (eval (make-workflow line)))
(declaim (sb-ext:unmuffle-conditions cl:style-warning))

;;; Problem 1

(defun part-accepted-p (part)
  (let ((|x| (part-x part))
        (|m| (part-m part))
        (|a| (part-a part))
        (|s| (part-s part)))
    (declare (sb-ext:muffle-conditions cl:style-warning))
    (|in|)))

(defun problem1 ()
  (loop for part in *parts*
        when (part-accepted-p part)
        sum (+ (part-x part) (part-m part) (part-a part) (part-s part))))

(print (problem1))

;;; Problem 2

(defstruct range low high)

(defvar *empty-range* (make-range :low -1 :high -1))
(defvar *default-range* (make-range :low 1 :high 4001))

(defun range-count (range)
  (- (range-high range)
     (range-low range)))

(defun partition-range (range split)
  (let ((low (range-low range))
        (high (range-high range)))
    (cond ((<= split low)
           (values *empty-range* range))
          ((<= high split)
           (values range *empty-range*))
          (:else
            (values (make-range :low low :high split)
                    (make-range :low split :high high))))))

(defvar |x-2|)
(defvar |m-2|)
(defvar |a-2|)
(defvar |s-2|)

(defun R-2 () 0)

(defun A-2 ()
  (* (range-count |x-2|)
     (range-count |m-2|)
     (range-count |a-2|)
     (range-count |s-2|)))

(defun append-2 (string)
  (concatenate 'string string "-2"))

(defun workflow-2-body (rules) 
  (multiple-value-bind (match captures)
    (ppcre:scan-to-strings "^([a-z]+)([<>])([0-9]+):([ARa-z]+),(.*)$" rules)
    (if match
        `(multiple-value-bind
           ,(if (equal (aref captures 1) "<")
                '(within without)
                '(without within))
           (partition-range ,(intern (append-2 (aref captures 0)))
                            ,(+ (parse-integer (aref captures 2))
                                (if (equal (aref captures 1) "<") 0 1)))
           (+ (let ((,(intern (append-2 (aref captures 0))) within))
                ,(workflow-2-body (aref captures 3)))
              (let ((,(intern (append-2 (aref captures 0))) without))
                ,(workflow-2-body (aref captures 4))))
           )
        `(,(intern (append-2 rules))))))

(defun make-workflow-2 (line)
  (multiple-value-bind (match captures)
    (ppcre:scan-to-strings "^([^{]+){([^}]+)}$" line)
    (declare (ignore match))
    `(defun ,(intern (append-2 (aref captures 0))) ()
       ,(workflow-2-body (aref captures 1)))))

(declaim (sb-ext:muffle-conditions cl:style-warning))
(dolist (line (first *paragraphs*))
  (eval (make-workflow-2 line)))
(declaim (sb-ext:unmuffle-conditions cl:style-warning))

(defun problem2 ()
  (let ((|x-2| *default-range*)
        (|m-2| *default-range*)
        (|a-2| *default-range*)
        (|s-2| *default-range*))
    (declare (sb-ext:muffle-conditions cl:style-warning))
    (|in-2|)))

(print (problem2))
