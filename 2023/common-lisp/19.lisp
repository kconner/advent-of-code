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
    (declare (special |x| |m| |a| |s|))
    (declare (sb-ext:muffle-conditions cl:style-warning))
    (|in|)))

(defun problem1 ()
  (loop for part in *parts*
        when (part-accepted-p part)
        sum (+ (part-x part) (part-m part) (part-a part) (part-s part))))

(print (problem1))

;;; Problem 2
