(ql:quickload :cl-ppcre)

;;; Model

(defstruct condition-record
  (springs nil :type string)
  (damaged-runs nil :type list))

;;; Parsing

(defun file-lines (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          collect line)))

(defparameter *lines* (file-lines "12.txt"))

(defvar *space-regex* (cl-ppcre:parse-string " "))
(defvar *comma-regex* (cl-ppcre:parse-string ","))
(defvar *unknown-spring-regex* (cl-ppcre:parse-string "\\?"))

(defun parse-condition-record (line)
  (let ((parts (cl-ppcre:split *space-regex* line)))
    (make-condition-record :springs (first parts)
                           :damaged-runs (mapcar #'parse-integer (cl-ppcre:split *comma-regex* (second parts))))))

;;; Problem 1

; unique permutations of a list with nonunique items
(defun permutations (damaged-count operational-count)
  (if (zerop damaged-count)
      (if (zerop operational-count)
          '(())
          (mapcar (lambda (x) (cons #\. x))
                  (permutations 0 (1- operational-count))))
      (if (zerop operational-count)
          (mapcar (lambda (x) (cons #\# x))
                  (permutations (1- damaged-count) 0))
          (append (mapcar (lambda (x) (cons #\# x))
                          (permutations (1- damaged-count) operational-count))
                  (mapcar (lambda (x) (cons #\. x))
                          (permutations damaged-count (1- operational-count)))))))

; memoized function call
(defun memoize (fn)
  (let ((cache (make-hash-table :test #'equal)))
    (lambda (&rest args)
      (multiple-value-bind (val foundp) (gethash args cache)
        (if foundp
            val
            (setf (gethash args cache) (apply fn args)))))))

(defparameter *memoized-permutations* (memoize #'permutations))

(defun unknown-spring-permutations (condition-record)
  (let* ((springs (condition-record-springs condition-record))
         (unknown-count (count #\? springs))
         (unknown-damaged-count (- (apply #'+ (condition-record-damaged-runs condition-record))
                                   (count #\# springs)))
         (unknown-operational-count (- unknown-count unknown-damaged-count)))
         (funcall *memoized-permutations* unknown-damaged-count unknown-operational-count)))
         
; for a given unknown spring permutation and a springs string,
; substitute the unknowns into the springs string
(defun substitute-unknowns (unknown-spring-permutation springs)
  (let ((replacements unknown-spring-permutation))
    (coerce (loop for c across springs
                  collect (if (char= c #\?)
                              (let ((replacement (car replacements)))
                                (setf replacements (cdr replacements))
                                replacement)
                              c))
            'string)))

; regex for the damaged run sequence such as /#{4}\.+#{1}\.+#{1}/
(defun damaged-run-validation-regex (damaged-runs)
  (cl-ppcre:parse-string (format nil "~{#{~A}~^.+~}" damaged-runs)))

; The count of different arrangements of operational and damaged springs
; is the count of unique orderings of N #s and M .s for which,
; after substituting them into the ? characters of the string,
; the string matches the line's damaged run validation regex.
(defun count-arrangements (line)
  (let* ((condition-record (parse-condition-record line))
         (validation-regex (damaged-run-validation-regex (condition-record-damaged-runs condition-record)))
         (unknown-spring-permutations (unknown-spring-permutations condition-record)))
    (count-if (lambda (unknown-spring-permutation)
                (cl-ppcre:scan validation-regex
                               (substitute-unknowns unknown-spring-permutation
                                                    (condition-record-springs condition-record))))
              unknown-spring-permutations)))

(defun problem1 ()
  (apply #'+ (mapcar #'count-arrangements *lines*)))

;;; Problem 2

