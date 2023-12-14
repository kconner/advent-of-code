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

;;;; Retry

(ql:quickload :cl-ppcre)

(defstruct condition-record
  (springs nil :type list)
  (damaged-runs nil :type list))

(defun file-lines (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          collect line)))

(defparameter *lines* (file-lines "12.txt"))

(defvar *space-regex* (cl-ppcre:parse-string " "))
(defvar *comma-regex* (cl-ppcre:parse-string ","))

; Match springs not including leading and trailing .s
; (defvar *trimmed-springs-regex* (cl-ppcre:parse-string "[^\\.].*[^\\.]"))

(defun parse-condition-record (line)
  (let ((parts (cl-ppcre:split *space-regex* line)))
    (make-condition-record :springs (string->list (first parts))
                           :damaged-runs (mapcar #'parse-integer (cl-ppcre:split *comma-regex* (second parts))))))

; Given a list of characters, trim conses whose car is #\. from the front of the list.
(defun skip-leading-dots (list)
  (loop while (and list (char= (car list) #\.))
        do (setf list (cdr list)))
  list)

(defparameter *search-for-runs-cache* (make-hash-table :test #'equal))

; To search, given springs known to begin with ? or # and runs known to be nonempty,
; - If the current character is ?,
;   - Return the sum of
;     - A test of the current run starting at this character and
;     - The recursion, dropping this character and trimming further .s.
; - Otherwise the current character is #, so
;   - Return a test of the current run starting at this character
(defun search-for-runs (characters runs)
  (let ((key (list characters runs)))
    (multiple-value-bind (val foundp) (gethash key *search-for-runs-cache*)
      (if foundp
          val
          (setf (gethash key *search-for-runs-cache*)
                (if (null characters)
                    0
                    (if (char= (car characters) #\?)
                        (+ (test-run characters runs)
                           (search-for-runs (skip-leading-dots (cdr characters)) runs))
                        (test-run characters runs))))))))

; To test a run starting at a character, given characters known to begin with ? or # and runs known to be nonempty,
; - Step forward through the number of characters given by the current run value.
;   - If any of these characters doesn't exist or is ., return 0.
; - If it passes,
;   - Drop the current run.
;   - If the runs are empty,
;     - If any #s remain, return +0 matches
;     - Else return +1 match
;   - Else
;     - If no characters remain or the next character is #, return 0.
;     - Else trim any leading .s and recurse to search.
(defun test-run (characters runs)
  (let ((characters characters) (runs runs)) ; remove when this works
    (loop repeat (pop runs)
          for character = (pop characters)
          do (when (or (null character)
                       (char= character #\.))
               (return-from test-run 0)))
    (if (null runs)
        (if (member #\# characters)
            0
            1)
        (if (or (null characters)
                (char= (car characters) #\#))
            0
            (search-for-runs (skip-leading-dots (cdr characters)) runs)))))

(defun count-arrangements (line)
  (let* ((condition-record (parse-condition-record line))
         (characters (condition-record-springs condition-record))
         (runs (condition-record-damaged-runs condition-record)))
    (clrhash *search-for-runs-cache*)
    (search-for-runs (skip-leading-dots characters) runs)))

(defun problem1 ()
  (apply #'+ (mapcar #'count-arrangements *lines*)))

;;; Problem 2

; Given a list of characters, return the list of all its items, then a ?, then all its items again, five times, but with no trailing ?.
(defun expanded-characters (characters)
  (let ((result characters))
    (loop repeat 4
          do (setf result (cons #\? result))
          do (setf result (append characters result)))
    result))

; Given a list, return the list of the list's items repeated 5 times.
(defun expanded-runs (runs)
  (let ((result runs))
    (loop repeat 4
          do (setf result (append runs result)))
    result))

(defun count-expanded-record-arrangements (line)
  (let* ((condition-record (parse-condition-record line))
         (characters (condition-record-springs condition-record))
         (runs (condition-record-damaged-runs condition-record)))
    (let ((result (search-for-runs (skip-leading-dots (expanded-characters characters))
                     (expanded-runs runs))))
      (clrhash *search-for-runs-cache*)
      result)))

(defun problem2 ()
  (apply #'+ (mapcar #'count-expanded-record-arrangements *lines*)))

(defun test ()
  (time (count-expanded-record-arrangements "???????.??? 1,3")))

