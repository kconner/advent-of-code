;;; Model

; A pattern is a list of line strings.

;;; Parsing

(defun file-lines (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          collect line)))

(defun split-lines-by-empty-string (lines)
  (loop for line in lines
        with groups = nil
        with group-lines = nil
        do (if (equal line "")
               (progn
                 (push (reverse group-lines) groups)
                 (setf group-lines nil))
               (push line group-lines))
        finally (return (reverse (push (reverse group-lines) groups)))))

(defparameter *patterns* (split-lines-by-empty-string (file-lines "13.txt")))

;;; Problem 1

(defun prefix-palindrome-center-index (seq)
  (let ((reflection (list (elt seq 0))))
    (loop for index from 1 to (/ (length seq) 2)
          for reflend from 2 by 2
          do (if (equal reflection (subseq seq index reflend))
                 (return index)
                 (push (elt seq index) reflection)))))

(defun palindrome-center-index (seq)
  (or (prefix-palindrome-center-index seq)
      (let ((index-of-reflection (prefix-palindrome-center-index (reverse seq))))
        (when index-of-reflection
          (- (length seq) index-of-reflection)))))

(defun string->list (s)
  (coerce s 'list))

(defun list->string (l)
  (coerce l 'string))

(defun transpose (list-of-strings)
  (let ((list-of-char-lists (mapcar #'string->list list-of-strings)))
    (mapcar #'list->string (apply #'mapcar #'list list-of-char-lists))))

(defun pattern-score (pattern)
  (let ((horizontal-score (palindrome-center-index pattern)))
    (if horizontal-score (* 100 horizontal-score)
        (palindrome-center-index (transpose pattern)))))

(apply #'+ (mapcar #'pattern-score *patterns*))

;;; Problem 2

