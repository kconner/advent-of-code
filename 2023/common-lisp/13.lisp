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

(defun flip-char (char)
  (code-char (- 81 (char-code char))))

(defun print-pattern (pattern)
  (loop for line in pattern
        do (print line))
  (print ""))

; Loop over all characters of all lines of the pattern.
; Flip the character in place, then check the pattern-score.
; - When it's not nil, return it.
; Flip the character back and continue the iteration.
(defun unsmudged-pattern-score (pattern)
  ; (print-pattern pattern)
  (let ((smudged-score (pattern-score pattern)))
    (loop for line in pattern
          for line-index from 0
          do (loop for char across line
                   for char-index from 0
                   do (progn
                        (setf (elt line char-index) (flip-char char))
                        (print (list line-index char-index))
                        (print-pattern pattern)
                        (print-pattern (transpose pattern))
                        (let ((score (pattern-score pattern)))
                          (setf (elt line char-index) char)
                          (when score (print score))
                          (when (and score (not (equal score smudged-score)))
                            (return-from unsmudged-pattern-score score))))))))

(unsmudged-pattern-score (car *patterns*))
; (mapcar #'unsmudged-pattern-score *patterns*)
; (apply #'+ (mapcar #'unsmudged-pattern-score *patterns*))

(setf p5 (elt *patterns* 5))
(print-pattern p5)
(unsmudged-pattern-score p5)

