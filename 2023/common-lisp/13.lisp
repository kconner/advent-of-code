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

(defun prefix-palindrome-center-index (seq forbidden-result)
  (let ((reflection (list (elt seq 0))))
    (loop for index from 1 to (/ (length seq) 2)
          for reflend from 2 by 2
          do (if (and (not (= forbidden-result index))
                      (equal reflection (subseq seq index reflend)))
                 (return index)
                 (push (elt seq index) reflection)))))

(defun palindrome-center-index (seq forbidden-result)
  (let ((seqlength (length seq)))
    (or (prefix-palindrome-center-index seq forbidden-result)
        (let ((index-of-reflection
                (prefix-palindrome-center-index (reverse seq)
                                                (- seqlength forbidden-result))))
          (when index-of-reflection
            (- seqlength index-of-reflection))))))

(defun string->list (s)
  (coerce s 'list))

(defun list->string (l)
  (coerce l 'string))

(defun transpose (list-of-strings)
  (let ((list-of-char-lists (mapcar #'string->list list-of-strings)))
    (mapcar #'list->string (apply #'mapcar #'list list-of-char-lists))))

(defun pattern-score (pattern &optional (forbidden-score -1))
  (let ((horizontal-score
          (palindrome-center-index pattern
                                   (if (<= 100 forbidden-score )
                                       (/ forbidden-score 100)
                                       -1))))
    (if horizontal-score (* 100 horizontal-score)
        (palindrome-center-index (transpose pattern)
                                 (if (<= 100 forbidden-score)
                                     -1
                                     forbidden-score)))))

(print (apply #'+ (mapcar #'pattern-score *patterns*)))

;;; Problem 2

(defun flip-char (char)
  (code-char (- 81 (char-code char))))

; Loop over all characters of all lines of the pattern.
; Flip the character in place, then check the pattern-score.
; - When it's not nil, return it.
; Flip the character back and continue the iteration.
(defun unsmudged-pattern-score (pattern)
  (let ((forbidden-score (pattern-score pattern)))
    (loop for line in pattern
          for line-index from 0
          do (loop for char across line
                   for char-index from 0
                   do (progn
                        (setf (elt line char-index) (flip-char char))
                        (let ((score (pattern-score pattern forbidden-score)))
                          (setf (elt line char-index) char)
                          (when score
                            (return-from unsmudged-pattern-score score))))))))

(print (apply #'+ (mapcar #'unsmudged-pattern-score *patterns*)))

