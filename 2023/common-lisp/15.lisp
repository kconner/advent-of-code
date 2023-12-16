;;; Model

; Steps are a string list.

(defstruct lens label focal-length)

; A box is a lens list. The lens at the head is in the back of the box.
; Boxes are a hashtable of hash to box.

;;; Parsing

(defun file->lines (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          collect line)))

(defun line->steps (line)
  (loop for start = 0 then (1+ end)
        for end = (position #\, line :start start)
        while end
        collect (subseq line start end) into steps
        finally (return (append steps (list (subseq line start))))))

(defparameter *steps* (line->steps (car (file->lines "15.txt"))))

;;; Problem 1

(defun label->hash (label)
  (reduce (lambda (acc character)
            (incf acc (char-code character))
            (setf acc (* acc 17))
            (setf acc (mask-field (byte 8 0) acc))
            acc)
          label
          :initial-value 0)) 

(defun problem1 ()
  (reduce #'+ (mapcar #'label->hash *steps*)))

(print (problem1))

;;; Problem 2

(defun upsert-box-lens (box lens)
  (let ((matching-lens-index (position (lens-label lens)
                                       (mapcar #'lens-label box)
                                       :test #'string=)))
    (if matching-lens-index
        (progn
          (setf (nth matching-lens-index box) lens)
          box)
        (cons lens box))))

(defun steps->boxes (steps)
  (loop with boxes = (make-hash-table)
        for step in steps
        do (progn
             (let* ((op-index (or (position #\- step) (position #\= step)))
                    (op (aref step op-index))
                    (label (subseq step 0 op-index))
                    (hash (label->hash label))
                    (box (gethash hash boxes)))
               (case op
                 (#\- (setf (gethash hash boxes)
                            (remove-if (lambda (lens)
                                         (string= (lens-label lens) label))
                                       box
                                       :from-end t
                                       :count 1)))
                 (#\= (setf (gethash hash boxes)
                            (upsert-box-lens
                              box
                              (make-lens
                                :label label
                                :focal-length (parse-integer (subseq step (1+ op-index))))
                              ))))
               ))
        finally (return boxes)))

(defun boxes->power (boxes)
  (loop for hash from 0 below 256
        for box-number from 1
        summing (loop with box = (gethash hash boxes)
                      for lens in box
                      for index downfrom (length box)
                      summing (* box-number index (lens-focal-length lens)))))

(defun problem2 ()
  (boxes->power (steps->boxes *steps*)))

(print (problem2))
