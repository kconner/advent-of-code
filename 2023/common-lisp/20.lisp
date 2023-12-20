(ql:quickload :cl-ppcre)

;;; Model

(defstruct pulse-payload source highness)

(defstruct pulse destination payload)

(defun hash-table-values (hash)
  (loop for value being the hash-values of hash
        collect value))

(defun make-pulses (destination-names payload)
  (mapcar (lambda (destination)
            (make-pulse :destination destination :payload payload))
          destination-names))

(defun make-simple-module (name destination-names)
  (lambda (source highness)
    (declare (ignore source))
    (make-pulses destination-names
                 (make-pulse-payload :source name :highness highness))))

(defun make-flip-flop-module (name destination-names)
  (let ((on nil))
    (lambda (source highness)
      (declare (ignore source))
      (if highness
          nil
          (progn
            (setf on (not on))
            (make-pulses destination-names
                         (make-pulse-payload :source name :highness on)))))))

(defun make-conjunction-module (name destination-names source-names)
  (let ((source-highnesses (make-hash-table)))
    (loop for source-name in source-names
          do (setf (gethash source-name source-highnesses) nil))
    (lambda (source highness)
      (setf (gethash source source-highnesses) highness)
      (make-pulses destination-names
                   (make-pulse-payload
                     :source name
                     :highness (notevery #'identity
                                      (hash-table-values source-highnesses)))))))

(defstruct module-spec type-signifier name destination-names)

;;; Parsing

(defun file->lines (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          collect line)))

(defun string->keyword (string)
  (intern (string-upcase string) :keyword))

(defun line->module-spec (line)
  (multiple-value-bind (match captures)
    (ppcre:scan-to-strings "^([%&]?)([a-z]+) -> (.*)$" line)
    (declare (ignore match))
    (let ((type-signifier (aref captures 0))
          (name (string->keyword (aref captures 1)))
          (destination-names (mapcar #'string->keyword
                                     (ppcre:split "\\W+" (aref captures 2)))))
      (make-module-spec :type-signifier type-signifier
                        :name name
                        :destination-names destination-names))))

(defun module-specs->sources-by-destination (specs)
  (let ((sources-by-destination (make-hash-table)))
    (loop for spec in specs
          do (loop for destination-name in (module-spec-destination-names spec)
                   do (push (module-spec-name spec)
                            (gethash destination-name sources-by-destination))))
    sources-by-destination))

(defun module-spec->module (spec sources-by-destination)
  (let ((type-signifier (module-spec-type-signifier spec))
        (name (module-spec-name spec))
        (destination-names (module-spec-destination-names spec)))
    (cond
      ((equal type-signifier "")
       (make-simple-module name destination-names))
      ((equal type-signifier "%")
       (make-flip-flop-module name destination-names))
      ((equal type-signifier "&")
       (make-conjunction-module name
                                destination-names
                                (gethash name sources-by-destination))))))

(defparameter *module-specs*
  (mapcar #'line->module-spec (file->lines "20.txt")))

(defun make-module-hash ()
  (let ((sources-by-destination
          (module-specs->sources-by-destination *module-specs*))
        (module-hash (make-hash-table)))
    (loop for spec in *module-specs*
          do (setf (gethash (module-spec-name spec) module-hash)
                   (module-spec->module spec sources-by-destination)))
    module-hash))

;;; Problem 1

(defun press-button-and-simulate (module-hash)
  (let ((pulse-queue (make-array 1000 :fill-pointer 0 :adjustable t)))
    (vector-push-extend (make-pulse :destination :broadcaster
                                    :payload (make-pulse-payload :source :button
                                                                 :highness nil))
                        pulse-queue)
    (loop with pulse-queue-cursor = 0
          while (< pulse-queue-cursor (fill-pointer pulse-queue))
          for pulse = (aref pulse-queue pulse-queue-cursor)
          do (progn
               (incf pulse-queue-cursor)
               (let* ((destination (pulse-destination pulse))
                      (module (gethash destination module-hash))
                      (payload (pulse-payload pulse))
                      (source (pulse-payload-source payload))
                      (highness (pulse-payload-highness payload))
                      (next-pulses (if module
                                       (funcall module
                                                source
                                                highness)
                                       nil)))
                 (loop for new-pulse in next-pulses
                       do (vector-push-extend new-pulse pulse-queue)))))
    pulse-queue))

(defun pulse-queue->counts (pulse-queue)
  (let* ((pulse-count (length pulse-queue))
         (high-pulse-count (count-if (lambda (pulse)
                                       (pulse-payload-highness
                                         (pulse-payload pulse))) pulse-queue))
         (low-pulse-count (- pulse-count high-pulse-count)))
    (list low-pulse-count high-pulse-count)))

(defun pulse-counts-over-sequence ()
  (let ((module-hash (make-module-hash)))
    (loop repeat 1000
          collecting (pulse-queue->counts
                       (press-button-and-simulate module-hash)) into counts
          finally (return (apply #'mapcar #'+ counts)))))

(defun problem1 ()
  (apply #'* (pulse-counts-over-sequence)))

(print (problem1))

;;; Problem 2

(defun low-pulse-to-rx-p (pulse)
  (and (equal (pulse-destination pulse) :rx)
       (not (pulse-payload-highness (pulse-payload pulse)))))

(defun problem2 ()
  (let ((module-hash (make-module-hash)))
    (loop for count from 1
          for pulse-queue = (press-button-and-simulate module-hash)
          until (find-if #'low-pulse-to-rx-p pulse-queue)
          finally (return count))))

(print (problem2))
