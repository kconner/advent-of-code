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

(defvar *initial-pulse*
  (make-pulse :destination :broadcaster
              :payload (make-pulse-payload :source :button
                                           :highness nil)))

(defun press-button-and-simulate (module-hash pulse-queue &key traced-destination)
  (setf (fill-pointer pulse-queue) 0)
  (vector-push *initial-pulse* pulse-queue)
  (let ((traced-bits nil))
    (loop for pulse-queue-cursor from 0
          while (< pulse-queue-cursor (length pulse-queue))
          for pulse = (aref pulse-queue pulse-queue-cursor)
          do 
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
            (when (and traced-destination (eq destination traced-destination))
              (push (if highness 1 0) traced-bits))
            (loop for new-pulse in next-pulses
                  do (vector-push new-pulse pulse-queue))))
    traced-bits))

(defun pulse-queue->counts (pulse-queue)
  (let* ((pulse-count (length pulse-queue))
         (high-pulse-count (count-if (lambda (pulse)
                                       (pulse-payload-highness
                                         (pulse-payload pulse))) pulse-queue))
         (low-pulse-count (- pulse-count high-pulse-count)))
    (list low-pulse-count high-pulse-count)))

(defun pulse-counts-over-sequence ()
  (let ((module-hash (make-module-hash))
        (pulse-queue (make-array 1000000 :fill-pointer 0 :adjustable t)))
    (loop repeat 1000
          do (press-button-and-simulate module-hash pulse-queue)
          collecting (pulse-queue->counts pulse-queue) into counts
          finally (return (apply #'mapcar #'+ counts)))))

(defun problem1 ()
  (apply #'* (pulse-counts-over-sequence)))

(print (problem1))

;;; Problem 2

; rx <- &dn <- &fc <- &cc <-> %… <- broadcaster
;       &dn <- &xp <- &pm <-> %… <- broadcaster
;       &dn <- &fh <- &bd <-> %… <- broadcaster
;       &dn <- &dd <- &rs <-> %… <- broadcaster

; rx gets a low pulse at the first moment that dn's inputs are all high at once.
; each input to dn, such as fc, has only one input itself so acts as a simple NOT.
; therefore, when cc, pm, bd, and rs have all signaled low more recently than high,
; fc, xp, fh, and dd have all signaled high more recently than low,
; and dn has most recently signaled low to rx.

; so the problem can be simplified to: when do fc, xp, fh, and dd all receive low at once?

; first detect repeating patterns in signals to each of fc, xp, fh, and dd.
; i think there might be repeating cycles every 4096 button presses,
; because each of these conjunctions is influenced by an isolated series of 12 flip-flops.

; then play those patterns forward all by themselves and count cycles until they are all low.

(defun trace-low-pulses-to-module (module-name)
  (let ((pulse-queue (make-array 1000000 :fill-pointer 0 :adjustable t))
        (module-hash (make-module-hash)))
    (loop for index from 1 to 10000
          do (let ((traced-bits
                     (press-button-and-simulate module-hash
                                                pulse-queue
                                                :traced-destination module-name)))
               (when (and (not (null traced-bits))
                          (find 0 traced-bits :test #'=))
                 (print (list index traced-bits)))))))

(trace-low-pulses-to-module :fc) ; 3917
(trace-low-pulses-to-module :xp) ; 3919
(trace-low-pulses-to-module :fh) ; 4027
(trace-low-pulses-to-module :dd) ; 4003

(print (* 3917 3919 4027 4003))
