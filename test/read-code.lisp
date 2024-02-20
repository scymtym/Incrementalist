(cl:in-package #:incrementalist.test)

(in-suite :incrementalist)

(defun progress-reporter (&key (stream *trace-output*) (indicator #\.))
  (let ((i 0)
        start-time)
    (lambda (&optional finalp)
      (when (zerop i)
        (setf start-time (get-internal-real-time))
        (format stream "~&;   "))
      (when (zerop (mod i 1000))
        (when (and (plusp i)
                   *print-right-margin*
                   (zerop (mod (/ i 1000) *print-right-margin*)))
          (format stream "~&;   "))
        (write-char indicator stream)
        (force-output stream))
      (incf i)
      (when finalp
        (let ((end-time (get-internal-real-time)))
          (values (/ (- end-time start-time)
                     internal-time-units-per-second)
                  i))))))

(defun insert-then-delete (content)
  (let (count insert-time delete-time)
    (multiple-value-bind (analyzer cache buffer cursor)
        (prepared-analyzer "")
      ;; Insert
      (loop with reporter = (progress-reporter :indicator #\+ :stream *trace-output*)
            for character across content
            do (funcall reporter)
            do (case character
                 (#\Newline (cluffer:split-line cursor))
                 (t         (cluffer:insert-item cursor character)))
               (let ((result (inc:update-cache analyzer)))
                 ;; TODO get buffer items for each wad? just top-level?
                 (finishes result))
            finally (setf (values insert-time count) (funcall reporter t)))
      ;; Delete
      (cluffer:detach-cursor cursor)
      (cluffer:attach-cursor cursor (cluffer:find-line buffer 0))
      (loop with reporter = (progress-reporter :indicator #\- :stream *trace-output*)
            while (or (> (cluffer:line-count buffer) 1) ; efficiency hack
                      (plusp (cluffer:item-count buffer)))
            for i from 0
            do (funcall reporter)
            do (if (cluffer:end-of-line-p cursor)
                   (cluffer:join-line cursor)
                   (cluffer:delete-item cursor))
               (let ((result (update-cache analyzer cache)))
                 (finishes result))
            finally (setf delete-time (funcall reporter t))))
    (values count insert-time delete-time)))

(defun insert-then-delete-file (filename)
  (format *trace-output* "~&; ~A~%" (file-namestring filename))
  (let ((content (a:read-file-into-string filename)))
    (multiple-value-bind (count insert-time delete-time)
        (insert-then-delete content)
      (format *trace-output* "~&;   insert: ~,2F s~@[ (~:D/s)~] ~@
                                    delete: ~,2F s~@[ (~:D/s)~]~%"
              insert-time (when (plusp insert-time) (floor count insert-time))
              delete-time (when (plusp delete-time) (floor count delete-time))))))

(test read-code
  "Incrementally insert and delete contents of all source files, parsing
after each change."
  (let ((fiveam:*test-dribble* nil)
        (*print-right-margin*  80))
    (map-all-system-files #'insert-then-delete-file)))

