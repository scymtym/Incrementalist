(cl:in-package #:incrementalist.test)

(defun bug ()
  (let* ((line   (make-instance 'cluffer-standard-line:closed-line))
         (buffer (make-instance 'cluffer-standard-buffer:buffer :initial-line line))
         (cursor (make-instance 'cluffer-standard-line:right-sticky-cursor))
         (cache (make-instance 'incrementalist:cache :cluffer-buffer buffer))
         (analyzer (make-instance 'incrementalist::analyzer :cache cache
                                                            :buffer buffer
                                                            :lines (incrementalist:lines cache))))
    (labels ((update ()
               (incrementalist:update-cache analyzer)
               (incrementalist:map-wads-and-spaces
                cache
                0 (1- (cluffer:line-count buffer))
                (lambda (&rest args)) (lambda (&rest args))))
             (do-and-update (thunk)
               (funcall thunk)
               (update))
             (insert* (character)
               (do-and-update (lambda () (cluffer:insert-item cursor character)))))
      ;; Insert some stuff
      (cluffer:attach-cursor cursor line)
      (do-and-update (lambda () (cluffer:split-line cursor)))
      (insert* #\#)
      (insert* #\+)

      ;; Trigger problem
      (cluffer:detach-cursor cursor)
      (cluffer:attach-cursor cursor (cluffer:find-line buffer 0))
      (insert* #\()

      (clouseau:inspect (vector buffer cache analyzer)))))
