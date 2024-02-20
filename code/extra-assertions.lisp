(cl:in-package #:incrementalist)

;;; Model

(defmethod map-children :around ((function t) (node t))
  (let ((seen     (make-hash-table :test #'eq))
        (previous nil))
    (flet ((do-it (child)
             ;; Duplicates
             (assert (not (gethash child seen)))
             (setf (gethash child seen) t)
             ;; Buffer location order
             (when previous
               ;; TODO would be nice to assert this
               ;; (assert (wad-ends-after-wad-p previous child))
               )
             (setf previous child)
             ;; Original function
             (funcall function child)))
      (call-next-method #'do-it node))))

(defun check-absolute-wad-with-relative-descendants (wad)
  (assert (not (relative-p wad)))
  (labels ((check-child (child)
             (assert (relative-p child))
             (map-children #'check-child child)))
    (map-children #'check-child wad))
  wad)

(defun check-absolute-line-numbers (top-level-wad)
  ;; Make sure the wad itself is absolute, so that we need to compute
  ;; the absolute line numbers only of its children.
  (assert (not (relative-p top-level-wad)))
  (assert (eql (absolute-start-line-number top-level-wad)
               (start-line top-level-wad)))
  (labels ((process-children (relative-wads offset)
             (loop with base = offset
                   for wad in relative-wads
                   for absolute-start-line-number = (+ base (start-line wad))
                   do (assert (eql (absolute-start-line-number wad)
                                   absolute-start-line-number))
                      (process-wad wad base absolute-start-line-number)
                      (setf base absolute-start-line-number)))
           (process-wad (wad parent-start-line-number wad-start-line-number)
             (let ((children (children wad)))
               (assert (every #'relative-p children))
               (process-children children wad-start-line-number))
             (loop for error-wad in (errors wad)
                   for absolute-start-line-number = (if (relative-p error-wad) ; TODO can we make errors always relative?
                                                        (+ parent-start-line-number (start-line error-wad))
                                                        (start-line error-wad))
                   do (assert (eql (absolute-start-line-number error-wad)
                                   absolute-start-line-number)))))
    (process-wad top-level-wad nil (start-line top-level-wad))))

;;; Reader

(defun after-adjust-result-class (result)
  (labels ((check-wad (result)
             (assert (not (relative-p result)))
             (assert (eq (absolute-start-line-number result) (start-line result)))
             (let ((children (children result)))
               (cond ((null children))
                     ((some #'relative-p children)
                      (check-absolute-line-numbers result)) ; asserts that children are relative
                     (t
                      (mapc #'check-wad children))))))
    (when (typep result 'wad)
      (check-wad result))))
#+sbcl (trace adjust-result-class :report      nil
                                  :print-after (progn
                                                 (after-adjust-result-class
                                                  (sb-debug:arg 0))
                                                 (values)))

(defun before-adjust-result (extra-children)
  (when extra-children
    (mapc (lambda (child)
            (check-absolute-wad-with-relative-descendants child))
          extra-children)))
#+sbcl (trace adjust-result :report nil
                            :print  (progn
                                      (before-adjust-result (sb-debug:arg 4))
                                      (values)))

(defmethod eclector.parse-result:make-expression-result
    :around ((client client) (result t) (children t) (source t))
  ;; When WADs appear as children, they have to be absolute WADs while
  ;; their respective descendants have to be relative (as always). The
  ;; purpose of checking this recursively here is mainly to catch
  ;; inadvertent mutation after the creation of the WADs.
  (mapc (lambda (child)
          (when (typep child 'wad)
            (check-absolute-wad-with-relative-descendants child)))
        children)
  ;; The return value must be an absolute WAD with relative
  ;; descendants. The returned WAD either remains absolute and becomes
  ;; a top-level WAD or becomes the child of another WAD and is made
  ;; relative at that point.
  (check-absolute-wad-with-relative-descendants (call-next-method)))

(defmethod concrete-syntax-tree:reconstruct
    :around ((client client) (expression t) (cst t) &key default-source)
  (declare (ignore default-source))
  (let ((result (call-next-method)))
    (assert (or (null (cst:source result)) (typep result 'wad)))
    (when (and (typep result 'wad) (not (parent result)))
      (check-absolute-wad-with-relative-descendants result))
    ; (format *trace-output* "~&From reconstruct~%~2@T~A~%~2@T~A~%~2@T~A~%" expression cst default-source)
    ; (print-wad-tree result)
    ; (print-cst result)
    result))

;;; Cache

(defun before-push-to-worklist (cache wad)
  (declare (ignore cache))
  (check-absolute-wad-with-relative-descendants wad))
#+sbcl (trace push-to-worklist :report nil
                               :print  (progn
                                         (before-push-to-worklist
                                          (sb-debug:arg 0) (sb-debug:arg 1))
                                         (values)))
(defun before-push-to-residue (cache wad)
  (declare (ignore cache))
  (check-absolute-wad-with-relative-descendants wad))
#+sbcl (trace push-to-residue :report nil
                              :print  (progn
                                        (before-push-to-residue
                                         (sb-debug:arg 0) (sb-debug:arg 1))
                                        (values)))

(defun after-pop-from-worklist (result)
  (check-absolute-wad-with-relative-descendants result))
#+sbcl (trace pop-from-worklist :report      nil
                                :print-after (progn
                                               (after-pop-from-worklist
                                                (sb-debug:arg 0))
                                               (values)))

#+maybe (defun adjust-wad (wad amount)
          (flet ((adjust (wad)
                   (incf (start-line wad) amount)
                   (when (not (relative-p wad))
                     (setf (absolute-start-line-number wad) (start-line wad)))))
            (adjust wad)

            (labels ((invalidate-children (wad)
                       (when (relative-p wad)
                         (setf (slot-value wad '%absolute-start-line-number) :invalid))
                       (map-children #'invalidate-children wad)))
              (invalidate-children wad))

            (let ((errors (errors wad)))
              (unless (null errors)
                (mapc #'adjust errors)))))

(defun after-cached-wad (result)
  (when result
    (check-absolute-wad-with-relative-descendants result)
    (assert (null (parent result)))
    ; (format *trace-output* "~&From cache~%")
    ; (print-wad-tree result)
    ; (print-cst result)
    ))
#+sbcl (trace cached-wad :report      nil
                         :print-after (progn
                                        (after-cached-wad (sb-debug:arg 0))
                                        (values)))
