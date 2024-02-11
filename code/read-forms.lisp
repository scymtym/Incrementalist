(cl:in-package #:incrementalist)

(defun read-forms (analyzer)
  (let ((cache (cache analyzer)))
    (with-accessors ((prefix prefix) (suffix suffix) (residue residue))
        cache
      ;; Position stream after last prefix wad (remember the cache
      ;; prefix is stored in reverse order) if any.
      (update-lines-cache analyzer (lines analyzer))
      (setf (values (line-number analyzer) (item-number analyzer))
            (if (null prefix)
                (values 0 0)
                (let ((first (first prefix)))
                  (values (end-line first) (end-column first)))))
      ;;
      (loop with client  = (make-instance 'client :stream* analyzer)
            with *cache* = cache
            for ((object . kind) result)
               = (multiple-value-list
                  (eclector.reader:call-as-top-level-read
                   client (lambda ()
                            (parse-and-cache analyzer client))
                   analyzer t nil nil))
            ;; If an object was read, process it.
            when (eq kind :object)
              do #+no (print-wad-tree result)
                 (let ((builder 'list #+no (make-instance 'iconoclast-builder:builder))
                       (s-expression-syntax.expression-grammar:*client*
                         (make-instance 's-expression-client)))
                   ;; TODO remove syntax errors from descendants of RESULT since the s-expression parse is not incremental
                   ;;      probably not needed though if we attach to the top-level wad (RESULT)
                   (handler-case
                       (let ((ast (s-expression-syntax:parse builder t result)))
                         #+no (let ((*standard-output* *trace-output*))
                                (iconoclast-print-tree:print-tree ast)
                                )
                         #+no (print-ast ast))
                     (s-expression-syntax:invalid-syntax-error (error)
                       (progn
                         (assert (not (relative-p result)))
                         (let ((source       (s-expression-syntax:expression error))
                               (attach-point result))
                           (multiple-value-bind (line column height width)
                               (error-location source)
                             (if (null line)
                                 (warn "Cannot attach error ~A to ~A" error attach-point)
                                 (let ((wad (make-error-wad error line column height width))) ; TODO width
                                   #+no (setf (slot-value wad '%absolute-start-line-number) line)
                                   ;; TODO
                                   (when (relative-p attach-point)
                                     (make-relative (list wad) (absolute-start-line-number attach-point)))

                                   #+no (add-extra-children source (list wad))
                                   (setf (parent wad) attach-point)
                                   (setf (errors attach-point) (merge 'list
                                                                      (list wad)
                                                                      (copy-list (errors attach-point))
                                                                      #'wad-starts-before-wad-p))
                                        ; (compute-absolute-line-numbers result) ; TODO mostly wasted work since push-to-prefix does this
                                   ))
                             )

                           #+old (cond ((typep source '(not wad)) ; TODO could use cst source here
                                        (warn "Cannot attach error ~A to ~A" error attach-point))
                                       (t
                                        (let* ((line   (absolute-start-line-number source))
                                               (column (start-column source))
                                               (height (height source))
                                               (width  (- (end-column source) column)))
                                          (let ((wad (make-error-wad error line column height width))) ; TODO width
                                            #+no (setf (slot-value wad '%absolute-start-line-number) line)
                                            ;; TODO
                                            (when (relative-p attach-point)
                                              (make-relative (list wad) (absolute-start-line-number attach-point)))

                                            #+no (add-extra-children source (list wad))
                                            (setf (parent wad) attach-point)
                                            (setf (errors attach-point) (merge 'list
                                                                               (list wad)
                                                                               (copy-list (errors attach-point))
                                                                               #'wad-starts-before-wad-p))
                                        ; (compute-absolute-line-numbers result) ; TODO mostly wasted work since push-to-prefix does this
                                            )
                                          ))))
                                        ; (compute-absolute-line-numbers result) ; TODO

                         ;; Back to relative
                         #+no (labels ((rec (wad)
                                         (unless (relative-p wad) ; TODO can it be relative due to caching or what?
                                           (let ((children (children wad)))
                                             (when children
                                               (map nil #'rec children)
                                               (break)
                                               (make-relative (remove-if #'relative-p children)
                                                              (start-line wad)))))))
                                (rec result)))

                       (format *trace-output* "~&Error: ~A at ~A~%"
                               error (s-expression-syntax:expression error)))))
              ;; If we reach EOF while reading whitespace, suffix must
              ;; be empty, and the residue is either empty or it
              ;; contains wads that should be removed.  If we do not
              ;; reach EOF, then we stop only if the current position
              ;; is that of the first parse result on the suffix.
              when (or (eq kind :eof)
                       (and (not (null suffix))
                            (position= (first suffix) analyzer)))
                do (setf residue '())
                   (return-from read-forms nil)
              ;; In case we skipped some whitespace, discard any wads
              ;; on the cache residue and cache suffix that are now
              ;; before the current stream position.
              unless (eq kind :whitespace)
                do (loop while (and (not (null residue))
                                    (position< (first residue) analyzer))
                         do (pop-from-residue cache))
                   (when (null residue)
                     (loop while (and (not (null suffix))
                                      (position< (first suffix) analyzer))
                           do (pop-from-suffix cache)))))))
