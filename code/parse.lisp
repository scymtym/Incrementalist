(cl:in-package #:incrementalist)

(defun last-descendant-source (cst)
  ;; TODO must consider wad children
  (loop :for cst* = cst :then (cst:rest cst*)
        :for end-source = (or (cst:source cst*)
                              (when (typep cst* 'wad)
                                (cons (cons (absolute-start-line-number cst*)
                                            (start-column cst*))
                                      (cons (+ (absolute-start-line-number cst*) (height cst*))
                                            (end-column cst*))))
                              (when (cst:consp cst*)
                                (or (last-descendant-source (cst:rest cst*))
                                    (last-descendant-source (cst:first cst*))))
                              end-source)
        :do (format *trace-output* "maybe first ~A with source ~A~%"
                    cst* (cst:source cst*))
        :until (cst:atom cst*)
        :finally (format *trace-output* "~A with ~A => ~A~%"
                         cst* (cst:source cst*) end-source)
                 (return end-source)))

(defun cst-source (cst)
  (alexandria:if-let ((source (cst:source cst)))
    source
    (when (cst:consp cst)
      (alexandria:when-let ((first (cst:source (cst:first cst))))
        (let ((last (last-descendant-source cst)))
          (format *trace-output* "first ~S last ~S~%" first last)
          (values first (or last first)))))))

(defun error-location (source)
  (typecase source
    (wad
     (let* ((line   (absolute-start-line-number source))
            (column (start-column source))
            (height (height source))
            (width  (- (end-column source) column)))
       (values line column height width)))
    (cst:cst
     (multiple-value-bind (start-source end-source) (cst-source source)
       (when start-source
         (destructuring-bind ((first-start-line . first-start-column)
                              . first-end)
             start-source
           (declare (ignore first-end))
           (destructuring-bind (last-start . (last-end-line . last-end-column))
               end-source
             (declare (ignore last-start))
             (values first-start-line
                     first-start-column
                     (- last-end-line first-start-line)
                     (- last-end-column first-start-column)))))))))


(defun make-error-wad (condition start-line start-column height width) ; TODO pass end-column instead of width?
  (let ((end-column (+ start-column width)))
    (make-wad 'error-wad :relative-p     nil
                         :start-line     start-line
                         :start-column   start-column
                         :height         height
                         :end-column     end-column

                         :absolute-start-line-number start-line

                         :condition      condition)))

;;; PARSE-AND-CACHE collects Eclector errors in *ERRORS* during
;;; parsing and ADD-CHILDREN adds the resulting ERROR-WADs to the
;;; appropriate nodes in the wad tree.
(defvar *errors*)

;;; Establish a handler which collects READER-ERRORs into *ERRORS* and
;;; recovers around BODY.
(defmacro with-error-recording (() &body body)
  `(handler-bind
       ((reader-error
          (lambda (condition)
            (destructuring-bind (line . column)
                (eclector.base:stream-position condition)
              (let* ((start-column (max 0 (+ column (eclector.base:position-offset condition))))
                     (width        (eclector.base:range-length condition))
                     (error-wad    (make-error-wad
                                    condition line start-column 0 width)))
                                        ; (setf (slot-value error-wad '%absolute-start-line-number) line) ;; TODO
                (push error-wad *errors*)))
            (eclector.reader:recover))))
     ,@body))

#+unused (defun merge-children (children extra-children)
  (break)
  (merge 'list (copy-list children) (copy-list extra-children)
         #'wad-starts-before-wad-p))

;;; Add the wads in EXTRA-CHILDREN to WAD.
#+unused (defun add-children (wad extra-children)
  (break)
  ;; Make descendants of WAD absolute so we can compute the
  ;; appropriate insertion points for EXTRA-CHILDREN.
  ;; TODO Can we just compute the absolute line numbers instead?
  (labels ((rec (wad)
             (let ((children (children wad)))
               (when children
                 (when (every #'relative-p children)
                   (make-absolute children (start-line wad)))
                 (map nil #'rec children)))))
    (rec wad))
  (let* (;; Merge children of WAD and EXTRA-CHILDREN based on their
         ;; positions.
         (children (merge-children (children wad) extra-children))
         ;; Possibly push some elements of EXTRA-CHILDREN into
         ;; existing children of WAD. A wad E in EXTRA-CHILDREN must
         ;; become a child of an existing child C of WAD if the source
         ;; range of E is contained in the source range of C.
         (children* (loop for last-child = nil then child
                          for last-end-line = nil then (end-line last-child)
                          for last-end-column = nil then (end-column last-child) ; TODO consider lines
                          for child in children
                          if (and (member child extra-children)
                                  (not (null last-end-column))
                                  (or (< (end-line child) last-end-line)
                                      (and (= (end-line child) last-end-line)
                                           (< (start-column child) last-end-column)
                                           (<= (end-column child) last-end-column))))
                            do (add-children last-child (list child))
                          else if (cond ((not (member child extra-children))
                                         t)
                                        ((and (%position<= (end-line child) (end-column child)
                                                           (end-line wad) (end-column wad))
                                              (not (relative-p wad)))
                                         (assert (not (relative-p wad)))
                                         t)
                                        (t
                                         (warn "Dropping ~A (~A)~&  parent         ~A~&  previous child ~A"
                                               child (class-name (class-of (condition* child)))
                                               wad
                                               last-child)
                                         nil))
                                 collect child)))
    (reinitialize-instance wad :children children*)))

(defmethod reader:read-maybe-nothing :around
    ((client client) (stream analyzer) eof-error-p eof-value)
  (let ((cached (cached-wad stream)))
    (if (or (null cached)
            ;; Can't use zero length cached wad (can happen for
            ;; error-wad) since the ADVANCE-STREAM-TO-BEYOND-WAD would
            ;; not advance in that case and the read loop would not
            ;; make any progress.
            (and (= (start-line cached) (end-line cached))
                 (= (start-column cached) (end-column cached))))
        ;; Nothing has been cached, so call
        ;; READ-MAYBE-NOTHING. Collect errors in *ERRORS* and
        ;; integrate them into the wad tree.
        (let ((*errors* '()))
          (multiple-value-bind (object kind result)
              (call-next-method)
            ;; TODO could associate RESULT with any errors so the caller doesn't have to traverse the result tree as much when adding error children
            #+no (when *errors*
              (assert (not (relative-p result)))
              (compute-absolute-line-numbers result)
              ;; (add-extra-children result (reverse *errors*))
              #+no (add-children result (reverse *errors*)))
            (when (eq kind :object)
              (alexandria:when-let ((errors *errors*))
                ;; FIXME should not really be needed but the `absolute-start-line-number' `:before' method checks for this
                (mapc (lambda (error)
                        (assert (not (relative-p error)))
                        (when (relative-p result)
                          (absolute-to-relative error (absolute-start-line-number result)))
                        (setf (parent error) result))
                      errors)
                (setf (errors result) errors))

              (check-absolute-wad-with-relative-descendants result))
            (values object kind result)))
        ;; There is a cached wad for the current input position. Turn
        ;; the wad into appropriate return values, inject it into
        ;; Eclector's result stack and advance STREAM.
        (multiple-value-prog1
            (etypecase cached
              (skipped-wad (values nil              :skip   cached t))
              (error-wad   (values nil              :skip   cached t))
              (cst-wad     (values (cst:raw cached) :object cached t)))
          (check-absolute-wad-with-relative-descendants cached)
          (push cached (first eclector.parse-result::*stack*)) ; HACK
          (advance-stream-to-beyond-wad stream cached)))))

(defun parse-and-cache (analyzer client)
  ;; Use ECLECTOR.READER:READ-MAYBE-NOTHING to read either a single
  ;; skipped input or an object. Both are pushed into the prefix of
  ;; the cache that is associated with ANALYZER.
  ;;
  ;; For any errors during parsing, WITH-ERROR-RECORDING creates an
  ;; ERROR-WAD records it in *ERRORS*, then asks Eclector to perform
  ;; the appropriate recovery. The READ-MAYBE-NOTHING method takes
  ;; care of integrating the collected ERROR-WADs into the wad tree.
  (let ((*errors* '()))           ; TODO this binding should be unused
    (multiple-value-bind (object kind wad)
        (with-error-recording ()
          (eclector.reader:read-maybe-nothing client analyzer nil nil))
      (case kind
        (:eof)             ; nothing to do for end of input
        (:whitespace)      ; nothing to do for whitespace
        (t                 ; got a tree of absolute wads. make relative
         (when (null wad)
           (assert (eq kind :skip)))
         (when wad
           (when *errors*
             (let ((*print-right-margin* 60)
                   (*print-pretty* t))
               (format *trace-output* "Errors: ~S~%" *errors*)))
           #+no (compute-absolute-line-numbers wad)
           #+no (add-extra-children wad *errors*)
           #+no (progn
                  (format *trace-output* "Before making relative~%")
                  (second-climacs-syntax-common-lisp::print-wad-tree wad *trace-output*)
                  (terpri *trace-output*))

           (assert (typep wad 'wad))
           (assert (not (relative-p wad)))
           (check-absolute-wad-with-relative-descendants wad)

           (labels ((rec (wad)
                      ;; FIXED I think add-children messes things up
                      (when (relative-p wad)
                        (labels ((rec (desc)
                                   (assert (relative-p desc))
                                   (mapc (lambda (e) (assert (relative-p e))) (errors desc))
                                   (mapc #'rec (children desc))))
                          (rec wad)))
                      (unless (relative-p wad) ; TODO can it be relative due to caching or what?
                        (let (; (children (children wad))
                                        ; (errors   (errors wad))
                              )
                          (progn ; when children
                            ; (mapc #'rec children)
                            (let ((every-relative-p t))
                              (map-children (lambda (child)
                                              (when (not (relative-p child))
                                                (setf every-relative-p nil))
                                              (rec child))
                                            wad)
                             (unless every-relative-p ; (every #'relative-p children)
                               (make-relative (children wad) (start-line wad))))) ; TODO avoid consing the children list
                                        ; (absolute-to-relative wad (start-line wad))
                          #+no (when errors
                                 (unless (every #'relative-p errors)
                                   (mapc (alexandria:rcurry #'absolute-to-relative (start-line wad))))))
                        )))
             (handler-bind ((error (lambda (condition)
                                     ; (clouseau:inspect wad :new-process t)
                                     )))
               (rec wad)))

           #+no (progn
                  (format *trace-output* "After making relative~%")
                  (second-climacs-syntax-common-lisp::print-wad-tree wad *trace-output*)
                  (terpri *trace-output*))
           (push-to-prefix (cache analyzer) wad))))
      (values (cons object kind) wad)))) ; HACK

;;; S-Expression Client

(defclass s-expression-client (s-expression-syntax.concrete-syntax-tree::cst-client)
  ())

(defmethod s-expression-syntax.expression-grammar:typep*
    ((client s-expression-client)
     (thing  t)
     (type   t))
  (typep (s-expression-syntax.expression-grammar:naturalize client thing) type))

;;; CSTs

(defmethod s-expression-syntax.expression-grammar:naturalize
    ((client s-expression-client)
     (thing  cst:cst))
  (cst:raw thing))

(defmethod s-expression-syntax.expression-grammar:symbol-name*
    ((client s-expression-client)
     (symbol cst:cst)) ; not `atom-cst' because of labeled objects
  (let ((raw (cst:raw symbol)))
    (typecase raw
      (symbol-token (name raw))
      (t            (call-next-method)))))

(defmethod s-expression-syntax.expression-grammar:symbol-package*
    ((client s-expression-client)
     (symbol cst:cst)) ; not `atom-cst' because of labeled objects
  (let ((raw (cst:raw symbol)))
    (typecase raw
      (symbol-token (find-package (package-name raw)))
      (t            (call-next-method)))))

(defmethod s-expression-syntax.expression-grammar:typep*
    ((client s-expression-client)
     (thing  cst:cst) ; not `atom-cst' because of labeled objects
     (type   (eql 'symbol)))
  (let ((raw (cst:raw thing)))
    (typecase raw
      (symbol-token t)
      (t            (call-next-method)))))

(defmethod s-expression-syntax.expression-grammar:equal*
    ((client s-expression-client)
     (left   cst:cst) ; not `atom-cst' because of labeled objects
     (right  symbol))
  (let ((raw (cst:raw left)))
    (typecase raw
      (symbol-token
       (and (string= (cl:package-name (symbol-package right))
                     (package-name raw))
            (string= (cl:symbol-name right)
                     (name raw))))
      (t
       (call-next-method)))))

(defmethod s-expression-syntax.expression-grammar:eql*
    ((client s-expression-client)
     (left   cst:cst) ; not `atom-cst' because of labeled objects
     (right  symbol))
  (let ((raw (cst:raw left)))
    (typecase raw
      (symbol-token
       (and (string= (cl:package-name (symbol-package right))
                     (package-name raw))
            (string= (cl:symbol-name right)
                     (name raw))))
      (t
       (call-next-method)))))
