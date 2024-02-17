(cl:in-package #:incrementalist)

(defclass client (eclector.concrete-syntax-tree:definition-csts-mixin
                  eclector.concrete-syntax-tree:reference-csts-mixin
                  eclector.concrete-syntax-tree:cst-client)
  ;; TODO it would be nicer not to store the stream in the client like
  ;; this, but the method on make-expression-result needs the stream
  ;; and cannot access it in other ways.
  ((stream* :initarg :stream*
            :reader  stream*)))

;;; Feature expressions

(defmethod reader:check-feature-expression ((client client) (feature-expression t))
  t)

(defmethod reader:evaluate-feature-expression ((client client) (feature-expression t))
  nil)

;;; Read-time evaluation

(defmethod reader:evaluate-expression ((client client) (expression t))
  1)

;;; Token interpretation

(defmethod reader:call-reader-macro :around ((client       client)
                                             (input-stream t)
                                             (char         t)
                                             (readtable    t))
  (let ((values (multiple-value-list (call-next-method))))
    (typecase values
      (null
       (values))
      ((cons null null)
       (make-instance 'existing-symbol-token :name         "NIL"
                                             :package-name "COMMON-LISP"))
      (t
       (let ((result (first values)))
         (apply #'values
                (typecase result
                  (number (make-instance 'numeric-token :characters "TODO" :value result))
                  (t      result))
                (rest values)))))))

(defmethod reader:interpret-token :around
    ((client client) input-stream token escape-ranges)
  (let ((result (call-next-method)))
    (if (eq result eclector.reader::*consing-dot*) ; TODO not having `other-token' would resolve this automatically
        result
        (typecase result
          (token  result)
          (number (make-instance 'numeric-token :characters token :value result))
          (t      (make-instance 'other-token :characters token))))))

(defmethod reader:interpret-symbol-token
    ((client client) input-stream token position-package-marker-1 position-package-marker-2)
  (multiple-value-bind (package-designator symbol-name)
      (cond ((null position-package-marker-1)
             (values *package* token))
            ((null position-package-marker-2)
             (values (if (= position-package-marker-1 0)
                         "KEYWORD"
                         (subseq token 0 position-package-marker-1))
                     (subseq token (1+ position-package-marker-1))))
            (t
             (values (subseq token 0 position-package-marker-1)
                     (subseq token (1+ position-package-marker-2)))))
    (let ((package (find-package package-designator)))
      (if (null package)
          (make-instance 'non-existing-package-symbol-token
                         :package-name package-designator
                         :package-marker-1 position-package-marker-1
                         :package-marker-2 position-package-marker-2
                         :name symbol-name)
          (multiple-value-bind (symbol status)
              (find-symbol symbol-name package)
            (if (null status)
                (make-instance 'non-existing-symbol-token
                               :package-name (cl:package-name package)
                               :package-marker-1 position-package-marker-1
                               :package-marker-2 position-package-marker-2
                               :name symbol-name)
                (make-instance 'existing-symbol-token
                               :package-name (cl:package-name (symbol-package symbol))
                               :package-marker-1 position-package-marker-1
                               :package-marker-2 position-package-marker-2
                               :name (cl:symbol-name symbol))))))))

;;; Source position

(defmethod eclector.base:source-position ((client client) (stream buffer-stream))
  (cons (current-line-number stream) (current-item-number stream)))

(defmethod eclector.base:make-source-range ((client client) (start t) (end t))
  (cons start end))

;;; Result construction

(defun make-result-wad (class stream source children ; still used for WORD-WADs
                        &rest extra-initargs &key &allow-other-keys)
  (destructuring-bind ((start-line . start-column) . (end-line . end-column))
      source
    (let* ((line-number    (current-line-number stream))
           (max-line-width (compute-max-line-width
                            stream start-line line-number '())))
      (let ((result (apply #'make-wad class :start-line     start-line
                                            :height         (- end-line start-line)
                                            :start-column   start-column
                                            :end-column     end-column
                                            :relative-p     nil
                                            :max-line-width max-line-width
                                            :children       children

                                            :absolute-start-line-number start-line

                                            extra-initargs)))
        (when children
          (make-relative children (start-line result)))
        result))))

(defun make-word-wads (stream source
                       &key (start-column-offset 0)
                            (end-column-offset   0 end-column-offset-p))
  (destructuring-bind ((start-line . start-column) . (end-line . end-column*))
      source
    (let* ((cache             (cache stream))
           (word              (make-array 0 :element-type 'character
                                            :adjustable   t
                                            :fill-pointer 0))
           (word-start-column (+ start-column start-column-offset))
           (words             '()))
      (flet ((terminatingp (character)
               (let ((spacep       (whitespacep character))
                     (punctuationp (punctuationp character)))
                 (values (or spacep punctuationp) punctuationp)))
             (commit (line column checkp)
               (when (and (plusp (length word))
                          (notany #'digit-char-p word)
                          (notevery #'punctuationp word))
                 (let ((source      (cons (cons line word-start-column)
                                          (cons line column)))
                       (misspelledp (and checkp
                                         (null (spell:english-lookup word)))))
                   (when (> line end-line) (break))
                   (when (and (= line end-line) (> column end-column*)) (break))
                   (push (make-result-wad 'word-wad stream source '()
                                          :misspelled misspelledp)
                         words)))
               (setf (fill-pointer word) 0
                     word-start-column   column)))
        (loop for line     from start-line to (if (zerop end-column*)
                                                  (1- end-line)
                                                  end-line)
              for contents =    (line-contents cache line)
              do (loop with end-column = (if (= line end-line)
                                             (+ end-column*
                                                (if end-column-offset-p
                                                    end-column-offset
                                                    0))
                                             (length contents))
                       for column from word-start-column below end-column
                       for character = (aref contents column)
                       for (terminatingp punctuationp)
                          = (multiple-value-list (terminatingp character))
                       do (cond ((not terminatingp)
                                 (vector-push-extend character word))
                                (punctuationp
                                 (commit line column t)
                                 (vector-push-extend character word)
                                 (commit line (1+ column) nil))
                                (t
                                 (commit line column t)
                                 (incf word-start-column)))
                       finally (commit line column t))
                 (setf word-start-column 0))
        (nreverse words)))))

(defmethod eclector.parse-result:make-skipped-input-result
    ((client client) (stream t) (reason t) (source t))
  (flet ((make-it (class &rest extra-initargs)
           (apply #'make-result-wad class stream source '() extra-initargs)))
    (etypecase reason
      ((cons (eql :line-comment))
       ;; Eclector returns the beginning of the following line as the
       ;; end of the comment.  But we want it to be the end of the
       ;; same line.  But I don't know how to do it correctly (yet).
       (let* ((semicolon-count (cdr reason))
              (words           (make-word-wads
                                stream source
                                :start-column-offset semicolon-count)))
         (make-result-wad 'semicolon-comment-wad stream source words
                          :semicolon-count semicolon-count)))
      ((eql :block-comment)
       (let ((words (make-word-wads stream source :start-column-offset 2
                                                  :end-column-offset   -2)))
         (make-result-wad 'block-comment-wad stream source words)))
      ((eql :reader-macro)
       (make-it 'reader-macro-wad))
      ((eql *read-suppress*)
       (make-it 'read-suppress-wad))
      ((cons (eql :sharpsign-plus))
       (make-it 'sharpsign-plus-wad  :expression (cdr reason)))
      ((cons (eql :sharpsign-minus))
       (make-it 'sharpsign-minus-wad :expression (cdr reason))))))

(defmethod eclector.parse-result:make-expression-result
    ((client client) (result symbol-token) (children t) (source t))
  (if (and (null children) (not (string= (package-name result) "COMMON-LISP")))
      (let ((words (make-word-wads (stream* client) source)))
        (make-result-wad 'atom-wad (stream* client) source words
                         :raw result))
      (call-next-method)))

(defmethod eclector.parse-result:make-expression-result
    ((client client) (result string) (children t) (source t))
  (if (null children)
      (let ((words (make-word-wads (stream* client) source)))
        (make-result-wad 'atom-wad (stream* client) source words
                         :raw result))
      (call-next-method)))

(defun adjust-result-class (cst new-class stream source)
  (destructuring-bind ((start-line . start-column) . (end-line . end-column))
      source
    (let* ((line-number    (current-line-number stream))
           (max-line-width (compute-max-line-width
                            stream start-line line-number '())))
      (change-class cst new-class
                    :cache          *cache*
                    :start-line     start-line
                    :height         (- end-line start-line)
                    :start-column   start-column
                    :end-column     end-column
                    :relative-p     nil
                    :max-line-width max-line-width
                    :absolute-start-line-number start-line))))

(defun adjust-result (cst new-class stream source extra-children wrap-around)
  (let ((result (if (null source)
                    cst
                    (adjust-result-class cst new-class stream source))))
    (when extra-children
      (mapc (lambda (child)
              (check-absolute-wad-with-relative-descendants child))
            extra-children)

      (labels ((check-wad (result)
                 (assert (not (relative-p result)))
                 (assert (eq (absolute-start-line-number result) (start-line result)))
                 (let ((children (children result)))
                   (cond ((null children))
                         ((some #'relative-p children)
                          (check-absolute-line-numbers result)) ; asserts that children are relative
                         (t
                          (mapc #'check-wad children))))))
        (check-wad result))

      (mapc (lambda (child)
              (check-absolute-wad-with-relative-descendants child))
            extra-children)

      (add-extra-children result extra-children wrap-around))

    ;; There may be "indirect" children of the form
    ;; <RESULT is a CONS-WAD> -> rest ... rest -> CONS-WITH-CHILDREN -> children -> <A-WAD-CHILD>
    ;; So refresh the family relations here.
    (set-family-relations-of-children result)

    (assert (not (relative-p result)))
    (map-children (lambda (child)
                    (check-absolute-wad-with-relative-descendants child))
                  result)
    (map-children (lambda (child)
                    (assert (not (eq child (first (suffix (cache child)))))))
                  result)
    (make-relative (children result) (start-line result))
    ;; TODO do everything in one pass: family relations and making children relative

    result))

(defmethod eclector.parse-result:make-expression-result
    ((client client) (result t) (children t) (source t))
  ;; In case we call `cst:reconstruct', we may "consume" a child wad
  ;; that is still on the residue or suffix without performing the
  ;; corresponding recursive `read-maybe-nothing' call. As a
  ;; workaround, consume any such wads here.
  (cached-wad (stream* client))

  (multiple-value-bind (cst-children extra-children)
      (loop for child in children
            if (typep child 'cst:cst)
              collect child into cst-children
            else
              collect child into extra-children
            finally (return (values cst-children extra-children)))
    (mapc (lambda (child)
            (when (typep child 'wad)
              (check-absolute-wad-with-relative-descendants child)))
          cst-children)
    (mapc (lambda (child)
            (check-absolute-wad-with-relative-descendants child))
          extra-children)
    (let* ((cst       (call-next-method client result cst-children source))
           (new-class (if (cst:consp cst)
                          'cons-wad
                          (progn
                            (check-type cst cst:atom-cst) ; TODO remove later
                            'atom-wad)))
           (direct-cst-children (loop :for child :in cst-children
                                      :unless (parent child)
                                        :collect child))
           (orphans   (loop :for child :in direct-cst-children
                            :unless (block nil
                                      (labels ((rec (cst)
                                                 (when (eq cst child)
                                                   (return t))
                                                 (map-children #'rec cst)))
                                        (rec cst))
                                      nil) ; TODO do not call children repeatedly
                            :collect child)))
      #+no (format *trace-output* "Result         ~A~@
                              CST children   ~:A~@
                              Direct CST ch. ~:A~@
                              Orphans        ~:A~@
                              Extra children ~:A~@
                              WAD children   ~:A~@
                              "
              cst cst-children direct-cst-children orphans extra-children '())
      #+no (when (cst:consp cst)
        (format *trace-output* "First          ~A~%Rest           ~:A~%"
                (cst:first cst) (cst:rest cst)))
      (mapc (lambda (child)
              (check-absolute-wad-with-relative-descendants child))
            extra-children)
      (mapc (lambda (child)
              (check-absolute-wad-with-relative-descendants child))
            orphans)

      (assert (not (typep cst 'wad)))
      (let ((wrap-around (and cst-children
                              (cst:consp cst)
                              (or (not (typep (cst:first cst) 'wad))
                                  (parent (cst:first cst)))
                              (or (not (typep (cst:rest cst) 'wad))
                                  (parent (cst:rest cst))))))
        (when wrap-around
          (assert direct-cst-children))
        (let ((result (adjust-result cst new-class (stream* client) source
                              (sort (append extra-children orphans) #'< ; TODO HACK
                                    :key (lambda (child)
                                           (position child children)))
                              wrap-around)))
          result)))))

(defmethod eclector.parse-result:make-expression-result
    ((client   client)
     (result   eclector.parse-result:definition)
     (children t)
     (source   t))
  (let ((cst (call-next-method)))
    (adjust-result cst 'labeled-object-definition-wad (stream* client) source '() nil)))

(defmethod eclector.parse-result:make-expression-result
    ((client   client)
     (result   eclector.parse-result:reference)
     (children t)
     (source   t))
  (let ((cst (call-next-method))) ; TODO handle children
    (adjust-result cst 'labeled-object-reference-wad (stream* client) source '() nil)))

;;; S-expression generation

(flet ((make-form (symbol-name package-name &rest rest)
         (let ((head (make-instance 'existing-symbol-token
                                    :name         symbol-name
                                    :package-name package-name)))
           (list* head rest))))

  (defmethod eclector.reader:wrap-in-quote ((client client) (material t))
    (make-form "QUOTE" "COMMON-LISP" material))

  (defmethod eclector.reader:wrap-in-quasiquote ((client client) (form t))
    (make-form "QUASIQUOTE" "KEYWORD" form))

  (defmethod eclector.reader:wrap-in-unquote ((client client) (form t))
    (make-form "UNQUOTE" "KEYWORD" form))

  (defmethod eclector.reader:wrap-in-unquote-splicing ((client client) (form t))
    (make-form "UNQUOTE-SPLICING" "KEYWORD" form))

  (defmethod eclector.reader:wrap-in-function ((client client) (name t))
    (make-form "FUNCTION" "COMMON-LISP" name)))
