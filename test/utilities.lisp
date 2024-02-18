(cl:in-package #:incrementalist.test)

;;; Constructing buffer, analyzer and cache

(defpackage incrementalist.test.test-package (:use))

(defun buffer-string (buffer)
  (with-output-to-string (stream)
    (loop :for line-number :below (cluffer:line-count buffer)
          :for line        =      (cluffer:find-line buffer line-number)
          :for content     =      (cluffer:items line)
          :unless (zerop line-number)
            :do (terpri stream)
          :do (map nil (a:rcurry #'write-char stream) content))))

(defun prepared-buffer (content)
  (let* ((line   (make-instance 'cluffer-standard-line:open-line))
         (buffer (make-instance 'cluffer-standard-buffer:buffer :initial-line line))
         (cursor (make-instance 'cluffer-standard-line:right-sticky-cursor)))
    (cluffer:attach-cursor cursor line 0)
    (loop :for c :across content
          :do (case c
                (#\Newline (cluffer:split-line cursor))
                (t         (cluffer:insert-item cursor c))))
    (values buffer cursor)))

(defun prepared-analyzer (buffer-content)
  (multiple-value-bind (buffer cursor) (prepared-buffer buffer-content)
    (let* ((cache    (make-instance 'inc:cache :cluffer-buffer buffer))
           (analyzer (make-instance 'inc:analyzer :buffer buffer
                                                  :lines  (inc:lines cache)
                                                  :cache  cache)))
      (values analyzer cache buffer cursor))))

(defun update-cache (analyzer cache)
  (let ((*package* (find-package '#:incrementalist.test.test-package)))
    (inc:update-cache analyzer))
  (append (reverse (inc::prefix cache)) (inc::suffix cache)))

(defun parse-result (buffer-content)
  (multiple-value-bind (analyzer cache) (prepared-analyzer buffer-content)
    (update-cache analyzer cache)))

;;; Reporting utilities

(defun format-node (stream root-and-node &optional colon? at?)
  (declare (ignore colon? at?))
  (destructuring-bind (root . node) root-and-node
    (utilities.print-tree:print-tree
     stream root
     (utilities.print-tree:make-node-printer
      (lambda (stream depth node*)
        (declare (ignore depth))
        (princ node* stream)
        (when (eq node* node)
          (write-string " ***" stream))
        nil)
      nil
      (lambda (wad)
        (unless (typep wad 'inc::error-wad)
          (append (inc::errors wad) (inc:children wad))))))))

;;; Predicates

(defun is-sequence (child-predicate expected-sequence actual-sequence result-info label
                    &key input)
  (let ((label-control (concatenate 'string "~1:*" label "~*")))
    (is (= (length expected-sequence) (length actual-sequence))
        "~@<For~@[ input~@:_~@:_~
         ~S~
         ~@:_~@:_ and~] result~@:_~@:_~
         ~/incrementalist.test::format-node/~
         ~@:_~@:_expected the wad to have ~D ~@? but it has ~D ~@?.~@:>"
        input result-info
        (length expected-sequence) label-control
        (length actual-sequence)   label-control))
  (mapc child-predicate expected-sequence actual-sequence))

(defun is-wad-data (expected-type expected-location wad result-info
                    &key input)
  (is (alexandria:type= (class-of wad) expected-type)
      "~@<For~@[ input~@:_~@:_~
       ~S~
       ~@:_~@:_ and~] result~@:_~@:_~
       ~/incrementalist.test::format-node/~
       ~@:_~@:_expected the wad to be of type ~A but it is of type ~A.~@:>"
      input result-info expected-type (class-name (class-of wad)))
  (let* ((start-line      (inc:absolute-start-line-number wad))
         (end-line        (+ start-line (inc:height wad)))
         (actual-location (list (list start-line (inc:start-column wad))
                                (list end-line   (inc:end-column wad)))))
    (is (equal expected-location actual-location)
        "~@<For~@[ input~@:_~@:_~
         ~S~
         ~@:_~@:_ and~] result~@:_~@:_~
         ~/incrementalist.test::format-node/~
         ~@:_~@:_expected location of the wad to be ~S but its location is ~
         ~S.~@:>"
        input result-info expected-location actual-location)))

(defun is-error (expected-error wad result-info &key input)
  (let ((result-info (cons (car result-info) wad)))
    (destructuring-bind (expected-location expected-condition-type) expected-error
      (is-wad-data 'inc::error-wad expected-location wad result-info
                   :input input)
      (let ((condition (inc::condition* wad)))
        (is (typep condition expected-condition-type)
            "~@<For~@[ input~@:_~@:_~
             ~S~
             ~@:_~@:_ and~] result~@:_~@:_~
             ~/incrementalist.test::format-node/~
             ~@:_~@:_expected the condition in the error wad to be of type ~
             ~A but it is of type ~A.~@:>"
            input result-info
            expected-condition-type
            (class-name (class-of condition)))))))

(defun is-result (expected root-result &key input)
  (labels
      ((rec (expected result)
         (destructuring-bind
             (expected-type expected-location
              &optional ((&key ((:errors expected-errors) '())
                               ((:raw expected-raw)
                                nil
                                expected-raw-supplied?))
                         '())
              &rest     expected-children)
             expected
           (let ((result-info (cons root-result result)))
             ;; Assertions for type and location
             (is-wad-data expected-type expected-location result result-info
                          :input input)
             ;; Assertions for errors
             (is-sequence (lambda (expected-error actual-error)
                            (is-error expected-error actual-error result-info))
                          expected-errors (inc::errors result)
                          result-info "error~:P" :input input)
             ;; Assertions for raw value
             (when (and expected-raw-supplied?
                        (typep result expected-type))
               (let ((raw (cst:raw result)))
                 (destructuring-bind (expected-type
                                      &key ((:value expected-value)
                                            nil
                                            expected-value-supplied?)
                                           ((:symbol expected-symbol)
                                            nil
                                            expected-symbol-supplied?))
                     expected-raw
                   (is (typep raw expected-type))
                   (when expected-value-supplied?
                     (is (eql expected-value (inc::value raw))))
                   (when expected-symbol-supplied?
                     (destructuring-bind
                         (expected-package-name expected-name)
                         expected-symbol
                       (let ((actual-package-name (inc::package-name raw))
                             (actual-name         (inc::name raw)))
                         (is (string= expected-package-name
                                      actual-package-name)
                             "~@<For ~@[input~@:_~@:_~
                              ~S~
                              ~@:_~@:_, and~] result~@:_~@:_~
                              ~/incrementalist.test::format-node/~
                              ~@:_~@:_expected symbol token of the ~
                              node to have package name ~S but the ~
                              package name is ~S.~@:>"
                             input result-info expected-package-name actual-package-name)
                         (is (string= expected-name actual-name)
                             "~@<For ~@[input~@:_~@:_~
                              ~S~
                              ~@:_~@:_, and~] result~@:_~@:_~
                              ~/incrementalist.test::format-node/~
                              ~@:_~@:_expected symbol token of the ~
                              node to have package name ~S but the ~
                              package name is ~S.~@:>"
                             input result-info expected-name actual-name)))))))
             ;; Recursively check children
             (is-sequence #'rec expected-children (inc:children result)
                          result-info "~:*child~[ren~;~:;ren~]" :input input)))))
    (rec expected root-result)))

(defun are-results (expected-results actual-results &key input)
  (is (= (length expected-results) (length actual-results))
      "~@<~@[For input~@:_~@:_~
       ~S~
       ~@:_~@:_,~] expected ~D result~:P there are ~D result~:P.~@:>"
      input (length expected-results) (length actual-results))
  (mapc (lambda (expected-result actual-result)
          (is-result expected-result actual-result :input input))
        expected-results actual-results))

;;; Utilities for analysis test cases

(defun expected-consing-dot (location)
  `(inc::atom-wad ,location (:raw (symbol)))) ; TODO make this a token?

(defun expected-symbol-wad (location symbol-name
                            &key (package-name "INCREMENTALIST.TEST.TEST-PACKAGE")
                                 (token-class  'inc:non-existing-symbol-token)
                                 (words        `((inc::word-wad ,location))))
  `(inc::atom-wad ,location
    (:raw (,token-class :symbol (,package-name ,symbol-name)))
    ,@words))

(defun analysis-test-case (input expected-result)
  (are-results expected-result (parse-result input) :input input))

(defmacro analysis-cases (() &body cases)
  `(progn
     ,@(mapcar (lambda (case)
                 (destructuring-bind (input expected-result) case
                   `(analysis-test-case ,input ,expected-result)))
               cases)))
