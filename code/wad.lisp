(cl:in-package #:incrementalist)

;;; wad
;;;  ├─────────────────────────────┐────────────────────────────────┐
;;;  │                        cst  │                                │
;;;  │                         │   │                                │
;;;  │          ┌──────────────┴──────────────┐                     │
;;;  │      cons-cst               │      atom-cst                  │
;;;  │          │                  │          │                     │
;;;  │    ┌─────┴──────┐           │    ┌─────┴──────┐              │
;;;  │    │            │           │    │            │              │
;;; cons-wad  cons-with-children  atom-wad  atom-with-children  skipped-wad
;;;     │              │              │              │
;;;     └───────┬──────┘              └───────┬──────┘
;;;             │                             │
;;;  cons-extra-children-mixin          children-mixin

;;; `children-mixin' and `cons-extra-children-mixin'

(defmethod children ((node t))
  ;; TODO maybe cache this?
  (let ((result '()))
    (map-children (lambda (child) (push child result)) node)
    (nreverse result)))

(defclass children-mixin ()
  ((%children :initarg  :children
              :type     list
              :accessor children
              :reader   extra-children
              :initform '())))

(defmethod map-children ((function t) (object children-mixin))
  (mapc function (extra-children object)))

;;; Instances of `cons-with-children' and `cons-wad' have two types of
;;; children: the `cst:first' and `cst:rest' relations point to one
;;; kind of children and the `%before-first', `%middle' and
;;; `%after-rest' slots contain the second kind of children.
;;;
;;; * The children in the `%before-first' slot must precede the
;;;   CST-based children in the `cst:first' relation.
;;;
;;; * The children in the `%middle' slot must follow the CST-based
;;;   children in the `cst:first' relation and follow the CST-based
;;;   children in the `cst:rest' relation.
;;;
;;; * The children in the `%after-rest' slot must follow the CST-based
;;;   children in the `cst:rest' relation.
;;
;;; So input of the form
;;;
;;;   (#|a|# 1 #|b|# . 2 #|c|#)
;;;
;;; would be represented as a `cons-wad' with the comment-wad for
;;; #|a|# in the `%before-first' slot, an `atom-wad' corresponding to
;;; the 1 atom as the `cst:first' relation, the comment-wad for #|b|#
;;; in the `%middle' slot, an `atom-wad' corresponding to the to the 2
;;; atom in the `cst:rest' relation and the comment-wad for the #|c|#
;;; in the `%after-rest' slot.
;;;
;;; The example
;;;
;;;   (1 #|a|#)
;;;
;;; would be represented as an `atom-wad' corresponding to the 1 atom
;;; as the `cst:first' relation, an `atom-cst' (not `atom-wad')
;;; without source information corresponding to the implicit `nil' in
;;; the `cst:rest' relation and a comment-wad for #|a|# in the
;;; `%after-rest' slot. Note that the `atom-cst' for the implicit
;;; `nil' would not be returned by the `children' function since it is
;;; not a wad and has no source information.
(defclass cons-extra-children-mixin ()
  ;; TODO maybe do this as a single slot which contains a plist or
  ;; similar? Since extra children and in particular all three kinds
  ;; of extra children should relatively rare. But consider that
  ;; (:before-first (child))
  ;; is two extra conses.
  ((%before-first :initarg  :before-first
                  :type     list ; TODO could store singletons without the list
                  :reader   before-first
                  :initform '())
   (%middle       :initarg  :middle
                  :type     list
                  :reader   middle
                  :initform '())
   (%after-rest   :initarg  :after-rest
                  :type     list
                  :reader   after-rest
                  :initform '())))

(defclass family-relations-mixin ()
  (;; This slot contains the parent wad of this wad, or NIL if this
   ;; wad is a top-level wad.
   (%parent        :initarg  :parent
                   :accessor parent
                   :initform nil)
   ;; This slot contains the left sibling wad of this wad, or NIL if
   ;; this wad is the first child of its parent.  If this wad is a
   ;; top-level wad, then this slot contains the preceding top-level
   ;; wad, or NIL if this is the first top-level wad in the buffer.
   (%left-sibling  :initarg  :left-sibling
                   :accessor left-sibling
                   :initform nil)
   ;; This slot contains the right sibling wad of this wad, or NIL if
   ;; this wad is the last child of its parent.  If this wad is a
   ;; top-level wad, then this slot contains the following top-level
   ;; wad, or NIL if this is the last top-level wad in the buffer.
   (%right-sibling :initarg  :right-sibling
                   :accessor right-sibling
                   :initform nil)))

;;; A WAD is the result of parsing an expression or some material that
;;; is normally skipped, such as a comment or an inactive reader
;;; conditional.

(defclass basic-wad ()
  (;; This slot contains the cache that this wad is part of.
   (%cache                      :initarg  :cache
                                :reader   cache)
   ;; This slot contains TRUE if and only if the START-LINE slot is
   ;; relative to some other line.
   (%relative-p                 :initarg :relative-p
                                :accessor relative-p)
   ;; This slot contains the absolute start line of the wad.  Its
   ;; contents is valid only when the wad is on the prefix, and when
   ;; the wad is the top-level wad which is the first on the suffix.
   ;; With a wad in a different place, this slot may contain some
   ;; obsolete value.  We define a :BEFORE method on the slot reader TODO now wrong
   ;; so that the wad of the argument will always be on the prefix
   ;; when the absolute line number is asked for.
   (%absolute-start-line-number :initarg  :absolute-start-line-number
                                :type     (or (eql :invalid) (integer 0))
                                :accessor absolute-start-line-number) ; TODO rename to absolute-start-line
   ;; This slot contains information about the start line of the wad.
   ;; Simple applications might always store the absolute line number
   ;; of the first line of the wad in this slot.  Other applications
   ;; might store a line number relative to some other wad.
   (%start-line                 :initarg  :start-line
                                ; :type     (integer 0)
                                :accessor start-line)
   ;; This slot contains the difference between the start line and the
   ;; end line.  A value of 0 indicates that the wad starts and ends
   ;; in the same line.
   (%height                     :initarg :height
                                :type    (integer 0)
                                :reader  height)
   ;; This slot contains the absolute column of the first character in
   ;; this wad.  A value of 0 indicates that this wad starts in the
   ;; leftmost position in the source code.
   (%start-column               :initarg  :start-column
                                :type     (integer 0)
                                :accessor start-column)
   ;; This slot contains the absolute column of the last character of
   ;; the wad.  The value of this slot can never be 0.  If the last
   ;; character of the wad is the leftmost character in a line, then
   ;; this slot contains the value 1.
   (%end-column                 :initarg  :end-column
                                :type     (integer 0) ; TODO despite what the comment says, 0 is used in some cases
                                :accessor end-column)))

(declaim (inline absolute-p))
(defun absolute-p (wad)
  (not (relative-p wad)))

(defclass wad (family-relations-mixin
               basic-wad)
  (;; This slot contains the column number of the leftmost known
   ;; non-whitespace character of the wad.  It may not be entirely
   ;; correct if a reader macro reads character by character and such
   ;; characters happen to be outside the part that is returned by a
   ;; call to READ.  But we use this information only for
   ;; highlighting, and selection.  Not for drawing.
   (%min-column-number :initarg :min-column-number :reader min-column-number)
   ;; This slot contains the column number of the rightmost known
   ;; non-whitespace character of the wad.  It may not be entirely
   ;; correct for the same reason as the preceding slot.
   (%max-column-number :initarg :max-column-number :reader max-column-number)
   ;; This slot contains the maximum line width of any line that is
   ;; part of the wad.
   (%max-line-width :initarg :max-line-width :reader max-line-width)
   ;; This slot stores different kind of errors which are represented
   ;; as `error-wad's. Character syntax errors as reported by Eclector
   ;; are stored in the "closest surrounding" wad. S-expression syntax
   ;; errors are stored in the containing top-level wad.
   ;;
   ;; Invariants for errors:
   ;; The error wad are relative iff containing wad is relative. The
   ;; start-line of relative error wads is relative to the parent of
   ;; the containing wad, not relative to preceding wads. So when
   ;; turning the containing wad from relative to absolute, each error
   ;; wad must be processed with the same offset as the containing
   ;; wad.
   (%errors :initarg  :errors
            :type     list
            :accessor errors
            :initform '())
   ;; This slot contains the absolute column that the first character
   ;; of this wad should be positioned in, as computed by the rules of
   ;; indentation.  If this wad is not the first one on the line, then
   ;; this slot contains NIL.
   (%indentation  :initarg  :indentation
                  :accessor indentation
                  :initform nil)))

(defun set-family-relations-of-children (wad)
  (let* ((children (children wad))
         (length   (length children)))
    (loop for child in children
          do (assert (or (null (parent child))
                         (eq (parent child) wad)))
          do (setf (parent child) wad))
    (when (plusp length)
      (setf (left-sibling  (first children))        nil
            (right-sibling (first (last children))) nil)
      (loop for (left right) on children
            repeat (1- length)
            do (setf (right-sibling left) right
                     (left-sibling right) left)))))

(defmethod (setf children) :after (children (wad wad))
  (declare (ignorable children))
  (set-family-relations-of-children wad))

(defmethod shared-initialize :after ((wad wad) (slot-names t) &key) ; TODO instance
  (set-family-relations-of-children wad))

(defmethod initialize-instance :after ((object wad) &key) ; TODO instance
  (let* ((start-column (start-column object))
         (end-column   (end-column object))
         (min-column   (reduce #'min (children object)
                               :initial-value (min start-column end-column)
                               :key #'min-column-number))
         (max-column   (reduce #'max (children object)
                               :initial-value (max start-column end-column)
                               :key #'max-column-number)))
    (reinitialize-instance object :min-column-number min-column
                                  :max-column-number max-column)))

(defun print-wad-position (wad stream)
  (format stream "~:[abs~;rel~]:~d[~d],~d -> ~d,~d"
          (relative-p wad)
          (start-line wad)
          (when (slot-boundp wad '%absolute-start-line-number)
            (slot-value wad '%absolute-start-line-number))
          (start-column wad)
          (if (relative-p wad)
              (height wad)
              (end-line wad))
          (end-column wad)))

(defmethod print-object ((object wad) stream)
  (print-unreadable-object (object stream :type t)
    (print-wad-position object stream)))

;;; During parsing, this variable holds the cache to which all created
;;; wads belong.
(defvar *cache*)

;;; Define an indirection for MAKE-INSTANCE for creating wads.  The
;;; main purpose is so that the creation of wads can be traced.
(defun make-wad (class &rest initargs)
  (apply #'make-instance class :cache *cache* initargs))

;;; Return true if and only if the position indicated by
;;; RELATIVE-LINE-NUMBER and COLUMN-NUMBER is entirely before WAD.  If
;;; WAD is an absolute wad, then RELATIVE-LINE-NUMBER must be the
;;; absolute line number of the position.  If WAD is a relative wad,
;;; then RELATIVE-LINE-NUMBER must be the difference between the
;;; absolute line number of the position, and the start line of the
;;; wad to which WAD is relative.  The position is before WAD if
;;; either RELATIVE-LINE-NUMBER is strictly less than the start line
;;; of WAD, or if RELATIVE-LINE-NUMBER is equal to the start line of
;;; WAD, and COLUMN-NUMBER is less than or equal to the start column
;;; of WAD.
(defun position-is-before-wad-p (wad relative-line-number column-number)
  (%position<= relative-line-number column-number
               (start-line wad) (start-column wad)))

;;; Return true if and only if the position indicated by
;;; RELATIVE-LINE-NUMBER and COLUMN-NUMBER is entirely after WAD.  If
;;; WAD is an absolute wad, then RELATIVE-LINE-NUMBER must be the
;;; absolute line number of the position.  If WAD is a relative wad,
;;; then RELATIVE-LINE-NUMBER must be the difference between the
;;; absolute line number of the position, and the start line of the
;;; wad to which WAD is relative.  The position is after WAD if either
;;; RELATIVE-LINE-NUMBER is strictly greater than the sum of the start
;;; line of WAD and the height of WAD, or if RELATIVE-LINE-NUMBER is
;;; equal to the sum of the start line of WAD and the height of WAD,
;;; and COLUMN-NUMBER is greater than or equal to the end column of
;;; WAD.
(defun position-is-after-wad-p (wad relative-line-number column-number)
  (%position>= relative-line-number column-number
               (+ (start-line wad) (height wad)) (end-column wad)))

;;; Return true if and only if the position indicated by
;;; RELATIVE-LINE-NUMBER and COLUMN-NUMBER is inside WAD.  If WAD is
;;; an absolute wad, then RELATIVE-LINE-NUMBER must be the absolute
;;; line number of the position.  If WAD is a relative wad, then
;;; RELATIVE-LINE-NUMBER must be the difference between the absolute
;;; line number of the position, and the start line of the wad to
;;; which WAD is relative.  The position is inside WAD if it is
;;; neither before WAD nor after WAD.
(defun position-is-inside-wad-p (wad relative-line-number column-number)
  (not (or (position-is-before-wad-p wad relative-line-number column-number)
           (position-is-after-wad-p wad relative-line-number column-number))))

(defun wad-starts-before-wad-p (wad1 wad2)
  (%position< (start-line wad1) (start-column wad1)
              (start-line wad2) (start-column wad2)))

(defun wad-ends-after-wad-p (wad1 wad2)
  (not (%position< (end-line wad2) (end-column wad2)
                   (end-line wad1) (end-column wad1))))

(defun wad-contains-wad-p (wad1 wad2)
  (and (wad-starts-before-wad-p wad1 wad2)
       (wad-ends-after-wad-p wad1 wad2)))

;;; `error-wad'
;;;
;;; Not a real wad but has the same

(defclass error-wad (family-relations-mixin basic-wad) ; TODO may nod need some of the inherited slots
  ((%condition :initarg :condition
               :reader  condition*)))

(defmethod print-object ((object error-wad) stream)
  (print-unreadable-object (object stream :type t)
    (print-wad-position object stream)
    (format stream " condition: ~a"
            (class-name (class-of (condition* object))))))

(defmethod children ((wad error-wad))
  '())

(defmethod map-children ((function t) (object error-wad)))

;;; CST wads
;;;
;;; CST wads contain a "raw" expression. Note that wads based on
;;; `cst:atom-cst' and `cst:cons-cst' are not the only CST wads. One
;;; other example are labeled object definitions and references which
;;; are based `eclector.concrete-syntax-tree:wrapper-cst'.

(defclass cst-wad (wad cst:cst)
  ())

;;; Atom

(defmethod map-children ((function t) (wad cst:atom-cst))
  '())

(defclass atom-with-children (children-mixin
                              cst:atom-cst)
  ())

(defclass atom-wad (children-mixin
                    cst-wad
                    cst:atom-cst) ; TODO we inherit the `%source' slot which we do not use
  ())

(defmethod print-object ((object atom-wad) stream)
  (print-unreadable-object (object stream :type t)
    (print-wad-position object stream)
    (format stream " raw: ~S" (cst:raw object))))

;;; Cons

(defclass cons-with-children (cons-extra-children-mixin
                              cst:cons-cst)
  ())

(defclass cons-wad (cons-extra-children-mixin
                    cst-wad
                    cst:cons-cst) ; TODO we inherit the `%source' slot which we do not use
  ())

;;; TODO Add a class `cons-wad-with-extra-children' so that we can
;;;      have a `cons-wad' (without extra children) and a simple
;;;      `map-children' method
;;; TODO `error-location' does something very similar
(defun cst-node-location (child)
  (labels ((rec (node)
             (typecase node
               (wad
                (let* ((start-line (absolute-start-line-number node))
                       (end-line   (+ start-line (height node))))
                  (values start-line (start-column node)
                          end-line   (end-column node))))
               (cst:cons-cst            ; TODO use `cst:consp'?
                (multiple-value-bind (first-start-line first-start-column
                                      first-end-line   first-end-column)
                    (rec (cst:first node))
                  (multiple-value-bind (rest-start-line rest-start-column
                                        rest-end-line   rest-end-column)
                      (rec (cst:rest node))
                    (cond ((and first-start-line rest-start-line)
                           (values first-start-line first-start-column
                                   rest-end-line    rest-end-column))
                          (first-start-line
                           (values first-start-line first-start-column
                                   first-end-line    first-end-column))
                          (rest-start-line
                           (values rest-start-line rest-start-column
                                   rest-end-line   rest-end-column)))))))))
    (rec child)))

;;; EXTRA-CHILDREN are non-CST children like comments and other
;;; skipped material.
(defun distribute-extra-cons-children (result extra-children)
  (check-type result cst:cons-cst)
  (assert (not (null extra-children)))
  (multiple-value-bind (first-start-line first-start-column
                        first-end-line   first-end-column)
      (cst-node-location (cst:first result))
    (multiple-value-bind (rest-start-line rest-start-column
                          rest-end-line   rest-end-column)
        (cst-node-location (cst:rest result))
      (let ((remainder    extra-children)
            (before-first '())
            (in-first     '())
            (before-rest  '())
            (in-rest      '()))
        (when first-start-line
          ;; Before first child
          (loop for extra-child = (first remainder)
                while (and extra-child
                           (let ((end-line (+ (absolute-start-line-number extra-child) (height extra-child))))
                             (or (< end-line first-start-line)
                                 (and (= end-line first-start-line)
                                      (<= (end-column extra-child) first-start-column)))))
                do (push extra-child before-first)
                   (pop remainder))
          ;; Overlapping first child
          (loop for extra-child = (first remainder)
                while (and extra-child
                           (or (< (absolute-start-line-number extra-child) first-start-line)
                               (and (= (absolute-start-line-number extra-child) first-start-line)
                                    (< (start-column extra-child) first-start-column))))
                do (warn "~@<Dropping ~S~@:_~2@Tfrom ~S~@:_~2@Tat ~S~@:_~2@TFIRST of ~A~@:>"
                         extra-child
                         (cst:first result)
                         (cst:source (cst:first result))
                         result)
                   (pop remainder))
          ;; Contained in first child
          (loop for extra-child = (first remainder)
                while (and extra-child
                           (let ((end-line (+ (absolute-start-line-number extra-child) (height extra-child))))
                             (or (< end-line first-end-line)
                                 (and (= end-line first-end-line)
                                      (<= (end-column extra-child) first-end-column)))))
                do (push extra-child in-first)
                   (pop remainder)))
        (when rest-start-line
          ;; Before rest child
          (loop for extra-child = (first remainder)
                while (and extra-child
                           (let ((end-line (+ (absolute-start-line-number extra-child) (height extra-child))))
                             (or (< end-line rest-start-line)
                                 (and (= end-line rest-start-line)
                                      (<= (end-column extra-child) rest-start-column)))))
                do (push extra-child before-rest)
                   (pop remainder))
          ;; Overlapping rest
          (loop for extra-child = (first remainder)
                while (and extra-child
                           (or (< (absolute-start-line-number extra-child) rest-start-line)
                               (and (= (absolute-start-line-number extra-child) rest-start-line)
                                    (< (start-column extra-child) rest-start-column))))
                do (warn "~@<Dropping ~S~@:_~2@Tfrom ~S~@:_~2@Tat ~S~@:_~2@TREST of ~A~@:>"
                         extra-child
                         (cst:rest result)
                         (cons (cons rest-start-line rest-start-column)
                               (cons rest-end-line rest-end-column))
                         result)
                   (pop remainder))
          ;; Contained in rest child
          (loop for extra-child = (first remainder)
                while (and extra-child
                           (let ((end-line (+ (absolute-start-line-number extra-child) (height extra-child))))
                             (or (< end-line rest-end-line)
                                 (and (= end-line rest-end-line)
                                      (<= (end-column extra-child) rest-end-column)))))
                do (push extra-child in-rest)
                   (pop remainder)))
        (values (nreverse before-first) (nreverse in-first) (nreverse before-rest) (nreverse in-rest) remainder)))))

;;; TODO just return initargs here. `adjust-result' should pick up the initargs and do all change-class/reinitialize actions at once
(defun add-extra-children (result extra-children wrap-around)
  (typecase result
    (cst:atom-cst
     (assert (null wrap-around))
     (if (typep result 'children-mixin)
         (reinitialize-instance result :children extra-children)
         (change-class result 'atom-with-children :children extra-children)))
    (cst:cons-cst
     (multiple-value-bind (before-first in-first before-rest in-rest remaining)
         (if wrap-around
             (values '() '() extra-children '() '())
             (distribute-extra-cons-children result extra-children))
       (when (or before-first before-rest remaining)
         (if (typep result 'cons-extra-children-mixin)
             (reinitialize-instance result :before-first before-first
                                           :middle       before-rest
                                           :after-rest   remaining)
             (change-class result 'cons-with-children :before-first before-first
                                                      :middle       before-rest
                                                      :after-rest   remaining)))
       (when in-first
         (break "cannot add children ~S to~@
                 ~A~@
                 ~2@Tfirst  ~A~@
                 ~2@Tsecond ~A"
                in-first result (cst:first result) (cst:rest result)))
       (when in-rest
         ;; TODO first cst:first contains a cons-cst, try to add children there?
         ;; for something like ((a #|x|# b) . (c d)) maybe?
         (add-extra-children (cst:rest result) in-rest nil))
       result))))

(defmethod map-children ((function t) (wad cst:cons-cst))
  (let ((function (alexandria:ensure-function function)))
    (labels ((visit-wad-child (child expected-parent)
               (when (let ((parent (parent child)))
                       (or (null parent) (eq parent expected-parent)))
                 (funcall function child)))
             (visit-maybe-wad-child (child parent)
               ;; It is possible that CHILD is a CST child (that is
               ;; `cst:first' or `cst:rest') or PARENT but not a WAD
               ;; child. This can happen due to `cst:reconstruct' in
               ;; cases like the invalid expression ,(a b) since the
               ;; whole expression is represented as a `cons-wad' with
               ;; one `atom-wad' child for symbol a and a `cons-cst'
               ;; child for (b). However, on the WAD level, the top
               ;; `cons-wad' contains an intermediate `cons-wad' which
               ;; represents (a b).
               (when (typep child 'wad) ; TODO make `wad-p' function?
                 (visit-wad-child child parent)))
             (visit-cons (node wad-parent dotted? extra-children?)
               (when extra-children?
                 (mapc (lambda (wad-child) ; TODO ensure no consing
                         (visit-wad-child wad-child wad-parent))
                       (before-first node)))
               (visit-maybe-wad-child (cst:first node) wad-parent)
               (when extra-children?
                 (mapc (lambda (wad-child)
                         (visit-wad-child wad-child wad-parent))
                       (middle node)))
               (when dotted?
                 (visit-maybe-wad-child (cst:rest node) wad-parent)))
             (visit-atom (node)
               (typecase node
                 (wad ; dotted list TODO explain
                  (funcall function node))
                 (atom-with-children
                  (map-children function node)))))
      (declare (inline visit-wad-child visit-maybe-wad-child))
      (loop with dangling = '()
            for previous = nil then node
            for node = wad then (cst:rest node)
            if (cst:consp node)
              do (let* ((rest       (cst:rest node))
                        (dotted?    (not (or (cst:consp rest) (cst:null rest))))
                        (wad-child? (and (not (eq node wad)) (typep node 'wad))))
                   (if wad-child?
                       (funcall function node) ; TODO extra-children? not needed here
                       (let ((extra-children? (typep node 'cons-extra-children-mixin)))
                         (visit-cons node wad dotted? extra-children?)
                         (when extra-children?
                           (alexandria:when-let ((after-rest (after-rest node)))
                             (setf dangling (append dangling after-rest))))))
                    ; TODO quadratic
                   (when (or dotted? wad-child?)
                     (loop-finish)))
            else ; atom-cst
              do (visit-atom node)
                 (loop-finish)
            finally (mapc (lambda (wad-child)
                            (visit-wad-child wad-child wad))
                          dangling)))))

;;; Definition and reference wads

(defclass labeled-object-wad (children-mixin cst-wad)
  ())

(defclass labeled-object-definition-wad (labeled-object-wad
                                         eclector.concrete-syntax-tree:definition-cst)
  ())

(defmethod children ((node labeled-object-definition-wad))
  ;; TODO think about ordering
  (list* (eclector.concrete-syntax-tree:target node)
         (call-next-method)))

(defmethod map-children ((function t) (node labeled-object-definition-wad))
  ;; TODO think about ordering
  (funcall function (eclector.concrete-syntax-tree:target node))
  (call-next-method))

(defclass labeled-object-reference-wad (labeled-object-wad
                                        eclector.concrete-syntax-tree:reference-cst)
  ())

;;; Non-expression wads

(defclass skipped-wad (children-mixin wad)
  ())

;;; This class is the base class of all comment wads.
(defclass comment-wad (skipped-wad)
  ())

;;; This class is used for a block comment introduced by #|.
(defclass block-comment-wad (comment-wad)
  ())

;;; This class is used for a comment introduced by one or more
;;; semicolons.
(defclass semicolon-comment-wad (comment-wad)
  (;; This slot contains the number of consecutive initial semicolons
   ;; of the comment.
   (%semicolon-count :initarg :semicolon-count :reader semicolon-count)))

(defclass word-wad (skipped-wad)
  ((%misspelled :initarg :misspelled
                :reader  misspelled)))

(defclass ignored-wad (skipped-wad)
  ())

(defclass sharpsign-wad (ignored-wad)
  ((%expression :initarg :expression :reader expression)))

(defclass sharpsign-plus-wad (sharpsign-wad)
  ())

(defclass sharpsign-minus-wad (sharpsign-wad)
  ())

(defclass read-suppress-wad (ignored-wad)
  ())

(defclass reader-macro-wad (ignored-wad)
  ())

(defgeneric relative-to-absolute (wad offset)
  (:method ((wad wad) offset)
    (flet ((adjust (wad)
             (assert (relative-p wad))
             (let ((new-start-line (+ (start-line wad) offset)))
               (setf (start-line                 wad) new-start-line
                     (absolute-start-line-number wad) new-start-line
                     (relative-p                 wad) nil))))
      (adjust wad)
      (setf (parent wad) nil)
      (mapc #'adjust (errors wad)))))

(defgeneric absolute-to-relative (wad offset)
  (:method ((wad wad) offset)
    (flet ((adjust (wad)
             (assert (not (relative-p wad)))
             (decf (start-line wad) offset)
             (setf (relative-p wad) t)))
      (adjust wad)
      (mapc #'adjust (errors wad)))))

;;; RELATIVE-WADS is a list of wads where the start line of the first
;;; element is relative to OFFSET, and the start line of each of the
;;; other elements is relative to the start line of the preceding
;;; element.  Modify the wads in the list so that they are absolute.
;;; Return the original list, now containing the modified wads.
(defun make-absolute (relative-wads offset)
  (loop with base = offset
        for wad in relative-wads
        do (relative-to-absolute wad base)
           (setf base (start-line wad)))
  relative-wads)

;;; ABSOLUTE-WADS is a list of absolute wads.  Modify the wads in the
;;; list so that the start line of the first element is absolute to
;;; OFFSET, and the start line of each of the other elements is
;;; relative to the start line of the preceding element.  Return the
;;; original list, now containing the modified wads.
(defun make-relative (absolute-wads offset)
  (loop for base = offset then start-line
        for wad in absolute-wads
        for start-line = (start-line wad)
        do (absolute-to-relative wad base))
  absolute-wads)

(defgeneric end-line (wad)
  (:method ((wad basic-wad))
    (assert (not (relative-p wad)))
    (+ (start-line wad) (height wad))))

(defun compute-absolute-line-numbers (top-level-wad)
  ;; Make sure the wad itself is absolute, so that we need to compute
  ;; the absolute line numbers only of its children.
  (assert (not (relative-p top-level-wad)))
  (setf (absolute-start-line-number top-level-wad)
        (start-line top-level-wad))
  (labels ((process-children (relative-wads offset)
             (loop with base = offset
                   for wad in relative-wads
                   for absolute-start-line-number = (+ base (start-line wad))
                   do (setf (absolute-start-line-number wad)
                            absolute-start-line-number)
                      (process-wad wad base absolute-start-line-number)
                      (setf base absolute-start-line-number)))
           (process-wad (wad parent-start-line-number wad-start-line-number)
             (assert (every #'relative-p (children wad)))
             (process-children (children wad) wad-start-line-number)
             (loop for error-wad in (errors wad)
                   for absolute-start-line-number = (if (relative-p error-wad) ; TODO can we make errors always relative?
                                                        (+ parent-start-line-number (start-line error-wad))
                                                        (start-line error-wad))
                   do (setf (absolute-start-line-number error-wad)
                            absolute-start-line-number))))
    (process-wad top-level-wad nil (start-line top-level-wad)))
  top-level-wad)
