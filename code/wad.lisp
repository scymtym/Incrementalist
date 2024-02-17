(cl:in-package #:incrementalist)

;;; wad
;;;  ├─────────────────────────────┐
;;;  │                        cst  │
;;;  │                         │   │
;;;  │          ┌──────────────┴──────────────┐
;;;  │      cons-cst               │      atom-cst
;;;  │          │                  │          │
;;;  │    ┌─────┴──────┐           │    ┌─────┴──────┐
;;;  │    │            │           │    │            │
;;; cons-wad  cons-with-children  atom-wad  atom-with-children
;;;     │              │              │              │
;;;     └───────┬──────┘              └───────┬──────┘
;;;             │                             │
;;;  cons-extra-children-mixin          children-mixin

;;; `children-mixin' and `cons-extra-children-mixin'

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
  (mapcar function (extra-children object)))

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
   ;; obsolete value.  We define a :BEFORE method on the slot reader
   ;; so that the wad of the argument will always be on the prefix
   ;; when the absolute line number is asked for.
   (%absolute-start-line-number :initarg  :absolute-start-line-number
                                :type     (integer 0)
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

(defmethod shared-initialize :after ((wad wad) (slot-names t) &key)
  (set-family-relations-of-children wad))

(defmethod initialize-instance :after ((object wad) &key)
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

(defmethod map-children ((function t) (wad cst:atom-cst))
  '())

;;; Cons

(defclass cons-with-children (cons-extra-children-mixin
                              cst:cons-cst)
  ())

(defclass cons-wad (cons-extra-children-mixin
                    cst-wad
                    cst:cons-cst) ; TODO we inherit the `%source' slot which we do not use
  ())

;;; EXTRA-CHILDREN are non-CST children like comments and other
;;; skipped material.
(defun distribute-extra-cons-children (result extra-children)
  (check-type result cst:cons-cst)
  (assert (not (null extra-children)))
  (let* ((first              (let ((first (cst:first result)))
                               (cond ((typep first 'wad)
                                      first)
                                     ((and (typep first 'cst:cons-cst)
                                           (typep (cst:first first) 'wad))
                                      (cst:first first)))))
         (first-start-line   (when first (absolute-start-line-number first)))
         (first-start-column (when first (start-column first)))
         (first-end-line     (when first (+ first-start-line (height first))))
         (first-end-column   (when first (end-column first)))
         (rest               (let ((rest (cst:rest result)))
                               (cond ((typep rest 'wad)
                                      rest)
                                     ((and (typep rest 'cst:cons-cst)
                                           (typep (cst:first rest) 'wad))
                                      (cst:first rest)))))
         (rest-start-line    (when rest (absolute-start-line-number rest)))
         (rest-start-column  (when rest (start-column rest)))
         (rest-end-line      (when rest (+ rest-start-line (height rest))))
         (rest-end-column    (when rest (end-column rest))))
                                        ; (assert (not (relative-p cst-child)))
    (let ((before-first '())
          (in-first     '())
          (before-rest  '())
          (in-rest      '())
          (remainder    extra-children))
      (when first
        ;; Before first child
        (loop for extra-child = (first remainder)
                                        ; do (assert (not (relative-p extra-child))) ; TODO
              while (and extra-child
                         (let ((end-line (+ (absolute-start-line-number extra-child) (height extra-child))))
                           (or (< end-line first-start-line)
                               (and (= end-line first-start-line)
                                    (<= (end-column extra-child) first-start-column)))))
              do (push extra-child before-first)
                 (pop remainder))
        ;; Overlapping first child
        (loop for extra-child = (first remainder)
                                        ; do (assert (not (relative-p extra-child))) ; TODO
              while (and extra-child
                         (or (< (absolute-start-line-number extra-child) first-start-line)
                             (and (= (absolute-start-line-number extra-child) first-start-line)
                                  (< (start-column extra-child) first-start-column))))
              do (warn "Dropping ~S ~S" extra-child first)
                 (pop remainder))
        ;; Contained in first child
        (loop for extra-child = (first remainder)
                                        ; do (assert (not (relative-p extra-child))) ; TODO
              while (and extra-child
                         (let ((end-line (+ (absolute-start-line-number extra-child) (height extra-child))))
                           (or (< end-line first-end-line)
                               (and (= end-line first-end-line)
                                    (<= (end-column extra-child) first-end-column)))))
              do (push extra-child in-first)
                 (pop remainder)))
      (when rest
        ;; Before rest child
        (loop for extra-child = (first remainder)
                                        ; do (assert (not (relative-p extra-child))) ; TODO
              while (and extra-child
                         (let ((end-line (+ (absolute-start-line-number extra-child) (height extra-child))))
                           (or (< end-line rest-start-line)
                               (and (= end-line rest-start-line)
                                    (<= (end-column extra-child) rest-start-column)))))
              do (push extra-child before-rest)
                 (pop remainder))
        ;; Overlapping rest
        (loop for extra-child = (first remainder)
                                        ; do (assert (not (relative-p extra-child))) ; TODO
              while (and extra-child
                         (or (< (absolute-start-line-number extra-child) rest-start-line)
                             (and (= (absolute-start-line-number extra-child) rest-start-line)
                                  (< (start-column extra-child) rest-start-column))))
              do (warn "Dropping ~S ~S" extra-child rest)
                 (pop remainder))
        ;; Contained in rest child
        (loop for extra-child = (first remainder)
                                        ; do (assert (not (relative-p extra-child))) ; TODO
              while (and extra-child
                         (let ((end-line (+ (absolute-start-line-number extra-child) (height extra-child))))
                           (or (< end-line rest-end-line)
                               (and (= end-line rest-end-line)
                                    (<= (end-column extra-child) rest-end-column)))))
              do (push extra-child in-first)
                 (pop remainder)))
      (values (nreverse before-first) (nreverse in-first) (nreverse before-rest) (nreverse in-rest) remainder))
    #+old (let ((index (position-if (lambda (non-cst-child)
                                      (assert (not (relative-p non-cst-child)))
                                      (or (> (absolute-start-line-number non-cst-child) child-start-line)
                                          (and (= (absolute-start-line-number non-cst-child) child-start-line)
                                               (> (start-column non-cst-child) child-start-column))))
                                    extra-children)))
            (if index
                (values (subseq extra-children 0 index)
                        (subseq extra-children index))
                (values extra-children '())))))

(defun add-extra-children (result extra-children)
  (typecase result
    (cst:atom-cst
     (unless (typep result 'children-mixin)
       (change-class result 'atom-with-children))
     (reinitialize-instance result :children extra-children))
    (cst:cons-cst
     (multiple-value-bind (before-first in-first before-rest in-rest remaining)
         (distribute-extra-cons-children result extra-children)
       #+no (when before-first

              (let ((existing-children (typecase result
                                         (children-mixin
                                          (children result))
                                         (cst:cons-cst
                                          (change-class result 'cons-with-children)
                                          '()))))
                (typecase result
                  (children-mixin
                   (setf (children result)
                         (if (null existing-children)
                             before-first
                             (merge 'list
                                    before-first
                                    (copy-list existing-children)
                                    (lambda (child1 child2)
                                      (wad-starts-before-wad-p child1 child2))))))
                  )))
       (when (or before-first before-rest remaining)
         (unless (typep result 'cons-extra-children-mixin)
           (change-class result 'cons-with-children))
         (reinitialize-instance result :before-first before-first
                                       :middle       before-rest
                                       :after-rest   (if (typep (cst:rest result) 'cst:cons-cst)
                                                         '()
                                                         remaining)))
       (when in-first
         (warn "cannot add children ~S to~@
                ~A~@
                ~2@Tfirst  ~A~@
                ~2@Tsecond ~A"
               in-first result (cst:first result) (cst:rest result))
                                        ; (break)
         #+no (clouseau:inspect
          (list (cons :result result)
                (cons :before-first before-first)
                (cons :before-first before-first)
                (cons :in-first in-first)
                (cons :before-rest before-rest)
                (cons :in-rest in-rest)
                (cons :remaining remaining)))

                                        ; (add-extra-children (cst:first result) in-first)
         )
       (when (or in-rest (and (typep (cst:rest result) 'cst:cons-cst) remaining))
         (let ((rest (cst:rest result)))
           ;; TODO first cst:first contains a cons-cst, try to add children there?
           ;; for something like ((a #|x|# b) . (c d)) maybe?
           (add-extra-children rest (append in-rest (if (typep (cst:rest result) 'cst:cons-cst)
                                                        remaining
                                                        '())))))
       result))))

(defmethod children ((wad cons-wad))
  ;; TODO maybe cache this?
  (let ((result '()))
    (map-children (lambda (child) (push child result)) wad)
    (nreverse result)))

;;; This method must apply the function only to children which are
;;; WADs and thus have source information.
#+old (defmethod map-children ((function t) (wad cst:cons-cst))
  (let ((function (alexandria:ensure-function function)))
    (flet ((visit-cons (node dotted?)
             (when (typep node 'cons-extra-children-mixin)
               (mapc function (before-first node)))
             (let ((first (cst:first node)))
               (when (typep first 'wad)
                 (funcall function first)))
             (when (typep node 'cons-extra-children-mixin)
               (mapc function (middle node)))
             (when dotted?
               (let ((rest (cst:rest node)))
                 (when (typep rest 'wad)
                   (funcall function rest)))))
           (visit-atom (node)
             (typecase node
               (wad ; dotted list TODO explain
                (funcall function node))
               (atom-with-children
                (map-children function node)))))
      (loop for previous = nil then node
            for node = wad then (cst:rest node)
            if (cst:consp node)
              do (let ((dotted?    (not (or (cst:consp (cst:rest node))
                                            (cst:null (cst:rest node)))))
                       (wad-child? (and (not (eq node wad)) (typep node 'wad))))
                   (if wad-child?
                       (funcall function node)
                       (visit-cons node dotted?))
                   (when (and previous
                              (typep previous 'cons-extra-children-mixin))
                     (mapc function (after-rest previous)))
                   (when dotted?
                     (when (not wad-child?)
                       (when (typep node 'cons-extra-children-mixin)
                         (mapc function (after-rest node))))
                     (loop-finish)))
            else ; if (not (cst:null node))
              do (visit-atom node)
                 (when (and previous
                            (typep previous 'cons-extra-children-mixin))
                   (mapc function (after-rest previous)))
                 (loop-finish)))))

(defmethod map-children ((function t) (wad cst:cons-cst))
  (let ((function (alexandria:ensure-function function)))
    (flet ((visit-cons (node dotted?)
             (when (typep node 'cons-extra-children-mixin)
               (mapc function (before-first node)))
             (let ((first (cst:first node)))
               (when (typep first 'wad)
                 (funcall function first)))
             (when (typep node 'cons-extra-children-mixin)
               (mapc function (middle node)))
             (when dotted?
               (let ((rest (cst:rest node)))
                 (when (typep rest 'wad)
                   (funcall function rest)))))
           (visit-atom (node)
             (typecase node
               (wad ; dotted list TODO explain
                (funcall function node))
               (atom-with-children
                (map-children function node)))))
      (loop for previous = nil then node
            for node = wad then (cst:rest node)
            do (when (and previous
                          (typep previous 'cons-extra-children-mixin))
                 (mapc function (after-rest previous)))
            if (cst:consp node)
              do (let ((dotted?    (not (or (cst:consp (cst:rest node))
                                            (cst:null (cst:rest node)))))
                       (wad-child? (and (not (eq node wad)) (typep node 'wad))))
                   (if wad-child?
                       (funcall function node)
                       (visit-cons node dotted?))
                   (when dotted?
                     #+maybe (when (not wad-child?)
                       (when (typep node 'cons-extra-children-mixin)
                         (mapc function (after-rest node))))
                     (loop-finish)))
            else ; if (not (cst:null node))
              do (visit-atom node)
                 (when (typep node 'cons-extra-children-mixin)
                   (mapc function (after-rest node)))
                 (loop-finish))
      (when (typep wad 'cons-extra-children-mixin)
        (mapc function (after-rest wad))))))

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
             (incf (start-line wad) offset)
             (setf (relative-p wad) nil)))
      (adjust wad)
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
                   do (assert (relative-p wad))
                   do (setf (absolute-start-line-number wad)
                            absolute-start-line-number)
                      (process-wad wad base absolute-start-line-number)
                      (setf base absolute-start-line-number)))
           (process-wad (wad parent-start-line-number wad-start-line-number)
             (process-children (children wad) wad-start-line-number)
             (loop for error-wad in (errors wad)
                   for absolute-start-line-number = (if (relative-p error-wad)
                                                        (+ parent-start-line-number (start-line error-wad))
                                                        (start-line error-wad))
                   do (setf (absolute-start-line-number error-wad)
                            absolute-start-line-number))))
    (process-wad top-level-wad nil (start-line top-level-wad)))
  top-level-wad)
