(cl:in-package #:incrementalist)

(defmethod current-line-number ((object buffer-stream))
  (line-number object))

(defmethod (setf current-line-number) ((new-value t) (object buffer-stream))
  (setf (line-number object) new-value))

(defmethod current-item-number ((object buffer-stream))
  (item-number object))

(defmethod (setf current-item-number) ((new-value t) (object buffer-stream))
  (setf (item-number object) new-value))

(defclass buffer-stream (gs:fundamental-character-input-stream)
  ((%lines         :initarg  :lines
                   :reader   lines)
   ;; Current position
   (%file-position :reader   gs:stream-file-position
                   :initform 0)
   (%line-number   :accessor line-number)
   (%item-number   :accessor item-number)
   ;; Cached line information
   (%line-count    :accessor %line-count)
   (%line          :accessor %line)
   (%item-count    :accessor %item-count))
  (:default-initargs
   :lines (alexandria:required-argument :lines)))

(declaim (inline update-line-count-cache update-line-cache))
(defun update-line-count-cache (stream lines)
  (setf (%line-count stream) (flx:nb-elements lines)))

(defun update-line-cache (stream lines new-line-number)
  (let ((line (flx:element* lines new-line-number)))
    (setf (%line       stream) line
          (%item-count stream) (length line))))

(defmethod shared-initialize :after ((instance   buffer-stream)
                                     (slot-names t)
                                     &key (lines nil lines-supplied-p))
  (when lines-supplied-p
    (update-line-count-cache instance lines)
    #+no (let ((line-number (line-number instance)))
      (update-line-cache instance lines line-number))))

(defmethod (setf line-number) :after ((new-value integer)
                                      (object    buffer-stream))
  (update-line-cache object (lines object) new-value))

(defmethod gs:stream-peek-char ((stream buffer-stream))
  (let* ((item-number   (item-number stream))
         (end-of-line-p (= item-number (%item-count stream))))
    (cond ((not end-of-line-p)
           (let ((line (%line stream)))
             (aref line item-number)))
          ((= (line-number stream) (1- (%line-count stream)))
           :eof)
          (t
           #\Newline))))

(defmethod gs:stream-read-char ((stream buffer-stream))
  (let* (line-number
         (item-number   (item-number stream))
         (end-of-line-p (= item-number (%item-count stream))))
    (cond ((not end-of-line-p)
           (prog1
               (let ((line (%line stream)))
                 (aref line item-number))
             (setf (item-number stream) (1+ item-number))))
          ((= (setf line-number (line-number stream))
              (1- (%line-count stream)))
           :eof)
          (t
           (prog1
               #\Newline
             (setf (line-number stream) (1+ line-number) ; updates cache
                   (item-number stream) 0))))))

(defmethod gs:stream-unread-char ((stream buffer-stream) char)
  (declare (ignore char))
  (let* (line-number
         (item-number         (item-number stream))
         (beginning-of-line-p (zerop item-number)))
    (cond ((not beginning-of-line-p)
           (setf (item-number stream) (1- item-number)))
          ((zerop (setf line-number (line-number stream)))
           (error "Attempt to unread a character at position 0"))
          (t
           (setf (line-number stream) (1- line-number)  ; updates cache
                 (item-number stream) (length (%line stream)))))))
