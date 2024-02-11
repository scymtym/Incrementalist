(cl:in-package #:incrementalist.test)

(defun map-all-system-files (function
                             &key (systems '("incrementalist"
                                             "incrementalist/test")))
  (labels ((map-component-files (component)
             (typecase component
               (asdf:source-file
                (when (equal (asdf:file-type component) "lisp")
                  (let ((filename (asdf:component-pathname component)))
                    (funcall function filename))))
               (asdf:module
                (let ((children (asdf:component-children component)))
                  (mapc #'map-component-files children))))))
    (alexandria:mappend (lambda (system-name)
                          (let ((system (asdf:find-system system-name)))
                            (map-component-files system)))
                        systems)))
