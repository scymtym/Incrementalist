(cl:in-package #:incrementalist.test)

(in-suite :incrementalist)

(test regressions
  (let ((fiveam:*test-dribble* nil))
   (finishes (insert-then-delete
              "; ,
 (defun position-is-before-wad-p)
"))

    (finishes (insert-then-delete "foo'"))

    (finishes (insert-then-delete "(cl:in-package #:incrementalist)

              (defun whitespacep (character)
                (member character '(#\\Space #\\Newline)))

              (defun punctuationp (character)
                (member character '(#\\. #\\? #\\! #\\: #\\, #\\;
                                    #\\( #\\) #\\< #\\> #\\[ #\\] #\\{ #\\}
                                    #\\\" #\\' #\\` #\\/ #\\_ #\\- #\\+ #\\* #\\% #\\= #\\#)))

;;; Return the line number and the column number of CURSOR as two
;;; values.
              (defun cursor-positions (cursor)
                (values (cluffer:line-number cursor)
                        (cluffer:cursor-position cursor)))

;;; Set the line number and the column number of CURSOR.
              (defun set-cursor-positions (cursor line-number column-number)
                (let ((buffer (cluffer:buffer cursor)))
                  (when (cluffer:cursor-attached-p cursor)
                    (cluffer:detach-cursor cursor))
                  (cluffer:attach-cursor
                   cursor
                   (cluffer:find-line buffer line-number)
                   column-number)))
"))

    (finishes (insert-then-delete
               "(
(a .
`b))"))

    (finishes (insert-then-delete ";,
(a () ;
#-b c)"))))
