(cl:in-package #:incrementalist.test)

(in-suite :incrementalist)

(test smoke
  "Initial smoke test for the incrementalist system."
  (are-results '((inc::atom-wad ((0 0) (0 8))
                  (:raw (inc:symbol-token :symbol ("FOO" "BAR")))
                  (inc::word-wad ((0 0) (0 3)))
                  (inc::word-wad ((0 5) (0 8)))))
               (parse-result "foo::bar"))

  (are-results '((inc::cons-wad ((0 0) (0 8)) ()
                  (inc::cons-wad ((0 1) (0 8)) ()
                   (inc::atom-wad ((0 2) (0 3))
                    (:raw (inc:numeric-token :value 1)))
                   (inc::atom-wad ((0 4) (0 5))
                    (:raw (inc:numeric-token :value 2)))
                   (inc::atom-wad ((0 6) (0 7))
                    (:raw (inc:numeric-token :value 3))))))
               (parse-result "'(1 2 3)"))

  (are-results '((inc::cons-wad ((0 0) (0 13)) ()
                  (inc::cons-wad ((0 1) (0 13)) ()
                   (inc::labeled-object-definition-wad ((0 2) (0 6)) ()
                    (inc::atom-wad ((0 5) (0 6))
                     (:raw (inc:numeric-token :value 1))))
                   (inc::labeled-object-reference-wad ((0 7) (0 10))
                    (:raw (inc:numeric-token :value 1)))
                   (inc::atom-wad ((0 11) (0 12))
                    (:raw (inc:numeric-token :value 3))))))
               (parse-result "'(#1=1 #1# 3)")))

(defun expected-consing-dot (location)
  `(inc::atom-wad ,location (:raw (symbol)))) ; TODO make this a token?

(defun expected-symbol-wad (location symbol-name
                            &key (package-name "INCREMENTALIST.TEST.TEST-PACKAGE")
                                 (token-class  'inc:non-existing-symbol-token)
                                 (words        `((inc::word-wad ,location))))
  `(inc::atom-wad ,location
    (:raw (,token-class :symbol (,package-name ,symbol-name)))
    ,@words))

(test incomplete
  (are-results
   `((inc::cons-wad ((0 0) (0 19)) ()
                    (inc::cons-wad ((0 1) (0 8)) ()
                                   ,(expected-symbol-wad '((0 2) (0 3)) "A")
                                   ,(expected-consing-dot '((0 4) (0 5)))
                                   ,(expected-symbol-wad '((0 6) (0 7)) "B"))
                    ,(expected-consing-dot '((0 9) (0 10)))
                    (inc::cons-wad ((0 11) (0 18)) ()
                                   ,(expected-symbol-wad '((0 12) (0 13)) "C")
                                   ,(expected-consing-dot '((0 14) (0 15)))
                                   ,(expected-symbol-wad '((0 16) (0 17)) "D"))))
   (parse-result "((a . b) . (c . d))"))

  (are-results
   `((inc::cons-wad ((0 0) (0 15))
                    (:errors ((((0 15) (0 15)) eclector.reader::unterminated-list)))
                    (inc::cons-wad ((0 1) (0 8)) ()
                                   ,(expected-symbol-wad '((0 2) (0 3)) "A")
                                   ,(expected-consing-dot '((0 4) (0 5)))
                                   ,(expected-symbol-wad '((0 6) (0 7)) "B"))
                    ,(expected-consing-dot '((0 9) (0 10)))
                    (inc::cons-wad ((0 11) (0 15))
                                   (:errors ((((0 15) (0 15)) eclector.reader::unterminated-list)
                                             (((0 15) (0 15)) eclector.reader::end-of-input-after-consing-dot)))
                                   ,(expected-symbol-wad '((0 12) (0 13)) "C")
                                   ,(expected-consing-dot '((0 14) (0 15))))))
   (parse-result "((a . b) . (c ."))

  (are-results `((inc::cons-wad ((0 0) (0 16))
                 (:errors ((((0 16) (0 16)) eclector.reader::unterminated-list)
                           (((0 16) (0 16)) eclector.reader::end-of-input-after-consing-dot)))
                 (inc::cons-wad ((0 1) (0 8)) ()
                  ,(expected-symbol-wad '((0 2) (0 3)) "A")
                  ,(expected-consing-dot '((0 4) (0 5)))
                  ,(expected-symbol-wad '((0 6) (0 7)) "B"))
                 ,(expected-consing-dot '((0 9) (0 10)))
                 (inc::semicolon-comment-wad ((0 11) (0 16)) () ; TODO helper function
                  (inc::word-wad ((0 13) (0 16))))))
               (parse-result "((a . b) . ; foo")))

(test errors
  "Tests for errors."
  (are-results '((inc::atom-wad ((0 0) (0 16))
                  (:errors ((((0 2) (0 16)) eclector.reader:unknown-character-name)))))
               (parse-result "#\\does-not-exist"))
  (are-results '((inc::atom-wad ((0 0) (0 7))
                  (:errors ((((0 4) (0 5)) eclector.reader:digit-expected)))))
               (parse-result "#b00200")))
