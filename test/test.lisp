(cl:in-package #:incrementalist.test)

(in-suite :incrementalist)

(test smoke
  "Initial smoke test for the incrementalist system."
  (analysis-cases ()
    ("#\\Return"
     '((inc::atom-wad ((0 0) (0 8))
        ; TODO (:raw (inc:value-token :value #\Return))
        )))
    ("\"foo\""
     '((inc::atom-wad ((0 0) (0 5))
        () ; TODO (:raw (inc:value-token :value #\Return))
        (inc::word-wad ((0 1) (0 4))))))
    ("foo::bar"
     '((inc::atom-wad ((0 0) (0 8))
        (:raw (inc:symbol-token :symbol ("FOO" "BAR")))
        (inc::word-wad ((0 0) (0 3)))
        (inc::word-wad ((0 5) (0 8))))))
    ("'(1 2 3)"
     '((inc::cons-wad ((0 0) (0 8)) ()
        (inc::cons-wad ((0 1) (0 8)) ()
         (inc::atom-wad ((0 2) (0 3))
          (:raw (inc:numeric-token :value 1)))
         (inc::atom-wad ((0 4) (0 5))
          (:raw (inc:numeric-token :value 2)))
         (inc::atom-wad ((0 6) (0 7))
          (:raw (inc:numeric-token :value 3)))))))
    ("'(#1=1 #1# 3)"
     '((inc::cons-wad ((0 0) (0 13)) ()
        (inc::cons-wad ((0 1) (0 13)) ()
         (inc::labeled-object-definition-wad ((0 2) (0 6)) ()
          (inc::atom-wad ((0 5) (0 6))
           (:raw (inc:numeric-token :value 1))))
         (inc::labeled-object-reference-wad ((0 7) (0 10))
          (:raw (inc:numeric-token :value 1)))
         (inc::atom-wad ((0 11) (0 12))
          (:raw (inc:numeric-token :value 3)))))))
    #+TODO ("#+a b"
     `((inc::sharpsign-plus-wad ((0 0) (0 5))
       (:raw (inc:non-existing-symbol-token :symbol ("INCREMENTALIST.TEST.TEST-PACKAGE" "B")))
       ,(expected-symbol-wad '((0 2) (0 3)) "A" :token-class  'inc:existing-symbol-token
                                                :package-name "KEYWORD")
       ,(expected-symbol-wad '((0 4) (0 5)) "B"))))
    ("#-a b"
     `((inc::atom-wad ((0 0) (0 5)) ; TODO should be sharpsign-minus-wad
        (:raw (inc:non-existing-symbol-token :symbol ("INCREMENTALIST.TEST.TEST-PACKAGE" "B")))
        ,(expected-symbol-wad '((0 2) (0 3)) "A" :token-class  'inc:existing-symbol-token
                                                 :package-name "KEYWORD")
        ,(expected-symbol-wad '((0 4) (0 5)) "B"))))))

(test incomplete
  "Tests for incomplete expressions."
  (analysis-cases ()
    ("(a #||#)"
     `((inc::cons-wad-with-extra-children ((0 0) (0 8)) ()
        ,(expected-symbol-wad '((0 1) (0 2)) "A")
        (inc:block-comment-wad ((0 3) (0 7))))))
    ("((a . b) . ; foo"
     `((inc::cons-wad-with-extra-children ((0 0) (0 16))
        (:errors ((((0 16) (0 16)) eclector.reader::unterminated-list)
                  (((0 16) (0 16)) eclector.reader::end-of-input-after-consing-dot)))
        (inc::cons-wad-with-extra-children ((0 1) (0 8)) ()
         ,(expected-symbol-wad '((0 2) (0 3)) "A")
         ,(expected-consing-dot '((0 4) (0 5)))
         ,(expected-symbol-wad '((0 6) (0 7)) "B"))
        ,(expected-consing-dot '((0 9) (0 10)))
        (inc::semicolon-comment-wad ((0 11) (0 16)) () ; TODO helper function
         (inc::word-wad ((0 13) (0 16)))))))

    ("((a . b) . (c ."
     `((inc::cons-wad-with-extra-children ((0 0) (0 15))
        (:errors ((((0 15) (0 15)) eclector.reader::unterminated-list)))
        (inc::cons-wad-with-extra-children ((0 1) (0 8)) ()
         ,(expected-symbol-wad '((0 2) (0 3)) "A")
         ,(expected-consing-dot '((0 4) (0 5)))
         ,(expected-symbol-wad '((0 6) (0 7)) "B"))
        ,(expected-consing-dot '((0 9) (0 10)))
        (inc::cons-wad-with-extra-children ((0 11) (0 15))
         (:errors ((((0 15) (0 15)) eclector.reader::unterminated-list)
                   (((0 15) (0 15)) eclector.reader::end-of-input-after-consing-dot)))
         ,(expected-symbol-wad '((0 12) (0 13)) "C")
         ,(expected-consing-dot '((0 14) (0 15)))))))

    ("((a . b) . (c . d))"
     `((inc::cons-wad-with-extra-children ((0 0) (0 19)) ()
        (inc::cons-wad-with-extra-children ((0 1) (0 8)) ()
         ,(expected-symbol-wad '((0 2) (0 3)) "A")
         ,(expected-consing-dot '((0 4) (0 5)))
         ,(expected-symbol-wad '((0 6) (0 7)) "B"))
        ,(expected-consing-dot '((0 9) (0 10)))
        (inc::cons-wad-with-extra-children ((0 11) (0 18)) ()
         ,(expected-symbol-wad '((0 12) (0 13)) "C")
         ,(expected-consing-dot '((0 14) (0 15)))
         ,(expected-symbol-wad '((0 16) (0 17)) "D")))))

    ("((a . b) . (c . d) #||#)"
     `((inc::cons-wad-with-extra-children ((0 0) (0 24)) ()
        (inc::cons-wad-with-extra-children ((0 1) (0 8)) ()
         ,(expected-symbol-wad '((0 2) (0 3)) "A")
         ,(expected-consing-dot '((0 4) (0 5)))
         ,(expected-symbol-wad '((0 6) (0 7)) "B"))
        ,(expected-consing-dot '((0 9) (0 10)))
        (inc::cons-wad-with-extra-children ((0 11) (0 18)) ()
         ,(expected-symbol-wad '((0 12) (0 13)) "C")
         ,(expected-consing-dot '((0 14) (0 15)))
         ,(expected-symbol-wad '((0 16) (0 17)) "D"))
        (inc::block-comment-wad ((0 19) (0 23))))))

    ("((a . b) c . d #||#)"
     `((inc::cons-wad-with-extra-children ((0 0) (0 20)) ()
       (inc::cons-wad-with-extra-children ((0 1) (0 8)) ()
        ,(expected-symbol-wad '((0 2) (0 3)) "A")
        ,(expected-consing-dot '((0 4) (0 5)))
        ,(expected-symbol-wad '((0 6) (0 7)) "B"))
       ,(expected-symbol-wad '((0 9) (0 10)) "C")
       ,(expected-consing-dot '((0 11) (0 12)))
       ,(expected-symbol-wad '((0 13) (0 14)) "D")
       (inc::block-comment-wad ((0 15) (0 19))))))

    ("((a . b) #||# #||# . (c . d) #||#)"
     `((inc::cons-wad-with-extra-children ((0 0) (0 34)) ()
       (inc::cons-wad-with-extra-children ((0 1) (0 8)) ()
        ,(expected-symbol-wad '((0 2) (0 3)) "A")
        ,(expected-consing-dot '((0 4) (0 5)))
        ,(expected-symbol-wad '((0 6) (0 7)) "B"))
       (inc::block-comment-wad ((0 9) (0 13)))
       (inc::block-comment-wad ((0 14) (0 18)))
       ,(expected-consing-dot '((0 19) (0 20)))
       (inc::cons-wad-with-extra-children ((0 21) (0 28)) ()
        ,(expected-symbol-wad '((0 22) (0 23)) "C")
        ,(expected-consing-dot '((0 24) (0 25)))
        ,(expected-symbol-wad '((0 26) (0 27)) "D"))
       (inc::block-comment-wad ((0 29) (0 33))))))

    ("`(a . (b ,c))"
     `((inc::cons-wad ((0 0) (0 13)) ()
        (inc::cons-wad-with-extra-children ((0 1) (0 13)) ()
         ,(expected-symbol-wad '((0 2) (0 3)) "A")
         ,(expected-consing-dot '((0 4) (0 5)))
         (inc::cons-wad ((0 6) (0 12)) ()
          ,(expected-symbol-wad '((0 7) (0 8)) "B")
         (inc::cons-wad ((0 9) (0 11)) ()
          ,(expected-symbol-wad '((0 10) (0 11)) "C")))))))))

(test errors
  "Tests for errors."
  (analysis-cases ()
    ("#\\does-not-exist"
     '((inc::atom-wad ((0 0) (0 16))
        (:errors ((((0 2) (0 16)) eclector.reader:unknown-character-name))))))
    ("#b00200"
     '((inc::atom-wad ((0 0) (0 7))
        (:errors ((((0 4) (0 5)) eclector.reader:digit-expected))))))))

(test todo
  (finishes (parse-result "(
; a
; b
(1 . 2)
; c
; d
3"))
  (finishes (parse-result "((a . b) . (c ."))

  (finishes (parse-result "(
; a
; b
(1 . 2)
; c
.
; d
3"))


  (finishes (parse-result "(
; a
(1 . 2)
#||#
.
#||#
3"))

  (finishes (parse-result ",`bar"))

  (finishes (parse-result "`foo, `bar")))
