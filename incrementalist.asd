(defsystem "incrementalist"
  :description "Incremental parsing of Common Lisp code represented as a Cluffer buffer."
  :license     "BSD" ; see COPYING file
  :author      ("Robert Strandh"
                #1="Jan Moringen <jmoringe@techfak.uni-bielefeld.de>")
  :maintainer  #1#

  :version     (:read-file-form "data/version-string.sexp")
  :depends-on  ("trivial-gray-streams"
                "cluffer"
                "flexichain"
                "eclector"
                "spell")

  :components  ((:module "code"
                 :serial      t
                 :components ((:file "packages")
                              (:file "utilities")
                              (:file "wad")
                              (:file "buffer-stream")
                              (:file "cache")
                              (:file "analyzer")
                              (:file "token")
                              (:file "client")
                              (:file "parse")
                              (:file "read-forms")
                              (:file "update-cache")
                              (:file "find-wad-beginning-line")
                              (:file "find-wad-containing-position")
                              (:file "mapwad")
                              (:file "check-wad-graph"))))

  :in-order-to ((test-op (test-op "incrementalist/test"))))

(defsystem "incrementalist/test"
  :description "Tests for the incrementalist system."
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"

  :version     (:read-file-form "data/version-string.sexp")
  :depends-on  ("fiveam"

                "incrementalist")

  :components  ((:module     "test"
                 :serial     t
                 :components ((:file "package")
                              ;; Test utilities
                              (:file "utilities")
                              ;; Tests
                              (:file "test"))))

  :perform     (test-op (operation component)
                 (uiop:symbol-call '#:incrementalist.test '#:run-tests)))
