(asdf:defsystem #:priority-queue-tests
  :serial t
  :depends-on (#:priority-queue
               #:fiveam)
  :components ((:file "test-package")
               (:file "tests")))
