(asdf:defsystem #:pettomato-indexed-priority-queue-tests
  :serial t
  :depends-on (#:pettomato-indexed-priority-queue
               #:fiveam)
  :components ((:file "test-package")
               (:file "tests")))
