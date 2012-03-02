(asdf:defsystem #:pettomato-indexed-priority-queue-tests
  :description "Test suite for pettomato-indexed-priority-queue."
  :author "Austin Haas <austin@pettomato.com>"
  :licence "MIT"
  :version "0.1.3"
  :depends-on (#:pettomato-indexed-priority-queue
               #:fiveam)
  :components ((:module "test"
                :serial t
                :components ((:file "package")
                             (:file "tests")))))
