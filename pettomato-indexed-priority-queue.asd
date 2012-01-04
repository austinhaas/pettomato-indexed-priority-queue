(asdf:defsystem #:pettomato-indexed-priority-queue
  :description "A binary heap based priority queue implementation with efficient support for find, update, replace, and delete operations."
  :author "Austin Haas <austin@pettomato.com>"
  :licence "MIT"
  :version "0.1.1"
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             (:file "priority-queue")))))
