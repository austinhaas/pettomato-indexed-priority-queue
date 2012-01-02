(asdf:defsystem #:priority-queue
  :description "A binary heap based priority queue implementation with efficient support for find, update, replace, and delete operations."
  :author "Austin Haas <austin@pettomato.com>"
  :licence "MIT"
  :version "0.1.0"
  :serial t
  :components ((:file "package")
               (:file "priority-queue")))
