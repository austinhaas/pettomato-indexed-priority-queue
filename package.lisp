(defpackage #:priority-queue
  (:use #:cl)
  (:export
   #:make-empty-queue
   #:queue-empty-p
   #:queue-peek
   #:queue-pop
   #:queue-insert
   #:queue-replace
   #:queue-delete
   #:queue-update))
