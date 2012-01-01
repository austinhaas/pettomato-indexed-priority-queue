(in-package #:priority-queue-tests)

(defun run-test (test-spec)
  "Same as run!, but adds an extra message."
  (format t "~&Testing: ~S" test-spec)
  (run! test-spec))

(defun make-queue (compare-fn)
  (let ((hash (make-hash-table :test 'eql)))
    (make-empty-queue compare-fn
                      (lambda (item index)
                        (setf (gethash item hash) index))
                      (lambda (item) (gethash item hash -1)))))

(defparameter *test-queue-size* 40)
(defparameter *last-random-state* nil)

(def-suite basic-suite :description "Basic suite.")

(in-suite basic-suite)

(test basic
  (finishes (make-queue #'<))
  (let ((q (make-queue #'<)))
    (is (queue-empty-p q))
    (signals empty-queue-error (queue-pop q))
    (signals empty-queue-error (queue-peek q))
    (is (eq q (queue-insert q 2)))
    (is (eq q (queue-insert q 3)))
    (is (eq q (queue-insert q 1)))
    (is (eq q (queue-insert q 4)))
    (is (eq q (queue-insert q 0)))
    (is-false (queue-empty-p q))
    (is (= 0 (queue-peek q)))
    (is (= 0 (queue-pop q)))
    (is (= 1 (queue-pop q)))
    (is (= 2 (queue-pop q)))
    (is (= 3 (queue-pop q)))
    (is (= 4 (queue-pop q)))
    (is (queue-empty-p q))
    (signals empty-queue-error (queue-pop q))
    (signals empty-queue-error (queue-peek q))))

(test delete
  (let ((q (make-queue #'<)))
    (signals empty-queue-error (queue-delete q nil))
    (queue-insert q 1)
    (signals item-not-found-error (queue-delete q 2))
    (is (eq q (queue-delete q 1)))
    (is (queue-empty-p q))
    (queue-insert q 1)
    (queue-insert q 2)
    ;; delete first
    (queue-delete q 1)
    (is (= (queue-peek q) 2))
    ;; delete last
    (queue-insert q 1)
    (queue-delete q 2)
    (is (= (queue-peek q) 1))))

(test delete2
  ;; This test case exposed a bug in heap-delete where we were
  ;; replacing the deleted element with the last element, but only
  ;; calling heapify from there, rather than first checking if the
  ;; replacement was better than its new parent and calling
  ;; heap-improve-key in that case.
  (let ((q (make-queue (lambda (a b) (< (second a) (second b)))))
        (items '((0 38) (1 33) (2 266) (3 233) (4 650) (5 872) (6 512) (7 222) (8 229)
                 (9 617) (10 571) (11 523) (12 989) (13 439) (14 775) (15 371) (16 944))))
    (dolist (item items)
      (queue-insert q item))
    (loop for random-index in '(2 3 2) do
      (let ((random-item (nth random-index items)))
        (is (eq q (queue-delete q random-item)))
        (setf items (remove random-item items))))
    ;; Note that we only compare the second value because the
    ;; priority queue is not a stable sort.
    (let ((queue (loop while (not (queue-empty-p q)) collecting (queue-pop q)))
          (control (sort items #'< :key #'second)))
      (is-false (mismatch queue control :key #'second)))))

(test random-delete
  (let ((q (make-queue (lambda (a b) (< (second a) (second b)))))
        (items (loop for i from 0 below *test-queue-size* collecting (list i (random 999)))))
    (dolist (item items)
      (queue-insert q item))
    (loop repeat 15 do
      (let* ((random-index (random (length items)))
             (random-item (nth random-index items)))
        (is (eq q (queue-delete q random-item)))
        (setf items (delete random-item items))))
    ;; Note that we only compare the second value because the
    ;; priority queue is not a stable sort.
    (let ((queue (loop while (not (queue-empty-p q)) collecting (queue-pop q)))
          (control (sort items #'< :key #'second)))
      (is-false (mismatch queue control :key #'second)))))

(test update
  (let ((q (make-queue (lambda (a b) (< (second a) (second b))))))
    (signals item-not-found-error (queue-update q nil))
    (let ((item (list 0 200)))
      (queue-insert q item)
      (queue-insert q (list 1 100))
      (queue-insert q (list 2 300))
      ;; move up
      (setf (second item) 0)
      (is (eq q (queue-update q item)))
      (is (eq (queue-peek q) item))
      ;; move down
      (setf (second item) 400)
      (is (eq q (queue-update q item)))
      (queue-pop q)
      (queue-pop q)
      (is (eq (queue-peek q) item)))))

(test random-update
  (let ((q (make-queue (lambda (a b) (< (second a) (second b)))))
        (items (loop for i from 0 below *test-queue-size* collecting (list i (random 999)))))
    (dolist (item items)
      (queue-insert q item))
    (loop repeat 15 do
     (let ((random-item (nth (random (length items)) items)))
       (setf (second random-item) (random 999))
       (is (eq q (queue-update q random-item)))))
    ;; Note that we only compare the second value because the
    ;; priority queue is not a stable sort.
    (let ((queue (loop while (not (queue-empty-p q)) collecting (queue-pop q)))
          (control (sort items #'< :key #'second)))
      (is-false (mismatch queue control :key #'second)))))

(test replace
  (let ((q (make-queue (lambda (a b) (< (second a) (second b))))))
    (signals item-not-found-error (queue-replace q nil nil))
    (let ((item (list 0 200))
          (replacement-1 (list 3 0))
          (replacement-2 (list 4 400)))
      (queue-insert q item)
      (queue-insert q (list 1 100))
      (queue-insert q (list 2 300))
      ;; move up
      (is (eq q (queue-replace q item replacement-1)))
      (is (eq (queue-peek q) replacement-1))
      (signals item-not-found-error (queue-replace q item nil))
      ;; move down
      (is (eq q (queue-replace q replacement-1 replacement-2)))
      (queue-pop q)
      (queue-pop q)
      (is (eq (queue-peek q) replacement-2)))))

(test random-replace
  (let ((q (make-queue (lambda (a b) (< (second a) (second b)))))
        (items (loop for i from 0 below *test-queue-size* collecting (list i (random 999)))))
    (dolist (item items)
      (queue-insert q item))
    (loop for i from 0 below 15 do
      (let ((random-item (nth (random (length items)) items))
            (new-item (list (+ *test-queue-size* i) (random 999))))
        (is (eq q (queue-replace q random-item new-item)))
        (setf items (nsubstitute new-item random-item items))))
    ;; Note that we only compare the second value because the
    ;; priority queue is not a stable sort.
    (let ((queue (loop while (not (queue-empty-p q)) collecting (queue-pop q)))
          (control (sort items #'< :key #'second)))
      (is-false (mismatch queue control :key #'second)))))

(test random-everything
  (loop repeat 100 do
    (loop with items = nil
          with q = (make-queue (lambda (a b) (< (second a) (second b))))
          for i from 0 below 999
          for random-int = (if (= (length items) 0)
                               0
                               (random 100))
          do (cond ((< random-int 30)
                    ;; insert
                    (let ((item (list i (random 999))))
                      (push item items)
                      (is (eq q (queue-insert q item)))))
                   ((< random-int 45)
                    ;; pop
                    (setf items (sort items #'< :key #'second))
                    ;; There is a chance they aren't the same item,
                    ;; but they have the same value. In order to keep
                    ;; things consistent, we find the match in items
                    ;; and delete it, rather than just popping the
                    ;; first item off.
                    (let ((item (queue-pop q)))
                      (is (eql (second (first items))
                               (second item)))
                      (setf items (delete item items))))
                   ((< random-int 60)
                    ;; delete
                    (let ((item (nth (random (length items)) items)))
                      (setf items (delete item items))
                      (is (eq q (queue-delete q item)))))
                   ((< random-int 80)
                    ;; update
                    (let ((item (nth (random (length items)) items)))
                      (setf (second item) (random 999))
                      (is (eq q (queue-update q item))))
                    )
                   ((< random-int 100)
                    ;; replace
                    (let ((item (nth (random (length items)) items))
                          (new (list i (random 999))))
                      (is (eq q (queue-replace q item new)))
                      (setf items (nsubstitute new item items)))))
          finally
             ;; Note that we only compare the second value because the
             ;; priority queue is not a stable sort.
             (let ((queue (loop while (not (queue-empty-p q)) collecting (queue-pop q)))
                   (control (sort items #'< :key #'second)))
               (is-false (mismatch queue control :key #'second))))))

(defun run-tests (&optional (random-seed (random 999999)))
  (format t "~&random seed: ~A~%" random-seed)
  (let ((*random-state* (sb-ext:seed-random-state random-seed)))
    (run-test 'basic-suite)))

(defun run-profile (&optional (random-seed (random 999999)))
  (format t "~&random seed: ~A~%" random-seed)
  (let ((*random-state* (sb-ext:seed-random-state random-seed)))
    (sb-profile:profile "PRIORITY-QUEUE")
    (loop repeat 100 do
      (loop with items = nil
            with q = (make-queue (lambda (a b) (< (second a) (second b))))
            for i from 0 below 9999
            for random-int = (if (= (length items) 0)
                                 0
                                 (random 100))
            do (cond ((< random-int 30)
                      ;; insert
                      (let ((item (list i (random 999))))
                        (push item items)
                        (queue-insert q item)))
                     ((< random-int 45)
                      ;; pop
                      ;; There is a chance they aren't the same item,
                      ;; but they have the same value. In order to keep
                      ;; things consistent, we find the match in items
                      ;; and delete it, rather than just popping the
                      ;; first item off.
                      (let ((item (queue-pop q)))
                        (setf items (delete item items))))
                     ((< random-int 60)
                      ;; delete
                      (let ((item (nth (random (length items)) items)))
                        (setf items (delete item items))
                        (queue-delete q item)))
                     ((< random-int 80)
                      ;; update
                      (let ((item (nth (random (length items)) items)))
                        (setf (second item) (random 999))
                        (queue-update q item))
                      )
                     ((< random-int 100)
                      ;; replace
                      (let ((item (nth (random (length items)) items))
                            (new (list i (random 999))))
                        (queue-replace q item new)
                        (setf items (nsubstitute new item items)))))
            finally
               ;; Note that we only compare the second value because the
               ;; priority queue is not a stable sort.
               (let ((queue (loop while (not (queue-empty-p q)) collecting (queue-pop q)))
                     (control (sort items #'< :key #'second)))
                 (when (mismatch queue control :key #'second)
                     (error "Test failed.")))))
    (sb-profile:report :print-no-call-list nil)
    (sb-profile:reset)
    (sb-profile:unprofile "PRIORITY-QUEUE")))
