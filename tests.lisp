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
                      (lambda (item) (gethash item hash)))))

(defparameter *test-queue-size* 40)

(def-suite basic-suite :description "Basic suite.")

(in-suite basic-suite)

(test basic
  (finishes (make-queue #'<))
  (let ((q (make-queue #'<)))
    (is (queue-empty-p q))
    (signals empty-queue-error (queue-pop q))
    (signals empty-queue-error (queue-peek q))
    (is (eq q (queue-insert q 1)))
    (is (= 1 (queue-peek q)))
    (is (= 1 (queue-pop q)))
    (is (queue-empty-p q))
    (signals empty-queue-error (queue-pop q))
    (signals empty-queue-error (queue-peek q))))

(test delete
  (let ((q (make-queue (lambda (a b) (< (second a) (second b)))))
        (items (loop for i from 0 below *test-queue-size* collecting (list i (random 999)))))
    (dolist (item items)
      (queue-insert q item))
    (loop repeat 5 do
      (let* ((random-index (random (length items)))
             (random-item (nth random-index items)))
        (is (eq q (queue-delete q random-item)))
        (setf items (delete random-item items))))
    ;; Note that we only compare the second value because the
    ;; priority queue is not a stable sort.
    (let ((queue (loop while (not (queue-empty-p q)) collecting (queue-pop q)))
          (control (sort items #'< :key #'second)))
      (is-false (mismatch queue control :key #'second)))))

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

(test update
  (let ((q (make-queue (lambda (a b) (< (second a) (second b)))))
        (items (loop for i from 0 below *test-queue-size* collecting (list i (random 999)))))
    (dolist (item items)
      (queue-insert q item))
    (loop repeat 5 do
     (let ((random-item (nth (random (length items)) items)))
       (setf (second random-item) (random 999))
       (is (eq q (queue-update q random-item)))))
    ;; Note that we only compare the second value because the
    ;; priority queue is not a stable sort.
    (let ((queue (loop while (not (queue-empty-p q)) collecting (queue-pop q)))
          (control (sort items #'< :key #'second)))
      (is-false (mismatch queue control :key #'second)))))

(test replace
  (let ((q (make-queue (lambda (a b) (< (second a) (second b)))))
        (items (loop for i from 0 below *test-queue-size* collecting (list i (random 999)))))
    (dolist (item items)
      (queue-insert q item))
    (loop for i from 0 below 5 do
      (let ((random-item (nth (random (length items)) items))
            (new-item (list (+ *test-queue-size* i) (random 999))))
        (is (eq q (queue-replace q random-item new-item)))
        (setf items (nsubstitute new-item random-item items))))
    ;; Note that we only compare the second value because the
    ;; priority queue is not a stable sort.
    (let ((queue (loop while (not (queue-empty-p q)) collecting (queue-pop q)))
          (control (sort items #'< :key #'second)))
      (is-false (mismatch queue control :key #'second)))))

(defun run-tests ()
  (run-test 'basic-suite))
