(in-package #:priority-queue-tests)

(defun run-test (test-spec)
  "Same as run!, but adds an extra message."
  (format t "~&Testing: ~S" test-spec)
  (run! test-spec))

(def-suite basic-suite :description "Basic suite.")

(in-suite basic-suite)

(defun make-queue (compare-fn)
  (let ((hash (make-hash-table :test 'eql)))
    (make-empty-queue compare-fn
                      (lambda (item index) (setf (gethash item hash) index))
                      (lambda (item) (gethash item hash)))))

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

(defparameter *test-queue-size* 10)

(test delete
  (let ((q (make-queue (lambda (a b) (< (second a) (second b)))))
        (items (loop for i from 0 below *test-queue-size* collecting (list i (random 999)))))
    (dolist (item items)
      (queue-insert q item))
    (let ((random-item (nth (random *test-queue-size*) items)))
      (is (eq q (queue-delete q random-item)))
      (setf items (delete random-item items))
      ;; Note that we only compare the second value because the
      ;; priority queue is not a stable sort.
      (let ((queue (loop while (not (queue-empty-p q)) collecting (queue-pop q)))
            (control (sort items #'< :key #'second)))
        (is-false (mismatch queue control :key #'second))))))

(test update
  (let ((q (make-queue (lambda (a b) (< (second a) (second b)))))
        (items (loop for i from 0 below *test-queue-size* collecting (list i (random 999)))))
    (dolist (item items)
      (queue-insert q item))
    (let ((random-item (nth (random *test-queue-size*) items)))
      (setf (second random-item) (random 999))
      (is (eq q (queue-update q random-item)))
      ;; Note that we only compare the second value because the
      ;; priority queue is not a stable sort.
      (let ((queue (loop while (not (queue-empty-p q)) collecting (queue-pop q)))
            (control (sort items #'< :key #'second)))
       (is-false (mismatch queue control :key #'second))))))

(test replace
  (let ((q (make-queue (lambda (a b) (< (second a) (second b)))))
        (items (loop for i from 0 below *test-queue-size* collecting (list i (random 999)))))
    (dolist (item items)
      (queue-insert q item))
    (let ((random-item (nth (random *test-queue-size*) items))
          (new-item (list *test-queue-size* (random 999))))
      (is (eq q (queue-replace q random-item new-item)))
      (setf items (nsubstitute new-item random-item items))
      ;; Note that we only compare the second value because the
      ;; priority queue is not a stable sort.
      (let ((queue (loop while (not (queue-empty-p q)) collecting (queue-pop q)))
            (control (sort items #'< :key #'second)))
       (is-false (mismatch queue control :key #'second))))))

(defun run-tests ()
  (run-test 'basic-suite))
