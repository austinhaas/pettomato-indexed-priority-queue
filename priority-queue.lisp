(in-package #:priority-queue)

;;;; Conditions

(define-condition empty-queue-error (error)
  ()
  (:documentation "Signaled when queue-peek or queue-pop is called on an empty queue."))

(define-condition item-not-found-error (error)
  ()
  (:documentation "Signaled when an operation depends on an item that was not found in the queue."))

;;;; Basic Operations on Queues

(defun make-empty-queue (compare-fn set-index-fn get-index-fn)
  "Return a new empty queue.

compare-fn is a function that takes two arguments and returns a true
value iff the first argument should be prioritized higher in the
queue. If the two items have equal priority, then compare-fn should
return nil.

set-index-fn is a function that takes two arguments: the first is an item
in the queue and the second is the current index position of that item
in the heap. This function should do something to record the
association between the item and the index, so that the index can be
used later with the queue mutating operations that require such an
index.

get-index-fn is a function that takes an item and returns the heap
index that was associated with it via set-index-fn.
"
  (make-q :compare-fn compare-fn
          :set-index-fn set-index-fn
          :get-index-fn get-index-fn
          :items (make-heap)))

(defun queue-empty-p (q)
  "Are there no items in the queue?"
  (= (length (q-items q)) 0))

(defun queue-peek (q)
  "Return the element at the front of the queue. Signals
empty-queue-error if the queue is empty."
  (if (queue-empty-p q)
      (error 'empty-queue-error)
      (aref (q-items q) 0)))

(defun queue-pop (q)
  "Remove the element from the front of the queue and return
it. Signals empty-queue-error if the queue is empty."
  (if (queue-empty-p q)
      (error 'empty-queue-error)
      (heap-extract-min (q-items q) (q-compare-fn q) (q-set-index-fn q))))

(defun queue-insert (q item)
  "Insert the item by priority according to the compare
function. Returns the queue."
  (heap-insert (q-items q) item (q-compare-fn q) (q-set-index-fn q))
  q)

(defun queue-replace (q old new)
  "Replace old with new in the queue. This is analogous to deleting
the old item and inserting the new one, but it is almost always more
efficient (and never less efficient) because we can just fix up the
heap from the position of the swapped item. Returns the queue. Signals
item-not-found-error if the old item wasn't found."
  (let ((compare-fn (q-compare-fn q))
        (items (q-items q))
        (index (funcall (q-get-index-fn q) old))
        (set-index-fn (q-set-index-fn q)))
    (funcall set-index-fn new index)
    (setf (aref items index) new)
    (if (funcall compare-fn new old)
        (heap-improve-key items index compare-fn set-index-fn)
        (heapify items index compare-fn set-index-fn)))
  q)

(defun queue-update (q item)
  "queue-update makes sure that item is sorted correctly in q. Call
queue-update after changing item in such a way that would affect its
priority. Returns the queue. Signals item-not-found-error if the item
wasn't found."
  (let ((compare-fn (q-compare-fn q))
        (items (q-items q))
        (index (funcall (q-get-index-fn q) item)))
    (unless index
      (error 'item-not-found-error))
    (if (and (> index 0)
             (funcall compare-fn item (aref items (heap-parent index))))
        (heap-improve-key items index compare-fn (q-set-index-fn q))
        (heapify items index compare-fn (q-set-index-fn q))))
  q)

(defun queue-delete (q item)
  "Delete item from the queue. Returns the queue. Signals
item-not-found-error if the item wasn't found."
  (let ((index (funcall (q-get-index-fn q) item)))
    (unless index
      (error 'item-not-found-error))
    (heap-delete (q-items q) index (q-compare-fn q) (q-set-index-fn q)))
  q)

;;;; The Heap Implementation of Priority Queues

(defun make-heap (&optional (size 100))
  (make-array size :fill-pointer 0 :adjustable t))

(declaim (inline heap-parent))
(defun heap-parent (i) (declare (fixnum i)) (floor (- i 1) 2))
(declaim (inline heap-left))
(defun heap-left (i) (declare (fixnum i)) (the fixnum (+ 1 i i)))
(declaim (inline heap-right))
(defun heap-right (i) (declare (fixnum i)) (the fixnum (+ 2 i i)))
(declaim (inline exchange))
(defun exchange (heap a b set-index-fn)
  (rotatef (aref heap a) (aref heap b))
  (funcall set-index-fn (aref heap a) a)
  (funcall set-index-fn (aref heap b) b))

(defun heapify (heap i compare-fn set-index-fn)
  "Assume that the children of i are heaps, but that heap[i] may be
worse than its children. If it is, move heap[i] down where it
belongs. [Page 130 CL&R 2nd ed.]."
  (declare (array heap) (fixnum i) (function compare-fn) (function set-index-fn))
  (loop with N = (length heap)
        with item = (aref heap i)
        for l = (heap-left i)
        for r = (heap-right i)
        for best = i
        do (when (and (< l N)
                      (funcall compare-fn (aref heap l) (aref heap i)))
             (setf best l))
           (when (and (< r N)
                      (funcall compare-fn (aref heap r) (aref heap best)))
             (setf best r))
           (cond ((/= best i)
                  (rotatef (aref heap i) (aref heap best))
                  (funcall set-index-fn (aref heap i) i)
                  (setf i best))
                 (t
                  (funcall set-index-fn item i)
                  (return)))))

(defun heap-extract-min (heap compare-fn set-index-fn)
  "Pop the best (lowest valued) item off the heap. [Page 139 CL&R 2nd ed.]."
  (declare (array heap) (function compare-fn) (function set-index-fn))
  (assert (> (length heap) 0) nil "heap underflow")
  (let ((min (aref heap 0)))
    (heap-delete heap 0 compare-fn set-index-fn)
    min))

(defun heap-improve-key (heap i compare-fn set-index-fn)
  (declare (array heap) (fixnum i) (function compare-fn) (function set-index-fn))
  (let ((item (aref heap i)))
    (loop while (and (> i 0)
                     (funcall compare-fn item (aref heap (heap-parent i))))
          do (exchange heap i (heap-parent i) set-index-fn)
             (setf i (heap-parent i)))))

(defun heap-insert (heap item compare-fn set-index-fn)
  "Put an item into a heap. [Page 140 CL&R 2nd ed.]."
  (declare (array heap) (function compare-fn) (function set-index-fn))
  (let ((i (vector-push-extend item heap)))
    (funcall set-index-fn item i)
    (heap-improve-key heap i compare-fn set-index-fn)))

(defun heap-delete (heap i compare-fn set-index-fn)
  "Delete the item at position i."
  (declare (array heap) (fixnum i) (function compare-fn) (function set-index-fn))
  (let ((L (length heap)))
    (assert (> L 0) nil "heap underflow")
    (assert (and (>= i 0)
                 (< i L)) nil "index out of range")
    (cond ((or (= L 1)
               (= i (1- L)))
           ;; There is only one item in the heap or the item we are
           ;; after is positioned last in the heap.
           (decf (fill-pointer heap)))
          (t
           (let ((last (aref heap (1- L))))
             (setf (aref heap i) last)
             (funcall set-index-fn last i)
             (decf (fill-pointer heap))
             (if (and (> i 0)
                      (funcall compare-fn last (aref heap (heap-parent i))))
                 (heap-improve-key heap i compare-fn set-index-fn)
                 (heapify heap i compare-fn set-index-fn)))))))
