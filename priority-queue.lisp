(in-package #:priority-queue)

(defstruct q
  (compare-fn nil :type function)
  (item-index nil :type hash-table)
  (elements nil :type array))

;;;; Basic Operations on Queues

(defun make-empty-queue (compare-fn &optional (equal-test-fn 'eql))
  "Return a new empty queue.

compare-fn is a function that takes two arguments and returns a true
value iff the first argument should be prioritized higher in the
queue. If the two items have equal priority, then compare-fn should
return nil.

equal-test-fn must be eq, eql, equal, or equalp. The default is
eql. Every item in the queue must be uniquely identifiable via this
function. Bad things will happen if two distinct items compare equally
using this function."
  (make-q :compare-fn compare-fn
          :item-index (make-hash-table :test equal-test-fn)
          :elements (make-heap)))

(defun queue-empty-p (q)
  "Are there no elements in the queue?"
  (= (length (q-elements q)) 0))

(defun queue-peek (q)
  "Return the element at the front of the queue."
  (aref (q-elements q) 0))

(defun queue-pop (q)
  "Remove the element from the front of the queue and return it."
  (heap-extract-min (q-elements q) (q-item-index q) (q-compare-fn q)))

(defun queue-insert (q item)
  "Insert the item by priority according to the compare
function. Returns the queue."
  (heap-insert (q-elements q) (q-item-index q) item (q-compare-fn q))
  q)

(defun queue-replace (q old new)
  "Replace old with new in the queue. This is analogous to deleting
the old item and inserting the new one, but it is almost always more
efficient (and never less efficient) because we can just fix up the
heap from the position of the swapped item. Returns the queue."
  (let* ((hash (q-item-index q))
         (compare-fn (q-compare-fn q))
         (elements (q-elements q))
         (index (gethash old hash)))
    (assert (remhash old hash) nil "Tried to replace an item, but it wasn't found in the hash.")
    (setf (gethash new hash) index)
    (setf (aref elements index) new)
    (if (funcall compare-fn new old)
        (heap-improve-key elements hash index compare-fn)
        (heapify elements hash index compare-fn)))
  q)

(defun queue-update (q item)
  "queue-update makes sure that item is sorted correctly in q. Call
queue-update after changing item in such a way that would affect its
priority. Returns the queue."
  (let ((compare-fn (q-compare-fn q))
        (elements (q-elements q))
        (hash (q-item-index q))
        (index (gethash item (q-item-index q))))
    (if (and (> index 0)
             (funcall compare-fn item (aref elements (heap-parent index))))
        (heap-improve-key elements hash index compare-fn)
        (heapify elements hash index compare-fn)))
  q)

(defun queue-delete (q item)
  "Delete item from the queue. Returns the queue."
  (let* ((hash (q-item-index q))
         (index (gethash item hash)))
    (assert index nil "index for item ~A not found in hash" item)
    (heap-delete (q-elements q) hash index (q-compare-fn q)))
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
(defun exchange (heap hash a b)
  (rotatef (gethash (aref heap a) hash) (gethash (aref heap b) hash))
  (rotatef (aref heap a) (aref heap b)))

(defun heapify (heap hash i compare-fn)
  "Assume that the children of i are heaps, but that heap[i] may be
worse than its children. If it is, move heap[i] down where it
belongs. [Page 130 CL&R 2nd ed.]."
  (declare (array heap) (hash-table hash) (fixnum i) (function compare-fn))
  (let ((l (heap-left i))
        (r (heap-right i))
        (N (length heap))
        (best 0))
    (declare (fixnum l r n best))
    (setf best (if (and (< l N)
                        (funcall compare-fn (aref heap l) (aref heap i)))
                   l
                   i))
    (when (and (< r N)
               (funcall compare-fn (aref heap r) (aref heap best)))
      (setf best r))
    (when (/= best i)
      (exchange heap hash i best)
      (heapify heap hash best compare-fn))))

(defun heap-extract-min (heap hash compare-fn)
  "Pop the best (lowest valued) item off the heap. [Page 139 CL&R 2nd ed.]."
  (declare (array heap) (hash-table hash) (function compare-fn))
  (assert (> (length heap) 0) nil "heap underflow")
  (let ((min (aref heap 0)))
    (heap-delete heap hash 0 compare-fn)
    min))

(defun heap-improve-key (heap hash i compare-fn)
  (declare (array heap) (hash-table hash) (fixnum i) (function compare-fn))
  (let ((item (aref heap i)))
    (loop while (and (> i 0)
                     (funcall compare-fn item (aref heap (heap-parent i))))
          do
             (exchange heap hash i (heap-parent i))
             (setf i (heap-parent i)))))

(defun heap-insert (heap hash item compare-fn)
  "Put an item into a heap. [Page 140 CL&R 2nd ed.]."
  (declare (array heap) (hash-table hash) (function compare-fn))
  ;; Note that ITEM is the value to be inserted, and KEY is a function
  ;; that extracts the numeric value from the item.
  (let ((index (vector-push-extend item heap)))
    (setf (gethash item hash) index)
    (heap-improve-key heap hash index compare-fn)))

(defun heap-delete (heap hash i compare-fn)
  "Delete the item at position i."
  (declare (array heap) (hash-table hash) (fixnum i) (function compare-fn))
  (let ((L (length heap)))
    (assert (> L 0) nil "heap underflow")
    (assert (and (>= i 0)
                 (< i L)) nil "index out of range")
    (assert (remhash (aref heap i) hash) nil "Tried to delete an item, but it wasn't found in the hash")
    (cond ((or (= L 1)
               (= i (1- L)))
           ;; There is only one item in the heap or the item we are
           ;; after is positioned last in the heap.
           (decf (fill-pointer heap)))
          (t
           (let ((last (aref heap (1- (length heap)))))
             (setf (aref heap i) last)
             (setf (gethash last hash) i))
           (decf (fill-pointer heap))
           (heapify heap hash i compare-fn)))))
