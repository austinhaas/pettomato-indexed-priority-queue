(in-package #:priority-queue)

(defconstant +not-in-queue+ -1)

(define-condition empty-queue-error (error)
  ()
  (:documentation "Signaled when queue-peek or queue-pop is called on an empty queue."))

(define-condition item-not-found-error (error)
  ()
  (:documentation "Signaled when an operation depends on an item that was not found in the queue."))

(declaim (inline parent))
(defun parent (i) (declare (fixnum i)) (floor (- i 1) 2))
(declaim (inline left))
(defun left (i) (declare (fixnum i)) (the fixnum (+ 1 i i)))
(declaim (inline right))
(defun right (i) (declare (fixnum i)) (the fixnum (+ 2 i i)))

(defun heapify (heap i compare-fn set-index-fn)
  "Assume that the children of i are heaps, but that heap[i] may be
worse than its children. If it is, move heap[i] down where it
belongs. [Page 130 CL&R 2nd ed.]."
  (declare (array heap) (fixnum i) (function compare-fn) (function set-index-fn))
  (loop with N = (length heap)
        with item = (aref heap i)
        for l = (left i)
        for r = (right i)
        for best = i
        do (when (and (< l N) (funcall compare-fn (aref heap l) (aref heap i)))
             (setf best l))
           (when (and (< r N) (funcall compare-fn (aref heap r) (aref heap best)))
             (setf best r))
           (cond ((/= best i)
                  (rotatef (aref heap i) (aref heap best))
                  (funcall set-index-fn (aref heap i) i)
                  (setf i best))
                 (t
                  (funcall set-index-fn item i)
                  (return)))))

(defun improve-key (heap i compare-fn set-index-fn)
  (declare (array heap) (fixnum i) (function compare-fn) (function set-index-fn))
  (let ((item (aref heap i)))
    (loop for index = i then parent-index
          while (> index 0)
          for parent-index = (parent index)
          for parent = (aref heap parent-index)
          while (funcall compare-fn item parent)
          do
             (funcall set-index-fn parent index)
             (rotatef (aref heap index) (aref heap parent-index))
          finally
             (funcall set-index-fn item index))))

(defstruct q
  (compare-fn nil :type function)
  (set-index-fn nil :type function)
  (get-index-fn nil :type function)
  (items nil :type array))

(defun make-empty-queue (compare-fn set-index-fn get-index-fn &key (size 100))
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
          :items (make-array size :fill-pointer 0 :adjustable t)))

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
it. Signals empty-queue-error if the queue is empty. [Page 139 CL&R
2nd ed.]."
  (if (queue-empty-p q)
      (error 'empty-queue-error)
      (let* ((heap (q-items q))
             (set-index-fn (q-set-index-fn q))
             (min (aref heap 0)))
        (when (> (length heap) 1)
          (setf (aref heap 0) (aref heap (1- (length heap))))
          (funcall set-index-fn (aref heap 0) 0))
        (decf (fill-pointer heap))
        (heapify heap 0 (q-compare-fn q) set-index-fn)
        (funcall set-index-fn min +not-in-queue+)
        min)))

(defun queue-insert (q item)
  "Insert the item by priority according to the compare
function. Returns the queue. [Page 140 CL&R 2nd ed.]."
  (let ((heap (q-items q))
        (compare-fn (q-compare-fn q))
        (set-index-fn (q-set-index-fn q)))
    (let ((i (vector-push-extend item heap)))
      (funcall set-index-fn item i)
      (improve-key heap i compare-fn set-index-fn)))
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
    (declare (fixnum index))
    (when (or (null index)
              (= index +not-in-queue+))
      (error 'item-not-found-error))
    (funcall set-index-fn old +not-in-queue+)
    (funcall set-index-fn new index)
    (setf (aref items index) new)
    (if (funcall compare-fn new old)
        (improve-key items index compare-fn set-index-fn)
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
    (declare (fixnum index))
    (when (or (null index)
              (= index +not-in-queue+))
      (error 'item-not-found-error))
    (if (and (> index 0)
             (funcall compare-fn item (aref items (parent index))))
        (improve-key items index compare-fn (q-set-index-fn q))
        (heapify items index compare-fn (q-set-index-fn q))))
  q)

(defun queue-delete (q item)
  "Delete item from the queue. Returns the queue. Signals
item-not-found-error if the item wasn't found."
  (when (queue-empty-p q)
    (error 'empty-queue-error))
  (let ((heap (q-items q))
        (compare-fn (q-compare-fn q))
        (get-index-fn (q-get-index-fn q))
        (set-index-fn (q-set-index-fn q)))
   (let ((index (funcall get-index-fn item)))
     (declare (fixnum index))
     (when (or (null index)
               (= index +not-in-queue+))
       (error 'item-not-found-error))
     (let ((L (length heap)))
       (cond ((or (= L 1)
                  (= index (1- L)))
              ;; There is only one item in the heap or the item we are
              ;; after is positioned last in the heap.
              (decf (fill-pointer heap)))
             (t
              (let ((last (aref heap (1- L))))
                (setf (aref heap index) last)
                (funcall set-index-fn last index)
                (decf (fill-pointer heap))
                (if (and (> index 0)
                         (funcall compare-fn last (aref heap (parent index))))
                    (improve-key heap index compare-fn set-index-fn)
                    (heapify heap index compare-fn set-index-fn))))))))
  (funcall (q-set-index-fn q) item +not-in-queue+)
  q)
