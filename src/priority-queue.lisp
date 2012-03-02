(in-package #:pettomato-indexed-priority-queue)

(defconstant +not-in-queue+ -1
  "A sentinel value associated with an item that is not in the queue.")

(define-condition empty-queue-error (error)
  ()
  (:documentation "Signaled when an operation depends on a non-empty queue."))

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
        for best-item = item
        do (when (< l N)
             (let ((left-item (aref heap l)))
               (when (funcall compare-fn left-item item)
                 (setf best l
                       best-item left-item))))
           (when (< r N)
             (let ((right-item (aref heap r)))
               (when (funcall compare-fn right-item best-item)
                 (setf best r
                       best-item right-item))))
           (cond ((/= best i)
                  (setf (aref heap i) best-item)
                  (funcall set-index-fn best-item i)
                  (setf i best))
                 (t
                  (setf (aref heap i) item)
                  (funcall set-index-fn item i)
                  (return)))))

(defun improve-key (heap i compare-fn set-index-fn)
  "The item at i may be better than its parent. Promote the item until
it is in the correct position."
  (declare (array heap) (fixnum i) (function compare-fn) (function set-index-fn))
  (let ((item (aref heap i)))
    (loop for index = i then parent-index
          while (> index 0)
          for parent-index = (parent index)
          for parent = (aref heap parent-index)
          while (funcall compare-fn item parent)
          do
             (setf (aref heap index) parent)
             (funcall set-index-fn parent index)
          finally
             (setf (aref heap index) item)
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

set-index-fn and get-index-fn are a pair. Some queue operations (e.g.,
queue-delete) require that the index for an item can be
retrieved. These functions allow the user to control how that mapping
is maintained.

set-index-fn is a function that takes two arguments: the first is an
item in the queue and the second is the current index position of that
item in the heap. This function should do something to record the
association between the item and the index. Each item's index may
change many times while it is stored in the queue; this function will
be called each time.

get-index-fn is a function that takes an item and returns the heap
index that was associated with it via set-index-fn. get-index-fn must
return -1 if the item is not in the queue. The priority queue will
call set-index-fn with -1 when an item is being removed from the
queue, but that obviously only works for items it has seen before. If
there is a chance client code will try to call one of the operations
that relies on get-index-fn with items that have never been in the
queue, you must make sure that get-index-fn returns -1. See example
below.

A trivial way to implement set-index-fn / get-index-fn would be like
this:

 (let* ((hash (make-hash-table))
        (set-index-fn (lambda (item index))
                         (setf (gethash item hash) index))
        (get-index-fn (lamda (item)
                         (gethash item hash -1))))
   (make-empty-queue #'< set-index-fn get-index-fn))

Notice that get-index-fn returns -1 for items that aren't in the
hash. It might also be a good idea to change set-index-fn to something
like this:

 (lambda (item index)
    (if (= index -1)
        (remhash item hash)
        (setf (gethash item hash) index)))

Of course, this code all depends on each item being distinguishable
via 'eql, since that is the default test for a hash-table.

The motivation for handling the index maintenance this way is that it
is straightforward and flexible. Any item can be stored in the queue
and the user has control over how they are identified and where this
storage takes place. In many cases, it may make sense to store the
index directly as a property of the item being stored. In other cases,
we may want a looser mapping that only uses some attributes of the
item being stored, so that we can lookup similar items (e.g., if we
were using the priority queue in a search algorithm we could check to
see if the queue already contains a node with the same state as a
newly discovered node).
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
heap from the position of the swapped item. Obviously, this only makes
sense if you expect the two items to be located very close to each
other in the queue (perhaps even in the same location, such as if the
priority is the same, but you need to swap the item being stored
because of other data it contains). Returns the queue. Signals
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
empty-queue-error if the queue is empty. Signals item-not-found-error
if the item wasn't found."
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
                    (heapify heap index compare-fn set-index-fn)))))))
    (funcall set-index-fn item +not-in-queue+))
  q)

(defun queue-find (q item)
  "Checks if item is in the queue (using the supplied
get-index-fn). If it is, returns two values: the actual item from the
queue (which could be different than the supplied item, since the
client can setup the mapping however they like) and the index. If it
isn't, returns nil and -1."
  (let ((index (funcall (q-get-index-fn q) item)))
    (if (= index +not-in-queue+)
        (values nil -1)
        (values (aref (q-items q) index) index))))
