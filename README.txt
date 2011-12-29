This is a priority queue implementation based off of Peter Norvig's
implementation found here:

http://aima.cs.berkeley.edu/lisp/utilities/queue.lisp

The most significant change to the original code that we've made is
that we've added a hash-table to the queue structure which maps items
to their index within the heap. This allows us to efficiently
implement operations for queue-replace, queue-update, and
queue-delete, at the expense of more memory, and more overhead in the
original operations.
