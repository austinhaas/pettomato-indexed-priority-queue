This is a binary heap based priority queue implementation. In addition
to the properties and operations common to all binary heaps (see
http://en.wikipedia.org/wiki/Binary_heap), this implementation also
supports efficient find, update, replace, and delete operations.

Please see the docstrings of the exported symbols for more
information, especially make-empty-queue for an explanation of how
item lookups are achieved.

--------------------------------
Acknowledgments

This code was originally based off of Peter Norvig's implementation
found here:

http://aima.cs.berkeley.edu/lisp/utilities/queue.lisp

Norvig's code was built on top of the heap algorthms in "Introduction
to Algorithms" by Corman, Leiserson, and Rivest. I trivially updated
some of that code to match the 2nd edition of the book by Corman,
Leiserson, Rivest, and Stein. Specifically, I updated the referenced
page numbers in the docstrings and implemented the Heap-Increase-Key
algorithm (called improve-key in my code), which was not in the first
edition (and is very useful for the new operations we added).
