#|
These functions' defintions are given as exercies from the course CSC324H1, taught my David Liu
in University of Toronto. The concrete body of these functions are
implemented by myself
Link:
https://www.cs.toronto.edu/~david/courses/csc324_f14/learn.html
|#
#lang racket


#|
(subsets-k lst k)
  lst: a list of distinct elements
  k: a natural number (including 0)

  Returns a list containing all subsets of lst of size k.
  Note that every set has a subset of size 0 (the empty set).

  Order does NOT matter within the subsets, nor does it matter in what
  order the subsets appear in the output.

> (subsets-k '(1 2 3) 2)
'((1 2) (1 3) (2 3))
> (subsets-k '(1 2) 5)
'()
|#
(define (subsets-k lst k) (void))
