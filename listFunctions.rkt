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

#|
(subsets-recur lst n curr k)
  lst: a list of distinct elements
  n: an int indicates number of options in the lst left
  curr: current list assembled till now
  k: number of space left in the current list

|#
(define (subsets-recur lst n curr k)
  (if (equal? k 0)
      (list curr)
      (if (> n k)
          (append (subsets-recur (rest lst) (- n 1) (append curr (list (first lst))) (- k 1))
           (subsets-recur (rest lst) (- n 1) curr k))
          (subsets-recur (rest lst) (- n 1) (append curr (list (first lst))) (- k 1)))))

(define test-lst '(2 3 4 5 6))
(define test-n (length test-lst))
(define test-curr empty)
(define test-k 2)
(subsets-recur test-lst test-n test-curr test-k)
