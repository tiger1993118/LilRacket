#|
These functions' defintions are given as exercies from the course CSC324H1, taught my David Liu
in University of Toronto. The concrete body of these functions are
implemented by myself
Link:
https://www.cs.toronto.edu/~david/courses/csc324_f14/learn.html
|#
#lang racket

#|----------------------------------------------------------------------
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
(define (subsets-k lst k) (subsets-recur lst empty k))

#|
(subsets-recur lst n curr k)
  lst: a list of distinct elements
  curr: current list assembled till now
  k: number of space left in the current list
|#
(define (subsets-recur lst curr k)
  (if (equal? k 0);Base case
      (list curr)
      (if (> (length lst) k);Checking options
          (append (vertical-move lst curr k)(horizontal-move lst curr k))
          (vertical-move lst curr k))));Only option, move vertically
#|
(horizontal-move lst curr k)
  Looping the lst horizontally and looking into the options
|#
(define (horizontal-move lst curr k) (subsets-recur (rest lst) curr k))

#|
(vertical-move lst curr k)
  Deeping into the lst vertically and picking numbers
|#
(define (vertical-move lst curr k)
  (subsets-recur (rest lst) (append curr (list (first lst))) (- k 1)))

(define test-lst1 '(2 3 4 5 6))
(define test-k1 4)

;(subsets-k test-lst1 test-k1)





#|----------------------------------------------------------------------
(my-remove-duplicates lst)
  lst: a list

  Output a new list containing the distinct elements in lst.
  Order matters: only the *first* occurrence of any duplicate remains.

> (my-remove-duplicates '(1 2 3 1 3 1))
'(1 2 3) ; the *last three elements* are removed.
> (my-remove-duplicates '(4 3 2 1))
'(4 3 2 1)

NOTE: name changed to *my-*remove-duplicates.
There is already a function called remove-duplicates provided by Racket;
of course, you are NOT allowed to use this function in your code.
|#
(define (my-remove-duplicates lst) (foldl append-unique empty lst))

#|
(append-unique x lst)
Append x to lst, if it isn't in lst
|#
(define (append-unique x lst)
  (if (unique x lst) (append lst (list x)) lst))

#|
(unique x lst)
Return True if x isn't in lst, False elsewise
|#
(define (unique x lst)
  (if (empty? lst) #t
      (and (not (equal? (first lst) x)) (unique x (rest lst))))); lazy evaluation

;(my-remove-duplicates '(1 2 3 1 3 1))

#| 
(apply-functions list-of-functions arg)
  list-of-functions: a list of unary functions
  arg: a valid argument to each function in list-of-functions

  Return a list of the results of applying each function to arg.

> (apply-functions (list (lambda (x) (+ x x)) (lambda (x) (+ 1 3))) 10)
'(20 4)
|#
(define (apply-functions list-of-functions arg)
  (map (lambda (f) (f arg)) list-of-functions))

(apply-functions (list (lambda (x) (+ x x)) (lambda (x) (+ 1 3))) 10)

#|
(test-f f inputs outputs)
  f: a unary function
  inputs: a list of valid arguments to f
  outputs: a list of the same length as inputs

  For each corresponding (input, output) pair from inputs and outputs,
  evaluate f at each input; a SUCCESS occurs when the result is
  "equal?" to output.

  Return the number of SUCCESSes.

> (test-f (lambda (x) (+ x 1)) '(1 2 3) '(3 3 10))
1 ; the only SUCCESS is the pair of second elements

HINT: look up the Racket documentation of map; it can take two lists!
|#
(define (test-f f inputs outputs) (void))