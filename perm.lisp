; (defpackage :perm
;   (:use :common-lisp)
;   (:export 
;     #:sgn #:even? #:odd? 
;     #:disjointp #:uniquep #:permute #:all-perms
;     #:fix #:move))

; (in-package :perm)

; permutations

; given a permutation in disjoint cycle representation
; return the sign of a permutation
; sgn(alpha) = (-1)^{n-r}
; alpha is a permutation
; n is the cardinality of the symmetric group that alpha belongs to ie S_n
; r is the number of disjoint cycles in alpha's complete factorization
(defun sgn (alpha)
  (let ((n (length (flatten alpha)))
        (r (length alpha)))
    (power -1 (mod (- n r) 2))))

(defun flatten (llist)
  (cond ((null llist) nil)
        ((atom llist) (list llist))
        (t (append (flatten (car llist)) (flatten (cdr llist))))))

; raise base b to the exponent n ie b^n
(defun power (b n)
  (labels ((exponent (b n acc)
                     (if (zerop n)
                         acc
                         (exponent b (- n 1) (* b acc)))))
          (exponent b n 1)))

; parity
(defun even? (alpha)
  (= 1 (sgn alpha)))
(defun odd? (alpha)
  (= -1 (sgn alpha)))

; determine if two permutations alpha and beta are disjoint
(defun disjointp (alpha beta)
  (null (intersection alpha beta :test-not #'uniquep)))

;(defun disjointp (alpha beta)
;  (intersection (apply alpha beta :test #'uniquep))

; determine if two cycles a and b are unique up to order
(defun uniquep (a b)
  (notany #'(lambda (b-perm) (equal a b-perm)) (all-perms b)))

; permute
; >(permute '(1 2 3))
; (2 3 1)
(defun permute (cycle)
  (append (cdr cycle) (list (car cycle))))

; return a list of all permutations of a cycle
; >(all-perms '(1 2 3))
; ((2 3 1) (3 1 2) (1 2 3))
(defun all-perms (cycle)
  (labels ((next-perm (next-cycle)
                      (if (equal next-cycle cycle)
                          (list next-cycle)
                          (append (list next-cycle) (next-perm (permute next-cycle))))))
          (next-perm (permute cycle))))

; >(prod '((1 2)(3)) '((1)(2 3)))
; 
(defun prod (alpha beta)
  (loop for i from 1 to (apply #'+ (mapcar #'length alpha))
        collect (apply-c i alpha)))

; return the orbit of element i in alpha
; orbit_{alpha}(i) = {\alpha^k(i) : k >= 0} subset X_n
; alpha is in disjoint cycle representation
; >(orbit 1 '((1 4 5)(2 3)))
; (1 4 5)
(defun orbit-c (i alpha)
  (find-if #'(lambda (cycle) (member i cycle)) alpha))

; X_n = {1, 2, ..., n}
; move(alpha) = {i in X_n : alpha(i) != i}
(defun move (alpha)
  (remove-if #'(lambda (cycle) (= (length cycle) 1)) alpha))

; return the elements of X_n that are fixed by alpha
; fix(alpha) = {i in X_n : alpha(i) = i}
(defun fix (alpha)
  (remove-if-not #'(lambda (cycle) (= (length cycle) 1)) alpha))

; given a permutation alpha
; graph representation of alpha
; (1 2 3 4 5
;  4 3 2 5 1)
; also simply
; (4 3 2 5 1)
; disjoint cycle representation of alpha
; (1 4 5)(2 3)


; >(to-graph '((1 4 5)(2 3)))
; (4 3 2 5 1)
(defun to-graph (alpha)
  (loop for i from 1 to (apply #'+ (mapcar #'length alpha))
        collect (apply-c i alpha)))

; find the factorization
; >(to-complete '(4 3 2 5 1))
; ((1 4 5)(2 3))
(defun to-complete (alpha)
  (all-orbits alpha))

; given a permutation alpha in graph representation
; return a list of the orbit of every element
; remove duplicate cycles
; >(all-orbits '(2 3 1))
(defun all-orbits (alpha)
  (let ((all-orbits nil))
    (loop for i from 1 to (length alpha) do
          (setq all-orbits (cons (orbit-g i alpha) all-orbits)))
    (remove-duplicates all-orbits :test-not #'uniquep)))

; given a permutation alpha in graph representation
; and an element i of alpha
; construct the oribit of i in alpha
; uses one-based arrays
; until j appears in the constructed orbit
; >(orbit-g 1 '(2 3 1))
; (2 3 1)
(defun orbit-g (i alpha)
  (let ((j i) (i-orbit nil))
    (loop until (member i i-orbit) do
          (setq j (apply-g j alpha))
          (setq i-orbit (append i-orbit (list j))))
    i-orbit))

; apply a permutation in graph representation to an element
; converts from one-based user input to zero-based arrays used by lisp
; >(apply-g 1 '(2 3 1))
; 2
(defun apply-g (pos perm-graph)
  (nth (- pos 1) perm-graph))

; apply a permutation alpha in complete cycle representation 
; to an element i of alpha
; >(apply-c 1 '((1 4 5)(2 3)))
; 4
(defun apply-c (i alpha)  
  (let ((i-orbit (orbit-c i alpha)))
    (nth (mod (+ 1 (position i i-orbit)) (length i-orbit)) i-orbit)))

; determine the order of element i in alpha
; >(order 1 '(2 3 1))
; 2
(defun order-g (i alpha)
  (length (orbit-g i alpha)))

(defun order-c (i alpha)
  (length (orbit-c i alpha)))
