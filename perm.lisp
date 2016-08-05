(defpackage :perm
  (:use :common-lisp)
  (:export 
    #:sgn #:even? #:odd? 
    #:disjointp #:uniquep #:permute #:all-perms
    #:fix #:move))

(in-package :perm)

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
  nil)

; return the orbit of element i in alpha
; orbit_{alpha}(i) = {\alpha^k(i) : k >= 0} subset X_n
; alpha is in disjoint cycle representation
(defun orbit (i alpha)
  (find-if #'(lambda (cycle) (member i cycle)) alpha))

(defun factorization ()
  nil)

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

(defun to-graph (alpha)
  nil)

; >(to-complete '(4 3 2 5 1))
; ((1 4 5)(2 3))
(defun to-complete (alpha)
  (labels ((
            ))
          )
  (remove-duplicates  :test-not #:uniquep))

; given a permutation alpha in graph representation
; and an element i of alpha
; construct the oribit of i in alpha
; uses zero-based arrays
; >(orbit-in-graph 1 '(2 3 1))
; (2 3 1)
(defun orbit-in-graph (i alpha)
  (let ((j i))
    (loop until (= j i) do
          (setq j (mult j alpha))
          (print j)
          collect j)))

; >(mult 1 '(2 3 1))
; 2
(defun mult (pos perm-graph)
  (nth (- pos 1) perm-graph))
