; implement permutations

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

; determine if two cycles alpha and beta are disjoint
; TODO cycle representation is not unique, need to detect this
(defun disjoint? (alpha beta)
  (intersection alpha beta))

; return the orbit of element i in alpha
; orbit_{alpha}(i) = {\alpha^k(i) : k >= 0} subset X_n
(defun orbit (i alpha)
  (remove-if-not #'(lambda (cycle) (member i cycle)) alpha))

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

(defun to-complete (alpha)
  nil)



; tests
; (setq alpha '((1 4 5)(2 3)))
; (sgn alpha)
; (even? alpha)
; (odd? alpha)
