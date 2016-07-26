; implement permutations

; return the sign of a permutation
; sgn(alpha) = (-1)^{n-r}
; alpha is a permutation
; n is the cardinality of the symmetric group that alpha belongs to ie S_n
; r is the number of disjoint cycles in alpha's complete factorization
(defun sgn (alpha)
  nil)

; (defun evenp (alpha)
;   (= 1 (sgn alpha)))

; (defun oddp (alpha)
;   (= -1 (sgn alpha)))

; determine if 
(defun disjointp (alpha beta)
  nil)

; return the orbit of element i in alpha
; orbit_{alpha}(i) = {\alpha^k(i) : k >= 0} subset X_n
(defun orbit (i alpha)
  nil)

(defun factorization)

; X_n = {1, 2, ..., n}
; move(alpha) = {i in X_n : alpha(i) != i}
(defun move (alpha)
  nil)

; fix(alpha) = {i in X_n : alpha(i) = i}
(defun fix (alpha)
  nil)

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
