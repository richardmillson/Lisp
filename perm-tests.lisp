(defpackage :perm-tests
  (:use :common-lisp :lisp-unit :perm))

(in-package :perm-tests)

(define-test permute 
  (assert-equal '(2 3 1) (permute '(1 2 3))))
(define-test uniquep 
  (assert-false (uniquep '(1 2 3) '(2 3 1)))
  (assert-true (uniquep '(1 2 3) '(1 3 2))))
(define-test disjointp
  (assert-true (disjointp '((1)) '((2))))
  (assert-false (disjointp '((1 2)) '((2 1))))
  (assert-true (disjointp '((1)(2)) '((1 2))))
  (assert-false (disjointp '((1)(2)(3)) '((1 2)(3)))))

; (setq alpha '((1 4 5)(2 3)) 
;   beta '((1 4)(5)(3 2)))
; (disjointp alpha beta)
; (orbit 1 alpha)
; (sgn alpha)
; (even? alpha)
; (odd? alpha)


(disjointp '((1)(2)(3)) '((1 2)(3)))

; >(intersection '(1 2 3) '(4 5 6) :test (lambda (a b) (equal a (- b 1))))
; (3)
; >(intersection '(4 5 6) '(1 2 3) :test (lambda (a b) (equal a (- b 1))))
; nil
