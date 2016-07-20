; trying to understand how recursive lambda functions work

(let ((fact ((lambda (x) (x x))
             (lambda (fact-gen)
               (lambda (n)
                 (if (zero? n)
                     1
                     (* n ((fact-gen fact-gen) (sub1 n)))))))))
  (display (fact 5)))
