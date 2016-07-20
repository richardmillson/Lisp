; return n factorial ie n!

(defun fact (n)
  ((lambda (n acc)
     (if (zerop n)
         acc
         (lambda (- n 1) (* n acc))))
   n 1))
