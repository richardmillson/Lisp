; return n factorial ie n!

(defun fact (n)
  (flet ((factorial (n acc)
                    (if (zerop n)
                        acc
                        (factorial (- n 1) (* n acc))))))
  (factorial n 1))

; (factorial 3 10)
