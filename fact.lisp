; TODO factorial function should not be user accessible, replace defun with flet or labels

(defun fact (n)
	(defun factorial (n acc)
		(if (zerop n)
			acc
			(factorial (- n 1) (* n acc))))
	(factorial n 1))
