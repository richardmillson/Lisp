(defun gcd (a b)
	(if (< a b)
		(gcd b a)
		(if (zerop b) 
			a
			(gcd b (mod a b)))))
