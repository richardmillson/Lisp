; string matching

(search "we" "If we can't be free we can at least be cheap")

; naive method trying every possible shift
; returns the position of the first match, if any
(defun naive (match string)  
  (cond ((equal string "") nil)
        (char= (char match 0) (char string 0))
        (everything-before (subseq match 1) (subseq string 1))))

(defun rabin-karp (match string)
  )

(defun boyer-moore (match string)
  )

(defun knuth-morris-pratt (match string)
  )
