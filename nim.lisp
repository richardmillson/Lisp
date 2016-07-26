; find the minimax analysis of a game of nim
; nim is a game where players take turns dividing a pile of discrete size
; where the player that makes an even division loses
; assume that min starts
; hence an odd number of piles mean it is min's turn

(defun play (piles)
  (when (lostp piles) ; is losing state
    (if (oddp (length piles)) ; is min's move
        t ; max wins
        nil)) ; is max's move, max loses
  )

; a losing state contains only piles of size 1 or 2
(defun lostp (piles)
  (not (dividable piles)))

; return the piles that can be further divided
(defun dividable (piles)
  (remove-if #'(lambda (pile) (or (= 1 pile) (= 2 pile))) piles))

; return all ways to split a natural number
; generate all possible pairs
; exclude even divisions
(defun all-splits (num)
  (let ((left 1) (splits nil))
    (loop while (floor (< left (+ 1 (/ num 2)))) do
          (setf splits (append splits (list (list left (- num left)))))
          (setf left (+ 1 left)))
    splits))
; (enqueue-list (mapcar #'(lambda (x) (cons x front)) (set)))

; (defun ttest ()
;   (let ((x 5))
;   (loop while (< x 7) do
;         (print x)
;         (setf x (+ 1 x))
;         finally x)
;   x))

; (defun ttest ()
;   (let ((x 5))
;   (print x)))
