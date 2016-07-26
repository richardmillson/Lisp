; find the minimax analysis of a game of nim
; nim is a game where players take turns dividing a pile of discrete size
; where the player that makes an even division loses
; assume that min starts
; hence an odd number of piles mean it is min's turn

(defun play (piles)
  (when (lostp piles) ; is losing state
    (if (oddp (length piles)) ; is min's move
        1
        0)) ; is max's move
  )

; a losing state is a pile containing one or more 2's and some number of 1's
(defun lostp (piles)
  (remove-if #'(lambda (pile) (or (= 1 pile) (= 2 pile))) piles))
