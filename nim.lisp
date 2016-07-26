; find the minimax analysis of a game of nim
; nim is a game where players take turns dividing a pile of discrete size
; where the player that makes an even division loses
; assume that min starts
; hence an odd number of piles mean it is min's turn

(defun play (piles)
  (when (lostp piles) ; is losing state
    (if (oddp (length piles)) ; is min's move
        1 ; min loses, return 1 to delineate win for max
        0)) ; is max's move, max loses, return 0 to delineate loss for max
  )

; a losing state contains only piles of size 1 or 2
(defun lostp (piles)
  (not (dividable piles)))

; return the piles that can be further divided
(defun dividable (piles)
  (remove-if #'(lambda (pile) (or (= 1 pile) (= 2 pile))) piles))
