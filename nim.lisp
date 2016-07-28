; find the minimax analysis of a game of nim
; nim is a game where players take turns dividing a pile of discrete size
; where the player that makes an even division loses
; assume that min starts
; hence an odd number of piles mean it is min's turn

(defun play (piles)
  (if (lostp piles) ; is losing state
      (if (oddp (length piles)) ; is min's move
          t ; max wins
          nil) ; is max's move, max loses
      (mapcar #'play (children piles)))
  )

; a losing state contains only piles of size 1 or 2
(defun lostp (piles)
  (not (dividable piles)))

; return the piles that can be further divided
(defun dividable (piles)
  (remove-if #'(lambda (pile) (or (= 1 pile) (= 2 pile))) piles))

; return all ways to split a natural number into two parts
; exclude even divisions
(defun split (num)
  (mapcar (lambda (x) (list x (- num x)))
          (range 1 (/ num 2))))

(defun range (start stop)
  (loop for n from start below stop
        collect n))

; (defun range (start stop step)
;   (loop for n from start below stop by step
;         collect n))

; given a state, generate its children
; ie all possible splits that can be made to the states piles
(defun children (piles)
  nil)
