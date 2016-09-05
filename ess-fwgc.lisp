
;;; top level interpreter loop
; (defun lisp-shell ()
;   (prin1  'name-of-file-containg-knowledge-base-as-string> )
;   (let ((file-name (read)))
;     (terpri)
;     (cond ((equal file-name 'quit) 'bye)
;           (t (load file-name)
;              (terpri)
;              (lisp-shell)))))

(setq *assertions* 
      '(
        ; take goat
        (rule if (state0 (var x0) '(w w w w))
              then (state1 (var x1) '(e w e w)) 1)
        ; return
        (rule if (state1 (var x1) '(e w e w))
              then (state2 (var x2) '(w w e w)) 1)
        
        
        ; take wolf
        (rule if (state2 (var x2) '(w w e w))
              then (state3 (var x3) '(e e e w)) 1)
        ; return with goat
        (rule if (state3 (var x3) '(e e e w))
              then (state4 (var x4) '(w e w w)) 1)
        ; take cabbage
        (rule if (state4 (var x4) '(w e w w))
              then (state5 (var x5) '(e e w e)) 1)
        
        
        ; take cabbage   
        (rule if (state2 (var x2) '(w w e w))
              then (state3 (var x3) '(e w e e)) 1)
        ; return with goat
        (rule if (state3 (var x3) '(e w e e))
              then (state4 (var x4) '(w w w e)) 1)
        ; take wolf
        (rule if (state4 (var x4) '(w w w e))
              then (state5 (var x5) '(e e w e)) 1)
        
        
        ; return
        (rule if (state5 (var x5) '(e e w e))
              then (state6 (var x6) '(w e w e)) 1)
        ; take goat
        (rule if (state6 (var x6) '(w e w e))
              then (state7 (var x7) '(e e e e)) 1)
        (rule if t
              then (state7 (var x) '(e e e e)))        
        ))

; (state1 path-1 (var x))

; (rule 
;   if (and (size (var x) small ) (woody (var x)))
;   then (bush (var x)) .9)
; (rule 
;   if (and (tree (var x)) (evergreen (var x))(color (var x) blue))
;   then (kind (var x) spruce) .8)
; (rule 
;   if (and (tree (var x)) (evergreen (var x))(color (var x) green))
;   then (kind (var x) pine) .9)
; (rule
;   if (and (tree (var x)) (deciduous (var x)) (bears (var x) fruit))
;   then (fruit-tree (var x)) 1)
; (rule 
;   if (and (fruit-tree (var x)) (color fruit red) (taste fruit sweet))
;   then (kind (var x) apple-tree) .9)
; (rule 
;   if (and (fruit-tree (var x)) (color fruit yellow) (taste fruit sour))
;   then (kind (var x) lemon-tree) .8)
; (rule 
;   if (and (bush (var x)) (flowering (var x)) (thorny (var x)))
;   then (rose (var x)) 1)
; (rule
;   if (and (rose (var x)) (color (var x) red))
;   then (kind (var x) american-beauty) 1)))


; (path path-1)

; (setq *askables*
;       nil)
       
;       '(
;         (branch (var x))
;         ))

; 		  (size (var x) (var y))
;         (woody (var x))
;         (soft (var x))
;         (color (var x) (var y))
;         (evergreen (var x))
;         (thorny (var x))
;         (deciduous (var x))
;         (bears (var x) (var y))
;         (taste (var x) (var y))
;         (flowering (var x))))