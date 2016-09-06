
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
        (rule if (state0 (var x) '(w w w w))
              then (state1 (var x) '(e w e w)) 1)
        ; return
        (rule if (state1 (var x) '(e w e w))
              then (state2 (var x) '(w w e w)) 1)
        
        
        ; ; take wolf
        ; (rule if (state2 (var x) '(w w e w))
        ;       then (state3 (var x) '(e e e w)) 1)
        ; return with goat
        (rule if (state3 (var x) '(e e e w))
              then (state4 (var x) '(w e w w)) 1)
        ; take cabbage
        (rule if (state4 (var x) '(w e w w))
              then (state5 (var x) '(e e w e)) 1)
        
        
        ; ; take cabbage   
        ; (rule if (state2 (var x) '(w w e w))
        ;       then (state3 (var x) '(e w e e)) 1)
        ; return with goat
        (rule if (state3 (var x) '(e w e e))
              then (state4 (var x) '(w w w e)) 1)
        ; take wolf
        (rule if (state4 (var x) '(w w w e))
              then (state5 (var x) '(e e w e)) 1)
        
        
        ; ; return
        ; (rule if (state5 (var x) '(e e w e))
        ;       then (state6 (var x) '(w e w e)) 1)
        ; take goat
        (rule if (state6 (var x) '(w e w e))
              then (state7 (var x) '(e e e e)) 1)
        
        (rule if 
              then (state7 (var x) '(e e e e)))
        (rule if (and (state0 (var x) '(w w w w))
                      (state1 (var x) '(e w e w))
                      
                      (state2 (var x) '(w w e w))
                      (state3 (var x) '(e e e w))                      
                      (state4 (var x) '(w e w w))
                      
                      (state5 (var x) '(e e w e))
                      (state6 (var x) '(w e w e))
                      (state7 (var x) '(e e e e)))
              then (path (var x) a))
        ))

; '((w w w w) (e w e w) (w w e w) (e e e w) (w e w w) (e e w e) (w e w e) (e e e e)))
      
; (state7 path1 (var x))
; (state3 path1 (var x))
; (path path-1 (var x))

(setq *askables*
      '(
      (state0 (var x) (var y))
      (state1 (var x) (var y))
      (state2 (var x) (var y))
      (state3 (var x) (var y))
      (state4 (var x) (var y))
      (state5 (var x) (var y))
      (state6 (var x) (var y))
      (state7 (var x) (var y))
      ))
