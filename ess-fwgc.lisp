
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
        (rule if (state0=w-w-w-w (var x))
              then (state1=e-w-e-w (var x)) 1)
        ; return
        (rule if (state1=e-w-e-w (var x))
              then (state2=w-w-e-w (var x)) 1)
        
        
        ; take wolf
        (rule if (state2=w-w-e-w (var x))
              then (state3=e-e-e-w (var x)) 1)
        ; return with goat
        (rule if (state3=e-e-e-w (var x))
              then (state4=w-e-w-w (var x)) 1)
        ; take cabbage
        (rule if (state4=w-e-w-w (var x))
              then (state5=e-e-w-e (var x)) 1)
        
        
        ; take cabbage   
        (rule if (state2=w-w-e-w (var x))
              then (state3=e-w-e-e (var x)) 1)
        ; return with goat
        (rule if (state3=e-w-e-e (var x))
              then (state4=w-w-w-e (var x)) 1)
        ; take wolf
        (rule if (state4=w-w-w-e (var x))
              then (state5=e-e-w-e (var x)) 1)
        
        
        ; return
        (rule if (state5=e-e-w-e (var x))
              then (state6=w-e-w-e (var x)) 1)
        ; take goat
        (rule if (state6=w-e-w-e (var x))
              then (state7=e-e-e-e (var x)) 1)
        ; (rule if t
        ;       then (state7=e-e-e-e (var x)))
        ))

; (path path-1)

(setq *askables*
      (state0=w-w-w-w (var x))
      (state1=e-w-e-w (var x))
      (state2=w-w-e-w (var x))
      
      (state3=e-e-e-w (var x))
      (state4=w-e-w-w (var x))
      
      (state3=e-w-e-e (var x))
      (state4=w-w-w-e (var x))
      
      (state5=e-e-w-e (var x))
      (state6=w-e-w-e (var x))
      )