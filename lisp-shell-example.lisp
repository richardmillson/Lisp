
; example for lisp-shell

; execution

; > (lisp-shell)
; lisp-shell>(kind tree-1 (var x))
; (size tree-1 tall) >y
; (woody tree-1) >y
; (evergreen tree-1) >y
; (color tree-1 blue) >n
; (color tree-1 green) >y
; (deciduous tree-1) >n
; (size tree-1 small) >n
; (kind tree-1 pine) cf 0.81
; lisp-shell>(kind bush-2 (var x))
; (size bush-2 tall) >n
; (size bush-2 small) >y
; (woody bush-2) >y
; (flowering bush-2) >y
; (thorny bush-2) >y
; (color bush-2 red) >y
; (kind bush-2 american-beauty) cf 0.9
; lisp-shell>(kind tree-3 (var x))
; (size tree-3 tall) >y
; (woody tree-3) >y
; (evergreen tree-3) >n
; (deciduous tree-3) >y
; (bears tree-3 fruit) >y
; (color fruit red) >n
; (color fruit yellow) >y
; (taste fruit sour) >y
; (size tree-3 small) >n
; (kind tree-3 lemon-tree) cf 0.72
; lisp-shell>quit
; bye
; ?

(setq *assertions* 
      '((rule
          if (and (size (var x) tall)
                  (woody (var x)))
          then (tree (var x)) .9)
        (rule
          if (and (size (var x) small)
                  (woody (var x)))
          then (bush (var x)) .9)
        (rule
          if (and (tree (var x)) (evergreen (var x))
                  (color (var x) blue))
          then (kind (var x) spruce) .8)
        (rule
          if (and (tree (var x)) (evergreen (var x))
                  (color (var x) green))
          then (kind (var x) pine) .9)
        (rule
          if (and (tree (var x)) (deciduous (var x))
                  (bears (var x) fruit))
          then (fruit-tree (var x)) 1)
        (rule
          if (and (fruit-tree (var x))
                  (color fruit red)
                  (taste fruit sweet))
          then (kind (var x) apple-tree) .9)
        (rule
          if (and (fruit-tree (var x))
                  (color fruit yellow)
                  (taste fruit sour))
          then (kind (var x) lemon-tree) .8)
        (rule
          if (and (bush (var x))
                  (flowering (var x))
                  (thorny (var x)))
          then (rose (var x)) 1)
        (rule
          if (and (rose (var x)) (color (var x) red))
          then (kind (var x) american-beauty) 1)))

(setq *askables* 
      '((size (var x) (var y))
        (woody (var x))
        (soft (var x))
        (color (var x) (var y))
        (evergreen (var x))
        (thorny (var x))
        (deciduous (var x))
        (bears (var x) (var y))
        (taste (var x) (var y))
        (flowering (var x))))
