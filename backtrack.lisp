; sl state list, nsl new state list, de dead ends, cs current state
; needs is-goal predicate which takes states and is nil if state is not a goal

; uses stack

(defun backtrack (is-goal)
  (let ((sl start) (nsl start) (de nil) (cs start))
    (loop while nsl do ; while there are still untested states ie possible solutions
          (cond ((is-goal cs) cs)
                ((not (has-children cs) ))
                (t (push-list (children cs) nsl)
                   (setf cs (top nsl))
                   (push cs sl))))))

(defun push-list (llist)
  (unless (null llist)
    (push (car llist))
    (push-list (cdr llist)))
  t)

; (defun ttest ()
;   (setq x '(a b))
;   (loop while x do (setf x (cdr x))
;         finally (print x)))

; function backtrack;
; begin
; SL := [Start]; NSL := [Start]; DE := [ ]; CS := Start;
; % initialize:
; while NSL ≠ [ ] do
; % while there are states to be tried
; begin
; if CS = goal (or meets goal description)
; then return SL;
; % on success, return list of states in path.
; if CS has no children (excluding nodes already on DE, SL, and NSL)
; then begin
; 	while SL is not empty and CS = the first element of SL do
; 	begin
; 	add CS to DE;
; 	% record state as dead end
; 	remove first element from SL;
; 	%backtrack
; 	remove first element from NSL;
; 	CS := first element of NSL;
; end
; add CS to SL;
; end
; else begin
; place children of CS (except nodes already on DE, SL, or NSL) on NSL;
; CS := first element of NSL;
; add CS to SL
; end
; end;
; return FAIL;
; end.