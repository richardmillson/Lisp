; implementation of backtrack from George Luger's Artificial Intelligence 6ed p 97
; sl state list ie states in current path being tried
; nsl new state list ie unvisited nodes awaiting evaluation
; de dead ends ie states whose descendants failed to contain a goal
; cs current state
; is-goal predicate takes a state and returns nil if not a goal

(defun backtrack (start children is-goal)
  (let ((sl start) (nsl start) (de nil) (cs start))
    (loop while nsl do ; while there are still untested states ie possible solutions
          (cond ((is-goal cs) 
                 cs)
                ((or (null (children cs))
                     (intersection (children cs) (append sl nsl de)))
                 (push cs de)
                 (pop sl)
                 (pop nsl)
                 (setf cs (nth 0 nsl)))
                (t 
                  (append (set-difference (children cs) (append sl nsl de)) nsl)
                  (setf cs (nth 0 nsl))
                  (push cs sl))))))

; (set-graph '((a b) (b a c) (c b d) (d c)))
; (backtrack 'a #'neighbours #'(lambda (node) (equal node d)))

(setq graph nil)
(defun set-graph (graph-as-list)
  (setf graph graph-as-list))

(defun find-node (node-name)
  (defun next-node (unchecked-graph)
    (cond ((null unchecked-graph) nil)
          ((equal node-name (caar unchecked-graph)) (car unchecked-graph))
          (t (next-node (cdr unchecked-graph)))))
  (next-node graph))

(defun neighbours (node-name)
  (cdr (find-node node-name)))

(defun appears-in (target llist)
  (cond ((null llist) nil)
        ((equal (car llist) target) t)
        (t (appears-in (cdr llist)))))

(defun is-neighbour (desired given)
  ; (member desired (find-node given)))
  (appears-in desired (find-node given)))

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