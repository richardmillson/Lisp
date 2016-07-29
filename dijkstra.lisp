; Dijkstra's algorithm for finding the shortest path between nodes in a weighted graph

; >(setq X '(A B))
; >(member X '((A B)))
; NIL
; >(member X (list X))
; ((A B))
; >(equal (list X) '((A B)))
; T

(defun dijkstra (graph start)
  (setq dijkstra (list start 0))
  
  )

; weighted directed graph implementation
; the graph is a list of nodes
; a node is a list whose first element is the name of the node
; and whose remaining elements are pairs of a neighbours name 
; and cost the to that neighbour
; (set-graph '((a (b 3)) (b (c 1) (d 5)) (c (d 2)) (d (b 2))))

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
        (t (appears-in target (cdr llist)))))

(defun is-neighbour (desired given)
  ; (member desired (find-node given)))
  (appears-in desired (find-node given)))

(defun list-nodes (graph)
  (mapcar #'car graph))
