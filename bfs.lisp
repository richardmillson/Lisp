; find the shortest path between two nodes in an unweighted graph
; ie perform a breadth first search

; the graph is a list of nodes
; a node is a list whose first element is the name of the node
; and whose remaining elements are the names of any neighbours
; (set-graph '((a b c) (b a) (c a)))

(setq graph nil)
(defun set-graph (graph-as-list)
  (setf graph graph-as-list))

