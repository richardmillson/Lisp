
;;; graph implementation
;;; graph is a list of nodes
;;; for unweighted, undirected graph
;;; a node is a list whose first element is the name of the node
;;; and whose remaining elements are the names of any neighbours
;;; >(set-graph '((a b) (b a c) (c b d) (d c)))
;;; for weighted, directed graph
;;; a node is a list whose first element is the name of the node
;;; and whose remaining elements are pairs of a neighbours name 
;;; and cost the to that neighbour
;;; >(set-graph '((a (b 3)) (b (c 1) (d 5)) (c (d 2)) (d (b 2))))(setq graph nil)
(setq graph nil)
(defun set-graph (graph-as-list)
  (setf graph graph-as-list))

(defun find-node (node-name)
  (labels ((next-node (unchecked-graph)
                      (cond ((null unchecked-graph) nil)
                            ((equal node-name (caar unchecked-graph)) (car unchecked-graph))
                            (t (next-node (cdr unchecked-graph))))))
          (next-node graph)))

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
