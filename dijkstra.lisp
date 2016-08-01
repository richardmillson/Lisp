; Dijkstra's algorithm for finding the shortest path between nodes in a weighted graph

; >(setq X '(A B))
; >(member X '((A B)))
; NIL
; >(member X (list X))
; ((A B))
; >(equal (list X) '((A B)))
; T

(defun dijkstra (graph start)
  (init-costs graph)
  (update-cost start 0)
  (init-predecessors graph)
  (init-visited)
  (init-candidates start)
  (loop while candidates do
        (let ((current (min-candidate)))
          (add-visited current)
          (loop for neighbour in (set-difference (neighbours current) visited) do
                (cond ()( ())
                      ))))

; keep track of the smallest cost to get to each node found by dijkstra
; note that all paths start at the start node called in dijkstra
(defun init-costs (graph)
  (let ((impossible (+ 1 (apply '+ (remove-if-not #'integerp (flatten graph))))))
    (setq costs (mapcar #'(lambda (node) (list node impossible)) (list-nodes graph)))))

(defun flatten (llist)
    (cond ((null llist) nil)
        ((atom llist) (list llist))
        (t (append (flatten (car llist)) (flatten (cdr llist))))))

(defun update-cost (end cost)
  (setq costs (append (remove-if #'(lambda (node) (equal (car node) end)) costs) (list (list end cost)))))

(defun cost (target)
  (cadar (remove-if-not #'(lambda (node) (equal (car node) target)) costs)))

; keep track of the predecessor of each node on the shortest path to the start node
(defun init-predecessors (graph)
  (setq predecessors (mapcar #'(lambda (node) (list node nil)) (list-nodes graph))))

(defun update-predecessor (end predecessor)
  (setq predecessors (append (remove-if #'(lambda (node) (equal (car node) end)) predecessors) (list (list end predecessor)))))

(defun init-visited ()
  (setq visited nil))

(defun add-visited (node)
  (setq visited (append visited (list node))))

(defun init-candidates (start)
  (setq candidates (list start)))

(defun add-candidate (node)
  (setq candidate (append candidate (list node))))

(defun min-candidate ()
  (setq candidates (sort candidates (lambda (left right)  (< (cost left) (cost right)))))
  (pop candidates))



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

; tests

(set-graph '((a (b 3)) (b (c 1) (d 5)) (c (d 2)) (d (b 2))))
; (init-costs graph)
; (update-cost 'a 0)
(dijkstra graph 'a)
