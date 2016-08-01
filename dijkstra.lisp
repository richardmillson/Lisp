;;; dijkstra's algorithm 
;;; find the shortest path between nodes in a weighted graph
(defun dijkstra (graph start)
  (init-path-costs graph)
  (update-path-cost start 0)
  (init-predecessors graph)
  (init-visited)
  (init-candidates start)
  ;; current is a name
  (loop while candidates do
        (let ((current (min-candidate)))
          (add-visited current)
          ; fix visited is only names, neighbours is node type
          ; rewrite cost-to
          (loop for neighbour in (set-difference (list-nodes (neighbours current)) 
                                                 visited 
                                                 :test #'equal)
                do
                (when (< (+ (path-cost current) (cost-to-neighbour current neighbour)) (path-cost neighbour))
                  (update-path-cost neighbour (+ (path-cost current) (cost-to-neighbour current neighbour)))
                  (update-predecessor neighbour current))
                (when (not (member neighbour candidates)) (add-candidate neighbour))
                finally predecessors))))

;;; keep track of the smallest cost to get to each node found by dijkstra
;;; note that all paths start at the start node called in dijkstra
(defun init-path-costs (graph)
  (let ((impossible (+ 1 (apply '+ (remove-if-not #'integerp 
                                                  (flatten graph))))))
    (setq path-costs 
          (mapcar #'(lambda (node) (list node impossible)) 
                  (list-nodes graph)))))

(defun flatten (llist)
  (cond ((null llist) nil)
        ((atom llist) (list llist))
        (t (append (flatten (car llist)) (flatten (cdr llist))))))

(defun update-path-cost (end path-cost)
  (setq path-costs 
        (append (remove-if #'(lambda (node) (equal (car node) end)) 
                           path-costs) 
                (list (list end path-cost)))))

;;; takes the name of a node
(defun path-cost (target)
  (cadar (remove-if-not #'(lambda (node) (equal (car node) target)) 
                        path-costs)))

;;; keep track of the predecessor of each node on the shortest path to the start node
(defun init-predecessors (graph)
  (setq predecessors (mapcar #'(lambda (node) (list node nil)) 
                             (list-nodes graph))))

;;; end and predecessor are names of a nodes
(defun update-predecessor (end predecessor)
  (setq predecessors (append (remove-if #'(lambda (node) (equal (car node) end))
                                        predecessors) 
                             (list (list end predecessor)))))

(defun get-predecessor (start)
  (cadr (find-if #'(lambda (node) (equal (car node) start)) predecessors)))

;;; return the shortest path 
(defun get-path (node)
  (cond ((null node) nil)
        (t (append (list node) (get-path (get-predecessor node))))))

(defun init-visited ()
  (setq visited nil))

(defun add-visited (node)
  (setq visited (append visited (list node))))

(defun init-candidates (start)
  (setq candidates (list start)))

(defun add-candidate (node)
  (setq candidates (append candidates (list node)))  
  (setq candidates (sort candidates 
                         (lambda (left right) (< (path-cost left) 
                                                 (path-cost right))))))

(defun min-candidate ()
  (pop candidates))

;;; nodes are stored as (name cost)
(defun cost-to-neighbour (start end)
  (cadr (find-if #'(lambda (node) (equal (car node) end)) (neighbours start))))

(defun name (node)
  (car node))


;;; weighted directed graph implementation
;;; the graph is a list of nodes
;;; a node is a list whose first element is the name of the node
;;; and whose remaining elements are pairs of a neighbours name 
;;; and cost the to that neighbour
;;; (set-graph '((a (b 3)) (b (c 1) (d 5)) (c (d 2)) (d (b 2))))

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
; (init-path-costs graph)
; (update-path-cost 'a 0)
; (init-predecessors graph)
; (init-visited)
; (init-candidates 'a)
(print (path-cost 'a))
(print path-costs)
(print predecessors)
(dijkstra graph 'a)
(print visited)
(print candidates)
