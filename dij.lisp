
;;; dijkstra's algorithm
;;; find the shortest path between nodes in a weighted, directed graph
;;; >(set-graph '((a (b 3)) (b (c 1) (d 5)) (c (d 2)) (d (b 2))))
;;; >(print-dij 'a 'd)
;;; >(print-dij 'a 'z)
;;; >(print-dij 'z 'd)
;;; >(print-dij 'b 'a)
(defun dij (graph start)
  (init-path-costs graph)
  (update-path-cost start 0)
  (init-predecessors graph)
  (init-visited)
  (init-candidates start)
  ;; use names not node objects
  (loop while candidates do
        (let ((current (min-candidate)))
          (add-visited current)
          (loop for neighbour in (set-difference (list-nodes (neighbours current)) visited :test #'equal) do
                (when (< (+ (path-cost current) (cost-to-neighbour current neighbour)) (path-cost neighbour))
                  (update-path-cost neighbour (+ (path-cost current) (cost-to-neighbour current neighbour)))
                  (update-predecessor neighbour current))
                (when (not (member neighbour candidates)) (add-candidate neighbour)))))
  predecessors)

;;; remember the smallest cost to get to each node
;;; all paths start at the start node called in dij
(defun init-path-costs (graph)
  (let ((impossible (impossible-cost graph)))
    (setq path-costs (mapcar #'(lambda (node) (list node impossible)) (list-nodes graph)))))

(defun impossible-cost (graph)
  (+ 1 (apply '+ (remove-if-not #'integerp (flatten graph)))))

(defun flatten (llist)
  (cond ((null llist) nil)
        ((atom llist) (list llist))
        (t (append (flatten (car llist)) (flatten (cdr llist))))))

(defun update-path-cost (end path-cost)
  (setq path-costs 
        (append (remove-if #'(lambda (node) (equal (car node) end)) path-costs) (list (list end path-cost)))))

;;; takes the name of a node
(defun path-cost (target)
  (cadar (remove-if-not #'(lambda (node) (equal (car node) target)) path-costs)))

;;; remember the predecessor of each node on the shortest path to the start node
(defun init-predecessors (graph)
  (setq predecessors (mapcar #'(lambda (node) (list node nil)) (list-nodes graph))))

;;; end and predecessor are names of nodes
(defun update-predecessor (end predecessor)
  (setq predecessors (append (remove-if #'(lambda (node) (equal (car node) end)) predecessors) (list (list end predecessor)))))

(defun get-predecessor (start)
  (cadr (find-if #'(lambda (node) (equal (car node) start)) predecessors)))

;;; return the shortest path 
(defun get-path (node)
  (labels ((get-reversed-path (nnode)
                              (cond ((null nnode) nil)
                                    (t (append (list nnode) (get-reversed-path (get-predecessor nnode)))))))
          (reverse (get-reversed-path node))))

;;; shortest path from start to end
(defun shortest-dij (start end)
  (dij graph start)
  (get-path end))

;;; print the shortest path from start to end
(defun print-dij (start end)  
  (dij graph start)
  (cond ((not (member start (list-nodes graph))) (format nil "~a does not appear in the graph" start))
        ((not (member end (list-nodes graph))) (format nil "~a does not appear in the graph" end))
        ((= (path-cost end) (impossible-cost graph)) (format nil "no path between ~a and ~a exists" start end))
        (t (format nil "the minimum path between ~a and ~a is ~a with cost = ~a" start end (get-path end) (path-cost end)))))

(defun init-visited ()
  (setq visited nil))

(defun add-visited (node)
  (setq visited (append visited (list node))))

(defun init-candidates (start)
  (setq candidates (list start)))

(defun add-candidate (node)
  (setq candidates (append candidates (list node)))  
  (setq candidates (sort candidates (lambda (left right) (< (path-cost left) (path-cost right))))))

(defun min-candidate ()
  (pop candidates))

;;; nodes stored as (name cost)
(defun cost-to-neighbour (start end)
  (cadr (find-if #'(lambda (node) (equal (car node) end)) (neighbours start))))



;;; graph implementation
;;; graph is a list of nodes
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
