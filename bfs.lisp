
;;; find the shortest path between two nodes in an unweighted graph
;;; ie perform a breadth first search

;;; partial solutions are stored on the queue as ordered lists of node names
;;; check if start and goal are same?
;;; check if neighbour is goal sooner?
;;; can this handle cases where no path exists?

(defun bfs (start goal)
  (init-queue)
  (enqueue (list start))
  (setq visited nil)
  (labels ((check-next (front)
                       (cond ((null front) nil) ; exhausted all potential solution paths, none found
                             ((equal (car front) goal) (reverse front)) ; return found solution
                             ((member (car front) visited) (check-next (dequeue)))
                             (t (enqueue-list (mapcar #'(lambda (x) (cons x front)) 
                                                      (set-difference (neighbours (car front)) visited))) ; add potential solution paths to queue to be checked later
                                (setq visited (cons (car front) visited)) ; add current node to visited
                                (check-next (dequeue))))))
          (check-next (dequeue))))

;;; print the shortest path from start to end
(defun print-bfs (start end)
  (let ((solution (bfs start end)))
    (if solution
        (format nil "the minimum path between ~a and ~a is ~a with cost = ~a" start end solution (- (length solution) 1))
        (format nil "no solution exists"))))



;;; graph implementation
;;; graph is a list of nodes
;;; a node is a list whose first element is the name of the node
;;; and whose remaining elements are the names of any neighbours
;;; >(set-graph '((a b) (b a c) (c b d) (d c)))

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
        (t (appears-in (cdr llist)))))

(defun is-neighbour (desired given)
  ; (member desired (find-node given)))
  (appears-in desired (find-node given)))

(defun list-nodes (graph)
  (mapcar #'car graph))



;;; queue implementation

(defun init-queue ()
  (setq queue nil))

(defun enqueue (element)
  (setf queue (append queue (list element)))
  element)

(defun enqueue-list (llist)
  (unless (null llist)
    (enqueue (car llist))
    (enqueue-list (cdr llist)))
  t)

(defun front ()
  (car queue))

(defun list-queue ()
  queue)

(defun dequeue ()
  (let ((front-element (front)))
    (setf queue (cdr queue))
    front-element))



; tests

(set-graph '((a b) (b a c) (c b d) (d c)))
(print-bfs 'a 'd)
(print-bfs 'a 'z)
