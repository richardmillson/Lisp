; find the shortest path between two nodes in an unweighted graph
; ie perform a breadth first search

; partial solutions are stored on the queue as ordered lists of node names
; assume nil cannot be the name of a node
; check if start and goal are same?
; check if neighbour is goal sooner?

(defun bfs (start goal)
  (init-queue)
  (enqueue (list start))
  (setq visited nil)
  (defun check-next (front)
    (cond ((null front) nil) ; exhausted all potential solution paths
          ((equal (car front) goal) (reverse front)) ; return found solution
          ((member (car front) visited) (c(quit)heck-next (dequeue)))
          (t (enqueue-list (mapcar #'(lambda (x) (cons x front)) (neighbours (car front)))) ; add potential solution paths to queue to be checked later
             (cons (car front) visited) ; add current node to visited
             (check-next (dequeue)))))
  (check-next (dequeue)))

; (mapcar #'(lambda (x) (cons x front)) (neighbours (car front)))
; (setq x '(1 1 1))
; (setq y '(2 3 4))
; (mapcar #'(lambda (x) (cons x y)) x)



; the graph is a list of nodes
; a node is a list whose first element is the name of the node
; and whose remaining elements are the names of any neighbours
; (set-graph '((a b) (b a c) (c b d) (d c)))
; graph implementation

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

; queue implementation

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
