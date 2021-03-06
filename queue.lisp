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
