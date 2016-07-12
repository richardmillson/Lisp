(defun init-queue ()
  (setq queue nil))

(defun enqueue (element)
  (setf queue (append queue (list element)))
  element)

(defun front ()
  (car queue))

(defun list-queue ()
  queue)

(defun dequeue ()
  (let ((front-element (front)))
    (setf queue (cdr queue))
    front-element))
