; stack implementation

(defun init-stack ()
  (setq stack nil))

(defun push (element)
  (setf stack (append (list element) stack))
  element)

(defun push-list (llist)
  (unless (null llist)
    (push (car llist))
    (push-list (cdr llist)))
  t)

(defun top ()
  (car stack))

(defun list-stack ()
  stack)

(defun pop ()
  (let ((top-element (top)))
    (setf stack (cdr stack))
    top-element))
