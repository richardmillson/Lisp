(defun init-stack ()
  (setq stack nil))

(defun push (element)
  (setf stack (append (list element) stack))
  element)

(defun top ()
  (car stack))

(defun list-stack ()
  stack)

(defun pop ()
  (let ((top-element (top)))
    (setf stack (cdr stack))
    top-element))
