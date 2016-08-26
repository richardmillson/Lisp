
; from AI Algorithms, Data Structures, and Idioms in Prolog, Lisp, and Java by George Luger



; streams

;;; cons-stream adds a new first element to a stream.
(defun cons-stream (element stream)
  (cons element stream))

;;; head-stream returns the first element of the stream.
(defun head-stream (stream) 
  (car stream))

;;; tail-stream returns the stream with first element deleted.
(defun tail-stream (stream) 
  (cdr stream))

;;; empty-stream-p is true if the stream is empty.
(defun empty-stream-p (stream) 
  (null stream))

;;; make-empty-stream creates an empty stream.
(defun make-empty-stream () 
  nil)

;;; combine-stream appends two streams.
(defun combine-streams (stream1 stream2)
  (cond ((empty-stream-p stream1) stream2)
        (t (cons-stream (head-stream stream1)
                        (combine-streams (tail-stream stream1) 
                                         stream2)))))

; logic programming interpreter shell

;;; reads goals and attempts to satisfy them 
;;; against the logic database bound to *assertions*
(defun logic-shell ()
  (print '? )
  (let ((goal (read)))
    (cond ((equal goal 'quit) 'bye)
          (t (print-solutions goal (solve goal nil))
             (terpri)
             (logic-shell)))))

;;; recursive call to solve solves the goal under each substitution set
(defun solve (goal substitutions)
  (declare (special *assertions*))
  (if (conjunctive-goal-p goal)
      (filter-through-conj-goals (body goal)
                                 (cons-stream substitutions
                                              (make-empty-stream)))
      (infer goal substitutions *assertions*)))

;;; passes a stream of substitution sets through a sequence of goals
(defun filter-through-conj-goals (goals substitution-stream)
  (if (null goals) 
      substitution-stream
      (filter-through-conj-goals 
        (cdr goals)
        (filter-through-goal (car goals) substitution-stream))))

;;; filters substitution-stream through a single goal
(defun filter-through-goal (goal substitution-stream)
  (if (empty-stream-p substitution-stream)
      (make-empty-stream)
      (combine-streams
        (solve goal (head-stream substitution-stream))
        (filter-through-goal goal (tail-stream substitution-stream)))))

;;; simple goals handled by infer
(defun infer (goal substitutions kb)
  (if (null kb)
      (make-empty-stream)
      (let* ((assertion
               (rename-variables (car kb)))
             (match (if (rulep assertion)
                        (unify goal (conclusion assertion) substitutions)
                        (unify goal assertion substitutions))))
        (if (equal match 'failed)
            (infer goal substitutions (cdr kb))
            (if (rulep assertion)
                (combine-streams
                  (solve (premise assertion) match)
                  (infer goal substitutions (cdr kb)))
                (cons-stream match
                             (infer goal substitutions (cdr kb))))))))

(defun print-solutions (goal substitution-stream)
  (cond ((empty-stream-p substitution-stream) nil)
        (t (print (apply-substitutions goal (head-stream substitution-stream)))
           (terpri)
           (print-solutions goal (tail-stream substitution-stream)))))

;;; replace variables with their values under a substitution set
(defun apply-substitutions
  (pattern substitution-list)
  (cond ((is-constant-p pattern) pattern)
        ((varp pattern)
         (let ((binding (get-binding pattern substitution-list)))
           (cond (binding (apply-substitutions (get-binding-value binding) substitution-list))
                 (t pattern))))
        (t (cons (apply-substitutions (car pattern) substitution-list)
                 (apply-substitutions (cdr pattern) substitution-list)))))

; renaming scheme

(defun rename-variables (assertion)
  (declare (special *name-list*))
  (setq *name-list* nil)
  (rename-rec assertion))

(defun rename-rec (exp)
  (declare (special *name-list*))
  (cond ((is-constant-p exp) exp)
        ((varp exp) (rename exp))
        (t (cons (rename-rec (car exp))
                 (rename-rec (cdr exp))))))

(defun rename (var)
  (declare (special *name-list*))
  (list 'var (or (cdr (assoc var *name-list* :test #'equal))
                 (let ((name (gensym)))
                   (setq *name-list* (acons var name *name-list*))
                   name))))

; access components of rules and goals

(defun premise (rule) 
  (nth 2 rule))

(defun conclusion (rule) 
  (nth 4 rule))

(defun rulep (pattern)
  (and (listp pattern) (equal (nth 0 pattern) 'rule)))

(defun conjunctive-goal-p (goal)
  (and (listp goal) (equal (car goal) 'and)))

(defun body (goal) 
  (cdr goal))

; unification

(defun unify (pattern1 pattern2 substitution-list)
  (cond ((equal substitution-list 'failed) 
         'failed)
        ((varp pattern1)
         (match-var pattern1
                    pattern2 substitution-list))
        ((varp pattern2)
         (match-var pattern2
                    pattern1 substitution-list))
        ((is-constant-p pattern1)
         (cond ((equal pattern1 pattern2) substitution-list)
               (t 'failed)))
        ((is-constant-p pattern2) 'failed)
        (t (unify (cdr pattern1)
                  (cdr pattern2)
                  (unify (car pattern1)
                         (car pattern2)
                         substitution-list)))))

(defun match-var (var pattern substitution-list)
  (cond ((equal var pattern) substitution-list)
        (t (let ((binding
                   (get-binding var substitution-list)))
             (cond (binding (unify
                              (get-binding-value binding)
                              pattern substitution-list))
                   ((occursp var pattern) 'failed)
                   (t (add-substitution var pattern
                                        substitution-list)))))))

(defun occursp (var pattern)
  (cond ((equal var pattern) t)
        ((or (varp pattern) (is-constant-p pattern)) nil)
        (t (or (occursp var (car pattern))
               (occursp var (cdr pattern))))))

(defun is-constant-p (item)
  (atom item))

(defun varp (item)
  (and (listp item)
       (equal (length item) 2)
       (equal (car item) 'var)))

(defun get-binding (var substitution-list)
  (assoc var substitution-list :test #'equal))

(defun get-binding-value (binding) 
  (cdr binding))

(defun add-substitution (var pattern substitution-list)
  (acons var pattern substitution-list))



; unification example execution

; > (unify '(p a (var x)) '(p a b) ( ))
; (((var x) . b))
; ; Returns substitution of b for (var x).
; > (unify '(p (var y) b) '(p a (var x)) ( ))
; (((var x) . b) ((var y) . a))
; ; Variables appear in both patterns.
; > (unify '(p (var x)) '(p (q a (var y))) ( ))
; (((var x) q a (var y)))
; ; Variable is bound to a complex pattern.
; > (unify '(p a) '(p a) ( ))
; nil
; ; nil indicates no substitution required.
; > (unify '(p a) '(q a) ( ))
; failed
; Returns atom “failed” to indicate failure.



; logic programming interpreter shell example

(setq *assertions*
      '((likes george beer)
        (likes george kate)
        (likes george kids)
        (likes bill kids)
        (likes bill music)
        (likes bill pizza)
        (likes bill wine)
        (rule if (and (likes (var x) (var z))
                      (likes (var y) (var z)))
              then (friend (var x) (var y)))))

; logic programming interpreter shell example execution

; > (logic-shell)
; ?(likes bill (var x))
; (likes bill kids)
; (likes bill music)
; (likes bill pizza)
; (likes bill wine)
; ?(likes george kate)
; (likes george kate)
; ; Failed query returns nothing.
; ?(likes george taxes)
; ?(friend bill george)
; (friend bill george)
; ; From (and (likes bill kids) (likes george kids)).
; ?(friend bill roy)
; ; roy not in knowledge base, fail.
; ?(friend bill (var x))
; (friend bill george) 
; ; From (and (likes bill kids) (likes george kids)).
; (friend bill bill) 
; ; From (and (likes bill kids) (likes bill kids)).
; (friend bill bill) 
; ; From (and (likes bill music) (likes bill music)).
; (friend bill bill) 
; ; From (and (likes bill pizza) (likes bill pizza)).
; (friend bill bill) 
; ; From (and (likes bill wine) (likes bill wine)).
; ?quit
; bye
; >
