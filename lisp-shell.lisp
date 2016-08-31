
; lisp-shell
; from AI Algorithms, Data Structures, and Idioms in Prolog, Lisp, and Java 
; by George Luger



; streams

; delayed evaluation
; function produces first element of stream
; then execution frozen / delayed until next element needed
; gives us lazily evaluated infinite lists

; function closures create the delayed portion of the stream
; closure is a function with its variable bindings in the current environemnt
; can bind a closure to a variable or pass as parameter
; evaluated with funcall
; a closure freezes a function application until a later time

;;; force and delay control evaluation of expressions
(defmacro delay (exp) 
  `(function (lambda () ,exp)))

(defun force (function-closure) 
  (funcall function-closure))

;;; add new first element to a stream
(defmacro cons-stream (exp stream) 
  `(cons ,exp (delay ,stream)))

;;; returns the first element of the stream
(defun head-stream (stream)
  (car stream))

;;; returns the stream with first element deleted
(defun tail-stream (stream) 
  (force (cdr stream)))

;;; appends two streams
(defun combine-streams (stream1 stream2) 
  (cond ((empty-stream-p stream1) stream2)
        (t (cons-stream (head-stream stream1)
                        (combine-streams (tail-stream stream1) stream2)))))

;;; true if stream is empty
(defun empty-stream-p (stream) 
  (null stream))

;;; creates an empty stream
(defun make-empty-stream () 
  nil)

;;; returns stream with function applied to each element
(defun map-stream (stream func)
  (cond ((empty-stream-p stream) (make-empty-stream))
        (t (cons-stream (funcall func (head-stream stream))
                        (map-stream (tail-stream stream) func)))))

;;; applies test to the first element of stream
;;; if test true conses that element with filtered tail
;;; otherwise returns filtered tail
(defun filter-stream (stream test)
  (cond ((empty-stream-p stream) (make-empty-stream))
        ((funcall test (head-stream stream)) 
         (cons-stream (head-stream stream) 
                      (filter-stream (tail-stream stream) test)))
        (t (filter-stream (tail-stream stream)test))))



; unification

;;; recursive unification algorithm
;;; takes two patterns and list of substitutions found so far 
;;; returns either "failed" or the substitution-list
;;; augmented with those bindings needed for a match
(defun unify (pattern1 pattern2 substitution-list)
  (cond ((equal substitution-list 'failed) 'failed)
        ((varp pattern1) 
         (match-var pattern1 pattern2 substitution-list))
        ((varp pattern2) 
         (match-var pattern2 pattern1 substitution-list))
        ((is-constant-p pattern1) 
         (cond ((equal pattern1 pattern2) substitution-list)
               (t 'failed)))
        ((is-constant-p pattern2) 'failed)
        (t (unify (cdr pattern1) (cdr pattern2) 
                  (unify (car pattern1) (car pattern2)
                         substitution-list)))))

;;; attempts to match a variable to a pattern
;;; first checks for existing bindings on the variable
;;; then performs an occurs check
(defun match-var (var pattern substitution-list)
  (cond ((equal var pattern) substitution-list)
        (t (let ((binding (get-binding var substitution-list)))
             (cond (binding 
                     (unify (get-binding-value binding) 
                            pattern substitution-list))
                   ((occursp var pattern) 'failed)
                   (t (acons var pattern  substitution-list)))))))


;;; check if a variable occurs in a pattern
(defun occursp (var pattern)
  (cond ((equal var pattern) t)
        ((or (varp pattern) (is-constant-p pattern))
         nil)
        (t (or (occursp var (car pattern))
               (occursp var (cdr pattern))))))

;;; determines if an item is a constant
;;; assume that all constants are atoms
(defun is-constant-p (item)
  (atom item))

(defun varp (item)
  (and (listp item) 
       (equal (length item) 2)
       (equal (car item) 'var)))


;;; takes a variable and a substitution list
;;; returns a (variable . binding-value) pair 
(defun get-binding (var substitution-list) 
  (assoc var substitution-list :test #'equal))

;;; returns the binding value from a (variable . binding-value) pair
(defun get-binding-value (binding) 
  (cdr binding))

;;;adds a variable and a binding-value to a substitution-list
(defun add-substitution (var pattern substitution-list)
  (acons var pattern substitution-list))



; expert system shell

; functions for manipulating our abstract data type for stream elements
; facts are dotted pairs (<assertion> . <cf>)
; where <assertion> is a positive literal 
; and <cf> is its certainty measure
; rules are in the format (rule if <premise> then <conclusion> <cf>)
; where <cf> is the certainty factor

;;; top level interpreter loop
(defun lisp-shell ()
  (declare (special *case-specific-data*))
  (setq *case-specific-data* ())
  (prin1 'lisp-shell> )
  (let ((goal (read)))
    (terpri)
    (cond ((equal goal 'quit) 'bye)
          (t (print-solutions goal (solve goal (subst-record nil  0)))
             (terpri)
             (lisp-shell)))))

;;; solve will take a single goal and a set of substitutions and return a
;;; stream of augmented substitutions that satisfy the goal.

(defun solve (goal substitutions)
  (filter-stream 
    (if (conjunctive-goal-p goal) 
        (filter-through-conj-goals 
          (cdr (body goal)) 
          (solve (car (body goal)) substitutions))
        (solve-simple-goal goal substitutions))
    #'(lambda (x) (< 0.2 (subst-cf x)))))

(defun solve-simple-goal (goal substitutions)
  (declare (special *assertions*))
  (declare (special *case-specific-data*))
  (or (told goal substitutions *case-specific-data*)
      (infer goal substitutions *assertions*)
      (ask-for goal substitutions)))

;;; takes list of goals and a stream of substitutions
;;; filters them through the goals one at a time
(defun filter-through-conj-goals (goals substitution-stream)
  (if (null goals) 
      substitution-stream
      (filter-through-conj-goals
        (cdr goals) 
        (filter-through-goal (car goals) substitution-stream))))

(defun filter-through-goal (goal substitution-stream)
  (if (empty-stream-p substitution-stream) 
      (make-empty-stream)
      (let ((subs (head-stream substitution-stream)))
        (combine-streams
          (map-stream (solve goal subs)
                      #'(lambda (x) ( subst-record (subst-list x) 
                                     (min (subst-cf x) (subst-cf subs)))))
          (filter-through-goal goal 
                               (tail-stream substitution-stream))))))

;;; takes a goal, a set of substitutions and a knowledge base
;;; attempts to infer the goal from the kb
(defun infer (goal substitutions kb)
  (if (null kb) 
      (make-empty-stream)
      (let* ((assertion (rename-variables (car kb)))
             (match (if (rulep assertion)
                        (unify goal (conclusion assertion) 
                               (subst-list substitutions))
                        (unify goal assertion (subst-list substitutions)))))
        (if (equal match 'failed)
            (infer goal substitutions (cdr kb))
            (if (rulep assertion)
                (combine-streams 
                  (solve-rule 
                    assertion 
                    (subst-record match (subst-cf substitutions)))
                  (infer goal substitutions (cdr kb)))
                (cons-stream (subst-record match (fact-cf assertion)) 
                             (infer goal substitutions (cdr kb))))))))

(defun solve-rule (rule substitutions)
  (map-stream (solve (premise rule) substitutions)
              #'(lambda (x) (subst-record 
                              (subst-list x)
                              (* (subst-cf x) (rule-cf rule))))))

;;; return the result of applying a set of substitutions to a pattern
(defun apply-substitutions (pattern substitution-list)
  (cond ((is-constant-p pattern) pattern)
        ((varp pattern)
         (let ((binding (get-binding pattern substitution-list)))
           (cond (binding (apply-substitutions 
                            (get-binding-value binding)
                            substitution-list))
                 (t pattern))))
        (t (cons (apply-substitutions (car pattern) substitution-list)
                 (apply-substitutions (cdr pattern) substitution-list)))))

;;; take a goal and a stream of substitutions
;;; print that goal with each substitution in the stream
(defun print-solutions (goal substitution-stream)
  (cond ((empty-stream-p substitution-stream) nil)
        (t (print (apply-substitutions 
                    goal (subst-list (head-stream substitution-stream))))
           (write-string " cf = ")
           (prin1 (subst-cf (head-stream substitution-stream)))
           (terpri) (terpri)
           (print-solutions goal (tail-stream substitution-stream)))))

; functions for handling rules and facts

; rule functions     
; rule format is 
; (rule if  then )

(defun premise (rule) 
  (nth 2 rule))

(defun conclusion (rule) 
  (nth 4 rule))

(defun rulep (pattern) 
  (and (listp pattern)
       (equal (nth 0 pattern) 'rule)))

(defun rule-cf (rule) 
  (nth 5 rule))

; fact functions
; fact format is
; ( . cf)

(defun fact-pattern (fact) 
  (car fact))

(defun fact-cf (fact) 
  (cdr fact))

; substitutions format is
; ( . cf)

;;; returns the set of bindings from a pair
(defun subst-list (substitutions) 
  (car substitutions))

;;; returns certainty factor
(defun subst-cf (substitutions) 
  (cdr substitutions))

;;; constructs a pair from a set of substitutions and a certainty factor
(defun subst-record (substitutions cf) 
  (cons substitutions cf))


; conjunctive goals are goals of the form 
; (and   ... )

(defun conjunctive-goal-p (goal)
  (and (listp goal)
       (equal (car goal) 'and)))

(defun body (goal) 
  (cdr goal))

;;; takes an assertion and rename all its variables using gensym
(defun rename-variables (assertion)
  (declare (special *name-list*))
  (setq *name-list* ())
  (rename-rec assertion))

(defun rename-rec (exp)
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

; user interaction

(defun ask-for (goal substitutions)
  (declare (special *askables*))
  (declare (special *case-specific-data*))
  (if (askable goal *askables*) 
      (let* ((query (apply-substitutions goal (subst-list substitutions)))
             (result (ask-rec query)))
        (setq *case-specific-data* (cons (subst-record query result) 
                                         *case-specific-data*))
        (cons-stream (subst-record (subst-list substitutions) result)
                     (make-empty-stream)))))

(defun ask-rec (query)
  (prin1 query)
  (write-string "  >")
  (let ((answer (read)))
    (cond ((equal answer 'y) 1)
          ((equal answer 'n) -1)
          (t (print "answer must be y or n")
             (terpri)
             (ask-rec query)))))

(defun askable (goal askables)
  (cond ((null askables) nil)
        ((not (equal (unify goal (car askables) ()) 'failed)) t)
        (t (askable goal (cdr askables)))))

(defun told (goal substitutions case-specific-data)
  (if (null case-specific-data) (make-empty-stream)
      (let ((match (unify goal 
                          (fact-pattern (car case-specific-data)) 
                          (subst-list substitutions))))
        (if (equal match 'failed)
            (told goal substitutions (cdr case-specific-data))
            (cons-stream 
              (subst-record match (fact-cf (car case-specific-data)))
              (make-empty-stream))))))
