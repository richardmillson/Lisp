
; lisp-shell
; from AI Algorithms, Data Structures, and Idioms in Prolog, Lisp, and Java by George Luger



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

(defmacro delay (exp) 
  `(function (lambda () ,exp)))

(defun force (function-closure) 
  (funcall function-closure))

(defmacro cons-stream (exp stream) 
  `(cons ,exp (delay ,stream)))

; tail-stream returns the stream with first element deleted.
(defun tail-stream (stream) 
  (force (cdr stream)))

; combine-stream appends two streams.
(defmacro combine-streams (stream1 stream2) 
  `(comb-f ,stream1 (delay ,stream2)))

(defun comb-f (stream1 stream2)
  (if (empty-stream-p stream1)
      (force stream2)
      (cons-stream (head-stream stream1)
                   (comb-f (tail-stream stream1) stream2))))

; head-stream returns the first element of the stream.
(defun head-stream (stream) 
  (car stream))

; empty-stream-p is true if the stream is empty.
(defun empty-stream-p (stream) 
  (null stream))

; make-empty-stream creates an empty stream.
(defun make-empty-stream () 
  nil)

; returns a list of the results of applying a functional to all the elements of a list
; simplified version of mapcar 
; mapcar allows more than one argument list
(defun map-simple (func list)
  (cond ((null list) nil)
        (t (cons (funcall func (car list))
                 (map-simple func (cdr list))))))

; applies the test to the first element of the list
; if the test returns non-nil, it conses the element onto the result of filter applied to the cdr of the list
; otherwise, it just returns the filtered cdr.
(defun filter (list-of-elements test)
  (cond ((null list-of-elements) nil)
        ((funcall test (car list-of-elements))
         (cons (car list-of-elements)
               (filter (cdr list-of-elements) test)))
        (t (filter (cdr list-of-elements) test))))



; expert system shell

; functions for manipulating our abstract data type for stream elements
; facts are dotted pairs (<assertion>. <cf>)
; where <assertion> is a positive literal 
; and <cf> is its certainty measure
; rules are in the format (rule if <premise> then <conclusion> <cf>)
; where <cf> is the certainty factor

; returns the set of bindings from a pair
(defun subst-list (substitutions)
  (car substitutions))

; returns certainty factor
(defun subst-cf (substitutions)
  (cdr substitutions))

; constructs a pair from a set of substitutions and a certainty factor
(defun subst-record (substitutions cf)
  (cons substitutions cf))

; functions for handling rules and facts

(defun premise (rule)
  (nth 2 rule))

(defun conclusion (rule)
  (nth 4 rule))

(defun rule-cf (rule)
  (nth 5 rule))

(defun rulep (pattern)
  (and (listp pattern)
       (equal (nth 0 pattern) 'rule)))

(defun fact-pattern (fact)
  (car fact))

(defun fact-cf (fact)
  (cdr fact))

; rule interpreter

; (defun logic-shell ()
;   (print '? )
;   (let ((goal (read)))
;     (cond ((equal goal 'quit) 'bye)
;           (t (print-solutions goal
;                               (solve goal nil))
;              (terpri)
;              (logic-shell)))))

; recursive call to solve solves the goal under each substitution set
; (defun solve (goal substitutions)
;   (declare (special *assertions*))
;   (if (conjunctive-goal-p goal)
;       (filter-through-conj-goals (body goal)
;                                  (cons-stream substitutions
;                                               (make-empty-stream)))
;       (infer goal substitutions *assertions*)))

; passes a stream of substitution sets through a sequence of goals
; (defun filter-through-conj-goals (goals substitution-stream)
;   (if (null goals) substitution-stream
;       (filter-through-conj-goals (cdr goals)
;                                  (filter-through-goal (car goals)
;                                                       substitution-stream))))

; filters substitution-stream through a single goal
; (defun filter-through-goal
;   (goal substitution-stream)
;   (if (empty-stream-p substitution-stream)
;       (make-empty-stream)
;       (combine-streams
;         (solve goal
;                (head-stream substitution-stream))
;         (filter-through-goal goal
;                              (tail-stream substitution-stream)))))

; simple goals handled by infer
; (defun infer (goal substitutions kb)
;   (if (null kb)
;       (make-empty-stream)
;       (let* ((assertion
;                (rename-variables (car kb)))
;              (match (if (rulep assertion)
;                         (unify goal (conclusion assertion)
;                                substitutions)
;                         (unify goal assertion substitutions))))
;         (if (equal match 'failed)
;             (infer goal substitutions (cdr kb))
;             (if (rulep assertion)
;                 (combine-streams
;                   (solve (premise assertion) match)
;                   (infer goal substitutions
;                          (cdr kb)))
;                 (cons-stream match
;                              (infer goal substitutions
;                                     (cdr kb))))))))

; (defun print-solutions (goal substitution-stream)
;   (cond ((empty-stream-p substitution-stream)
;          nil)
;         (t (print (apply-substitutions goal
;                                        (head-stream
;                                          substitution-stream)))
;            (terpri)
;            (print-solutions goal
;                             (tail-stream substitution-stream)))))

(defun apply-substitutions
  (pattern substitution-list)
  (cond ((is-constant-p pattern) pattern)
        ((varp pattern)
         (let ((binding
                 (get-binding pattern
                              substitution-list)))
           (cond (binding (apply-substitutions
                            (get-binding-value binding)
                            substitution-list))
                 (t pattern))))
        (t (cons (apply-substitutions
                   (car pattern)
                   substitution-list)
                 (apply-substitutions (cdr pattern)
                                      substitution-list)))))

; naming scheme

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
  (list 'var (or (cdr (assoc var *name-list*
                             :test #'equal))
                 (let ((name (gensym)))
                   (setq *name-list*
                         (acons var name *name-list*))
                   name))))

(defun premise (rule) (nth 2 rule))

(defun conclusion (rule) (nth 4 rule))

(defun rulep (pattern)
  (and (listp pattern) (equal (nth 0 pattern) 'rule)))

(defun conjunctive-goal-p (goal)
  (and (listp goal) (equal (car goal) 'and)))

(defun body (goal) (cdr goal))

; rule interpreter

; does not return solution stream directly
; recursive call to solve solves the goal under each substitution set
(defun solve (goal substitutions)
  (filter-stream
    (if (conjunctive-goal-p goal)
        (filter-through-conj-goals
          (cdr (body goal))
          (solve (car (body goal)) substitutions))
        (solve-simple-goal goal substitutions))
    #'(lambda (x)
       (< 0.2 (subst-cf x)))))

(defun solve-simple-goal (goal substitutions)
  (declare (special *assertions*))
  (declare (special *case-specific-data*))
  (or (told goal substitutions
            *case-specific-data*)
      (infer goal substitutions *assertions*)
      (ask-for goal substitutions)))

(defun lisp-shell ()
  (declare (special *case-specific-data*))
  (setq *case-specific-data* ( ))
  (prin1 'lisp-shell> )
  (let ((goal (read)))
    (terpri)
    (cond ((equal goal 'quit) 'bye)
          (t (print-solutions goal
                              (solve goal
                                     (subst-record nil 0)))
             (terpri)
             (lisp-shell)))))

; passes a stream of substitution sets through a sequence of goals
(defun filter-through-conj-goals (goals substitution-stream)
  (if (null goals)
      substitution-stream
      (filter-through-conj-goals (cdr goals)
                                 (filter-through-goal (car goals)
                                                      substitution-stream))))

; filters substitution-stream through a single goal
(defun filter-through-goal (goal substitution-stream)
  (if (empty-stream-p substitution-stream)
      (make-empty-stream)
      (let ((subs (head-stream
                    substitution-stream)))
        (combine-streams
          (map-stream (solve goal subs)
                      #'(lambda (x)
                         (subst-record (subst-list x)
                                       (min (subst-cf x)
                                            (subst-cf subs)))))
          (filter-through-goal goal
                               (tail-stream
                                 substitution-stream))))))

(defun infer (goal substitutions kb)
  (if (null kb)
      (make-empty-stream)
      (let* ((assertion
               (rename-variables (car kb)))
             (match (if (rulep assertion)
                        (unify goal (conclusion assertion)
                               subst-list substitutions))
                    (unify goal assertion
                           (subst-list substitutions)))))
      (if (equal match 'failed)
          (infer goal substitutions
                 (cdr kb))
          (if (rulep assertion)
              (combine-streams
                (solve-rule assertion
                            (subst-record match
                                          (subst-cf substitutions)))
                (infer goal substitutions
                       (cdr kb)))
              (cons-stream
                (subst-record match
                              (fact-cf assertion))
                (infer goal substitutions
                       (cdr kb)))))))

(defun solve-rule (rule substitutions)
  (map-stream
    (solve (premise rule) substitutions)
    #'(lambda (x) (subst-record
                    (subst-list x)
                    (* (subst-cf x)
                       (rule-cf rule))))))

(defun print-solutions (goal substitution-stream)
  (cond ((empty-stream-p substitution-stream)nil)
        (t (print (apply-substitutions goal
                                       (subst-list (head-stream
                                                     substitution-stream))))
           (write-string “cf =“)
           (prin1 (subst-cf (head-stream
                              substitution-stream)))
           (terpri)
           (print-solutions goal
                            (tail-stream
                              substitution-stream)))))

; user interactions

(defun ask-for (goal substitutions)
  (declare (special *askables*))
  (declare (special *case-specific-data*))
  (when (askable goal *askables*)
    (let* ((query (apply-substitutions goal (subst-list substitutions)))
           (result (ask-rec query)))
      (progn (setq *case-specific-data* (cons (subst-record query result) *case-specific-data*))
             (cons-stream (subst-record (subst-list substitutions) result) (make-empty-stream))))))

(defun ask-rec (query)
  (prin1 query)
  (write-string “>“)
  (let ((answer (read)))
    (cond ((equal answer 'y) 1)
          ((equal answer 'n) – 1)
          (t (print
               “answer must be y or n”)
             (terpri)
             (ask-rec query)))))

(defun askable (goal askables)
  (cond ((null askables) nil)
        ((not (equal (unify goal car askables) ()) 'failed) t)
        (t (askable goal (cdr askables)))))

(defun told (goal substitutions case-specific-data)
  (cond ((null case-specific-data)
         (make-empty-stream))
        (t (combine-streams
             (use-fact goal (car case-specific-data)
                       substitutions)
             (told goal substitutions
                   (cdr case-specific-data))))))

; example

(setq *assertions* '((rule
                       if (and (size (var x) tall)
                               (woody (var x)))
                       then (tree (var x)) .9)
                     (rule
                       if (and (size (var x) small)
                               (woody (var x)))
                       then (bush (var x)) .9)
                     (rule
                       if (and (tree (var x)) (evergreen (var x))
                               (color (var x) blue))
                       then (kind (var x) spruce) .8)
                     (rule
                       if (and (tree (var x)) (evergreen (var x))
                               (color (var x) green))
                       then (kind (var x) pine) .9)
                     (rule
                       if (and (tree (var x)) (deciduous (var x))
                               (bears (var x) fruit))
                       then (fruit-tree (var x)) 1)
                     (rule
                       if (and (fruit-tree (var x))
                               (color fruit red)
                               (taste fruit sweet))
                       then (kind (var x) apple-tree) .9)
                     (rule
                       if (and (fruit-tree (var x))
                               (color fruit yellow)
                               (taste fruit sour))
                       then (kind (var x) lemon-tree) .8)
                     (rule
                       if (and (bush (var x))
                               (flowering (var x))
                               (thorny (var x)))
                       then (rose (var x)) 1)
                     (rule
                       if (and (rose (var x)) (color (var x) red))
                       then (kind (var x) american-beauty) 1)))

(setq *askables* '((size (var x) (var y))
                   (woody (var x))
                   (soft (var x))
                   (color (var x) (var y))
                   (evergreen (var x))
                   (thorny (var x))
                   (deciduous (var x))
                   (bears (var x) (var y))
                   (taste (var x) (var y))
                   (flowering (var x))))

; > (lisp-shell)
; lisp-shell>(kind tree-1 (var x))
; (size tree-1 tall) >y
; (woody tree-1) >y
; (evergreen tree-1) >y
; (color tree-1 blue) >n
; (color tree-1 green) >y
; (kind tree-1 pine) cf 0.81
; (deciduous tree-1) >n
; (size tree-1 small) >n
; lisp-shell>(kind bush-2 (var x))
; (size bush-2 tall) >n
; (size bush-2 small) >y
; (woody bush-2) >y
; (flowering bush-2) >y
; (thorny bush-2) >y
; (color bush-2 red) >y
; (kind bush-2 american-beauty) cf 0.9
; lisp-shell>(kind tree-3 (var x))
; (size tree-3 tall) >y
; (woody tree-3) >y
; (evergreen tree-3) >n
; (deciduous tree-3) >y
; (bears tree-3 fruit) >y
; (color fruit red) >n
; (color fruit yellow) >y
; (taste fruit sour) >y
; (kind tree-3 lemon-tree) cf 0.72
; (size tree-3 small) >n
; lisp-shell>quit
; bye
; ?





(setq v 10)
(let ((v 20)) (setq f_closure (function (lambda () v))))
(funcall f_closure)

; macros give complete control over the evaluation of their arguments
; functions defined with defun evaluate their arguments before executing the body
; defmacro binds the unevaluated s-expressions in the call to the formal parameters and evaluates its body twice
; first evaluation macro-expansion, second evaluates the resulting form
; backquote ` prevents evaluation like a quote, allows selective evaluation of elements of backquoted expression
; any element of a backquoted s-expression preceded by a comma is evaluated and its value inserted into the resulting expression

; (+ 2 3) not evaluated, instead bound to formal parameter exp
(delay (+ 2 3))
(function (lambda () (+ 2 3)))

(defun fibonacci-stream (fibonacci-1 fibonacci-2)
  (cons-stream (+ fibonacci-1 fibonacci-2)
               (fibonacci-stream fibonacci-2
                                 (+ fibonacci-1 fibonacci-2))))

(defun filter-odds (stream)
  (cond ((evenp (head-stream stream)) (filter-odds (tail-stream stream)))
        (t (cons-stream (head-stream stream) (filter-odds (tail-stream stream))))))

(defun accumulate-into-list (n stream)
  (cond ((zerop n) nil)
        (t (cons (head-stream stream)
                 (accumulate-into-list (- n 1)
                                       (tail-stream stream))))))

(accumulate-into-list 25 (filter-odds (fibonacci-stream 0 1)))


