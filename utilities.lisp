;;;;;;;;;;;;;
;; PACKAGE ;;
;;;;;;;;;;;;;

(in-package :x-utils)

;;;;;;;;;;;;;;;;;;;;;
;; SYMBOL CREATION ;;
;;;;;;;;;;;;;;;;;;;;;

(defun mkstr (&rest args)
  "Concatenates strings"
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

(defun symb (&rest args)
  "Interns a symbol by concatenating args"
  (values (intern (apply #'mkstr args))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LETS, LABELS, LAMBDAS ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro nlet (n letargs &rest body)
  "Named let - for recursive solutions"
  `(labels ((,n ,(mapcar #'car letargs)
	      ,@body))
     (,n ,@(mapcar #'cadr letargs))))

(defmacro if-let ((name val) then &optional else)
  "Binds val to name and creates an (if name then else)"
  `(let ((,name ,val))
     (if ,name ,then ,else)))

(defmacro when-let ((name val) &body body)
  "Binds val to name and creates a (when name body)"
  `(let ((,name ,val))
     (when ,name ,@body)))

;;;;;;;;;;;;;;;;;;;;;;;
;; UTILITY FUNCTIONS ;;
;;;;;;;;;;;;;;;;;;;;;;;

(defun group (source n)
  "Groups a list in lists of length n, e.g. '(1 2 3 4) -> '((1 2) (3 4))"
  (if (zerop n) (error "zero length"))
  (labels ((rec (source acc)
	     (let ((rest (nthcdr n source)))
	       (if (consp rest)
		   (rec rest (cons (subseq source 0 n) acc))
		   (nreverse (cons source acc))))))
    (if source (rec source nil) nil)))

(defun flatten (x)
  "Flattens a list, e.g. '((1 2) ((3 4) (5 6))) -> '(1 2 3 4 5 6)"
  (labels ((rec (x acc)
	     (cond ((null x) acc)
		   ((atom x) (cons x acc))
		   (t (rec (car x) (rec (cdr x) acc))))))
    (rec x nil)))

(defun fact (x)
  "Factorial of x"
  (nlet rec ((x x) (acc 1))
	(if (zerop x)
	    acc
	    (rec (1- x) (* acc x)))))

(defun choose (n r)
  "C(n,r) - combinations"
  (/ (fact n)
     (fact (- n r))
     (fact r)))

(eval-when (:compile-toplevel :load-toplevel :execute)
 (defun tree-leaves%% (tree test result)
   (if tree
       (if (listp tree)
	   (cons (tree-leaves%% (car tree) test result)
		 (tree-leaves%% (cdr tree) test result))
	   (if (funcall test tree)
	       (funcall result tree)
	       tree)))))

(defmacro tree-leaves (tree test result)
  "Walk a tree and change leaves that satisfy test to result"
  `(tree-leaves%% ,tree
		  (lambda (x) (declare (ignorable x)) ,test)
		  (lambda (x) (declare (ignorable x)) ,result)))

;;;;;;;;;;
;; MAPS ;;
;;;;;;;;;;

(defun mapa-b (fn a b &optional (step 1))
  "Map a function on numbers from a to b by step"
  (do ((i a (+ a step))
       (acc))
      ((> i b) (nreverse acc))
    (push (funcall fn i) acc)))

(defun map0-n (fn n)
  "Map a function on numbers from 0 to n"
  (mapa-b fn 0 n))

(defun map1-n (fn n)
  "Map a function on numbers from 1 to n"
  (mapa-b fn 1 n))

(defun map-> (fn start test-fn succ-fn)
  "Map a function on start and successors decided by succ-fn until test-fn is satisfied"
  (do ((i start (funcall succ-fn i))
       (acc))
      ((funcall test-fn i) (nreverse acc))
    (push (funcall fn i) acc)))

(defun mappend (fn &rest lsts)
  "Non-destructive mapcan"
  (apply #'append (apply #'mapcar fn lsts)))

(defun mapcars (fn &rest lsts)
  "Instead of (mapcar fn (list a b)) you can write (mapcars fn a b)"
  (let ((result nil))
    (dolist (lst lsts)
      (dolist (obj lst)
	(push (funcall fn obj) result)))
    (nreverse result)))

(defun rmapcar (fn &rest args)
  "Mapcar recursing in a tree"
  (if (some #'atom args)
      (apply fn args)
      (apply #'mapcar
	     (lambda (&rest args)
	       (apply #'rmapcar fn args))
	     args)))

;;;;;;;;;;;
;; LOOPS ;;
;;;;;;;;;;;

(defmacro while (test &body body)
  "Loops while test is true"
  `(do ()
       ((not ,test))
     ,@body))

(defmacro until (test &body body)
  "Loops until test is true"
  `(do ()
       (,test)
     ,@body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MACRO WRITING UTILITY - DEFMACRO! ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro mac (expr)
  "Macroexpand-1 pretty printer without the annoying quote"
  `(pprint (macroexpand-1 ',expr)))

(defmacro with-gensyms (syms &body body)
    `(let ,(mapcar (lambda (s) `(,s (gensym)))
		   syms)
       ,@body))

(defun g!-symbol-p (s)
  "Returns true if s is a g! symbol"
  (and (symbolp s)
       (> (length (symbol-name s)) 2)
       (string= (symbol-name s) "G!" :start1 0 :end1 2)))

(defmacro defmacro/g! (name args &rest body)
  "Defmacro with auto-gensym for g! symbols"
  (let ((syms (remove-duplicates (remove-if-not #'g!-symbol-p (flatten body)))))
    `(defmacro ,name ,args
       (let ,(mapcar (lambda (s) `(,s (gensym ,(subseq (symbol-name s) 2))))
		     syms)
	 ,@body))))

(defun o!-symbol-p (s)
  "Returns true if s is a o! symbol"
  (and (symbolp s)
       (> (length (symbol-name s)) 2)
       (string= (symbol-name s) "O!" :start1 0 :end1 2)))

(defun o!-symbol-to-g!-symbol (s)
  "Interns a g! symbol for an o! symbol"
  (symb "G!" (subseq (symbol-name s) 2)))

(defmacro defmacro! (name args &rest body)
  "Defmacro with auto-gensym and auto once only"
  (let* ((os (remove-if-not #'o!-symbol-p args))
	 (gs (mapcar #'o!-symbol-to-g!-symbol os)))
    `(defmacro/g! ,name ,args
       `(let ,(mapcar #'list (list ,@gs) (list ,@os))
	  ,(progn ,@body)))))

;;;;;;;;;;;;;;
;; ANAPHORA ;;
;;;;;;;;;;;;;;

(defmacro aif (test then &optional else)
  "Anaphoric if, test stored in a symbol it"
  `(let ((it ,test))
     (if it ,then ,else)))

(defmacro awhen (test &body body)
  "Anaphoric when, test stored in a symbol it"
  `(aif ,test (progn ,@body)))

(defmacro awhile (expr &body body)
  "Anaphoric while"
  `(do ((it ,expr ,expr))
       ((not it))
     ,@body))

(defmacro auntil (expr &body body)
  "Anaphoric until"
  `(do ((it ,expr ,expr))
       (it)
     ,@body))

(defmacro alambda (params &body body)
  "Anaphoric lambda, can recurse through call to self"
  `(labels ((self ,params ,@body))
     #'self))

(defmacro acond (&rest clauses)
  "Anaphoric cond"
  (if (null clauses)
      (let ((cl1 (car clauses))
	    (sym (gensym)))
	`(let ((,sym ,(car cl1)))
	   (if ,sym
	       (let ((it ,sym))
		 (declare (ignorable it))
		 ,@(cdr cl1))
	       (acond ,@(cdr clauses)))))))

(defmacro aif2 (test &optional then else)
  "Anaphoric if for lookup tests"
  (let ((win (gensym)))
    `(multiple-value-bind (it ,win) ,test
       (if (or it ,win) ,then ,else))))

(defmacro awhen2 (test &body body)
  "Anaphoric when for lookup tests"
  `(aif2 ,test (progn ,@body)))

(defmacro acond2 (&rest clauses)
  "Anaphoric cond for lookup tests"
  (if (null clauses)
      nil
      (let ((cl1 (car clauses))
	    (val (gensym))
	    (win (gensym)))
	`(multiple-value-bind (,val ,win) ,(car cl1)
	   (if (or ,val ,win)
	       (let ((it ,val))
		 (declare (ignorable it))
		 ,@(cdr cl1))
	       (acond2 ,@(cdr clauses)))))))
