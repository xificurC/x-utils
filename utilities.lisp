(in-package :x-utils)

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

;;;;;;;;;;;;;;;
;; DEBUGGING ;;
;;;;;;;;;;;;;;;

(defmacro! dbg (o!sexp)
  "Debugging command that prints the result of the sexp and returns its value"
  `(progn
     (format t "~&~a returned ~a" ',o!sexp ,g!sexp)
     ,g!sexp))

(defun debug-funs (funs body)
  "Surrounds funs in body with dbg"
  (destructuring-bind (head . tail) body
    (if-let (next-list (position-if #'listp tail))
	    (if (listp head)
		(append (cons (debug-funs funs head)
			      (subseq tail 0 next-list))
			(debug-funs funs (subseq tail next-list)))
		(if (member head funs)
		    `(dbg (,head
			   ,@(subseq tail 0 next-list)
			   ,@(debug-funs funs (subseq tail next-list))))
		    (append (cons head
				  (subseq tail 0 next-list))
			    (debug-funs funs (subseq tail next-list)))))
	    (if (listp head)
		(cons (debug-funs funs head) tail)
		(if (member head funs)
		    `(dbg ,body)
		    body)))))

(defmacro! with-debug-funs (funs &body body)
  `(progn ,@(mapcar (lambda (g!x) (debug-funs funs g!x)) body)))

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

;;;;;;;;;;;;;;;;;;;;;
;; MACRO UTILITIES ;;
;;;;;;;;;;;;;;;;;;;;;

(defmacro mac (expr)
  "Macroexpand-1 pretty printer without the annoying quote"
  `(pprint (macroexpand-1 ',expr)))

(defmacro with-gensyms (syms &body body)
  "Generates gensyms for syms"
    `(let ,(mapcar (lambda (s) `(,s (gensym)))
		   syms)
       ,@body))
