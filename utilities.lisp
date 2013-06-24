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

(defmacro nlet (n letargs &rest body)
  "Named let - for recursive solutions"
  `(labels ((,n ,(mapcar #'car letargs)
	      ,@body))
     (,n ,@(mapcar #'cadr letargs))))

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

(defun tree-leaves%% (tree test result)
  (if tree
      (if (listp tree)
	  (cons (tree-leaves%% (car tree) test result)
		(tree-leaves%% (cdr tree) test result))
	  (if (funcall test tree)
	      (funcall result tree)
	      tree))))

(defmacro tree-leaves (tree test result)
  "Walk a tree and change leaves that satisfy test to result"
  `(tree-leaves%% ,tree
		  (lambda (x) (declare (ignorable x)) ,test)
		  (lambda (x) (declare (ignorable x)) ,result)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MACRO WRITING UTILITY - DEFMACRO! ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
  `(let ((it ,test))
     (if it ,then ,else)))

(defmacro awhen (test &body body)
  `(aif ,test ,@body))

(defmacro alambda (params &body body)
  `(labels ((self ,params ,@body))
     #'self))
