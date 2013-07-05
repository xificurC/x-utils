(in-package :x-utils)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun flatten (x)
  "Flattens a list, e.g. '((1 2) ((3 4) (5 6))) -> '(1 2 3 4 5 6)"
  (labels ((rec (x acc)
	     (cond ((null x) acc)
		   ((atom x) (cons x acc))
		   (t (rec (car x) (rec (cdr x) acc))))))
    (rec x nil)))
  
  (defun mkstr (&rest args)
    "Concatenates strings"
    (with-output-to-string (s)
      (dolist (a args) (princ a s))))

  (defun symb (&rest args)
    "Interns a symbol by concatenating args"
    (values (intern (apply #'mkstr args))))

  (defun g!-symbol-p (s)
    "Returns true if s is a g! symbol"
    (and (symbolp s)
	 (> (length (symbol-name s)) 2)
	 (string= (symbol-name s) "G!" :start1 0 :end1 2)))

  (defun o!-symbol-p (s)
    "Returns true if s is a o! symbol"
    (and (symbolp s)
	 (> (length (symbol-name s)) 2)
	 (string= (symbol-name s) "O!" :start1 0 :end1 2)))

  (defun o!-symbol-to-g!-symbol (s)
    "Interns a g! symbol for an o! symbol"
    (symb "G!" (subseq (symbol-name s) 2))))


(defmacro defmacro/g! (name args &rest body)
  "Defmacro with auto-gensym for g! symbols"
  (let ((syms (remove-duplicates (remove-if-not #'g!-symbol-p (flatten body)))))
    `(defmacro ,name ,args
       (let ,(mapcar (lambda (s) `(,s (gensym ,(subseq (symbol-name s) 2))))
		     syms)
	 ,@body))))

(defmacro defmacro! (name args &rest body)
  "Defmacro with auto-gensym and auto once only"
  (let* ((os (remove-if-not #'o!-symbol-p args))
	 (gs (mapcar #'o!-symbol-to-g!-symbol os)))
    `(defmacro/g! ,name ,args
       `(let ,(mapcar #'list (list ,@gs) (list ,@os))
	  ,(progn ,@body)))))
