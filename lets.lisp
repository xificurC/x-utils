(in-package :x-utils)

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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun l-d-m (bindings body)
    "Expands binding calls to lets, destructuring and multiple values"
    (if bindings
	(destructuring-bind (head . tail) bindings
	  (cond ((listp (car head))
		 `(destructuring-bind ,(car head) ,(cadr head)
		    ,(l-d-m tail body)))
		((= 2 (length head))
		 `(let ((,(car head) ,(cadr head)))
		    ,(l-d-m tail body)))
		(t
		 `(multiple-value-bind ,(butlast head) ,(car (last head))
		    ,(l-d-m tail body)))))
	body)))

(defmacro bind (bindings &body body)
  "Use let, destructuring and multiple value binding in one macro"
  (l-d-m bindings `(progn ,@body)))
