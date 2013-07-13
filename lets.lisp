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
 (defun l-l-d-m (bindings body)
   "Expands binding calls to lets, labels, destructuring and multiple values"
   (if bindings
       (destructuring-bind (head . tail) bindings
	 (let ((hd (car head))
	       (tl (cadr head)))
	   (cond ((and (atom hd) (caddr head))
		  (destructuring-bind (hd sd . tl) head
		    `(labels ((,hd ,sd ,@tl))
		       ,(l-l-d-m tail body))))
		 ((atom hd)
		  `(let ((,hd ,tl))
		     ,(l-l-d-m tail body)))
		 ((and (listp (cdr hd)) (not (cddr hd)))
		  `(multiple-value-bind ,hd ,tl
		     ,(l-l-d-m tail body)))
		 (t
		  `(destructuring-bind ,hd ,tl
		     ,(l-l-d-m tail body))))))
       body)))

(defmacro bind (bindings &body body)
  "Use let, destructuring and multiple value binding in one macro"
  (l-l-d-m bindings `(progn ,@body)))
