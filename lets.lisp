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
