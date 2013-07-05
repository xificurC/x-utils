(in-package :x-utils)

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
