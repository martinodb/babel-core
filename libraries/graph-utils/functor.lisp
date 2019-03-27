(in-package #:graph-utils)

(defstruct (functor
	     (:constructor %make-functor)
	     (:predicate functor?))
  name fn clauses (lock (make-recursive-lock)))

(defgeneric prolog-compile (functor))

(defun lookup-functor (name)
  (gethash name *user-functors*))

(defun make-functor (&key name clauses)
  (or (lookup-functor name)
      (let ((functor (%make-functor :name name :clauses clauses)))
	(with-recursive-lock-held ((functor-lock functor))
	  (prog1
	      (setf (gethash name *user-functors*) functor)
	    (prolog-compile functor))))))

(defun add-functor-clause (functor clause)
  (with-recursive-lock-held ((functor-lock functor))
    #+sbcl
    (sb-ext:cas (cdr (last (functor-clauses functor)))
                (cdr (last (functor-clauses functor)))
                (list clause))
    #-sbcl
    (setf (cdr (last (functor-clauses functor)))
          (list clause))
    (prolog-compile functor))
  (functor-clauses functor))

(defun delete-functor (functor)
  (remhash (functor-name functor) *user-functors*))

(defun reset-functor (functor)
  (with-recursive-lock-held ((functor-lock functor))
    #+sbcl
    (sb-ext:cas (functor-clauses functor) (functor-clauses functor) nil)
    #-sbcl
    (setf (functor-clauses functor) nil)
    (prolog-compile functor))
  nil)

(defun get-functor-fn (functor-symbol)
  (let ((f (lookup-functor functor-symbol)))
    (when (functor? f)
      (functor-fn f))))

(defun set-functor-fn (functor-symbol fn)
  (let ((f (lookup-functor functor-symbol)))
    (when *prolog-trace*
      (format t "TRACE: set-functor-fn for ~A got ~A~%" functor-symbol f))
    (if (functor? f)
	(setf (functor-fn f) fn)
	(error 'prolog-error
	       :reason (format nil "unknown functor ~A" functor-symbol)))))
