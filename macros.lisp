;;;; macros.lisp

(in-package #:kutils)

;;; Various utility macros.

(defmacro when (bindings &body body)
  "Evaluate the bindings in a let form; if they all evaluate to T,
evaluate @c(body) in an implicit @c(progn)."
  (let ((bindings (if (listp (first bindings)) bindings (list bindings))))
    `(let (,@bindings)
       (when (and ,@(mapcar #'first bindings))
	 ,@body))))

(defmacro whenlet* (bindings &body body)
  "Evaluate @c(bindings) in a @c(let*) form; if they all evaluate to T,
evaluate @c(body), which is wrapped in an implicit @c(progn)."
  (let ((bindings (if (listp (first bindings)) bindings (list bindings))))
    `(let* (,@bindings)
       (when (and ,@(mapcar #'first bindings))
	 ,@body))))

(defmacro unlesslet (bindings &body body)
  "Evaluate the bindings in a let form; if they don't all evaluate to T,
evaluate @c(body) in an implicit @c(progn)."
  (let ((bindings (if (listp (first bindings)) bindings (list bindings))))
    `(let (,@bindings)
       (unless (and ,@(mapcar #'first bindings))
	 ,@body))))

(defmacro unlesslet* (bindings &body body)
  "Evaluate @c(bindings) in a @c(let*) form; if they are all not NIL,
evaluate @c(body), which is wrapped in an implicit @c(progn)."
  (let ((bindings (if (listp (first bindings)) bindings (list bindings))))
    `(let* (,@bindings)
       (unless (and ,@(mapcar #'first bindings))
	 ,@body))))

(defmacro iflet (bindings &body (then &optional else))
  "Evaluate @c(bindings) in a @c(let) form; if they are all T, execute
the @c(then) form. Otherwise, execute the @c(else) form."  
  (let ((bindings (if (listp (first bindings)) bindings (list bindings))))
    `(let* (,@bindings)
       (if (and ,@(mapcar #'first bindings))
	   (progn ,then)
	   (progn ,else)))))

(defmacro iflet* (bindings &body (then &optional else))
  "Evaluate @c(bindings) in a @c(let*) form; if they are all T, execute
the @c(then) form. Otherwise, execute the @c(else) form."
  (let ((bindings (if (listp (first bindings)) bindings (list bindings))))
    `(let* (,@bindings)
       (if (and ,@(mapcar #'first bindings))
	   (progn ,then)
	   (progn ,else)))))

(defmacro condlet (bindings &body forms)
  "Evaluate @c(bindings) in a @c(let) form, then evaluate @c(forms) in
a @c(cond)."
  (let ((bindings (if (listp (first bindings)) bindings (list bindings))))
    `(let (,@bindings)
       (cond ,@forms))))

(defmacro condlet* (bindings &body forms)
  "Evaluate @c(bindings) in a @c(let*) form, then evaluate @c(forms) in
a @c(cond)."
  (let ((bindings (if (listp (first bindings)) bindings (list bindings))))
    `(let (,@bindings)
       (cond ,@forms))))

