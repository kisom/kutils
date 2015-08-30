;;;; kutils.lisp

(in-package #:kutils)

;;; "kutils" goes here. Hacks and glory await!

;;; This file contains various utilities I've written.

(defun take (n lst)
  "Take n elements from lst."
  (labels ((rec (n lst acc)
             (if (or (zerop n) (null lst))
                 (nreverse acc)
                 (rec (- n 1) (cdr lst) (cons (car lst) acc)))))
    (when (> n 0)
      (rec n lst nil))))

(defun drop (n lst)
  "Drop n elements from the list."
  (if (or (<= n 0) (null lst))
      lst
      (drop (- n 1) (cdr lst))))

(defun interpose (x sep)
  "Takes a list and a separator, and places separator between element
of the list."
  (let ((x (coerce x 'list)))
    (apply #'append
	   (mapcar (lambda (y)
		     (list y sep))
		   x))))

(defun build-list (arg)
  "If arg is an atom, return it as a list. If it's a list, return the
arg. If it's a vector, coerce it to a list. Otherwise, return nil."
  (cond
    ((listp   arg) (copy-list arg))
    ((vectorp arg) (coerce arg 'list))
    ((atom    arg) (list arg))
    (t nil)))

(defun partial (fn &rest initial-args)
  "partial provides partial function application. It returns a lambda
that will call the function given with the intial args and any
additional args provided to the lambda."
  (lambda (&rest args)
    (apply fn (append initial-args args))))

(defun macroexpand-n (n form)
  "Expand the macro n times."
  (let ((new-form form))
    (dotimes (i n)
      (multiple-value-bind
	    (expansion expanded)
	  (macroexpand-1 new-form)
	(if expanded
	    (setf new-form expansion)
	    (return))))
    new-form))

(defun mksymb (&rest args)
  "Create a symbol from arguments, upcasing strings as required."
  (let ((args (mapcar (lambda (arg)
			(if (stringp arg)
			    (string-upcase arg)
			    arg))
		      args)))
    (apply #'symb args)))

(defun mkkw (&rest args)
  "Create a keyword from its arguments."
  (intern (string (apply #'mksymb args)) "KEYWORD"))

(defun build-slot (class-name slot)
  (let* ((slot-sym (mksymb slot))
	 (accessor (mksymb class-name "-" slot-sym)))
    (list slot-sym
	  :initarg (mkkw slot-sym)
	  :accessor accessor)))

(defun build-slot-list (class-name &rest slots)
  (mapcar (partial #'build-slot class-name) slots))

(defun build-arg-list (slots)
  (mapcar (lambda (slot) (list (mkkw slot) (mksymb slot))) slots))

(defun inherited-slots (supers)
  (mapcar #'closer-mop:slot-definition-name
	  (flatten
	   (mapcar #'closer-mop:class-slots
		   (mapcar (lambda (cls) (find-class cls t nil)) supers)))))

(defun superclasses (superclasses)
  (mapcar #'class-name
	  (mapcar (lambda (cls) (find-class cls t nil))
		  superclasses)))

(defmacro defclass! (name superclass-spec slots &body body)
  "Defines a new class and default constructor for name, based on the
superclasses and slots provided. If the first argument to body is a
string, it will be used as the class's docstring."
  (let* ((name (mksymb name))
	 (docstring (if (stringp (first body))
		      (list :documentation (first body))
		      (list :documentation (format nil"Automatically generated class."))))
	 (supers (superclasses superclass-spec))
	 (body (if docstring (rest body) body))
	 (ctor (mksymb "make-" name))
	 (all-slots (flatten (append (inherited-slots supers) slots))))
    (format t "supers: ~A~%" supers)
    `(progn
       (closer-mop:ensure-finalized
	(defclass ,name ,supers
	  ,(loop for slot in slots collecting
		(list slot :initarg (mksymb slot)
                      :accessor (mksymb name #\- slot)))
	  ,docstring
	  ,@body))
       t)))

(defun zip (&rest lsts)
  "Zip together elements from each list: (zip '(a b c) '(1 2 3))
produces '((a 1) (b 2) (c 3))."
  (labels ((zip-acc (lsts)
	     (unless (some #'null lsts)
	       (cons (mapcar #'car lsts) (zip-acc (mapcar #'cdr lsts))))))
    (zip-acc lsts)))


(defun read-file-string (path)
  "Read the contents of the file at path as a string."
  (with-open-file (s path)
    (let ((data (make-string (file-length s))))
      (read-sequence data s)
      data)))

(defun new-vector ()
  "Create a new, empty, adjustable vector with fill pointer."
  (make-array 0 :adjustable t :fill-pointer t))

(defmacro mapv (fn &rest vecs)
  "Utility to map @c(fn) over the vectors @c(vecs), producing a vector."
  `(map 'vector ,fn ,@vecs))
    
(defun vectorise (lst)
  "Ensure @c(lst) is a vector."
  (cond ((listp lst) (apply #'vector lst))
        ((vectorp lst) lst)
        (otherwise (mapv #'identity lst))))

(defmacro assoc-val (item alist &rest key-args)
  "Return the value of @c(item) in @c(alist). @c(key-args) should
contain any additional keyword arguments to @c(assoc)."
  `(first (assoc ,item ,alist ,@key-args)))
