;;;; kutils.lisp

(in-package #:kutils)

;;; "kutils" goes here. Hacks and glory await!

;;; This file contains various utilities I've written.

(defun interpose (x sep)
  "Takes a list and a separator, and places separator between element
of the list."
  (mapcar (lambda (y)
	    (list y sep))
	  x))

(defun build-list (arg)
  "If arg is an atom, return it as a list. If it's a list, return the
arg. If it's a vector, coerce it to a list. Otherwise, return nil."
  (cond
    ((listp   arg) (copy-list arg))
    ((atom    arg) (list arg))
    ((vectorp arg) (coerce arg 'list))
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

;;; hash-table functions.


(defun |#{-reader| (stream sub-char numarg)
  (declare (ignore sub-char numarg))
  (let ((m     (make-hash-table :test 'equal))
	(k     nil)
	(v     nil)
	(key-p t))
    (labels ((finalise-read (x)
	       (reverse (concatenate 'string x)))
	     (finalise-kv-pair ()
	       (if key-p
		   (unless (null k)
		     (setq key-p nil
			   k     (read-from-string (finalise-read k))))
		   (unless (null v)
		     (setq key-p t
			   v     (read-from-string (finalise-read v)))
		     (setf (gethash k m) v)
		     (setq k nil v nil))))
	     (reading-complete-p ()
	       (and (null v)
		    (not
		     (null k)))))
      (do ((prev (read-char stream) curr)
	   (curr (read-char stream) (read-char stream)))
	  ((and (char= prev #\}) (char= curr #\#)))
	(if (char= prev #\Space)
	    (finalise-kv-pair)
	    (if key-p
		(push prev k)
		(push prev v))))
      (if (reading-complete-p)
	  (error "Mismatched key value pairs.")
	  (progn 
	    (finalise-kv-pair)
	    m)))))

(defun enable-hash-table-reader ()
  "Enables the reader macro #{}# for hash-tables. The resulting
hash-table will use #'equal for equality. For example,

#{:a :b :c :d}#

will create a hash-table with the keys :a and :c. :a stores the
value :b, and :c stores the value :d.
"
  (set-dispatch-macro-character
   #\# #\{ #'|#{-reader|))

(defun sethash (k v m)
  "Convenience notation for setting a value in a hash table."
  (setf (gethash k m) v))

(defun hashkeys (m)
  "Returns a list of the keys in the hash table."
  (let ((keys '()))
    (maphash (lambda (k v)
	       (declare (ignore v))
	       (push k keys))
	     m)
    keys))

(defun hash-table-to-alist (m)
  "Converts the hash-table given to an alist of (key . value) pairs."
  (let ((alist '()))
    (maphash (lambda (k v)
	       (let ((elt (cons k v)))
		 (setf alist (cons elt alist))))
	     m)
    alist))

(defun alist-to-hash-table (alist)
  "Converts the alist to a hash-table."
  (let ((m (make-hash-table :test 'equal)))
    (dolist (elt alist)
      (sethash (car elt) (cdr elt) m))
    m))

